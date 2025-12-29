# -*- coding: utf-8 -*-
"""
Integration Gateway Module - Enhanced
=====================================
Gateway centralizado para integracoes com isolamento completo de tenant.

Funcionalidades:
- Factory de conectores com isolamento por tenant
- Proxy seguro para integracoes externas
- Rate limiting por tenant e integracao
- Logging e auditoria de operacoes
- Cache de credenciais por tenant
- Tenant isolation em todas as integracoes
- Metricas e monitoramento
- Circuit breaker para resiliencia

Configuracao via variaveis de ambiente ou banco de dados.

Issue #116 - Gateway de Integracoes com Isolamento de Tenant
"""

import os
import json
import logging
import asyncio
import hashlib
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Type
from enum import Enum
from functools import wraps

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    aiohttp = None

logger = logging.getLogger(__name__)


class IntegrationType(str, Enum):
    """Tipos de integracao suportados"""
    # BI Tools
    POWERBI = "powerbi"
    TABLEAU = "tableau"
    EXCEL = "excel"

    # Project Management
    JIRA = "jira"
    AZURE_DEVOPS = "azure_devops"

    # Version Control
    GITHUB = "github"
    GITLAB = "gitlab"

    # CRM
    SALESFORCE = "salesforce"

    # ERP
    SAP = "sap"

    # Communication
    TEAMS = "teams"
    SLACK = "slack"
    EMAIL = "email"


class GatewayStatus(str, Enum):
    """Status do gateway"""
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    UNHEALTHY = "unhealthy"


@dataclass
class TenantIntegrationConfig:
    """Configuracao de integracao por tenant"""
    tenant_id: str
    integration_type: IntegrationType
    enabled: bool = True
    credentials: Dict[str, Any] = field(default_factory=dict)
    settings: Dict[str, Any] = field(default_factory=dict)
    rate_limit: int = 100  # requests por minuto
    rate_limit_window: int = 60  # segundos
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)


@dataclass
class ProxyRequest:
    """Request para proxy de integracao"""
    integration: IntegrationType
    endpoint: str
    method: str = "GET"
    data: Optional[Dict] = None
    headers: Optional[Dict] = None
    params: Optional[Dict] = None
    timeout: int = 30


@dataclass
class ProxyResponse:
    """Response do proxy de integracao"""
    success: bool
    status_code: int
    data: Any = None
    error: Optional[str] = None
    duration_ms: int = 0
    cached: bool = False


class RateLimitTracker:
    """Rastreador de rate limit por tenant e integracao"""

    def __init__(self):
        self._counters: Dict[str, Dict] = {}

    def _get_key(self, tenant_id: str, integration: IntegrationType) -> str:
        return f"{tenant_id}:{integration.value}"

    def check(self, tenant_id: str, integration: IntegrationType, limit: int, window: int) -> bool:
        """
        Verifica se requisicao esta dentro do rate limit.

        Args:
            tenant_id: ID do tenant
            integration: Tipo de integracao
            limit: Limite de requisicoes
            window: Janela em segundos

        Returns:
            bool: True se permitido
        """
        key = self._get_key(tenant_id, integration)
        now = datetime.utcnow()

        if key not in self._counters:
            self._counters[key] = {"count": 0, "reset_at": now + timedelta(seconds=window)}

        counter = self._counters[key]

        # Reset se janela expirou
        if now >= counter["reset_at"]:
            counter["count"] = 0
            counter["reset_at"] = now + timedelta(seconds=window)

        # Verificar limite
        if counter["count"] >= limit:
            return False

        counter["count"] += 1
        return True

    def get_remaining(self, tenant_id: str, integration: IntegrationType, limit: int) -> int:
        """Retorna requisicoes restantes"""
        key = self._get_key(tenant_id, integration)
        if key not in self._counters:
            return limit
        return max(0, limit - self._counters[key]["count"])

    def get_reset_time(self, tenant_id: str, integration: IntegrationType) -> Optional[datetime]:
        """Retorna quando o rate limit reseta"""
        key = self._get_key(tenant_id, integration)
        if key not in self._counters:
            return None
        return self._counters[key]["reset_at"]


class IntegrationGateway:
    """
    Gateway centralizado para integracoes com isolamento por tenant.

    Fornece:
    - Factory de conectores isolados por tenant
    - Proxy seguro para APIs externas
    - Rate limiting por tenant
    - Cache de configuracoes
    - Logging de auditoria

    Exemplo de uso:
    ```python
    gateway = IntegrationGateway(tenant_id="TENANT-001")

    # Obter conector
    connector = gateway.get_connector(IntegrationType.GITHUB)

    # Proxy para API externa
    response = await gateway.proxy_request(
        ProxyRequest(
            integration=IntegrationType.GITHUB,
            endpoint="/repos/owner/repo/issues",
            method="GET"
        )
    )
    ```
    """

    # Base URLs das integracoes
    BASE_URLS = {
        IntegrationType.GITHUB: "https://api.github.com",
        IntegrationType.GITLAB: "https://gitlab.com/api/v4",
        IntegrationType.JIRA: "",  # Configurado por tenant
        IntegrationType.AZURE_DEVOPS: "https://dev.azure.com",
        IntegrationType.SALESFORCE: "",  # Configurado por tenant
        IntegrationType.TEAMS: "https://graph.microsoft.com/v1.0",
        IntegrationType.SLACK: "https://slack.com/api"
    }

    def __init__(self, tenant_id: str):
        """
        Inicializa o gateway para um tenant.

        Args:
            tenant_id: ID do tenant
        """
        self.tenant_id = tenant_id
        self._config: Dict[IntegrationType, TenantIntegrationConfig] = {}
        self._connectors: Dict[IntegrationType, Any] = {}
        self._session: Optional[aiohttp.ClientSession] = None
        self._rate_limiter = RateLimitTracker()
        self._status = GatewayStatus.HEALTHY
        self._health_checks: Dict[IntegrationType, datetime] = {}

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    async def close(self):
        """Fecha recursos do gateway"""
        if self._session and not self._session.closed:
            await self._session.close()

    def _load_config(self, integration: IntegrationType) -> Optional[TenantIntegrationConfig]:
        """Carrega configuracao da integracao do banco ou cache"""
        if integration in self._config:
            return self._config[integration]

        # Tentar carregar do banco
        config = self._load_config_from_db(integration)
        if config:
            self._config[integration] = config
            return config

        # Tentar carregar de variaveis de ambiente (fallback)
        config = self._load_config_from_env(integration)
        if config:
            self._config[integration] = config
            return config

        return None

    def _load_config_from_db(self, integration: IntegrationType) -> Optional[TenantIntegrationConfig]:
        """Carrega configuracao do banco de dados"""
        # Implementacao futura - integrar com tabela de configuracoes
        return None

    def _load_config_from_env(self, integration: IntegrationType) -> Optional[TenantIntegrationConfig]:
        """Carrega configuracao de variaveis de ambiente"""
        prefix = integration.value.upper()

        # Verificar se existe token/credentials
        token_key = f"{prefix}_TOKEN"
        api_key = f"{prefix}_API_KEY"

        token = os.getenv(token_key) or os.getenv(api_key)
        if not token:
            return None

        credentials = {"token": token}

        # Adicionar outras credenciais especificas
        if integration == IntegrationType.GITHUB:
            credentials["owner"] = os.getenv("GITHUB_OWNER", "")
            credentials["repo"] = os.getenv("GITHUB_REPO", "")
        elif integration == IntegrationType.GITLAB:
            credentials["project_id"] = os.getenv("GITLAB_PROJECT_ID", "")
        elif integration == IntegrationType.JIRA:
            credentials["url"] = os.getenv("JIRA_URL", "")
            credentials["email"] = os.getenv("JIRA_EMAIL", "")
            credentials["project_key"] = os.getenv("JIRA_PROJECT_KEY", "")
        elif integration == IntegrationType.AZURE_DEVOPS:
            credentials["organization"] = os.getenv("AZURE_DEVOPS_ORG", "")
            credentials["project"] = os.getenv("AZURE_DEVOPS_PROJECT", "")

        return TenantIntegrationConfig(
            tenant_id=self.tenant_id,
            integration_type=integration,
            enabled=True,
            credentials=credentials
        )

    def get_connector(self, integration_type: IntegrationType) -> Any:
        """
        Factory para conectores isolados por tenant.

        Args:
            integration_type: Tipo de integracao

        Returns:
            Instancia do conector configurado para o tenant
        """
        if integration_type in self._connectors:
            return self._connectors[integration_type]

        config = self._load_config(integration_type)
        if not config or not config.enabled:
            logger.warning(f"[Gateway] Integracao {integration_type.value} nao configurada para tenant {self.tenant_id}")
            return None

        connector = self._create_connector(integration_type, config)
        if connector:
            self._connectors[integration_type] = connector
            logger.info(f"[Gateway] Conector {integration_type.value} criado para tenant {self.tenant_id}")

        return connector

    def _create_connector(self, integration_type: IntegrationType, config: TenantIntegrationConfig) -> Any:
        """Cria instancia do conector apropriado"""
        try:
            if integration_type == IntegrationType.POWERBI:
                from .powerbi_connector import PowerBIConnector
                return PowerBIConnector(self.tenant_id)

            elif integration_type == IntegrationType.TABLEAU:
                from .tableau_connector import TableauConnector
                return TableauConnector(self.tenant_id)

            elif integration_type == IntegrationType.EXCEL:
                from .excel_exporter import ExcelExporter
                return ExcelExporter(self.tenant_id)

            elif integration_type == IntegrationType.GITHUB:
                from .github import GitHubIntegration, GitHubConfig
                gh_config = GitHubConfig(
                    enabled=True,
                    token=config.credentials.get("token", ""),
                    owner=config.credentials.get("owner", ""),
                    repo=config.credentials.get("repo", "")
                )
                return GitHubIntegration(gh_config)

            elif integration_type == IntegrationType.JIRA:
                from .jira import JiraIntegration, JiraConfig
                jira_config = JiraConfig(
                    enabled=True,
                    url=config.credentials.get("url", ""),
                    email=config.credentials.get("email", ""),
                    api_token=config.credentials.get("token", ""),
                    project_key=config.credentials.get("project_key", "")
                )
                return JiraIntegration(jira_config)

            elif integration_type == IntegrationType.AZURE_DEVOPS:
                from .azure_devops import AzureDevOpsIntegration, AzureDevOpsConfig
                ado_config = AzureDevOpsConfig(
                    enabled=True,
                    organization=config.credentials.get("organization", ""),
                    project=config.credentials.get("project", ""),
                    pat=config.credentials.get("token", "")
                )
                return AzureDevOpsIntegration(ado_config)

            elif integration_type == IntegrationType.SALESFORCE:
                from .salesforce.client import SalesforceClient
                return SalesforceClient()

            elif integration_type == IntegrationType.SAP:
                # SAP tem varios modulos, retornar wrapper generico
                return None

            elif integration_type == IntegrationType.TEAMS:
                from .teams.graph_client import TeamsGraphClient
                return TeamsGraphClient()

            elif integration_type == IntegrationType.GITLAB:
                from .gitlab_integration import GitLabIntegration
                return GitLabIntegration(
                    token=config.credentials.get("token", ""),
                    project_id=config.credentials.get("project_id", "")
                )

        except ImportError as e:
            logger.warning(f"[Gateway] Modulo nao encontrado para {integration_type.value}: {e}")
        except Exception as e:
            logger.error(f"[Gateway] Erro ao criar conector {integration_type.value}: {e}")

        return None

    async def proxy_request(self, request: ProxyRequest) -> ProxyResponse:
        """
        Proxy seguro para integracoes externas.

        Implementa:
        - Autenticacao automatica
        - Rate limiting
        - Timeout
        - Logging

        Args:
            request: Dados da requisicao

        Returns:
            ProxyResponse com resultado
        """
        start_time = datetime.utcnow()

        # Verificar rate limit
        config = self._load_config(request.integration)
        if config:
            limit = config.rate_limit
            window = config.rate_limit_window
        else:
            limit = 100
            window = 60

        if not self._rate_limiter.check(self.tenant_id, request.integration, limit, window):
            remaining = self._rate_limiter.get_remaining(self.tenant_id, request.integration, limit)
            reset_at = self._rate_limiter.get_reset_time(self.tenant_id, request.integration)
            logger.warning(f"[Gateway] Rate limit atingido para {request.integration.value}")
            return ProxyResponse(
                success=False,
                status_code=429,
                error=f"Rate limit exceeded. Remaining: {remaining}. Reset at: {reset_at}",
                duration_ms=0
            )

        # Construir URL
        base_url = self.BASE_URLS.get(request.integration, "")
        if not base_url and config:
            base_url = config.credentials.get("url", "")

        if not base_url:
            return ProxyResponse(
                success=False,
                status_code=500,
                error=f"Base URL nao configurada para {request.integration.value}"
            )

        url = f"{base_url.rstrip('/')}/{request.endpoint.lstrip('/')}"

        # Construir headers
        headers = dict(request.headers or {})

        # Adicionar autenticacao
        if config and config.credentials:
            token = config.credentials.get("token", "")
            if request.integration == IntegrationType.GITHUB:
                headers["Authorization"] = f"Bearer {token}"
                headers["Accept"] = "application/vnd.github+json"
            elif request.integration == IntegrationType.GITLAB:
                headers["PRIVATE-TOKEN"] = token
            elif request.integration in [IntegrationType.JIRA, IntegrationType.AZURE_DEVOPS]:
                import base64
                user = config.credentials.get("email", config.credentials.get("user", ""))
                auth_str = base64.b64encode(f"{user}:{token}".encode()).decode()
                headers["Authorization"] = f"Basic {auth_str}"
            else:
                headers["Authorization"] = f"Bearer {token}"

        # Fazer requisicao
        try:
            session = await self._ensure_session()

            async with session.request(
                method=request.method,
                url=url,
                headers=headers,
                json=request.data if request.method in ["POST", "PUT", "PATCH"] else None,
                params=request.params,
                timeout=aiohttp.ClientTimeout(total=request.timeout)
            ) as response:
                duration = int((datetime.utcnow() - start_time).total_seconds() * 1000)

                try:
                    data = await response.json()
                except:
                    data = await response.text()

                # Log da operacao
                self._log_proxy_request(request, response.status, duration)

                return ProxyResponse(
                    success=200 <= response.status < 300,
                    status_code=response.status,
                    data=data,
                    duration_ms=duration
                )

        except asyncio.TimeoutError:
            duration = int((datetime.utcnow() - start_time).total_seconds() * 1000)
            logger.error(f"[Gateway] Timeout em {request.integration.value}: {url}")
            return ProxyResponse(
                success=False,
                status_code=504,
                error="Request timeout",
                duration_ms=duration
            )
        except Exception as e:
            duration = int((datetime.utcnow() - start_time).total_seconds() * 1000)
            logger.error(f"[Gateway] Erro em {request.integration.value}: {e}")
            return ProxyResponse(
                success=False,
                status_code=500,
                error=str(e),
                duration_ms=duration
            )

    def _log_proxy_request(self, request: ProxyRequest, status: int, duration_ms: int):
        """Registra requisicao de proxy para auditoria"""
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "tenant_id": self.tenant_id,
            "integration": request.integration.value,
            "endpoint": request.endpoint,
            "method": request.method,
            "status": status,
            "duration_ms": duration_ms
        }
        logger.info(f"[Gateway] Proxy: {json.dumps(log_entry)}")

    async def health_check(self, integration: Optional[IntegrationType] = None) -> Dict[str, Any]:
        """
        Verifica saude das integracoes.

        Args:
            integration: Integracao especifica (None = todas)

        Returns:
            Dict com status de saude
        """
        integrations_to_check = [integration] if integration else list(IntegrationType)
        results = {}

        for int_type in integrations_to_check:
            config = self._load_config(int_type)
            if not config or not config.enabled:
                results[int_type.value] = {
                    "status": "disabled",
                    "configured": False
                }
                continue

            # Verificar se conector pode ser criado
            connector = self.get_connector(int_type)
            if not connector:
                results[int_type.value] = {
                    "status": "error",
                    "configured": True,
                    "error": "Conector nao disponivel"
                }
                continue

            # Testar conexao se disponivel
            try:
                if hasattr(connector, "test_connection"):
                    success = await connector.test_connection()
                    results[int_type.value] = {
                        "status": "healthy" if success else "unhealthy",
                        "configured": True,
                        "last_check": datetime.utcnow().isoformat()
                    }
                else:
                    results[int_type.value] = {
                        "status": "unknown",
                        "configured": True,
                        "message": "Teste de conexao nao suportado"
                    }
            except Exception as e:
                results[int_type.value] = {
                    "status": "error",
                    "configured": True,
                    "error": str(e)
                }

            self._health_checks[int_type] = datetime.utcnow()

        # Atualizar status geral
        statuses = [r.get("status") for r in results.values()]
        if all(s == "healthy" for s in statuses):
            self._status = GatewayStatus.HEALTHY
        elif any(s in ["error", "unhealthy"] for s in statuses):
            self._status = GatewayStatus.DEGRADED if any(s == "healthy" for s in statuses) else GatewayStatus.UNHEALTHY
        else:
            self._status = GatewayStatus.HEALTHY

        return {
            "gateway_status": self._status.value,
            "tenant_id": self.tenant_id,
            "timestamp": datetime.utcnow().isoformat(),
            "integrations": results
        }

    def get_available_integrations(self) -> List[Dict[str, Any]]:
        """
        Retorna lista de integracoes disponiveis para o tenant.

        Returns:
            Lista com integracoes e seus status
        """
        available = []
        for int_type in IntegrationType:
            config = self._load_config(int_type)
            available.append({
                "type": int_type.value,
                "name": int_type.name.replace("_", " ").title(),
                "enabled": config.enabled if config else False,
                "configured": config is not None,
                "rate_limit": {
                    "limit": config.rate_limit if config else 100,
                    "remaining": self._rate_limiter.get_remaining(self.tenant_id, int_type, config.rate_limit if config else 100),
                    "reset_at": self._rate_limiter.get_reset_time(self.tenant_id, int_type)
                } if config else None
            })
        return available

    def configure_integration(
        self,
        integration_type: IntegrationType,
        credentials: Dict[str, Any],
        settings: Optional[Dict[str, Any]] = None,
        enabled: bool = True
    ) -> bool:
        """
        Configura uma integracao para o tenant.

        Args:
            integration_type: Tipo de integracao
            credentials: Credenciais de acesso
            settings: Configuracoes adicionais
            enabled: Se a integracao esta ativa

        Returns:
            bool: True se configurado com sucesso
        """
        try:
            config = TenantIntegrationConfig(
                tenant_id=self.tenant_id,
                integration_type=integration_type,
                enabled=enabled,
                credentials=credentials,
                settings=settings or {},
                rate_limit=settings.get("rate_limit", 100) if settings else 100,
                rate_limit_window=settings.get("rate_limit_window", 60) if settings else 60
            )

            self._config[integration_type] = config

            # Limpar conector em cache para recriar com nova config
            if integration_type in self._connectors:
                del self._connectors[integration_type]

            logger.info(f"[Gateway] Integracao {integration_type.value} configurada para tenant {self.tenant_id}")
            return True

        except Exception as e:
            logger.error(f"[Gateway] Erro ao configurar {integration_type.value}: {e}")
            return False


# Cache de gateways por tenant
_gateways: Dict[str, IntegrationGateway] = {}


def get_integration_gateway(tenant_id: str) -> IntegrationGateway:
    """
    Retorna gateway de integracoes para o tenant.

    Args:
        tenant_id: ID do tenant

    Returns:
        IntegrationGateway isolado por tenant
    """
    if tenant_id not in _gateways:
        _gateways[tenant_id] = IntegrationGateway(tenant_id)
    return _gateways[tenant_id]


async def get_tenant_integrations(tenant_id: str) -> Dict[IntegrationType, TenantIntegrationConfig]:
    """
    Retorna configuracoes de integracao do tenant.

    Args:
        tenant_id: ID do tenant

    Returns:
        Dict com configuracoes por tipo de integracao
    """
    gateway = get_integration_gateway(tenant_id)
    configs = {}
    for int_type in IntegrationType:
        config = gateway._load_config(int_type)
        if config:
            configs[int_type] = config
    return configs


# ====================
# Enhanced Tenant Isolation
# ====================

@dataclass
class TenantContext:
    """Contexto de tenant para isolamento"""
    tenant_id: str
    user_id: Optional[str] = None
    roles: List[str] = field(default_factory=list)
    permissions: Dict[str, List[str]] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)

    def has_permission(self, integration: IntegrationType, action: str) -> bool:
        """Verifica se tenant tem permissao para acao"""
        int_perms = self.permissions.get(integration.value, [])
        return action in int_perms or "*" in int_perms

    def to_dict(self) -> Dict:
        return {
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "roles": self.roles,
            "permissions": self.permissions,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat()
        }


@dataclass
class IntegrationCallLog:
    """Log de chamada de integracao"""
    id: str
    tenant_id: str
    integration: str
    endpoint: str
    method: str
    status_code: int
    duration_ms: int
    timestamp: datetime
    user_id: Optional[str] = None
    request_size: int = 0
    response_size: int = 0
    error: Optional[str] = None
    metadata: Dict = field(default_factory=dict)

    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "tenant_id": self.tenant_id,
            "integration": self.integration,
            "endpoint": self.endpoint,
            "method": self.method,
            "status_code": self.status_code,
            "duration_ms": self.duration_ms,
            "timestamp": self.timestamp.isoformat(),
            "user_id": self.user_id,
            "request_size": self.request_size,
            "response_size": self.response_size,
            "error": self.error,
            "metadata": self.metadata
        }


class CircuitBreakerState(str, Enum):
    """Estados do circuit breaker"""
    CLOSED = "closed"  # Normal
    OPEN = "open"  # Bloqueado
    HALF_OPEN = "half_open"  # Testando


@dataclass
class CircuitBreaker:
    """Circuit breaker para integracao"""
    failure_threshold: int = 5
    recovery_timeout: int = 30
    half_open_max_calls: int = 3

    state: CircuitBreakerState = CircuitBreakerState.CLOSED
    failure_count: int = 0
    success_count: int = 0
    last_failure_time: Optional[datetime] = None
    half_open_calls: int = 0

    def record_success(self):
        """Registra sucesso"""
        if self.state == CircuitBreakerState.HALF_OPEN:
            self.success_count += 1
            if self.success_count >= self.half_open_max_calls:
                self.state = CircuitBreakerState.CLOSED
                self.failure_count = 0
                self.success_count = 0
                self.half_open_calls = 0
        else:
            self.failure_count = 0

    def record_failure(self):
        """Registra falha"""
        self.failure_count += 1
        self.last_failure_time = datetime.utcnow()

        if self.failure_count >= self.failure_threshold:
            self.state = CircuitBreakerState.OPEN
            logger.warning(f"Circuit breaker aberto apos {self.failure_count} falhas")

    def can_execute(self) -> bool:
        """Verifica se pode executar"""
        if self.state == CircuitBreakerState.CLOSED:
            return True

        if self.state == CircuitBreakerState.OPEN:
            if self.last_failure_time:
                elapsed = (datetime.utcnow() - self.last_failure_time).total_seconds()
                if elapsed >= self.recovery_timeout:
                    self.state = CircuitBreakerState.HALF_OPEN
                    self.half_open_calls = 0
                    return True
            return False

        if self.state == CircuitBreakerState.HALF_OPEN:
            if self.half_open_calls < self.half_open_max_calls:
                self.half_open_calls += 1
                return True
            return False

        return False


class IntegrationAuditLogger:
    """Logger de auditoria para integracoes"""

    def __init__(self, tenant_id: str):
        self.tenant_id = tenant_id
        self._logs: List[IntegrationCallLog] = []
        self._max_logs = 10000

    def log_call(
        self,
        integration: IntegrationType,
        endpoint: str,
        method: str,
        status_code: int,
        duration_ms: int,
        user_id: Optional[str] = None,
        request_size: int = 0,
        response_size: int = 0,
        error: Optional[str] = None,
        metadata: Optional[Dict] = None
    ) -> IntegrationCallLog:
        """Registra chamada de integracao"""
        import uuid

        log = IntegrationCallLog(
            id=str(uuid.uuid4()),
            tenant_id=self.tenant_id,
            integration=integration.value,
            endpoint=endpoint,
            method=method,
            status_code=status_code,
            duration_ms=duration_ms,
            timestamp=datetime.utcnow(),
            user_id=user_id,
            request_size=request_size,
            response_size=response_size,
            error=error,
            metadata=metadata or {}
        )

        self._logs.append(log)

        # Manter limite
        if len(self._logs) > self._max_logs:
            self._logs = self._logs[-self._max_logs:]

        # Log estruturado
        logger.info(
            f"[Audit] {self.tenant_id} | {integration.value} | "
            f"{method} {endpoint} | {status_code} | {duration_ms}ms"
        )

        return log

    def get_logs(
        self,
        integration: Optional[IntegrationType] = None,
        from_date: Optional[datetime] = None,
        to_date: Optional[datetime] = None,
        limit: int = 100
    ) -> List[IntegrationCallLog]:
        """Retorna logs filtrados"""
        logs = self._logs

        if integration:
            logs = [l for l in logs if l.integration == integration.value]

        if from_date:
            logs = [l for l in logs if l.timestamp >= from_date]

        if to_date:
            logs = [l for l in logs if l.timestamp <= to_date]

        return logs[-limit:]

    def get_metrics(
        self,
        integration: Optional[IntegrationType] = None,
        period_minutes: int = 60
    ) -> Dict:
        """Retorna metricas agregadas"""
        cutoff = datetime.utcnow() - timedelta(minutes=period_minutes)

        logs = [l for l in self._logs if l.timestamp >= cutoff]

        if integration:
            logs = [l for l in logs if l.integration == integration.value]

        if not logs:
            return {
                "total_calls": 0,
                "success_rate": 0,
                "avg_duration_ms": 0,
                "error_count": 0
            }

        total = len(logs)
        errors = sum(1 for l in logs if l.error or l.status_code >= 400)
        total_duration = sum(l.duration_ms for l in logs)

        return {
            "total_calls": total,
            "success_rate": (total - errors) / total * 100,
            "avg_duration_ms": total_duration / total,
            "error_count": errors,
            "calls_per_minute": total / period_minutes
        }


class TenantIsolatedGateway(IntegrationGateway):
    """
    Gateway com isolamento completo de tenant.

    Adiciona sobre IntegrationGateway:
    - Contexto de tenant
    - Circuit breaker por integracao
    - Auditoria completa
    - Metricas por tenant
    - Validacao de permissoes
    """

    def __init__(self, tenant_id: str, context: Optional[TenantContext] = None):
        super().__init__(tenant_id)
        self.context = context or TenantContext(tenant_id=tenant_id)
        self._circuit_breakers: Dict[IntegrationType, CircuitBreaker] = {}
        self._audit_logger = IntegrationAuditLogger(tenant_id)

    def _get_circuit_breaker(self, integration: IntegrationType) -> CircuitBreaker:
        """Retorna circuit breaker para integracao"""
        if integration not in self._circuit_breakers:
            self._circuit_breakers[integration] = CircuitBreaker()
        return self._circuit_breakers[integration]

    def check_permission(self, integration: IntegrationType, action: str) -> bool:
        """Verifica permissao do tenant"""
        return self.context.has_permission(integration, action)

    async def proxy_request(self, request: ProxyRequest) -> ProxyResponse:
        """Proxy com isolamento de tenant, circuit breaker e auditoria"""
        start_time = datetime.utcnow()

        # Verificar circuit breaker
        cb = self._get_circuit_breaker(request.integration)
        if not cb.can_execute():
            logger.warning(f"[Gateway] Circuit breaker aberto para {request.integration.value}")
            return ProxyResponse(
                success=False,
                status_code=503,
                error="Service temporarily unavailable (circuit breaker open)",
                duration_ms=0
            )

        # Verificar permissao
        if not self.check_permission(request.integration, request.method.lower()):
            logger.warning(f"[Gateway] Permissao negada para {self.tenant_id}: {request.integration.value}")
            return ProxyResponse(
                success=False,
                status_code=403,
                error="Permission denied",
                duration_ms=0
            )

        # Executar request base
        response = await super().proxy_request(request)

        # Calcular duracao
        duration = int((datetime.utcnow() - start_time).total_seconds() * 1000)

        # Atualizar circuit breaker
        if response.success:
            cb.record_success()
        else:
            cb.record_failure()

        # Registrar auditoria
        self._audit_logger.log_call(
            integration=request.integration,
            endpoint=request.endpoint,
            method=request.method,
            status_code=response.status_code,
            duration_ms=duration,
            user_id=self.context.user_id,
            error=response.error,
            metadata={
                "tenant_id": self.tenant_id,
                "cached": response.cached
            }
        )

        return response

    def get_audit_logs(
        self,
        integration: Optional[IntegrationType] = None,
        limit: int = 100
    ) -> List[Dict]:
        """Retorna logs de auditoria"""
        logs = self._audit_logger.get_logs(integration=integration, limit=limit)
        return [log.to_dict() for log in logs]

    def get_metrics(
        self,
        integration: Optional[IntegrationType] = None,
        period_minutes: int = 60
    ) -> Dict:
        """Retorna metricas do tenant"""
        return self._audit_logger.get_metrics(
            integration=integration,
            period_minutes=period_minutes
        )

    def get_circuit_breaker_status(self) -> Dict[str, Dict]:
        """Retorna status dos circuit breakers"""
        status = {}
        for int_type, cb in self._circuit_breakers.items():
            status[int_type.value] = {
                "state": cb.state.value,
                "failure_count": cb.failure_count,
                "last_failure": cb.last_failure_time.isoformat() if cb.last_failure_time else None
            }
        return status

    async def health_check(self, integration: Optional[IntegrationType] = None) -> Dict[str, Any]:
        """Health check com status de circuit breaker"""
        base_health = await super().health_check(integration)

        # Adicionar status de circuit breaker
        base_health["circuit_breakers"] = self.get_circuit_breaker_status()

        # Adicionar metricas
        base_health["metrics"] = self.get_metrics(integration)

        return base_health


# Cache de gateways isolados por tenant
_isolated_gateways: Dict[str, TenantIsolatedGateway] = {}


def get_isolated_gateway(
    tenant_id: str,
    context: Optional[TenantContext] = None
) -> TenantIsolatedGateway:
    """
    Retorna gateway isolado para o tenant.

    Args:
        tenant_id: ID do tenant
        context: Contexto opcional do tenant

    Returns:
        TenantIsolatedGateway isolado
    """
    if tenant_id not in _isolated_gateways:
        _isolated_gateways[tenant_id] = TenantIsolatedGateway(tenant_id, context)
    elif context:
        # Atualizar contexto se fornecido
        _isolated_gateways[tenant_id].context = context

    return _isolated_gateways[tenant_id]


def clear_tenant_gateway(tenant_id: str):
    """Remove gateway do tenant do cache"""
    if tenant_id in _isolated_gateways:
        del _isolated_gateways[tenant_id]

    if tenant_id in _gateways:
        del _gateways[tenant_id]


async def get_all_tenant_metrics(period_minutes: int = 60) -> Dict[str, Dict]:
    """Retorna metricas de todos os tenants"""
    metrics = {}
    for tenant_id, gateway in _isolated_gateways.items():
        metrics[tenant_id] = gateway.get_metrics(period_minutes=period_minutes)
    return metrics
