# -*- coding: utf-8 -*-
"""
Integration Gateway Module
==========================
Gateway centralizado para integracoes com isolamento de tenant.

Funcionalidades:
- Factory de conectores com isolamento por tenant
- Proxy seguro para integracoes externas
- Rate limiting por tenant e integracao
- Logging e auditoria de operacoes
- Cache de credenciais por tenant

Configuracao via variaveis de ambiente ou banco de dados.
"""

import os
import json
import logging
import asyncio
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Type
from enum import Enum
import aiohttp

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
