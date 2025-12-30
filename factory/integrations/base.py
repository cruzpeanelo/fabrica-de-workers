# -*- coding: utf-8 -*-
"""
Base Integration Module
=======================
Classes base para integracoes com sistemas externos.

Terminal 5 - Issue #302: Adicionado OAuthTokenManager para token refresh padronizado.
"""

import asyncio
import threading
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Tuple
import logging

logger = logging.getLogger(__name__)


class IntegrationStatus(str, Enum):
    """Status da integracao"""
    DISCONNECTED = "disconnected"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    SYNCING = "syncing"
    ERROR = "error"


@dataclass
class SyncResult:
    """Resultado de uma sincronizacao"""
    success: bool
    items_synced: int = 0
    items_created: int = 0
    items_updated: int = 0
    items_failed: int = 0
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    details: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "items_synced": self.items_synced,
            "items_created": self.items_created,
            "items_updated": self.items_updated,
            "items_failed": self.items_failed,
            "errors": self.errors,
            "warnings": self.warnings,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "details": self.details
        }


@dataclass
class IntegrationConfig:
    """Configuracao base para integracoes"""
    tenant_id: str = ""  # ID do tenant para isolamento multi-tenant
    enabled: bool = False
    last_sync: Optional[datetime] = None
    sync_interval_minutes: int = 30
    auto_sync: bool = False
    api_version: str = ""  # Versao da API (quando aplicavel)


class IntegrationBase(ABC):
    """Classe base abstrata para integracoes"""

    def __init__(self, config: IntegrationConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        return self.status == IntegrationStatus.CONNECTED

    @property
    def last_error(self) -> Optional[str]:
        return self._last_error

    @abstractmethod
    async def connect(self) -> bool:
        """Conecta ao sistema externo"""
        pass

    @abstractmethod
    async def disconnect(self) -> bool:
        """Desconecta do sistema externo"""
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """Testa a conexao"""
        pass

    @abstractmethod
    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Sincroniza stories locais para o sistema externo"""
        pass

    @abstractmethod
    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Sincroniza do sistema externo para stories locais"""
        pass

    @abstractmethod
    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook do sistema externo"""
        pass

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "status": self.status.value,
            "connected": self.is_connected,
            "last_error": self._last_error,
            "last_sync": self.config.last_sync.isoformat() if self.config.last_sync else None,
            "auto_sync": self.config.auto_sync,
            "sync_interval_minutes": self.config.sync_interval_minutes
        }


# Status mapping - mapeamento entre status interno e sistemas externos
STATUS_MAPPING_TO_INTERNAL = {
    # Jira status -> Internal status
    "jira": {
        "To Do": "backlog",
        "Open": "backlog",
        "Backlog": "backlog",
        "Ready": "ready",
        "Ready for Development": "ready",
        "In Progress": "in_progress",
        "In Development": "in_progress",
        "In Review": "review",
        "Code Review": "review",
        "Testing": "testing",
        "QA": "testing",
        "Done": "done",
        "Closed": "done",
        "Resolved": "done"
    },
    # Azure DevOps status -> Internal status
    "azure_devops": {
        "New": "backlog",
        "Approved": "ready",
        "Committed": "ready",
        "Active": "in_progress",
        "In Progress": "in_progress",
        "Resolved": "review",
        "Review": "review",
        "Testing": "testing",
        "Done": "done",
        "Closed": "done",
        "Removed": "done"
    }
}

STATUS_MAPPING_TO_EXTERNAL = {
    # Internal status -> Jira status
    "jira": {
        "backlog": "To Do",
        "ready": "Ready for Development",
        "in_progress": "In Progress",
        "review": "In Review",
        "testing": "Testing",
        "done": "Done"
    },
    # Internal status -> Azure DevOps status
    "azure_devops": {
        "backlog": "New",
        "ready": "Approved",
        "in_progress": "Active",
        "review": "Resolved",
        "testing": "Testing",
        "done": "Done"
    }
}


def map_status_to_internal(external_status: str, system: str) -> str:
    """Mapeia status externo para status interno"""
    mapping = STATUS_MAPPING_TO_INTERNAL.get(system, {})
    return mapping.get(external_status, "backlog")


def map_status_to_external(internal_status: str, system: str) -> str:
    """Mapeia status interno para status externo"""
    mapping = STATUS_MAPPING_TO_EXTERNAL.get(system, {})
    return mapping.get(internal_status, "To Do" if system == "jira" else "New")


# Priority mapping
PRIORITY_MAPPING_TO_INTERNAL = {
    "jira": {
        "Highest": "urgent",
        "High": "high",
        "Medium": "medium",
        "Low": "low",
        "Lowest": "low"
    },
    "azure_devops": {
        "1": "urgent",
        "2": "high",
        "3": "medium",
        "4": "low"
    }
}

PRIORITY_MAPPING_TO_EXTERNAL = {
    "jira": {
        "urgent": "Highest",
        "high": "High",
        "medium": "Medium",
        "low": "Low"
    },
    "azure_devops": {
        "urgent": "1",
        "high": "2",
        "medium": "3",
        "low": "4"
    }
}


def map_priority_to_internal(external_priority: str, system: str) -> str:
    """Mapeia prioridade externa para prioridade interna"""
    mapping = PRIORITY_MAPPING_TO_INTERNAL.get(system, {})
    return mapping.get(external_priority, "medium")


def map_priority_to_external(internal_priority: str, system: str) -> str:
    """Mapeia prioridade interna para prioridade externa"""
    mapping = PRIORITY_MAPPING_TO_EXTERNAL.get(system, {})
    return mapping.get(internal_priority, "Medium" if system == "jira" else "3")


# =============================================================================
# OAuth Token Manager - Terminal 5 Issue #302
# =============================================================================

@dataclass
class OAuthToken:
    """
    Token OAuth com metadados.

    Attributes:
        access_token: Token de acesso
        token_type: Tipo do token (Bearer)
        expires_at: Data/hora de expiracao
        refresh_token: Token de refresh (opcional)
        scope: Escopos do token
        metadata: Metadados adicionais
    """
    access_token: str
    token_type: str = "Bearer"
    expires_at: Optional[datetime] = None
    expires_in: Optional[int] = None
    refresh_token: Optional[str] = None
    scope: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """Calcula expires_at se expires_in fornecido"""
        if self.expires_at is None and self.expires_in:
            self.expires_at = datetime.utcnow() + timedelta(seconds=self.expires_in)

    @property
    def is_expired(self) -> bool:
        """Verifica se o token expirou"""
        if self.expires_at is None:
            return False
        # Considera expirado 60 segundos antes para margem de seguranca
        return datetime.utcnow() >= (self.expires_at - timedelta(seconds=60))

    @property
    def time_until_expiry(self) -> Optional[timedelta]:
        """Retorna tempo ate expiracao"""
        if self.expires_at is None:
            return None
        return self.expires_at - datetime.utcnow()

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "access_token": self.access_token[:20] + "..." if self.access_token else None,
            "token_type": self.token_type,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "is_expired": self.is_expired,
            "has_refresh_token": self.refresh_token is not None,
            "scope": self.scope
        }


class OAuthTokenManager:
    """
    Gerenciador de tokens OAuth com refresh automatico.

    Fornece:
    - Cache de tokens em memoria
    - Refresh automatico antes da expiracao
    - Retry com backoff exponencial
    - Thread-safe para operacoes concorrentes
    - Fallback para re-autenticacao completa

    Exemplo:
        async def authenticate(config):
            # Implementar autenticacao
            return OAuthToken(access_token="...", expires_in=3600)

        async def refresh(refresh_token):
            # Implementar refresh
            return OAuthToken(access_token="...", expires_in=3600)

        manager = OAuthTokenManager(
            authenticate_func=authenticate,
            refresh_func=refresh,
            config=my_config
        )

        # Obter token (automaticamente faz refresh se necessario)
        token = await manager.get_token()
    """

    def __init__(
        self,
        authenticate_func: Callable,
        refresh_func: Optional[Callable] = None,
        config: Any = None,
        token_buffer_seconds: int = 60,
        max_retries: int = 3,
        retry_delay_seconds: float = 1.0,
        tenant_id: Optional[str] = None
    ):
        """
        Inicializa o gerenciador de tokens.

        Args:
            authenticate_func: Funcao async para autenticacao inicial
            refresh_func: Funcao async para refresh de token
            config: Configuracao passada para as funcoes
            token_buffer_seconds: Segundos antes da expiracao para refresh
            max_retries: Numero maximo de tentativas
            retry_delay_seconds: Delay entre tentativas
            tenant_id: ID do tenant para isolamento
        """
        self._authenticate_func = authenticate_func
        self._refresh_func = refresh_func
        self._config = config
        self._token_buffer_seconds = token_buffer_seconds
        self._max_retries = max_retries
        self._retry_delay_seconds = retry_delay_seconds
        self._tenant_id = tenant_id

        self._token: Optional[OAuthToken] = None
        self._lock = asyncio.Lock()
        self._last_refresh: Optional[datetime] = None
        self._refresh_count: int = 0
        self._error_count: int = 0

    async def get_token(self, force_refresh: bool = False) -> OAuthToken:
        """
        Obtem token valido.

        Automaticamente:
        - Autentica se nao houver token
        - Faz refresh se token esta proximo de expirar
        - Retenta em caso de falha

        Args:
            force_refresh: Forcar refresh mesmo se token valido

        Returns:
            OAuthToken valido

        Raises:
            RuntimeError: Se nao conseguir obter token
        """
        async with self._lock:
            # Verificar se precisa de token novo
            needs_new_token = (
                self._token is None or
                force_refresh or
                self._token.is_expired
            )

            if not needs_new_token:
                return self._token

            # Tentar refresh primeiro se tiver refresh_token
            if (
                self._token and
                self._token.refresh_token and
                self._refresh_func
            ):
                try:
                    new_token = await self._try_refresh()
                    if new_token:
                        self._token = new_token
                        self._refresh_count += 1
                        self._last_refresh = datetime.utcnow()
                        logger.info(
                            f"Token refreshed successfully "
                            f"(tenant={self._tenant_id}, count={self._refresh_count})"
                        )
                        return self._token
                except Exception as e:
                    logger.warning(f"Token refresh failed, falling back to auth: {e}")

            # Autenticar do zero
            new_token = await self._try_authenticate()
            if new_token:
                self._token = new_token
                self._last_refresh = datetime.utcnow()
                return self._token

            raise RuntimeError("Failed to obtain OAuth token")

    async def _try_refresh(self) -> Optional[OAuthToken]:
        """Tenta fazer refresh do token"""
        if not self._refresh_func or not self._token or not self._token.refresh_token:
            return None

        for attempt in range(self._max_retries):
            try:
                new_token = await self._refresh_func(
                    self._token.refresh_token,
                    self._config
                )
                return new_token
            except Exception as e:
                self._error_count += 1
                if attempt < self._max_retries - 1:
                    delay = self._retry_delay_seconds * (2 ** attempt)
                    logger.warning(
                        f"Refresh attempt {attempt + 1} failed: {e}. "
                        f"Retrying in {delay}s..."
                    )
                    await asyncio.sleep(delay)
                else:
                    logger.error(f"All refresh attempts failed: {e}")
                    raise

        return None

    async def _try_authenticate(self) -> Optional[OAuthToken]:
        """Tenta autenticar do zero"""
        for attempt in range(self._max_retries):
            try:
                new_token = await self._authenticate_func(self._config)
                return new_token
            except Exception as e:
                self._error_count += 1
                if attempt < self._max_retries - 1:
                    delay = self._retry_delay_seconds * (2 ** attempt)
                    logger.warning(
                        f"Auth attempt {attempt + 1} failed: {e}. "
                        f"Retrying in {delay}s..."
                    )
                    await asyncio.sleep(delay)
                else:
                    logger.error(f"All authentication attempts failed: {e}")
                    raise

        return None

    async def invalidate(self):
        """Invalida o token atual (forca re-autenticacao no proximo get_token)"""
        async with self._lock:
            self._token = None
            logger.info(f"Token invalidated (tenant={self._tenant_id})")

    async def get_access_token(self) -> str:
        """Retorna apenas o access_token string"""
        token = await self.get_token()
        return token.access_token

    async def get_authorization_header(self) -> str:
        """Retorna header Authorization formatado"""
        token = await self.get_token()
        return f"{token.token_type} {token.access_token}"

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do token manager"""
        return {
            "has_token": self._token is not None,
            "is_expired": self._token.is_expired if self._token else None,
            "expires_at": self._token.expires_at.isoformat() if self._token and self._token.expires_at else None,
            "time_until_expiry_seconds": self._token.time_until_expiry.total_seconds() if self._token and self._token.time_until_expiry else None,
            "refresh_count": self._refresh_count,
            "error_count": self._error_count,
            "last_refresh": self._last_refresh.isoformat() if self._last_refresh else None,
            "tenant_id": self._tenant_id
        }


class OAuthClientBase(ABC):
    """
    Classe base para clientes OAuth.

    Fornece integracao automatica com OAuthTokenManager.

    Exemplo de uso:
        class SalesforceClient(OAuthClientBase):
            async def _authenticate(self, config):
                # Implementar autenticacao Salesforce
                return OAuthToken(...)

            async def _refresh_token(self, refresh_token, config):
                # Implementar refresh Salesforce
                return OAuthToken(...)
    """

    def __init__(self, config: Any, tenant_id: Optional[str] = None):
        """
        Inicializa o cliente OAuth.

        Args:
            config: Configuracao do cliente
            tenant_id: ID do tenant para isolamento
        """
        self._config = config
        self._tenant_id = tenant_id
        self._token_manager: Optional[OAuthTokenManager] = None

    def _ensure_token_manager(self) -> OAuthTokenManager:
        """Garante que o token manager esta inicializado"""
        if self._token_manager is None:
            self._token_manager = OAuthTokenManager(
                authenticate_func=self._authenticate,
                refresh_func=self._refresh_token if hasattr(self, '_refresh_token') else None,
                config=self._config,
                tenant_id=self._tenant_id
            )
        return self._token_manager

    @abstractmethod
    async def _authenticate(self, config: Any) -> OAuthToken:
        """
        Implementar autenticacao OAuth.

        Args:
            config: Configuracao do cliente

        Returns:
            OAuthToken com access_token
        """
        pass

    async def _refresh_token(
        self,
        refresh_token: str,
        config: Any
    ) -> OAuthToken:
        """
        Implementar refresh de token (opcional).

        Args:
            refresh_token: Refresh token
            config: Configuracao do cliente

        Returns:
            OAuthToken com novo access_token
        """
        # Default: nao suporta refresh
        raise NotImplementedError("Token refresh not supported")

    async def get_token(self, force_refresh: bool = False) -> OAuthToken:
        """Obtem token valido"""
        manager = self._ensure_token_manager()
        return await manager.get_token(force_refresh)

    async def get_authorization_header(self) -> str:
        """Retorna header Authorization"""
        manager = self._ensure_token_manager()
        return await manager.get_authorization_header()

    async def invalidate_token(self):
        """Invalida token atual"""
        if self._token_manager:
            await self._token_manager.invalidate()

    def get_token_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do token"""
        if self._token_manager:
            return self._token_manager.get_stats()
        return {"initialized": False}
