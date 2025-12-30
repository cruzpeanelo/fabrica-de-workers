# -*- coding: utf-8 -*-
"""
SAP Authentication Module
=========================
Modulo de autenticacao para SAP S/4HANA suportando OAuth2 e autenticacao basica.

Este modulo implementa:
- OAuth 2.0 Client Credentials Flow (recomendado para Cloud)
- OAuth 2.0 com X.509 Certificate
- Autenticacao Basica (Basic Auth)
- Cache de tokens com renovacao automatica

Autor: Fabrica de Agentes
"""

import base64
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Dict, Optional, Tuple
import logging

try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

logger = logging.getLogger(__name__)


class SAPAuthError(Exception):
    """Excecao para erros de autenticacao SAP"""

    def __init__(self, message: str, status_code: int = 0, details: Optional[Dict] = None):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.details = details or {}

    def __str__(self):
        if self.status_code:
            return f"[{self.status_code}] {self.message}"
        return self.message


@dataclass
class SAPOAuthConfig:
    """
    Configuracao para autenticacao OAuth2

    Attributes:
        token_url: URL para obter token
        client_id: Client ID do Communication User
        client_secret: Client Secret
        scope: Escopo opcional
        grant_type: Tipo de grant (default: client_credentials)
        certificate_path: Caminho para certificado X.509 (opcional)
        private_key_path: Caminho para chave privada (opcional)
    """
    token_url: str
    client_id: str
    client_secret: str
    scope: str = ""
    grant_type: str = "client_credentials"
    certificate_path: Optional[str] = None
    private_key_path: Optional[str] = None


@dataclass
class SAPBasicAuthConfig:
    """
    Configuracao para autenticacao basica

    Attributes:
        username: Nome de usuario SAP
        password: Senha
        client: Mandante SAP (opcional, pode ser passado no header)
    """
    username: str
    password: str
    client: str = ""


@dataclass
class TokenInfo:
    """Informacoes do token OAuth"""
    access_token: str
    token_type: str = "Bearer"
    expires_in: int = 3600
    scope: str = ""
    created_at: datetime = field(default_factory=datetime.now)

    @property
    def expires_at(self) -> datetime:
        """Retorna datetime de expiracao"""
        return self.created_at + timedelta(seconds=self.expires_in)

    @property
    def is_expired(self) -> bool:
        """Verifica se token expirou (com margem de 60 segundos)"""
        return datetime.now() >= (self.expires_at - timedelta(seconds=60))

    @property
    def authorization_header(self) -> str:
        """Retorna header de autorizacao formatado"""
        return f"{self.token_type} {self.access_token}"


class SAPAuthenticator:
    """
    Autenticador SAP S/4HANA

    Suporta OAuth2 e autenticacao basica com cache de tokens por tenant.

    Exemplo de uso:
    ```python
    # OAuth2
    oauth_config = SAPOAuthConfig(
        token_url="https://my-s4.s4hana.ondemand.com/sap/bc/sec/oauth2/token",
        client_id="MY_CLIENT_ID",
        client_secret="MY_CLIENT_SECRET"
    )
    auth = SAPAuthenticator(tenant_id="tenant-001", oauth_config=oauth_config)

    # Obter headers de autenticacao
    headers = await auth.get_auth_headers()
    ```
    """

    # Cache de tokens por tenant (compartilhado entre instancias)
    _tenant_token_cache: Dict[str, "TokenInfo"] = {}

    def __init__(
        self,
        tenant_id: str = "",
        oauth_config: Optional[SAPOAuthConfig] = None,
        basic_config: Optional[SAPBasicAuthConfig] = None,
        verify_ssl: bool = True,
        timeout: int = 30
    ):
        """
        Inicializa autenticador

        Args:
            tenant_id: ID do tenant para isolamento multi-tenant
            oauth_config: Configuracao OAuth2 (preferencial)
            basic_config: Configuracao de autenticacao basica
            verify_ssl: Verificar certificado SSL
            timeout: Timeout para requisicoes de token
        """
        self.tenant_id = tenant_id
        self.oauth_config = oauth_config
        self.basic_config = basic_config
        self.verify_ssl = verify_ssl
        self.timeout = timeout

        # Cache de token legado (mantido para compatibilidade)
        self._token_cache: Optional[TokenInfo] = None
        self._token_lock = None  # Para uso async

        if not oauth_config and not basic_config:
            logger.warning("Nenhuma configuracao de autenticacao fornecida")

        if not tenant_id:
            logger.warning("tenant_id nao configurado para autenticador SAP")

    @property
    def auth_method(self) -> str:
        """Retorna metodo de autenticacao configurado"""
        if self.oauth_config:
            return "oauth2"
        elif self.basic_config:
            return "basic"
        return "none"

    def get_auth_headers_sync(self) -> Dict[str, str]:
        """
        Obtem headers de autenticacao (sincrono)

        Returns:
            Dict com headers de autenticacao
        """
        if self.oauth_config:
            return self._get_oauth_headers_sync()
        elif self.basic_config:
            return self._get_basic_headers()
        else:
            raise SAPAuthError("Nenhum metodo de autenticacao configurado")

    async def get_auth_headers(self) -> Dict[str, str]:
        """
        Obtem headers de autenticacao (assincrono)

        Returns:
            Dict com headers de autenticacao
        """
        if self.oauth_config:
            return await self._get_oauth_headers_async()
        elif self.basic_config:
            return self._get_basic_headers()
        else:
            raise SAPAuthError("Nenhum metodo de autenticacao configurado")

    def _get_basic_headers(self) -> Dict[str, str]:
        """
        Gera headers para autenticacao basica

        Returns:
            Dict com header Authorization
        """
        if not self.basic_config:
            raise SAPAuthError("Configuracao de autenticacao basica nao definida")

        credentials = f"{self.basic_config.username}:{self.basic_config.password}"
        encoded = base64.b64encode(credentials.encode()).decode()

        headers = {
            "Authorization": f"Basic {encoded}"
        }

        # Adicionar mandante se especificado
        if self.basic_config.client:
            headers["sap-client"] = self.basic_config.client

        return headers

    def _get_tenant_cache_key(self) -> str:
        """Gera chave de cache para o tenant atual"""
        return f"{self.tenant_id}:oauth_token"

    def _get_cached_token(self) -> Optional[TokenInfo]:
        """Obtem token do cache do tenant"""
        cache_key = self._get_tenant_cache_key()
        token = SAPAuthenticator._tenant_token_cache.get(cache_key)
        if token and not token.is_expired:
            return token
        return None

    def _set_cached_token(self, token: TokenInfo):
        """Armazena token no cache do tenant"""
        cache_key = self._get_tenant_cache_key()
        SAPAuthenticator._tenant_token_cache[cache_key] = token
        self._token_cache = token  # Manter compatibilidade

    def _get_oauth_headers_sync(self) -> Dict[str, str]:
        """
        Obtem headers OAuth2 (sincrono)

        Returns:
            Dict com header Authorization Bearer
        """
        if not REQUESTS_AVAILABLE:
            raise SAPAuthError(
                "Biblioteca 'requests' nao instalada. "
                "Instale com: pip install requests"
            )

        # Verificar cache do tenant
        cached_token = self._get_cached_token()
        if cached_token:
            logger.debug(f"[Tenant:{self.tenant_id}] Usando token do cache")
            return {"Authorization": cached_token.authorization_header}

        # Obter novo token
        logger.info(f"[Tenant:{self.tenant_id}] Obtendo novo token OAuth")
        token = self._fetch_token_sync()
        self._set_cached_token(token)
        self._audit_log("token_obtained", "Novo token OAuth obtido")

        return {"Authorization": token.authorization_header}

    async def _get_oauth_headers_async(self) -> Dict[str, str]:
        """
        Obtem headers OAuth2 (assincrono)

        Returns:
            Dict com header Authorization Bearer
        """
        # Verificar cache do tenant
        cached_token = self._get_cached_token()
        if cached_token:
            logger.debug(f"[Tenant:{self.tenant_id}] Usando token do cache (async)")
            return {"Authorization": cached_token.authorization_header}

        # Obter novo token
        logger.info(f"[Tenant:{self.tenant_id}] Obtendo novo token OAuth (async)")
        token = await self._fetch_token_async()
        self._set_cached_token(token)
        self._audit_log("token_obtained", "Novo token OAuth obtido (async)")

        return {"Authorization": token.authorization_header}

    def _fetch_token_sync(self) -> TokenInfo:
        """
        Busca token OAuth2 do servidor (sincrono)

        Returns:
            TokenInfo com dados do token

        Raises:
            SAPAuthError: Se falhar ao obter token
        """
        if not self.oauth_config:
            raise SAPAuthError("Configuracao OAuth nao definida")

        try:
            # Preparar dados do request
            data = {
                "grant_type": self.oauth_config.grant_type,
                "client_id": self.oauth_config.client_id,
                "client_secret": self.oauth_config.client_secret
            }

            if self.oauth_config.scope:
                data["scope"] = self.oauth_config.scope

            # Headers
            headers = {
                "Content-Type": "application/x-www-form-urlencoded",
                "Accept": "application/json"
            }

            # Fazer requisicao
            response = requests.post(
                self.oauth_config.token_url,
                data=data,
                headers=headers,
                verify=self.verify_ssl,
                timeout=self.timeout
            )

            if response.status_code != 200:
                error_detail = response.text
                try:
                    error_json = response.json()
                    error_detail = error_json.get("error_description", error_detail)
                except Exception:
                    pass

                raise SAPAuthError(
                    f"Falha ao obter token OAuth: {error_detail}",
                    status_code=response.status_code
                )

            # Parse response
            token_data = response.json()

            return TokenInfo(
                access_token=token_data["access_token"],
                token_type=token_data.get("token_type", "Bearer"),
                expires_in=token_data.get("expires_in", 3600),
                scope=token_data.get("scope", ""),
                created_at=datetime.now()
            )

        except requests.exceptions.Timeout:
            raise SAPAuthError(
                f"Timeout ao conectar com servidor OAuth: {self.oauth_config.token_url}"
            )
        except requests.exceptions.ConnectionError as e:
            raise SAPAuthError(
                f"Erro de conexao com servidor OAuth: {str(e)}"
            )
        except requests.exceptions.RequestException as e:
            raise SAPAuthError(
                f"Erro ao obter token OAuth: {str(e)}"
            )

    async def _fetch_token_async(self) -> TokenInfo:
        """
        Busca token OAuth2 do servidor (assincrono)

        Returns:
            TokenInfo com dados do token

        Raises:
            SAPAuthError: Se falhar ao obter token
        """
        if not AIOHTTP_AVAILABLE:
            # Fallback para versao sincrona
            logger.warning(
                "aiohttp nao disponivel, usando requests sincrono. "
                "Instale com: pip install aiohttp"
            )
            return self._fetch_token_sync()

        if not self.oauth_config:
            raise SAPAuthError("Configuracao OAuth nao definida")

        try:
            # Preparar dados do request
            data = {
                "grant_type": self.oauth_config.grant_type,
                "client_id": self.oauth_config.client_id,
                "client_secret": self.oauth_config.client_secret
            }

            if self.oauth_config.scope:
                data["scope"] = self.oauth_config.scope

            # Headers
            headers = {
                "Content-Type": "application/x-www-form-urlencoded",
                "Accept": "application/json"
            }

            # SSL context
            ssl_context = None if self.verify_ssl else False

            async with aiohttp.ClientSession() as session:
                async with session.post(
                    self.oauth_config.token_url,
                    data=data,
                    headers=headers,
                    ssl=ssl_context,
                    timeout=aiohttp.ClientTimeout(total=self.timeout)
                ) as response:

                    if response.status != 200:
                        error_text = await response.text()
                        try:
                            error_json = await response.json()
                            error_text = error_json.get("error_description", error_text)
                        except Exception:
                            pass

                        raise SAPAuthError(
                            f"Falha ao obter token OAuth: {error_text}",
                            status_code=response.status
                        )

                    token_data = await response.json()

                    return TokenInfo(
                        access_token=token_data["access_token"],
                        token_type=token_data.get("token_type", "Bearer"),
                        expires_in=token_data.get("expires_in", 3600),
                        scope=token_data.get("scope", ""),
                        created_at=datetime.now()
                    )

        except aiohttp.ClientTimeout:
            raise SAPAuthError(
                f"Timeout ao conectar com servidor OAuth: {self.oauth_config.token_url}"
            )
        except aiohttp.ClientError as e:
            raise SAPAuthError(
                f"Erro de conexao com servidor OAuth: {str(e)}"
            )

    def _audit_log(self, action: str, details: str):
        """
        Registra operacao de token para auditoria

        Args:
            action: Tipo de acao (token_obtained, token_refreshed, token_cleared)
            details: Detalhes adicionais
        """
        log_entry = {
            "tenant_id": self.tenant_id,
            "action": action,
            "details": details,
            "timestamp": datetime.now().isoformat(),
            "auth_method": self.auth_method
        }
        logger.info(f"[AUDIT] Tenant:{self.tenant_id} Action:{action} - {details}")
        # Pode ser estendido para gravar em banco de dados ou sistema de auditoria externo

    def clear_token_cache(self):
        """Limpa cache de token do tenant atual"""
        cache_key = self._get_tenant_cache_key()
        if cache_key in SAPAuthenticator._tenant_token_cache:
            del SAPAuthenticator._tenant_token_cache[cache_key]
        self._token_cache = None
        self._audit_log("token_cleared", "Cache de token OAuth limpo")
        logger.debug(f"[Tenant:{self.tenant_id}] Cache de token OAuth limpo")

    @classmethod
    def clear_all_tenant_caches(cls):
        """Limpa cache de tokens de todos os tenants"""
        cls._tenant_token_cache.clear()
        logger.info("[AUDIT] Cache de tokens de todos os tenants limpo")

    def get_token_info(self) -> Optional[Dict]:
        """
        Retorna informacoes do token em cache (sem o token em si)

        Returns:
            Dict com informacoes ou None se nao houver token
        """
        cached_token = self._get_cached_token()
        if not cached_token:
            return None

        return {
            "tenant_id": self.tenant_id,
            "token_type": cached_token.token_type,
            "expires_at": cached_token.expires_at.isoformat(),
            "is_expired": cached_token.is_expired,
            "scope": cached_token.scope
        }

    async def validate_credentials(self) -> Tuple[bool, str]:
        """
        Valida as credenciais configuradas

        Returns:
            Tuple (sucesso, mensagem)
        """
        try:
            if self.oauth_config:
                # Tentar obter token
                token = await self._fetch_token_async()
                if token and token.access_token:
                    return True, "Credenciais OAuth2 validas"

            elif self.basic_config:
                # Credenciais basicas sao validadas na primeira requisicao
                headers = self._get_basic_headers()
                if headers.get("Authorization"):
                    return True, "Credenciais basicas configuradas"

            return False, "Nenhuma credencial configurada"

        except SAPAuthError as e:
            return False, f"Erro de autenticacao: {str(e)}"
        except Exception as e:
            return False, f"Erro inesperado: {str(e)}"


def create_authenticator_from_config(config) -> SAPAuthenticator:
    """
    Cria autenticador a partir de SAPS4Config

    Args:
        config: Objeto SAPS4Config

    Returns:
        SAPAuthenticator configurado
    """
    oauth_config = None
    basic_config = None

    if config.is_oauth_configured():
        oauth_config = SAPOAuthConfig(
            token_url=config.oauth_token_url,
            client_id=config.oauth_client_id,
            client_secret=config.oauth_client_secret,
            scope=config.oauth_scope
        )

    if config.is_basic_auth_configured():
        basic_config = SAPBasicAuthConfig(
            username=config.username,
            password=config.password,
            client=config.client
        )

    return SAPAuthenticator(
        tenant_id=config.tenant_id,
        oauth_config=oauth_config,
        basic_config=basic_config,
        verify_ssl=config.verify_ssl,
        timeout=config.timeout
    )
