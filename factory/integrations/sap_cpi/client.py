# -*- coding: utf-8 -*-
"""
SAP CPI Client Module
=====================

Cliente base para comunicacao com SAP Cloud Platform Integration via OData API.

Funcionalidades:
- Autenticacao OAuth 2.0 (Client Credentials Flow)
- Gerenciamento automatico de tokens
- Requisicoes OData com tratamento de erros
- Cache de sessao HTTP

API Endpoints principais:
- /api/v1/IntegrationPackages - Packages de integracao
- /api/v1/IntegrationDesigntimeArtifacts - iFlows
- /api/v1/IntegrationRuntimeArtifacts - Artefatos em runtime
- /api/v1/MessageProcessingLogs - Logs de mensagens
"""

import os
import logging
import aiohttp
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union
from enum import Enum

from ..base import IntegrationConfig, IntegrationStatus

logger = logging.getLogger(__name__)


class CPIArtifactType(str, Enum):
    """Tipos de artefatos no CPI"""
    INTEGRATION_FLOW = "IntegrationFlow"
    VALUE_MAPPING = "ValueMapping"
    SCRIPT_COLLECTION = "ScriptCollection"
    MESSAGE_MAPPING = "MessageMapping"
    FUNCTION_LIBRARY = "FunctionLibrary"


class CPIArtifactStatus(str, Enum):
    """Status de artefatos no runtime"""
    STARTED = "STARTED"
    STOPPED = "STOPPED"
    ERROR = "ERROR"
    STARTING = "STARTING"
    STOPPING = "STOPPING"


class CPIMessageStatus(str, Enum):
    """Status de processamento de mensagens"""
    COMPLETED = "COMPLETED"
    FAILED = "FAILED"
    PROCESSING = "PROCESSING"
    RETRY = "RETRY"
    ESCALATED = "ESCALATED"
    DISCARDED = "DISCARDED"


@dataclass
class SAPCPIConfig(IntegrationConfig):
    """
    Configuracao para conexao com SAP CPI.

    Atributos:
        tenant_url: URL do tenant CPI (ex: https://xxx.it-cpi001.cfapps.us10.hana.ondemand.com)
        token_url: URL do OAuth token endpoint
        client_id: Client ID para autenticacao OAuth
        client_secret: Client Secret para autenticacao OAuth
        api_version: Versao da API (padrao: 1.0)
        timeout_seconds: Timeout para requisicoes HTTP
        max_retries: Numero maximo de tentativas em caso de erro
    """
    tenant_url: str = ""
    token_url: str = ""
    client_id: str = ""
    client_secret: str = ""
    api_version: str = "1.0"
    timeout_seconds: int = 30
    max_retries: int = 3
    verify_ssl: bool = True

    @classmethod
    def from_env(cls) -> "SAPCPIConfig":
        """
        Cria configuracao a partir de variaveis de ambiente.

        Variaveis esperadas:
        - SAP_CPI_ENABLED: Habilitar integracao (true/false)
        - SAP_CPI_TENANT_URL: URL do tenant CPI
        - SAP_CPI_TOKEN_URL: URL do OAuth token endpoint
        - SAP_CPI_CLIENT_ID: Client ID OAuth
        - SAP_CPI_CLIENT_SECRET: Client Secret OAuth
        - SAP_CPI_API_VERSION: Versao da API (opcional)
        - SAP_CPI_TIMEOUT: Timeout em segundos (opcional)
        - SAP_CPI_AUTO_SYNC: Sincronizacao automatica (opcional)
        - SAP_CPI_SYNC_INTERVAL: Intervalo de sync em minutos (opcional)
        """
        return cls(
            enabled=os.getenv("SAP_CPI_ENABLED", "false").lower() == "true",
            tenant_url=os.getenv("SAP_CPI_TENANT_URL", "").rstrip("/"),
            token_url=os.getenv("SAP_CPI_TOKEN_URL", ""),
            client_id=os.getenv("SAP_CPI_CLIENT_ID", ""),
            client_secret=os.getenv("SAP_CPI_CLIENT_SECRET", ""),
            api_version=os.getenv("SAP_CPI_API_VERSION", "1.0"),
            timeout_seconds=int(os.getenv("SAP_CPI_TIMEOUT", "30")),
            max_retries=int(os.getenv("SAP_CPI_MAX_RETRIES", "3")),
            verify_ssl=os.getenv("SAP_CPI_VERIFY_SSL", "true").lower() == "true",
            auto_sync=os.getenv("SAP_CPI_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("SAP_CPI_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao tem todos os campos obrigatorios"""
        return bool(
            self.tenant_url and
            self.token_url and
            self.client_id and
            self.client_secret
        )

    def to_dict(self) -> Dict[str, Any]:
        """Converte configuracao para dicionario (sem secrets)"""
        return {
            "enabled": self.enabled,
            "tenant_url": self.tenant_url,
            "token_url": self.token_url,
            "client_id": self.client_id[:8] + "..." if self.client_id else "",
            "api_version": self.api_version,
            "timeout_seconds": self.timeout_seconds,
            "max_retries": self.max_retries,
            "verify_ssl": self.verify_ssl,
            "auto_sync": self.auto_sync,
            "sync_interval_minutes": self.sync_interval_minutes
        }


@dataclass
class OAuthToken:
    """Token OAuth com gerenciamento de expiracao"""
    access_token: str
    token_type: str
    expires_in: int
    issued_at: datetime = field(default_factory=datetime.utcnow)
    scope: str = ""

    @property
    def expires_at(self) -> datetime:
        """Data/hora de expiracao do token"""
        return self.issued_at + timedelta(seconds=self.expires_in)

    @property
    def is_expired(self) -> bool:
        """Verifica se o token expirou (com margem de 60s)"""
        return datetime.utcnow() >= self.expires_at - timedelta(seconds=60)


class SAPCPIClient:
    """
    Cliente HTTP para comunicacao com SAP CPI.

    Gerencia autenticacao OAuth 2.0 e requisicoes OData.

    Exemplo:
    ```python
    client = SAPCPIClient(config)
    if await client.connect():
        packages = await client.get("/IntegrationPackages")
        print(packages)
    ```
    """

    # Endpoints OData principais
    ENDPOINTS = {
        "packages": "/api/v1/IntegrationPackages",
        "designtime": "/api/v1/IntegrationDesigntimeArtifacts",
        "runtime": "/api/v1/IntegrationRuntimeArtifacts",
        "logs": "/api/v1/MessageProcessingLogs",
        "log_runs": "/api/v1/MessageProcessingLogRuns",
        "configurations": "/api/v1/Configurations",
        "security_materials": "/api/v1/SecurityArtifactDescriptors",
        "value_mappings": "/api/v1/ValueMappingDesigntimeArtifacts",
        "custom_tags": "/api/v1/CustomTagConfigurations",
        "build_deploy": "/api/v1/BuildAndDeployStatus",
    }

    def __init__(self, config: SAPCPIConfig):
        """
        Inicializa o cliente CPI.

        Args:
            config: Configuracao SAPCPIConfig
        """
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._session: Optional[aiohttp.ClientSession] = None
        self._token: Optional[OAuthToken] = None
        self._last_error: Optional[str] = None

    @property
    def base_url(self) -> str:
        """URL base para requisicoes OData"""
        return self.config.tenant_url

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado e com token valido"""
        return (
            self.status == IntegrationStatus.CONNECTED and
            self._token is not None and
            not self._token.is_expired
        )

    @property
    def last_error(self) -> Optional[str]:
        """Ultimo erro ocorrido"""
        return self._last_error

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            connector = aiohttp.TCPConnector(ssl=self.config.verify_ssl)
            timeout = aiohttp.ClientTimeout(total=self.config.timeout_seconds)
            self._session = aiohttp.ClientSession(
                connector=connector,
                timeout=timeout
            )
        return self._session

    async def _get_oauth_token(self) -> Optional[OAuthToken]:
        """
        Obtem token OAuth via Client Credentials Flow.

        Returns:
            OAuthToken ou None se falhar
        """
        try:
            session = await self._ensure_session()

            # Dados para OAuth Client Credentials
            data = {
                "grant_type": "client_credentials",
                "client_id": self.config.client_id,
                "client_secret": self.config.client_secret
            }

            async with session.post(
                self.config.token_url,
                data=data,
                headers={"Content-Type": "application/x-www-form-urlencoded"}
            ) as response:
                if response.status == 200:
                    token_data = await response.json()
                    return OAuthToken(
                        access_token=token_data["access_token"],
                        token_type=token_data.get("token_type", "Bearer"),
                        expires_in=token_data.get("expires_in", 3600),
                        scope=token_data.get("scope", "")
                    )
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro OAuth {response.status}: {error_text}"
                    logger.error(self._last_error)

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao OAuth: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado OAuth: {str(e)}"
            logger.error(self._last_error)

        return None

    async def _ensure_token(self) -> bool:
        """Garante que existe um token valido, renovando se necessario"""
        if self._token is None or self._token.is_expired:
            self._token = await self._get_oauth_token()
        return self._token is not None

    def _get_headers(self, content_type: str = "application/json") -> Dict[str, str]:
        """
        Retorna headers para requisicoes.

        Args:
            content_type: Tipo de conteudo (application/json, application/xml, etc)
        """
        headers = {
            "Accept": "application/json",
            "Content-Type": content_type,
        }

        if self._token:
            headers["Authorization"] = f"{self._token.token_type} {self._token.access_token}"

        return headers

    async def connect(self) -> bool:
        """
        Conecta ao SAP CPI obtendo token OAuth.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique tenant_url, token_url, client_id e client_secret."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao SAP CPI: {self.config.tenant_url}")

        try:
            # Obtem token OAuth
            self._token = await self._get_oauth_token()

            if self._token:
                # Testa conexao listando packages
                test = await self.get(self.ENDPOINTS["packages"], params={"$top": 1})
                if test is not None:
                    self.status = IntegrationStatus.CONNECTED
                    logger.info("Conectado ao SAP CPI com sucesso")
                    return True
                else:
                    self._last_error = "Token obtido mas falha ao acessar API"

        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        self.status = IntegrationStatus.ERROR
        return False

    async def disconnect(self) -> bool:
        """
        Desconecta do SAP CPI.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._token = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do SAP CPI")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o SAP CPI.

        Returns:
            bool: True se a conexao esta funcionando
        """
        try:
            if not await self._ensure_token():
                return False

            result = await self.get(self.ENDPOINTS["packages"], params={"$top": 1})
            return result is not None
        except Exception:
            return False

    async def get(
        self,
        endpoint: str,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Executa requisicao GET.

        Args:
            endpoint: Endpoint relativo (ex: /api/v1/IntegrationPackages)
            params: Parametros de query (OData $select, $filter, etc)
            headers: Headers adicionais

        Returns:
            Dict com resposta ou None se erro
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            request_headers = self._get_headers()
            if headers:
                request_headers.update(headers)

            async with session.get(url, params=params, headers=request_headers) as response:
                if response.status == 200:
                    return await response.json()
                elif response.status == 401:
                    # Token pode ter expirado, tenta renovar
                    self._token = None
                    if await self._ensure_token():
                        return await self.get(endpoint, params, headers)
                    self._last_error = "Falha na autenticacao"
                elif response.status == 403:
                    self._last_error = "Acesso negado"
                elif response.status == 404:
                    self._last_error = f"Recurso nao encontrado: {endpoint}"
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

                logger.error(f"GET {endpoint}: {self._last_error}")

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return None

    async def post(
        self,
        endpoint: str,
        data: Optional[Union[Dict, str, bytes]] = None,
        params: Optional[Dict[str, Any]] = None,
        content_type: str = "application/json",
        headers: Optional[Dict[str, str]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Executa requisicao POST.

        Args:
            endpoint: Endpoint relativo
            data: Dados do body (Dict para JSON, str/bytes para outros)
            params: Parametros de query
            content_type: Tipo do conteudo
            headers: Headers adicionais

        Returns:
            Dict com resposta ou None se erro
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            request_headers = self._get_headers(content_type)
            if headers:
                request_headers.update(headers)

            # Prepara dados conforme tipo
            if isinstance(data, dict):
                async with session.post(
                    url, json=data, params=params, headers=request_headers
                ) as response:
                    return await self._handle_response(response, endpoint, "POST")
            else:
                async with session.post(
                    url, data=data, params=params, headers=request_headers
                ) as response:
                    return await self._handle_response(response, endpoint, "POST")

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return None

    async def put(
        self,
        endpoint: str,
        data: Optional[Union[Dict, str, bytes]] = None,
        params: Optional[Dict[str, Any]] = None,
        content_type: str = "application/json",
        headers: Optional[Dict[str, str]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Executa requisicao PUT.

        Args:
            endpoint: Endpoint relativo
            data: Dados do body
            params: Parametros de query
            content_type: Tipo do conteudo
            headers: Headers adicionais

        Returns:
            Dict com resposta ou None se erro
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            request_headers = self._get_headers(content_type)
            if headers:
                request_headers.update(headers)

            if isinstance(data, dict):
                async with session.put(
                    url, json=data, params=params, headers=request_headers
                ) as response:
                    return await self._handle_response(response, endpoint, "PUT")
            else:
                async with session.put(
                    url, data=data, params=params, headers=request_headers
                ) as response:
                    return await self._handle_response(response, endpoint, "PUT")

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return None

    async def delete(
        self,
        endpoint: str,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Executa requisicao DELETE.

        Args:
            endpoint: Endpoint relativo
            params: Parametros de query
            headers: Headers adicionais

        Returns:
            bool: True se deletado com sucesso
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            request_headers = self._get_headers()
            if headers:
                request_headers.update(headers)

            async with session.delete(
                url, params=params, headers=request_headers
            ) as response:
                if response.status in (200, 204):
                    return True
                elif response.status == 404:
                    logger.warning(f"Recurso nao encontrado para deletar: {endpoint}")
                    return False
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro DELETE {response.status}: {error_text}"
                    logger.error(self._last_error)

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return False

    async def download(
        self,
        endpoint: str,
        params: Optional[Dict[str, Any]] = None
    ) -> Optional[bytes]:
        """
        Faz download de conteudo binario.

        Args:
            endpoint: Endpoint relativo
            params: Parametros de query

        Returns:
            bytes com conteudo ou None se erro
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            headers = self._get_headers()
            headers["Accept"] = "application/zip"

            async with session.get(url, params=params, headers=headers) as response:
                if response.status == 200:
                    return await response.read()
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro download {response.status}: {error_text}"
                    logger.error(self._last_error)

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return None

    async def upload(
        self,
        endpoint: str,
        file_content: bytes,
        file_name: str,
        content_type: str = "application/zip",
        params: Optional[Dict[str, Any]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Faz upload de arquivo.

        Args:
            endpoint: Endpoint relativo
            file_content: Conteudo do arquivo em bytes
            file_name: Nome do arquivo
            content_type: Tipo MIME do arquivo
            params: Parametros de query

        Returns:
            Dict com resposta ou None se erro
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}{endpoint}"

            headers = self._get_headers(content_type)
            headers["Slug"] = file_name

            async with session.post(
                url, data=file_content, params=params, headers=headers
            ) as response:
                return await self._handle_response(response, endpoint, "UPLOAD")

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            logger.error(self._last_error)
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.error(self._last_error)

        return None

    async def _handle_response(
        self,
        response: aiohttp.ClientResponse,
        endpoint: str,
        method: str
    ) -> Optional[Dict[str, Any]]:
        """
        Processa resposta HTTP.

        Args:
            response: Resposta aiohttp
            endpoint: Endpoint chamado
            method: Metodo HTTP usado

        Returns:
            Dict com resposta ou None se erro
        """
        if response.status in (200, 201, 202):
            content_type = response.headers.get("Content-Type", "")
            if "application/json" in content_type:
                return await response.json()
            else:
                # Retorna dict com status para tipos nao-JSON
                return {"status": "success", "data": await response.text()}

        elif response.status == 204:
            return {"status": "success", "message": "No content"}

        elif response.status == 401:
            self._token = None
            self._last_error = "Nao autorizado"

        elif response.status == 403:
            self._last_error = "Acesso negado"

        elif response.status == 404:
            self._last_error = f"Recurso nao encontrado: {endpoint}"

        elif response.status == 409:
            self._last_error = "Conflito - recurso ja existe ou esta em uso"

        else:
            error_text = await response.text()
            self._last_error = f"Erro {response.status}: {error_text}"

        logger.error(f"{method} {endpoint}: {self._last_error}")
        return None

    def get_status(self) -> Dict[str, Any]:
        """
        Retorna status da conexao.

        Returns:
            Dict com informacoes de status
        """
        return {
            "status": self.status.value,
            "connected": self.is_connected,
            "tenant_url": self.config.tenant_url,
            "last_error": self._last_error,
            "token_valid": self._token is not None and not self._token.is_expired,
            "token_expires_at": self._token.expires_at.isoformat() if self._token else None
        }
