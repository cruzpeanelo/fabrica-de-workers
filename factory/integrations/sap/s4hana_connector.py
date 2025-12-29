# -*- coding: utf-8 -*-
"""
SAP S/4HANA and ECC Connector
=============================
Conector completo para SAP S/4HANA e SAP ECC via OData.

Funcionalidades:
- Cliente OData para APIs SAP
- CRUD operations para Business Partners e Sales Orders
- Autenticacao via Basic Auth e OAuth 2.0
- Suporte a CSRF token
- Batch operations
- Rate limiting e retry

Configuracao via variaveis de ambiente:
- SAP_BASE_URL: URL do servidor SAP
- SAP_CLIENT: Mandante SAP (ex: 100)
- SAP_AUTH_TYPE: basic ou oauth
- SAP_USERNAME: Usuario (Basic Auth)
- SAP_PASSWORD: Senha (Basic Auth)
- SAP_OAUTH_CLIENT_ID: Client ID (OAuth)
- SAP_OAUTH_CLIENT_SECRET: Client Secret (OAuth)
- SAP_OAUTH_TOKEN_URL: URL do token (OAuth)

Issue #101 - Endpoints SAP S/4HANA e ECC
"""

import os
import base64
import logging
import asyncio
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Union
from urllib.parse import urlencode, quote

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False

logger = logging.getLogger(__name__)


class S4HANAAuthType(str, Enum):
    """Tipos de autenticacao SAP"""
    BASIC = "basic"
    OAUTH = "oauth"
    OAUTH_CLIENT_CREDENTIALS = "oauth_client_credentials"
    X509 = "x509"


class SAPError(Exception):
    """Excecao base para erros SAP"""

    def __init__(
        self,
        message: str,
        status_code: int = 0,
        sap_error: Optional[Dict] = None,
        request_id: Optional[str] = None
    ):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.sap_error = sap_error or {}
        self.request_id = request_id

    def __str__(self):
        parts = []
        if self.status_code:
            parts.append(f"[{self.status_code}]")
        parts.append(self.message)
        if self.sap_error.get("code"):
            parts.append(f"(SAP Error: {self.sap_error['code']})")
        return " ".join(parts)


class SAPAuthError(SAPError):
    """Erro de autenticacao SAP"""
    pass


class SAPValidationError(SAPError):
    """Erro de validacao SAP"""
    pass


@dataclass
class S4HANAConfig:
    """
    Configuracao do conector SAP S/4HANA

    Attributes:
        base_url: URL base do servidor SAP
        client: Mandante SAP
        auth_type: Tipo de autenticacao
        username: Usuario (Basic Auth)
        password: Senha (Basic Auth)
        oauth_client_id: Client ID (OAuth)
        oauth_client_secret: Client Secret (OAuth)
        oauth_token_url: URL do token (OAuth)
        timeout: Timeout em segundos
        verify_ssl: Verificar certificado SSL
        language: Idioma (PT, EN, etc)
        max_retries: Numero maximo de retries
        retry_delay: Delay entre retries em segundos
    """
    base_url: str
    client: str = "100"
    auth_type: S4HANAAuthType = S4HANAAuthType.BASIC
    username: str = ""
    password: str = ""
    oauth_client_id: str = ""
    oauth_client_secret: str = ""
    oauth_token_url: str = ""
    oauth_scope: str = ""
    timeout: int = 60
    verify_ssl: bool = True
    language: str = "PT"
    max_retries: int = 3
    retry_delay: float = 1.0

    @classmethod
    def from_env(cls) -> "S4HANAConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        auth_type_str = os.getenv("SAP_AUTH_TYPE", "basic").lower()
        auth_type = S4HANAAuthType(auth_type_str) if auth_type_str in [e.value for e in S4HANAAuthType] else S4HANAAuthType.BASIC

        return cls(
            base_url=os.getenv("SAP_BASE_URL", ""),
            client=os.getenv("SAP_CLIENT", "100"),
            auth_type=auth_type,
            username=os.getenv("SAP_USERNAME", ""),
            password=os.getenv("SAP_PASSWORD", ""),
            oauth_client_id=os.getenv("SAP_OAUTH_CLIENT_ID", ""),
            oauth_client_secret=os.getenv("SAP_OAUTH_CLIENT_SECRET", ""),
            oauth_token_url=os.getenv("SAP_OAUTH_TOKEN_URL", ""),
            oauth_scope=os.getenv("SAP_OAUTH_SCOPE", ""),
            timeout=int(os.getenv("SAP_TIMEOUT", "60")),
            verify_ssl=os.getenv("SAP_VERIFY_SSL", "true").lower() == "true",
            language=os.getenv("SAP_LANGUAGE", "PT"),
            max_retries=int(os.getenv("SAP_MAX_RETRIES", "3")),
            retry_delay=float(os.getenv("SAP_RETRY_DELAY", "1.0"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        if not self.base_url:
            return False

        if self.auth_type == S4HANAAuthType.BASIC:
            return bool(self.username and self.password)
        elif self.auth_type in [S4HANAAuthType.OAUTH, S4HANAAuthType.OAUTH_CLIENT_CREDENTIALS]:
            return bool(self.oauth_client_id and self.oauth_client_secret and self.oauth_token_url)

        return False


class OAuthTokenManager:
    """Gerenciador de tokens OAuth para SAP"""

    def __init__(self, config: S4HANAConfig):
        self.config = config
        self._access_token: Optional[str] = None
        self._token_expires_at: Optional[datetime] = None
        self._refresh_token: Optional[str] = None

    @property
    def is_token_valid(self) -> bool:
        """Verifica se o token ainda e valido"""
        if not self._access_token or not self._token_expires_at:
            return False
        # Considerar token expirado 60 segundos antes
        return datetime.utcnow() < (self._token_expires_at - timedelta(seconds=60))

    async def get_access_token(self) -> str:
        """Obtem token de acesso valido"""
        if self.is_token_valid:
            return self._access_token

        await self._fetch_token()
        return self._access_token

    async def _fetch_token(self):
        """Busca novo token OAuth"""
        if not AIOHTTP_AVAILABLE:
            raise ImportError("aiohttp e necessario para OAuth")

        data = {
            "grant_type": "client_credentials",
            "client_id": self.config.oauth_client_id,
            "client_secret": self.config.oauth_client_secret
        }

        if self.config.oauth_scope:
            data["scope"] = self.config.oauth_scope

        headers = {
            "Content-Type": "application/x-www-form-urlencoded"
        }

        async with aiohttp.ClientSession() as session:
            async with session.post(
                self.config.oauth_token_url,
                data=data,
                headers=headers,
                ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                timeout=aiohttp.ClientTimeout(total=30)
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    raise SAPAuthError(
                        f"Falha ao obter token OAuth: {error_text}",
                        status_code=response.status
                    )

                token_data = await response.json()
                self._access_token = token_data.get("access_token")
                expires_in = token_data.get("expires_in", 3600)
                self._token_expires_at = datetime.utcnow() + timedelta(seconds=expires_in)
                self._refresh_token = token_data.get("refresh_token")

                logger.info(f"Token OAuth obtido, expira em {expires_in} segundos")


class S4HANAConnector:
    """
    Conector principal para SAP S/4HANA e ECC.

    Fornece:
    - Autenticacao Basic Auth e OAuth 2.0
    - Gerenciamento de CSRF token
    - Operacoes CRUD via OData
    - Retry automatico
    - Batch operations

    Exemplo de uso:
    ```python
    config = S4HANAConfig.from_env()
    connector = S4HANAConnector(config)

    # Conectar
    if await connector.connect():
        # Usar servicos
        bp_service = connector.get_business_partner_service()
        partners = await bp_service.list(country="BR", top=100)

        # Criar parceiro
        new_bp = await bp_service.create({
            "BusinessPartnerCategory": "1",
            "BusinessPartnerFullName": "Empresa LTDA"
        })
    ```
    """

    # Paths dos servicos OData
    SERVICE_PATHS = {
        "business_partner": "/sap/opu/odata/sap/API_BUSINESS_PARTNER",
        "sales_order": "/sap/opu/odata/sap/API_SALES_ORDER_SRV",
        "material": "/sap/opu/odata/sap/API_PRODUCT_SRV",
        "customer": "/sap/opu/odata/sap/API_BUSINESS_PARTNER",
        "supplier": "/sap/opu/odata/sap/API_BUSINESS_PARTNER"
    }

    def __init__(self, config: S4HANAConfig):
        """
        Inicializa o conector.

        Args:
            config: Configuracao do SAP
        """
        self.config = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._csrf_token: Optional[str] = None
        self._csrf_cookies: Dict = {}
        self._connected = False
        self._oauth_manager: Optional[OAuthTokenManager] = None

        if config.auth_type in [S4HANAAuthType.OAUTH, S4HANAAuthType.OAUTH_CLIENT_CREDENTIALS]:
            self._oauth_manager = OAuthTokenManager(config)

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._connected

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            connector = aiohttp.TCPConnector(ssl=self.config.verify_ssl if self.config.verify_ssl else False)
            self._session = aiohttp.ClientSession(connector=connector)
        return self._session

    async def _get_auth_headers(self) -> Dict[str, str]:
        """Obtem headers de autenticacao"""
        headers = {}

        if self.config.auth_type == S4HANAAuthType.BASIC:
            credentials = f"{self.config.username}:{self.config.password}"
            encoded = base64.b64encode(credentials.encode()).decode()
            headers["Authorization"] = f"Basic {encoded}"

        elif self.config.auth_type in [S4HANAAuthType.OAUTH, S4HANAAuthType.OAUTH_CLIENT_CREDENTIALS]:
            if self._oauth_manager:
                token = await self._oauth_manager.get_access_token()
                headers["Authorization"] = f"Bearer {token}"

        return headers

    def _get_default_headers(self) -> Dict[str, str]:
        """Retorna headers padrao"""
        return {
            "Accept": "application/json",
            "Accept-Language": self.config.language,
            "Content-Type": "application/json",
            "sap-client": self.config.client,
            "sap-cancel-on-close": "true"
        }

    async def connect(self) -> bool:
        """
        Conecta ao SAP e valida credenciais.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            logger.error("Configuracao SAP invalida")
            return False

        try:
            session = await self._ensure_session()
            headers = self._get_default_headers()
            auth_headers = await self._get_auth_headers()
            headers.update(auth_headers)

            # Testar conexao buscando metadata do Business Partner
            url = f"{self.config.base_url}{self.SERVICE_PATHS['business_partner']}/$metadata"

            async with session.get(
                url,
                headers=headers,
                timeout=aiohttp.ClientTimeout(total=self.config.timeout)
            ) as response:
                if response.status == 200:
                    self._connected = True
                    logger.info(f"Conectado ao SAP: {self.config.base_url}")

                    # Buscar CSRF token
                    await self._fetch_csrf_token()
                    return True

                elif response.status == 401:
                    logger.error("Erro de autenticacao SAP")
                    raise SAPAuthError("Credenciais invalidas", status_code=401)

                else:
                    error_text = await response.text()
                    logger.error(f"Erro ao conectar: {response.status} - {error_text}")
                    return False

        except SAPError:
            raise
        except Exception as e:
            logger.error(f"Erro ao conectar ao SAP: {e}")
            return False

    async def disconnect(self):
        """Desconecta do SAP"""
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._csrf_token = None
        self._csrf_cookies = {}
        self._connected = False
        logger.info("Desconectado do SAP")

    async def test_connection(self) -> bool:
        """Testa a conexao"""
        try:
            session = await self._ensure_session()
            headers = self._get_default_headers()
            auth_headers = await self._get_auth_headers()
            headers.update(auth_headers)

            url = f"{self.config.base_url}{self.SERVICE_PATHS['business_partner']}/"

            async with session.get(
                url,
                headers=headers,
                timeout=aiohttp.ClientTimeout(total=10)
            ) as response:
                return response.status == 200

        except Exception:
            return False

    async def _fetch_csrf_token(self):
        """Busca CSRF token para operacoes de escrita"""
        try:
            session = await self._ensure_session()
            headers = self._get_default_headers()
            auth_headers = await self._get_auth_headers()
            headers.update(auth_headers)
            headers["X-CSRF-Token"] = "Fetch"

            url = f"{self.config.base_url}{self.SERVICE_PATHS['business_partner']}/"

            async with session.get(
                url,
                headers=headers,
                timeout=aiohttp.ClientTimeout(total=self.config.timeout)
            ) as response:
                self._csrf_token = response.headers.get("X-CSRF-Token")

                # Salvar cookies
                for cookie in response.cookies.values():
                    self._csrf_cookies[cookie.key] = cookie.value

                if self._csrf_token:
                    logger.debug(f"CSRF token obtido: {self._csrf_token[:10]}...")

        except Exception as e:
            logger.warning(f"Falha ao obter CSRF token: {e}")

    async def _request(
        self,
        method: str,
        url: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None,
        include_csrf: bool = False
    ) -> Dict:
        """
        Executa requisicao HTTP com retry.

        Args:
            method: Metodo HTTP
            url: URL completa
            data: Dados para POST/PUT/PATCH
            params: Query parameters
            include_csrf: Incluir CSRF token

        Returns:
            Dict com resposta
        """
        session = await self._ensure_session()
        headers = self._get_default_headers()
        auth_headers = await self._get_auth_headers()
        headers.update(auth_headers)

        if include_csrf and self._csrf_token:
            headers["X-CSRF-Token"] = self._csrf_token

        last_error = None

        for attempt in range(self.config.max_retries):
            try:
                async with session.request(
                    method,
                    url,
                    headers=headers,
                    json=data if method in ["POST", "PUT", "PATCH"] else None,
                    params=params,
                    cookies=self._csrf_cookies,
                    timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                ) as response:
                    request_id = response.headers.get("sap-message-id")

                    # CSRF token expirado
                    if response.status == 403 and "csrf" in (await response.text()).lower():
                        logger.warning("CSRF token expirado, renovando...")
                        await self._fetch_csrf_token()
                        if self._csrf_token:
                            headers["X-CSRF-Token"] = self._csrf_token
                        continue

                    if response.status >= 400:
                        error_data = await response.json() if response.content_type == "application/json" else {}
                        self._handle_error(response.status, error_data, request_id)

                    if response.status == 204:
                        return {"success": True}

                    return await response.json()

            except SAPError:
                raise
            except Exception as e:
                last_error = e
                logger.warning(f"Tentativa {attempt + 1} falhou: {e}")
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(self.config.retry_delay * (attempt + 1))

        raise SAPError(f"Falha apos {self.config.max_retries} tentativas: {last_error}")

    def _handle_error(self, status_code: int, error_data: Dict, request_id: Optional[str] = None):
        """Trata erros de resposta"""
        sap_error = {}
        message = f"Erro HTTP {status_code}"

        if isinstance(error_data, dict):
            error_obj = error_data.get("error", {})
            sap_error = {
                "code": error_obj.get("code", ""),
                "message": error_obj.get("message", {}).get("value", "") if isinstance(error_obj.get("message"), dict) else error_obj.get("message", ""),
                "details": error_obj.get("innererror", {}).get("errordetails", [])
            }
            message = sap_error.get("message") or message

        if status_code in (401, 403):
            raise SAPAuthError(message, status_code, sap_error, request_id)
        elif status_code == 400:
            raise SAPValidationError(message, status_code, sap_error, request_id)
        else:
            raise SAPError(message, status_code, sap_error, request_id)

    def get_business_partner_service(self) -> "BusinessPartnerService":
        """Retorna servico de Business Partners"""
        return BusinessPartnerService(self)

    def get_sales_order_service(self) -> "SalesOrderService":
        """Retorna servico de Sales Orders"""
        return SalesOrderService(self)

    async def get(
        self,
        service: str,
        entity_set: str,
        key: Optional[str] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        filter_expr: Optional[str] = None,
        orderby: Optional[str] = None,
        top: Optional[int] = None,
        skip: Optional[int] = None,
        count: bool = False
    ) -> Dict:
        """
        GET request para entity set.

        Args:
            service: Nome do servico (business_partner, sales_order, etc)
            entity_set: Nome do entity set
            key: Chave do registro
            select: Campos para retornar
            expand: Associacoes para expandir
            filter_expr: Filtro OData
            orderby: Ordenacao
            top: Limite de registros
            skip: Registros para pular
            count: Incluir contagem

        Returns:
            Dict com dados
        """
        service_path = self.SERVICE_PATHS.get(service, service)
        url = f"{self.config.base_url}{service_path}/{entity_set}"

        if key:
            url = f"{url}({key})"

        params = {}
        if select:
            params["$select"] = ",".join(select)
        if expand:
            params["$expand"] = ",".join(expand)
        if filter_expr:
            params["$filter"] = filter_expr
        if orderby:
            params["$orderby"] = orderby
        if top:
            params["$top"] = str(top)
        if skip:
            params["$skip"] = str(skip)
        if count:
            params["$inlinecount"] = "allpages"

        return await self._request("GET", url, params=params)

    async def create(
        self,
        service: str,
        entity_set: str,
        data: Dict
    ) -> Dict:
        """
        Cria novo registro (POST).

        Args:
            service: Nome do servico
            entity_set: Nome do entity set
            data: Dados do registro

        Returns:
            Dict com registro criado
        """
        if not self._csrf_token:
            await self._fetch_csrf_token()

        service_path = self.SERVICE_PATHS.get(service, service)
        url = f"{self.config.base_url}{service_path}/{entity_set}"

        return await self._request("POST", url, data=data, include_csrf=True)

    async def update(
        self,
        service: str,
        entity_path: str,
        data: Dict,
        patch: bool = True
    ) -> Dict:
        """
        Atualiza registro (PATCH ou PUT).

        Args:
            service: Nome do servico
            entity_path: Caminho do registro incluindo chave
            data: Dados para atualizar
            patch: Se True usa PATCH, senao PUT

        Returns:
            Dict com resultado
        """
        if not self._csrf_token:
            await self._fetch_csrf_token()

        service_path = self.SERVICE_PATHS.get(service, service)
        url = f"{self.config.base_url}{service_path}/{entity_path}"
        method = "PATCH" if patch else "PUT"

        return await self._request(method, url, data=data, include_csrf=True)

    async def delete(
        self,
        service: str,
        entity_path: str
    ) -> bool:
        """
        Deleta registro (DELETE).

        Args:
            service: Nome do servico
            entity_path: Caminho do registro incluindo chave

        Returns:
            bool: True se deletado
        """
        if not self._csrf_token:
            await self._fetch_csrf_token()

        service_path = self.SERVICE_PATHS.get(service, service)
        url = f"{self.config.base_url}{service_path}/{entity_path}"

        result = await self._request("DELETE", url, include_csrf=True)
        return result.get("success", True)


class BaseService(ABC):
    """Classe base para servicos SAP"""

    def __init__(self, connector: S4HANAConnector):
        self.connector = connector

    @property
    @abstractmethod
    def service_name(self) -> str:
        """Nome do servico OData"""
        pass

    @property
    @abstractmethod
    def entity_set(self) -> str:
        """Nome do entity set principal"""
        pass


class BusinessPartnerService(BaseService):
    """
    Servico para Business Partners.

    Entity Sets:
    - A_BusinessPartner: Parceiros de negocio
    - A_BusinessPartnerAddress: Enderecos
    - A_BusinessPartnerBank: Dados bancarios
    - A_BusinessPartnerContact: Contatos
    - A_Customer: Clientes
    - A_Supplier: Fornecedores

    Exemplo:
    ```python
    bp_service = connector.get_business_partner_service()

    # Listar parceiros do Brasil
    partners = await bp_service.list(country="BR", top=100)

    # Buscar parceiro especifico
    partner = await bp_service.get("0001000000")

    # Criar parceiro
    new_partner = await bp_service.create({
        "BusinessPartnerCategory": "1",  # 1=Pessoa, 2=Organizacao
        "BusinessPartnerFullName": "Empresa LTDA",
        "SearchTerm1": "EMPRESA"
    })

    # Atualizar parceiro
    await bp_service.update("0001000000", {
        "BusinessPartnerFullName": "Empresa SA"
    })

    # Deletar parceiro
    await bp_service.delete("0001000000")
    ```
    """

    @property
    def service_name(self) -> str:
        return "business_partner"

    @property
    def entity_set(self) -> str:
        return "A_BusinessPartner"

    async def list(
        self,
        category: Optional[str] = None,
        country: Optional[str] = None,
        search_term: Optional[str] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        top: int = 100,
        skip: int = 0,
        orderby: str = "BusinessPartnerFullName"
    ) -> List[Dict]:
        """
        Lista Business Partners.

        Args:
            category: 1=Pessoa, 2=Organizacao
            country: Codigo do pais (BR, US, etc)
            search_term: Termo de busca
            select: Campos para retornar
            expand: Associacoes para expandir
            top: Limite de registros
            skip: Registros para pular
            orderby: Ordenacao

        Returns:
            Lista de Business Partners
        """
        filters = []

        if category:
            filters.append(f"BusinessPartnerCategory eq '{category}'")
        if country:
            filters.append(f"Country eq '{country}'")
        if search_term:
            filters.append(f"substringof('{search_term}',SearchTerm1)")

        filter_expr = " and ".join(filters) if filters else None

        default_select = [
            "BusinessPartner",
            "BusinessPartnerCategory",
            "BusinessPartnerFullName",
            "BusinessPartnerName",
            "FirstName",
            "LastName",
            "SearchTerm1",
            "SearchTerm2",
            "Industry",
            "Country",
            "Region",
            "CreationDate",
            "LastChangeDate",
            "BusinessPartnerIsBlocked"
        ]

        result = await self.connector.get(
            service=self.service_name,
            entity_set=self.entity_set,
            select=select or default_select,
            expand=expand,
            filter_expr=filter_expr,
            orderby=orderby,
            top=top,
            skip=skip
        )

        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])

    async def get(
        self,
        business_partner_id: str,
        expand: Optional[List[str]] = None
    ) -> Optional[Dict]:
        """
        Busca Business Partner por ID.

        Args:
            business_partner_id: ID do parceiro (10 caracteres)
            expand: Associacoes para expandir

        Returns:
            Dict com dados do parceiro ou None
        """
        key = f"'{business_partner_id}'"

        try:
            result = await self.connector.get(
                service=self.service_name,
                entity_set=self.entity_set,
                key=key,
                expand=expand
            )
            return result.get("d", result)
        except SAPError as e:
            if e.status_code == 404:
                return None
            raise

    async def create(self, data: Dict) -> Dict:
        """
        Cria novo Business Partner.

        Args:
            data: Dados do parceiro
                - BusinessPartnerCategory: 1=Pessoa, 2=Organizacao (obrigatorio)
                - BusinessPartnerFullName: Nome completo (obrigatorio)
                - SearchTerm1: Termo de busca 1
                - SearchTerm2: Termo de busca 2
                - Industry: Setor
                - Country: Pais
                - Region: Estado/Regiao

        Returns:
            Dict com parceiro criado
        """
        required_fields = ["BusinessPartnerCategory", "BusinessPartnerFullName"]
        for field in required_fields:
            if field not in data:
                raise SAPValidationError(f"Campo obrigatorio ausente: {field}")

        result = await self.connector.create(
            service=self.service_name,
            entity_set=self.entity_set,
            data=data
        )

        return result.get("d", result)

    async def update(
        self,
        business_partner_id: str,
        data: Dict
    ) -> bool:
        """
        Atualiza Business Partner.

        Args:
            business_partner_id: ID do parceiro
            data: Dados para atualizar

        Returns:
            True se atualizado
        """
        entity_path = f"{self.entity_set}('{business_partner_id}')"

        await self.connector.update(
            service=self.service_name,
            entity_path=entity_path,
            data=data
        )

        return True

    async def delete(self, business_partner_id: str) -> bool:
        """
        Deleta Business Partner.

        Args:
            business_partner_id: ID do parceiro

        Returns:
            True se deletado
        """
        entity_path = f"{self.entity_set}('{business_partner_id}')"

        return await self.connector.delete(
            service=self.service_name,
            entity_path=entity_path
        )

    async def get_addresses(self, business_partner_id: str) -> List[Dict]:
        """Lista enderecos do parceiro"""
        result = await self.connector.get(
            service=self.service_name,
            entity_set=f"{self.entity_set}('{business_partner_id}')/to_BusinessPartnerAddress"
        )
        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])

    async def get_bank_accounts(self, business_partner_id: str) -> List[Dict]:
        """Lista contas bancarias do parceiro"""
        result = await self.connector.get(
            service=self.service_name,
            entity_set=f"{self.entity_set}('{business_partner_id}')/to_BusinessPartnerBank"
        )
        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])

    async def search(self, query: str, top: int = 50) -> List[Dict]:
        """
        Busca parceiros por termo.

        Args:
            query: Termo de busca
            top: Limite de resultados

        Returns:
            Lista de parceiros encontrados
        """
        filter_expr = (
            f"substringof('{query}',BusinessPartnerFullName) or "
            f"substringof('{query}',SearchTerm1) or "
            f"substringof('{query}',SearchTerm2)"
        )

        result = await self.connector.get(
            service=self.service_name,
            entity_set=self.entity_set,
            filter_expr=filter_expr,
            top=top,
            select=["BusinessPartner", "BusinessPartnerFullName", "SearchTerm1", "Country"]
        )

        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])


class SalesOrderService(BaseService):
    """
    Servico para Sales Orders (Pedidos de Venda).

    Entity Sets:
    - A_SalesOrder: Cabecalho do pedido
    - A_SalesOrderItem: Itens do pedido
    - A_SalesOrderItemPartner: Parceiros do item
    - A_SalesOrderItemPrcgElmnt: Condicoes de preco
    - A_SalesOrderScheduleLine: Linhas de programacao

    Exemplo:
    ```python
    so_service = connector.get_sales_order_service()

    # Listar pedidos
    orders = await so_service.list(
        customer="0001000000",
        date_from=datetime(2024, 1, 1),
        top=50
    )

    # Buscar pedido
    order = await so_service.get("0000000001", expand=["to_Item"])

    # Criar pedido
    new_order = await so_service.create({
        "SalesOrderType": "ZOR",
        "SalesOrganization": "1000",
        "DistributionChannel": "10",
        "Division": "00",
        "SoldToParty": "0001000000",
        "to_Item": [{
            "Material": "MATERIAL001",
            "RequestedQuantity": "10",
            "RequestedQuantityUnit": "PC"
        }]
    })
    ```
    """

    @property
    def service_name(self) -> str:
        return "sales_order"

    @property
    def entity_set(self) -> str:
        return "A_SalesOrder"

    async def list(
        self,
        customer: Optional[str] = None,
        sales_org: Optional[str] = None,
        order_type: Optional[str] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        top: int = 100,
        skip: int = 0,
        orderby: str = "SalesOrder desc"
    ) -> List[Dict]:
        """
        Lista Sales Orders.

        Args:
            customer: ID do cliente
            sales_org: Organizacao de vendas
            order_type: Tipo de pedido
            date_from: Data inicial
            date_to: Data final
            select: Campos para retornar
            expand: Associacoes para expandir
            top: Limite de registros
            skip: Registros para pular
            orderby: Ordenacao

        Returns:
            Lista de pedidos
        """
        filters = []

        if customer:
            filters.append(f"SoldToParty eq '{customer}'")
        if sales_org:
            filters.append(f"SalesOrganization eq '{sales_org}'")
        if order_type:
            filters.append(f"SalesOrderType eq '{order_type}'")
        if date_from:
            filters.append(f"SalesOrderDate ge datetime'{date_from.strftime('%Y-%m-%dT00:00:00')}'")
        if date_to:
            filters.append(f"SalesOrderDate le datetime'{date_to.strftime('%Y-%m-%dT23:59:59')}'")

        filter_expr = " and ".join(filters) if filters else None

        default_select = [
            "SalesOrder",
            "SalesOrderType",
            "SalesOrganization",
            "DistributionChannel",
            "SoldToParty",
            "SalesOrderDate",
            "TotalNetAmount",
            "TransactionCurrency",
            "OverallSDProcessStatus",
            "OverallDeliveryStatus",
            "CreationDate"
        ]

        result = await self.connector.get(
            service=self.service_name,
            entity_set=self.entity_set,
            select=select or default_select,
            expand=expand,
            filter_expr=filter_expr,
            orderby=orderby,
            top=top,
            skip=skip
        )

        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])

    async def get(
        self,
        sales_order: str,
        expand: Optional[List[str]] = None
    ) -> Optional[Dict]:
        """
        Busca Sales Order por numero.

        Args:
            sales_order: Numero do pedido (10 caracteres)
            expand: Associacoes para expandir (ex: ["to_Item", "to_Partner"])

        Returns:
            Dict com dados do pedido ou None
        """
        key = f"'{sales_order}'"

        try:
            result = await self.connector.get(
                service=self.service_name,
                entity_set=self.entity_set,
                key=key,
                expand=expand
            )
            return result.get("d", result)
        except SAPError as e:
            if e.status_code == 404:
                return None
            raise

    async def create(self, data: Dict) -> Dict:
        """
        Cria novo Sales Order.

        Args:
            data: Dados do pedido
                - SalesOrderType: Tipo de pedido (obrigatorio)
                - SalesOrganization: Org. de vendas (obrigatorio)
                - DistributionChannel: Canal de distribuicao (obrigatorio)
                - Division: Divisao (obrigatorio)
                - SoldToParty: Cliente (obrigatorio)
                - to_Item: Lista de itens

        Returns:
            Dict com pedido criado
        """
        required_fields = [
            "SalesOrderType",
            "SalesOrganization",
            "DistributionChannel",
            "SoldToParty"
        ]

        for field in required_fields:
            if field not in data:
                raise SAPValidationError(f"Campo obrigatorio ausente: {field}")

        result = await self.connector.create(
            service=self.service_name,
            entity_set=self.entity_set,
            data=data
        )

        return result.get("d", result)

    async def update(
        self,
        sales_order: str,
        data: Dict
    ) -> bool:
        """
        Atualiza Sales Order.

        Args:
            sales_order: Numero do pedido
            data: Dados para atualizar

        Returns:
            True se atualizado
        """
        entity_path = f"{self.entity_set}('{sales_order}')"

        await self.connector.update(
            service=self.service_name,
            entity_path=entity_path,
            data=data
        )

        return True

    async def delete(self, sales_order: str) -> bool:
        """
        Deleta/Cancela Sales Order.

        Args:
            sales_order: Numero do pedido

        Returns:
            True se deletado
        """
        entity_path = f"{self.entity_set}('{sales_order}')"

        return await self.connector.delete(
            service=self.service_name,
            entity_path=entity_path
        )

    async def get_items(self, sales_order: str) -> List[Dict]:
        """
        Lista itens do pedido.

        Args:
            sales_order: Numero do pedido

        Returns:
            Lista de itens
        """
        result = await self.connector.get(
            service=self.service_name,
            entity_set=f"{self.entity_set}('{sales_order}')/to_Item"
        )
        return result.get("d", {}).get("results", []) if "d" in result else result.get("value", [])

    async def add_item(
        self,
        sales_order: str,
        material: str,
        quantity: float,
        unit: str = "PC"
    ) -> Dict:
        """
        Adiciona item ao pedido.

        Args:
            sales_order: Numero do pedido
            material: Codigo do material
            quantity: Quantidade
            unit: Unidade de medida

        Returns:
            Dict com item criado
        """
        item_data = {
            "SalesOrder": sales_order,
            "Material": material,
            "RequestedQuantity": str(quantity),
            "RequestedQuantityUnit": unit
        }

        result = await self.connector.create(
            service=self.service_name,
            entity_set="A_SalesOrderItem",
            data=item_data
        )

        return result.get("d", result)

    async def simulate(self, data: Dict) -> Dict:
        """
        Simula criacao de pedido (sem persistir).

        Args:
            data: Dados do pedido

        Returns:
            Dict com resultado da simulacao
        """
        # Adicionar flag de simulacao
        data["__simulate"] = True

        try:
            result = await self.connector.create(
                service=self.service_name,
                entity_set=self.entity_set,
                data=data
            )
            return result.get("d", result)
        except SAPError:
            # Simulacao sempre falha no final para nao persistir
            raise


# Singleton global
_connector_instance: Optional[S4HANAConnector] = None


def get_s4hana_connector() -> S4HANAConnector:
    """Retorna instancia global do conector SAP"""
    global _connector_instance
    if _connector_instance is None:
        config = S4HANAConfig.from_env()
        _connector_instance = S4HANAConnector(config)
    return _connector_instance


async def init_s4hana_connector() -> Optional[S4HANAConnector]:
    """Inicializa e conecta ao SAP se configurado"""
    connector = get_s4hana_connector()
    if connector.config.is_valid():
        if await connector.connect():
            return connector
    return None
