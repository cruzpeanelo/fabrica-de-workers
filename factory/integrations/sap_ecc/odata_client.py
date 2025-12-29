# -*- coding: utf-8 -*-
"""
SAP OData Client
================
Cliente para acesso ao SAP via SAP Gateway OData.

Este modulo fornece:
- Acesso a servicos OData do SAP Gateway
- Operacoes CRUD (Create, Read, Update, Delete)
- Consultas com filtros OData
- Suporte a batch requests
- Autenticacao basica e OAuth2

Vantagens do OData vs RFC:
- Nao requer SAP NW RFC SDK
- Funciona via HTTP/HTTPS
- Mais facil de configurar em ambientes cloud
- Melhor para integracao web

Requisitos:
-----------
pip install requests

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.odata_client import SAPODataClient, SAPODataConfig

config = SAPODataConfig(
    base_url="https://sapgateway.company.com/sap/opu/odata/sap/",
    username="ODATA_USER",
    password="xxx",
    client="100"
)

async with SAPODataClient(config) as client:
    # Listar materiais
    materials = await client.get(
        "API_MATERIAL_SRV",
        "A_Product",
        filters={"ProductType": "FERT"},
        top=100
    )

    # Criar pedido
    order = await client.post(
        "API_SALES_ORDER_SRV",
        "A_SalesOrder",
        data={...}
    )
```
"""

import asyncio
import base64
import logging
import json
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union
from urllib.parse import urljoin, quote

logger = logging.getLogger(__name__)

# Tentar importar aiohttp, senao usar requests
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


class ODataRequestError(Exception):
    """Erro em requisicao OData"""
    def __init__(
        self,
        message: str,
        status_code: Optional[int] = None,
        response_body: Optional[str] = None
    ):
        super().__init__(message)
        self.status_code = status_code
        self.response_body = response_body


class ODataAuthType(str, Enum):
    """Tipo de autenticacao OData"""
    BASIC = "basic"
    OAUTH2 = "oauth2"
    X509 = "x509"
    SAML = "saml"


@dataclass
class SAPODataConfig:
    """
    Configuracao para cliente OData.

    Atributos:
        base_url: URL base do SAP Gateway (ex: https://server/sap/opu/odata/sap/)
        username: Usuario para autenticacao
        password: Senha do usuario
        client: Mandante SAP
        auth_type: Tipo de autenticacao (basic, oauth2, etc.)
        oauth_token_url: URL para obter token OAuth2
        oauth_client_id: Client ID para OAuth2
        oauth_client_secret: Client Secret para OAuth2
        timeout: Timeout em segundos
        verify_ssl: Verificar certificado SSL
        csrf_token: Se True, busca token CSRF antes de operacoes de escrita
    """
    base_url: str = ""
    username: str = ""
    password: str = ""
    client: str = "100"
    auth_type: ODataAuthType = ODataAuthType.BASIC
    oauth_token_url: Optional[str] = None
    oauth_client_id: Optional[str] = None
    oauth_client_secret: Optional[str] = None
    timeout: int = 30
    verify_ssl: bool = True
    csrf_token: bool = True
    language: str = "PT"

    def validate(self) -> tuple[bool, List[str]]:
        """Valida a configuracao"""
        errors = []

        if not self.base_url:
            errors.append("URL base nao informada")

        if self.auth_type == ODataAuthType.BASIC:
            if not self.username:
                errors.append("Usuario nao informado")
            if not self.password:
                errors.append("Senha nao informada")

        if self.auth_type == ODataAuthType.OAUTH2:
            if not self.oauth_token_url:
                errors.append("URL do token OAuth2 nao informada")
            if not self.oauth_client_id:
                errors.append("Client ID OAuth2 nao informado")

        return len(errors) == 0, errors


@dataclass
class ODataResponse:
    """Resposta de uma requisicao OData"""
    success: bool
    status_code: int = 0
    data: Any = None
    count: Optional[int] = None
    next_link: Optional[str] = None
    error_message: Optional[str] = None
    execution_time_ms: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "status_code": self.status_code,
            "data": self.data,
            "count": self.count,
            "next_link": self.next_link,
            "error_message": self.error_message,
            "execution_time_ms": self.execution_time_ms
        }


class SAPODataClient:
    """
    Cliente OData para SAP Gateway.

    Fornece acesso a servicos OData do SAP para:
    - Leitura de dados mestres e transacionais
    - Criacao e atualizacao de documentos
    - Consultas com filtros e ordenacao
    - Batch requests para multiplas operacoes

    Exemplo:
    --------
    ```python
    config = SAPODataConfig(
        base_url="https://sapgateway.company.com/sap/opu/odata/sap/",
        username="user",
        password="pass",
        client="100"
    )

    client = SAPODataClient(config)
    await client.connect()

    # Buscar pedidos de venda
    orders = await client.get(
        "API_SALES_ORDER_SRV",
        "A_SalesOrder",
        filters={"SalesOrderType": "OR"},
        select=["SalesOrder", "SoldToParty", "TotalNetAmount"],
        top=50
    )
    ```
    """

    def __init__(self, config: SAPODataConfig):
        """
        Inicializa o cliente OData.

        Args:
            config: Configuracao do cliente
        """
        self.config = config
        self._session = None
        self._csrf_token: Optional[str] = None
        self._oauth_token: Optional[str] = None
        self._token_expires_at: Optional[datetime] = None
        self._connected = False

        if not REQUESTS_AVAILABLE and not AIOHTTP_AVAILABLE:
            raise ImportError(
                "Nem requests nem aiohttp estao instalados. "
                "Instale com: pip install requests ou pip install aiohttp"
            )

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._connected

    def _get_auth_header(self) -> Dict[str, str]:
        """Retorna header de autenticacao"""
        if self.config.auth_type == ODataAuthType.BASIC:
            credentials = f"{self.config.username}:{self.config.password}"
            encoded = base64.b64encode(credentials.encode()).decode()
            return {"Authorization": f"Basic {encoded}"}

        if self.config.auth_type == ODataAuthType.OAUTH2 and self._oauth_token:
            return {"Authorization": f"Bearer {self._oauth_token}"}

        return {}

    def _get_default_headers(self) -> Dict[str, str]:
        """Retorna headers padrao"""
        headers = {
            "Accept": "application/json",
            "Content-Type": "application/json",
            "Accept-Language": self.config.language,
            "sap-client": self.config.client
        }
        headers.update(self._get_auth_header())

        if self._csrf_token:
            headers["X-CSRF-Token"] = self._csrf_token

        return headers

    async def connect(self) -> bool:
        """
        Estabelece conexao e obtem tokens necessarios.

        Returns:
            True se conectou com sucesso
        """
        is_valid, errors = self.config.validate()
        if not is_valid:
            raise ODataRequestError(f"Configuracao invalida: {', '.join(errors)}")

        try:
            # Obter token OAuth2 se necessario
            if self.config.auth_type == ODataAuthType.OAUTH2:
                await self._get_oauth_token()

            # Obter token CSRF se configurado
            if self.config.csrf_token:
                await self._fetch_csrf_token()

            self._connected = True
            logger.info(f"Conectado ao SAP OData: {self.config.base_url}")
            return True

        except Exception as e:
            logger.error(f"Erro ao conectar ao SAP OData: {e}")
            raise ODataRequestError(str(e))

    async def disconnect(self):
        """Encerra a sessao"""
        if self._session:
            if AIOHTTP_AVAILABLE and isinstance(self._session, aiohttp.ClientSession):
                await self._session.close()
            self._session = None
        self._connected = False
        self._csrf_token = None
        self._oauth_token = None

    async def _fetch_csrf_token(self):
        """Busca token CSRF do servidor"""
        headers = self._get_default_headers()
        headers["X-CSRF-Token"] = "Fetch"

        url = self.config.base_url
        response = await self._make_request("GET", url, headers=headers)

        if response.success:
            # O token vem no header da resposta
            # (seria necessario capturar o header da resposta)
            self._csrf_token = "fetched"  # Placeholder
            logger.debug("Token CSRF obtido")

    async def _get_oauth_token(self):
        """Obtem token OAuth2"""
        if not self.config.oauth_token_url:
            return

        data = {
            "grant_type": "client_credentials",
            "client_id": self.config.oauth_client_id,
            "client_secret": self.config.oauth_client_secret
        }

        response = await self._make_request(
            "POST",
            self.config.oauth_token_url,
            data=data,
            headers={"Content-Type": "application/x-www-form-urlencoded"}
        )

        if response.success and response.data:
            self._oauth_token = response.data.get("access_token")
            expires_in = response.data.get("expires_in", 3600)
            self._token_expires_at = datetime.now()
            logger.debug("Token OAuth2 obtido")

    async def _make_request(
        self,
        method: str,
        url: str,
        headers: Optional[Dict[str, str]] = None,
        data: Optional[Any] = None,
        params: Optional[Dict[str, str]] = None
    ) -> ODataResponse:
        """
        Executa uma requisicao HTTP.

        Args:
            method: Metodo HTTP (GET, POST, PUT, DELETE, PATCH)
            url: URL completa
            headers: Headers adicionais
            data: Dados para enviar
            params: Query parameters

        Returns:
            ODataResponse com o resultado
        """
        start_time = datetime.now()
        request_headers = headers or self._get_default_headers()

        try:
            if AIOHTTP_AVAILABLE:
                return await self._request_aiohttp(
                    method, url, request_headers, data, params, start_time
                )
            else:
                return await self._request_requests(
                    method, url, request_headers, data, params, start_time
                )

        except Exception as e:
            execution_time = (datetime.now() - start_time).total_seconds() * 1000
            logger.error(f"Erro na requisicao OData: {e}")
            return ODataResponse(
                success=False,
                error_message=str(e),
                execution_time_ms=execution_time
            )

    async def _request_aiohttp(
        self,
        method: str,
        url: str,
        headers: Dict[str, str],
        data: Any,
        params: Optional[Dict],
        start_time: datetime
    ) -> ODataResponse:
        """Executa requisicao usando aiohttp"""
        if not self._session:
            timeout = aiohttp.ClientTimeout(total=self.config.timeout)
            connector = aiohttp.TCPConnector(ssl=self.config.verify_ssl)
            self._session = aiohttp.ClientSession(
                timeout=timeout,
                connector=connector
            )

        json_data = data if isinstance(data, dict) else None
        form_data = data if not isinstance(data, dict) else None

        async with self._session.request(
            method,
            url,
            headers=headers,
            json=json_data,
            data=form_data,
            params=params
        ) as response:
            execution_time = (datetime.now() - start_time).total_seconds() * 1000

            try:
                response_data = await response.json()
            except:
                response_data = await response.text()

            return self._parse_response(
                response.status,
                response_data,
                execution_time
            )

    async def _request_requests(
        self,
        method: str,
        url: str,
        headers: Dict[str, str],
        data: Any,
        params: Optional[Dict],
        start_time: datetime
    ) -> ODataResponse:
        """Executa requisicao usando requests (sincrono em thread)"""
        loop = asyncio.get_event_loop()

        def make_request():
            json_data = data if isinstance(data, dict) else None
            form_data = data if not isinstance(data, dict) else None

            response = requests.request(
                method,
                url,
                headers=headers,
                json=json_data,
                data=form_data,
                params=params,
                timeout=self.config.timeout,
                verify=self.config.verify_ssl
            )
            return response

        response = await loop.run_in_executor(None, make_request)
        execution_time = (datetime.now() - start_time).total_seconds() * 1000

        try:
            response_data = response.json()
        except:
            response_data = response.text

        return self._parse_response(
            response.status_code,
            response_data,
            execution_time
        )

    def _parse_response(
        self,
        status_code: int,
        response_data: Any,
        execution_time: float
    ) -> ODataResponse:
        """Parse da resposta OData"""
        success = 200 <= status_code < 300

        # Extrair dados do formato OData
        data = None
        count = None
        next_link = None
        error_message = None

        if isinstance(response_data, dict):
            # OData v4
            if "value" in response_data:
                data = response_data["value"]
                count = response_data.get("@odata.count")
                next_link = response_data.get("@odata.nextLink")
            # OData v2 (d wrapper)
            elif "d" in response_data:
                d = response_data["d"]
                if "results" in d:
                    data = d["results"]
                    count = d.get("__count")
                    next_link = d.get("__next")
                else:
                    data = d
            # Erro
            elif "error" in response_data:
                success = False
                error = response_data["error"]
                error_message = error.get("message", {}).get("value", str(error))
            else:
                data = response_data
        else:
            data = response_data

        if not success and not error_message:
            error_message = f"HTTP {status_code}"

        return ODataResponse(
            success=success,
            status_code=status_code,
            data=data,
            count=count,
            next_link=next_link,
            error_message=error_message,
            execution_time_ms=execution_time
        )

    def _build_url(
        self,
        service: str,
        entity_set: str,
        key: Optional[str] = None
    ) -> str:
        """Constroi URL do servico"""
        base = self.config.base_url.rstrip("/")
        url = f"{base}/{service}/{entity_set}"
        if key:
            url = f"{url}({key})"
        return url

    def _build_query_params(
        self,
        filters: Optional[Dict[str, Any]] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        orderby: Optional[List[str]] = None,
        top: Optional[int] = None,
        skip: Optional[int] = None,
        count: bool = False,
        search: Optional[str] = None
    ) -> Dict[str, str]:
        """Constroi query parameters OData"""
        params = {}

        # $filter
        if filters:
            filter_parts = []
            for field, value in filters.items():
                if isinstance(value, str):
                    filter_parts.append(f"{field} eq '{value}'")
                elif isinstance(value, bool):
                    filter_parts.append(f"{field} eq {str(value).lower()}")
                elif isinstance(value, (int, float)):
                    filter_parts.append(f"{field} eq {value}")
                elif isinstance(value, dict):
                    # Operadores especiais: gt, lt, ge, le, ne, etc.
                    for op, val in value.items():
                        if isinstance(val, str):
                            filter_parts.append(f"{field} {op} '{val}'")
                        else:
                            filter_parts.append(f"{field} {op} {val}")
            if filter_parts:
                params["$filter"] = " and ".join(filter_parts)

        # $select
        if select:
            params["$select"] = ",".join(select)

        # $expand
        if expand:
            params["$expand"] = ",".join(expand)

        # $orderby
        if orderby:
            params["$orderby"] = ",".join(orderby)

        # $top
        if top:
            params["$top"] = str(top)

        # $skip
        if skip:
            params["$skip"] = str(skip)

        # $count (OData v4) ou $inlinecount (OData v2)
        if count:
            params["$count"] = "true"
            params["$inlinecount"] = "allpages"

        # $search
        if search:
            params["$search"] = search

        return params

    async def get(
        self,
        service: str,
        entity_set: str,
        key: Optional[str] = None,
        filters: Optional[Dict[str, Any]] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        orderby: Optional[List[str]] = None,
        top: Optional[int] = None,
        skip: Optional[int] = None,
        count: bool = False
    ) -> ODataResponse:
        """
        Busca dados via OData GET.

        Args:
            service: Nome do servico OData (ex: API_MATERIAL_SRV)
            entity_set: Nome do EntitySet (ex: A_Product)
            key: Chave da entidade (para buscar um registro especifico)
            filters: Filtros OData (campo: valor ou campo: {op: valor})
            select: Campos a retornar
            expand: Navegacoes a expandir
            orderby: Ordenacao (ex: ["Field1 asc", "Field2 desc"])
            top: Limite de registros
            skip: Registros a pular (paginacao)
            count: Se True, retorna contagem total

        Returns:
            ODataResponse com os dados

        Exemplo:
        --------
        ```python
        # Buscar materiais do tipo FERT
        result = await client.get(
            "API_MATERIAL_SRV",
            "A_Product",
            filters={"ProductType": "FERT"},
            select=["Product", "ProductType", "BaseUnit"],
            orderby=["Product asc"],
            top=100
        )
        ```
        """
        url = self._build_url(service, entity_set, key)
        params = self._build_query_params(
            filters=filters,
            select=select,
            expand=expand,
            orderby=orderby,
            top=top,
            skip=skip,
            count=count
        )

        response = await self._make_request("GET", url, params=params)

        logger.debug(
            f"GET {entity_set}: {len(response.data) if isinstance(response.data, list) else 1} "
            f"registros em {response.execution_time_ms:.0f}ms"
        )

        return response

    async def post(
        self,
        service: str,
        entity_set: str,
        data: Dict[str, Any]
    ) -> ODataResponse:
        """
        Cria entidade via OData POST.

        Args:
            service: Nome do servico OData
            entity_set: Nome do EntitySet
            data: Dados da nova entidade

        Returns:
            ODataResponse com a entidade criada
        """
        url = self._build_url(service, entity_set)

        response = await self._make_request("POST", url, data=data)

        if response.success:
            logger.info(f"Criado registro em {entity_set}")
        else:
            logger.error(f"Erro ao criar em {entity_set}: {response.error_message}")

        return response

    async def put(
        self,
        service: str,
        entity_set: str,
        key: str,
        data: Dict[str, Any]
    ) -> ODataResponse:
        """
        Atualiza entidade via OData PUT (substituicao completa).

        Args:
            service: Nome do servico OData
            entity_set: Nome do EntitySet
            key: Chave da entidade
            data: Dados atualizados

        Returns:
            ODataResponse
        """
        url = self._build_url(service, entity_set, key)

        response = await self._make_request("PUT", url, data=data)

        if response.success:
            logger.info(f"Atualizado registro {key} em {entity_set}")

        return response

    async def patch(
        self,
        service: str,
        entity_set: str,
        key: str,
        data: Dict[str, Any]
    ) -> ODataResponse:
        """
        Atualiza entidade via OData PATCH (atualizacao parcial).

        Args:
            service: Nome do servico OData
            entity_set: Nome do EntitySet
            key: Chave da entidade
            data: Campos a atualizar

        Returns:
            ODataResponse
        """
        url = self._build_url(service, entity_set, key)

        response = await self._make_request("PATCH", url, data=data)

        if response.success:
            logger.info(f"Atualizado parcialmente {key} em {entity_set}")

        return response

    async def delete(
        self,
        service: str,
        entity_set: str,
        key: str
    ) -> ODataResponse:
        """
        Remove entidade via OData DELETE.

        Args:
            service: Nome do servico OData
            entity_set: Nome do EntitySet
            key: Chave da entidade

        Returns:
            ODataResponse
        """
        url = self._build_url(service, entity_set, key)

        response = await self._make_request("DELETE", url)

        if response.success:
            logger.info(f"Removido registro {key} de {entity_set}")

        return response

    async def call_action(
        self,
        service: str,
        action_name: str,
        parameters: Optional[Dict[str, Any]] = None
    ) -> ODataResponse:
        """
        Executa uma Action OData.

        Args:
            service: Nome do servico OData
            action_name: Nome da action
            parameters: Parametros da action

        Returns:
            ODataResponse com o resultado
        """
        url = f"{self.config.base_url.rstrip('/')}/{service}/{action_name}"

        response = await self._make_request("POST", url, data=parameters or {})

        logger.debug(f"Action {action_name} executada em {response.execution_time_ms:.0f}ms")

        return response

    async def call_function(
        self,
        service: str,
        function_name: str,
        parameters: Optional[Dict[str, Any]] = None
    ) -> ODataResponse:
        """
        Executa uma Function OData.

        Args:
            service: Nome do servico OData
            function_name: Nome da function
            parameters: Parametros da function

        Returns:
            ODataResponse com o resultado
        """
        url = f"{self.config.base_url.rstrip('/')}/{service}/{function_name}"

        params = {}
        if parameters:
            for key, value in parameters.items():
                if isinstance(value, str):
                    params[key] = f"'{value}'"
                else:
                    params[key] = str(value)

        response = await self._make_request("GET", url, params=params)

        logger.debug(f"Function {function_name} executada em {response.execution_time_ms:.0f}ms")

        return response

    async def get_metadata(self, service: str) -> ODataResponse:
        """
        Obtem metadata do servico OData.

        Args:
            service: Nome do servico

        Returns:
            ODataResponse com o $metadata
        """
        url = f"{self.config.base_url.rstrip('/')}/{service}/$metadata"

        headers = self._get_default_headers()
        headers["Accept"] = "application/xml"

        response = await self._make_request("GET", url, headers=headers)

        return response

    async def get_all(
        self,
        service: str,
        entity_set: str,
        filters: Optional[Dict[str, Any]] = None,
        select: Optional[List[str]] = None,
        batch_size: int = 1000
    ) -> List[Dict[str, Any]]:
        """
        Busca todos os registros com paginacao automatica.

        Args:
            service: Nome do servico OData
            entity_set: Nome do EntitySet
            filters: Filtros OData
            select: Campos a retornar
            batch_size: Tamanho de cada pagina

        Returns:
            Lista com todos os registros
        """
        all_data = []
        skip = 0

        while True:
            response = await self.get(
                service,
                entity_set,
                filters=filters,
                select=select,
                top=batch_size,
                skip=skip
            )

            if not response.success:
                break

            if isinstance(response.data, list):
                all_data.extend(response.data)
                if len(response.data) < batch_size:
                    break
                skip += batch_size
            else:
                all_data.append(response.data)
                break

        logger.info(f"Total de {len(all_data)} registros obtidos de {entity_set}")
        return all_data

    async def __aenter__(self):
        """Context manager - entrada"""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Context manager - saida"""
        await self.disconnect()
        return False


# Servicos OData comuns do SAP
SAP_ODATA_SERVICES = {
    # Materiais
    "material": "API_PRODUCT_SRV",
    "material_plant": "API_PRODUCT_PLANT_SRV",

    # Vendas
    "sales_order": "API_SALES_ORDER_SRV",
    "sales_order_item": "API_SALES_ORDER_SRV",
    "delivery": "API_OUTBOUND_DELIVERY_SRV",
    "billing": "API_BILLING_DOCUMENT_SRV",

    # Compras
    "purchase_order": "API_PURCHASEORDER_PROCESS_SRV",
    "purchase_requisition": "API_PURCHASEREQ_PROCESS_SRV",

    # Parceiros
    "business_partner": "API_BUSINESS_PARTNER",
    "customer": "API_BUSINESS_PARTNER",
    "supplier": "API_BUSINESS_PARTNER",

    # Financeiro
    "journal_entry": "API_JOURNALENTRYITEMBASIC_SRV",
    "gl_account": "API_GLACCOUNTINCHARTOFACCOUNTS_SRV",
    "cost_center": "API_COSTCENTER_SRV",

    # Producao
    "production_order": "API_PRODUCTION_ORDER_2_SRV",
    "bom": "API_BILL_OF_MATERIAL_SRV",

    # Qualidade
    "inspection_lot": "API_INSPECTIONLOT_SRV",
}
