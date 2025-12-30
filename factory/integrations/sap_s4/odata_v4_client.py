# -*- coding: utf-8 -*-
"""
SAP OData V4 Client
===================
Cliente completo para APIs OData V4 do SAP S/4HANA.

Este modulo implementa:
- Cliente OData V4 com suporte completo a queries
- Query Builder para construcao fluente de consultas
- Suporte a operacoes CRUD
- Batch requests
- Deep insert/update
- Tratamento de erros SAP

Autor: Fabrica de Agentes
"""

import json
import uuid
from dataclasses import dataclass, field
from datetime import datetime, date
from enum import Enum
from typing import Any, Dict, List, Optional, Union, Callable
from urllib.parse import urlencode, quote
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

from .sap_auth import SAPAuthenticator, SAPAuthError

logger = logging.getLogger(__name__)


class ODataError(Exception):
    """Excecao para erros OData"""

    def __init__(
        self,
        message: str,
        status_code: int = 0,
        odata_error: Optional[Dict] = None,
        request_id: Optional[str] = None
    ):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.odata_error = odata_error or {}
        self.request_id = request_id

    def __str__(self):
        parts = []
        if self.status_code:
            parts.append(f"[{self.status_code}]")
        parts.append(self.message)
        if self.odata_error.get("code"):
            parts.append(f"(SAP Error: {self.odata_error['code']})")
        return " ".join(parts)


class ODataAuthError(ODataError):
    """Excecao para erros de autenticacao OData"""
    pass


class ODataValidationError(ODataError):
    """Excecao para erros de validacao"""
    pass


@dataclass
class ODataConfig:
    """
    Configuracao do cliente OData

    Attributes:
        base_url: URL base do servico OData
        service_path: Caminho do servico (ex: /sap/opu/odata4/sap/api_business_partner/)
        api_version: Versao da API OData (v4)
        default_page_size: Tamanho padrao de pagina
        max_page_size: Tamanho maximo de pagina
        timeout: Timeout em segundos
        verify_ssl: Verificar certificado SSL
        csrf_required: Se CSRF token e necessario
        language: Idioma (PT, EN, etc)
        format: Formato de resposta (json)
    """
    base_url: str
    service_path: str = ""
    api_version: str = "v4"
    default_page_size: int = 100
    max_page_size: int = 1000
    timeout: int = 60
    verify_ssl: bool = True
    csrf_required: bool = True
    language: str = "PT"
    format: str = "json"

    @property
    def full_url(self) -> str:
        """Retorna URL completa do servico"""
        base = self.base_url.rstrip("/")
        path = self.service_path.strip("/")
        return f"{base}/{path}" if path else base


class FilterOperator(str, Enum):
    """Operadores de filtro OData"""
    EQ = "eq"
    NE = "ne"
    GT = "gt"
    GE = "ge"
    LT = "lt"
    LE = "le"
    AND = "and"
    OR = "or"
    NOT = "not"
    CONTAINS = "contains"
    STARTSWITH = "startswith"
    ENDSWITH = "endswith"
    IN = "in"


class ODataQueryBuilder:
    """
    Query Builder para construcao fluente de consultas OData V4

    Exemplo:
    ```python
    query = (ODataQueryBuilder()
        .select("BusinessPartner", "BusinessPartnerFullName", "Country")
        .filter("Country", FilterOperator.EQ, "BR")
        .filter("BusinessPartnerCategory", FilterOperator.EQ, "1")
        .expand("to_BusinessPartnerAddress")
        .orderby("BusinessPartnerFullName")
        .top(100)
        .skip(0)
    )

    # Usar com cliente
    results = await client.query("A_BusinessPartner", query)
    ```
    """

    def __init__(self):
        self._select: List[str] = []
        self._filters: List[str] = []
        self._expand: List[str] = []
        self._orderby: List[str] = []
        self._top: Optional[int] = None
        self._skip: Optional[int] = None
        self._search: Optional[str] = None
        self._count: bool = False
        self._apply: Optional[str] = None

    def select(self, *fields: str) -> "ODataQueryBuilder":
        """
        Define campos a retornar ($select)

        Args:
            *fields: Nomes dos campos

        Returns:
            Self para encadeamento
        """
        self._select.extend(fields)
        return self

    def filter(
        self,
        field: str,
        operator: Union[FilterOperator, str],
        value: Any
    ) -> "ODataQueryBuilder":
        """
        Adiciona filtro ($filter)

        Args:
            field: Nome do campo
            operator: Operador de comparacao
            value: Valor para comparar

        Returns:
            Self para encadeamento
        """
        op = operator.value if isinstance(operator, FilterOperator) else operator

        # Formatar valor
        formatted_value = self._format_value(value)

        # Funcoes especiais
        if op in ("contains", "startswith", "endswith"):
            self._filters.append(f"{op}({field},{formatted_value})")
        elif op == "in":
            if isinstance(value, (list, tuple)):
                values = ",".join(self._format_value(v) for v in value)
                self._filters.append(f"{field} in ({values})")
            else:
                self._filters.append(f"{field} in ({formatted_value})")
        else:
            self._filters.append(f"{field} {op} {formatted_value}")

        return self

    def filter_raw(self, expression: str) -> "ODataQueryBuilder":
        """
        Adiciona filtro raw (expressao completa)

        Args:
            expression: Expressao de filtro completa

        Returns:
            Self para encadeamento
        """
        self._filters.append(expression)
        return self

    def and_filter(
        self,
        field: str,
        operator: Union[FilterOperator, str],
        value: Any
    ) -> "ODataQueryBuilder":
        """Alias para filter() - adiciona com AND implicito"""
        return self.filter(field, operator, value)

    def or_filter(
        self,
        field: str,
        operator: Union[FilterOperator, str],
        value: Any
    ) -> "ODataQueryBuilder":
        """
        Adiciona filtro com OR

        Args:
            field: Nome do campo
            operator: Operador de comparacao
            value: Valor para comparar

        Returns:
            Self para encadeamento
        """
        if self._filters:
            # Pegar ultimo filtro e combinar com OR
            last_filter = self._filters.pop()
            op = operator.value if isinstance(operator, FilterOperator) else operator
            formatted_value = self._format_value(value)
            new_filter = f"({last_filter} or {field} {op} {formatted_value})"
            self._filters.append(new_filter)
        else:
            self.filter(field, operator, value)
        return self

    def expand(self, *associations: str, nested_query: Optional["ODataQueryBuilder"] = None) -> "ODataQueryBuilder":
        """
        Adiciona expansao de associacoes ($expand)

        Args:
            *associations: Nomes das associacoes
            nested_query: Query aninhada para a expansao

        Returns:
            Self para encadeamento
        """
        if nested_query and len(associations) == 1:
            # Expansao com query aninhada
            nested_params = nested_query.build(include_prefix=False)
            if nested_params:
                self._expand.append(f"{associations[0]}({nested_params})")
            else:
                self._expand.append(associations[0])
        else:
            self._expand.extend(associations)
        return self

    def orderby(self, *fields: str, desc: bool = False) -> "ODataQueryBuilder":
        """
        Define ordenacao ($orderby)

        Args:
            *fields: Campos para ordenar
            desc: Se True, ordem descendente

        Returns:
            Self para encadeamento
        """
        direction = " desc" if desc else ""
        for field in fields:
            self._orderby.append(f"{field}{direction}")
        return self

    def top(self, count: int) -> "ODataQueryBuilder":
        """
        Limita numero de resultados ($top)

        Args:
            count: Numero maximo de resultados

        Returns:
            Self para encadeamento
        """
        self._top = count
        return self

    def skip(self, count: int) -> "ODataQueryBuilder":
        """
        Pula resultados para paginacao ($skip)

        Args:
            count: Numero de resultados para pular

        Returns:
            Self para encadeamento
        """
        self._skip = count
        return self

    def search(self, term: str) -> "ODataQueryBuilder":
        """
        Adiciona busca textual ($search)

        Args:
            term: Termo de busca

        Returns:
            Self para encadeamento
        """
        self._search = term
        return self

    def count(self, include: bool = True) -> "ODataQueryBuilder":
        """
        Inclui contagem total ($count)

        Args:
            include: Se True, inclui contagem

        Returns:
            Self para encadeamento
        """
        self._count = include
        return self

    def apply(self, expression: str) -> "ODataQueryBuilder":
        """
        Adiciona transformacao de agregacao ($apply)

        Args:
            expression: Expressao de agregacao

        Returns:
            Self para encadeamento

        Exemplo:
            query.apply("groupby((Country),aggregate(TotalAmount with sum as TotalByCountry))")
        """
        self._apply = expression
        return self

    def _format_value(self, value: Any) -> str:
        """Formata valor para OData"""
        if value is None:
            return "null"
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, str):
            # Escapar aspas simples
            escaped = value.replace("'", "''")
            return f"'{escaped}'"
        elif isinstance(value, (int, float)):
            return str(value)
        elif isinstance(value, datetime):
            return value.strftime("%Y-%m-%dT%H:%M:%SZ")
        elif isinstance(value, date):
            return value.strftime("%Y-%m-%d")
        elif isinstance(value, uuid.UUID):
            return str(value)
        else:
            return f"'{str(value)}'"

    def build(self, include_prefix: bool = True) -> str:
        """
        Constroi query string

        Args:
            include_prefix: Se True, inclui '?' no inicio

        Returns:
            Query string formatada
        """
        params = []

        if self._select:
            params.append(f"$select={','.join(self._select)}")

        if self._filters:
            filter_expr = " and ".join(self._filters)
            params.append(f"$filter={filter_expr}")

        if self._expand:
            params.append(f"$expand={','.join(self._expand)}")

        if self._orderby:
            params.append(f"$orderby={','.join(self._orderby)}")

        if self._top is not None:
            params.append(f"$top={self._top}")

        if self._skip is not None:
            params.append(f"$skip={self._skip}")

        if self._search:
            params.append(f"$search={quote(self._search)}")

        if self._count:
            params.append("$count=true")

        if self._apply:
            params.append(f"$apply={quote(self._apply)}")

        if not params:
            return ""

        query = "&".join(params)
        return f"?{query}" if include_prefix else query

    def reset(self) -> "ODataQueryBuilder":
        """Reseta o builder"""
        self._select = []
        self._filters = []
        self._expand = []
        self._orderby = []
        self._top = None
        self._skip = None
        self._search = None
        self._count = False
        self._apply = None
        return self


@dataclass
class ODataResponse:
    """Resposta de uma requisicao OData"""
    data: Union[Dict, List]
    count: Optional[int] = None
    next_link: Optional[str] = None
    delta_link: Optional[str] = None
    context: Optional[str] = None
    etag: Optional[str] = None
    request_id: Optional[str] = None

    @property
    def is_collection(self) -> bool:
        """Verifica se resposta e uma colecao"""
        return isinstance(self.data, list)

    @property
    def items(self) -> List[Dict]:
        """Retorna itens como lista"""
        if isinstance(self.data, list):
            return self.data
        return [self.data] if self.data else []

    @property
    def first(self) -> Optional[Dict]:
        """Retorna primeiro item ou None"""
        items = self.items
        return items[0] if items else None


class ODataV4Client:
    """
    Cliente OData V4 para SAP S/4HANA

    Exemplo de uso:
    ```python
    from factory.integrations.sap_s4 import ODataV4Client, ODataConfig, ODataQueryBuilder

    # Configurar cliente
    config = ODataConfig(
        base_url="https://my-s4.s4hana.ondemand.com",
        service_path="/sap/opu/odata4/sap/api_business_partner/srvd_a2x/sap/a_businesspartner/0001/"
    )

    client = ODataV4Client(config, authenticator, tenant_id="tenant-001")

    # Query simples
    bp_list = await client.get("A_BusinessPartner", top=10)

    # Query complexa
    query = (ODataQueryBuilder()
        .select("BusinessPartner", "BusinessPartnerFullName")
        .filter("Country", "eq", "BR")
        .orderby("BusinessPartnerFullName")
        .top(50)
    )
    results = await client.query("A_BusinessPartner", query)

    # Criar registro
    new_bp = await client.create("A_BusinessPartner", {
        "BusinessPartnerCategory": "1",
        "BusinessPartnerFullName": "Empresa LTDA"
    })

    # Atualizar
    await client.update("A_BusinessPartner('0001000000')", {
        "BusinessPartnerFullName": "Empresa SA"
    })

    # Deletar
    await client.delete("A_BusinessPartner('0001000000')")
    ```
    """

    def __init__(
        self,
        config: ODataConfig,
        authenticator: SAPAuthenticator,
        tenant_id: str = ""
    ):
        """
        Inicializa cliente OData

        Args:
            config: Configuracao do servico OData
            authenticator: Autenticador SAP
            tenant_id: ID do tenant para isolamento multi-tenant
        """
        self.config = config
        self.authenticator = authenticator
        self.tenant_id = tenant_id
        self._csrf_token: Optional[str] = None
        self._session_cookies: Dict = {}

        if not REQUESTS_AVAILABLE:
            logger.warning(
                "Biblioteca 'requests' nao instalada. "
                "Use: pip install requests"
            )

        if not tenant_id:
            logger.warning("tenant_id nao configurado para cliente OData")

    @property
    def base_url(self) -> str:
        """Retorna URL base do servico"""
        return self.config.full_url

    def _get_default_headers(self) -> Dict[str, str]:
        """Retorna headers padrao incluindo X-Tenant-ID"""
        headers = {
            "Accept": "application/json",
            "Accept-Language": self.config.language,
            "Content-Type": "application/json",
            "sap-cancel-on-close": "true"
        }

        # Adicionar header de tenant para isolamento
        if self.tenant_id:
            headers["X-Tenant-ID"] = self.tenant_id

        return headers

    async def _get_headers(self, include_csrf: bool = False) -> Dict[str, str]:
        """
        Obtem headers completos incluindo autenticacao

        Args:
            include_csrf: Se True, inclui CSRF token

        Returns:
            Dict com headers
        """
        headers = self._get_default_headers()

        # Adicionar autenticacao
        auth_headers = await self.authenticator.get_auth_headers()
        headers.update(auth_headers)

        # Adicionar CSRF se necessario
        if include_csrf and self._csrf_token:
            headers["X-CSRF-Token"] = self._csrf_token

        return headers

    async def _fetch_csrf_token(self):
        """Busca CSRF token para operacoes de escrita"""
        if not self.config.csrf_required:
            return

        try:
            headers = await self._get_headers()
            headers["X-CSRF-Token"] = "Fetch"

            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.head(
                        self.base_url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        self._csrf_token = response.headers.get("X-CSRF-Token")
            else:
                response = requests.head(
                    self.base_url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                self._csrf_token = response.headers.get("X-CSRF-Token")

            logger.debug(f"CSRF token obtido: {self._csrf_token[:10]}..." if self._csrf_token else "Sem CSRF token")

        except Exception as e:
            logger.warning(f"Falha ao obter CSRF token: {e}")

    def _parse_response(self, response_data: Dict, etag: Optional[str] = None, request_id: Optional[str] = None) -> ODataResponse:
        """
        Parse da resposta OData

        Args:
            response_data: Dados da resposta
            etag: ETag do header
            request_id: ID da requisicao

        Returns:
            ODataResponse
        """
        # Verificar se e uma colecao (tem "value")
        if "value" in response_data:
            data = response_data["value"]
            count = response_data.get("@odata.count")
            next_link = response_data.get("@odata.nextLink")
            delta_link = response_data.get("@odata.deltaLink")
        else:
            # Registro unico
            data = response_data
            count = 1
            next_link = None
            delta_link = None

        return ODataResponse(
            data=data,
            count=count,
            next_link=next_link,
            delta_link=delta_link,
            context=response_data.get("@odata.context"),
            etag=etag,
            request_id=request_id
        )

    def _handle_error_response(self, status_code: int, response_data: Any, request_id: Optional[str] = None):
        """
        Trata resposta de erro

        Args:
            status_code: Codigo HTTP
            response_data: Dados da resposta
            request_id: ID da requisicao

        Raises:
            ODataAuthError: Para erros 401/403
            ODataValidationError: Para erros 400
            ODataError: Para outros erros
        """
        odata_error = {}
        message = f"Erro HTTP {status_code}"

        if isinstance(response_data, dict):
            # Parse erro OData
            error_obj = response_data.get("error", {})
            odata_error = {
                "code": error_obj.get("code", ""),
                "message": error_obj.get("message", ""),
                "target": error_obj.get("target", ""),
                "details": error_obj.get("details", [])
            }
            message = odata_error.get("message") or message

        if status_code in (401, 403):
            raise ODataAuthError(
                f"Erro de autenticacao: {message}",
                status_code=status_code,
                odata_error=odata_error,
                request_id=request_id
            )
        elif status_code == 400:
            raise ODataValidationError(
                f"Erro de validacao: {message}",
                status_code=status_code,
                odata_error=odata_error,
                request_id=request_id
            )
        else:
            raise ODataError(
                message,
                status_code=status_code,
                odata_error=odata_error,
                request_id=request_id
            )

    async def get(
        self,
        entity_set: str,
        key: Optional[str] = None,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        top: Optional[int] = None,
        skip: Optional[int] = None,
        filter_expr: Optional[str] = None,
        orderby: Optional[str] = None,
        count: bool = False
    ) -> ODataResponse:
        """
        Busca registros (GET)

        Args:
            entity_set: Nome do entity set (ex: "A_BusinessPartner")
            key: Chave do registro (ex: "'0001000000'")
            select: Lista de campos
            expand: Lista de associacoes para expandir
            top: Limite de registros
            skip: Registros para pular
            filter_expr: Expressao de filtro
            orderby: Ordenacao
            count: Incluir contagem total

        Returns:
            ODataResponse com os dados
        """
        # Construir URL
        url = f"{self.base_url}/{entity_set}"
        if key:
            url = f"{url}({key})"

        # Construir query params
        params = []
        if select:
            params.append(f"$select={','.join(select)}")
        if expand:
            params.append(f"$expand={','.join(expand)}")
        if top:
            params.append(f"$top={top}")
        if skip:
            params.append(f"$skip={skip}")
        if filter_expr:
            params.append(f"$filter={filter_expr}")
        if orderby:
            params.append(f"$orderby={orderby}")
        if count:
            params.append("$count=true")

        if params:
            url = f"{url}?{'&'.join(params)}"

        # Fazer requisicao
        headers = await self._get_headers()

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.get(
                        url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")
                        etag = response.headers.get("ETag")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        data = await response.json()
                        return self._parse_response(data, etag, request_id)
            else:
                response = requests.get(
                    url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")
                etag = response.headers.get("ETag")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                return self._parse_response(response.json(), etag, request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao fazer requisicao GET: {str(e)}")

    async def query(
        self,
        entity_set: str,
        query_builder: ODataQueryBuilder
    ) -> ODataResponse:
        """
        Executa query usando ODataQueryBuilder

        Args:
            entity_set: Nome do entity set
            query_builder: Instancia de ODataQueryBuilder

        Returns:
            ODataResponse com os dados
        """
        url = f"{self.base_url}/{entity_set}{query_builder.build()}"
        headers = await self._get_headers()

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.get(
                        url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")
                        etag = response.headers.get("ETag")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        data = await response.json()
                        return self._parse_response(data, etag, request_id)
            else:
                response = requests.get(
                    url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")
                etag = response.headers.get("ETag")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                return self._parse_response(response.json(), etag, request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao executar query: {str(e)}")

    async def create(
        self,
        entity_set: str,
        data: Dict[str, Any]
    ) -> ODataResponse:
        """
        Cria novo registro (POST)

        Args:
            entity_set: Nome do entity set
            data: Dados do registro

        Returns:
            ODataResponse com registro criado
        """
        await self._fetch_csrf_token()

        url = f"{self.base_url}/{entity_set}"
        headers = await self._get_headers(include_csrf=True)

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.post(
                        url,
                        headers=headers,
                        json=data,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")
                        etag = response.headers.get("ETag")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        result = await response.json()
                        return self._parse_response(result, etag, request_id)
            else:
                response = requests.post(
                    url,
                    headers=headers,
                    json=data,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")
                etag = response.headers.get("ETag")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                return self._parse_response(response.json(), etag, request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao criar registro: {str(e)}")

    async def update(
        self,
        entity_path: str,
        data: Dict[str, Any],
        etag: Optional[str] = None,
        patch: bool = True
    ) -> ODataResponse:
        """
        Atualiza registro (PATCH ou PUT)

        Args:
            entity_path: Caminho do registro (ex: "A_BusinessPartner('0001')")
            data: Dados para atualizar
            etag: ETag para concorrencia otimista
            patch: Se True usa PATCH, senao PUT

        Returns:
            ODataResponse
        """
        await self._fetch_csrf_token()

        url = f"{self.base_url}/{entity_path}"
        headers = await self._get_headers(include_csrf=True)

        if etag:
            headers["If-Match"] = etag

        method = "PATCH" if patch else "PUT"

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.request(
                        method,
                        url,
                        headers=headers,
                        json=data,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")
                        new_etag = response.headers.get("ETag")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        # PATCH pode retornar 204 No Content
                        if response.status == 204:
                            return ODataResponse(data={}, etag=new_etag, request_id=request_id)

                        result = await response.json()
                        return self._parse_response(result, new_etag, request_id)
            else:
                response = requests.request(
                    method,
                    url,
                    headers=headers,
                    json=data,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")
                new_etag = response.headers.get("ETag")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                if response.status_code == 204:
                    return ODataResponse(data={}, etag=new_etag, request_id=request_id)

                return self._parse_response(response.json(), new_etag, request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao atualizar registro: {str(e)}")

    async def delete(
        self,
        entity_path: str,
        etag: Optional[str] = None
    ) -> bool:
        """
        Deleta registro (DELETE)

        Args:
            entity_path: Caminho do registro
            etag: ETag para concorrencia otimista

        Returns:
            True se deletado com sucesso
        """
        await self._fetch_csrf_token()

        url = f"{self.base_url}/{entity_path}"
        headers = await self._get_headers(include_csrf=True)

        if etag:
            headers["If-Match"] = etag
        else:
            headers["If-Match"] = "*"  # Forcar delete

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.delete(
                        url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data)

                        return response.status in (200, 204)
            else:
                response = requests.delete(
                    url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data)

                return response.status_code in (200, 204)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao deletar registro: {str(e)}")

    async def get_metadata(self) -> Dict:
        """
        Busca metadata do servico ($metadata)

        Returns:
            Dict com metadata do servico
        """
        url = f"{self.base_url}/$metadata"
        headers = await self._get_headers()
        headers["Accept"] = "application/xml"

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.get(
                        url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        if response.status >= 400:
                            self._handle_error_response(response.status, {})

                        text = await response.text()
                        return {"metadata_xml": text}
            else:
                response = requests.get(
                    url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )

                if response.status_code >= 400:
                    self._handle_error_response(response.status_code, {})

                return {"metadata_xml": response.text}

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao buscar metadata: {str(e)}")

    async def call_function(
        self,
        function_name: str,
        parameters: Optional[Dict[str, Any]] = None
    ) -> ODataResponse:
        """
        Chama Function Import OData

        Args:
            function_name: Nome da funcao
            parameters: Parametros da funcao

        Returns:
            ODataResponse com resultado
        """
        # Construir URL com parametros
        url = f"{self.base_url}/{function_name}"
        if parameters:
            param_str = ",".join(
                f"{k}={self._format_param_value(v)}"
                for k, v in parameters.items()
            )
            url = f"{url}({param_str})"
        else:
            url = f"{url}()"

        headers = await self._get_headers()

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.get(
                        url,
                        headers=headers,
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        data = await response.json()
                        return self._parse_response(data, request_id=request_id)
            else:
                response = requests.get(
                    url,
                    headers=headers,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                return self._parse_response(response.json(), request_id=request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao chamar funcao: {str(e)}")

    async def call_action(
        self,
        action_name: str,
        parameters: Optional[Dict[str, Any]] = None,
        bound_entity: Optional[str] = None
    ) -> ODataResponse:
        """
        Chama Action OData (POST)

        Args:
            action_name: Nome da action
            parameters: Parametros da action
            bound_entity: Entidade vinculada (para bound actions)

        Returns:
            ODataResponse com resultado
        """
        await self._fetch_csrf_token()

        # Construir URL
        if bound_entity:
            url = f"{self.base_url}/{bound_entity}/{action_name}"
        else:
            url = f"{self.base_url}/{action_name}"

        headers = await self._get_headers(include_csrf=True)

        try:
            if AIOHTTP_AVAILABLE:
                async with aiohttp.ClientSession() as session:
                    async with session.post(
                        url,
                        headers=headers,
                        json=parameters or {},
                        ssl=self.config.verify_ssl if self.config.verify_ssl else False,
                        timeout=aiohttp.ClientTimeout(total=self.config.timeout)
                    ) as response:
                        request_id = response.headers.get("sap-message-id")

                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error_response(response.status, error_data, request_id)

                        if response.status == 204:
                            return ODataResponse(data={}, request_id=request_id)

                        data = await response.json()
                        return self._parse_response(data, request_id=request_id)
            else:
                response = requests.post(
                    url,
                    headers=headers,
                    json=parameters or {},
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )
                request_id = response.headers.get("sap-message-id")

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error_response(response.status_code, error_data, request_id)

                if response.status_code == 204:
                    return ODataResponse(data={}, request_id=request_id)

                return self._parse_response(response.json(), request_id=request_id)

        except ODataError:
            raise
        except Exception as e:
            raise ODataError(f"Erro ao chamar action: {str(e)}")

    def _format_param_value(self, value: Any) -> str:
        """Formata valor de parametro para URL"""
        if value is None:
            return "null"
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, str):
            return f"'{quote(value)}'"
        elif isinstance(value, (int, float)):
            return str(value)
        else:
            return f"'{quote(str(value))}'"

    async def get_all(
        self,
        entity_set: str,
        query_builder: Optional[ODataQueryBuilder] = None,
        max_records: int = 10000,
        page_size: int = 1000,
        on_page: Optional[Callable[[List[Dict], int], None]] = None
    ) -> List[Dict]:
        """
        Busca todos os registros com paginacao automatica

        Args:
            entity_set: Nome do entity set
            query_builder: Query builder opcional
            max_records: Maximo de registros
            page_size: Tamanho da pagina
            on_page: Callback chamado para cada pagina

        Returns:
            Lista com todos os registros
        """
        all_records = []
        skip = 0

        # Clonar query builder ou criar novo
        if query_builder:
            # Reset top/skip para controlar aqui
            query_builder._top = page_size
            query_builder._skip = skip
        else:
            query_builder = ODataQueryBuilder().top(page_size).skip(skip)

        while len(all_records) < max_records:
            query_builder._skip = skip

            response = await self.query(entity_set, query_builder)

            if not response.items:
                break

            all_records.extend(response.items)

            if on_page:
                on_page(response.items, len(all_records))

            if len(response.items) < page_size:
                break

            skip += page_size

        return all_records[:max_records]
