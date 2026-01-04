# -*- coding: utf-8 -*-
"""
SAP Graph Client
================
Cliente para SAP Graph API - camada de integracao unificada do SAP.

SAP Graph fornece uma visao unificada e simplificada dos dados de negocios
atraves de uma API GraphQL-like, abstraindo a complexidade das APIs subjacentes.

Autor: Plataforma E
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, Union
from enum import Enum
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


class GraphError(Exception):
    """Excecao para erros do SAP Graph"""

    def __init__(
        self,
        message: str,
        status_code: int = 0,
        error_code: Optional[str] = None,
        details: Optional[Dict] = None
    ):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.error_code = error_code
        self.details = details or {}

    def __str__(self):
        parts = []
        if self.status_code:
            parts.append(f"[{self.status_code}]")
        if self.error_code:
            parts.append(f"({self.error_code})")
        parts.append(self.message)
        return " ".join(parts)


class GraphEntityType(str, Enum):
    """Tipos de entidade do SAP Graph"""
    BUSINESS_PARTNER = "sap.graph/BusinessPartner"
    CUSTOMER = "sap.graph/Customer"
    SUPPLIER = "sap.graph/Supplier"
    PRODUCT = "sap.graph/Product"
    SALES_ORDER = "sap.graph/SalesOrder"
    PURCHASE_ORDER = "sap.graph/PurchaseOrder"
    EMPLOYEE = "sap.graph/Employee"
    COST_CENTER = "sap.graph/CostCenter"
    PROJECT = "sap.graph/Project"


@dataclass
class GraphConfig:
    """
    Configuracao do cliente SAP Graph

    Attributes:
        graph_url: URL base do SAP Graph (ex: https://my-s4.s4hana.ondemand.com/sap/graph/api/v1/)
        business_data_graph: Nome do Business Data Graph
        timeout: Timeout em segundos
        verify_ssl: Verificar certificado SSL
        language: Idioma preferencial
    """
    graph_url: str
    business_data_graph: str = "default"
    timeout: int = 60
    verify_ssl: bool = True
    language: str = "PT"

    @property
    def base_url(self) -> str:
        """Retorna URL base formatada"""
        return self.graph_url.rstrip("/")


@dataclass
class GraphQueryOptions:
    """Opcoes de query para SAP Graph"""
    select: List[str] = field(default_factory=list)
    expand: List[str] = field(default_factory=list)
    filter: Optional[str] = None
    orderby: Optional[str] = None
    top: Optional[int] = None
    skip: Optional[int] = None
    search: Optional[str] = None

    def to_params(self) -> Dict[str, str]:
        """Converte para parametros de query"""
        params = {}

        if self.select:
            params["$select"] = ",".join(self.select)
        if self.expand:
            params["$expand"] = ",".join(self.expand)
        if self.filter:
            params["$filter"] = self.filter
        if self.orderby:
            params["$orderby"] = self.orderby
        if self.top:
            params["$top"] = str(self.top)
        if self.skip:
            params["$skip"] = str(self.skip)
        if self.search:
            params["$search"] = self.search

        return params


@dataclass
class GraphResponse:
    """Resposta de uma requisicao ao SAP Graph"""
    data: Union[Dict, List]
    count: Optional[int] = None
    next_link: Optional[str] = None
    context: Optional[str] = None

    @property
    def items(self) -> List[Dict]:
        """Retorna itens como lista"""
        if isinstance(self.data, list):
            return self.data
        return [self.data] if self.data else []


class SAPGraphClient:
    """
    Cliente para SAP Graph API

    SAP Graph e uma camada de API unificada que simplifica o acesso a dados
    de negocios em todo o ecossistema SAP.

    Exemplo de uso:
    ```python
    from factory.integrations.sap_s4 import SAPGraphClient, GraphConfig

    config = GraphConfig(
        graph_url="https://my-s4.s4hana.ondemand.com/sap/graph/api/v1/"
    )

    client = SAPGraphClient(config, authenticator)

    # Buscar Business Partners
    partners = await client.get_business_partners(
        top=10,
        filter="Country eq 'BR'"
    )

    # Buscar produtos
    products = await client.get_products(search="Material")

    # Buscar entidade especifica
    customer = await client.get_entity(
        GraphEntityType.CUSTOMER,
        key="CUST001"
    )
    ```
    """

    def __init__(
        self,
        config: GraphConfig,
        authenticator: SAPAuthenticator
    ):
        """
        Inicializa cliente SAP Graph

        Args:
            config: Configuracao do SAP Graph
            authenticator: Autenticador SAP
        """
        self.config = config
        self.authenticator = authenticator

        if not REQUESTS_AVAILABLE:
            logger.warning(
                "Biblioteca 'requests' nao instalada. "
                "Use: pip install requests"
            )

    @property
    def base_url(self) -> str:
        """Retorna URL base"""
        return self.config.base_url

    def _get_default_headers(self) -> Dict[str, str]:
        """Retorna headers padrao"""
        return {
            "Accept": "application/json",
            "Accept-Language": self.config.language,
            "Content-Type": "application/json"
        }

    async def _get_headers(self) -> Dict[str, str]:
        """Obtem headers com autenticacao"""
        headers = self._get_default_headers()
        auth_headers = await self.authenticator.get_auth_headers()
        headers.update(auth_headers)
        return headers

    def _build_url(self, path: str, params: Optional[Dict] = None) -> str:
        """Constroi URL com parametros"""
        url = f"{self.base_url}/{path.lstrip('/')}"

        if params:
            query = "&".join(f"{k}={v}" for k, v in params.items())
            url = f"{url}?{query}"

        return url

    def _parse_response(self, response_data: Dict) -> GraphResponse:
        """Parse da resposta"""
        if "value" in response_data:
            return GraphResponse(
                data=response_data["value"],
                count=response_data.get("@odata.count"),
                next_link=response_data.get("@odata.nextLink"),
                context=response_data.get("@odata.context")
            )
        else:
            return GraphResponse(
                data=response_data,
                context=response_data.get("@odata.context")
            )

    def _handle_error(self, status_code: int, response_data: Any):
        """Trata erro de resposta"""
        error_msg = "Erro desconhecido"
        error_code = None

        if isinstance(response_data, dict):
            error_obj = response_data.get("error", {})
            error_msg = error_obj.get("message", error_msg)
            error_code = error_obj.get("code")

        raise GraphError(
            error_msg,
            status_code=status_code,
            error_code=error_code,
            details=response_data if isinstance(response_data, dict) else {}
        )

    async def _request(
        self,
        method: str,
        path: str,
        params: Optional[Dict] = None,
        data: Optional[Dict] = None
    ) -> GraphResponse:
        """
        Faz requisicao ao SAP Graph

        Args:
            method: Metodo HTTP
            path: Caminho da API
            params: Parametros de query
            data: Dados do body

        Returns:
            GraphResponse
        """
        url = self._build_url(path, params)
        headers = await self._get_headers()

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
                        if response.status >= 400:
                            error_data = await response.json() if response.content_type == "application/json" else {}
                            self._handle_error(response.status, error_data)

                        if response.status == 204:
                            return GraphResponse(data={})

                        result = await response.json()
                        return self._parse_response(result)
            else:
                response = requests.request(
                    method,
                    url,
                    headers=headers,
                    json=data,
                    verify=self.config.verify_ssl,
                    timeout=self.config.timeout
                )

                if response.status_code >= 400:
                    try:
                        error_data = response.json()
                    except Exception:
                        error_data = {}
                    self._handle_error(response.status_code, error_data)

                if response.status_code == 204:
                    return GraphResponse(data={})

                return self._parse_response(response.json())

        except GraphError:
            raise
        except Exception as e:
            raise GraphError(f"Erro na requisicao: {str(e)}")

    # =====================================
    # Business Partners
    # =====================================

    async def get_business_partners(
        self,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        orderby: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        search: Optional[str] = None
    ) -> GraphResponse:
        """
        Busca Business Partners

        Args:
            select: Campos a retornar
            filter: Expressao de filtro
            orderby: Ordenacao
            top: Limite de registros
            skip: Registros para pular
            search: Termo de busca

        Returns:
            GraphResponse com Business Partners
        """
        options = GraphQueryOptions(
            select=select or [],
            filter=filter,
            orderby=orderby,
            top=top,
            skip=skip,
            search=search
        )

        return await self._request(
            "GET",
            "BusinessPartner",
            params=options.to_params()
        )

    async def get_business_partner(self, partner_id: str) -> GraphResponse:
        """
        Busca Business Partner por ID

        Args:
            partner_id: ID do Business Partner

        Returns:
            GraphResponse com dados do parceiro
        """
        return await self._request("GET", f"BusinessPartner('{partner_id}')")

    # =====================================
    # Customers
    # =====================================

    async def get_customers(
        self,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        search: Optional[str] = None
    ) -> GraphResponse:
        """
        Busca Customers

        Args:
            select: Campos a retornar
            filter: Expressao de filtro
            top: Limite
            skip: Offset
            search: Busca

        Returns:
            GraphResponse com Customers
        """
        options = GraphQueryOptions(
            select=select or [],
            filter=filter,
            top=top,
            skip=skip,
            search=search
        )

        return await self._request(
            "GET",
            "Customer",
            params=options.to_params()
        )

    async def get_customer(self, customer_id: str) -> GraphResponse:
        """Busca Customer por ID"""
        return await self._request("GET", f"Customer('{customer_id}')")

    # =====================================
    # Suppliers
    # =====================================

    async def get_suppliers(
        self,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        search: Optional[str] = None
    ) -> GraphResponse:
        """Busca Suppliers"""
        options = GraphQueryOptions(
            select=select or [],
            filter=filter,
            top=top,
            skip=skip,
            search=search
        )

        return await self._request(
            "GET",
            "Supplier",
            params=options.to_params()
        )

    async def get_supplier(self, supplier_id: str) -> GraphResponse:
        """Busca Supplier por ID"""
        return await self._request("GET", f"Supplier('{supplier_id}')")

    # =====================================
    # Products
    # =====================================

    async def get_products(
        self,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        search: Optional[str] = None
    ) -> GraphResponse:
        """Busca Products"""
        options = GraphQueryOptions(
            select=select or [],
            filter=filter,
            top=top,
            skip=skip,
            search=search
        )

        return await self._request(
            "GET",
            "Product",
            params=options.to_params()
        )

    async def get_product(self, product_id: str) -> GraphResponse:
        """Busca Product por ID"""
        return await self._request("GET", f"Product('{product_id}')")

    # =====================================
    # Sales Orders
    # =====================================

    async def get_sales_orders(
        self,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0
    ) -> GraphResponse:
        """
        Busca Sales Orders

        Args:
            select: Campos
            expand: Associacoes para expandir
            filter: Filtro
            top: Limite
            skip: Offset

        Returns:
            GraphResponse com Sales Orders
        """
        options = GraphQueryOptions(
            select=select or [],
            expand=expand or [],
            filter=filter,
            top=top,
            skip=skip
        )

        return await self._request(
            "GET",
            "SalesOrder",
            params=options.to_params()
        )

    async def get_sales_order(
        self,
        order_id: str,
        expand: Optional[List[str]] = None
    ) -> GraphResponse:
        """Busca Sales Order por ID"""
        params = {}
        if expand:
            params["$expand"] = ",".join(expand)

        return await self._request(
            "GET",
            f"SalesOrder('{order_id}')",
            params=params if params else None
        )

    # =====================================
    # Purchase Orders
    # =====================================

    async def get_purchase_orders(
        self,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0
    ) -> GraphResponse:
        """Busca Purchase Orders"""
        options = GraphQueryOptions(
            select=select or [],
            expand=expand or [],
            filter=filter,
            top=top,
            skip=skip
        )

        return await self._request(
            "GET",
            "PurchaseOrder",
            params=options.to_params()
        )

    async def get_purchase_order(
        self,
        order_id: str,
        expand: Optional[List[str]] = None
    ) -> GraphResponse:
        """Busca Purchase Order por ID"""
        params = {}
        if expand:
            params["$expand"] = ",".join(expand)

        return await self._request(
            "GET",
            f"PurchaseOrder('{order_id}')",
            params=params if params else None
        )

    # =====================================
    # Employees
    # =====================================

    async def get_employees(
        self,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100,
        skip: int = 0
    ) -> GraphResponse:
        """Busca Employees"""
        options = GraphQueryOptions(
            select=select or [],
            filter=filter,
            top=top,
            skip=skip
        )

        return await self._request(
            "GET",
            "Employee",
            params=options.to_params()
        )

    async def get_employee(self, employee_id: str) -> GraphResponse:
        """Busca Employee por ID"""
        return await self._request("GET", f"Employee('{employee_id}')")

    # =====================================
    # Cost Centers
    # =====================================

    async def get_cost_centers(
        self,
        controlling_area: str,
        select: Optional[List[str]] = None,
        filter: Optional[str] = None,
        top: int = 100
    ) -> GraphResponse:
        """
        Busca Cost Centers

        Args:
            controlling_area: Area de controlling
            select: Campos
            filter: Filtro
            top: Limite

        Returns:
            GraphResponse com Cost Centers
        """
        options = GraphQueryOptions(
            select=select or [],
            filter=f"ControllingArea eq '{controlling_area}'" + (f" and {filter}" if filter else ""),
            top=top
        )

        return await self._request(
            "GET",
            "CostCenter",
            params=options.to_params()
        )

    # =====================================
    # Generic Entity Access
    # =====================================

    async def get_entity(
        self,
        entity_type: Union[GraphEntityType, str],
        key: str,
        select: Optional[List[str]] = None,
        expand: Optional[List[str]] = None
    ) -> GraphResponse:
        """
        Busca entidade generica por tipo e chave

        Args:
            entity_type: Tipo da entidade
            key: Chave primaria
            select: Campos
            expand: Associacoes

        Returns:
            GraphResponse com a entidade
        """
        # Extrair nome da entidade
        if isinstance(entity_type, GraphEntityType):
            entity_name = entity_type.value.split("/")[-1]
        else:
            entity_name = entity_type.split("/")[-1] if "/" in entity_type else entity_type

        params = {}
        if select:
            params["$select"] = ",".join(select)
        if expand:
            params["$expand"] = ",".join(expand)

        return await self._request(
            "GET",
            f"{entity_name}('{key}')",
            params=params if params else None
        )

    async def query_entity(
        self,
        entity_type: Union[GraphEntityType, str],
        options: GraphQueryOptions
    ) -> GraphResponse:
        """
        Query generico em entidade

        Args:
            entity_type: Tipo da entidade
            options: Opcoes de query

        Returns:
            GraphResponse
        """
        if isinstance(entity_type, GraphEntityType):
            entity_name = entity_type.value.split("/")[-1]
        else:
            entity_name = entity_type.split("/")[-1] if "/" in entity_type else entity_type

        return await self._request(
            "GET",
            entity_name,
            params=options.to_params()
        )

    # =====================================
    # Schema / Metadata
    # =====================================

    async def get_schema(self) -> GraphResponse:
        """
        Busca schema do SAP Graph

        Returns:
            GraphResponse com metadados
        """
        return await self._request("GET", "$metadata")

    async def get_available_entities(self) -> List[str]:
        """
        Lista entidades disponiveis

        Returns:
            Lista de nomes de entidades
        """
        # Entidades conhecidas do SAP Graph
        return [
            "BusinessPartner",
            "Customer",
            "Supplier",
            "Product",
            "SalesOrder",
            "SalesOrderItem",
            "PurchaseOrder",
            "PurchaseOrderItem",
            "Employee",
            "CostCenter",
            "ProfitCenter",
            "Project",
            "WorkPackage",
            "Material",
            "Plant"
        ]

    # =====================================
    # Cross-Entity Queries
    # =====================================

    async def get_customer_with_orders(
        self,
        customer_id: str,
        order_limit: int = 10
    ) -> Dict:
        """
        Busca cliente com seus pedidos

        Args:
            customer_id: ID do cliente
            order_limit: Limite de pedidos

        Returns:
            Dict com dados do cliente e pedidos
        """
        # Buscar cliente
        customer_resp = await self.get_customer(customer_id)

        # Buscar pedidos do cliente
        orders_resp = await self.get_sales_orders(
            filter=f"SoldToParty eq '{customer_id}'",
            top=order_limit
        )

        return {
            "customer": customer_resp.data,
            "sales_orders": orders_resp.items
        }

    async def get_supplier_with_orders(
        self,
        supplier_id: str,
        order_limit: int = 10
    ) -> Dict:
        """
        Busca fornecedor com pedidos de compra

        Args:
            supplier_id: ID do fornecedor
            order_limit: Limite de pedidos

        Returns:
            Dict com dados do fornecedor e pedidos
        """
        # Buscar fornecedor
        supplier_resp = await self.get_supplier(supplier_id)

        # Buscar pedidos de compra
        orders_resp = await self.get_purchase_orders(
            filter=f"Supplier eq '{supplier_id}'",
            top=order_limit
        )

        return {
            "supplier": supplier_resp.data,
            "purchase_orders": orders_resp.items
        }

    async def search_across_entities(
        self,
        search_term: str,
        entity_types: Optional[List[str]] = None,
        limit_per_entity: int = 5
    ) -> Dict[str, List[Dict]]:
        """
        Busca termo em multiplas entidades

        Args:
            search_term: Termo de busca
            entity_types: Tipos de entidade (default: todas principais)
            limit_per_entity: Limite por entidade

        Returns:
            Dict com resultados por entidade
        """
        if entity_types is None:
            entity_types = [
                "BusinessPartner",
                "Product",
                "SalesOrder",
                "PurchaseOrder"
            ]

        results = {}

        for entity_type in entity_types:
            try:
                options = GraphQueryOptions(
                    search=search_term,
                    top=limit_per_entity
                )
                response = await self.query_entity(entity_type, options)
                results[entity_type] = response.items
            except GraphError as e:
                logger.warning(f"Erro ao buscar em {entity_type}: {e}")
                results[entity_type] = []

        return results
