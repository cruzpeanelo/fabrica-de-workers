# -*- coding: utf-8 -*-
"""
S4 Read Skill
=============
Skill para leitura de dados do SAP S/4HANA.

Esta skill fornece capacidades de leitura de dados mestres
e transacionais do S/4HANA via OData.

Autor: Fabrica de Agentes
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class S4EntityType(str, Enum):
    """Tipos de entidade S/4HANA"""
    BUSINESS_PARTNER = "BusinessPartner"
    CUSTOMER = "Customer"
    SUPPLIER = "Supplier"
    PRODUCT = "Product"
    SALES_ORDER = "SalesOrder"
    PURCHASE_ORDER = "PurchaseOrder"
    JOURNAL_ENTRY = "JournalEntry"
    COST_CENTER = "CostCenter"
    PROFIT_CENTER = "ProfitCenter"
    EMPLOYEE = "Employee"
    MATERIAL_DOCUMENT = "MaterialDocument"


@dataclass
class S4ReadResult:
    """Resultado de uma operacao de leitura"""
    success: bool
    data: List[Dict] = field(default_factory=list)
    count: int = 0
    message: str = ""
    entity_type: Optional[str] = None
    next_link: Optional[str] = None


class S4ReadSkill:
    """
    Skill de leitura de dados SAP S/4HANA

    Fornece metodos de alto nivel para leitura de dados mestres
    e transacionais do S/4HANA.

    Exemplo de uso:
    ```python
    skill = S4ReadSkill(odata_client, graph_client, tenant_id="tenant-001")

    # Ler Business Partners do Brasil
    result = await skill.read_business_partners(
        country="BR",
        category="1",
        limit=100
    )

    # Ler ordens de venda de um cliente
    result = await skill.read_sales_orders(
        customer="CUST001",
        date_from="2024-01-01"
    )

    # Pesquisa generica
    result = await skill.search(
        term="Material XYZ",
        entity_types=[S4EntityType.PRODUCT, S4EntityType.MATERIAL_DOCUMENT]
    )
    ```
    """

    def __init__(self, odata_client=None, graph_client=None, tenant_id: str = ""):
        """
        Inicializa skill

        Args:
            odata_client: Cliente OData V4
            graph_client: Cliente SAP Graph (opcional)
            tenant_id: ID do tenant para isolamento multi-tenant
        """
        self.odata_client = odata_client
        self.graph_client = graph_client
        self.tenant_id = tenant_id

        if not tenant_id:
            logger.warning("tenant_id nao configurado para S4ReadSkill")

    async def read_business_partners(
        self,
        country: Optional[str] = None,
        category: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100,
        offset: int = 0,
        fields: Optional[List[str]] = None
    ) -> S4ReadResult:
        """
        Le Business Partners do S/4HANA

        Args:
            country: Codigo do pais (BR, US, etc)
            category: Categoria (1=Organizacao, 2=Pessoa)
            search: Termo de busca
            limit: Limite de registros
            offset: Offset para paginacao
            fields: Campos especificos para retornar

        Returns:
            S4ReadResult com Business Partners
        """
        try:
            # Construir filtros
            filters = []
            if country:
                filters.append(f"Country eq '{country}'")
            if category:
                filters.append(f"BusinessPartnerCategory eq '{category}'")

            filter_expr = " and ".join(filters) if filters else None

            # Campos padrao
            if not fields:
                fields = [
                    "BusinessPartner",
                    "BusinessPartnerFullName",
                    "BusinessPartnerCategory",
                    "Country",
                    "Region",
                    "CityName",
                    "PostalCode"
                ]

            # Preferir Graph se disponivel
            if self.graph_client:
                response = await self.graph_client.get_business_partners(
                    filter=filter_expr,
                    search=search,
                    top=limit,
                    skip=offset
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_BusinessPartner",
                    select=fields,
                    filter_expr=filter_expr,
                    top=limit,
                    skip=offset
                )
            else:
                return S4ReadResult(
                    success=False,
                    message="Nenhum cliente configurado (OData ou Graph)"
                )

            return S4ReadResult(
                success=True,
                data=response.items,
                count=response.count or len(response.items),
                entity_type=S4EntityType.BUSINESS_PARTNER.value,
                next_link=response.next_link if hasattr(response, 'next_link') else None
            )

        except Exception as e:
            logger.error(f"Erro ao ler Business Partners: {e}")
            return S4ReadResult(
                success=False,
                message=str(e),
                entity_type=S4EntityType.BUSINESS_PARTNER.value
            )

    async def read_customers(
        self,
        country: Optional[str] = None,
        sales_organization: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ) -> S4ReadResult:
        """
        Le Customers do S/4HANA

        Args:
            country: Codigo do pais
            sales_organization: Organizacao de vendas
            search: Termo de busca
            limit: Limite

        Returns:
            S4ReadResult com Customers
        """
        try:
            filters = []
            if country:
                filters.append(f"Country eq '{country}'")
            if sales_organization:
                filters.append(f"SalesOrganization eq '{sales_organization}'")

            filter_expr = " and ".join(filters) if filters else None

            if self.graph_client:
                response = await self.graph_client.get_customers(
                    filter=filter_expr,
                    search=search,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_Customer",
                    filter_expr=filter_expr,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.CUSTOMER.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Customers: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def read_suppliers(
        self,
        country: Optional[str] = None,
        purchasing_organization: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ) -> S4ReadResult:
        """
        Le Suppliers do S/4HANA

        Args:
            country: Codigo do pais
            purchasing_organization: Organizacao de compras
            search: Termo de busca
            limit: Limite

        Returns:
            S4ReadResult com Suppliers
        """
        try:
            filters = []
            if country:
                filters.append(f"Country eq '{country}'")
            if purchasing_organization:
                filters.append(f"PurchasingOrganization eq '{purchasing_organization}'")

            filter_expr = " and ".join(filters) if filters else None

            if self.graph_client:
                response = await self.graph_client.get_suppliers(
                    filter=filter_expr,
                    search=search,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_Supplier",
                    filter_expr=filter_expr,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.SUPPLIER.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Suppliers: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def read_products(
        self,
        product_type: Optional[str] = None,
        material_group: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ) -> S4ReadResult:
        """
        Le Products/Materials do S/4HANA

        Args:
            product_type: Tipo de produto
            material_group: Grupo de material
            search: Termo de busca
            limit: Limite

        Returns:
            S4ReadResult com Products
        """
        try:
            filters = []
            if product_type:
                filters.append(f"ProductType eq '{product_type}'")
            if material_group:
                filters.append(f"MaterialGroup eq '{material_group}'")

            filter_expr = " and ".join(filters) if filters else None

            if self.graph_client:
                response = await self.graph_client.get_products(
                    filter=filter_expr,
                    search=search,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_Product",
                    filter_expr=filter_expr,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.PRODUCT.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Products: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def read_sales_orders(
        self,
        customer: Optional[str] = None,
        sales_organization: Optional[str] = None,
        date_from: Optional[str] = None,
        date_to: Optional[str] = None,
        status: Optional[str] = None,
        limit: int = 100,
        include_items: bool = False
    ) -> S4ReadResult:
        """
        Le Sales Orders do S/4HANA

        Args:
            customer: Codigo do cliente
            sales_organization: Organizacao de vendas
            date_from: Data inicial (YYYY-MM-DD)
            date_to: Data final (YYYY-MM-DD)
            status: Status do documento
            limit: Limite
            include_items: Incluir itens

        Returns:
            S4ReadResult com Sales Orders
        """
        try:
            filters = []
            if customer:
                filters.append(f"SoldToParty eq '{customer}'")
            if sales_organization:
                filters.append(f"SalesOrganization eq '{sales_organization}'")
            if date_from:
                filters.append(f"SalesOrderDate ge {date_from}")
            if date_to:
                filters.append(f"SalesOrderDate le {date_to}")

            filter_expr = " and ".join(filters) if filters else None

            expand = ["to_Item"] if include_items else None

            if self.graph_client:
                response = await self.graph_client.get_sales_orders(
                    filter=filter_expr,
                    expand=expand,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_SalesOrder",
                    filter_expr=filter_expr,
                    expand=expand,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.SALES_ORDER.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Sales Orders: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def read_purchase_orders(
        self,
        supplier: Optional[str] = None,
        purchasing_organization: Optional[str] = None,
        date_from: Optional[str] = None,
        date_to: Optional[str] = None,
        limit: int = 100,
        include_items: bool = False
    ) -> S4ReadResult:
        """
        Le Purchase Orders do S/4HANA

        Args:
            supplier: Codigo do fornecedor
            purchasing_organization: Organizacao de compras
            date_from: Data inicial
            date_to: Data final
            limit: Limite
            include_items: Incluir itens

        Returns:
            S4ReadResult com Purchase Orders
        """
        try:
            filters = []
            if supplier:
                filters.append(f"Supplier eq '{supplier}'")
            if purchasing_organization:
                filters.append(f"PurchasingOrganization eq '{purchasing_organization}'")
            if date_from:
                filters.append(f"PurchaseOrderDate ge {date_from}")
            if date_to:
                filters.append(f"PurchaseOrderDate le {date_to}")

            filter_expr = " and ".join(filters) if filters else None
            expand = ["to_PurchaseOrderItem"] if include_items else None

            if self.graph_client:
                response = await self.graph_client.get_purchase_orders(
                    filter=filter_expr,
                    expand=expand,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_PurchaseOrder",
                    filter_expr=filter_expr,
                    expand=expand,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.PURCHASE_ORDER.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Purchase Orders: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def read_cost_centers(
        self,
        controlling_area: str,
        company_code: Optional[str] = None,
        limit: int = 100
    ) -> S4ReadResult:
        """
        Le Cost Centers do S/4HANA

        Args:
            controlling_area: Area de controlling
            company_code: Codigo da empresa
            limit: Limite

        Returns:
            S4ReadResult com Cost Centers
        """
        try:
            filters = [f"ControllingArea eq '{controlling_area}'"]
            if company_code:
                filters.append(f"CompanyCode eq '{company_code}'")

            filter_expr = " and ".join(filters)

            if self.graph_client:
                response = await self.graph_client.get_cost_centers(
                    controlling_area=controlling_area,
                    filter=filter_expr if company_code else None,
                    top=limit
                )
            elif self.odata_client:
                response = await self.odata_client.get(
                    "A_CostCenter",
                    filter_expr=filter_expr,
                    top=limit
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

            return S4ReadResult(
                success=True,
                data=response.items,
                count=len(response.items),
                entity_type=S4EntityType.COST_CENTER.value
            )

        except Exception as e:
            logger.error(f"Erro ao ler Cost Centers: {e}")
            return S4ReadResult(success=False, message=str(e))

    async def search(
        self,
        term: str,
        entity_types: Optional[List[S4EntityType]] = None,
        limit_per_entity: int = 10
    ) -> Dict[str, S4ReadResult]:
        """
        Pesquisa termo em multiplas entidades

        Args:
            term: Termo de busca
            entity_types: Tipos de entidade para buscar
            limit_per_entity: Limite por entidade

        Returns:
            Dict com resultados por tipo de entidade
        """
        if entity_types is None:
            entity_types = [
                S4EntityType.BUSINESS_PARTNER,
                S4EntityType.PRODUCT,
                S4EntityType.SALES_ORDER
            ]

        results = {}

        for entity_type in entity_types:
            try:
                if entity_type == S4EntityType.BUSINESS_PARTNER:
                    result = await self.read_business_partners(
                        search=term,
                        limit=limit_per_entity
                    )
                elif entity_type == S4EntityType.PRODUCT:
                    result = await self.read_products(
                        search=term,
                        limit=limit_per_entity
                    )
                elif entity_type == S4EntityType.SALES_ORDER:
                    result = await self.read_sales_orders(
                        limit=limit_per_entity
                    )
                elif entity_type == S4EntityType.PURCHASE_ORDER:
                    result = await self.read_purchase_orders(
                        limit=limit_per_entity
                    )
                elif entity_type == S4EntityType.CUSTOMER:
                    result = await self.read_customers(
                        search=term,
                        limit=limit_per_entity
                    )
                elif entity_type == S4EntityType.SUPPLIER:
                    result = await self.read_suppliers(
                        search=term,
                        limit=limit_per_entity
                    )
                else:
                    result = S4ReadResult(
                        success=False,
                        message=f"Tipo {entity_type.value} nao suportado para busca"
                    )

                results[entity_type.value] = result

            except Exception as e:
                logger.warning(f"Erro ao buscar em {entity_type.value}: {e}")
                results[entity_type.value] = S4ReadResult(
                    success=False,
                    message=str(e)
                )

        return results

    async def get_entity_by_key(
        self,
        entity_type: S4EntityType,
        key: str,
        expand: Optional[List[str]] = None
    ) -> S4ReadResult:
        """
        Busca entidade especifica por chave

        Args:
            entity_type: Tipo de entidade
            key: Chave primaria
            expand: Associacoes para expandir

        Returns:
            S4ReadResult com a entidade
        """
        entity_mapping = {
            S4EntityType.BUSINESS_PARTNER: "A_BusinessPartner",
            S4EntityType.CUSTOMER: "A_Customer",
            S4EntityType.SUPPLIER: "A_Supplier",
            S4EntityType.PRODUCT: "A_Product",
            S4EntityType.SALES_ORDER: "A_SalesOrder",
            S4EntityType.PURCHASE_ORDER: "A_PurchaseOrder"
        }

        entity_set = entity_mapping.get(entity_type)
        if not entity_set:
            return S4ReadResult(
                success=False,
                message=f"Tipo {entity_type.value} nao mapeado"
            )

        try:
            if self.odata_client:
                response = await self.odata_client.get(
                    entity_set,
                    key=f"'{key}'",
                    expand=expand
                )

                return S4ReadResult(
                    success=True,
                    data=[response.first] if response.first else [],
                    count=1 if response.first else 0,
                    entity_type=entity_type.value
                )
            else:
                return S4ReadResult(success=False, message="Cliente nao configurado")

        except Exception as e:
            logger.error(f"Erro ao buscar {entity_type.value} {key}: {e}")
            return S4ReadResult(success=False, message=str(e))
