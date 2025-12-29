# -*- coding: utf-8 -*-
"""
SAP Read Skill
==============
Skill para leitura de dados SAP.

Esta skill fornece capacidades de leitura para:
- Dados mestres (materiais, clientes, fornecedores)
- Documentos (pedidos, faturas, entregas)
- Tabelas SAP
- Configuracoes

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.skills import SAPReadSkill

skill = SAPReadSkill(rfc_client)

# Buscar material
material = await skill.get_material("MATERIAL001")

# Buscar pedidos de venda
orders = await skill.get_sales_orders(
    sales_org="1000",
    date_from="2024-01-01"
)
```
"""

import logging
from dataclasses import dataclass
from datetime import datetime, date
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de execucao de skill"""
    success: bool
    data: Any = None
    message: str = ""
    error: Optional[str] = None
    execution_time_ms: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "error": self.error,
            "execution_time_ms": self.execution_time_ms
        }


class SAPReadSkill:
    """
    Skill de leitura de dados SAP.

    Fornece metodos de alto nivel para leitura de:
    - Materiais
    - Clientes/Fornecedores
    - Pedidos de venda/compra
    - Entregas
    - Faturas
    - Dados financeiros
    """

    def __init__(self, rfc_client):
        """
        Inicializa a skill.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client

    # =========================================================================
    # Dados Mestres - Materiais
    # =========================================================================

    async def get_material(
        self,
        material_number: str,
        plant: Optional[str] = None
    ) -> SkillResult:
        """
        Busca dados de um material.

        Args:
            material_number: Numero do material
            plant: Centro (opcional)

        Returns:
            SkillResult com dados do material
        """
        start_time = datetime.now()

        try:
            # Buscar BAPI de material
            params = {"MATERIAL": material_number.upper()}

            if plant:
                params["PLANT"] = plant

            result = await self.rfc_client.call_function(
                "BAPI_MATERIAL_GET_DETAIL",
                params
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            # Montar dados do material
            material_data = {
                "general": result.data.get("MATERIAL_GENERAL_DATA", {}),
                "plant": result.data.get("MATERIAL_PLANT_DATA", {}),
                "valuation": result.data.get("MATERIAL_VALUATION_DATA", {}),
                "description": result.data.get("MATERIAL_DESCRIPTION", {})
            }

            return SkillResult(
                success=True,
                data=material_data,
                message=f"Material {material_number} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar material {material_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def search_materials(
        self,
        material_type: Optional[str] = None,
        description: Optional[str] = None,
        plant: Optional[str] = None,
        max_results: int = 100
    ) -> SkillResult:
        """
        Pesquisa materiais.

        Args:
            material_type: Tipo de material (FERT, HALB, ROH, etc.)
            description: Texto na descricao
            plant: Centro
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de materiais
        """
        start_time = datetime.now()

        try:
            # Montar filtros
            options = []
            if material_type:
                options.append({"TEXT": f"MTART = '{material_type}'"})
            if description:
                options.append({"TEXT": f"AND MAKTX LIKE '%{description}%'" if options else f"MAKTX LIKE '%{description}%'"})

            # Buscar materiais via RFC_READ_TABLE
            materials = await self.rfc_client.read_table(
                "MARA",
                fields=["MATNR", "MTART", "MATKL", "MEINS", "BRGEW", "NTGEW", "GEWEI"],
                options=options if options else None,
                max_rows=max_results
            )

            # Buscar descricoes
            if materials:
                matnr_list = [m["MATNR"] for m in materials]
                matnr_str = "', '".join(matnr_list)

                descriptions = await self.rfc_client.read_table(
                    "MAKT",
                    fields=["MATNR", "SPRAS", "MAKTX"],
                    options=[{"TEXT": f"MATNR IN ('{matnr_str}') AND SPRAS = 'P'"}],
                    max_rows=max_results
                )

                desc_map = {d["MATNR"]: d.get("MAKTX", "") for d in descriptions}

                for mat in materials:
                    mat["MAKTX"] = desc_map.get(mat["MATNR"], "")

            return SkillResult(
                success=True,
                data=materials,
                message=f"Encontrados {len(materials)} materiais",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao pesquisar materiais: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Dados Mestres - Parceiros
    # =========================================================================

    async def get_customer(
        self,
        customer_number: str,
        sales_org: Optional[str] = None
    ) -> SkillResult:
        """
        Busca dados de um cliente.

        Args:
            customer_number: Numero do cliente
            sales_org: Organizacao de vendas

        Returns:
            SkillResult com dados do cliente
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_CUSTOMER_GETDETAIL2",
                {"CUSTOMERNO": customer_number.zfill(10)}
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            customer_data = {
                "general": result.data.get("CUSTOMERADDRESS", {}),
                "company": result.data.get("COMPANYDATA", {}),
                "sales": result.data.get("SALESDATA", {})
            }

            return SkillResult(
                success=True,
                data=customer_data,
                message=f"Cliente {customer_number} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar cliente {customer_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_vendor(self, vendor_number: str) -> SkillResult:
        """
        Busca dados de um fornecedor.

        Args:
            vendor_number: Numero do fornecedor

        Returns:
            SkillResult com dados do fornecedor
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_VENDOR_GETDETAIL",
                {"VENDORNO": vendor_number.zfill(10)}
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            vendor_data = {
                "general": result.data.get("GENERALDATA", {}),
                "company": result.data.get("COMPANYDATA", {}),
                "purchasing": result.data.get("PURCHASEDATA", {})
            }

            return SkillResult(
                success=True,
                data=vendor_data,
                message=f"Fornecedor {vendor_number} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar fornecedor {vendor_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Documentos - Vendas
    # =========================================================================

    async def get_sales_order(self, order_number: str) -> SkillResult:
        """
        Busca dados de um pedido de venda.

        Args:
            order_number: Numero do pedido

        Returns:
            SkillResult com dados do pedido
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_SALESORDER_GETLIST",
                {"SALESDOCUMENT": order_number}
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            order_data = {
                "header": result.data.get("ORDER_HEADER_OUT", {}),
                "items": result.tables.get("ORDER_ITEMS_OUT", []),
                "partners": result.tables.get("ORDER_PARTNERS_OUT", []),
                "conditions": result.tables.get("ORDER_CONDITIONS_OUT", [])
            }

            return SkillResult(
                success=True,
                data=order_data,
                message=f"Pedido {order_number} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar pedido {order_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_sales_orders(
        self,
        sales_org: Optional[str] = None,
        customer: Optional[str] = None,
        date_from: Optional[str] = None,
        date_to: Optional[str] = None,
        max_results: int = 100
    ) -> SkillResult:
        """
        Lista pedidos de venda.

        Args:
            sales_org: Organizacao de vendas
            customer: Cliente
            date_from: Data inicial (YYYY-MM-DD)
            date_to: Data final (YYYY-MM-DD)
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de pedidos
        """
        start_time = datetime.now()

        try:
            # Montar filtros
            options = []
            if sales_org:
                options.append({"TEXT": f"VKORG = '{sales_org}'"})
            if customer:
                options.append({"TEXT": f"AND KUNNR = '{customer.zfill(10)}'" if options else f"KUNNR = '{customer.zfill(10)}'"})
            if date_from:
                sap_date = date_from.replace("-", "")
                options.append({"TEXT": f"AND AUDAT >= '{sap_date}'" if options else f"AUDAT >= '{sap_date}'"})
            if date_to:
                sap_date = date_to.replace("-", "")
                options.append({"TEXT": f"AND AUDAT <= '{sap_date}'"})

            orders = await self.rfc_client.read_table(
                "VBAK",
                fields=["VBELN", "AUDAT", "VKORG", "KUNNR", "NETWR", "WAERK", "AUART"],
                options=options if options else None,
                max_rows=max_results
            )

            return SkillResult(
                success=True,
                data=orders,
                message=f"Encontrados {len(orders)} pedidos",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao listar pedidos de venda: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Documentos - Compras
    # =========================================================================

    async def get_purchase_order(self, po_number: str) -> SkillResult:
        """
        Busca dados de um pedido de compra.

        Args:
            po_number: Numero do pedido de compra

        Returns:
            SkillResult com dados do pedido
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_PO_GETDETAIL1",
                {"PURCHASEORDER": po_number}
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            po_data = {
                "header": result.data.get("POHEADER", {}),
                "items": result.tables.get("POITEM", []),
                "schedules": result.tables.get("POSCHEDULE", []),
                "conditions": result.tables.get("POCOND", [])
            }

            return SkillResult(
                success=True,
                data=po_data,
                message=f"Pedido de compra {po_number} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar PC {po_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Documentos - Financeiro
    # =========================================================================

    async def get_gl_account(
        self,
        account_number: str,
        company_code: str
    ) -> SkillResult:
        """
        Busca dados de uma conta contabil.

        Args:
            account_number: Numero da conta
            company_code: Empresa

        Returns:
            SkillResult com dados da conta
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_GL_ACC_GETDETAIL",
                {
                    "COMPANYCODE": company_code,
                    "GLACCT": account_number.zfill(10)
                }
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            account_data = {
                "general": result.data.get("ACCOUNT_DATA", {}),
                "company": result.data.get("COMPANY_DATA", {}),
                "description": result.data.get("ACCOUNT_NAME", "")
            }

            return SkillResult(
                success=True,
                data=account_data,
                message=f"Conta {account_number} encontrada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar conta {account_number}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_cost_center(
        self,
        cost_center: str,
        controlling_area: str
    ) -> SkillResult:
        """
        Busca dados de um centro de custo.

        Args:
            cost_center: Centro de custo
            controlling_area: Area de controladoria

        Returns:
            SkillResult com dados do centro de custo
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_function(
                "BAPI_COSTCENTER_GETDETAIL1",
                {
                    "CONTROLLINGAREA": controlling_area,
                    "COSTCENTER": cost_center.zfill(10)
                }
            )

            if not result.success:
                return SkillResult(
                    success=False,
                    error=result.error,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            return SkillResult(
                success=True,
                data=result.data.get("COSTCENTERDETAIL", {}),
                message=f"Centro de custo {cost_center} encontrado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar centro de custo {cost_center}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Utilitarios
    # =========================================================================

    async def read_table(
        self,
        table_name: str,
        fields: Optional[List[str]] = None,
        where: Optional[str] = None,
        max_rows: int = 100
    ) -> SkillResult:
        """
        Le dados de uma tabela SAP.

        Args:
            table_name: Nome da tabela
            fields: Campos a retornar
            where: Clausula WHERE
            max_rows: Maximo de linhas

        Returns:
            SkillResult com dados da tabela
        """
        start_time = datetime.now()

        try:
            options = None
            if where:
                options = [{"TEXT": where}]

            data = await self.rfc_client.read_table(
                table_name.upper(),
                fields=fields,
                options=options,
                max_rows=max_rows
            )

            return SkillResult(
                success=True,
                data=data,
                message=f"Lidas {len(data)} linhas de {table_name}",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao ler tabela {table_name}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def call_bapi(
        self,
        bapi_name: str,
        parameters: Dict[str, Any]
    ) -> SkillResult:
        """
        Executa uma BAPI generica.

        Args:
            bapi_name: Nome da BAPI
            parameters: Parametros

        Returns:
            SkillResult com resultado da BAPI
        """
        start_time = datetime.now()

        try:
            result = await self.rfc_client.call_bapi(
                bapi_name.upper(),
                parameters,
                auto_commit=False  # Nao comitar - apenas leitura
            )

            if result.has_errors():
                return SkillResult(
                    success=False,
                    error="; ".join(result.get_error_messages()),
                    data=result.data,
                    execution_time_ms=self._elapsed_ms(start_time)
                )

            return SkillResult(
                success=True,
                data={
                    "export": result.data,
                    "tables": result.tables
                },
                message=f"BAPI {bapi_name} executada com sucesso",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao executar BAPI {bapi_name}: {e}")
            return SkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    def _elapsed_ms(self, start_time: datetime) -> float:
        """Calcula tempo decorrido em milissegundos"""
        return (datetime.now() - start_time).total_seconds() * 1000
