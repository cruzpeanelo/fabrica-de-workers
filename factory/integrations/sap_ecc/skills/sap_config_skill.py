# -*- coding: utf-8 -*-
"""
SAP Config Skill
================
Skill para analise e documentacao de configuracoes SAP.

Esta skill fornece capacidades de:
- Analise de customizing IMG
- Documentacao de configuracoes
- Comparacao de ambientes
- Sugestoes de configuracao

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.skills import SAPConfigSkill

skill = SAPConfigSkill(rfc_client)

# Analisar configuracao de precos
pricing = await skill.analyze_pricing_config("RVAA01")

# Documentar configuracoes de organizacao de vendas
doc = await skill.document_sales_org_config("1000")
```
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional

from ..analyzers import ConfigAnalyzer, TableAnalyzer

logger = logging.getLogger(__name__)


@dataclass
class ConfigSkillResult:
    """Resultado de execucao de skill de configuracao"""
    success: bool
    config: Optional[Dict[str, Any]] = None
    documentation: str = ""
    message: str = ""
    error: Optional[str] = None
    execution_time_ms: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "config": self.config,
            "documentation": self.documentation,
            "message": self.message,
            "error": self.error,
            "execution_time_ms": self.execution_time_ms
        }


class SAPConfigSkill:
    """
    Skill de analise de configuracoes SAP.

    Combina analise de tabelas de customizing com
    geracao de documentacao estruturada.
    """

    def __init__(self, rfc_client):
        """
        Inicializa a skill.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client
        self.config_analyzer = ConfigAnalyzer(rfc_client)
        self.table_analyzer = TableAnalyzer(rfc_client)

    # =========================================================================
    # Estrutura Organizacional
    # =========================================================================

    async def get_company_codes(self) -> ConfigSkillResult:
        """
        Lista empresas (company codes).

        Returns:
            ConfigSkillResult com empresas
        """
        start_time = datetime.now()

        try:
            companies = await self.rfc_client.read_table(
                "T001",
                fields=["BUKRS", "BUTXT", "WAERS", "LAND1", "SPRAS"],
                max_rows=100
            )

            return ConfigSkillResult(
                success=True,
                config={"companies": companies, "count": len(companies)},
                message=f"Encontradas {len(companies)} empresas",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar empresas: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_sales_organizations(self) -> ConfigSkillResult:
        """
        Lista organizacoes de vendas.

        Returns:
            ConfigSkillResult com org vendas
        """
        start_time = datetime.now()

        try:
            # Buscar organizacoes
            orgs = await self.rfc_client.read_table(
                "TVKO",
                fields=["VKORG", "BUKRS", "WAESSION"],
                max_rows=100
            )

            # Buscar textos
            org_ids = [o["VKORG"] for o in orgs]
            texts = {}

            if org_ids:
                org_str = "', '".join(org_ids)
                text_data = await self.rfc_client.read_table(
                    "TVKOT",
                    fields=["VKORG", "VTEXT"],
                    options=[{"TEXT": f"VKORG IN ('{org_str}') AND SPRAS = 'P'"}],
                    max_rows=100
                )
                texts = {t["VKORG"]: t.get("VTEXT", "") for t in text_data}

            for org in orgs:
                org["VTEXT"] = texts.get(org["VKORG"], "")

            return ConfigSkillResult(
                success=True,
                config={"sales_orgs": orgs, "count": len(orgs)},
                message=f"Encontradas {len(orgs)} organizacoes de vendas",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar org vendas: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_plants(self) -> ConfigSkillResult:
        """
        Lista centros (plantas).

        Returns:
            ConfigSkillResult com centros
        """
        start_time = datetime.now()

        try:
            plants = await self.rfc_client.read_table(
                "T001W",
                fields=["WERKS", "NAME1", "LAND1", "REESSION", "BUKRS"],
                max_rows=100
            )

            return ConfigSkillResult(
                success=True,
                config={"plants": plants, "count": len(plants)},
                message=f"Encontrados {len(plants)} centros",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar centros: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_purchasing_organizations(self) -> ConfigSkillResult:
        """
        Lista organizacoes de compras.

        Returns:
            ConfigSkillResult com org compras
        """
        start_time = datetime.now()

        try:
            orgs = await self.rfc_client.read_table(
                "T024E",
                fields=["EKORG", "EKOTX", "BUKRS"],
                max_rows=100
            )

            return ConfigSkillResult(
                success=True,
                config={"purchasing_orgs": orgs, "count": len(orgs)},
                message=f"Encontradas {len(orgs)} organizacoes de compras",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar org compras: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Configuracoes SD
    # =========================================================================

    async def analyze_pricing_config(
        self,
        pricing_procedure: str
    ) -> ConfigSkillResult:
        """
        Analisa configuracao de procedimento de precos.

        Args:
            pricing_procedure: Nome do procedimento

        Returns:
            ConfigSkillResult com analise
        """
        start_time = datetime.now()

        try:
            config = await self.config_analyzer.get_pricing_config(pricing_procedure)

            # Gerar documentacao
            doc = self._generate_pricing_documentation(pricing_procedure, config)

            return ConfigSkillResult(
                success=True,
                config=config,
                documentation=doc,
                message=f"Procedimento {pricing_procedure} analisado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao analisar pricing: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    def _generate_pricing_documentation(
        self,
        procedure: str,
        config: Dict[str, Any]
    ) -> str:
        """Gera documentacao de pricing"""
        lines = [
            f"# Procedimento de Precos: {procedure}",
            "",
            "## Tipos de Condicao",
            "",
            "| Step | Tipo | Descricao |",
            "|------|------|-----------|"
        ]

        for ct in config.get("condition_types", []):
            lines.append(f"| {ct.get('step', '')} | {ct.get('condition_type', '')} | |")

        lines.extend([
            "",
            "## Sequencias de Acesso",
            ""
        ])

        for seq in config.get("access_sequences", []):
            lines.append(f"- {seq}")

        return "\n".join(lines)

    async def get_document_types(
        self,
        area: str = "SD"
    ) -> ConfigSkillResult:
        """
        Lista tipos de documento.

        Args:
            area: Area (SD, MM, FI)

        Returns:
            ConfigSkillResult com tipos
        """
        start_time = datetime.now()

        try:
            if area == "SD":
                # Tipos de pedido de venda
                types = await self.rfc_client.read_table(
                    "TVAK",
                    fields=["AUART", "BEZEI"],
                    max_rows=100
                )
                doc_type = "pedido de venda"

            elif area == "MM":
                # Tipos de pedido de compra
                types = await self.rfc_client.read_table(
                    "T161",
                    fields=["BSART", "BSTYP"],
                    max_rows=100
                )
                doc_type = "pedido de compra"

            elif area == "FI":
                # Tipos de documento contabil
                types = await self.rfc_client.read_table(
                    "T003",
                    fields=["BLART", "BLTEXT"],
                    max_rows=100
                )
                doc_type = "documento contabil"

            else:
                return ConfigSkillResult(
                    success=False,
                    error=f"Area {area} nao suportada"
                )

            return ConfigSkillResult(
                success=True,
                config={"document_types": types, "area": area},
                message=f"Encontrados {len(types)} tipos de {doc_type}",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar tipos de documento: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Configuracoes MM
    # =========================================================================

    async def get_material_types(self) -> ConfigSkillResult:
        """
        Lista tipos de material.

        Returns:
            ConfigSkillResult com tipos
        """
        start_time = datetime.now()

        try:
            types = await self.rfc_client.read_table(
                "T134",
                fields=["MTART", "MTBEZ", "MTREF", "BESSION", "PSTAT"],
                max_rows=100
            )

            # Buscar textos
            type_ids = [t["MTART"] for t in types]
            texts = {}

            if type_ids:
                type_str = "', '".join(type_ids)
                text_data = await self.rfc_client.read_table(
                    "T134T",
                    fields=["MTART", "MTBEZ"],
                    options=[{"TEXT": f"MTART IN ('{type_str}') AND SPRAS = 'P'"}],
                    max_rows=100
                )
                texts = {t["MTART"]: t.get("MTBEZ", "") for t in text_data}

            for mt in types:
                mt["DESCRIPTION"] = texts.get(mt["MTART"], mt.get("MTBEZ", ""))

            return ConfigSkillResult(
                success=True,
                config={"material_types": types, "count": len(types)},
                message=f"Encontrados {len(types)} tipos de material",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar tipos de material: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_movement_types(self) -> ConfigSkillResult:
        """
        Lista tipos de movimento.

        Returns:
            ConfigSkillResult com movimentos
        """
        start_time = datetime.now()

        try:
            movements = await self.rfc_client.read_table(
                "T156",
                fields=["BWART", "XAESSION", "XAKL", "XKOST", "XVBR"],
                max_rows=200
            )

            # Buscar textos
            mov_ids = [m["BWART"] for m in movements]
            texts = {}

            if mov_ids:
                mov_str = "', '".join(mov_ids)
                text_data = await self.rfc_client.read_table(
                    "T156T",
                    fields=["BWART", "BTEXT"],
                    options=[{"TEXT": f"BWART IN ('{mov_str}') AND SPRAS = 'P'"}],
                    max_rows=200
                )
                texts = {t["BWART"]: t.get("BTEXT", "") for t in text_data}

            for mov in movements:
                mov["BTEXT"] = texts.get(mov["BWART"], "")

            return ConfigSkillResult(
                success=True,
                config={"movement_types": movements, "count": len(movements)},
                message=f"Encontrados {len(movements)} tipos de movimento",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar tipos de movimento: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Configuracoes FI
    # =========================================================================

    async def get_chart_of_accounts(
        self,
        chart: str = ""
    ) -> ConfigSkillResult:
        """
        Lista planos de contas.

        Args:
            chart: Plano de contas especifico

        Returns:
            ConfigSkillResult com planos
        """
        start_time = datetime.now()

        try:
            options = None
            if chart:
                options = [{"TEXT": f"KTOPL = '{chart}'"}]

            charts = await self.rfc_client.read_table(
                "T004",
                fields=["KTOPL", "KTPLT", "WAESSION", "KTMAX"],
                options=options,
                max_rows=50
            )

            return ConfigSkillResult(
                success=True,
                config={"charts_of_accounts": charts, "count": len(charts)},
                message=f"Encontrados {len(charts)} planos de contas",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar planos de contas: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def get_fiscal_year_variants(self) -> ConfigSkillResult:
        """
        Lista variantes de ano fiscal.

        Returns:
            ConfigSkillResult com variantes
        """
        start_time = datetime.now()

        try:
            variants = await self.rfc_client.read_table(
                "T009",
                fields=["PERIV", "XKALE", "XPERM", "ANESSION"],
                max_rows=50
            )

            return ConfigSkillResult(
                success=True,
                config={"fiscal_year_variants": variants, "count": len(variants)},
                message=f"Encontradas {len(variants)} variantes",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar variantes: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Documentacao
    # =========================================================================

    async def document_config(
        self,
        module: str,
        output_format: str = "markdown"
    ) -> ConfigSkillResult:
        """
        Gera documentacao completa de um modulo.

        Args:
            module: Modulo SAP (SD, MM, FI, CO, PP)
            output_format: Formato de saida (markdown, html)

        Returns:
            ConfigSkillResult com documentacao
        """
        start_time = datetime.now()

        try:
            doc_lines = [
                f"# Documentacao de Configuracao SAP - Modulo {module}",
                "",
                f"Data de geracao: {datetime.now().strftime('%d/%m/%Y %H:%M')}",
                "",
                "---",
                ""
            ]

            config_data = {}

            if module == "SD":
                # Documentar SD
                orgs = await self.get_sales_organizations()
                if orgs.success:
                    config_data["sales_orgs"] = orgs.config
                    doc_lines.extend(self._format_sales_org_doc(orgs.config))

                pricing = await self.get_document_types("SD")
                if pricing.success:
                    config_data["doc_types"] = pricing.config
                    doc_lines.extend(self._format_doc_types(pricing.config, "SD"))

            elif module == "MM":
                # Documentar MM
                orgs = await self.get_purchasing_organizations()
                if orgs.success:
                    config_data["purchasing_orgs"] = orgs.config
                    doc_lines.extend(self._format_purchasing_org_doc(orgs.config))

                mat_types = await self.get_material_types()
                if mat_types.success:
                    config_data["material_types"] = mat_types.config
                    doc_lines.extend(self._format_material_types_doc(mat_types.config))

                movements = await self.get_movement_types()
                if movements.success:
                    config_data["movement_types"] = movements.config

            elif module == "FI":
                # Documentar FI
                companies = await self.get_company_codes()
                if companies.success:
                    config_data["companies"] = companies.config
                    doc_lines.extend(self._format_companies_doc(companies.config))

                charts = await self.get_chart_of_accounts()
                if charts.success:
                    config_data["charts"] = charts.config

            documentation = "\n".join(doc_lines)

            return ConfigSkillResult(
                success=True,
                config=config_data,
                documentation=documentation,
                message=f"Documentacao de {module} gerada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao documentar {module}: {e}")
            return ConfigSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    def _format_sales_org_doc(self, config: Dict) -> List[str]:
        """Formata documentacao de org vendas"""
        lines = [
            "## Organizacoes de Vendas",
            "",
            "| Org. Vendas | Descricao | Empresa |",
            "|-------------|-----------|---------|"
        ]

        for org in config.get("sales_orgs", []):
            lines.append(f"| {org.get('VKORG', '')} | {org.get('VTEXT', '')} | {org.get('BUKRS', '')} |")

        lines.append("")
        return lines

    def _format_purchasing_org_doc(self, config: Dict) -> List[str]:
        """Formata documentacao de org compras"""
        lines = [
            "## Organizacoes de Compras",
            "",
            "| Org. Compras | Descricao | Empresa |",
            "|--------------|-----------|---------|"
        ]

        for org in config.get("purchasing_orgs", []):
            lines.append(f"| {org.get('EKORG', '')} | {org.get('EKOTX', '')} | {org.get('BUKRS', '')} |")

        lines.append("")
        return lines

    def _format_material_types_doc(self, config: Dict) -> List[str]:
        """Formata documentacao de tipos de material"""
        lines = [
            "## Tipos de Material",
            "",
            "| Tipo | Descricao |",
            "|------|-----------|"
        ]

        for mt in config.get("material_types", []):
            lines.append(f"| {mt.get('MTART', '')} | {mt.get('DESCRIPTION', '')} |")

        lines.append("")
        return lines

    def _format_companies_doc(self, config: Dict) -> List[str]:
        """Formata documentacao de empresas"""
        lines = [
            "## Empresas (Company Codes)",
            "",
            "| Empresa | Nome | Moeda | Pais |",
            "|---------|------|-------|------|"
        ]

        for co in config.get("companies", []):
            lines.append(f"| {co.get('BUKRS', '')} | {co.get('BUTXT', '')} | {co.get('WAERS', '')} | {co.get('LAND1', '')} |")

        lines.append("")
        return lines

    def _format_doc_types(self, config: Dict, area: str) -> List[str]:
        """Formata documentacao de tipos de documento"""
        lines = [
            f"## Tipos de Documento ({area})",
            "",
            "| Tipo | Descricao |",
            "|------|-----------|"
        ]

        for dt in config.get("document_types", []):
            if area == "SD":
                lines.append(f"| {dt.get('AUART', '')} | {dt.get('BEZEI', '')} |")
            elif area == "FI":
                lines.append(f"| {dt.get('BLART', '')} | {dt.get('BLTEXT', '')} |")

        lines.append("")
        return lines

    def _elapsed_ms(self, start_time: datetime) -> float:
        """Calcula tempo decorrido em milissegundos"""
        return (datetime.now() - start_time).total_seconds() * 1000
