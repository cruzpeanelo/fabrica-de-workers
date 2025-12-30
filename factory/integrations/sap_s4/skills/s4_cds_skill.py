# -*- coding: utf-8 -*-
"""
S4 CDS Skill
============
Skill para trabalhar com CDS Views no SAP S/4HANA.

Esta skill fornece capacidades de analise e geracao
de CDS Views.

Autor: Fabrica de Agentes
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
import logging

from ..analyzers.cds_analyzer import CDSAnalyzer, CDSViewInfo, CDSViewType
from ..generators.cds_generator import (
    CDSGenerator,
    CDSViewSpec,
    CDSFieldSpec,
    CDSViewTypeVDM
)

logger = logging.getLogger(__name__)


@dataclass
class CDSSkillResult:
    """Resultado de operacao da skill CDS"""
    success: bool
    data: Any = None
    code: Optional[str] = None
    message: str = ""
    warnings: List[str] = field(default_factory=list)


class S4CDSSkill:
    """
    Skill para trabalhar com CDS Views

    Fornece capacidades de:
    - Analise de CDS Views existentes
    - Geracao de novas CDS Views
    - Validacao de codigo CDS
    - Geracao de views de consumo

    Exemplo de uso:
    ```python
    skill = S4CDSSkill(odata_client, tenant_id="tenant-001")

    # Analisar view existente
    result = await skill.analyze_view("I_SalesOrder")

    # Gerar view de consumo
    result = skill.generate_consumption_view(
        name="Z_SALES_ORDER_ANALYSIS",
        base_view="I_SalesOrder",
        fields=["SalesOrder", "SoldToParty", "TotalNetAmount"]
    )

    # Gerar view analitica
    result = skill.generate_analytical_view(
        name="Z_SALES_ANALYTICS",
        base_view="I_SalesOrder",
        dimensions=["SalesOrganization", "Customer"],
        measures=[{"name": "TotalNetAmount", "aggregation": "SUM"}]
    )
    ```
    """

    def __init__(self, odata_client=None, tenant_id: str = ""):
        """
        Inicializa skill

        Args:
            odata_client: Cliente OData V4 opcional
            tenant_id: ID do tenant para isolamento multi-tenant
        """
        self.analyzer = CDSAnalyzer(odata_client)
        self.generator = CDSGenerator()
        self.tenant_id = tenant_id

        if not tenant_id:
            logger.warning("tenant_id nao configurado para S4CDSSkill")

    async def analyze_view(self, view_name: str) -> CDSSkillResult:
        """
        Analisa CDS View do sistema

        Args:
            view_name: Nome da view

        Returns:
            CDSSkillResult com informacoes da view
        """
        try:
            view_info = await self.analyzer.analyze_view(view_name)

            return CDSSkillResult(
                success=True,
                data=view_info.to_dict(),
                message=f"View {view_name} analisada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar view {view_name}: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro ao analisar view: {str(e)}"
            )

    def parse_cds_source(self, source: str) -> CDSSkillResult:
        """
        Faz parse de codigo-fonte CDS

        Args:
            source: Codigo fonte CDS

        Returns:
            CDSSkillResult com informacoes extraidas
        """
        try:
            view_info = self.analyzer.parse_cds_source(source)

            # Validar
            validation_issues = self.analyzer.validate_view(view_info)
            warnings = validation_issues if validation_issues else []

            return CDSSkillResult(
                success=True,
                data=view_info.to_dict(),
                message="Parse realizado com sucesso",
                warnings=warnings
            )

        except Exception as e:
            logger.error(f"Erro ao fazer parse do CDS: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro no parse: {str(e)}"
            )

    def generate_consumption_view(
        self,
        name: str,
        base_view: str,
        fields: List[str],
        description: str = "",
        odata_publish: bool = True
    ) -> CDSSkillResult:
        """
        Gera CDS View de consumo

        Args:
            name: Nome da view (ex: Z_MY_VIEW)
            base_view: View base (ex: I_SalesOrder)
            fields: Lista de campos
            description: Descricao da view
            odata_publish: Publicar via OData

        Returns:
            CDSSkillResult com codigo CDS gerado
        """
        try:
            # Criar especificacao
            spec = CDSViewSpec(
                name=name,
                sql_view_name=self._generate_sql_name(name),
                description=description or f"View de consumo baseada em {base_view}",
                source_entity=base_view,
                view_type=CDSViewTypeVDM.CONSUMPTION,
                odata_publish=odata_publish,
                fields=[
                    CDSFieldSpec(name=f, is_key=(i == 0))
                    for i, f in enumerate(fields)
                ]
            )

            # Gerar codigo
            cds_code = self.generator.generate(spec)

            return CDSSkillResult(
                success=True,
                code=cds_code,
                data={
                    "name": name,
                    "sql_view_name": spec.sql_view_name,
                    "base_view": base_view,
                    "field_count": len(fields)
                },
                message="CDS View gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar CDS View: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro ao gerar CDS: {str(e)}"
            )

    def generate_analytical_view(
        self,
        name: str,
        base_view: str,
        dimensions: List[str],
        measures: List[Dict],
        description: str = ""
    ) -> CDSSkillResult:
        """
        Gera CDS View analitica

        Args:
            name: Nome da view
            base_view: View base
            dimensions: Lista de campos de dimensao
            measures: Lista de medidas [{"name": "Amount", "aggregation": "SUM"}]
            description: Descricao

        Returns:
            CDSSkillResult com codigo CDS gerado
        """
        try:
            cds_code = self.generator.generate_analytical_view(
                name=name,
                base_view=base_view,
                dimensions=dimensions,
                measures=measures,
                description=description or f"View analitica baseada em {base_view}"
            )

            return CDSSkillResult(
                success=True,
                code=cds_code,
                data={
                    "name": name,
                    "base_view": base_view,
                    "dimension_count": len(dimensions),
                    "measure_count": len(measures)
                },
                message="CDS View analitica gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar CDS analitica: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro ao gerar CDS: {str(e)}"
            )

    def generate_extension(
        self,
        original_view: str,
        extension_name: str,
        additional_fields: List[Dict]
    ) -> CDSSkillResult:
        """
        Gera Extension Include para view existente

        Args:
            original_view: Nome da view original
            extension_name: Nome da extensao
            additional_fields: Campos adicionais [{"name": "...", "label": "..."}]

        Returns:
            CDSSkillResult com codigo da extensao
        """
        try:
            field_specs = [
                CDSFieldSpec(
                    name=f["name"],
                    label=f.get("label"),
                    source_field=f.get("source_field"),
                    source_entity=f.get("source_entity")
                )
                for f in additional_fields
            ]

            cds_code = self.generator.generate_extension_include(
                original_view=original_view,
                extension_name=extension_name,
                additional_fields=field_specs
            )

            return CDSSkillResult(
                success=True,
                code=cds_code,
                data={
                    "original_view": original_view,
                    "extension_name": extension_name,
                    "additional_fields": len(additional_fields)
                },
                message="Extension gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar extension: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro ao gerar extension: {str(e)}"
            )

    def generate_from_table(
        self,
        table_name: str,
        view_name: str,
        view_type: str = "consumption",
        description: str = ""
    ) -> CDSSkillResult:
        """
        Gera CDS View a partir de uma tabela

        Args:
            table_name: Nome da tabela
            view_name: Nome da view a gerar
            view_type: Tipo (consumption, basic)
            description: Descricao

        Returns:
            CDSSkillResult com codigo CDS
        """
        try:
            # Determinar tipo VDM
            vdm_type = (
                CDSViewTypeVDM.CONSUMPTION
                if view_type == "consumption"
                else CDSViewTypeVDM.BASIC
            )

            # Campos comuns de uma tabela
            common_fields = [
                CDSFieldSpec(name="*", source_field="*", is_key=False)
            ]

            spec = CDSViewSpec(
                name=view_name,
                sql_view_name=self._generate_sql_name(view_name),
                description=description or f"View para tabela {table_name}",
                source_entity=table_name,
                view_type=vdm_type,
                fields=common_fields
            )

            # Gerar template basico
            cds_template = f'''@AbapCatalog.sqlViewName: '{spec.sql_view_name}'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '{spec.description}'
@VDM.viewType: #{vdm_type.value}

define view {view_name}
  as select from {table_name}
{{
  // TODO: Adicionar campos chave com 'key'
  // TODO: Definir campos da view
  *
}}
'''

            return CDSSkillResult(
                success=True,
                code=cds_template,
                data={
                    "view_name": view_name,
                    "table_name": table_name,
                    "view_type": view_type
                },
                message="Template CDS gerado - ajuste os campos conforme necessario",
                warnings=["Template gerado com todos os campos (*). Refine a selecao."]
            )

        except Exception as e:
            logger.error(f"Erro ao gerar CDS de tabela: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    async def list_views(
        self,
        filter_pattern: Optional[str] = None,
        limit: int = 100
    ) -> CDSSkillResult:
        """
        Lista CDS Views disponiveis no sistema

        Args:
            filter_pattern: Padrao de filtro (ex: "Z*")
            limit: Limite de resultados

        Returns:
            CDSSkillResult com lista de views
        """
        try:
            views = await self.analyzer.list_views(
                filter_pattern=filter_pattern,
                limit=limit
            )

            view_list = [v.to_dict() for v in views]

            return CDSSkillResult(
                success=True,
                data=view_list,
                message=f"Encontradas {len(views)} views"
            )

        except Exception as e:
            logger.error(f"Erro ao listar views: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro ao listar: {str(e)}"
            )

    def validate_cds(self, source: str) -> CDSSkillResult:
        """
        Valida codigo CDS

        Args:
            source: Codigo fonte CDS

        Returns:
            CDSSkillResult com resultado da validacao
        """
        try:
            view_info = self.analyzer.parse_cds_source(source)
            issues = self.analyzer.validate_view(view_info)

            is_valid = len(issues) == 0

            return CDSSkillResult(
                success=is_valid,
                data={
                    "is_valid": is_valid,
                    "issues": issues,
                    "view_name": view_info.name,
                    "field_count": view_info.field_count
                },
                message="Validacao concluida",
                warnings=issues
            )

        except Exception as e:
            logger.error(f"Erro na validacao: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro na validacao: {str(e)}"
            )

    def get_dependencies(self, source: str) -> CDSSkillResult:
        """
        Analisa dependencias de uma CDS View

        Args:
            source: Codigo fonte CDS

        Returns:
            CDSSkillResult com dependencias
        """
        try:
            view_info = self.analyzer.parse_cds_source(source)
            deps = self.analyzer.get_dependencies(view_info)

            return CDSSkillResult(
                success=True,
                data=deps,
                message=f"Encontradas {len(deps.get('all_dependencies', []))} dependencias"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar dependencias: {e}")
            return CDSSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def _generate_sql_name(self, view_name: str) -> str:
        """Gera nome SQL valido (max 16 caracteres)"""
        name = view_name.upper()
        for prefix in ("Z_", "ZC_", "ZI_", "Y_", "YC_", "YI_"):
            if name.startswith(prefix):
                name = name[len(prefix):]
                break
        return "Z" + name[:15]
