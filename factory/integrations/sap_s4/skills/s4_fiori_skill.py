# -*- coding: utf-8 -*-
"""
S4 Fiori Skill
==============
Skill para trabalhar com aplicacoes SAP Fiori.

Esta skill fornece capacidades de analise e geracao
de aplicacoes Fiori Elements e Freestyle.

Autor: Plataforma E
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
import logging

from ..analyzers.fiori_analyzer import (
    FioriAnalyzer,
    FioriAppInfo,
    FioriAppType
)
from ..generators.fiori_generator import (
    FioriGenerator,
    FioriAppSpec,
    FioriFloorplanType,
    FioriFieldSpec,
    FioriActionSpec
)

logger = logging.getLogger(__name__)


@dataclass
class FioriSkillResult:
    """Resultado de operacao da skill Fiori"""
    success: bool
    files: Dict[str, str] = field(default_factory=dict)
    data: Any = None
    message: str = ""
    warnings: List[str] = field(default_factory=list)


class S4FioriSkill:
    """
    Skill para trabalhar com aplicacoes Fiori

    Fornece capacidades de:
    - Analise de apps Fiori existentes
    - Geracao de apps Fiori Elements
    - Geracao de annotations
    - Criacao de projetos Fiori

    Exemplo de uso:
    ```python
    skill = S4FioriSkill(tenant_id="tenant-001")

    # Gerar List Report
    result = skill.generate_list_report(
        app_id="com.company.salesorders",
        title="Ordens de Venda",
        odata_service="/sap/opu/odata4/sap/api_sales_order/",
        entity_set="SalesOrder",
        list_fields=[
            {"name": "SalesOrder", "label": "Ordem", "is_key": True},
            {"name": "Customer", "label": "Cliente"},
            {"name": "TotalAmount", "label": "Valor Total"}
        ]
    )

    # Analisar manifest.json
    result = skill.analyze_manifest(manifest_json)
    ```
    """

    def __init__(self, odata_client=None, tenant_id: str = ""):
        """
        Inicializa skill

        Args:
            odata_client: Cliente OData V4 opcional
            tenant_id: ID do tenant para isolamento multi-tenant
        """
        self.analyzer = FioriAnalyzer(odata_client)
        self.generator = FioriGenerator()
        self.tenant_id = tenant_id

        if not tenant_id:
            logger.warning("tenant_id nao configurado para S4FioriSkill")

    def analyze_manifest(self, manifest: Dict) -> FioriSkillResult:
        """
        Analisa manifest.json de uma aplicacao Fiori

        Args:
            manifest: Conteudo do manifest.json

        Returns:
            FioriSkillResult com informacoes do app
        """
        try:
            app_info = self.analyzer.parse_manifest(manifest)

            return FioriSkillResult(
                success=True,
                data=app_info.to_dict(),
                message=f"App '{app_info.title}' analisado com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar manifest: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro ao analisar: {str(e)}"
            )

    def analyze_view_xml(self, xml_content: str) -> FioriSkillResult:
        """
        Analisa View XML

        Args:
            xml_content: Conteudo XML da view

        Returns:
            FioriSkillResult com analise
        """
        try:
            analysis = self.analyzer.analyze_view_xml(xml_content)

            return FioriSkillResult(
                success=True,
                data=analysis,
                message="View analisada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar view: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def generate_list_report(
        self,
        app_id: str,
        title: str,
        odata_service: str,
        entity_set: str,
        list_fields: List[Dict],
        filter_fields: Optional[List[Dict]] = None,
        description: str = "",
        with_object_page: bool = True
    ) -> FioriSkillResult:
        """
        Gera aplicacao Fiori Elements List Report

        Args:
            app_id: ID da aplicacao (ex: com.company.app)
            title: Titulo da aplicacao
            odata_service: URL do servico OData
            entity_set: Entity set principal
            list_fields: Campos da lista [{"name": "...", "label": "..."}]
            filter_fields: Campos de filtro
            description: Descricao
            with_object_page: Incluir Object Page

        Returns:
            FioriSkillResult com arquivos gerados
        """
        try:
            # Converter fields
            list_specs = [
                FioriFieldSpec(
                    name=f["name"],
                    label=f.get("label", f["name"]),
                    is_key=f.get("is_key", False),
                    position=i * 10,
                    importance=f.get("importance", "HIGH")
                )
                for i, f in enumerate(list_fields)
            ]

            filter_specs = []
            if filter_fields:
                filter_specs = [
                    FioriFieldSpec(
                        name=f["name"],
                        label=f.get("label", f["name"])
                    )
                    for f in filter_fields
                ]
            else:
                # Usar primeiros campos como filtros
                filter_specs = list_specs[:3]

            # Criar especificacao
            spec = FioriAppSpec(
                app_id=app_id,
                title=title,
                description=description or f"Aplicacao {title}",
                floorplan=FioriFloorplanType.LIST_REPORT,
                odata_service_url=odata_service,
                main_entity_set=entity_set,
                list_fields=list_specs,
                filter_fields=filter_specs,
                detail_fields=list_specs if with_object_page else []
            )

            # Gerar arquivos
            files = self.generator.generate(spec)

            return FioriSkillResult(
                success=True,
                files=files,
                data={
                    "app_id": app_id,
                    "floorplan": "List Report",
                    "entity_set": entity_set,
                    "files_generated": list(files.keys())
                },
                message=f"Aplicacao '{title}' gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar List Report: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro ao gerar: {str(e)}"
            )

    def generate_worklist(
        self,
        app_id: str,
        title: str,
        odata_service: str,
        entity_set: str,
        list_fields: List[Dict],
        description: str = ""
    ) -> FioriSkillResult:
        """
        Gera aplicacao Fiori Elements Worklist

        Args:
            app_id: ID da aplicacao
            title: Titulo
            odata_service: URL do servico OData
            entity_set: Entity set
            list_fields: Campos da lista
            description: Descricao

        Returns:
            FioriSkillResult com arquivos
        """
        try:
            list_specs = [
                FioriFieldSpec(
                    name=f["name"],
                    label=f.get("label", f["name"]),
                    is_key=f.get("is_key", False)
                )
                for f in list_fields
            ]

            spec = FioriAppSpec(
                app_id=app_id,
                title=title,
                description=description,
                floorplan=FioriFloorplanType.WORKLIST,
                odata_service_url=odata_service,
                main_entity_set=entity_set,
                list_fields=list_specs
            )

            files = self.generator.generate(spec)

            return FioriSkillResult(
                success=True,
                files=files,
                data={
                    "app_id": app_id,
                    "floorplan": "Worklist",
                    "entity_set": entity_set
                },
                message="Worklist gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar Worklist: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def generate_analytical_list_page(
        self,
        app_id: str,
        title: str,
        odata_service: str,
        entity_set: str,
        dimensions: List[Dict],
        measures: List[Dict],
        description: str = ""
    ) -> FioriSkillResult:
        """
        Gera Analytical List Page

        Args:
            app_id: ID da aplicacao
            title: Titulo
            odata_service: Servico OData
            entity_set: Entity set
            dimensions: Campos de dimensao
            measures: Campos de medida
            description: Descricao

        Returns:
            FioriSkillResult com arquivos
        """
        try:
            all_fields = [
                FioriFieldSpec(
                    name=d["name"],
                    label=d.get("label", d["name"]),
                    is_key=True
                )
                for d in dimensions
            ] + [
                FioriFieldSpec(
                    name=m["name"],
                    label=m.get("label", m["name"])
                )
                for m in measures
            ]

            spec = FioriAppSpec(
                app_id=app_id,
                title=title,
                description=description,
                floorplan=FioriFloorplanType.ANALYTICAL_LIST_PAGE,
                odata_service_url=odata_service,
                main_entity_set=entity_set,
                list_fields=all_fields
            )

            files = self.generator.generate(spec)

            return FioriSkillResult(
                success=True,
                files=files,
                data={
                    "app_id": app_id,
                    "floorplan": "Analytical List Page",
                    "dimensions": len(dimensions),
                    "measures": len(measures)
                },
                message="ALP gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar ALP: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def generate_freestyle_view(
        self,
        view_name: str,
        controller_name: str,
        namespace: str,
        content_type: str = "table"
    ) -> FioriSkillResult:
        """
        Gera View Freestyle basica

        Args:
            view_name: Nome da view
            controller_name: Nome do controller
            namespace: Namespace
            content_type: Tipo de conteudo (table, form, list)

        Returns:
            FioriSkillResult com arquivos
        """
        try:
            # Template de conteudo baseado no tipo
            content_templates = {
                "table": '''            <Table
                id="mainTable"
                items="{/Items}">
                <columns>
                    <Column>
                        <Text text="{i18n>column1}"/>
                    </Column>
                    <Column>
                        <Text text="{i18n>column2}"/>
                    </Column>
                </columns>
                <items>
                    <ColumnListItem>
                        <cells>
                            <Text text="{Field1}"/>
                            <Text text="{Field2}"/>
                        </cells>
                    </ColumnListItem>
                </items>
            </Table>''',
                "form": '''            <VBox class="sapUiMediumMargin">
                <Label text="{i18n>field1Label}"/>
                <Input value="{/Field1}"/>
                <Label text="{i18n>field2Label}"/>
                <Input value="{/Field2}"/>
            </VBox>''',
                "list": '''            <List
                id="mainList"
                items="{/Items}">
                <StandardListItem
                    title="{Title}"
                    description="{Description}"
                    type="Navigation"/>
            </List>'''
            }

            content = content_templates.get(content_type, content_templates["table"])

            # Gerar view
            view_xml = self.generator.generate_view_xml(
                view_name=view_name,
                controller_name=controller_name,
                content=content,
                namespace=namespace
            )

            # Gerar controller
            controller_js = self.generator.generate_controller_js(
                controller_name=controller_name,
                namespace=namespace,
                methods=["onPress", "onSearch", "onRefresh"]
            )

            files = {
                f"view/{view_name}.view.xml": view_xml,
                f"controller/{controller_name}.controller.js": controller_js
            }

            return FioriSkillResult(
                success=True,
                files=files,
                data={
                    "view_name": view_name,
                    "controller_name": controller_name,
                    "content_type": content_type
                },
                message="View freestyle gerada com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar view freestyle: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def generate_annotations(
        self,
        namespace: str,
        entity_set: str,
        line_item_fields: List[Dict],
        filter_fields: Optional[List[Dict]] = None,
        header_info: Optional[Dict] = None
    ) -> FioriSkillResult:
        """
        Gera arquivo de annotations OData

        Args:
            namespace: Namespace do servico
            entity_set: Entity set
            line_item_fields: Campos para LineItem
            filter_fields: Campos para SelectionFields
            header_info: Informacoes de header

        Returns:
            FioriSkillResult com XML de annotations
        """
        try:
            lines = [
                '<?xml version="1.0" encoding="utf-8"?>',
                '<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">',
                '  <edmx:DataServices>',
                f'    <Schema Namespace="{namespace}.annotations" xmlns="http://docs.oasis-open.org/odata/ns/edm">',
                f'      <Annotations Target="{namespace}.{entity_set}Type">'
            ]

            # SelectionFields
            if filter_fields:
                lines.append('        <Annotation Term="UI.SelectionFields">')
                lines.append('          <Collection>')
                for f in filter_fields:
                    lines.append(f'            <PropertyPath>{f["name"]}</PropertyPath>')
                lines.append('          </Collection>')
                lines.append('        </Annotation>')

            # LineItem
            if line_item_fields:
                lines.append('        <Annotation Term="UI.LineItem">')
                lines.append('          <Collection>')
                for f in line_item_fields:
                    lines.append('            <Record Type="UI.DataField">')
                    lines.append(f'              <PropertyValue Property="Value" Path="{f["name"]}"/>')
                    lines.append(f'              <PropertyValue Property="Label" String="{f.get("label", f["name"])}"/>')
                    lines.append('            </Record>')
                lines.append('          </Collection>')
                lines.append('        </Annotation>')

            # HeaderInfo
            if header_info:
                lines.append('        <Annotation Term="UI.HeaderInfo">')
                lines.append('          <Record Type="UI.HeaderInfoType">')
                lines.append(f'            <PropertyValue Property="TypeName" String="{header_info.get("type_name", entity_set)}"/>')
                lines.append(f'            <PropertyValue Property="TypeNamePlural" String="{header_info.get("type_name_plural", entity_set + "s")}"/>')
                if header_info.get("title_field"):
                    lines.append('            <PropertyValue Property="Title">')
                    lines.append('              <Record Type="UI.DataField">')
                    lines.append(f'                <PropertyValue Property="Value" Path="{header_info["title_field"]}"/>')
                    lines.append('              </Record>')
                    lines.append('            </PropertyValue>')
                lines.append('          </Record>')
                lines.append('        </Annotation>')

            lines.extend([
                '      </Annotations>',
                '    </Schema>',
                '  </edmx:DataServices>',
                '</edmx:Edmx>'
            ])

            annotation_xml = "\n".join(lines)

            return FioriSkillResult(
                success=True,
                files={"annotations/annotation.xml": annotation_xml},
                data={
                    "entity_set": entity_set,
                    "line_item_count": len(line_item_fields),
                    "filter_count": len(filter_fields) if filter_fields else 0
                },
                message="Annotations geradas com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar annotations: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )

    def generate_i18n(
        self,
        app_title: str,
        app_description: str,
        labels: Dict[str, str]
    ) -> FioriSkillResult:
        """
        Gera arquivo i18n.properties

        Args:
            app_title: Titulo do app
            app_description: Descricao
            labels: Dicionario de labels {key: value}

        Returns:
            FioriSkillResult com arquivo i18n
        """
        try:
            lines = [
                f"appTitle={app_title}",
                f"appDescription={app_description}",
                ""
            ]

            for key, value in labels.items():
                lines.append(f"{key}={value}")

            i18n_content = "\n".join(lines)

            return FioriSkillResult(
                success=True,
                files={"i18n/i18n.properties": i18n_content},
                data={"label_count": len(labels)},
                message="i18n gerado com sucesso"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar i18n: {e}")
            return FioriSkillResult(
                success=False,
                message=f"Erro: {str(e)}"
            )
