# -*- coding: utf-8 -*-
"""
CDS View Generator
==================
Gerador de CDS Views para SAP S/4HANA.

Este modulo gera codigo CDS View seguindo as melhores praticas SAP,
incluindo anotacoes VDM, UI e semanticas.

Autor: Fabrica de Agentes
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class CDSViewTypeVDM(str, Enum):
    """Tipos de View conforme Virtual Data Model SAP"""
    BASIC = "BASIC"              # I_ prefix - Interface View
    COMPOSITE = "COMPOSITE"       # C_ prefix (parcial) - View Composta
    CONSUMPTION = "CONSUMPTION"   # C_ prefix - View de Consumo


class CDSFieldType(str, Enum):
    """Tipos de campos ABAP/CDS"""
    STRING = "abap.char"
    INT = "abap.int4"
    DECIMAL = "abap.dec"
    DATE = "abap.dats"
    TIME = "abap.tims"
    TIMESTAMP = "timestampl"
    BOOLEAN = "abap_boolean"
    AMOUNT = "abap.curr"
    QUANTITY = "abap.quan"
    UUID = "sysuuid_x16"


@dataclass
class CDSFieldSpec:
    """Especificacao de um campo CDS"""
    name: str
    alias: Optional[str] = None
    source_field: Optional[str] = None
    source_entity: Optional[str] = None
    data_type: Optional[str] = None
    length: Optional[int] = None
    decimals: Optional[int] = None
    is_key: bool = False
    is_localized: bool = False
    label: Optional[str] = None
    quick_info: Optional[str] = None

    # Anotacoes semanticas
    semantic_type: Optional[str] = None  # @Semantics.amount.currencyCode, etc

    # Anotacoes UI
    ui_hidden: bool = False
    ui_importance: Optional[str] = None  # HIGH, MEDIUM, LOW
    ui_position: Optional[int] = None

    # Expressao (para campos calculados)
    expression: Optional[str] = None

    @property
    def output_name(self) -> str:
        """Nome de saida do campo"""
        return self.alias or self.name


@dataclass
class CDSAssociationSpec:
    """Especificacao de uma associacao CDS"""
    name: str
    target_entity: str
    cardinality: str = "[0..1]"
    condition_fields: List[tuple] = field(default_factory=list)  # [(source, target), ...]
    is_composition: bool = False


@dataclass
class CDSParameterSpec:
    """Especificacao de um parametro CDS"""
    name: str
    data_type: str
    default_value: Optional[str] = None
    label: Optional[str] = None


@dataclass
class CDSViewSpec:
    """
    Especificacao completa para geracao de CDS View

    Contem todas as informacoes necessarias para gerar
    uma CDS View completa e valida.
    """
    # Identificacao
    name: str
    sql_view_name: str
    description: str = ""

    # Classificacao VDM
    view_type: CDSViewTypeVDM = CDSViewTypeVDM.CONSUMPTION

    # Fonte de dados
    source_entity: str = ""
    join_entities: List[Dict] = field(default_factory=list)

    # Campos
    fields: List[CDSFieldSpec] = field(default_factory=list)

    # Associacoes
    associations: List[CDSAssociationSpec] = field(default_factory=list)

    # Parametros
    parameters: List[CDSParameterSpec] = field(default_factory=list)

    # Anotacoes de nivel de view
    odata_publish: bool = True
    analytics_dataextraction: bool = False
    authorization_check: str = "#CHECK"
    search_searchable: bool = True

    # Metadados
    package: str = "$TMP"
    author: str = ""

    def validate(self) -> List[str]:
        """Valida especificacao"""
        errors = []

        if not self.name:
            errors.append("Nome da view e obrigatorio")

        if not self.sql_view_name:
            errors.append("Nome do SQL View e obrigatorio")

        if len(self.sql_view_name) > 16:
            errors.append("Nome do SQL View deve ter no maximo 16 caracteres")

        if not self.source_entity:
            errors.append("Entidade fonte e obrigatoria")

        if not self.fields:
            errors.append("View deve ter pelo menos um campo")

        key_fields = [f for f in self.fields if f.is_key]
        if not key_fields:
            errors.append("View deve ter pelo menos um campo chave")

        return errors


class CDSGenerator:
    """
    Gerador de CDS Views para SAP S/4HANA

    Gera codigo CDS View completo seguindo padroes SAP:
    - Anotacoes VDM para classificacao
    - Anotacoes UI para Fiori
    - Anotacoes semanticas
    - Access Control

    Exemplo de uso:
    ```python
    generator = CDSGenerator()

    spec = CDSViewSpec(
        name="Z_SALES_ORDER_ANALYSIS",
        sql_view_name="ZSALESORDANALYS",
        description="Analise de Ordens de Venda",
        source_entity="I_SalesOrder",
        view_type=CDSViewTypeVDM.CONSUMPTION,
        fields=[
            CDSFieldSpec(name="SalesOrder", is_key=True, label="Ordem de Venda"),
            CDSFieldSpec(name="SoldToParty", label="Cliente"),
            CDSFieldSpec(name="TotalNetAmount", label="Valor Total")
        ]
    )

    cds_code = generator.generate(spec)
    ```
    """

    def __init__(self, indent: str = "  "):
        """
        Inicializa gerador

        Args:
            indent: String de indentacao (default: 2 espacos)
        """
        self.indent = indent

    def generate(self, spec: CDSViewSpec) -> str:
        """
        Gera codigo CDS View completo

        Args:
            spec: Especificacao da view

        Returns:
            String com codigo CDS

        Raises:
            ValueError: Se especificacao invalida
        """
        errors = spec.validate()
        if errors:
            raise ValueError(f"Especificacao invalida: {', '.join(errors)}")

        parts = []

        # Adicionar header com anotacoes
        parts.append(self._generate_header_annotations(spec))

        # Adicionar definicao da view
        parts.append(self._generate_view_definition(spec))

        # Adicionar corpo da view
        parts.append(self._generate_view_body(spec))

        return "\n".join(parts)

    def _generate_header_annotations(self, spec: CDSViewSpec) -> str:
        """Gera anotacoes de header"""
        lines = []

        # AbapCatalog annotations
        lines.append(f"@AbapCatalog.sqlViewName: '{spec.sql_view_name}'")
        lines.append("@AbapCatalog.compiler.compareFilter: true")
        lines.append("@AbapCatalog.preserveKey: true")

        # AccessControl
        lines.append(f"@AccessControl.authorizationCheck: {spec.authorization_check}")

        # EndUserText
        if spec.description:
            lines.append(f"@EndUserText.label: '{self._escape_string(spec.description)}'")

        # OData
        if spec.odata_publish:
            lines.append("@OData.publish: true")

        # VDM
        lines.append(f"@VDM.viewType: #{spec.view_type.value}")

        # Analytics
        if spec.analytics_dataextraction:
            lines.append("@Analytics.dataExtraction.enabled: true")

        # Search
        if spec.search_searchable:
            lines.append("@Search.searchable: true")

        lines.append("")  # Linha em branco antes da definicao

        return "\n".join(lines)

    def _generate_view_definition(self, spec: CDSViewSpec) -> str:
        """Gera linha de definicao da view"""
        parts = ["define view"]
        parts.append(spec.name)

        # Parametros
        if spec.parameters:
            param_defs = []
            for param in spec.parameters:
                param_def = f"{param.name} : {param.data_type}"
                if param.default_value:
                    param_def += f" = {param.default_value}"
                param_defs.append(param_def)
            parts.append(f"with parameters {', '.join(param_defs)}")

        # Fonte
        parts.append("as select from")
        parts.append(spec.source_entity)

        # Joins
        for join in spec.join_entities:
            join_type = join.get("type", "inner join")
            entity = join.get("entity")
            alias = join.get("alias", entity)
            condition = join.get("condition", "")
            parts.append(f"{join_type} {entity} as {alias} on {condition}")

        # Associacoes
        for assoc in spec.associations:
            lines = self._generate_association(assoc)
            parts.extend(lines)

        return " ".join(parts)

    def _generate_association(self, assoc: CDSAssociationSpec) -> List[str]:
        """Gera definicao de associacao"""
        lines = []

        keyword = "composition" if assoc.is_composition else "association"
        lines.append(f"{keyword} {assoc.cardinality} to {assoc.target_entity} as {assoc.name}")

        if assoc.condition_fields:
            conditions = []
            for source, target in assoc.condition_fields:
                conditions.append(f"$projection.{source} = {assoc.name}.{target}")
            lines.append(f"  on {' and '.join(conditions)}")

        return lines

    def _generate_view_body(self, spec: CDSViewSpec) -> str:
        """Gera corpo da view (campos)"""
        lines = ["{"]

        for i, field_spec in enumerate(spec.fields):
            field_lines = self._generate_field(field_spec, is_last=(i == len(spec.fields) - 1))
            lines.extend(field_lines)

        # Associacoes expostas
        for assoc in spec.associations:
            lines.append(f"{self.indent}{assoc.name}")

        lines.append("}")

        return "\n".join(lines)

    def _generate_field(self, field_spec: CDSFieldSpec, is_last: bool = False) -> List[str]:
        """Gera definicao de campo com anotacoes"""
        lines = []

        # Anotacoes de campo
        annotations = self._generate_field_annotations(field_spec)
        for ann in annotations:
            lines.append(f"{self.indent}{ann}")

        # Definicao do campo
        field_def = self.indent
        if field_spec.is_key:
            field_def += "key "

        # Campo ou expressao
        if field_spec.expression:
            field_def += field_spec.expression
        elif field_spec.source_entity:
            field_def += f"{field_spec.source_entity}.{field_spec.source_field or field_spec.name}"
        else:
            field_def += field_spec.source_field or field_spec.name

        # Alias
        if field_spec.alias:
            field_def += f" as {field_spec.alias}"

        # Virgula (exceto ultimo)
        if not is_last:
            field_def += ","

        lines.append(field_def)

        return lines

    def _generate_field_annotations(self, field_spec: CDSFieldSpec) -> List[str]:
        """Gera anotacoes para um campo"""
        annotations = []

        # EndUserText
        if field_spec.label:
            annotations.append(f"@EndUserText.label: '{self._escape_string(field_spec.label)}'")

        if field_spec.quick_info:
            annotations.append(f"@EndUserText.quickInfo: '{self._escape_string(field_spec.quick_info)}'")

        # UI annotations
        if field_spec.ui_hidden:
            annotations.append("@UI.hidden: true")

        if field_spec.ui_importance:
            annotations.append(f"@UI.importance: #{field_spec.ui_importance}")

        if field_spec.ui_position is not None:
            annotations.append(f"@UI.lineItem: [{{ position: {field_spec.ui_position} }}]")

        # Semantic annotations
        if field_spec.semantic_type:
            annotations.append(f"@Semantics.{field_spec.semantic_type}")

        # Search
        if not field_spec.is_key and field_spec.data_type in ("abap.char", "abap.string"):
            annotations.append("@Search.defaultSearchElement: true")

        return annotations

    def _escape_string(self, value: str) -> str:
        """Escapa string para CDS"""
        return value.replace("'", "''")

    def generate_consumption_view(
        self,
        name: str,
        base_view: str,
        fields: List[str],
        description: str = "",
        with_associations: bool = True
    ) -> str:
        """
        Gera CDS View de consumo simples

        Args:
            name: Nome da view
            base_view: View base (geralmente I_*)
            fields: Lista de nomes de campos
            description: Descricao
            with_associations: Incluir associacoes

        Returns:
            Codigo CDS
        """
        sql_view_name = self._generate_sql_view_name(name)

        spec = CDSViewSpec(
            name=name,
            sql_view_name=sql_view_name,
            description=description,
            source_entity=base_view,
            view_type=CDSViewTypeVDM.CONSUMPTION,
            fields=[
                CDSFieldSpec(name=f, is_key=(i == 0))
                for i, f in enumerate(fields)
            ]
        )

        return self.generate(spec)

    def generate_analytical_view(
        self,
        name: str,
        base_view: str,
        dimensions: List[str],
        measures: List[Dict],
        description: str = ""
    ) -> str:
        """
        Gera CDS View analitica

        Args:
            name: Nome da view
            base_view: View base
            dimensions: Lista de campos de dimensao
            measures: Lista de medidas [{"name": "Amount", "aggregation": "SUM"}]
            description: Descricao

        Returns:
            Codigo CDS
        """
        sql_view_name = self._generate_sql_view_name(name)

        fields = []

        # Dimensoes
        for dim in dimensions:
            fields.append(CDSFieldSpec(
                name=dim,
                is_key=True
            ))

        # Medidas
        for measure in measures:
            field_spec = CDSFieldSpec(
                name=measure["name"],
                semantic_type=f"aggregate.{measure.get('aggregation', 'SUM').lower()}"
            )
            fields.append(field_spec)

        spec = CDSViewSpec(
            name=name,
            sql_view_name=sql_view_name,
            description=description,
            source_entity=base_view,
            view_type=CDSViewTypeVDM.CONSUMPTION,
            analytics_dataextraction=True,
            fields=fields
        )

        # Adicionar anotacao @Analytics.query
        cds_code = self.generate(spec)

        # Inserir anotacao analitica
        cds_code = cds_code.replace(
            "@VDM.viewType:",
            "@Analytics.query: true\n@VDM.viewType:"
        )

        return cds_code

    def _generate_sql_view_name(self, view_name: str) -> str:
        """Gera nome de SQL View valido (max 16 caracteres)"""
        # Remover prefixos comuns
        name = view_name.upper()
        for prefix in ("Z_", "ZC_", "ZI_", "Y_", "YC_", "YI_"):
            if name.startswith(prefix):
                name = name[len(prefix):]
                break

        # Truncar e adicionar prefixo Z
        name = "Z" + name[:15]

        return name

    def generate_extension_include(
        self,
        original_view: str,
        extension_name: str,
        additional_fields: List[CDSFieldSpec]
    ) -> str:
        """
        Gera Extension Include para adicionar campos a view existente

        Args:
            original_view: Nome da view original
            extension_name: Nome da extensao
            additional_fields: Campos adicionais

        Returns:
            Codigo CDS para extension include
        """
        lines = [
            f"@AbapCatalog.sqlViewAppendName: 'Z{extension_name[:15].upper()}'",
            f"@EndUserText.label: 'Extension for {original_view}'",
            "",
            f"extend view {original_view} with {extension_name}",
            "{"
        ]

        for i, field_spec in enumerate(additional_fields):
            field_lines = self._generate_field(
                field_spec,
                is_last=(i == len(additional_fields) - 1)
            )
            lines.extend(field_lines)

        lines.append("}")

        return "\n".join(lines)
