# -*- coding: utf-8 -*-
"""
Salesforce Object Analyzer
==========================
Analisador de objetos Salesforce (standard e custom).

Funcionalidades:
- Analise de estrutura de objetos
- Mapeamento de campos e tipos
- Identificacao de relacionamentos
- Analise de triggers e validation rules
- Geracao de documentacao
- Analise de dependencias

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.analyzers import ObjectAnalyzer

    sf = SalesforceClient(config)
    await sf.connect()

    analyzer = ObjectAnalyzer(sf)

    # Analisar objeto
    analysis = await analyzer.analyze("Account")
    print(f"Campos: {len(analysis.fields)}")
    print(f"Relacionamentos: {len(analysis.relationships)}")

    # Gerar documentacao
    doc = await analyzer.generate_documentation("Account")
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class FieldType(str, Enum):
    """Tipos de campos Salesforce"""
    ID = "id"
    STRING = "string"
    TEXTAREA = "textarea"
    PICKLIST = "picklist"
    MULTIPICKLIST = "multipicklist"
    BOOLEAN = "boolean"
    INTEGER = "int"
    DOUBLE = "double"
    CURRENCY = "currency"
    PERCENT = "percent"
    DATE = "date"
    DATETIME = "datetime"
    TIME = "time"
    EMAIL = "email"
    PHONE = "phone"
    URL = "url"
    REFERENCE = "reference"
    LOOKUP = "reference"
    MASTERDETAIL = "reference"
    ENCRYPTEDSTRING = "encryptedstring"
    BASE64 = "base64"
    ADDRESS = "address"
    LOCATION = "location"
    COMBOBOX = "combobox"


class RelationshipType(str, Enum):
    """Tipos de relacionamento"""
    LOOKUP = "Lookup"
    MASTER_DETAIL = "MasterDetail"
    EXTERNAL_LOOKUP = "ExternalLookup"
    INDIRECT_LOOKUP = "IndirectLookup"
    HIERARCHY = "Hierarchy"


@dataclass
class FieldInfo:
    """Informacoes de um campo"""
    name: str
    label: str
    type: str
    length: int = 0
    precision: int = 0
    scale: int = 0
    is_required: bool = False
    is_unique: bool = False
    is_custom: bool = False
    is_createable: bool = True
    is_updateable: bool = True
    is_external_id: bool = False
    is_encrypted: bool = False
    default_value: Optional[str] = None
    formula: Optional[str] = None
    picklist_values: List[str] = field(default_factory=list)
    reference_to: Optional[str] = None
    relationship_name: Optional[str] = None
    help_text: Optional[str] = None
    description: Optional[str] = None


@dataclass
class RelationshipInfo:
    """Informacoes de um relacionamento"""
    name: str
    related_object: str
    relationship_type: str
    field_name: str
    is_child: bool
    is_cascade_delete: bool = False
    child_object: Optional[str] = None
    relationship_order: Optional[int] = None


@dataclass
class ValidationRuleInfo:
    """Informacoes de uma validation rule"""
    name: str
    active: bool
    formula: str
    error_message: str
    error_display_field: Optional[str] = None
    description: Optional[str] = None


@dataclass
class TriggerInfo:
    """Informacoes de um trigger"""
    name: str
    is_active: bool
    events: List[str] = field(default_factory=list)
    body: Optional[str] = None


@dataclass
class RecordTypeInfo:
    """Informacoes de um record type"""
    id: str
    name: str
    developer_name: str
    is_active: bool
    is_default: bool
    is_master: bool
    description: Optional[str] = None


@dataclass
class ObjectAnalysis:
    """Resultado da analise de um objeto"""
    name: str
    label: str
    label_plural: str
    api_name: str
    is_custom: bool
    is_queryable: bool
    is_createable: bool
    is_updateable: bool
    is_deletable: bool
    key_prefix: str

    # Estrutura
    fields: List[FieldInfo] = field(default_factory=list)
    relationships: List[RelationshipInfo] = field(default_factory=list)
    record_types: List[RecordTypeInfo] = field(default_factory=list)
    validation_rules: List[ValidationRuleInfo] = field(default_factory=list)
    triggers: List[TriggerInfo] = field(default_factory=list)

    # Metricas
    total_fields: int = 0
    custom_fields: int = 0
    required_fields: int = 0
    formula_fields: int = 0
    lookup_fields: int = 0
    master_detail_fields: int = 0

    # Dependencias
    depends_on: List[str] = field(default_factory=list)
    used_by: List[str] = field(default_factory=list)

    analyzed_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        """Converte analise para dicionario"""
        return {
            "name": self.name,
            "label": self.label,
            "label_plural": self.label_plural,
            "api_name": self.api_name,
            "is_custom": self.is_custom,
            "key_prefix": self.key_prefix,
            "metrics": {
                "total_fields": self.total_fields,
                "custom_fields": self.custom_fields,
                "required_fields": self.required_fields,
                "formula_fields": self.formula_fields,
                "lookup_fields": self.lookup_fields,
                "master_detail_fields": self.master_detail_fields,
                "record_types": len(self.record_types),
                "validation_rules": len(self.validation_rules),
                "triggers": len(self.triggers)
            },
            "fields": [
                {
                    "name": f.name,
                    "label": f.label,
                    "type": f.type,
                    "is_required": f.is_required,
                    "is_custom": f.is_custom
                }
                for f in self.fields
            ],
            "relationships": [
                {
                    "name": r.name,
                    "related_object": r.related_object,
                    "type": r.relationship_type,
                    "is_child": r.is_child
                }
                for r in self.relationships
            ],
            "depends_on": self.depends_on,
            "used_by": self.used_by,
            "analyzed_at": self.analyzed_at.isoformat() if self.analyzed_at else None
        }


class ObjectAnalyzer:
    """
    Analisador de objetos Salesforce

    Fornece analise detalhada de objetos incluindo campos,
    relacionamentos, triggers e validation rules.
    """

    def __init__(self, sf_client):
        """
        Inicializa o analisador

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client
        self._cache: Dict[str, ObjectAnalysis] = {}

    async def analyze(
        self,
        sobject: str,
        include_triggers: bool = True,
        include_validation_rules: bool = True,
        use_cache: bool = True
    ) -> ObjectAnalysis:
        """
        Analisa um objeto Salesforce

        Args:
            sobject: Nome do objeto
            include_triggers: Incluir triggers
            include_validation_rules: Incluir validation rules
            use_cache: Usar cache

        Returns:
            ObjectAnalysis com detalhes completos
        """
        # Verificar cache
        if use_cache and sobject in self._cache:
            logger.debug(f"Usando cache para {sobject}")
            return self._cache[sobject]

        logger.info(f"Analisando objeto: {sobject}")

        # Obter describe
        describe = await self.sf.describe(sobject)

        # Criar analise base
        analysis = ObjectAnalysis(
            name=describe.name,
            label=describe.label,
            label_plural=describe.label_plural,
            api_name=describe.name,
            is_custom=describe.is_custom,
            is_queryable=describe.is_queryable,
            is_createable=describe.is_createable,
            is_updateable=describe.is_updateable,
            is_deletable=describe.is_deletable,
            key_prefix=describe.key_prefix,
            analyzed_at=datetime.now()
        )

        # Analisar campos
        analysis.fields = self._analyze_fields(describe.fields)
        analysis.total_fields = len(analysis.fields)
        analysis.custom_fields = sum(1 for f in analysis.fields if f.is_custom)
        analysis.required_fields = sum(1 for f in analysis.fields if f.is_required)
        analysis.formula_fields = sum(1 for f in analysis.fields if f.formula)
        analysis.lookup_fields = sum(1 for f in analysis.fields if f.reference_to and not f.name.endswith("__c"))
        analysis.master_detail_fields = sum(1 for f in analysis.fields if f.type == "reference" and f.name.endswith("__c"))

        # Analisar relacionamentos
        analysis.relationships = self._analyze_relationships(describe)

        # Analisar record types
        analysis.record_types = self._analyze_record_types(describe.record_type_infos)

        # Identificar dependencias
        for field in analysis.fields:
            if field.reference_to and field.reference_to not in analysis.depends_on:
                analysis.depends_on.append(field.reference_to)

        # Triggers (se solicitado)
        if include_triggers:
            analysis.triggers = await self._get_triggers(sobject)

        # Validation rules (se solicitado)
        if include_validation_rules:
            analysis.validation_rules = await self._get_validation_rules(sobject)

        # Cachear
        self._cache[sobject] = analysis

        return analysis

    def _analyze_fields(self, fields: List[Dict[str, Any]]) -> List[FieldInfo]:
        """Analisa lista de campos"""
        result = []

        for field_data in fields:
            field = FieldInfo(
                name=field_data.get("name", ""),
                label=field_data.get("label", ""),
                type=field_data.get("type", ""),
                length=field_data.get("length", 0),
                precision=field_data.get("precision", 0),
                scale=field_data.get("scale", 0),
                is_required=not field_data.get("nillable", True),
                is_unique=field_data.get("unique", False),
                is_custom=field_data.get("custom", False),
                is_createable=field_data.get("createable", True),
                is_updateable=field_data.get("updateable", True),
                is_external_id=field_data.get("externalId", False),
                is_encrypted=field_data.get("encrypted", False),
                default_value=str(field_data.get("defaultValue", "")) if field_data.get("defaultValue") else None,
                formula=field_data.get("calculatedFormula"),
                help_text=field_data.get("inlineHelpText")
            )

            # Picklist values
            if field_data.get("picklistValues"):
                field.picklist_values = [
                    pv.get("value") for pv in field_data["picklistValues"]
                    if pv.get("active", True)
                ]

            # Reference
            if field_data.get("referenceTo"):
                refs = field_data["referenceTo"]
                field.reference_to = refs[0] if refs else None
                field.relationship_name = field_data.get("relationshipName")

            result.append(field)

        return result

    def _analyze_relationships(self, describe) -> List[RelationshipInfo]:
        """Analisa relacionamentos do objeto"""
        relationships = []

        # Relacionamentos pai (campos de lookup)
        for field_data in describe.fields:
            if field_data.get("type") == "reference" and field_data.get("referenceTo"):
                for ref_object in field_data["referenceTo"]:
                    relationships.append(RelationshipInfo(
                        name=field_data.get("relationshipName", ""),
                        related_object=ref_object,
                        relationship_type=RelationshipType.LOOKUP.value,
                        field_name=field_data.get("name", ""),
                        is_child=False
                    ))

        # Relacionamentos filho (child relationships)
        for child_rel in describe.child_relationships:
            if child_rel.get("relationshipName"):
                relationships.append(RelationshipInfo(
                    name=child_rel.get("relationshipName", ""),
                    related_object=child_rel.get("childSObject", ""),
                    relationship_type=RelationshipType.LOOKUP.value,
                    field_name=child_rel.get("field", ""),
                    is_child=True,
                    child_object=child_rel.get("childSObject"),
                    is_cascade_delete=child_rel.get("cascadeDelete", False)
                ))

        return relationships

    def _analyze_record_types(
        self,
        record_types: List[Dict[str, Any]]
    ) -> List[RecordTypeInfo]:
        """Analisa record types"""
        result = []

        for rt in record_types:
            result.append(RecordTypeInfo(
                id=rt.get("recordTypeId", ""),
                name=rt.get("name", ""),
                developer_name=rt.get("developerName", ""),
                is_active=rt.get("active", True),
                is_default=rt.get("defaultRecordTypeMapping", False),
                is_master=rt.get("master", False)
            ))

        return result

    async def _get_triggers(self, sobject: str) -> List[TriggerInfo]:
        """Obtem triggers do objeto"""
        try:
            from ..tooling_client import ToolingClient
            tooling = ToolingClient(self.sf)

            triggers = await tooling.list_apex_triggers(sobject=sobject)

            return [
                TriggerInfo(
                    name=t.name,
                    is_active=t.status == "Active",
                    events=[
                        event for event, is_used in [
                            ("before insert", t.usage_before_insert),
                            ("after insert", t.usage_after_insert),
                            ("before update", t.usage_before_update),
                            ("after update", t.usage_after_update),
                            ("before delete", t.usage_before_delete),
                            ("after delete", t.usage_after_delete),
                            ("after undelete", t.usage_after_undelete)
                        ] if is_used
                    ],
                    body=t.body
                )
                for t in triggers
            ]
        except Exception as e:
            logger.warning(f"Erro ao obter triggers: {e}")
            return []

    async def _get_validation_rules(self, sobject: str) -> List[ValidationRuleInfo]:
        """Obtem validation rules do objeto"""
        try:
            from ..metadata_client import MetadataClient
            metadata = MetadataClient(self.sf)

            # Listar validation rules
            rules_list = await metadata.list_metadata("ValidationRule", folder=None)

            # Filtrar pelo objeto
            object_rules = [
                r for r in rules_list
                if r.get("fullName", "").startswith(f"{sobject}.")
            ]

            if not object_rules:
                return []

            # Ler detalhes
            full_names = [r["fullName"] for r in object_rules]
            rules_data = await metadata.read_metadata("ValidationRule", full_names)

            return [
                ValidationRuleInfo(
                    name=r.get("fullName", "").split(".")[-1],
                    active=r.get("active", "true").lower() == "true",
                    formula=r.get("errorConditionFormula", ""),
                    error_message=r.get("errorMessage", ""),
                    error_display_field=r.get("errorDisplayField"),
                    description=r.get("description")
                )
                for r in rules_data
            ]
        except Exception as e:
            logger.warning(f"Erro ao obter validation rules: {e}")
            return []

    async def list_all_objects(
        self,
        include_standard: bool = True,
        include_custom: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Lista todos os objetos da org

        Args:
            include_standard: Incluir objetos padrao
            include_custom: Incluir objetos customizados

        Returns:
            Lista de objetos com informacoes basicas
        """
        describe = await self.sf.describe_global()
        sobjects = describe.get("sobjects", [])

        result = []
        for obj in sobjects:
            is_custom = obj.get("custom", False)

            if is_custom and not include_custom:
                continue
            if not is_custom and not include_standard:
                continue

            result.append({
                "name": obj.get("name"),
                "label": obj.get("label"),
                "label_plural": obj.get("labelPlural"),
                "key_prefix": obj.get("keyPrefix"),
                "is_custom": is_custom,
                "is_queryable": obj.get("queryable", False),
                "is_createable": obj.get("createable", False),
                "is_updateable": obj.get("updateable", False),
                "is_deletable": obj.get("deletable", False)
            })

        return result

    async def get_field_usage(
        self,
        sobject: str,
        sample_size: int = 1000
    ) -> Dict[str, Dict[str, Any]]:
        """
        Analisa uso dos campos (porcentagem preenchida)

        Args:
            sobject: Nome do objeto
            sample_size: Tamanho da amostra

        Returns:
            Dict com estatisticas por campo
        """
        # Obter campos
        describe = await self.sf.describe(sobject)
        fields = [
            f["name"] for f in describe.fields
            if f.get("type") not in ("address", "location")
        ]

        # Buscar amostra
        field_list = ", ".join(fields[:50])  # Limite de campos por query
        soql = f"SELECT {field_list} FROM {sobject} LIMIT {sample_size}"

        try:
            result = await self.sf.query(soql)
            records = result.records
        except Exception as e:
            logger.warning(f"Erro ao buscar amostra: {e}")
            return {}

        # Calcular uso
        usage = {}
        total_records = len(records)

        if total_records == 0:
            return usage

        for field_name in fields[:50]:
            filled = sum(
                1 for r in records
                if r.get(field_name) is not None and r.get(field_name) != ""
            )
            usage[field_name] = {
                "filled_count": filled,
                "total_count": total_records,
                "usage_percentage": round(filled / total_records * 100, 2)
            }

        return usage

    async def generate_documentation(
        self,
        sobject: str,
        format: str = "markdown"
    ) -> str:
        """
        Gera documentacao do objeto

        Args:
            sobject: Nome do objeto
            format: Formato (markdown, html)

        Returns:
            Documentacao formatada
        """
        analysis = await self.analyze(sobject)

        if format == "markdown":
            return self._generate_markdown_doc(analysis)
        else:
            raise ValueError(f"Formato nao suportado: {format}")

    def _generate_markdown_doc(self, analysis: ObjectAnalysis) -> str:
        """Gera documentacao em Markdown"""
        lines = []

        # Cabecalho
        lines.append(f"# {analysis.label} ({analysis.name})")
        lines.append("")
        lines.append(f"**API Name:** `{analysis.api_name}`")
        lines.append(f"**Key Prefix:** `{analysis.key_prefix}`")
        lines.append(f"**Custom:** {'Sim' if analysis.is_custom else 'Nao'}")
        lines.append("")

        # Metricas
        lines.append("## Metricas")
        lines.append("")
        lines.append(f"- Total de campos: {analysis.total_fields}")
        lines.append(f"- Campos customizados: {analysis.custom_fields}")
        lines.append(f"- Campos obrigatorios: {analysis.required_fields}")
        lines.append(f"- Campos formula: {analysis.formula_fields}")
        lines.append(f"- Record Types: {len(analysis.record_types)}")
        lines.append(f"- Validation Rules: {len(analysis.validation_rules)}")
        lines.append(f"- Triggers: {len(analysis.triggers)}")
        lines.append("")

        # Campos
        lines.append("## Campos")
        lines.append("")
        lines.append("| Campo | Label | Tipo | Obrigatorio | Custom |")
        lines.append("|-------|-------|------|-------------|--------|")

        for field in analysis.fields:
            required = "Sim" if field.is_required else "Nao"
            custom = "Sim" if field.is_custom else "Nao"
            lines.append(f"| {field.name} | {field.label} | {field.type} | {required} | {custom} |")

        lines.append("")

        # Relacionamentos
        if analysis.relationships:
            lines.append("## Relacionamentos")
            lines.append("")
            lines.append("| Nome | Objeto Relacionado | Tipo | Direcao |")
            lines.append("|------|-------------------|------|---------|")

            for rel in analysis.relationships:
                direction = "Filho" if rel.is_child else "Pai"
                lines.append(f"| {rel.name} | {rel.related_object} | {rel.relationship_type} | {direction} |")

            lines.append("")

        # Record Types
        if analysis.record_types:
            lines.append("## Record Types")
            lines.append("")
            for rt in analysis.record_types:
                if not rt.is_master:
                    default = " (padrao)" if rt.is_default else ""
                    active = "" if rt.is_active else " [inativo]"
                    lines.append(f"- **{rt.name}**{default}{active}")

            lines.append("")

        # Validation Rules
        if analysis.validation_rules:
            lines.append("## Validation Rules")
            lines.append("")
            for vr in analysis.validation_rules:
                status = "Ativa" if vr.active else "Inativa"
                lines.append(f"### {vr.name} ({status})")
                lines.append("")
                lines.append(f"**Mensagem:** {vr.error_message}")
                lines.append("")
                lines.append("```")
                lines.append(vr.formula)
                lines.append("```")
                lines.append("")

        # Triggers
        if analysis.triggers:
            lines.append("## Triggers")
            lines.append("")
            for trigger in analysis.triggers:
                status = "Ativo" if trigger.is_active else "Inativo"
                events = ", ".join(trigger.events)
                lines.append(f"- **{trigger.name}** ({status}): {events}")

            lines.append("")

        # Dependencias
        if analysis.depends_on:
            lines.append("## Dependencias")
            lines.append("")
            lines.append("Este objeto depende de:")
            for dep in analysis.depends_on:
                lines.append(f"- {dep}")
            lines.append("")

        # Rodape
        lines.append("---")
        lines.append(f"*Analise gerada em: {analysis.analyzed_at.strftime('%Y-%m-%d %H:%M:%S')}*")

        return "\n".join(lines)

    async def compare_objects(
        self,
        sobject1: str,
        sobject2: str
    ) -> Dict[str, Any]:
        """
        Compara dois objetos

        Args:
            sobject1: Primeiro objeto
            sobject2: Segundo objeto

        Returns:
            Comparacao detalhada
        """
        analysis1 = await self.analyze(sobject1)
        analysis2 = await self.analyze(sobject2)

        fields1 = {f.name for f in analysis1.fields}
        fields2 = {f.name for f in analysis2.fields}

        return {
            "object1": sobject1,
            "object2": sobject2,
            "common_fields": list(fields1 & fields2),
            "only_in_object1": list(fields1 - fields2),
            "only_in_object2": list(fields2 - fields1),
            "metrics_comparison": {
                sobject1: {
                    "total_fields": analysis1.total_fields,
                    "custom_fields": analysis1.custom_fields,
                    "validation_rules": len(analysis1.validation_rules),
                    "triggers": len(analysis1.triggers)
                },
                sobject2: {
                    "total_fields": analysis2.total_fields,
                    "custom_fields": analysis2.custom_fields,
                    "validation_rules": len(analysis2.validation_rules),
                    "triggers": len(analysis2.triggers)
                }
            }
        }

    def clear_cache(self, sobject: Optional[str] = None):
        """Limpa cache de analises"""
        if sobject:
            self._cache.pop(sobject, None)
        else:
            self._cache.clear()
