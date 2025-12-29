# -*- coding: utf-8 -*-
"""
RAP Service Generator
=====================
Gerador de servicos RAP (ABAP RESTful Application Programming Model).

Este modulo gera artefatos RAP completos:
- Behavior Definition (BDEF)
- Service Definition (SDEF)
- Service Binding
- Implementation Classes

Autor: Fabrica de Agentes
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class RAPScenarioType(str, Enum):
    """Tipos de cenario RAP"""
    MANAGED = "managed"
    UNMANAGED = "unmanaged"
    PROJECTION = "projection"


class RAPImplementationType(str, Enum):
    """Tipos de implementacao"""
    STANDARD = "standard"
    WITH_ADDITIONAL_SAVE = "with_additional_save"
    WITH_UNMANAGED_SAVE = "with_unmanaged_save"


@dataclass
class RAPFieldSpec:
    """Especificacao de campo para entidade RAP"""
    name: str
    abap_type: str
    is_key: bool = False
    is_readonly: bool = False
    is_mandatory: bool = False
    is_virtual: bool = False
    has_value_help: bool = False
    default_value: Optional[str] = None
    semantic: Optional[str] = None  # user, system_date, etc


@dataclass
class RAPActionSpec:
    """Especificacao de action RAP"""
    name: str
    label: str = ""
    is_instance: bool = True
    is_factory: bool = False
    parameters: List[Dict] = field(default_factory=list)  # [{"name": "p1", "type": "..."}]
    result_type: Optional[str] = None


@dataclass
class RAPDeterminationSpec:
    """Especificacao de determination"""
    name: str
    trigger: str = "on modify"  # on modify, on save
    operations: List[str] = field(default_factory=lambda: ["create"])
    fields: List[str] = field(default_factory=list)


@dataclass
class RAPValidationSpec:
    """Especificacao de validation"""
    name: str
    trigger: str = "on save"
    operations: List[str] = field(default_factory=lambda: ["create", "update"])
    fields: List[str] = field(default_factory=list)


@dataclass
class RAPAssociationSpec:
    """Especificacao de associacao/composicao"""
    name: str
    target_entity: str
    cardinality: str = "[0..*]"
    is_composition: bool = False
    binding_fields: List[tuple] = field(default_factory=list)  # [(source, target), ...]


@dataclass
class RAPEntitySpec:
    """Especificacao de entidade RAP"""
    name: str
    alias: Optional[str] = None
    table_name: str = ""
    is_root: bool = False

    # Campos
    fields: List[RAPFieldSpec] = field(default_factory=list)

    # Operacoes CRUD
    create: bool = True
    update: bool = True
    delete: bool = True

    # Features
    with_draft: bool = False
    lock_master: bool = True
    etag_field: str = "LocalLastChangedAt"

    # Comportamentos
    actions: List[RAPActionSpec] = field(default_factory=list)
    determinations: List[RAPDeterminationSpec] = field(default_factory=list)
    validations: List[RAPValidationSpec] = field(default_factory=list)

    # Associacoes
    associations: List[RAPAssociationSpec] = field(default_factory=list)

    @property
    def short_name(self) -> str:
        """Nome curto para alias"""
        return self.alias or self.name.split("_")[-1]


@dataclass
class RAPServiceSpec:
    """
    Especificacao completa para geracao de servico RAP

    Contem todas as informacoes necessarias para gerar
    um servico RAP completo.
    """
    # Identificacao
    name: str
    description: str = ""
    package: str = "$TMP"

    # Tipo de cenario
    scenario: RAPScenarioType = RAPScenarioType.MANAGED
    implementation_type: RAPImplementationType = RAPImplementationType.STANDARD

    # Entidades
    entities: List[RAPEntitySpec] = field(default_factory=list)

    # Service Definition
    service_definition_name: Optional[str] = None

    # Service Binding
    service_binding_name: Optional[str] = None
    binding_type: str = "OData V4 - UI"

    # Autorizacao
    authorization_check: str = "#CHECK"

    # Draft
    draft_enabled: bool = False
    draft_table_suffix: str = "_D"

    def validate(self) -> List[str]:
        """Valida especificacao"""
        errors = []

        if not self.name:
            errors.append("Nome do servico e obrigatorio")

        if not self.entities:
            errors.append("Servico deve ter pelo menos uma entidade")

        root_entities = [e for e in self.entities if e.is_root]
        if not root_entities:
            errors.append("Servico deve ter uma entidade raiz")
        elif len(root_entities) > 1:
            errors.append("Servico deve ter apenas uma entidade raiz")

        return errors

    @property
    def root_entity(self) -> Optional[RAPEntitySpec]:
        """Retorna entidade raiz"""
        roots = [e for e in self.entities if e.is_root]
        return roots[0] if roots else None


class RAPGenerator:
    """
    Gerador de servicos RAP para SAP S/4HANA

    Gera artefatos RAP completos:
    - Behavior Definition
    - Service Definition
    - Service Binding
    - Implementation Class skeleton

    Exemplo de uso:
    ```python
    generator = RAPGenerator()

    spec = RAPServiceSpec(
        name="Z_SALES_ORDER",
        description="Servico de Ordens de Venda",
        scenario=RAPScenarioType.MANAGED,
        entities=[
            RAPEntitySpec(
                name="ZI_SalesOrder",
                table_name="zsalesorder",
                is_root=True,
                fields=[
                    RAPFieldSpec(name="SalesOrderId", abap_type="sysuuid_x16", is_key=True),
                    RAPFieldSpec(name="Customer", abap_type="abap.char(10)"),
                    RAPFieldSpec(name="TotalAmount", abap_type="abap.dec(15,2)")
                ],
                actions=[
                    RAPActionSpec(name="approve", label="Aprovar")
                ]
            )
        ]
    )

    files = generator.generate(spec)
    ```
    """

    def __init__(self):
        """Inicializa gerador"""
        pass

    def generate(self, spec: RAPServiceSpec) -> Dict[str, str]:
        """
        Gera todos os artefatos RAP

        Args:
            spec: Especificacao do servico

        Returns:
            Dict com nome do arquivo -> conteudo

        Raises:
            ValueError: Se especificacao invalida
        """
        errors = spec.validate()
        if errors:
            raise ValueError(f"Especificacao invalida: {', '.join(errors)}")

        files = {}

        # Behavior Definition
        bdef_name = spec.root_entity.name if spec.root_entity else spec.name
        files[f"{bdef_name}.bdef.asddls"] = self._generate_behavior_definition(spec)

        # Service Definition
        sdef_name = spec.service_definition_name or f"Z{spec.name}_SD"
        files[f"{sdef_name}.sdef.asddls"] = self._generate_service_definition(spec)

        # Implementation Class skeleton
        impl_class = self._get_implementation_class_name(spec)
        files[f"{impl_class.lower()}.abap"] = self._generate_implementation_class(spec)

        # Draft tables DDL (se draft enabled)
        if spec.draft_enabled:
            for entity in spec.entities:
                draft_table = entity.table_name + spec.draft_table_suffix
                files[f"{draft_table.lower()}.asddls"] = self._generate_draft_table(entity, spec)

        return files

    def _get_implementation_class_name(self, spec: RAPServiceSpec) -> str:
        """Gera nome da classe de implementacao"""
        if spec.root_entity:
            return f"ZCL_BP_{spec.root_entity.name.upper()}"
        return f"ZCL_BP_{spec.name.upper()}"

    def _generate_behavior_definition(self, spec: RAPServiceSpec) -> str:
        """Gera Behavior Definition"""
        lines = []

        # Header
        impl_class = self._get_implementation_class_name(spec)
        scenario = spec.scenario.value

        if spec.implementation_type == RAPImplementationType.WITH_ADDITIONAL_SAVE:
            scenario += " with additional save"
        elif spec.implementation_type == RAPImplementationType.WITH_UNMANAGED_SAVE:
            scenario += " with unmanaged save"

        lines.append(f"{scenario} implementation in class {impl_class} unique;")
        lines.append("strict ( 2 );")
        lines.append("")

        # Gerar behavior para cada entidade
        for entity in spec.entities:
            entity_lines = self._generate_entity_behavior(entity, spec)
            lines.extend(entity_lines)
            lines.append("")

        return "\n".join(lines)

    def _generate_entity_behavior(self, entity: RAPEntitySpec, spec: RAPServiceSpec) -> List[str]:
        """Gera behavior para uma entidade"""
        lines = []

        # Definicao
        root_keyword = "root " if entity.is_root else ""
        lines.append(f"define {root_keyword}behavior for {entity.name} alias {entity.short_name}")
        lines.append(f"persistent table {entity.table_name}")

        # Draft
        if entity.with_draft:
            draft_table = entity.table_name + spec.draft_table_suffix
            lines.append(f"draft table {draft_table}")

        # Lock e authorization
        if entity.lock_master:
            lines.append("lock master")
        lines.append(f"authorization master ( instance )")

        # Etag
        if entity.etag_field:
            lines.append(f"etag master {entity.etag_field}")

        lines.append("{")

        # CRUD operations
        crud_ops = []
        if entity.create:
            crud_ops.append("create")
        if entity.update:
            crud_ops.append("update")
        if entity.delete:
            crud_ops.append("delete")
        if crud_ops:
            lines.append(f"  {', '.join(crud_ops)};")

        # Draft
        if entity.with_draft:
            lines.append("")
            lines.append("  draft action Edit;")
            lines.append("  draft action Activate optimized;")
            lines.append("  draft action Discard;")
            lines.append("  draft action Resume;")
            lines.append("  draft determine action Prepare;")

        # Field controls
        readonly_fields = [f.name for f in entity.fields if f.is_readonly]
        if readonly_fields:
            lines.append("")
            lines.append(f"  field ( readonly ) {', '.join(readonly_fields)};")

        mandatory_fields = [f.name for f in entity.fields if f.is_mandatory and not f.is_key]
        if mandatory_fields:
            lines.append(f"  field ( mandatory ) {', '.join(mandatory_fields)};")

        # Actions
        for action in entity.actions:
            lines.append("")
            action_line = self._generate_action(action)
            lines.append(f"  {action_line}")

        # Determinations
        for det in entity.determinations:
            lines.append("")
            det_line = self._generate_determination(det)
            lines.append(f"  {det_line}")

        # Validations
        for val in entity.validations:
            lines.append("")
            val_line = self._generate_validation(val)
            lines.append(f"  {val_line}")

        # Associations
        for assoc in entity.associations:
            lines.append("")
            assoc_keyword = "composition" if assoc.is_composition else "association"
            lines.append(f"  {assoc_keyword} {assoc.name};")

        # Mapping
        lines.append("")
        lines.append(f"  mapping for {entity.table_name}")
        lines.append("  {")
        for f in entity.fields:
            db_field = self._to_db_field_name(f.name)
            lines.append(f"    {f.name} = {db_field};")
        lines.append("  }")

        lines.append("}")

        return lines

    def _generate_action(self, action: RAPActionSpec) -> str:
        """Gera definicao de action"""
        parts = []

        if not action.is_instance:
            parts.append("static")

        if action.is_factory:
            parts.append("factory")

        parts.append("action")
        parts.append(action.name)

        # Parametros
        if action.parameters:
            param_strs = []
            for param in action.parameters:
                param_strs.append(f"{param['name']} : {param['type']}")
            parts.append(f"parameter {{ {'; '.join(param_strs)} }}")

        # Resultado
        if action.result_type:
            parts.append(f"result [1] $self")

        parts.append(";")

        return " ".join(parts)

    def _generate_determination(self, det: RAPDeterminationSpec) -> str:
        """Gera definicao de determination"""
        ops = ", ".join(det.operations)
        line = f"determination {det.name} {det.trigger} {{ {ops}; }}"
        return line

    def _generate_validation(self, val: RAPValidationSpec) -> str:
        """Gera definicao de validation"""
        ops = ", ".join(val.operations)
        line = f"validation {val.name} {val.trigger} {{ {ops}; }}"
        return line

    def _to_db_field_name(self, field_name: str) -> str:
        """Converte nome do campo para formato DB (snake_case)"""
        result = []
        for char in field_name:
            if char.isupper() and result:
                result.append("_")
            result.append(char.lower())
        return "".join(result)

    def _generate_service_definition(self, spec: RAPServiceSpec) -> str:
        """Gera Service Definition"""
        sdef_name = spec.service_definition_name or f"Z{spec.name}_SD"

        lines = [
            f"@EndUserText.label: '{spec.description}'",
            f"define service {sdef_name} {{"
        ]

        # Expor entidades
        for entity in spec.entities:
            alias = entity.short_name
            lines.append(f"  expose {entity.name} as {alias};")

        lines.append("}")

        return "\n".join(lines)

    def _generate_implementation_class(self, spec: RAPServiceSpec) -> str:
        """Gera skeleton da classe de implementacao"""
        class_name = self._get_implementation_class_name(spec)
        root = spec.root_entity

        # Determinar handler methods necessarios
        handlers = []

        if root:
            # Determinations
            for det in root.determinations:
                handlers.append(f"    METHODS {det.name} FOR DETERMINE ON MODIFY")
                handlers.append(f"      IMPORTING keys FOR {root.short_name}~{det.name}.")

            # Validations
            for val in root.validations:
                handlers.append(f"    METHODS {val.name} FOR VALIDATE ON SAVE")
                handlers.append(f"      IMPORTING keys FOR {root.short_name}~{val.name}.")

            # Actions
            for action in root.actions:
                if action.is_instance:
                    handlers.append(f"    METHODS {action.name} FOR MODIFY")
                    handlers.append(f"      IMPORTING keys FOR ACTION {root.short_name}~{action.name}.")
                else:
                    handlers.append(f"    METHODS {action.name} FOR MODIFY")
                    handlers.append(f"      IMPORTING keys FOR STATIC ACTION {root.short_name}~{action.name}.")

        handlers_str = "\n".join(handlers) if handlers else "    \" TODO: Add handler methods"

        return f'''CLASS {class_name} DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF {root.name if root else spec.name}.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS {class_name} IMPLEMENTATION.

ENDCLASS.

* Local handler class for {root.short_name if root else spec.name}
CLASS lhc_{root.short_name.lower() if root else spec.name.lower()} DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
{handlers_str}

ENDCLASS.

CLASS lhc_{root.short_name.lower() if root else spec.name.lower()} IMPLEMENTATION.

{"  METHOD " + root.determinations[0].name + "." if root and root.determinations else "  \" TODO: Implement handler methods"}
{"    \" TODO: Implement determination logic" if root and root.determinations else ""}
{"  ENDMETHOD." if root and root.determinations else ""}

ENDCLASS.
'''

    def _generate_draft_table(self, entity: RAPEntitySpec, spec: RAPServiceSpec) -> str:
        """Gera DDL para draft table"""
        draft_table = entity.table_name + spec.draft_table_suffix

        lines = [
            f"@EndUserText.label: 'Draft table for {entity.name}'",
            f"@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE",
            f"define table {draft_table} {{",
            "  key client            : abap.clnt not null;",
            f"  key {entity.fields[0].name.lower() if entity.fields else 'uuid'} : sysuuid_x16 not null;",
            "  draftentityuuid       : sysuuid_x16;",
            "  draftentityid         : abap.string(1600);",
            "  draftentitytype       : abap.string(30);",
            "  draftparentuuid       : sysuuid_x16;",
            "  draftparententityid   : abap.string(1600);",
            "  draftparententitytype : abap.string(30);",
            "  draftrootuuid         : sysuuid_x16;",
            "  draftrootentityid     : abap.string(1600);",
            "  draftrootentitytype   : abap.string(30);",
            "  draftadminuuid        : sysuuid_x16;"
        ]

        # Adicionar campos da entidade
        for f in entity.fields:
            if not f.is_key:  # Key ja adicionada
                db_name = self._to_db_field_name(f.name)
                lines.append(f"  {db_name.ljust(25)} : {f.abap_type};")

        # Campos administrativos
        lines.extend([
            "  created_by            : abap.char(12);",
            "  created_at            : timestampl;",
            "  last_changed_by       : abap.char(12);",
            "  last_changed_at       : timestampl;",
            "  local_last_changed_at : timestampl;",
            "}"
        ])

        return "\n".join(lines)

    def generate_cds_for_entity(self, entity: RAPEntitySpec) -> str:
        """
        Gera CDS View para uma entidade RAP

        Args:
            entity: Especificacao da entidade

        Returns:
            Codigo CDS
        """
        sql_view = f"Z{entity.name[:14].upper()}"

        lines = [
            f"@AbapCatalog.sqlViewName: '{sql_view}'",
            "@AbapCatalog.compiler.compareFilter: true",
            "@AbapCatalog.preserveKey: true",
            "@AccessControl.authorizationCheck: #CHECK",
            f"@EndUserText.label: '{entity.alias or entity.name}'",
            "",
            f"define view {entity.name}",
            f"  as select from {entity.table_name}",
            "{"
        ]

        # Campos
        for i, f in enumerate(entity.fields):
            is_last = (i == len(entity.fields) - 1)
            prefix = "  key " if f.is_key else "      "
            db_field = self._to_db_field_name(f.name)
            suffix = "" if is_last else ","
            lines.append(f"{prefix}{db_field} as {f.name}{suffix}")

        lines.append("}")

        return "\n".join(lines)
