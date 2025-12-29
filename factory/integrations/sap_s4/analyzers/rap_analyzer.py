# -*- coding: utf-8 -*-
"""
RAP Service Analyzer
====================
Analisador de servicos RAP (ABAP RESTful Application Programming Model).

RAP e o modelo de programacao moderno do SAP para criar aplicacoes
transacionais e analiticas no S/4HANA.

Autor: Fabrica de Agentes
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class RAPServiceType(str, Enum):
    """Tipos de servico RAP"""
    MANAGED = "managed"              # Managed scenario (persistencia automatica)
    UNMANAGED = "unmanaged"          # Unmanaged scenario (persistencia manual)
    PROJECTION = "projection"        # Projection para consumo


class RAPBehaviorType(str, Enum):
    """Tipos de behavior definition"""
    DEFINITION = "definition"
    IMPLEMENTATION = "implementation"
    PROJECTION = "projection"


class RAPOperationType(str, Enum):
    """Tipos de operacao RAP"""
    CREATE = "create"
    UPDATE = "update"
    DELETE = "delete"
    ACTION = "action"
    FUNCTION = "function"
    DETERMINATION = "determination"
    VALIDATION = "validation"


@dataclass
class RAPAction:
    """Representa uma action RAP"""
    name: str
    is_instance: bool = True
    parameters: List[Dict[str, str]] = field(default_factory=list)
    result_type: Optional[str] = None
    is_factory: bool = False


@dataclass
class RAPDetermination:
    """Representa uma determination RAP"""
    name: str
    trigger: str  # on modify, on save
    trigger_operations: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)


@dataclass
class RAPValidation:
    """Representa uma validation RAP"""
    name: str
    trigger: str  # on save
    trigger_operations: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)


@dataclass
class RAPAssociation:
    """Representa uma associacao em RAP"""
    name: str
    target: str
    cardinality: str = "[0..1]"
    is_composition: bool = False


@dataclass
class RAPEntityBehavior:
    """Comportamento de uma entidade RAP"""
    entity_name: str
    alias: Optional[str] = None
    is_root: bool = False

    # Operacoes CRUD
    create_enabled: bool = False
    update_enabled: bool = False
    delete_enabled: bool = False

    # Features especiais
    draft_enabled: bool = False
    etag_field: Optional[str] = None
    lock_master: bool = False

    # Actions e funcoes
    actions: List[RAPAction] = field(default_factory=list)
    functions: List[RAPAction] = field(default_factory=list)

    # Determinations e validations
    determinations: List[RAPDetermination] = field(default_factory=list)
    validations: List[RAPValidation] = field(default_factory=list)

    # Associacoes
    associations: List[RAPAssociation] = field(default_factory=list)

    # Field controls
    field_readonly: List[str] = field(default_factory=list)
    field_mandatory: List[str] = field(default_factory=list)


@dataclass
class RAPServiceInfo:
    """
    Informacoes completas sobre um servico RAP

    Contem metadados do behavior definition, service definition
    e service binding.
    """
    # Identificacao
    name: str
    description: str = ""
    service_type: RAPServiceType = RAPServiceType.MANAGED

    # Behavior Definition
    behavior_definition_name: Optional[str] = None
    implementation_class: Optional[str] = None

    # Entidades
    entities: List[RAPEntityBehavior] = field(default_factory=list)
    root_entity: Optional[str] = None

    # Service Definition
    service_definition_name: Optional[str] = None
    exposed_entities: List[str] = field(default_factory=list)

    # Service Binding
    service_binding_name: Optional[str] = None
    binding_type: str = "OData V4"
    service_url: Optional[str] = None

    # Draft
    draft_enabled: bool = False
    draft_table: Optional[str] = None

    # Authorization
    authorization_check: str = "#CHECK"

    # Metadados
    created_at: Optional[datetime] = None
    modified_at: Optional[datetime] = None
    package: Optional[str] = None

    @property
    def is_managed(self) -> bool:
        """Verifica se e managed scenario"""
        return self.service_type == RAPServiceType.MANAGED

    def get_entity(self, name: str) -> Optional[RAPEntityBehavior]:
        """Busca entidade por nome"""
        for entity in self.entities:
            if entity.entity_name == name or entity.alias == name:
                return entity
        return None

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "name": self.name,
            "description": self.description,
            "service_type": self.service_type.value,
            "behavior_definition": self.behavior_definition_name,
            "implementation_class": self.implementation_class,
            "entity_count": len(self.entities),
            "root_entity": self.root_entity,
            "draft_enabled": self.draft_enabled,
            "binding_type": self.binding_type,
            "service_url": self.service_url
        }


class RAPAnalyzer:
    """
    Analisador de servicos RAP do SAP S/4HANA

    Analisa:
    - Behavior Definition (BDEF)
    - Service Definition
    - Service Binding
    - Metadata Extensions

    Exemplo de uso:
    ```python
    analyzer = RAPAnalyzer()

    # Parse de behavior definition
    service_info = analyzer.parse_behavior_definition(bdef_source)

    # Parse de service definition
    analyzer.parse_service_definition(sdef_source, service_info)

    # Validar servico
    issues = analyzer.validate_service(service_info)
    ```
    """

    # Regex patterns
    MANAGED_PATTERN = re.compile(
        r"managed\s+implementation\s+in\s+class\s+(\w+)",
        re.IGNORECASE
    )

    UNMANAGED_PATTERN = re.compile(
        r"unmanaged\s+implementation\s+in\s+class\s+(\w+)",
        re.IGNORECASE
    )

    ENTITY_PATTERN = re.compile(
        r"define\s+behavior\s+for\s+(\w+)(?:\s+alias\s+(\w+))?",
        re.IGNORECASE
    )

    ROOT_PATTERN = re.compile(
        r"define\s+root\s+behavior\s+for\s+(\w+)",
        re.IGNORECASE
    )

    ACTION_PATTERN = re.compile(
        r"(static\s+)?action\s+(\w+)(?:\s+parameter\s+(\w+))?(?:\s+result\s+\[\d+\]\s+(\w+))?",
        re.IGNORECASE
    )

    DETERMINATION_PATTERN = re.compile(
        r"determination\s+(\w+)\s+on\s+(modify|save)",
        re.IGNORECASE
    )

    VALIDATION_PATTERN = re.compile(
        r"validation\s+(\w+)\s+on\s+save",
        re.IGNORECASE
    )

    def __init__(self, odata_client=None):
        """
        Inicializa analisador

        Args:
            odata_client: Cliente OData opcional
        """
        self.odata_client = odata_client

    def parse_behavior_definition(self, source: str) -> RAPServiceInfo:
        """
        Parse de Behavior Definition

        Args:
            source: Codigo fonte BDEF

        Returns:
            RAPServiceInfo com dados extraidos
        """
        service_info = RAPServiceInfo(name="")

        # Detectar tipo (managed/unmanaged)
        managed_match = self.MANAGED_PATTERN.search(source)
        unmanaged_match = self.UNMANAGED_PATTERN.search(source)

        if managed_match:
            service_info.service_type = RAPServiceType.MANAGED
            service_info.implementation_class = managed_match.group(1)
        elif unmanaged_match:
            service_info.service_type = RAPServiceType.UNMANAGED
            service_info.implementation_class = unmanaged_match.group(1)

        # Extrair entidades
        self._extract_entities(source, service_info)

        # Extrair actions, determinations, validations
        self._extract_behaviors(source, service_info)

        return service_info

    def _extract_entities(self, source: str, service_info: RAPServiceInfo):
        """Extrai entidades do BDEF"""
        # Buscar root entity
        root_match = self.ROOT_PATTERN.search(source)
        if root_match:
            root_entity_name = root_match.group(1)
            service_info.root_entity = root_entity_name
            service_info.name = root_entity_name

            root_entity = RAPEntityBehavior(
                entity_name=root_entity_name,
                is_root=True
            )
            service_info.entities.append(root_entity)

        # Buscar outras entidades
        for match in self.ENTITY_PATTERN.finditer(source):
            entity_name = match.group(1)
            alias = match.group(2)

            # Verificar se ja foi adicionada como root
            existing = service_info.get_entity(entity_name)
            if existing:
                if alias:
                    existing.alias = alias
                continue

            entity = RAPEntityBehavior(
                entity_name=entity_name,
                alias=alias
            )
            service_info.entities.append(entity)

    def _extract_behaviors(self, source: str, service_info: RAPServiceInfo):
        """Extrai behaviors (CRUD, actions, etc)"""
        # Dividir por entidade
        entity_blocks = self._split_by_entity(source)

        for entity_name, block in entity_blocks.items():
            entity = service_info.get_entity(entity_name)
            if not entity:
                continue

            # CRUD operations
            if re.search(r"\bcreate\b", block, re.IGNORECASE):
                entity.create_enabled = True
            if re.search(r"\bupdate\b", block, re.IGNORECASE):
                entity.update_enabled = True
            if re.search(r"\bdelete\b", block, re.IGNORECASE):
                entity.delete_enabled = True

            # Draft
            if re.search(r"\bwith\s+draft\b", block, re.IGNORECASE):
                entity.draft_enabled = True
                service_info.draft_enabled = True

            # Etag
            etag_match = re.search(r"etag\s+master\s+(\w+)", block, re.IGNORECASE)
            if etag_match:
                entity.etag_field = etag_match.group(1)

            # Lock master
            if re.search(r"\block\s+master\b", block, re.IGNORECASE):
                entity.lock_master = True

            # Actions
            for action_match in self.ACTION_PATTERN.finditer(block):
                is_static = action_match.group(1) is not None
                action = RAPAction(
                    name=action_match.group(2),
                    is_instance=not is_static,
                    result_type=action_match.group(4)
                )
                entity.actions.append(action)

            # Determinations
            for det_match in self.DETERMINATION_PATTERN.finditer(block):
                determination = RAPDetermination(
                    name=det_match.group(1),
                    trigger=det_match.group(2)
                )
                entity.determinations.append(determination)

            # Validations
            for val_match in self.VALIDATION_PATTERN.finditer(block):
                validation = RAPValidation(
                    name=val_match.group(1),
                    trigger="save"
                )
                entity.validations.append(validation)

            # Field controls
            readonly_match = re.search(
                r"field\s*\(\s*readonly\s*\)\s*([^;]+)",
                block,
                re.IGNORECASE
            )
            if readonly_match:
                fields = [f.strip() for f in readonly_match.group(1).split(",")]
                entity.field_readonly = fields

            mandatory_match = re.search(
                r"field\s*\(\s*mandatory\s*\)\s*([^;]+)",
                block,
                re.IGNORECASE
            )
            if mandatory_match:
                fields = [f.strip() for f in mandatory_match.group(1).split(",")]
                entity.field_mandatory = fields

            # Associacoes/Composicoes
            assoc_pattern = re.compile(
                r"(association|composition)\s+(\w+)",
                re.IGNORECASE
            )
            for assoc_match in assoc_pattern.finditer(block):
                is_composition = assoc_match.group(1).lower() == "composition"
                association = RAPAssociation(
                    name=assoc_match.group(2),
                    target=assoc_match.group(2),
                    is_composition=is_composition
                )
                entity.associations.append(association)

    def _split_by_entity(self, source: str) -> Dict[str, str]:
        """Divide source em blocos por entidade"""
        blocks = {}

        # Pattern para encontrar inicio de cada entidade
        pattern = re.compile(
            r"define\s+(?:root\s+)?behavior\s+for\s+(\w+)[^{]*\{",
            re.IGNORECASE
        )

        matches = list(pattern.finditer(source))

        for i, match in enumerate(matches):
            entity_name = match.group(1)
            start = match.end()

            # Encontrar fim do bloco (proximo define ou fim)
            if i + 1 < len(matches):
                end = matches[i + 1].start()
            else:
                end = len(source)

            # Ajustar para encontrar } correspondente
            block = source[start:end]
            brace_count = 1
            actual_end = 0
            for j, char in enumerate(block):
                if char == "{":
                    brace_count += 1
                elif char == "}":
                    brace_count -= 1
                    if brace_count == 0:
                        actual_end = j
                        break

            blocks[entity_name] = block[:actual_end] if actual_end else block

        return blocks

    def parse_service_definition(self, source: str, service_info: RAPServiceInfo):
        """
        Parse de Service Definition

        Args:
            source: Codigo fonte SDEF
            service_info: RAPServiceInfo para atualizar
        """
        # Nome do servico
        name_match = re.search(r"define\s+service\s+(\w+)", source, re.IGNORECASE)
        if name_match:
            service_info.service_definition_name = name_match.group(1)

        # Entidades expostas
        expose_pattern = re.compile(r"expose\s+(\w+)(?:\s+as\s+(\w+))?", re.IGNORECASE)
        for match in expose_pattern.finditer(source):
            entity = match.group(1)
            service_info.exposed_entities.append(entity)

    def parse_service_binding(self, source: str, service_info: RAPServiceInfo):
        """
        Parse de Service Binding

        Args:
            source: Codigo fonte ou metadados do binding
            service_info: RAPServiceInfo para atualizar
        """
        # Nome do binding
        name_match = re.search(r"name\s*=\s*['\"]?(\w+)", source, re.IGNORECASE)
        if name_match:
            service_info.service_binding_name = name_match.group(1)

        # Tipo de binding
        if "odata_v4" in source.lower() or "odata v4" in source.lower():
            service_info.binding_type = "OData V4"
        elif "odata_v2" in source.lower() or "odata v2" in source.lower():
            service_info.binding_type = "OData V2"
        elif "ui" in source.lower():
            service_info.binding_type = "OData V4 - UI"

    def validate_service(self, service_info: RAPServiceInfo) -> List[str]:
        """
        Valida configuracao do servico RAP

        Args:
            service_info: Servico a validar

        Returns:
            Lista de problemas encontrados
        """
        issues = []

        # Verificar root entity
        if not service_info.root_entity:
            issues.append("Servico RAP deve ter uma root entity definida")

        # Verificar implementation class para managed/unmanaged
        if service_info.service_type in (RAPServiceType.MANAGED, RAPServiceType.UNMANAGED):
            if not service_info.implementation_class:
                issues.append(
                    f"Servico {service_info.service_type.value} requer implementation class"
                )

        # Verificar entidades
        for entity in service_info.entities:
            # Verificar operacoes CRUD
            if not any([entity.create_enabled, entity.update_enabled, entity.delete_enabled]):
                issues.append(
                    f"Entidade '{entity.entity_name}' nao tem operacoes CRUD habilitadas"
                )

            # Verificar etag para entidades com update
            if entity.update_enabled and not entity.etag_field:
                issues.append(
                    f"Entidade '{entity.entity_name}' com update mas sem etag definido"
                )

            # Verificar determinations/validations tem triggers
            for det in entity.determinations:
                if not det.trigger:
                    issues.append(
                        f"Determination '{det.name}' sem trigger definido"
                    )

        # Verificar draft
        if service_info.draft_enabled:
            root = service_info.get_entity(service_info.root_entity) if service_info.root_entity else None
            if root and not root.draft_enabled:
                issues.append("Draft habilitado mas root entity nao tem 'with draft'")

        return issues

    def generate_behavior_template(
        self,
        entity_name: str,
        is_managed: bool = True,
        with_draft: bool = False,
        crud: List[str] = None
    ) -> str:
        """
        Gera template de Behavior Definition

        Args:
            entity_name: Nome da entidade
            is_managed: Se True, managed scenario
            with_draft: Habilitar draft
            crud: Lista de operacoes ['create', 'update', 'delete']

        Returns:
            String com codigo BDEF
        """
        crud = crud or ['create', 'update', 'delete']

        scenario = "managed" if is_managed else "unmanaged"
        impl_class = f"ZCL_BP_{entity_name.upper()}"

        draft_clause = " with draft" if with_draft else ""
        operations = ", ".join(crud)

        template = f'''managed implementation in class {impl_class} unique
strict ( 2 );

define behavior for {entity_name} alias {entity_name.split('_')[-1]}
persistent table z{entity_name.lower()}{draft_clause}
lock master
authorization master ( instance )
etag master LocalLastChangedAt
{{
  {operations};

  field ( readonly ) CreatedAt, CreatedBy, LastChangedAt, LastChangedBy;
  field ( mandatory ) Key1;

  determination setDefaults on modify {{ create; }}

  validation validateData on save {{ create; update; }}

  mapping for z{entity_name.lower()}
  {{
    Key1 = key1;
    Field1 = field1;
    Field2 = field2;
    CreatedAt = created_at;
    CreatedBy = created_by;
    LastChangedAt = last_changed_at;
    LastChangedBy = last_changed_by;
  }}
}}'''

        return template

    def generate_service_definition(
        self,
        service_name: str,
        entities: List[str]
    ) -> str:
        """
        Gera template de Service Definition

        Args:
            service_name: Nome do servico
            entities: Lista de entidades a expor

        Returns:
            String com codigo SDEF
        """
        expose_lines = "\n  ".join([f"expose {e} as {e.split('_')[-1]};" for e in entities])

        template = f'''@EndUserText.label: 'Service Definition for {service_name}'
define service {service_name} {{
  {expose_lines}
}}'''

        return template
