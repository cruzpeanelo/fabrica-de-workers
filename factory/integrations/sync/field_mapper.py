# -*- coding: utf-8 -*-
"""
Field Mapper
============
Mapeamento configuravel de campos entre sistemas.

Issue #364 - Terminal A
"""

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


class FieldDirection(str, Enum):
    """Direcao do mapeamento"""
    JIRA_TO_SYSTEM = "jira_to_system"
    SYSTEM_TO_JIRA = "system_to_jira"
    BIDIRECTIONAL = "bidirectional"


@dataclass
class FieldMapping:
    """
    Mapeamento de um campo.

    Attributes:
        jira_field: Nome do campo no Jira
        system_field: Nome do campo no sistema
        direction: Direcao da sincronizacao
        transform: Funcao de transformacao (opcional)
        required: Se o campo e obrigatorio
        default: Valor padrao se campo vazio
    """
    jira_field: str
    system_field: str
    direction: FieldDirection = FieldDirection.BIDIRECTIONAL
    transform: Optional[Callable[[Any], Any]] = None
    reverse_transform: Optional[Callable[[Any], Any]] = None
    required: bool = False
    default: Any = None

    def map_to_system(self, jira_value: Any) -> Any:
        """Mapeia valor do Jira para o sistema"""
        if jira_value is None:
            return self.default
        if self.transform:
            return self.transform(jira_value)
        return jira_value

    def map_to_jira(self, system_value: Any) -> Any:
        """Mapeia valor do sistema para o Jira"""
        if system_value is None:
            return self.default
        if self.reverse_transform:
            return self.reverse_transform(system_value)
        return system_value


# Transformadores comuns
def jira_status_to_system(jira_status: str) -> str:
    """Converte status do Jira para status do sistema"""
    mapping = {
        "To Do": "backlog",
        "In Progress": "in_progress",
        "In Review": "review",
        "Testing": "testing",
        "Done": "done",
        "Closed": "done",
        "Open": "ready",
        "Reopened": "in_progress"
    }
    return mapping.get(jira_status, "backlog")


def system_status_to_jira(system_status: str) -> str:
    """Converte status do sistema para status do Jira"""
    mapping = {
        "backlog": "To Do",
        "ready": "Open",
        "in_progress": "In Progress",
        "review": "In Review",
        "testing": "Testing",
        "done": "Done"
    }
    return mapping.get(system_status, "To Do")


def jira_priority_to_system(jira_priority: str) -> str:
    """Converte prioridade do Jira para o sistema"""
    mapping = {
        "Highest": "urgent",
        "High": "high",
        "Medium": "medium",
        "Low": "low",
        "Lowest": "low"
    }
    return mapping.get(jira_priority, "medium")


def system_priority_to_jira(system_priority: str) -> str:
    """Converte prioridade do sistema para Jira"""
    mapping = {
        "urgent": "Highest",
        "high": "High",
        "medium": "Medium",
        "low": "Low"
    }
    return mapping.get(system_priority, "Medium")


def jira_story_points_to_system(value: Any) -> int:
    """Extrai story points do campo customizado do Jira"""
    if isinstance(value, (int, float)):
        return int(value)
    if isinstance(value, str) and value.isdigit():
        return int(value)
    return 0


def extract_jira_description(fields: Dict) -> str:
    """Extrai descricao do formato Atlassian Document"""
    description = fields.get("description")
    if not description:
        return ""

    # Atlassian Document Format (ADF)
    if isinstance(description, dict):
        content = description.get("content", [])
        text_parts = []
        for block in content:
            if block.get("type") == "paragraph":
                for item in block.get("content", []):
                    if item.get("type") == "text":
                        text_parts.append(item.get("text", ""))
        return "\n".join(text_parts)

    return str(description)


# Mapeamentos padrao Jira <-> Sistema
DEFAULT_JIRA_MAPPINGS: List[FieldMapping] = [
    FieldMapping(
        jira_field="key",
        system_field="external_id",
        direction=FieldDirection.JIRA_TO_SYSTEM,
        required=True
    ),
    FieldMapping(
        jira_field="fields.summary",
        system_field="title",
        direction=FieldDirection.BIDIRECTIONAL,
        required=True
    ),
    FieldMapping(
        jira_field="fields.description",
        system_field="description",
        direction=FieldDirection.BIDIRECTIONAL,
        transform=extract_jira_description
    ),
    FieldMapping(
        jira_field="fields.status.name",
        system_field="status",
        direction=FieldDirection.BIDIRECTIONAL,
        transform=jira_status_to_system,
        reverse_transform=system_status_to_jira
    ),
    FieldMapping(
        jira_field="fields.priority.name",
        system_field="priority",
        direction=FieldDirection.BIDIRECTIONAL,
        transform=jira_priority_to_system,
        reverse_transform=system_priority_to_jira
    ),
    FieldMapping(
        jira_field="fields.assignee.displayName",
        system_field="assigned_to",
        direction=FieldDirection.JIRA_TO_SYSTEM
    ),
    FieldMapping(
        jira_field="fields.reporter.displayName",
        system_field="created_by",
        direction=FieldDirection.JIRA_TO_SYSTEM
    ),
    FieldMapping(
        jira_field="fields.labels",
        system_field="tags",
        direction=FieldDirection.BIDIRECTIONAL
    ),
    FieldMapping(
        jira_field="fields.customfield_10016",  # Story Points (padrao)
        system_field="story_points",
        direction=FieldDirection.BIDIRECTIONAL,
        transform=jira_story_points_to_system
    ),
    FieldMapping(
        jira_field="fields.created",
        system_field="created_at",
        direction=FieldDirection.JIRA_TO_SYSTEM
    ),
    FieldMapping(
        jira_field="fields.updated",
        system_field="updated_at",
        direction=FieldDirection.JIRA_TO_SYSTEM
    )
]


class FieldMapper:
    """
    Mapeador de campos entre Jira e Sistema.

    Exemplo:
        mapper = FieldMapper()
        mapper.add_mapping(FieldMapping(...))

        # Mapear issue Jira para Story
        story_data = mapper.map_to_system(jira_issue)

        # Mapear Story para Jira
        jira_data = mapper.map_to_jira(story)
    """

    def __init__(self, mappings: Optional[List[FieldMapping]] = None):
        self._mappings = mappings or DEFAULT_JIRA_MAPPINGS.copy()
        self._jira_to_system_cache: Dict[str, FieldMapping] = {}
        self._system_to_jira_cache: Dict[str, FieldMapping] = {}
        self._build_cache()

    def _build_cache(self):
        """Constroi cache de mapeamentos"""
        for mapping in self._mappings:
            if mapping.direction in (
                FieldDirection.JIRA_TO_SYSTEM,
                FieldDirection.BIDIRECTIONAL
            ):
                self._jira_to_system_cache[mapping.jira_field] = mapping

            if mapping.direction in (
                FieldDirection.SYSTEM_TO_JIRA,
                FieldDirection.BIDIRECTIONAL
            ):
                self._system_to_jira_cache[mapping.system_field] = mapping

    def add_mapping(self, mapping: FieldMapping):
        """Adiciona mapeamento"""
        self._mappings.append(mapping)
        self._build_cache()

    def remove_mapping(self, jira_field: str = None, system_field: str = None):
        """Remove mapeamento"""
        self._mappings = [
            m for m in self._mappings
            if (jira_field is None or m.jira_field != jira_field) and
               (system_field is None or m.system_field != system_field)
        ]
        self._build_cache()

    def _get_nested_value(self, obj: Dict, path: str) -> Any:
        """Obtem valor de campo aninhado (ex: fields.status.name)"""
        parts = path.split(".")
        current = obj
        for part in parts:
            if isinstance(current, dict):
                current = current.get(part)
            else:
                return None
            if current is None:
                return None
        return current

    def _set_nested_value(self, obj: Dict, path: str, value: Any):
        """Define valor em campo aninhado"""
        parts = path.split(".")
        current = obj
        for part in parts[:-1]:
            if part not in current:
                current[part] = {}
            current = current[part]
        current[parts[-1]] = value

    def map_to_system(self, jira_issue: Dict) -> Dict[str, Any]:
        """
        Mapeia issue do Jira para dados do sistema.

        Args:
            jira_issue: Issue do Jira

        Returns:
            Dicionario com campos mapeados para o sistema
        """
        result = {}

        for jira_field, mapping in self._jira_to_system_cache.items():
            jira_value = self._get_nested_value(jira_issue, jira_field)

            if jira_value is None and mapping.required:
                logger.warning(f"Campo obrigatorio ausente: {jira_field}")
                continue

            system_value = mapping.map_to_system(jira_value)
            result[mapping.system_field] = system_value

        return result

    def map_to_jira(self, system_data: Dict) -> Dict[str, Any]:
        """
        Mapeia dados do sistema para formato Jira.

        Args:
            system_data: Dados do sistema (Story)

        Returns:
            Dicionario com campos no formato Jira
        """
        result = {"fields": {}}

        for system_field, mapping in self._system_to_jira_cache.items():
            system_value = system_data.get(system_field)

            if system_value is None and mapping.required:
                logger.warning(f"Campo obrigatorio ausente: {system_field}")
                continue

            jira_value = mapping.map_to_jira(system_value)

            # Remove o prefixo 'fields.' para construir o payload
            jira_field = mapping.jira_field
            if jira_field.startswith("fields."):
                jira_field = jira_field[7:]

            self._set_nested_value(result["fields"], jira_field, jira_value)

        return result

    def get_mappings(self) -> List[FieldMapping]:
        """Retorna lista de mapeamentos"""
        return self._mappings.copy()

    def get_mapping_summary(self) -> List[Dict]:
        """Retorna resumo dos mapeamentos"""
        return [
            {
                "jira_field": m.jira_field,
                "system_field": m.system_field,
                "direction": m.direction.value,
                "required": m.required
            }
            for m in self._mappings
        ]
