# -*- coding: utf-8 -*-
"""
Base Integration Module
=======================
Classes base para integracoes com sistemas externos.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class IntegrationStatus(str, Enum):
    """Status da integracao"""
    DISCONNECTED = "disconnected"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    SYNCING = "syncing"
    ERROR = "error"


@dataclass
class SyncResult:
    """Resultado de uma sincronizacao"""
    success: bool
    items_synced: int = 0
    items_created: int = 0
    items_updated: int = 0
    items_failed: int = 0
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    details: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "items_synced": self.items_synced,
            "items_created": self.items_created,
            "items_updated": self.items_updated,
            "items_failed": self.items_failed,
            "errors": self.errors,
            "warnings": self.warnings,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "details": self.details
        }


@dataclass
class IntegrationConfig:
    """Configuracao base para integracoes"""
    enabled: bool = False
    last_sync: Optional[datetime] = None
    sync_interval_minutes: int = 30
    auto_sync: bool = False


class IntegrationBase(ABC):
    """Classe base abstrata para integracoes"""

    def __init__(self, config: IntegrationConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        return self.status == IntegrationStatus.CONNECTED

    @property
    def last_error(self) -> Optional[str]:
        return self._last_error

    @abstractmethod
    async def connect(self) -> bool:
        """Conecta ao sistema externo"""
        pass

    @abstractmethod
    async def disconnect(self) -> bool:
        """Desconecta do sistema externo"""
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """Testa a conexao"""
        pass

    @abstractmethod
    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Sincroniza stories locais para o sistema externo"""
        pass

    @abstractmethod
    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Sincroniza do sistema externo para stories locais"""
        pass

    @abstractmethod
    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook do sistema externo"""
        pass

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "status": self.status.value,
            "connected": self.is_connected,
            "last_error": self._last_error,
            "last_sync": self.config.last_sync.isoformat() if self.config.last_sync else None,
            "auto_sync": self.config.auto_sync,
            "sync_interval_minutes": self.config.sync_interval_minutes
        }


# Status mapping - mapeamento entre status interno e sistemas externos
STATUS_MAPPING_TO_INTERNAL = {
    # Jira status -> Internal status
    "jira": {
        "To Do": "backlog",
        "Open": "backlog",
        "Backlog": "backlog",
        "Ready": "ready",
        "Ready for Development": "ready",
        "In Progress": "in_progress",
        "In Development": "in_progress",
        "In Review": "review",
        "Code Review": "review",
        "Testing": "testing",
        "QA": "testing",
        "Done": "done",
        "Closed": "done",
        "Resolved": "done"
    },
    # Azure DevOps status -> Internal status
    "azure_devops": {
        "New": "backlog",
        "Approved": "ready",
        "Committed": "ready",
        "Active": "in_progress",
        "In Progress": "in_progress",
        "Resolved": "review",
        "Review": "review",
        "Testing": "testing",
        "Done": "done",
        "Closed": "done",
        "Removed": "done"
    }
}

STATUS_MAPPING_TO_EXTERNAL = {
    # Internal status -> Jira status
    "jira": {
        "backlog": "To Do",
        "ready": "Ready for Development",
        "in_progress": "In Progress",
        "review": "In Review",
        "testing": "Testing",
        "done": "Done"
    },
    # Internal status -> Azure DevOps status
    "azure_devops": {
        "backlog": "New",
        "ready": "Approved",
        "in_progress": "Active",
        "review": "Resolved",
        "testing": "Testing",
        "done": "Done"
    }
}


def map_status_to_internal(external_status: str, system: str) -> str:
    """Mapeia status externo para status interno"""
    mapping = STATUS_MAPPING_TO_INTERNAL.get(system, {})
    return mapping.get(external_status, "backlog")


def map_status_to_external(internal_status: str, system: str) -> str:
    """Mapeia status interno para status externo"""
    mapping = STATUS_MAPPING_TO_EXTERNAL.get(system, {})
    return mapping.get(internal_status, "To Do" if system == "jira" else "New")


# Priority mapping
PRIORITY_MAPPING_TO_INTERNAL = {
    "jira": {
        "Highest": "urgent",
        "High": "high",
        "Medium": "medium",
        "Low": "low",
        "Lowest": "low"
    },
    "azure_devops": {
        "1": "urgent",
        "2": "high",
        "3": "medium",
        "4": "low"
    }
}

PRIORITY_MAPPING_TO_EXTERNAL = {
    "jira": {
        "urgent": "Highest",
        "high": "High",
        "medium": "Medium",
        "low": "Low"
    },
    "azure_devops": {
        "urgent": "1",
        "high": "2",
        "medium": "3",
        "low": "4"
    }
}


def map_priority_to_internal(external_priority: str, system: str) -> str:
    """Mapeia prioridade externa para prioridade interna"""
    mapping = PRIORITY_MAPPING_TO_INTERNAL.get(system, {})
    return mapping.get(external_priority, "medium")


def map_priority_to_external(internal_priority: str, system: str) -> str:
    """Mapeia prioridade interna para prioridade externa"""
    mapping = PRIORITY_MAPPING_TO_EXTERNAL.get(system, {})
    return mapping.get(internal_priority, "Medium" if system == "jira" else "3")
