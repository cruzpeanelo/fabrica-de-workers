# -*- coding: utf-8 -*-
"""
Sync Module
===========
Sistema de sincronizacao bidirecional para integracoes.

Issue #364 - Terminal A

Modulos:
- JiraSyncManager: Sincronizacao Jira <-> Sistema
- FieldMapper: Mapeamento de campos configuravel
- ConflictResolver: Resolucao de conflitos
- SyncScheduler: Agendamento de sincronizacoes
"""

from .field_mapper import (
    FieldMapper,
    FieldMapping,
    FieldDirection,
    DEFAULT_JIRA_MAPPINGS
)
from .conflict_resolver import (
    ConflictResolver,
    ConflictResolution,
    ConflictStrategy
)
from .jira_sync import (
    JiraSyncManager,
    SyncConfig,
    SyncResult,
    SyncDirection,
    SyncStatus
)
from .scheduler import (
    SyncScheduler,
    SyncJob,
    get_scheduler
)

__all__ = [
    # Field Mapper
    "FieldMapper",
    "FieldMapping",
    "FieldDirection",
    "DEFAULT_JIRA_MAPPINGS",

    # Conflict Resolver
    "ConflictResolver",
    "ConflictResolution",
    "ConflictStrategy",

    # Jira Sync
    "JiraSyncManager",
    "SyncConfig",
    "SyncResult",
    "SyncDirection",
    "SyncStatus",

    # Scheduler
    "SyncScheduler",
    "SyncJob",
    "get_scheduler"
]
