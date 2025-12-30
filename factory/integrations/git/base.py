# -*- coding: utf-8 -*-
"""
Base Integration Module Re-export
=================================

Re-exports from parent base module for convenience.
"""

from ..base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult,
    STATUS_MAPPING_TO_INTERNAL,
    STATUS_MAPPING_TO_EXTERNAL,
    PRIORITY_MAPPING_TO_INTERNAL,
    PRIORITY_MAPPING_TO_EXTERNAL,
    map_status_to_internal,
    map_status_to_external,
    map_priority_to_internal,
    map_priority_to_external,
)

__all__ = [
    "IntegrationBase",
    "IntegrationConfig",
    "IntegrationStatus",
    "SyncResult",
    "STATUS_MAPPING_TO_INTERNAL",
    "STATUS_MAPPING_TO_EXTERNAL",
    "PRIORITY_MAPPING_TO_INTERNAL",
    "PRIORITY_MAPPING_TO_EXTERNAL",
    "map_status_to_internal",
    "map_status_to_external",
    "map_priority_to_internal",
    "map_priority_to_external",
]
