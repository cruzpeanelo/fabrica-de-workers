# -*- coding: utf-8 -*-
"""
Event Sourcing Module - Plataforma E
====================================

POC for Event Sourcing pattern implementation.

Issue #448: POC: Event Sourcing para Audit Trail
"""

from .event_store import (
    Event,
    EventStore,
    EventType,
    get_event_store,
)
from .aggregate import (
    Aggregate,
    AggregateRoot,
    DomainEvent,
)
from .snapshot import (
    Snapshot,
    SnapshotStore,
    get_snapshot_store,
)
from .projections import (
    Projection,
    ProjectionManager,
    get_projection_manager,
)

__all__ = [
    # Event Store
    "Event",
    "EventStore",
    "EventType",
    "get_event_store",
    # Aggregates
    "Aggregate",
    "AggregateRoot",
    "DomainEvent",
    # Snapshots
    "Snapshot",
    "SnapshotStore",
    "get_snapshot_store",
    # Projections
    "Projection",
    "ProjectionManager",
    "get_projection_manager",
]
