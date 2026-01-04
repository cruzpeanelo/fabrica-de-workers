# -*- coding: utf-8 -*-
"""
Aggregate - Plataforma E
========================

Base classes for aggregates in event sourcing.

Issue #448: POC: Event Sourcing para Audit Trail
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, Type
import uuid

from .event_store import Event, EventStore


@dataclass
class DomainEvent:
    """Base class for domain events."""
    event_type: str
    data: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.utcnow)

    def to_event(
        self,
        aggregate_type: str,
        aggregate_id: str,
        version: int,
    ) -> Event:
        """Convert to Event for storage."""
        return Event.create(
            event_type=self.event_type,
            aggregate_type=aggregate_type,
            aggregate_id=aggregate_id,
            data=self.data,
            metadata=self.metadata,
            version=version,
        )


class Aggregate(ABC):
    """
    Base class for aggregates.

    An aggregate is a cluster of domain objects that can be treated
    as a single unit for data changes.
    """

    def __init__(self, aggregate_id: Optional[str] = None):
        self._id = aggregate_id or str(uuid.uuid4())
        self._version = 0
        self._pending_events: List[DomainEvent] = []

    @property
    def id(self) -> str:
        """Get aggregate ID."""
        return self._id

    @property
    def version(self) -> int:
        """Get current version."""
        return self._version

    @property
    @abstractmethod
    def aggregate_type(self) -> str:
        """Get aggregate type name."""
        pass

    def _apply_event(self, event: DomainEvent) -> None:
        """Apply a domain event."""
        self._version += 1
        self._pending_events.append(event)
        self._when(event)

    @abstractmethod
    def _when(self, event: DomainEvent) -> None:
        """Handle event application (to be overridden)."""
        pass

    def get_pending_events(self) -> List[DomainEvent]:
        """Get pending uncommitted events."""
        return list(self._pending_events)

    def clear_pending_events(self) -> None:
        """Clear pending events after commit."""
        self._pending_events.clear()

    def load_from_history(self, events: List[Event]) -> None:
        """Reconstruct aggregate from event history."""
        for event in events:
            domain_event = DomainEvent(
                event_type=event.event_type,
                data=event.data,
                metadata=event.metadata,
                timestamp=event.timestamp,
            )
            self._version = event.version
            self._when(domain_event)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary (for snapshots)."""
        return {
            "id": self._id,
            "version": self._version,
            "aggregate_type": self.aggregate_type,
        }


class AggregateRoot(Aggregate):
    """
    Aggregate root with repository support.

    Provides save/load functionality using an event store.
    """

    def __init__(self, aggregate_id: Optional[str] = None, event_store: Optional[EventStore] = None):
        super().__init__(aggregate_id)
        self._event_store = event_store

    def set_event_store(self, event_store: EventStore) -> None:
        """Set the event store."""
        self._event_store = event_store

    def save(self) -> int:
        """Save pending events to the store."""
        if not self._event_store:
            raise ValueError("Event store not set")

        pending = self.get_pending_events()
        if not pending:
            return 0

        events = []
        for event in pending:
            events.append(event.to_event(
                aggregate_type=self.aggregate_type,
                aggregate_id=self._id,
                version=event.data.get("version", self._version),
            ))

        self._event_store.append_batch(events)
        count = len(pending)
        self.clear_pending_events()

        return count

    def load(self) -> None:
        """Load aggregate from event store."""
        if not self._event_store:
            raise ValueError("Event store not set")

        events = self._event_store.get_all_events(
            aggregate_type=self.aggregate_type,
            aggregate_id=self._id,
        )

        self.load_from_history(events)


# Example Aggregate: Story

class StoryAggregate(AggregateRoot):
    """
    Story aggregate for event sourcing.

    Tracks all changes to a story through events.
    """

    def __init__(
        self,
        story_id: Optional[str] = None,
        event_store: Optional[EventStore] = None,
    ):
        super().__init__(story_id, event_store)
        self._title = ""
        self._description = ""
        self._status = "backlog"
        self._assigned_to: Optional[str] = None
        self._story_points: Optional[int] = None
        self._created_at: Optional[datetime] = None
        self._is_deleted = False

    @property
    def aggregate_type(self) -> str:
        return "story"

    @property
    def title(self) -> str:
        return self._title

    @property
    def status(self) -> str:
        return self._status

    @property
    def assigned_to(self) -> Optional[str]:
        return self._assigned_to

    @property
    def is_deleted(self) -> bool:
        return self._is_deleted

    # Commands (cause events)

    def create(
        self,
        title: str,
        description: str = "",
        story_points: Optional[int] = None,
        created_by: Optional[str] = None,
    ) -> None:
        """Create a new story."""
        event = DomainEvent(
            event_type="story.created",
            data={
                "title": title,
                "description": description,
                "story_points": story_points,
                "created_by": created_by,
            },
        )
        self._apply_event(event)

    def update(
        self,
        title: Optional[str] = None,
        description: Optional[str] = None,
        story_points: Optional[int] = None,
        updated_by: Optional[str] = None,
    ) -> None:
        """Update story details."""
        changes = {}
        if title is not None:
            changes["title"] = title
        if description is not None:
            changes["description"] = description
        if story_points is not None:
            changes["story_points"] = story_points

        if not changes:
            return

        event = DomainEvent(
            event_type="story.updated",
            data={"changes": changes, "updated_by": updated_by},
        )
        self._apply_event(event)

    def move(self, new_status: str, moved_by: Optional[str] = None) -> None:
        """Move story to a new status."""
        event = DomainEvent(
            event_type="story.moved",
            data={
                "old_status": self._status,
                "new_status": new_status,
                "moved_by": moved_by,
            },
        )
        self._apply_event(event)

    def assign(self, user_id: str, assigned_by: Optional[str] = None) -> None:
        """Assign story to a user."""
        event = DomainEvent(
            event_type="story.assigned",
            data={
                "old_assignee": self._assigned_to,
                "new_assignee": user_id,
                "assigned_by": assigned_by,
            },
        )
        self._apply_event(event)

    def delete(self, deleted_by: Optional[str] = None) -> None:
        """Delete the story."""
        event = DomainEvent(
            event_type="story.deleted",
            data={"deleted_by": deleted_by},
        )
        self._apply_event(event)

    # Event handlers

    def _when(self, event: DomainEvent) -> None:
        """Apply event to aggregate state."""
        handler_name = f"_on_{event.event_type.replace('.', '_')}"
        handler = getattr(self, handler_name, None)

        if handler:
            handler(event)

    def _on_story_created(self, event: DomainEvent) -> None:
        self._title = event.data.get("title", "")
        self._description = event.data.get("description", "")
        self._story_points = event.data.get("story_points")
        self._status = "backlog"
        self._created_at = event.timestamp

    def _on_story_updated(self, event: DomainEvent) -> None:
        changes = event.data.get("changes", {})
        if "title" in changes:
            self._title = changes["title"]
        if "description" in changes:
            self._description = changes["description"]
        if "story_points" in changes:
            self._story_points = changes["story_points"]

    def _on_story_moved(self, event: DomainEvent) -> None:
        self._status = event.data.get("new_status", self._status)

    def _on_story_assigned(self, event: DomainEvent) -> None:
        self._assigned_to = event.data.get("new_assignee")

    def _on_story_deleted(self, event: DomainEvent) -> None:
        self._is_deleted = True

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        base = super().to_dict()
        base.update({
            "title": self._title,
            "description": self._description,
            "status": self._status,
            "assigned_to": self._assigned_to,
            "story_points": self._story_points,
            "is_deleted": self._is_deleted,
            "created_at": self._created_at.isoformat() if self._created_at else None,
        })
        return base
