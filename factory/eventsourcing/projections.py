# -*- coding: utf-8 -*-
"""
Projections - Plataforma E
==========================

Read model projections for event sourcing.

Issue #448: POC: Event Sourcing para Audit Trail
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Type
from threading import Lock

from .event_store import Event, EventStore


class Projection(ABC):
    """
    Base class for read model projections.

    A projection builds a read-optimized view from events.
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """Projection name."""
        pass

    @abstractmethod
    def handle(self, event: Event) -> None:
        """Handle an event."""
        pass

    @abstractmethod
    def get_state(self) -> Dict[str, Any]:
        """Get current projection state."""
        pass

    @abstractmethod
    def reset(self) -> None:
        """Reset projection state."""
        pass


class StorySummaryProjection(Projection):
    """
    Projection: Story summary by status.

    Maintains counts of stories in each status.
    """

    def __init__(self):
        self._lock = Lock()
        self._stories: Dict[str, Dict[str, Any]] = {}
        self._by_status: Dict[str, int] = {
            "backlog": 0,
            "ready": 0,
            "in_progress": 0,
            "review": 0,
            "testing": 0,
            "done": 0,
        }
        self._total_points = 0
        self._completed_points = 0
        self._last_updated: Optional[datetime] = None

    @property
    def name(self) -> str:
        return "story_summary"

    def handle(self, event: Event) -> None:
        """Handle story events."""
        if event.aggregate_type != "story":
            return

        with self._lock:
            handler_name = f"_on_{event.event_type.replace('.', '_')}"
            handler = getattr(self, handler_name, None)

            if handler:
                handler(event)

            self._last_updated = event.timestamp

    def _on_story_created(self, event: Event) -> None:
        story_id = event.aggregate_id
        self._stories[story_id] = {
            "id": story_id,
            "title": event.data.get("title", ""),
            "status": "backlog",
            "points": event.data.get("story_points", 0) or 0,
        }
        self._by_status["backlog"] += 1
        self._total_points += event.data.get("story_points", 0) or 0

    def _on_story_moved(self, event: Event) -> None:
        story_id = event.aggregate_id
        old_status = event.data.get("old_status")
        new_status = event.data.get("new_status")

        if story_id in self._stories:
            story = self._stories[story_id]
            points = story.get("points", 0)

            if old_status in self._by_status:
                self._by_status[old_status] -= 1
            if new_status in self._by_status:
                self._by_status[new_status] += 1

            # Track completed points
            if new_status == "done" and old_status != "done":
                self._completed_points += points
            elif old_status == "done" and new_status != "done":
                self._completed_points -= points

            story["status"] = new_status

    def _on_story_deleted(self, event: Event) -> None:
        story_id = event.aggregate_id
        if story_id in self._stories:
            story = self._stories[story_id]
            status = story.get("status", "backlog")
            points = story.get("points", 0)

            if status in self._by_status:
                self._by_status[status] -= 1
            self._total_points -= points

            if status == "done":
                self._completed_points -= points

            del self._stories[story_id]

    def get_state(self) -> Dict[str, Any]:
        """Get projection state."""
        with self._lock:
            return {
                "total_stories": len(self._stories),
                "by_status": dict(self._by_status),
                "total_points": self._total_points,
                "completed_points": self._completed_points,
                "velocity": self._completed_points,
                "last_updated": self._last_updated.isoformat() if self._last_updated else None,
            }

    def get_stories_by_status(self, status: str) -> List[Dict[str, Any]]:
        """Get stories by status."""
        with self._lock:
            return [
                story for story in self._stories.values()
                if story.get("status") == status
            ]

    def reset(self) -> None:
        """Reset projection."""
        with self._lock:
            self._stories.clear()
            self._by_status = {
                "backlog": 0,
                "ready": 0,
                "in_progress": 0,
                "review": 0,
                "testing": 0,
                "done": 0,
            }
            self._total_points = 0
            self._completed_points = 0
            self._last_updated = None


class ActivityLogProjection(Projection):
    """
    Projection: Activity log for audit trail.

    Maintains a chronological log of all events.
    """

    def __init__(self, max_entries: int = 1000):
        self._lock = Lock()
        self._entries: List[Dict[str, Any]] = []
        self._max_entries = max_entries

    @property
    def name(self) -> str:
        return "activity_log"

    def handle(self, event: Event) -> None:
        """Log all events."""
        with self._lock:
            entry = {
                "event_id": event.event_id,
                "event_type": event.event_type,
                "aggregate_type": event.aggregate_type,
                "aggregate_id": event.aggregate_id,
                "user": event.metadata.get("user_id") or event.data.get("created_by") or "system",
                "summary": self._create_summary(event),
                "timestamp": event.timestamp.isoformat(),
            }

            self._entries.append(entry)

            # Trim old entries
            if len(self._entries) > self._max_entries:
                self._entries = self._entries[-self._max_entries:]

    def _create_summary(self, event: Event) -> str:
        """Create human-readable summary."""
        event_type = event.event_type

        if event_type == "story.created":
            return f"Created story: {event.data.get('title', 'Untitled')}"
        elif event_type == "story.moved":
            return f"Moved story from {event.data.get('old_status')} to {event.data.get('new_status')}"
        elif event_type == "story.assigned":
            return f"Assigned story to {event.data.get('new_assignee')}"
        elif event_type == "story.deleted":
            return "Deleted story"
        elif event_type == "story.updated":
            changes = event.data.get("changes", {})
            return f"Updated story: {', '.join(changes.keys())}"

        return event_type

    def get_state(self) -> Dict[str, Any]:
        """Get projection state."""
        with self._lock:
            return {
                "total_entries": len(self._entries),
                "max_entries": self._max_entries,
            }

    def get_entries(
        self,
        limit: int = 50,
        offset: int = 0,
        event_type: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """Get activity log entries."""
        with self._lock:
            entries = self._entries

            if event_type:
                entries = [e for e in entries if e["event_type"] == event_type]

            # Reverse for most recent first
            entries = list(reversed(entries))

            return entries[offset:offset + limit]

    def reset(self) -> None:
        """Reset projection."""
        with self._lock:
            self._entries.clear()


class ProjectionManager:
    """
    Manages projections and their subscription to events.
    """

    def __init__(self, event_store: Optional[EventStore] = None):
        self._event_store = event_store
        self._projections: Dict[str, Projection] = {}
        self._lock = Lock()

    def set_event_store(self, event_store: EventStore) -> None:
        """Set the event store and subscribe."""
        self._event_store = event_store
        event_store.subscribe_all(self._handle_event)

    def register(self, projection: Projection) -> None:
        """Register a projection."""
        with self._lock:
            self._projections[projection.name] = projection

    def unregister(self, name: str) -> None:
        """Unregister a projection."""
        with self._lock:
            if name in self._projections:
                del self._projections[name]

    def get(self, name: str) -> Optional[Projection]:
        """Get a projection by name."""
        return self._projections.get(name)

    def list_projections(self) -> List[str]:
        """List all registered projections."""
        return list(self._projections.keys())

    def _handle_event(self, event: Event) -> None:
        """Handle event for all projections."""
        for projection in self._projections.values():
            try:
                projection.handle(event)
            except Exception:
                pass

    def rebuild(self, name: str) -> int:
        """Rebuild a projection from event store."""
        if not self._event_store:
            raise ValueError("Event store not set")

        projection = self._projections.get(name)
        if not projection:
            raise ValueError(f"Projection '{name}' not found")

        projection.reset()

        return self._event_store.replay_all(projection.handle)

    def rebuild_all(self) -> Dict[str, int]:
        """Rebuild all projections."""
        results = {}
        for name in self._projections:
            try:
                results[name] = self.rebuild(name)
            except Exception as e:
                results[name] = -1

        return results

    def get_all_states(self) -> Dict[str, Dict[str, Any]]:
        """Get state of all projections."""
        return {
            name: projection.get_state()
            for name, projection in self._projections.items()
        }


# Singleton instance
_manager: Optional[ProjectionManager] = None


def get_projection_manager() -> ProjectionManager:
    """Get global projection manager instance."""
    global _manager
    if _manager is None:
        _manager = ProjectionManager()
        # Register default projections
        _manager.register(StorySummaryProjection())
        _manager.register(ActivityLogProjection())
    return _manager
