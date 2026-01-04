# -*- coding: utf-8 -*-
"""
Event Store - Plataforma E
==========================

Append-only event store for event sourcing.

Issue #448: POC: Event Sourcing para Audit Trail
"""

import json
import sqlite3
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Type
from threading import Lock


class EventType(str, Enum):
    """Standard event types."""
    # Story events
    STORY_CREATED = "story.created"
    STORY_UPDATED = "story.updated"
    STORY_DELETED = "story.deleted"
    STORY_MOVED = "story.moved"
    STORY_ASSIGNED = "story.assigned"

    # Task events
    TASK_CREATED = "task.created"
    TASK_COMPLETED = "task.completed"
    TASK_PROGRESS = "task.progress"

    # Sprint events
    SPRINT_STARTED = "sprint.started"
    SPRINT_COMPLETED = "sprint.completed"

    # User events
    USER_ACTION = "user.action"

    # System events
    SYSTEM_EVENT = "system.event"

    # Custom
    CUSTOM = "custom"


@dataclass
class Event:
    """
    Immutable domain event.

    Events are facts that happened in the past and cannot be changed.
    """
    event_id: str
    event_type: str
    aggregate_type: str
    aggregate_id: str
    data: Dict[str, Any]
    metadata: Dict[str, Any]
    version: int
    timestamp: datetime = field(default_factory=datetime.utcnow)

    @classmethod
    def create(
        cls,
        event_type: str,
        aggregate_type: str,
        aggregate_id: str,
        data: Dict[str, Any],
        metadata: Optional[Dict[str, Any]] = None,
        version: int = 1,
    ) -> "Event":
        """Create a new event."""
        return cls(
            event_id=str(uuid.uuid4()),
            event_type=event_type,
            aggregate_type=aggregate_type,
            aggregate_id=aggregate_id,
            data=data,
            metadata=metadata or {},
            version=version,
        )

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "event_id": self.event_id,
            "event_type": self.event_type,
            "aggregate_type": self.aggregate_type,
            "aggregate_id": self.aggregate_id,
            "data": self.data,
            "metadata": self.metadata,
            "version": self.version,
            "timestamp": self.timestamp.isoformat(),
        }

    @classmethod
    def from_dict(cls, d: Dict[str, Any]) -> "Event":
        """Create from dictionary."""
        return cls(
            event_id=d["event_id"],
            event_type=d["event_type"],
            aggregate_type=d["aggregate_type"],
            aggregate_id=d["aggregate_id"],
            data=d["data"],
            metadata=d.get("metadata", {}),
            version=d["version"],
            timestamp=datetime.fromisoformat(d["timestamp"]) if isinstance(d["timestamp"], str) else d["timestamp"],
        )


class EventStore:
    """
    Append-only event store.

    Features:
    - Append-only storage (immutable)
    - Event versioning per aggregate
    - Event handlers/subscriptions
    - Event replay
    """

    def __init__(self, db_path: Optional[Path] = None):
        self.db_path = db_path or Path("factory/database/event_store.db")
        self._lock = Lock()
        self._handlers: Dict[str, List[Callable[[Event], None]]] = {}
        self._global_handlers: List[Callable[[Event], None]] = []

        self._init_db()

    def _init_db(self):
        """Initialize database."""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS events (
                sequence_number INTEGER PRIMARY KEY AUTOINCREMENT,
                event_id TEXT UNIQUE NOT NULL,
                event_type TEXT NOT NULL,
                aggregate_type TEXT NOT NULL,
                aggregate_id TEXT NOT NULL,
                data TEXT NOT NULL,
                metadata TEXT,
                version INTEGER NOT NULL,
                timestamp TEXT NOT NULL
            )
        """)
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_aggregate
            ON events(aggregate_type, aggregate_id)
        """)
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_event_type ON events(event_type)
        """)
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_timestamp ON events(timestamp)
        """)
        conn.commit()
        conn.close()

    # Core Operations

    def append(self, event: Event) -> int:
        """
        Append an event to the store.

        Returns the sequence number.
        """
        with self._lock:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.execute("""
                INSERT INTO events
                (event_id, event_type, aggregate_type, aggregate_id, data, metadata, version, timestamp)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                event.event_id,
                event.event_type,
                event.aggregate_type,
                event.aggregate_id,
                json.dumps(event.data),
                json.dumps(event.metadata),
                event.version,
                event.timestamp.isoformat(),
            ))
            sequence_number = cursor.lastrowid
            conn.commit()
            conn.close()

            # Notify handlers
            self._dispatch(event)

            return sequence_number

    def append_batch(self, events: List[Event]) -> List[int]:
        """Append multiple events atomically."""
        with self._lock:
            conn = sqlite3.connect(self.db_path)
            sequence_numbers = []

            for event in events:
                cursor = conn.execute("""
                    INSERT INTO events
                    (event_id, event_type, aggregate_type, aggregate_id, data, metadata, version, timestamp)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    event.event_id,
                    event.event_type,
                    event.aggregate_type,
                    event.aggregate_id,
                    json.dumps(event.data),
                    json.dumps(event.metadata),
                    event.version,
                    event.timestamp.isoformat(),
                ))
                sequence_numbers.append(cursor.lastrowid)

            conn.commit()
            conn.close()

            # Notify handlers
            for event in events:
                self._dispatch(event)

            return sequence_numbers

    # Querying

    def get_events(
        self,
        aggregate_type: str,
        aggregate_id: str,
        from_version: int = 0,
    ) -> List[Event]:
        """Get events for an aggregate from a specific version."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT event_id, event_type, aggregate_type, aggregate_id,
                   data, metadata, version, timestamp
            FROM events
            WHERE aggregate_type = ? AND aggregate_id = ? AND version > ?
            ORDER BY version
        """, (aggregate_type, aggregate_id, from_version))

        events = []
        for row in cursor.fetchall():
            events.append(Event(
                event_id=row[0],
                event_type=row[1],
                aggregate_type=row[2],
                aggregate_id=row[3],
                data=json.loads(row[4]),
                metadata=json.loads(row[5]) if row[5] else {},
                version=row[6],
                timestamp=datetime.fromisoformat(row[7]),
            ))

        conn.close()
        return events

    def get_all_events(
        self,
        aggregate_type: str,
        aggregate_id: str,
    ) -> List[Event]:
        """Get all events for an aggregate."""
        return self.get_events(aggregate_type, aggregate_id, 0)

    def get_events_by_type(
        self,
        event_type: str,
        limit: int = 100,
        offset: int = 0,
    ) -> List[Event]:
        """Get events by event type."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT event_id, event_type, aggregate_type, aggregate_id,
                   data, metadata, version, timestamp
            FROM events
            WHERE event_type = ?
            ORDER BY timestamp DESC
            LIMIT ? OFFSET ?
        """, (event_type, limit, offset))

        events = []
        for row in cursor.fetchall():
            events.append(Event(
                event_id=row[0],
                event_type=row[1],
                aggregate_type=row[2],
                aggregate_id=row[3],
                data=json.loads(row[4]),
                metadata=json.loads(row[5]) if row[5] else {},
                version=row[6],
                timestamp=datetime.fromisoformat(row[7]),
            ))

        conn.close()
        return events

    def get_events_since(
        self,
        since: datetime,
        limit: int = 1000,
    ) -> List[Event]:
        """Get events since a timestamp."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT event_id, event_type, aggregate_type, aggregate_id,
                   data, metadata, version, timestamp
            FROM events
            WHERE timestamp >= ?
            ORDER BY timestamp
            LIMIT ?
        """, (since.isoformat(), limit))

        events = []
        for row in cursor.fetchall():
            events.append(Event(
                event_id=row[0],
                event_type=row[1],
                aggregate_type=row[2],
                aggregate_id=row[3],
                data=json.loads(row[4]),
                metadata=json.loads(row[5]) if row[5] else {},
                version=row[6],
                timestamp=datetime.fromisoformat(row[7]),
            ))

        conn.close()
        return events

    def get_latest_version(self, aggregate_type: str, aggregate_id: str) -> int:
        """Get the latest version number for an aggregate."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT MAX(version) FROM events
            WHERE aggregate_type = ? AND aggregate_id = ?
        """, (aggregate_type, aggregate_id))

        row = cursor.fetchone()
        conn.close()

        return row[0] if row[0] else 0

    def get_event_by_id(self, event_id: str) -> Optional[Event]:
        """Get a specific event by ID."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT event_id, event_type, aggregate_type, aggregate_id,
                   data, metadata, version, timestamp
            FROM events
            WHERE event_id = ?
        """, (event_id,))

        row = cursor.fetchone()
        conn.close()

        if row:
            return Event(
                event_id=row[0],
                event_type=row[1],
                aggregate_type=row[2],
                aggregate_id=row[3],
                data=json.loads(row[4]),
                metadata=json.loads(row[5]) if row[5] else {},
                version=row[6],
                timestamp=datetime.fromisoformat(row[7]),
            )

        return None

    # Event Handlers/Subscriptions

    def subscribe(
        self,
        event_type: str,
        handler: Callable[[Event], None],
    ) -> None:
        """Subscribe to events of a specific type."""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)

    def subscribe_all(self, handler: Callable[[Event], None]) -> None:
        """Subscribe to all events."""
        self._global_handlers.append(handler)

    def unsubscribe(
        self,
        event_type: str,
        handler: Callable[[Event], None],
    ) -> None:
        """Unsubscribe from events."""
        if event_type in self._handlers:
            if handler in self._handlers[event_type]:
                self._handlers[event_type].remove(handler)

    def _dispatch(self, event: Event) -> None:
        """Dispatch event to handlers."""
        # Type-specific handlers
        handlers = self._handlers.get(event.event_type, [])
        for handler in handlers:
            try:
                handler(event)
            except Exception:
                pass

        # Global handlers
        for handler in self._global_handlers:
            try:
                handler(event)
            except Exception:
                pass

    # Replay

    def replay(
        self,
        aggregate_type: str,
        aggregate_id: str,
        handler: Callable[[Event], None],
    ) -> int:
        """Replay all events for an aggregate."""
        events = self.get_all_events(aggregate_type, aggregate_id)

        for event in events:
            handler(event)

        return len(events)

    def replay_all(
        self,
        handler: Callable[[Event], None],
        from_sequence: int = 0,
        batch_size: int = 1000,
    ) -> int:
        """Replay all events from the store."""
        conn = sqlite3.connect(self.db_path)
        total_replayed = 0

        while True:
            cursor = conn.execute("""
                SELECT event_id, event_type, aggregate_type, aggregate_id,
                       data, metadata, version, timestamp
                FROM events
                WHERE sequence_number > ?
                ORDER BY sequence_number
                LIMIT ?
            """, (from_sequence, batch_size))

            rows = cursor.fetchall()
            if not rows:
                break

            for row in rows:
                event = Event(
                    event_id=row[0],
                    event_type=row[1],
                    aggregate_type=row[2],
                    aggregate_id=row[3],
                    data=json.loads(row[4]),
                    metadata=json.loads(row[5]) if row[5] else {},
                    version=row[6],
                    timestamp=datetime.fromisoformat(row[7]),
                )
                handler(event)
                total_replayed += 1

            from_sequence += len(rows)

        conn.close()
        return total_replayed

    # Statistics

    def get_statistics(self) -> Dict[str, Any]:
        """Get event store statistics."""
        conn = sqlite3.connect(self.db_path)

        # Total events
        cursor = conn.execute("SELECT COUNT(*) FROM events")
        total_events = cursor.fetchone()[0]

        # Events by type
        cursor = conn.execute("""
            SELECT event_type, COUNT(*) as count
            FROM events
            GROUP BY event_type
            ORDER BY count DESC
        """)
        events_by_type = {row[0]: row[1] for row in cursor.fetchall()}

        # Unique aggregates
        cursor = conn.execute("""
            SELECT COUNT(DISTINCT aggregate_type || ':' || aggregate_id)
            FROM events
        """)
        unique_aggregates = cursor.fetchone()[0]

        # Latest event
        cursor = conn.execute("""
            SELECT timestamp FROM events ORDER BY timestamp DESC LIMIT 1
        """)
        row = cursor.fetchone()
        latest_event = row[0] if row else None

        conn.close()

        return {
            "total_events": total_events,
            "events_by_type": events_by_type,
            "unique_aggregates": unique_aggregates,
            "latest_event_timestamp": latest_event,
            "db_path": str(self.db_path),
        }

    def clear(self) -> None:
        """Clear all events (for testing only)."""
        with self._lock:
            conn = sqlite3.connect(self.db_path)
            conn.execute("DELETE FROM events")
            conn.commit()
            conn.close()


# Singleton instance
_store: Optional[EventStore] = None


def get_event_store() -> EventStore:
    """Get global event store instance."""
    global _store
    if _store is None:
        _store = EventStore()
    return _store
