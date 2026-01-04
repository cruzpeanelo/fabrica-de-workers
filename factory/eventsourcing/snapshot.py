# -*- coding: utf-8 -*-
"""
Snapshot Store - Plataforma E
=============================

Snapshots for aggregate state to improve replay performance.

Issue #448: POC: Event Sourcing para Audit Trail
"""

import json
import sqlite3
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional
from threading import Lock


@dataclass
class Snapshot:
    """
    A snapshot of aggregate state at a point in time.

    Snapshots allow skipping event replay for performance.
    """
    aggregate_type: str
    aggregate_id: str
    version: int
    state: Dict[str, Any]
    created_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "aggregate_type": self.aggregate_type,
            "aggregate_id": self.aggregate_id,
            "version": self.version,
            "state": self.state,
            "created_at": self.created_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, d: Dict[str, Any]) -> "Snapshot":
        """Create from dictionary."""
        return cls(
            aggregate_type=d["aggregate_type"],
            aggregate_id=d["aggregate_id"],
            version=d["version"],
            state=d["state"],
            created_at=datetime.fromisoformat(d["created_at"]) if isinstance(d["created_at"], str) else d["created_at"],
        )


class SnapshotStore:
    """
    Store for aggregate snapshots.

    Features:
    - Save snapshots at specific versions
    - Load latest snapshot for an aggregate
    - Automatic snapshot cleanup
    """

    DEFAULT_SNAPSHOT_FREQUENCY = 10  # Take snapshot every N events

    def __init__(
        self,
        db_path: Optional[Path] = None,
        snapshot_frequency: int = DEFAULT_SNAPSHOT_FREQUENCY,
    ):
        self.db_path = db_path or Path("factory/database/snapshots.db")
        self.snapshot_frequency = snapshot_frequency
        self._lock = Lock()

        self._init_db()

    def _init_db(self):
        """Initialize database."""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS snapshots (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                aggregate_type TEXT NOT NULL,
                aggregate_id TEXT NOT NULL,
                version INTEGER NOT NULL,
                state TEXT NOT NULL,
                created_at TEXT NOT NULL,
                UNIQUE(aggregate_type, aggregate_id, version)
            )
        """)
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_aggregate
            ON snapshots(aggregate_type, aggregate_id)
        """)
        conn.commit()
        conn.close()

    def save(self, snapshot: Snapshot) -> None:
        """Save a snapshot."""
        with self._lock:
            conn = sqlite3.connect(self.db_path)
            conn.execute("""
                INSERT OR REPLACE INTO snapshots
                (aggregate_type, aggregate_id, version, state, created_at)
                VALUES (?, ?, ?, ?, ?)
            """, (
                snapshot.aggregate_type,
                snapshot.aggregate_id,
                snapshot.version,
                json.dumps(snapshot.state),
                snapshot.created_at.isoformat(),
            ))
            conn.commit()
            conn.close()

    def get_latest(
        self,
        aggregate_type: str,
        aggregate_id: str,
    ) -> Optional[Snapshot]:
        """Get the latest snapshot for an aggregate."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT aggregate_type, aggregate_id, version, state, created_at
            FROM snapshots
            WHERE aggregate_type = ? AND aggregate_id = ?
            ORDER BY version DESC
            LIMIT 1
        """, (aggregate_type, aggregate_id))

        row = cursor.fetchone()
        conn.close()

        if row:
            return Snapshot(
                aggregate_type=row[0],
                aggregate_id=row[1],
                version=row[2],
                state=json.loads(row[3]),
                created_at=datetime.fromisoformat(row[4]),
            )

        return None

    def get_at_version(
        self,
        aggregate_type: str,
        aggregate_id: str,
        version: int,
    ) -> Optional[Snapshot]:
        """Get snapshot at or before a specific version."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT aggregate_type, aggregate_id, version, state, created_at
            FROM snapshots
            WHERE aggregate_type = ? AND aggregate_id = ? AND version <= ?
            ORDER BY version DESC
            LIMIT 1
        """, (aggregate_type, aggregate_id, version))

        row = cursor.fetchone()
        conn.close()

        if row:
            return Snapshot(
                aggregate_type=row[0],
                aggregate_id=row[1],
                version=row[2],
                state=json.loads(row[3]),
                created_at=datetime.fromisoformat(row[4]),
            )

        return None

    def should_snapshot(self, current_version: int, last_snapshot_version: int = 0) -> bool:
        """Check if a snapshot should be taken based on version gap."""
        return (current_version - last_snapshot_version) >= self.snapshot_frequency

    def delete_old_snapshots(
        self,
        aggregate_type: str,
        aggregate_id: str,
        keep_count: int = 3,
    ) -> int:
        """Delete old snapshots, keeping the most recent ones."""
        with self._lock:
            conn = sqlite3.connect(self.db_path)

            # Get snapshots to delete
            cursor = conn.execute("""
                SELECT id FROM snapshots
                WHERE aggregate_type = ? AND aggregate_id = ?
                ORDER BY version DESC
                LIMIT -1 OFFSET ?
            """, (aggregate_type, aggregate_id, keep_count))

            ids_to_delete = [row[0] for row in cursor.fetchall()]

            if ids_to_delete:
                placeholders = ",".join("?" * len(ids_to_delete))
                conn.execute(f"""
                    DELETE FROM snapshots
                    WHERE id IN ({placeholders})
                """, ids_to_delete)
                conn.commit()

            conn.close()

            return len(ids_to_delete)

    def get_all_for_aggregate(
        self,
        aggregate_type: str,
        aggregate_id: str,
    ) -> List[Snapshot]:
        """Get all snapshots for an aggregate."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute("""
            SELECT aggregate_type, aggregate_id, version, state, created_at
            FROM snapshots
            WHERE aggregate_type = ? AND aggregate_id = ?
            ORDER BY version
        """, (aggregate_type, aggregate_id))

        snapshots = []
        for row in cursor.fetchall():
            snapshots.append(Snapshot(
                aggregate_type=row[0],
                aggregate_id=row[1],
                version=row[2],
                state=json.loads(row[3]),
                created_at=datetime.fromisoformat(row[4]),
            ))

        conn.close()
        return snapshots

    def get_statistics(self) -> Dict[str, Any]:
        """Get snapshot store statistics."""
        conn = sqlite3.connect(self.db_path)

        # Total snapshots
        cursor = conn.execute("SELECT COUNT(*) FROM snapshots")
        total_snapshots = cursor.fetchone()[0]

        # Unique aggregates
        cursor = conn.execute("""
            SELECT COUNT(DISTINCT aggregate_type || ':' || aggregate_id)
            FROM snapshots
        """)
        unique_aggregates = cursor.fetchone()[0]

        # By aggregate type
        cursor = conn.execute("""
            SELECT aggregate_type, COUNT(*) as count
            FROM snapshots
            GROUP BY aggregate_type
        """)
        by_type = {row[0]: row[1] for row in cursor.fetchall()}

        conn.close()

        return {
            "total_snapshots": total_snapshots,
            "unique_aggregates": unique_aggregates,
            "snapshots_by_type": by_type,
            "snapshot_frequency": self.snapshot_frequency,
            "db_path": str(self.db_path),
        }

    def clear(self) -> None:
        """Clear all snapshots (for testing only)."""
        with self._lock:
            conn = sqlite3.connect(self.db_path)
            conn.execute("DELETE FROM snapshots")
            conn.commit()
            conn.close()


# Singleton instance
_store: Optional[SnapshotStore] = None


def get_snapshot_store() -> SnapshotStore:
    """Get global snapshot store instance."""
    global _store
    if _store is None:
        _store = SnapshotStore()
    return _store
