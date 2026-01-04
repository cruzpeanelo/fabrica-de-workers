# -*- coding: utf-8 -*-
"""
Audit Logger - Plataforma E
===========================

Main audit logging engine.

Issue #439: API de Auditoria com Logs de Acoes
"""

import csv
import io
import json
from collections import defaultdict
from datetime import datetime, timedelta
from threading import Lock
from typing import Any, Callable, Dict, List, Optional

from .models import AuditAction, AuditLog


class AuditLogger:
    """Append-only audit logger with query capabilities."""

    DEFAULT_RETENTION_DAYS = 90

    def __init__(self, retention_days: int = DEFAULT_RETENTION_DAYS):
        self._logs: List[AuditLog] = []
        self._index_by_resource: Dict[str, List[int]] = defaultdict(list)
        self._index_by_user: Dict[str, List[int]] = defaultdict(list)
        self._lock = Lock()
        self._retention_days = retention_days
        self._listeners: List[Callable[[AuditLog], None]] = []

    def log(
        self,
        user_id: str,
        action: AuditAction,
        resource_type: str,
        resource_id: str,
        old_value: Optional[Dict[str, Any]] = None,
        new_value: Optional[Dict[str, Any]] = None,
        ip_address: str = "",
        user_agent: str = "",
        session_id: str = "",
        metadata: Optional[Dict[str, Any]] = None,
    ) -> AuditLog:
        """Log an audit event (append-only)."""
        with self._lock:
            entry = AuditLog(
                user_id=user_id,
                action=action,
                resource_type=resource_type,
                resource_id=resource_id,
                old_value=old_value,
                new_value=new_value,
                ip_address=ip_address,
                user_agent=user_agent,
                session_id=session_id,
                metadata=metadata or {},
            )

            # Append to main list
            idx = len(self._logs)
            self._logs.append(entry)

            # Update indexes
            resource_key = f"{resource_type}:{resource_id}"
            self._index_by_resource[resource_key].append(idx)
            self._index_by_user[user_id].append(idx)

            # Notify listeners
            for listener in self._listeners:
                try:
                    listener(entry)
                except Exception:
                    pass

            return entry

    def log_create(
        self,
        user_id: str,
        resource_type: str,
        resource_id: str,
        new_value: Dict[str, Any],
        **kwargs,
    ) -> AuditLog:
        """Log a CREATE action."""
        return self.log(
            user_id=user_id,
            action=AuditAction.CREATE,
            resource_type=resource_type,
            resource_id=resource_id,
            new_value=new_value,
            **kwargs,
        )

    def log_update(
        self,
        user_id: str,
        resource_type: str,
        resource_id: str,
        old_value: Dict[str, Any],
        new_value: Dict[str, Any],
        **kwargs,
    ) -> AuditLog:
        """Log an UPDATE action."""
        return self.log(
            user_id=user_id,
            action=AuditAction.UPDATE,
            resource_type=resource_type,
            resource_id=resource_id,
            old_value=old_value,
            new_value=new_value,
            **kwargs,
        )

    def log_delete(
        self,
        user_id: str,
        resource_type: str,
        resource_id: str,
        old_value: Dict[str, Any],
        **kwargs,
    ) -> AuditLog:
        """Log a DELETE action."""
        return self.log(
            user_id=user_id,
            action=AuditAction.DELETE,
            resource_type=resource_type,
            resource_id=resource_id,
            old_value=old_value,
            **kwargs,
        )

    def add_listener(self, callback: Callable[[AuditLog], None]) -> None:
        """Add a listener for audit events."""
        self._listeners.append(callback)

    def remove_listener(self, callback: Callable[[AuditLog], None]) -> None:
        """Remove a listener."""
        if callback in self._listeners:
            self._listeners.remove(callback)

    # Query methods

    def get_logs(
        self,
        user_id: Optional[str] = None,
        action: Optional[AuditAction] = None,
        resource_type: Optional[str] = None,
        resource_id: Optional[str] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None,
        limit: int = 100,
        offset: int = 0,
    ) -> List[AuditLog]:
        """Query audit logs with filters."""
        with self._lock:
            results = []

            # Start with indexed search if possible
            if resource_type and resource_id:
                key = f"{resource_type}:{resource_id}"
                indices = self._index_by_resource.get(key, [])
                candidates = [self._logs[i] for i in indices]
            elif user_id:
                indices = self._index_by_user.get(user_id, [])
                candidates = [self._logs[i] for i in indices]
            else:
                candidates = self._logs

            # Apply filters
            for log in reversed(candidates):  # Most recent first
                if user_id and log.user_id != user_id:
                    continue
                if action and log.action != action:
                    continue
                if resource_type and log.resource_type != resource_type:
                    continue
                if resource_id and log.resource_id != resource_id:
                    continue
                if date_from and log.timestamp < date_from:
                    continue
                if date_to and log.timestamp > date_to:
                    continue

                results.append(log)

            # Apply pagination
            return results[offset : offset + limit]

    def get_resource_history(
        self,
        resource_type: str,
        resource_id: str,
        limit: int = 50,
    ) -> List[AuditLog]:
        """Get complete history for a resource."""
        return self.get_logs(
            resource_type=resource_type,
            resource_id=resource_id,
            limit=limit,
        )

    def get_user_activity(
        self,
        user_id: str,
        days: int = 7,
        limit: int = 100,
    ) -> List[AuditLog]:
        """Get recent activity for a user."""
        date_from = datetime.now() - timedelta(days=days)
        return self.get_logs(
            user_id=user_id,
            date_from=date_from,
            limit=limit,
        )

    def count_logs(
        self,
        user_id: Optional[str] = None,
        action: Optional[AuditAction] = None,
        resource_type: Optional[str] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None,
    ) -> int:
        """Count logs matching filters."""
        logs = self.get_logs(
            user_id=user_id,
            action=action,
            resource_type=resource_type,
            date_from=date_from,
            date_to=date_to,
            limit=999999,
        )
        return len(logs)

    def get_statistics(
        self,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        """Get audit statistics."""
        logs = self.get_logs(
            date_from=date_from,
            date_to=date_to,
            limit=999999,
        )

        action_counts = defaultdict(int)
        resource_counts = defaultdict(int)
        user_counts = defaultdict(int)

        for log in logs:
            action_counts[log.action.value] += 1
            resource_counts[log.resource_type] += 1
            user_counts[log.user_id] += 1

        return {
            "total_logs": len(logs),
            "by_action": dict(action_counts),
            "by_resource": dict(resource_counts),
            "by_user": dict(user_counts),
            "most_active_users": sorted(
                user_counts.items(),
                key=lambda x: x[1],
                reverse=True,
            )[:10],
        }

    # Export methods

    def export_json(
        self,
        logs: Optional[List[AuditLog]] = None,
        **filter_kwargs,
    ) -> str:
        """Export logs to JSON."""
        if logs is None:
            logs = self.get_logs(**filter_kwargs)

        return json.dumps(
            [log.to_dict() for log in logs],
            indent=2,
            ensure_ascii=False,
        )

    def export_csv(
        self,
        logs: Optional[List[AuditLog]] = None,
        **filter_kwargs,
    ) -> str:
        """Export logs to CSV."""
        if logs is None:
            logs = self.get_logs(**filter_kwargs)

        output = io.StringIO()
        writer = csv.writer(output)

        # Header
        writer.writerow([
            "id",
            "timestamp",
            "user_id",
            "action",
            "resource_type",
            "resource_id",
            "ip_address",
            "session_id",
        ])

        # Data
        for log in logs:
            writer.writerow([
                log.id,
                log.timestamp.isoformat(),
                log.user_id,
                log.action.value,
                log.resource_type,
                log.resource_id,
                log.ip_address,
                log.session_id,
            ])

        return output.getvalue()

    # Retention

    def apply_retention(self) -> int:
        """Remove logs older than retention period."""
        with self._lock:
            cutoff = datetime.now() - timedelta(days=self._retention_days)
            original_count = len(self._logs)

            # Find logs to keep
            keep_indices = []
            for i, log in enumerate(self._logs):
                if log.timestamp >= cutoff:
                    keep_indices.append(i)

            if len(keep_indices) == original_count:
                return 0

            # Rebuild logs and indexes
            new_logs = [self._logs[i] for i in keep_indices]
            self._logs = new_logs

            # Rebuild indexes
            self._index_by_resource.clear()
            self._index_by_user.clear()

            for i, log in enumerate(self._logs):
                key = f"{log.resource_type}:{log.resource_id}"
                self._index_by_resource[key].append(i)
                self._index_by_user[log.user_id].append(i)

            return original_count - len(self._logs)

    @property
    def total_logs(self) -> int:
        """Get total number of logs."""
        return len(self._logs)

    @property
    def retention_days(self) -> int:
        """Get retention period in days."""
        return self._retention_days

    def set_retention(self, days: int) -> None:
        """Set retention period."""
        if days < 1:
            raise ValueError("Retention must be at least 1 day")
        self._retention_days = days


# Singleton instance
_logger: Optional[AuditLogger] = None


def get_audit_logger() -> AuditLogger:
    """Get global audit logger instance."""
    global _logger
    if _logger is None:
        _logger = AuditLogger()
    return _logger
