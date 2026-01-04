# -*- coding: utf-8 -*-
"""
Audit Models - Plataforma E
===========================

Data models for audit logging.

Issue #439: API de Auditoria com Logs de Acoes
"""

import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, Optional


class AuditAction(str, Enum):
    """Types of auditable actions."""
    CREATE = "CREATE"
    READ = "READ"
    UPDATE = "UPDATE"
    DELETE = "DELETE"
    LOGIN = "LOGIN"
    LOGOUT = "LOGOUT"
    EXPORT = "EXPORT"
    IMPORT = "IMPORT"
    ASSIGN = "ASSIGN"
    UNASSIGN = "UNASSIGN"
    MOVE = "MOVE"
    APPROVE = "APPROVE"
    REJECT = "REJECT"


class ResourceType(str, Enum):
    """Types of auditable resources."""
    STORY = "story"
    TASK = "task"
    SPRINT = "sprint"
    EPIC = "epic"
    PROJECT = "project"
    USER = "user"
    AGENT = "agent"
    TAG = "tag"
    CATEGORY = "category"
    REPORT = "report"
    SYSTEM = "system"


@dataclass
class AuditLog:
    """An audit log entry (immutable after creation)."""
    user_id: str
    action: AuditAction
    resource_type: str
    resource_id: str
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    old_value: Optional[Dict[str, Any]] = None
    new_value: Optional[Dict[str, Any]] = None
    ip_address: str = ""
    user_agent: str = ""
    session_id: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    
    def __post_init__(self):
        # Ensure action is AuditAction enum
        if isinstance(self.action, str):
            self.action = AuditAction(self.action)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary (read-only serialization)."""
        return {
            "id": self.id,
            "user_id": self.user_id,
            "action": self.action.value,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "old_value": self.old_value,
            "new_value": self.new_value,
            "ip_address": self.ip_address,
            "user_agent": self.user_agent,
            "session_id": self.session_id,
            "metadata": self.metadata,
            "timestamp": self.timestamp.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AuditLog":
        """Create from dictionary."""
        timestamp = data.get("timestamp")
        if isinstance(timestamp, str):
            timestamp = datetime.fromisoformat(timestamp)
        elif timestamp is None:
            timestamp = datetime.now()

        action = data["action"]
        if isinstance(action, str):
            action = AuditAction(action)

        return cls(
            id=data.get("id", str(uuid.uuid4())),
            user_id=data["user_id"],
            action=action,
            resource_type=data["resource_type"],
            resource_id=data["resource_id"],
            old_value=data.get("old_value"),
            new_value=data.get("new_value"),
            ip_address=data.get("ip_address", ""),
            user_agent=data.get("user_agent", ""),
            session_id=data.get("session_id", ""),
            metadata=data.get("metadata", {}),
            timestamp=timestamp,
        )

    def get_changes(self) -> Dict[str, Dict[str, Any]]:
        """Get diff between old and new values."""
        if not self.old_value or not self.new_value:
            return {}

        changes = {}
        all_keys = set(self.old_value.keys()) | set(self.new_value.keys())

        for key in all_keys:
            old = self.old_value.get(key)
            new = self.new_value.get(key)
            if old != new:
                changes[key] = {"old": old, "new": new}

        return changes

    @property
    def summary(self) -> str:
        """Get human-readable summary of the action."""
        return f"{self.user_id} {self.action.value} {self.resource_type}/{self.resource_id}"
