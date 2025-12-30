# -*- coding: utf-8 -*-
"""
API Key Models - Issue #341
===========================
Database models for API key management.
"""

from datetime import datetime
from typing import List, Optional
from dataclasses import dataclass, field
from enum import Enum


class APIKeyScope(str, Enum):
    """Available API key scopes."""
    # Stories
    STORIES_READ = "stories:read"
    STORIES_WRITE = "stories:write"

    # Projects
    PROJECTS_READ = "projects:read"
    PROJECTS_WRITE = "projects:write"

    # Tasks
    TASKS_READ = "tasks:read"
    TASKS_WRITE = "tasks:write"

    # Users (admin only)
    USERS_READ = "users:read"
    USERS_WRITE = "users:write"

    # Webhooks
    WEBHOOKS_MANAGE = "webhooks:manage"

    # Full access
    FULL_ACCESS = "*"


@dataclass
class APIKey:
    """
    API key model.

    Keys are prefixed to indicate environment:
    - pk_live_xxx - Production keys
    - pk_test_xxx - Test/development keys
    """
    id: str  # pk_live_xxx or pk_test_xxx
    tenant_id: str
    name: str  # Human-readable name
    key_hash: str  # bcrypt hash of the secret
    scopes: List[str] = field(default_factory=list)
    rate_limit: int = 1000  # Requests per minute
    expires_at: Optional[datetime] = None
    last_used_at: Optional[datetime] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    created_by: str = ""
    is_active: bool = True
    is_test: bool = False
    metadata: dict = field(default_factory=dict)

    def has_scope(self, scope: str) -> bool:
        """Check if key has a specific scope."""
        if APIKeyScope.FULL_ACCESS.value in self.scopes:
            return True
        return scope in self.scopes

    def is_expired(self) -> bool:
        """Check if key has expired."""
        if self.expires_at is None:
            return False
        return datetime.utcnow() > self.expires_at

    def to_dict(self, include_sensitive: bool = False) -> dict:
        """Convert to dictionary."""
        data = {
            "id": self.id,
            "tenant_id": self.tenant_id,
            "name": self.name,
            "scopes": self.scopes,
            "rate_limit": self.rate_limit,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "last_used_at": self.last_used_at.isoformat() if self.last_used_at else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "created_by": self.created_by,
            "is_active": self.is_active,
            "is_test": self.is_test
        }
        if include_sensitive:
            data["key_hash"] = self.key_hash
        return data


@dataclass
class APIKeyCreateResult:
    """Result of creating an API key."""
    key: APIKey
    secret: str  # Only returned once at creation

    def to_dict(self) -> dict:
        return {
            "id": self.key.id,
            "secret": self.secret,
            "full_key": f"{self.key.id}.{self.secret}",
            "name": self.key.name,
            "scopes": self.key.scopes,
            "expires_at": self.key.expires_at.isoformat() if self.key.expires_at else None,
            "message": "Store this secret securely. It will not be shown again."
        }
