# -*- coding: utf-8 -*-
"""
Audit Log Models - Issue #339
=============================
Database models for audit logging.
"""

from datetime import datetime
from typing import Optional, Dict, Any
from sqlalchemy import Column, String, DateTime, Text, Integer, JSON, Index
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


class AuditLog(Base):
    """
    Audit log entry for tracking all actions in the system.

    Captures who did what, when, and from where.
    """
    __tablename__ = "audit_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # When
    timestamp = Column(DateTime, default=datetime.utcnow, nullable=False, index=True)

    # Who
    tenant_id = Column(String(100), nullable=True, index=True)
    user_id = Column(String(100), nullable=True, index=True)
    username = Column(String(255), nullable=True)

    # What
    action = Column(String(50), nullable=False, index=True)  # CREATE, READ, UPDATE, DELETE, LOGIN, etc
    resource_type = Column(String(100), nullable=False, index=True)  # story, project, user, etc
    resource_id = Column(String(100), nullable=True)

    # Details
    old_value = Column(JSON, nullable=True)  # Previous state
    new_value = Column(JSON, nullable=True)  # New state
    changes = Column(JSON, nullable=True)  # Diff of changes
    metadata = Column(JSON, nullable=True)  # Additional context

    # Request context
    ip_address = Column(String(45), nullable=True)  # IPv6 compatible
    user_agent = Column(Text, nullable=True)
    request_id = Column(String(100), nullable=True)
    request_path = Column(String(500), nullable=True)
    request_method = Column(String(10), nullable=True)

    # Result
    status = Column(String(20), default="success")  # success, failure, error
    error_message = Column(Text, nullable=True)

    # Indexes for common queries
    __table_args__ = (
        Index('ix_audit_tenant_timestamp', 'tenant_id', 'timestamp'),
        Index('ix_audit_user_timestamp', 'user_id', 'timestamp'),
        Index('ix_audit_resource', 'resource_type', 'resource_id'),
        Index('ix_audit_action_timestamp', 'action', 'timestamp'),
    )

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None,
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "username": self.username,
            "action": self.action,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "old_value": self.old_value,
            "new_value": self.new_value,
            "changes": self.changes,
            "metadata": self.metadata,
            "ip_address": self.ip_address,
            "request_id": self.request_id,
            "request_path": self.request_path,
            "request_method": self.request_method,
            "status": self.status,
            "error_message": self.error_message
        }


# Action constants
class AuditAction:
    """Standard audit action types."""
    CREATE = "CREATE"
    READ = "READ"
    UPDATE = "UPDATE"
    DELETE = "DELETE"
    LOGIN = "LOGIN"
    LOGOUT = "LOGOUT"
    LOGIN_FAILED = "LOGIN_FAILED"
    PASSWORD_CHANGE = "PASSWORD_CHANGE"
    PASSWORD_RESET = "PASSWORD_RESET"
    PERMISSION_CHANGE = "PERMISSION_CHANGE"
    EXPORT = "EXPORT"
    IMPORT = "IMPORT"
    API_KEY_CREATE = "API_KEY_CREATE"
    API_KEY_REVOKE = "API_KEY_REVOKE"
