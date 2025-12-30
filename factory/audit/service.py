# -*- coding: utf-8 -*-
"""
Audit Service - Issue #339
==========================
Service for logging and querying audit entries.
"""

import logging
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List
from contextlib import contextmanager

logger = logging.getLogger(__name__)

# Global audit service instance
_audit_service = None


class AuditService:
    """
    Service for creating and querying audit log entries.

    Provides async-safe logging that doesn't block the main request.
    """

    def __init__(self, session_factory=None):
        """
        Initialize audit service.

        Args:
            session_factory: SQLAlchemy session factory
        """
        self._session_factory = session_factory
        self._queue = []  # Buffer for batch inserts
        self._max_queue_size = 100

    def _get_session(self):
        """Get database session."""
        if self._session_factory:
            return self._session_factory()
        try:
            from factory.database.connection import SessionLocal
            return SessionLocal()
        except ImportError:
            return None

    def log(
        self,
        action: str,
        resource_type: str,
        resource_id: str = None,
        tenant_id: str = None,
        user_id: str = None,
        username: str = None,
        old_value: Dict = None,
        new_value: Dict = None,
        changes: Dict = None,
        metadata: Dict = None,
        ip_address: str = None,
        user_agent: str = None,
        request_id: str = None,
        request_path: str = None,
        request_method: str = None,
        status: str = "success",
        error_message: str = None
    ) -> Optional[int]:
        """
        Log an audit entry.

        Args:
            action: Action type (CREATE, UPDATE, DELETE, etc)
            resource_type: Type of resource (story, project, user)
            resource_id: ID of the resource
            tenant_id: Tenant identifier
            user_id: User identifier
            username: Username for display
            old_value: Previous state of resource
            new_value: New state of resource
            changes: Dict of changed fields
            metadata: Additional context
            ip_address: Client IP address
            user_agent: Client user agent
            request_id: Request correlation ID
            request_path: API path called
            request_method: HTTP method
            status: Result status (success, failure, error)
            error_message: Error details if failed

        Returns:
            Audit log entry ID or None
        """
        try:
            from .models import AuditLog

            session = self._get_session()
            if not session:
                logger.warning("[Audit] No database session available")
                return None

            try:
                entry = AuditLog(
                    timestamp=datetime.utcnow(),
                    tenant_id=tenant_id,
                    user_id=user_id,
                    username=username,
                    action=action,
                    resource_type=resource_type,
                    resource_id=resource_id,
                    old_value=old_value,
                    new_value=new_value,
                    changes=changes,
                    metadata=metadata,
                    ip_address=ip_address,
                    user_agent=user_agent,
                    request_id=request_id,
                    request_path=request_path,
                    request_method=request_method,
                    status=status,
                    error_message=error_message
                )

                session.add(entry)
                session.commit()

                logger.debug(f"[Audit] Logged: {action} {resource_type}/{resource_id} by {user_id}")
                return entry.id

            finally:
                session.close()

        except Exception as e:
            logger.error(f"[Audit] Failed to log entry: {e}")
            return None

    def query(
        self,
        tenant_id: str = None,
        user_id: str = None,
        action: str = None,
        resource_type: str = None,
        resource_id: str = None,
        from_date: datetime = None,
        to_date: datetime = None,
        status: str = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[Dict[str, Any]]:
        """
        Query audit log entries.

        Args:
            tenant_id: Filter by tenant
            user_id: Filter by user
            action: Filter by action type
            resource_type: Filter by resource type
            resource_id: Filter by specific resource
            from_date: Start date
            to_date: End date
            status: Filter by status
            limit: Max results
            offset: Pagination offset

        Returns:
            List of audit log entries as dicts
        """
        try:
            from .models import AuditLog

            session = self._get_session()
            if not session:
                return []

            try:
                query = session.query(AuditLog)

                if tenant_id:
                    query = query.filter(AuditLog.tenant_id == tenant_id)
                if user_id:
                    query = query.filter(AuditLog.user_id == user_id)
                if action:
                    query = query.filter(AuditLog.action == action)
                if resource_type:
                    query = query.filter(AuditLog.resource_type == resource_type)
                if resource_id:
                    query = query.filter(AuditLog.resource_id == resource_id)
                if from_date:
                    query = query.filter(AuditLog.timestamp >= from_date)
                if to_date:
                    query = query.filter(AuditLog.timestamp <= to_date)
                if status:
                    query = query.filter(AuditLog.status == status)

                query = query.order_by(AuditLog.timestamp.desc())
                query = query.limit(limit).offset(offset)

                return [entry.to_dict() for entry in query.all()]

            finally:
                session.close()

        except Exception as e:
            logger.error(f"[Audit] Query failed: {e}")
            return []

    def get_by_id(self, audit_id: int) -> Optional[Dict[str, Any]]:
        """Get single audit entry by ID."""
        try:
            from .models import AuditLog

            session = self._get_session()
            if not session:
                return None

            try:
                entry = session.query(AuditLog).filter(AuditLog.id == audit_id).first()
                return entry.to_dict() if entry else None
            finally:
                session.close()

        except Exception as e:
            logger.error(f"[Audit] Get by ID failed: {e}")
            return None

    def cleanup(self, retention_days: int = 90) -> int:
        """
        Delete old audit entries.

        Args:
            retention_days: Keep entries for this many days

        Returns:
            Number of deleted entries
        """
        try:
            from .models import AuditLog

            session = self._get_session()
            if not session:
                return 0

            try:
                cutoff_date = datetime.utcnow() - timedelta(days=retention_days)
                deleted = session.query(AuditLog).filter(
                    AuditLog.timestamp < cutoff_date
                ).delete()
                session.commit()

                logger.info(f"[Audit] Cleaned up {deleted} entries older than {retention_days} days")
                return deleted

            finally:
                session.close()

        except Exception as e:
            logger.error(f"[Audit] Cleanup failed: {e}")
            return 0


def get_audit_service() -> AuditService:
    """Get global audit service instance."""
    global _audit_service
    if _audit_service is None:
        _audit_service = AuditService()
    return _audit_service


def log_action(
    action: str,
    resource_type: str,
    resource_id: str = None,
    **kwargs
) -> Optional[int]:
    """
    Convenience function to log an audit entry.

    Usage:
        log_action("CREATE", "story", story_id, user_id=user.id)
    """
    service = get_audit_service()
    return service.log(action, resource_type, resource_id, **kwargs)
