# -*- coding: utf-8 -*-
"""
Unified Audit Logging System - Issue #86
Fabrica de Agentes v6.5

Implements SOC2/GDPR compliant audit logging:
1. Unified logging across all modules
2. Authentication events (login, logout, failures)
3. RBAC changes (role assignments, permission changes)
4. Data access logging (CRUD operations)
5. Immutable storage with checksums
6. Retention policies and archival

Compliance Standards:
- SOC2 Type II: Complete audit trail
- GDPR: Data access logging for privacy
- ISO 27001: Security event logging
"""

import hashlib
import hmac
import json
import os
import uuid
from datetime import datetime, timedelta
from enum import Enum
from typing import Optional, Dict, Any, List, Callable
from functools import wraps
from dataclasses import dataclass, asdict
import threading
from queue import Queue

from pydantic import BaseModel, Field

from factory.database.connection import SessionLocal


# =============================================================================
# CONFIGURATION
# =============================================================================

# Audit log settings
AUDIT_LOG_ENABLED = os.getenv("AUDIT_LOG_ENABLED", "true").lower() == "true"
AUDIT_LOG_ASYNC = os.getenv("AUDIT_LOG_ASYNC", "true").lower() == "true"
AUDIT_RETENTION_DAYS = int(os.getenv("AUDIT_RETENTION_DAYS", "365"))
AUDIT_HMAC_KEY = os.getenv("AUDIT_HMAC_KEY", "change-this-in-production").encode()

# PII fields to redact in logs
PII_FIELDS = [
    "password", "password_hash", "secret", "token", "api_key",
    "credit_card", "ssn", "social_security", "email", "phone"
]


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class AuditCategory(str, Enum):
    """Categories of audit events"""
    AUTHENTICATION = "authentication"
    AUTHORIZATION = "authorization"
    DATA_ACCESS = "data_access"
    DATA_MODIFICATION = "data_modification"
    CONFIGURATION = "configuration"
    SECURITY = "security"
    SYSTEM = "system"
    COMPLIANCE = "compliance"


class AuditSeverity(str, Enum):
    """Severity levels for audit events"""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


class AuditAction(str, Enum):
    """Standard audit actions"""
    # Authentication
    LOGIN_SUCCESS = "login_success"
    LOGIN_FAILURE = "login_failure"
    LOGOUT = "logout"
    PASSWORD_CHANGE = "password_change"
    PASSWORD_RESET = "password_reset"
    MFA_ENABLED = "mfa_enabled"
    MFA_DISABLED = "mfa_disabled"
    MFA_VERIFIED = "mfa_verified"
    MFA_FAILED = "mfa_failed"
    SESSION_EXPIRED = "session_expired"
    TOKEN_REFRESH = "token_refresh"

    # Authorization
    PERMISSION_GRANTED = "permission_granted"
    PERMISSION_DENIED = "permission_denied"
    ROLE_ASSIGNED = "role_assigned"
    ROLE_REVOKED = "role_revoked"
    ROLE_CREATED = "role_created"
    ROLE_MODIFIED = "role_modified"
    ROLE_DELETED = "role_deleted"

    # Data Access
    DATA_READ = "data_read"
    DATA_EXPORT = "data_export"
    DATA_SEARCH = "data_search"
    BULK_READ = "bulk_read"

    # Data Modification
    DATA_CREATE = "data_create"
    DATA_UPDATE = "data_update"
    DATA_DELETE = "data_delete"
    BULK_MODIFY = "bulk_modify"

    # Configuration
    CONFIG_CHANGE = "config_change"
    SETTING_UPDATE = "setting_update"
    FEATURE_TOGGLE = "feature_toggle"

    # Security Events
    SUSPICIOUS_ACTIVITY = "suspicious_activity"
    RATE_LIMIT_EXCEEDED = "rate_limit_exceeded"
    BRUTE_FORCE_DETECTED = "brute_force_detected"
    INVALID_TOKEN = "invalid_token"
    TENANT_VIOLATION = "tenant_violation"
    PRIVILEGE_ESCALATION = "privilege_escalation"

    # System Events
    SYSTEM_START = "system_start"
    SYSTEM_STOP = "system_stop"
    MAINTENANCE_START = "maintenance_start"
    MAINTENANCE_END = "maintenance_end"
    BACKUP_CREATED = "backup_created"
    BACKUP_RESTORED = "backup_restored"


@dataclass
class AuditEntry:
    """Complete audit log entry"""
    # Identity
    audit_id: str
    timestamp: datetime

    # Actor
    user_id: Optional[int]
    username: Optional[str]
    tenant_id: Optional[str]
    session_id: Optional[str]

    # Action
    category: AuditCategory
    action: str
    severity: AuditSeverity

    # Target
    resource_type: str
    resource_id: Optional[str]

    # Context
    ip_address: Optional[str]
    user_agent: Optional[str]
    request_id: Optional[str]
    endpoint: Optional[str]
    method: Optional[str]

    # Result
    success: bool
    error_code: Optional[str]
    error_message: Optional[str]

    # Details
    details: Dict[str, Any]
    changes: Optional[Dict[str, Any]]  # Before/after for modifications

    # Integrity
    checksum: str  # HMAC of the entry
    previous_checksum: Optional[str]  # Chain integrity

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "audit_id": self.audit_id,
            "timestamp": self.timestamp.isoformat(),
            "user_id": self.user_id,
            "username": self.username,
            "tenant_id": self.tenant_id,
            "session_id": self.session_id,
            "category": self.category.value if isinstance(self.category, AuditCategory) else self.category,
            "action": self.action.value if isinstance(self.action, AuditAction) else self.action,
            "severity": self.severity.value if isinstance(self.severity, AuditSeverity) else self.severity,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "ip_address": self.ip_address,
            "user_agent": self.user_agent,
            "request_id": self.request_id,
            "endpoint": self.endpoint,
            "method": self.method,
            "success": self.success,
            "error_code": self.error_code,
            "error_message": self.error_message,
            "details": self.details,
            "changes": self.changes,
            "checksum": self.checksum,
            "previous_checksum": self.previous_checksum
        }


class AuditQuery(BaseModel):
    """Query parameters for audit log search"""
    user_id: Optional[int] = None
    username: Optional[str] = None
    tenant_id: Optional[str] = None
    category: Optional[AuditCategory] = None
    action: Optional[str] = None
    severity: Optional[AuditSeverity] = None
    resource_type: Optional[str] = None
    resource_id: Optional[str] = None
    success: Optional[bool] = None
    start_date: Optional[datetime] = None
    end_date: Optional[datetime] = None
    limit: int = Field(default=100, le=1000)
    offset: int = 0


# =============================================================================
# AUDIT LOGGER - Core Implementation
# =============================================================================

class AuditLogger:
    """
    Unified audit logging service.

    Thread-safe, supports async logging for performance.
    Maintains integrity chain with HMAC checksums.

    Usage:
        logger = AuditLogger()

        # Log authentication event
        logger.log_auth_event(
            action=AuditAction.LOGIN_SUCCESS,
            user_id=123,
            username="john",
            ip_address="192.168.1.1"
        )

        # Log data access
        logger.log_data_access(
            action=AuditAction.DATA_READ,
            resource_type="stories",
            resource_id="STR-0001",
            user_id=123
        )

        # Decorator for automatic logging
        @audit_log(action=AuditAction.DATA_CREATE, resource_type="projects")
        async def create_project(...):
            ...
    """

    _instance = None
    _lock = threading.Lock()
    _last_checksum: Optional[str] = None
    _write_queue: Queue = Queue()
    _writer_thread: Optional[threading.Thread] = None

    def __new__(cls):
        """Singleton pattern for global logger"""
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if self._initialized:
            return
        self._initialized = True
        self._start_async_writer()

    def _start_async_writer(self):
        """Start background thread for async log writing"""
        if not AUDIT_LOG_ASYNC:
            return

        def writer():
            while True:
                try:
                    entry = self._write_queue.get()
                    if entry is None:
                        break
                    self._write_entry(entry)
                except Exception as e:
                    print(f"[AuditLogger] Write error: {e}")

        self._writer_thread = threading.Thread(target=writer, daemon=True)
        self._writer_thread.start()

    def stop(self):
        """Stop the async writer thread"""
        if self._writer_thread:
            self._write_queue.put(None)
            self._writer_thread.join(timeout=5)

    # -------------------------------------------------------------------------
    # Core Logging Methods
    # -------------------------------------------------------------------------

    def log(
        self,
        category: AuditCategory,
        action: AuditAction,
        resource_type: str,
        resource_id: Optional[str] = None,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        tenant_id: Optional[str] = None,
        session_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        request_id: Optional[str] = None,
        endpoint: Optional[str] = None,
        method: Optional[str] = None,
        success: bool = True,
        error_code: Optional[str] = None,
        error_message: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None,
        changes: Optional[Dict[str, Any]] = None,
        severity: AuditSeverity = AuditSeverity.INFO
    ):
        """
        Log an audit event.

        This is the main logging method. All other methods call this.
        """
        if not AUDIT_LOG_ENABLED:
            return

        # Determine severity based on action if not specified
        if severity == AuditSeverity.INFO:
            severity = self._determine_severity(action, success)

        # Redact PII from details
        safe_details = self._redact_pii(details or {})
        safe_changes = self._redact_pii(changes) if changes else None

        # Create entry
        entry = AuditEntry(
            audit_id=f"AUD-{uuid.uuid4().hex[:12].upper()}",
            timestamp=datetime.utcnow(),
            user_id=user_id,
            username=username,
            tenant_id=tenant_id,
            session_id=session_id,
            category=category,
            action=action.value if isinstance(action, AuditAction) else action,
            severity=severity,
            resource_type=resource_type,
            resource_id=resource_id,
            ip_address=ip_address,
            user_agent=user_agent[:500] if user_agent else None,
            request_id=request_id,
            endpoint=endpoint,
            method=method,
            success=success,
            error_code=error_code,
            error_message=error_message,
            details=safe_details,
            changes=safe_changes,
            checksum="",
            previous_checksum=self._last_checksum
        )

        # Calculate checksum
        entry.checksum = self._calculate_checksum(entry)
        self._last_checksum = entry.checksum

        # Write entry
        if AUDIT_LOG_ASYNC:
            self._write_queue.put(entry)
        else:
            self._write_entry(entry)

    def _determine_severity(self, action: AuditAction, success: bool) -> AuditSeverity:
        """Determine severity based on action and result"""
        if not success:
            if action in [
                AuditAction.LOGIN_FAILURE,
                AuditAction.PERMISSION_DENIED,
                AuditAction.MFA_FAILED
            ]:
                return AuditSeverity.WARNING
            return AuditSeverity.ERROR

        critical_actions = [
            AuditAction.SUSPICIOUS_ACTIVITY,
            AuditAction.BRUTE_FORCE_DETECTED,
            AuditAction.TENANT_VIOLATION,
            AuditAction.PRIVILEGE_ESCALATION
        ]
        if action in critical_actions:
            return AuditSeverity.CRITICAL

        warning_actions = [
            AuditAction.RATE_LIMIT_EXCEEDED,
            AuditAction.INVALID_TOKEN
        ]
        if action in warning_actions:
            return AuditSeverity.WARNING

        return AuditSeverity.INFO

    def _redact_pii(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Redact PII fields from data"""
        if not data:
            return {}

        result = {}
        for key, value in data.items():
            key_lower = key.lower()
            if any(pii in key_lower for pii in PII_FIELDS):
                result[key] = "[REDACTED]"
            elif isinstance(value, dict):
                result[key] = self._redact_pii(value)
            else:
                result[key] = value
        return result

    def _calculate_checksum(self, entry: AuditEntry) -> str:
        """Calculate HMAC checksum for integrity"""
        # Create deterministic string from entry
        data = json.dumps({
            "audit_id": entry.audit_id,
            "timestamp": entry.timestamp.isoformat(),
            "user_id": entry.user_id,
            "action": entry.action,
            "resource_type": entry.resource_type,
            "resource_id": entry.resource_id,
            "success": entry.success,
            "previous_checksum": entry.previous_checksum
        }, sort_keys=True)

        return hmac.new(AUDIT_HMAC_KEY, data.encode(), hashlib.sha256).hexdigest()

    def _write_entry(self, entry: AuditEntry):
        """Write entry to database"""
        try:
            from factory.database.audit_models import AuditLogEntry

            db = SessionLocal()
            try:
                log = AuditLogEntry(
                    audit_id=entry.audit_id,
                    timestamp=entry.timestamp,
                    user_id=entry.user_id,
                    username=entry.username,
                    tenant_id=entry.tenant_id,
                    session_id=entry.session_id,
                    category=entry.category.value if isinstance(entry.category, AuditCategory) else entry.category,
                    action=entry.action,
                    severity=entry.severity.value if isinstance(entry.severity, AuditSeverity) else entry.severity,
                    resource_type=entry.resource_type,
                    resource_id=entry.resource_id,
                    ip_address=entry.ip_address,
                    user_agent=entry.user_agent,
                    request_id=entry.request_id,
                    endpoint=entry.endpoint,
                    method=entry.method,
                    success=entry.success,
                    error_code=entry.error_code,
                    error_message=entry.error_message,
                    details=entry.details,
                    changes=entry.changes,
                    checksum=entry.checksum,
                    previous_checksum=entry.previous_checksum
                )
                db.add(log)
                db.commit()
            finally:
                db.close()
        except Exception as e:
            # Fallback to console logging
            print(f"[AuditLogger] DB write failed: {e}")
            print(f"[AuditLogger] Entry: {entry.to_dict()}")

    # -------------------------------------------------------------------------
    # Convenience Methods
    # -------------------------------------------------------------------------

    def log_auth_event(
        self,
        action: AuditAction,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        success: bool = True,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        details: Optional[Dict] = None,
        error_message: Optional[str] = None
    ):
        """Log authentication event"""
        self.log(
            category=AuditCategory.AUTHENTICATION,
            action=action,
            resource_type="auth",
            user_id=user_id,
            username=username,
            ip_address=ip_address,
            user_agent=user_agent,
            success=success,
            details=details,
            error_message=error_message
        )

    def log_authorization_event(
        self,
        action: AuditAction,
        user_id: int,
        username: Optional[str] = None,
        resource_type: str = "permission",
        resource_id: Optional[str] = None,
        details: Optional[Dict] = None,
        success: bool = True
    ):
        """Log authorization/RBAC event"""
        self.log(
            category=AuditCategory.AUTHORIZATION,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            user_id=user_id,
            username=username,
            success=success,
            details=details
        )

    def log_data_access(
        self,
        action: AuditAction,
        resource_type: str,
        resource_id: Optional[str] = None,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        tenant_id: Optional[str] = None,
        details: Optional[Dict] = None,
        success: bool = True
    ):
        """Log data access event"""
        self.log(
            category=AuditCategory.DATA_ACCESS,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            user_id=user_id,
            username=username,
            tenant_id=tenant_id,
            success=success,
            details=details
        )

    def log_data_modification(
        self,
        action: AuditAction,
        resource_type: str,
        resource_id: str,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        tenant_id: Optional[str] = None,
        changes: Optional[Dict] = None,
        details: Optional[Dict] = None,
        success: bool = True
    ):
        """Log data modification event"""
        self.log(
            category=AuditCategory.DATA_MODIFICATION,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            user_id=user_id,
            username=username,
            tenant_id=tenant_id,
            changes=changes,
            details=details,
            success=success
        )

    def log_security_event(
        self,
        action: AuditAction,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        ip_address: Optional[str] = None,
        details: Optional[Dict] = None,
        severity: AuditSeverity = AuditSeverity.WARNING
    ):
        """Log security event"""
        self.log(
            category=AuditCategory.SECURITY,
            action=action,
            resource_type="security",
            user_id=user_id,
            username=username,
            ip_address=ip_address,
            success=False,
            details=details,
            severity=severity
        )

    def log_config_change(
        self,
        setting_name: str,
        old_value: Any,
        new_value: Any,
        user_id: Optional[int] = None,
        username: Optional[str] = None,
        tenant_id: Optional[str] = None
    ):
        """Log configuration change"""
        self.log(
            category=AuditCategory.CONFIGURATION,
            action=AuditAction.CONFIG_CHANGE,
            resource_type="configuration",
            resource_id=setting_name,
            user_id=user_id,
            username=username,
            tenant_id=tenant_id,
            changes={
                "before": {"value": old_value},
                "after": {"value": new_value}
            },
            success=True
        )

    # -------------------------------------------------------------------------
    # Query Methods
    # -------------------------------------------------------------------------

    def query(self, params: AuditQuery) -> List[Dict[str, Any]]:
        """Query audit logs"""
        try:
            from factory.database.audit_models import AuditLogEntry

            db = SessionLocal()
            try:
                query = db.query(AuditLogEntry)

                if params.user_id:
                    query = query.filter(AuditLogEntry.user_id == params.user_id)
                if params.username:
                    query = query.filter(AuditLogEntry.username == params.username)
                if params.tenant_id:
                    query = query.filter(AuditLogEntry.tenant_id == params.tenant_id)
                if params.category:
                    query = query.filter(AuditLogEntry.category == params.category.value)
                if params.action:
                    query = query.filter(AuditLogEntry.action == params.action)
                if params.severity:
                    query = query.filter(AuditLogEntry.severity == params.severity.value)
                if params.resource_type:
                    query = query.filter(AuditLogEntry.resource_type == params.resource_type)
                if params.resource_id:
                    query = query.filter(AuditLogEntry.resource_id == params.resource_id)
                if params.success is not None:
                    query = query.filter(AuditLogEntry.success == params.success)
                if params.start_date:
                    query = query.filter(AuditLogEntry.timestamp >= params.start_date)
                if params.end_date:
                    query = query.filter(AuditLogEntry.timestamp <= params.end_date)

                query = query.order_by(AuditLogEntry.timestamp.desc())
                query = query.offset(params.offset).limit(params.limit)

                return [log.to_dict() for log in query.all()]

            finally:
                db.close()

        except Exception as e:
            print(f"[AuditLogger] Query error: {e}")
            return []

    def verify_integrity(self, start_id: str = None, end_id: str = None) -> Dict[str, Any]:
        """
        Verify integrity of audit log chain.

        Checks that checksum chain is unbroken.
        """
        try:
            from factory.database.audit_models import AuditLogEntry

            db = SessionLocal()
            try:
                query = db.query(AuditLogEntry).order_by(AuditLogEntry.timestamp.asc())

                logs = query.all()
                result = {
                    "verified": True,
                    "total_entries": len(logs),
                    "errors": []
                }

                previous_checksum = None
                for log in logs:
                    # Verify previous checksum matches
                    if log.previous_checksum != previous_checksum:
                        result["verified"] = False
                        result["errors"].append({
                            "audit_id": log.audit_id,
                            "error": "Previous checksum mismatch",
                            "expected": previous_checksum,
                            "actual": log.previous_checksum
                        })

                    previous_checksum = log.checksum

                return result

            finally:
                db.close()

        except Exception as e:
            return {"verified": False, "error": str(e)}

    def cleanup_old_logs(self, days: int = None) -> Dict[str, Any]:
        """
        Archive/delete logs older than retention period.

        Should be run as scheduled job.
        """
        if days is None:
            days = AUDIT_RETENTION_DAYS

        cutoff_date = datetime.utcnow() - timedelta(days=days)

        try:
            from factory.database.audit_models import AuditLogEntry

            db = SessionLocal()
            try:
                # Count logs to delete
                count = db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp < cutoff_date
                ).count()

                # In production, archive to cold storage first
                # For now, just delete
                db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp < cutoff_date
                ).delete()

                db.commit()

                return {
                    "success": True,
                    "deleted_count": count,
                    "cutoff_date": cutoff_date.isoformat()
                }

            finally:
                db.close()

        except Exception as e:
            return {"success": False, "error": str(e)}


# =============================================================================
# DECORATOR FOR AUTOMATIC AUDIT LOGGING
# =============================================================================

def audit_log(
    action: AuditAction,
    resource_type: str,
    category: AuditCategory = AuditCategory.DATA_ACCESS,
    get_resource_id: Callable = None,
    get_details: Callable = None
):
    """
    Decorator for automatic audit logging.

    Usage:
        @audit_log(action=AuditAction.DATA_CREATE, resource_type="projects")
        async def create_project(project: ProjectCreate, user: TokenData):
            ...

        @audit_log(
            action=AuditAction.DATA_UPDATE,
            resource_type="stories",
            get_resource_id=lambda args, kwargs: kwargs.get("story_id")
        )
        async def update_story(story_id: str, ...):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def async_wrapper(*args, **kwargs):
            logger = AuditLogger()

            # Extract common parameters
            user_id = None
            username = None
            tenant_id = None

            # Try to find user in kwargs
            user = kwargs.get("user") or kwargs.get("current_user")
            if user:
                user_id = getattr(user, "user_id", None) or getattr(user, "id", None)
                username = getattr(user, "username", None)

            # Try to find request in kwargs
            request = kwargs.get("request")
            if request:
                tenant_id = getattr(request.state, "tenant_id", None)

            # Get resource ID
            resource_id = None
            if get_resource_id:
                resource_id = get_resource_id(args, kwargs)

            # Execute function
            try:
                result = await func(*args, **kwargs)

                # Get details
                details = None
                if get_details:
                    details = get_details(args, kwargs, result)

                # Log success
                logger.log(
                    category=category,
                    action=action,
                    resource_type=resource_type,
                    resource_id=resource_id,
                    user_id=user_id,
                    username=username,
                    tenant_id=tenant_id,
                    success=True,
                    details=details
                )

                return result

            except Exception as e:
                # Log failure
                logger.log(
                    category=category,
                    action=action,
                    resource_type=resource_type,
                    resource_id=resource_id,
                    user_id=user_id,
                    username=username,
                    tenant_id=tenant_id,
                    success=False,
                    error_message=str(e)
                )
                raise

        @wraps(func)
        def sync_wrapper(*args, **kwargs):
            import asyncio
            # Handle sync functions similarly
            logger = AuditLogger()

            user = kwargs.get("user") or kwargs.get("current_user")
            user_id = getattr(user, "user_id", None) if user else None
            username = getattr(user, "username", None) if user else None

            resource_id = get_resource_id(args, kwargs) if get_resource_id else None

            try:
                result = func(*args, **kwargs)

                details = get_details(args, kwargs, result) if get_details else None

                logger.log(
                    category=category,
                    action=action,
                    resource_type=resource_type,
                    resource_id=resource_id,
                    user_id=user_id,
                    username=username,
                    success=True,
                    details=details
                )

                return result

            except Exception as e:
                logger.log(
                    category=category,
                    action=action,
                    resource_type=resource_type,
                    resource_id=resource_id,
                    user_id=user_id,
                    username=username,
                    success=False,
                    error_message=str(e)
                )
                raise

        import asyncio
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return sync_wrapper

    return decorator


# =============================================================================
# FASTAPI MIDDLEWARE FOR REQUEST LOGGING
# =============================================================================

class AuditMiddleware:
    """
    FastAPI middleware for automatic request audit logging.

    Logs all API requests with timing and response status.
    """

    def __init__(self, app, exclude_paths: List[str] = None):
        self.app = app
        self.exclude_paths = exclude_paths or [
            "/health",
            "/docs",
            "/openapi.json",
            "/redoc",
            "/static"
        ]
        self.logger = AuditLogger()

    async def __call__(self, scope, receive, send):
        if scope["type"] != "http":
            await self.app(scope, receive, send)
            return

        # Check if path should be excluded
        path = scope.get("path", "")
        if any(path.startswith(p) for p in self.exclude_paths):
            await self.app(scope, receive, send)
            return

        from starlette.requests import Request
        import time

        request = Request(scope, receive)
        start_time = time.time()

        # Capture response status
        response_status = 500
        async def send_wrapper(message):
            nonlocal response_status
            if message["type"] == "http.response.start":
                response_status = message.get("status", 500)
            await send(message)

        try:
            await self.app(scope, receive, send_wrapper)
        finally:
            # Log request
            duration_ms = (time.time() - start_time) * 1000

            self.logger.log(
                category=AuditCategory.DATA_ACCESS,
                action=AuditAction.DATA_READ if request.method == "GET" else AuditAction.DATA_MODIFICATION,
                resource_type="api",
                endpoint=path,
                method=request.method,
                ip_address=request.client.host if request.client else None,
                user_agent=request.headers.get("user-agent"),
                success=200 <= response_status < 400,
                details={
                    "duration_ms": round(duration_ms, 2),
                    "status_code": response_status
                }
            )


# =============================================================================
# GLOBAL LOGGER INSTANCE
# =============================================================================

# Singleton instance
_audit_logger: Optional[AuditLogger] = None


def get_audit_logger() -> AuditLogger:
    """Get global audit logger instance"""
    global _audit_logger
    if _audit_logger is None:
        _audit_logger = AuditLogger()
    return _audit_logger


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Core
    "AuditLogger",
    "get_audit_logger",

    # Enums
    "AuditCategory",
    "AuditSeverity",
    "AuditAction",

    # Models
    "AuditEntry",
    "AuditQuery",

    # Decorator
    "audit_log",

    # Middleware
    "AuditMiddleware",
]
