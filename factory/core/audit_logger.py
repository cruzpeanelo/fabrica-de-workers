# -*- coding: utf-8 -*-
"""
Unified Audit Logging System - Issue #86
Plataforma E v6.5

Implements SOC2/GDPR compliant audit logging:
1. Unified logging across all modules
2. Authentication events (login, logout, failures)
3. RBAC changes (role assignments, permission changes)
4. Data access logging (CRUD operations)
5. Immutable storage with checksums
6. Retention policies and archival
7. SIEM export (Splunk, Elasticsearch, Syslog)
8. Configurable retention with archival

Compliance Standards:
- SOC2 Type II: Complete audit trail
- GDPR: Data access logging for privacy
- ISO 27001: Security event logging
- HIPAA: Healthcare data access tracking
- PCI-DSS: Payment data security logging
"""

import hashlib
import hmac
import json
import os
import uuid
import socket
import gzip
import io
from datetime import datetime, timedelta
from enum import Enum
from typing import Optional, Dict, Any, List, Callable, Union
from functools import wraps
from dataclasses import dataclass, asdict, field
import threading
from queue import Queue
import logging
import struct
import time

from pydantic import BaseModel, Field

try:
    from factory.database.connection import SessionLocal
except ImportError:
    SessionLocal = None


# =============================================================================
# CONFIGURATION
# =============================================================================

# Audit log settings
AUDIT_LOG_ENABLED = os.getenv("AUDIT_LOG_ENABLED", "true").lower() == "true"
AUDIT_LOG_ASYNC = os.getenv("AUDIT_LOG_ASYNC", "true").lower() == "true"
AUDIT_RETENTION_DAYS = int(os.getenv("AUDIT_RETENTION_DAYS", "365"))
AUDIT_ARCHIVE_DAYS = int(os.getenv("AUDIT_ARCHIVE_DAYS", "90"))  # Move to archive after
AUDIT_HMAC_KEY = os.getenv("AUDIT_HMAC_KEY", "change-this-in-production").encode()

# SIEM Configuration
SIEM_ENABLED = os.getenv("SIEM_ENABLED", "false").lower() == "true"
SIEM_TYPE = os.getenv("SIEM_TYPE", "syslog")  # syslog, splunk, elasticsearch, fluentd
SIEM_HOST = os.getenv("SIEM_HOST", "localhost")
SIEM_PORT = int(os.getenv("SIEM_PORT", "514"))
SIEM_PROTOCOL = os.getenv("SIEM_PROTOCOL", "udp")  # udp, tcp, http
SIEM_TOKEN = os.getenv("SIEM_TOKEN", "")  # For Splunk HEC
SIEM_INDEX = os.getenv("SIEM_INDEX", "audit_logs")  # For Splunk/Elasticsearch
SIEM_BATCH_SIZE = int(os.getenv("SIEM_BATCH_SIZE", "100"))
SIEM_FLUSH_INTERVAL = int(os.getenv("SIEM_FLUSH_INTERVAL", "5"))  # seconds

# Retention Configuration
RETENTION_CONFIG = {
    "authentication": int(os.getenv("RETENTION_AUTH_DAYS", "365")),
    "authorization": int(os.getenv("RETENTION_AUTHZ_DAYS", "365")),
    "data_access": int(os.getenv("RETENTION_DATA_ACCESS_DAYS", "180")),
    "data_modification": int(os.getenv("RETENTION_DATA_MOD_DAYS", "2555")),  # 7 years for SOC2
    "configuration": int(os.getenv("RETENTION_CONFIG_DAYS", "2555")),
    "security": int(os.getenv("RETENTION_SECURITY_DAYS", "2555")),
    "system": int(os.getenv("RETENTION_SYSTEM_DAYS", "90")),
    "compliance": int(os.getenv("RETENTION_COMPLIANCE_DAYS", "2555")),
}

# PII fields to redact in logs
PII_FIELDS = [
    "password", "password_hash", "secret", "token", "api_key",
    "credit_card", "ssn", "social_security", "email", "phone",
    "date_of_birth", "address", "national_id", "passport", "driver_license"
]

# CEF (Common Event Format) settings for SIEM
CEF_VERSION = "0"
CEF_DEVICE_VENDOR = "FabricaDeAgentes"
CEF_DEVICE_PRODUCT = "AuditLogger"
CEF_DEVICE_VERSION = "6.5"


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
    """Complete audit log entry with SOC2/GDPR compliance fields"""
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

    # SOC2/GDPR Compliance Fields
    old_value: Optional[Any] = None  # Original value before change
    new_value: Optional[Any] = None  # New value after change
    data_classification: Optional[str] = None  # public, internal, confidential, restricted
    legal_basis: Optional[str] = None  # GDPR: consent, contract, legal_obligation, etc.
    data_subject_id: Optional[str] = None  # GDPR: ID of person whose data was accessed
    retention_days: Optional[int] = None  # Override default retention
    correlation_id: Optional[str] = None  # Link related events
    source_system: Optional[str] = None  # Origin system for federated logs
    geo_location: Optional[str] = None  # Geographic location (country/region)
    risk_score: Optional[int] = None  # 0-100 risk assessment

    # Integrity
    checksum: str = ""  # HMAC of the entry
    previous_checksum: Optional[str] = None  # Chain integrity

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
            "old_value": self.old_value,
            "new_value": self.new_value,
            "data_classification": self.data_classification,
            "legal_basis": self.legal_basis,
            "data_subject_id": self.data_subject_id,
            "retention_days": self.retention_days,
            "correlation_id": self.correlation_id,
            "source_system": self.source_system,
            "geo_location": self.geo_location,
            "risk_score": self.risk_score,
            "checksum": self.checksum,
            "previous_checksum": self.previous_checksum
        }

    def to_cef(self) -> str:
        """Convert to CEF (Common Event Format) for SIEM integration"""
        severity_map = {
            AuditSeverity.DEBUG: 0,
            AuditSeverity.INFO: 3,
            AuditSeverity.WARNING: 6,
            AuditSeverity.ERROR: 8,
            AuditSeverity.CRITICAL: 10
        }
        sev_value = self.severity if isinstance(self.severity, AuditSeverity) else AuditSeverity(self.severity)
        cef_severity = severity_map.get(sev_value, 5)

        # Build extension fields
        extensions = []
        if self.user_id:
            extensions.append(f"suid={self.user_id}")
        if self.username:
            extensions.append(f"suser={self.username}")
        if self.ip_address:
            extensions.append(f"src={self.ip_address}")
        if self.resource_id:
            extensions.append(f"cs1={self.resource_id}")
            extensions.append("cs1Label=ResourceID")
        if self.tenant_id:
            extensions.append(f"cs2={self.tenant_id}")
            extensions.append("cs2Label=TenantID")
        if self.endpoint:
            extensions.append(f"request={self.endpoint}")
        if self.method:
            extensions.append(f"requestMethod={self.method}")
        extensions.append(f"outcome={'Success' if self.success else 'Failure'}")
        extensions.append(f"rt={int(self.timestamp.timestamp() * 1000)}")

        action_str = self.action.value if isinstance(self.action, AuditAction) else self.action
        category_str = self.category.value if isinstance(self.category, AuditCategory) else self.category

        return (
            f"CEF:{CEF_VERSION}|{CEF_DEVICE_VENDOR}|{CEF_DEVICE_PRODUCT}|"
            f"{CEF_DEVICE_VERSION}|{action_str}|{category_str}|{cef_severity}|"
            f"{' '.join(extensions)}"
        )

    def to_syslog(self, facility: int = 13, hostname: str = None) -> bytes:
        """Convert to RFC 5424 syslog format"""
        severity_map = {
            AuditSeverity.DEBUG: 7,
            AuditSeverity.INFO: 6,
            AuditSeverity.WARNING: 4,
            AuditSeverity.ERROR: 3,
            AuditSeverity.CRITICAL: 2
        }
        sev_value = self.severity if isinstance(self.severity, AuditSeverity) else AuditSeverity(self.severity)
        priority = facility * 8 + severity_map.get(sev_value, 6)

        hostname = hostname or socket.gethostname()
        timestamp = self.timestamp.strftime("%Y-%m-%dT%H:%M:%S.%fZ")
        app_name = "FabricaDeAgentes"
        proc_id = self.audit_id
        msg_id = self.action.value if isinstance(self.action, AuditAction) else self.action

        structured_data = (
            f'[audit@47450 user_id="{self.user_id or "-"}" '
            f'resource_type="{self.resource_type}" '
            f'resource_id="{self.resource_id or "-"}" '
            f'success="{self.success}"]'
        )

        message = json.dumps(self.to_dict(), default=str)

        syslog_msg = (
            f"<{priority}>1 {timestamp} {hostname} {app_name} "
            f"{proc_id} {msg_id} {structured_data} {message}"
        )

        return syslog_msg.encode("utf-8")

    def to_splunk_hec(self) -> Dict[str, Any]:
        """Convert to Splunk HTTP Event Collector format"""
        return {
            "time": self.timestamp.timestamp(),
            "host": socket.gethostname(),
            "source": "plataforma_e",
            "sourcetype": "audit_log",
            "index": SIEM_INDEX,
            "event": self.to_dict()
        }

    def to_elasticsearch(self) -> Dict[str, Any]:
        """Convert to Elasticsearch document format"""
        doc = self.to_dict()
        doc["@timestamp"] = self.timestamp.isoformat()
        doc["_index"] = f"{SIEM_INDEX}-{self.timestamp.strftime('%Y.%m.%d')}"
        return doc


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
    data_subject_id: Optional[str] = None  # GDPR: find all access to specific person's data
    correlation_id: Optional[str] = None  # Find related events
    ip_address: Optional[str] = None  # Filter by IP
    data_classification: Optional[str] = None  # Filter by data classification
    limit: int = Field(default=100, le=1000)
    offset: int = 0


class RetentionPolicy(BaseModel):
    """Configurable retention policy for audit logs"""
    category: str
    retention_days: int = Field(default=365, ge=30, le=2555)
    archive_after_days: int = Field(default=90, ge=7, le=365)
    compress_archive: bool = True
    delete_after_archive: bool = False
    notify_before_delete_days: int = Field(default=30, ge=7, le=90)


class SIEMExportConfig(BaseModel):
    """SIEM export configuration"""
    siem_type: str = Field(default="syslog", pattern="^(syslog|splunk|elasticsearch|fluentd)$")
    host: str
    port: int = Field(default=514, ge=1, le=65535)
    protocol: str = Field(default="udp", pattern="^(udp|tcp|http|https)$")
    token: Optional[str] = None  # For Splunk HEC
    index: str = Field(default="audit_logs")
    batch_size: int = Field(default=100, ge=1, le=1000)
    flush_interval_seconds: int = Field(default=5, ge=1, le=60)
    tls_enabled: bool = False
    tls_verify: bool = True
    format: str = Field(default="json", pattern="^(json|cef|syslog)$")


# =============================================================================
# AUDIT LOGGER - Core Implementation
# =============================================================================

class SIEMExporter:
    """
    SIEM (Security Information and Event Management) Exporter.

    Supports multiple SIEM platforms:
    - Syslog (RFC 5424)
    - Splunk (HTTP Event Collector)
    - Elasticsearch
    - Fluentd

    Thread-safe with batching for performance.
    """

    def __init__(self, config: SIEMExportConfig = None):
        self.config = config or SIEMExportConfig(
            siem_type=SIEM_TYPE,
            host=SIEM_HOST,
            port=SIEM_PORT,
            protocol=SIEM_PROTOCOL,
            token=SIEM_TOKEN,
            index=SIEM_INDEX,
            batch_size=SIEM_BATCH_SIZE,
            flush_interval_seconds=SIEM_FLUSH_INTERVAL
        )
        self._batch: List[AuditEntry] = []
        self._batch_lock = threading.Lock()
        self._socket: Optional[socket.socket] = None
        self._flush_thread: Optional[threading.Thread] = None
        self._running = False
        self._logger = logging.getLogger("SIEMExporter")

    def start(self):
        """Start the SIEM exporter background thread"""
        if not SIEM_ENABLED:
            return

        self._running = True
        self._flush_thread = threading.Thread(target=self._flush_loop, daemon=True)
        self._flush_thread.start()
        self._logger.info(f"SIEM exporter started: {self.config.siem_type}://{self.config.host}:{self.config.port}")

    def stop(self):
        """Stop the SIEM exporter and flush remaining entries"""
        self._running = False
        if self._flush_thread:
            self._flush_thread.join(timeout=10)
        self._flush_batch()
        if self._socket:
            self._socket.close()

    def export(self, entry: AuditEntry):
        """Add entry to batch for export"""
        if not SIEM_ENABLED:
            return

        with self._batch_lock:
            self._batch.append(entry)
            if len(self._batch) >= self.config.batch_size:
                self._flush_batch()

    def _flush_loop(self):
        """Background thread to periodically flush batches"""
        while self._running:
            time.sleep(self.config.flush_interval_seconds)
            self._flush_batch()

    def _flush_batch(self):
        """Flush current batch to SIEM"""
        with self._batch_lock:
            if not self._batch:
                return

            entries_to_send = self._batch.copy()
            self._batch.clear()

        try:
            if self.config.siem_type == "syslog":
                self._send_syslog(entries_to_send)
            elif self.config.siem_type == "splunk":
                self._send_splunk(entries_to_send)
            elif self.config.siem_type == "elasticsearch":
                self._send_elasticsearch(entries_to_send)
            elif self.config.siem_type == "fluentd":
                self._send_fluentd(entries_to_send)
        except Exception as e:
            self._logger.error(f"SIEM export failed: {e}")
            # Re-add entries to batch for retry (with limit to prevent memory issues)
            with self._batch_lock:
                if len(self._batch) < self.config.batch_size * 10:
                    self._batch.extend(entries_to_send)

    def _get_socket(self) -> socket.socket:
        """Get or create socket connection"""
        if self._socket is None:
            if self.config.protocol == "udp":
                self._socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            else:
                self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                self._socket.connect((self.config.host, self.config.port))
        return self._socket

    def _send_syslog(self, entries: List[AuditEntry]):
        """Send entries via syslog protocol"""
        sock = self._get_socket()
        for entry in entries:
            if self.config.format == "cef":
                message = entry.to_cef().encode("utf-8")
            else:
                message = entry.to_syslog()

            if self.config.protocol == "udp":
                sock.sendto(message, (self.config.host, self.config.port))
            else:
                sock.send(message + b"\n")

    def _send_splunk(self, entries: List[AuditEntry]):
        """Send entries to Splunk HEC"""
        import urllib.request
        import ssl

        url = f"{'https' if self.config.tls_enabled else 'http'}://{self.config.host}:{self.config.port}/services/collector/event"

        for entry in entries:
            data = json.dumps(entry.to_splunk_hec()).encode("utf-8")

            req = urllib.request.Request(url, data=data, method="POST")
            req.add_header("Authorization", f"Splunk {self.config.token}")
            req.add_header("Content-Type", "application/json")

            context = None
            if self.config.tls_enabled and not self.config.tls_verify:
                context = ssl.create_default_context()
                context.check_hostname = False
                context.verify_mode = ssl.CERT_NONE

            urllib.request.urlopen(req, timeout=10, context=context)

    def _send_elasticsearch(self, entries: List[AuditEntry]):
        """Send entries to Elasticsearch"""
        import urllib.request
        import ssl

        url = f"{'https' if self.config.tls_enabled else 'http'}://{self.config.host}:{self.config.port}/_bulk"

        # Build bulk request body
        bulk_body = ""
        for entry in entries:
            doc = entry.to_elasticsearch()
            index_name = doc.pop("_index")
            bulk_body += json.dumps({"index": {"_index": index_name}}) + "\n"
            bulk_body += json.dumps(doc) + "\n"

        req = urllib.request.Request(url, data=bulk_body.encode("utf-8"), method="POST")
        req.add_header("Content-Type", "application/x-ndjson")

        if self.config.token:
            req.add_header("Authorization", f"ApiKey {self.config.token}")

        context = None
        if self.config.tls_enabled and not self.config.tls_verify:
            context = ssl.create_default_context()
            context.check_hostname = False
            context.verify_mode = ssl.CERT_NONE

        urllib.request.urlopen(req, timeout=30, context=context)

    def _send_fluentd(self, entries: List[AuditEntry]):
        """Send entries to Fluentd via forward protocol"""
        sock = self._get_socket()
        tag = "fabrica.audit"

        for entry in entries:
            # Fluentd forward protocol: [tag, timestamp, record]
            record = entry.to_dict()
            timestamp = int(entry.timestamp.timestamp())
            message = json.dumps([tag, timestamp, record]).encode("utf-8")
            sock.send(struct.pack(">I", len(message)) + message)

    def export_batch(self, entries: List[AuditEntry], format: str = "json") -> bytes:
        """
        Export entries as a batch for download/archival.

        Args:
            entries: List of audit entries
            format: Export format (json, csv, cef)

        Returns:
            Compressed bytes of the export
        """
        if format == "json":
            data = json.dumps([e.to_dict() for e in entries], indent=2, default=str)
        elif format == "cef":
            data = "\n".join(e.to_cef() for e in entries)
        elif format == "csv":
            import csv
            import io as csv_io
            output = csv_io.StringIO()
            if entries:
                writer = csv.DictWriter(output, fieldnames=entries[0].to_dict().keys())
                writer.writeheader()
                for e in entries:
                    writer.writerow(e.to_dict())
            data = output.getvalue()
        else:
            data = json.dumps([e.to_dict() for e in entries], default=str)

        # Compress with gzip
        buffer = io.BytesIO()
        with gzip.GzipFile(fileobj=buffer, mode="wb") as f:
            f.write(data.encode("utf-8"))
        return buffer.getvalue()


class AuditLogger:
    """
    Unified audit logging service.

    Thread-safe, supports async logging for performance.
    Maintains integrity chain with HMAC checksums.
    Supports SIEM export and configurable retention.

    Usage:
        logger = AuditLogger()

        # Log authentication event
        logger.log_auth_event(
            action=AuditAction.LOGIN_SUCCESS,
            user_id=123,
            username="john",
            ip_address="192.168.1.1"
        )

        # Log data access with GDPR fields
        logger.log_data_access(
            action=AuditAction.DATA_READ,
            resource_type="stories",
            resource_id="STR-0001",
            user_id=123,
            data_subject_id="customer-456",
            legal_basis="contract"
        )

        # Log data modification with old/new values
        logger.log_data_modification(
            action=AuditAction.DATA_UPDATE,
            resource_type="users",
            resource_id="USR-123",
            old_value={"status": "active"},
            new_value={"status": "inactive"}
        )

        # Decorator for automatic logging
        @audit_log(action=AuditAction.DATA_CREATE, resource_type="projects")
        async def create_project(...):
            ...

        # Export to SIEM
        logger.export_to_siem(start_date, end_date, format="cef")
    """

    _instance = None
    _lock = threading.Lock()
    _last_checksum: Optional[str] = None
    _write_queue: Queue = Queue()
    _writer_thread: Optional[threading.Thread] = None
    _siem_exporter: Optional[SIEMExporter] = None
    _retention_policies: Dict[str, RetentionPolicy] = {}

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
        self._start_siem_exporter()
        self._load_retention_policies()

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

    def _start_siem_exporter(self):
        """Start SIEM exporter if enabled"""
        if SIEM_ENABLED:
            self._siem_exporter = SIEMExporter()
            self._siem_exporter.start()

    def _load_retention_policies(self):
        """Load retention policies from configuration"""
        for category, days in RETENTION_CONFIG.items():
            self._retention_policies[category] = RetentionPolicy(
                category=category,
                retention_days=days,
                archive_after_days=min(days, AUDIT_ARCHIVE_DAYS)
            )

    def set_retention_policy(self, policy: RetentionPolicy):
        """Set or update a retention policy for a category"""
        self._retention_policies[policy.category] = policy

    def get_retention_policy(self, category: str) -> Optional[RetentionPolicy]:
        """Get retention policy for a category"""
        return self._retention_policies.get(category)

    def stop(self):
        """Stop the async writer thread and SIEM exporter"""
        if self._writer_thread:
            self._write_queue.put(None)
            self._writer_thread.join(timeout=5)
        if self._siem_exporter:
            self._siem_exporter.stop()

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
        severity: AuditSeverity = AuditSeverity.INFO,
        # SOC2/GDPR Compliance Fields
        old_value: Optional[Any] = None,
        new_value: Optional[Any] = None,
        data_classification: Optional[str] = None,
        legal_basis: Optional[str] = None,
        data_subject_id: Optional[str] = None,
        retention_days: Optional[int] = None,
        correlation_id: Optional[str] = None,
        source_system: Optional[str] = None,
        geo_location: Optional[str] = None,
        risk_score: Optional[int] = None
    ):
        """
        Log an audit event with full SOC2/GDPR compliance.

        This is the main logging method. All other methods call this.

        Args:
            category: Event category (authentication, data_access, etc.)
            action: Specific action taken
            resource_type: Type of resource affected
            resource_id: ID of the specific resource
            user_id: ID of the user performing the action
            username: Username of the actor
            tenant_id: Multi-tenant organization ID
            session_id: Session identifier
            ip_address: Client IP address
            user_agent: Browser/client user agent
            request_id: Unique request identifier
            endpoint: API endpoint accessed
            method: HTTP method used
            success: Whether the action succeeded
            error_code: Error code if failed
            error_message: Error message if failed
            details: Additional event details
            changes: Before/after changes dict
            severity: Event severity level
            old_value: Original value before change (SOC2/GDPR)
            new_value: New value after change (SOC2/GDPR)
            data_classification: Data classification level
            legal_basis: GDPR legal basis for processing
            data_subject_id: GDPR data subject identifier
            retention_days: Custom retention period
            correlation_id: ID to link related events
            source_system: Origin system for federated logs
            geo_location: Geographic location
            risk_score: Risk assessment score (0-100)
        """
        if not AUDIT_LOG_ENABLED:
            return

        # Determine severity based on action if not specified
        if severity == AuditSeverity.INFO:
            severity = self._determine_severity(action, success)

        # Redact PII from details and values
        safe_details = self._redact_pii(details or {})
        safe_changes = self._redact_pii(changes) if changes else None
        safe_old_value = self._redact_pii_value(old_value)
        safe_new_value = self._redact_pii_value(new_value)

        # Get retention days from policy if not specified
        if retention_days is None:
            cat_str = category.value if isinstance(category, AuditCategory) else category
            policy = self._retention_policies.get(cat_str)
            if policy:
                retention_days = policy.retention_days

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
            old_value=safe_old_value,
            new_value=safe_new_value,
            data_classification=data_classification,
            legal_basis=legal_basis,
            data_subject_id=data_subject_id,
            retention_days=retention_days,
            correlation_id=correlation_id,
            source_system=source_system or "plataforma_e",
            geo_location=geo_location,
            risk_score=risk_score,
            checksum="",
            previous_checksum=self._last_checksum
        )

        # Calculate checksum
        entry.checksum = self._calculate_checksum(entry)
        self._last_checksum = entry.checksum

        # Write entry to database
        if AUDIT_LOG_ASYNC:
            self._write_queue.put(entry)
        else:
            self._write_entry(entry)

        # Export to SIEM if enabled
        if self._siem_exporter:
            self._siem_exporter.export(entry)

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
            elif isinstance(value, list):
                result[key] = [self._redact_pii_value(v) for v in value]
            else:
                result[key] = value
        return result

    def _redact_pii_value(self, value: Any) -> Any:
        """Redact PII from a single value (dict, list, or primitive)"""
        if value is None:
            return None
        if isinstance(value, dict):
            return self._redact_pii(value)
        if isinstance(value, list):
            return [self._redact_pii_value(v) for v in value]
        return value

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
        success: bool = True,
        # GDPR compliance fields
        data_subject_id: Optional[str] = None,
        legal_basis: Optional[str] = None,
        data_classification: Optional[str] = None
    ):
        """Log data access event with GDPR compliance"""
        self.log(
            category=AuditCategory.DATA_ACCESS,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            user_id=user_id,
            username=username,
            tenant_id=tenant_id,
            success=success,
            details=details,
            data_subject_id=data_subject_id,
            legal_basis=legal_basis,
            data_classification=data_classification
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
        success: bool = True,
        # SOC2/GDPR compliance fields
        old_value: Optional[Any] = None,
        new_value: Optional[Any] = None,
        data_subject_id: Optional[str] = None,
        data_classification: Optional[str] = None,
        correlation_id: Optional[str] = None
    ):
        """Log data modification event with old/new values for SOC2/GDPR compliance"""
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
            success=success,
            old_value=old_value,
            new_value=new_value,
            data_subject_id=data_subject_id,
            data_classification=data_classification,
            correlation_id=correlation_id
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

    # -------------------------------------------------------------------------
    # SIEM Export Methods
    # -------------------------------------------------------------------------

    def export_to_siem(
        self,
        start_date: datetime,
        end_date: datetime,
        format: str = "json",
        siem_config: Optional[SIEMExportConfig] = None
    ) -> Dict[str, Any]:
        """
        Export audit logs to SIEM system.

        Args:
            start_date: Start of date range
            end_date: End of date range
            format: Export format (json, cef, syslog)
            siem_config: Optional SIEM configuration override

        Returns:
            Export result with count and status
        """
        try:
            from factory.database.audit_models import AuditLogEntry

            if SessionLocal is None:
                return {"success": False, "error": "Database not available"}

            db = SessionLocal()
            try:
                logs = db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp >= start_date,
                    AuditLogEntry.timestamp <= end_date
                ).order_by(AuditLogEntry.timestamp.asc()).all()

                # Convert to AuditEntry objects
                entries = []
                for log in logs:
                    entry = AuditEntry(
                        audit_id=log.audit_id,
                        timestamp=log.timestamp,
                        user_id=log.user_id,
                        username=log.username,
                        tenant_id=log.tenant_id,
                        session_id=log.session_id,
                        category=log.category,
                        action=log.action,
                        severity=log.severity,
                        resource_type=log.resource_type,
                        resource_id=log.resource_id,
                        ip_address=log.ip_address,
                        user_agent=log.user_agent,
                        request_id=log.request_id,
                        endpoint=log.endpoint,
                        method=log.method,
                        success=log.success,
                        error_code=log.error_code,
                        error_message=log.error_message,
                        details=log.details or {},
                        changes=log.changes,
                        checksum=log.checksum,
                        previous_checksum=log.previous_checksum
                    )
                    entries.append(entry)

                # Export using provided config or default exporter
                exporter = SIEMExporter(siem_config) if siem_config else self._siem_exporter
                if exporter is None:
                    exporter = SIEMExporter()

                for entry in entries:
                    exporter.export(entry)

                # Flush immediately
                exporter._flush_batch()

                return {
                    "success": True,
                    "exported_count": len(entries),
                    "start_date": start_date.isoformat(),
                    "end_date": end_date.isoformat(),
                    "format": format
                }

            finally:
                db.close()

        except Exception as e:
            return {"success": False, "error": str(e)}

    def export_for_download(
        self,
        start_date: datetime,
        end_date: datetime,
        format: str = "json",
        category: Optional[str] = None,
        user_id: Optional[int] = None
    ) -> bytes:
        """
        Export audit logs for download (compliance reports).

        Args:
            start_date: Start of date range
            end_date: End of date range
            format: Export format (json, csv, cef)
            category: Optional category filter
            user_id: Optional user filter

        Returns:
            Gzipped bytes of the export
        """
        try:
            from factory.database.audit_models import AuditLogEntry

            if SessionLocal is None:
                return b""

            db = SessionLocal()
            try:
                query = db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp >= start_date,
                    AuditLogEntry.timestamp <= end_date
                )

                if category:
                    query = query.filter(AuditLogEntry.category == category)
                if user_id:
                    query = query.filter(AuditLogEntry.user_id == user_id)

                logs = query.order_by(AuditLogEntry.timestamp.asc()).all()

                # Convert to AuditEntry objects
                entries = []
                for log in logs:
                    entry = AuditEntry(
                        audit_id=log.audit_id,
                        timestamp=log.timestamp,
                        user_id=log.user_id,
                        username=log.username,
                        tenant_id=log.tenant_id,
                        session_id=log.session_id,
                        category=log.category,
                        action=log.action,
                        severity=log.severity,
                        resource_type=log.resource_type,
                        resource_id=log.resource_id,
                        ip_address=log.ip_address,
                        user_agent=log.user_agent,
                        request_id=log.request_id,
                        endpoint=log.endpoint,
                        method=log.method,
                        success=log.success,
                        error_code=log.error_code,
                        error_message=log.error_message,
                        details=log.details or {},
                        changes=log.changes,
                        checksum=log.checksum,
                        previous_checksum=log.previous_checksum
                    )
                    entries.append(entry)

                # Use SIEMExporter's batch export
                exporter = SIEMExporter()
                return exporter.export_batch(entries, format)

            finally:
                db.close()

        except Exception as e:
            print(f"[AuditLogger] Export error: {e}")
            return b""

    def archive_old_logs(self, days: int = None) -> Dict[str, Any]:
        """
        Archive logs older than specified days to cold storage.

        Moves logs to archive table with compression.
        """
        if days is None:
            days = AUDIT_ARCHIVE_DAYS

        cutoff_date = datetime.utcnow() - timedelta(days=days)

        try:
            from factory.database.audit_models import AuditLogEntry, AuditLogArchive

            if SessionLocal is None:
                return {"success": False, "error": "Database not available"}

            db = SessionLocal()
            try:
                # Get logs to archive
                logs = db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp < cutoff_date
                ).all()

                if not logs:
                    return {"success": True, "archived_count": 0}

                # Create archive batch ID
                batch_id = f"ARCH-{uuid.uuid4().hex[:8].upper()}"

                # Archive each log
                archived_count = 0
                for log in logs:
                    archive_entry = AuditLogArchive(
                        audit_id=log.audit_id,
                        original_timestamp=log.timestamp,
                        data=log.to_dict(),
                        archive_batch=batch_id,
                        checksum=log.checksum
                    )
                    db.add(archive_entry)
                    db.delete(log)
                    archived_count += 1

                db.commit()

                return {
                    "success": True,
                    "archived_count": archived_count,
                    "batch_id": batch_id,
                    "cutoff_date": cutoff_date.isoformat()
                }

            finally:
                db.close()

        except Exception as e:
            return {"success": False, "error": str(e)}

    def get_gdpr_data_subject_logs(
        self,
        data_subject_id: str,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None
    ) -> List[Dict[str, Any]]:
        """
        Get all audit logs related to a specific data subject (GDPR compliance).

        Used for Data Subject Access Requests (DSAR).

        Args:
            data_subject_id: ID of the data subject
            start_date: Optional start date filter
            end_date: Optional end date filter

        Returns:
            List of audit log entries for the data subject
        """
        try:
            from factory.database.audit_models import AuditLogEntry

            if SessionLocal is None:
                return []

            db = SessionLocal()
            try:
                query = db.query(AuditLogEntry).filter(
                    AuditLogEntry.details.contains({"data_subject_id": data_subject_id})
                )

                if start_date:
                    query = query.filter(AuditLogEntry.timestamp >= start_date)
                if end_date:
                    query = query.filter(AuditLogEntry.timestamp <= end_date)

                logs = query.order_by(AuditLogEntry.timestamp.desc()).all()
                return [log.to_dict() for log in logs]

            finally:
                db.close()

        except Exception as e:
            print(f"[AuditLogger] GDPR query error: {e}")
            return []

    def generate_compliance_report(
        self,
        report_type: str,
        start_date: datetime,
        end_date: datetime,
        tenant_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Generate a compliance report for SOC2/GDPR audits.

        Args:
            report_type: Type of report (soc2, gdpr, iso27001)
            start_date: Report period start
            end_date: Report period end
            tenant_id: Optional tenant filter

        Returns:
            Compliance report with statistics and findings
        """
        try:
            from factory.database.audit_models import AuditLogEntry, ComplianceReport

            if SessionLocal is None:
                return {"success": False, "error": "Database not available"}

            db = SessionLocal()
            try:
                # Build query
                query = db.query(AuditLogEntry).filter(
                    AuditLogEntry.timestamp >= start_date,
                    AuditLogEntry.timestamp <= end_date
                )

                if tenant_id:
                    query = query.filter(AuditLogEntry.tenant_id == tenant_id)

                logs = query.all()

                # Calculate statistics
                total_events = len(logs)
                auth_events = sum(1 for l in logs if l.category == "authentication")
                security_events = sum(1 for l in logs if l.category == "security")
                failed_events = sum(1 for l in logs if not l.success)
                critical_events = sum(1 for l in logs if l.severity == "critical")

                # Group by category
                category_counts = {}
                for log in logs:
                    cat = log.category
                    category_counts[cat] = category_counts.get(cat, 0) + 1

                # Find potential issues
                findings = []

                # Check for brute force patterns
                failed_logins = [l for l in logs if l.action == "login_failure"]
                if len(failed_logins) > 10:
                    findings.append({
                        "severity": "warning",
                        "finding": f"High number of failed login attempts: {len(failed_logins)}",
                        "recommendation": "Review failed login patterns for potential brute force attacks"
                    })

                # Check for privilege escalation
                priv_events = [l for l in logs if l.action == "privilege_escalation"]
                if priv_events:
                    findings.append({
                        "severity": "critical",
                        "finding": f"Privilege escalation attempts detected: {len(priv_events)}",
                        "recommendation": "Immediately investigate these events"
                    })

                # Generate report
                report_id = f"RPT-{uuid.uuid4().hex[:8].upper()}"
                report = {
                    "report_id": report_id,
                    "report_type": report_type,
                    "period": {
                        "start": start_date.isoformat(),
                        "end": end_date.isoformat()
                    },
                    "tenant_id": tenant_id,
                    "summary": {
                        "total_events": total_events,
                        "authentication_events": auth_events,
                        "security_events": security_events,
                        "failed_events": failed_events,
                        "critical_events": critical_events
                    },
                    "category_breakdown": category_counts,
                    "findings": findings,
                    "compliance_status": "compliant" if not critical_events else "review_required",
                    "generated_at": datetime.utcnow().isoformat()
                }

                # Save report to database
                db_report = ComplianceReport(
                    report_id=report_id,
                    report_type=report_type,
                    title=f"{report_type.upper()} Compliance Report",
                    period_start=start_date,
                    period_end=end_date,
                    tenant_id=tenant_id,
                    summary=report["summary"],
                    findings=findings,
                    statistics=category_counts,
                    status="generated"
                )
                db.add(db_report)
                db.commit()

                return report

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
    "SIEMExporter",

    # Enums
    "AuditCategory",
    "AuditSeverity",
    "AuditAction",

    # Models
    "AuditEntry",
    "AuditQuery",
    "RetentionPolicy",
    "SIEMExportConfig",

    # Decorator
    "audit_log",

    # Middleware
    "AuditMiddleware",

    # Configuration
    "RETENTION_CONFIG",
    "SIEM_ENABLED",
    "AUDIT_RETENTION_DAYS",
    "AUDIT_ARCHIVE_DAYS",
]
