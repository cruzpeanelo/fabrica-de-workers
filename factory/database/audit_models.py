# -*- coding: utf-8 -*-
"""
Audit Log Database Models - Issue #86
Plataforma E v6.5

SQLAlchemy models for storing audit logs.
Designed for SOC2/GDPR compliance with:
- Immutable entries (no update/delete)
- Checksum chain for integrity verification
- Efficient querying with proper indexes
- Retention policy support
"""

from datetime import datetime
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, Boolean, Index, event
from sqlalchemy.orm import relationship

try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# AUDIT LOG ENTRY
# =============================================================================

class AuditLogEntry(Base):
    """
    Audit log entry for compliance tracking.

    Stores all security-relevant events including:
    - Authentication events
    - Authorization changes
    - Data access/modification
    - Configuration changes
    - Security incidents

    IMPORTANT: This table is append-only.
    Entries should NEVER be updated or deleted in production.
    """
    __tablename__ = "audit_log_entries"

    # Primary key
    id = Column(Integer, primary_key=True, autoincrement=True)
    audit_id = Column(String(50), unique=True, nullable=False, index=True)

    # Timestamp (immutable)
    timestamp = Column(DateTime, nullable=False, default=datetime.utcnow, index=True)

    # Actor information
    user_id = Column(Integer, nullable=True, index=True)
    username = Column(String(100), nullable=True, index=True)
    tenant_id = Column(String(50), nullable=True, index=True)
    session_id = Column(String(100), nullable=True)

    # Event classification
    category = Column(String(50), nullable=False, index=True)
    action = Column(String(50), nullable=False, index=True)
    severity = Column(String(20), nullable=False, default="info", index=True)

    # Target resource
    resource_type = Column(String(50), nullable=False, index=True)
    resource_id = Column(String(100), nullable=True, index=True)

    # Request context
    ip_address = Column(String(50), nullable=True, index=True)
    user_agent = Column(String(500), nullable=True)
    request_id = Column(String(100), nullable=True)
    endpoint = Column(String(200), nullable=True)
    method = Column(String(10), nullable=True)

    # Result
    success = Column(Boolean, nullable=False, default=True, index=True)
    error_code = Column(String(50), nullable=True)
    error_message = Column(Text, nullable=True)

    # Event details (JSON)
    details = Column(JSON, default=dict)
    changes = Column(JSON, nullable=True)  # Before/after for modifications

    # Integrity verification
    checksum = Column(String(64), nullable=False)  # SHA256 HMAC
    previous_checksum = Column(String(64), nullable=True)  # Chain to previous entry

    # Metadata
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)

    # Composite indexes for common queries
    __table_args__ = (
        Index("idx_audit_user_time", "user_id", "timestamp"),
        Index("idx_audit_tenant_time", "tenant_id", "timestamp"),
        Index("idx_audit_category_action", "category", "action"),
        Index("idx_audit_resource", "resource_type", "resource_id"),
        Index("idx_audit_security", "category", "severity", "success"),
    )

    def to_dict(self):
        """Convert to dictionary for API responses"""
        return {
            "audit_id": self.audit_id,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None,
            "user_id": self.user_id,
            "username": self.username,
            "tenant_id": self.tenant_id,
            "session_id": self.session_id,
            "category": self.category,
            "action": self.action,
            "severity": self.severity,
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
            "details": self.details or {},
            "changes": self.changes,
            "checksum": self.checksum,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<AuditLogEntry {self.audit_id}: {self.action} on {self.resource_type}>"


# =============================================================================
# AUDIT LOG ARCHIVE
# =============================================================================

class AuditLogArchive(Base):
    """
    Archived audit logs for long-term retention.

    Logs older than the active retention period are moved here.
    This allows for efficient querying of recent logs while
    maintaining compliance with retention requirements.
    """
    __tablename__ = "audit_log_archive"

    id = Column(Integer, primary_key=True, autoincrement=True)
    audit_id = Column(String(50), nullable=False, index=True)

    # Original timestamp
    original_timestamp = Column(DateTime, nullable=False, index=True)

    # Archived data (compressed JSON)
    data = Column(JSON, nullable=False)

    # Archive metadata
    archived_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    archive_batch = Column(String(50), nullable=True)  # Batch ID for bulk operations

    # Integrity
    checksum = Column(String(64), nullable=False)

    def to_dict(self):
        return {
            "audit_id": self.audit_id,
            "original_timestamp": self.original_timestamp.isoformat() if self.original_timestamp else None,
            "data": self.data,
            "archived_at": self.archived_at.isoformat() if self.archived_at else None
        }


# =============================================================================
# COMPLIANCE REPORT
# =============================================================================

class ComplianceReport(Base):
    """
    Generated compliance reports for auditors.

    Stores pre-generated reports for SOC2/GDPR audits.
    """
    __tablename__ = "compliance_reports"

    id = Column(Integer, primary_key=True, autoincrement=True)
    report_id = Column(String(50), unique=True, nullable=False, index=True)

    # Report type
    report_type = Column(String(50), nullable=False)  # soc2, gdpr, iso27001, custom
    title = Column(String(200), nullable=False)

    # Time range
    period_start = Column(DateTime, nullable=False)
    period_end = Column(DateTime, nullable=False)

    # Scope
    tenant_id = Column(String(50), nullable=True, index=True)
    scope = Column(JSON, default=dict)  # Categories, resources included

    # Report content
    summary = Column(JSON, default=dict)
    findings = Column(JSON, default=list)
    statistics = Column(JSON, default=dict)
    full_report = Column(Text, nullable=True)  # Full report content

    # Status
    status = Column(String(20), default="generated")  # generated, reviewed, approved
    reviewed_by = Column(String(100), nullable=True)
    reviewed_at = Column(DateTime, nullable=True)

    # Metadata
    generated_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    generated_by = Column(String(100), default="system")

    def to_dict(self):
        return {
            "report_id": self.report_id,
            "report_type": self.report_type,
            "title": self.title,
            "period_start": self.period_start.isoformat() if self.period_start else None,
            "period_end": self.period_end.isoformat() if self.period_end else None,
            "tenant_id": self.tenant_id,
            "scope": self.scope,
            "summary": self.summary,
            "findings": self.findings,
            "statistics": self.statistics,
            "status": self.status,
            "reviewed_by": self.reviewed_by,
            "reviewed_at": self.reviewed_at.isoformat() if self.reviewed_at else None,
            "generated_at": self.generated_at.isoformat() if self.generated_at else None
        }


# =============================================================================
# DATA ACCESS LOG (GDPR Specific)
# =============================================================================

class DataAccessLog(Base):
    """
    GDPR-specific data access logging.

    Tracks all access to personal data for GDPR compliance.
    Required for:
    - Data Subject Access Requests (DSAR)
    - Right to be Forgotten verification
    - Data breach impact assessment
    """
    __tablename__ = "data_access_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    log_id = Column(String(50), unique=True, nullable=False, index=True)

    # Who accessed
    user_id = Column(Integer, nullable=True, index=True)
    username = Column(String(100), nullable=True)
    tenant_id = Column(String(50), nullable=True, index=True)

    # What was accessed
    data_subject_id = Column(String(100), nullable=False, index=True)  # ID of person whose data was accessed
    data_category = Column(String(50), nullable=False)  # personal, sensitive, financial, health
    data_fields = Column(JSON, default=list)  # Which fields were accessed

    # Access type
    access_type = Column(String(30), nullable=False)  # view, export, modify, delete

    # Legal basis
    legal_basis = Column(String(50), nullable=True)  # consent, contract, legal_obligation, etc.

    # Context
    purpose = Column(String(200), nullable=True)  # Why data was accessed
    ip_address = Column(String(50), nullable=True)

    # Timestamp
    accessed_at = Column(DateTime, default=datetime.utcnow, nullable=False, index=True)

    # Composite indexes
    __table_args__ = (
        Index("idx_data_access_subject", "data_subject_id", "accessed_at"),
        Index("idx_data_access_user", "user_id", "accessed_at"),
    )

    def to_dict(self):
        return {
            "log_id": self.log_id,
            "user_id": self.user_id,
            "username": self.username,
            "tenant_id": self.tenant_id,
            "data_subject_id": self.data_subject_id,
            "data_category": self.data_category,
            "data_fields": self.data_fields,
            "access_type": self.access_type,
            "legal_basis": self.legal_basis,
            "purpose": self.purpose,
            "ip_address": self.ip_address,
            "accessed_at": self.accessed_at.isoformat() if self.accessed_at else None
        }


# =============================================================================
# PREVENT UPDATES/DELETES ON AUDIT LOGS
# =============================================================================

@event.listens_for(AuditLogEntry, "before_update")
def prevent_audit_update(mapper, connection, target):
    """Prevent updates to audit log entries"""
    raise ValueError("Audit log entries are immutable and cannot be updated")


@event.listens_for(AuditLogEntry, "before_delete")
def prevent_audit_delete(mapper, connection, target):
    """Prevent deletes from audit log (in production)"""
    import os
    if os.getenv("ENV", "development") != "development":
        raise ValueError("Audit log entries cannot be deleted in production")


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "AuditLogEntry",
    "AuditLogArchive",
    "ComplianceReport",
    "DataAccessLog",
]
