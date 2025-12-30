# -*- coding: utf-8 -*-
"""
Audit Log Routes - Issue #339
=============================
API endpoints for querying audit logs.
"""

import csv
import io
from datetime import datetime, timedelta
from typing import Optional, List
from fastapi import APIRouter, Query, HTTPException, Response
from pydantic import BaseModel

from .service import get_audit_service

router = APIRouter(prefix="/api/audit-logs", tags=["Audit"])


class AuditLogResponse(BaseModel):
    """Audit log entry response."""
    id: int
    timestamp: str
    tenant_id: Optional[str]
    user_id: Optional[str]
    username: Optional[str]
    action: str
    resource_type: str
    resource_id: Optional[str]
    status: str
    ip_address: Optional[str]
    request_path: Optional[str]


class AuditLogListResponse(BaseModel):
    """Paginated audit log list response."""
    items: List[dict]
    total: int
    limit: int
    offset: int


@router.get("", response_model=AuditLogListResponse)
async def list_audit_logs(
    tenant_id: Optional[str] = Query(None, description="Filter by tenant"),
    user_id: Optional[str] = Query(None, description="Filter by user"),
    action: Optional[str] = Query(None, description="Filter by action (CREATE, UPDATE, DELETE, etc)"),
    resource_type: Optional[str] = Query(None, description="Filter by resource type"),
    resource_id: Optional[str] = Query(None, description="Filter by resource ID"),
    from_date: Optional[str] = Query(None, description="Start date (ISO format)"),
    to_date: Optional[str] = Query(None, description="End date (ISO format)"),
    status: Optional[str] = Query(None, description="Filter by status (success, failure, error)"),
    limit: int = Query(100, ge=1, le=1000, description="Max results"),
    offset: int = Query(0, ge=0, description="Pagination offset")
):
    """
    List audit log entries with filtering and pagination.

    Requires admin permissions.
    """
    service = get_audit_service()

    # Parse dates
    parsed_from = None
    parsed_to = None
    if from_date:
        try:
            parsed_from = datetime.fromisoformat(from_date.replace("Z", "+00:00"))
        except ValueError:
            raise HTTPException(400, "Invalid from_date format")
    if to_date:
        try:
            parsed_to = datetime.fromisoformat(to_date.replace("Z", "+00:00"))
        except ValueError:
            raise HTTPException(400, "Invalid to_date format")

    items = service.query(
        tenant_id=tenant_id,
        user_id=user_id,
        action=action,
        resource_type=resource_type,
        resource_id=resource_id,
        from_date=parsed_from,
        to_date=parsed_to,
        status=status,
        limit=limit,
        offset=offset
    )

    return {
        "items": items,
        "total": len(items),  # TODO: Add count query
        "limit": limit,
        "offset": offset
    }


@router.get("/{audit_id}")
async def get_audit_log(audit_id: int):
    """
    Get a single audit log entry by ID.

    Requires admin permissions.
    """
    service = get_audit_service()
    entry = service.get_by_id(audit_id)

    if not entry:
        raise HTTPException(404, "Audit log entry not found")

    return entry


@router.get("/export/csv")
async def export_audit_logs_csv(
    tenant_id: Optional[str] = Query(None),
    user_id: Optional[str] = Query(None),
    action: Optional[str] = Query(None),
    resource_type: Optional[str] = Query(None),
    from_date: Optional[str] = Query(None),
    to_date: Optional[str] = Query(None),
    limit: int = Query(10000, ge=1, le=100000)
):
    """
    Export audit logs as CSV file.

    Requires admin permissions.
    """
    service = get_audit_service()

    # Parse dates
    parsed_from = None
    parsed_to = None
    if from_date:
        parsed_from = datetime.fromisoformat(from_date.replace("Z", "+00:00"))
    if to_date:
        parsed_to = datetime.fromisoformat(to_date.replace("Z", "+00:00"))

    items = service.query(
        tenant_id=tenant_id,
        user_id=user_id,
        action=action,
        resource_type=resource_type,
        from_date=parsed_from,
        to_date=parsed_to,
        limit=limit
    )

    # Generate CSV
    output = io.StringIO()
    writer = csv.writer(output)

    # Header
    writer.writerow([
        "ID", "Timestamp", "Tenant ID", "User ID", "Username",
        "Action", "Resource Type", "Resource ID", "Status",
        "IP Address", "Request Path"
    ])

    # Data
    for item in items:
        writer.writerow([
            item.get("id"),
            item.get("timestamp"),
            item.get("tenant_id"),
            item.get("user_id"),
            item.get("username"),
            item.get("action"),
            item.get("resource_type"),
            item.get("resource_id"),
            item.get("status"),
            item.get("ip_address"),
            item.get("request_path")
        ])

    content = output.getvalue()
    filename = f"audit_logs_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}.csv"

    return Response(
        content=content,
        media_type="text/csv",
        headers={"Content-Disposition": f"attachment; filename={filename}"}
    )


@router.post("/cleanup")
async def cleanup_old_logs(
    retention_days: int = Query(90, ge=1, le=365, description="Days to retain")
):
    """
    Delete audit logs older than retention period.

    Requires super_admin permissions.
    """
    service = get_audit_service()
    deleted = service.cleanup(retention_days)

    return {
        "deleted": deleted,
        "retention_days": retention_days,
        "message": f"Deleted {deleted} entries older than {retention_days} days"
    }
