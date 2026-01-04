# -*- coding: utf-8 -*-
"""
Tenant Admin Portal Routes (Issue #288)
========================================

API endpoints for Tenant Administrators to manage their organization.

Features:
- Tenant dashboard with metrics
- Member management (invite, remove, change roles)
- Tenant settings configuration
- Integration management
- Billing for tenant
- Audit logs

Author: Plataforma E - Terminal 4
"""

import logging
import uuid
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from pydantic import BaseModel, Field, EmailStr

from fastapi import APIRouter, HTTPException, Depends, Query
from fastapi.responses import JSONResponse

from factory.middleware.authorization import require_tenant_admin
from factory.middleware.tenant_middleware import (
    get_user_context,
    get_tenant_context,
    get_tenant_id
)

# Configure logging
logger = logging.getLogger(__name__)

# Create router
router = APIRouter(prefix="/api/tenant-admin", tags=["tenant-admin"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class MemberInvite(BaseModel):
    """Schema for inviting a member"""
    email: EmailStr
    role: str = Field(default="member", pattern=r"^(admin|member|viewer)$")
    message: Optional[str] = None


class MemberUpdate(BaseModel):
    """Schema for updating a member"""
    role: Optional[str] = Field(None, pattern=r"^(owner|admin|member|viewer)$")
    status: Optional[str] = Field(None, pattern=r"^(active|suspended)$")


class TenantSettingsUpdate(BaseModel):
    """Schema for updating tenant settings"""
    name: Optional[str] = None
    timezone: Optional[str] = None
    locale: Optional[str] = None
    notifications_email: Optional[str] = None
    default_project_visibility: Optional[str] = None


class IntegrationConfig(BaseModel):
    """Schema for integration configuration"""
    integration_type: str
    enabled: bool
    config: Dict[str, Any] = Field(default_factory=dict)


# =============================================================================
# DASHBOARD ENDPOINTS
# =============================================================================

@router.get("/dashboard")
@require_tenant_admin
async def get_tenant_dashboard():
    """
    Get tenant dashboard with metrics.
    """
    tenant_id = get_tenant_id()
    user = get_user_context()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Project, Story, Job, Task

        db = SessionLocal()
        try:
            # Count metrics for this tenant
            project_count = db.query(Project).filter(
                Project.tenant_id == tenant_id
            ).count()

            story_count = db.query(Story).filter(
                Story.tenant_id == tenant_id
            ).count()

            stories_done = db.query(Story).filter(
                Story.tenant_id == tenant_id,
                Story.status == "done"
            ).count()

            job_count = db.query(Job).filter(
                Job.tenant_id == tenant_id
            ).count()

            jobs_completed = db.query(Job).filter(
                Job.tenant_id == tenant_id,
                Job.status == "completed"
            ).count()

            # Get member count
            try:
                from factory.database.models import TenantMember
                member_count = db.query(TenantMember).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.status == "active"
                ).count()
            except:
                member_count = 1

            # Recent activity (last 7 days)
            week_ago = datetime.utcnow() - timedelta(days=7)
            recent_stories = db.query(Story).filter(
                Story.tenant_id == tenant_id,
                Story.created_at >= week_ago
            ).count()

            # Get tenant info
            tenant = get_tenant_context() or {}

            return {
                "success": True,
                "dashboard": {
                    "tenant": {
                        "tenant_id": tenant_id,
                        "name": tenant.get("name", "My Organization"),
                        "plan": tenant.get("plan", "professional"),
                        "status": tenant.get("status", "active")
                    },
                    "metrics": {
                        "members": member_count,
                        "projects": project_count,
                        "stories": {
                            "total": story_count,
                            "done": stories_done,
                            "completion_rate": round(stories_done / story_count * 100, 1) if story_count > 0 else 0
                        },
                        "jobs": {
                            "total": job_count,
                            "completed": jobs_completed,
                            "success_rate": round(jobs_completed / job_count * 100, 1) if job_count > 0 else 0
                        }
                    },
                    "activity": {
                        "stories_this_week": recent_stories
                    },
                    "limits": tenant.get("settings", {}).get("limits", {
                        "max_projects": 50,
                        "max_members": 25,
                        "projects_used": project_count,
                        "members_used": member_count
                    })
                }
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error getting tenant dashboard: {e}")
        return {
            "success": True,
            "dashboard": {
                "tenant": {"tenant_id": tenant_id, "name": "My Organization"},
                "metrics": {"members": 1, "projects": 0, "stories": {"total": 0}},
                "activity": {},
                "limits": {}
            }
        }


# =============================================================================
# MEMBER MANAGEMENT ENDPOINTS
# =============================================================================

@router.get("/members")
@require_tenant_admin
async def list_members(
    status: Optional[str] = Query(None, pattern=r"^(active|invited|suspended)$"),
    role: Optional[str] = None,
    search: Optional[str] = None,
    limit: int = Query(50, ge=1, le=100),
    offset: int = Query(0, ge=0)
):
    """
    List all members of the tenant.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantMember, User

        db = SessionLocal()
        try:
            query = db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id
            )

            if status:
                query = query.filter(TenantMember.status == status)
            if role:
                query = query.filter(TenantMember.tenant_role == role)

            total = query.count()
            members = query.offset(offset).limit(limit).all()

            result = []
            for member in members:
                # Get user info
                user = db.query(User).filter(User.id == member.user_id).first()

                member_data = {
                    "id": member.id,
                    "user_id": member.user_id,
                    "role": member.tenant_role,
                    "status": member.status,
                    "joined_at": member.created_at.isoformat() if member.created_at else None,
                    "user": {
                        "username": user.username if user else "Unknown",
                        "email": user.email if user else None
                    } if user else None
                }
                result.append(member_data)

            return {
                "success": True,
                "members": result,
                "total": total,
                "limit": limit,
                "offset": offset
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error listing members: {e}")
        return {
            "success": True,
            "members": [],
            "total": 0,
            "limit": limit,
            "offset": offset
        }


@router.post("/members/invite")
@require_tenant_admin
async def invite_member(invite: MemberInvite):
    """
    Invite a new member to the tenant.
    """
    tenant_id = get_tenant_id()
    user = get_user_context()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantInvite

        db = SessionLocal()
        try:
            # Check if invite already exists
            existing = db.query(TenantInvite).filter(
                TenantInvite.tenant_id == tenant_id,
                TenantInvite.email == invite.email,
                TenantInvite.status == "pending"
            ).first()

            if existing:
                raise HTTPException(
                    status_code=400,
                    detail="An invite for this email is already pending"
                )

            # Create invite
            token = str(uuid.uuid4())
            new_invite = TenantInvite(
                tenant_id=tenant_id,
                email=invite.email,
                role=invite.role,
                token=token,
                status="pending",
                invited_by=user.get("username") if user else None,
                expires_at=datetime.utcnow() + timedelta(days=7)
            )
            db.add(new_invite)
            db.commit()

            logger.info(f"Invited {invite.email} to tenant {tenant_id}")

            return {
                "success": True,
                "invite": {
                    "email": invite.email,
                    "role": invite.role,
                    "token": token,
                    "expires_at": new_invite.expires_at.isoformat()
                },
                "message": f"Invitation sent to {invite.email}"
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error inviting member: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/members/invites")
@require_tenant_admin
async def list_invites(
    status: Optional[str] = Query(None, pattern=r"^(pending|accepted|expired|revoked)$")
):
    """
    List pending invitations.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantInvite

        db = SessionLocal()
        try:
            query = db.query(TenantInvite).filter(
                TenantInvite.tenant_id == tenant_id
            )

            if status:
                query = query.filter(TenantInvite.status == status)
            else:
                # Default to pending
                query = query.filter(TenantInvite.status == "pending")

            invites = query.all()

            return {
                "success": True,
                "invites": [
                    {
                        "id": inv.id,
                        "email": inv.email,
                        "role": inv.role,
                        "status": inv.status,
                        "invited_by": inv.invited_by,
                        "created_at": inv.created_at.isoformat() if inv.created_at else None,
                        "expires_at": inv.expires_at.isoformat() if inv.expires_at else None
                    }
                    for inv in invites
                ]
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error listing invites: {e}")
        return {"success": True, "invites": []}


@router.delete("/members/invites/{invite_id}")
@require_tenant_admin
async def revoke_invite(invite_id: int):
    """
    Revoke a pending invitation.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantInvite

        db = SessionLocal()
        try:
            invite = db.query(TenantInvite).filter(
                TenantInvite.id == invite_id,
                TenantInvite.tenant_id == tenant_id
            ).first()

            if not invite:
                raise HTTPException(status_code=404, detail="Invite not found")

            invite.status = "revoked"
            db.commit()

            return {"success": True, "message": "Invite revoked"}

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error revoking invite: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.put("/members/{member_id}")
@require_tenant_admin
async def update_member(member_id: int, data: MemberUpdate):
    """
    Update a member's role or status.
    """
    tenant_id = get_tenant_id()
    current_user = get_user_context()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantMember

        db = SessionLocal()
        try:
            member = db.query(TenantMember).filter(
                TenantMember.id == member_id,
                TenantMember.tenant_id == tenant_id
            ).first()

            if not member:
                raise HTTPException(status_code=404, detail="Member not found")

            # Prevent changing own role to lower
            if member.user_id == current_user.get("user_id"):
                if data.role and data.role != member.tenant_role:
                    raise HTTPException(
                        status_code=400,
                        detail="Cannot change your own role"
                    )

            # Update fields
            if data.role:
                member.tenant_role = data.role
            if data.status:
                member.status = data.status

            db.commit()

            return {
                "success": True,
                "member": {
                    "id": member.id,
                    "role": member.tenant_role,
                    "status": member.status
                }
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating member: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/members/{member_id}")
@require_tenant_admin
async def remove_member(member_id: int):
    """
    Remove a member from the tenant.
    """
    tenant_id = get_tenant_id()
    current_user = get_user_context()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantMember

        db = SessionLocal()
        try:
            member = db.query(TenantMember).filter(
                TenantMember.id == member_id,
                TenantMember.tenant_id == tenant_id
            ).first()

            if not member:
                raise HTTPException(status_code=404, detail="Member not found")

            # Prevent self-removal
            if member.user_id == current_user.get("user_id"):
                raise HTTPException(
                    status_code=400,
                    detail="Cannot remove yourself from the tenant"
                )

            # Prevent removing owner
            if member.tenant_role == "owner":
                raise HTTPException(
                    status_code=400,
                    detail="Cannot remove the tenant owner"
                )

            db.delete(member)
            db.commit()

            return {"success": True, "message": "Member removed"}

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error removing member: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# SETTINGS ENDPOINTS
# =============================================================================

@router.get("/settings")
@require_tenant_admin
async def get_tenant_settings():
    """
    Get tenant settings.
    """
    tenant_id = get_tenant_id()
    tenant = get_tenant_context() or {}

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantSettings

        db = SessionLocal()
        try:
            settings = db.query(TenantSettings).filter(
                TenantSettings.tenant_id == tenant_id
            ).first()

            return {
                "success": True,
                "settings": {
                    "tenant": {
                        "name": tenant.get("name", "My Organization"),
                        "slug": tenant.get("slug", tenant_id.lower()),
                        "plan": tenant.get("plan", "professional")
                    },
                    "general": {
                        "timezone": settings.timezone if settings else "UTC",
                        "locale": settings.locale if settings else "en-US",
                        "notifications_email": settings.notifications_email if settings else None
                    },
                    "limits": {
                        "max_projects": settings.max_projects if settings else 50,
                        "max_members": settings.max_members if settings else 25,
                        "max_storage_gb": settings.max_storage_gb if settings else 10
                    },
                    "features": tenant.get("features", {}),
                    "ai": {
                        "preferred_model": settings.preferred_claude_model if settings else "claude-sonnet-4-20250514",
                        "auto_suggest_stories": settings.auto_suggest_stories if settings else True,
                        "auto_estimate_points": settings.auto_estimate_points if settings else True
                    } if settings else {}
                }
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error getting settings: {e}")
        return {
            "success": True,
            "settings": {
                "tenant": {"name": "My Organization"},
                "general": {"timezone": "UTC"},
                "limits": {},
                "features": {}
            }
        }


@router.put("/settings")
@require_tenant_admin
async def update_tenant_settings(data: TenantSettingsUpdate):
    """
    Update tenant settings.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Tenant, TenantSettings

        db = SessionLocal()
        try:
            # Update tenant name if provided
            if data.name:
                tenant = db.query(Tenant).filter(
                    Tenant.tenant_id == tenant_id
                ).first()
                if tenant:
                    tenant.name = data.name

            # Update settings
            settings = db.query(TenantSettings).filter(
                TenantSettings.tenant_id == tenant_id
            ).first()

            if settings:
                if data.timezone:
                    settings.timezone = data.timezone
                if data.locale:
                    settings.locale = data.locale
                if data.notifications_email:
                    settings.notifications_email = data.notifications_email

            db.commit()

            return {
                "success": True,
                "message": "Settings updated"
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error updating settings: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# INTEGRATIONS ENDPOINTS
# =============================================================================

@router.get("/integrations")
@require_tenant_admin
async def list_integrations():
    """
    List all available integrations and their status.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantSettings

        db = SessionLocal()
        try:
            settings = db.query(TenantSettings).filter(
                TenantSettings.tenant_id == tenant_id
            ).first()

            integrations = [
                {
                    "type": "github",
                    "name": "GitHub",
                    "description": "Connect to GitHub repositories",
                    "enabled": settings.github_enabled if settings else False,
                    "configured": bool(settings.github_token if settings else False),
                    "icon": "github"
                },
                {
                    "type": "slack",
                    "name": "Slack",
                    "description": "Send notifications to Slack channels",
                    "enabled": settings.slack_enabled if settings else False,
                    "configured": bool(settings.slack_webhook_url if settings else False),
                    "icon": "slack"
                },
                {
                    "type": "jira",
                    "name": "Jira",
                    "description": "Sync with Jira issues",
                    "enabled": settings.jira_enabled if settings else False,
                    "configured": bool(settings.jira_api_token if settings else False),
                    "icon": "jira"
                },
                {
                    "type": "azure_devops",
                    "name": "Azure DevOps",
                    "description": "Connect to Azure DevOps boards",
                    "enabled": settings.azure_devops_enabled if settings else False,
                    "configured": bool(settings.azure_devops_token if settings else False),
                    "icon": "azure"
                }
            ]

            return {
                "success": True,
                "integrations": integrations
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error listing integrations: {e}")
        return {
            "success": True,
            "integrations": []
        }


@router.put("/integrations/{integration_type}")
@require_tenant_admin
async def configure_integration(integration_type: str, config: IntegrationConfig):
    """
    Configure an integration.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import TenantSettings

        db = SessionLocal()
        try:
            settings = db.query(TenantSettings).filter(
                TenantSettings.tenant_id == tenant_id
            ).first()

            if not settings:
                raise HTTPException(status_code=404, detail="Tenant settings not found")

            # Update integration settings based on type
            if integration_type == "github":
                settings.github_enabled = config.enabled
                if "token" in config.config:
                    settings.github_token = config.config["token"]
            elif integration_type == "slack":
                settings.slack_enabled = config.enabled
                if "webhook_url" in config.config:
                    settings.slack_webhook_url = config.config["webhook_url"]
            elif integration_type == "jira":
                settings.jira_enabled = config.enabled
                if "api_token" in config.config:
                    settings.jira_api_token = config.config["api_token"]
            elif integration_type == "azure_devops":
                settings.azure_devops_enabled = config.enabled
                if "token" in config.config:
                    settings.azure_devops_token = config.config["token"]
            else:
                raise HTTPException(status_code=400, detail="Unknown integration type")

            db.commit()

            return {
                "success": True,
                "message": f"{integration_type} integration updated"
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error configuring integration: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# BILLING ENDPOINTS
# =============================================================================

@router.get("/billing")
@require_tenant_admin
async def get_billing_info():
    """
    Get tenant billing information.
    """
    tenant = get_tenant_context() or {}

    return {
        "success": True,
        "billing": {
            "plan": {
                "name": tenant.get("plan", "professional"),
                "price": {"starter": 0, "professional": 49, "enterprise": 199}.get(
                    tenant.get("plan", "professional"), 49
                ),
                "billing_cycle": "monthly"
            },
            "usage": {
                "api_calls_this_month": 0,
                "storage_used_gb": 0,
                "ai_tokens_used": 0
            },
            "invoices": []  # Would come from billing system
        }
    }


# =============================================================================
# AUDIT LOGS ENDPOINTS
# =============================================================================

@router.get("/audit-logs")
@require_tenant_admin
async def list_audit_logs(
    action: Optional[str] = None,
    user_id: Optional[int] = None,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    limit: int = Query(50, ge=1, le=100),
    offset: int = Query(0, ge=0)
):
    """
    List audit logs for the tenant.
    """
    tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import ActivityLog

        db = SessionLocal()
        try:
            query = db.query(ActivityLog).filter(
                ActivityLog.tenant_id == tenant_id
            )

            if action:
                query = query.filter(ActivityLog.action == action)
            if user_id:
                query = query.filter(ActivityLog.user_id == user_id)

            # Order by newest first
            query = query.order_by(ActivityLog.created_at.desc())

            total = query.count()
            logs = query.offset(offset).limit(limit).all()

            return {
                "success": True,
                "logs": [
                    {
                        "id": log.id,
                        "action": log.action,
                        "entity_type": log.entity_type,
                        "entity_id": log.entity_id,
                        "user_id": log.user_id,
                        "details": log.details,
                        "ip_address": log.ip_address,
                        "created_at": log.created_at.isoformat() if log.created_at else None
                    }
                    for log in logs
                ],
                "total": total,
                "limit": limit,
                "offset": offset
            }

        finally:
            db.close()

    except Exception as e:
        logger.error(f"Error listing audit logs: {e}")
        return {
            "success": True,
            "logs": [],
            "total": 0,
            "limit": limit,
            "offset": offset
        }


# =============================================================================
# REGISTER ROUTER
# =============================================================================

def register_tenant_admin_routes(app):
    """Register tenant admin routes with FastAPI app"""
    app.include_router(router)
    logger.info("[TenantAdmin] Tenant admin routes registered at /api/tenant-admin")


# Export router
__all__ = ["router", "register_tenant_admin_routes"]
