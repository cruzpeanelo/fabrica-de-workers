# -*- coding: utf-8 -*-
"""
User Context & Navigation Routes (Issues #292, #293)
=====================================================

API endpoints for:
- User persona information
- Tenant selector (switch between tenants)
- Navigation configuration per persona
- User preferences

Author: Plataforma E - Terminal 4
"""

import logging
from datetime import datetime
from typing import Optional, List, Dict, Any
from pydantic import BaseModel

from fastapi import APIRouter, HTTPException, Depends, Response, Request
from fastapi.responses import JSONResponse

from factory.auth.personas import (
    get_persona_for_role,
    persona_registry,
    PersonaType,
    Resource,
    Action
)
from factory.middleware.tenant_middleware import (
    get_user_context,
    get_tenant_context,
    get_tenant_id,
    set_tenant_context
)

# Configure logging
logger = logging.getLogger(__name__)

# Create router
router = APIRouter(prefix="/api/user", tags=["user-context"])


# =============================================================================
# NAVIGATION CONFIGURATION BY PERSONA
# =============================================================================

NAVIGATION_BY_PERSONA = {
    "super_admin": {
        "primary": [
            {"id": "platform", "label": "Platform", "icon": "globe", "route": "/platform", "badge": "Admin"},
            {"id": "tenants", "label": "Tenants", "icon": "building", "route": "/platform/tenants"},
            {"id": "divider1", "type": "divider"},
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "projects", "label": "Projects", "icon": "folder", "route": "/projects"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
        ],
        "secondary": [
            {"id": "settings", "label": "Platform Settings", "icon": "cog", "route": "/platform/settings"},
            {"id": "billing", "label": "Billing", "icon": "credit-card", "route": "/platform/billing"},
            {"id": "modules", "label": "Modules", "icon": "puzzle", "route": "/platform/modules"},
        ],
        "dashboard_type": "admin"
    },
    "admin": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "projects", "label": "Projects", "icon": "folder", "route": "/projects"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
            {"id": "tasks", "label": "Tasks", "icon": "check-square", "route": "/tasks"},
            {"id": "divider1", "type": "divider"},
            {"id": "team", "label": "Team", "icon": "users", "route": "/tenant-admin/members"},
        ],
        "secondary": [
            {"id": "settings", "label": "Settings", "icon": "cog", "route": "/tenant-admin/settings"},
            {"id": "integrations", "label": "Integrations", "icon": "plug", "route": "/tenant-admin/integrations"},
            {"id": "billing", "label": "Billing", "icon": "credit-card", "route": "/tenant-admin/billing"},
            {"id": "audit", "label": "Audit Logs", "icon": "shield", "route": "/tenant-admin/audit-logs"},
        ],
        "dashboard_type": "admin"
    },
    "project_manager": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "projects", "label": "Projects", "icon": "folder", "route": "/projects"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
            {"id": "sprints", "label": "Sprints", "icon": "calendar", "route": "/sprints"},
            {"id": "epics", "label": "Epics", "icon": "layers", "route": "/epics"},
            {"id": "team", "label": "Team", "icon": "users", "route": "/team"},
        ],
        "secondary": [
            {"id": "reports", "label": "Reports", "icon": "bar-chart", "route": "/reports"},
            {"id": "timeline", "label": "Timeline", "icon": "clock", "route": "/timeline"},
        ],
        "dashboard_type": "manager"
    },
    "tech_lead": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
            {"id": "tasks", "label": "Tasks", "icon": "check-square", "route": "/tasks"},
            {"id": "jobs", "label": "Jobs", "icon": "cpu", "route": "/jobs"},
            {"id": "workers", "label": "Workers", "icon": "robot", "route": "/workers"},
        ],
        "secondary": [
            {"id": "code", "label": "Code Review", "icon": "code", "route": "/code-review"},
            {"id": "metrics", "label": "Metrics", "icon": "activity", "route": "/metrics"},
            {"id": "docs", "label": "Documentation", "icon": "file-text", "route": "/docs"},
        ],
        "dashboard_type": "manager"
    },
    "developer": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "my-tasks", "label": "My Tasks", "icon": "check-square", "route": "/tasks?assigned=me"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
            {"id": "jobs", "label": "Jobs", "icon": "cpu", "route": "/jobs"},
        ],
        "secondary": [
            {"id": "docs", "label": "Documentation", "icon": "file-text", "route": "/docs"},
            {"id": "chat", "label": "AI Assistant", "icon": "message-circle", "route": "/chat"},
        ],
        "dashboard_type": "developer"
    },
    "qa_engineer": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "testing", "label": "Testing", "icon": "check-circle", "route": "/stories?status=testing"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories"},
            {"id": "tasks", "label": "Tasks", "icon": "check-square", "route": "/tasks"},
        ],
        "secondary": [
            {"id": "test-docs", "label": "Test Cases", "icon": "clipboard", "route": "/docs?type=test"},
            {"id": "bugs", "label": "Bug Reports", "icon": "bug", "route": "/bugs"},
        ],
        "dashboard_type": "developer"
    },
    "stakeholder": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "progress", "label": "Progress", "icon": "trending-up", "route": "/progress"},
            {"id": "reports", "label": "Reports", "icon": "bar-chart", "route": "/reports"},
        ],
        "secondary": [
            {"id": "timeline", "label": "Timeline", "icon": "clock", "route": "/timeline"},
        ],
        "dashboard_type": "stakeholder"
    },
    "viewer": {
        "primary": [
            {"id": "dashboard", "label": "Dashboard", "icon": "dashboard", "route": "/"},
            {"id": "stories", "label": "Stories", "icon": "book", "route": "/stories", "readonly": True},
            {"id": "progress", "label": "Progress", "icon": "trending-up", "route": "/progress"},
        ],
        "secondary": [],
        "dashboard_type": "viewer"
    }
}


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class TenantSwitch(BaseModel):
    """Schema for switching tenant"""
    tenant_id: str


# =============================================================================
# USER PERSONA ENDPOINT
# =============================================================================

@router.get("/persona")
async def get_user_persona():
    """
    Get current user's persona information and navigation config.

    Returns the user's persona type, permissions, and navigation menu
    adapted to their role.
    """
    user = get_user_context()

    if not user:
        # Return viewer config for unauthenticated
        return {
            "success": True,
            "authenticated": False,
            "persona": {
                "type": "viewer",
                "name": "Viewer",
                "level": 100,
                "dashboard_type": "viewer"
            },
            "permissions": [],
            "navigation": NAVIGATION_BY_PERSONA.get("viewer", {}),
            "features": []
        }

    role = user.get("role", "VIEWER")
    persona = get_persona_for_role(role)

    if not persona:
        # Fallback to viewer
        persona = persona_registry.get_persona(PersonaType.VIEWER)

    # Get navigation config
    persona_key = persona.persona_type.value.lower()
    navigation = NAVIGATION_BY_PERSONA.get(persona_key, NAVIGATION_BY_PERSONA["viewer"])

    return {
        "success": True,
        "authenticated": True,
        "user": {
            "id": user.get("user_id"),
            "username": user.get("username"),
            "email": user.get("email"),
            "role": role
        },
        "persona": {
            "type": persona.persona_type.value,
            "name": persona.name,
            "description": persona.description,
            "level": persona.level,
            "dashboard_type": persona.dashboard_type
        },
        "permissions": list(persona.get_all_permissions()),
        "allowed_features": persona.allowed_features,
        "navigation": navigation,
        "restrictions": persona.restrictions
    }


@router.get("/permissions")
async def get_user_permissions():
    """
    Get detailed permissions for current user.

    Returns a map of resources to allowed actions.
    """
    user = get_user_context()

    if not user:
        return {"success": True, "authenticated": False, "permissions": {}}

    role = user.get("role", "VIEWER")
    persona = get_persona_for_role(role)

    if not persona:
        return {"success": True, "authenticated": True, "permissions": {}}

    # Build permissions map by resource
    permissions_map = {}
    all_perms = persona.get_all_permissions()

    for perm in all_perms:
        if ":" in perm:
            resource, action = perm.split(":", 1)
            if resource not in permissions_map:
                permissions_map[resource] = []
            permissions_map[resource].append(action)

    return {
        "success": True,
        "authenticated": True,
        "role": role,
        "persona_type": persona.persona_type.value,
        "permissions": permissions_map,
        "can_access": {
            "platform_portal": persona.persona_type == PersonaType.SUPER_ADMIN,
            "tenant_admin": persona.level <= 10,  # Admin level
            "manager_features": persona.level <= 40,
            "developer_features": persona.level <= 60,
            "edit_features": persona.level < 100
        }
    }


# =============================================================================
# TENANT SELECTOR ENDPOINTS
# =============================================================================

@router.get("/tenants")
async def get_user_tenants(request: Request):
    """
    Get list of tenants the user belongs to, including branding info.

    For Super Admin, shows all tenants.
    For regular users, shows only their tenants.
    """
    # Try to get user from middleware context
    user = get_user_context()

    # If not in context, try to decode from Authorization header
    if not user:
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Bearer "):
            token = auth_header.replace("Bearer ", "")
            try:
                from factory.api.auth import decode_token
                payload = decode_token(token)
                if payload:
                    # decode_token returns object with username/role attributes
                    user = {
                        "username": getattr(payload, "username", None) or getattr(payload, "sub", None),
                        "role": getattr(payload, "role", "VIEWER")
                    }
                    # Get user_id from database
                    from factory.database.connection import SessionLocal
                    from factory.database.models import User as UserModel
                    db = SessionLocal()
                    try:
                        db_user = db.query(UserModel).filter(UserModel.username == user["username"]).first()
                        if db_user:
                            user["user_id"] = db_user.id
                            logger.info(f"User context from JWT: {user['username']}, ID: {user['user_id']}")
                    finally:
                        db.close()
            except Exception as e:
                logger.error(f"Error decoding token: {e}")

    if not user:
        return {"success": True, "tenants": [], "current": None}

    role = user.get("role", "VIEWER")
    current_tenant_id = get_tenant_id()

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Tenant, TenantMember, BrandingConfig

        db = SessionLocal()
        try:
            tenants = []

            def get_tenant_branding(tenant_id):
                """Get branding config for a tenant"""
                branding = db.query(BrandingConfig).filter(
                    BrandingConfig.tenant_id == tenant_id
                ).first()
                if branding:
                    colors = branding.colors or {}
                    return {
                        "primary_color": colors.get("primary", "#003B4A"),
                        "secondary_color": colors.get("secondary", "#FF6C00"),
                        "logo_url": branding.logo_url,
                        "display_name": branding.display_name
                    }
                return None

            # Super Admin sees all tenants
            if role.upper() in ["SUPER_ADMIN", "SUPERADMIN", "PLATFORM_ADMIN"]:
                all_tenants = db.query(Tenant).filter(Tenant.status.in_(["active", "trial"])).limit(50).all()
                for t in all_tenants:
                    tenants.append({
                        "tenant_id": t.tenant_id,
                        "name": t.name,
                        "slug": t.slug,
                        "plan": t.plan,
                        "role": "super_admin",
                        "is_current": t.tenant_id == current_tenant_id,
                        "branding": get_tenant_branding(t.tenant_id)
                    })

                # Add platform view option
                tenants.insert(0, {
                    "tenant_id": "__platform__",
                    "name": "Platform View",
                    "slug": "platform",
                    "plan": "platform",
                    "role": "super_admin",
                    "is_platform": True,
                    "is_current": current_tenant_id is None or current_tenant_id == "__platform__",
                    "branding": {
                        "primary_color": "#1E293B",
                        "secondary_color": "#3B82F6",
                        "logo_url": "/static/logos/platform.png",
                        "display_name": "Platform Admin"
                    }
                })
            else:
                # Regular user - show only their tenants
                memberships = db.query(TenantMember).filter(
                    TenantMember.user_id == user.get("user_id"),
                    TenantMember.status == "active"
                ).all()

                for m in memberships:
                    tenant = db.query(Tenant).filter(
                        Tenant.tenant_id == m.tenant_id
                    ).first()
                    if tenant:
                        tenants.append({
                            "tenant_id": tenant.tenant_id,
                            "name": tenant.name,
                            "slug": tenant.slug,
                            "plan": tenant.plan,
                            "role": m.tenant_role,
                            "is_current": tenant.tenant_id == current_tenant_id,
                            "branding": get_tenant_branding(tenant.tenant_id)
                        })

            return {
                "success": True,
                "tenants": tenants,
                "current": current_tenant_id,
                "can_switch": len(tenants) > 1
            }

        finally:
            db.close()

    except Exception as e:
        # Issue #318: Propagar erros em vez de retornar dados mock
        logger.error(f"Error getting user tenants: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Database error while getting user tenants: {str(e)}"
        )


@router.post("/switch-tenant")
async def switch_tenant(data: TenantSwitch, response: Response):
    """
    Switch to a different tenant.

    Updates the current_tenant cookie.
    """
    user = get_user_context()

    if not user:
        raise HTTPException(status_code=401, detail="Authentication required")

    role = user.get("role", "VIEWER")
    target_tenant_id = data.tenant_id

    # Platform view for super admin
    if target_tenant_id == "__platform__":
        if role.upper() not in ["SUPER_ADMIN", "SUPERADMIN", "PLATFORM_ADMIN"]:
            raise HTTPException(status_code=403, detail="Platform view requires super admin")

        response.set_cookie(
            key="current_tenant",
            value="__platform__",
            max_age=30 * 24 * 60 * 60,
            httponly=True,
            samesite="lax"
        )
        return {
            "success": True,
            "tenant_id": "__platform__",
            "message": "Switched to Platform view"
        }

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Tenant, TenantMember

        db = SessionLocal()
        try:
            # Verify tenant exists
            tenant = db.query(Tenant).filter(
                Tenant.tenant_id == target_tenant_id
            ).first()

            if not tenant:
                raise HTTPException(status_code=404, detail="Tenant not found")

            # For super admin, allow any tenant
            if role.upper() not in ["SUPER_ADMIN", "SUPERADMIN", "PLATFORM_ADMIN"]:
                # Verify membership
                member = db.query(TenantMember).filter(
                    TenantMember.tenant_id == target_tenant_id,
                    TenantMember.user_id == user.get("user_id"),
                    TenantMember.status == "active"
                ).first()

                if not member:
                    raise HTTPException(
                        status_code=403,
                        detail="You are not a member of this tenant"
                    )

            # Set cookie
            response.set_cookie(
                key="current_tenant",
                value=target_tenant_id,
                max_age=30 * 24 * 60 * 60,
                httponly=True,
                samesite="lax"
            )

            logger.info(f"User {user.get('username')} switched to tenant {target_tenant_id}")

            return {
                "success": True,
                "tenant_id": target_tenant_id,
                "tenant_name": tenant.name,
                "message": f"Switched to {tenant.name}"
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error switching tenant: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/current-tenant")
async def get_current_tenant():
    """
    Get current tenant information.
    """
    tenant = get_tenant_context()
    tenant_id = get_tenant_id()

    if not tenant and tenant_id != "__platform__":
        return {
            "success": True,
            "tenant": None,
            "is_platform_view": False
        }

    if tenant_id == "__platform__":
        return {
            "success": True,
            "tenant": {
                "tenant_id": "__platform__",
                "name": "Platform View",
                "slug": "platform"
            },
            "is_platform_view": True
        }

    return {
        "success": True,
        "tenant": {
            "tenant_id": tenant.get("tenant_id"),
            "name": tenant.get("name"),
            "slug": tenant.get("slug"),
            "plan": tenant.get("plan"),
            "status": tenant.get("status"),
            "features": tenant.get("features", {})
        },
        "is_platform_view": False
    }


# =============================================================================
# NAVIGATION ENDPOINTS
# =============================================================================

@router.get("/navigation")
async def get_navigation():
    """
    Get navigation configuration for current user.

    Returns menu items adapted to user's persona.
    """
    user = get_user_context()

    if not user:
        return {
            "success": True,
            "navigation": NAVIGATION_BY_PERSONA["viewer"]
        }

    role = user.get("role", "VIEWER")
    persona = get_persona_for_role(role)

    if not persona:
        return {
            "success": True,
            "navigation": NAVIGATION_BY_PERSONA["viewer"]
        }

    persona_key = persona.persona_type.value.lower()
    navigation = NAVIGATION_BY_PERSONA.get(persona_key, NAVIGATION_BY_PERSONA["viewer"])

    # Add user-specific items
    navigation["user"] = {
        "username": user.get("username"),
        "role": role,
        "persona": persona_key
    }

    return {
        "success": True,
        "navigation": navigation
    }


@router.get("/dashboard-type")
async def get_dashboard_type():
    """
    Get the dashboard type for current user.

    Used to render the appropriate dashboard view.
    """
    user = get_user_context()

    if not user:
        return {
            "success": True,
            "dashboard_type": "viewer",
            "components": ["progress", "stories_readonly"]
        }

    role = user.get("role", "VIEWER")
    persona = get_persona_for_role(role)

    if not persona:
        return {
            "success": True,
            "dashboard_type": "viewer",
            "components": ["progress", "stories_readonly"]
        }

    # Define components by dashboard type
    dashboard_components = {
        "admin": [
            "overview_metrics",
            "tenant_stats",
            "recent_activity",
            "alerts",
            "quick_actions",
            "member_activity"
        ],
        "manager": [
            "sprint_burndown",
            "velocity_chart",
            "stories_by_status",
            "team_workload",
            "blockers",
            "upcoming_deadlines"
        ],
        "developer": [
            "my_tasks",
            "current_stories",
            "recent_jobs",
            "quick_actions",
            "ai_assistant"
        ],
        "stakeholder": [
            "overall_progress",
            "project_status",
            "budget_status",
            "milestone_timeline",
            "kpi_summary"
        ],
        "viewer": [
            "progress",
            "stories_readonly",
            "project_status"
        ]
    }

    dashboard_type = persona.dashboard_type
    components = dashboard_components.get(dashboard_type, dashboard_components["viewer"])

    return {
        "success": True,
        "dashboard_type": dashboard_type,
        "persona_type": persona.persona_type.value,
        "components": components
    }


# =============================================================================
# REGISTER ROUTER
# =============================================================================

def register_user_context_routes(app):
    """Register user context routes with FastAPI app"""
    app.include_router(router)
    logger.info("[UserContext] User context routes registered at /api/user")


# Export router
__all__ = ["router", "register_user_context_routes", "NAVIGATION_BY_PERSONA"]
