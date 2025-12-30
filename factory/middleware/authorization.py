# -*- coding: utf-8 -*-
"""
Authorization Middleware (Issue #290)
======================================

Middleware for enforcing permissions based on the persona system.

This middleware:
1. Extracts user persona from context
2. Maps HTTP method to action
3. Maps route to resource
4. Validates permission using personas.py
5. Returns 403 if permission denied

Usage:
    from factory.middleware.authorization import AuthorizationMiddleware

    app.add_middleware(AuthorizationMiddleware)

Author: Fabrica de Agentes - Terminal 4
"""

import os
import re
import logging
from typing import Optional, Dict, Any, List, Callable
from functools import wraps

from fastapi import Request, Response, HTTPException, Depends
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from factory.auth.personas import (
    check_access,
    get_persona_for_role,
    PersonaType,
    Resource,
    Action,
    persona_registry,
    permission_checker
)
from factory.middleware.tenant_middleware import (
    get_user_context,
    get_tenant_context,
    get_tenant_id
)

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# ROUTE TO RESOURCE MAPPING
# =============================================================================

ROUTE_RESOURCE_MAP = {
    # Stories
    r"^/api/stories.*": Resource.STORIES.value,
    r"^/api/story-tasks.*": Resource.TASKS.value,

    # Tasks
    r"^/api/tasks.*": Resource.TASKS.value,

    # Projects
    r"^/api/projects.*": Resource.PROJECTS.value,

    # Jobs & Workers
    r"^/api/v1/jobs.*": Resource.JOBS.value,
    r"^/api/v1/workers.*": Resource.WORKERS.value,
    r"^/api/jobs.*": Resource.JOBS.value,
    r"^/api/workers.*": Resource.WORKERS.value,

    # Users & Roles
    r"^/api/users.*": Resource.USERS.value,
    r"^/api/roles.*": Resource.ROLES.value,
    r"^/api/permissions.*": Resource.PERMISSIONS.value,

    # Tenant Management
    r"^/api/tenant.*": Resource.TENANT_SETTINGS.value,
    r"^/api/tenants.*": Resource.TENANTS.value,
    r"^/tenant-admin.*": Resource.TENANT_SETTINGS.value,

    # Platform (Super Admin)
    r"^/api/platform.*": Resource.TENANTS.value,
    r"^/platform.*": Resource.TENANTS.value,

    # Documents & Content
    r"^/api/stories/.*/docs.*": Resource.DOCUMENTS.value,
    r"^/api/docs.*": Resource.DOCUMENTS.value,
    r"^/api/documents.*": Resource.DOCUMENTS.value,
    r"^/api/attachments.*": Resource.ATTACHMENTS.value,
    r"^/api/upload.*": Resource.ATTACHMENTS.value,

    # Analytics
    r"^/api/metrics.*": Resource.METRICS.value,
    r"^/api/reports.*": Resource.REPORTS.value,
    r"^/api/audit.*": Resource.AUDIT_LOGS.value,

    # Settings & Integrations
    r"^/api/settings.*": Resource.SYSTEM_SETTINGS.value,
    r"^/api/integrations.*": Resource.INTEGRATIONS.value,
    r"^/api/webhooks.*": Resource.WEBHOOKS.value,
    r"^/api/api-keys.*": Resource.API_KEYS.value,

    # Billing
    r"^/api/billing.*": Resource.BILLING.value,

    # Epics & Sprints (part of projects)
    r"^/api/epics.*": Resource.PROJECTS.value,
    r"^/api/sprints.*": Resource.PROJECTS.value,

    # Chat
    r"^/api/chat.*": Resource.PROJECTS.value,

    # Search
    r"^/api/search.*": Resource.PROJECTS.value,

    # Designs
    r"^/api/designs.*": Resource.DESIGNS.value,
}

# HTTP Method to Action mapping
METHOD_ACTION_MAP = {
    "GET": Action.READ.value,
    "POST": Action.CREATE.value,
    "PUT": Action.UPDATE.value,
    "PATCH": Action.UPDATE.value,
    "DELETE": Action.DELETE.value,
    "HEAD": Action.READ.value,
    "OPTIONS": Action.READ.value,
}

# Special action overrides for specific endpoints
SPECIAL_ACTIONS = {
    # Jobs execution
    (r"^/api/v1/jobs/.*/execute$", "POST"): Action.EXECUTE.value,
    (r"^/api/jobs/.*/execute$", "POST"): Action.EXECUTE.value,

    # Approvals
    (r"^/api/stories/.*/approve$", "POST"): Action.APPROVE.value,
    (r"^/api/tasks/.*/approve$", "POST"): Action.APPROVE.value,

    # Exports
    (r"^/api/reports/.*/export$", "GET"): Action.EXPORT.value,
    (r"^/api/reports/.*/export$", "POST"): Action.EXPORT.value,

    # Imports
    (r"^/api/projects/import$", "POST"): Action.IMPORT.value,
    (r"^/api/stories/import$", "POST"): Action.IMPORT.value,

    # Manage actions
    (r"^/api/users/.*/roles$", "PUT"): Action.MANAGE.value,
    (r"^/api/tenant/members.*", "POST"): Action.MANAGE.value,
    (r"^/api/tenant/members.*", "DELETE"): Action.MANAGE.value,
}


# =============================================================================
# AUTHORIZATION MIDDLEWARE
# =============================================================================

class AuthorizationMiddleware(BaseHTTPMiddleware):
    """
    Authorization Middleware (Issue #290)

    Enforces permission checks on all protected routes using
    the persona system defined in factory/auth/personas.py.
    """

    # Paths that skip authorization (public + auth)
    # Issue #371: Added login/register/forgot-password to prevent auth loop
    SKIP_PATHS = [
        "/health",
        "/docs",
        "/openapi.json",
        "/redoc",
        "/api/public",
        "/api/v1/auth",
        "/api/auth",
        "/api/webhooks",
        "/static",
        "/favicon.ico",
        "/ws",
        "/api/personas",  # Personas info is public
        "/",  # Root dashboard
        # Issue #371: Public auth pages
        "/login",
        "/register",
        "/forgot-password",
        "/reset-password",
        "/verify-email",
        "/mfa",
        "/api/docs",
        # Issue #371: Debug endpoint
        "/api/debug",
        "/api/health",
    ]

    def __init__(
        self,
        app,
        skip_paths: List[str] = None,
        enabled: bool = True,
        log_denials: bool = True
    ):
        """
        Initialize authorization middleware.

        Args:
            app: FastAPI application
            skip_paths: Additional paths to skip
            enabled: Enable/disable middleware
            log_denials: Log permission denials
        """
        super().__init__(app)
        self.skip_paths = (skip_paths or []) + self.SKIP_PATHS
        self.enabled = enabled
        self.log_denials = log_denials

    async def dispatch(self, request: Request, call_next) -> Response:
        """Process request checking authorization"""

        # Skip if disabled
        if not self.enabled:
            return await call_next(request)

        path = request.url.path
        method = request.method

        # Skip public paths
        if self._should_skip(path):
            return await call_next(request)

        # Get user context
        user = get_user_context()

        # No user = let tenant middleware handle auth
        if not user:
            return await call_next(request)

        # Extract resource and action
        resource = self._get_resource(path)
        action = self._get_action(path, method)

        # No resource mapping = allow (unknown route)
        if not resource:
            return await call_next(request)

        # Get user role and check permission
        role = user.get("role", "VIEWER")
        tenant_id = get_tenant_id()

        context = {
            "tenant_id": tenant_id,
            "user_id": user.get("user_id"),
            "path": path,
            "method": method
        }

        # Check permission
        has_permission = check_access(role, resource, action, context)

        if not has_permission:
            if self.log_denials:
                logger.warning(
                    f"Permission denied: user={user.get('username')} "
                    f"role={role} resource={resource} action={action} "
                    f"path={path} tenant={tenant_id}"
                )

            return JSONResponse(
                status_code=403,
                content={
                    "detail": f"Permission denied. Role '{role}' cannot perform '{action}' on '{resource}'",
                    "code": "PERMISSION_DENIED",
                    "resource": resource,
                    "action": action,
                    "required_permission": f"{resource}:{action}"
                },
                headers={"X-Error-Code": "PERMISSION_DENIED"}
            )

        # Permission granted, continue
        return await call_next(request)

    def _should_skip(self, path: str) -> bool:
        """Check if path should skip authorization"""
        for skip in self.skip_paths:
            if path == skip or path.startswith(skip + "/") or path.startswith(skip + "?"):
                return True
        return False

    def _get_resource(self, path: str) -> Optional[str]:
        """Map path to resource"""
        for pattern, resource in ROUTE_RESOURCE_MAP.items():
            if re.match(pattern, path):
                return resource
        return None

    def _get_action(self, path: str, method: str) -> str:
        """Map path and method to action"""
        # Check special actions first
        for (pattern, req_method), action in SPECIAL_ACTIONS.items():
            if req_method == method and re.match(pattern, path):
                return action

        # Default method mapping
        return METHOD_ACTION_MAP.get(method, Action.READ.value)


# =============================================================================
# PERMISSION DECORATORS
# =============================================================================

def require_permission(resource: str, action: str):
    """
    Decorator requiring specific permission.

    Usage:
        @router.post("/stories")
        @require_permission("stories", "create")
        async def create_story(...):
            pass
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = get_user_context()

            if not user:
                raise HTTPException(
                    status_code=401,
                    detail="Authentication required"
                )

            role = user.get("role", "VIEWER")
            tenant_id = get_tenant_id()

            context = {
                "tenant_id": tenant_id,
                "user_id": user.get("user_id")
            }

            if not check_access(role, resource, action, context):
                raise HTTPException(
                    status_code=403,
                    detail=f"Permission denied. Role '{role}' cannot perform '{action}' on '{resource}'"
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_persona(persona_types: List[str]):
    """
    Decorator requiring specific persona type.

    Usage:
        @router.get("/platform/tenants")
        @require_persona(["super_admin"])
        async def list_all_tenants(...):
            pass
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = get_user_context()

            if not user:
                raise HTTPException(
                    status_code=401,
                    detail="Authentication required"
                )

            role = user.get("role", "VIEWER")
            persona = get_persona_for_role(role)

            if not persona:
                raise HTTPException(
                    status_code=403,
                    detail="Invalid user role"
                )

            if persona.persona_type.value not in persona_types:
                raise HTTPException(
                    status_code=403,
                    detail=f"Access restricted to personas: {', '.join(persona_types)}"
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_super_admin(func: Callable) -> Callable:
    """
    Decorator requiring super admin (platform owner).

    Usage:
        @router.get("/platform/overview")
        @require_super_admin
        async def platform_overview(...):
            pass
    """
    @wraps(func)
    async def wrapper(*args, **kwargs):
        user = get_user_context()

        if not user:
            raise HTTPException(
                status_code=401,
                detail="Authentication required"
            )

        role = user.get("role", "").upper()

        if role not in ["SUPER_ADMIN", "SUPERADMIN", "PLATFORM_ADMIN"]:
            raise HTTPException(
                status_code=403,
                detail="Platform administrator access required"
            )

        return await func(*args, **kwargs)
    return wrapper


def require_tenant_admin(func: Callable) -> Callable:
    """
    Decorator requiring tenant admin or higher.

    Usage:
        @router.get("/tenant/settings")
        @require_tenant_admin
        async def tenant_settings(...):
            pass
    """
    @wraps(func)
    async def wrapper(*args, **kwargs):
        user = get_user_context()

        if not user:
            raise HTTPException(
                status_code=401,
                detail="Authentication required"
            )

        role = user.get("role", "").upper()

        admin_roles = ["SUPER_ADMIN", "SUPERADMIN", "PLATFORM_ADMIN", "ADMIN", "OWNER", "TENANT_ADMIN"]
        if role not in admin_roles:
            raise HTTPException(
                status_code=403,
                detail="Tenant administrator access required"
            )

        return await func(*args, **kwargs)
    return wrapper


# =============================================================================
# FASTAPI DEPENDENCIES
# =============================================================================

class PermissionDependency:
    """
    FastAPI dependency for permission checking.

    Usage:
        create_story_permission = PermissionDependency("stories", "create")

        @router.post("/stories")
        async def create_story(
            _: bool = Depends(create_story_permission),
            ...
        ):
            pass
    """

    def __init__(self, resource: str, action: str):
        self.resource = resource
        self.action = action

    async def __call__(self, request: Request) -> bool:
        user = get_user_context()

        if not user:
            raise HTTPException(
                status_code=401,
                detail="Authentication required"
            )

        role = user.get("role", "VIEWER")
        tenant_id = get_tenant_id()

        context = {
            "tenant_id": tenant_id,
            "user_id": user.get("user_id"),
            "path": str(request.url.path)
        }

        if not check_access(role, self.resource, self.action, context):
            raise HTTPException(
                status_code=403,
                detail=f"Permission denied: {self.resource}:{self.action}"
            )

        return True


async def get_user_permissions(request: Request) -> Dict[str, Any]:
    """
    Dependency returning user's permissions.

    Usage:
        @router.get("/me/permissions")
        async def my_permissions(
            permissions: dict = Depends(get_user_permissions)
        ):
            return permissions
    """
    user = get_user_context()

    if not user:
        return {"permissions": [], "persona": None}

    role = user.get("role", "VIEWER")
    persona = get_persona_for_role(role)

    if not persona:
        return {"permissions": [], "persona": None}

    return {
        "persona": persona.to_dict(),
        "permissions": list(persona.get_all_permissions()),
        "allowed_features": persona.allowed_features,
        "dashboard_type": persona.dashboard_type,
        "level": persona.level
    }


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_allowed_actions_for_resource(role: str, resource: str) -> List[str]:
    """
    Get list of allowed actions for a resource based on role.

    Args:
        role: User role (ADMIN, DEVELOPER, etc)
        resource: Resource name (stories, tasks, etc)

    Returns:
        List of allowed actions
    """
    persona = get_persona_for_role(role)
    if not persona:
        return []

    allowed = []
    for action in Action:
        if persona.has_permission(resource, action.value):
            allowed.append(action.value)

    return allowed


def can_user_access(user: Dict[str, Any], resource: str, action: str) -> bool:
    """
    Check if user can access resource with action.

    Args:
        user: User dict with 'role' key
        resource: Resource name
        action: Action name

    Returns:
        True if allowed
    """
    if not user:
        return False

    role = user.get("role", "VIEWER")
    return check_access(role, resource, action)


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Middleware
    "AuthorizationMiddleware",
    # Decorators
    "require_permission",
    "require_persona",
    "require_super_admin",
    "require_tenant_admin",
    # Dependencies
    "PermissionDependency",
    "get_user_permissions",
    # Helpers
    "get_allowed_actions_for_resource",
    "can_user_access",
    # Maps
    "ROUTE_RESOURCE_MAP",
    "METHOD_ACTION_MAP",
]
