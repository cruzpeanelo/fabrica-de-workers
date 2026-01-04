# -*- coding: utf-8 -*-
"""
Global Tenant Middleware (Issue #105)
======================================

Middleware for extracting and managing tenant context across all requests.

Features:
- Extract tenant_id from JWT token
- Extract tenant_id from X-Tenant-ID header
- Inject tenant context in all queries
- Support tenant switching for admins
- Thread-safe context management

Usage:
    from factory.middleware.tenant_middleware import GlobalTenantMiddleware

    app.add_middleware(GlobalTenantMiddleware)

    # In routes
    @router.get("/projects")
    async def list_projects(tenant: dict = Depends(get_required_tenant)):
        tenant_id = tenant["tenant_id"]
        ...

Author: Plataforma E

VERSION: Issue #371 Fix - 2025-12-30 - Added /login to PUBLIC_PATHS
"""

# Issue #371: Version marker to verify code is loaded
_MIDDLEWARE_VERSION = "371-fix-2025-12-30"
print(f"[TenantMiddleware] Loading version: {_MIDDLEWARE_VERSION}")

import os
import logging
from datetime import datetime
from contextvars import ContextVar
from typing import Optional, Dict, Any, List, Callable
from functools import wraps

from fastapi import Request, Response, HTTPException, Depends
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONTEXT VARIABLES (Thread-Safe)
# =============================================================================

# Current tenant context
_tenant_context: ContextVar[Optional[Dict[str, Any]]] = ContextVar(
    "tenant_context", default=None
)

# Current user context
_user_context: ContextVar[Optional[Dict[str, Any]]] = ContextVar(
    "user_context", default=None
)

# Original tenant (for admin switching)
_original_tenant: ContextVar[Optional[Dict[str, Any]]] = ContextVar(
    "original_tenant", default=None
)

# Is admin switching tenants?
_is_tenant_switched: ContextVar[bool] = ContextVar(
    "is_tenant_switched", default=False
)


def get_tenant_context() -> Optional[Dict[str, Any]]:
    """Get current tenant from context"""
    return _tenant_context.get()


def get_tenant_id() -> Optional[str]:
    """Get only tenant_id from context"""
    tenant = _tenant_context.get()
    return tenant.get("tenant_id") if tenant else None


def get_user_context() -> Optional[Dict[str, Any]]:
    """Get current user from context"""
    return _user_context.get()


def get_original_tenant() -> Optional[Dict[str, Any]]:
    """Get original tenant (before admin switch)"""
    return _original_tenant.get()


def is_tenant_switched() -> bool:
    """Check if admin is viewing another tenant"""
    return _is_tenant_switched.get()


def set_tenant_context(tenant: Optional[Dict[str, Any]]) -> None:
    """Set tenant in current context"""
    _tenant_context.set(tenant)


def set_user_context(user: Optional[Dict[str, Any]]) -> None:
    """Set user in current context"""
    _user_context.set(user)


def set_original_tenant(tenant: Optional[Dict[str, Any]]) -> None:
    """Set original tenant (for admin switching)"""
    _original_tenant.set(tenant)


def set_tenant_switched(switched: bool) -> None:
    """Set tenant switched flag"""
    _is_tenant_switched.set(switched)


def clear_context() -> None:
    """Clear all context"""
    _tenant_context.set(None)
    _user_context.set(None)
    _original_tenant.set(None)
    _is_tenant_switched.set(False)


# =============================================================================
# GLOBAL TENANT MIDDLEWARE
# =============================================================================

class GlobalTenantMiddleware(BaseHTTPMiddleware):
    """
    Global Tenant Middleware (Issue #105)

    This middleware:
    1. Extracts tenant_id from JWT token or X-Tenant-ID header
    2. Validates user belongs to tenant
    3. Sets tenant and user context for the request
    4. Supports admin tenant switching via X-Switch-Tenant-ID header
    5. Applies to ALL routes (except public ones)

    Usage:
        from factory.middleware.tenant_middleware import GlobalTenantMiddleware

        app.add_middleware(GlobalTenantMiddleware)
    """

    # Paths that don't require tenant/authentication
    # Issue #371: Added HTML auth pages to prevent auth loop
    PUBLIC_PATHS = [
        "/health",
        "/docs",
        "/openapi.json",
        "/redoc",
        "/api/public",
        "/api/v1/auth/login",
        "/api/v1/auth/register",
        "/api/v1/auth/forgot-password",
        "/api/auth/login",
        "/api/auth/register",
        "/api/webhooks",
        "/static",
        "/favicon.ico",
        "/ws",
        # Issue #371: HTML auth pages
        "/",
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

    # Admin roles that can switch tenants
    ADMIN_ROLES = ["ADMIN", "SUPER_ADMIN", "PLATFORM_ADMIN"]

    def __init__(
        self,
        app,
        jwt_secret: str = None,
        jwt_algorithm: str = "HS256",
        public_paths: List[str] = None,
        admin_roles: List[str] = None,
        require_tenant: bool = True
    ):
        """
        Initialize the middleware.

        Args:
            app: FastAPI application
            jwt_secret: JWT secret key (defaults to env)
            jwt_algorithm: JWT algorithm (default HS256)
            public_paths: Additional public paths
            admin_roles: Roles that can switch tenants
            require_tenant: Require tenant for non-public routes
        """
        super().__init__(app)
        self.jwt_secret = jwt_secret or self._get_jwt_secret()
        self.jwt_algorithm = jwt_algorithm
        self.public_paths = (public_paths or []) + self.PUBLIC_PATHS
        self.admin_roles = admin_roles or self.ADMIN_ROLES
        self.require_tenant = require_tenant

    def _get_jwt_secret(self) -> str:
        """Get JWT secret from environment or auth module"""
        # Try environment variable
        secret = os.getenv("JWT_SECRET_KEY")
        if secret and secret != "your-super-secret-key-change-this-in-production":
            return secret

        # Try auth module
        try:
            from factory.api.auth import SECRET_KEY
            return SECRET_KEY
        except ImportError:
            pass

        # Default (development only)
        return "development-secret-key"

    async def dispatch(self, request: Request, call_next) -> Response:
        """Process request extracting tenant context"""

        # Clear previous context
        clear_context()

        path = request.url.path

        # Issue #371 DEBUG: Log path checking for auth pages
        if path in ["/login", "/register", "/forgot-password", "/mfa"]:
            logger.info(f"[Issue #371 DEBUG] Auth page request: path='{path}' public_paths={self.public_paths[:5]}...")

        # Check public paths
        if self._is_public_path(path):
            # Issue #371 DEBUG: Confirm path recognized as public
            if path in ["/login", "/register", "/forgot-password", "/mfa"]:
                logger.info(f"[Issue #371 DEBUG] Path '{path}' recognized as PUBLIC - allowing access")
            return await call_next(request)

        # Extract auth from JWT
        auth_result = await self._extract_auth(request)

        if auth_result:
            tenant_data, user_data = auth_result

            # Handle admin tenant switching
            switch_tenant_id = request.headers.get("X-Switch-Tenant-ID")
            if switch_tenant_id and user_data:
                switch_result = await self._handle_tenant_switch(
                    switch_tenant_id, tenant_data, user_data, request
                )
                if isinstance(switch_result, Response):
                    return switch_result
                tenant_data = switch_result

            if tenant_data:
                # Validate tenant status
                status_error = self._validate_tenant_status(tenant_data)
                if status_error:
                    return status_error

                # Validate user membership (skip for switched tenants if admin)
                if not is_tenant_switched() and user_data:
                    membership_valid = await self._validate_tenant_membership(
                        tenant_data.get("tenant_id"),
                        user_data.get("user_id")
                    )
                    if not membership_valid:
                        return JSONResponse(
                            status_code=403,
                            content={
                                "detail": "User does not belong to this tenant",
                                "code": "NOT_MEMBER"
                            },
                            headers={"X-Error-Code": "NOT_MEMBER"}
                        )

                # Set context
                set_tenant_context(tenant_data)

            if user_data:
                set_user_context(user_data)

        elif self.require_tenant:
            # No auth and tenant required
            # Issue #371 DEBUG: Log when returning AUTH_REQUIRED
            logger.warning(
                f"[Issue #371 DEBUG] AUTH_REQUIRED for path='{path}' - "
                f"is_public={self._is_public_path(path)} require_tenant={self.require_tenant}"
            )
            return JSONResponse(
                status_code=401,
                content={
                    "detail": "Authentication required. Provide Authorization header with valid JWT.",
                    "code": "AUTH_REQUIRED"
                },
                headers={"WWW-Authenticate": "Bearer"}
            )

        # Process request
        try:
            response = await call_next(request)

            # Add tenant headers
            tenant_id = get_tenant_id()
            if tenant_id:
                response.headers["X-Tenant-ID"] = tenant_id

            if is_tenant_switched():
                response.headers["X-Tenant-Switched"] = "true"
                original = get_original_tenant()
                if original:
                    response.headers["X-Original-Tenant-ID"] = original.get("tenant_id", "")

            return response

        finally:
            # Clear context after request
            clear_context()

    def _is_public_path(self, path: str) -> bool:
        """Check if path is public"""
        for public in self.public_paths:
            if path == public or path.startswith(public):
                return True
        return False

    def _validate_tenant_status(self, tenant_data: Dict[str, Any]) -> Optional[Response]:
        """Validate tenant status, return error response if invalid"""
        status = tenant_data.get("status")

        if status == "suspended":
            return JSONResponse(
                status_code=403,
                content={
                    "detail": "Tenant account is suspended. Please contact support.",
                    "code": "TENANT_SUSPENDED"
                },
                headers={"X-Error-Code": "TENANT_SUSPENDED"}
            )

        if status == "cancelled":
            return JSONResponse(
                status_code=403,
                content={
                    "detail": "Tenant account has been cancelled.",
                    "code": "TENANT_CANCELLED"
                },
                headers={"X-Error-Code": "TENANT_CANCELLED"}
            )

        if status not in ["active", "trial", "trialing", "pending"]:
            return JSONResponse(
                status_code=403,
                content={
                    "detail": f"Tenant status '{status}' does not allow access.",
                    "code": "TENANT_INACTIVE"
                },
                headers={"X-Error-Code": "TENANT_INACTIVE"}
            )

        return None

    async def _extract_auth(self, request: Request) -> Optional[tuple]:
        """
        Extract tenant and user from JWT, headers, or subdomain.

        Returns:
            Tuple (tenant_data, user_data) or None
        """
        auth_header = request.headers.get("Authorization", "")

        # Try JWT first
        if auth_header.startswith("Bearer "):
            return await self._extract_from_jwt(auth_header, request)

        # Fallback to X-Tenant-ID header
        tenant_id = request.headers.get("X-Tenant-ID")
        if tenant_id:
            tenant_data = await self._resolve_tenant(tenant_id)
            return (tenant_data, None) if tenant_data else None

        # Issue #368 T0-FIX: Mobile apps can send subdomain in header
        tenant_subdomain = request.headers.get("X-Tenant-Subdomain")
        if tenant_subdomain:
            logger.debug(f"[Issue #368] Mobile tenant header: {tenant_subdomain}")
            tenant_data = await self._resolve_tenant_by_subdomain(tenant_subdomain)
            if tenant_data:
                return (tenant_data, None)

        # Issue #413 T0-FIX: Extract from subdomain (e.g., tenant1.app.com)
        host = request.headers.get("Host", "")
        if host:
            subdomain = self._extract_subdomain(host)
            if subdomain:
                logger.debug(f"[Issue #413] Extracted subdomain: {subdomain} from host: {host}")
                tenant_data = await self._resolve_tenant_by_subdomain(subdomain)
                if tenant_data:
                    return (tenant_data, None)

        return None

    def _extract_subdomain(self, host: str) -> Optional[str]:
        """Extract subdomain from host header. Issue #413 fix."""
        # Remove port if present
        if ":" in host:
            host = host.split(":")[0]

        # Skip localhost and IP addresses
        if host in ("localhost", "127.0.0.1") or host.startswith("192.168.") or host.startswith("10."):
            return None

        # Extract subdomain (first part before main domain)
        parts = host.split(".")
        # Need at least 3 parts for subdomain (e.g., tenant.example.com)
        if len(parts) >= 3:
            subdomain = parts[0]
            # Skip common non-tenant subdomains
            if subdomain not in ("www", "api", "app", "admin", "mail", "ftp"):
                return subdomain

        return None

    async def _resolve_tenant_by_subdomain(self, subdomain: str) -> Optional[Dict]:
        """Resolve tenant by subdomain. Issue #413 fix."""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.tenant_models import Tenant

            with SessionLocal() as db:
                tenant = db.query(Tenant).filter(
                    Tenant.subdomain == subdomain,
                    Tenant.is_active == True
                ).first()

                if tenant:
                    return {
                        "tenant_id": str(tenant.id),
                        "tenant_name": tenant.name,
                        "subdomain": tenant.subdomain,
                        "plan": getattr(tenant, "plan", "free"),
                        "status": getattr(tenant, "status", "active")
                    }
        except Exception as e:
            logger.warning(f"[Issue #413] Failed to resolve tenant by subdomain '{subdomain}': {e}")

        return None

    async def _extract_from_jwt(
        self,
        auth_header: str,
        request: Request
    ) -> Optional[tuple]:
        """Extract tenant and user from JWT token"""
        token = auth_header.replace("Bearer ", "")

        try:
            from jose import jwt, JWTError

            payload = jwt.decode(
                token,
                self.jwt_secret,
                algorithms=[self.jwt_algorithm]
            )

            # Extract user data
            user_data = {
                "user_id": payload.get("user_id"),
                "username": payload.get("sub"),
                "email": payload.get("email"),
                "role": payload.get("role"),
                "tenant_id": payload.get("tenant_id"),
                "exp": payload.get("exp"),
            }

            # Get tenant_id from token or header
            tenant_id = payload.get("tenant_id")
            if not tenant_id:
                tenant_id = request.headers.get("X-Tenant-ID")

            # Resolve full tenant data
            tenant_data = None
            if tenant_id:
                tenant_data = await self._resolve_tenant(tenant_id)
                if tenant_data:
                    user_data["tenant_id"] = tenant_data.get("tenant_id")

            return (tenant_data, user_data)

        except Exception as e:
            logger.warning(f"Failed to decode JWT: {e}")
            return None

    async def _resolve_tenant(self, tenant_id: str) -> Optional[Dict[str, Any]]:
        """Resolve full tenant data from database"""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.tenant_models import Tenant, TenantSettings, BrandingConfig

            db = SessionLocal()
            try:
                # Find by tenant_id or slug
                tenant = db.query(Tenant).filter(
                    (Tenant.tenant_id == tenant_id) | (Tenant.slug == tenant_id)
                ).first()

                if not tenant:
                    return None

                tenant_data = tenant.to_dict()

                # Add settings if available
                if hasattr(tenant, 'settings') and tenant.settings:
                    tenant_data["settings"] = tenant.settings.to_dict()

                # Add branding if available
                if hasattr(tenant, 'branding') and tenant.branding:
                    tenant_data["branding"] = tenant.branding.to_dict()

                return tenant_data

            finally:
                db.close()

        except Exception as e:
            logger.error(f"Error resolving tenant {tenant_id}: {e}")

            # Return mock tenant in development
            if os.getenv("ENV", "development") == "development":
                return self._mock_tenant(tenant_id)

            return None

    async def _validate_tenant_membership(
        self,
        tenant_id: str,
        user_id: Any
    ) -> bool:
        """Validate user belongs to tenant"""
        if not tenant_id or not user_id:
            return False

        try:
            from factory.database.connection import SessionLocal
            from factory.database.tenant_models import TenantMember

            db = SessionLocal()
            try:
                member = db.query(TenantMember).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.user_id == user_id,
                    TenantMember.active == True
                ).first()

                return member is not None

            finally:
                db.close()

        except Exception as e:
            logger.error(f"Error validating membership: {e}")

            # Allow in development
            if os.getenv("ENV", "development") == "development":
                return True

            return False

    async def _handle_tenant_switch(
        self,
        switch_tenant_id: str,
        current_tenant: Optional[Dict[str, Any]],
        user_data: Dict[str, Any],
        request: Request
    ) -> Any:
        """
        Handle admin tenant switching.

        Returns:
            New tenant data or error Response
        """
        user_role = user_data.get("role", "")

        # Check if user is admin
        if user_role not in self.admin_roles:
            return JSONResponse(
                status_code=403,
                content={
                    "detail": "Only administrators can switch tenants",
                    "code": "ADMIN_REQUIRED"
                },
                headers={"X-Error-Code": "ADMIN_REQUIRED"}
            )

        # Resolve target tenant
        target_tenant = await self._resolve_tenant(switch_tenant_id)
        if not target_tenant:
            return JSONResponse(
                status_code=404,
                content={
                    "detail": f"Tenant '{switch_tenant_id}' not found",
                    "code": "TENANT_NOT_FOUND"
                },
                headers={"X-Error-Code": "TENANT_NOT_FOUND"}
            )

        # Store original tenant and set switched flag
        set_original_tenant(current_tenant)
        set_tenant_switched(True)

        logger.info(
            f"Admin tenant switch: user={user_data.get('username')} "
            f"from={current_tenant.get('tenant_id') if current_tenant else 'none'} "
            f"to={switch_tenant_id}"
        )

        return target_tenant

    def _mock_tenant(self, tenant_id: str) -> Dict[str, Any]:
        """Mock tenant for development"""
        return {
            "tenant_id": tenant_id,
            "name": f"Tenant {tenant_id}",
            "slug": tenant_id.lower(),
            "plan": "professional",
            "status": "active",
            "features": {
                "stories": True,
                "kanban": True,
                "workers": True,
                "chat_assistant": True,
                "custom_branding": True,
                "api_access": True
            },
            "settings": {
                "max_projects": 100,
                "max_members": 50
            }
        }


# =============================================================================
# FASTAPI DEPENDENCIES
# =============================================================================

class TenantContextDependency:
    """
    FastAPI dependency for tenant context.

    Usage:
        tenant_dep = TenantContextDependency(required=True)

        @router.get("/projects")
        async def list_projects(tenant: dict = Depends(tenant_dep)):
            ...
    """

    def __init__(self, required: bool = True):
        self.required = required

    async def __call__(self, request: Request) -> Optional[Dict[str, Any]]:
        tenant = get_tenant_context()

        if self.required and not tenant:
            raise HTTPException(
                status_code=401,
                detail="Tenant context required. Provide Authorization header with valid JWT.",
                headers={"WWW-Authenticate": "Bearer"}
            )

        return tenant


async def get_required_tenant(request: Request) -> Dict[str, Any]:
    """
    Dependency that requires tenant context.

    Usage:
        @router.get("/projects")
        async def list_projects(tenant: dict = Depends(get_required_tenant)):
            tenant_id = tenant["tenant_id"]
            ...
    """
    tenant = get_tenant_context()
    if not tenant:
        raise HTTPException(
            status_code=401,
            detail="Tenant not identified. Provide Authorization header with valid JWT."
        )
    return tenant


async def get_optional_tenant(request: Request) -> Optional[Dict[str, Any]]:
    """
    Dependency that optionally gets tenant context.

    Usage:
        @router.get("/public-info")
        async def get_info(tenant: dict = Depends(get_optional_tenant)):
            if tenant:
                # Tenant-specific logic
            ...
    """
    return get_tenant_context()


async def get_required_user(request: Request) -> Dict[str, Any]:
    """
    Dependency that requires user context.

    Usage:
        @router.get("/me")
        async def get_me(user: dict = Depends(get_required_user)):
            ...
    """
    user = get_user_context()
    if not user:
        raise HTTPException(
            status_code=401,
            detail="User not authenticated"
        )
    return user


def require_admin_tenant_switch():
    """
    Dependency to check if request is an admin tenant switch.

    Usage:
        @router.get("/admin/tenant/{tenant_id}")
        async def admin_view(
            info: dict = Depends(require_admin_tenant_switch())
        ):
            # Access info["original_tenant"], info["current_tenant"], info["is_switched"]
            ...
    """
    async def dependency(request: Request) -> Dict[str, Any]:
        user = get_user_context()

        if not user:
            raise HTTPException(
                status_code=401,
                detail="Authentication required"
            )

        role = user.get("role", "")
        if role not in GlobalTenantMiddleware.ADMIN_ROLES:
            raise HTTPException(
                status_code=403,
                detail="Admin role required for tenant switching"
            )

        return {
            "original_tenant": get_original_tenant(),
            "current_tenant": get_tenant_context(),
            "is_switched": is_tenant_switched(),
            "user": user
        }

    return dependency


# =============================================================================
# DECORATORS
# =============================================================================

def require_tenant(func: Callable) -> Callable:
    """
    Decorator requiring tenant context.

    Usage:
        @require_tenant
        async def my_endpoint():
            tenant = get_tenant_context()
            ...
    """
    @wraps(func)
    async def wrapper(*args, **kwargs):
        tenant = get_tenant_context()
        if not tenant:
            raise HTTPException(
                status_code=401,
                detail="Tenant not identified. Provide X-Tenant-ID header or JWT."
            )
        return await func(*args, **kwargs)
    return wrapper


def require_feature(feature_name: str):
    """
    Decorator requiring tenant feature.

    Usage:
        @require_feature("custom_branding")
        async def my_endpoint():
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            tenant = get_tenant_context()
            if not tenant:
                raise HTTPException(
                    status_code=401,
                    detail="Tenant not identified"
                )

            features = tenant.get("features", {})
            if not features.get(feature_name, False):
                raise HTTPException(
                    status_code=403,
                    detail=f"Feature '{feature_name}' not available in your plan"
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_role(allowed_roles: List[str]):
    """
    Decorator requiring user role.

    Usage:
        @require_role(["admin", "owner"])
        async def admin_endpoint():
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = get_user_context()
            if not user:
                raise HTTPException(
                    status_code=401,
                    detail="User not authenticated"
                )

            role = user.get("role", "viewer")
            if role not in allowed_roles:
                raise HTTPException(
                    status_code=403,
                    detail=f"Permission denied. Role '{role}' not authorized."
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


# =============================================================================
# TENANT QUERY HELPER
# =============================================================================

class TenantQueryHelper:
    """
    Helper for tenant-isolated database queries.

    Usage:
        helper = TenantQueryHelper(db_session)

        # Query with automatic tenant filter
        projects = helper.query(Project).all()

        # Create with automatic tenant_id
        project = helper.create(Project, name="My Project")

        # Validate access to entity
        helper.validate_access(some_project)
    """

    def __init__(self, db_session):
        self.db = db_session

    def get_tenant_id(self) -> str:
        """Get tenant_id from context or raise error"""
        tenant_id = get_tenant_id()
        if not tenant_id:
            raise HTTPException(
                status_code=401,
                detail="Tenant not identified in context"
            )
        return tenant_id

    def query(self, model_class):
        """Return query filtered by tenant"""
        tenant_id = self.get_tenant_id()

        query = self.db.query(model_class)
        if hasattr(model_class, 'tenant_id'):
            query = query.filter(model_class.tenant_id == tenant_id)

        return query

    def create(self, model_class, **kwargs):
        """Create entity with automatic tenant_id"""
        tenant_id = self.get_tenant_id()

        if hasattr(model_class, 'tenant_id') and 'tenant_id' not in kwargs:
            kwargs['tenant_id'] = tenant_id

        entity = model_class(**kwargs)
        self.db.add(entity)
        return entity

    def validate_access(self, entity) -> bool:
        """Validate entity belongs to current tenant"""
        tenant_id = self.get_tenant_id()

        if hasattr(entity, 'tenant_id'):
            if entity.tenant_id != tenant_id:
                raise HTTPException(
                    status_code=403,
                    detail="Access denied to this resource"
                )
        return True


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Context functions
    "get_tenant_context",
    "get_tenant_id",
    "get_user_context",
    "get_original_tenant",
    "is_tenant_switched",
    "set_tenant_context",
    "set_user_context",
    "clear_context",
    # Middleware
    "GlobalTenantMiddleware",
    # Dependencies
    "TenantContextDependency",
    "get_required_tenant",
    "get_optional_tenant",
    "get_required_user",
    "require_admin_tenant_switch",
    # Decorators
    "require_tenant",
    "require_feature",
    "require_role",
    # Helpers
    "TenantQueryHelper",
]
