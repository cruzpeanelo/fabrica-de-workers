# -*- coding: utf-8 -*-
"""
Tenant Isolation Middleware - Issue #122
Plataforma E v6.5

Implementa isolamento total de dados entre tenants:
1. Middleware obrigatorio em TODAS as rotas
2. Query filter automatico por tenant
3. Validacao dupla (middleware + query)
4. Suporte a Row Level Security (RLS)

CRITICO: Este modulo garante que dados de um tenant
NUNCA sejam acessiveis por outro tenant.
"""

import hashlib
import hmac
import os
from datetime import datetime
from functools import wraps
from typing import Optional, Dict, Any, List, Callable, TypeVar, Generic
from contextvars import ContextVar

from fastapi import Request, Response, HTTPException, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from starlette.middleware.base import BaseHTTPMiddleware
from sqlalchemy import event, inspect
from sqlalchemy.orm import Session, Query
from pydantic import BaseModel

# Context variable for tenant isolation
_tenant_context: ContextVar[Optional[str]] = ContextVar("tenant_id", default=None)
_tenant_verified: ContextVar[bool] = ContextVar("tenant_verified", default=False)


# =============================================================================
# TENANT CONTEXT MANAGEMENT
# =============================================================================

def get_current_tenant_id() -> Optional[str]:
    """Get current tenant ID from context - thread-safe"""
    return _tenant_context.get()


def set_current_tenant_id(tenant_id: Optional[str]) -> None:
    """Set current tenant ID in context - thread-safe"""
    _tenant_context.set(tenant_id)


def is_tenant_verified() -> bool:
    """Check if tenant was verified by middleware"""
    return _tenant_verified.get()


def set_tenant_verified(verified: bool) -> None:
    """Mark tenant as verified"""
    _tenant_verified.set(verified)


def clear_tenant_context() -> None:
    """Clear all tenant context"""
    _tenant_context.set(None)
    _tenant_verified.set(False)


def require_tenant_context() -> str:
    """Get tenant ID or raise exception if not set"""
    tenant_id = get_current_tenant_id()
    if not tenant_id:
        raise HTTPException(
            status_code=403,
            detail="Tenant context not established. Access denied."
        )
    if not is_tenant_verified():
        raise HTTPException(
            status_code=403,
            detail="Tenant not verified. Access denied."
        )
    return tenant_id


# =============================================================================
# TENANT ISOLATION MIDDLEWARE
# =============================================================================

class TenantIsolationMiddleware(BaseHTTPMiddleware):
    """
    Middleware that enforces tenant isolation on ALL routes.

    CRITICAL SECURITY COMPONENT:
    - Extracts tenant from request (header, JWT, subdomain)
    - Validates tenant exists and is active
    - Sets tenant context for request lifecycle
    - Blocks access if tenant cannot be determined

    Routes without tenant context are BLOCKED by default.
    Only explicitly whitelisted routes bypass tenant check.
    """

    # Routes that don't require tenant context
    PUBLIC_ROUTES = [
        "/health",
        "/docs",
        "/openapi.json",
        "/redoc",
        "/api/public",
        "/api/v1/auth/login",
        "/api/v1/auth/register",
        "/static",
        "/favicon.ico",
    ]

    def __init__(self, app, tenant_resolver: Callable = None, strict_mode: bool = True):
        """
        Initialize middleware.

        Args:
            app: FastAPI application
            tenant_resolver: Optional async function to validate tenant
            strict_mode: If True, block requests without tenant. If False, allow but log warning.
        """
        super().__init__(app)
        self.tenant_resolver = tenant_resolver or self._default_resolver
        self.strict_mode = strict_mode
        self._hmac_key = os.getenv("TENANT_HMAC_KEY", "default-key-change-in-production").encode()

    async def dispatch(self, request: Request, call_next) -> Response:
        """Process each request with tenant isolation"""

        # Clear any previous context
        clear_tenant_context()

        # Check if route is public
        path = request.url.path
        if self._is_public_route(path):
            return await call_next(request)

        # Extract tenant from request
        tenant_id = await self._extract_tenant(request)

        if not tenant_id:
            if self.strict_mode:
                return Response(
                    content='{"error": "Tenant not identified", "code": "TENANT_REQUIRED"}',
                    status_code=403,
                    media_type="application/json"
                )
            else:
                # Development mode - allow but log
                print(f"[TenantIsolation] WARNING: Request without tenant: {path}")
                return await call_next(request)

        # Validate tenant
        is_valid = await self._validate_tenant(tenant_id)
        if not is_valid:
            return Response(
                content='{"error": "Invalid or inactive tenant", "code": "TENANT_INVALID"}',
                status_code=403,
                media_type="application/json"
            )

        # Verify tenant signature if provided (prevents tenant ID spoofing)
        tenant_signature = request.headers.get("X-Tenant-Signature")
        if tenant_signature and not self._verify_tenant_signature(tenant_id, tenant_signature):
            return Response(
                content='{"error": "Invalid tenant signature", "code": "TENANT_SIGNATURE_INVALID"}',
                status_code=403,
                media_type="application/json"
            )

        # Set tenant context
        set_current_tenant_id(tenant_id)
        set_tenant_verified(True)

        try:
            # Store tenant in request state for easy access
            request.state.tenant_id = tenant_id

            # Process request
            response = await call_next(request)

            # Add tenant header to response
            response.headers["X-Tenant-ID"] = tenant_id

            return response

        finally:
            # Always clear context after request
            clear_tenant_context()

    def _is_public_route(self, path: str) -> bool:
        """Check if route is public (no tenant required)"""
        for public_path in self.PUBLIC_ROUTES:
            if path.startswith(public_path):
                return True
        return False

    async def _extract_tenant(self, request: Request) -> Optional[str]:
        """
        Extract tenant ID from request.

        Priority order:
        1. X-Tenant-ID header (APIs)
        2. JWT token claim (authenticated users)
        3. Query parameter tenant_id (development/testing)
        4. Subdomain extraction
        5. Cookie (web sessions)
        """
        # 1. Header
        tenant_id = request.headers.get("X-Tenant-ID")
        if tenant_id:
            return self._sanitize_tenant_id(tenant_id)

        # 2. JWT token
        auth_header = request.headers.get("Authorization")
        if auth_header and auth_header.startswith("Bearer "):
            token = auth_header[7:]
            tenant_from_jwt = await self._extract_tenant_from_jwt(token)
            if tenant_from_jwt:
                return self._sanitize_tenant_id(tenant_from_jwt)

        # 3. Query parameter (development only)
        if os.getenv("ENV", "development") == "development":
            tenant_id = request.query_params.get("tenant_id")
            if tenant_id:
                return self._sanitize_tenant_id(tenant_id)

        # 4. Subdomain
        host = request.headers.get("host", "")
        tenant_from_subdomain = self._extract_from_subdomain(host)
        if tenant_from_subdomain:
            return tenant_from_subdomain

        # 5. Cookie
        tenant_id = request.cookies.get("tenant_id")
        if tenant_id:
            return self._sanitize_tenant_id(tenant_id)

        # 6. Default tenant (development only)
        if os.getenv("ENV", "development") == "development":
            default_tenant = os.getenv("DEFAULT_TENANT_ID")
            if default_tenant:
                return default_tenant

        return None

    def _sanitize_tenant_id(self, tenant_id: str) -> str:
        """Sanitize tenant ID to prevent injection attacks"""
        # Remove any non-alphanumeric characters except dash and underscore
        import re
        sanitized = re.sub(r'[^a-zA-Z0-9\-_]', '', tenant_id)
        # Limit length
        return sanitized[:50]

    def _extract_from_subdomain(self, host: str) -> Optional[str]:
        """Extract tenant from subdomain"""
        import re
        # Issue #210: Verificar se ha pelo menos 3 partes (subdomain.domain.tld)
        # Exemplo: acme.fabrica.com tem subdomain, fabrica.com nao tem
        parts = host.split('.')
        if len(parts) < 3:
            return None

        # Pattern: tenant.domain.com
        match = re.match(r'^([a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9])\.', host)
        if match:
            subdomain = match.group(1).lower()
            # Ignore common subdomains
            if subdomain not in ['www', 'api', 'app', 'localhost', 'admin']:
                return subdomain
        return None

    async def _extract_tenant_from_jwt(self, token: str) -> Optional[str]:
        """Extract tenant ID from JWT token"""
        try:
            from jose import jwt
            from factory.api.auth import SECRET_KEY, ALGORITHM

            payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
            return payload.get("tenant_id")
        except Exception:
            return None

    async def _validate_tenant(self, tenant_id: str) -> bool:
        """Validate tenant exists and is active"""
        return await self.tenant_resolver(tenant_id)

    async def _default_resolver(self, tenant_id: str) -> bool:
        """Default tenant validation - check database"""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.tenant_models import Tenant

            db = SessionLocal()
            try:
                tenant = db.query(Tenant).filter(
                    Tenant.tenant_id == tenant_id,
                    Tenant.status.in_(["active", "trial"])
                ).first()
                return tenant is not None
            finally:
                db.close()
        except Exception as e:
            # In development, allow if tenant models not available
            if os.getenv("ENV", "development") == "development":
                print(f"[TenantIsolation] Dev mode: allowing tenant {tenant_id}")
                return True
            print(f"[TenantIsolation] Error validating tenant: {e}")
            return False

    def _verify_tenant_signature(self, tenant_id: str, signature: str) -> bool:
        """Verify HMAC signature of tenant ID"""
        expected = hmac.new(
            self._hmac_key,
            tenant_id.encode(),
            hashlib.sha256
        ).hexdigest()
        return hmac.compare_digest(expected, signature)

    def generate_tenant_signature(self, tenant_id: str) -> str:
        """Generate HMAC signature for tenant ID"""
        return hmac.new(
            self._hmac_key,
            tenant_id.encode(),
            hashlib.sha256
        ).hexdigest()


# =============================================================================
# TENANT-AWARE QUERY FILTER
# =============================================================================

class TenantQueryFilter:
    """
    Automatic query filter that enforces tenant isolation at the database level.

    CRITICAL: This provides the second layer of defense.
    Even if middleware is bypassed, queries are filtered by tenant.

    Usage:
        # In repository/service layer
        query = TenantQueryFilter.apply(db.query(Project))

        # Or with filter
        query = TenantQueryFilter.filter(db.query(Story), Story)
    """

    @staticmethod
    def apply(query: Query, model_class=None) -> Query:
        """
        Apply tenant filter to query.

        IMPORTANT: This method MUST be called on every query
        that accesses tenant-specific data.
        """
        tenant_id = get_current_tenant_id()

        if not tenant_id:
            # SECURITY: No tenant = no data
            # Return impossible condition to prevent data leakage
            if model_class and hasattr(model_class, 'tenant_id'):
                return query.filter(model_class.tenant_id == "__NO_TENANT_ACCESS__")
            # Fallback: filter by False to return empty result
            from sqlalchemy import false
            return query.filter(false())

        if model_class and hasattr(model_class, 'tenant_id'):
            return query.filter(model_class.tenant_id == tenant_id)

        # Try to infer model class from query
        try:
            entities = query.column_descriptions
            for entity in entities:
                cls = entity.get('entity')
                if cls and hasattr(cls, 'tenant_id'):
                    return query.filter(cls.tenant_id == tenant_id)
        except Exception:
            pass

        return query

    @staticmethod
    def filter(query: Query, model_class) -> Query:
        """Explicit filter with model class"""
        return TenantQueryFilter.apply(query, model_class)

    @staticmethod
    def set_tenant_on_entity(entity: Any) -> Any:
        """
        Set tenant_id on new entity before insert.

        Usage:
            project = Project(name="New Project")
            TenantQueryFilter.set_tenant_on_entity(project)
            db.add(project)
        """
        tenant_id = get_current_tenant_id()

        if not tenant_id:
            raise HTTPException(
                status_code=403,
                detail="Cannot create entity without tenant context"
            )

        if hasattr(entity, 'tenant_id'):
            entity.tenant_id = tenant_id

        return entity

    @staticmethod
    def validate_entity_tenant(entity: Any) -> bool:
        """
        Validate that entity belongs to current tenant.

        IMPORTANT: Call this before updating/deleting any entity.
        """
        tenant_id = get_current_tenant_id()

        if not tenant_id:
            return False

        if hasattr(entity, 'tenant_id'):
            return entity.tenant_id == tenant_id

        return True


# =============================================================================
# SQLALCHEMY EVENT LISTENERS FOR AUTOMATIC TENANT FILTERING
# =============================================================================

def setup_tenant_query_events(engine):
    """
    Set up SQLAlchemy event listeners for automatic tenant filtering.

    This provides defense-in-depth by filtering at the ORM level.

    Usage:
        from factory.database.connection import sync_engine
        setup_tenant_query_events(sync_engine)
    """

    @event.listens_for(Session, "do_orm_execute")
    def _filter_by_tenant(orm_execute_state):
        """Automatically filter SELECT queries by tenant"""

        # Only filter SELECT statements
        if not orm_execute_state.is_select:
            return

        tenant_id = get_current_tenant_id()
        if not tenant_id:
            return

        # Get the statement
        stmt = orm_execute_state.statement

        # Try to add tenant filter to each entity
        # This is a best-effort approach
        try:
            for column in stmt.selected_columns:
                if hasattr(column, 'class_') and hasattr(column.class_, 'tenant_id'):
                    orm_execute_state.statement = stmt.filter(
                        column.class_.tenant_id == tenant_id
                    )
                    break
        except Exception:
            pass


def setup_tenant_insert_events(session_class):
    """
    Set up event listener to automatically set tenant_id on insert.

    Usage:
        from factory.database.connection import SessionLocal
        setup_tenant_insert_events(SessionLocal)
    """

    @event.listens_for(session_class, "before_flush")
    def _set_tenant_on_new(session, flush_context, instances):
        """Automatically set tenant_id on new entities"""

        tenant_id = get_current_tenant_id()
        if not tenant_id:
            return

        for obj in session.new:
            if hasattr(obj, 'tenant_id') and obj.tenant_id is None:
                obj.tenant_id = tenant_id


# =============================================================================
# DECORATORS FOR TENANT-AWARE FUNCTIONS
# =============================================================================

def require_tenant(func: Callable) -> Callable:
    """
    Decorator that requires tenant context to execute function.

    Usage:
        @require_tenant
        async def get_projects():
            tenant_id = get_current_tenant_id()
            ...
    """
    @wraps(func)
    async def async_wrapper(*args, **kwargs):
        require_tenant_context()
        return await func(*args, **kwargs)

    @wraps(func)
    def sync_wrapper(*args, **kwargs):
        require_tenant_context()
        return func(*args, **kwargs)

    import asyncio
    if asyncio.iscoroutinefunction(func):
        return async_wrapper
    return sync_wrapper


def tenant_scoped(func: Callable) -> Callable:
    """
    Decorator that injects tenant_id as parameter.

    Usage:
        @tenant_scoped
        async def get_projects(tenant_id: str):
            # tenant_id is automatically injected
            ...
    """
    @wraps(func)
    async def async_wrapper(*args, **kwargs):
        tenant_id = require_tenant_context()
        kwargs['tenant_id'] = tenant_id
        return await func(*args, **kwargs)

    @wraps(func)
    def sync_wrapper(*args, **kwargs):
        tenant_id = require_tenant_context()
        kwargs['tenant_id'] = tenant_id
        return func(*args, **kwargs)

    import asyncio
    if asyncio.iscoroutinefunction(func):
        return async_wrapper
    return sync_wrapper


# =============================================================================
# FASTAPI DEPENDENCIES
# =============================================================================

async def get_tenant_id(request: Request) -> str:
    """
    FastAPI dependency to get current tenant ID.

    Usage:
        @app.get("/projects")
        async def list_projects(tenant_id: str = Depends(get_tenant_id)):
            ...
    """
    tenant_id = getattr(request.state, 'tenant_id', None)
    if not tenant_id:
        tenant_id = get_current_tenant_id()

    if not tenant_id:
        raise HTTPException(
            status_code=403,
            detail="Tenant context required"
        )

    return tenant_id


async def validate_tenant_access(
    request: Request,
    resource_tenant_id: str = None
) -> bool:
    """
    Validate that current request can access resource from specific tenant.

    Usage:
        @app.get("/projects/{project_id}")
        async def get_project(
            project_id: str,
            _: bool = Depends(lambda r: validate_tenant_access(r, project.tenant_id))
        ):
            ...
    """
    current_tenant = get_current_tenant_id()

    if not current_tenant:
        raise HTTPException(status_code=403, detail="No tenant context")

    if resource_tenant_id and resource_tenant_id != current_tenant:
        raise HTTPException(
            status_code=403,
            detail="Access denied: resource belongs to different tenant"
        )

    return True


# =============================================================================
# TENANT ISOLATION AUDIT
# =============================================================================

class TenantIsolationAudit:
    """
    Audit trail for tenant isolation events.

    Logs all tenant access attempts for security monitoring.
    """

    @staticmethod
    def log_access(
        tenant_id: str,
        resource: str,
        action: str,
        user_id: Optional[int] = None,
        success: bool = True,
        details: Optional[Dict] = None
    ):
        """Log tenant access event"""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.models import AuditLog

            db = SessionLocal()
            try:
                log = AuditLog(
                    user_id=user_id,
                    action=action,
                    resource=resource,
                    resource_id=tenant_id,
                    details={
                        "tenant_id": tenant_id,
                        "isolation_event": True,
                        **(details or {})
                    },
                    success=success,
                    timestamp=datetime.utcnow()
                )
                db.add(log)
                db.commit()
            finally:
                db.close()
        except Exception as e:
            print(f"[TenantIsolation] Audit log error: {e}")

    @staticmethod
    def log_violation(
        attempted_tenant: str,
        actual_tenant: str,
        resource: str,
        user_id: Optional[int] = None
    ):
        """Log tenant isolation violation attempt"""
        TenantIsolationAudit.log_access(
            tenant_id=attempted_tenant,
            resource=resource,
            action="ISOLATION_VIOLATION",
            user_id=user_id,
            success=False,
            details={
                "attempted_tenant": attempted_tenant,
                "actual_tenant": actual_tenant,
                "severity": "CRITICAL"
            }
        )


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Context management
    "get_current_tenant_id",
    "set_current_tenant_id",
    "clear_tenant_context",
    "require_tenant_context",
    "is_tenant_verified",

    # Middleware
    "TenantIsolationMiddleware",

    # Query filter
    "TenantQueryFilter",

    # Event setup
    "setup_tenant_query_events",
    "setup_tenant_insert_events",

    # Decorators
    "require_tenant",
    "tenant_scoped",

    # Dependencies
    "get_tenant_id",
    "validate_tenant_access",

    # Audit
    "TenantIsolationAudit",
]
