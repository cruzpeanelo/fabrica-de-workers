# -*- coding: utf-8 -*-
"""
Plan Limits Enforcement Middleware (Issue #104)
================================================

Middleware for enforcing plan limits per tenant.

Features:
- Verify limits by plan (free, pro, enterprise)
- Limits: number of projects, stories, workers, API calls
- Return HTTP 429 when limit exceeded
- Support for custom limits per tenant
- Real-time usage tracking

Usage:
    from factory.middleware.plan_limits import PlanLimitsMiddleware

    app.add_middleware(PlanLimitsMiddleware)

    # Or use decorator
    @router.post("/projects")
    @enforce_plan_limit("projects")
    async def create_project():
        ...

Author: Plataforma E
"""

import logging
import time
from datetime import datetime, date
from typing import Optional, Dict, Any, Tuple, List, Callable
from functools import wraps
from enum import Enum
from contextvars import ContextVar

from fastapi import Request, Response, HTTPException, Depends
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS AND CONSTANTS
# =============================================================================

class PlanType(str, Enum):
    """Available plan types"""
    FREE = "free"
    PRO = "pro"
    ENTERPRISE = "enterprise"


class ResourceType(str, Enum):
    """Resource types controlled by limits"""
    PROJECTS = "projects"
    STORIES = "stories"
    WORKERS = "workers"
    AGENTS = "agents"
    USERS = "users"
    API_CALLS_PER_DAY = "api_calls_per_day"
    API_CALLS_PER_MINUTE = "api_calls_per_minute"
    TOKENS_PER_MONTH = "tokens_per_month"
    STORAGE_GB = "storage_gb"


# Environment detection for QA/Development mode
import os
IS_QA_MODE = os.getenv("QA_MODE", "").lower() in ("true", "1", "yes") or os.getenv("ENVIRONMENT", "development").lower() != "production"

# Default limits per plan (increased for QA/Development)
DEFAULT_PLAN_LIMITS: Dict[str, Dict[str, int]] = {
    PlanType.FREE.value: {
        ResourceType.PROJECTS.value: 100 if IS_QA_MODE else 2,
        ResourceType.STORIES.value: 1000 if IS_QA_MODE else 50,
        ResourceType.WORKERS.value: 10 if IS_QA_MODE else 1,
        ResourceType.AGENTS.value: 10 if IS_QA_MODE else 1,
        ResourceType.USERS.value: 50 if IS_QA_MODE else 3,
        ResourceType.API_CALLS_PER_DAY.value: 1000000 if IS_QA_MODE else 100,  # 1M for QA
        ResourceType.API_CALLS_PER_MINUTE.value: 10000 if IS_QA_MODE else 10,
        ResourceType.TOKENS_PER_MONTH.value: 10000000 if IS_QA_MODE else 10000,
        ResourceType.STORAGE_GB.value: 100 if IS_QA_MODE else 1,
    },
    PlanType.PRO.value: {
        ResourceType.PROJECTS.value: 20,
        ResourceType.STORIES.value: 500,
        ResourceType.WORKERS.value: 5,
        ResourceType.AGENTS.value: 10,
        ResourceType.USERS.value: 20,
        ResourceType.API_CALLS_PER_DAY.value: 5000,
        ResourceType.API_CALLS_PER_MINUTE.value: 60,
        ResourceType.TOKENS_PER_MONTH.value: 500000,
        ResourceType.STORAGE_GB.value: 50,
    },
    PlanType.ENTERPRISE.value: {
        ResourceType.PROJECTS.value: -1,  # -1 = unlimited
        ResourceType.STORIES.value: -1,
        ResourceType.WORKERS.value: -1,
        ResourceType.AGENTS.value: -1,
        ResourceType.USERS.value: -1,
        ResourceType.API_CALLS_PER_DAY.value: -1,
        ResourceType.API_CALLS_PER_MINUTE.value: -1,
        ResourceType.TOKENS_PER_MONTH.value: -1,
        ResourceType.STORAGE_GB.value: -1,
    },
}


# Route to resource type mapping for automatic enforcement
ROUTE_RESOURCE_MAP: Dict[str, str] = {
    "/api/projects": ResourceType.PROJECTS.value,
    "/api/v1/projects": ResourceType.PROJECTS.value,
    "/api/stories": ResourceType.STORIES.value,
    "/api/v1/stories": ResourceType.STORIES.value,
    "/api/workers": ResourceType.WORKERS.value,
    "/api/v1/workers": ResourceType.WORKERS.value,
    "/api/agents": ResourceType.AGENTS.value,
    "/api/v1/agents": ResourceType.AGENTS.value,
    "/api/users/invite": ResourceType.USERS.value,
    "/api/v1/users/invite": ResourceType.USERS.value,
    "/api/tenants/members": ResourceType.USERS.value,
    "/api/v1/tenants/members": ResourceType.USERS.value,
}


# =============================================================================
# EXCEPTIONS
# =============================================================================

class PlanLimitExceededError(Exception):
    """Exception raised when a plan limit is exceeded"""

    def __init__(
        self,
        resource_type: str,
        current: int,
        limit: int,
        tenant_id: str = None,
        plan_type: str = None,
        retry_after: int = None,
        message: str = None
    ):
        self.resource_type = resource_type
        self.current = current
        self.limit = limit
        self.tenant_id = tenant_id
        self.plan_type = plan_type
        self.retry_after = retry_after
        self.message = message or self._default_message()
        super().__init__(self.message)

    def _default_message(self) -> str:
        if self.resource_type == ResourceType.API_CALLS_PER_MINUTE.value:
            return f"Rate limit exceeded. Please wait before making more requests."
        elif self.resource_type == ResourceType.API_CALLS_PER_DAY.value:
            return f"Daily API call limit reached ({self.current}/{self.limit}). Upgrade your plan for more."
        else:
            return f"Limit for {self.resource_type} exceeded ({self.current}/{self.limit}). Upgrade your plan."

    def to_response(self) -> JSONResponse:
        """Convert to HTTP 429 response"""
        headers = {
            "X-RateLimit-Resource": self.resource_type,
            "X-RateLimit-Limit": str(self.limit),
            "X-RateLimit-Current": str(self.current),
            "X-RateLimit-Reset": str(int(time.time()) + (self.retry_after or 60)),
        }

        if self.retry_after:
            headers["Retry-After"] = str(self.retry_after)

        return JSONResponse(
            status_code=429,
            content={
                "detail": self.message,
                "code": "LIMIT_EXCEEDED",
                "resource_type": self.resource_type,
                "current": self.current,
                "limit": self.limit,
                "plan_type": self.plan_type,
                "upgrade_url": "/billing/upgrade",
                "retry_after": self.retry_after,
            },
            headers=headers
        )


# =============================================================================
# PLAN LIMIT CHECKER
# =============================================================================

class PlanLimitChecker:
    """
    Service for checking and enforcing plan limits (Issue #104)

    This class provides methods to:
    - Check if a tenant can create more resources
    - Get current usage counts
    - Enforce limits with exceptions
    - Track API rate limiting

    Usage:
        checker = PlanLimitChecker(db_session)

        # Check before creating
        can_create, msg = checker.check_limit(tenant_id, "projects")
        if not can_create:
            raise HTTPException(429, msg)

        # Or use enforce (raises exception)
        checker.enforce(tenant_id, "projects")
    """

    def __init__(self, db_session=None):
        """
        Initialize the checker.

        Args:
            db_session: SQLAlchemy database session (optional)
        """
        self.db = db_session
        self._rate_limit_cache: Dict[str, Dict[str, Any]] = {}

    def _get_db(self):
        """Get database session"""
        if self.db:
            return self.db
        try:
            from factory.database.connection import SessionLocal
            return SessionLocal()
        except ImportError:
            return None

    def get_tenant_plan(self, tenant_id: str) -> Tuple[str, Dict[str, int]]:
        """
        Get tenant's plan type and limits.

        Args:
            tenant_id: Tenant ID

        Returns:
            Tuple (plan_type, limits_dict)
        """
        db = self._get_db()
        if not db:
            return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

        try:
            from factory.database.tenant_models import Tenant
            from factory.billing.models import Subscription, Plan

            # Find tenant
            tenant = db.query(Tenant).filter(
                Tenant.tenant_id == tenant_id
            ).first()

            if not tenant:
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Find active subscription
            active_sub = db.query(Subscription).filter(
                Subscription.tenant_id == tenant_id,
                Subscription.status.in_(["active", "trialing"])
            ).first()

            if not active_sub:
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Get plan
            plan = db.query(Plan).filter(
                Plan.plan_id == active_sub.plan_id
            ).first()

            if not plan:
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Merge plan limits with custom tenant limits
            plan_limits = plan.limits or {}
            custom_limits = tenant.custom_limits or {}

            # Start with default limits for plan type
            base_limits = DEFAULT_PLAN_LIMITS.get(
                plan.plan_type,
                DEFAULT_PLAN_LIMITS[PlanType.FREE.value]
            )

            effective_limits = {**base_limits, **plan_limits, **custom_limits}

            return plan.plan_type, effective_limits

        except Exception as e:
            logger.error(f"Error getting tenant plan for {tenant_id}: {e}")
            return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]
        finally:
            if not self.db and db:
                db.close()

    def get_limit(self, tenant_id: str, resource_type: str) -> int:
        """
        Get limit for a specific resource type.

        Args:
            tenant_id: Tenant ID
            resource_type: Resource type (projects, stories, etc)

        Returns:
            Limit value (-1 = unlimited, 0 = blocked)
        """
        _, limits = self.get_tenant_plan(tenant_id)
        return limits.get(resource_type, 0)

    def get_current_count(self, tenant_id: str, resource_type: str) -> int:
        """
        Count current resources for tenant.

        Args:
            tenant_id: Tenant ID
            resource_type: Resource type

        Returns:
            Current count
        """
        db = self._get_db()
        if not db:
            return 0

        try:
            from sqlalchemy import func
            from factory.database.models import Project, Story, Task
            from factory.database.tenant_models import TenantMember

            count = 0

            if resource_type == ResourceType.PROJECTS.value:
                count = db.query(func.count(Project.id)).filter(
                    Project.tenant_id == tenant_id
                ).scalar() or 0

            elif resource_type == ResourceType.STORIES.value:
                count = db.query(func.count(Story.id)).filter(
                    Story.tenant_id == tenant_id
                ).scalar() or 0

            elif resource_type == ResourceType.USERS.value:
                count = db.query(func.count(TenantMember.id)).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.active == True
                ).scalar() or 0

            elif resource_type == ResourceType.WORKERS.value:
                # Count active workers from jobs/workers table
                try:
                    from factory.database.models import Worker
                    count = db.query(func.count(Worker.id)).filter(
                        Worker.tenant_id == tenant_id,
                        Worker.status == "active"
                    ).scalar() or 0
                except Exception:
                    count = 0

            elif resource_type == ResourceType.AGENTS.value:
                # Count agents from agents table
                try:
                    from factory.database.models import Agent
                    count = db.query(func.count(Agent.id)).filter(
                        Agent.tenant_id == tenant_id,
                        Agent.active == True
                    ).scalar() or 0
                except Exception:
                    count = 0

            elif resource_type == ResourceType.API_CALLS_PER_DAY.value:
                from factory.billing.models import Usage, UsageMetric
                today = date.today()

                usage = db.query(Usage).filter(
                    Usage.tenant_id == tenant_id,
                    Usage.metric == UsageMetric.API_REQUESTS.value,
                    Usage.period == today,
                    Usage.period_type == "daily"
                ).first()
                count = usage.value if usage else 0

            elif resource_type == ResourceType.TOKENS_PER_MONTH.value:
                from factory.billing.models import Usage, UsageMetric
                first_day = date.today().replace(day=1)

                usage = db.query(Usage).filter(
                    Usage.tenant_id == tenant_id,
                    Usage.metric == UsageMetric.LLM_TOKENS.value,
                    Usage.period >= first_day,
                    Usage.period_type == "monthly"
                ).first()
                count = usage.value if usage else 0

            elif resource_type == ResourceType.API_CALLS_PER_MINUTE.value:
                # Use in-memory cache for rate limiting
                cache_key = f"{tenant_id}:rate_minute"
                cache_data = self._rate_limit_cache.get(cache_key, {})

                current_minute = int(time.time() / 60)
                if cache_data.get("minute") == current_minute:
                    count = cache_data.get("count", 0)
                else:
                    count = 0

            return count

        except Exception as e:
            logger.error(f"Error counting {resource_type} for tenant {tenant_id}: {e}")
            return 0
        finally:
            if not self.db and db:
                db.close()

    def check_limit(
        self,
        tenant_id: str,
        resource_type: str,
        increment: int = 1
    ) -> Tuple[bool, str]:
        """
        Check if tenant can create more resources.

        Args:
            tenant_id: Tenant ID
            resource_type: Resource type
            increment: Amount to add (default 1)

        Returns:
            Tuple (can_create, message)
        """
        limit = self.get_limit(tenant_id, resource_type)

        # -1 = unlimited
        if limit == -1:
            return True, f"No limit for {resource_type}"

        # 0 = blocked
        if limit == 0:
            return False, f"Resource {resource_type} not available in your plan"

        current = self.get_current_count(tenant_id, resource_type)

        if current + increment > limit:
            return False, f"Limit for {resource_type} reached ({current}/{limit}). Please upgrade your plan."

        remaining = limit - current - increment + 1
        return True, f"Can create {remaining} more {resource_type}"

    def enforce(
        self,
        tenant_id: str,
        resource_type: str,
        increment: int = 1
    ) -> None:
        """
        Check limit and raise exception if exceeded.

        Args:
            tenant_id: Tenant ID
            resource_type: Resource type
            increment: Amount to add

        Raises:
            PlanLimitExceededError if limit exceeded
        """
        can_proceed, message = self.check_limit(tenant_id, resource_type, increment)

        if not can_proceed:
            plan_type, _ = self.get_tenant_plan(tenant_id)
            limit = self.get_limit(tenant_id, resource_type)
            current = self.get_current_count(tenant_id, resource_type)

            # Calculate retry_after for rate limits
            retry_after = None
            if resource_type == ResourceType.API_CALLS_PER_MINUTE.value:
                retry_after = 60
            elif resource_type == ResourceType.API_CALLS_PER_DAY.value:
                # Seconds until midnight
                now = datetime.now()
                midnight = now.replace(hour=0, minute=0, second=0, microsecond=0)
                from datetime import timedelta
                next_midnight = midnight + timedelta(days=1)
                retry_after = int((next_midnight - now).total_seconds())

            raise PlanLimitExceededError(
                resource_type=resource_type,
                current=current,
                limit=limit,
                tenant_id=tenant_id,
                plan_type=plan_type,
                retry_after=retry_after,
                message=message
            )

    def increment_api_calls(self, tenant_id: str) -> None:
        """
        Increment API call counter (for rate limiting).

        Args:
            tenant_id: Tenant ID
        """
        # Update in-memory cache for minute-based rate limiting
        cache_key = f"{tenant_id}:rate_minute"
        current_minute = int(time.time() / 60)

        cache_data = self._rate_limit_cache.get(cache_key, {})
        if cache_data.get("minute") == current_minute:
            cache_data["count"] = cache_data.get("count", 0) + 1
        else:
            cache_data = {"minute": current_minute, "count": 1}

        self._rate_limit_cache[cache_key] = cache_data

        # Also increment daily counter in database
        db = self._get_db()
        if db:
            try:
                from factory.billing.models import Usage, UsageMetric
                import uuid

                today = date.today()
                usage = db.query(Usage).filter(
                    Usage.tenant_id == tenant_id,
                    Usage.metric == UsageMetric.API_REQUESTS.value,
                    Usage.period == today,
                    Usage.period_type == "daily"
                ).first()

                if usage:
                    usage.value += 1
                    usage.updated_at = datetime.utcnow()
                else:
                    usage = Usage(
                        usage_id=f"USG-{uuid.uuid4().hex[:8].upper()}",
                        tenant_id=tenant_id,
                        period=today,
                        period_type="daily",
                        metric=UsageMetric.API_REQUESTS.value,
                        value=1,
                        limit_value=self.get_limit(tenant_id, ResourceType.API_CALLS_PER_DAY.value)
                    )
                    db.add(usage)

                db.commit()
            except Exception as e:
                logger.error(f"Error incrementing API calls for {tenant_id}: {e}")
                db.rollback()
            finally:
                if not self.db:
                    db.close()

    def get_usage_summary(self, tenant_id: str) -> Dict[str, Any]:
        """
        Get usage summary for tenant.

        Args:
            tenant_id: Tenant ID

        Returns:
            Dict with usage and limits
        """
        plan_type, limits = self.get_tenant_plan(tenant_id)

        summary = {
            "tenant_id": tenant_id,
            "plan_type": plan_type,
            "resources": {}
        }

        for resource_type in ResourceType:
            limit = limits.get(resource_type.value, 0)
            current = self.get_current_count(tenant_id, resource_type.value)

            summary["resources"][resource_type.value] = {
                "current": current,
                "limit": limit if limit != -1 else "unlimited",
                "remaining": limit - current if limit != -1 else "unlimited",
                "usage_percent": round((current / limit) * 100, 1) if limit > 0 else 0
            }

        return summary


# =============================================================================
# MIDDLEWARE
# =============================================================================

class PlanLimitsMiddleware(BaseHTTPMiddleware):
    """
    Middleware for enforcing plan limits (Issue #104).

    This middleware:
    1. Intercepts POST requests to resource creation routes
    2. Checks if tenant has available quota
    3. Returns HTTP 429 if limit exceeded
    4. Tracks API call rate limits

    Usage:
        from factory.middleware.plan_limits import PlanLimitsMiddleware

        app.add_middleware(PlanLimitsMiddleware)
    """

    # Paths exempt from limit checking
    EXEMPT_PATHS = [
        "/health",
        "/docs",
        "/openapi.json",
        "/redoc",
        "/api/auth",
        "/api/v1/auth",
        "/api/public",
        "/static",
        "/favicon.ico",
        "/api/webhooks",
    ]

    def __init__(
        self,
        app,
        route_resource_map: Dict[str, str] = None,
        exempt_paths: List[str] = None,
        enable_rate_limiting: bool = True
    ):
        """
        Initialize the middleware.

        Args:
            app: FastAPI application
            route_resource_map: Custom route to resource mapping
            exempt_paths: Additional paths to exempt
            enable_rate_limiting: Enable API rate limiting
        """
        super().__init__(app)
        self.route_resource_map = route_resource_map or ROUTE_RESOURCE_MAP
        self.exempt_paths = (exempt_paths or []) + self.EXEMPT_PATHS
        self.enable_rate_limiting = enable_rate_limiting
        self._checker = PlanLimitChecker()

    async def dispatch(self, request: Request, call_next) -> Response:
        """Process request and check plan limits."""

        path = request.url.path

        # Check if path is exempt
        if self._is_exempt_path(path):
            return await call_next(request)

        # Get tenant_id from context
        tenant_id = self._get_tenant_id(request)
        if not tenant_id:
            # No tenant, let authentication handle it
            return await call_next(request)

        # Check API rate limits (all requests)
        if self.enable_rate_limiting:
            try:
                # Check per-minute rate limit
                self._checker.enforce(
                    tenant_id,
                    ResourceType.API_CALLS_PER_MINUTE.value
                )

                # Check daily limit
                self._checker.enforce(
                    tenant_id,
                    ResourceType.API_CALLS_PER_DAY.value
                )

                # Increment API call counter
                self._checker.increment_api_calls(tenant_id)

            except PlanLimitExceededError as e:
                logger.warning(
                    f"Rate limit exceeded: tenant={tenant_id}, "
                    f"resource={e.resource_type}, current={e.current}, limit={e.limit}"
                )
                return e.to_response()

        # Check resource limits for POST requests
        if request.method == "POST":
            resource_type = self._get_resource_type(path)
            if resource_type:
                try:
                    self._checker.enforce(tenant_id, resource_type)
                except PlanLimitExceededError as e:
                    logger.warning(
                        f"Plan limit exceeded: tenant={tenant_id}, "
                        f"resource={e.resource_type}, current={e.current}, limit={e.limit}"
                    )
                    return e.to_response()

        # Process request
        response = await call_next(request)

        # Add usage headers
        if tenant_id:
            try:
                plan_type, limits = self._checker.get_tenant_plan(tenant_id)
                response.headers["X-Plan-Type"] = plan_type

                # Add rate limit headers
                minute_limit = limits.get(ResourceType.API_CALLS_PER_MINUTE.value, 0)
                if minute_limit > 0:
                    current = self._checker.get_current_count(
                        tenant_id,
                        ResourceType.API_CALLS_PER_MINUTE.value
                    )
                    response.headers["X-RateLimit-Limit"] = str(minute_limit)
                    response.headers["X-RateLimit-Remaining"] = str(max(0, minute_limit - current))
                    response.headers["X-RateLimit-Reset"] = str(int(time.time() / 60 + 1) * 60)
            except Exception as e:
                logger.debug(f"Error adding usage headers: {e}")

        return response

    def _is_exempt_path(self, path: str) -> bool:
        """Check if path is exempt from limit checking."""
        for exempt in self.exempt_paths:
            if path == exempt or path.startswith(exempt):
                return True
        return False

    def _get_resource_type(self, path: str) -> Optional[str]:
        """Get resource type for route."""
        for route, resource in self.route_resource_map.items():
            if path.startswith(route):
                return resource
        return None

    def _get_tenant_id(self, request: Request) -> Optional[str]:
        """Get tenant_id from request context."""
        try:
            from factory.core.multi_tenant import get_current_tenant_id
            return get_current_tenant_id()
        except ImportError:
            # Fallback to header
            return request.headers.get("X-Tenant-ID")


# =============================================================================
# DECORATORS
# =============================================================================

def enforce_plan_limit(resource_type: str):
    """
    Decorator to enforce plan limit before executing endpoint.

    Usage:
        @router.post("/projects")
        @enforce_plan_limit("projects")
        async def create_project(db: Session = Depends(get_db)):
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            try:
                from factory.core.multi_tenant import get_current_tenant_id
                tenant_id = get_current_tenant_id()
            except ImportError:
                tenant_id = None

            if not tenant_id:
                raise HTTPException(
                    status_code=401,
                    detail="Tenant not identified"
                )

            checker = PlanLimitChecker()
            try:
                checker.enforce(tenant_id, resource_type)
            except PlanLimitExceededError as e:
                raise HTTPException(
                    status_code=429,
                    detail=e.message,
                    headers={
                        "X-RateLimit-Resource": resource_type,
                        "X-RateLimit-Limit": str(e.limit),
                        "X-RateLimit-Current": str(e.current),
                        "Retry-After": str(e.retry_after or 60),
                    }
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_plan(allowed_plans: List[str]):
    """
    Decorator requiring specific plan type.

    Usage:
        @router.get("/advanced-analytics")
        @require_plan(["pro", "enterprise"])
        async def analytics():
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            try:
                from factory.core.multi_tenant import get_current_tenant_id
                tenant_id = get_current_tenant_id()
            except ImportError:
                tenant_id = None

            if not tenant_id:
                raise HTTPException(
                    status_code=401,
                    detail="Tenant not identified"
                )

            checker = PlanLimitChecker()
            plan_type, _ = checker.get_tenant_plan(tenant_id)

            if plan_type not in allowed_plans:
                raise HTTPException(
                    status_code=403,
                    detail=f"This feature requires plan {', '.join(allowed_plans)}. Current plan: {plan_type}",
                    headers={"X-Required-Plan": ",".join(allowed_plans)}
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


# =============================================================================
# FASTAPI DEPENDENCY
# =============================================================================

def get_plan_limit_checker() -> PlanLimitChecker:
    """
    FastAPI dependency for PlanLimitChecker.

    Usage:
        @router.post("/projects")
        async def create(
            checker: PlanLimitChecker = Depends(get_plan_limit_checker)
        ):
            checker.enforce(tenant_id, "projects")
    """
    return PlanLimitChecker()


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Enums
    "PlanType",
    "ResourceType",
    # Classes
    "PlanLimitChecker",
    "PlanLimitsMiddleware",
    # Exceptions
    "PlanLimitExceededError",
    # Decorators
    "enforce_plan_limit",
    "require_plan",
    # Dependencies
    "get_plan_limit_checker",
    # Constants
    "DEFAULT_PLAN_LIMITS",
    "ROUTE_RESOURCE_MAP",
]
