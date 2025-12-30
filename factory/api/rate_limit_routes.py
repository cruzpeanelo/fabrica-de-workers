# -*- coding: utf-8 -*-
"""
Rate Limit Routes - Issue #393
==============================
API endpoints for managing rate limits.

Admin-only endpoints for:
- Viewing rate limit rules
- Setting tenant/user limits
- Resetting rate limits
- Viewing statistics
"""

from typing import Optional
from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel, Field
import logging

from factory.security.rate_limiter import (
    get_rate_limiter,
    RateLimitScope,
    RateLimitRule,
    check_rate_limit
)

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/rate-limits", tags=["Rate Limits"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class SetLimitRequest(BaseModel):
    """Request to set a rate limit."""
    identifier: str = Field(..., description="tenant_id, user_id, or endpoint")
    requests_per_minute: int = Field(..., ge=1, le=10000)


class RuleRequest(BaseModel):
    """Request to create a rate limit rule."""
    name: str = Field(..., min_length=1, max_length=100)
    scope: str = Field(..., description="global, tenant, user, ip, endpoint")
    requests_per_minute: int = Field(default=60, ge=1, le=10000)
    burst_size: int = Field(default=10, ge=1, le=1000)
    applies_to: Optional[str] = None


class CheckLimitRequest(BaseModel):
    """Request to check rate limit."""
    identifier: str
    scope: str = "ip"
    endpoint: Optional[str] = None


class ResetLimitRequest(BaseModel):
    """Request to reset a rate limit."""
    identifier: str
    scope: str = "ip"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def require_admin(request: Request):
    """Verify request is from admin user."""
    user = getattr(request.state, "user", None)
    if not user:
        role = request.headers.get("X-User-Role", "USER")
    else:
        role = user.get("role", "USER") if isinstance(user, dict) else getattr(user, "role", "USER")

    if role not in ("ADMIN", "admin"):
        raise HTTPException(403, "Admin access required")


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/rules")
async def list_rules(request: Request):
    """List all rate limit rules."""
    require_admin(request)

    rate_limiter = get_rate_limiter()

    return {
        "rules": rate_limiter.get_rules(),
        "count": len(rate_limiter._rules)
    }


@router.post("/rules")
async def create_rule(data: RuleRequest, request: Request):
    """Create a new rate limit rule."""
    require_admin(request)

    # Validate scope
    try:
        scope = RateLimitScope(data.scope.lower())
    except ValueError:
        raise HTTPException(
            400,
            f"Invalid scope: {data.scope}. Valid: global, tenant, user, ip, endpoint"
        )

    rule = RateLimitRule(
        name=data.name,
        scope=scope,
        requests_per_minute=data.requests_per_minute,
        burst_size=data.burst_size,
        applies_to=data.applies_to
    )

    rate_limiter = get_rate_limiter()
    rate_limiter.add_rule(rule)

    return {
        "message": "Rule created",
        "rule": rule.to_dict()
    }


@router.delete("/rules/{name}")
async def delete_rule(name: str, request: Request):
    """Delete a rate limit rule."""
    require_admin(request)

    rate_limiter = get_rate_limiter()

    if not rate_limiter.remove_rule(name):
        raise HTTPException(404, f"Rule not found: {name}")

    return {"message": "Rule deleted", "name": name}


@router.get("/tenant/{tenant_id}")
async def get_tenant_limit(tenant_id: str, request: Request):
    """Get rate limit for a tenant."""
    require_admin(request)

    rate_limiter = get_rate_limiter()
    limit = rate_limiter._tenant_limits.get(tenant_id)

    return {
        "tenant_id": tenant_id,
        "requests_per_minute": limit or "default (60)",
        "custom_limit": limit is not None
    }


@router.put("/tenant/{tenant_id}")
async def set_tenant_limit(tenant_id: str, data: SetLimitRequest, request: Request):
    """Set rate limit for a tenant."""
    require_admin(request)

    rate_limiter = get_rate_limiter()
    rate_limiter.set_tenant_limit(tenant_id, data.requests_per_minute)

    logger.info(f"Set rate limit for tenant {tenant_id}: {data.requests_per_minute}/min")

    return {
        "message": "Tenant limit updated",
        "tenant_id": tenant_id,
        "requests_per_minute": data.requests_per_minute
    }


@router.delete("/tenant/{tenant_id}")
async def remove_tenant_limit(tenant_id: str, request: Request):
    """Remove custom rate limit for a tenant (revert to default)."""
    require_admin(request)

    rate_limiter = get_rate_limiter()

    if tenant_id in rate_limiter._tenant_limits:
        del rate_limiter._tenant_limits[tenant_id]

    return {
        "message": "Tenant limit removed (using default)",
        "tenant_id": tenant_id
    }


@router.get("/user/{user_id}")
async def get_user_limit(user_id: str, request: Request):
    """Get rate limit for a user."""
    require_admin(request)

    rate_limiter = get_rate_limiter()
    limit = rate_limiter._user_limits.get(user_id)

    return {
        "user_id": user_id,
        "requests_per_minute": limit or "default (60)",
        "custom_limit": limit is not None
    }


@router.put("/user/{user_id}")
async def set_user_limit(user_id: str, data: SetLimitRequest, request: Request):
    """Set rate limit for a user."""
    require_admin(request)

    rate_limiter = get_rate_limiter()
    rate_limiter.set_user_limit(user_id, data.requests_per_minute)

    logger.info(f"Set rate limit for user {user_id}: {data.requests_per_minute}/min")

    return {
        "message": "User limit updated",
        "user_id": user_id,
        "requests_per_minute": data.requests_per_minute
    }


@router.delete("/user/{user_id}")
async def remove_user_limit(user_id: str, request: Request):
    """Remove custom rate limit for a user."""
    require_admin(request)

    rate_limiter = get_rate_limiter()

    if user_id in rate_limiter._user_limits:
        del rate_limiter._user_limits[user_id]

    return {
        "message": "User limit removed (using default)",
        "user_id": user_id
    }


@router.put("/endpoint")
async def set_endpoint_limit(data: SetLimitRequest, request: Request):
    """Set rate limit for an endpoint."""
    require_admin(request)

    rate_limiter = get_rate_limiter()
    rate_limiter.set_endpoint_limit(data.identifier, data.requests_per_minute)

    logger.info(f"Set rate limit for endpoint {data.identifier}: {data.requests_per_minute}/min")

    return {
        "message": "Endpoint limit updated",
        "endpoint": data.identifier,
        "requests_per_minute": data.requests_per_minute
    }


@router.post("/check")
async def check_limit(data: CheckLimitRequest, request: Request):
    """Check rate limit status for an identifier (testing purposes)."""
    require_admin(request)

    try:
        scope = RateLimitScope(data.scope.lower())
    except ValueError:
        raise HTTPException(400, f"Invalid scope: {data.scope}")

    result = check_rate_limit(
        identifier=data.identifier,
        scope=scope,
        endpoint=data.endpoint
    )

    return {
        "allowed": result.allowed,
        "limit": result.limit,
        "remaining": result.remaining,
        "reset_at": result.reset_at.isoformat(),
        "retry_after": result.retry_after,
        "scope": result.scope
    }


@router.post("/reset")
async def reset_limit(data: ResetLimitRequest, request: Request):
    """Reset rate limit for an identifier."""
    require_admin(request)

    try:
        scope = RateLimitScope(data.scope.lower())
    except ValueError:
        raise HTTPException(400, f"Invalid scope: {data.scope}")

    rate_limiter = get_rate_limiter()

    if not rate_limiter.reset_bucket(data.identifier, scope):
        return {
            "message": "No active rate limit bucket found",
            "identifier": data.identifier
        }

    return {
        "message": "Rate limit reset",
        "identifier": data.identifier,
        "scope": data.scope
    }


@router.get("/stats")
async def get_stats(request: Request):
    """Get rate limiter statistics."""
    require_admin(request)

    rate_limiter = get_rate_limiter()

    return {
        "stats": rate_limiter.get_stats(),
        "default_limit": 60,
        "auth_endpoint_limit": 10
    }


@router.get("/my-limit")
async def get_my_limit(request: Request):
    """
    Get current rate limit status for the requesting client.

    Non-admin endpoint - users can check their own status.
    """
    client_ip = request.headers.get("X-Forwarded-For", "").split(",")[0].strip()
    if not client_ip:
        client_ip = request.client.host if request.client else "unknown"

    user_id = request.headers.get("X-User-Id")

    result = check_rate_limit(
        identifier=client_ip,
        scope=RateLimitScope.IP
    )

    return {
        "ip": client_ip,
        "user_id": user_id,
        "allowed": result.allowed,
        "limit": result.limit,
        "remaining": result.remaining,
        "reset_at": result.reset_at.isoformat()
    }
