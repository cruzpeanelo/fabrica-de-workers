# -*- coding: utf-8 -*-
"""
Rate Limit Middleware - Issue #393
==================================
FastAPI middleware for rate limiting requests.

Applies rate limits based on:
- IP address (default)
- Tenant ID (if authenticated)
- User ID (if authenticated)
- Endpoint path
"""

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse
from typing import Optional
import logging

from factory.security.rate_limiter import (
    get_rate_limiter,
    RateLimitScope,
    RateLimitResult
)

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Paths excluded from rate limiting
EXCLUDED_PATHS = [
    "/health",
    "/docs",
    "/openapi.json",
    "/redoc",
    "/favicon.ico",
    "/static",
]

# Paths with stricter rate limits (handled by rules)
SENSITIVE_PATHS = [
    "/api/auth/login",
    "/api/auth/register",
    "/api/auth/forgot-password",
    "/api/auth/reset-password",
]


# =============================================================================
# MIDDLEWARE
# =============================================================================

class RateLimitMiddleware(BaseHTTPMiddleware):
    """
    Middleware for rate limiting API requests.

    Checks rate limits based on client IP, tenant, and user.
    Returns 429 Too Many Requests when limit exceeded.
    """

    async def dispatch(self, request: Request, call_next):
        """Process request through rate limiter."""
        # Skip excluded paths
        path = request.url.path
        if self._should_skip(path):
            return await call_next(request)

        # Get client identifier
        client_ip = self._get_client_ip(request)
        tenant_id = self._get_tenant_id(request)
        user_id = self._get_user_id(request)

        # Check rate limit
        rate_limiter = get_rate_limiter()

        # Primary check by IP
        result = rate_limiter.check(
            identifier=client_ip,
            scope=RateLimitScope.IP,
            endpoint=path,
            tenant_id=tenant_id,
            user_id=user_id
        )

        if not result.allowed:
            return self._rate_limit_response(result, client_ip, path)

        # Additional check by user if authenticated
        if user_id:
            user_result = rate_limiter.check(
                identifier=user_id,
                scope=RateLimitScope.USER,
                endpoint=path,
                tenant_id=tenant_id
            )

            if not user_result.allowed:
                return self._rate_limit_response(user_result, user_id, path)

            # Use the more restrictive result for headers
            result = user_result if user_result.remaining < result.remaining else result

        # Process request
        response = await call_next(request)

        # Add rate limit headers to response
        for key, value in result.to_headers().items():
            response.headers[key] = value

        return response

    def _should_skip(self, path: str) -> bool:
        """Check if path should skip rate limiting."""
        for excluded in EXCLUDED_PATHS:
            if path.startswith(excluded):
                return True
        return False

    def _get_client_ip(self, request: Request) -> str:
        """Extract client IP from request."""
        # Check forwarded headers
        forwarded = request.headers.get("X-Forwarded-For")
        if forwarded:
            return forwarded.split(",")[0].strip()

        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip

        # Fall back to client host
        return request.client.host if request.client else "unknown"

    def _get_tenant_id(self, request: Request) -> Optional[str]:
        """Extract tenant ID from request."""
        # From request state (set by auth middleware)
        if hasattr(request.state, "tenant_id"):
            return request.state.tenant_id

        # From headers
        return request.headers.get("X-Tenant-Id")

    def _get_user_id(self, request: Request) -> Optional[str]:
        """Extract user ID from request."""
        # From request state
        if hasattr(request.state, "user"):
            user = request.state.user
            if isinstance(user, dict):
                return user.get("user_id") or user.get("sub")
            return getattr(user, "user_id", None)

        # From headers
        return request.headers.get("X-User-Id")

    def _rate_limit_response(
        self,
        result: RateLimitResult,
        identifier: str,
        path: str
    ) -> JSONResponse:
        """Create rate limit exceeded response."""
        logger.warning(
            f"Rate limit exceeded: {identifier} on {path} "
            f"(scope: {result.scope})"
        )

        return JSONResponse(
            status_code=429,
            content={
                "error": "Too Many Requests",
                "message": f"Rate limit exceeded. Try again in {result.retry_after} seconds.",
                "retry_after": result.retry_after,
                "limit": result.limit
            },
            headers=result.to_headers()
        )


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def add_rate_limit_middleware(app):
    """Add rate limit middleware to FastAPI app."""
    app.add_middleware(RateLimitMiddleware)
    logger.info("Rate limit middleware enabled")
