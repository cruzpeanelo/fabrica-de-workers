# -*- coding: utf-8 -*-
"""
API Key Authentication Middleware - Issue #341
==============================================
Middleware for authenticating requests via API keys.

Supports both API key and JWT authentication.
API key takes precedence if X-API-Key header is present.
"""

from typing import Optional, Callable
from fastapi import Request, HTTPException
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from factory.auth.api_keys import validate_api_key_header, APIKey


class APIKeyAuthMiddleware(BaseHTTPMiddleware):
    """
    Middleware that authenticates requests using API keys.

    If X-API-Key header is present, validates the key.
    Otherwise, falls through to JWT authentication.
    """

    def __init__(
        self,
        app,
        excluded_paths: Optional[list] = None,
        required_paths: Optional[list] = None
    ):
        """
        Initialize middleware.

        Args:
            app: FastAPI application
            excluded_paths: Paths that don't require API key
            required_paths: Paths that require API key (if set, only these are checked)
        """
        super().__init__(app)
        self.excluded_paths = excluded_paths or [
            "/docs",
            "/redoc",
            "/openapi.json",
            "/health",
            "/api/auth/login",
            "/api/auth/register",
        ]
        self.required_paths = required_paths

    async def dispatch(self, request: Request, call_next: Callable):
        """Process the request."""
        path = request.url.path

        # Skip excluded paths
        if self._is_excluded(path):
            return await call_next(request)

        # Check if API key header is present
        api_key_header = request.headers.get("X-API-Key")

        if api_key_header:
            # Validate API key
            success, key, message = validate_api_key_header(api_key_header)

            if not success:
                return JSONResponse(
                    status_code=401,
                    content={
                        "error": "api_key_invalid",
                        "message": message
                    },
                    headers={
                        "WWW-Authenticate": "API-Key",
                        "X-Auth-Method": "api_key"
                    }
                )

            # Add key info to request state for use in endpoints
            request.state.api_key = key
            request.state.auth_method = "api_key"
            request.state.tenant_id = key.tenant_id

            # Add rate limit headers to response
            response = await call_next(request)
            self._add_rate_limit_headers(response, key)
            return response

        # No API key - continue to JWT auth
        request.state.auth_method = "jwt"
        return await call_next(request)

    def _is_excluded(self, path: str) -> bool:
        """Check if path is excluded from API key auth."""
        for excluded in self.excluded_paths:
            if path.startswith(excluded):
                return True
        return False

    def _add_rate_limit_headers(self, response, key: APIKey):
        """Add rate limit headers to response."""
        from factory.auth.api_keys import get_api_key_service

        service = get_api_key_service()
        status = service.get_rate_limit_status(key.id)

        if "error" not in status:
            response.headers["X-RateLimit-Limit"] = str(status["limit"])
            response.headers["X-RateLimit-Remaining"] = str(status["remaining"])
            response.headers["X-RateLimit-Reset"] = status["reset_at"]


# =============================================================================
# DEPENDENCY FOR FASTAPI
# =============================================================================

def require_api_key(required_scope: Optional[str] = None):
    """
    FastAPI dependency that requires API key authentication.

    Usage:
        @app.get("/api/data")
        async def get_data(key: APIKey = Depends(require_api_key("data:read"))):
            return {"tenant_id": key.tenant_id}
    """
    async def dependency(request: Request) -> APIKey:
        api_key_header = request.headers.get("X-API-Key")

        if not api_key_header:
            raise HTTPException(
                status_code=401,
                detail="API key required",
                headers={"WWW-Authenticate": "API-Key"}
            )

        success, key, message = validate_api_key_header(
            api_key_header,
            required_scope
        )

        if not success:
            raise HTTPException(
                status_code=401 if "not found" in message or "Invalid" in message else 403,
                detail=message
            )

        return key

    return dependency


def optional_api_key():
    """
    FastAPI dependency that optionally extracts API key.

    Returns None if no API key provided.
    """
    async def dependency(request: Request) -> Optional[APIKey]:
        api_key_header = request.headers.get("X-API-Key")

        if not api_key_header:
            return None

        success, key, _ = validate_api_key_header(api_key_header)
        return key if success else None

    return dependency


# =============================================================================
# SCOPE-BASED DECORATORS
# =============================================================================

def require_scope(scope: str):
    """
    Decorator to require a specific scope.

    Usage:
        @app.get("/api/stories")
        @require_scope("stories:read")
        async def list_stories(request: Request):
            ...
    """
    def decorator(func: Callable):
        async def wrapper(request: Request, *args, **kwargs):
            key = getattr(request.state, "api_key", None)

            if not key:
                raise HTTPException(401, "API key required")

            if not key.has_scope(scope):
                raise HTTPException(
                    403,
                    f"Missing required scope: {scope}"
                )

            return await func(request, *args, **kwargs)

        wrapper.__name__ = func.__name__
        wrapper.__doc__ = func.__doc__
        return wrapper

    return decorator
