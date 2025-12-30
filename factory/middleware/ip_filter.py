# -*- coding: utf-8 -*-
"""
IP Filter Middleware - Issue #343
=================================
Middleware for enforcing IP policies per tenant.
"""

from typing import Optional, Callable, List
from fastapi import Request
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from factory.security.ip_policy import get_ip_policy_service, BlockReason


class IPFilterMiddleware(BaseHTTPMiddleware):
    """
    Middleware that enforces IP policies for each tenant.

    Checks the client IP against tenant-specific rules:
    - IP whitelist/blacklist
    - CIDR ranges
    - Country restrictions
    """

    def __init__(
        self,
        app,
        excluded_paths: Optional[List[str]] = None
    ):
        """
        Initialize middleware.

        Args:
            app: FastAPI application
            excluded_paths: Paths that bypass IP filtering
        """
        super().__init__(app)
        self.excluded_paths = excluded_paths or [
            "/docs",
            "/redoc",
            "/openapi.json",
            "/health",
            "/api/auth/login",
        ]

    async def dispatch(self, request: Request, call_next: Callable):
        """Process the request."""
        path = request.url.path

        # Skip excluded paths
        if self._is_excluded(path):
            return await call_next(request)

        # Get tenant ID from request
        tenant_id = self._get_tenant_id(request)
        if not tenant_id:
            return await call_next(request)

        # Get client IP
        client_ip = self._get_client_ip(request)

        # Get user info from request state
        is_admin = getattr(request.state, "is_admin", False)
        is_super_admin = getattr(request.state, "is_super_admin", False)
        is_api_key = getattr(request.state, "auth_method", None) == "api_key"
        user_agent = request.headers.get("User-Agent", "")

        # Check access
        service = get_ip_policy_service()
        allowed, reason, message = service.check_access(
            tenant_id=tenant_id,
            client_ip=client_ip,
            is_admin=is_admin,
            is_super_admin=is_super_admin,
            is_api_key=is_api_key,
            path=path,
            user_agent=user_agent
        )

        if not allowed:
            # Get country for response headers
            from factory.security.ip_policy import GeoIPService
            country = GeoIPService.get_country(client_ip)

            return JSONResponse(
                status_code=403,
                content={
                    "error": "access_denied",
                    "reason": reason.value if reason else "ip_blocked",
                    "message": message or "Access denied"
                },
                headers={
                    "X-Blocked-Reason": reason.value if reason else "unknown",
                    "X-Client-IP": client_ip,
                    "X-Client-Country": country or "unknown"
                }
            )

        return await call_next(request)

    def _is_excluded(self, path: str) -> bool:
        """Check if path is excluded from IP filtering."""
        for excluded in self.excluded_paths:
            if path.startswith(excluded):
                return True
        return False

    def _get_tenant_id(self, request: Request) -> Optional[str]:
        """Extract tenant ID from request."""
        # Try from request state (set by tenant middleware)
        tenant_id = getattr(request.state, "tenant_id", None)
        if tenant_id:
            return tenant_id

        # Try from header
        return request.headers.get("X-Tenant-Id")

    def _get_client_ip(self, request: Request) -> str:
        """
        Get client IP address.

        Handles proxies via X-Forwarded-For header.
        """
        # Check X-Forwarded-For header (for reverse proxies)
        forwarded_for = request.headers.get("X-Forwarded-For")
        if forwarded_for:
            # Take the first IP (original client)
            return forwarded_for.split(",")[0].strip()

        # Check X-Real-IP header
        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip

        # Fallback to client host
        client = request.client
        if client:
            return client.host

        return "unknown"
