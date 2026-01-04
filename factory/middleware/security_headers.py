# -*- coding: utf-8 -*-
"""
Security Headers Middleware - Issue #345, #396
==============================================
HTTP security headers for protection against common attacks.

Features:
- X-Frame-Options (clickjacking protection)
- X-Content-Type-Options (MIME sniffing protection)
- Strict-Transport-Security (HTTPS enforcement)
- Content-Security-Policy (XSS protection)
- Referrer-Policy
- Permissions-Policy
- Per-tenant configuration (Issue #396)
"""

import os
import logging
from typing import Dict, Optional
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import Response

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Paths that should skip certain headers (e.g., API docs need frames)
SKIP_FRAME_OPTIONS_PATHS = [
    "/docs",
    "/redoc",
    "/openapi.json",
]

# Paths that should skip all security headers
SKIP_ALL_HEADERS_PATHS = [
    "/health",
    "/metrics",
]


class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """
    Middleware to add security headers to all responses.

    Headers are configured based on environment and tenant:
    - Development: Relaxed CSP for hot-reload
    - Production: Strict security headers
    - Per-tenant: Custom configuration per tenant (Issue #396)
    """

    # Base security headers (all environments)
    BASE_HEADERS: Dict[str, str] = {
        # Prevent clickjacking
        "X-Frame-Options": "DENY",

        # Prevent MIME type sniffing
        "X-Content-Type-Options": "nosniff",

        # XSS protection (legacy browsers)
        "X-XSS-Protection": "1; mode=block",

        # Referrer policy
        "Referrer-Policy": "strict-origin-when-cross-origin",

        # Permissions policy (disable dangerous features)
        "Permissions-Policy": "geolocation=(), microphone=(), camera=(), payment=()",
    }

    # Production-only headers
    PRODUCTION_HEADERS: Dict[str, str] = {
        # HSTS - enforce HTTPS for 1 year
        "Strict-Transport-Security": "max-age=31536000; includeSubDomains; preload",
    }

    # CSP by environment
    # Issue #456: Added media-src and frame-src for audio notifications and preview iframes
    CSP_DEVELOPMENT = (
        "default-src 'self'; "
        "script-src 'self' 'unsafe-inline' 'unsafe-eval' "
            "https://unpkg.com https://cdn.tailwindcss.com https://cdn.jsdelivr.net; "
        "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com https://cdn.jsdelivr.net; "
        "img-src 'self' data: https: blob:; "
        "font-src 'self' data: https://fonts.gstatic.com; "
        "connect-src 'self' ws: wss: http: https:; "
        "media-src 'self' data: blob:; "
        "frame-src 'self' http://localhost:3000 http://localhost:8000; "
        "frame-ancestors 'none';"
    )

    CSP_PRODUCTION = (
        "default-src 'self'; "
        "script-src 'self' 'unsafe-inline'; "
        "style-src 'self' 'unsafe-inline'; "
        "img-src 'self' data: https:; "
        "font-src 'self'; "
        "connect-src 'self' wss: https:; "
        "media-src 'self' data: blob:; "
        "frame-src 'self'; "
        "frame-ancestors 'none'; "
        "base-uri 'self'; "
        "form-action 'self'; "
        "upgrade-insecure-requests;"
    )

    def __init__(self, app, environment: str = None, report_uri: str = None):
        """
        Initialize security headers middleware.

        Args:
            app: ASGI application
            environment: Environment name (development, staging, production)
            report_uri: URI for CSP violation reports
        """
        super().__init__(app)
        self.environment = environment or os.getenv("ENVIRONMENT", "development")
        self.report_uri = report_uri
        self.is_production = self.environment in ("production", "staging")
        self.use_tenant_config = True  # Issue #396: Enable per-tenant config

        # Build default headers for this environment
        self.headers = self._build_headers()

        logger.info(f"[Security] Headers middleware initialized for {self.environment}")

    def _build_headers(self) -> Dict[str, str]:
        """Build headers dict based on environment."""
        headers = dict(self.BASE_HEADERS)

        # Add production headers
        if self.is_production:
            headers.update(self.PRODUCTION_HEADERS)

        # Add CSP
        csp = self.CSP_PRODUCTION if self.is_production else self.CSP_DEVELOPMENT

        # Add report-uri if configured
        if self.report_uri:
            csp += f" report-uri {self.report_uri};"

        headers["Content-Security-Policy"] = csp

        return headers

    def _should_skip_all(self, path: str) -> bool:
        """Check if path should skip all security headers."""
        for skip_path in SKIP_ALL_HEADERS_PATHS:
            if path.startswith(skip_path):
                return True
        return False

    def _should_skip_frame_options(self, path: str) -> bool:
        """Check if path should skip frame options (for docs)."""
        for skip_path in SKIP_FRAME_OPTIONS_PATHS:
            if path.startswith(skip_path):
                return True
        return False

    def _get_tenant_id(self, request: Request) -> Optional[str]:
        """Extract tenant ID from request."""
        if hasattr(request.state, "tenant_id"):
            return request.state.tenant_id
        return request.headers.get("X-Tenant-Id")

    def _get_tenant_headers(self, tenant_id: str) -> Optional[Dict[str, str]]:
        """Get tenant-specific headers if configured (Issue #396)."""
        try:
            from factory.security.security_headers import build_security_headers
            return build_security_headers(tenant_id)
        except ImportError:
            return None

    async def dispatch(self, request: Request, call_next) -> Response:
        """Add security headers to response."""
        path = request.url.path

        # Skip completely for certain paths
        if self._should_skip_all(path):
            return await call_next(request)

        response = await call_next(request)

        # Try tenant-specific headers first (Issue #396)
        headers = self.headers
        if self.use_tenant_config:
            tenant_id = self._get_tenant_id(request)
            if tenant_id:
                tenant_headers = self._get_tenant_headers(tenant_id)
                if tenant_headers:
                    headers = tenant_headers

        # Skip frame options for docs
        if self._should_skip_frame_options(path):
            headers = dict(headers)  # Copy to avoid modifying original
            headers.pop("X-Frame-Options", None)
            if "Content-Security-Policy" in headers:
                csp = headers["Content-Security-Policy"]
                csp = csp.replace("frame-ancestors 'none'", "frame-ancestors 'self'")
                headers["Content-Security-Policy"] = csp

        # Add security headers
        for header, value in headers.items():
            response.headers[header] = value

        return response


def get_security_headers_middleware(
    environment: str = None,
    report_uri: str = None
) -> type:
    """
    Factory function to create configured middleware class.

    Usage:
        app.add_middleware(
            get_security_headers_middleware(environment="production")
        )
    """
    class ConfiguredSecurityHeadersMiddleware(SecurityHeadersMiddleware):
        def __init__(self, app):
            super().__init__(app, environment=environment, report_uri=report_uri)

    return ConfiguredSecurityHeadersMiddleware


def add_security_headers_middleware(app):
    """Add security headers middleware to FastAPI app."""
    app.add_middleware(SecurityHeadersMiddleware)
    logger.info("Security headers middleware enabled")


def configure_security_headers(
    app,
    production: bool = False,
    csp_report_uri: Optional[str] = None
):
    """
    Configure and add security headers middleware.

    Args:
        app: FastAPI application
        production: Use production-ready strict headers
        csp_report_uri: URI for CSP violation reports
    """
    environment = "production" if production else "development"
    middleware_class = get_security_headers_middleware(
        environment=environment,
        report_uri=csp_report_uri
    )
    app.add_middleware(middleware_class)

    logger.info(
        f"Security headers configured: mode={environment}, report_uri={csp_report_uri}"
    )
