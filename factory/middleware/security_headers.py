# -*- coding: utf-8 -*-
"""
Security Headers Middleware - Issue #345
========================================
HTTP security headers for protection against common attacks.

Features:
- X-Frame-Options (clickjacking protection)
- X-Content-Type-Options (MIME sniffing protection)
- Strict-Transport-Security (HTTPS enforcement)
- Content-Security-Policy (XSS protection)
- Referrer-Policy
- Permissions-Policy
"""

import os
import logging
from typing import Dict, Optional
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import Response

logger = logging.getLogger(__name__)


class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """
    Middleware to add security headers to all responses.

    Headers are configured based on environment:
    - Development: Relaxed CSP for hot-reload
    - Production: Strict security headers
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
    CSP_DEVELOPMENT = (
        "default-src 'self'; "
        "script-src 'self' 'unsafe-inline' 'unsafe-eval'; "
        "style-src 'self' 'unsafe-inline'; "
        "img-src 'self' data: https: blob:; "
        "font-src 'self' data:; "
        "connect-src 'self' ws: wss: http: https:; "
        "frame-ancestors 'none';"
    )

    CSP_PRODUCTION = (
        "default-src 'self'; "
        "script-src 'self' 'unsafe-inline'; "
        "style-src 'self' 'unsafe-inline'; "
        "img-src 'self' data: https:; "
        "font-src 'self'; "
        "connect-src 'self' wss: https:; "
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

        # Build headers for this environment
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

    async def dispatch(self, request: Request, call_next) -> Response:
        """Add security headers to response."""
        response = await call_next(request)

        # Add security headers
        for header, value in self.headers.items():
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
