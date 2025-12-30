# -*- coding: utf-8 -*-
"""
Security Headers - Issue #396
=============================
Security header configuration for web applications.

Implements OWASP recommended security headers:
- HSTS (HTTP Strict Transport Security)
- CSP (Content Security Policy)
- X-Frame-Options
- X-Content-Type-Options
- X-XSS-Protection
- Referrer-Policy
- Permissions-Policy
"""

from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field
from enum import Enum
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS
# =============================================================================

class FrameOptions(str, Enum):
    """X-Frame-Options values."""
    DENY = "DENY"
    SAMEORIGIN = "SAMEORIGIN"


class ReferrerPolicy(str, Enum):
    """Referrer-Policy values."""
    NO_REFERRER = "no-referrer"
    NO_REFERRER_WHEN_DOWNGRADE = "no-referrer-when-downgrade"
    ORIGIN = "origin"
    ORIGIN_WHEN_CROSS_ORIGIN = "origin-when-cross-origin"
    SAME_ORIGIN = "same-origin"
    STRICT_ORIGIN = "strict-origin"
    STRICT_ORIGIN_WHEN_CROSS_ORIGIN = "strict-origin-when-cross-origin"
    UNSAFE_URL = "unsafe-url"


# =============================================================================
# CSP CONFIGURATION
# =============================================================================

@dataclass
class CSPConfig:
    """Content Security Policy configuration."""

    default_src: List[str] = field(default_factory=lambda: ["'self'"])
    script_src: List[str] = field(default_factory=lambda: ["'self'"])
    style_src: List[str] = field(default_factory=lambda: ["'self'", "'unsafe-inline'"])
    img_src: List[str] = field(default_factory=lambda: ["'self'", "data:", "https:"])
    font_src: List[str] = field(default_factory=lambda: ["'self'", "https:"])
    connect_src: List[str] = field(default_factory=lambda: ["'self'"])
    frame_src: List[str] = field(default_factory=lambda: ["'none'"])
    object_src: List[str] = field(default_factory=lambda: ["'none'"])
    base_uri: List[str] = field(default_factory=lambda: ["'self'"])
    form_action: List[str] = field(default_factory=lambda: ["'self'"])
    frame_ancestors: List[str] = field(default_factory=lambda: ["'none'"])
    upgrade_insecure_requests: bool = True
    report_uri: Optional[str] = None
    report_only: bool = False

    def build(self) -> str:
        """Build CSP header value."""
        directives = []

        if self.default_src:
            directives.append(f"default-src {' '.join(self.default_src)}")
        if self.script_src:
            directives.append(f"script-src {' '.join(self.script_src)}")
        if self.style_src:
            directives.append(f"style-src {' '.join(self.style_src)}")
        if self.img_src:
            directives.append(f"img-src {' '.join(self.img_src)}")
        if self.font_src:
            directives.append(f"font-src {' '.join(self.font_src)}")
        if self.connect_src:
            directives.append(f"connect-src {' '.join(self.connect_src)}")
        if self.frame_src:
            directives.append(f"frame-src {' '.join(self.frame_src)}")
        if self.object_src:
            directives.append(f"object-src {' '.join(self.object_src)}")
        if self.base_uri:
            directives.append(f"base-uri {' '.join(self.base_uri)}")
        if self.form_action:
            directives.append(f"form-action {' '.join(self.form_action)}")
        if self.frame_ancestors:
            directives.append(f"frame-ancestors {' '.join(self.frame_ancestors)}")
        if self.upgrade_insecure_requests:
            directives.append("upgrade-insecure-requests")
        if self.report_uri:
            directives.append(f"report-uri {self.report_uri}")

        return "; ".join(directives)

    @classmethod
    def permissive(cls) -> "CSPConfig":
        """Create a permissive CSP for development."""
        return cls(
            default_src=["'self'", "'unsafe-inline'", "'unsafe-eval'", "https:", "data:"],
            script_src=["'self'", "'unsafe-inline'", "'unsafe-eval'", "https:"],
            style_src=["'self'", "'unsafe-inline'", "https:"],
            img_src=["'self'", "data:", "https:", "blob:"],
            connect_src=["'self'", "https:", "wss:"],
            frame_src=["'self'"],
            frame_ancestors=["'self'"],
            upgrade_insecure_requests=False
        )

    @classmethod
    def strict(cls) -> "CSPConfig":
        """Create a strict CSP for production."""
        return cls(
            default_src=["'self'"],
            script_src=["'self'"],
            style_src=["'self'"],
            img_src=["'self'"],
            font_src=["'self'"],
            connect_src=["'self'"],
            frame_src=["'none'"],
            object_src=["'none'"],
            base_uri=["'self'"],
            form_action=["'self'"],
            frame_ancestors=["'none'"],
            upgrade_insecure_requests=True
        )


# =============================================================================
# SECURITY HEADERS CONFIG
# =============================================================================

@dataclass
class SecurityHeadersConfig:
    """Complete security headers configuration."""

    # HSTS
    hsts_enabled: bool = True
    hsts_max_age: int = 31536000  # 1 year
    hsts_include_subdomains: bool = True
    hsts_preload: bool = False

    # X-Frame-Options
    frame_options: FrameOptions = FrameOptions.DENY

    # X-Content-Type-Options
    content_type_nosniff: bool = True

    # X-XSS-Protection (deprecated but still useful for older browsers)
    xss_protection: bool = True
    xss_protection_mode: str = "1; mode=block"

    # Referrer-Policy
    referrer_policy: ReferrerPolicy = ReferrerPolicy.STRICT_ORIGIN_WHEN_CROSS_ORIGIN

    # Permissions-Policy
    permissions_policy_enabled: bool = True
    permissions_policy: Dict[str, List[str]] = field(default_factory=dict)

    # CSP
    csp_enabled: bool = True
    csp: CSPConfig = field(default_factory=CSPConfig)

    # Custom headers
    custom_headers: Dict[str, str] = field(default_factory=dict)

    def __post_init__(self):
        """Initialize default permissions policy."""
        if not self.permissions_policy:
            self.permissions_policy = {
                "camera": [],
                "microphone": [],
                "geolocation": [],
                "payment": [],
                "usb": [],
                "accelerometer": [],
                "gyroscope": [],
                "magnetometer": []
            }

    def build_headers(self) -> Dict[str, str]:
        """Build all security headers."""
        headers = {}

        # HSTS
        if self.hsts_enabled:
            hsts_value = f"max-age={self.hsts_max_age}"
            if self.hsts_include_subdomains:
                hsts_value += "; includeSubDomains"
            if self.hsts_preload:
                hsts_value += "; preload"
            headers["Strict-Transport-Security"] = hsts_value

        # X-Frame-Options
        headers["X-Frame-Options"] = self.frame_options.value

        # X-Content-Type-Options
        if self.content_type_nosniff:
            headers["X-Content-Type-Options"] = "nosniff"

        # X-XSS-Protection
        if self.xss_protection:
            headers["X-XSS-Protection"] = self.xss_protection_mode

        # Referrer-Policy
        headers["Referrer-Policy"] = self.referrer_policy.value

        # Permissions-Policy
        if self.permissions_policy_enabled:
            policy_parts = []
            for feature, allowed in self.permissions_policy.items():
                if not allowed:
                    policy_parts.append(f"{feature}=()")
                else:
                    values = " ".join(allowed)
                    policy_parts.append(f"{feature}=({values})")
            if policy_parts:
                headers["Permissions-Policy"] = ", ".join(policy_parts)

        # CSP
        if self.csp_enabled:
            csp_value = self.csp.build()
            header_name = "Content-Security-Policy-Report-Only" if self.csp.report_only else "Content-Security-Policy"
            headers[header_name] = csp_value

        # Custom headers
        headers.update(self.custom_headers)

        return headers

    @classmethod
    def development(cls) -> "SecurityHeadersConfig":
        """Create development-friendly configuration."""
        return cls(
            hsts_enabled=False,
            frame_options=FrameOptions.SAMEORIGIN,
            csp_enabled=True,
            csp=CSPConfig.permissive(),
            permissions_policy_enabled=False
        )

    @classmethod
    def production(cls) -> "SecurityHeadersConfig":
        """Create production-ready configuration."""
        return cls(
            hsts_enabled=True,
            hsts_max_age=31536000,
            hsts_include_subdomains=True,
            hsts_preload=True,
            frame_options=FrameOptions.DENY,
            content_type_nosniff=True,
            xss_protection=True,
            referrer_policy=ReferrerPolicy.STRICT_ORIGIN_WHEN_CROSS_ORIGIN,
            permissions_policy_enabled=True,
            csp_enabled=True,
            csp=CSPConfig.strict()
        )


# =============================================================================
# TENANT-SPECIFIC CONFIGURATION
# =============================================================================

_tenant_configs: Dict[str, SecurityHeadersConfig] = {}
_default_config: Optional[SecurityHeadersConfig] = None


def get_security_headers_config(tenant_id: Optional[str] = None) -> SecurityHeadersConfig:
    """Get security headers configuration for a tenant."""
    if tenant_id and tenant_id in _tenant_configs:
        return _tenant_configs[tenant_id]

    global _default_config
    if _default_config is None:
        _default_config = SecurityHeadersConfig.development()

    return _default_config


def set_security_headers_config(
    config: SecurityHeadersConfig,
    tenant_id: Optional[str] = None
):
    """Set security headers configuration."""
    if tenant_id:
        _tenant_configs[tenant_id] = config
        logger.info(f"Set security headers config for tenant {tenant_id}")
    else:
        global _default_config
        _default_config = config
        logger.info("Set default security headers config")


def set_production_mode():
    """Enable production security headers."""
    set_security_headers_config(SecurityHeadersConfig.production())
    logger.info("Security headers set to production mode")


def set_development_mode():
    """Enable development security headers."""
    set_security_headers_config(SecurityHeadersConfig.development())
    logger.info("Security headers set to development mode")


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def build_security_headers(tenant_id: Optional[str] = None) -> Dict[str, str]:
    """Build security headers for a tenant."""
    config = get_security_headers_config(tenant_id)
    return config.build_headers()
