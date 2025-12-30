# -*- coding: utf-8 -*-
"""
CORS Configuration - Issue #399
===============================
Multi-tenant CORS configuration and validation.

Features:
- Per-tenant allowed origins
- Wildcard subdomain support (*.example.com)
- Dynamic configuration via API
- Fallback to global defaults
"""

import re
import fnmatch
from datetime import datetime
from typing import Optional, List, Dict, Any, Set
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION DEFAULTS
# =============================================================================

DEFAULT_ALLOWED_ORIGINS = [
    "http://localhost:3000",
    "http://localhost:8000",
    "http://localhost:9001",
    "http://127.0.0.1:3000",
    "http://127.0.0.1:8000",
    "http://127.0.0.1:9001",
]

DEFAULT_ALLOWED_METHODS = [
    "GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD"
]

DEFAULT_ALLOWED_HEADERS = [
    "Accept",
    "Accept-Language",
    "Content-Type",
    "Content-Language",
    "Authorization",
    "X-Requested-With",
    "X-Tenant-Id",
    "X-User-Id",
    "X-Device-Id",
    "X-Request-Id",
]

DEFAULT_EXPOSE_HEADERS = [
    "X-RateLimit-Limit",
    "X-RateLimit-Remaining",
    "X-RateLimit-Reset",
    "X-Request-Id",
]


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class CORSConfig:
    """CORS configuration for a tenant."""
    tenant_id: Optional[str] = None
    allowed_origins: List[str] = field(default_factory=list)
    allowed_methods: List[str] = field(default_factory=lambda: DEFAULT_ALLOWED_METHODS.copy())
    allowed_headers: List[str] = field(default_factory=lambda: DEFAULT_ALLOWED_HEADERS.copy())
    expose_headers: List[str] = field(default_factory=lambda: DEFAULT_EXPOSE_HEADERS.copy())
    allow_credentials: bool = True
    max_age: int = 3600  # 1 hour
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def __post_init__(self):
        if not self.allowed_origins:
            self.allowed_origins = DEFAULT_ALLOWED_ORIGINS.copy()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "allowed_origins": self.allowed_origins,
            "allowed_methods": self.allowed_methods,
            "allowed_headers": self.allowed_headers,
            "expose_headers": self.expose_headers,
            "allow_credentials": self.allow_credentials,
            "max_age": self.max_age,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat()
        }

    def add_origin(self, origin: str):
        """Add an allowed origin."""
        if origin not in self.allowed_origins:
            self.allowed_origins.append(origin)
            self.updated_at = datetime.utcnow()

    def remove_origin(self, origin: str) -> bool:
        """Remove an allowed origin."""
        if origin in self.allowed_origins:
            self.allowed_origins.remove(origin)
            self.updated_at = datetime.utcnow()
            return True
        return False


# =============================================================================
# STORAGE
# =============================================================================

_tenant_configs: Dict[str, CORSConfig] = {}
_global_config: Optional[CORSConfig] = None


# =============================================================================
# CORS SERVICE
# =============================================================================

class CORSService:
    """
    Service for managing CORS configuration per tenant.

    Supports:
    - Per-tenant configuration
    - Wildcard patterns (*.example.com)
    - Dynamic configuration updates
    """

    # -------------------------------------------------------------------------
    # CONFIGURATION MANAGEMENT
    # -------------------------------------------------------------------------

    @classmethod
    def get_config(cls, tenant_id: Optional[str] = None) -> CORSConfig:
        """Get CORS config for a tenant or global default."""
        if tenant_id and tenant_id in _tenant_configs:
            return _tenant_configs[tenant_id]

        global _global_config
        if _global_config is None:
            _global_config = CORSConfig()

        return _global_config

    @classmethod
    def set_config(cls, config: CORSConfig, tenant_id: Optional[str] = None):
        """Set CORS config for a tenant or global default."""
        if tenant_id:
            config.tenant_id = tenant_id
            _tenant_configs[tenant_id] = config
            logger.info(f"Set CORS config for tenant {tenant_id}: {len(config.allowed_origins)} origins")
        else:
            global _global_config
            _global_config = config
            logger.info(f"Set global CORS config: {len(config.allowed_origins)} origins")

    @classmethod
    def delete_config(cls, tenant_id: str) -> bool:
        """Delete tenant-specific config (revert to global)."""
        if tenant_id in _tenant_configs:
            del _tenant_configs[tenant_id]
            logger.info(f"Deleted CORS config for tenant {tenant_id}")
            return True
        return False

    @classmethod
    def list_configs(cls) -> List[Dict[str, Any]]:
        """List all tenant configs."""
        configs = []

        # Add global config
        if _global_config:
            global_dict = _global_config.to_dict()
            global_dict["type"] = "global"
            configs.append(global_dict)

        # Add tenant configs
        for tenant_id, config in _tenant_configs.items():
            config_dict = config.to_dict()
            config_dict["type"] = "tenant"
            configs.append(config_dict)

        return configs

    # -------------------------------------------------------------------------
    # ORIGIN MANAGEMENT
    # -------------------------------------------------------------------------

    @classmethod
    def add_origin(
        cls,
        origin: str,
        tenant_id: Optional[str] = None
    ) -> CORSConfig:
        """Add an allowed origin to a tenant's config."""
        config = cls.get_config(tenant_id)

        # Create tenant config if doesn't exist
        if tenant_id and tenant_id not in _tenant_configs:
            config = CORSConfig(tenant_id=tenant_id)
            _tenant_configs[tenant_id] = config

        config.add_origin(origin)
        logger.info(f"Added origin {origin} for tenant {tenant_id or 'global'}")

        return config

    @classmethod
    def remove_origin(
        cls,
        origin: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """Remove an allowed origin from a tenant's config."""
        config = cls.get_config(tenant_id)

        if config.remove_origin(origin):
            logger.info(f"Removed origin {origin} for tenant {tenant_id or 'global'}")
            return True

        return False

    @classmethod
    def get_origins(cls, tenant_id: Optional[str] = None) -> List[str]:
        """Get allowed origins for a tenant."""
        config = cls.get_config(tenant_id)
        return config.allowed_origins.copy()

    # -------------------------------------------------------------------------
    # ORIGIN VALIDATION
    # -------------------------------------------------------------------------

    @classmethod
    def is_origin_allowed(
        cls,
        origin: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """
        Check if an origin is allowed.

        Supports:
        - Exact match
        - Wildcard patterns (*.example.com)
        - Protocol wildcards (http(s)://example.com)
        """
        if not origin:
            return False

        config = cls.get_config(tenant_id)
        allowed_origins = config.allowed_origins

        # Check each allowed origin
        for allowed in allowed_origins:
            if cls._matches_origin(origin, allowed):
                return True

        # If tenant config exists but doesn't match, try global
        if tenant_id and _global_config:
            for allowed in _global_config.allowed_origins:
                if cls._matches_origin(origin, allowed):
                    return True

        return False

    @classmethod
    def _matches_origin(cls, origin: str, pattern: str) -> bool:
        """
        Check if origin matches a pattern.

        Patterns:
        - Exact: "https://example.com"
        - Wildcard: "*.example.com" or "https://*.example.com"
        - Any: "*"
        """
        # Wildcard for all
        if pattern == "*":
            return True

        # Exact match
        if origin == pattern:
            return True

        # Wildcard subdomain pattern
        if "*" in pattern:
            # Convert pattern to regex
            # *.example.com -> matches foo.example.com, bar.example.com
            regex_pattern = pattern.replace(".", r"\.").replace("*", r"[^\.]+")
            try:
                if re.match(f"^{regex_pattern}$", origin):
                    return True
            except re.error:
                pass

            # Also try with fnmatch for simpler patterns
            if fnmatch.fnmatch(origin, pattern):
                return True

        return False

    @classmethod
    def validate_and_get_origin(
        cls,
        origin: str,
        tenant_id: Optional[str] = None
    ) -> Optional[str]:
        """
        Validate origin and return it if allowed.

        Returns:
            Origin string if allowed, None otherwise
        """
        if cls.is_origin_allowed(origin, tenant_id):
            return origin
        return None

    # -------------------------------------------------------------------------
    # CORS HEADERS
    # -------------------------------------------------------------------------

    @classmethod
    def get_cors_headers(
        cls,
        origin: str,
        tenant_id: Optional[str] = None,
        is_preflight: bool = False
    ) -> Dict[str, str]:
        """
        Build CORS response headers for a request.

        Args:
            origin: Request Origin header
            tenant_id: Tenant ID for config lookup
            is_preflight: True for OPTIONS preflight requests

        Returns:
            Dict of CORS headers to add to response
        """
        headers = {}
        config = cls.get_config(tenant_id)

        # Validate origin
        if not cls.is_origin_allowed(origin, tenant_id):
            return headers

        # Access-Control-Allow-Origin
        headers["Access-Control-Allow-Origin"] = origin

        # Access-Control-Allow-Credentials
        if config.allow_credentials:
            headers["Access-Control-Allow-Credentials"] = "true"

        # Preflight-specific headers
        if is_preflight:
            # Access-Control-Allow-Methods
            headers["Access-Control-Allow-Methods"] = ", ".join(config.allowed_methods)

            # Access-Control-Allow-Headers
            headers["Access-Control-Allow-Headers"] = ", ".join(config.allowed_headers)

            # Access-Control-Max-Age
            headers["Access-Control-Max-Age"] = str(config.max_age)

        # Always expose headers
        if config.expose_headers:
            headers["Access-Control-Expose-Headers"] = ", ".join(config.expose_headers)

        return headers


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def get_cors_config(tenant_id: Optional[str] = None) -> CORSConfig:
    """Get CORS configuration for a tenant."""
    return CORSService.get_config(tenant_id)


def set_cors_config(
    allowed_origins: List[str],
    tenant_id: Optional[str] = None,
    allow_credentials: bool = True
):
    """Set CORS configuration."""
    config = CORSConfig(
        tenant_id=tenant_id,
        allowed_origins=allowed_origins,
        allow_credentials=allow_credentials
    )
    CORSService.set_config(config, tenant_id)


def add_allowed_origin(origin: str, tenant_id: Optional[str] = None):
    """Add an allowed origin."""
    CORSService.add_origin(origin, tenant_id)


def is_origin_allowed(origin: str, tenant_id: Optional[str] = None) -> bool:
    """Check if origin is allowed."""
    return CORSService.is_origin_allowed(origin, tenant_id)


def get_cors_headers(
    origin: str,
    tenant_id: Optional[str] = None,
    is_preflight: bool = False
) -> Dict[str, str]:
    """Get CORS headers for a request."""
    return CORSService.get_cors_headers(origin, tenant_id, is_preflight)
