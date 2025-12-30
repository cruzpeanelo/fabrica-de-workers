"""
Factory Infrastructure Module
Contains infrastructure components for enterprise deployments:
- HashiCorp Vault integration (secrets management)
- WAF configuration (web application firewall)
- Infrastructure-as-code helpers
"""

from .vault_client import (
    VaultClient,
    VaultConfig,
    SecretCache,
    get_vault_client,
    get_secret,
    rotate_secret,
)
from .waf_config import (
    WAFConfig,
    WAFRule,
    RateLimitRule,
    IPFilter,
    WAFManager,
    create_waf_middleware,
)

__all__ = [
    # Vault
    "VaultClient",
    "VaultConfig",
    "SecretCache",
    "get_vault_client",
    "get_secret",
    "rotate_secret",
    # WAF
    "WAFConfig",
    "WAFRule",
    "RateLimitRule",
    "IPFilter",
    "WAFManager",
    "create_waf_middleware",
]
