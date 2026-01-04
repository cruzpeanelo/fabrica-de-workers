# -*- coding: utf-8 -*-
"""
Security Module for Plataforma E
======================================

This module provides security features including:
- Tenant isolation (Issue #122)
- Row-level security
- Query interceptors
- Audit logging for cross-tenant access attempts
- Per-tenant encryption

Author: Plataforma E
"""

try:
    from factory.config.vault_client import VaultClient
except ImportError:
    VaultClient = None

from .tenant_isolation import (
    TenantIsolation,
    TenantContext,
    get_tenant_context,
    set_tenant_context,
    clear_tenant_context,
    require_tenant,
    TenantQueryInterceptor,
    tenant_query_interceptor,
    TenantEncryption,
    get_tenant_encryption,
    CrossTenantAccessAudit,
    audit_cross_tenant_access,
    TenantIsolationError,
    CrossTenantAccessError,
    TenantNotFoundError,
    TenantIsolationMiddleware,
    IsolationLevel,
    AccessType,
    initialize_tenant_isolation,
)

# IP Policy (Issue #343)
from .ip_policy import (
    IPPolicyService,
    get_ip_policy_service,
    TenantIPPolicy,
    IPPolicyMode,
    BlockReason,
    GeoIPService,
)
from .ip_policy_routes import router as ip_policy_router

# Encryption at Rest (Issue #344)
from .encryption import (
    EncryptionService,
    KeyManager,
    BulkEncryptor,
    EncryptedString,
    encrypted_column,
    initialize_encryption,
    is_encryption_available,
)
from .encryption_routes import router as encryption_router

# Brute Force Protection (Issue #402)
from .brute_force import (
    BruteForceProtection,
    record_login_attempt,
    check_brute_force,
    is_blocked,
    unlock_account,
    whitelist_ip,
)

# CORS Multi-tenant (Issue #399)
from .cors_config import (
    CORSService,
    CORSConfig,
    get_cors_config,
    set_cors_config,
    add_allowed_origin,
    is_origin_allowed,
    get_cors_headers,
)

# CSRF Protection (Issue #411)
from .csrf import (
    CSRFService,
    generate_csrf_token,
    validate_csrf_token,
    get_csrf_cookie_options,
    CSRF_HEADER_NAME,
    CSRF_COOKIE_NAME,
)

# Rate Limiting (Issue #393)
from .rate_limiter import (
    RateLimiter,
    RateLimitRule,
    RateLimitScope,
    RateLimitResult,
    check_rate_limit,
    get_rate_limiter,
)

# Security Headers (Issue #396)
from .security_headers import (
    SecurityHeadersConfig,
    CSPConfig,
    build_security_headers,
    get_security_headers_config,
)

__all__ = [
    "VaultClient",
    "TenantIsolation",
    "TenantContext",
    "get_tenant_context",
    "set_tenant_context",
    "clear_tenant_context",
    "require_tenant",
    "TenantQueryInterceptor",
    "tenant_query_interceptor",
    "TenantEncryption",
    "get_tenant_encryption",
    "CrossTenantAccessAudit",
    "audit_cross_tenant_access",
    "TenantIsolationError",
    "CrossTenantAccessError",
    "TenantNotFoundError",
    "TenantIsolationMiddleware",
    "IsolationLevel",
    "AccessType",
    "initialize_tenant_isolation",
    # IP Policy (Issue #343)
    "IPPolicyService",
    "get_ip_policy_service",
    "TenantIPPolicy",
    "IPPolicyMode",
    "BlockReason",
    "GeoIPService",
    "ip_policy_router",
    # Encryption at Rest (Issue #344)
    "EncryptionService",
    "KeyManager",
    "BulkEncryptor",
    "EncryptedString",
    "encrypted_column",
    "initialize_encryption",
    "is_encryption_available",
    "encryption_router",
    # Brute Force Protection (Issue #402)
    "BruteForceProtection",
    "record_login_attempt",
    "check_brute_force",
    "is_blocked",
    "unlock_account",
    "whitelist_ip",
    # CORS Multi-tenant (Issue #399)
    "CORSService",
    "CORSConfig",
    "get_cors_config",
    "set_cors_config",
    "add_allowed_origin",
    "is_origin_allowed",
    "get_cors_headers",
    # CSRF Protection (Issue #411)
    "CSRFService",
    "generate_csrf_token",
    "validate_csrf_token",
    "get_csrf_cookie_options",
    "CSRF_HEADER_NAME",
    "CSRF_COOKIE_NAME",
    # Rate Limiting (Issue #393)
    "RateLimiter",
    "RateLimitRule",
    "RateLimitScope",
    "RateLimitResult",
    "check_rate_limit",
    "get_rate_limiter",
    # Security Headers (Issue #396)
    "SecurityHeadersConfig",
    "CSPConfig",
    "build_security_headers",
    "get_security_headers_config",
]
