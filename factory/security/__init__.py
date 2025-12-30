# -*- coding: utf-8 -*-
"""
Security Module for Fabrica de Agentes
======================================

This module provides security features including:
- Tenant isolation (Issue #122)
- Row-level security
- Query interceptors
- Audit logging for cross-tenant access attempts
- Per-tenant encryption

Author: Fabrica de Agentes
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
]
