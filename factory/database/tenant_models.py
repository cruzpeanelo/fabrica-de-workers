# -*- coding: utf-8 -*-
"""
Modelos de Multi-tenancy e White Label para a Plataforma E v5.0

DEPRECATED: Este arquivo foi mantido apenas para compatibilidade.
Todos os modelos de multi-tenancy foram unificados em models.py (Issues #81, #82).

Use os imports de models.py diretamente:
    from factory.database.models import (
        Tenant, TenantSettings, BrandingConfig, TenantMember,
        TenantInvite, TenantUsageLog, ProjectMember,
        TenantStatus, TenantPlan, MemberRole, MemberStatus, InviteStatus
    )
"""

# Re-exportar todos os modelos de multi-tenancy do models.py consolidado
from factory.database.models import (
    # Enums
    TenantStatus,
    TenantPlan,
    MemberRole,
    MemberStatus,
    InviteStatus,
    ProjectRole,

    # Modelos principais
    Tenant,
    TenantSettings,
    BrandingConfig,
    TenantMember,
    TenantInvite,
    TenantUsageLog,
    TenantAuditLog,  # Issue #430: Audit log para compliance
    ProjectMember,
)

# Aliases para compatibilidade com codigo legado
# Estes aliases serao removidos em uma versao futura

__all__ = [
    # Enums
    "TenantStatus",
    "TenantPlan",
    "MemberRole",
    "MemberStatus",
    "InviteStatus",
    "ProjectRole",

    # Modelos
    "Tenant",
    "TenantSettings",
    "BrandingConfig",
    "TenantMember",
    "TenantInvite",
    "TenantUsageLog",
    "TenantAuditLog",  # Issue #430
    "ProjectMember",
]

# Aviso de deprecacao
import warnings

def _deprecated_warning():
    warnings.warn(
        "tenant_models.py esta deprecated. "
        "Use 'from factory.database.models import ...' diretamente. "
        "Este arquivo sera removido em uma versao futura.",
        DeprecationWarning,
        stacklevel=3
    )

# Emitir aviso apenas uma vez na importacao
_deprecated_warning()
