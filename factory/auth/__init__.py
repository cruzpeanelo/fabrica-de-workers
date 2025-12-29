"""
Factory Auth Module - SSO Integration and RBAC
Includes SAML 2.0, Azure AD OAuth2 support, and Role-Based Access Control
"""
from .sso import (
    sso_router,
    SSOConfig,
    SAMLConfig,
    AzureADConfig,
    get_sso_config,
    is_sso_enabled
)

from .rbac import (
    RBACManager,
    require_permission,
    require_role,
    get_current_user,
    check_permission,
    DEFAULT_ROLES,
    RESOURCES,
    ACTIONS,
    rbac_router
)

__all__ = [
    # SSO
    'sso_router',
    'SSOConfig',
    'SAMLConfig',
    'AzureADConfig',
    'get_sso_config',
    'is_sso_enabled',
    # RBAC
    'RBACManager',
    'require_permission',
    'require_role',
    'get_current_user',
    'check_permission',
    'DEFAULT_ROLES',
    'RESOURCES',
    'ACTIONS',
    'rbac_router'
]
