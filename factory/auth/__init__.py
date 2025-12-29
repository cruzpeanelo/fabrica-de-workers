"""
Factory Auth Module - SSO Integration and RBAC
Includes SAML 2.0, Azure AD OAuth2 support, and Role-Based Access Control
"""

# SSO imports
try:
    from .sso import (
        sso_router,
        SSOConfig,
        SAMLConfig,
        AzureADConfig,
        get_sso_config,
        is_sso_enabled
    )
    HAS_SSO = True
except ImportError:
    HAS_SSO = False

# RBAC imports
try:
    from .rbac import (
        RBACManager,
        rbac_router,
        require_permission,
        require_role,
        get_current_user,
        check_permission,
        DEFAULT_ROLES,
        RESOURCES,
        ACTIONS,
        UserContext
    )
    HAS_RBAC = True
except ImportError:
    HAS_RBAC = False

__all__ = []

# Add SSO exports if available
if HAS_SSO:
    __all__.extend([
        'sso_router',
        'SSOConfig',
        'SAMLConfig',
        'AzureADConfig',
        'get_sso_config',
        'is_sso_enabled'
    ])

# Add RBAC exports if available
if HAS_RBAC:
    __all__.extend([
        'RBACManager',
        'rbac_router',
        'require_permission',
        'require_role',
        'get_current_user',
        'check_permission',
        'DEFAULT_ROLES',
        'RESOURCES',
        'ACTIONS',
        'UserContext',
        'HAS_RBAC'
    ])
