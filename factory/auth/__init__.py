"""
Factory Auth Module - Complete Authentication System
Fabrica de Agentes v6.5

Includes:
- MFA/2FA: TOTP, backup codes, recovery flow (Issue #103)
- OAuth 2.0: Authorization Code, Client Credentials, PKCE (Issue #109)
- SSO: SAML 2.0, Azure AD OAuth2 support
- RBAC: Role-Based Access Control
"""

# =============================================================================
# MFA IMPORTS (Issue #103)
# =============================================================================
try:
    from .mfa import (
        MFAService,
        MFAStatus,
        MFAMethod,
        MFASetupRequest,
        MFASetupResponse,
        MFAVerifyRequest,
        MFAVerifyResponse,
        MFAEnforcementPolicy
    )
    HAS_MFA = True
except ImportError as e:
    print(f"[Auth] MFA module not available: {e}")
    HAS_MFA = False

# =============================================================================
# OAUTH 2.0 IMPORTS (Issue #109)
# =============================================================================
try:
    from .oauth2 import (
        OAuth2Service,
        get_oauth_service,
        OAuthClient,
        OAuthScopes,
        GrantType,
        ResponseType,
        TokenType,
        ClientType,
        CodeChallengeMethod,
        TokenResponse,
        TokenIntrospectionResponse,
        oauth2_router,
        require_oauth_token,
        require_oauth_scope
    )
    HAS_OAUTH2 = True
except ImportError as e:
    print(f"[Auth] OAuth2 module not available: {e}")
    HAS_OAUTH2 = False

# =============================================================================
# SSO IMPORTS
# =============================================================================
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

# =============================================================================
# RBAC IMPORTS
# =============================================================================
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

# =============================================================================
# EXPORTS
# =============================================================================
__all__ = [
    # Feature flags
    'HAS_MFA',
    'HAS_OAUTH2',
    'HAS_SSO',
    'HAS_RBAC'
]

# Add MFA exports if available
if HAS_MFA:
    __all__.extend([
        'MFAService',
        'MFAStatus',
        'MFAMethod',
        'MFASetupRequest',
        'MFASetupResponse',
        'MFAVerifyRequest',
        'MFAVerifyResponse',
        'MFAEnforcementPolicy'
    ])

# Add OAuth 2.0 exports if available
if HAS_OAUTH2:
    __all__.extend([
        'OAuth2Service',
        'get_oauth_service',
        'OAuthClient',
        'OAuthScopes',
        'GrantType',
        'ResponseType',
        'TokenType',
        'ClientType',
        'CodeChallengeMethod',
        'TokenResponse',
        'TokenIntrospectionResponse',
        'oauth2_router',
        'require_oauth_token',
        'require_oauth_scope'
    ])

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
        'UserContext'
    ])
