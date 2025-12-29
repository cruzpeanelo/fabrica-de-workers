"""
Factory Auth Module - SSO Integration
Includes SAML 2.0 and Azure AD OAuth2 support
"""
from .sso import (
    sso_router,
    SSOConfig,
    SAMLConfig,
    AzureADConfig,
    get_sso_config,
    is_sso_enabled
)

__all__ = [
    'sso_router',
    'SSOConfig',
    'SAMLConfig',
    'AzureADConfig',
    'get_sso_config',
    'is_sso_enabled'
]
