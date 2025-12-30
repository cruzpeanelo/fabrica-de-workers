# -*- coding: utf-8 -*-
"""
SSO Authentication Module - SAML 2.0 and Azure AD OAuth2
=========================================================
Provides enterprise Single Sign-On integration for Fabrica de Agentes.

Features:
- SAML 2.0 authentication with configurable IdP
- Azure AD OAuth2/OpenID Connect
- Automatic user provisioning (create/update local users)
- JWT token generation for internal API usage

Environment Variables:
- SAML_IDP_URL: SAML Identity Provider SSO URL
- SAML_CERTIFICATE: SAML IdP certificate (base64 or file path)
- SAML_ENTITY_ID: Service Provider entity ID
- SAML_ACS_URL: Assertion Consumer Service URL
- AZURE_CLIENT_ID: Azure AD application client ID
- AZURE_CLIENT_SECRET: Azure AD application client secret
- AZURE_TENANT_ID: Azure AD tenant ID
- SSO_CALLBACK_URL: Base URL for SSO callbacks

Version: 1.0.0
"""
import os
import uuid
import base64
import hashlib
import secrets
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List
from urllib.parse import urlencode, quote, parse_qs, urlparse
import json
import xml.etree.ElementTree as ET
from xml.dom import minidom
import zlib

from fastapi import APIRouter, HTTPException, Request, Response, Depends, Query
from fastapi.responses import RedirectResponse, HTMLResponse, JSONResponse
from pydantic import BaseModel, Field
from dotenv import load_dotenv

load_dotenv()

# Try to import httpx for Azure AD OAuth2 calls
try:
    import httpx
    HAS_HTTPX = True
except ImportError:
    HAS_HTTPX = False
    print("[SSO] httpx not available - install with: pip install httpx")

# Import auth module for JWT generation
from factory.api.auth import create_access_token, get_password_hash, Token


# =============================================================================
# CONFIGURATION MODELS
# =============================================================================

class SAMLConfig(BaseModel):
    """SAML 2.0 Configuration"""
    idp_url: str = Field(default="", description="Identity Provider SSO URL")
    idp_entity_id: str = Field(default="", description="Identity Provider Entity ID")
    idp_certificate: str = Field(default="", description="IdP X.509 Certificate (PEM format or base64)")
    sp_entity_id: str = Field(default="", description="Service Provider Entity ID")
    sp_acs_url: str = Field(default="", description="Assertion Consumer Service URL")
    sp_slo_url: str = Field(default="", description="Single Logout URL")
    name_id_format: str = Field(
        default="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress",
        description="NameID format"
    )
    sign_requests: bool = Field(default=False, description="Sign SAML requests")
    want_assertions_signed: bool = Field(default=True, description="Require signed assertions")
    enabled: bool = Field(default=False, description="Is SAML enabled")

    @classmethod
    def from_env(cls) -> "SAMLConfig":
        """Load SAML configuration from environment variables"""
        base_url = os.getenv("SSO_CALLBACK_URL", "http://localhost:9001")
        return cls(
            idp_url=os.getenv("SAML_IDP_URL", ""),
            idp_entity_id=os.getenv("SAML_IDP_ENTITY_ID", ""),
            idp_certificate=os.getenv("SAML_CERTIFICATE", ""),
            sp_entity_id=os.getenv("SAML_ENTITY_ID", f"{base_url}/api/auth/saml/metadata"),
            sp_acs_url=os.getenv("SAML_ACS_URL", f"{base_url}/api/auth/saml/callback"),
            sp_slo_url=os.getenv("SAML_SLO_URL", f"{base_url}/api/auth/saml/logout"),
            name_id_format=os.getenv("SAML_NAME_ID_FORMAT", "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"),
            sign_requests=os.getenv("SAML_SIGN_REQUESTS", "false").lower() == "true",
            want_assertions_signed=os.getenv("SAML_WANT_ASSERTIONS_SIGNED", "true").lower() == "true",
            enabled=bool(os.getenv("SAML_IDP_URL", ""))
        )


class AzureADConfig(BaseModel):
    """Azure AD OAuth2/OpenID Connect Configuration"""
    client_id: str = Field(default="", description="Azure AD Application Client ID")
    client_secret: str = Field(default="", description="Azure AD Application Client Secret")
    tenant_id: str = Field(default="", description="Azure AD Tenant ID")
    redirect_uri: str = Field(default="", description="OAuth2 Redirect URI")
    scopes: List[str] = Field(
        default=["openid", "profile", "email", "User.Read"],
        description="OAuth2 Scopes"
    )
    authority: str = Field(default="", description="Azure AD Authority URL")
    enabled: bool = Field(default=False, description="Is Azure AD enabled")

    @classmethod
    def from_env(cls) -> "AzureADConfig":
        """Load Azure AD configuration from environment variables"""
        tenant_id = os.getenv("AZURE_TENANT_ID", "")
        base_url = os.getenv("SSO_CALLBACK_URL", "http://localhost:9001")
        return cls(
            client_id=os.getenv("AZURE_CLIENT_ID", ""),
            client_secret=os.getenv("AZURE_CLIENT_SECRET", ""),
            tenant_id=tenant_id,
            redirect_uri=os.getenv("AZURE_REDIRECT_URI", f"{base_url}/api/auth/azure/callback"),
            scopes=os.getenv("AZURE_SCOPES", "openid,profile,email,User.Read").split(","),
            authority=f"https://login.microsoftonline.com/{tenant_id}" if tenant_id else "",
            enabled=bool(os.getenv("AZURE_CLIENT_ID", "") and os.getenv("AZURE_TENANT_ID", ""))
        )


class SSOConfig(BaseModel):
    """Combined SSO Configuration"""
    saml: SAMLConfig
    azure_ad: AzureADConfig
    default_role: str = Field(default="VIEWER", description="Default role for new SSO users")
    auto_provision: bool = Field(default=True, description="Auto-create users on first SSO login")
    update_on_login: bool = Field(default=True, description="Update user info on each login")
    # Issue #148: Mapeamento de grupos do IdP para roles locais
    group_role_mapping: Dict[str, str] = Field(
        default_factory=dict,
        description="Mapping from IdP group names to local roles"
    )

    @classmethod
    def from_env(cls) -> "SSOConfig":
        """Load all SSO configuration from environment"""
        # Issue #148: Carregar mapeamento de grupos para roles
        # Formato: SSO_GROUP_MAPPING=grupo1:ADMIN,grupo2:DEVELOPER,grupo3:MANAGER
        group_mapping = {}
        mapping_str = os.getenv("SSO_GROUP_MAPPING", "")
        if mapping_str:
            for item in mapping_str.split(","):
                if ":" in item:
                    group, role = item.strip().split(":", 1)
                    group_mapping[group.strip()] = role.strip().upper()

        return cls(
            saml=SAMLConfig.from_env(),
            azure_ad=AzureADConfig.from_env(),
            default_role=os.getenv("SSO_DEFAULT_ROLE", "VIEWER"),
            auto_provision=os.getenv("SSO_AUTO_PROVISION", "true").lower() == "true",
            update_on_login=os.getenv("SSO_UPDATE_ON_LOGIN", "true").lower() == "true",
            group_role_mapping=group_mapping
        )


# =============================================================================
# SSO STATE MANAGEMENT
# =============================================================================

# In-memory state storage (use Redis in production)
_sso_states: Dict[str, Dict[str, Any]] = {}


def create_sso_state(provider: str, redirect_url: str = "/") -> str:
    """Create a state token for SSO flow"""
    state = secrets.token_urlsafe(32)
    _sso_states[state] = {
        "provider": provider,
        "redirect_url": redirect_url,
        "created_at": datetime.utcnow().isoformat(),
        "expires_at": (datetime.utcnow() + timedelta(minutes=10)).isoformat()
    }
    return state


def validate_sso_state(state: str) -> Optional[Dict[str, Any]]:
    """Validate and consume a state token"""
    if state not in _sso_states:
        return None

    state_data = _sso_states.pop(state)
    expires_at = datetime.fromisoformat(state_data["expires_at"])

    if datetime.utcnow() > expires_at:
        return None

    return state_data


def cleanup_expired_states():
    """Remove expired state tokens"""
    now = datetime.utcnow()
    expired = [
        k for k, v in _sso_states.items()
        if datetime.fromisoformat(v["expires_at"]) < now
    ]
    for k in expired:
        del _sso_states[k]


# =============================================================================
# USER PROVISIONING
# =============================================================================

# Issue #148: Role priority for group mapping (higher = more privileged)
ROLE_PRIORITY = {
    "ADMIN": 100,
    "MANAGER": 80,
    "DEVELOPER": 60,
    "VIEWER": 40,
}


def determine_role_from_groups(groups: List[str], config: "SSOConfig") -> str:
    """
    Issue #148: Determina o role baseado nos grupos do IdP.

    Args:
        groups: Lista de grupos do usuário vindos do IdP
        config: Configuração SSO com mapeamento de grupos

    Returns:
        Role mais privilegiado baseado nos grupos, ou default_role se nenhum match

    O role é determinado pela seguinte lógica:
    1. Para cada grupo do usuário, verificar se existe mapeamento
    2. Se houver múltiplos matches, usar o role mais privilegiado
    3. Se nenhum match, usar default_role
    """
    if not groups or not config.group_role_mapping:
        return config.default_role

    matched_roles = []
    for group in groups:
        # Verificar match exato
        if group in config.group_role_mapping:
            matched_roles.append(config.group_role_mapping[group])
        # Verificar match case-insensitive
        else:
            for mapping_group, role in config.group_role_mapping.items():
                if group.lower() == mapping_group.lower():
                    matched_roles.append(role)
                    break

    if not matched_roles:
        return config.default_role

    # Retornar role mais privilegiado
    return max(matched_roles, key=lambda r: ROLE_PRIORITY.get(r, 0))


def provision_sso_user(
    username: str,
    email: str,
    display_name: Optional[str] = None,
    provider: str = "sso",
    provider_id: Optional[str] = None,
    groups: Optional[List[str]] = None
) -> Dict[str, Any]:
    """
    Create or update a local user from SSO data.
    Returns user dict with JWT token.
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import User

    config = get_sso_config()
    db = SessionLocal()

    try:
        # Try to find existing user by username or email
        user = db.query(User).filter(
            (User.username == username) | (User.email == email)
        ).first()

        # Issue #148: Determinar role baseado nos grupos do IdP
        assigned_role = determine_role_from_groups(groups or [], config)

        if user:
            # Update existing user if configured
            if config.update_on_login:
                if email:
                    user.email = email
                user.last_login = datetime.utcnow()
                # Update SSO metadata in quotas field
                if user.quotas is None:
                    user.quotas = {}
                user.quotas["sso_provider"] = provider
                user.quotas["sso_provider_id"] = provider_id
                user.quotas["sso_groups"] = groups or []
                user.quotas["sso_last_login"] = datetime.utcnow().isoformat()

                # Issue #148: Atualizar role se grupos mudaram e há mapeamento
                if config.group_role_mapping and groups:
                    old_role = user.role
                    if assigned_role != old_role:
                        user.role = assigned_role
                        user.quotas["sso_role_updated_at"] = datetime.utcnow().isoformat()
                        user.quotas["sso_role_previous"] = old_role

                db.commit()
        else:
            # Create new user if auto-provision is enabled
            if not config.auto_provision:
                raise HTTPException(
                    status_code=403,
                    detail="User not found and auto-provisioning is disabled"
                )

            # Generate a random password (user will use SSO)
            random_password = secrets.token_urlsafe(32)

            user = User(
                username=username,
                email=email,
                password_hash=get_password_hash(random_password),
                role=assigned_role,  # Issue #148: Usar role baseado em grupos
                active=True,
                quotas={
                    "max_jobs_per_day": 10,
                    "max_concurrent_jobs": 2,
                    "max_projects": 20,
                    "api_tier": "free",
                    "sso_provider": provider,
                    "sso_provider_id": provider_id,
                    "sso_groups": groups or [],
                    "sso_display_name": display_name,
                    "sso_created_at": datetime.utcnow().isoformat(),
                    "sso_role_assigned_by": "group_mapping" if config.group_role_mapping else "default"
                },
                created_at=datetime.utcnow(),
                last_login=datetime.utcnow()
            )
            db.add(user)
            db.commit()
            db.refresh(user)

        # Generate JWT token
        token = create_access_token(
            data={"sub": user.username, "role": user.role}
        )
        expires_at = datetime.utcnow() + timedelta(minutes=1440)  # 24h

        return {
            "user": user.to_dict(),
            "token": Token(
                access_token=token,
                token_type="bearer",
                expires_at=expires_at.isoformat()
            ).dict()
        }

    finally:
        db.close()


# =============================================================================
# SAML 2.0 IMPLEMENTATION
# =============================================================================

def create_saml_authn_request(config: SAMLConfig, relay_state: str) -> str:
    """
    Create a SAML 2.0 AuthnRequest.
    Returns the redirect URL with encoded request.
    """
    request_id = f"_{''.join(secrets.token_hex(16))}"
    issue_instant = datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ")

    # Build AuthnRequest XML
    authn_request = f"""<?xml version="1.0" encoding="UTF-8"?>
<samlp:AuthnRequest
    xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"
    xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
    ID="{request_id}"
    Version="2.0"
    IssueInstant="{issue_instant}"
    Destination="{config.idp_url}"
    AssertionConsumerServiceURL="{config.sp_acs_url}"
    ProtocolBinding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST">
    <saml:Issuer>{config.sp_entity_id}</saml:Issuer>
    <samlp:NameIDPolicy
        Format="{config.name_id_format}"
        AllowCreate="true"/>
</samlp:AuthnRequest>"""

    # Deflate and base64 encode for HTTP-Redirect binding
    compressed = zlib.compress(authn_request.encode('utf-8'))[2:-4]  # Remove zlib header/trailer
    encoded = base64.b64encode(compressed).decode('utf-8')

    # Build redirect URL
    params = {
        "SAMLRequest": encoded,
        "RelayState": relay_state
    }

    separator = "&" if "?" in config.idp_url else "?"
    return f"{config.idp_url}{separator}{urlencode(params)}"


def parse_saml_response(saml_response_b64: str, config: SAMLConfig) -> Dict[str, Any]:
    """
    Parse and validate a SAML 2.0 Response.
    Returns extracted user attributes.

    Note: This is a simplified implementation. In production, use a library
    like python3-saml or pysaml2 for proper signature validation.
    """
    try:
        # Decode base64
        saml_response_xml = base64.b64decode(saml_response_b64).decode('utf-8')

        # Parse XML
        root = ET.fromstring(saml_response_xml)

        # Define namespaces
        ns = {
            'saml': 'urn:oasis:names:tc:SAML:2.0:assertion',
            'samlp': 'urn:oasis:names:tc:SAML:2.0:protocol'
        }

        # Check status
        status = root.find('.//samlp:StatusCode', ns)
        if status is not None:
            status_value = status.get('Value', '')
            if 'Success' not in status_value:
                raise ValueError(f"SAML Response indicates failure: {status_value}")

        # Extract assertion
        assertion = root.find('.//saml:Assertion', ns)
        if assertion is None:
            raise ValueError("No assertion found in SAML response")

        # Extract NameID (usually email or username)
        name_id_elem = assertion.find('.//saml:NameID', ns)
        name_id = name_id_elem.text if name_id_elem is not None else None

        # Extract attributes
        attributes = {}
        attr_statement = assertion.find('.//saml:AttributeStatement', ns)
        if attr_statement is not None:
            for attr in attr_statement.findall('saml:Attribute', ns):
                attr_name = attr.get('Name', '')
                attr_values = [v.text for v in attr.findall('saml:AttributeValue', ns)]
                # Clean up attribute name (remove namespace prefixes)
                short_name = attr_name.split('/')[-1] if '/' in attr_name else attr_name
                attributes[short_name] = attr_values[0] if len(attr_values) == 1 else attr_values

        # Map common attribute names
        email = (
            attributes.get('emailaddress') or
            attributes.get('email') or
            attributes.get('mail') or
            attributes.get('http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress') or
            name_id
        )

        display_name = (
            attributes.get('displayname') or
            attributes.get('name') or
            attributes.get('http://schemas.microsoft.com/identity/claims/displayname') or
            attributes.get('givenname', '')
        )

        username = (
            attributes.get('samaccountname') or
            attributes.get('upn') or
            attributes.get('http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn') or
            email.split('@')[0] if email else None
        )

        groups = attributes.get('groups', [])
        if isinstance(groups, str):
            groups = [groups]

        return {
            "name_id": name_id,
            "email": email,
            "username": username,
            "display_name": display_name,
            "groups": groups,
            "attributes": attributes
        }

    except ET.ParseError as e:
        raise ValueError(f"Failed to parse SAML response: {e}")


def generate_sp_metadata(config: SAMLConfig) -> str:
    """Generate Service Provider SAML metadata XML"""
    metadata = f"""<?xml version="1.0" encoding="UTF-8"?>
<md:EntityDescriptor
    xmlns:md="urn:oasis:names:tc:SAML:2.0:metadata"
    entityID="{config.sp_entity_id}">
    <md:SPSSODescriptor
        AuthnRequestsSigned="{str(config.sign_requests).lower()}"
        WantAssertionsSigned="{str(config.want_assertions_signed).lower()}"
        protocolSupportEnumeration="urn:oasis:names:tc:SAML:2.0:protocol">
        <md:NameIDFormat>{config.name_id_format}</md:NameIDFormat>
        <md:AssertionConsumerService
            Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST"
            Location="{config.sp_acs_url}"
            index="0"
            isDefault="true"/>
        <md:SingleLogoutService
            Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect"
            Location="{config.sp_slo_url}"/>
    </md:SPSSODescriptor>
    <md:Organization>
        <md:OrganizationName xml:lang="en">Fabrica de Agentes</md:OrganizationName>
        <md:OrganizationDisplayName xml:lang="en">Fabrica de Agentes</md:OrganizationDisplayName>
        <md:OrganizationURL xml:lang="en">https://github.com/fabricadeagentes</md:OrganizationURL>
    </md:Organization>
</md:EntityDescriptor>"""
    return metadata


# =============================================================================
# AZURE AD OAUTH2 IMPLEMENTATION
# =============================================================================

def get_azure_authorize_url(config: AzureADConfig, state: str) -> str:
    """Generate Azure AD OAuth2 authorization URL"""
    params = {
        "client_id": config.client_id,
        "response_type": "code",
        "redirect_uri": config.redirect_uri,
        "response_mode": "query",
        "scope": " ".join(config.scopes),
        "state": state,
        "prompt": "select_account"  # Always show account picker
    }

    authorize_url = f"{config.authority}/oauth2/v2.0/authorize"
    return f"{authorize_url}?{urlencode(params)}"


async def exchange_azure_code(config: AzureADConfig, code: str) -> Dict[str, Any]:
    """Exchange authorization code for tokens"""
    if not HAS_HTTPX:
        raise HTTPException(
            status_code=500,
            detail="httpx library required for Azure AD. Install with: pip install httpx"
        )

    token_url = f"{config.authority}/oauth2/v2.0/token"

    data = {
        "client_id": config.client_id,
        "client_secret": config.client_secret,
        "code": code,
        "redirect_uri": config.redirect_uri,
        "grant_type": "authorization_code",
        "scope": " ".join(config.scopes)
    }

    async with httpx.AsyncClient() as client:
        response = await client.post(token_url, data=data)

        if response.status_code != 200:
            error_data = response.json()
            raise HTTPException(
                status_code=400,
                detail=f"Azure AD token error: {error_data.get('error_description', 'Unknown error')}"
            )

        return response.json()


async def get_azure_user_info(access_token: str) -> Dict[str, Any]:
    """Get user info from Microsoft Graph API"""
    if not HAS_HTTPX:
        raise HTTPException(
            status_code=500,
            detail="httpx library required for Azure AD"
        )

    async with httpx.AsyncClient() as client:
        response = await client.get(
            "https://graph.microsoft.com/v1.0/me",
            headers={"Authorization": f"Bearer {access_token}"}
        )

        if response.status_code != 200:
            raise HTTPException(
                status_code=400,
                detail="Failed to get user info from Microsoft Graph"
            )

        user_data = response.json()

        # Also get user's group memberships
        groups = []
        try:
            groups_response = await client.get(
                "https://graph.microsoft.com/v1.0/me/memberOf",
                headers={"Authorization": f"Bearer {access_token}"}
            )
            if groups_response.status_code == 200:
                groups_data = groups_response.json()
                groups = [
                    g.get("displayName", g.get("id"))
                    for g in groups_data.get("value", [])
                    if g.get("@odata.type") == "#microsoft.graph.group"
                ]
        except Exception:
            pass  # Groups are optional

        return {
            "id": user_data.get("id"),
            "email": user_data.get("mail") or user_data.get("userPrincipalName"),
            "username": user_data.get("userPrincipalName", "").split("@")[0],
            "display_name": user_data.get("displayName"),
            "given_name": user_data.get("givenName"),
            "surname": user_data.get("surname"),
            "job_title": user_data.get("jobTitle"),
            "groups": groups
        }


# =============================================================================
# GLOBAL CONFIGURATION
# =============================================================================

_sso_config: Optional[SSOConfig] = None


def get_sso_config() -> SSOConfig:
    """Get SSO configuration (singleton)"""
    global _sso_config
    if _sso_config is None:
        _sso_config = SSOConfig.from_env()
    return _sso_config


def reload_sso_config():
    """Reload SSO configuration from environment"""
    global _sso_config
    _sso_config = SSOConfig.from_env()
    return _sso_config


def is_sso_enabled() -> bool:
    """Check if any SSO provider is enabled"""
    config = get_sso_config()
    return config.saml.enabled or config.azure_ad.enabled


# =============================================================================
# API ROUTER
# =============================================================================

sso_router = APIRouter(prefix="/api/auth", tags=["SSO Authentication"])


# -----------------------------------------------------------------------------
# SSO Status & Configuration
# -----------------------------------------------------------------------------

@sso_router.get("/sso/status")
async def sso_status():
    """Get SSO providers status"""
    config = get_sso_config()
    return {
        "sso_enabled": is_sso_enabled(),
        "providers": {
            "saml": {
                "enabled": config.saml.enabled,
                "idp_url": config.saml.idp_url if config.saml.enabled else None
            },
            "azure_ad": {
                "enabled": config.azure_ad.enabled,
                "tenant_id": config.azure_ad.tenant_id if config.azure_ad.enabled else None
            }
        }
    }


@sso_router.get("/sso/config")
async def get_sso_configuration():
    """Get SSO configuration (admin only, excludes secrets)"""
    config = get_sso_config()
    return {
        "saml": {
            "enabled": config.saml.enabled,
            "idp_url": config.saml.idp_url,
            "idp_entity_id": config.saml.idp_entity_id,
            "sp_entity_id": config.saml.sp_entity_id,
            "sp_acs_url": config.saml.sp_acs_url,
            "name_id_format": config.saml.name_id_format
        },
        "azure_ad": {
            "enabled": config.azure_ad.enabled,
            "client_id": config.azure_ad.client_id,
            "tenant_id": config.azure_ad.tenant_id,
            "redirect_uri": config.azure_ad.redirect_uri,
            "scopes": config.azure_ad.scopes
        },
        "default_role": config.default_role,
        "auto_provision": config.auto_provision,
        "update_on_login": config.update_on_login
    }


# -----------------------------------------------------------------------------
# SAML 2.0 Endpoints
# -----------------------------------------------------------------------------

@sso_router.get("/saml/login")
async def saml_login(
    redirect_url: str = Query("/", description="URL to redirect after login")
):
    """
    Initiate SAML 2.0 authentication.
    Redirects user to the Identity Provider.
    """
    config = get_sso_config().saml

    if not config.enabled:
        raise HTTPException(
            status_code=400,
            detail="SAML authentication is not configured"
        )

    # Create state for CSRF protection
    state = create_sso_state("saml", redirect_url)

    # Generate AuthnRequest and redirect URL
    redirect_to = create_saml_authn_request(config, state)

    return RedirectResponse(url=redirect_to, status_code=302)


@sso_router.post("/saml/callback")
async def saml_callback(
    request: Request
):
    """
    SAML 2.0 Assertion Consumer Service (ACS).
    Receives and processes SAML Response from IdP.
    """
    config = get_sso_config().saml

    if not config.enabled:
        raise HTTPException(
            status_code=400,
            detail="SAML authentication is not configured"
        )

    # Get form data
    form = await request.form()
    saml_response = form.get("SAMLResponse")
    relay_state = form.get("RelayState", "")

    if not saml_response:
        raise HTTPException(
            status_code=400,
            detail="Missing SAMLResponse"
        )

    # Validate state
    state_data = validate_sso_state(relay_state)
    if not state_data:
        raise HTTPException(
            status_code=400,
            detail="Invalid or expired state"
        )

    try:
        # Parse SAML response
        user_attrs = parse_saml_response(saml_response, config)

        if not user_attrs.get("email") and not user_attrs.get("username"):
            raise HTTPException(
                status_code=400,
                detail="SAML response missing required user attributes"
            )

        # Provision/update local user
        result = provision_sso_user(
            username=user_attrs["username"] or user_attrs["email"].split("@")[0],
            email=user_attrs["email"],
            display_name=user_attrs.get("display_name"),
            provider="saml",
            provider_id=user_attrs.get("name_id"),
            groups=user_attrs.get("groups", [])
        )

        # Redirect with token
        redirect_url = state_data.get("redirect_url", "/")
        token = result["token"]["access_token"]

        # Return HTML page that stores token and redirects
        return HTMLResponse(content=f"""
<!DOCTYPE html>
<html>
<head>
    <title>SSO Login Successful</title>
    <script>
        localStorage.setItem('auth_token', '{token}');
        localStorage.setItem('auth_expires', '{result["token"]["expires_at"]}');
        localStorage.setItem('auth_user', JSON.stringify({json.dumps(result["user"])}));
        window.location.href = '{redirect_url}';
    </script>
</head>
<body>
    <p>Login successful. Redirecting...</p>
</body>
</html>
""")

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@sso_router.get("/saml/metadata", response_class=Response)
async def saml_metadata():
    """
    Service Provider SAML metadata.
    Provide this URL to your Identity Provider for configuration.
    """
    config = get_sso_config().saml
    metadata = generate_sp_metadata(config)

    return Response(
        content=metadata,
        media_type="application/xml",
        headers={
            "Content-Disposition": "attachment; filename=sp-metadata.xml"
        }
    )


@sso_router.get("/saml/logout")
async def saml_logout():
    """SAML Single Logout (SLO) endpoint"""
    # Clear local session and redirect to home
    return HTMLResponse(content="""
<!DOCTYPE html>
<html>
<head>
    <title>Logout</title>
    <script>
        localStorage.removeItem('auth_token');
        localStorage.removeItem('auth_expires');
        localStorage.removeItem('auth_user');
        window.location.href = '/';
    </script>
</head>
<body>
    <p>Logging out...</p>
</body>
</html>
""")


# -----------------------------------------------------------------------------
# Azure AD OAuth2 Endpoints
# -----------------------------------------------------------------------------

@sso_router.get("/azure/login")
async def azure_login(
    redirect_url: str = Query("/", description="URL to redirect after login")
):
    """
    Initiate Azure AD OAuth2 authentication.
    Redirects user to Microsoft login.
    """
    config = get_sso_config().azure_ad

    if not config.enabled:
        raise HTTPException(
            status_code=400,
            detail="Azure AD authentication is not configured"
        )

    # Create state for CSRF protection
    state = create_sso_state("azure_ad", redirect_url)

    # Generate authorization URL
    auth_url = get_azure_authorize_url(config, state)

    return RedirectResponse(url=auth_url, status_code=302)


@sso_router.get("/azure/callback")
async def azure_callback(
    code: str = Query(None, description="Authorization code"),
    state: str = Query(None, description="State parameter"),
    error: str = Query(None, description="Error code"),
    error_description: str = Query(None, description="Error description")
):
    """
    Azure AD OAuth2 callback.
    Exchanges authorization code for tokens and user info.
    """
    config = get_sso_config().azure_ad

    if not config.enabled:
        raise HTTPException(
            status_code=400,
            detail="Azure AD authentication is not configured"
        )

    # Check for errors
    if error:
        raise HTTPException(
            status_code=400,
            detail=f"Azure AD error: {error} - {error_description}"
        )

    if not code or not state:
        raise HTTPException(
            status_code=400,
            detail="Missing authorization code or state"
        )

    # Validate state
    state_data = validate_sso_state(state)
    if not state_data:
        raise HTTPException(
            status_code=400,
            detail="Invalid or expired state"
        )

    try:
        # Exchange code for tokens
        tokens = await exchange_azure_code(config, code)
        access_token = tokens.get("access_token")

        if not access_token:
            raise HTTPException(
                status_code=400,
                detail="Failed to obtain access token"
            )

        # Get user info from Microsoft Graph
        user_info = await get_azure_user_info(access_token)

        if not user_info.get("email") and not user_info.get("username"):
            raise HTTPException(
                status_code=400,
                detail="Failed to get user email from Azure AD"
            )

        # Provision/update local user
        result = provision_sso_user(
            username=user_info["username"] or user_info["email"].split("@")[0],
            email=user_info["email"],
            display_name=user_info.get("display_name"),
            provider="azure_ad",
            provider_id=user_info.get("id"),
            groups=user_info.get("groups", [])
        )

        # Redirect with token
        redirect_url = state_data.get("redirect_url", "/")
        token = result["token"]["access_token"]

        # Return HTML page that stores token and redirects
        return HTMLResponse(content=f"""
<!DOCTYPE html>
<html>
<head>
    <title>SSO Login Successful</title>
    <script>
        localStorage.setItem('auth_token', '{token}');
        localStorage.setItem('auth_expires', '{result["token"]["expires_at"]}');
        localStorage.setItem('auth_user', JSON.stringify({json.dumps(result["user"])}));
        window.location.href = '{redirect_url}';
    </script>
</head>
<body>
    <p>Login successful. Redirecting...</p>
</body>
</html>
""")

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Authentication failed: {str(e)}"
        )


@sso_router.get("/azure/logout")
async def azure_logout():
    """Azure AD logout - clears local session and optionally signs out of Azure"""
    config = get_sso_config().azure_ad

    # Azure AD logout URL (optional - signs out of Microsoft account)
    azure_logout_url = ""
    if config.enabled:
        post_logout_uri = quote(os.getenv("SSO_CALLBACK_URL", "http://localhost:9001"))
        azure_logout_url = f"{config.authority}/oauth2/v2.0/logout?post_logout_redirect_uri={post_logout_uri}"

    return HTMLResponse(content=f"""
<!DOCTYPE html>
<html>
<head>
    <title>Logout</title>
    <script>
        localStorage.removeItem('auth_token');
        localStorage.removeItem('auth_expires');
        localStorage.removeItem('auth_user');
        {"window.location.href = '" + azure_logout_url + "';" if azure_logout_url else "window.location.href = '/';"}
    </script>
</head>
<body>
    <p>Logging out...</p>
</body>
</html>
""")


# -----------------------------------------------------------------------------
# SSO Configuration Admin Endpoints
# -----------------------------------------------------------------------------

class SSOConfigUpdate(BaseModel):
    """SSO Configuration Update Request"""
    saml_idp_url: Optional[str] = None
    saml_idp_entity_id: Optional[str] = None
    saml_certificate: Optional[str] = None
    azure_client_id: Optional[str] = None
    azure_client_secret: Optional[str] = None
    azure_tenant_id: Optional[str] = None
    default_role: Optional[str] = None
    auto_provision: Optional[bool] = None


@sso_router.post("/sso/test-connection")
async def test_sso_connection(provider: str = Query(..., description="Provider to test: saml or azure_ad")):
    """Test SSO provider connection (admin only)"""
    config = get_sso_config()

    if provider == "saml":
        if not config.saml.enabled:
            return {"success": False, "error": "SAML is not configured"}

        # For SAML, we can only verify configuration exists
        return {
            "success": True,
            "provider": "saml",
            "idp_url": config.saml.idp_url,
            "sp_entity_id": config.saml.sp_entity_id,
            "message": "SAML configuration found. Test login to verify."
        }

    elif provider == "azure_ad":
        if not config.azure_ad.enabled:
            return {"success": False, "error": "Azure AD is not configured"}

        if not HAS_HTTPX:
            return {"success": False, "error": "httpx library required"}

        # Test Azure AD by fetching OpenID configuration
        try:
            async with httpx.AsyncClient() as client:
                openid_url = f"{config.azure_ad.authority}/v2.0/.well-known/openid-configuration"
                response = await client.get(openid_url)

                if response.status_code == 200:
                    return {
                        "success": True,
                        "provider": "azure_ad",
                        "tenant_id": config.azure_ad.tenant_id,
                        "message": "Azure AD connection successful"
                    }
                else:
                    return {
                        "success": False,
                        "error": f"Azure AD returned status {response.status_code}"
                    }
        except Exception as e:
            return {"success": False, "error": str(e)}

    else:
        raise HTTPException(status_code=400, detail=f"Unknown provider: {provider}")


# -----------------------------------------------------------------------------
# SSO Login Page (Standalone)
# -----------------------------------------------------------------------------

@sso_router.get("/sso/login-page", response_class=HTMLResponse)
async def sso_login_page(redirect_url: str = Query("/", description="URL to redirect after login")):
    """
    Standalone SSO login page with buttons for all configured providers.
    """
    config = get_sso_config()

    azure_enabled = config.azure_ad.enabled
    saml_enabled = config.saml.enabled

    return HTMLResponse(content=f"""
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Login SSO - Fabrica de Agentes</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <style>
        .belgo-blue {{ background-color: #003B4A; }}
        .belgo-orange {{ background-color: #FF6C00; }}
    </style>
</head>
<body class="bg-gray-100 min-h-screen flex items-center justify-center">
    <div class="w-full max-w-md">
        <!-- Logo/Header -->
        <div class="text-center mb-8">
            <div class="belgo-blue text-white w-16 h-16 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg class="w-8 h-8" fill="currentColor" viewBox="0 0 24 24">
                    <path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/>
                </svg>
            </div>
            <h1 class="text-2xl font-bold text-gray-800">Fabrica de Agentes</h1>
            <p class="text-gray-500 mt-1">Autenticacao Single Sign-On</p>
        </div>

        <!-- Login Card -->
        <div class="bg-white rounded-lg shadow-lg p-8">
            <h2 class="text-lg font-semibold text-gray-700 mb-6 text-center">Escolha seu provedor</h2>

            <div class="space-y-4">
                {'<a href="/api/auth/azure/login?redirect_url=' + redirect_url + '" class="w-full flex items-center justify-center gap-3 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"><svg class="w-6 h-6" viewBox="0 0 21 21" fill="none"><rect width="10" height="10" fill="#F25022"/><rect x="11" width="10" height="10" fill="#7FBA00"/><rect y="11" width="10" height="10" fill="#00A4EF"/><rect x="11" y="11" width="10" height="10" fill="#FFB900"/></svg><span class="font-medium text-gray-700">Login com Microsoft Azure</span></a>' if azure_enabled else ''}

                {'<a href="/api/auth/saml/login?redirect_url=' + redirect_url + '" class="w-full flex items-center justify-center gap-3 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"><svg class="w-6 h-6 text-[#003B4A]" fill="currentColor" viewBox="0 0 24 24"><path d="M12 1L3 5v6c0 5.55 3.84 10.74 9 12 5.16-1.26 9-6.45 9-12V5l-9-4z"/></svg><span class="font-medium text-gray-700">Login com SSO Corporativo (SAML)</span></a>' if saml_enabled else ''}

                {'<div class="text-center py-8 text-gray-400"><svg class="w-12 h-12 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/></svg><p>Nenhum provedor SSO configurado</p></div>' if not azure_enabled and not saml_enabled else ''}
            </div>

            <div class="relative my-6">
                <div class="absolute inset-0 flex items-center">
                    <div class="w-full border-t border-gray-300"></div>
                </div>
                <div class="relative flex justify-center text-sm">
                    <span class="px-2 bg-white text-gray-500">ou</span>
                </div>
            </div>

            <a href="/" class="block w-full text-center py-2 text-gray-500 hover:text-gray-700 text-sm">
                Voltar para o Dashboard
            </a>
        </div>

        <!-- Footer -->
        <p class="text-center text-gray-400 text-xs mt-8">
            Fabrica de Agentes v6.0 - Sistema Agile
        </p>
    </div>
</body>
</html>
""")
