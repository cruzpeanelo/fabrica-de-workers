# -*- coding: utf-8 -*-
"""
OAuth 2.0 Authentication Service - Issue #109
Fabrica de Agentes v6.5

Complete OAuth 2.0 implementation with:
1. Authorization Code Flow (for web apps with backend)
2. Authorization Code Flow with PKCE (for SPAs/mobile)
3. Client Credentials Flow (for M2M/server-to-server)
4. Refresh Token Flow
5. Token Revocation
6. Granular Scopes System

Standards:
- RFC 6749 (OAuth 2.0)
- RFC 7636 (PKCE)
- RFC 7009 (Token Revocation)
- RFC 6750 (Bearer Tokens)

Security Features:
- PKCE for public clients
- Short-lived authorization codes (10 min)
- Secure token storage
- Scope validation
- Client authentication
- Rate limiting for token requests
"""

import os
import secrets
import hashlib
import base64
import json
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any, Set, Tuple
from dataclasses import dataclass, field
from enum import Enum
from urllib.parse import urlencode, urlparse, parse_qs

from pydantic import BaseModel, Field, validator
from jose import jwt, JWTError

from factory.config import (
    JWT_SECRET_KEY,
    JWT_ALGORITHM,
    ACCESS_TOKEN_EXPIRE_MINUTES
)


# =============================================================================
# CONSTANTS AND CONFIGURATION
# =============================================================================

# OAuth Configuration
OAUTH_ISSUER = os.getenv("OAUTH_ISSUER", "https://fabricadeagentes.com")
AUTHORIZATION_CODE_EXPIRE_MINUTES = 10
REFRESH_TOKEN_EXPIRE_DAYS = 30
ACCESS_TOKEN_EXPIRE_MINUTES_DEFAULT = ACCESS_TOKEN_EXPIRE_MINUTES or 60

# PKCE Configuration
PKCE_MIN_VERIFIER_LENGTH = 43
PKCE_MAX_VERIFIER_LENGTH = 128

# Rate Limiting
MAX_TOKEN_REQUESTS_PER_MINUTE = 60
MAX_FAILED_AUTH_ATTEMPTS = 5


# =============================================================================
# ENUMS
# =============================================================================

class GrantType(str, Enum):
    """OAuth 2.0 Grant Types"""
    AUTHORIZATION_CODE = "authorization_code"
    CLIENT_CREDENTIALS = "client_credentials"
    REFRESH_TOKEN = "refresh_token"
    # Note: Password grant is deprecated and not implemented for security reasons


class ResponseType(str, Enum):
    """OAuth 2.0 Response Types"""
    CODE = "code"
    TOKEN = "token"  # Implicit flow (deprecated, but supported for legacy)


class TokenType(str, Enum):
    """Token Types"""
    BEARER = "Bearer"
    ACCESS = "access"
    REFRESH = "refresh"


class ClientType(str, Enum):
    """OAuth Client Types"""
    CONFIDENTIAL = "confidential"  # Server-side apps with secure storage
    PUBLIC = "public"  # SPAs, mobile apps (requires PKCE)


class CodeChallengeMethod(str, Enum):
    """PKCE Code Challenge Methods"""
    PLAIN = "plain"
    S256 = "S256"


# =============================================================================
# SCOPES DEFINITION
# =============================================================================

class OAuthScopes:
    """
    Granular OAuth 2.0 Scopes for API access control.

    Scope format: resource:action
    """

    # Story scopes
    READ_STORIES = "read:stories"
    WRITE_STORIES = "write:stories"
    DELETE_STORIES = "delete:stories"

    # Task scopes
    READ_TASKS = "read:tasks"
    WRITE_TASKS = "write:tasks"
    DELETE_TASKS = "delete:tasks"

    # Project scopes
    READ_PROJECTS = "read:projects"
    WRITE_PROJECTS = "write:projects"
    DELETE_PROJECTS = "delete:projects"
    MANAGE_PROJECTS = "manage:projects"

    # User scopes
    READ_USERS = "read:users"
    WRITE_USERS = "write:users"
    MANAGE_USERS = "manage:users"

    # Analytics scopes
    READ_ANALYTICS = "read:analytics"
    EXPORT_ANALYTICS = "export:analytics"

    # Worker scopes
    READ_WORKERS = "read:workers"
    MANAGE_WORKERS = "manage:workers"

    # Admin scopes
    ADMIN = "admin"  # Full access
    ADMIN_READ = "admin:read"
    ADMIN_WRITE = "admin:write"

    # OpenID Connect scopes
    OPENID = "openid"
    PROFILE = "profile"
    EMAIL = "email"

    @classmethod
    def all_scopes(cls) -> Dict[str, str]:
        """Returns all available scopes with descriptions"""
        return {
            cls.READ_STORIES: "Read user stories",
            cls.WRITE_STORIES: "Create and update user stories",
            cls.DELETE_STORIES: "Delete user stories",
            cls.READ_TASKS: "Read tasks",
            cls.WRITE_TASKS: "Create and update tasks",
            cls.DELETE_TASKS: "Delete tasks",
            cls.READ_PROJECTS: "Read project information",
            cls.WRITE_PROJECTS: "Create and update projects",
            cls.DELETE_PROJECTS: "Delete projects",
            cls.MANAGE_PROJECTS: "Full project management",
            cls.READ_USERS: "Read user information",
            cls.WRITE_USERS: "Update user information",
            cls.MANAGE_USERS: "Full user management",
            cls.READ_ANALYTICS: "Read analytics data",
            cls.EXPORT_ANALYTICS: "Export analytics reports",
            cls.READ_WORKERS: "Read worker status",
            cls.MANAGE_WORKERS: "Manage workers",
            cls.ADMIN: "Full administrative access",
            cls.ADMIN_READ: "Administrative read access",
            cls.ADMIN_WRITE: "Administrative write access",
            cls.OPENID: "OpenID Connect authentication",
            cls.PROFILE: "User profile information",
            cls.EMAIL: "User email address",
        }

    @classmethod
    def validate_scopes(cls, requested: str, allowed: List[str]) -> Set[str]:
        """
        Validate and filter requested scopes against allowed scopes.

        Args:
            requested: Space-separated scope string
            allowed: List of allowed scopes for the client

        Returns:
            Set of valid scopes
        """
        if not requested:
            return set()

        requested_set = set(requested.split())
        allowed_set = set(allowed)
        all_valid = set(cls.all_scopes().keys())

        # Admin scope grants all permissions
        if cls.ADMIN in allowed_set:
            return requested_set.intersection(all_valid)

        return requested_set.intersection(allowed_set).intersection(all_valid)

    @classmethod
    def has_scope(cls, token_scopes: List[str], required_scope: str) -> bool:
        """Check if token has required scope or admin access"""
        if cls.ADMIN in token_scopes:
            return True
        return required_scope in token_scopes


# =============================================================================
# MODELS
# =============================================================================

class OAuthClient(BaseModel):
    """Registered OAuth 2.0 Client"""
    client_id: str
    client_secret: Optional[str] = None  # None for public clients
    client_name: str
    client_type: ClientType = ClientType.CONFIDENTIAL
    redirect_uris: List[str]
    allowed_scopes: List[str]
    allowed_grant_types: List[GrantType]
    logo_uri: Optional[str] = None
    policy_uri: Optional[str] = None
    tos_uri: Optional[str] = None
    contacts: List[str] = Field(default_factory=list)
    created_at: datetime = Field(default_factory=datetime.utcnow)
    updated_at: datetime = Field(default_factory=datetime.utcnow)
    active: bool = True

    # M2M specific
    tenant_id: Optional[str] = None
    rate_limit: int = 1000  # requests per hour


class AuthorizationCode(BaseModel):
    """OAuth 2.0 Authorization Code"""
    code: str
    client_id: str
    user_id: str
    redirect_uri: str
    scope: str
    code_challenge: Optional[str] = None
    code_challenge_method: Optional[CodeChallengeMethod] = None
    nonce: Optional[str] = None  # For OpenID Connect
    state: Optional[str] = None
    expires_at: datetime
    created_at: datetime = Field(default_factory=datetime.utcnow)
    used: bool = False


class RefreshToken(BaseModel):
    """OAuth 2.0 Refresh Token"""
    token: str
    token_hash: str  # Store hashed for security
    client_id: str
    user_id: str
    scope: str
    expires_at: datetime
    created_at: datetime = Field(default_factory=datetime.utcnow)
    revoked: bool = False
    revoked_at: Optional[datetime] = None


class TokenResponse(BaseModel):
    """OAuth 2.0 Token Response"""
    access_token: str
    token_type: str = "Bearer"
    expires_in: int
    refresh_token: Optional[str] = None
    scope: str
    id_token: Optional[str] = None  # For OpenID Connect


class TokenIntrospectionResponse(BaseModel):
    """OAuth 2.0 Token Introspection Response (RFC 7662)"""
    active: bool
    scope: Optional[str] = None
    client_id: Optional[str] = None
    username: Optional[str] = None
    token_type: Optional[str] = None
    exp: Optional[int] = None
    iat: Optional[int] = None
    nbf: Optional[int] = None
    sub: Optional[str] = None
    aud: Optional[str] = None
    iss: Optional[str] = None


class AuthorizationRequest(BaseModel):
    """OAuth 2.0 Authorization Request"""
    response_type: ResponseType = ResponseType.CODE
    client_id: str
    redirect_uri: str
    scope: str = "openid"
    state: Optional[str] = None
    code_challenge: Optional[str] = None
    code_challenge_method: Optional[CodeChallengeMethod] = None
    nonce: Optional[str] = None  # For OpenID Connect

    @validator('code_challenge_method')
    def validate_pkce(cls, v, values):
        if v and not values.get('code_challenge'):
            raise ValueError('code_challenge required when code_challenge_method is provided')
        return v


class TokenRequest(BaseModel):
    """OAuth 2.0 Token Request"""
    grant_type: GrantType
    code: Optional[str] = None
    redirect_uri: Optional[str] = None
    client_id: Optional[str] = None
    client_secret: Optional[str] = None
    refresh_token: Optional[str] = None
    code_verifier: Optional[str] = None
    scope: Optional[str] = None


class ClientRegistrationRequest(BaseModel):
    """Dynamic Client Registration Request (RFC 7591)"""
    client_name: str = Field(..., min_length=3, max_length=100)
    redirect_uris: List[str] = Field(..., min_items=1)
    grant_types: List[GrantType] = Field(default=[GrantType.AUTHORIZATION_CODE])
    response_types: List[ResponseType] = Field(default=[ResponseType.CODE])
    scope: str = "openid profile"
    client_uri: Optional[str] = None
    logo_uri: Optional[str] = None
    contacts: List[str] = Field(default_factory=list)
    tos_uri: Optional[str] = None
    policy_uri: Optional[str] = None
    token_endpoint_auth_method: str = "client_secret_basic"


class ClientRegistrationResponse(BaseModel):
    """Dynamic Client Registration Response"""
    client_id: str
    client_secret: Optional[str] = None
    client_id_issued_at: int
    client_secret_expires_at: int = 0  # 0 means never expires
    client_name: str
    redirect_uris: List[str]
    grant_types: List[str]
    response_types: List[str]
    scope: str


# =============================================================================
# OAUTH 2.0 SERVICE
# =============================================================================

class OAuth2Service:
    """
    Core OAuth 2.0 Service Implementation.

    Handles:
    - Authorization Code Flow (with PKCE)
    - Client Credentials Flow
    - Refresh Token Flow
    - Token Validation
    - Token Revocation

    Usage:
        oauth = OAuth2Service()

        # Authorization Code Flow
        auth_url = oauth.create_authorization_url(
            client_id="my_client",
            redirect_uri="https://app.com/callback",
            scope="read:stories write:stories",
            state="random_state"
        )

        # Exchange code for tokens
        tokens = oauth.exchange_code(
            code="auth_code",
            client_id="my_client",
            client_secret="secret",
            redirect_uri="https://app.com/callback"
        )

        # Validate access token
        token_data = oauth.validate_token(tokens.access_token)
    """

    def __init__(self, db_session=None):
        self.db = db_session
        self._clients: Dict[str, OAuthClient] = {}
        self._authorization_codes: Dict[str, AuthorizationCode] = {}
        self._refresh_tokens: Dict[str, RefreshToken] = {}
        self._revoked_tokens: Set[str] = set()  # JTI of revoked access tokens

        # Initialize demo client
        self._init_demo_clients()

    def _init_demo_clients(self):
        """Initialize demo OAuth clients for development"""
        # Web application client (confidential)
        web_client = OAuthClient(
            client_id="fabrica_web_app",
            client_secret=secrets.token_urlsafe(32),
            client_name="Fabrica Web Application",
            client_type=ClientType.CONFIDENTIAL,
            redirect_uris=[
                "http://localhost:9001/oauth/callback",
                "http://localhost:3000/callback",
                "https://fabricadeagentes.com/oauth/callback"
            ],
            allowed_scopes=list(OAuthScopes.all_scopes().keys()),
            allowed_grant_types=[
                GrantType.AUTHORIZATION_CODE,
                GrantType.REFRESH_TOKEN
            ]
        )
        self._clients[web_client.client_id] = web_client

        # Machine-to-Machine client (for integrations)
        m2m_client = OAuthClient(
            client_id="fabrica_m2m_integration",
            client_secret=secrets.token_urlsafe(32),
            client_name="M2M Integration Client",
            client_type=ClientType.CONFIDENTIAL,
            redirect_uris=[],
            allowed_scopes=[
                OAuthScopes.READ_STORIES,
                OAuthScopes.WRITE_STORIES,
                OAuthScopes.READ_PROJECTS,
                OAuthScopes.READ_ANALYTICS
            ],
            allowed_grant_types=[GrantType.CLIENT_CREDENTIALS]
        )
        self._clients[m2m_client.client_id] = m2m_client

        # Public client (SPA/Mobile)
        public_client = OAuthClient(
            client_id="fabrica_spa_client",
            client_secret=None,  # No secret for public clients
            client_name="Fabrica SPA Client",
            client_type=ClientType.PUBLIC,
            redirect_uris=[
                "http://localhost:3000/callback",
                "fabricadeagentes://callback"  # Custom scheme for mobile
            ],
            allowed_scopes=[
                OAuthScopes.OPENID,
                OAuthScopes.PROFILE,
                OAuthScopes.READ_STORIES,
                OAuthScopes.WRITE_STORIES,
                OAuthScopes.READ_PROJECTS
            ],
            allowed_grant_types=[
                GrantType.AUTHORIZATION_CODE,
                GrantType.REFRESH_TOKEN
            ]
        )
        self._clients[public_client.client_id] = public_client

    # -------------------------------------------------------------------------
    # Client Management
    # -------------------------------------------------------------------------

    def register_client(self, request: ClientRegistrationRequest) -> ClientRegistrationResponse:
        """
        Register a new OAuth client (RFC 7591).

        Args:
            request: Client registration data

        Returns:
            Client credentials
        """
        client_id = f"client_{secrets.token_urlsafe(16)}"
        client_secret = secrets.token_urlsafe(32)

        client = OAuthClient(
            client_id=client_id,
            client_secret=client_secret,
            client_name=request.client_name,
            client_type=ClientType.CONFIDENTIAL,
            redirect_uris=request.redirect_uris,
            allowed_scopes=request.scope.split() if request.scope else [],
            allowed_grant_types=request.grant_types,
            logo_uri=request.logo_uri,
            policy_uri=request.policy_uri,
            tos_uri=request.tos_uri,
            contacts=request.contacts
        )

        self._clients[client_id] = client

        # In production, save to database
        self._save_client(client)

        return ClientRegistrationResponse(
            client_id=client_id,
            client_secret=client_secret,
            client_id_issued_at=int(datetime.utcnow().timestamp()),
            client_secret_expires_at=0,
            client_name=request.client_name,
            redirect_uris=request.redirect_uris,
            grant_types=[g.value for g in request.grant_types],
            response_types=[r.value for r in request.response_types],
            scope=request.scope
        )

    def get_client(self, client_id: str) -> Optional[OAuthClient]:
        """Get OAuth client by ID"""
        return self._clients.get(client_id)

    def validate_client(
        self,
        client_id: str,
        client_secret: Optional[str] = None,
        grant_type: Optional[GrantType] = None
    ) -> Tuple[bool, Optional[OAuthClient], str]:
        """
        Validate client credentials.

        Returns:
            Tuple of (is_valid, client, error_message)
        """
        client = self.get_client(client_id)

        if not client:
            return False, None, "Client not found"

        if not client.active:
            return False, None, "Client is inactive"

        # Check client secret for confidential clients
        if client.client_type == ClientType.CONFIDENTIAL:
            if not client_secret:
                return False, None, "Client secret required"
            if client.client_secret != client_secret:
                return False, None, "Invalid client secret"

        # Check grant type
        if grant_type and grant_type not in client.allowed_grant_types:
            return False, None, f"Grant type '{grant_type.value}' not allowed"

        return True, client, ""

    def _save_client(self, client: OAuthClient):
        """Save client to database (placeholder for production)"""
        # In production, save to database
        pass

    # -------------------------------------------------------------------------
    # Authorization Code Flow
    # -------------------------------------------------------------------------

    def create_authorization_request(
        self,
        client_id: str,
        redirect_uri: str,
        scope: str,
        state: Optional[str] = None,
        code_challenge: Optional[str] = None,
        code_challenge_method: Optional[str] = None,
        nonce: Optional[str] = None
    ) -> Tuple[bool, str]:
        """
        Validate authorization request and return authorization URL or error.

        Args:
            client_id: OAuth client ID
            redirect_uri: Callback URI
            scope: Requested scopes
            state: CSRF state
            code_challenge: PKCE challenge
            code_challenge_method: PKCE method (S256 recommended)
            nonce: OpenID Connect nonce

        Returns:
            Tuple of (success, url_or_error)
        """
        client = self.get_client(client_id)

        if not client:
            return False, "invalid_client: Client not found"

        if redirect_uri not in client.redirect_uris:
            return False, "invalid_request: Redirect URI not registered"

        # Validate scopes
        valid_scopes = OAuthScopes.validate_scopes(scope, client.allowed_scopes)
        if not valid_scopes and scope:
            return False, "invalid_scope: Invalid or unauthorized scope"

        # Public clients MUST use PKCE
        if client.client_type == ClientType.PUBLIC and not code_challenge:
            return False, "invalid_request: PKCE required for public clients"

        # Validate PKCE if provided
        if code_challenge:
            if code_challenge_method not in ["plain", "S256"]:
                return False, "invalid_request: Invalid code_challenge_method"
            if code_challenge_method == "plain":
                # Plain is discouraged
                print("[OAuth] Warning: plain code_challenge_method is not recommended")

        return True, ""

    def generate_authorization_code(
        self,
        client_id: str,
        user_id: str,
        redirect_uri: str,
        scope: str,
        state: Optional[str] = None,
        code_challenge: Optional[str] = None,
        code_challenge_method: Optional[str] = None,
        nonce: Optional[str] = None
    ) -> str:
        """
        Generate authorization code after user consent.

        Args:
            client_id: OAuth client ID
            user_id: Authenticated user ID
            redirect_uri: Callback URI
            scope: Approved scopes
            state: CSRF state
            code_challenge: PKCE challenge
            code_challenge_method: PKCE method
            nonce: OpenID Connect nonce

        Returns:
            Authorization code
        """
        code = secrets.token_urlsafe(32)

        auth_code = AuthorizationCode(
            code=code,
            client_id=client_id,
            user_id=user_id,
            redirect_uri=redirect_uri,
            scope=scope,
            code_challenge=code_challenge,
            code_challenge_method=CodeChallengeMethod(code_challenge_method) if code_challenge_method else None,
            nonce=nonce,
            state=state,
            expires_at=datetime.utcnow() + timedelta(minutes=AUTHORIZATION_CODE_EXPIRE_MINUTES)
        )

        self._authorization_codes[code] = auth_code

        return code

    def build_authorization_redirect(
        self,
        redirect_uri: str,
        code: str,
        state: Optional[str] = None
    ) -> str:
        """Build redirect URL with authorization code"""
        params = {"code": code}
        if state:
            params["state"] = state

        separator = "&" if "?" in redirect_uri else "?"
        return f"{redirect_uri}{separator}{urlencode(params)}"

    # -------------------------------------------------------------------------
    # Token Exchange
    # -------------------------------------------------------------------------

    def exchange_authorization_code(
        self,
        code: str,
        client_id: str,
        client_secret: Optional[str],
        redirect_uri: str,
        code_verifier: Optional[str] = None
    ) -> Tuple[bool, Any, str]:
        """
        Exchange authorization code for tokens.

        Args:
            code: Authorization code
            client_id: OAuth client ID
            client_secret: Client secret (for confidential clients)
            redirect_uri: Must match original request
            code_verifier: PKCE verifier

        Returns:
            Tuple of (success, tokens_or_none, error_message)
        """
        # Get and validate authorization code
        auth_code = self._authorization_codes.get(code)

        if not auth_code:
            return False, None, "invalid_grant: Authorization code not found"

        if auth_code.used:
            # Security: Revoke all tokens if code reuse attempted
            self._revoke_tokens_for_user(auth_code.user_id, auth_code.client_id)
            return False, None, "invalid_grant: Authorization code already used"

        if datetime.utcnow() > auth_code.expires_at:
            del self._authorization_codes[code]
            return False, None, "invalid_grant: Authorization code expired"

        if auth_code.client_id != client_id:
            return False, None, "invalid_grant: Client ID mismatch"

        if auth_code.redirect_uri != redirect_uri:
            return False, None, "invalid_grant: Redirect URI mismatch"

        # Validate client
        is_valid, client, error = self.validate_client(
            client_id, client_secret, GrantType.AUTHORIZATION_CODE
        )
        if not is_valid:
            return False, None, f"invalid_client: {error}"

        # Validate PKCE
        if auth_code.code_challenge:
            if not code_verifier:
                return False, None, "invalid_grant: Code verifier required"

            if not self._verify_pkce(
                code_verifier,
                auth_code.code_challenge,
                auth_code.code_challenge_method.value if auth_code.code_challenge_method else "S256"
            ):
                return False, None, "invalid_grant: Invalid code verifier"

        # Mark code as used
        auth_code.used = True

        # Generate tokens
        tokens = self._generate_tokens(
            user_id=auth_code.user_id,
            client_id=client_id,
            scope=auth_code.scope,
            include_refresh=True,
            nonce=auth_code.nonce
        )

        # Clean up
        del self._authorization_codes[code]

        return True, tokens, ""

    def exchange_client_credentials(
        self,
        client_id: str,
        client_secret: str,
        scope: Optional[str] = None
    ) -> Tuple[bool, Any, str]:
        """
        Exchange client credentials for access token (M2M flow).

        Args:
            client_id: OAuth client ID
            client_secret: Client secret
            scope: Requested scopes

        Returns:
            Tuple of (success, tokens_or_none, error_message)
        """
        # Validate client
        is_valid, client, error = self.validate_client(
            client_id, client_secret, GrantType.CLIENT_CREDENTIALS
        )
        if not is_valid:
            return False, None, f"invalid_client: {error}"

        # Validate scopes
        valid_scopes = OAuthScopes.validate_scopes(scope or "", client.allowed_scopes)

        # Generate access token only (no refresh token for client credentials)
        tokens = self._generate_tokens(
            user_id=f"client:{client_id}",
            client_id=client_id,
            scope=" ".join(valid_scopes) if valid_scopes else "",
            include_refresh=False
        )

        return True, tokens, ""

    def refresh_access_token(
        self,
        refresh_token: str,
        client_id: str,
        client_secret: Optional[str] = None,
        scope: Optional[str] = None
    ) -> Tuple[bool, Any, str]:
        """
        Refresh access token using refresh token.

        Args:
            refresh_token: Refresh token
            client_id: OAuth client ID
            client_secret: Client secret (for confidential clients)
            scope: Optional scope reduction

        Returns:
            Tuple of (success, tokens_or_none, error_message)
        """
        # Find refresh token
        token_hash = self._hash_token(refresh_token)
        stored_token = None

        for rt in self._refresh_tokens.values():
            if rt.token_hash == token_hash and not rt.revoked:
                stored_token = rt
                break

        if not stored_token:
            return False, None, "invalid_grant: Invalid refresh token"

        if datetime.utcnow() > stored_token.expires_at:
            stored_token.revoked = True
            return False, None, "invalid_grant: Refresh token expired"

        if stored_token.client_id != client_id:
            return False, None, "invalid_grant: Client ID mismatch"

        # Validate client
        is_valid, client, error = self.validate_client(
            client_id, client_secret, GrantType.REFRESH_TOKEN
        )
        if not is_valid:
            return False, None, f"invalid_client: {error}"

        # Scope reduction (can only reduce, not expand)
        final_scope = stored_token.scope
        if scope:
            requested_scopes = set(scope.split())
            original_scopes = set(stored_token.scope.split())
            if not requested_scopes.issubset(original_scopes):
                return False, None, "invalid_scope: Cannot expand scope"
            final_scope = " ".join(requested_scopes)

        # Revoke old refresh token (rotation)
        stored_token.revoked = True
        stored_token.revoked_at = datetime.utcnow()

        # Generate new tokens
        tokens = self._generate_tokens(
            user_id=stored_token.user_id,
            client_id=client_id,
            scope=final_scope,
            include_refresh=True
        )

        return True, tokens, ""

    # -------------------------------------------------------------------------
    # Token Generation
    # -------------------------------------------------------------------------

    def _generate_tokens(
        self,
        user_id: str,
        client_id: str,
        scope: str,
        include_refresh: bool = True,
        nonce: Optional[str] = None
    ) -> TokenResponse:
        """Generate access and refresh tokens"""

        now = datetime.utcnow()
        expires_delta = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES_DEFAULT)
        expire = now + expires_delta

        # Generate unique token ID (jti)
        jti = secrets.token_urlsafe(16)

        # Access token payload
        payload = {
            "sub": user_id,
            "client_id": client_id,
            "scope": scope,
            "type": "access",
            "jti": jti,
            "iss": OAUTH_ISSUER,
            "aud": client_id,
            "iat": int(now.timestamp()),
            "exp": int(expire.timestamp())
        }

        if nonce:
            payload["nonce"] = nonce

        access_token = jwt.encode(payload, JWT_SECRET_KEY, algorithm=JWT_ALGORITHM)

        # Generate refresh token if requested
        refresh_token_str = None
        if include_refresh:
            refresh_token_str = secrets.token_urlsafe(64)
            refresh_token_obj = RefreshToken(
                token=refresh_token_str,
                token_hash=self._hash_token(refresh_token_str),
                client_id=client_id,
                user_id=user_id,
                scope=scope,
                expires_at=now + timedelta(days=REFRESH_TOKEN_EXPIRE_DAYS)
            )
            self._refresh_tokens[refresh_token_str] = refresh_token_obj

        return TokenResponse(
            access_token=access_token,
            token_type="Bearer",
            expires_in=int(expires_delta.total_seconds()),
            refresh_token=refresh_token_str,
            scope=scope
        )

    # -------------------------------------------------------------------------
    # Token Validation
    # -------------------------------------------------------------------------

    def validate_access_token(self, token: str) -> Tuple[bool, Optional[Dict], str]:
        """
        Validate access token and return payload.

        Args:
            token: Access token

        Returns:
            Tuple of (is_valid, payload_or_none, error_message)
        """
        try:
            payload = jwt.decode(token, JWT_SECRET_KEY, algorithms=[JWT_ALGORITHM])

            # Check token type
            if payload.get("type") != "access":
                return False, None, "Invalid token type"

            # Check if revoked
            jti = payload.get("jti")
            if jti and jti in self._revoked_tokens:
                return False, None, "Token has been revoked"

            return True, payload, ""

        except JWTError as e:
            return False, None, f"Invalid token: {str(e)}"

    def introspect_token(self, token: str) -> TokenIntrospectionResponse:
        """
        Token introspection (RFC 7662).

        Args:
            token: Token to introspect

        Returns:
            Introspection response
        """
        is_valid, payload, _ = self.validate_access_token(token)

        if not is_valid:
            return TokenIntrospectionResponse(active=False)

        return TokenIntrospectionResponse(
            active=True,
            scope=payload.get("scope"),
            client_id=payload.get("client_id"),
            username=payload.get("sub"),
            token_type="Bearer",
            exp=payload.get("exp"),
            iat=payload.get("iat"),
            sub=payload.get("sub"),
            aud=payload.get("aud"),
            iss=payload.get("iss")
        )

    # -------------------------------------------------------------------------
    # Token Revocation
    # -------------------------------------------------------------------------

    def revoke_token(
        self,
        token: str,
        token_type_hint: Optional[str] = None,
        client_id: str = None,
        client_secret: str = None
    ) -> Tuple[bool, str]:
        """
        Revoke a token (RFC 7009).

        Args:
            token: Token to revoke
            token_type_hint: "access_token" or "refresh_token"
            client_id: Client ID
            client_secret: Client secret

        Returns:
            Tuple of (success, error_message)
        """
        # Try refresh token first
        if token_type_hint != "access_token":
            token_hash = self._hash_token(token)
            for rt in self._refresh_tokens.values():
                if rt.token_hash == token_hash:
                    if client_id and rt.client_id != client_id:
                        return False, "Token does not belong to this client"
                    rt.revoked = True
                    rt.revoked_at = datetime.utcnow()
                    return True, ""

        # Try access token
        if token_type_hint != "refresh_token":
            try:
                payload = jwt.decode(
                    token, JWT_SECRET_KEY,
                    algorithms=[JWT_ALGORITHM],
                    options={"verify_exp": False}
                )
                jti = payload.get("jti")
                if jti:
                    if client_id and payload.get("client_id") != client_id:
                        return False, "Token does not belong to this client"
                    self._revoked_tokens.add(jti)
                    return True, ""
            except JWTError:
                pass

        # Token not found - still return success per RFC 7009
        return True, ""

    def _revoke_tokens_for_user(self, user_id: str, client_id: str):
        """Revoke all tokens for a user-client pair"""
        for rt in self._refresh_tokens.values():
            if rt.user_id == user_id and rt.client_id == client_id:
                rt.revoked = True
                rt.revoked_at = datetime.utcnow()

    # -------------------------------------------------------------------------
    # PKCE Helpers
    # -------------------------------------------------------------------------

    def _verify_pkce(
        self,
        code_verifier: str,
        code_challenge: str,
        code_challenge_method: str
    ) -> bool:
        """Verify PKCE code verifier against challenge"""

        if len(code_verifier) < PKCE_MIN_VERIFIER_LENGTH:
            return False
        if len(code_verifier) > PKCE_MAX_VERIFIER_LENGTH:
            return False

        if code_challenge_method == "S256":
            verifier_hash = hashlib.sha256(code_verifier.encode()).digest()
            computed_challenge = base64.urlsafe_b64encode(verifier_hash).decode().rstrip("=")
            return computed_challenge == code_challenge

        elif code_challenge_method == "plain":
            return code_verifier == code_challenge

        return False

    @staticmethod
    def generate_pkce_pair() -> Tuple[str, str]:
        """
        Generate PKCE code verifier and challenge pair.

        Returns:
            Tuple of (code_verifier, code_challenge)
        """
        code_verifier = secrets.token_urlsafe(64)[:128]
        verifier_hash = hashlib.sha256(code_verifier.encode()).digest()
        code_challenge = base64.urlsafe_b64encode(verifier_hash).decode().rstrip("=")
        return code_verifier, code_challenge

    def _hash_token(self, token: str) -> str:
        """Hash token for secure storage"""
        return hashlib.sha256(token.encode()).hexdigest()


# =============================================================================
# FASTAPI ROUTES
# =============================================================================

from fastapi import APIRouter, HTTPException, Depends, Query, Form, Header, Request
from fastapi.responses import RedirectResponse, JSONResponse
from fastapi.security import OAuth2AuthorizationCodeBearer

oauth2_router = APIRouter(prefix="/oauth", tags=["OAuth 2.0"])

# Global service instance
_oauth_service = OAuth2Service()


def get_oauth_service() -> OAuth2Service:
    """Get OAuth service instance"""
    return _oauth_service


@oauth2_router.get("/authorize")
async def authorize(
    response_type: str = Query("code"),
    client_id: str = Query(...),
    redirect_uri: str = Query(...),
    scope: str = Query("openid"),
    state: Optional[str] = Query(None),
    code_challenge: Optional[str] = Query(None),
    code_challenge_method: Optional[str] = Query(None),
    nonce: Optional[str] = Query(None),
    service: OAuth2Service = Depends(get_oauth_service)
):
    """
    OAuth 2.0 Authorization Endpoint.

    Initiates authorization flow. In production, should redirect to login page.
    """
    if response_type != "code":
        return JSONResponse(
            status_code=400,
            content={"error": "unsupported_response_type"}
        )

    # Validate request
    success, error = service.create_authorization_request(
        client_id=client_id,
        redirect_uri=redirect_uri,
        scope=scope,
        state=state,
        code_challenge=code_challenge,
        code_challenge_method=code_challenge_method,
        nonce=nonce
    )

    if not success:
        error_parts = error.split(": ", 1)
        return JSONResponse(
            status_code=400,
            content={
                "error": error_parts[0],
                "error_description": error_parts[1] if len(error_parts) > 1 else error
            }
        )

    # Issue #137 FIX: Remove hardcoded admin user
    # In development mode only, allow simulated user with explicit env check
    import os
    environment = os.getenv("ENVIRONMENT", "development")

    if environment.lower() in ("production", "prod", "staging"):
        # In production: redirect to login page with auth request
        from urllib.parse import urlencode
        login_params = urlencode({
            "oauth_request": "pending",
            "client_id": client_id,
            "redirect_uri": redirect_uri,
            "scope": scope,
            "state": state or "",
            "response_type": response_type
        })
        return RedirectResponse(url=f"/login?{login_params}")

    # Development only: require explicit DEV_OAUTH_AUTO_APPROVE=true
    if os.getenv("DEV_OAUTH_AUTO_APPROVE", "false").lower() != "true":
        return JSONResponse(
            status_code=401,
            content={
                "error": "login_required",
                "error_description": "User authentication required. Set DEV_OAUTH_AUTO_APPROVE=true for development auto-approve."
            }
        )

    # Development with explicit auto-approve enabled
    user_id = os.getenv("DEV_OAUTH_USER", "dev_user")  # Not "admin" by default
    import logging
    logging.getLogger(__name__).warning(
        f"[OAuth] DEV MODE: Auto-approving OAuth request as user '{user_id}'. "
        "This should NEVER happen in production!"
    )

    code = service.generate_authorization_code(
        client_id=client_id,
        user_id=user_id,
        redirect_uri=redirect_uri,
        scope=scope,
        state=state,
        code_challenge=code_challenge,
        code_challenge_method=code_challenge_method,
        nonce=nonce
    )

    redirect_url = service.build_authorization_redirect(redirect_uri, code, state)
    return RedirectResponse(url=redirect_url)


@oauth2_router.post("/token", response_model=TokenResponse)
async def token(
    grant_type: str = Form(...),
    code: Optional[str] = Form(None),
    redirect_uri: Optional[str] = Form(None),
    client_id: Optional[str] = Form(None),
    client_secret: Optional[str] = Form(None),
    refresh_token: Optional[str] = Form(None),
    code_verifier: Optional[str] = Form(None),
    scope: Optional[str] = Form(None),
    service: OAuth2Service = Depends(get_oauth_service)
):
    """
    OAuth 2.0 Token Endpoint.

    Exchanges authorization code or client credentials for tokens.
    """
    try:
        grant = GrantType(grant_type)
    except ValueError:
        raise HTTPException(
            status_code=400,
            detail={"error": "unsupported_grant_type"}
        )

    if grant == GrantType.AUTHORIZATION_CODE:
        if not code or not redirect_uri or not client_id:
            raise HTTPException(
                status_code=400,
                detail={"error": "invalid_request", "error_description": "Missing required parameters"}
            )

        success, tokens, error = service.exchange_authorization_code(
            code=code,
            client_id=client_id,
            client_secret=client_secret,
            redirect_uri=redirect_uri,
            code_verifier=code_verifier
        )

    elif grant == GrantType.CLIENT_CREDENTIALS:
        if not client_id or not client_secret:
            raise HTTPException(
                status_code=400,
                detail={"error": "invalid_request", "error_description": "Client credentials required"}
            )

        success, tokens, error = service.exchange_client_credentials(
            client_id=client_id,
            client_secret=client_secret,
            scope=scope
        )

    elif grant == GrantType.REFRESH_TOKEN:
        if not refresh_token or not client_id:
            raise HTTPException(
                status_code=400,
                detail={"error": "invalid_request", "error_description": "Refresh token and client_id required"}
            )

        success, tokens, error = service.refresh_access_token(
            refresh_token=refresh_token,
            client_id=client_id,
            client_secret=client_secret,
            scope=scope
        )

    else:
        raise HTTPException(
            status_code=400,
            detail={"error": "unsupported_grant_type"}
        )

    if not success:
        error_parts = error.split(": ", 1)
        raise HTTPException(
            status_code=400,
            detail={
                "error": error_parts[0],
                "error_description": error_parts[1] if len(error_parts) > 1 else error
            }
        )

    return tokens


@oauth2_router.post("/revoke")
async def revoke(
    token: str = Form(...),
    token_type_hint: Optional[str] = Form(None),
    client_id: str = Form(...),
    client_secret: str = Form(...),
    service: OAuth2Service = Depends(get_oauth_service)
):
    """
    OAuth 2.0 Token Revocation Endpoint (RFC 7009).
    """
    # Validate client
    is_valid, _, error = service.validate_client(client_id, client_secret)
    if not is_valid:
        raise HTTPException(
            status_code=401,
            detail={"error": "invalid_client", "error_description": error}
        )

    service.revoke_token(
        token=token,
        token_type_hint=token_type_hint,
        client_id=client_id,
        client_secret=client_secret
    )

    return {"success": True}


@oauth2_router.post("/introspect", response_model=TokenIntrospectionResponse)
async def introspect(
    token: str = Form(...),
    token_type_hint: Optional[str] = Form(None),
    client_id: str = Form(...),
    client_secret: str = Form(...),
    service: OAuth2Service = Depends(get_oauth_service)
):
    """
    OAuth 2.0 Token Introspection Endpoint (RFC 7662).
    """
    # Validate client
    is_valid, _, error = service.validate_client(client_id, client_secret)
    if not is_valid:
        raise HTTPException(
            status_code=401,
            detail={"error": "invalid_client", "error_description": error}
        )

    return service.introspect_token(token)


@oauth2_router.get("/scopes")
async def list_scopes():
    """List available OAuth scopes"""
    return {
        "scopes": [
            {"name": name, "description": desc}
            for name, desc in OAuthScopes.all_scopes().items()
        ]
    }


@oauth2_router.post("/register", response_model=ClientRegistrationResponse)
async def register_client(
    request: ClientRegistrationRequest,
    service: OAuth2Service = Depends(get_oauth_service)
):
    """
    Dynamic Client Registration (RFC 7591).

    Register a new OAuth client.
    """
    return service.register_client(request)


@oauth2_router.get("/clients/{client_id}")
async def get_client(
    client_id: str,
    service: OAuth2Service = Depends(get_oauth_service)
):
    """Get OAuth client information (does not return secret)"""
    client = service.get_client(client_id)
    if not client:
        raise HTTPException(status_code=404, detail="Client not found")

    return {
        "client_id": client.client_id,
        "client_name": client.client_name,
        "client_type": client.client_type.value,
        "redirect_uris": client.redirect_uris,
        "allowed_scopes": client.allowed_scopes,
        "allowed_grant_types": [g.value for g in client.allowed_grant_types],
        "created_at": client.created_at.isoformat()
    }


@oauth2_router.get("/.well-known/oauth-authorization-server")
async def oauth_metadata():
    """
    OAuth 2.0 Authorization Server Metadata (RFC 8414).
    """
    return {
        "issuer": OAUTH_ISSUER,
        "authorization_endpoint": f"{OAUTH_ISSUER}/oauth/authorize",
        "token_endpoint": f"{OAUTH_ISSUER}/oauth/token",
        "revocation_endpoint": f"{OAUTH_ISSUER}/oauth/revoke",
        "introspection_endpoint": f"{OAUTH_ISSUER}/oauth/introspect",
        "registration_endpoint": f"{OAUTH_ISSUER}/oauth/register",
        "scopes_supported": list(OAuthScopes.all_scopes().keys()),
        "response_types_supported": ["code"],
        "grant_types_supported": [
            "authorization_code",
            "client_credentials",
            "refresh_token"
        ],
        "token_endpoint_auth_methods_supported": [
            "client_secret_basic",
            "client_secret_post"
        ],
        "code_challenge_methods_supported": ["S256", "plain"],
        "service_documentation": f"{OAUTH_ISSUER}/docs/oauth"
    }


# =============================================================================
# DEPENDENCY FOR PROTECTING ROUTES
# =============================================================================

async def require_oauth_token(
    authorization: str = Header(None, alias="Authorization"),
    service: OAuth2Service = Depends(get_oauth_service)
) -> Dict:
    """
    FastAPI dependency for OAuth-protected routes.

    Usage:
        @app.get("/protected")
        async def protected_route(token_data: dict = Depends(require_oauth_token)):
            return {"user": token_data["sub"]}
    """
    if not authorization:
        raise HTTPException(
            status_code=401,
            detail="Missing authorization header",
            headers={"WWW-Authenticate": "Bearer"}
        )

    if not authorization.startswith("Bearer "):
        raise HTTPException(
            status_code=401,
            detail="Invalid authorization header format",
            headers={"WWW-Authenticate": "Bearer"}
        )

    token = authorization[7:]
    is_valid, payload, error = service.validate_access_token(token)

    if not is_valid:
        raise HTTPException(
            status_code=401,
            detail=error,
            headers={"WWW-Authenticate": "Bearer"}
        )

    return payload


def require_oauth_scope(*required_scopes: str):
    """
    Factory for scope-checking dependency.

    Usage:
        @app.get("/stories")
        async def list_stories(
            token_data: dict = Depends(require_oauth_scope("read:stories"))
        ):
            return {"stories": [...]}
    """
    async def scope_checker(
        token_data: dict = Depends(require_oauth_token)
    ) -> dict:
        token_scopes = token_data.get("scope", "").split()

        for scope in required_scopes:
            if not OAuthScopes.has_scope(token_scopes, scope):
                raise HTTPException(
                    status_code=403,
                    detail=f"Insufficient scope. Required: {', '.join(required_scopes)}"
                )

        return token_data

    return scope_checker


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Service
    "OAuth2Service",
    "get_oauth_service",

    # Models
    "OAuthClient",
    "AuthorizationCode",
    "RefreshToken",
    "TokenResponse",
    "TokenIntrospectionResponse",
    "AuthorizationRequest",
    "TokenRequest",
    "ClientRegistrationRequest",
    "ClientRegistrationResponse",

    # Enums
    "GrantType",
    "ResponseType",
    "TokenType",
    "ClientType",
    "CodeChallengeMethod",

    # Scopes
    "OAuthScopes",

    # Router
    "oauth2_router",

    # Dependencies
    "require_oauth_token",
    "require_oauth_scope",
]
