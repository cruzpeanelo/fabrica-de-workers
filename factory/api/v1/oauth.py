"""
OAuth 2.0 Handler - Issue #109
==============================

Implementacao de OAuth 2.0 para API.

Fluxos suportados:
- Authorization Code: Para aplicacoes web com backend
- Client Credentials: Para integracao server-to-server
- Refresh Token: Para renovacao de tokens
- PKCE: Para aplicacoes SPA/mobile

Scopes disponiveis:
- read:stories - Ler stories
- write:stories - Criar/editar stories
- read:projects - Ler projetos
- write:projects - Criar/editar projetos
- read:analytics - Acessar analytics
- admin - Acesso administrativo completo
"""

import secrets
import hashlib
import base64
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Set
from enum import Enum

from fastapi import APIRouter, Depends, HTTPException, Query, Form, Request, status
from fastapi.security import OAuth2PasswordBearer, OAuth2AuthorizationCodeBearer
from fastapi.responses import RedirectResponse, JSONResponse
from pydantic import BaseModel, Field
from jose import jwt, JWTError

# Configuracoes
from factory.config import (
    JWT_SECRET_KEY,
    JWT_ALGORITHM,
    ACCESS_TOKEN_EXPIRE_MINUTES
)

# Import authentication dependencies (Issue #137)
from factory.api.auth import get_current_user, TokenData

router = APIRouter(tags=["OAuth 2.0"])


# =============================================================================
# MODELS
# =============================================================================

class GrantType(str, Enum):
    """Tipos de grant OAuth 2.0"""
    AUTHORIZATION_CODE = "authorization_code"
    CLIENT_CREDENTIALS = "client_credentials"
    REFRESH_TOKEN = "refresh_token"


class TokenType(str, Enum):
    """Tipos de token"""
    BEARER = "Bearer"
    ACCESS = "access"
    REFRESH = "refresh"


class OAuthClient(BaseModel):
    """Cliente OAuth registrado"""
    client_id: str
    client_secret: str
    client_name: str
    redirect_uris: List[str]
    allowed_scopes: List[str]
    grant_types: List[str]
    is_confidential: bool = True
    created_at: datetime = Field(default_factory=datetime.utcnow)


class AuthorizationCode(BaseModel):
    """Codigo de autorizacao"""
    code: str
    client_id: str
    user_id: str
    redirect_uri: str
    scope: str
    code_challenge: Optional[str] = None
    code_challenge_method: Optional[str] = None
    expires_at: datetime
    created_at: datetime = Field(default_factory=datetime.utcnow)


class TokenResponse(BaseModel):
    """Resposta de token OAuth"""
    access_token: str
    token_type: str = "Bearer"
    expires_in: int
    refresh_token: Optional[str] = None
    scope: str


class TokenRequest(BaseModel):
    """Requisicao de token"""
    grant_type: GrantType
    code: Optional[str] = None
    redirect_uri: Optional[str] = None
    client_id: Optional[str] = None
    client_secret: Optional[str] = None
    refresh_token: Optional[str] = None
    code_verifier: Optional[str] = None
    scope: Optional[str] = None


class AuthorizeRequest(BaseModel):
    """Requisicao de autorizacao"""
    response_type: str = "code"
    client_id: str
    redirect_uri: str
    scope: str = "read:stories"
    state: Optional[str] = None
    code_challenge: Optional[str] = None
    code_challenge_method: Optional[str] = None


class ClientCreate(BaseModel):
    """Criar cliente OAuth"""
    client_name: str = Field(..., min_length=3, max_length=100)
    redirect_uris: List[str] = Field(..., min_items=1)
    allowed_scopes: List[str] = Field(default=["read:stories"])
    grant_types: List[str] = Field(default=["authorization_code"])


class ClientResponse(BaseModel):
    """Resposta de cliente OAuth"""
    client_id: str
    client_name: str
    redirect_uris: List[str]
    allowed_scopes: List[str]
    grant_types: List[str]
    created_at: str


# =============================================================================
# SCOPES
# =============================================================================

AVAILABLE_SCOPES = {
    "read:stories": "Ler user stories",
    "write:stories": "Criar e editar user stories",
    "read:tasks": "Ler tasks",
    "write:tasks": "Criar e editar tasks",
    "read:projects": "Ler projetos",
    "write:projects": "Criar e editar projetos",
    "read:analytics": "Acessar metricas e analytics",
    "read:users": "Ler informacoes de usuarios",
    "write:users": "Gerenciar usuarios",
    "admin": "Acesso administrativo completo"
}


def validate_scopes(requested_scopes: str, allowed_scopes: List[str]) -> Set[str]:
    """Valida e retorna scopes permitidos"""
    if not requested_scopes:
        return set()

    requested = set(requested_scopes.split())
    allowed = set(allowed_scopes)

    # Admin tem todos os scopes
    if "admin" in allowed:
        return requested

    valid = requested.intersection(allowed)
    return valid


# =============================================================================
# STORES (Em memoria - substituir por banco em producao)
# =============================================================================

class OAuth2Store:
    """Store para dados OAuth (em memoria para desenvolvimento)"""

    def __init__(self):
        self.clients: Dict[str, OAuthClient] = {}
        self.authorization_codes: Dict[str, AuthorizationCode] = {}
        self.refresh_tokens: Dict[str, dict] = {}

        # Registrar cliente de exemplo
        self._register_demo_client()

    def _register_demo_client(self):
        """Registra cliente de demonstracao"""
        demo_client = OAuthClient(
            client_id="demo_client_001",
            client_secret=secrets.token_urlsafe(32),
            client_name="Demo Application",
            redirect_uris=["http://localhost:3000/callback", "http://localhost:9001/oauth/callback"],
            allowed_scopes=list(AVAILABLE_SCOPES.keys()),
            grant_types=["authorization_code", "client_credentials", "refresh_token"]
        )
        self.clients[demo_client.client_id] = demo_client

    def get_client(self, client_id: str) -> Optional[OAuthClient]:
        """Busca cliente por ID"""
        return self.clients.get(client_id)

    def create_client(self, data: ClientCreate) -> OAuthClient:
        """Cria novo cliente OAuth"""
        client = OAuthClient(
            client_id=f"client_{secrets.token_urlsafe(16)}",
            client_secret=secrets.token_urlsafe(32),
            client_name=data.client_name,
            redirect_uris=data.redirect_uris,
            allowed_scopes=data.allowed_scopes,
            grant_types=data.grant_types
        )
        self.clients[client.client_id] = client
        return client

    def create_authorization_code(
        self,
        client_id: str,
        user_id: str,
        redirect_uri: str,
        scope: str,
        code_challenge: Optional[str] = None,
        code_challenge_method: Optional[str] = None
    ) -> str:
        """Gera codigo de autorizacao"""
        code = secrets.token_urlsafe(32)

        auth_code = AuthorizationCode(
            code=code,
            client_id=client_id,
            user_id=user_id,
            redirect_uri=redirect_uri,
            scope=scope,
            code_challenge=code_challenge,
            code_challenge_method=code_challenge_method,
            expires_at=datetime.utcnow() + timedelta(minutes=10)
        )

        self.authorization_codes[code] = auth_code
        return code

    def get_authorization_code(self, code: str) -> Optional[AuthorizationCode]:
        """Busca codigo de autorizacao"""
        auth_code = self.authorization_codes.get(code)

        if auth_code and auth_code.expires_at > datetime.utcnow():
            return auth_code

        # Expirado ou nao existe
        if code in self.authorization_codes:
            del self.authorization_codes[code]

        return None

    def consume_authorization_code(self, code: str) -> Optional[AuthorizationCode]:
        """Consome e remove codigo de autorizacao"""
        auth_code = self.get_authorization_code(code)
        if auth_code:
            del self.authorization_codes[code]
        return auth_code

    def store_refresh_token(self, token: str, data: dict):
        """Armazena refresh token"""
        self.refresh_tokens[token] = {
            **data,
            "created_at": datetime.utcnow().isoformat()
        }

    def get_refresh_token(self, token: str) -> Optional[dict]:
        """Busca dados do refresh token"""
        return self.refresh_tokens.get(token)

    def revoke_refresh_token(self, token: str) -> bool:
        """Revoga refresh token"""
        if token in self.refresh_tokens:
            del self.refresh_tokens[token]
            return True
        return False


# Instancia global do store
oauth_store = OAuth2Store()


# =============================================================================
# HANDLERS
# =============================================================================

class OAuth2Handler:
    """Handler principal OAuth 2.0"""

    def __init__(self, store: OAuth2Store):
        self.store = store

    def authorize(
        self,
        client_id: str,
        redirect_uri: str,
        scope: str,
        state: Optional[str] = None,
        code_challenge: Optional[str] = None,
        code_challenge_method: Optional[str] = None,
        user_id: str = None
    ) -> str:
        """
        Gera codigo de autorizacao.

        Args:
            client_id: ID do cliente
            redirect_uri: URI de callback
            scope: Scopes solicitados
            state: Estado para CSRF
            code_challenge: PKCE challenge
            code_challenge_method: Metodo PKCE (S256)
            user_id: ID do usuario autenticado

        Returns:
            URL de redirect com codigo
        """
        # Validar cliente
        client = self.store.get_client(client_id)
        if not client:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Cliente nao encontrado"
            )

        # Validar redirect URI
        if redirect_uri not in client.redirect_uris:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Redirect URI nao autorizado"
            )

        # Validar scopes
        valid_scopes = validate_scopes(scope, client.allowed_scopes)
        if not valid_scopes:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Scopes invalidos"
            )

        # Gerar codigo
        code = self.store.create_authorization_code(
            client_id=client_id,
            user_id=user_id or "anonymous",
            redirect_uri=redirect_uri,
            scope=" ".join(valid_scopes),
            code_challenge=code_challenge,
            code_challenge_method=code_challenge_method
        )

        # Construir URL de redirect
        params = [f"code={code}"]
        if state:
            params.append(f"state={state}")

        separator = "&" if "?" in redirect_uri else "?"
        return f"{redirect_uri}{separator}{'&'.join(params)}"

    def token(
        self,
        grant_type: GrantType,
        code: Optional[str] = None,
        redirect_uri: Optional[str] = None,
        client_id: Optional[str] = None,
        client_secret: Optional[str] = None,
        refresh_token: Optional[str] = None,
        code_verifier: Optional[str] = None,
        scope: Optional[str] = None
    ) -> TokenResponse:
        """
        Troca codigo/credenciais por token.

        Args:
            grant_type: Tipo de grant
            code: Codigo de autorizacao
            redirect_uri: URI de redirect
            client_id: ID do cliente
            client_secret: Secret do cliente
            refresh_token: Token de refresh
            code_verifier: PKCE verifier
            scope: Scopes (para client_credentials)

        Returns:
            TokenResponse com access e refresh tokens
        """
        if grant_type == GrantType.AUTHORIZATION_CODE:
            return self._handle_authorization_code(
                code=code,
                redirect_uri=redirect_uri,
                client_id=client_id,
                client_secret=client_secret,
                code_verifier=code_verifier
            )

        elif grant_type == GrantType.CLIENT_CREDENTIALS:
            return self._handle_client_credentials(
                client_id=client_id,
                client_secret=client_secret,
                scope=scope
            )

        elif grant_type == GrantType.REFRESH_TOKEN:
            return self._handle_refresh_token(
                refresh_token=refresh_token,
                client_id=client_id,
                client_secret=client_secret
            )

        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Grant type nao suportado: {grant_type}"
        )

    def _handle_authorization_code(
        self,
        code: str,
        redirect_uri: str,
        client_id: str,
        client_secret: str,
        code_verifier: Optional[str] = None
    ) -> TokenResponse:
        """Processa authorization code grant"""

        # Buscar e consumir codigo
        auth_code = self.store.consume_authorization_code(code)
        if not auth_code:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Codigo de autorizacao invalido ou expirado"
            )

        # Validar cliente
        client = self.store.get_client(client_id)
        if not client:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Cliente nao encontrado"
            )

        # Validar secret (para clientes confidenciais)
        if client.is_confidential and client.client_secret != client_secret:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Client secret invalido"
            )

        # Validar redirect URI
        if auth_code.redirect_uri != redirect_uri:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Redirect URI nao corresponde"
            )

        # Validar PKCE se usado
        if auth_code.code_challenge:
            if not code_verifier:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail="Code verifier obrigatorio"
                )

            if not self._verify_pkce(
                code_verifier,
                auth_code.code_challenge,
                auth_code.code_challenge_method
            ):
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail="Code verifier invalido"
                )

        # Gerar tokens
        return self._generate_tokens(
            user_id=auth_code.user_id,
            client_id=client_id,
            scope=auth_code.scope
        )

    def _handle_client_credentials(
        self,
        client_id: str,
        client_secret: str,
        scope: Optional[str] = None
    ) -> TokenResponse:
        """Processa client credentials grant"""

        # Validar cliente
        client = self.store.get_client(client_id)
        if not client:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Cliente nao encontrado"
            )

        # Validar secret
        if client.client_secret != client_secret:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Credenciais invalidas"
            )

        # Validar grant type permitido
        if "client_credentials" not in client.grant_types:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Grant type nao permitido para este cliente"
            )

        # Validar scopes
        valid_scopes = validate_scopes(scope or "", client.allowed_scopes)
        if scope and not valid_scopes:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Scopes solicitados nao permitidos"
            )

        # Gerar tokens (sem refresh para client_credentials)
        return self._generate_tokens(
            user_id=f"client:{client_id}",
            client_id=client_id,
            scope=" ".join(valid_scopes) if valid_scopes else "",
            include_refresh=False
        )

    def _handle_refresh_token(
        self,
        refresh_token: str,
        client_id: str,
        client_secret: Optional[str] = None
    ) -> TokenResponse:
        """Processa refresh token grant"""

        # Buscar dados do refresh token
        token_data = self.store.get_refresh_token(refresh_token)
        if not token_data:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Refresh token invalido"
            )

        # Validar cliente
        if token_data.get("client_id") != client_id:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Cliente nao corresponde"
            )

        # Revogar token antigo
        self.store.revoke_refresh_token(refresh_token)

        # Gerar novos tokens
        return self._generate_tokens(
            user_id=token_data.get("user_id"),
            client_id=client_id,
            scope=token_data.get("scope", "")
        )

    def _generate_tokens(
        self,
        user_id: str,
        client_id: str,
        scope: str,
        include_refresh: bool = True
    ) -> TokenResponse:
        """Gera access e refresh tokens"""

        expires_delta = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
        expire = datetime.utcnow() + expires_delta

        # Payload do access token
        payload = {
            "sub": user_id,
            "client_id": client_id,
            "scope": scope,
            "type": "access",
            "exp": expire,
            "iat": datetime.utcnow()
        }

        access_token = jwt.encode(payload, JWT_SECRET_KEY, algorithm=JWT_ALGORITHM)

        # Gerar refresh token se solicitado
        refresh_token = None
        if include_refresh:
            refresh_token = secrets.token_urlsafe(64)
            self.store.store_refresh_token(
                refresh_token,
                {
                    "user_id": user_id,
                    "client_id": client_id,
                    "scope": scope
                }
            )

        return TokenResponse(
            access_token=access_token,
            token_type="Bearer",
            expires_in=int(expires_delta.total_seconds()),
            refresh_token=refresh_token,
            scope=scope
        )

    def _verify_pkce(
        self,
        code_verifier: str,
        code_challenge: str,
        code_challenge_method: str
    ) -> bool:
        """Verifica PKCE code verifier"""

        if code_challenge_method == "S256":
            # SHA256 hash
            verifier_hash = hashlib.sha256(code_verifier.encode()).digest()
            computed_challenge = base64.urlsafe_b64encode(verifier_hash).decode().rstrip("=")
            return computed_challenge == code_challenge

        elif code_challenge_method == "plain":
            return code_verifier == code_challenge

        return False


# Instancia global do handler
oauth_handler = OAuth2Handler(oauth_store)


# =============================================================================
# ROUTES
# =============================================================================

@router.get("/authorize")
async def authorize(
    response_type: str = Query("code"),
    client_id: str = Query(...),
    redirect_uri: str = Query(...),
    scope: str = Query("read:stories"),
    state: Optional[str] = Query(None),
    code_challenge: Optional[str] = Query(None),
    code_challenge_method: Optional[str] = Query(None),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Endpoint de autorizacao OAuth 2.0.

    Redireciona usuario para login e depois para redirect_uri com codigo.

    Requer autenticacao JWT para identificar o usuario.
    """
    if response_type != "code":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Apenas response_type=code e suportado"
        )

    # Obter user_id do token JWT autenticado (Issue #137)
    user_id = current_user.username

    redirect_url = oauth_handler.authorize(
        client_id=client_id,
        redirect_uri=redirect_uri,
        scope=scope,
        state=state,
        code_challenge=code_challenge,
        code_challenge_method=code_challenge_method,
        user_id=user_id
    )

    return RedirectResponse(url=redirect_url)


@router.post("/token", response_model=TokenResponse)
async def token(
    grant_type: str = Form(...),
    code: Optional[str] = Form(None),
    redirect_uri: Optional[str] = Form(None),
    client_id: Optional[str] = Form(None),
    client_secret: Optional[str] = Form(None),
    refresh_token: Optional[str] = Form(None),
    code_verifier: Optional[str] = Form(None),
    scope: Optional[str] = Form(None)
):
    """
    Token endpoint OAuth 2.0.

    Troca authorization code ou client credentials por access token.
    """
    try:
        grant = GrantType(grant_type)
    except ValueError:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Grant type invalido: {grant_type}"
        )

    return oauth_handler.token(
        grant_type=grant,
        code=code,
        redirect_uri=redirect_uri,
        client_id=client_id,
        client_secret=client_secret,
        refresh_token=refresh_token,
        code_verifier=code_verifier,
        scope=scope
    )


@router.post("/revoke")
async def revoke_token(
    token: str = Form(...),
    token_type_hint: Optional[str] = Form(None),
    client_id: str = Form(...),
    client_secret: str = Form(...)
):
    """
    Revoga um token.

    Suporta revogacao de refresh tokens.
    """
    # Validar cliente
    client = oauth_store.get_client(client_id)
    if not client or client.client_secret != client_secret:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Credenciais invalidas"
        )

    # Revogar token
    if token_type_hint == "refresh_token" or oauth_store.get_refresh_token(token):
        oauth_store.revoke_refresh_token(token)

    return {"success": True, "message": "Token revogado"}


@router.get("/scopes")
async def list_scopes():
    """
    Lista scopes disponiveis.
    """
    return {
        "scopes": [
            {"name": name, "description": desc}
            for name, desc in AVAILABLE_SCOPES.items()
        ]
    }


@router.post("/clients", response_model=ClientResponse)
async def register_client(client_data: ClientCreate):
    """
    Registra novo cliente OAuth.

    Retorna client_id e client_secret (armazenar de forma segura!).
    """
    client = oauth_store.create_client(client_data)

    return ClientResponse(
        client_id=client.client_id,
        client_name=client.client_name,
        redirect_uris=client.redirect_uris,
        allowed_scopes=client.allowed_scopes,
        grant_types=client.grant_types,
        created_at=client.created_at.isoformat()
    )


@router.get("/clients/{client_id}")
async def get_client(client_id: str):
    """
    Busca informacoes de um cliente.

    Nao retorna client_secret.
    """
    client = oauth_store.get_client(client_id)
    if not client:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Cliente nao encontrado"
        )

    return ClientResponse(
        client_id=client.client_id,
        client_name=client.client_name,
        redirect_uris=client.redirect_uris,
        allowed_scopes=client.allowed_scopes,
        grant_types=client.grant_types,
        created_at=client.created_at.isoformat()
    )


# =============================================================================
# TOKEN VALIDATION (para uso em outros modulos)
# =============================================================================

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/api/v1/oauth/token", auto_error=False)


async def get_current_token(token: str = Depends(oauth2_scheme)) -> Optional[dict]:
    """
    Dependency para obter dados do token OAuth.

    Uso:
        @app.get("/protected")
        async def protected_route(token_data: dict = Depends(get_current_token)):
            if not token_data:
                raise HTTPException(status_code=401)
            return token_data
    """
    if not token:
        return None

    try:
        payload = jwt.decode(token, JWT_SECRET_KEY, algorithms=[JWT_ALGORITHM])

        if payload.get("type") != "access":
            return None

        return {
            "user_id": payload.get("sub"),
            "client_id": payload.get("client_id"),
            "scope": payload.get("scope", "").split(),
            "exp": payload.get("exp")
        }

    except JWTError:
        return None


def require_scope(*required_scopes: str):
    """
    Decorator para exigir scopes especificos.

    Uso:
        @app.get("/stories")
        @require_scope("read:stories")
        async def list_stories():
            ...
    """
    async def scope_checker(token_data: dict = Depends(get_current_token)):
        if not token_data:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Token de acesso invalido"
            )

        user_scopes = set(token_data.get("scope", []))

        # Admin tem todos os scopes
        if "admin" in user_scopes:
            return token_data

        # Verificar scopes requeridos
        if not set(required_scopes).issubset(user_scopes):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Scopes requeridos: {', '.join(required_scopes)}"
            )

        return token_data

    return scope_checker
