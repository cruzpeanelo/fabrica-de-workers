"""
Authentication Module v5.0 - JWT com rotacao de chaves e seguranca aprimorada
Plataforma E - Security Hardening

Issue #83 - JWT Authentication Security:
- Removidas credenciais hardcoded
- Implementada rotacao de chaves JWT
- Variaveis de ambiente obrigatorias em producao
"""
import os
import secrets
import hashlib
import json
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Dict, List
import logging

from fastapi import Depends, HTTPException, status, Request, APIRouter
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import JWTError, jwt
from passlib.context import CryptContext
from pydantic import BaseModel, validator

from dotenv import load_dotenv
load_dotenv()

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION - Secure Key Management
# =============================================================================

# Diretorio seguro para armazenar chaves
SECRETS_DIR = Path(__file__).parent.parent / ".secrets"
SECRET_KEY_FILE = SECRETS_DIR / "jwt_keys.json"


def _is_production() -> bool:
    """Verifica se esta em ambiente de producao"""
    return os.getenv("ENVIRONMENT", "development").lower() == "production"


def _validate_key_strength(key: str) -> bool:
    """Valida se a chave tem forca adequada (min 32 bytes)"""
    return len(key) >= 43  # 32 bytes base64 encoded = ~43 chars


class JWTKeyManager:
    """
    Gerenciador seguro de chaves JWT com suporte a rotacao.

    Funcionalidades:
    - Armazenamento seguro de chaves
    - Rotacao automatica de chaves
    - Suporte a multiplas chaves ativas (para transicao)
    - Validacao de forca de chave
    """

    def __init__(self):
        self._keys: Dict[str, dict] = {}
        self._current_key_id: Optional[str] = None
        self._load_keys()

    def _ensure_secrets_dir(self):
        """Garante que o diretorio de secrets existe com permissoes corretas"""
        if not SECRETS_DIR.exists():
            SECRETS_DIR.mkdir(mode=0o700, parents=True)
            logger.info(f"[Auth] Diretorio de secrets criado: {SECRETS_DIR}")

    def _generate_key_id(self) -> str:
        """Gera ID unico para a chave"""
        return f"key_{datetime.utcnow().strftime('%Y%m%d%H%M%S')}_{secrets.token_hex(4)}"

    def _generate_secure_key(self) -> str:
        """Gera uma chave criptograficamente segura"""
        return secrets.token_urlsafe(64)  # 512 bits

    def _load_keys(self):
        """Carrega chaves do armazenamento"""
        # Primeiro, verificar variavel de ambiente (producao)
        env_key = os.getenv("JWT_SECRET_KEY")
        if env_key:
            if not _validate_key_strength(env_key):
                if _is_production():
                    raise ValueError(
                        "JWT_SECRET_KEY deve ter no minimo 32 bytes (43 caracteres base64). "
                        "Gere uma chave segura com: python -c \"import secrets; print(secrets.token_urlsafe(64))\""
                    )
                else:
                    logger.warning("[Auth] JWT_SECRET_KEY fraca detectada. Use uma chave mais forte em producao.")

            self._keys["env_key"] = {
                "key": env_key,
                "created_at": datetime.utcnow().isoformat(),
                "active": True,
                "source": "environment"
            }
            self._current_key_id = "env_key"
            return

        # Em producao, exigir variavel de ambiente
        if _is_production():
            raise ValueError(
                "JWT_SECRET_KEY obrigatoria em producao. "
                "Configure a variavel de ambiente com uma chave segura."
            )

        # Em desenvolvimento, usar arquivo local
        self._ensure_secrets_dir()

        if SECRET_KEY_FILE.exists():
            try:
                data = json.loads(SECRET_KEY_FILE.read_text())
                self._keys = data.get("keys", {})
                self._current_key_id = data.get("current_key_id")

                # Validar que a chave atual existe
                if self._current_key_id and self._current_key_id in self._keys:
                    logger.info(f"[Auth] Chaves JWT carregadas. Chave ativa: {self._current_key_id}")
                    return
            except (json.JSONDecodeError, Exception) as e:
                logger.warning(f"[Auth] Erro ao carregar chaves: {e}. Gerando novas.")

        # Gerar nova chave
        self._generate_initial_key()

    def _generate_initial_key(self):
        """Gera chave inicial para desenvolvimento"""
        key_id = self._generate_key_id()
        self._keys[key_id] = {
            "key": self._generate_secure_key(),
            "created_at": datetime.utcnow().isoformat(),
            "active": True,
            "source": "generated"
        }
        self._current_key_id = key_id
        self._save_keys()
        logger.info(f"[Auth] Nova chave JWT gerada: {key_id}")

    def _save_keys(self):
        """Salva chaves no arquivo com permissoes restritas"""
        if _is_production():
            # Em producao, nao salvar em arquivo
            return

        self._ensure_secrets_dir()

        data = {
            "keys": self._keys,
            "current_key_id": self._current_key_id,
            "updated_at": datetime.utcnow().isoformat()
        }

        SECRET_KEY_FILE.write_text(json.dumps(data, indent=2))
        SECRET_KEY_FILE.chmod(0o600)  # Apenas owner pode ler

    def get_current_key(self) -> str:
        """Retorna a chave atual para assinatura de tokens"""
        if not self._current_key_id or self._current_key_id not in self._keys:
            raise ValueError("Nenhuma chave JWT configurada")
        return self._keys[self._current_key_id]["key"]

    def get_current_key_id(self) -> str:
        """Retorna o ID da chave atual"""
        return self._current_key_id

    def get_all_active_keys(self) -> List[str]:
        """Retorna todas as chaves ativas (para validacao durante rotacao)"""
        return [
            data["key"]
            for data in self._keys.values()
            if data.get("active", False)
        ]

    def rotate_key(self) -> str:
        """
        Rotaciona a chave JWT.

        A chave antiga permanece ativa por um periodo para permitir
        validacao de tokens existentes.

        Returns:
            ID da nova chave
        """
        if _is_production() and os.getenv("JWT_SECRET_KEY"):
            raise ValueError(
                "Rotacao de chave em producao requer atualizacao da variavel JWT_SECRET_KEY. "
                "Atualize a variavel e reinicie a aplicacao."
            )

        # Marcar chave antiga como em transicao
        if self._current_key_id and self._current_key_id in self._keys:
            self._keys[self._current_key_id]["transitioning"] = True
            self._keys[self._current_key_id]["transition_started"] = datetime.utcnow().isoformat()

        # Gerar nova chave
        new_key_id = self._generate_key_id()
        self._keys[new_key_id] = {
            "key": self._generate_secure_key(),
            "created_at": datetime.utcnow().isoformat(),
            "active": True,
            "source": "rotated"
        }

        old_key_id = self._current_key_id
        self._current_key_id = new_key_id
        self._save_keys()

        logger.info(f"[Auth] Chave JWT rotacionada: {old_key_id} -> {new_key_id}")
        return new_key_id

    def deactivate_old_keys(self, max_age_hours: int = 24):
        """
        Desativa chaves antigas apos periodo de transicao.

        Args:
            max_age_hours: Tempo em horas para manter chaves antigas ativas
        """
        cutoff = datetime.utcnow() - timedelta(hours=max_age_hours)
        deactivated = []

        for key_id, data in list(self._keys.items()):
            if key_id == self._current_key_id:
                continue

            created = datetime.fromisoformat(data["created_at"])
            if created < cutoff and data.get("active", False):
                self._keys[key_id]["active"] = False
                self._keys[key_id]["deactivated_at"] = datetime.utcnow().isoformat()
                deactivated.append(key_id)

        if deactivated:
            self._save_keys()
            logger.info(f"[Auth] Chaves desativadas: {deactivated}")

        return deactivated

    def get_key_info(self) -> Dict:
        """Retorna informacoes sobre as chaves (sem expor valores)"""
        return {
            "current_key_id": self._current_key_id,
            "total_keys": len(self._keys),
            "active_keys": sum(1 for d in self._keys.values() if d.get("active", False)),
            "keys": [
                {
                    "key_id": kid,
                    "created_at": data["created_at"],
                    "active": data.get("active", False),
                    "source": data.get("source", "unknown"),
                    "is_current": kid == self._current_key_id
                }
                for kid, data in self._keys.items()
            ]
        }


# Instancia global do gerenciador de chaves
_key_manager: Optional[JWTKeyManager] = None


def get_key_manager() -> JWTKeyManager:
    """Retorna o gerenciador de chaves JWT (singleton)"""
    global _key_manager
    if _key_manager is None:
        _key_manager = JWTKeyManager()
    return _key_manager


# Configuracoes JWT
def get_secret_key() -> str:
    """Obtem a chave secreta atual para JWT"""
    return get_key_manager().get_current_key()


SECRET_KEY = property(lambda self: get_secret_key())  # Lazy loading
ALGORITHM = os.getenv("JWT_ALGORITHM", "HS256")
ACCESS_TOKEN_EXPIRE_MINUTES = int(os.getenv("ACCESS_TOKEN_EXPIRE_MINUTES", 1440))

# Password hashing
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

# Bearer token security
security = HTTPBearer(auto_error=False)


# =============================================================================
# MODELS
# =============================================================================

class UserInfo(BaseModel):
    """Informacoes basicas do usuario"""
    id: Optional[int] = None
    username: str
    email: Optional[str] = None
    role: str


class Token(BaseModel):
    """Token de acesso"""
    access_token: str
    token_type: str = "bearer"
    expires_at: str
    force_password_change: bool = False  # Issue #138: Indica se usuário precisa trocar senha
    user: Optional[UserInfo] = None  # Informacoes do usuario logado


class TokenData(BaseModel):
    """Dados extraidos do token"""
    username: Optional[str] = None
    role: Optional[str] = None
    exp: Optional[datetime] = None


class UserCreate(BaseModel):
    """Dados para criar usuario"""
    username: str
    password: str
    email: Optional[str] = None
    role: str = "VIEWER"


class UserLogin(BaseModel):
    """Dados para login"""
    username: str
    password: str


class UserResponse(BaseModel):
    """Response do usuario"""
    username: str
    email: Optional[str]
    role: str
    active: bool


# =============================================================================
# PASSWORD FUNCTIONS
# =============================================================================

def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Verifica se senha corresponde ao hash"""
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password: str) -> str:
    """Gera hash da senha"""
    return pwd_context.hash(password)


# =============================================================================
# TOKEN FUNCTIONS
# =============================================================================

def create_access_token(data: dict, expires_delta: timedelta = None) -> str:
    """
    Cria JWT token

    Args:
        data: Dados para incluir no token (sub=username, role=role)
        expires_delta: Tempo de expiracao customizado

    Returns:
        Token JWT encoded
    """
    to_encode = data.copy()

    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

    to_encode.update({"exp": expire})

    # Adicionar key_id para suporte a rotacao
    key_manager = get_key_manager()
    to_encode.update({"kid": key_manager.get_current_key_id()})

    secret_key = key_manager.get_current_key()
    encoded_jwt = jwt.encode(to_encode, secret_key, algorithm=ALGORITHM)
    return encoded_jwt


def decode_token(token: str) -> TokenData:
    """
    Decodifica e valida JWT token

    Suporta validacao com multiplas chaves ativas para
    permitir rotacao sem invalidar tokens existentes.

    Args:
        token: Token JWT

    Returns:
        TokenData com dados do usuario

    Raises:
        HTTPException: Se token invalido
    """
    key_manager = get_key_manager()

    # Tentar validar com todas as chaves ativas
    active_keys = key_manager.get_all_active_keys()

    last_error = None
    for secret_key in active_keys:
        try:
            payload = jwt.decode(token, secret_key, algorithms=[ALGORITHM])
            username = payload.get("sub")
            role = payload.get("role")
            exp = payload.get("exp")

            if username is None:
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Invalid token: missing username"
                )

            return TokenData(
                username=username,
                role=role,
                exp=datetime.fromtimestamp(exp) if exp else None
            )

        except JWTError as e:
            last_error = e
            continue

    # Nenhuma chave conseguiu validar
    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail=f"Invalid token: {str(last_error) if last_error else 'validation failed'}"
    )


# =============================================================================
# DEPENDENCIES
# =============================================================================

async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security)
) -> TokenData:
    """
    Dependency para obter usuario atual do token

    Usage:
        @app.get("/protected")
        async def protected_route(user: TokenData = Depends(get_current_user)):
            return {"user": user.username}
    """
    if credentials is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Missing authorization header"
        )

    return decode_token(credentials.credentials)


async def get_current_user_optional(
    credentials: HTTPAuthorizationCredentials = Depends(security)
) -> Optional[TokenData]:
    """
    Dependency para obter usuario atual (opcional)

    Retorna None se nao autenticado ao inves de erro.
    """
    if credentials is None:
        return None

    try:
        return decode_token(credentials.credentials)
    except HTTPException:
        return None


async def require_role(required_role: str):
    """
    Factory para dependency que requer role especifica

    Usage:
        @app.get("/admin")
        async def admin_route(user: TokenData = Depends(require_role("ADMIN"))):
            return {"admin": user.username}
    """
    async def role_checker(user: TokenData = Depends(get_current_user)) -> TokenData:
        if user.role != required_role and user.role != "ADMIN":
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Required role: {required_role}"
            )
        return user

    return role_checker


# =============================================================================
# AUTH ROUTES (para incluir no app)
# =============================================================================

from fastapi import APIRouter, Request

auth_router = APIRouter(prefix="/api/v1/auth", tags=["Authentication"])


@auth_router.post("/login", response_model=Token)
async def login(credentials: UserLogin):
    """
    Autentica usuario e retorna token JWT

    Credenciais devem estar configuradas no banco de dados ou
    via variaveis de ambiente (DEFAULT_ADMIN_USER/DEFAULT_ADMIN_PASS).

    Issue #138: Em produção, credenciais default são bloqueadas.
    """
    # Issue #138: Verificar se credenciais estão na lista de bloqueio (produção)
    from factory.config import is_credential_blocked
    if is_credential_blocked(credentials.username, credentials.password):
        logger.warning(
            f"[Auth] BLOCKED: Tentativa de login com credencial bloqueada em produção: {credentials.username}"
        )
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="This credential is not allowed in production. Please use a secure password."
        )

    # Verificar credenciais de admin via variaveis de ambiente (apenas desenvolvimento)
    if not _is_production():
        default_user = os.getenv("DEFAULT_ADMIN_USER")
        default_pass = os.getenv("DEFAULT_ADMIN_PASS")

        if default_user and default_pass:
            if credentials.username == default_user and credentials.password == default_pass:
                logger.info(f"[Auth] Login via credenciais de ambiente: {default_user}")
                token = create_access_token(
                    data={"sub": default_user, "role": "ADMIN"}
                )
                expires_at = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

                return Token(
                    access_token=token,
                    token_type="bearer",
                    expires_at=expires_at.isoformat()
                )
    else:
        # Issue #138: Log warning se alguém tentou usar credenciais de ambiente em produção
        default_user = os.getenv("DEFAULT_ADMIN_USER")
        if default_user and credentials.username == default_user:
            logger.warning(
                f"[Auth] Tentativa de login com DEFAULT_ADMIN_USER em produção (bloqueado): {default_user}"
            )

    # Buscar usuario do banco de dados
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            user = db.query(User).filter(
                User.username == credentials.username,
                User.active == True
            ).first()

            if user and verify_password(credentials.password, user.password_hash):
                # Issue #138: Verificar se usuário precisa trocar senha
                force_change = getattr(user, 'force_password_change', False)

                token = create_access_token(
                    data={
                        "sub": user.username,
                        "role": user.role,
                        "force_password_change": force_change
                    }
                )
                expires_at = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

                # Atualizar last_login
                user.last_login = datetime.utcnow()
                db.commit()

                logger.info(f"[Auth] Login bem sucedido: {user.username}")

                return Token(
                    access_token=token,
                    token_type="bearer",
                    expires_at=expires_at.isoformat(),
                    force_password_change=force_change,
                    user=UserInfo(
                        id=user.id,
                        username=user.username,
                        email=user.email,
                        role=user.role
                    )
                )
        finally:
            db.close()
    except Exception as e:
        logger.error(f"[Auth] Erro ao buscar usuario: {e}")

    # Log tentativa falha (sem expor se usuario existe)
    logger.warning(f"[Auth] Tentativa de login falha para: {credentials.username}")

    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Invalid username or password"
    )


@auth_router.get("/me", response_model=UserResponse)
async def get_me(user: TokenData = Depends(get_current_user)):
    """Retorna dados do usuario atual"""
    return UserResponse(
        username=user.username,
        email=None,
        role=user.role,
        active=True
    )


@auth_router.post("/refresh", response_model=Token)
async def refresh_token(user: TokenData = Depends(get_current_user)):
    """Renova token JWT"""
    new_token = create_access_token(
        data={"sub": user.username, "role": user.role}
    )
    expires_at = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

    return Token(
        access_token=new_token,
        token_type="bearer",
        expires_at=expires_at.isoformat()
    )


# =============================================================================
# LOGOUT - Issue #358 JWT Blacklist
# =============================================================================

class LogoutResponse(BaseModel):
    """Resposta de logout"""
    message: str
    token_revoked: bool = True


@auth_router.post("/logout", response_model=LogoutResponse)
async def logout(
    request: Request,
    user: TokenData = Depends(get_current_user)
):
    """
    Logout - revoga o token JWT atual.

    Issue #358: Implementa logout com blacklist de token.
    """
    try:
        # Extrair token do header
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Bearer "):
            token = auth_header[7:]

            # Revogar token usando blacklist
            from factory.auth.token_blacklist import revoke_token
            revoke_token(token, user.username, reason="logout")

            logger.info(f"[Auth] Logout: user={user.username} token revoked")

            return LogoutResponse(
                message="Logout successful. Token has been revoked.",
                token_revoked=True
            )
    except ImportError:
        logger.warning("[Auth] token_blacklist module not available")
    except Exception as e:
        logger.error(f"[Auth] Logout error: {e}")

    # Even if blacklist fails, return success (token will expire naturally)
    return LogoutResponse(
        message="Logout successful.",
        token_revoked=False
    )


# =============================================================================
# PASSWORD CHANGE - Issue #138
# =============================================================================

class PasswordChangeRequest(BaseModel):
    """Requisição de mudança de senha"""
    current_password: str
    new_password: str


class PasswordChangeResponse(BaseModel):
    """Resposta de mudança de senha"""
    message: str
    access_token: Optional[str] = None
    expires_at: Optional[str] = None


@auth_router.post("/change-password", response_model=PasswordChangeResponse)
async def change_password(
    data: PasswordChangeRequest,
    user: TokenData = Depends(get_current_user)
):
    """
    Issue #138: Muda a senha do usuário atual.

    Valida:
    - Senha atual correta
    - Nova senha atende requisitos de força (em produção)
    - Nova senha não é igual à atual
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import User
    from factory.config import validate_password_strength, is_credential_blocked

    # Validar força da nova senha em produção
    is_valid, error_msg = validate_password_strength(data.new_password)
    if not is_valid:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Nova senha não atende requisitos: {error_msg}"
        )

    # Verificar se nova credencial seria bloqueada
    if is_credential_blocked(user.username, data.new_password):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Esta combinação de usuário/senha não é permitida. Escolha uma senha mais forte."
        )

    db = SessionLocal()
    try:
        db_user = db.query(User).filter(
            User.username == user.username,
            User.active == True
        ).first()

        if not db_user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Usuário não encontrado"
            )

        # Verificar senha atual
        if not verify_password(data.current_password, db_user.password_hash):
            logger.warning(f"[Auth] Tentativa de mudança de senha com senha atual incorreta: {user.username}")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Senha atual incorreta"
            )

        # Verificar se nova senha é diferente
        if verify_password(data.new_password, db_user.password_hash):
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Nova senha deve ser diferente da atual"
            )

        # Atualizar senha
        db_user.password_hash = get_password_hash(data.new_password)
        db_user.force_password_change = False  # Issue #138: Marcar que já trocou
        db_user.updated_at = datetime.utcnow()
        db.commit()

        logger.info(f"[Auth] Senha alterada com sucesso: {user.username}")

        # Gerar novo token (sem force_password_change)
        new_token = create_access_token(
            data={"sub": user.username, "role": user.role, "force_password_change": False}
        )
        expires_at = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

        return PasswordChangeResponse(
            message="Senha alterada com sucesso",
            access_token=new_token,
            expires_at=expires_at.isoformat()
        )

    finally:
        db.close()


# =============================================================================
# KEY ROTATION ENDPOINTS
# =============================================================================

class KeyRotationResponse(BaseModel):
    """Resposta de rotacao de chave"""
    message: str
    new_key_id: str
    old_key_id: Optional[str] = None


class KeyInfoResponse(BaseModel):
    """Informacoes sobre chaves JWT"""
    current_key_id: str
    total_keys: int
    active_keys: int
    keys: List[dict]


@auth_router.get("/keys/info", response_model=KeyInfoResponse)
async def get_key_info(user: TokenData = Depends(get_current_user)):
    """
    Retorna informacoes sobre as chaves JWT (apenas admin).
    Nao expoe os valores das chaves, apenas metadados.
    """
    if user.role != "ADMIN":
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Admin role required"
        )

    key_manager = get_key_manager()
    info = key_manager.get_key_info()

    return KeyInfoResponse(**info)


@auth_router.post("/keys/rotate", response_model=KeyRotationResponse)
async def rotate_jwt_key(user: TokenData = Depends(get_current_user)):
    """
    Rotaciona a chave JWT (apenas admin).

    A chave antiga permanece ativa por 24h para permitir
    validacao de tokens existentes.

    Em producao com JWT_SECRET_KEY, a rotacao deve ser
    feita atualizando a variavel de ambiente.
    """
    if user.role != "ADMIN":
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Admin role required"
        )

    key_manager = get_key_manager()
    old_key_id = key_manager.get_current_key_id()

    try:
        new_key_id = key_manager.rotate_key()
        logger.info(f"[Auth] Chave rotacionada por {user.username}: {old_key_id} -> {new_key_id}")

        return KeyRotationResponse(
            message="Chave JWT rotacionada com sucesso. Tokens existentes permanecem validos por 24h.",
            new_key_id=new_key_id,
            old_key_id=old_key_id
        )
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )


@auth_router.post("/keys/cleanup")
async def cleanup_old_keys(
    max_age_hours: int = 24,
    user: TokenData = Depends(get_current_user)
):
    """
    Desativa chaves antigas apos periodo de transicao (apenas admin).

    Args:
        max_age_hours: Tempo em horas para manter chaves antigas (default: 24)
    """
    if user.role != "ADMIN":
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Admin role required"
        )

    key_manager = get_key_manager()
    deactivated = key_manager.deactivate_old_keys(max_age_hours)

    logger.info(f"[Auth] Limpeza de chaves por {user.username}: {len(deactivated)} desativadas")

    return {
        "message": f"{len(deactivated)} chave(s) desativada(s)",
        "deactivated_keys": deactivated
    }
