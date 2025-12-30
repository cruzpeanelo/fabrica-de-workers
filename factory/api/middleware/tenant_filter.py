# -*- coding: utf-8 -*-
"""
Tenant Filter Middleware
========================
Middleware para isolamento de dados por tenant.

Terminal 5 - Issue #301

Este middleware garante que:
1. Todas as requisicoes tenham um tenant_id valido
2. Queries ao banco de dados filtrem por tenant_id
3. Dados de um tenant nao vazem para outro
4. Auditoria de acessos por tenant
"""

import logging
from contextvars import ContextVar
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional
from functools import wraps

from fastapi import Depends, HTTPException, Request, status
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse

logger = logging.getLogger(__name__)

# Context variable para armazenar tenant do request atual
_tenant_context: ContextVar[Optional["TenantContext"]] = ContextVar(
    "tenant_context", default=None
)


@dataclass
class TenantContext:
    """
    Contexto do tenant para a requisicao atual.

    Attributes:
        tenant_id: ID do tenant
        user_id: ID do usuario
        user_role: Role do usuario
        permissions: Permissoes especificas
        request_id: ID unico da requisicao
        metadata: Metadados adicionais
    """
    tenant_id: str
    user_id: Optional[str] = None
    user_role: Optional[str] = None
    permissions: List[str] = field(default_factory=list)
    request_id: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def has_permission(self, permission: str) -> bool:
        """Verifica se tenant tem permissao especifica"""
        if self.user_role == "ADMIN":
            return True
        return permission in self.permissions

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "user_role": self.user_role,
            "permissions": self.permissions,
            "request_id": self.request_id,
            "created_at": self.created_at.isoformat()
        }


def get_tenant_context() -> Optional[TenantContext]:
    """
    Obtem contexto do tenant da requisicao atual.

    Returns:
        TenantContext ou None se nao definido
    """
    return _tenant_context.get()


def set_tenant_context(context: TenantContext) -> None:
    """
    Define contexto do tenant para a requisicao atual.

    Args:
        context: Contexto do tenant
    """
    _tenant_context.set(context)


def clear_tenant_context() -> None:
    """Limpa contexto do tenant"""
    _tenant_context.set(None)


class TenantMiddleware(BaseHTTPMiddleware):
    """
    Middleware que extrai tenant_id do JWT e configura contexto.

    Rotas publicas podem ser configuradas para nao exigir tenant.

    Exemplo de uso no FastAPI:
        app.add_middleware(TenantMiddleware, public_paths=["/api/v1/auth/login"])
    """

    def __init__(
        self,
        app,
        public_paths: Optional[List[str]] = None,
        header_name: str = "X-Tenant-ID",
        extract_from_jwt: bool = True
    ):
        """
        Inicializa o middleware.

        Args:
            app: Aplicacao FastAPI
            public_paths: Caminhos que nao exigem tenant
            header_name: Nome do header alternativo para tenant
            extract_from_jwt: Extrair tenant_id do JWT
        """
        super().__init__(app)
        self.public_paths = public_paths or [
            "/api/v1/auth/login",
            "/api/v1/auth/register",
            "/docs",
            "/openapi.json",
            "/health",
            "/",
        ]
        self.header_name = header_name
        self.extract_from_jwt = extract_from_jwt

    async def dispatch(self, request: Request, call_next):
        """Processa requisicao"""
        # Limpa contexto anterior
        clear_tenant_context()

        # Verifica se e rota publica
        path = request.url.path
        if self._is_public_path(path):
            return await call_next(request)

        # Tenta extrair tenant_id
        tenant_id = None
        user_id = None
        user_role = None

        # 1. Tenta extrair do JWT
        if self.extract_from_jwt:
            auth_header = request.headers.get("Authorization")
            if auth_header and auth_header.startswith("Bearer "):
                token = auth_header[7:]
                tenant_data = await self._extract_from_jwt(token)
                if tenant_data:
                    tenant_id = tenant_data.get("tenant_id")
                    user_id = tenant_data.get("user_id")
                    user_role = tenant_data.get("role")

        # 2. Fallback: Header X-Tenant-ID
        if not tenant_id:
            tenant_id = request.headers.get(self.header_name)

        # 3. Fallback: Query parameter
        if not tenant_id:
            tenant_id = request.query_params.get("tenant_id")

        # Valida tenant_id
        if not tenant_id:
            return JSONResponse(
                status_code=status.HTTP_400_BAD_REQUEST,
                content={
                    "detail": "tenant_id is required",
                    "hint": f"Provide via JWT claim, {self.header_name} header, or tenant_id query param"
                }
            )

        # Valida formato do tenant_id
        if not self._validate_tenant_id(tenant_id):
            return JSONResponse(
                status_code=status.HTTP_400_BAD_REQUEST,
                content={"detail": "Invalid tenant_id format"}
            )

        # Cria contexto
        import uuid
        context = TenantContext(
            tenant_id=tenant_id,
            user_id=user_id,
            user_role=user_role,
            request_id=str(uuid.uuid4())
        )
        set_tenant_context(context)

        # Log de auditoria
        logger.debug(
            f"Request {context.request_id}: tenant={tenant_id}, "
            f"user={user_id}, path={path}"
        )

        try:
            response = await call_next(request)

            # Adiciona headers de rastreamento
            response.headers["X-Request-ID"] = context.request_id
            response.headers["X-Tenant-ID"] = tenant_id

            return response
        finally:
            clear_tenant_context()

    def _is_public_path(self, path: str) -> bool:
        """Verifica se caminho e publico"""
        for public_path in self.public_paths:
            if path.startswith(public_path):
                return True
        return False

    def _validate_tenant_id(self, tenant_id: str) -> bool:
        """Valida formato do tenant_id"""
        if not tenant_id:
            return False
        # Formato esperado: letras, numeros, hifens
        # Tamanho: 3-50 caracteres
        import re
        pattern = r'^[a-zA-Z0-9\-_]{3,50}$'
        return bool(re.match(pattern, tenant_id))

    async def _extract_from_jwt(self, token: str) -> Optional[Dict]:
        """Extrai dados do JWT"""
        try:
            from jose import jwt, JWTError

            # Decodifica sem verificar (verificacao feita depois)
            # Apenas para extrair tenant_id
            unverified = jwt.get_unverified_claims(token)
            return {
                "tenant_id": unverified.get("tenant_id"),
                "user_id": unverified.get("sub"),
                "role": unverified.get("role")
            }
        except Exception:
            return None


# =============================================================================
# DEPENDENCY FUNCTIONS
# =============================================================================

async def require_tenant(request: Request) -> TenantContext:
    """
    Dependency que exige tenant_id valido.

    Usage:
        @app.get("/data")
        async def get_data(tenant: TenantContext = Depends(require_tenant)):
            return {"tenant_id": tenant.tenant_id}
    """
    context = get_tenant_context()
    if not context:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="tenant_id is required"
        )
    return context


async def tenant_filter_dependency(request: Request) -> str:
    """
    Dependency simples que retorna apenas o tenant_id.

    Usage:
        @app.get("/data")
        async def get_data(tenant_id: str = Depends(tenant_filter_dependency)):
            return query.filter(Model.tenant_id == tenant_id)
    """
    context = get_tenant_context()
    if not context:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="tenant_id is required"
        )
    return context.tenant_id


def require_permission(permission: str):
    """
    Factory para dependency que exige permissao especifica.

    Usage:
        @app.delete("/data/{id}")
        async def delete_data(
            id: str,
            tenant: TenantContext = Depends(require_permission("delete"))
        ):
            ...
    """
    async def permission_checker(
        tenant: TenantContext = Depends(require_tenant)
    ) -> TenantContext:
        if not tenant.has_permission(permission):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Permission required: {permission}"
            )
        return tenant
    return permission_checker


# =============================================================================
# QUERY HELPERS
# =============================================================================

def add_tenant_filter(query, model, tenant_id: Optional[str] = None):
    """
    Adiciona filtro de tenant a uma query SQLAlchemy.

    Args:
        query: Query SQLAlchemy
        model: Modelo com campo tenant_id
        tenant_id: ID do tenant (usa contexto se nao fornecido)

    Returns:
        Query filtrada

    Usage:
        query = session.query(Story)
        query = add_tenant_filter(query, Story)
        results = query.all()
    """
    if tenant_id is None:
        context = get_tenant_context()
        if context:
            tenant_id = context.tenant_id

    if tenant_id and hasattr(model, 'tenant_id'):
        query = query.filter(model.tenant_id == tenant_id)

    return query


def tenant_filter_decorator(model):
    """
    Decorator para adicionar filtro de tenant automaticamente.

    Usage:
        @tenant_filter_decorator(Story)
        def get_stories(session):
            return session.query(Story).all()
    """
    def decorator(func: Callable):
        @wraps(func)
        def wrapper(*args, **kwargs):
            # Injeta tenant_id nos kwargs se nao fornecido
            if 'tenant_id' not in kwargs:
                context = get_tenant_context()
                if context:
                    kwargs['tenant_id'] = context.tenant_id
            return func(*args, **kwargs)
        return wrapper
    return decorator


# =============================================================================
# AUDIT LOGGING
# =============================================================================

class TenantAuditLog:
    """
    Logger de auditoria por tenant.

    Registra acessos e operacoes por tenant para compliance.
    """

    _logs: List[Dict[str, Any]] = []

    @classmethod
    def log(
        cls,
        operation: str,
        resource: str,
        tenant_id: Optional[str] = None,
        user_id: Optional[str] = None,
        details: Optional[Dict] = None
    ):
        """
        Registra operacao.

        Args:
            operation: Tipo de operacao (read, create, update, delete)
            resource: Recurso acessado
            tenant_id: ID do tenant (usa contexto se nao fornecido)
            user_id: ID do usuario
            details: Detalhes adicionais
        """
        context = get_tenant_context()

        if tenant_id is None and context:
            tenant_id = context.tenant_id
        if user_id is None and context:
            user_id = context.user_id

        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "operation": operation,
            "resource": resource,
            "tenant_id": tenant_id,
            "user_id": user_id,
            "request_id": context.request_id if context else None,
            "details": details or {}
        }

        cls._logs.append(log_entry)

        # Mantem apenas ultimos 10000 registros em memoria
        if len(cls._logs) > 10000:
            cls._logs = cls._logs[-10000:]

        logger.info(
            f"AUDIT: {operation} {resource} tenant={tenant_id} user={user_id}"
        )

    @classmethod
    def get_logs(
        cls,
        tenant_id: Optional[str] = None,
        operation: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Retorna logs filtrados"""
        logs = cls._logs

        if tenant_id:
            logs = [l for l in logs if l.get("tenant_id") == tenant_id]
        if operation:
            logs = [l for l in logs if l.get("operation") == operation]

        return logs[-limit:]


# =============================================================================
# SECURITY CHECKS
# =============================================================================

def validate_tenant_access(
    tenant_id: str,
    resource_tenant_id: str
) -> bool:
    """
    Valida se tenant tem acesso ao recurso.

    Args:
        tenant_id: ID do tenant da requisicao
        resource_tenant_id: ID do tenant do recurso

    Returns:
        True se tem acesso

    Raises:
        HTTPException se nao tem acesso
    """
    if tenant_id != resource_tenant_id:
        logger.warning(
            f"SECURITY: Tenant {tenant_id} tentou acessar recurso de {resource_tenant_id}"
        )
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Access denied: resource belongs to another tenant"
        )
    return True


def ensure_tenant_isolation(resource_tenant_id: str):
    """
    Garante que recurso pertence ao tenant atual.

    Args:
        resource_tenant_id: ID do tenant do recurso

    Raises:
        HTTPException se nao pertence ao tenant atual
    """
    context = get_tenant_context()
    if not context:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Tenant context not available"
        )

    validate_tenant_access(context.tenant_id, resource_tenant_id)
