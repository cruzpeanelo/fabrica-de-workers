# -*- coding: utf-8 -*-
"""
Middleware de Isolamento Multi-Tenant
======================================

Implementa:
- Deteccao automatica do tenant (subdominio, header, token)
- Injecao do tenant no contexto da requisicao
- Filtros automaticos de query por tenant
- Protecao de acesso entre tenants

Autor: Fabrica de Agentes
"""

import logging
from typing import Optional, Callable, Any
from contextvars import ContextVar
from functools import wraps

from fastapi import Request, HTTPException, Depends
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware
from sqlalchemy.orm import Session

from .models import Tenant, TenantStatus

# Configurar logging
logger = logging.getLogger(__name__)

# Context variable para armazenar o tenant atual
_current_tenant: ContextVar[Optional[Tenant]] = ContextVar("current_tenant", default=None)
_current_tenant_id: ContextVar[Optional[str]] = ContextVar("current_tenant_id", default=None)


def get_current_tenant() -> Optional[Tenant]:
    """
    Obtem o tenant atual do contexto.

    Returns:
        Tenant atual ou None
    """
    return _current_tenant.get()


def get_current_tenant_id() -> Optional[str]:
    """
    Obtem o ID do tenant atual do contexto.

    Returns:
        tenant_id ou None
    """
    return _current_tenant_id.get()


def set_current_tenant(tenant: Optional[Tenant]) -> None:
    """
    Define o tenant atual no contexto.

    Args:
        tenant: Tenant a definir
    """
    _current_tenant.set(tenant)
    _current_tenant_id.set(tenant.tenant_id if tenant else None)


def require_tenant(func: Callable) -> Callable:
    """
    Decorator que exige que um tenant esteja definido.

    Uso:
        @require_tenant
        async def my_endpoint():
            tenant = get_current_tenant()
            ...
    """
    @wraps(func)
    async def wrapper(*args, **kwargs):
        tenant = get_current_tenant()
        if not tenant:
            raise HTTPException(
                status_code=401,
                detail="Tenant nao identificado. Verifique o subdominio ou header X-Tenant-ID."
            )
        if tenant.status == TenantStatus.SUSPENDED.value:
            raise HTTPException(
                status_code=403,
                detail="Conta suspensa. Entre em contato com o suporte."
            )
        if not tenant.is_active:
            raise HTTPException(
                status_code=403,
                detail="Conta inativa. Entre em contato com o suporte."
            )
        return await func(*args, **kwargs)
    return wrapper


class TenantMiddleware(BaseHTTPMiddleware):
    """
    Middleware para deteccao e validacao de tenant.

    Metodos de deteccao (em ordem de prioridade):
    1. Header X-Tenant-ID
    2. Subdominio (empresa.fabricadeagentes.com)
    3. Path parameter (/api/v1/tenants/{tenant_id}/...)
    4. Token JWT (claim tenant_id)
    """

    def __init__(
        self,
        app,
        get_db: Callable,
        base_domain: str = "fabricadeagentes.com",
        exempt_paths: Optional[list] = None,
        require_tenant: bool = True
    ):
        """
        Inicializa o middleware.

        Args:
            app: Aplicacao FastAPI
            get_db: Funcao para obter sessao do banco
            base_domain: Dominio base para deteccao de subdominio
            exempt_paths: Paths que nao requerem tenant
            require_tenant: Se True, bloqueia requisicoes sem tenant
        """
        super().__init__(app)
        self.get_db = get_db
        self.base_domain = base_domain
        self.require_tenant = require_tenant
        self.exempt_paths = exempt_paths or [
            "/",
            "/health",
            "/docs",
            "/openapi.json",
            "/redoc",
            "/api/auth/login",
            "/api/auth/register",
            "/api/public",
            "/api/webhooks/stripe",
            "/api/plans",
        ]

    async def dispatch(self, request: Request, call_next) -> Any:
        """Processa a requisicao identificando o tenant."""
        # Verificar paths isentos
        path = request.url.path
        if self._is_exempt_path(path):
            return await call_next(request)

        # Tentar identificar tenant
        tenant = await self._identify_tenant(request)

        if tenant:
            # Definir no contexto
            set_current_tenant(tenant)
            logger.debug(f"Tenant identificado: {tenant.tenant_id}")

            # Verificar status
            if tenant.status == TenantStatus.SUSPENDED.value:
                return JSONResponse(
                    status_code=403,
                    content={
                        "detail": "Conta suspensa",
                        "code": "TENANT_SUSPENDED",
                        "tenant_id": tenant.tenant_id
                    }
                )

            if not tenant.is_active:
                return JSONResponse(
                    status_code=403,
                    content={
                        "detail": "Conta inativa",
                        "code": "TENANT_INACTIVE",
                        "tenant_id": tenant.tenant_id
                    }
                )

        elif self.require_tenant:
            # Tenant nao encontrado e e obrigatorio
            return JSONResponse(
                status_code=401,
                content={
                    "detail": "Tenant nao identificado",
                    "code": "TENANT_NOT_FOUND",
                    "hint": "Inclua o header X-Tenant-ID ou use um subdominio valido"
                }
            )

        try:
            response = await call_next(request)
            return response
        finally:
            # Limpar contexto
            set_current_tenant(None)

    def _is_exempt_path(self, path: str) -> bool:
        """Verifica se o path esta isento de verificacao de tenant."""
        for exempt in self.exempt_paths:
            if path == exempt or path.startswith(exempt):
                return True
        return False

    async def _identify_tenant(self, request: Request) -> Optional[Tenant]:
        """
        Identifica o tenant da requisicao.

        Args:
            request: Requisicao FastAPI

        Returns:
            Tenant ou None
        """
        # 1. Header X-Tenant-ID
        tenant_id = request.headers.get("X-Tenant-ID")
        if tenant_id:
            tenant = await self._get_tenant_by_id(tenant_id)
            if tenant:
                return tenant

        # 2. Subdominio
        host = request.headers.get("host", "")
        slug = self._extract_slug_from_host(host)
        if slug:
            tenant = await self._get_tenant_by_slug(slug)
            if tenant:
                return tenant

        # 3. Path parameter
        tenant_id = self._extract_tenant_from_path(request.url.path)
        if tenant_id:
            tenant = await self._get_tenant_by_id(tenant_id)
            if tenant:
                return tenant

        # 4. Token JWT (se disponivel)
        tenant_id = self._extract_tenant_from_token(request)
        if tenant_id:
            tenant = await self._get_tenant_by_id(tenant_id)
            if tenant:
                return tenant

        return None

    def _extract_slug_from_host(self, host: str) -> Optional[str]:
        """
        Extrai slug do subdominio.

        Exemplo: empresa.fabricadeagentes.com -> empresa
        """
        if not host:
            return None

        # Remover porta
        host = host.split(":")[0]

        # Verificar se e o dominio base
        if not host.endswith(f".{self.base_domain}"):
            # Verificar localhost/desenvolvimento
            if host in ["localhost", "127.0.0.1"]:
                return None
            # Pode ser IP ou outro dominio
            return None

        # Extrair subdominio
        slug = host.replace(f".{self.base_domain}", "")

        # Ignorar subdominios padrao
        if slug in ["www", "api", "app", "admin"]:
            return None

        return slug if slug else None

    def _extract_tenant_from_path(self, path: str) -> Optional[str]:
        """
        Extrai tenant_id do path.

        Exemplo: /api/v1/tenants/TEN-123/projects -> TEN-123
        """
        parts = path.split("/")
        for i, part in enumerate(parts):
            if part == "tenants" and i + 1 < len(parts):
                potential_id = parts[i + 1]
                if potential_id.startswith("TEN-"):
                    return potential_id
        return None

    def _extract_tenant_from_token(self, request: Request) -> Optional[str]:
        """
        Extrai tenant_id do token JWT.

        Args:
            request: Requisicao com header Authorization

        Returns:
            tenant_id ou None
        """
        auth_header = request.headers.get("Authorization", "")
        if not auth_header.startswith("Bearer "):
            return None

        token = auth_header.replace("Bearer ", "")

        try:
            import jwt
            from ..config import JWT_SECRET_KEY, JWT_ALGORITHM

            payload = jwt.decode(
                token,
                JWT_SECRET_KEY,
                algorithms=[JWT_ALGORITHM]
            )
            return payload.get("tenant_id")

        except Exception:
            return None

    async def _get_tenant_by_id(self, tenant_id: str) -> Optional[Tenant]:
        """Busca tenant por ID."""
        try:
            db = next(self.get_db())
            tenant = db.query(Tenant).filter(
                Tenant.tenant_id == tenant_id
            ).first()
            return tenant
        except Exception as e:
            logger.error(f"Erro ao buscar tenant por ID: {e}")
            return None

    async def _get_tenant_by_slug(self, slug: str) -> Optional[Tenant]:
        """Busca tenant por slug."""
        try:
            db = next(self.get_db())
            tenant = db.query(Tenant).filter(
                Tenant.slug == slug
            ).first()
            return tenant
        except Exception as e:
            logger.error(f"Erro ao buscar tenant por slug: {e}")
            return None


class TenantQueryFilter:
    """
    Mixin para adicionar filtro automatico de tenant em queries.

    Uso com SQLAlchemy:
        class ProjectRepository(TenantQueryFilter):
            def list_projects(self, tenant_id: str):
                query = self.db.query(Project)
                query = self.filter_by_tenant(query, Project, tenant_id)
                return query.all()
    """

    def filter_by_tenant(self, query, model, tenant_id: Optional[str] = None):
        """
        Adiciona filtro de tenant na query.

        Args:
            query: Query SQLAlchemy
            model: Modelo com campo tenant_id
            tenant_id: ID do tenant (usa contexto se None)

        Returns:
            Query filtrada
        """
        # Usar tenant do contexto se nao especificado
        if tenant_id is None:
            tenant_id = get_current_tenant_id()

        if tenant_id is None:
            raise HTTPException(
                status_code=401,
                detail="Tenant nao identificado"
            )

        # Verificar se modelo tem tenant_id
        if hasattr(model, "tenant_id"):
            query = query.filter(model.tenant_id == tenant_id)

        return query

    def validate_tenant_access(self, resource, tenant_id: Optional[str] = None):
        """
        Valida se o recurso pertence ao tenant atual.

        Args:
            resource: Objeto com campo tenant_id
            tenant_id: ID do tenant (usa contexto se None)

        Raises:
            HTTPException se acesso negado
        """
        if tenant_id is None:
            tenant_id = get_current_tenant_id()

        if hasattr(resource, "tenant_id"):
            if resource.tenant_id != tenant_id:
                logger.warning(
                    f"Tentativa de acesso cross-tenant: "
                    f"recurso={resource.tenant_id}, requisicao={tenant_id}"
                )
                raise HTTPException(
                    status_code=403,
                    detail="Acesso negado a este recurso"
                )


# Dependency para FastAPI
async def get_tenant_dependency(request: Request) -> Tenant:
    """
    Dependency do FastAPI para obter tenant atual.

    Uso:
        @router.get("/items")
        async def list_items(tenant: Tenant = Depends(get_tenant_dependency)):
            ...
    """
    tenant = get_current_tenant()
    if not tenant:
        raise HTTPException(
            status_code=401,
            detail="Tenant nao identificado"
        )
    return tenant


async def get_optional_tenant_dependency(request: Request) -> Optional[Tenant]:
    """
    Dependency do FastAPI para obter tenant atual (opcional).

    Uso:
        @router.get("/public")
        async def public_endpoint(tenant: Optional[Tenant] = Depends(get_optional_tenant_dependency)):
            if tenant:
                # Logica com tenant
            else:
                # Logica publica
    """
    return get_current_tenant()
