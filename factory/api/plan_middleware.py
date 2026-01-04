# -*- coding: utf-8 -*-
"""
Middleware de Verificação de Plano (Issue #104)
================================================

Middleware para verificar limites de plano antes de processar requests.
Aplicado automaticamente a rotas que criam recursos.

Autor: Plataforma E
"""

import logging
from typing import Optional, Dict, Any, List, Callable
from functools import wraps

from fastapi import Request, HTTPException, Depends
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from ..core.multi_tenant import get_current_tenant_id, get_current_tenant
from ..billing.plan_limits import (
    PlanEnforcer, LimitExceededError, ResourceType
)

# Configurar logging
logger = logging.getLogger(__name__)


# =============================================================================
# MAPEAMENTO DE ROTAS PARA RECURSOS
# =============================================================================

# Rotas POST que criam recursos e seus tipos correspondentes
ROUTE_RESOURCE_MAP = {
    "/api/projects": ResourceType.PROJECTS.value,
    "/api/v1/projects": ResourceType.PROJECTS.value,
    "/api/stories": ResourceType.STORIES.value,
    "/api/v1/stories": ResourceType.STORIES.value,
    "/api/users/invite": ResourceType.USERS.value,
    "/api/v1/users/invite": ResourceType.USERS.value,
    "/api/tenants/members": ResourceType.USERS.value,
    "/api/v1/tenants/members": ResourceType.USERS.value,
}


# =============================================================================
# PLAN CHECK MIDDLEWARE
# =============================================================================

class PlanCheckMiddleware(BaseHTTPMiddleware):
    """
    Middleware que verifica limites de plano antes de criar recursos.

    Funcionalidade:
    - Intercepta requests POST em rotas de criação
    - Verifica se tenant tem quota disponível
    - Bloqueia request se limite excedido
    - Permite customização de rotas verificadas

    Uso:
        from factory.api.plan_middleware import PlanCheckMiddleware

        app.add_middleware(PlanCheckMiddleware)
    """

    def __init__(
        self,
        app,
        route_resource_map: Dict[str, str] = None,
        exempt_paths: List[str] = None
    ):
        """
        Inicializa o middleware.

        Args:
            app: Aplicação FastAPI
            route_resource_map: Mapeamento customizado de rotas para recursos
            exempt_paths: Rotas que não precisam de verificação
        """
        super().__init__(app)
        self.route_resource_map = route_resource_map or ROUTE_RESOURCE_MAP
        self.exempt_paths = exempt_paths or [
            "/health",
            "/docs",
            "/openapi.json",
            "/api/auth",
            "/api/public"
        ]

    async def dispatch(self, request: Request, call_next):
        """Processa request verificando limites de plano."""

        # Apenas verificar POST requests
        if request.method != "POST":
            return await call_next(request)

        path = request.url.path

        # Verificar paths isentos
        if self._is_exempt_path(path):
            return await call_next(request)

        # Verificar se rota está mapeada
        resource_type = self._get_resource_type(path)
        if not resource_type:
            return await call_next(request)

        # Obter tenant_id do contexto
        tenant_id = get_current_tenant_id()
        if not tenant_id:
            # Sem tenant, deixar passar (autenticação vai bloquear)
            return await call_next(request)

        # Verificar limite
        try:
            from ..database.connection import SessionLocal

            db = SessionLocal()
            try:
                enforcer = PlanEnforcer(db)
                enforcer.enforce(tenant_id, resource_type)
            finally:
                db.close()

        except LimitExceededError as e:
            logger.warning(
                f"Limite excedido: tenant={tenant_id}, "
                f"resource={resource_type}, current={e.current}, limit={e.limit}"
            )
            return JSONResponse(
                status_code=403,
                content={
                    "detail": e.message,
                    "code": "LIMIT_EXCEEDED",
                    "resource_type": e.resource_type,
                    "current": e.current,
                    "limit": e.limit,
                    "upgrade_url": "/billing/upgrade"
                },
                headers={
                    "X-Limit-Exceeded": resource_type,
                    "X-Current-Usage": str(e.current),
                    "X-Limit": str(e.limit)
                }
            )

        except Exception as e:
            logger.error(f"Erro ao verificar limite: {e}")
            # Em caso de erro, permitir (fail open)
            pass

        return await call_next(request)

    def _is_exempt_path(self, path: str) -> bool:
        """Verifica se path está isento de verificação."""
        for exempt in self.exempt_paths:
            if path.startswith(exempt):
                return True
        return False

    def _get_resource_type(self, path: str) -> Optional[str]:
        """Obtém tipo de recurso para a rota."""
        for route, resource in self.route_resource_map.items():
            if path.startswith(route):
                return resource
        return None


# =============================================================================
# DEPENDENCY HELPERS
# =============================================================================

async def verify_project_limit(request: Request):
    """
    Dependency que verifica limite de projetos.

    Uso:
        @router.post("/projects")
        async def create_project(
            _: None = Depends(verify_project_limit)
        ):
            ...
    """
    await _verify_limit(ResourceType.PROJECTS.value)


async def verify_story_limit(request: Request):
    """
    Dependency que verifica limite de stories.

    Uso:
        @router.post("/stories")
        async def create_story(
            _: None = Depends(verify_story_limit)
        ):
            ...
    """
    await _verify_limit(ResourceType.STORIES.value)


async def verify_user_limit(request: Request):
    """
    Dependency que verifica limite de usuários.

    Uso:
        @router.post("/users/invite")
        async def invite_user(
            _: None = Depends(verify_user_limit)
        ):
            ...
    """
    await _verify_limit(ResourceType.USERS.value)


async def verify_token_limit(tokens_needed: int = 1000):
    """
    Dependency que verifica limite de tokens LLM.

    Uso:
        @router.post("/ai/generate")
        async def generate(
            _: None = Depends(lambda: verify_token_limit(5000))
        ):
            ...
    """
    await _verify_limit(ResourceType.TOKENS_PER_MONTH.value, tokens_needed)


async def _verify_limit(resource_type: str, increment: int = 1):
    """Função auxiliar para verificar limite."""
    tenant_id = get_current_tenant_id()
    if not tenant_id:
        raise HTTPException(
            status_code=401,
            detail="Tenant não identificado"
        )

    from ..database.connection import SessionLocal

    db = SessionLocal()
    try:
        enforcer = PlanEnforcer(db)
        enforcer.enforce(tenant_id, resource_type, increment)
    except LimitExceededError as e:
        raise HTTPException(
            status_code=403,
            detail=e.message,
            headers={
                "X-Limit-Exceeded": resource_type,
                "X-Current-Usage": str(e.current),
                "X-Limit": str(e.limit)
            }
        )
    finally:
        db.close()


# =============================================================================
# RESPONSE HEADERS
# =============================================================================

async def add_usage_headers(request: Request, call_next):
    """
    Middleware que adiciona headers com informações de uso.

    Headers adicionados:
    - X-Usage-Projects: 5/10
    - X-Usage-Stories: 50/100
    - X-Plan-Type: pro
    """
    response = await call_next(request)

    tenant_id = get_current_tenant_id()
    if tenant_id:
        try:
            from ..database.connection import SessionLocal

            db = SessionLocal()
            try:
                enforcer = PlanEnforcer(db)
                plan_type, limits = enforcer.get_tenant_plan(tenant_id)

                # Adicionar headers
                response.headers["X-Plan-Type"] = plan_type

                # Adicionar uso de projetos
                projects_current = enforcer.get_current_count(tenant_id, "projects")
                projects_limit = limits.get("projects", 0)
                if projects_limit != -1:
                    response.headers["X-Usage-Projects"] = f"{projects_current}/{projects_limit}"

                # Adicionar uso de stories
                stories_current = enforcer.get_current_count(tenant_id, "stories")
                stories_limit = limits.get("stories", 0)
                if stories_limit != -1:
                    response.headers["X-Usage-Stories"] = f"{stories_current}/{stories_limit}"

            finally:
                db.close()

        except Exception as e:
            logger.debug(f"Erro ao adicionar headers de uso: {e}")

    return response


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "PlanCheckMiddleware",
    "verify_project_limit",
    "verify_story_limit",
    "verify_user_limit",
    "verify_token_limit",
    "add_usage_headers",
    "ROUTE_RESOURCE_MAP",
]
