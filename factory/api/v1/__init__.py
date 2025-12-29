"""
API v1 - Fabrica de Agentes
===========================

API Foundation implementando:
- Issue #114: API-First Design Pattern com routers por dominio
- Issue #107: Paginacao baseada em cursor
- Issue #108: Idempotencia em endpoints de criacao
- Issue #109: OAuth 2.0 para API

Estrutura de dominios:
- /api/v1/core/ - Stories, Tasks, Projects (CRUD principal)
- /api/v1/analytics/ - Metricas, KPIs, relatorios
- /api/v1/admin/ - Administracao (users, roles, config)
- /api/v1/integrations/ - SAP, Salesforce, GitHub
- /api/v1/webhooks/ - Eventos e callbacks
"""

from fastapi import APIRouter

from .schemas import APIResponse, PaginationMeta, CursorPaginationParams
from .core_routes import router as core_router
from .analytics_routes import router as analytics_router
from .admin_routes import router as admin_router
from .integrations_routes import router as integrations_router
from .webhooks_routes import router as webhooks_router

# Router principal v1
router = APIRouter(prefix="/api/v1", tags=["API v1"])

# Incluir routers de dominio
router.include_router(core_router, prefix="/core", tags=["Core"])
router.include_router(analytics_router, prefix="/analytics", tags=["Analytics"])
router.include_router(admin_router, prefix="/admin", tags=["Admin"])
router.include_router(integrations_router, prefix="/integrations", tags=["Integrations"])
router.include_router(webhooks_router, prefix="/webhooks", tags=["Webhooks"])


@router.get("/", tags=["API v1"])
async def api_v1_info():
    """
    Informacoes da API v1
    """
    return APIResponse(
        data={
            "version": "1.0.0",
            "name": "Fabrica de Agentes API",
            "description": "API REST para gestao de projetos, stories e desenvolvimento autonomo",
            "domains": {
                "core": "/api/v1/core - CRUD de Stories, Tasks, Projects",
                "analytics": "/api/v1/analytics - Metricas e KPIs",
                "admin": "/api/v1/admin - Administracao de usuarios e roles",
                "integrations": "/api/v1/integrations - Integracoes externas",
                "webhooks": "/api/v1/webhooks - Eventos e callbacks"
            },
            "features": {
                "pagination": "Cursor-based pagination para grandes datasets",
                "idempotency": "Header Idempotency-Key para operacoes seguras",
                "oauth2": "OAuth 2.0 com Authorization Code e Client Credentials"
            }
        },
        meta={
            "api_version": "v1",
            "spec": "OpenAPI 3.0"
        }
    ).model_dump()


__all__ = [
    "router",
    "APIResponse",
    "PaginationMeta",
    "CursorPaginationParams",
    "core_router",
    "analytics_router",
    "admin_router",
    "integrations_router",
    "webhooks_router"
]
