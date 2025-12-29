# -*- coding: utf-8 -*-
"""
Marketplace API Routes - Fabrica de Agentes
=============================================

Endpoints da API REST para o Marketplace de Templates e Skills.

Uso:
    from factory.api.marketplace_routes import router as marketplace_router
    app.include_router(marketplace_router, prefix="/api/marketplace")
"""

from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

# Import do Marketplace
try:
    from factory.core.marketplace import (
        get_marketplace, search_marketplace, get_marketplace_item,
        install_marketplace_item, apply_template, MarketplaceCategory
    )
    HAS_MARKETPLACE = True
except ImportError:
    HAS_MARKETPLACE = False
    print("[Marketplace Routes] Marketplace module not available")


# Router
router = APIRouter(tags=["Marketplace"])


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class MarketplaceInstallRequest(BaseModel):
    """Request para instalar item do marketplace"""
    user_id: Optional[str] = "anonymous"
    project_id: Optional[str] = None


class MarketplaceReviewRequest(BaseModel):
    """Request para adicionar review"""
    user_id: str
    rating: int
    comment: Optional[str] = ""


class ApplyTemplateRequest(BaseModel):
    """Request para aplicar template"""
    project_id: str
    variables: Optional[Dict[str, str]] = {}


class PublishTemplateRequest(BaseModel):
    """Request para publicar template"""
    name: str
    description: str
    author: str
    template_data: Dict[str, Any]


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/search")
def search(
    query: str = "",
    category: Optional[str] = None,
    tags: Optional[str] = None,
    verified_only: bool = False,
    featured_only: bool = False,
    sort_by: str = "downloads",
    limit: int = 20
):
    """
    Busca itens no marketplace.

    Parametros:
    - query: Texto para busca
    - category: Filtrar por categoria (project_template, story_template, skill, agent_config)
    - tags: Tags separadas por virgula
    - verified_only: Apenas itens verificados
    - featured_only: Apenas itens em destaque
    - sort_by: Ordenar por (downloads, rating, recent, name)
    - limit: Limite de resultados
    """
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    tags_list = tags.split(",") if tags else None

    results = search_marketplace(
        query=query,
        category=category,
        tags=tags_list,
        verified_only=verified_only,
        featured_only=featured_only,
        sort_by=sort_by,
        limit=limit
    )

    return {"items": results, "total": len(results)}


@router.get("/categories")
def categories():
    """Lista categorias disponiveis no marketplace"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    return {
        "categories": [
            {
                "id": "project_template",
                "name": "Templates de Projeto",
                "icon": "folder",
                "description": "Starter kits e arquiteturas completas"
            },
            {
                "id": "story_template",
                "name": "Templates de Stories",
                "icon": "file-text",
                "description": "User stories pre-definidas para features comuns"
            },
            {
                "id": "skill",
                "name": "Skills",
                "icon": "cpu",
                "description": "Skills reutilizaveis para agentes"
            },
            {
                "id": "agent_config",
                "name": "Configuracoes de Agentes",
                "icon": "user",
                "description": "Perfis de agentes especializados"
            }
        ]
    }


@router.get("/featured")
def featured():
    """Lista itens em destaque"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    return {"items": marketplace.get_featured()}


@router.get("/popular")
def popular(limit: int = 10):
    """Lista itens mais populares"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    return {"items": marketplace.get_popular(limit)}


@router.get("/statistics")
def statistics():
    """Estatisticas do marketplace"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    return marketplace.get_statistics()


@router.get("/items/{item_id}")
def get_item(item_id: str):
    """Busca item por ID com reviews"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    item = get_marketplace_item(item_id)
    if not item:
        raise HTTPException(404, "Item not found")

    marketplace = get_marketplace()
    reviews = marketplace.get_reviews(item_id)

    return {
        "item": item,
        "reviews": reviews
    }


@router.post("/items/{item_id}/install")
def install_item(item_id: str, request: MarketplaceInstallRequest):
    """Instala/baixa um item do marketplace"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    item = install_marketplace_item(item_id, request.user_id)
    if not item:
        raise HTTPException(404, "Item not found")

    return {
        "success": True,
        "message": f"Item '{item.get('name')}' instalado com sucesso",
        "item": item
    }


@router.post("/items/{item_id}/review")
def add_review(item_id: str, request: MarketplaceReviewRequest):
    """Adiciona review a um item"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    if not 1 <= request.rating <= 5:
        raise HTTPException(400, "Rating must be between 1 and 5")

    marketplace = get_marketplace()
    review = marketplace.add_review(
        item_id=item_id,
        user_id=request.user_id,
        rating=request.rating,
        comment=request.comment
    )

    if not review:
        raise HTTPException(404, "Item not found or invalid review")

    return {
        "success": True,
        "review": review.to_dict()
    }


@router.get("/items/{item_id}/reviews")
def get_reviews(item_id: str):
    """Lista reviews de um item"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    return {"reviews": marketplace.get_reviews(item_id)}


@router.post("/templates/{template_id}/apply")
def apply_template_endpoint(template_id: str, request: ApplyTemplateRequest):
    """
    Aplica um template de story gerando dados para criacao.

    Retorna os dados preenchidos da story pronta para criacao.
    """
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    result = apply_template(template_id, request.project_id, request.variables)

    if "error" in result:
        raise HTTPException(404, result["error"])

    return {
        "success": True,
        "story_data": result
    }


@router.post("/publish/story-template")
def publish_story_template(request: PublishTemplateRequest):
    """
    Publica um novo template de story no marketplace.

    O template ficara disponivel para outros usuarios apos moderacao.
    """
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    template = marketplace.publish_story_template(
        name=request.name,
        description=request.description,
        author=request.author,
        template_data=request.template_data
    )

    return {
        "success": True,
        "message": f"Template '{template.name}' publicado com sucesso",
        "item": template.to_dict()
    }


@router.get("/user/{user_id}/downloads")
def user_downloads(user_id: str):
    """Lista itens baixados por um usuario"""
    if not HAS_MARKETPLACE:
        raise HTTPException(503, "Marketplace module not available")

    marketplace = get_marketplace()
    return {"items": marketplace.get_user_downloads(user_id)}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def check_marketplace_available():
    """Verifica se o marketplace esta disponivel"""
    return HAS_MARKETPLACE
