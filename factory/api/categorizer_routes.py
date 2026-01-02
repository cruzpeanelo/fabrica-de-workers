# -*- coding: utf-8 -*-
"""
Story Categorizer API Routes (Issue #246)
=========================================

Endpoints para auto-categorização de stories com NLP.
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
import logging

from factory.ai.categorizer import (
    categorize_story,
    suggest_story_type,
    suggest_story_labels,
    get_categorizer,
    StoryType
)

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/ai/categorize", tags=["AI Categorization"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class CategorizeRequest(BaseModel):
    """Request para categorização de story."""
    story_id: str = Field(..., description="ID da story")
    title: str = Field(..., description="Título da story")
    description: Optional[str] = Field("", description="Descrição da story")


class CategorizeResponse(BaseModel):
    """Response da categorização."""
    story_id: str
    title: str
    suggested_type: str
    type_confidence: float
    type_reason: str
    suggested_labels: List[Dict[str, Any]]
    epic_suggestion: Optional[Dict[str, Any]]
    similar_stories: List[Dict[str, Any]]


class TypeSuggestionRequest(BaseModel):
    """Request para sugestão de tipo."""
    title: str = Field(..., description="Título da story")
    description: Optional[str] = Field("", description="Descrição")


class TypeSuggestionResponse(BaseModel):
    """Response de sugestão de tipo."""
    type: str
    confidence: float
    reason: str


class LabelSuggestionResponse(BaseModel):
    """Response de sugestão de labels."""
    labels: List[Dict[str, Any]]


class BatchCategorizeRequest(BaseModel):
    """Request para categorização em lote."""
    stories: List[CategorizeRequest]


class BatchCategorizeResponse(BaseModel):
    """Response de categorização em lote."""
    results: List[CategorizeResponse]
    total: int
    processed: int


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/story", response_model=CategorizeResponse)
async def categorize_single_story(request: CategorizeRequest):
    """
    Categoriza uma story automaticamente.

    Retorna:
    - Tipo sugerido (feature, bug, tech-debt, etc)
    - Labels sugeridas
    - Épico sugerido
    - Stories similares
    """
    try:
        result = categorize_story(
            story_id=request.story_id,
            title=request.title,
            description=request.description or ""
        )

        return CategorizeResponse(
            story_id=result.story_id,
            title=result.title,
            suggested_type=result.suggested_type.value,
            type_confidence=result.type_confidence,
            type_reason=result.type_reason,
            suggested_labels=[
                {"label": l.label, "confidence": l.confidence}
                for l in result.suggested_labels
            ],
            epic_suggestion={
                "epic_id": result.epic_suggestion.epic_id,
                "epic_title": result.epic_suggestion.epic_title,
                "confidence": result.epic_suggestion.confidence,
                "reason": result.epic_suggestion.reason
            } if result.epic_suggestion else None,
            similar_stories=result.similar_stories
        )

    except Exception as e:
        logger.error(f"[Categorizer API] Error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/type", response_model=TypeSuggestionResponse)
async def suggest_type(request: TypeSuggestionRequest):
    """
    Sugere o tipo de uma story baseado no título e descrição.

    Tipos possíveis:
    - feature: Nova funcionalidade
    - bug: Correção de erro
    - tech-debt: Dívida técnica
    - docs: Documentação
    - improvement: Melhoria
    - refactor: Refatoração
    - test: Testes
    """
    try:
        result = suggest_story_type(
            title=request.title,
            description=request.description or ""
        )

        return TypeSuggestionResponse(
            type=result["type"],
            confidence=result["confidence"],
            reason=result["reason"]
        )

    except Exception as e:
        logger.error(f"[Categorizer API] Type suggestion error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/labels", response_model=LabelSuggestionResponse)
async def suggest_labels(request: TypeSuggestionRequest):
    """
    Sugere labels para uma story.

    Labels possíveis:
    - frontend, backend, security, performance
    - mobile, database, integration, ai, urgent
    """
    try:
        labels = suggest_story_labels(
            title=request.title,
            description=request.description or ""
        )

        return LabelSuggestionResponse(labels=labels)

    except Exception as e:
        logger.error(f"[Categorizer API] Labels suggestion error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/batch", response_model=BatchCategorizeResponse)
async def categorize_batch(request: BatchCategorizeRequest):
    """
    Categoriza múltiplas stories em lote.

    Útil para processar backlog existente.
    """
    try:
        categorizer = get_categorizer()

        stories_data = [
            {
                "story_id": s.story_id,
                "title": s.title,
                "description": s.description or ""
            }
            for s in request.stories
        ]

        results = categorizer.categorize_batch(stories_data)

        response_results = []
        for result in results:
            response_results.append(CategorizeResponse(
                story_id=result.story_id,
                title=result.title,
                suggested_type=result.suggested_type.value,
                type_confidence=result.type_confidence,
                type_reason=result.type_reason,
                suggested_labels=[
                    {"label": l.label, "confidence": l.confidence}
                    for l in result.suggested_labels
                ],
                epic_suggestion={
                    "epic_id": result.epic_suggestion.epic_id,
                    "epic_title": result.epic_suggestion.epic_title,
                    "confidence": result.epic_suggestion.confidence,
                    "reason": result.epic_suggestion.reason
                } if result.epic_suggestion else None,
                similar_stories=result.similar_stories
            ))

        return BatchCategorizeResponse(
            results=response_results,
            total=len(request.stories),
            processed=len(response_results)
        )

    except Exception as e:
        logger.error(f"[Categorizer API] Batch error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/types")
async def get_available_types():
    """
    Retorna os tipos de story disponíveis.
    """
    return {
        "types": [
            {"value": t.value, "name": t.name}
            for t in StoryType
        ]
    }


# =============================================================================
# REGISTRO
# =============================================================================

def register_categorizer_routes(app):
    """Registra rotas de categorização no app."""
    app.include_router(router)
    logger.info("[AI] Categorizer routes registered: /api/ai/categorize/*")
