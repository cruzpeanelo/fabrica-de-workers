# -*- coding: utf-8 -*-
"""
OKR API Routes - Issue #281
===========================

API endpoints para gerenciamento de OKRs (Objectives and Key Results).
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import List, Optional
from datetime import datetime

from factory.database.connection import SessionLocal
from factory.database.models import Objective, KeyResult, OKRStatus, OKRPeriod
from factory.core.okr_manager import OKRManager


router = APIRouter(prefix="/api", tags=["OKRs"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class ObjectiveCreate(BaseModel):
    """Request para criar Objective"""
    title: str = Field(..., min_length=1, max_length=300)
    description: Optional[str] = None
    period: Optional[str] = OKRPeriod.QUARTERLY.value
    start_date: Optional[datetime] = None
    end_date: Optional[datetime] = None
    owner_id: Optional[str] = None
    owner_name: Optional[str] = None
    parent_objective_id: Optional[str] = None
    category: Optional[str] = None
    tags: Optional[List[str]] = None


class ObjectiveUpdate(BaseModel):
    """Request para atualizar Objective"""
    title: Optional[str] = Field(None, min_length=1, max_length=300)
    description: Optional[str] = None
    period: Optional[str] = None
    start_date: Optional[datetime] = None
    end_date: Optional[datetime] = None
    owner_id: Optional[str] = None
    owner_name: Optional[str] = None
    status: Optional[str] = None
    parent_objective_id: Optional[str] = None
    category: Optional[str] = None
    tags: Optional[List[str]] = None


class KeyResultCreate(BaseModel):
    """Request para criar Key Result"""
    title: str = Field(..., min_length=1, max_length=300)
    target: float = Field(..., gt=0)
    description: Optional[str] = None
    initial: Optional[float] = 0.0
    unit: Optional[str] = "units"
    metric_type: Optional[str] = "increase"  # increase, decrease, maintain
    weight: Optional[float] = 1.0
    owner_id: Optional[str] = None
    owner_name: Optional[str] = None


class KeyResultUpdate(BaseModel):
    """Request para atualizar Key Result"""
    title: Optional[str] = Field(None, min_length=1, max_length=300)
    description: Optional[str] = None
    target: Optional[float] = None
    initial: Optional[float] = None
    unit: Optional[str] = None
    metric_type: Optional[str] = None
    weight: Optional[float] = None
    status: Optional[str] = None
    owner_id: Optional[str] = None
    owner_name: Optional[str] = None


class KeyResultValueUpdate(BaseModel):
    """Request para atualizar valor do Key Result"""
    value: float
    note: Optional[str] = None


class StoryLinkRequest(BaseModel):
    """Request para vincular story"""
    story_id: str
    contribution: Optional[float] = 1.0
    notes: Optional[str] = None


class ProjectLinkRequest(BaseModel):
    """Request para vincular projeto"""
    project_id: str
    contribution: Optional[float] = 1.0
    notes: Optional[str] = None


# =============================================================================
# OBJECTIVES ENDPOINTS
# =============================================================================

@router.post("/objectives")
def create_objective(request: ObjectiveCreate, tenant_id: Optional[str] = None):
    """
    Cria um novo Objective.

    Args:
        request: Dados do objetivo
        tenant_id: ID do tenant (opcional)

    Returns:
        Dados do objetivo criado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        objective = manager.create_objective(
            title=request.title,
            description=request.description,
            period=request.period,
            start_date=request.start_date,
            end_date=request.end_date,
            owner_id=request.owner_id,
            owner_name=request.owner_name,
            parent_objective_id=request.parent_objective_id,
            category=request.category,
            tags=request.tags
        )
        return objective
    except Exception as e:
        raise HTTPException(500, f"Error creating objective: {str(e)}")
    finally:
        db.close()


@router.get("/objectives")
def list_objectives(
    status: Optional[str] = None,
    period: Optional[str] = None,
    owner_id: Optional[str] = None,
    category: Optional[str] = None,
    parent_objective_id: Optional[str] = None,
    include_key_results: bool = False,
    tenant_id: Optional[str] = None
):
    """
    Lista objetivos com filtros.

    Args:
        status: Filtrar por status
        period: Filtrar por periodo
        owner_id: Filtrar por owner
        category: Filtrar por categoria
        parent_objective_id: Filtrar por objetivo pai
        include_key_results: Incluir key results
        tenant_id: ID do tenant

    Returns:
        Lista de objetivos
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        objectives = manager.list_objectives(
            status=status,
            period=period,
            owner_id=owner_id,
            category=category,
            parent_objective_id=parent_objective_id,
            include_key_results=include_key_results
        )
        return objectives
    finally:
        db.close()


@router.get("/objectives/{objective_id}")
def get_objective(objective_id: str, tenant_id: Optional[str] = None):
    """
    Retorna detalhes de um objetivo.

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Dados do objetivo com key results e links
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        objective = manager.get_objective(objective_id)
        if not objective:
            raise HTTPException(404, "Objective not found")
        return objective
    finally:
        db.close()


@router.put("/objectives/{objective_id}")
def update_objective(
    objective_id: str,
    request: ObjectiveUpdate,
    tenant_id: Optional[str] = None
):
    """
    Atualiza um objetivo.

    Args:
        objective_id: ID do objetivo
        request: Dados a atualizar
        tenant_id: ID do tenant

    Returns:
        Objetivo atualizado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        updates = request.model_dump(exclude_unset=True)
        objective = manager.update_objective(objective_id, **updates)
        if not objective:
            raise HTTPException(404, "Objective not found")
        return objective
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error updating objective: {str(e)}")
    finally:
        db.close()


@router.delete("/objectives/{objective_id}")
def delete_objective(objective_id: str, tenant_id: Optional[str] = None):
    """
    Deleta um objetivo (soft delete).

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Confirmacao
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        success = manager.delete_objective(objective_id)
        if not success:
            raise HTTPException(404, "Objective not found")
        return {"message": "Objective deleted successfully"}
    finally:
        db.close()


@router.post("/objectives/{objective_id}/activate")
def activate_objective(objective_id: str, tenant_id: Optional[str] = None):
    """
    Ativa um objetivo.

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Objetivo atualizado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        objective = manager.activate_objective(objective_id)
        if not objective:
            raise HTTPException(404, "Objective not found")
        return objective
    finally:
        db.close()


@router.post("/objectives/{objective_id}/complete")
def complete_objective(objective_id: str, tenant_id: Optional[str] = None):
    """
    Marca um objetivo como completado.

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Objetivo atualizado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        objective = manager.complete_objective(objective_id)
        if not objective:
            raise HTTPException(404, "Objective not found")
        return objective
    finally:
        db.close()


@router.get("/objectives/{objective_id}/progress")
def get_objective_progress(objective_id: str, tenant_id: Optional[str] = None):
    """
    Retorna progresso detalhado de um objetivo.

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Progresso detalhado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        progress = manager.get_okr_progress(objective_id)
        if not progress:
            raise HTTPException(404, "Objective not found")
        return progress
    finally:
        db.close()


@router.get("/objectives/{objective_id}/hierarchy")
def get_objective_hierarchy(objective_id: str, tenant_id: Optional[str] = None):
    """
    Retorna hierarquia completa de um objetivo.

    Args:
        objective_id: ID do objetivo
        tenant_id: ID do tenant

    Returns:
        Objetivo com filhos aninhados
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        hierarchy = manager.get_objective_hierarchy(objective_id)
        if not hierarchy:
            raise HTTPException(404, "Objective not found")
        return hierarchy
    finally:
        db.close()


# =============================================================================
# KEY RESULTS ENDPOINTS
# =============================================================================

@router.post("/objectives/{objective_id}/key-results")
def create_key_result(objective_id: str, request: KeyResultCreate):
    """
    Cria um novo Key Result para um objetivo.

    Args:
        objective_id: ID do objetivo
        request: Dados do key result

    Returns:
        Key result criado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        key_result = manager.create_key_result(
            objective_id=objective_id,
            title=request.title,
            target=request.target,
            description=request.description,
            initial=request.initial,
            unit=request.unit,
            metric_type=request.metric_type,
            weight=request.weight,
            owner_id=request.owner_id,
            owner_name=request.owner_name
        )
        if not key_result:
            raise HTTPException(404, "Objective not found")
        return key_result
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error creating key result: {str(e)}")
    finally:
        db.close()


@router.get("/key-results/{key_result_id}")
def get_key_result(key_result_id: str):
    """
    Retorna detalhes de um key result.

    Args:
        key_result_id: ID do key result

    Returns:
        Dados do key result
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        key_result = manager.get_key_result(key_result_id)
        if not key_result:
            raise HTTPException(404, "Key result not found")
        return key_result
    finally:
        db.close()


@router.put("/key-results/{key_result_id}")
def update_key_result(key_result_id: str, request: KeyResultUpdate):
    """
    Atualiza um key result.

    Args:
        key_result_id: ID do key result
        request: Dados a atualizar

    Returns:
        Key result atualizado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        updates = request.model_dump(exclude_unset=True)
        key_result = manager.update_key_result(key_result_id, **updates)
        if not key_result:
            raise HTTPException(404, "Key result not found")
        return key_result
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error updating key result: {str(e)}")
    finally:
        db.close()


@router.patch("/key-results/{key_result_id}/value")
def update_key_result_value(key_result_id: str, request: KeyResultValueUpdate):
    """
    Atualiza o valor atual de um key result.

    Args:
        key_result_id: ID do key result
        request: Novo valor e nota

    Returns:
        Key result atualizado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        key_result = manager.update_key_result_value(
            key_result_id=key_result_id,
            new_value=request.value,
            note=request.note
        )
        if not key_result:
            raise HTTPException(404, "Key result not found")
        return key_result
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error updating key result value: {str(e)}")
    finally:
        db.close()


@router.delete("/key-results/{key_result_id}")
def delete_key_result(key_result_id: str):
    """
    Deleta um key result (soft delete).

    Args:
        key_result_id: ID do key result

    Returns:
        Confirmacao
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        success = manager.delete_key_result(key_result_id)
        if not success:
            raise HTTPException(404, "Key result not found")
        return {"message": "Key result deleted successfully"}
    finally:
        db.close()


# =============================================================================
# STORY LINKING ENDPOINTS
# =============================================================================

@router.post("/objectives/{objective_id}/stories")
def link_story(objective_id: str, request: StoryLinkRequest):
    """
    Vincula uma story a um objetivo.

    Args:
        objective_id: ID do objetivo
        request: Dados do link

    Returns:
        Link criado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        link = manager.link_story(
            objective_id=objective_id,
            story_id=request.story_id,
            contribution=request.contribution,
            notes=request.notes
        )
        if not link:
            raise HTTPException(404, "Objective or Story not found")
        return link
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error linking story: {str(e)}")
    finally:
        db.close()


@router.get("/objectives/{objective_id}/stories")
def get_objective_stories(objective_id: str):
    """
    Retorna stories vinculadas a um objetivo.

    Args:
        objective_id: ID do objetivo

    Returns:
        Lista de stories
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        stories = manager.get_stories_for_objective(objective_id)
        return stories
    finally:
        db.close()


@router.delete("/objectives/{objective_id}/stories/{story_id}")
def unlink_story(objective_id: str, story_id: str):
    """
    Remove vinculo entre story e objetivo.

    Args:
        objective_id: ID do objetivo
        story_id: ID da story

    Returns:
        Confirmacao
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        success = manager.unlink_story(objective_id, story_id)
        if not success:
            raise HTTPException(404, "Link not found")
        return {"message": "Story unlinked successfully"}
    finally:
        db.close()


# =============================================================================
# PROJECT LINKING ENDPOINTS
# =============================================================================

@router.post("/objectives/{objective_id}/projects")
def link_project(objective_id: str, request: ProjectLinkRequest):
    """
    Vincula um projeto a um objetivo.

    Args:
        objective_id: ID do objetivo
        request: Dados do link

    Returns:
        Link criado
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        link = manager.link_project(
            objective_id=objective_id,
            project_id=request.project_id,
            contribution=request.contribution,
            notes=request.notes
        )
        if not link:
            raise HTTPException(404, "Objective or Project not found")
        return link
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error linking project: {str(e)}")
    finally:
        db.close()


@router.get("/objectives/{objective_id}/projects")
def get_objective_projects(objective_id: str):
    """
    Retorna projetos vinculados a um objetivo.

    Args:
        objective_id: ID do objetivo

    Returns:
        Lista de projetos
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        projects = manager.get_projects_for_objective(objective_id)
        return projects
    finally:
        db.close()


@router.delete("/objectives/{objective_id}/projects/{project_id}")
def unlink_project(objective_id: str, project_id: str):
    """
    Remove vinculo entre projeto e objetivo.

    Args:
        objective_id: ID do objetivo
        project_id: ID do projeto

    Returns:
        Confirmacao
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db)
        success = manager.unlink_project(objective_id, project_id)
        if not success:
            raise HTTPException(404, "Link not found")
        return {"message": "Project unlinked successfully"}
    finally:
        db.close()


# =============================================================================
# ANALYTICS ENDPOINTS
# =============================================================================

@router.get("/okrs/summary")
def get_okr_summary(tenant_id: Optional[str] = None):
    """
    Retorna resumo geral dos OKRs.

    Args:
        tenant_id: ID do tenant

    Returns:
        Metricas gerais
    """
    db = SessionLocal()
    try:
        manager = OKRManager(db, tenant_id)
        summary = manager.get_okr_summary()
        return summary
    finally:
        db.close()
