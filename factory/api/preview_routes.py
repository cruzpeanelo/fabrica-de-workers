# -*- coding: utf-8 -*-
"""
Preview API Routes - Rotas da API para Ambientes de Preview e Staging
======================================================================

Issue #66: Ambiente de Preview e Staging Automatico

Endpoints:
- POST /api/previews/story/{story_id} - Criar preview de story
- POST /api/previews/branch - Criar preview de branch
- POST /api/previews/staging - Criar ambiente staging
- GET /api/previews - Listar todos previews
- GET /api/previews/{preview_id} - Buscar preview
- POST /api/previews/{preview_id}/start - Iniciar preview
- POST /api/previews/{preview_id}/stop - Parar preview
- DELETE /api/previews/{preview_id} - Destruir preview
- GET /api/previews/{preview_id}/health - Health check
- GET /api/previews/{preview_id}/logs - Logs do preview
- GET /api/previews/{preview_id}/qrcode - QR Code para mobile
- POST /api/previews/cleanup - Limpar previews expirados

Autor: Fabrica de Agentes
Data: 2025-12-29
"""

from fastapi import APIRouter, HTTPException, Query, BackgroundTasks
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime

# Import do PreviewManager
from factory.core.preview_manager import (
    get_preview_manager,
    PreviewManager,
    PreviewType,
    PreviewStatus
)

# Criar router
router = APIRouter(prefix="/api/previews", tags=["Previews"])


# =============================================================================
# SCHEMAS
# =============================================================================

class StoryPreviewCreate(BaseModel):
    """Schema para criar preview de story"""
    name: Optional[str] = Field(None, description="Nome do preview")
    auto_destroy: bool = Field(True, description="Destruir automaticamente")
    destroy_after_hours: int = Field(24, ge=1, le=168, description="Horas ate destruir")


class BranchPreviewCreate(BaseModel):
    """Schema para criar preview de branch"""
    branch_name: str = Field(..., description="Nome da branch")
    project_id: str = Field(..., description="ID do projeto")
    name: Optional[str] = Field(None, description="Nome do preview")
    auto_destroy: bool = Field(True, description="Destruir automaticamente")
    destroy_after_hours: int = Field(48, ge=1, le=168, description="Horas ate destruir")


class StagingCreate(BaseModel):
    """Schema para criar ambiente staging"""
    project_id: str = Field(..., description="ID do projeto")
    name: Optional[str] = Field(None, description="Nome do staging")
    reset_daily: bool = Field(False, description="Resetar diariamente")


class PreviewResponse(BaseModel):
    """Schema de resposta do preview"""
    preview_id: str
    project_id: Optional[str]
    story_id: Optional[str]
    branch_name: Optional[str]
    preview_type: str
    name: str
    description: Optional[str]
    status: str
    url: Optional[str]
    port: Optional[int]
    health_status: str
    qr_code: Optional[str]
    auto_destroy: bool
    destroy_after_hours: int
    created_at: Optional[str]
    deployed_at: Optional[str]
    expires_at: Optional[str]


class PreviewListResponse(BaseModel):
    """Schema de resposta para lista de previews"""
    success: bool
    total: int
    previews: List[Dict[str, Any]]


class HealthCheckResponse(BaseModel):
    """Schema de resposta do health check"""
    success: bool
    health_status: str
    last_check: Optional[str]


class ActionResponse(BaseModel):
    """Schema de resposta para acoes"""
    success: bool
    message: str
    preview: Optional[Dict[str, Any]] = None


# =============================================================================
# ENDPOINTS - CRIACAO
# =============================================================================

@router.post("/story/{story_id}", response_model=ActionResponse)
async def create_story_preview(
    story_id: str,
    project_id: str = Query(..., description="ID do projeto"),
    data: StoryPreviewCreate = None,
    background_tasks: BackgroundTasks = None
):
    """
    Cria um ambiente de preview para uma Story

    O preview sera criado e o deploy iniciado em background.
    A URL do preview estara disponivel quando o status for 'running'.

    Exemplo:
    - URL: story-str-0001.preview.fabrica.local:9100
    - QR Code disponivel para acesso mobile
    """
    manager = get_preview_manager()

    data = data or StoryPreviewCreate()

    try:
        result = await manager.create_story_preview(
            story_id=story_id,
            project_id=project_id,
            name=data.name,
            auto_destroy=data.auto_destroy,
            destroy_after_hours=data.destroy_after_hours
        )

        return ActionResponse(
            success=result["success"],
            message=result["message"],
            preview=result.get("preview")
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/branch", response_model=ActionResponse)
async def create_branch_preview(
    data: BranchPreviewCreate,
    background_tasks: BackgroundTasks = None
):
    """
    Cria um ambiente de preview para uma Branch

    O preview sera atualizado automaticamente a cada push na branch.
    Sera destruido automaticamente quando a branch for merged.

    Exemplo:
    - URL: branch-feature-login.preview.fabrica.local:9101
    """
    manager = get_preview_manager()

    try:
        result = await manager.create_branch_preview(
            branch_name=data.branch_name,
            project_id=data.project_id,
            name=data.name,
            auto_destroy=data.auto_destroy,
            destroy_after_hours=data.destroy_after_hours
        )

        return ActionResponse(
            success=result["success"],
            message=result["message"],
            preview=result.get("preview")
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/staging", response_model=ActionResponse)
async def create_staging(
    data: StagingCreate,
    background_tasks: BackgroundTasks = None
):
    """
    Cria um ambiente de Staging permanente

    O staging nao expira automaticamente e serve como ambiente
    de pre-producao para testes.

    Exemplo:
    - URL: staging-belgo-bpm.preview.fabrica.local:9102
    """
    manager = get_preview_manager()

    try:
        result = await manager.create_staging(
            project_id=data.project_id,
            name=data.name,
            reset_daily=data.reset_daily
        )

        return ActionResponse(
            success=result["success"],
            message=result["message"],
            preview=result.get("preview")
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# ENDPOINTS - LISTAGEM
# =============================================================================

@router.get("", response_model=PreviewListResponse)
async def list_previews(
    project_id: Optional[str] = Query(None, description="Filtrar por projeto"),
    preview_type: Optional[str] = Query(None, description="Filtrar por tipo (story, branch, staging)"),
    status: Optional[str] = Query(None, description="Filtrar por status")
):
    """
    Lista todos os ambientes de preview

    Filtros disponiveis:
    - project_id: ID do projeto
    - preview_type: story, branch, staging, production
    - status: pending, building, deploying, running, stopped, failed, destroyed
    """
    manager = get_preview_manager()

    previews = manager.list_previews(
        project_id=project_id,
        preview_type=preview_type,
        status=status
    )

    return PreviewListResponse(
        success=True,
        total=len(previews),
        previews=previews
    )


@router.get("/{preview_id}")
async def get_preview(preview_id: str):
    """
    Busca informacoes de um preview especifico

    Retorna todos os detalhes do preview incluindo:
    - Status atual
    - URL de acesso
    - Logs de build/deploy
    - QR Code para mobile
    - Metricas de saude
    """
    manager = get_preview_manager()

    preview = manager.get_preview(preview_id)

    if not preview:
        raise HTTPException(status_code=404, detail="Preview nao encontrado")

    return {
        "success": True,
        "preview": preview
    }


# =============================================================================
# ENDPOINTS - ACOES
# =============================================================================

@router.post("/{preview_id}/start", response_model=ActionResponse)
async def start_preview(preview_id: str):
    """
    Inicia um preview que esta parado

    So funciona para previews com status 'stopped'.
    """
    manager = get_preview_manager()

    result = await manager.start_preview(preview_id)

    if not result["success"]:
        raise HTTPException(status_code=400, detail=result["message"])

    return ActionResponse(
        success=True,
        message=result["message"],
        preview=result.get("preview")
    )


@router.post("/{preview_id}/stop", response_model=ActionResponse)
async def stop_preview(preview_id: str):
    """
    Para um preview em execucao

    O preview pode ser reiniciado posteriormente.
    """
    manager = get_preview_manager()

    result = await manager.stop_preview(preview_id)

    if not result["success"]:
        raise HTTPException(status_code=400, detail=result["message"])

    return ActionResponse(
        success=True,
        message=result["message"],
        preview=result.get("preview")
    )


@router.delete("/{preview_id}", response_model=ActionResponse)
async def destroy_preview(preview_id: str):
    """
    Destroi um preview completamente

    Remove:
    - Container Docker (se existir)
    - Imagem Docker
    - Arquivos do preview
    - Database do preview (se existir)

    Esta acao NAO pode ser desfeita!
    """
    manager = get_preview_manager()

    result = await manager.destroy_preview(preview_id)

    if not result["success"]:
        raise HTTPException(status_code=400, detail=result["message"])

    return ActionResponse(
        success=True,
        message=result["message"]
    )


# =============================================================================
# ENDPOINTS - MONITORAMENTO
# =============================================================================

@router.get("/{preview_id}/health", response_model=HealthCheckResponse)
async def check_health(preview_id: str):
    """
    Verifica a saude de um preview

    Tenta acessar:
    1. /health endpoint (se disponivel)
    2. URL raiz

    Retorna status: healthy, unhealthy, unreachable
    """
    manager = get_preview_manager()

    result = await manager.health_check(preview_id)

    if not result["success"]:
        raise HTTPException(status_code=400, detail=result["message"])

    return HealthCheckResponse(
        success=True,
        health_status=result["health_status"],
        last_check=result.get("last_check")
    )


@router.get("/{preview_id}/logs")
async def get_logs(
    preview_id: str,
    log_type: str = Query("all", description="Tipo de log: build, deploy, all")
):
    """
    Retorna logs do preview

    Tipos de log:
    - build: Logs do processo de build
    - deploy: Logs do deploy
    - all: Todos os logs
    """
    manager = get_preview_manager()

    preview = manager.get_preview(preview_id)

    if not preview:
        raise HTTPException(status_code=404, detail="Preview nao encontrado")

    logs = {}

    if log_type in ["build", "all"]:
        logs["build_logs"] = preview.get("build_logs", "")

    if log_type in ["deploy", "all"]:
        logs["deploy_logs"] = preview.get("deploy_logs", "")

    if preview.get("last_error"):
        logs["last_error"] = preview["last_error"]

    return {
        "success": True,
        "preview_id": preview_id,
        "logs": logs
    }


@router.get("/{preview_id}/qrcode")
async def get_qrcode(preview_id: str):
    """
    Retorna o QR Code do preview para acesso mobile

    O QR Code esta em formato Base64 (data:image/png;base64,...).
    Pode ser renderizado diretamente em uma tag <img>.
    """
    manager = get_preview_manager()

    preview = manager.get_preview(preview_id)

    if not preview:
        raise HTTPException(status_code=404, detail="Preview nao encontrado")

    qr_code = preview.get("qr_code")

    if not qr_code:
        raise HTTPException(status_code=404, detail="QR Code nao disponivel")

    return {
        "success": True,
        "preview_id": preview_id,
        "url": preview.get("url"),
        "qr_code": qr_code
    }


# =============================================================================
# ENDPOINTS - MANUTENCAO
# =============================================================================

@router.post("/cleanup", response_model=ActionResponse)
async def cleanup_expired():
    """
    Limpa todos os previews expirados

    Remove automaticamente previews que:
    - Passaram do tempo de expiracao (destroy_after_hours)
    - Estao com status 'destroyed' mas ainda tem arquivos

    Retorna lista de previews destruidos.
    """
    manager = get_preview_manager()

    result = await manager.cleanup_expired()

    return ActionResponse(
        success=True,
        message=result["message"],
        preview={"destroyed": result["destroyed"], "errors": result.get("errors", [])}
    )


@router.get("/stats")
async def get_stats():
    """
    Retorna estatisticas dos ambientes de preview

    Inclui:
    - Total de previews por status
    - Total por tipo
    - Uso de portas
    - Previews proximos de expirar
    """
    manager = get_preview_manager()

    previews = manager.list_previews()

    # Calcular estatisticas
    stats = {
        "total": len(previews),
        "by_status": {},
        "by_type": {},
        "expiring_soon": 0
    }

    for preview in previews:
        status = preview.get("status", "unknown")
        ptype = preview.get("preview_type", "unknown")

        stats["by_status"][status] = stats["by_status"].get(status, 0) + 1
        stats["by_type"][ptype] = stats["by_type"].get(ptype, 0) + 1

        # Verificar se expira em menos de 2 horas
        expires_at = preview.get("expires_at")
        if expires_at:
            try:
                exp_date = datetime.fromisoformat(expires_at.replace("Z", "+00:00"))
                from datetime import timezone
                now = datetime.now(timezone.utc)
                if (exp_date - now).total_seconds() < 7200:  # 2 horas
                    stats["expiring_soon"] += 1
            except Exception:
                pass

    return {
        "success": True,
        "stats": stats
    }


# =============================================================================
# ENDPOINTS - ACOES EM LOTE
# =============================================================================

@router.post("/stop-all")
async def stop_all_previews(
    project_id: Optional[str] = Query(None, description="Filtrar por projeto")
):
    """
    Para todos os previews (opcionalmente filtrado por projeto)
    """
    manager = get_preview_manager()

    previews = manager.list_previews(project_id=project_id, status=PreviewStatus.RUNNING.value)

    stopped = []
    errors = []

    for preview in previews:
        result = await manager.stop_preview(preview["preview_id"])
        if result["success"]:
            stopped.append(preview["preview_id"])
        else:
            errors.append({"preview_id": preview["preview_id"], "error": result["message"]})

    return {
        "success": True,
        "stopped": stopped,
        "errors": errors,
        "message": f"{len(stopped)} previews parados"
    }


@router.post("/destroy-all")
async def destroy_all_previews(
    project_id: Optional[str] = Query(None, description="Filtrar por projeto"),
    confirm: bool = Query(False, description="Confirmar destruicao")
):
    """
    Destroi todos os previews (opcionalmente filtrado por projeto)

    ATENCAO: Esta acao NAO pode ser desfeita!
    Use confirm=true para confirmar.
    """
    if not confirm:
        raise HTTPException(
            status_code=400,
            detail="Use confirm=true para confirmar a destruicao de todos os previews"
        )

    manager = get_preview_manager()

    previews = manager.list_previews(project_id=project_id)

    destroyed = []
    errors = []

    for preview in previews:
        if preview["status"] != PreviewStatus.DESTROYED.value:
            result = await manager.destroy_preview(preview["preview_id"])
            if result["success"]:
                destroyed.append(preview["preview_id"])
            else:
                errors.append({"preview_id": preview["preview_id"], "error": result["message"]})

    return {
        "success": True,
        "destroyed": destroyed,
        "errors": errors,
        "message": f"{len(destroyed)} previews destruidos"
    }
