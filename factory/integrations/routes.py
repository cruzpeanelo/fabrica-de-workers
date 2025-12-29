# -*- coding: utf-8 -*-
"""
Integration API Routes
======================
Endpoints para gerenciar integracoes com Jira e Azure DevOps.

Endpoints:
- POST /api/integrations/jira/connect
- POST /api/integrations/jira/disconnect
- POST /api/integrations/jira/sync
- GET  /api/integrations/jira/status
- POST /api/integrations/jira/webhook

- POST /api/integrations/azure-devops/connect
- POST /api/integrations/azure-devops/disconnect
- POST /api/integrations/azure-devops/sync
- GET  /api/integrations/azure-devops/status
- POST /api/integrations/azure-devops/webhook

- GET  /api/integrations/status
"""

import logging
from datetime import datetime
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, BackgroundTasks, Request
from pydantic import BaseModel, Field

from .jira import (
    JiraIntegration,
    JiraConfig,
    get_jira_integration
)
from .azure_devops import (
    AzureDevOpsIntegration,
    AzureDevOpsConfig,
    get_azure_devops_integration
)
from .base import IntegrationStatus, SyncResult

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/integrations", tags=["Integrations"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class JiraConnectRequest(BaseModel):
    """Request para conectar ao Jira"""
    url: str = Field(..., description="URL do Jira (ex: https://empresa.atlassian.net)")
    email: str = Field(..., description="Email do usuario")
    api_token: str = Field(..., description="Token de API")
    project_key: str = Field(..., description="Chave do projeto (ex: PROJ)")
    auto_sync: bool = Field(default=False, description="Ativar sincronizacao automatica")
    sync_interval_minutes: int = Field(default=30, description="Intervalo de sincronizacao em minutos")


class AzureDevOpsConnectRequest(BaseModel):
    """Request para conectar ao Azure DevOps"""
    organization: str = Field(..., description="Nome da organizacao")
    project: str = Field(..., description="Nome do projeto")
    pat: str = Field(..., description="Personal Access Token")
    team: Optional[str] = Field(None, description="Nome do time (opcional)")
    area_path: Optional[str] = Field(None, description="Area path (opcional)")
    iteration_path: Optional[str] = Field(None, description="Iteration path (opcional)")
    auto_sync: bool = Field(default=False, description="Ativar sincronizacao automatica")
    sync_interval_minutes: int = Field(default=30, description="Intervalo de sincronizacao em minutos")


class SyncRequest(BaseModel):
    """Request para sincronizacao"""
    project_id: str = Field(..., description="ID do projeto local")
    direction: str = Field(default="from_external", description="Direcao: from_external ou to_external")
    story_ids: Optional[List[str]] = Field(None, description="IDs das stories para sincronizar (to_external)")


class IntegrationStatusResponse(BaseModel):
    """Response de status da integracao"""
    system: str
    status: str
    connected: bool
    last_error: Optional[str]
    last_sync: Optional[str]
    auto_sync: bool
    sync_interval_minutes: int
    details: Dict[str, Any] = {}


class SyncResultResponse(BaseModel):
    """Response de resultado de sincronizacao"""
    success: bool
    items_synced: int
    items_created: int
    items_updated: int
    items_failed: int
    errors: List[str]
    warnings: List[str]
    started_at: Optional[str]
    completed_at: Optional[str]


# =============================================================================
# JIRA ENDPOINTS
# =============================================================================

@router.post("/jira/connect")
async def jira_connect(request: JiraConnectRequest):
    """
    Conecta ao Jira com as credenciais fornecidas.

    Valida as credenciais e estabelece conexao com a API do Jira.
    """
    jira = get_jira_integration()

    # Atualiza configuracao
    jira.config.url = request.url
    jira.config.email = request.email
    jira.config.api_token = request.api_token
    jira.config.project_key = request.project_key
    jira.config.auto_sync = request.auto_sync
    jira.config.sync_interval_minutes = request.sync_interval_minutes
    jira.config.enabled = True

    # Tenta conectar
    success = await jira.connect()

    if success:
        return {
            "success": True,
            "message": "Conectado ao Jira com sucesso",
            "status": jira.get_status()
        }
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "success": False,
                "message": f"Falha ao conectar: {jira.last_error}",
                "error": jira.last_error
            }
        )


@router.post("/jira/disconnect")
async def jira_disconnect():
    """
    Desconecta do Jira.
    """
    jira = get_jira_integration()
    await jira.disconnect()
    jira.config.enabled = False

    return {
        "success": True,
        "message": "Desconectado do Jira"
    }


@router.get("/jira/status", response_model=IntegrationStatusResponse)
async def jira_status():
    """
    Retorna o status atual da integracao com Jira.
    """
    jira = get_jira_integration()
    status = jira.get_status()

    return IntegrationStatusResponse(
        system="jira",
        status=status.get("status", "disconnected"),
        connected=status.get("connected", False),
        last_error=status.get("last_error"),
        last_sync=status.get("last_sync"),
        auto_sync=status.get("auto_sync", False),
        sync_interval_minutes=status.get("sync_interval_minutes", 30),
        details={
            "url": status.get("url"),
            "project_key": status.get("project_key"),
            "user": status.get("user"),
            "user_email": status.get("user_email")
        }
    )


@router.post("/jira/sync")
async def jira_sync(request: SyncRequest, background_tasks: BackgroundTasks):
    """
    Sincroniza stories com o Jira.

    Direcoes:
    - from_external: Importa issues do Jira para stories locais
    - to_external: Exporta stories locais para o Jira
    """
    jira = get_jira_integration()

    if not jira.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Jira nao esta conectado. Conecte primeiro usando /jira/connect"
        )

    if request.direction == "from_external":
        result = await jira.sync_from_external(request.project_id)
    elif request.direction == "to_external":
        if not request.story_ids:
            raise HTTPException(
                status_code=400,
                detail="story_ids e obrigatorio para sincronizacao to_external"
            )
        # Aqui precisariamos buscar as stories do banco
        # Por ora, retornamos erro informativo
        raise HTTPException(
            status_code=501,
            detail="Sincronizacao to_external requer integracao com repositorio de stories"
        )
    else:
        raise HTTPException(
            status_code=400,
            detail="direction deve ser 'from_external' ou 'to_external'"
        )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_created=result.items_created,
        items_updated=result.items_updated,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings,
        started_at=result.started_at.isoformat() if result.started_at else None,
        completed_at=result.completed_at.isoformat() if result.completed_at else None
    )


@router.get("/jira/projects")
async def jira_list_projects():
    """
    Lista projetos disponiveis no Jira.
    """
    jira = get_jira_integration()

    if not jira.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Jira nao esta conectado"
        )

    projects = await jira.get_projects()
    return {
        "projects": [
            {
                "key": p.get("key"),
                "name": p.get("name"),
                "id": p.get("id")
            }
            for p in projects
        ]
    }


@router.post("/jira/webhook")
async def jira_webhook(request: Request):
    """
    Endpoint para receber webhooks do Jira.

    Configure o webhook no Jira para enviar eventos para este endpoint.
    """
    jira = get_jira_integration()

    try:
        payload = await request.json()
        success = await jira.handle_webhook(payload)

        if success:
            return {"status": "processed"}
        else:
            return {"status": "ignored"}

    except Exception as e:
        logger.error(f"Erro ao processar webhook Jira: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/jira/test")
async def jira_test_connection():
    """
    Testa a conexao com o Jira.
    """
    jira = get_jira_integration()

    if await jira.test_connection():
        return {"success": True, "message": "Conexao OK"}
    else:
        return {"success": False, "message": "Falha na conexao", "error": jira.last_error}


# =============================================================================
# AZURE DEVOPS ENDPOINTS
# =============================================================================

@router.post("/azure-devops/connect")
async def azure_devops_connect(request: AzureDevOpsConnectRequest):
    """
    Conecta ao Azure DevOps com as credenciais fornecidas.

    Valida as credenciais e estabelece conexao com a API do Azure DevOps.
    """
    azure = get_azure_devops_integration()

    # Atualiza configuracao
    azure.config.organization = request.organization
    azure.config.project = request.project
    azure.config.pat = request.pat
    azure.config.team = request.team or ""
    azure.config.area_path = request.area_path or ""
    azure.config.iteration_path = request.iteration_path or ""
    azure.config.auto_sync = request.auto_sync
    azure.config.sync_interval_minutes = request.sync_interval_minutes
    azure.config.enabled = True

    # Tenta conectar
    success = await azure.connect()

    if success:
        return {
            "success": True,
            "message": "Conectado ao Azure DevOps com sucesso",
            "status": azure.get_status()
        }
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "success": False,
                "message": f"Falha ao conectar: {azure.last_error}",
                "error": azure.last_error
            }
        )


@router.post("/azure-devops/disconnect")
async def azure_devops_disconnect():
    """
    Desconecta do Azure DevOps.
    """
    azure = get_azure_devops_integration()
    await azure.disconnect()
    azure.config.enabled = False

    return {
        "success": True,
        "message": "Desconectado do Azure DevOps"
    }


@router.get("/azure-devops/status", response_model=IntegrationStatusResponse)
async def azure_devops_status():
    """
    Retorna o status atual da integracao com Azure DevOps.
    """
    azure = get_azure_devops_integration()
    status = azure.get_status()

    return IntegrationStatusResponse(
        system="azure_devops",
        status=status.get("status", "disconnected"),
        connected=status.get("connected", False),
        last_error=status.get("last_error"),
        last_sync=status.get("last_sync"),
        auto_sync=status.get("auto_sync", False),
        sync_interval_minutes=status.get("sync_interval_minutes", 30),
        details={
            "organization": status.get("organization"),
            "project": status.get("project"),
            "team": status.get("team"),
            "area_path": status.get("area_path"),
            "iteration_path": status.get("iteration_path")
        }
    )


@router.post("/azure-devops/sync")
async def azure_devops_sync(request: SyncRequest, background_tasks: BackgroundTasks):
    """
    Sincroniza stories com o Azure DevOps.

    Direcoes:
    - from_external: Importa work items do Azure DevOps para stories locais
    - to_external: Exporta stories locais para o Azure DevOps
    """
    azure = get_azure_devops_integration()

    if not azure.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Azure DevOps nao esta conectado. Conecte primeiro usando /azure-devops/connect"
        )

    if request.direction == "from_external":
        result = await azure.sync_from_external(request.project_id)
    elif request.direction == "to_external":
        if not request.story_ids:
            raise HTTPException(
                status_code=400,
                detail="story_ids e obrigatorio para sincronizacao to_external"
            )
        # Aqui precisariamos buscar as stories do banco
        raise HTTPException(
            status_code=501,
            detail="Sincronizacao to_external requer integracao com repositorio de stories"
        )
    else:
        raise HTTPException(
            status_code=400,
            detail="direction deve ser 'from_external' ou 'to_external'"
        )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_created=result.items_created,
        items_updated=result.items_updated,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings,
        started_at=result.started_at.isoformat() if result.started_at else None,
        completed_at=result.completed_at.isoformat() if result.completed_at else None
    )


@router.get("/azure-devops/iterations")
async def azure_devops_list_iterations():
    """
    Lista iterations (sprints) disponiveis no Azure DevOps.
    """
    azure = get_azure_devops_integration()

    if not azure.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Azure DevOps nao esta conectado"
        )

    iterations = await azure.get_iterations()
    return {
        "iterations": [
            {
                "id": i.get("id"),
                "name": i.get("name"),
                "path": i.get("path"),
                "attributes": i.get("attributes", {})
            }
            for i in iterations
        ]
    }


@router.get("/azure-devops/areas")
async def azure_devops_list_areas():
    """
    Lista areas disponiveis no Azure DevOps.
    """
    azure = get_azure_devops_integration()

    if not azure.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Azure DevOps nao esta conectado"
        )

    areas = await azure.get_areas()
    return {"areas": areas}


@router.post("/azure-devops/webhook")
async def azure_devops_webhook(request: Request):
    """
    Endpoint para receber webhooks do Azure DevOps.

    Configure o Service Hook no Azure DevOps para enviar eventos para este endpoint.
    """
    azure = get_azure_devops_integration()

    try:
        payload = await request.json()
        success = await azure.handle_webhook(payload)

        if success:
            return {"status": "processed"}
        else:
            return {"status": "ignored"}

    except Exception as e:
        logger.error(f"Erro ao processar webhook Azure DevOps: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/azure-devops/test")
async def azure_devops_test_connection():
    """
    Testa a conexao com o Azure DevOps.
    """
    azure = get_azure_devops_integration()

    if await azure.test_connection():
        return {"success": True, "message": "Conexao OK"}
    else:
        return {"success": False, "message": "Falha na conexao", "error": azure.last_error}


# =============================================================================
# GENERAL INTEGRATION ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_all_integrations_status():
    """
    Retorna o status de todas as integracoes configuradas.
    """
    jira = get_jira_integration()
    azure = get_azure_devops_integration()

    return {
        "integrations": {
            "jira": jira.get_status(),
            "azure_devops": azure.get_status()
        },
        "timestamp": datetime.utcnow().isoformat()
    }


@router.post("/sync-all")
async def sync_all_integrations(project_id: str, background_tasks: BackgroundTasks):
    """
    Sincroniza todas as integracoes ativas.
    """
    results = {}

    jira = get_jira_integration()
    if jira.is_connected:
        results["jira"] = (await jira.sync_from_external(project_id)).to_dict()

    azure = get_azure_devops_integration()
    if azure.is_connected:
        results["azure_devops"] = (await azure.sync_from_external(project_id)).to_dict()

    return {
        "success": True,
        "results": results,
        "timestamp": datetime.utcnow().isoformat()
    }
