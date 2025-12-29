# -*- coding: utf-8 -*-
"""
GitHub Integration API Routes
=============================
Endpoints para gerenciar integracao com GitHub Issues.

Endpoints:
- POST /api/integrations/github/connect
- POST /api/integrations/github/disconnect
- POST /api/integrations/github/sync
- GET  /api/integrations/github/status
- POST /api/integrations/github/webhook
- GET  /api/integrations/github/test
- GET  /api/integrations/github/labels
- GET  /api/integrations/github/milestones
"""

import logging
import hmac
import hashlib
from datetime import datetime
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, BackgroundTasks, Request, Header
from pydantic import BaseModel, Field

from .github import (
    GitHubIntegration,
    GitHubConfig,
    get_github_integration
)
from .base import IntegrationStatus, SyncResult

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/integrations/github", tags=["GitHub Integration"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class GitHubConnectRequest(BaseModel):
    """Request para conectar ao GitHub"""
    token: str = Field(..., description="Personal Access Token do GitHub")
    owner: str = Field(..., description="Owner do repositorio (usuario ou organizacao)")
    repo: str = Field(..., description="Nome do repositorio")
    auto_sync: bool = Field(default=False, description="Ativar sincronizacao automatica")
    sync_interval_minutes: int = Field(default=30, description="Intervalo de sincronizacao em minutos")
    create_labels: bool = Field(default=True, description="Criar labels automaticamente")


class GitHubSyncRequest(BaseModel):
    """Request para sincronizacao com GitHub"""
    project_id: str = Field(..., description="ID do projeto local")
    direction: str = Field(default="from_external", description="Direcao: from_external ou to_external")
    story_ids: Optional[List[str]] = Field(None, description="IDs das stories para sincronizar (to_external)")


class GitHubStatusResponse(BaseModel):
    """Response de status da integracao GitHub"""
    system: str = "github"
    status: str
    connected: bool
    last_error: Optional[str]
    last_sync: Optional[str]
    auto_sync: bool
    sync_interval_minutes: int
    details: Dict[str, Any] = {}


class GitHubSyncResultResponse(BaseModel):
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
# GITHUB ENDPOINTS
# =============================================================================

@router.post("/connect")
async def github_connect(request: GitHubConnectRequest):
    """
    Conecta ao GitHub com as credenciais fornecidas.

    Valida o Personal Access Token e verifica acesso ao repositorio.
    """
    github = get_github_integration()

    # Atualiza configuracao
    github.config.token = request.token
    github.config.owner = request.owner
    github.config.repo = request.repo
    github.config.auto_sync = request.auto_sync
    github.config.sync_interval_minutes = request.sync_interval_minutes
    github.config.create_labels = request.create_labels
    github.config.enabled = True

    # Tenta conectar
    success = await github.connect()

    if success:
        return {
            "success": True,
            "message": "Conectado ao GitHub com sucesso",
            "status": github.get_status()
        }
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "success": False,
                "message": f"Falha ao conectar: {github.last_error}",
                "error": github.last_error
            }
        )


@router.post("/disconnect")
async def github_disconnect():
    """
    Desconecta do GitHub.
    """
    github = get_github_integration()
    await github.disconnect()
    github.config.enabled = False

    return {
        "success": True,
        "message": "Desconectado do GitHub"
    }


@router.get("/status", response_model=GitHubStatusResponse)
async def github_status():
    """
    Retorna o status atual da integracao com GitHub.
    """
    github = get_github_integration()
    status = github.get_status()

    return GitHubStatusResponse(
        system="github",
        status=status.get("status", "disconnected"),
        connected=status.get("connected", False),
        last_error=status.get("last_error"),
        last_sync=status.get("last_sync"),
        auto_sync=status.get("auto_sync", False),
        sync_interval_minutes=status.get("sync_interval_minutes", 30),
        details={
            "owner": status.get("owner"),
            "repo": status.get("repo"),
            "full_name": status.get("full_name"),
            "user": status.get("user"),
            "repo_url": status.get("repo_url"),
            "repo_private": status.get("repo_private")
        }
    )


@router.post("/sync")
async def github_sync(request: GitHubSyncRequest, background_tasks: BackgroundTasks):
    """
    Sincroniza stories com o GitHub.

    Direcoes:
    - from_external: Importa issues do GitHub para stories locais
    - to_external: Exporta stories locais para o GitHub
    """
    github = get_github_integration()

    if not github.is_connected:
        raise HTTPException(
            status_code=400,
            detail="GitHub nao esta conectado. Conecte primeiro usando /github/connect"
        )

    if request.direction == "from_external":
        result = await github.sync_from_external(request.project_id)
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

    return GitHubSyncResultResponse(
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


@router.get("/labels")
async def github_list_labels():
    """
    Lista labels disponiveis no repositorio GitHub.
    """
    github = get_github_integration()

    if not github.is_connected:
        raise HTTPException(
            status_code=400,
            detail="GitHub nao esta conectado"
        )

    labels = await github.get_labels()
    return {
        "labels": [
            {
                "name": label.get("name"),
                "color": label.get("color"),
                "description": label.get("description")
            }
            for label in labels
        ]
    }


@router.get("/milestones")
async def github_list_milestones():
    """
    Lista milestones (sprints) do repositorio GitHub.
    """
    github = get_github_integration()

    if not github.is_connected:
        raise HTTPException(
            status_code=400,
            detail="GitHub nao esta conectado"
        )

    milestones = await github.get_milestones()
    return {
        "milestones": [
            {
                "number": m.get("number"),
                "title": m.get("title"),
                "description": m.get("description"),
                "state": m.get("state"),
                "due_on": m.get("due_on"),
                "open_issues": m.get("open_issues"),
                "closed_issues": m.get("closed_issues")
            }
            for m in milestones
        ]
    }


@router.post("/webhook")
async def github_webhook(
    request: Request,
    x_hub_signature_256: Optional[str] = Header(None),
    x_github_event: Optional[str] = Header(None)
):
    """
    Endpoint para receber webhooks do GitHub.

    Configure o webhook no GitHub para enviar eventos para este endpoint.
    Eventos suportados: issues, issue_comment

    Para maior seguranca, configure um webhook secret no GitHub e
    defina a variavel GITHUB_WEBHOOK_SECRET.
    """
    github = get_github_integration()

    try:
        body = await request.body()
        payload = await request.json()

        # Valida assinatura se configurada
        if github.config.webhook_secret and x_hub_signature_256:
            expected_signature = "sha256=" + hmac.new(
                github.config.webhook_secret.encode(),
                body,
                hashlib.sha256
            ).hexdigest()

            if not hmac.compare_digest(expected_signature, x_hub_signature_256):
                logger.warning("Assinatura de webhook invalida")
                raise HTTPException(status_code=401, detail="Assinatura invalida")

        # Processa webhook
        logger.info(f"GitHub webhook recebido: {x_github_event}")
        success = await github.handle_webhook(payload)

        if success:
            return {"status": "processed", "event": x_github_event}
        else:
            return {"status": "ignored", "event": x_github_event}

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao processar webhook GitHub: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/test")
async def github_test_connection():
    """
    Testa a conexao com o GitHub.
    """
    github = get_github_integration()

    if await github.test_connection():
        return {"success": True, "message": "Conexao OK"}
    else:
        return {"success": False, "message": "Falha na conexao", "error": github.last_error}


@router.get("/issues")
async def github_list_issues(
    state: str = "all",
    labels: Optional[str] = None,
    per_page: int = 30,
    page: int = 1
):
    """
    Lista issues do repositorio GitHub.

    Args:
        state: open, closed ou all
        labels: Labels separadas por virgula
        per_page: Issues por pagina (max 100)
        page: Numero da pagina
    """
    github = get_github_integration()

    if not github.is_connected:
        raise HTTPException(
            status_code=400,
            detail="GitHub nao esta conectado"
        )

    label_list = labels.split(",") if labels else None
    issues = await github.list_issues(
        state=state,
        labels=label_list,
        per_page=min(per_page, 100),
        page=page
    )

    return {
        "issues": [
            {
                "number": i.get("number"),
                "title": i.get("title"),
                "state": i.get("state"),
                "labels": [l.get("name") for l in i.get("labels", [])],
                "assignees": [a.get("login") for a in i.get("assignees", [])],
                "milestone": i.get("milestone", {}).get("title") if i.get("milestone") else None,
                "created_at": i.get("created_at"),
                "updated_at": i.get("updated_at"),
                "html_url": i.get("html_url")
            }
            for i in issues
        ],
        "page": page,
        "per_page": per_page
    }
