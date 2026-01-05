# -*- coding: utf-8 -*-
"""
GitHub Sync API Routes
======================

Endpoints REST para sincronizacao de codigo com GitHub.
Complementa as rotas de Issues existentes em github_routes.py.
"""

from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel, Field
from typing import Dict, List, Optional, Any

from factory.core.github_sync import github_sync, SyncStatus


router = APIRouter(prefix="/api/github-sync", tags=["GitHub Sync"])


# === Request/Response Models ===

class InitRepoRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    remote_url: Optional[str] = Field(None, description="URL do repositorio remoto")


class ConfigureSyncRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    remote_url: str = Field(..., description="URL do repositorio GitHub")
    branch: str = Field("main", description="Branch principal")
    branch_strategy: str = Field("feature", description="Estrategia de branches: single, feature, gitflow")
    auto_push: bool = Field(True, description="Push automatico apos commits")
    auto_pull: bool = Field(True, description="Pull automatico antes de editar")


class CommitRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    message: str = Field(..., description="Mensagem do commit")
    files: Optional[List[str]] = Field(None, description="Arquivos especificos (None = todos)")


class PushRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    branch: Optional[str] = Field(None, description="Branch para push")
    force: bool = Field(False, description="Force push")


class PullRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    branch: Optional[str] = Field(None, description="Branch para pull")


class CreateBranchRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    story_id: str = Field(..., description="ID da story (ex: STR-001)")
    story_title: str = Field(..., description="Titulo da story")


class CreatePRRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    title: str = Field(..., description="Titulo do PR")
    body: str = Field(..., description="Descricao do PR")
    head_branch: str = Field(..., description="Branch de origem")
    base_branch: str = Field("main", description="Branch de destino")


class RollbackRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    version_id: str = Field(..., description="ID da versao para rollback")


# === Endpoints ===

@router.post("/init", response_model=Dict)
async def init_repository(request: InitRepoRequest):
    """
    Inicializa repositorio Git em um projeto.

    Cria estrutura Git, .gitignore e commit inicial.
    Opcionalmente configura repositorio remoto.
    """
    result = github_sync.init_repo(request.project_id, request.remote_url)

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/configure", response_model=Dict)
async def configure_sync(request: ConfigureSyncRequest):
    """
    Configura sincronizacao GitHub para um projeto.

    Define URL remoto, estrategia de branches e comportamento automatico.
    """
    # Primeiro inicializar se necessario
    init_result = github_sync.init_repo(request.project_id, request.remote_url)
    if not init_result.success:
        raise HTTPException(status_code=400, detail=init_result.error)

    # Configurar
    success = github_sync.configure_sync(
        project_id=request.project_id,
        remote_url=request.remote_url,
        branch=request.branch,
        branch_strategy=request.branch_strategy,
        auto_push=request.auto_push,
        auto_pull=request.auto_pull
    )

    return {
        "success": success,
        "message": "Sincronizacao configurada com sucesso",
        "config": {
            "remote_url": request.remote_url,
            "branch": request.branch,
            "branch_strategy": request.branch_strategy,
            "auto_push": request.auto_push,
            "auto_pull": request.auto_pull
        }
    }


@router.get("/status/{project_id}", response_model=Dict)
async def get_sync_status(project_id: str):
    """
    Retorna status de sincronizacao de um projeto.

    Inclui: branch atual, commits pendentes, mudancas nao commitadas.
    """
    return github_sync.get_sync_status(project_id)


@router.post("/commit", response_model=Dict)
async def commit_changes(request: CommitRequest):
    """
    Cria commit com as mudancas do projeto.

    Se files nao especificado, commita todas as mudancas.
    """
    result = github_sync.commit_changes(
        request.project_id,
        request.message,
        request.files
    )

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/push", response_model=Dict)
async def push_changes(request: PushRequest):
    """
    Push das mudancas para o repositorio remoto.
    """
    result = github_sync.push(request.project_id, request.branch, request.force)

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/pull", response_model=Dict)
async def pull_changes(request: PullRequest):
    """
    Pull das mudancas do repositorio remoto.
    """
    result = github_sync.pull(request.project_id, request.branch)

    if not result.success:
        if result.status == SyncStatus.CONFLICT:
            raise HTTPException(
                status_code=409,
                detail={"message": "Conflitos detectados", "conflicts": result.conflicts}
            )
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/sync/{project_id}", response_model=Dict)
async def full_sync(project_id: str, message: str = "Sync changes"):
    """
    Sincronizacao completa: commit + pull + push.

    Ideal para sincronizar todas as mudancas de uma vez.
    """
    results = []

    # 1. Commit mudancas locais
    commit_result = github_sync.commit_changes(project_id, message)
    results.append({"step": "commit", **commit_result.to_dict()})

    # 2. Pull mudancas remotas
    pull_result = github_sync.pull(project_id)
    results.append({"step": "pull", **pull_result.to_dict()})

    if not pull_result.success and pull_result.status == SyncStatus.CONFLICT:
        return {
            "success": False,
            "status": "conflict",
            "message": "Conflitos detectados durante pull",
            "steps": results,
            "conflicts": pull_result.conflicts
        }

    # 3. Push mudancas
    push_result = github_sync.push(project_id)
    results.append({"step": "push", **push_result.to_dict()})

    all_success = all(r.get("success", False) for r in results)

    return {
        "success": all_success,
        "status": "success" if all_success else "partial",
        "message": "Sincronizacao completa" if all_success else "Sincronizacao parcial",
        "steps": results
    }


@router.post("/branch", response_model=Dict)
async def create_feature_branch(request: CreateBranchRequest):
    """
    Cria branch para uma feature/story.

    Nomeia automaticamente: feature/{story_id}-{story_title_slug}
    """
    result = github_sync.create_feature_branch(
        request.project_id,
        request.story_id,
        request.story_title
    )

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/pr", response_model=Dict)
async def create_pull_request(request: CreatePRRequest):
    """
    Cria Pull Request no GitHub.

    Requer: GitHub token configurado, branch ja criada e pushada.
    """
    result = await github_sync.create_pull_request(
        request.project_id,
        request.title,
        request.body,
        request.head_branch,
        request.base_branch
    )

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.get("/versions/{project_id}", response_model=List[Dict])
async def get_versions(project_id: str, limit: int = 20):
    """
    Lista historico de versoes de um projeto.

    Retorna snapshots de commits com possibilidade de rollback.
    """
    versions = github_sync.get_versions(project_id, limit)
    return [v.to_dict() for v in versions]


@router.post("/rollback", response_model=Dict)
async def rollback_version(request: RollbackRequest):
    """
    Rollback para uma versao anterior.

    Cria branch de backup antes de reverter.
    CUIDADO: Esta operacao altera o historico local.
    """
    result = github_sync.rollback(request.project_id, request.version_id)

    if not result.success:
        raise HTTPException(status_code=400, detail=result.error)

    return result.to_dict()


@router.post("/auto-commit/{project_id}", response_model=Dict)
async def auto_commit_on_story_complete(project_id: str, story_id: str, story_title: str):
    """
    Commit automatico quando uma story e completada.

    Chamado internamente quando story muda para status 'done'.
    """
    message = f"feat({story_id}): {story_title}\n\nStory completada via Plataforma E"

    result = github_sync.commit_changes(project_id, message)

    if result.success:
        # Auto push se configurado
        config = github_sync.configs.get(project_id)
        if config and config.auto_push:
            push_result = github_sync.push(project_id)
            return {
                "commit": result.to_dict(),
                "push": push_result.to_dict()
            }

    return {"commit": result.to_dict()}


@router.get("/quick-setup/{project_id}", response_model=Dict)
async def quick_setup_info(project_id: str):
    """
    Retorna informacoes para setup rapido de GitHub.

    Inclui comandos git e URL sugerida para repositorio.
    """
    from factory.core.github_sync import github_sync

    owner = github_sync.github_owner or "seu-usuario"
    repo_name = f"plataformae-{project_id}"
    suggested_url = f"https://github.com/{owner}/{repo_name}.git"

    return {
        "project_id": project_id,
        "suggested_repo_name": repo_name,
        "suggested_url": suggested_url,
        "github_configured": bool(github_sync.github_token),
        "setup_steps": [
            f"1. Crie o repositorio no GitHub: {repo_name}",
            f"2. Configure a URL: {suggested_url}",
            "3. Clique em 'Conectar' para iniciar sync"
        ],
        "git_commands": [
            f"git remote add origin {suggested_url}",
            "git push -u origin main"
        ]
    }
