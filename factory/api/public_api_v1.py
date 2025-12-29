# -*- coding: utf-8 -*-
"""
API Publica v1 - Fabrica de Agentes v6.5
========================================

Endpoints publicos versionados para integracao com desenvolvedores externos.
Documentacao completa em: https://docs.fabricadeagentes.com/api

Autenticacao: API Key via header X-API-Key ou query param api_key
Rate Limiting: Baseado no tier da API Key (free, basic, pro, enterprise)
"""
from datetime import datetime
from typing import List, Optional
from fastapi import APIRouter, HTTPException, Depends, Query, Request, Path, Body
from pydantic import BaseModel, Field

from factory.api.api_key_auth import (
    get_api_key, get_api_key_optional, require_scope, require_tier,
    APIKeyInfo, create_api_key, list_api_keys, revoke_api_key, get_api_key_stats,
    APIKeyCreate, APIKeyResponse
)
from factory.api.rate_limit_v2 import rate_limit_by_api_key, rate_limit_custom


# =============================================================================
# ROUTER PRINCIPAL
# =============================================================================

router = APIRouter(
    prefix="/api/v1",
    tags=["API Publica v1"],
    responses={
        401: {"description": "API Key invalida ou ausente"},
        403: {"description": "Scope ou tier insuficiente"},
        429: {"description": "Rate limit excedido"},
    }
)


# =============================================================================
# SCHEMAS
# =============================================================================

# --- Projetos ---

class ProjectCreate(BaseModel):
    """Criar um novo projeto"""
    name: str = Field(..., min_length=1, max_length=200, description="Nome do projeto")
    description: Optional[str] = Field(None, description="Descricao do projeto")
    project_type: str = Field("web-app", description="Tipo: web-app, api-service, data-analysis, automation")

    class Config:
        json_schema_extra = {
            "example": {
                "name": "Meu App",
                "description": "Aplicacao web moderna",
                "project_type": "web-app"
            }
        }


class ProjectResponse(BaseModel):
    """Resposta de projeto"""
    project_id: str
    name: str
    description: Optional[str]
    project_type: str
    status: str
    progress: float
    folder_path: Optional[str]
    github_url: Optional[str]
    created_at: str
    updated_at: str


class ProjectListResponse(BaseModel):
    """Lista paginada de projetos"""
    projects: List[ProjectResponse]
    total: int
    page: int
    per_page: int
    has_next: bool


# --- Stories ---

class StoryCreate(BaseModel):
    """Criar uma User Story"""
    title: str = Field(..., min_length=1, max_length=300, description="Titulo da story")
    persona: Optional[str] = Field(None, description="Como um [tipo de usuario]")
    action: Optional[str] = Field(None, description="Eu quero [funcionalidade]")
    benefit: Optional[str] = Field(None, description="Para que [beneficio]")
    acceptance_criteria: List[str] = Field(default=[], description="Criterios de aceite")
    story_points: int = Field(0, ge=0, le=21, description="Story points (Fibonacci: 1,2,3,5,8,13,21)")
    priority: str = Field("medium", description="Prioridade: low, medium, high, urgent")

    class Config:
        json_schema_extra = {
            "example": {
                "title": "Login com Google",
                "persona": "usuario do sistema",
                "action": "fazer login com minha conta Google",
                "benefit": "nao precise criar nova senha",
                "acceptance_criteria": [
                    "Botao 'Login com Google' visivel na pagina de login",
                    "Redirect correto para autenticacao Google",
                    "Usuario criado/atualizado apos autenticacao"
                ],
                "story_points": 5,
                "priority": "high"
            }
        }


class StoryResponse(BaseModel):
    """Resposta de story"""
    story_id: str
    project_id: str
    title: str
    narrative: str
    status: str
    story_points: int
    priority: str
    progress: float
    tasks_total: int
    tasks_completed: int
    created_at: str


class StoryListResponse(BaseModel):
    """Lista de stories"""
    stories: List[StoryResponse]
    total: int


# --- Jobs ---

class JobCreate(BaseModel):
    """Criar um job de desenvolvimento"""
    story_id: Optional[str] = Field(None, description="ID da story associada")
    description: str = Field(..., min_length=10, description="Descricao do que construir")
    tech_stack: Optional[str] = Field(None, description="Stack: python, fastapi, react, etc")
    features: List[str] = Field(default=[], description="Features desejadas")

    class Config:
        json_schema_extra = {
            "example": {
                "story_id": "STR-0001",
                "description": "Implementar sistema de autenticacao JWT com login/logout",
                "tech_stack": "python,fastapi",
                "features": ["login", "logout", "refresh_token"]
            }
        }


class JobResponse(BaseModel):
    """Resposta de job"""
    job_id: str
    description: str
    status: str
    current_step: str
    progress: float
    output_path: Optional[str]
    files_created: List[str]
    error_message: Optional[str]
    created_at: str
    completed_at: Optional[str]


# --- Webhooks ---

class WebhookCreate(BaseModel):
    """Criar um webhook"""
    name: str = Field(..., min_length=1, max_length=200)
    url: str = Field(..., description="URL de destino (HTTPS recomendado)")
    events: List[str] = Field(..., description="Eventos para assinar")
    secret: Optional[str] = Field(None, description="Secret para assinatura HMAC (gerado automaticamente se nao fornecido)")

    class Config:
        json_schema_extra = {
            "example": {
                "name": "Notificacoes Slack",
                "url": "https://meuapp.com/webhooks/fabrica",
                "events": ["story.completed", "job.completed", "job.failed"]
            }
        }


class WebhookResponse(BaseModel):
    """Resposta de webhook"""
    webhook_id: str
    name: str
    url: str
    events: List[str]
    status: str
    deliveries_total: int
    deliveries_failed: int
    created_at: str


# --- API Keys ---

class APIKeyListResponse(BaseModel):
    """Lista de API Keys"""
    api_keys: List[dict]
    total: int


# --- Genericos ---

class SuccessResponse(BaseModel):
    """Resposta de sucesso"""
    success: bool = True
    message: str


class ErrorResponse(BaseModel):
    """Resposta de erro"""
    error: str
    message: str
    details: Optional[dict] = None


# =============================================================================
# ENDPOINTS - HEALTH & INFO
# =============================================================================

@router.get(
    "/",
    summary="Informacoes da API",
    description="Retorna informacoes sobre a API publica",
    tags=["Info"]
)
async def api_info():
    """Informacoes basicas da API"""
    return {
        "name": "Fabrica de Agentes API",
        "version": "1.0.0",
        "docs": "/api/v1/docs",
        "openapi": "/api/v1/openapi.json",
        "status": "operational",
        "timestamp": datetime.utcnow().isoformat()
    }


@router.get(
    "/health",
    summary="Health Check",
    description="Verifica saude da API",
    tags=["Info"]
)
async def health_check():
    """Health check simples (sem autenticacao)"""
    return {
        "status": "healthy",
        "service": "fabrica-api-v1",
        "timestamp": datetime.utcnow().isoformat()
    }


# =============================================================================
# ENDPOINTS - API KEYS
# =============================================================================

@router.post(
    "/api-keys",
    response_model=APIKeyResponse,
    summary="Criar API Key",
    description="Cria uma nova API Key. A chave completa so e mostrada uma vez!",
    tags=["API Keys"]
)
async def create_new_api_key(
    data: APIKeyCreate,
    api_key: APIKeyInfo = Depends(require_scope("admin"))
):
    """
    Cria uma nova API Key.

    **Importante**: A chave completa (api_key) so e mostrada nesta resposta.
    Guarde-a em local seguro!
    """
    try:
        result = create_api_key(
            name=data.name,
            description=data.description,
            tier=data.tier,
            scopes=data.scopes,
            user_id=api_key.user_id,
            expires_in_days=data.expires_in_days,
        )

        return APIKeyResponse(
            key_id=result["key_id"],
            api_key=result["api_key"],
            name=result["name"],
            tier=result["tier"],
            scopes=result["scopes"],
            rate_limits=result["rate_limits"],
            expires_at=result["expires_at"],
            created_at=result["created_at"]
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get(
    "/api-keys",
    response_model=APIKeyListResponse,
    summary="Listar API Keys",
    description="Lista todas as API Keys do usuario",
    tags=["API Keys"]
)
async def list_user_api_keys(
    api_key: APIKeyInfo = Depends(get_api_key)
):
    """Lista API Keys (sem mostrar a chave completa)"""
    keys = list_api_keys(user_id=api_key.user_id)
    return APIKeyListResponse(api_keys=keys, total=len(keys))


@router.get(
    "/api-keys/me",
    summary="Info da API Key atual",
    description="Retorna informacoes da API Key usada nesta requisicao",
    tags=["API Keys"]
)
async def get_current_api_key_info(
    api_key: APIKeyInfo = Depends(get_api_key)
):
    """Retorna info da API Key atual"""
    stats = get_api_key_stats(api_key.key_id)
    return {
        "key_id": api_key.key_id,
        "name": api_key.name,
        "tier": api_key.tier,
        "scopes": api_key.scopes,
        "rate_limits": api_key.rate_limits,
        "usage": stats
    }


@router.delete(
    "/api-keys/{key_id}",
    response_model=SuccessResponse,
    summary="Revogar API Key",
    description="Revoga uma API Key permanentemente",
    tags=["API Keys"]
)
async def revoke_user_api_key(
    key_id: str = Path(..., description="ID da API Key"),
    api_key: APIKeyInfo = Depends(require_scope("admin"))
):
    """Revoga uma API Key"""
    success = revoke_api_key(key_id, user_id=api_key.user_id)
    if not success:
        raise HTTPException(status_code=404, detail="API Key nao encontrada")
    return SuccessResponse(message=f"API Key {key_id} revogada com sucesso")


# =============================================================================
# ENDPOINTS - PROJETOS
# =============================================================================

@router.get(
    "/projects",
    response_model=ProjectListResponse,
    summary="Listar Projetos",
    description="Lista projetos com paginacao",
    tags=["Projetos"]
)
async def list_projects(
    page: int = Query(1, ge=1, description="Pagina"),
    per_page: int = Query(20, ge=1, le=100, description="Itens por pagina"),
    status: Optional[str] = Query(None, description="Filtrar por status"),
    api_key: APIKeyInfo = Depends(get_api_key),
    _: None = Depends(rate_limit_by_api_key)
):
    """Lista projetos do usuario/organizacao"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            query = db.query(Project)
            if status:
                query = query.filter(Project.status == status)

            total = query.count()
            projects = query.offset((page - 1) * per_page).limit(per_page).all()

            return ProjectListResponse(
                projects=[
                    ProjectResponse(
                        project_id=p.project_id,
                        name=p.name,
                        description=p.description,
                        project_type=p.project_type,
                        status=p.status,
                        progress=p.progress or 0,
                        folder_path=p.folder_path,
                        github_url=p.github_url,
                        created_at=p.created_at.isoformat() if p.created_at else "",
                        updated_at=p.updated_at.isoformat() if p.updated_at else "",
                    ) for p in projects
                ],
                total=total,
                page=page,
                per_page=per_page,
                has_next=(page * per_page) < total
            )
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post(
    "/projects",
    response_model=ProjectResponse,
    status_code=201,
    summary="Criar Projeto",
    description="Cria um novo projeto",
    tags=["Projetos"]
)
async def create_project(
    data: ProjectCreate,
    api_key: APIKeyInfo = Depends(require_scope("write")),
    _: None = Depends(rate_limit_custom(10, 100))  # 10/min, 100/dia
):
    """Cria um novo projeto"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Project
        import uuid

        db = SessionLocal()
        try:
            project = Project(
                project_id=f"PRJ-{uuid.uuid4().hex[:8].upper()}",
                name=data.name,
                description=data.description,
                project_type=data.project_type,
                status="PLANNING",
                progress=0.0,
            )
            db.add(project)
            db.commit()
            db.refresh(project)

            return ProjectResponse(
                project_id=project.project_id,
                name=project.name,
                description=project.description,
                project_type=project.project_type,
                status=project.status,
                progress=project.progress or 0,
                folder_path=project.folder_path,
                github_url=project.github_url,
                created_at=project.created_at.isoformat(),
                updated_at=project.updated_at.isoformat(),
            )
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get(
    "/projects/{project_id}",
    response_model=ProjectResponse,
    summary="Buscar Projeto",
    description="Busca projeto por ID",
    tags=["Projetos"]
)
async def get_project(
    project_id: str = Path(..., description="ID do projeto"),
    api_key: APIKeyInfo = Depends(get_api_key),
    _: None = Depends(rate_limit_by_api_key)
):
    """Busca projeto por ID"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            project = db.query(Project).filter(Project.project_id == project_id).first()
            if not project:
                raise HTTPException(status_code=404, detail="Projeto nao encontrado")

            return ProjectResponse(
                project_id=project.project_id,
                name=project.name,
                description=project.description,
                project_type=project.project_type,
                status=project.status,
                progress=project.progress or 0,
                folder_path=project.folder_path,
                github_url=project.github_url,
                created_at=project.created_at.isoformat() if project.created_at else "",
                updated_at=project.updated_at.isoformat() if project.updated_at else "",
            )
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# ENDPOINTS - STORIES
# =============================================================================

@router.get(
    "/projects/{project_id}/stories",
    response_model=StoryListResponse,
    summary="Listar Stories",
    description="Lista stories de um projeto",
    tags=["Stories"]
)
async def list_stories(
    project_id: str = Path(..., description="ID do projeto"),
    status: Optional[str] = Query(None, description="Filtrar por status"),
    api_key: APIKeyInfo = Depends(get_api_key),
    _: None = Depends(rate_limit_by_api_key)
):
    """Lista stories do projeto"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            query = db.query(Story).filter(Story.project_id == project_id)
            if status:
                query = query.filter(Story.status == status)

            stories = query.order_by(Story.kanban_order).all()

            return StoryListResponse(
                stories=[
                    StoryResponse(
                        story_id=s.story_id,
                        project_id=s.project_id,
                        title=s.title,
                        narrative=f"Como um {s.persona or '[persona]'}, eu quero {s.action or '[acao]'}, para que {s.benefit or '[beneficio]'}",
                        status=s.status,
                        story_points=s.story_points or 0,
                        priority=s.priority or "medium",
                        progress=s.progress or 0,
                        tasks_total=s.tasks_total or 0,
                        tasks_completed=s.tasks_completed or 0,
                        created_at=s.created_at.isoformat() if s.created_at else "",
                    ) for s in stories
                ],
                total=len(stories)
            )
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post(
    "/projects/{project_id}/stories",
    response_model=StoryResponse,
    status_code=201,
    summary="Criar Story",
    description="Cria uma nova User Story",
    tags=["Stories"]
)
async def create_story(
    project_id: str = Path(..., description="ID do projeto"),
    data: StoryCreate = Body(...),
    api_key: APIKeyInfo = Depends(require_scope("write")),
    _: None = Depends(rate_limit_custom(20, 500))
):
    """Cria uma nova story"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Story, Project
        import uuid

        db = SessionLocal()
        try:
            # Verificar se projeto existe
            project = db.query(Project).filter(Project.project_id == project_id).first()
            if not project:
                raise HTTPException(status_code=404, detail="Projeto nao encontrado")

            story = Story(
                story_id=f"STR-{uuid.uuid4().hex[:6].upper()}",
                project_id=project_id,
                title=data.title,
                persona=data.persona,
                action=data.action,
                benefit=data.benefit,
                acceptance_criteria=data.acceptance_criteria,
                story_points=data.story_points,
                priority=data.priority,
                status="backlog",
            )
            db.add(story)
            db.commit()
            db.refresh(story)

            return StoryResponse(
                story_id=story.story_id,
                project_id=story.project_id,
                title=story.title,
                narrative=f"Como um {story.persona or '[persona]'}, eu quero {story.action or '[acao]'}, para que {story.benefit or '[beneficio]'}",
                status=story.status,
                story_points=story.story_points or 0,
                priority=story.priority or "medium",
                progress=story.progress or 0,
                tasks_total=story.tasks_total or 0,
                tasks_completed=story.tasks_completed or 0,
                created_at=story.created_at.isoformat(),
            )
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post(
    "/stories/{story_id}/execute",
    response_model=JobResponse,
    status_code=202,
    summary="Executar Story",
    description="Inicia desenvolvimento autonomo da story",
    tags=["Stories"]
)
async def execute_story(
    story_id: str = Path(..., description="ID da story"),
    api_key: APIKeyInfo = Depends(require_tier("basic")),  # Requer tier basic+
    _: None = Depends(rate_limit_custom(5, 50))  # 5/min, 50/dia
):
    """
    Inicia desenvolvimento autonomo da story.

    Requer tier **basic** ou superior.
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Story, Job
        import uuid

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if not story:
                raise HTTPException(status_code=404, detail="Story nao encontrada")

            # Criar job para a story
            job = Job(
                job_id=f"JOB-{uuid.uuid4().hex[:8].upper()}",
                project_id=story.project_id,
                description=f"Desenvolver story: {story.title}",
                status="pending",
                current_step="queued",
                progress=0,
            )
            db.add(job)

            # Atualizar status da story
            story.status = "in_progress"

            db.commit()
            db.refresh(job)

            return JobResponse(
                job_id=job.job_id,
                description=job.description,
                status=job.status,
                current_step=job.current_step,
                progress=job.progress or 0,
                output_path=job.output_path,
                files_created=job.artifacts or [],
                error_message=job.error_message,
                created_at=job.created_at.isoformat() if job.created_at else "",
                completed_at=job.completed_at.isoformat() if job.completed_at else None,
            )
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# ENDPOINTS - JOBS
# =============================================================================

@router.get(
    "/jobs",
    summary="Listar Jobs",
    description="Lista jobs de desenvolvimento",
    tags=["Jobs"]
)
async def list_jobs(
    status: Optional[str] = Query(None, description="Filtrar por status"),
    limit: int = Query(20, ge=1, le=100),
    api_key: APIKeyInfo = Depends(get_api_key),
    _: None = Depends(rate_limit_by_api_key)
):
    """Lista jobs"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            query = db.query(Job)
            if status:
                query = query.filter(Job.status == status)

            jobs = query.order_by(Job.created_at.desc()).limit(limit).all()

            return {
                "jobs": [
                    {
                        "job_id": j.job_id,
                        "description": j.description,
                        "status": j.status,
                        "current_step": j.current_step,
                        "progress": j.progress or 0,
                        "created_at": j.created_at.isoformat() if j.created_at else None,
                    } for j in jobs
                ],
                "total": len(jobs)
            }
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get(
    "/jobs/{job_id}",
    response_model=JobResponse,
    summary="Buscar Job",
    description="Busca job por ID",
    tags=["Jobs"]
)
async def get_job(
    job_id: str = Path(..., description="ID do job"),
    api_key: APIKeyInfo = Depends(get_api_key),
    _: None = Depends(rate_limit_by_api_key)
):
    """Busca job por ID"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            if not job:
                raise HTTPException(status_code=404, detail="Job nao encontrado")

            return JobResponse(
                job_id=job.job_id,
                description=job.description,
                status=job.status,
                current_step=job.current_step,
                progress=job.progress or 0,
                output_path=job.output_path,
                files_created=job.artifacts or [],
                error_message=job.error_message,
                created_at=job.created_at.isoformat() if job.created_at else "",
                completed_at=job.completed_at.isoformat() if job.completed_at else None,
            )
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post(
    "/jobs",
    response_model=JobResponse,
    status_code=201,
    summary="Criar Job",
    description="Cria um novo job de desenvolvimento",
    tags=["Jobs"]
)
async def create_job(
    data: JobCreate,
    api_key: APIKeyInfo = Depends(require_tier("basic")),
    _: None = Depends(rate_limit_custom(10, 100))
):
    """
    Cria um novo job de desenvolvimento.

    Requer tier **basic** ou superior.
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Job
        import uuid

        db = SessionLocal()
        try:
            job = Job(
                job_id=f"JOB-{uuid.uuid4().hex[:8].upper()}",
                description=data.description,
                tech_stack=data.tech_stack,
                features=data.features,
                status="pending",
                current_step="queued",
                progress=0,
            )
            db.add(job)
            db.commit()
            db.refresh(job)

            return JobResponse(
                job_id=job.job_id,
                description=job.description,
                status=job.status,
                current_step=job.current_step,
                progress=job.progress or 0,
                output_path=job.output_path,
                files_created=job.artifacts or [],
                error_message=job.error_message,
                created_at=job.created_at.isoformat() if job.created_at else "",
                completed_at=job.completed_at.isoformat() if job.completed_at else None,
            )
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# ENDPOINTS - WEBHOOKS
# =============================================================================

@router.get(
    "/webhooks",
    summary="Listar Webhooks",
    description="Lista webhooks configurados",
    tags=["Webhooks"]
)
async def list_webhooks(
    api_key: APIKeyInfo = Depends(require_scope("webhooks")),
    _: None = Depends(rate_limit_by_api_key)
):
    """Lista webhooks do usuario"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.api_models import Webhook

        db = SessionLocal()
        try:
            webhooks = db.query(Webhook).filter(
                Webhook.api_key_id == api_key.key_id
            ).all()

            return {
                "webhooks": [w.to_dict() for w in webhooks],
                "total": len(webhooks)
            }
        finally:
            db.close()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post(
    "/webhooks",
    response_model=WebhookResponse,
    status_code=201,
    summary="Criar Webhook",
    description="Cria um novo webhook",
    tags=["Webhooks"]
)
async def create_webhook(
    data: WebhookCreate,
    api_key: APIKeyInfo = Depends(require_scope("webhooks")),
    _: None = Depends(rate_limit_custom(10, 50))
):
    """Cria um novo webhook"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.api_models import Webhook
        import uuid

        # Validar eventos
        valid_events = [
            "project.created", "project.updated", "project.completed", "project.deleted",
            "story.created", "story.updated", "story.status_changed", "story.completed",
            "job.created", "job.started", "job.completed", "job.failed",
            "task.created", "task.completed"
        ]

        for event in data.events:
            if event not in valid_events:
                raise HTTPException(
                    status_code=400,
                    detail=f"Evento invalido: {event}. Eventos validos: {', '.join(valid_events)}"
                )

        db = SessionLocal()
        try:
            webhook = Webhook(
                webhook_id=f"WHK-{uuid.uuid4().hex[:8].upper()}",
                api_key_id=api_key.key_id,
                name=data.name,
                url=data.url,
                events=data.events,
                secret=data.secret or Webhook.generate_secret(),
                status="active",
            )
            db.add(webhook)
            db.commit()
            db.refresh(webhook)

            return WebhookResponse(
                webhook_id=webhook.webhook_id,
                name=webhook.name,
                url=webhook.url,
                events=webhook.events,
                status=webhook.status,
                deliveries_total=0,
                deliveries_failed=0,
                created_at=webhook.created_at.isoformat(),
            )
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.delete(
    "/webhooks/{webhook_id}",
    response_model=SuccessResponse,
    summary="Deletar Webhook",
    description="Remove um webhook",
    tags=["Webhooks"]
)
async def delete_webhook(
    webhook_id: str = Path(..., description="ID do webhook"),
    api_key: APIKeyInfo = Depends(require_scope("webhooks"))
):
    """Remove um webhook"""
    try:
        from factory.database.connection import SessionLocal
        from factory.database.api_models import Webhook

        db = SessionLocal()
        try:
            webhook = db.query(Webhook).filter(
                Webhook.webhook_id == webhook_id,
                Webhook.api_key_id == api_key.key_id
            ).first()

            if not webhook:
                raise HTTPException(status_code=404, detail="Webhook nao encontrado")

            db.delete(webhook)
            db.commit()

            return SuccessResponse(message=f"Webhook {webhook_id} removido")
        finally:
            db.close()
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
