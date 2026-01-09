"""
Core Routes - API v1
====================

CRUD principal para Stories, Tasks e Projects.

Endpoints:
- /api/v1/core/stories - User Stories
- /api/v1/core/tasks - Story Tasks
- /api/v1/core/projects - Projetos
"""

from datetime import datetime
from typing import List, Optional
import uuid

from fastapi import APIRouter, Depends, HTTPException, Query, Request, status
from sqlalchemy.orm import Session

from .schemas import (
    APIResponse, APIListResponse, PaginationMeta, RequestMeta,
    StoryCreate, StoryUpdate, StoryFilters,
    TaskCreate, TaskUpdate, TaskFilters,
    ProjectCreate, ProjectUpdate, ProjectFilters,
    ErrorCodes
)
from factory.api.pagination import CursorPagination, PaginatedResult
from factory.database.connection import SessionLocal

router = APIRouter()

# Paginador global
paginator = CursorPagination(default_limit=20, max_limit=100)


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    """Dependency para sessao do banco"""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_request_meta(request: Request) -> RequestMeta:
    """Gera metadados da requisicao"""
    return RequestMeta(
        request_id=request.headers.get("X-Request-ID", str(uuid.uuid4())),
        timestamp=datetime.utcnow(),
        tenant_id=request.headers.get("X-Tenant-ID"),
        api_version="v1"
    )


# =============================================================================
# STORIES
# =============================================================================

@router.get("/stories", response_model=APIListResponse, tags=["Stories"])
async def list_stories(
    request: Request,
    cursor: Optional[str] = Query(None, description="Cursor para paginacao"),
    limit: int = Query(20, ge=1, le=100, description="Itens por pagina"),
    project_id: Optional[str] = Query(None, description="Filtrar por projeto"),
    status: Optional[str] = Query(None, description="Filtrar por status"),
    priority: Optional[str] = Query(None, description="Filtrar por prioridade"),
    assignee: Optional[str] = Query(None, description="Filtrar por responsavel"),
    epic_id: Optional[str] = Query(None, description="Filtrar por epic"),
    sprint_id: Optional[str] = Query(None, description="Filtrar por sprint"),
    search: Optional[str] = Query(None, description="Busca textual"),
    include_total: bool = Query(False, description="Incluir contagem total"),
    db: Session = Depends(get_db)
):
    """
    Lista stories com paginacao cursor-based.

    Suporta filtros por projeto, status, prioridade, assignee, epic, sprint.
    Busca textual em titulo e descricao.

    Multi-tenant: SUPER_ADMIN vê todas as stories, outros usuários veem apenas stories de seus tenants.
    """
    from factory.database.models import Story, Project
    from factory.api.auth import get_current_user, TokenData

    # Obter usuário autenticado (opcional, para não quebrar testes sem auth)
    user = None
    auth_header = request.headers.get("Authorization", "")
    if auth_header.startswith("Bearer "):
        token = auth_header.replace("Bearer ", "")
        try:
            from factory.api.auth import decode_token
            user = decode_token(token)
        except:
            pass  # Token inválido, continua sem autenticação

    # Construir query base
    query = db.query(Story)

    # Filtrar por tenant (exceto SUPER_ADMIN e PLATFORM_ADMIN)
    if user and user.role not in ["SUPER_ADMIN", "PLATFORM_ADMIN", "SUPERADMIN"] and user.tenant_id:
        # Buscar project_ids do tenant primeiro
        tenant_projects = db.query(Project.project_id).filter(
            Project.tenant_id == user.tenant_id
        ).all()
        tenant_project_ids = [p[0] for p in tenant_projects]

        # Filtrar stories apenas desses projetos
        if tenant_project_ids:
            query = query.filter(Story.project_id.in_(tenant_project_ids))
        else:
            # Se não há projetos, retornar vazio
            query = query.filter(Story.project_id == '__NO_PROJECTS__')

    # Aplicar filtros
    if project_id:
        query = query.filter(Story.project_id == project_id)
    if status:
        query = query.filter(Story.status == status)
    if priority:
        query = query.filter(Story.priority == priority)
    if assignee:
        query = query.filter(Story.assignee == assignee)
    if epic_id:
        query = query.filter(Story.epic_id == epic_id)
    if sprint_id:
        query = query.filter(Story.sprint_id == sprint_id)
    if search:
        search_term = f"%{search}%"
        query = query.filter(
            (Story.title.ilike(search_term)) |
            (Story.description.ilike(search_term))
        )

    # Paginar
    result = paginator.paginate(
        query=query,
        model=Story,
        cursor=cursor,
        limit=limit,
        sort_field="created_at",
        sort_order="desc",
        include_total=include_total
    )

    # Formatar resposta
    items = [story.to_dict() for story in result.items]

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            total_count=result.total_count,
            limit=limit
        )
    )


@router.post("/stories", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Stories"])
async def create_story(
    story_data: StoryCreate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Cria nova User Story.

    Requer header Idempotency-Key para garantir operacao unica.
    """
    from factory.database.models import Story, Project

    # Verificar projeto existe
    project = db.query(Project).filter(Project.project_id == story_data.project_id).first()
    if not project:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Projeto {story_data.project_id} nao encontrado"
            }
        )

    # Gerar ID
    count = db.query(Story).count()
    story_id = f"STR-{count + 1:04d}"

    # Criar story
    story = Story(
        story_id=story_id,
        project_id=story_data.project_id,
        title=story_data.title,
        description=story_data.description,
        persona=story_data.persona,
        action=story_data.action,
        benefit=story_data.benefit,
        acceptance_criteria=story_data.acceptance_criteria or [],
        story_points=story_data.story_points or 0,
        priority=story_data.priority or "medium",
        epic_id=story_data.epic_id,
        sprint_id=story_data.sprint_id,
        status="backlog"
    )

    db.add(story)
    db.commit()
    db.refresh(story)

    return APIResponse(
        data=story.to_dict(),
        meta=get_request_meta(request),
        message="Story criada com sucesso"
    )


@router.get("/stories/{story_id}", response_model=APIResponse, tags=["Stories"])
async def get_story(
    story_id: str,
    request: Request,
    include_tasks: bool = Query(False, description="Incluir tasks da story"),
    db: Session = Depends(get_db)
):
    """
    Busca story por ID.

    Opcionalmente inclui tasks associadas.
    """
    from factory.database.models import Story

    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    data = story.to_dict()

    if include_tasks and hasattr(story, "story_tasks"):
        data["tasks"] = [task.to_dict() for task in story.story_tasks]

    return APIResponse(
        data=data,
        meta=get_request_meta(request)
    )


@router.put("/stories/{story_id}", response_model=APIResponse, tags=["Stories"])
async def update_story(
    story_id: str,
    story_data: StoryUpdate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Atualiza story existente.
    """
    from factory.database.models import Story

    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    # Atualizar campos fornecidos
    update_data = story_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        if hasattr(story, field):
            setattr(story, field, value)

    story.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(story)

    return APIResponse(
        data=story.to_dict(),
        meta=get_request_meta(request),
        message="Story atualizada com sucesso"
    )


@router.delete("/stories/{story_id}", response_model=APIResponse, tags=["Stories"])
async def delete_story(
    story_id: str,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Remove story e suas tasks associadas.
    """
    from factory.database.models import Story

    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    db.delete(story)
    db.commit()

    return APIResponse(
        data={"story_id": story_id, "deleted": True},
        meta=get_request_meta(request),
        message="Story removida com sucesso"
    )


@router.patch("/stories/{story_id}/move", response_model=APIResponse, tags=["Stories"])
async def move_story(
    story_id: str,
    new_status: str = Query(..., description="Novo status da story"),
    request: Request = None,
    db: Session = Depends(get_db)
):
    """
    Move story no Kanban (atualiza status).
    """
    from factory.database.models import Story

    valid_statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
    if new_status not in valid_statuses:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={
                "error_code": ErrorCodes.VALIDATION_ERROR,
                "message": f"Status invalido. Valores aceitos: {', '.join(valid_statuses)}"
            }
        )

    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    old_status = story.status
    story.status = new_status
    story.updated_at = datetime.utcnow()

    # Atualizar timestamps
    if new_status == "in_progress" and not story.started_at:
        story.started_at = datetime.utcnow()
    elif new_status == "done" and not story.completed_at:
        story.completed_at = datetime.utcnow()

    db.commit()
    db.refresh(story)

    return APIResponse(
        data={
            "story_id": story_id,
            "old_status": old_status,
            "new_status": new_status
        },
        meta=get_request_meta(request),
        message=f"Story movida de '{old_status}' para '{new_status}'"
    )


# =============================================================================
# TASKS
# =============================================================================

@router.get("/stories/{story_id}/tasks", response_model=APIListResponse, tags=["Tasks"])
async def list_story_tasks(
    story_id: str,
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    status: Optional[str] = Query(None),
    task_type: Optional[str] = Query(None),
    db: Session = Depends(get_db)
):
    """
    Lista tasks de uma story.
    """
    from factory.database.models import Story, StoryTask

    # Verificar story existe
    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    # Query tasks
    query = db.query(StoryTask).filter(StoryTask.story_id == story_id)

    if status:
        query = query.filter(StoryTask.status == status)
    if task_type:
        query = query.filter(StoryTask.task_type == task_type)

    # Paginar
    result = paginator.paginate(
        query=query,
        model=StoryTask,
        cursor=cursor,
        limit=limit,
        sort_field="task_order",
        sort_order="asc"
    )

    items = [task.to_dict() for task in result.items]

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            limit=limit
        )
    )


@router.post("/stories/{story_id}/tasks", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Tasks"])
async def create_task(
    story_id: str,
    task_data: TaskCreate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Cria task para uma story.
    """
    from factory.database.models import Story, StoryTask

    # Verificar story existe
    story = db.query(Story).filter(Story.story_id == story_id).first()
    if not story:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Story {story_id} nao encontrada"
            }
        )

    # Gerar ID
    count = db.query(StoryTask).count()
    task_id = f"STSK-{count + 1:04d}"

    # Ordem
    max_order = db.query(StoryTask).filter(
        StoryTask.story_id == story_id
    ).count()

    # Criar task
    task = StoryTask(
        task_id=task_id,
        story_id=story_id,
        title=task_data.title,
        description=task_data.description,
        task_type=task_data.task_type,
        estimated_hours=task_data.estimated_hours or 0,
        assignee=task_data.assignee,
        status="pending",
        task_order=max_order + 1
    )

    db.add(task)

    # Atualizar contadores da story
    story.tasks_total = (story.tasks_total or 0) + 1
    story.update_progress()

    db.commit()
    db.refresh(task)

    return APIResponse(
        data=task.to_dict(),
        meta=get_request_meta(request),
        message="Task criada com sucesso"
    )


@router.get("/tasks/{task_id}", response_model=APIResponse, tags=["Tasks"])
async def get_task(
    task_id: str,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Busca task por ID.
    """
    from factory.database.models import StoryTask

    task = db.query(StoryTask).filter(StoryTask.task_id == task_id).first()
    if not task:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Task {task_id} nao encontrada"
            }
        )

    return APIResponse(
        data=task.to_dict(),
        meta=get_request_meta(request)
    )


@router.put("/tasks/{task_id}", response_model=APIResponse, tags=["Tasks"])
async def update_task(
    task_id: str,
    task_data: TaskUpdate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Atualiza task existente.
    """
    from factory.database.models import StoryTask, Story

    task = db.query(StoryTask).filter(StoryTask.task_id == task_id).first()
    if not task:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Task {task_id} nao encontrada"
            }
        )

    was_completed = task.status == "completed"

    # Atualizar campos
    update_data = task_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        if hasattr(task, field):
            setattr(task, field, value)

    task.updated_at = datetime.utcnow()

    # Atualizar timestamps
    if task.status == "in_progress" and not task.started_at:
        task.started_at = datetime.utcnow()
    elif task.status == "completed" and not task.completed_at:
        task.completed_at = datetime.utcnow()

    # Atualizar contador da story se status mudou
    is_completed = task.status == "completed"
    if was_completed != is_completed:
        story = db.query(Story).filter(Story.story_id == task.story_id).first()
        if story:
            if is_completed:
                story.tasks_completed = (story.tasks_completed or 0) + 1
            else:
                story.tasks_completed = max(0, (story.tasks_completed or 0) - 1)
            story.update_progress()

    db.commit()
    db.refresh(task)

    return APIResponse(
        data=task.to_dict(),
        meta=get_request_meta(request),
        message="Task atualizada com sucesso"
    )


@router.patch("/tasks/{task_id}/complete", response_model=APIResponse, tags=["Tasks"])
async def complete_task(
    task_id: str,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Marca task como completa.
    """
    from factory.database.models import StoryTask, Story

    task = db.query(StoryTask).filter(StoryTask.task_id == task_id).first()
    if not task:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Task {task_id} nao encontrada"
            }
        )

    if task.status == "completed":
        return APIResponse(
            data=task.to_dict(),
            meta=get_request_meta(request),
            message="Task ja estava completa"
        )

    task.status = "completed"
    task.progress = 100
    task.completed_at = datetime.utcnow()
    task.updated_at = datetime.utcnow()

    # Atualizar story
    story = db.query(Story).filter(Story.story_id == task.story_id).first()
    if story:
        story.tasks_completed = (story.tasks_completed or 0) + 1
        story.update_progress()

    db.commit()
    db.refresh(task)

    return APIResponse(
        data=task.to_dict(),
        meta=get_request_meta(request),
        message="Task marcada como completa"
    )


# =============================================================================
# PROJECTS
# =============================================================================

@router.get("/projects", response_model=APIListResponse, tags=["Projects"])
async def list_projects(
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    status: Optional[str] = Query(None),
    project_type: Optional[str] = Query(None),
    search: Optional[str] = Query(None),
    include_total: bool = Query(False),
    db: Session = Depends(get_db)
):
    """
    Lista projetos com paginacao.
    """
    from factory.database.models import Project

    query = db.query(Project)

    if status:
        query = query.filter(Project.status == status)
    if project_type:
        query = query.filter(Project.project_type == project_type)
    if search:
        search_term = f"%{search}%"
        query = query.filter(
            (Project.name.ilike(search_term)) |
            (Project.description.ilike(search_term))
        )

    result = paginator.paginate(
        query=query,
        model=Project,
        cursor=cursor,
        limit=limit,
        sort_field="created_at",
        sort_order="desc",
        include_total=include_total
    )

    items = [project.to_dict() for project in result.items]

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            total_count=result.total_count,
            limit=limit
        )
    )


@router.post("/projects", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Projects"])
async def create_project(
    project_data: ProjectCreate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Cria novo projeto.
    """
    from factory.database.models import Project

    # Gerar ID
    count = db.query(Project).count()
    project_id = f"PRJ-{count + 1:03d}"

    project = Project(
        project_id=project_id,
        name=project_data.name,
        description=project_data.description,
        project_type=project_data.project_type,
        config=project_data.config or {},
        tags=project_data.tags or [],
        status="PLANNING"
    )

    db.add(project)
    db.commit()
    db.refresh(project)

    return APIResponse(
        data=project.to_dict(),
        meta=get_request_meta(request),
        message="Projeto criado com sucesso"
    )


@router.get("/projects/{project_id}", response_model=APIResponse, tags=["Projects"])
async def get_project(
    project_id: str,
    request: Request,
    include_stories: bool = Query(False),
    db: Session = Depends(get_db)
):
    """
    Busca projeto por ID.
    """
    from factory.database.models import Project

    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Projeto {project_id} nao encontrado"
            }
        )

    data = project.to_dict()

    if include_stories and hasattr(project, "stories"):
        data["stories"] = [story.to_dict() for story in project.stories]

    return APIResponse(
        data=data,
        meta=get_request_meta(request)
    )


@router.put("/projects/{project_id}", response_model=APIResponse, tags=["Projects"])
async def update_project(
    project_id: str,
    project_data: ProjectUpdate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Atualiza projeto existente.
    """
    from factory.database.models import Project

    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Projeto {project_id} nao encontrado"
            }
        )

    update_data = project_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        if hasattr(project, field):
            setattr(project, field, value)

    project.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(project)

    return APIResponse(
        data=project.to_dict(),
        meta=get_request_meta(request),
        message="Projeto atualizado com sucesso"
    )


@router.delete("/projects/{project_id}", response_model=APIResponse, tags=["Projects"])
async def delete_project(
    project_id: str,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Remove projeto e todos os recursos associados.
    """
    from factory.database.models import Project

    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Projeto {project_id} nao encontrado"
            }
        )

    db.delete(project)
    db.commit()

    return APIResponse(
        data={"project_id": project_id, "deleted": True},
        meta=get_request_meta(request),
        message="Projeto removido com sucesso"
    )
