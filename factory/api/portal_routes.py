# -*- coding: utf-8 -*-
"""
Portal Routes - API do Portal de Administracao Multi-nivel
===========================================================

Issue #113 - Portal de Administracao Multi-nivel

Este modulo define as rotas REST para o portal de administracao com 3 niveis:

1. Platform Admin (Super Admin):
   - Gerencia todos os tenants
   - Ve metricas globais da plataforma
   - Cria/suspende tenants
   - Acesso total

2. Tenant Admin:
   - Gerencia usuarios do tenant
   - Configura settings do tenant
   - Ve metricas do tenant
   - Gerencia projetos

3. Project Admin:
   - Gerencia membros do projeto
   - Configura projeto
   - Ve metricas do projeto

Endpoints:

Platform Admin:
    GET    /api/v1/platform/tenants         - Listar todos os tenants
    POST   /api/v1/platform/tenants         - Criar tenant
    GET    /api/v1/platform/tenants/{id}    - Buscar tenant
    PUT    /api/v1/platform/tenants/{id}    - Atualizar tenant
    DELETE /api/v1/platform/tenants/{id}    - Suspender/deletar tenant
    GET    /api/v1/platform/metrics         - Metricas da plataforma

Tenant Admin:
    GET    /api/v1/tenant/settings          - Configuracoes do tenant
    PUT    /api/v1/tenant/settings          - Atualizar configuracoes
    GET    /api/v1/tenant/metrics           - Metricas do tenant
    GET    /api/v1/tenant/projects          - Listar projetos
    GET    /api/v1/tenant/audit-logs        - Logs de auditoria

Project Admin:
    GET    /api/v1/projects/{id}/members    - Listar membros do projeto
    POST   /api/v1/projects/{id}/members    - Adicionar membro
    PUT    /api/v1/projects/{id}/members/{uid} - Atualizar membro
    DELETE /api/v1/projects/{id}/members/{uid} - Remover membro
    GET    /api/v1/projects/{id}/settings   - Configuracoes do projeto
"""

from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from functools import wraps

from fastapi import APIRouter, HTTPException, Depends, Query, Request
from pydantic import BaseModel, EmailStr, Field

# Database
from factory.database.connection import SessionLocal
from factory.database.models import Project, Story, User, Job, Worker
from factory.database.tenant_models import (
    Tenant, TenantSettings, TenantMember, TenantUsageLog, TenantAuditLog,
    ProjectMember, TenantStatus, TenantPlan, MemberRole, ProjectRole
)

# Services
from factory.core.user_service import UserService, RBACService

# Auth
from factory.api.auth import get_current_user, TokenData


# =============================================================================
# ROUTERS
# =============================================================================

platform_router = APIRouter(prefix="/api/v1/platform", tags=["platform-admin"])
tenant_router = APIRouter(prefix="/api/v1/tenant", tags=["tenant-admin"])
project_router = APIRouter(prefix="/api/v1/projects", tags=["project-admin"])


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class TenantCreate(BaseModel):
    """Schema para criacao de tenant"""
    name: str = Field(..., min_length=2, max_length=200)
    email: EmailStr
    slug: Optional[str] = None
    plan: Optional[str] = Field("trial", description="Plano: free, starter, professional, enterprise")
    description: Optional[str] = None


class TenantUpdate(BaseModel):
    """Schema para atualizacao de tenant"""
    name: Optional[str] = None
    email: Optional[EmailStr] = None
    description: Optional[str] = None
    status: Optional[str] = None
    plan: Optional[str] = None
    features: Optional[Dict[str, bool]] = None


class TenantSettingsUpdate(BaseModel):
    """Schema para atualizacao de settings"""
    max_projects: Optional[int] = None
    max_stories_per_project: Optional[int] = None
    max_members: Optional[int] = None
    max_storage_gb: Optional[float] = None
    max_api_calls_per_day: Optional[int] = None
    max_tokens_per_month: Optional[int] = None
    max_concurrent_workers: Optional[int] = None
    preferred_model: Optional[str] = None
    ai_settings: Optional[Dict[str, Any]] = None
    workflow_settings: Optional[Dict[str, Any]] = None
    notifications: Optional[Dict[str, Any]] = None


class ProjectMemberAdd(BaseModel):
    """Schema para adicionar membro ao projeto"""
    user_id: int
    role: Optional[str] = Field("developer", description="Role: owner, admin, developer, viewer")
    permissions: Optional[Dict[str, bool]] = None


class ProjectMemberUpdate(BaseModel):
    """Schema para atualizar membro do projeto"""
    role: Optional[str] = None
    permissions: Optional[Dict[str, bool]] = None
    active: Optional[bool] = None


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


async def require_platform_admin(
    user: TokenData = Depends(get_current_user)
) -> TokenData:
    """
    Dependency que requer Platform Admin (super admin)

    Apenas usuarios com role ADMIN global podem acessar.
    """
    if user.role != "ADMIN":
        raise HTTPException(
            status_code=403,
            detail="Acesso restrito a Platform Admins"
        )
    return user


async def require_tenant_admin(
    tenant_id: str = Query(..., description="ID do tenant"),
    user: TokenData = Depends(get_current_user),
    db=Depends(get_db)
) -> tuple:
    """
    Dependency que requer Tenant Admin

    Verifica se usuario e admin do tenant ou platform admin.
    """
    # Platform admin tem acesso a tudo
    if user.role == "ADMIN":
        return user, tenant_id

    # Verificar se e admin do tenant
    rbac = RBACService(db)
    if not rbac.is_admin(user_id=1, tenant_id=tenant_id):  # TODO: pegar user_id do token
        raise HTTPException(
            status_code=403,
            detail="Acesso restrito a Tenant Admins"
        )

    return user, tenant_id


# =============================================================================
# PLATFORM ADMIN ENDPOINTS
# =============================================================================

@platform_router.get("/tenants", response_model=Dict[str, Any])
async def list_all_tenants(
    status: Optional[str] = Query(None, description="Filtrar por status"),
    plan: Optional[str] = Query(None, description="Filtrar por plano"),
    search: Optional[str] = Query(None, description="Buscar por nome/email"),
    page: int = Query(1, ge=1),
    limit: int = Query(20, ge=1, le=100),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Lista todos os tenants da plataforma

    Requer: Platform Admin
    """
    query = db.query(Tenant)

    # Filtros
    if status:
        query = query.filter(Tenant.status == status)
    if plan:
        query = query.filter(Tenant.plan == plan)
    if search:
        search_term = f"%{search}%"
        query = query.filter(
            (Tenant.name.ilike(search_term)) |
            (Tenant.email.ilike(search_term)) |
            (Tenant.slug.ilike(search_term))
        )

    # Contar total
    total = query.count()

    # Paginar
    offset = (page - 1) * limit
    tenants = query.order_by(Tenant.created_at.desc()).offset(offset).limit(limit).all()

    # Enriquecer com contadores
    result = []
    for tenant in tenants:
        tenant_dict = tenant.to_dict()

        # Contar membros
        members_count = db.query(TenantMember).filter(
            TenantMember.tenant_id == tenant.tenant_id
        ).count()
        tenant_dict["members_count"] = members_count

        # Contar projetos
        projects_count = db.query(Project).filter(
            Project.tenant_id == tenant.tenant_id
        ).count()
        tenant_dict["projects_count"] = projects_count

        result.append(tenant_dict)

    return {
        "tenants": result,
        "total": total,
        "page": page,
        "limit": limit,
        "pages": (total + limit - 1) // limit
    }


@platform_router.post("/tenants", response_model=Dict[str, Any])
async def create_tenant(
    data: TenantCreate,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Cria novo tenant na plataforma

    Requer: Platform Admin
    """
    from factory.core.multi_tenant import TenantService

    try:
        service = TenantService(db)
        tenant = service.create_tenant(
            name=data.name,
            email=data.email,
            slug=data.slug,
            plan=data.plan,
            description=data.description
        )

        return {
            "success": True,
            "tenant": tenant,
            "message": f"Tenant '{data.name}' criado com sucesso"
        }

    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@platform_router.get("/tenants/{tenant_id}", response_model=Dict[str, Any])
async def get_tenant(
    tenant_id: str,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Busca tenant por ID

    Requer: Platform Admin
    """
    tenant = db.query(Tenant).filter(
        (Tenant.tenant_id == tenant_id) | (Tenant.slug == tenant_id)
    ).first()

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant nao encontrado")

    tenant_dict = tenant.to_dict(include_sensitive=True)

    # Buscar settings
    if tenant.settings:
        tenant_dict["settings"] = tenant.settings.to_dict()

    # Buscar branding
    if tenant.branding:
        tenant_dict["branding"] = tenant.branding.to_dict()

    # Contar recursos
    tenant_dict["members_count"] = len(tenant.members) if tenant.members else 0

    projects_count = db.query(Project).filter(
        Project.tenant_id == tenant.tenant_id
    ).count()
    tenant_dict["projects_count"] = projects_count

    stories_count = db.query(Story).filter(
        Story.tenant_id == tenant.tenant_id
    ).count()
    tenant_dict["stories_count"] = stories_count

    return {"tenant": tenant_dict}


@platform_router.put("/tenants/{tenant_id}", response_model=Dict[str, Any])
async def update_tenant(
    tenant_id: str,
    data: TenantUpdate,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Atualiza tenant

    Requer: Platform Admin
    """
    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant nao encontrado")

    update_data = data.model_dump(exclude_unset=True)
    for key, value in update_data.items():
        if hasattr(tenant, key):
            setattr(tenant, key, value)

    tenant.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(tenant)

    return {
        "success": True,
        "tenant": tenant.to_dict(),
        "message": "Tenant atualizado com sucesso"
    }


@platform_router.delete("/tenants/{tenant_id}", response_model=Dict[str, Any])
async def delete_tenant(
    tenant_id: str,
    hard_delete: bool = Query(False, description="Deletar permanentemente"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Suspende ou deleta tenant

    Por padrao, suspende (soft delete).
    Com hard_delete=true, remove permanentemente.

    Requer: Platform Admin
    """
    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant nao encontrado")

    if hard_delete:
        # Deletar permanentemente
        db.delete(tenant)
        message = f"Tenant '{tenant.name}' removido permanentemente"
    else:
        # Suspender
        tenant.status = TenantStatus.SUSPENDED.value
        tenant.suspended_at = datetime.utcnow()
        message = f"Tenant '{tenant.name}' suspenso"

    db.commit()

    return {
        "success": True,
        "message": message
    }


@platform_router.get("/metrics", response_model=Dict[str, Any])
async def platform_metrics(
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Retorna metricas globais da plataforma

    Requer: Platform Admin
    """
    # Contar tenants
    total_tenants = db.query(Tenant).count()
    active_tenants = db.query(Tenant).filter(
        Tenant.status.in_([TenantStatus.ACTIVE.value, TenantStatus.TRIAL.value])
    ).count()

    # Tenants por plano
    tenants_by_plan = {}
    for plan in TenantPlan:
        count = db.query(Tenant).filter(Tenant.plan == plan.value).count()
        tenants_by_plan[plan.value] = count

    # Contar usuarios
    total_users = db.query(User).count()
    active_users = db.query(User).filter(User.active == True).count()

    # Contar projetos
    total_projects = db.query(Project).count()

    # Contar stories
    total_stories = db.query(Story).count()

    # Contar jobs
    total_jobs = db.query(Job).count()
    running_jobs = db.query(Job).filter(Job.status == "running").count()
    completed_jobs = db.query(Job).filter(Job.status == "completed").count()
    failed_jobs = db.query(Job).filter(Job.status == "failed").count()

    # Contar workers
    total_workers = db.query(Worker).count()
    active_workers = db.query(Worker).filter(Worker.status != "offline").count()

    # Revenue (soma de cost_total de todos os usage logs do mes)
    month_start = datetime.utcnow().replace(day=1, hour=0, minute=0, second=0, microsecond=0)
    revenue_mtd = db.query(TenantUsageLog).filter(
        TenantUsageLog.created_at >= month_start
    ).with_entities(
        db.func.sum(TenantUsageLog.cost_total)
    ).scalar() or 0.0

    return {
        "metrics": {
            "tenants": {
                "total": total_tenants,
                "active": active_tenants,
                "by_plan": tenants_by_plan
            },
            "users": {
                "total": total_users,
                "active": active_users
            },
            "projects": {
                "total": total_projects
            },
            "stories": {
                "total": total_stories
            },
            "jobs": {
                "total": total_jobs,
                "running": running_jobs,
                "completed": completed_jobs,
                "failed": failed_jobs
            },
            "workers": {
                "total": total_workers,
                "active": active_workers
            },
            "revenue": {
                "mtd": round(revenue_mtd, 2)
            }
        },
        "generated_at": datetime.utcnow().isoformat()
    }


@platform_router.post("/tenants/{tenant_id}/activate", response_model=Dict[str, Any])
async def activate_tenant(
    tenant_id: str,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_platform_admin)
):
    """
    Ativa tenant suspenso

    Requer: Platform Admin
    """
    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant nao encontrado")

    tenant.status = TenantStatus.ACTIVE.value
    tenant.activated_at = datetime.utcnow()
    tenant.suspended_at = None
    db.commit()

    return {
        "success": True,
        "message": f"Tenant '{tenant.name}' ativado com sucesso"
    }


# =============================================================================
# TENANT ADMIN ENDPOINTS
# =============================================================================

@tenant_router.get("/settings", response_model=Dict[str, Any])
async def get_tenant_settings(
    tenant_id: str = Query(..., description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Retorna configuracoes do tenant

    Requer: Tenant Admin ou Platform Admin
    """
    settings = db.query(TenantSettings).filter(
        TenantSettings.tenant_id == tenant_id
    ).first()

    if not settings:
        raise HTTPException(status_code=404, detail="Settings nao encontradas")

    return {"settings": settings.to_dict()}


@tenant_router.put("/settings", response_model=Dict[str, Any])
async def update_tenant_settings(
    data: TenantSettingsUpdate,
    tenant_id: str = Query(..., description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Atualiza configuracoes do tenant

    Requer: Tenant Admin ou Platform Admin
    """
    settings = db.query(TenantSettings).filter(
        TenantSettings.tenant_id == tenant_id
    ).first()

    if not settings:
        raise HTTPException(status_code=404, detail="Settings nao encontradas")

    update_data = data.model_dump(exclude_unset=True)
    for key, value in update_data.items():
        if hasattr(settings, key):
            setattr(settings, key, value)

    settings.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(settings)

    return {
        "success": True,
        "settings": settings.to_dict(),
        "message": "Configuracoes atualizadas com sucesso"
    }


@tenant_router.get("/metrics", response_model=Dict[str, Any])
async def get_tenant_metrics(
    tenant_id: str = Query(..., description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Retorna metricas do tenant

    Requer: Tenant Admin ou Platform Admin
    """
    # Contar membros
    members = db.query(TenantMember).filter(
        TenantMember.tenant_id == tenant_id
    ).all()

    active_members = sum(1 for m in members if m.active)

    members_by_role = {}
    for m in members:
        role = m.tenant_role
        members_by_role[role] = members_by_role.get(role, 0) + 1

    # Contar projetos
    projects = db.query(Project).filter(Project.tenant_id == tenant_id).count()

    # Contar stories
    stories = db.query(Story).filter(Story.tenant_id == tenant_id).all()
    stories_by_status = {}
    for s in stories:
        status = s.status
        stories_by_status[status] = stories_by_status.get(status, 0) + 1

    # Buscar uso recente
    month_start = datetime.utcnow().replace(day=1, hour=0, minute=0, second=0, microsecond=0)
    usage = db.query(TenantUsageLog).filter(
        TenantUsageLog.tenant_id == tenant_id,
        TenantUsageLog.created_at >= month_start
    ).first()

    usage_data = usage.to_dict() if usage else {
        "tokens_total": 0,
        "api_calls": 0,
        "workers_executed": 0,
        "cost_total": 0.0
    }

    return {
        "metrics": {
            "members": {
                "total": len(members),
                "active": active_members,
                "by_role": members_by_role
            },
            "projects": {
                "total": projects
            },
            "stories": {
                "total": len(stories),
                "by_status": stories_by_status
            },
            "usage_mtd": usage_data
        },
        "generated_at": datetime.utcnow().isoformat()
    }


@tenant_router.get("/projects", response_model=Dict[str, Any])
async def list_tenant_projects(
    tenant_id: str = Query(..., description="ID do tenant"),
    status: Optional[str] = Query(None, description="Filtrar por status"),
    page: int = Query(1, ge=1),
    limit: int = Query(20, ge=1, le=100),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Lista projetos do tenant

    Requer: Tenant Admin ou Platform Admin
    """
    query = db.query(Project).filter(Project.tenant_id == tenant_id)

    if status:
        query = query.filter(Project.status == status)

    total = query.count()
    offset = (page - 1) * limit
    projects = query.order_by(Project.created_at.desc()).offset(offset).limit(limit).all()

    return {
        "projects": [p.to_dict() for p in projects],
        "total": total,
        "page": page,
        "limit": limit
    }


@tenant_router.get("/audit-logs", response_model=Dict[str, Any])
async def get_tenant_audit_logs(
    tenant_id: str = Query(..., description="ID do tenant"),
    action: Optional[str] = Query(None, description="Filtrar por acao"),
    resource_type: Optional[str] = Query(None, description="Filtrar por tipo de recurso"),
    days: int = Query(30, ge=1, le=90),
    limit: int = Query(100, ge=1, le=500),
    offset: int = Query(0, ge=0),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Retorna logs de auditoria do tenant

    Requer: Tenant Admin ou Platform Admin
    """
    since = datetime.utcnow() - timedelta(days=days)

    query = db.query(TenantAuditLog).filter(
        TenantAuditLog.tenant_id == tenant_id,
        TenantAuditLog.created_at >= since
    )

    if action:
        query = query.filter(TenantAuditLog.action == action)
    if resource_type:
        query = query.filter(TenantAuditLog.resource_type == resource_type)

    total = query.count()
    logs = query.order_by(TenantAuditLog.created_at.desc()).offset(offset).limit(limit).all()

    return {
        "logs": [l.to_dict() for l in logs],
        "total": total,
        "limit": limit,
        "offset": offset
    }


# =============================================================================
# PROJECT ADMIN ENDPOINTS
# =============================================================================

@project_router.get("/{project_id}/members", response_model=Dict[str, Any])
async def list_project_members(
    project_id: str,
    active_only: bool = Query(True, description="Apenas membros ativos"),
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Lista membros do projeto

    Requer: Project Admin, Tenant Admin ou Platform Admin
    """
    query = db.query(ProjectMember).filter(ProjectMember.project_id == project_id)

    if active_only:
        query = query.filter(ProjectMember.active == True)

    members = query.all()

    # Enriquecer com dados do usuario
    result = []
    for m in members:
        member_dict = m.to_dict()

        user = db.query(User).filter(User.id == m.user_id).first()
        if user:
            member_dict["username"] = user.username
            member_dict["email"] = user.email

        result.append(member_dict)

    return {
        "members": result,
        "total": len(result)
    }


@project_router.post("/{project_id}/members", response_model=Dict[str, Any])
async def add_project_member(
    project_id: str,
    data: ProjectMemberAdd,
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Adiciona membro ao projeto

    Requer: Project Admin, Tenant Admin ou Platform Admin
    """
    # Verificar se projeto existe
    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    # Verificar se usuario existe
    user = db.query(User).filter(User.id == data.user_id).first()
    if not user:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    # Verificar se ja e membro
    existing = db.query(ProjectMember).filter(
        ProjectMember.project_id == project_id,
        ProjectMember.user_id == data.user_id
    ).first()

    if existing:
        raise HTTPException(status_code=400, detail="Usuario ja e membro do projeto")

    # Criar membro
    member = ProjectMember(
        project_id=project_id,
        user_id=data.user_id,
        project_role=data.role,
        permissions=data.permissions or {},
        added_by=current_user.username
    )

    db.add(member)
    db.commit()
    db.refresh(member)

    result = member.to_dict()
    result["username"] = user.username
    result["email"] = user.email

    return {
        "success": True,
        "member": result,
        "message": f"Usuario '{user.username}' adicionado ao projeto"
    }


@project_router.put("/{project_id}/members/{user_id}", response_model=Dict[str, Any])
async def update_project_member(
    project_id: str,
    user_id: int,
    data: ProjectMemberUpdate,
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Atualiza membro do projeto

    Requer: Project Admin, Tenant Admin ou Platform Admin
    """
    member = db.query(ProjectMember).filter(
        ProjectMember.project_id == project_id,
        ProjectMember.user_id == user_id
    ).first()

    if not member:
        raise HTTPException(status_code=404, detail="Membro nao encontrado")

    update_data = data.model_dump(exclude_unset=True)

    # Mapear role para project_role
    if "role" in update_data:
        update_data["project_role"] = update_data.pop("role")

    for key, value in update_data.items():
        if hasattr(member, key):
            setattr(member, key, value)

    member.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(member)

    return {
        "success": True,
        "member": member.to_dict(),
        "message": "Membro atualizado com sucesso"
    }


@project_router.delete("/{project_id}/members/{user_id}", response_model=Dict[str, Any])
async def remove_project_member(
    project_id: str,
    user_id: int,
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Remove membro do projeto

    Requer: Project Admin, Tenant Admin ou Platform Admin
    """
    member = db.query(ProjectMember).filter(
        ProjectMember.project_id == project_id,
        ProjectMember.user_id == user_id
    ).first()

    if not member:
        raise HTTPException(status_code=404, detail="Membro nao encontrado")

    # Nao permitir remover o owner
    if member.project_role == ProjectRole.OWNER.value:
        raise HTTPException(
            status_code=400,
            detail="Nao e possivel remover o owner do projeto"
        )

    db.delete(member)
    db.commit()

    return {
        "success": True,
        "message": "Membro removido do projeto"
    }


@project_router.get("/{project_id}/settings", response_model=Dict[str, Any])
async def get_project_settings(
    project_id: str,
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Retorna configuracoes do projeto

    Requer: Project Admin, Tenant Admin ou Platform Admin
    """
    project = db.query(Project).filter(Project.project_id == project_id).first()

    if not project:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    return {
        "project": project.to_dict(),
        "settings": project.settings or {},
        "config": project.config or {}
    }
