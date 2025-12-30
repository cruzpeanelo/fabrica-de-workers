"""
Admin Routes - API v1
=====================

Endpoints de administracao do sistema.

Endpoints:
- /api/v1/admin/users - Gestao de usuarios
- /api/v1/admin/roles - Gestao de roles/permissoes
- /api/v1/admin/config - Configuracoes do sistema
- /api/v1/admin/audit - Logs de auditoria
"""

from datetime import datetime
from typing import List, Optional
import uuid

from fastapi import APIRouter, Depends, Query, Request, HTTPException, status
from pydantic import BaseModel, Field, EmailStr
from sqlalchemy.orm import Session

from .schemas import APIResponse, APIListResponse, PaginationMeta, RequestMeta, ErrorCodes
from factory.api.pagination import CursorPagination
from factory.database.connection import SessionLocal

router = APIRouter()
paginator = CursorPagination()


# =============================================================================
# MODELS
# =============================================================================

class UserCreate(BaseModel):
    """Criar usuario"""
    username: str = Field(..., min_length=3, max_length=100)
    email: Optional[EmailStr] = None
    password: str = Field(..., min_length=6)
    role: str = Field("VIEWER", description="Role: ADMIN, MANAGER, VIEWER")


class UserUpdate(BaseModel):
    """Atualizar usuario"""
    email: Optional[EmailStr] = None
    role: Optional[str] = None
    active: Optional[bool] = None
    quotas: Optional[dict] = None


class RoleCreate(BaseModel):
    """Criar role"""
    name: str = Field(..., min_length=2, max_length=100)
    description: Optional[str] = None
    permissions: List[str] = Field(default_factory=list)
    level: int = Field(0, ge=0, le=100)


class RoleUpdate(BaseModel):
    """Atualizar role"""
    name: Optional[str] = None
    description: Optional[str] = None
    permissions: Optional[List[str]] = None
    level: Optional[int] = None
    active: Optional[bool] = None


class ConfigUpdate(BaseModel):
    """Atualizar configuracao"""
    value: str
    description: Optional[str] = None


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_request_meta(request: Request) -> RequestMeta:
    return RequestMeta(
        request_id=request.headers.get("X-Request-ID", str(uuid.uuid4())),
        timestamp=datetime.utcnow(),
        tenant_id=request.headers.get("X-Tenant-ID"),
        api_version="v1"
    )


# =============================================================================
# USERS
# =============================================================================

@router.get("/users", response_model=APIListResponse, tags=["Users"])
async def list_users(
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    role: Optional[str] = Query(None),
    active: Optional[bool] = Query(None),
    search: Optional[str] = Query(None),
    db: Session = Depends(get_db)
):
    """
    Lista usuarios do sistema.
    """
    from factory.database.models import User

    query = db.query(User)

    if role:
        query = query.filter(User.role == role)
    if active is not None:
        query = query.filter(User.active == active)
    if search:
        search_term = f"%{search}%"
        query = query.filter(
            (User.username.ilike(search_term)) |
            (User.email.ilike(search_term))
        )

    result = paginator.paginate(
        query=query,
        model=User,
        cursor=cursor,
        limit=limit
    )

    # Remover senha dos resultados
    items = []
    for user in result.items:
        user_dict = user.to_dict()
        user_dict.pop("password_hash", None)
        items.append(user_dict)

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            limit=limit
        )
    )


@router.post("/users", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Users"])
async def create_user(
    user_data: UserCreate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Cria novo usuario.
    """
    from factory.database.models import User
    from factory.api.auth import get_password_hash

    # Verificar se username ja existe
    existing = db.query(User).filter(User.username == user_data.username).first()
    if existing:
        raise HTTPException(
            status_code=status.HTTP_409_CONFLICT,
            detail={
                "error_code": ErrorCodes.DUPLICATE_RESOURCE,
                "message": f"Usuario '{user_data.username}' ja existe"
            }
        )

    # Criar usuario
    user = User(
        username=user_data.username,
        email=user_data.email,
        password_hash=get_password_hash(user_data.password),
        role=user_data.role,
        active=True
    )

    db.add(user)
    db.commit()
    db.refresh(user)

    user_dict = user.to_dict()
    user_dict.pop("password_hash", None)

    return APIResponse(
        data=user_dict,
        meta=get_request_meta(request),
        message="Usuario criado com sucesso"
    )


@router.get("/users/{user_id}", response_model=APIResponse, tags=["Users"])
async def get_user(
    user_id: int,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Busca usuario por ID.
    """
    from factory.database.models import User

    user = db.query(User).filter(User.id == user_id).first()
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Usuario {user_id} nao encontrado"
            }
        )

    user_dict = user.to_dict()
    user_dict.pop("password_hash", None)

    return APIResponse(
        data=user_dict,
        meta=get_request_meta(request)
    )


@router.put("/users/{user_id}", response_model=APIResponse, tags=["Users"])
async def update_user(
    user_id: int,
    user_data: UserUpdate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Atualiza usuario.
    """
    from factory.database.models import User

    user = db.query(User).filter(User.id == user_id).first()
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Usuario {user_id} nao encontrado"
            }
        )

    update_data = user_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        if hasattr(user, field):
            setattr(user, field, value)

    user.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(user)

    user_dict = user.to_dict()
    user_dict.pop("password_hash", None)

    return APIResponse(
        data=user_dict,
        meta=get_request_meta(request),
        message="Usuario atualizado com sucesso"
    )


@router.delete("/users/{user_id}", response_model=APIResponse, tags=["Users"])
async def delete_user(
    user_id: int,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Remove usuario (desativa).
    """
    from factory.database.models import User

    user = db.query(User).filter(User.id == user_id).first()
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Usuario {user_id} nao encontrado"
            }
        )

    # Desativar ao inves de deletar
    user.active = False
    user.updated_at = datetime.utcnow()
    db.commit()

    return APIResponse(
        data={"user_id": user_id, "deactivated": True},
        meta=get_request_meta(request),
        message="Usuario desativado com sucesso"
    )


# =============================================================================
# ROLES
# =============================================================================

@router.get("/roles", response_model=APIListResponse, tags=["Roles"])
async def list_roles(
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    active: Optional[bool] = Query(None),
    db: Session = Depends(get_db)
):
    """
    Lista roles do sistema.
    """
    from factory.database.models import Role

    query = db.query(Role)

    if active is not None:
        query = query.filter(Role.active == active)

    result = paginator.paginate(
        query=query,
        model=Role,
        cursor=cursor,
        limit=limit
    )

    items = [role.to_dict() for role in result.items]

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            limit=limit
        )
    )


@router.post("/roles", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Roles"])
async def create_role(
    role_data: RoleCreate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Cria nova role.
    """
    from factory.database.models import Role
    import uuid

    # Verificar se nome ja existe
    existing = db.query(Role).filter(Role.name == role_data.name).first()
    if existing:
        raise HTTPException(
            status_code=status.HTTP_409_CONFLICT,
            detail={
                "error_code": ErrorCodes.DUPLICATE_RESOURCE,
                "message": f"Role '{role_data.name}' ja existe"
            }
        )

    role = Role(
        role_id=f"role_{uuid.uuid4().hex[:8]}",
        name=role_data.name,
        description=role_data.description,
        permissions=role_data.permissions,
        level=role_data.level,
        active=True
    )

    db.add(role)
    db.commit()
    db.refresh(role)

    return APIResponse(
        data=role.to_dict(),
        meta=get_request_meta(request),
        message="Role criada com sucesso"
    )


@router.get("/roles/{role_id}", response_model=APIResponse, tags=["Roles"])
async def get_role(
    role_id: str,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Busca role por ID.
    """
    from factory.database.models import Role

    role = db.query(Role).filter(Role.role_id == role_id).first()
    if not role:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Role {role_id} nao encontrada"
            }
        )

    return APIResponse(
        data=role.to_dict(),
        meta=get_request_meta(request)
    )


@router.put("/roles/{role_id}", response_model=APIResponse, tags=["Roles"])
async def update_role(
    role_id: str,
    role_data: RoleUpdate,
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Atualiza role.
    """
    from factory.database.models import Role

    role = db.query(Role).filter(Role.role_id == role_id).first()
    if not role:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Role {role_id} nao encontrada"
            }
        )

    if role.is_system:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail={
                "error_code": ErrorCodes.FORBIDDEN,
                "message": "Roles de sistema nao podem ser alteradas"
            }
        )

    update_data = role_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        if hasattr(role, field):
            setattr(role, field, value)

    role.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(role)

    return APIResponse(
        data=role.to_dict(),
        meta=get_request_meta(request),
        message="Role atualizada com sucesso"
    )


# =============================================================================
# AUDIT LOGS
# =============================================================================

@router.get("/audit", response_model=APIListResponse, tags=["Audit"])
async def list_audit_logs(
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(50, ge=1, le=200),
    user_id: Optional[int] = Query(None),
    action: Optional[str] = Query(None),
    resource: Optional[str] = Query(None),
    start_date: Optional[datetime] = Query(None),
    end_date: Optional[datetime] = Query(None),
    db: Session = Depends(get_db)
):
    """
    Lista logs de auditoria.
    """
    from factory.database.models import AuditLog

    query = db.query(AuditLog)

    if user_id:
        query = query.filter(AuditLog.user_id == user_id)
    if action:
        query = query.filter(AuditLog.action == action)
    if resource:
        query = query.filter(AuditLog.resource == resource)
    if start_date:
        query = query.filter(AuditLog.timestamp >= start_date)
    if end_date:
        query = query.filter(AuditLog.timestamp <= end_date)

    result = paginator.paginate(
        query=query,
        model=AuditLog,
        cursor=cursor,
        limit=limit,
        sort_field="timestamp",
        sort_order="desc"
    )

    items = [log.to_dict() for log in result.items]

    return APIListResponse(
        data=items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            limit=limit
        )
    )


@router.get("/audit/actions", response_model=APIResponse, tags=["Audit"])
async def list_audit_actions(
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Lista tipos de acoes de auditoria.
    """
    from factory.database.models import AuditLog
    from sqlalchemy import func

    # Buscar acoes distintas
    actions = db.query(
        AuditLog.action,
        func.count(AuditLog.id).label("count")
    ).group_by(AuditLog.action).all()

    return APIResponse(
        data={
            "actions": [
                {"action": a[0], "count": a[1]}
                for a in actions
            ]
        },
        meta=get_request_meta(request)
    )


# =============================================================================
# SYSTEM CONFIG
# =============================================================================

@router.get("/config", response_model=APIResponse, tags=["Config"])
async def get_system_config(
    request: Request
):
    """
    Retorna configuracoes do sistema.
    """
    from factory.config import (
        DASHBOARD_VERSION,
        MIN_WORKERS,
        MAX_WORKERS,
        RATE_LIMIT_ENABLED,
        RATE_LIMIT_REQUESTS,
        MULTI_TENANT_ENABLED,
        LLM_PROVIDER
    )

    config = {
        "version": DASHBOARD_VERSION,
        "workers": {
            "min": MIN_WORKERS,
            "max": MAX_WORKERS
        },
        "rate_limit": {
            "enabled": RATE_LIMIT_ENABLED,
            "requests_per_minute": RATE_LIMIT_REQUESTS
        },
        "features": {
            "multi_tenant": MULTI_TENANT_ENABLED,
            "llm_provider": LLM_PROVIDER
        }
    }

    return APIResponse(
        data=config,
        meta=get_request_meta(request)
    )


@router.get("/health", response_model=APIResponse, tags=["Health"])
async def admin_health_check(
    request: Request,
    db: Session = Depends(get_db)
):
    """
    Health check detalhado para administradores.
    """
    from factory.database.models import Project, Story, User

    # Verificar banco
    try:
        from sqlalchemy import text
        db.execute(text("SELECT 1"))
        db_status = "healthy"
    except Exception as e:
        db_status = f"unhealthy: {str(e)}"

    # Contagens
    projects_count = db.query(Project).count()
    stories_count = db.query(Story).count()
    users_count = db.query(User).count()

    return APIResponse(
        data={
            "status": "healthy" if db_status == "healthy" else "degraded",
            "timestamp": datetime.utcnow().isoformat(),
            "components": {
                "database": db_status,
                "api": "healthy"
            },
            "stats": {
                "projects": projects_count,
                "stories": stories_count,
                "users": users_count
            }
        },
        meta=get_request_meta(request)
    )
