# -*- coding: utf-8 -*-
"""
Admin Routes - Rotas de Administracao de Usuarios
===================================================

Issue #87 - Painel de Administracao de Usuarios
Issue #TBD - Sistema de Multiplos Perfis por Usuario

Este modulo define as rotas REST para administracao de usuarios:

Usuarios:
    GET    /api/v1/admin/users              - Listar usuarios
    POST   /api/v1/admin/users              - Criar usuario
    GET    /api/v1/admin/users/{id}         - Buscar usuario
    PUT    /api/v1/admin/users/{id}         - Atualizar usuario
    DELETE /api/v1/admin/users/{id}         - Desativar usuario

Roles (Legacy - Deprecated):
    GET    /api/v1/admin/users/{id}/roles   - Listar roles do usuario
    POST   /api/v1/admin/users/{id}/roles   - Atribuir role

Profiles (New - Multiple Profiles System):
    POST   /api/v1/admin/users/{id}/profiles            - Atribuir multiplos perfis
    GET    /api/v1/admin/users/{id}/profiles            - Listar perfis do usuario
    DELETE /api/v1/admin/users/{id}/profiles/{profile}  - Remover perfil

Convites:
    GET    /api/v1/admin/users/invites      - Listar convites
    POST   /api/v1/admin/users/invite       - Enviar convite
    DELETE /api/v1/admin/users/invites/{id} - Revogar convite

Estatisticas:
    GET    /api/v1/admin/users/stats        - Estatisticas de usuarios
"""

from datetime import datetime
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Depends, Query
from pydantic import BaseModel, EmailStr, Field

# Database
from factory.database.connection import SessionLocal

# Services
from factory.core.user_service import (
    UserService,
    RBACService,
    assign_profiles,
    get_user_profiles,
    remove_user_profile
)

# Auth
from factory.api.auth import get_current_user, TokenData


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/v1/admin", tags=["admin"])


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class UserCreate(BaseModel):
    """Schema para criacao de usuario"""
    username: str = Field(..., min_length=3, max_length=100)
    email: EmailStr
    password: str = Field(..., min_length=8)
    role: Optional[str] = Field("VIEWER", description="Role global: ADMIN, DEVELOPER, VIEWER")
    tenant_role: Optional[str] = Field("member", description="Role no tenant: owner, admin, member, viewer")


class UserUpdate(BaseModel):
    """Schema para atualizacao de usuario"""
    email: Optional[EmailStr] = None
    role: Optional[str] = None
    active: Optional[bool] = None
    password: Optional[str] = None
    quotas: Optional[Dict[str, Any]] = None
    billing: Optional[Dict[str, Any]] = None


class RoleAssignment(BaseModel):
    """Schema para atribuicao de role"""
    role: str = Field(..., description="Nova role a atribuir")
    tenant_id: Optional[str] = Field(None, description="ID do tenant (para role de tenant)")


class UserInvite(BaseModel):
    """Schema para convite de usuario"""
    email: EmailStr
    role: Optional[str] = Field("member", description="Role no tenant")
    message: Optional[str] = Field(None, description="Mensagem personalizada")
    expires_in_days: Optional[int] = Field(7, ge=1, le=30)


class UserFilters(BaseModel):
    """Filtros para listagem de usuarios"""
    status: Optional[str] = Field(None, description="Status: active, inactive, pending")
    role: Optional[str] = Field(None, description="Role do usuario")
    search: Optional[str] = Field(None, description="Busca por nome ou email")


class AssignProfilesRequest(BaseModel):
    """Schema para atribuição de múltiplos perfis a um usuário"""
    profile_ids: List[str] = Field(..., min_items=1, description="Lista de profile_ids para atribuir")
    scope: str = Field("global", description="Escopo: global, tenant, project")
    scope_id: Optional[str] = Field(None, description="ID do tenant ou projeto (se scope != global)")


class ProfileAssignmentResponse(BaseModel):
    """Schema de resposta de perfil atribuído"""
    profile_id: str
    profile_name: str
    scope: str
    scope_id: Optional[str]
    is_primary: bool
    active: bool
    assigned_at: Optional[str]


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


def get_current_tenant(
    tenant_id: Optional[str] = Query(None, description="ID do tenant")
) -> Optional[str]:
    """Dependency para obter tenant atual"""
    return tenant_id


async def require_admin(
    user: TokenData = Depends(get_current_user)
) -> TokenData:
    """
    Dependency que requer usuario admin

    Verifica se o usuario tem role ADMIN ou e admin do tenant.
    """
    if user.role not in ["ADMIN", "admin"]:
        raise HTTPException(
            status_code=403,
            detail="Acesso restrito a administradores"
        )
    return user


# =============================================================================
# ENDPOINTS - USUARIOS
# =============================================================================

@router.get("/users", response_model=Dict[str, Any])
async def list_users(
    tenant_id: Optional[str] = Query(None, description="Filtrar por tenant"),
    page: int = Query(1, ge=1, description="Pagina"),
    limit: int = Query(20, ge=1, le=100, description="Itens por pagina"),
    status: Optional[str] = Query(None, description="Status: active, inactive, pending"),
    role: Optional[str] = Query(None, description="Role do usuario"),
    search: Optional[str] = Query(None, description="Busca por nome ou email"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Lista usuarios do tenant com filtros

    Requer: role admin
    """
    service = UserService(db)

    try:
        filters = {
            "status": status,
            "role": role,
            "search": search
        }
        # Remove None values
        filters = {k: v for k, v in filters.items() if v is not None}

        result = service.list_users(
            tenant_id=tenant_id,
            page=page,
            limit=limit,
            filters=filters
        )

        return result

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/users", response_model=Dict[str, Any])
async def create_user(
    user: UserCreate,
    tenant_id: str = Query(..., description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Cria novo usuario no tenant

    Requer: role admin
    """
    service = UserService(db)

    try:
        new_user = service.create_user(
            tenant_id=tenant_id,
            username=user.username,
            email=user.email,
            password=user.password,
            role=user.role,
            tenant_role=user.tenant_role
        )

        return {
            "success": True,
            "user": new_user,
            "message": f"Usuario '{user.username}' criado com sucesso"
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/users/{user_id}", response_model=Dict[str, Any])
async def get_user(
    user_id: int,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Busca usuario por ID

    Requer: role admin
    """
    service = UserService(db)
    user = service.get_user(user_id)

    if not user:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    return {"user": user}


@router.put("/users/{user_id}", response_model=Dict[str, Any])
async def update_user(
    user_id: int,
    data: UserUpdate,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Atualiza usuario

    Requer: role admin
    """
    service = UserService(db)

    try:
        update_data = data.model_dump(exclude_unset=True)
        user = service.update_user(user_id, **update_data)

        if not user:
            raise HTTPException(status_code=404, detail="Usuario nao encontrado")

        return {
            "success": True,
            "user": user,
            "message": "Usuario atualizado com sucesso"
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/users/{user_id}", response_model=Dict[str, Any])
async def delete_user(
    user_id: int,
    hard_delete: bool = Query(False, description="Deletar permanentemente"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Desativa usuario (soft delete)

    Com hard_delete=true, remove permanentemente.

    Requer: role admin
    """
    service = UserService(db)

    success = service.delete_user(user_id, hard_delete=hard_delete)

    if not success:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    action = "removido" if hard_delete else "desativado"
    return {
        "success": True,
        "message": f"Usuario {action} com sucesso"
    }


@router.post("/users/{user_id}/activate", response_model=Dict[str, Any])
async def activate_user(
    user_id: int,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Reativa usuario desativado

    Requer: role admin
    """
    service = UserService(db)

    success = service.activate_user(user_id)

    if not success:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    return {
        "success": True,
        "message": "Usuario ativado com sucesso"
    }


# =============================================================================
# ENDPOINTS - ROLES
# =============================================================================

@router.get("/users/{user_id}/roles", response_model=Dict[str, Any])
async def get_user_roles(
    user_id: int,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Lista todas as roles do usuario

    Retorna role global e roles em cada tenant.

    Requer: role admin
    """
    service = UserService(db)
    roles = service.get_user_roles(user_id)

    if not roles:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    return {"roles": roles}


@router.post("/users/{user_id}/roles", response_model=Dict[str, Any])
async def assign_role(
    user_id: int,
    data: RoleAssignment,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Atribui role ao usuario

    Se tenant_id for especificado, atribui role no tenant.
    Caso contrario, atribui role global.

    Requer: role admin
    """
    service = UserService(db)

    try:
        result = service.assign_role(
            user_id=user_id,
            role=data.role,
            tenant_id=data.tenant_id
        )

        return {
            "success": True,
            "result": result,
            "message": f"Role '{data.role}' atribuida com sucesso"
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


# =============================================================================
# ENDPOINTS - PROFILES (Multiple Profiles per User)
# =============================================================================

@router.post("/users/{user_id}/profiles", response_model=Dict[str, Any])
async def assign_user_profiles(
    user_id: int,
    data: AssignProfilesRequest,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Atribui múltiplos perfis a um usuário

    Body:
    {
        "profile_ids": ["dev_frontend", "qa_manual"],
        "scope": "global",  // ou "tenant", "project"
        "scope_id": null    // ou tenant_id/project_id
    }

    Regras de hierarquia:
    - SUPER_ADMIN: Pode atribuir qualquer perfil
    - ADMIN: Pode atribuir perfis com level >= 10 (não pode criar SUPER_ADMIN)
    - Tenant Admin não pode criar outro SUPER_ADMIN
    - Tenant Admin não pode se auto-demover

    Requer: role admin
    """
    try:
        # Validar hierarquia: verificar se current_user pode atribuir os perfis solicitados
        if not _can_assign_profiles(current_user, data.profile_ids, db):
            raise HTTPException(
                status_code=403,
                detail="Você não tem permissão para atribuir esses perfis. "
                       "ADMIN não pode criar SUPER_ADMIN."
            )

        # Validar que não está tentando se auto-demover
        if user_id == current_user.user_id and current_user.role == "ADMIN":
            # Verificar se está removendo o perfil admin de si mesmo
            from factory.database.models import Profile
            profiles = db.query(Profile).filter(Profile.profile_id.in_(data.profile_ids)).all()
            admin_profile_exists = any(p.profile_id in ["super_admin", "admin"] for p in profiles)

            if not admin_profile_exists:
                raise HTTPException(
                    status_code=403,
                    detail="Você não pode remover seu próprio perfil de administrador"
                )

        # Atribuir perfis
        result = assign_profiles(
            user_id=user_id,
            profile_ids=data.profile_ids,
            scope=data.scope,
            scope_id=data.scope_id,
            assigned_by=current_user.user_id,
            db=db
        )

        # Buscar perfis atualizados
        profiles = get_user_profiles(
            user_id=user_id,
            scope=data.scope,
            scope_id=data.scope_id,
            db=db
        )

        return {
            "success": True,
            "user_id": user_id,
            "profiles_removed": result.get("profiles_removed", 0),
            "profiles_added": result.get("profiles_added", 0),
            "profiles": profiles,
            "message": f"{len(data.profile_ids)} perfis atribuídos com sucesso"
        }

    except HTTPException:
        raise
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Erro ao atribuir perfis: {str(e)}")


@router.get("/users/{user_id}/profiles", response_model=Dict[str, Any])
async def get_user_profiles_endpoint(
    user_id: int,
    scope: str = Query("global", description="Escopo: global, tenant, project"),
    scope_id: Optional[str] = Query(None, description="ID do tenant ou projeto"),
    include_inactive: bool = Query(False, description="Incluir perfis inativos"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Lista perfis atribuídos a um usuário

    Query params:
    - scope: Escopo dos perfis (global, tenant, project)
    - scope_id: ID do tenant ou projeto (se scope != global)
    - include_inactive: Incluir perfis inativos

    Requer: role admin
    """
    try:
        profiles = get_user_profiles(
            user_id=user_id,
            scope=scope,
            scope_id=scope_id,
            include_inactive=include_inactive,
            db=db
        )

        return {
            "user_id": user_id,
            "scope": scope,
            "scope_id": scope_id,
            "profile_count": len(profiles),
            "profiles": profiles
        }

    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Erro ao buscar perfis: {str(e)}")


@router.delete("/users/{user_id}/profiles/{profile_id}", response_model=Dict[str, Any])
async def remove_user_profile_endpoint(
    user_id: int,
    profile_id: str,
    scope: str = Query("global", description="Escopo: global, tenant, project"),
    scope_id: Optional[str] = Query(None, description="ID do tenant ou projeto"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Remove um perfil específico de um usuário

    Regras:
    - Admin não pode remover seu próprio perfil de administrador
    - Admin não pode remover perfis de SUPER_ADMIN

    Requer: role admin
    """
    try:
        # Validar que não está tentando se auto-demover
        if user_id == current_user.user_id and profile_id in ["super_admin", "admin"]:
            raise HTTPException(
                status_code=403,
                detail="Você não pode remover seu próprio perfil de administrador"
            )

        # Validar hierarquia
        if current_user.role == "ADMIN" and profile_id == "super_admin":
            raise HTTPException(
                status_code=403,
                detail="ADMIN não pode remover perfis de SUPER_ADMIN"
            )

        # Remover perfil
        success = remove_user_profile(
            user_id=user_id,
            profile_id=profile_id,
            scope=scope,
            scope_id=scope_id,
            db=db
        )

        if not success:
            raise HTTPException(
                status_code=404,
                detail=f"Perfil '{profile_id}' não encontrado para este usuário"
            )

        return {
            "success": True,
            "message": f"Perfil '{profile_id}' removido com sucesso"
        }

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Erro ao remover perfil: {str(e)}")


def _can_assign_profiles(current_user: TokenData, profile_ids: List[str], db) -> bool:
    """
    Valida se current_user pode atribuir os profiles solicitados

    Regras:
    - SUPER_ADMIN pode atribuir qualquer perfil
    - ADMIN pode atribuir perfis com level >= 10 (não pode criar SUPER_ADMIN)
    """
    user_role = current_user.role

    if user_role == "SUPER_ADMIN":
        return True  # Pode atribuir qualquer perfil

    if user_role == "ADMIN":
        # Buscar os perfis solicitados
        from factory.database.models import Profile
        profiles = db.query(Profile).filter(Profile.profile_id.in_(profile_ids)).all()

        # ADMIN não pode atribuir SUPER_ADMIN
        for profile in profiles:
            if profile.profile_id == "super_admin":
                return False
            if profile.level < 10:  # Níveis reservados
                return False

        return True

    return False  # Outros roles não podem atribuir perfis


# =============================================================================
# ENDPOINTS - CONVITES
# =============================================================================

@router.get("/users/invites", response_model=Dict[str, Any])
async def list_invites(
    tenant_id: str = Query(..., description="ID do tenant"),
    status: Optional[str] = Query(None, description="Status do convite: pending, accepted, expired, revoked"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Lista convites do tenant

    Requer: role admin
    """
    service = UserService(db)
    invites = service.list_invites(tenant_id, status)

    return {
        "invites": invites,
        "total": len(invites)
    }


@router.post("/users/invite", response_model=Dict[str, Any])
async def invite_user(
    invite: UserInvite,
    tenant_id: str = Query(..., description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Envia convite por email

    Requer: role admin
    """
    service = UserService(db)

    try:
        result = service.send_invite(
            tenant_id=tenant_id,
            email=invite.email,
            role=invite.role,
            message=invite.message,
            invited_by=current_user.username,
            expires_in_days=invite.expires_in_days
        )

        return {
            "success": True,
            "invite": result,
            "message": f"Convite enviado para '{invite.email}'"
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.delete("/users/invites/{invite_id}", response_model=Dict[str, Any])
async def revoke_invite(
    invite_id: str,
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Revoga convite pendente

    Requer: role admin
    """
    service = UserService(db)
    success = service.revoke_invite(invite_id)

    if not success:
        raise HTTPException(status_code=404, detail="Convite nao encontrado")

    return {
        "success": True,
        "message": "Convite revogado com sucesso"
    }


# =============================================================================
# ENDPOINTS - ESTATISTICAS
# =============================================================================

@router.get("/users/stats", response_model=Dict[str, Any])
async def get_user_stats(
    tenant_id: Optional[str] = Query(None, description="ID do tenant (None = global)"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Retorna estatisticas de usuarios

    Se tenant_id for especificado, retorna stats do tenant.
    Caso contrario, retorna stats globais.

    Requer: role admin
    """
    service = UserService(db)
    stats = service.get_user_stats(tenant_id)

    return {"stats": stats}


# =============================================================================
# ENDPOINTS - PERMISSOES
# =============================================================================

@router.get("/users/{user_id}/permissions", response_model=Dict[str, Any])
async def get_user_permissions(
    user_id: int,
    tenant_id: Optional[str] = Query(None, description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Lista permissoes do usuario

    Requer: role admin
    """
    rbac = RBACService(db)
    permissions = rbac.get_user_permissions(user_id, tenant_id)

    return {
        "user_id": user_id,
        "tenant_id": tenant_id,
        "permissions": permissions,
        "is_admin": rbac.is_admin(user_id, tenant_id)
    }


@router.post("/users/{user_id}/check-permission", response_model=Dict[str, Any])
async def check_permission(
    user_id: int,
    permission: str = Query(..., description="Permissao a verificar"),
    tenant_id: Optional[str] = Query(None, description="ID do tenant"),
    db=Depends(get_db),
    current_user: TokenData = Depends(require_admin)
):
    """
    Verifica se usuario tem permissao especifica

    Requer: role admin
    """
    rbac = RBACService(db)
    has_permission = rbac.check_permission(user_id, permission, tenant_id)

    return {
        "user_id": user_id,
        "permission": permission,
        "tenant_id": tenant_id,
        "has_permission": has_permission
    }
