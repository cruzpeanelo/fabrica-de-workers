# -*- coding: utf-8 -*-
"""
Rotas de API para Multi-tenancy e White Label
==============================================

Este modulo define as rotas REST para gestao de tenants:

Tenants (Admin):
    POST   /api/admin/tenants           - Criar tenant
    GET    /api/admin/tenants           - Listar tenants
    GET    /api/admin/tenants/{id}      - Buscar tenant
    PUT    /api/admin/tenants/{id}      - Atualizar tenant
    DELETE /api/admin/tenants/{id}      - Deletar tenant

Settings:
    GET    /api/admin/tenants/{id}/settings    - Buscar settings
    PUT    /api/admin/tenants/{id}/settings    - Atualizar settings

Branding (White Label):
    GET    /api/admin/tenants/{id}/branding    - Buscar branding
    PUT    /api/admin/tenants/{id}/branding    - Atualizar branding
    GET    /api/admin/tenants/{id}/branding/css - Gerar CSS

Membros:
    GET    /api/admin/tenants/{id}/members     - Listar membros
    POST   /api/admin/tenants/{id}/members     - Adicionar membro
    DELETE /api/admin/tenants/{id}/members/{user_id} - Remover membro

Convites:
    GET    /api/admin/tenants/{id}/invites     - Listar convites
    POST   /api/admin/tenants/{id}/invites     - Criar convite
    DELETE /api/admin/tenants/{id}/invites/{invite_id} - Revogar convite

Uso (Billing):
    GET    /api/admin/tenants/{id}/usage       - Resumo de uso
    GET    /api/admin/tenants/{id}/usage/history - Historico de uso

SSO:
    GET    /api/admin/tenants/{id}/sso         - Buscar config SSO
    PUT    /api/admin/tenants/{id}/sso         - Atualizar SSO

Audit (Logs):
    GET    /api/admin/tenants/{id}/audit       - Logs de auditoria
"""

from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Depends, Query, Response
from pydantic import BaseModel, EmailStr

# Database
try:
    from factory.database.connection import SessionLocal
    from factory.database.tenant_models import (
        Tenant, TenantSettings, BrandingConfig, TenantMember,
        TenantInvite, TenantUsageLog, TenantAuditLog, SSOConfig,
        TenantStatus, TenantPlan, MemberRole, InviteStatus
    )
except ImportError:
    # Fallback para testes
    pass

# Services
from factory.core.multi_tenant import (
    TenantService, BrandingService,
    get_current_tenant, get_current_tenant_id,
    require_tenant, require_role
)

# Auth (Issue #140)
from .auth import get_current_user, TokenData


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/admin/tenants", tags=["tenants"])


# =============================================================================
# SCHEMAS PYDANTIC
# =============================================================================

class TenantCreate(BaseModel):
    """Schema para criacao de tenant"""
    name: str
    email: EmailStr
    plan: Optional[str] = "trial"
    slug: Optional[str] = None
    description: Optional[str] = None
    custom_domain: Optional[str] = None
    timezone: Optional[str] = "America/Sao_Paulo"
    locale: Optional[str] = "pt-BR"


class TenantUpdate(BaseModel):
    """Schema para atualizacao de tenant"""
    name: Optional[str] = None
    email: Optional[EmailStr] = None
    description: Optional[str] = None
    custom_domain: Optional[str] = None
    timezone: Optional[str] = None
    locale: Optional[str] = None
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
    allowed_models: Optional[List[str]] = None
    ai_settings: Optional[Dict[str, Any]] = None
    workflow_settings: Optional[Dict[str, Any]] = None
    notifications: Optional[Dict[str, Any]] = None
    security_settings: Optional[Dict[str, Any]] = None


class BrandingUpdate(BaseModel):
    """Schema para atualizacao de branding"""
    logo_url: Optional[str] = None
    logo_dark_url: Optional[str] = None
    favicon_url: Optional[str] = None
    display_name: Optional[str] = None
    tagline: Optional[str] = None
    colors: Optional[Dict[str, str]] = None
    dark_colors: Optional[Dict[str, str]] = None
    dark_mode_enabled: Optional[bool] = None
    fonts: Optional[Dict[str, str]] = None
    spacing: Optional[Dict[str, str]] = None
    footer: Optional[Dict[str, Any]] = None
    login_config: Optional[Dict[str, Any]] = None
    custom_css: Optional[str] = None


class MemberAdd(BaseModel):
    """Schema para adicionar membro"""
    user_id: int
    role: Optional[str] = "member"


class MemberUpdate(BaseModel):
    """Schema para atualizar membro"""
    role: Optional[str] = None
    active: Optional[bool] = None
    custom_permissions: Optional[Dict[str, bool]] = None


class InviteCreate(BaseModel):
    """Schema para criar convite"""
    email: EmailStr
    role: Optional[str] = "member"
    message: Optional[str] = None
    expires_in_days: Optional[int] = 7


class SSOUpdate(BaseModel):
    """Schema para atualizacao de SSO"""
    enabled: Optional[bool] = None
    provider: Optional[str] = None
    saml_config: Optional[Dict[str, Any]] = None
    oauth_config: Optional[Dict[str, Any]] = None
    attribute_mapping: Optional[Dict[str, str]] = None
    scim_enabled: Optional[bool] = None
    scim_config: Optional[Dict[str, Any]] = None
    provisioning_rules: Optional[Dict[str, Any]] = None
    settings: Optional[Dict[str, Any]] = None


# =============================================================================
# DEPENDENCY - Database Session
# =============================================================================

def get_db():
    """Dependency para sessao do banco"""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - TENANTS (CRUD)
# =============================================================================

@router.post("", response_model=Dict[str, Any])
async def create_tenant(data: TenantCreate, db=Depends(get_db)):
    """
    Cria um novo tenant

    Requer: role admin ou superadmin (nivel plataforma)
    """
    service = TenantService(db)

    try:
        tenant = service.create_tenant(
            name=data.name,
            email=data.email,
            plan=data.plan,
            slug=data.slug,
            description=data.description,
            custom_domain=data.custom_domain,
            timezone=data.timezone,
            locale=data.locale
        )
        return {"success": True, "tenant": tenant}

    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("", response_model=Dict[str, Any])
async def list_tenants(
    status: Optional[str] = None,
    plan: Optional[str] = None,
    search: Optional[str] = None,
    limit: int = Query(50, le=100),
    offset: int = 0,
    db=Depends(get_db)
):
    """
    Lista todos os tenants (admin plataforma)
    """
    query = db.query(Tenant)

    if status:
        query = query.filter(Tenant.status == status)

    if plan:
        query = query.filter(Tenant.plan == plan)

    if search:
        query = query.filter(
            (Tenant.name.ilike(f"%{search}%")) |
            (Tenant.slug.ilike(f"%{search}%")) |
            (Tenant.email.ilike(f"%{search}%"))
        )

    total = query.count()
    tenants = query.order_by(Tenant.created_at.desc()).offset(offset).limit(limit).all()

    return {
        "tenants": [t.to_dict() for t in tenants],
        "total": total,
        "limit": limit,
        "offset": offset
    }


@router.get("/{tenant_id}", response_model=Dict[str, Any])
async def get_tenant(tenant_id: str, db=Depends(get_db)):
    """
    Busca tenant por ID ou slug
    """
    service = TenantService(db)
    tenant = service.get_tenant(tenant_id, include_settings=True)

    if not tenant:
        raise HTTPException(status_code=404, detail=f"Tenant {tenant_id} nao encontrado")

    return {"tenant": tenant}


@router.put("/{tenant_id}", response_model=Dict[str, Any])
async def update_tenant(tenant_id: str, data: TenantUpdate, db=Depends(get_db)):
    """
    Atualiza dados do tenant
    """
    service = TenantService(db)

    update_data = data.model_dump(exclude_unset=True)
    tenant = service.update_tenant(tenant_id, **update_data)

    if not tenant:
        raise HTTPException(status_code=404, detail=f"Tenant {tenant_id} nao encontrado")

    return {"success": True, "tenant": tenant}


@router.delete("/{tenant_id}", response_model=Dict[str, Any])
async def delete_tenant(
    tenant_id: str,
    hard_delete: bool = Query(False),
    db=Depends(get_db)
):
    """
    Deleta ou desativa tenant

    Por padrao faz soft delete (status = cancelled).
    Com hard_delete=true, remove permanentemente.
    """
    service = TenantService(db)
    success = service.delete_tenant(tenant_id, soft_delete=not hard_delete)

    if not success:
        raise HTTPException(status_code=404, detail=f"Tenant {tenant_id} nao encontrado")

    return {
        "success": True,
        "message": f"Tenant {tenant_id} {'removido' if hard_delete else 'desativado'}"
    }


# =============================================================================
# ENDPOINTS - SETTINGS
# =============================================================================

@router.get("/{tenant_id}/settings", response_model=Dict[str, Any])
async def get_tenant_settings(tenant_id: str, db=Depends(get_db)):
    """
    Retorna configuracoes do tenant
    """
    settings = db.query(TenantSettings).filter(
        TenantSettings.tenant_id == tenant_id
    ).first()

    if not settings:
        raise HTTPException(status_code=404, detail="Settings nao encontradas")

    return {"settings": settings.to_dict()}


@router.put("/{tenant_id}/settings", response_model=Dict[str, Any])
async def update_tenant_settings(
    tenant_id: str,
    data: TenantSettingsUpdate,
    db=Depends(get_db)
):
    """
    Atualiza configuracoes do tenant
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

    return {"success": True, "settings": settings.to_dict()}


# =============================================================================
# ENDPOINTS - BRANDING (WHITE LABEL)
# =============================================================================

@router.get("/{tenant_id}/branding", response_model=Dict[str, Any])
async def get_tenant_branding(tenant_id: str, db=Depends(get_db)):
    """
    Retorna configuracoes de branding do tenant
    """
    service = BrandingService(db)
    branding = service.get_branding(tenant_id)

    if not branding:
        raise HTTPException(status_code=404, detail="Branding nao encontrado")

    return {"branding": branding}


@router.put("/{tenant_id}/branding", response_model=Dict[str, Any])
async def update_tenant_branding(
    tenant_id: str,
    data: BrandingUpdate,
    db=Depends(get_db)
):
    """
    Atualiza configuracoes de branding (white label)
    """
    service = BrandingService(db)

    update_data = data.model_dump(exclude_unset=True)
    branding = service.update_branding(tenant_id, **update_data)

    return {"success": True, "branding": branding}


@router.get("/{tenant_id}/branding/css")
async def get_tenant_css(
    tenant_id: str,
    dark_mode: bool = Query(False),
    db=Depends(get_db)
):
    """
    Gera CSS customizado para o tenant

    Retorna CSS com todas as variaveis customizadas.
    Pode ser incluido diretamente no HTML.
    """
    service = BrandingService(db)
    css = service.generate_css(tenant_id, dark_mode)

    return Response(
        content=css,
        media_type="text/css",
        headers={"Cache-Control": "public, max-age=3600"}
    )


# =============================================================================
# ENDPOINTS - MEMBERS
# =============================================================================

@router.get("/{tenant_id}/members", response_model=Dict[str, Any])
async def list_tenant_members(
    tenant_id: str,
    role: Optional[str] = None,
    active_only: bool = True,
    db=Depends(get_db)
):
    """
    Lista membros do tenant
    """
    query = db.query(TenantMember).filter(TenantMember.tenant_id == tenant_id)

    if role:
        query = query.filter(TenantMember.role == role)

    if active_only:
        query = query.filter(TenantMember.active == True)

    members = query.all()

    return {
        "members": [m.to_dict() for m in members],
        "total": len(members)
    }


@router.post("/{tenant_id}/members", response_model=Dict[str, Any])
async def add_tenant_member(
    tenant_id: str,
    data: MemberAdd,
    db=Depends(get_db)
):
    """
    Adiciona membro ao tenant
    """
    service = TenantService(db)

    try:
        member = service.add_member(
            tenant_id=tenant_id,
            user_id=data.user_id,
            role=data.role
        )
        return {"success": True, "member": member}

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.put("/{tenant_id}/members/{user_id}", response_model=Dict[str, Any])
async def update_tenant_member(
    tenant_id: str,
    user_id: int,
    data: MemberUpdate,
    db=Depends(get_db)
):
    """
    Atualiza role/status de membro
    """
    member = db.query(TenantMember).filter(
        TenantMember.tenant_id == tenant_id,
        TenantMember.user_id == user_id
    ).first()

    if not member:
        raise HTTPException(status_code=404, detail="Membro nao encontrado")

    update_data = data.model_dump(exclude_unset=True)
    for key, value in update_data.items():
        if hasattr(member, key):
            setattr(member, key, value)

    db.commit()
    db.refresh(member)

    return {"success": True, "member": member.to_dict()}


@router.delete("/{tenant_id}/members/{user_id}", response_model=Dict[str, Any])
async def remove_tenant_member(
    tenant_id: str,
    user_id: int,
    db=Depends(get_db)
):
    """
    Remove membro do tenant
    """
    service = TenantService(db)

    try:
        success = service.remove_member(tenant_id, user_id)
        if not success:
            raise HTTPException(status_code=404, detail="Membro nao encontrado")
        return {"success": True, "message": "Membro removido"}

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


# =============================================================================
# ENDPOINTS - INVITES
# =============================================================================

@router.get("/{tenant_id}/invites", response_model=Dict[str, Any])
async def list_tenant_invites(
    tenant_id: str,
    status: Optional[str] = None,
    db=Depends(get_db)
):
    """
    Lista convites pendentes do tenant
    """
    query = db.query(TenantInvite).filter(TenantInvite.tenant_id == tenant_id)

    if status:
        query = query.filter(TenantInvite.status == status)

    invites = query.order_by(TenantInvite.created_at.desc()).all()

    return {
        "invites": [i.to_dict() for i in invites],
        "total": len(invites)
    }


@router.post("/{tenant_id}/invites", response_model=Dict[str, Any])
async def create_tenant_invite(
    tenant_id: str,
    data: InviteCreate,
    db=Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Cria convite para novo membro
    """
    service = TenantService(db)

    # Issue #140: Extrair usuario do JWT em vez de hardcoded
    invited_by = current_user.username if current_user else "system"

    invite = service.create_invite(
        tenant_id=tenant_id,
        email=data.email,
        role=data.role,
        message=data.message,
        expires_in_days=data.expires_in_days,
        invited_by=invited_by
    )

    return {"success": True, "invite": invite}


@router.delete("/{tenant_id}/invites/{invite_id}", response_model=Dict[str, Any])
async def revoke_tenant_invite(
    tenant_id: str,
    invite_id: str,
    db=Depends(get_db)
):
    """
    Revoga convite pendente
    """
    invite = db.query(TenantInvite).filter(
        TenantInvite.tenant_id == tenant_id,
        TenantInvite.invite_id == invite_id
    ).first()

    if not invite:
        raise HTTPException(status_code=404, detail="Convite nao encontrado")

    invite.status = InviteStatus.REVOKED.value
    db.commit()

    return {"success": True, "message": "Convite revogado"}


# =============================================================================
# ENDPOINTS - USAGE (BILLING)
# =============================================================================

@router.get("/{tenant_id}/usage", response_model=Dict[str, Any])
async def get_tenant_usage(tenant_id: str, db=Depends(get_db)):
    """
    Retorna resumo de uso atual do tenant
    """
    # Buscar ultimo registro de uso
    usage = db.query(TenantUsageLog).filter(
        TenantUsageLog.tenant_id == tenant_id
    ).order_by(TenantUsageLog.created_at.desc()).first()

    # Buscar settings para calcular quotas
    settings = db.query(TenantSettings).filter(
        TenantSettings.tenant_id == tenant_id
    ).first()

    if not usage:
        return {
            "usage": {
                "tokens_used": 0,
                "api_calls": 0,
                "storage_bytes": 0,
                "workers_executed": 0,
                "stories_created": 0,
                "projects_created": 0,
                "cost_total": 0.0
            },
            "quotas": settings.to_dict() if settings else {}
        }

    return {
        "usage": usage.to_dict(),
        "quotas": settings.to_dict() if settings else {}
    }


@router.get("/{tenant_id}/usage/history", response_model=Dict[str, Any])
async def get_tenant_usage_history(
    tenant_id: str,
    days: int = Query(30, le=365),
    db=Depends(get_db)
):
    """
    Retorna historico de uso do tenant
    """
    since = datetime.utcnow() - timedelta(days=days)

    history = db.query(TenantUsageLog).filter(
        TenantUsageLog.tenant_id == tenant_id,
        TenantUsageLog.created_at >= since
    ).order_by(TenantUsageLog.created_at.asc()).all()

    return {
        "history": [h.to_dict() for h in history],
        "total_records": len(history),
        "period_days": days
    }


# =============================================================================
# ENDPOINTS - SSO
# =============================================================================

@router.get("/{tenant_id}/sso", response_model=Dict[str, Any])
async def get_tenant_sso(tenant_id: str, db=Depends(get_db)):
    """
    Retorna configuracao SSO do tenant
    """
    sso = db.query(SSOConfig).filter(SSOConfig.tenant_id == tenant_id).first()

    if not sso:
        return {"sso": None, "enabled": False}

    # Nao retornar dados sensiveis
    return {"sso": sso.to_dict(include_sensitive=False)}


@router.put("/{tenant_id}/sso", response_model=Dict[str, Any])
async def update_tenant_sso(
    tenant_id: str,
    data: SSOUpdate,
    db=Depends(get_db)
):
    """
    Atualiza configuracao SSO do tenant
    """
    sso = db.query(SSOConfig).filter(SSOConfig.tenant_id == tenant_id).first()

    if not sso:
        sso = SSOConfig(tenant_id=tenant_id)
        db.add(sso)

    update_data = data.model_dump(exclude_unset=True)
    for key, value in update_data.items():
        if hasattr(sso, key):
            setattr(sso, key, value)

    sso.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(sso)

    return {"success": True, "sso": sso.to_dict(include_sensitive=False)}


# =============================================================================
# ENDPOINTS - AUDIT LOGS
# =============================================================================

@router.get("/{tenant_id}/audit", response_model=Dict[str, Any])
async def get_tenant_audit_logs(
    tenant_id: str,
    action: Optional[str] = None,
    resource_type: Optional[str] = None,
    user_id: Optional[int] = None,
    days: int = Query(30, le=90),
    limit: int = Query(100, le=500),
    offset: int = 0,
    db=Depends(get_db)
):
    """
    Retorna logs de auditoria do tenant
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

    if user_id:
        query = query.filter(TenantAuditLog.user_id == user_id)

    total = query.count()
    logs = query.order_by(
        TenantAuditLog.created_at.desc()
    ).offset(offset).limit(limit).all()

    return {
        "logs": [l.to_dict() for l in logs],
        "total": total,
        "limit": limit,
        "offset": offset
    }


# =============================================================================
# ENDPOINTS - PUBLIC (SEM AUTH)
# =============================================================================

# Router publico para endpoints que nao precisam de autenticacao admin
public_router = APIRouter(prefix="/api/tenant", tags=["tenant-public"])


@public_router.get("/current", response_model=Dict[str, Any])
async def get_current_tenant_info():
    """
    Retorna informacoes do tenant atual (baseado no contexto)

    Usado pelo frontend para carregar branding e configuracoes.
    """
    tenant = get_current_tenant()

    if not tenant:
        return {
            "tenant": None,
            "branding": None,
            "message": "Nenhum tenant identificado"
        }

    return {
        "tenant": {
            "tenant_id": tenant.get("tenant_id"),
            "name": tenant.get("name"),
            "slug": tenant.get("slug"),
            "plan": tenant.get("plan"),
            "features": tenant.get("features", {})
        },
        "branding": tenant.get("branding", {})
    }


@public_router.get("/branding.css")
async def get_current_tenant_css(dark_mode: bool = Query(False)):
    """
    Retorna CSS do tenant atual

    Pode ser incluido diretamente no HTML:
    <link rel="stylesheet" href="/api/tenant/branding.css">
    """
    tenant_id = get_current_tenant_id()

    if not tenant_id:
        # Retornar CSS padrao
        service = BrandingService()
        css = service._default_css()
    else:
        service = BrandingService()
        css = service.generate_css(tenant_id, dark_mode)

    return Response(
        content=css,
        media_type="text/css",
        headers={
            "Cache-Control": "public, max-age=300",
            "X-Tenant-ID": tenant_id or "default"
        }
    )


@public_router.post("/join/{token}", response_model=Dict[str, Any])
async def accept_invite(token: str, user_id: int, db=Depends(get_db)):
    """
    Aceita convite para entrar em um tenant

    Requer usuario ja autenticado (user_id).
    """
    invite = db.query(TenantInvite).filter(TenantInvite.token == token).first()

    if not invite:
        raise HTTPException(status_code=404, detail="Convite nao encontrado")

    if not invite.is_valid():
        raise HTTPException(
            status_code=400,
            detail="Convite expirado ou ja utilizado"
        )

    # Adicionar usuario como membro
    service = TenantService(db)
    try:
        member = service.add_member(
            tenant_id=invite.tenant_id,
            user_id=user_id,
            role=invite.role,
            invited_by=invite.invited_by
        )

        # Marcar convite como aceito
        invite.status = InviteStatus.ACCEPTED.value
        invite.accepted_at = datetime.utcnow()
        db.commit()

        return {
            "success": True,
            "tenant_id": invite.tenant_id,
            "role": invite.role,
            "member": member
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
