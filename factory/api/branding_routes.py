# -*- coding: utf-8 -*-
"""
Branding Routes - White Label API
=================================

API para carregar configuracoes de branding (white label) por tenant.

Endpoints:
- GET /api/tenant/{tenant_id}/branding - Branding por tenant_id
- GET /api/tenant/{tenant_id}/config - Configuracoes funcionais
- GET /api/tenant/slug/{slug}/branding - Branding por slug (publico)
- GET /api/tenant/by-domain/{domain} - Lookup por dominio
- PUT /api/tenant/{tenant_id}/branding - Atualizar branding

Author: Plataforma E - Terminal 4
Issue #383 - Terminal A (extended)
"""

import time
from functools import lru_cache
from fastapi import APIRouter, HTTPException, Depends, Request
from pydantic import BaseModel
from typing import Optional, Dict, Any, List

router = APIRouter(prefix="/api/tenant", tags=["Branding"])

# Cache simples com TTL de 5 minutos
_branding_cache: Dict[str, tuple] = {}  # key -> (value, timestamp)
CACHE_TTL = 300  # 5 minutos


def get_cached_branding(key: str) -> Optional[Any]:
    """Obtem branding do cache se ainda valido"""
    if key in _branding_cache:
        value, timestamp = _branding_cache[key]
        if time.time() - timestamp < CACHE_TTL:
            return value
        else:
            del _branding_cache[key]
    return None


def set_cached_branding(key: str, value: Any):
    """Armazena branding no cache"""
    _branding_cache[key] = (value, time.time())


def invalidate_branding_cache(tenant_id: str):
    """Invalida cache de branding para um tenant"""
    keys_to_remove = [k for k in _branding_cache if tenant_id in k]
    for key in keys_to_remove:
        del _branding_cache[key]


class BrandingResponse(BaseModel):
    tenant_id: str
    display_name: str
    tagline: Optional[str] = None
    logo_url: Optional[str] = None
    logo_dark_url: Optional[str] = None
    favicon_url: Optional[str] = None
    colors: Optional[Dict] = None
    dark_colors: Optional[Dict] = None
    fonts: Optional[Dict] = None
    footer: Optional[Dict] = None
    # Convenience properties for frontend
    primary_color: str = "#003B4A"
    secondary_color: str = "#FF6C00"
    background_color: str = "#F8FAFC"
    text_color: str = "#1E293B"


class BrandingUpdate(BaseModel):
    display_name: Optional[str] = None
    logo_url: Optional[str] = None
    logo_dark_url: Optional[str] = None
    favicon_url: Optional[str] = None
    primary_color: Optional[str] = None
    secondary_color: Optional[str] = None
    accent_color: Optional[str] = None
    background_color: Optional[str] = None
    text_color: Optional[str] = None
    header_style: Optional[Dict] = None
    sidebar_style: Optional[Dict] = None
    button_style: Optional[Dict] = None
    custom_css: Optional[str] = None
    footer: Optional[Dict] = None


class TenantConfigResponse(BaseModel):
    """Configuracoes funcionais do tenant para app mobile - Issue #383"""
    tenant_id: str
    features_enabled: List[str] = ["stories", "kanban", "chat"]
    default_language: str = "pt-BR"
    user_mode_default: str = "basic"
    show_onboarding: bool = True
    max_file_upload_mb: int = 10
    session_timeout_minutes: int = 60
    enable_offline_mode: bool = False
    enable_push_notifications: bool = True


class DomainLookupResponse(BaseModel):
    """Resposta de lookup por dominio - Issue #383"""
    tenant_id: str
    branding: BrandingResponse


def get_db():
    """Get database session"""
    from factory.database.connection import SessionLocal
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def extract_colors(branding):
    """Extract convenience color properties from colors JSON"""
    colors = branding.colors or {}
    return {
        "primary_color": colors.get("primary", "#003B4A"),
        "secondary_color": colors.get("secondary", "#FF6C00"),
        "background_color": colors.get("background", "#F8FAFC"),
        "text_color": colors.get("text_primary", "#1E293B")
    }


@router.get("/{tenant_id}/branding", response_model=BrandingResponse)
async def get_tenant_branding(tenant_id: str, db=Depends(get_db)):
    """
    Get branding configuration for a tenant.
    Requires authentication.
    """
    from factory.database.models import Tenant, BrandingConfig

    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found")

    branding = db.query(BrandingConfig).filter(
        BrandingConfig.tenant_id == tenant_id
    ).first()

    if branding:
        colors_props = extract_colors(branding)
        return BrandingResponse(
            tenant_id=tenant_id,
            display_name=branding.display_name or tenant.name,
            tagline=branding.tagline,
            logo_url=branding.logo_url,
            logo_dark_url=branding.logo_dark_url,
            favicon_url=branding.favicon_url,
            colors=branding.colors,
            dark_colors=branding.dark_colors,
            fonts=branding.fonts,
            footer=branding.footer,
            **colors_props
        )
    else:
        # Return default branding
        return BrandingResponse(
            tenant_id=tenant_id,
            display_name=tenant.name
        )


@router.get("/slug/{slug}/branding", response_model=BrandingResponse)
async def get_tenant_branding_by_slug(slug: str, db=Depends(get_db)):
    """
    Get branding configuration by tenant slug.
    Public endpoint for login page.
    """
    from factory.database.models import Tenant, BrandingConfig

    tenant = db.query(Tenant).filter(Tenant.slug == slug).first()
    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found")

    branding = db.query(BrandingConfig).filter(
        BrandingConfig.tenant_id == tenant.tenant_id
    ).first()

    if branding:
        colors_props = extract_colors(branding)
        return BrandingResponse(
            tenant_id=tenant.tenant_id,
            display_name=branding.display_name or tenant.name,
            tagline=branding.tagline,
            logo_url=branding.logo_url,
            logo_dark_url=branding.logo_dark_url,
            favicon_url=branding.favicon_url,
            colors=branding.colors,
            dark_colors=branding.dark_colors,
            fonts=branding.fonts,
            footer=branding.footer,
            **colors_props
        )
    else:
        return BrandingResponse(
            tenant_id=tenant.tenant_id,
            display_name=tenant.name
        )


@router.put("/{tenant_id}/branding", response_model=BrandingResponse)
async def update_tenant_branding(
    tenant_id: str,
    branding_update: BrandingUpdate,
    request: Request,
    db=Depends(get_db)
):
    """
    Update branding configuration for a tenant.
    Requires tenant admin or super admin role.
    """
    from factory.database.models import Tenant, BrandingConfig

    # Verify tenant exists
    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found")

    # TODO: Add authorization check

    # Get or create branding config
    branding = db.query(BrandingConfig).filter(
        BrandingConfig.tenant_id == tenant_id
    ).first()

    if not branding:
        branding = BrandingConfig(tenant_id=tenant_id)
        db.add(branding)

    # Update fields
    update_data = branding_update.dict(exclude_unset=True)
    for field, value in update_data.items():
        if value is not None:
            setattr(branding, field, value)

    db.commit()
    db.refresh(branding)

    colors_props = extract_colors(branding)
    return BrandingResponse(
        tenant_id=tenant_id,
        display_name=branding.display_name or tenant.name,
        tagline=branding.tagline,
        logo_url=branding.logo_url,
        logo_dark_url=branding.logo_dark_url,
        favicon_url=branding.favicon_url,
        colors=branding.colors,
        dark_colors=branding.dark_colors,
        fonts=branding.fonts,
        footer=branding.footer,
        **colors_props
    )


@router.get("/current/branding", response_model=BrandingResponse)
async def get_current_tenant_branding(request: Request, db=Depends(get_db)):
    """
    Get branding for current tenant from header X-Tenant-ID.
    Used by dashboard to load branding dynamically.
    """
    from factory.database.models import Tenant, BrandingConfig

    tenant_id = request.headers.get("X-Tenant-ID")
    if not tenant_id:
        # Return default branding
        return BrandingResponse(
            tenant_id="default",
            display_name="Plataforma E"
        )

    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
    if not tenant:
        return BrandingResponse(
            tenant_id="default",
            display_name="Plataforma E"
        )

    branding = db.query(BrandingConfig).filter(
        BrandingConfig.tenant_id == tenant_id
    ).first()

    if branding:
        colors_props = extract_colors(branding)
        return BrandingResponse(
            tenant_id=tenant_id,
            display_name=branding.display_name or tenant.name,
            tagline=branding.tagline,
            logo_url=branding.logo_url,
            logo_dark_url=branding.logo_dark_url,
            favicon_url=branding.favicon_url,
            colors=branding.colors,
            dark_colors=branding.dark_colors,
            fonts=branding.fonts,
            footer=branding.footer,
            **colors_props
        )
    else:
        return BrandingResponse(
            tenant_id=tenant_id,
            display_name=tenant.name
        )


# ============== Issue #383 - New Endpoints for Mobile ==============

@router.get("/{tenant_id}/config", response_model=TenantConfigResponse)
async def get_tenant_config(tenant_id: str, db=Depends(get_db)):
    """
    Get functional configuration for a tenant.
    Used by mobile app to load feature flags and settings.

    Issue #383 - Terminal A
    """
    from factory.database.models import Tenant

    # Check cache
    cache_key = f"config:{tenant_id}"
    cached = get_cached_branding(cache_key)
    if cached:
        return cached

    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found")

    # Get config from tenant or use defaults
    config = TenantConfigResponse(
        tenant_id=tenant_id,
        features_enabled=getattr(tenant, 'features_enabled', ["stories", "kanban", "chat"]),
        default_language=getattr(tenant, 'default_language', "pt-BR"),
        user_mode_default=getattr(tenant, 'user_mode_default', "basic"),
        show_onboarding=getattr(tenant, 'show_onboarding', True),
        max_file_upload_mb=getattr(tenant, 'max_file_upload_mb', 10),
        session_timeout_minutes=getattr(tenant, 'session_timeout_minutes', 60),
        enable_offline_mode=getattr(tenant, 'enable_offline_mode', False),
        enable_push_notifications=getattr(tenant, 'enable_push_notifications', True)
    )

    # Cache result
    set_cached_branding(cache_key, config)

    return config


@router.get("/by-domain/{domain}", response_model=DomainLookupResponse)
async def get_tenant_by_domain(domain: str, db=Depends(get_db)):
    """
    Lookup tenant by domain for white label.
    Used by mobile app to determine tenant from custom domain.

    Issue #383 - Terminal A
    """
    from factory.database.models import Tenant, BrandingConfig

    # Check cache
    cache_key = f"domain:{domain}"
    cached = get_cached_branding(cache_key)
    if cached:
        return cached

    # Try to find tenant by custom domain
    tenant = db.query(Tenant).filter(Tenant.custom_domain == domain).first()

    # Fallback: try by slug (subdomain pattern)
    if not tenant:
        # Extract subdomain from domain (e.g., belgo.fabricadeagentes.com.br -> belgo)
        subdomain = domain.split('.')[0] if '.' in domain else domain
        tenant = db.query(Tenant).filter(Tenant.slug == subdomain).first()

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found for domain")

    # Get branding
    branding = db.query(BrandingConfig).filter(
        BrandingConfig.tenant_id == tenant.tenant_id
    ).first()

    if branding:
        colors_props = extract_colors(branding)
        branding_response = BrandingResponse(
            tenant_id=tenant.tenant_id,
            display_name=branding.display_name or tenant.name,
            tagline=branding.tagline,
            logo_url=branding.logo_url,
            logo_dark_url=branding.logo_dark_url,
            favicon_url=branding.favicon_url,
            colors=branding.colors,
            dark_colors=branding.dark_colors,
            fonts=branding.fonts,
            footer=branding.footer,
            **colors_props
        )
    else:
        branding_response = BrandingResponse(
            tenant_id=tenant.tenant_id,
            display_name=tenant.name
        )

    result = DomainLookupResponse(
        tenant_id=tenant.tenant_id,
        branding=branding_response
    )

    # Cache result
    set_cached_branding(cache_key, result)

    return result


@router.delete("/cache/{tenant_id}")
async def clear_tenant_cache(tenant_id: str):
    """
    Clear branding cache for a specific tenant.
    Used after updating branding configuration.

    Issue #383 - Terminal A
    """
    invalidate_branding_cache(tenant_id)
    return {"status": "ok", "message": f"Cache cleared for tenant {tenant_id}"}


def register_branding_routes(app):
    """Register branding routes"""
    app.include_router(router)
    print("[Branding] Routes registered: /api/tenant/{id}/branding, /api/tenant/{id}/config, /api/tenant/by-domain/{domain}")
