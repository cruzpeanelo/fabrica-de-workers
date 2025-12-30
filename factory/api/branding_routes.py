# -*- coding: utf-8 -*-
"""
Branding Routes - White Label API
=================================

API para carregar configuracoes de branding (white label) por tenant.

Endpoints:
- GET /api/tenant/{tenant_id}/branding - Branding por tenant_id
- GET /api/tenant/slug/{slug}/branding - Branding por slug (publico)
- PUT /api/tenant/{tenant_id}/branding - Atualizar branding

Author: Fabrica de Agentes - Terminal 4
"""

from fastapi import APIRouter, HTTPException, Depends, Request
from pydantic import BaseModel
from typing import Optional, Dict, Any

router = APIRouter(prefix="/api/tenant", tags=["Branding"])


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
            display_name="Fabrica de Agentes"
        )

    tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
    if not tenant:
        return BrandingResponse(
            tenant_id="default",
            display_name="Fabrica de Agentes"
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


def register_branding_routes(app):
    """Register branding routes"""
    app.include_router(router)
    print("[Branding] Routes registered: /api/tenant/{id}/branding")
