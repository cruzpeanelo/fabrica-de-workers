# -*- coding: utf-8 -*-
"""
CORS Routes - Issue #399
========================
API endpoints for managing CORS configuration.

Admin-only endpoints for:
- Viewing CORS configuration
- Adding/removing allowed origins
- Per-tenant configuration
"""

from typing import Optional, List
from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel, Field
import logging

from factory.security.cors_config import (
    CORSService,
    CORSConfig,
    get_cors_config,
    is_origin_allowed
)

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/cors", tags=["CORS"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class AddOriginRequest(BaseModel):
    """Request to add an allowed origin."""
    origin: str = Field(..., min_length=1, max_length=500)
    tenant_id: Optional[str] = None


class RemoveOriginRequest(BaseModel):
    """Request to remove an allowed origin."""
    origin: str
    tenant_id: Optional[str] = None


class SetConfigRequest(BaseModel):
    """Request to set full CORS configuration."""
    allowed_origins: List[str] = Field(..., min_items=1)
    allowed_methods: Optional[List[str]] = None
    allowed_headers: Optional[List[str]] = None
    expose_headers: Optional[List[str]] = None
    allow_credentials: bool = True
    max_age: int = Field(default=3600, ge=0, le=86400)
    tenant_id: Optional[str] = None


class CheckOriginRequest(BaseModel):
    """Request to check if an origin is allowed."""
    origin: str
    tenant_id: Optional[str] = None


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def require_admin(request: Request):
    """Verify request is from admin user."""
    user = getattr(request.state, "user", None)
    if not user:
        role = request.headers.get("X-User-Role", "USER")
    else:
        role = user.get("role", "USER") if isinstance(user, dict) else getattr(user, "role", "USER")

    if role not in ("ADMIN", "admin"):
        raise HTTPException(403, "Admin access required")


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/config")
async def get_config(
    request: Request,
    tenant_id: Optional[str] = None
):
    """Get CORS configuration for a tenant or global."""
    require_admin(request)

    config = get_cors_config(tenant_id)

    return {
        "config": config.to_dict(),
        "tenant_id": tenant_id or "global"
    }


@router.get("/configs")
async def list_configs(request: Request):
    """List all CORS configurations."""
    require_admin(request)

    configs = CORSService.list_configs()

    return {
        "configs": configs,
        "count": len(configs)
    }


@router.put("/config")
async def set_config(data: SetConfigRequest, request: Request):
    """Set CORS configuration for a tenant or global."""
    require_admin(request)

    config = CORSConfig(
        tenant_id=data.tenant_id,
        allowed_origins=data.allowed_origins,
        allow_credentials=data.allow_credentials,
        max_age=data.max_age
    )

    if data.allowed_methods:
        config.allowed_methods = data.allowed_methods
    if data.allowed_headers:
        config.allowed_headers = data.allowed_headers
    if data.expose_headers:
        config.expose_headers = data.expose_headers

    CORSService.set_config(config, data.tenant_id)

    logger.info(
        f"CORS config updated for {data.tenant_id or 'global'}: "
        f"{len(data.allowed_origins)} origins"
    )

    return {
        "message": "Configuration updated",
        "config": config.to_dict()
    }


@router.delete("/config/{tenant_id}")
async def delete_config(tenant_id: str, request: Request):
    """Delete tenant-specific CORS configuration."""
    require_admin(request)

    if not CORSService.delete_config(tenant_id):
        raise HTTPException(404, f"No configuration found for tenant {tenant_id}")

    return {
        "message": "Configuration deleted",
        "tenant_id": tenant_id
    }


@router.get("/origins")
async def list_origins(
    request: Request,
    tenant_id: Optional[str] = None
):
    """List allowed origins for a tenant."""
    require_admin(request)

    origins = CORSService.get_origins(tenant_id)

    return {
        "origins": origins,
        "count": len(origins),
        "tenant_id": tenant_id or "global"
    }


@router.post("/origins")
async def add_origin(data: AddOriginRequest, request: Request):
    """Add an allowed origin."""
    require_admin(request)

    # Validate origin format
    if not data.origin.startswith(("http://", "https://", "*")):
        raise HTTPException(
            400,
            "Origin must start with http://, https://, or * for wildcard"
        )

    config = CORSService.add_origin(data.origin, data.tenant_id)

    logger.info(
        f"Added CORS origin {data.origin} for {data.tenant_id or 'global'}"
    )

    return {
        "message": "Origin added",
        "origin": data.origin,
        "tenant_id": data.tenant_id or "global",
        "total_origins": len(config.allowed_origins)
    }


@router.delete("/origins")
async def remove_origin(data: RemoveOriginRequest, request: Request):
    """Remove an allowed origin."""
    require_admin(request)

    if not CORSService.remove_origin(data.origin, data.tenant_id):
        raise HTTPException(
            404,
            f"Origin {data.origin} not found in configuration"
        )

    return {
        "message": "Origin removed",
        "origin": data.origin,
        "tenant_id": data.tenant_id or "global"
    }


@router.post("/check")
async def check_origin(data: CheckOriginRequest, request: Request):
    """Check if an origin is allowed."""
    require_admin(request)

    allowed = is_origin_allowed(data.origin, data.tenant_id)

    return {
        "origin": data.origin,
        "tenant_id": data.tenant_id or "global",
        "allowed": allowed
    }


@router.get("/my-origin")
async def check_my_origin(request: Request):
    """
    Check if the current request's origin is allowed.

    Non-admin endpoint for clients to verify their origin.
    """
    origin = request.headers.get("Origin")
    tenant_id = request.headers.get("X-Tenant-Id")

    if not origin:
        return {
            "origin": None,
            "allowed": False,
            "message": "No Origin header in request"
        }

    allowed = is_origin_allowed(origin, tenant_id)

    return {
        "origin": origin,
        "tenant_id": tenant_id,
        "allowed": allowed
    }


@router.get("/defaults")
async def get_defaults(request: Request):
    """Get default CORS configuration values."""
    require_admin(request)

    from factory.security.cors_config import (
        DEFAULT_ALLOWED_ORIGINS,
        DEFAULT_ALLOWED_METHODS,
        DEFAULT_ALLOWED_HEADERS,
        DEFAULT_EXPOSE_HEADERS
    )

    return {
        "default_allowed_origins": DEFAULT_ALLOWED_ORIGINS,
        "default_allowed_methods": DEFAULT_ALLOWED_METHODS,
        "default_allowed_headers": DEFAULT_ALLOWED_HEADERS,
        "default_expose_headers": DEFAULT_EXPOSE_HEADERS
    }
