# -*- coding: utf-8 -*-
"""
API Key Routes - Issue #341
===========================
FastAPI endpoints for API key management.
"""

from datetime import datetime
from typing import List, Optional
from fastapi import APIRouter, HTTPException, Depends, Header, Request
from pydantic import BaseModel, Field

from .service import APIKeyService, get_api_key_service, validate_api_key_header
from .models import APIKeyScope


router = APIRouter(prefix="/api/api-keys", tags=["API Keys"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class CreateAPIKeyRequest(BaseModel):
    """Request to create a new API key."""
    name: str = Field(..., min_length=1, max_length=100)
    scopes: List[str] = Field(default_factory=list)
    rate_limit: int = Field(default=1000, ge=1, le=100000)
    expires_in_days: Optional[int] = Field(default=365, ge=1, le=3650)
    is_test: bool = Field(default=False)
    metadata: Optional[dict] = None


class UpdateAPIKeyRequest(BaseModel):
    """Request to update an API key."""
    name: Optional[str] = Field(None, min_length=1, max_length=100)
    scopes: Optional[List[str]] = None
    rate_limit: Optional[int] = Field(None, ge=1, le=100000)
    is_active: Optional[bool] = None
    metadata: Optional[dict] = None


class APIKeyResponse(BaseModel):
    """API key response (without secret)."""
    id: str
    tenant_id: str
    name: str
    scopes: List[str]
    rate_limit: int
    expires_at: Optional[str]
    last_used_at: Optional[str]
    created_at: str
    created_by: str
    is_active: bool
    is_test: bool


class APIKeyCreateResponse(BaseModel):
    """Response when creating API key (includes secret once)."""
    id: str
    secret: str
    full_key: str
    name: str
    scopes: List[str]
    expires_at: Optional[str]
    message: str


class RateLimitStatusResponse(BaseModel):
    """Rate limit status response."""
    key_id: str
    limit: int
    current_usage: int
    remaining: int
    reset_at: str


class ScopeInfo(BaseModel):
    """Available scope information."""
    scope: str
    name: str


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_current_user_id(request: Request) -> str:
    """Extract user ID from request (JWT or session)."""
    # In production, extract from JWT token
    # For now, use a header or default
    return request.headers.get("X-User-Id", "system")


def get_current_tenant_id(request: Request) -> str:
    """Extract tenant ID from request."""
    return request.headers.get("X-Tenant-Id", "default")


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("", response_model=APIKeyCreateResponse)
async def create_api_key(
    data: CreateAPIKeyRequest,
    request: Request
):
    """
    Create a new API key.

    The secret is only returned once - store it securely!
    """
    user_id = get_current_user_id(request)
    tenant_id = get_current_tenant_id(request)

    service = get_api_key_service(tenant_id)

    result = service.create_key(
        name=data.name,
        tenant_id=tenant_id,
        created_by=user_id,
        scopes=data.scopes,
        rate_limit=data.rate_limit,
        expires_in_days=data.expires_in_days,
        is_test=data.is_test,
        metadata=data.metadata
    )

    return APIKeyCreateResponse(**result.to_dict())


@router.get("", response_model=List[APIKeyResponse])
async def list_api_keys(request: Request):
    """List all API keys for the current tenant."""
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    keys = service.list_keys(tenant_id)

    return [
        APIKeyResponse(
            id=k.id,
            tenant_id=k.tenant_id,
            name=k.name,
            scopes=k.scopes,
            rate_limit=k.rate_limit,
            expires_at=k.expires_at.isoformat() if k.expires_at else None,
            last_used_at=k.last_used_at.isoformat() if k.last_used_at else None,
            created_at=k.created_at.isoformat() if k.created_at else None,
            created_by=k.created_by,
            is_active=k.is_active,
            is_test=k.is_test
        )
        for k in keys
    ]


@router.get("/scopes", response_model=List[ScopeInfo])
async def get_available_scopes():
    """Get list of all available API key scopes."""
    return APIKeyService.get_available_scopes()


@router.get("/{key_id}", response_model=APIKeyResponse)
async def get_api_key(key_id: str, request: Request):
    """Get details of a specific API key."""
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    key = service.get_key(key_id)
    if not key:
        raise HTTPException(404, "API key not found")

    return APIKeyResponse(
        id=key.id,
        tenant_id=key.tenant_id,
        name=key.name,
        scopes=key.scopes,
        rate_limit=key.rate_limit,
        expires_at=key.expires_at.isoformat() if key.expires_at else None,
        last_used_at=key.last_used_at.isoformat() if key.last_used_at else None,
        created_at=key.created_at.isoformat() if key.created_at else None,
        created_by=key.created_by,
        is_active=key.is_active,
        is_test=key.is_test
    )


@router.patch("/{key_id}", response_model=APIKeyResponse)
async def update_api_key(
    key_id: str,
    data: UpdateAPIKeyRequest,
    request: Request
):
    """Update an API key's properties."""
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    key = service.update_key(
        key_id=key_id,
        name=data.name,
        scopes=data.scopes,
        rate_limit=data.rate_limit,
        is_active=data.is_active,
        metadata=data.metadata
    )

    if not key:
        raise HTTPException(404, "API key not found")

    return APIKeyResponse(
        id=key.id,
        tenant_id=key.tenant_id,
        name=key.name,
        scopes=key.scopes,
        rate_limit=key.rate_limit,
        expires_at=key.expires_at.isoformat() if key.expires_at else None,
        last_used_at=key.last_used_at.isoformat() if key.last_used_at else None,
        created_at=key.created_at.isoformat() if key.created_at else None,
        created_by=key.created_by,
        is_active=key.is_active,
        is_test=key.is_test
    )


@router.delete("/{key_id}")
async def revoke_api_key(key_id: str, request: Request):
    """Revoke an API key (soft delete)."""
    user_id = get_current_user_id(request)
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    if not service.revoke_key(key_id, user_id):
        raise HTTPException(404, "API key not found")

    return {"message": "API key revoked successfully", "key_id": key_id}


@router.post("/{key_id}/rotate", response_model=APIKeyCreateResponse)
async def rotate_api_key(key_id: str, request: Request):
    """
    Rotate an API key - generates new secret.

    The old secret becomes invalid immediately.
    The new secret is only returned once - store it securely!
    """
    user_id = get_current_user_id(request)
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    result = service.rotate_key(key_id, user_id)
    if not result:
        raise HTTPException(404, "API key not found")

    return APIKeyCreateResponse(**result.to_dict())


@router.get("/{key_id}/rate-limit", response_model=RateLimitStatusResponse)
async def get_rate_limit_status(key_id: str, request: Request):
    """Get current rate limit status for an API key."""
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    status = service.get_rate_limit_status(key_id)
    if "error" in status:
        raise HTTPException(404, status["error"])

    return RateLimitStatusResponse(**status)


@router.get("/{key_id}/usage")
async def get_api_key_usage(
    key_id: str,
    limit: int = 100,
    request: Request = None
):
    """Get usage log for an API key."""
    tenant_id = get_current_tenant_id(request)
    service = get_api_key_service(tenant_id)

    # Verify key belongs to tenant
    key = service.get_key(key_id)
    if not key:
        raise HTTPException(404, "API key not found")

    logs = service.get_usage_log(key_id, limit)
    return {"key_id": key_id, "usage": logs}


# =============================================================================
# TEST ENDPOINT (for validating API key auth)
# =============================================================================

@router.get("/test/auth")
async def test_api_key_auth(
    x_api_key: str = Header(..., alias="X-API-Key")
):
    """
    Test endpoint to validate API key authentication.

    Send the API key in the X-API-Key header.
    """
    success, key, message = validate_api_key_header(x_api_key)

    if not success:
        raise HTTPException(401, message)

    return {
        "authenticated": True,
        "key_id": key.id,
        "tenant_id": key.tenant_id,
        "scopes": key.scopes,
        "message": "API key is valid"
    }
