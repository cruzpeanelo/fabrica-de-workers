# -*- coding: utf-8 -*-
"""
IP Policy API Routes - Issue #343
=================================
FastAPI endpoints for IP policy management.
"""

from typing import List, Optional
from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel, Field

from .ip_policy import (
    IPPolicyService,
    get_ip_policy_service,
    IPPolicyMode,
    GeoIPService
)


router = APIRouter(prefix="/api/tenant", tags=["IP Policy"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class IPPolicyRequest(BaseModel):
    """Request to create/update IP policy."""
    mode: str = Field(default="disabled", description="whitelist, blacklist, or disabled")
    ip_ranges: List[str] = Field(default_factory=list, description="CIDR ranges e.g. 192.168.1.0/24")
    allowed_countries: List[str] = Field(default_factory=list, description="ISO 3166-1 alpha-2 codes")
    blocked_countries: List[str] = Field(default_factory=list, description="ISO 3166-1 alpha-2 codes")
    enforce_on_admin_only: bool = Field(default=False)
    notify_on_block: bool = Field(default=True)
    bypass_for_api_keys: bool = Field(default=False)


class IPPolicyResponse(BaseModel):
    """IP policy response."""
    tenant_id: str
    mode: str
    ip_ranges: List[str]
    allowed_countries: List[str]
    blocked_countries: List[str]
    enforce_on_admin_only: bool
    notify_on_block: bool
    bypass_for_api_keys: bool
    created_at: str
    updated_at: str


class IPTestRequest(BaseModel):
    """Request to test an IP."""
    ip: str = Field(..., description="IP address to test")


class IPTestResponse(BaseModel):
    """IP test result."""
    ip: str
    country: Optional[str]
    allowed: bool
    block_reason: Optional[str]
    policy_mode: str


class BlockedAttemptResponse(BaseModel):
    """Blocked attempt log entry."""
    tenant_id: str
    client_ip: str
    country_code: Optional[str]
    reason: str
    user_agent: Optional[str]
    path: str
    timestamp: str


class GeoIPStatusResponse(BaseModel):
    """GeoIP service status."""
    available: bool
    database_path: str
    message: str


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/{tenant_id}/ip-policy", response_model=Optional[IPPolicyResponse])
async def get_ip_policy(tenant_id: str, request: Request):
    """Get IP policy for a tenant."""
    service = get_ip_policy_service()
    policy = service.get_policy(tenant_id)

    if not policy:
        return None

    return IPPolicyResponse(**policy.to_dict())


@router.put("/{tenant_id}/ip-policy", response_model=IPPolicyResponse)
async def update_ip_policy(
    tenant_id: str,
    data: IPPolicyRequest,
    request: Request
):
    """Create or update IP policy for a tenant."""
    # Validate mode
    try:
        mode = IPPolicyMode(data.mode)
    except ValueError:
        raise HTTPException(
            400,
            f"Invalid mode: {data.mode}. Must be: whitelist, blacklist, or disabled"
        )

    service = get_ip_policy_service()
    policy = service.create_or_update_policy(
        tenant_id=tenant_id,
        mode=mode,
        ip_ranges=data.ip_ranges,
        allowed_countries=data.allowed_countries,
        blocked_countries=data.blocked_countries,
        enforce_on_admin_only=data.enforce_on_admin_only,
        notify_on_block=data.notify_on_block,
        bypass_for_api_keys=data.bypass_for_api_keys
    )

    return IPPolicyResponse(**policy.to_dict())


@router.delete("/{tenant_id}/ip-policy")
async def delete_ip_policy(tenant_id: str, request: Request):
    """Delete IP policy for a tenant (disables restrictions)."""
    service = get_ip_policy_service()

    if not service.delete_policy(tenant_id):
        raise HTTPException(404, "IP policy not found")

    return {"message": "IP policy deleted", "tenant_id": tenant_id}


@router.post("/{tenant_id}/ip-policy/test", response_model=IPTestResponse)
async def test_ip(
    tenant_id: str,
    data: IPTestRequest,
    request: Request
):
    """Test if a specific IP would be allowed."""
    service = get_ip_policy_service()
    result = service.test_ip(tenant_id, data.ip)

    return IPTestResponse(
        ip=result["ip"],
        country=result.get("country"),
        allowed=result["allowed"],
        block_reason=result.get("block_reason"),
        policy_mode=result.get("policy_mode", "disabled")
    )


# =============================================================================
# SECURITY LOGS
# =============================================================================

@router.get("/security/blocked-attempts", response_model=List[BlockedAttemptResponse])
async def get_blocked_attempts(
    tenant_id: Optional[str] = None,
    limit: int = 100,
    request: Request = None
):
    """Get log of blocked access attempts."""
    service = get_ip_policy_service()
    attempts = service.get_blocked_attempts(tenant_id, limit)

    return [BlockedAttemptResponse(**a) for a in attempts]


@router.get("/security/geoip-status", response_model=GeoIPStatusResponse)
async def get_geoip_status():
    """Check if GeoIP service is available."""
    from .ip_policy import GEOIP_DB_PATH

    available = GeoIPService.is_available()

    return GeoIPStatusResponse(
        available=available,
        database_path=GEOIP_DB_PATH,
        message="GeoIP is available" if available else "GeoIP database not found. Install geoip2 and download GeoLite2-Country.mmdb"
    )
