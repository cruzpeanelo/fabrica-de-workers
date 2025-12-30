# -*- coding: utf-8 -*-
"""
Mobile Auth Routes - Issue #384
===============================
API endpoints for mobile app authentication.
"""

from typing import Optional
from fastapi import APIRouter, HTTPException, Request, Header
from pydantic import BaseModel, Field

from factory.auth.refresh_token import (
    RefreshTokenService,
    TokenPair,
    create_tokens,
    refresh_tokens,
    revoke_all_tokens
)
from factory.auth.device_manager import (
    DeviceManager,
    DevicePlatform
)
from factory.security.ip_policy import get_ip_policy_service


router = APIRouter(prefix="/api/auth", tags=["Mobile Auth"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class RefreshTokenRequest(BaseModel):
    """Request to refresh tokens."""
    refresh_token: str = Field(..., min_length=10)


class TokenResponse(BaseModel):
    """Token pair response."""
    access_token: str
    refresh_token: str
    token_type: str = "Bearer"
    expires_in: int


class RegisterDeviceRequest(BaseModel):
    """Request to register a device."""
    device_id: str = Field(..., min_length=1)
    device_name: str = Field(..., min_length=1, max_length=100)
    platform: str = Field(..., description="ios, android, web, desktop")
    push_token: Optional[str] = None
    app_version: Optional[str] = None
    os_version: Optional[str] = None


class DeviceResponse(BaseModel):
    """Device registration response."""
    device_id: str
    device_name: str
    platform: str
    trust_level: str
    registered_at: str


class LogoutAllRequest(BaseModel):
    """Request to logout all devices."""
    notify_devices: bool = Field(default=True, description="Send push to other devices")


class LogoutAllResponse(BaseModel):
    """Logout all response."""
    sessions_terminated: int
    devices_notified: int
    message: str


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_client_ip(request: Request) -> str:
    """Get client IP from request."""
    # Check for mobile app header
    device_ip = request.headers.get("X-Device-IP")
    if device_ip:
        return device_ip

    # Check forwarded headers
    forwarded = request.headers.get("X-Forwarded-For")
    if forwarded:
        return forwarded.split(",")[0].strip()

    return request.client.host if request.client else "unknown"


def get_current_user(request: Request) -> dict:
    """Extract user from request state or headers."""
    # From request state (set by auth middleware)
    if hasattr(request.state, "user"):
        return request.state.user

    # From headers (for mobile apps)
    user_id = request.headers.get("X-User-Id")
    tenant_id = request.headers.get("X-Tenant-Id")
    role = request.headers.get("X-User-Role", "USER")

    if user_id and tenant_id:
        return {
            "user_id": user_id,
            "tenant_id": tenant_id,
            "role": role
        }

    return None


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/refresh", response_model=TokenResponse)
async def refresh_token(
    data: RefreshTokenRequest,
    request: Request
):
    """
    Refresh access and refresh tokens.

    Implements token rotation - old refresh token is invalidated.
    """
    ip_address = get_client_ip(request)

    token_pair, error = refresh_tokens(data.refresh_token)

    if not token_pair:
        raise HTTPException(401, error)

    return TokenResponse(
        access_token=token_pair.access_token,
        refresh_token=token_pair.refresh_token,
        token_type=token_pair.token_type,
        expires_in=token_pair.expires_in
    )


@router.post("/register-device", response_model=DeviceResponse)
async def register_device(
    data: RegisterDeviceRequest,
    request: Request
):
    """
    Register a mobile device.

    Call after successful login to enable:
    - Push notifications
    - Device-specific security
    - Multi-device management
    """
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    ip_address = get_client_ip(request)

    # Validate IP against tenant policy
    tenant_id = user.get("tenant_id")
    if tenant_id:
        ip_service = get_ip_policy_service()
        allowed, reason, message = ip_service.check_access(
            tenant_id=tenant_id,
            client_ip=ip_address,
            path="/api/auth/register-device"
        )

        if not allowed:
            raise HTTPException(
                403,
                detail=message,
                headers={
                    "X-Blocked-Reason": reason.value if reason else "ip_blocked",
                    "X-Client-IP": ip_address
                }
            )

    # Register device
    device = DeviceManager.register_device(
        user_id=user["user_id"],
        tenant_id=tenant_id,
        device_id=data.device_id,
        device_name=data.device_name,
        platform=data.platform,
        push_token=data.push_token,
        ip_address=ip_address,
        app_version=data.app_version,
        os_version=data.os_version
    )

    return DeviceResponse(
        device_id=device.device_id,
        device_name=device.device_name,
        platform=device.platform.value,
        trust_level=device.trust_level.value,
        registered_at=device.registered_at.isoformat()
    )


@router.post("/logout-all-devices", response_model=LogoutAllResponse)
async def logout_all_devices(
    data: LogoutAllRequest,
    request: Request
):
    """
    Logout from all devices.

    - Revokes all refresh tokens
    - Optionally sends push notification to other devices
    """
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    user_id = user["user_id"]
    current_device = request.headers.get("X-Device-Id")

    # Revoke all tokens
    sessions_terminated = revoke_all_tokens(user_id)

    # Notify other devices
    devices_notified = 0
    if data.notify_devices:
        devices_notified = DeviceManager.send_push_to_user(
            user_id=user_id,
            title="Session Terminated",
            body="You have been logged out from all devices",
            exclude_device=current_device
        )

    return LogoutAllResponse(
        sessions_terminated=sessions_terminated,
        devices_notified=devices_notified,
        message="All sessions terminated successfully"
    )


@router.get("/devices")
async def list_devices(request: Request):
    """List all registered devices for the current user."""
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    devices = DeviceManager.get_user_devices(user["user_id"])

    return {
        "devices": [d.to_dict() for d in devices],
        "count": len(devices)
    }


@router.delete("/devices/{device_id}")
async def unregister_device(
    device_id: str,
    request: Request
):
    """Unregister a device."""
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    success = DeviceManager.unregister_device(device_id, user["user_id"])

    if not success:
        raise HTTPException(404, "Device not found")

    # Revoke tokens for this device
    from factory.auth.refresh_token import RefreshTokenService
    RefreshTokenService.revoke_device_tokens(user["user_id"], device_id)

    return {"message": "Device unregistered", "device_id": device_id}


@router.get("/sessions")
async def list_sessions(request: Request):
    """List all active sessions for the current user."""
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    sessions = RefreshTokenService.get_user_active_sessions(user["user_id"])

    return {
        "sessions": sessions,
        "count": len(sessions)
    }


@router.post("/validate-ip")
async def validate_ip(
    request: Request,
    x_device_ip: Optional[str] = Header(None)
):
    """
    Validate if client IP is allowed for the tenant.

    Mobile apps should call this before sensitive operations.
    """
    user = get_current_user(request)
    if not user:
        raise HTTPException(401, "Authentication required")

    ip_address = x_device_ip or get_client_ip(request)
    tenant_id = user.get("tenant_id")

    if not tenant_id:
        return {"allowed": True, "ip": ip_address, "reason": "No tenant policy"}

    ip_service = get_ip_policy_service()
    allowed, reason, message = ip_service.check_access(
        tenant_id=tenant_id,
        client_ip=ip_address,
        path="/api/auth/validate-ip"
    )

    return {
        "allowed": allowed,
        "ip": ip_address,
        "reason": message if not allowed else "IP allowed"
    }
