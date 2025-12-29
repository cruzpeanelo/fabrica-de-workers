# -*- coding: utf-8 -*-
"""
MFA API Routes - Issue #103
Fabrica de Agentes v6.5

REST API endpoints for MFA (Multi-Factor Authentication) operations:
- Setup and enrollment
- Verification
- Backup code management
- Recovery flow
- Status and configuration

All endpoints require authentication unless explicitly noted.
"""

from typing import Optional
from datetime import datetime

from fastapi import APIRouter, HTTPException, Depends, Request, Header
from pydantic import BaseModel, Field, EmailStr

from factory.auth.mfa import (
    MFAService,
    MFAStatus,
    MFAMethod,
    MFASetupResponse,
    MFAVerifyResponse,
    MFAEnforcementPolicy
)
from factory.api.auth import get_current_user, TokenData


# =============================================================================
# ROUTER CONFIGURATION
# =============================================================================

mfa_router = APIRouter(prefix="/api/v1/mfa", tags=["MFA"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class MFASetupStartRequest(BaseModel):
    """Request to start MFA setup"""
    pass  # No additional data needed, uses current user


class MFASetupCompleteRequest(BaseModel):
    """Request to complete MFA setup"""
    code: str = Field(..., min_length=6, max_length=6, description="TOTP code from authenticator")


class MFAVerifyRequest(BaseModel):
    """Request to verify MFA during login"""
    code: str = Field(..., min_length=6, max_length=10, description="TOTP or backup code")
    method: MFAMethod = MFAMethod.TOTP


class MFADisableRequest(BaseModel):
    """Request to disable MFA"""
    code: str = Field(..., description="Current TOTP code for verification")
    confirm: bool = Field(False, description="Confirm disabling MFA")


class MFARecoveryStartRequest(BaseModel):
    """Request to start MFA recovery"""
    email: EmailStr


class MFARecoveryCompleteRequest(BaseModel):
    """Request to complete MFA recovery"""
    token: str


class MFARegenerateBackupRequest(BaseModel):
    """Request to regenerate backup codes"""
    code: str = Field(..., min_length=6, max_length=6, description="Current TOTP code")


class MFAStatusResponse(BaseModel):
    """MFA status response"""
    enabled: bool
    status: str
    enabled_at: Optional[str] = None
    last_used_at: Optional[str] = None
    backup_codes_remaining: int = 0
    locked_until: Optional[str] = None
    required: bool = False
    enforcement_reason: Optional[str] = None


class MFABackupCodesResponse(BaseModel):
    """Response with new backup codes"""
    success: bool
    backup_codes: list = []
    message: str


class MFAPolicyResponse(BaseModel):
    """MFA policy response"""
    require_for_roles: list
    require_for_all: bool
    grace_period_days: int
    allow_backup_codes: bool


# =============================================================================
# ENDPOINTS - Setup and Enrollment
# =============================================================================

@mfa_router.post("/setup/start", response_model=MFASetupResponse)
async def start_mfa_setup(
    user: TokenData = Depends(get_current_user)
):
    """
    Start MFA setup for current user.

    Returns:
    - Secret key for manual entry
    - QR code (base64) for authenticator apps
    - Backup codes for recovery

    User must complete setup by verifying their first TOTP code.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        # Get user ID from token
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            result = mfa_service.start_setup(db_user.id)
            return result

        finally:
            db.close()

    except ValueError as e:
        raise HTTPException(400, str(e))
    except Exception as e:
        raise HTTPException(500, f"Failed to start MFA setup: {str(e)}")


@mfa_router.post("/setup/complete", response_model=MFAVerifyResponse)
async def complete_mfa_setup(
    request: MFASetupCompleteRequest,
    user: TokenData = Depends(get_current_user)
):
    """
    Complete MFA setup by verifying first TOTP code.

    This confirms the authenticator app is correctly configured
    and activates MFA for the account.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            result = mfa_service.complete_setup(db_user.id, request.code)

            if not result.success:
                raise HTTPException(400, result.message)

            return result

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Failed to complete MFA setup: {str(e)}")


@mfa_router.post("/setup/cancel")
async def cancel_mfa_setup(
    user: TokenData = Depends(get_current_user)
):
    """
    Cancel pending MFA setup.

    Use this if user wants to start over or abandon MFA enrollment.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            # Clear pending MFA
            if db_user.quotas and "_mfa_pending" in db_user.quotas:
                del db_user.quotas["_mfa_pending"]
                db.commit()

            return {"success": True, "message": "MFA setup cancelled"}

        finally:
            db.close()

    except Exception as e:
        raise HTTPException(500, f"Failed to cancel setup: {str(e)}")


# =============================================================================
# ENDPOINTS - Verification
# =============================================================================

@mfa_router.post("/verify", response_model=MFAVerifyResponse)
async def verify_mfa(
    request: MFAVerifyRequest,
    user: TokenData = Depends(get_current_user)
):
    """
    Verify MFA code.

    Called during login after password authentication.
    Supports both TOTP codes and backup codes.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            result = mfa_service.verify(
                db_user.id,
                request.code,
                request.method
            )

            if not result.success and result.remaining_attempts == 0:
                raise HTTPException(
                    423,  # Locked
                    f"Account locked until {result.locked_until}"
                )

            return result

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Verification failed: {str(e)}")


@mfa_router.post("/verify/bypass")
async def verify_mfa_bypass(
    code: str,
    x_user_id: str = Header(None, alias="X-User-ID")
):
    """
    Verify MFA code during login flow.

    This endpoint is called by the auth flow when MFA is required.
    Uses X-User-ID header for user identification.
    """
    if not x_user_id:
        raise HTTPException(400, "X-User-ID header required")

    try:
        user_id = int(x_user_id)
    except ValueError:
        raise HTTPException(400, "Invalid user ID")

    try:
        mfa_service = MFAService()
        result = mfa_service.verify(user_id, code)

        if not result.success:
            raise HTTPException(401, result.message)

        return {"success": True, "message": "MFA verified"}

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Verification failed: {str(e)}")


# =============================================================================
# ENDPOINTS - Status and Info
# =============================================================================

@mfa_router.get("/status", response_model=MFAStatusResponse)
async def get_mfa_status(
    user: TokenData = Depends(get_current_user),
    x_tenant_id: Optional[str] = Header(None, alias="X-Tenant-ID")
):
    """
    Get current MFA status for user.

    Returns whether MFA is:
    - Enabled/disabled
    - Required by policy
    - Currently locked
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            status = mfa_service.get_status(db_user.id)

            # Check if MFA is required
            required = mfa_service.is_mfa_required(db_user.id, x_tenant_id)
            enforcement_reason = None

            if required and not status.get("enabled"):
                if db_user.role == "ADMIN":
                    enforcement_reason = "Required for admin accounts"
                elif x_tenant_id:
                    enforcement_reason = "Required by tenant policy"

            return MFAStatusResponse(
                enabled=status.get("enabled", False),
                status=status.get("status", MFAStatus.DISABLED.value),
                enabled_at=status.get("enabled_at"),
                last_used_at=status.get("last_used_at"),
                backup_codes_remaining=status.get("backup_codes_remaining", 0),
                locked_until=status.get("locked_until"),
                required=required,
                enforcement_reason=enforcement_reason
            )

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Failed to get status: {str(e)}")


@mfa_router.get("/required")
async def check_mfa_required(
    user: TokenData = Depends(get_current_user),
    x_tenant_id: Optional[str] = Header(None, alias="X-Tenant-ID")
):
    """
    Check if MFA is required for current user.

    Returns requirement status and enforcement policy.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            required = mfa_service.is_mfa_required(db_user.id, x_tenant_id)
            status = mfa_service.get_status(db_user.id)

            return {
                "required": required,
                "enabled": status.get("enabled", False),
                "needs_setup": required and not status.get("enabled", False),
                "role": db_user.role
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Check failed: {str(e)}")


# =============================================================================
# ENDPOINTS - Disable
# =============================================================================

@mfa_router.post("/disable", response_model=MFAVerifyResponse)
async def disable_mfa(
    request: MFADisableRequest,
    user: TokenData = Depends(get_current_user)
):
    """
    Disable MFA for current user.

    Requires:
    - Valid TOTP code for verification
    - Explicit confirmation

    Note: Some accounts may not be allowed to disable MFA
    based on role or tenant policy.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    if not request.confirm:
        raise HTTPException(400, "Must confirm MFA disable with confirm=true")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            # Check if user can disable MFA
            mfa_service = MFAService(db)
            if mfa_service.is_mfa_required(db_user.id):
                raise HTTPException(
                    403,
                    "MFA cannot be disabled for your account due to security policy"
                )

            result = mfa_service.disable(db_user.id, request.code)

            if not result.success:
                raise HTTPException(400, result.message)

            return result

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Failed to disable MFA: {str(e)}")


# =============================================================================
# ENDPOINTS - Backup Codes
# =============================================================================

@mfa_router.post("/backup-codes/regenerate", response_model=MFABackupCodesResponse)
async def regenerate_backup_codes(
    request: MFARegenerateBackupRequest,
    user: TokenData = Depends(get_current_user)
):
    """
    Regenerate backup codes.

    Requires current TOTP code for verification.
    Invalidates all previous backup codes.

    IMPORTANT: Store the new codes securely!
    They will not be shown again.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            result = mfa_service.regenerate_backup_codes(db_user.id, request.code)

            if not result.get("success"):
                raise HTTPException(400, result.get("message"))

            return MFABackupCodesResponse(
                success=True,
                backup_codes=result.get("backup_codes", []),
                message=result.get("message")
            )

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Failed to regenerate codes: {str(e)}")


@mfa_router.get("/backup-codes/count")
async def get_backup_codes_count(
    user: TokenData = Depends(get_current_user)
):
    """
    Get remaining backup codes count.

    Returns how many backup codes are still available.
    Recommends regeneration if count is low.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.username == user.username).first()
            if not db_user:
                raise HTTPException(404, "User not found")

            mfa_service = MFAService(db)
            status = mfa_service.get_status(db_user.id)

            remaining = status.get("backup_codes_remaining", 0)
            low_warning = remaining < 3

            return {
                "remaining": remaining,
                "low_warning": low_warning,
                "recommendation": "Regenerate backup codes" if low_warning else None
            }

        finally:
            db.close()

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Failed to get count: {str(e)}")


# =============================================================================
# ENDPOINTS - Recovery
# =============================================================================

@mfa_router.post("/recovery/start")
async def start_mfa_recovery(request: MFARecoveryStartRequest):
    """
    Start MFA recovery process.

    Sends a recovery link to the user's email.
    The link allows disabling MFA to regain access.

    This endpoint does not require authentication
    (user lost access to their MFA device).
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import User

        db = SessionLocal()
        try:
            db_user = db.query(User).filter(User.email == request.email).first()

            mfa_service = MFAService(db)

            if db_user:
                result = mfa_service.start_recovery(db_user.id, request.email)
            else:
                # Don't reveal if email exists
                result = {
                    "success": True,
                    "message": "If the email is valid, a recovery link will be sent"
                }

            return result

        finally:
            db.close()

    except Exception as e:
        # Don't reveal errors that might indicate user existence
        return {
            "success": True,
            "message": "If the email is valid, a recovery link will be sent"
        }


@mfa_router.post("/recovery/complete")
async def complete_mfa_recovery(request: MFARecoveryCompleteRequest):
    """
    Complete MFA recovery using recovery token.

    Disables MFA for the account, allowing login without MFA.
    User should set up MFA again after logging in.

    This endpoint does not require authentication.
    """
    try:
        mfa_service = MFAService()
        result = mfa_service.complete_recovery(request.token)

        if not result.get("success"):
            raise HTTPException(400, result.get("message"))

        return result

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Recovery failed: {str(e)}")


# =============================================================================
# ENDPOINTS - Policy (Admin)
# =============================================================================

@mfa_router.get("/policy", response_model=MFAPolicyResponse)
async def get_mfa_policy(
    user: TokenData = Depends(get_current_user),
    x_tenant_id: Optional[str] = Header(None, alias="X-Tenant-ID")
):
    """
    Get MFA enforcement policy for current tenant.

    Shows which roles require MFA and other policy settings.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    mfa_service = MFAService()
    policy = mfa_service.get_enforcement_policy(x_tenant_id)

    if not policy:
        return MFAPolicyResponse(
            require_for_roles=["ADMIN"],
            require_for_all=False,
            grace_period_days=7,
            allow_backup_codes=True
        )

    return MFAPolicyResponse(
        require_for_roles=policy.require_for_roles,
        require_for_all=policy.require_for_all,
        grace_period_days=policy.grace_period_days,
        allow_backup_codes=policy.allow_backup_codes
    )


@mfa_router.put("/policy")
async def update_mfa_policy(
    policy: MFAPolicyResponse,
    user: TokenData = Depends(get_current_user),
    x_tenant_id: Optional[str] = Header(None, alias="X-Tenant-ID")
):
    """
    Update MFA enforcement policy for tenant.

    Requires admin role.
    """
    if not user.username:
        raise HTTPException(401, "Authentication required")

    if user.role != "ADMIN":
        raise HTTPException(403, "Admin role required")

    # In production, save to database
    # For now, return success
    return {
        "success": True,
        "message": "MFA policy updated",
        "policy": policy.dict()
    }


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = ["mfa_router"]
