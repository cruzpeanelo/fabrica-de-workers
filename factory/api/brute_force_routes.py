# -*- coding: utf-8 -*-
"""
Brute Force Routes - Issue #402
===============================
API endpoints for managing brute force protection.

Admin endpoints for:
- Viewing lockouts
- Unlocking accounts
- Managing whitelist
- Viewing statistics
"""

from typing import Optional
from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel, Field
import logging

from factory.security.brute_force import (
    BruteForceProtection,
    check_brute_force,
    is_blocked,
    unlock_account
)

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/brute-force", tags=["Brute Force Protection"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class BlockRequest(BaseModel):
    """Request to manually block an identifier."""
    identifier: str = Field(..., min_length=1)
    duration_seconds: int = Field(default=86400, ge=60, le=604800)  # 1 min to 7 days
    reason: Optional[str] = "admin_blocked"


class WhitelistRequest(BaseModel):
    """Request to add/remove from whitelist."""
    identifier: str = Field(..., min_length=1)


class CheckRequest(BaseModel):
    """Request to check if an identifier is blocked."""
    identifier: str
    ip_address: Optional[str] = None


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

@router.get("/stats")
async def get_stats(request: Request):
    """Get brute force protection statistics."""
    require_admin(request)

    return {
        "stats": BruteForceProtection.get_stats()
    }


@router.get("/lockouts")
async def list_lockouts(request: Request):
    """List all active lockouts."""
    require_admin(request)

    lockouts = BruteForceProtection.get_active_lockouts()

    return {
        "lockouts": lockouts,
        "count": len(lockouts)
    }


@router.get("/lockouts/{identifier}")
async def get_lockout(identifier: str, request: Request):
    """Get lockout status for a specific identifier."""
    require_admin(request)

    lockout = BruteForceProtection.get_lockout(identifier)

    if not lockout:
        return {
            "identifier": identifier,
            "is_locked": False,
            "message": "No lockout record found"
        }

    return {
        "identifier": identifier,
        "lockout": lockout.to_dict()
    }


@router.delete("/lockouts/{identifier}")
async def unlock(identifier: str, request: Request):
    """Manually unlock an identifier."""
    require_admin(request)

    if not unlock_account(identifier):
        raise HTTPException(404, f"No active lockout found for {identifier}")

    logger.info(f"Admin unlocked: {identifier}")

    return {
        "message": "Account unlocked",
        "identifier": identifier
    }


@router.post("/block")
async def block_identifier(data: BlockRequest, request: Request):
    """Manually block an identifier."""
    require_admin(request)

    BruteForceProtection.block(
        identifier=data.identifier,
        duration_seconds=data.duration_seconds,
        reason=data.reason or "admin_blocked"
    )

    logger.warning(f"Admin blocked {data.identifier} for {data.duration_seconds}s")

    return {
        "message": "Identifier blocked",
        "identifier": data.identifier,
        "duration_seconds": data.duration_seconds
    }


@router.get("/attempts/{identifier}")
async def get_attempts(identifier: str, request: Request, limit: int = 50):
    """Get login attempt history for an identifier."""
    require_admin(request)

    attempts = BruteForceProtection.get_attempts(identifier, limit)

    return {
        "identifier": identifier,
        "attempts": attempts,
        "count": len(attempts)
    }


@router.delete("/attempts/{identifier}")
async def clear_attempts(identifier: str, request: Request):
    """Clear attempt history for an identifier."""
    require_admin(request)

    BruteForceProtection.clear_attempts(identifier)

    return {
        "message": "Attempt history cleared",
        "identifier": identifier
    }


@router.get("/whitelist")
async def list_whitelist(request: Request):
    """List all whitelisted identifiers."""
    require_admin(request)

    whitelist = BruteForceProtection.get_whitelist()

    return {
        "whitelist": whitelist,
        "count": len(whitelist)
    }


@router.post("/whitelist")
async def add_to_whitelist(data: WhitelistRequest, request: Request):
    """Add an identifier to the whitelist."""
    require_admin(request)

    BruteForceProtection.add_to_whitelist(data.identifier)

    logger.info(f"Added to whitelist: {data.identifier}")

    return {
        "message": "Added to whitelist",
        "identifier": data.identifier
    }


@router.delete("/whitelist/{identifier}")
async def remove_from_whitelist(identifier: str, request: Request):
    """Remove an identifier from the whitelist."""
    require_admin(request)

    if not BruteForceProtection.remove_from_whitelist(identifier):
        raise HTTPException(404, f"{identifier} not in whitelist")

    return {
        "message": "Removed from whitelist",
        "identifier": identifier
    }


@router.post("/check")
async def check_status(data: CheckRequest, request: Request):
    """Check if an identifier is blocked (for testing)."""
    require_admin(request)

    allowed, message, remaining = check_brute_force(
        data.identifier,
        data.ip_address
    )

    return {
        "identifier": data.identifier,
        "ip_address": data.ip_address,
        "allowed": allowed,
        "message": message,
        "remaining_seconds": remaining
    }


@router.get("/my-status")
async def check_my_status(request: Request):
    """
    Check if the current client is blocked.

    Non-admin endpoint for clients to check their status.
    """
    # Get client IP
    forwarded = request.headers.get("X-Forwarded-For")
    if forwarded:
        client_ip = forwarded.split(",")[0].strip()
    else:
        client_ip = request.client.host if request.client else "unknown"

    allowed, message, remaining = check_brute_force(client_ip)

    return {
        "ip": client_ip,
        "allowed": allowed,
        "message": message,
        "remaining_seconds": remaining
    }


@router.post("/cleanup")
async def run_cleanup(request: Request):
    """Run cleanup of old records (admin only)."""
    require_admin(request)

    BruteForceProtection.cleanup()

    return {
        "message": "Cleanup completed",
        "stats": BruteForceProtection.get_stats()
    }
