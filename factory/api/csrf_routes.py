# -*- coding: utf-8 -*-
"""
CSRF Routes - Issue #411
========================
API endpoints for CSRF token management.

Endpoints:
- GET /api/csrf-token - Get new CSRF token
- POST /api/csrf-token/refresh - Refresh CSRF token
- GET /api/csrf-token/stats - Statistics (admin)

Author: Fabrica de Agentes - Terminal B
"""

from typing import Optional
from fastapi import APIRouter, HTTPException, Request, Response
from fastapi.responses import JSONResponse
from pydantic import BaseModel
import logging

from factory.security.csrf import (
    CSRFService,
    generate_csrf_token,
    get_csrf_cookie_options,
    CSRF_COOKIE_NAME,
    CSRF_HEADER_NAME,
    CSRF_TOKEN_EXPIRY
)

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/csrf-token", tags=["CSRF"])


# =============================================================================
# REQUEST MODELS
# =============================================================================

class RefreshTokenRequest(BaseModel):
    """Request to refresh CSRF token."""
    old_token: str


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_session_id(request: Request) -> Optional[str]:
    """Get session ID from request."""
    # Try header first
    session_id = request.headers.get("X-Session-ID")
    if session_id:
        return session_id

    # Try user context
    user = getattr(request.state, "user", None)
    if user and isinstance(user, dict):
        return user.get("session_id")

    return None


def get_user_id(request: Request) -> Optional[str]:
    """Get user ID from request."""
    user = getattr(request.state, "user", None)
    if user and isinstance(user, dict):
        return user.get("user_id") or user.get("sub")
    return None


def require_admin(request: Request):
    """Verify request is from admin user."""
    user = getattr(request.state, "user", None)
    if not user:
        raise HTTPException(401, "Authentication required")

    role = user.get("role", "USER") if isinstance(user, dict) else getattr(user, "role", "USER")
    if role not in ("ADMIN", "admin", "SUPER_ADMIN", "PLATFORM_ADMIN"):
        raise HTTPException(403, "Admin access required")


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("")
async def get_csrf_token(request: Request, response: Response):
    """
    Get a CSRF token.

    Returns a new CSRF token for the current session.
    Also sets the token in a cookie for double-submit pattern.
    """
    session_id = get_session_id(request)
    if not session_id:
        # Generate a temporary session ID for unauthenticated requests
        import uuid
        session_id = str(uuid.uuid4())
        logger.debug(f"Generated temp session for CSRF: {session_id[:8]}...")

    user_id = get_user_id(request)
    ip_address = request.client.host if request.client else None

    token = generate_csrf_token(session_id, user_id)

    # Set cookie for double-submit pattern
    cookie_options = get_csrf_cookie_options()
    response.set_cookie(
        key=cookie_options["key"],
        value=token,
        httponly=cookie_options["httponly"],
        secure=cookie_options.get("secure", False),
        samesite=cookie_options.get("samesite", "lax"),
        max_age=cookie_options.get("max_age", CSRF_TOKEN_EXPIRY)
    )

    return {
        "csrf_token": token,
        "header_name": CSRF_HEADER_NAME,
        "cookie_name": CSRF_COOKIE_NAME,
        "expires_in": CSRF_TOKEN_EXPIRY,
        "session_id": session_id
    }


@router.post("/refresh")
async def refresh_csrf_token(
    request: Request,
    response: Response,
    data: RefreshTokenRequest
):
    """
    Refresh CSRF token.

    Validates the old token and generates a new one.
    """
    session_id = get_session_id(request)
    if not session_id:
        raise HTTPException(400, "Session ID required for token refresh")

    user_id = get_user_id(request)
    ip_address = request.client.host if request.client else None

    new_token = CSRFService.refresh_token(
        session_id=session_id,
        old_token=data.old_token,
        user_id=user_id,
        ip_address=ip_address
    )

    if not new_token:
        raise HTTPException(400, "Invalid or expired token")

    # Update cookie
    cookie_options = get_csrf_cookie_options()
    response.set_cookie(
        key=cookie_options["key"],
        value=new_token,
        httponly=cookie_options["httponly"],
        secure=cookie_options.get("secure", False),
        samesite=cookie_options.get("samesite", "lax"),
        max_age=cookie_options.get("max_age", CSRF_TOKEN_EXPIRY)
    )

    return {
        "csrf_token": new_token,
        "expires_in": CSRF_TOKEN_EXPIRY,
        "old_token_revoked": True
    }


@router.delete("")
async def revoke_csrf_token(request: Request, response: Response):
    """
    Revoke current CSRF token.

    Removes the CSRF token for the current session.
    """
    session_id = get_session_id(request)
    if not session_id:
        raise HTTPException(400, "Session ID required")

    count = CSRFService.revoke_session_tokens(session_id)

    # Clear cookie
    response.delete_cookie(CSRF_COOKIE_NAME)

    return {
        "message": "CSRF tokens revoked",
        "tokens_revoked": count
    }


@router.get("/validate")
async def validate_token(request: Request):
    """
    Validate current CSRF token.

    Checks if the token in the header/cookie is valid.
    """
    session_id = get_session_id(request)
    if not session_id:
        return {
            "valid": False,
            "message": "No session ID"
        }

    # Get token from header
    token = request.headers.get(CSRF_HEADER_NAME)
    cookie_token = request.cookies.get(CSRF_COOKIE_NAME)

    if token and cookie_token:
        valid, message = CSRFService.validate_request(token, cookie_token, session_id)
    elif token:
        valid, message = CSRFService.validate_token(token, session_id)
    elif cookie_token:
        valid, message = CSRFService.validate_token(cookie_token, session_id)
    else:
        valid, message = False, "No CSRF token provided"

    return {
        "valid": valid,
        "message": message,
        "has_header": bool(token),
        "has_cookie": bool(cookie_token)
    }


# =============================================================================
# ADMIN ENDPOINTS
# =============================================================================

@router.get("/stats")
async def get_stats(request: Request):
    """
    Get CSRF token statistics.

    Admin only endpoint.
    """
    require_admin(request)

    return {
        "stats": CSRFService.get_stats()
    }


@router.post("/cleanup")
async def run_cleanup(request: Request):
    """
    Run CSRF token cleanup.

    Admin only endpoint.
    """
    require_admin(request)

    cleaned = CSRFService.cleanup()

    return {
        "message": "Cleanup completed",
        "tokens_cleaned": cleaned,
        "stats": CSRFService.get_stats()
    }
