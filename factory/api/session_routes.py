# -*- coding: utf-8 -*-
"""
Session Routes - Issue #409
===========================
API endpoints for session management.

Endpoints:
- GET /api/sessions - List user's sessions
- DELETE /api/sessions - Logout all sessions
- DELETE /api/sessions/{session_id} - Invalidate specific session
- GET /api/sessions/stats - Session statistics (admin)
- PUT /api/sessions/config - Update session config (admin)

Author: Fabrica de Agentes - Terminal B
"""

from typing import Optional, List
from fastapi import APIRouter, HTTPException, Request
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
import logging

from factory.auth.session_manager import (
    SessionManager,
    SessionConfig,
    SessionLimitError,
    get_user_sessions,
    invalidate_session,
    logout_all_sessions
)

logger = logging.getLogger(__name__)


router = APIRouter(prefix="/api/sessions", tags=["Sessions"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class SessionConfigRequest(BaseModel):
    """Request to update session configuration."""
    inactivity_timeout: int = Field(default=1800, ge=60, le=86400)  # 1 min to 24h
    absolute_timeout: int = Field(default=28800, ge=300, le=604800)  # 5 min to 7 days
    max_concurrent_sessions: int = Field(default=5, ge=1, le=50)
    invalidate_oldest_on_limit: bool = True
    tenant_id: Optional[str] = None


class LogoutAllRequest(BaseModel):
    """Request to logout all sessions."""
    keep_current: bool = True  # Keep current session active


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_current_user(request: Request) -> dict:
    """Get current user from request context."""
    user = getattr(request.state, "user", None)
    if not user:
        raise HTTPException(401, "Authentication required")
    return user


def require_admin(request: Request):
    """Verify request is from admin user."""
    user = get_current_user(request)
    role = user.get("role", "USER") if isinstance(user, dict) else getattr(user, "role", "USER")

    if role not in ("ADMIN", "admin", "SUPER_ADMIN", "PLATFORM_ADMIN"):
        raise HTTPException(403, "Admin access required")


def get_session_id_from_request(request: Request) -> Optional[str]:
    """Extract session ID from request (from JWT or header)."""
    # Try X-Session-ID header
    session_id = request.headers.get("X-Session-ID")
    if session_id:
        return session_id

    # Try from user context (if set by middleware)
    user = getattr(request.state, "user", None)
    if user and isinstance(user, dict):
        return user.get("session_id")

    return None


# =============================================================================
# USER ENDPOINTS
# =============================================================================

@router.get("")
async def list_my_sessions(request: Request):
    """
    List current user's sessions.

    Returns all active sessions for the authenticated user.
    """
    user = get_current_user(request)
    user_id = user.get("user_id") or user.get("sub")

    if not user_id:
        raise HTTPException(400, "User ID not found in token")

    sessions = get_user_sessions(str(user_id))
    current_session = get_session_id_from_request(request)

    # Mark current session
    for session in sessions:
        session["is_current"] = session["session_id"] == current_session

    return {
        "sessions": sessions,
        "count": len(sessions),
        "current_session_id": current_session
    }


@router.delete("")
async def logout_all(request: Request, data: LogoutAllRequest = None):
    """
    Logout from all sessions.

    Invalidates all sessions for the current user.
    Optionally keeps the current session active.
    """
    user = get_current_user(request)
    user_id = user.get("user_id") or user.get("sub")

    if not user_id:
        raise HTTPException(400, "User ID not found in token")

    current_session = None
    if data and data.keep_current:
        current_session = get_session_id_from_request(request)

    count = logout_all_sessions(str(user_id), except_current=current_session)

    logger.info(f"User {user_id} logged out from {count} sessions")

    return {
        "message": f"Logged out from {count} sessions",
        "sessions_invalidated": count,
        "current_session_kept": current_session is not None
    }


@router.delete("/{session_id}")
async def invalidate_specific_session(session_id: str, request: Request):
    """
    Invalidate a specific session.

    Only the session owner or admin can invalidate a session.
    """
    user = get_current_user(request)
    user_id = str(user.get("user_id") or user.get("sub"))
    role = user.get("role", "USER")

    # Get session to verify ownership
    session = SessionManager.get_session(session_id)
    if not session:
        raise HTTPException(404, "Session not found")

    # Check permission
    is_admin = role in ("ADMIN", "admin", "SUPER_ADMIN", "PLATFORM_ADMIN")
    is_owner = str(session.user_id) == user_id

    if not is_admin and not is_owner:
        raise HTTPException(403, "Not authorized to invalidate this session")

    # Check if trying to invalidate current session
    current_session = get_session_id_from_request(request)
    if session_id == current_session:
        raise HTTPException(400, "Cannot invalidate current session. Use logout endpoint instead.")

    if not invalidate_session(session_id):
        raise HTTPException(500, "Failed to invalidate session")

    return {
        "message": "Session invalidated",
        "session_id": session_id
    }


@router.get("/current")
async def get_current_session(request: Request):
    """
    Get current session information.
    """
    session_id = get_session_id_from_request(request)
    if not session_id:
        return {
            "session_id": None,
            "message": "No session ID in request"
        }

    session = SessionManager.get_session(session_id)
    if not session:
        return {
            "session_id": session_id,
            "valid": False,
            "message": "Session not found"
        }

    valid, message = SessionManager.validate_session(session_id, refresh=False)

    return {
        "session": session.to_dict(),
        "valid": valid,
        "message": message
    }


# =============================================================================
# ADMIN ENDPOINTS
# =============================================================================

@router.get("/stats")
async def get_session_stats(request: Request):
    """
    Get session statistics.

    Admin only endpoint.
    """
    require_admin(request)

    return {
        "stats": SessionManager.get_stats()
    }


@router.get("/all")
async def list_all_sessions(
    request: Request,
    active_only: bool = True,
    user_id: Optional[str] = None,
    limit: int = 100
):
    """
    List all sessions.

    Admin only endpoint.
    """
    require_admin(request)

    if user_id:
        sessions = SessionManager.get_user_sessions(user_id, active_only=active_only)
    else:
        sessions = SessionManager.get_all_sessions(active_only=active_only)

    return {
        "sessions": [s.to_dict() for s in sessions[:limit]],
        "count": len(sessions),
        "filtered_by_user": user_id,
        "active_only": active_only
    }


@router.put("/config")
async def update_session_config(data: SessionConfigRequest, request: Request):
    """
    Update session configuration.

    Admin only endpoint.
    """
    require_admin(request)

    config = SessionConfig(
        tenant_id=data.tenant_id,
        inactivity_timeout=data.inactivity_timeout,
        absolute_timeout=data.absolute_timeout,
        max_concurrent_sessions=data.max_concurrent_sessions,
        invalidate_oldest_on_limit=data.invalidate_oldest_on_limit
    )

    SessionManager.set_config(config, data.tenant_id)

    logger.info(
        f"Session config updated for {data.tenant_id or 'global'}: "
        f"inactivity={data.inactivity_timeout}s, absolute={data.absolute_timeout}s, "
        f"max_concurrent={data.max_concurrent_sessions}"
    )

    return {
        "message": "Configuration updated",
        "config": {
            "tenant_id": data.tenant_id or "global",
            "inactivity_timeout": data.inactivity_timeout,
            "absolute_timeout": data.absolute_timeout,
            "max_concurrent_sessions": data.max_concurrent_sessions,
            "invalidate_oldest_on_limit": data.invalidate_oldest_on_limit
        }
    }


@router.delete("/user/{user_id}")
async def force_logout_user(user_id: str, request: Request):
    """
    Force logout a specific user from all sessions.

    Admin only endpoint.
    """
    require_admin(request)

    count = logout_all_sessions(user_id)

    logger.warning(f"Admin forced logout for user {user_id}: {count} sessions invalidated")

    return {
        "message": f"User {user_id} logged out from all sessions",
        "sessions_invalidated": count
    }


@router.post("/cleanup")
async def run_cleanup(request: Request):
    """
    Run session cleanup.

    Admin only endpoint.
    """
    require_admin(request)

    cleaned = SessionManager.cleanup()

    return {
        "message": "Cleanup completed",
        "sessions_cleaned": cleaned,
        "stats": SessionManager.get_stats()
    }
