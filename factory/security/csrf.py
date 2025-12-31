# -*- coding: utf-8 -*-
"""
CSRF Protection - Issue #411
============================
Cross-Site Request Forgery protection for API endpoints.

Features:
- Token generation per session
- Token validation middleware
- Configurable expiration
- Double-submit cookie pattern

Author: Fabrica de Agentes - Terminal B
"""

import secrets
import time
import hashlib
import threading
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Tuple
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

CSRF_TOKEN_LENGTH = 32  # 256 bits
CSRF_TOKEN_EXPIRY = 3600  # 1 hour
CSRF_HEADER_NAME = "X-CSRF-Token"
CSRF_COOKIE_NAME = "csrf_token"
CLEANUP_INTERVAL = 600  # 10 minutes


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class CSRFToken:
    """Represents a CSRF token."""
    token: str
    session_id: str
    user_id: Optional[str]
    created_at: datetime
    expires_at: datetime
    ip_address: Optional[str] = None

    def is_expired(self) -> bool:
        """Check if token has expired."""
        return datetime.utcnow() > self.expires_at

    def is_valid_for_session(self, session_id: str) -> bool:
        """Check if token is valid for the given session."""
        return self.session_id == session_id and not self.is_expired()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "token": self.token,
            "session_id": self.session_id,
            "user_id": self.user_id,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat(),
            "is_expired": self.is_expired()
        }


# =============================================================================
# STORAGE
# =============================================================================

_tokens: Dict[str, CSRFToken] = {}  # token -> CSRFToken
_session_tokens: Dict[str, str] = {}  # session_id -> token
_lock = threading.Lock()


# =============================================================================
# CSRF SERVICE
# =============================================================================

class CSRFService:
    """
    CSRF Protection Service - Issue #411

    Generates and validates CSRF tokens using the double-submit cookie pattern.
    """

    # -------------------------------------------------------------------------
    # TOKEN GENERATION
    # -------------------------------------------------------------------------

    @classmethod
    def generate_token(
        cls,
        session_id: str,
        user_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        expiry_seconds: int = CSRF_TOKEN_EXPIRY
    ) -> str:
        """
        Generate a new CSRF token for a session.

        If a token already exists for the session, it returns the existing one
        unless it's close to expiration.
        """
        with _lock:
            # Check for existing valid token
            existing_token = _session_tokens.get(session_id)
            if existing_token and existing_token in _tokens:
                token_obj = _tokens[existing_token]
                # Return existing if more than 50% time remaining
                remaining = (token_obj.expires_at - datetime.utcnow()).total_seconds()
                if remaining > expiry_seconds / 2:
                    return existing_token

            # Generate new token
            token = secrets.token_urlsafe(CSRF_TOKEN_LENGTH)
            now = datetime.utcnow()

            token_obj = CSRFToken(
                token=token,
                session_id=session_id,
                user_id=user_id,
                created_at=now,
                expires_at=now + timedelta(seconds=expiry_seconds),
                ip_address=ip_address
            )

            _tokens[token] = token_obj
            _session_tokens[session_id] = token

            logger.debug(f"CSRF token generated for session {session_id[:8]}...")

            return token

    @classmethod
    def refresh_token(
        cls,
        session_id: str,
        old_token: str,
        user_id: Optional[str] = None,
        ip_address: Optional[str] = None
    ) -> Optional[str]:
        """
        Refresh a CSRF token.

        Validates the old token before generating a new one.
        """
        with _lock:
            # Validate old token
            if old_token not in _tokens:
                logger.warning(f"CSRF refresh failed: old token not found")
                return None

            old_token_obj = _tokens[old_token]
            if old_token_obj.session_id != session_id:
                logger.warning(f"CSRF refresh failed: session mismatch")
                return None

            # Remove old token
            del _tokens[old_token]

        # Generate new token
        return cls.generate_token(session_id, user_id, ip_address)

    # -------------------------------------------------------------------------
    # TOKEN VALIDATION
    # -------------------------------------------------------------------------

    @classmethod
    def validate_token(
        cls,
        token: str,
        session_id: str,
        ip_address: Optional[str] = None
    ) -> Tuple[bool, str]:
        """
        Validate a CSRF token.

        Args:
            token: The CSRF token to validate
            session_id: The session ID the token should belong to
            ip_address: Optional IP to verify (if IP binding is enabled)

        Returns:
            (valid, message) tuple
        """
        if not token:
            return False, "CSRF token is required"

        token_obj = _tokens.get(token)
        if not token_obj:
            return False, "Invalid CSRF token"

        if token_obj.is_expired():
            return False, "CSRF token has expired"

        if token_obj.session_id != session_id:
            logger.warning(
                f"CSRF validation failed: session mismatch. "
                f"Expected {token_obj.session_id[:8]}..., got {session_id[:8]}..."
            )
            return False, "CSRF token session mismatch"

        # Optional IP validation (disabled by default for flexibility)
        # if ip_address and token_obj.ip_address and token_obj.ip_address != ip_address:
        #     return False, "CSRF token IP mismatch"

        return True, "Valid"

    @classmethod
    def validate_request(
        cls,
        token: str,
        cookie_token: str,
        session_id: str
    ) -> Tuple[bool, str]:
        """
        Validate CSRF using double-submit cookie pattern.

        Both the header token and cookie token must match.
        """
        if not token or not cookie_token:
            return False, "Both CSRF header and cookie are required"

        if token != cookie_token:
            return False, "CSRF header and cookie tokens do not match"

        return cls.validate_token(token, session_id)

    # -------------------------------------------------------------------------
    # TOKEN MANAGEMENT
    # -------------------------------------------------------------------------

    @classmethod
    def revoke_token(cls, token: str) -> bool:
        """Revoke a specific token."""
        with _lock:
            if token in _tokens:
                token_obj = _tokens[token]
                del _tokens[token]
                if _session_tokens.get(token_obj.session_id) == token:
                    del _session_tokens[token_obj.session_id]
                logger.debug(f"CSRF token revoked")
                return True
            return False

    @classmethod
    def revoke_session_tokens(cls, session_id: str) -> int:
        """Revoke all tokens for a session."""
        count = 0
        with _lock:
            tokens_to_remove = [
                t for t, obj in _tokens.items()
                if obj.session_id == session_id
            ]
            for token in tokens_to_remove:
                del _tokens[token]
                count += 1

            if session_id in _session_tokens:
                del _session_tokens[session_id]

        return count

    @classmethod
    def get_session_token(cls, session_id: str) -> Optional[str]:
        """Get current token for a session."""
        token = _session_tokens.get(session_id)
        if token and token in _tokens:
            token_obj = _tokens[token]
            if not token_obj.is_expired():
                return token
        return None

    # -------------------------------------------------------------------------
    # STATISTICS
    # -------------------------------------------------------------------------

    @classmethod
    def get_stats(cls) -> Dict[str, Any]:
        """Get CSRF token statistics."""
        active = sum(1 for t in _tokens.values() if not t.is_expired())
        expired = sum(1 for t in _tokens.values() if t.is_expired())

        return {
            "total_tokens": len(_tokens),
            "active_tokens": active,
            "expired_tokens": expired,
            "sessions_with_tokens": len(_session_tokens),
            "token_expiry_seconds": CSRF_TOKEN_EXPIRY
        }

    # -------------------------------------------------------------------------
    # CLEANUP
    # -------------------------------------------------------------------------

    @classmethod
    def cleanup(cls):
        """Clean up expired tokens."""
        cleaned = 0
        with _lock:
            expired_tokens = [
                token for token, obj in _tokens.items()
                if obj.is_expired()
            ]
            for token in expired_tokens:
                token_obj = _tokens[token]
                del _tokens[token]
                if _session_tokens.get(token_obj.session_id) == token:
                    del _session_tokens[token_obj.session_id]
                cleaned += 1

        if cleaned > 0:
            logger.debug(f"CSRF cleanup: removed {cleaned} expired tokens")

        return cleaned


# =============================================================================
# MIDDLEWARE
# =============================================================================

# Methods that require CSRF validation
CSRF_PROTECTED_METHODS = ["POST", "PUT", "DELETE", "PATCH"]

# Paths that skip CSRF validation
CSRF_EXEMPT_PATHS = [
    "/api/auth/login",
    "/api/auth/register",
    "/api/v1/auth/login",
    "/api/v1/auth/register",
    "/api/webhooks",
    "/api/public",
    "/health",
    "/api/csrf-token",  # Token endpoint itself is exempt
]


async def csrf_middleware(request, call_next):
    """
    CSRF validation middleware.

    Validates CSRF token for protected methods (POST, PUT, DELETE, PATCH).
    """
    from fastapi import Request
    from fastapi.responses import JSONResponse

    path = request.url.path
    method = request.method

    # Skip non-protected methods
    if method not in CSRF_PROTECTED_METHODS:
        return await call_next(request)

    # Skip exempt paths
    for exempt in CSRF_EXEMPT_PATHS:
        if path == exempt or path.startswith(exempt + "/"):
            return await call_next(request)

    # Get session ID from request
    session_id = request.headers.get("X-Session-ID")
    if not session_id:
        # Try from user context
        user = getattr(request.state, "user", None)
        if user and isinstance(user, dict):
            session_id = user.get("session_id")

    if not session_id:
        # No session = no CSRF needed (probably API key auth)
        return await call_next(request)

    # Get CSRF token from header
    token = request.headers.get(CSRF_HEADER_NAME)

    # Get CSRF token from cookie
    cookie_token = request.cookies.get(CSRF_COOKIE_NAME)

    # Validate using double-submit pattern
    if token and cookie_token:
        valid, message = CSRFService.validate_request(token, cookie_token, session_id)
    elif token:
        valid, message = CSRFService.validate_token(token, session_id)
    else:
        valid, message = False, "CSRF token required"

    if not valid:
        logger.warning(f"CSRF validation failed: {message} for path {path}")
        return JSONResponse(
            status_code=403,
            content={
                "detail": message,
                "code": "CSRF_VALIDATION_FAILED"
            },
            headers={"X-Error-Code": "CSRF_VALIDATION_FAILED"}
        )

    return await call_next(request)


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def generate_csrf_token(
    session_id: str,
    user_id: str = None
) -> str:
    """Generate a CSRF token for a session."""
    return CSRFService.generate_token(session_id, user_id)


def validate_csrf_token(token: str, session_id: str) -> Tuple[bool, str]:
    """Validate a CSRF token."""
    return CSRFService.validate_token(token, session_id)


def get_csrf_cookie_options() -> Dict[str, Any]:
    """Get cookie options for CSRF token."""
    return {
        "key": CSRF_COOKIE_NAME,
        "httponly": False,  # JS needs to read it
        "secure": True,  # HTTPS only
        "samesite": "strict",
        "max_age": CSRF_TOKEN_EXPIRY
    }
