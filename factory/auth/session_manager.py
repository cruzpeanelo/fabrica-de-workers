# -*- coding: utf-8 -*-
"""
Session Manager - Issue #409
============================
Session management with timeout and concurrent session control.

Features:
- Session timeout (inactivity and absolute)
- Concurrent session limits
- Session listing and invalidation
- Integration with token_blacklist.py

Author: Plataforma E - Terminal B
"""

import time
import uuid
import threading
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

DEFAULT_INACTIVITY_TIMEOUT = 1800  # 30 minutes
DEFAULT_ABSOLUTE_TIMEOUT = 28800   # 8 hours
DEFAULT_MAX_CONCURRENT_SESSIONS = 5
CLEANUP_INTERVAL = 300  # 5 minutes


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class Session:
    """Represents a user session."""
    session_id: str
    user_id: str
    tenant_id: Optional[str]
    token_jti: str  # JWT token ID for blacklisting
    created_at: datetime
    last_activity: datetime
    expires_at: datetime
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    device_id: Optional[str] = None
    is_active: bool = True

    def is_expired(self) -> bool:
        """Check if session has expired (absolute timeout)."""
        return datetime.utcnow() > self.expires_at

    def is_inactive(self, timeout_seconds: int = DEFAULT_INACTIVITY_TIMEOUT) -> bool:
        """Check if session is inactive."""
        cutoff = datetime.utcnow() - timedelta(seconds=timeout_seconds)
        return self.last_activity < cutoff

    def refresh(self):
        """Refresh last activity timestamp."""
        self.last_activity = datetime.utcnow()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "session_id": self.session_id,
            "user_id": self.user_id,
            "tenant_id": self.tenant_id,
            "created_at": self.created_at.isoformat(),
            "last_activity": self.last_activity.isoformat(),
            "expires_at": self.expires_at.isoformat(),
            "ip_address": self.ip_address,
            "user_agent": self.user_agent,
            "device_id": self.device_id,
            "is_active": self.is_active,
            "is_expired": self.is_expired()
        }


@dataclass
class SessionConfig:
    """Session configuration per tenant."""
    tenant_id: Optional[str] = None
    inactivity_timeout: int = DEFAULT_INACTIVITY_TIMEOUT
    absolute_timeout: int = DEFAULT_ABSOLUTE_TIMEOUT
    max_concurrent_sessions: int = DEFAULT_MAX_CONCURRENT_SESSIONS
    invalidate_oldest_on_limit: bool = True  # Invalidate oldest session when limit reached


# =============================================================================
# STORAGE
# =============================================================================

_sessions: Dict[str, Session] = {}  # session_id -> Session
_user_sessions: Dict[str, List[str]] = {}  # user_id -> [session_ids]
_tenant_configs: Dict[str, SessionConfig] = {}
_global_config: SessionConfig = SessionConfig()
_lock = threading.Lock()


# =============================================================================
# SESSION MANAGER
# =============================================================================

class SessionManager:
    """
    Session Manager - Issue #409

    Manages user sessions with:
    - Inactivity timeout
    - Absolute timeout
    - Concurrent session limits
    - Session invalidation
    """

    # -------------------------------------------------------------------------
    # SESSION LIFECYCLE
    # -------------------------------------------------------------------------

    @classmethod
    def create_session(
        cls,
        user_id: str,
        token_jti: str,
        tenant_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        device_id: Optional[str] = None
    ) -> Session:
        """
        Create a new session for a user.

        Enforces concurrent session limits.
        """
        config = cls.get_config(tenant_id)

        with _lock:
            # Check concurrent session limit
            user_session_ids = _user_sessions.get(user_id, [])
            active_sessions = [sid for sid in user_session_ids if sid in _sessions and _sessions[sid].is_active]

            if len(active_sessions) >= config.max_concurrent_sessions:
                if config.invalidate_oldest_on_limit:
                    # Invalidate oldest session
                    oldest_sid = active_sessions[0]
                    cls._invalidate_session(oldest_sid, reason="concurrent_limit")
                    active_sessions = active_sessions[1:]
                else:
                    raise SessionLimitError(
                        f"Maximum concurrent sessions ({config.max_concurrent_sessions}) reached"
                    )

            # Create new session
            now = datetime.utcnow()
            session = Session(
                session_id=str(uuid.uuid4()),
                user_id=user_id,
                tenant_id=tenant_id,
                token_jti=token_jti,
                created_at=now,
                last_activity=now,
                expires_at=now + timedelta(seconds=config.absolute_timeout),
                ip_address=ip_address,
                user_agent=user_agent,
                device_id=device_id
            )

            _sessions[session.session_id] = session

            if user_id not in _user_sessions:
                _user_sessions[user_id] = []
            _user_sessions[user_id].append(session.session_id)

            logger.info(
                f"Session created: user={user_id} session={session.session_id[:8]}... "
                f"active_sessions={len(active_sessions) + 1}"
            )

            return session

    @classmethod
    def get_session(cls, session_id: str) -> Optional[Session]:
        """Get session by ID."""
        return _sessions.get(session_id)

    @classmethod
    def validate_session(
        cls,
        session_id: str,
        refresh: bool = True
    ) -> tuple:
        """
        Validate a session.

        Returns:
            (valid, message) - whether session is valid
        """
        session = _sessions.get(session_id)
        if not session:
            return False, "Session not found"

        if not session.is_active:
            return False, "Session has been invalidated"

        if session.is_expired():
            cls._invalidate_session(session_id, reason="expired")
            return False, "Session has expired"

        config = cls.get_config(session.tenant_id)
        if session.is_inactive(config.inactivity_timeout):
            cls._invalidate_session(session_id, reason="inactive")
            return False, "Session timed out due to inactivity"

        if refresh:
            session.refresh()

        return True, None

    @classmethod
    def invalidate_session(cls, session_id: str, reason: str = "manual") -> bool:
        """Invalidate a specific session."""
        with _lock:
            return cls._invalidate_session(session_id, reason)

    @classmethod
    def _invalidate_session(cls, session_id: str, reason: str) -> bool:
        """Internal session invalidation (must hold lock)."""
        session = _sessions.get(session_id)
        if not session:
            return False

        session.is_active = False

        # Blacklist the token
        try:
            from factory.auth.token_blacklist import blacklist_token
            blacklist_token(session.token_jti, reason=f"session_{reason}")
        except ImportError:
            logger.warning("token_blacklist module not available")

        logger.info(f"Session invalidated: session={session_id[:8]}... reason={reason}")
        return True

    @classmethod
    def invalidate_all_user_sessions(
        cls,
        user_id: str,
        except_session: Optional[str] = None
    ) -> int:
        """Invalidate all sessions for a user."""
        count = 0
        with _lock:
            session_ids = _user_sessions.get(user_id, []).copy()
            for sid in session_ids:
                if sid != except_session:
                    if cls._invalidate_session(sid, reason="logout_all"):
                        count += 1

        logger.info(f"Invalidated {count} sessions for user {user_id}")
        return count

    # -------------------------------------------------------------------------
    # SESSION LISTING
    # -------------------------------------------------------------------------

    @classmethod
    def get_user_sessions(cls, user_id: str, active_only: bool = True) -> List[Session]:
        """Get all sessions for a user."""
        session_ids = _user_sessions.get(user_id, [])
        sessions = []

        for sid in session_ids:
            session = _sessions.get(sid)
            if session:
                if not active_only or session.is_active:
                    sessions.append(session)

        return sessions

    @classmethod
    def get_active_session_count(cls, user_id: str) -> int:
        """Get count of active sessions for a user."""
        return len(cls.get_user_sessions(user_id, active_only=True))

    @classmethod
    def get_all_sessions(cls, active_only: bool = True) -> List[Session]:
        """Get all sessions (admin only)."""
        sessions = []
        for session in _sessions.values():
            if not active_only or session.is_active:
                sessions.append(session)
        return sessions

    # -------------------------------------------------------------------------
    # CONFIGURATION
    # -------------------------------------------------------------------------

    @classmethod
    def get_config(cls, tenant_id: Optional[str] = None) -> SessionConfig:
        """Get session config for tenant or global."""
        if tenant_id and tenant_id in _tenant_configs:
            return _tenant_configs[tenant_id]
        return _global_config

    @classmethod
    def set_config(cls, config: SessionConfig, tenant_id: Optional[str] = None):
        """Set session config for tenant or global."""
        if tenant_id:
            config.tenant_id = tenant_id
            _tenant_configs[tenant_id] = config
            logger.info(f"Session config updated for tenant {tenant_id}")
        else:
            global _global_config
            _global_config = config
            logger.info("Global session config updated")

    # -------------------------------------------------------------------------
    # STATISTICS
    # -------------------------------------------------------------------------

    @classmethod
    def get_stats(cls) -> Dict[str, Any]:
        """Get session statistics."""
        active = sum(1 for s in _sessions.values() if s.is_active)
        expired = sum(1 for s in _sessions.values() if s.is_expired())
        users_with_sessions = len(_user_sessions)

        return {
            "total_sessions": len(_sessions),
            "active_sessions": active,
            "expired_sessions": expired,
            "users_with_sessions": users_with_sessions,
            "config": {
                "inactivity_timeout": _global_config.inactivity_timeout,
                "absolute_timeout": _global_config.absolute_timeout,
                "max_concurrent_sessions": _global_config.max_concurrent_sessions
            }
        }

    # -------------------------------------------------------------------------
    # CLEANUP
    # -------------------------------------------------------------------------

    @classmethod
    def cleanup(cls):
        """Clean up expired and inactive sessions."""
        cleaned = 0
        with _lock:
            for session_id, session in list(_sessions.items()):
                if session.is_expired():
                    cls._invalidate_session(session_id, reason="cleanup_expired")
                    cleaned += 1
                elif not session.is_active:
                    # Keep inactive sessions for a while for logging
                    if session.last_activity < datetime.utcnow() - timedelta(days=7):
                        del _sessions[session_id]
                        if session.user_id in _user_sessions:
                            _user_sessions[session.user_id] = [
                                sid for sid in _user_sessions[session.user_id]
                                if sid != session_id
                            ]
                        cleaned += 1

        if cleaned > 0:
            logger.info(f"Session cleanup: removed {cleaned} sessions")

        return cleaned


# =============================================================================
# EXCEPTIONS
# =============================================================================

class SessionLimitError(Exception):
    """Raised when concurrent session limit is reached."""
    pass


class SessionExpiredError(Exception):
    """Raised when session has expired."""
    pass


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_session(
    user_id: str,
    token_jti: str,
    tenant_id: str = None,
    ip_address: str = None,
    user_agent: str = None
) -> Session:
    """Create a new session."""
    return SessionManager.create_session(
        user_id=user_id,
        token_jti=token_jti,
        tenant_id=tenant_id,
        ip_address=ip_address,
        user_agent=user_agent
    )


def validate_session(session_id: str, refresh: bool = True) -> tuple:
    """Validate a session."""
    return SessionManager.validate_session(session_id, refresh)


def invalidate_session(session_id: str) -> bool:
    """Invalidate a session."""
    return SessionManager.invalidate_session(session_id)


def logout_all_sessions(user_id: str, except_current: str = None) -> int:
    """Logout from all sessions."""
    return SessionManager.invalidate_all_user_sessions(user_id, except_session=except_current)


def get_user_sessions(user_id: str) -> List[Dict]:
    """Get all sessions for a user."""
    sessions = SessionManager.get_user_sessions(user_id)
    return [s.to_dict() for s in sessions]
