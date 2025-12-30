# -*- coding: utf-8 -*-
"""
Redis Session Store - Issue #206
================================
Distributed session storage using Redis for stateless deployments.

Usage:
    from factory.sessions import get_session_store

    sessions = get_session_store()
    sessions.create("session_id", {"user_id": "123"})
    data = sessions.get("session_id")
"""

import os
import json
import logging
import uuid
from typing import Optional, Dict, Any
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)

# Redis import (graceful fallback)
try:
    import redis
    HAS_REDIS = True
except ImportError:
    HAS_REDIS = False
    redis = None
    logger.info("[Sessions] Redis not installed. Run: pip install redis")


class RedisSessionStore:
    """
    Redis-based session storage.

    Provides secure session management with TTL support.
    Falls back to in-memory dict if Redis is not available.
    """

    DEFAULT_TTL = 3600  # 1 hour
    SESSION_PREFIX = "session"

    def __init__(self, redis_url: str = None, default_ttl: int = None):
        """
        Initialize session store.

        Args:
            redis_url: Redis connection URL
            default_ttl: Default session TTL in seconds
        """
        self.default_ttl = default_ttl or self.DEFAULT_TTL
        self._local_store = {}  # Fallback store
        self._redis = None

        if HAS_REDIS:
            redis_url = redis_url or os.getenv("REDIS_URL", "redis://localhost:6379")
            try:
                self._redis = redis.from_url(
                    redis_url,
                    decode_responses=True,
                    socket_connect_timeout=5,
                    socket_timeout=5
                )
                self._redis.ping()
                logger.info(f"[Sessions] Connected to Redis")
            except Exception as e:
                logger.warning(f"[Sessions] Redis connection failed: {e}. Using in-memory store.")
                self._redis = None
        else:
            logger.warning("[Sessions] Redis not available. Using in-memory store.")

    def _make_key(self, session_id: str) -> str:
        """Create session key."""
        return f"{self.SESSION_PREFIX}:{session_id}"

    def create(
        self,
        session_id: str = None,
        data: Dict[str, Any] = None,
        ttl: int = None
    ) -> str:
        """
        Create a new session.

        Args:
            session_id: Optional session ID (generated if not provided)
            data: Initial session data
            ttl: Session TTL in seconds

        Returns:
            Session ID
        """
        session_id = session_id or str(uuid.uuid4())
        ttl = ttl or self.default_ttl

        session_data = {
            "id": session_id,
            "created_at": datetime.utcnow().isoformat(),
            "last_accessed": datetime.utcnow().isoformat(),
            "data": data or {}
        }

        full_key = self._make_key(session_id)

        try:
            serialized = json.dumps(session_data)

            if self._redis:
                self._redis.setex(full_key, ttl, serialized)
            else:
                self._local_store[full_key] = serialized

            logger.debug(f"[Sessions] Created session: {session_id}")
            return session_id
        except Exception as e:
            logger.error(f"[Sessions] Failed to create session: {e}")
            raise

    def get(self, session_id: str) -> Optional[Dict[str, Any]]:
        """
        Get session data.

        Args:
            session_id: Session ID

        Returns:
            Session data or None if not found
        """
        full_key = self._make_key(session_id)

        try:
            if self._redis:
                value = self._redis.get(full_key)
            else:
                value = self._local_store.get(full_key)

            if value is None:
                return None

            session_data = json.loads(value)

            # Update last accessed
            session_data["last_accessed"] = datetime.utcnow().isoformat()
            self._update_raw(full_key, session_data)

            return session_data.get("data", {})
        except Exception as e:
            logger.error(f"[Sessions] Failed to get session {session_id}: {e}")
            return None

    def set(self, session_id: str, data: Dict[str, Any], ttl: int = None) -> bool:
        """
        Update session data.

        Args:
            session_id: Session ID
            data: New session data
            ttl: Optional new TTL

        Returns:
            True if successful
        """
        full_key = self._make_key(session_id)
        ttl = ttl or self.default_ttl

        try:
            # Get existing session
            if self._redis:
                existing = self._redis.get(full_key)
            else:
                existing = self._local_store.get(full_key)

            if existing:
                session_data = json.loads(existing)
                session_data["data"] = data
                session_data["last_accessed"] = datetime.utcnow().isoformat()
            else:
                session_data = {
                    "id": session_id,
                    "created_at": datetime.utcnow().isoformat(),
                    "last_accessed": datetime.utcnow().isoformat(),
                    "data": data
                }

            serialized = json.dumps(session_data)

            if self._redis:
                self._redis.setex(full_key, ttl, serialized)
            else:
                self._local_store[full_key] = serialized

            return True
        except Exception as e:
            logger.error(f"[Sessions] Failed to set session {session_id}: {e}")
            return False

    def _update_raw(self, full_key: str, session_data: Dict) -> None:
        """Update raw session data without changing TTL."""
        try:
            serialized = json.dumps(session_data)
            if self._redis:
                ttl = self._redis.ttl(full_key)
                if ttl > 0:
                    self._redis.setex(full_key, ttl, serialized)
            else:
                self._local_store[full_key] = serialized
        except Exception:
            pass

    def delete(self, session_id: str) -> bool:
        """
        Delete a session.

        Args:
            session_id: Session ID

        Returns:
            True if deleted
        """
        full_key = self._make_key(session_id)

        try:
            if self._redis:
                self._redis.delete(full_key)
            else:
                self._local_store.pop(full_key, None)

            logger.debug(f"[Sessions] Deleted session: {session_id}")
            return True
        except Exception as e:
            logger.error(f"[Sessions] Failed to delete session {session_id}: {e}")
            return False

    def extend(self, session_id: str, ttl: int = None) -> bool:
        """
        Extend session TTL.

        Args:
            session_id: Session ID
            ttl: New TTL in seconds

        Returns:
            True if extended
        """
        full_key = self._make_key(session_id)
        ttl = ttl or self.default_ttl

        try:
            if self._redis:
                return bool(self._redis.expire(full_key, ttl))
            return full_key in self._local_store
        except Exception as e:
            logger.error(f"[Sessions] Failed to extend session {session_id}: {e}")
            return False

    def exists(self, session_id: str) -> bool:
        """Check if session exists."""
        full_key = self._make_key(session_id)

        try:
            if self._redis:
                return bool(self._redis.exists(full_key))
            return full_key in self._local_store
        except Exception:
            return False

    @property
    def is_connected(self) -> bool:
        """Check if connected to Redis."""
        if self._redis:
            try:
                self._redis.ping()
                return True
            except Exception:
                return False
        return False


# Global session store instance
_session_store: Optional[RedisSessionStore] = None


def get_session_store() -> RedisSessionStore:
    """Get the global session store instance."""
    global _session_store
    if _session_store is None:
        _session_store = RedisSessionStore()
    return _session_store
