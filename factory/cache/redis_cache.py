# -*- coding: utf-8 -*-
"""
Redis Cache - Issue #206
========================
Distributed cache using Redis for stateless deployments.

Usage:
    from factory.cache import get_cache

    cache = get_cache()
    cache.set("key", {"data": "value"}, ttl=3600)
    data = cache.get("key")
"""

import os
import json
import logging
from typing import Optional, Any, Union
from datetime import timedelta

logger = logging.getLogger(__name__)

# Redis import (graceful fallback)
try:
    import redis
    HAS_REDIS = True
except ImportError:
    HAS_REDIS = False
    redis = None
    logger.info("[Cache] Redis not installed. Run: pip install redis")


class RedisCache:
    """
    Redis-based distributed cache.

    Provides a simple interface for caching with TTL support.
    Falls back to in-memory dict if Redis is not available.
    """

    def __init__(self, redis_url: str = None, prefix: str = "factory"):
        """
        Initialize Redis cache.

        Args:
            redis_url: Redis connection URL
            prefix: Key prefix for namespacing
        """
        self.prefix = prefix
        self._local_cache = {}  # Fallback cache
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
                # Test connection
                self._redis.ping()
                logger.info(f"[Cache] Connected to Redis: {redis_url.split('@')[-1]}")
            except Exception as e:
                logger.warning(f"[Cache] Redis connection failed: {e}. Using in-memory cache.")
                self._redis = None
        else:
            logger.warning("[Cache] Redis not available. Using in-memory cache.")

    def _make_key(self, key: str) -> str:
        """Create prefixed key."""
        return f"{self.prefix}:{key}"

    def set(
        self,
        key: str,
        value: Any,
        ttl: Union[int, timedelta] = 3600
    ) -> bool:
        """
        Set a value in cache.

        Args:
            key: Cache key
            value: Value to cache (will be JSON serialized)
            ttl: Time to live in seconds or timedelta

        Returns:
            True if successful
        """
        full_key = self._make_key(key)

        if isinstance(ttl, timedelta):
            ttl = int(ttl.total_seconds())

        try:
            serialized = json.dumps(value)

            if self._redis:
                self._redis.setex(full_key, ttl, serialized)
            else:
                self._local_cache[full_key] = serialized

            return True
        except Exception as e:
            logger.error(f"[Cache] Failed to set {key}: {e}")
            return False

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a value from cache.

        Args:
            key: Cache key
            default: Default value if not found

        Returns:
            Cached value or default
        """
        full_key = self._make_key(key)

        try:
            if self._redis:
                value = self._redis.get(full_key)
            else:
                value = self._local_cache.get(full_key)

            if value is None:
                return default

            return json.loads(value)
        except Exception as e:
            logger.error(f"[Cache] Failed to get {key}: {e}")
            return default

    def delete(self, key: str) -> bool:
        """
        Delete a value from cache.

        Args:
            key: Cache key

        Returns:
            True if deleted
        """
        full_key = self._make_key(key)

        try:
            if self._redis:
                self._redis.delete(full_key)
            else:
                self._local_cache.pop(full_key, None)
            return True
        except Exception as e:
            logger.error(f"[Cache] Failed to delete {key}: {e}")
            return False

    def exists(self, key: str) -> bool:
        """Check if key exists in cache."""
        full_key = self._make_key(key)

        try:
            if self._redis:
                return bool(self._redis.exists(full_key))
            return full_key in self._local_cache
        except Exception:
            return False

    def clear_prefix(self, prefix: str) -> int:
        """
        Clear all keys with a given prefix.

        Args:
            prefix: Key prefix to clear

        Returns:
            Number of keys deleted
        """
        pattern = self._make_key(f"{prefix}:*")

        try:
            if self._redis:
                keys = self._redis.keys(pattern)
                if keys:
                    return self._redis.delete(*keys)
            else:
                to_delete = [k for k in self._local_cache if k.startswith(pattern.replace("*", ""))]
                for k in to_delete:
                    del self._local_cache[k]
                return len(to_delete)
            return 0
        except Exception as e:
            logger.error(f"[Cache] Failed to clear prefix {prefix}: {e}")
            return 0

    def get_or_set(
        self,
        key: str,
        factory: callable,
        ttl: Union[int, timedelta] = 3600
    ) -> Any:
        """
        Get value from cache or compute and store it.

        Args:
            key: Cache key
            factory: Function to compute value if not cached
            ttl: Time to live

        Returns:
            Cached or computed value
        """
        value = self.get(key)
        if value is not None:
            return value

        value = factory()
        self.set(key, value, ttl)
        return value

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


# Global cache instance
_cache_instance: Optional[RedisCache] = None


def get_cache() -> RedisCache:
    """Get the global cache instance."""
    global _cache_instance
    if _cache_instance is None:
        _cache_instance = RedisCache()
    return _cache_instance
