# -*- coding: utf-8 -*-
"""
Cache Manager - Plataforma E
============================

Unified caching layer with multiple backends.

Issue #447: Cache Manager - Camada de Cache Distribuido
"""

import json
import time
import hashlib
import logging
from abc import ABC, abstractmethod
from typing import Optional, Any, Dict, List, Callable
from dataclasses import dataclass, field
from datetime import datetime

logger = logging.getLogger(__name__)


@dataclass
class CacheStats:
    """Cache performance statistics."""
    hits: int = 0
    misses: int = 0
    sets: int = 0
    deletes: int = 0
    errors: int = 0
    
    @property
    def hit_rate(self) -> float:
        total = self.hits + self.misses
        return (self.hits / total * 100) if total > 0 else 0.0
    
    def to_dict(self) -> dict:
        return {
            "hits": self.hits,
            "misses": self.misses,
            "sets": self.sets,
            "deletes": self.deletes,
            "errors": self.errors,
            "hit_rate": round(self.hit_rate, 2)
        }


@dataclass
class CacheEntry:
    """Single cache entry with TTL support."""
    key: str
    value: Any
    created_at: float = field(default_factory=time.time)
    ttl: Optional[int] = None
    tags: List[str] = field(default_factory=list)
    
    @property
    def is_expired(self) -> bool:
        if self.ttl is None:
            return False
        return time.time() > (self.created_at + self.ttl)
    
    @property
    def remaining_ttl(self) -> Optional[int]:
        if self.ttl is None:
            return None
        remaining = int((self.created_at + self.ttl) - time.time())
        return max(0, remaining)


class CacheBackend(ABC):
    """Abstract base class for cache backends."""
    
    @abstractmethod
    def get(self, key: str) -> Optional[Any]:
        pass
    
    @abstractmethod
    def set(self, key: str, value: Any, ttl: Optional[int] = None, tags: List[str] = None) -> bool:
        pass
    
    @abstractmethod
    def delete(self, key: str) -> bool:
        pass
    
    @abstractmethod
    def exists(self, key: str) -> bool:
        pass
    
    @abstractmethod
    def clear(self) -> bool:
        pass
    
    @abstractmethod
    def keys(self, pattern: str = "*") -> List[str]:
        pass


class MemoryBackend(CacheBackend):
    """In-memory cache backend."""
    
    def __init__(self, max_size: int = 10000):
        self._cache: Dict[str, CacheEntry] = {}
        self._max_size = max_size
    
    def get(self, key: str) -> Optional[Any]:
        entry = self._cache.get(key)
        if entry is None:
            return None
        if entry.is_expired:
            del self._cache[key]
            return None
        return entry.value
    
    def set(self, key: str, value: Any, ttl: Optional[int] = None, tags: List[str] = None) -> bool:
        if len(self._cache) >= self._max_size:
            self._evict_oldest()
        
        self._cache[key] = CacheEntry(
            key=key,
            value=value,
            ttl=ttl,
            tags=tags or []
        )
        return True
    
    def delete(self, key: str) -> bool:
        if key in self._cache:
            del self._cache[key]
            return True
        return False
    
    def exists(self, key: str) -> bool:
        entry = self._cache.get(key)
        if entry is None:
            return False
        if entry.is_expired:
            del self._cache[key]
            return False
        return True
    
    def clear(self) -> bool:
        self._cache.clear()
        return True
    
    def keys(self, pattern: str = "*") -> List[str]:
        import fnmatch
        all_keys = list(self._cache.keys())
        if pattern == "*":
            return all_keys
        return [k for k in all_keys if fnmatch.fnmatch(k, pattern)]
    
    def delete_by_tag(self, tag: str) -> int:
        to_delete = [k for k, v in self._cache.items() if tag in v.tags]
        for k in to_delete:
            del self._cache[k]
        return len(to_delete)
    
    def _evict_oldest(self):
        if not self._cache:
            return
        sorted_keys = sorted(self._cache.keys(), key=lambda k: self._cache[k].created_at)
        evict_count = max(1, len(sorted_keys) // 10)
        for key in sorted_keys[:evict_count]:
            del self._cache[key]


class CacheManager:
    """Unified cache manager with automatic fallback."""
    
    DEFAULT_TTLS = {
        "session": 3600,
        "user": 300,
        "config": 600,
        "query": 60,
        "dashboard": 30,
        "default": 300
    }
    
    def __init__(self, backend: str = "auto", **kwargs):
        self._stats = CacheStats()
        self._backend = MemoryBackend(**kwargs)
    
    def get(self, key: str, default: Any = None) -> Any:
        try:
            value = self._backend.get(key)
            if value is not None:
                self._stats.hits += 1
                return value
            self._stats.misses += 1
            return default
        except Exception as e:
            self._stats.errors += 1
            return default
    
    def set(self, key: str, value: Any, ttl: Optional[int] = None,
            category: str = "default", tags: List[str] = None) -> bool:
        try:
            if ttl is None:
                ttl = self.DEFAULT_TTLS.get(category, self.DEFAULT_TTLS["default"])
            success = self._backend.set(key, value, ttl, tags)
            if success:
                self._stats.sets += 1
            return success
        except Exception as e:
            self._stats.errors += 1
            return False
    
    def delete(self, key: str) -> bool:
        try:
            result = self._backend.delete(key)
            if result:
                self._stats.deletes += 1
            return result
        except:
            self._stats.errors += 1
            return False
    
    def exists(self, key: str) -> bool:
        return self._backend.exists(key)
    
    def clear(self, pattern: str = "*") -> bool:
        if pattern == "*":
            return self._backend.clear()
        keys = self._backend.keys(pattern)
        for key in keys:
            self.delete(key)
        return True
    
    def invalidate_tag(self, tag: str) -> int:
        if hasattr(self._backend, "delete_by_tag"):
            return self._backend.delete_by_tag(tag)
        return 0
    
    def get_or_set(self, key: str, factory: Callable[[], Any],
                   ttl: Optional[int] = None, category: str = "default") -> Any:
        value = self.get(key)
        if value is not None:
            return value
        value = factory()
        self.set(key, value, ttl, category)
        return value
    
    def get_stats(self) -> dict:
        return self._stats.to_dict()
    
    def get_info(self) -> dict:
        return {
            "backend": type(self._backend).__name__,
            "stats": self.get_stats(),
            "key_count": len(self._backend.keys())
        }


_cache: Optional[CacheManager] = None


def get_cache() -> CacheManager:
    global _cache
    if _cache is None:
        _cache = CacheManager(backend="auto")
    return _cache


def init_cache(backend: str = "auto", **kwargs) -> CacheManager:
    global _cache
    _cache = CacheManager(backend=backend, **kwargs)
    return _cache
