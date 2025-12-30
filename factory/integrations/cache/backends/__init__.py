# -*- coding: utf-8 -*-
"""
Cache Backends
==============
Backends de cache disponíveis.

Issue #365 - Terminal A
"""

from .memory import MemoryCache, CacheEntry
from .redis import RedisCache, REDIS_AVAILABLE

__all__ = [
    "MemoryCache",
    "CacheEntry",
    "RedisCache",
    "REDIS_AVAILABLE"
]
