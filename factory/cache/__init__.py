# -*- coding: utf-8 -*-
"""
Cache Module - Plataforma E
===========================

Distributed caching layer for improved performance.
"""

from .cache_manager import CacheManager, get_cache
from .decorators import cached, cache_invalidate

__all__ = [
    "CacheManager",
    "get_cache",
    "cached",
    "cache_invalidate"
]
