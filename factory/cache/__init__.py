# -*- coding: utf-8 -*-
"""
Cache Module - Issue #206
=========================
Redis-based cache for stateless deployments.
"""

from .redis_cache import RedisCache, get_cache

__all__ = ["RedisCache", "get_cache"]
