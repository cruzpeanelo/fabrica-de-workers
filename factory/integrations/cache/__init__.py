# -*- coding: utf-8 -*-
"""
Cache Module
============
Sistema de cache para integracoes.

Issue #365 - Terminal A

Features:
- Cache em memoria (desenvolvimento)
- Cache Redis (producao)
- TTL configuravel por tipo de dado
- Decoradores para caching automatico
- Invalidacao baseada em eventos

Exemplo:
    from factory.integrations.cache import (
        get_cache_manager,
        init_cache,
        CacheConfig,
        CacheBackend,
        CacheType,
        cached
    )

    # Inicializar cache
    config = CacheConfig(backend=CacheBackend.MEMORY)
    await init_cache(config)

    # Usar cache manager
    manager = get_cache_manager()
    await manager.cache_user("tenant-123", "user-456", user_data)
    user = await manager.get_user("tenant-123", "user-456")

    # Usar decorator
    @cached(ttl=300, key_prefix="jira:issue")
    async def get_jira_issue(issue_key: str):
        ...

    # Invalidar cache
    from factory.integrations.cache import trigger_invalidation, InvalidationEvent
    await trigger_invalidation(InvalidationEvent.STORY_UPDATED, story_id="STR-001")
"""

# Cache Manager
from .cache_manager import (
    CacheManager,
    CacheConfig,
    CacheBackend,
    CacheType,
    DEFAULT_TTLS,
    get_cache_manager,
    init_cache,
    reset_cache_manager
)

# Backends
from .backends import (
    MemoryCache,
    CacheEntry,
    RedisCache,
    REDIS_AVAILABLE
)

# Decorators
from .decorators import (
    cached,
    cached_property,
    cache_aside,
    write_through,
    invalidate_cache
)

# Invalidation
from .invalidation import (
    CacheInvalidator,
    InvalidationEvent,
    InvalidationRule,
    InvalidationResult,
    DEFAULT_INVALIDATION_RULES,
    get_invalidator,
    reset_invalidator,
    trigger_invalidation
)

__all__ = [
    # Cache Manager
    "CacheManager",
    "CacheConfig",
    "CacheBackend",
    "CacheType",
    "DEFAULT_TTLS",
    "get_cache_manager",
    "init_cache",
    "reset_cache_manager",

    # Backends
    "MemoryCache",
    "CacheEntry",
    "RedisCache",
    "REDIS_AVAILABLE",

    # Decorators
    "cached",
    "cached_property",
    "cache_aside",
    "write_through",
    "invalidate_cache",

    # Invalidation
    "CacheInvalidator",
    "InvalidationEvent",
    "InvalidationRule",
    "InvalidationResult",
    "DEFAULT_INVALIDATION_RULES",
    "get_invalidator",
    "reset_invalidator",
    "trigger_invalidation"
]
