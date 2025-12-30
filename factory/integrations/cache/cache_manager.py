# -*- coding: utf-8 -*-
"""
Cache Manager
=============
Gerenciador central de cache para integracoes.

Issue #365 - Terminal A
"""

import logging
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional, Protocol, Union

from .backends.memory import MemoryCache
from .backends.redis import RedisCache, REDIS_AVAILABLE

logger = logging.getLogger(__name__)


class CacheBackend(str, Enum):
    """Backends de cache disponiveis"""
    MEMORY = "memory"
    REDIS = "redis"


class CacheType(str, Enum):
    """Tipos de cache com TTL pre-definidos"""
    USER = "user"              # TTL: 1 hora
    PROJECT = "project"        # TTL: 15 minutos
    ISSUE = "issue"            # TTL: 5 minutos
    METADATA = "metadata"      # TTL: 24 horas
    SESSION = "session"        # TTL: 30 minutos
    RATE_LIMIT = "rate_limit"  # TTL: 1 minuto
    CUSTOM = "custom"          # TTL configuravel


# TTLs padrao por tipo (em segundos)
DEFAULT_TTLS: Dict[CacheType, int] = {
    CacheType.USER: 3600,       # 1 hora
    CacheType.PROJECT: 900,     # 15 minutos
    CacheType.ISSUE: 300,       # 5 minutos
    CacheType.METADATA: 86400,  # 24 horas
    CacheType.SESSION: 1800,    # 30 minutos
    CacheType.RATE_LIMIT: 60,   # 1 minuto
    CacheType.CUSTOM: 300,      # 5 minutos (padrao)
}


class CacheProtocol(Protocol):
    """Protocolo que todos os backends devem implementar"""

    async def get(self, key: str) -> Optional[Any]: ...
    async def set(
        self,
        key: str,
        value: Any,
        ttl: Optional[int] = None,
        tags: Optional[List[str]] = None
    ) -> bool: ...
    async def delete(self, key: str) -> bool: ...
    async def exists(self, key: str) -> bool: ...
    async def invalidate_by_tag(self, tag: str) -> int: ...
    async def invalidate_by_pattern(self, pattern: str) -> int: ...
    async def clear(self) -> None: ...
    async def get_many(self, keys: List[str]) -> Dict[str, Any]: ...
    def get_stats(self) -> Dict[str, Any]: ...


@dataclass
class CacheConfig:
    """Configuracao do cache"""
    backend: CacheBackend = CacheBackend.MEMORY

    # Memory backend
    memory_max_size: int = 10000
    memory_cleanup_interval: int = 60

    # Redis backend
    redis_host: str = "localhost"
    redis_port: int = 6379
    redis_db: int = 0
    redis_password: Optional[str] = None
    redis_key_prefix: str = "factory:cache:"

    # Geral
    default_ttl: Optional[int] = None
    enable_stats: bool = True


class CacheManager:
    """
    Gerenciador central de cache.

    Features:
    - Suporta multiplos backends (memory, redis)
    - TTL por tipo de dado
    - Namespacing por tenant
    - Cache warming
    - Estatisticas

    Exemplo:
        config = CacheConfig(backend=CacheBackend.MEMORY)
        manager = CacheManager(config)
        await manager.start()

        # Cache de usuario
        await manager.cache_user("tenant-123", "user-456", user_data)
        user = await manager.get_user("tenant-123", "user-456")

        # Cache generico
        await manager.set("my:key", value, cache_type=CacheType.PROJECT)
    """

    def __init__(self, config: Optional[CacheConfig] = None):
        self._config = config or CacheConfig()
        self._backend: Optional[Union[MemoryCache, RedisCache]] = None
        self._started = False

    async def start(self):
        """Inicializa o cache backend"""
        if self._started:
            return

        if self._config.backend == CacheBackend.MEMORY:
            self._backend = MemoryCache(
                max_size=self._config.memory_max_size,
                default_ttl=self._config.default_ttl,
                cleanup_interval=self._config.memory_cleanup_interval
            )
            await self._backend.start()

        elif self._config.backend == CacheBackend.REDIS:
            if not REDIS_AVAILABLE:
                logger.warning("Redis not available, falling back to memory cache")
                self._backend = MemoryCache(
                    max_size=self._config.memory_max_size,
                    default_ttl=self._config.default_ttl
                )
                await self._backend.start()
            else:
                self._backend = RedisCache(
                    host=self._config.redis_host,
                    port=self._config.redis_port,
                    db=self._config.redis_db,
                    password=self._config.redis_password,
                    key_prefix=self._config.redis_key_prefix,
                    default_ttl=self._config.default_ttl
                )
                await self._backend.connect()

        self._started = True
        logger.info(f"CacheManager started with {self._config.backend.value} backend")

    async def stop(self):
        """Para o cache backend"""
        if not self._started:
            return

        if isinstance(self._backend, MemoryCache):
            await self._backend.stop()
        elif isinstance(self._backend, RedisCache):
            await self._backend.disconnect()

        self._started = False
        logger.info("CacheManager stopped")

    def _make_key(
        self,
        key: str,
        cache_type: CacheType = CacheType.CUSTOM,
        tenant_id: Optional[str] = None
    ) -> str:
        """Constroi chave completa com namespace"""
        parts = []
        if tenant_id:
            parts.append(f"t:{tenant_id}")
        parts.append(cache_type.value)
        parts.append(key)
        return ":".join(parts)

    def _get_ttl(self, cache_type: CacheType, custom_ttl: Optional[int] = None) -> int:
        """Obtem TTL para tipo de cache"""
        if custom_ttl is not None:
            return custom_ttl
        return DEFAULT_TTLS.get(cache_type, 300)

    # === Metodos Genericos ===

    async def get(
        self,
        key: str,
        cache_type: CacheType = CacheType.CUSTOM,
        tenant_id: Optional[str] = None
    ) -> Optional[Any]:
        """Obtem valor do cache"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        full_key = self._make_key(key, cache_type, tenant_id)
        return await self._backend.get(full_key)

    async def set(
        self,
        key: str,
        value: Any,
        cache_type: CacheType = CacheType.CUSTOM,
        tenant_id: Optional[str] = None,
        ttl: Optional[int] = None,
        tags: Optional[List[str]] = None
    ) -> bool:
        """Define valor no cache"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        full_key = self._make_key(key, cache_type, tenant_id)
        effective_ttl = self._get_ttl(cache_type, ttl)

        # Adiciona tags automaticas
        all_tags = list(tags) if tags else []
        all_tags.append(cache_type.value)
        if tenant_id:
            all_tags.append(f"tenant:{tenant_id}")

        return await self._backend.set(full_key, value, ttl=effective_ttl, tags=all_tags)

    async def delete(
        self,
        key: str,
        cache_type: CacheType = CacheType.CUSTOM,
        tenant_id: Optional[str] = None
    ) -> bool:
        """Remove entrada do cache"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        full_key = self._make_key(key, cache_type, tenant_id)
        return await self._backend.delete(full_key)

    async def exists(
        self,
        key: str,
        cache_type: CacheType = CacheType.CUSTOM,
        tenant_id: Optional[str] = None
    ) -> bool:
        """Verifica se chave existe"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        full_key = self._make_key(key, cache_type, tenant_id)
        return await self._backend.exists(full_key)

    # === Metodos por Tipo ===

    async def cache_user(
        self,
        tenant_id: str,
        user_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """Cache de dados de usuario"""
        return await self.set(
            user_id,
            data,
            cache_type=CacheType.USER,
            tenant_id=tenant_id,
            tags=["user"]
        )

    async def get_user(
        self,
        tenant_id: str,
        user_id: str
    ) -> Optional[Dict[str, Any]]:
        """Obtem dados de usuario do cache"""
        return await self.get(user_id, CacheType.USER, tenant_id)

    async def cache_project(
        self,
        tenant_id: str,
        project_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """Cache de dados de projeto"""
        return await self.set(
            project_id,
            data,
            cache_type=CacheType.PROJECT,
            tenant_id=tenant_id,
            tags=["project"]
        )

    async def get_project(
        self,
        tenant_id: str,
        project_id: str
    ) -> Optional[Dict[str, Any]]:
        """Obtem dados de projeto do cache"""
        return await self.get(project_id, CacheType.PROJECT, tenant_id)

    async def cache_issue(
        self,
        tenant_id: str,
        issue_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """Cache de dados de issue"""
        return await self.set(
            issue_id,
            data,
            cache_type=CacheType.ISSUE,
            tenant_id=tenant_id,
            tags=["issue"]
        )

    async def get_issue(
        self,
        tenant_id: str,
        issue_id: str
    ) -> Optional[Dict[str, Any]]:
        """Obtem dados de issue do cache"""
        return await self.get(issue_id, CacheType.ISSUE, tenant_id)

    async def cache_metadata(
        self,
        tenant_id: str,
        key: str,
        data: Any
    ) -> bool:
        """Cache de metadata (schemas, campos, etc)"""
        return await self.set(
            key,
            data,
            cache_type=CacheType.METADATA,
            tenant_id=tenant_id,
            tags=["metadata"]
        )

    async def get_metadata(
        self,
        tenant_id: str,
        key: str
    ) -> Optional[Any]:
        """Obtem metadata do cache"""
        return await self.get(key, CacheType.METADATA, tenant_id)

    # === Invalidacao ===

    async def invalidate_tenant(self, tenant_id: str) -> int:
        """Invalida todo o cache de um tenant"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        return await self._backend.invalidate_by_tag(f"tenant:{tenant_id}")

    async def invalidate_type(self, cache_type: CacheType) -> int:
        """Invalida todo o cache de um tipo"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        return await self._backend.invalidate_by_tag(cache_type.value)

    async def invalidate_by_tag(self, tag: str) -> int:
        """Invalida por tag customizada"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        return await self._backend.invalidate_by_tag(tag)

    async def clear(self):
        """Limpa todo o cache"""
        if not self._started:
            raise RuntimeError("CacheManager not started")

        await self._backend.clear()

    # === Cache Warming ===

    async def warm_cache(
        self,
        items: List[Dict[str, Any]],
        cache_type: CacheType,
        tenant_id: str,
        key_field: str = "id"
    ) -> int:
        """
        Pre-carrega itens no cache (cache warming).

        Args:
            items: Lista de itens a cachear
            cache_type: Tipo de cache
            tenant_id: ID do tenant
            key_field: Campo a usar como chave

        Returns:
            Numero de itens cacheados
        """
        count = 0
        for item in items:
            key = item.get(key_field)
            if key:
                if await self.set(str(key), item, cache_type, tenant_id):
                    count += 1
        logger.info(f"Cache warmed with {count} items for {cache_type.value}")
        return count

    # === Estatisticas ===

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do cache"""
        if not self._started or not self._backend:
            return {"status": "not_started"}

        stats = self._backend.get_stats()
        stats["backend"] = self._config.backend.value
        return stats


# Singleton global
_cache_manager: Optional[CacheManager] = None


def get_cache_manager(config: Optional[CacheConfig] = None) -> CacheManager:
    """
    Obtem instancia global do cache manager.

    Args:
        config: Configuracao (apenas na primeira chamada)

    Returns:
        Instancia do CacheManager
    """
    global _cache_manager
    if _cache_manager is None:
        _cache_manager = CacheManager(config=config)
    return _cache_manager


async def init_cache(config: Optional[CacheConfig] = None) -> CacheManager:
    """Inicializa e retorna o cache manager"""
    manager = get_cache_manager(config)
    await manager.start()
    return manager


def reset_cache_manager():
    """Reseta cache manager global (para testes)"""
    global _cache_manager
    _cache_manager = None
