# -*- coding: utf-8 -*-
"""
In-Memory Cache Backend
=======================
Cache em memoria para desenvolvimento e testes.

Issue #365 - Terminal A
"""

import asyncio
import logging
import time
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set
from collections import OrderedDict

logger = logging.getLogger(__name__)


@dataclass
class CacheEntry:
    """Entrada de cache com metadata"""
    key: str
    value: Any
    created_at: float = field(default_factory=time.time)
    expires_at: Optional[float] = None
    hits: int = 0
    tags: Set[str] = field(default_factory=set)

    @property
    def is_expired(self) -> bool:
        if self.expires_at is None:
            return False
        return time.time() > self.expires_at

    @property
    def ttl_remaining(self) -> Optional[float]:
        if self.expires_at is None:
            return None
        remaining = self.expires_at - time.time()
        return max(0, remaining)


class MemoryCache:
    """
    Backend de cache em memoria.

    Features:
    - TTL por entrada
    - LRU eviction quando atinge max_size
    - Tags para invalidacao em grupo
    - Estatisticas de uso

    Exemplo:
        cache = MemoryCache(max_size=1000)
        await cache.set("user:123", user_data, ttl=3600)
        user = await cache.get("user:123")
    """

    def __init__(
        self,
        max_size: int = 10000,
        default_ttl: Optional[int] = None,
        cleanup_interval: int = 60
    ):
        """
        Args:
            max_size: Numero maximo de entradas
            default_ttl: TTL padrao em segundos
            cleanup_interval: Intervalo de limpeza em segundos
        """
        self._cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self._tags: Dict[str, Set[str]] = {}  # tag -> set of keys
        self._max_size = max_size
        self._default_ttl = default_ttl
        self._cleanup_interval = cleanup_interval
        self._lock = asyncio.Lock()
        self._cleanup_task: Optional[asyncio.Task] = None

        # Estatisticas
        self._hits = 0
        self._misses = 0
        self._evictions = 0

    async def start(self):
        """Inicia task de limpeza periodica"""
        if self._cleanup_task is None:
            self._cleanup_task = asyncio.create_task(self._cleanup_loop())
            logger.info("MemoryCache cleanup task started")

    async def stop(self):
        """Para task de limpeza"""
        if self._cleanup_task:
            self._cleanup_task.cancel()
            try:
                await self._cleanup_task
            except asyncio.CancelledError:
                pass
            self._cleanup_task = None
            logger.info("MemoryCache cleanup task stopped")

    async def get(self, key: str) -> Optional[Any]:
        """
        Obtem valor do cache.

        Args:
            key: Chave do cache

        Returns:
            Valor ou None se nao encontrado/expirado
        """
        async with self._lock:
            entry = self._cache.get(key)

            if entry is None:
                self._misses += 1
                return None

            if entry.is_expired:
                self._misses += 1
                await self._delete_entry(key)
                return None

            # Move para o final (LRU)
            self._cache.move_to_end(key)
            entry.hits += 1
            self._hits += 1

            return entry.value

    async def set(
        self,
        key: str,
        value: Any,
        ttl: Optional[int] = None,
        tags: Optional[List[str]] = None
    ) -> bool:
        """
        Define valor no cache.

        Args:
            key: Chave do cache
            value: Valor a armazenar
            ttl: TTL em segundos (None = sem expiracao)
            tags: Tags para agrupamento

        Returns:
            True se armazenado com sucesso
        """
        async with self._lock:
            # Calcula expiracao
            expires_at = None
            effective_ttl = ttl if ttl is not None else self._default_ttl
            if effective_ttl is not None:
                expires_at = time.time() + effective_ttl

            # Remove entrada existente se houver
            if key in self._cache:
                await self._delete_entry(key)

            # Evict se necessario
            while len(self._cache) >= self._max_size:
                await self._evict_oldest()

            # Cria entrada
            entry = CacheEntry(
                key=key,
                value=value,
                expires_at=expires_at,
                tags=set(tags) if tags else set()
            )

            self._cache[key] = entry

            # Registra tags
            for tag in entry.tags:
                if tag not in self._tags:
                    self._tags[tag] = set()
                self._tags[tag].add(key)

            return True

    async def delete(self, key: str) -> bool:
        """Remove entrada do cache"""
        async with self._lock:
            return await self._delete_entry(key)

    async def _delete_entry(self, key: str) -> bool:
        """Remove entrada (sem lock)"""
        entry = self._cache.pop(key, None)
        if entry is None:
            return False

        # Remove das tags
        for tag in entry.tags:
            if tag in self._tags:
                self._tags[tag].discard(key)
                if not self._tags[tag]:
                    del self._tags[tag]

        return True

    async def _evict_oldest(self):
        """Remove entrada mais antiga (LRU)"""
        if self._cache:
            key, _ = self._cache.popitem(last=False)
            self._evictions += 1
            logger.debug(f"Cache eviction: {key}")

    async def exists(self, key: str) -> bool:
        """Verifica se chave existe e nao expirou"""
        value = await self.get(key)
        return value is not None

    async def invalidate_by_tag(self, tag: str) -> int:
        """
        Invalida todas as entradas com uma tag.

        Args:
            tag: Tag para invalidar

        Returns:
            Numero de entradas removidas
        """
        async with self._lock:
            keys = self._tags.get(tag, set()).copy()
            count = 0
            for key in keys:
                if await self._delete_entry(key):
                    count += 1
            logger.info(f"Invalidated {count} entries with tag: {tag}")
            return count

    async def invalidate_by_pattern(self, pattern: str) -> int:
        """
        Invalida entradas que correspondem a um pattern.

        Args:
            pattern: Pattern simples (suporta * no final)

        Returns:
            Numero de entradas removidas
        """
        async with self._lock:
            if pattern.endswith("*"):
                prefix = pattern[:-1]
                keys = [k for k in self._cache.keys() if k.startswith(prefix)]
            else:
                keys = [pattern] if pattern in self._cache else []

            count = 0
            for key in keys:
                if await self._delete_entry(key):
                    count += 1

            logger.info(f"Invalidated {count} entries matching: {pattern}")
            return count

    async def clear(self):
        """Limpa todo o cache"""
        async with self._lock:
            self._cache.clear()
            self._tags.clear()
            logger.info("Cache cleared")

    async def get_many(self, keys: List[str]) -> Dict[str, Any]:
        """Obtem multiplas chaves de uma vez"""
        result = {}
        for key in keys:
            value = await self.get(key)
            if value is not None:
                result[key] = value
        return result

    async def set_many(
        self,
        items: Dict[str, Any],
        ttl: Optional[int] = None,
        tags: Optional[List[str]] = None
    ) -> int:
        """Define multiplas chaves de uma vez"""
        count = 0
        for key, value in items.items():
            if await self.set(key, value, ttl=ttl, tags=tags):
                count += 1
        return count

    async def _cleanup_loop(self):
        """Loop de limpeza de entradas expiradas"""
        while True:
            try:
                await asyncio.sleep(self._cleanup_interval)
                await self._cleanup_expired()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Cleanup error: {e}")

    async def _cleanup_expired(self):
        """Remove entradas expiradas"""
        async with self._lock:
            now = time.time()
            expired_keys = [
                key for key, entry in self._cache.items()
                if entry.expires_at and entry.expires_at <= now
            ]

            for key in expired_keys:
                await self._delete_entry(key)

            if expired_keys:
                logger.debug(f"Cleaned up {len(expired_keys)} expired entries")

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do cache"""
        total_requests = self._hits + self._misses
        hit_rate = (self._hits / total_requests * 100) if total_requests > 0 else 0

        return {
            "size": len(self._cache),
            "max_size": self._max_size,
            "hits": self._hits,
            "misses": self._misses,
            "hit_rate": round(hit_rate, 2),
            "evictions": self._evictions,
            "tags_count": len(self._tags)
        }

    def get_keys(self, pattern: Optional[str] = None) -> List[str]:
        """Lista chaves no cache"""
        keys = list(self._cache.keys())
        if pattern and pattern.endswith("*"):
            prefix = pattern[:-1]
            keys = [k for k in keys if k.startswith(prefix)]
        return keys
