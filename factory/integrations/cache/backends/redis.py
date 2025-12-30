# -*- coding: utf-8 -*-
"""
Redis Cache Backend
===================
Cache distribuido usando Redis.

Issue #365 - Terminal A
"""

import json
import logging
import time
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)

# Tenta importar redis, mas nao falha se nao disponivel
try:
    import redis.asyncio as redis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    logger.warning("redis package not installed - RedisCache unavailable")


class RedisCache:
    """
    Backend de cache usando Redis.

    Features:
    - Distribuido entre multiplas instancias
    - TTL nativo do Redis
    - Pub/Sub para invalidacao
    - Serializa automaticamente para JSON

    Exemplo:
        cache = RedisCache(host="localhost", port=6379)
        await cache.connect()
        await cache.set("user:123", user_data, ttl=3600)
        user = await cache.get("user:123")
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 6379,
        db: int = 0,
        password: Optional[str] = None,
        key_prefix: str = "cache:",
        default_ttl: Optional[int] = None,
        connection_pool_size: int = 10
    ):
        """
        Args:
            host: Host do Redis
            port: Porta do Redis
            db: Database number
            password: Senha (opcional)
            key_prefix: Prefixo para todas as chaves
            default_ttl: TTL padrao em segundos
            connection_pool_size: Tamanho do pool de conexoes
        """
        if not REDIS_AVAILABLE:
            raise RuntimeError("redis package not installed")

        self._host = host
        self._port = port
        self._db = db
        self._password = password
        self._key_prefix = key_prefix
        self._default_ttl = default_ttl
        self._pool_size = connection_pool_size

        self._client: Optional[redis.Redis] = None
        self._connected = False

        # Estatisticas
        self._hits = 0
        self._misses = 0

    async def connect(self):
        """Conecta ao Redis"""
        if self._connected:
            return

        try:
            self._client = redis.Redis(
                host=self._host,
                port=self._port,
                db=self._db,
                password=self._password,
                max_connections=self._pool_size,
                decode_responses=True
            )

            # Testa conexao
            await self._client.ping()
            self._connected = True
            logger.info(f"Connected to Redis at {self._host}:{self._port}")

        except Exception as e:
            logger.error(f"Failed to connect to Redis: {e}")
            raise

    async def disconnect(self):
        """Desconecta do Redis"""
        if self._client:
            await self._client.close()
            self._connected = False
            logger.info("Disconnected from Redis")

    def _make_key(self, key: str) -> str:
        """Adiciona prefixo a chave"""
        return f"{self._key_prefix}{key}"

    def _serialize(self, value: Any) -> str:
        """Serializa valor para armazenamento"""
        return json.dumps(value, default=str)

    def _deserialize(self, data: str) -> Any:
        """Deserializa valor do armazenamento"""
        try:
            return json.loads(data)
        except (json.JSONDecodeError, TypeError):
            return data

    async def get(self, key: str) -> Optional[Any]:
        """
        Obtem valor do cache.

        Args:
            key: Chave do cache

        Returns:
            Valor ou None se nao encontrado
        """
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            data = await self._client.get(full_key)

            if data is None:
                self._misses += 1
                return None

            self._hits += 1
            return self._deserialize(data)

        except Exception as e:
            logger.error(f"Redis get error for {key}: {e}")
            self._misses += 1
            return None

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
            ttl: TTL em segundos
            tags: Tags para agrupamento

        Returns:
            True se armazenado com sucesso
        """
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            data = self._serialize(value)

            effective_ttl = ttl if ttl is not None else self._default_ttl

            if effective_ttl:
                await self._client.setex(full_key, effective_ttl, data)
            else:
                await self._client.set(full_key, data)

            # Registra tags
            if tags:
                for tag in tags:
                    tag_key = f"{self._key_prefix}tag:{tag}"
                    await self._client.sadd(tag_key, key)

            return True

        except Exception as e:
            logger.error(f"Redis set error for {key}: {e}")
            return False

    async def delete(self, key: str) -> bool:
        """Remove entrada do cache"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            result = await self._client.delete(full_key)
            return result > 0

        except Exception as e:
            logger.error(f"Redis delete error for {key}: {e}")
            return False

    async def exists(self, key: str) -> bool:
        """Verifica se chave existe"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            return await self._client.exists(full_key) > 0

        except Exception as e:
            logger.error(f"Redis exists error for {key}: {e}")
            return False

    async def invalidate_by_tag(self, tag: str) -> int:
        """
        Invalida todas as entradas com uma tag.

        Args:
            tag: Tag para invalidar

        Returns:
            Numero de entradas removidas
        """
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            tag_key = f"{self._key_prefix}tag:{tag}"
            keys = await self._client.smembers(tag_key)

            count = 0
            for key in keys:
                full_key = self._make_key(key)
                if await self._client.delete(full_key):
                    count += 1

            # Remove o conjunto de tags
            await self._client.delete(tag_key)

            logger.info(f"Invalidated {count} entries with tag: {tag}")
            return count

        except Exception as e:
            logger.error(f"Redis invalidate_by_tag error: {e}")
            return 0

    async def invalidate_by_pattern(self, pattern: str) -> int:
        """
        Invalida entradas que correspondem a um pattern.

        Args:
            pattern: Pattern (usa SCAN do Redis)

        Returns:
            Numero de entradas removidas
        """
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_pattern = self._make_key(pattern)
            count = 0

            async for key in self._client.scan_iter(match=full_pattern):
                await self._client.delete(key)
                count += 1

            logger.info(f"Invalidated {count} entries matching: {pattern}")
            return count

        except Exception as e:
            logger.error(f"Redis invalidate_by_pattern error: {e}")
            return 0

    async def clear(self):
        """Limpa todas as chaves com o prefixo"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            pattern = f"{self._key_prefix}*"
            count = 0

            async for key in self._client.scan_iter(match=pattern):
                await self._client.delete(key)
                count += 1

            logger.info(f"Cleared {count} cache entries")

        except Exception as e:
            logger.error(f"Redis clear error: {e}")

    async def get_many(self, keys: List[str]) -> Dict[str, Any]:
        """Obtem multiplas chaves de uma vez"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_keys = [self._make_key(k) for k in keys]
            values = await self._client.mget(full_keys)

            result = {}
            for key, value in zip(keys, values):
                if value is not None:
                    result[key] = self._deserialize(value)
                    self._hits += 1
                else:
                    self._misses += 1

            return result

        except Exception as e:
            logger.error(f"Redis get_many error: {e}")
            return {}

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

    async def get_ttl(self, key: str) -> Optional[int]:
        """Obtem TTL restante de uma chave"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            ttl = await self._client.ttl(full_key)
            return ttl if ttl > 0 else None

        except Exception as e:
            logger.error(f"Redis get_ttl error for {key}: {e}")
            return None

    async def extend_ttl(self, key: str, additional_ttl: int) -> bool:
        """Estende TTL de uma chave"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_key = self._make_key(key)
            current_ttl = await self._client.ttl(full_key)

            if current_ttl > 0:
                new_ttl = current_ttl + additional_ttl
                return await self._client.expire(full_key, new_ttl)

            return False

        except Exception as e:
            logger.error(f"Redis extend_ttl error for {key}: {e}")
            return False

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do cache"""
        total_requests = self._hits + self._misses
        hit_rate = (self._hits / total_requests * 100) if total_requests > 0 else 0

        return {
            "connected": self._connected,
            "host": self._host,
            "port": self._port,
            "hits": self._hits,
            "misses": self._misses,
            "hit_rate": round(hit_rate, 2)
        }

    async def get_keys(self, pattern: Optional[str] = None) -> List[str]:
        """Lista chaves no cache"""
        if not self._connected:
            raise RuntimeError("Not connected to Redis")

        try:
            full_pattern = self._make_key(pattern or "*")
            keys = []

            async for key in self._client.scan_iter(match=full_pattern):
                # Remove prefixo
                short_key = key.replace(self._key_prefix, "", 1)
                keys.append(short_key)

            return keys

        except Exception as e:
            logger.error(f"Redis get_keys error: {e}")
            return []

    async def health_check(self) -> bool:
        """Verifica saude da conexao"""
        try:
            if not self._connected:
                return False
            await self._client.ping()
            return True
        except Exception:
            return False
