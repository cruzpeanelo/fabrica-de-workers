"""
Rate Limiting Module v4.1 - Redis-based rate limiting with in-memory fallback
Fabrica de Agentes - Nova Arquitetura MVP

Issue #141: Implementa fallback in-memory quando Redis não está disponível.
"""
import os
import time
import threading
from collections import defaultdict
from datetime import datetime
from typing import Optional, Dict, List

from fastapi import HTTPException, Request, status
from dotenv import load_dotenv

load_dotenv()

# Configuracoes
RATE_LIMIT_REQUESTS = int(os.getenv("RATE_LIMIT_REQUESTS", 100))
RATE_LIMIT_WINDOW = int(os.getenv("RATE_LIMIT_WINDOW", 60))  # segundos
REDIS_URL = os.getenv("REDIS_URL", "redis://localhost:6379")


# =============================================================================
# Issue #141: In-Memory Rate Limiter Fallback
# =============================================================================

class InMemoryRateLimiter:
    """
    Rate limiter em memória para usar quando Redis não está disponível.

    Issue #141: Previne DoS quando Redis cai.
    Implementa sliding window usando timestamps.
    """

    def __init__(self, cleanup_interval: int = 60):
        self._requests: Dict[str, List[float]] = defaultdict(list)
        self._lock = threading.Lock()
        self._cleanup_interval = cleanup_interval
        self._last_cleanup = time.time()

    def check_rate_limit(
        self,
        key: str,
        limit: int,
        window: int
    ) -> tuple:
        """
        Verifica rate limit usando sliding window em memória.

        Args:
            key: Identificador da requisição
            limit: Máximo de requisições
            window: Janela em segundos

        Returns:
            tuple: (allowed, info_dict)
        """
        current_time = time.time()

        with self._lock:
            # Limpar requests antigos periodicamente
            if current_time - self._last_cleanup > self._cleanup_interval:
                self._cleanup_old_entries(window)
                self._last_cleanup = current_time

            # Limpar requests fora da janela para esta key
            cutoff = current_time - window
            self._requests[key] = [
                ts for ts in self._requests[key] if ts > cutoff
            ]

            current_count = len(self._requests[key])

            # Calcular reset time
            if self._requests[key]:
                oldest = min(self._requests[key])
                reset = int(oldest + window - current_time)
            else:
                reset = window

            info = {
                "limit": limit,
                "remaining": max(0, limit - current_count - 1),
                "reset": max(0, reset),
                "current": current_count + 1,
                "backend": "memory"  # Indicar que está usando fallback
            }

            if current_count >= limit:
                return False, info

            # Registrar nova requisição
            self._requests[key].append(current_time)
            return True, info

    def _cleanup_old_entries(self, window: int):
        """Remove entradas antigas de todas as keys"""
        cutoff = time.time() - window
        keys_to_remove = []

        for key in self._requests:
            self._requests[key] = [
                ts for ts in self._requests[key] if ts > cutoff
            ]
            if not self._requests[key]:
                keys_to_remove.append(key)

        for key in keys_to_remove:
            del self._requests[key]

    def reset(self, key: str):
        """Remove limite para uma chave"""
        with self._lock:
            if key in self._requests:
                del self._requests[key]


# Instância global do fallback
_memory_limiter = InMemoryRateLimiter()


class RateLimiter:
    """
    Rate Limiter usando Redis

    Implementa sliding window rate limiting.
    """

    KEY_PREFIX = "ratelimit:"

    def __init__(self, redis_url: str = None):
        self.redis_url = redis_url or REDIS_URL
        self._redis = None

    async def _get_redis(self):
        """Obtem cliente Redis"""
        if self._redis is None:
            try:
                import redis.asyncio as aioredis
                self._redis = aioredis.from_url(
                    self.redis_url,
                    encoding="utf-8",
                    decode_responses=True
                )
            except ImportError:
                print("[RateLimit] Redis nao disponivel")
                return None
        return self._redis

    async def check_rate_limit(
        self,
        key: str,
        limit: int = None,
        window: int = None
    ) -> tuple[bool, dict]:
        """
        Verifica se requisicao esta dentro do rate limit

        Args:
            key: Identificador (IP, user_id, api_key)
            limit: Limite de requisicoes (default: RATE_LIMIT_REQUESTS)
            window: Janela em segundos (default: RATE_LIMIT_WINDOW)

        Returns:
            tuple: (allowed: bool, info: dict)
        """
        limit = limit or RATE_LIMIT_REQUESTS
        window = window or RATE_LIMIT_WINDOW

        redis = await self._get_redis()
        if redis is None:
            # Issue #141: Usar fallback em memória quando Redis não disponível
            return _memory_limiter.check_rate_limit(key, limit, window)

        redis_key = f"{self.KEY_PREFIX}{key}"

        try:
            # Incrementar contador
            current = await redis.incr(redis_key)

            # Se eh a primeira requisicao, definir TTL
            if current == 1:
                await redis.expire(redis_key, window)

            # Obter TTL
            ttl = await redis.ttl(redis_key)

            info = {
                "limit": limit,
                "remaining": max(0, limit - current),
                "reset": ttl,
                "current": current,
                "backend": "redis"
            }

            if current > limit:
                return False, info

            return True, info

        except Exception as e:
            print(f"[RateLimit] Redis erro, usando fallback em memória: {e}")
            # Issue #141: Usar fallback em memória em caso de erro
            return _memory_limiter.check_rate_limit(key, limit, window)

    async def reset(self, key: str):
        """Remove limite para uma chave"""
        # Issue #141: Reset em ambos backends
        _memory_limiter.reset(key)

        redis = await self._get_redis()
        if redis:
            try:
                await redis.delete(f"{self.KEY_PREFIX}{key}")
            except Exception as e:
                print(f"[RateLimit] Erro ao resetar no Redis: {e}")


# Instancia global
_rate_limiter: Optional[RateLimiter] = None


def get_rate_limiter() -> RateLimiter:
    """Obtem instancia do rate limiter"""
    global _rate_limiter
    if _rate_limiter is None:
        _rate_limiter = RateLimiter()
    return _rate_limiter


# =============================================================================
# DEPENDENCIES
# =============================================================================

async def rate_limit_by_ip(request: Request):
    """
    Dependency para rate limit por IP

    Usage:
        @app.get("/api/endpoint")
        async def endpoint(_: None = Depends(rate_limit_by_ip)):
            return {"message": "ok"}
    """
    limiter = get_rate_limiter()
    client_ip = request.client.host if request.client else "unknown"

    allowed, info = await limiter.check_rate_limit(f"ip:{client_ip}")

    # Adicionar headers de rate limit
    request.state.rate_limit_info = info

    if not allowed:
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail={
                "error": "Rate limit exceeded",
                "limit": info["limit"],
                "reset_in": info["reset"]
            },
            headers={
                "X-RateLimit-Limit": str(info["limit"]),
                "X-RateLimit-Remaining": str(info["remaining"]),
                "X-RateLimit-Reset": str(info["reset"]),
                "Retry-After": str(info["reset"])
            }
        )


async def rate_limit_by_user(request: Request):
    """
    Dependency para rate limit por usuario autenticado

    Usa username do token JWT se disponivel, senao usa IP.
    """
    from factory.api.auth import get_current_user_optional

    limiter = get_rate_limiter()

    # Tentar obter usuario do token
    try:
        from fastapi.security import HTTPBearer
        security = HTTPBearer(auto_error=False)
        credentials = await security(request)
        if credentials:
            from factory.api.auth import decode_token
            token_data = decode_token(credentials.credentials)
            key = f"user:{token_data.username}"
        else:
            key = f"ip:{request.client.host}"
    except Exception:
        key = f"ip:{request.client.host if request.client else 'unknown'}"

    allowed, info = await limiter.check_rate_limit(key)
    request.state.rate_limit_info = info

    if not allowed:
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail={
                "error": "Rate limit exceeded",
                "limit": info["limit"],
                "reset_in": info["reset"]
            }
        )


def rate_limit_custom(limit: int, window: int):
    """
    Factory para rate limit customizado

    Usage:
        @app.post("/api/expensive")
        async def expensive(_: None = Depends(rate_limit_custom(10, 3600))):
            return {"message": "ok"}
    """
    async def custom_rate_limit(request: Request):
        limiter = get_rate_limiter()
        client_ip = request.client.host if request.client else "unknown"

        # Usar path como parte da chave para limites por endpoint
        path = request.url.path
        key = f"custom:{client_ip}:{path}"

        allowed, info = await limiter.check_rate_limit(key, limit=limit, window=window)
        request.state.rate_limit_info = info

        if not allowed:
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail={
                    "error": "Rate limit exceeded",
                    "limit": info["limit"],
                    "reset_in": info["reset"]
                }
            )

    return custom_rate_limit


# =============================================================================
# MIDDLEWARE
# =============================================================================

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import Response


class RateLimitMiddleware(BaseHTTPMiddleware):
    """
    Middleware para adicionar headers de rate limit

    Adiciona headers X-RateLimit-* a todas as respostas.
    """

    async def dispatch(self, request: Request, call_next):
        response = await call_next(request)

        # Adicionar headers se rate limit info disponivel
        if hasattr(request.state, "rate_limit_info"):
            info = request.state.rate_limit_info
            response.headers["X-RateLimit-Limit"] = str(info.get("limit", 0))
            response.headers["X-RateLimit-Remaining"] = str(info.get("remaining", 0))
            response.headers["X-RateLimit-Reset"] = str(info.get("reset", 0))

        return response
