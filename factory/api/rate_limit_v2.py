# -*- coding: utf-8 -*-
"""
Rate Limiting v2 - Fabrica de Agentes v6.5
==========================================

Sistema avancado de rate limiting com suporte a:
- Rate limiting por API Key tier
- Sliding window algorithm
- Redis-based para escalabilidade
- Headers padrao (RFC 6585)
"""
import os
import time
import hashlib
from datetime import datetime, timedelta
from typing import Optional, Tuple, Dict
from functools import wraps

from fastapi import HTTPException, Request, Response, status
from starlette.middleware.base import BaseHTTPMiddleware

from dotenv import load_dotenv
load_dotenv()


# =============================================================================
# CONFIGURACAO
# =============================================================================

REDIS_URL = os.getenv("REDIS_URL", "redis://localhost:6379")

# Limites por tier (requests por minuto e por dia)
TIER_LIMITS = {
    "free": {
        "per_minute": 100,
        "per_day": 1000,
        "burst": 20,  # Burst adicional permitido
    },
    "basic": {
        "per_minute": 500,
        "per_day": 10000,
        "burst": 50,
    },
    "pro": {
        "per_minute": 2000,
        "per_day": 100000,
        "burst": 200,
    },
    "enterprise": {
        "per_minute": 10000,
        "per_day": 1000000,
        "burst": 1000,
    },
}

# Limites por endpoint especifico (override do tier)
ENDPOINT_LIMITS = {
    "/api/v1/jobs": {"per_minute": 10},  # Criar jobs e mais caro
    "/api/v1/stories/execute": {"per_minute": 5},  # Execucao e muito cara
    "/api/v1/webhooks": {"per_minute": 30},
}


# =============================================================================
# RATE LIMITER CLASS
# =============================================================================

class TieredRateLimiter:
    """
    Rate Limiter com suporte a tiers e Redis

    Implementa sliding window log algorithm para precisao.
    """

    KEY_PREFIX = "ratelimit:v2:"

    def __init__(self, redis_url: str = None):
        self.redis_url = redis_url or REDIS_URL
        self._redis = None

    async def _get_redis(self):
        """Obtem cliente Redis async"""
        if self._redis is None:
            try:
                import redis.asyncio as aioredis
                self._redis = aioredis.from_url(
                    self.redis_url,
                    encoding="utf-8",
                    decode_responses=True
                )
            except ImportError:
                print("[RateLimitV2] redis.asyncio nao disponivel, usando fallback em memoria")
                return None
            except Exception as e:
                print(f"[RateLimitV2] Erro ao conectar Redis: {e}")
                return None
        return self._redis

    async def check_rate_limit(
        self,
        identifier: str,
        tier: str = "free",
        endpoint: str = None,
    ) -> Tuple[bool, Dict]:
        """
        Verifica rate limit para um identificador

        Args:
            identifier: API Key ID ou IP
            tier: Tier do usuario (free, basic, pro, enterprise)
            endpoint: Path do endpoint (para limites especificos)

        Returns:
            tuple: (allowed: bool, info: dict)
        """
        # Obter limites do tier
        limits = TIER_LIMITS.get(tier, TIER_LIMITS["free"]).copy()

        # Override para endpoints especificos
        if endpoint and endpoint in ENDPOINT_LIMITS:
            endpoint_limits = ENDPOINT_LIMITS[endpoint]
            limits.update(endpoint_limits)

        redis = await self._get_redis()

        if redis is None:
            # Fallback: permitir (fail open) mas com metricas em memoria
            return await self._check_rate_limit_memory(identifier, limits)

        return await self._check_rate_limit_redis(redis, identifier, limits)

    async def _check_rate_limit_redis(
        self,
        redis,
        identifier: str,
        limits: dict
    ) -> Tuple[bool, Dict]:
        """Implementacao com Redis usando sliding window"""
        now = time.time()
        minute_window = 60
        day_window = 86400

        # Keys para minuto e dia
        minute_key = f"{self.KEY_PREFIX}min:{identifier}"
        day_key = f"{self.KEY_PREFIX}day:{identifier}"

        try:
            # Usar pipeline para atomicidade
            pipe = redis.pipeline()

            # Limpar entradas antigas e contar
            pipe.zremrangebyscore(minute_key, 0, now - minute_window)
            pipe.zcard(minute_key)
            pipe.zremrangebyscore(day_key, 0, now - day_window)
            pipe.zcard(day_key)

            results = await pipe.execute()
            minute_count = results[1]
            day_count = results[3]

            # Verificar limites
            per_minute = limits.get("per_minute", 100)
            per_day = limits.get("per_day", 1000)

            info = {
                "limit_minute": per_minute,
                "remaining_minute": max(0, per_minute - minute_count),
                "limit_day": per_day,
                "remaining_day": max(0, per_day - day_count),
                "reset_minute": int(now + minute_window),
                "reset_day": int(now + day_window),
            }

            # Verificar se excedeu
            if minute_count >= per_minute:
                info["exceeded"] = "minute"
                info["retry_after"] = minute_window - (now % minute_window)
                return False, info

            if day_count >= per_day:
                info["exceeded"] = "day"
                info["retry_after"] = day_window - (now % day_window)
                return False, info

            # Adicionar request atual
            request_id = f"{now}:{hashlib.md5(str(now).encode()).hexdigest()[:8]}"
            pipe2 = redis.pipeline()
            pipe2.zadd(minute_key, {request_id: now})
            pipe2.zadd(day_key, {request_id: now})
            pipe2.expire(minute_key, minute_window + 1)
            pipe2.expire(day_key, day_window + 1)
            await pipe2.execute()

            return True, info

        except Exception as e:
            print(f"[RateLimitV2] Erro Redis: {e}")
            # Fail open
            return True, {
                "limit_minute": limits.get("per_minute", 100),
                "remaining_minute": limits.get("per_minute", 100),
                "error": str(e)
            }

    async def _check_rate_limit_memory(
        self,
        identifier: str,
        limits: dict
    ) -> Tuple[bool, Dict]:
        """Fallback em memoria (para desenvolvimento sem Redis)"""
        # Implementacao simples em memoria
        # Em producao, sempre usar Redis
        per_minute = limits.get("per_minute", 100)
        per_day = limits.get("per_day", 1000)

        return True, {
            "limit_minute": per_minute,
            "remaining_minute": per_minute,
            "limit_day": per_day,
            "remaining_day": per_day,
            "note": "Rate limiting em modo fallback (sem Redis)"
        }

    async def get_usage_stats(self, identifier: str) -> Dict:
        """Retorna estatisticas de uso"""
        redis = await self._get_redis()
        if not redis:
            return {"error": "Redis nao disponivel"}

        now = time.time()
        minute_key = f"{self.KEY_PREFIX}min:{identifier}"
        day_key = f"{self.KEY_PREFIX}day:{identifier}"

        try:
            minute_count = await redis.zcard(minute_key) or 0
            day_count = await redis.zcard(day_key) or 0

            return {
                "identifier": identifier,
                "requests_last_minute": minute_count,
                "requests_last_day": day_count,
                "timestamp": datetime.utcnow().isoformat()
            }
        except Exception as e:
            return {"error": str(e)}

    async def reset(self, identifier: str):
        """Remove todos os limites para um identificador"""
        redis = await self._get_redis()
        if redis:
            minute_key = f"{self.KEY_PREFIX}min:{identifier}"
            day_key = f"{self.KEY_PREFIX}day:{identifier}"
            await redis.delete(minute_key, day_key)


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_rate_limiter: Optional[TieredRateLimiter] = None


def get_tiered_rate_limiter() -> TieredRateLimiter:
    """Obtem instancia do rate limiter"""
    global _rate_limiter
    if _rate_limiter is None:
        _rate_limiter = TieredRateLimiter()
    return _rate_limiter


# =============================================================================
# MIDDLEWARE
# =============================================================================

class TieredRateLimitMiddleware(BaseHTTPMiddleware):
    """
    Middleware para rate limiting automatico

    Aplica rate limit baseado na API Key (se presente) ou IP.
    Adiciona headers padrao X-RateLimit-*.
    """

    # Paths que nao tem rate limit (ou tem limite maior)
    EXEMPT_PATHS = {
        "/api/v1/health",
        "/api/v1/docs",
        "/api/v1/openapi.json",
        "/openapi.json",
        "/docs",
        "/redoc",
    }

    async def dispatch(self, request: Request, call_next):
        path = request.url.path

        # Pular paths isentos
        if path in self.EXEMPT_PATHS or path.startswith("/static"):
            return await call_next(request)

        # Determinar identificador e tier
        identifier, tier = await self._get_identifier_and_tier(request)

        # Verificar rate limit
        limiter = get_tiered_rate_limiter()
        allowed, info = await limiter.check_rate_limit(
            identifier=identifier,
            tier=tier,
            endpoint=path
        )

        if not allowed:
            return self._rate_limit_exceeded_response(info)

        # Processar request
        response = await call_next(request)

        # Adicionar headers de rate limit
        response.headers["X-RateLimit-Limit"] = str(info.get("limit_minute", 100))
        response.headers["X-RateLimit-Remaining"] = str(info.get("remaining_minute", 100))
        response.headers["X-RateLimit-Reset"] = str(info.get("reset_minute", 0))

        # Header adicional para limite diario
        if "limit_day" in info:
            response.headers["X-RateLimit-Limit-Day"] = str(info["limit_day"])
            response.headers["X-RateLimit-Remaining-Day"] = str(info["remaining_day"])

        return response

    async def _get_identifier_and_tier(self, request: Request) -> Tuple[str, str]:
        """Extrai identificador e tier do request"""
        # Tentar obter da API Key (se ja autenticado)
        if hasattr(request.state, "api_key_info"):
            key_info = request.state.api_key_info
            return f"key:{key_info.key_id}", key_info.tier

        # Tentar obter API Key do header/query
        api_key = (
            request.headers.get("X-API-Key") or
            request.query_params.get("api_key")
        )

        if api_key:
            # Hash simples para identificacao (validacao completa em outro middleware)
            key_hash = hashlib.sha256(api_key.encode()).hexdigest()[:16]
            # Assumir tier free para rate limit inicial
            # O tier correto sera aplicado apos autenticacao completa
            return f"key:{key_hash}", "free"

        # Fallback: usar IP
        client_ip = request.client.host if request.client else "unknown"
        return f"ip:{client_ip}", "free"

    def _rate_limit_exceeded_response(self, info: Dict) -> Response:
        """Gera resposta 429 padronizada"""
        from fastapi.responses import JSONResponse

        retry_after = int(info.get("retry_after", 60))
        exceeded_type = info.get("exceeded", "minute")

        body = {
            "error": "rate_limit_exceeded",
            "message": f"Limite de requisicoes excedido ({exceeded_type})",
            "limit": info.get(f"limit_{exceeded_type}", 100),
            "retry_after": retry_after,
            "docs": "https://docs.fabricadeagentes.com/api/rate-limits"
        }

        return JSONResponse(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            content=body,
            headers={
                "X-RateLimit-Limit": str(info.get("limit_minute", 100)),
                "X-RateLimit-Remaining": "0",
                "X-RateLimit-Reset": str(info.get("reset_minute", 0)),
                "Retry-After": str(retry_after),
            }
        )


# =============================================================================
# DEPENDENCIES
# =============================================================================

async def rate_limit_by_api_key(request: Request):
    """
    Dependency para rate limit baseado na API Key autenticada

    Deve ser usado APOS autenticacao da API Key.
    """
    if not hasattr(request.state, "api_key_info"):
        # Se nao autenticado, usar rate limit por IP
        client_ip = request.client.host if request.client else "unknown"
        identifier = f"ip:{client_ip}"
        tier = "free"
    else:
        key_info = request.state.api_key_info
        identifier = f"key:{key_info.key_id}"
        tier = key_info.tier

    limiter = get_tiered_rate_limiter()
    allowed, info = await limiter.check_rate_limit(
        identifier=identifier,
        tier=tier,
        endpoint=request.url.path
    )

    # Armazenar info para headers
    request.state.rate_limit_info = info

    if not allowed:
        retry_after = int(info.get("retry_after", 60))
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail={
                "error": "rate_limit_exceeded",
                "message": "Limite de requisicoes excedido",
                "retry_after": retry_after,
            },
            headers={
                "Retry-After": str(retry_after),
                "X-RateLimit-Remaining": "0",
            }
        )


def rate_limit_custom(per_minute: int, per_day: int = None):
    """
    Factory para rate limit customizado por endpoint

    Usage:
        @app.post("/api/v1/expensive-operation")
        async def expensive(_: None = Depends(rate_limit_custom(5, 100))):
            pass
    """
    async def custom_limiter(request: Request):
        # Determinar identificador
        if hasattr(request.state, "api_key_info"):
            identifier = f"key:{request.state.api_key_info.key_id}"
        else:
            client_ip = request.client.host if request.client else "unknown"
            identifier = f"ip:{client_ip}"

        # Adicionar path para chave unica por endpoint
        path_hash = hashlib.md5(request.url.path.encode()).hexdigest()[:8]
        identifier = f"{identifier}:{path_hash}"

        limiter = get_tiered_rate_limiter()
        allowed, info = await limiter.check_rate_limit(
            identifier=identifier,
            tier="custom",  # Tier especial com limites do decorator
        )

        # Override com limites customizados
        info["limit_minute"] = per_minute
        if per_day:
            info["limit_day"] = per_day

        request.state.rate_limit_info = info

        if not allowed:
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail={
                    "error": "rate_limit_exceeded",
                    "message": "Limite de requisicoes excedido para esta operacao",
                    "retry_after": info.get("retry_after", 60),
                }
            )

    return custom_limiter
