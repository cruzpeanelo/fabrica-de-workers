# -*- coding: utf-8 -*-
"""
Rate Limiter
============
Rate limiting para integracoes usando Token Bucket algorithm.

Issue #366 - Terminal A
"""

import asyncio
import logging
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)


class RateLimitStrategy(str, Enum):
    """Estrategias de rate limiting"""
    TOKEN_BUCKET = "token_bucket"
    SLIDING_WINDOW = "sliding_window"
    FIXED_WINDOW = "fixed_window"


@dataclass
class RateLimitConfig:
    """
    Configuracao de rate limit.

    Attributes:
        requests: Numero maximo de requisicoes
        period: Periodo em segundos
        burst: Capacidade de burst (para token bucket)
        strategy: Estrategia de rate limiting
    """
    requests: int = 100
    period: int = 60
    burst: Optional[int] = None
    strategy: RateLimitStrategy = RateLimitStrategy.TOKEN_BUCKET

    def __post_init__(self):
        if self.burst is None:
            self.burst = self.requests


@dataclass
class RateLimitResult:
    """Resultado de verificacao de rate limit"""
    allowed: bool
    remaining: int
    reset_at: float
    retry_after: Optional[float] = None

    def to_headers(self) -> Dict[str, str]:
        """Gera headers HTTP de rate limit"""
        return {
            "X-RateLimit-Limit": str(self.remaining + (0 if self.allowed else 1)),
            "X-RateLimit-Remaining": str(max(0, self.remaining)),
            "X-RateLimit-Reset": str(int(self.reset_at)),
            **({"Retry-After": str(int(self.retry_after))} if self.retry_after else {})
        }


class TokenBucket:
    """
    Implementacao do Token Bucket algorithm.

    O bucket e preenchido com tokens a uma taxa constante.
    Cada requisicao consome um token.
    Se nao ha tokens, a requisicao e bloqueada.
    """

    def __init__(
        self,
        capacity: int,
        refill_rate: float,
        initial_tokens: Optional[int] = None
    ):
        """
        Args:
            capacity: Capacidade maxima de tokens
            refill_rate: Tokens adicionados por segundo
            initial_tokens: Tokens iniciais (default: capacity)
        """
        self._capacity = capacity
        self._refill_rate = refill_rate
        self._tokens = initial_tokens if initial_tokens is not None else capacity
        self._last_refill = time.time()
        self._lock = asyncio.Lock()

    async def acquire(self, tokens: int = 1) -> Tuple[bool, int, float]:
        """
        Tenta adquirir tokens.

        Args:
            tokens: Numero de tokens a adquirir

        Returns:
            Tupla (sucesso, tokens_restantes, tempo_para_reset)
        """
        async with self._lock:
            now = time.time()
            self._refill(now)

            if self._tokens >= tokens:
                self._tokens -= tokens
                return True, int(self._tokens), now + (self._capacity / self._refill_rate)
            else:
                wait_time = (tokens - self._tokens) / self._refill_rate
                return False, 0, now + wait_time

    def _refill(self, now: float):
        """Reabastece tokens baseado no tempo decorrido"""
        elapsed = now - self._last_refill
        new_tokens = elapsed * self._refill_rate
        self._tokens = min(self._capacity, self._tokens + new_tokens)
        self._last_refill = now

    @property
    def tokens(self) -> int:
        """Tokens atuais (sem lock, aproximado)"""
        return int(self._tokens)


class SlidingWindowCounter:
    """
    Implementacao do Sliding Window Counter.

    Conta requisicoes em uma janela deslizante.
    """

    def __init__(self, limit: int, window_size: int):
        """
        Args:
            limit: Limite de requisicoes
            window_size: Tamanho da janela em segundos
        """
        self._limit = limit
        self._window_size = window_size
        self._requests: List[float] = []
        self._lock = asyncio.Lock()

    async def acquire(self) -> Tuple[bool, int, float]:
        """Tenta adquirir permissao"""
        async with self._lock:
            now = time.time()
            cutoff = now - self._window_size

            # Remove requisicoes antigas
            self._requests = [t for t in self._requests if t > cutoff]

            remaining = self._limit - len(self._requests)
            reset_at = now + self._window_size

            if remaining > 0:
                self._requests.append(now)
                return True, remaining - 1, reset_at
            else:
                oldest = self._requests[0] if self._requests else now
                retry_after = oldest + self._window_size - now
                return False, 0, now + retry_after


class RateLimiter:
    """
    Rate limiter central para integracoes.

    Suporta limites por integracao e por tenant.

    Exemplo:
        limiter = RateLimiter()

        # Configurar limite para Jira
        limiter.configure("jira", RateLimitConfig(requests=100, period=60))

        # Verificar antes de chamar
        result = await limiter.acquire("jira", tenant_id="tenant-123")
        if result.allowed:
            # Fazer requisicao
            ...
        else:
            # Aguardar ou retornar erro
            await asyncio.sleep(result.retry_after)
    """

    # Limites padrao por integracao
    DEFAULT_LIMITS: Dict[str, RateLimitConfig] = {
        "jira": RateLimitConfig(requests=100, period=60),
        "salesforce": RateLimitConfig(requests=100, period=60),
        "azure_devops": RateLimitConfig(requests=200, period=60),
        "sap": RateLimitConfig(requests=50, period=60),
        "github": RateLimitConfig(requests=60, period=60),
    }

    def __init__(self, use_defaults: bool = True):
        self._configs: Dict[str, RateLimitConfig] = {}
        self._buckets: Dict[str, TokenBucket] = {}
        self._windows: Dict[str, SlidingWindowCounter] = {}
        self._stats: Dict[str, Dict[str, int]] = {}  # integration -> {allowed, blocked}

        if use_defaults:
            for name, config in self.DEFAULT_LIMITS.items():
                self.configure(name, config)

    def configure(self, integration: str, config: RateLimitConfig):
        """Configura limite para uma integracao"""
        self._configs[integration] = config
        logger.info(
            f"Rate limit configured: {integration} - "
            f"{config.requests} req/{config.period}s"
        )

    def _get_bucket_key(
        self,
        integration: str,
        tenant_id: Optional[str] = None
    ) -> str:
        """Gera chave para o bucket"""
        if tenant_id:
            return f"{integration}:{tenant_id}"
        return integration

    def _get_or_create_bucket(
        self,
        key: str,
        config: RateLimitConfig
    ) -> TokenBucket:
        """Obtem ou cria bucket para a chave"""
        if key not in self._buckets:
            refill_rate = config.requests / config.period
            self._buckets[key] = TokenBucket(
                capacity=config.burst,
                refill_rate=refill_rate
            )
        return self._buckets[key]

    def _get_or_create_window(
        self,
        key: str,
        config: RateLimitConfig
    ) -> SlidingWindowCounter:
        """Obtem ou cria sliding window para a chave"""
        if key not in self._windows:
            self._windows[key] = SlidingWindowCounter(
                limit=config.requests,
                window_size=config.period
            )
        return self._windows[key]

    async def acquire(
        self,
        integration: str,
        tenant_id: Optional[str] = None,
        tokens: int = 1
    ) -> RateLimitResult:
        """
        Tenta adquirir permissao para fazer requisicao.

        Args:
            integration: Nome da integracao
            tenant_id: ID do tenant (opcional)
            tokens: Numero de tokens a consumir

        Returns:
            Resultado da verificacao
        """
        config = self._configs.get(integration)
        if not config:
            # Sem limite configurado, permite tudo
            return RateLimitResult(
                allowed=True,
                remaining=float('inf'),
                reset_at=time.time()
            )

        key = self._get_bucket_key(integration, tenant_id)

        # Inicializa estatisticas
        if integration not in self._stats:
            self._stats[integration] = {"allowed": 0, "blocked": 0}

        if config.strategy == RateLimitStrategy.TOKEN_BUCKET:
            bucket = self._get_or_create_bucket(key, config)
            allowed, remaining, reset_at = await bucket.acquire(tokens)

        elif config.strategy == RateLimitStrategy.SLIDING_WINDOW:
            window = self._get_or_create_window(key, config)
            allowed, remaining, reset_at = await window.acquire()

        else:
            # Fixed window - usa sliding window com implementacao simples
            window = self._get_or_create_window(key, config)
            allowed, remaining, reset_at = await window.acquire()

        # Atualiza estatisticas
        if allowed:
            self._stats[integration]["allowed"] += 1
        else:
            self._stats[integration]["blocked"] += 1
            logger.warning(
                f"Rate limit exceeded: {integration}"
                + (f" tenant={tenant_id}" if tenant_id else "")
            )

        result = RateLimitResult(
            allowed=allowed,
            remaining=remaining,
            reset_at=reset_at,
            retry_after=None if allowed else (reset_at - time.time())
        )

        return result

    async def wait_and_acquire(
        self,
        integration: str,
        tenant_id: Optional[str] = None,
        max_wait: float = 30.0
    ) -> RateLimitResult:
        """
        Aguarda se necessario e adquire permissao.

        Args:
            integration: Nome da integracao
            tenant_id: ID do tenant
            max_wait: Tempo maximo de espera em segundos

        Returns:
            Resultado da aquisicao

        Raises:
            RateLimitExceeded: Se exceder max_wait
        """
        start = time.time()

        while True:
            result = await self.acquire(integration, tenant_id)

            if result.allowed:
                return result

            if result.retry_after > max_wait:
                raise RateLimitExceeded(
                    f"Rate limit exceeded for {integration}, "
                    f"retry after {result.retry_after:.1f}s"
                )

            elapsed = time.time() - start
            if elapsed + result.retry_after > max_wait:
                raise RateLimitExceeded(
                    f"Rate limit exceeded for {integration}, "
                    f"would exceed max_wait of {max_wait}s"
                )

            await asyncio.sleep(min(result.retry_after, 1.0))

    def get_remaining(
        self,
        integration: str,
        tenant_id: Optional[str] = None
    ) -> int:
        """Retorna tokens/requisicoes restantes"""
        key = self._get_bucket_key(integration, tenant_id)

        if key in self._buckets:
            return self._buckets[key].tokens

        config = self._configs.get(integration)
        if config:
            return config.requests

        return float('inf')

    def reset(
        self,
        integration: Optional[str] = None,
        tenant_id: Optional[str] = None
    ):
        """Reseta limites"""
        if integration and tenant_id:
            key = self._get_bucket_key(integration, tenant_id)
            self._buckets.pop(key, None)
            self._windows.pop(key, None)
        elif integration:
            keys_to_remove = [k for k in self._buckets if k.startswith(integration)]
            for key in keys_to_remove:
                self._buckets.pop(key, None)
                self._windows.pop(key, None)
        else:
            self._buckets.clear()
            self._windows.clear()

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas de rate limiting"""
        result = {}
        for integration, stats in self._stats.items():
            total = stats["allowed"] + stats["blocked"]
            blocked_pct = (stats["blocked"] / total * 100) if total > 0 else 0
            result[integration] = {
                **stats,
                "total": total,
                "blocked_percentage": round(blocked_pct, 2)
            }
        return result


class RateLimitExceeded(Exception):
    """Excecao quando rate limit e excedido"""

    def __init__(self, message: str, retry_after: Optional[float] = None):
        super().__init__(message)
        self.retry_after = retry_after


# Singleton global
_rate_limiter: Optional[RateLimiter] = None


def get_rate_limiter() -> RateLimiter:
    """Obtem instancia global do rate limiter"""
    global _rate_limiter
    if _rate_limiter is None:
        _rate_limiter = RateLimiter()
    return _rate_limiter


def reset_rate_limiter():
    """Reseta rate limiter global"""
    global _rate_limiter
    _rate_limiter = None
