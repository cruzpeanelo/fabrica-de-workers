# -*- coding: utf-8 -*-
"""
Resilience Module
=================
Rate limiting e retry para integracoes.

Issue #366 - Terminal A

Features:
- Rate limiting com Token Bucket
- Retry com backoff exponencial
- Decoradores para uso facil
- Integracao com circuit breaker

Exemplo:
    from factory.integrations.resilience import (
        rate_limited,
        with_retry,
        resilient,
        RateLimiter,
        Retry
    )

    # Rate limiting
    @rate_limited(integration="jira", requests=100, period=60)
    async def call_jira_api():
        ...

    # Retry com backoff
    @with_retry(max_attempts=3, backoff_factor=2)
    async def unreliable_call():
        ...

    # Combinado
    @resilient(integration="salesforce", requests=100, max_attempts=3)
    async def call_salesforce():
        ...
"""

# Rate Limiter
from .rate_limiter import (
    RateLimiter,
    RateLimitConfig,
    RateLimitResult,
    RateLimitStrategy,
    RateLimitExceeded,
    TokenBucket,
    SlidingWindowCounter,
    get_rate_limiter,
    reset_rate_limiter
)

# Retry
from .retry import (
    Retry,
    RetryConfig,
    RetryResult,
    RetryAttempt,
    RetryStrategy,
    RetryExhausted,
    RetryContext,
    RETRY_PRESETS,
    RETRIABLE_HTTP_CODES,
    get_retry_preset,
    is_retriable_http_error
)

# Decorators
from .decorators import (
    rate_limited,
    with_retry,
    resilient,
    retry_on,
    no_retry_on,
    RateLimitedRetry,
    CircuitOpen
)

__all__ = [
    # Rate Limiter
    "RateLimiter",
    "RateLimitConfig",
    "RateLimitResult",
    "RateLimitStrategy",
    "RateLimitExceeded",
    "TokenBucket",
    "SlidingWindowCounter",
    "get_rate_limiter",
    "reset_rate_limiter",

    # Retry
    "Retry",
    "RetryConfig",
    "RetryResult",
    "RetryAttempt",
    "RetryStrategy",
    "RetryExhausted",
    "RetryContext",
    "RETRY_PRESETS",
    "RETRIABLE_HTTP_CODES",
    "get_retry_preset",
    "is_retriable_http_error",

    # Decorators
    "rate_limited",
    "with_retry",
    "resilient",
    "retry_on",
    "no_retry_on",
    "RateLimitedRetry",
    "CircuitOpen"
]
