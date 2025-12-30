# -*- coding: utf-8 -*-
"""
Resilience Decorators
=====================
Decoradores para rate limiting e retry.

Issue #366 - Terminal A
"""

import asyncio
import functools
import logging
from typing import Any, Callable, Optional, Set, Type, TypeVar, Union

from .rate_limiter import (
    RateLimiter,
    RateLimitConfig,
    RateLimitExceeded,
    get_rate_limiter
)
from .retry import (
    Retry,
    RetryConfig,
    RetryStrategy,
    RetryAttempt,
    RetryExhausted,
    get_retry_preset
)

logger = logging.getLogger(__name__)

F = TypeVar("F", bound=Callable[..., Any])


def rate_limited(
    integration: Optional[str] = None,
    requests: int = 100,
    period: int = 60,
    tenant_arg: Optional[str] = None,
    wait: bool = True,
    max_wait: float = 30.0,
    rate_limiter: Optional[RateLimiter] = None
):
    """
    Decorator para rate limiting.

    Args:
        integration: Nome da integracao (usa nome da funcao se None)
        requests: Numero maximo de requisicoes
        period: Periodo em segundos
        tenant_arg: Nome do argumento que contem tenant_id
        wait: Se True, aguarda quando limite excedido
        max_wait: Tempo maximo de espera
        rate_limiter: Instancia de RateLimiter (usa global se None)

    Exemplo:
        @rate_limited(integration="jira", requests=100, period=60)
        async def call_jira_api():
            ...

        @rate_limited(requests=50, period=60, tenant_arg="tenant_id")
        async def call_api(tenant_id: str, data: dict):
            ...
    """
    def decorator(func: F) -> F:
        if not asyncio.iscoroutinefunction(func):
            raise TypeError("@rate_limited can only be applied to async functions")

        integration_name = integration or func.__name__

        # Configura limite se nao existir
        limiter = rate_limiter or get_rate_limiter()
        if integration_name not in limiter._configs:
            limiter.configure(integration_name, RateLimitConfig(
                requests=requests,
                period=period
            ))

        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # Extrai tenant_id
            tenant_id = None
            if tenant_arg and tenant_arg in kwargs:
                tenant_id = kwargs[tenant_arg]

            # Adquire permissao
            if wait:
                try:
                    await limiter.wait_and_acquire(
                        integration_name,
                        tenant_id=tenant_id,
                        max_wait=max_wait
                    )
                except RateLimitExceeded:
                    logger.error(f"Rate limit exceeded for {integration_name}")
                    raise
            else:
                result = await limiter.acquire(integration_name, tenant_id)
                if not result.allowed:
                    raise RateLimitExceeded(
                        f"Rate limit exceeded for {integration_name}",
                        retry_after=result.retry_after
                    )

            return await func(*args, **kwargs)

        return wrapper

    return decorator


def with_retry(
    max_attempts: int = 3,
    initial_delay: float = 1.0,
    backoff_factor: float = 2.0,
    max_delay: float = 60.0,
    jitter: float = 0.1,
    strategy: RetryStrategy = RetryStrategy.EXPONENTIAL,
    retriable_exceptions: Optional[Set[Type[Exception]]] = None,
    non_retriable_exceptions: Optional[Set[Type[Exception]]] = None,
    on_retry: Optional[Callable[[RetryAttempt], Any]] = None,
    preset: Optional[str] = None
):
    """
    Decorator para retry com backoff exponencial.

    Args:
        max_attempts: Numero maximo de tentativas
        initial_delay: Delay inicial em segundos
        backoff_factor: Fator de multiplicacao
        max_delay: Delay maximo em segundos
        jitter: Jitter aleatorio (0-1)
        strategy: Estrategia de backoff
        retriable_exceptions: Excecoes que disparam retry
        non_retriable_exceptions: Excecoes que NAO disparam retry
        on_retry: Callback chamado em cada retry
        preset: Nome do preset de configuracao

    Exemplo:
        @with_retry(max_attempts=3, backoff_factor=2)
        async def unreliable_api_call():
            ...

        @with_retry(preset="aggressive")
        async def critical_call():
            ...
    """
    def decorator(func: F) -> F:
        if not asyncio.iscoroutinefunction(func):
            raise TypeError("@with_retry can only be applied to async functions")

        # Usa preset se especificado
        if preset:
            config = get_retry_preset(preset)
        else:
            config = RetryConfig(
                max_attempts=max_attempts,
                initial_delay=initial_delay,
                backoff_factor=backoff_factor,
                max_delay=max_delay,
                jitter=jitter,
                strategy=strategy
            )

        # Override excecoes se especificado
        if retriable_exceptions:
            config.retriable_exceptions = retriable_exceptions
        if non_retriable_exceptions:
            config.non_retriable_exceptions = non_retriable_exceptions

        retry = Retry(config)

        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            return await retry.execute(func, *args, on_retry=on_retry, **kwargs)

        # Adiciona metodo para obter config
        wrapper.retry_config = config

        return wrapper

    return decorator


def resilient(
    integration: Optional[str] = None,
    # Rate limit params
    requests: int = 100,
    period: int = 60,
    tenant_arg: Optional[str] = None,
    # Retry params
    max_attempts: int = 3,
    backoff_factor: float = 2.0,
    max_delay: float = 60.0,
    # Circuit breaker integration
    circuit_breaker: Optional[Any] = None,
    on_retry: Optional[Callable[[RetryAttempt], Any]] = None
):
    """
    Decorator combinando rate limiting e retry.

    Aplica na ordem:
    1. Rate limiting
    2. Retry com backoff
    3. Circuit breaker (se fornecido)

    Exemplo:
        @resilient(
            integration="jira",
            requests=100, period=60,
            max_attempts=3, backoff_factor=2
        )
        async def call_jira_api():
            ...
    """
    def decorator(func: F) -> F:
        if not asyncio.iscoroutinefunction(func):
            raise TypeError("@resilient can only be applied to async functions")

        # Aplica decoradores na ordem correta
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            integration_name = integration or func.__name__

            # Rate limiting
            limiter = get_rate_limiter()
            if integration_name not in limiter._configs:
                limiter.configure(integration_name, RateLimitConfig(
                    requests=requests,
                    period=period
                ))

            tenant_id = kwargs.get(tenant_arg) if tenant_arg else None

            await limiter.wait_and_acquire(
                integration_name,
                tenant_id=tenant_id,
                max_wait=30.0
            )

            # Retry
            config = RetryConfig(
                max_attempts=max_attempts,
                backoff_factor=backoff_factor,
                max_delay=max_delay,
                jitter=0.1
            )
            retry = Retry(config)

            async def execute():
                # Circuit breaker check
                if circuit_breaker:
                    if hasattr(circuit_breaker, 'is_open') and circuit_breaker.is_open:
                        raise CircuitOpen(f"Circuit breaker is open for {integration_name}")

                try:
                    result = await func(*args, **kwargs)

                    # Registra sucesso no circuit breaker
                    if circuit_breaker and hasattr(circuit_breaker, 'record_success'):
                        circuit_breaker.record_success()

                    return result

                except Exception as e:
                    # Registra falha no circuit breaker
                    if circuit_breaker and hasattr(circuit_breaker, 'record_failure'):
                        circuit_breaker.record_failure()
                    raise

            return await retry.execute(execute, on_retry=on_retry)

        return wrapper

    return decorator


class CircuitOpen(Exception):
    """Excecao quando circuit breaker esta aberto"""
    pass


def retry_on(*exceptions: Type[Exception], **retry_kwargs):
    """
    Decorator simplificado para retry em excecoes especificas.

    Exemplo:
        @retry_on(ConnectionError, TimeoutError, max_attempts=5)
        async def make_request():
            ...
    """
    retry_kwargs['retriable_exceptions'] = set(exceptions)
    return with_retry(**retry_kwargs)


def no_retry_on(*exceptions: Type[Exception], **retry_kwargs):
    """
    Decorator que faz retry em tudo EXCETO as excecoes especificadas.

    Exemplo:
        @no_retry_on(ValueError, KeyError, max_attempts=3)
        async def process_data():
            ...
    """
    retry_kwargs['non_retriable_exceptions'] = set(exceptions)
    return with_retry(**retry_kwargs)


class RateLimitedRetry:
    """
    Classe para uso programatico de rate limiting + retry.

    Exemplo:
        handler = RateLimitedRetry(
            integration="jira",
            requests=100,
            max_attempts=3
        )

        result = await handler.execute(api_call, arg1, arg2)
    """

    def __init__(
        self,
        integration: str,
        requests: int = 100,
        period: int = 60,
        max_attempts: int = 3,
        backoff_factor: float = 2.0,
        max_delay: float = 60.0
    ):
        self._integration = integration
        self._limiter = get_rate_limiter()

        if integration not in self._limiter._configs:
            self._limiter.configure(integration, RateLimitConfig(
                requests=requests,
                period=period
            ))

        self._retry = Retry(RetryConfig(
            max_attempts=max_attempts,
            backoff_factor=backoff_factor,
            max_delay=max_delay
        ))

    async def execute(
        self,
        func: Callable,
        *args,
        tenant_id: Optional[str] = None,
        on_retry: Optional[Callable] = None,
        **kwargs
    ) -> Any:
        """Executa funcao com rate limiting e retry"""
        await self._limiter.wait_and_acquire(
            self._integration,
            tenant_id=tenant_id
        )

        return await self._retry.execute(func, *args, on_retry=on_retry, **kwargs)

    @property
    def remaining_requests(self) -> int:
        """Requisicoes restantes"""
        return self._limiter.get_remaining(self._integration)

    def get_stats(self) -> dict:
        """Estatisticas de rate limiting"""
        return self._limiter.get_stats().get(self._integration, {})
