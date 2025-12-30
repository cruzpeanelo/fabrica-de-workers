# -*- coding: utf-8 -*-
"""
Retry with Exponential Backoff
==============================
Sistema de retry com backoff exponencial e jitter.

Issue #366 - Terminal A
"""

import asyncio
import logging
import random
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set, Type, Union

logger = logging.getLogger(__name__)


class RetryStrategy(str, Enum):
    """Estrategias de retry"""
    EXPONENTIAL = "exponential"     # Backoff exponencial
    LINEAR = "linear"               # Backoff linear
    CONSTANT = "constant"           # Delay constante
    FIBONACCI = "fibonacci"         # Sequencia Fibonacci


@dataclass
class RetryConfig:
    """
    Configuracao de retry.

    Attributes:
        max_attempts: Numero maximo de tentativas
        initial_delay: Delay inicial em segundos
        max_delay: Delay maximo em segundos
        backoff_factor: Fator de multiplicacao (para exponencial)
        jitter: Adicionar jitter aleatorio (0-1)
        strategy: Estrategia de backoff
        retriable_exceptions: Excecoes que disparam retry
        non_retriable_exceptions: Excecoes que NAO disparam retry
    """
    max_attempts: int = 3
    initial_delay: float = 1.0
    max_delay: float = 60.0
    backoff_factor: float = 2.0
    jitter: float = 0.1
    strategy: RetryStrategy = RetryStrategy.EXPONENTIAL
    retriable_exceptions: Set[Type[Exception]] = field(
        default_factory=lambda: {
            ConnectionError,
            TimeoutError,
            asyncio.TimeoutError,
        }
    )
    non_retriable_exceptions: Set[Type[Exception]] = field(
        default_factory=lambda: {
            ValueError,
            TypeError,
            KeyError,
        }
    )


@dataclass
class RetryAttempt:
    """Informacoes sobre uma tentativa"""
    attempt: int
    max_attempts: int
    delay: float
    error: Optional[Exception]
    timestamp: datetime = field(default_factory=datetime.utcnow)


@dataclass
class RetryResult:
    """Resultado apos todas as tentativas"""
    success: bool
    result: Any
    attempts: int
    total_delay: float
    history: List[RetryAttempt]
    final_error: Optional[Exception] = None


class RetryExhausted(Exception):
    """Excecao quando todas as tentativas falham"""

    def __init__(
        self,
        message: str,
        attempts: int,
        last_error: Optional[Exception] = None
    ):
        super().__init__(message)
        self.attempts = attempts
        self.last_error = last_error


class Retry:
    """
    Sistema de retry com backoff exponencial.

    Exemplo:
        retry = Retry(RetryConfig(max_attempts=3, backoff_factor=2))

        # Uso basico
        result = await retry.execute(async_function, arg1, arg2)

        # Com callback de retry
        async def on_retry(attempt: RetryAttempt):
            logger.warning(f"Retry {attempt.attempt}: {attempt.error}")

        result = await retry.execute(
            async_function,
            on_retry=on_retry
        )
    """

    def __init__(self, config: Optional[RetryConfig] = None):
        self._config = config or RetryConfig()
        self._fibonacci_cache = [1, 1]

    def _calculate_delay(self, attempt: int) -> float:
        """Calcula delay baseado na estrategia"""
        config = self._config

        if config.strategy == RetryStrategy.EXPONENTIAL:
            delay = config.initial_delay * (config.backoff_factor ** (attempt - 1))

        elif config.strategy == RetryStrategy.LINEAR:
            delay = config.initial_delay * attempt

        elif config.strategy == RetryStrategy.CONSTANT:
            delay = config.initial_delay

        elif config.strategy == RetryStrategy.FIBONACCI:
            delay = config.initial_delay * self._get_fibonacci(attempt)

        else:
            delay = config.initial_delay

        # Aplica jitter
        if config.jitter > 0:
            jitter_range = delay * config.jitter
            delay += random.uniform(-jitter_range, jitter_range)

        # Aplica max_delay
        delay = min(delay, config.max_delay)

        return max(0, delay)

    def _get_fibonacci(self, n: int) -> int:
        """Obtem n-esimo numero de Fibonacci"""
        while len(self._fibonacci_cache) <= n:
            self._fibonacci_cache.append(
                self._fibonacci_cache[-1] + self._fibonacci_cache[-2]
            )
        return self._fibonacci_cache[n]

    def _should_retry(self, error: Exception) -> bool:
        """Determina se deve fazer retry para a excecao"""
        # Se esta na lista de non-retriable, nao faz retry
        for exc_type in self._config.non_retriable_exceptions:
            if isinstance(error, exc_type):
                return False

        # Se retriable_exceptions esta vazia, todas sao retriable
        if not self._config.retriable_exceptions:
            return True

        # Verifica se esta na lista de retriable
        for exc_type in self._config.retriable_exceptions:
            if isinstance(error, exc_type):
                return True

        # Por padrao, faz retry para excecoes nao especificadas
        return True

    async def execute(
        self,
        func: Callable,
        *args,
        on_retry: Optional[Callable[[RetryAttempt], Any]] = None,
        on_success: Optional[Callable[[Any], Any]] = None,
        on_failure: Optional[Callable[[RetryResult], Any]] = None,
        **kwargs
    ) -> Any:
        """
        Executa funcao com retry.

        Args:
            func: Funcao async a executar
            *args: Argumentos para a funcao
            on_retry: Callback chamado em cada retry
            on_success: Callback chamado em sucesso
            on_failure: Callback chamado em falha final
            **kwargs: Argumentos nomeados para a funcao

        Returns:
            Resultado da funcao

        Raises:
            RetryExhausted: Se todas as tentativas falharem
        """
        attempts = []
        total_delay = 0.0
        last_error = None

        for attempt in range(1, self._config.max_attempts + 1):
            try:
                if asyncio.iscoroutinefunction(func):
                    result = await func(*args, **kwargs)
                else:
                    result = func(*args, **kwargs)

                # Sucesso
                if on_success:
                    if asyncio.iscoroutinefunction(on_success):
                        await on_success(result)
                    else:
                        on_success(result)

                return result

            except Exception as e:
                last_error = e
                delay = self._calculate_delay(attempt)

                attempt_info = RetryAttempt(
                    attempt=attempt,
                    max_attempts=self._config.max_attempts,
                    delay=delay,
                    error=e
                )
                attempts.append(attempt_info)

                # Verifica se deve fazer retry
                if not self._should_retry(e):
                    logger.error(f"Non-retriable error: {e}")
                    raise

                # Se nao e a ultima tentativa
                if attempt < self._config.max_attempts:
                    logger.warning(
                        f"Attempt {attempt}/{self._config.max_attempts} failed: {e}. "
                        f"Retrying in {delay:.2f}s"
                    )

                    if on_retry:
                        if asyncio.iscoroutinefunction(on_retry):
                            await on_retry(attempt_info)
                        else:
                            on_retry(attempt_info)

                    await asyncio.sleep(delay)
                    total_delay += delay

        # Todas as tentativas falharam
        retry_result = RetryResult(
            success=False,
            result=None,
            attempts=len(attempts),
            total_delay=total_delay,
            history=attempts,
            final_error=last_error
        )

        if on_failure:
            if asyncio.iscoroutinefunction(on_failure):
                await on_failure(retry_result)
            else:
                on_failure(retry_result)

        raise RetryExhausted(
            f"All {self._config.max_attempts} retry attempts failed",
            attempts=len(attempts),
            last_error=last_error
        )

    async def execute_with_result(
        self,
        func: Callable,
        *args,
        **kwargs
    ) -> RetryResult:
        """
        Executa funcao e retorna resultado detalhado (sem raise).

        Args:
            func: Funcao async a executar
            *args: Argumentos
            **kwargs: Argumentos nomeados

        Returns:
            RetryResult com detalhes da execucao
        """
        attempts = []
        total_delay = 0.0

        for attempt in range(1, self._config.max_attempts + 1):
            try:
                if asyncio.iscoroutinefunction(func):
                    result = await func(*args, **kwargs)
                else:
                    result = func(*args, **kwargs)

                return RetryResult(
                    success=True,
                    result=result,
                    attempts=attempt,
                    total_delay=total_delay,
                    history=attempts
                )

            except Exception as e:
                delay = self._calculate_delay(attempt)
                attempt_info = RetryAttempt(
                    attempt=attempt,
                    max_attempts=self._config.max_attempts,
                    delay=delay,
                    error=e
                )
                attempts.append(attempt_info)

                if not self._should_retry(e):
                    return RetryResult(
                        success=False,
                        result=None,
                        attempts=attempt,
                        total_delay=total_delay,
                        history=attempts,
                        final_error=e
                    )

                if attempt < self._config.max_attempts:
                    await asyncio.sleep(delay)
                    total_delay += delay

        return RetryResult(
            success=False,
            result=None,
            attempts=len(attempts),
            total_delay=total_delay,
            history=attempts,
            final_error=attempts[-1].error if attempts else None
        )


class RetryContext:
    """
    Context manager para retry.

    Exemplo:
        async with RetryContext(max_attempts=3) as ctx:
            for attempt in ctx:
                try:
                    result = await some_api_call()
                    break
                except Exception as e:
                    if not ctx.should_retry(e):
                        raise
    """

    def __init__(
        self,
        max_attempts: int = 3,
        initial_delay: float = 1.0,
        backoff_factor: float = 2.0,
        max_delay: float = 60.0,
        jitter: float = 0.1
    ):
        self._config = RetryConfig(
            max_attempts=max_attempts,
            initial_delay=initial_delay,
            backoff_factor=backoff_factor,
            max_delay=max_delay,
            jitter=jitter
        )
        self._retry = Retry(self._config)
        self._attempt = 0
        self._last_error: Optional[Exception] = None

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        pass

    def __iter__(self):
        self._attempt = 0
        return self

    def __next__(self) -> int:
        self._attempt += 1
        if self._attempt > self._config.max_attempts:
            raise StopIteration
        return self._attempt

    async def wait_before_retry(self):
        """Aguarda antes do proximo retry"""
        if self._attempt > 0:
            delay = self._retry._calculate_delay(self._attempt)
            await asyncio.sleep(delay)

    def should_retry(self, error: Exception) -> bool:
        """Verifica se deve fazer retry para o erro"""
        self._last_error = error
        return (
            self._attempt < self._config.max_attempts and
            self._retry._should_retry(error)
        )

    @property
    def attempt(self) -> int:
        return self._attempt

    @property
    def last_error(self) -> Optional[Exception]:
        return self._last_error


# HTTP status codes retriable
RETRIABLE_HTTP_CODES = {408, 429, 500, 502, 503, 504}


def is_retriable_http_error(error: Exception) -> bool:
    """Verifica se erro HTTP e retriable"""
    # Verifica se tem atributo status_code
    status_code = getattr(error, 'status_code', None)
    if status_code:
        return status_code in RETRIABLE_HTTP_CODES

    # Verifica se tem response.status_code (requests/httpx)
    response = getattr(error, 'response', None)
    if response:
        status_code = getattr(response, 'status_code', None)
        if status_code:
            return status_code in RETRIABLE_HTTP_CODES

    return False


# Presets de configuracao
RETRY_PRESETS: Dict[str, RetryConfig] = {
    "aggressive": RetryConfig(
        max_attempts=5,
        initial_delay=0.5,
        backoff_factor=2,
        max_delay=30,
        jitter=0.2
    ),
    "conservative": RetryConfig(
        max_attempts=3,
        initial_delay=2.0,
        backoff_factor=3,
        max_delay=60,
        jitter=0.1
    ),
    "quick": RetryConfig(
        max_attempts=2,
        initial_delay=0.1,
        backoff_factor=2,
        max_delay=5,
        jitter=0.05
    ),
    "patient": RetryConfig(
        max_attempts=10,
        initial_delay=1.0,
        backoff_factor=1.5,
        max_delay=120,
        jitter=0.3
    ),
}


def get_retry_preset(name: str) -> RetryConfig:
    """Obtem preset de retry por nome"""
    return RETRY_PRESETS.get(name, RetryConfig())
