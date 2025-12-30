# -*- coding: utf-8 -*-
"""
Circuit Breaker
===============
Implementacao do padrao Circuit Breaker para integracoes.

Issue #333 - Terminal A

Estados:
- CLOSED: Circuito fechado, operacao normal
- OPEN: Circuito aberto, requisicoes bloqueadas
- HALF_OPEN: Testando recuperacao

Referencias:
- https://martinfowler.com/bliki/CircuitBreaker.html
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, Optional, TypeVar, Generic

logger = logging.getLogger(__name__)

T = TypeVar('T')


class CircuitState(str, Enum):
    """Estados do circuit breaker"""
    CLOSED = "closed"       # Operacao normal
    OPEN = "open"           # Bloqueando requisicoes
    HALF_OPEN = "half_open" # Testando recuperacao


class CircuitOpenError(Exception):
    """Erro quando circuito esta aberto"""

    def __init__(self, circuit_name: str, retry_after: datetime):
        self.circuit_name = circuit_name
        self.retry_after = retry_after
        super().__init__(
            f"Circuit {circuit_name} is open. Retry after {retry_after}"
        )


@dataclass
class CircuitStats:
    """Estatisticas do circuit breaker"""
    total_calls: int = 0
    successful_calls: int = 0
    failed_calls: int = 0
    consecutive_failures: int = 0
    last_failure_time: Optional[datetime] = None
    last_success_time: Optional[datetime] = None
    state_changed_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "total_calls": self.total_calls,
            "successful_calls": self.successful_calls,
            "failed_calls": self.failed_calls,
            "consecutive_failures": self.consecutive_failures,
            "last_failure_time": self.last_failure_time.isoformat() if self.last_failure_time else None,
            "last_success_time": self.last_success_time.isoformat() if self.last_success_time else None,
            "state_changed_at": self.state_changed_at.isoformat()
        }


class CircuitBreaker:
    """
    Implementacao do padrao Circuit Breaker.

    O circuit breaker monitora falhas e bloqueia requisicoes
    quando o limite de falhas e atingido, permitindo que o
    servico se recupere.

    Estados:
    - CLOSED: Operacao normal. Conta falhas.
    - OPEN: Requisicoes bloqueadas. Aguarda timeout.
    - HALF_OPEN: Permite uma requisicao de teste.

    Exemplo:
        breaker = CircuitBreaker("jira", failure_threshold=5)

        # Uso com decorator
        @breaker
        async def call_jira():
            ...

        # Uso manual
        async with breaker:
            await call_jira()

        # Verificar estado
        if breaker.is_closed:
            ...
    """

    def __init__(
        self,
        name: str,
        failure_threshold: int = 5,
        recovery_timeout_seconds: int = 30,
        half_open_max_calls: int = 1,
        exclude_exceptions: tuple = ()
    ):
        """
        Inicializa o circuit breaker.

        Args:
            name: Nome do circuito
            failure_threshold: Numero de falhas para abrir circuito
            recovery_timeout_seconds: Tempo de espera antes de tentar recuperar
            half_open_max_calls: Chamadas permitidas em half-open
            exclude_exceptions: Excecoes que nao contam como falha
        """
        self.name = name
        self.failure_threshold = failure_threshold
        self.recovery_timeout = timedelta(seconds=recovery_timeout_seconds)
        self.half_open_max_calls = half_open_max_calls
        self.exclude_exceptions = exclude_exceptions

        self._state = CircuitState.CLOSED
        self._stats = CircuitStats()
        self._half_open_calls = 0
        self._lock = asyncio.Lock()

    @property
    def state(self) -> CircuitState:
        """Estado atual do circuito"""
        return self._state

    @property
    def is_closed(self) -> bool:
        return self._state == CircuitState.CLOSED

    @property
    def is_open(self) -> bool:
        return self._state == CircuitState.OPEN

    @property
    def is_half_open(self) -> bool:
        return self._state == CircuitState.HALF_OPEN

    @property
    def stats(self) -> CircuitStats:
        return self._stats

    def _should_allow_request(self) -> bool:
        """Verifica se requisicao deve ser permitida"""
        if self._state == CircuitState.CLOSED:
            return True

        if self._state == CircuitState.OPEN:
            # Verifica se timeout de recuperacao passou
            if self._stats.last_failure_time:
                time_since_failure = datetime.utcnow() - self._stats.last_failure_time
                if time_since_failure >= self.recovery_timeout:
                    self._transition_to(CircuitState.HALF_OPEN)
                    return True
            return False

        if self._state == CircuitState.HALF_OPEN:
            return self._half_open_calls < self.half_open_max_calls

        return False

    def _transition_to(self, new_state: CircuitState):
        """Transiciona para novo estado"""
        old_state = self._state
        self._state = new_state
        self._stats.state_changed_at = datetime.utcnow()

        if new_state == CircuitState.HALF_OPEN:
            self._half_open_calls = 0

        logger.info(
            f"Circuit {self.name}: {old_state.value} -> {new_state.value}"
        )

    def _record_success(self):
        """Registra sucesso"""
        self._stats.total_calls += 1
        self._stats.successful_calls += 1
        self._stats.consecutive_failures = 0
        self._stats.last_success_time = datetime.utcnow()

        if self._state == CircuitState.HALF_OPEN:
            # Recuperacao bem sucedida
            self._transition_to(CircuitState.CLOSED)

    def _record_failure(self, exception: Exception):
        """Registra falha"""
        # Verifica se excecao deve ser ignorada
        if isinstance(exception, self.exclude_exceptions):
            return

        self._stats.total_calls += 1
        self._stats.failed_calls += 1
        self._stats.consecutive_failures += 1
        self._stats.last_failure_time = datetime.utcnow()

        if self._state == CircuitState.HALF_OPEN:
            # Falha durante teste de recuperacao
            self._transition_to(CircuitState.OPEN)

        elif self._state == CircuitState.CLOSED:
            if self._stats.consecutive_failures >= self.failure_threshold:
                self._transition_to(CircuitState.OPEN)

    async def __aenter__(self):
        """Context manager async"""
        async with self._lock:
            if not self._should_allow_request():
                retry_after = self._stats.last_failure_time + self.recovery_timeout
                raise CircuitOpenError(self.name, retry_after)

            if self._state == CircuitState.HALF_OPEN:
                self._half_open_calls += 1

        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Saida do context manager"""
        async with self._lock:
            if exc_type is None:
                self._record_success()
            else:
                self._record_failure(exc_val)

        return False  # Nao suprime excecao

    def __call__(self, func: Callable) -> Callable:
        """Decorator para funcoes async"""
        @wraps(func)
        async def wrapper(*args, **kwargs):
            async with self:
                return await func(*args, **kwargs)
        return wrapper

    def reset(self):
        """Reseta o circuit breaker"""
        self._state = CircuitState.CLOSED
        self._stats = CircuitStats()
        self._half_open_calls = 0
        logger.info(f"Circuit {self.name} reset")

    def force_open(self):
        """Forca abertura do circuito"""
        self._transition_to(CircuitState.OPEN)
        self._stats.last_failure_time = datetime.utcnow()

    def force_close(self):
        """Forca fechamento do circuito"""
        self._transition_to(CircuitState.CLOSED)
        self._stats.consecutive_failures = 0

    def get_status(self) -> Dict[str, Any]:
        """Obtem status completo do circuit breaker"""
        return {
            "name": self.name,
            "state": self._state.value,
            "failure_threshold": self.failure_threshold,
            "recovery_timeout_seconds": self.recovery_timeout.total_seconds(),
            "stats": self._stats.to_dict()
        }


class CircuitBreakerRegistry:
    """
    Registro de circuit breakers.

    Permite gerenciar multiplos circuit breakers.

    Exemplo:
        registry = CircuitBreakerRegistry()

        # Obter ou criar circuit breaker
        jira_breaker = registry.get_or_create("jira")

        # Obter status de todos
        status = registry.get_all_status()
    """

    def __init__(self, default_failure_threshold: int = 5, default_timeout: int = 30):
        self.default_failure_threshold = default_failure_threshold
        self.default_timeout = default_timeout
        self._breakers: Dict[str, CircuitBreaker] = {}

    def get_or_create(
        self,
        name: str,
        failure_threshold: Optional[int] = None,
        recovery_timeout_seconds: Optional[int] = None
    ) -> CircuitBreaker:
        """Obtem ou cria circuit breaker"""
        if name not in self._breakers:
            self._breakers[name] = CircuitBreaker(
                name=name,
                failure_threshold=failure_threshold or self.default_failure_threshold,
                recovery_timeout_seconds=recovery_timeout_seconds or self.default_timeout
            )
        return self._breakers[name]

    def get(self, name: str) -> Optional[CircuitBreaker]:
        """Obtem circuit breaker existente"""
        return self._breakers.get(name)

    def get_all_status(self) -> Dict[str, Dict]:
        """Obtem status de todos os circuit breakers"""
        return {
            name: breaker.get_status()
            for name, breaker in self._breakers.items()
        }

    def reset_all(self):
        """Reseta todos os circuit breakers"""
        for breaker in self._breakers.values():
            breaker.reset()


# Registro global
circuit_registry = CircuitBreakerRegistry()


def get_circuit_breaker(name: str) -> CircuitBreaker:
    """Helper para obter circuit breaker do registro global"""
    return circuit_registry.get_or_create(name)
