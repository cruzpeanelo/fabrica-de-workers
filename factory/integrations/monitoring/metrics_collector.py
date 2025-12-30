# -*- coding: utf-8 -*-
"""
Metrics Collector
=================
Sistema de coleta de metricas para integracoes.

Issue #333 - Terminal A

Metricas coletadas:
- Latencia (min, max, avg, p95, p99)
- Taxa de erro
- Requests por minuto
- Tempo de uptime
"""

import asyncio
import logging
import statistics
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from threading import Lock
from typing import Any, Deque, Dict, List, Optional

logger = logging.getLogger(__name__)


class MetricType(str, Enum):
    """Tipos de metricas"""
    LATENCY = "latency"              # Latencia em ms
    ERROR_RATE = "error_rate"        # Taxa de erro (0-1)
    REQUEST_COUNT = "request_count"  # Numero de requests
    SUCCESS_COUNT = "success_count"  # Requests bem sucedidos
    FAILURE_COUNT = "failure_count"  # Requests com falha
    UPTIME = "uptime"                # Tempo de uptime em segundos


@dataclass
class MetricPoint:
    """Ponto de metrica com timestamp"""
    value: float
    timestamp: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class IntegrationMetrics:
    """Metricas agregadas de uma integracao"""
    name: str
    period_start: datetime
    period_end: datetime

    # Latencia
    latency_avg_ms: float = 0.0
    latency_min_ms: float = 0.0
    latency_max_ms: float = 0.0
    latency_p95_ms: float = 0.0
    latency_p99_ms: float = 0.0

    # Requests
    total_requests: int = 0
    successful_requests: int = 0
    failed_requests: int = 0
    requests_per_minute: float = 0.0

    # Taxa de erro
    error_rate: float = 0.0

    # Uptime
    uptime_percentage: float = 0.0
    last_downtime: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "period": {
                "start": self.period_start.isoformat(),
                "end": self.period_end.isoformat()
            },
            "latency": {
                "avg_ms": self.latency_avg_ms,
                "min_ms": self.latency_min_ms,
                "max_ms": self.latency_max_ms,
                "p95_ms": self.latency_p95_ms,
                "p99_ms": self.latency_p99_ms
            },
            "requests": {
                "total": self.total_requests,
                "successful": self.successful_requests,
                "failed": self.failed_requests,
                "per_minute": self.requests_per_minute
            },
            "error_rate": self.error_rate,
            "uptime": {
                "percentage": self.uptime_percentage,
                "last_downtime": self.last_downtime.isoformat() if self.last_downtime else None
            }
        }


class MetricsCollector:
    """
    Coletor de metricas para integracoes.

    Exemplo:
        collector = MetricsCollector()

        # Registrar request
        collector.record_request("jira", latency_ms=150, success=True)

        # Obter metricas
        metrics = collector.get_metrics("jira")

        # Obter metricas agregadas
        all_metrics = collector.get_aggregated_metrics()
    """

    # Tamanho maximo do buffer de metricas
    MAX_BUFFER_SIZE = 10000

    # Retencao de metricas (1 hora por padrao)
    DEFAULT_RETENTION_SECONDS = 3600

    def __init__(
        self,
        retention_seconds: int = DEFAULT_RETENTION_SECONDS,
        buffer_size: int = MAX_BUFFER_SIZE
    ):
        """
        Inicializa o coletor.

        Args:
            retention_seconds: Tempo de retencao de metricas em segundos
            buffer_size: Tamanho maximo do buffer por integracao
        """
        self.retention_seconds = retention_seconds
        self.buffer_size = buffer_size

        self._metrics: Dict[str, Dict[MetricType, Deque[MetricPoint]]] = {}
        self._uptime_start: Dict[str, datetime] = {}
        self._last_failure: Dict[str, datetime] = {}
        self._lock = Lock()

    def _ensure_integration(self, name: str):
        """Garante que estrutura de metricas existe para integracao"""
        with self._lock:
            if name not in self._metrics:
                self._metrics[name] = {
                    metric_type: deque(maxlen=self.buffer_size)
                    for metric_type in MetricType
                }
                self._uptime_start[name] = datetime.utcnow()

    def record_request(
        self,
        integration_name: str,
        latency_ms: float,
        success: bool,
        metadata: Optional[Dict] = None
    ):
        """
        Registra uma requisicao.

        Args:
            integration_name: Nome da integracao
            latency_ms: Latencia em milissegundos
            success: Se a requisicao foi bem sucedida
            metadata: Metadados adicionais
        """
        self._ensure_integration(integration_name)

        now = datetime.utcnow()
        metrics = self._metrics[integration_name]

        # Latencia
        metrics[MetricType.LATENCY].append(
            MetricPoint(value=latency_ms, metadata=metadata or {})
        )

        # Contadores
        metrics[MetricType.REQUEST_COUNT].append(
            MetricPoint(value=1)
        )

        if success:
            metrics[MetricType.SUCCESS_COUNT].append(MetricPoint(value=1))
        else:
            metrics[MetricType.FAILURE_COUNT].append(MetricPoint(value=1))
            self._last_failure[integration_name] = now

    def record_error(
        self,
        integration_name: str,
        error: str,
        metadata: Optional[Dict] = None
    ):
        """
        Registra um erro.

        Args:
            integration_name: Nome da integracao
            error: Mensagem de erro
            metadata: Metadados adicionais
        """
        self._ensure_integration(integration_name)

        now = datetime.utcnow()
        meta = metadata or {}
        meta["error"] = error

        self._metrics[integration_name][MetricType.FAILURE_COUNT].append(
            MetricPoint(value=1, metadata=meta)
        )
        self._last_failure[integration_name] = now

    def get_metrics(
        self,
        integration_name: str,
        period_seconds: Optional[int] = None
    ) -> IntegrationMetrics:
        """
        Obtem metricas de uma integracao.

        Args:
            integration_name: Nome da integracao
            period_seconds: Periodo em segundos (padrao: retention)

        Returns:
            Metricas agregadas
        """
        self._ensure_integration(integration_name)

        period = period_seconds or self.retention_seconds
        cutoff = datetime.utcnow() - timedelta(seconds=period)
        now = datetime.utcnow()

        metrics = self._metrics[integration_name]

        # Filtra metricas pelo periodo
        latencies = [
            p.value for p in metrics[MetricType.LATENCY]
            if p.timestamp > cutoff
        ]
        requests = sum(
            1 for p in metrics[MetricType.REQUEST_COUNT]
            if p.timestamp > cutoff
        )
        successes = sum(
            1 for p in metrics[MetricType.SUCCESS_COUNT]
            if p.timestamp > cutoff
        )
        failures = sum(
            1 for p in metrics[MetricType.FAILURE_COUNT]
            if p.timestamp > cutoff
        )

        # Calcula estatisticas de latencia
        if latencies:
            sorted_latencies = sorted(latencies)
            latency_avg = statistics.mean(latencies)
            latency_min = min(latencies)
            latency_max = max(latencies)
            latency_p95 = sorted_latencies[int(len(sorted_latencies) * 0.95)] if len(sorted_latencies) >= 20 else latency_max
            latency_p99 = sorted_latencies[int(len(sorted_latencies) * 0.99)] if len(sorted_latencies) >= 100 else latency_max
        else:
            latency_avg = latency_min = latency_max = latency_p95 = latency_p99 = 0.0

        # Calcula taxa de erro
        error_rate = failures / requests if requests > 0 else 0.0

        # Calcula requests por minuto
        period_minutes = period / 60
        requests_per_minute = requests / period_minutes if period_minutes > 0 else 0

        # Calcula uptime
        uptime_start = self._uptime_start.get(integration_name, now)
        total_uptime = (now - uptime_start).total_seconds()
        last_failure = self._last_failure.get(integration_name)

        # Estima uptime baseado em falhas
        if failures == 0:
            uptime_percentage = 100.0
        else:
            # Assume 30s de downtime por falha
            estimated_downtime = failures * 30
            uptime_percentage = max(0, (total_uptime - estimated_downtime) / total_uptime * 100)

        return IntegrationMetrics(
            name=integration_name,
            period_start=cutoff,
            period_end=now,
            latency_avg_ms=latency_avg,
            latency_min_ms=latency_min,
            latency_max_ms=latency_max,
            latency_p95_ms=latency_p95,
            latency_p99_ms=latency_p99,
            total_requests=requests,
            successful_requests=successes,
            failed_requests=failures,
            requests_per_minute=requests_per_minute,
            error_rate=error_rate,
            uptime_percentage=uptime_percentage,
            last_downtime=last_failure
        )

    def get_aggregated_metrics(
        self,
        period_seconds: Optional[int] = None
    ) -> Dict[str, IntegrationMetrics]:
        """
        Obtem metricas de todas as integracoes.

        Args:
            period_seconds: Periodo em segundos

        Returns:
            Dicionario com metricas por integracao
        """
        return {
            name: self.get_metrics(name, period_seconds)
            for name in self._metrics.keys()
        }

    def cleanup_old_metrics(self):
        """Remove metricas antigas"""
        cutoff = datetime.utcnow() - timedelta(seconds=self.retention_seconds)

        with self._lock:
            for integration_metrics in self._metrics.values():
                for metric_type in MetricType:
                    buffer = integration_metrics[metric_type]
                    # Remove metricas antigas (do inicio da deque)
                    while buffer and buffer[0].timestamp < cutoff:
                        buffer.popleft()

    def reset(self, integration_name: Optional[str] = None):
        """
        Reseta metricas.

        Args:
            integration_name: Nome da integracao (None para todas)
        """
        with self._lock:
            if integration_name:
                if integration_name in self._metrics:
                    for metric_type in MetricType:
                        self._metrics[integration_name][metric_type].clear()
                    self._uptime_start[integration_name] = datetime.utcnow()
            else:
                self._metrics.clear()
                self._uptime_start.clear()
                self._last_failure.clear()


# Instancia global
metrics_collector = MetricsCollector()


def record_integration_request(
    integration_name: str,
    latency_ms: float,
    success: bool,
    metadata: Optional[Dict] = None
):
    """Helper para registrar request na instancia global"""
    metrics_collector.record_request(integration_name, latency_ms, success, metadata)


def get_integration_metrics(
    integration_name: str,
    period_seconds: Optional[int] = None
) -> IntegrationMetrics:
    """Helper para obter metricas da instancia global"""
    return metrics_collector.get_metrics(integration_name, period_seconds)
