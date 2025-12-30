# -*- coding: utf-8 -*-
"""
Integration Monitoring Module
=============================
Sistema de health check e monitoring para integracoes.

Issue #333 - Terminal A

Funcionalidades:
- Health Check: Verifica status de cada integracao
- Metrics Collector: Coleta metricas de latencia, erros, requests
- Alerts: Sistema de alertas para falhas
- Circuit Breaker: Retry automatico com protecao

Uso:
    from factory.integrations.monitoring import (
        HealthChecker,
        MetricsCollector,
        AlertManager,
        CircuitBreaker
    )

    # Health check
    checker = HealthChecker()
    status = await checker.check_all()

    # Metricas
    collector = MetricsCollector()
    metrics = collector.get_aggregated_metrics()

    # Alertas
    alert_manager = AlertManager()
    alert_manager.on_integration_failure("jira", error)
"""

from .health_checker import (
    HealthChecker,
    HealthStatus,
    IntegrationHealth,
    HealthCheckResult
)
from .metrics_collector import (
    MetricsCollector,
    IntegrationMetrics,
    MetricType
)
from .alerts import (
    AlertManager,
    Alert,
    AlertLevel,
    AlertHandler
)
from .circuit_breaker import (
    CircuitBreaker,
    CircuitState
)

__all__ = [
    # Health Check
    "HealthChecker",
    "HealthStatus",
    "IntegrationHealth",
    "HealthCheckResult",
    # Metrics
    "MetricsCollector",
    "IntegrationMetrics",
    "MetricType",
    # Alerts
    "AlertManager",
    "Alert",
    "AlertLevel",
    "AlertHandler",
    # Circuit Breaker
    "CircuitBreaker",
    "CircuitState"
]

__version__ = "1.0.0"
