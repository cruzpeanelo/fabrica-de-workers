# -*- coding: utf-8 -*-
"""
Monitoring Module - Plataforma E
================================

APM and monitoring functionality.

Issues:
- #444: APM - Application Performance Monitoring
- #450: Dashboard de Performance APM
"""

from .apm import (
    RequestMetric,
    EndpointStats,
    MetricsStore,
    APMMiddleware,
    get_metrics_store,
    timed,
)
from .apm_dashboard import (
    APMDashboard,
    Alert,
    AlertSeverity,
    AlertThreshold,
    MetricType,
    TimeSeriesPoint,
    get_apm_dashboard,
)

__all__ = [
    # APM Core
    "RequestMetric",
    "EndpointStats",
    "MetricsStore",
    "APMMiddleware",
    "get_metrics_store",
    "timed",
    # APM Dashboard
    "APMDashboard",
    "Alert",
    "AlertSeverity",
    "AlertThreshold",
    "MetricType",
    "TimeSeriesPoint",
    "get_apm_dashboard",
]
