# -*- coding: utf-8 -*-
"""
APM Dashboard Provider - Plataforma E
=====================================

Dashboard data aggregation for APM visualization.

Issue #450: Dashboard de Performance APM
"""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Tuple
from enum import Enum
from collections import defaultdict
from threading import Lock

from .apm import get_metrics_store, MetricsStore, RequestMetric


class AlertSeverity(str, Enum):
    """Alert severity levels."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"


class MetricType(str, Enum):
    """Types of metrics for time series."""
    REQUESTS = "requests"
    LATENCY = "latency"
    ERRORS = "errors"
    THROUGHPUT = "throughput"


@dataclass
class Alert:
    """Performance alert."""
    alert_id: str
    severity: AlertSeverity
    title: str
    description: str
    metric_name: str
    current_value: float
    threshold: float
    endpoint: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    acknowledged: bool = False

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "alert_id": self.alert_id,
            "severity": self.severity.value,
            "title": self.title,
            "description": self.description,
            "metric_name": self.metric_name,
            "current_value": round(self.current_value, 2),
            "threshold": self.threshold,
            "endpoint": self.endpoint,
            "created_at": self.created_at.isoformat(),
            "acknowledged": self.acknowledged,
        }


@dataclass
class TimeSeriesPoint:
    """A point in time series data."""
    timestamp: datetime
    value: float

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "timestamp": self.timestamp.isoformat(),
            "value": round(self.value, 2),
        }


@dataclass
class AlertThreshold:
    """Configuration for alert thresholds."""
    metric: str
    warning_threshold: float
    critical_threshold: float
    endpoint: Optional[str] = None  # None means global


class APMDashboard:
    """APM Dashboard data provider."""

    def __init__(self, metrics_store: Optional[MetricsStore] = None):
        self._store = metrics_store or get_metrics_store()
        self._lock = Lock()
        self._alerts: List[Alert] = []
        self._alert_counter = 0
        self._thresholds: List[AlertThreshold] = self._default_thresholds()
        self._time_series: Dict[str, List[TimeSeriesPoint]] = defaultdict(list)

    def _default_thresholds(self) -> List[AlertThreshold]:
        """Default alert thresholds."""
        return [
            AlertThreshold("latency_avg", warning_threshold=500, critical_threshold=1000),
            AlertThreshold("latency_p95", warning_threshold=1000, critical_threshold=2000),
            AlertThreshold("latency_p99", warning_threshold=2000, critical_threshold=5000),
            AlertThreshold("error_rate", warning_threshold=1.0, critical_threshold=5.0),
        ]

    # Dashboard Overview

    def get_overview(self) -> Dict[str, Any]:
        """Get dashboard overview data."""
        summary = self._store.get_summary()
        stats = self._store.get_stats()

        # Calculate health score (0-100)
        health_score = self._calculate_health_score(summary, stats)

        # Get trend indicators
        latency_trend = self._get_trend("latency")
        error_trend = self._get_trend("errors")

        return {
            "health_score": health_score,
            "total_requests": summary["total_requests"],
            "total_errors": summary["total_errors"],
            "error_rate": summary["error_rate"],
            "avg_latency_ms": summary["avg_duration_ms"],
            "endpoint_count": summary["endpoint_count"],
            "latency_trend": latency_trend,
            "error_trend": error_trend,
            "active_alerts": len([a for a in self._alerts if not a.acknowledged]),
            "last_updated": datetime.utcnow().isoformat(),
        }

    def _calculate_health_score(
        self,
        summary: Dict[str, Any],
        stats: List[Dict[str, Any]],
    ) -> int:
        """Calculate overall health score 0-100."""
        score = 100

        # Deduct for error rate
        error_rate = summary.get("error_rate", 0)
        if error_rate > 5:
            score -= 30
        elif error_rate > 1:
            score -= 15
        elif error_rate > 0.1:
            score -= 5

        # Deduct for high latency
        avg_latency = summary.get("avg_duration_ms", 0)
        if avg_latency > 2000:
            score -= 25
        elif avg_latency > 1000:
            score -= 15
        elif avg_latency > 500:
            score -= 5

        # Deduct for slow endpoints
        slow_count = len([s for s in stats if s.get("avg_duration_ms", 0) > 1000])
        score -= min(slow_count * 5, 20)

        return max(0, score)

    def _get_trend(self, metric_type: str) -> str:
        """Get trend direction for a metric."""
        series = self._time_series.get(metric_type, [])
        if len(series) < 2:
            return "stable"

        recent = series[-5:]
        if len(recent) < 2:
            return "stable"

        first_avg = sum(p.value for p in recent[:len(recent)//2]) / max(len(recent)//2, 1)
        last_avg = sum(p.value for p in recent[len(recent)//2:]) / max(len(recent) - len(recent)//2, 1)

        if first_avg == 0:
            return "stable"

        change_pct = ((last_avg - first_avg) / first_avg) * 100

        if change_pct > 10:
            return "increasing"
        elif change_pct < -10:
            return "decreasing"
        return "stable"

    # Time Series Data

    def get_time_series(
        self,
        metric_type: MetricType,
        duration_minutes: int = 60,
        interval_seconds: int = 60,
    ) -> Dict[str, Any]:
        """Get time series data for charts."""
        # Generate time buckets
        now = datetime.utcnow()
        buckets: List[Dict[str, Any]] = []
        bucket_count = duration_minutes * 60 // interval_seconds

        for i in range(bucket_count):
            bucket_time = now - timedelta(seconds=interval_seconds * (bucket_count - i - 1))
            buckets.append({
                "timestamp": bucket_time.isoformat(),
                "label": bucket_time.strftime("%H:%M"),
                "value": 0,
            })

        # Get recent metrics
        recent = self._store.get_recent_metrics(duration_minutes)

        # Aggregate into buckets based on metric type
        for metric in recent:
            metric_time = datetime.fromisoformat(metric["timestamp"])
            for i, bucket in enumerate(buckets):
                bucket_time = datetime.fromisoformat(bucket["timestamp"])
                next_time = (
                    datetime.fromisoformat(buckets[i + 1]["timestamp"])
                    if i + 1 < len(buckets)
                    else now
                )
                if bucket_time <= metric_time < next_time:
                    if metric_type == MetricType.REQUESTS:
                        bucket["value"] += 1
                    elif metric_type == MetricType.LATENCY:
                        if bucket.get("_count", 0) == 0:
                            bucket["value"] = metric["duration_ms"]
                            bucket["_count"] = 1
                        else:
                            bucket["value"] = (
                                bucket["value"] * bucket["_count"] + metric["duration_ms"]
                            ) / (bucket["_count"] + 1)
                            bucket["_count"] += 1
                    elif metric_type == MetricType.ERRORS:
                        if metric.get("error"):
                            bucket["value"] += 1
                    elif metric_type == MetricType.THROUGHPUT:
                        bucket["value"] += 1
                    break

        # Clean up internal fields
        for bucket in buckets:
            bucket.pop("_count", None)

        return {
            "metric_type": metric_type.value,
            "duration_minutes": duration_minutes,
            "interval_seconds": interval_seconds,
            "labels": [b["label"] for b in buckets],
            "data": [round(b["value"], 2) for b in buckets],
            "points": buckets,
        }

    def record_time_series_point(self, metric_type: str, value: float) -> None:
        """Record a time series point for trend tracking."""
        with self._lock:
            point = TimeSeriesPoint(timestamp=datetime.utcnow(), value=value)
            self._time_series[metric_type].append(point)

            # Keep only last 100 points
            if len(self._time_series[metric_type]) > 100:
                self._time_series[metric_type] = self._time_series[metric_type][-100:]

    # Endpoint Performance

    def get_endpoint_performance(
        self,
        sort_by: str = "avg_duration_ms",
        limit: int = 20,
        descending: bool = True,
    ) -> List[Dict[str, Any]]:
        """Get endpoint performance data."""
        stats = self._store.get_stats()

        # Add performance grade
        for stat in stats:
            stat["grade"] = self._calculate_endpoint_grade(stat)

        # Sort
        stats.sort(key=lambda x: x.get(sort_by, 0), reverse=descending)

        return stats[:limit]

    def _calculate_endpoint_grade(self, stat: Dict[str, Any]) -> str:
        """Calculate performance grade A-F."""
        score = 100

        # Latency impact
        avg = stat.get("avg_duration_ms", 0)
        if avg > 2000:
            score -= 40
        elif avg > 1000:
            score -= 25
        elif avg > 500:
            score -= 10

        # Error rate impact
        error_rate = stat.get("error_rate", 0)
        if error_rate > 5:
            score -= 40
        elif error_rate > 1:
            score -= 20
        elif error_rate > 0.1:
            score -= 10

        # p95 impact
        p95 = stat.get("p95_duration_ms", 0)
        if p95 > 3000:
            score -= 20
        elif p95 > 2000:
            score -= 10

        if score >= 90:
            return "A"
        elif score >= 80:
            return "B"
        elif score >= 70:
            return "C"
        elif score >= 60:
            return "D"
        return "F"

    def get_top_endpoints(self, by: str = "requests", limit: int = 10) -> List[Dict[str, Any]]:
        """Get top endpoints by various metrics."""
        stats = self._store.get_stats()

        sort_field = {
            "requests": "request_count",
            "latency": "avg_duration_ms",
            "errors": "error_count",
            "p95": "p95_duration_ms",
        }.get(by, "request_count")

        stats.sort(key=lambda x: x.get(sort_field, 0), reverse=True)

        return stats[:limit]

    # Alerts

    def check_alerts(self) -> List[Alert]:
        """Check thresholds and generate alerts."""
        summary = self._store.get_summary()
        stats = self._store.get_stats()
        new_alerts: List[Alert] = []

        # Check global thresholds
        for threshold in self._thresholds:
            if threshold.endpoint:
                continue  # Skip endpoint-specific thresholds here

            current_value = self._get_metric_value(threshold.metric, summary, None)

            if current_value >= threshold.critical_threshold:
                alert = self._create_alert(
                    AlertSeverity.CRITICAL,
                    threshold.metric,
                    current_value,
                    threshold.critical_threshold,
                )
                new_alerts.append(alert)
            elif current_value >= threshold.warning_threshold:
                alert = self._create_alert(
                    AlertSeverity.WARNING,
                    threshold.metric,
                    current_value,
                    threshold.warning_threshold,
                )
                new_alerts.append(alert)

        # Check per-endpoint thresholds
        for stat in stats:
            endpoint = stat.get("endpoint")
            for threshold in self._thresholds:
                if threshold.endpoint and threshold.endpoint != endpoint:
                    continue

                current_value = self._get_metric_value(threshold.metric, summary, stat)

                if current_value >= threshold.critical_threshold:
                    alert = self._create_alert(
                        AlertSeverity.CRITICAL,
                        threshold.metric,
                        current_value,
                        threshold.critical_threshold,
                        endpoint,
                    )
                    new_alerts.append(alert)
                elif current_value >= threshold.warning_threshold:
                    alert = self._create_alert(
                        AlertSeverity.WARNING,
                        threshold.metric,
                        current_value,
                        threshold.warning_threshold,
                        endpoint,
                    )
                    new_alerts.append(alert)

        with self._lock:
            self._alerts.extend(new_alerts)

        return new_alerts

    def _get_metric_value(
        self,
        metric: str,
        summary: Dict[str, Any],
        stat: Optional[Dict[str, Any]],
    ) -> float:
        """Get current value for a metric."""
        if stat:
            mapping = {
                "latency_avg": "avg_duration_ms",
                "latency_p95": "p95_duration_ms",
                "latency_p99": "p99_duration_ms",
                "error_rate": "error_rate",
            }
            return stat.get(mapping.get(metric, metric), 0)

        mapping = {
            "latency_avg": "avg_duration_ms",
            "error_rate": "error_rate",
        }
        return summary.get(mapping.get(metric, metric), 0)

    def _create_alert(
        self,
        severity: AlertSeverity,
        metric: str,
        value: float,
        threshold: float,
        endpoint: Optional[str] = None,
    ) -> Alert:
        """Create a new alert."""
        with self._lock:
            self._alert_counter += 1
            alert_id = f"ALR-{self._alert_counter:04d}"

        metric_names = {
            "latency_avg": "Average Latency",
            "latency_p95": "P95 Latency",
            "latency_p99": "P99 Latency",
            "error_rate": "Error Rate",
        }

        title = f"{severity.value.upper()}: {metric_names.get(metric, metric)}"
        if endpoint:
            title += f" on {endpoint}"

        description = f"{metric_names.get(metric, metric)} is {value:.2f}, exceeding threshold of {threshold}"

        return Alert(
            alert_id=alert_id,
            severity=severity,
            title=title,
            description=description,
            metric_name=metric,
            current_value=value,
            threshold=threshold,
            endpoint=endpoint,
        )

    def get_alerts(self, include_acknowledged: bool = False) -> List[Dict[str, Any]]:
        """Get all alerts."""
        with self._lock:
            alerts = self._alerts
            if not include_acknowledged:
                alerts = [a for a in alerts if not a.acknowledged]
            return [a.to_dict() for a in alerts]

    def acknowledge_alert(self, alert_id: str) -> bool:
        """Acknowledge an alert."""
        with self._lock:
            for alert in self._alerts:
                if alert.alert_id == alert_id:
                    alert.acknowledged = True
                    return True
        return False

    def add_threshold(
        self,
        metric: str,
        warning: float,
        critical: float,
        endpoint: Optional[str] = None,
    ) -> None:
        """Add or update alert threshold."""
        with self._lock:
            # Remove existing threshold for same metric/endpoint
            self._thresholds = [
                t for t in self._thresholds
                if not (t.metric == metric and t.endpoint == endpoint)
            ]
            self._thresholds.append(AlertThreshold(
                metric=metric,
                warning_threshold=warning,
                critical_threshold=critical,
                endpoint=endpoint,
            ))

    # Chart Data (Chart.js format)

    def get_latency_chart_data(self, duration_minutes: int = 60) -> Dict[str, Any]:
        """Get latency chart data for Chart.js."""
        series = self.get_time_series(MetricType.LATENCY, duration_minutes)

        return {
            "labels": series["labels"],
            "datasets": [
                {
                    "label": "Average Latency (ms)",
                    "data": series["data"],
                    "borderColor": "#3B82F6",
                    "backgroundColor": "rgba(59, 130, 246, 0.1)",
                    "fill": True,
                    "tension": 0.4,
                }
            ],
        }

    def get_requests_chart_data(self, duration_minutes: int = 60) -> Dict[str, Any]:
        """Get requests chart data for Chart.js."""
        requests = self.get_time_series(MetricType.REQUESTS, duration_minutes)
        errors = self.get_time_series(MetricType.ERRORS, duration_minutes)

        return {
            "labels": requests["labels"],
            "datasets": [
                {
                    "label": "Requests",
                    "data": requests["data"],
                    "borderColor": "#10B981",
                    "backgroundColor": "rgba(16, 185, 129, 0.1)",
                    "fill": True,
                },
                {
                    "label": "Errors",
                    "data": errors["data"],
                    "borderColor": "#EF4444",
                    "backgroundColor": "rgba(239, 68, 68, 0.1)",
                    "fill": True,
                },
            ],
        }

    def get_endpoint_comparison_chart(self, limit: int = 10) -> Dict[str, Any]:
        """Get endpoint comparison bar chart data."""
        endpoints = self.get_top_endpoints("latency", limit)

        return {
            "labels": [e["endpoint"][:30] for e in endpoints],
            "datasets": [
                {
                    "label": "Avg Latency (ms)",
                    "data": [e["avg_duration_ms"] for e in endpoints],
                    "backgroundColor": "#3B82F6",
                },
                {
                    "label": "P95 Latency (ms)",
                    "data": [e["p95_duration_ms"] for e in endpoints],
                    "backgroundColor": "#F59E0B",
                },
            ],
        }

    def get_error_distribution_chart(self) -> Dict[str, Any]:
        """Get error distribution pie chart data."""
        stats = self._store.get_stats()
        error_endpoints = [s for s in stats if s.get("error_count", 0) > 0]
        error_endpoints.sort(key=lambda x: x["error_count"], reverse=True)
        top_errors = error_endpoints[:5]

        return {
            "labels": [e["endpoint"][:20] for e in top_errors],
            "datasets": [
                {
                    "data": [e["error_count"] for e in top_errors],
                    "backgroundColor": [
                        "#EF4444",
                        "#F97316",
                        "#F59E0B",
                        "#EAB308",
                        "#84CC16",
                    ],
                }
            ],
        }

    # Full Dashboard Data

    def get_dashboard_data(self) -> Dict[str, Any]:
        """Get all dashboard data in one call."""
        return {
            "overview": self.get_overview(),
            "latency_chart": self.get_latency_chart_data(),
            "requests_chart": self.get_requests_chart_data(),
            "endpoint_comparison": self.get_endpoint_comparison_chart(),
            "error_distribution": self.get_error_distribution_chart(),
            "top_endpoints": self.get_endpoint_performance(limit=10),
            "slow_endpoints": self._store.get_slow_endpoints(limit=5),
            "error_endpoints": self._store.get_error_endpoints(limit=5),
            "alerts": self.get_alerts(),
            "generated_at": datetime.utcnow().isoformat(),
        }


# Singleton instance
_dashboard: Optional[APMDashboard] = None


def get_apm_dashboard() -> APMDashboard:
    """Get global APM dashboard instance."""
    global _dashboard
    if _dashboard is None:
        _dashboard = APMDashboard()
    return _dashboard
