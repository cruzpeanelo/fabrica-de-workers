# -*- coding: utf-8 -*-
"""
Metrics Collector - Plataforma E
================================

Prometheus-compatible metrics collection.

Issue #442: Health Check Endpoints Avancados
"""

import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from threading import Lock
from typing import Any, Dict, List, Optional


class MetricType(str, Enum):
    """Types of metrics."""
    COUNTER = "counter"
    GAUGE = "gauge"
    HISTOGRAM = "histogram"
    SUMMARY = "summary"


@dataclass
class Metric:
    """A single metric with labels."""
    name: str
    type: MetricType
    help: str
    value: float = 0.0
    labels: Dict[str, str] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_prometheus(self) -> str:
        """Convert to Prometheus format line."""
        label_str = ""
        if self.labels:
            label_parts = [f'{k}="{v}"' for k, v in self.labels.items()]
            label_str = "{" + ",".join(label_parts) + "}"

        return f"{self.name}{label_str} {self.value}"


class MetricsCollector:
    """Collects and manages application metrics."""

    def __init__(self):
        self._metrics: Dict[str, Dict[str, Metric]] = defaultdict(dict)
        self._histograms: Dict[str, List[float]] = defaultdict(list)
        self._lock = Lock()
        self._start_time = time.time()

        # Register default metrics
        self._register_defaults()

    def _register_defaults(self):
        """Register default application metrics."""
        self.register(
            "app_info",
            MetricType.GAUGE,
            "Application information",
            1.0,
            {"version": "6.5.0", "platform": "plataforma_e"}
        )
        self.register(
            "app_start_time_seconds",
            MetricType.GAUGE,
            "Application start time as Unix timestamp",
            self._start_time
        )

    def _get_label_key(self, labels: Optional[Dict[str, str]]) -> str:
        """Generate a unique key for labels."""
        if not labels:
            return ""
        return ",".join(f"{k}={v}" for k, v in sorted(labels.items()))

    def register(self, name: str, type: MetricType, help: str,
                value: float = 0.0, labels: Optional[Dict[str, str]] = None) -> None:
        """Register a new metric."""
        with self._lock:
            label_key = self._get_label_key(labels)
            self._metrics[name][label_key] = Metric(
                name=name,
                type=type,
                help=help,
                value=value,
                labels=labels or {},
            )

    def increment(self, name: str, value: float = 1.0,
                 labels: Optional[Dict[str, str]] = None) -> None:
        """Increment a counter metric."""
        with self._lock:
            label_key = self._get_label_key(labels)
            if name not in self._metrics or label_key not in self._metrics[name]:
                self._metrics[name][label_key] = Metric(
                    name=name,
                    type=MetricType.COUNTER,
                    help=f"Counter for {name}",
                    value=0.0,
                    labels=labels or {},
                )
            self._metrics[name][label_key].value += value
            self._metrics[name][label_key].timestamp = datetime.now()

    def set(self, name: str, value: float,
           labels: Optional[Dict[str, str]] = None) -> None:
        """Set a gauge metric."""
        with self._lock:
            label_key = self._get_label_key(labels)
            if name not in self._metrics or label_key not in self._metrics[name]:
                self._metrics[name][label_key] = Metric(
                    name=name,
                    type=MetricType.GAUGE,
                    help=f"Gauge for {name}",
                    value=value,
                    labels=labels or {},
                )
            else:
                self._metrics[name][label_key].value = value
            self._metrics[name][label_key].timestamp = datetime.now()

    def observe(self, name: str, value: float,
               labels: Optional[Dict[str, str]] = None) -> None:
        """Record a histogram observation."""
        with self._lock:
            label_key = self._get_label_key(labels)
            hist_key = f"{name}:{label_key}"
            self._histograms[hist_key].append(value)

        # Track count and sum outside the lock to avoid deadlock
        self._increment_internal(f"{name}_count", 1.0, labels)
        self._increment_internal(f"{name}_sum", value, labels)

    def _increment_internal(self, name: str, value: float,
                           labels: Optional[Dict[str, str]] = None) -> None:
        """Internal increment without re-acquiring lock for nested calls."""
        with self._lock:
            label_key = self._get_label_key(labels)
            if name not in self._metrics or label_key not in self._metrics[name]:
                self._metrics[name][label_key] = Metric(
                    name=name,
                    type=MetricType.COUNTER,
                    help=f"Counter for {name}",
                    value=0.0,
                    labels=labels or {},
                )
            self._metrics[name][label_key].value += value
            self._metrics[name][label_key].timestamp = datetime.now()

    def get(self, name: str, labels: Optional[Dict[str, str]] = None) -> Optional[float]:
        """Get current value of a metric."""
        with self._lock:
            label_key = self._get_label_key(labels)
            if name in self._metrics and label_key in self._metrics[name]:
                return self._metrics[name][label_key].value
            return None

    def get_all(self) -> Dict[str, Dict[str, float]]:
        """Get all metrics as a dictionary."""
        with self._lock:
            result = {}
            for name, label_metrics in self._metrics.items():
                if name not in result:
                    result[name] = {}
                for label_key, metric in label_metrics.items():
                    result[name][label_key or "default"] = metric.value
            return result

    # Common metric helpers

    def record_request(self, method: str, endpoint: str,
                      status_code: int, duration_seconds: float) -> None:
        """Record HTTP request metrics."""
        labels = {"method": method, "endpoint": endpoint, "status": str(status_code)}

        self.increment("http_requests_total", 1.0, labels)
        self.observe("http_request_duration_seconds", duration_seconds, labels)

        if status_code >= 400:
            self.increment("http_errors_total", 1.0, labels)

    def record_story_action(self, action: str, status: str) -> None:
        """Record story-related metrics."""
        self.increment("stories_actions_total", 1.0, {"action": action, "status": status})

    def record_agent_task(self, agent_id: str, task_type: str, success: bool) -> None:
        """Record agent task metrics."""
        self.increment(
            "agent_tasks_total",
            1.0,
            {"agent": agent_id, "type": task_type, "success": str(success).lower()}
        )

    def set_active_connections(self, count: int) -> None:
        """Set number of active WebSocket connections."""
        self.set("active_connections", float(count))

    def set_stories_by_status(self, status_counts: Dict[str, int]) -> None:
        """Set story counts by status."""
        for status, count in status_counts.items():
            self.set("stories_total", float(count), {"status": status})


class PrometheusExporter:
    """Exports metrics in Prometheus format."""

    def __init__(self, collector: MetricsCollector):
        self.collector = collector

    def export(self) -> str:
        """Export all metrics in Prometheus/OpenMetrics format."""
        lines = []
        seen_names = set()

        with self.collector._lock:
            for name, label_metrics in self.collector._metrics.items():
                # Add HELP and TYPE once per metric name
                if name not in seen_names:
                    first_metric = next(iter(label_metrics.values()))
                    lines.append(f"# HELP {name} {first_metric.help}")
                    lines.append(f"# TYPE {name} {first_metric.type.value}")
                    seen_names.add(name)

                # Add metric values
                for metric in label_metrics.values():
                    lines.append(metric.to_prometheus())

            # Add histogram percentiles
            for hist_key, values in self.collector._histograms.items():
                if values:
                    name = hist_key.split(":")[0]
                    label_str = hist_key.split(":", 1)[1] if ":" in hist_key else ""

                    sorted_values = sorted(values)
                    n = len(sorted_values)

                    percentiles = {
                        0.5: sorted_values[int(n * 0.5)] if n > 0 else 0,
                        0.9: sorted_values[int(n * 0.9)] if n > 0 else 0,
                        0.99: sorted_values[int(n * 0.99)] if n > 0 else 0,
                    }

                    for p, v in percentiles.items():
                        p_labels = f'quantile="{p}"'
                        if label_str:
                            p_labels += f",{label_str}"
                        lines.append(f"{name}{{{p_labels}}} {v}")

        lines.append("")  # Trailing newline
        return "\n".join(lines)

    def export_json(self) -> Dict[str, Any]:
        """Export metrics as JSON (for debugging)."""
        return {
            "metrics": self.collector.get_all(),
            "exported_at": datetime.now().isoformat(),
        }


# Singleton instance
_collector: Optional[MetricsCollector] = None


def get_metrics_collector() -> MetricsCollector:
    """Get global metrics collector instance."""
    global _collector
    if _collector is None:
        _collector = MetricsCollector()
    return _collector
