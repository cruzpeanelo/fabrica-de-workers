# -*- coding: utf-8 -*-
"""
APM - Application Performance Monitoring
Plataforma E

Issue #444: APM - Application Performance Monitoring
"""

import time
import functools
import statistics
import logging
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from collections import defaultdict
from threading import Lock

logger = logging.getLogger(__name__)


@dataclass
class RequestMetric:
    """Single request metric."""
    endpoint: str
    method: str
    status_code: int
    duration_ms: float
    timestamp: datetime = field(default_factory=datetime.utcnow)
    error: Optional[str] = None


@dataclass
class EndpointStats:
    """Aggregated stats for an endpoint."""
    endpoint: str
    method: str
    request_count: int = 0
    error_count: int = 0
    total_duration_ms: float = 0
    min_duration_ms: float = float('inf')
    max_duration_ms: float = 0
    durations: List[float] = field(default_factory=list)

    @property
    def avg_duration_ms(self) -> float:
        if self.request_count == 0:
            return 0
        return self.total_duration_ms / self.request_count

    @property
    def p50_duration_ms(self) -> float:
        if not self.durations:
            return 0
        sorted_d = sorted(self.durations)
        return sorted_d[len(sorted_d) // 2]

    @property
    def p95_duration_ms(self) -> float:
        if not self.durations:
            return 0
        sorted_d = sorted(self.durations)
        idx = int(len(sorted_d) * 0.95)
        return sorted_d[min(idx, len(sorted_d) - 1)]

    @property
    def p99_duration_ms(self) -> float:
        if not self.durations:
            return 0
        sorted_d = sorted(self.durations)
        idx = int(len(sorted_d) * 0.99)
        return sorted_d[min(idx, len(sorted_d) - 1)]

    @property
    def error_rate(self) -> float:
        if self.request_count == 0:
            return 0
        return (self.error_count / self.request_count) * 100

    def to_dict(self) -> dict:
        return {
            'endpoint': self.endpoint,
            'method': self.method,
            'request_count': self.request_count,
            'error_count': self.error_count,
            'error_rate': round(self.error_rate, 2),
            'avg_duration_ms': round(self.avg_duration_ms, 2),
            'min_duration_ms': round(self.min_duration_ms, 2) if self.min_duration_ms != float('inf') else 0,
            'max_duration_ms': round(self.max_duration_ms, 2),
            'p50_duration_ms': round(self.p50_duration_ms, 2),
            'p95_duration_ms': round(self.p95_duration_ms, 2),
            'p99_duration_ms': round(self.p99_duration_ms, 2)
        }


class MetricsStore:
    """In-memory metrics storage with retention."""

    def __init__(self, max_metrics: int = 10000, retention_hours: int = 24):
        self._metrics: List[RequestMetric] = []
        self._endpoint_stats: Dict[str, EndpointStats] = {}
        self._lock = Lock()
        self._max_metrics = max_metrics
        self._retention_hours = retention_hours

    def record(self, metric: RequestMetric):
        """Record a new metric."""
        with self._lock:
            self._metrics.append(metric)

            key = f'{metric.method}:{metric.endpoint}'
            if key not in self._endpoint_stats:
                self._endpoint_stats[key] = EndpointStats(
                    endpoint=metric.endpoint,
                    method=metric.method
                )

            stats = self._endpoint_stats[key]
            stats.request_count += 1
            stats.total_duration_ms += metric.duration_ms
            stats.min_duration_ms = min(stats.min_duration_ms, metric.duration_ms)
            stats.max_duration_ms = max(stats.max_duration_ms, metric.duration_ms)
            stats.durations.append(metric.duration_ms)

            if metric.error:
                stats.error_count += 1

            if len(stats.durations) > 1000:
                stats.durations = stats.durations[-1000:]

            if len(self._metrics) > self._max_metrics:
                self._metrics = self._metrics[-self._max_metrics:]

    def get_stats(self) -> List[dict]:
        """Get all endpoint stats."""
        with self._lock:
            return [s.to_dict() for s in self._endpoint_stats.values()]

    def get_slow_endpoints(self, threshold_ms: float = 1000, limit: int = 10) -> List[dict]:
        """Get slowest endpoints."""
        with self._lock:
            slow = [s for s in self._endpoint_stats.values() if s.avg_duration_ms > threshold_ms]
            slow.sort(key=lambda x: x.avg_duration_ms, reverse=True)
            return [s.to_dict() for s in slow[:limit]]

    def get_error_endpoints(self, limit: int = 10) -> List[dict]:
        """Get endpoints with highest error rates."""
        with self._lock:
            with_errors = [s for s in self._endpoint_stats.values() if s.error_count > 0]
            with_errors.sort(key=lambda x: x.error_rate, reverse=True)
            return [s.to_dict() for s in with_errors[:limit]]

    def get_recent_metrics(self, minutes: int = 5) -> List[dict]:
        """Get metrics from last N minutes."""
        cutoff = datetime.utcnow() - timedelta(minutes=minutes)
        with self._lock:
            recent = [m for m in self._metrics if m.timestamp > cutoff]
            return [{
                'endpoint': m.endpoint,
                'method': m.method,
                'status_code': m.status_code,
                'duration_ms': m.duration_ms,
                'timestamp': m.timestamp.isoformat(),
                'error': m.error
            } for m in recent[-100:]]

    def get_summary(self) -> dict:
        """Get overall summary."""
        with self._lock:
            total_requests = sum(s.request_count for s in self._endpoint_stats.values())
            total_errors = sum(s.error_count for s in self._endpoint_stats.values())

            all_durations = []
            for s in self._endpoint_stats.values():
                all_durations.extend(s.durations[-100:])

            avg_duration = statistics.mean(all_durations) if all_durations else 0

            return {
                'total_requests': total_requests,
                'total_errors': total_errors,
                'error_rate': round((total_errors / total_requests * 100) if total_requests > 0 else 0, 2),
                'avg_duration_ms': round(avg_duration, 2),
                'endpoint_count': len(self._endpoint_stats),
                'metrics_stored': len(self._metrics)
            }

    def clear(self):
        """Clear all metrics."""
        with self._lock:
            self._metrics.clear()
            self._endpoint_stats.clear()


# Global metrics store
_metrics_store: Optional[MetricsStore] = None


def get_metrics_store() -> MetricsStore:
    """Get global metrics store."""
    global _metrics_store
    if _metrics_store is None:
        _metrics_store = MetricsStore()
    return _metrics_store


def timed(name: Optional[str] = None):
    """Decorator to time function execution."""
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            start = time.perf_counter()
            error = None
            try:
                return func(*args, **kwargs)
            except Exception as e:
                error = str(e)
                raise
            finally:
                duration_ms = (time.perf_counter() - start) * 1000
                metric = RequestMetric(
                    endpoint=name or func.__name__,
                    method='FUNC',
                    status_code=500 if error else 200,
                    duration_ms=duration_ms,
                    error=error
                )
                get_metrics_store().record(metric)

        @functools.wraps(func)
        async def async_wrapper(*args, **kwargs):
            start = time.perf_counter()
            error = None
            try:
                return await func(*args, **kwargs)
            except Exception as e:
                error = str(e)
                raise
            finally:
                duration_ms = (time.perf_counter() - start) * 1000
                metric = RequestMetric(
                    endpoint=name or func.__name__,
                    method='FUNC',
                    status_code=500 if error else 200,
                    duration_ms=duration_ms,
                    error=error
                )
                get_metrics_store().record(metric)

        import asyncio
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return wrapper

    return decorator


class APMMiddleware:
    """ASGI middleware for request timing."""

    def __init__(self, app):
        self.app = app

    async def __call__(self, scope, receive, send):
        if scope['type'] != 'http':
            return await self.app(scope, receive, send)

        start = time.perf_counter()
        status_code = 200
        error = None

        async def send_wrapper(message):
            nonlocal status_code
            if message['type'] == 'http.response.start':
                status_code = message['status']
            await send(message)

        try:
            await self.app(scope, receive, send_wrapper)
        except Exception as e:
            error = str(e)
            status_code = 500
            raise
        finally:
            duration_ms = (time.perf_counter() - start) * 1000
            path = scope.get('path', '/')
            method = scope.get('method', 'GET')

            metric = RequestMetric(
                endpoint=path,
                method=method,
                status_code=status_code,
                duration_ms=duration_ms,
                error=error
            )
            get_metrics_store().record(metric)
