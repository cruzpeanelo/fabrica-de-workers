# -*- coding: utf-8 -*-
"""
Tests for APM - Application Performance Monitoring
Plataforma E v6.5

Tests for Issue #444
"""

import pytest
import time
from datetime import datetime


class TestRequestMetric:
    """Tests for RequestMetric dataclass"""

    def test_metric_creation(self):
        from factory.monitoring.apm import RequestMetric

        metric = RequestMetric(
            endpoint="/api/users",
            method="GET",
            status_code=200,
            duration_ms=45.5
        )

        assert metric.endpoint == "/api/users"
        assert metric.method == "GET"
        assert metric.status_code == 200
        assert metric.duration_ms == 45.5

    def test_metric_with_error(self):
        from factory.monitoring.apm import RequestMetric

        metric = RequestMetric(
            endpoint="/api/users",
            method="POST",
            status_code=500,
            duration_ms=100.0,
            error="Database connection failed"
        )

        assert metric.error == "Database connection failed"


class TestEndpointStats:
    """Tests for EndpointStats dataclass"""

    def test_stats_creation(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        assert stats.request_count == 0
        assert stats.error_count == 0

    def test_avg_duration(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        stats.request_count = 4
        stats.total_duration_ms = 200.0

        assert stats.avg_duration_ms == 50.0

    def test_avg_duration_zero_requests(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        assert stats.avg_duration_ms == 0

    def test_percentiles(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        stats.durations = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

        assert stats.p50_duration_ms == 60  # Middle value
        assert stats.p95_duration_ms >= 90
        assert stats.p99_duration_ms >= 90

    def test_error_rate(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        stats.request_count = 100
        stats.error_count = 5

        assert stats.error_rate == 5.0

    def test_to_dict(self):
        from factory.monitoring.apm import EndpointStats

        stats = EndpointStats(endpoint="/api/users", method="GET")
        stats.request_count = 10
        stats.durations = [50]

        d = stats.to_dict()
        assert d['endpoint'] == "/api/users"
        assert d['method'] == "GET"
        assert d['request_count'] == 10


class TestMetricsStore:
    """Tests for MetricsStore"""

    @pytest.fixture
    def store(self):
        from factory.monitoring.apm import MetricsStore
        return MetricsStore()

    def test_record_metric(self, store):
        from factory.monitoring.apm import RequestMetric

        metric = RequestMetric(
            endpoint="/api/users",
            method="GET",
            status_code=200,
            duration_ms=50.0
        )
        store.record(metric)

        stats = store.get_stats()
        assert len(stats) == 1
        assert stats[0]['endpoint'] == "/api/users"

    def test_multiple_metrics_same_endpoint(self, store):
        from factory.monitoring.apm import RequestMetric

        for i in range(5):
            metric = RequestMetric(
                endpoint="/api/users",
                method="GET",
                status_code=200,
                duration_ms=50.0 + i * 10
            )
            store.record(metric)

        stats = store.get_stats()
        assert len(stats) == 1
        assert stats[0]['request_count'] == 5

    def test_different_endpoints(self, store):
        from factory.monitoring.apm import RequestMetric

        store.record(RequestMetric("/api/users", "GET", 200, 50.0))
        store.record(RequestMetric("/api/posts", "GET", 200, 60.0))

        stats = store.get_stats()
        assert len(stats) == 2

    def test_error_tracking(self, store):
        from factory.monitoring.apm import RequestMetric

        store.record(RequestMetric("/api/users", "GET", 200, 50.0))
        store.record(RequestMetric("/api/users", "GET", 500, 100.0, error="Error"))
        store.record(RequestMetric("/api/users", "GET", 200, 50.0))

        stats = store.get_stats()
        assert stats[0]['error_count'] == 1
        assert stats[0]['error_rate'] > 0

    def test_get_slow_endpoints(self, store):
        from factory.monitoring.apm import RequestMetric

        store.record(RequestMetric("/api/fast", "GET", 200, 50.0))
        store.record(RequestMetric("/api/slow", "GET", 200, 2000.0))

        slow = store.get_slow_endpoints(threshold_ms=1000)
        assert len(slow) == 1
        assert slow[0]['endpoint'] == "/api/slow"

    def test_get_error_endpoints(self, store):
        from factory.monitoring.apm import RequestMetric

        store.record(RequestMetric("/api/ok", "GET", 200, 50.0))
        store.record(RequestMetric("/api/bad", "GET", 500, 50.0, error="Error"))

        errors = store.get_error_endpoints()
        assert len(errors) == 1
        assert errors[0]['endpoint'] == "/api/bad"

    def test_get_summary(self, store):
        from factory.monitoring.apm import RequestMetric

        for i in range(10):
            store.record(RequestMetric("/api/users", "GET", 200, 50.0))

        summary = store.get_summary()
        assert summary['total_requests'] == 10
        assert summary['total_errors'] == 0
        assert summary['endpoint_count'] == 1

    def test_clear(self, store):
        from factory.monitoring.apm import RequestMetric

        store.record(RequestMetric("/api/users", "GET", 200, 50.0))
        store.clear()

        stats = store.get_stats()
        assert len(stats) == 0

    def test_max_metrics_eviction(self):
        from factory.monitoring.apm import MetricsStore, RequestMetric

        store = MetricsStore(max_metrics=10)
        for i in range(20):
            store.record(RequestMetric("/api/test", "GET", 200, 50.0))

        # Should only keep last 10
        assert store.get_summary()['metrics_stored'] <= 10


class TestTimedDecorator:
    """Tests for @timed decorator"""

    def test_timed_sync_function(self):
        from factory.monitoring.apm import timed, get_metrics_store

        store = get_metrics_store()
        store.clear()

        @timed("test_function")
        def slow_function():
            time.sleep(0.01)
            return "result"

        result = slow_function()
        assert result == "result"

        stats = store.get_stats()
        assert len(stats) >= 1

    def test_timed_with_error(self):
        from factory.monitoring.apm import timed, get_metrics_store

        store = get_metrics_store()
        store.clear()

        @timed("error_function")
        def error_function():
            raise ValueError("Test error")

        with pytest.raises(ValueError):
            error_function()

        errors = store.get_error_endpoints()
        assert len(errors) >= 1


class TestGlobalStore:
    """Tests for global metrics store"""

    def test_get_metrics_store_singleton(self):
        from factory.monitoring.apm import get_metrics_store

        store1 = get_metrics_store()
        store2 = get_metrics_store()

        assert store1 is store2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
