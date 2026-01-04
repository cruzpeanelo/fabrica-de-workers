# -*- coding: utf-8 -*-
"""
Tests for Health Check System
Plataforma E v6.5

Tests for Issue #442
"""

import pytest
import time


class TestHealthStatus:
    """Tests for HealthStatus enum"""

    def test_status_values(self):
        from factory.health.health_check import HealthStatus

        assert HealthStatus.HEALTHY.value == "healthy"
        assert HealthStatus.DEGRADED.value == "degraded"
        assert HealthStatus.UNHEALTHY.value == "unhealthy"
        assert HealthStatus.UNKNOWN.value == "unknown"


class TestComponentHealth:
    """Tests for ComponentHealth dataclass"""

    def test_creation(self):
        from factory.health.health_check import ComponentHealth, HealthStatus

        health = ComponentHealth(
            name="test",
            status=HealthStatus.HEALTHY,
            latency_ms=5.0
        )

        assert health.name == "test"
        assert health.status == HealthStatus.HEALTHY
        assert health.latency_ms == 5.0

    def test_to_dict(self):
        from factory.health.health_check import ComponentHealth, HealthStatus

        health = ComponentHealth(
            name="db",
            status=HealthStatus.HEALTHY,
            latency_ms=10.5,
            details={"type": "sqlite"}
        )

        d = health.to_dict()
        assert d["status"] == "healthy"
        assert d["latency_ms"] == 10.5
        assert d["type"] == "sqlite"

    def test_with_message(self):
        from factory.health.health_check import ComponentHealth, HealthStatus

        health = ComponentHealth(
            name="redis",
            status=HealthStatus.UNHEALTHY,
            message="Connection refused"
        )

        d = health.to_dict()
        assert d["message"] == "Connection refused"


class TestHealthReport:
    """Tests for HealthReport dataclass"""

    def test_creation(self):
        from factory.health.health_check import HealthReport, HealthStatus

        report = HealthReport(
            status=HealthStatus.HEALTHY,
            uptime_seconds=3600.0
        )

        assert report.status == HealthStatus.HEALTHY
        assert report.uptime_seconds == 3600.0
        assert report.version == "6.5.0"

    def test_to_dict(self):
        from factory.health.health_check import HealthReport, HealthStatus, ComponentHealth

        report = HealthReport(
            status=HealthStatus.HEALTHY,
            components={
                "db": ComponentHealth(name="db", status=HealthStatus.HEALTHY)
            }
        )

        d = report.to_dict()
        assert d["status"] == "healthy"
        assert "db" in d["components"]
        assert "timestamp" in d


class TestHealthChecker:
    """Tests for HealthChecker class"""

    @pytest.fixture
    def checker(self):
        from factory.health.health_check import HealthChecker
        return HealthChecker()

    def test_creation(self, checker):
        assert checker is not None
        assert len(checker._checks) > 0

    def test_uptime(self, checker):
        time.sleep(0.1)
        assert checker.uptime_seconds > 0

    def test_check_basic(self, checker):
        result = checker.check_basic()

        assert result["status"] == "healthy"
        assert "timestamp" in result

    def test_check_detailed(self, checker):
        report = checker.check_detailed()

        assert report is not None
        assert report.status is not None
        assert len(report.components) > 0

    def test_check_readiness(self, checker):
        result = checker.check_readiness()

        assert "ready" in result
        assert "status" in result
        assert "checks_passed" in result
        assert "checks_total" in result

    def test_check_liveness(self, checker):
        result = checker.check_liveness()

        assert result["alive"] is True
        assert "uptime_seconds" in result
        assert "timestamp" in result

    def test_is_healthy(self, checker):
        result = checker.is_healthy()
        assert isinstance(result, bool)

    def test_register_custom_check(self, checker):
        from factory.health.health_check import ComponentHealth, HealthStatus

        def custom_check():
            return ComponentHealth(
                name="custom",
                status=HealthStatus.HEALTHY,
                message="All good"
            )

        checker.register_check("custom", custom_check)
        report = checker.check_detailed()

        assert "custom" in report.components
        assert report.components["custom"].status == HealthStatus.HEALTHY

    def test_unregister_check(self, checker):
        initial_count = len(checker._checks)
        checker.unregister_check("cpu")

        assert len(checker._checks) == initial_count - 1

    def test_unregister_nonexistent(self, checker):
        result = checker.unregister_check("nonexistent")
        assert result is False


class TestSystemChecks:
    """Tests for built-in system checks"""

    @pytest.fixture
    def checker(self):
        from factory.health.health_check import HealthChecker
        return HealthChecker()

    def test_memory_check(self, checker):
        from factory.health.health_check import HealthStatus

        health = checker._check_memory()

        assert health.name == "memory"
        assert health.status in list(HealthStatus)
        assert "used_percent" in health.details

    def test_disk_check(self, checker):
        from factory.health.health_check import HealthStatus

        health = checker._check_disk()

        assert health.name == "disk"
        assert health.status in list(HealthStatus)
        assert "free_gb" in health.details

    def test_cpu_check(self, checker):
        from factory.health.health_check import HealthStatus

        health = checker._check_cpu()

        assert health.name == "cpu"
        assert health.status in list(HealthStatus)
        assert "usage_percent" in health.details

    def test_system_check(self, checker):
        from factory.health.health_check import HealthStatus

        health = checker._check_system()

        assert health.name == "system"
        assert health.status in list(HealthStatus)


class TestMetricType:
    """Tests for MetricType enum"""

    def test_metric_types(self):
        from factory.health.metrics import MetricType

        assert MetricType.COUNTER.value == "counter"
        assert MetricType.GAUGE.value == "gauge"
        assert MetricType.HISTOGRAM.value == "histogram"


class TestMetric:
    """Tests for Metric dataclass"""

    def test_to_prometheus_no_labels(self):
        from factory.health.metrics import Metric, MetricType

        metric = Metric(
            name="test_metric",
            type=MetricType.COUNTER,
            help="Test metric",
            value=42.0
        )

        assert metric.to_prometheus() == "test_metric 42.0"

    def test_to_prometheus_with_labels(self):
        from factory.health.metrics import Metric, MetricType

        metric = Metric(
            name="http_requests",
            type=MetricType.COUNTER,
            help="HTTP requests",
            value=100.0,
            labels={"method": "GET", "status": "200"}
        )

        result = metric.to_prometheus()
        assert "http_requests" in result
        assert 'method="GET"' in result
        assert 'status="200"' in result
        assert "100.0" in result


class TestMetricsCollector:
    """Tests for MetricsCollector"""

    @pytest.fixture
    def collector(self):
        from factory.health.metrics import MetricsCollector
        return MetricsCollector()

    def test_creation(self, collector):
        assert collector is not None

    def test_increment(self, collector):
        collector.increment("test_counter", 1.0)
        collector.increment("test_counter", 2.0)

        value = collector.get("test_counter")
        assert value == 3.0

    def test_increment_with_labels(self, collector):
        collector.increment("requests", 1.0, {"method": "GET"})
        collector.increment("requests", 1.0, {"method": "POST"})
        collector.increment("requests", 1.0, {"method": "GET"})

        get_value = collector.get("requests", {"method": "GET"})
        post_value = collector.get("requests", {"method": "POST"})

        assert get_value == 2.0
        assert post_value == 1.0

    def test_set(self, collector):
        collector.set("gauge_metric", 42.0)
        assert collector.get("gauge_metric") == 42.0

        collector.set("gauge_metric", 100.0)
        assert collector.get("gauge_metric") == 100.0

    def test_observe(self, collector):
        collector.observe("duration", 0.5)
        collector.observe("duration", 1.0)
        collector.observe("duration", 1.5)

        count = collector.get("duration_count")
        total = collector.get("duration_sum")

        assert count == 3.0
        assert total == 3.0

    def test_get_all(self, collector):
        collector.increment("metric1", 1.0)
        collector.set("metric2", 2.0)

        all_metrics = collector.get_all()
        assert "metric1" in all_metrics
        assert "metric2" in all_metrics

    def test_record_request(self, collector):
        collector.record_request("GET", "/api/stories", 200, 0.05)

        total = collector.get("http_requests_total", {
            "method": "GET",
            "endpoint": "/api/stories",
            "status": "200"
        })
        assert total == 1.0

    def test_record_error(self, collector):
        collector.record_request("POST", "/api/error", 500, 0.1)

        errors = collector.get("http_errors_total", {
            "method": "POST",
            "endpoint": "/api/error",
            "status": "500"
        })
        assert errors == 1.0

    def test_record_agent_task(self, collector):
        collector.record_agent_task("BACK", "code_gen", True)
        collector.record_agent_task("BACK", "code_gen", False)

        success = collector.get("agent_tasks_total", {
            "agent": "BACK",
            "type": "code_gen",
            "success": "true"
        })
        assert success == 1.0


class TestPrometheusExporter:
    """Tests for PrometheusExporter"""

    @pytest.fixture
    def exporter(self):
        from factory.health.metrics import MetricsCollector, PrometheusExporter
        collector = MetricsCollector()
        collector.increment("test_requests", 100.0, {"method": "GET"})
        return PrometheusExporter(collector)

    def test_export(self, exporter):
        output = exporter.export()

        assert "# HELP" in output
        assert "# TYPE" in output
        assert "test_requests" in output

    def test_export_json(self, exporter):
        result = exporter.export_json()

        assert "metrics" in result
        assert "exported_at" in result


class TestGlobalInstances:
    """Tests for global singleton instances"""

    def test_get_health_checker_singleton(self):
        from factory.health.health_check import get_health_checker

        checker1 = get_health_checker()
        checker2 = get_health_checker()
        assert checker1 is checker2

    def test_get_metrics_collector_singleton(self):
        from factory.health.metrics import get_metrics_collector

        collector1 = get_metrics_collector()
        collector2 = get_metrics_collector()
        assert collector1 is collector2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
