# -*- coding: utf-8 -*-
"""
Tests for APM Dashboard
Plataforma E v6.5

Tests for Issue #450
"""

import pytest
from datetime import datetime, timedelta


class TestAlertSeverity:
    """Tests for AlertSeverity enum"""

    def test_severity_values(self):
        from factory.monitoring.apm_dashboard import AlertSeverity

        assert AlertSeverity.INFO.value == "info"
        assert AlertSeverity.WARNING.value == "warning"
        assert AlertSeverity.CRITICAL.value == "critical"


class TestMetricType:
    """Tests for MetricType enum"""

    def test_metric_types(self):
        from factory.monitoring.apm_dashboard import MetricType

        assert MetricType.REQUESTS.value == "requests"
        assert MetricType.LATENCY.value == "latency"
        assert MetricType.ERRORS.value == "errors"
        assert MetricType.THROUGHPUT.value == "throughput"


class TestAlert:
    """Tests for Alert dataclass"""

    def test_alert_creation(self):
        from factory.monitoring.apm_dashboard import Alert, AlertSeverity

        alert = Alert(
            alert_id="ALR-0001",
            severity=AlertSeverity.WARNING,
            title="High Latency",
            description="Latency exceeded 500ms",
            metric_name="latency_avg",
            current_value=750.5,
            threshold=500.0,
        )

        assert alert.alert_id == "ALR-0001"
        assert alert.severity == AlertSeverity.WARNING
        assert alert.acknowledged is False

    def test_alert_with_endpoint(self):
        from factory.monitoring.apm_dashboard import Alert, AlertSeverity

        alert = Alert(
            alert_id="ALR-0002",
            severity=AlertSeverity.CRITICAL,
            title="Error Rate",
            description="Error rate exceeded 5%",
            metric_name="error_rate",
            current_value=8.5,
            threshold=5.0,
            endpoint="/api/users",
        )

        assert alert.endpoint == "/api/users"

    def test_alert_to_dict(self):
        from factory.monitoring.apm_dashboard import Alert, AlertSeverity

        alert = Alert(
            alert_id="ALR-0001",
            severity=AlertSeverity.WARNING,
            title="Test Alert",
            description="Test",
            metric_name="latency_avg",
            current_value=750.0,
            threshold=500.0,
        )

        d = alert.to_dict()
        assert d["alert_id"] == "ALR-0001"
        assert d["severity"] == "warning"
        assert d["current_value"] == 750.0
        assert "created_at" in d


class TestTimeSeriesPoint:
    """Tests for TimeSeriesPoint"""

    def test_point_creation(self):
        from factory.monitoring.apm_dashboard import TimeSeriesPoint

        point = TimeSeriesPoint(
            timestamp=datetime.utcnow(),
            value=100.5,
        )

        assert point.value == 100.5

    def test_point_to_dict(self):
        from factory.monitoring.apm_dashboard import TimeSeriesPoint

        point = TimeSeriesPoint(
            timestamp=datetime(2024, 1, 1, 12, 0, 0),
            value=50.123,
        )

        d = point.to_dict()
        assert d["value"] == 50.12
        assert "timestamp" in d


class TestAPMDashboard:
    """Tests for APMDashboard"""

    @pytest.fixture
    def dashboard(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore

        store = MetricsStore()
        return APMDashboard(metrics_store=store)

    @pytest.fixture
    def dashboard_with_data(self, dashboard):
        from factory.monitoring.apm import RequestMetric

        # Add some test metrics
        for i in range(50):
            metric = RequestMetric(
                endpoint="/api/users",
                method="GET",
                status_code=200,
                duration_ms=50.0 + i,
            )
            dashboard._store.record(metric)

        for i in range(20):
            metric = RequestMetric(
                endpoint="/api/posts",
                method="POST",
                status_code=201,
                duration_ms=100.0 + i * 2,
            )
            dashboard._store.record(metric)

        # Add some errors
        for i in range(5):
            metric = RequestMetric(
                endpoint="/api/error",
                method="GET",
                status_code=500,
                duration_ms=200.0,
                error="Internal error",
            )
            dashboard._store.record(metric)

        return dashboard

    def test_creation(self, dashboard):
        assert dashboard is not None
        assert dashboard._store is not None

    def test_get_overview(self, dashboard_with_data):
        overview = dashboard_with_data.get_overview()

        assert "health_score" in overview
        assert "total_requests" in overview
        assert "total_errors" in overview
        assert "error_rate" in overview
        assert "avg_latency_ms" in overview
        assert "endpoint_count" in overview
        assert "last_updated" in overview

    def test_health_score_calculation(self, dashboard_with_data):
        overview = dashboard_with_data.get_overview()

        # With some errors, health should be below 100
        assert overview["health_score"] <= 100
        assert overview["health_score"] >= 0

    def test_get_overview_empty(self, dashboard):
        overview = dashboard.get_overview()

        assert overview["total_requests"] == 0
        assert overview["health_score"] == 100  # Perfect health when empty


class TestTimeSeries:
    """Tests for time series functionality"""

    @pytest.fixture
    def dashboard(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore

        store = MetricsStore()
        return APMDashboard(metrics_store=store)

    def test_get_time_series_requests(self, dashboard):
        from factory.monitoring.apm_dashboard import MetricType

        series = dashboard.get_time_series(
            MetricType.REQUESTS,
            duration_minutes=60,
            interval_seconds=60,
        )

        assert "metric_type" in series
        assert series["metric_type"] == "requests"
        assert "labels" in series
        assert "data" in series
        assert len(series["labels"]) == 60

    def test_get_time_series_latency(self, dashboard):
        from factory.monitoring.apm_dashboard import MetricType

        series = dashboard.get_time_series(
            MetricType.LATENCY,
            duration_minutes=30,
            interval_seconds=60,
        )

        assert series["metric_type"] == "latency"
        assert len(series["data"]) == 30

    def test_record_time_series_point(self, dashboard):
        dashboard.record_time_series_point("latency", 100.0)
        dashboard.record_time_series_point("latency", 150.0)

        assert len(dashboard._time_series["latency"]) == 2

    def test_time_series_max_points(self, dashboard):
        for i in range(150):
            dashboard.record_time_series_point("test", float(i))

        # Should keep only last 100
        assert len(dashboard._time_series["test"]) == 100


class TestEndpointPerformance:
    """Tests for endpoint performance"""

    @pytest.fixture
    def dashboard_with_endpoints(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore, RequestMetric

        store = MetricsStore()
        dashboard = APMDashboard(metrics_store=store)

        # Fast endpoint
        for i in range(30):
            store.record(RequestMetric("/api/fast", "GET", 200, 20.0))

        # Slow endpoint
        for i in range(20):
            store.record(RequestMetric("/api/slow", "GET", 200, 2000.0))

        # Medium endpoint with errors
        for i in range(25):
            status = 500 if i % 5 == 0 else 200
            error = "Error" if status == 500 else None
            store.record(RequestMetric("/api/medium", "GET", status, 300.0, error=error))

        return dashboard

    def test_get_endpoint_performance(self, dashboard_with_endpoints):
        performance = dashboard_with_endpoints.get_endpoint_performance()

        assert len(performance) == 3
        # Should include grade
        assert all("grade" in p for p in performance)

    def test_endpoint_grade_calculation(self, dashboard_with_endpoints):
        performance = dashboard_with_endpoints.get_endpoint_performance()

        # Find endpoints
        fast = next(p for p in performance if p["endpoint"] == "/api/fast")
        slow = next(p for p in performance if p["endpoint"] == "/api/slow")
        medium = next(p for p in performance if p["endpoint"] == "/api/medium")

        # Fast endpoint should have best grade
        assert fast["grade"] in ["A", "B"]
        # Slow endpoint (2000ms) should have lower grade than fast
        assert fast["grade"] <= slow["grade"]  # A < B < C etc
        # Medium with errors should have worse grade than fast
        assert fast["grade"] <= medium["grade"]

    def test_get_top_endpoints_by_requests(self, dashboard_with_endpoints):
        top = dashboard_with_endpoints.get_top_endpoints(by="requests")

        assert len(top) <= 10
        assert top[0]["request_count"] >= top[-1]["request_count"]

    def test_get_top_endpoints_by_latency(self, dashboard_with_endpoints):
        top = dashboard_with_endpoints.get_top_endpoints(by="latency")

        assert top[0]["endpoint"] == "/api/slow"

    def test_get_top_endpoints_by_errors(self, dashboard_with_endpoints):
        top = dashboard_with_endpoints.get_top_endpoints(by="errors")

        # Medium has errors
        assert any(e["endpoint"] == "/api/medium" for e in top)


class TestAlerts:
    """Tests for alert functionality"""

    @pytest.fixture
    def dashboard(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore

        store = MetricsStore()
        return APMDashboard(metrics_store=store)

    @pytest.fixture
    def dashboard_with_high_latency(self, dashboard):
        from factory.monitoring.apm import RequestMetric

        # Add high latency metrics
        for i in range(20):
            dashboard._store.record(RequestMetric(
                "/api/slow",
                "GET",
                200,
                1500.0,  # High latency
            ))

        return dashboard

    def test_check_alerts_high_latency(self, dashboard_with_high_latency):
        alerts = dashboard_with_high_latency.check_alerts()

        # Should trigger latency alerts
        assert len(alerts) > 0

    def test_check_alerts_no_issues(self, dashboard):
        from factory.monitoring.apm import RequestMetric

        # Add normal metrics
        for i in range(10):
            dashboard._store.record(RequestMetric(
                "/api/normal",
                "GET",
                200,
                50.0,  # Normal latency
            ))

        alerts = dashboard.check_alerts()

        # Should not trigger any alerts
        assert len(alerts) == 0

    def test_get_alerts(self, dashboard_with_high_latency):
        dashboard_with_high_latency.check_alerts()
        alerts = dashboard_with_high_latency.get_alerts()

        assert isinstance(alerts, list)
        for alert in alerts:
            assert "alert_id" in alert
            assert "severity" in alert

    def test_acknowledge_alert(self, dashboard_with_high_latency):
        dashboard_with_high_latency.check_alerts()
        alerts = dashboard_with_high_latency.get_alerts()

        if alerts:
            alert_id = alerts[0]["alert_id"]
            result = dashboard_with_high_latency.acknowledge_alert(alert_id)
            assert result is True

            # Should not appear in unacknowledged list
            unack = dashboard_with_high_latency.get_alerts(include_acknowledged=False)
            assert not any(a["alert_id"] == alert_id for a in unack)

    def test_acknowledge_nonexistent_alert(self, dashboard):
        result = dashboard.acknowledge_alert("FAKE-0001")
        assert result is False

    def test_add_threshold(self, dashboard):
        dashboard.add_threshold("latency_avg", warning=100, critical=200)

        threshold = next(
            (t for t in dashboard._thresholds if t.metric == "latency_avg"),
            None
        )
        assert threshold is not None
        assert threshold.warning_threshold == 100
        assert threshold.critical_threshold == 200


class TestChartData:
    """Tests for chart data generation"""

    @pytest.fixture
    def dashboard_with_data(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore, RequestMetric

        store = MetricsStore()
        dashboard = APMDashboard(metrics_store=store)

        # Add metrics
        for i in range(50):
            store.record(RequestMetric("/api/users", "GET", 200, 50.0 + i))
            store.record(RequestMetric("/api/posts", "POST", 201, 100.0))

        return dashboard

    def test_get_latency_chart_data(self, dashboard_with_data):
        chart = dashboard_with_data.get_latency_chart_data()

        assert "labels" in chart
        assert "datasets" in chart
        assert len(chart["datasets"]) == 1
        assert chart["datasets"][0]["label"] == "Average Latency (ms)"

    def test_get_requests_chart_data(self, dashboard_with_data):
        chart = dashboard_with_data.get_requests_chart_data()

        assert "labels" in chart
        assert "datasets" in chart
        assert len(chart["datasets"]) == 2  # Requests and Errors

    def test_get_endpoint_comparison_chart(self, dashboard_with_data):
        chart = dashboard_with_data.get_endpoint_comparison_chart()

        assert "labels" in chart
        assert "datasets" in chart
        assert len(chart["datasets"]) == 2  # Avg and P95

    def test_get_error_distribution_chart(self, dashboard_with_data):
        from factory.monitoring.apm import RequestMetric

        # Add some errors
        for i in range(10):
            dashboard_with_data._store.record(RequestMetric(
                "/api/error1",
                "GET",
                500,
                100.0,
                error="Error",
            ))

        chart = dashboard_with_data.get_error_distribution_chart()

        assert "labels" in chart
        assert "datasets" in chart


class TestDashboardData:
    """Tests for full dashboard data"""

    @pytest.fixture
    def dashboard_with_data(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore, RequestMetric

        store = MetricsStore()
        dashboard = APMDashboard(metrics_store=store)

        for i in range(30):
            store.record(RequestMetric("/api/users", "GET", 200, 50.0))

        return dashboard

    def test_get_dashboard_data(self, dashboard_with_data):
        data = dashboard_with_data.get_dashboard_data()

        assert "overview" in data
        assert "latency_chart" in data
        assert "requests_chart" in data
        assert "endpoint_comparison" in data
        assert "error_distribution" in data
        assert "top_endpoints" in data
        assert "slow_endpoints" in data
        assert "error_endpoints" in data
        assert "alerts" in data
        assert "generated_at" in data


class TestTrendCalculation:
    """Tests for trend calculation"""

    @pytest.fixture
    def dashboard(self):
        from factory.monitoring.apm_dashboard import APMDashboard
        from factory.monitoring.apm import MetricsStore

        store = MetricsStore()
        return APMDashboard(metrics_store=store)

    def test_trend_stable(self, dashboard):
        for i in range(10):
            dashboard.record_time_series_point("test", 100.0)

        trend = dashboard._get_trend("test")
        assert trend == "stable"

    def test_trend_increasing(self, dashboard):
        for i in range(10):
            dashboard.record_time_series_point("test", float(i * 20))

        trend = dashboard._get_trend("test")
        assert trend == "increasing"

    def test_trend_decreasing(self, dashboard):
        for i in range(10):
            dashboard.record_time_series_point("test", float(100 - i * 15))

        trend = dashboard._get_trend("test")
        assert trend == "decreasing"

    def test_trend_no_data(self, dashboard):
        trend = dashboard._get_trend("nonexistent")
        assert trend == "stable"


class TestGlobalAPMDashboard:
    """Tests for global singleton"""

    def test_get_apm_dashboard_singleton(self):
        from factory.monitoring.apm_dashboard import get_apm_dashboard

        d1 = get_apm_dashboard()
        d2 = get_apm_dashboard()

        assert d1 is d2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
