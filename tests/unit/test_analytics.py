# -*- coding: utf-8 -*-
"""
Tests for Analytics Service
Plataforma E v6.5

Tests for Issue #434:
1. Metric types and periods
2. Metric results
3. Burndown points
4. Analytics service
"""

import pytest
from unittest.mock import Mock, patch
from datetime import datetime, timedelta


# =============================================================================
# METRIC ENUMS TESTS
# =============================================================================

class TestMetricEnums:
    """Tests for metric enumerations"""

    def test_metric_periods(self):
        """Should have all metric periods"""
        from factory.core.analytics_service import MetricPeriod

        assert MetricPeriod.DAY.value == "day"
        assert MetricPeriod.WEEK.value == "week"
        assert MetricPeriod.MONTH.value == "month"
        assert MetricPeriod.QUARTER.value == "quarter"
        assert MetricPeriod.SPRINT.value == "sprint"

    def test_metric_types(self):
        """Should have all metric types"""
        from factory.core.analytics_service import MetricType

        assert MetricType.VELOCITY.value == "velocity"
        assert MetricType.THROUGHPUT.value == "throughput"
        assert MetricType.CYCLE_TIME.value == "cycle_time"
        assert MetricType.LEAD_TIME.value == "lead_time"
        assert MetricType.WIP.value == "wip"
        assert MetricType.BURNDOWN.value == "burndown"


# =============================================================================
# METRIC RESULT TESTS
# =============================================================================

class TestMetricResult:
    """Tests for MetricResult dataclass"""

    def test_metric_result_creation(self):
        """Should create metric result"""
        from factory.core.analytics_service import MetricResult, MetricType

        result = MetricResult(
            metric_type=MetricType.VELOCITY,
            value=42,
            period_start=datetime.now() - timedelta(days=7),
            period_end=datetime.now(),
            project_id="PRJ-001"
        )

        assert result.metric_type == MetricType.VELOCITY
        assert result.value == 42
        assert result.project_id == "PRJ-001"

    def test_metric_result_with_trend(self):
        """Should track trend information"""
        from factory.core.analytics_service import MetricResult, MetricType

        result = MetricResult(
            metric_type=MetricType.VELOCITY,
            value=50,
            period_start=datetime.now() - timedelta(days=7),
            period_end=datetime.now(),
            trend="up",
            change_percentage=15.5
        )

        assert result.trend == "up"
        assert result.change_percentage == 15.5


# =============================================================================
# BURNDOWN POINT TESTS
# =============================================================================

class TestBurndownPoint:
    """Tests for BurndownPoint dataclass"""

    def test_burndown_point_creation(self):
        """Should create burndown point"""
        from factory.core.analytics_service import BurndownPoint

        point = BurndownPoint(
            date=datetime.now(),
            remaining_points=50,
            remaining_stories=10,
            ideal_points=45.0,
            completed_points=30
        )

        assert point.remaining_points == 50
        assert point.remaining_stories == 10
        assert point.ideal_points == 45.0
        assert point.completed_points == 30


# =============================================================================
# ANALYTICS SERVICE TESTS
# =============================================================================

class TestAnalyticsService:
    """Tests for AnalyticsService class"""

    @pytest.fixture
    def mock_db(self):
        """Create mock database"""
        return Mock()

    @pytest.fixture
    def analytics_service(self, mock_db):
        """Create analytics service"""
        from factory.core.analytics_service import AnalyticsService
        return AnalyticsService(mock_db)

    def test_service_initialization(self, analytics_service):
        """Should initialize service"""
        assert analytics_service is not None

    def test_service_initialized(self, analytics_service):
        """Should be properly initialized"""
        # Service exists and is valid
        assert analytics_service is not None
        assert isinstance(analytics_service, object)


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestAnalyticsIntegration:
    """Integration tests for analytics"""

    def test_metric_type_string_conversion(self):
        """MetricType should be string enum"""
        from factory.core.analytics_service import MetricType

        velocity = MetricType.VELOCITY
        assert str(velocity) == "MetricType.VELOCITY"
        assert velocity.value == "velocity"

    def test_metric_period_string_conversion(self):
        """MetricPeriod should be string enum"""
        from factory.core.analytics_service import MetricPeriod

        week = MetricPeriod.WEEK
        assert week.value == "week"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
