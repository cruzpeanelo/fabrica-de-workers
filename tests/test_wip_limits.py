# -*- coding: utf-8 -*-
"""
Tests for WIP Limits Implementation - Issue #237
Fabrica de Agentes v6.5

Comprehensive tests for:
1. KanbanPolicy model
2. FlowMetric model
3. WIP validation logic
4. API endpoints
5. Integration with story movement
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime, timedelta
import json


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_db():
    """Create mock database session"""
    return Mock()


@pytest.fixture
def mock_project():
    """Create mock project object"""
    project = Mock()
    project.project_id = "PRJ-001"
    project.name = "Test Project"
    return project


@pytest.fixture
def mock_story():
    """Create mock story object"""
    story = Mock()
    story.story_id = "STR-001"
    story.project_id = "PRJ-001"
    story.status = "backlog"
    story.title = "Test Story"
    story.is_deleted = False
    return story


@pytest.fixture
def mock_policy():
    """Create mock kanban policy"""
    policy = Mock()
    policy.policy_id = "POL-001"
    policy.project_id = "PRJ-001"
    policy.wip_limits = {
        "backlog": None,
        "ready": 10,
        "in_progress": 5,
        "review": 3,
        "testing": 5,
        "done": None
    }
    policy.wip_policy = "soft"
    policy.alert_on_exceed = True
    policy.notify_team = False
    return policy


# =============================================================================
# MODEL TESTS
# =============================================================================

class TestKanbanPolicyModel:
    """Tests for KanbanPolicy model"""

    def test_policy_to_dict(self):
        """Should convert policy to dictionary"""
        from factory.database.models import KanbanPolicy

        policy = KanbanPolicy(
            policy_id="POL-001",
            project_id="PRJ-001",
            wip_limits={"in_progress": 5},
            wip_policy="soft"
        )

        result = policy.to_dict()

        assert result["policy_id"] == "POL-001"
        assert result["project_id"] == "PRJ-001"
        assert result["wip_limits"]["in_progress"] == 5
        assert result["wip_policy"] == "soft"

    def test_get_limit_returns_value(self):
        """Should return limit for status"""
        from factory.database.models import KanbanPolicy

        policy = KanbanPolicy(
            policy_id="POL-001",
            project_id="PRJ-001",
            wip_limits={"in_progress": 5, "review": 3}
        )

        assert policy.get_limit("in_progress") == 5
        assert policy.get_limit("review") == 3
        assert policy.get_limit("backlog") is None

    def test_is_hard_limit(self):
        """Should correctly identify hard limit policy"""
        from factory.database.models import KanbanPolicy

        soft_policy = KanbanPolicy(wip_policy="soft")
        hard_policy = KanbanPolicy(wip_policy="hard")

        assert soft_policy.is_hard_limit() is False
        assert hard_policy.is_hard_limit() is True


class TestFlowMetricModel:
    """Tests for FlowMetric model"""

    def test_metric_to_dict(self):
        """Should convert metric to dictionary"""
        from factory.database.models import FlowMetric

        metric = FlowMetric(
            metric_id="MET-001",
            project_id="PRJ-001",
            date=datetime(2025, 1, 15),
            backlog_count=10,
            in_progress_count=5,
            done_count=20,
            throughput=2.5,
            avg_cycle_time=3.2
        )

        result = metric.to_dict()

        assert result["metric_id"] == "MET-001"
        assert result["counts"]["backlog"] == 10
        assert result["counts"]["in_progress"] == 5
        assert result["metrics"]["throughput"] == 2.5

    def test_total_wip_property(self):
        """Should calculate total WIP correctly"""
        from factory.database.models import FlowMetric

        metric = FlowMetric(
            in_progress_count=5,
            review_count=3,
            testing_count=2
        )

        assert metric.total_wip == 10


# =============================================================================
# WIP VALIDATION TESTS
# =============================================================================

class TestWipValidation:
    """Tests for WIP validation logic"""

    def _setup_db_mock(self, mock_db, policy, count):
        """Setup mock db to handle chained filter calls correctly"""
        # Create a mock query object that can chain .filter() calls
        mock_query = Mock()
        mock_query.filter.return_value = mock_query
        mock_query.first.return_value = policy
        mock_query.count.return_value = count
        mock_db.query.return_value = mock_query
        return mock_query

    def test_validate_wip_allows_within_limit(self, mock_db, mock_story, mock_policy):
        """Should allow move when within limit"""
        # Issue #210: Fix mock to handle chained filter calls
        self._setup_db_mock(mock_db, mock_policy, 3)  # under limit of 5

        from factory.dashboard.app_v6_agile import validate_wip_limit

        result = validate_wip_limit(mock_db, mock_story, "in_progress")

        assert result["allowed"] is True
        assert result["warning"] is False

    def test_validate_wip_soft_limit_allows_with_warning(self, mock_db, mock_story, mock_policy):
        """Should allow move with warning when soft limit exceeded"""
        mock_policy.wip_policy = "soft"
        self._setup_db_mock(mock_db, mock_policy, 5)  # At limit

        from factory.dashboard.app_v6_agile import validate_wip_limit

        result = validate_wip_limit(mock_db, mock_story, "in_progress")

        assert result["allowed"] is True
        assert result["warning"] is True
        assert "excedido" in result["message"].lower() or "limite" in result["message"].lower()

    def test_validate_wip_hard_limit_blocks(self, mock_db, mock_story, mock_policy):
        """Should block move when hard limit exceeded"""
        mock_policy.wip_policy = "hard"
        self._setup_db_mock(mock_db, mock_policy, 5)  # At limit

        from factory.dashboard.app_v6_agile import validate_wip_limit

        result = validate_wip_limit(mock_db, mock_story, "in_progress")

        assert result["allowed"] is False
        assert result["warning"] is True
        assert "limite" in result["message"].lower()

    def test_validate_wip_no_limit_allows(self, mock_db, mock_story):
        """Should allow move when no limit defined"""
        # No policy - backlog has no default limit
        self._setup_db_mock(mock_db, None, 0)

        from factory.dashboard.app_v6_agile import validate_wip_limit

        result = validate_wip_limit(mock_db, mock_story, "backlog")  # No default limit

        assert result["allowed"] is True
        assert result["warning"] is False


# =============================================================================
# API ENDPOINT TESTS
# =============================================================================

class TestKanbanAPIEndpoints:
    """Tests for Kanban WIP API endpoints"""

    @pytest.fixture
    def client(self):
        """Create test client"""
        from fastapi.testclient import TestClient
        from factory.api.kanban_routes import router
        from fastapi import FastAPI

        app = FastAPI()
        app.include_router(router)
        return TestClient(app)

    def test_get_policy_default(self, client):
        """Should return default policy when none exists"""
        with patch('factory.api.kanban_routes.get_async_db'):
            # Note: This would need async mock setup
            pass  # Placeholder for async test

    def test_create_policy(self):
        """Should create new WIP policy"""
        from factory.api.kanban_routes import KanbanPolicyCreate, WipLimitsConfig

        policy_data = KanbanPolicyCreate(
            project_id="PRJ-001",
            wip_limits=WipLimitsConfig(in_progress=5, review=3),
            wip_policy="soft"
        )

        assert policy_data.project_id == "PRJ-001"
        assert policy_data.wip_limits.in_progress == 5
        assert policy_data.wip_policy == "soft"


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestWipIntegration:
    """Integration tests for WIP Limits"""

    def test_wip_state_calculation(self):
        """Test WIP state calculation logic"""
        # Simulate the getWipState JavaScript function in Python
        def get_wip_state(status, count, limit):
            if not limit:
                return 'normal'
            if count > limit:
                return 'exceeded'
            if count >= limit * 0.8:
                return 'warning'
            return 'normal'

        # Test cases
        assert get_wip_state('in_progress', 3, 5) == 'normal'
        assert get_wip_state('in_progress', 4, 5) == 'warning'  # 80% threshold
        assert get_wip_state('in_progress', 5, 5) == 'warning'  # At limit
        assert get_wip_state('in_progress', 6, 5) == 'exceeded'  # Over limit
        assert get_wip_state('backlog', 100, None) == 'normal'  # No limit

    def test_wip_percentage_calculation(self):
        """Test WIP percentage calculation"""
        def get_wip_percentage(count, limit):
            if not limit:
                return 0
            return (count / limit) * 100

        assert get_wip_percentage(3, 5) == 60.0
        assert get_wip_percentage(5, 5) == 100.0
        assert get_wip_percentage(6, 5) == 120.0
        assert get_wip_percentage(10, None) == 0


# =============================================================================
# SCHEMA VALIDATION TESTS
# =============================================================================

class TestSchemaValidation:
    """Tests for Pydantic schema validation"""

    def test_wip_limits_config_defaults(self):
        """Should have sensible defaults"""
        from factory.api.kanban_routes import WipLimitsConfig

        config = WipLimitsConfig()

        assert config.backlog is None
        assert config.in_progress == 5
        assert config.review == 3
        assert config.done is None

    def test_kanban_policy_create_validation(self):
        """Should validate policy creation data"""
        from factory.api.kanban_routes import KanbanPolicyCreate, WipLimitsConfig

        # Valid data
        policy = KanbanPolicyCreate(
            project_id="PRJ-001",
            wip_policy="hard"
        )
        assert policy.wip_policy == "hard"

        # Check default wip_limits
        assert policy.wip_limits.in_progress == 5

    def test_wip_validation_result(self):
        """Should have correct structure"""
        from factory.api.kanban_routes import WipValidationResult

        result = WipValidationResult(
            allowed=False,
            warning=True,
            message="Limite excedido",
            current_count=6,
            limit=5,
            status="in_progress"
        )

        assert result.allowed is False
        assert result.warning is True
        assert result.current_count == 6
        assert result.limit == 5


# =============================================================================
# EDGE CASE TESTS
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases and error handling"""

    def test_empty_wip_limits(self):
        """Should handle empty WIP limits"""
        from factory.database.models import KanbanPolicy

        policy = KanbanPolicy(
            policy_id="POL-001",
            project_id="PRJ-001",
            wip_limits={}
        )

        assert policy.get_limit("in_progress") is None
        assert policy.to_dict()["wip_limits"] == {}

    def test_null_wip_limits(self):
        """Should handle None WIP limits"""
        from factory.database.models import KanbanPolicy

        policy = KanbanPolicy(
            policy_id="POL-001",
            project_id="PRJ-001",
            wip_limits=None
        )

        assert policy.get_limit("in_progress") is None

    def test_flow_metric_zero_values(self):
        """Should handle zero values in metrics"""
        from factory.database.models import FlowMetric

        metric = FlowMetric(
            metric_id="MET-001",
            project_id="PRJ-001",
            date=datetime.utcnow(),
            in_progress_count=0,
            review_count=0,
            testing_count=0
        )

        assert metric.total_wip == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
