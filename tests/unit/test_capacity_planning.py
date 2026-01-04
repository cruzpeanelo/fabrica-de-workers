# -*- coding: utf-8 -*-
"""
Tests for Capacity Planning Module
Plataforma E v6.5

Tests for Issue #229:
1. Team member capacity
2. Alert types and severity
3. Capacity calculations
4. Sprint planning service
"""

import pytest
from unittest.mock import Mock
from datetime import datetime


# =============================================================================
# ALERT ENUMS TESTS
# =============================================================================

class TestAlertEnums:
    """Tests for alert enumerations"""

    def test_alert_severity(self):
        """Should have all severity levels"""
        from factory.core.capacity_planning import AlertSeverity

        assert AlertSeverity.INFO.value == "info"
        assert AlertSeverity.WARNING.value == "warning"
        assert AlertSeverity.CRITICAL.value == "critical"

    def test_alert_types(self):
        """Should have all alert types"""
        from factory.core.capacity_planning import AlertType

        assert AlertType.OVERALLOCATED.value == "overallocated"
        assert AlertType.UNDERALLOCATED.value == "underallocated"
        assert AlertType.NO_CAPACITY_SET.value == "no_capacity_set"
        assert AlertType.UNBALANCED_LOAD.value == "unbalanced_load"
        assert AlertType.SPRINT_AT_RISK.value == "sprint_at_risk"


# =============================================================================
# TEAM MEMBER CAPACITY TESTS
# =============================================================================

class TestTeamMemberCapacity:
    """Tests for TeamMemberCapacity dataclass"""

    def test_capacity_creation(self):
        """Should create team member capacity"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="joao",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=0.8
        )

        assert capacity.user_id == 1
        assert capacity.username == "joao"
        assert capacity.sprint_id == "SPR-001"
        assert capacity.available_hours == 40.0
        assert capacity.focus_factor == 0.8

    def test_effective_hours_calculation(self):
        """Should calculate effective hours with focus factor"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="maria",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=0.8
        )

        # 40 * 0.8 = 32 effective hours
        assert capacity.effective_hours == 32.0

    def test_story_points_capacity(self):
        """Should estimate story points capacity"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="pedro",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=1.0  # 100% focus
        )

        # 40 hours / 4 hours per point = 10 story points
        assert capacity.story_points_capacity == 10

    def test_remaining_hours_calculation(self):
        """Should calculate remaining hours"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="ana",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=0.8,
            allocated_hours=16.0
        )

        # effective = 40 * 0.8 = 32
        # remaining = 32 - 16 = 16
        assert capacity.remaining_hours == 16.0

    def test_default_values(self):
        """Should have sensible defaults"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="test",
            sprint_id="SPR-001"
        )

        assert capacity.available_hours == 40.0
        assert capacity.focus_factor == 0.8
        assert capacity.allocated_hours == 0.0


# =============================================================================
# CAPACITY UTILIZATION TESTS
# =============================================================================

class TestCapacityUtilization:
    """Tests for capacity utilization calculations"""

    def test_utilization_percentage(self):
        """Should calculate utilization percentage"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="test",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=1.0,
            allocated_hours=20.0
        )

        # 20 / 40 = 50% utilization
        utilization = capacity.allocated_hours / capacity.effective_hours
        assert utilization == 0.5

    def test_overallocated_detection(self):
        """Should detect overallocation"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="test",
            sprint_id="SPR-001",
            available_hours=40.0,
            focus_factor=0.8,
            allocated_hours=40.0  # More than effective (32)
        )

        # effective = 32, allocated = 40 -> overallocated
        is_overallocated = capacity.allocated_hours > capacity.effective_hours
        assert is_overallocated is True


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestCapacityPlanningIntegration:
    """Integration tests for capacity planning"""

    def test_timestamps_set(self):
        """Should set timestamps automatically"""
        from factory.core.capacity_planning import TeamMemberCapacity

        capacity = TeamMemberCapacity(
            user_id=1,
            username="test",
            sprint_id="SPR-001"
        )

        assert capacity.created_at is not None
        assert capacity.updated_at is not None
        assert isinstance(capacity.created_at, datetime)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
