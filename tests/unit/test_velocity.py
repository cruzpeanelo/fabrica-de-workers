# -*- coding: utf-8 -*-
"""
Tests for Velocity Tracker
Plataforma E v6.5

Tests for Issue #438
"""

import pytest
from datetime import datetime, timedelta


class TestBurndownPoint:
    """Tests for BurndownPoint"""

    def test_creation(self):
        from factory.velocity.velocity_tracker import BurndownPoint

        point = BurndownPoint(
            day=1,
            date=datetime.now(),
            remaining_points=30.0,
            ideal_points=28.0,
            completed_points=5.0,
        )

        assert point.day == 1
        assert point.remaining_points == 30.0
        assert point.ideal_points == 28.0

    def test_to_dict(self):
        from factory.velocity.velocity_tracker import BurndownPoint

        point = BurndownPoint(
            day=1,
            date=datetime.now(),
            remaining_points=25.0,
            ideal_points=24.0,
        )

        d = point.to_dict()
        assert "day" in d
        assert "remaining_points" in d
        assert "ideal_points" in d


class TestSprintMetrics:
    """Tests for SprintMetrics"""

    def test_creation(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now() - timedelta(days=14),
            end_date=datetime.now(),
            planned_points=30.0,
            completed_points=28.0,
        )

        assert sprint.sprint_id == "SPR-001"
        assert sprint.planned_points == 30.0

    def test_velocity(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
            completed_points=28.0,
        )

        assert sprint.velocity == 28.0

    def test_completion_rate(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
            planned_points=40.0,
            completed_points=30.0,
        )

        assert sprint.completion_rate == 75.0

    def test_completion_rate_zero_planned(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
            planned_points=0.0,
            completed_points=10.0,
        )

        assert sprint.completion_rate == 0.0

    def test_scope_change(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
            stories_added=3,
            stories_removed=1,
        )

        assert sprint.scope_change == 2

    def test_duration_days(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
        )

        assert sprint.duration_days == 14

    def test_to_dict(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        sprint = SprintMetrics(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
            planned_points=30.0,
            completed_points=28.0,
        )

        d = sprint.to_dict()
        assert d["sprint_id"] == "SPR-001"
        assert d["velocity"] == 28.0
        assert "completion_rate" in d


class TestVelocityData:
    """Tests for VelocityData"""

    @pytest.fixture
    def sample_sprints(self):
        from factory.velocity.velocity_tracker import SprintMetrics

        now = datetime.now()
        return [
            SprintMetrics(
                sprint_id=f"SPR-00{i}",
                name=f"Sprint {i}",
                start_date=now - timedelta(days=14 * (4 - i)),
                end_date=now - timedelta(days=14 * (3 - i)),
                planned_points=30.0,
                completed_points=25.0 + i * 2,
            )
            for i in range(1, 5)
        ]

    def test_average_velocity(self, sample_sprints):
        from factory.velocity.velocity_tracker import VelocityData

        data = VelocityData(sprints=sample_sprints)

        # (27 + 29 + 31 + 33) / 4 = 30
        assert data.average_velocity == 30.0

    def test_velocity_trend(self, sample_sprints):
        from factory.velocity.velocity_tracker import VelocityData

        data = VelocityData(sprints=sample_sprints)

        # Velocity is increasing
        assert data.velocity_trend == "increasing"

    def test_predicted_velocity(self, sample_sprints):
        from factory.velocity.velocity_tracker import VelocityData

        data = VelocityData(sprints=sample_sprints)

        # Should be weighted average of last 3
        assert data.predicted_velocity > 0

    def test_empty_data(self):
        from factory.velocity.velocity_tracker import VelocityData

        data = VelocityData()

        assert data.average_velocity == 0.0
        assert data.velocity_trend == "stable"

    def test_to_dict(self, sample_sprints):
        from factory.velocity.velocity_tracker import VelocityData

        data = VelocityData(sprints=sample_sprints)
        d = data.to_dict()

        assert "sprints" in d
        assert "average_velocity" in d
        assert "velocity_trend" in d


class TestVelocityTracker:
    """Tests for VelocityTracker"""

    @pytest.fixture
    def tracker(self):
        from factory.velocity.velocity_tracker import VelocityTracker
        return VelocityTracker()

    def test_creation(self, tracker):
        assert tracker is not None
        assert len(tracker._sprints) > 0  # Has sample data

    def test_get_sprint(self, tracker):
        sprint = tracker.get_sprint("SPR-001")
        assert sprint is not None
        assert sprint.name == "Sprint 1"

    def test_add_sprint(self, tracker):
        from factory.velocity.velocity_tracker import SprintMetrics

        new_sprint = SprintMetrics(
            sprint_id="SPR-NEW",
            name="New Sprint",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=14),
        )

        tracker.add_sprint(new_sprint)
        assert tracker.get_sprint("SPR-NEW") is not None

    def test_update_sprint(self, tracker):
        updated = tracker.update_sprint(
            "SPR-001",
            completed_points=35.0
        )

        assert updated.completed_points == 35.0

    def test_get_velocity_data(self, tracker):
        data = tracker.get_velocity_data()

        assert len(data.sprints) > 0
        assert data.average_velocity > 0

    def test_get_velocity_data_limit(self, tracker):
        data = tracker.get_velocity_data(limit=3)
        assert len(data.sprints) == 3

    def test_get_burndown(self, tracker):
        burndown = tracker.get_burndown("SPR-001")

        assert len(burndown) > 0
        assert burndown[0].day == 0
        assert burndown[0].remaining_points > 0

    def test_get_burnup(self, tracker):
        burnup = tracker.get_burnup("SPR-001")

        assert "sprint_id" in burnup
        assert "total_scope" in burnup
        assert "points" in burnup

    def test_get_kpis(self, tracker):
        kpis = tracker.get_kpis()

        assert "average_velocity" in kpis
        assert "velocity_trend" in kpis
        assert "total_stories_completed" in kpis
        assert "completion_rate" in kpis

    def test_get_velocity_chart_data(self, tracker):
        chart_data = tracker.get_velocity_chart_data()

        assert "labels" in chart_data
        assert "planned" in chart_data
        assert "completed" in chart_data
        assert "average_line" in chart_data

    def test_record_story_completion(self, tracker):
        sprint = tracker.get_sprint("SPR-006")
        initial_points = sprint.completed_points

        tracker.record_story_completion("SPR-006", 5.0)

        assert sprint.completed_points == initial_points + 5.0
        assert sprint.stories_completed > 0

    def test_export_json(self, tracker):
        import json

        output = tracker.export_data(format="json")
        data = json.loads(output)

        assert "sprints" in data
        assert "kpis" in data

    def test_export_csv(self, tracker):
        output = tracker.export_data(format="csv")

        assert "sprint_id" in output
        assert "SPR-001" in output


class TestGlobalVelocityTracker:
    """Tests for global singleton"""

    def test_get_velocity_tracker_singleton(self):
        from factory.velocity.velocity_tracker import get_velocity_tracker

        tracker1 = get_velocity_tracker()
        tracker2 = get_velocity_tracker()

        assert tracker1 is tracker2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
