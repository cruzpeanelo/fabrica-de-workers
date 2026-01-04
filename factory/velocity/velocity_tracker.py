# -*- coding: utf-8 -*-
"""
Velocity Tracker - Plataforma E
===============================

Track and calculate team velocity metrics.

Issue #438: Dashboard de Velocidade do Time
"""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from statistics import mean, stdev
from threading import Lock


@dataclass
class BurndownPoint:
    """A point on the burndown chart."""
    day: int
    date: datetime
    remaining_points: float
    ideal_points: float
    completed_points: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "day": self.day,
            "date": self.date.isoformat(),
            "remaining_points": self.remaining_points,
            "ideal_points": round(self.ideal_points, 1),
            "completed_points": self.completed_points,
        }


@dataclass
class SprintMetrics:
    """Metrics for a single sprint."""
    sprint_id: str
    name: str
    start_date: datetime
    end_date: datetime
    planned_points: float = 0.0
    completed_points: float = 0.0
    stories_planned: int = 0
    stories_completed: int = 0
    stories_added: int = 0  # Scope creep
    stories_removed: int = 0

    @property
    def velocity(self) -> float:
        """Calculate sprint velocity."""
        return self.completed_points

    @property
    def completion_rate(self) -> float:
        """Calculate completion rate percentage."""
        if self.planned_points == 0:
            return 0.0
        return (self.completed_points / self.planned_points) * 100

    @property
    def scope_change(self) -> int:
        """Calculate net scope change."""
        return self.stories_added - self.stories_removed

    @property
    def duration_days(self) -> int:
        """Calculate sprint duration in days."""
        return (self.end_date - self.start_date).days

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "sprint_id": self.sprint_id,
            "name": self.name,
            "start_date": self.start_date.isoformat(),
            "end_date": self.end_date.isoformat(),
            "planned_points": self.planned_points,
            "completed_points": self.completed_points,
            "velocity": self.velocity,
            "completion_rate": round(self.completion_rate, 1),
            "stories_planned": self.stories_planned,
            "stories_completed": self.stories_completed,
            "scope_change": self.scope_change,
            "duration_days": self.duration_days,
        }


@dataclass
class VelocityData:
    """Aggregated velocity data."""
    sprints: List[SprintMetrics] = field(default_factory=list)

    @property
    def average_velocity(self) -> float:
        """Calculate average velocity across sprints."""
        if not self.sprints:
            return 0.0
        velocities = [s.velocity for s in self.sprints]
        return mean(velocities)

    @property
    def velocity_trend(self) -> str:
        """Determine velocity trend."""
        if len(self.sprints) < 2:
            return "stable"

        recent = self.sprints[-3:] if len(self.sprints) >= 3 else self.sprints
        velocities = [s.velocity for s in recent]

        if len(velocities) < 2:
            return "stable"

        first_half = mean(velocities[:len(velocities)//2 or 1])
        second_half = mean(velocities[len(velocities)//2:])

        diff_pct = ((second_half - first_half) / first_half * 100) if first_half > 0 else 0

        if diff_pct > 10:
            return "increasing"
        elif diff_pct < -10:
            return "decreasing"
        return "stable"

    @property
    def velocity_std_dev(self) -> float:
        """Calculate velocity standard deviation."""
        if len(self.sprints) < 2:
            return 0.0
        velocities = [s.velocity for s in self.sprints]
        return stdev(velocities)

    @property
    def predicted_velocity(self) -> float:
        """Predict next sprint velocity based on trend."""
        if len(self.sprints) < 3:
            return self.average_velocity

        # Use weighted average of last 3 sprints
        recent = self.sprints[-3:]
        weights = [0.2, 0.3, 0.5]  # More weight to recent
        weighted_sum = sum(s.velocity * w for s, w in zip(recent, weights))
        return round(weighted_sum, 1)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "sprints": [s.to_dict() for s in self.sprints],
            "average_velocity": round(self.average_velocity, 1),
            "velocity_trend": self.velocity_trend,
            "velocity_std_dev": round(self.velocity_std_dev, 1),
            "predicted_velocity": self.predicted_velocity,
            "total_sprints": len(self.sprints),
        }


class VelocityTracker:
    """Track and calculate team velocity metrics."""

    def __init__(self):
        self._sprints: Dict[str, SprintMetrics] = {}
        self._burndown_data: Dict[str, List[BurndownPoint]] = {}
        self._lock = Lock()
        self._initialize_sample_data()

    def _initialize_sample_data(self):
        """Initialize with sample sprint data."""
        now = datetime.now()

        sample_sprints = [
            SprintMetrics(
                sprint_id="SPR-001",
                name="Sprint 1",
                start_date=now - timedelta(days=84),
                end_date=now - timedelta(days=70),
                planned_points=30,
                completed_points=28,
                stories_planned=6,
                stories_completed=5,
            ),
            SprintMetrics(
                sprint_id="SPR-002",
                name="Sprint 2",
                start_date=now - timedelta(days=70),
                end_date=now - timedelta(days=56),
                planned_points=32,
                completed_points=30,
                stories_planned=7,
                stories_completed=6,
            ),
            SprintMetrics(
                sprint_id="SPR-003",
                name="Sprint 3",
                start_date=now - timedelta(days=56),
                end_date=now - timedelta(days=42),
                planned_points=35,
                completed_points=32,
                stories_planned=8,
                stories_completed=7,
            ),
            SprintMetrics(
                sprint_id="SPR-004",
                name="Sprint 4",
                start_date=now - timedelta(days=42),
                end_date=now - timedelta(days=28),
                planned_points=34,
                completed_points=35,
                stories_planned=7,
                stories_completed=7,
                stories_added=1,
            ),
            SprintMetrics(
                sprint_id="SPR-005",
                name="Sprint 5",
                start_date=now - timedelta(days=28),
                end_date=now - timedelta(days=14),
                planned_points=36,
                completed_points=34,
                stories_planned=8,
                stories_completed=7,
            ),
            SprintMetrics(
                sprint_id="SPR-006",
                name="Sprint 6 (Current)",
                start_date=now - timedelta(days=14),
                end_date=now,
                planned_points=38,
                completed_points=25,
                stories_planned=9,
                stories_completed=6,
            ),
        ]

        for sprint in sample_sprints:
            self._sprints[sprint.sprint_id] = sprint

    def add_sprint(self, sprint: SprintMetrics) -> None:
        """Add a sprint to tracking."""
        with self._lock:
            self._sprints[sprint.sprint_id] = sprint

    def update_sprint(self, sprint_id: str, **kwargs) -> Optional[SprintMetrics]:
        """Update sprint metrics."""
        with self._lock:
            if sprint_id not in self._sprints:
                return None

            sprint = self._sprints[sprint_id]
            for key, value in kwargs.items():
                if hasattr(sprint, key):
                    setattr(sprint, key, value)
            return sprint

    def get_sprint(self, sprint_id: str) -> Optional[SprintMetrics]:
        """Get a specific sprint."""
        return self._sprints.get(sprint_id)

    def get_velocity_data(self, limit: int = 6) -> VelocityData:
        """Get velocity data for recent sprints."""
        with self._lock:
            sprints = sorted(
                self._sprints.values(),
                key=lambda s: s.start_date,
            )[-limit:]
            return VelocityData(sprints=sprints)

    def get_burndown(self, sprint_id: str) -> List[BurndownPoint]:
        """Get burndown chart data for a sprint."""
        sprint = self._sprints.get(sprint_id)
        if not sprint:
            return []

        # Generate burndown data if not cached
        if sprint_id not in self._burndown_data:
            self._burndown_data[sprint_id] = self._generate_burndown(sprint)

        return self._burndown_data[sprint_id]

    def _generate_burndown(self, sprint: SprintMetrics) -> List[BurndownPoint]:
        """Generate burndown chart points."""
        points = []
        total_days = sprint.duration_days
        total_points = sprint.planned_points
        daily_ideal = total_points / total_days if total_days > 0 else 0

        # Simulate burndown (in real app, this would come from actual data)
        remaining = total_points
        completed = 0.0

        for day in range(total_days + 1):
            date = sprint.start_date + timedelta(days=day)
            ideal = total_points - (daily_ideal * day)

            # Simulate work completion (random-ish but trending down)
            if day > 0 and day <= total_days:
                daily_progress = (sprint.completed_points / total_days)
                # Add some variance
                variance = (day % 3 - 1) * 0.5
                actual_progress = max(0, daily_progress + variance)
                remaining = max(0, remaining - actual_progress)
                completed += actual_progress

            points.append(BurndownPoint(
                day=day,
                date=date,
                remaining_points=round(remaining, 1),
                ideal_points=round(ideal, 1),
                completed_points=round(completed, 1),
            ))

        return points

    def get_burnup(self, sprint_id: str) -> Dict[str, Any]:
        """Get burnup chart data for a sprint."""
        sprint = self._sprints.get(sprint_id)
        if not sprint:
            return {}

        burndown = self.get_burndown(sprint_id)

        return {
            "sprint_id": sprint_id,
            "total_scope": sprint.planned_points,
            "points": [
                {
                    "day": p.day,
                    "date": p.date.isoformat(),
                    "completed": p.completed_points,
                    "scope": sprint.planned_points,
                }
                for p in burndown
            ],
        }

    def get_kpis(self) -> Dict[str, Any]:
        """Get key performance indicators."""
        velocity_data = self.get_velocity_data()
        all_sprints = list(self._sprints.values())

        total_stories = sum(s.stories_completed for s in all_sprints)
        total_points = sum(s.completed_points for s in all_sprints)

        # Calculate average cycle time (simplified)
        avg_points_per_story = total_points / total_stories if total_stories > 0 else 0
        avg_stories_per_sprint = total_stories / len(all_sprints) if all_sprints else 0

        return {
            "average_velocity": round(velocity_data.average_velocity, 1),
            "velocity_trend": velocity_data.velocity_trend,
            "predicted_velocity": velocity_data.predicted_velocity,
            "total_stories_completed": total_stories,
            "total_points_delivered": round(total_points, 1),
            "avg_points_per_story": round(avg_points_per_story, 1),
            "avg_stories_per_sprint": round(avg_stories_per_sprint, 1),
            "total_sprints": len(all_sprints),
            "completion_rate": round(
                mean([s.completion_rate for s in all_sprints]) if all_sprints else 0,
                1
            ),
        }

    def get_velocity_chart_data(self, limit: int = 6) -> Dict[str, Any]:
        """Get data formatted for velocity chart."""
        velocity_data = self.get_velocity_data(limit)

        return {
            "labels": [s.name for s in velocity_data.sprints],
            "planned": [s.planned_points for s in velocity_data.sprints],
            "completed": [s.completed_points for s in velocity_data.sprints],
            "average_line": [velocity_data.average_velocity] * len(velocity_data.sprints),
            "trend": velocity_data.velocity_trend,
        }

    def record_story_completion(
        self,
        sprint_id: str,
        story_points: float,
    ) -> bool:
        """Record a story completion in a sprint."""
        with self._lock:
            if sprint_id not in self._sprints:
                return False

            sprint = self._sprints[sprint_id]
            sprint.completed_points += story_points
            sprint.stories_completed += 1

            # Clear cached burndown
            if sprint_id in self._burndown_data:
                del self._burndown_data[sprint_id]

            return True

    def export_data(self, format: str = "json") -> str:
        """Export velocity data."""
        import json

        data = {
            "sprints": [s.to_dict() for s in self._sprints.values()],
            "kpis": self.get_kpis(),
            "exported_at": datetime.now().isoformat(),
        }

        if format == "json":
            return json.dumps(data, indent=2)

        # CSV format
        import csv
        import io

        output = io.StringIO()
        writer = csv.writer(output)
        writer.writerow([
            "sprint_id", "name", "planned_points", "completed_points",
            "velocity", "completion_rate"
        ])

        for sprint in self._sprints.values():
            writer.writerow([
                sprint.sprint_id,
                sprint.name,
                sprint.planned_points,
                sprint.completed_points,
                sprint.velocity,
                f"{sprint.completion_rate:.1f}%",
            ])

        return output.getvalue()


# Singleton instance
_tracker: Optional[VelocityTracker] = None


def get_velocity_tracker() -> VelocityTracker:
    """Get global velocity tracker instance."""
    global _tracker
    if _tracker is None:
        _tracker = VelocityTracker()
    return _tracker
