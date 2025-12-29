# -*- coding: utf-8 -*-
"""
Productivity Analytics and Team Insights (Issue #65)
=====================================================
Comprehensive productivity metrics for teams and individual contributors.

Features:
- Developer/Agent metrics tracking
- Project velocity and throughput
- Story type analysis (Features vs Bugs vs Tech Debt)
- Automatic insights generation
- Historical comparisons
- Export capabilities
"""
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum
import statistics
import json


class MetricPeriod(str, Enum):
    """Time period for metrics aggregation"""
    DAY = "day"
    WEEK = "week"
    SPRINT = "sprint"
    MONTH = "month"
    QUARTER = "quarter"


class TrendDirection(str, Enum):
    """Trend direction indicator"""
    INCREASING = "increasing"
    STABLE = "stable"
    DECREASING = "decreasing"


@dataclass
class DeveloperMetrics:
    """Metrics for an individual developer or agent"""
    assignee: str
    stories_completed: int = 0
    stories_total: int = 0
    points_delivered: int = 0
    points_total: int = 0
    stories_in_progress: int = 0
    stories_in_review: int = 0
    avg_cycle_time_hours: float = 0.0
    avg_cycle_time_days: float = 0.0
    rework_rate: float = 0.0
    completion_rate: float = 0.0
    success_rate: float = 0.0
    bugs_fixed: int = 0
    features_delivered: int = 0
    tech_debt_resolved: int = 0
    specializations: List[str] = field(default_factory=list)
    efficiency_score: float = 0.0  # 0-100

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class TeamMetrics:
    """Aggregated team-level metrics"""
    total_stories: int = 0
    stories_completed: int = 0
    total_points: int = 0
    points_delivered: int = 0
    completion_rate: float = 0.0
    avg_velocity: float = 0.0
    velocity_unit: str = "points/sprint"
    velocity_trend: float = 0.0
    velocity_trend_direction: str = "stable"
    predictability_score: float = 100.0
    collaboration_rate: float = 0.0
    throughput: float = 0.0
    throughput_unit: str = "stories/week"
    avg_cycle_time_days: float = 0.0
    wip_count: int = 0
    lead_time_avg_days: float = 0.0
    lead_time_p50_days: float = 0.0
    lead_time_p85_days: float = 0.0
    lead_time_p95_days: float = 0.0
    status_distribution: Dict[str, int] = field(default_factory=dict)
    category_distribution: Dict[str, int] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class StoryTypeMetrics:
    """Metrics broken down by story type/category"""
    category: str
    count: int = 0
    points: int = 0
    completed: int = 0
    avg_cycle_time_days: float = 0.0
    avg_points: float = 0.0
    estimation_accuracy: float = 0.0  # How accurate were estimates

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class SprintMetrics:
    """Metrics for a specific sprint"""
    sprint_id: str
    sprint_name: str
    velocity: int = 0
    capacity: int = 0
    stories_completed: int = 0
    stories_total: int = 0
    commitment_met: bool = False
    completion_percentage: float = 0.0
    start_date: Optional[str] = None
    end_date: Optional[str] = None
    status: str = "active"
    bugs_found: int = 0
    scope_change_percentage: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class ProductivityInsight:
    """AI-generated insight about productivity"""
    category: str  # velocity, quality, process, collaboration, bottleneck
    title: str
    description: str
    impact: str  # high, medium, low
    data_point: Optional[str] = None
    recommendation: Optional[str] = None
    trend: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class ProductivityAlert:
    """Alert for productivity issues"""
    alert_type: str  # danger, warning, info, success
    category: str
    title: str
    message: str
    threshold: Optional[float] = None
    current_value: Optional[float] = None
    created_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class ProductivityAnalytics:
    """
    Comprehensive productivity analytics engine.

    Tracks and analyzes:
    - Individual developer/agent metrics
    - Team velocity and throughput
    - Story type distribution
    - Cycle time and lead time
    - Sprint metrics and velocity history
    - Bottleneck detection
    - AI-powered insights
    """

    def __init__(self, db_session, story_model, sprint_model, story_task_model=None):
        """
        Initialize the analytics engine.

        Args:
            db_session: SQLAlchemy session factory (SessionLocal)
            story_model: Story model class
            sprint_model: Sprint model class
            story_task_model: Optional StoryTask model for detailed analysis
        """
        self.SessionLocal = db_session
        self.Story = story_model
        self.Sprint = sprint_model
        self.StoryTask = story_task_model

    def _get_db(self):
        """Get a database session"""
        return self.SessionLocal()

    def calculate_percentile(self, values: List[float], percentile: int) -> float:
        """Calculate percentile of a list of values"""
        if not values:
            return 0.0
        sorted_values = sorted(values)
        index = (percentile / 100) * (len(sorted_values) - 1)
        lower = int(index)
        upper = lower + 1
        if upper >= len(sorted_values):
            return sorted_values[-1]
        weight = index - lower
        return sorted_values[lower] * (1 - weight) + sorted_values[upper] * weight

    def detect_trend(self, values: List[float], threshold: float = 0.1) -> TrendDirection:
        """Detect trend direction from a series of values"""
        if len(values) < 3:
            return TrendDirection.STABLE

        recent = sum(values[-3:]) / 3 if len(values) >= 3 else values[-1]
        older = sum(values[:-3]) / max(1, len(values) - 3) if len(values) > 3 else recent

        if older == 0:
            return TrendDirection.INCREASING if recent > 0 else TrendDirection.STABLE

        change = (recent - older) / older

        if change > threshold:
            return TrendDirection.INCREASING
        elif change < -threshold:
            return TrendDirection.DECREASING
        return TrendDirection.STABLE

    def get_developer_metrics(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30,
        assignee: Optional[str] = None
    ) -> List[DeveloperMetrics]:
        """
        Calculate metrics for each developer/agent.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze
            assignee: Filter for specific assignee

        Returns:
            List of DeveloperMetrics objects
        """
        db = self._get_db()
        try:
            end_date = datetime.utcnow()
            start_date = end_date - timedelta(days=days)

            query = db.query(self.Story)
            if project_id:
                query = query.filter(self.Story.project_id == project_id)
            if sprint_id:
                query = query.filter(self.Story.sprint_id == sprint_id)
            if assignee:
                query = query.filter(self.Story.assignee == assignee)

            all_stories = query.all()

            # Group stories by assignee
            dev_data: Dict[str, Dict] = {}

            for story in all_stories:
                dev_name = story.assignee or "Unassigned"

                if dev_name not in dev_data:
                    dev_data[dev_name] = {
                        "assignee": dev_name,
                        "stories_total": 0,
                        "stories_completed": 0,
                        "points_total": 0,
                        "points_delivered": 0,
                        "stories_in_progress": 0,
                        "stories_in_review": 0,
                        "cycle_times": [],
                        "bugs_fixed": 0,
                        "features_delivered": 0,
                        "tech_debt_resolved": 0,
                        "categories": [],
                        "success_count": 0
                    }

                dev = dev_data[dev_name]
                dev["stories_total"] += 1
                dev["points_total"] += story.story_points or 0
                dev["categories"].append(story.category or "feature")

                if story.status == 'done':
                    dev["stories_completed"] += 1
                    dev["points_delivered"] += story.story_points or 0
                    dev["success_count"] += 1

                    # Track category
                    category = story.category or "feature"
                    if category == "bug":
                        dev["bugs_fixed"] += 1
                    elif category == "feature":
                        dev["features_delivered"] += 1
                    elif category == "tech_debt":
                        dev["tech_debt_resolved"] += 1

                    # Calculate cycle time
                    if story.started_at and story.completed_at:
                        cycle_hours = (story.completed_at - story.started_at).total_seconds() / 3600
                        dev["cycle_times"].append(cycle_hours)

                elif story.status == 'in_progress':
                    dev["stories_in_progress"] += 1
                elif story.status == 'review':
                    dev["stories_in_review"] += 1

            # Convert to DeveloperMetrics objects
            result = []
            for dev_name, dev in dev_data.items():
                avg_cycle = sum(dev["cycle_times"]) / len(dev["cycle_times"]) if dev["cycle_times"] else 0
                rework_rate = (dev["stories_in_review"] /
                              (dev["stories_completed"] + dev["stories_in_review"]) * 100
                              if dev["stories_completed"] > 0 else 0)
                completion_rate = (dev["stories_completed"] / dev["stories_total"] * 100
                                  if dev["stories_total"] > 0 else 0)
                success_rate = (dev["success_count"] / dev["stories_total"] * 100
                               if dev["stories_total"] > 0 else 0)

                # Determine specializations based on most common categories
                from collections import Counter
                category_counts = Counter(dev["categories"])
                specializations = [cat for cat, count in category_counts.most_common(3)
                                  if count >= 2]

                # Calculate efficiency score (weighted average of metrics)
                efficiency = min(100, (
                    completion_rate * 0.3 +
                    success_rate * 0.3 +
                    (100 - min(rework_rate, 100)) * 0.2 +
                    (100 - min(avg_cycle / 24, 100)) * 0.2  # Penalize long cycle times
                ))

                metrics = DeveloperMetrics(
                    assignee=dev_name,
                    stories_completed=dev["stories_completed"],
                    stories_total=dev["stories_total"],
                    points_delivered=dev["points_delivered"],
                    points_total=dev["points_total"],
                    stories_in_progress=dev["stories_in_progress"],
                    stories_in_review=dev["stories_in_review"],
                    avg_cycle_time_hours=round(avg_cycle, 1),
                    avg_cycle_time_days=round(avg_cycle / 24, 1) if avg_cycle > 0 else 0,
                    rework_rate=round(rework_rate, 1),
                    completion_rate=round(completion_rate, 1),
                    success_rate=round(success_rate, 1),
                    bugs_fixed=dev["bugs_fixed"],
                    features_delivered=dev["features_delivered"],
                    tech_debt_resolved=dev["tech_debt_resolved"],
                    specializations=specializations,
                    efficiency_score=round(efficiency, 1)
                )
                result.append(metrics)

            # Sort by points delivered
            result.sort(key=lambda x: x.points_delivered, reverse=True)
            return result

        finally:
            db.close()

    def get_team_metrics(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> TeamMetrics:
        """
        Calculate aggregated team metrics.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            TeamMetrics object with aggregated data
        """
        db = self._get_db()
        try:
            end_date = datetime.utcnow()
            start_date = end_date - timedelta(days=days)

            query = db.query(self.Story)
            if project_id:
                query = query.filter(self.Story.project_id == project_id)
            if sprint_id:
                query = query.filter(self.Story.sprint_id == sprint_id)

            all_stories = query.all()
            completed_stories = [s for s in all_stories
                               if s.status == 'done' and s.completed_at and s.completed_at >= start_date]

            # Basic counts
            total_points = sum(s.story_points or 0 for s in all_stories)
            done_points = sum(s.story_points or 0 for s in all_stories if s.status == 'done')

            # Status distribution
            status_counts = {}
            for story in all_stories:
                status_counts[story.status] = status_counts.get(story.status, 0) + 1

            # Category distribution
            category_counts = {}
            for story in all_stories:
                cat = story.category or "feature"
                category_counts[cat] = category_counts.get(cat, 0) + 1

            # Velocity from sprints
            sprints_data = db.query(self.Sprint).filter(self.Sprint.status == 'completed').all()
            velocities = [s.velocity for s in sprints_data if s.velocity and s.velocity > 0]
            avg_velocity = sum(velocities) / len(velocities) if velocities else 0

            # Fallback velocity from completed stories
            if not velocities and completed_stories:
                avg_velocity = sum(s.story_points or 0 for s in completed_stories) / max(1, days / 7)

            # Velocity trend
            velocity_trend = 0.0
            velocity_trend_direction = TrendDirection.STABLE
            if len(velocities) >= 2:
                velocity_trend_direction = self.detect_trend(velocities)
                recent_avg = sum(velocities[-3:]) / min(3, len(velocities))
                older_avg = sum(velocities[:-3]) / max(1, len(velocities) - 3) if len(velocities) > 3 else recent_avg
                velocity_trend = ((recent_avg - older_avg) / older_avg * 100) if older_avg > 0 else 0

            # Predictability score (lower variance = higher predictability)
            predictability = 100.0
            if len(velocities) >= 2:
                mean_vel = statistics.mean(velocities)
                if mean_vel > 0:
                    predictability = max(0, 100 - (statistics.stdev(velocities) / mean_vel) * 100)

            # Collaboration rate (stories with multiple contributors on tasks)
            collab_count = 0
            if self.StoryTask:
                stories_with_tasks = [s for s in all_stories if hasattr(s, 'story_tasks') and s.story_tasks]
                for story in stories_with_tasks:
                    assignees = set(t.assignee for t in story.story_tasks if t.assignee)
                    if len(assignees) > 1:
                        collab_count += 1
                collab_rate = (collab_count / len(stories_with_tasks) * 100) if stories_with_tasks else 0
            else:
                collab_rate = 0

            # Cycle times and lead times
            cycle_times = []
            lead_times = []
            for story in completed_stories:
                if story.started_at and story.completed_at:
                    cycle_days = (story.completed_at - story.started_at).total_seconds() / 3600 / 24
                    cycle_times.append(cycle_days)
                if story.created_at and story.completed_at:
                    lead_days = (story.completed_at - story.created_at).total_seconds() / 3600 / 24
                    lead_times.append(lead_days)

            avg_cycle_time = sum(cycle_times) / len(cycle_times) if cycle_times else 0
            wip_count = sum(1 for s in all_stories if s.status in ['in_progress', 'review'])
            throughput = len(completed_stories) / max(1, days / 7)

            return TeamMetrics(
                total_stories=len(all_stories),
                stories_completed=len(completed_stories),
                total_points=total_points,
                points_delivered=done_points,
                completion_rate=round((done_points / total_points * 100) if total_points > 0 else 0, 1),
                avg_velocity=round(avg_velocity, 1),
                velocity_unit="points/sprint" if velocities else "points/week",
                velocity_trend=round(velocity_trend, 1),
                velocity_trend_direction=velocity_trend_direction.value,
                predictability_score=round(predictability, 1),
                collaboration_rate=round(collab_rate, 1),
                throughput=round(throughput, 2),
                throughput_unit="stories/week",
                avg_cycle_time_days=round(avg_cycle_time, 1),
                wip_count=wip_count,
                lead_time_avg_days=round(sum(lead_times) / len(lead_times) if lead_times else 0, 1),
                lead_time_p50_days=round(self.calculate_percentile(lead_times, 50), 1),
                lead_time_p85_days=round(self.calculate_percentile(lead_times, 85), 1),
                lead_time_p95_days=round(self.calculate_percentile(lead_times, 95), 1),
                status_distribution=status_counts,
                category_distribution=category_counts
            )
        finally:
            db.close()

    def get_story_type_metrics(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> List[StoryTypeMetrics]:
        """
        Calculate metrics broken down by story type/category.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            List of StoryTypeMetrics objects
        """
        db = self._get_db()
        try:
            query = db.query(self.Story)
            if project_id:
                query = query.filter(self.Story.project_id == project_id)
            if sprint_id:
                query = query.filter(self.Story.sprint_id == sprint_id)

            all_stories = query.all()

            # Group by category
            category_data: Dict[str, Dict] = {}

            for story in all_stories:
                cat = story.category or "feature"

                if cat not in category_data:
                    category_data[cat] = {
                        "count": 0,
                        "points": 0,
                        "completed": 0,
                        "cycle_times": [],
                        "estimated_points": 0,
                        "actual_points": 0
                    }

                data = category_data[cat]
                data["count"] += 1
                data["points"] += story.story_points or 0
                data["estimated_points"] += story.story_points or 0

                if story.status == 'done':
                    data["completed"] += 1
                    data["actual_points"] += story.story_points or 0
                    if story.started_at and story.completed_at:
                        cycle_days = (story.completed_at - story.started_at).total_seconds() / 3600 / 24
                        data["cycle_times"].append(cycle_days)

            result = []
            for cat, data in category_data.items():
                avg_cycle = sum(data["cycle_times"]) / len(data["cycle_times"]) if data["cycle_times"] else 0
                avg_points = data["points"] / data["count"] if data["count"] > 0 else 0
                estimation_accuracy = (data["actual_points"] / data["estimated_points"] * 100
                                      if data["estimated_points"] > 0 else 100)

                result.append(StoryTypeMetrics(
                    category=cat,
                    count=data["count"],
                    points=data["points"],
                    completed=data["completed"],
                    avg_cycle_time_days=round(avg_cycle, 1),
                    avg_points=round(avg_points, 1),
                    estimation_accuracy=round(estimation_accuracy, 1)
                ))

            result.sort(key=lambda x: x.count, reverse=True)
            return result
        finally:
            db.close()

    def get_velocity_history(
        self,
        project_id: Optional[str] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """
        Get velocity history for trend analysis.

        Args:
            project_id: Filter by project
            limit: Number of sprints to include

        Returns:
            Dictionary with history data and trend analysis
        """
        db = self._get_db()
        try:
            query = db.query(self.Sprint).filter(
                self.Sprint.status.in_(['completed', 'active'])
            )
            if project_id:
                query = query.filter(self.Sprint.project_id == project_id)

            sprints = query.order_by(self.Sprint.end_date.desc()).limit(limit).all()

            history = []
            for sprint in reversed(sprints):
                stories = db.query(self.Story).filter(
                    self.Story.sprint_id == sprint.sprint_id,
                    self.Story.status == 'done'
                ).all()
                points = sum(s.story_points or 0 for s in stories)

                history.append(SprintMetrics(
                    sprint_id=sprint.sprint_id,
                    sprint_name=sprint.name,
                    velocity=sprint.velocity or points,
                    capacity=sprint.capacity or 0,
                    stories_completed=len(stories),
                    stories_total=len(stories),
                    completion_percentage=round((sprint.velocity or points) / sprint.capacity * 100
                                               if sprint.capacity else 0, 1),
                    start_date=sprint.start_date.isoformat() if sprint.start_date else None,
                    end_date=sprint.end_date.isoformat() if sprint.end_date else None,
                    status=sprint.status
                ))

            velocities = [h.velocity for h in history if h.velocity > 0]
            avg_velocity = sum(velocities) / len(velocities) if velocities else 0
            trend = self.detect_trend(velocities)

            return {
                "history": [h.to_dict() for h in history],
                "avg_velocity": round(avg_velocity, 1),
                "trend": trend.value,
                "total_sprints": len(history),
                "min_velocity": min(velocities) if velocities else 0,
                "max_velocity": max(velocities) if velocities else 0
            }
        finally:
            db.close()

    def generate_alerts(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> List[ProductivityAlert]:
        """
        Generate alerts based on productivity thresholds.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            List of ProductivityAlert objects
        """
        alerts = []
        team_metrics = self.get_team_metrics(project_id, sprint_id, days)

        # WIP Alert
        if team_metrics.wip_count > 5:
            alerts.append(ProductivityAlert(
                alert_type="warning",
                category="wip",
                title="WIP Elevado",
                message=f"{team_metrics.wip_count} stories em progresso simultaneamente. Considere limitar o WIP.",
                threshold=5.0,
                current_value=float(team_metrics.wip_count)
            ))

        # Cycle Time Alert
        if team_metrics.avg_cycle_time_days > 14:
            alerts.append(ProductivityAlert(
                alert_type="warning",
                category="cycle_time",
                title="Cycle Time Alto",
                message=f"Media de {team_metrics.avg_cycle_time_days:.1f} dias por story. Identifique gargalos.",
                threshold=14.0,
                current_value=team_metrics.avg_cycle_time_days
            ))

        # Velocity Trend Alert
        if team_metrics.velocity_trend < -20:
            alerts.append(ProductivityAlert(
                alert_type="danger",
                category="velocity",
                title="Velocity em Queda",
                message=f"Reducao de {abs(team_metrics.velocity_trend):.0f}% em relacao ao periodo anterior.",
                threshold=-20.0,
                current_value=team_metrics.velocity_trend
            ))

        # Review Backlog Alert
        review_count = team_metrics.status_distribution.get('review', 0)
        if review_count > 3:
            alerts.append(ProductivityAlert(
                alert_type="info",
                category="review",
                title="Acumulo em Review",
                message=f"{review_count} stories aguardando revisao.",
                threshold=3.0,
                current_value=float(review_count)
            ))

        # Predictability Alert
        if team_metrics.predictability_score < 60:
            alerts.append(ProductivityAlert(
                alert_type="warning",
                category="predictability",
                title="Baixa Previsibilidade",
                message=f"Score de {team_metrics.predictability_score:.0f}%. Alta variancia na velocity.",
                threshold=60.0,
                current_value=team_metrics.predictability_score
            ))

        # Low Completion Rate Alert
        if team_metrics.completion_rate < 50 and team_metrics.total_stories > 5:
            alerts.append(ProductivityAlert(
                alert_type="warning",
                category="completion",
                title="Taxa de Conclusao Baixa",
                message=f"Apenas {team_metrics.completion_rate:.0f}% dos pontos foram entregues.",
                threshold=50.0,
                current_value=team_metrics.completion_rate
            ))

        # Positive Alerts
        if team_metrics.velocity_trend > 20:
            alerts.append(ProductivityAlert(
                alert_type="success",
                category="velocity",
                title="Velocity em Alta",
                message=f"Aumento de {team_metrics.velocity_trend:.0f}% em relacao ao periodo anterior!",
                threshold=20.0,
                current_value=team_metrics.velocity_trend
            ))

        if team_metrics.predictability_score > 90:
            alerts.append(ProductivityAlert(
                alert_type="success",
                category="predictability",
                title="Alta Previsibilidade",
                message=f"Score de {team_metrics.predictability_score:.0f}%. Time muito consistente!",
                threshold=90.0,
                current_value=team_metrics.predictability_score
            ))

        return alerts

    def generate_insights(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> List[ProductivityInsight]:
        """
        Generate rule-based insights about productivity.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            List of ProductivityInsight objects
        """
        insights = []

        team_metrics = self.get_team_metrics(project_id, sprint_id, days)
        dev_metrics = self.get_developer_metrics(project_id, sprint_id, days)
        type_metrics = self.get_story_type_metrics(project_id, sprint_id, days)

        # Velocity Insights
        if team_metrics.velocity_trend_direction == "increasing":
            insights.append(ProductivityInsight(
                category="velocity",
                title="Velocity Crescente",
                description="A velocidade do time esta aumentando consistentemente.",
                impact="high",
                data_point=f"+{team_metrics.velocity_trend:.0f}%",
                recommendation="Mantenha as praticas atuais que estao funcionando.",
                trend="positive"
            ))
        elif team_metrics.velocity_trend_direction == "decreasing":
            insights.append(ProductivityInsight(
                category="velocity",
                title="Velocity em Declinio",
                description="A velocidade do time esta diminuindo nos ultimos sprints.",
                impact="high",
                data_point=f"{team_metrics.velocity_trend:.0f}%",
                recommendation="Identifique impedimentos e realize uma retrospectiva focada.",
                trend="negative"
            ))

        # WIP Insights
        if team_metrics.wip_count > 5:
            insights.append(ProductivityInsight(
                category="process",
                title="WIP Muito Alto",
                description=f"{team_metrics.wip_count} itens em progresso pode indicar falta de foco.",
                impact="medium",
                data_point=f"{team_metrics.wip_count} itens",
                recommendation="Implemente limites de WIP para melhorar o fluxo.",
                trend="negative"
            ))

        # Cycle Time Insights
        if team_metrics.avg_cycle_time_days > 14:
            insights.append(ProductivityInsight(
                category="process",
                title="Cycle Time Elevado",
                description=f"Media de {team_metrics.avg_cycle_time_days:.0f} dias por story esta acima do ideal.",
                impact="high",
                data_point=f"{team_metrics.avg_cycle_time_days:.1f} dias",
                recommendation="Quebre stories em tamanhos menores e identifique gargalos.",
                trend="negative"
            ))
        elif team_metrics.avg_cycle_time_days > 0 and team_metrics.avg_cycle_time_days < 5:
            insights.append(ProductivityInsight(
                category="process",
                title="Cycle Time Excelente",
                description=f"Media de {team_metrics.avg_cycle_time_days:.1f} dias por story e muito boa.",
                impact="high",
                data_point=f"{team_metrics.avg_cycle_time_days:.1f} dias",
                recommendation="Continue mantendo stories bem definidas e de tamanho adequado.",
                trend="positive"
            ))

        # Developer Distribution Insights
        if len(dev_metrics) > 1:
            points_list = [d.points_delivered for d in dev_metrics if d.assignee != "Unassigned"]
            if points_list and len(points_list) > 1:
                max_points = max(points_list)
                min_points = min(points_list)
                if max_points > min_points * 3 and min_points > 0:
                    insights.append(ProductivityInsight(
                        category="collaboration",
                        title="Distribuicao Desigual",
                        description="Alguns desenvolvedores tem muito mais pontos entregues que outros.",
                        impact="medium",
                        data_point=f"Max: {max_points}, Min: {min_points}",
                        recommendation="Revise a distribuicao de trabalho para balancear a carga.",
                        trend="warning"
                    ))

        # Top Performer Insight
        if dev_metrics:
            top_dev = dev_metrics[0]
            if top_dev.points_delivered > 0 and top_dev.assignee != "Unassigned":
                insights.append(ProductivityInsight(
                    category="collaboration",
                    title="Top Contributor",
                    description=f"{top_dev.assignee} liderou com {top_dev.points_delivered} pontos entregues.",
                    impact="low",
                    data_point=f"{top_dev.points_delivered} pts",
                    recommendation=None,
                    trend="positive"
                ))

        # Story Type Insights
        bug_metrics = next((t for t in type_metrics if t.category == "bug"), None)
        feature_metrics = next((t for t in type_metrics if t.category == "feature"), None)

        if bug_metrics and feature_metrics and feature_metrics.count > 0:
            bug_ratio = bug_metrics.count / (feature_metrics.count + bug_metrics.count) * 100
            if bug_ratio > 30:
                insights.append(ProductivityInsight(
                    category="quality",
                    title="Alta Taxa de Bugs",
                    description=f"{bug_ratio:.0f}% do trabalho e correcao de bugs.",
                    impact="high",
                    data_point=f"{bug_ratio:.0f}% bugs",
                    recommendation="Invista em qualidade: testes automatizados e code review.",
                    trend="negative"
                ))

        # Tech Debt Insight
        tech_debt = next((t for t in type_metrics if t.category == "tech_debt"), None)
        total_stories = sum(t.count for t in type_metrics)
        if tech_debt and total_stories > 0:
            debt_ratio = tech_debt.count / total_stories * 100
            if debt_ratio > 20:
                insights.append(ProductivityInsight(
                    category="quality",
                    title="Foco em Debito Tecnico",
                    description=f"{debt_ratio:.0f}% do trabalho dedicado a debito tecnico.",
                    impact="medium",
                    data_point=f"{debt_ratio:.0f}% tech debt",
                    recommendation="Bom equilibrio! Mantenha a pratica de pagar debito regularmente.",
                    trend="positive"
                ))
            elif debt_ratio < 5 and total_stories > 10:
                insights.append(ProductivityInsight(
                    category="quality",
                    title="Debito Tecnico Negligenciado",
                    description=f"Apenas {debt_ratio:.0f}% do trabalho dedicado a debito tecnico.",
                    impact="medium",
                    data_point=f"{debt_ratio:.0f}% tech debt",
                    recommendation="Aloque tempo para enderecear debito tecnico antes que acumule.",
                    trend="warning"
                ))

        return insights

    def get_full_analytics(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> Dict[str, Any]:
        """
        Get comprehensive analytics including all metrics and insights.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            Dictionary with all analytics data
        """
        end_date = datetime.utcnow()
        start_date = end_date - timedelta(days=days)

        team_metrics = self.get_team_metrics(project_id, sprint_id, days)
        dev_metrics = self.get_developer_metrics(project_id, sprint_id, days)
        type_metrics = self.get_story_type_metrics(project_id, sprint_id, days)
        velocity_history = self.get_velocity_history(project_id)
        alerts = self.generate_alerts(project_id, sprint_id, days)
        insights = self.generate_insights(project_id, sprint_id, days)

        return {
            "period": {
                "start_date": start_date.isoformat(),
                "end_date": end_date.isoformat(),
                "days": days
            },
            "team_metrics": team_metrics.to_dict(),
            "developer_metrics": [d.to_dict() for d in dev_metrics],
            "top_contributors": [d.to_dict() for d in dev_metrics[:5]],
            "story_type_metrics": [t.to_dict() for t in type_metrics],
            "velocity_history": velocity_history,
            "alerts": [a.to_dict() for a in alerts],
            "insights": [i.to_dict() for i in insights],
            "generated_at": datetime.utcnow().isoformat()
        }

    def export_to_csv(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ) -> str:
        """
        Export analytics data to CSV format.

        Args:
            project_id: Filter by project
            sprint_id: Filter by sprint
            days: Number of days to analyze

        Returns:
            CSV string with analytics data
        """
        import csv
        from io import StringIO

        dev_metrics = self.get_developer_metrics(project_id, sprint_id, days)

        output = StringIO()
        writer = csv.writer(output)

        # Header
        writer.writerow([
            "Assignee", "Stories Completed", "Stories Total", "Points Delivered",
            "Points Total", "Avg Cycle Time (days)", "Completion Rate (%)",
            "Efficiency Score", "Bugs Fixed", "Features Delivered"
        ])

        # Data
        for dev in dev_metrics:
            writer.writerow([
                dev.assignee,
                dev.stories_completed,
                dev.stories_total,
                dev.points_delivered,
                dev.points_total,
                dev.avg_cycle_time_days,
                dev.completion_rate,
                dev.efficiency_score,
                dev.bugs_fixed,
                dev.features_delivered
            ])

        return output.getvalue()

    def compare_periods(
        self,
        project_id: Optional[str] = None,
        current_days: int = 14,
        previous_days: int = 14
    ) -> Dict[str, Any]:
        """
        Compare metrics between two time periods.

        Args:
            project_id: Filter by project
            current_days: Days in current period
            previous_days: Days in previous period

        Returns:
            Dictionary with comparison data
        """
        current_metrics = self.get_team_metrics(project_id, days=current_days)

        # Get previous period metrics
        db = self._get_db()
        try:
            end_date = datetime.utcnow() - timedelta(days=current_days)
            start_date = end_date - timedelta(days=previous_days)

            query = db.query(self.Story)
            if project_id:
                query = query.filter(self.Story.project_id == project_id)

            all_stories = query.all()
            prev_completed = [s for s in all_stories
                            if s.status == 'done' and s.completed_at
                            and start_date <= s.completed_at < end_date]

            prev_points = sum(s.story_points or 0 for s in prev_completed)
            prev_throughput = len(prev_completed) / max(1, previous_days / 7)

            # Calculate changes
            points_change = ((current_metrics.points_delivered - prev_points) / prev_points * 100
                            if prev_points > 0 else 0)
            throughput_change = ((current_metrics.throughput - prev_throughput) / prev_throughput * 100
                                if prev_throughput > 0 else 0)

            return {
                "current_period": {
                    "days": current_days,
                    "points_delivered": current_metrics.points_delivered,
                    "throughput": current_metrics.throughput,
                    "stories_completed": current_metrics.stories_completed
                },
                "previous_period": {
                    "days": previous_days,
                    "points_delivered": prev_points,
                    "throughput": round(prev_throughput, 2),
                    "stories_completed": len(prev_completed)
                },
                "changes": {
                    "points_change_percent": round(points_change, 1),
                    "throughput_change_percent": round(throughput_change, 1),
                    "stories_change": current_metrics.stories_completed - len(prev_completed)
                },
                "trend": "improving" if points_change > 10 else ("declining" if points_change < -10 else "stable")
            }
        finally:
            db.close()


# Factory function for easy instantiation
def create_analytics(db_session, story_model, sprint_model, story_task_model=None) -> ProductivityAnalytics:
    """
    Factory function to create ProductivityAnalytics instance.

    Args:
        db_session: SQLAlchemy SessionLocal
        story_model: Story model class
        sprint_model: Sprint model class
        story_task_model: Optional StoryTask model

    Returns:
        ProductivityAnalytics instance
    """
    return ProductivityAnalytics(db_session, story_model, sprint_model, story_task_model)
