# -*- coding: utf-8 -*-
"""
Capacity Planning Module - Issue #279
======================================

Enterprise feature for team capacity planning in sprints.

Features:
- TeamMember capacity tracking (hours available per sprint)
- Sprint capacity calculator
- Capacity vs commitment comparison
- Overallocation warnings

Usage:
    from factory.core.capacity_planning import (
        CapacityPlanningService,
        TeamMemberCapacity,
        SprintCapacitySummary
    )

    service = CapacityPlanningService(db_session)

    # Set team member capacity
    service.set_member_capacity(
        sprint_id="SPR-001",
        user_id=1,
        available_hours=40,
        focus_factor=0.8
    )

    # Get sprint capacity summary
    summary = service.get_sprint_capacity_summary("SPR-001")

    # Get capacity alerts
    alerts = service.get_capacity_alerts("SPR-001")
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, List, Dict, Any
from enum import Enum


class AlertSeverity(str, Enum):
    """Severity levels for capacity alerts"""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"


class AlertType(str, Enum):
    """Types of capacity alerts"""
    OVERALLOCATED = "overallocated"
    UNDERALLOCATED = "underallocated"
    NO_CAPACITY_SET = "no_capacity_set"
    UNBALANCED_LOAD = "unbalanced_load"
    SPRINT_AT_RISK = "sprint_at_risk"


@dataclass
class TeamMemberCapacity:
    """
    Represents a team member's capacity for a specific sprint.

    Attributes:
        user_id: User ID
        username: Username for display
        sprint_id: Sprint ID
        available_hours: Total hours available in sprint
        focus_factor: Percentage of time available for sprint work (0.0-1.0)
        allocated_hours: Hours already allocated to tasks
        remaining_hours: Hours still available
        story_points_capacity: Estimated story points capacity
        created_at: When capacity was set
        updated_at: Last update time
    """
    user_id: int
    username: str
    sprint_id: str
    available_hours: float = 40.0
    focus_factor: float = 0.8  # 80% focus on sprint work
    allocated_hours: float = 0.0
    remaining_hours: float = 0.0
    story_points_capacity: int = 0
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def __post_init__(self):
        """Calculate derived fields"""
        effective_hours = self.available_hours * self.focus_factor
        self.remaining_hours = max(0, effective_hours - self.allocated_hours)
        # Estimate: 1 story point = 4 hours (configurable)
        self.story_points_capacity = int(effective_hours / 4)

    @property
    def effective_hours(self) -> float:
        """Hours after applying focus factor"""
        return self.available_hours * self.focus_factor

    @property
    def utilization_percentage(self) -> float:
        """Percentage of capacity being used"""
        if self.effective_hours == 0:
            return 0.0
        return min(100.0, (self.allocated_hours / self.effective_hours) * 100)

    @property
    def is_overallocated(self) -> bool:
        """Check if member is overallocated"""
        return self.allocated_hours > self.effective_hours

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "user_id": self.user_id,
            "username": self.username,
            "sprint_id": self.sprint_id,
            "available_hours": self.available_hours,
            "focus_factor": self.focus_factor,
            "effective_hours": self.effective_hours,
            "allocated_hours": self.allocated_hours,
            "remaining_hours": self.remaining_hours,
            "story_points_capacity": self.story_points_capacity,
            "utilization_percentage": round(self.utilization_percentage, 1),
            "is_overallocated": self.is_overallocated,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }


@dataclass
class CapacityAlert:
    """
    Represents a capacity-related alert.

    Attributes:
        alert_type: Type of alert
        severity: Severity level
        message: Human-readable message
        sprint_id: Related sprint
        user_id: Related user (if applicable)
        details: Additional alert details
    """
    alert_type: AlertType
    severity: AlertSeverity
    message: str
    sprint_id: str
    user_id: Optional[int] = None
    details: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "alert_type": self.alert_type.value,
            "severity": self.severity.value,
            "message": self.message,
            "sprint_id": self.sprint_id,
            "user_id": self.user_id,
            "details": self.details,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }


@dataclass
class SprintCapacitySummary:
    """
    Summary of capacity for an entire sprint.

    Attributes:
        sprint_id: Sprint ID
        sprint_name: Sprint name
        total_available_hours: Sum of all team members' available hours
        total_effective_hours: After applying focus factors
        total_allocated_hours: Hours allocated to tasks
        total_remaining_hours: Hours still available
        total_story_points_capacity: Estimated total story points
        committed_story_points: Story points committed to sprint
        team_size: Number of team members
        average_utilization: Average utilization across team
        members: List of team member capacities
        alerts: List of capacity alerts
    """
    sprint_id: str
    sprint_name: str = ""
    total_available_hours: float = 0.0
    total_effective_hours: float = 0.0
    total_allocated_hours: float = 0.0
    total_remaining_hours: float = 0.0
    total_story_points_capacity: int = 0
    committed_story_points: int = 0
    team_size: int = 0
    average_utilization: float = 0.0
    members: List[TeamMemberCapacity] = field(default_factory=list)
    alerts: List[CapacityAlert] = field(default_factory=list)

    @property
    def commitment_ratio(self) -> float:
        """Ratio of committed to capacity story points"""
        if self.total_story_points_capacity == 0:
            return 0.0
        return self.committed_story_points / self.total_story_points_capacity

    @property
    def is_overcommitted(self) -> bool:
        """Check if sprint is overcommitted"""
        return self.commitment_ratio > 1.0

    @property
    def health_status(self) -> str:
        """Overall health status of sprint capacity"""
        ratio = self.commitment_ratio
        if ratio == 0:
            return "not_started"
        elif ratio < 0.7:
            return "under_capacity"
        elif ratio <= 1.0:
            return "healthy"
        elif ratio <= 1.2:
            return "at_risk"
        else:
            return "overcommitted"

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "sprint_id": self.sprint_id,
            "sprint_name": self.sprint_name,
            "total_available_hours": round(self.total_available_hours, 1),
            "total_effective_hours": round(self.total_effective_hours, 1),
            "total_allocated_hours": round(self.total_allocated_hours, 1),
            "total_remaining_hours": round(self.total_remaining_hours, 1),
            "total_story_points_capacity": self.total_story_points_capacity,
            "committed_story_points": self.committed_story_points,
            "commitment_ratio": round(self.commitment_ratio, 2),
            "team_size": self.team_size,
            "average_utilization": round(self.average_utilization, 1),
            "is_overcommitted": self.is_overcommitted,
            "health_status": self.health_status,
            "members": [m.to_dict() for m in self.members],
            "alerts": [a.to_dict() for a in self.alerts],
            "alerts_count": {
                "info": len([a for a in self.alerts if a.severity == AlertSeverity.INFO]),
                "warning": len([a for a in self.alerts if a.severity == AlertSeverity.WARNING]),
                "critical": len([a for a in self.alerts if a.severity == AlertSeverity.CRITICAL])
            }
        }


class CapacityPlanningService:
    """
    Service for managing team capacity planning in sprints.

    Provides:
    - Team member capacity tracking
    - Sprint capacity calculations
    - Capacity vs commitment comparison
    - Overallocation warnings and alerts
    """

    # Default hours per story point (configurable)
    HOURS_PER_STORY_POINT = 4.0

    # Thresholds for alerts
    OVERALLOCATION_WARNING_THRESHOLD = 0.9  # 90% utilization
    OVERALLOCATION_CRITICAL_THRESHOLD = 1.0  # 100% utilization
    UNDERALLOCATION_THRESHOLD = 0.5  # Below 50% is underutilized
    UNBALANCED_LOAD_THRESHOLD = 30  # 30% difference between min/max utilization

    def __init__(self, db_session=None, hours_per_story_point: float = 4.0):
        """
        Initialize capacity planning service.

        Args:
            db_session: SQLAlchemy database session
            hours_per_story_point: Hours per story point for estimation
        """
        self.db = db_session
        self.HOURS_PER_STORY_POINT = hours_per_story_point

        # In-memory storage for capacity data (can be replaced with DB)
        self._capacities: Dict[str, Dict[int, TeamMemberCapacity]] = {}

    def set_member_capacity(
        self,
        sprint_id: str,
        user_id: int,
        username: str = "Unknown",
        available_hours: float = 40.0,
        focus_factor: float = 0.8,
        allocated_hours: float = 0.0
    ) -> TeamMemberCapacity:
        """
        Set or update team member capacity for a sprint.

        Args:
            sprint_id: Sprint ID
            user_id: User ID
            username: Username for display
            available_hours: Total hours available
            focus_factor: Focus percentage (0.0-1.0)
            allocated_hours: Hours already allocated

        Returns:
            TeamMemberCapacity object
        """
        # Validate inputs
        if focus_factor < 0 or focus_factor > 1:
            raise ValueError("focus_factor must be between 0 and 1")
        if available_hours < 0:
            raise ValueError("available_hours must be non-negative")
        if allocated_hours < 0:
            raise ValueError("allocated_hours must be non-negative")

        # Initialize sprint dict if needed
        if sprint_id not in self._capacities:
            self._capacities[sprint_id] = {}

        # Check if updating existing capacity
        now = datetime.utcnow()
        created_at = now
        if user_id in self._capacities[sprint_id]:
            created_at = self._capacities[sprint_id][user_id].created_at

        capacity = TeamMemberCapacity(
            user_id=user_id,
            username=username,
            sprint_id=sprint_id,
            available_hours=available_hours,
            focus_factor=focus_factor,
            allocated_hours=allocated_hours,
            created_at=created_at,
            updated_at=now
        )

        self._capacities[sprint_id][user_id] = capacity

        return capacity

    def get_member_capacity(
        self,
        sprint_id: str,
        user_id: int
    ) -> Optional[TeamMemberCapacity]:
        """
        Get team member capacity for a sprint.

        Args:
            sprint_id: Sprint ID
            user_id: User ID

        Returns:
            TeamMemberCapacity or None
        """
        if sprint_id not in self._capacities:
            return None
        return self._capacities[sprint_id].get(user_id)

    def remove_member_capacity(
        self,
        sprint_id: str,
        user_id: int
    ) -> bool:
        """
        Remove team member capacity from a sprint.

        Args:
            sprint_id: Sprint ID
            user_id: User ID

        Returns:
            True if removed, False if not found
        """
        if sprint_id not in self._capacities:
            return False
        if user_id not in self._capacities[sprint_id]:
            return False

        del self._capacities[sprint_id][user_id]
        return True

    def update_allocated_hours(
        self,
        sprint_id: str,
        user_id: int,
        allocated_hours: float
    ) -> Optional[TeamMemberCapacity]:
        """
        Update allocated hours for a team member.

        Args:
            sprint_id: Sprint ID
            user_id: User ID
            allocated_hours: New allocated hours

        Returns:
            Updated TeamMemberCapacity or None
        """
        capacity = self.get_member_capacity(sprint_id, user_id)
        if not capacity:
            return None

        return self.set_member_capacity(
            sprint_id=sprint_id,
            user_id=user_id,
            username=capacity.username,
            available_hours=capacity.available_hours,
            focus_factor=capacity.focus_factor,
            allocated_hours=allocated_hours
        )

    def get_sprint_members(self, sprint_id: str) -> List[TeamMemberCapacity]:
        """
        Get all team members with capacity set for a sprint.

        Args:
            sprint_id: Sprint ID

        Returns:
            List of TeamMemberCapacity objects
        """
        if sprint_id not in self._capacities:
            return []
        return list(self._capacities[sprint_id].values())

    def calculate_sprint_capacity(
        self,
        sprint_id: str,
        committed_story_points: int = 0
    ) -> SprintCapacitySummary:
        """
        Calculate total sprint capacity from all team members.

        Args:
            sprint_id: Sprint ID
            committed_story_points: Story points committed to sprint

        Returns:
            SprintCapacitySummary object
        """
        members = self.get_sprint_members(sprint_id)

        total_available = sum(m.available_hours for m in members)
        total_effective = sum(m.effective_hours for m in members)
        total_allocated = sum(m.allocated_hours for m in members)
        total_remaining = sum(m.remaining_hours for m in members)
        total_sp_capacity = sum(m.story_points_capacity for m in members)

        # Calculate average utilization
        utilizations = [m.utilization_percentage for m in members]
        avg_utilization = sum(utilizations) / len(utilizations) if utilizations else 0.0

        # Get sprint name from DB if available
        sprint_name = ""
        if self.db:
            try:
                from factory.database.models import Sprint
                sprint = self.db.query(Sprint).filter(
                    Sprint.sprint_id == sprint_id
                ).first()
                if sprint:
                    sprint_name = sprint.name
                    if committed_story_points == 0:
                        committed_story_points = sprint.capacity or 0
            except Exception:
                pass

        summary = SprintCapacitySummary(
            sprint_id=sprint_id,
            sprint_name=sprint_name,
            total_available_hours=total_available,
            total_effective_hours=total_effective,
            total_allocated_hours=total_allocated,
            total_remaining_hours=total_remaining,
            total_story_points_capacity=total_sp_capacity,
            committed_story_points=committed_story_points,
            team_size=len(members),
            average_utilization=avg_utilization,
            members=members
        )

        # Generate alerts
        summary.alerts = self._generate_alerts(summary)

        return summary

    def get_sprint_capacity_summary(
        self,
        sprint_id: str,
        committed_story_points: int = 0
    ) -> SprintCapacitySummary:
        """
        Get comprehensive capacity summary for a sprint.

        Alias for calculate_sprint_capacity with better naming.

        Args:
            sprint_id: Sprint ID
            committed_story_points: Story points committed to sprint

        Returns:
            SprintCapacitySummary object
        """
        return self.calculate_sprint_capacity(sprint_id, committed_story_points)

    def get_capacity_alerts(
        self,
        sprint_id: str,
        severity: Optional[AlertSeverity] = None
    ) -> List[CapacityAlert]:
        """
        Get capacity alerts for a sprint.

        Args:
            sprint_id: Sprint ID
            severity: Filter by severity (optional)

        Returns:
            List of CapacityAlert objects
        """
        summary = self.get_sprint_capacity_summary(sprint_id)
        alerts = summary.alerts

        if severity:
            alerts = [a for a in alerts if a.severity == severity]

        return alerts

    def _generate_alerts(self, summary: SprintCapacitySummary) -> List[CapacityAlert]:
        """
        Generate capacity alerts based on summary data.

        Args:
            summary: SprintCapacitySummary object

        Returns:
            List of CapacityAlert objects
        """
        alerts = []
        sprint_id = summary.sprint_id

        # Check for no capacity set
        if summary.team_size == 0:
            alerts.append(CapacityAlert(
                alert_type=AlertType.NO_CAPACITY_SET,
                severity=AlertSeverity.WARNING,
                message="Nenhuma capacidade definida para este sprint",
                sprint_id=sprint_id,
                details={"recommendation": "Defina a capacidade de cada membro do time"}
            ))
            return alerts

        # Check individual member overallocation
        for member in summary.members:
            utilization = member.utilization_percentage

            if utilization >= self.OVERALLOCATION_CRITICAL_THRESHOLD * 100:
                alerts.append(CapacityAlert(
                    alert_type=AlertType.OVERALLOCATED,
                    severity=AlertSeverity.CRITICAL,
                    message=f"{member.username} esta sobrecarregado ({utilization:.0f}% alocado)",
                    sprint_id=sprint_id,
                    user_id=member.user_id,
                    details={
                        "allocated_hours": member.allocated_hours,
                        "effective_hours": member.effective_hours,
                        "excess_hours": member.allocated_hours - member.effective_hours,
                        "utilization": utilization
                    }
                ))
            elif utilization >= self.OVERALLOCATION_WARNING_THRESHOLD * 100:
                alerts.append(CapacityAlert(
                    alert_type=AlertType.OVERALLOCATED,
                    severity=AlertSeverity.WARNING,
                    message=f"{member.username} proximo do limite ({utilization:.0f}% alocado)",
                    sprint_id=sprint_id,
                    user_id=member.user_id,
                    details={
                        "allocated_hours": member.allocated_hours,
                        "effective_hours": member.effective_hours,
                        "remaining_hours": member.remaining_hours,
                        "utilization": utilization
                    }
                ))
            elif utilization < self.UNDERALLOCATION_THRESHOLD * 100 and member.allocated_hours > 0:
                alerts.append(CapacityAlert(
                    alert_type=AlertType.UNDERALLOCATED,
                    severity=AlertSeverity.INFO,
                    message=f"{member.username} pode assumir mais tarefas ({utilization:.0f}% alocado)",
                    sprint_id=sprint_id,
                    user_id=member.user_id,
                    details={
                        "allocated_hours": member.allocated_hours,
                        "remaining_hours": member.remaining_hours,
                        "utilization": utilization
                    }
                ))

        # Check for unbalanced load
        if len(summary.members) >= 2:
            utilizations = [m.utilization_percentage for m in summary.members]
            max_util = max(utilizations)
            min_util = min(utilizations)

            if max_util - min_util > self.UNBALANCED_LOAD_THRESHOLD:
                alerts.append(CapacityAlert(
                    alert_type=AlertType.UNBALANCED_LOAD,
                    severity=AlertSeverity.WARNING,
                    message=f"Carga desbalanceada no time (variacao de {max_util - min_util:.0f}%)",
                    sprint_id=sprint_id,
                    details={
                        "max_utilization": max_util,
                        "min_utilization": min_util,
                        "difference": max_util - min_util,
                        "recommendation": "Redistribua tarefas entre os membros do time"
                    }
                ))

        # Check sprint commitment
        if summary.is_overcommitted:
            excess = summary.committed_story_points - summary.total_story_points_capacity
            alerts.append(CapacityAlert(
                alert_type=AlertType.SPRINT_AT_RISK,
                severity=AlertSeverity.CRITICAL,
                message=f"Sprint sobrecomprometido: {summary.committed_story_points} SP comprometidos vs {summary.total_story_points_capacity} SP capacidade",
                sprint_id=sprint_id,
                details={
                    "committed": summary.committed_story_points,
                    "capacity": summary.total_story_points_capacity,
                    "excess_points": excess,
                    "commitment_ratio": summary.commitment_ratio,
                    "recommendation": f"Remova {excess} story points do sprint ou aumente a capacidade do time"
                }
            ))
        elif summary.commitment_ratio > 0.9:
            alerts.append(CapacityAlert(
                alert_type=AlertType.SPRINT_AT_RISK,
                severity=AlertSeverity.WARNING,
                message=f"Sprint proximo do limite: {summary.commitment_ratio * 100:.0f}% da capacidade comprometida",
                sprint_id=sprint_id,
                details={
                    "committed": summary.committed_story_points,
                    "capacity": summary.total_story_points_capacity,
                    "commitment_ratio": summary.commitment_ratio
                }
            ))

        return alerts

    def estimate_story_points_from_hours(self, hours: float) -> int:
        """
        Estimate story points from hours.

        Args:
            hours: Number of hours

        Returns:
            Estimated story points
        """
        return int(hours / self.HOURS_PER_STORY_POINT)

    def estimate_hours_from_story_points(self, story_points: int) -> float:
        """
        Estimate hours from story points.

        Args:
            story_points: Number of story points

        Returns:
            Estimated hours
        """
        return story_points * self.HOURS_PER_STORY_POINT

    def get_recommended_team_load(
        self,
        sprint_id: str,
        target_utilization: float = 0.8
    ) -> Dict[str, Any]:
        """
        Get recommended story points per team member to achieve target utilization.

        Args:
            sprint_id: Sprint ID
            target_utilization: Target utilization percentage (0.0-1.0)

        Returns:
            Dictionary with recommendations per member
        """
        members = self.get_sprint_members(sprint_id)
        recommendations = {}
        total_recommended_sp = 0

        for member in members:
            target_hours = member.effective_hours * target_utilization
            recommended_sp = self.estimate_story_points_from_hours(target_hours)

            recommendations[member.username] = {
                "user_id": member.user_id,
                "current_allocated_hours": member.allocated_hours,
                "target_hours": target_hours,
                "recommended_story_points": recommended_sp,
                "current_utilization": member.utilization_percentage,
                "target_utilization": target_utilization * 100
            }
            total_recommended_sp += recommended_sp

        return {
            "sprint_id": sprint_id,
            "target_utilization": target_utilization * 100,
            "total_recommended_story_points": total_recommended_sp,
            "members": recommendations
        }


# =============================================================================
# SINGLETON INSTANCE
# =============================================================================

_capacity_service: Optional[CapacityPlanningService] = None


def get_capacity_service(db_session=None) -> CapacityPlanningService:
    """
    Get or create capacity planning service singleton.

    Args:
        db_session: Optional database session

    Returns:
        CapacityPlanningService instance
    """
    global _capacity_service
    if _capacity_service is None:
        _capacity_service = CapacityPlanningService(db_session)
    elif db_session is not None:
        _capacity_service.db = db_session
    return _capacity_service
