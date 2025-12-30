# -*- coding: utf-8 -*-
"""
Capacity Planning API Routes - Issue #279
==========================================

REST API endpoints for team capacity planning in sprints.

Endpoints:
    POST   /api/capacity/sprints/{sprint_id}/members         - Set team member capacity
    GET    /api/capacity/sprints/{sprint_id}/members         - List team member capacities
    GET    /api/capacity/sprints/{sprint_id}/members/{user_id} - Get member capacity
    PUT    /api/capacity/sprints/{sprint_id}/members/{user_id} - Update member capacity
    DELETE /api/capacity/sprints/{sprint_id}/members/{user_id} - Remove member capacity
    GET    /api/capacity/sprints/{sprint_id}/summary         - Get sprint capacity summary
    GET    /api/capacity/sprints/{sprint_id}/alerts          - Get capacity alerts
    POST   /api/capacity/sprints/{sprint_id}/allocate        - Update allocated hours
    GET    /api/capacity/sprints/{sprint_id}/recommendations - Get load recommendations
"""

from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Depends, Query
from pydantic import BaseModel, Field

# Capacity Planning Service
from factory.core.capacity_planning import (
    CapacityPlanningService,
    get_capacity_service,
    AlertSeverity
)

# Database
try:
    from factory.database.connection import SessionLocal
except ImportError:
    SessionLocal = None


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/capacity", tags=["capacity-planning"])


# =============================================================================
# SCHEMAS
# =============================================================================

class MemberCapacityCreate(BaseModel):
    """Schema for setting team member capacity"""
    user_id: int = Field(..., description="User ID")
    username: str = Field(..., min_length=1, description="Username for display")
    available_hours: float = Field(40.0, ge=0, description="Total hours available in sprint")
    focus_factor: float = Field(0.8, ge=0, le=1, description="Focus percentage (0.0-1.0)")
    allocated_hours: float = Field(0.0, ge=0, description="Hours already allocated")


class MemberCapacityUpdate(BaseModel):
    """Schema for updating team member capacity"""
    available_hours: Optional[float] = Field(None, ge=0, description="Total hours available")
    focus_factor: Optional[float] = Field(None, ge=0, le=1, description="Focus percentage")
    allocated_hours: Optional[float] = Field(None, ge=0, description="Hours allocated")


class AllocationUpdate(BaseModel):
    """Schema for updating allocated hours"""
    user_id: int = Field(..., description="User ID")
    allocated_hours: float = Field(..., ge=0, description="New allocated hours")


class BulkCapacityCreate(BaseModel):
    """Schema for setting capacity for multiple members"""
    members: List[MemberCapacityCreate] = Field(..., min_length=1)


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    """Dependency for database session"""
    if SessionLocal is None:
        return None
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_service(db=Depends(get_db)) -> CapacityPlanningService:
    """Dependency for capacity planning service"""
    return get_capacity_service(db)


# =============================================================================
# ENDPOINTS - MEMBER CAPACITY
# =============================================================================

@router.post("/sprints/{sprint_id}/members", response_model=Dict[str, Any])
async def set_member_capacity(
    sprint_id: str,
    data: MemberCapacityCreate,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Set team member capacity for a sprint.

    Creates or updates capacity allocation for a team member.
    """
    try:
        capacity = service.set_member_capacity(
            sprint_id=sprint_id,
            user_id=data.user_id,
            username=data.username,
            available_hours=data.available_hours,
            focus_factor=data.focus_factor,
            allocated_hours=data.allocated_hours
        )
        return {
            "success": True,
            "capacity": capacity.to_dict()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/sprints/{sprint_id}/members/bulk", response_model=Dict[str, Any])
async def set_bulk_capacity(
    sprint_id: str,
    data: BulkCapacityCreate,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Set capacity for multiple team members at once.
    """
    capacities = []
    errors = []

    for member in data.members:
        try:
            capacity = service.set_member_capacity(
                sprint_id=sprint_id,
                user_id=member.user_id,
                username=member.username,
                available_hours=member.available_hours,
                focus_factor=member.focus_factor,
                allocated_hours=member.allocated_hours
            )
            capacities.append(capacity.to_dict())
        except ValueError as e:
            errors.append({
                "user_id": member.user_id,
                "error": str(e)
            })

    return {
        "success": len(errors) == 0,
        "capacities": capacities,
        "errors": errors,
        "total_set": len(capacities),
        "total_errors": len(errors)
    }


@router.get("/sprints/{sprint_id}/members", response_model=Dict[str, Any])
async def list_member_capacities(
    sprint_id: str,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    List all team member capacities for a sprint.
    """
    members = service.get_sprint_members(sprint_id)

    return {
        "sprint_id": sprint_id,
        "members": [m.to_dict() for m in members],
        "total": len(members)
    }


@router.get("/sprints/{sprint_id}/members/{user_id}", response_model=Dict[str, Any])
async def get_member_capacity(
    sprint_id: str,
    user_id: int,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Get capacity for a specific team member.
    """
    capacity = service.get_member_capacity(sprint_id, user_id)

    if not capacity:
        raise HTTPException(
            status_code=404,
            detail=f"Capacity not found for user {user_id} in sprint {sprint_id}"
        )

    return {
        "capacity": capacity.to_dict()
    }


@router.put("/sprints/{sprint_id}/members/{user_id}", response_model=Dict[str, Any])
async def update_member_capacity(
    sprint_id: str,
    user_id: int,
    data: MemberCapacityUpdate,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Update capacity for a team member.
    """
    existing = service.get_member_capacity(sprint_id, user_id)

    if not existing:
        raise HTTPException(
            status_code=404,
            detail=f"Capacity not found for user {user_id} in sprint {sprint_id}"
        )

    try:
        capacity = service.set_member_capacity(
            sprint_id=sprint_id,
            user_id=user_id,
            username=existing.username,
            available_hours=data.available_hours if data.available_hours is not None else existing.available_hours,
            focus_factor=data.focus_factor if data.focus_factor is not None else existing.focus_factor,
            allocated_hours=data.allocated_hours if data.allocated_hours is not None else existing.allocated_hours
        )
        return {
            "success": True,
            "capacity": capacity.to_dict()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.delete("/sprints/{sprint_id}/members/{user_id}", response_model=Dict[str, Any])
async def remove_member_capacity(
    sprint_id: str,
    user_id: int,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Remove team member capacity from a sprint.
    """
    success = service.remove_member_capacity(sprint_id, user_id)

    if not success:
        raise HTTPException(
            status_code=404,
            detail=f"Capacity not found for user {user_id} in sprint {sprint_id}"
        )

    return {
        "success": True,
        "message": f"Capacity removed for user {user_id}"
    }


# =============================================================================
# ENDPOINTS - SPRINT CAPACITY SUMMARY
# =============================================================================

@router.get("/sprints/{sprint_id}/summary", response_model=Dict[str, Any])
async def get_sprint_capacity_summary(
    sprint_id: str,
    committed_story_points: int = Query(0, ge=0, description="Story points committed to sprint"),
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Get comprehensive capacity summary for a sprint.

    Returns total capacity, allocation status, and health indicators.
    """
    summary = service.get_sprint_capacity_summary(sprint_id, committed_story_points)

    return {
        "summary": summary.to_dict()
    }


# =============================================================================
# ENDPOINTS - CAPACITY ALERTS
# =============================================================================

@router.get("/sprints/{sprint_id}/alerts", response_model=Dict[str, Any])
async def get_capacity_alerts(
    sprint_id: str,
    severity: Optional[str] = Query(None, description="Filter by severity: info, warning, critical"),
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Get capacity alerts for a sprint.

    Alerts include:
    - Overallocated team members
    - Underallocated team members
    - Unbalanced workload
    - Sprint at risk (overcommitted)
    """
    alert_severity = None
    if severity:
        try:
            alert_severity = AlertSeverity(severity.lower())
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Invalid severity: {severity}. Must be one of: info, warning, critical"
            )

    alerts = service.get_capacity_alerts(sprint_id, alert_severity)

    return {
        "sprint_id": sprint_id,
        "alerts": [a.to_dict() for a in alerts],
        "total": len(alerts),
        "by_severity": {
            "info": len([a for a in alerts if a.severity == AlertSeverity.INFO]),
            "warning": len([a for a in alerts if a.severity == AlertSeverity.WARNING]),
            "critical": len([a for a in alerts if a.severity == AlertSeverity.CRITICAL])
        }
    }


# =============================================================================
# ENDPOINTS - ALLOCATION
# =============================================================================

@router.post("/sprints/{sprint_id}/allocate", response_model=Dict[str, Any])
async def update_allocation(
    sprint_id: str,
    data: AllocationUpdate,
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Update allocated hours for a team member.

    Use this to update allocation without changing available hours or focus factor.
    """
    capacity = service.update_allocated_hours(
        sprint_id=sprint_id,
        user_id=data.user_id,
        allocated_hours=data.allocated_hours
    )

    if not capacity:
        raise HTTPException(
            status_code=404,
            detail=f"Capacity not found for user {data.user_id} in sprint {sprint_id}"
        )

    return {
        "success": True,
        "capacity": capacity.to_dict()
    }


# =============================================================================
# ENDPOINTS - RECOMMENDATIONS
# =============================================================================

@router.get("/sprints/{sprint_id}/recommendations", response_model=Dict[str, Any])
async def get_load_recommendations(
    sprint_id: str,
    target_utilization: float = Query(0.8, ge=0.1, le=1.0, description="Target utilization (0.1-1.0)"),
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Get recommended story points per team member.

    Calculates optimal load distribution to achieve target utilization.
    """
    members = service.get_sprint_members(sprint_id)

    if not members:
        raise HTTPException(
            status_code=404,
            detail=f"No team members found for sprint {sprint_id}"
        )

    recommendations = service.get_recommended_team_load(sprint_id, target_utilization)

    return {
        "recommendations": recommendations
    }


# =============================================================================
# ENDPOINTS - UTILITIES
# =============================================================================

@router.get("/estimate/hours-to-points", response_model=Dict[str, Any])
async def estimate_story_points(
    hours: float = Query(..., ge=0, description="Number of hours"),
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Estimate story points from hours.
    """
    points = service.estimate_story_points_from_hours(hours)

    return {
        "hours": hours,
        "story_points": points,
        "hours_per_point": service.HOURS_PER_STORY_POINT
    }


@router.get("/estimate/points-to-hours", response_model=Dict[str, Any])
async def estimate_hours(
    story_points: int = Query(..., ge=0, description="Number of story points"),
    service: CapacityPlanningService = Depends(get_service)
):
    """
    Estimate hours from story points.
    """
    hours = service.estimate_hours_from_story_points(story_points)

    return {
        "story_points": story_points,
        "hours": hours,
        "hours_per_point": service.HOURS_PER_STORY_POINT
    }
