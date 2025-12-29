# -*- coding: utf-8 -*-
"""
Analytics Routes - Issue #50
============================
API routes for Executive BI Dashboard analytics
"""
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, HTTPException
from fastapi.responses import HTMLResponse, Response

# Create router
analytics_router = APIRouter(prefix="/api/analytics", tags=["Analytics"])

# Try to import analytics service
try:
    from factory.core.analytics import AnalyticsService
    HAS_ANALYTICS = True
except ImportError:
    HAS_ANALYTICS = False
    AnalyticsService = None


@analytics_router.get("/summary")
def get_analytics_summary(project_id: Optional[str] = None, days: int = 30):
    """Retorna resumo executivo com KPIs principais"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return service.get_executive_summary(project_id, days)
    finally:
        service.close()


@analytics_router.get("/velocity")
def get_velocity(project_id: Optional[str] = None, limit: int = 10):
    """Retorna velocity por sprint"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return {
            "sprints": service.get_velocity_by_sprint(project_id, limit),
            "average": service.get_average_velocity(project_id, limit)
        }
    finally:
        service.close()


@analytics_router.get("/throughput")
def get_throughput_data(project_id: Optional[str] = None, days: int = 30, group_by: str = "week"):
    """Retorna throughput por periodo"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return {
            "data": service.get_throughput(project_id, days, group_by),
            "average": service.get_weekly_throughput_average(project_id)
        }
    finally:
        service.close()


@analytics_router.get("/cycle-time")
def get_cycle_time_data(project_id: Optional[str] = None, days: int = 30):
    """Retorna cycle time e lead time"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return {
            "cycle_time": service.get_cycle_time(project_id, days),
            "lead_time": service.get_lead_time(project_id, days)
        }
    finally:
        service.close()


@analytics_router.get("/burndown/{sprint_id}")
def get_burndown_data(sprint_id: str):
    """Retorna dados para grafico de burndown"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return service.get_burndown_data(sprint_id)
    finally:
        service.close()


@analytics_router.get("/status-distribution")
def get_status_distribution(project_id: Optional[str] = None):
    """Retorna distribuicao de stories por status"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return service.get_story_status_distribution(project_id)
    finally:
        service.close()


@analytics_router.get("/agents")
def get_agent_productivity_data(days: int = 30):
    """Retorna produtividade dos agentes/workers"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        return {
            "agents": service.get_agent_productivity(days),
            "task_completion": service.get_task_completion_by_agent(days=days)
        }
    finally:
        service.close()


@analytics_router.get("/export")
def export_analytics_data(project_id: Optional[str] = None, days: int = 30, format: str = "json"):
    """Exporta dados de analytics"""
    if not HAS_ANALYTICS:
        raise HTTPException(503, "Analytics service not available")
    service = AnalyticsService()
    try:
        data = service.export_to_dict(project_id, days)
        if format == "json":
            return data
        else:
            return {
                "format": format,
                "data": data,
                "message": f"Export to {format} available via client-side processing"
            }
    finally:
        service.close()
