"""
Analytics Routes - API v1
=========================

Endpoints para metricas, KPIs e relatorios.

Endpoints:
- /api/v1/analytics/metrics - Metricas gerais
- /api/v1/analytics/kpis - KPIs por projeto/sprint
- /api/v1/analytics/reports - Relatorios exportaveis
- /api/v1/analytics/velocity - Velocity do time
- /api/v1/analytics/burndown - Burndown charts
"""

from datetime import datetime, timedelta
from typing import List, Optional
import uuid

from fastapi import APIRouter, Depends, Query, Request, HTTPException, status
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session
from sqlalchemy import func

from .schemas import APIResponse, RequestMeta
from factory.database.connection import SessionLocal

router = APIRouter()


# =============================================================================
# MODELS
# =============================================================================

class MetricValue(BaseModel):
    """Valor de metrica"""
    name: str
    value: float
    unit: Optional[str] = None
    change_percent: Optional[float] = None
    trend: Optional[str] = None  # up, down, stable


class KPIData(BaseModel):
    """Dados de KPI"""
    kpi_name: str
    current_value: float
    target_value: Optional[float] = None
    previous_value: Optional[float] = None
    unit: Optional[str] = None
    status: str = "on_track"  # on_track, at_risk, behind


class VelocityPoint(BaseModel):
    """Ponto de velocity"""
    sprint_id: str
    sprint_name: str
    planned_points: int
    delivered_points: int
    completion_rate: float


class BurndownPoint(BaseModel):
    """Ponto do burndown"""
    date: str
    remaining_points: int
    ideal_points: float


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_request_meta(request: Request) -> RequestMeta:
    return RequestMeta(
        request_id=request.headers.get("X-Request-ID", str(uuid.uuid4())),
        timestamp=datetime.utcnow(),
        tenant_id=request.headers.get("X-Tenant-ID"),
        api_version="v1"
    )


# =============================================================================
# METRICS
# =============================================================================

@router.get("/metrics", response_model=APIResponse, tags=["Metrics"])
async def get_metrics(
    request: Request,
    project_id: Optional[str] = Query(None, description="Filtrar por projeto"),
    period: str = Query("week", description="Periodo: day, week, month, quarter"),
    db: Session = Depends(get_db)
):
    """
    Retorna metricas gerais do sistema.

    Metricas incluem:
    - Total de stories/tasks
    - Taxa de conclusao
    - Tempo medio de ciclo
    - Bugs abertos
    """
    from factory.database.models import Story, StoryTask, Project

    # Calcular periodo
    now = datetime.utcnow()
    if period == "day":
        start_date = now - timedelta(days=1)
    elif period == "week":
        start_date = now - timedelta(weeks=1)
    elif period == "month":
        start_date = now - timedelta(days=30)
    else:  # quarter
        start_date = now - timedelta(days=90)

    # Query base
    story_query = db.query(Story)
    task_query = db.query(StoryTask)

    if project_id:
        story_query = story_query.filter(Story.project_id == project_id)
        task_query = task_query.join(Story).filter(Story.project_id == project_id)

    # Metricas
    total_stories = story_query.count()
    completed_stories = story_query.filter(Story.status == "done").count()
    in_progress_stories = story_query.filter(Story.status == "in_progress").count()

    total_tasks = task_query.count()
    completed_tasks = task_query.filter(StoryTask.status == "completed").count()

    # Taxa de conclusao
    completion_rate = (completed_stories / total_stories * 100) if total_stories > 0 else 0

    # Stories no periodo
    stories_created = story_query.filter(Story.created_at >= start_date).count()
    stories_completed = story_query.filter(
        Story.completed_at >= start_date,
        Story.completed_at != None
    ).count()

    # Story points
    total_points = db.query(func.sum(Story.story_points)).filter(
        Story.status == "done"
    ).scalar() or 0

    metrics = [
        MetricValue(name="total_stories", value=total_stories, unit="stories"),
        MetricValue(name="completed_stories", value=completed_stories, unit="stories"),
        MetricValue(name="in_progress_stories", value=in_progress_stories, unit="stories"),
        MetricValue(name="total_tasks", value=total_tasks, unit="tasks"),
        MetricValue(name="completed_tasks", value=completed_tasks, unit="tasks"),
        MetricValue(name="completion_rate", value=round(completion_rate, 1), unit="%"),
        MetricValue(name="stories_created_period", value=stories_created, unit="stories"),
        MetricValue(name="stories_completed_period", value=stories_completed, unit="stories"),
        MetricValue(name="total_points_delivered", value=total_points, unit="points"),
    ]

    return APIResponse(
        data={
            "period": period,
            "start_date": start_date.isoformat(),
            "end_date": now.isoformat(),
            "metrics": [m.model_dump() for m in metrics]
        },
        meta=get_request_meta(request)
    )


@router.get("/kpis", response_model=APIResponse, tags=["KPIs"])
async def get_kpis(
    request: Request,
    project_id: Optional[str] = Query(None),
    sprint_id: Optional[str] = Query(None),
    db: Session = Depends(get_db)
):
    """
    Retorna KPIs chave para projeto/sprint.

    KPIs incluem:
    - Velocity
    - Throughput
    - Cycle Time
    - Lead Time
    - Bug Rate
    """
    from factory.database.models import Story, Sprint

    kpis = []

    # Query base
    story_query = db.query(Story)
    if project_id:
        story_query = story_query.filter(Story.project_id == project_id)
    if sprint_id:
        story_query = story_query.filter(Story.sprint_id == sprint_id)

    # Velocity (pontos entregues por sprint)
    completed_points = db.query(func.sum(Story.story_points)).filter(
        Story.status == "done"
    )
    if project_id:
        completed_points = completed_points.filter(Story.project_id == project_id)

    velocity = completed_points.scalar() or 0

    # Sprints para calcular velocity media
    sprint_count = db.query(func.count(Sprint.id.distinct()))
    if project_id:
        sprint_count = sprint_count.filter(Sprint.project_id == project_id)
    num_sprints = sprint_count.scalar() or 1

    avg_velocity = velocity / num_sprints

    kpis.append(KPIData(
        kpi_name="velocity",
        current_value=round(avg_velocity, 1),
        target_value=None,
        unit="points/sprint",
        status="on_track"
    ))

    # Throughput (stories por semana)
    last_week = datetime.utcnow() - timedelta(weeks=1)
    stories_last_week = story_query.filter(
        Story.completed_at >= last_week,
        Story.completed_at != None
    ).count()

    kpis.append(KPIData(
        kpi_name="throughput",
        current_value=stories_last_week,
        unit="stories/week",
        status="on_track"
    ))

    # Cycle Time medio (dias de in_progress ate done)
    completed_stories = story_query.filter(
        Story.status == "done",
        Story.started_at != None,
        Story.completed_at != None
    ).all()

    if completed_stories:
        cycle_times = [
            (s.completed_at - s.started_at).days
            for s in completed_stories
        ]
        avg_cycle_time = sum(cycle_times) / len(cycle_times)
    else:
        avg_cycle_time = 0

    kpis.append(KPIData(
        kpi_name="cycle_time",
        current_value=round(avg_cycle_time, 1),
        target_value=5.0,
        unit="days",
        status="on_track" if avg_cycle_time <= 5 else "at_risk"
    ))

    # Lead Time medio (dias de criacao ate done)
    if completed_stories:
        lead_times = [
            (s.completed_at - s.created_at).days
            for s in completed_stories
        ]
        avg_lead_time = sum(lead_times) / len(lead_times)
    else:
        avg_lead_time = 0

    kpis.append(KPIData(
        kpi_name="lead_time",
        current_value=round(avg_lead_time, 1),
        target_value=14.0,
        unit="days",
        status="on_track" if avg_lead_time <= 14 else "at_risk"
    ))

    # Work in Progress (WIP)
    wip = story_query.filter(Story.status.in_(["in_progress", "review", "testing"])).count()

    kpis.append(KPIData(
        kpi_name="wip",
        current_value=wip,
        target_value=10,
        unit="stories",
        status="on_track" if wip <= 10 else "at_risk"
    ))

    return APIResponse(
        data={
            "project_id": project_id,
            "sprint_id": sprint_id,
            "kpis": [k.model_dump() for k in kpis]
        },
        meta=get_request_meta(request)
    )


@router.get("/velocity", response_model=APIResponse, tags=["Velocity"])
async def get_velocity(
    request: Request,
    project_id: str = Query(..., description="ID do projeto"),
    num_sprints: int = Query(6, ge=1, le=20, description="Numero de sprints"),
    db: Session = Depends(get_db)
):
    """
    Retorna historico de velocity.
    """
    from factory.database.models import Story, Sprint

    # Buscar sprints
    sprints = db.query(Sprint).filter(
        Sprint.project_id == project_id,
        Sprint.status == "completed"
    ).order_by(Sprint.created_at.desc()).limit(num_sprints).all()

    velocity_data = []

    for sprint in reversed(sprints):
        # Pontos planejados
        planned = db.query(func.sum(Story.story_points)).filter(
            Story.sprint_id == sprint.sprint_id
        ).scalar() or 0

        # Pontos entregues
        delivered = db.query(func.sum(Story.story_points)).filter(
            Story.sprint_id == sprint.sprint_id,
            Story.status == "done"
        ).scalar() or 0

        completion_rate = (delivered / planned * 100) if planned > 0 else 0

        velocity_data.append(VelocityPoint(
            sprint_id=sprint.sprint_id,
            sprint_name=sprint.name,
            planned_points=planned,
            delivered_points=delivered,
            completion_rate=round(completion_rate, 1)
        ))

    # Calcular medias
    if velocity_data:
        avg_velocity = sum(v.delivered_points for v in velocity_data) / len(velocity_data)
        avg_completion = sum(v.completion_rate for v in velocity_data) / len(velocity_data)
    else:
        avg_velocity = 0
        avg_completion = 0

    return APIResponse(
        data={
            "project_id": project_id,
            "velocity_history": [v.model_dump() for v in velocity_data],
            "average_velocity": round(avg_velocity, 1),
            "average_completion_rate": round(avg_completion, 1)
        },
        meta=get_request_meta(request)
    )


@router.get("/burndown", response_model=APIResponse, tags=["Burndown"])
async def get_burndown(
    request: Request,
    sprint_id: str = Query(..., description="ID do sprint"),
    db: Session = Depends(get_db)
):
    """
    Retorna dados do burndown chart.
    """
    from factory.database.models import Story, Sprint

    # Buscar sprint
    sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()
    if not sprint:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Sprint {sprint_id} nao encontrado"
        )

    # Calcular total de pontos
    total_points = db.query(func.sum(Story.story_points)).filter(
        Story.sprint_id == sprint_id
    ).scalar() or 0

    # Periodo do sprint
    start_date = sprint.start_date or sprint.created_at
    end_date = sprint.end_date or (start_date + timedelta(weeks=2))
    total_days = (end_date - start_date).days

    burndown_data = []
    current_date = start_date

    while current_date <= end_date:
        # Pontos completados ate esta data
        completed = db.query(func.sum(Story.story_points)).filter(
            Story.sprint_id == sprint_id,
            Story.status == "done",
            Story.completed_at <= current_date
        ).scalar() or 0

        remaining = total_points - completed

        # Linha ideal
        days_passed = (current_date - start_date).days
        ideal_remaining = total_points - (total_points / total_days * days_passed)

        burndown_data.append(BurndownPoint(
            date=current_date.strftime("%Y-%m-%d"),
            remaining_points=remaining,
            ideal_points=round(max(0, ideal_remaining), 1)
        ))

        current_date += timedelta(days=1)

    return APIResponse(
        data={
            "sprint_id": sprint_id,
            "sprint_name": sprint.name,
            "total_points": total_points,
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
            "burndown": [b.model_dump() for b in burndown_data]
        },
        meta=get_request_meta(request)
    )


@router.get("/reports/summary", response_model=APIResponse, tags=["Reports"])
async def get_summary_report(
    request: Request,
    project_id: Optional[str] = Query(None),
    period: str = Query("month", description="Periodo: week, month, quarter"),
    db: Session = Depends(get_db)
):
    """
    Gera relatorio resumido.
    """
    from factory.database.models import Story, StoryTask, Project

    now = datetime.utcnow()
    if period == "week":
        start_date = now - timedelta(weeks=1)
    elif period == "month":
        start_date = now - timedelta(days=30)
    else:
        start_date = now - timedelta(days=90)

    # Query base
    story_query = db.query(Story)
    if project_id:
        story_query = story_query.filter(Story.project_id == project_id)

    # Estatisticas
    total_stories = story_query.count()
    completed = story_query.filter(Story.status == "done").count()
    in_progress = story_query.filter(Story.status == "in_progress").count()

    # Periodo
    created_period = story_query.filter(Story.created_at >= start_date).count()
    completed_period = story_query.filter(
        Story.completed_at >= start_date,
        Story.completed_at != None
    ).count()

    # Por status
    status_distribution = {}
    for s in ["backlog", "ready", "in_progress", "review", "testing", "done"]:
        count = story_query.filter(Story.status == s).count()
        status_distribution[s] = count

    # Por prioridade
    priority_distribution = {}
    for p in ["low", "medium", "high", "urgent"]:
        count = story_query.filter(Story.priority == p).count()
        priority_distribution[p] = count

    return APIResponse(
        data={
            "period": period,
            "start_date": start_date.isoformat(),
            "end_date": now.isoformat(),
            "summary": {
                "total_stories": total_stories,
                "completed_stories": completed,
                "in_progress_stories": in_progress,
                "stories_created_period": created_period,
                "stories_completed_period": completed_period,
                "completion_rate": round(completed / total_stories * 100, 1) if total_stories > 0 else 0
            },
            "status_distribution": status_distribution,
            "priority_distribution": priority_distribution
        },
        meta=get_request_meta(request)
    )
