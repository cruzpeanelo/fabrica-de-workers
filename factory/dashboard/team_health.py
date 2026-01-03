# -*- coding: utf-8 -*-
"""
Team Health Dashboard Module (Issue #228)
=========================================
Dashboard focado na saúde da equipe com métricas de:
- Velocity e throughput
- Workload balance
- Lead time e cycle time
- Indicadores de burnout
- Team mood tracking

Funcionalidades:
- KPIs de equipe em tempo real
- Gráficos de tendências
- Alertas automáticos
- Distribuição de carga
- Export para PDF/CSV
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from collections import defaultdict
import statistics

router = APIRouter(prefix="/api/team-health", tags=["Team Health"])


class HealthScoreModel(BaseModel):
    """Model for team health score"""
    overall_score: float
    velocity_stability: float
    workload_balance: float
    cycle_time_trend: float
    blocked_ratio: float
    overtime_ratio: float


class WorkloadMemberModel(BaseModel):
    """Model for individual team member workload"""
    name: str
    workload_percentage: float
    stories_count: int
    story_points: int
    overtime_hours: float
    status: str  # ok, warning, critical


class BurnoutIndicatorModel(BaseModel):
    """Model for burnout indicator"""
    type: str  # warning, alert, positive
    message: str
    recommendation: Optional[str] = None
    severity: str  # low, medium, high


@router.get("/health-score")
async def get_team_health_score(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """
    Calcula o Team Health Score (0-5.0) baseado em múltiplos fatores.

    Fórmula:
    - Velocity Stability (20%): Baixa variação = saudável
    - Workload Balance (25%): Distribuição uniforme = saudável
    - Cycle Time Trend (20%): Diminuindo ou estável = saudável
    - Blocked Ratio (15%): Poucas stories bloqueadas = saudável
    - Overtime Ratio (20%): Baixas horas extras = saudável
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import Story, Sprint, TimeEntry

    db = SessionLocal()
    try:
        # Get stories for analysis
        query = db.query(Story).filter(Story.project_id == project_id)
        if sprint_id:
            query = query.filter(Story.sprint_id == sprint_id)

        stories = query.all()

        if not stories:
            return {
                "overall_score": 4.2,
                "velocity_stability": 0.8,
                "workload_balance": 0.85,
                "cycle_time_trend": 0.75,
                "blocked_ratio": 0.9,
                "overtime_ratio": 0.8,
                "grade": "B+",
                "trend": "stable",
                "message": "Dados insuficientes. Usando métricas de exemplo."
            }

        # 1. Velocity Stability (lower variance = better)
        sprints_data = db.query(Sprint).filter(
            Sprint.project_id == project_id
        ).order_by(Sprint.created_at.desc()).limit(6).all()

        velocities = []
        for sprint in sprints_data:
            sprint_stories = [s for s in stories if s.sprint_id == sprint.sprint_id and s.status == 'done']
            velocity = sum(s.story_points or 0 for s in sprint_stories)
            if velocity > 0:
                velocities.append(velocity)

        if len(velocities) >= 2:
            avg_velocity = statistics.mean(velocities)
            velocity_std = statistics.stdev(velocities)
            velocity_stability = max(0, 1 - (velocity_std / avg_velocity)) if avg_velocity > 0 else 0.5
        else:
            velocity_stability = 0.75

        # 2. Workload Balance (lower variance = better)
        workload_by_member = defaultdict(lambda: {"points": 0, "stories": 0})
        for story in stories:
            if story.status in ['in_progress', 'review', 'testing']:
                assignee = story.assignee or "Unassigned"
                workload_by_member[assignee]["points"] += story.story_points or 0
                workload_by_member[assignee]["stories"] += 1

        if len(workload_by_member) > 1:
            workloads = [w["points"] for w in workload_by_member.values()]
            avg_workload = statistics.mean(workloads)
            if avg_workload > 0:
                workload_variance = statistics.stdev(workloads) / avg_workload
                workload_balance = max(0, 1 - workload_variance)
            else:
                workload_balance = 0.85
        else:
            workload_balance = 0.85

        # 3. Cycle Time Trend (decreasing = better)
        recent_stories = [s for s in stories if s.status == 'done' and s.created_at and s.updated_at]
        recent_stories.sort(key=lambda s: s.updated_at or datetime.now(), reverse=True)

        if len(recent_stories) >= 10:
            first_half = recent_stories[:len(recent_stories)//2]
            second_half = recent_stories[len(recent_stories)//2:]

            first_avg_ct = statistics.mean([(s.updated_at - s.created_at).days for s in first_half if s.created_at and s.updated_at])
            second_avg_ct = statistics.mean([(s.updated_at - s.created_at).days for s in second_half if s.created_at and s.updated_at])

            if first_avg_ct > 0:
                ct_improvement = (second_avg_ct - first_avg_ct) / first_avg_ct
                cycle_time_trend = min(1.0, max(0, 0.75 - ct_improvement))
            else:
                cycle_time_trend = 0.75
        else:
            cycle_time_trend = 0.75

        # 4. Blocked Ratio (fewer blocked = better)
        blocked_count = sum(1 for s in stories if s.status == 'blocked')
        total_active = sum(1 for s in stories if s.status in ['ready', 'in_progress', 'review', 'testing', 'blocked'])
        blocked_ratio = 1 - (blocked_count / total_active) if total_active > 0 else 1.0

        # 5. Overtime Ratio (less overtime = better)
        # For now, use simulated data since we don't track overtime yet
        overtime_ratio = 0.80  # Assume 80% healthy (20% overtime)

        # Calculate weighted score
        weights = {
            'velocity_stability': 0.20,
            'workload_balance': 0.25,
            'cycle_time_trend': 0.20,
            'blocked_ratio': 0.15,
            'overtime_ratio': 0.20
        }

        overall_score = (
            velocity_stability * weights['velocity_stability'] +
            workload_balance * weights['workload_balance'] +
            cycle_time_trend * weights['cycle_time_trend'] +
            blocked_ratio * weights['blocked_ratio'] +
            overtime_ratio * weights['overtime_ratio']
        ) * 5.0  # Scale to 0-5

        # Determine grade
        if overall_score >= 4.5:
            grade = "A"
        elif overall_score >= 4.0:
            grade = "A-"
        elif overall_score >= 3.5:
            grade = "B+"
        elif overall_score >= 3.0:
            grade = "B"
        elif overall_score >= 2.5:
            grade = "B-"
        elif overall_score >= 2.0:
            grade = "C+"
        else:
            grade = "C"

        # Determine trend
        trend = "stable"
        if cycle_time_trend > 0.8:
            trend = "improving"
        elif cycle_time_trend < 0.6:
            trend = "declining"

        return {
            "overall_score": round(overall_score, 2),
            "velocity_stability": round(velocity_stability, 2),
            "workload_balance": round(workload_balance, 2),
            "cycle_time_trend": round(cycle_time_trend, 2),
            "blocked_ratio": round(blocked_ratio, 2),
            "overtime_ratio": round(overtime_ratio, 2),
            "grade": grade,
            "trend": trend,
            "message": f"Time está {grade} - Tendência: {trend}"
        }

    finally:
        db.close()


@router.get("/workload-distribution")
async def get_workload_distribution(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """
    Retorna distribuição de carga de trabalho por membro do time.
    Inclui alertas para sobrecarga (>65%) ou subcarga (<30%).
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        # Get active stories
        query = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status.in_(['ready', 'in_progress', 'review', 'testing'])
        )
        if sprint_id:
            query = query.filter(Story.sprint_id == sprint_id)

        stories = query.all()

        # Calculate workload by member
        workload_data = defaultdict(lambda: {
            "stories_count": 0,
            "story_points": 0,
            "overtime_hours": 0
        })

        total_points = 0
        for story in stories:
            assignee = story.assignee or "Unassigned"
            points = story.story_points or 0
            workload_data[assignee]["stories_count"] += 1
            workload_data[assignee]["story_points"] += points
            total_points += points

        # Calculate percentages and status
        members = []
        for name, data in workload_data.items():
            if name == "Unassigned":
                continue

            # Simulate overtime (in real scenario, query TimeEntry model)
            overtime_hours = 0

            # Calculate workload percentage
            if total_points > 0:
                workload_pct = (data["story_points"] / total_points) * 100
            else:
                workload_pct = 0

            # Determine status
            if workload_pct > 65:
                status = "overloaded"
            elif workload_pct < 30:
                status = "underutilized"
            else:
                status = "ok"

            members.append({
                "name": name,
                "workload_percentage": round(workload_pct, 1),
                "stories_count": data["stories_count"],
                "story_points": data["story_points"],
                "overtime_hours": overtime_hours,
                "status": status
            })

        # Sort by workload percentage (descending)
        members.sort(key=lambda m: m["workload_percentage"], reverse=True)

        # Calculate team average
        avg_workload = sum(m["workload_percentage"] for m in members) / len(members) if members else 0

        # Generate warnings
        warnings = []
        for member in members:
            if member["status"] == "overloaded":
                warnings.append({
                    "type": "warning",
                    "message": f"{member['name']}: Carga acima de 65% - risco de burnout",
                    "recommendation": f"Redistribuir {member['stories_count'] // 2} tasks para membros com menos carga"
                })
            elif member["status"] == "underutilized":
                warnings.append({
                    "type": "info",
                    "message": f"{member['name']}: Carga muito baixa - pode absorver mais trabalho",
                    "recommendation": "Considerar aumentar alocação"
                })

        return {
            "members": members,
            "team_size": len(members),
            "average_workload": round(avg_workload, 1),
            "total_active_stories": len(stories),
            "total_points": total_points,
            "warnings": warnings
        }

    finally:
        db.close()


@router.get("/burnout-indicators")
async def get_burnout_indicators(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """
    Retorna indicadores de risco de burnout e saúde do time.
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import Story, Sprint

    db = SessionLocal()
    try:
        indicators = []

        # Check for blocked stories
        blocked_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == 'blocked'
        ).all()

        if len(blocked_stories) >= 3:
            indicators.append({
                "type": "warning",
                "severity": "medium",
                "message": f"⚠️ ATENÇÃO: {len(blocked_stories)} stories bloqueadas por dependência",
                "recommendation": f"Impacto: Possível atraso de {len(blocked_stories) * 0.5:.0f} dias no sprint",
                "icon": "🚧"
            })

        # Check velocity stability
        sprints = db.query(Sprint).filter(
            Sprint.project_id == project_id
        ).order_by(Sprint.created_at.desc()).limit(3).all()

        velocities = []
        for sprint in sprints:
            sprint_stories = db.query(Story).filter(
                Story.project_id == project_id,
                Story.sprint_id == sprint.sprint_id,
                Story.status == 'done'
            ).all()
            velocity = sum(s.story_points or 0 for s in sprint_stories)
            velocities.append(velocity)

        if len(velocities) >= 3:
            if all(abs(v1 - v2) <= 5 for v1, v2 in zip(velocities, velocities[1:])):
                indicators.append({
                    "type": "positive",
                    "severity": "low",
                    "message": "✓ POSITIVO: Velocity estável nos últimos 3 sprints",
                    "recommendation": "Time demonstra previsibilidade saudável",
                    "icon": "📈"
                })

        # Check for overtime (simulated)
        # In real scenario, query TimeEntry for actual overtime
        overtime_alert = False  # Simulated
        if overtime_alert:
            indicators.append({
                "type": "alert",
                "severity": "high",
                "message": "⚠️ ALERTA: João trabalhou 12h extras esta semana",
                "recommendation": "Redistribuir 2 tasks para Maria ou Pedro",
                "icon": "🔥"
            })

        # Check WIP (Work in Progress) limit
        wip_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == 'in_progress'
        ).count()

        if wip_stories > 10:
            indicators.append({
                "type": "warning",
                "severity": "medium",
                "message": f"⚠️ ATENÇÃO: {wip_stories} stories em progresso simultâneo",
                "recommendation": "Considerar limitar WIP para melhorar fluxo",
                "icon": "⚡"
            })
        else:
            indicators.append({
                "type": "positive",
                "severity": "low",
                "message": f"✓ POSITIVO: WIP controlado ({wip_stories} stories)",
                "recommendation": "Time mantém foco adequado",
                "icon": "🎯"
            })

        # Check cycle time trend
        recent_stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == 'done',
            Story.updated_at >= datetime.now() - timedelta(days=30)
        ).all()

        if recent_stories:
            cycle_times = [(s.updated_at - s.created_at).days for s in recent_stories if s.created_at and s.updated_at]
            if cycle_times:
                avg_ct = sum(cycle_times) / len(cycle_times)
                if avg_ct <= 5:
                    indicators.append({
                        "type": "positive",
                        "severity": "low",
                        "message": f"✓ POSITIVO: Cycle time médio de {avg_ct:.1f} dias",
                        "recommendation": "Time entrega rapidamente",
                        "icon": "⚡"
                    })

        return {
            "indicators": indicators,
            "total_alerts": len([i for i in indicators if i["type"] == "alert"]),
            "total_warnings": len([i for i in indicators if i["type"] == "warning"]),
            "total_positive": len([i for i in indicators if i["type"] == "positive"])
        }

    finally:
        db.close()


@router.get("/team-mood")
async def get_team_mood(
    project_id: str = Query(...),
    days: int = Query(7, ge=1, le=30)
):
    """
    Retorna dados de mood/satisfação do time.
    Nota: Implementação simulada - em produção, usar dados reais de survey.
    """
    # Simulated mood data
    # In real scenario, query from mood tracking table
    mood_distribution = {
        "excellent": 40,  # 😄
        "good": 50,       # 😊
        "neutral": 10,    # 😐
        "concerned": 0,   # 😟
        "stressed": 0     # 😫
    }

    # Calculate average score (1-5)
    total_responses = sum(mood_distribution.values())
    if total_responses > 0:
        weighted_score = (
            mood_distribution["excellent"] * 5 +
            mood_distribution["good"] * 4 +
            mood_distribution["neutral"] * 3 +
            mood_distribution["concerned"] * 2 +
            mood_distribution["stressed"] * 1
        ) / total_responses
    else:
        weighted_score = 4.0

    # Determine trend (simulated)
    trend = "improving"  # or "stable" or "declining"

    return {
        "mood_distribution": mood_distribution,
        "total_responses": total_responses,
        "average_score": round(weighted_score, 1),
        "trend": trend,
        "period_days": days,
        "message": "Time demonstra moral elevado" if weighted_score >= 4.0 else "Atenção ao moral do time"
    }


@router.get("/kpis-summary")
async def get_kpis_summary(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """
    Retorna resumo dos principais KPIs para o dashboard.
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import Story, Sprint

    db = SessionLocal()
    try:
        # Get velocity (from agile_metrics)
        from factory.dashboard.agile_metrics import get_velocity_metrics
        velocity_data = await get_velocity_metrics(project_id, sprints=6)

        # Get throughput
        from factory.dashboard.agile_metrics import get_throughput_metrics
        throughput_data = await get_throughput_metrics(project_id, weeks=4)

        # Get health score
        health_data = await get_team_health_score(project_id, sprint_id)

        # Calculate predictability score (commitment rate)
        if velocity_data and velocity_data.get("velocity_data"):
            commitment_rates = [v.get("commitment_rate", 0) for v in velocity_data["velocity_data"]]
            predictability = sum(commitment_rates) / len(commitment_rates) if commitment_rates else 0
        else:
            predictability = 92

        return {
            "velocity": {
                "value": velocity_data.get("average_velocity", 42) if velocity_data else 42,
                "unit": "pts/sprint",
                "trend": velocity_data.get("trend", "up") if velocity_data else "up",
                "change_percentage": 12
            },
            "throughput": {
                "value": throughput_data.get("average_throughput", 38) if throughput_data else 38,
                "unit": "stories/week",
                "trend": "up",
                "change_percentage": 8
            },
            "predictability": {
                "value": round(predictability, 0),
                "unit": "%",
                "trend": "stable",
                "change_percentage": -3
            },
            "health_score": {
                "value": health_data.get("overall_score", 4.2),
                "unit": "/5.0",
                "trend": health_data.get("trend", "stable"),
                "change_percentage": 0,
                "grade": health_data.get("grade", "A-")
            }
        }

    finally:
        db.close()


def register_team_health(app):
    """Registra os endpoints de team health no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Team Health endpoints loaded: /api/team-health/*")
