# -*- coding: utf-8 -*-
"""
Analytics Service - Business Intelligence para Plataforma E
================================================================
Metricas Agile: Velocity, Throughput, Cycle Time, Lead Time
KPIs executivos para gestores e stakeholders
"""
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from collections import defaultdict
from statistics import mean, stdev
import json

# Database imports
try:
    from factory.database.connection import SessionLocal
    from factory.database.models import (
        Story, StoryStatus, StoryTask, StoryTaskStatus,
        Sprint, Project, Job, JobStatus, Worker, ExecutionLog
    )
except ImportError:
    from database.connection import SessionLocal
    from database.models import (
        Story, StoryStatus, StoryTask, StoryTaskStatus,
        Sprint, Project, Job, JobStatus, Worker, ExecutionLog
    )


class AnalyticsService:
    """
    Servico de Analytics e Business Intelligence

    Metricas calculadas:
    - Velocity: Story points entregues por sprint
    - Throughput: Stories entregues por periodo
    - Cycle Time: Tempo em cada coluna do Kanban
    - Lead Time: Tempo total do backlog ate done
    - WIP (Work in Progress): Items em andamento
    - Story Completion Rate: Taxa de conclusao
    - Agent Productivity: Produtividade dos agentes/workers
    """

    def __init__(self, db_session=None):
        self.db = db_session
        self._should_close = False
        if not self.db:
            self.db = SessionLocal()
            self._should_close = True

    def __del__(self):
        if self._should_close and self.db:
            self.db.close()

    def close(self):
        """Fecha a sessao do banco de dados"""
        if self._should_close and self.db:
            self.db.close()

    # =========================================================================
    # VELOCITY METRICS
    # =========================================================================

    def get_velocity_by_sprint(self, project_id: str = None, limit: int = 10) -> List[Dict]:
        """
        Calcula velocity (story points entregues) por sprint

        Returns:
            List de dicts com sprint_id, sprint_name, velocity, capacity, completion_rate
        """
        query = self.db.query(Sprint)
        if project_id:
            query = query.filter(Sprint.project_id == project_id)

        sprints = query.order_by(Sprint.start_date.desc()).limit(limit).all()

        result = []
        for sprint in sprints:
            # Buscar stories completadas no sprint
            stories = self.db.query(Story).filter(
                Story.sprint_id == sprint.sprint_id,
                Story.status == StoryStatus.DONE.value
            ).all()

            velocity = sum(s.story_points or 0 for s in stories)
            completion_rate = (velocity / sprint.capacity * 100) if sprint.capacity > 0 else 0

            result.append({
                "sprint_id": sprint.sprint_id,
                "sprint_name": sprint.name,
                "velocity": velocity,
                "capacity": sprint.capacity or 0,
                "completion_rate": round(completion_rate, 1),
                "start_date": sprint.start_date.isoformat() if sprint.start_date else None,
                "end_date": sprint.end_date.isoformat() if sprint.end_date else None,
                "status": sprint.status
            })

        return result

    def get_average_velocity(self, project_id: str = None, sprints: int = 5) -> Dict:
        """
        Calcula velocity media dos ultimos N sprints

        Returns:
            Dict com average, min, max, trend
        """
        velocities = self.get_velocity_by_sprint(project_id, limit=sprints)

        if not velocities:
            return {"average": 0, "min": 0, "max": 0, "trend": "stable", "sprints_analyzed": 0}

        points = [v["velocity"] for v in velocities]
        avg = mean(points) if points else 0

        # Calcular tendencia (comparando primeira metade com segunda metade)
        if len(points) >= 4:
            first_half = mean(points[:len(points)//2])
            second_half = mean(points[len(points)//2:])
            if second_half > first_half * 1.1:
                trend = "increasing"
            elif second_half < first_half * 0.9:
                trend = "decreasing"
            else:
                trend = "stable"
        else:
            trend = "stable"

        return {
            "average": round(avg, 1),
            "min": min(points),
            "max": max(points),
            "trend": trend,
            "sprints_analyzed": len(points),
            "std_deviation": round(stdev(points), 1) if len(points) > 1 else 0
        }

    # =========================================================================
    # THROUGHPUT METRICS
    # =========================================================================

    def get_throughput(self, project_id: str = None, days: int = 30, group_by: str = "week") -> List[Dict]:
        """
        Calcula throughput (stories entregues) por periodo

        Args:
            project_id: Filtrar por projeto
            days: Periodo de analise em dias
            group_by: 'day', 'week', ou 'month'

        Returns:
            List de dicts com period, count, story_points
        """
        start_date = datetime.utcnow() - timedelta(days=days)

        query = self.db.query(Story).filter(
            Story.status == StoryStatus.DONE.value,
            Story.completed_at >= start_date
        )

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()

        # Agrupar por periodo
        grouped = defaultdict(lambda: {"count": 0, "story_points": 0})

        for story in stories:
            if story.completed_at:
                if group_by == "day":
                    key = story.completed_at.strftime("%Y-%m-%d")
                elif group_by == "week":
                    key = story.completed_at.strftime("%Y-W%W")
                else:  # month
                    key = story.completed_at.strftime("%Y-%m")

                grouped[key]["count"] += 1
                grouped[key]["story_points"] += story.story_points or 0

        # Converter para lista ordenada
        result = [
            {"period": k, "count": v["count"], "story_points": v["story_points"]}
            for k, v in sorted(grouped.items())
        ]

        return result

    def get_weekly_throughput_average(self, project_id: str = None, weeks: int = 8) -> Dict:
        """
        Calcula media de throughput semanal
        """
        throughput = self.get_throughput(project_id, days=weeks * 7, group_by="week")

        if not throughput:
            return {"average_count": 0, "average_points": 0, "weeks_analyzed": 0}

        counts = [t["count"] for t in throughput]
        points = [t["story_points"] for t in throughput]

        return {
            "average_count": round(mean(counts), 1) if counts else 0,
            "average_points": round(mean(points), 1) if points else 0,
            "weeks_analyzed": len(throughput)
        }

    # =========================================================================
    # CYCLE TIME & LEAD TIME
    # =========================================================================

    def get_cycle_time(self, project_id: str = None, days: int = 30) -> Dict:
        """
        Calcula Cycle Time (tempo em cada coluna do Kanban)

        Returns:
            Dict com average_cycle_time, by_status (tempo medio por coluna)
        """
        start_date = datetime.utcnow() - timedelta(days=days)

        query = self.db.query(Story).filter(
            Story.status == StoryStatus.DONE.value,
            Story.completed_at >= start_date,
            Story.started_at.isnot(None)
        )

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()

        if not stories:
            return {
                "average_cycle_time_hours": 0,
                "average_cycle_time_days": 0,
                "stories_analyzed": 0
            }

        cycle_times = []
        for story in stories:
            if story.started_at and story.completed_at:
                delta = story.completed_at - story.started_at
                cycle_times.append(delta.total_seconds() / 3600)  # Em horas

        avg_hours = mean(cycle_times) if cycle_times else 0

        return {
            "average_cycle_time_hours": round(avg_hours, 1),
            "average_cycle_time_days": round(avg_hours / 24, 1),
            "min_hours": round(min(cycle_times), 1) if cycle_times else 0,
            "max_hours": round(max(cycle_times), 1) if cycle_times else 0,
            "stories_analyzed": len(cycle_times)
        }

    def get_lead_time(self, project_id: str = None, days: int = 30) -> Dict:
        """
        Calcula Lead Time (tempo total do backlog ate done)

        Returns:
            Dict com average_lead_time, min, max
        """
        start_date = datetime.utcnow() - timedelta(days=days)

        query = self.db.query(Story).filter(
            Story.status == StoryStatus.DONE.value,
            Story.completed_at >= start_date
        )

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()

        if not stories:
            return {
                "average_lead_time_hours": 0,
                "average_lead_time_days": 0,
                "stories_analyzed": 0
            }

        lead_times = []
        for story in stories:
            if story.created_at and story.completed_at:
                delta = story.completed_at - story.created_at
                lead_times.append(delta.total_seconds() / 3600)  # Em horas

        avg_hours = mean(lead_times) if lead_times else 0

        return {
            "average_lead_time_hours": round(avg_hours, 1),
            "average_lead_time_days": round(avg_hours / 24, 1),
            "min_hours": round(min(lead_times), 1) if lead_times else 0,
            "max_hours": round(max(lead_times), 1) if lead_times else 0,
            "stories_analyzed": len(lead_times)
        }

    # =========================================================================
    # STORY STATUS DISTRIBUTION
    # =========================================================================

    def get_story_status_distribution(self, project_id: str = None) -> Dict:
        """
        Retorna distribuicao de stories por status

        Returns:
            Dict com contagem por status e percentuais
        """
        query = self.db.query(Story)
        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()

        distribution = defaultdict(int)
        total_points = defaultdict(int)

        for story in stories:
            distribution[story.status] += 1
            total_points[story.status] += story.story_points or 0

        total = len(stories)

        result = {
            "total_stories": total,
            "by_status": {},
            "story_points_by_status": {}
        }

        for status in StoryStatus:
            count = distribution.get(status.value, 0)
            points = total_points.get(status.value, 0)
            result["by_status"][status.value] = {
                "count": count,
                "percentage": round(count / total * 100, 1) if total > 0 else 0
            }
            result["story_points_by_status"][status.value] = points

        return result

    def get_wip_count(self, project_id: str = None) -> Dict:
        """
        Retorna Work in Progress (stories em andamento)
        """
        wip_statuses = [StoryStatus.IN_PROGRESS.value, StoryStatus.REVIEW.value, StoryStatus.TESTING.value]

        query = self.db.query(Story).filter(Story.status.in_(wip_statuses))
        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()

        by_status = defaultdict(int)
        for story in stories:
            by_status[story.status] += 1

        return {
            "total_wip": len(stories),
            "by_status": dict(by_status),
            "wip_story_points": sum(s.story_points or 0 for s in stories)
        }

    # =========================================================================
    # COMPLETION RATES
    # =========================================================================

    def get_story_completion_rate(self, project_id: str = None, days: int = 30) -> Dict:
        """
        Calcula taxa de conclusao de stories
        """
        start_date = datetime.utcnow() - timedelta(days=days)

        query = self.db.query(Story).filter(Story.created_at >= start_date)
        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.all()
        total = len(stories)
        completed = len([s for s in stories if s.status == StoryStatus.DONE.value])

        return {
            "total": total,
            "completed": completed,
            "completion_rate": round(completed / total * 100, 1) if total > 0 else 0,
            "period_days": days
        }

    # =========================================================================
    # AGENT PRODUCTIVITY
    # =========================================================================

    def get_agent_productivity(self, days: int = 30) -> List[Dict]:
        """
        Calcula produtividade dos agentes/workers

        Returns:
            List de dicts com worker_id, tasks_completed, avg_duration, etc.
        """
        workers = self.db.query(Worker).all()

        result = []
        for worker in workers:
            success_rate = 0
            if worker.jobs_completed + worker.jobs_failed > 0:
                success_rate = worker.jobs_completed / (worker.jobs_completed + worker.jobs_failed) * 100

            result.append({
                "worker_id": worker.worker_id,
                "status": worker.status,
                "jobs_completed": worker.jobs_completed,
                "jobs_failed": worker.jobs_failed,
                "success_rate": round(success_rate, 1),
                "avg_job_duration": round(worker.avg_job_duration, 1) if worker.avg_job_duration else 0,
                "total_processing_time": worker.total_processing_time or 0,
                "model": worker.model,
                "last_heartbeat": worker.last_heartbeat.isoformat() if worker.last_heartbeat else None
            })

        return sorted(result, key=lambda x: x["jobs_completed"], reverse=True)

    def get_task_completion_by_agent(self, project_id: str = None, days: int = 30) -> List[Dict]:
        """
        Calcula tasks completadas por agente
        """
        start_date = datetime.utcnow() - timedelta(days=days)

        query = self.db.query(StoryTask).filter(
            StoryTask.status == StoryTaskStatus.COMPLETED.value,
            StoryTask.completed_at >= start_date
        )

        tasks = query.all()

        by_agent = defaultdict(lambda: {"count": 0, "total_hours": 0})

        for task in tasks:
            agent = task.agent_id or task.assignee or "unassigned"
            by_agent[agent]["count"] += 1
            by_agent[agent]["total_hours"] += task.actual_hours or 0

        return [
            {"agent": k, "tasks_completed": v["count"], "total_hours": round(v["total_hours"], 1)}
            for k, v in sorted(by_agent.items(), key=lambda x: x[1]["count"], reverse=True)
        ]

    # =========================================================================
    # BURNDOWN DATA
    # =========================================================================

    def get_burndown_data(self, sprint_id: str) -> Dict:
        """
        Gera dados para grafico de burndown

        Returns:
            Dict com ideal_line, actual_line, forecast
        """
        sprint = self.db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()
        if not sprint or not sprint.start_date or not sprint.end_date:
            return {"error": "Sprint not found or missing dates"}

        # Buscar stories do sprint
        stories = self.db.query(Story).filter(Story.sprint_id == sprint_id).all()
        total_points = sum(s.story_points or 0 for s in stories)

        # Calcular linha ideal
        sprint_days = (sprint.end_date - sprint.start_date).days
        if sprint_days <= 0:
            sprint_days = 14  # Default 2 weeks

        ideal_line = []
        daily_burn = total_points / sprint_days
        for day in range(sprint_days + 1):
            ideal_line.append({
                "day": day,
                "date": (sprint.start_date + timedelta(days=day)).strftime("%Y-%m-%d"),
                "remaining": round(total_points - (daily_burn * day), 1)
            })

        # Calcular linha real
        actual_line = []
        completed_stories = [s for s in stories if s.status == StoryStatus.DONE.value]

        # Agrupar por dia de conclusao
        completed_by_day = defaultdict(int)
        for story in completed_stories:
            if story.completed_at:
                day_num = (story.completed_at.date() - sprint.start_date.date()).days
                if 0 <= day_num <= sprint_days:
                    completed_by_day[day_num] += story.story_points or 0

        remaining = total_points
        for day in range(sprint_days + 1):
            if sprint.start_date + timedelta(days=day) <= datetime.utcnow():
                remaining -= completed_by_day.get(day, 0)
                actual_line.append({
                    "day": day,
                    "date": (sprint.start_date + timedelta(days=day)).strftime("%Y-%m-%d"),
                    "remaining": remaining
                })

        # Calcular forecast (se continuar no ritmo atual)
        forecast = []
        if len(actual_line) >= 2:
            points_completed = total_points - actual_line[-1]["remaining"]
            days_elapsed = len(actual_line) - 1
            if days_elapsed > 0:
                daily_rate = points_completed / days_elapsed
                remaining_points = actual_line[-1]["remaining"]
                for day in range(len(actual_line) - 1, sprint_days + 1):
                    forecast.append({
                        "day": day,
                        "date": (sprint.start_date + timedelta(days=day)).strftime("%Y-%m-%d"),
                        "remaining": max(0, round(remaining_points - (daily_rate * (day - len(actual_line) + 1)), 1))
                    })

        return {
            "sprint_id": sprint_id,
            "sprint_name": sprint.name,
            "total_points": total_points,
            "start_date": sprint.start_date.strftime("%Y-%m-%d"),
            "end_date": sprint.end_date.strftime("%Y-%m-%d"),
            "ideal_line": ideal_line,
            "actual_line": actual_line,
            "forecast": forecast
        }

    # =========================================================================
    # EXECUTIVE SUMMARY
    # =========================================================================

    def get_executive_summary(self, project_id: str = None, days: int = 30) -> Dict:
        """
        Retorna resumo executivo com principais KPIs

        Returns:
            Dict com todos os KPIs principais para o dashboard executivo
        """
        velocity = self.get_average_velocity(project_id)
        throughput = self.get_weekly_throughput_average(project_id)
        cycle_time = self.get_cycle_time(project_id, days)
        lead_time = self.get_lead_time(project_id, days)
        status_dist = self.get_story_status_distribution(project_id)
        wip = self.get_wip_count(project_id)
        completion = self.get_story_completion_rate(project_id, days)
        agents = self.get_agent_productivity(days)

        # Alertas automaticos
        alerts = []

        # Alerta de WIP alto
        if wip["total_wip"] > 10:
            alerts.append({
                "type": "warning",
                "message": f"WIP alto: {wip['total_wip']} stories em andamento",
                "metric": "wip"
            })

        # Alerta de cycle time alto
        if cycle_time["average_cycle_time_days"] > 5:
            alerts.append({
                "type": "warning",
                "message": f"Cycle time alto: {cycle_time['average_cycle_time_days']} dias em media",
                "metric": "cycle_time"
            })

        # Alerta de velocity em queda
        if velocity["trend"] == "decreasing":
            alerts.append({
                "type": "warning",
                "message": "Velocity em tendencia de queda",
                "metric": "velocity"
            })

        # Alerta de taxa de conclusao baixa
        if completion["completion_rate"] < 50:
            alerts.append({
                "type": "warning",
                "message": f"Taxa de conclusao baixa: {completion['completion_rate']}%",
                "metric": "completion_rate"
            })

        return {
            "generated_at": datetime.utcnow().isoformat(),
            "period_days": days,
            "project_id": project_id,
            "kpis": {
                "velocity": {
                    "value": velocity["average"],
                    "unit": "pts/sprint",
                    "trend": velocity["trend"]
                },
                "throughput": {
                    "value": throughput["average_count"],
                    "unit": "stories/week",
                    "points_per_week": throughput["average_points"]
                },
                "cycle_time": {
                    "value": cycle_time["average_cycle_time_days"],
                    "unit": "days"
                },
                "lead_time": {
                    "value": lead_time["average_lead_time_days"],
                    "unit": "days"
                },
                "wip": {
                    "value": wip["total_wip"],
                    "story_points": wip["wip_story_points"]
                },
                "completion_rate": {
                    "value": completion["completion_rate"],
                    "unit": "%"
                }
            },
            "status_distribution": status_dist,
            "agents_productivity": agents[:5],  # Top 5 agents
            "alerts": alerts
        }

    # =========================================================================
    # EXPORT FUNCTIONS
    # =========================================================================

    def export_to_dict(self, project_id: str = None, days: int = 30) -> Dict:
        """
        Exporta todos os dados de analytics para um dicionario
        """
        return {
            "summary": self.get_executive_summary(project_id, days),
            "velocity_by_sprint": self.get_velocity_by_sprint(project_id),
            "throughput": self.get_throughput(project_id, days),
            "cycle_time": self.get_cycle_time(project_id, days),
            "lead_time": self.get_lead_time(project_id, days),
            "agent_productivity": self.get_agent_productivity(days),
            "task_completion_by_agent": self.get_task_completion_by_agent(project_id, days)
        }

    def export_to_json(self, project_id: str = None, days: int = 30) -> str:
        """
        Exporta todos os dados de analytics para JSON
        """
        return json.dumps(self.export_to_dict(project_id, days), indent=2, default=str)


# =============================================================================
# Singleton instance
# =============================================================================

_analytics_instance: Optional[AnalyticsService] = None

def get_analytics_service() -> AnalyticsService:
    """Retorna instancia singleton do AnalyticsService"""
    global _analytics_instance
    if _analytics_instance is None:
        _analytics_instance = AnalyticsService()
    return _analytics_instance


# =============================================================================
# CLI / Testing
# =============================================================================

if __name__ == "__main__":
    print("=" * 60)
    print("  Analytics Service - Plataforma E")
    print("=" * 60)

    service = AnalyticsService()

    print("\n[Executive Summary]")
    summary = service.get_executive_summary()
    print(json.dumps(summary, indent=2, default=str))

    print("\n[Velocity by Sprint]")
    velocity = service.get_velocity_by_sprint()
    print(json.dumps(velocity, indent=2, default=str))

    print("\n[Agent Productivity]")
    agents = service.get_agent_productivity()
    print(json.dumps(agents, indent=2, default=str))

    service.close()
