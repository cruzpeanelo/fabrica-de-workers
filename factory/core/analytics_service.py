# -*- coding: utf-8 -*-
"""
Analytics Service Module
========================
Servico de analytics para metricas de projetos e stories.

Funcionalidades:
- Calcular metricas de velocity, throughput, cycle time
- Gerar dados para burndown charts
- Calcular distribuicoes por status, prioridade, etc.
- Suporte a OData para BI tools
- Isolamento por tenant

Este servico alimenta os dashboards Power BI, Tableau e outros BI tools.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Tuple
from enum import Enum

logger = logging.getLogger(__name__)


class MetricPeriod(str, Enum):
    """Periodos para agregacao de metricas"""
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    QUARTER = "quarter"
    SPRINT = "sprint"


class MetricType(str, Enum):
    """Tipos de metricas disponiveis"""
    VELOCITY = "velocity"
    THROUGHPUT = "throughput"
    CYCLE_TIME = "cycle_time"
    LEAD_TIME = "lead_time"
    WIP = "wip"
    BURNDOWN = "burndown"
    COMPLETION_RATE = "completion_rate"
    STORY_DISTRIBUTION = "story_distribution"
    PRIORITY_DISTRIBUTION = "priority_distribution"
    CATEGORY_DISTRIBUTION = "category_distribution"


@dataclass
class MetricResult:
    """Resultado de uma metrica calculada"""
    metric_type: MetricType
    value: Any
    period_start: datetime
    period_end: datetime
    project_id: Optional[str] = None
    sprint_id: Optional[str] = None
    details: Dict[str, Any] = field(default_factory=dict)
    trend: Optional[str] = None  # up, down, stable
    change_percentage: float = 0.0


@dataclass
class BurndownPoint:
    """Ponto de dados do burndown chart"""
    date: datetime
    remaining_points: int
    remaining_stories: int
    ideal_points: float
    completed_points: int


class AnalyticsService:
    """
    Servico de analytics para projetos e stories.

    Fornece metricas agregadas para dashboards e BI tools.

    Exemplo de uso:
    ```python
    service = AnalyticsService(tenant_id="...")

    # Calcular velocity
    velocity = service.calculate_velocity(project_id="PROJ-001")

    # Gerar burndown
    burndown = service.generate_burndown(sprint_id="SPR-001")

    # Obter todas as metricas
    metrics = service.get_project_metrics(project_id="PROJ-001")
    ```
    """

    def __init__(self, tenant_id: str):
        """
        Inicializa o servico de analytics.

        Args:
            tenant_id: ID do tenant para isolamento
        """
        self.tenant_id = tenant_id
        self._cache: Dict[str, Tuple[datetime, Any]] = {}
        self._cache_ttl = timedelta(minutes=5)

    def _get_cached(self, key: str) -> Optional[Any]:
        """Retorna valor do cache se valido"""
        if key in self._cache:
            cached_at, value = self._cache[key]
            if datetime.utcnow() - cached_at < self._cache_ttl:
                return value
        return None

    def _set_cached(self, key: str, value: Any):
        """Armazena valor no cache"""
        self._cache[key] = (datetime.utcnow(), value)

    def _log_operation(self, operation: str, details: Dict[str, Any] = None):
        """Registra operacao para auditoria"""
        logger.info(f"[Analytics] {operation} - tenant: {self.tenant_id}, details: {details}")

    def get_stories(
        self,
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        status: Optional[str] = None,
        include_tasks: bool = False
    ) -> List[Dict]:
        """Busca stories do banco com filtros"""
        try:
            from factory.database.connection import get_session
            from factory.database.models import Story, StoryTask

            with get_session() as session:
                query = session.query(Story)

                if project_id:
                    query = query.filter(Story.project_id == project_id)
                if sprint_id:
                    query = query.filter(Story.sprint_id == sprint_id)
                if status:
                    query = query.filter(Story.status == status)

                stories = query.all()
                result = []

                for story in stories:
                    story_dict = story.to_dict()
                    if include_tasks:
                        tasks = session.query(StoryTask).filter(
                            StoryTask.story_id == story.story_id
                        ).all()
                        story_dict["tasks"] = [t.to_dict() for t in tasks]
                    result.append(story_dict)

                return result

        except Exception as e:
            logger.error(f"[Analytics] Erro ao buscar stories: {e}")
            return []

    def get_sprints(self, project_id: str) -> List[Dict]:
        """Busca sprints do projeto"""
        try:
            from factory.database.connection import get_session
            from factory.database.models import Sprint

            with get_session() as session:
                sprints = session.query(Sprint).filter(
                    Sprint.project_id == project_id
                ).order_by(Sprint.start_date.desc()).all()

                return [s.to_dict() for s in sprints]

        except Exception as e:
            logger.error(f"[Analytics] Erro ao buscar sprints: {e}")
            return []

    def calculate_velocity(
        self,
        project_id: str,
        num_sprints: int = 5
    ) -> MetricResult:
        """
        Calcula velocity (story points entregues por sprint).

        Args:
            project_id: ID do projeto
            num_sprints: Numero de sprints para considerar

        Returns:
            MetricResult com velocity
        """
        cache_key = f"velocity:{project_id}:{num_sprints}"
        cached = self._get_cached(cache_key)
        if cached:
            return cached

        self._log_operation("calculate_velocity", {"project_id": project_id})

        sprints = self.get_sprints(project_id)
        velocity_history = []

        for sprint in sprints[:num_sprints]:
            stories = self.get_stories(
                project_id=project_id,
                sprint_id=sprint.get("sprint_id"),
                status="done"
            )
            points = sum(s.get("story_points", 0) for s in stories)
            velocity_history.append({
                "sprint_id": sprint.get("sprint_id"),
                "sprint_name": sprint.get("name"),
                "points": points,
                "stories_count": len(stories),
                "start_date": sprint.get("start_date"),
                "end_date": sprint.get("end_date")
            })

        # Calcular metricas
        if velocity_history:
            points_list = [v["points"] for v in velocity_history]
            current = points_list[0] if points_list else 0
            previous = points_list[1] if len(points_list) > 1 else current
            average = sum(points_list) / len(points_list)

            trend = "stable"
            change = 0.0
            if previous > 0:
                change = ((current - previous) / previous) * 100
                if change > 5:
                    trend = "up"
                elif change < -5:
                    trend = "down"
        else:
            current = 0
            average = 0
            trend = "stable"
            change = 0.0

        result = MetricResult(
            metric_type=MetricType.VELOCITY,
            value=current,
            period_start=datetime.utcnow() - timedelta(days=14),
            period_end=datetime.utcnow(),
            project_id=project_id,
            trend=trend,
            change_percentage=round(change, 2),
            details={
                "current_sprint": current,
                "average": round(average, 2),
                "history": velocity_history
            }
        )

        self._set_cached(cache_key, result)
        return result

    def calculate_throughput(
        self,
        project_id: str,
        period: MetricPeriod = MetricPeriod.WEEK
    ) -> MetricResult:
        """
        Calcula throughput (stories entregues por periodo).

        Args:
            project_id: ID do projeto
            period: Periodo de agregacao

        Returns:
            MetricResult com throughput
        """
        self._log_operation("calculate_throughput", {"project_id": project_id, "period": period.value})

        stories = self.get_stories(project_id=project_id, status="done")

        # Calcular periodo
        now = datetime.utcnow()
        if period == MetricPeriod.DAY:
            period_start = now - timedelta(days=1)
        elif period == MetricPeriod.WEEK:
            period_start = now - timedelta(weeks=1)
        elif period == MetricPeriod.MONTH:
            period_start = now - timedelta(days=30)
        else:
            period_start = now - timedelta(weeks=2)  # Sprint padrao

        # Filtrar por periodo
        stories_in_period = [
            s for s in stories
            if s.get("completed_at") and
            datetime.fromisoformat(s["completed_at"].replace("Z", "")) >= period_start
        ]

        return MetricResult(
            metric_type=MetricType.THROUGHPUT,
            value=len(stories_in_period),
            period_start=period_start,
            period_end=now,
            project_id=project_id,
            details={
                "stories": [s.get("story_id") for s in stories_in_period],
                "total_points": sum(s.get("story_points", 0) for s in stories_in_period),
                "period": period.value
            }
        )

    def calculate_cycle_time(
        self,
        project_id: str,
        num_stories: int = 20
    ) -> MetricResult:
        """
        Calcula cycle time medio (tempo de in_progress ate done).

        Args:
            project_id: ID do projeto
            num_stories: Numero de stories recentes para considerar

        Returns:
            MetricResult com cycle time em dias
        """
        self._log_operation("calculate_cycle_time", {"project_id": project_id})

        stories = self.get_stories(project_id=project_id, status="done")
        cycle_times = []

        for story in stories[:num_stories]:
            started = story.get("started_at")
            completed = story.get("completed_at")

            if started and completed:
                try:
                    start_dt = datetime.fromisoformat(started.replace("Z", ""))
                    end_dt = datetime.fromisoformat(completed.replace("Z", ""))
                    days = (end_dt - start_dt).days
                    if days >= 0:
                        cycle_times.append({
                            "story_id": story.get("story_id"),
                            "days": days,
                            "points": story.get("story_points", 0)
                        })
                except:
                    pass

        avg_cycle_time = sum(c["days"] for c in cycle_times) / len(cycle_times) if cycle_times else 0

        return MetricResult(
            metric_type=MetricType.CYCLE_TIME,
            value=round(avg_cycle_time, 2),
            period_start=datetime.utcnow() - timedelta(days=30),
            period_end=datetime.utcnow(),
            project_id=project_id,
            details={
                "unit": "days",
                "sample_size": len(cycle_times),
                "min": min(c["days"] for c in cycle_times) if cycle_times else 0,
                "max": max(c["days"] for c in cycle_times) if cycle_times else 0,
                "distribution": cycle_times[:10]
            }
        )

    def calculate_lead_time(
        self,
        project_id: str,
        num_stories: int = 20
    ) -> MetricResult:
        """
        Calcula lead time medio (tempo de criacao ate done).

        Args:
            project_id: ID do projeto
            num_stories: Numero de stories recentes

        Returns:
            MetricResult com lead time em dias
        """
        self._log_operation("calculate_lead_time", {"project_id": project_id})

        stories = self.get_stories(project_id=project_id, status="done")
        lead_times = []

        for story in stories[:num_stories]:
            created = story.get("created_at")
            completed = story.get("completed_at")

            if created and completed:
                try:
                    start_dt = datetime.fromisoformat(created.replace("Z", ""))
                    end_dt = datetime.fromisoformat(completed.replace("Z", ""))
                    days = (end_dt - start_dt).days
                    if days >= 0:
                        lead_times.append({
                            "story_id": story.get("story_id"),
                            "days": days,
                            "points": story.get("story_points", 0)
                        })
                except:
                    pass

        avg_lead_time = sum(l["days"] for l in lead_times) / len(lead_times) if lead_times else 0

        return MetricResult(
            metric_type=MetricType.LEAD_TIME,
            value=round(avg_lead_time, 2),
            period_start=datetime.utcnow() - timedelta(days=30),
            period_end=datetime.utcnow(),
            project_id=project_id,
            details={
                "unit": "days",
                "sample_size": len(lead_times),
                "min": min(l["days"] for l in lead_times) if lead_times else 0,
                "max": max(l["days"] for l in lead_times) if lead_times else 0
            }
        )

    def calculate_wip(self, project_id: str) -> MetricResult:
        """
        Calcula Work In Progress (stories em andamento).

        Args:
            project_id: ID do projeto

        Returns:
            MetricResult com WIP atual
        """
        self._log_operation("calculate_wip", {"project_id": project_id})

        stories = self.get_stories(project_id=project_id)

        wip_statuses = ["in_progress", "review", "testing"]
        wip_stories = [s for s in stories if s.get("status") in wip_statuses]

        by_status = {}
        for story in wip_stories:
            status = story.get("status", "unknown")
            by_status[status] = by_status.get(status, 0) + 1

        return MetricResult(
            metric_type=MetricType.WIP,
            value=len(wip_stories),
            period_start=datetime.utcnow(),
            period_end=datetime.utcnow(),
            project_id=project_id,
            details={
                "by_status": by_status,
                "total_points": sum(s.get("story_points", 0) for s in wip_stories),
                "stories": [{"id": s.get("story_id"), "status": s.get("status")} for s in wip_stories]
            }
        )

    def generate_burndown(
        self,
        sprint_id: str,
        project_id: Optional[str] = None
    ) -> List[BurndownPoint]:
        """
        Gera dados para burndown chart do sprint.

        Args:
            sprint_id: ID do sprint
            project_id: ID do projeto (opcional)

        Returns:
            Lista de BurndownPoint
        """
        self._log_operation("generate_burndown", {"sprint_id": sprint_id})

        stories = self.get_stories(sprint_id=sprint_id)
        sprints = self.get_sprints(project_id) if project_id else []

        # Buscar dados do sprint
        sprint = next((s for s in sprints if s.get("sprint_id") == sprint_id), None)

        if sprint:
            try:
                start_date = datetime.fromisoformat(sprint.get("start_date", "").replace("Z", ""))
                end_date = datetime.fromisoformat(sprint.get("end_date", "").replace("Z", ""))
            except:
                start_date = datetime.utcnow() - timedelta(days=14)
                end_date = datetime.utcnow()
        else:
            start_date = datetime.utcnow() - timedelta(days=14)
            end_date = datetime.utcnow()

        total_points = sum(s.get("story_points", 0) for s in stories)
        total_stories = len(stories)
        sprint_days = (end_date - start_date).days or 1

        burndown = []
        current_date = start_date

        while current_date <= min(end_date, datetime.utcnow()):
            # Calcular stories/pontos completados ate esta data
            completed_points = 0
            completed_stories = 0

            for story in stories:
                completed_at = story.get("completed_at")
                if completed_at:
                    try:
                        completed_dt = datetime.fromisoformat(completed_at.replace("Z", ""))
                        if completed_dt.date() <= current_date.date():
                            completed_points += story.get("story_points", 0)
                            completed_stories += 1
                    except:
                        pass

            # Calcular linha ideal
            days_elapsed = (current_date - start_date).days
            ideal_remaining = total_points * (1 - days_elapsed / sprint_days)

            burndown.append(BurndownPoint(
                date=current_date,
                remaining_points=total_points - completed_points,
                remaining_stories=total_stories - completed_stories,
                ideal_points=max(0, ideal_remaining),
                completed_points=completed_points
            ))

            current_date += timedelta(days=1)

        return burndown

    def get_status_distribution(self, project_id: str) -> Dict[str, int]:
        """
        Calcula distribuicao de stories por status.

        Args:
            project_id: ID do projeto

        Returns:
            Dict com contagem por status
        """
        stories = self.get_stories(project_id=project_id)
        distribution = {}

        for story in stories:
            status = story.get("status", "backlog")
            distribution[status] = distribution.get(status, 0) + 1

        return distribution

    def get_priority_distribution(self, project_id: str) -> Dict[str, int]:
        """
        Calcula distribuicao de stories por prioridade.

        Args:
            project_id: ID do projeto

        Returns:
            Dict com contagem por prioridade
        """
        stories = self.get_stories(project_id=project_id)
        distribution = {}

        for story in stories:
            priority = story.get("priority", "medium")
            distribution[priority] = distribution.get(priority, 0) + 1

        return distribution

    def get_category_distribution(self, project_id: str) -> Dict[str, int]:
        """
        Calcula distribuicao de stories por categoria.

        Args:
            project_id: ID do projeto

        Returns:
            Dict com contagem por categoria
        """
        stories = self.get_stories(project_id=project_id)
        distribution = {}

        for story in stories:
            category = story.get("category", "feature")
            distribution[category] = distribution.get(category, 0) + 1

        return distribution

    def get_project_metrics(self, project_id: str) -> Dict[str, Any]:
        """
        Retorna todas as metricas do projeto.

        Args:
            project_id: ID do projeto

        Returns:
            Dict com todas as metricas agregadas
        """
        cache_key = f"project_metrics:{project_id}"
        cached = self._get_cached(cache_key)
        if cached:
            return cached

        self._log_operation("get_project_metrics", {"project_id": project_id})

        stories = self.get_stories(project_id=project_id)
        total = len(stories)
        done = len([s for s in stories if s.get("status") == "done"])
        total_points = sum(s.get("story_points", 0) for s in stories)
        done_points = sum(s.get("story_points", 0) for s in stories if s.get("status") == "done")

        velocity = self.calculate_velocity(project_id)
        cycle_time = self.calculate_cycle_time(project_id)
        wip = self.calculate_wip(project_id)

        metrics = {
            "project_id": project_id,
            "tenant_id": self.tenant_id,
            "generated_at": datetime.utcnow().isoformat(),
            "summary": {
                "total_stories": total,
                "completed_stories": done,
                "completion_rate": round((done / total * 100) if total > 0 else 0, 2),
                "total_points": total_points,
                "completed_points": done_points,
                "points_completion_rate": round((done_points / total_points * 100) if total_points > 0 else 0, 2)
            },
            "velocity": {
                "current": velocity.value,
                "average": velocity.details.get("average", 0),
                "trend": velocity.trend,
                "history": velocity.details.get("history", [])
            },
            "cycle_time": {
                "average_days": cycle_time.value,
                "min_days": cycle_time.details.get("min", 0),
                "max_days": cycle_time.details.get("max", 0)
            },
            "wip": {
                "count": wip.value,
                "by_status": wip.details.get("by_status", {}),
                "total_points": wip.details.get("total_points", 0)
            },
            "distributions": {
                "by_status": self.get_status_distribution(project_id),
                "by_priority": self.get_priority_distribution(project_id),
                "by_category": self.get_category_distribution(project_id)
            }
        }

        self._set_cached(cache_key, metrics)
        return metrics

    def get_odata_stories(
        self,
        project_id: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        filter_expr: Optional[str] = None,
        orderby: Optional[str] = None,
        select: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Retorna stories em formato OData.

        Args:
            project_id: Filtrar por projeto
            top: Limite de resultados
            skip: Pular N resultados
            filter_expr: Expressao de filtro OData
            orderby: Campo para ordenacao
            select: Campos a retornar

        Returns:
            Dict em formato OData
        """
        stories = self.get_stories(project_id=project_id)

        # Aplicar paginacao
        paginated = stories[skip:skip + top]

        # Formatar para OData
        odata_stories = []
        for s in paginated:
            story_data = {
                "StoryId": s.get("story_id"),
                "ProjectId": s.get("project_id"),
                "Title": s.get("title"),
                "Description": s.get("description"),
                "Status": s.get("status"),
                "Priority": s.get("priority"),
                "StoryPoints": s.get("story_points"),
                "Complexity": s.get("complexity"),
                "Category": s.get("category"),
                "EpicId": s.get("epic_id"),
                "SprintId": s.get("sprint_id"),
                "Assignee": s.get("assignee"),
                "Progress": s.get("progress"),
                "TasksTotal": s.get("tasks_total"),
                "TasksCompleted": s.get("tasks_completed"),
                "CreatedAt": s.get("created_at"),
                "UpdatedAt": s.get("updated_at"),
                "CompletedAt": s.get("completed_at")
            }

            # Aplicar select se especificado
            if select:
                story_data = {k: v for k, v in story_data.items() if k in select}

            odata_stories.append(story_data)

        return {
            "@odata.context": "$metadata#Stories",
            "@odata.count": len(stories),
            "value": odata_stories
        }


# Cache de servicos por tenant
_services: Dict[str, AnalyticsService] = {}


def get_analytics_service(tenant_id: str) -> AnalyticsService:
    """
    Retorna servico de analytics para o tenant.

    Args:
        tenant_id: ID do tenant

    Returns:
        AnalyticsService isolado por tenant
    """
    if tenant_id not in _services:
        _services[tenant_id] = AnalyticsService(tenant_id)
    return _services[tenant_id]
