# -*- coding: utf-8 -*-
"""
Analytics Export API
====================
Endpoints otimizados para ferramentas de BI (Power BI, Tableau, Excel).

Funcionalidades:
- Endpoints REST para exportacao de dados
- Formatos: JSON, CSV, OData
- Agregacoes pre-calculadas
- Paginacao otimizada para grandes volumes
- Cache de consultas frequentes
- Isolamento por tenant

Issue #115 - Endpoints de Analytics para Power BI
"""

import csv
import io
import json
import logging
from datetime import datetime, timedelta
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, List, Optional, Union
from dataclasses import dataclass, field, asdict

try:
    from fastapi import APIRouter, Depends, HTTPException, Query, Request, Response
    from fastapi.responses import StreamingResponse, JSONResponse
    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False

logger = logging.getLogger(__name__)


class ExportFormat(str, Enum):
    """Formatos de exportacao suportados"""
    JSON = "json"
    CSV = "csv"
    ODATA = "odata"
    EXCEL = "excel"


class AggregationType(str, Enum):
    """Tipos de agregacao"""
    COUNT = "count"
    SUM = "sum"
    AVG = "avg"
    MIN = "min"
    MAX = "max"
    DISTINCT = "distinct"


class TimeGranularity(str, Enum):
    """Granularidade temporal"""
    HOUR = "hour"
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    QUARTER = "quarter"
    YEAR = "year"


@dataclass
class AnalyticsMetric:
    """Metrica de analytics"""
    name: str
    value: Union[int, float]
    change: Optional[float] = None
    change_period: Optional[str] = None
    unit: Optional[str] = None


@dataclass
class AnalyticsDimension:
    """Dimensao de analytics"""
    name: str
    value: str
    count: int = 0


@dataclass
class TimeSeriesPoint:
    """Ponto de serie temporal"""
    timestamp: datetime
    value: Union[int, float]
    label: Optional[str] = None


@dataclass
class ODataResponse:
    """Resposta no formato OData"""
    context: str
    value: List[Dict]
    count: Optional[int] = None
    next_link: Optional[str] = None

    def to_dict(self) -> Dict:
        result = {
            "@odata.context": self.context,
            "value": self.value
        }
        if self.count is not None:
            result["@odata.count"] = self.count
        if self.next_link:
            result["@odata.nextLink"] = self.next_link
        return result


class AnalyticsCache:
    """Cache simples para consultas de analytics"""

    def __init__(self, ttl_seconds: int = 300):
        self.ttl = ttl_seconds
        self._cache: Dict[str, Dict] = {}

    def _get_key(self, tenant_id: str, query_hash: str) -> str:
        return f"{tenant_id}:{query_hash}"

    def get(self, tenant_id: str, query_hash: str) -> Optional[Any]:
        key = self._get_key(tenant_id, query_hash)
        if key in self._cache:
            entry = self._cache[key]
            if datetime.utcnow() < entry["expires_at"]:
                return entry["data"]
            else:
                del self._cache[key]
        return None

    def set(self, tenant_id: str, query_hash: str, data: Any):
        key = self._get_key(tenant_id, query_hash)
        self._cache[key] = {
            "data": data,
            "expires_at": datetime.utcnow() + timedelta(seconds=self.ttl)
        }

    def invalidate(self, tenant_id: str):
        """Invalida cache de um tenant"""
        keys_to_delete = [k for k in self._cache if k.startswith(f"{tenant_id}:")]
        for key in keys_to_delete:
            del self._cache[key]


# Cache global
_analytics_cache = AnalyticsCache()


class AnalyticsQueryBuilder:
    """
    Builder para consultas de analytics.

    Exemplo:
    ```python
    query = (AnalyticsQueryBuilder()
        .select("project_id", "status", "story_points")
        .where("status", "!=", "deleted")
        .where("created_at", ">=", "2024-01-01")
        .group_by("status")
        .aggregate("story_points", AggregationType.SUM, "total_points")
        .aggregate("id", AggregationType.COUNT, "story_count")
        .order_by("total_points", desc=True)
        .limit(100)
    )
    ```
    """

    def __init__(self):
        self._select: List[str] = []
        self._where: List[Dict] = []
        self._group_by: List[str] = []
        self._aggregates: List[Dict] = []
        self._order_by: List[Dict] = []
        self._limit: Optional[int] = None
        self._offset: int = 0

    def select(self, *fields: str) -> "AnalyticsQueryBuilder":
        self._select.extend(fields)
        return self

    def where(self, field: str, operator: str, value: Any) -> "AnalyticsQueryBuilder":
        self._where.append({"field": field, "operator": operator, "value": value})
        return self

    def group_by(self, *fields: str) -> "AnalyticsQueryBuilder":
        self._group_by.extend(fields)
        return self

    def aggregate(
        self,
        field: str,
        aggregation: AggregationType,
        alias: Optional[str] = None
    ) -> "AnalyticsQueryBuilder":
        self._aggregates.append({
            "field": field,
            "type": aggregation.value,
            "alias": alias or f"{aggregation.value}_{field}"
        })
        return self

    def order_by(self, field: str, desc: bool = False) -> "AnalyticsQueryBuilder":
        self._order_by.append({"field": field, "desc": desc})
        return self

    def limit(self, count: int) -> "AnalyticsQueryBuilder":
        self._limit = count
        return self

    def offset(self, count: int) -> "AnalyticsQueryBuilder":
        self._offset = count
        return self

    def to_dict(self) -> Dict:
        return {
            "select": self._select,
            "where": self._where,
            "group_by": self._group_by,
            "aggregates": self._aggregates,
            "order_by": self._order_by,
            "limit": self._limit,
            "offset": self._offset
        }

    def hash(self) -> str:
        """Gera hash unico da query para cache"""
        import hashlib
        query_str = json.dumps(self.to_dict(), sort_keys=True)
        return hashlib.md5(query_str.encode()).hexdigest()


class AnalyticsDataProvider:
    """
    Provider de dados para analytics.

    Abstrai a fonte de dados para facilitar testes e diferentes backends.
    """

    def __init__(self, tenant_id: str):
        self.tenant_id = tenant_id

    async def get_stories_summary(
        self,
        project_id: Optional[str] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None
    ) -> Dict[str, Any]:
        """Retorna resumo de stories"""
        # Implementacao de exemplo - substituir por acesso real ao banco
        return {
            "total_stories": 150,
            "by_status": {
                "backlog": 45,
                "ready": 20,
                "in_progress": 25,
                "review": 15,
                "testing": 10,
                "done": 35
            },
            "total_story_points": 450,
            "completed_story_points": 180,
            "velocity_avg": 25.5,
            "cycle_time_avg_days": 4.2
        }

    async def get_stories_by_status(
        self,
        project_id: Optional[str] = None
    ) -> List[Dict]:
        """Retorna contagem de stories por status"""
        return [
            {"status": "backlog", "count": 45, "story_points": 120},
            {"status": "ready", "count": 20, "story_points": 60},
            {"status": "in_progress", "count": 25, "story_points": 80},
            {"status": "review", "count": 15, "story_points": 45},
            {"status": "testing", "count": 10, "story_points": 35},
            {"status": "done", "count": 35, "story_points": 110}
        ]

    async def get_stories_timeline(
        self,
        project_id: Optional[str] = None,
        granularity: TimeGranularity = TimeGranularity.DAY,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None
    ) -> List[Dict]:
        """Retorna timeline de stories"""
        # Gerar dados de exemplo
        now = datetime.utcnow()
        data = []

        for i in range(30):
            date = now - timedelta(days=29-i)
            data.append({
                "date": date.strftime("%Y-%m-%d"),
                "created": 3 + (i % 5),
                "completed": 2 + (i % 4),
                "in_progress": 5 + (i % 3)
            })

        return data

    async def get_sprint_metrics(
        self,
        sprint_id: Optional[str] = None
    ) -> List[Dict]:
        """Retorna metricas de sprints"""
        return [
            {
                "sprint_id": "SPR-001",
                "name": "Sprint 1",
                "planned_points": 40,
                "completed_points": 38,
                "velocity": 38,
                "completion_rate": 95.0,
                "stories_planned": 8,
                "stories_completed": 7,
                "start_date": "2024-01-01",
                "end_date": "2024-01-14"
            },
            {
                "sprint_id": "SPR-002",
                "name": "Sprint 2",
                "planned_points": 45,
                "completed_points": 42,
                "velocity": 42,
                "completion_rate": 93.3,
                "stories_planned": 9,
                "stories_completed": 8,
                "start_date": "2024-01-15",
                "end_date": "2024-01-28"
            }
        ]

    async def get_team_performance(
        self,
        project_id: Optional[str] = None,
        date_from: Optional[datetime] = None,
        date_to: Optional[datetime] = None
    ) -> List[Dict]:
        """Retorna performance por membro do time"""
        return [
            {
                "user": "joao.silva",
                "stories_completed": 15,
                "story_points": 45,
                "avg_cycle_time_days": 3.5,
                "bugs_fixed": 8,
                "pr_reviewed": 22
            },
            {
                "user": "maria.santos",
                "stories_completed": 18,
                "story_points": 52,
                "avg_cycle_time_days": 3.2,
                "bugs_fixed": 5,
                "pr_reviewed": 28
            }
        ]

    async def get_project_health(
        self,
        project_id: str
    ) -> Dict:
        """Retorna saude do projeto"""
        return {
            "project_id": project_id,
            "health_score": 85,
            "metrics": {
                "velocity_trend": "stable",
                "burndown_status": "on_track",
                "quality_score": 92,
                "team_satisfaction": 4.2,
                "code_coverage": 78.5,
                "tech_debt_ratio": 12.3
            },
            "risks": [
                {"type": "scope_creep", "level": "low", "description": "Aumento de escopo controlado"},
                {"type": "resource", "level": "medium", "description": "1 desenvolvedor de ferias"}
            ],
            "recommendations": [
                "Aumentar cobertura de testes para 80%",
                "Planejar reducao de divida tecnica no proximo sprint"
            ]
        }

    async def get_stories_list(
        self,
        project_id: Optional[str] = None,
        status: Optional[str] = None,
        limit: int = 100,
        offset: int = 0
    ) -> tuple[List[Dict], int]:
        """Retorna lista de stories paginada"""
        # Dados de exemplo
        stories = [
            {
                "story_id": f"STR-{i:04d}",
                "title": f"Story {i}",
                "status": ["backlog", "ready", "in_progress", "done"][i % 4],
                "priority": ["low", "medium", "high", "urgent"][i % 4],
                "story_points": [1, 2, 3, 5, 8][i % 5],
                "assignee": f"user{i % 5}",
                "sprint_id": f"SPR-{(i // 10) + 1:03d}",
                "created_at": (datetime.utcnow() - timedelta(days=30-i)).isoformat(),
                "updated_at": (datetime.utcnow() - timedelta(days=15-i)).isoformat()
            }
            for i in range(1, 101)
        ]

        if status:
            stories = [s for s in stories if s["status"] == status]

        total = len(stories)
        stories = stories[offset:offset + limit]

        return stories, total


def format_csv_response(data: List[Dict], filename: str = "export.csv") -> StreamingResponse:
    """Formata resposta CSV"""
    if not data:
        return StreamingResponse(
            iter(["No data"]),
            media_type="text/csv",
            headers={"Content-Disposition": f"attachment; filename={filename}"}
        )

    output = io.StringIO()
    writer = csv.DictWriter(output, fieldnames=data[0].keys())
    writer.writeheader()
    writer.writerows(data)

    output.seek(0)

    return StreamingResponse(
        iter([output.getvalue()]),
        media_type="text/csv",
        headers={"Content-Disposition": f"attachment; filename={filename}"}
    )


def format_odata_response(
    data: List[Dict],
    context: str,
    count: Optional[int] = None,
    next_link: Optional[str] = None
) -> Dict:
    """Formata resposta OData"""
    response = ODataResponse(
        context=context,
        value=data,
        count=count,
        next_link=next_link
    )
    return response.to_dict()


if FASTAPI_AVAILABLE:
    router = APIRouter(prefix="/api/analytics", tags=["Analytics"])

    async def get_tenant_id(request: Request) -> str:
        """Extrai tenant_id do request"""
        # Tentar obter de diferentes fontes
        tenant_id = request.headers.get("X-Tenant-ID")
        if not tenant_id:
            tenant_id = request.query_params.get("tenant_id")
        if not tenant_id:
            tenant_id = "default"
        return tenant_id

    @router.get("/summary")
    async def get_analytics_summary(
        request: Request,
        project_id: Optional[str] = Query(None, description="ID do projeto"),
        date_from: Optional[str] = Query(None, description="Data inicial (YYYY-MM-DD)"),
        date_to: Optional[str] = Query(None, description="Data final (YYYY-MM-DD)"),
        format: ExportFormat = Query(ExportFormat.JSON, description="Formato de saida"),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna resumo de analytics do projeto.

        Metricas incluidas:
        - Total de stories por status
        - Story points totais e concluidos
        - Velocidade media
        - Tempo de ciclo medio
        """
        provider = AnalyticsDataProvider(tenant_id)

        date_from_dt = datetime.fromisoformat(date_from) if date_from else None
        date_to_dt = datetime.fromisoformat(date_to) if date_to else None

        data = await provider.get_stories_summary(
            project_id=project_id,
            date_from=date_from_dt,
            date_to=date_to_dt
        )

        if format == ExportFormat.ODATA:
            return format_odata_response([data], "$metadata#summary")
        elif format == ExportFormat.CSV:
            return format_csv_response([data], "summary.csv")

        return data

    @router.get("/stories/by-status")
    async def get_stories_by_status(
        request: Request,
        project_id: Optional[str] = Query(None),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna contagem de stories agrupadas por status.

        Ideal para graficos de pizza/donut no Power BI.
        """
        provider = AnalyticsDataProvider(tenant_id)
        data = await provider.get_stories_by_status(project_id)

        if format == ExportFormat.ODATA:
            return format_odata_response(data, "$metadata#storiesByStatus")
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "stories_by_status.csv")

        return data

    @router.get("/stories/timeline")
    async def get_stories_timeline(
        request: Request,
        project_id: Optional[str] = Query(None),
        granularity: TimeGranularity = Query(TimeGranularity.DAY),
        date_from: Optional[str] = Query(None),
        date_to: Optional[str] = Query(None),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna timeline de stories.

        Ideal para graficos de linha/area no Power BI.
        """
        provider = AnalyticsDataProvider(tenant_id)

        date_from_dt = datetime.fromisoformat(date_from) if date_from else None
        date_to_dt = datetime.fromisoformat(date_to) if date_to else None

        data = await provider.get_stories_timeline(
            project_id=project_id,
            granularity=granularity,
            date_from=date_from_dt,
            date_to=date_to_dt
        )

        if format == ExportFormat.ODATA:
            return format_odata_response(data, "$metadata#storiesTimeline")
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "stories_timeline.csv")

        return data

    @router.get("/sprints")
    async def get_sprint_metrics(
        request: Request,
        sprint_id: Optional[str] = Query(None),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna metricas de sprints.

        Incluindo velocidade, burndown, completion rate.
        """
        provider = AnalyticsDataProvider(tenant_id)
        data = await provider.get_sprint_metrics(sprint_id)

        if format == ExportFormat.ODATA:
            return format_odata_response(data, "$metadata#sprintMetrics")
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "sprint_metrics.csv")

        return data

    @router.get("/team/performance")
    async def get_team_performance(
        request: Request,
        project_id: Optional[str] = Query(None),
        date_from: Optional[str] = Query(None),
        date_to: Optional[str] = Query(None),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna performance por membro do time.

        Metricas por usuario: stories, pontos, tempo de ciclo.
        """
        provider = AnalyticsDataProvider(tenant_id)

        date_from_dt = datetime.fromisoformat(date_from) if date_from else None
        date_to_dt = datetime.fromisoformat(date_to) if date_to else None

        data = await provider.get_team_performance(
            project_id=project_id,
            date_from=date_from_dt,
            date_to=date_to_dt
        )

        if format == ExportFormat.ODATA:
            return format_odata_response(data, "$metadata#teamPerformance")
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "team_performance.csv")

        return data

    @router.get("/projects/{project_id}/health")
    async def get_project_health(
        project_id: str,
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna indicadores de saude do projeto.

        Health score, metricas, riscos e recomendacoes.
        """
        provider = AnalyticsDataProvider(tenant_id)
        data = await provider.get_project_health(project_id)

        if format == ExportFormat.ODATA:
            return format_odata_response([data], "$metadata#projectHealth")

        return data

    @router.get("/stories")
    async def get_stories_list(
        request: Request,
        project_id: Optional[str] = Query(None),
        status: Optional[str] = Query(None),
        limit: int = Query(100, ge=1, le=1000, alias="$top"),
        offset: int = Query(0, ge=0, alias="$skip"),
        count: bool = Query(False, alias="$count"),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Lista stories com paginacao.

        Suporta parametros OData: $top, $skip, $count.
        Ideal para conexao direta do Power BI.
        """
        provider = AnalyticsDataProvider(tenant_id)
        data, total = await provider.get_stories_list(
            project_id=project_id,
            status=status,
            limit=limit,
            offset=offset
        )

        if format == ExportFormat.ODATA:
            next_link = None
            if offset + limit < total:
                next_link = f"{request.url.path}?$top={limit}&$skip={offset + limit}"
                if project_id:
                    next_link += f"&project_id={project_id}"
                if status:
                    next_link += f"&status={status}"

            return format_odata_response(
                data,
                "$metadata#stories",
                count=total if count else None,
                next_link=next_link
            )
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "stories.csv")

        return {
            "data": data,
            "total": total,
            "limit": limit,
            "offset": offset
        }

    @router.get("/velocity")
    async def get_velocity_metrics(
        request: Request,
        project_id: Optional[str] = Query(None),
        sprints: int = Query(6, ge=1, le=20, description="Numero de sprints"),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna metricas de velocidade.

        Velocidade por sprint, media movel, tendencia.
        """
        # Dados de exemplo
        data = {
            "current_velocity": 42,
            "average_velocity": 38.5,
            "velocity_trend": "increasing",
            "sprints": [
                {"sprint": "Sprint 1", "velocity": 35, "planned": 40},
                {"sprint": "Sprint 2", "velocity": 38, "planned": 40},
                {"sprint": "Sprint 3", "velocity": 40, "planned": 42},
                {"sprint": "Sprint 4", "velocity": 42, "planned": 45},
                {"sprint": "Sprint 5", "velocity": 39, "planned": 42},
                {"sprint": "Sprint 6", "velocity": 42, "planned": 42}
            ]
        }

        if format == ExportFormat.ODATA:
            return format_odata_response([data], "$metadata#velocity")
        elif format == ExportFormat.CSV:
            return format_csv_response(data["sprints"], "velocity.csv")

        return data

    @router.get("/burndown")
    async def get_burndown_chart(
        request: Request,
        sprint_id: str = Query(..., description="ID do sprint"),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna dados para grafico de burndown.

        Pontos restantes por dia, linha ideal, linha real.
        """
        # Dados de exemplo
        data = {
            "sprint_id": sprint_id,
            "total_points": 45,
            "days": [
                {"day": 1, "ideal": 42, "actual": 45, "date": "2024-01-15"},
                {"day": 2, "ideal": 39, "actual": 43, "date": "2024-01-16"},
                {"day": 3, "ideal": 36, "actual": 40, "date": "2024-01-17"},
                {"day": 4, "ideal": 33, "actual": 35, "date": "2024-01-18"},
                {"day": 5, "ideal": 30, "actual": 32, "date": "2024-01-19"},
                {"day": 6, "ideal": 27, "actual": 28, "date": "2024-01-20"},
                {"day": 7, "ideal": 24, "actual": 25, "date": "2024-01-21"},
                {"day": 8, "ideal": 21, "actual": 20, "date": "2024-01-22"},
                {"day": 9, "ideal": 18, "actual": 15, "date": "2024-01-23"},
                {"day": 10, "ideal": 15, "actual": 12, "date": "2024-01-24"}
            ]
        }

        if format == ExportFormat.ODATA:
            return format_odata_response(data["days"], "$metadata#burndown")
        elif format == ExportFormat.CSV:
            return format_csv_response(data["days"], f"burndown_{sprint_id}.csv")

        return data

    @router.get("/cumulative-flow")
    async def get_cumulative_flow(
        request: Request,
        project_id: Optional[str] = Query(None),
        days: int = Query(30, ge=7, le=90),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna dados para diagrama de fluxo cumulativo.

        Acumulado de stories por status ao longo do tempo.
        """
        now = datetime.utcnow()
        data = []

        for i in range(days):
            date = now - timedelta(days=days-1-i)
            data.append({
                "date": date.strftime("%Y-%m-%d"),
                "backlog": 50 - (i * 0.5),
                "ready": 10 + (i * 0.2),
                "in_progress": 8 + (i * 0.1),
                "review": 5 + (i * 0.1),
                "testing": 4 + (i * 0.1),
                "done": 5 + (i * 0.8)
            })

        if format == ExportFormat.ODATA:
            return format_odata_response(data, "$metadata#cumulativeFlow")
        elif format == ExportFormat.CSV:
            return format_csv_response(data, "cumulative_flow.csv")

        return data

    @router.get("/cycle-time")
    async def get_cycle_time_metrics(
        request: Request,
        project_id: Optional[str] = Query(None),
        date_from: Optional[str] = Query(None),
        date_to: Optional[str] = Query(None),
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Retorna metricas de tempo de ciclo.

        Tempo medio por etapa, distribuicao, outliers.
        """
        data = {
            "average_cycle_time_days": 4.2,
            "median_cycle_time_days": 3.5,
            "p85_cycle_time_days": 7.0,
            "by_status": {
                "ready_to_in_progress": 0.5,
                "in_progress_to_review": 2.1,
                "review_to_testing": 0.8,
                "testing_to_done": 0.8
            },
            "by_complexity": [
                {"complexity": "low", "avg_days": 2.1, "count": 45},
                {"complexity": "medium", "avg_days": 4.5, "count": 38},
                {"complexity": "high", "avg_days": 7.2, "count": 22},
                {"complexity": "very_high", "avg_days": 12.5, "count": 8}
            ],
            "distribution": [
                {"range": "0-2 days", "count": 25, "percentage": 22.1},
                {"range": "2-4 days", "count": 45, "percentage": 39.8},
                {"range": "4-7 days", "count": 30, "percentage": 26.5},
                {"range": "7+ days", "count": 13, "percentage": 11.5}
            ]
        }

        if format == ExportFormat.ODATA:
            return format_odata_response([data], "$metadata#cycleTime")

        return data

    @router.post("/custom-query")
    async def execute_custom_query(
        request: Request,
        query: Dict,
        format: ExportFormat = Query(ExportFormat.JSON),
        tenant_id: str = Depends(get_tenant_id)
    ):
        """
        Executa query customizada de analytics.

        Permite construir queries complexas com agregacoes.
        """
        # Validar e executar query
        builder = AnalyticsQueryBuilder()

        if "select" in query:
            builder.select(*query["select"])
        if "where" in query:
            for condition in query["where"]:
                builder.where(condition["field"], condition["operator"], condition["value"])
        if "group_by" in query:
            builder.group_by(*query["group_by"])
        if "aggregates" in query:
            for agg in query["aggregates"]:
                builder.aggregate(agg["field"], AggregationType(agg["type"]), agg.get("alias"))
        if "order_by" in query:
            for order in query["order_by"]:
                builder.order_by(order["field"], order.get("desc", False))
        if "limit" in query:
            builder.limit(query["limit"])
        if "offset" in query:
            builder.offset(query["offset"])

        # Verificar cache
        query_hash = builder.hash()
        cached = _analytics_cache.get(tenant_id, query_hash)
        if cached:
            return cached

        # Executar query (implementacao simplificada)
        result = {"query": builder.to_dict(), "data": [], "cached": False}

        # Salvar no cache
        _analytics_cache.set(tenant_id, query_hash, result)

        return result

    @router.delete("/cache")
    async def clear_analytics_cache(
        tenant_id: str = Depends(get_tenant_id)
    ):
        """Limpa cache de analytics do tenant"""
        _analytics_cache.invalidate(tenant_id)
        return {"message": "Cache cleared", "tenant_id": tenant_id}


# Funcao para registrar rotas
def register_analytics_routes(app):
    """Registra rotas de analytics no app FastAPI"""
    if FASTAPI_AVAILABLE:
        app.include_router(router)
        logger.info("Analytics routes registered")
    else:
        logger.warning("FastAPI not available, analytics routes not registered")
