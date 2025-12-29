# -*- coding: utf-8 -*-
"""
Analytics API Routes
====================
Endpoints de analytics para Power BI e BI Tools.

Endpoints OData-compatible:
- GET /api/v1/analytics/stories
- GET /api/v1/analytics/metrics
- GET /api/v1/analytics/velocity
- GET /api/v1/analytics/burndown
- GET /api/v1/analytics/odata/stories
- GET /api/v1/analytics/odata/tasks
- GET /api/v1/analytics/odata/$metadata

Endpoints para Tableau:
- GET /api/v1/analytics/tableau/schema
- GET /api/v1/analytics/tableau/data
- GET /api/v1/analytics/tableau/wdc

Endpoints para Excel:
- GET /api/v1/analytics/export/excel
- GET /api/v1/analytics/export/csv
"""

import logging
from datetime import datetime
from typing import Optional, List

from fastapi import APIRouter, HTTPException, Query, Request, Response, Depends
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field
import io

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1/analytics", tags=["Analytics"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class MetricsRequest(BaseModel):
    """Request para metricas"""
    project_id: str = Field(..., description="ID do projeto")
    sprint_id: Optional[str] = Field(None, description="ID do sprint")
    period: Optional[str] = Field("week", description="Periodo: day, week, month, sprint")


class VelocityResponse(BaseModel):
    """Response de velocity"""
    current: float
    average: float
    trend: str
    change_percentage: float
    history: List[dict]


class BurndownPoint(BaseModel):
    """Ponto do burndown chart"""
    date: str
    remaining_points: int
    remaining_stories: int
    ideal_points: float
    completed_points: int


class ODataResponse(BaseModel):
    """Response padrao OData"""
    odata_context: str = Field(..., alias="@odata.context")
    odata_count: Optional[int] = Field(None, alias="@odata.count")
    value: List[dict]


# =============================================================================
# HELPERS
# =============================================================================

def get_tenant_id(request: Request) -> str:
    """Extrai tenant_id do request"""
    # Tentar do header
    tenant_id = request.headers.get("X-Tenant-ID")
    if tenant_id:
        return tenant_id

    # Tentar do token JWT
    try:
        from factory.api.auth import get_current_user_optional
        # Implementar se necessario
    except:
        pass

    # Default para desenvolvimento
    return "default"


# =============================================================================
# METRICS ENDPOINTS
# =============================================================================

@router.get("/stories")
async def get_stories_metrics(
    request: Request,
    project_id: str = Query(..., description="ID do projeto")
):
    """
    Retorna metricas agregadas de stories do projeto.

    Inclui:
    - Total de stories por status
    - Distribuicao por prioridade
    - Distribuicao por categoria
    - Progresso geral
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.core.analytics_service import get_analytics_service

        service = get_analytics_service(tenant_id)
        metrics = service.get_project_metrics(project_id)

        return metrics

    except Exception as e:
        logger.error(f"Erro ao buscar metricas: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/metrics")
async def get_all_metrics(
    request: Request,
    project_id: str = Query(..., description="ID do projeto"),
    sprint_id: Optional[str] = Query(None, description="ID do sprint")
):
    """
    Retorna todas as metricas do projeto/sprint.

    Metricas incluidas:
    - Velocity
    - Throughput
    - Cycle Time
    - Lead Time
    - WIP
    - Distribuicoes
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.core.analytics_service import get_analytics_service

        service = get_analytics_service(tenant_id)
        metrics = service.get_project_metrics(project_id)

        if sprint_id:
            burndown = service.generate_burndown(sprint_id, project_id)
            metrics["burndown"] = [
                {
                    "date": b.date.isoformat(),
                    "remaining_points": b.remaining_points,
                    "ideal_points": b.ideal_points
                }
                for b in burndown
            ]

        return metrics

    except Exception as e:
        logger.error(f"Erro ao buscar metricas: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/velocity")
async def get_velocity(
    request: Request,
    project_id: str = Query(..., description="ID do projeto"),
    num_sprints: int = Query(5, description="Numero de sprints para media")
):
    """
    Retorna dados de velocity do projeto.

    Velocity = Story Points entregues por sprint.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.core.analytics_service import get_analytics_service

        service = get_analytics_service(tenant_id)
        velocity = service.calculate_velocity(project_id, num_sprints)

        return {
            "project_id": project_id,
            "current": velocity.value,
            "average": velocity.details.get("average", 0),
            "trend": velocity.trend,
            "change_percentage": velocity.change_percentage,
            "history": velocity.details.get("history", []),
            "period_start": velocity.period_start.isoformat(),
            "period_end": velocity.period_end.isoformat()
        }

    except Exception as e:
        logger.error(f"Erro ao calcular velocity: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/burndown")
async def get_burndown(
    request: Request,
    sprint_id: str = Query(..., description="ID do sprint"),
    project_id: Optional[str] = Query(None, description="ID do projeto")
):
    """
    Retorna dados para burndown chart do sprint.

    Inclui:
    - Pontos restantes por dia
    - Linha ideal
    - Pontos completados
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.core.analytics_service import get_analytics_service

        service = get_analytics_service(tenant_id)
        burndown = service.generate_burndown(sprint_id, project_id)

        return {
            "sprint_id": sprint_id,
            "data": [
                {
                    "date": b.date.isoformat(),
                    "remaining_points": b.remaining_points,
                    "remaining_stories": b.remaining_stories,
                    "ideal_points": round(b.ideal_points, 2),
                    "completed_points": b.completed_points
                }
                for b in burndown
            ]
        }

    except Exception as e:
        logger.error(f"Erro ao gerar burndown: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# ODATA ENDPOINTS
# =============================================================================

@router.get("/odata/$metadata")
async def get_odata_metadata(request: Request):
    """
    Retorna metadata EDMX para Power BI.

    Este endpoint permite que Power BI descubra o schema dos dados.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.powerbi_connector import get_powerbi_connector

        connector = get_powerbi_connector(tenant_id)
        metadata = connector.get_odata_metadata()

        return Response(
            content=metadata,
            media_type="application/xml"
        )

    except Exception as e:
        logger.error(f"Erro ao gerar metadata: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/odata/stories")
async def get_odata_stories(
    request: Request,
    project_id: Optional[str] = Query(None, description="Filtrar por projeto"),
    top: int = Query(100, alias="$top", description="Limite de resultados"),
    skip: int = Query(0, alias="$skip", description="Pular N resultados"),
    filter: Optional[str] = Query(None, alias="$filter", description="Filtro OData"),
    orderby: Optional[str] = Query(None, alias="$orderby", description="Ordenacao"),
    select: Optional[str] = Query(None, alias="$select", description="Campos a retornar")
):
    """
    Retorna stories em formato OData para Power BI.

    Suporta operadores OData:
    - $top: Limite de resultados
    - $skip: Paginacao
    - $filter: Filtros
    - $orderby: Ordenacao
    - $select: Campos especificos
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.core.analytics_service import get_analytics_service

        service = get_analytics_service(tenant_id)

        select_fields = select.split(",") if select else None
        result = service.get_odata_stories(
            project_id=project_id,
            top=top,
            skip=skip,
            filter_expr=filter,
            orderby=orderby,
            select=select_fields
        )

        return result

    except Exception as e:
        logger.error(f"Erro ao buscar stories OData: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/odata/metrics")
async def get_odata_metrics(
    request: Request,
    project_id: Optional[str] = Query(None, description="Filtrar por projeto")
):
    """
    Retorna metricas em formato OData.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.powerbi_connector import get_powerbi_connector

        connector = get_powerbi_connector(tenant_id)
        metrics = connector.get_metrics_feed(project_id)

        return metrics

    except Exception as e:
        logger.error(f"Erro ao buscar metricas OData: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# TABLEAU ENDPOINTS
# =============================================================================

@router.get("/tableau/schema")
async def get_tableau_schema(request: Request):
    """
    Retorna schema para Tableau Web Data Connector.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.tableau_connector import get_tableau_connector

        connector = get_tableau_connector(tenant_id)
        schema = connector.get_tableau_schema()

        return schema

    except Exception as e:
        logger.error(f"Erro ao gerar schema Tableau: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/tableau/data")
async def get_tableau_data(
    request: Request,
    project_id: str = Query(..., description="ID do projeto"),
    table: str = Query("stories", description="Tabela: stories, tasks, metrics")
):
    """
    Retorna dados para Tableau Web Data Connector.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.tableau_connector import get_tableau_connector

        connector = get_tableau_connector(tenant_id)
        data = connector.get_wdc_data(
            project_id=project_id,
            table_id=table,
            include_metadata=False
        )

        return data

    except Exception as e:
        logger.error(f"Erro ao buscar dados Tableau: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/tableau/wdc/{project_id}")
async def get_tableau_wdc(
    request: Request,
    project_id: str
):
    """
    Retorna HTML do Web Data Connector para Tableau.

    Acesse este endpoint no Tableau Desktop para conectar.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.tableau_connector import get_tableau_connector

        connector = get_tableau_connector(tenant_id)
        html = connector.get_wdc_html(project_id)

        return Response(
            content=html,
            media_type="text/html"
        )

    except Exception as e:
        logger.error(f"Erro ao gerar WDC: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# EXPORT ENDPOINTS
# =============================================================================

@router.get("/export/excel")
async def export_to_excel(
    request: Request,
    project_id: str = Query(..., description="ID do projeto"),
    include_tasks: bool = Query(True, description="Incluir tasks"),
    include_metrics: bool = Query(True, description="Incluir metricas"),
    include_charts: bool = Query(True, description="Incluir graficos")
):
    """
    Exporta projeto para Excel (.xlsx).

    Gera arquivo Excel com:
    - Dashboard com graficos
    - Planilha de Stories
    - Planilha de Tasks (opcional)
    - Planilha de Metricas (opcional)
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.excel_exporter import get_excel_exporter

        exporter = get_excel_exporter(tenant_id)
        excel_bytes = exporter.export_project(
            project_id=project_id,
            include_tasks=include_tasks,
            include_metrics=include_metrics,
            include_charts=include_charts
        )

        filename = f"projeto_{project_id}_{datetime.utcnow().strftime('%Y%m%d')}.xlsx"

        return StreamingResponse(
            io.BytesIO(excel_bytes),
            media_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            headers={
                "Content-Disposition": f"attachment; filename={filename}"
            }
        )

    except Exception as e:
        logger.error(f"Erro ao exportar Excel: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/export/csv")
async def export_to_csv(
    request: Request,
    project_id: str = Query(..., description="ID do projeto")
):
    """
    Exporta stories para CSV.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.excel_exporter import get_excel_exporter

        exporter = get_excel_exporter(tenant_id)
        csv_content = exporter.export_to_csv(project_id)

        filename = f"stories_{project_id}_{datetime.utcnow().strftime('%Y%m%d')}.csv"

        return Response(
            content=csv_content,
            media_type="text/csv",
            headers={
                "Content-Disposition": f"attachment; filename={filename}"
            }
        )

    except Exception as e:
        logger.error(f"Erro ao exportar CSV: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# POWERBI ENDPOINTS
# =============================================================================

@router.get("/powerbi/config")
async def get_powerbi_config(
    request: Request,
    project_id: str = Query(..., description="ID do projeto")
):
    """
    Retorna configuracao para DirectQuery do Power BI.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.powerbi_connector import get_powerbi_connector

        connector = get_powerbi_connector(tenant_id)
        config = connector.export_for_direct_query(project_id)

        return config

    except Exception as e:
        logger.error(f"Erro ao gerar config Power BI: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# INTEGRATIONS STATUS
# =============================================================================

@router.get("/integrations/status")
async def get_integrations_status(request: Request):
    """
    Retorna status de todas as integracoes de BI.
    """
    tenant_id = get_tenant_id(request)

    return {
        "tenant_id": tenant_id,
        "timestamp": datetime.utcnow().isoformat(),
        "integrations": {
            "powerbi": {
                "enabled": True,
                "endpoints": [
                    "/api/v1/analytics/odata/$metadata",
                    "/api/v1/analytics/odata/stories",
                    "/api/v1/analytics/powerbi/config"
                ]
            },
            "tableau": {
                "enabled": True,
                "endpoints": [
                    "/api/v1/analytics/tableau/schema",
                    "/api/v1/analytics/tableau/data",
                    "/api/v1/analytics/tableau/wdc/{project_id}"
                ]
            },
            "excel": {
                "enabled": True,
                "endpoints": [
                    "/api/v1/analytics/export/excel",
                    "/api/v1/analytics/export/csv"
                ]
            }
        }
    }
