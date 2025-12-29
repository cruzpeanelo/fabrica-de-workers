"""
Fabrica de Agentes - Metrics API Endpoint
==========================================
Issue #96: Stack de Metricas Prometheus/Grafana

Este modulo fornece o endpoint /metrics para exposicao de
metricas Prometheus.

Uso:
    from factory.api.metrics_endpoint import router as metrics_router
    app.include_router(metrics_router)

Endpoints:
    GET /metrics - Metricas Prometheus em formato text/plain
    GET /metrics/json - Metricas em formato JSON (debug)
"""

from fastapi import APIRouter, Response, Request
from fastapi.responses import PlainTextResponse, JSONResponse
import time
import logging

from factory.core.metrics import (
    get_metrics, get_metrics_content_type,
    api_requests, api_latency, api_requests_in_progress,
    update_queue_metrics, update_story_metrics,
    active_workers, jobs_pending, jobs_running,
    stories_total, story_points_total
)

logger = logging.getLogger(__name__)

router = APIRouter(tags=["Metrics"])


# =============================================================================
# Middleware para metricas automaticas
# =============================================================================

class MetricsMiddleware:
    """
    Middleware para coletar metricas de todas as requests.

    Uso:
        app.add_middleware(MetricsMiddleware)
    """

    def __init__(self, app):
        self.app = app

    async def __call__(self, scope, receive, send):
        if scope["type"] != "http":
            await self.app(scope, receive, send)
            return

        start_time = time.time()
        path = scope["path"]
        method = scope["method"]

        # Ignorar metricas do proprio endpoint de metricas
        if path == "/metrics":
            await self.app(scope, receive, send)
            return

        # Normalizar path para evitar cardinalidade alta
        normalized_path = self._normalize_path(path)

        # Incrementar requests em progresso
        api_requests_in_progress.labels(endpoint=normalized_path).inc()

        # Capturar status code
        status_code = 500

        async def send_wrapper(message):
            nonlocal status_code
            if message["type"] == "http.response.start":
                status_code = message["status"]
            await send(message)

        try:
            await self.app(scope, receive, send_wrapper)
        finally:
            # Decrementar requests em progresso
            api_requests_in_progress.labels(endpoint=normalized_path).dec()

            # Registrar request
            duration = time.time() - start_time
            api_requests.labels(
                method=method,
                endpoint=normalized_path,
                status=str(status_code)
            ).inc()
            api_latency.labels(endpoint=normalized_path).observe(duration)

    def _normalize_path(self, path: str) -> str:
        """
        Normaliza path para evitar cardinalidade alta nas metricas.

        Exemplo:
            /api/jobs/abc123 -> /api/jobs/{id}
            /api/stories/STR-0001/tasks -> /api/stories/{id}/tasks
        """
        parts = path.strip("/").split("/")
        normalized_parts = []

        for i, part in enumerate(parts):
            # Detectar IDs (UUIDs, job IDs, story IDs, etc)
            if self._is_id(part):
                normalized_parts.append("{id}")
            else:
                normalized_parts.append(part)

        return "/" + "/".join(normalized_parts)

    def _is_id(self, part: str) -> bool:
        """Verifica se a parte e um ID."""
        # UUID
        if len(part) == 36 and part.count("-") == 4:
            return True
        # Short ID (8+ chars hex)
        if len(part) >= 8 and all(c in "0123456789abcdef-" for c in part.lower()):
            return True
        # Story ID (STR-0001)
        if part.startswith(("STR-", "TSK-", "STSK-", "DOC-", "JOB-")):
            return True
        # Numeric ID
        if part.isdigit():
            return True
        return False


# =============================================================================
# Endpoints
# =============================================================================

@router.get("/metrics", response_class=PlainTextResponse)
async def metrics():
    """
    Endpoint de metricas Prometheus.

    Retorna todas as metricas da aplicacao em formato Prometheus.
    Este endpoint e consumido pelo Prometheus server.

    Returns:
        text/plain: Metricas formatadas para Prometheus
    """
    try:
        # Atualizar metricas dinamicas antes de retornar
        await _update_dynamic_metrics()

        # Gerar metricas
        metrics_data = get_metrics()

        return Response(
            content=metrics_data,
            media_type=get_metrics_content_type()
        )
    except Exception as e:
        logger.error(f"Error generating metrics: {e}")
        return Response(
            content=f"# Error generating metrics: {e}\n",
            media_type="text/plain",
            status_code=500
        )


@router.get("/metrics/json")
async def metrics_json():
    """
    Endpoint de metricas em formato JSON (para debug).

    Retorna um resumo das metricas principais em JSON.

    Returns:
        JSON: Resumo das metricas
    """
    try:
        # Importar para buscar valores atuais
        from prometheus_client import REGISTRY

        metrics_summary = {
            "api": {
                "total_requests": _get_counter_value(api_requests),
            },
            "workers": {
                "active": _get_gauge_value(active_workers),
                "jobs_pending": _get_gauge_value(jobs_pending),
                "jobs_running": _get_gauge_value(jobs_running),
            },
            "stories": {
                status: _get_gauge_value(stories_total, {"status": status})
                for status in ["backlog", "ready", "in_progress", "review", "testing", "done"]
            }
        }

        return JSONResponse(content=metrics_summary)
    except Exception as e:
        logger.error(f"Error generating metrics JSON: {e}")
        return JSONResponse(
            content={"error": str(e)},
            status_code=500
        )


# =============================================================================
# Helper Functions
# =============================================================================

async def _update_dynamic_metrics():
    """Atualiza metricas que precisam ser buscadas de outras fontes."""
    try:
        # Buscar estatisticas da fila
        from factory.core.job_queue import get_queue
        queue = await get_queue()
        stats = await queue.get_stats()

        update_queue_metrics(
            pending=stats.get("pending", 0),
            running=stats.get("running", 0),
            workers=len(await queue.get_active_workers())
        )
    except Exception as e:
        logger.debug(f"Could not update queue metrics: {e}")

    try:
        # Buscar estatisticas de stories
        from factory.database.repositories import StoryRepository
        from factory.database.connection import get_db

        async with get_db() as db:
            repo = StoryRepository(db)
            stories_by_status = await repo.count_by_status()
            points_by_status = await repo.sum_points_by_status()

            update_story_metrics(stories_by_status, points_by_status)
    except Exception as e:
        logger.debug(f"Could not update story metrics: {e}")


def _get_counter_value(counter, labels: dict = None) -> float:
    """Obtem valor de um counter."""
    try:
        if labels:
            return counter.labels(**labels)._value.get()
        # Soma de todos os labels
        return sum(
            sample.value
            for sample in counter.collect()[0].samples
            if sample.name.endswith("_total")
        )
    except Exception:
        return 0.0


def _get_gauge_value(gauge, labels: dict = None) -> float:
    """Obtem valor de um gauge."""
    try:
        if labels:
            return gauge.labels(**labels)._value.get()
        return gauge._value.get()
    except Exception:
        return 0.0
