"""
Módulo de Métricas - Observabilidade com Prometheus
====================================================
Issue #378: Implementa métricas para monitoramento da aplicação

Métricas disponíveis:
- http_requests_total: Total de requisições HTTP
- http_request_duration_seconds: Latência das requisições
- http_requests_in_progress: Requisições em andamento
- stories_total: Total de stories por status
- tasks_total: Total de tasks por status
- workers_active: Workers ativos
- jobs_total: Jobs por status
"""

import time
from functools import wraps
from typing import Callable

from prometheus_client import Counter, Histogram, Gauge, Info, generate_latest, CONTENT_TYPE_LATEST
from fastapi import APIRouter, Response
from fastapi.requests import Request

# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(tags=["Metrics"])

# =============================================================================
# MÉTRICAS DE APLICAÇÃO
# =============================================================================

# Info da aplicação
APP_INFO = Info(
    "fabrica_agentes",
    "Informações da Plataforma E"
)
APP_INFO.info({
    "version": "6.5",
    "environment": "development"
})

# Métricas de Stories
STORIES_TOTAL = Gauge(
    "fabrica_stories_total",
    "Total de User Stories por status",
    ["status"]
)

STORIES_CREATED = Counter(
    "fabrica_stories_created_total",
    "Total de stories criadas"
)

# Métricas de Tasks
TASKS_TOTAL = Gauge(
    "fabrica_tasks_total",
    "Total de Tasks por status",
    ["status"]
)

TASKS_COMPLETED = Counter(
    "fabrica_tasks_completed_total",
    "Total de tasks completadas"
)

# Métricas de Workers
WORKERS_ACTIVE = Gauge(
    "fabrica_workers_active",
    "Número de workers ativos"
)

WORKERS_TOTAL = Gauge(
    "fabrica_workers_total",
    "Total de workers configurados"
)

# Métricas de Jobs
JOBS_TOTAL = Gauge(
    "fabrica_jobs_total",
    "Total de jobs por status",
    ["status"]
)

JOBS_PROCESSED = Counter(
    "fabrica_jobs_processed_total",
    "Total de jobs processados",
    ["result"]  # success, failed, timeout
)

JOB_DURATION = Histogram(
    "fabrica_job_duration_seconds",
    "Duração do processamento de jobs",
    buckets=[1, 5, 10, 30, 60, 120, 300, 600]
)

# Métricas de Database
DB_CONNECTIONS_ACTIVE = Gauge(
    "fabrica_db_connections_active",
    "Conexões ativas com o banco"
)

DB_QUERY_DURATION = Histogram(
    "fabrica_db_query_duration_seconds",
    "Duração das queries ao banco",
    buckets=[0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0]
)

# Métricas de Cache (Redis)
CACHE_HITS = Counter(
    "fabrica_cache_hits_total",
    "Total de cache hits"
)

CACHE_MISSES = Counter(
    "fabrica_cache_misses_total",
    "Total de cache misses"
)

# Métricas de AI/Claude
CLAUDE_REQUESTS = Counter(
    "fabrica_claude_requests_total",
    "Total de requisições à API Claude",
    ["model", "result"]  # result: success, error, timeout
)

CLAUDE_TOKENS_USED = Counter(
    "fabrica_claude_tokens_total",
    "Total de tokens utilizados",
    ["type"]  # input, output
)

CLAUDE_LATENCY = Histogram(
    "fabrica_claude_latency_seconds",
    "Latência das chamadas à API Claude",
    buckets=[0.5, 1, 2, 5, 10, 30, 60]
)

# =============================================================================
# HELPERS
# =============================================================================

def track_job_duration(func: Callable) -> Callable:
    """Decorator para medir duração de jobs"""
    @wraps(func)
    async def wrapper(*args, **kwargs):
        start = time.time()
        try:
            result = await func(*args, **kwargs)
            JOBS_PROCESSED.labels(result="success").inc()
            return result
        except TimeoutError:
            JOBS_PROCESSED.labels(result="timeout").inc()
            raise
        except Exception:
            JOBS_PROCESSED.labels(result="failed").inc()
            raise
        finally:
            JOB_DURATION.observe(time.time() - start)
    return wrapper


def track_claude_call(model: str = "sonnet"):
    """Decorator para medir chamadas à API Claude"""
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            start = time.time()
            try:
                result = await func(*args, **kwargs)
                CLAUDE_REQUESTS.labels(model=model, result="success").inc()
                return result
            except Exception as e:
                if "timeout" in str(e).lower():
                    CLAUDE_REQUESTS.labels(model=model, result="timeout").inc()
                else:
                    CLAUDE_REQUESTS.labels(model=model, result="error").inc()
                raise
            finally:
                CLAUDE_LATENCY.observe(time.time() - start)
        return wrapper
    return decorator


async def update_business_metrics():
    """
    Atualiza métricas de negócio consultando o banco.
    Chamado periodicamente ou em eventos específicos.
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Story, StoryTask, Project
        from sqlalchemy import func

        db = SessionLocal()
        try:
            # Contagem de stories por status
            story_counts = db.query(
                Story.status, func.count(Story.story_id)
            ).group_by(Story.status).all()

            for status, count in story_counts:
                STORIES_TOTAL.labels(status=status).set(count)

            # Contagem de tasks por status
            task_counts = db.query(
                StoryTask.status, func.count(StoryTask.task_id)
            ).group_by(StoryTask.status).all()

            for status, count in task_counts:
                TASKS_TOTAL.labels(status=status).set(count)

        finally:
            db.close()
    except Exception:
        # Não falhar se não conseguir atualizar métricas
        pass


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/metrics", include_in_schema=False)
async def metrics():
    """
    Endpoint de métricas para scraping do Prometheus.

    Formato: text/plain (Prometheus exposition format)
    """
    # Atualiza métricas de negócio antes de retornar
    await update_business_metrics()

    return Response(
        content=generate_latest(),
        media_type=CONTENT_TYPE_LATEST
    )


@router.get("/metrics/health")
async def metrics_health():
    """Health check específico para métricas"""
    return {
        "status": "healthy",
        "metrics_enabled": True
    }


# =============================================================================
# SETUP
# =============================================================================

def setup_metrics(app):
    """
    Configura instrumentação automática do FastAPI.

    Uso no app principal:
        from factory.api.metrics import setup_metrics
        setup_metrics(app)
    """
    try:
        from prometheus_fastapi_instrumentator import Instrumentator

        # Instrumentador com métricas padrão HTTP
        instrumentator = Instrumentator(
            should_group_status_codes=True,
            should_ignore_untemplated=True,
            should_respect_env_var=True,
            should_instrument_requests_inprogress=True,
            excluded_handlers=["/health", "/metrics"],
            inprogress_name="http_requests_inprogress",
            inprogress_labels=True,
        )

        # Adiciona métricas customizadas
        instrumentator.add(
            lambda info: info.modified_duration
        )

        # Instrumenta a app
        instrumentator.instrument(app).expose(
            app,
            endpoint="/metrics/http",
            include_in_schema=False
        )

        print("[Metrics] Instrumentação Prometheus configurada")
        return True

    except ImportError:
        print("[Metrics] prometheus-fastapi-instrumentator não instalado")
        return False
    except Exception as e:
        print(f"[Metrics] Erro ao configurar instrumentação: {e}")
        return False
