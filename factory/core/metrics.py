"""
Plataforma E - Prometheus Metrics Module
===============================================
Issue #96: Stack de Metricas Prometheus/Grafana

Este modulo define todas as metricas Prometheus para monitoramento
da aplicacao Plataforma E.

Metricas disponíveis:
- API requests (contador, latencia)
- Workers (ativos, jobs processados)
- Jobs (pendentes, rodando, completados, falhos)
- LLM (tokens utilizados, custo estimado)
- Database (conexoes, queries)
- Redis (comandos, memoria)

Uso:
    from factory.core.metrics import (
        api_requests, api_latency, track_request,
        active_workers, llm_tokens
    )

    # Incrementar contador de requests
    api_requests.labels(method='GET', endpoint='/api/jobs', status='200').inc()

    # Observar latencia
    with api_latency.labels(endpoint='/api/jobs').time():
        # codigo da request

    # Ou usar o decorator
    @track_request('/api/jobs')
    async def list_jobs():
        ...
"""

from prometheus_client import (
    Counter, Histogram, Gauge, Summary, Info,
    generate_latest, CONTENT_TYPE_LATEST,
    CollectorRegistry, REGISTRY
)
from functools import wraps
import time
from typing import Callable, Optional
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# API Metrics - Metricas da API REST
# =============================================================================

# Contador de requests por metodo, endpoint e status
api_requests = Counter(
    'api_requests_total',
    'Total number of API requests',
    ['method', 'endpoint', 'status']
)

# Histograma de latencia por endpoint
api_latency = Histogram(
    'api_request_duration_seconds',
    'API request latency in seconds',
    ['endpoint'],
    buckets=(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0)
)

# Contador de erros por tipo
api_errors = Counter(
    'api_errors_total',
    'Total number of API errors',
    ['endpoint', 'error_type']
)

# Requests em andamento (gauge)
api_requests_in_progress = Gauge(
    'api_requests_in_progress',
    'Number of API requests currently in progress',
    ['endpoint']
)


# =============================================================================
# Worker Metrics - Metricas dos Workers
# =============================================================================

# Numero de workers ativos
active_workers = Gauge(
    'active_workers',
    'Number of active workers'
)

# Jobs por status
jobs_pending = Gauge(
    'jobs_pending',
    'Number of pending jobs in queue'
)

jobs_running = Gauge(
    'jobs_running',
    'Number of currently running jobs'
)

# Contadores de jobs
jobs_completed_total = Counter(
    'jobs_completed_total',
    'Total number of completed jobs',
    ['worker_id']
)

jobs_failed_total = Counter(
    'jobs_failed_total',
    'Total number of failed jobs',
    ['worker_id', 'error_type']
)

jobs_cancelled_total = Counter(
    'jobs_cancelled_total',
    'Total number of cancelled jobs'
)

# Duracao dos jobs
job_duration_seconds = Histogram(
    'job_duration_seconds',
    'Job processing duration in seconds',
    ['job_type'],
    buckets=(10, 30, 60, 120, 300, 600, 1200, 1800, 3600)
)

# Steps por job
job_steps_total = Counter(
    'job_steps_total',
    'Total number of job steps executed',
    ['step_name', 'status']
)


# =============================================================================
# LLM Metrics - Metricas de uso do LLM
# =============================================================================

# Tokens utilizados por modelo e tipo
llm_tokens_total = Counter(
    'llm_tokens_total',
    'Total number of LLM tokens used',
    ['model', 'type']  # type: input, output
)

# Custo estimado em USD
llm_cost_usd = Counter(
    'llm_cost_usd_total',
    'Estimated LLM cost in USD',
    ['model']
)

# Chamadas ao LLM
llm_calls_total = Counter(
    'llm_calls_total',
    'Total number of LLM API calls',
    ['model', 'status']
)

# Latencia do LLM
llm_latency_seconds = Histogram(
    'llm_latency_seconds',
    'LLM API call latency in seconds',
    ['model'],
    buckets=(0.5, 1, 2, 5, 10, 20, 30, 60, 120)
)


# =============================================================================
# Database Metrics - Metricas do banco de dados
# =============================================================================

# Conexoes ativas
db_connections_active = Gauge(
    'db_connections_active',
    'Number of active database connections'
)

# Pool de conexoes
db_pool_size = Gauge(
    'db_pool_size',
    'Current database connection pool size'
)

# Queries executadas
db_queries_total = Counter(
    'db_queries_total',
    'Total number of database queries',
    ['operation', 'table']
)

# Latencia de queries
db_query_duration_seconds = Histogram(
    'db_query_duration_seconds',
    'Database query duration in seconds',
    ['operation'],
    buckets=(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0)
)


# =============================================================================
# Story/Task Metrics - Metricas de Stories e Tasks
# =============================================================================

stories_total = Gauge(
    'stories_total',
    'Total number of stories by status',
    ['status']
)

story_points_total = Gauge(
    'story_points_total',
    'Total story points by status',
    ['status']
)

tasks_total = Gauge(
    'tasks_total',
    'Total number of tasks by status',
    ['status']
)


# =============================================================================
# Application Info
# =============================================================================

app_info = Info(
    'fabrica_agentes',
    'Plataforma E application information'
)


# =============================================================================
# Helper Functions
# =============================================================================

def init_metrics(version: str = "6.5.0", environment: str = "production"):
    """
    Inicializa metricas da aplicacao.

    Args:
        version: Versao da aplicacao
        environment: Ambiente (production, staging, development)
    """
    app_info.info({
        'version': version,
        'environment': environment,
        'python_version': '3.12'
    })
    logger.info(f"Metrics initialized - version={version}, env={environment}")


def track_request(endpoint: str):
    """
    Decorator para rastrear metricas de requests.

    Uso:
        @track_request('/api/jobs')
        async def list_jobs():
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            api_requests_in_progress.labels(endpoint=endpoint).inc()
            start_time = time.time()
            status = '200'

            try:
                result = await func(*args, **kwargs)
                return result
            except Exception as e:
                status = '500'
                api_errors.labels(endpoint=endpoint, error_type=type(e).__name__).inc()
                raise
            finally:
                duration = time.time() - start_time
                api_requests_in_progress.labels(endpoint=endpoint).dec()
                api_latency.labels(endpoint=endpoint).observe(duration)
                # Note: status is set in middleware based on response

        return wrapper
    return decorator


def track_llm_usage(
    model: str,
    input_tokens: int,
    output_tokens: int,
    latency_seconds: float,
    success: bool = True
):
    """
    Registra uso do LLM.

    Args:
        model: Nome do modelo (opus, sonnet, haiku)
        input_tokens: Tokens de entrada
        output_tokens: Tokens de saida
        latency_seconds: Latencia da chamada
        success: Se a chamada foi bem sucedida
    """
    llm_tokens_total.labels(model=model, type='input').inc(input_tokens)
    llm_tokens_total.labels(model=model, type='output').inc(output_tokens)
    llm_latency_seconds.labels(model=model).observe(latency_seconds)
    llm_calls_total.labels(model=model, status='success' if success else 'error').inc()

    # Calculo estimado de custo (precos aproximados)
    cost_per_1k = {
        'opus': {'input': 0.015, 'output': 0.075},
        'sonnet': {'input': 0.003, 'output': 0.015},
        'haiku': {'input': 0.00025, 'output': 0.00125}
    }

    if model.lower() in cost_per_1k:
        rates = cost_per_1k[model.lower()]
        cost = (input_tokens / 1000 * rates['input']) + (output_tokens / 1000 * rates['output'])
        llm_cost_usd.labels(model=model).inc(cost)


def track_job_completion(
    worker_id: str,
    job_type: str,
    duration_seconds: float,
    success: bool = True,
    error_type: Optional[str] = None
):
    """
    Registra conclusao de job.

    Args:
        worker_id: ID do worker
        job_type: Tipo do job
        duration_seconds: Duracao em segundos
        success: Se o job foi bem sucedido
        error_type: Tipo de erro (se falhou)
    """
    job_duration_seconds.labels(job_type=job_type).observe(duration_seconds)

    if success:
        jobs_completed_total.labels(worker_id=worker_id).inc()
    else:
        jobs_failed_total.labels(worker_id=worker_id, error_type=error_type or 'unknown').inc()


def update_queue_metrics(pending: int, running: int, workers: int):
    """
    Atualiza metricas da fila.

    Args:
        pending: Jobs pendentes
        running: Jobs rodando
        workers: Workers ativos
    """
    jobs_pending.set(pending)
    jobs_running.set(running)
    active_workers.set(workers)


def update_story_metrics(stories_by_status: dict, points_by_status: dict):
    """
    Atualiza metricas de stories.

    Args:
        stories_by_status: Dict com contagem por status
        points_by_status: Dict com points por status
    """
    for status, count in stories_by_status.items():
        stories_total.labels(status=status).set(count)

    for status, points in points_by_status.items():
        story_points_total.labels(status=status).set(points)


# =============================================================================
# Metrics Endpoint Response
# =============================================================================

def get_metrics() -> bytes:
    """
    Retorna metricas em formato Prometheus.

    Returns:
        bytes: Metricas formatadas
    """
    return generate_latest(REGISTRY)


def get_metrics_content_type() -> str:
    """
    Retorna content-type para resposta de metricas.

    Returns:
        str: Content-type header value
    """
    return CONTENT_TYPE_LATEST
