"""
Factory API Routes v4.0 - Simplified Job-centric API
Fabrica de Agentes - Nova Arquitetura MVP
"""
from fastapi import APIRouter, HTTPException, Depends, Query
from pydantic import BaseModel, Field
from typing import List, Optional
from datetime import datetime

from factory.core.job_queue import get_queue, RedisJobQueue

router = APIRouter(prefix="/api/v1", tags=["Jobs"])


# =============================================================================
# MODELS
# =============================================================================

class JobCreate(BaseModel):
    """Request para criar job"""
    description: str = Field(..., min_length=10, description="Descricao do que construir")
    tech_stack: Optional[str] = Field(None, description="Stack tecnologica (python, fastapi, react, etc)")
    features: Optional[List[str]] = Field(default=[], description="Lista de features")
    project_id: Optional[str] = Field(None, description="ID do projeto associado")
    # Suporte a multiplos modelos (Issue #26)
    model: Optional[str] = Field(None, description="Modelo Claude (opus, sonnet, haiku ou ID completo)")
    complexity: Optional[str] = Field(None, description="Complexidade (simple, medium, complex, very_complex)")
    story_points: Optional[int] = Field(None, ge=1, le=21, description="Story points para selecao de modelo")


class ClaudeModelInfo(BaseModel):
    """Informacoes de um modelo Claude"""
    key: str
    id: str
    name: str
    description: str
    cost_tier: str
    recommended_for: List[str]


class JobResponse(BaseModel):
    """Response do job"""
    job_id: str
    description: str
    tech_stack: Optional[str]
    features: List[str]
    status: str
    current_step: str
    progress: float
    worker_id: Optional[str]
    output_path: Optional[str]
    error_message: Optional[str]
    queued_at: Optional[str]
    started_at: Optional[str]
    completed_at: Optional[str]


class QueueStats(BaseModel):
    """Estatisticas da fila"""
    pending: int
    running: int
    completed: int
    failed: int
    cancelled: int
    total: int
    queue_length: int


class WorkerResponse(BaseModel):
    """Response do worker"""
    worker_id: str
    status: str
    current_job_id: Optional[str]
    model: str
    mcp_tools: List[str]
    jobs_completed: int
    jobs_failed: int
    avg_job_duration: float
    last_heartbeat: Optional[str]


# =============================================================================
# DEPENDENCIES
# =============================================================================

async def get_job_queue() -> RedisJobQueue:
    """Dependency para obter fila de jobs"""
    return await get_queue()


# =============================================================================
# JOB ROUTES
# =============================================================================

@router.post("/jobs", response_model=JobResponse, status_code=201)
async def create_job(
    job: JobCreate,
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """
    Cria um novo job na fila

    O job sera processado por um worker disponivel.
    """
    result = await queue.enqueue(job.model_dump())
    return result


@router.get("/jobs", response_model=List[JobResponse])
async def list_jobs(
    status: Optional[str] = Query(None, description="Filtrar por status"),
    limit: int = Query(50, ge=1, le=100),
    offset: int = Query(0, ge=0),
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Lista jobs com filtros"""
    jobs = await queue.list_jobs(status=status, limit=limit, offset=offset)
    return jobs


@router.get("/jobs/{job_id}", response_model=JobResponse)
async def get_job(
    job_id: str,
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Busca job por ID"""
    job = await queue.get_job(job_id)
    if not job:
        raise HTTPException(status_code=404, detail="Job not found")
    return job


@router.delete("/jobs/{job_id}")
async def cancel_job(
    job_id: str,
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Cancela um job pendente"""
    success = await queue.cancel_job(job_id)
    if not success:
        raise HTTPException(
            status_code=400,
            detail="Job cannot be cancelled (not pending or not found)"
        )
    return {"success": True, "message": f"Job {job_id} cancelled"}


# =============================================================================
# QUEUE ROUTES
# =============================================================================

@router.get("/queue/stats", response_model=QueueStats)
async def queue_stats(
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Retorna estatisticas da fila"""
    return await queue.get_stats()


@router.get("/queue/peek", response_model=List[JobResponse])
async def queue_peek(
    limit: int = Query(10, ge=1, le=50),
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Lista jobs na fila sem remover"""
    return await queue.peek(limit=limit)


# =============================================================================
# WORKER ROUTES
# =============================================================================

@router.get("/workers", response_model=List[WorkerResponse])
async def list_workers(
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Lista todos os workers"""
    return await queue.get_workers()


@router.get("/workers/{worker_id}", response_model=WorkerResponse)
async def get_worker(
    worker_id: str,
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Busca worker por ID"""
    worker = await queue.get_worker(worker_id)
    if not worker:
        raise HTTPException(status_code=404, detail="Worker not found")
    return worker


@router.get("/workers/active", response_model=List[WorkerResponse])
async def list_active_workers(
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Lista workers ativos"""
    return await queue.get_active_workers()


# =============================================================================
# HEALTH ROUTES
# =============================================================================

@router.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "service": "factory-api",
        "version": "4.0.0",
        "timestamp": datetime.utcnow().isoformat()
    }


@router.get("/health/detailed")
async def detailed_health_check(
    queue: RedisJobQueue = Depends(get_job_queue)
):
    """Health check detalhado"""
    from factory.database.connection import check_db_health

    db_health = await check_db_health()
    queue_stats = await queue.get_stats()
    workers = await queue.get_active_workers()

    return {
        "status": "healthy",
        "timestamp": datetime.utcnow().isoformat(),
        "database": db_health.get("database"),
        "redis": db_health.get("redis"),
        "queue": {
            "pending": queue_stats.get("pending", 0),
            "running": queue_stats.get("running", 0)
        },
        "workers": {
            "active": len(workers),
            "total": len(await queue.get_workers())
        }
    }
