"""
Execution Replay & Debug API Routes
Issue #70: [Feature] Replay e Debug de Execucoes

API endpoints para:
- Listar execucoes
- Ver timeline de execucao
- Replay passo a passo
- Debug de falhas
- Comparar execucoes
"""
import json
from typing import Optional
from datetime import datetime

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel

from factory.database.connection import SessionLocal
from factory.database.models import ExecutionLog
from factory.core.execution_recorder import (
    ExecutionRecorder,
    load_execution_from_db,
    save_execution_to_db
)
from factory.core.execution_replayer import (
    ExecutionReplayer,
    ExecutionComparator,
    ReplaySpeed,
    analyze_failures,
    get_execution_patterns,
    export_execution_report
)


router = APIRouter(prefix="/api/executions", tags=["Execution Replay"])


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class ReplayRequest(BaseModel):
    """Request para iniciar replay"""
    speed: Optional[float] = 1.0  # 0.5, 1.0, 2.0


class RerunRequest(BaseModel):
    """Request para re-executar a partir de um step"""
    step_id: str
    model: Optional[str] = None


class CompareRequest(BaseModel):
    """Request para comparar execucoes"""
    execution1_id: str
    execution2_id: str


# =============================================================================
# API ENDPOINTS
# =============================================================================

@router.get("")
def list_executions(
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    task_id: Optional[str] = None,
    job_id: Optional[str] = None,
    status: Optional[str] = None,
    limit: int = Query(default=50, le=200),
    offset: int = 0
):
    """
    Lista execucoes com filtros

    Args:
        project_id: Filtrar por projeto
        story_id: Filtrar por story
        task_id: Filtrar por task
        job_id: Filtrar por job
        status: Filtrar por status (running, success, failed, cancelled)
        limit: Limite de resultados (max 200)
        offset: Offset para paginacao
    """
    db = SessionLocal()
    try:
        query = db.query(ExecutionLog)

        if project_id:
            query = query.filter(ExecutionLog.project_id == project_id)
        if story_id:
            query = query.filter(ExecutionLog.story_id == story_id)
        if task_id:
            query = query.filter(ExecutionLog.task_id == task_id)
        if job_id:
            query = query.filter(ExecutionLog.job_id == job_id)
        if status:
            query = query.filter(ExecutionLog.status == status)

        total = query.count()
        executions = query.order_by(ExecutionLog.created_at.desc()).offset(offset).limit(limit).all()

        return {
            "executions": [e.to_dict() for e in executions],
            "total": total,
            "limit": limit,
            "offset": offset
        }
    finally:
        db.close()


@router.get("/{execution_id}")
def get_execution(execution_id: str):
    """
    Busca detalhes de uma execucao

    Args:
        execution_id: ID da execucao (EXEC-XXXX)

    Returns:
        Dados completos da execucao incluindo steps
    """
    db = SessionLocal()
    try:
        execution = db.query(ExecutionLog).filter(
            ExecutionLog.execution_id == execution_id
        ).first()

        if not execution:
            raise HTTPException(404, f"Execution {execution_id} not found")

        return execution.to_dict()
    finally:
        db.close()


@router.get("/{execution_id}/timeline")
def get_execution_timeline(execution_id: str):
    """
    Retorna timeline da execucao para visualizacao

    Timeline visual:
    - Generate (30s)
    - Lint (5s) - 2 erros
    - Fix (15s)
    - Lint (3s) - OK
    - Test (45s) - 1 falha
    - Fix (20s)
    - Test (40s) - OK
    - Complete

    Args:
        execution_id: ID da execucao

    Returns:
        Lista de eventos na timeline com timestamps e status
    """
    recorder = load_execution_from_db(execution_id)
    if not recorder:
        raise HTTPException(404, f"Execution {execution_id} not found")

    return {
        "execution_id": execution_id,
        "timeline": recorder.get_timeline(),
        "info": {
            "status": recorder.status,
            "started_at": recorder.started_at.isoformat() if recorder.started_at else None,
            "ended_at": recorder.ended_at.isoformat() if recorder.ended_at else None,
            "duration_ms": recorder.duration_ms,
            "steps_count": len(recorder.steps),
            "files_created": recorder.files_created,
            "files_modified": recorder.files_modified,
            "total_tokens": recorder.total_tokens,
            "error_message": recorder.error_message
        }
    }


@router.get("/{execution_id}/steps/{step_id}")
def get_execution_step(execution_id: str, step_id: str):
    """
    Retorna detalhes de um step especifico

    Para cada step do autonomous loop:
    - Input enviado ao Claude
    - Prompt completo
    - Response recebida
    - Arquivos gerados
    - Tempo de execucao

    Args:
        execution_id: ID da execucao
        step_id: ID do step (STEP-XXX)

    Returns:
        Detalhes do step incluindo prompt, response, file changes, errors
    """
    recorder = load_execution_from_db(execution_id)
    if not recorder:
        raise HTTPException(404, f"Execution {execution_id} not found")

    step = recorder.get_step(step_id)
    if not step:
        raise HTTPException(404, f"Step {step_id} not found")

    return {
        "step_id": step.step_id,
        "step_number": step.step_number,
        "step_type": step.step_type,
        "name": step.name,
        "status": step.status,
        "started_at": step.started_at,
        "ended_at": step.ended_at,
        "duration_ms": step.duration_ms,
        "input_data": step.input_data,
        "output_data": step.output_data,
        "prompt": step.prompt,
        "response": step.response,
        "model": step.model,
        "tokens_input": step.tokens_input,
        "tokens_output": step.tokens_output,
        "file_changes": [
            fc.to_dict() if hasattr(fc, 'to_dict') else fc
            for fc in (step.file_changes or [])
        ],
        "error_message": step.error_message,
        "error_type": step.error_type,
        "stack_trace": step.stack_trace,
        "attempt": step.attempt,
        "can_replay": step.can_replay
    }


@router.post("/{execution_id}/replay")
async def replay_execution(execution_id: str, request: ReplayRequest = ReplayRequest()):
    """
    Inicia replay de uma execucao

    Replay Mode - Reproduzir execucao de job:
    - Timeline de eventos
    - Play/Pause/Step
    - Velocidade ajustavel (0.5x, 1x, 2x)

    Args:
        execution_id: ID da execucao
        request: Configuracao do replay (velocidade)

    Returns:
        Info do replayer e timeline para UI
    """
    replayer = ExecutionReplayer(
        execution_id=execution_id,
        speed=ReplaySpeed(request.speed) if request.speed in [0.5, 1.0, 2.0] else ReplaySpeed.NORMAL
    )

    if not await replayer.load():
        raise HTTPException(404, f"Execution {execution_id} not found")

    # Retornar info inicial
    return {
        "status": "ready",
        "execution_info": replayer.get_execution_info(),
        "timeline": replayer.get_timeline(),
        "speed": request.speed or 1.0,
        "controls": {
            "can_play": True,
            "can_pause": True,
            "can_step": True,
            "can_rerun": True
        }
    }


@router.post("/{execution_id}/rerun")
async def rerun_from_step(execution_id: str, request: RerunRequest):
    """
    Re-executa a partir de um step especifico

    Permite debugar falhas re-executando a partir de um ponto especifico.

    Args:
        execution_id: ID da execucao original
        request: Step de onde iniciar e modelo a usar

    Returns:
        Info sobre o rerun preparado
    """
    replayer = ExecutionReplayer(execution_id=execution_id)

    if not await replayer.load():
        raise HTTPException(404, f"Execution {execution_id} not found")

    # Verificar se step existe
    step_details = replayer.get_step_details(request.step_id)
    if not step_details:
        raise HTTPException(404, f"Step {request.step_id} not found")

    return {
        "status": "prepared",
        "message": f"Rerun prepared from step {request.step_id}",
        "original_execution": execution_id,
        "from_step": request.step_id,
        "model": request.model or replayer._execution_data.get("agent_model"),
        "steps_to_skip": step_details.get("step_number", 1) - 1,
        "steps_to_rerun": len(replayer._steps) - step_details.get("step_number", 1) + 1
    }


@router.post("/compare")
async def compare_executions(request: CompareRequest):
    """
    Compara duas execucoes

    Comparacao de Execucoes:
    - Diff entre duas execucoes
    - Por que uma funcionou e outra nao
    - Variaveis que mudaram

    Args:
        request: IDs das duas execucoes a comparar

    Returns:
        Resultado da comparacao incluindo diferencas
    """
    comparator = ExecutionComparator(
        execution1_id=request.execution1_id,
        execution2_id=request.execution2_id
    )

    if not await comparator.load():
        raise HTTPException(404, "One or both executions not found")

    result = comparator.compare()

    return {
        "execution1_id": result.execution1_id,
        "execution2_id": result.execution2_id,
        "same_status": result.same_status,
        "same_steps_count": result.same_steps_count,
        "steps_diff": result.steps_diff,
        "files_diff": result.files_diff,
        "duration_diff_ms": result.duration_diff_ms,
        "tokens_diff": result.tokens_diff,
        "summary": result.summary
    }


@router.get("/{execution_id}/analysis")
def analyze_execution(execution_id: str):
    """
    Analisa falhas de uma execucao

    Debug de Falhas:
    - Ponto exato da falha
    - Stack trace
    - Estado do contexto
    - Sugestoes de fix

    Args:
        execution_id: ID da execucao

    Returns:
        Analise das falhas com sugestoes
    """
    analysis = analyze_failures(execution_id)
    if "error" in analysis:
        raise HTTPException(404, analysis["error"])
    return analysis


@router.get("/{execution_id}/export")
def export_execution(execution_id: str, format: str = "json"):
    """
    Exporta relatorio de execucao

    Export de Logs:
    - JSON completo
    - Formato legivel (text)
    - Compartilhar para suporte

    Args:
        execution_id: ID da execucao
        format: Formato (json, text)

    Returns:
        Relatorio no formato especificado
    """
    report = export_execution_report(execution_id, format)

    if format == "json":
        return json.loads(report)
    else:
        return {"format": "text", "content": report}


# =============================================================================
# PROJECT-LEVEL ENDPOINTS
# =============================================================================

project_router = APIRouter(prefix="/api/projects", tags=["Project Executions"])


@project_router.get("/{project_id}/executions")
def get_project_executions(
    project_id: str,
    status: Optional[str] = None,
    limit: int = Query(default=50, le=200),
    offset: int = 0
):
    """
    Lista execucoes de um projeto

    Args:
        project_id: ID do projeto
        status: Filtrar por status
        limit: Limite de resultados
        offset: Offset para paginacao
    """
    db = SessionLocal()
    try:
        query = db.query(ExecutionLog).filter(ExecutionLog.project_id == project_id)

        if status:
            query = query.filter(ExecutionLog.status == status)

        total = query.count()
        executions = query.order_by(ExecutionLog.created_at.desc()).offset(offset).limit(limit).all()

        return {
            "project_id": project_id,
            "executions": [e.to_dict() for e in executions],
            "total": total,
            "limit": limit,
            "offset": offset
        }
    finally:
        db.close()


@project_router.get("/{project_id}/execution-patterns")
def get_patterns(project_id: str, limit: int = Query(default=100, le=500)):
    """
    Analisa padroes de execucao de um projeto

    Uso para Melhoria - Identificar patterns:
    - Steps que mais falham
    - Prompts que funcionam melhor
    - Otimizacoes possiveis

    Args:
        project_id: ID do projeto
        limit: Limite de execucoes a analisar

    Returns:
        Padroes identificados (falhas frequentes, tempos, etc)
    """
    patterns = get_execution_patterns(project_id, limit)
    if "error" in patterns:
        raise HTTPException(404, patterns["error"])
    return patterns


@project_router.get("/{project_id}/execution-stats")
def get_execution_stats(project_id: str):
    """
    Retorna estatisticas de execucao do projeto

    Args:
        project_id: ID do projeto

    Returns:
        Estatisticas agregadas
    """
    db = SessionLocal()
    try:
        from sqlalchemy import func

        total = db.query(ExecutionLog).filter(ExecutionLog.project_id == project_id).count()
        success = db.query(ExecutionLog).filter(
            ExecutionLog.project_id == project_id,
            ExecutionLog.status == "success"
        ).count()
        failed = db.query(ExecutionLog).filter(
            ExecutionLog.project_id == project_id,
            ExecutionLog.status == "failed"
        ).count()

        avg_duration = db.query(func.avg(ExecutionLog.duration_ms)).filter(
            ExecutionLog.project_id == project_id,
            ExecutionLog.status == "success"
        ).scalar() or 0

        avg_tokens = db.query(func.avg(ExecutionLog.total_tokens)).filter(
            ExecutionLog.project_id == project_id
        ).scalar() or 0

        return {
            "project_id": project_id,
            "total_executions": total,
            "success_count": success,
            "failed_count": failed,
            "success_rate": (success / total * 100) if total > 0 else 0,
            "avg_duration_ms": int(avg_duration),
            "avg_tokens": int(avg_tokens)
        }
    finally:
        db.close()


# =============================================================================
# HELPER FUNCTION TO REGISTER ROUTERS
# =============================================================================

def register_execution_routes(app):
    """
    Registra as rotas de execucao no app FastAPI

    Usage:
        from factory.api.execution_routes import register_execution_routes
        register_execution_routes(app)
    """
    app.include_router(router)
    app.include_router(project_router)
