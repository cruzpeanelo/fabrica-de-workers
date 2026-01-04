"""
Supervisor Routes - API para controle de agentes IA
Plataforma E v6.5
"""
from fastapi import APIRouter, HTTPException, Depends, Query
from typing import List, Optional
from pydantic import BaseModel, Field
import asyncio

from factory.core.agent_supervisor import (
    AgentSupervisor,
    AgentTask,
    AgentStatus,
    get_supervisor
)


router = APIRouter(prefix="/api/supervisor", tags=["Agent Supervisor"])


# =============================================================================
# SCHEMAS
# =============================================================================

class TaskCreate(BaseModel):
    """Schema para criar nova tarefa"""
    description: str = Field(..., min_length=5, max_length=200)
    prompt: str = Field(..., min_length=10)
    priority: int = Field(default=5, ge=1, le=10)
    issue_id: Optional[str] = None
    github_issue: Optional[int] = None
    timeout_seconds: int = Field(default=600, ge=60, le=3600)
    metadata: dict = Field(default_factory=dict)


class TaskResponse(BaseModel):
    """Resposta com dados da tarefa"""
    task_id: str
    agent_id: str
    description: str
    status: str
    priority: int
    started_at: Optional[str]
    completed_at: Optional[str]
    last_heartbeat: Optional[str]
    elapsed_seconds: Optional[float] = None
    health: Optional[str] = None
    error_message: str = ""
    files_modified: List[str] = []
    retries: int = 0
    max_retries: int = 3


class SupervisorStatus(BaseModel):
    """Status geral do supervisor"""
    timestamp: str
    active_agents: int
    max_concurrent: int
    monitoring_active: bool
    status_counts: dict
    total_tasks: int


class MetricsResponse(BaseModel):
    """Metricas agregadas"""
    total_tasks: int
    completed: int
    failed: int
    success_rate: float
    avg_duration_seconds: float
    overall_success_rate: float


class HeartbeatRequest(BaseModel):
    """Request de heartbeat"""
    checkpoint: Optional[dict] = None


class RetryResponse(BaseModel):
    """Resposta de retry"""
    success: bool
    original_task_id: str
    new_task_id: Optional[str] = None
    message: str


# =============================================================================
# DEPENDENCY
# =============================================================================

def get_agent_supervisor() -> AgentSupervisor:
    """Dependency para obter supervisor"""
    return get_supervisor()


# =============================================================================
# ENDPOINTS - STATUS
# =============================================================================

@router.get("/status", response_model=SupervisorStatus)
async def get_status(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Retorna status geral do supervisor de agentes

    Inclui contagem por status, agentes ativos e estado do monitoramento
    """
    return supervisor.get_status()


@router.get("/metrics", response_model=MetricsResponse)
async def get_metrics(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Retorna metricas agregadas de performance

    Taxa de sucesso, duracao media, totais
    """
    return supervisor.get_metrics()


# =============================================================================
# ENDPOINTS - TASKS
# =============================================================================

@router.get("/tasks", response_model=List[TaskResponse])
async def list_tasks(
    status: Optional[str] = Query(None, description="Filtrar por status"),
    limit: int = Query(50, ge=1, le=200),
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Lista todas as tarefas com filtros opcionais

    Status possiveis: pending, running, completed, failed, timeout, lost_context
    """
    tasks = supervisor.get_task_history(limit=limit)

    if status:
        tasks = [t for t in tasks if t.get("status") == status]

    return [TaskResponse(**t) for t in tasks]


@router.get("/tasks/running", response_model=List[TaskResponse])
async def list_running_tasks(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Lista tarefas em execucao com informacoes de saude

    Inclui tempo decorrido, timeout restante e indicador de saude
    """
    tasks = supervisor.get_running_tasks()
    return [TaskResponse(**t) for t in tasks]


@router.get("/tasks/failed", response_model=List[TaskResponse])
async def list_failed_tasks(
    include_recovered: bool = Query(False),
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Lista tarefas que falharam

    Opcionalmente inclui tarefas que ja foram recuperadas
    """
    tasks = supervisor.get_failed_tasks(include_recovered=include_recovered)
    return [TaskResponse(**t) for t in tasks]


@router.get("/tasks/{task_id}", response_model=TaskResponse)
async def get_task(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Busca tarefa especifica por ID
    """
    task = supervisor.get_task(task_id)
    if not task:
        raise HTTPException(status_code=404, detail=f"Task {task_id} not found")

    return TaskResponse(**task.to_dict())


@router.post("/tasks", response_model=TaskResponse)
async def create_task(
    data: TaskCreate,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Cria nova tarefa para execucao por agente

    A tarefa e criada com status 'pending' e pode ser iniciada manualmente
    """
    task = supervisor.create_task(
        description=data.description,
        prompt=data.prompt,
        priority=data.priority,
        issue_id=data.issue_id,
        github_issue=data.github_issue,
        timeout_seconds=data.timeout_seconds,
        metadata=data.metadata
    )

    return TaskResponse(**task.to_dict())


# =============================================================================
# ENDPOINTS - CONTROL
# =============================================================================

@router.post("/tasks/{task_id}/start")
async def start_task(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Inicia execucao de uma tarefa pendente

    Marca a tarefa como 'running' e registra timestamp
    """
    success = supervisor.start_task(task_id)
    if not success:
        raise HTTPException(status_code=404, detail=f"Task {task_id} not found")

    return {"success": True, "task_id": task_id, "status": "running"}


@router.post("/tasks/{task_id}/heartbeat")
async def send_heartbeat(
    task_id: str,
    data: HeartbeatRequest,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Envia heartbeat para tarefa em execucao

    Atualiza timestamp e opcionalmente salva checkpoint de contexto
    """
    success = supervisor.heartbeat(task_id, data.checkpoint)
    if not success:
        raise HTTPException(status_code=404, detail=f"Task {task_id} not found")

    return {"success": True, "task_id": task_id}


@router.post("/tasks/{task_id}/complete")
async def complete_task(
    task_id: str,
    output: str = "",
    files_modified: List[str] = [],
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Marca tarefa como concluida com sucesso
    """
    success = supervisor.complete_task(task_id, output, files_modified)
    if not success:
        raise HTTPException(status_code=404, detail=f"Task {task_id} not found")

    return {"success": True, "task_id": task_id, "status": "completed"}


@router.post("/tasks/{task_id}/fail")
async def fail_task(
    task_id: str,
    error: str = "",
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Marca tarefa como falha
    """
    success = supervisor.fail_task(task_id, error)
    if not success:
        raise HTTPException(status_code=404, detail=f"Task {task_id} not found")

    return {"success": True, "task_id": task_id, "status": "failed"}


# =============================================================================
# ENDPOINTS - RECOVERY
# =============================================================================

@router.post("/tasks/{task_id}/retry", response_model=RetryResponse)
async def retry_task(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Tenta re-executar tarefa falhada

    Cria nova tarefa baseada na original com contexto recuperado.
    Falha se max_retries foi atingido.
    """
    new_task = supervisor.retry_task(task_id)

    if new_task:
        return RetryResponse(
            success=True,
            original_task_id=task_id,
            new_task_id=new_task.task_id,
            message=f"Retry criado com sucesso"
        )
    else:
        return RetryResponse(
            success=False,
            original_task_id=task_id,
            message="Max retries atingido ou tarefa nao encontrada"
        )


@router.post("/tasks/{task_id}/force-restart", response_model=RetryResponse)
async def force_restart_task(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Forca reinicio imediato de uma tarefa

    Cancela tarefa atual e cria nova com maxima prioridade.
    Ignora limite de retries.
    """
    new_task = supervisor.force_restart(task_id)

    if new_task:
        return RetryResponse(
            success=True,
            original_task_id=task_id,
            new_task_id=new_task.task_id,
            message="Force restart executado"
        )
    else:
        raise HTTPException(
            status_code=404,
            detail=f"Task {task_id} not found"
        )


@router.post("/retry-all-failed")
async def retry_all_failed(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Re-executa todas as tarefas falhadas que ainda tem retries disponiveis
    """
    failed = supervisor.get_failed_tasks()
    results = {
        "retried": [],
        "skipped": [],
        "total_failed": len(failed)
    }

    for task_data in failed:
        task_id = task_data["task_id"]
        if task_data["retries"] < task_data["max_retries"]:
            new_task = supervisor.retry_task(task_id)
            if new_task:
                results["retried"].append({
                    "original": task_id,
                    "new": new_task.task_id
                })
            else:
                results["skipped"].append(task_id)
        else:
            results["skipped"].append(task_id)

    return results


# =============================================================================
# ENDPOINTS - MONITORING
# =============================================================================

@router.post("/monitoring/start")
async def start_monitoring(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Inicia monitoramento automatico de agentes

    Detecta timeouts e heartbeats parados automaticamente
    """
    await supervisor.start_monitoring()
    return {"success": True, "message": "Monitoring started"}


@router.post("/monitoring/stop")
async def stop_monitoring(
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Para monitoramento automatico
    """
    await supervisor.stop_monitoring()
    return {"success": True, "message": "Monitoring stopped"}


# =============================================================================
# ENDPOINTS - CONTEXT MANAGEMENT
# =============================================================================

class ContextReport(BaseModel):
    """Report de nivel de contexto"""
    context_remaining: float = Field(..., ge=0.0, le=1.0, description="Percentual de contexto restante")


class ContextAction(BaseModel):
    """Acao recomendada para o agente"""
    action: str
    save_state: bool = False
    save_checkpoint: bool = False
    message: str = ""


@router.post("/tasks/{task_id}/context", response_model=ContextAction)
async def report_context(
    task_id: str,
    report: ContextReport,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Agente reporta nivel de contexto

    O supervisor analisa e retorna instrucoes:
    - continue: Tudo ok
    - continue_with_caution: Contexto baixo, salvar checkpoints
    - compact_and_continue: Executar /compact e continuar
    - pause: Pausar execucao
    """
    result = supervisor.report_context_level(task_id, report.context_remaining)
    return ContextAction(**result)


@router.get("/tasks/{task_id}/resume-prompt")
async def get_resume_prompt(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Gera prompt para retomar tarefa que perdeu contexto

    Inclui estado salvo, checkpoint e instrucoes de continuacao
    """
    prompt = supervisor.get_resume_prompt(task_id)
    if not prompt:
        raise HTTPException(
            status_code=404,
            detail=f"No saved state found for task {task_id}"
        )

    return {"task_id": task_id, "resume_prompt": prompt}


@router.get("/tasks/{task_id}/state")
async def get_task_state(
    task_id: str,
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Recupera ultimo estado salvo de uma tarefa
    """
    state = supervisor.recover_state(task_id)
    if not state:
        raise HTTPException(
            status_code=404,
            detail=f"No saved state found for task {task_id}"
        )

    return state


@router.post("/context/adjust-concurrency")
async def adjust_concurrency(
    context_level: float = Query(..., ge=0.0, le=1.0),
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Ajusta numero de agentes baseado no contexto disponivel

    Reduz automaticamente quando contexto esta baixo
    """
    new_limit = supervisor.adjust_concurrency_for_context(context_level)
    return {
        "previous_limit": supervisor.max_concurrent_agents,
        "new_limit": new_limit,
        "context_level": context_level
    }


@router.post("/context/pause-low-priority")
async def pause_low_priority(
    count: int = Query(1, ge=1, le=10),
    supervisor: AgentSupervisor = Depends(get_agent_supervisor)
):
    """
    Pausa agentes de menor prioridade para liberar contexto

    Estados sao salvos automaticamente para recuperacao posterior
    """
    paused = supervisor.pause_lowest_priority_agents(count)
    return {
        "paused_count": len(paused),
        "paused_tasks": paused,
        "message": f"{len(paused)} agentes pausados - estados salvos"
    }


# =============================================================================
# WEBSOCKET - REAL-TIME UPDATES
# =============================================================================

from fastapi import WebSocket, WebSocketDisconnect
import json

connected_clients: List[WebSocket] = []


@router.websocket("/ws")
async def supervisor_websocket(websocket: WebSocket):
    """
    WebSocket para atualizacoes em tempo real do supervisor

    Eventos enviados:
    - task_started
    - task_completed
    - task_failed
    - task_timeout
    - task_lost_context
    - task_recovered
    """
    await websocket.accept()
    connected_clients.append(websocket)

    supervisor = get_supervisor()

    # Registrar callbacks para eventos
    async def send_event(event_type: str):
        async def handler(task):
            data = {
                "event": event_type,
                "task_id": task.task_id,
                "description": task.description,
                "status": task.status.value,
                "timestamp": task.completed_at or task.started_at
            }
            for client in connected_clients:
                try:
                    await client.send_json(data)
                except:
                    pass
        return handler

    # Registrar handlers
    for event in ["task_started", "task_completed", "task_failed",
                  "task_timeout", "task_lost_context", "task_recovered"]:
        supervisor.on(event, await send_event(event))

    try:
        while True:
            # Manter conexao aberta
            data = await websocket.receive_text()

            # Comandos via websocket
            if data == "status":
                status = supervisor.get_status()
                await websocket.send_json({"event": "status", "data": status})
            elif data == "running":
                running = supervisor.get_running_tasks()
                await websocket.send_json({"event": "running", "data": running})

    except WebSocketDisconnect:
        connected_clients.remove(websocket)
