"""
Kanban API Routes - Issue #237
WIP Limits e Politicas de Fluxo para Kanban

Endpoints para gerenciar:
- Limites de WIP por coluna
- Politicas (soft/hard)
- Metricas de fluxo (throughput, cycle time, lead time)
- Validacao de movimentacao
"""
from fastapi import APIRouter, HTTPException, Depends, Query
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime, date, timedelta
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, func, and_
import uuid

from factory.database.connection import get_async_db
from factory.database.models import (
    KanbanPolicy, FlowMetric, Story, StoryStatus, WipPolicyType
)

router = APIRouter(prefix="/api/kanban", tags=["Kanban"])


# =============================================================================
# SCHEMAS
# =============================================================================

class WipLimitsConfig(BaseModel):
    """Configuracao de WIP limits por coluna"""
    backlog: Optional[int] = Field(None, description="Limite para backlog (null = sem limite)")
    ready: Optional[int] = Field(None, description="Limite para ready")
    in_progress: Optional[int] = Field(5, description="Limite para in_progress")
    review: Optional[int] = Field(3, description="Limite para review")
    testing: Optional[int] = Field(5, description="Limite para testing")
    done: Optional[int] = Field(None, description="Limite para done (null = sem limite)")


class KanbanPolicyCreate(BaseModel):
    """Request para criar/atualizar politica Kanban"""
    project_id: str = Field(..., description="ID do projeto")
    wip_limits: WipLimitsConfig = Field(default_factory=WipLimitsConfig)
    wip_policy: str = Field("soft", description="Tipo: soft (aviso) ou hard (bloqueia)")
    alert_on_exceed: bool = Field(True, description="Alertar quando exceder")
    notify_team: bool = Field(False, description="Notificar time")
    track_cycle_time: bool = Field(True, description="Rastrear cycle time")
    track_lead_time: bool = Field(True, description="Rastrear lead time")


class KanbanPolicyResponse(BaseModel):
    """Response da politica Kanban"""
    policy_id: str
    project_id: str
    wip_limits: Dict[str, Optional[int]]
    wip_policy: str
    alert_on_exceed: bool
    notify_team: bool
    track_cycle_time: bool
    track_lead_time: bool
    created_at: Optional[str]
    updated_at: Optional[str]


class WipValidationResult(BaseModel):
    """Resultado da validacao de WIP"""
    allowed: bool
    warning: bool = False
    message: Optional[str] = None
    current_count: int
    limit: Optional[int]
    status: str


class ColumnWipStatus(BaseModel):
    """Status WIP de uma coluna"""
    status: str
    count: int
    limit: Optional[int]
    percentage: float
    state: str  # normal, warning, exceeded


class KanbanWipOverview(BaseModel):
    """Overview completo do WIP do Kanban"""
    project_id: str
    columns: List[ColumnWipStatus]
    total_wip: int
    policy_type: str


class FlowMetricsResponse(BaseModel):
    """Metricas de fluxo do projeto"""
    project_id: str
    period_start: str
    period_end: str
    throughput: float
    avg_cycle_time: float
    avg_lead_time: float
    wip_average: float
    daily_metrics: List[Dict[str, Any]]


# =============================================================================
# WIP POLICY ENDPOINTS
# =============================================================================

@router.post("/policies", response_model=KanbanPolicyResponse, status_code=201)
async def create_or_update_policy(
    policy_data: KanbanPolicyCreate,
    db: AsyncSession = Depends(get_async_db)
):
    """
    Cria ou atualiza politica de WIP para um projeto.
    Cada projeto pode ter apenas uma politica.
    """
    # Verificar se ja existe politica para o projeto
    result = await db.execute(
        select(KanbanPolicy).where(KanbanPolicy.project_id == policy_data.project_id)
    )
    existing = result.scalar_one_or_none()

    # Converter WipLimitsConfig para dict
    wip_limits_dict = {
        "backlog": policy_data.wip_limits.backlog,
        "ready": policy_data.wip_limits.ready,
        "in_progress": policy_data.wip_limits.in_progress,
        "review": policy_data.wip_limits.review,
        "testing": policy_data.wip_limits.testing,
        "done": policy_data.wip_limits.done
    }

    if existing:
        # Atualizar existente
        existing.wip_limits = wip_limits_dict
        existing.wip_policy = policy_data.wip_policy
        existing.alert_on_exceed = policy_data.alert_on_exceed
        existing.notify_team = policy_data.notify_team
        existing.track_cycle_time = policy_data.track_cycle_time
        existing.track_lead_time = policy_data.track_lead_time
        existing.updated_at = datetime.utcnow()
        policy = existing
    else:
        # Criar nova
        policy = KanbanPolicy(
            policy_id=f"POL-{uuid.uuid4().hex[:8].upper()}",
            project_id=policy_data.project_id,
            wip_limits=wip_limits_dict,
            wip_policy=policy_data.wip_policy,
            alert_on_exceed=policy_data.alert_on_exceed,
            notify_team=policy_data.notify_team,
            track_cycle_time=policy_data.track_cycle_time,
            track_lead_time=policy_data.track_lead_time
        )
        db.add(policy)

    await db.commit()
    await db.refresh(policy)

    return KanbanPolicyResponse(
        policy_id=policy.policy_id,
        project_id=policy.project_id,
        wip_limits=policy.wip_limits or {},
        wip_policy=policy.wip_policy,
        alert_on_exceed=policy.alert_on_exceed,
        notify_team=policy.notify_team,
        track_cycle_time=policy.track_cycle_time,
        track_lead_time=policy.track_lead_time,
        created_at=policy.created_at.isoformat() if policy.created_at else None,
        updated_at=policy.updated_at.isoformat() if policy.updated_at else None
    )


@router.get("/policies/{project_id}", response_model=KanbanPolicyResponse)
async def get_policy(
    project_id: str,
    db: AsyncSession = Depends(get_async_db)
):
    """Obtem politica de WIP do projeto"""
    result = await db.execute(
        select(KanbanPolicy).where(KanbanPolicy.project_id == project_id)
    )
    policy = result.scalar_one_or_none()

    if not policy:
        # Retornar politica padrao se nao existir
        return KanbanPolicyResponse(
            policy_id="default",
            project_id=project_id,
            wip_limits={
                "backlog": None,
                "ready": 10,
                "in_progress": 5,
                "review": 3,
                "testing": 5,
                "done": None
            },
            wip_policy="soft",
            alert_on_exceed=True,
            notify_team=False,
            track_cycle_time=True,
            track_lead_time=True,
            created_at=None,
            updated_at=None
        )

    return KanbanPolicyResponse(
        policy_id=policy.policy_id,
        project_id=policy.project_id,
        wip_limits=policy.wip_limits or {},
        wip_policy=policy.wip_policy,
        alert_on_exceed=policy.alert_on_exceed,
        notify_team=policy.notify_team,
        track_cycle_time=policy.track_cycle_time,
        track_lead_time=policy.track_lead_time,
        created_at=policy.created_at.isoformat() if policy.created_at else None,
        updated_at=policy.updated_at.isoformat() if policy.updated_at else None
    )


@router.delete("/policies/{project_id}", status_code=204)
async def delete_policy(
    project_id: str,
    db: AsyncSession = Depends(get_async_db)
):
    """Remove politica de WIP do projeto"""
    result = await db.execute(
        select(KanbanPolicy).where(KanbanPolicy.project_id == project_id)
    )
    policy = result.scalar_one_or_none()

    if policy:
        await db.delete(policy)
        await db.commit()


# =============================================================================
# WIP VALIDATION ENDPOINTS
# =============================================================================

@router.get("/validate-move/{project_id}", response_model=WipValidationResult)
async def validate_wip_move(
    project_id: str,
    to_status: str = Query(..., description="Status de destino"),
    story_id: Optional[str] = Query(None, description="ID da story sendo movida"),
    db: AsyncSession = Depends(get_async_db)
):
    """
    Valida se uma story pode ser movida para o status de destino.
    Verifica limite WIP da coluna de destino.
    """
    # Obter politica do projeto
    result = await db.execute(
        select(KanbanPolicy).where(KanbanPolicy.project_id == project_id)
    )
    policy = result.scalar_one_or_none()

    # Contar stories no status de destino
    count_query = select(func.count(Story.id)).where(
        and_(
            Story.project_id == project_id,
            Story.status == to_status,
            Story.is_deleted == False
        )
    )

    # Se estiver movendo uma story que ja esta no destino, nao contar
    if story_id:
        story_result = await db.execute(
            select(Story).where(Story.story_id == story_id)
        )
        story = story_result.scalar_one_or_none()
        if story and story.status == to_status:
            # Story ja esta no destino, permitir
            return WipValidationResult(
                allowed=True,
                warning=False,
                message=None,
                current_count=0,
                limit=None,
                status=to_status
            )

    count_result = await db.execute(count_query)
    current_count = count_result.scalar() or 0

    # Obter limite para o status
    limit = None
    if policy and policy.wip_limits:
        limit = policy.wip_limits.get(to_status)
    else:
        # Limites padrao
        default_limits = {
            "ready": 10,
            "in_progress": 5,
            "review": 3,
            "testing": 5
        }
        limit = default_limits.get(to_status)

    # Sem limite definido
    if limit is None:
        return WipValidationResult(
            allowed=True,
            warning=False,
            message=None,
            current_count=current_count,
            limit=None,
            status=to_status
        )

    # Verificar se excede limite
    is_hard = policy and policy.wip_policy == WipPolicyType.HARD.value

    if current_count >= limit:
        if is_hard:
            # Hard limit - bloqueia
            return WipValidationResult(
                allowed=False,
                warning=True,
                message=f"Coluna {to_status} esta no limite ({current_count}/{limit}). Finalize items antes de adicionar mais.",
                current_count=current_count,
                limit=limit,
                status=to_status
            )
        else:
            # Soft limit - apenas aviso
            return WipValidationResult(
                allowed=True,
                warning=True,
                message=f"WIP limit excedido em {to_status} ({current_count}/{limit}). Considere finalizar items existentes.",
                current_count=current_count,
                limit=limit,
                status=to_status
            )

    return WipValidationResult(
        allowed=True,
        warning=False,
        message=None,
        current_count=current_count,
        limit=limit,
        status=to_status
    )


@router.get("/wip-overview/{project_id}", response_model=KanbanWipOverview)
async def get_wip_overview(
    project_id: str,
    db: AsyncSession = Depends(get_async_db)
):
    """
    Retorna overview do WIP de todas as colunas do Kanban.
    Inclui contagem atual, limite e estado (normal/warning/exceeded).
    """
    # Obter politica
    policy_result = await db.execute(
        select(KanbanPolicy).where(KanbanPolicy.project_id == project_id)
    )
    policy = policy_result.scalar_one_or_none()

    # Limites (da politica ou padrao)
    wip_limits = {}
    policy_type = "soft"
    if policy:
        wip_limits = policy.wip_limits or {}
        policy_type = policy.wip_policy
    else:
        wip_limits = {
            "backlog": None,
            "ready": 10,
            "in_progress": 5,
            "review": 3,
            "testing": 5,
            "done": None
        }

    # Contar stories por status
    statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
    columns = []
    total_wip = 0

    for status in statuses:
        count_result = await db.execute(
            select(func.count(Story.id)).where(
                and_(
                    Story.project_id == project_id,
                    Story.status == status,
                    Story.is_deleted == False
                )
            )
        )
        count = count_result.scalar() or 0
        limit = wip_limits.get(status)

        # Calcular porcentagem e estado
        if limit and limit > 0:
            percentage = (count / limit) * 100
            if count > limit:
                state = "exceeded"
            elif count >= limit * 0.8:  # 80% ou mais
                state = "warning"
            else:
                state = "normal"
        else:
            percentage = 0
            state = "normal"

        columns.append(ColumnWipStatus(
            status=status,
            count=count,
            limit=limit,
            percentage=round(percentage, 1),
            state=state
        ))

        # Contar WIP (apenas in_progress, review, testing)
        if status in ["in_progress", "review", "testing"]:
            total_wip += count

    return KanbanWipOverview(
        project_id=project_id,
        columns=columns,
        total_wip=total_wip,
        policy_type=policy_type
    )


# =============================================================================
# FLOW METRICS ENDPOINTS
# =============================================================================

@router.get("/metrics/{project_id}", response_model=FlowMetricsResponse)
async def get_flow_metrics(
    project_id: str,
    days: int = Query(30, ge=7, le=90, description="Periodo em dias"),
    db: AsyncSession = Depends(get_async_db)
):
    """
    Retorna metricas de fluxo do projeto.
    Inclui throughput, cycle time, lead time e dados para CFD.
    """
    end_date = datetime.utcnow()
    start_date = end_date - timedelta(days=days)

    # Buscar metricas do periodo
    result = await db.execute(
        select(FlowMetric).where(
            and_(
                FlowMetric.project_id == project_id,
                FlowMetric.date >= start_date,
                FlowMetric.date <= end_date
            )
        ).order_by(FlowMetric.date)
    )
    metrics = result.scalars().all()

    # Se nao houver metricas, calcular do estado atual
    if not metrics:
        # Calcular metricas atuais
        throughput = await _calculate_throughput(db, project_id, start_date, end_date)
        cycle_time = await _calculate_avg_cycle_time(db, project_id, start_date, end_date)
        lead_time = await _calculate_avg_lead_time(db, project_id, start_date, end_date)
        wip_avg = await _calculate_wip_average(db, project_id)

        return FlowMetricsResponse(
            project_id=project_id,
            period_start=start_date.isoformat(),
            period_end=end_date.isoformat(),
            throughput=throughput,
            avg_cycle_time=cycle_time,
            avg_lead_time=lead_time,
            wip_average=wip_avg,
            daily_metrics=[]
        )

    # Agregar metricas
    total_throughput = sum(m.throughput or 0 for m in metrics)
    avg_cycle = sum(m.avg_cycle_time or 0 for m in metrics) / len(metrics) if metrics else 0
    avg_lead = sum(m.avg_lead_time or 0 for m in metrics) / len(metrics) if metrics else 0
    avg_wip = sum(m.wip_average or 0 for m in metrics) / len(metrics) if metrics else 0

    daily_data = [m.to_dict() for m in metrics]

    return FlowMetricsResponse(
        project_id=project_id,
        period_start=start_date.isoformat(),
        period_end=end_date.isoformat(),
        throughput=round(total_throughput, 2),
        avg_cycle_time=round(avg_cycle, 2),
        avg_lead_time=round(avg_lead, 2),
        wip_average=round(avg_wip, 2),
        daily_metrics=daily_data
    )


@router.post("/metrics/{project_id}/snapshot", status_code=201)
async def create_daily_snapshot(
    project_id: str,
    db: AsyncSession = Depends(get_async_db)
):
    """
    Cria snapshot diario das metricas.
    Deve ser chamado por um job scheduled (cron).
    """
    today = datetime.utcnow().replace(hour=0, minute=0, second=0, microsecond=0)

    # Verificar se ja existe snapshot de hoje
    existing = await db.execute(
        select(FlowMetric).where(
            and_(
                FlowMetric.project_id == project_id,
                FlowMetric.date == today
            )
        )
    )
    if existing.scalar_one_or_none():
        raise HTTPException(status_code=409, detail="Snapshot de hoje ja existe")

    # Contar stories por status
    statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
    counts = {}

    for status in statuses:
        result = await db.execute(
            select(func.count(Story.id)).where(
                and_(
                    Story.project_id == project_id,
                    Story.status == status,
                    Story.is_deleted == False
                )
            )
        )
        counts[status] = result.scalar() or 0

    # Calcular metricas
    yesterday = today - timedelta(days=1)
    throughput = await _calculate_throughput(db, project_id, yesterday, today)
    cycle_time = await _calculate_avg_cycle_time(db, project_id, yesterday, today)
    lead_time = await _calculate_avg_lead_time(db, project_id, yesterday, today)
    wip_avg = counts["in_progress"] + counts["review"] + counts["testing"]

    # Criar snapshot
    metric = FlowMetric(
        metric_id=f"MET-{uuid.uuid4().hex[:8].upper()}",
        project_id=project_id,
        date=today,
        backlog_count=counts["backlog"],
        ready_count=counts["ready"],
        in_progress_count=counts["in_progress"],
        review_count=counts["review"],
        testing_count=counts["testing"],
        done_count=counts["done"],
        throughput=throughput,
        avg_cycle_time=cycle_time,
        avg_lead_time=lead_time,
        wip_average=wip_avg
    )

    db.add(metric)
    await db.commit()

    return {"message": "Snapshot criado", "metric_id": metric.metric_id}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

async def _calculate_throughput(
    db: AsyncSession,
    project_id: str,
    start_date: datetime,
    end_date: datetime
) -> float:
    """Calcula throughput (stories completadas no periodo)"""
    result = await db.execute(
        select(func.count(Story.id)).where(
            and_(
                Story.project_id == project_id,
                Story.status == "done",
                Story.completed_at >= start_date,
                Story.completed_at <= end_date,
                Story.is_deleted == False
            )
        )
    )
    return float(result.scalar() or 0)


async def _calculate_avg_cycle_time(
    db: AsyncSession,
    project_id: str,
    start_date: datetime,
    end_date: datetime
) -> float:
    """Calcula cycle time medio (started_at -> completed_at)"""
    result = await db.execute(
        select(Story).where(
            and_(
                Story.project_id == project_id,
                Story.status == "done",
                Story.started_at.isnot(None),
                Story.completed_at.isnot(None),
                Story.completed_at >= start_date,
                Story.completed_at <= end_date,
                Story.is_deleted == False
            )
        )
    )
    stories = result.scalars().all()

    if not stories:
        return 0.0

    total_days = 0.0
    for story in stories:
        delta = story.completed_at - story.started_at
        total_days += delta.total_seconds() / 86400  # Converter para dias

    return round(total_days / len(stories), 2)


async def _calculate_avg_lead_time(
    db: AsyncSession,
    project_id: str,
    start_date: datetime,
    end_date: datetime
) -> float:
    """Calcula lead time medio (created_at -> completed_at)"""
    result = await db.execute(
        select(Story).where(
            and_(
                Story.project_id == project_id,
                Story.status == "done",
                Story.completed_at.isnot(None),
                Story.completed_at >= start_date,
                Story.completed_at <= end_date,
                Story.is_deleted == False
            )
        )
    )
    stories = result.scalars().all()

    if not stories:
        return 0.0

    total_days = 0.0
    for story in stories:
        delta = story.completed_at - story.created_at
        total_days += delta.total_seconds() / 86400

    return round(total_days / len(stories), 2)


async def _calculate_wip_average(
    db: AsyncSession,
    project_id: str
) -> float:
    """Calcula WIP atual (in_progress + review + testing)"""
    result = await db.execute(
        select(func.count(Story.id)).where(
            and_(
                Story.project_id == project_id,
                Story.status.in_(["in_progress", "review", "testing"]),
                Story.is_deleted == False
            )
        )
    )
    return float(result.scalar() or 0)
