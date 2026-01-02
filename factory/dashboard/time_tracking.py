# -*- coding: utf-8 -*-
"""
Time Tracking Module (Issue #224)
==================================
Sistema de time tracking inline para Tasks.

Funcionalidades:
- Timer inline em tasks com start/pause/stop
- Registro de tempo com descricao do trabalho
- Dashboard de tempo por sprint/story/usuario
- Comparacao estimado vs real
- Export CSV para relatorios
"""

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import HTMLResponse
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime
from uuid import uuid4
import logging

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/time-tracking", tags=["Time Tracking"])


# =============================================================================
# MODELS
# =============================================================================

class TimeEntryCreate(BaseModel):
    """Modelo para criar uma entrada de tempo."""
    task_id: str = Field(..., description="ID da task")
    story_id: Optional[str] = Field(None, description="ID da story")
    user_id: str = Field(..., description="ID do usuario")
    user_name: str = Field(..., description="Nome do usuario")
    description: Optional[str] = Field(None, description="Descricao do trabalho")
    is_billable: bool = Field(True, description="Horas faturáveis")


class TimeEntryStop(BaseModel):
    """Modelo para parar o timer."""
    description: Optional[str] = Field(None, description="Descricao do trabalho realizado")


class TimeEntryUpdate(BaseModel):
    """Modelo para atualizar uma entrada de tempo."""
    description: Optional[str] = Field(None)
    is_billable: Optional[bool] = Field(None)
    duration_seconds: Optional[int] = Field(None, description="Duracao manual em segundos")


class TimeEntryResponse(BaseModel):
    """Resposta de uma entrada de tempo."""
    entry_id: str
    task_id: str
    story_id: Optional[str]
    user_id: str
    user_name: str
    started_at: str
    ended_at: Optional[str]
    duration_seconds: int
    duration_formatted: str
    description: Optional[str]
    is_running: bool
    is_billable: bool


class TimeReport(BaseModel):
    """Relatorio de tempo."""
    total_seconds: int
    total_formatted: str
    entries: List[TimeEntryResponse]
    by_user: Dict[str, int]
    by_story: Dict[str, int]


# =============================================================================
# IN-MEMORY STORAGE (Dev Mode)
# =============================================================================

_time_entries: Dict[str, Dict] = {}
_active_timers: Dict[str, str] = {}  # user_id -> entry_id


def _generate_entry_id() -> str:
    """Gera ID unico para entrada de tempo."""
    return f"TE-{uuid4().hex[:8].upper()}"


def _format_duration(seconds: int) -> str:
    """Formata duracao em formato legivel."""
    hours = seconds // 3600
    minutes = (seconds % 3600) // 60
    secs = seconds % 60
    return f"{hours:02d}:{minutes:02d}:{secs:02d}"


def _calculate_duration(started_at: datetime, ended_at: Optional[datetime] = None) -> int:
    """Calcula duracao em segundos."""
    end = ended_at or datetime.utcnow()
    return int((end - started_at).total_seconds())


# =============================================================================
# API ENDPOINTS
# =============================================================================

@router.post("/start", response_model=TimeEntryResponse)
async def start_timer(entry: TimeEntryCreate):
    """
    Inicia o timer para uma task.

    Se o usuario ja tem um timer ativo, retorna erro.
    """
    # Verificar se usuario ja tem timer ativo
    if entry.user_id in _active_timers:
        active_id = _active_timers[entry.user_id]
        raise HTTPException(
            status_code=400,
            detail=f"Usuario ja tem timer ativo: {active_id}. Pare-o antes de iniciar outro."
        )

    entry_id = _generate_entry_id()
    now = datetime.utcnow()

    time_entry = {
        "entry_id": entry_id,
        "task_id": entry.task_id,
        "story_id": entry.story_id,
        "user_id": entry.user_id,
        "user_name": entry.user_name,
        "started_at": now,
        "ended_at": None,
        "duration_seconds": 0,
        "description": entry.description,
        "is_running": True,
        "is_billable": entry.is_billable,
        "created_at": now
    }

    _time_entries[entry_id] = time_entry
    _active_timers[entry.user_id] = entry_id

    logger.info(f"[TimeTracking] Timer started: {entry_id} for task {entry.task_id} by {entry.user_name}")

    return TimeEntryResponse(
        entry_id=entry_id,
        task_id=entry.task_id,
        story_id=entry.story_id,
        user_id=entry.user_id,
        user_name=entry.user_name,
        started_at=now.isoformat(),
        ended_at=None,
        duration_seconds=0,
        duration_formatted="00:00:00",
        description=entry.description,
        is_running=True,
        is_billable=entry.is_billable
    )


@router.post("/stop/{entry_id}", response_model=TimeEntryResponse)
async def stop_timer(entry_id: str, stop_data: TimeEntryStop):
    """
    Para o timer e registra o tempo.
    """
    if entry_id not in _time_entries:
        raise HTTPException(status_code=404, detail="Entrada de tempo nao encontrada")

    entry = _time_entries[entry_id]

    if not entry["is_running"]:
        raise HTTPException(status_code=400, detail="Timer ja esta parado")

    now = datetime.utcnow()
    duration = _calculate_duration(entry["started_at"], now)

    entry["ended_at"] = now
    entry["duration_seconds"] = duration
    entry["is_running"] = False
    if stop_data.description:
        entry["description"] = stop_data.description

    # Remover do timer ativo
    if entry["user_id"] in _active_timers:
        del _active_timers[entry["user_id"]]

    logger.info(f"[TimeTracking] Timer stopped: {entry_id} - {_format_duration(duration)}")

    return TimeEntryResponse(
        entry_id=entry_id,
        task_id=entry["task_id"],
        story_id=entry["story_id"],
        user_id=entry["user_id"],
        user_name=entry["user_name"],
        started_at=entry["started_at"].isoformat(),
        ended_at=now.isoformat(),
        duration_seconds=duration,
        duration_formatted=_format_duration(duration),
        description=entry["description"],
        is_running=False,
        is_billable=entry["is_billable"]
    )


@router.get("/active/{user_id}", response_model=Optional[TimeEntryResponse])
async def get_active_timer(user_id: str):
    """
    Retorna o timer ativo do usuario, se houver.
    """
    if user_id not in _active_timers:
        return None

    entry_id = _active_timers[user_id]
    entry = _time_entries.get(entry_id)

    if not entry or not entry["is_running"]:
        # Limpar timer invalido
        del _active_timers[user_id]
        return None

    # Calcular duracao atual
    duration = _calculate_duration(entry["started_at"])

    return TimeEntryResponse(
        entry_id=entry_id,
        task_id=entry["task_id"],
        story_id=entry["story_id"],
        user_id=entry["user_id"],
        user_name=entry["user_name"],
        started_at=entry["started_at"].isoformat(),
        ended_at=None,
        duration_seconds=duration,
        duration_formatted=_format_duration(duration),
        description=entry["description"],
        is_running=True,
        is_billable=entry["is_billable"]
    )


@router.get("/task/{task_id}", response_model=TimeReport)
async def get_task_time_entries(task_id: str):
    """
    Retorna todas as entradas de tempo de uma task.
    """
    entries = [e for e in _time_entries.values() if e["task_id"] == task_id]

    total_seconds = sum(
        _calculate_duration(e["started_at"], e["ended_at"]) if e["is_running"]
        else e["duration_seconds"]
        for e in entries
    )

    # Agrupar por usuario
    by_user: Dict[str, int] = {}
    for e in entries:
        user = e["user_name"]
        duration = e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])
        by_user[user] = by_user.get(user, 0) + duration

    response_entries = [
        TimeEntryResponse(
            entry_id=e["entry_id"],
            task_id=e["task_id"],
            story_id=e["story_id"],
            user_id=e["user_id"],
            user_name=e["user_name"],
            started_at=e["started_at"].isoformat(),
            ended_at=e["ended_at"].isoformat() if e["ended_at"] else None,
            duration_seconds=e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"]),
            duration_formatted=_format_duration(e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])),
            description=e["description"],
            is_running=e["is_running"],
            is_billable=e["is_billable"]
        )
        for e in sorted(entries, key=lambda x: x["started_at"], reverse=True)
    ]

    return TimeReport(
        total_seconds=total_seconds,
        total_formatted=_format_duration(total_seconds),
        entries=response_entries,
        by_user=by_user,
        by_story={}
    )


@router.get("/story/{story_id}", response_model=TimeReport)
async def get_story_time_entries(story_id: str):
    """
    Retorna todas as entradas de tempo de uma story.
    """
    entries = [e for e in _time_entries.values() if e["story_id"] == story_id]

    total_seconds = sum(
        _calculate_duration(e["started_at"], e["ended_at"]) if e["is_running"]
        else e["duration_seconds"]
        for e in entries
    )

    # Agrupar por usuario
    by_user: Dict[str, int] = {}
    for e in entries:
        user = e["user_name"]
        duration = e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])
        by_user[user] = by_user.get(user, 0) + duration

    # Agrupar por task
    by_task: Dict[str, int] = {}
    for e in entries:
        task = e["task_id"]
        duration = e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])
        by_task[task] = by_task.get(task, 0) + duration

    response_entries = [
        TimeEntryResponse(
            entry_id=e["entry_id"],
            task_id=e["task_id"],
            story_id=e["story_id"],
            user_id=e["user_id"],
            user_name=e["user_name"],
            started_at=e["started_at"].isoformat(),
            ended_at=e["ended_at"].isoformat() if e["ended_at"] else None,
            duration_seconds=e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"]),
            duration_formatted=_format_duration(e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])),
            description=e["description"],
            is_running=e["is_running"],
            is_billable=e["is_billable"]
        )
        for e in sorted(entries, key=lambda x: x["started_at"], reverse=True)
    ]

    return TimeReport(
        total_seconds=total_seconds,
        total_formatted=_format_duration(total_seconds),
        entries=response_entries,
        by_user=by_user,
        by_story=by_task  # Using by_story to hold by_task data for stories
    )


@router.get("/user/{user_id}", response_model=TimeReport)
async def get_user_time_entries(
    user_id: str,
    start_date: Optional[str] = Query(None, description="Data inicio (YYYY-MM-DD)"),
    end_date: Optional[str] = Query(None, description="Data fim (YYYY-MM-DD)")
):
    """
    Retorna todas as entradas de tempo de um usuario.
    """
    entries = [e for e in _time_entries.values() if e["user_id"] == user_id]

    # Filtrar por data se especificado
    if start_date:
        start = datetime.fromisoformat(start_date)
        entries = [e for e in entries if e["started_at"] >= start]
    if end_date:
        end = datetime.fromisoformat(end_date)
        entries = [e for e in entries if e["started_at"] <= end]

    total_seconds = sum(
        _calculate_duration(e["started_at"], e["ended_at"]) if e["is_running"]
        else e["duration_seconds"]
        for e in entries
    )

    # Agrupar por story
    by_story: Dict[str, int] = {}
    for e in entries:
        story = e["story_id"] or "sem-story"
        duration = e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])
        by_story[story] = by_story.get(story, 0) + duration

    response_entries = [
        TimeEntryResponse(
            entry_id=e["entry_id"],
            task_id=e["task_id"],
            story_id=e["story_id"],
            user_id=e["user_id"],
            user_name=e["user_name"],
            started_at=e["started_at"].isoformat(),
            ended_at=e["ended_at"].isoformat() if e["ended_at"] else None,
            duration_seconds=e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"]),
            duration_formatted=_format_duration(e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])),
            description=e["description"],
            is_running=e["is_running"],
            is_billable=e["is_billable"]
        )
        for e in sorted(entries, key=lambda x: x["started_at"], reverse=True)
    ]

    return TimeReport(
        total_seconds=total_seconds,
        total_formatted=_format_duration(total_seconds),
        entries=response_entries,
        by_user={},
        by_story=by_story
    )


@router.put("/{entry_id}", response_model=TimeEntryResponse)
async def update_time_entry(entry_id: str, update: TimeEntryUpdate):
    """
    Atualiza uma entrada de tempo.
    """
    if entry_id not in _time_entries:
        raise HTTPException(status_code=404, detail="Entrada de tempo nao encontrada")

    entry = _time_entries[entry_id]

    if update.description is not None:
        entry["description"] = update.description
    if update.is_billable is not None:
        entry["is_billable"] = update.is_billable
    if update.duration_seconds is not None and not entry["is_running"]:
        entry["duration_seconds"] = update.duration_seconds

    duration = entry["duration_seconds"] if not entry["is_running"] else _calculate_duration(entry["started_at"])

    return TimeEntryResponse(
        entry_id=entry_id,
        task_id=entry["task_id"],
        story_id=entry["story_id"],
        user_id=entry["user_id"],
        user_name=entry["user_name"],
        started_at=entry["started_at"].isoformat(),
        ended_at=entry["ended_at"].isoformat() if entry["ended_at"] else None,
        duration_seconds=duration,
        duration_formatted=_format_duration(duration),
        description=entry["description"],
        is_running=entry["is_running"],
        is_billable=entry["is_billable"]
    )


@router.delete("/{entry_id}")
async def delete_time_entry(entry_id: str):
    """
    Remove uma entrada de tempo.
    """
    if entry_id not in _time_entries:
        raise HTTPException(status_code=404, detail="Entrada de tempo nao encontrada")

    entry = _time_entries[entry_id]

    # Se estiver rodando, remover do timer ativo
    if entry["is_running"] and entry["user_id"] in _active_timers:
        del _active_timers[entry["user_id"]]

    del _time_entries[entry_id]

    logger.info(f"[TimeTracking] Entry deleted: {entry_id}")

    return {"message": "Entrada de tempo removida", "entry_id": entry_id}


@router.get("/export/csv")
async def export_time_entries_csv(
    start_date: Optional[str] = Query(None, description="Data inicio (YYYY-MM-DD)"),
    end_date: Optional[str] = Query(None, description="Data fim (YYYY-MM-DD)"),
    user_id: Optional[str] = Query(None, description="Filtrar por usuario")
):
    """
    Exporta entradas de tempo para CSV.
    """
    entries = list(_time_entries.values())

    # Filtros
    if user_id:
        entries = [e for e in entries if e["user_id"] == user_id]
    if start_date:
        start = datetime.fromisoformat(start_date)
        entries = [e for e in entries if e["started_at"] >= start]
    if end_date:
        end = datetime.fromisoformat(end_date)
        entries = [e for e in entries if e["started_at"] <= end]

    # Gerar CSV
    csv_lines = ["entry_id,task_id,story_id,user_name,started_at,ended_at,duration_seconds,duration_formatted,description,is_billable"]

    for e in sorted(entries, key=lambda x: x["started_at"]):
        duration = e["duration_seconds"] if not e["is_running"] else _calculate_duration(e["started_at"])
        line = ",".join([
            e["entry_id"],
            e["task_id"],
            e["story_id"] or "",
            f'"{e["user_name"]}"',
            e["started_at"].isoformat(),
            e["ended_at"].isoformat() if e["ended_at"] else "",
            str(duration),
            _format_duration(duration),
            f'"{e["description"] or ""}"',
            "Sim" if e["is_billable"] else "Nao"
        ])
        csv_lines.append(line)

    csv_content = "\n".join(csv_lines)

    return HTMLResponse(
        content=csv_content,
        media_type="text/csv",
        headers={"Content-Disposition": f"attachment; filename=time_entries_{datetime.now().strftime('%Y%m%d')}.csv"}
    )


# =============================================================================
# REGISTRO
# =============================================================================

def register_time_tracking(app):
    """Registra rotas de time tracking no app."""
    app.include_router(router)
    logger.info("[TimeTracking] Routes registered: /api/time-tracking/*")
