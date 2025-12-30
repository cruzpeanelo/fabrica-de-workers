# -*- coding: utf-8 -*-
"""
Calendar Integration Routes (Issue #264)
=========================================
Endpoints para gerenciar integracoes com Google Calendar e Outlook.

Endpoints:
- GET    /api/integrations/calendar/status         - Status de todos os calendarios
- POST   /api/integrations/calendar/connect        - Conecta a um calendario
- POST   /api/integrations/calendar/disconnect     - Desconecta de um calendario
- POST   /api/integrations/calendar/sync/sprint    - Sincroniza sprint
- POST   /api/integrations/calendar/sync/deadline  - Sincroniza deadline
- DELETE /api/integrations/calendar/event          - Remove evento sincronizado
- GET    /api/integrations/calendar/events         - Lista eventos sincronizados

Configuracao:
- Google Calendar: GOOGLE_CALENDAR_CREDENTIALS, GOOGLE_CALENDAR_TOKEN, GOOGLE_CALENDAR_ID
- Outlook: AZURE_TENANT_ID, AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, OUTLOOK_CALENDAR_USER
"""

import logging
from datetime import datetime
from typing import List, Optional

from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel, Field

from .google_calendar import GoogleCalendarConfig, get_google_calendar_client
from .outlook_calendar import OutlookCalendarConfig, get_outlook_calendar_client
from .calendar_sync import (
    CalendarSyncService,
    CalendarProvider,
    CalendarEventType,
    get_calendar_sync_service
)

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/integrations/calendar", tags=["Calendar Integration"])


# =============================================================================
# REQUEST/RESPONSE MODELS
# =============================================================================

class GoogleCalendarConnectRequest(BaseModel):
    """Request para conectar ao Google Calendar"""
    credentials_path: str = Field(..., description="Path para credentials.json")
    token_path: str = Field(default="token.json", description="Path para token.json")
    calendar_id: str = Field(default="primary", description="ID do calendario")


class OutlookCalendarConnectRequest(BaseModel):
    """Request para conectar ao Outlook Calendar"""
    tenant_id: str = Field(..., description="Azure AD Tenant ID")
    client_id: str = Field(..., description="Application (client) ID")
    client_secret: str = Field(..., description="Client secret")
    user_email: str = Field(..., description="Email do usuario do calendario")


class SprintSyncRequest(BaseModel):
    """Request para sincronizar sprint"""
    sprint_id: str = Field(..., description="ID do sprint")
    name: str = Field(..., description="Nome do sprint")
    start_date: datetime = Field(..., description="Data de inicio")
    end_date: datetime = Field(..., description="Data de fim")
    goal: str = Field(default="", description="Objetivo do sprint")
    attendees: List[str] = Field(default=[], description="Lista de emails dos participantes")
    provider: str = Field(default="all", description="Provedor: google, outlook, all")


class DeadlineSyncRequest(BaseModel):
    """Request para sincronizar deadline"""
    story_id: str = Field(..., description="ID da story")
    title: str = Field(..., description="Titulo da story")
    deadline: datetime = Field(..., description="Data de deadline")
    description: str = Field(default="", description="Descricao")
    priority: str = Field(default="medium", description="Prioridade: low, medium, high, urgent")
    provider: str = Field(default="all", description="Provedor: google, outlook, all")


class RemoveEventRequest(BaseModel):
    """Request para remover evento"""
    external_id: str = Field(..., description="ID externo (sprint_id ou story_id)")
    provider: str = Field(default="all", description="Provedor: google, outlook, all")


class CalendarStatusResponse(BaseModel):
    """Response de status do calendario"""
    provider: str
    status: str
    connected: bool
    enabled: bool
    last_error: Optional[str] = None
    details: dict = {}


class SyncResultResponse(BaseModel):
    """Response de resultado de sincronizacao"""
    success: bool
    items_synced: int
    items_failed: int
    errors: List[str] = []
    warnings: List[str] = []


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _get_provider_enum(provider: str) -> CalendarProvider:
    """Converte string para CalendarProvider enum"""
    provider_lower = provider.lower()
    if provider_lower == "google":
        return CalendarProvider.GOOGLE
    elif provider_lower == "outlook":
        return CalendarProvider.OUTLOOK
    return CalendarProvider.ALL


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_calendar_status():
    """
    Retorna o status de todas as integracoes de calendario.

    Returns:
        Status de Google Calendar e Outlook Calendar
    """
    sync_service = get_calendar_sync_service()
    return {
        "calendars": sync_service.get_status(),
        "timestamp": datetime.utcnow().isoformat()
    }


@router.get("/google/status", response_model=CalendarStatusResponse)
async def get_google_calendar_status():
    """
    Retorna o status da integracao com Google Calendar.
    """
    client = get_google_calendar_client()
    status = client.get_status()

    return CalendarStatusResponse(
        provider="google_calendar",
        status=status.get("status", "disconnected"),
        connected=status.get("connected", False),
        enabled=status.get("enabled", False),
        last_error=status.get("last_error"),
        details={"calendar_id": status.get("calendar_id")}
    )


@router.get("/outlook/status", response_model=CalendarStatusResponse)
async def get_outlook_calendar_status():
    """
    Retorna o status da integracao com Outlook Calendar.
    """
    client = get_outlook_calendar_client()
    status = client.get_status()

    return CalendarStatusResponse(
        provider="outlook_calendar",
        status=status.get("status", "disconnected"),
        connected=status.get("connected", False),
        enabled=status.get("enabled", False),
        last_error=status.get("last_error"),
        details={
            "user_email": status.get("user_email"),
            "token_expires": status.get("token_expires")
        }
    )


@router.post("/google/connect")
async def connect_google_calendar(request: GoogleCalendarConnectRequest):
    """
    Conecta ao Google Calendar.

    Requer arquivo credentials.json do Google Cloud Console.
    """
    client = get_google_calendar_client()

    # Atualiza configuracao
    client.config.credentials_path = request.credentials_path
    client.config.token_path = request.token_path
    client.config.calendar_id = request.calendar_id
    client.config.enabled = True

    success = await client.connect()

    if success:
        return {
            "success": True,
            "message": "Conectado ao Google Calendar com sucesso",
            "status": client.get_status()
        }
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "success": False,
                "message": f"Falha ao conectar: {client.last_error}",
                "error": client.last_error
            }
        )


@router.post("/outlook/connect")
async def connect_outlook_calendar(request: OutlookCalendarConnectRequest):
    """
    Conecta ao Outlook Calendar via Microsoft Graph.

    Requer credenciais do Azure AD (tenant_id, client_id, client_secret).
    """
    client = get_outlook_calendar_client()

    # Atualiza configuracao
    client.config.tenant_id = request.tenant_id
    client.config.client_id = request.client_id
    client.config.client_secret = request.client_secret
    client.config.user_email = request.user_email
    client.config.enabled = True

    success = await client.connect()

    if success:
        return {
            "success": True,
            "message": "Conectado ao Outlook Calendar com sucesso",
            "status": client.get_status()
        }
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "success": False,
                "message": f"Falha ao conectar: {client.last_error}",
                "error": client.last_error
            }
        )


@router.post("/google/disconnect")
async def disconnect_google_calendar():
    """
    Desconecta do Google Calendar.
    """
    client = get_google_calendar_client()
    await client.disconnect()
    client.config.enabled = False

    return {
        "success": True,
        "message": "Desconectado do Google Calendar"
    }


@router.post("/outlook/disconnect")
async def disconnect_outlook_calendar():
    """
    Desconecta do Outlook Calendar.
    """
    client = get_outlook_calendar_client()
    await client.disconnect()
    client.config.enabled = False

    return {
        "success": True,
        "message": "Desconectado do Outlook Calendar"
    }


@router.post("/sync/sprint", response_model=SyncResultResponse)
async def sync_sprint_to_calendar(request: SprintSyncRequest):
    """
    Sincroniza um sprint com o(s) calendario(s).

    Cria ou atualiza evento representando o periodo do sprint.
    """
    sync_service = get_calendar_sync_service()

    # Inicializa se necessario
    if not sync_service.is_initialized:
        await sync_service.initialize()

    provider = _get_provider_enum(request.provider)

    result = await sync_service.sync_sprint(
        sprint_id=request.sprint_id,
        name=request.name,
        start_date=request.start_date,
        end_date=request.end_date,
        goal=request.goal,
        attendees=request.attendees,
        provider=provider
    )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings
    )


@router.post("/sync/deadline", response_model=SyncResultResponse)
async def sync_deadline_to_calendar(request: DeadlineSyncRequest):
    """
    Sincroniza deadline de story com o(s) calendario(s).

    Cria ou atualiza evento representando a deadline.
    """
    sync_service = get_calendar_sync_service()

    # Inicializa se necessario
    if not sync_service.is_initialized:
        await sync_service.initialize()

    provider = _get_provider_enum(request.provider)

    result = await sync_service.sync_deadline(
        story_id=request.story_id,
        title=request.title,
        deadline=request.deadline,
        description=request.description,
        priority=request.priority,
        provider=provider
    )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings
    )


@router.delete("/event", response_model=SyncResultResponse)
async def remove_calendar_event(request: RemoveEventRequest):
    """
    Remove evento sincronizado do(s) calendario(s).

    Remove evento pelo ID externo (sprint_id ou story_id).
    """
    sync_service = get_calendar_sync_service()

    # Inicializa se necessario
    if not sync_service.is_initialized:
        await sync_service.initialize()

    provider = _get_provider_enum(request.provider)

    result = await sync_service.remove_event(
        external_id=request.external_id,
        provider=provider
    )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings
    )


@router.get("/google/events")
async def list_google_calendar_events(
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    max_results: int = 50
):
    """
    Lista eventos do Google Calendar.

    Args:
        start_date: Data inicial (YYYY-MM-DD)
        end_date: Data final (YYYY-MM-DD)
        max_results: Numero maximo de eventos

    Returns:
        Lista de eventos
    """
    client = get_google_calendar_client()

    if not client.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Google Calendar nao esta conectado"
        )

    time_min = datetime.strptime(start_date, "%Y-%m-%d") if start_date else None
    time_max = datetime.strptime(end_date, "%Y-%m-%d") if end_date else None

    events = await client.get_events(
        time_min=time_min,
        time_max=time_max,
        max_results=max_results
    )

    return {
        "events": events,
        "total": len(events)
    }


@router.get("/outlook/events")
async def list_outlook_calendar_events(
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    top: int = 50
):
    """
    Lista eventos do Outlook Calendar.

    Args:
        start_date: Data inicial (YYYY-MM-DD)
        end_date: Data final (YYYY-MM-DD)
        top: Numero maximo de eventos

    Returns:
        Lista de eventos
    """
    client = get_outlook_calendar_client()

    if not client.is_connected:
        raise HTTPException(
            status_code=400,
            detail="Outlook Calendar nao esta conectado"
        )

    start_datetime = datetime.strptime(start_date, "%Y-%m-%d") if start_date else None
    end_datetime = datetime.strptime(end_date, "%Y-%m-%d") if end_date else None

    events = await client.get_events(
        start_datetime=start_datetime,
        end_datetime=end_datetime,
        top=top
    )

    return {
        "events": events,
        "total": len(events)
    }


@router.get("/google/test")
async def test_google_calendar_connection():
    """
    Testa a conexao com o Google Calendar.
    """
    client = get_google_calendar_client()

    if await client.test_connection():
        return {"success": True, "message": "Conexao OK"}
    else:
        return {"success": False, "message": "Falha na conexao", "error": client.last_error}


@router.get("/outlook/test")
async def test_outlook_calendar_connection():
    """
    Testa a conexao com o Outlook Calendar.
    """
    client = get_outlook_calendar_client()

    if await client.test_connection():
        return {"success": True, "message": "Conexao OK"}
    else:
        return {"success": False, "message": "Falha na conexao", "error": client.last_error}


@router.post("/sync/sprints/batch", response_model=SyncResultResponse)
async def sync_sprints_batch(
    sprints: List[SprintSyncRequest],
    background_tasks: BackgroundTasks
):
    """
    Sincroniza multiplos sprints em batch.

    Os sprints sao sincronizados em background para evitar timeout.
    """
    sync_service = get_calendar_sync_service()

    if not sync_service.is_initialized:
        await sync_service.initialize()

    # Converte requests para dicionarios
    sprint_dicts = [
        {
            "sprint_id": s.sprint_id,
            "name": s.name,
            "start_date": s.start_date,
            "end_date": s.end_date,
            "goal": s.goal
        }
        for s in sprints
    ]

    # Determina o provider (usa o primeiro ou "all" se misturado)
    provider = CalendarProvider.ALL
    if sprints and all(s.provider == sprints[0].provider for s in sprints):
        provider = _get_provider_enum(sprints[0].provider)

    result = await sync_service.sync_all_sprints(
        sprints=sprint_dicts,
        provider=provider
    )

    return SyncResultResponse(
        success=result.success,
        items_synced=result.items_synced,
        items_failed=result.items_failed,
        errors=result.errors,
        warnings=result.warnings
    )
