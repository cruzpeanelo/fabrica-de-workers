# -*- coding: utf-8 -*-
"""
Calendar Integration Routes
===========================
API endpoints para integracao com Google Calendar e Outlook Calendar.

Endpoints:
- GET  /api/v1/calendar/status - Status da integracao de calendario
- POST /api/v1/calendar/connect - Conecta ao provedor de calendario
- POST /api/v1/calendar/disconnect - Desconecta do calendario
- GET  /api/v1/calendar/events - Lista eventos
- POST /api/v1/calendar/events - Cria evento
- PUT  /api/v1/calendar/events/{event_id} - Atualiza evento
- DELETE /api/v1/calendar/events/{event_id} - Deleta evento
- POST /api/v1/calendar/sync-sprint - Sincroniza datas de sprint
- POST /api/v1/calendar/story-meeting - Cria reuniao para story
"""

import logging
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List

from fastapi import APIRouter, HTTPException, Request, Query
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1/calendar", tags=["Calendar Integration"])


# =============================================================================
# MODELS
# =============================================================================

class CalendarConnectRequest(BaseModel):
    """Request para conectar ao calendario"""
    provider: str = Field(..., description="Provedor: 'google' ou 'outlook'")
    credentials: Optional[Dict[str, str]] = Field(None, description="Credenciais OAuth (opcional se env vars configuradas)")


class CalendarEventRequest(BaseModel):
    """Request para criar/atualizar evento"""
    title: str = Field(..., min_length=1, max_length=255, description="Titulo do evento")
    start_time: datetime = Field(..., description="Data/hora de inicio")
    end_time: datetime = Field(..., description="Data/hora de fim")
    description: Optional[str] = Field(None, description="Descricao do evento")
    location: Optional[str] = Field(None, description="Local do evento")
    attendees: Optional[List[str]] = Field(default=[], description="Lista de emails dos participantes")
    reminder_minutes: int = Field(15, ge=0, le=1440, description="Lembrete em minutos antes")
    is_all_day: bool = Field(False, description="Evento de dia inteiro")
    recurrence: Optional[str] = Field(None, description="Regra de recorrencia (RRULE)")


class SprintSyncRequest(BaseModel):
    """Request para sincronizar sprint com calendario"""
    sprint_id: str = Field(..., description="ID do sprint")
    sprint_name: str = Field(..., description="Nome do sprint")
    start_date: datetime = Field(..., description="Data de inicio do sprint")
    end_date: datetime = Field(..., description="Data de fim do sprint")
    team_members: Optional[List[str]] = Field(default=[], description="Emails dos membros do time")
    create_daily_standups: bool = Field(False, description="Criar eventos diarios de standup")


class StoryMeetingRequest(BaseModel):
    """Request para criar reuniao de story"""
    story_id: str = Field(..., description="ID da story")
    story_title: str = Field(..., description="Titulo da story")
    meeting_time: datetime = Field(..., description="Data/hora da reuniao")
    duration_minutes: int = Field(30, ge=15, le=480, description="Duracao em minutos")
    attendees: Optional[List[str]] = Field(default=[], description="Emails dos participantes")
    description: Optional[str] = Field(None, description="Descricao adicional")


class CalendarEventResponse(BaseModel):
    """Response de evento de calendario"""
    id: Optional[str]
    title: str
    start_time: str
    end_time: str
    description: Optional[str]
    location: Optional[str]
    attendees: List[str]
    is_all_day: bool
    reminder_minutes: int


# =============================================================================
# HELPERS
# =============================================================================

def get_tenant_id(request: Request) -> str:
    """Extrai tenant_id do request"""
    tenant_id = request.headers.get("X-Tenant-ID")
    return tenant_id if tenant_id else "default"


async def get_calendar_provider(provider: str = "auto"):
    """Obtem provedor de calendario"""
    from factory.integrations.calendar_integration import get_calendar_provider as get_provider

    calendar = get_provider(provider)
    if not calendar:
        raise HTTPException(
            status_code=400,
            detail=f"Provedor de calendario '{provider}' nao configurado. Verifique as variaveis de ambiente."
        )
    return calendar


# =============================================================================
# STATUS ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_calendar_status(request: Request, provider: str = "auto"):
    """
    Retorna status da integracao de calendario.

    - provider: 'google', 'outlook' ou 'auto' (detecta automaticamente)
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.calendar_integration import (
            get_google_calendar,
            get_outlook_calendar
        )

        statuses = {}

        # Google Calendar status
        google = get_google_calendar()
        if google.config.is_valid():
            statuses["google"] = {
                "configured": True,
                "enabled": google.config.enabled,
                "connected": google.is_connected,
                "calendar_id": google.config.calendar_id,
                "last_error": google.last_error
            }
        else:
            statuses["google"] = {"configured": False}

        # Outlook Calendar status
        outlook = get_outlook_calendar()
        if outlook.config.is_valid():
            statuses["outlook"] = {
                "configured": True,
                "enabled": outlook.config.enabled,
                "connected": outlook.is_connected,
                "calendar_id": outlook.config.calendar_id,
                "last_error": outlook.last_error
            }
        else:
            statuses["outlook"] = {"configured": False}

        return {
            "tenant_id": tenant_id,
            "providers": statuses,
            "active_provider": provider if provider != "auto" else None
        }

    except Exception as e:
        logger.error(f"Erro ao buscar status do calendario: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/providers")
async def list_calendar_providers():
    """
    Lista provedores de calendario disponiveis.
    """
    return {
        "providers": [
            {
                "id": "google",
                "name": "Google Calendar",
                "description": "Integracao com Google Calendar via API v3",
                "required_env_vars": [
                    "GOOGLE_CALENDAR_CLIENT_ID",
                    "GOOGLE_CALENDAR_CLIENT_SECRET",
                    "GOOGLE_CALENDAR_REFRESH_TOKEN"
                ]
            },
            {
                "id": "outlook",
                "name": "Microsoft Outlook Calendar",
                "description": "Integracao com Outlook via Microsoft Graph API",
                "required_env_vars": [
                    "OUTLOOK_CALENDAR_CLIENT_ID",
                    "OUTLOOK_CALENDAR_CLIENT_SECRET",
                    "OUTLOOK_CALENDAR_REFRESH_TOKEN"
                ]
            }
        ]
    }


# =============================================================================
# CONNECTION ENDPOINTS
# =============================================================================

@router.post("/connect")
async def connect_calendar(request: Request, connect_req: CalendarConnectRequest):
    """
    Conecta ao provedor de calendario.

    Se as credenciais forem fornecidas, usa-as temporariamente.
    Caso contrario, usa as variaveis de ambiente configuradas.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.calendar_integration import (
            GoogleCalendarProvider,
            OutlookCalendarProvider,
            GoogleCalendarConfig,
            OutlookCalendarConfig
        )

        provider = connect_req.provider.lower()

        if provider == "google":
            if connect_req.credentials:
                config = GoogleCalendarConfig(
                    enabled=True,
                    provider="google",
                    client_id=connect_req.credentials.get("client_id", ""),
                    client_secret=connect_req.credentials.get("client_secret", ""),
                    refresh_token=connect_req.credentials.get("refresh_token", "")
                )
            else:
                config = GoogleCalendarConfig.from_env()

            calendar = GoogleCalendarProvider(config)

        elif provider == "outlook":
            if connect_req.credentials:
                config = OutlookCalendarConfig(
                    enabled=True,
                    provider="outlook",
                    client_id=connect_req.credentials.get("client_id", ""),
                    client_secret=connect_req.credentials.get("client_secret", ""),
                    tenant_id=connect_req.credentials.get("tenant_id", "common"),
                    refresh_token=connect_req.credentials.get("refresh_token", "")
                )
            else:
                config = OutlookCalendarConfig.from_env()

            calendar = OutlookCalendarProvider(config)

        else:
            raise HTTPException(
                status_code=400,
                detail=f"Provedor invalido: {provider}. Use 'google' ou 'outlook'."
            )

        success = await calendar.connect()

        if success:
            return {
                "success": True,
                "message": f"Conectado ao {provider} calendar com sucesso",
                "provider": provider,
                "status": calendar.get_status()
            }
        else:
            raise HTTPException(
                status_code=400,
                detail=f"Falha ao conectar: {calendar.last_error}"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao conectar calendario: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/disconnect")
async def disconnect_calendar(request: Request, provider: str = "auto"):
    """
    Desconecta do provedor de calendario.
    """
    try:
        calendar = await get_calendar_provider(provider)
        await calendar.disconnect()

        return {
            "success": True,
            "message": f"Desconectado do calendario",
            "provider": calendar.config.provider
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao desconectar calendario: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# EVENT ENDPOINTS
# =============================================================================

@router.get("/events")
async def list_events(
    request: Request,
    provider: str = "auto",
    start_date: Optional[datetime] = None,
    end_date: Optional[datetime] = None,
    max_results: int = Query(50, ge=1, le=500)
):
    """
    Lista eventos do calendario.

    - start_date: Data inicial (padrao: hoje)
    - end_date: Data final (padrao: 30 dias a partir de start_date)
    - max_results: Maximo de eventos retornados
    """
    try:
        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        # Define periodo padrao
        if not start_date:
            start_date = datetime.utcnow().replace(hour=0, minute=0, second=0, microsecond=0)
        if not end_date:
            end_date = start_date + timedelta(days=30)

        events = await calendar.list_events(start_date, end_date, max_results)

        return {
            "provider": calendar.config.provider,
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
            "count": len(events),
            "events": [event.to_dict() for event in events]
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao listar eventos: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/events")
async def create_event(request: Request, event_req: CalendarEventRequest, provider: str = "auto"):
    """
    Cria um novo evento no calendario.
    """
    try:
        from factory.integrations.calendar_integration import CalendarEvent

        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        event = CalendarEvent(
            title=event_req.title,
            start_time=event_req.start_time,
            end_time=event_req.end_time,
            description=event_req.description,
            location=event_req.location,
            attendees=event_req.attendees or [],
            reminder_minutes=event_req.reminder_minutes,
            is_all_day=event_req.is_all_day,
            recurrence=event_req.recurrence
        )

        event_id = await calendar.create_event(event)

        if event_id:
            return {
                "success": True,
                "message": "Evento criado com sucesso",
                "event_id": event_id,
                "provider": calendar.config.provider
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao criar evento no calendario"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao criar evento: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/events/{event_id}")
async def get_event(request: Request, event_id: str, provider: str = "auto"):
    """
    Busca um evento especifico pelo ID.
    """
    try:
        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        event = await calendar.get_event(event_id)

        if event:
            return {
                "provider": calendar.config.provider,
                "event": event.to_dict()
            }
        else:
            raise HTTPException(
                status_code=404,
                detail=f"Evento nao encontrado: {event_id}"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao buscar evento: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.put("/events/{event_id}")
async def update_event(
    request: Request,
    event_id: str,
    event_req: CalendarEventRequest,
    provider: str = "auto"
):
    """
    Atualiza um evento existente.
    """
    try:
        from factory.integrations.calendar_integration import CalendarEvent

        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        event = CalendarEvent(
            title=event_req.title,
            start_time=event_req.start_time,
            end_time=event_req.end_time,
            description=event_req.description,
            location=event_req.location,
            attendees=event_req.attendees or [],
            reminder_minutes=event_req.reminder_minutes,
            is_all_day=event_req.is_all_day,
            recurrence=event_req.recurrence,
            external_id=event_id
        )

        success = await calendar.update_event(event_id, event)

        if success:
            return {
                "success": True,
                "message": "Evento atualizado com sucesso",
                "event_id": event_id,
                "provider": calendar.config.provider
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao atualizar evento"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao atualizar evento: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/events/{event_id}")
async def delete_event(request: Request, event_id: str, provider: str = "auto"):
    """
    Deleta um evento do calendario.
    """
    try:
        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        success = await calendar.delete_event(event_id)

        if success:
            return {
                "success": True,
                "message": "Evento deletado com sucesso",
                "event_id": event_id,
                "provider": calendar.config.provider
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao deletar evento"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao deletar evento: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# SPRINT SYNC ENDPOINTS
# =============================================================================

@router.post("/sync-sprint")
async def sync_sprint_dates(request: Request, sprint_req: SprintSyncRequest, provider: str = "auto"):
    """
    Sincroniza datas de sprint para o calendario.

    Cria automaticamente eventos para:
    - Sprint Start
    - Sprint Review
    - Sprint Planning
    - Daily Standups (opcional)
    """
    tenant_id = get_tenant_id(request)

    try:
        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        # Sincroniza eventos do sprint
        event_ids = await calendar.sync_sprint_dates(
            sprint_id=sprint_req.sprint_id,
            sprint_name=sprint_req.sprint_name,
            start_date=sprint_req.start_date,
            end_date=sprint_req.end_date,
            team_members=sprint_req.team_members
        )

        # Criar daily standups se solicitado
        standup_ids = []
        if sprint_req.create_daily_standups:
            from factory.integrations.calendar_integration import CalendarEvent, CalendarEventType

            current_date = sprint_req.start_date
            while current_date <= sprint_req.end_date:
                # Pular finais de semana
                if current_date.weekday() < 5:  # Segunda a Sexta
                    standup = CalendarEvent(
                        title=f"Daily Standup - {sprint_req.sprint_name}",
                        start_time=current_date.replace(hour=9, minute=0),
                        end_time=current_date.replace(hour=9, minute=15),
                        description=f"Daily standup do sprint {sprint_req.sprint_name}",
                        event_type=CalendarEventType.DAILY_STANDUP,
                        attendees=sprint_req.team_members,
                        metadata={"sprint_id": sprint_req.sprint_id}
                    )
                    standup_id = await calendar.create_event(standup)
                    if standup_id:
                        standup_ids.append(standup_id)

                current_date += timedelta(days=1)

        return {
            "success": True,
            "message": f"Sprint sincronizado com calendario",
            "sprint_id": sprint_req.sprint_id,
            "sprint_name": sprint_req.sprint_name,
            "provider": calendar.config.provider,
            "events_created": {
                "sprint_start": event_ids.get("sprint_start"),
                "sprint_review": event_ids.get("sprint_review"),
                "sprint_planning": event_ids.get("sprint_planning"),
                "daily_standups": standup_ids if sprint_req.create_daily_standups else []
            },
            "total_events": len([v for v in event_ids.values() if v]) + len(standup_ids)
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao sincronizar sprint: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/story-meeting")
async def create_story_meeting(request: Request, meeting_req: StoryMeetingRequest, provider: str = "auto"):
    """
    Cria uma reuniao para discutir uma story.
    """
    tenant_id = get_tenant_id(request)

    try:
        calendar = await get_calendar_provider(provider)

        if not calendar.is_connected:
            success = await calendar.connect()
            if not success:
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {calendar.last_error}"
                )

        event_id = await calendar.create_story_meeting(
            story_id=meeting_req.story_id,
            story_title=meeting_req.story_title,
            meeting_time=meeting_req.meeting_time,
            duration_minutes=meeting_req.duration_minutes,
            attendees=meeting_req.attendees,
            description=meeting_req.description
        )

        if event_id:
            return {
                "success": True,
                "message": "Reuniao de story criada com sucesso",
                "event_id": event_id,
                "story_id": meeting_req.story_id,
                "provider": calendar.config.provider
            }
        else:
            raise HTTPException(
                status_code=400,
                detail="Falha ao criar reuniao de story"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao criar reuniao de story: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# OAUTH CALLBACK ENDPOINTS
# =============================================================================

@router.get("/oauth/google/callback")
async def google_oauth_callback(code: str, state: Optional[str] = None):
    """
    Callback para OAuth do Google Calendar.

    Troca o codigo de autorizacao por tokens.
    """
    try:
        import aiohttp

        from factory.integrations.calendar_integration import GoogleCalendarConfig

        config = GoogleCalendarConfig.from_env()

        async with aiohttp.ClientSession() as session:
            token_url = "https://oauth2.googleapis.com/token"
            data = {
                "code": code,
                "client_id": config.client_id,
                "client_secret": config.client_secret,
                "redirect_uri": config.redirect_uri,
                "grant_type": "authorization_code"
            }

            async with session.post(token_url, data=data) as response:
                if response.status == 200:
                    token_data = await response.json()
                    return {
                        "success": True,
                        "message": "Autorizacao concedida",
                        "refresh_token": token_data.get("refresh_token"),
                        "access_token": token_data.get("access_token", "")[:20] + "...",
                        "expires_in": token_data.get("expires_in"),
                        "note": "Salve o refresh_token na variavel GOOGLE_CALENDAR_REFRESH_TOKEN"
                    }
                else:
                    error = await response.text()
                    raise HTTPException(status_code=400, detail=f"Erro no OAuth: {error}")

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no callback OAuth Google: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/oauth/outlook/callback")
async def outlook_oauth_callback(code: str, state: Optional[str] = None):
    """
    Callback para OAuth do Outlook Calendar.

    Troca o codigo de autorizacao por tokens.
    """
    try:
        import aiohttp

        from factory.integrations.calendar_integration import OutlookCalendarConfig

        config = OutlookCalendarConfig.from_env()

        token_url = f"https://login.microsoftonline.com/{config.tenant_id}/oauth2/v2.0/token"

        async with aiohttp.ClientSession() as session:
            data = {
                "code": code,
                "client_id": config.client_id,
                "client_secret": config.client_secret,
                "redirect_uri": config.redirect_uri,
                "grant_type": "authorization_code",
                "scope": "https://graph.microsoft.com/Calendars.ReadWrite offline_access"
            }

            async with session.post(token_url, data=data) as response:
                if response.status == 200:
                    token_data = await response.json()
                    return {
                        "success": True,
                        "message": "Autorizacao concedida",
                        "refresh_token": token_data.get("refresh_token"),
                        "access_token": token_data.get("access_token", "")[:20] + "...",
                        "expires_in": token_data.get("expires_in"),
                        "note": "Salve o refresh_token na variavel OUTLOOK_CALENDAR_REFRESH_TOKEN"
                    }
                else:
                    error = await response.text()
                    raise HTTPException(status_code=400, detail=f"Erro no OAuth: {error}")

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no callback OAuth Outlook: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/oauth/google/authorize")
async def get_google_auth_url():
    """
    Retorna URL para autorizar acesso ao Google Calendar.
    """
    from factory.integrations.calendar_integration import GoogleCalendarConfig

    config = GoogleCalendarConfig.from_env()

    if not config.client_id:
        raise HTTPException(
            status_code=400,
            detail="GOOGLE_CALENDAR_CLIENT_ID nao configurado"
        )

    scopes = [
        "https://www.googleapis.com/auth/calendar",
        "https://www.googleapis.com/auth/calendar.events"
    ]

    auth_url = (
        f"https://accounts.google.com/o/oauth2/v2/auth"
        f"?client_id={config.client_id}"
        f"&redirect_uri={config.redirect_uri}"
        f"&response_type=code"
        f"&scope={' '.join(scopes)}"
        f"&access_type=offline"
        f"&prompt=consent"
    )

    return {
        "auth_url": auth_url,
        "instructions": "Acesse a URL acima para autorizar o acesso ao Google Calendar"
    }


@router.get("/oauth/outlook/authorize")
async def get_outlook_auth_url():
    """
    Retorna URL para autorizar acesso ao Outlook Calendar.
    """
    from factory.integrations.calendar_integration import OutlookCalendarConfig

    config = OutlookCalendarConfig.from_env()

    if not config.client_id:
        raise HTTPException(
            status_code=400,
            detail="OUTLOOK_CALENDAR_CLIENT_ID nao configurado"
        )

    scopes = [
        "https://graph.microsoft.com/Calendars.ReadWrite",
        "offline_access"
    ]

    auth_url = (
        f"https://login.microsoftonline.com/{config.tenant_id}/oauth2/v2.0/authorize"
        f"?client_id={config.client_id}"
        f"&redirect_uri={config.redirect_uri}"
        f"&response_type=code"
        f"&scope={' '.join(scopes)}"
    )

    return {
        "auth_url": auth_url,
        "instructions": "Acesse a URL acima para autorizar o acesso ao Outlook Calendar"
    }
