# -*- coding: utf-8 -*-
"""
Calendar Integration Module
===========================
Integracao com Google Calendar e Microsoft Outlook Calendar.

Funcionalidades:
- CalendarProvider abstrato para diferentes provedores
- GoogleCalendarProvider usando Google Calendar API
- OutlookCalendarProvider usando Microsoft Graph API
- Criar, atualizar e deletar eventos
- Sincronizar datas de sprint para calendario

Configuracao via variaveis de ambiente:
Google Calendar:
- GOOGLE_CALENDAR_CLIENT_ID: ID do cliente OAuth
- GOOGLE_CALENDAR_CLIENT_SECRET: Secret do cliente OAuth
- GOOGLE_CALENDAR_REDIRECT_URI: URI de redirect
- GOOGLE_CALENDAR_REFRESH_TOKEN: Token de refresh

Microsoft Outlook:
- OUTLOOK_CALENDAR_CLIENT_ID: ID do cliente Azure AD
- OUTLOOK_CALENDAR_CLIENT_SECRET: Secret do cliente Azure AD
- OUTLOOK_CALENDAR_TENANT_ID: ID do tenant Azure AD
- OUTLOOK_CALENDAR_REDIRECT_URI: URI de redirect
- OUTLOOK_CALENDAR_REFRESH_TOKEN: Token de refresh
"""

import os
import logging
import aiohttp
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult,
    OAuthToken,
    OAuthTokenManager
)

logger = logging.getLogger(__name__)


class CalendarEventType(str, Enum):
    """Tipos de eventos de calendario"""
    SPRINT_START = "sprint_start"
    SPRINT_END = "sprint_end"
    SPRINT_REVIEW = "sprint_review"
    SPRINT_PLANNING = "sprint_planning"
    DAILY_STANDUP = "daily_standup"
    STORY_MEETING = "story_meeting"
    CUSTOM = "custom"


@dataclass
class CalendarEvent:
    """Representa um evento de calendario"""
    title: str
    start_time: datetime
    end_time: datetime
    description: Optional[str] = None
    location: Optional[str] = None
    event_type: CalendarEventType = CalendarEventType.CUSTOM
    attendees: List[str] = field(default_factory=list)
    reminder_minutes: int = 15
    is_all_day: bool = False
    recurrence: Optional[str] = None
    external_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "title": self.title,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "description": self.description,
            "location": self.location,
            "event_type": self.event_type.value,
            "attendees": self.attendees,
            "reminder_minutes": self.reminder_minutes,
            "is_all_day": self.is_all_day,
            "recurrence": self.recurrence,
            "external_id": self.external_id,
            "metadata": self.metadata
        }


@dataclass
class CalendarConfig(IntegrationConfig):
    """Configuracao base para integracoes de calendario"""
    provider: str = ""
    calendar_id: str = "primary"
    default_reminder_minutes: int = 15
    sync_sprint_events: bool = True
    sync_story_events: bool = True
    create_recurring_standups: bool = False


@dataclass
class GoogleCalendarConfig(CalendarConfig):
    """Configuracao especifica para Google Calendar"""
    client_id: str = ""
    client_secret: str = ""
    redirect_uri: str = ""
    access_token: str = ""
    refresh_token: str = ""
    token_expiry: Optional[datetime] = None

    @classmethod
    def from_env(cls) -> "GoogleCalendarConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("GOOGLE_CALENDAR_ENABLED", "false").lower() == "true",
            provider="google",
            client_id=os.getenv("GOOGLE_CALENDAR_CLIENT_ID", ""),
            client_secret=os.getenv("GOOGLE_CALENDAR_CLIENT_SECRET", ""),
            redirect_uri=os.getenv("GOOGLE_CALENDAR_REDIRECT_URI", ""),
            refresh_token=os.getenv("GOOGLE_CALENDAR_REFRESH_TOKEN", ""),
            calendar_id=os.getenv("GOOGLE_CALENDAR_ID", "primary"),
            default_reminder_minutes=int(os.getenv("GOOGLE_CALENDAR_REMINDER_MINUTES", "15")),
            sync_sprint_events=os.getenv("GOOGLE_CALENDAR_SYNC_SPRINTS", "true").lower() == "true",
            sync_story_events=os.getenv("GOOGLE_CALENDAR_SYNC_STORIES", "true").lower() == "true",
            auto_sync=os.getenv("GOOGLE_CALENDAR_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("GOOGLE_CALENDAR_SYNC_INTERVAL", "60"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.client_id and self.client_secret and self.refresh_token)


@dataclass
class OutlookCalendarConfig(CalendarConfig):
    """Configuracao especifica para Outlook Calendar"""
    client_id: str = ""
    client_secret: str = ""
    tenant_id: str = ""
    redirect_uri: str = ""
    access_token: str = ""
    refresh_token: str = ""
    token_expiry: Optional[datetime] = None

    @classmethod
    def from_env(cls) -> "OutlookCalendarConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("OUTLOOK_CALENDAR_ENABLED", "false").lower() == "true",
            provider="outlook",
            client_id=os.getenv("OUTLOOK_CALENDAR_CLIENT_ID", ""),
            client_secret=os.getenv("OUTLOOK_CALENDAR_CLIENT_SECRET", ""),
            tenant_id=os.getenv("OUTLOOK_CALENDAR_TENANT_ID", "common"),
            redirect_uri=os.getenv("OUTLOOK_CALENDAR_REDIRECT_URI", ""),
            refresh_token=os.getenv("OUTLOOK_CALENDAR_REFRESH_TOKEN", ""),
            calendar_id=os.getenv("OUTLOOK_CALENDAR_ID", ""),
            default_reminder_minutes=int(os.getenv("OUTLOOK_CALENDAR_REMINDER_MINUTES", "15")),
            sync_sprint_events=os.getenv("OUTLOOK_CALENDAR_SYNC_SPRINTS", "true").lower() == "true",
            sync_story_events=os.getenv("OUTLOOK_CALENDAR_SYNC_STORIES", "true").lower() == "true",
            auto_sync=os.getenv("OUTLOOK_CALENDAR_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("OUTLOOK_CALENDAR_SYNC_INTERVAL", "60"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.client_id and self.client_secret and self.refresh_token)


class CalendarProvider(ABC):
    """
    Classe base abstrata para provedores de calendario.

    Define a interface comum para integracoes com Google Calendar,
    Outlook Calendar e outros provedores.
    """

    def __init__(self, config: CalendarConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._last_error: Optional[str] = None
        self._session: Optional[aiohttp.ClientSession] = None
        self._token_manager: Optional[OAuthTokenManager] = None

    @property
    def is_connected(self) -> bool:
        return self.status == IntegrationStatus.CONNECTED

    @property
    def last_error(self) -> Optional[str]:
        return self._last_error

    @abstractmethod
    async def connect(self) -> bool:
        """Conecta ao provedor de calendario"""
        pass

    @abstractmethod
    async def disconnect(self) -> bool:
        """Desconecta do provedor de calendario"""
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """Testa a conexao com o provedor"""
        pass

    @abstractmethod
    async def create_event(self, event: CalendarEvent) -> Optional[str]:
        """
        Cria um evento no calendario.

        Args:
            event: Dados do evento

        Returns:
            ID do evento criado ou None em caso de erro
        """
        pass

    @abstractmethod
    async def update_event(self, event_id: str, event: CalendarEvent) -> bool:
        """
        Atualiza um evento existente.

        Args:
            event_id: ID do evento
            event: Novos dados do evento

        Returns:
            True se atualizado com sucesso
        """
        pass

    @abstractmethod
    async def delete_event(self, event_id: str) -> bool:
        """
        Deleta um evento do calendario.

        Args:
            event_id: ID do evento

        Returns:
            True se deletado com sucesso
        """
        pass

    @abstractmethod
    async def get_event(self, event_id: str) -> Optional[CalendarEvent]:
        """
        Busca um evento especifico.

        Args:
            event_id: ID do evento

        Returns:
            CalendarEvent ou None se nao encontrado
        """
        pass

    @abstractmethod
    async def list_events(
        self,
        start_date: datetime,
        end_date: datetime,
        max_results: int = 100
    ) -> List[CalendarEvent]:
        """
        Lista eventos em um periodo.

        Args:
            start_date: Data inicial
            end_date: Data final
            max_results: Maximo de resultados

        Returns:
            Lista de eventos
        """
        pass

    async def sync_sprint_dates(
        self,
        sprint_id: str,
        sprint_name: str,
        start_date: datetime,
        end_date: datetime,
        team_members: Optional[List[str]] = None
    ) -> Dict[str, Optional[str]]:
        """
        Sincroniza datas de sprint para o calendario.

        Cria eventos para:
        - Sprint Start
        - Sprint End (Review)
        - Daily Standups (opcional)

        Args:
            sprint_id: ID do sprint
            sprint_name: Nome do sprint
            start_date: Data de inicio
            end_date: Data de fim
            team_members: Lista de emails dos membros

        Returns:
            Dict com IDs dos eventos criados
        """
        event_ids = {}
        attendees = team_members or []

        # Evento de inicio do Sprint
        sprint_start = CalendarEvent(
            title=f"Sprint Start: {sprint_name}",
            start_time=start_date.replace(hour=9, minute=0),
            end_time=start_date.replace(hour=10, minute=0),
            description=f"Inicio do sprint {sprint_name}\n\nSprint ID: {sprint_id}",
            event_type=CalendarEventType.SPRINT_START,
            attendees=attendees,
            reminder_minutes=self.config.default_reminder_minutes,
            metadata={"sprint_id": sprint_id}
        )
        event_ids["sprint_start"] = await self.create_event(sprint_start)

        # Evento de fim do Sprint (Review)
        sprint_review = CalendarEvent(
            title=f"Sprint Review: {sprint_name}",
            start_time=end_date.replace(hour=14, minute=0),
            end_time=end_date.replace(hour=16, minute=0),
            description=f"Review e retrospectiva do sprint {sprint_name}\n\nSprint ID: {sprint_id}",
            event_type=CalendarEventType.SPRINT_REVIEW,
            attendees=attendees,
            reminder_minutes=self.config.default_reminder_minutes,
            metadata={"sprint_id": sprint_id}
        )
        event_ids["sprint_review"] = await self.create_event(sprint_review)

        # Evento de Planning (proximo sprint)
        sprint_planning = CalendarEvent(
            title=f"Sprint Planning: {sprint_name}",
            start_time=end_date.replace(hour=16, minute=30),
            end_time=end_date.replace(hour=18, minute=0),
            description=f"Planning do proximo sprint apos {sprint_name}\n\nSprint ID: {sprint_id}",
            event_type=CalendarEventType.SPRINT_PLANNING,
            attendees=attendees,
            reminder_minutes=self.config.default_reminder_minutes,
            metadata={"sprint_id": sprint_id}
        )
        event_ids["sprint_planning"] = await self.create_event(sprint_planning)

        return event_ids

    async def create_story_meeting(
        self,
        story_id: str,
        story_title: str,
        meeting_time: datetime,
        duration_minutes: int = 30,
        attendees: Optional[List[str]] = None,
        description: Optional[str] = None
    ) -> Optional[str]:
        """
        Cria uma reuniao para discutir uma story.

        Args:
            story_id: ID da story
            story_title: Titulo da story
            meeting_time: Data/hora da reuniao
            duration_minutes: Duracao em minutos
            attendees: Lista de emails dos participantes
            description: Descricao adicional

        Returns:
            ID do evento criado
        """
        event = CalendarEvent(
            title=f"Story Discussion: {story_title}",
            start_time=meeting_time,
            end_time=meeting_time + timedelta(minutes=duration_minutes),
            description=description or f"Discussao sobre a story: {story_title}\n\nStory ID: {story_id}",
            event_type=CalendarEventType.STORY_MEETING,
            attendees=attendees or [],
            reminder_minutes=self.config.default_reminder_minutes,
            metadata={"story_id": story_id}
        )
        return await self.create_event(event)

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "provider": self.config.provider,
            "status": self.status.value,
            "connected": self.is_connected,
            "last_error": self._last_error,
            "calendar_id": self.config.calendar_id,
            "auto_sync": self.config.auto_sync,
            "sync_sprint_events": self.config.sync_sprint_events,
            "sync_story_events": self.config.sync_story_events
        }


class GoogleCalendarProvider(CalendarProvider):
    """
    Provedor de calendario para Google Calendar.

    Usa a Google Calendar API v3.

    Exemplo de uso:
    ```python
    config = GoogleCalendarConfig.from_env()
    google = GoogleCalendarProvider(config)

    if await google.connect():
        event = CalendarEvent(
            title="Sprint Planning",
            start_time=datetime.now(),
            end_time=datetime.now() + timedelta(hours=2)
        )
        event_id = await google.create_event(event)
        print(f"Evento criado: {event_id}")
    ```
    """

    BASE_URL = "https://www.googleapis.com/calendar/v3"
    TOKEN_URL = "https://oauth2.googleapis.com/token"

    def __init__(self, config: GoogleCalendarConfig):
        super().__init__(config)
        self.config: GoogleCalendarConfig = config
        self._user_info: Optional[Dict] = None

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    async def _get_access_token(self) -> Optional[str]:
        """Obtem ou renova access token usando refresh token"""
        try:
            session = await self._ensure_session()

            data = {
                "client_id": self.config.client_id,
                "client_secret": self.config.client_secret,
                "refresh_token": self.config.refresh_token,
                "grant_type": "refresh_token"
            }

            async with session.post(self.TOKEN_URL, data=data) as response:
                if response.status == 200:
                    token_data = await response.json()
                    self.config.access_token = token_data["access_token"]
                    expires_in = token_data.get("expires_in", 3600)
                    self.config.token_expiry = datetime.utcnow() + timedelta(seconds=expires_in)
                    return self.config.access_token
                else:
                    error = await response.text()
                    logger.error(f"Erro ao renovar token Google: {error}")
                    return None
        except Exception as e:
            logger.error(f"Erro ao obter access token: {e}")
            return None

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": f"Bearer {self.config.access_token}",
            "Content-Type": "application/json",
            "Accept": "application/json"
        }

    async def connect(self) -> bool:
        """Conecta ao Google Calendar"""
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique client_id, client_secret e refresh_token."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info("Conectando ao Google Calendar...")

        try:
            # Obtem access token
            access_token = await self._get_access_token()
            if not access_token:
                self._last_error = "Falha ao obter access token"
                self.status = IntegrationStatus.ERROR
                return False

            session = await self._ensure_session()

            # Testa conexao listando calendarios
            url = f"{self.BASE_URL}/users/me/calendarList"
            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    data = await response.json()
                    calendars = data.get("items", [])
                    self._user_info = {"calendars": len(calendars)}
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(f"Conectado ao Google Calendar ({len(calendars)} calendarios)")
                    return True
                elif response.status == 401:
                    self._last_error = "Token invalido ou expirado"
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao Google Calendar: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """Desconecta do Google Calendar"""
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self.config.access_token = ""
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Google Calendar")
        return True

    async def test_connection(self) -> bool:
        """Testa a conexao com o Google Calendar"""
        try:
            # Renova token se necessario
            if self.config.token_expiry and datetime.utcnow() >= self.config.token_expiry:
                await self._get_access_token()

            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}"

            async with session.get(url, headers=self._get_headers()) as response:
                return response.status == 200
        except Exception:
            return False

    async def create_event(self, event: CalendarEvent) -> Optional[str]:
        """Cria um evento no Google Calendar"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}/events"

            # Formata evento para API Google
            event_body = self._event_to_google_format(event)

            async with session.post(url, headers=self._get_headers(), json=event_body) as response:
                if response.status in (200, 201):
                    data = await response.json()
                    event_id = data.get("id")
                    logger.info(f"Evento criado no Google Calendar: {event_id}")
                    return event_id
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao criar evento: {e}")

        return None

    async def update_event(self, event_id: str, event: CalendarEvent) -> bool:
        """Atualiza um evento no Google Calendar"""
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}/events/{event_id}"

            event_body = self._event_to_google_format(event)

            async with session.put(url, headers=self._get_headers(), json=event_body) as response:
                if response.status == 200:
                    logger.info(f"Evento atualizado: {event_id}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao atualizar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao atualizar evento {event_id}: {e}")

        return False

    async def delete_event(self, event_id: str) -> bool:
        """Deleta um evento do Google Calendar"""
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}/events/{event_id}"

            async with session.delete(url, headers=self._get_headers()) as response:
                if response.status in (200, 204):
                    logger.info(f"Evento deletado: {event_id}")
                    return True
                elif response.status == 404:
                    logger.warning(f"Evento nao encontrado: {event_id}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao deletar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao deletar evento {event_id}: {e}")

        return False

    async def get_event(self, event_id: str) -> Optional[CalendarEvent]:
        """Busca um evento especifico"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}/events/{event_id}"

            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    data = await response.json()
                    return self._google_event_to_calendar_event(data)
                elif response.status == 404:
                    logger.warning(f"Evento nao encontrado: {event_id}")
        except Exception as e:
            logger.error(f"Erro ao buscar evento {event_id}: {e}")

        return None

    async def list_events(
        self,
        start_date: datetime,
        end_date: datetime,
        max_results: int = 100
    ) -> List[CalendarEvent]:
        """Lista eventos em um periodo"""
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.BASE_URL}/calendars/{self.config.calendar_id}/events"

            params = {
                "timeMin": start_date.isoformat() + "Z",
                "timeMax": end_date.isoformat() + "Z",
                "maxResults": max_results,
                "singleEvents": "true",
                "orderBy": "startTime"
            }

            async with session.get(url, headers=self._get_headers(), params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    events = []
                    for item in data.get("items", []):
                        event = self._google_event_to_calendar_event(item)
                        if event:
                            events.append(event)
                    return events
        except Exception as e:
            logger.error(f"Erro ao listar eventos: {e}")

        return []

    def _event_to_google_format(self, event: CalendarEvent) -> Dict[str, Any]:
        """Converte CalendarEvent para formato Google API"""
        body = {
            "summary": event.title,
            "description": event.description or "",
            "location": event.location or ""
        }

        if event.is_all_day:
            body["start"] = {"date": event.start_time.strftime("%Y-%m-%d")}
            body["end"] = {"date": event.end_time.strftime("%Y-%m-%d")}
        else:
            body["start"] = {"dateTime": event.start_time.isoformat(), "timeZone": "UTC"}
            body["end"] = {"dateTime": event.end_time.isoformat(), "timeZone": "UTC"}

        if event.attendees:
            body["attendees"] = [{"email": email} for email in event.attendees]

        if event.reminder_minutes > 0:
            body["reminders"] = {
                "useDefault": False,
                "overrides": [
                    {"method": "popup", "minutes": event.reminder_minutes},
                    {"method": "email", "minutes": event.reminder_minutes}
                ]
            }

        if event.recurrence:
            body["recurrence"] = [event.recurrence]

        return body

    def _google_event_to_calendar_event(self, data: Dict) -> Optional[CalendarEvent]:
        """Converte evento Google para CalendarEvent"""
        try:
            start = data.get("start", {})
            end = data.get("end", {})

            # Determina se e all-day
            is_all_day = "date" in start

            if is_all_day:
                start_time = datetime.strptime(start["date"], "%Y-%m-%d")
                end_time = datetime.strptime(end["date"], "%Y-%m-%d")
            else:
                start_str = start.get("dateTime", start.get("date"))
                end_str = end.get("dateTime", end.get("date"))
                start_time = datetime.fromisoformat(start_str.replace("Z", "+00:00"))
                end_time = datetime.fromisoformat(end_str.replace("Z", "+00:00"))

            attendees = [a.get("email") for a in data.get("attendees", []) if a.get("email")]

            return CalendarEvent(
                title=data.get("summary", ""),
                start_time=start_time,
                end_time=end_time,
                description=data.get("description"),
                location=data.get("location"),
                attendees=attendees,
                is_all_day=is_all_day,
                external_id=data.get("id"),
                recurrence=data.get("recurrence", [None])[0] if data.get("recurrence") else None
            )
        except Exception as e:
            logger.error(f"Erro ao converter evento Google: {e}")
            return None


class OutlookCalendarProvider(CalendarProvider):
    """
    Provedor de calendario para Microsoft Outlook.

    Usa a Microsoft Graph API.

    Exemplo de uso:
    ```python
    config = OutlookCalendarConfig.from_env()
    outlook = OutlookCalendarProvider(config)

    if await outlook.connect():
        event = CalendarEvent(
            title="Sprint Planning",
            start_time=datetime.now(),
            end_time=datetime.now() + timedelta(hours=2)
        )
        event_id = await outlook.create_event(event)
        print(f"Evento criado: {event_id}")
    ```
    """

    GRAPH_URL = "https://graph.microsoft.com/v1.0"
    TOKEN_URL = "https://login.microsoftonline.com/{tenant}/oauth2/v2.0/token"

    def __init__(self, config: OutlookCalendarConfig):
        super().__init__(config)
        self.config: OutlookCalendarConfig = config
        self._user_info: Optional[Dict] = None

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    async def _get_access_token(self) -> Optional[str]:
        """Obtem ou renova access token usando refresh token"""
        try:
            session = await self._ensure_session()

            token_url = self.TOKEN_URL.format(tenant=self.config.tenant_id)
            data = {
                "client_id": self.config.client_id,
                "client_secret": self.config.client_secret,
                "refresh_token": self.config.refresh_token,
                "grant_type": "refresh_token",
                "scope": "https://graph.microsoft.com/Calendars.ReadWrite"
            }

            async with session.post(token_url, data=data) as response:
                if response.status == 200:
                    token_data = await response.json()
                    self.config.access_token = token_data["access_token"]
                    if token_data.get("refresh_token"):
                        self.config.refresh_token = token_data["refresh_token"]
                    expires_in = token_data.get("expires_in", 3600)
                    self.config.token_expiry = datetime.utcnow() + timedelta(seconds=expires_in)
                    return self.config.access_token
                else:
                    error = await response.text()
                    logger.error(f"Erro ao renovar token Outlook: {error}")
                    return None
        except Exception as e:
            logger.error(f"Erro ao obter access token: {e}")
            return None

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": f"Bearer {self.config.access_token}",
            "Content-Type": "application/json",
            "Accept": "application/json"
        }

    async def connect(self) -> bool:
        """Conecta ao Outlook Calendar"""
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique client_id, client_secret e refresh_token."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info("Conectando ao Outlook Calendar...")

        try:
            # Obtem access token
            access_token = await self._get_access_token()
            if not access_token:
                self._last_error = "Falha ao obter access token"
                self.status = IntegrationStatus.ERROR
                return False

            session = await self._ensure_session()

            # Testa conexao buscando perfil do usuario
            url = f"{self.GRAPH_URL}/me"
            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    user_data = await response.json()
                    self._user_info = {
                        "name": user_data.get("displayName"),
                        "email": user_data.get("mail") or user_data.get("userPrincipalName")
                    }
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(f"Conectado ao Outlook Calendar como: {self._user_info.get('name')}")
                    return True
                elif response.status == 401:
                    self._last_error = "Token invalido ou expirado"
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao Outlook Calendar: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """Desconecta do Outlook Calendar"""
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self.config.access_token = ""
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Outlook Calendar")
        return True

    async def test_connection(self) -> bool:
        """Testa a conexao com o Outlook Calendar"""
        try:
            # Renova token se necessario
            if self.config.token_expiry and datetime.utcnow() >= self.config.token_expiry:
                await self._get_access_token()

            session = await self._ensure_session()
            url = f"{self.GRAPH_URL}/me/calendars"

            async with session.get(url, headers=self._get_headers()) as response:
                return response.status == 200
        except Exception:
            return False

    def _get_calendar_url(self) -> str:
        """Retorna URL do calendario"""
        if self.config.calendar_id:
            return f"{self.GRAPH_URL}/me/calendars/{self.config.calendar_id}/events"
        return f"{self.GRAPH_URL}/me/calendar/events"

    async def create_event(self, event: CalendarEvent) -> Optional[str]:
        """Cria um evento no Outlook Calendar"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = self._get_calendar_url()

            # Formata evento para API Microsoft Graph
            event_body = self._event_to_outlook_format(event)

            async with session.post(url, headers=self._get_headers(), json=event_body) as response:
                if response.status in (200, 201):
                    data = await response.json()
                    event_id = data.get("id")
                    logger.info(f"Evento criado no Outlook Calendar: {event_id}")
                    return event_id
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao criar evento: {e}")

        return None

    async def update_event(self, event_id: str, event: CalendarEvent) -> bool:
        """Atualiza um evento no Outlook Calendar"""
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()

            if self.config.calendar_id:
                url = f"{self.GRAPH_URL}/me/calendars/{self.config.calendar_id}/events/{event_id}"
            else:
                url = f"{self.GRAPH_URL}/me/calendar/events/{event_id}"

            event_body = self._event_to_outlook_format(event)

            async with session.patch(url, headers=self._get_headers(), json=event_body) as response:
                if response.status == 200:
                    logger.info(f"Evento atualizado: {event_id}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao atualizar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao atualizar evento {event_id}: {e}")

        return False

    async def delete_event(self, event_id: str) -> bool:
        """Deleta um evento do Outlook Calendar"""
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()

            if self.config.calendar_id:
                url = f"{self.GRAPH_URL}/me/calendars/{self.config.calendar_id}/events/{event_id}"
            else:
                url = f"{self.GRAPH_URL}/me/calendar/events/{event_id}"

            async with session.delete(url, headers=self._get_headers()) as response:
                if response.status in (200, 204):
                    logger.info(f"Evento deletado: {event_id}")
                    return True
                elif response.status == 404:
                    logger.warning(f"Evento nao encontrado: {event_id}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao deletar evento: {error}")
        except Exception as e:
            logger.error(f"Erro ao deletar evento {event_id}: {e}")

        return False

    async def get_event(self, event_id: str) -> Optional[CalendarEvent]:
        """Busca um evento especifico"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()

            if self.config.calendar_id:
                url = f"{self.GRAPH_URL}/me/calendars/{self.config.calendar_id}/events/{event_id}"
            else:
                url = f"{self.GRAPH_URL}/me/calendar/events/{event_id}"

            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    data = await response.json()
                    return self._outlook_event_to_calendar_event(data)
                elif response.status == 404:
                    logger.warning(f"Evento nao encontrado: {event_id}")
        except Exception as e:
            logger.error(f"Erro ao buscar evento {event_id}: {e}")

        return None

    async def list_events(
        self,
        start_date: datetime,
        end_date: datetime,
        max_results: int = 100
    ) -> List[CalendarEvent]:
        """Lista eventos em um periodo"""
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = self._get_calendar_url()

            params = {
                "$filter": f"start/dateTime ge '{start_date.isoformat()}' and end/dateTime le '{end_date.isoformat()}'",
                "$top": max_results,
                "$orderby": "start/dateTime"
            }

            async with session.get(url, headers=self._get_headers(), params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    events = []
                    for item in data.get("value", []):
                        event = self._outlook_event_to_calendar_event(item)
                        if event:
                            events.append(event)
                    return events
        except Exception as e:
            logger.error(f"Erro ao listar eventos: {e}")

        return []

    def _event_to_outlook_format(self, event: CalendarEvent) -> Dict[str, Any]:
        """Converte CalendarEvent para formato Microsoft Graph"""
        body = {
            "subject": event.title,
            "body": {
                "contentType": "text",
                "content": event.description or ""
            },
            "location": {
                "displayName": event.location or ""
            }
        }

        if event.is_all_day:
            body["isAllDay"] = True
            body["start"] = {
                "dateTime": event.start_time.strftime("%Y-%m-%dT00:00:00"),
                "timeZone": "UTC"
            }
            body["end"] = {
                "dateTime": event.end_time.strftime("%Y-%m-%dT00:00:00"),
                "timeZone": "UTC"
            }
        else:
            body["start"] = {
                "dateTime": event.start_time.isoformat(),
                "timeZone": "UTC"
            }
            body["end"] = {
                "dateTime": event.end_time.isoformat(),
                "timeZone": "UTC"
            }

        if event.attendees:
            body["attendees"] = [
                {
                    "emailAddress": {"address": email},
                    "type": "required"
                }
                for email in event.attendees
            ]

        if event.reminder_minutes > 0:
            body["reminderMinutesBeforeStart"] = event.reminder_minutes

        if event.recurrence:
            # Parse recurrence pattern - simplified
            body["recurrence"] = self._parse_recurrence(event.recurrence)

        return body

    def _parse_recurrence(self, recurrence: str) -> Optional[Dict]:
        """Converte string de recorrencia para formato Outlook"""
        # Implementacao simplificada - suporta padroes basicos
        # RRULE:FREQ=DAILY;COUNT=10
        if not recurrence or not recurrence.startswith("RRULE:"):
            return None

        parts = recurrence.replace("RRULE:", "").split(";")
        pattern = {}
        range_info = {"type": "noEnd"}

        for part in parts:
            key, value = part.split("=")
            if key == "FREQ":
                pattern["type"] = value.lower()
                pattern["interval"] = 1
            elif key == "COUNT":
                range_info = {"type": "numbered", "numberOfOccurrences": int(value)}
            elif key == "INTERVAL":
                pattern["interval"] = int(value)

        if pattern:
            return {
                "pattern": pattern,
                "range": range_info
            }
        return None

    def _outlook_event_to_calendar_event(self, data: Dict) -> Optional[CalendarEvent]:
        """Converte evento Outlook para CalendarEvent"""
        try:
            start = data.get("start", {})
            end = data.get("end", {})
            is_all_day = data.get("isAllDay", False)

            start_str = start.get("dateTime")
            end_str = end.get("dateTime")

            if start_str:
                start_time = datetime.fromisoformat(start_str.replace("Z", "+00:00"))
            else:
                start_time = datetime.now()

            if end_str:
                end_time = datetime.fromisoformat(end_str.replace("Z", "+00:00"))
            else:
                end_time = start_time + timedelta(hours=1)

            attendees = [
                a.get("emailAddress", {}).get("address")
                for a in data.get("attendees", [])
                if a.get("emailAddress", {}).get("address")
            ]

            body = data.get("body", {})
            description = body.get("content", "") if isinstance(body, dict) else ""

            location = data.get("location", {})
            location_name = location.get("displayName", "") if isinstance(location, dict) else ""

            return CalendarEvent(
                title=data.get("subject", ""),
                start_time=start_time,
                end_time=end_time,
                description=description,
                location=location_name,
                attendees=attendees,
                is_all_day=is_all_day,
                external_id=data.get("id"),
                reminder_minutes=data.get("reminderMinutesBeforeStart", 15)
            )
        except Exception as e:
            logger.error(f"Erro ao converter evento Outlook: {e}")
            return None


# =============================================================================
# Factory e Helpers
# =============================================================================

_google_instance: Optional[GoogleCalendarProvider] = None
_outlook_instance: Optional[OutlookCalendarProvider] = None


def get_google_calendar() -> GoogleCalendarProvider:
    """Retorna instancia global do Google Calendar provider"""
    global _google_instance
    if _google_instance is None:
        config = GoogleCalendarConfig.from_env()
        _google_instance = GoogleCalendarProvider(config)
    return _google_instance


def get_outlook_calendar() -> OutlookCalendarProvider:
    """Retorna instancia global do Outlook Calendar provider"""
    global _outlook_instance
    if _outlook_instance is None:
        config = OutlookCalendarConfig.from_env()
        _outlook_instance = OutlookCalendarProvider(config)
    return _outlook_instance


def get_calendar_provider(provider: str = "auto") -> Optional[CalendarProvider]:
    """
    Retorna o provedor de calendario apropriado.

    Args:
        provider: "google", "outlook" ou "auto" (detecta baseado em config)

    Returns:
        CalendarProvider configurado ou None
    """
    if provider == "google":
        return get_google_calendar()
    elif provider == "outlook":
        return get_outlook_calendar()
    elif provider == "auto":
        # Tenta Google primeiro, depois Outlook
        google = get_google_calendar()
        if google.config.is_valid():
            return google
        outlook = get_outlook_calendar()
        if outlook.config.is_valid():
            return outlook
    return None


async def init_calendar_integration(provider: str = "auto") -> Optional[CalendarProvider]:
    """
    Inicializa e conecta a integracao de calendario.

    Args:
        provider: "google", "outlook" ou "auto"

    Returns:
        CalendarProvider conectado ou None
    """
    calendar = get_calendar_provider(provider)
    if calendar and calendar.config.enabled:
        if await calendar.connect():
            return calendar
    return None
