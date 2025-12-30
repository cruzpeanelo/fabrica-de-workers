# -*- coding: utf-8 -*-
"""
Google Calendar Integration (Issue #264)
=========================================
Cliente para integracao com Google Calendar API.

Funcionalidades:
- Autenticacao OAuth 2.0
- Criar/atualizar/deletar eventos
- Sincronizar sprints como eventos
- Adicionar deadlines de stories

Configuracao via variaveis de ambiente:
- GOOGLE_CALENDAR_CREDENTIALS: Path para credentials.json
- GOOGLE_CALENDAR_TOKEN: Path para token.json
- GOOGLE_CALENDAR_ID: ID do calendario (default: primary)
- GOOGLE_CALENDAR_ENABLED: true/false

Permissoes necessarias no Google Cloud Console:
- https://www.googleapis.com/auth/calendar
- https://www.googleapis.com/auth/calendar.events
"""

import os
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from pathlib import Path
from enum import Enum

from ..base import IntegrationStatus

logger = logging.getLogger(__name__)

# Importacoes opcionais do Google API
try:
    from google.oauth2.credentials import Credentials
    from google_auth_oauthlib.flow import InstalledAppFlow
    from google.auth.transport.requests import Request
    from googleapiclient.discovery import build
    from googleapiclient.errors import HttpError
    GOOGLE_API_AVAILABLE = True
except ImportError:
    GOOGLE_API_AVAILABLE = False
    Credentials = None
    InstalledAppFlow = None
    Request = None
    build = None
    HttpError = None


class GoogleCalendarEventColor(str, Enum):
    """Cores disponiveis para eventos no Google Calendar"""
    LAVENDER = "1"
    SAGE = "2"
    GRAPE = "3"
    FLAMINGO = "4"
    BANANA = "5"
    TANGERINE = "6"
    PEACOCK = "7"
    GRAPHITE = "8"
    BLUEBERRY = "9"
    BASIL = "10"
    TOMATO = "11"


# Mapeamento de cores por tipo de evento
EVENT_COLOR_MAPPING = {
    "sprint": GoogleCalendarEventColor.PEACOCK,      # Azul
    "deadline": GoogleCalendarEventColor.TOMATO,     # Vermelho
    "milestone": GoogleCalendarEventColor.GRAPE,     # Roxo
    "meeting": GoogleCalendarEventColor.SAGE,        # Verde
    "review": GoogleCalendarEventColor.TANGERINE,    # Laranja
}


@dataclass
class GoogleCalendarConfig:
    """Configuracao para Google Calendar"""
    credentials_path: str = ""
    token_path: str = ""
    calendar_id: str = "primary"
    enabled: bool = False
    scopes: List[str] = field(default_factory=lambda: [
        "https://www.googleapis.com/auth/calendar",
        "https://www.googleapis.com/auth/calendar.events"
    ])
    timezone: str = "America/Sao_Paulo"

    @classmethod
    def from_env(cls) -> "GoogleCalendarConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            credentials_path=os.getenv("GOOGLE_CALENDAR_CREDENTIALS", "credentials.json"),
            token_path=os.getenv("GOOGLE_CALENDAR_TOKEN", "token.json"),
            calendar_id=os.getenv("GOOGLE_CALENDAR_ID", "primary"),
            enabled=os.getenv("GOOGLE_CALENDAR_ENABLED", "false").lower() == "true",
            timezone=os.getenv("GOOGLE_CALENDAR_TIMEZONE", "America/Sao_Paulo")
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(
            self.credentials_path and
            Path(self.credentials_path).exists()
        )


@dataclass
class GoogleCalendarEvent:
    """Representa um evento do Google Calendar"""
    summary: str
    start: datetime
    end: datetime
    description: str = ""
    location: str = ""
    attendees: List[str] = field(default_factory=list)
    color_id: Optional[str] = None
    reminders: List[Dict[str, Any]] = field(default_factory=list)
    recurrence: Optional[List[str]] = None
    event_type: str = "default"
    external_id: Optional[str] = None  # ID interno (sprint_id, story_id)

    def to_google_format(self, timezone: str = "America/Sao_Paulo") -> Dict[str, Any]:
        """Converte para formato Google Calendar API"""
        event = {
            "summary": self.summary,
            "description": self.description,
            "start": {
                "dateTime": self.start.isoformat(),
                "timeZone": timezone
            },
            "end": {
                "dateTime": self.end.isoformat(),
                "timeZone": timezone
            }
        }

        if self.location:
            event["location"] = self.location

        if self.attendees:
            event["attendees"] = [{"email": email} for email in self.attendees]

        if self.color_id:
            event["colorId"] = self.color_id

        if self.reminders:
            event["reminders"] = {
                "useDefault": False,
                "overrides": self.reminders
            }
        else:
            # Lembrete padrao: 1 dia e 1 hora antes
            event["reminders"] = {
                "useDefault": False,
                "overrides": [
                    {"method": "popup", "minutes": 60},
                    {"method": "email", "minutes": 1440}
                ]
            }

        if self.recurrence:
            event["recurrence"] = self.recurrence

        # Armazena ID externo em propriedades estendidas
        if self.external_id:
            event["extendedProperties"] = {
                "private": {
                    "externalId": self.external_id,
                    "eventType": self.event_type,
                    "source": "fabrica_agentes"
                }
            }

        return event


class GoogleCalendarClient:
    """
    Cliente para Google Calendar API.

    Suporta:
    - Autenticacao OAuth 2.0
    - Criacao/atualizacao/delecao de eventos
    - Sincronizacao de sprints e deadlines
    - Cores por tipo de evento

    Exemplo de uso:
        config = GoogleCalendarConfig.from_env()
        client = GoogleCalendarClient(config)

        if await client.connect():
            event = GoogleCalendarEvent(
                summary="Sprint 1",
                start=datetime.now(),
                end=datetime.now() + timedelta(weeks=2)
            )
            await client.create_event(event)
    """

    def __init__(self, config: GoogleCalendarConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._credentials: Optional[Any] = None
        self._service: Optional[Any] = None
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self.status == IntegrationStatus.CONNECTED and self._service is not None

    @property
    def last_error(self) -> Optional[str]:
        """Retorna ultimo erro"""
        return self._last_error

    def _check_dependencies(self) -> bool:
        """Verifica se dependencias estao instaladas"""
        if not GOOGLE_API_AVAILABLE:
            self._last_error = (
                "Bibliotecas Google nao instaladas. Execute: "
                "pip install google-auth-oauthlib google-auth-httplib2 google-api-python-client"
            )
            return False
        return True

    async def connect(self) -> bool:
        """
        Conecta ao Google Calendar via OAuth 2.0.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self._check_dependencies():
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique credentials_path."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info("Conectando ao Google Calendar...")

        try:
            creds = None
            token_path = Path(self.config.token_path)

            # Tenta carregar token existente
            if token_path.exists():
                creds = Credentials.from_authorized_user_file(
                    str(token_path),
                    self.config.scopes
                )

            # Se nao ha credenciais validas, faz o fluxo OAuth
            if not creds or not creds.valid:
                if creds and creds.expired and creds.refresh_token:
                    creds.refresh(Request())
                else:
                    flow = InstalledAppFlow.from_client_secrets_file(
                        self.config.credentials_path,
                        self.config.scopes
                    )
                    creds = flow.run_local_server(port=0)

                # Salva as credenciais
                with open(token_path, "w") as token:
                    token.write(creds.to_json())

            self._credentials = creds
            self._service = build("calendar", "v3", credentials=creds)

            self.status = IntegrationStatus.CONNECTED
            logger.info("Conectado ao Google Calendar com sucesso")
            return True

        except Exception as e:
            self._last_error = f"Erro ao conectar: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.exception("Erro ao conectar ao Google Calendar")
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do Google Calendar.

        Returns:
            bool: True se desconectado com sucesso
        """
        self._credentials = None
        self._service = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Google Calendar")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com Google Calendar.

        Returns:
            bool: True se a conexao esta funcionando
        """
        if not self.is_connected:
            return False

        try:
            # Tenta listar calendarios
            self._service.calendarList().list(maxResults=1).execute()
            return True
        except Exception as e:
            logger.error(f"Erro ao testar conexao: {e}")
            return False

    async def create_event(self, event: GoogleCalendarEvent) -> Optional[Dict[str, Any]]:
        """
        Cria um evento no calendario.

        Args:
            event: Evento a criar

        Returns:
            Dict com evento criado ou None
        """
        if not self.is_connected:
            self._last_error = "Nao conectado ao Google Calendar"
            return None

        try:
            created_event = self._service.events().insert(
                calendarId=self.config.calendar_id,
                body=event.to_google_format(self.config.timezone)
            ).execute()

            logger.info(f"Evento criado: {event.summary} (ID: {created_event.get('id')})")
            return created_event

        except HttpError as e:
            self._last_error = f"Erro ao criar evento: {e}"
            logger.error(self._last_error)
            return None
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao criar evento")
            return None

    async def update_event(
        self,
        event_id: str,
        event: GoogleCalendarEvent
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza um evento existente.

        Args:
            event_id: ID do evento no Google Calendar
            event: Dados atualizados do evento

        Returns:
            Dict com evento atualizado ou None
        """
        if not self.is_connected:
            self._last_error = "Nao conectado ao Google Calendar"
            return None

        try:
            updated_event = self._service.events().update(
                calendarId=self.config.calendar_id,
                eventId=event_id,
                body=event.to_google_format(self.config.timezone)
            ).execute()

            logger.info(f"Evento atualizado: {event.summary}")
            return updated_event

        except HttpError as e:
            self._last_error = f"Erro ao atualizar evento: {e}"
            logger.error(self._last_error)
            return None
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao atualizar evento")
            return None

    async def delete_event(self, event_id: str) -> bool:
        """
        Deleta um evento do calendario.

        Args:
            event_id: ID do evento

        Returns:
            bool: True se deletado
        """
        if not self.is_connected:
            self._last_error = "Nao conectado ao Google Calendar"
            return False

        try:
            self._service.events().delete(
                calendarId=self.config.calendar_id,
                eventId=event_id
            ).execute()

            logger.info(f"Evento deletado: {event_id}")
            return True

        except HttpError as e:
            self._last_error = f"Erro ao deletar evento: {e}"
            logger.error(self._last_error)
            return False
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao deletar evento")
            return False

    async def get_events(
        self,
        time_min: Optional[datetime] = None,
        time_max: Optional[datetime] = None,
        max_results: int = 100,
        single_events: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Lista eventos do calendario.

        Args:
            time_min: Data/hora minima
            time_max: Data/hora maxima
            max_results: Numero maximo de eventos
            single_events: Expandir eventos recorrentes

        Returns:
            List[Dict]: Lista de eventos
        """
        if not self.is_connected:
            return []

        try:
            if not time_min:
                time_min = datetime.utcnow()
            if not time_max:
                time_max = time_min + timedelta(days=90)

            events_result = self._service.events().list(
                calendarId=self.config.calendar_id,
                timeMin=time_min.isoformat() + "Z",
                timeMax=time_max.isoformat() + "Z",
                maxResults=max_results,
                singleEvents=single_events,
                orderBy="startTime"
            ).execute()

            return events_result.get("items", [])

        except HttpError as e:
            logger.error(f"Erro ao listar eventos: {e}")
            return []
        except Exception as e:
            logger.exception(f"Erro ao listar eventos: {e}")
            return []

    async def find_event_by_external_id(self, external_id: str) -> Optional[Dict[str, Any]]:
        """
        Busca evento por ID externo (sprint_id, story_id).

        Args:
            external_id: ID externo

        Returns:
            Dict com evento ou None
        """
        if not self.is_connected:
            return None

        try:
            # Busca eventos com propriedade estendida
            events_result = self._service.events().list(
                calendarId=self.config.calendar_id,
                privateExtendedProperty=f"externalId={external_id}",
                maxResults=1
            ).execute()

            items = events_result.get("items", [])
            return items[0] if items else None

        except Exception as e:
            logger.error(f"Erro ao buscar evento: {e}")
            return None

    async def sync_sprint(
        self,
        sprint_id: str,
        name: str,
        start_date: datetime,
        end_date: datetime,
        goal: str = "",
        attendees: List[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Sincroniza um sprint com o calendario.
        Cria ou atualiza evento existente.

        Args:
            sprint_id: ID do sprint
            name: Nome do sprint
            start_date: Data de inicio
            end_date: Data de fim
            goal: Objetivo do sprint
            attendees: Participantes

        Returns:
            Dict com evento criado/atualizado ou None
        """
        event = GoogleCalendarEvent(
            summary=f"[Sprint] {name}",
            start=start_date,
            end=end_date,
            description=f"Goal: {goal}\n\nFabrica de Agentes - Sprint Tracking",
            color_id=EVENT_COLOR_MAPPING.get("sprint", GoogleCalendarEventColor.PEACOCK).value,
            attendees=attendees or [],
            event_type="sprint",
            external_id=sprint_id
        )

        # Verifica se evento ja existe
        existing = await self.find_event_by_external_id(sprint_id)
        if existing:
            return await self.update_event(existing["id"], event)
        else:
            return await self.create_event(event)

    async def sync_deadline(
        self,
        story_id: str,
        title: str,
        deadline: datetime,
        description: str = "",
        priority: str = "medium"
    ) -> Optional[Dict[str, Any]]:
        """
        Sincroniza deadline de story com o calendario.

        Args:
            story_id: ID da story
            title: Titulo da story
            deadline: Data de deadline
            description: Descricao
            priority: Prioridade (low, medium, high, urgent)

        Returns:
            Dict com evento criado/atualizado ou None
        """
        # Evento de 1 hora no dia do deadline
        event = GoogleCalendarEvent(
            summary=f"[Deadline] {title}",
            start=deadline.replace(hour=9, minute=0),
            end=deadline.replace(hour=10, minute=0),
            description=f"Priority: {priority}\n\n{description}\n\nFabrica de Agentes",
            color_id=EVENT_COLOR_MAPPING.get("deadline", GoogleCalendarEventColor.TOMATO).value,
            event_type="deadline",
            external_id=story_id
        )

        # Verifica se evento ja existe
        existing = await self.find_event_by_external_id(story_id)
        if existing:
            return await self.update_event(existing["id"], event)
        else:
            return await self.create_event(event)

    async def remove_synced_event(self, external_id: str) -> bool:
        """
        Remove evento sincronizado pelo ID externo.

        Args:
            external_id: ID externo (sprint_id, story_id)

        Returns:
            bool: True se removido
        """
        existing = await self.find_event_by_external_id(external_id)
        if existing:
            return await self.delete_event(existing["id"])
        return True

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "system": "google_calendar",
            "status": self.status.value,
            "connected": self.is_connected,
            "calendar_id": self.config.calendar_id if self.is_connected else None,
            "last_error": self._last_error,
            "enabled": self.config.enabled
        }


# =============================================================================
# INSTANCIA GLOBAL (SINGLETON)
# =============================================================================

_google_calendar_instance: Optional[GoogleCalendarClient] = None


def get_google_calendar_client() -> GoogleCalendarClient:
    """Retorna instancia global do cliente Google Calendar"""
    global _google_calendar_instance
    if _google_calendar_instance is None:
        config = GoogleCalendarConfig.from_env()
        _google_calendar_instance = GoogleCalendarClient(config)
    return _google_calendar_instance


async def init_google_calendar() -> Optional[GoogleCalendarClient]:
    """Inicializa e conecta o cliente Google Calendar se configurado"""
    client = get_google_calendar_client()
    if client.config.is_valid() and client.config.enabled:
        if await client.connect():
            return client
    return None
