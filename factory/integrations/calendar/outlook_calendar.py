# -*- coding: utf-8 -*-
"""
Outlook Calendar Integration (Issue #264)
==========================================
Cliente para integracao com Microsoft Outlook Calendar via Graph API.

Funcionalidades:
- Autenticacao OAuth 2.0 via Azure AD
- Criar/atualizar/deletar eventos
- Sincronizar sprints como eventos
- Adicionar deadlines de stories
- Suporte a reunioes Teams

Configuracao via variaveis de ambiente:
- AZURE_TENANT_ID: ID do tenant Azure AD
- AZURE_CLIENT_ID: ID da aplicacao registrada
- AZURE_CLIENT_SECRET: Secret da aplicacao
- OUTLOOK_CALENDAR_USER: Email do usuario do calendario
- OUTLOOK_CALENDAR_ENABLED: true/false

Permissoes necessarias no Azure AD:
- Calendars.ReadWrite
- User.Read
"""

import os
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

from ..base import IntegrationStatus

logger = logging.getLogger(__name__)

# Importacoes opcionais
try:
    import msal
    MSAL_AVAILABLE = True
except ImportError:
    MSAL_AVAILABLE = False
    msal = None

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    aiohttp = None


class OutlookEventCategory(str, Enum):
    """Categorias de eventos no Outlook"""
    RED = "Red category"
    ORANGE = "Orange category"
    YELLOW = "Yellow category"
    GREEN = "Green category"
    BLUE = "Blue category"
    PURPLE = "Purple category"


class OutlookEventImportance(str, Enum):
    """Importancia de eventos"""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"


class OutlookShowAs(str, Enum):
    """Status de disponibilidade"""
    FREE = "free"
    TENTATIVE = "tentative"
    BUSY = "busy"
    OOF = "oof"
    WORKING_ELSEWHERE = "workingElsewhere"


# Mapeamento de cores por tipo de evento
EVENT_CATEGORY_MAPPING = {
    "sprint": OutlookEventCategory.BLUE,
    "deadline": OutlookEventCategory.RED,
    "milestone": OutlookEventCategory.PURPLE,
    "meeting": OutlookEventCategory.GREEN,
    "review": OutlookEventCategory.ORANGE,
}


@dataclass
class OutlookCalendarConfig:
    """Configuracao para Outlook Calendar via Microsoft Graph"""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    user_email: str = ""
    enabled: bool = False
    scopes: List[str] = field(default_factory=lambda: [
        "https://graph.microsoft.com/.default"
    ])
    timezone: str = "America/Sao_Paulo"

    @classmethod
    def from_env(cls) -> "OutlookCalendarConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            tenant_id=os.getenv("AZURE_TENANT_ID", ""),
            client_id=os.getenv("AZURE_CLIENT_ID", ""),
            client_secret=os.getenv("AZURE_CLIENT_SECRET", ""),
            user_email=os.getenv("OUTLOOK_CALENDAR_USER", os.getenv("GRAPH_SENDER_EMAIL", "")),
            enabled=os.getenv("OUTLOOK_CALENDAR_ENABLED", "false").lower() == "true",
            timezone=os.getenv("OUTLOOK_CALENDAR_TIMEZONE", "America/Sao_Paulo")
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(
            self.tenant_id and
            self.client_id and
            self.client_secret and
            self.user_email
        )


@dataclass
class OutlookCalendarEvent:
    """Representa um evento do Outlook Calendar"""
    subject: str
    start: datetime
    end: datetime
    body: str = ""
    body_type: str = "HTML"
    location: str = ""
    attendees: List[str] = field(default_factory=list)
    categories: List[str] = field(default_factory=list)
    importance: OutlookEventImportance = OutlookEventImportance.NORMAL
    show_as: OutlookShowAs = OutlookShowAs.BUSY
    is_online_meeting: bool = False
    is_all_day: bool = False
    reminder_minutes: int = 15
    recurrence: Optional[Dict] = None
    event_type: str = "default"
    external_id: Optional[str] = None  # ID interno (sprint_id, story_id)

    def to_graph_format(self, timezone: str = "America/Sao_Paulo") -> Dict[str, Any]:
        """Converte para formato Microsoft Graph API"""
        event = {
            "subject": self.subject,
            "body": {
                "contentType": self.body_type,
                "content": self.body
            },
            "start": {
                "dateTime": self.start.strftime("%Y-%m-%dT%H:%M:%S"),
                "timeZone": timezone
            },
            "end": {
                "dateTime": self.end.strftime("%Y-%m-%dT%H:%M:%S"),
                "timeZone": timezone
            },
            "importance": self.importance.value,
            "showAs": self.show_as.value,
            "isAllDay": self.is_all_day,
            "reminderMinutesBeforeStart": self.reminder_minutes,
            "isReminderOn": True
        }

        if self.location:
            event["location"] = {"displayName": self.location}

        if self.attendees:
            event["attendees"] = [
                {
                    "emailAddress": {"address": email},
                    "type": "required"
                }
                for email in self.attendees
            ]

        if self.categories:
            event["categories"] = self.categories

        if self.is_online_meeting:
            event["isOnlineMeeting"] = True
            event["onlineMeetingProvider"] = "teamsForBusiness"

        if self.recurrence:
            event["recurrence"] = self.recurrence

        # Armazena ID externo em single value extended property
        if self.external_id:
            event["singleValueExtendedProperties"] = [
                {
                    "id": "String {66f5a359-4659-4830-9070-00047ec6ac6e} Name ExternalId",
                    "value": self.external_id
                },
                {
                    "id": "String {66f5a359-4659-4830-9070-00047ec6ac6e} Name EventType",
                    "value": self.event_type
                },
                {
                    "id": "String {66f5a359-4659-4830-9070-00047ec6ac6e} Name Source",
                    "value": "fabrica_agentes"
                }
            ]

        return event


class OutlookCalendarClient:
    """
    Cliente para Microsoft Outlook Calendar via Graph API.

    Suporta:
    - Autenticacao OAuth 2.0 via Azure AD
    - Criacao/atualizacao/delecao de eventos
    - Sincronizacao de sprints e deadlines
    - Reunioes Teams integradas

    Exemplo de uso:
        config = OutlookCalendarConfig.from_env()
        client = OutlookCalendarClient(config)

        if await client.connect():
            event = OutlookCalendarEvent(
                subject="Sprint 1",
                start=datetime.now(),
                end=datetime.now() + timedelta(weeks=2)
            )
            await client.create_event(event)
    """

    GRAPH_BASE_URL = "https://graph.microsoft.com/v1.0"
    AUTHORITY_BASE = "https://login.microsoftonline.com"

    def __init__(self, config: OutlookCalendarConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._access_token: Optional[str] = None
        self._token_expires: Optional[datetime] = None
        self._msal_app: Optional[Any] = None
        self._session: Optional[Any] = None
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado e com token valido"""
        if self.status != IntegrationStatus.CONNECTED:
            return False
        if not self._access_token:
            return False
        if self._token_expires and datetime.utcnow() >= self._token_expires:
            return False
        return True

    @property
    def last_error(self) -> Optional[str]:
        """Retorna ultimo erro"""
        return self._last_error

    def _check_dependencies(self) -> bool:
        """Verifica se dependencias estao instaladas"""
        if not MSAL_AVAILABLE:
            self._last_error = "Biblioteca 'msal' nao instalada. Execute: pip install msal"
            return False
        if not AIOHTTP_AVAILABLE:
            self._last_error = "Biblioteca 'aiohttp' nao instalada. Execute: pip install aiohttp"
            return False
        return True

    async def connect(self) -> bool:
        """
        Conecta ao Microsoft Graph via OAuth 2.0.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self._check_dependencies():
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique tenant_id, client_id, client_secret e user_email."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info("Conectando ao Outlook Calendar...")

        try:
            authority = f"{self.AUTHORITY_BASE}/{self.config.tenant_id}"

            self._msal_app = msal.ConfidentialClientApplication(
                client_id=self.config.client_id,
                client_credential=self.config.client_secret,
                authority=authority
            )

            result = self._msal_app.acquire_token_for_client(
                scopes=self.config.scopes
            )

            if "access_token" in result:
                self._access_token = result["access_token"]
                expires_in = result.get("expires_in", 3600)
                self._token_expires = datetime.utcnow() + timedelta(seconds=expires_in - 300)

                self.status = IntegrationStatus.CONNECTED
                logger.info("Conectado ao Outlook Calendar com sucesso")
                return True
            else:
                self._last_error = result.get("error_description", "Erro desconhecido na autenticacao")
                self.status = IntegrationStatus.ERROR
                logger.error(f"Falha na autenticacao: {self._last_error}")
                return False

        except Exception as e:
            self._last_error = f"Erro ao conectar: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.exception("Erro ao conectar ao Outlook Calendar")
            return False

    async def _ensure_token(self) -> bool:
        """Garante que o token esta valido, renovando se necessario"""
        if self._access_token and self._token_expires:
            if datetime.utcnow() < self._token_expires:
                return True
        return await self.connect()

    async def _ensure_session(self):
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
            "Prefer": 'outlook.timezone="America/Sao_Paulo"'
        }

    async def disconnect(self) -> bool:
        """
        Desconecta do Outlook Calendar.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._access_token = None
        self._token_expires = None
        self._msal_app = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Outlook Calendar")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com Outlook Calendar.

        Returns:
            bool: True se a conexao esta funcionando
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar"

            async with session.get(url, headers=self._get_headers()) as response:
                return response.status == 200

        except Exception as e:
            logger.error(f"Erro ao testar conexao: {e}")
            return False

    async def create_event(self, event: OutlookCalendarEvent) -> Optional[Dict[str, Any]]:
        """
        Cria um evento no calendario.

        Args:
            event: Evento a criar

        Returns:
            Dict com evento criado ou None
        """
        if not await self._ensure_token():
            self._last_error = "Nao conectado ao Outlook Calendar"
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar/events"

            async with session.post(
                url,
                headers=self._get_headers(),
                json=event.to_graph_format(self.config.timezone)
            ) as response:
                if response.status == 201:
                    event_data = await response.json()
                    logger.info(f"Evento criado: {event.subject} (ID: {event_data.get('id')})")
                    return event_data
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"
                    logger.error(f"Falha ao criar evento: {self._last_error}")
                    return None

        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao criar evento")
            return None

    async def update_event(
        self,
        event_id: str,
        event: OutlookCalendarEvent
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza um evento existente.

        Args:
            event_id: ID do evento no Outlook
            event: Dados atualizados do evento

        Returns:
            Dict com evento atualizado ou None
        """
        if not await self._ensure_token():
            self._last_error = "Nao conectado ao Outlook Calendar"
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar/events/{event_id}"

            async with session.patch(
                url,
                headers=self._get_headers(),
                json=event.to_graph_format(self.config.timezone)
            ) as response:
                if response.status == 200:
                    event_data = await response.json()
                    logger.info(f"Evento atualizado: {event.subject}")
                    return event_data
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"
                    logger.error(f"Falha ao atualizar evento: {self._last_error}")
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
        if not await self._ensure_token():
            self._last_error = "Nao conectado ao Outlook Calendar"
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar/events/{event_id}"

            async with session.delete(url, headers=self._get_headers()) as response:
                if response.status == 204:
                    logger.info(f"Evento deletado: {event_id}")
                    return True
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"
                    logger.error(f"Falha ao deletar evento: {self._last_error}")
                    return False

        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao deletar evento")
            return False

    async def get_events(
        self,
        start_datetime: Optional[datetime] = None,
        end_datetime: Optional[datetime] = None,
        top: int = 100,
        order_by: str = "start/dateTime"
    ) -> List[Dict[str, Any]]:
        """
        Lista eventos do calendario.

        Args:
            start_datetime: Data/hora inicial
            end_datetime: Data/hora final
            top: Numero maximo de eventos
            order_by: Campo para ordenacao

        Returns:
            List[Dict]: Lista de eventos
        """
        if not await self._ensure_token():
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar/events"

            params = {
                "$top": top,
                "$orderby": order_by
            }

            if start_datetime and end_datetime:
                params["$filter"] = (
                    f"start/dateTime ge '{start_datetime.isoformat()}' and "
                    f"end/dateTime le '{end_datetime.isoformat()}'"
                )

            async with session.get(
                url,
                headers=self._get_headers(),
                params=params
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("value", [])
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
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.user_email}/calendar/events"

            # Busca usando extended property
            params = {
                "$filter": f"singleValueExtendedProperties/Any(ep: ep/id eq 'String {{66f5a359-4659-4830-9070-00047ec6ac6e}} Name ExternalId' and ep/value eq '{external_id}')",
                "$top": 1
            }

            async with session.get(
                url,
                headers=self._get_headers(),
                params=params
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    items = data.get("value", [])
                    return items[0] if items else None
                return None

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
        event = OutlookCalendarEvent(
            subject=f"[Sprint] {name}",
            start=start_date,
            end=end_date,
            body=f"<h2>Sprint Goal</h2><p>{goal}</p><hr><p><em>Fabrica de Agentes - Sprint Tracking</em></p>",
            categories=[EVENT_CATEGORY_MAPPING.get("sprint", OutlookEventCategory.BLUE).value],
            importance=OutlookEventImportance.HIGH,
            show_as=OutlookShowAs.BUSY,
            is_all_day=True,
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
        importance = OutlookEventImportance.NORMAL
        if priority in ["high", "urgent"]:
            importance = OutlookEventImportance.HIGH
        elif priority == "low":
            importance = OutlookEventImportance.LOW

        event = OutlookCalendarEvent(
            subject=f"[Deadline] {title}",
            start=deadline.replace(hour=9, minute=0),
            end=deadline.replace(hour=10, minute=0),
            body=f"<h3>Priority: {priority.upper()}</h3><p>{description}</p><hr><p><em>Fabrica de Agentes</em></p>",
            categories=[EVENT_CATEGORY_MAPPING.get("deadline", OutlookEventCategory.RED).value],
            importance=importance,
            show_as=OutlookShowAs.TENTATIVE,
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
            "system": "outlook_calendar",
            "status": self.status.value,
            "connected": self.is_connected,
            "user_email": self.config.user_email if self.is_connected else None,
            "last_error": self._last_error,
            "enabled": self.config.enabled,
            "token_expires": self._token_expires.isoformat() if self._token_expires else None
        }


# =============================================================================
# INSTANCIA GLOBAL (SINGLETON)
# =============================================================================

_outlook_calendar_instance: Optional[OutlookCalendarClient] = None


def get_outlook_calendar_client() -> OutlookCalendarClient:
    """Retorna instancia global do cliente Outlook Calendar"""
    global _outlook_calendar_instance
    if _outlook_calendar_instance is None:
        config = OutlookCalendarConfig.from_env()
        _outlook_calendar_instance = OutlookCalendarClient(config)
    return _outlook_calendar_instance


async def init_outlook_calendar() -> Optional[OutlookCalendarClient]:
    """Inicializa e conecta o cliente Outlook Calendar se configurado"""
    client = get_outlook_calendar_client()
    if client.config.is_valid() and client.config.enabled:
        if await client.connect():
            return client
    return None
