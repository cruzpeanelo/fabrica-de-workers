# -*- coding: utf-8 -*-
"""
Calendar Sync Service (Issue #264)
===================================
Servico de sincronizacao entre Plataforma E e calendarios externos.

Funcionalidades:
- Sincronizacao automatica de sprints
- Sincronizacao de deadlines de stories
- Suporte a multiplos provedores (Google/Outlook)
- Sincronizacao bidirecional (futuro)

Uso:
    from factory.integrations.calendar import get_calendar_sync_service

    sync = get_calendar_sync_service()
    await sync.sync_sprint(sprint)
    await sync.sync_story_deadline(story)
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union
from enum import Enum

from .google_calendar import GoogleCalendarClient, get_google_calendar_client
from .outlook_calendar import OutlookCalendarClient, get_outlook_calendar_client
from ..base import SyncResult

logger = logging.getLogger(__name__)


class CalendarEventType(str, Enum):
    """Tipos de eventos de calendario"""
    SPRINT = "sprint"
    DEADLINE = "deadline"
    MILESTONE = "milestone"
    MEETING = "meeting"
    REVIEW = "review"


class CalendarProvider(str, Enum):
    """Provedores de calendario suportados"""
    GOOGLE = "google"
    OUTLOOK = "outlook"
    ALL = "all"


@dataclass
class CalendarEvent:
    """
    Evento de calendario generico.

    Usado para abstrar eventos independente do provedor.
    """
    external_id: str  # ID interno (sprint_id, story_id)
    title: str
    start: datetime
    end: datetime
    event_type: CalendarEventType
    description: str = ""
    location: str = ""
    attendees: List[str] = field(default_factory=list)
    priority: str = "medium"
    all_day: bool = False
    reminder_minutes: int = 15
    metadata: Dict[str, Any] = field(default_factory=dict)


class CalendarSyncService:
    """
    Servico de sincronizacao de calendario.

    Gerencia a sincronizacao de eventos entre a Plataforma E
    e calendarios externos (Google Calendar e Outlook).

    Exemplo:
        sync = CalendarSyncService()
        await sync.initialize()

        # Sincronizar sprint
        await sync.sync_sprint(
            sprint_id="SPR-001",
            name="Sprint 1",
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(weeks=2),
            goal="Completar modulo de autenticacao"
        )

        # Sincronizar deadline
        await sync.sync_deadline(
            story_id="STR-001",
            title="Implementar login",
            deadline=datetime(2024, 2, 15),
            priority="high"
        )
    """

    def __init__(
        self,
        google_client: Optional[GoogleCalendarClient] = None,
        outlook_client: Optional[OutlookCalendarClient] = None
    ):
        """
        Inicializa o servico de sincronizacao.

        Args:
            google_client: Cliente Google Calendar (opcional)
            outlook_client: Cliente Outlook Calendar (opcional)
        """
        self._google_client = google_client
        self._outlook_client = outlook_client
        self._initialized = False
        self._last_sync: Optional[datetime] = None
        self._sync_errors: List[str] = []

    async def initialize(self) -> bool:
        """
        Inicializa os clientes de calendario.

        Tenta conectar aos provedores configurados.

        Returns:
            bool: True se pelo menos um provedor foi conectado
        """
        success_count = 0

        # Inicializa Google Calendar
        if self._google_client is None:
            self._google_client = get_google_calendar_client()

        if self._google_client.config.enabled:
            try:
                if await self._google_client.connect():
                    success_count += 1
                    logger.info("Google Calendar conectado")
            except Exception as e:
                logger.warning(f"Falha ao conectar Google Calendar: {e}")

        # Inicializa Outlook Calendar
        if self._outlook_client is None:
            self._outlook_client = get_outlook_calendar_client()

        if self._outlook_client.config.enabled:
            try:
                if await self._outlook_client.connect():
                    success_count += 1
                    logger.info("Outlook Calendar conectado")
            except Exception as e:
                logger.warning(f"Falha ao conectar Outlook Calendar: {e}")

        self._initialized = success_count > 0
        return self._initialized

    @property
    def is_initialized(self) -> bool:
        """Verifica se o servico esta inicializado"""
        return self._initialized

    @property
    def google_connected(self) -> bool:
        """Verifica se Google Calendar esta conectado"""
        return self._google_client is not None and self._google_client.is_connected

    @property
    def outlook_connected(self) -> bool:
        """Verifica se Outlook Calendar esta conectado"""
        return self._outlook_client is not None and self._outlook_client.is_connected

    async def sync_sprint(
        self,
        sprint_id: str,
        name: str,
        start_date: datetime,
        end_date: datetime,
        goal: str = "",
        attendees: List[str] = None,
        provider: CalendarProvider = CalendarProvider.ALL
    ) -> SyncResult:
        """
        Sincroniza um sprint com os calendarios.

        Args:
            sprint_id: ID do sprint
            name: Nome do sprint
            start_date: Data de inicio
            end_date: Data de fim
            goal: Objetivo do sprint
            attendees: Lista de emails dos participantes
            provider: Provedor(es) de calendario

        Returns:
            SyncResult com resultado da sincronizacao
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        attendees = attendees or []
        sync_google = provider in [CalendarProvider.ALL, CalendarProvider.GOOGLE]
        sync_outlook = provider in [CalendarProvider.ALL, CalendarProvider.OUTLOOK]

        # Sincroniza com Google Calendar
        if sync_google and self.google_connected:
            try:
                event = await self._google_client.sync_sprint(
                    sprint_id=sprint_id,
                    name=name,
                    start_date=start_date,
                    end_date=end_date,
                    goal=goal,
                    attendees=attendees
                )
                if event:
                    result.items_synced += 1
                    logger.info(f"Sprint {sprint_id} sincronizado com Google Calendar")
                else:
                    result.items_failed += 1
                    result.errors.append(f"Google: {self._google_client.last_error}")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Google: {str(e)}")
                logger.error(f"Erro ao sincronizar sprint com Google: {e}")

        # Sincroniza com Outlook Calendar
        if sync_outlook and self.outlook_connected:
            try:
                event = await self._outlook_client.sync_sprint(
                    sprint_id=sprint_id,
                    name=name,
                    start_date=start_date,
                    end_date=end_date,
                    goal=goal,
                    attendees=attendees
                )
                if event:
                    result.items_synced += 1
                    logger.info(f"Sprint {sprint_id} sincronizado com Outlook Calendar")
                else:
                    result.items_failed += 1
                    result.errors.append(f"Outlook: {self._outlook_client.last_error}")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Outlook: {str(e)}")
                logger.error(f"Erro ao sincronizar sprint com Outlook: {e}")

        result.completed_at = datetime.utcnow()
        result.success = result.items_synced > 0 or (not self.google_connected and not self.outlook_connected)

        if result.items_synced == 0 and (self.google_connected or self.outlook_connected):
            result.warnings.append("Nenhum calendario sincronizado com sucesso")

        self._last_sync = datetime.utcnow()
        return result

    async def sync_deadline(
        self,
        story_id: str,
        title: str,
        deadline: datetime,
        description: str = "",
        priority: str = "medium",
        provider: CalendarProvider = CalendarProvider.ALL
    ) -> SyncResult:
        """
        Sincroniza deadline de story com os calendarios.

        Args:
            story_id: ID da story
            title: Titulo da story
            deadline: Data de deadline
            description: Descricao da story
            priority: Prioridade (low, medium, high, urgent)
            provider: Provedor(es) de calendario

        Returns:
            SyncResult com resultado da sincronizacao
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        sync_google = provider in [CalendarProvider.ALL, CalendarProvider.GOOGLE]
        sync_outlook = provider in [CalendarProvider.ALL, CalendarProvider.OUTLOOK]

        # Sincroniza com Google Calendar
        if sync_google and self.google_connected:
            try:
                event = await self._google_client.sync_deadline(
                    story_id=story_id,
                    title=title,
                    deadline=deadline,
                    description=description,
                    priority=priority
                )
                if event:
                    result.items_synced += 1
                    logger.info(f"Deadline {story_id} sincronizado com Google Calendar")
                else:
                    result.items_failed += 1
                    result.errors.append(f"Google: {self._google_client.last_error}")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Google: {str(e)}")
                logger.error(f"Erro ao sincronizar deadline com Google: {e}")

        # Sincroniza com Outlook Calendar
        if sync_outlook and self.outlook_connected:
            try:
                event = await self._outlook_client.sync_deadline(
                    story_id=story_id,
                    title=title,
                    deadline=deadline,
                    description=description,
                    priority=priority
                )
                if event:
                    result.items_synced += 1
                    logger.info(f"Deadline {story_id} sincronizado com Outlook Calendar")
                else:
                    result.items_failed += 1
                    result.errors.append(f"Outlook: {self._outlook_client.last_error}")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Outlook: {str(e)}")
                logger.error(f"Erro ao sincronizar deadline com Outlook: {e}")

        result.completed_at = datetime.utcnow()
        result.success = result.items_synced > 0 or (not self.google_connected and not self.outlook_connected)

        self._last_sync = datetime.utcnow()
        return result

    async def sync_event(
        self,
        event: CalendarEvent,
        provider: CalendarProvider = CalendarProvider.ALL
    ) -> SyncResult:
        """
        Sincroniza um evento generico com os calendarios.

        Args:
            event: Evento a sincronizar
            provider: Provedor(es) de calendario

        Returns:
            SyncResult com resultado da sincronizacao
        """
        if event.event_type == CalendarEventType.SPRINT:
            return await self.sync_sprint(
                sprint_id=event.external_id,
                name=event.title,
                start_date=event.start,
                end_date=event.end,
                goal=event.description,
                attendees=event.attendees,
                provider=provider
            )
        elif event.event_type == CalendarEventType.DEADLINE:
            return await self.sync_deadline(
                story_id=event.external_id,
                title=event.title,
                deadline=event.start,
                description=event.description,
                priority=event.priority,
                provider=provider
            )
        else:
            # Implementar outros tipos de evento conforme necessario
            return SyncResult(
                success=False,
                errors=[f"Tipo de evento nao suportado: {event.event_type}"]
            )

    async def remove_event(
        self,
        external_id: str,
        provider: CalendarProvider = CalendarProvider.ALL
    ) -> SyncResult:
        """
        Remove evento sincronizado dos calendarios.

        Args:
            external_id: ID externo do evento (sprint_id, story_id)
            provider: Provedor(es) de calendario

        Returns:
            SyncResult com resultado da remocao
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        sync_google = provider in [CalendarProvider.ALL, CalendarProvider.GOOGLE]
        sync_outlook = provider in [CalendarProvider.ALL, CalendarProvider.OUTLOOK]

        # Remove do Google Calendar
        if sync_google and self.google_connected:
            try:
                if await self._google_client.remove_synced_event(external_id):
                    result.items_synced += 1
                    logger.info(f"Evento {external_id} removido do Google Calendar")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Google: {str(e)}")

        # Remove do Outlook Calendar
        if sync_outlook and self.outlook_connected:
            try:
                if await self._outlook_client.remove_synced_event(external_id):
                    result.items_synced += 1
                    logger.info(f"Evento {external_id} removido do Outlook Calendar")
            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Outlook: {str(e)}")

        result.completed_at = datetime.utcnow()
        return result

    async def sync_all_sprints(
        self,
        sprints: List[Dict[str, Any]],
        provider: CalendarProvider = CalendarProvider.ALL
    ) -> SyncResult:
        """
        Sincroniza multiplos sprints com os calendarios.

        Args:
            sprints: Lista de sprints (dicionarios com id, name, start_date, end_date, goal)
            provider: Provedor(es) de calendario

        Returns:
            SyncResult agregado
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        for sprint in sprints:
            sprint_result = await self.sync_sprint(
                sprint_id=sprint.get("sprint_id") or sprint.get("id"),
                name=sprint.get("name", "Sprint"),
                start_date=sprint.get("start_date"),
                end_date=sprint.get("end_date"),
                goal=sprint.get("goal", ""),
                provider=provider
            )

            result.items_synced += sprint_result.items_synced
            result.items_failed += sprint_result.items_failed
            result.errors.extend(sprint_result.errors)
            result.warnings.extend(sprint_result.warnings)

        result.completed_at = datetime.utcnow()
        result.success = result.items_failed == 0

        return result

    async def disconnect(self) -> None:
        """Desconecta de todos os calendarios"""
        if self._google_client and self._google_client.is_connected:
            await self._google_client.disconnect()

        if self._outlook_client and self._outlook_client.is_connected:
            await self._outlook_client.disconnect()

        self._initialized = False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status do servico de sincronizacao"""
        return {
            "initialized": self._initialized,
            "google_calendar": self._google_client.get_status() if self._google_client else None,
            "outlook_calendar": self._outlook_client.get_status() if self._outlook_client else None,
            "last_sync": self._last_sync.isoformat() if self._last_sync else None,
            "providers_connected": sum([
                1 if self.google_connected else 0,
                1 if self.outlook_connected else 0
            ])
        }


# =============================================================================
# INSTANCIA GLOBAL (SINGLETON)
# =============================================================================

_calendar_sync_instance: Optional[CalendarSyncService] = None


def get_calendar_sync_service() -> CalendarSyncService:
    """Retorna instancia global do servico de sincronizacao"""
    global _calendar_sync_instance
    if _calendar_sync_instance is None:
        _calendar_sync_instance = CalendarSyncService()
    return _calendar_sync_instance


async def init_calendar_sync() -> Optional[CalendarSyncService]:
    """Inicializa o servico de sincronizacao de calendario"""
    service = get_calendar_sync_service()
    if await service.initialize():
        return service
    return None
