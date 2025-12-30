# -*- coding: utf-8 -*-
"""
Calendar Integrations Module (Issue #264)
==========================================
Integracoes com calendarios externos (Google Calendar e Outlook).

Funcionalidades:
- Sincronizacao de sprints/deadlines com calendarios
- Criacao automatica de eventos
- Atualizacao bidirecional de eventos
- Suporte a Google Calendar e Microsoft Outlook

Configuracao via variaveis de ambiente:
- GOOGLE_CALENDAR_CREDENTIALS: Path para credentials.json
- GOOGLE_CALENDAR_TOKEN: Path para token.json
- AZURE_TENANT_ID, AZURE_CLIENT_ID, AZURE_CLIENT_SECRET: Para Outlook
"""

from .google_calendar import (
    GoogleCalendarClient,
    GoogleCalendarConfig,
    get_google_calendar_client
)
from .outlook_calendar import (
    OutlookCalendarClient,
    OutlookCalendarConfig,
    get_outlook_calendar_client
)
from .calendar_sync import (
    CalendarSyncService,
    CalendarEvent,
    CalendarEventType,
    get_calendar_sync_service
)

__all__ = [
    # Google Calendar
    'GoogleCalendarClient',
    'GoogleCalendarConfig',
    'get_google_calendar_client',

    # Outlook Calendar
    'OutlookCalendarClient',
    'OutlookCalendarConfig',
    'get_outlook_calendar_client',

    # Sync Service
    'CalendarSyncService',
    'CalendarEvent',
    'CalendarEventType',
    'get_calendar_sync_service'
]
