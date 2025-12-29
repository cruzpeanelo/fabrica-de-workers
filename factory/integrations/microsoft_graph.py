# -*- coding: utf-8 -*-
"""
Microsoft Graph Integration
===========================
Integracao completa com Microsoft Graph API para Exchange/Outlook.

Este modulo fornece acesso facilitado a todas as funcionalidades
de email e calendario via Microsoft Graph API.

Funcionalidades:
- Envio de emails via Microsoft Graph API
- Leitura de caixa de entrada
- Gerenciamento de calendario (reunioes)
- Processamento de anexos
- Templates HTML personalizados
- Notificacoes automaticas
- Relatorios periodicos
- Skills para agentes Claude

Configuracao:
    Defina as seguintes variaveis de ambiente:

    # Azure AD Application
    AZURE_TENANT_ID=seu-tenant-id
    AZURE_CLIENT_ID=seu-client-id
    AZURE_CLIENT_SECRET=seu-client-secret

    # Email
    GRAPH_SENDER_EMAIL=fabrica@empresa.com
    GRAPH_DEFAULT_RECIPIENTS=team@empresa.com
    GRAPH_ADMIN_RECIPIENTS=admin@empresa.com
    GRAPH_ENABLED=true

    # SMTP Fallback (opcional)
    SMTP_SERVER=smtp.office365.com
    SMTP_PORT=587
    SMTP_USERNAME=fabrica@empresa.com
    SMTP_PASSWORD=sua-senha
    SMTP_ENABLED=false

Exemplo de uso:
    from factory.integrations.microsoft_graph import (
        MicrosoftGraphClient,
        EmailConfig,
        init_email_integration
    )

    # Inicializacao automatica
    client = await init_email_integration()

    # Enviar email
    await client.send_email(
        to=["usuario@empresa.com"],
        subject="Projeto Concluido",
        body="<h1>Sucesso!</h1><p>Seu projeto foi finalizado.</p>"
    )

    # Enviar com template
    await client.send_email_with_template(
        to=["usuario@empresa.com"],
        template_name="project_completed",
        template_vars={
            "project_name": "Meu Projeto",
            "project_id": "PROJ-001"
        }
    )

    # Buscar emails
    messages = await client.get_messages(
        folder="Inbox",
        filter_query="isRead eq false",
        top=10
    )

    # Agendar reuniao
    from datetime import datetime, timedelta
    await client.schedule_meeting(
        subject="Review do Projeto",
        start=datetime.now() + timedelta(days=1),
        duration_minutes=30,
        attendees=["team@empresa.com"]
    )

Permissoes Azure AD necessarias:
    - Mail.Send
    - Mail.Read
    - Mail.ReadWrite
    - Calendars.ReadWrite
    - User.Read

Para registrar a aplicacao no Azure AD:
    1. Acesse portal.azure.com
    2. Azure Active Directory > App registrations > New registration
    3. Defina o nome da aplicacao
    4. Em "API permissions", adicione as permissoes listadas acima
    5. Crie um client secret em "Certificates & secrets"
    6. Copie tenant_id, client_id e client_secret
"""

import logging
from typing import Optional

# Re-exporta classes principais do modulo email
from .email import (
    # Cliente principal
    MicrosoftGraphClient,
    EmailConfig,
    EmailMessage,
    EmailAttachment,
    CalendarEvent,
    get_graph_client,
    init_graph_client,

    # SMTP fallback
    SmtpClient,
    SmtpConfig,
    get_smtp_client
)

# Importa readers, senders e skills
from .email.readers import (
    InboxReader,
    EmailFilter,
    ProcessedEmail,
    AttachmentProcessor,
    ProcessedAttachment
)

from .email.senders import (
    NotificationSender,
    NotificationType,
    ReportSender,
    ReportType
)

from .email.skills import (
    EmailSendSkill,
    EmailReadSkill
)

logger = logging.getLogger(__name__)

__all__ = [
    # Cliente principal
    'MicrosoftGraphClient',
    'EmailConfig',
    'EmailMessage',
    'EmailAttachment',
    'CalendarEvent',
    'get_graph_client',
    'init_graph_client',

    # SMTP
    'SmtpClient',
    'SmtpConfig',
    'get_smtp_client',

    # Readers
    'InboxReader',
    'EmailFilter',
    'ProcessedEmail',
    'AttachmentProcessor',
    'ProcessedAttachment',

    # Senders
    'NotificationSender',
    'NotificationType',
    'ReportSender',
    'ReportType',

    # Skills
    'EmailSendSkill',
    'EmailReadSkill',

    # Helpers
    'init_email_integration',
    'get_email_client'
]


# =============================================================================
# FUNCOES DE CONVENIENCIA
# =============================================================================

async def init_email_integration() -> Optional[MicrosoftGraphClient]:
    """
    Inicializa a integracao de email.

    Tenta conectar via Microsoft Graph primeiro.
    Se falhar, tenta SMTP como fallback.

    Returns:
        Cliente de email conectado ou None
    """
    # Tenta Microsoft Graph primeiro
    graph_client = await init_graph_client()
    if graph_client:
        logger.info("Integracao de email via Microsoft Graph iniciada")
        return graph_client

    # Fallback para SMTP
    from .email.smtp_client import init_smtp_client
    smtp_client = await init_smtp_client()
    if smtp_client:
        logger.info("Integracao de email via SMTP iniciada (fallback)")
        return smtp_client

    logger.warning("Nenhuma integracao de email disponivel")
    return None


def get_email_client():
    """
    Retorna cliente de email (Graph ou SMTP).

    Returns:
        Cliente de email ou None
    """
    graph = get_graph_client()
    if graph.config.is_valid():
        return graph

    smtp = get_smtp_client()
    if smtp.config.is_valid():
        return smtp

    return None


# =============================================================================
# INTEGRACAO COM O SISTEMA
# =============================================================================

class EmailIntegration:
    """
    Classe facade para integracao completa de email.

    Combina todas as funcionalidades em uma interface unificada.

    Exemplo:
        integration = EmailIntegration()
        await integration.connect()

        # Enviar notificacao
        await integration.notify_project_completed(project_data)

        # Monitorar emails
        await integration.start_inbox_monitor(callback)
    """

    def __init__(self):
        self.client: Optional[MicrosoftGraphClient] = None
        self.notification_sender: Optional[NotificationSender] = None
        self.report_sender: Optional[ReportSender] = None
        self.inbox_reader: Optional[InboxReader] = None
        self.attachment_processor: Optional[AttachmentProcessor] = None
        self._connected = False

    @property
    def is_connected(self) -> bool:
        return self._connected and self.client is not None

    async def connect(self) -> bool:
        """Conecta a integracao de email"""
        self.client = await init_email_integration()

        if self.client:
            self._connected = True
            self.notification_sender = NotificationSender(self.client)
            self.report_sender = ReportSender(self.client)
            self.inbox_reader = InboxReader(self.client)
            self.attachment_processor = AttachmentProcessor(self.client)
            return True

        return False

    async def disconnect(self):
        """Desconecta a integracao"""
        if self.client:
            await self.client.disconnect()
        self._connected = False

    # Metodos de notificacao delegados
    async def notify_project_created(self, project_data, recipients=None):
        if self.notification_sender:
            return await self.notification_sender.notify_project_created(project_data, recipients)
        return False

    async def notify_project_completed(self, project_data, execution_data=None, recipients=None):
        if self.notification_sender:
            return await self.notification_sender.notify_project_completed(
                project_data, execution_data, recipients
            )
        return False

    async def notify_story_updated(self, story_data, old_status=None, new_status=None, recipients=None):
        if self.notification_sender:
            return await self.notification_sender.notify_story_updated(
                story_data, old_status, new_status, recipients
            )
        return False

    async def notify_error(self, error_type, error_message, **kwargs):
        if self.notification_sender:
            return await self.notification_sender.notify_error(
                error_type, error_message, **kwargs
            )
        return False

    # Metodos de relatorio delegados
    async def send_daily_report(self, data=None, recipients=None):
        if self.report_sender:
            return await self.report_sender.send_daily_report(data, recipients)
        return False

    async def send_project_report(self, project_data, execution_data=None, recipients=None):
        if self.report_sender:
            return await self.report_sender.send_project_report(
                project_data, execution_data, recipients
            )
        return False

    # Metodos de leitura delegados
    async def get_unread_emails(self, folder="Inbox", max_results=20):
        if self.inbox_reader:
            return await self.inbox_reader.get_unread_emails(folder, max_results)
        return []

    async def get_project_requests(self, days_back=7, max_results=10):
        if self.inbox_reader:
            return await self.inbox_reader.get_project_requests(days_back, max_results)
        return []

    async def process_email_for_project(self, email):
        """Processa email e extrai dados de projeto"""
        if self.inbox_reader:
            project_data = await self.inbox_reader.extract_project_requirements(email)

            # Processa anexos se houver
            if email.has_attachments and self.attachment_processor:
                att_data = await self.attachment_processor.process_email_attachments(
                    email.id,
                    extract_text=True
                )
                if att_data.get("requirements"):
                    project_data.setdefault("requirements", []).extend(att_data["requirements"])
                if att_data.get("user_stories"):
                    project_data["user_stories"] = att_data["user_stories"]

            return project_data
        return None

    async def start_inbox_monitor(self, callback, check_interval=60, folder="Inbox"):
        """Inicia monitoramento de caixa de entrada"""
        if self.inbox_reader:
            await self.inbox_reader.watch_for_projects(callback, check_interval, folder)


# Instancia global
_email_integration: Optional[EmailIntegration] = None


def get_email_integration() -> EmailIntegration:
    """Retorna instancia global da integracao de email"""
    global _email_integration
    if _email_integration is None:
        _email_integration = EmailIntegration()
    return _email_integration
