# -*- coding: utf-8 -*-
"""
Email Integration Module
========================
Integracao com Microsoft Exchange/Outlook via Microsoft Graph API e SMTP.

Este modulo fornece:
- Envio de emails via Microsoft Graph API
- Envio de emails via SMTP (fallback)
- Leitura de caixa de entrada
- Processamento de anexos
- Templates HTML personalizados
- Integracao com calendario (reunioes)

Exemplo de uso:
    from factory.integrations.email import MicrosoftGraphClient, EmailConfig

    config = EmailConfig.from_env()
    client = MicrosoftGraphClient(config)

    if await client.connect():
        await client.send_email(
            to=["usuario@empresa.com"],
            subject="Projeto Concluido",
            body="Seu projeto foi finalizado com sucesso!"
        )
"""

from .graph_mail_client import (
    MicrosoftGraphClient,
    EmailConfig,
    EmailMessage,
    EmailAttachment,
    CalendarEvent,
    get_graph_client,
    init_graph_client
)

from .smtp_client import (
    SmtpClient,
    SmtpConfig,
    get_smtp_client
)

__all__ = [
    # Microsoft Graph
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
    'get_smtp_client'
]
