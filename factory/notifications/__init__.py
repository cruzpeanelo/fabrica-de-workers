# -*- coding: utf-8 -*-
"""
Sistema Centralizado de Notificacoes Multi-Canal
Fabrica de Agentes v6.0

Este modulo fornece um sistema completo para envio de notificacoes
atraves de multiplos canais (Email, Slack, Teams, etc).

Principais componentes:
- NotificationManager: Gerenciador central de notificacoes
- Channels: Implementacoes de canais (Email, Slack, Teams)
- RuleEngine: Motor de regras para routing de notificacoes
- Templates: Templates padrao para cada tipo de evento

Uso basico:
    from factory.notifications import notification_manager

    # Configurar canal de email
    notification_manager.register_channel("email", {
        "smtp_host": "smtp.gmail.com",
        "smtp_port": 587,
        "username": "user@gmail.com",
        "password": "app_password",
        "from_email": "user@gmail.com",
        "use_tls": True
    })

    # Configurar canal de Slack
    notification_manager.register_channel("slack", {
        "webhook_url": "https://hooks.slack.com/services/XXX/YYY/ZZZ"
    })

    # Configurar canal de Teams
    notification_manager.register_channel("teams", {
        "webhook_url": "https://outlook.office.com/webhook/XXX"
    })

    # Enviar notificacao
    await notification_manager.notify(
        event_type="project_completed",
        data={
            "project_name": "Sistema de Vendas",
            "duration": "2h 30min",
            "files_count": 45
        },
        recipients=["team@company.com"],
        channels=["email", "teams"]
    )

Eventos suportados:
    - project_created, project_completed, project_archived
    - story_created, story_started, story_completed, story_blocked
    - task_created, task_completed, task_failed
    - job_queued, job_started, job_completed, job_failed
    - error, warning, approval_required
    - report_daily, report_weekly
    - worker_online, worker_offline
"""

from .notification_manager import (
    NotificationManager,
    NotificationResult,
    notification_manager,
    get_notification_manager
)

from .channels import (
    BaseChannel,
    NotificationMessage,
    ChannelResponse,
    EmailChannel,
    SlackChannel,
    TeamsChannel
)

from .rules import (
    RuleEngine,
    DEFAULT_RULES,
    create_default_rules
)

from .models import (
    ChannelType,
    NotificationStatus,
    EventType,
    NotificationPriority,
    NotificationChannel,
    NotificationPreference,
    NotificationRule,
    NotificationLog,
    NotificationTemplate
)

from .templates.default_templates import (
    EMAIL_TEMPLATES,
    SLACK_TEMPLATES,
    TEAMS_TEMPLATES,
    get_template,
    list_available_templates,
    get_template_variables
)

from .event_handlers import (
    NotificationEventHandler,
    event_handler,
    get_event_handler,
    notify_on_complete,
    notify_on_error
)

from .config import (
    NotificationConfig,
    EmailConfig,
    SlackConfig,
    TeamsConfig,
    QuietHoursConfig,
    load_config,
    setup_notification_manager
)

__all__ = [
    # Manager
    "NotificationManager",
    "NotificationResult",
    "notification_manager",
    "get_notification_manager",

    # Channels
    "BaseChannel",
    "NotificationMessage",
    "ChannelResponse",
    "EmailChannel",
    "SlackChannel",
    "TeamsChannel",

    # Rules
    "RuleEngine",
    "DEFAULT_RULES",
    "create_default_rules",

    # Models
    "ChannelType",
    "NotificationStatus",
    "EventType",
    "NotificationPriority",
    "NotificationChannel",
    "NotificationPreference",
    "NotificationRule",
    "NotificationLog",
    "NotificationTemplate",

    # Templates
    "EMAIL_TEMPLATES",
    "SLACK_TEMPLATES",
    "TEAMS_TEMPLATES",
    "get_template",
    "list_available_templates",
    "get_template_variables",

    # Event Handlers
    "NotificationEventHandler",
    "event_handler",
    "get_event_handler",
    "notify_on_complete",
    "notify_on_error",

    # Config
    "NotificationConfig",
    "EmailConfig",
    "SlackConfig",
    "TeamsConfig",
    "QuietHoursConfig",
    "load_config",
    "setup_notification_manager"
]

__version__ = "1.0.0"
