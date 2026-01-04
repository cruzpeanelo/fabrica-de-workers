# -*- coding: utf-8 -*-
"""
Configuracao do Sistema de Notificacoes
Plataforma E v6.0

Configuracoes e variaveis de ambiente para o sistema de notificacoes.
Permite configurar canais via variaveis de ambiente ou arquivo .env.

Variaveis de ambiente suportadas:

EMAIL:
    NOTIFICATION_EMAIL_ENABLED=true
    NOTIFICATION_EMAIL_SMTP_HOST=smtp.gmail.com
    NOTIFICATION_EMAIL_SMTP_PORT=587
    NOTIFICATION_EMAIL_USERNAME=user@gmail.com
    NOTIFICATION_EMAIL_PASSWORD=app_password
    NOTIFICATION_EMAIL_FROM=user@gmail.com
    NOTIFICATION_EMAIL_FROM_NAME=Plataforma E
    NOTIFICATION_EMAIL_USE_TLS=true

SLACK:
    NOTIFICATION_SLACK_ENABLED=true
    NOTIFICATION_SLACK_WEBHOOK_URL=https://hooks.slack.com/services/...
    NOTIFICATION_SLACK_CHANNEL=#notifications
    NOTIFICATION_SLACK_USERNAME=Fabrica Bot

TEAMS:
    NOTIFICATION_TEAMS_ENABLED=true
    NOTIFICATION_TEAMS_WEBHOOK_URL=https://outlook.office.com/webhook/...

GERAL:
    NOTIFICATION_QUIET_HOURS_ENABLED=true
    NOTIFICATION_QUIET_HOURS_START=22:00
    NOTIFICATION_QUIET_HOURS_END=07:00
    NOTIFICATION_QUIET_HOURS_TIMEZONE=America/Sao_Paulo
"""

import os
from typing import Dict, Any, Optional
from dataclasses import dataclass, field


def get_env_bool(key: str, default: bool = False) -> bool:
    """Obtem variavel de ambiente como boolean"""
    value = os.getenv(key, str(default)).lower()
    return value in ("true", "1", "yes", "on")


def get_env_int(key: str, default: int = 0) -> int:
    """Obtem variavel de ambiente como inteiro"""
    try:
        return int(os.getenv(key, str(default)))
    except ValueError:
        return default


@dataclass
class EmailConfig:
    """Configuracao do canal de email"""
    enabled: bool = False
    smtp_host: str = ""
    smtp_port: int = 587
    username: str = ""
    password: str = ""
    from_email: str = ""
    from_name: str = "Plataforma E"
    use_tls: bool = True
    use_ssl: bool = False

    @classmethod
    def from_env(cls) -> "EmailConfig":
        """Carrega configuracao das variaveis de ambiente"""
        return cls(
            enabled=get_env_bool("NOTIFICATION_EMAIL_ENABLED"),
            smtp_host=os.getenv("NOTIFICATION_EMAIL_SMTP_HOST", ""),
            smtp_port=get_env_int("NOTIFICATION_EMAIL_SMTP_PORT", 587),
            username=os.getenv("NOTIFICATION_EMAIL_USERNAME", ""),
            password=os.getenv("NOTIFICATION_EMAIL_PASSWORD", ""),
            from_email=os.getenv("NOTIFICATION_EMAIL_FROM", ""),
            from_name=os.getenv("NOTIFICATION_EMAIL_FROM_NAME", "Plataforma E"),
            use_tls=get_env_bool("NOTIFICATION_EMAIL_USE_TLS", True),
            use_ssl=get_env_bool("NOTIFICATION_EMAIL_USE_SSL", False)
        )

    def to_channel_config(self) -> Dict[str, Any]:
        """Converte para formato de configuracao de canal"""
        return {
            "smtp_host": self.smtp_host,
            "smtp_port": self.smtp_port,
            "username": self.username,
            "password": self.password,
            "from_email": self.from_email,
            "from_name": self.from_name,
            "use_tls": self.use_tls,
            "use_ssl": self.use_ssl
        }

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return all([
            self.smtp_host,
            self.smtp_port,
            self.username,
            self.password,
            self.from_email
        ])


@dataclass
class SlackConfig:
    """Configuracao do canal Slack"""
    enabled: bool = False
    webhook_url: str = ""
    channel: str = ""
    username: str = "Fabrica Bot"
    icon_emoji: str = ":factory:"

    @classmethod
    def from_env(cls) -> "SlackConfig":
        """Carrega configuracao das variaveis de ambiente"""
        return cls(
            enabled=get_env_bool("NOTIFICATION_SLACK_ENABLED"),
            webhook_url=os.getenv("NOTIFICATION_SLACK_WEBHOOK_URL", ""),
            channel=os.getenv("NOTIFICATION_SLACK_CHANNEL", ""),
            username=os.getenv("NOTIFICATION_SLACK_USERNAME", "Fabrica Bot"),
            icon_emoji=os.getenv("NOTIFICATION_SLACK_ICON_EMOJI", ":factory:")
        )

    def to_channel_config(self) -> Dict[str, Any]:
        """Converte para formato de configuracao de canal"""
        return {
            "webhook_url": self.webhook_url,
            "channel": self.channel,
            "username": self.username,
            "icon_emoji": self.icon_emoji
        }

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.webhook_url)


@dataclass
class TeamsConfig:
    """Configuracao do canal Teams"""
    enabled: bool = False
    webhook_url: str = ""

    @classmethod
    def from_env(cls) -> "TeamsConfig":
        """Carrega configuracao das variaveis de ambiente"""
        return cls(
            enabled=get_env_bool("NOTIFICATION_TEAMS_ENABLED"),
            webhook_url=os.getenv("NOTIFICATION_TEAMS_WEBHOOK_URL", "")
        )

    def to_channel_config(self) -> Dict[str, Any]:
        """Converte para formato de configuracao de canal"""
        return {
            "webhook_url": self.webhook_url
        }

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.webhook_url)


@dataclass
class QuietHoursConfig:
    """Configuracao de horario de nao perturbe"""
    enabled: bool = True
    start: str = "22:00"
    end: str = "07:00"
    timezone: str = "America/Sao_Paulo"
    exceptions: list = field(default_factory=lambda: ["error", "approval_required"])

    @classmethod
    def from_env(cls) -> "QuietHoursConfig":
        """Carrega configuracao das variaveis de ambiente"""
        exceptions_str = os.getenv("NOTIFICATION_QUIET_HOURS_EXCEPTIONS", "error,approval_required")
        exceptions = [e.strip() for e in exceptions_str.split(",") if e.strip()]

        return cls(
            enabled=get_env_bool("NOTIFICATION_QUIET_HOURS_ENABLED", True),
            start=os.getenv("NOTIFICATION_QUIET_HOURS_START", "22:00"),
            end=os.getenv("NOTIFICATION_QUIET_HOURS_END", "07:00"),
            timezone=os.getenv("NOTIFICATION_QUIET_HOURS_TIMEZONE", "America/Sao_Paulo"),
            exceptions=exceptions
        )


@dataclass
class NotificationConfig:
    """Configuracao completa do sistema de notificacoes"""
    email: EmailConfig = field(default_factory=EmailConfig)
    slack: SlackConfig = field(default_factory=SlackConfig)
    teams: TeamsConfig = field(default_factory=TeamsConfig)
    quiet_hours: QuietHoursConfig = field(default_factory=QuietHoursConfig)
    global_enabled: bool = True

    @classmethod
    def from_env(cls) -> "NotificationConfig":
        """Carrega toda a configuracao das variaveis de ambiente"""
        return cls(
            email=EmailConfig.from_env(),
            slack=SlackConfig.from_env(),
            teams=TeamsConfig.from_env(),
            quiet_hours=QuietHoursConfig.from_env(),
            global_enabled=get_env_bool("NOTIFICATION_ENABLED", True)
        )

    def get_enabled_channels(self) -> Dict[str, Dict[str, Any]]:
        """Retorna configuracao dos canais habilitados"""
        channels = {}

        if self.email.enabled and self.email.is_valid():
            channels["email"] = self.email.to_channel_config()

        if self.slack.enabled and self.slack.is_valid():
            channels["slack"] = self.slack.to_channel_config()

        if self.teams.enabled and self.teams.is_valid():
            channels["teams"] = self.teams.to_channel_config()

        return channels

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario (sem dados sensiveis)"""
        return {
            "global_enabled": self.global_enabled,
            "email": {
                "enabled": self.email.enabled,
                "smtp_host": self.email.smtp_host,
                "smtp_port": self.email.smtp_port,
                "from_email": self.email.from_email,
                "from_name": self.email.from_name,
                "use_tls": self.email.use_tls
            },
            "slack": {
                "enabled": self.slack.enabled,
                "channel": self.slack.channel,
                "username": self.slack.username,
                "has_webhook": bool(self.slack.webhook_url)
            },
            "teams": {
                "enabled": self.teams.enabled,
                "has_webhook": bool(self.teams.webhook_url)
            },
            "quiet_hours": {
                "enabled": self.quiet_hours.enabled,
                "start": self.quiet_hours.start,
                "end": self.quiet_hours.end,
                "timezone": self.quiet_hours.timezone,
                "exceptions": self.quiet_hours.exceptions
            }
        }


def load_config() -> NotificationConfig:
    """Carrega configuracao do sistema"""
    return NotificationConfig.from_env()


def setup_notification_manager(config: NotificationConfig = None) -> None:
    """
    Configura o NotificationManager com base na configuracao.

    Args:
        config: Configuracao a usar (carrega de env se None)
    """
    from .notification_manager import notification_manager

    if config is None:
        config = load_config()

    # Desabilitar se globalmente desabilitado
    if not config.global_enabled:
        notification_manager.disable()
        return

    # Registrar canais
    channels = config.get_enabled_channels()

    for channel_type, channel_config in channels.items():
        try:
            notification_manager.register_channel(channel_type, channel_config)
        except Exception as e:
            import logging
            logging.getLogger(__name__).error(
                f"Erro ao registrar canal {channel_type}: {e}"
            )

    # Configurar quiet hours
    if config.quiet_hours.enabled:
        notification_manager.set_quiet_hours(
            start=config.quiet_hours.start,
            end=config.quiet_hours.end,
            timezone=config.quiet_hours.timezone,
            exceptions=config.quiet_hours.exceptions
        )


# =============================================================================
# CONFIGURACAO PADRAO (para desenvolvimento)
# =============================================================================

DEFAULT_CONFIG = {
    "channels": {
        "email": {
            "enabled": False,
            "priority": 1
        },
        "teams": {
            "enabled": False,
            "priority": 2
        },
        "slack": {
            "enabled": False,
            "priority": 3
        }
    },
    "quiet_hours": {
        "enabled": True,
        "start": "22:00",
        "end": "07:00",
        "timezone": "America/Sao_Paulo",
        "exceptions": ["error", "approval_required", "worker_offline"]
    },
    "defaults": {
        "max_retries": 3,
        "retry_delay_minutes": 5,
        "batch_size": 100,
        "rate_limit_per_minute": 60
    }
}
