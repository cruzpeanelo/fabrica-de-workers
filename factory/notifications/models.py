# -*- coding: utf-8 -*-
"""
Modelos de Dados para o Sistema de Notificacoes
Plataforma E v6.0

Este modulo define os modelos SQLAlchemy para:
- NotificationPreference: Preferencias de notificacao por usuario
- NotificationRule: Regras de notificacao configuráveis
- NotificationLog: Historico de notificacoes enviadas
- NotificationChannel: Configuracao de canais
"""

from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float
from sqlalchemy.orm import relationship
from datetime import datetime
from enum import Enum

# Import Base do projeto
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from database.connection import Base
except ImportError:
    try:
        from factory.database.connection import Base
    except ImportError:
        from sqlalchemy.ext.declarative import declarative_base
        Base = declarative_base()


# =============================================================================
# ENUMS
# =============================================================================

class ChannelType(str, Enum):
    """Tipos de canais de notificacao"""
    EMAIL = "email"
    SLACK = "slack"
    TEAMS = "teams"
    WHATSAPP = "whatsapp"
    SMS = "sms"
    PUSH = "push"
    WEBHOOK = "webhook"


class NotificationStatus(str, Enum):
    """Status de uma notificacao"""
    PENDING = "pending"
    SENDING = "sending"
    SENT = "sent"
    FAILED = "failed"
    CANCELLED = "cancelled"


class EventType(str, Enum):
    """Tipos de eventos que geram notificacoes"""
    # Projetos
    PROJECT_CREATED = "project_created"
    PROJECT_COMPLETED = "project_completed"
    PROJECT_ARCHIVED = "project_archived"

    # Stories
    STORY_CREATED = "story_created"
    STORY_STARTED = "story_started"
    STORY_COMPLETED = "story_completed"
    STORY_BLOCKED = "story_blocked"
    STORY_MOVED = "story_moved"

    # Tasks
    TASK_CREATED = "task_created"
    TASK_COMPLETED = "task_completed"
    TASK_FAILED = "task_failed"

    # Jobs
    JOB_QUEUED = "job_queued"
    JOB_STARTED = "job_started"
    JOB_COMPLETED = "job_completed"
    JOB_FAILED = "job_failed"

    # Sistema
    ERROR = "error"
    WARNING = "warning"
    APPROVAL_REQUIRED = "approval_required"
    REPORT_DAILY = "report_daily"
    REPORT_WEEKLY = "report_weekly"
    LIMIT_ALERT = "limit_alert"

    # Worker
    WORKER_ONLINE = "worker_online"
    WORKER_OFFLINE = "worker_offline"


class NotificationPriority(str, Enum):
    """Prioridade da notificacao"""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"
    URGENT = "urgent"


# =============================================================================
# NOTIFICATION_CHANNEL - Configuracao de Canais
# =============================================================================

class NotificationChannel(Base):
    """
    Modelo para Configuracao de Canais de Notificacao
    Armazena credenciais e configuracoes de cada canal
    """
    __tablename__ = "notification_channels"

    id = Column(Integer, primary_key=True, autoincrement=True)
    channel_id = Column(String(50), unique=True, nullable=False, index=True)

    # Tipo do canal
    channel_type = Column(String(30), nullable=False, index=True)

    # Nome amigavel
    name = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Configuracao especifica do canal (JSON)
    # Email: {"smtp_host", "smtp_port", "username", "password", "from_email", "use_tls"}
    # Slack: {"webhook_url", "channel", "bot_token"}
    # Teams: {"webhook_url"}
    # WhatsApp: {"account_sid", "auth_token", "from_number"}
    # SMS: {"account_sid", "auth_token", "from_number"}
    config = Column(JSON, default=dict)

    # Status
    enabled = Column(Boolean, default=True)
    verified = Column(Boolean, default=False)  # Se as credenciais foram verificadas

    # Prioridade padrao (ordem de preferencia)
    priority = Column(Integer, default=1)

    # Limites de rate limiting
    rate_limit_per_minute = Column(Integer, default=60)
    rate_limit_per_hour = Column(Integer, default=500)

    # Metricas
    messages_sent = Column(Integer, default=0)
    messages_failed = Column(Integer, default=0)
    last_success = Column(DateTime, nullable=True)
    last_failure = Column(DateTime, nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relacionamentos
    preferences = relationship("NotificationPreference", back_populates="channel")
    logs = relationship("NotificationLog", back_populates="channel")

    def to_dict(self):
        return {
            "channel_id": self.channel_id,
            "channel_type": self.channel_type,
            "name": self.name,
            "description": self.description,
            "config": self._safe_config(),
            "enabled": self.enabled,
            "verified": self.verified,
            "priority": self.priority,
            "rate_limit_per_minute": self.rate_limit_per_minute,
            "rate_limit_per_hour": self.rate_limit_per_hour,
            "messages_sent": self.messages_sent,
            "messages_failed": self.messages_failed,
            "last_success": self.last_success.isoformat() if self.last_success else None,
            "last_failure": self.last_failure.isoformat() if self.last_failure else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def _safe_config(self):
        """Retorna config sem dados sensiveis"""
        if not self.config:
            return {}
        safe = dict(self.config)
        # Remove campos sensiveis
        sensitive_fields = ["password", "auth_token", "bot_token", "api_key", "secret"]
        for field in sensitive_fields:
            if field in safe:
                safe[field] = "***HIDDEN***"
        return safe

    def __repr__(self):
        return f"<NotificationChannel {self.channel_id}: {self.name} [{self.channel_type}]>"


# =============================================================================
# NOTIFICATION_PREFERENCE - Preferencias do Usuario
# =============================================================================

class NotificationPreference(Base):
    """
    Modelo para Preferencias de Notificacao por Usuario
    Define quais eventos o usuario quer receber e por qual canal
    """
    __tablename__ = "notification_preferences"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Usuario (referencia ao User do sistema)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)

    # Canal de notificacao
    channel_id = Column(String(50), ForeignKey("notification_channels.channel_id"), nullable=False)
    channel = relationship("NotificationChannel", back_populates="preferences")

    # Tipo de evento
    event_type = Column(String(50), nullable=False, index=True)

    # Habilitado
    enabled = Column(Boolean, default=True)

    # Prioridade deste canal para este evento (menor = mais prioritario)
    priority = Column(Integer, default=1)

    # Destino especifico (email alternativo, canal Slack, etc)
    destination = Column(String(300), nullable=True)

    # Configuracoes especificas
    # Ex: {"quiet_hours": {"start": "22:00", "end": "07:00"}, "digest": true}
    settings = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "id": self.id,
            "user_id": self.user_id,
            "channel_id": self.channel_id,
            "event_type": self.event_type,
            "enabled": self.enabled,
            "priority": self.priority,
            "destination": self.destination,
            "settings": self.settings or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def __repr__(self):
        return f"<NotificationPreference user={self.user_id} event={self.event_type} channel={self.channel_id}>"


# =============================================================================
# NOTIFICATION_RULE - Regras de Notificacao
# =============================================================================

class NotificationRule(Base):
    """
    Modelo para Regras de Notificacao
    Define condicoes e acoes para envio automatico
    """
    __tablename__ = "notification_rules"

    id = Column(Integer, primary_key=True, autoincrement=True)
    rule_id = Column(String(50), unique=True, nullable=False, index=True)

    # Nome e descricao
    name = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Tipo de evento que dispara a regra
    event_type = Column(String(50), nullable=False, index=True)

    # Condicoes em JSON (avaliadas pelo motor de regras)
    # Exemplo:
    # {
    #   "operator": "AND",
    #   "conditions": [
    #     {"field": "priority", "op": "eq", "value": "high"},
    #     {"field": "project_id", "op": "in", "value": ["PRJ-001", "PRJ-002"]}
    #   ]
    # }
    conditions = Column(JSON, default=dict)

    # Canais para enviar (lista de channel_ids)
    channels = Column(JSON, default=list)

    # Destinatarios
    # Valores especiais: "owner", "team", "admin", "assignee"
    # Ou emails/IDs especificos
    recipients = Column(JSON, default=list)

    # Template a usar (ID do template ou nome)
    template = Column(String(100), nullable=True)

    # Prioridade da notificacao
    priority = Column(String(20), default=NotificationPriority.NORMAL.value)

    # Configuracoes adicionais
    # Ex: {"delay_minutes": 5, "aggregate": true, "aggregate_window": 30}
    config = Column(JSON, default=dict)

    # Status
    is_active = Column(Boolean, default=True)

    # Escopo (global ou por projeto)
    project_id = Column(String(50), nullable=True, index=True)  # Se null, aplica globalmente

    # Metricas
    triggered_count = Column(Integer, default=0)
    last_triggered = Column(DateTime, nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = Column(String(100), nullable=True)

    def to_dict(self):
        return {
            "rule_id": self.rule_id,
            "name": self.name,
            "description": self.description,
            "event_type": self.event_type,
            "conditions": self.conditions or {},
            "channels": self.channels or [],
            "recipients": self.recipients or [],
            "template": self.template,
            "priority": self.priority,
            "config": self.config or {},
            "is_active": self.is_active,
            "project_id": self.project_id,
            "triggered_count": self.triggered_count,
            "last_triggered": self.last_triggered.isoformat() if self.last_triggered else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<NotificationRule {self.rule_id}: {self.name} [{self.event_type}]>"


# =============================================================================
# NOTIFICATION_LOG - Historico de Notificacoes
# =============================================================================

class NotificationLog(Base):
    """
    Modelo para Log de Notificacoes Enviadas
    Registra cada notificacao para auditoria e debugging
    """
    __tablename__ = "notification_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    notification_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamentos
    user_id = Column(Integer, nullable=True, index=True)  # Usuario destinatario
    channel_id = Column(String(50), ForeignKey("notification_channels.channel_id"), nullable=False)
    channel = relationship("NotificationChannel", back_populates="logs")
    rule_id = Column(String(50), nullable=True, index=True)  # Regra que disparou

    # Evento
    event_type = Column(String(50), nullable=False, index=True)
    event_data = Column(JSON, default=dict)  # Dados do evento

    # Destino
    recipient = Column(String(300), nullable=False)  # Email, telefone, webhook, etc

    # Conteudo
    subject = Column(String(500), nullable=True)
    content = Column(Text, nullable=True)
    content_html = Column(Text, nullable=True)  # Versao HTML se aplicavel

    # Template usado
    template_used = Column(String(100), nullable=True)

    # Status
    status = Column(String(20), default=NotificationStatus.PENDING.value, index=True)

    # Tentativas
    attempts = Column(Integer, default=0)
    max_attempts = Column(Integer, default=3)

    # Erro (se falhou)
    error_message = Column(Text, nullable=True)
    error_code = Column(String(50), nullable=True)

    # Resposta do provedor (para debugging)
    provider_response = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    sent_at = Column(DateTime, nullable=True)
    next_retry_at = Column(DateTime, nullable=True)

    # Metricas de entrega (para email)
    opened_at = Column(DateTime, nullable=True)
    clicked_at = Column(DateTime, nullable=True)

    def to_dict(self):
        return {
            "notification_id": self.notification_id,
            "user_id": self.user_id,
            "channel_id": self.channel_id,
            "rule_id": self.rule_id,
            "event_type": self.event_type,
            "event_data": self.event_data or {},
            "recipient": self.recipient,
            "subject": self.subject,
            "content": self.content,
            "template_used": self.template_used,
            "status": self.status,
            "attempts": self.attempts,
            "max_attempts": self.max_attempts,
            "error_message": self.error_message,
            "error_code": self.error_code,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "sent_at": self.sent_at.isoformat() if self.sent_at else None,
            "next_retry_at": self.next_retry_at.isoformat() if self.next_retry_at else None,
            "opened_at": self.opened_at.isoformat() if self.opened_at else None,
            "clicked_at": self.clicked_at.isoformat() if self.clicked_at else None
        }

    def mark_sent(self):
        """Marca notificacao como enviada"""
        self.status = NotificationStatus.SENT.value
        self.sent_at = datetime.utcnow()

    def mark_failed(self, error_message: str, error_code: str = None):
        """Marca notificacao como falha"""
        self.attempts += 1
        self.error_message = error_message
        self.error_code = error_code

        if self.attempts >= self.max_attempts:
            self.status = NotificationStatus.FAILED.value
        else:
            self.status = NotificationStatus.PENDING.value
            # Retry com backoff exponencial (5min, 15min, 45min)
            from datetime import timedelta
            retry_minutes = 5 * (3 ** (self.attempts - 1))
            self.next_retry_at = datetime.utcnow() + timedelta(minutes=retry_minutes)

    def __repr__(self):
        return f"<NotificationLog {self.notification_id}: {self.event_type} -> {self.recipient} [{self.status}]>"


# =============================================================================
# NOTIFICATION_TEMPLATE - Templates de Mensagem
# =============================================================================

class NotificationTemplate(Base):
    """
    Modelo para Templates de Notificacao
    Armazena templates personalizáveis para cada tipo de evento/canal
    """
    __tablename__ = "notification_templates"

    id = Column(Integer, primary_key=True, autoincrement=True)
    template_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Tipo de evento e canal
    event_type = Column(String(50), nullable=False, index=True)
    channel_type = Column(String(30), nullable=False, index=True)

    # Conteudo do template (usa Jinja2 ou similar)
    subject_template = Column(String(500), nullable=True)  # Para email
    body_template = Column(Text, nullable=False)
    body_html_template = Column(Text, nullable=True)  # Para email HTML

    # Variaveis disponiveis (documentacao)
    available_variables = Column(JSON, default=list)
    # Ex: ["project_name", "story_title", "user_name", "timestamp"]

    # Status
    is_active = Column(Boolean, default=True)
    is_default = Column(Boolean, default=False)  # Template padrao para este evento/canal

    # Idioma
    language = Column(String(10), default="pt-BR")

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = Column(String(100), nullable=True)

    def to_dict(self):
        return {
            "template_id": self.template_id,
            "name": self.name,
            "description": self.description,
            "event_type": self.event_type,
            "channel_type": self.channel_type,
            "subject_template": self.subject_template,
            "body_template": self.body_template,
            "body_html_template": self.body_html_template,
            "available_variables": self.available_variables or [],
            "is_active": self.is_active,
            "is_default": self.is_default,
            "language": self.language,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<NotificationTemplate {self.template_id}: {self.name} [{self.event_type}/{self.channel_type}]>"
