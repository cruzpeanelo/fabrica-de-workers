# -*- coding: utf-8 -*-
"""
Modelos de API Publica - Fabrica de Agentes v6.5
===============================================

Modelos SQLAlchemy para gerenciamento de API Keys, Webhooks e Rate Limiting.
Permite integracao de desenvolvedores externos com a plataforma.
"""
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float, Index
from sqlalchemy.orm import relationship
from datetime import datetime, timedelta
from enum import Enum
import secrets
import hashlib

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# ENUMS
# =============================================================================

class APIKeyTier(str, Enum):
    """Niveis de API Key com diferentes limites"""
    FREE = "free"           # 100 req/min, 1000/dia
    BASIC = "basic"         # 500 req/min, 10000/dia
    PRO = "pro"             # 2000 req/min, 100000/dia
    ENTERPRISE = "enterprise"  # Ilimitado


class APIKeyStatus(str, Enum):
    """Status da API Key"""
    ACTIVE = "active"
    INACTIVE = "inactive"
    REVOKED = "revoked"
    EXPIRED = "expired"


class WebhookEventType(str, Enum):
    """Tipos de eventos para webhooks"""
    # Projetos
    PROJECT_CREATED = "project.created"
    PROJECT_UPDATED = "project.updated"
    PROJECT_COMPLETED = "project.completed"
    PROJECT_DELETED = "project.deleted"

    # Stories
    STORY_CREATED = "story.created"
    STORY_UPDATED = "story.updated"
    STORY_STATUS_CHANGED = "story.status_changed"
    STORY_COMPLETED = "story.completed"

    # Jobs
    JOB_CREATED = "job.created"
    JOB_STARTED = "job.started"
    JOB_COMPLETED = "job.completed"
    JOB_FAILED = "job.failed"

    # Tasks
    TASK_CREATED = "task.created"
    TASK_COMPLETED = "task.completed"


class WebhookStatus(str, Enum):
    """Status do webhook"""
    ACTIVE = "active"
    PAUSED = "paused"
    FAILED = "failed"  # Muitas falhas consecutivas


# =============================================================================
# API KEY - Chaves de API para desenvolvedores
# =============================================================================

class APIKey(Base):
    """
    Modelo para API Keys

    Permite que desenvolvedores externos acessem a API da Fabrica.
    Cada chave tem um tier que define limites de rate limiting.
    """
    __tablename__ = "api_keys"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Identificador publico (prefixo visivel)
    key_id = Column(String(50), unique=True, nullable=False, index=True)

    # Hash da chave secreta (nunca armazenar plain text)
    key_hash = Column(String(255), nullable=False)

    # Prefixo visivel (primeiros 8 caracteres, para identificacao)
    key_prefix = Column(String(12), nullable=False)

    # Dono da chave
    user_id = Column(Integer, ForeignKey("users.id"), nullable=True, index=True)

    # Metadados
    name = Column(String(200), nullable=False)  # Nome amigavel (ex: "Minha App de Producao")
    description = Column(Text, nullable=True)

    # Tier e limites
    tier = Column(String(20), default=APIKeyTier.FREE.value, index=True)

    # Limites customizados (override do tier)
    custom_rate_limit = Column(Integer, nullable=True)  # req/min
    custom_daily_limit = Column(Integer, nullable=True)  # req/dia

    # Status
    status = Column(String(20), default=APIKeyStatus.ACTIVE.value, index=True)

    # Permissoes (lista de scopes)
    scopes = Column(JSON, default=lambda: ["read"])  # read, write, admin, webhooks

    # Restricoes de IP (lista de IPs/CIDRs permitidos, vazio = todos)
    allowed_ips = Column(JSON, default=list)

    # Metricas de uso
    requests_today = Column(Integer, default=0)
    requests_total = Column(Integer, default=0)
    last_used_at = Column(DateTime, nullable=True)
    last_used_ip = Column(String(50), nullable=True)

    # Expiracao
    expires_at = Column(DateTime, nullable=True)  # Null = nunca expira

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    revoked_at = Column(DateTime, nullable=True)

    # Indices compostos
    __table_args__ = (
        Index('idx_api_key_status_tier', 'status', 'tier'),
    )

    @staticmethod
    def generate_key() -> tuple:
        """
        Gera uma nova API key

        Returns:
            tuple: (key_id, full_key, key_hash, key_prefix)
        """
        # Gerar key ID (identificador publico)
        key_id = f"fab_{secrets.token_urlsafe(8)}"

        # Gerar chave secreta (32 bytes = 256 bits)
        secret = secrets.token_urlsafe(32)

        # Chave completa no formato: sk-fab_XXXXX_YYYYYYYYYYYY
        full_key = f"sk-{key_id}_{secret}"

        # Hash da chave para armazenamento
        key_hash = hashlib.sha256(full_key.encode()).hexdigest()

        # Prefixo visivel (para identificacao)
        key_prefix = f"sk-{key_id[:8]}..."

        return key_id, full_key, key_hash, key_prefix

    @staticmethod
    def hash_key(full_key: str) -> str:
        """Gera hash de uma chave"""
        return hashlib.sha256(full_key.encode()).hexdigest()

    def verify_key(self, full_key: str) -> bool:
        """Verifica se a chave corresponde ao hash"""
        return self.key_hash == self.hash_key(full_key)

    def is_valid(self) -> bool:
        """Verifica se a chave esta valida para uso"""
        if self.status != APIKeyStatus.ACTIVE.value:
            return False
        if self.expires_at and datetime.utcnow() > self.expires_at:
            return False
        return True

    def get_rate_limits(self) -> dict:
        """Retorna limites de rate limiting baseado no tier"""
        tier_limits = {
            APIKeyTier.FREE.value: {"per_minute": 100, "per_day": 1000},
            APIKeyTier.BASIC.value: {"per_minute": 500, "per_day": 10000},
            APIKeyTier.PRO.value: {"per_minute": 2000, "per_day": 100000},
            APIKeyTier.ENTERPRISE.value: {"per_minute": 10000, "per_day": 1000000},
        }

        limits = tier_limits.get(self.tier, tier_limits[APIKeyTier.FREE.value])

        # Override com limites customizados se definidos
        if self.custom_rate_limit:
            limits["per_minute"] = self.custom_rate_limit
        if self.custom_daily_limit:
            limits["per_day"] = self.custom_daily_limit

        return limits

    def has_scope(self, scope: str) -> bool:
        """Verifica se a chave tem um determinado scope"""
        if not self.scopes:
            return False
        if "admin" in self.scopes:
            return True
        return scope in self.scopes

    def to_dict(self, include_sensitive: bool = False) -> dict:
        """Converte para dicionario"""
        data = {
            "key_id": self.key_id,
            "key_prefix": self.key_prefix,
            "name": self.name,
            "description": self.description,
            "tier": self.tier,
            "status": self.status,
            "scopes": self.scopes or [],
            "allowed_ips": self.allowed_ips or [],
            "rate_limits": self.get_rate_limits(),
            "requests_today": self.requests_today,
            "requests_total": self.requests_total,
            "last_used_at": self.last_used_at.isoformat() if self.last_used_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

        if include_sensitive:
            data["user_id"] = self.user_id
            data["last_used_ip"] = self.last_used_ip

        return data

    def __repr__(self):
        return f"<APIKey {self.key_id}: {self.name} [{self.tier}]>"


# =============================================================================
# WEBHOOK - Notificacoes para sistemas externos
# =============================================================================

class Webhook(Base):
    """
    Modelo para Webhooks

    Permite que desenvolvedores recebam notificacoes de eventos
    em seus sistemas quando algo acontece na Fabrica.
    """
    __tablename__ = "webhooks"

    id = Column(Integer, primary_key=True, autoincrement=True)
    webhook_id = Column(String(50), unique=True, nullable=False, index=True)

    # Dono
    api_key_id = Column(String(50), ForeignKey("api_keys.key_id"), nullable=False, index=True)

    # Configuracao
    name = Column(String(200), nullable=False)
    url = Column(String(500), nullable=False)  # URL de destino

    # Eventos assinados (lista de WebhookEventType)
    events = Column(JSON, default=list)

    # Seguranca
    secret = Column(String(255), nullable=True)  # Secret para assinatura HMAC

    # Filtros opcionais
    project_ids = Column(JSON, default=list)  # Filtrar por projetos especificos

    # Status
    status = Column(String(20), default=WebhookStatus.ACTIVE.value, index=True)

    # Metricas
    deliveries_total = Column(Integer, default=0)
    deliveries_failed = Column(Integer, default=0)
    consecutive_failures = Column(Integer, default=0)  # Reset em sucesso
    last_delivery_at = Column(DateTime, nullable=True)
    last_failure_at = Column(DateTime, nullable=True)
    last_failure_reason = Column(Text, nullable=True)

    # Headers customizados (JSON: {"X-Custom": "value"})
    custom_headers = Column(JSON, default=dict)

    # Retry config
    retry_count = Column(Integer, default=3)
    retry_delay = Column(Integer, default=60)  # segundos

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relacionamentos
    deliveries = relationship("WebhookDelivery", back_populates="webhook", cascade="all, delete-orphan")

    @staticmethod
    def generate_secret() -> str:
        """Gera secret para assinatura HMAC"""
        return secrets.token_urlsafe(32)

    def should_trigger(self, event_type: str, project_id: str = None) -> bool:
        """Verifica se o webhook deve ser disparado para este evento"""
        if self.status != WebhookStatus.ACTIVE.value:
            return False

        if event_type not in (self.events or []):
            return False

        # Filtro por projeto
        if self.project_ids and project_id:
            if project_id not in self.project_ids:
                return False

        return True

    def record_success(self):
        """Registra entrega bem-sucedida"""
        self.deliveries_total += 1
        self.consecutive_failures = 0
        self.last_delivery_at = datetime.utcnow()

    def record_failure(self, reason: str):
        """Registra falha na entrega"""
        self.deliveries_total += 1
        self.deliveries_failed += 1
        self.consecutive_failures += 1
        self.last_failure_at = datetime.utcnow()
        self.last_failure_reason = reason

        # Pausar apos muitas falhas consecutivas
        if self.consecutive_failures >= 10:
            self.status = WebhookStatus.FAILED.value

    def to_dict(self) -> dict:
        return {
            "webhook_id": self.webhook_id,
            "api_key_id": self.api_key_id,
            "name": self.name,
            "url": self.url,
            "events": self.events or [],
            "project_ids": self.project_ids or [],
            "status": self.status,
            "deliveries_total": self.deliveries_total,
            "deliveries_failed": self.deliveries_failed,
            "last_delivery_at": self.last_delivery_at.isoformat() if self.last_delivery_at else None,
            "last_failure_reason": self.last_failure_reason,
            "retry_count": self.retry_count,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def __repr__(self):
        return f"<Webhook {self.webhook_id}: {self.name} [{self.status}]>"


# =============================================================================
# WEBHOOK_DELIVERY - Historico de entregas
# =============================================================================

class WebhookDelivery(Base):
    """
    Modelo para Historico de Entregas de Webhook

    Armazena cada tentativa de entrega para debugging e auditoria.
    """
    __tablename__ = "webhook_deliveries"

    id = Column(Integer, primary_key=True, autoincrement=True)
    delivery_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento
    webhook_id = Column(String(50), ForeignKey("webhooks.webhook_id"), nullable=False, index=True)
    webhook = relationship("Webhook", back_populates="deliveries")

    # Evento
    event_type = Column(String(50), nullable=False)
    event_id = Column(String(50), nullable=True)  # ID do recurso (story_id, job_id, etc)

    # Payload enviado
    payload = Column(JSON, nullable=False)

    # Resposta
    success = Column(Boolean, default=False)
    status_code = Column(Integer, nullable=True)
    response_body = Column(Text, nullable=True)
    response_time_ms = Column(Integer, nullable=True)

    # Erro (se falhou)
    error_message = Column(Text, nullable=True)

    # Retry
    attempt = Column(Integer, default=1)
    next_retry_at = Column(DateTime, nullable=True)

    # Timestamp
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    def to_dict(self) -> dict:
        return {
            "delivery_id": self.delivery_id,
            "webhook_id": self.webhook_id,
            "event_type": self.event_type,
            "event_id": self.event_id,
            "success": self.success,
            "status_code": self.status_code,
            "response_time_ms": self.response_time_ms,
            "error_message": self.error_message,
            "attempt": self.attempt,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def __repr__(self):
        status = "OK" if self.success else "FAILED"
        return f"<WebhookDelivery {self.delivery_id}: {self.event_type} [{status}]>"


# =============================================================================
# API_REQUEST_LOG - Log de requisicoes para analytics
# =============================================================================

class APIRequestLog(Base):
    """
    Modelo para Log de Requisicoes da API

    Armazena metricas de uso por API Key para analytics e billing.
    """
    __tablename__ = "api_request_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # API Key
    api_key_id = Column(String(50), nullable=False, index=True)

    # Request
    method = Column(String(10), nullable=False)
    path = Column(String(500), nullable=False)
    query_params = Column(JSON, default=dict)

    # Response
    status_code = Column(Integer, nullable=False)
    response_time_ms = Column(Integer, nullable=True)

    # Metadata
    ip_address = Column(String(50), nullable=True)
    user_agent = Column(String(500), nullable=True)

    # Timestamp
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    # Indice para queries de analytics
    __table_args__ = (
        Index('idx_api_log_key_date', 'api_key_id', 'created_at'),
        Index('idx_api_log_path', 'path', 'created_at'),
    )

    def to_dict(self) -> dict:
        return {
            "api_key_id": self.api_key_id,
            "method": self.method,
            "path": self.path,
            "status_code": self.status_code,
            "response_time_ms": self.response_time_ms,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def __repr__(self):
        return f"<APIRequestLog {self.method} {self.path} [{self.status_code}]>"
