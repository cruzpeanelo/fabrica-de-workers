# -*- coding: utf-8 -*-
"""
Interface Base para Canais de Notificacao
Fabrica de Agentes v6.0

Define a interface abstrata que todos os canais devem implementar.
Padrao Strategy para permitir adicionar novos canais facilmente.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Any
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class MessageFormat(str, Enum):
    """Formato da mensagem"""
    PLAIN_TEXT = "plain_text"
    HTML = "html"
    MARKDOWN = "markdown"
    ADAPTIVE_CARD = "adaptive_card"  # Para Teams
    BLOCKS = "blocks"  # Para Slack


@dataclass
class NotificationMessage:
    """
    Representa uma mensagem de notificacao padronizada.
    Todos os canais recebem este objeto e convertem para seu formato especifico.
    """
    # Identificacao
    notification_id: str
    event_type: str

    # Conteudo
    subject: str  # Assunto (para email) ou titulo
    body: str  # Corpo principal da mensagem
    body_html: Optional[str] = None  # Versao HTML (para email)

    # Destinatarios
    recipients: List[str] = field(default_factory=list)

    # Prioridade
    priority: str = "normal"  # low, normal, high, urgent

    # Dados adicionais para templates
    data: Dict[str, Any] = field(default_factory=dict)

    # Metadados
    timestamp: datetime = field(default_factory=datetime.utcnow)
    source: str = "fabrica-agentes"

    # Anexos (para email)
    attachments: List[Dict[str, Any]] = field(default_factory=list)

    # Acoes/Botoes (para Slack/Teams)
    actions: List[Dict[str, Any]] = field(default_factory=list)

    # Formato preferido
    format: MessageFormat = MessageFormat.PLAIN_TEXT

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "notification_id": self.notification_id,
            "event_type": self.event_type,
            "subject": self.subject,
            "body": self.body,
            "body_html": self.body_html,
            "recipients": self.recipients,
            "priority": self.priority,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
            "source": self.source,
            "attachments": self.attachments,
            "actions": self.actions,
            "format": self.format.value
        }


@dataclass
class ChannelResponse:
    """
    Resposta padronizada de um canal apos tentativa de envio.
    """
    success: bool
    channel_type: str
    notification_id: str

    # Identificador do provedor (message_id, etc)
    provider_id: Optional[str] = None

    # Erro (se falhou)
    error_message: Optional[str] = None
    error_code: Optional[str] = None

    # Resposta bruta do provedor
    raw_response: Optional[Dict[str, Any]] = None

    # Timestamp
    timestamp: datetime = field(default_factory=datetime.utcnow)

    # Metricas
    latency_ms: Optional[int] = None

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "success": self.success,
            "channel_type": self.channel_type,
            "notification_id": self.notification_id,
            "provider_id": self.provider_id,
            "error_message": self.error_message,
            "error_code": self.error_code,
            "raw_response": self.raw_response,
            "timestamp": self.timestamp.isoformat(),
            "latency_ms": self.latency_ms
        }


class BaseChannel(ABC):
    """
    Classe base abstrata para todos os canais de notificacao.

    Todos os canais devem implementar:
    - send(): Envia uma mensagem
    - validate_config(): Valida a configuracao do canal
    - test_connection(): Testa se a conexao esta funcionando
    - format_message(): Converte mensagem para formato do canal
    """

    def __init__(self, config: Dict[str, Any]):
        """
        Inicializa o canal com sua configuracao.

        Args:
            config: Dicionario com configuracoes do canal
        """
        self.config = config
        self.channel_type = self._get_channel_type()
        self._validate_required_config()

    @abstractmethod
    def _get_channel_type(self) -> str:
        """Retorna o tipo do canal (email, slack, teams, etc)"""
        pass

    @abstractmethod
    def _get_required_config_keys(self) -> List[str]:
        """Retorna lista de chaves obrigatorias na configuracao"""
        pass

    @abstractmethod
    async def send(self, message: NotificationMessage) -> ChannelResponse:
        """
        Envia uma mensagem pelo canal.

        Args:
            message: Mensagem a ser enviada

        Returns:
            ChannelResponse com resultado do envio
        """
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """
        Testa se a conexao com o provedor esta funcionando.

        Returns:
            True se a conexao esta ok, False caso contrario
        """
        pass

    @abstractmethod
    def format_message(self, message: NotificationMessage) -> Any:
        """
        Converte a mensagem padronizada para o formato do canal.

        Args:
            message: Mensagem padronizada

        Returns:
            Mensagem no formato especifico do canal
        """
        pass

    def _validate_required_config(self) -> None:
        """Valida se todas as configuracoes obrigatorias estao presentes"""
        required = self._get_required_config_keys()
        missing = [key for key in required if key not in self.config]

        if missing:
            raise ValueError(
                f"Configuracoes obrigatorias ausentes para canal {self.channel_type}: {missing}"
            )

    def validate_config(self) -> Dict[str, Any]:
        """
        Valida a configuracao do canal.

        Returns:
            Dicionario com resultado da validacao
        """
        result = {
            "valid": True,
            "errors": [],
            "warnings": []
        }

        try:
            self._validate_required_config()
        except ValueError as e:
            result["valid"] = False
            result["errors"].append(str(e))

        return result

    def get_priority_emoji(self, priority: str) -> str:
        """Retorna emoji baseado na prioridade"""
        emojis = {
            "low": "",
            "normal": "",
            "high": "",
            "urgent": ""
        }
        return emojis.get(priority, "")

    def get_event_emoji(self, event_type: str) -> str:
        """Retorna emoji baseado no tipo de evento"""
        emojis = {
            # Projetos
            "project_created": "",
            "project_completed": "",
            "project_archived": "",

            # Stories
            "story_created": "",
            "story_started": "",
            "story_completed": "",
            "story_blocked": "",
            "story_moved": "",

            # Tasks
            "task_created": "",
            "task_completed": "",
            "task_failed": "",

            # Jobs
            "job_queued": "",
            "job_started": "",
            "job_completed": "",
            "job_failed": "",

            # Sistema
            "error": "",
            "warning": "",
            "approval_required": "",
            "report_daily": "",
            "report_weekly": "",
            "limit_alert": "",

            # Worker
            "worker_online": "",
            "worker_offline": ""
        }
        return emojis.get(event_type, "")

    def get_priority_color(self, priority: str) -> str:
        """Retorna cor hex baseada na prioridade"""
        colors = {
            "low": "#6B7280",      # Cinza
            "normal": "#3B82F6",   # Azul
            "high": "#F59E0B",     # Laranja
            "urgent": "#EF4444"    # Vermelho
        }
        return colors.get(priority, "#3B82F6")

    def truncate_text(self, text: str, max_length: int = 200) -> str:
        """Trunca texto para caber em campos limitados"""
        if len(text) <= max_length:
            return text
        return text[:max_length - 3] + "..."

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} channel_type={self.channel_type}>"
