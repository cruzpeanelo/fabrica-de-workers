# -*- coding: utf-8 -*-
"""
WebSocket Events - Plataforma E
=====================================
Definicao de tipos de eventos e estrutura de mensagens WebSocket.

Eventos Disponiveis:
- connection: Conexao estabelecida
- heartbeat: Ping/pong para manter conexao ativa
- story_created: Nova story criada
- story_updated: Story atualizada
- story_moved: Story movida no Kanban
- story_deleted: Story removida
- task_created: Nova task criada
- task_updated: Task atualizada
- task_completed: Task concluida
- task_deleted: Task removida
- chat_message: Nova mensagem no chat
- project_updated: Projeto atualizado
- notification: Notificacao generica
- error: Erro no processamento
"""

from enum import Enum
from datetime import datetime
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field, asdict
import json


class EventType(str, Enum):
    """
    Tipos de eventos WebSocket suportados.

    Categorias:
    - Conexao: connection, heartbeat, subscribed, unsubscribed
    - Stories: story_created, story_updated, story_moved, story_deleted
    - Tasks: task_created, task_updated, task_completed, task_deleted
    - Chat: chat_message
    - Sistema: notification, error, broadcast
    """
    # Eventos de Conexao
    CONNECTION = "connection"
    HEARTBEAT = "heartbeat"
    PING = "ping"
    PONG = "pong"
    SUBSCRIBED = "subscribed"
    UNSUBSCRIBED = "unsubscribed"

    # Eventos de Story
    STORY_CREATED = "story_created"
    STORY_UPDATED = "story_updated"
    STORY_MOVED = "story_moved"
    STORY_DELETED = "story_deleted"

    # Eventos de Task
    TASK_CREATED = "task_created"
    TASK_UPDATED = "task_updated"
    TASK_COMPLETED = "task_completed"
    TASK_DELETED = "task_deleted"

    # Eventos de Chat
    CHAT_MESSAGE = "chat_message"

    # Eventos de Projeto
    PROJECT_UPDATED = "project_updated"
    PROJECT_CREATED = "project_created"

    # Eventos do Sistema
    NOTIFICATION = "notification"
    ERROR = "error"
    BROADCAST = "broadcast"

    # Eventos de Job/Worker
    JOB_STARTED = "job_started"
    JOB_COMPLETED = "job_completed"
    JOB_FAILED = "job_failed"
    JOB_PROGRESS = "job_progress"

    # Eventos de Log
    LOG_MESSAGE = "log_message"


@dataclass
class WebSocketEvent:
    """
    Estrutura padrao de evento WebSocket.

    Atributos:
        type: Tipo do evento (EventType)
        data: Dados do evento
        timestamp: Data/hora do evento (ISO 8601)
        project_id: ID do projeto (para roteamento por grupo)
        story_id: ID da story relacionada (opcional)
        task_id: ID da task relacionada (opcional)
        user_id: ID do usuario que gerou o evento (opcional)
        message_id: ID unico da mensagem (para deduplicacao)

    Exemplo de uso:
        event = WebSocketEvent(
            type=EventType.STORY_CREATED,
            data={"story_id": "STR-0001", "title": "Minha Story"},
            project_id="PROJ-001"
        )
        json_str = event.to_json()
    """
    type: EventType
    data: Dict[str, Any] = field(default_factory=dict)
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat() + "Z")
    project_id: Optional[str] = None
    story_id: Optional[str] = None
    task_id: Optional[str] = None
    user_id: Optional[str] = None
    message_id: Optional[str] = None

    def __post_init__(self):
        """Gera message_id se nao fornecido."""
        if not self.message_id:
            import uuid
            self.message_id = str(uuid.uuid4())[:8]

    def to_dict(self) -> Dict[str, Any]:
        """Converte evento para dicionario."""
        result = {
            "type": self.type.value if isinstance(self.type, EventType) else self.type,
            "data": self.data,
            "timestamp": self.timestamp,
            "message_id": self.message_id
        }
        if self.project_id:
            result["project_id"] = self.project_id
        if self.story_id:
            result["story_id"] = self.story_id
        if self.task_id:
            result["task_id"] = self.task_id
        if self.user_id:
            result["user_id"] = self.user_id
        return result

    def to_json(self) -> str:
        """Converte evento para JSON string."""
        return json.dumps(self.to_dict(), ensure_ascii=False)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'WebSocketEvent':
        """Cria evento a partir de dicionario."""
        event_type = data.get("type", "notification")
        if isinstance(event_type, str):
            try:
                event_type = EventType(event_type)
            except ValueError:
                event_type = EventType.NOTIFICATION

        return cls(
            type=event_type,
            data=data.get("data", {}),
            timestamp=data.get("timestamp", datetime.utcnow().isoformat() + "Z"),
            project_id=data.get("project_id"),
            story_id=data.get("story_id"),
            task_id=data.get("task_id"),
            user_id=data.get("user_id"),
            message_id=data.get("message_id")
        )

    @classmethod
    def from_json(cls, json_str: str) -> 'WebSocketEvent':
        """Cria evento a partir de JSON string."""
        return cls.from_dict(json.loads(json_str))


# =============================================================================
# FUNCOES HELPER PARA CRIAR EVENTOS
# =============================================================================

def create_connection_event(status: str = "connected", message: str = "Conectado ao servidor") -> WebSocketEvent:
    """Cria evento de conexao."""
    return WebSocketEvent(
        type=EventType.CONNECTION,
        data={"status": status, "message": message}
    )


def create_heartbeat_event() -> WebSocketEvent:
    """Cria evento de heartbeat."""
    return WebSocketEvent(
        type=EventType.HEARTBEAT,
        data={"status": "alive"}
    )


def create_pong_event() -> WebSocketEvent:
    """Cria evento pong em resposta a ping."""
    return WebSocketEvent(
        type=EventType.PONG,
        data={"status": "pong"}
    )


def create_story_event(
    event_type: EventType,
    story_id: str,
    title: str,
    project_id: str,
    extra_data: Optional[Dict[str, Any]] = None
) -> WebSocketEvent:
    """Cria evento relacionado a story."""
    data = {
        "story_id": story_id,
        "title": title,
        "project_id": project_id
    }
    if extra_data:
        data.update(extra_data)

    return WebSocketEvent(
        type=event_type,
        data=data,
        project_id=project_id,
        story_id=story_id
    )


def create_task_event(
    event_type: EventType,
    task_id: str,
    story_id: str,
    title: str,
    project_id: Optional[str] = None,
    extra_data: Optional[Dict[str, Any]] = None
) -> WebSocketEvent:
    """Cria evento relacionado a task."""
    data = {
        "task_id": task_id,
        "story_id": story_id,
        "title": title
    }
    if project_id:
        data["project_id"] = project_id
    if extra_data:
        data.update(extra_data)

    return WebSocketEvent(
        type=event_type,
        data=data,
        project_id=project_id,
        story_id=story_id,
        task_id=task_id
    )


def create_chat_event(
    message_preview: str,
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    user_id: Optional[str] = None
) -> WebSocketEvent:
    """Cria evento de chat."""
    return WebSocketEvent(
        type=EventType.CHAT_MESSAGE,
        data={"preview": message_preview[:100]},
        project_id=project_id,
        story_id=story_id,
        user_id=user_id
    )


def create_notification_event(
    title: str,
    message: str,
    level: str = "info",
    project_id: Optional[str] = None
) -> WebSocketEvent:
    """
    Cria evento de notificacao generica.

    Args:
        title: Titulo da notificacao
        message: Mensagem da notificacao
        level: Nivel (info, success, warning, error)
        project_id: ID do projeto (opcional)
    """
    return WebSocketEvent(
        type=EventType.NOTIFICATION,
        data={"title": title, "message": message, "level": level},
        project_id=project_id
    )


def create_error_event(
    error_message: str,
    error_code: Optional[str] = None,
    project_id: Optional[str] = None
) -> WebSocketEvent:
    """Cria evento de erro."""
    data = {"message": error_message}
    if error_code:
        data["code"] = error_code

    return WebSocketEvent(
        type=EventType.ERROR,
        data=data,
        project_id=project_id
    )


def create_job_event(
    event_type: EventType,
    job_id: str,
    status: str,
    progress: Optional[int] = None,
    output: Optional[str] = None,
    project_id: Optional[str] = None
) -> WebSocketEvent:
    """Cria evento relacionado a job/worker."""
    data = {
        "job_id": job_id,
        "status": status
    }
    if progress is not None:
        data["progress"] = progress
    if output:
        data["output"] = output[:500]  # Limita tamanho

    return WebSocketEvent(
        type=event_type,
        data=data,
        project_id=project_id
    )


def create_log_event(
    level: str,
    message: str,
    source: Optional[str] = None,
    project_id: Optional[str] = None
) -> WebSocketEvent:
    """Cria evento de log para streaming."""
    data = {
        "level": level,
        "message": message
    }
    if source:
        data["source"] = source

    return WebSocketEvent(
        type=EventType.LOG_MESSAGE,
        data=data,
        project_id=project_id
    )
