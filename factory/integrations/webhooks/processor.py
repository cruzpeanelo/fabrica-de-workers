# -*- coding: utf-8 -*-
"""
Webhook Event Processor
=======================
Processador assíncrono de eventos de webhook.

Issue #363 - Terminal A

Funcionalidades:
- Fila de processamento assíncrono
- Mapeamento para eventos internos
- Retry com backoff exponencial
- Logging de auditoria
"""

import asyncio
import logging
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Deque
import uuid

logger = logging.getLogger(__name__)


class EventStatus(str, Enum):
    """Status de processamento do evento"""
    PENDING = "pending"
    PROCESSING = "processing"
    COMPLETED = "completed"
    FAILED = "failed"
    RETRYING = "retrying"


class WebhookSource(str, Enum):
    """Fonte do webhook"""
    JIRA = "jira"
    GITHUB = "github"
    AZURE_DEVOPS = "azure_devops"
    SALESFORCE = "salesforce"
    INTERNAL = "internal"


@dataclass
class WebhookEvent:
    """Evento de webhook para processamento"""
    event_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    source: WebhookSource = WebhookSource.INTERNAL
    event_type: str = ""
    payload: Dict[str, Any] = field(default_factory=dict)
    tenant_id: str = ""
    received_at: datetime = field(default_factory=datetime.utcnow)
    status: EventStatus = EventStatus.PENDING
    retry_count: int = 0
    max_retries: int = 3
    next_retry: Optional[datetime] = None
    error_message: Optional[str] = None
    processed_at: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "event_id": self.event_id,
            "source": self.source.value,
            "event_type": self.event_type,
            "tenant_id": self.tenant_id,
            "received_at": self.received_at.isoformat(),
            "status": self.status.value,
            "retry_count": self.retry_count,
            "error_message": self.error_message,
            "processed_at": self.processed_at.isoformat() if self.processed_at else None
        }


# Mapeamento de eventos externos para internos
EVENT_MAPPINGS: Dict[WebhookSource, Dict[str, str]] = {
    WebhookSource.JIRA: {
        "jira:issue_created": "story.created",
        "jira:issue_updated": "story.updated",
        "jira:issue_deleted": "story.deleted",
        "jira:sprint_started": "sprint.started",
        "jira:sprint_closed": "sprint.completed",
        "jira:comment_created": "comment.created",
        "jira:comment_updated": "comment.updated",
    },
    WebhookSource.GITHUB: {
        "push": "code.pushed",
        "pull_request.opened": "pr.created",
        "pull_request.closed": "pr.closed",
        "pull_request.merged": "pr.merged",
        "issues.opened": "story.created",
        "issues.closed": "story.completed",
        "workflow_run.completed": "job.completed",
    },
    WebhookSource.AZURE_DEVOPS: {
        "workitem.created": "story.created",
        "workitem.updated": "story.updated",
        "workitem.deleted": "story.deleted",
        "build.complete": "job.completed",
        "release.deployment.completed": "deploy.completed",
        "git.push": "code.pushed",
    },
    WebhookSource.SALESFORCE: {
        "case.created": "story.created",
        "case.updated": "story.updated",
        "case.closed": "story.completed",
        "opportunity.won": "deal.won",
        "opportunity.lost": "deal.lost",
    }
}


# Tipo para handlers de eventos
EventHandler = Callable[[WebhookEvent], Any]


class WebhookEventProcessor:
    """
    Processador de eventos de webhook.

    Implementa fila assíncrona com retry e backoff exponencial.

    Exemplo:
        processor = WebhookEventProcessor()

        # Registrar handler
        @processor.on("story.created")
        async def handle_story_created(event: WebhookEvent):
            print(f"Story criada: {event.payload}")

        # Iniciar processamento
        await processor.start()

        # Enfileirar evento
        await processor.enqueue(event)

        # Parar
        await processor.stop()
    """

    DEFAULT_MAX_QUEUE_SIZE = 10000
    DEFAULT_WORKERS = 4
    DEFAULT_BATCH_SIZE = 10

    def __init__(
        self,
        max_queue_size: int = DEFAULT_MAX_QUEUE_SIZE,
        num_workers: int = DEFAULT_WORKERS,
        batch_size: int = DEFAULT_BATCH_SIZE
    ):
        self.max_queue_size = max_queue_size
        self.num_workers = num_workers
        self.batch_size = batch_size

        self._queue: Deque[WebhookEvent] = deque(maxlen=max_queue_size)
        self._retry_queue: Deque[WebhookEvent] = deque(maxlen=max_queue_size)
        self._handlers: Dict[str, List[EventHandler]] = {}
        self._workers: List[asyncio.Task] = []
        self._running = False
        self._processed_count = 0
        self._failed_count = 0
        self._event_log: Deque[Dict] = deque(maxlen=1000)

    def on(self, event_type: str):
        """Decorator para registrar handler de evento"""
        def decorator(func: EventHandler):
            self.register_handler(event_type, func)
            return func
        return decorator

    def register_handler(self, event_type: str, handler: EventHandler):
        """Registra handler para tipo de evento"""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)
        logger.debug(f"Handler registrado para {event_type}")

    def map_event_type(self, source: WebhookSource, external_type: str) -> str:
        """Mapeia tipo de evento externo para interno"""
        mappings = EVENT_MAPPINGS.get(source, {})
        return mappings.get(external_type, f"{source.value}.{external_type}")

    async def enqueue(self, event: WebhookEvent) -> bool:
        """
        Adiciona evento à fila de processamento.

        Args:
            event: Evento a processar

        Returns:
            True se enfileirado com sucesso
        """
        if len(self._queue) >= self.max_queue_size:
            logger.warning("Queue cheio, evento descartado")
            return False

        # Mapeia tipo de evento se necessário
        if event.source != WebhookSource.INTERNAL:
            mapped_type = self.map_event_type(event.source, event.event_type)
            event.metadata["original_event_type"] = event.event_type
            event.event_type = mapped_type

        self._queue.append(event)
        logger.debug(f"Evento {event.event_id} enfileirado: {event.event_type}")
        return True

    async def enqueue_from_webhook(
        self,
        source: WebhookSource,
        event_type: str,
        payload: Dict[str, Any],
        tenant_id: str = ""
    ) -> WebhookEvent:
        """
        Cria e enfileira evento a partir de webhook.

        Args:
            source: Fonte do webhook
            event_type: Tipo do evento
            payload: Dados do evento
            tenant_id: ID do tenant

        Returns:
            Evento criado
        """
        event = WebhookEvent(
            source=source,
            event_type=event_type,
            payload=payload,
            tenant_id=tenant_id
        )
        await self.enqueue(event)
        return event

    async def start(self):
        """Inicia workers de processamento"""
        if self._running:
            return

        self._running = True

        # Inicia workers
        for i in range(self.num_workers):
            worker = asyncio.create_task(self._worker_loop(i))
            self._workers.append(worker)

        # Inicia loop de retry
        retry_worker = asyncio.create_task(self._retry_loop())
        self._workers.append(retry_worker)

        logger.info(f"Processor iniciado com {self.num_workers} workers")

    async def stop(self):
        """Para workers de processamento"""
        if not self._running:
            return

        self._running = False

        # Cancela workers
        for worker in self._workers:
            worker.cancel()

        # Aguarda cancelamento
        await asyncio.gather(*self._workers, return_exceptions=True)
        self._workers.clear()

        logger.info("Processor parado")

    async def _worker_loop(self, worker_id: int):
        """Loop do worker de processamento"""
        logger.debug(f"Worker {worker_id} iniciado")

        while self._running:
            try:
                # Busca eventos da fila
                events = []
                for _ in range(self.batch_size):
                    if self._queue:
                        events.append(self._queue.popleft())
                    else:
                        break

                if not events:
                    await asyncio.sleep(0.1)
                    continue

                # Processa batch
                for event in events:
                    await self._process_event(event)

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Worker {worker_id} error: {e}")
                await asyncio.sleep(1)

        logger.debug(f"Worker {worker_id} finalizado")

    async def _retry_loop(self):
        """Loop de retry para eventos falhos"""
        while self._running:
            try:
                now = datetime.utcnow()

                # Processa eventos prontos para retry
                ready_for_retry = []
                remaining = deque()

                while self._retry_queue:
                    event = self._retry_queue.popleft()
                    if event.next_retry and now >= event.next_retry:
                        ready_for_retry.append(event)
                    else:
                        remaining.append(event)

                self._retry_queue = remaining

                # Reenfileira eventos para retry
                for event in ready_for_retry:
                    event.status = EventStatus.RETRYING
                    event.retry_count += 1
                    self._queue.append(event)

                await asyncio.sleep(1)

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Retry loop error: {e}")
                await asyncio.sleep(5)

    async def _process_event(self, event: WebhookEvent):
        """Processa um evento"""
        event.status = EventStatus.PROCESSING

        try:
            handlers = self._handlers.get(event.event_type, [])

            if not handlers:
                # Tenta handler genérico
                handlers = self._handlers.get("*", [])

            if not handlers:
                logger.debug(f"Nenhum handler para {event.event_type}")
                event.status = EventStatus.COMPLETED
                event.processed_at = datetime.utcnow()
                self._log_event(event, "no_handler")
                return

            # Executa handlers
            for handler in handlers:
                try:
                    result = handler(event)
                    if asyncio.iscoroutine(result):
                        await result
                except Exception as e:
                    logger.error(f"Handler error for {event.event_type}: {e}")
                    raise

            # Sucesso
            event.status = EventStatus.COMPLETED
            event.processed_at = datetime.utcnow()
            self._processed_count += 1
            self._log_event(event, "success")

        except Exception as e:
            event.error_message = str(e)

            if event.retry_count < event.max_retries:
                # Agenda retry com backoff exponencial
                delay = 2 ** event.retry_count  # 1, 2, 4, 8, ...
                event.next_retry = datetime.utcnow() + timedelta(seconds=delay)
                event.status = EventStatus.PENDING
                self._retry_queue.append(event)
                self._log_event(event, "retry_scheduled")
                logger.warning(
                    f"Evento {event.event_id} falhou, retry em {delay}s: {e}"
                )
            else:
                # Falha permanente
                event.status = EventStatus.FAILED
                self._failed_count += 1
                self._log_event(event, "failed")
                logger.error(
                    f"Evento {event.event_id} falhou permanentemente: {e}"
                )

    def _log_event(self, event: WebhookEvent, result: str):
        """Loga evento para auditoria"""
        log_entry = {
            "event_id": event.event_id,
            "source": event.source.value,
            "event_type": event.event_type,
            "tenant_id": event.tenant_id,
            "result": result,
            "retry_count": event.retry_count,
            "timestamp": datetime.utcnow().isoformat(),
            "error": event.error_message
        }
        self._event_log.append(log_entry)

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatísticas do processor"""
        return {
            "running": self._running,
            "queue_size": len(self._queue),
            "retry_queue_size": len(self._retry_queue),
            "workers": len(self._workers),
            "processed_count": self._processed_count,
            "failed_count": self._failed_count,
            "registered_handlers": len(self._handlers)
        }

    def get_event_log(self, limit: int = 100) -> List[Dict]:
        """Retorna log de eventos recentes"""
        return list(self._event_log)[-limit:]


# Instância global do processor
_processor: Optional[WebhookEventProcessor] = None


def get_processor() -> WebhookEventProcessor:
    """Obtém instância global do processor"""
    global _processor
    if _processor is None:
        _processor = WebhookEventProcessor()
    return _processor


async def process_webhook_event(
    source: WebhookSource,
    event_type: str,
    payload: Dict[str, Any],
    tenant_id: str = ""
) -> WebhookEvent:
    """
    Helper para processar evento de webhook.

    Args:
        source: Fonte do webhook
        event_type: Tipo do evento
        payload: Dados do evento
        tenant_id: ID do tenant

    Returns:
        Evento criado
    """
    processor = get_processor()
    return await processor.enqueue_from_webhook(
        source=source,
        event_type=event_type,
        payload=payload,
        tenant_id=tenant_id
    )
