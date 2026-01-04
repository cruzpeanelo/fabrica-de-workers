# -*- coding: utf-8 -*-
"""
Handlers de Eventos para Notificacoes
Plataforma E v6.0

Este modulo conecta os eventos do sistema ao sistema de notificacoes.
Pode ser integrado com:
- Dashboard (Flask/Gradio)
- Workers
- Job Queue
- Kanban Watcher

Uso:
    from factory.notifications.event_handlers import NotificationEventHandler

    # Criar handler
    handler = NotificationEventHandler()

    # Registrar no sistema de eventos
    event_bus.register(handler.on_project_created)

    # Ou chamar diretamente
    await handler.on_story_completed(story_data)
"""

import asyncio
import logging
from datetime import datetime
from typing import Dict, Any, Optional, List

from .notification_manager import notification_manager, NotificationResult

logger = logging.getLogger(__name__)


class NotificationEventHandler:
    """
    Handler de eventos que dispara notificacoes.

    Conecta eventos do sistema ao NotificationManager.
    """

    def __init__(self):
        """Inicializa o handler"""
        self.enabled = True
        self._event_history: List[Dict[str, Any]] = []
        self._max_history = 100

    # =========================================================================
    # EVENTOS DE PROJETO
    # =========================================================================

    async def on_project_created(
        self,
        project_id: str,
        project_name: str,
        project_type: str,
        created_by: str,
        description: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um projeto e criado.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            project_type: Tipo do projeto
            created_by: Usuario que criou
            description: Descricao do projeto
        """
        data = {
            "project_id": project_id,
            "project_name": project_name,
            "project_type": project_type,
            "created_by": created_by,
            "description": description,
            "created_at": datetime.utcnow().strftime("%d/%m/%Y %H:%M"),
            **kwargs
        }

        return await self._notify("project_created", data, context={
            "project_id": project_id,
            "owner": created_by
        })

    async def on_project_completed(
        self,
        project_id: str,
        project_name: str,
        duration: str,
        files_count: int,
        summary: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um projeto e concluido.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            duration: Tempo de execucao
            files_count: Numero de arquivos gerados
            summary: Resumo do projeto
        """
        data = {
            "project_id": project_id,
            "project_name": project_name,
            "duration": duration,
            "files_count": files_count,
            "summary": summary,
            **kwargs
        }

        return await self._notify("project_completed", data, context={
            "project_id": project_id
        })

    # =========================================================================
    # EVENTOS DE STORY
    # =========================================================================

    async def on_story_created(
        self,
        story_id: str,
        title: str,
        project_id: str,
        project_name: str,
        persona: str = "",
        action: str = "",
        benefit: str = "",
        priority: str = "medium",
        story_points: int = 0,
        acceptance_criteria: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma story e criada.
        """
        data = {
            "story_id": story_id,
            "title": title,
            "project_id": project_id,
            "project_name": project_name,
            "persona": persona,
            "action": action,
            "benefit": benefit,
            "priority": priority,
            "story_points": story_points,
            "acceptance_criteria": acceptance_criteria,
            **kwargs
        }

        return await self._notify("story_created", data, context={
            "project_id": project_id,
            "priority": priority
        })

    async def on_story_started(
        self,
        story_id: str,
        title: str,
        assignee: str,
        sprint_name: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma story comeca a ser trabalhada.
        """
        data = {
            "story_id": story_id,
            "title": title,
            "assignee": assignee,
            "sprint_name": sprint_name,
            **kwargs
        }

        return await self._notify("story_started", data, context={
            "assignee": assignee
        })

    async def on_story_completed(
        self,
        story_id: str,
        title: str,
        assignee: str,
        duration: str = "",
        tasks_completed: int = 0,
        tasks_total: int = 0,
        tasks_summary: str = "",
        files_list: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma story e finalizada.
        """
        data = {
            "story_id": story_id,
            "title": title,
            "assignee": assignee,
            "duration": duration,
            "tasks_completed": tasks_completed,
            "tasks_total": tasks_total,
            "tasks_summary": tasks_summary,
            "files_list": files_list,
            **kwargs
        }

        return await self._notify("story_completed", data, context={
            "assignee": assignee
        })

    async def on_story_blocked(
        self,
        story_id: str,
        title: str,
        assignee: str,
        blocked_reason: str,
        blocked_by: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma story e bloqueada.
        """
        data = {
            "story_id": story_id,
            "title": title,
            "assignee": assignee,
            "blocked_reason": blocked_reason,
            "blocked_by": blocked_by,
            **kwargs
        }

        return await self._notify("story_blocked", data, context={
            "assignee": assignee
        }, priority="high")

    async def on_story_moved(
        self,
        story_id: str,
        title: str,
        from_status: str,
        to_status: str,
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma story e movida no Kanban.
        """
        data = {
            "story_id": story_id,
            "title": title,
            "from_status": from_status,
            "to_status": to_status,
            **kwargs
        }

        return await self._notify("story_moved", data)

    # =========================================================================
    # EVENTOS DE TASK
    # =========================================================================

    async def on_task_completed(
        self,
        task_id: str,
        title: str,
        story_id: str,
        task_type: str = "development",
        story_progress: float = 0,
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma task e completada.
        """
        data = {
            "task_id": task_id,
            "title": title,
            "story_id": story_id,
            "task_type": task_type,
            "story_progress": f"{story_progress:.0f}",
            **kwargs
        }

        return await self._notify("task_completed", data)

    async def on_task_failed(
        self,
        task_id: str,
        title: str,
        story_id: str,
        error_message: str,
        error_details: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando uma task falha.
        """
        data = {
            "task_id": task_id,
            "title": title,
            "story_id": story_id,
            "error_message": error_message,
            "error_details": error_details,
            **kwargs
        }

        return await self._notify("task_failed", data, priority="high")

    # =========================================================================
    # EVENTOS DE JOB
    # =========================================================================

    async def on_job_started(
        self,
        job_id: str,
        project_name: str,
        worker_id: str,
        description: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um job e iniciado.
        """
        data = {
            "job_id": job_id,
            "project_name": project_name,
            "worker_id": worker_id,
            "description": description,
            **kwargs
        }

        return await self._notify("job_started", data)

    async def on_job_completed(
        self,
        job_id: str,
        project_name: str,
        duration: str,
        files_count: int = 0,
        output_path: str = "",
        summary: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um job e concluido.
        """
        data = {
            "job_id": job_id,
            "project_name": project_name,
            "duration": duration,
            "files_count": files_count,
            "output_path": output_path,
            "summary": summary,
            **kwargs
        }

        return await self._notify("job_completed", data)

    async def on_job_failed(
        self,
        job_id: str,
        project_name: str,
        current_step: str,
        error_message: str,
        attempt: int = 1,
        max_attempts: int = 5,
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um job falha.
        """
        data = {
            "job_id": job_id,
            "project_name": project_name,
            "current_step": current_step,
            "error_message": error_message,
            "attempt": attempt,
            "max_attempts": max_attempts,
            **kwargs
        }

        return await self._notify("job_failed", data, priority="high")

    # =========================================================================
    # EVENTOS DE SISTEMA
    # =========================================================================

    async def on_error(
        self,
        error_type: str,
        error_message: str,
        severity: str = "error",
        component: str = "system",
        stack_trace: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica sobre erros do sistema.
        """
        data = {
            "error_type": error_type,
            "error_message": error_message,
            "severity": severity,
            "component": component,
            "stack_trace": stack_trace,
            "timestamp": datetime.utcnow().strftime("%d/%m/%Y %H:%M:%S"),
            **kwargs
        }

        priority = "urgent" if severity in ["critical", "fatal"] else "high"
        return await self._notify("error", data, priority=priority)

    async def on_worker_offline(
        self,
        worker_id: str,
        last_heartbeat: str,
        pending_jobs: int = 0,
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um worker fica offline.
        """
        data = {
            "worker_id": worker_id,
            "last_heartbeat": last_heartbeat,
            "pending_jobs": pending_jobs,
            **kwargs
        }

        return await self._notify("worker_offline", data, priority="high")

    async def on_limit_alert(
        self,
        limit_type: str,
        current_usage: str,
        limit_value: str,
        percentage: float,
        resource_name: str = "",
        **kwargs
    ) -> NotificationResult:
        """
        Notifica quando um limite e atingido.
        """
        data = {
            "limit_type": limit_type,
            "current_usage": current_usage,
            "limit_value": limit_value,
            "percentage": f"{percentage:.1f}",
            "resource_name": resource_name,
            **kwargs
        }

        priority = "urgent" if percentage >= 95 else "high"
        return await self._notify("limit_alert", data, priority=priority)

    # =========================================================================
    # METODOS AUXILIARES
    # =========================================================================

    async def _notify(
        self,
        event_type: str,
        data: Dict[str, Any],
        context: Dict[str, Any] = None,
        priority: str = "normal",
        recipients: List[str] = None,
        channels: List[str] = None
    ) -> NotificationResult:
        """
        Metodo interno para enviar notificacao.
        """
        if not self.enabled:
            logger.debug(f"Handler desabilitado, ignorando evento {event_type}")
            return None

        # Registrar no historico
        self._add_to_history(event_type, data)

        # Enviar notificacao
        result = await notification_manager.notify(
            event_type=event_type,
            data=data,
            recipients=recipients,
            channels=channels,
            priority=priority,
            context=context or {}
        )

        if result.success:
            logger.info(f"Evento {event_type} notificado: {result.channels_sent}")
        else:
            logger.warning(f"Falha ao notificar evento {event_type}")

        return result

    def _add_to_history(self, event_type: str, data: Dict[str, Any]) -> None:
        """Adiciona evento ao historico"""
        self._event_history.append({
            "event_type": event_type,
            "data": data,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Manter tamanho maximo
        if len(self._event_history) > self._max_history:
            self._event_history = self._event_history[-self._max_history:]

    def get_history(self, limit: int = 50) -> List[Dict[str, Any]]:
        """Retorna historico de eventos"""
        return self._event_history[-limit:]

    def enable(self) -> None:
        """Habilita o handler"""
        self.enabled = True

    def disable(self) -> None:
        """Desabilita o handler"""
        self.enabled = False


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_handler_instance: Optional[NotificationEventHandler] = None


def get_event_handler() -> NotificationEventHandler:
    """Retorna instancia global do handler"""
    global _handler_instance
    if _handler_instance is None:
        _handler_instance = NotificationEventHandler()
    return _handler_instance


# Alias para facilitar uso
event_handler = get_event_handler()


# =============================================================================
# DECORADORES PARA INTEGRACAO
# =============================================================================

def notify_on_complete(event_type: str, data_mapper: callable = None):
    """
    Decorador que notifica quando uma funcao completa com sucesso.

    Uso:
        @notify_on_complete("job_completed", lambda result: {"job_id": result.job_id})
        async def process_job(job_id):
            ...
            return result
    """
    def decorator(func):
        async def wrapper(*args, **kwargs):
            result = await func(*args, **kwargs)

            if data_mapper:
                data = data_mapper(result)
            else:
                data = {"result": str(result)}

            await notification_manager.notify(event_type=event_type, data=data)
            return result

        return wrapper
    return decorator


def notify_on_error(event_type: str = "error"):
    """
    Decorador que notifica quando uma funcao lanca excecao.

    Uso:
        @notify_on_error()
        async def risky_operation():
            ...
    """
    def decorator(func):
        async def wrapper(*args, **kwargs):
            try:
                return await func(*args, **kwargs)
            except Exception as e:
                await event_handler.on_error(
                    error_type=type(e).__name__,
                    error_message=str(e),
                    component=func.__module__,
                    severity="error"
                )
                raise

        return wrapper
    return decorator
