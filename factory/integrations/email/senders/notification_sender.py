# -*- coding: utf-8 -*-
"""
Notification Sender
===================
Enviador de notificacoes automaticas por email.

Tipos de notificacoes:
- Projeto criado
- Projeto concluido
- Story atualizada
- Erro detectado
- Alerta de sistema

Uso:
    from factory.integrations.email.senders import NotificationSender

    sender = NotificationSender(graph_client)
    await sender.notify_project_created(project_data, recipients)
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, Union
from enum import Enum
from pathlib import Path

logger = logging.getLogger(__name__)


class NotificationType(str, Enum):
    """Tipos de notificacao"""
    PROJECT_CREATED = "project_created"
    PROJECT_COMPLETED = "project_completed"
    PROJECT_ERROR = "project_error"
    STORY_STATUS_UPDATE = "story_status_update"
    STORY_COMPLETED = "story_completed"
    ERROR_ALERT = "error_alert"
    WARNING_ALERT = "warning_alert"
    INFO = "info"
    MEETING_SCHEDULED = "meeting_scheduled"


@dataclass
class NotificationConfig:
    """Configuracao de notificacoes"""
    enabled: bool = True
    notify_on_project_created: bool = True
    notify_on_project_completed: bool = True
    notify_on_story_completed: bool = True
    notify_on_errors: bool = True
    notify_on_warnings: bool = False
    default_recipients: List[str] = field(default_factory=list)
    admin_recipients: List[str] = field(default_factory=list)
    dashboard_url: str = "http://localhost:9001"


class NotificationSender:
    """
    Enviador de notificacoes por email.

    Centraliza o envio de todos os tipos de notificacoes do sistema,
    usando templates HTML padronizados.

    Exemplo:
        client = MicrosoftGraphClient(config)
        sender = NotificationSender(client)

        # Notificar projeto criado
        await sender.notify_project_created({
            "project_id": "PROJ-001",
            "project_name": "Meu Projeto",
            "description": "Descricao do projeto"
        })

        # Notificar erro
        await sender.notify_error(
            error_type="Falha de Processamento",
            error_message="Arquivo nao encontrado",
            severity="high"
        )
    """

    def __init__(
        self,
        email_client,
        config: Optional[NotificationConfig] = None
    ):
        """
        Inicializa o sender.

        Args:
            email_client: Cliente de email (Graph ou SMTP)
            config: Configuracao de notificacoes
        """
        self.client = email_client
        self.config = config or NotificationConfig()

    async def send_notification(
        self,
        notification_type: NotificationType,
        recipients: Optional[List[str]] = None,
        template_vars: Optional[Dict[str, Any]] = None,
        subject: Optional[str] = None,
        cc: Optional[List[str]] = None,
        attachments: Optional[List] = None
    ) -> bool:
        """
        Envia uma notificacao generica.

        Args:
            notification_type: Tipo da notificacao
            recipients: Destinatarios (usa default se nao informado)
            template_vars: Variaveis do template
            subject: Assunto customizado
            cc: Copia
            attachments: Anexos

        Returns:
            bool: True se enviado com sucesso
        """
        if not self.config.enabled:
            logger.debug("Notificacoes desabilitadas")
            return False

        # Define destinatarios
        if not recipients:
            recipients = self.config.default_recipients

        if not recipients:
            logger.warning("Nenhum destinatario definido para notificacao")
            return False

        # Define variaveis do template
        vars_dict = template_vars or {}
        vars_dict["dashboard_url"] = self.config.dashboard_url
        vars_dict["timestamp"] = datetime.now().strftime("%d/%m/%Y %H:%M")

        # Mapeia tipo para template
        template_mapping = {
            NotificationType.PROJECT_CREATED: "project_created",
            NotificationType.PROJECT_COMPLETED: "project_completed",
            NotificationType.STORY_STATUS_UPDATE: "story_status_update",
            NotificationType.ERROR_ALERT: "error_alert",
            NotificationType.WARNING_ALERT: "error_alert",
            NotificationType.PROJECT_ERROR: "error_alert"
        }

        template_name = template_mapping.get(notification_type, "base")

        # Define assunto padrao
        if not subject:
            subject = self._get_default_subject(notification_type, vars_dict)

        try:
            return await self.client.send_email_with_template(
                to=recipients,
                template_name=template_name,
                template_vars=vars_dict,
                subject=subject,
                cc=cc,
                attachments=attachments
            )
        except Exception as e:
            logger.exception(f"Erro ao enviar notificacao: {e}")
            return False

    def _get_default_subject(
        self,
        notification_type: NotificationType,
        vars_dict: Dict[str, Any]
    ) -> str:
        """Gera assunto padrao baseado no tipo"""
        project_name = vars_dict.get("project_name", "")
        story_id = vars_dict.get("story_id", "")

        subjects = {
            NotificationType.PROJECT_CREATED:
                f"[Fabrica] Projeto Criado: {project_name}",
            NotificationType.PROJECT_COMPLETED:
                f"[Fabrica] Projeto Concluido: {project_name}",
            NotificationType.PROJECT_ERROR:
                f"[Fabrica] ERRO: {project_name}",
            NotificationType.STORY_STATUS_UPDATE:
                f"[Fabrica] Story Atualizada: {story_id}",
            NotificationType.STORY_COMPLETED:
                f"[Fabrica] Story Concluida: {story_id}",
            NotificationType.ERROR_ALERT:
                f"[Fabrica] ALERTA: {vars_dict.get('error_type', 'Erro')}",
            NotificationType.WARNING_ALERT:
                f"[Fabrica] Aviso: {vars_dict.get('warning_type', 'Atencao')}",
            NotificationType.INFO:
                "[Fabrica] Informacao",
            NotificationType.MEETING_SCHEDULED:
                f"[Fabrica] Reuniao Agendada: {vars_dict.get('subject', 'Review')}"
        }

        return subjects.get(notification_type, "[Fabrica] Notificacao")

    # =========================================================================
    # METODOS DE CONVENIENCIA
    # =========================================================================

    async def notify_project_created(
        self,
        project_data: Dict[str, Any],
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica que um projeto foi criado.

        Args:
            project_data: Dados do projeto
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        if not self.config.notify_on_project_created:
            return False

        template_vars = {
            "project_id": project_data.get("project_id", ""),
            "project_name": project_data.get("name", project_data.get("project_name", "")),
            "project_type": project_data.get("type", "web-app"),
            "description": project_data.get("description", ""),
            "created_at": datetime.now().strftime("%d/%m/%Y %H:%M"),
            "created_by": project_data.get("created_by", "Sistema"),
            "features": project_data.get("features", []),
            "stories_count": project_data.get("stories_count", 0),
            "project_url": f"{self.config.dashboard_url}/projects/{project_data.get('project_id', '')}"
        }

        return await self.send_notification(
            NotificationType.PROJECT_CREATED,
            recipients,
            template_vars
        )

    async def notify_project_completed(
        self,
        project_data: Dict[str, Any],
        execution_data: Optional[Dict[str, Any]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica que um projeto foi concluido.

        Args:
            project_data: Dados do projeto
            execution_data: Dados de execucao
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        if not self.config.notify_on_project_completed:
            return False

        exec_data = execution_data or {}

        template_vars = {
            "project_id": project_data.get("project_id", ""),
            "project_name": project_data.get("name", project_data.get("project_name", "")),
            "started_at": exec_data.get("started_at", ""),
            "completed_at": exec_data.get("completed_at", datetime.now().strftime("%d/%m/%Y %H:%M")),
            "execution_time": exec_data.get("execution_time", "N/A"),
            "files_count": exec_data.get("files_count", 0),
            "stories_completed": exec_data.get("stories_completed", 0),
            "lines_of_code": exec_data.get("lines_of_code", "N/A"),
            "tests_passed": exec_data.get("tests_passed"),
            "tests_failed": exec_data.get("tests_failed", 0),
            "agents_count": exec_data.get("agents_count", 1),
            "files_generated": exec_data.get("files_generated", []),
            "project_url": f"{self.config.dashboard_url}/projects/{project_data.get('project_id', '')}",
            "documentation_url": exec_data.get("documentation_url")
        }

        return await self.send_notification(
            NotificationType.PROJECT_COMPLETED,
            recipients,
            template_vars
        )

    async def notify_story_updated(
        self,
        story_data: Dict[str, Any],
        old_status: Optional[str] = None,
        new_status: Optional[str] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica que uma story foi atualizada.

        Args:
            story_data: Dados da story
            old_status: Status anterior
            new_status: Novo status
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        template_vars = {
            "story_id": story_data.get("story_id", ""),
            "story_title": story_data.get("title", ""),
            "project_name": story_data.get("project_name", ""),
            "old_status": old_status,
            "new_status": new_status or story_data.get("status", ""),
            "story_points": story_data.get("story_points"),
            "priority": story_data.get("priority", "medium"),
            "updated_at": datetime.now().strftime("%d/%m/%Y %H:%M"),
            "assignee": story_data.get("assignee"),
            "story_persona": story_data.get("persona"),
            "story_action": story_data.get("action"),
            "story_benefit": story_data.get("benefit"),
            "story_narrative": bool(story_data.get("persona")),
            "tasks": story_data.get("tasks", []),
            "tasks_completed": story_data.get("tasks_completed", 0),
            "tasks_total": story_data.get("tasks_total", 0),
            "task_progress": story_data.get("progress", 0),
            "files_generated": story_data.get("files_generated", []),
            "story_url": f"{self.config.dashboard_url}/stories/{story_data.get('story_id', '')}"
        }

        return await self.send_notification(
            NotificationType.STORY_STATUS_UPDATE,
            recipients,
            template_vars
        )

    async def notify_story_completed(
        self,
        story_data: Dict[str, Any],
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica que uma story foi concluida.

        Args:
            story_data: Dados da story
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        if not self.config.notify_on_story_completed:
            return False

        return await self.notify_story_updated(
            story_data,
            old_status=story_data.get("previous_status"),
            new_status="done",
            recipients=recipients
        )

    async def notify_error(
        self,
        error_type: str,
        error_message: str,
        severity: str = "high",
        context: Optional[Dict[str, Any]] = None,
        stack_trace: Optional[str] = None,
        project_data: Optional[Dict[str, Any]] = None,
        story_data: Optional[Dict[str, Any]] = None,
        suggested_actions: Optional[List[str]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica sobre um erro.

        Args:
            error_type: Tipo do erro
            error_message: Mensagem de erro
            severity: Severidade (low, medium, high, critical)
            context: Contexto adicional
            stack_trace: Stack trace
            project_data: Dados do projeto (se aplicavel)
            story_data: Dados da story (se aplicavel)
            suggested_actions: Acoes sugeridas
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        if not self.config.notify_on_errors:
            return False

        # Erros vao para admins se nao especificado
        if not recipients:
            recipients = self.config.admin_recipients or self.config.default_recipients

        template_vars = {
            "error_type": error_type,
            "error_message": error_message,
            "severity": severity,
            "timestamp": datetime.now().strftime("%d/%m/%Y %H:%M:%S"),
            "stack_trace": stack_trace,
            "context": context,
            "suggested_actions": suggested_actions or [
                "Verificar logs do sistema",
                "Reiniciar o worker afetado",
                "Contatar equipe de suporte"
            ],
            "retry_available": context.get("retry_available", False) if context else False,
            "auto_retry": context.get("auto_retry", False) if context else False,
            "retry_delay": context.get("retry_delay", 30) if context else 30
        }

        # Adiciona dados de projeto/story se disponiveis
        if project_data:
            template_vars["project_id"] = project_data.get("project_id")
            template_vars["project_name"] = project_data.get("name")

        if story_data:
            template_vars["story_id"] = story_data.get("story_id")

        return await self.send_notification(
            NotificationType.ERROR_ALERT,
            recipients,
            template_vars
        )

    async def notify_warning(
        self,
        warning_type: str,
        warning_message: str,
        context: Optional[Dict[str, Any]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Notifica sobre um aviso.

        Args:
            warning_type: Tipo do aviso
            warning_message: Mensagem
            context: Contexto adicional
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        if not self.config.notify_on_warnings:
            return False

        template_vars = {
            "error_type": warning_type,
            "error_message": warning_message,
            "severity": "warning",
            "timestamp": datetime.now().strftime("%d/%m/%Y %H:%M:%S"),
            "context": context
        }

        return await self.send_notification(
            NotificationType.WARNING_ALERT,
            recipients,
            template_vars
        )

    async def schedule_review_meeting(
        self,
        project_data: Dict[str, Any],
        attendees: List[str],
        start_datetime: datetime,
        duration_minutes: int = 30
    ) -> Optional[Dict[str, Any]]:
        """
        Agenda reuniao de review e notifica participantes.

        Args:
            project_data: Dados do projeto
            attendees: Participantes
            start_datetime: Data/hora de inicio
            duration_minutes: Duracao

        Returns:
            Dict com dados do evento ou None
        """
        # Cria evento no calendario
        event = await self.client.schedule_meeting(
            subject=f"Review: {project_data.get('name', 'Projeto')}",
            start=start_datetime,
            duration_minutes=duration_minutes,
            attendees=attendees,
            body=f"""
            <h2>Review de Projeto</h2>
            <p><strong>Projeto:</strong> {project_data.get('name', '')}</p>
            <p><strong>ID:</strong> {project_data.get('project_id', '')}</p>
            <p>Reuniao para revisar o progresso e resultados do projeto.</p>
            <p><a href="{self.config.dashboard_url}/projects/{project_data.get('project_id', '')}">
                Ver Projeto no Dashboard
            </a></p>
            """,
            reminder_minutes=15
        )

        return event


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_notification_sender: Optional[NotificationSender] = None


def get_notification_sender(email_client=None) -> NotificationSender:
    """Retorna instancia global do notification sender"""
    global _notification_sender
    if _notification_sender is None:
        if email_client is None:
            from ..graph_mail_client import get_graph_client
            email_client = get_graph_client()
        _notification_sender = NotificationSender(email_client)
    return _notification_sender
