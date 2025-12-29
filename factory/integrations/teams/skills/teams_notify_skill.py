# -*- coding: utf-8 -*-
"""
Teams Notify Skill
==================
Skill para envio de notificacoes via Microsoft Teams.
Pode ser usado pelos agentes para notificar usuarios e canais.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

from ..webhook_client import WebhookClient, WebhookConfig
from ..notifications.card_builder import AdaptiveCardBuilder
from ..notifications.channel_notifier import ChannelNotifier, ChannelNotifierConfig, ChannelConfig

logger = logging.getLogger(__name__)


class NotificationType(str, Enum):
    """Tipos de notificacao"""
    INFO = "info"
    SUCCESS = "success"
    WARNING = "warning"
    ERROR = "error"
    PROJECT_UPDATE = "project_update"
    STORY_UPDATE = "story_update"
    TASK_COMPLETE = "task_complete"
    DAILY_SUMMARY = "daily_summary"


@dataclass
class TeamsNotifySkillConfig:
    """Configuracao do skill de notificacao"""
    # Webhooks por canal
    webhooks: Dict[str, str] = field(default_factory=dict)
    # Canal padrao
    default_channel: str = "general"
    # Habilitar notificacoes
    enabled: bool = True
    # Tipos de notificacao habilitados
    enabled_types: List[NotificationType] = field(
        default_factory=lambda: list(NotificationType)
    )
    # Horario de silencio (nao notifica)
    quiet_hours_start: int = 22  # 22:00
    quiet_hours_end: int = 7     # 07:00
    # Respeitar horario de silencio
    respect_quiet_hours: bool = False


class TeamsNotifySkill:
    """
    Skill para envio de notificacoes via Microsoft Teams.

    Este skill pode ser usado pelos agentes da Fabrica para
    enviar notificacoes automaticas para canais do Teams.

    Exemplo:
        config = TeamsNotifySkillConfig(
            webhooks={
                "dev": "https://webhook.office.com/xxx",
                "alerts": "https://webhook.office.com/yyy"
            },
            default_channel="dev"
        )

        skill = TeamsNotifySkill(config)

        # Notificacao simples
        await skill.notify("Tarefa concluida!", channel="dev")

        # Notificacao de projeto
        await skill.notify_project_completed(
            project_id="PROJ-001",
            project_name="Sistema X",
            files_count=45,
            duration="2 minutos"
        )
    """

    skill_name = "teams_notify"
    skill_description = "Envia notificacoes para canais do Microsoft Teams"

    def __init__(self, config: TeamsNotifySkillConfig):
        """
        Inicializa o skill.

        Args:
            config: Configuracao do skill
        """
        self.config = config
        self._card_builder = AdaptiveCardBuilder()

        # Configura cliente de webhook
        webhook_config = WebhookConfig(
            webhooks=config.webhooks,
            default_webhook=config.webhooks.get(config.default_channel, "")
        )
        self._webhook_client = WebhookClient(webhook_config)

    async def close(self) -> None:
        """Fecha conexoes"""
        await self._webhook_client.close()

    def _is_quiet_hours(self) -> bool:
        """Verifica se esta em horario de silencio"""
        if not self.config.respect_quiet_hours:
            return False

        hour = datetime.now().hour

        if self.config.quiet_hours_start > self.config.quiet_hours_end:
            # Ex: 22:00 - 07:00 (cruza meia-noite)
            return hour >= self.config.quiet_hours_start or hour < self.config.quiet_hours_end
        else:
            # Ex: 23:00 - 06:00
            return self.config.quiet_hours_start <= hour < self.config.quiet_hours_end

    def _is_notification_enabled(self, notification_type: NotificationType) -> bool:
        """Verifica se tipo de notificacao esta habilitado"""
        if not self.config.enabled:
            return False

        return notification_type in self.config.enabled_types

    def _get_webhook_url(self, channel: str = None) -> str:
        """Obtem URL do webhook para canal"""
        channel_name = channel or self.config.default_channel
        return self.config.webhooks.get(channel_name, "")

    # =========================================================================
    # Notificacoes Simples
    # =========================================================================

    async def notify(
        self,
        message: str,
        title: str = "",
        notification_type: NotificationType = NotificationType.INFO,
        channel: str = None
    ) -> bool:
        """
        Envia notificacao simples.

        Args:
            message: Mensagem
            title: Titulo opcional
            notification_type: Tipo de notificacao
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        if not self._is_notification_enabled(notification_type):
            logger.debug(f"Notificacao {notification_type} desabilitada")
            return False

        if self._is_quiet_hours():
            logger.debug("Horario de silencio - notificacao nao enviada")
            return False

        # Seleciona metodo baseado no tipo
        if notification_type == NotificationType.SUCCESS:
            return await self._webhook_client.send_success(title, message, channel)
        elif notification_type == NotificationType.WARNING:
            return await self._webhook_client.send_warning(title, message, channel)
        elif notification_type == NotificationType.ERROR:
            return await self._webhook_client.send_error(title, message, channel)
        else:
            return await self._webhook_client.send_info(title, message, channel)

    async def notify_card(
        self,
        card: Dict[str, Any],
        notification_type: NotificationType = NotificationType.INFO,
        channel: str = None
    ) -> bool:
        """
        Envia notificacao com card adaptativo.

        Args:
            card: Card Adaptativo
            notification_type: Tipo de notificacao
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        if not self._is_notification_enabled(notification_type):
            return False

        if self._is_quiet_hours():
            return False

        return await self._webhook_client.send_adaptive_card(card, channel)

    # =========================================================================
    # Notificacoes de Projeto
    # =========================================================================

    async def notify_project_started(
        self,
        project_id: str,
        project_name: str,
        description: str = "",
        assigned_to: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica inicio de projeto.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            description: Descricao
            assigned_to: Responsavel
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.PROJECT_UPDATE):
            return False

        return await self._webhook_client.send_project_started(
            project_name=project_name,
            project_id=project_id,
            description=description,
            assigned_to=assigned_to,
            channel=channel
        )

    async def notify_project_completed(
        self,
        project_id: str,
        project_name: str,
        files_count: int,
        duration: str,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de projeto.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            files_count: Numero de arquivos gerados
            duration: Duracao do desenvolvimento
            url: URL do projeto
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.PROJECT_UPDATE):
            return False

        return await self._webhook_client.send_project_completed(
            project_name=project_name,
            project_id=project_id,
            files_count=files_count,
            duration=duration,
            url=url,
            channel=channel
        )

    async def notify_project_error(
        self,
        project_id: str,
        project_name: str,
        error_message: str,
        step: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica erro em projeto.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            error_message: Mensagem de erro
            step: Etapa do erro
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.ERROR):
            return False

        return await self._webhook_client.send_project_error(
            project_name=project_name,
            project_id=project_id,
            error_message=error_message,
            step=step,
            channel=channel
        )

    # =========================================================================
    # Notificacoes de Story
    # =========================================================================

    async def notify_story_status_change(
        self,
        story_id: str,
        story_title: str,
        old_status: str,
        new_status: str,
        assigned_to: str = "",
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica mudanca de status de story.

        Args:
            story_id: ID da story
            story_title: Titulo
            old_status: Status anterior
            new_status: Novo status
            assigned_to: Responsavel
            url: URL da story
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.STORY_UPDATE):
            return False

        return await self._webhook_client.send_story_status_change(
            story_id=story_id,
            story_title=story_title,
            old_status=old_status,
            new_status=new_status,
            assigned_to=assigned_to,
            url=url,
            channel=channel
        )

    # =========================================================================
    # Notificacoes de Task
    # =========================================================================

    async def notify_task_completed(
        self,
        task_id: str,
        task_title: str,
        story_id: str,
        story_title: str,
        files_created: List[str] = None,
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de tarefa.

        Args:
            task_id: ID da tarefa
            task_title: Titulo da tarefa
            story_id: ID da story
            story_title: Titulo da story
            files_created: Arquivos criados
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.TASK_COMPLETE):
            return False

        return await self._webhook_client.send_task_completed(
            task_id=task_id,
            task_title=task_title,
            story_id=story_id,
            story_title=story_title,
            files_created=files_created,
            channel=channel
        )

    # =========================================================================
    # Resumos
    # =========================================================================

    async def notify_daily_summary(
        self,
        stories_completed: int,
        stories_in_progress: int,
        tasks_completed: int,
        files_generated: int,
        top_contributors: List[Dict[str, Any]] = None,
        channel: str = None
    ) -> bool:
        """
        Envia resumo diario.

        Args:
            stories_completed: Stories concluidas
            stories_in_progress: Stories em progresso
            tasks_completed: Tarefas concluidas
            files_generated: Arquivos gerados
            top_contributors: Top contribuidores
            channel: Canal de destino
        """
        if not self._is_notification_enabled(NotificationType.DAILY_SUMMARY):
            return False

        return await self._webhook_client.send_daily_summary(
            date=datetime.now(),
            stories_completed=stories_completed,
            stories_in_progress=stories_in_progress,
            tasks_completed=tasks_completed,
            files_generated=files_generated,
            top_contributors=top_contributors,
            channel=channel
        )

    # =========================================================================
    # Skill Interface
    # =========================================================================

    async def execute(
        self,
        action: str,
        params: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Executa acao do skill.

        Args:
            action: Acao a executar
            params: Parametros

        Returns:
            Resultado da acao
        """
        actions = {
            "notify": self._action_notify,
            "project_started": self._action_project_started,
            "project_completed": self._action_project_completed,
            "project_error": self._action_project_error,
            "story_status": self._action_story_status,
            "task_completed": self._action_task_completed,
            "daily_summary": self._action_daily_summary
        }

        handler = actions.get(action)
        if not handler:
            return {
                "success": False,
                "error": f"Acao desconhecida: {action}"
            }

        try:
            result = await handler(params)
            return {"success": result}
        except Exception as e:
            logger.error(f"Erro ao executar skill: {e}")
            return {"success": False, "error": str(e)}

    async def _action_notify(self, params: Dict) -> bool:
        """Acao de notificacao simples"""
        return await self.notify(
            message=params.get("message", ""),
            title=params.get("title", ""),
            notification_type=NotificationType(
                params.get("type", NotificationType.INFO.value)
            ),
            channel=params.get("channel")
        )

    async def _action_project_started(self, params: Dict) -> bool:
        """Acao de projeto iniciado"""
        return await self.notify_project_started(
            project_id=params.get("project_id", ""),
            project_name=params.get("project_name", ""),
            description=params.get("description", ""),
            assigned_to=params.get("assigned_to", ""),
            channel=params.get("channel")
        )

    async def _action_project_completed(self, params: Dict) -> bool:
        """Acao de projeto concluido"""
        return await self.notify_project_completed(
            project_id=params.get("project_id", ""),
            project_name=params.get("project_name", ""),
            files_count=params.get("files_count", 0),
            duration=params.get("duration", ""),
            url=params.get("url", ""),
            channel=params.get("channel")
        )

    async def _action_project_error(self, params: Dict) -> bool:
        """Acao de erro em projeto"""
        return await self.notify_project_error(
            project_id=params.get("project_id", ""),
            project_name=params.get("project_name", ""),
            error_message=params.get("error_message", ""),
            step=params.get("step", ""),
            channel=params.get("channel")
        )

    async def _action_story_status(self, params: Dict) -> bool:
        """Acao de mudanca de status de story"""
        return await self.notify_story_status_change(
            story_id=params.get("story_id", ""),
            story_title=params.get("story_title", ""),
            old_status=params.get("old_status", ""),
            new_status=params.get("new_status", ""),
            assigned_to=params.get("assigned_to", ""),
            url=params.get("url", ""),
            channel=params.get("channel")
        )

    async def _action_task_completed(self, params: Dict) -> bool:
        """Acao de tarefa concluida"""
        return await self.notify_task_completed(
            task_id=params.get("task_id", ""),
            task_title=params.get("task_title", ""),
            story_id=params.get("story_id", ""),
            story_title=params.get("story_title", ""),
            files_created=params.get("files_created", []),
            channel=params.get("channel")
        )

    async def _action_daily_summary(self, params: Dict) -> bool:
        """Acao de resumo diario"""
        return await self.notify_daily_summary(
            stories_completed=params.get("stories_completed", 0),
            stories_in_progress=params.get("stories_in_progress", 0),
            tasks_completed=params.get("tasks_completed", 0),
            files_generated=params.get("files_generated", 0),
            top_contributors=params.get("top_contributors"),
            channel=params.get("channel")
        )
