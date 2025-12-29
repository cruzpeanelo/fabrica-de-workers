# -*- coding: utf-8 -*-
"""
Channel Notifier
================
Notificador de canais do Microsoft Teams.
Permite enviar notificacoes para canais usando Graph API ou Webhooks.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

from ..graph_client import GraphClient, GraphConfig
from ..webhook_client import WebhookClient, WebhookConfig
from .card_builder import AdaptiveCardBuilder, CardType

logger = logging.getLogger(__name__)


class NotificationMethod(str, Enum):
    """Metodo de notificacao"""
    WEBHOOK = "webhook"
    GRAPH_API = "graph_api"


@dataclass
class ChannelConfig:
    """Configuracao de um canal"""
    name: str
    team_id: str = ""
    channel_id: str = ""
    webhook_url: str = ""
    method: NotificationMethod = NotificationMethod.WEBHOOK


@dataclass
class ChannelNotifierConfig:
    """Configuracao do notificador de canais"""
    # Canais configurados
    channels: Dict[str, ChannelConfig] = field(default_factory=dict)
    # Canal padrao
    default_channel: str = ""
    # Configuracao do Graph API (opcional)
    graph_config: Optional[GraphConfig] = None
    # Webhook Config (opcional)
    webhook_config: Optional[WebhookConfig] = None


class ChannelNotifier:
    """
    Notificador de canais do Microsoft Teams.

    Permite enviar notificacoes para canais usando:
    - Webhooks (simples, sem autenticacao)
    - Graph API (completo, requer Azure AD)

    Exemplo:
        config = ChannelNotifierConfig(
            channels={
                "dev": ChannelConfig(
                    name="Desenvolvimento",
                    webhook_url="https://outlook.office.com/webhook/xxx"
                ),
                "alerts": ChannelConfig(
                    name="Alertas",
                    webhook_url="https://outlook.office.com/webhook/yyy"
                )
            },
            default_channel="dev"
        )

        notifier = ChannelNotifier(config)
        await notifier.notify("Projeto concluido!", channel="dev")
    """

    def __init__(self, config: ChannelNotifierConfig):
        """
        Inicializa o notificador.

        Args:
            config: Configuracao do notificador
        """
        self.config = config
        self._graph_client: Optional[GraphClient] = None
        self._webhook_client: Optional[WebhookClient] = None
        self._card_builder = AdaptiveCardBuilder()

        # Inicializa clientes
        if config.graph_config:
            self._graph_client = GraphClient(config.graph_config)

        if config.webhook_config:
            self._webhook_client = WebhookClient(config.webhook_config)

    async def close(self) -> None:
        """Fecha conexoes"""
        if self._graph_client:
            await self._graph_client.close()
        if self._webhook_client:
            await self._webhook_client.close()

    def _get_channel(self, channel: str = None) -> Optional[ChannelConfig]:
        """Obtem configuracao de canal"""
        channel_name = channel or self.config.default_channel
        return self.config.channels.get(channel_name)

    async def _send_via_graph(
        self,
        channel_config: ChannelConfig,
        content: str = None,
        card: Dict = None
    ) -> bool:
        """Envia via Graph API"""
        if not self._graph_client:
            logger.error("Graph client nao configurado")
            return False

        try:
            if card:
                result = await self._graph_client.send_channel_card(
                    channel_config.team_id,
                    channel_config.channel_id,
                    card
                )
            else:
                result = await self._graph_client.send_channel_message(
                    channel_config.team_id,
                    channel_config.channel_id,
                    content or ""
                )

            return result is not None

        except Exception as e:
            logger.error(f"Erro ao enviar via Graph API: {e}")
            return False

    async def _send_via_webhook(
        self,
        channel_config: ChannelConfig,
        content: str = None,
        card: Dict = None
    ) -> bool:
        """Envia via Webhook"""
        if not self._webhook_client and not channel_config.webhook_url:
            logger.error("Webhook nao configurado")
            return False

        try:
            # Se tem webhook especifico do canal, cria cliente temporario
            if channel_config.webhook_url:
                temp_config = WebhookConfig(
                    default_webhook=channel_config.webhook_url
                )
                client = WebhookClient(temp_config)
            else:
                client = self._webhook_client

            if card:
                result = await client.send_adaptive_card(card)
            elif content:
                result = await client.send_message(content)
            else:
                return False

            # Fecha cliente temporario
            if channel_config.webhook_url:
                await client.close()

            return result

        except Exception as e:
            logger.error(f"Erro ao enviar via Webhook: {e}")
            return False

    async def notify(
        self,
        message: str,
        title: str = "",
        channel: str = None
    ) -> bool:
        """
        Envia notificacao simples.

        Args:
            message: Mensagem
            title: Titulo opcional
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        channel_config = self._get_channel(channel)
        if not channel_config:
            logger.error(f"Canal nao encontrado: {channel}")
            return False

        content = f"<h3>{title}</h3>{message}" if title else message

        if channel_config.method == NotificationMethod.GRAPH_API:
            return await self._send_via_graph(channel_config, content=content)
        else:
            return await self._send_via_webhook(channel_config, content=content)

    async def notify_card(
        self,
        card: Dict[str, Any],
        channel: str = None
    ) -> bool:
        """
        Envia card adaptativo.

        Args:
            card: Card Adaptativo
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        channel_config = self._get_channel(channel)
        if not channel_config:
            logger.error(f"Canal nao encontrado: {channel}")
            return False

        if channel_config.method == NotificationMethod.GRAPH_API:
            return await self._send_via_graph(channel_config, card=card)
        else:
            return await self._send_via_webhook(channel_config, card=card)

    # =========================================================================
    # Notificacoes de Projeto
    # =========================================================================

    async def notify_project_started(
        self,
        project_name: str,
        project_id: str,
        description: str = "",
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica inicio de projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            description: Descricao
            url: URL do projeto
            channel: Canal de destino
        """
        card = self._card_builder.create_project_card(
            project_name=project_name,
            project_id=project_id,
            status="Iniciado",
            description=description,
            facts=[
                ("Iniciado em", datetime.now().strftime("%d/%m/%Y %H:%M")),
                ("Status", "Em Desenvolvimento")
            ],
            url=url
        )

        return await self.notify_card(card, channel)

    async def notify_project_completed(
        self,
        project_name: str,
        project_id: str,
        files_count: int,
        duration: str,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            files_count: Numero de arquivos
            duration: Duracao
            url: URL do projeto
            channel: Canal de destino
        """
        card = self._card_builder.create_project_card(
            project_name=project_name,
            project_id=project_id,
            status="Concluido",
            facts=[
                ("Arquivos Gerados", str(files_count)),
                ("Duracao", duration),
                ("Concluido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
            ],
            url=url
        )

        return await self.notify_card(card, channel)

    async def notify_project_error(
        self,
        project_name: str,
        project_id: str,
        error_message: str,
        step: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica erro em projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            error_message: Mensagem de erro
            step: Etapa do erro
            channel: Canal de destino
        """
        facts = [
            ("Erro", error_message),
            ("Ocorrido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
        ]

        if step:
            facts.insert(0, ("Etapa", step))

        card = self._card_builder.create_project_card(
            project_name=project_name,
            project_id=project_id,
            status="Erro",
            facts=facts
        )

        return await self.notify_card(card, channel)

    # =========================================================================
    # Notificacoes de Story
    # =========================================================================

    async def notify_story_created(
        self,
        story_id: str,
        title: str,
        persona: str,
        action: str,
        benefit: str,
        story_points: int = 0,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica criacao de story.

        Args:
            story_id: ID da story
            title: Titulo
            persona: "Como um..."
            action: "Eu quero..."
            benefit: "Para que..."
            story_points: Story points
            url: URL da story
            channel: Canal de destino
        """
        card = self._card_builder.create_story_card(
            story_id=story_id,
            title=title,
            persona=persona,
            action=action,
            benefit=benefit,
            status="Nova",
            story_points=story_points,
            url=url
        )

        return await self.notify_card(card, channel)

    async def notify_story_status_change(
        self,
        story_id: str,
        title: str,
        old_status: str,
        new_status: str,
        progress: int = 0,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica mudanca de status de story.

        Args:
            story_id: ID da story
            title: Titulo
            old_status: Status anterior
            new_status: Novo status
            progress: Progresso
            url: URL da story
            channel: Canal de destino
        """
        self._card_builder.clear()

        self._card_builder.add_heading(f"{story_id}: {title}")
        self._card_builder.add_text(f"{old_status} → {new_status}")
        self._card_builder.add_status_indicator(new_status)

        if progress > 0:
            self._card_builder.add_progress_bar(progress)

        self._card_builder.add_text(
            datetime.now().strftime("%d/%m/%Y %H:%M"),
            size=self._card_builder.version  # TextSize.SMALL workaround
        )

        if url:
            self._card_builder.add_action_url("Ver Story", url)

        card = self._card_builder.build()
        return await self.notify_card(card, channel)

    async def notify_story_completed(
        self,
        story_id: str,
        title: str,
        tasks_completed: int,
        files_generated: int,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de story.

        Args:
            story_id: ID da story
            title: Titulo
            tasks_completed: Tarefas concluidas
            files_generated: Arquivos gerados
            url: URL da story
            channel: Canal de destino
        """
        card = self._card_builder.create_alert_card(
            alert_type="success",
            title=f"Story Concluida: {story_id}",
            message=title,
            details=[
                ("Tarefas Concluidas", str(tasks_completed)),
                ("Arquivos Gerados", str(files_generated)),
                ("Concluido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
            ],
            action_url=url,
            action_title="Ver Story"
        )

        return await self.notify_card(card, channel)

    # =========================================================================
    # Notificacoes de Task
    # =========================================================================

    async def notify_task_completed(
        self,
        task_id: str,
        task_title: str,
        story_id: str,
        files_created: List[str] = None,
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de tarefa.

        Args:
            task_id: ID da tarefa
            task_title: Titulo da tarefa
            story_id: ID da story
            files_created: Arquivos criados
            channel: Canal de destino
        """
        details = [
            ("Tarefa", task_title),
            ("Story", story_id),
            ("Concluido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
        ]

        if files_created:
            details.append(("Arquivos", f"{len(files_created)} arquivos criados"))

        card = self._card_builder.create_alert_card(
            alert_type="success",
            title=f"Tarefa Concluida: {task_id}",
            message=task_title,
            details=details
        )

        return await self.notify_card(card, channel)

    # =========================================================================
    # Alertas
    # =========================================================================

    async def send_alert(
        self,
        alert_type: str,
        title: str,
        message: str,
        details: List[tuple] = None,
        action_url: str = "",
        channel: str = None
    ) -> bool:
        """
        Envia alerta.

        Args:
            alert_type: Tipo (success, warning, error, info)
            title: Titulo
            message: Mensagem
            details: Detalhes
            action_url: URL de acao
            channel: Canal de destino
        """
        card = self._card_builder.create_alert_card(
            alert_type=alert_type,
            title=title,
            message=message,
            details=details,
            action_url=action_url
        )

        return await self.notify_card(card, channel)

    async def send_daily_summary(
        self,
        stories_completed: int,
        stories_in_progress: int,
        tasks_completed: int,
        files_generated: int,
        channel: str = None
    ) -> bool:
        """
        Envia resumo diario.

        Args:
            stories_completed: Stories concluidas
            stories_in_progress: Stories em progresso
            tasks_completed: Tarefas concluidas
            files_generated: Arquivos gerados
            channel: Canal de destino
        """
        self._card_builder.clear()

        self._card_builder.add_heading("Resumo Diario - Fabrica de Agentes")
        self._card_builder.add_text(datetime.now().strftime("%d/%m/%Y"))

        self._card_builder.add_fact_set([
            ("Stories Concluidas", str(stories_completed)),
            ("Stories em Progresso", str(stories_in_progress)),
            ("Tarefas Concluidas", str(tasks_completed)),
            ("Arquivos Gerados", str(files_generated))
        ], separator=True)

        card = self._card_builder.build()
        return await self.notify_card(card, channel)
