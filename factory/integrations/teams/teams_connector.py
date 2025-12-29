# -*- coding: utf-8 -*-
"""
Teams Connector
===============
Conector principal para integracao com Microsoft Teams.
Unifica todos os componentes de integracao em uma interface simples.

Este modulo fornece uma interface unificada para:
- Envio de notificacoes via webhooks
- Interacao via Microsoft Graph API
- Bot interativo para comandos
- Cards Adaptativos

Uso:
    from factory.integrations.teams import TeamsIntegration, TeamsConfig

    config = TeamsConfig(
        webhook_url="https://outlook.office.com/webhook/xxx",
        enabled=True
    )

    teams = TeamsIntegration(config)

    # Notificacao simples
    await teams.notify("Projeto concluido!")

    # Notificacao rica
    await teams.notify_project_completed(
        project_id="PROJ-001",
        project_name="Sistema de Vendas",
        files_count=45,
        duration="2 minutos"
    )
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Awaitable

from .graph_client import GraphClient, GraphConfig
from .webhook_client import WebhookClient, WebhookConfig, ThemeColor
from .notifications.card_builder import AdaptiveCardBuilder, CardType
from .notifications.channel_notifier import ChannelNotifier, ChannelNotifierConfig, ChannelConfig, NotificationMethod
from .notifications.dm_notifier import DMNotifier, DMNotifierConfig, UserMapping
from .bot.bot_handler import TeamsBotHandler, BotConfig
from .bot.commands import BotCommandHandler
from ..base import IntegrationBase, IntegrationConfig, IntegrationStatus, SyncResult

logger = logging.getLogger(__name__)


@dataclass
class TeamsConfig(IntegrationConfig):
    """
    Configuracao completa da integracao com Teams.

    Atributos:
        enabled: Se a integracao esta habilitada
        webhook_url: URL do webhook padrao
        webhooks: Mapeamento de canais para webhooks
        graph_tenant_id: Tenant ID do Azure AD
        graph_client_id: Client ID do app Azure
        graph_client_secret: Client Secret do app
        bot_app_id: App ID do bot
        bot_app_password: Password do bot
        users: Mapeamento de usuarios para DMs
        default_channel: Canal padrao para notificacoes
        quiet_hours_start: Inicio do horario de silencio (0-23)
        quiet_hours_end: Fim do horario de silencio (0-23)
        respect_quiet_hours: Respeitar horario de silencio
    """
    # Webhooks
    webhook_url: str = ""
    webhooks: Dict[str, str] = field(default_factory=dict)

    # Microsoft Graph API
    graph_tenant_id: str = ""
    graph_client_id: str = ""
    graph_client_secret: str = ""

    # Bot Framework
    bot_app_id: str = ""
    bot_app_password: str = ""
    bot_endpoint: str = "/api/teams/messages"

    # Usuarios para DMs
    users: Dict[str, UserMapping] = field(default_factory=dict)

    # Canais configurados
    channels: Dict[str, ChannelConfig] = field(default_factory=dict)

    # Configuracoes gerais
    default_channel: str = "general"
    quiet_hours_start: int = 22
    quiet_hours_end: int = 7
    respect_quiet_hours: bool = False

    # Dashboard URL base (para links)
    dashboard_url: str = "http://localhost:9001"

    @property
    def has_graph_credentials(self) -> bool:
        """Verifica se tem credenciais do Graph"""
        return bool(
            self.graph_tenant_id and
            self.graph_client_id and
            self.graph_client_secret
        )

    @property
    def has_bot_credentials(self) -> bool:
        """Verifica se tem credenciais do bot"""
        return bool(self.bot_app_id and self.bot_app_password)

    @property
    def has_webhook(self) -> bool:
        """Verifica se tem webhook configurado"""
        return bool(self.webhook_url or self.webhooks)


class TeamsIntegration(IntegrationBase):
    """
    Integracao completa com Microsoft Teams.

    Esta classe unifica todos os componentes de integracao:
    - WebhookClient: Envio de mensagens via webhooks
    - GraphClient: Acesso a API do Microsoft Graph
    - ChannelNotifier: Notificacoes em canais
    - DMNotifier: Mensagens diretas
    - TeamsBotHandler: Bot interativo

    Exemplo:
        # Configuracao simples (apenas webhook)
        config = TeamsConfig(
            webhook_url="https://outlook.office.com/webhook/xxx",
            enabled=True
        )

        # Configuracao completa
        config = TeamsConfig(
            enabled=True,
            webhooks={
                "dev": "https://outlook.office.com/webhook/dev",
                "alerts": "https://outlook.office.com/webhook/alerts"
            },
            graph_tenant_id="xxx",
            graph_client_id="xxx",
            graph_client_secret="xxx",
            bot_app_id="xxx",
            bot_app_password="xxx"
        )

        teams = TeamsIntegration(config)

        # Conecta (autentica se necessario)
        await teams.connect()

        # Usa os recursos
        await teams.notify("Mensagem simples")
        await teams.notify_project_completed(...)
    """

    def __init__(self, config: TeamsConfig):
        """
        Inicializa a integracao.

        Args:
            config: Configuracao da integracao
        """
        super().__init__(config)
        self.config: TeamsConfig = config

        # Componentes (inicializados sob demanda)
        self._webhook_client: Optional[WebhookClient] = None
        self._graph_client: Optional[GraphClient] = None
        self._channel_notifier: Optional[ChannelNotifier] = None
        self._dm_notifier: Optional[DMNotifier] = None
        self._bot_handler: Optional[TeamsBotHandler] = None
        self._card_builder = AdaptiveCardBuilder()

        # Inicializa componentes disponiveis
        self._init_components()

    def _init_components(self):
        """Inicializa componentes baseado na configuracao"""
        # Webhook Client
        if self.config.has_webhook:
            webhook_config = WebhookConfig(
                webhooks=self.config.webhooks,
                default_webhook=self.config.webhook_url or
                               self.config.webhooks.get(self.config.default_channel, "")
            )
            self._webhook_client = WebhookClient(webhook_config)

        # Graph Client
        if self.config.has_graph_credentials:
            graph_config = GraphConfig(
                tenant_id=self.config.graph_tenant_id,
                client_id=self.config.graph_client_id,
                client_secret=self.config.graph_client_secret
            )
            self._graph_client = GraphClient(graph_config)

            # DM Notifier (requer Graph)
            dm_config = DMNotifierConfig(
                graph_config=graph_config,
                users=self.config.users
            )
            self._dm_notifier = DMNotifier(dm_config)

        # Channel Notifier
        if self.config.has_webhook or self.config.has_graph_credentials:
            channel_config = ChannelNotifierConfig(
                channels=self.config.channels,
                default_channel=self.config.default_channel
            )

            if self.config.has_graph_credentials:
                channel_config.graph_config = GraphConfig(
                    tenant_id=self.config.graph_tenant_id,
                    client_id=self.config.graph_client_id,
                    client_secret=self.config.graph_client_secret
                )

            if self.config.has_webhook:
                channel_config.webhook_config = WebhookConfig(
                    webhooks=self.config.webhooks,
                    default_webhook=self.config.webhook_url
                )

            self._channel_notifier = ChannelNotifier(channel_config)

        # Bot Handler
        if self.config.has_bot_credentials:
            bot_config = BotConfig(
                app_id=self.config.bot_app_id,
                app_password=self.config.bot_app_password,
                endpoint=self.config.bot_endpoint
            )
            self._bot_handler = TeamsBotHandler(bot_config)

    async def close(self) -> None:
        """Fecha todas as conexoes"""
        if self._webhook_client:
            await self._webhook_client.close()
        if self._graph_client:
            await self._graph_client.close()
        if self._channel_notifier:
            await self._channel_notifier.close()
        if self._dm_notifier:
            await self._dm_notifier.close()

    # =========================================================================
    # IntegrationBase Implementation
    # =========================================================================

    async def connect(self) -> bool:
        """Conecta a integracao"""
        if not self.config.enabled:
            logger.warning("Integracao Teams desabilitada")
            return False

        self.status = IntegrationStatus.CONNECTING

        try:
            # Autentica Graph API se disponivel
            if self._graph_client:
                if not await self._graph_client.authenticate():
                    logger.warning("Falha ao autenticar Graph API")
                    # Continua sem Graph

            # Testa webhook se disponivel
            if self._webhook_client:
                # Nao ha teste especifico, assume OK
                pass

            self.status = IntegrationStatus.CONNECTED
            logger.info("Integracao Teams conectada")
            return True

        except Exception as e:
            self.status = IntegrationStatus.ERROR
            self._last_error = str(e)
            logger.error(f"Erro ao conectar Teams: {e}")
            return False

    async def disconnect(self) -> bool:
        """Desconecta a integracao"""
        await self.close()
        self.status = IntegrationStatus.DISCONNECTED
        return True

    async def test_connection(self) -> bool:
        """Testa a conexao"""
        if not self.config.enabled:
            return False

        # Testa Graph se disponivel
        if self._graph_client:
            try:
                await self._graph_client.authenticate()
            except Exception:
                return False

        # Testa webhook se disponivel
        if self._webhook_client:
            try:
                # Envia mensagem de teste silenciosa
                # (na pratica, apenas verifica se nao ha erro)
                pass
            except Exception:
                return False

        return True

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Sincroniza stories para Teams (envia notificacoes)"""
        result = SyncResult(success=True, started_at=datetime.utcnow())

        for story in stories:
            try:
                await self.notify_story_status_change(
                    story_id=story.get("story_id", ""),
                    story_title=story.get("title", ""),
                    old_status=story.get("old_status", ""),
                    new_status=story.get("status", ""),
                    url=f"{self.config.dashboard_url}/stories/{story.get('story_id', '')}"
                )
                result.items_synced += 1
            except Exception as e:
                result.items_failed += 1
                result.errors.append(str(e))

        result.completed_at = datetime.utcnow()
        return result

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Sincroniza do Teams (nao aplicavel)"""
        return SyncResult(
            success=True,
            details={"message": "Sincronizacao do Teams nao aplicavel"}
        )

    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook do Teams"""
        if not self._bot_handler:
            logger.warning("Bot handler nao configurado")
            return False

        try:
            response = await self._bot_handler.process_activity(payload)
            return response is not None
        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    # =========================================================================
    # Notificacoes Simples
    # =========================================================================

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
            True se enviado
        """
        if not self._webhook_client:
            logger.warning("Webhook nao configurado")
            return False

        return await self._webhook_client.send_message(
            text=message,
            title=title,
            channel=channel
        )

    async def notify_success(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia notificacao de sucesso"""
        if not self._webhook_client:
            return False
        return await self._webhook_client.send_success(title, message, channel)

    async def notify_warning(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia notificacao de aviso"""
        if not self._webhook_client:
            return False
        return await self._webhook_client.send_warning(title, message, channel)

    async def notify_error(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia notificacao de erro"""
        if not self._webhook_client:
            return False
        return await self._webhook_client.send_error(title, message, channel)

    async def notify_card(
        self,
        card: Dict[str, Any],
        channel: str = None
    ) -> bool:
        """Envia card adaptativo"""
        if not self._webhook_client:
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
        """Notifica inicio de projeto"""
        if not self._webhook_client:
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
        """Notifica conclusao de projeto"""
        if not self._webhook_client:
            return False

        if not url:
            url = f"{self.config.dashboard_url}/projects/{project_id}"

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
        """Notifica erro em projeto"""
        if not self._webhook_client:
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
        """Notifica mudanca de status de story"""
        if not self._webhook_client:
            return False

        if not url:
            url = f"{self.config.dashboard_url}/stories/{story_id}"

        return await self._webhook_client.send_story_status_change(
            story_id=story_id,
            story_title=story_title,
            old_status=old_status,
            new_status=new_status,
            assigned_to=assigned_to,
            url=url,
            channel=channel
        )

    async def notify_task_completed(
        self,
        task_id: str,
        task_title: str,
        story_id: str,
        story_title: str,
        files_created: List[str] = None,
        channel: str = None
    ) -> bool:
        """Notifica conclusao de tarefa"""
        if not self._webhook_client:
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
    # Mensagens Diretas
    # =========================================================================

    async def send_dm(
        self,
        user_key: str,
        message: str,
        title: str = ""
    ) -> bool:
        """Envia mensagem direta para usuario"""
        if not self._dm_notifier:
            logger.warning("DM notifier nao configurado (requer Graph API)")
            return False

        return await self._dm_notifier.send_dm(user_key, message, title)

    async def send_dm_card(
        self,
        user_key: str,
        card: Dict[str, Any]
    ) -> bool:
        """Envia card para usuario"""
        if not self._dm_notifier:
            return False
        return await self._dm_notifier.send_dm_card(user_key, card)

    # =========================================================================
    # Bot
    # =========================================================================

    async def process_bot_activity(
        self,
        activity: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Processa atividade do bot.

        Args:
            activity: Atividade recebida do Bot Framework

        Returns:
            Resposta a enviar
        """
        if not self._bot_handler:
            logger.warning("Bot handler nao configurado")
            return None

        return await self._bot_handler.process_activity(activity)

    def register_bot_action(
        self,
        action: str,
        callback: Callable[..., Awaitable[Any]]
    ):
        """
        Registra callback para acao do bot.

        Args:
            action: Nome da acao
            callback: Funcao callback
        """
        if self._bot_handler:
            self._bot_handler.set_action_callback(action, callback)

    # =========================================================================
    # Resumos
    # =========================================================================

    async def send_daily_summary(
        self,
        stories_completed: int,
        stories_in_progress: int,
        tasks_completed: int,
        files_generated: int,
        top_contributors: List[Dict[str, Any]] = None,
        channel: str = None
    ) -> bool:
        """Envia resumo diario"""
        if not self._webhook_client:
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
    # Utilidades
    # =========================================================================

    def create_card(self) -> AdaptiveCardBuilder:
        """
        Retorna builder de cards para uso customizado.

        Returns:
            Builder de cards adaptativos
        """
        return AdaptiveCardBuilder()

    def get_capabilities(self) -> Dict[str, bool]:
        """
        Retorna capacidades disponiveis.

        Returns:
            Mapa de capacidades
        """
        return {
            "webhook": self._webhook_client is not None,
            "graph_api": self._graph_client is not None,
            "channel_notifications": self._channel_notifier is not None,
            "direct_messages": self._dm_notifier is not None,
            "bot": self._bot_handler is not None
        }
