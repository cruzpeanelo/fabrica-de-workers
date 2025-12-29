# -*- coding: utf-8 -*-
"""
Microsoft Teams Integration Module
==================================
Integracao completa com Microsoft Teams para notificacoes,
colaboracao e interacao com os agentes da Fabrica.

Funcionalidades:
- Notificacoes em canais via Webhooks
- Cards Adaptativos para mensagens ricas
- Bot interativo para comandos
- Integracao com Microsoft Graph API

Uso:
    from factory.integrations.teams import TeamsIntegration, TeamsConfig

    config = TeamsConfig(
        webhook_url="https://webhook.office.com/...",
        enabled=True
    )
    teams = TeamsIntegration(config)
    await teams.send_notification("Projeto concluido!")
"""

from .graph_client import GraphClient, GraphConfig
from .webhook_client import WebhookClient, WebhookConfig
from .teams_connector import TeamsIntegration, TeamsConfig
from .notifications.card_builder import AdaptiveCardBuilder, CardType
from .notifications.channel_notifier import ChannelNotifier
from .notifications.dm_notifier import DMNotifier
from .bot.bot_handler import TeamsBotHandler
from .bot.commands import BotCommandHandler
from .bot.cards import BotCardBuilder

__all__ = [
    # Principal
    'TeamsIntegration',
    'TeamsConfig',
    # Graph API
    'GraphClient',
    'GraphConfig',
    # Webhooks
    'WebhookClient',
    'WebhookConfig',
    # Cards
    'AdaptiveCardBuilder',
    'CardType',
    'BotCardBuilder',
    # Notificadores
    'ChannelNotifier',
    'DMNotifier',
    # Bot
    'TeamsBotHandler',
    'BotCommandHandler',
]
