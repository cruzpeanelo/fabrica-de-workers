# -*- coding: utf-8 -*-
"""
Canais de Notificacao - Plataforma E
Modulo que exporta todos os canais disponiveis
"""

from .base_channel import BaseChannel, NotificationMessage, ChannelResponse
from .email_channel import EmailChannel
from .slack_channel import SlackChannel
from .teams_channel import TeamsChannel

__all__ = [
    "BaseChannel",
    "NotificationMessage",
    "ChannelResponse",
    "EmailChannel",
    "SlackChannel",
    "TeamsChannel",
]
