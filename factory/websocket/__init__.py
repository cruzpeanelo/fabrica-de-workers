# -*- coding: utf-8 -*-
"""
WebSocket Module - Fabrica de Agentes
=====================================
Sistema de WebSocket para atualizacoes em tempo real.
"""

from .manager import WebSocketManager, ws_manager
from .events import WebSocketEvent, EventType

__all__ = ['WebSocketManager', 'ws_manager', 'WebSocketEvent', 'EventType']
