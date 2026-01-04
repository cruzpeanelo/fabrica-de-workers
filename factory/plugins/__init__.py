# -*- coding: utf-8 -*-
"""
Plugin System - Plataforma E
============================

Extensibility architecture for the platform.
"""

from .plugin_base import PluginBase, PluginMetadata, PluginStatus
from .plugin_manager import PluginManager, get_plugin_manager

__all__ = [
    "PluginBase",
    "PluginMetadata",
    "PluginStatus",
    "PluginManager",
    "get_plugin_manager"
]
