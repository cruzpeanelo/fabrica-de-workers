# -*- coding: utf-8 -*-
"""
Plugin Manager - Plataforma E
=============================

Manages plugin lifecycle: discovery, loading, activation.

Issue #443: Plugin System - Arquitetura de Extensibilidade
"""

import os
import sys
import json
import logging
import importlib
import importlib.util
from pathlib import Path
from typing import Dict, List, Optional, Type, Any
from dataclasses import dataclass, field

from .plugin_base import PluginBase, PluginStatus, PluginMetadata

logger = logging.getLogger(__name__)


@dataclass
class PluginInfo:
    """Information about a registered plugin."""
    plugin_id: str
    instance: PluginBase
    module_path: Optional[str] = None
    enabled: bool = True


class PluginManager:
    """
    Manages plugin lifecycle.

    Usage:
        manager = PluginManager()
        manager.discover_plugins("./plugins")
        manager.load_plugin("my_plugin")
        manager.activate_plugin("my_plugin")
    """

    def __init__(self, plugins_dir: Optional[str] = None):
        """
        Initialize plugin manager.

        Args:
            plugins_dir: Directory to scan for plugins
        """
        self._plugins: Dict[str, PluginInfo] = {}
        self._plugins_dir = Path(plugins_dir) if plugins_dir else None
        self._config_file: Optional[Path] = None

    def register_plugin(self, plugin_id: str, plugin_class: Type[PluginBase],
                        module_path: Optional[str] = None) -> bool:
        """
        Register a plugin class.

        Args:
            plugin_id: Unique plugin identifier
            plugin_class: Plugin class (not instance)
            module_path: Path to the module file

        Returns:
            True if registration was successful
        """
        if plugin_id in self._plugins:
            logger.warning(f"Plugin {plugin_id} already registered")
            return False

        try:
            instance = plugin_class()
            self._plugins[plugin_id] = PluginInfo(
                plugin_id=plugin_id,
                instance=instance,
                module_path=module_path
            )
            logger.info(f"Registered plugin: {plugin_id}")
            return True

        except Exception as e:
            logger.error(f"Failed to register plugin {plugin_id}: {e}")
            return False

    def unregister_plugin(self, plugin_id: str) -> bool:
        """
        Unregister a plugin.

        Args:
            plugin_id: Plugin identifier

        Returns:
            True if unregistration was successful
        """
        if plugin_id not in self._plugins:
            return False

        info = self._plugins[plugin_id]

        # Unload if loaded
        if info.instance.status in [PluginStatus.LOADED, PluginStatus.ACTIVE]:
            self.unload_plugin(plugin_id)

        del self._plugins[plugin_id]
        logger.info(f"Unregistered plugin: {plugin_id}")
        return True

    def discover_plugins(self, directory: str) -> List[str]:
        """
        Discover plugins in a directory.

        Looks for Python files with a `Plugin` class that inherits from PluginBase.

        Args:
            directory: Directory to scan

        Returns:
            List of discovered plugin IDs
        """
        discovered = []
        plugins_path = Path(directory)

        if not plugins_path.exists():
            logger.warning(f"Plugins directory not found: {directory}")
            return discovered

        for file_path in plugins_path.glob("*.py"):
            if file_path.name.startswith("_"):
                continue

            plugin_id = file_path.stem
            if self._load_plugin_module(plugin_id, str(file_path)):
                discovered.append(plugin_id)

        logger.info(f"Discovered {len(discovered)} plugins")
        return discovered

    def _load_plugin_module(self, plugin_id: str, file_path: str) -> bool:
        """Load a plugin module from file."""
        try:
            spec = importlib.util.spec_from_file_location(plugin_id, file_path)
            if spec is None or spec.loader is None:
                return False

            module = importlib.util.module_from_spec(spec)
            sys.modules[plugin_id] = module
            spec.loader.exec_module(module)

            # Find Plugin class
            plugin_class = None
            for name in dir(module):
                obj = getattr(module, name)
                if (isinstance(obj, type) and
                    issubclass(obj, PluginBase) and
                    obj is not PluginBase):
                    plugin_class = obj
                    break

            if plugin_class:
                return self.register_plugin(plugin_id, plugin_class, file_path)

            return False

        except Exception as e:
            logger.error(f"Failed to load plugin module {plugin_id}: {e}")
            return False

    def load_plugin(self, plugin_id: str) -> bool:
        """
        Load a plugin.

        Args:
            plugin_id: Plugin identifier

        Returns:
            True if loading was successful
        """
        if plugin_id not in self._plugins:
            logger.error(f"Plugin not found: {plugin_id}")
            return False

        info = self._plugins[plugin_id]
        plugin = info.instance

        if plugin.status != PluginStatus.UNLOADED:
            logger.warning(f"Plugin {plugin_id} is already loaded")
            return False

        try:
            if plugin.on_load():
                plugin._status = PluginStatus.LOADED
                logger.info(f"Loaded plugin: {plugin_id}")
                return True
            else:
                plugin._status = PluginStatus.ERROR
                return False

        except Exception as e:
            plugin._status = PluginStatus.ERROR
            plugin._error = str(e)
            logger.error(f"Failed to load plugin {plugin_id}: {e}")
            return False

    def unload_plugin(self, plugin_id: str) -> bool:
        """
        Unload a plugin.

        Args:
            plugin_id: Plugin identifier

        Returns:
            True if unloading was successful
        """
        if plugin_id not in self._plugins:
            return False

        info = self._plugins[plugin_id]
        plugin = info.instance

        # Deactivate first if active
        if plugin.status == PluginStatus.ACTIVE:
            self.deactivate_plugin(plugin_id)

        if plugin.status not in [PluginStatus.LOADED, PluginStatus.ERROR]:
            return False

        try:
            plugin.on_unload()
            plugin._status = PluginStatus.UNLOADED
            logger.info(f"Unloaded plugin: {plugin_id}")
            return True

        except Exception as e:
            logger.error(f"Failed to unload plugin {plugin_id}: {e}")
            return False

    def activate_plugin(self, plugin_id: str) -> bool:
        """
        Activate a plugin.

        Args:
            plugin_id: Plugin identifier

        Returns:
            True if activation was successful
        """
        if plugin_id not in self._plugins:
            return False

        info = self._plugins[plugin_id]
        plugin = info.instance

        # Load first if needed
        if plugin.status == PluginStatus.UNLOADED:
            if not self.load_plugin(plugin_id):
                return False

        if plugin.status != PluginStatus.LOADED:
            return False

        try:
            if plugin.on_activate():
                plugin._status = PluginStatus.ACTIVE
                logger.info(f"Activated plugin: {plugin_id}")
                return True
            return False

        except Exception as e:
            plugin._error = str(e)
            logger.error(f"Failed to activate plugin {plugin_id}: {e}")
            return False

    def deactivate_plugin(self, plugin_id: str) -> bool:
        """
        Deactivate a plugin.

        Args:
            plugin_id: Plugin identifier

        Returns:
            True if deactivation was successful
        """
        if plugin_id not in self._plugins:
            return False

        info = self._plugins[plugin_id]
        plugin = info.instance

        if plugin.status != PluginStatus.ACTIVE:
            return False

        try:
            plugin.on_deactivate()
            plugin._status = PluginStatus.LOADED
            logger.info(f"Deactivated plugin: {plugin_id}")
            return True

        except Exception as e:
            logger.error(f"Failed to deactivate plugin {plugin_id}: {e}")
            return False

    def enable_plugin(self, plugin_id: str) -> bool:
        """Enable a disabled plugin."""
        if plugin_id not in self._plugins:
            return False
        self._plugins[plugin_id].enabled = True
        return True

    def disable_plugin(self, plugin_id: str) -> bool:
        """Disable a plugin."""
        if plugin_id not in self._plugins:
            return False

        self.deactivate_plugin(plugin_id)
        self._plugins[plugin_id].enabled = False
        self._plugins[plugin_id].instance._status = PluginStatus.DISABLED
        return True

    def configure_plugin(self, plugin_id: str, config: Dict[str, Any]) -> bool:
        """
        Configure a plugin.

        Args:
            plugin_id: Plugin identifier
            config: Configuration dictionary

        Returns:
            True if configuration was applied
        """
        if plugin_id not in self._plugins:
            return False

        return self._plugins[plugin_id].instance.configure(config)

    def get_plugin(self, plugin_id: str) -> Optional[PluginBase]:
        """Get a plugin instance."""
        if plugin_id not in self._plugins:
            return None
        return self._plugins[plugin_id].instance

    def get_plugin_info(self, plugin_id: str) -> Optional[dict]:
        """Get plugin information."""
        if plugin_id not in self._plugins:
            return None

        info = self._plugins[plugin_id]
        return {
            "plugin_id": info.plugin_id,
            "enabled": info.enabled,
            "module_path": info.module_path,
            **info.instance.get_info()
        }

    def list_plugins(self) -> List[dict]:
        """List all registered plugins."""
        return [self.get_plugin_info(pid) for pid in self._plugins]

    def get_active_plugins(self) -> List[PluginBase]:
        """Get all active plugins."""
        return [
            info.instance for info in self._plugins.values()
            if info.instance.status == PluginStatus.ACTIVE
        ]

    def get_plugins_by_type(self, plugin_type: Type[PluginBase]) -> List[PluginBase]:
        """Get all plugins of a specific type."""
        return [
            info.instance for info in self._plugins.values()
            if isinstance(info.instance, plugin_type)
        ]


# Global plugin manager
_plugin_manager: Optional[PluginManager] = None


def get_plugin_manager() -> PluginManager:
    """Get global plugin manager."""
    global _plugin_manager
    if _plugin_manager is None:
        _plugin_manager = PluginManager()
    return _plugin_manager


def init_plugin_manager(plugins_dir: Optional[str] = None) -> PluginManager:
    """Initialize global plugin manager."""
    global _plugin_manager
    _plugin_manager = PluginManager(plugins_dir)
    return _plugin_manager
