# -*- coding: utf-8 -*-
"""
Plugin Base - Plataforma E
==========================

Base class and interfaces for plugins.

Issue #443: Plugin System - Arquitetura de Extensibilidade
"""

from abc import ABC, abstractmethod
from enum import Enum
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field


class PluginStatus(str, Enum):
    """Plugin lifecycle status."""
    UNLOADED = "unloaded"
    LOADED = "loaded"
    ACTIVE = "active"
    ERROR = "error"
    DISABLED = "disabled"


@dataclass
class PluginMetadata:
    """Plugin metadata."""
    name: str
    version: str
    description: str = ""
    author: str = ""
    homepage: str = ""
    dependencies: List[str] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    config_schema: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "author": self.author,
            "homepage": self.homepage,
            "dependencies": self.dependencies,
            "tags": self.tags
        }


class PluginBase(ABC):
    """
    Base class for all plugins.

    Plugins must implement:
    - metadata: Plugin information
    - on_load(): Called when plugin is loaded
    - on_unload(): Called when plugin is unloaded

    Optional hooks:
    - on_activate(): Called when plugin is activated
    - on_deactivate(): Called when plugin is deactivated
    - on_config_change(): Called when configuration changes
    """

    def __init__(self):
        self._status = PluginStatus.UNLOADED
        self._config: Dict[str, Any] = {}
        self._error: Optional[str] = None

    @property
    @abstractmethod
    def metadata(self) -> PluginMetadata:
        """Return plugin metadata."""
        pass

    @property
    def status(self) -> PluginStatus:
        """Get current status."""
        return self._status

    @property
    def config(self) -> Dict[str, Any]:
        """Get current configuration."""
        return self._config

    @property
    def error(self) -> Optional[str]:
        """Get error message if status is ERROR."""
        return self._error

    def configure(self, config: Dict[str, Any]) -> bool:
        """
        Apply configuration to plugin.

        Args:
            config: Configuration dictionary

        Returns:
            True if configuration was applied successfully
        """
        old_config = self._config.copy()
        self._config = config

        try:
            self.on_config_change(old_config, config)
            return True
        except Exception as e:
            self._config = old_config
            self._error = str(e)
            return False

    @abstractmethod
    def on_load(self) -> bool:
        """
        Called when plugin is loaded.

        Returns:
            True if loading was successful
        """
        pass

    @abstractmethod
    def on_unload(self) -> bool:
        """
        Called when plugin is unloaded.

        Returns:
            True if unloading was successful
        """
        pass

    def on_activate(self) -> bool:
        """
        Called when plugin is activated.

        Returns:
            True if activation was successful
        """
        return True

    def on_deactivate(self) -> bool:
        """
        Called when plugin is deactivated.

        Returns:
            True if deactivation was successful
        """
        return True

    def on_config_change(self, old_config: Dict, new_config: Dict):
        """
        Called when configuration changes.

        Args:
            old_config: Previous configuration
            new_config: New configuration
        """
        pass

    def get_info(self) -> dict:
        """Get plugin information."""
        return {
            "metadata": self.metadata.to_dict(),
            "status": self.status.value,
            "config": self._config,
            "error": self._error
        }


class AgentPlugin(PluginBase):
    """
    Plugin that adds a new agent type.

    Implement:
    - agent_type: The agent type identifier
    - process_task(): Handle agent tasks
    """

    @property
    @abstractmethod
    def agent_type(self) -> str:
        """Return the agent type identifier."""
        pass

    @abstractmethod
    def process_task(self, task: str, context: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process a task.

        Args:
            task: Task description
            context: Task context

        Returns:
            Result dictionary
        """
        pass


class IntegrationPlugin(PluginBase):
    """
    Plugin that integrates with external services.

    Implement:
    - service_name: The service being integrated
    - connect(): Establish connection
    - disconnect(): Close connection
    """

    @property
    @abstractmethod
    def service_name(self) -> str:
        """Return the service name."""
        pass

    @abstractmethod
    def connect(self) -> bool:
        """
        Connect to the external service.

        Returns:
            True if connection was successful
        """
        pass

    @abstractmethod
    def disconnect(self) -> bool:
        """
        Disconnect from the external service.

        Returns:
            True if disconnection was successful
        """
        pass

    @abstractmethod
    def is_connected(self) -> bool:
        """Check if connected to service."""
        pass


class WorkflowPlugin(PluginBase):
    """
    Plugin that adds workflow steps.

    Implement:
    - step_type: The step type identifier
    - execute_step(): Execute the step
    """

    @property
    @abstractmethod
    def step_type(self) -> str:
        """Return the step type identifier."""
        pass

    @abstractmethod
    def execute_step(self, inputs: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute the workflow step.

        Args:
            inputs: Step inputs

        Returns:
            Step outputs
        """
        pass
