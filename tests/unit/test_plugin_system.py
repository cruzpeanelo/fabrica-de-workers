# -*- coding: utf-8 -*-
"""
Tests for Plugin System
Plataforma E v6.5

Tests for Issue #443
"""

import pytest
from typing import Dict, Any


class TestPluginStatus:
    """Tests for PluginStatus enum"""

    def test_status_values(self):
        from factory.plugins.plugin_base import PluginStatus

        assert PluginStatus.UNLOADED.value == "unloaded"
        assert PluginStatus.LOADED.value == "loaded"
        assert PluginStatus.ACTIVE.value == "active"
        assert PluginStatus.ERROR.value == "error"
        assert PluginStatus.DISABLED.value == "disabled"


class TestPluginMetadata:
    """Tests for PluginMetadata dataclass"""

    def test_metadata_creation(self):
        from factory.plugins.plugin_base import PluginMetadata

        meta = PluginMetadata(
            name="Test Plugin",
            version="1.0.0",
            description="A test plugin"
        )

        assert meta.name == "Test Plugin"
        assert meta.version == "1.0.0"
        assert meta.description == "A test plugin"

    def test_metadata_defaults(self):
        from factory.plugins.plugin_base import PluginMetadata

        meta = PluginMetadata(name="Test", version="1.0.0")
        assert meta.author == ""
        assert meta.dependencies == []
        assert meta.tags == []

    def test_metadata_to_dict(self):
        from factory.plugins.plugin_base import PluginMetadata

        meta = PluginMetadata(
            name="Test",
            version="1.0.0",
            author="Dev"
        )

        d = meta.to_dict()
        assert d["name"] == "Test"
        assert d["version"] == "1.0.0"
        assert d["author"] == "Dev"


class TestPluginBase:
    """Tests for PluginBase abstract class"""

    @pytest.fixture
    def sample_plugin_class(self):
        from factory.plugins.plugin_base import PluginBase, PluginMetadata

        class SamplePlugin(PluginBase):
            @property
            def metadata(self) -> PluginMetadata:
                return PluginMetadata(
                    name="Sample Plugin",
                    version="1.0.0",
                    description="A sample plugin for testing"
                )

            def on_load(self) -> bool:
                return True

            def on_unload(self) -> bool:
                return True

        return SamplePlugin

    def test_plugin_creation(self, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        plugin = sample_plugin_class()
        assert plugin.status == PluginStatus.UNLOADED
        assert plugin.config == {}
        assert plugin.error is None

    def test_plugin_metadata(self, sample_plugin_class):
        plugin = sample_plugin_class()
        assert plugin.metadata.name == "Sample Plugin"
        assert plugin.metadata.version == "1.0.0"

    def test_plugin_configure(self, sample_plugin_class):
        plugin = sample_plugin_class()
        result = plugin.configure({"key": "value"})
        assert result is True
        assert plugin.config == {"key": "value"}

    def test_plugin_get_info(self, sample_plugin_class):
        plugin = sample_plugin_class()
        info = plugin.get_info()

        assert "metadata" in info
        assert "status" in info
        assert "config" in info
        assert info["metadata"]["name"] == "Sample Plugin"


class TestPluginManager:
    """Tests for PluginManager"""

    @pytest.fixture
    def manager(self):
        from factory.plugins.plugin_manager import PluginManager
        return PluginManager()

    @pytest.fixture
    def sample_plugin_class(self):
        from factory.plugins.plugin_base import PluginBase, PluginMetadata

        class TestPlugin(PluginBase):
            @property
            def metadata(self) -> PluginMetadata:
                return PluginMetadata(name="TestPlugin", version="1.0.0")

            def on_load(self) -> bool:
                return True

            def on_unload(self) -> bool:
                return True

        return TestPlugin

    def test_register_plugin(self, manager, sample_plugin_class):
        result = manager.register_plugin("test", sample_plugin_class)
        assert result is True

        plugins = manager.list_plugins()
        assert len(plugins) == 1
        assert plugins[0]["plugin_id"] == "test"

    def test_register_duplicate(self, manager, sample_plugin_class):
        manager.register_plugin("test", sample_plugin_class)
        result = manager.register_plugin("test", sample_plugin_class)
        assert result is False

    def test_unregister_plugin(self, manager, sample_plugin_class):
        manager.register_plugin("test", sample_plugin_class)
        result = manager.unregister_plugin("test")
        assert result is True
        assert len(manager.list_plugins()) == 0

    def test_load_plugin(self, manager, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        manager.register_plugin("test", sample_plugin_class)
        result = manager.load_plugin("test")
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.status == PluginStatus.LOADED

    def test_load_nonexistent(self, manager):
        result = manager.load_plugin("nonexistent")
        assert result is False

    def test_unload_plugin(self, manager, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        manager.register_plugin("test", sample_plugin_class)
        manager.load_plugin("test")
        result = manager.unload_plugin("test")
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.status == PluginStatus.UNLOADED

    def test_activate_plugin(self, manager, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        manager.register_plugin("test", sample_plugin_class)
        result = manager.activate_plugin("test")
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.status == PluginStatus.ACTIVE

    def test_deactivate_plugin(self, manager, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        manager.register_plugin("test", sample_plugin_class)
        manager.activate_plugin("test")
        result = manager.deactivate_plugin("test")
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.status == PluginStatus.LOADED

    def test_disable_plugin(self, manager, sample_plugin_class):
        from factory.plugins.plugin_base import PluginStatus

        manager.register_plugin("test", sample_plugin_class)
        manager.activate_plugin("test")
        result = manager.disable_plugin("test")
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.status == PluginStatus.DISABLED

    def test_enable_plugin(self, manager, sample_plugin_class):
        manager.register_plugin("test", sample_plugin_class)
        manager.disable_plugin("test")
        result = manager.enable_plugin("test")
        assert result is True

    def test_configure_plugin(self, manager, sample_plugin_class):
        manager.register_plugin("test", sample_plugin_class)
        result = manager.configure_plugin("test", {"option": "value"})
        assert result is True

        plugin = manager.get_plugin("test")
        assert plugin.config["option"] == "value"

    def test_get_plugin_info(self, manager, sample_plugin_class):
        manager.register_plugin("test", sample_plugin_class)
        info = manager.get_plugin_info("test")

        assert info is not None
        assert info["plugin_id"] == "test"
        assert "metadata" in info
        assert "status" in info

    def test_get_active_plugins(self, manager, sample_plugin_class):
        manager.register_plugin("test1", sample_plugin_class)
        manager.register_plugin("test2", sample_plugin_class)
        manager.activate_plugin("test1")

        active = manager.get_active_plugins()
        assert len(active) == 1


class TestAgentPlugin:
    """Tests for AgentPlugin base class"""

    def test_agent_plugin_interface(self):
        from factory.plugins.plugin_base import AgentPlugin, PluginMetadata

        class CustomAgent(AgentPlugin):
            @property
            def metadata(self) -> PluginMetadata:
                return PluginMetadata(name="CustomAgent", version="1.0.0")

            @property
            def agent_type(self) -> str:
                return "CUSTOM"

            def on_load(self) -> bool:
                return True

            def on_unload(self) -> bool:
                return True

            def process_task(self, task: str, context: Dict[str, Any]) -> Dict[str, Any]:
                return {"result": f"Processed: {task}"}

        agent = CustomAgent()
        assert agent.agent_type == "CUSTOM"
        result = agent.process_task("test task", {})
        assert result["result"] == "Processed: test task"


class TestIntegrationPlugin:
    """Tests for IntegrationPlugin base class"""

    def test_integration_plugin_interface(self):
        from factory.plugins.plugin_base import IntegrationPlugin, PluginMetadata

        class SlackIntegration(IntegrationPlugin):
            def __init__(self):
                super().__init__()
                self._connected = False

            @property
            def metadata(self) -> PluginMetadata:
                return PluginMetadata(name="Slack", version="1.0.0")

            @property
            def service_name(self) -> str:
                return "Slack"

            def on_load(self) -> bool:
                return True

            def on_unload(self) -> bool:
                return True

            def connect(self) -> bool:
                self._connected = True
                return True

            def disconnect(self) -> bool:
                self._connected = False
                return True

            def is_connected(self) -> bool:
                return self._connected

        plugin = SlackIntegration()
        assert plugin.service_name == "Slack"
        assert plugin.is_connected() is False
        plugin.connect()
        assert plugin.is_connected() is True


class TestGlobalPluginManager:
    """Tests for global plugin manager"""

    def test_get_plugin_manager_singleton(self):
        from factory.plugins.plugin_manager import get_plugin_manager

        manager1 = get_plugin_manager()
        manager2 = get_plugin_manager()
        assert manager1 is manager2

    def test_init_plugin_manager(self):
        from factory.plugins.plugin_manager import init_plugin_manager, get_plugin_manager

        manager = init_plugin_manager()
        assert manager is get_plugin_manager()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
