# -*- coding: utf-8 -*-
"""
Tests for Notification System
Plataforma E v6.5

Tests for Issue #432:
1. Notification manager
2. Channel registration
3. Rule engine
4. Template rendering
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from datetime import datetime


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def notification_manager():
    """Create notification manager instance"""
    from factory.notifications.notification_manager import NotificationManager
    return NotificationManager()


# =============================================================================
# NOTIFICATION RESULT TESTS
# =============================================================================

class TestNotificationResult:
    """Tests for NotificationResult dataclass"""

    def test_result_creation(self):
        """Should create notification result"""
        from factory.notifications.notification_manager import NotificationResult

        result = NotificationResult(
            notification_id="NOT-001",
            event_type="project_completed",
            success=True,
            channels_sent=["email", "slack"],
            channels_failed=[],
            recipients_count=5,
            responses=[],
            timestamp=datetime.now()
        )

        assert result.notification_id == "NOT-001"
        assert result.success is True
        assert "email" in result.channels_sent
        assert result.recipients_count == 5

    def test_result_with_failure(self):
        """Should track failed channels"""
        from factory.notifications.notification_manager import NotificationResult

        result = NotificationResult(
            notification_id="NOT-002",
            event_type="task_assigned",
            success=False,
            channels_sent=["email"],
            channels_failed=["slack"],
            recipients_count=1,
            responses=[],
            timestamp=datetime.now()
        )

        assert result.success is False
        assert "slack" in result.channels_failed


# =============================================================================
# NOTIFICATION MANAGER TESTS
# =============================================================================

class TestNotificationManager:
    """Tests for NotificationManager class"""

    def test_init_creates_channels_dict(self, notification_manager):
        """Should initialize with empty channels"""
        assert hasattr(notification_manager, 'channels')
        assert isinstance(notification_manager.channels, dict)

    def test_init_creates_rule_engine(self, notification_manager):
        """Should initialize rule engine"""
        assert hasattr(notification_manager, 'rule_engine')
        assert notification_manager.rule_engine is not None

    def test_enabled_by_default(self, notification_manager):
        """Should be enabled by default"""
        assert notification_manager.enabled is True

    def test_channel_classes_defined(self, notification_manager):
        """Should have channel classes defined"""
        assert "email" in notification_manager.CHANNEL_CLASSES
        assert "slack" in notification_manager.CHANNEL_CLASSES
        assert "teams" in notification_manager.CHANNEL_CLASSES


# =============================================================================
# CHANNEL REGISTRATION TESTS
# =============================================================================

class TestChannelRegistration:
    """Tests for channel registration"""

    def test_register_channel(self, notification_manager):
        """Should register a channel with required config"""
        notification_manager.register_channel(
            channel_type="email",
            config={
                "smtp_host": "localhost",
                "smtp_port": 587,
                "username": "test@test.com",
                "password": "test123",
                "from_email": "noreply@test.com"
            }
        )

        # Channel is registered with type-uuid format
        assert len(notification_manager.channels) > 0
        assert any("email" in key for key in notification_manager.channels.keys())

    def test_register_invalid_channel(self, notification_manager):
        """Should handle invalid channel type"""
        # Should not raise, just log warning
        notification_manager.register_channel(
            channel_type="invalid_channel",
            config={}
        )


# =============================================================================
# RULE ENGINE TESTS
# =============================================================================

class TestRuleEngine:
    """Tests for notification rules"""

    def test_rule_engine_exists(self, notification_manager):
        """Should have rule engine"""
        assert notification_manager.rule_engine is not None

    def test_quiet_hours_configurable(self, notification_manager):
        """Should support quiet hours configuration"""
        notification_manager.rule_engine.set_quiet_hours(
            start="22:00",
            end="08:00",
            timezone="America/Sao_Paulo"
        )

        # Should not raise


# =============================================================================
# TEMPLATE TESTS
# =============================================================================

class TestTemplates:
    """Tests for notification templates"""

    def test_templates_dict_exists(self, notification_manager):
        """Should have templates dictionary"""
        assert hasattr(notification_manager, 'templates')
        assert isinstance(notification_manager.templates, dict)


# =============================================================================
# CALLBACK TESTS
# =============================================================================

class TestCallbacks:
    """Tests for notification callbacks"""

    def test_on_send_callbacks_list(self, notification_manager):
        """Should have on_send callbacks list"""
        assert hasattr(notification_manager, '_on_send_callbacks')
        assert isinstance(notification_manager._on_send_callbacks, list)

    def test_on_error_callbacks_list(self, notification_manager):
        """Should have on_error callbacks list"""
        assert hasattr(notification_manager, '_on_error_callbacks')
        assert isinstance(notification_manager._on_error_callbacks, list)


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestNotificationIntegration:
    """Integration tests for notification system"""

    def test_disable_notifications(self, notification_manager):
        """Should be able to disable notifications"""
        notification_manager.enabled = False
        assert notification_manager.enabled is False

    def test_channel_configs_stored(self, notification_manager):
        """Should store channel configurations"""
        assert hasattr(notification_manager, 'channel_configs')
        assert isinstance(notification_manager.channel_configs, dict)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
