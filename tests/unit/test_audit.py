# -*- coding: utf-8 -*-
"""
Tests for Audit System
Plataforma E v6.5

Tests for Issue #439
"""

import pytest
from datetime import datetime, timedelta


class TestAuditAction:
    """Tests for AuditAction enum"""

    def test_action_values(self):
        from factory.audit.models import AuditAction

        assert AuditAction.CREATE.value == "CREATE"
        assert AuditAction.UPDATE.value == "UPDATE"
        assert AuditAction.DELETE.value == "DELETE"
        assert AuditAction.LOGIN.value == "LOGIN"


class TestAuditLog:
    """Tests for AuditLog model"""

    def test_log_creation(self):
        from factory.audit.models import AuditLog, AuditAction

        log = AuditLog(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )

        assert log.user_id == "user-1"
        assert log.action == AuditAction.CREATE
        assert log.resource_type == "story"
        assert log.id is not None

    def test_log_with_values(self):
        from factory.audit.models import AuditLog, AuditAction

        log = AuditLog(
            user_id="user-1",
            action=AuditAction.UPDATE,
            resource_type="story",
            resource_id="STR-001",
            old_value={"status": "backlog"},
            new_value={"status": "in_progress"},
        )

        assert log.old_value["status"] == "backlog"
        assert log.new_value["status"] == "in_progress"

    def test_log_to_dict(self):
        from factory.audit.models import AuditLog, AuditAction

        log = AuditLog(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )

        d = log.to_dict()

        assert d["user_id"] == "user-1"
        assert d["action"] == "CREATE"
        assert "timestamp" in d

    def test_log_from_dict(self):
        from factory.audit.models import AuditLog, AuditAction

        data = {
            "user_id": "user-2",
            "action": "DELETE",
            "resource_type": "task",
            "resource_id": "TSK-001",
        }

        log = AuditLog.from_dict(data)

        assert log.user_id == "user-2"
        assert log.action == AuditAction.DELETE

    def test_get_changes(self):
        from factory.audit.models import AuditLog, AuditAction

        log = AuditLog(
            user_id="user-1",
            action=AuditAction.UPDATE,
            resource_type="story",
            resource_id="STR-001",
            old_value={"status": "backlog", "points": 5},
            new_value={"status": "done", "points": 5},
        )

        changes = log.get_changes()

        assert "status" in changes
        assert changes["status"]["old"] == "backlog"
        assert changes["status"]["new"] == "done"
        assert "points" not in changes  # Unchanged

    def test_summary(self):
        from factory.audit.models import AuditLog, AuditAction

        log = AuditLog(
            user_id="admin",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )

        assert log.summary == "admin CREATE story/STR-001"


class TestAuditLogger:
    """Tests for AuditLogger"""

    @pytest.fixture
    def logger(self):
        from factory.audit.audit_logger import AuditLogger
        return AuditLogger()

    def test_log_action(self, logger):
        from factory.audit.models import AuditAction

        log = logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
            new_value={"title": "New Story"},
        )

        assert log is not None
        assert logger.total_logs == 1

    def test_log_create_helper(self, logger):
        log = logger.log_create(
            user_id="user-1",
            resource_type="story",
            resource_id="STR-001",
            new_value={"title": "Test"},
        )

        from factory.audit.models import AuditAction
        assert log.action == AuditAction.CREATE

    def test_log_update_helper(self, logger):
        log = logger.log_update(
            user_id="user-1",
            resource_type="story",
            resource_id="STR-001",
            old_value={"status": "backlog"},
            new_value={"status": "done"},
        )

        from factory.audit.models import AuditAction
        assert log.action == AuditAction.UPDATE

    def test_log_delete_helper(self, logger):
        log = logger.log_delete(
            user_id="user-1",
            resource_type="story",
            resource_id="STR-001",
            old_value={"title": "Deleted Story"},
        )

        from factory.audit.models import AuditAction
        assert log.action == AuditAction.DELETE


class TestAuditQueries:
    """Tests for audit query methods"""

    @pytest.fixture
    def logger_with_data(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction

        logger = AuditLogger()

        # Add some test logs
        logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )
        logger.log(
            user_id="user-2",
            action=AuditAction.UPDATE,
            resource_type="story",
            resource_id="STR-001",
        )
        logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="task",
            resource_id="TSK-001",
        )

        return logger

    def test_get_logs_all(self, logger_with_data):
        logs = logger_with_data.get_logs()
        assert len(logs) == 3

    def test_get_logs_by_user(self, logger_with_data):
        logs = logger_with_data.get_logs(user_id="user-1")
        assert len(logs) == 2

    def test_get_logs_by_action(self, logger_with_data):
        from factory.audit.models import AuditAction

        logs = logger_with_data.get_logs(action=AuditAction.CREATE)
        assert len(logs) == 2

    def test_get_logs_by_resource(self, logger_with_data):
        logs = logger_with_data.get_logs(
            resource_type="story",
            resource_id="STR-001",
        )
        assert len(logs) == 2

    def test_get_logs_pagination(self, logger_with_data):
        logs = logger_with_data.get_logs(limit=2)
        assert len(logs) == 2

        logs = logger_with_data.get_logs(limit=2, offset=2)
        assert len(logs) == 1

    def test_get_resource_history(self, logger_with_data):
        history = logger_with_data.get_resource_history("story", "STR-001")
        assert len(history) == 2

    def test_get_user_activity(self, logger_with_data):
        activity = logger_with_data.get_user_activity("user-1")
        assert len(activity) == 2

    def test_count_logs(self, logger_with_data):
        count = logger_with_data.count_logs(user_id="user-1")
        assert count == 2


class TestAuditStatistics:
    """Tests for audit statistics"""

    @pytest.fixture
    def logger_with_data(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction

        logger = AuditLogger()

        for i in range(5):
            logger.log(
                user_id="user-1",
                action=AuditAction.CREATE,
                resource_type="story",
                resource_id=f"STR-00{i}",
            )

        for i in range(3):
            logger.log(
                user_id="user-2",
                action=AuditAction.UPDATE,
                resource_type="task",
                resource_id=f"TSK-00{i}",
            )

        return logger

    def test_get_statistics(self, logger_with_data):
        stats = logger_with_data.get_statistics()

        assert stats["total_logs"] == 8
        assert stats["by_action"]["CREATE"] == 5
        assert stats["by_action"]["UPDATE"] == 3
        assert stats["by_resource"]["story"] == 5

    def test_most_active_users(self, logger_with_data):
        stats = logger_with_data.get_statistics()

        assert len(stats["most_active_users"]) == 2
        assert stats["most_active_users"][0][0] == "user-1"
        assert stats["most_active_users"][0][1] == 5


class TestAuditExport:
    """Tests for audit export methods"""

    @pytest.fixture
    def logger_with_data(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction

        logger = AuditLogger()
        logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )
        return logger

    def test_export_json(self, logger_with_data):
        import json

        output = logger_with_data.export_json()
        data = json.loads(output)

        assert len(data) == 1
        assert data[0]["user_id"] == "user-1"

    def test_export_csv(self, logger_with_data):
        output = logger_with_data.export_csv()

        assert "user_id" in output  # Header
        assert "user-1" in output


class TestAuditRetention:
    """Tests for audit retention"""

    def test_apply_retention(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction, AuditLog
        from datetime import datetime, timedelta

        logger = AuditLogger(retention_days=30)

        # Add old log
        old_log = AuditLog(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )
        # Manually set old timestamp
        old_log.timestamp = datetime.now() - timedelta(days=60)
        logger._logs.append(old_log)

        # Add recent log
        logger.log(
            user_id="user-2",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-002",
        )

        removed = logger.apply_retention()

        assert removed == 1
        assert logger.total_logs == 1

    def test_set_retention(self):
        from factory.audit.audit_logger import AuditLogger

        logger = AuditLogger()
        logger.set_retention(60)

        assert logger.retention_days == 60

    def test_invalid_retention(self):
        from factory.audit.audit_logger import AuditLogger

        logger = AuditLogger()

        with pytest.raises(ValueError):
            logger.set_retention(0)


class TestAuditListeners:
    """Tests for audit event listeners"""

    def test_add_listener(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction

        logger = AuditLogger()
        events = []

        def callback(log):
            events.append(log)

        logger.add_listener(callback)

        logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )

        assert len(events) == 1
        assert events[0].user_id == "user-1"

    def test_remove_listener(self):
        from factory.audit.audit_logger import AuditLogger
        from factory.audit.models import AuditAction

        logger = AuditLogger()
        events = []

        def callback(log):
            events.append(log)

        logger.add_listener(callback)
        logger.remove_listener(callback)

        logger.log(
            user_id="user-1",
            action=AuditAction.CREATE,
            resource_type="story",
            resource_id="STR-001",
        )

        assert len(events) == 0


class TestGlobalAuditLogger:
    """Tests for global singleton"""

    def test_get_audit_logger_singleton(self):
        from factory.audit.audit_logger import get_audit_logger

        logger1 = get_audit_logger()
        logger2 = get_audit_logger()

        assert logger1 is logger2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
