# -*- coding: utf-8 -*-
"""
Tests for Backup Manager - Disaster Recovery
Plataforma E v6.5

Tests for Issue #435:
1. Backup configuration
2. Backup operations
3. Restore operations
4. Retention policies
5. Verification
"""

import pytest
from unittest.mock import Mock, patch, MagicMock, AsyncMock
from datetime import datetime, timedelta
from pathlib import Path
import tempfile
import os


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def temp_backup_dir():
    """Create temporary backup directory"""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def backup_config(temp_backup_dir):
    """Create test backup configuration"""
    from factory.core.backup_manager import BackupConfig

    return BackupConfig(
        backup_dir=temp_backup_dir,
        database_url="sqlite:///test.db",
        daily_retention=7,
        weekly_retention=28,
        monthly_retention=365,
        encryption_enabled=False,
        alert_on_failure=False
    )


@pytest.fixture
def backup_manager(backup_config):
    """Create backup manager instance"""
    from factory.core.backup_manager import BackupManager
    return BackupManager(backup_config)


# =============================================================================
# CONFIG TESTS
# =============================================================================

class TestBackupConfig:
    """Tests for BackupConfig"""

    def test_default_config(self):
        """Should have sensible defaults"""
        from factory.core.backup_manager import BackupConfig

        config = BackupConfig()

        assert config.daily_retention == 7
        assert config.weekly_retention == 28
        assert config.monthly_retention == 365
        assert config.compression_level == 9
        assert config.encryption_enabled is True

    def test_custom_config(self, temp_backup_dir):
        """Should accept custom configuration"""
        from factory.core.backup_manager import BackupConfig

        config = BackupConfig(
            backup_dir=temp_backup_dir,
            daily_retention=14,
            s3_bucket="my-bucket"
        )

        assert config.backup_dir == temp_backup_dir
        assert config.daily_retention == 14
        assert config.s3_bucket == "my-bucket"


# =============================================================================
# BACKUP TYPE TESTS
# =============================================================================

class TestBackupTypes:
    """Tests for backup types"""

    def test_backup_types_exist(self):
        """Should have all backup types"""
        from factory.core.backup_manager import BackupType

        assert BackupType.FULL.value == "full"
        assert BackupType.INCREMENTAL.value == "incremental"
        assert BackupType.DIFFERENTIAL.value == "differential"

    def test_backup_status_exist(self):
        """Should have all status values"""
        from factory.core.backup_manager import BackupStatus

        assert BackupStatus.PENDING.value == "pending"
        assert BackupStatus.RUNNING.value == "running"
        assert BackupStatus.COMPLETED.value == "completed"
        assert BackupStatus.FAILED.value == "failed"
        assert BackupStatus.VERIFIED.value == "verified"


# =============================================================================
# BACKUP MANAGER TESTS
# =============================================================================

class TestBackupManager:
    """Tests for BackupManager class"""

    def test_init_creates_directories(self, backup_manager, temp_backup_dir):
        """Should create backup directories on init"""
        assert (temp_backup_dir / "daily").exists()
        assert (temp_backup_dir / "weekly").exists()
        assert (temp_backup_dir / "monthly").exists()
        assert (temp_backup_dir / "yearly").exists()
        assert (temp_backup_dir / "temp").exists()

    def test_generate_backup_id(self, backup_manager):
        """Should generate unique backup IDs"""
        id1 = backup_manager._generate_backup_id()
        id2 = backup_manager._generate_backup_id()

        assert id1 != id2
        assert len(id1) > 0

    def test_calculate_checksum(self, backup_manager, temp_backup_dir):
        """Should calculate file checksum"""
        # Create test file
        test_file = temp_backup_dir / "test.txt"
        test_file.write_text("test content")

        checksum = backup_manager._calculate_checksum(test_file)

        assert checksum is not None
        assert len(checksum) == 64  # SHA256 hex length


# =============================================================================
# BACKUP RESULT TESTS
# =============================================================================

class TestBackupResult:
    """Tests for BackupResult dataclass"""

    def test_backup_result_creation(self):
        """Should create backup result with all fields"""
        from factory.core.backup_manager import BackupResult, BackupType, BackupStatus

        result = BackupResult(
            backup_id="BKP-001",
            backup_type=BackupType.FULL,
            status=BackupStatus.COMPLETED,
            started_at=datetime.now(),
            completed_at=datetime.now(),
            file_size=1024
        )

        assert result.backup_id == "BKP-001"
        assert result.backup_type == BackupType.FULL
        assert result.status == BackupStatus.COMPLETED
        assert result.file_size == 1024

    def test_backup_result_failed(self):
        """Should record failure details"""
        from factory.core.backup_manager import BackupResult, BackupType, BackupStatus

        result = BackupResult(
            backup_id="BKP-002",
            backup_type=BackupType.FULL,
            status=BackupStatus.FAILED,
            started_at=datetime.now(),
            error="Connection refused"
        )

        assert result.status == BackupStatus.FAILED
        assert result.error == "Connection refused"


# =============================================================================
# RETENTION POLICY TESTS
# =============================================================================

class TestRetentionPolicy:
    """Tests for backup retention policies"""

    def test_daily_retention(self, backup_config):
        """Daily retention should be 7 days by default"""
        assert backup_config.daily_retention == 7

    def test_weekly_retention(self, backup_config):
        """Weekly retention should be 28 days"""
        assert backup_config.weekly_retention == 28

    def test_monthly_retention(self, backup_config):
        """Monthly retention should be 365 days"""
        assert backup_config.monthly_retention == 365

    def test_retention_hierarchy(self, backup_config):
        """Retention should increase: daily < weekly < monthly"""
        assert backup_config.daily_retention < backup_config.weekly_retention
        assert backup_config.weekly_retention < backup_config.monthly_retention


# =============================================================================
# COMPRESSION TESTS
# =============================================================================

class TestCompression:
    """Tests for backup compression"""

    @pytest.mark.asyncio
    async def test_compress_file(self, backup_manager, temp_backup_dir):
        """Should compress file with gzip"""
        # Create test file
        test_file = temp_backup_dir / "test.txt"
        test_file.write_text("test content " * 100)

        original_size = test_file.stat().st_size

        compressed = await backup_manager._compress_file(test_file)

        assert compressed.exists()
        assert compressed.suffix == ".gz"
        assert compressed.stat().st_size < original_size


# =============================================================================
# SCHEDULE TESTS
# =============================================================================

class TestBackupSchedule:
    """Tests for backup scheduling"""

    def test_daily_schedule_format(self):
        """Daily schedule should be valid cron format"""
        from factory.core.backup_manager import BackupConfig

        config = BackupConfig()
        # Should be 5 fields: minute hour day month weekday
        parts = config.daily_schedule.split()
        assert len(parts) == 5

    def test_weekly_schedule_format(self):
        """Weekly schedule should be valid cron format"""
        from factory.core.backup_manager import BackupConfig

        config = BackupConfig()
        parts = config.weekly_schedule.split()
        assert len(parts) == 5
        assert parts[4] == "0"  # Sunday


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestBackupIntegration:
    """Integration tests for backup system"""

    def test_backup_history_tracking(self, backup_manager):
        """Should track backup history"""
        assert hasattr(backup_manager, '_backup_history')
        assert isinstance(backup_manager._backup_history, list)

    def test_config_s3_optional(self):
        """S3 configuration should be optional"""
        from factory.core.backup_manager import BackupConfig

        config = BackupConfig()
        assert config.s3_bucket is None

    def test_encryption_configurable(self):
        """Encryption should be configurable"""
        from factory.core.backup_manager import BackupConfig

        config_encrypted = BackupConfig(encryption_enabled=True)
        config_plain = BackupConfig(encryption_enabled=False)

        assert config_encrypted.encryption_enabled is True
        assert config_plain.encryption_enabled is False


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
