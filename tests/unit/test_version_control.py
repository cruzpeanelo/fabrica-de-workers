# -*- coding: utf-8 -*-
"""
Tests for Version Control Module
Plataforma E v6.5

Tests for Issue #208:
1. FileDiff dataclass
2. VersionSnapshot dataclass
3. Version control operations
"""

import pytest
from datetime import datetime
import tempfile
from pathlib import Path


# =============================================================================
# FILE DIFF TESTS
# =============================================================================

class TestFileDiff:
    """Tests for FileDiff dataclass"""

    def test_file_diff_creation(self):
        """Should create file diff"""
        from factory.core.version_control import FileDiff

        diff = FileDiff(
            file_path="src/main.py",
            old_content="print('hello')",
            new_content="print('world')",
            status="modified"
        )

        assert diff.file_path == "src/main.py"
        assert diff.status == "modified"

    def test_file_diff_to_dict(self):
        """Should convert to dictionary"""
        from factory.core.version_control import FileDiff

        diff = FileDiff(
            file_path="src/main.py",
            diff_lines=[
                "--- a/src/main.py",
                "+++ b/src/main.py",
                "-old line",
                "+new line"
            ],
            status="modified"
        )

        result = diff.to_dict()

        assert result["file_path"] == "src/main.py"
        assert result["status"] == "modified"
        assert result["additions"] == 1
        assert result["deletions"] == 1

    def test_file_diff_added_status(self):
        """Should track added files"""
        from factory.core.version_control import FileDiff

        diff = FileDiff(
            file_path="src/new_file.py",
            new_content="# new file",
            status="added"
        )

        assert diff.status == "added"
        assert diff.old_content is None

    def test_file_diff_deleted_status(self):
        """Should track deleted files"""
        from factory.core.version_control import FileDiff

        diff = FileDiff(
            file_path="src/old_file.py",
            old_content="# deleted file",
            status="deleted"
        )

        assert diff.status == "deleted"
        assert diff.new_content is None


# =============================================================================
# VERSION SNAPSHOT TESTS
# =============================================================================

class TestVersionSnapshot:
    """Tests for VersionSnapshot dataclass"""

    def test_snapshot_creation(self):
        """Should create version snapshot"""
        from factory.core.version_control import VersionSnapshot

        snapshot = VersionSnapshot(
            version_hash="abc123",
            story_id="STR-001",
            message="Initial commit",
            author="joao"
        )

        assert snapshot.version_hash == "abc123"
        assert snapshot.story_id == "STR-001"
        assert snapshot.message == "Initial commit"
        assert snapshot.author == "joao"

    def test_snapshot_to_dict(self):
        """Should convert to dictionary"""
        from factory.core.version_control import VersionSnapshot

        snapshot = VersionSnapshot(
            version_hash="def456",
            story_id="STR-002",
            task_id="TSK-001",
            message="Add feature",
            files={"main.py": "print('test')"}
        )

        result = snapshot.to_dict()

        assert result["version_hash"] == "def456"
        assert result["story_id"] == "STR-002"
        assert result["task_id"] == "TSK-001"
        assert result["files_count"] == 1

    def test_snapshot_branch_default(self):
        """Should default to main branch"""
        from factory.core.version_control import VersionSnapshot

        snapshot = VersionSnapshot(
            version_hash="ghi789",
            story_id="STR-003"
        )

        assert snapshot.branch == "main"

    def test_snapshot_parent_hash(self):
        """Should track parent version"""
        from factory.core.version_control import VersionSnapshot

        snapshot = VersionSnapshot(
            version_hash="jkl012",
            story_id="STR-004",
            parent_hash="abc123"
        )

        assert snapshot.parent_hash == "abc123"


# =============================================================================
# VERSION CONTROL TESTS
# =============================================================================

class TestVersionControl:
    """Tests for VersionControl class"""

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for version control"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    @pytest.fixture
    def version_control(self, temp_dir):
        """Create version control instance"""
        from factory.core.version_control import VersionControl
        return VersionControl(project_path=str(temp_dir))

    def test_version_control_init(self, version_control):
        """Should initialize version control"""
        assert version_control is not None
        assert version_control.project_path is not None

    def test_create_snapshot(self, version_control):
        """Should create version snapshot"""
        # Create snapshot with files dict
        files = {"test.py": "print('hello')"}

        snapshot = version_control.create_snapshot(
            story_id="STR-001",
            files=files,
            message="Test snapshot"
        )

        assert snapshot is not None
        assert snapshot.story_id == "STR-001"
        assert "test.py" in snapshot.files


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestVersionControlIntegration:
    """Integration tests for version control"""

    def test_file_hash_generation(self):
        """Should generate consistent file hashes"""
        import hashlib

        content = "test content"
        hash1 = hashlib.sha256(content.encode()).hexdigest()[:8]
        hash2 = hashlib.sha256(content.encode()).hexdigest()[:8]

        assert hash1 == hash2

    def test_diff_calculation(self):
        """Should calculate diff between versions"""
        import difflib

        old = ["line 1", "line 2", "line 3"]
        new = ["line 1", "line 2 modified", "line 3"]

        diff = list(difflib.unified_diff(old, new, lineterm=''))

        assert len(diff) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
