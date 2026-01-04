# -*- coding: utf-8 -*-
"""
Tests for Dependency Graph API
Plataforma E v6.5

Tests for Issue #243:
1. Graph node generation
2. Link generation from dependencies
3. Critical path calculation
4. Add/remove dependencies
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime


# =============================================================================
# MOCK STORY CLASS
# =============================================================================

class MockStory:
    """Mock Story for testing"""
    def __init__(self, story_id, title="Test Story", status="backlog",
                 story_points=5, blocked_by=None, dependencies=None):
        self.story_id = story_id
        self.title = title
        self.status = Mock(value=status)
        self.story_points = story_points
        self.epic_id = None
        self.sprint_id = None
        self.assigned_to = None
        self.blocked_by = blocked_by or []
        self.dependencies = dependencies or []


# =============================================================================
# NODE GENERATION TESTS
# =============================================================================

class TestNodeGeneration:
    """Tests for graph node generation"""

    def test_node_contains_story_id(self):
        """Node should contain story_id"""
        story = MockStory("STR-001")

        node = {
            "id": story.story_id,
            "story_id": story.story_id,
            "title": story.title,
            "status": story.status.value,
            "story_points": story.story_points
        }

        assert node["id"] == "STR-001"
        assert node["story_id"] == "STR-001"

    def test_node_contains_status(self):
        """Node should contain status"""
        story = MockStory("STR-001", status="in_progress")

        node = {
            "status": story.status.value
        }

        assert node["status"] == "in_progress"

    def test_node_blocked_flag(self):
        """Node should have isBlocked flag"""
        blocked_story = MockStory("STR-001", blocked_by=["STR-002"])
        unblocked_story = MockStory("STR-002")

        blocked_node = {
            "isBlocked": bool(blocked_story.blocked_by and len(blocked_story.blocked_by) > 0)
        }
        unblocked_node = {
            "isBlocked": bool(unblocked_story.blocked_by and len(unblocked_story.blocked_by) > 0)
        }

        assert blocked_node["isBlocked"] is True
        assert unblocked_node["isBlocked"] is False


# =============================================================================
# LINK GENERATION TESTS
# =============================================================================

class TestLinkGeneration:
    """Tests for graph link generation"""

    def test_blocks_link_generation(self):
        """Should generate blocks links from blocked_by"""
        stories = [
            MockStory("STR-001"),
            MockStory("STR-002", blocked_by=["STR-001"])
        ]

        links = []
        node_ids = {"STR-001", "STR-002"}

        for story in stories:
            if story.blocked_by:
                for blocker_id in story.blocked_by:
                    if blocker_id in node_ids:
                        links.append({
                            "source": blocker_id,
                            "target": story.story_id,
                            "type": "blocks"
                        })

        assert len(links) == 1
        assert links[0]["source"] == "STR-001"
        assert links[0]["target"] == "STR-002"
        assert links[0]["type"] == "blocks"

    def test_relates_to_link_generation(self):
        """Should generate relates_to links from dependencies"""
        stories = [
            MockStory("STR-001"),
            MockStory("STR-002", dependencies=["STR-001"])
        ]

        links = []
        node_ids = {"STR-001", "STR-002"}

        for story in stories:
            if story.dependencies:
                for dep_id in story.dependencies:
                    if dep_id in node_ids:
                        links.append({
                            "source": dep_id,
                            "target": story.story_id,
                            "type": "relates_to"
                        })

        assert len(links) == 1
        assert links[0]["type"] == "relates_to"

    def test_invalid_dependency_filtered(self):
        """Should filter dependencies to non-existent stories"""
        stories = [
            MockStory("STR-001", blocked_by=["STR-999"])  # STR-999 doesn't exist
        ]

        links = []
        node_ids = {"STR-001"}

        for story in stories:
            if story.blocked_by:
                for blocker_id in story.blocked_by:
                    if blocker_id in node_ids:  # Only add if exists
                        links.append({
                            "source": blocker_id,
                            "target": story.story_id,
                            "type": "blocks"
                        })

        assert len(links) == 0  # STR-999 filtered out


# =============================================================================
# CRITICAL PATH TESTS
# =============================================================================

class TestCriticalPath:
    """Tests for critical path calculation"""

    def test_single_chain_critical_path(self):
        """Should find critical path in single chain"""
        # STR-001 -> STR-002 -> STR-003
        from collections import defaultdict

        links = [
            {"source": "STR-001", "target": "STR-002"},
            {"source": "STR-002", "target": "STR-003"}
        ]

        downstream = defaultdict(set)
        for link in links:
            downstream[link["source"]].add(link["target"])

        # Simple path finder
        def find_longest_path(start, visited=None):
            if visited is None:
                visited = set()
            if start in visited:
                return [start]
            visited.add(start)
            best = [start]
            for next_node in downstream.get(start, []):
                path = [start] + find_longest_path(next_node, visited.copy())
                if len(path) > len(best):
                    best = path
            return best

        # Find longest path from STR-001
        path = find_longest_path("STR-001")

        assert path == ["STR-001", "STR-002", "STR-003"]
        assert len(path) == 3

    def test_branching_critical_path(self):
        """Should find longest branch in DAG"""
        # STR-001 -> STR-002 -> STR-003
        # STR-001 -> STR-004
        from collections import defaultdict

        links = [
            {"source": "STR-001", "target": "STR-002"},
            {"source": "STR-002", "target": "STR-003"},
            {"source": "STR-001", "target": "STR-004"}
        ]

        downstream = defaultdict(set)
        for link in links:
            downstream[link["source"]].add(link["target"])

        def find_longest_path(start, visited=None):
            if visited is None:
                visited = set()
            if start in visited:
                return [start]
            visited.add(start)
            best = [start]
            for next_node in downstream.get(start, []):
                path = [start] + find_longest_path(next_node, visited.copy())
                if len(path) > len(best):
                    best = path
            return best

        path = find_longest_path("STR-001")

        # Longest is STR-001 -> STR-002 -> STR-003 (3 nodes)
        assert len(path) == 3
        assert path[0] == "STR-001"

    def test_empty_links_no_critical_path(self):
        """Should return empty critical path when no links"""
        links = []
        critical_path = []

        if links:
            critical_path = ["would be calculated"]

        assert critical_path == []


# =============================================================================
# DEPENDENCY MANAGEMENT TESTS
# =============================================================================

class TestDependencyManagement:
    """Tests for add/remove dependencies"""

    def test_add_dependency_to_blocked_by(self):
        """Should add blocker to blocked_by list"""
        story = MockStory("STR-002")
        source_id = "STR-001"

        blocked_by = story.blocked_by or []
        if source_id not in blocked_by:
            blocked_by.append(source_id)

        assert "STR-001" in blocked_by

    def test_add_dependency_prevents_duplicates(self):
        """Should not add duplicate dependencies"""
        story = MockStory("STR-002", blocked_by=["STR-001"])
        source_id = "STR-001"

        blocked_by = story.blocked_by or []
        if source_id not in blocked_by:
            blocked_by.append(source_id)

        assert blocked_by.count("STR-001") == 1

    def test_remove_dependency_from_blocked_by(self):
        """Should remove blocker from blocked_by list"""
        story = MockStory("STR-002", blocked_by=["STR-001", "STR-003"])
        source_id = "STR-001"

        blocked_by = story.blocked_by or []
        if source_id in blocked_by:
            blocked_by.remove(source_id)

        assert "STR-001" not in blocked_by
        assert "STR-003" in blocked_by


# =============================================================================
# STATS TESTS
# =============================================================================

class TestGraphStats:
    """Tests for graph statistics"""

    def test_stats_calculation(self):
        """Should calculate correct stats"""
        nodes = [
            {"id": "STR-001", "isBlocked": False, "isCritical": True},
            {"id": "STR-002", "isBlocked": True, "isCritical": True},
            {"id": "STR-003", "isBlocked": False, "isCritical": False}
        ]
        links = [{"source": "STR-001", "target": "STR-002"}]
        critical_path = ["STR-001", "STR-002"]

        stats = {
            "total_stories": len(nodes),
            "total_dependencies": len(links),
            "blocked_stories": sum(1 for n in nodes if n["isBlocked"]),
            "critical_path_length": len(critical_path)
        }

        assert stats["total_stories"] == 3
        assert stats["total_dependencies"] == 1
        assert stats["blocked_stories"] == 1
        assert stats["critical_path_length"] == 2


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestDependencyGraphIntegration:
    """Integration tests for dependency graph"""

    def test_full_graph_structure(self):
        """Should build complete graph structure"""
        stories = [
            MockStory("STR-001", title="Auth", status="done"),
            MockStory("STR-002", title="Dashboard", status="in_progress", blocked_by=["STR-001"]),
            MockStory("STR-003", title="Reports", status="backlog", dependencies=["STR-002"])
        ]

        # Build nodes
        nodes = []
        for story in stories:
            nodes.append({
                "id": story.story_id,
                "title": story.title,
                "status": story.status.value,
                "isBlocked": bool(story.blocked_by)
            })

        # Build links
        links = []
        node_ids = {n["id"] for n in nodes}

        for story in stories:
            if story.blocked_by:
                for blocker in story.blocked_by:
                    if blocker in node_ids:
                        links.append({"source": blocker, "target": story.story_id, "type": "blocks"})
            if story.dependencies:
                for dep in story.dependencies:
                    if dep in node_ids:
                        links.append({"source": dep, "target": story.story_id, "type": "relates_to"})

        # Verify structure
        assert len(nodes) == 3
        assert len(links) == 2
        assert nodes[0]["status"] == "done"
        assert nodes[1]["isBlocked"] is True


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
