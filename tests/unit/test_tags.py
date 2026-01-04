# -*- coding: utf-8 -*-
"""
Tests for Tags System
Plataforma E v6.5

Tests for Issue #441
"""

import pytest


class TestTag:
    """Tests for Tag model"""

    def test_tag_creation(self):
        from factory.tags.models import Tag

        tag = Tag(name="Test Tag", color="#FF0000")

        assert tag.name == "test-tag"  # Normalized
        assert tag.color == "#FF0000"
        assert tag.id is not None

    def test_tag_normalize_name(self):
        from factory.tags.models import Tag

        assert Tag.normalize_name("My Tag") == "my-tag"
        assert Tag.normalize_name("  spaces  ") == "spaces"
        assert Tag.normalize_name("UPPERCASE") == "uppercase"
        assert Tag.normalize_name("special@#$chars") == "specialchars"

    def test_tag_color_validation(self):
        from factory.tags.models import Tag

        assert Tag.is_valid_color("#FF0000") is True
        assert Tag.is_valid_color("#000") is False
        assert Tag.is_valid_color("red") is False
        assert Tag.is_valid_color("#GGGGGG") is False

    def test_tag_invalid_color_defaults(self):
        from factory.tags.models import Tag

        tag = Tag(name="test", color="invalid")
        assert tag.color == "#3B82F6"  # Default blue

    def test_tag_to_dict(self):
        from factory.tags.models import Tag

        tag = Tag(name="test", color="#FF0000", description="A test tag")
        d = tag.to_dict()

        assert d["name"] == "test"
        assert d["color"] == "#FF0000"
        assert d["description"] == "A test tag"
        assert "id" in d
        assert "created_at" in d

    def test_tag_from_dict(self):
        from factory.tags.models import Tag

        data = {
            "name": "imported",
            "color": "#00FF00",
            "usage_count": 5,
        }
        tag = Tag.from_dict(data)

        assert tag.name == "imported"
        assert tag.color == "#00FF00"
        assert tag.usage_count == 5


class TestStoryTag:
    """Tests for StoryTag model"""

    def test_story_tag_creation(self):
        from factory.tags.models import StoryTag

        st = StoryTag(story_id="STR-001", tag_id="tag-1")

        assert st.story_id == "STR-001"
        assert st.tag_id == "tag-1"
        assert st.added_by == "system"

    def test_story_tag_to_dict(self):
        from factory.tags.models import StoryTag

        st = StoryTag(story_id="STR-001", tag_id="tag-1", added_by="user1")
        d = st.to_dict()

        assert d["story_id"] == "STR-001"
        assert d["tag_id"] == "tag-1"
        assert d["added_by"] == "user1"


class TestCategory:
    """Tests for Category model"""

    def test_category_creation(self):
        from factory.tags.models import Category

        cat = Category(name="Frontend", color="#3B82F6")

        assert cat.name == "Frontend"
        assert cat.color == "#3B82F6"
        assert cat.parent_id is None

    def test_category_hierarchy(self):
        from factory.tags.models import Category

        parent = Category(name="Frontend", id="parent-1")
        child = Category(name="Components", parent_id="parent-1")

        assert child.parent_id == parent.id

    def test_category_to_dict(self):
        from factory.tags.models import Category

        cat = Category(name="Backend", icon="server")
        d = cat.to_dict()

        assert d["name"] == "Backend"
        assert d["icon"] == "server"
        assert "id" in d


class TestTagManager:
    """Tests for TagManager"""

    @pytest.fixture
    def manager(self):
        from factory.tags.tag_manager import TagManager
        return TagManager()

    def test_create_tag(self, manager):
        tag = manager.create_tag("Feature", color="#FF0000")

        assert tag.name == "feature"
        assert tag.color == "#FF0000"
        assert manager.get_tag(tag.id) is not None

    def test_create_duplicate_tag(self, manager):
        manager.create_tag("test")

        with pytest.raises(ValueError):
            manager.create_tag("test")

    def test_get_tag_by_name(self, manager):
        manager.create_tag("bug-fix")

        tag = manager.get_tag_by_name("bug-fix")
        assert tag is not None
        assert tag.name == "bug-fix"

    def test_update_tag(self, manager):
        tag = manager.create_tag("old-name")

        updated = manager.update_tag(tag.id, name="new-name", color="#00FF00")

        assert updated.name == "new-name"
        assert updated.color == "#00FF00"

    def test_delete_tag(self, manager):
        tag = manager.create_tag("to-delete")
        tag_id = tag.id

        result = manager.delete_tag(tag_id)

        assert result is True
        assert manager.get_tag(tag_id) is None

    def test_list_tags(self, manager):
        manager.create_tag("tag-a")
        manager.create_tag("tag-b")
        manager.create_tag("tag-c")

        tags = manager.list_tags(sort_by="name")

        assert len(tags) == 3
        assert tags[0].name == "tag-a"

    def test_search_tags(self, manager):
        manager.create_tag("feature-auth")
        manager.create_tag("feature-api")
        manager.create_tag("bug-fix")

        results = manager.search_tags("feature")

        assert len(results) == 2
        assert all("feature" in t.name for t in results)


class TestStoryTagAssociation:
    """Tests for story-tag associations"""

    @pytest.fixture
    def manager(self):
        from factory.tags.tag_manager import TagManager
        return TagManager()

    def test_add_tag_to_story(self, manager):
        tag = manager.create_tag("test-tag")

        result = manager.add_tag_to_story("STR-001", tag.id)

        assert result is True
        assert tag.usage_count == 1

    def test_add_nonexistent_tag(self, manager):
        result = manager.add_tag_to_story("STR-001", "nonexistent")
        assert result is False

    def test_remove_tag_from_story(self, manager):
        tag = manager.create_tag("test-tag")
        manager.add_tag_to_story("STR-001", tag.id)

        result = manager.remove_tag_from_story("STR-001", tag.id)

        assert result is True
        assert tag.usage_count == 0

    def test_get_story_tags(self, manager):
        tag1 = manager.create_tag("tag-1")
        tag2 = manager.create_tag("tag-2")
        manager.add_tag_to_story("STR-001", tag1.id)
        manager.add_tag_to_story("STR-001", tag2.id)

        tags = manager.get_story_tags("STR-001")

        assert len(tags) == 2

    def test_max_tags_limit(self, manager):
        # Create 10 tags and add them
        for i in range(10):
            tag = manager.create_tag(f"tag-{i}")
            manager.add_tag_to_story("STR-001", tag.id)

        # 11th should fail
        tag11 = manager.create_tag("tag-11")
        with pytest.raises(ValueError):
            manager.add_tag_to_story("STR-001", tag11.id)

    def test_set_story_tags(self, manager):
        tag1 = manager.create_tag("tag-1")
        tag2 = manager.create_tag("tag-2")
        tag3 = manager.create_tag("tag-3")

        manager.add_tag_to_story("STR-001", tag1.id)

        # Replace with new set
        result = manager.set_story_tags("STR-001", [tag2.id, tag3.id])

        assert len(result) == 2
        assert tag1.usage_count == 0
        assert tag2.usage_count == 1

    def test_find_stories_by_tags_or(self, manager):
        tag1 = manager.create_tag("bug")
        tag2 = manager.create_tag("feature")

        manager.add_tag_to_story("STR-001", tag1.id)
        manager.add_tag_to_story("STR-002", tag2.id)
        manager.add_tag_to_story("STR-003", tag1.id)
        manager.add_tag_to_story("STR-003", tag2.id)

        # OR search
        results = manager.find_stories_by_tags([tag1.id], match_all=False)
        assert "STR-001" in results
        assert "STR-003" in results
        assert "STR-002" not in results

    def test_find_stories_by_tags_and(self, manager):
        tag1 = manager.create_tag("bug")
        tag2 = manager.create_tag("urgent")

        manager.add_tag_to_story("STR-001", tag1.id)
        manager.add_tag_to_story("STR-002", tag1.id)
        manager.add_tag_to_story("STR-002", tag2.id)

        # AND search
        results = manager.find_stories_by_tags([tag1.id, tag2.id], match_all=True)
        assert "STR-002" in results
        assert "STR-001" not in results

    def test_bulk_add_tags(self, manager):
        tag1 = manager.create_tag("bulk-tag-1")
        tag2 = manager.create_tag("bulk-tag-2")

        result = manager.bulk_add_tags(
            ["STR-001", "STR-002", "STR-003"],
            [tag1.id, tag2.id]
        )

        assert result["added"] == 6
        assert tag1.usage_count == 3


class TestTagCloud:
    """Tests for tag cloud functionality"""

    @pytest.fixture
    def manager(self):
        from factory.tags.tag_manager import TagManager
        return TagManager()

    def test_get_tag_cloud(self, manager):
        # Create tags with different usage
        tag1 = manager.create_tag("popular")
        tag2 = manager.create_tag("less-popular")

        # Add usage
        manager.add_tag_to_story("STR-001", tag1.id)
        manager.add_tag_to_story("STR-002", tag1.id)
        manager.add_tag_to_story("STR-003", tag1.id)
        manager.add_tag_to_story("STR-001", tag2.id)

        cloud = manager.get_tag_cloud(limit=10)

        assert len(cloud) == 2
        assert cloud[0]["name"] == "popular"
        assert cloud[0]["weight"] == 1.0  # Max weight

    def test_get_statistics(self, manager):
        tag1 = manager.create_tag("tag-1")
        tag2 = manager.create_tag("tag-2")
        manager.create_tag("unused-tag")

        manager.add_tag_to_story("STR-001", tag1.id)
        manager.add_tag_to_story("STR-001", tag2.id)
        manager.add_tag_to_story("STR-002", tag1.id)

        stats = manager.get_statistics()

        assert stats["total_tags"] == 3
        assert stats["total_assignments"] == 3
        assert stats["stories_with_tags"] == 2
        assert stats["unused_tags"] == 1


class TestCategories:
    """Tests for category management"""

    @pytest.fixture
    def manager(self):
        from factory.tags.tag_manager import TagManager
        return TagManager()

    def test_default_categories_exist(self, manager):
        categories = manager.list_categories()
        assert len(categories) > 0

    def test_create_category(self, manager):
        cat = manager.create_category("Testing", color="#FF0000")

        assert cat.name == "Testing"
        assert cat.color == "#FF0000"

    def test_create_child_category(self, manager):
        parent = manager.create_category("Parent")
        child = manager.create_category("Child", parent_id=parent.id)

        assert child.parent_id == parent.id

        children = manager.list_categories(parent_id=parent.id)
        assert len(children) == 1

    def test_get_category_tree(self, manager):
        # Clear default categories for clean test
        manager._categories.clear()

        parent = manager.create_category("Root")
        manager.create_category("Child1", parent_id=parent.id)
        manager.create_category("Child2", parent_id=parent.id)

        tree = manager.get_category_tree()

        assert len(tree) == 1
        assert tree[0]["name"] == "Root"
        assert len(tree[0]["children"]) == 2

    def test_delete_category(self, manager):
        cat = manager.create_category("ToDelete")
        cat_id = cat.id

        result = manager.delete_category(cat_id)

        assert result is True
        assert manager.get_category(cat_id) is None


class TestImportExport:
    """Tests for import/export functionality"""

    @pytest.fixture
    def manager(self):
        from factory.tags.tag_manager import TagManager
        return TagManager()

    def test_export_data(self, manager):
        tag = manager.create_tag("export-test")
        manager.add_tag_to_story("STR-001", tag.id)

        data = manager.export_data()

        assert "tags" in data
        assert "categories" in data
        assert "story_tags" in data
        assert len(data["tags"]) > 0

    def test_import_data(self, manager):
        # Clear existing
        manager._tags.clear()
        manager._categories.clear()
        manager._story_tags.clear()

        data = {
            "tags": [
                {"name": "imported-tag", "color": "#FF0000"},
            ],
            "categories": [
                {"name": "Imported Category"},
            ],
            "story_tags": {
                "STR-001": ["some-tag-id"],
            },
        }

        result = manager.import_data(data)

        assert result["tags"] == 1
        assert result["categories"] == 1


class TestGlobalManager:
    """Tests for global singleton"""

    def test_get_tag_manager_singleton(self):
        from factory.tags.tag_manager import get_tag_manager

        m1 = get_tag_manager()
        m2 = get_tag_manager()

        assert m1 is m2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
