# -*- coding: utf-8 -*-
"""
Tag Manager - Plataforma E
==========================

Tag and category management logic.

Issue #441: Sistema de Tags e Categorias
"""

from collections import Counter
from typing import Any, Dict, List, Optional, Set
from threading import Lock

from .models import Tag, StoryTag, Category, DEFAULT_CATEGORIES


class TagManager:
    """Manages tags and categories for stories."""

    MAX_TAGS_PER_STORY = 10

    def __init__(self):
        self._tags: Dict[str, Tag] = {}
        self._story_tags: Dict[str, Set[str]] = {}  # story_id -> set of tag_ids
        self._categories: Dict[str, Category] = {}
        self._lock = Lock()

        # Initialize default categories
        for cat in DEFAULT_CATEGORIES:
            self._categories[cat.id] = cat

    # Tag CRUD

    def create_tag(self, name: str, color: str = "#3B82F6",
                  description: str = "", created_by: str = "system") -> Tag:
        """Create a new tag."""
        with self._lock:
            # Check for duplicate name
            normalized = Tag.normalize_name(name)
            for tag in self._tags.values():
                if tag.name == normalized:
                    raise ValueError(f"Tag '{normalized}' already exists")

            tag = Tag(
                name=name,
                color=color,
                description=description,
                created_by=created_by,
            )
            self._tags[tag.id] = tag
            return tag

    def get_tag(self, tag_id: str) -> Optional[Tag]:
        """Get a tag by ID."""
        return self._tags.get(tag_id)

    def get_tag_by_name(self, name: str) -> Optional[Tag]:
        """Get a tag by name."""
        normalized = Tag.normalize_name(name)
        for tag in self._tags.values():
            if tag.name == normalized:
                return tag
        return None

    def update_tag(self, tag_id: str, **kwargs) -> Optional[Tag]:
        """Update a tag."""
        with self._lock:
            tag = self._tags.get(tag_id)
            if not tag:
                return None

            if "name" in kwargs:
                tag.name = Tag.normalize_name(kwargs["name"])
            if "color" in kwargs and Tag.is_valid_color(kwargs["color"]):
                tag.color = kwargs["color"]
            if "description" in kwargs:
                tag.description = kwargs["description"]

            return tag

    def delete_tag(self, tag_id: str) -> bool:
        """Delete a tag and remove from all stories."""
        with self._lock:
            if tag_id not in self._tags:
                return False

            # Remove from all stories
            for story_id in list(self._story_tags.keys()):
                self._story_tags[story_id].discard(tag_id)

            del self._tags[tag_id]
            return True

    def list_tags(self, sort_by: str = "usage") -> List[Tag]:
        """List all tags, optionally sorted."""
        tags = list(self._tags.values())

        if sort_by == "usage":
            tags.sort(key=lambda t: t.usage_count, reverse=True)
        elif sort_by == "name":
            tags.sort(key=lambda t: t.name)
        elif sort_by == "created":
            tags.sort(key=lambda t: t.created_at, reverse=True)

        return tags

    def search_tags(self, query: str, limit: int = 10) -> List[Tag]:
        """Search tags by name prefix (for autocomplete)."""
        query = query.lower()
        matches = [
            tag for tag in self._tags.values()
            if tag.name.startswith(query) or query in tag.name
        ]
        # Sort by usage and limit
        matches.sort(key=lambda t: t.usage_count, reverse=True)
        return matches[:limit]

    # Story-Tag association

    def add_tag_to_story(self, story_id: str, tag_id: str,
                        added_by: str = "system") -> bool:
        """Add a tag to a story."""
        with self._lock:
            if tag_id not in self._tags:
                return False

            if story_id not in self._story_tags:
                self._story_tags[story_id] = set()

            # Check max tags limit
            if len(self._story_tags[story_id]) >= self.MAX_TAGS_PER_STORY:
                raise ValueError(f"Maximum {self.MAX_TAGS_PER_STORY} tags per story")

            if tag_id not in self._story_tags[story_id]:
                self._story_tags[story_id].add(tag_id)
                self._tags[tag_id].usage_count += 1

            return True

    def remove_tag_from_story(self, story_id: str, tag_id: str) -> bool:
        """Remove a tag from a story."""
        with self._lock:
            if story_id not in self._story_tags:
                return False

            if tag_id in self._story_tags[story_id]:
                self._story_tags[story_id].remove(tag_id)
                if tag_id in self._tags:
                    self._tags[tag_id].usage_count = max(0, self._tags[tag_id].usage_count - 1)
                return True

            return False

    def get_story_tags(self, story_id: str) -> List[Tag]:
        """Get all tags for a story."""
        tag_ids = self._story_tags.get(story_id, set())
        return [self._tags[tid] for tid in tag_ids if tid in self._tags]

    def set_story_tags(self, story_id: str, tag_ids: List[str],
                      added_by: str = "system") -> List[Tag]:
        """Set tags for a story (replaces existing)."""
        with self._lock:
            # Validate all tags exist
            valid_ids = [tid for tid in tag_ids if tid in self._tags][:self.MAX_TAGS_PER_STORY]

            # Decrement old usage
            old_ids = self._story_tags.get(story_id, set())
            for old_id in old_ids:
                if old_id in self._tags:
                    self._tags[old_id].usage_count = max(0, self._tags[old_id].usage_count - 1)

            # Set new tags
            self._story_tags[story_id] = set(valid_ids)

            # Increment new usage
            for new_id in valid_ids:
                self._tags[new_id].usage_count += 1

            return [self._tags[tid] for tid in valid_ids]

    def find_stories_by_tags(self, tag_ids: List[str],
                            match_all: bool = False) -> List[str]:
        """Find stories by tags.

        Args:
            tag_ids: List of tag IDs to search for
            match_all: If True, stories must have ALL tags (AND).
                      If False, stories can have ANY tag (OR).

        Returns:
            List of story IDs matching the criteria.
        """
        if not tag_ids:
            return []

        tag_set = set(tag_ids)
        results = []

        for story_id, story_tags in self._story_tags.items():
            if match_all:
                # AND: story must have all tags
                if tag_set.issubset(story_tags):
                    results.append(story_id)
            else:
                # OR: story must have at least one tag
                if story_tags.intersection(tag_set):
                    results.append(story_id)

        return results

    def bulk_add_tags(self, story_ids: List[str], tag_ids: List[str],
                     added_by: str = "system") -> Dict[str, int]:
        """Add tags to multiple stories at once."""
        with self._lock:
            added = 0
            skipped = 0

            for story_id in story_ids:
                if story_id not in self._story_tags:
                    self._story_tags[story_id] = set()

                for tag_id in tag_ids:
                    if tag_id not in self._tags:
                        skipped += 1
                        continue

                    if len(self._story_tags[story_id]) >= self.MAX_TAGS_PER_STORY:
                        skipped += 1
                        continue

                    if tag_id not in self._story_tags[story_id]:
                        self._story_tags[story_id].add(tag_id)
                        self._tags[tag_id].usage_count += 1
                        added += 1

            return {"added": added, "skipped": skipped}

    # Tag cloud / statistics

    def get_tag_cloud(self, limit: int = 20) -> List[Dict[str, Any]]:
        """Get tag cloud data (most used tags with weights)."""
        tags = self.list_tags(sort_by="usage")[:limit]
        if not tags:
            return []

        max_usage = max(t.usage_count for t in tags) or 1

        return [
            {
                "id": tag.id,
                "name": tag.name,
                "color": tag.color,
                "count": tag.usage_count,
                "weight": round(tag.usage_count / max_usage, 2),
            }
            for tag in tags
        ]

    def get_statistics(self) -> Dict[str, Any]:
        """Get tag system statistics."""
        total_tags = len(self._tags)
        total_assignments = sum(
            len(tags) for tags in self._story_tags.values()
        )
        stories_with_tags = len([
            s for s in self._story_tags.values() if s
        ])
        unused_tags = len([
            t for t in self._tags.values() if t.usage_count == 0
        ])

        return {
            "total_tags": total_tags,
            "total_assignments": total_assignments,
            "stories_with_tags": stories_with_tags,
            "unused_tags": unused_tags,
            "avg_tags_per_story": (
                round(total_assignments / stories_with_tags, 1)
                if stories_with_tags > 0 else 0
            ),
        }

    # Category management

    def create_category(self, name: str, parent_id: Optional[str] = None,
                       color: str = "#6B7280", description: str = "",
                       icon: str = "", created_by: str = "admin") -> Category:
        """Create a new category."""
        with self._lock:
            # Validate parent exists
            if parent_id and parent_id not in self._categories:
                raise ValueError(f"Parent category '{parent_id}' not found")

            category = Category(
                name=name,
                parent_id=parent_id,
                color=color,
                description=description,
                icon=icon,
                created_by=created_by,
            )
            self._categories[category.id] = category
            return category

    def get_category(self, category_id: str) -> Optional[Category]:
        """Get a category by ID."""
        return self._categories.get(category_id)

    def list_categories(self, parent_id: Optional[str] = None) -> List[Category]:
        """List categories, optionally filtered by parent."""
        if parent_id is None:
            # Return root categories
            return [
                c for c in self._categories.values()
                if c.parent_id is None
            ]
        return [
            c for c in self._categories.values()
            if c.parent_id == parent_id
        ]

    def get_category_tree(self) -> List[Dict[str, Any]]:
        """Get categories as a tree structure."""
        def build_tree(parent_id: Optional[str]) -> List[Dict[str, Any]]:
            children = self.list_categories(parent_id)
            return [
                {
                    **cat.to_dict(),
                    "children": build_tree(cat.id),
                }
                for cat in sorted(children, key=lambda c: c.order)
            ]

        return build_tree(None)

    def delete_category(self, category_id: str) -> bool:
        """Delete a category."""
        with self._lock:
            if category_id not in self._categories:
                return False

            # Move children to parent
            cat = self._categories[category_id]
            for child in self._categories.values():
                if child.parent_id == category_id:
                    child.parent_id = cat.parent_id

            del self._categories[category_id]
            return True

    # Import/Export

    def export_data(self) -> Dict[str, Any]:
        """Export all tag data."""
        return {
            "tags": [tag.to_dict() for tag in self._tags.values()],
            "categories": [cat.to_dict() for cat in self._categories.values()],
            "story_tags": {
                story_id: list(tag_ids)
                for story_id, tag_ids in self._story_tags.items()
            },
        }

    def import_data(self, data: Dict[str, Any]) -> Dict[str, int]:
        """Import tag data."""
        with self._lock:
            imported_tags = 0
            imported_categories = 0
            imported_associations = 0

            for tag_data in data.get("tags", []):
                tag = Tag.from_dict(tag_data)
                self._tags[tag.id] = tag
                imported_tags += 1

            for cat_data in data.get("categories", []):
                cat = Category.from_dict(cat_data)
                self._categories[cat.id] = cat
                imported_categories += 1

            for story_id, tag_ids in data.get("story_tags", {}).items():
                self._story_tags[story_id] = set(tag_ids)
                imported_associations += len(tag_ids)

            return {
                "tags": imported_tags,
                "categories": imported_categories,
                "associations": imported_associations,
            }


# Singleton instance
_manager: Optional[TagManager] = None


def get_tag_manager() -> TagManager:
    """Get global tag manager instance."""
    global _manager
    if _manager is None:
        _manager = TagManager()
    return _manager
