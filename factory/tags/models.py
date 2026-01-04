# -*- coding: utf-8 -*-
"""
Tag Models - Plataforma E
=========================

Data models for tags and categories.

Issue #441: Sistema de Tags e Categorias
"""

import re
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional


def generate_id() -> str:
    """Generate a unique tag ID."""
    return str(uuid.uuid4())[:8]


@dataclass
class Tag:
    """A tag for organizing stories."""
    name: str
    color: str = "#3B82F6"  # Default blue
    description: str = ""
    id: str = field(default_factory=generate_id)
    usage_count: int = 0
    created_by: str = "system"
    created_at: datetime = field(default_factory=datetime.now)

    def __post_init__(self):
        # Normalize tag name
        self.name = self.normalize_name(self.name)
        # Validate color
        if not self.is_valid_color(self.color):
            self.color = "#3B82F6"

    @staticmethod
    def normalize_name(name: str) -> str:
        """Normalize tag name (lowercase, no special chars)."""
        # Remove leading/trailing whitespace
        name = name.strip()
        # Convert to lowercase
        name = name.lower()
        # Replace spaces with hyphens
        name = re.sub(r'\s+', '-', name)
        # Remove special characters except hyphens
        name = re.sub(r'[^a-z0-9\-]', '', name)
        return name

    @staticmethod
    def is_valid_color(color: str) -> bool:
        """Check if color is valid hex."""
        return bool(re.match(r'^#[0-9A-Fa-f]{6}$', color))

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "id": self.id,
            "name": self.name,
            "color": self.color,
            "description": self.description,
            "usage_count": self.usage_count,
            "created_by": self.created_by,
            "created_at": self.created_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Tag":
        """Create from dictionary."""
        created_at = data.get("created_at")
        if isinstance(created_at, str):
            created_at = datetime.fromisoformat(created_at)
        elif created_at is None:
            created_at = datetime.now()

        return cls(
            id=data.get("id", generate_id()),
            name=data["name"],
            color=data.get("color", "#3B82F6"),
            description=data.get("description", ""),
            usage_count=data.get("usage_count", 0),
            created_by=data.get("created_by", "system"),
            created_at=created_at,
        )


@dataclass
class StoryTag:
    """Association between a story and a tag."""
    story_id: str
    tag_id: str
    added_by: str = "system"
    added_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "story_id": self.story_id,
            "tag_id": self.tag_id,
            "added_by": self.added_by,
            "added_at": self.added_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "StoryTag":
        """Create from dictionary."""
        added_at = data.get("added_at")
        if isinstance(added_at, str):
            added_at = datetime.fromisoformat(added_at)
        elif added_at is None:
            added_at = datetime.now()

        return cls(
            story_id=data["story_id"],
            tag_id=data["tag_id"],
            added_by=data.get("added_by", "system"),
            added_at=added_at,
        )


@dataclass
class Category:
    """A category for organizing stories (hierarchical)."""
    name: str
    parent_id: Optional[str] = None
    color: str = "#6B7280"  # Default gray
    description: str = ""
    icon: str = ""
    id: str = field(default_factory=generate_id)
    order: int = 0
    created_by: str = "admin"
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "id": self.id,
            "name": self.name,
            "parent_id": self.parent_id,
            "color": self.color,
            "description": self.description,
            "icon": self.icon,
            "order": self.order,
            "created_by": self.created_by,
            "created_at": self.created_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Category":
        """Create from dictionary."""
        created_at = data.get("created_at")
        if isinstance(created_at, str):
            created_at = datetime.fromisoformat(created_at)
        elif created_at is None:
            created_at = datetime.now()

        return cls(
            id=data.get("id", generate_id()),
            name=data["name"],
            parent_id=data.get("parent_id"),
            color=data.get("color", "#6B7280"),
            description=data.get("description", ""),
            icon=data.get("icon", ""),
            order=data.get("order", 0),
            created_by=data.get("created_by", "admin"),
            created_at=created_at,
        )


# Predefined tag colors
TAG_COLORS = {
    "red": "#EF4444",
    "orange": "#F97316",
    "amber": "#F59E0B",
    "yellow": "#EAB308",
    "lime": "#84CC16",
    "green": "#22C55E",
    "emerald": "#10B981",
    "teal": "#14B8A6",
    "cyan": "#06B6D4",
    "sky": "#0EA5E9",
    "blue": "#3B82F6",
    "indigo": "#6366F1",
    "violet": "#8B5CF6",
    "purple": "#A855F7",
    "fuchsia": "#D946EF",
    "pink": "#EC4899",
    "rose": "#F43F5E",
    "gray": "#6B7280",
}


# Default categories
DEFAULT_CATEGORIES = [
    Category(id="cat-frontend", name="Frontend", color="#3B82F6", icon="code"),
    Category(id="cat-backend", name="Backend", color="#10B981", icon="server"),
    Category(id="cat-devops", name="DevOps", color="#F59E0B", icon="cloud"),
    Category(id="cat-security", name="Security", color="#EF4444", icon="shield"),
    Category(id="cat-design", name="Design", color="#A855F7", icon="palette"),
    Category(id="cat-docs", name="Documentation", color="#6B7280", icon="book"),
]
