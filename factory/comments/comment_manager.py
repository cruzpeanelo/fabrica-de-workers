# -*- coding: utf-8 -*-
"""
Comment Manager - Plataforma E
==============================

Manage comments, threads, mentions and reactions.

Issue #440: Componente de Comentarios e Discussoes
"""

import re
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set
from threading import Lock


class ReactionType(str, Enum):
    """Types of reactions."""
    LIKE = "like"
    APPROVE = "approve"
    QUESTION = "question"
    CELEBRATE = "celebrate"
    HEART = "heart"
    THUMBS_DOWN = "thumbs_down"


@dataclass
class Reaction:
    """A reaction to a comment."""
    reaction_type: ReactionType
    user_id: str
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": self.reaction_type.value,
            "user_id": self.user_id,
            "created_at": self.created_at.isoformat(),
        }


@dataclass
class CommentEdit:
    """Record of a comment edit."""
    content: str
    edited_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "content": self.content,
            "edited_at": self.edited_at.isoformat(),
        }


@dataclass
class Comment:
    """A comment on a story."""
    story_id: str
    author_id: str
    content: str
    id: str = field(default_factory=lambda: str(uuid.uuid4())[:8])
    parent_id: Optional[str] = None  # For threaded replies
    mentions: List[str] = field(default_factory=list)
    reactions: List[Reaction] = field(default_factory=list)
    attachments: List[str] = field(default_factory=list)
    edit_history: List[CommentEdit] = field(default_factory=list)
    is_deleted: bool = False
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)

    def __post_init__(self):
        # Extract mentions from content
        self.mentions = self._extract_mentions(self.content)

    @staticmethod
    def _extract_mentions(content: str) -> List[str]:
        """Extract @mentions from content."""
        pattern = r'@([\w\-]+)'
        return list(set(re.findall(pattern, content)))

    @property
    def is_reply(self) -> bool:
        """Check if comment is a reply."""
        return self.parent_id is not None

    @property
    def is_edited(self) -> bool:
        """Check if comment was edited."""
        return len(self.edit_history) > 0

    @property
    def reaction_counts(self) -> Dict[str, int]:
        """Get reaction counts by type."""
        counts: Dict[str, int] = {}
        for reaction in self.reactions:
            key = reaction.reaction_type.value
            counts[key] = counts.get(key, 0) + 1
        return counts

    def to_dict(self, include_history: bool = False) -> Dict[str, Any]:
        """Convert to dictionary."""
        result = {
            "id": self.id,
            "story_id": self.story_id,
            "author_id": self.author_id,
            "content": self.content if not self.is_deleted else "[deleted]",
            "parent_id": self.parent_id,
            "mentions": self.mentions,
            "reactions": self.reaction_counts,
            "attachments": self.attachments,
            "is_deleted": self.is_deleted,
            "is_edited": self.is_edited,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
        }

        if include_history and self.edit_history:
            result["edit_history"] = [e.to_dict() for e in self.edit_history]

        return result


class CommentManager:
    """Manage comments with threads, mentions, and reactions."""

    MAX_ATTACHMENTS = 5
    MAX_ATTACHMENT_SIZE_MB = 5

    def __init__(self):
        self._comments: Dict[str, Comment] = {}
        self._by_story: Dict[str, List[str]] = {}  # story_id -> comment_ids
        self._lock = Lock()
        self._listeners: List[Callable[[str, Comment], None]] = []

    def create_comment(
        self,
        story_id: str,
        author_id: str,
        content: str,
        parent_id: Optional[str] = None,
        attachments: Optional[List[str]] = None,
    ) -> Comment:
        """Create a new comment."""
        with self._lock:
            # Validate parent exists
            if parent_id and parent_id not in self._comments:
                raise ValueError(f"Parent comment '{parent_id}' not found")

            # Validate attachments
            if attachments and len(attachments) > self.MAX_ATTACHMENTS:
                raise ValueError(f"Maximum {self.MAX_ATTACHMENTS} attachments allowed")

            comment = Comment(
                story_id=story_id,
                author_id=author_id,
                content=content,
                parent_id=parent_id,
                attachments=attachments or [],
            )

            self._comments[comment.id] = comment

            if story_id not in self._by_story:
                self._by_story[story_id] = []
            self._by_story[story_id].append(comment.id)

            # Notify listeners
            self._notify("create", comment)

            return comment

    def get_comment(self, comment_id: str) -> Optional[Comment]:
        """Get a comment by ID."""
        return self._comments.get(comment_id)

    def update_comment(
        self,
        comment_id: str,
        author_id: str,
        new_content: str,
    ) -> Optional[Comment]:
        """Update a comment (only by author)."""
        with self._lock:
            comment = self._comments.get(comment_id)
            if not comment:
                return None

            if comment.author_id != author_id:
                raise PermissionError("Only the author can edit this comment")

            if comment.is_deleted:
                raise ValueError("Cannot edit deleted comment")

            # Save to history
            comment.edit_history.append(CommentEdit(content=comment.content))

            # Update content
            comment.content = new_content
            comment.mentions = Comment._extract_mentions(new_content)
            comment.updated_at = datetime.now()

            self._notify("update", comment)

            return comment

    def delete_comment(
        self,
        comment_id: str,
        user_id: str,
        is_admin: bool = False,
    ) -> bool:
        """Soft delete a comment (author or admin only)."""
        with self._lock:
            comment = self._comments.get(comment_id)
            if not comment:
                return False

            if comment.author_id != user_id and not is_admin:
                raise PermissionError("Only the author or admin can delete")

            comment.is_deleted = True
            comment.updated_at = datetime.now()

            self._notify("delete", comment)

            return True

    def add_reaction(
        self,
        comment_id: str,
        user_id: str,
        reaction_type: ReactionType,
    ) -> bool:
        """Add a reaction to a comment."""
        with self._lock:
            comment = self._comments.get(comment_id)
            if not comment:
                return False

            # Check if user already reacted with this type
            for reaction in comment.reactions:
                if reaction.user_id == user_id and reaction.reaction_type == reaction_type:
                    return False  # Already reacted

            comment.reactions.append(Reaction(
                reaction_type=reaction_type,
                user_id=user_id,
            ))

            self._notify("reaction", comment)

            return True

    def remove_reaction(
        self,
        comment_id: str,
        user_id: str,
        reaction_type: ReactionType,
    ) -> bool:
        """Remove a reaction from a comment."""
        with self._lock:
            comment = self._comments.get(comment_id)
            if not comment:
                return False

            for i, reaction in enumerate(comment.reactions):
                if reaction.user_id == user_id and reaction.reaction_type == reaction_type:
                    comment.reactions.pop(i)
                    return True

            return False

    def get_story_comments(
        self,
        story_id: str,
        include_deleted: bool = False,
    ) -> List[Comment]:
        """Get all comments for a story."""
        comment_ids = self._by_story.get(story_id, [])
        comments = [
            self._comments[cid]
            for cid in comment_ids
            if cid in self._comments
        ]

        if not include_deleted:
            comments = [c for c in comments if not c.is_deleted]

        # Sort by created_at
        comments.sort(key=lambda c: c.created_at)

        return comments

    def get_threaded_comments(
        self,
        story_id: str,
    ) -> List[Dict[str, Any]]:
        """Get comments organized in threads."""
        comments = self.get_story_comments(story_id)

        # Separate root comments and replies
        root_comments = [c for c in comments if not c.is_reply]
        replies = [c for c in comments if c.is_reply]

        # Build thread structure
        result = []
        for root in root_comments:
            thread = root.to_dict()
            thread["replies"] = [
                r.to_dict()
                for r in replies
                if r.parent_id == root.id
            ]
            result.append(thread)

        return result

    def get_comment_count(self, story_id: str) -> int:
        """Get count of non-deleted comments for a story."""
        comments = self.get_story_comments(story_id, include_deleted=False)
        return len(comments)

    def get_user_comments(
        self,
        user_id: str,
        limit: int = 50,
    ) -> List[Comment]:
        """Get comments by a user."""
        comments = [
            c for c in self._comments.values()
            if c.author_id == user_id and not c.is_deleted
        ]
        comments.sort(key=lambda c: c.created_at, reverse=True)
        return comments[:limit]

    def get_mentions(self, user_id: str, limit: int = 50) -> List[Comment]:
        """Get comments that mention a user."""
        mentions = [
            c for c in self._comments.values()
            if user_id in c.mentions and not c.is_deleted
        ]
        mentions.sort(key=lambda c: c.created_at, reverse=True)
        return mentions[:limit]

    def search_comments(
        self,
        query: str,
        story_id: Optional[str] = None,
        limit: int = 50,
    ) -> List[Comment]:
        """Search comments by content."""
        query_lower = query.lower()
        results = []

        for comment in self._comments.values():
            if comment.is_deleted:
                continue
            if story_id and comment.story_id != story_id:
                continue
            if query_lower in comment.content.lower():
                results.append(comment)

        results.sort(key=lambda c: c.created_at, reverse=True)
        return results[:limit]

    # Event listeners

    def add_listener(self, callback: Callable[[str, Comment], None]) -> None:
        """Add event listener for comment events."""
        self._listeners.append(callback)

    def remove_listener(self, callback: Callable[[str, Comment], None]) -> None:
        """Remove event listener."""
        if callback in self._listeners:
            self._listeners.remove(callback)

    def _notify(self, event: str, comment: Comment) -> None:
        """Notify listeners of comment events."""
        for listener in self._listeners:
            try:
                listener(event, comment)
            except Exception:
                pass

    # Statistics

    def get_statistics(self, story_id: Optional[str] = None) -> Dict[str, Any]:
        """Get comment statistics."""
        if story_id:
            comments = self.get_story_comments(story_id, include_deleted=True)
        else:
            comments = list(self._comments.values())

        active = [c for c in comments if not c.is_deleted]
        deleted = [c for c in comments if c.is_deleted]
        with_reactions = [c for c in active if c.reactions]
        replies = [c for c in active if c.is_reply]

        return {
            "total_comments": len(active),
            "deleted_comments": len(deleted),
            "comments_with_reactions": len(with_reactions),
            "total_replies": len(replies),
            "unique_authors": len(set(c.author_id for c in active)),
        }


# Singleton instance
_manager: Optional[CommentManager] = None


def get_comment_manager() -> CommentManager:
    """Get global comment manager instance."""
    global _manager
    if _manager is None:
        _manager = CommentManager()
    return _manager
