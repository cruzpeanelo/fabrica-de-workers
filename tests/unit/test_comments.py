# -*- coding: utf-8 -*-
"""
Tests for Comments System
Plataforma E v6.5

Tests for Issue #440
"""

import pytest


class TestReactionType:
    """Tests for ReactionType enum"""

    def test_reaction_types(self):
        from factory.comments.comment_manager import ReactionType

        assert ReactionType.LIKE.value == "like"
        assert ReactionType.APPROVE.value == "approve"
        assert ReactionType.CELEBRATE.value == "celebrate"


class TestComment:
    """Tests for Comment model"""

    def test_comment_creation(self):
        from factory.comments.comment_manager import Comment

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="This is a test comment",
        )

        assert comment.story_id == "STR-001"
        assert comment.author_id == "user-1"
        assert comment.id is not None

    def test_mention_extraction(self):
        from factory.comments.comment_manager import Comment

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Hey @john and @mary, please review this",
        )

        assert "john" in comment.mentions
        assert "mary" in comment.mentions
        assert len(comment.mentions) == 2

    def test_is_reply(self):
        from factory.comments.comment_manager import Comment

        root = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Root comment",
        )

        reply = Comment(
            story_id="STR-001",
            author_id="user-2",
            content="Reply",
            parent_id=root.id,
        )

        assert root.is_reply is False
        assert reply.is_reply is True

    def test_is_edited(self):
        from factory.comments.comment_manager import Comment, CommentEdit

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Original",
        )

        assert comment.is_edited is False

        comment.edit_history.append(CommentEdit(content="Original"))
        assert comment.is_edited is True

    def test_reaction_counts(self):
        from factory.comments.comment_manager import Comment, Reaction, ReactionType

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        comment.reactions.append(Reaction(ReactionType.LIKE, "user-2"))
        comment.reactions.append(Reaction(ReactionType.LIKE, "user-3"))
        comment.reactions.append(Reaction(ReactionType.APPROVE, "user-4"))

        counts = comment.reaction_counts
        assert counts["like"] == 2
        assert counts["approve"] == 1

    def test_to_dict(self):
        from factory.comments.comment_manager import Comment

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test content",
        )

        d = comment.to_dict()
        assert d["story_id"] == "STR-001"
        assert d["content"] == "Test content"
        assert "created_at" in d

    def test_deleted_comment_to_dict(self):
        from factory.comments.comment_manager import Comment

        comment = Comment(
            story_id="STR-001",
            author_id="user-1",
            content="Secret content",
        )
        comment.is_deleted = True

        d = comment.to_dict()
        assert d["content"] == "[deleted]"


class TestCommentManager:
    """Tests for CommentManager"""

    @pytest.fixture
    def manager(self):
        from factory.comments.comment_manager import CommentManager
        return CommentManager()

    def test_create_comment(self, manager):
        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="New comment",
        )

        assert comment is not None
        assert comment.id in manager._comments

    def test_create_reply(self, manager):
        root = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Root",
        )

        reply = manager.create_comment(
            story_id="STR-001",
            author_id="user-2",
            content="Reply",
            parent_id=root.id,
        )

        assert reply.parent_id == root.id

    def test_create_reply_invalid_parent(self, manager):
        with pytest.raises(ValueError):
            manager.create_comment(
                story_id="STR-001",
                author_id="user-1",
                content="Reply",
                parent_id="nonexistent",
            )

    def test_get_comment(self, manager):
        created = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        retrieved = manager.get_comment(created.id)
        assert retrieved == created

    def test_update_comment(self, manager):
        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Original",
        )

        updated = manager.update_comment(
            comment.id,
            author_id="user-1",
            new_content="Updated content",
        )

        assert updated.content == "Updated content"
        assert updated.is_edited is True

    def test_update_comment_wrong_author(self, manager):
        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        with pytest.raises(PermissionError):
            manager.update_comment(
                comment.id,
                author_id="user-2",
                new_content="Hacked",
            )

    def test_delete_comment(self, manager):
        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="To delete",
        )

        result = manager.delete_comment(comment.id, "user-1")

        assert result is True
        assert comment.is_deleted is True

    def test_delete_comment_as_admin(self, manager):
        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        result = manager.delete_comment(comment.id, "admin", is_admin=True)
        assert result is True


class TestReactions:
    """Tests for reaction functionality"""

    @pytest.fixture
    def manager(self):
        from factory.comments.comment_manager import CommentManager
        return CommentManager()

    def test_add_reaction(self, manager):
        from factory.comments.comment_manager import ReactionType

        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        result = manager.add_reaction(
            comment.id,
            "user-2",
            ReactionType.LIKE,
        )

        assert result is True
        assert len(comment.reactions) == 1

    def test_add_duplicate_reaction(self, manager):
        from factory.comments.comment_manager import ReactionType

        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        manager.add_reaction(comment.id, "user-2", ReactionType.LIKE)
        result = manager.add_reaction(comment.id, "user-2", ReactionType.LIKE)

        assert result is False
        assert len(comment.reactions) == 1

    def test_remove_reaction(self, manager):
        from factory.comments.comment_manager import ReactionType

        comment = manager.create_comment(
            story_id="STR-001",
            author_id="user-1",
            content="Test",
        )

        manager.add_reaction(comment.id, "user-2", ReactionType.LIKE)
        result = manager.remove_reaction(comment.id, "user-2", ReactionType.LIKE)

        assert result is True
        assert len(comment.reactions) == 0


class TestQueries:
    """Tests for query methods"""

    @pytest.fixture
    def manager_with_data(self):
        from factory.comments.comment_manager import CommentManager

        manager = CommentManager()

        # Add comments to story 1
        c1 = manager.create_comment("STR-001", "user-1", "Comment 1")
        c2 = manager.create_comment("STR-001", "user-2", "Comment 2")
        manager.create_comment("STR-001", "user-1", "Reply", parent_id=c1.id)

        # Add comment to story 2
        manager.create_comment("STR-002", "user-1", "Other story")

        return manager

    def test_get_story_comments(self, manager_with_data):
        comments = manager_with_data.get_story_comments("STR-001")
        assert len(comments) == 3

    def test_get_threaded_comments(self, manager_with_data):
        threads = manager_with_data.get_threaded_comments("STR-001")

        assert len(threads) == 2  # 2 root comments
        # First comment has 1 reply
        root_with_reply = [t for t in threads if t["replies"]]
        assert len(root_with_reply) == 1
        assert len(root_with_reply[0]["replies"]) == 1

    def test_get_comment_count(self, manager_with_data):
        count = manager_with_data.get_comment_count("STR-001")
        assert count == 3

    def test_get_user_comments(self, manager_with_data):
        comments = manager_with_data.get_user_comments("user-1")
        assert len(comments) == 3  # 2 in STR-001, 1 in STR-002

    def test_get_mentions(self, manager_with_data):
        manager_with_data.create_comment(
            "STR-001",
            "user-1",
            "Hey @user-2, check this",
        )

        mentions = manager_with_data.get_mentions("user-2")
        assert len(mentions) == 1

    def test_search_comments(self, manager_with_data):
        results = manager_with_data.search_comments("Comment")
        assert len(results) >= 2


class TestEventListeners:
    """Tests for event listener functionality"""

    @pytest.fixture
    def manager(self):
        from factory.comments.comment_manager import CommentManager
        return CommentManager()

    def test_listener_on_create(self, manager):
        events = []

        def callback(event, comment):
            events.append((event, comment))

        manager.add_listener(callback)
        manager.create_comment("STR-001", "user-1", "Test")

        assert len(events) == 1
        assert events[0][0] == "create"

    def test_remove_listener(self, manager):
        events = []

        def callback(event, comment):
            events.append(event)

        manager.add_listener(callback)
        manager.remove_listener(callback)
        manager.create_comment("STR-001", "user-1", "Test")

        assert len(events) == 0


class TestStatistics:
    """Tests for statistics"""

    @pytest.fixture
    def manager_with_data(self):
        from factory.comments.comment_manager import CommentManager, ReactionType

        manager = CommentManager()

        c1 = manager.create_comment("STR-001", "user-1", "Comment 1")
        c2 = manager.create_comment("STR-001", "user-2", "Comment 2")
        manager.create_comment("STR-001", "user-1", "Reply", parent_id=c1.id)

        manager.add_reaction(c1.id, "user-2", ReactionType.LIKE)

        manager.delete_comment(c2.id, "user-2")

        return manager

    def test_get_statistics(self, manager_with_data):
        stats = manager_with_data.get_statistics("STR-001")

        assert stats["total_comments"] == 2  # Excluding deleted
        assert stats["deleted_comments"] == 1
        assert stats["comments_with_reactions"] == 1
        assert stats["total_replies"] == 1
        assert stats["unique_authors"] == 1  # user-2's comment deleted


class TestGlobalCommentManager:
    """Tests for global singleton"""

    def test_get_comment_manager_singleton(self):
        from factory.comments.comment_manager import get_comment_manager

        m1 = get_comment_manager()
        m2 = get_comment_manager()

        assert m1 is m2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
