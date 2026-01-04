# -*- coding: utf-8 -*-
"""
Tests for Event Sourcing
Plataforma E v6.5

Tests for Issue #448
"""

import pytest
from pathlib import Path
import tempfile
from datetime import datetime, timedelta


class TestEventType:
    """Tests for EventType enum"""

    def test_event_types(self):
        from factory.eventsourcing.event_store import EventType

        assert EventType.STORY_CREATED.value == "story.created"
        assert EventType.STORY_MOVED.value == "story.moved"
        assert EventType.SPRINT_COMPLETED.value == "sprint.completed"


class TestEvent:
    """Tests for Event dataclass"""

    def test_create_event(self):
        from factory.eventsourcing.event_store import Event

        event = Event.create(
            event_type="story.created",
            aggregate_type="story",
            aggregate_id="STR-001",
            data={"title": "Test Story"},
        )

        assert event.event_type == "story.created"
        assert event.aggregate_id == "STR-001"
        assert event.event_id is not None

    def test_event_to_dict(self):
        from factory.eventsourcing.event_store import Event

        event = Event.create(
            event_type="story.created",
            aggregate_type="story",
            aggregate_id="STR-001",
            data={"title": "Test"},
            metadata={"user_id": "user-1"},
        )

        d = event.to_dict()
        assert d["event_type"] == "story.created"
        assert d["data"]["title"] == "Test"
        assert d["metadata"]["user_id"] == "user-1"

    def test_event_from_dict(self):
        from factory.eventsourcing.event_store import Event

        d = {
            "event_id": "evt-001",
            "event_type": "story.created",
            "aggregate_type": "story",
            "aggregate_id": "STR-001",
            "data": {"title": "Test"},
            "metadata": {},
            "version": 1,
            "timestamp": "2024-01-01T12:00:00",
        }

        event = Event.from_dict(d)
        assert event.event_id == "evt-001"
        assert event.version == 1


class TestEventStore:
    """Tests for EventStore"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store(self, temp_db):
        from factory.eventsourcing.event_store import EventStore
        return EventStore(db_path=temp_db)

    def test_creation(self, store):
        assert store is not None
        assert store.db_path.exists()

    def test_append_event(self, store):
        from factory.eventsourcing.event_store import Event

        event = Event.create(
            event_type="story.created",
            aggregate_type="story",
            aggregate_id="STR-001",
            data={"title": "Test"},
        )

        seq = store.append(event)
        assert seq > 0

    def test_append_batch(self, store):
        from factory.eventsourcing.event_store import Event

        events = [
            Event.create("story.created", "story", "STR-001", {"title": "Story 1"}, version=1),
            Event.create("story.updated", "story", "STR-001", {"changes": {}}, version=2),
        ]

        seqs = store.append_batch(events)
        assert len(seqs) == 2

    def test_get_all_events(self, store):
        from factory.eventsourcing.event_store import Event

        for i in range(3):
            event = Event.create(
                event_type="story.updated",
                aggregate_type="story",
                aggregate_id="STR-001",
                data={"version": i},
                version=i + 1,
            )
            store.append(event)

        events = store.get_all_events("story", "STR-001")
        assert len(events) == 3

    def test_get_events_from_version(self, store):
        from factory.eventsourcing.event_store import Event

        for i in range(5):
            store.append(Event.create(
                "story.updated", "story", "STR-001",
                {"v": i}, version=i + 1,
            ))

        events = store.get_events("story", "STR-001", from_version=2)
        assert len(events) == 3  # Versions 3, 4, 5

    def test_get_events_by_type(self, store):
        from factory.eventsourcing.event_store import Event

        store.append(Event.create("story.created", "story", "STR-001", {}))
        store.append(Event.create("story.moved", "story", "STR-001", {}))
        store.append(Event.create("story.created", "story", "STR-002", {}))

        events = store.get_events_by_type("story.created")
        assert len(events) == 2

    def test_get_events_since(self, store):
        from factory.eventsourcing.event_store import Event

        now = datetime.utcnow()

        store.append(Event.create("story.created", "story", "STR-001", {}))
        store.append(Event.create("story.created", "story", "STR-002", {}))

        events = store.get_events_since(now - timedelta(seconds=1))
        assert len(events) == 2

    def test_get_latest_version(self, store):
        from factory.eventsourcing.event_store import Event

        store.append(Event.create("story.created", "story", "STR-001", {}, version=1))
        store.append(Event.create("story.updated", "story", "STR-001", {}, version=2))
        store.append(Event.create("story.updated", "story", "STR-001", {}, version=3))

        version = store.get_latest_version("story", "STR-001")
        assert version == 3

    def test_get_event_by_id(self, store):
        from factory.eventsourcing.event_store import Event

        event = Event.create("story.created", "story", "STR-001", {"title": "Test"})
        store.append(event)

        retrieved = store.get_event_by_id(event.event_id)
        assert retrieved is not None
        assert retrieved.data["title"] == "Test"


class TestEventHandlers:
    """Tests for event handlers/subscriptions"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store(self, temp_db):
        from factory.eventsourcing.event_store import EventStore
        return EventStore(db_path=temp_db)

    def test_subscribe_and_receive(self, store):
        from factory.eventsourcing.event_store import Event

        received = []

        def handler(event):
            received.append(event)

        store.subscribe("story.created", handler)
        store.append(Event.create("story.created", "story", "STR-001", {}))

        assert len(received) == 1

    def test_subscribe_all(self, store):
        from factory.eventsourcing.event_store import Event

        received = []

        store.subscribe_all(lambda e: received.append(e))
        store.append(Event.create("story.created", "story", "STR-001", {}))
        store.append(Event.create("story.moved", "story", "STR-001", {}))

        assert len(received) == 2

    def test_unsubscribe(self, store):
        from factory.eventsourcing.event_store import Event

        received = []

        def handler(event):
            received.append(event)

        store.subscribe("story.created", handler)
        store.unsubscribe("story.created", handler)
        store.append(Event.create("story.created", "story", "STR-001", {}))

        assert len(received) == 0


class TestReplay:
    """Tests for event replay"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store_with_events(self, temp_db):
        from factory.eventsourcing.event_store import EventStore, Event

        store = EventStore(db_path=temp_db)

        for i in range(10):
            store.append(Event.create(
                "story.created", "story", f"STR-{i:03d}",
                {"title": f"Story {i}"}, version=1,
            ))

        return store

    def test_replay_aggregate(self, store_with_events):
        replayed = []
        count = store_with_events.replay(
            "story", "STR-000",
            lambda e: replayed.append(e),
        )

        assert count == 1

    def test_replay_all(self, store_with_events):
        replayed = []
        count = store_with_events.replay_all(lambda e: replayed.append(e))

        assert count == 10


class TestDomainEvent:
    """Tests for DomainEvent"""

    def test_creation(self):
        from factory.eventsourcing.aggregate import DomainEvent

        event = DomainEvent(
            event_type="story.created",
            data={"title": "Test"},
        )

        assert event.event_type == "story.created"
        assert event.data["title"] == "Test"

    def test_to_event(self):
        from factory.eventsourcing.aggregate import DomainEvent

        domain_event = DomainEvent(
            event_type="story.created",
            data={"title": "Test"},
        )

        event = domain_event.to_event("story", "STR-001", 1)
        assert event.aggregate_type == "story"
        assert event.version == 1


class TestStoryAggregate:
    """Tests for StoryAggregate"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store(self, temp_db):
        from factory.eventsourcing.event_store import EventStore
        return EventStore(db_path=temp_db)

    def test_create_story(self, store):
        from factory.eventsourcing.aggregate import StoryAggregate

        story = StoryAggregate(event_store=store)
        story.create(title="Test Story", description="Description", story_points=5)

        assert story.title == "Test Story"
        assert story.status == "backlog"
        assert len(story.get_pending_events()) == 1

    def test_move_story(self, store):
        from factory.eventsourcing.aggregate import StoryAggregate

        story = StoryAggregate(event_store=store)
        story.create(title="Test Story")
        story.move("in_progress")

        assert story.status == "in_progress"

    def test_assign_story(self, store):
        from factory.eventsourcing.aggregate import StoryAggregate

        story = StoryAggregate(event_store=store)
        story.create(title="Test Story")
        story.assign("user-1")

        assert story.assigned_to == "user-1"

    def test_delete_story(self, store):
        from factory.eventsourcing.aggregate import StoryAggregate

        story = StoryAggregate(event_store=store)
        story.create(title="Test Story")
        story.delete()

        assert story.is_deleted is True

    def test_save_and_load(self, store):
        from factory.eventsourcing.aggregate import StoryAggregate

        # Create and save
        story = StoryAggregate(story_id="STR-001", event_store=store)
        story.create(title="Test Story", story_points=5)
        story.move("in_progress")
        story.save()

        # Load fresh
        loaded = StoryAggregate(story_id="STR-001", event_store=store)
        loaded.load()

        assert loaded.title == "Test Story"
        assert loaded.status == "in_progress"


class TestSnapshot:
    """Tests for Snapshot"""

    def test_snapshot_creation(self):
        from factory.eventsourcing.snapshot import Snapshot

        snapshot = Snapshot(
            aggregate_type="story",
            aggregate_id="STR-001",
            version=10,
            state={"title": "Test", "status": "done"},
        )

        assert snapshot.version == 10
        assert snapshot.state["title"] == "Test"

    def test_snapshot_to_dict(self):
        from factory.eventsourcing.snapshot import Snapshot

        snapshot = Snapshot(
            aggregate_type="story",
            aggregate_id="STR-001",
            version=5,
            state={"title": "Test"},
        )

        d = snapshot.to_dict()
        assert d["version"] == 5
        assert d["state"]["title"] == "Test"


class TestSnapshotStore:
    """Tests for SnapshotStore"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_snapshots.db"

    @pytest.fixture
    def store(self, temp_db):
        from factory.eventsourcing.snapshot import SnapshotStore
        return SnapshotStore(db_path=temp_db)

    def test_save_and_get_snapshot(self, store):
        from factory.eventsourcing.snapshot import Snapshot

        snapshot = Snapshot(
            aggregate_type="story",
            aggregate_id="STR-001",
            version=10,
            state={"title": "Test"},
        )

        store.save(snapshot)
        retrieved = store.get_latest("story", "STR-001")

        assert retrieved is not None
        assert retrieved.version == 10

    def test_get_at_version(self, store):
        from factory.eventsourcing.snapshot import Snapshot

        store.save(Snapshot("story", "STR-001", 5, {"v": 5}))
        store.save(Snapshot("story", "STR-001", 10, {"v": 10}))
        store.save(Snapshot("story", "STR-001", 15, {"v": 15}))

        snapshot = store.get_at_version("story", "STR-001", 12)
        assert snapshot.version == 10

    def test_should_snapshot(self, store):
        assert store.should_snapshot(10, 0) is True
        assert store.should_snapshot(5, 0) is False
        assert store.should_snapshot(25, 15) is True


class TestProjections:
    """Tests for Projections"""

    def test_story_summary_projection(self):
        from factory.eventsourcing.projections import StorySummaryProjection
        from factory.eventsourcing.event_store import Event

        projection = StorySummaryProjection()

        # Create story
        projection.handle(Event.create(
            "story.created", "story", "STR-001",
            {"title": "Test", "story_points": 5},
        ))

        state = projection.get_state()
        assert state["total_stories"] == 1
        assert state["by_status"]["backlog"] == 1
        assert state["total_points"] == 5

    def test_story_move_in_projection(self):
        from factory.eventsourcing.projections import StorySummaryProjection
        from factory.eventsourcing.event_store import Event

        projection = StorySummaryProjection()

        projection.handle(Event.create(
            "story.created", "story", "STR-001",
            {"title": "Test", "story_points": 5},
        ))

        projection.handle(Event.create(
            "story.moved", "story", "STR-001",
            {"old_status": "backlog", "new_status": "done"},
        ))

        state = projection.get_state()
        assert state["by_status"]["backlog"] == 0
        assert state["by_status"]["done"] == 1
        assert state["completed_points"] == 5

    def test_activity_log_projection(self):
        from factory.eventsourcing.projections import ActivityLogProjection
        from factory.eventsourcing.event_store import Event

        projection = ActivityLogProjection()

        projection.handle(Event.create(
            "story.created", "story", "STR-001",
            {"title": "Test Story"},
        ))

        entries = projection.get_entries()
        assert len(entries) == 1
        assert "Created story" in entries[0]["summary"]


class TestProjectionManager:
    """Tests for ProjectionManager"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store(self, temp_db):
        from factory.eventsourcing.event_store import EventStore
        return EventStore(db_path=temp_db)

    def test_register_projection(self, store):
        from factory.eventsourcing.projections import ProjectionManager, StorySummaryProjection

        manager = ProjectionManager(store)
        projection = StorySummaryProjection()
        manager.register(projection)

        assert "story_summary" in manager.list_projections()

    def test_get_projection(self, store):
        from factory.eventsourcing.projections import ProjectionManager, StorySummaryProjection

        manager = ProjectionManager(store)
        projection = StorySummaryProjection()
        manager.register(projection)

        retrieved = manager.get("story_summary")
        assert retrieved is projection

    def test_get_all_states(self, store):
        from factory.eventsourcing.projections import ProjectionManager, StorySummaryProjection, ActivityLogProjection

        manager = ProjectionManager(store)
        manager.register(StorySummaryProjection())
        manager.register(ActivityLogProjection())

        states = manager.get_all_states()
        assert "story_summary" in states
        assert "activity_log" in states


class TestEventStoreStatistics:
    """Tests for event store statistics"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_events.db"

    @pytest.fixture
    def store_with_events(self, temp_db):
        from factory.eventsourcing.event_store import EventStore, Event

        store = EventStore(db_path=temp_db)

        store.append(Event.create("story.created", "story", "STR-001", {}))
        store.append(Event.create("story.moved", "story", "STR-001", {}))
        store.append(Event.create("story.created", "story", "STR-002", {}))

        return store

    def test_get_statistics(self, store_with_events):
        stats = store_with_events.get_statistics()

        assert stats["total_events"] == 3
        assert stats["events_by_type"]["story.created"] == 2
        assert stats["events_by_type"]["story.moved"] == 1


class TestGlobalSingletons:
    """Tests for global singletons"""

    def test_get_event_store_singleton(self):
        from factory.eventsourcing.event_store import get_event_store

        s1 = get_event_store()
        s2 = get_event_store()

        assert s1 is s2

    def test_get_snapshot_store_singleton(self):
        from factory.eventsourcing.snapshot import get_snapshot_store

        s1 = get_snapshot_store()
        s2 = get_snapshot_store()

        assert s1 is s2

    def test_get_projection_manager_singleton(self):
        from factory.eventsourcing.projections import get_projection_manager

        m1 = get_projection_manager()
        m2 = get_projection_manager()

        assert m1 is m2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
