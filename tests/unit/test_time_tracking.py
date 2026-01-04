# -*- coding: utf-8 -*-
"""
Unit Tests for Time Tracking Module - Issue #224
Plataforma E v6.5

Tests for factory.dashboard.time_tracking module.
"""

import pytest
from fastapi.testclient import TestClient
from datetime import datetime
import time


class TestTimeTrackingEndpoints:
    """Tests for Time Tracking API endpoints"""

    @pytest.fixture
    def client(self):
        """Create test client"""
        from factory.dashboard.time_tracking import router
        from fastapi import FastAPI
        app = FastAPI()
        app.include_router(router)
        return TestClient(app)

    @pytest.fixture(autouse=True)
    def clean_storage(self):
        """Clean storage before each test"""
        from factory.dashboard import time_tracking
        time_tracking._time_entries.clear()
        time_tracking._active_timers.clear()
        yield
        time_tracking._time_entries.clear()
        time_tracking._active_timers.clear()

    @pytest.mark.unit
    def test_start_timer_success(self, client):
        """Should start a timer for a task"""
        response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "story_id": "STR-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })

        assert response.status_code == 200
        data = response.json()
        assert data["entry_id"].startswith("TE-")
        assert data["task_id"] == "TASK-001"
        assert data["is_running"] is True

    @pytest.mark.unit
    def test_start_timer_duplicate_blocked(self, client):
        """Should block starting a second timer for same user"""
        # Start first timer
        client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })

        # Try to start second timer
        response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-002",
            "user_id": "user-001",
            "user_name": "Test User"
        })

        assert response.status_code == 400
        assert "ja tem timer ativo" in response.json()["detail"]

    @pytest.mark.unit
    def test_stop_timer_success(self, client):
        """Should stop a running timer"""
        # Start timer
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]

        # Wait a bit
        time.sleep(0.1)

        # Stop timer
        response = client.post(f"/api/time-tracking/stop/{entry_id}", json={
            "description": "Work completed"
        })

        assert response.status_code == 200
        data = response.json()
        assert data["is_running"] is False
        assert data["description"] == "Work completed"
        assert data["duration_seconds"] >= 0

    @pytest.mark.unit
    def test_stop_nonexistent_timer(self, client):
        """Should return 404 for non-existent timer"""
        response = client.post("/api/time-tracking/stop/TE-NONEXIST", json={
            "description": "Test"
        })

        assert response.status_code == 404

    @pytest.mark.unit
    def test_get_active_timer(self, client):
        """Should return active timer for user"""
        # Start timer
        client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })

        # Get active timer
        response = client.get("/api/time-tracking/active/user-001")

        assert response.status_code == 200
        data = response.json()
        assert data["task_id"] == "TASK-001"
        assert data["is_running"] is True

    @pytest.mark.unit
    def test_get_active_timer_none(self, client):
        """Should return null when no active timer"""
        response = client.get("/api/time-tracking/active/user-001")

        assert response.status_code == 200
        assert response.json() is None

    @pytest.mark.unit
    def test_get_task_time_entries(self, client):
        """Should return all time entries for a task"""
        # Start and stop timer
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]
        client.post(f"/api/time-tracking/stop/{entry_id}", json={
            "description": "First entry"
        })

        # Get entries
        response = client.get("/api/time-tracking/task/TASK-001")

        assert response.status_code == 200
        data = response.json()
        assert data["total_seconds"] >= 0
        assert len(data["entries"]) == 1

    @pytest.mark.unit
    def test_get_story_time_entries(self, client):
        """Should return all time entries for a story"""
        # Start and stop timer
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "story_id": "STR-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]
        client.post(f"/api/time-tracking/stop/{entry_id}", json={})

        # Get entries
        response = client.get("/api/time-tracking/story/STR-001")

        assert response.status_code == 200
        data = response.json()
        assert len(data["entries"]) == 1

    @pytest.mark.unit
    def test_get_user_time_entries(self, client):
        """Should return all time entries for a user"""
        # Start and stop timer
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]
        client.post(f"/api/time-tracking/stop/{entry_id}", json={})

        # Get entries
        response = client.get("/api/time-tracking/user/user-001")

        assert response.status_code == 200
        data = response.json()
        assert len(data["entries"]) == 1

    @pytest.mark.unit
    def test_update_time_entry(self, client):
        """Should update a time entry"""
        # Create and stop entry
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User",
            "is_billable": True
        })
        entry_id = start_response.json()["entry_id"]
        client.post(f"/api/time-tracking/stop/{entry_id}", json={})

        # Update entry
        response = client.put(f"/api/time-tracking/{entry_id}", json={
            "description": "Updated description",
            "is_billable": False
        })

        assert response.status_code == 200
        data = response.json()
        assert data["description"] == "Updated description"
        assert data["is_billable"] is False

    @pytest.mark.unit
    def test_delete_time_entry(self, client):
        """Should delete a time entry"""
        # Create entry
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]

        # Delete entry
        response = client.delete(f"/api/time-tracking/{entry_id}")

        assert response.status_code == 200
        assert "removida" in response.json()["message"]

        # Verify it's gone
        task_response = client.get("/api/time-tracking/task/TASK-001")
        assert len(task_response.json()["entries"]) == 0

    @pytest.mark.unit
    def test_export_csv(self, client):
        """Should export time entries to CSV"""
        # Create and stop entry
        start_response = client.post("/api/time-tracking/start", json={
            "task_id": "TASK-001",
            "user_id": "user-001",
            "user_name": "Test User"
        })
        entry_id = start_response.json()["entry_id"]
        client.post(f"/api/time-tracking/stop/{entry_id}", json={
            "description": "Test work"
        })

        # Export CSV
        response = client.get("/api/time-tracking/export/csv")

        assert response.status_code == 200
        assert "text/csv" in response.headers["content-type"]
        assert "entry_id,task_id" in response.text


class TestTimeTrackingHelpers:
    """Tests for helper functions"""

    @pytest.mark.unit
    def test_format_duration(self):
        """Should format duration correctly"""
        from factory.dashboard.time_tracking import _format_duration

        assert _format_duration(0) == "00:00:00"
        assert _format_duration(61) == "00:01:01"
        assert _format_duration(3661) == "01:01:01"
        assert _format_duration(7200) == "02:00:00"

    @pytest.mark.unit
    def test_generate_entry_id(self):
        """Should generate unique entry IDs"""
        from factory.dashboard.time_tracking import _generate_entry_id

        id1 = _generate_entry_id()
        id2 = _generate_entry_id()

        assert id1.startswith("TE-")
        assert id2.startswith("TE-")
        assert id1 != id2

    @pytest.mark.unit
    def test_calculate_duration(self):
        """Should calculate duration in seconds"""
        from factory.dashboard.time_tracking import _calculate_duration
        from datetime import datetime, timedelta

        start = datetime(2024, 1, 1, 10, 0, 0)
        end = datetime(2024, 1, 1, 11, 30, 0)

        duration = _calculate_duration(start, end)
        assert duration == 5400  # 1.5 hours


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
