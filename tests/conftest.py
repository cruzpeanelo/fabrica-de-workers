# -*- coding: utf-8 -*-
"""
Pytest Configuration and Fixtures
=================================

Shared fixtures for all tests in the Fabrica de Agentes test suite.
Atualizado para v4.0 - Arquitetura Worker-based.
"""

import os
import sys
import pytest
import uuid
from pathlib import Path
from datetime import datetime

# Add project root to path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Set test environment
os.environ["TESTING"] = "1"

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from factory.database.connection import Base
from factory.database.models import (
    Project, Story, Task, ActivityLog,
    User, Sprint, Worker, Job, StoryTask,
    ProjectStatus, TaskStatus, JobStatus, WorkerStatus
)


def generate_unique_id(prefix: str) -> str:
    """Generate a unique ID for test fixtures"""
    return f"{prefix}-{uuid.uuid4().hex[:8].upper()}"


# =============================================================================
# DATABASE FIXTURES
# =============================================================================

@pytest.fixture(scope="function")
def engine():
    """Create test database engine - in memory for isolation"""
    engine = create_engine(
        "sqlite:///:memory:",
        connect_args={"check_same_thread": False}
    )
    Base.metadata.create_all(bind=engine)
    yield engine
    Base.metadata.drop_all(bind=engine)
    engine.dispose()


@pytest.fixture(scope="function")
def db_session(engine):
    """Create a new database session for each test"""
    Session = sessionmaker(bind=engine)
    session = Session()
    yield session
    session.rollback()
    session.close()


# =============================================================================
# MODEL FIXTURES
# =============================================================================

@pytest.fixture
def sample_project(db_session):
    """Create a sample project with unique ID"""
    project = Project(
        project_id=generate_unique_id("PRJ"),
        name="Test Project",
        description="A test project for unit tests",
        project_type="web-app",
        status=ProjectStatus.PLANNING.value,
        progress=0.0,
        config={"framework": "fastapi"},
        tags=["test", "automated"]
    )
    db_session.add(project)
    db_session.commit()
    db_session.refresh(project)
    return project


@pytest.fixture
def sample_worker(db_session):
    """Create a sample worker with unique ID (v4.0 - substitui Agent)"""
    worker = Worker(
        worker_id=generate_unique_id("WRK"),
        status=WorkerStatus.IDLE.value,
        model="claude-sonnet-4-20250514",
        mcp_tools=["filesystem", "bash"],
        hostname="test-host",
        ip_address="127.0.0.1"
    )
    db_session.add(worker)
    db_session.commit()
    db_session.refresh(worker)
    return worker


@pytest.fixture
def sample_story(db_session, sample_project):
    """Create a sample story with unique ID"""
    story = Story(
        story_id=generate_unique_id("US"),
        project_id=sample_project.project_id,
        title="Test User Story",
        description="As a tester, I want to run tests",
        status="backlog",
        story_points=3,
        priority="medium",
        persona="tester",
        action="run automated tests",
        benefit="ensure code quality",
        acceptance_criteria=["Tests pass", "Coverage > 80%"],
        tags=["test"]
    )
    db_session.add(story)
    db_session.commit()
    db_session.refresh(story)
    return story


@pytest.fixture
def sample_job(db_session, sample_project):
    """Create a sample job with unique ID (v4.0)"""
    job = Job(
        job_id=generate_unique_id("JOB"),
        project_id=sample_project.project_id,
        description="Test job for unit tests",
        tech_stack="python, fastapi",
        features=["CRUD", "Auth"],
        status=JobStatus.PENDING.value,
        current_step="queued"
    )
    db_session.add(job)
    db_session.commit()
    db_session.refresh(job)
    return job


@pytest.fixture
def sample_task(db_session, sample_project):
    """Create a sample task with unique ID"""
    task = Task(
        task_id=generate_unique_id("TASK"),
        project_id=sample_project.project_id,
        title="Test Task",
        description="A task for testing",
        status=TaskStatus.BACKLOG.value,
        priority="medium"
    )
    db_session.add(task)
    db_session.commit()
    db_session.refresh(task)
    return task


@pytest.fixture
def sample_story_task(db_session, sample_story):
    """Create a sample story task"""
    story_task = StoryTask(
        task_id=generate_unique_id("STSK"),
        story_id=sample_story.story_id,
        title="Implement feature",
        description="Implement the feature",
        task_type="development",
        status="pending"
    )
    db_session.add(story_task)
    db_session.commit()
    db_session.refresh(story_task)
    return story_task


@pytest.fixture
def sample_sprint(db_session, sample_project):
    """Create a sample sprint"""
    sprint = Sprint(
        sprint_id=generate_unique_id("SPR"),
        project_id=sample_project.project_id,
        name="Sprint 1 - MVP",
        status="planned",
        goal="Complete MVP features",
        capacity=21
    )
    db_session.add(sprint)
    db_session.commit()
    db_session.refresh(sprint)
    return sprint


@pytest.fixture
def sample_user(db_session):
    """Create a sample user with unique username"""
    try:
        from passlib.context import CryptContext
        pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
        password_hash = pwd_context.hash("testpass123")
    except ImportError:
        # Fallback se passlib nao estiver instalado
        password_hash = "hashed_testpass123"

    user = User(
        username=f"testuser_{uuid.uuid4().hex[:6]}",
        password_hash=password_hash,
        email=f"test_{uuid.uuid4().hex[:6]}@example.com",
        role="ADMIN",
        active=True
    )
    db_session.add(user)
    db_session.commit()
    db_session.refresh(user)
    return user


# =============================================================================
# API CLIENT FIXTURES
# =============================================================================

@pytest.fixture
def api_client():
    """Create FastAPI test client"""
    try:
        from fastapi.testclient import TestClient
        from factory.dashboard.app import app
        return TestClient(app)
    except ImportError:
        pytest.skip("FastAPI test client not available")


@pytest.fixture
def authenticated_client(api_client, sample_user, db_session):
    """Create authenticated API client"""
    response = api_client.post("/api/auth/login", json={
        "username": sample_user.username,
        "password": "testpass123"
    })
    if response.status_code == 200:
        token = response.json().get("token")
        api_client.headers["Authorization"] = f"Bearer {token}"
    return api_client


# =============================================================================
# UTILITY FIXTURES
# =============================================================================

@pytest.fixture
def temp_directory(tmp_path):
    """Create a temporary directory for file operations"""
    test_dir = tmp_path / "test_factory"
    test_dir.mkdir()
    return test_dir


@pytest.fixture
def mock_claude_client_fixture(mocker):
    """Mock Claude API client"""
    try:
        mock = mocker.patch("factory.ai.claude_integration.ClaudeClient")
        mock_instance = mock.return_value
        mock_instance.is_available.return_value = False
        mock_instance.chat.return_value = mocker.Mock(
            success=False,
            content="",
            error="Mock - Claude not available"
        )
        return mock_instance
    except Exception:
        return None


# =============================================================================
# CLEANUP FIXTURES
# =============================================================================

@pytest.fixture(autouse=True)
def cleanup_test_files():
    """Cleanup any test files after each test"""
    yield
    # Cleanup test database files if they exist
    test_db_path = PROJECT_ROOT / "test_factory.db"
    if test_db_path.exists():
        try:
            test_db_path.unlink()
        except PermissionError:
            pass
