"""
Unit Tests for Repositories
============================

Tests for data access layer in factory.database.repositories
"""

import pytest
from datetime import datetime

from factory.database.repositories import (
    ProjectRepository, StoryRepository, AgentRepository,
    SkillRepository, TaskRepository, SprintRepository,
    ActivityLogRepository, FactoryEventRepository,
    TemplateRepository, UserRepository
)
from factory.database.models import (
    Project, Story, Agent, Skill, Task, Sprint,
    ActivityLog, FactoryEvent, Template, User,
    ProjectStatus, AgentStatus, TaskStatus
)


class TestProjectRepository:
    """Tests for ProjectRepository"""

    @pytest.mark.unit
    def test_create_project(self, db_session):
        """Test creating a project through repository"""
        repo = ProjectRepository(db_session)
        project = repo.create({
            "project_id": "PRJ-REPO-001",
            "name": "Repo Test Project",
            "description": "Testing repository",
            "project_type": "web-app",
            "status": ProjectStatus.PLANNING.value
        })

        assert project.project_id == "PRJ-REPO-001"
        assert project.name == "Repo Test Project"

    @pytest.mark.unit
    def test_get_by_id(self, db_session, sample_project):
        """Test fetching project by ID"""
        repo = ProjectRepository(db_session)
        project = repo.get_by_id(sample_project.project_id)

        assert project is not None
        assert project.project_id == sample_project.project_id

    @pytest.mark.unit
    def test_get_by_id_not_found(self, db_session):
        """Test fetching non-existent project"""
        repo = ProjectRepository(db_session)
        project = repo.get_by_id("NON-EXISTENT")

        assert project is None

    @pytest.mark.unit
    def test_get_all(self, db_session, sample_project):
        """Test listing all projects"""
        repo = ProjectRepository(db_session)
        projects = repo.get_all()

        assert len(projects) >= 1
        assert any(p.project_id == sample_project.project_id for p in projects)

    @pytest.mark.unit
    def test_get_all_with_status_filter(self, db_session, sample_project):
        """Test listing projects filtered by status"""
        repo = ProjectRepository(db_session)
        projects = repo.get_all(status=ProjectStatus.PLANNING.value)

        assert all(p.status == ProjectStatus.PLANNING.value for p in projects)

    @pytest.mark.unit
    def test_update_project(self, db_session, sample_project):
        """Test updating a project"""
        repo = ProjectRepository(db_session)
        updated = repo.update(sample_project.project_id, {
            "name": "Updated Project Name",
            "progress": 50.0
        })

        assert updated.name == "Updated Project Name"
        assert updated.progress == 50.0

    @pytest.mark.unit
    def test_delete_project(self, db_session):
        """Test deleting a project"""
        repo = ProjectRepository(db_session)
        project = repo.create({
            "project_id": "PRJ-DELETE",
            "name": "To Delete",
            "project_type": "test"
        })

        result = repo.delete("PRJ-DELETE")
        assert result is True

        deleted = repo.get_by_id("PRJ-DELETE")
        assert deleted is None

    @pytest.mark.unit
    def test_count_by_status(self, db_session, sample_project):
        """Test counting projects by status"""
        repo = ProjectRepository(db_session)
        counts = repo.count_by_status()

        assert isinstance(counts, dict)
        assert ProjectStatus.PLANNING.value in counts


class TestStoryRepository:
    """Tests for StoryRepository"""

    @pytest.mark.unit
    def test_create_story(self, db_session, sample_project):
        """Test creating a story"""
        repo = StoryRepository(db_session)
        story = repo.create({
            "story_id": "US-REPO-001",
            "project_id": sample_project.project_id,
            "title": "Repo Test Story",
            "status": "BACKLOG",
            "sprint": 1
        })

        assert story.story_id == "US-REPO-001"
        assert story.project_id == sample_project.project_id

    @pytest.mark.unit
    def test_get_by_project(self, db_session, sample_project, sample_story):
        """Test getting stories by project"""
        repo = StoryRepository(db_session)
        stories = repo.get_by_project(sample_project.project_id)

        assert len(stories) >= 1
        assert all(s.project_id == sample_project.project_id for s in stories)

    @pytest.mark.unit
    def test_get_by_sprint(self, db_session, sample_project, sample_story):
        """Test getting stories by sprint"""
        repo = StoryRepository(db_session)
        stories = repo.get_by_sprint(1, sample_project.project_id)

        assert all(s.sprint == 1 for s in stories)

    @pytest.mark.unit
    def test_update_status(self, db_session, sample_story):
        """Test updating story status"""
        repo = StoryRepository(db_session)
        updated = repo.update_status(sample_story.story_id, "IN_PROGRESS")

        assert updated.status == "IN_PROGRESS"


class TestAgentRepository:
    """Tests for AgentRepository"""

    @pytest.mark.unit
    def test_create_agent(self, db_session):
        """Test creating an agent"""
        repo = AgentRepository(db_session)
        agent = repo.create({
            "agent_id": "AG-REPO-001",
            "name": "Repo Test Agent",
            "role": "Tester",
            "domain": "testing",
            "status": AgentStatus.STANDBY.value,
            "enabled": True
        })

        assert agent.agent_id == "AG-REPO-001"
        assert agent.enabled is True

    @pytest.mark.unit
    def test_get_by_domain(self, db_session, sample_agent):
        """Test getting agents by domain"""
        repo = AgentRepository(db_session)
        agents = repo.get_by_domain(sample_agent.domain)

        assert all(a.domain == sample_agent.domain for a in agents)

    @pytest.mark.unit
    def test_get_by_status(self, db_session, sample_agent):
        """Test getting agents by status"""
        repo = AgentRepository(db_session)
        agents = repo.get_by_status(AgentStatus.STANDBY.value)

        assert all(a.status == AgentStatus.STANDBY.value for a in agents)

    @pytest.mark.unit
    def test_update_status(self, db_session, sample_agent):
        """Test updating agent status"""
        repo = AgentRepository(db_session)
        updated = repo.update_status(
            sample_agent.agent_id,
            AgentStatus.EXECUTING.value,
            task_id="TASK-001"
        )

        assert updated.status == AgentStatus.EXECUTING.value
        assert updated.current_task_id == "TASK-001"

    @pytest.mark.unit
    def test_increment_completed(self, db_session, sample_agent):
        """Test incrementing completed tasks counter"""
        repo = AgentRepository(db_session)
        initial_count = sample_agent.tasks_completed

        repo.increment_completed(sample_agent.agent_id)
        db_session.refresh(sample_agent)

        assert sample_agent.tasks_completed == initial_count + 1


class TestSkillRepository:
    """Tests for SkillRepository"""

    @pytest.mark.unit
    def test_create_skill(self, db_session):
        """Test creating a skill"""
        repo = SkillRepository(db_session)
        skill = repo.create({
            "skill_id": "SKILL-REPO-001",
            "name": "Repo Test Skill",
            "skill_type": "core",
            "category": "testing",
            "enabled": True
        })

        assert skill.skill_id == "SKILL-REPO-001"

    @pytest.mark.unit
    def test_get_by_type(self, db_session, sample_skill):
        """Test getting skills by type"""
        repo = SkillRepository(db_session)
        skills = repo.get_by_type("core")

        assert all(s.skill_type == "core" for s in skills)

    @pytest.mark.unit
    def test_get_by_category(self, db_session, sample_skill):
        """Test getting skills by category"""
        repo = SkillRepository(db_session)
        skills = repo.get_by_category(sample_skill.category)

        assert all(s.category == sample_skill.category for s in skills)


class TestTaskRepository:
    """Tests for TaskRepository"""

    @pytest.mark.unit
    def test_create_task(self, db_session, sample_project):
        """Test creating a task"""
        repo = TaskRepository(db_session)
        task = repo.create({
            "task_id": "TASK-REPO-001",
            "task_type": "development",
            "project_id": sample_project.project_id,
            "title": "Repo Test Task",
            "status": TaskStatus.PENDING.value
        })

        assert task.task_id == "TASK-REPO-001"

    @pytest.mark.unit
    def test_get_pending(self, db_session, sample_task):
        """Test getting pending tasks"""
        repo = TaskRepository(db_session)
        tasks = repo.get_pending()

        assert all(t.status == TaskStatus.PENDING.value for t in tasks)

    @pytest.mark.unit
    def test_get_by_project(self, db_session, sample_project, sample_task):
        """Test getting tasks by project"""
        repo = TaskRepository(db_session)
        tasks = repo.get_by_project(sample_project.project_id)

        assert all(t.project_id == sample_project.project_id for t in tasks)

    @pytest.mark.unit
    def test_update_status_to_in_progress(self, db_session, sample_task):
        """Test updating task status to in_progress sets started_at"""
        repo = TaskRepository(db_session)
        updated = repo.update_status(sample_task.task_id, TaskStatus.IN_PROGRESS.value)

        assert updated.status == TaskStatus.IN_PROGRESS.value
        assert updated.started_at is not None

    @pytest.mark.unit
    def test_update_status_to_completed(self, db_session, sample_task):
        """Test updating task status to completed sets completed_at"""
        repo = TaskRepository(db_session)
        updated = repo.update_status(
            sample_task.task_id,
            TaskStatus.COMPLETED.value,
            result="Task completed successfully"
        )

        assert updated.status == TaskStatus.COMPLETED.value
        assert updated.completed_at is not None
        assert updated.result == "Task completed successfully"


class TestSprintRepository:
    """Tests for SprintRepository"""

    @pytest.mark.unit
    def test_create_sprint(self, db_session, sample_project):
        """Test creating a sprint"""
        repo = SprintRepository(db_session)
        sprint = repo.create({
            "project_id": sample_project.project_id,
            "sprint_number": 2,
            "name": "Sprint 2",
            "status": "planned"
        })

        assert sprint.sprint_number == 2

    @pytest.mark.unit
    def test_get_by_project(self, db_session, sample_project, sample_sprint):
        """Test getting sprints by project"""
        repo = SprintRepository(db_session)
        sprints = repo.get_by_project(sample_project.project_id)

        assert all(s.project_id == sample_project.project_id for s in sprints)

    @pytest.mark.unit
    def test_get_or_create_existing(self, db_session, sample_project, sample_sprint):
        """Test get_or_create with existing sprint"""
        repo = SprintRepository(db_session)
        sprint = repo.get_or_create(sample_project.project_id, sample_sprint.sprint_number)

        assert sprint.id == sample_sprint.id

    @pytest.mark.unit
    def test_get_or_create_new(self, db_session, sample_project):
        """Test get_or_create with new sprint"""
        repo = SprintRepository(db_session)
        sprint = repo.get_or_create(sample_project.project_id, 99)

        assert sprint.sprint_number == 99
        assert sprint.status == "planned"

    @pytest.mark.unit
    def test_activate_sprint(self, db_session, sample_project, sample_sprint):
        """Test activating a sprint"""
        repo = SprintRepository(db_session)
        activated = repo.activate_sprint(
            sample_project.project_id,
            sample_sprint.sprint_number
        )

        assert activated.status == "active"
        assert activated.start_date is not None

    @pytest.mark.unit
    def test_complete_sprint(self, db_session, sample_sprint):
        """Test completing a sprint"""
        repo = SprintRepository(db_session)
        completed = repo.complete_sprint(sample_sprint.id)

        assert completed.status == "completed"
        assert completed.end_date is not None


class TestActivityLogRepository:
    """Tests for ActivityLogRepository"""

    @pytest.mark.unit
    def test_create_log(self, db_session):
        """Test creating activity log"""
        repo = ActivityLogRepository(db_session)
        log = repo.create({
            "source": "test",
            "event_type": "test_event",
            "message": "Test log message",
            "level": "INFO"
        })

        assert log.id is not None
        assert log.message == "Test log message"

    @pytest.mark.unit
    def test_get_recent(self, db_session):
        """Test getting recent logs"""
        repo = ActivityLogRepository(db_session)

        # Create some logs
        for i in range(5):
            repo.create({
                "source": "test",
                "event_type": "test",
                "message": f"Log {i}"
            })

        logs = repo.get_recent(limit=3)
        assert len(logs) <= 3

    @pytest.mark.unit
    def test_get_by_level(self, db_session):
        """Test getting logs by level"""
        repo = ActivityLogRepository(db_session)

        repo.create({
            "source": "test",
            "event_type": "error_test",
            "message": "Error occurred",
            "level": "ERROR"
        })

        logs = repo.get_by_level("ERROR")
        assert all(l.level == "ERROR" for l in logs)


class TestUserRepository:
    """Tests for UserRepository"""

    @pytest.mark.unit
    def test_get_by_username(self, db_session, sample_user):
        """Test getting user by username"""
        repo = UserRepository(db_session)
        user = repo.get_by_username(sample_user.username)

        assert user is not None
        assert user.username == sample_user.username

    @pytest.mark.unit
    def test_get_by_username_not_found(self, db_session):
        """Test getting non-existent user"""
        repo = UserRepository(db_session)
        user = repo.get_by_username("nonexistent")

        assert user is None

    @pytest.mark.unit
    def test_update_last_login(self, db_session, sample_user):
        """Test updating last login"""
        repo = UserRepository(db_session)
        before = sample_user.last_login

        repo.update_last_login(sample_user.username)
        db_session.refresh(sample_user)

        assert sample_user.last_login is not None
        if before:
            assert sample_user.last_login > before
