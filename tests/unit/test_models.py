"""
Unit Tests for Database Models
==============================

Tests for SQLAlchemy models in factory.database.models
"""

import pytest
from datetime import datetime

from factory.database.models import (
    Project, Story, Agent, Skill, Task, ActivityLog,
    FactoryEvent, Template, User, Sprint,
    ProjectStatus, AgentStatus, TaskStatus, SkillType
)


class TestProjectModel:
    """Tests for Project model"""

    @pytest.mark.unit
    def test_project_creation(self, db_session):
        """Test creating a new project"""
        project = Project(
            project_id="PRJ-001",
            name="Test Project",
            description="A test project",
            project_type="web-app",
            status=ProjectStatus.PLANNING.value
        )
        db_session.add(project)
        db_session.commit()

        assert project.id is not None
        assert project.project_id == "PRJ-001"
        assert project.status == "PLANNING"
        assert project.progress == 0.0

    @pytest.mark.unit
    def test_project_to_dict(self, sample_project):
        """Test project serialization to dictionary"""
        data = sample_project.to_dict()

        assert "project_id" in data
        assert "name" in data
        assert "status" in data
        assert "progress" in data
        assert "created_at" in data
        assert data["project_id"] == sample_project.project_id

    @pytest.mark.unit
    def test_project_status_enum(self):
        """Test ProjectStatus enum values"""
        assert ProjectStatus.PLANNING.value == "PLANNING"
        assert ProjectStatus.IN_PROGRESS.value == "IN_PROGRESS"
        assert ProjectStatus.COMPLETED.value == "COMPLETED"
        assert ProjectStatus.PAUSED.value == "PAUSED"
        assert ProjectStatus.ARCHIVED.value == "ARCHIVED"

    @pytest.mark.unit
    def test_project_relationships(self, db_session, sample_project, sample_story):
        """Test project relationships with stories"""
        db_session.refresh(sample_project)

        assert len(sample_project.stories) >= 1
        assert sample_project.stories[0].story_id == sample_story.story_id


class TestStoryModel:
    """Tests for Story model"""

    @pytest.mark.unit
    def test_story_creation(self, db_session, sample_project):
        """Test creating a new story"""
        story = Story(
            story_id="US-002",
            project_id=sample_project.project_id,
            title="Another Story",
            description="Test story",
            status="backlog",
            sprint_id="SPR-001",
            story_points=5
        )
        db_session.add(story)
        db_session.commit()

        assert story.id is not None
        assert story.story_id == "US-002"
        assert story.story_points == 5

    @pytest.mark.unit
    def test_story_to_dict(self, sample_story):
        """Test story serialization"""
        data = sample_story.to_dict()

        assert "story_id" in data
        assert "title" in data
        assert "narrative" in data
        assert "acceptance_criteria" in data
        assert data["story_id"] == sample_story.story_id

    @pytest.mark.unit
    def test_story_narrative(self, sample_story):
        """Test story narrative generation"""
        data = sample_story.to_dict()
        narrative = data["narrative"]

        # Narrative is a formatted string containing persona, action, benefit
        assert isinstance(narrative, str)
        assert "Como um" in narrative
        assert sample_story.persona in narrative
        assert sample_story.action in narrative
        assert sample_story.benefit in narrative

    @pytest.mark.unit
    def test_story_acceptance_criteria(self, sample_story):
        """Test story acceptance criteria"""
        assert isinstance(sample_story.acceptance_criteria, list)
        assert len(sample_story.acceptance_criteria) > 0


class TestAgentModel:
    """Tests for Agent model"""

    @pytest.mark.unit
    def test_agent_creation(self, db_session):
        """Test creating a new agent"""
        agent = Agent(
            agent_id="AG-001",
            name="Developer Agent",
            role="Backend Developer",
            domain="technology",
            status=AgentStatus.STANDBY.value,
            capabilities=["python", "fastapi"],
            enabled=True
        )
        db_session.add(agent)
        db_session.commit()

        assert agent.id is not None
        assert agent.agent_id == "AG-001"
        assert agent.enabled is True

    @pytest.mark.unit
    def test_agent_to_dict(self, sample_agent):
        """Test agent serialization"""
        data = sample_agent.to_dict()

        assert "agent_id" in data
        assert "name" in data
        assert "capabilities" in data
        assert "metrics" in data
        assert data["agent_id"] == sample_agent.agent_id

    @pytest.mark.unit
    def test_agent_status_enum(self):
        """Test AgentStatus enum values"""
        assert AgentStatus.STANDBY.value == "STANDBY"
        assert AgentStatus.READY.value == "READY"
        assert AgentStatus.EXECUTING.value == "EXECUTING"
        assert AgentStatus.ERROR.value == "ERROR"

    @pytest.mark.unit
    def test_agent_metrics(self, sample_agent):
        """Test agent metrics in serialization"""
        data = sample_agent.to_dict()
        metrics = data["metrics"]

        assert "tasks_completed" in metrics
        assert "tasks_failed" in metrics
        assert "total_execution_time" in metrics


class TestSkillModel:
    """Tests for Skill model"""

    @pytest.mark.unit
    def test_skill_creation(self, db_session):
        """Test creating a new skill"""
        skill = Skill(
            skill_id="SKILL-001",
            name="Code Generation",
            description="Generates code from specifications",
            skill_type=SkillType.CORE.value,
            category="development",
            enabled=True
        )
        db_session.add(skill)
        db_session.commit()

        assert skill.id is not None
        assert skill.skill_id == "SKILL-001"
        assert skill.skill_type == "core"

    @pytest.mark.unit
    def test_skill_to_dict(self, sample_skill):
        """Test skill serialization"""
        data = sample_skill.to_dict()

        assert "skill_id" in data
        assert "name" in data
        assert "skill_type" in data
        assert "enabled" in data

    @pytest.mark.unit
    def test_skill_type_enum(self):
        """Test SkillType enum values"""
        assert SkillType.CORE.value == "core"
        assert SkillType.MCP.value == "mcp"
        assert SkillType.VESSEL.value == "vessel"
        assert SkillType.CUSTOM.value == "custom"


class TestTaskModel:
    """Tests for Task model"""

    @pytest.mark.unit
    def test_task_creation(self, db_session, sample_project):
        """Test creating a new task"""
        task = Task(
            task_id="TASK-001",
            project_id=sample_project.project_id,
            title="Implement Feature",
            status=TaskStatus.BACKLOG.value,
            priority="high"
        )
        db_session.add(task)
        db_session.commit()

        assert task.id is not None
        assert task.task_id == "TASK-001"
        assert task.status == "backlog"

    @pytest.mark.unit
    def test_task_to_dict(self, sample_task):
        """Test task serialization"""
        data = sample_task.to_dict()

        assert "task_id" in data
        assert "status" in data
        assert "priority" in data

    @pytest.mark.unit
    def test_task_status_enum(self):
        """Test TaskStatus enum values"""
        assert TaskStatus.BACKLOG.value == "backlog"
        assert TaskStatus.TODO.value == "todo"
        assert TaskStatus.IN_DEVELOPMENT.value == "in_development"
        assert TaskStatus.DONE.value == "done"


class TestSprintModel:
    """Tests for Sprint model"""

    @pytest.mark.unit
    def test_sprint_creation(self, db_session, sample_project):
        """Test creating a new sprint"""
        sprint = Sprint(
            sprint_id="SPR-TEST-002",
            project_id=sample_project.project_id,
            name="Sprint 2",
            status="planned",
            goal="Implement core features"
        )
        db_session.add(sprint)
        db_session.commit()

        assert sprint.id is not None
        assert sprint.sprint_id == "SPR-TEST-002"

    @pytest.mark.unit
    def test_sprint_to_dict(self, sample_sprint):
        """Test sprint serialization"""
        data = sample_sprint.to_dict()

        assert "sprint_id" in data
        assert "name" in data
        assert "status" in data
        assert "goal" in data


class TestUserModel:
    """Tests for User model"""

    @pytest.mark.unit
    def test_user_creation(self, db_session):
        """Test creating a new user"""
        from passlib.context import CryptContext
        pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

        user = User(
            username="newuser",
            password_hash=pwd_context.hash("password123"),
            email="new@example.com",
            role="VIEWER",
            active=True
        )
        db_session.add(user)
        db_session.commit()

        assert user.id is not None
        assert user.username == "newuser"
        assert user.role == "VIEWER"

    @pytest.mark.unit
    def test_user_to_dict(self, sample_user):
        """Test user serialization (should not include password)"""
        data = sample_user.to_dict()

        assert "username" in data
        assert "email" in data
        assert "role" in data
        assert "password_hash" not in data
        assert "password" not in data


class TestActivityLogModel:
    """Tests for ActivityLog model"""

    @pytest.mark.unit
    def test_activity_log_creation(self, db_session, sample_agent):
        """Test creating activity log"""
        log = ActivityLog(
            source="test",
            source_id=sample_agent.agent_id,
            level="INFO",
            event_type="test_event",
            message="Test log message"
        )
        db_session.add(log)
        db_session.commit()

        assert log.id is not None
        assert log.level == "INFO"

    @pytest.mark.unit
    def test_activity_log_to_dict(self, db_session, sample_agent):
        """Test activity log serialization"""
        log = ActivityLog(
            source="test",
            event_type="test",
            message="Test"
        )
        db_session.add(log)
        db_session.commit()

        data = log.to_dict()
        assert "timestamp" in data
        assert "message" in data
        assert "level" in data


class TestTemplateModel:
    """Tests for Template model"""

    @pytest.mark.unit
    def test_template_creation(self, db_session):
        """Test creating a template"""
        template = Template(
            template_id="TPL-001",
            name="FastAPI Starter",
            description="A FastAPI starter template",
            project_type="api-service",
            category="backend",
            stack={"backend": "fastapi", "database": "postgresql"},
            enabled=True
        )
        db_session.add(template)
        db_session.commit()

        assert template.id is not None
        assert template.template_id == "TPL-001"

    @pytest.mark.unit
    def test_template_to_dict(self, db_session):
        """Test template serialization"""
        template = Template(
            template_id="TPL-002",
            name="Test Template",
            project_type="web-app"
        )
        db_session.add(template)
        db_session.commit()

        data = template.to_dict()
        assert "template_id" in data
        assert "stack" in data
        assert "required_skills" in data
