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
    ProjectStatus, TaskStatus, JobStatus, WorkerStatus,
    # Issue #316: Novos modelos
    Agent, AgentStatus, Skill, SkillType, Template, FactoryEvent
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
def sample_agent(db_session):
    """Create a sample agent (Issue #316)"""
    agent = Agent(
        agent_id=generate_unique_id("AG"),
        name="Developer Agent",
        role="Backend Developer",
        domain="technology",
        status=AgentStatus.STANDBY.value,
        capabilities=["python", "fastapi", "sql"],
        enabled=True
    )
    db_session.add(agent)
    db_session.commit()
    db_session.refresh(agent)
    return agent


@pytest.fixture
def sample_skill(db_session):
    """Create a sample skill (Issue #316)"""
    skill = Skill(
        skill_id=generate_unique_id("SKILL"),
        name="Code Generation",
        description="Generates code from specifications",
        skill_type=SkillType.CORE.value,
        category="development",
        enabled=True
    )
    db_session.add(skill)
    db_session.commit()
    db_session.refresh(skill)
    return skill


@pytest.fixture
def sample_template(db_session):
    """Create a sample template (Issue #316)"""
    template = Template(
        template_id=generate_unique_id("TPL"),
        name="FastAPI Starter",
        description="A FastAPI starter template",
        project_type="api-service",
        category="backend",
        stack={"backend": "fastapi", "database": "postgresql"},
        required_skills=["python", "api"],
        enabled=True
    )
    db_session.add(template)
    db_session.commit()
    db_session.refresh(template)
    return template


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


# =============================================================================
# WORKER FLOW TEST FIXTURES (Issue #29)
# =============================================================================

import tempfile
import shutil
import json
import asyncio
from unittest.mock import MagicMock, AsyncMock


def pytest_configure(config):
    """Configuracao inicial do pytest - registra markers customizados"""
    config.addinivalue_line("markers", "unit: marca testes unitarios")
    config.addinivalue_line("markers", "integration: marca testes de integracao")
    config.addinivalue_line("markers", "e2e: marca testes end-to-end")
    config.addinivalue_line("markers", "slow: marca testes lentos")


@pytest.fixture
def temp_project_dir():
    """Cria diretorio temporario para projetos de teste"""
    temp_dir = tempfile.mkdtemp(prefix="fabrica_test_")
    yield Path(temp_dir)
    # Cleanup apos o teste
    if Path(temp_dir).exists():
        shutil.rmtree(temp_dir)


@pytest.fixture
def minimal_loop_config(temp_project_dir):
    """Configuracao minima do loop (todas validacoes desabilitadas)"""
    from factory.core.autonomous_loop import LoopConfig
    return LoopConfig(
        max_attempts=1,
        lint_enabled=False,
        type_check_enabled=False,
        test_enabled=False,
        security_scan_enabled=False,
        auto_commit=False,
        project_base_dir=temp_project_dir
    )


@pytest.fixture
def standard_loop_config(temp_project_dir):
    """Configuracao padrao do loop para testes rapidos"""
    from factory.core.autonomous_loop import LoopConfig
    return LoopConfig(
        max_attempts=3,
        lint_enabled=False,
        type_check_enabled=False,
        test_enabled=False,
        security_scan_enabled=False,
        auto_commit=False,
        project_base_dir=temp_project_dir
    )


@pytest.fixture
def full_loop_config(temp_project_dir):
    """Configuracao completa do loop (todas validacoes habilitadas)"""
    from factory.core.autonomous_loop import LoopConfig
    return LoopConfig(
        max_attempts=2,
        lint_enabled=True,
        type_check_enabled=True,
        test_enabled=True,
        security_scan_enabled=True,
        auto_commit=True,
        project_base_dir=temp_project_dir
    )


@pytest.fixture
def sqlite_queue_config():
    """Configuracao de fila SQLite"""
    from factory.core.job_queue import QueueConfig, QueueBackend
    return QueueConfig(
        backend=QueueBackend.SQLITE,
        max_workers=1,
        job_timeout=60
    )


@pytest.fixture
def redis_queue_config():
    """Configuracao de fila Redis"""
    from factory.core.job_queue import QueueConfig, QueueBackend
    return QueueConfig(
        backend=QueueBackend.REDIS,
        redis_url=os.getenv("REDIS_URL", "redis://localhost:6379"),
        max_workers=3,
        job_timeout=600
    )


@pytest.fixture
def mock_claude_client():
    """Mock do cliente Claude API com resposta padrao para geracao de codigo"""
    mock_client = MagicMock()
    mock_response = MagicMock()
    mock_response.content = [MagicMock(text="""
---FILE: main.py---
from fastapi import FastAPI

app = FastAPI(title="Generated API")

@app.get("/")
async def root():
    return {"message": "Hello World", "status": "ok"}

@app.get("/health")
async def health():
    return {"status": "healthy"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
---END FILE---

---FILE: requirements.txt---
fastapi>=0.104.0
uvicorn>=0.24.0
---END FILE---

---FILE: tests/test_main.py---
from fastapi.testclient import TestClient
from main import app

client = TestClient(app)

def test_root():
    response = client.get("/")
    assert response.status_code == 200
    assert response.json()["status"] == "ok"

def test_health():
    response = client.get("/health")
    assert response.status_code == 200
---END FILE---
""")]
    mock_client.messages.create.return_value = mock_response
    return mock_client


@pytest.fixture
def mock_claude_client_error():
    """Mock do cliente Claude API que retorna erro"""
    mock_client = MagicMock()
    mock_client.messages.create.side_effect = Exception("Claude API unavailable")
    return mock_client


@pytest.fixture
def sample_story_data():
    """Dados de exemplo para uma User Story (para testes de worker flow)"""
    from factory.core.story_generator import DEFAULT_DOD
    return {
        "project_id": "TEST-PROJECT-001",
        "title": "Implementar endpoint de login",
        "persona": "usuario do sistema",
        "action": "fazer login com email e senha",
        "benefit": "acessar minha conta de forma segura",
        "acceptance_criteria": [
            "Usuario pode fazer login com email valido",
            "Senha deve ter minimo 8 caracteres",
            "Sistema retorna token JWT apos sucesso"
        ],
        "definition_of_done": DEFAULT_DOD,
        "story_points": 5,
        "complexity": "medium",
        "priority": "high"
    }


@pytest.fixture
def sample_job_data():
    """Dados de exemplo para um Job (para testes de worker flow)"""
    return {
        "description": "Criar API REST para gerenciamento de tarefas",
        "tech_stack": "python, fastapi, sqlite",
        "features": ["CRUD de tarefas", "Autenticacao JWT", "Documentacao OpenAPI"],
        "created_by": "test_user"
    }


@pytest.fixture
def autonomous_loop(standard_loop_config):
    """Instancia do AutonomousLoop para testes"""
    from factory.core.autonomous_loop import AutonomousLoop
    return AutonomousLoop(standard_loop_config)


@pytest.fixture
def autonomous_loop_with_claude(standard_loop_config, mock_claude_client):
    """AutonomousLoop configurado com mock do Claude"""
    from factory.core.autonomous_loop import AutonomousLoop

    loop = AutonomousLoop(standard_loop_config)
    loop._claude_client = mock_claude_client
    loop._model = "claude-sonnet-4-20250514"

    return loop


@pytest.fixture
def mock_worker():
    """Mock de um ClaudeWorker"""
    from factory.core.worker import ClaudeWorker

    worker = ClaudeWorker(worker_id="test-worker")
    worker._queue = AsyncMock()
    worker._client = MagicMock()
    worker._running = False

    return worker


@pytest.fixture
def mock_redis_queue():
    """Mock completo da RedisJobQueue"""
    from factory.core.job_queue import RedisJobQueue, QueueConfig, QueueBackend

    config = QueueConfig(backend=QueueBackend.REDIS)
    queue = RedisJobQueue(config)

    # Mock Redis client
    mock_redis = AsyncMock()
    mock_redis.ping = AsyncMock(return_value=True)
    mock_redis.hset = AsyncMock()
    mock_redis.hget = AsyncMock()
    mock_redis.hgetall = AsyncMock(return_value={})
    mock_redis.rpush = AsyncMock()
    mock_redis.blpop = AsyncMock()
    mock_redis.llen = AsyncMock(return_value=0)
    mock_redis.lrange = AsyncMock(return_value=[])
    mock_redis.publish = AsyncMock()
    mock_redis.hdel = AsyncMock()
    mock_redis.close = AsyncMock()

    queue._redis = mock_redis

    return queue


@pytest.fixture
def python_project_structure(temp_project_dir):
    """Cria estrutura de projeto Python para testes"""
    # main.py
    (temp_project_dir / "main.py").write_text('''
from fastapi import FastAPI

app = FastAPI(title="Test API")

@app.get("/")
async def root():
    return {"message": "Hello", "status": "ok"}

@app.get("/health")
async def health():
    return {"status": "healthy"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
''')

    # requirements.txt
    (temp_project_dir / "requirements.txt").write_text('''
fastapi>=0.104.0
uvicorn>=0.24.0
sqlalchemy>=2.0.0
''')

    # pyproject.toml
    (temp_project_dir / "pyproject.toml").write_text('''
[project]
name = "test-project"
version = "0.1.0"

[tool.pytest.ini_options]
testpaths = ["tests"]
''')

    # tests directory
    tests_dir = temp_project_dir / "tests"
    tests_dir.mkdir(exist_ok=True)
    (tests_dir / "__init__.py").write_text("")
    (tests_dir / "test_main.py").write_text('''
def test_example():
    assert True
''')

    return temp_project_dir


@pytest.fixture
def python_project_with_models(temp_project_dir):
    """Cria projeto Python com modelos SQLAlchemy para testes de AppGenerator"""
    # main.py
    (temp_project_dir / "main.py").write_text('''
from fastapi import FastAPI

app = FastAPI()

@app.get("/")
def root():
    return {"status": "ok"}

@app.get("/users")
def list_users():
    return []
''')

    # models.py
    (temp_project_dir / "models.py").write_text('''
from sqlalchemy import Column, Integer, String, ForeignKey
from sqlalchemy.orm import relationship
from database import Base

class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String(100))
    email = Column(String(200), unique=True)

    tasks = relationship("Task", back_populates="user")

class Task(Base):
    __tablename__ = "tasks"

    id = Column(Integer, primary_key=True)
    title = Column(String(200))
    user_id = Column(Integer, ForeignKey("users.id"))

    user = relationship("User", back_populates="tasks")
''')

    # requirements.txt
    (temp_project_dir / "requirements.txt").write_text('''
fastapi>=0.104.0
uvicorn>=0.24.0
sqlalchemy>=2.0.0
''')

    return temp_project_dir


@pytest.fixture(scope="session")
def event_loop():
    """Cria event loop para testes assincronos"""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()
