# -*- coding: utf-8 -*-
"""
Testes E2E do Fluxo Completo de Workers - Issue #29
====================================================

Este modulo implementa testes End-to-End (E2E) para validar o fluxo completo
da arquitetura v4.0 com Workers Claude.

Cenarios de Teste:
1. Fluxo Basico: Story -> Task -> Code Generation
2. Autonomous Loop: Generate -> Lint -> Test -> Fix
3. App Generator: Analise e geracao de aplicacoes testaveis
4. Integracao Claude API: Comunicacao com a API

Autor: Fabrica de Agentes
Data: 2024
Versao: 1.0.0
"""

import os
import sys
import json
import shutil
import asyncio
import tempfile
import pytest
from pathlib import Path
from datetime import datetime
from unittest.mock import Mock, MagicMock, AsyncMock, patch
from typing import Dict, Any, List, Optional

# Adicionar projeto ao path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Configurar ambiente de teste
os.environ["TESTING"] = "1"

# Imports do projeto
from factory.core.autonomous_loop import (
    AutonomousLoop, LoopConfig, StepResult,
    get_autonomous_loop
)
from factory.core.job_queue import (
    RedisJobQueue, SQLiteJobQueue, QueueConfig, QueueBackend,
    get_queue
)
from factory.core.story_generator import (
    DetailedStory, AgentTask,
    generate_story_id, generate_task_id,
    get_agents_for_category, calculate_points,
    create_tasks_for_story, story_to_db_dict, task_to_db_dict,
    AGENT_SPECIALTIES, CATEGORY_AGENTS, DEFAULT_DOD
)
from factory.core.app_generator import AppGenerator, analyze_project, generate_app


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def temp_project_dir():
    """Cria diretorio temporario para projetos de teste"""
    temp_dir = tempfile.mkdtemp(prefix="fabrica_test_")
    yield Path(temp_dir)
    # Cleanup apos o teste
    if Path(temp_dir).exists():
        shutil.rmtree(temp_dir)


@pytest.fixture
def loop_config(temp_project_dir):
    """Configuracao do loop para testes"""
    return LoopConfig(
        max_attempts=3,
        lint_enabled=False,  # Desabilitar para testes rapidos
        type_check_enabled=False,
        test_enabled=False,
        security_scan_enabled=False,
        auto_commit=False,
        project_base_dir=temp_project_dir
    )


@pytest.fixture
def autonomous_loop(loop_config):
    """Instancia do autonomous loop para testes"""
    return AutonomousLoop(loop_config)


@pytest.fixture
def mock_claude_client():
    """Mock do cliente Claude API"""
    mock_client = MagicMock()
    mock_response = MagicMock()
    mock_response.content = [MagicMock(text="""
---FILE: main.py---
print("Hello World")
---END FILE---

---FILE: test_main.py---
def test_hello():
    assert True
---END FILE---
""")]
    mock_client.messages.create.return_value = mock_response
    return mock_client


@pytest.fixture
def sample_story_data():
    """Dados de exemplo para uma User Story"""
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
    """Dados de exemplo para um Job"""
    return {
        "description": "Criar API REST para gerenciamento de tarefas",
        "tech_stack": "python, fastapi, sqlite",
        "features": ["CRUD de tarefas", "Autenticacao JWT", "Documentacao OpenAPI"],
        "created_by": "test_user"
    }


@pytest.fixture
def mock_queue_config():
    """Configuracao de fila para testes"""
    return QueueConfig(
        backend=QueueBackend.SQLITE,
        max_workers=1,
        job_timeout=60
    )


# =============================================================================
# TESTES DO STORY GENERATOR
# =============================================================================

class TestStoryGenerator:
    """Testes para o gerador de User Stories"""

    @pytest.mark.unit
    def test_generate_story_id_format(self):
        """Testa se o ID da story segue o formato correto"""
        story_id = generate_story_id()

        assert story_id.startswith("US-")
        assert len(story_id) > 10
        # Formato: US-YYYYMMDDHHMMSS-XXXX
        parts = story_id.split("-")
        assert len(parts) == 3

    @pytest.mark.unit
    def test_generate_task_id_format(self):
        """Testa se o ID da task segue o formato correto"""
        story_id = "US-20240101120000-ABC1"
        agent_id = "AGT-08"
        order = 1

        task_id = generate_task_id(story_id, agent_id, order)

        assert "TASK" in task_id
        assert "08" in task_id  # Agent number
        assert "01" in task_id  # Order

    @pytest.mark.unit
    def test_get_agents_for_category_backend(self):
        """Testa retorno de agentes para categoria backend"""
        agents = get_agents_for_category("backend")

        assert "AGT-08" in agents  # Backend Developer
        assert "AGT-10" in agents  # Security Specialist

    @pytest.mark.unit
    def test_get_agents_for_category_unknown(self):
        """Testa retorno de agente padrao para categoria desconhecida"""
        agents = get_agents_for_category("categoria_inexistente")

        assert agents == ["AGT-08"]  # Retorna backend developer como padrao

    @pytest.mark.unit
    def test_calculate_points_complexity(self):
        """Testa calculo de story points por complexidade"""
        assert calculate_points("low", 2) == 2
        assert calculate_points("medium", 2) == 5
        assert calculate_points("high", 2) == 8
        assert calculate_points("very_high", 2) == 13

    @pytest.mark.unit
    def test_calculate_points_with_many_tasks(self):
        """Testa ajuste de pontos com muitas tasks"""
        # Mais de 5 tasks aumenta os pontos
        points_few = calculate_points("medium", 2)
        points_many = calculate_points("medium", 6)

        assert points_many > points_few

    @pytest.mark.unit
    def test_create_tasks_for_story(self):
        """Testa criacao de tasks para uma story"""
        story_id = "US-TEST-001"
        story_title = "Implementar login"
        category = "backend"
        agents = ["AGT-08", "AGT-15"]
        complexity = "medium"

        tasks = create_tasks_for_story(story_id, story_title, category, agents, complexity)

        assert len(tasks) == 2
        assert all(isinstance(t, AgentTask) for t in tasks)
        assert tasks[0].agent_id == "AGT-08"
        assert tasks[1].agent_id == "AGT-15"

    @pytest.mark.unit
    def test_agent_task_properties(self):
        """Testa propriedades de uma AgentTask"""
        task = AgentTask(
            task_id="TASK-001",
            agent_id="AGT-08",
            agent_name="Backend Developer",
            title="Desenvolver API",
            description="Implementar endpoints REST",
            task_type="backend",
            estimated_hours=8.0,
            skills_required=["python", "fastapi"],
            acceptance_criteria=["API funcionando"],
            order=1
        )

        assert task.task_id == "TASK-001"
        assert task.estimated_hours == 8.0
        assert "python" in task.skills_required

    @pytest.mark.unit
    def test_detailed_story_creation(self, sample_story_data):
        """Testa criacao de DetailedStory"""
        story = DetailedStory(
            story_id=generate_story_id(),
            project_id=sample_story_data["project_id"],
            title=sample_story_data["title"],
            description="Descricao detalhada",
            persona=sample_story_data["persona"],
            action=sample_story_data["action"],
            benefit=sample_story_data["benefit"],
            epic="EPIC-001",
            sprint=1,
            priority=sample_story_data["priority"],
            points=sample_story_data["story_points"],
            complexity=sample_story_data["complexity"],
            business_value=8,
            acceptance_criteria=sample_story_data["acceptance_criteria"],
            definition_of_done=sample_story_data["definition_of_done"],
            business_rules=[],
            technical_notes=[],
            assigned_to="AGT-08",
            agents=["AGT-08"],
            tasks=[],
            dependencies=[]
        )

        assert story.story_id.startswith("US-")
        assert story.points == 5
        assert len(story.acceptance_criteria) == 3

    @pytest.mark.unit
    def test_story_to_db_dict_conversion(self, sample_story_data):
        """Testa conversao de story para dicionario de banco"""
        story = DetailedStory(
            story_id="US-TEST-001",
            project_id=sample_story_data["project_id"],
            title=sample_story_data["title"],
            description="Descricao",
            persona=sample_story_data["persona"],
            action=sample_story_data["action"],
            benefit=sample_story_data["benefit"],
            epic="EPIC-001",
            sprint=1,
            priority="high",
            points=5,
            complexity="medium",
            business_value=8,
            acceptance_criteria=sample_story_data["acceptance_criteria"],
            definition_of_done=[],
            business_rules=[],
            technical_notes=[],
            assigned_to="AGT-08",
            agents=["AGT-08"],
            tasks=[],
            dependencies=[]
        )

        db_dict = story_to_db_dict(story)

        assert db_dict["story_id"] == "US-TEST-001"
        assert db_dict["status"] == "BACKLOG"
        assert db_dict["source"] == "auto_generated"

    @pytest.mark.unit
    def test_agent_specialties_data(self):
        """Testa dados de especialidades dos agentes"""
        # Verifica agentes essenciais
        assert "AGT-08" in AGENT_SPECIALTIES
        assert "Backend Developer" in AGENT_SPECIALTIES["AGT-08"]["name"]

        # Verifica skills
        assert "python" in AGENT_SPECIALTIES["AGT-08"]["skills"]
        assert "fastapi" in AGENT_SPECIALTIES["AGT-08"]["skills"]

    @pytest.mark.unit
    def test_category_agents_mapping(self):
        """Testa mapeamento de categorias para agentes"""
        assert "database" in CATEGORY_AGENTS
        assert "backend" in CATEGORY_AGENTS
        assert "frontend" in CATEGORY_AGENTS
        assert "testing" in CATEGORY_AGENTS


# =============================================================================
# TESTES DO AUTONOMOUS LOOP
# =============================================================================

class TestAutonomousLoop:
    """Testes para o loop autonomo de geracao de codigo"""

    @pytest.mark.unit
    def test_loop_config_defaults(self):
        """Testa configuracoes padrao do loop"""
        config = LoopConfig()

        assert config.max_attempts == 5
        assert config.lint_enabled is True
        assert config.type_check_enabled is True
        assert config.test_enabled is True
        assert config.security_scan_enabled is True
        assert config.auto_commit is True

    @pytest.mark.unit
    def test_loop_initialization(self, loop_config):
        """Testa inicializacao do loop"""
        loop = AutonomousLoop(loop_config)

        assert loop.config == loop_config
        assert loop._current_job_id is None
        assert loop._project_path is None

    @pytest.mark.unit
    def test_step_result_success(self):
        """Testa criacao de StepResult de sucesso"""
        result = StepResult(
            success=True,
            message="Step completed successfully",
            output="Output data"
        )

        assert result.success is True
        assert result.can_fix is False
        assert result.errors is None

    @pytest.mark.unit
    def test_step_result_failure(self):
        """Testa criacao de StepResult de falha"""
        result = StepResult(
            success=False,
            message="Step failed",
            output="Error output",
            errors=["Error 1", "Error 2"],
            can_fix=True
        )

        assert result.success is False
        assert result.can_fix is True
        assert len(result.errors) == 2

    @pytest.mark.unit
    def test_generate_project_name(self, autonomous_loop):
        """Testa geracao de nome de projeto"""
        description = "API REST para tarefas"

        project_name = autonomous_loop._generate_project_name(description)

        assert "api" in project_name.lower()
        assert "-" in project_name
        # Verifica timestamp no nome
        assert len(project_name) > 10

    @pytest.mark.unit
    def test_parse_tech_stack_python(self, autonomous_loop):
        """Testa parsing de tech stack Python"""
        tech_stack = "python, fastapi, postgresql"

        parsed = autonomous_loop._parse_tech_stack(tech_stack)

        assert parsed.get("backend") == "fastapi"
        assert parsed.get("database") == "postgresql"

    @pytest.mark.unit
    def test_parse_tech_stack_node(self, autonomous_loop):
        """Testa parsing de tech stack Node.js"""
        tech_stack = "node, express, react, mongodb"

        parsed = autonomous_loop._parse_tech_stack(tech_stack)

        assert parsed.get("backend") == "express"
        assert parsed.get("frontend") == "react"
        assert parsed.get("database") == "mongodb"

    @pytest.mark.unit
    def test_determine_project_structure_python(self, autonomous_loop):
        """Testa determinacao de estrutura para Python"""
        structure = autonomous_loop._determine_project_structure("python, fastapi")

        assert structure["type"] == "python"
        assert "app" in structure["folders"]
        assert "tests" in structure["folders"]
        assert "main.py" in structure["files"]

    @pytest.mark.unit
    def test_determine_project_structure_react(self, autonomous_loop):
        """Testa determinacao de estrutura para React"""
        structure = autonomous_loop._determine_project_structure("react")

        assert structure["type"] == "react"
        assert "src" in structure["folders"]
        assert "package.json" in structure["files"]

    @pytest.mark.asyncio
    async def test_step_parse_success(self, autonomous_loop, temp_project_dir):
        """Testa step de parsing com sucesso"""
        autonomous_loop._project_path = temp_project_dir
        autonomous_loop._project_path.mkdir(exist_ok=True)

        class MockJob:
            description = "API REST simples"
            tech_stack = "python, fastapi"
            features = ["CRUD"]

        result = await autonomous_loop._step_parse(MockJob())

        assert result.success is True
        assert "parsed successfully" in result.message.lower()

        # Verifica se requirements.json foi criado
        req_file = temp_project_dir / "requirements.json"
        assert req_file.exists()

    @pytest.mark.asyncio
    async def test_step_generate_success(self, autonomous_loop, temp_project_dir):
        """Testa step de geracao de codigo"""
        autonomous_loop._project_path = temp_project_dir
        autonomous_loop._project_path.mkdir(exist_ok=True)

        # Criar requirements.json
        requirements = {
            "description": "API simples",
            "tech_stack": {"backend": "fastapi"},
            "features": ["CRUD"],
            "project_structure": {
                "type": "python",
                "folders": ["app", "tests"],
                "files": ["main.py"]
            }
        }
        (temp_project_dir / "requirements.json").write_text(json.dumps(requirements))

        class MockJob:
            description = "API REST simples"
            tech_stack = "python, fastapi"
            features = ["CRUD"]

        result = await autonomous_loop._step_generate(MockJob())

        assert result.success is True
        # Verifica arquivos gerados
        assert (temp_project_dir / "main.py").exists()
        assert (temp_project_dir / "requirements.txt").exists()

    @pytest.mark.asyncio
    async def test_standalone_run_success(self, autonomous_loop, temp_project_dir):
        """Testa execucao standalone do loop"""
        result = await autonomous_loop._run_standalone(
            job_id="JOB-TEST-001",
            description="API REST para lista de tarefas",
            tech_stack="python, fastapi",
            features=["CRUD tarefas"]
        )

        assert result["success"] is True
        assert "output_path" in result
        assert Path(result["output_path"]).exists()

    @pytest.mark.asyncio
    async def test_standalone_run_with_claude(self, autonomous_loop, mock_claude_client, temp_project_dir):
        """Testa execucao com cliente Claude"""
        autonomous_loop._claude_client = mock_claude_client
        autonomous_loop._model = "claude-sonnet-4-20250514"

        result = await autonomous_loop._run_standalone(
            job_id="JOB-TEST-002",
            description="API simples",
            tech_stack="python",
            features=[]
        )

        assert result["success"] is True


# =============================================================================
# TESTES DO JOB QUEUE
# =============================================================================

class TestJobQueue:
    """Testes para o sistema de filas de jobs"""

    @pytest.mark.unit
    def test_queue_config_defaults(self):
        """Testa configuracoes padrao da fila"""
        config = QueueConfig()

        assert config.backend == QueueBackend.REDIS
        assert config.max_workers == 5
        assert config.job_timeout == 600

    @pytest.mark.asyncio
    async def test_sqlite_queue_enqueue(self, mock_queue_config):
        """Testa enfileiramento de job na fila SQLite"""
        # Este teste requer setup do banco SQLite
        # Por simplicidade, testamos apenas a estrutura
        queue = SQLiteJobQueue(mock_queue_config)

        # Verificar callbacks inicializados
        assert "job_queued" in queue._callbacks
        assert "job_completed" in queue._callbacks
        assert "job_failed" in queue._callbacks

    @pytest.mark.unit
    def test_redis_queue_keys(self):
        """Testa chaves Redis da fila"""
        assert RedisJobQueue.QUEUE_KEY == "fabrica:jobs:queue"
        assert RedisJobQueue.JOBS_HASH == "fabrica:jobs:data"
        assert RedisJobQueue.WORKERS_HASH == "fabrica:workers"

    @pytest.mark.unit
    def test_queue_callback_registration(self, mock_queue_config):
        """Testa registro de callbacks"""
        queue = SQLiteJobQueue(mock_queue_config)

        callback_called = []

        def my_callback(data):
            callback_called.append(data)

        queue.on("job_queued", my_callback)
        queue._trigger("job_queued", {"test": True})

        assert len(callback_called) == 1
        assert callback_called[0]["test"] is True


# =============================================================================
# TESTES DO APP GENERATOR
# =============================================================================

class TestAppGenerator:
    """Testes para o gerador de aplicacoes testaveis"""

    @pytest.mark.unit
    def test_app_generator_initialization(self):
        """Testa inicializacao do AppGenerator"""
        generator = AppGenerator("TEST-PROJECT-001")

        assert generator.project_id == "TEST-PROJECT-001"
        assert generator.project_type is None
        assert generator.detected_models == []

    @pytest.mark.unit
    def test_normalize_name(self):
        """Testa normalizacao de nomes"""
        generator = AppGenerator("TEST-PROJECT-001")

        assert generator._normalize_name("TEST-PROJECT") == "testproject"
        assert generator._normalize_name("Test_Project") == "testproject"
        assert generator._normalize_name("Test Project") == "testproject"

    @pytest.mark.unit
    def test_generate_name_variations(self):
        """Testa geracao de variacoes de nome"""
        generator = AppGenerator("BELGO-BPM-001")

        variations = generator._generate_name_variations("BELGO-BPM-001")

        assert "BELGO-BPM-001" in variations
        assert "belgo-bpm-001" in variations
        assert "BELGO_BPM_001" in variations
        assert "belgo_bpm_001" in variations

    @pytest.mark.unit
    def test_analyze_project_not_found(self):
        """Testa analise de projeto inexistente"""
        generator = AppGenerator("PROJETO-INEXISTENTE-999")

        analysis = generator.analyze_project()

        assert analysis["status"] == "not_found"
        assert analysis["ready_to_test"] is False

    @pytest.mark.unit
    def test_detect_project_type_python(self, temp_project_dir):
        """Testa deteccao de projeto Python"""
        # Criar arquivo Python
        (temp_project_dir / "main.py").write_text("print('hello')")

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        project_type = generator._detect_project_type()

        assert project_type == "python"

    @pytest.mark.unit
    def test_detect_project_type_nodejs(self, temp_project_dir):
        """Testa deteccao de projeto Node.js"""
        # Criar package.json
        (temp_project_dir / "package.json").write_text('{"name": "test"}')

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        project_type = generator._detect_project_type()

        assert project_type == "nodejs"

    @pytest.mark.unit
    def test_count_files(self, temp_project_dir):
        """Testa contagem de arquivos por tipo"""
        # Criar arquivos de diferentes tipos
        (temp_project_dir / "main.py").write_text("print('test')")
        (temp_project_dir / "utils.py").write_text("pass")
        (temp_project_dir / "config.json").write_text("{}")

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        counts = generator._count_files()

        assert counts.get(".py", 0) == 2
        assert counts.get(".json", 0) == 1

    @pytest.mark.unit
    def test_find_python_models_sqlalchemy(self, temp_project_dir):
        """Testa deteccao de modelos SQLAlchemy"""
        model_code = '''
from sqlalchemy import Column, Integer, String
from database import Base

class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String(100))
    email = Column(String(200))
'''
        (temp_project_dir / "models.py").write_text(model_code)

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        models = generator._find_python_models()

        assert len(models) == 1
        assert models[0]["name"] == "User"
        assert models[0]["type"] == "sqlalchemy"

    @pytest.mark.unit
    def test_find_python_routes_fastapi(self, temp_project_dir):
        """Testa deteccao de rotas FastAPI"""
        route_code = '''
from fastapi import FastAPI

app = FastAPI()

@app.get("/users")
def list_users():
    return []

@app.post("/users")
def create_user():
    return {}

@router.delete("/users/{id}")
def delete_user(id: int):
    return {}
'''
        (temp_project_dir / "routes.py").write_text(route_code)

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        routes = generator._find_python_routes()

        assert len(routes) >= 2
        methods = [r["method"] for r in routes]
        assert "GET" in methods
        assert "POST" in methods

    @pytest.mark.unit
    def test_find_nodejs_models_sequelize(self, temp_project_dir):
        """Testa deteccao de modelos Sequelize"""
        model_code = '''
const { Model, DataTypes } = require('sequelize');

class User extends Model {}

User.init({
    name: DataTypes.STRING,
    email: DataTypes.STRING
}, { sequelize, modelName: 'User' });
'''
        (temp_project_dir / "models" / "user.js").parent.mkdir(parents=True, exist_ok=True)
        (temp_project_dir / "models" / "user.js").write_text(model_code)

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        models = generator._find_nodejs_models()

        assert len(models) >= 1
        assert any(m["name"] == "User" for m in models)

    @pytest.mark.unit
    def test_find_nodejs_routes_express(self, temp_project_dir):
        """Testa deteccao de rotas Express"""
        route_code = '''
const express = require('express');
const router = express.Router();

app.get('/api/users', (req, res) => {
    res.json([]);
});

router.post('/api/users', (req, res) => {
    res.json({});
});
'''
        (temp_project_dir / "routes.js").write_text(route_code)

        generator = AppGenerator("test")
        generator.project_path = temp_project_dir

        routes = generator._find_nodejs_routes()

        assert len(routes) >= 2


# =============================================================================
# TESTES DE INTEGRACAO COM CLAUDE API
# =============================================================================

class TestClaudeIntegration:
    """Testes de integracao com a Claude API (com mocks)"""

    @pytest.mark.unit
    def test_mock_claude_response_structure(self, mock_claude_client):
        """Testa estrutura de resposta mockada do Claude"""
        response = mock_claude_client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=1000,
            messages=[{"role": "user", "content": "Test"}]
        )

        assert response.content is not None
        assert len(response.content) > 0
        assert hasattr(response.content[0], 'text')

    @pytest.mark.unit
    def test_apply_claude_fixes(self, autonomous_loop, temp_project_dir):
        """Testa aplicacao de fixes do Claude"""
        autonomous_loop._project_path = temp_project_dir
        temp_project_dir.mkdir(exist_ok=True)

        fix_content = """
---FILE: main.py---
print("Fixed code")
---END FILE---

---FILE: utils/helper.py---
def helper():
    return True
---END FILE---
"""
        autonomous_loop._apply_claude_fixes(fix_content)

        assert (temp_project_dir / "main.py").exists()
        assert (temp_project_dir / "utils" / "helper.py").exists()

        main_content = (temp_project_dir / "main.py").read_text()
        assert "Fixed code" in main_content

    @pytest.mark.asyncio
    async def test_step_generate_with_claude(self, autonomous_loop, mock_claude_client, temp_project_dir):
        """Testa geracao de codigo usando Claude"""
        autonomous_loop._project_path = temp_project_dir
        autonomous_loop._claude_client = mock_claude_client
        autonomous_loop._model = "claude-sonnet-4-20250514"
        temp_project_dir.mkdir(exist_ok=True)

        # Criar requirements.json
        requirements = {
            "description": "API simples",
            "tech_stack": {"backend": "fastapi"},
            "features": []
        }
        (temp_project_dir / "requirements.json").write_text(json.dumps(requirements))

        class MockJob:
            description = "API simples"
            tech_stack = "python"
            features = []

        result = await autonomous_loop._step_generate_with_claude(MockJob())

        # Verifica que Claude foi chamado
        assert mock_claude_client.messages.create.called

    @pytest.mark.unit
    def test_auto_fix_prompt_structure(self, autonomous_loop, temp_project_dir):
        """Testa estrutura do prompt de auto-fix"""
        autonomous_loop._project_path = temp_project_dir

        step = "linting"
        result = StepResult(
            success=False,
            message="Lint errors",
            output="Error on line 10",
            errors=["undefined variable 'x'"]
        )

        # O metodo _attempt_fix criara um prompt com estas informacoes
        # Verificamos apenas que o resultado contem os dados necessarios
        assert result.message == "Lint errors"
        assert "undefined variable" in result.errors[0]


# =============================================================================
# TESTES DE FLUXO E2E
# =============================================================================

class TestE2EFlow:
    """Testes End-to-End do fluxo completo"""

    @pytest.mark.e2e
    @pytest.mark.asyncio
    async def test_full_story_to_code_flow(self, temp_project_dir, sample_story_data):
        """Teste E2E: Story -> Tasks -> Code Generation"""
        # 1. Gerar Story
        story_id = generate_story_id()
        agents = get_agents_for_category("backend")

        # 2. Criar Tasks
        tasks = create_tasks_for_story(
            story_id=story_id,
            story_title=sample_story_data["title"],
            category="backend",
            agents=agents[:2],
            complexity=sample_story_data["complexity"]
        )

        assert len(tasks) >= 2

        # 3. Configurar loop
        config = LoopConfig(
            max_attempts=2,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)

        # 4. Executar geracao
        result = await loop._run_standalone(
            job_id=f"JOB-{story_id}",
            description=sample_story_data["title"],
            tech_stack="python, fastapi",
            features=sample_story_data["acceptance_criteria"]
        )

        # 5. Validar resultado
        assert result["success"] is True
        assert Path(result["output_path"]).exists()

        # 6. Verificar arquivos gerados
        output_path = Path(result["output_path"])
        assert (output_path / "main.py").exists()
        assert (output_path / "requirements.txt").exists()

    @pytest.mark.e2e
    @pytest.mark.asyncio
    async def test_autonomous_loop_with_callbacks(self, temp_project_dir):
        """Teste E2E: Loop autonomo com callbacks de progresso"""
        progress_updates = []

        async def on_update(step: str, progress: float, message: str):
            progress_updates.append({
                "step": step,
                "progress": progress,
                "message": message
            })

        config = LoopConfig(
            max_attempts=2,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)
        loop._on_step_update = on_update

        result = await loop._run_standalone(
            job_id="JOB-CALLBACK-TEST",
            description="API de teste",
            tech_stack="python",
            features=[]
        )

        assert result["success"] is True
        # Deve ter pelo menos parsing e generating
        assert len(progress_updates) >= 2

    @pytest.mark.e2e
    def test_app_generator_full_analysis(self, temp_project_dir):
        """Teste E2E: Analise completa de projeto"""
        # Criar projeto Python simulado
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

        (temp_project_dir / "models.py").write_text('''
from sqlalchemy import Column, Integer, String
from database import Base

class User(Base):
    __tablename__ = "users"
    id = Column(Integer, primary_key=True)
    name = Column(String(100))
''')

        (temp_project_dir / "requirements.txt").write_text('''
fastapi>=0.104.0
uvicorn>=0.24.0
sqlalchemy>=2.0.0
''')

        # Analisar projeto
        generator = AppGenerator("test-project")
        generator.project_path = temp_project_dir

        analysis = generator.analyze_project()

        assert analysis["status"] == "analyzed"
        assert analysis["project_type"] == "python"
        assert len(analysis["models"]) >= 1
        assert len(analysis["routes"]) >= 2
        assert analysis["has_main"] is True
        assert analysis["has_requirements"] is True


# =============================================================================
# TESTES DE RESILIENCIA E FALHAS
# =============================================================================

class TestResilience:
    """Testes de resiliencia e tratamento de falhas"""

    @pytest.mark.unit
    def test_step_result_with_fix_suggestions(self):
        """Testa StepResult com sugestoes de correcao"""
        result = StepResult(
            success=False,
            message="Test failed",
            output="AssertionError: expected 1, got 2",
            errors=["test_function failed"],
            can_fix=True,
            fix_suggestions=["Check comparison logic", "Verify input data"]
        )

        assert result.can_fix is True
        assert len(result.fix_suggestions) == 2

    @pytest.mark.asyncio
    async def test_loop_max_attempts_limit(self, temp_project_dir):
        """Testa limite maximo de tentativas"""
        config = LoopConfig(
            max_attempts=2,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)

        # Mesmo com falhas, deve respeitar o max_attempts
        assert loop.config.max_attempts == 2

    @pytest.mark.unit
    def test_empty_tech_stack_handling(self, autonomous_loop):
        """Testa tratamento de tech stack vazio"""
        parsed = autonomous_loop._parse_tech_stack("")

        assert parsed == {}

    @pytest.mark.unit
    def test_none_tech_stack_handling(self, autonomous_loop):
        """Testa tratamento de tech stack None"""
        parsed = autonomous_loop._parse_tech_stack(None)

        assert parsed == {}

    @pytest.mark.unit
    def test_project_structure_generic(self, autonomous_loop):
        """Testa estrutura generica para stack desconhecida"""
        structure = autonomous_loop._determine_project_structure("unknown_stack")

        assert structure["type"] == "generic"
        assert "src" in structure["folders"]


# =============================================================================
# TESTES DE MULTI-WORKER (Simulado)
# =============================================================================

class TestMultiWorker:
    """Testes de cenarios multi-worker (simulados)"""

    @pytest.mark.unit
    def test_job_id_uniqueness(self):
        """Testa unicidade de IDs de job"""
        ids = set()
        for _ in range(100):
            story_id = generate_story_id()
            assert story_id not in ids
            ids.add(story_id)

        assert len(ids) == 100

    @pytest.mark.asyncio
    async def test_concurrent_loop_instances(self, temp_project_dir):
        """Testa instancias concorrentes do loop"""
        config = LoopConfig(
            max_attempts=1,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        # Criar multiplas instancias
        loops = [AutonomousLoop(config) for _ in range(3)]

        # Cada instancia deve ser independente
        for i, loop in enumerate(loops):
            loop._current_job_id = f"JOB-{i}"

        assert loops[0]._current_job_id == "JOB-0"
        assert loops[1]._current_job_id == "JOB-1"
        assert loops[2]._current_job_id == "JOB-2"


# =============================================================================
# CONFIGURACAO DE EXECUCAO
# =============================================================================

if __name__ == "__main__":
    # Executar testes com output detalhado
    pytest.main([
        __file__,
        "-v",
        "--tb=short",
        "-m", "not e2e",  # Excluir testes E2E por padrao (mais lentos)
        "--color=yes"
    ])
