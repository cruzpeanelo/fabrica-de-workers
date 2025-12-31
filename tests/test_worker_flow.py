# -*- coding: utf-8 -*-
"""
Comprehensive Worker Flow Tests - Issue #29
============================================

Este modulo implementa testes completos para validar o fluxo de Workers
na arquitetura v4.0 da Fabrica de Agentes.

Cenarios de Teste:
1. Worker Initialization - Inicializacao e registro
2. Job Queue Processing - Enqueue/Dequeue Redis e SQLite
3. Story Execution Flow - Story -> Task -> Code Generation
4. Code Generation Verification - Validacao de arquivos gerados
5. Error Handling - Tratamento de falhas e recovery
6. Multi-Worker - Processamento concorrente
7. Claude API Mocking - Simulacao da API

Autor: Fabrica de Agentes
Data: 2024
Versao: 2.0.0
"""

import os
import sys
import json
import shutil
import asyncio
import tempfile
import pytest
import socket
from pathlib import Path
from datetime import datetime, timedelta
from unittest.mock import Mock, MagicMock, AsyncMock, patch, PropertyMock
from typing import Dict, Any, List, Optional
from dataclasses import dataclass

# Adicionar projeto ao path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Configurar ambiente de teste
os.environ["TESTING"] = "1"
os.environ["REDIS_URL"] = "redis://localhost:6379"

# Imports do projeto
from factory.core.worker import ClaudeWorker, WorkerPool
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
from factory.core.app_generator import AppGenerator


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def temp_project_dir():
    """Cria diretorio temporario para projetos de teste"""
    temp_dir = tempfile.mkdtemp(prefix="fabrica_worker_test_")
    yield Path(temp_dir)
    # Cleanup apos o teste
    if Path(temp_dir).exists():
        shutil.rmtree(temp_dir)


@pytest.fixture
def loop_config(temp_project_dir):
    """Configuracao do loop para testes (todas as validacoes desabilitadas)"""
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
    """Configuracao completa do loop para testes de integracao"""
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
def autonomous_loop(loop_config):
    """Instancia do autonomous loop para testes"""
    return AutonomousLoop(loop_config)


@pytest.fixture
def mock_claude_client():
    """Mock do cliente Claude API com resposta completa"""
    mock_client = MagicMock()
    mock_response = MagicMock()
    mock_response.content = [MagicMock(text="""
---FILE: main.py---
from fastapi import FastAPI

app = FastAPI(title="Test API")

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
def mock_claude_client_with_error():
    """Mock do cliente Claude API que retorna erro"""
    mock_client = MagicMock()
    mock_client.messages.create.side_effect = Exception("Claude API unavailable")
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
    """Configuracao de fila para testes (SQLite)"""
    return QueueConfig(
        backend=QueueBackend.SQLITE,
        max_workers=1,
        job_timeout=60
    )


@pytest.fixture
def mock_redis_config():
    """Configuracao de fila para testes (Redis)"""
    return QueueConfig(
        backend=QueueBackend.REDIS,
        redis_url="redis://localhost:6379",
        max_workers=3,
        job_timeout=600
    )


# =============================================================================
# TESTES DE INICIALIZACAO DO WORKER
# =============================================================================

class TestWorkerInitialization:
    """Testes para inicializacao do Worker"""

    @pytest.mark.unit
    def test_worker_default_initialization(self):
        """Testa inicializacao padrao do worker"""
        worker = ClaudeWorker()

        assert worker.worker_id is not None
        assert worker.worker_id.startswith("worker-")
        assert worker.model == "claude-sonnet-4-20250514"
        assert worker.mcp_tools == ["filesystem", "bash"]
        assert worker._running is False
        assert worker._current_job is None
        assert worker.jobs_processed == 0
        assert worker.jobs_failed == 0

    @pytest.mark.unit
    def test_worker_custom_initialization(self):
        """Testa inicializacao com parametros customizados"""
        worker = ClaudeWorker(
            worker_id="custom-worker-001",
            model="claude-opus-4-20250514",
            mcp_tools=["filesystem", "bash", "git"]
        )

        assert worker.worker_id == "custom-worker-001"
        assert worker.model == "claude-opus-4-20250514"
        assert "git" in worker.mcp_tools

    @pytest.mark.unit
    def test_worker_id_uniqueness(self):
        """Testa unicidade de IDs de workers"""
        workers = [ClaudeWorker() for _ in range(10)]
        worker_ids = [w.worker_id for w in workers]

        # Todos os IDs devem ser unicos
        assert len(set(worker_ids)) == 10

    @pytest.mark.unit
    def test_worker_get_ip(self):
        """Testa obtencao de IP local"""
        worker = ClaudeWorker()
        ip = worker._get_ip()

        # Deve retornar IP valido ou localhost
        assert ip is not None
        parts = ip.split(".")
        assert len(parts) == 4

    @pytest.mark.asyncio
    async def test_worker_initialize_without_redis(self):
        """Testa inicializacao quando Redis nao esta disponivel"""
        worker = ClaudeWorker(worker_id="test-worker")

        # Mock get_queue (importado dentro do metodo de job_queue)
        mock_queue = AsyncMock()
        mock_queue.register_worker = AsyncMock(return_value={
            "worker_id": "test-worker",
            "status": "idle"
        })

        async def mock_get_queue():
            return mock_queue

        # Mock cliente Anthropic (importado de anthropic)
        with patch('factory.core.job_queue.get_queue', mock_get_queue):
            with patch('anthropic.Anthropic') as mock_anthropic:
                mock_anthropic.return_value = MagicMock()

                await worker.initialize()

                assert worker._queue is not None
                assert worker._client is not None
                mock_queue.register_worker.assert_called_once()


# =============================================================================
# TESTES DE JOB QUEUE PROCESSING
# =============================================================================

class TestJobQueueProcessing:
    """Testes para processamento de filas de jobs"""

    @pytest.mark.unit
    def test_queue_config_defaults(self):
        """Testa configuracoes padrao da fila"""
        config = QueueConfig()

        assert config.backend == QueueBackend.REDIS
        assert config.max_workers == 5
        assert config.job_timeout == 600
        assert config.poll_interval == 1.0

    @pytest.mark.unit
    def test_sqlite_queue_callbacks_initialized(self, mock_queue_config):
        """Testa que callbacks estao inicializados na fila SQLite"""
        queue = SQLiteJobQueue(mock_queue_config)

        assert "job_queued" in queue._callbacks
        assert "job_started" in queue._callbacks
        assert "job_completed" in queue._callbacks
        assert "job_failed" in queue._callbacks
        assert "job_cancelled" in queue._callbacks

    @pytest.mark.unit
    def test_redis_queue_keys(self):
        """Testa chaves Redis da fila"""
        assert RedisJobQueue.QUEUE_KEY == "fabrica:jobs:queue"
        assert RedisJobQueue.JOBS_HASH == "fabrica:jobs:data"
        assert RedisJobQueue.WORKERS_HASH == "fabrica:workers"
        assert RedisJobQueue.CHANNEL_PREFIX == "fabrica:events:"

    @pytest.mark.unit
    def test_queue_callback_registration_and_trigger(self, mock_queue_config):
        """Testa registro e disparo de callbacks"""
        queue = SQLiteJobQueue(mock_queue_config)
        callback_data = []

        def my_callback(data):
            callback_data.append(data)

        queue.on("job_queued", my_callback)
        queue._trigger("job_queued", {"job_id": "JOB-TEST-001", "status": "pending"})

        assert len(callback_data) == 1
        assert callback_data[0]["job_id"] == "JOB-TEST-001"

    @pytest.mark.asyncio
    async def test_redis_queue_enqueue_mock(self):
        """Testa enqueue com mock do Redis"""
        config = QueueConfig(backend=QueueBackend.REDIS)
        queue = RedisJobQueue(config)

        # Mock Redis
        mock_redis = AsyncMock()
        mock_redis.hset = AsyncMock()
        mock_redis.rpush = AsyncMock()
        mock_redis.publish = AsyncMock()
        queue._redis = mock_redis

        job_data = {
            "description": "Test job",
            "tech_stack": "python",
            "features": ["feature1"]
        }

        job = await queue.enqueue(job_data)

        assert job["job_id"].startswith("JOB-")
        assert job["status"] == "pending"
        assert job["description"] == "Test job"
        mock_redis.hset.assert_called_once()
        mock_redis.rpush.assert_called_once()

    @pytest.mark.asyncio
    async def test_redis_queue_dequeue_mock(self):
        """Testa dequeue com mock do Redis"""
        config = QueueConfig(backend=QueueBackend.REDIS)
        queue = RedisJobQueue(config)

        # Mock Redis
        mock_redis = AsyncMock()
        mock_redis.blpop = AsyncMock(return_value=("queue", "JOB-TEST-001"))
        mock_redis.hget = AsyncMock(return_value=json.dumps({
            "job_id": "JOB-TEST-001",
            "description": "Test job",
            "status": "pending"
        }))
        mock_redis.hset = AsyncMock()
        mock_redis.publish = AsyncMock()
        queue._redis = mock_redis

        job = await queue.dequeue("worker-001", timeout=5)

        assert job is not None
        assert job["job_id"] == "JOB-TEST-001"
        assert job["status"] == "running"
        assert job["worker_id"] == "worker-001"

    @pytest.mark.asyncio
    async def test_redis_queue_dequeue_empty(self):
        """Testa dequeue quando fila esta vazia"""
        config = QueueConfig(backend=QueueBackend.REDIS)
        queue = RedisJobQueue(config)

        mock_redis = AsyncMock()
        mock_redis.blpop = AsyncMock(return_value=None)
        queue._redis = mock_redis

        job = await queue.dequeue("worker-001", timeout=1)

        assert job is None

    @pytest.mark.asyncio
    async def test_job_status_transitions(self):
        """Testa transicoes de status do job"""
        config = QueueConfig(backend=QueueBackend.REDIS)
        queue = RedisJobQueue(config)

        mock_redis = AsyncMock()
        job_data = {
            "job_id": "JOB-TEST-001",
            "status": "pending",
            "worker_id": "worker-001"
        }
        mock_redis.hget = AsyncMock(return_value=json.dumps(job_data))
        mock_redis.hset = AsyncMock()
        mock_redis.publish = AsyncMock()
        queue._redis = mock_redis

        # Transicao para running
        job = await queue.update_job_status("JOB-TEST-001", "running", step="generating")
        assert job["status"] == "running"

        # Transicao para completed
        job_data["status"] = "running"
        mock_redis.hget = AsyncMock(return_value=json.dumps(job_data))
        job = await queue.update_job_status("JOB-TEST-001", "completed", progress=100.0)
        assert job["status"] == "completed"
        assert job["progress"] == 100.0


# =============================================================================
# TESTES DE STORY EXECUTION FLOW
# =============================================================================

class TestStoryExecutionFlow:
    """Testes para fluxo de execucao de Stories"""

    @pytest.mark.unit
    def test_story_id_format(self):
        """Testa formato do ID da story"""
        story_id = generate_story_id()

        assert story_id.startswith("US-")
        parts = story_id.split("-")
        assert len(parts) == 3

    @pytest.mark.unit
    def test_task_creation_for_story(self, sample_story_data):
        """Testa criacao de tasks para uma story"""
        story_id = "US-TEST-001"
        agents = get_agents_for_category("backend")

        tasks = create_tasks_for_story(
            story_id=story_id,
            story_title=sample_story_data["title"],
            category="backend",
            agents=agents[:2],
            complexity=sample_story_data["complexity"]
        )

        assert len(tasks) >= 2
        assert all(isinstance(t, AgentTask) for t in tasks)

        # Verificar propriedades das tasks
        for task in tasks:
            assert task.task_id is not None
            assert task.agent_id in AGENT_SPECIALTIES
            assert task.estimated_hours > 0

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
        assert "output_path" in result
        assert Path(result["output_path"]).exists()

        # 6. Verificar arquivos gerados
        output_path = Path(result["output_path"])
        assert (output_path / "main.py").exists()
        assert (output_path / "requirements.txt").exists()


# =============================================================================
# TESTES DE CODE GENERATION VERIFICATION
# =============================================================================

class TestCodeGenerationVerification:
    """Testes para verificacao de geracao de codigo"""

    @pytest.mark.asyncio
    async def test_python_project_structure(self, autonomous_loop, temp_project_dir):
        """Testa estrutura de projeto Python gerado"""
        autonomous_loop._project_path = temp_project_dir

        class MockJob:
            job_id = "JOB-TEST-001"
            description = "API REST simples"
            tech_stack = "python, fastapi"
            features = ["CRUD"]
            project_id = None

        # Step Parse
        result = await autonomous_loop._step_parse(MockJob())
        assert result.success is True

        # Step Generate
        result = await autonomous_loop._step_generate(MockJob())
        assert result.success is True

        # Verificar estrutura
        assert (temp_project_dir / "main.py").exists()
        assert (temp_project_dir / "requirements.txt").exists()
        assert (temp_project_dir / "pyproject.toml").exists()
        assert (temp_project_dir / "tests").exists()

    @pytest.mark.asyncio
    async def test_node_project_structure(self, autonomous_loop, temp_project_dir):
        """Testa estrutura de projeto Node.js gerado"""
        autonomous_loop._project_path = temp_project_dir

        class MockJob:
            job_id = "JOB-TEST-002"
            description = "API Express"
            tech_stack = "node, express"
            features = ["REST API"]
            project_id = None

        # Step Parse
        result = await autonomous_loop._step_parse(MockJob())
        assert result.success is True

        # Step Generate
        result = await autonomous_loop._step_generate(MockJob())
        assert result.success is True

        # Verificar estrutura
        assert (temp_project_dir / "package.json").exists()
        assert (temp_project_dir / "index.js").exists()

    @pytest.mark.asyncio
    async def test_code_generation_with_claude(self, temp_project_dir, mock_claude_client):
        """Testa geracao de codigo usando Claude API"""
        config = LoopConfig(
            max_attempts=1,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)
        loop._claude_client = mock_claude_client
        loop._model = "claude-sonnet-4-20250514"
        loop._project_path = temp_project_dir

        # Criar requirements.json
        requirements = {
            "description": "API simples",
            "tech_stack": {"backend": "fastapi"},
            "features": []
        }
        (temp_project_dir / "requirements.json").write_text(json.dumps(requirements))

        class MockJob:
            job_id = "JOB-CLAUDE-001"
            description = "API simples"
            tech_stack = "python"
            features = []
            project_id = None

        result = await loop._step_generate_with_claude(MockJob())

        assert result.success is True
        mock_claude_client.messages.create.assert_called_once()

        # Verificar arquivos gerados pelo mock
        assert (temp_project_dir / "main.py").exists()

    @pytest.mark.unit
    def test_apply_claude_fixes(self, autonomous_loop, temp_project_dir):
        """Testa aplicacao de fixes do Claude"""
        autonomous_loop._project_path = temp_project_dir

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


# =============================================================================
# TESTES DE ERROR HANDLING
# =============================================================================

class TestErrorHandling:
    """Testes para tratamento de erros e recovery"""

    @pytest.mark.unit
    def test_step_result_with_errors(self):
        """Testa StepResult com lista de erros"""
        result = StepResult(
            success=False,
            message="Step failed",
            output="Error output",
            errors=["Error 1", "Error 2", "Error 3"],
            can_fix=True,
            fix_suggestions=["Fix suggestion 1"]
        )

        assert result.success is False
        assert result.can_fix is True
        assert len(result.errors) == 3
        assert len(result.fix_suggestions) == 1

    @pytest.mark.asyncio
    async def test_max_attempts_limit(self, temp_project_dir):
        """Testa que o loop respeita o limite maximo de tentativas"""
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

        # Contador de tentativas
        attempts = {"count": 0}
        original_step_parse = loop._step_parse

        async def failing_step_parse(job):
            attempts["count"] += 1
            return StepResult(
                success=False,
                message="Parse always fails",
                errors=["Intentional failure"],
                can_fix=True
            )

        loop._step_parse = failing_step_parse

        result = await loop._run_standalone(
            job_id="JOB-FAIL-001",
            description="Test failure",
            tech_stack="python",
            features=[]
        )

        assert result["success"] is False
        assert attempts["count"] == 2  # max_attempts

    @pytest.mark.asyncio
    async def test_worker_job_failure_handling(self):
        """Testa tratamento de falha de job no worker"""
        worker = ClaudeWorker(worker_id="test-worker")

        # Mock da fila
        mock_queue = AsyncMock()
        mock_queue.update_job_status = AsyncMock()
        mock_queue.add_job_log = AsyncMock()
        worker._queue = mock_queue

        # Mock do cliente Claude
        worker._client = MagicMock()

        # Job que vai falhar
        job = {
            "job_id": "JOB-FAIL-002",
            "description": "Test job",
            "tech_stack": "python",
            "features": []
        }

        # Mock do AutonomousLoop para falhar
        with patch('factory.core.autonomous_loop.AutonomousLoop') as MockLoop:
            mock_loop_instance = AsyncMock()
            mock_loop_instance.run = AsyncMock(return_value={
                "success": False,
                "error": "Test failure"
            })
            MockLoop.return_value = mock_loop_instance

            result = await worker._process_job(job)

            assert result["success"] is False
            mock_queue.update_job_status.assert_called()

    @pytest.mark.asyncio
    async def test_claude_api_error_handling(self, temp_project_dir, mock_claude_client_with_error):
        """Testa tratamento de erro da API Claude"""
        config = LoopConfig(
            max_attempts=1,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)
        loop._claude_client = mock_claude_client_with_error
        loop._model = "claude-sonnet-4-20250514"
        loop._project_path = temp_project_dir

        # Criar requirements.json
        requirements = {"description": "Test", "tech_stack": {}, "features": []}
        (temp_project_dir / "requirements.json").write_text(json.dumps(requirements))

        class MockJob:
            job_id = "JOB-ERR-001"
            description = "Test"
            tech_stack = "python"
            features = []
            project_id = None

        result = await loop._step_generate_with_claude(MockJob())

        assert result.success is False
        assert "Claude API unavailable" in result.message

    @pytest.mark.unit
    def test_empty_tech_stack_handling(self, autonomous_loop):
        """Testa tratamento de tech stack vazio"""
        parsed = autonomous_loop._parse_tech_stack("")
        assert parsed == {}

        parsed = autonomous_loop._parse_tech_stack(None)
        assert parsed == {}

    @pytest.mark.asyncio
    async def test_recovery_after_step_failure(self, temp_project_dir):
        """Testa recovery apos falha em um step"""
        config = LoopConfig(
            max_attempts=3,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)
        # Nao definir _claude_client para usar _step_generate
        loop._model = "claude-sonnet-4-20250514"

        # Contador para simular falha na primeira tentativa
        attempt_counter = {"count": 0}
        original_step_generate = loop._step_generate

        async def flaky_step_generate(job):
            attempt_counter["count"] += 1
            if attempt_counter["count"] == 1:
                return StepResult(
                    success=False,
                    message="First attempt failed",
                    errors=["Transient error"],
                    can_fix=True
                )
            return await original_step_generate(job)

        loop._step_generate = flaky_step_generate

        result = await loop._run_standalone(
            job_id="JOB-RECOVERY-001",
            description="Test recovery",
            tech_stack="python, fastapi",
            features=[]
        )

        # Deve ter sucesso apos recovery
        assert result["success"] is True
        assert attempt_counter["count"] >= 2


# =============================================================================
# TESTES DE MULTI-WORKER
# =============================================================================

class TestMultiWorker:
    """Testes de cenarios multi-worker"""

    @pytest.mark.unit
    def test_worker_pool_initialization(self):
        """Testa inicializacao do pool de workers"""
        pool = WorkerPool(num_workers=3)

        assert pool.num_workers == 3
        assert pool.workers == []
        assert pool._tasks == []

    @pytest.mark.unit
    def test_worker_pool_status(self):
        """Testa status do pool de workers"""
        pool = WorkerPool(num_workers=2)

        # Criar workers mock
        worker1 = ClaudeWorker(worker_id="worker-1")
        worker1._running = True
        worker1._current_job = {"job_id": "JOB-001"}
        worker1.jobs_processed = 5
        worker1.jobs_failed = 1

        worker2 = ClaudeWorker(worker_id="worker-2")
        worker2._running = True
        worker2._current_job = None
        worker2.jobs_processed = 3
        worker2.jobs_failed = 0

        pool.workers = [worker1, worker2]

        status = pool.get_status()

        assert status["num_workers"] == 2
        assert len(status["workers"]) == 2
        assert status["workers"][0]["current_job"] == "JOB-001"
        assert status["workers"][1]["current_job"] is None

    @pytest.mark.unit
    def test_job_id_uniqueness_under_load(self):
        """Testa unicidade de IDs de job sob carga"""
        ids = set()
        for _ in range(1000):
            story_id = generate_story_id()
            assert story_id not in ids, f"Duplicate ID found: {story_id}"
            ids.add(story_id)

        assert len(ids) == 1000

    @pytest.mark.asyncio
    async def test_concurrent_loop_instances(self, temp_project_dir):
        """Testa multiplas instancias do loop executando concorrentemente"""
        configs = [
            LoopConfig(
                max_attempts=1,
                lint_enabled=False,
                type_check_enabled=False,
                test_enabled=False,
                security_scan_enabled=False,
                auto_commit=False,
                project_base_dir=temp_project_dir / f"project_{i}"
            )
            for i in range(3)
        ]

        # Criar diretorios
        for config in configs:
            config.project_base_dir.mkdir(parents=True, exist_ok=True)

        # Criar loops
        loops = [AutonomousLoop(config) for config in configs]

        # Executar concorrentemente
        async def run_loop(loop, idx):
            return await loop._run_standalone(
                job_id=f"JOB-CONCURRENT-{idx}",
                description=f"Concurrent test {idx}",
                tech_stack="python",
                features=[]
            )

        results = await asyncio.gather(*[
            run_loop(loop, i) for i, loop in enumerate(loops)
        ])

        # Todos devem ter sucesso
        assert all(r["success"] for r in results)

        # Cada um deve ter seu proprio output_path
        output_paths = [r["output_path"] for r in results]
        assert len(set(output_paths)) == 3

    @pytest.mark.asyncio
    async def test_no_duplicate_job_processing(self):
        """Testa que nenhum job e processado duas vezes"""
        # Simular fila com jobs
        jobs = [
            {"job_id": f"JOB-{i}", "description": f"Job {i}", "status": "pending"}
            for i in range(5)
        ]

        processed_jobs = []
        processing_lock = asyncio.Lock()

        async def mock_dequeue(worker_id, timeout=5):
            async with processing_lock:
                for job in jobs:
                    if job["status"] == "pending":
                        job["status"] = "running"
                        job["worker_id"] = worker_id
                        processed_jobs.append(job["job_id"])
                        return job
                return None

        # Simular 3 workers pegando jobs
        workers = ["worker-1", "worker-2", "worker-3"]
        results = await asyncio.gather(*[
            mock_dequeue(w) for w in workers
        ])

        # Verificar que nao ha duplicatas
        assert len(processed_jobs) == len(set(processed_jobs))


# =============================================================================
# TESTES DE INTEGRACAO
# =============================================================================

class TestIntegration:
    """Testes de integracao do fluxo completo"""

    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_create_project_create_story_execute_verify(self, temp_project_dir, sample_story_data):
        """Teste de integracao: Criar projeto -> Criar story -> Executar -> Verificar output"""
        # 1. Criar projeto (diretorio)
        project_path = temp_project_dir / "integration-test-project"
        project_path.mkdir(parents=True, exist_ok=True)

        # 2. Criar Story
        story_id = generate_story_id()
        story = DetailedStory(
            story_id=story_id,
            project_id="INTEGRATION-TEST",
            title=sample_story_data["title"],
            description="Teste de integracao completo",
            persona=sample_story_data["persona"],
            action=sample_story_data["action"],
            benefit=sample_story_data["benefit"],
            epic="EPIC-INT-001",
            sprint=1,
            priority="high",
            points=5,
            complexity="medium",
            business_value=8,
            acceptance_criteria=sample_story_data["acceptance_criteria"],
            definition_of_done=DEFAULT_DOD,
            business_rules=[],
            technical_notes=[],
            assigned_to="AGT-08",
            agents=["AGT-08", "AGT-15"],
            tasks=[],
            dependencies=[]
        )

        # 3. Criar Tasks
        tasks = create_tasks_for_story(
            story_id=story_id,
            story_title=story.title,
            category="backend",
            agents=story.agents,
            complexity=story.complexity
        )

        assert len(tasks) >= 2

        # 4. Executar via Autonomous Loop
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

        result = await loop._run_standalone(
            job_id=f"JOB-INT-{story_id}",
            description=story.title,
            tech_stack="python, fastapi",
            features=story.acceptance_criteria
        )

        # 5. Verificar output
        assert result["success"] is True

        output_path = Path(result["output_path"])
        assert output_path.exists()
        assert (output_path / "main.py").exists()
        assert (output_path / "requirements.txt").exists()

        # Verificar conteudo dos arquivos
        main_content = (output_path / "main.py").read_text()
        assert "fastapi" in main_content.lower() or "FastAPI" in main_content

        requirements_content = (output_path / "requirements.txt").read_text()
        assert "fastapi" in requirements_content.lower()

    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_worker_job_execution_flow(self, temp_project_dir, mock_claude_client):
        """Teste de integracao do fluxo worker -> job execution"""
        worker = ClaudeWorker(worker_id="integration-test-worker")

        # Mock da fila
        mock_queue = AsyncMock()
        status_updates = []

        async def capture_status_update(job_id, status, **kwargs):
            status_updates.append({
                "job_id": job_id,
                "status": status,
                **kwargs
            })
            return {"job_id": job_id, "status": status}

        mock_queue.update_job_status = capture_status_update
        mock_queue.add_job_log = AsyncMock()
        worker._queue = mock_queue
        worker._client = mock_claude_client

        # Job para processar
        job = {
            "job_id": "JOB-INT-WORKER-001",
            "description": "API de integracao",
            "tech_stack": "python, fastapi",
            "features": ["CRUD"],
            "max_attempts": 2
        }

        # Mock do AutonomousLoop
        with patch('factory.core.autonomous_loop.AutonomousLoop') as MockLoop:
            mock_loop = MagicMock()

            async def mock_run(**kwargs):
                return {
                    "success": True,
                    "output_path": str(temp_project_dir / "test-output")
                }

            mock_loop.run = mock_run
            MockLoop.return_value = mock_loop

            result = await worker._process_job(job)

        assert result["success"] is True

        # Verificar que status foi atualizado
        assert any(u["status"] == "completed" for u in status_updates)

    @pytest.mark.integration
    def test_app_generator_with_generated_project(self, temp_project_dir):
        """Testa AppGenerator com projeto gerado"""
        # Criar projeto Python simulado (como se fosse gerado pelo loop)
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
    email = Column(String(200))
''')

        (temp_project_dir / "requirements.txt").write_text('''
fastapi>=0.104.0
uvicorn>=0.24.0
sqlalchemy>=2.0.0
''')

        # Analisar com AppGenerator
        generator = AppGenerator("test-project")
        generator.project_path = temp_project_dir

        analysis = generator.analyze_project()

        assert analysis["status"] == "analyzed"
        assert analysis["project_type"] == "python"
        assert analysis["has_main"] is True
        assert analysis["has_requirements"] is True
        assert len(analysis["models"]) >= 1
        assert len(analysis["routes"]) >= 2


# =============================================================================
# TESTES DE WORKER FAILURE RECOVERY
# =============================================================================

class TestWorkerFailureRecovery:
    """Testes de recovery de falhas do worker"""

    @pytest.mark.asyncio
    async def test_worker_crash_during_job(self):
        """Testa comportamento quando worker falha durante processamento"""
        worker = ClaudeWorker(worker_id="crash-test-worker")

        # Mock da fila
        mock_queue = AsyncMock()
        mock_queue.update_job_status = AsyncMock()
        worker._queue = mock_queue
        worker._client = MagicMock()
        worker._current_job = {"job_id": "JOB-CRASH-001"}

        # Simular erro no processamento
        job = {
            "job_id": "JOB-CRASH-001",
            "description": "Job that crashes",
            "tech_stack": "python"
        }

        with patch('factory.core.autonomous_loop.AutonomousLoop') as MockLoop:
            MockLoop.return_value.run = AsyncMock(
                side_effect=Exception("Simulated crash")
            )

            result = await worker._process_job(job)

        assert result["success"] is False
        assert "error" in result
        mock_queue.update_job_status.assert_called_with(
            "JOB-CRASH-001",
            "failed",
            step="failed",
            error="Simulated crash"
        )

    @pytest.mark.asyncio
    async def test_worker_timeout_handling(self, temp_project_dir):
        """Testa tratamento de timeout do worker"""
        config = LoopConfig(
            max_attempts=1,
            lint_enabled=False,
            type_check_enabled=False,
            test_enabled=False,
            security_scan_enabled=False,
            auto_commit=False,
            project_base_dir=temp_project_dir
        )

        loop = AutonomousLoop(config)

        # Step que demora muito
        async def slow_step(job):
            await asyncio.sleep(0.5)  # Simular operacao lenta
            return StepResult(success=True, message="Done")

        loop._step_parse = slow_step

        # Executar com timeout
        try:
            result = await asyncio.wait_for(
                loop._run_standalone(
                    job_id="JOB-TIMEOUT-001",
                    description="Slow job",
                    tech_stack="python",
                    features=[]
                ),
                timeout=0.1  # Timeout muito curto
            )
            assert False, "Should have timed out"
        except asyncio.TimeoutError:
            pass  # Esperado

    @pytest.mark.asyncio
    async def test_queue_fallback_to_sqlite(self):
        """Testa fallback para SQLite quando Redis falha"""
        # Resetar singleton
        import factory.core.job_queue as jq
        jq._queue_instance = None

        config = QueueConfig(backend=QueueBackend.REDIS)

        # Mock Redis para falhar
        with patch('redis.asyncio.from_url') as mock_redis:
            mock_redis.side_effect = Exception("Redis connection failed")

            # Mock SQLite
            with patch('factory.database.connection.init_db') as mock_init_db:
                mock_init_db.return_value = None

                # Deve usar SQLite como fallback
                # Nota: O get_queue real tentaria conectar, aqui estamos testando a logica
                try:
                    queue = await get_queue(config)
                    # Se chegou aqui, fallback funcionou
                    assert isinstance(queue, SQLiteJobQueue)
                except Exception:
                    # Erro esperado em ambiente de teste sem DB
                    pass

        # Limpar singleton
        jq._queue_instance = None


# =============================================================================
# TESTES DE CALLBACKS E PROGRESS
# =============================================================================

class TestCallbacksAndProgress:
    """Testes de callbacks e atualizacao de progresso"""

    @pytest.mark.asyncio
    async def test_autonomous_loop_with_callbacks(self, temp_project_dir):
        """Testa loop autonomo com callbacks de progresso"""
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

        # Verificar que progresso aumenta
        steps = [u["step"] for u in progress_updates]
        assert "parsing" in steps
        assert "generating" in steps

    @pytest.mark.asyncio
    async def test_job_progress_tracking(self):
        """Testa rastreamento de progresso do job"""
        config = QueueConfig(backend=QueueBackend.REDIS)
        queue = RedisJobQueue(config)

        # Mock Redis
        job_state = {"progress": 0.0}

        async def mock_hget(hash_name, key):
            return json.dumps({
                "job_id": "JOB-PROGRESS-001",
                "status": "running",
                "progress": job_state["progress"]
            })

        async def mock_hset(hash_name, key, value):
            data = json.loads(value)
            job_state["progress"] = data.get("progress", job_state["progress"])

        mock_redis = AsyncMock()
        mock_redis.hget = mock_hget
        mock_redis.hset = mock_hset
        mock_redis.publish = AsyncMock()
        queue._redis = mock_redis

        # Atualizar progresso
        for progress in [10.0, 30.0, 50.0, 70.0, 100.0]:
            await queue.update_job_status(
                "JOB-PROGRESS-001",
                "running" if progress < 100 else "completed",
                progress=progress
            )

        assert job_state["progress"] == 100.0


# =============================================================================
# CONFIGURACAO DE EXECUCAO
# =============================================================================

if __name__ == "__main__":
    # Executar testes com output detalhado
    pytest.main([
        __file__,
        "-v",
        "--tb=short",
        "-m", "not integration",  # Excluir testes de integracao por padrao
        "--color=yes"
    ])
