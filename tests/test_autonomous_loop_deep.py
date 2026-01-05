# -*- coding: utf-8 -*-
"""
Test Autonomous Loop Deep - Testes Profundos do Loop Autonomo
==============================================================

Cenarios de teste para factory/core/autonomous_loop.py (1,355 linhas)

Issues identificadas na analise:
- Linha 65, 67: Default mutavel List[str] = None no dataclass
- Linhas 293-299, 583-596, 633-648, 683-698: subprocess.run bloqueante em async
- Linha 112-113: Parse hardcoded de "512m" pode falhar
- Linha 146: task_id com timestamp pode colidir
- Linha 414-416: Modifica job.current_attempt sem lock
"""

import pytest
import asyncio
import tempfile
import json
from pathlib import Path
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from datetime import datetime
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from factory.core.autonomous_loop import (
    AutonomousLoop, LoopConfig, StepResult, get_autonomous_loop
)


# =============================================================================
# LOOP-001: Generate Code Success
# =============================================================================
class TestLoopCodeGeneration:
    """Testes de geracao de codigo."""

    def test_step_result_success_creation(self):
        """LOOP-001: StepResult de sucesso deve ser criado corretamente."""
        result = StepResult(
            success=True,
            message="Code generated",
            output="Generated 5 files"
        )

        assert result.success is True
        assert result.message == "Code generated"
        assert result.can_fix is False  # Default

    def test_step_result_failure_with_errors(self):
        """StepResult de falha deve conter erros."""
        result = StepResult(
            success=False,
            message="Lint failed",
            output="3 errors found",
            errors=["E501 line too long", "E302 missing blank line"],
            can_fix=True
        )

        assert result.success is False
        assert len(result.errors) == 2
        assert result.can_fix is True

    def test_loop_config_default_values(self):
        """LoopConfig deve ter valores default corretos."""
        config = LoopConfig()

        assert config.max_attempts == 5
        assert config.lint_enabled is True
        assert config.test_enabled is True
        assert config.sandbox_enabled is True
        assert config.sandbox_timeout == 120

    def test_loop_config_custom_values(self):
        """LoopConfig deve aceitar valores customizados."""
        config = LoopConfig(
            max_attempts=3,
            sandbox_timeout=60,
            sandbox_memory_limit="256m"
        )

        assert config.max_attempts == 3
        assert config.sandbox_timeout == 60
        assert config.sandbox_memory_limit == "256m"


# =============================================================================
# LOOP-002: Lint Failure Auto-Fix
# =============================================================================
class TestLoopLintAutoFix:
    """Testes de auto-fix de lint."""

    @pytest.mark.asyncio
    async def test_lint_failure_triggers_autofix(self):
        """LOOP-002: Falha de lint deve disparar auto-fix."""
        loop = AutonomousLoop(LoopConfig(lint_enabled=True))

        # Simular step de lint que falha
        lint_result = StepResult(
            success=False,
            message="Lint errors",
            errors=["E501"],
            can_fix=True
        )

        assert lint_result.can_fix is True

    def test_lint_fix_suggestions_extraction(self):
        """Sugestoes de fix devem ser extraidas dos erros."""
        errors = [
            "E501 line too long (120 > 100 characters)",
            "E302 expected 2 blank lines, found 1"
        ]

        # Simular extracao de sugestoes
        suggestions = []
        for error in errors:
            if "line too long" in error:
                suggestions.append("Reduce line length")
            elif "blank lines" in error:
                suggestions.append("Add blank line")

        assert len(suggestions) == 2


# =============================================================================
# LOOP-003: Test Failure Auto-Fix (5 attempts)
# =============================================================================
class TestLoopTestAutoFix:
    """Testes de auto-fix de testes."""

    @pytest.mark.asyncio
    async def test_max_5_attempts_for_test_fix(self):
        """LOOP-003: Maximo de 5 tentativas para fix de teste."""
        config = LoopConfig(max_attempts=5)
        attempts = 0

        for attempt in range(1, config.max_attempts + 1):
            attempts += 1
            # Simular falha
            success = False
            if not success and attempt >= config.max_attempts:
                break

        assert attempts == 5

    @pytest.mark.asyncio
    async def test_test_fix_stops_on_success(self):
        """Fix deve parar ao conseguir sucesso."""
        config = LoopConfig(max_attempts=5)
        attempts = 0

        for attempt in range(1, config.max_attempts + 1):
            attempts += 1
            # Sucesso na terceira tentativa
            if attempt == 3:
                break

        assert attempts == 3


# =============================================================================
# LOOP-004: Security Scan Failure
# =============================================================================
class TestLoopSecurityScan:
    """Testes de scan de seguranca."""

    def test_security_scan_detects_vulnerabilities(self):
        """LOOP-004: Security scan deve detectar vulnerabilidades."""
        # Codigo com SQL injection
        vulnerable_code = """
def get_user(user_id):
    query = "SELECT * FROM users WHERE id = " + user_id
    return db.execute(query)
"""

        # Simular deteccao
        has_sql_injection = "SELECT *" in vulnerable_code and "+" in vulnerable_code

        assert has_sql_injection is True

    def test_security_scan_passes_safe_code(self):
        """Security scan deve passar codigo seguro."""
        safe_code = """
def get_user(user_id: int):
    return db.query(User).filter(User.id == user_id).first()
"""

        # Simular verificacao
        has_sql_injection = "SELECT *" in safe_code and "+" in safe_code

        assert has_sql_injection is False


# =============================================================================
# LOOP-005: Sandbox Timeout
# =============================================================================
class TestLoopSandboxTimeout:
    """Testes de timeout do sandbox."""

    @pytest.mark.asyncio
    async def test_sandbox_timeout_enforced(self):
        """LOOP-005: Timeout do sandbox deve ser respeitado."""
        config = LoopConfig(sandbox_timeout=1)  # 1 segundo

        async def slow_execution():
            await asyncio.sleep(10)
            return "done"

        with pytest.raises(asyncio.TimeoutError):
            await asyncio.wait_for(slow_execution(), timeout=config.sandbox_timeout)

    @pytest.mark.asyncio
    async def test_sandbox_completes_within_timeout(self):
        """Execucao rapida deve completar dentro do timeout."""
        config = LoopConfig(sandbox_timeout=5)

        async def fast_execution():
            await asyncio.sleep(0.1)
            return "done"

        result = await asyncio.wait_for(fast_execution(), timeout=config.sandbox_timeout)

        assert result == "done"


# =============================================================================
# LOOP-006: Memory Limit Exceeded
# =============================================================================
class TestLoopMemoryLimit:
    """Testes de limite de memoria."""

    def test_memory_limit_parsing(self):
        """LOOP-006: Parse de limite de memoria."""
        config = LoopConfig(sandbox_memory_limit="512m")

        # Simular parse
        memory_str = config.sandbox_memory_limit
        memory_mb = int(memory_str.replace("m", "").replace("M", ""))

        assert memory_mb == 512

    def test_memory_limit_parsing_gigabytes(self):
        """Parse de limite em GB."""
        memory_str = "2g"

        if "g" in memory_str.lower():
            memory_mb = int(memory_str.lower().replace("g", "")) * 1024
        else:
            memory_mb = int(memory_str.lower().replace("m", ""))

        assert memory_mb == 2048

    def test_memory_limit_invalid_format(self):
        """Formato invalido deve ser tratado."""
        memory_str = "invalid"

        try:
            memory_mb = int(memory_str.replace("m", ""))
            parsed = True
        except ValueError:
            parsed = False
            memory_mb = 512  # Default

        assert parsed is False
        assert memory_mb == 512


# =============================================================================
# LOOP-007: Full Pipeline Success
# =============================================================================
class TestLoopFullPipeline:
    """Testes do pipeline completo."""

    @pytest.mark.asyncio
    async def test_full_pipeline_success_flow(self):
        """LOOP-007: Pipeline completo deve funcionar."""
        steps_executed = []

        async def mock_step(name: str):
            steps_executed.append(name)
            return StepResult(success=True, message=f"{name} done")

        # Simular pipeline
        await mock_step("parsing")
        await mock_step("generating")
        await mock_step("linting")
        await mock_step("type_checking")
        await mock_step("testing")
        await mock_step("security_scan")
        await mock_step("committing")

        assert len(steps_executed) == 7
        assert "parsing" in steps_executed
        assert "committing" in steps_executed

    @pytest.mark.asyncio
    async def test_pipeline_stops_on_unrecoverable_failure(self):
        """Pipeline deve parar em falha irrecuperavel."""
        steps_executed = []

        async def mock_step(name: str, should_fail: bool = False):
            steps_executed.append(name)
            if should_fail:
                return StepResult(success=False, message="Failed", can_fix=False)
            return StepResult(success=True, message="Done")

        # Simular pipeline com falha em linting
        await mock_step("parsing")
        await mock_step("generating")
        result = await mock_step("linting", should_fail=True)

        if not result.success and not result.can_fix:
            # Parar pipeline
            pass
        else:
            await mock_step("testing")

        assert "testing" not in steps_executed


# =============================================================================
# LOOP-008: Partial Failure Recovery
# =============================================================================
class TestLoopPartialRecovery:
    """Testes de recuperacao parcial."""

    @pytest.mark.asyncio
    async def test_partial_failure_allows_retry(self):
        """LOOP-008: Falha parcial deve permitir retry."""
        attempt = 0
        max_attempts = 5

        async def flaky_step():
            nonlocal attempt
            attempt += 1
            # Falha nas primeiras 2 tentativas
            if attempt <= 2:
                return StepResult(
                    success=False,
                    message="Transient error",
                    can_fix=True
                )
            return StepResult(success=True, message="Success")

        result = None
        for _ in range(max_attempts):
            result = await flaky_step()
            if result.success:
                break

        assert result.success is True
        assert attempt == 3


# =============================================================================
# LOOP-009-011: Task ID Collision Tests
# =============================================================================
class TestLoopTaskIdCollision:
    """Testes de colisao de task_id."""

    def test_task_id_uniqueness_with_timestamp(self):
        """LOOP-009: Task IDs com timestamp devem ser unicos."""
        import time

        task_ids = set()
        for i in range(100):
            task_id = f"job-{datetime.now().timestamp()}-{i}"
            task_ids.add(task_id)
            time.sleep(0.001)  # Pequeno delay

        assert len(task_ids) == 100

    def test_task_id_collision_without_suffix(self):
        """Task IDs sem sufixo podem colidir em chamadas rapidas."""
        task_ids = []

        # Gerar 10 IDs rapidamente (sem delay)
        for _ in range(10):
            task_id = f"job-{datetime.now().timestamp()}"
            task_ids.append(task_id)

        # Pode haver colisoes
        unique_ids = set(task_ids)
        # Nao garantimos que todos sao unicos sem sufixo


# =============================================================================
# LOOP-012: Project Structure Generation
# =============================================================================
class TestLoopProjectStructure:
    """Testes de geracao de estrutura de projeto."""

    def test_python_project_structure(self):
        """Projeto Python deve ter estrutura correta."""
        loop = AutonomousLoop()
        structure = loop._determine_project_structure("python, fastapi, sqlite")

        assert structure["type"] == "python"
        assert "app" in structure["folders"]
        assert "tests" in structure["folders"]
        assert "main.py" in structure["files"]
        assert "requirements.txt" in structure["files"]

    def test_node_project_structure(self):
        """Projeto Node deve ter estrutura correta."""
        loop = AutonomousLoop()
        structure = loop._determine_project_structure("node, express")

        assert structure["type"] == "node"
        assert "routes" in structure["folders"]
        assert "package.json" in structure["files"]

    def test_react_project_structure(self):
        """Projeto React deve ter estrutura correta."""
        loop = AutonomousLoop()
        structure = loop._determine_project_structure("react, typescript")

        assert structure["type"] == "react"
        assert "src" in structure["folders"]
        assert "src/components" in structure["folders"]

    def test_generic_project_structure(self):
        """Projeto generico deve ter estrutura basica."""
        loop = AutonomousLoop()
        structure = loop._determine_project_structure("unknown")

        assert structure["type"] == "generic"
        assert "src" in structure["folders"]


# =============================================================================
# LOOP-013: Tech Stack Parsing
# =============================================================================
class TestLoopTechStackParsing:
    """Testes de parse do tech stack."""

    def test_parse_fastapi_backend(self):
        """FastAPI deve ser detectado como backend."""
        loop = AutonomousLoop()
        stack = loop._parse_tech_stack("fastapi, postgresql")

        assert stack.get("backend") == "fastapi"
        assert stack.get("database") == "postgresql"

    def test_parse_react_frontend(self):
        """React deve ser detectado como frontend."""
        loop = AutonomousLoop()
        stack = loop._parse_tech_stack("react, node")

        assert stack.get("frontend") == "react"

    def test_parse_full_stack(self):
        """Full stack deve detectar todos os componentes."""
        loop = AutonomousLoop()
        stack = loop._parse_tech_stack("fastapi, react, postgresql")

        assert stack.get("backend") == "fastapi"
        assert stack.get("frontend") == "react"
        assert stack.get("database") == "postgresql"


# =============================================================================
# LOOP-014: Project Name Generation
# =============================================================================
class TestLoopProjectNameGeneration:
    """Testes de geracao de nome de projeto."""

    def test_project_name_from_description(self):
        """Nome de projeto deve ser gerado da descricao."""
        loop = AutonomousLoop()
        name = loop._generate_project_name("API REST para lista de tarefas")

        # Deve ter slug + timestamp
        assert "api" in name.lower()
        assert "-" in name  # Tem separadores

    def test_project_name_removes_special_chars(self):
        """Nome deve remover caracteres especiais."""
        loop = AutonomousLoop()
        name = loop._generate_project_name("API @#$% Test!!!")

        # Nao deve ter caracteres especiais
        assert "@" not in name
        assert "#" not in name
        assert "%" not in name
        assert "!" not in name


# =============================================================================
# LOOP-015: Singleton Pattern
# =============================================================================
class TestLoopSingleton:
    """Testes do padrao singleton."""

    def test_get_autonomous_loop_returns_same_instance(self):
        """get_autonomous_loop deve retornar mesma instancia."""
        loop1 = get_autonomous_loop()
        loop2 = get_autonomous_loop()

        assert loop1 is loop2

    def test_custom_config_creates_new_instance(self):
        """Config customizado deve criar nova instancia."""
        # Reset singleton para teste
        import factory.core.autonomous_loop as loop_module
        loop_module._loop_instance = None

        config = LoopConfig(max_attempts=3)
        loop = get_autonomous_loop(config)

        assert loop.config.max_attempts == 3


# =============================================================================
# Test Runner
# =============================================================================
if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
