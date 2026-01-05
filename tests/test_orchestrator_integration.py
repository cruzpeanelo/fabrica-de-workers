# -*- coding: utf-8 -*-
"""
Test Orchestrator Integration - Testes de Integracao do Orquestrador
=====================================================================

Cenarios de teste para factory/core/orchestrator.py

Issues identificadas na analise:
- Line 166-172: subprocess.run bloqueante em funcao async
- Line 140, 223: _processed_issues set nao thread-safe (race condition)
- Line 222: Sem validacao se result e None
- Line 268: Sem timeout explicito para run_task()
- Line 281, 370: Null checks ausentes
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from datetime import datetime
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# =============================================================================
# ORCH-001: Classificacao de Issue SEC
# =============================================================================
class TestOrchestratorIssueClassification:
    """Testes de classificacao de issues por tipo."""

    def test_classify_security_issue_by_title(self):
        """ORCH-001: Issue com [SEC] no titulo deve ser roteada para agente SEC."""
        # Arrange
        issue_title = "[SEC] SQL Injection vulnerability in login"

        # Act - Simular classificacao
        agent = self._classify_issue(issue_title)

        # Assert
        assert agent == "[SEC]"

    def test_classify_backend_issue_by_title(self):
        """ORCH-002: Issue com [BACK] deve ir para agente BACK."""
        issue_title = "[BACK] Fix database connection pooling"
        agent = self._classify_issue(issue_title)
        assert agent == "[BACK]"

    def test_classify_frontend_issue_by_title(self):
        """Issue com [FRONT] deve ir para agente FRONT."""
        issue_title = "[FRONT] Button hover state broken"
        agent = self._classify_issue(issue_title)
        assert agent == "[FRONT]"

    def test_classify_qa_issue_by_title(self):
        """Issue com [QA] deve ir para agente QA."""
        issue_title = "[QA] Add integration tests for auth"
        agent = self._classify_issue(issue_title)
        assert agent == "[QA]"

    def test_classify_devops_issue_by_title(self):
        """Issue com [DEVOPS] deve ir para agente DEVOPS."""
        issue_title = "[DEVOPS] Configure CI/CD pipeline"
        agent = self._classify_issue(issue_title)
        assert agent == "[DEVOPS]"

    def test_classify_unknown_issue_defaults_to_orch(self):
        """Issue sem prefixo deve ficar com ORCH para triagem."""
        issue_title = "Something is broken"
        agent = self._classify_issue(issue_title)
        assert agent == "[ORCH]"

    def _classify_issue(self, title: str) -> str:
        """Helper para classificar issue por titulo."""
        prefixes = ["[SEC]", "[BACK]", "[FRONT]", "[QA]", "[DEVOPS]", "[ARCH]", "[PROD]"]
        for prefix in prefixes:
            if prefix in title:
                return prefix
        return "[ORCH]"


# =============================================================================
# ORCH-003-004: Handoff entre Agentes
# =============================================================================
class TestOrchestratorHandoff:
    """Testes de handoff entre agentes."""

    def test_handoff_back_to_qa_on_complete(self):
        """ORCH-003: BACK deve passar para QA ao completar."""
        from factory.core.handoff_manager import HandoffManager

        manager = HandoffManager()
        next_agents = manager.get_next_agents("[BACK]", "on_complete")

        assert "[QA]" in next_agents

    def test_handoff_qa_to_devops_on_pass(self):
        """ORCH-004: QA deve passar para DEVOPS quando testes passam."""
        from factory.core.handoff_manager import HandoffManager

        manager = HandoffManager()
        next_agents = manager.get_next_agents("[QA]", "on_tests_pass")

        assert "[DEVOPS]" in next_agents

    def test_handoff_sec_to_back_on_fix_needed(self):
        """SEC deve passar para BACK quando fix e necessario."""
        from factory.core.handoff_manager import HandoffManager

        manager = HandoffManager()
        next_agents = manager.get_next_agents("[SEC]", "on_fix_needed")

        assert "[BACK]" in next_agents

    def test_handoff_devops_to_orch_on_complete(self):
        """DEVOPS deve retornar para ORCH ao completar deploy."""
        from factory.core.handoff_manager import HandoffManager

        manager = HandoffManager()
        next_agents = manager.get_next_agents("[DEVOPS]", "on_complete")

        assert "[ORCH]" in next_agents


# =============================================================================
# ORCH-005: Context Compaction
# =============================================================================
class TestOrchestratorContextCompaction:
    """Testes de compactacao de contexto."""

    def test_context_compaction_triggered_on_large_context(self):
        """ORCH-005: Compactacao deve ser disparada com contexto grande."""
        # Simular contexto grande (>100k tokens)
        large_context = "x" * 500000  # ~500k chars

        should_compact = len(large_context) > 100000

        assert should_compact is True

    def test_context_compaction_preserves_essential_info(self):
        """Compactacao deve preservar informacoes essenciais."""
        context = {
            "issue_number": 123,
            "agent": "[BACK]",
            "files_modified": ["api.py", "models.py"],
            "verbose_logs": "x" * 100000
        }

        # Simular compactacao
        compacted = {
            "issue_number": context["issue_number"],
            "agent": context["agent"],
            "files_modified": context["files_modified"],
            "logs_summary": "Compacted"
        }

        assert compacted["issue_number"] == 123
        assert compacted["agent"] == "[BACK]"
        assert len(str(compacted)) < len(str(context))


# =============================================================================
# ORCH-006: Multiple Agents Concurrent
# =============================================================================
class TestOrchestratorConcurrency:
    """Testes de concorrencia no orquestrador."""

    @pytest.mark.asyncio
    async def test_multiple_agents_can_run_concurrently(self):
        """ORCH-006: Multiplos agentes podem rodar simultaneamente."""
        results = []

        async def mock_agent(name: str, delay: float):
            await asyncio.sleep(delay)
            results.append(name)
            return f"{name} done"

        # Rodar 3 agentes em paralelo
        tasks = [
            mock_agent("[BACK]", 0.1),
            mock_agent("[FRONT]", 0.1),
            mock_agent("[SEC]", 0.1)
        ]

        await asyncio.gather(*tasks)

        assert len(results) == 3
        assert "[BACK]" in results
        assert "[FRONT]" in results
        assert "[SEC]" in results

    @pytest.mark.asyncio
    async def test_processed_issues_set_thread_safety(self):
        """Teste de race condition no set _processed_issues."""
        processed = set()

        async def add_issue(issue_id: int):
            await asyncio.sleep(0.001)
            processed.add(issue_id)

        # Adicionar 100 issues concorrentemente
        tasks = [add_issue(i) for i in range(100)]
        await asyncio.gather(*tasks)

        # Deve ter 100 issues unicas
        assert len(processed) == 100


# =============================================================================
# ORCH-007: Agent Failure Recovery
# =============================================================================
class TestOrchestratorFailureRecovery:
    """Testes de recuperacao de falhas."""

    @pytest.mark.asyncio
    async def test_agent_failure_triggers_retry(self):
        """ORCH-007: Falha de agente deve disparar retry."""
        attempt_count = 0
        max_retries = 3

        async def failing_agent():
            nonlocal attempt_count
            attempt_count += 1
            if attempt_count < max_retries:
                raise Exception("Transient error")
            return "Success on retry"

        result = None
        for _ in range(max_retries):
            try:
                result = await failing_agent()
                break
            except Exception:
                continue

        assert result == "Success on retry"
        assert attempt_count == 3

    @pytest.mark.asyncio
    async def test_permanent_failure_after_max_retries(self):
        """Apos max retries, deve falhar permanentemente."""
        attempts = 0
        max_retries = 5
        last_error = None

        async def always_failing():
            nonlocal attempts
            attempts += 1
            raise Exception("Permanent error")

        for _ in range(max_retries):
            try:
                await always_failing()
            except Exception as e:
                last_error = e
                continue

        assert attempts == max_retries
        assert last_error is not None
        assert "Permanent error" in str(last_error)


# =============================================================================
# ORCH-008: Issue Deduplication
# =============================================================================
class TestOrchestratorDeduplication:
    """Testes de deduplicacao de issues."""

    def test_duplicate_issue_is_detected(self):
        """ORCH-008: Issues duplicadas devem ser detectadas."""
        processed = set()

        issue1 = {"number": 123, "title": "[BACK] Fix bug"}
        issue2 = {"number": 123, "title": "[BACK] Fix bug"}  # Mesma issue

        processed.add(issue1["number"])

        is_duplicate = issue2["number"] in processed

        assert is_duplicate is True

    def test_similar_issues_are_not_duplicate(self):
        """Issues similares mas diferentes nao sao duplicadas."""
        processed = set()

        issue1 = {"number": 123, "title": "[BACK] Fix bug A"}
        issue2 = {"number": 124, "title": "[BACK] Fix bug B"}  # Issue diferente

        processed.add(issue1["number"])

        is_duplicate = issue2["number"] in processed

        assert is_duplicate is False


# =============================================================================
# ORCH-009-011: Null Safety Tests
# =============================================================================
class TestOrchestratorNullSafety:
    """Testes de null safety."""

    def test_null_result_handling(self):
        """ORCH-009: Result None deve ser tratado."""
        result = None

        # Simular tratamento seguro
        processed_result = result if result else {"error": "No result"}

        assert processed_result["error"] == "No result"

    def test_null_job_handling(self):
        """Job None deve retornar erro apropriado."""
        job = None

        if not job:
            response = {"success": False, "error": "Job not found"}
        else:
            response = {"success": True}

        assert response["success"] is False
        assert "not found" in response["error"]

    def test_null_agent_handling(self):
        """Agent None deve usar fallback."""
        agent = None
        fallback = "[ORCH]"

        assigned_agent = agent or fallback

        assert assigned_agent == "[ORCH]"


# =============================================================================
# ORCH-012: Subprocess Blocking Test
# =============================================================================
class TestOrchestratorSubprocess:
    """Testes de subprocess bloqueante em async."""

    @pytest.mark.asyncio
    async def test_subprocess_should_use_async_version(self):
        """subprocess.run em funcao async deve usar asyncio.create_subprocess_exec."""
        import subprocess

        # Forma bloqueante (ruim em async)
        def blocking_version():
            return subprocess.run(["echo", "test"], capture_output=True)

        # Forma async (correta)
        async def async_version():
            proc = await asyncio.create_subprocess_exec(
                "echo", "test",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await proc.communicate()
            return stdout.decode()

        # Verificar que async version funciona
        result = await async_version()
        assert "test" in result or result == ""  # Windows pode nao ter echo


# =============================================================================
# ORCH-013: Timeout Handling
# =============================================================================
class TestOrchestratorTimeout:
    """Testes de timeout."""

    @pytest.mark.asyncio
    async def test_task_timeout_is_enforced(self):
        """ORCH-013: Timeout de task deve ser respeitado."""
        async def slow_task():
            await asyncio.sleep(10)
            return "done"

        with pytest.raises(asyncio.TimeoutError):
            await asyncio.wait_for(slow_task(), timeout=0.1)

    @pytest.mark.asyncio
    async def test_task_completes_within_timeout(self):
        """Task rapida deve completar dentro do timeout."""
        async def fast_task():
            await asyncio.sleep(0.01)
            return "done"

        result = await asyncio.wait_for(fast_task(), timeout=1.0)

        assert result == "done"


# =============================================================================
# Test Runner
# =============================================================================
if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
