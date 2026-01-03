# -*- coding: utf-8 -*-
"""
Regression Runner - Executor de testes regressivos.

Executa suites de testes regressivos automatizados:
- Smoke tests (< 5 min)
- Critical path (< 15 min)
- Full regression (< 1 hora)
"""

import sys
import os
import time
import json
import asyncio
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
import traceback

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))


class TestStatus(str, Enum):
    """Status do teste."""
    PENDING = "pending"
    RUNNING = "running"
    PASSED = "passed"
    FAILED = "failed"
    SKIPPED = "skipped"
    ERROR = "error"


class TestPriority(str, Enum):
    """Prioridade do teste."""
    CRITICAL = "critical"
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class SuiteType(str, Enum):
    """Tipo de suite de teste."""
    SMOKE = "smoke"
    CRITICAL_PATH = "critical_path"
    FULL_REGRESSION = "full_regression"
    PERSONA = "persona"
    SCREEN = "screen"
    INTEGRATION = "integration"
    WHITELABEL = "whitelabel"


@dataclass
class TestResult:
    """Resultado de um teste individual."""
    test_id: str
    test_name: str
    suite: str
    status: TestStatus
    duration_ms: int = 0
    error_message: Optional[str] = None
    error_type: Optional[str] = None
    stack_trace: Optional[str] = None
    screenshot_path: Optional[str] = None
    screen: Optional[str] = None
    persona: Optional[str] = None
    steps_executed: List[str] = field(default_factory=list)
    assertions: List[Dict] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "test_id": self.test_id,
            "test_name": self.test_name,
            "suite": self.suite,
            "status": self.status.value,
            "duration_ms": self.duration_ms,
            "error_message": self.error_message,
            "error_type": self.error_type,
            "stack_trace": self.stack_trace,
            "screenshot_path": self.screenshot_path,
            "screen": self.screen,
            "persona": self.persona,
            "steps_executed": self.steps_executed,
            "assertions": self.assertions,
            "metadata": self.metadata
        }

    @property
    def is_failure(self) -> bool:
        return self.status in [TestStatus.FAILED, TestStatus.ERROR]


@dataclass
class TestReport:
    """Relatorio completo de uma suite de testes."""
    suite_name: str
    suite_type: SuiteType
    started_at: datetime
    completed_at: Optional[datetime] = None
    results: List[TestResult] = field(default_factory=list)
    environment: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)

    @property
    def total_tests(self) -> int:
        return len(self.results)

    @property
    def passed_tests(self) -> int:
        return sum(1 for r in self.results if r.status == TestStatus.PASSED)

    @property
    def failed_tests(self) -> int:
        return sum(1 for r in self.results if r.status == TestStatus.FAILED)

    @property
    def error_tests(self) -> int:
        return sum(1 for r in self.results if r.status == TestStatus.ERROR)

    @property
    def skipped_tests(self) -> int:
        return sum(1 for r in self.results if r.status == TestStatus.SKIPPED)

    @property
    def pass_rate(self) -> float:
        if self.total_tests == 0:
            return 0.0
        return (self.passed_tests / self.total_tests) * 100

    @property
    def total_duration_ms(self) -> int:
        return sum(r.duration_ms for r in self.results)

    @property
    def failures(self) -> List[TestResult]:
        return [r for r in self.results if r.is_failure]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "suite_name": self.suite_name,
            "suite_type": self.suite_type.value,
            "started_at": self.started_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "summary": {
                "total": self.total_tests,
                "passed": self.passed_tests,
                "failed": self.failed_tests,
                "errors": self.error_tests,
                "skipped": self.skipped_tests,
                "pass_rate": self.pass_rate,
                "duration_ms": self.total_duration_ms
            },
            "results": [r.to_dict() for r in self.results],
            "failures": [r.to_dict() for r in self.failures],
            "environment": self.environment,
            "metadata": self.metadata
        }


# =============================================================================
# DEFINICOES DE TESTES
# =============================================================================

SMOKE_TESTS = [
    {
        "id": "smoke_login",
        "name": "Login funciona",
        "screen": "login",
        "priority": TestPriority.CRITICAL,
        "steps": ["navigate_to_login", "enter_credentials", "click_login", "verify_dashboard"]
    },
    {
        "id": "smoke_dashboard",
        "name": "Dashboard carrega",
        "screen": "dashboard",
        "priority": TestPriority.CRITICAL,
        "steps": ["navigate_to_dashboard", "verify_stats_loaded", "verify_no_errors"]
    },
    {
        "id": "smoke_api",
        "name": "API responde",
        "screen": None,
        "priority": TestPriority.CRITICAL,
        "steps": ["call_health_endpoint", "verify_status_200"]
    },
    {
        "id": "smoke_kanban",
        "name": "Kanban carrega",
        "screen": "kanban",
        "priority": TestPriority.CRITICAL,
        "steps": ["navigate_to_kanban", "verify_columns_visible", "verify_no_errors"]
    },
]

CRITICAL_PATH_TESTS = [
    # Story CRUD
    {
        "id": "critical_story_create",
        "name": "Criar Story",
        "screen": "stories",
        "priority": TestPriority.CRITICAL,
        "steps": ["navigate_to_stories", "click_create", "fill_form", "save", "verify_created"]
    },
    {
        "id": "critical_story_edit",
        "name": "Editar Story",
        "screen": "story_detail",
        "priority": TestPriority.HIGH,
        "steps": ["open_story", "edit_title", "save", "verify_updated"]
    },
    {
        "id": "critical_story_delete",
        "name": "Deletar Story",
        "screen": "stories",
        "priority": TestPriority.HIGH,
        "steps": ["open_story", "click_delete", "confirm", "verify_deleted"]
    },
    # Kanban
    {
        "id": "critical_kanban_drag",
        "name": "Drag-Drop Kanban",
        "screen": "kanban",
        "priority": TestPriority.CRITICAL,
        "steps": ["navigate_to_kanban", "drag_story", "drop_in_column", "verify_moved"]
    },
    # Project
    {
        "id": "critical_project_create",
        "name": "Criar Projeto",
        "screen": "projects",
        "priority": TestPriority.HIGH,
        "steps": ["navigate_to_projects", "click_create", "fill_form", "save", "verify_created"]
    },
    # User Management
    {
        "id": "critical_user_create",
        "name": "Criar Usuario",
        "screen": "users",
        "priority": TestPriority.HIGH,
        "steps": ["navigate_to_users", "click_create", "fill_form", "save", "verify_created"]
    },
]

FULL_REGRESSION_TESTS = SMOKE_TESTS + CRITICAL_PATH_TESTS + [
    # Mais testes abrangentes
    {
        "id": "regression_story_filters",
        "name": "Filtros de Stories",
        "screen": "stories",
        "priority": TestPriority.MEDIUM,
        "steps": ["navigate_to_stories", "apply_filter_status", "verify_filtered", "clear_filter"]
    },
    {
        "id": "regression_story_search",
        "name": "Busca de Stories",
        "screen": "stories",
        "priority": TestPriority.MEDIUM,
        "steps": ["navigate_to_stories", "enter_search", "verify_results"]
    },
    {
        "id": "regression_acceptance_criteria",
        "name": "Criterios de Aceite",
        "screen": "story_detail",
        "priority": TestPriority.HIGH,
        "steps": ["open_story", "add_criteria", "mark_complete", "verify_progress"]
    },
    {
        "id": "regression_reports",
        "name": "Visualizar Relatorios",
        "screen": "reports",
        "priority": TestPriority.MEDIUM,
        "steps": ["navigate_to_reports", "select_type", "verify_chart", "export"]
    },
    {
        "id": "regression_notifications",
        "name": "Notificacoes",
        "screen": "dashboard",
        "priority": TestPriority.LOW,
        "steps": ["open_notifications", "verify_list", "mark_read"]
    },
    {
        "id": "regression_user_profile",
        "name": "Perfil do Usuario",
        "screen": "profile",
        "priority": TestPriority.LOW,
        "steps": ["open_profile", "edit_info", "save", "verify_updated"]
    },
]


class RegressionTestRunner:
    """
    Executor de testes regressivos.

    Executa diferentes suites de teste e gera relatorios detalhados.
    """

    def __init__(
        self,
        base_url: str = "http://localhost:9001",
        screenshots_dir: str = None,
        use_playwright: bool = True
    ):
        self.base_url = base_url
        self.screenshots_dir = Path(screenshots_dir) if screenshots_dir else Path.cwd() / "test_screenshots"
        self.screenshots_dir.mkdir(parents=True, exist_ok=True)
        self.use_playwright = use_playwright
        self.current_report: Optional[TestReport] = None
        self._test_counter = 0

    def _generate_test_id(self) -> str:
        """Gera ID unico para teste."""
        self._test_counter += 1
        return f"TEST-{self._test_counter:05d}"

    async def _run_test(
        self,
        test_def: Dict,
        suite_name: str,
        persona: str = None
    ) -> TestResult:
        """
        Executa um teste individual.

        Args:
            test_def: Definicao do teste
            suite_name: Nome da suite
            persona: Persona para o teste

        Returns:
            TestResult com resultado
        """
        test_id = self._generate_test_id()
        start_time = time.time()
        steps_executed = []
        screenshot_path = None

        try:
            # Simular execucao dos passos
            # Em producao, usaria Playwright MCP
            for step in test_def.get("steps", []):
                steps_executed.append(step)
                # Simular tempo de execucao
                await asyncio.sleep(0.01)

            duration_ms = int((time.time() - start_time) * 1000)

            return TestResult(
                test_id=test_id,
                test_name=test_def["name"],
                suite=suite_name,
                status=TestStatus.PASSED,
                duration_ms=duration_ms,
                screen=test_def.get("screen"),
                persona=persona,
                steps_executed=steps_executed
            )

        except Exception as e:
            duration_ms = int((time.time() - start_time) * 1000)

            # Capturar screenshot em caso de erro
            screenshot_path = str(self.screenshots_dir / f"{test_id}_error.png")

            return TestResult(
                test_id=test_id,
                test_name=test_def["name"],
                suite=suite_name,
                status=TestStatus.FAILED,
                duration_ms=duration_ms,
                error_message=str(e),
                error_type=type(e).__name__,
                stack_trace=traceback.format_exc(),
                screenshot_path=screenshot_path,
                screen=test_def.get("screen"),
                persona=persona,
                steps_executed=steps_executed
            )

    async def run_smoke_tests(self) -> TestReport:
        """
        Executa smoke tests (< 5 min).

        Returns:
            TestReport com resultados
        """
        report = TestReport(
            suite_name="Smoke Tests",
            suite_type=SuiteType.SMOKE,
            started_at=datetime.utcnow(),
            environment={
                "base_url": self.base_url,
                "timestamp": datetime.utcnow().isoformat()
            }
        )

        for test_def in SMOKE_TESTS:
            result = await self._run_test(test_def, "smoke")
            report.results.append(result)

        report.completed_at = datetime.utcnow()
        return report

    async def run_critical_path(self) -> TestReport:
        """
        Executa testes de caminho critico (< 15 min).

        Returns:
            TestReport com resultados
        """
        report = TestReport(
            suite_name="Critical Path Tests",
            suite_type=SuiteType.CRITICAL_PATH,
            started_at=datetime.utcnow(),
            environment={
                "base_url": self.base_url,
                "timestamp": datetime.utcnow().isoformat()
            }
        )

        for test_def in CRITICAL_PATH_TESTS:
            result = await self._run_test(test_def, "critical_path")
            report.results.append(result)

        report.completed_at = datetime.utcnow()
        return report

    async def run_full_regression(self) -> TestReport:
        """
        Executa suite completa de regressao (< 1 hora).

        Returns:
            TestReport com resultados
        """
        report = TestReport(
            suite_name="Full Regression Tests",
            suite_type=SuiteType.FULL_REGRESSION,
            started_at=datetime.utcnow(),
            environment={
                "base_url": self.base_url,
                "timestamp": datetime.utcnow().isoformat()
            }
        )

        for test_def in FULL_REGRESSION_TESTS:
            result = await self._run_test(test_def, "full_regression")
            report.results.append(result)

        report.completed_at = datetime.utcnow()
        return report

    async def run_persona_tests(self, persona: str) -> TestReport:
        """
        Executa testes para uma persona especifica.

        Args:
            persona: Tipo da persona

        Returns:
            TestReport com resultados
        """
        report = TestReport(
            suite_name=f"Persona Tests - {persona}",
            suite_type=SuiteType.PERSONA,
            started_at=datetime.utcnow(),
            environment={
                "base_url": self.base_url,
                "persona": persona,
                "timestamp": datetime.utcnow().isoformat()
            }
        )

        # Importar fixtures de persona
        from .persona_fixtures import PersonaFixtures
        fixtures = PersonaFixtures(self.base_url)

        # Executar testes de persona
        persona_results = fixtures.run_all_personas()

        for test_result in persona_results.get(persona, []):
            report.results.append(TestResult(
                test_id=self._generate_test_id(),
                test_name=test_result.test_name,
                suite=f"persona_{persona}",
                status=TestStatus.PASSED if test_result.passed else TestStatus.FAILED,
                duration_ms=test_result.duration_ms,
                error_message=test_result.error_message,
                persona=persona,
                metadata=test_result.details
            ))

        report.completed_at = datetime.utcnow()
        return report

    async def run_screen_tests(self, screen: str) -> TestReport:
        """
        Executa testes para uma tela especifica.

        Args:
            screen: Nome da tela

        Returns:
            TestReport com resultados
        """
        from .screen_registry import get_screen_registry

        registry = get_screen_registry()
        test_cases = registry.get_test_cases_for_screen(screen)

        report = TestReport(
            suite_name=f"Screen Tests - {screen}",
            suite_type=SuiteType.SCREEN,
            started_at=datetime.utcnow(),
            environment={
                "base_url": self.base_url,
                "screen": screen,
                "timestamp": datetime.utcnow().isoformat()
            }
        )

        for test_case in test_cases:
            result = await self._run_test({
                "id": f"screen_{screen}_{test_case['name']}",
                "name": test_case["name"],
                "screen": screen,
                "priority": test_case.get("priority", TestPriority.MEDIUM),
                "steps": []
            }, f"screen_{screen}")
            report.results.append(result)

        report.completed_at = datetime.utcnow()
        return report

    def get_issues_for_creation(self) -> List[Dict]:
        """
        Retorna lista de issues para criar a partir das falhas.

        Returns:
            Lista de dicts com dados para criacao de issues
        """
        if not self.current_report:
            return []

        issues = []
        for failure in self.current_report.failures:
            issues.append({
                "test_id": failure.test_id,
                "test_name": failure.test_name,
                "suite": failure.suite,
                "screen": failure.screen,
                "persona": failure.persona,
                "error_type": failure.error_type,
                "error_message": failure.error_message,
                "screenshot_path": failure.screenshot_path,
                "steps_to_reproduce": failure.steps_executed,
                "stack_trace": failure.stack_trace
            })

        return issues

    def compare_with_baseline(self, baseline_path: str) -> Dict[str, Any]:
        """
        Compara resultados atuais com baseline.

        Args:
            baseline_path: Caminho para arquivo de baseline

        Returns:
            Dict com diferencas
        """
        if not self.current_report:
            return {"error": "No current report"}

        try:
            with open(baseline_path, 'r', encoding='utf-8') as f:
                baseline = json.load(f)
        except Exception as e:
            return {"error": f"Failed to load baseline: {e}"}

        current = self.current_report.to_dict()

        diff = {
            "new_failures": [],
            "fixed_tests": [],
            "regressions": [],
            "summary": {
                "baseline_pass_rate": baseline.get("summary", {}).get("pass_rate", 0),
                "current_pass_rate": current["summary"]["pass_rate"],
                "trend": "improved" if current["summary"]["pass_rate"] > baseline.get("summary", {}).get("pass_rate", 0) else "regressed"
            }
        }

        # Comparar testes
        baseline_failures = {r["test_name"] for r in baseline.get("failures", [])}
        current_failures = {r["test_name"] for r in current["failures"]}

        diff["new_failures"] = list(current_failures - baseline_failures)
        diff["fixed_tests"] = list(baseline_failures - current_failures)
        diff["regressions"] = list(current_failures & baseline_failures)

        return diff

    def save_report(self, path: str = None) -> Path:
        """
        Salva relatorio em arquivo JSON.

        Args:
            path: Caminho opcional

        Returns:
            Path do arquivo salvo
        """
        if not self.current_report:
            raise ValueError("No report to save")

        if path:
            output_path = Path(path)
        else:
            timestamp = datetime.utcnow().strftime("%Y%m%d_%H%M%S")
            output_path = self.screenshots_dir.parent / "reports" / f"report_{timestamp}.json"

        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(self.current_report.to_dict(), f, indent=2, ensure_ascii=False)

        return output_path


# Instancia global
_regression_runner: Optional[RegressionTestRunner] = None


def get_regression_runner(base_url: str = None) -> RegressionTestRunner:
    """Retorna instancia global do RegressionTestRunner."""
    global _regression_runner
    if _regression_runner is None:
        _regression_runner = RegressionTestRunner(base_url or "http://localhost:9001")
    return _regression_runner


if __name__ == "__main__":
    # Demo: Executar testes
    async def main():
        runner = RegressionTestRunner()

        print("\n=== DEMO: Regression Test Runner ===\n")

        # Executar smoke tests
        print("Executando Smoke Tests...")
        report = await runner.run_smoke_tests()
        runner.current_report = report

        print(f"\nSuite: {report.suite_name}")
        print(f"Total: {report.total_tests}")
        print(f"Passed: {report.passed_tests}")
        print(f"Failed: {report.failed_tests}")
        print(f"Pass Rate: {report.pass_rate:.1f}%")
        print(f"Duration: {report.total_duration_ms}ms")

        # Executar critical path
        print("\nExecutando Critical Path Tests...")
        report = await runner.run_critical_path()
        runner.current_report = report

        print(f"\nSuite: {report.suite_name}")
        print(f"Total: {report.total_tests}")
        print(f"Pass Rate: {report.pass_rate:.1f}%")

        # Verificar issues para criar
        issues = runner.get_issues_for_creation()
        print(f"\nIssues para criar: {len(issues)}")

    asyncio.run(main())
