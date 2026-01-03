# -*- coding: utf-8 -*-
"""
Issue Generator - Gerador automatico de issues no GitHub.

Cria issues automaticamente a partir de falhas de teste:
- Classificacao por tipo de erro (UI, API, Permissao, etc)
- Atribuicao automatica de agente responsavel
- Template padronizado com evidencias
"""

import os
import sys
import subprocess
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))


class ErrorType(str, Enum):
    """Tipos de erro encontrados nos testes."""
    UI_ERROR = "ui_error"                # Erro de interface (CSS, layout, render)
    API_ERROR = "api_error"              # Erro de API (500, timeout, etc)
    PERMISSION_ERROR = "permission_error" # Erro de permissao/acesso
    VALIDATION_ERROR = "validation_error" # Erro de validacao de dados
    PERFORMANCE_ERROR = "performance_error" # Erro de performance (lentidao)
    SECURITY_ERROR = "security_error"     # Erro de seguranca
    DATA_ERROR = "data_error"            # Erro de dados (inconsistencia)
    INTEGRATION_ERROR = "integration_error" # Erro de integracao externa
    UNKNOWN = "unknown"                   # Erro desconhecido


class IssuePriority(str, Enum):
    """Prioridade da issue."""
    CRITICAL = "critical"    # P0 - Bloqueador
    HIGH = "high"           # P1 - Alto impacto
    MEDIUM = "medium"       # P2 - Impacto moderado
    LOW = "low"             # P3 - Baixo impacto


# Mapeamento de tipo de erro para agente responsavel
ERROR_TO_AGENT = {
    ErrorType.UI_ERROR: "FRONT",
    ErrorType.API_ERROR: "BACK",
    ErrorType.PERMISSION_ERROR: "SEC",
    ErrorType.VALIDATION_ERROR: "BACK",
    ErrorType.PERFORMANCE_ERROR: "DEVOPS",
    ErrorType.SECURITY_ERROR: "SEC",
    ErrorType.DATA_ERROR: "BACK",
    ErrorType.INTEGRATION_ERROR: "BACK",
    ErrorType.UNKNOWN: "QA"
}

# Mapeamento de tipo de erro para labels
ERROR_TO_LABELS = {
    ErrorType.UI_ERROR: ["bug", "frontend", "ui"],
    ErrorType.API_ERROR: ["bug", "backend", "api"],
    ErrorType.PERMISSION_ERROR: ["bug", "security", "permissions"],
    ErrorType.VALIDATION_ERROR: ["bug", "backend", "validation"],
    ErrorType.PERFORMANCE_ERROR: ["bug", "performance", "devops"],
    ErrorType.SECURITY_ERROR: ["bug", "security", "critical"],
    ErrorType.DATA_ERROR: ["bug", "backend", "data"],
    ErrorType.INTEGRATION_ERROR: ["bug", "integration", "backend"],
    ErrorType.UNKNOWN: ["bug", "needs-triage"]
}


@dataclass
class TestFailure:
    """Representa uma falha de teste para criacao de issue."""
    test_id: str
    test_name: str
    suite: str
    screen: Optional[str]
    persona: Optional[str]
    error_type: ErrorType
    error_message: str
    stack_trace: Optional[str] = None
    screenshot_path: Optional[str] = None
    steps_to_reproduce: List[str] = field(default_factory=list)
    expected: str = ""
    actual: str = ""
    environment: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "test_id": self.test_id,
            "test_name": self.test_name,
            "suite": self.suite,
            "screen": self.screen,
            "persona": self.persona,
            "error_type": self.error_type.value,
            "error_message": self.error_message,
            "stack_trace": self.stack_trace,
            "screenshot_path": self.screenshot_path,
            "steps_to_reproduce": self.steps_to_reproduce,
            "expected": self.expected,
            "actual": self.actual,
            "environment": self.environment,
            "metadata": self.metadata
        }


@dataclass
class CreatedIssue:
    """Issue criada no GitHub."""
    issue_number: int
    url: str
    title: str
    agent_assigned: str
    priority: IssuePriority
    labels: List[str]
    test_failure: TestFailure
    created_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "issue_number": self.issue_number,
            "url": self.url,
            "title": self.title,
            "agent_assigned": self.agent_assigned,
            "priority": self.priority.value,
            "labels": self.labels,
            "test_failure": self.test_failure.to_dict(),
            "created_at": self.created_at.isoformat()
        }


class IssueGenerator:
    """
    Gerador de issues no GitHub a partir de falhas de teste.

    Cria issues automaticamente com:
    - Classificacao do tipo de erro
    - Atribuicao de agente responsavel
    - Template padronizado
    - Screenshots e evidencias
    """

    def __init__(
        self,
        repo: str = "anthropics/fabrica-de-agentes",
        screenshots_base_url: str = None
    ):
        self.repo = repo
        self.screenshots_base_url = screenshots_base_url or ""
        self._issue_counter = 0

    def classify_error(self, failure: TestFailure) -> ErrorType:
        """
        Classifica o tipo de erro baseado na mensagem e contexto.

        Args:
            failure: Falha de teste

        Returns:
            ErrorType classificado
        """
        error_msg = failure.error_message.lower()
        stack = (failure.stack_trace or "").lower()

        # Padroes de erro
        if any(x in error_msg for x in ["css", "style", "render", "display", "visible", "hidden"]):
            return ErrorType.UI_ERROR

        if any(x in error_msg for x in ["500", "502", "503", "504", "timeout", "api error"]):
            return ErrorType.API_ERROR

        if any(x in error_msg for x in ["403", "401", "forbidden", "unauthorized", "permission"]):
            return ErrorType.PERMISSION_ERROR

        if any(x in error_msg for x in ["validation", "invalid", "required", "format"]):
            return ErrorType.VALIDATION_ERROR

        if any(x in error_msg for x in ["slow", "performance", "timeout", "took too long"]):
            return ErrorType.PERFORMANCE_ERROR

        if any(x in error_msg for x in ["xss", "injection", "csrf", "security"]):
            return ErrorType.SECURITY_ERROR

        if any(x in error_msg for x in ["data", "null", "undefined", "not found", "missing"]):
            return ErrorType.DATA_ERROR

        if any(x in error_msg for x in ["integration", "external", "connection", "sync"]):
            return ErrorType.INTEGRATION_ERROR

        # Verificar no stack trace
        if "frontend" in stack or "react" in stack or "vue" in stack:
            return ErrorType.UI_ERROR

        if "api" in stack or "route" in stack or "endpoint" in stack:
            return ErrorType.API_ERROR

        return ErrorType.UNKNOWN

    def determine_priority(self, failure: TestFailure) -> IssuePriority:
        """
        Determina prioridade da issue baseado na falha.

        Args:
            failure: Falha de teste

        Returns:
            IssuePriority
        """
        # Testes criticos sao P0
        if "critical" in failure.suite.lower() or "smoke" in failure.suite.lower():
            return IssuePriority.CRITICAL

        # Erros de seguranca sao P1
        if failure.error_type in [ErrorType.SECURITY_ERROR, ErrorType.PERMISSION_ERROR]:
            return IssuePriority.HIGH

        # Telas principais sao P1
        if failure.screen in ["login", "dashboard", "kanban"]:
            return IssuePriority.HIGH

        # Persona admin/superadmin sao P1
        if failure.persona in ["ADMIN", "SUPER_ADMIN"]:
            return IssuePriority.HIGH

        # Erros de API sao P2
        if failure.error_type == ErrorType.API_ERROR:
            return IssuePriority.MEDIUM

        return IssuePriority.LOW

    def get_agent_for_error(self, error_type: ErrorType) -> str:
        """
        Retorna agente responsavel pelo tipo de erro.

        Args:
            error_type: Tipo do erro

        Returns:
            Codigo do agente (BACK, FRONT, etc)
        """
        return ERROR_TO_AGENT.get(error_type, "QA")

    def get_labels_for_error(self, error_type: ErrorType, priority: IssuePriority) -> List[str]:
        """
        Retorna labels para a issue.

        Args:
            error_type: Tipo do erro
            priority: Prioridade

        Returns:
            Lista de labels
        """
        labels = ERROR_TO_LABELS.get(error_type, ["bug"]).copy()

        # Adicionar label de prioridade
        priority_labels = {
            IssuePriority.CRITICAL: "priority:critical",
            IssuePriority.HIGH: "priority:high",
            IssuePriority.MEDIUM: "priority:medium",
            IssuePriority.LOW: "priority:low"
        }
        labels.append(priority_labels.get(priority, "priority:medium"))

        # Adicionar label de regressao
        labels.append("regression")

        return labels

    def generate_issue_title(self, failure: TestFailure) -> str:
        """
        Gera titulo da issue.

        Args:
            failure: Falha de teste

        Returns:
            Titulo formatado
        """
        screen_part = f"[{failure.screen}]" if failure.screen else "[General]"
        persona_part = f"({failure.persona})" if failure.persona else ""

        # Limitar tamanho do erro
        error_short = failure.error_message[:80] + "..." if len(failure.error_message) > 80 else failure.error_message

        return f"[Bug] {screen_part} {error_short} {persona_part}".strip()

    def generate_issue_body(self, failure: TestFailure, agent: str, priority: IssuePriority) -> str:
        """
        Gera corpo da issue usando template padrao.

        Args:
            failure: Falha de teste
            agent: Agente responsavel
            priority: Prioridade

        Returns:
            Corpo formatado em Markdown
        """
        # Formatar passos para reproduzir
        steps_md = "\n".join([f"{i+1}. {step}" for i, step in enumerate(failure.steps_to_reproduce)])
        if not steps_md:
            steps_md = "1. Executar teste automatizado"

        # Screenshot
        screenshot_md = ""
        if failure.screenshot_path:
            if self.screenshots_base_url:
                screenshot_url = f"{self.screenshots_base_url}/{Path(failure.screenshot_path).name}"
                screenshot_md = f"![Screenshot]({screenshot_url})"
            else:
                screenshot_md = f"*Screenshot disponivel em: {failure.screenshot_path}*"

        # Stack trace
        stack_md = ""
        if failure.stack_trace:
            stack_md = f"""
### Stack Trace
```
{failure.stack_trace[:2000]}
```
"""

        body = f"""## Bug Report - {failure.screen or 'General'} - {failure.error_type.value}

**Teste:** {failure.test_name}
**Suite:** {failure.suite}
**Persona:** {failure.persona or 'N/A'}
**Tela:** {failure.screen or 'N/A'}

---

### Passos para Reproduzir
{steps_md}

### Esperado
{failure.expected or 'Teste deve passar sem erros'}

### Atual
{failure.actual or failure.error_message}

### Screenshot
{screenshot_md}
{stack_md}

---

### Ambiente
- **URL:** {failure.environment.get('base_url', 'localhost:9001')}
- **Data:** {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S UTC')}
- **Test ID:** {failure.test_id}

### Classificacao Automatica
- **Tipo de Erro:** {failure.error_type.value}
- **Agente Sugerido:** {agent}
- **Prioridade:** {priority.value}

---

*🤖 Issue gerada automaticamente pelo QA Agent*
"""
        return body

    def create_issue_from_failure(self, failure: TestFailure) -> Optional[CreatedIssue]:
        """
        Cria issue no GitHub a partir de uma falha.

        Args:
            failure: Falha de teste

        Returns:
            CreatedIssue ou None se falhar
        """
        # Classificar erro
        if failure.error_type == ErrorType.UNKNOWN:
            failure.error_type = self.classify_error(failure)

        # Determinar prioridade e agente
        priority = self.determine_priority(failure)
        agent = self.get_agent_for_error(failure.error_type)
        labels = self.get_labels_for_error(failure.error_type, priority)

        # Gerar titulo e corpo
        title = self.generate_issue_title(failure)
        body = self.generate_issue_body(failure, agent, priority)

        # Criar issue via gh CLI
        try:
            labels_str = ",".join(labels)

            cmd = [
                "gh", "issue", "create",
                "--repo", self.repo,
                "--title", title,
                "--body", body,
                "--label", labels_str
            ]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                # Extrair URL e numero da issue
                url = result.stdout.strip()
                issue_number = int(url.split("/")[-1])

                return CreatedIssue(
                    issue_number=issue_number,
                    url=url,
                    title=title,
                    agent_assigned=agent,
                    priority=priority,
                    labels=labels,
                    test_failure=failure
                )
            else:
                print(f"Erro ao criar issue: {result.stderr}")
                return None

        except subprocess.TimeoutExpired:
            print("Timeout ao criar issue")
            return None
        except FileNotFoundError:
            print("gh CLI nao encontrado. Instalando...")
            return None
        except Exception as e:
            print(f"Erro ao criar issue: {e}")
            return None

    def create_batch_issues(self, failures: List[TestFailure]) -> List[CreatedIssue]:
        """
        Cria multiplas issues em batch.

        Args:
            failures: Lista de falhas

        Returns:
            Lista de issues criadas
        """
        created = []

        for failure in failures:
            issue = self.create_issue_from_failure(failure)
            if issue:
                created.append(issue)
                print(f"Issue criada: #{issue.issue_number} - {issue.title[:50]}...")

        return created

    def generate_summary_report(self, created_issues: List[CreatedIssue]) -> Dict[str, Any]:
        """
        Gera relatorio resumo das issues criadas.

        Args:
            created_issues: Lista de issues criadas

        Returns:
            Dict com resumo
        """
        by_agent = {}
        by_priority = {}
        by_error_type = {}

        for issue in created_issues:
            # Por agente
            agent = issue.agent_assigned
            if agent not in by_agent:
                by_agent[agent] = []
            by_agent[agent].append(issue.to_dict())

            # Por prioridade
            priority = issue.priority.value
            if priority not in by_priority:
                by_priority[priority] = 0
            by_priority[priority] += 1

            # Por tipo de erro
            error_type = issue.test_failure.error_type.value
            if error_type not in by_error_type:
                by_error_type[error_type] = 0
            by_error_type[error_type] += 1

        return {
            "total_issues": len(created_issues),
            "by_agent": {agent: len(issues) for agent, issues in by_agent.items()},
            "by_priority": by_priority,
            "by_error_type": by_error_type,
            "issues": [issue.to_dict() for issue in created_issues],
            "timestamp": datetime.utcnow().isoformat()
        }


# Instancia global
_issue_generator: Optional[IssueGenerator] = None


def get_issue_generator(repo: str = None) -> IssueGenerator:
    """Retorna instancia global do IssueGenerator."""
    global _issue_generator
    if _issue_generator is None:
        _issue_generator = IssueGenerator(repo or "anthropics/fabrica-de-agentes")
    return _issue_generator


if __name__ == "__main__":
    # Demo: Criar issue de teste
    generator = IssueGenerator()

    print("\n=== DEMO: Issue Generator ===\n")

    # Criar falha de teste simulada
    failure = TestFailure(
        test_id="TEST-00001",
        test_name="test_login_success",
        suite="smoke",
        screen="login",
        persona="DEVELOPER",
        error_type=ErrorType.UNKNOWN,
        error_message="Element #btn-login not visible after 5000ms timeout",
        stack_trace="TimeoutError: waiting for selector #btn-login\n  at LoginPage.clickLogin",
        steps_to_reproduce=[
            "Navegar para /login",
            "Preencher email: test@example.com",
            "Preencher senha: Test@123",
            "Clicar no botao Login"
        ],
        expected="Usuario deve ser redirecionado para dashboard",
        actual="Botao de login nao ficou visivel",
        environment={"base_url": "http://localhost:9001"}
    )

    # Classificar erro
    error_type = generator.classify_error(failure)
    print(f"Erro classificado como: {error_type.value}")

    # Determinar prioridade
    priority = generator.determine_priority(failure)
    print(f"Prioridade: {priority.value}")

    # Agente responsavel
    agent = generator.get_agent_for_error(error_type)
    print(f"Agente responsavel: {agent}")

    # Labels
    labels = generator.get_labels_for_error(error_type, priority)
    print(f"Labels: {labels}")

    # Gerar titulo
    title = generator.generate_issue_title(failure)
    print(f"\nTitulo: {title}")

    # Gerar corpo
    body = generator.generate_issue_body(failure, agent, priority)
    print(f"\nCorpo da issue:\n{'-'*40}\n{body[:500]}...")
