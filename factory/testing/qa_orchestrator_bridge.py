# -*- coding: utf-8 -*-
"""
QA Orchestrator Bridge - Ponte entre QA e Orquestrador.

Integra o sistema de testes QA com o orquestrador de agentes:
- Notifica inicio/fim de testes
- Solicita atribuicao de issues
- Monitora status das correcoes
"""

import os
import sys
import json
import asyncio
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))


@dataclass
class TestSuiteNotification:
    """Notificacao de suite de testes."""
    suite_name: str
    suite_type: str
    status: str  # started, completed, failed
    timestamp: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "suite_name": self.suite_name,
            "suite_type": self.suite_type,
            "status": self.status,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata
        }


@dataclass
class IssueAssignment:
    """Atribuicao de issue para agente."""
    issue_number: int
    issue_url: str
    agent_type: str
    priority: str
    status: str  # pending, assigned, in_progress, completed, failed
    assigned_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    task_id: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "issue_number": self.issue_number,
            "issue_url": self.issue_url,
            "agent_type": self.agent_type,
            "priority": self.priority,
            "status": self.status,
            "assigned_at": self.assigned_at.isoformat() if self.assigned_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "task_id": self.task_id
        }


class QAOrchestratorBridge:
    """
    Ponte entre QA Agent e Orquestrador.

    Permite comunicacao bidirecional para:
    - Notificar sobre execucao de testes
    - Solicitar atribuicao de issues aos agentes
    - Monitorar progresso das correcoes
    """

    def __init__(self, base_path: str = None):
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.state_path = self.base_path / "factory" / "state"
        self.tasks_path = self.state_path / "tasks"
        self.results_path = self.state_path / "results"
        self.qa_state_file = self.state_path / "qa_state.json"

        # Garantir que diretorios existem
        self.tasks_path.mkdir(parents=True, exist_ok=True)
        self.results_path.mkdir(parents=True, exist_ok=True)

        # Estado interno
        self.pending_assignments: List[IssueAssignment] = []
        self.completed_assignments: List[IssueAssignment] = []

    def _load_state(self) -> Dict[str, Any]:
        """Carrega estado do QA do arquivo."""
        if self.qa_state_file.exists():
            try:
                with open(self.qa_state_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
            except:
                pass
        return {"last_run": None, "pending_issues": [], "completed_issues": []}

    def _save_state(self, state: Dict[str, Any]):
        """Salva estado do QA no arquivo."""
        try:
            with open(self.qa_state_file, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Erro ao salvar estado QA: {e}")

    def notify_test_start(self, suite_name: str, suite_type: str, metadata: Dict = None):
        """
        Notifica inicio de suite de testes.

        Args:
            suite_name: Nome da suite
            suite_type: Tipo da suite (smoke, regression, etc)
            metadata: Metadados adicionais
        """
        notification = TestSuiteNotification(
            suite_name=suite_name,
            suite_type=suite_type,
            status="started",
            metadata=metadata or {}
        )

        # Salvar notificacao
        state = self._load_state()
        state["last_run"] = notification.to_dict()
        state["current_suite"] = {
            "name": suite_name,
            "type": suite_type,
            "started_at": notification.timestamp.isoformat(),
            "status": "running"
        }
        self._save_state(state)

        # Log de atividade
        try:
            from factory.core.activity_logger import log_activity
            log_activity(
                "QA",
                "task_start",
                f"Suite {suite_name} iniciada",
                description=f"Executando {suite_type} tests",
                details={"suite_type": suite_type}
            )
        except:
            pass

        print(f"[QA Bridge] Suite {suite_name} ({suite_type}) iniciada")

    def notify_test_complete(self, report: Dict[str, Any]):
        """
        Notifica conclusao de suite de testes.

        Args:
            report: Relatorio completo dos testes
        """
        suite_name = report.get("suite_name", "Unknown")
        summary = report.get("summary", {})

        # Atualizar estado
        state = self._load_state()
        state["last_run"]["status"] = "completed"
        state["last_run"]["completed_at"] = datetime.utcnow().isoformat()
        state["last_run"]["summary"] = summary
        state["current_suite"]["status"] = "completed"
        state["current_suite"]["completed_at"] = datetime.utcnow().isoformat()

        # Adicionar falhas pendentes
        failures = report.get("failures", [])
        for failure in failures:
            state["pending_issues"].append({
                "test_id": failure.get("test_id"),
                "test_name": failure.get("test_name"),
                "screen": failure.get("screen"),
                "error_message": failure.get("error_message"),
                "status": "pending_issue"
            })

        self._save_state(state)

        # Log de atividade
        try:
            from factory.core.activity_logger import log_activity
            log_activity(
                "QA",
                "task_complete",
                f"Suite {suite_name} concluida",
                description=f"Passou: {summary.get('passed', 0)}, Falhou: {summary.get('failed', 0)}",
                details=summary
            )
        except:
            pass

        print(f"[QA Bridge] Suite {suite_name} concluida - "
              f"Passed: {summary.get('passed', 0)}, Failed: {summary.get('failed', 0)}")

    def request_agent_assignment(self, issues: List[Dict]) -> List[IssueAssignment]:
        """
        Solicita atribuicao de issues aos agentes responsaveis.

        Args:
            issues: Lista de issues para atribuir

        Returns:
            Lista de IssueAssignment criadas
        """
        assignments = []

        for issue_data in issues:
            issue_number = issue_data.get("issue_number")
            issue_url = issue_data.get("url", "")
            agent_type = issue_data.get("agent_assigned", "QA")
            priority = issue_data.get("priority", "medium")

            assignment = IssueAssignment(
                issue_number=issue_number,
                issue_url=issue_url,
                agent_type=agent_type,
                priority=priority,
                status="pending"
            )

            # Criar task para o agente
            task = self._create_task_for_agent(assignment, issue_data)

            if task:
                assignment.status = "assigned"
                assignment.assigned_at = datetime.utcnow()
                assignment.task_id = task.get("task_id")

            assignments.append(assignment)
            self.pending_assignments.append(assignment)

        # Atualizar estado
        state = self._load_state()
        state["pending_assignments"] = [a.to_dict() for a in self.pending_assignments]
        self._save_state(state)

        # Notificar orquestrador
        self._notify_orchestrator(assignments)

        return assignments

    def _create_task_for_agent(self, assignment: IssueAssignment, issue_data: Dict) -> Optional[Dict]:
        """
        Cria task para o agente processar.

        Args:
            assignment: Atribuicao de issue
            issue_data: Dados da issue

        Returns:
            Dict com dados da task ou None
        """
        task_id = f"task_{assignment.agent_type}_{int(datetime.utcnow().timestamp())}"

        task = {
            "task_id": task_id,
            "agent_type": assignment.agent_type,
            "issue_number": assignment.issue_number,
            "title": f"Fix Issue #{assignment.issue_number}",
            "description": issue_data.get("error_message", ""),
            "priority": assignment.priority,
            "context": {
                "issue_url": assignment.issue_url,
                "screen": issue_data.get("screen"),
                "error_type": issue_data.get("error_type"),
                "test_name": issue_data.get("test_name"),
                "steps_to_reproduce": issue_data.get("steps_to_reproduce", [])
            },
            "created_at": datetime.utcnow().isoformat(),
            "status": "pending",
            "source": "qa_regression"
        }

        # Salvar task
        try:
            task_file = self.tasks_path / f"{assignment.agent_type}_task.json"
            with open(task_file, 'w', encoding='utf-8') as f:
                json.dump(task, f, indent=2, ensure_ascii=False)
            print(f"[QA Bridge] Task criada para {assignment.agent_type}: {task_id}")
            return task
        except Exception as e:
            print(f"[QA Bridge] Erro ao criar task: {e}")
            return None

    def _notify_orchestrator(self, assignments: List[IssueAssignment]):
        """
        Notifica orquestrador sobre novas atribuicoes.

        Args:
            assignments: Lista de atribuicoes
        """
        # Criar arquivo de notificacao para o orquestrador
        notification = {
            "type": "qa_issue_assignments",
            "timestamp": datetime.utcnow().isoformat(),
            "assignments": [a.to_dict() for a in assignments],
            "summary": {
                "total": len(assignments),
                "by_agent": {}
            }
        }

        # Contar por agente
        for a in assignments:
            if a.agent_type not in notification["summary"]["by_agent"]:
                notification["summary"]["by_agent"][a.agent_type] = 0
            notification["summary"]["by_agent"][a.agent_type] += 1

        # Salvar notificacao
        try:
            notification_file = self.state_path / "qa_notifications.json"
            with open(notification_file, 'w', encoding='utf-8') as f:
                json.dump(notification, f, indent=2, ensure_ascii=False)
        except:
            pass

        # Log de atividade
        try:
            from factory.core.activity_logger import log_activity
            for agent_type, count in notification["summary"]["by_agent"].items():
                log_activity(
                    "QA",
                    "handoff_out",
                    f"Handoff para {agent_type}",
                    description=f"{count} issues atribuidas",
                    details={"agent": agent_type, "count": count}
                )
        except:
            pass

    def get_assignment_status(self) -> Dict[str, Any]:
        """
        Retorna status atual das atribuicoes.

        Returns:
            Dict com status das atribuicoes
        """
        state = self._load_state()

        # Verificar resultados dos agentes
        completed = 0
        in_progress = 0
        pending = 0

        for assignment in self.pending_assignments:
            result_file = self.results_path / f"{assignment.agent_type}_result.json"

            if result_file.exists():
                try:
                    with open(result_file, 'r', encoding='utf-8') as f:
                        result = json.load(f)

                    if result.get("task_id") == assignment.task_id:
                        if result.get("status") == "completed":
                            assignment.status = "completed"
                            assignment.completed_at = datetime.utcnow()
                            completed += 1
                        elif result.get("status") == "failed":
                            assignment.status = "failed"
                            completed += 1  # Conta como processado
                        else:
                            in_progress += 1
                    else:
                        pending += 1
                except:
                    pending += 1
            else:
                pending += 1

        return {
            "total_assignments": len(self.pending_assignments),
            "completed": completed,
            "in_progress": in_progress,
            "pending": pending,
            "assignments": [a.to_dict() for a in self.pending_assignments],
            "last_check": datetime.utcnow().isoformat()
        }

    def wait_for_completions(self, timeout_seconds: int = 3600) -> Dict[str, Any]:
        """
        Aguarda conclusao das atribuicoes.

        Args:
            timeout_seconds: Timeout em segundos

        Returns:
            Status final
        """
        import time
        start = time.time()

        while time.time() - start < timeout_seconds:
            status = self.get_assignment_status()

            if status["pending"] == 0 and status["in_progress"] == 0:
                return {
                    "completed": True,
                    "status": status,
                    "duration_seconds": int(time.time() - start)
                }

            # Aguardar antes de verificar novamente
            time.sleep(30)

        return {
            "completed": False,
            "status": self.get_assignment_status(),
            "timeout": True,
            "duration_seconds": timeout_seconds
        }

    def generate_qa_report(self) -> Dict[str, Any]:
        """
        Gera relatorio completo do QA.

        Returns:
            Dict com relatorio
        """
        state = self._load_state()
        status = self.get_assignment_status()

        return {
            "summary": {
                "last_run": state.get("last_run"),
                "total_issues_found": len(state.get("pending_issues", [])),
                "issues_assigned": status["total_assignments"],
                "issues_completed": status["completed"],
                "issues_pending": status["pending"]
            },
            "current_suite": state.get("current_suite"),
            "pending_issues": state.get("pending_issues", []),
            "assignment_status": status,
            "timestamp": datetime.utcnow().isoformat()
        }


# Instancia global
_qa_bridge: Optional[QAOrchestratorBridge] = None


def get_qa_bridge(base_path: str = None) -> QAOrchestratorBridge:
    """Retorna instancia global do QAOrchestratorBridge."""
    global _qa_bridge
    if _qa_bridge is None:
        _qa_bridge = QAOrchestratorBridge(base_path)
    return _qa_bridge


if __name__ == "__main__":
    # Demo: Testar bridge
    bridge = QAOrchestratorBridge()

    print("\n=== DEMO: QA Orchestrator Bridge ===\n")

    # Notificar inicio
    bridge.notify_test_start("Smoke Tests", "smoke", {"environment": "staging"})

    # Simular relatorio
    report = {
        "suite_name": "Smoke Tests",
        "suite_type": "smoke",
        "summary": {
            "total": 10,
            "passed": 8,
            "failed": 2
        },
        "failures": [
            {
                "test_id": "TEST-001",
                "test_name": "test_login",
                "screen": "login",
                "error_message": "Timeout ao clicar no botao"
            },
            {
                "test_id": "TEST-002",
                "test_name": "test_dashboard",
                "screen": "dashboard",
                "error_message": "Elemento nao encontrado"
            }
        ]
    }

    # Notificar conclusao
    bridge.notify_test_complete(report)

    # Solicitar atribuicao
    issues = [
        {
            "issue_number": 500,
            "url": "https://github.com/test/repo/issues/500",
            "agent_assigned": "FRONT",
            "priority": "high",
            "error_message": "Timeout ao clicar no botao",
            "screen": "login"
        },
        {
            "issue_number": 501,
            "url": "https://github.com/test/repo/issues/501",
            "agent_assigned": "BACK",
            "priority": "medium",
            "error_message": "Elemento nao encontrado",
            "screen": "dashboard"
        }
    ]

    assignments = bridge.request_agent_assignment(issues)
    print(f"\nAtribuicoes criadas: {len(assignments)}")

    # Verificar status
    status = bridge.get_assignment_status()
    print(f"Status: {status['completed']} completed, {status['pending']} pending")

    # Gerar relatorio
    qa_report = bridge.generate_qa_report()
    print(f"\nRelatorio QA:")
    print(f"  Issues encontradas: {qa_report['summary']['total_issues_found']}")
    print(f"  Issues atribuidas: {qa_report['summary']['issues_assigned']}")
