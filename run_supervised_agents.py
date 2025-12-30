#!/usr/bin/env python3
"""
Supervised Agent Runner - Executa agentes com monitoramento e recovery

Uso:
    python run_supervised_agents.py                    # Executar todas issues pendentes
    python run_supervised_agents.py --issues 136,137  # Executar issues especificas
    python run_supervised_agents.py --status          # Ver status dos agentes
    python run_supervised_agents.py --retry-failed    # Re-executar tarefas falhadas
    python run_supervised_agents.py --force TASK-ID   # Forcar re-execucao
"""
import asyncio
import argparse
import json
import os
import sys
import subprocess
from datetime import datetime
from typing import List, Dict, Optional

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from factory.core.agent_supervisor import (
    AgentSupervisor,
    AgentTask,
    AgentStatus,
    BatchAgentExecutor,
    get_supervisor
)


# =============================================================================
# CONFIGURACAO
# =============================================================================

ISSUE_CATEGORIES = {
    "security": {
        "label": "P0-Emergencia",
        "priority": 1,
        "timeout": 900,  # 15 min
        "issues": [
            {"id": 136, "title": "Twilio webhook validation", "files": ["factory/api/input_routes.py"]},
            {"id": 137, "title": "Jira HMAC validation", "files": ["factory/integrations/jira_integration.py"]},
            {"id": 138, "title": "Sanitize error messages", "files": ["factory/api/middleware.py"]},
            {"id": 139, "title": "Rate limit fallback", "files": ["factory/api/middleware.py"]},
        ]
    },
    "database": {
        "label": "P1-Critico",
        "priority": 2,
        "timeout": 600,
        "issues": [
            {"id": 140, "title": "Soft delete repository filters", "files": ["factory/database/repositories.py"]},
            {"id": 141, "title": "Database indexes", "files": ["factory/database/models.py"]},
            {"id": 142, "title": "Fix N+1 query story progress", "files": ["factory/database/repositories.py"]},
            {"id": 143, "title": "Transaction boundaries", "files": ["factory/database/connection.py"]},
            {"id": 144, "title": "Fibonacci constraint story_points", "files": ["factory/database/models.py"]},
        ]
    },
    "rbac": {
        "label": "P1-Critico",
        "priority": 2,
        "timeout": 600,
        "issues": [
            {"id": 145, "title": "RBAC cache invalidation", "files": ["factory/auth/rbac.py"]},
            {"id": 146, "title": "Role-persona mapping", "files": ["factory/auth/personas.py"]},
            {"id": 147, "title": "Fix require_permission decorator", "files": ["factory/auth/rbac.py"]},
            {"id": 148, "title": "ABAC policies persist", "files": ["factory/auth/rbac.py"]},
            {"id": 149, "title": "SSO role mapping", "files": ["factory/api/v1/oauth.py"]},
            {"id": 150, "title": "OAuth scopes alignment", "files": ["factory/api/v1/oauth.py"]},
        ]
    },
    "frontend": {
        "label": "P2-Alto",
        "priority": 3,
        "timeout": 600,
        "issues": [
            {"id": 151, "title": "Build integrations menu", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 152, "title": "Voice input button", "files": ["factory/dashboard/static/"]},
            {"id": 153, "title": "File/doc viewers", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 154, "title": "Analytics dashboard charts", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 155, "title": "Tenant selector UI", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 156, "title": "MFA/Security settings UI", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 157, "title": "Billing dashboard integration", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 158, "title": "Upload interface docs/videos", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 159, "title": "Executive KPI dashboard", "files": ["factory/dashboard/app_v6_agile.py"]},
            {"id": 160, "title": "RBAC admin dashboard", "files": ["factory/dashboard/app_v6_agile.py"]},
        ]
    },
    "infrastructure": {
        "label": "P2-Alto",
        "priority": 3,
        "timeout": 600,
        "issues": [
            {"id": 161, "title": "Webhook callbacks", "files": ["factory/api/routes.py"]},
            {"id": 162, "title": "Audit logging permissions", "files": ["factory/auth/rbac.py"]},
            {"id": 163, "title": "Request size limits", "files": ["factory/api/middleware.py"]},
            {"id": 164, "title": "Env var validation startup", "files": ["factory/config.py"]},
            {"id": 165, "title": "Omnichannel input selector", "files": ["factory/api/input_routes.py"]},
        ]
    },
    "integrations": {
        "label": "P3-Medio",
        "priority": 4,
        "timeout": 600,
        "issues": [
            {"id": 166, "title": "Consolidate GitHub integrations", "files": ["factory/integrations/git/"]},
            {"id": 167, "title": "GitLab integration docs", "files": ["docs/"]},
        ]
    }
}


# =============================================================================
# ISSUE PROMPTS
# =============================================================================

def get_issue_prompt(issue: dict, category: dict) -> str:
    """Gera prompt detalhado para uma issue"""
    return f"""
TAREFA: Implementar issue #{issue['id']} - {issue['title']}

CATEGORIA: {category['label']}
ARQUIVOS ALVO: {', '.join(issue['files'])}

INSTRUCOES:
1. Leia os arquivos existentes para entender o contexto
2. Implemente a solucao seguindo os padroes do projeto
3. Adicione testes se aplicavel
4. Nao quebre funcionalidades existentes
5. Use type hints e docstrings

CONTEXTO DO PROJETO:
- Framework: FastAPI + SQLAlchemy
- Auth: JWT + RBAC
- Dashboard: HTML/CSS/JS (sem framework frontend)
- Database: SQLite (dev) / PostgreSQL (prod)

PADRAO DE CODIGO:
- Async/await para operacoes I/O
- Dependency injection do FastAPI
- Repository pattern para database
- Decorators para auth/rbac

RETORNE:
- Lista de arquivos modificados
- Resumo das alteracoes
- Testes adicionados (se aplicavel)
"""


# =============================================================================
# AGENT EXECUTOR
# =============================================================================

class SupervisedAgentRunner:
    """Executa agentes Claude Code com supervisao"""

    def __init__(self, supervisor: AgentSupervisor):
        self.supervisor = supervisor
        self.active_processes: Dict[str, subprocess.Popen] = {}

    async def execute_task(self, task: AgentTask) -> Dict:
        """
        Executa uma tarefa usando Claude Code CLI

        Usa subprocesso para chamar claude com o prompt
        """
        print(f"\n[Runner] Iniciando: {task.task_id}")
        print(f"         Issue: {task.description}")

        # Heartbeat inicial
        self.supervisor.heartbeat(task.task_id, {"stage": "starting"})

        try:
            # Executar claude CLI em modo nao-interativo
            # Nota: Em producao, usar SDK ou API diretamente
            result = await self._run_claude_task(task)

            # Heartbeat final
            self.supervisor.heartbeat(task.task_id, {"stage": "completed", "result": result})

            return result

        except Exception as e:
            print(f"[Runner] ERRO em {task.task_id}: {e}")
            raise

    async def _run_claude_task(self, task: AgentTask) -> Dict:
        """Simula execucao de tarefa Claude"""
        # Em producao, aqui chamaria o SDK Claude ou subprocess do CLI

        # Heartbeat durante execucao
        for i in range(5):
            await asyncio.sleep(1)
            self.supervisor.heartbeat(task.task_id, {
                "stage": "processing",
                "progress": (i + 1) * 20
            })

        # Simular resultado
        return {
            "success": True,
            "files": task.metadata.get("files", []),
            "message": f"Implementado: {task.description}"
        }

    def get_all_issues(self) -> List[Dict]:
        """Retorna todas as issues configuradas"""
        all_issues = []
        for cat_name, category in ISSUE_CATEGORIES.items():
            for issue in category["issues"]:
                all_issues.append({
                    "category": cat_name,
                    "priority": category["priority"],
                    "timeout": category["timeout"],
                    "label": category["label"],
                    **issue
                })
        return sorted(all_issues, key=lambda x: x["priority"])

    def create_tasks_for_issues(self, issue_ids: List[int] = None) -> List[AgentTask]:
        """Cria tarefas supervisionadas para issues"""
        all_issues = self.get_all_issues()

        if issue_ids:
            all_issues = [i for i in all_issues if i["id"] in issue_ids]

        tasks = []
        for issue in all_issues:
            category = ISSUE_CATEGORIES[issue["category"]]
            prompt = get_issue_prompt(issue, category)

            task = self.supervisor.create_task(
                description=f"Issue #{issue['id']}: {issue['title']}",
                prompt=prompt,
                priority=issue["priority"],
                issue_id=f"ISSUE-{issue['id']}",
                github_issue=issue["id"],
                timeout_seconds=issue["timeout"],
                metadata={
                    "category": issue["category"],
                    "files": issue["files"],
                    "label": issue["label"]
                }
            )
            tasks.append(task)

        return tasks


# =============================================================================
# CLI INTERFACE
# =============================================================================

def print_status(supervisor: AgentSupervisor):
    """Imprime status detalhado"""
    status = supervisor.get_status()
    metrics = supervisor.get_metrics()
    running = supervisor.get_running_tasks()
    failed = supervisor.get_failed_tasks()

    print("\n" + "=" * 60)
    print("       AGENT SUPERVISOR - STATUS")
    print("=" * 60)

    print(f"\n[Pool]")
    print(f"  Active Agents: {status['active_agents']}/{status['max_concurrent']}")
    print(f"  Monitoring: {'ON' if status['monitoring_active'] else 'OFF'}")

    print(f"\n[Tasks]")
    for s, count in status['status_counts'].items():
        if count > 0:
            icon = {
                "pending": "[PEND]",
                "running": "[RUN ]",
                "completed": "[ OK ]",
                "failed": "[FAIL]",
                "timeout": "[TIME]",
                "lost_context": "[LOST]",
                "recovered": "[RECV]",
                "cancelled": "[CANC]"
            }.get(s, "[---]")
            print(f"  {icon} {s}: {count}")

    print(f"\n[Metrics]")
    print(f"  Total: {metrics['total_tasks']}")
    print(f"  Success Rate: {metrics['success_rate']:.1f}%")
    print(f"  Avg Duration: {metrics['avg_duration_seconds']:.0f}s")

    if running:
        print(f"\n[Running ({len(running)})]")
        for task in running[:10]:
            health_icon = {
                "healthy": "[OK]",
                "caution": "[!!]",
                "warning": "[??]",
                "critical": "[XX]"
            }.get(task["health"], "[--]")
            print(f"  {health_icon} {task['task_id'][:20]}... | {task['elapsed_seconds']:.0f}s | {task['description'][:30]}...")

    if failed:
        print(f"\n[Failed ({len(failed)})]")
        for task in failed[:5]:
            print(f"  [X] {task['task_id'][:20]}... | {task['description'][:40]}...")
            print(f"      Erro: {task['error_message'][:50]}...")

    print("\n" + "=" * 60)


async def run_agents(
    supervisor: AgentSupervisor,
    issue_ids: List[int] = None,
    max_concurrent: int = 8
):
    """Executa agentes para implementar issues"""
    runner = SupervisedAgentRunner(supervisor)

    # Criar tarefas
    tasks = runner.create_tasks_for_issues(issue_ids)
    print(f"\n[Supervisor] Criadas {len(tasks)} tarefas")

    # Iniciar monitoramento
    await supervisor.start_monitoring()

    try:
        # Criar executor em lote
        executor = BatchAgentExecutor(supervisor, max_concurrent)

        # Converter para formato de batch
        task_dicts = [
            {
                "description": t.description,
                "prompt": t.prompt,
                "priority": t.priority,
                "issue_id": t.issue_id,
                "github_issue": t.github_issue,
                "timeout_seconds": t.timeout_seconds,
                "metadata": t.metadata
            }
            for t in tasks
        ]

        # Executar
        results = await executor.execute_batch(
            task_dicts,
            runner.execute_task
        )

        # Resumo
        summary = executor.get_summary()
        print(f"\n[Supervisor] Execucao concluida:")
        print(f"  Total: {summary['total']}")
        print(f"  Success: {summary['success']}")
        print(f"  Failed: {summary['failed']}")
        print(f"  Success Rate: {summary['success_rate']:.1f}%")

    finally:
        await supervisor.stop_monitoring()


async def retry_failed(supervisor: AgentSupervisor):
    """Re-executa tarefas falhadas"""
    failed = supervisor.get_failed_tasks()
    print(f"\n[Supervisor] {len(failed)} tarefas falhadas encontradas")

    for task_data in failed:
        task_id = task_data["task_id"]
        if task_data["retries"] < task_data["max_retries"]:
            new_task = supervisor.retry_task(task_id)
            if new_task:
                print(f"  ✅ Retry criado: {new_task.task_id}")
        else:
            print(f"  ⚠️ Max retries atingido: {task_id}")


def resume_paused(supervisor: AgentSupervisor):
    """Resume tarefas pausadas ou com contexto perdido"""
    conn = __import__('sqlite3').connect(supervisor.db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT task_id, description FROM agent_tasks
        WHERE status IN ('lost_context', 'timeout')
        ORDER BY priority, created_at
    """)
    tasks = cursor.fetchall()
    conn.close()

    if not tasks:
        print("\nNenhuma tarefa pausada encontrada.")
        return

    print(f"\n=== TAREFAS PAUSADAS ({len(tasks)}) ===\n")
    for task_id, desc in tasks:
        print(f"  {task_id}: {desc[:50]}...")

        # Gerar prompt de resumo
        prompt = supervisor.get_resume_prompt(task_id)
        if prompt:
            print(f"    [Estado salvo disponivel]")

            # Perguntar se quer resumir
            resp = input(f"    Resumir esta tarefa? (s/N): ").strip().lower()
            if resp == 's':
                new_task = supervisor.retry_task(task_id)
                if new_task:
                    print(f"    -> Nova tarefa criada: {new_task.task_id}")
        print()


def main():
    parser = argparse.ArgumentParser(description="Supervised Agent Runner")
    parser.add_argument("--issues", type=str, help="IDs das issues (ex: 136,137,138)")
    parser.add_argument("--status", action="store_true", help="Mostrar status")
    parser.add_argument("--retry-failed", action="store_true", help="Re-executar falhadas")
    parser.add_argument("--force", type=str, help="Forcar re-execucao de task ID")
    parser.add_argument("--max-concurrent", type=int, default=8, help="Max agentes paralelos")
    parser.add_argument("--list-issues", action="store_true", help="Listar issues disponiveis")
    parser.add_argument("--resume", action="store_true", help="Resumir tarefas pausadas")
    parser.add_argument("--pause-low", type=int, help="Pausar N agentes de baixa prioridade")
    parser.add_argument("--context-level", type=float, help="Reportar nivel de contexto (0.0-1.0)")

    args = parser.parse_args()

    supervisor = get_supervisor()

    if args.status:
        print_status(supervisor)

    elif args.list_issues:
        runner = SupervisedAgentRunner(supervisor)
        issues = runner.get_all_issues()
        print(f"\n=== ISSUES DISPONIVEIS ({len(issues)}) ===\n")
        for issue in issues:
            print(f"  [{issue['label']}] #{issue['id']}: {issue['title']}")
            print(f"      Files: {', '.join(issue['files'])}")

    elif args.retry_failed:
        asyncio.run(retry_failed(supervisor))

    elif args.resume:
        resume_paused(supervisor)

    elif args.pause_low:
        paused = supervisor.pause_lowest_priority_agents(args.pause_low)
        print(f"\n{len(paused)} agentes pausados:")
        for task_id in paused:
            print(f"  - {task_id}")
        print("\nEstados salvos. Use --resume para retomar.")

    elif args.context_level is not None:
        new_limit = supervisor.adjust_concurrency_for_context(args.context_level)
        print(f"\nContexto: {args.context_level*100:.0f}%")
        print(f"Limite de agentes ajustado para: {new_limit}")

    elif args.force:
        new_task = supervisor.force_restart(args.force)
        if new_task:
            print(f"Force restart criado: {new_task.task_id}")
        else:
            print(f"Nao foi possivel forcar restart de {args.force}")

    else:
        issue_ids = None
        if args.issues:
            issue_ids = [int(i.strip()) for i in args.issues.split(",")]

        asyncio.run(run_agents(
            supervisor,
            issue_ids=issue_ids,
            max_concurrent=args.max_concurrent
        ))


if __name__ == "__main__":
    main()
