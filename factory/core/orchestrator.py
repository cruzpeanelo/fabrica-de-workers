# -*- coding: utf-8 -*-
"""
Agent Orchestrator - Fabrica de Agentes
=======================================
Loop principal que coordena todos os agentes.

O orquestrador:
1. Verifica GitHub por novas issues a cada 30s
2. Classifica e distribui issues para agentes
3. Gerencia execucao paralela de agentes
4. Processa handoffs entre agentes
5. Monitora contexto e dispara auto-compact
6. Persiste estado em JSON

Author: Fabrica de Agentes
"""

import os
import sys
import json
import asyncio
import logging
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field, asdict

# Adicionar diretorio pai ao path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.config.automation import AutomationConfig, get_all_agents
from factory.core.agent_runner import AgentRunner, AgentPool
from factory.core.context_manager import ContextManager
from factory.core.handoff_manager import HandoffManager
from factory.core.runtime_manager import (
    RuntimeManager, RuntimeConfig, RuntimeStatus,
    format_duration, format_time_remaining
)

logger = logging.getLogger(__name__)

# Diretorio base
BASE_DIR = Path(__file__).parent.parent.parent
STATE_DIR = BASE_DIR / "factory" / "state"
STATE_DIR.mkdir(exist_ok=True)


@dataclass
class OrchestratorState:
    """Estado persistido do orquestrador."""
    status: str = "stopped"  # stopped | running | paused
    started_at: Optional[str] = None
    last_poll: Optional[str] = None
    issues_processed: int = 0
    tasks_distributed: int = 0
    errors_count: int = 0
    current_cycle: int = 0

    # Runtime info
    runtime_duration: Optional[int] = None  # segundos configurados
    runtime_elapsed: int = 0  # segundos decorridos
    runtime_remaining: Optional[int] = None  # segundos restantes
    runtime_end_time: Optional[str] = None  # hora de termino prevista

    def save(self):
        """Salva estado em arquivo JSON."""
        path = STATE_DIR / "orchestrator_state.json"
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(asdict(self), f, indent=2, ensure_ascii=False)

    @classmethod
    def load(cls) -> "OrchestratorState":
        """Carrega estado de arquivo JSON."""
        path = STATE_DIR / "orchestrator_state.json"
        if path.exists():
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                return cls(**data)
        return cls()


class AgentOrchestrator:
    """
    Loop principal que coordena todos os agentes.

    Uso:
        orchestrator = AgentOrchestrator(mode="autonomous")
        await orchestrator.run_forever()
    """

    # Keywords para classificacao de issues
    CLASSIFICATION_RULES = {
        "[SEC]": ['security', 'auth', 'login', 'jwt', 'csrf', 'xss',
                  'cors', 'permission', 'rbac', 'vulnerability', 'encryption',
                  'mfa', '2fa', 'token', 'password', 'oauth'],
        "[DEVOPS]": ['docker', 'kubernetes', 'k8s', 'ci/cd', 'deploy',
                     'infra', 'monitoring', 'terraform', 'helm', 'redis',
                     'postgres', 'health check', 'migration', 'scaling'],
        "[FRONT]": ['ui', 'ux', 'frontend', 'component', 'mobile',
                    'pwa', 'css', 'theme', 'dark mode', 'responsive',
                    'animation', 'modal', 'button', 'form'],
        "[QA]": ['test', 'pytest', 'coverage', 'validar', 'testar',
                 'e2e', 'unit test', 'quality'],
        "[ARCH]": ['architect', 'design', 'pattern', 'refactor',
                   'structure', 'reorganiz'],
        "[PROD]": ['feature', 'user story', 'roadmap', 'backlog',
                   'requirement', 'acceptance'],
        "[INOV]": ['research', 'poc', 'experiment', 'ml', 'ai',
                   'nlp', 'benchmark', 'innovation'],
        "[FIN]": ['cost', 'pricing', 'billing', 'subscription',
                  'revenue', 'financial', 'metering'],
        "[GROWTH]": ['marketing', 'launch', 'go-to-market', 'sales',
                     'acquisition', 'retention'],
        "[ORCH]": ['coordinate', 'review', 'validate', 'approve'],
    }

    def __init__(self, mode: str = "autonomous", config: AutomationConfig = None):
        """
        Inicializa orquestrador.

        Args:
            mode: Modo de operacao (autonomous | supervised | manual)
            config: Configuracao de automacao (opcional)
        """
        self.mode = mode
        self.config = config or AutomationConfig.load()
        self.config.mode = mode

        self.state = OrchestratorState.load()
        self.agent_pool = AgentPool()
        self.context_manager = ContextManager()
        self.handoff_manager = HandoffManager()

        # Runtime Manager para controle de duracao
        self.runtime: Optional[RuntimeManager] = None

        self._running = False
        self._paused = False
        self._processed_issues: set = set()

    def classify_issue(self, title: str) -> str:
        """
        Classifica issue baseado no titulo.

        Returns:
            Prefixo do agente (ex: "[BACK]")
        """
        t = title.lower()

        for prefix, keywords in self.CLASSIFICATION_RULES.items():
            if any(k in t for k in keywords):
                return prefix

        # Default: Backend
        return "[BACK]"

    async def check_github_issues(self) -> List[Dict[str, Any]]:
        """
        Verifica GitHub por novas issues.

        Returns:
            Lista de issues novas nao processadas
        """
        try:
            result = subprocess.run(
                ["gh", "issue", "list", "--state", "open", "--limit", "50",
                 "--json", "number,title,labels,state"],
                capture_output=True,
                text=True,
                cwd=str(BASE_DIR)
            )

            if result.returncode != 0:
                logger.error(f"Erro ao buscar issues: {result.stderr}")
                return []

            issues = json.loads(result.stdout)

            # Filtrar issues ja processadas
            new_issues = [
                issue for issue in issues
                if issue["number"] not in self._processed_issues
            ]

            return new_issues

        except Exception as e:
            logger.error(f"Erro ao verificar GitHub: {e}")
            return []

    async def assign_to_agent(self, agent_prefix: str, issue: Dict[str, Any]):
        """
        Atribui issue a um agente.

        Args:
            agent_prefix: Prefixo do agente (ex: "[BACK]")
            issue: Dados da issue
        """
        agent_id = agent_prefix.strip("[]")
        runner = self.agent_pool.get_runner(agent_id)

        if not runner:
            logger.warning(f"Agente {agent_id} nao encontrado")
            return

        task = f"""Processar issue #{issue['number']}: {issue['title']}

Acesse a issue no GitHub para obter detalhes:
gh issue view {issue['number']}

Implemente a solucao, faca os commits necessarios e feche a issue quando concluido:
gh issue close {issue['number']} -c "Implementado"
"""

        logger.info(f"Atribuindo issue #{issue['number']} para {agent_id}")

        # Em modo autonomo, executar diretamente
        if self.mode == "autonomous":
            result = await runner.run_task(task)

            if result["success"]:
                self._processed_issues.add(issue["number"])
                self.state.issues_processed += 1

                # Criar handoff para QA
                self.handoff_manager.process_task_completion(
                    from_agent=agent_prefix,
                    task_description=f"Revisar issue #{issue['number']}",
                    status="on_complete",
                    issue_number=issue["number"]
                )
        else:
            # Em modo supervisionado, adicionar a fila para aprovacao
            logger.info(f"Issue #{issue['number']} aguardando aprovacao")

        self.state.tasks_distributed += 1
        self.state.save()

    async def process_handoffs(self):
        """Processa handoffs pendentes."""
        pending = self.handoff_manager.get_all_pending()

        for handoff in pending:
            agent_id = handoff.to_agent.strip("[]")
            runner = self.agent_pool.get_runner(agent_id)

            if not runner:
                continue

            # Verificar se agente esta disponivel
            status = runner.get_status()
            if status["status"] == "running":
                continue

            # Marcar como em progresso
            self.handoff_manager.mark_in_progress(handoff.handoff_id)

            # Executar tarefa
            task = f"""Handoff de {handoff.from_agent}:

{handoff.task_description}

Issue: #{handoff.issue_number if handoff.issue_number else 'N/A'}
Contexto: {json.dumps(handoff.context, indent=2) if handoff.context else 'N/A'}
"""

            result = await runner.run_task(task)

            if result["success"]:
                self.handoff_manager.mark_completed(handoff.handoff_id)
            else:
                self.handoff_manager.mark_failed(
                    handoff.handoff_id,
                    result.get("error")
                )

    async def check_context_usage(self):
        """Verifica uso de contexto e compacta se necessario."""
        for agent_id, runner in self.agent_pool.runners.items():
            usage = self.context_manager.get_usage(agent_id)

            if self.context_manager.should_compact(agent_id):
                logger.info(f"[{agent_id}] Contexto em {usage:.0%}, compactando...")

                summary = f"Agente {agent_id} completou {runner.state.tasks_completed} tarefas"
                await self.context_manager.compact_agent_context(
                    agent_id=agent_id,
                    current_summary=summary,
                    agent_runner=runner
                )

    async def run_cycle(self):
        """Executa um ciclo do orquestrador."""
        self.state.current_cycle += 1
        self.state.last_poll = datetime.utcnow().isoformat()

        try:
            # 1. Verificar novas issues
            new_issues = await self.check_github_issues()

            # 2. Classificar e distribuir
            for issue in new_issues:
                # Verificar se ja tem prefixo
                title = issue["title"]
                if title.startswith("["):
                    prefix = title.split("]")[0] + "]"
                else:
                    prefix = self.classify_issue(title)

                await self.assign_to_agent(prefix, issue)

            # 3. Processar handoffs
            await self.process_handoffs()

            # 4. Verificar contexto
            await self.check_context_usage()

        except Exception as e:
            self.state.errors_count += 1
            logger.error(f"Erro no ciclo: {e}")

        self.state.save()

    def _init_runtime(self):
        """Inicializa RuntimeManager se duracao configurada."""
        runtime_config = RuntimeConfig(
            duration_seconds=self.config.runtime_duration,
            graceful_shutdown_delay=self.config.graceful_shutdown_delay,
            warn_before_shutdown=self.config.warn_before_shutdown,
            warn_intervals=self.config.warn_intervals,
            state_file=str(STATE_DIR / "runtime_state.json")
        )
        self.runtime = RuntimeManager(runtime_config)

        # Registrar callback de aviso
        def on_warning(remaining: int):
            logger.warning(f"Runtime warning: {format_time_remaining(remaining)} restantes")

        self.runtime.on_warning(on_warning)
        self.runtime.start()

        # Atualizar estado
        self.state.runtime_duration = self.config.runtime_duration
        if self.runtime.end_time:
            self.state.runtime_end_time = self.runtime.end_time.isoformat()

    def _update_runtime_state(self):
        """Atualiza estado do runtime."""
        if self.runtime and self.runtime.is_running():
            self.state.runtime_elapsed = self.runtime.get_elapsed()
            self.state.runtime_remaining = self.runtime.check_time_remaining()

    async def run_forever(self):
        """
        Loop infinito enquanto computador estiver ligado.

        Para parar, use orchestrator.stop()
        Ou configure runtime_duration para parar automaticamente.
        """
        self._running = True
        self.state.status = "running"
        self.state.started_at = datetime.utcnow().isoformat()

        # Inicializar runtime manager
        self._init_runtime()

        if self.config.runtime_duration:
            duration_str = format_duration(self.config.runtime_duration)
            end_time = self.runtime.end_time.strftime('%Y-%m-%d %H:%M:%S')
            logger.info(f"Orquestrador iniciado em modo {self.mode} por {duration_str}")
            logger.info(f"Termino previsto: {end_time}")
        else:
            logger.info(f"Orquestrador iniciado em modo {self.mode} (ilimitado)")

        self.state.save()

        while self._running:
            # Verificar se deve encerrar por tempo
            if self.runtime and self.runtime.should_shutdown():
                logger.info("Tempo de execucao atingido, parando...")
                break

            # Verificar avisos
            if self.runtime:
                self.runtime.check_warnings()

            if not self._paused:
                await self.run_cycle()
                self._update_runtime_state()

            await asyncio.sleep(self.config.poll_interval)

        # Parar runtime e salvar estado
        if self.runtime:
            summary = self.runtime.stop()
            logger.info(f"Resumo: {summary}")

        self.state.status = "stopped"
        self.state.save()
        logger.info("Orquestrador parado")

    def pause(self):
        """Pausa o orquestrador."""
        self._paused = True
        self.state.status = "paused"
        self.state.save()
        logger.info("Orquestrador pausado")

    def resume(self):
        """Retoma o orquestrador."""
        self._paused = False
        self.state.status = "running"
        self.state.save()
        logger.info("Orquestrador retomado")

    async def stop(self):
        """Para o orquestrador."""
        self._running = False
        await self.agent_pool.stop_all()
        logger.info("Orquestrador parando...")

    def get_status(self) -> Dict[str, Any]:
        """Retorna status do orquestrador."""
        status = {
            "status": self.state.status,
            "mode": self.mode,
            "started_at": self.state.started_at,
            "last_poll": self.state.last_poll,
            "current_cycle": self.state.current_cycle,
            "issues_processed": self.state.issues_processed,
            "tasks_distributed": self.state.tasks_distributed,
            "errors_count": self.state.errors_count,
            "agents": self.agent_pool.get_all_status(),
            "handoffs": self.handoff_manager.get_stats()
        }

        # Adicionar info de runtime se disponivel
        if self.runtime:
            status["runtime"] = self.runtime.get_status_dict()
        else:
            status["runtime"] = {
                "duration_configured": self.state.runtime_duration,
                "elapsed_seconds": self.state.runtime_elapsed,
                "remaining_seconds": self.state.runtime_remaining,
                "end_time": self.state.runtime_end_time,
                "is_unlimited": self.state.runtime_duration is None
            }

        return status


# Script para executar diretamente
async def main():
    """Ponto de entrada principal."""
    import argparse
    from factory.core.runtime_manager import parse_duration

    parser = argparse.ArgumentParser(
        description="Fabrica de Agentes - Orquestrador (Async)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
  python orchestrator.py                        # Autonomo ilimitado
  python orchestrator.py -m autonomous -t 2h    # Autonomo por 2 horas
  python orchestrator.py -m autonomous -t 8h    # Autonomo por 8 horas
        """
    )
    parser.add_argument(
        "--mode", "-m",
        choices=["autonomous", "supervised", "manual"],
        default="autonomous",
        help="Modo de operacao"
    )
    parser.add_argument(
        "--duration", "-t",
        type=str,
        default="unlimited",
        help="Duracao da execucao (ex: 1h, 2h, 8h, 24h)"
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Ativar modo debug"
    )

    args = parser.parse_args()

    # Configurar logging
    logging.basicConfig(
        level=logging.DEBUG if args.debug else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s"
    )

    # Carregar configuracao e aplicar duracao
    config = AutomationConfig.load()
    config.runtime_duration = parse_duration(args.duration)

    # Criar e executar orquestrador
    orchestrator = AgentOrchestrator(mode=args.mode, config=config)

    try:
        await orchestrator.run_forever()
    except KeyboardInterrupt:
        print("\nParando orquestrador...")
        await orchestrator.stop()


if __name__ == "__main__":
    asyncio.run(main())
