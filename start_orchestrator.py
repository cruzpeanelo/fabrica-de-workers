#!/usr/bin/env python3
"""
Start Orchestrator - Script principal do Agente Orquestrador.

Este script inicia o Agente Orquestrador que:
1. Pergunta o modo de operacao ao usuario
2. Carrega Skills, Hooks e MCPs
3. Monitora GitHub por novas issues
4. Classifica e distribui tasks para agentes
5. Spawna terminais Git Bash para cada agente
6. Processa handoffs entre agentes
7. Gerencia o ciclo de vida dos terminais

Uso:
    python start_orchestrator.py                        # Pergunta modo ao iniciar
    python start_orchestrator.py --mode autonomous      # Modo autonomo direto
    python start_orchestrator.py --mode supervised      # Modo supervisionado
    python start_orchestrator.py --mode interactive     # Modo interativo
    python start_orchestrator.py --spawn BACK           # Spawna agente especifico
    python start_orchestrator.py -m autonomous -d 2h    # Autonomo por 2 horas
    python start_orchestrator.py -m autonomous -d 8h    # Autonomo por 8 horas
    python start_orchestrator.py -m autonomous -d 24h   # Autonomo por 24 horas
"""

import argparse
import os
import sys
import json
import time
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Optional, Dict, List, Callable
from dataclasses import dataclass, field

# Adicionar factory ao path
sys.path.insert(0, str(Path(__file__).parent))

from factory.core.terminal_spawner import TerminalSpawner, TaskMessage, create_task
from factory.core.runtime_manager import (
    RuntimeManager, RuntimeConfig, RuntimeStatus,
    parse_duration, format_duration, format_time_remaining,
    DURATION_PRESETS
)


# Diretorio base do projeto
BASE_PATH = Path(__file__).parent
PROMPTS_PATH = BASE_PATH / "prompts"
STATE_PATH = BASE_PATH / "factory" / "state"
CONFIG_PATH = BASE_PATH / "factory" / "config"


# Modos de operacao
class OperationMode:
    AUTONOMOUS = "autonomous"      # Sem intervencao humana
    SUPERVISED = "supervised"      # Pede confirmacao em acoes criticas
    INTERACTIVE = "interactive"    # Menu manual para cada acao


# Regras de classificacao de issues por keywords
CLASSIFICATION_RULES = {
    "BACK": ["api", "backend", "endpoint", "database", "model", "repository", "service"],
    "FRONT": ["ui", "frontend", "component", "css", "html", "mobile", "responsive"],
    "DEVOPS": ["docker", "k8s", "kubernetes", "ci", "cd", "deploy", "infra", "terraform"],
    "SEC": ["security", "auth", "jwt", "oauth", "csrf", "xss", "vulnerability"],
    "QA": ["test", "pytest", "coverage", "e2e", "quality"],
    "ARCH": ["architecture", "design", "refactor", "structure", "adr"],
    "PROD": ["feature", "story", "user story", "requirement", "backlog"],
    "INOV": ["research", "poc", "benchmark", "innovation", "trend"],
    "FIN": ["cost", "pricing", "budget", "roi", "financial"],
    "GROWTH": ["marketing", "launch", "acquisition", "growth", "campaign"],
}

# Regras de handoff
HANDOFF_RULES = {
    "BACK": {"on_complete": "QA", "on_security": "SEC", "on_needs_frontend": "FRONT"},
    "FRONT": {"on_complete": "QA", "on_needs_api": "BACK"},
    "QA": {"on_pass": "DEVOPS", "on_fail_backend": "BACK", "on_fail_frontend": "FRONT"},
    "DEVOPS": {"on_complete": "ORCH", "on_security": "SEC"},
    "SEC": {"on_complete": "QA"},
    "ARCH": {"on_backend": "BACK", "on_frontend": "FRONT", "on_devops": "DEVOPS"},
    "PROD": {"on_design": "ARCH", "on_implement": "BACK"},
    "INOV": {"on_approve": "ARCH", "on_implement": "BACK"},
    "FIN": {"on_optimize": "DEVOPS", "on_pricing": "PROD"},
    "GROWTH": {"on_landing": "FRONT", "on_analytics": "BACK"},
}


@dataclass
class OrchestratorConfig:
    """Configuracao do orquestrador."""
    mode: str = OperationMode.INTERACTIVE
    poll_interval: int = 30
    max_concurrent_agents: int = 5
    auto_commit: bool = True
    auto_handoff: bool = True
    require_confirmation: List[str] = field(default_factory=lambda: [
        "delete", "force_push", "deploy_prod"
    ])
    skills_enabled: bool = True
    hooks_enabled: bool = True
    mcp_enabled: bool = True

    # Runtime Duration - duracao da execucao autonoma
    runtime_duration: Optional[int] = None  # segundos, None = ilimitado
    graceful_shutdown_delay: int = 60  # segundos para cleanup
    warn_before_shutdown: int = 300  # aviso 5 min antes
    warn_intervals: List[int] = field(default_factory=lambda: [300, 60, 30, 10])


class Orchestrator:
    """Agente Orquestrador - Coordenador do Squad."""

    def __init__(self, base_path: Optional[str] = None, config: Optional[OrchestratorConfig] = None):
        self.base_path = Path(base_path) if base_path else BASE_PATH
        self.spawner = TerminalSpawner(str(self.base_path))
        self.running = False
        self.config = config or OrchestratorConfig()
        self.mode = self.config.mode

        # Runtime Manager para controle de duracao
        self.runtime: Optional[RuntimeManager] = None

        # Managers (serao carregados se disponiveis)
        self.skill_manager = None
        self.hook_manager = None
        self.mcp_manager = None

        # Carregar managers
        self._load_managers()

    def _load_managers(self):
        """Carrega managers de Skills, Hooks e MCP."""
        # Tentar carregar Skill Manager
        if self.config.skills_enabled:
            try:
                from factory.skills.skill_manager import SkillManager
                self.skill_manager = SkillManager()  # Nao aceita argumentos
                print("  [OK] Skills carregadas")
            except ImportError as e:
                print(f"  [--] Skills nao disponiveis: {e}")
            except Exception as e:
                print(f"  [!!] Erro ao carregar Skills: {e}")

        # Tentar carregar Hook Manager
        if self.config.hooks_enabled:
            try:
                from factory.hooks.hook_manager import HookManager
                self.hook_manager = HookManager(str(self.base_path))
                print("  [OK] Hooks carregados")
            except ImportError as e:
                print(f"  [--] Hooks nao disponiveis: {e}")
            except Exception as e:
                print(f"  [!!] Erro ao carregar Hooks: {e}")

        # Tentar carregar MCP Manager
        if self.config.mcp_enabled:
            try:
                from factory.mcp.mcp_manager import MCPManager
                self.mcp_manager = MCPManager(str(self.base_path))
                print("  [OK] MCP carregado")
            except ImportError as e:
                print(f"  [--] MCP nao disponivel: {e}")
            except Exception as e:
                print(f"  [!!] Erro ao carregar MCP: {e}")

    def trigger_hook(self, event: str, context: Dict = None):
        """Dispara um hook se disponivel."""
        if self.hook_manager:
            self.hook_manager.trigger(event, context or {})

    def execute_skill(self, skill_id: str, params: Dict = None) -> Dict:
        """Executa uma skill se disponivel."""
        if self.skill_manager:
            return self.skill_manager.execute_skill(skill_id, params)
        return {"success": False, "error": "SkillManager nao disponivel"}

    def print_banner(self):
        """Exibe banner do orquestrador."""
        banner = """
+---------------------------------------------------------------+
|                                                               |
|   FABRICA DE AGENTES                                          |
|   Sistema de Desenvolvimento Autonomo                         |
|                                                               |
|   [ORCH] ORQUESTRADOR - Tech Lead do Squad                    |
|                                                               |
+---------------------------------------------------------------+
        """
        print(banner)
        print(f"  Iniciado em: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"  Git Bash: {self.spawner.git_bash_path or 'NAO ENCONTRADO'}")
        print()
        print("  Carregando componentes...")

    def select_mode_menu(self) -> str:
        """Exibe menu de selecao de modo e retorna o modo escolhido."""
        menu = """
+---------------------------------------------------------------+
|              SELECIONE O MODO DE OPERACAO                     |
+---------------------------------------------------------------+

   [1] AUTONOMO por 1 hora
   [2] AUTONOMO por 2 horas
   [3] AUTONOMO por 4 horas
   [4] AUTONOMO por 8 horas (jornada de trabalho)
   [5] AUTONOMO por 12 horas
   [6] AUTONOMO por 24 horas
   [7] AUTONOMO ilimitado

   [8] SUPERVISIONADO
       - Pede confirmacao antes de commits
       - Review antes de handoffs

   [9] INTERATIVO
       - Menu manual para cada acao
       - Controle total do usuario

+---------------------------------------------------------------+
        """
        print(menu)

        while True:
            choice = input("\n  Escolha o modo [1-9]: ").strip()

            # Modos autonomos com duracao
            if choice == "1":
                self.config.runtime_duration = 3600  # 1h
                return OperationMode.AUTONOMOUS
            elif choice == "2":
                self.config.runtime_duration = 7200  # 2h
                return OperationMode.AUTONOMOUS
            elif choice == "3":
                self.config.runtime_duration = 14400  # 4h
                return OperationMode.AUTONOMOUS
            elif choice == "4":
                self.config.runtime_duration = 28800  # 8h
                return OperationMode.AUTONOMOUS
            elif choice == "5":
                self.config.runtime_duration = 43200  # 12h
                return OperationMode.AUTONOMOUS
            elif choice == "6":
                self.config.runtime_duration = 86400  # 24h
                return OperationMode.AUTONOMOUS
            elif choice == "7":
                self.config.runtime_duration = None  # ilimitado
                return OperationMode.AUTONOMOUS
            elif choice == "8":
                return OperationMode.SUPERVISED
            elif choice == "9":
                return OperationMode.INTERACTIVE
            else:
                print("  Opcao invalida. Digite um numero de 1 a 9.")

    def confirm_action(self, action: str, details: str = "") -> bool:
        """Pede confirmacao do usuario no modo supervisionado."""
        if self.mode == OperationMode.AUTONOMOUS:
            return True

        if self.mode == OperationMode.SUPERVISED:
            print(f"\n  [CONFIRMACAO] {action}")
            if details:
                print(f"  Detalhes: {details}")
            response = input("  Confirmar? [S/n]: ").strip().lower()
            return response != "n"

        return True  # Interactive mode handles this differently

    def print_mode_info(self):
        """Exibe informacoes do modo atual."""
        mode_info = {
            OperationMode.AUTONOMOUS: ("AUTONOMO", "Sem intervencao humana"),
            OperationMode.SUPERVISED: ("SUPERVISIONADO", "Confirmacao em acoes criticas"),
            OperationMode.INTERACTIVE: ("INTERATIVO", "Controle manual completo"),
        }

        name, desc = mode_info.get(self.mode, ("DESCONHECIDO", ""))
        print(f"\n  Modo: {name}")
        print(f"  {desc}")

        # Mostrar duracao se autonomo
        if self.mode == OperationMode.AUTONOMOUS:
            duration = self.config.runtime_duration
            if duration:
                print(f"  Duracao: {format_duration(duration)}")
            else:
                print("  Duracao: ILIMITADA (Ctrl+C para parar)")
        print()

    def print_status(self):
        """Exibe status atual."""
        terminals = self.spawner.get_active_terminals()

        print("\n" + "="*60)
        print(" STATUS DOS AGENTES")
        print("="*60)

        # Mostrar runtime info se disponivel
        if self.runtime and self.runtime.is_running():
            status = self.runtime.get_status_dict()
            print(f"  {self.runtime.print_status_line()}")
            print()

        if terminals:
            for agent, info in terminals.items():
                print(f"  [{agent}] PID: {info.pid} | Status: {info.status} | Task: {info.task_id}")
        else:
            print("  Nenhum agente ativo")

        # Mostrar skills se disponiveis
        if self.skill_manager:
            try:
                skills = self.skill_manager.get_all_skills()
                if skills:
                    skill_names = [s.get("name", s.get("skill_id", "?")) for s in skills[:5]]
                    print(f"\n  Skills disponiveis: {len(skills)} ({', '.join(skill_names)}...)")
            except Exception:
                pass  # Ignora erros de skills

        # Mostrar MCPs se disponiveis
        if self.mcp_manager:
            mcps = self.mcp_manager.list_servers()
            if mcps:
                print("  MCPs ativos:", ", ".join(mcps))

        print("="*60 + "\n")

    def classify_issue(self, title: str, body: str = "") -> str:
        """Classifica uma issue para o agente apropriado."""
        text = f"{title} {body}".lower()

        scores = {agent: 0 for agent in CLASSIFICATION_RULES}

        for agent, keywords in CLASSIFICATION_RULES.items():
            for keyword in keywords:
                if keyword in text:
                    scores[agent] += 1

        # Retornar agente com maior score (default BACK)
        best_agent = max(scores, key=scores.get)
        return best_agent if scores[best_agent] > 0 else "BACK"

    def spawn_agent(self, agent_type: str, title: str, description: str = "",
                    issue_number: Optional[int] = None) -> bool:
        """Spawna um novo terminal para um agente."""
        # Trigger hook pre_spawn
        self.trigger_hook("on_spawn", {
            "agent_type": agent_type,
            "title": title,
            "issue_number": issue_number
        })

        # Confirmacao no modo supervisionado
        if not self.confirm_action(
            f"Spawnar agente [{agent_type}]",
            f"Task: {title}"
        ):
            print("  Acao cancelada pelo usuario")
            return False

        print(f"\nSpawnando agente [{agent_type}]...")

        task = create_task(
            agent_type=agent_type,
            title=title,
            description=description,
            issue_number=issue_number
        )

        result = self.spawner.spawn_terminal(agent_type, task)

        if result:
            print(f"  Terminal criado - PID: {result.pid}")
            return True
        else:
            print(f"  Falha ao criar terminal")
            return False

    def process_handoff(self, from_agent: str, handoff_type: str) -> Optional[str]:
        """Processa um handoff entre agentes."""
        rules = HANDOFF_RULES.get(from_agent, {})
        next_agent = rules.get(handoff_type)

        if next_agent:
            # Trigger hook pre_handoff
            self.trigger_hook("pre_handoff", {
                "from_agent": from_agent,
                "to_agent": next_agent,
                "handoff_type": handoff_type
            })

            print(f"\nHandoff: [{from_agent}] -> [{next_agent}] ({handoff_type})")

            # Trigger hook post_handoff
            self.trigger_hook("post_handoff", {
                "from_agent": from_agent,
                "to_agent": next_agent,
                "handoff_type": handoff_type
            })

            return next_agent
        return None

    def check_results(self):
        """Verifica resultados de agentes."""
        for agent_type in self.spawner.AGENT_TYPES:
            result = self.spawner.read_result(agent_type)

            if result and result.status != "pending":
                # Trigger hook post_task
                self.trigger_hook("post_task", {
                    "agent_type": agent_type,
                    "status": result.status,
                    "task_id": result.task_id
                })

                print(f"\n[{agent_type}] Resultado recebido:")
                print(f"  Status: {result.status}")
                print(f"  Arquivos: {result.files_changed}")

                if result.handoff_to:
                    next_agent = result.handoff_to.strip("[]")
                    if next_agent in self.spawner.AGENT_TYPES:
                        # Criar nova task para proximo agente
                        if self.config.auto_handoff or self.confirm_action(
                            f"Handoff para [{next_agent}]",
                            f"Task anterior: {result.task_id}"
                        ):
                            self.spawn_agent(
                                next_agent,
                                f"Handoff de [{agent_type}]",
                                f"Continuar trabalho iniciado por [{agent_type}]. Task anterior: {result.task_id}"
                            )

                # Limpar resultado processado
                self.spawner.clear_result(agent_type)

    def interactive_menu(self):
        """Menu interativo do orquestrador."""
        while True:
            print("\n" + "-"*50)
            print(" MENU DO ORQUESTRADOR [INTERATIVO]")
            print("-"*50)
            print(" 1. Spawnar agente")
            print(" 2. Ver status")
            print(" 3. Verificar resultados")
            print(" 4. Terminar agente")
            print(" 5. Terminar todos")
            print(" 6. Mudar para modo autonomo")
            print(" 7. Iniciar Claude (modo livre)")
            if self.skill_manager:
                print(" 8. Executar skill")
            if self.mcp_manager:
                print(" 9. Gerenciar MCPs")
            print(" 0. Sair")
            print("-"*50)

            choice = input("\nEscolha: ").strip()

            if choice == "1":
                self.menu_spawn_agent()
            elif choice == "2":
                self.print_status()
            elif choice == "3":
                self.check_results()
            elif choice == "4":
                self.menu_terminate_agent()
            elif choice == "5":
                self.spawner.terminate_all()
                print("Todos os agentes terminados.")
            elif choice == "6":
                self.mode = OperationMode.AUTONOMOUS
                self.run_autonomous()
            elif choice == "7":
                self.run_claude_free()
            elif choice == "8" and self.skill_manager:
                self.menu_execute_skill()
            elif choice == "9" and self.mcp_manager:
                self.menu_manage_mcp()
            elif choice == "0":
                print("\nEncerrando orquestrador...")
                self.spawner.terminate_all()
                self.trigger_hook("on_terminate", {"reason": "user_exit"})
                break
            else:
                print("Opcao invalida")

    def menu_spawn_agent(self):
        """Menu para spawnar agente."""
        print("\nAgentes disponiveis:")
        for i, agent in enumerate(self.spawner.AGENT_TYPES, 1):
            print(f"  {i:2}. [{agent}]")

        choice = input("\nNumero do agente (ou 0 para voltar): ").strip()

        try:
            idx = int(choice)
            if idx == 0:
                return
            if 1 <= idx <= len(self.spawner.AGENT_TYPES):
                agent_type = self.spawner.AGENT_TYPES[idx - 1]
                title = input("Titulo da task: ").strip() or "Task manual"
                description = input("Descricao (opcional): ").strip()

                self.spawn_agent(agent_type, title, description)
        except ValueError:
            print("Entrada invalida")

    def menu_terminate_agent(self):
        """Menu para terminar agente."""
        terminals = self.spawner.get_active_terminals()

        if not terminals:
            print("Nenhum agente ativo")
            return

        print("\nAgentes ativos:")
        agents = list(terminals.keys())
        for i, agent in enumerate(agents, 1):
            info = terminals[agent]
            print(f"  {i}. [{agent}] PID: {info.pid}")

        choice = input("\nNumero do agente (ou 0 para voltar): ").strip()

        try:
            idx = int(choice)
            if idx == 0:
                return
            if 1 <= idx <= len(agents):
                agent = agents[idx - 1]
                self.spawner.terminate_terminal(agent)
                print(f"Agente [{agent}] terminado")
        except ValueError:
            print("Entrada invalida")

    def menu_execute_skill(self):
        """Menu para executar skill."""
        if not self.skill_manager:
            print("Skills nao disponiveis")
            return

        skills = self.skill_manager.list_skills()
        if not skills:
            print("Nenhuma skill registrada")
            return

        print("\nSkills disponiveis:")
        for i, skill in enumerate(skills, 1):
            info = self.skill_manager.get_skill_info(skill)
            desc = info.get("description", "") if info else ""
            print(f"  {i}. {skill} - {desc}")

        choice = input("\nNumero da skill (ou 0 para voltar): ").strip()

        try:
            idx = int(choice)
            if idx == 0:
                return
            if 1 <= idx <= len(skills):
                skill_name = skills[idx - 1]
                args = input("Argumentos (opcional): ").strip()
                self.execute_skill(skill_name, args)
        except ValueError:
            print("Entrada invalida")

    def menu_manage_mcp(self):
        """Menu para gerenciar MCPs."""
        if not self.mcp_manager:
            print("MCP nao disponivel")
            return

        print("\n" + "-"*40)
        print(" GERENCIAR MCPs")
        print("-"*40)
        print(" 1. Listar servidores")
        print(" 2. Iniciar servidor")
        print(" 3. Parar servidor")
        print(" 0. Voltar")
        print("-"*40)

        choice = input("\nEscolha: ").strip()

        if choice == "1":
            servers = self.mcp_manager.list_servers()
            print("\nServidores MCP:")
            for name, status in servers.items():
                print(f"  {name}: {status}")
        elif choice == "2":
            name = input("Nome do servidor: ").strip()
            if self.mcp_manager.start_server(name):
                print(f"Servidor {name} iniciado")
            else:
                print(f"Falha ao iniciar {name}")
        elif choice == "3":
            name = input("Nome do servidor: ").strip()
            if self.mcp_manager.stop_server(name):
                print(f"Servidor {name} parado")
            else:
                print(f"Falha ao parar {name}")

    def _graceful_shutdown(self):
        """Executa shutdown gracioso."""
        print("\n" + "="*60)
        print(" INICIANDO SHUTDOWN GRACIOSO")
        print("="*60)

        # Verificar agentes ativos
        terminals = self.spawner.get_active_terminals()
        if terminals:
            print(f"\n  Aguardando {len(terminals)} agentes...")
            delay = self.config.graceful_shutdown_delay

            # Dar tempo para agentes terminarem
            for i in range(delay):
                remaining = self.spawner.get_active_terminals()
                if not remaining:
                    print("  Todos os agentes finalizados.")
                    break
                print(f"\r  Aguardando... {delay - i}s restantes", end="", flush=True)
                time.sleep(1)

            # Forcar termino se ainda houver agentes
            remaining = self.spawner.get_active_terminals()
            if remaining:
                print(f"\n  Terminando {len(remaining)} agentes restantes...")
                self.spawner.terminate_all()

        # Salvar estado
        if self.runtime:
            summary = self.runtime.stop()
            self._print_session_summary(summary)

        # Trigger hook
        self.trigger_hook("on_shutdown", {"graceful": True})

    def _print_session_summary(self, summary: Dict):
        """Imprime resumo da sessao."""
        print("\n" + "="*60)
        print(" RESUMO DA SESSAO")
        print("="*60)
        print(f"  Duracao configurada: {summary.get('duration_configured', 'N/A')}")
        print(f"  Duracao real: {summary.get('duration_actual', 'N/A')}")
        print(f"  Inicio: {summary.get('started_at', 'N/A')}")
        print(f"  Fim: {summary.get('ended_at', 'N/A')}")

        stats = summary.get('stats', {})
        if stats:
            print(f"\n  Ciclos executados: {stats.get('cycles_completed', 0)}")
            print(f"  Tasks processadas: {stats.get('tasks_processed', 0)}")
            print(f"  Issues tratadas: {stats.get('issues_handled', 0)}")
            print(f"  Agentes spawnados: {stats.get('agents_spawned', 0)}")
            print(f"  Erros: {stats.get('errors_count', 0)}")

        print("="*60 + "\n")

    def run_autonomous(self):
        """Executa modo autonomo com controle de duracao."""
        # Configurar RuntimeManager
        runtime_config = RuntimeConfig(
            duration_seconds=self.config.runtime_duration,
            graceful_shutdown_delay=self.config.graceful_shutdown_delay,
            warn_before_shutdown=self.config.warn_before_shutdown,
            warn_intervals=self.config.warn_intervals,
            state_file=str(STATE_PATH / "runtime_state.json")
        )
        self.runtime = RuntimeManager(runtime_config)

        # Registrar callbacks
        def on_warning(remaining: int):
            duration_str = format_time_remaining(remaining)
            print(f"\n  [!] AVISO: {duration_str} restantes para encerramento!")

        self.runtime.on_warning(on_warning)

        # Iniciar runtime
        self.runtime.start()

        # Exibir informacoes de inicio
        print("\n" + "="*60)
        print(" MODO AUTONOMO INICIADO")
        print("="*60)
        print(f"  Inicio: {self.runtime.start_time.strftime('%Y-%m-%d %H:%M:%S')}")

        if self.config.runtime_duration:
            print(f"  Duracao: {format_duration(self.config.runtime_duration)}")
            print(f"  Termino previsto: {self.runtime.end_time.strftime('%Y-%m-%d %H:%M:%S')}")
        else:
            print("  Duracao: ILIMITADA")
            print("  Pressione Ctrl+C para parar")

        print("="*60 + "\n")

        self.running = True
        poll_interval = self.config.poll_interval
        cycle = 0

        try:
            while self.running:
                cycle += 1

                # Verificar se deve encerrar por tempo
                if self.runtime.should_shutdown():
                    print("\n[!] Tempo de execucao atingido!")
                    self._graceful_shutdown()
                    break

                # Verificar avisos de tempo
                self.runtime.check_warnings()

                # Log do ciclo
                timestamp = datetime.now().strftime('%H:%M:%S')
                print(f"\n[{timestamp}] Ciclo #{cycle}")

                # Trigger hook de ciclo
                self.trigger_hook("on_cycle", {
                    "timestamp": datetime.now().isoformat(),
                    "cycle": cycle
                })

                # Verificar resultados de agentes
                self.check_results()

                # Limpar terminais completados
                self.spawner.cleanup_completed()

                # Atualizar estatisticas
                self.runtime.increment_stat("cycles_completed")

                # Mostrar status
                self.print_status()

                # Aguardar proximo ciclo
                remaining = self.runtime.check_time_remaining()
                if remaining is not None and remaining < poll_interval:
                    # Ajustar intervalo se proximo do fim
                    wait_time = min(remaining, poll_interval)
                else:
                    wait_time = poll_interval

                if wait_time > 0:
                    print(f"Proxima verificacao em {wait_time}s...")
                    time.sleep(wait_time)

        except KeyboardInterrupt:
            print("\n\n[!] Interrompido pelo usuario")
            self.running = False
            self.trigger_hook("on_interrupt", {"reason": "keyboard_interrupt"})
            self._graceful_shutdown()

    def run_supervised(self):
        """Executa modo supervisionado."""
        print("\n" + "="*60)
        print(" MODO SUPERVISIONADO INICIADO")
        print(" Acoes criticas requerem confirmacao")
        print(" Pressione Ctrl+C para parar")
        print("="*60 + "\n")

        self.running = True
        poll_interval = self.config.poll_interval

        try:
            while self.running:
                print(f"\n[{datetime.now().strftime('%H:%M:%S')}] Verificando...")

                # Verificar resultados de agentes
                self.check_results()

                # Limpar terminais completados
                self.spawner.cleanup_completed()

                self.print_status()

                # Menu rapido
                print("\n  [Enter] Continuar | [m] Menu | [q] Sair")
                choice = input("  > ").strip().lower()

                if choice == "m":
                    self.interactive_menu()
                elif choice == "q":
                    break

                time.sleep(poll_interval)

        except KeyboardInterrupt:
            print("\n\nModo supervisionado interrompido")
            self.running = False

    def run_claude_free(self):
        """Executa Claude Code em modo livre como Orquestrador."""
        print("\n" + "="*60)
        print(" INICIANDO CLAUDE CODE COMO ORQUESTRADOR")
        print("="*60 + "\n")

        # Carregar prompt do orquestrador
        prompt_file = PROMPTS_PATH / "agente_orquestrador.md"

        if not prompt_file.exists():
            print(f"Erro: Prompt nao encontrado: {prompt_file}")
            return

        with open(prompt_file, 'r', encoding='utf-8') as f:
            base_prompt = f.read()

        # Listar skills disponiveis
        skills_info = ""
        if self.skill_manager:
            skills = self.skill_manager.list_skills()
            if skills:
                skills_info = f"\n\nSkills disponiveis: {', '.join(skills)}"

        # Construir prompt completo
        full_prompt = f"""# Voce e o Agente Orquestrador [ORCH]

## Suas Instrucoes
{base_prompt}

---

## Contexto Atual

Voce esta executando no terminal principal da Plataforma E.
Modo atual: {self.mode}

Voce pode:
1. Analisar issues do GitHub
2. Decidir qual agente deve trabalhar em cada task
3. Solicitar spawn de novos terminais para agentes
4. Executar skills disponíveis

Para spawnar um agente, informe no formato:
```
SPAWN: [AGENTE] - Titulo da task
DESCRICAO: Descricao detalhada
```

Agentes disponiveis: {', '.join(self.spawner.AGENT_TYPES)}
{skills_info}

Comece analisando as issues pendentes ou aguardando instrucoes.
"""

        # Salvar prompt
        prompt_path = STATE_PATH / "prompt_ORCH.md"
        prompt_path.parent.mkdir(parents=True, exist_ok=True)

        with open(prompt_path, 'w', encoding='utf-8') as f:
            f.write(full_prompt)

        # Executar Claude
        cmd = [
            "claude",
            "--dangerously-skip-permissions",
            "-p", str(prompt_path)
        ]

        try:
            subprocess.run(cmd, cwd=str(self.base_path))
        except FileNotFoundError:
            print("Erro: Claude CLI nao encontrado")
        except Exception as e:
            print(f"Erro: {e}")


def main():
    parser = argparse.ArgumentParser(
        description="Plataforma E - Orquestrador",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos de uso:
  python start_orchestrator.py                        # Menu interativo
  python start_orchestrator.py -m autonomous -d 2h    # Autonomo por 2 horas
  python start_orchestrator.py -m autonomous -d 8h    # Autonomo por 8 horas
  python start_orchestrator.py -m autonomous -d 24h   # Autonomo por 24 horas
  python start_orchestrator.py -m autonomous          # Autonomo ilimitado
  python start_orchestrator.py --spawn BACK           # Spawnar agente BACK
        """
    )
    parser.add_argument(
        "--mode", "-m",
        type=str,
        choices=["autonomous", "supervised", "interactive"],
        help="Modo de operacao (pula menu de selecao)"
    )
    parser.add_argument(
        "--duration", "-d",
        type=str,
        default="unlimited",
        help="Duracao da execucao autonoma (ex: 1h, 2h, 4h, 8h, 10h, 12h, 24h, 30m, ou segundos)"
    )
    parser.add_argument(
        "--graceful-delay",
        type=int,
        default=60,
        help="Segundos para shutdown gracioso (default: 60)"
    )
    parser.add_argument(
        "--spawn",
        type=str,
        choices=TerminalSpawner.AGENT_TYPES,
        help="Spawnar agente especifico imediatamente"
    )
    parser.add_argument(
        "--task",
        type=str,
        default="",
        help="Titulo da task (usado com --spawn)"
    )
    parser.add_argument(
        "--claude",
        action="store_true",
        help="Iniciar Claude Code como Orquestrador"
    )
    parser.add_argument(
        "--no-skills",
        action="store_true",
        help="Desabilitar carregamento de skills"
    )
    parser.add_argument(
        "--no-hooks",
        action="store_true",
        help="Desabilitar carregamento de hooks"
    )
    parser.add_argument(
        "--no-mcp",
        action="store_true",
        help="Desabilitar carregamento de MCP"
    )

    args = parser.parse_args()

    # Parse duracao
    runtime_duration = parse_duration(args.duration)

    # Configuracao
    config = OrchestratorConfig(
        skills_enabled=not args.no_skills,
        hooks_enabled=not args.no_hooks,
        mcp_enabled=not args.no_mcp,
        runtime_duration=runtime_duration,
        graceful_shutdown_delay=args.graceful_delay
    )

    # Criar orquestrador
    orch = Orchestrator(config=config)
    orch.print_banner()

    # Determinar modo
    if args.mode:
        orch.mode = args.mode
    elif not args.spawn and not args.claude:
        # Perguntar modo ao usuario
        orch.mode = orch.select_mode_menu()

    orch.print_mode_info()

    # Executar acao
    if args.spawn:
        orch.spawn_agent(args.spawn, args.task or f"Task para [{args.spawn}]")
    elif args.claude:
        orch.run_claude_free()
    elif orch.mode == OperationMode.AUTONOMOUS:
        orch.run_autonomous()
    elif orch.mode == OperationMode.SUPERVISED:
        orch.run_supervised()
    else:
        orch.interactive_menu()


if __name__ == "__main__":
    main()
