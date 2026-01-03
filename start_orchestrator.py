#!/usr/bin/env python3
"""
Start Orchestrator - Script principal do Agente Orquestrador.

Este script inicia o Agente Orquestrador que:
1. Monitora GitHub por novas issues
2. Classifica e distribui tasks para agentes
3. Spawna terminais Git Bash para cada agente
4. Processa handoffs entre agentes
5. Gerencia o ciclo de vida dos terminais

Uso:
    python start_orchestrator.py                    # Modo interativo
    python start_orchestrator.py --auto             # Modo autonomo
    python start_orchestrator.py --spawn BACK      # Spawna agente especifico
"""

import argparse
import os
import sys
import json
import time
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Optional, Dict, List

# Adicionar factory ao path
sys.path.insert(0, str(Path(__file__).parent))

from factory.core.terminal_spawner import TerminalSpawner, TaskMessage, create_task


# Diretorio base do projeto
BASE_PATH = Path(__file__).parent
PROMPTS_PATH = BASE_PATH / "prompts"
STATE_PATH = BASE_PATH / "factory" / "state"


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


class Orchestrator:
    """Agente Orquestrador - Coordenador do Squad."""

    def __init__(self, base_path: Optional[str] = None):
        self.base_path = Path(base_path) if base_path else BASE_PATH
        self.spawner = TerminalSpawner(str(self.base_path))
        self.running = False
        self.mode = "interactive"  # interactive, autonomous

    def print_banner(self):
        """Exibe banner do orquestrador."""
        banner = """
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║   ███████╗ █████╗ ██████╗ ██████╗ ██╗ ██████╗ █████╗         ║
║   ██╔════╝██╔══██╗██╔══██╗██╔══██╗██║██╔════╝██╔══██╗        ║
║   █████╗  ███████║██████╔╝██████╔╝██║██║     ███████║        ║
║   ██╔══╝  ██╔══██║██╔══██╗██╔══██╗██║██║     ██╔══██║        ║
║   ██║     ██║  ██║██████╔╝██║  ██║██║╚██████╗██║  ██║        ║
║   ╚═╝     ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝╚═╝ ╚═════╝╚═╝  ╚═╝        ║
║                                                               ║
║   DE AGENTES - Sistema de Desenvolvimento Autonomo            ║
║                                                               ║
║   [ORCH] ORQUESTRADOR - Tech Lead do Squad                   ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
        """
        print(banner)
        print(f"  Iniciado em: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"  Modo: {self.mode}")
        print(f"  Git Bash: {self.spawner.git_bash_path or 'NAO ENCONTRADO'}")
        print()

    def print_status(self):
        """Exibe status atual."""
        terminals = self.spawner.get_active_terminals()

        print("\n" + "="*60)
        print(" STATUS DOS AGENTES")
        print("="*60)

        if terminals:
            for agent, info in terminals.items():
                print(f"  [{agent}] PID: {info.pid} | Status: {info.status} | Task: {info.task_id}")
        else:
            print("  Nenhum agente ativo")

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
            print(f"\nHandoff: [{from_agent}] -> [{next_agent}] ({handoff_type})")
            return next_agent
        return None

    def check_results(self):
        """Verifica resultados de agentes."""
        for agent_type in self.spawner.AGENT_TYPES:
            result = self.spawner.read_result(agent_type)

            if result and result.status != "pending":
                print(f"\n[{agent_type}] Resultado recebido:")
                print(f"  Status: {result.status}")
                print(f"  Arquivos: {result.files_changed}")

                if result.handoff_to:
                    next_agent = result.handoff_to.strip("[]")
                    if next_agent in self.spawner.AGENT_TYPES:
                        # Criar nova task para proximo agente
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
            print("\n" + "-"*40)
            print(" MENU DO ORQUESTRADOR")
            print("-"*40)
            print(" 1. Spawnar agente")
            print(" 2. Ver status")
            print(" 3. Verificar resultados")
            print(" 4. Terminar agente")
            print(" 5. Terminar todos")
            print(" 6. Modo autonomo")
            print(" 7. Iniciar Claude (modo livre)")
            print(" 0. Sair")
            print("-"*40)

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
                self.run_autonomous()
            elif choice == "7":
                self.run_claude_free()
            elif choice == "0":
                print("\nEncerrando orquestrador...")
                self.spawner.terminate_all()
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

    def run_autonomous(self):
        """Executa modo autonomo."""
        print("\n" + "="*60)
        print(" MODO AUTONOMO INICIADO")
        print(" Pressione Ctrl+C para parar")
        print("="*60 + "\n")

        self.running = True
        poll_interval = 30  # segundos

        try:
            while self.running:
                print(f"[{datetime.now().strftime('%H:%M:%S')}] Verificando...")

                # Verificar resultados de agentes
                self.check_results()

                # Limpar terminais completados
                self.spawner.cleanup_completed()

                # TODO: Aqui seria a integracao com GitHub para buscar issues
                # Por enquanto, apenas monitora

                self.print_status()

                print(f"Proxima verificacao em {poll_interval}s...")
                time.sleep(poll_interval)

        except KeyboardInterrupt:
            print("\n\nModo autonomo interrompido pelo usuario")
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

        # Construir prompt completo
        full_prompt = f"""# Voce e o Agente Orquestrador [ORCH]

## Suas Instrucoes
{base_prompt}

---

## Contexto Atual

Voce esta executando no terminal principal da Fabrica de Agentes.
Voce pode:
1. Analisar issues do GitHub
2. Decidir qual agente deve trabalhar em cada task
3. Solicitar spawn de novos terminais para agentes

Para spawnar um agente, informe no formato:
```
SPAWN: [AGENTE] - Titulo da task
DESCRICAO: Descricao detalhada
```

Agentes disponiveis: {', '.join(self.spawner.AGENT_TYPES)}

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
        description="Fabrica de Agentes - Orquestrador"
    )
    parser.add_argument(
        "--auto",
        action="store_true",
        help="Iniciar em modo autonomo"
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

    args = parser.parse_args()

    orch = Orchestrator()
    orch.print_banner()

    if args.spawn:
        orch.spawn_agent(args.spawn, args.task or f"Task para [{args.spawn}]")
    elif args.auto:
        orch.mode = "autonomous"
        orch.run_autonomous()
    elif args.claude:
        orch.run_claude_free()
    else:
        orch.interactive_menu()


if __name__ == "__main__":
    main()
