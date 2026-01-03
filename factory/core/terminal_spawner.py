"""
Terminal Spawner - Gerenciador de Terminais Git Bash para Agentes

Este modulo e responsavel por:
- Spawnar terminais Git Bash para cada agente
- Rastrear PIDs dos terminais ativos
- Fechar terminais quando necessario
- Comunicar via arquivos JSON
"""

import subprocess
import os
import json
import time
from pathlib import Path
from dataclasses import dataclass, field, asdict
from typing import Optional, Dict, List
from datetime import datetime
import platform


@dataclass
class TerminalInfo:
    """Informacoes sobre um terminal spawneado."""
    agent_type: str
    pid: int
    task_id: str
    started_at: str
    status: str = "running"  # running, completed, failed, terminated
    title: str = ""


@dataclass
class TaskMessage:
    """Mensagem de task para um agente."""
    task_id: str
    agent_type: str
    issue_number: Optional[int] = None
    title: str = ""
    description: str = ""
    priority: str = "medium"
    created_at: str = field(default_factory=lambda: datetime.now().isoformat())
    status: str = "pending"  # pending, in_progress, completed, failed
    context: Dict = field(default_factory=dict)


@dataclass
class ResultMessage:
    """Mensagem de resultado de um agente."""
    task_id: str
    agent_type: str
    status: str = "completed"  # completed, failed, blocked
    files_changed: List[str] = field(default_factory=list)
    commit_hash: Optional[str] = None
    handoff_to: Optional[str] = None
    error_message: Optional[str] = None
    completed_at: str = field(default_factory=lambda: datetime.now().isoformat())
    output: str = ""


class TerminalSpawner:
    """Gerenciador de terminais Git Bash para agentes."""

    # Possiveis locais do Git Bash no Windows
    GIT_BASH_PATHS = [
        "C:/Program Files/Git/usr/bin/mintty.exe",
        "C:/Program Files/Git/git-bash.exe",
        "C:/Program Files (x86)/Git/usr/bin/mintty.exe",
        "C:/Program Files (x86)/Git/git-bash.exe",
    ]

    AGENT_TYPES = [
        "ORCH", "ARCH", "BACK", "FRONT", "DEVOPS",
        "SEC", "QA", "PROD", "INOV", "FIN", "GROWTH"
    ]

    def __init__(self, base_path: Optional[str] = None):
        """
        Inicializa o spawner.

        Args:
            base_path: Caminho base do projeto. Se None, usa o diretorio atual.
        """
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.state_path = self.base_path / "factory" / "state"
        self.tasks_path = self.state_path / "tasks"
        self.results_path = self.state_path / "results"
        self.terminals_file = self.state_path / "active_terminals.json"

        # Criar diretorios se nao existirem
        self.tasks_path.mkdir(parents=True, exist_ok=True)
        self.results_path.mkdir(parents=True, exist_ok=True)

        # Encontrar Git Bash
        self.git_bash_path = self._find_git_bash()

        # Carregar terminais ativos
        self.active_terminals: Dict[str, TerminalInfo] = {}
        self._load_terminals()

    def _find_git_bash(self) -> Optional[str]:
        """Encontra o executavel do Git Bash."""
        for path in self.GIT_BASH_PATHS:
            if os.path.exists(path):
                return path

        # Tentar encontrar via PATH
        try:
            result = subprocess.run(
                ["where", "mintty"] if platform.system() == "Windows" else ["which", "mintty"],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                return result.stdout.strip().split('\n')[0]
        except:
            pass

        return None

    def _load_terminals(self):
        """Carrega terminais ativos do arquivo."""
        if self.terminals_file.exists():
            try:
                with open(self.terminals_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    for agent, info in data.items():
                        self.active_terminals[agent] = TerminalInfo(**info)
            except Exception as e:
                print(f"Erro ao carregar terminais: {e}")

    def _save_terminals(self):
        """Salva terminais ativos no arquivo."""
        try:
            data = {k: asdict(v) for k, v in self.active_terminals.items()}
            with open(self.terminals_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Erro ao salvar terminais: {e}")

    def spawn_terminal(
        self,
        agent_type: str,
        task: TaskMessage,
        keep_open: bool = True
    ) -> Optional[TerminalInfo]:
        """
        Spawna um novo terminal Git Bash para um agente.

        Args:
            agent_type: Tipo do agente (BACK, FRONT, etc.)
            task: Mensagem de task para o agente
            keep_open: Manter terminal aberto apos conclusao

        Returns:
            TerminalInfo se sucesso, None se falha
        """
        if agent_type not in self.AGENT_TYPES:
            print(f"Tipo de agente invalido: {agent_type}")
            return None

        if not self.git_bash_path:
            print("Git Bash nao encontrado!")
            return None

        # Verificar se ja tem terminal ativo para este agente
        if agent_type in self.active_terminals:
            existing = self.active_terminals[agent_type]
            if existing.status == "running":
                print(f"Agente [{agent_type}] ja tem terminal ativo (PID: {existing.pid})")
                return existing

        # Salvar task no arquivo
        self._write_task(task)

        # Construir comando
        title = f"[{agent_type}] {task.title[:30]}..."
        script_path = self.base_path / "start_agent.py"

        # Comando para o terminal
        if keep_open:
            bash_cmd = f'python "{script_path}" --agent {agent_type} --task-id {task.task_id}; echo "Pressione Enter para fechar..."; read'
        else:
            bash_cmd = f'python "{script_path}" --agent {agent_type} --task-id {task.task_id}'

        try:
            # Spawnar terminal
            if "mintty" in self.git_bash_path.lower():
                # Usando mintty (terminal do Git Bash)
                process = subprocess.Popen([
                    self.git_bash_path,
                    "-t", title,
                    "-e", "/bin/bash", "-l", "-c", bash_cmd
                ])
            else:
                # Usando git-bash.exe diretamente
                process = subprocess.Popen([
                    self.git_bash_path,
                    "-c", bash_cmd
                ])

            # Registrar terminal
            terminal_info = TerminalInfo(
                agent_type=agent_type,
                pid=process.pid,
                task_id=task.task_id,
                started_at=datetime.now().isoformat(),
                status="running",
                title=title
            )

            self.active_terminals[agent_type] = terminal_info
            self._save_terminals()

            print(f"Terminal spawneado para [{agent_type}] - PID: {process.pid}")
            return terminal_info

        except Exception as e:
            print(f"Erro ao spawnar terminal: {e}")
            return None

    def _write_task(self, task: TaskMessage):
        """Escreve task no arquivo JSON."""
        task_file = self.tasks_path / f"{task.agent_type}_task.json"
        try:
            with open(task_file, 'w', encoding='utf-8') as f:
                json.dump(asdict(task), f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Erro ao escrever task: {e}")

    def read_task(self, agent_type: str) -> Optional[TaskMessage]:
        """Le task do arquivo JSON."""
        task_file = self.tasks_path / f"{agent_type}_task.json"
        if not task_file.exists():
            return None

        try:
            with open(task_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                return TaskMessage(**data)
        except Exception as e:
            print(f"Erro ao ler task: {e}")
            return None

    def write_result(self, result: ResultMessage):
        """Escreve resultado no arquivo JSON."""
        result_file = self.results_path / f"{result.agent_type}_result.json"
        try:
            with open(result_file, 'w', encoding='utf-8') as f:
                json.dump(asdict(result), f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Erro ao escrever resultado: {e}")

    def read_result(self, agent_type: str) -> Optional[ResultMessage]:
        """Le resultado do arquivo JSON."""
        result_file = self.results_path / f"{agent_type}_result.json"
        if not result_file.exists():
            return None

        try:
            with open(result_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                return ResultMessage(**data)
        except Exception as e:
            print(f"Erro ao ler resultado: {e}")
            return None

    def clear_task(self, agent_type: str):
        """Remove arquivo de task."""
        task_file = self.tasks_path / f"{agent_type}_task.json"
        if task_file.exists():
            task_file.unlink()

    def clear_result(self, agent_type: str):
        """Remove arquivo de resultado."""
        result_file = self.results_path / f"{agent_type}_result.json"
        if result_file.exists():
            result_file.unlink()

    def check_terminal_status(self, agent_type: str) -> Optional[str]:
        """
        Verifica status de um terminal.

        Returns:
            "running", "completed", "failed", "not_found"
        """
        if agent_type not in self.active_terminals:
            return "not_found"

        terminal = self.active_terminals[agent_type]

        # Verificar se processo ainda existe
        try:
            # No Windows, tentamos abrir o processo
            if platform.system() == "Windows":
                import ctypes
                kernel32 = ctypes.windll.kernel32
                handle = kernel32.OpenProcess(0x1000, False, terminal.pid)  # PROCESS_QUERY_LIMITED_INFORMATION
                if handle:
                    kernel32.CloseHandle(handle)
                    # Verificar se tem resultado
                    result = self.read_result(agent_type)
                    if result:
                        terminal.status = result.status
                        self._save_terminals()
                        return result.status
                    return "running"
                else:
                    terminal.status = "terminated"
                    self._save_terminals()
                    return "terminated"
            else:
                os.kill(terminal.pid, 0)
                return "running"
        except (OSError, ProcessLookupError):
            terminal.status = "terminated"
            self._save_terminals()
            return "terminated"

    def wait_for_result(
        self,
        agent_type: str,
        timeout: int = 1800,
        poll_interval: int = 5
    ) -> Optional[ResultMessage]:
        """
        Aguarda resultado de um agente.

        Args:
            agent_type: Tipo do agente
            timeout: Timeout em segundos (default 30 min)
            poll_interval: Intervalo de polling em segundos

        Returns:
            ResultMessage se completou, None se timeout
        """
        start_time = time.time()

        while time.time() - start_time < timeout:
            # Verificar se tem resultado
            result = self.read_result(agent_type)
            if result:
                return result

            # Verificar status do terminal
            status = self.check_terminal_status(agent_type)
            if status in ["terminated", "failed"]:
                print(f"Terminal [{agent_type}] terminou sem resultado")
                return None

            time.sleep(poll_interval)

        print(f"Timeout aguardando resultado de [{agent_type}]")
        return None

    def terminate_terminal(self, agent_type: str) -> bool:
        """Termina um terminal."""
        if agent_type not in self.active_terminals:
            return False

        terminal = self.active_terminals[agent_type]

        try:
            if platform.system() == "Windows":
                subprocess.run(["taskkill", "/F", "/PID", str(terminal.pid)],
                             capture_output=True)
            else:
                os.kill(terminal.pid, 9)

            terminal.status = "terminated"
            self._save_terminals()
            return True
        except Exception as e:
            print(f"Erro ao terminar terminal: {e}")
            return False

    def terminate_all(self):
        """Termina todos os terminais ativos."""
        for agent_type in list(self.active_terminals.keys()):
            self.terminate_terminal(agent_type)

    def get_active_terminals(self) -> Dict[str, TerminalInfo]:
        """Retorna dicionario de terminais ativos."""
        # Atualizar status
        for agent_type in list(self.active_terminals.keys()):
            self.check_terminal_status(agent_type)

        return {k: v for k, v in self.active_terminals.items()
                if v.status == "running"}

    def cleanup_completed(self):
        """Remove terminais que ja completaram."""
        for agent_type in list(self.active_terminals.keys()):
            terminal = self.active_terminals[agent_type]
            if terminal.status in ["completed", "failed", "terminated"]:
                del self.active_terminals[agent_type]
                self.clear_task(agent_type)
                # Manter resultado para processamento

        self._save_terminals()


# Funcao de conveniencia para criar task
def create_task(
    agent_type: str,
    title: str,
    description: str = "",
    issue_number: Optional[int] = None,
    priority: str = "medium",
    context: Optional[Dict] = None
) -> TaskMessage:
    """Cria uma nova mensagem de task."""
    task_id = f"task_{agent_type}_{int(time.time())}"
    return TaskMessage(
        task_id=task_id,
        agent_type=agent_type,
        issue_number=issue_number,
        title=title,
        description=description,
        priority=priority,
        context=context or {}
    )


if __name__ == "__main__":
    # Teste basico
    spawner = TerminalSpawner()

    print(f"Git Bash encontrado: {spawner.git_bash_path}")
    print(f"Tasks path: {spawner.tasks_path}")
    print(f"Results path: {spawner.results_path}")

    # Teste de criacao de task
    task = create_task(
        agent_type="BACK",
        title="Teste de terminal",
        description="Esta e uma task de teste para verificar o spawn de terminal"
    )

    print(f"\nTask criada: {task.task_id}")
    print(f"Agent: {task.agent_type}")
    print(f"Title: {task.title}")
