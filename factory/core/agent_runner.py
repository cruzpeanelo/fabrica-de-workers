# -*- coding: utf-8 -*-
"""
Agent Runner - Fabrica de Agentes
=================================
Wrapper para executar agentes Claude Code como processos separados.

Cada agente roda em seu proprio processo, com:
- Isolamento de contexto
- Auto-commit habilitado
- Persistencia de estado em JSON

Author: Fabrica de Agentes
"""

import os
import sys
import json
import asyncio
import subprocess
import logging
from pathlib import Path
from datetime import datetime
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field, asdict

logger = logging.getLogger(__name__)

# Diretorio base
BASE_DIR = Path(__file__).parent.parent.parent
STATE_DIR = BASE_DIR / "factory" / "state" / "agent_states"
STATE_DIR.mkdir(parents=True, exist_ok=True)


@dataclass
class AgentState:
    """Estado persistido de um agente."""
    agent_id: str
    status: str = "idle"  # idle | running | paused | error | completed
    current_task: Optional[str] = None
    current_issue: Optional[int] = None
    context_usage: float = 0.0
    tasks_completed: int = 0
    tasks_failed: int = 0
    last_activity: Optional[str] = None
    last_error: Optional[str] = None
    history: List[Dict[str, Any]] = field(default_factory=list)

    def save(self):
        """Salva estado em arquivo JSON."""
        path = STATE_DIR / f"{self.agent_id}.json"
        self.last_activity = datetime.utcnow().isoformat()
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(asdict(self), f, indent=2, ensure_ascii=False)

    @classmethod
    def load(cls, agent_id: str) -> "AgentState":
        """Carrega estado de arquivo JSON."""
        path = STATE_DIR / f"{agent_id}.json"
        if path.exists():
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                return cls(**data)
        return cls(agent_id=agent_id)

    def add_history(self, action: str, details: Dict[str, Any] = None):
        """Adiciona entrada ao historico."""
        entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "action": action,
            "details": details or {}
        }
        self.history.append(entry)
        # Manter apenas ultimas 100 entradas
        if len(self.history) > 100:
            self.history = self.history[-100:]


class AgentRunner:
    """
    Executa um agente Claude Code como processo separado.

    Uso:
        runner = AgentRunner("back", "prompts/agente_backend.md")
        result = await runner.run_task("Implementar issue #123: ...")
    """

    def __init__(
        self,
        agent_id: str,
        prompt_file: str,
        working_dir: Optional[str] = None,
        allowed_tools: Optional[List[str]] = None,
        auto_approve: bool = True
    ):
        self.agent_id = agent_id
        self.prompt_file = Path(prompt_file)
        self.working_dir = Path(working_dir or BASE_DIR)
        self.allowed_tools = allowed_tools or [
            "Read", "Write", "Edit", "Bash", "Grep", "Glob",
            "WebFetch", "WebSearch", "TodoWrite"
        ]
        self.auto_approve = auto_approve
        self.state = AgentState.load(agent_id)
        self.process: Optional[asyncio.subprocess.Process] = None

    def _build_command(self, task: str) -> List[str]:
        """Constroi comando para executar Claude Code."""
        cmd = ["claude"]

        # Modo headless com auto-aprovacao
        if self.auto_approve:
            cmd.append("--dangerously-skip-permissions")

        # Ferramentas permitidas
        if self.allowed_tools:
            cmd.extend(["--allowedTools", ",".join(self.allowed_tools)])

        # Output em JSON para parsing
        cmd.extend(["--output-format", "json"])

        # Print para capturar output
        cmd.append("--print")

        # System prompt do agente
        if self.prompt_file.exists():
            prompt_content = self.prompt_file.read_text(encoding='utf-8')
            cmd.extend(["-p", prompt_content])

        # Tarefa a executar
        cmd.append(task)

        return cmd

    async def run_task(self, task: str, timeout: int = 1800) -> Dict[str, Any]:
        """
        Executa uma tarefa usando Claude Code.

        Args:
            task: Descricao da tarefa
            timeout: Timeout em segundos (default 30 min)

        Returns:
            Dict com resultado da execucao
        """
        self.state.status = "running"
        self.state.current_task = task[:100]
        self.state.add_history("task_started", {"task": task[:200]})
        self.state.save()

        result = {
            "success": False,
            "output": "",
            "error": None,
            "duration": 0,
            "agent_id": self.agent_id
        }

        start_time = datetime.utcnow()

        try:
            cmd = self._build_command(task)
            logger.info(f"[{self.agent_id}] Executando: {task[:50]}...")

            # Executar processo
            self.process = await asyncio.create_subprocess_exec(
                *cmd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd=str(self.working_dir)
            )

            try:
                stdout, stderr = await asyncio.wait_for(
                    self.process.communicate(),
                    timeout=timeout
                )
            except asyncio.TimeoutError:
                self.process.kill()
                await self.process.wait()
                raise TimeoutError(f"Task timeout after {timeout}s")

            result["output"] = stdout.decode('utf-8', errors='replace')
            result["success"] = self.process.returncode == 0

            if stderr:
                result["error"] = stderr.decode('utf-8', errors='replace')

            # Atualizar estado
            if result["success"]:
                self.state.tasks_completed += 1
                self.state.status = "idle"
            else:
                self.state.tasks_failed += 1
                self.state.status = "error"
                self.state.last_error = result.get("error", "Unknown error")

        except Exception as e:
            result["error"] = str(e)
            self.state.tasks_failed += 1
            self.state.status = "error"
            self.state.last_error = str(e)
            logger.error(f"[{self.agent_id}] Erro: {e}")

        finally:
            end_time = datetime.utcnow()
            result["duration"] = (end_time - start_time).total_seconds()

            self.state.current_task = None
            self.state.add_history(
                "task_completed" if result["success"] else "task_failed",
                {"duration": result["duration"], "success": result["success"]}
            )
            self.state.save()

        return result

    async def stop(self):
        """Para o agente se estiver rodando."""
        if self.process and self.process.returncode is None:
            self.process.terminate()
            try:
                await asyncio.wait_for(self.process.wait(), timeout=5)
            except asyncio.TimeoutError:
                self.process.kill()

        self.state.status = "idle"
        self.state.current_task = None
        self.state.save()

    def get_status(self) -> Dict[str, Any]:
        """Retorna status atual do agente."""
        return {
            "agent_id": self.agent_id,
            "status": self.state.status,
            "current_task": self.state.current_task,
            "tasks_completed": self.state.tasks_completed,
            "tasks_failed": self.state.tasks_failed,
            "last_activity": self.state.last_activity,
            "context_usage": self.state.context_usage
        }


class AgentPool:
    """
    Pool de agentes para execucao paralela.

    Gerencia multiplos AgentRunners e distribui tarefas.
    """

    def __init__(self, config: Optional[Dict] = None):
        self.runners: Dict[str, AgentRunner] = {}
        self.config = config or {}
        self._initialize_runners()

    def _initialize_runners(self):
        """Inicializa runners para todos os agentes."""
        from factory.config.automation import DEFAULT_AGENTS

        for agent_id, agent_config in DEFAULT_AGENTS.items():
            self.runners[agent_id] = AgentRunner(
                agent_id=agent_config.agent_id,
                prompt_file=str(BASE_DIR / agent_config.prompt_file),
                working_dir=str(BASE_DIR),
                allowed_tools=agent_config.allowed_tools,
                auto_approve=True
            )

    def get_runner(self, agent_id: str) -> Optional[AgentRunner]:
        """Retorna runner de um agente."""
        return self.runners.get(agent_id.upper())

    async def run_task(self, agent_id: str, task: str) -> Dict[str, Any]:
        """Executa tarefa em um agente especifico."""
        runner = self.get_runner(agent_id)
        if not runner:
            return {"success": False, "error": f"Agent {agent_id} not found"}
        return await runner.run_task(task)

    def get_all_status(self) -> Dict[str, Dict[str, Any]]:
        """Retorna status de todos os agentes."""
        return {
            agent_id: runner.get_status()
            for agent_id, runner in self.runners.items()
        }

    async def stop_all(self):
        """Para todos os agentes."""
        await asyncio.gather(*[
            runner.stop() for runner in self.runners.values()
        ])
