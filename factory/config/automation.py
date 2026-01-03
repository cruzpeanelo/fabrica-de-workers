# -*- coding: utf-8 -*-
"""
Automation Configuration - Fabrica de Agentes
==============================================
Configuracoes para execucao autonoma de agentes.

Modos de operacao:
- autonomous: Agentes trabalham 24/7 sem intervencao
- supervised: Agentes pedem aprovacao em pontos criticos
- manual: Cada acao requer aprovacao

Author: Fabrica de Agentes
"""

import os
import json
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional
from pathlib import Path

# Diretorio base para persistencia
STATE_DIR = Path(__file__).parent.parent / "state"
STATE_DIR.mkdir(exist_ok=True)


@dataclass
class AutomationConfig:
    """Configuracoes de automacao por usuario/projeto."""

    # Modo de operacao
    mode: str = "supervised"  # autonomous | supervised | manual

    # Acoes que sempre requerem aprovacao (mesmo em modo autonomo)
    require_approval: List[str] = field(default_factory=lambda: [
        "delete_database",
        "drop_table",
        "force_push",
    ])

    # Acoes que requerem aprovacao em modo supervisionado
    supervised_approval: List[str] = field(default_factory=lambda: [
        "git_push",
        "delete_file",
        "security_change",
        "database_migration",
    ])

    # Intervalo de polling para novas issues (segundos)
    poll_interval: int = 30

    # Limite de tarefas simultaneas por agente
    max_concurrent_tasks: int = 3

    # Auto-compact
    auto_compact_enabled: bool = True
    context_threshold: float = 0.8  # Compactar em 80% do limite

    # Notificacoes
    notify_on_error: bool = True
    notify_on_complete: bool = False
    notification_channels: List[str] = field(default_factory=lambda: ["log"])

    # GitHub
    github_repo: str = ""
    github_labels_filter: List[str] = field(default_factory=list)

    # Agentes habilitados
    enabled_agents: List[str] = field(default_factory=lambda: [
        "ORCH", "ARCH", "BACK", "FRONT", "DEVOPS",
        "SEC", "QA", "PROD", "INOV", "FIN", "GROWTH"
    ])

    # Timeout por tarefa (segundos)
    task_timeout: int = 1800  # 30 minutos

    # Retry em caso de falha
    max_retries: int = 3
    retry_delay: int = 60  # segundos

    # === Runtime Duration ===
    # Duracao maxima de execucao autonoma (segundos)
    # None = ilimitado (padrao)
    runtime_duration: Optional[int] = None

    # Delay para shutdown gracioso (segundos)
    graceful_shutdown_delay: int = 60

    # Tempo antes do fim para mostrar aviso (segundos)
    warn_before_shutdown: int = 300  # 5 minutos

    # Intervalos de aviso (segundos antes do fim)
    warn_intervals: List[int] = field(default_factory=lambda: [300, 60, 30, 10])

    # Maximo de agentes simultaneos
    max_concurrent_agents: int = 5

    # Auto-commit de alteracoes
    auto_commit: bool = True

    # Auto-handoff entre agentes
    auto_handoff: bool = True

    # Salvar estado automaticamente ao parar
    auto_save_state: bool = True

    def save(self, path: Optional[Path] = None):
        """Salva configuracao em arquivo JSON."""
        path = path or STATE_DIR / "automation_config.json"
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(asdict(self), f, indent=2, ensure_ascii=False)

    @classmethod
    def load(cls, path: Optional[Path] = None) -> "AutomationConfig":
        """Carrega configuracao de arquivo JSON."""
        path = path or STATE_DIR / "automation_config.json"
        if path.exists():
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                return cls(**data)
        return cls()

    def requires_approval(self, action: str) -> bool:
        """Verifica se uma acao requer aprovacao."""
        # Acoes criticas sempre requerem aprovacao
        if action in self.require_approval:
            return True

        # Em modo manual, tudo requer aprovacao
        if self.mode == "manual":
            return True

        # Em modo supervisionado, verificar lista
        if self.mode == "supervised":
            return action in self.supervised_approval

        # Modo autonomo: nao requer aprovacao (exceto criticas)
        return False


@dataclass
class AgentConfig:
    """Configuracao especifica de um agente."""

    agent_id: str
    prefix: str
    name: str
    enabled: bool = True
    max_concurrent: int = 2
    allowed_tools: List[str] = field(default_factory=lambda: [
        "Read", "Write", "Edit", "Bash", "Grep", "Glob"
    ])
    prompt_file: str = ""
    working_directory: str = ""

    # Estado atual
    status: str = "idle"  # idle | running | paused | error
    current_task: Optional[str] = None
    context_usage: float = 0.0
    tasks_completed: int = 0
    tasks_failed: int = 0


# Registro de agentes padrao
DEFAULT_AGENTS: Dict[str, AgentConfig] = {
    "ORCH": AgentConfig(
        agent_id="orch",
        prefix="[ORCH]",
        name="Orquestrador",
        prompt_file="prompts/agente_orquestrador.md"
    ),
    "ARCH": AgentConfig(
        agent_id="arch",
        prefix="[ARCH]",
        name="Arquiteto",
        prompt_file="prompts/agente_arquiteto.md"
    ),
    "BACK": AgentConfig(
        agent_id="back",
        prefix="[BACK]",
        name="Backend",
        prompt_file="prompts/agente_backend.md"
    ),
    "FRONT": AgentConfig(
        agent_id="front",
        prefix="[FRONT]",
        name="Frontend",
        prompt_file="prompts/agente_frontend.md"
    ),
    "DEVOPS": AgentConfig(
        agent_id="devops",
        prefix="[DEVOPS]",
        name="DevOps",
        prompt_file="prompts/agente_devops.md"
    ),
    "SEC": AgentConfig(
        agent_id="sec",
        prefix="[SEC]",
        name="Security",
        prompt_file="prompts/agente_security.md"
    ),
    "QA": AgentConfig(
        agent_id="qa",
        prefix="[QA]",
        name="QA",
        prompt_file="prompts/agente_qa.md"
    ),
    "PROD": AgentConfig(
        agent_id="prod",
        prefix="[PROD]",
        name="Produto",
        prompt_file="prompts/agente_produto.md"
    ),
    "INOV": AgentConfig(
        agent_id="inov",
        prefix="[INOV]",
        name="Inovacao",
        prompt_file="prompts/agente_inovacao.md"
    ),
    "FIN": AgentConfig(
        agent_id="fin",
        prefix="[FIN]",
        name="Financeiro",
        prompt_file="prompts/agente_financeiro.md"
    ),
    "GROWTH": AgentConfig(
        agent_id="growth",
        prefix="[GROWTH]",
        name="Growth",
        prompt_file="prompts/agente_growth.md"
    ),
}


def get_agent_config(agent_id: str) -> Optional[AgentConfig]:
    """Retorna configuracao de um agente."""
    return DEFAULT_AGENTS.get(agent_id.upper())


def get_all_agents() -> Dict[str, AgentConfig]:
    """Retorna todos os agentes."""
    return DEFAULT_AGENTS.copy()
