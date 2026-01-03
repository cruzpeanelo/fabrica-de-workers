# -*- coding: utf-8 -*-
"""
Handoff Manager - Fabrica de Agentes
====================================
Gerencia passagem de tarefas entre agentes.

Quando um agente completa uma tarefa, o HandoffManager
determina para qual(is) agente(s) a tarefa deve seguir.

Author: Fabrica de Agentes
"""

import json
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field, asdict
from enum import Enum

logger = logging.getLogger(__name__)

# Diretorio base para persistencia
STATE_DIR = Path(__file__).parent.parent / "state"
STATE_DIR.mkdir(exist_ok=True)


class HandoffStatus(Enum):
    """Status de um handoff."""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class Handoff:
    """Representa uma passagem de tarefa entre agentes."""
    handoff_id: str
    from_agent: str
    to_agent: str
    task_description: str
    issue_number: Optional[int] = None
    status: str = "pending"
    created_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())
    completed_at: Optional[str] = None
    context: Dict[str, Any] = field(default_factory=dict)
    result: Optional[str] = None


class HandoffManager:
    """
    Gerencia passagem de tarefas entre agentes.

    Define regras de handoff baseadas no tipo de tarefa
    e no agente que a completou.
    """

    # Regras de handoff: de qual agente para quais agentes
    HANDOFF_RULES: Dict[str, Dict[str, List[str]]] = {
        "[ORCH]": {
            "on_distribute": ["[BACK]", "[FRONT]", "[SEC]", "[DEVOPS]"],
            "on_review_needed": ["[QA]"],
        },
        "[ARCH]": {
            "on_design_complete": ["[BACK]", "[FRONT]"],
            "on_infrastructure": ["[DEVOPS]"],
            "on_security_review": ["[SEC]"],
        },
        "[BACK]": {
            "on_complete": ["[QA]"],
            "on_security_review": ["[SEC]"],
            "on_blocked": ["[ARCH]"],
            "on_needs_frontend": ["[FRONT]"],
        },
        "[FRONT]": {
            "on_complete": ["[QA]"],
            "on_needs_api": ["[BACK]"],
            "on_blocked": ["[ARCH]"],
        },
        "[DEVOPS]": {
            "on_deploy_ready": ["[QA]"],
            "on_security_check": ["[SEC]"],
            "on_complete": ["[ORCH]"],
        },
        "[SEC]": {
            "on_approved": ["[DEVOPS]"],
            "on_critical": ["[ORCH]"],
            "on_fix_needed": ["[BACK]"],
        },
        "[QA]": {
            "on_tests_pass": ["[DEVOPS]"],
            "on_tests_fail": ["[BACK]", "[FRONT]"],
            "on_complete": ["[ORCH]"],
        },
        "[PROD]": {
            "on_story_ready": ["[ARCH]"],
            "on_clarification": ["[ORCH]"],
        },
        "[INOV]": {
            "on_poc_ready": ["[ARCH]"],
            "on_adoption": ["[BACK]"],
        },
        "[FIN]": {
            "on_cost_alert": ["[DEVOPS]"],
            "on_pricing_update": ["[PROD]"],
        },
        "[GROWTH]": {
            "on_feature_request": ["[PROD]"],
            "on_campaign": ["[FRONT]"],
        },
    }

    def __init__(self):
        self.queue_file = STATE_DIR / "handoff_queue.json"
        self.handoffs: List[Handoff] = []
        self._load_queue()

    def _load_queue(self):
        """Carrega fila de handoffs do arquivo."""
        if self.queue_file.exists():
            with open(self.queue_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                self.handoffs = [Handoff(**h) for h in data]
        else:
            self.handoffs = []

    def _save_queue(self):
        """Salva fila de handoffs no arquivo."""
        with open(self.queue_file, 'w', encoding='utf-8') as f:
            json.dump(
                [asdict(h) for h in self.handoffs],
                f, indent=2, ensure_ascii=False
            )

    def _generate_id(self) -> str:
        """Gera ID unico para handoff."""
        import uuid
        return f"hnd_{uuid.uuid4().hex[:8]}"

    def get_next_agents(self, from_agent: str, status: str) -> List[str]:
        """
        Retorna lista de agentes para os quais enviar handoff.

        Args:
            from_agent: Agente de origem (ex: "[BACK]")
            status: Status da tarefa (ex: "on_complete")

        Returns:
            Lista de agentes destino
        """
        rules = self.HANDOFF_RULES.get(from_agent, {})
        return rules.get(status, [])

    def create_handoff(
        self,
        from_agent: str,
        to_agent: str,
        task_description: str,
        issue_number: Optional[int] = None,
        context: Optional[Dict] = None
    ) -> Handoff:
        """
        Cria um novo handoff.

        Args:
            from_agent: Agente de origem
            to_agent: Agente destino
            task_description: Descricao da tarefa
            issue_number: Numero da issue (opcional)
            context: Contexto adicional (opcional)

        Returns:
            Handoff criado
        """
        handoff = Handoff(
            handoff_id=self._generate_id(),
            from_agent=from_agent,
            to_agent=to_agent,
            task_description=task_description,
            issue_number=issue_number,
            context=context or {}
        )
        self.handoffs.append(handoff)
        self._save_queue()

        logger.info(
            f"Handoff criado: {from_agent} -> {to_agent}: {task_description[:50]}..."
        )
        return handoff

    def process_task_completion(
        self,
        from_agent: str,
        task_description: str,
        status: str = "on_complete",
        issue_number: Optional[int] = None,
        context: Optional[Dict] = None
    ) -> List[Handoff]:
        """
        Processa conclusao de tarefa e cria handoffs necessarios.

        Args:
            from_agent: Agente que completou a tarefa
            task_description: Descricao da tarefa
            status: Status de conclusao (determina proximos agentes)
            issue_number: Numero da issue
            context: Contexto para proximos agentes

        Returns:
            Lista de handoffs criados
        """
        next_agents = self.get_next_agents(from_agent, status)
        handoffs = []

        for to_agent in next_agents:
            handoff = self.create_handoff(
                from_agent=from_agent,
                to_agent=to_agent,
                task_description=task_description,
                issue_number=issue_number,
                context=context
            )
            handoffs.append(handoff)

        return handoffs

    def get_pending_for_agent(self, agent: str) -> List[Handoff]:
        """Retorna handoffs pendentes para um agente."""
        return [
            h for h in self.handoffs
            if h.to_agent == agent and h.status == "pending"
        ]

    def get_all_pending(self) -> List[Handoff]:
        """Retorna todos os handoffs pendentes."""
        return [h for h in self.handoffs if h.status == "pending"]

    def mark_in_progress(self, handoff_id: str) -> Optional[Handoff]:
        """Marca handoff como em progresso."""
        for h in self.handoffs:
            if h.handoff_id == handoff_id:
                h.status = "in_progress"
                self._save_queue()
                return h
        return None

    def mark_completed(
        self,
        handoff_id: str,
        result: Optional[str] = None
    ) -> Optional[Handoff]:
        """Marca handoff como completado."""
        for h in self.handoffs:
            if h.handoff_id == handoff_id:
                h.status = "completed"
                h.completed_at = datetime.utcnow().isoformat()
                h.result = result
                self._save_queue()
                return h
        return None

    def mark_failed(
        self,
        handoff_id: str,
        error: Optional[str] = None
    ) -> Optional[Handoff]:
        """Marca handoff como falho."""
        for h in self.handoffs:
            if h.handoff_id == handoff_id:
                h.status = "failed"
                h.completed_at = datetime.utcnow().isoformat()
                h.result = error
                self._save_queue()
                return h
        return None

    def cleanup_old_handoffs(self, max_age_days: int = 30):
        """Remove handoffs antigos completados."""
        from datetime import timedelta

        cutoff = datetime.utcnow() - timedelta(days=max_age_days)

        self.handoffs = [
            h for h in self.handoffs
            if h.status in ("pending", "in_progress") or
            (h.completed_at and datetime.fromisoformat(h.completed_at) > cutoff)
        ]
        self._save_queue()

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas de handoffs."""
        stats = {
            "total": len(self.handoffs),
            "pending": len([h for h in self.handoffs if h.status == "pending"]),
            "in_progress": len([h for h in self.handoffs if h.status == "in_progress"]),
            "completed": len([h for h in self.handoffs if h.status == "completed"]),
            "failed": len([h for h in self.handoffs if h.status == "failed"]),
            "by_agent": {}
        }

        for h in self.handoffs:
            if h.to_agent not in stats["by_agent"]:
                stats["by_agent"][h.to_agent] = {"pending": 0, "completed": 0}
            if h.status == "pending":
                stats["by_agent"][h.to_agent]["pending"] += 1
            elif h.status == "completed":
                stats["by_agent"][h.to_agent]["completed"] += 1

        return stats
