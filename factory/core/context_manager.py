# -*- coding: utf-8 -*-
"""
Context Manager - Fabrica de Agentes
====================================
Gerenciamento de contexto e auto-compact para evitar overflow.

Monitora uso de contexto de cada agente e dispara compactacao
quando necessario para evitar perda de informacao.

Author: Fabrica de Agentes
"""

import json
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field, asdict

logger = logging.getLogger(__name__)

# Diretorio base para persistencia
STATE_DIR = Path(__file__).parent.parent / "state"
STATE_DIR.mkdir(exist_ok=True)


@dataclass
class ContextSnapshot:
    """Snapshot do contexto de um agente."""
    agent_id: str
    timestamp: str
    summary: str
    tasks_in_progress: List[str] = field(default_factory=list)
    important_files: List[str] = field(default_factory=list)
    last_commits: List[str] = field(default_factory=list)
    pending_handoffs: List[Dict] = field(default_factory=list)


class ContextManager:
    """
    Gerencia contexto e auto-compact para evitar overflow.

    Monitora o uso de contexto e salva snapshots antes de
    reiniciar agentes com contexto limpo.
    """

    # Limites de tokens (estimativa)
    MAX_CONTEXT_TOKENS = 150000  # Limite seguro
    COMPACT_THRESHOLD = 0.8      # Compactar em 80%
    TOKENS_PER_CHAR = 0.25       # Estimativa: 4 chars = 1 token

    def __init__(self):
        self.snapshots_dir = STATE_DIR / "context_snapshots"
        self.snapshots_dir.mkdir(exist_ok=True)
        self.agent_contexts: Dict[str, float] = {}

    def estimate_tokens(self, text: str) -> int:
        """Estima numero de tokens em um texto."""
        return int(len(text) * self.TOKENS_PER_CHAR)

    def update_context_usage(self, agent_id: str, context_size: int):
        """Atualiza estimativa de uso de contexto de um agente."""
        self.agent_contexts[agent_id] = context_size / self.MAX_CONTEXT_TOKENS
        logger.debug(f"[{agent_id}] Context usage: {self.agent_contexts[agent_id]:.1%}")

    def should_compact(self, agent_id: str) -> bool:
        """Verifica se agente precisa compactar contexto."""
        usage = self.agent_contexts.get(agent_id, 0)
        return usage >= self.COMPACT_THRESHOLD

    def get_usage(self, agent_id: str) -> float:
        """Retorna percentual de uso de contexto."""
        return self.agent_contexts.get(agent_id, 0)

    def get_all_usage(self) -> Dict[str, float]:
        """Retorna uso de contexto de todos os agentes."""
        return self.agent_contexts.copy()

    def create_snapshot(
        self,
        agent_id: str,
        summary: str,
        tasks_in_progress: List[str] = None,
        important_files: List[str] = None,
        last_commits: List[str] = None,
        pending_handoffs: List[Dict] = None
    ) -> ContextSnapshot:
        """
        Cria snapshot do contexto atual antes de compactar.

        Este snapshot sera usado para restaurar contexto essencial
        apos reiniciar o agente.
        """
        snapshot = ContextSnapshot(
            agent_id=agent_id,
            timestamp=datetime.utcnow().isoformat(),
            summary=summary,
            tasks_in_progress=tasks_in_progress or [],
            important_files=important_files or [],
            last_commits=last_commits or [],
            pending_handoffs=pending_handoffs or []
        )

        # Salvar snapshot
        filename = f"{agent_id}_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}.json"
        path = self.snapshots_dir / filename
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(asdict(snapshot), f, indent=2, ensure_ascii=False)

        logger.info(f"[{agent_id}] Context snapshot saved: {filename}")
        return snapshot

    def get_latest_snapshot(self, agent_id: str) -> Optional[ContextSnapshot]:
        """Retorna snapshot mais recente de um agente."""
        snapshots = sorted(
            self.snapshots_dir.glob(f"{agent_id}_*.json"),
            reverse=True
        )
        if snapshots:
            with open(snapshots[0], 'r', encoding='utf-8') as f:
                data = json.load(f)
                return ContextSnapshot(**data)
        return None

    def build_restoration_prompt(self, snapshot: ContextSnapshot) -> str:
        """
        Constroi prompt para restaurar contexto apos compact.

        Este prompt e enviado ao agente apos reiniciar para
        restaurar o contexto essencial.
        """
        prompt = f"""## Resumo do Contexto Anterior

{snapshot.summary}

### Tarefas em Progresso
{chr(10).join(f'- {t}' for t in snapshot.tasks_in_progress) if snapshot.tasks_in_progress else '- Nenhuma'}

### Arquivos Importantes Modificados
{chr(10).join(f'- {f}' for f in snapshot.important_files) if snapshot.important_files else '- Nenhum'}

### Ultimos Commits
{chr(10).join(f'- {c}' for c in snapshot.last_commits) if snapshot.last_commits else '- Nenhum'}

### Handoffs Pendentes
{chr(10).join(f'- {h.get("agent")}: {h.get("task")}' for h in snapshot.pending_handoffs) if snapshot.pending_handoffs else '- Nenhum'}

---
Continue de onde parou. Use os comandos git e gh para verificar o estado atual se necessario.
"""
        return prompt

    async def compact_agent_context(
        self,
        agent_id: str,
        current_summary: str,
        agent_runner: Any = None
    ) -> bool:
        """
        Compacta contexto de um agente.

        1. Cria snapshot do estado atual
        2. Salva trabalho pendente
        3. Reinicia agente com contexto limpo
        4. Restaura contexto essencial

        Returns:
            True se compactacao foi bem sucedida
        """
        try:
            logger.info(f"[{agent_id}] Iniciando compactacao de contexto...")

            # Criar snapshot
            snapshot = self.create_snapshot(
                agent_id=agent_id,
                summary=current_summary
            )

            # Se temos um runner, parar e reiniciar
            if agent_runner:
                await agent_runner.stop()

                # Construir prompt de restauracao
                restoration_prompt = self.build_restoration_prompt(snapshot)

                # Reiniciar com contexto restaurado
                # O runner sera reiniciado pelo orquestrador

            # Reset usage
            self.agent_contexts[agent_id] = 0.1  # Comecar com 10%

            logger.info(f"[{agent_id}] Contexto compactado com sucesso")
            return True

        except Exception as e:
            logger.error(f"[{agent_id}] Erro ao compactar contexto: {e}")
            return False

    def cleanup_old_snapshots(self, max_age_days: int = 7):
        """Remove snapshots antigos."""
        from datetime import timedelta

        cutoff = datetime.utcnow() - timedelta(days=max_age_days)

        for snapshot_file in self.snapshots_dir.glob("*.json"):
            try:
                # Extrair timestamp do nome do arquivo
                parts = snapshot_file.stem.split('_')
                if len(parts) >= 2:
                    date_str = parts[1]
                    file_date = datetime.strptime(date_str, '%Y%m%d')
                    if file_date < cutoff:
                        snapshot_file.unlink()
                        logger.debug(f"Removed old snapshot: {snapshot_file.name}")
            except Exception as e:
                logger.warning(f"Error processing snapshot {snapshot_file}: {e}")
