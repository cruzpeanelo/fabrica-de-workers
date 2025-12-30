# -*- coding: utf-8 -*-
"""
Conflict Resolver
=================
Resolucao de conflitos de sincronizacao.

Issue #364 - Terminal A
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class ConflictStrategy(str, Enum):
    """Estrategia de resolucao de conflitos"""
    JIRA_WINS = "jira_wins"        # Jira sempre prevalece
    SYSTEM_WINS = "system_wins"    # Sistema sempre prevalece
    NEWEST_WINS = "newest_wins"    # Mais recente prevalece
    MANUAL = "manual"              # Requer resolucao manual
    MERGE = "merge"                # Tenta mesclar alteracoes


@dataclass
class ConflictResolution:
    """
    Resultado da resolucao de conflito.

    Attributes:
        field: Campo em conflito
        jira_value: Valor no Jira
        system_value: Valor no sistema
        resolved_value: Valor resolvido
        strategy_used: Estrategia usada
        requires_manual: Se requer intervencao manual
    """
    field: str
    jira_value: Any
    system_value: Any
    resolved_value: Any
    strategy_used: ConflictStrategy
    requires_manual: bool = False
    notes: str = ""


@dataclass
class SyncConflict:
    """
    Conflito detectado durante sincronizacao.

    Attributes:
        story_id: ID da story no sistema
        jira_key: Key da issue no Jira
        field: Campo em conflito
        jira_value: Valor no Jira
        system_value: Valor no sistema
        jira_updated_at: Ultima atualizacao no Jira
        system_updated_at: Ultima atualizacao no sistema
        detected_at: Quando o conflito foi detectado
    """
    story_id: str
    jira_key: str
    field: str
    jira_value: Any
    system_value: Any
    jira_updated_at: Optional[datetime] = None
    system_updated_at: Optional[datetime] = None
    detected_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "story_id": self.story_id,
            "jira_key": self.jira_key,
            "field": self.field,
            "jira_value": str(self.jira_value),
            "system_value": str(self.system_value),
            "jira_updated_at": self.jira_updated_at.isoformat() if self.jira_updated_at else None,
            "system_updated_at": self.system_updated_at.isoformat() if self.system_updated_at else None,
            "detected_at": self.detected_at.isoformat()
        }


class ConflictResolver:
    """
    Resolvedor de conflitos de sincronizacao.

    Exemplo:
        resolver = ConflictResolver(default_strategy=ConflictStrategy.NEWEST_WINS)

        # Configurar estrategia por campo
        resolver.set_field_strategy("status", ConflictStrategy.JIRA_WINS)
        resolver.set_field_strategy("description", ConflictStrategy.MERGE)

        # Resolver conflito
        resolution = resolver.resolve(conflict)
    """

    # Campos que podem ser mesclados
    MERGEABLE_FIELDS = {"description", "tags", "labels", "comments"}

    def __init__(
        self,
        default_strategy: ConflictStrategy = ConflictStrategy.NEWEST_WINS,
        field_strategies: Optional[Dict[str, ConflictStrategy]] = None
    ):
        self.default_strategy = default_strategy
        self._field_strategies = field_strategies or {}
        self._pending_conflicts: List[SyncConflict] = []
        self._resolved_conflicts: List[ConflictResolution] = []

    def set_field_strategy(self, field: str, strategy: ConflictStrategy):
        """Define estrategia para campo especifico"""
        self._field_strategies[field] = strategy

    def get_strategy(self, field: str) -> ConflictStrategy:
        """Obtem estrategia para campo"""
        return self._field_strategies.get(field, self.default_strategy)

    def detect_conflict(
        self,
        story_id: str,
        jira_key: str,
        field: str,
        jira_value: Any,
        system_value: Any,
        jira_updated_at: Optional[datetime] = None,
        system_updated_at: Optional[datetime] = None
    ) -> Optional[SyncConflict]:
        """
        Detecta se ha conflito entre valores.

        Args:
            story_id: ID da story
            jira_key: Key do Jira
            field: Nome do campo
            jira_value: Valor no Jira
            system_value: Valor no sistema
            jira_updated_at: Ultima atualizacao Jira
            system_updated_at: Ultima atualizacao sistema

        Returns:
            SyncConflict se houver conflito, None caso contrario
        """
        # Normaliza valores para comparacao
        jira_normalized = self._normalize_value(jira_value)
        system_normalized = self._normalize_value(system_value)

        # Se valores sao iguais, nao ha conflito
        if jira_normalized == system_normalized:
            return None

        # Se um dos valores e vazio, nao e conflito (e uma atualizacao)
        if jira_normalized is None or system_normalized is None:
            return None

        conflict = SyncConflict(
            story_id=story_id,
            jira_key=jira_key,
            field=field,
            jira_value=jira_value,
            system_value=system_value,
            jira_updated_at=jira_updated_at,
            system_updated_at=system_updated_at
        )

        self._pending_conflicts.append(conflict)
        logger.info(
            f"Conflito detectado: {jira_key}/{field} - "
            f"Jira: {jira_value} vs Sistema: {system_value}"
        )

        return conflict

    def _normalize_value(self, value: Any) -> Any:
        """Normaliza valor para comparacao"""
        if value is None:
            return None
        if isinstance(value, str):
            return value.strip().lower()
        if isinstance(value, list):
            return sorted([self._normalize_value(v) for v in value])
        return value

    def resolve(self, conflict: SyncConflict) -> ConflictResolution:
        """
        Resolve um conflito usando a estrategia configurada.

        Args:
            conflict: Conflito a resolver

        Returns:
            Resolucao do conflito
        """
        strategy = self.get_strategy(conflict.field)

        if strategy == ConflictStrategy.JIRA_WINS:
            return self._resolve_jira_wins(conflict)

        elif strategy == ConflictStrategy.SYSTEM_WINS:
            return self._resolve_system_wins(conflict)

        elif strategy == ConflictStrategy.NEWEST_WINS:
            return self._resolve_newest_wins(conflict)

        elif strategy == ConflictStrategy.MERGE:
            return self._resolve_merge(conflict)

        else:  # MANUAL
            return self._resolve_manual(conflict)

    def _resolve_jira_wins(self, conflict: SyncConflict) -> ConflictResolution:
        """Jira sempre prevalece"""
        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=conflict.jira_value,
            strategy_used=ConflictStrategy.JIRA_WINS,
            notes="Jira value selected as source of truth"
        )
        self._resolved_conflicts.append(resolution)
        return resolution

    def _resolve_system_wins(self, conflict: SyncConflict) -> ConflictResolution:
        """Sistema sempre prevalece"""
        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=conflict.system_value,
            strategy_used=ConflictStrategy.SYSTEM_WINS,
            notes="System value selected as source of truth"
        )
        self._resolved_conflicts.append(resolution)
        return resolution

    def _resolve_newest_wins(self, conflict: SyncConflict) -> ConflictResolution:
        """Mais recente prevalece"""
        jira_time = conflict.jira_updated_at or datetime.min
        system_time = conflict.system_updated_at or datetime.min

        if jira_time >= system_time:
            resolved_value = conflict.jira_value
            notes = f"Jira is newer ({jira_time})"
        else:
            resolved_value = conflict.system_value
            notes = f"System is newer ({system_time})"

        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=resolved_value,
            strategy_used=ConflictStrategy.NEWEST_WINS,
            notes=notes
        )
        self._resolved_conflicts.append(resolution)
        return resolution

    def _resolve_merge(self, conflict: SyncConflict) -> ConflictResolution:
        """Tenta mesclar valores"""
        if conflict.field not in self.MERGEABLE_FIELDS:
            # Se nao e mesclavel, usa newest_wins
            return self._resolve_newest_wins(conflict)

        jira_val = conflict.jira_value
        system_val = conflict.system_value

        # Merge de listas
        if isinstance(jira_val, list) and isinstance(system_val, list):
            merged = list(set(jira_val + system_val))
            notes = f"Merged {len(jira_val)} + {len(system_val)} items"

        # Merge de strings (descricao)
        elif isinstance(jira_val, str) and isinstance(system_val, str):
            if jira_val in system_val:
                merged = system_val
                notes = "Jira content is subset of system"
            elif system_val in jira_val:
                merged = jira_val
                notes = "System content is subset of Jira"
            else:
                # Concatena com separador
                merged = f"{jira_val}\n\n---\n\n{system_val}"
                notes = "Concatenated both versions"
        else:
            # Fallback para newest_wins
            return self._resolve_newest_wins(conflict)

        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=merged,
            strategy_used=ConflictStrategy.MERGE,
            notes=notes
        )
        self._resolved_conflicts.append(resolution)
        return resolution

    def _resolve_manual(self, conflict: SyncConflict) -> ConflictResolution:
        """Marca para resolucao manual"""
        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=None,
            strategy_used=ConflictStrategy.MANUAL,
            requires_manual=True,
            notes="Requires manual resolution"
        )
        self._resolved_conflicts.append(resolution)
        return resolution

    def resolve_manual(
        self,
        conflict: SyncConflict,
        selected_value: Any,
        notes: str = ""
    ) -> ConflictResolution:
        """
        Resolve conflito manualmente.

        Args:
            conflict: Conflito a resolver
            selected_value: Valor selecionado
            notes: Notas sobre a resolucao

        Returns:
            Resolucao do conflito
        """
        resolution = ConflictResolution(
            field=conflict.field,
            jira_value=conflict.jira_value,
            system_value=conflict.system_value,
            resolved_value=selected_value,
            strategy_used=ConflictStrategy.MANUAL,
            requires_manual=False,
            notes=f"Manually resolved: {notes}"
        )
        self._resolved_conflicts.append(resolution)

        # Remove da lista de pendentes
        self._pending_conflicts = [
            c for c in self._pending_conflicts
            if not (c.story_id == conflict.story_id and c.field == conflict.field)
        ]

        return resolution

    def get_pending_conflicts(self) -> List[SyncConflict]:
        """Retorna conflitos pendentes de resolucao manual"""
        return [
            c for c in self._pending_conflicts
            if self.get_strategy(c.field) == ConflictStrategy.MANUAL
        ]

    def get_all_conflicts(self) -> List[SyncConflict]:
        """Retorna todos os conflitos pendentes"""
        return self._pending_conflicts.copy()

    def get_resolved_conflicts(self) -> List[ConflictResolution]:
        """Retorna historico de resolucoes"""
        return self._resolved_conflicts.copy()

    def clear_history(self):
        """Limpa historico"""
        self._pending_conflicts.clear()
        self._resolved_conflicts.clear()
