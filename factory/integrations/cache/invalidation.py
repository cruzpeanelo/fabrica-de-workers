# -*- coding: utf-8 -*-
"""
Cache Invalidation
==================
Sistema de invalidacao de cache baseado em eventos.

Issue #365 - Terminal A
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set
from uuid import uuid4

logger = logging.getLogger(__name__)


class InvalidationEvent(str, Enum):
    """Eventos que podem disparar invalidacao"""
    # Story events
    STORY_CREATED = "story.created"
    STORY_UPDATED = "story.updated"
    STORY_DELETED = "story.deleted"
    STORY_STATUS_CHANGED = "story.status_changed"

    # User events
    USER_CREATED = "user.created"
    USER_UPDATED = "user.updated"
    USER_DELETED = "user.deleted"

    # Project events
    PROJECT_CREATED = "project.created"
    PROJECT_UPDATED = "project.updated"
    PROJECT_DELETED = "project.deleted"

    # Integration events
    JIRA_SYNC = "jira.sync"
    GITHUB_PUSH = "github.push"
    AZURE_UPDATE = "azure.update"

    # Manual invalidation
    MANUAL = "manual"
    FULL_CLEAR = "full_clear"


@dataclass
class InvalidationRule:
    """
    Regra de invalidacao.

    Define quais caches invalidar quando um evento ocorre.

    Attributes:
        rule_id: ID unico da regra
        event: Evento que dispara a regra
        patterns: Patterns de chaves a invalidar
        tags: Tags a invalidar
        tenant_specific: Se aplica apenas ao tenant do evento
        enabled: Se a regra esta ativa
    """
    rule_id: str = field(default_factory=lambda: str(uuid4()))
    event: InvalidationEvent = InvalidationEvent.MANUAL
    patterns: List[str] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    tenant_specific: bool = True
    enabled: bool = True
    description: str = ""


@dataclass
class InvalidationResult:
    """Resultado de invalidacao"""
    event: InvalidationEvent
    rules_applied: int
    keys_invalidated: int
    tenant_id: Optional[str]
    timestamp: datetime = field(default_factory=datetime.utcnow)
    details: Dict[str, Any] = field(default_factory=dict)


# Regras padrao de invalidacao
DEFAULT_INVALIDATION_RULES: List[InvalidationRule] = [
    # Story events
    InvalidationRule(
        event=InvalidationEvent.STORY_CREATED,
        tags=["issue", "project"],
        description="Invalida cache de issues quando story e criada"
    ),
    InvalidationRule(
        event=InvalidationEvent.STORY_UPDATED,
        patterns=["issue:{story_id}*"],
        tags=["issue"],
        description="Invalida cache da story atualizada"
    ),
    InvalidationRule(
        event=InvalidationEvent.STORY_DELETED,
        patterns=["issue:{story_id}*"],
        tags=["issue"],
        description="Invalida cache da story deletada"
    ),
    InvalidationRule(
        event=InvalidationEvent.STORY_STATUS_CHANGED,
        patterns=["issue:{story_id}*"],
        tags=["issue"],
        description="Invalida cache quando status muda"
    ),

    # User events
    InvalidationRule(
        event=InvalidationEvent.USER_CREATED,
        tags=["user"],
        description="Invalida cache de usuarios"
    ),
    InvalidationRule(
        event=InvalidationEvent.USER_UPDATED,
        patterns=["user:{user_id}*"],
        tags=["user"],
        description="Invalida cache do usuario atualizado"
    ),
    InvalidationRule(
        event=InvalidationEvent.USER_DELETED,
        patterns=["user:{user_id}*"],
        tags=["user"],
        description="Invalida cache do usuario deletado"
    ),

    # Project events
    InvalidationRule(
        event=InvalidationEvent.PROJECT_CREATED,
        tags=["project"],
        description="Invalida cache de projetos"
    ),
    InvalidationRule(
        event=InvalidationEvent.PROJECT_UPDATED,
        patterns=["project:{project_id}*"],
        tags=["project", "issue"],
        description="Invalida cache do projeto e suas issues"
    ),
    InvalidationRule(
        event=InvalidationEvent.PROJECT_DELETED,
        patterns=["project:{project_id}*"],
        tags=["project", "issue"],
        description="Invalida cache do projeto deletado"
    ),

    # Integration events
    InvalidationRule(
        event=InvalidationEvent.JIRA_SYNC,
        tags=["issue", "metadata"],
        description="Invalida cache apos sync do Jira"
    ),
    InvalidationRule(
        event=InvalidationEvent.GITHUB_PUSH,
        tags=["issue"],
        description="Invalida cache apos push do GitHub"
    ),
    InvalidationRule(
        event=InvalidationEvent.AZURE_UPDATE,
        tags=["issue"],
        description="Invalida cache apos update do Azure"
    ),
]


class CacheInvalidator:
    """
    Sistema de invalidacao de cache baseado em eventos.

    Conecta eventos do sistema a invalidacoes de cache.

    Exemplo:
        invalidator = CacheInvalidator(cache_manager)

        # Adicionar regra customizada
        invalidator.add_rule(InvalidationRule(
            event=InvalidationEvent.STORY_UPDATED,
            patterns=["dashboard:*"],
            tags=["dashboard"]
        ))

        # Disparar invalidacao
        result = await invalidator.invalidate(
            InvalidationEvent.STORY_UPDATED,
            tenant_id="tenant-123",
            context={"story_id": "STR-001"}
        )
    """

    def __init__(self, cache_manager=None, use_defaults: bool = True):
        """
        Args:
            cache_manager: Instancia do CacheManager
            use_defaults: Se deve carregar regras padrao
        """
        self._cache_manager = cache_manager
        self._rules: Dict[InvalidationEvent, List[InvalidationRule]] = {}
        self._handlers: Dict[InvalidationEvent, List[Callable]] = {}
        self._history: List[InvalidationResult] = []

        if use_defaults:
            for rule in DEFAULT_INVALIDATION_RULES:
                self.add_rule(rule)

    def set_cache_manager(self, cache_manager):
        """Define cache manager"""
        self._cache_manager = cache_manager

    def add_rule(self, rule: InvalidationRule):
        """Adiciona regra de invalidacao"""
        if rule.event not in self._rules:
            self._rules[rule.event] = []
        self._rules[rule.event].append(rule)
        logger.debug(f"Invalidation rule added: {rule.event.value}")

    def remove_rule(self, rule_id: str) -> bool:
        """Remove regra por ID"""
        for event, rules in self._rules.items():
            for rule in rules:
                if rule.rule_id == rule_id:
                    rules.remove(rule)
                    return True
        return False

    def get_rules(
        self,
        event: Optional[InvalidationEvent] = None
    ) -> List[InvalidationRule]:
        """Lista regras, opcionalmente filtradas por evento"""
        if event:
            return self._rules.get(event, [])

        all_rules = []
        for rules in self._rules.values():
            all_rules.extend(rules)
        return all_rules

    def on_event(self, event: InvalidationEvent):
        """
        Decorator para registrar handler de evento.

        Exemplo:
            @invalidator.on_event(InvalidationEvent.STORY_UPDATED)
            async def handle_story_update(context):
                # Logica adicional
                pass
        """
        def decorator(func: Callable):
            if event not in self._handlers:
                self._handlers[event] = []
            self._handlers[event].append(func)
            return func
        return decorator

    async def invalidate(
        self,
        event: InvalidationEvent,
        tenant_id: Optional[str] = None,
        context: Optional[Dict[str, Any]] = None
    ) -> InvalidationResult:
        """
        Dispara invalidacao baseada em evento.

        Args:
            event: Evento de invalidacao
            tenant_id: ID do tenant
            context: Contexto com variaveis para substituicao

        Returns:
            Resultado da invalidacao
        """
        context = context or {}
        rules_applied = 0
        total_invalidated = 0

        rules = self._rules.get(event, [])

        for rule in rules:
            if not rule.enabled:
                continue

            # Pula regras tenant-specific se nao tiver tenant
            if rule.tenant_specific and not tenant_id:
                continue

            # Processa patterns
            for pattern in rule.patterns:
                resolved_pattern = self._resolve_pattern(pattern, context)
                try:
                    count = await self._cache_manager._backend.invalidate_by_pattern(
                        resolved_pattern
                    )
                    total_invalidated += count
                except Exception as e:
                    logger.error(f"Pattern invalidation error: {e}")

            # Processa tags
            for tag in rule.tags:
                try:
                    if rule.tenant_specific and tenant_id:
                        # Invalida com tenant
                        count = await self._cache_manager.invalidate_by_tag(
                            f"tenant:{tenant_id}"
                        )
                    else:
                        count = await self._cache_manager.invalidate_by_tag(tag)
                    total_invalidated += count
                except Exception as e:
                    logger.error(f"Tag invalidation error: {e}")

            rules_applied += 1

        # Executa handlers customizados
        handlers = self._handlers.get(event, [])
        for handler in handlers:
            try:
                if asyncio.iscoroutinefunction(handler):
                    await handler(context)
                else:
                    handler(context)
            except Exception as e:
                logger.error(f"Handler error for {event.value}: {e}")

        result = InvalidationResult(
            event=event,
            rules_applied=rules_applied,
            keys_invalidated=total_invalidated,
            tenant_id=tenant_id,
            details=context
        )

        self._history.append(result)
        if len(self._history) > 1000:
            self._history = self._history[-500:]

        logger.info(
            f"Cache invalidation: {event.value} - "
            f"{rules_applied} rules, {total_invalidated} keys"
        )

        return result

    def _resolve_pattern(
        self,
        pattern: str,
        context: Dict[str, Any]
    ) -> str:
        """Substitui variaveis no pattern"""
        resolved = pattern
        for key, value in context.items():
            resolved = resolved.replace(f"{{{key}}}", str(value))
        return resolved

    async def invalidate_full(self, tenant_id: Optional[str] = None):
        """Invalida todo o cache (ou de um tenant)"""
        if tenant_id:
            await self._cache_manager.invalidate_tenant(tenant_id)
        else:
            await self._cache_manager.clear()

    def get_history(
        self,
        limit: int = 100,
        event: Optional[InvalidationEvent] = None
    ) -> List[InvalidationResult]:
        """Retorna historico de invalidacoes"""
        history = self._history
        if event:
            history = [r for r in history if r.event == event]
        return history[-limit:]

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas de invalidacao"""
        total_invalidations = len(self._history)
        events_count = {}

        for result in self._history:
            event_name = result.event.value
            if event_name not in events_count:
                events_count[event_name] = 0
            events_count[event_name] += 1

        return {
            "total_invalidations": total_invalidations,
            "rules_count": sum(len(r) for r in self._rules.values()),
            "handlers_count": sum(len(h) for h in self._handlers.values()),
            "events_count": events_count
        }


# Import necessario para handlers async
import asyncio

# Singleton global
_invalidator: Optional[CacheInvalidator] = None


def get_invalidator(cache_manager=None) -> CacheInvalidator:
    """Obtem instancia global do invalidator"""
    global _invalidator
    if _invalidator is None:
        _invalidator = CacheInvalidator(cache_manager=cache_manager)
    elif cache_manager and _invalidator._cache_manager is None:
        _invalidator.set_cache_manager(cache_manager)
    return _invalidator


def reset_invalidator():
    """Reseta invalidator global (para testes)"""
    global _invalidator
    _invalidator = None


# Funcao helper para disparar eventos
async def trigger_invalidation(
    event: InvalidationEvent,
    tenant_id: Optional[str] = None,
    **context
) -> InvalidationResult:
    """
    Dispara evento de invalidacao de forma simples.

    Exemplo:
        await trigger_invalidation(
            InvalidationEvent.STORY_UPDATED,
            tenant_id="tenant-123",
            story_id="STR-001"
        )
    """
    invalidator = get_invalidator()
    return await invalidator.invalidate(event, tenant_id, context)
