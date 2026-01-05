# -*- coding: utf-8 -*-
"""
Lookup Service - Plataforma E
==============================

Servico centralizado para buscar valores de lookup do banco de dados.
Implementa cache com TTL para performance.

Uso:
    from factory.services import LookupService

    # Buscar status de stories
    statuses = LookupService.get_statuses("story")

    # Buscar prioridades
    priorities = LookupService.get_priorities()

    # Buscar complexidade por pontos
    complexity = LookupService.get_complexity_by_points(8)

Author: Plataforma E
"""

import time
import logging
from typing import Dict, List, Any, Optional
from functools import lru_cache

logger = logging.getLogger(__name__)


class CacheEntry:
    """Entrada de cache com TTL."""

    def __init__(self, value: Any, ttl_seconds: int = 300):
        self.value = value
        self.expires_at = time.time() + ttl_seconds

    def is_expired(self) -> bool:
        return time.time() > self.expires_at


class LookupService:
    """
    Servico para buscar valores de lookup do banco de dados.

    Features:
    - Cache com TTL configuravel (padrao: 5 minutos)
    - Fallback para constantes quando banco indisponivel
    - Suporte a multi-tenancy
    - Thread-safe
    """

    # Cache em memoria
    _cache: Dict[str, CacheEntry] = {}

    # TTL padrao em segundos (5 minutos)
    DEFAULT_TTL = 300

    # Flag para indicar se deve usar banco
    _use_database = True

    @classmethod
    def _get_cache_key(cls, key_type: str, tenant_id: Optional[str] = None) -> str:
        """Gera chave de cache."""
        return f"{key_type}:{tenant_id or 'global'}"

    @classmethod
    def _get_from_cache(cls, key: str) -> Optional[Any]:
        """Busca valor do cache se nao expirado."""
        entry = cls._cache.get(key)
        if entry and not entry.is_expired():
            return entry.value
        return None

    @classmethod
    def _set_cache(cls, key: str, value: Any, ttl: int = None):
        """Armazena valor no cache."""
        cls._cache[key] = CacheEntry(value, ttl or cls.DEFAULT_TTL)

    @classmethod
    def clear_cache(cls):
        """Limpa todo o cache."""
        cls._cache.clear()
        logger.info("Cache de lookups limpo")

    @classmethod
    def clear_cache_for_tenant(cls, tenant_id: str):
        """Limpa cache de um tenant especifico."""
        keys_to_remove = [k for k in cls._cache.keys() if k.endswith(f":{tenant_id}")]
        for key in keys_to_remove:
            del cls._cache[key]
        logger.info(f"Cache limpo para tenant {tenant_id}")

    # =========================================================================
    # STATUS
    # =========================================================================

    @classmethod
    def get_statuses(
        cls,
        entity_type: str,
        tenant_id: Optional[str] = None,
        include_inactive: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Retorna lista de status para um tipo de entidade.

        Args:
            entity_type: Tipo de entidade (story, task, project)
            tenant_id: ID do tenant (opcional, usa global se None)
            include_inactive: Se True, inclui status inativos

        Returns:
            Lista de dicionarios com status
        """
        cache_key = cls._get_cache_key(f"status:{entity_type}", tenant_id)

        # Tentar cache primeiro
        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        # Tentar banco de dados
        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import StatusLookup

                with get_session() as session:
                    query = session.query(StatusLookup).filter(
                        StatusLookup.entity_type == entity_type
                    )

                    # Filtrar por tenant (global ou especifico)
                    if tenant_id:
                        query = query.filter(
                            (StatusLookup.tenant_id == tenant_id) |
                            (StatusLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(StatusLookup.tenant_id.is_(None))

                    if not include_inactive:
                        query = query.filter(StatusLookup.is_active == True)

                    query = query.order_by(StatusLookup.sort_order)

                    result = [
                        {
                            "code": s.status_code,
                            "label": s.label,
                            "label_en": s.label_en,
                            "color": s.color,
                            "icon": s.icon,
                            "sort_order": s.sort_order,
                            "is_initial": s.is_initial,
                            "is_final": s.is_final
                        }
                        for s in query.all()
                    ]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar status do banco: {e}")

        # Fallback para constantes
        from factory.constants.lookups import STORY_STATUS, TASK_STATUS, PROJECT_STATUS

        fallback_map = {
            "story": STORY_STATUS,
            "task": TASK_STATUS,
            "project": PROJECT_STATUS
        }

        result = fallback_map.get(entity_type, [])
        if result:
            cls._set_cache(cache_key, result)
        return result

    @classmethod
    def get_status_codes(cls, entity_type: str, tenant_id: Optional[str] = None) -> List[str]:
        """Retorna apenas os codigos de status."""
        statuses = cls.get_statuses(entity_type, tenant_id)
        return [s["code"] for s in statuses]

    @classmethod
    def get_status_by_code(
        cls,
        entity_type: str,
        code: str,
        tenant_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """Retorna um status especifico pelo codigo."""
        statuses = cls.get_statuses(entity_type, tenant_id)
        for status in statuses:
            if status["code"] == code:
                return status
        return None

    # =========================================================================
    # PRIORITIES
    # =========================================================================

    @classmethod
    def get_priorities(
        cls,
        tenant_id: Optional[str] = None,
        include_inactive: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Retorna lista de prioridades.

        Args:
            tenant_id: ID do tenant (opcional)
            include_inactive: Se True, inclui prioridades inativas

        Returns:
            Lista de dicionarios com prioridades
        """
        cache_key = cls._get_cache_key("priorities", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import PriorityLookup

                with get_session() as session:
                    query = session.query(PriorityLookup)

                    if tenant_id:
                        query = query.filter(
                            (PriorityLookup.tenant_id == tenant_id) |
                            (PriorityLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(PriorityLookup.tenant_id.is_(None))

                    if not include_inactive:
                        query = query.filter(PriorityLookup.is_active == True)

                    query = query.order_by(PriorityLookup.sort_order)

                    result = [
                        {
                            "code": p.priority_code,
                            "label": p.label,
                            "label_en": p.label_en,
                            "color": p.color,
                            "icon": p.icon,
                            "numeric_value": p.numeric_value,
                            "sort_order": p.sort_order,
                            "is_default": p.is_default
                        }
                        for p in query.all()
                    ]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar prioridades do banco: {e}")

        # Fallback
        from factory.constants.lookups import PRIORITIES
        cls._set_cache(cache_key, PRIORITIES)
        return PRIORITIES

    @classmethod
    def get_priority_codes(cls, tenant_id: Optional[str] = None) -> List[str]:
        """Retorna apenas os codigos de prioridade."""
        priorities = cls.get_priorities(tenant_id)
        return [p["code"] for p in priorities]

    @classmethod
    def get_priority_by_code(
        cls,
        code: str,
        tenant_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """Retorna uma prioridade especifica pelo codigo."""
        priorities = cls.get_priorities(tenant_id)
        for priority in priorities:
            if priority["code"] == code:
                return priority
        return None

    # =========================================================================
    # COMPLEXITY
    # =========================================================================

    @classmethod
    def get_complexities(
        cls,
        tenant_id: Optional[str] = None,
        include_inactive: bool = False
    ) -> List[Dict[str, Any]]:
        """Retorna lista de complexidades."""
        cache_key = cls._get_cache_key("complexity", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import ComplexityLookup

                with get_session() as session:
                    query = session.query(ComplexityLookup)

                    if tenant_id:
                        query = query.filter(
                            (ComplexityLookup.tenant_id == tenant_id) |
                            (ComplexityLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(ComplexityLookup.tenant_id.is_(None))

                    if not include_inactive:
                        query = query.filter(ComplexityLookup.is_active == True)

                    query = query.order_by(ComplexityLookup.sort_order)

                    result = [
                        {
                            "code": c.complexity_code,
                            "label": c.label,
                            "label_en": c.label_en,
                            "color": c.color,
                            "min_points": c.min_points,
                            "max_points": c.max_points,
                            "estimated_hours_min": c.estimated_hours_min,
                            "estimated_hours_max": c.estimated_hours_max,
                            "sort_order": c.sort_order
                        }
                        for c in query.all()
                    ]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar complexidades do banco: {e}")

        # Fallback
        from factory.constants.lookups import COMPLEXITY
        cls._set_cache(cache_key, COMPLEXITY)
        return COMPLEXITY

    @classmethod
    def get_complexity_by_points(
        cls,
        points: int,
        tenant_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """Retorna complexidade baseada nos story points."""
        complexities = cls.get_complexities(tenant_id)
        for complexity in complexities:
            min_p = complexity.get("min_points", 0)
            max_p = complexity.get("max_points", 999)
            if min_p <= points <= max_p:
                return complexity
        # Fallback para ultimo (very_high)
        return complexities[-1] if complexities else None

    # =========================================================================
    # STORY POINTS (FIBONACCI)
    # =========================================================================

    @classmethod
    def get_story_points(cls, tenant_id: Optional[str] = None) -> List[int]:
        """
        Retorna lista de valores validos de story points.

        Por padrao retorna sequencia Fibonacci: [0, 1, 2, 3, 5, 8, 13, 21]
        """
        cache_key = cls._get_cache_key("story_points", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import StoryPointsLookup

                with get_session() as session:
                    query = session.query(StoryPointsLookup).filter(
                        StoryPointsLookup.is_active == True
                    )

                    if tenant_id:
                        query = query.filter(
                            (StoryPointsLookup.tenant_id == tenant_id) |
                            (StoryPointsLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(StoryPointsLookup.tenant_id.is_(None))

                    query = query.order_by(StoryPointsLookup.sort_order)

                    result = [sp.points_value for sp in query.all()]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar story points do banco: {e}")

        # Fallback
        from factory.constants.lookups import FIBONACCI_POINTS
        cls._set_cache(cache_key, FIBONACCI_POINTS)
        return FIBONACCI_POINTS

    @classmethod
    def is_valid_story_points(cls, points: int, tenant_id: Optional[str] = None) -> bool:
        """Valida se um valor de story points e valido."""
        valid_points = cls.get_story_points(tenant_id)
        return points in valid_points

    # =========================================================================
    # TASK TYPES
    # =========================================================================

    @classmethod
    def get_task_types(
        cls,
        tenant_id: Optional[str] = None,
        include_inactive: bool = False
    ) -> List[Dict[str, Any]]:
        """Retorna lista de tipos de task."""
        cache_key = cls._get_cache_key("task_types", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import TaskTypeLookup

                with get_session() as session:
                    query = session.query(TaskTypeLookup)

                    if tenant_id:
                        query = query.filter(
                            (TaskTypeLookup.tenant_id == tenant_id) |
                            (TaskTypeLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(TaskTypeLookup.tenant_id.is_(None))

                    if not include_inactive:
                        query = query.filter(TaskTypeLookup.is_active == True)

                    query = query.order_by(TaskTypeLookup.sort_order)

                    result = [
                        {
                            "code": t.type_code,
                            "label": t.label,
                            "label_en": t.label_en,
                            "color": t.color,
                            "icon": t.icon,
                            "sort_order": t.sort_order,
                            "is_default": t.is_default
                        }
                        for t in query.all()
                    ]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar tipos de task do banco: {e}")

        # Fallback
        from factory.constants.lookups import TASK_TYPES
        cls._set_cache(cache_key, TASK_TYPES)
        return TASK_TYPES

    @classmethod
    def get_task_type_codes(cls, tenant_id: Optional[str] = None) -> List[str]:
        """Retorna apenas os codigos de tipos de task."""
        task_types = cls.get_task_types(tenant_id)
        return [t["code"] for t in task_types]

    # =========================================================================
    # ROLES
    # =========================================================================

    @classmethod
    def get_roles(
        cls,
        tenant_id: Optional[str] = None,
        include_inactive: bool = False
    ) -> List[Dict[str, Any]]:
        """Retorna lista de papeis de usuario."""
        cache_key = cls._get_cache_key("roles", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import RoleLookup

                with get_session() as session:
                    query = session.query(RoleLookup)

                    if tenant_id:
                        query = query.filter(
                            (RoleLookup.tenant_id == tenant_id) |
                            (RoleLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(RoleLookup.tenant_id.is_(None))

                    if not include_inactive:
                        query = query.filter(RoleLookup.is_active == True)

                    query = query.order_by(RoleLookup.sort_order)

                    result = [
                        {
                            "code": r.role_code,
                            "label": r.label,
                            "label_en": r.label_en,
                            "description": r.description,
                            "permissions": r.permissions,
                            "is_system": r.is_system,
                            "sort_order": r.sort_order
                        }
                        for r in query.all()
                    ]

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar roles do banco: {e}")

        # Fallback
        from factory.constants.lookups import ROLES
        cls._set_cache(cache_key, ROLES)
        return ROLES

    @classmethod
    def get_role_codes(cls, tenant_id: Optional[str] = None) -> List[str]:
        """Retorna apenas os codigos de roles."""
        roles = cls.get_roles(tenant_id)
        return [r["code"] for r in roles]

    # =========================================================================
    # WIP LIMITS
    # =========================================================================

    @classmethod
    def get_wip_limits(
        cls,
        entity_type: str,
        tenant_id: Optional[str] = None
    ) -> Dict[str, int]:
        """
        Retorna limites WIP para um tipo de entidade.

        Args:
            entity_type: Tipo de entidade (story, task)
            tenant_id: ID do tenant (opcional)

        Returns:
            Dicionario {column_code: limit_value}
        """
        cache_key = cls._get_cache_key(f"wip:{entity_type}", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import WipLimitLookup

                with get_session() as session:
                    query = session.query(WipLimitLookup).filter(
                        WipLimitLookup.entity_type == entity_type
                    )

                    if tenant_id:
                        query = query.filter(
                            (WipLimitLookup.tenant_id == tenant_id) |
                            (WipLimitLookup.tenant_id.is_(None))
                        )
                    else:
                        query = query.filter(WipLimitLookup.tenant_id.is_(None))

                    result = {w.column_code: w.limit_value for w in query.all()}

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar WIP limits do banco: {e}")

        # Fallback
        from factory.constants.lookups import WIP_LIMITS
        result = WIP_LIMITS.get(entity_type, {})
        cls._set_cache(cache_key, result)
        return result

    @classmethod
    def get_wip_limit(
        cls,
        entity_type: str,
        column_code: str,
        tenant_id: Optional[str] = None
    ) -> int:
        """Retorna limite WIP para uma coluna especifica."""
        from factory.constants.lookups import DEFAULT_WIP_LIMIT

        limits = cls.get_wip_limits(entity_type, tenant_id)
        return limits.get(column_code, DEFAULT_WIP_LIMIT)

    # =========================================================================
    # SYSTEM CONFIG
    # =========================================================================

    @classmethod
    def get_config(
        cls,
        config_key: str,
        tenant_id: Optional[str] = None,
        default: Any = None
    ) -> Any:
        """
        Retorna valor de configuracao do sistema.

        Args:
            config_key: Chave da configuracao
            tenant_id: ID do tenant (opcional)
            default: Valor padrao se nao encontrado

        Returns:
            Valor da configuracao (tipado corretamente)
        """
        cache_key = cls._get_cache_key(f"config:{config_key}", tenant_id)

        cached = cls._get_from_cache(cache_key)
        if cached is not None:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import SystemConfig

                with get_session() as session:
                    query = session.query(SystemConfig).filter(
                        SystemConfig.config_key == config_key
                    )

                    if tenant_id:
                        # Tentar especifico do tenant primeiro
                        result = query.filter(SystemConfig.tenant_id == tenant_id).first()
                        if not result:
                            # Fallback para global
                            result = query.filter(SystemConfig.tenant_id.is_(None)).first()
                    else:
                        result = query.filter(SystemConfig.tenant_id.is_(None)).first()

                    if result:
                        value = result.get_typed_value()
                        cls._set_cache(cache_key, value)
                        return value

            except Exception as e:
                logger.warning(f"Erro ao buscar config do banco: {e}")

        # Fallback
        from factory.constants.lookups import SYSTEM_CONFIG_DEFAULTS

        config_def = SYSTEM_CONFIG_DEFAULTS.get(config_key)
        if config_def:
            value = config_def.get("value", default)
            data_type = config_def.get("data_type", "string")

            # Converter para tipo correto
            if data_type == "number":
                value = int(value) if value else default
            elif data_type == "boolean":
                value = str(value).lower() in ("true", "1", "yes", "on")

            cls._set_cache(cache_key, value)
            return value

        return default

    # =========================================================================
    # AGENT SKILLS
    # =========================================================================

    @classmethod
    def get_agent_skills(cls) -> Dict[str, List[str]]:
        """Retorna mapeamento de agentes para keywords."""
        cache_key = "agent_skills:global"

        cached = cls._get_from_cache(cache_key)
        if cached:
            return cached

        if cls._use_database:
            try:
                from factory.database.connection import get_session
                from factory.database.lookup_models import AgentSkillLookup

                with get_session() as session:
                    query = session.query(AgentSkillLookup).filter(
                        AgentSkillLookup.is_active == True
                    ).order_by(AgentSkillLookup.priority.desc())

                    result: Dict[str, List[str]] = {}
                    for skill in query.all():
                        if skill.agent_code not in result:
                            result[skill.agent_code] = []
                        result[skill.agent_code].append(skill.keyword)

                    if result:
                        cls._set_cache(cache_key, result)
                        return result

            except Exception as e:
                logger.warning(f"Erro ao buscar agent skills do banco: {e}")

        # Fallback
        from factory.constants.lookups import AGENT_SKILLS
        cls._set_cache(cache_key, AGENT_SKILLS)
        return AGENT_SKILLS

    @classmethod
    def get_agent_for_keyword(cls, keyword: str) -> str:
        """Retorna o agente responsavel por uma keyword."""
        from factory.constants.lookups import DEFAULT_AGENT

        keyword = keyword.lower()
        skills = cls.get_agent_skills()

        for agent, keywords in skills.items():
            if any(k in keyword for k in keywords):
                return agent

        return DEFAULT_AGENT

    # =========================================================================
    # UTILITY
    # =========================================================================

    @classmethod
    def disable_database(cls):
        """Desabilita uso do banco (para testes)."""
        cls._use_database = False
        cls.clear_cache()

    @classmethod
    def enable_database(cls):
        """Habilita uso do banco."""
        cls._use_database = True
        cls.clear_cache()

    @classmethod
    def set_ttl(cls, ttl_seconds: int):
        """Define TTL padrao do cache."""
        cls.DEFAULT_TTL = ttl_seconds
