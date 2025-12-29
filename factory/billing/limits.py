# -*- coding: utf-8 -*-
"""
Verificador de Limites por Plano
=================================

Implementa:
- Verificacao de limites antes de acoes
- Decorator para endpoints
- Enforcement de quotas

Autor: Fabrica de Agentes
"""

import logging
from typing import Optional, Dict, Any, Tuple, Callable
from functools import wraps
from datetime import datetime

from fastapi import HTTPException, Request
from sqlalchemy.orm import Session
from sqlalchemy import and_, func

from .models import (
    Tenant, Plan, Subscription, Usage,
    TenantStatus, SubscriptionStatus, UsageMetric
)
from .middleware import get_current_tenant_id

# Configurar logging
logger = logging.getLogger(__name__)


class LimitExceededError(Exception):
    """Excecao quando limite e excedido"""

    def __init__(self, metric: str, current: int, limit: int, message: str = None):
        self.metric = metric
        self.current = current
        self.limit = limit
        self.message = message or f"Limite de {metric} excedido: {current}/{limit}"
        super().__init__(self.message)


class PlanLimitsChecker:
    """
    Verificador de limites de plano.

    Uso:
        checker = PlanLimitsChecker(db_session)

        # Verificar antes de criar projeto
        if not checker.can_create_project(tenant_id):
            raise HTTPException(403, "Limite de projetos atingido")

        # Ou usar decorator
        @checker.require_quota("max_projects")
        async def create_project():
            ...
    """

    def __init__(self, db: Session):
        """
        Inicializa o verificador.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db

    def _get_tenant_with_plan(self, tenant_id: str) -> Tuple[Optional[Tenant], Optional[Plan]]:
        """
        Obtem tenant e plano ativo.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (Tenant, Plan)
        """
        tenant = self.db.query(Tenant).filter(
            Tenant.tenant_id == tenant_id
        ).first()

        if not tenant:
            return None, None

        subscription = self.db.query(Subscription).filter(
            and_(
                Subscription.tenant_id == tenant_id,
                Subscription.status.in_([
                    SubscriptionStatus.ACTIVE.value,
                    SubscriptionStatus.TRIALING.value
                ])
            )
        ).first()

        if not subscription:
            return tenant, None

        plan = self.db.query(Plan).filter(
            Plan.plan_id == subscription.plan_id
        ).first()

        return tenant, plan

    def get_effective_limit(
        self,
        tenant_id: str,
        limit_name: str
    ) -> Optional[int]:
        """
        Obtem o limite efetivo para um recurso.
        Considera limites customizados do tenant primeiro.

        Args:
            tenant_id: ID do tenant
            limit_name: Nome do limite (max_users, max_projects, etc)

        Returns:
            Limite ou None se ilimitado
        """
        tenant, plan = self._get_tenant_with_plan(tenant_id)

        if not tenant:
            return 0  # Tenant nao existe

        # Primeiro, verificar limites customizados
        if tenant.custom_limits and limit_name in tenant.custom_limits:
            return tenant.custom_limits[limit_name]

        # Depois, verificar limite do plano
        if plan and plan.limits:
            return plan.limits.get(limit_name)

        return None  # Sem limite

    # =========================================================================
    # VERIFICACOES ESPECIFICAS
    # =========================================================================

    def can_create_project(self, tenant_id: str) -> Tuple[bool, str]:
        """
        Verifica se pode criar mais projetos.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (pode_criar, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, "max_projects")

        if limit is None:
            return True, "Sem limite de projetos"

        # Contar projetos existentes
        from ..database.models import Project
        current_count = self.db.query(func.count(Project.id)).filter(
            Project.tenant_id == tenant_id
        ).scalar() or 0

        if current_count >= limit:
            return False, f"Limite de projetos atingido ({current_count}/{limit}). Faca upgrade do plano."

        return True, f"Pode criar mais {limit - current_count} projetos"

    def can_add_user(self, tenant_id: str) -> Tuple[bool, str]:
        """
        Verifica se pode adicionar mais usuarios.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (pode_adicionar, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, "max_users")

        if limit is None:
            return True, "Sem limite de usuarios"

        # Contar usuarios existentes
        from ..database.models import User
        current_count = self.db.query(func.count(User.id)).filter(
            User.tenant_id == tenant_id
        ).scalar() or 0

        if current_count >= limit:
            return False, f"Limite de usuarios atingido ({current_count}/{limit}). Faca upgrade do plano."

        return True, f"Pode adicionar mais {limit - current_count} usuarios"

    def can_create_story(self, tenant_id: str) -> Tuple[bool, str]:
        """
        Verifica se pode criar mais stories.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (pode_criar, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, "max_stories")

        if limit is None:
            return True, "Sem limite de stories"

        # Contar stories existentes
        from ..database.models import Story
        current_count = self.db.query(func.count(Story.id)).filter(
            Story.project.has(tenant_id=tenant_id)  # Via relacionamento
        ).scalar() or 0

        if current_count >= limit:
            return False, f"Limite de stories atingido ({current_count}/{limit}). Faca upgrade do plano."

        return True, f"Pode criar mais {limit - current_count} stories"

    def can_use_tokens(
        self,
        tenant_id: str,
        tokens_needed: int = 1
    ) -> Tuple[bool, str, int]:
        """
        Verifica se pode usar tokens LLM.

        Args:
            tenant_id: ID do tenant
            tokens_needed: Quantidade de tokens necessarios

        Returns:
            Tupla (pode_usar, mensagem, tokens_restantes)
        """
        limit = self.get_effective_limit(tenant_id, "max_tokens_monthly")

        if limit is None:
            return True, "Sem limite de tokens", -1

        # Buscar uso atual do mes
        from datetime import date
        current_month = date.today().replace(day=1)

        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == UsageMetric.LLM_TOKENS.value,
                Usage.period == current_month,
                Usage.period_type == "monthly"
            )
        ).first()

        current_usage = usage.value if usage else 0
        remaining = limit - current_usage

        if remaining < tokens_needed:
            return False, f"Limite de tokens atingido ({current_usage}/{limit}). Faca upgrade do plano.", remaining

        return True, f"Tokens restantes: {remaining}", remaining

    def can_execute_job(self, tenant_id: str) -> Tuple[bool, str]:
        """
        Verifica se pode executar mais jobs hoje.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (pode_executar, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, "max_jobs_daily")

        if limit is None:
            return True, "Sem limite de jobs"

        # Buscar uso de hoje
        from datetime import date
        today = date.today()

        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == UsageMetric.JOBS_EXECUTED.value,
                Usage.period == today,
                Usage.period_type == "daily"
            )
        ).first()

        current_usage = usage.value if usage else 0

        if current_usage >= limit:
            return False, f"Limite diario de jobs atingido ({current_usage}/{limit}). Tente novamente amanha."

        return True, f"Pode executar mais {limit - current_usage} jobs hoje"

    def can_make_api_request(self, tenant_id: str) -> Tuple[bool, str]:
        """
        Verifica se pode fazer mais requisicoes API.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (pode_fazer, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, "max_api_requests_daily")

        if limit is None:
            return True, "Sem limite de API"

        # Buscar uso de hoje
        from datetime import date
        today = date.today()

        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == UsageMetric.API_REQUESTS.value,
                Usage.period == today,
                Usage.period_type == "daily"
            )
        ).first()

        current_usage = usage.value if usage else 0

        if current_usage >= limit:
            return False, f"Limite de requisicoes API atingido ({current_usage}/{limit})"

        return True, f"Requisicoes restantes: {limit - current_usage}"

    # =========================================================================
    # FEATURES
    # =========================================================================

    def has_feature(self, tenant_id: str, feature_name: str) -> bool:
        """
        Verifica se o tenant tem acesso a uma feature.

        Args:
            tenant_id: ID do tenant
            feature_name: Nome da feature

        Returns:
            True se tem acesso
        """
        tenant, plan = self._get_tenant_with_plan(tenant_id)

        if not plan:
            return False

        features = plan.features or []
        return feature_name in features

    def require_feature(self, feature_name: str):
        """
        Decorator que exige uma feature especifica.

        Uso:
            @checker.require_feature("advanced_analytics")
            async def analytics_endpoint():
                ...
        """
        def decorator(func: Callable) -> Callable:
            @wraps(func)
            async def wrapper(*args, **kwargs):
                tenant_id = get_current_tenant_id()
                if not tenant_id:
                    raise HTTPException(status_code=401, detail="Tenant nao identificado")

                if not self.has_feature(tenant_id, feature_name):
                    raise HTTPException(
                        status_code=403,
                        detail=f"Feature '{feature_name}' nao disponivel no seu plano. Faca upgrade."
                    )

                return await func(*args, **kwargs)
            return wrapper
        return decorator

    # =========================================================================
    # DECORATORS
    # =========================================================================

    def require_quota(self, limit_name: str, increment: int = 1):
        """
        Decorator que verifica quota antes de executar.

        Uso:
            @checker.require_quota("max_projects")
            async def create_project():
                ...
        """
        def decorator(func: Callable) -> Callable:
            @wraps(func)
            async def wrapper(*args, **kwargs):
                tenant_id = get_current_tenant_id()
                if not tenant_id:
                    raise HTTPException(status_code=401, detail="Tenant nao identificado")

                # Verificar quota
                can_proceed, message = self._check_generic_quota(
                    tenant_id, limit_name, increment
                )

                if not can_proceed:
                    raise HTTPException(
                        status_code=403,
                        detail=message,
                        headers={"X-Limit-Exceeded": limit_name}
                    )

                return await func(*args, **kwargs)
            return wrapper
        return decorator

    def _check_generic_quota(
        self,
        tenant_id: str,
        limit_name: str,
        increment: int = 1
    ) -> Tuple[bool, str]:
        """
        Verificacao generica de quota.

        Args:
            tenant_id: ID do tenant
            limit_name: Nome do limite
            increment: Quantidade a incrementar

        Returns:
            Tupla (pode_prosseguir, mensagem)
        """
        limit = self.get_effective_limit(tenant_id, limit_name)

        if limit is None:
            return True, f"Sem limite para {limit_name}"

        # Mapear limit_name para metrica de uso
        metric_map = {
            "max_projects": ("projects", "count"),
            "max_users": ("users", "count"),
            "max_stories": ("stories", "count"),
            "max_agents": ("agents", "count"),
            "max_tokens_monthly": (UsageMetric.LLM_TOKENS.value, "monthly"),
            "max_jobs_daily": (UsageMetric.JOBS_EXECUTED.value, "daily"),
            "max_api_requests_daily": (UsageMetric.API_REQUESTS.value, "daily"),
        }

        if limit_name not in metric_map:
            logger.warning(f"Limite nao mapeado: {limit_name}")
            return True, "Limite nao configurado"

        metric_type, period = metric_map[limit_name]

        if period == "count":
            # Contar entidades diretamente
            current = self._count_entities(tenant_id, metric_type)
        else:
            # Buscar da tabela de uso
            current = self._get_usage_value(tenant_id, metric_type, period)

        if current + increment > limit:
            return False, f"Limite de {limit_name} atingido ({current}/{limit})"

        return True, f"Quota disponivel: {limit - current}"

    def _count_entities(self, tenant_id: str, entity_type: str) -> int:
        """Conta entidades de um tipo para o tenant"""
        # Esta funcao precisaria ser implementada para cada tipo de entidade
        # Por enquanto, retorna 0
        return 0

    def _get_usage_value(
        self,
        tenant_id: str,
        metric: str,
        period_type: str
    ) -> int:
        """Obtem valor de uso de uma metrica"""
        from datetime import date

        if period_type == "daily":
            period = date.today()
        else:  # monthly
            period = date.today().replace(day=1)

        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == metric,
                Usage.period == period,
                Usage.period_type == period_type
            )
        ).first()

        return usage.value if usage else 0

    # =========================================================================
    # RESUMO DE LIMITES
    # =========================================================================

    def get_limits_summary(self, tenant_id: str) -> Dict[str, Any]:
        """
        Obtem resumo de todos os limites e uso atual.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dict com limites e uso
        """
        tenant, plan = self._get_tenant_with_plan(tenant_id)

        if not tenant:
            return {"error": "Tenant nao encontrado"}

        if not plan:
            return {
                "tenant_id": tenant_id,
                "plan": None,
                "message": "Sem plano ativo"
            }

        summary = {
            "tenant_id": tenant_id,
            "plan": {
                "id": plan.plan_id,
                "name": plan.name,
                "type": plan.plan_type
            },
            "limits": {},
            "features": plan.features or [],
            "custom_limits": tenant.custom_limits or {}
        }

        # Verificar cada limite do plano
        for limit_name, limit_value in (plan.limits or {}).items():
            # Verificar se tem override customizado
            effective_limit = tenant.custom_limits.get(limit_name, limit_value) if tenant.custom_limits else limit_value

            summary["limits"][limit_name] = {
                "plan_limit": limit_value,
                "effective_limit": effective_limit,
                "is_custom": limit_name in (tenant.custom_limits or {})
            }

        return summary


def check_limit(
    db: Session,
    tenant_id: str,
    limit_name: str,
    increment: int = 1
) -> None:
    """
    Funcao utilitaria para verificar limite.
    Levanta HTTPException se limite excedido.

    Args:
        db: Sessao do banco
        tenant_id: ID do tenant
        limit_name: Nome do limite
        increment: Quantidade a incrementar

    Raises:
        HTTPException se limite excedido
    """
    checker = PlanLimitsChecker(db)
    can_proceed, message = checker._check_generic_quota(tenant_id, limit_name, increment)

    if not can_proceed:
        raise HTTPException(
            status_code=403,
            detail=message,
            headers={"X-Limit-Exceeded": limit_name}
        )


def enforce_plan_limits(limit_name: str):
    """
    Decorator para endpoints que verifica limite antes de executar.

    Uso:
        @router.post("/projects")
        @enforce_plan_limits("max_projects")
        async def create_project(db: Session = Depends(get_db)):
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            from ..database.connection import SessionLocal

            tenant_id = get_current_tenant_id()
            if not tenant_id:
                raise HTTPException(status_code=401, detail="Tenant nao identificado")

            db = SessionLocal()
            try:
                check_limit(db, tenant_id, limit_name)
            finally:
                db.close()

            return await func(*args, **kwargs)
        return wrapper
    return decorator
