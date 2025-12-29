# -*- coding: utf-8 -*-
"""
Sistema de Limites por Plano e Enforcement (Issue #104)
========================================================

Implementa:
- TenantPlan: Modelo de plano associado ao tenant
- PlanEnforcer: Verificacao e enforcement de limites
- Decorators para proteger endpoints
- Contadores de uso automaticos

Autor: Fabrica de Agentes
"""

import logging
from datetime import datetime, date
from typing import Optional, Dict, Any, Tuple, Callable, List
from functools import wraps
from enum import Enum

from fastapi import HTTPException, Request, Depends
from sqlalchemy.orm import Session
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float, and_, func

# Configurar logging
logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS
# =============================================================================

class PlanType(str, Enum):
    """Tipos de plano disponíveis"""
    FREE = "free"
    PRO = "pro"
    ENTERPRISE = "enterprise"


class ResourceType(str, Enum):
    """Tipos de recursos controlados por limites"""
    USERS = "users"
    PROJECTS = "projects"
    STORIES = "stories"
    STORIES_PER_MONTH = "stories_per_month"
    TOKENS_PER_MONTH = "tokens_per_month"
    STORAGE_GB = "storage_gb"
    API_CALLS_PER_DAY = "api_calls_per_day"
    WORKERS = "workers"
    AGENTS = "agents"


# =============================================================================
# LIMITES PADRÃO POR PLANO
# =============================================================================

DEFAULT_PLAN_LIMITS = {
    PlanType.FREE.value: {
        ResourceType.USERS.value: 3,
        ResourceType.PROJECTS.value: 2,
        ResourceType.STORIES.value: 50,
        ResourceType.STORIES_PER_MONTH.value: 10,
        ResourceType.TOKENS_PER_MONTH.value: 10000,
        ResourceType.STORAGE_GB.value: 1,
        ResourceType.API_CALLS_PER_DAY.value: 100,
        ResourceType.WORKERS.value: 1,
        ResourceType.AGENTS.value: 1,
    },
    PlanType.PRO.value: {
        ResourceType.USERS.value: 20,
        ResourceType.PROJECTS.value: 20,
        ResourceType.STORIES.value: 500,
        ResourceType.STORIES_PER_MONTH.value: 100,
        ResourceType.TOKENS_PER_MONTH.value: 500000,
        ResourceType.STORAGE_GB.value: 50,
        ResourceType.API_CALLS_PER_DAY.value: 5000,
        ResourceType.WORKERS.value: 5,
        ResourceType.AGENTS.value: 10,
    },
    PlanType.ENTERPRISE.value: {
        ResourceType.USERS.value: -1,  # -1 = ilimitado
        ResourceType.PROJECTS.value: -1,
        ResourceType.STORIES.value: -1,
        ResourceType.STORIES_PER_MONTH.value: -1,
        ResourceType.TOKENS_PER_MONTH.value: -1,
        ResourceType.STORAGE_GB.value: -1,
        ResourceType.API_CALLS_PER_DAY.value: -1,
        ResourceType.WORKERS.value: -1,
        ResourceType.AGENTS.value: -1,
    },
}


# =============================================================================
# EXCEÇÕES
# =============================================================================

class LimitExceededError(Exception):
    """Exceção quando um limite do plano é excedido"""

    def __init__(
        self,
        resource_type: str,
        current: int,
        limit: int,
        tenant_id: str = None,
        message: str = None
    ):
        self.resource_type = resource_type
        self.current = current
        self.limit = limit
        self.tenant_id = tenant_id
        self.message = message or f"Limite de {resource_type} excedido: {current}/{limit}"
        super().__init__(self.message)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "error": "limit_exceeded",
            "resource_type": self.resource_type,
            "current": self.current,
            "limit": self.limit,
            "tenant_id": self.tenant_id,
            "message": self.message
        }


class FeatureNotAvailableError(Exception):
    """Exceção quando uma feature não está disponível no plano"""

    def __init__(self, feature: str, plan_type: str, message: str = None):
        self.feature = feature
        self.plan_type = plan_type
        self.message = message or f"Feature '{feature}' não disponível no plano {plan_type}"
        super().__init__(self.message)


# =============================================================================
# PLAN ENFORCER - Verificação e Enforcement de Limites
# =============================================================================

class PlanEnforcer:
    """
    Sistema de Enforcement de Limites por Plano (Issue #104)

    Responsabilidades:
    - Verificar se tenant pode criar mais recursos
    - Bloquear operações se limite excedido
    - Contar recursos atuais
    - Registrar uso

    Uso:
        enforcer = PlanEnforcer(db_session)

        # Verificar antes de criar
        can_create, msg = enforcer.check_limit(tenant_id, "projects")
        if not can_create:
            raise HTTPException(403, msg)

        # Ou usar enforce que levanta exceção
        enforcer.enforce(tenant_id, "projects")
        # Se chegou aqui, pode criar

        # Incrementar uso após criar
        enforcer.increment_usage(tenant_id, "projects")
    """

    def __init__(self, db: Session):
        """
        Inicializa o enforcer.

        Args:
            db: Sessão do banco de dados SQLAlchemy
        """
        self.db = db

    def get_tenant_plan(self, tenant_id: str) -> Tuple[str, Dict[str, int]]:
        """
        Obtém o plano e limites do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (plan_type, limits_dict)
        """
        try:
            # Usar modelos do billing que nao conflitam
            from ..database.tenant_models import Tenant

            # Buscar tenant
            tenant = self.db.query(Tenant).filter(
                Tenant.tenant_id == tenant_id
            ).first()

            if not tenant:
                # Tenant não encontrado, usar free
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Buscar assinatura ativa via relacionamento
            active_sub = None
            for sub in (tenant.subscriptions or []):
                if sub.status in ["active", "trialing"]:
                    active_sub = sub
                    break

            if not active_sub:
                # Sem assinatura, usar free
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Buscar plano via relacionamento
            plan = active_sub.plan if hasattr(active_sub, 'plan') else None

            if not plan:
                return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

            # Mesclar limites do plano com limites customizados do tenant
            plan_limits = plan.limits or {}
            custom_limits = tenant.custom_limits or {}

            effective_limits = {**plan_limits, **custom_limits}

            return plan.plan_type, effective_limits

        except Exception as e:
            logger.error(f"Erro ao buscar plano do tenant {tenant_id}: {e}")
            return PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]

    def get_limit(self, tenant_id: str, resource_type: str) -> int:
        """
        Obtém o limite para um tipo de recurso.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo de recurso (users, projects, etc)

        Returns:
            Limite (-1 = ilimitado, 0 = bloqueado)
        """
        plan_type, limits = self.get_tenant_plan(tenant_id)
        return limits.get(resource_type, 0)

    def get_current_count(self, tenant_id: str, resource_type: str) -> int:
        """
        Conta recursos atuais do tenant.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo de recurso

        Returns:
            Quantidade atual
        """
        try:
            # Importar modelos necessários
            from ..database.models import Project, Story, Task, User
            from ..database.tenant_models import TenantMember

            count = 0

            if resource_type == ResourceType.USERS.value:
                count = self.db.query(func.count(TenantMember.id)).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.active == True
                ).scalar() or 0

            elif resource_type == ResourceType.PROJECTS.value:
                count = self.db.query(func.count(Project.id)).filter(
                    Project.tenant_id == tenant_id
                ).scalar() or 0

            elif resource_type == ResourceType.STORIES.value:
                count = self.db.query(func.count(Story.id)).filter(
                    Story.tenant_id == tenant_id
                ).scalar() or 0

            elif resource_type == ResourceType.STORIES_PER_MONTH.value:
                # Stories criadas este mês
                first_day = date.today().replace(day=1)
                count = self.db.query(func.count(Story.id)).filter(
                    Story.tenant_id == tenant_id,
                    Story.created_at >= first_day
                ).scalar() or 0

            elif resource_type == ResourceType.TOKENS_PER_MONTH.value:
                # Buscar da tabela de uso
                from .models import Usage, UsageMetric
                first_day = date.today().replace(day=1)

                usage = self.db.query(Usage).filter(
                    and_(
                        Usage.tenant_id == tenant_id,
                        Usage.metric == UsageMetric.LLM_TOKENS.value,
                        Usage.period >= first_day,
                        Usage.period_type == "monthly"
                    )
                ).first()
                count = usage.value if usage else 0

            elif resource_type == ResourceType.API_CALLS_PER_DAY.value:
                from .models import Usage, UsageMetric
                today = date.today()

                usage = self.db.query(Usage).filter(
                    and_(
                        Usage.tenant_id == tenant_id,
                        Usage.metric == UsageMetric.API_REQUESTS.value,
                        Usage.period == today,
                        Usage.period_type == "daily"
                    )
                ).first()
                count = usage.value if usage else 0

            return count

        except Exception as e:
            logger.error(f"Erro ao contar recursos {resource_type} para tenant {tenant_id}: {e}")
            return 0

    def check_limit(
        self,
        tenant_id: str,
        resource_type: str,
        increment: int = 1
    ) -> Tuple[bool, str]:
        """
        Verifica se tenant pode criar mais recursos.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo de recurso
            increment: Quantidade a adicionar (padrão 1)

        Returns:
            Tupla (pode_criar, mensagem)
        """
        limit = self.get_limit(tenant_id, resource_type)

        # -1 = ilimitado
        if limit == -1:
            return True, f"Sem limite para {resource_type}"

        # 0 = bloqueado
        if limit == 0:
            return False, f"Recurso {resource_type} não disponível no seu plano"

        current = self.get_current_count(tenant_id, resource_type)

        if current + increment > limit:
            return False, f"Limite de {resource_type} atingido ({current}/{limit}). Faça upgrade do plano."

        remaining = limit - current - increment + 1
        return True, f"Pode criar mais {remaining} {resource_type}"

    def enforce(
        self,
        tenant_id: str,
        resource_type: str,
        increment: int = 1
    ) -> None:
        """
        Verifica e levanta exceção se limite excedido.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo de recurso
            increment: Quantidade a adicionar

        Raises:
            LimitExceededError se limite excedido
        """
        can_proceed, message = self.check_limit(tenant_id, resource_type, increment)

        if not can_proceed:
            limit = self.get_limit(tenant_id, resource_type)
            current = self.get_current_count(tenant_id, resource_type)

            raise LimitExceededError(
                resource_type=resource_type,
                current=current,
                limit=limit,
                tenant_id=tenant_id,
                message=message
            )

    def increment_usage(
        self,
        tenant_id: str,
        resource_type: str,
        amount: int = 1
    ) -> None:
        """
        Incrementa contador de uso após criar recurso.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo de recurso
            amount: Quantidade a incrementar
        """
        try:
            from ..database.tenant_models import Usage
            import uuid

            # Determinar período e tipo
            if resource_type in [ResourceType.TOKENS_PER_MONTH.value, ResourceType.STORIES_PER_MONTH.value]:
                period = date.today().replace(day=1)
                period_type = "monthly"
            else:
                period = date.today()
                period_type = "daily"

            # Mapear resource_type para metric
            metric_map = {
                ResourceType.API_CALLS_PER_DAY.value: "api_requests",
                ResourceType.TOKENS_PER_MONTH.value: "llm_tokens",
                ResourceType.STORIES_PER_MONTH.value: "stories_created",
            }
            metric = metric_map.get(resource_type, resource_type)

            # Buscar ou criar registro de uso
            usage = self.db.query(Usage).filter(
                and_(
                    Usage.tenant_id == tenant_id,
                    Usage.metric == metric,
                    Usage.period == period,
                    Usage.period_type == period_type
                )
            ).first()

            if usage:
                usage.value += amount
                usage.updated_at = datetime.utcnow()
            else:
                usage = Usage(
                    usage_id=f"USG-{uuid.uuid4().hex[:8].upper()}",
                    tenant_id=tenant_id,
                    period=period,
                    period_type=period_type,
                    metric=metric,
                    value=amount,
                    limit_value=self.get_limit(tenant_id, resource_type)
                )
                self.db.add(usage)

            self.db.commit()

        except Exception as e:
            logger.error(f"Erro ao incrementar uso {resource_type} para tenant {tenant_id}: {e}")
            self.db.rollback()

    def get_usage_summary(self, tenant_id: str) -> Dict[str, Any]:
        """
        Retorna resumo de uso e limites do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dict com uso e limites
        """
        plan_type, limits = self.get_tenant_plan(tenant_id)

        summary = {
            "tenant_id": tenant_id,
            "plan_type": plan_type,
            "resources": {}
        }

        for resource_type in ResourceType:
            limit = limits.get(resource_type.value, 0)
            current = self.get_current_count(tenant_id, resource_type.value)

            summary["resources"][resource_type.value] = {
                "current": current,
                "limit": limit if limit != -1 else "unlimited",
                "remaining": limit - current if limit != -1 else "unlimited",
                "usage_percent": round((current / limit) * 100, 1) if limit > 0 else 0
            }

        return summary


# =============================================================================
# DECORATORS
# =============================================================================

def enforce_limit(resource_type: str):
    """
    Decorator que verifica limite antes de executar endpoint.

    Uso:
        @router.post("/projects")
        @enforce_limit("projects")
        async def create_project(db: Session = Depends(get_db)):
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            from ..core.multi_tenant import get_current_tenant_id
            from ..database.connection import SessionLocal

            tenant_id = get_current_tenant_id()
            if not tenant_id:
                raise HTTPException(
                    status_code=401,
                    detail="Tenant não identificado"
                )

            db = SessionLocal()
            try:
                enforcer = PlanEnforcer(db)
                enforcer.enforce(tenant_id, resource_type)
            except LimitExceededError as e:
                raise HTTPException(
                    status_code=403,
                    detail=e.message,
                    headers={"X-Limit-Exceeded": resource_type}
                )
            finally:
                db.close()

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_plan(allowed_plans: List[str]):
    """
    Decorator que requer plano específico.

    Uso:
        @router.get("/advanced-analytics")
        @require_plan(["pro", "enterprise"])
        async def analytics():
            ...
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            from ..core.multi_tenant import get_current_tenant_id
            from ..database.connection import SessionLocal

            tenant_id = get_current_tenant_id()
            if not tenant_id:
                raise HTTPException(
                    status_code=401,
                    detail="Tenant não identificado"
                )

            db = SessionLocal()
            try:
                enforcer = PlanEnforcer(db)
                plan_type, _ = enforcer.get_tenant_plan(tenant_id)

                if plan_type not in allowed_plans:
                    raise HTTPException(
                        status_code=403,
                        detail=f"Este recurso requer plano {', '.join(allowed_plans)}. Seu plano atual: {plan_type}",
                        headers={"X-Required-Plan": ",".join(allowed_plans)}
                    )
            finally:
                db.close()

            return await func(*args, **kwargs)
        return wrapper
    return decorator


# =============================================================================
# FASTAPI DEPENDENCY
# =============================================================================

def get_plan_enforcer(request: Request):
    """
    Dependency para obter PlanEnforcer.

    Uso:
        @router.post("/projects")
        async def create(
            enforcer: PlanEnforcer = Depends(get_plan_enforcer)
        ):
            enforcer.enforce(tenant_id, "projects")
    """
    from ..database.connection import SessionLocal
    db = SessionLocal()
    try:
        yield PlanEnforcer(db)
    finally:
        db.close()


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Enums
    "PlanType",
    "ResourceType",
    # Classes
    "PlanEnforcer",
    # Exceptions
    "LimitExceededError",
    "FeatureNotAvailableError",
    # Decorators
    "enforce_limit",
    "require_plan",
    # Dependencies
    "get_plan_enforcer",
    # Constants
    "DEFAULT_PLAN_LIMITS",
]
