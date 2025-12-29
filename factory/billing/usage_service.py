# -*- coding: utf-8 -*-
"""
Servico de Usage Metering
==========================

Implementa:
- Registro de metricas de uso
- Agregacao por periodo
- Alertas de limite
- Relatorios de consumo

Autor: Fabrica de Agentes
"""

import uuid
import logging
from datetime import datetime, date, timedelta
from typing import Optional, Dict, Any, List, Tuple
from collections import defaultdict

from sqlalchemy.orm import Session
from sqlalchemy import and_, func, extract

from .models import (
    Tenant, Plan, Subscription, Usage,
    UsageMetric, SubscriptionStatus
)

# Configurar logging
logger = logging.getLogger(__name__)


# Mapeamento de metricas para campos de limite nos planos
METRIC_TO_LIMIT_MAP = {
    UsageMetric.LLM_TOKENS.value: "max_tokens_monthly",
    UsageMetric.LLM_REQUESTS.value: "max_llm_requests_daily",
    UsageMetric.PROJECTS_CREATED.value: "max_projects",
    UsageMetric.STORIES_CREATED.value: "max_stories",
    UsageMetric.TASKS_COMPLETED.value: "max_tasks",
    UsageMetric.JOBS_EXECUTED.value: "max_jobs_daily",
    UsageMetric.STORAGE_BYTES.value: "max_storage_bytes",
    UsageMetric.API_REQUESTS.value: "max_api_requests_daily",
    UsageMetric.AGENTS_ACTIVE.value: "max_agents",
    UsageMetric.USERS_ACTIVE.value: "max_users",
}


class UsageService:
    """
    Servico de controle de uso/consumo.

    Uso:
        service = UsageService(db_session)
        service.track_usage(tenant_id, UsageMetric.LLM_TOKENS, 1500)
        remaining = service.get_remaining_quota(tenant_id, UsageMetric.LLM_TOKENS)
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    def _get_current_period(self, period_type: str = "daily") -> date:
        """
        Retorna a data do periodo atual.

        Args:
            period_type: daily ou monthly

        Returns:
            Data do periodo
        """
        today = date.today()
        if period_type == "monthly":
            return today.replace(day=1)
        return today

    def _get_tenant_plan(self, tenant_id: str) -> Optional[Plan]:
        """
        Obtem o plano ativo do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Plan ou None
        """
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
            return None

        return self.db.query(Plan).filter(
            Plan.plan_id == subscription.plan_id
        ).first()

    def _get_tenant_custom_limits(self, tenant_id: str) -> Dict[str, int]:
        """
        Obtem limites customizados do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dict de limites
        """
        tenant = self.db.query(Tenant).filter(
            Tenant.tenant_id == tenant_id
        ).first()

        if not tenant:
            return {}

        return tenant.custom_limits or {}

    # =========================================================================
    # TRACKING
    # =========================================================================

    def track_usage(
        self,
        tenant_id: str,
        metric: str,
        value: int = 1,
        context: Optional[Dict] = None,
        period_type: str = "daily"
    ) -> Tuple[Usage, bool]:
        """
        Registra uso de uma metrica.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica (UsageMetric)
            value: Valor a adicionar
            context: Contexto adicional (project_id, job_id, etc)
            period_type: daily ou monthly

        Returns:
            Tupla (Usage atualizado, is_over_limit)
        """
        period = self._get_current_period(period_type)

        # Buscar ou criar registro de uso
        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == metric,
                Usage.period == period,
                Usage.period_type == period_type
            )
        ).first()

        # Obter limite do plano
        limit_value = self._get_metric_limit(tenant_id, metric)

        if usage:
            # Atualizar valor existente
            usage.value += value
            if context:
                if not usage.context:
                    usage.context = {}
                usage.context.update(context)
        else:
            # Criar novo registro
            usage = Usage(
                usage_id=self._generate_id("USG"),
                tenant_id=tenant_id,
                period=period,
                period_type=period_type,
                metric=metric,
                value=value,
                limit_value=limit_value,
                context=context or {}
            )
            self.db.add(usage)

        # Atualizar limite (pode ter mudado)
        usage.limit_value = limit_value

        try:
            self.db.commit()
        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar uso: {e}")
            raise

        is_over_limit = usage.is_over_limit()

        if is_over_limit:
            logger.warning(
                f"Tenant {tenant_id} excedeu limite de {metric}: "
                f"{usage.value}/{limit_value}"
            )

        return usage, is_over_limit

    def track_llm_usage(
        self,
        tenant_id: str,
        input_tokens: int,
        output_tokens: int,
        model: str,
        job_id: Optional[str] = None,
        project_id: Optional[str] = None
    ) -> Tuple[Usage, bool]:
        """
        Registra uso de tokens LLM.

        Args:
            tenant_id: ID do tenant
            input_tokens: Tokens de entrada
            output_tokens: Tokens de saida
            model: Modelo usado
            job_id: ID do job (opcional)
            project_id: ID do projeto (opcional)

        Returns:
            Tupla (Usage, is_over_limit)
        """
        total_tokens = input_tokens + output_tokens

        context = {
            "model": model,
            "input_tokens": input_tokens,
            "output_tokens": output_tokens,
            "timestamp": datetime.utcnow().isoformat()
        }
        if job_id:
            context["job_id"] = job_id
        if project_id:
            context["project_id"] = project_id

        # Registrar tokens
        usage, over_limit = self.track_usage(
            tenant_id=tenant_id,
            metric=UsageMetric.LLM_TOKENS.value,
            value=total_tokens,
            context=context,
            period_type="monthly"  # Tokens sao mensais
        )

        # Registrar tambem o request
        self.track_usage(
            tenant_id=tenant_id,
            metric=UsageMetric.LLM_REQUESTS.value,
            value=1,
            context={"model": model},
            period_type="daily"
        )

        return usage, over_limit

    # =========================================================================
    # QUERIES
    # =========================================================================

    def get_current_usage(
        self,
        tenant_id: str,
        metric: str,
        period_type: str = "daily"
    ) -> int:
        """
        Obtem uso atual de uma metrica.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica
            period_type: daily ou monthly

        Returns:
            Valor atual de uso
        """
        period = self._get_current_period(period_type)

        usage = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == metric,
                Usage.period == period,
                Usage.period_type == period_type
            )
        ).first()

        return usage.value if usage else 0

    def get_remaining_quota(
        self,
        tenant_id: str,
        metric: str,
        period_type: str = "daily"
    ) -> int:
        """
        Obtem quota restante de uma metrica.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica
            period_type: daily ou monthly

        Returns:
            Quota restante (pode ser negativo se excedeu)
        """
        current_usage = self.get_current_usage(tenant_id, metric, period_type)
        limit = self._get_metric_limit(tenant_id, metric)

        if limit is None or limit == 0:
            return float('inf')  # Sem limite

        return limit - current_usage

    def check_quota(
        self,
        tenant_id: str,
        metric: str,
        required: int = 1,
        period_type: str = "daily"
    ) -> Tuple[bool, int]:
        """
        Verifica se ha quota disponivel.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica
            required: Quantidade necessaria
            period_type: daily ou monthly

        Returns:
            Tupla (has_quota, remaining)
        """
        remaining = self.get_remaining_quota(tenant_id, metric, period_type)

        if remaining == float('inf'):
            return True, -1  # Sem limite

        has_quota = remaining >= required
        return has_quota, int(remaining)

    def _get_metric_limit(self, tenant_id: str, metric: str) -> Optional[int]:
        """
        Obtem o limite para uma metrica.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica

        Returns:
            Limite ou None se ilimitado
        """
        # Primeiro, verificar limites customizados
        custom_limits = self._get_tenant_custom_limits(tenant_id)
        limit_key = METRIC_TO_LIMIT_MAP.get(metric)

        if limit_key and limit_key in custom_limits:
            return custom_limits[limit_key]

        # Depois, verificar limite do plano
        plan = self._get_tenant_plan(tenant_id)
        if plan and plan.limits:
            if limit_key and limit_key in plan.limits:
                return plan.limits[limit_key]

        return None  # Sem limite

    # =========================================================================
    # RELATORIOS
    # =========================================================================

    def get_usage_summary(
        self,
        tenant_id: str,
        period_type: str = "monthly"
    ) -> Dict[str, Any]:
        """
        Obtem resumo de uso do tenant.

        Args:
            tenant_id: ID do tenant
            period_type: daily ou monthly

        Returns:
            Dict com resumo de todas as metricas
        """
        period = self._get_current_period(period_type)

        usages = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.period == period,
                Usage.period_type == period_type
            )
        ).all()

        plan = self._get_tenant_plan(tenant_id)
        plan_limits = plan.limits if plan else {}
        custom_limits = self._get_tenant_custom_limits(tenant_id)

        summary = {
            "tenant_id": tenant_id,
            "period": period.isoformat(),
            "period_type": period_type,
            "metrics": {}
        }

        for metric_enum in UsageMetric:
            metric = metric_enum.value
            limit_key = METRIC_TO_LIMIT_MAP.get(metric)

            # Buscar uso atual
            usage = next((u for u in usages if u.metric == metric), None)
            current_value = usage.value if usage else 0

            # Obter limite
            limit = None
            if limit_key:
                limit = custom_limits.get(limit_key) or plan_limits.get(limit_key)

            summary["metrics"][metric] = {
                "current": current_value,
                "limit": limit,
                "remaining": (limit - current_value) if limit else None,
                "percentage": round((current_value / limit) * 100, 2) if limit else None,
                "is_over_limit": current_value >= limit if limit else False
            }

        return summary

    def get_usage_history(
        self,
        tenant_id: str,
        metric: str,
        days: int = 30
    ) -> List[Dict[str, Any]]:
        """
        Obtem historico de uso de uma metrica.

        Args:
            tenant_id: ID do tenant
            metric: Tipo de metrica
            days: Numero de dias

        Returns:
            Lista de registros de uso
        """
        start_date = date.today() - timedelta(days=days)

        usages = self.db.query(Usage).filter(
            and_(
                Usage.tenant_id == tenant_id,
                Usage.metric == metric,
                Usage.period >= start_date
            )
        ).order_by(Usage.period.asc()).all()

        return [
            {
                "date": u.period.isoformat(),
                "value": u.value,
                "limit": u.limit_value,
                "percentage": u.usage_percentage()
            }
            for u in usages
        ]

    def get_top_consumers(
        self,
        metric: str,
        period_type: str = "monthly",
        limit: int = 10
    ) -> List[Dict[str, Any]]:
        """
        Obtem os tenants que mais consomem uma metrica.

        Args:
            metric: Tipo de metrica
            period_type: daily ou monthly
            limit: Numero de resultados

        Returns:
            Lista de tenants com uso
        """
        period = self._get_current_period(period_type)

        results = self.db.query(
            Usage.tenant_id,
            func.sum(Usage.value).label("total")
        ).filter(
            and_(
                Usage.metric == metric,
                Usage.period == period,
                Usage.period_type == period_type
            )
        ).group_by(
            Usage.tenant_id
        ).order_by(
            func.sum(Usage.value).desc()
        ).limit(limit).all()

        return [
            {"tenant_id": r.tenant_id, "total": r.total}
            for r in results
        ]

    # =========================================================================
    # ALERTAS
    # =========================================================================

    def check_usage_alerts(self, tenant_id: str) -> List[Dict[str, Any]]:
        """
        Verifica alertas de uso para um tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Lista de alertas
        """
        alerts = []
        summary = self.get_usage_summary(tenant_id, "monthly")

        for metric, data in summary["metrics"].items():
            percentage = data.get("percentage")

            if percentage is None:
                continue

            if percentage >= 100:
                alerts.append({
                    "level": "critical",
                    "metric": metric,
                    "message": f"Limite de {metric} excedido ({percentage:.1f}%)",
                    "current": data["current"],
                    "limit": data["limit"],
                    "percentage": percentage
                })
            elif percentage >= 90:
                alerts.append({
                    "level": "warning",
                    "metric": metric,
                    "message": f"Uso de {metric} proximo do limite ({percentage:.1f}%)",
                    "current": data["current"],
                    "limit": data["limit"],
                    "percentage": percentage
                })
            elif percentage >= 75:
                alerts.append({
                    "level": "info",
                    "metric": metric,
                    "message": f"Uso de {metric} a {percentage:.1f}% do limite",
                    "current": data["current"],
                    "limit": data["limit"],
                    "percentage": percentage
                })

        return alerts

    def get_tenants_near_limit(
        self,
        metric: str,
        threshold_percent: float = 80.0
    ) -> List[Dict[str, Any]]:
        """
        Lista tenants proximos do limite de uma metrica.

        Args:
            metric: Tipo de metrica
            threshold_percent: Percentual minimo de uso

        Returns:
            Lista de tenants
        """
        period = self._get_current_period("monthly")

        usages = self.db.query(Usage).filter(
            and_(
                Usage.metric == metric,
                Usage.period == period,
                Usage.limit_value.isnot(None),
                Usage.limit_value > 0
            )
        ).all()

        near_limit = []
        for usage in usages:
            percentage = usage.usage_percentage()
            if percentage >= threshold_percent:
                near_limit.append({
                    "tenant_id": usage.tenant_id,
                    "current": usage.value,
                    "limit": usage.limit_value,
                    "percentage": percentage
                })

        return sorted(near_limit, key=lambda x: x["percentage"], reverse=True)

    # =========================================================================
    # RESET E MANUTENCAO
    # =========================================================================

    def reset_daily_usage(self, tenant_id: Optional[str] = None) -> int:
        """
        Reseta contadores diarios (usado em jobs de manutencao).

        Args:
            tenant_id: ID do tenant especifico (todos se None)

        Returns:
            Numero de registros resetados
        """
        yesterday = date.today() - timedelta(days=1)

        query = self.db.query(Usage).filter(
            and_(
                Usage.period < yesterday,
                Usage.period_type == "daily"
            )
        )

        if tenant_id:
            query = query.filter(Usage.tenant_id == tenant_id)

        # Arquivar em vez de deletar (para historico)
        # Por enquanto, apenas deletamos registros muito antigos
        old_date = date.today() - timedelta(days=90)
        count = query.filter(Usage.period < old_date).delete()

        self.db.commit()
        logger.info(f"Removidos {count} registros de uso antigos")

        return count

    def archive_monthly_usage(self) -> int:
        """
        Arquiva uso mensal para relatorios historicos.

        Returns:
            Numero de registros arquivados
        """
        # Meses anteriores ao atual
        current_month = date.today().replace(day=1)

        old_usages = self.db.query(Usage).filter(
            and_(
                Usage.period < current_month,
                Usage.period_type == "monthly"
            )
        ).all()

        # Aqui poderiamos mover para uma tabela de historico
        # Por enquanto, apenas mantemos os dados

        return len(old_usages)
