# -*- coding: utf-8 -*-
"""
Servico de Usage Metering - Enhanced Version
==============================================

Implementa:
- Registro de eventos de uso granulares (Issue #118)
- Agregacao por periodo (diario/mensal)
- Tracking de API calls, LLM tokens, storage, compute
- Tracking de logins e sessoes ativas
- Alertas de limite
- Relatorios de consumo
- Calculo de custos por uso

Autor: Fabrica de Agentes
"""

import uuid
import logging
from datetime import datetime, date, timedelta
from typing import Optional, Dict, Any, List, Tuple, Set
from collections import defaultdict

from sqlalchemy.orm import Session
from sqlalchemy import and_, func, extract, or_

from .models import (
    Tenant, Plan, Subscription, Usage,
    UsageMetric, SubscriptionStatus,
    UsageEvent, UsageAggregate, UsageEventType, PricingTier
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

    # =========================================================================
    # EVENTOS DE USO DETALHADOS (Issue #118)
    # =========================================================================

    def track_event(
        self,
        tenant_id: str,
        event_type: str,
        quantity: int = 1,
        unit: str = "calls",
        resource: Optional[str] = None,
        action: Optional[str] = None,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        metadata: Optional[Dict] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra um evento de uso granular.

        Args:
            tenant_id: ID do tenant
            event_type: Tipo do evento (api_call, llm_tokens, storage, compute, login)
            quantity: Quantidade do recurso
            unit: Unidade (calls, tokens, bytes, seconds)
            resource: Recurso acessado (ex: /api/stories, claude-sonnet)
            action: Acao realizada (create, read, update, delete)
            user_id: ID do usuario que realizou a acao
            project_id: ID do projeto relacionado
            metadata: Dados adicionais do evento
            ip_address: IP do cliente
            user_agent: User-Agent do cliente
            correlation_id: ID de correlacao da request

        Returns:
            UsageEvent criado
        """
        # Calcular custo do evento
        cost_cents = self._calculate_event_cost(
            tenant_id, event_type, quantity, unit, metadata
        )

        event = UsageEvent(
            event_id=self._generate_id("EVT"),
            tenant_id=tenant_id,
            user_id=user_id,
            project_id=project_id,
            event_type=event_type,
            resource=resource,
            action=action,
            quantity=quantity,
            unit=unit,
            cost_cents=cost_cents,
            event_data=metadata or {},
            ip_address=ip_address,
            user_agent=user_agent[:500] if user_agent else None,
            correlation_id=correlation_id,
            timestamp=datetime.utcnow()
        )

        try:
            self.db.add(event)
            self.db.flush()

            # Atualizar agregacao diaria
            self._update_daily_aggregate(event)

            # Tambem registrar no sistema legado de Usage para compatibilidade
            self._track_legacy_usage(event)

            self.db.commit()
            logger.debug(f"Evento registrado: {event.event_id} - {event_type}")
            return event

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar evento: {e}")
            raise

    def track_api_call(
        self,
        tenant_id: str,
        method: str,
        path: str,
        status_code: int,
        duration_ms: float,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra uma chamada de API.

        Args:
            tenant_id: ID do tenant
            method: Metodo HTTP (GET, POST, etc)
            path: Path da requisicao
            status_code: Codigo de status da resposta
            duration_ms: Duracao em milissegundos
            user_id: ID do usuario
            project_id: ID do projeto
            ip_address: IP do cliente
            user_agent: User-Agent do cliente
            correlation_id: ID de correlacao

        Returns:
            UsageEvent criado
        """
        metadata = {
            "method": method,
            "path": path,
            "status_code": status_code,
            "duration_ms": round(duration_ms, 2),
        }

        return self.track_event(
            tenant_id=tenant_id,
            event_type=UsageEventType.API_CALL.value,
            quantity=1,
            unit="calls",
            resource=path,
            action=method.lower(),
            user_id=user_id,
            project_id=project_id,
            metadata=metadata,
            ip_address=ip_address,
            user_agent=user_agent,
            correlation_id=correlation_id
        )

    def track_llm_tokens(
        self,
        tenant_id: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        job_id: Optional[str] = None,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> Tuple[UsageEvent, bool]:
        """
        Registra uso de tokens LLM com calculo de custo.

        Args:
            tenant_id: ID do tenant
            model: Nome do modelo (ex: claude-sonnet-4-20250514)
            input_tokens: Tokens de entrada
            output_tokens: Tokens de saida
            job_id: ID do job (opcional)
            project_id: ID do projeto
            user_id: ID do usuario
            correlation_id: ID de correlacao

        Returns:
            Tupla (UsageEvent, is_over_limit)
        """
        total_tokens = input_tokens + output_tokens

        metadata = {
            "model": model,
            "input_tokens": input_tokens,
            "output_tokens": output_tokens,
            "job_id": job_id,
        }

        event = self.track_event(
            tenant_id=tenant_id,
            event_type=UsageEventType.LLM_TOKENS.value,
            quantity=total_tokens,
            unit="tokens",
            resource=model,
            action="generate",
            user_id=user_id,
            project_id=project_id,
            metadata=metadata,
            correlation_id=correlation_id
        )

        # Verificar limite
        is_over_limit = self._check_llm_limit(tenant_id)

        if is_over_limit:
            logger.warning(f"Tenant {tenant_id} excedeu limite de tokens LLM")

        return event, is_over_limit

    def track_storage(
        self,
        tenant_id: str,
        operation: str,
        file_name: str,
        file_size: int,
        file_type: Optional[str] = None,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra operacao de storage (upload/download).

        Args:
            tenant_id: ID do tenant
            operation: Tipo de operacao (upload, download, delete)
            file_name: Nome do arquivo
            file_size: Tamanho em bytes
            file_type: Tipo MIME do arquivo
            user_id: ID do usuario
            project_id: ID do projeto
            correlation_id: ID de correlacao

        Returns:
            UsageEvent criado
        """
        event_type = (
            UsageEventType.FILE_UPLOAD.value if operation == "upload"
            else UsageEventType.FILE_DOWNLOAD.value if operation == "download"
            else UsageEventType.STORAGE.value
        )

        metadata = {
            "file_name": file_name,
            "file_size": file_size,
            "file_type": file_type,
            "operation": operation,
        }

        return self.track_event(
            tenant_id=tenant_id,
            event_type=event_type,
            quantity=file_size,
            unit="bytes",
            resource="storage",
            action=operation,
            user_id=user_id,
            project_id=project_id,
            metadata=metadata,
            correlation_id=correlation_id
        )

    def track_compute(
        self,
        tenant_id: str,
        worker_id: str,
        job_id: str,
        duration_seconds: int,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra tempo de compute (worker).

        Args:
            tenant_id: ID do tenant
            worker_id: ID do worker
            job_id: ID do job
            duration_seconds: Duracao em segundos
            project_id: ID do projeto
            user_id: ID do usuario
            correlation_id: ID de correlacao

        Returns:
            UsageEvent criado
        """
        metadata = {
            "worker_id": worker_id,
            "job_id": job_id,
            "duration_seconds": duration_seconds,
        }

        return self.track_event(
            tenant_id=tenant_id,
            event_type=UsageEventType.COMPUTE.value,
            quantity=duration_seconds,
            unit="seconds",
            resource=f"worker/{worker_id}",
            action="execute",
            user_id=user_id,
            project_id=project_id,
            metadata=metadata,
            correlation_id=correlation_id
        )

    def track_login(
        self,
        tenant_id: str,
        user_id: str,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        success: bool = True
    ) -> UsageEvent:
        """
        Registra evento de login.

        Args:
            tenant_id: ID do tenant
            user_id: ID do usuario
            ip_address: IP do cliente
            user_agent: User-Agent do cliente
            success: Se login foi bem sucedido

        Returns:
            UsageEvent criado
        """
        metadata = {
            "success": success,
            "timestamp": datetime.utcnow().isoformat()
        }

        return self.track_event(
            tenant_id=tenant_id,
            event_type=UsageEventType.LOGIN.value,
            quantity=1,
            unit="logins",
            resource="auth",
            action="login_success" if success else "login_failed",
            user_id=user_id,
            ip_address=ip_address,
            user_agent=user_agent,
            metadata=metadata
        )

    def track_session_active(
        self,
        tenant_id: str,
        user_id: str,
        session_id: str
    ) -> UsageEvent:
        """
        Registra sessao ativa.

        Args:
            tenant_id: ID do tenant
            user_id: ID do usuario
            session_id: ID da sessao

        Returns:
            UsageEvent criado
        """
        metadata = {
            "session_id": session_id,
            "timestamp": datetime.utcnow().isoformat()
        }

        return self.track_event(
            tenant_id=tenant_id,
            event_type=UsageEventType.SESSION.value,
            quantity=1,
            unit="sessions",
            resource="session",
            action="active",
            user_id=user_id,
            metadata=metadata
        )

    # =========================================================================
    # AGREGACAO DE USO
    # =========================================================================

    def _update_daily_aggregate(self, event: UsageEvent) -> UsageAggregate:
        """
        Atualiza agregacao diaria baseado em evento.

        Args:
            event: Evento de uso

        Returns:
            UsageAggregate atualizado
        """
        today = date.today().isoformat()

        # Buscar ou criar agregacao
        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == event.tenant_id,
                UsageAggregate.period == today,
                UsageAggregate.period_type == "daily"
            )
        ).first()

        if not aggregate:
            aggregate = UsageAggregate(
                aggregate_id=self._generate_id("AGG"),
                tenant_id=event.tenant_id,
                period=today,
                period_type="daily",
                # Initialize all counters to avoid None += int errors
                api_calls=0,
                llm_tokens_input=0,
                llm_tokens_output=0,
                storage_bytes=0,
                compute_seconds=0,
                active_users=0,
                active_sessions=0,
                logins_count=0,
                file_uploads=0,
                file_downloads=0,
                cost_api_cents=0,
                cost_llm_cents=0,
                cost_storage_cents=0,
                cost_compute_cents=0,
                cost_total_cents=0
            )
            self.db.add(aggregate)

        # Atualizar metricas baseado no tipo de evento
        if event.event_type == UsageEventType.API_CALL.value:
            aggregate.api_calls += 1
            aggregate.cost_api_cents += event.cost_cents

        elif event.event_type == UsageEventType.LLM_TOKENS.value:
            event_data = event.event_data or {}
            aggregate.llm_tokens_input += event_data.get("input_tokens", 0)
            aggregate.llm_tokens_output += event_data.get("output_tokens", 0)
            aggregate.cost_llm_cents += event.cost_cents

        elif event.event_type in [UsageEventType.STORAGE.value, UsageEventType.FILE_UPLOAD.value]:
            aggregate.storage_bytes += event.quantity
            aggregate.file_uploads += 1
            aggregate.cost_storage_cents += event.cost_cents

        elif event.event_type == UsageEventType.FILE_DOWNLOAD.value:
            aggregate.file_downloads += 1

        elif event.event_type == UsageEventType.COMPUTE.value:
            aggregate.compute_seconds += event.quantity
            aggregate.cost_compute_cents += event.cost_cents

        elif event.event_type == UsageEventType.LOGIN.value:
            aggregate.logins_count += 1

        elif event.event_type == UsageEventType.SESSION.value:
            aggregate.active_sessions += 1

        # Recalcular custo total
        aggregate.calculate_total_cost()

        return aggregate

    def aggregate_monthly_usage(self, tenant_id: str, month: str) -> UsageAggregate:
        """
        Agrega uso mensal a partir de agregacoes diarias.

        Args:
            tenant_id: ID do tenant
            month: Mes no formato YYYY-MM

        Returns:
            UsageAggregate mensal
        """
        # Buscar ou criar agregacao mensal
        monthly = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == month,
                UsageAggregate.period_type == "monthly"
            )
        ).first()

        if not monthly:
            monthly = UsageAggregate(
                aggregate_id=self._generate_id("AGG"),
                tenant_id=tenant_id,
                period=month,
                period_type="monthly"
            )
            self.db.add(monthly)

        # Somar todas as agregacoes diarias do mes
        daily_aggregates = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period.like(f"{month}-%"),
                UsageAggregate.period_type == "daily"
            )
        ).all()

        # Resetar e somar
        monthly.api_calls = sum(d.api_calls for d in daily_aggregates)
        monthly.llm_tokens_input = sum(d.llm_tokens_input for d in daily_aggregates)
        monthly.llm_tokens_output = sum(d.llm_tokens_output for d in daily_aggregates)
        monthly.storage_bytes = max(d.storage_bytes for d in daily_aggregates) if daily_aggregates else 0  # Storage e cumulativo
        monthly.compute_seconds = sum(d.compute_seconds for d in daily_aggregates)
        monthly.logins_count = sum(d.logins_count for d in daily_aggregates)
        monthly.file_uploads = sum(d.file_uploads for d in daily_aggregates)
        monthly.file_downloads = sum(d.file_downloads for d in daily_aggregates)

        # Custos
        monthly.cost_api_cents = sum(d.cost_api_cents for d in daily_aggregates)
        monthly.cost_llm_cents = sum(d.cost_llm_cents for d in daily_aggregates)
        monthly.cost_storage_cents = sum(d.cost_storage_cents for d in daily_aggregates)
        monthly.cost_compute_cents = sum(d.cost_compute_cents for d in daily_aggregates)
        monthly.calculate_total_cost()

        # Contar usuarios ativos unicos
        unique_users = self.db.query(func.count(func.distinct(UsageEvent.user_id))).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.timestamp >= datetime.strptime(f"{month}-01", "%Y-%m-%d"),
                UsageEvent.user_id.isnot(None)
            )
        ).scalar() or 0
        monthly.active_users = unique_users

        self.db.commit()
        return monthly

    def get_current_usage(
        self,
        tenant_id: str,
        period_type: str = "monthly"
    ) -> Dict[str, Any]:
        """
        Obtem uso atual do periodo.

        Args:
            tenant_id: ID do tenant
            period_type: daily ou monthly

        Returns:
            Dict com metricas de uso
        """
        if period_type == "monthly":
            period = date.today().strftime("%Y-%m")
        else:
            period = date.today().isoformat()

        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == period,
                UsageAggregate.period_type == period_type
            )
        ).first()

        if aggregate:
            return aggregate.to_dict()

        return {
            "tenant_id": tenant_id,
            "period": period,
            "period_type": period_type,
            "metrics": {
                "api_calls": 0,
                "llm_tokens_input": 0,
                "llm_tokens_output": 0,
                "llm_tokens_total": 0,
                "storage_bytes": 0,
                "compute_seconds": 0,
                "active_users": 0,
                "logins_count": 0,
            },
            "costs": {
                "total_cents": 0,
                "total_formatted": "R$ 0.00"
            }
        }

    # =========================================================================
    # CALCULO DE CUSTOS
    # =========================================================================

    def _calculate_event_cost(
        self,
        tenant_id: str,
        event_type: str,
        quantity: int,
        unit: str,
        metadata: Optional[Dict] = None
    ) -> int:
        """
        Calcula custo de um evento em centavos.

        Args:
            tenant_id: ID do tenant
            event_type: Tipo do evento
            quantity: Quantidade
            unit: Unidade
            metadata: Metadados adicionais

        Returns:
            Custo em centavos
        """
        # Buscar pricing tier do tenant
        pricing = self._get_tenant_pricing(tenant_id)

        if not pricing:
            return 0

        cost = 0

        if event_type == UsageEventType.API_CALL.value:
            # Custo por chamada de API
            cost = int(pricing.price_per_1k_api_calls_cents / 1000)

        elif event_type == UsageEventType.LLM_TOKENS.value:
            # Custo por token LLM
            model = (metadata or {}).get("model", "")
            input_tokens = (metadata or {}).get("input_tokens", 0)
            output_tokens = (metadata or {}).get("output_tokens", 0)

            # Precos diferenciados por modelo
            input_price = pricing.get_llm_price(model, "input")
            output_price = pricing.get_llm_price(model, "output")

            cost = int(
                (input_tokens * input_price / 1000) +
                (output_tokens * output_price / 1000)
            )

        elif event_type in [UsageEventType.STORAGE.value, UsageEventType.FILE_UPLOAD.value]:
            # Custo por storage (por GB)
            gb = quantity / (1024 * 1024 * 1024)
            cost = int(gb * pricing.price_per_gb_storage_cents)

        elif event_type == UsageEventType.COMPUTE.value:
            # Custo por compute (por hora)
            hours = quantity / 3600
            cost = int(hours * pricing.price_per_compute_hour_cents)

        return cost

    def _get_tenant_pricing(self, tenant_id: str) -> Optional[PricingTier]:
        """
        Obtem pricing tier do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            PricingTier ou None
        """
        # Buscar subscription ativa do tenant
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
            # Usar pricing padrao
            return self.db.query(PricingTier).filter(
                PricingTier.is_default == True
            ).first()

        # Buscar pricing especifico do plano
        pricing = self.db.query(PricingTier).filter(
            and_(
                PricingTier.plan_id == subscription.plan_id,
                PricingTier.is_active == True
            )
        ).first()

        if pricing:
            return pricing

        # Fallback para pricing padrao
        return self.db.query(PricingTier).filter(
            PricingTier.is_default == True
        ).first()

    def _check_llm_limit(self, tenant_id: str) -> bool:
        """
        Verifica se tenant excedeu limite de tokens LLM.

        Args:
            tenant_id: ID do tenant

        Returns:
            True se excedeu limite
        """
        month = date.today().strftime("%Y-%m")

        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == month,
                UsageAggregate.period_type == "monthly"
            )
        ).first()

        if not aggregate:
            return False

        total_tokens = aggregate.llm_tokens_input + aggregate.llm_tokens_output

        # Buscar limite do plano
        limit = self._get_metric_limit(tenant_id, "max_tokens_monthly")

        if limit is None or limit == 0:
            return False  # Sem limite

        return total_tokens >= limit

    def _track_legacy_usage(self, event: UsageEvent) -> None:
        """
        Mantém compatibilidade com sistema legado de Usage.

        Args:
            event: Evento de uso
        """
        # Mapear tipo de evento para metrica legada
        metric_map = {
            UsageEventType.API_CALL.value: (UsageMetric.API_REQUESTS.value, "daily"),
            UsageEventType.LLM_TOKENS.value: (UsageMetric.LLM_TOKENS.value, "monthly"),
            UsageEventType.STORAGE.value: (UsageMetric.STORAGE_BYTES.value, "monthly"),
            UsageEventType.FILE_UPLOAD.value: (UsageMetric.STORAGE_BYTES.value, "monthly"),
            UsageEventType.LOGIN.value: (UsageMetric.USERS_ACTIVE.value, "daily"),
        }

        mapping = metric_map.get(event.event_type)
        if not mapping:
            return

        metric, period_type = mapping

        # Usar metodo existente
        self.track_usage(
            tenant_id=event.tenant_id,
            metric=metric,
            value=event.quantity,
            context=event.event_data,
            period_type=period_type
        )

    # =========================================================================
    # QUERIES DE EVENTOS
    # =========================================================================

    def get_events(
        self,
        tenant_id: str,
        event_type: Optional[str] = None,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[UsageEvent]:
        """
        Lista eventos de uso com filtros.

        Args:
            tenant_id: ID do tenant
            event_type: Filtrar por tipo
            user_id: Filtrar por usuario
            project_id: Filtrar por projeto
            start_date: Data inicial
            end_date: Data final
            limit: Limite de resultados
            offset: Offset para paginacao

        Returns:
            Lista de UsageEvents
        """
        query = self.db.query(UsageEvent).filter(
            UsageEvent.tenant_id == tenant_id
        )

        if event_type:
            query = query.filter(UsageEvent.event_type == event_type)

        if user_id:
            query = query.filter(UsageEvent.user_id == user_id)

        if project_id:
            query = query.filter(UsageEvent.project_id == project_id)

        if start_date:
            query = query.filter(UsageEvent.timestamp >= start_date)

        if end_date:
            query = query.filter(UsageEvent.timestamp <= end_date)

        return query.order_by(UsageEvent.timestamp.desc()).offset(offset).limit(limit).all()

    def get_usage_breakdown(
        self,
        tenant_id: str,
        month: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem breakdown detalhado de uso e custos.

        Args:
            tenant_id: ID do tenant
            month: Mes no formato YYYY-MM (atual se None)

        Returns:
            Dict com breakdown de uso
        """
        if not month:
            month = date.today().strftime("%Y-%m")

        # Agregar uso mensal
        self.aggregate_monthly_usage(tenant_id, month)

        # Buscar agregacao
        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == month,
                UsageAggregate.period_type == "monthly"
            )
        ).first()

        if not aggregate:
            return {"tenant_id": tenant_id, "month": month, "breakdown": {}}

        # Buscar pricing para limites
        pricing = self._get_tenant_pricing(tenant_id)

        breakdown = {
            "tenant_id": tenant_id,
            "month": month,
            "summary": aggregate.to_dict(),
            "breakdown": {
                "api_calls": {
                    "used": aggregate.api_calls,
                    "included": pricing.included_api_calls if pricing else 0,
                    "overage": max(0, aggregate.api_calls - (pricing.included_api_calls if pricing else 0)),
                    "cost_cents": aggregate.cost_api_cents,
                },
                "llm_tokens": {
                    "input": aggregate.llm_tokens_input,
                    "output": aggregate.llm_tokens_output,
                    "total": aggregate.llm_tokens_input + aggregate.llm_tokens_output,
                    "included": pricing.included_tokens if pricing else 0,
                    "cost_cents": aggregate.cost_llm_cents,
                },
                "storage": {
                    "used_bytes": aggregate.storage_bytes,
                    "used_mb": round(aggregate.storage_bytes / (1024 * 1024), 2),
                    "included_mb": pricing.included_storage_mb if pricing else 0,
                    "cost_cents": aggregate.cost_storage_cents,
                },
                "compute": {
                    "used_seconds": aggregate.compute_seconds,
                    "used_minutes": round(aggregate.compute_seconds / 60, 2),
                    "included_minutes": pricing.included_compute_minutes if pricing else 0,
                    "cost_cents": aggregate.cost_compute_cents,
                },
                "activity": {
                    "active_users": aggregate.active_users,
                    "logins_count": aggregate.logins_count,
                    "file_uploads": aggregate.file_uploads,
                    "file_downloads": aggregate.file_downloads,
                },
            },
            "total_cost_cents": aggregate.cost_total_cents,
            "total_cost_formatted": f"R$ {aggregate.cost_total_cents / 100:.2f}",
        }

        return breakdown
