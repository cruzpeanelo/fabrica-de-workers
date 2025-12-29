# -*- coding: utf-8 -*-
"""
Sistema de Metering e Usage Tracking Completo (Issue #118)
==========================================================

Implementa:
- Track de API calls por tenant
- Track de storage utilizado
- Track de workers executados
- Agregacoes por hora/dia/mes
- Alertas de limite
- Relatorios de consumo detalhados
- Calculo de custos em tempo real

Autor: Fabrica de Agentes
"""

import uuid
import logging
from datetime import datetime, date, timedelta
from typing import Optional, Dict, Any, List, Tuple
from collections import defaultdict
from decimal import Decimal
from enum import Enum

from sqlalchemy.orm import Session
from sqlalchemy import and_, func, extract, or_, text

from .models import (
    Tenant, Plan, Subscription, Usage,
    UsageMetric, SubscriptionStatus,
    UsageEvent, UsageAggregate, UsageEventType, PricingTier
)

# Configurar logging
logger = logging.getLogger(__name__)


class AggregationPeriod(str, Enum):
    """Periodos de agregacao"""
    HOURLY = "hourly"
    DAILY = "daily"
    WEEKLY = "weekly"
    MONTHLY = "monthly"


class MeteringAlert(str, Enum):
    """Tipos de alerta de metering"""
    INFO = "info"           # 50% do limite
    WARNING = "warning"     # 75% do limite
    CRITICAL = "critical"   # 90% do limite
    EXCEEDED = "exceeded"   # 100%+ do limite


# Configuracao de custos por recurso (centavos)
DEFAULT_PRICING = {
    "api_calls": {
        "unit": "calls",
        "price_per_1k": 10,  # R$ 0.10 por 1000 chamadas
        "included": 10000,   # 10k chamadas incluidas
    },
    "llm_tokens": {
        "unit": "tokens",
        "input_per_1k": 3,   # R$ 0.03 por 1k tokens de entrada
        "output_per_1k": 15, # R$ 0.15 por 1k tokens de saida
        "included": 100000,  # 100k tokens incluidos
    },
    "storage": {
        "unit": "bytes",
        "price_per_gb": 50,  # R$ 0.50 por GB/mes
        "included_mb": 1024, # 1GB incluido
    },
    "compute": {
        "unit": "seconds",
        "price_per_hour": 100,  # R$ 1.00 por hora
        "included_minutes": 60, # 60 minutos incluidos
    },
    "workers": {
        "unit": "executions",
        "price_per_execution": 5,  # R$ 0.05 por execucao
        "included": 100,  # 100 execucoes incluidas
    },
}


class MeteringService:
    """
    Servico de Metering para tracking detalhado de uso.

    Responsabilidades:
    - Registrar eventos de uso granulares
    - Agregar uso por periodo (hora/dia/mes)
    - Calcular custos em tempo real
    - Gerar alertas de limite
    - Fornecer relatorios de consumo

    Uso:
        service = MeteringService(db_session)

        # Registrar uso de API
        service.track_api_call(tenant_id, "/api/stories", "POST", 201, 150)

        # Registrar uso de tokens
        service.track_llm_tokens(tenant_id, model="claude-sonnet", input=1000, output=500)

        # Obter resumo de uso
        summary = service.get_usage_summary(tenant_id, period="monthly")
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico de metering.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self._cache = {}  # Cache de agregacoes em memoria

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    def _get_current_period(self, period_type: str) -> str:
        """
        Retorna o periodo atual no formato apropriado.

        Args:
            period_type: hourly, daily, weekly, monthly

        Returns:
            String do periodo (ex: "2025-01", "2025-01-15", "2025-01-15-14")
        """
        now = datetime.utcnow()

        if period_type == AggregationPeriod.HOURLY.value:
            return now.strftime("%Y-%m-%d-%H")
        elif period_type == AggregationPeriod.WEEKLY.value:
            # ISO week number
            return now.strftime("%Y-W%V")
        elif period_type == AggregationPeriod.MONTHLY.value:
            return now.strftime("%Y-%m")
        else:  # daily
            return now.strftime("%Y-%m-%d")

    # =========================================================================
    # API CALL TRACKING
    # =========================================================================

    def track_api_call(
        self,
        tenant_id: str,
        endpoint: str,
        method: str,
        status_code: int,
        duration_ms: float,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        request_size_bytes: int = 0,
        response_size_bytes: int = 0,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra uma chamada de API.

        Args:
            tenant_id: ID do tenant
            endpoint: Path do endpoint (ex: /api/stories)
            method: Metodo HTTP (GET, POST, etc)
            status_code: Codigo de resposta
            duration_ms: Duracao em milissegundos
            user_id: ID do usuario (opcional)
            project_id: ID do projeto (opcional)
            ip_address: IP do cliente
            user_agent: User-Agent do cliente
            request_size_bytes: Tamanho da requisicao
            response_size_bytes: Tamanho da resposta
            correlation_id: ID de correlacao

        Returns:
            UsageEvent criado
        """
        event_data = {
            "endpoint": endpoint,
            "method": method,
            "status_code": status_code,
            "duration_ms": round(duration_ms, 2),
            "request_size_bytes": request_size_bytes,
            "response_size_bytes": response_size_bytes,
            "success": 200 <= status_code < 400,
        }

        # Calcular custo
        pricing = self._get_tenant_pricing(tenant_id)
        cost_cents = self._calculate_api_cost(pricing, 1)

        event = UsageEvent(
            event_id=self._generate_id("EVT"),
            tenant_id=tenant_id,
            user_id=user_id,
            project_id=project_id,
            event_type=UsageEventType.API_CALL.value,
            resource=endpoint,
            action=method.lower(),
            quantity=1,
            unit="calls",
            cost_cents=cost_cents,
            event_data=event_data,
            ip_address=ip_address,
            user_agent=user_agent[:500] if user_agent else None,
            correlation_id=correlation_id,
            timestamp=datetime.utcnow()
        )

        try:
            self.db.add(event)
            self.db.flush()

            # Atualizar agregacoes
            self._update_aggregates(event)

            self.db.commit()
            logger.debug(f"API call registrada: {endpoint} {method} [{status_code}]")
            return event

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar API call: {e}")
            raise

    # =========================================================================
    # STORAGE TRACKING
    # =========================================================================

    def track_storage_usage(
        self,
        tenant_id: str,
        operation: str,
        file_path: str,
        file_size_bytes: int,
        file_type: Optional[str] = None,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> UsageEvent:
        """
        Registra uso de storage.

        Args:
            tenant_id: ID do tenant
            operation: Tipo de operacao (upload, download, delete)
            file_path: Caminho do arquivo
            file_size_bytes: Tamanho em bytes
            file_type: Tipo MIME do arquivo
            user_id: ID do usuario
            project_id: ID do projeto
            correlation_id: ID de correlacao

        Returns:
            UsageEvent criado
        """
        event_type = UsageEventType.FILE_UPLOAD.value if operation == "upload" else (
            UsageEventType.FILE_DOWNLOAD.value if operation == "download" else
            UsageEventType.STORAGE.value
        )

        event_data = {
            "file_path": file_path,
            "file_size_bytes": file_size_bytes,
            "file_type": file_type,
            "operation": operation,
            "file_size_mb": round(file_size_bytes / (1024 * 1024), 4),
        }

        # Calcular custo (apenas para uploads - storage cumulativo)
        cost_cents = 0
        if operation == "upload":
            pricing = self._get_tenant_pricing(tenant_id)
            cost_cents = self._calculate_storage_cost(pricing, file_size_bytes)

        event = UsageEvent(
            event_id=self._generate_id("EVT"),
            tenant_id=tenant_id,
            user_id=user_id,
            project_id=project_id,
            event_type=event_type,
            resource="storage",
            action=operation,
            quantity=file_size_bytes,
            unit="bytes",
            cost_cents=cost_cents,
            event_data=event_data,
            correlation_id=correlation_id,
            timestamp=datetime.utcnow()
        )

        try:
            self.db.add(event)
            self.db.flush()

            self._update_aggregates(event)

            self.db.commit()
            logger.debug(f"Storage {operation} registrado: {file_path} ({file_size_bytes} bytes)")
            return event

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar storage: {e}")
            raise

    def get_storage_total(self, tenant_id: str) -> Dict[str, Any]:
        """
        Obtem total de storage utilizado pelo tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dict com total de storage em bytes e MB
        """
        # Somar uploads e subtrair deletes
        result = self.db.query(
            func.sum(
                func.case(
                    (UsageEvent.action == "upload", UsageEvent.quantity),
                    (UsageEvent.action == "delete", -UsageEvent.quantity),
                    else_=0
                )
            ).label("total_bytes")
        ).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.event_type.in_([
                    UsageEventType.STORAGE.value,
                    UsageEventType.FILE_UPLOAD.value
                ])
            )
        ).scalar() or 0

        return {
            "total_bytes": int(result),
            "total_mb": round(result / (1024 * 1024), 2),
            "total_gb": round(result / (1024 * 1024 * 1024), 4),
        }

    # =========================================================================
    # WORKER TRACKING
    # =========================================================================

    def track_worker_execution(
        self,
        tenant_id: str,
        worker_id: str,
        job_id: str,
        duration_seconds: int,
        status: str,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None,
        task_type: Optional[str] = None,
        correlation_id: Optional[str] = None,
        tokens_used: int = 0,
        files_generated: int = 0
    ) -> UsageEvent:
        """
        Registra execucao de worker.

        Args:
            tenant_id: ID do tenant
            worker_id: ID do worker
            job_id: ID do job
            duration_seconds: Duracao em segundos
            status: Status final (success, failed, timeout)
            project_id: ID do projeto
            user_id: ID do usuario que iniciou
            task_type: Tipo de tarefa (development, review, test)
            correlation_id: ID de correlacao
            tokens_used: Tokens LLM usados
            files_generated: Arquivos gerados

        Returns:
            UsageEvent criado
        """
        event_data = {
            "worker_id": worker_id,
            "job_id": job_id,
            "duration_seconds": duration_seconds,
            "duration_minutes": round(duration_seconds / 60, 2),
            "status": status,
            "task_type": task_type,
            "tokens_used": tokens_used,
            "files_generated": files_generated,
            "success": status == "success",
        }

        # Calcular custo (tempo de compute + execucao)
        pricing = self._get_tenant_pricing(tenant_id)
        cost_cents = self._calculate_compute_cost(pricing, duration_seconds)
        cost_cents += self._calculate_worker_cost(pricing, 1)

        event = UsageEvent(
            event_id=self._generate_id("EVT"),
            tenant_id=tenant_id,
            user_id=user_id,
            project_id=project_id,
            event_type=UsageEventType.COMPUTE.value,
            resource=f"worker/{worker_id}",
            action="execute",
            quantity=duration_seconds,
            unit="seconds",
            cost_cents=cost_cents,
            event_data=event_data,
            correlation_id=correlation_id,
            timestamp=datetime.utcnow()
        )

        try:
            self.db.add(event)
            self.db.flush()

            self._update_aggregates(event)

            self.db.commit()
            logger.debug(f"Worker execution registrada: {worker_id}/{job_id} ({duration_seconds}s)")
            return event

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar worker execution: {e}")
            raise

    # =========================================================================
    # LLM TOKEN TRACKING
    # =========================================================================

    def track_llm_usage(
        self,
        tenant_id: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        job_id: Optional[str] = None,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None,
        correlation_id: Optional[str] = None,
        prompt_type: Optional[str] = None,
        latency_ms: Optional[float] = None
    ) -> Tuple[UsageEvent, bool]:
        """
        Registra uso de tokens LLM.

        Args:
            tenant_id: ID do tenant
            model: Nome do modelo (claude-sonnet-4-20250514, etc)
            input_tokens: Tokens de entrada
            output_tokens: Tokens de saida
            job_id: ID do job relacionado
            project_id: ID do projeto
            user_id: ID do usuario
            correlation_id: ID de correlacao
            prompt_type: Tipo de prompt (code_generation, review, etc)
            latency_ms: Latencia da requisicao

        Returns:
            Tupla (UsageEvent, is_over_limit)
        """
        total_tokens = input_tokens + output_tokens

        event_data = {
            "model": model,
            "input_tokens": input_tokens,
            "output_tokens": output_tokens,
            "total_tokens": total_tokens,
            "job_id": job_id,
            "prompt_type": prompt_type,
            "latency_ms": latency_ms,
        }

        # Calcular custo
        pricing = self._get_tenant_pricing(tenant_id)
        cost_cents = self._calculate_llm_cost(pricing, model, input_tokens, output_tokens)

        event = UsageEvent(
            event_id=self._generate_id("EVT"),
            tenant_id=tenant_id,
            user_id=user_id,
            project_id=project_id,
            event_type=UsageEventType.LLM_TOKENS.value,
            resource=model,
            action="generate",
            quantity=total_tokens,
            unit="tokens",
            cost_cents=cost_cents,
            event_data=event_data,
            correlation_id=correlation_id,
            timestamp=datetime.utcnow()
        )

        try:
            self.db.add(event)
            self.db.flush()

            self._update_aggregates(event)

            # Verificar limite
            is_over_limit = self._check_token_limit(tenant_id)

            self.db.commit()

            if is_over_limit:
                logger.warning(f"Tenant {tenant_id} excedeu limite de tokens LLM")

            return event, is_over_limit

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao registrar LLM usage: {e}")
            raise

    # =========================================================================
    # AGGREGATION
    # =========================================================================

    def _update_aggregates(self, event: UsageEvent) -> None:
        """
        Atualiza agregacoes para todos os periodos.

        Args:
            event: Evento de uso
        """
        for period_type in [
            AggregationPeriod.HOURLY.value,
            AggregationPeriod.DAILY.value,
            AggregationPeriod.MONTHLY.value
        ]:
            self._update_aggregate(event, period_type)

    def _update_aggregate(self, event: UsageEvent, period_type: str) -> UsageAggregate:
        """
        Atualiza agregacao para um periodo especifico.

        Args:
            event: Evento de uso
            period_type: Tipo do periodo (hourly, daily, monthly)

        Returns:
            UsageAggregate atualizado
        """
        period = self._get_current_period(period_type)

        # Buscar ou criar agregacao
        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == event.tenant_id,
                UsageAggregate.period == period,
                UsageAggregate.period_type == period_type
            )
        ).first()

        if not aggregate:
            aggregate = UsageAggregate(
                aggregate_id=self._generate_id("AGG"),
                tenant_id=event.tenant_id,
                period=period,
                period_type=period_type
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

    def aggregate_period(
        self,
        tenant_id: str,
        period: str,
        period_type: str = "daily"
    ) -> UsageAggregate:
        """
        Agrega ou reagrega uso para um periodo especifico.

        Args:
            tenant_id: ID do tenant
            period: Periodo (ex: "2025-01-15", "2025-01")
            period_type: Tipo do periodo

        Returns:
            UsageAggregate
        """
        # Determinar range de datas baseado no periodo
        start_date, end_date = self._parse_period_range(period, period_type)

        # Buscar ou criar agregacao
        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == period,
                UsageAggregate.period_type == period_type
            )
        ).first()

        if not aggregate:
            aggregate = UsageAggregate(
                aggregate_id=self._generate_id("AGG"),
                tenant_id=tenant_id,
                period=period,
                period_type=period_type
            )
            self.db.add(aggregate)
        else:
            # Reset para reagregacao
            aggregate.api_calls = 0
            aggregate.llm_tokens_input = 0
            aggregate.llm_tokens_output = 0
            aggregate.storage_bytes = 0
            aggregate.compute_seconds = 0
            aggregate.active_users = 0
            aggregate.active_sessions = 0
            aggregate.logins_count = 0
            aggregate.file_uploads = 0
            aggregate.file_downloads = 0
            aggregate.cost_api_cents = 0
            aggregate.cost_llm_cents = 0
            aggregate.cost_storage_cents = 0
            aggregate.cost_compute_cents = 0

        # Buscar eventos do periodo
        events = self.db.query(UsageEvent).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.timestamp >= start_date,
                UsageEvent.timestamp < end_date
            )
        ).all()

        # Agregar
        for event in events:
            if event.event_type == UsageEventType.API_CALL.value:
                aggregate.api_calls += 1
                aggregate.cost_api_cents += event.cost_cents
            elif event.event_type == UsageEventType.LLM_TOKENS.value:
                ed = event.event_data or {}
                aggregate.llm_tokens_input += ed.get("input_tokens", 0)
                aggregate.llm_tokens_output += ed.get("output_tokens", 0)
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

        # Contar usuarios unicos
        unique_users = self.db.query(func.count(func.distinct(UsageEvent.user_id))).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.timestamp >= start_date,
                UsageEvent.timestamp < end_date,
                UsageEvent.user_id.isnot(None)
            )
        ).scalar() or 0
        aggregate.active_users = unique_users

        aggregate.calculate_total_cost()

        self.db.commit()
        return aggregate

    def aggregate_hourly(self, tenant_id: str, hour: str = None) -> UsageAggregate:
        """
        Agrega uso por hora.

        Args:
            tenant_id: ID do tenant
            hour: Hora no formato YYYY-MM-DD-HH (atual se None)

        Returns:
            UsageAggregate horario
        """
        if not hour:
            hour = self._get_current_period(AggregationPeriod.HOURLY.value)
        return self.aggregate_period(tenant_id, hour, AggregationPeriod.HOURLY.value)

    def aggregate_daily(self, tenant_id: str, day: str = None) -> UsageAggregate:
        """
        Agrega uso por dia.

        Args:
            tenant_id: ID do tenant
            day: Dia no formato YYYY-MM-DD (atual se None)

        Returns:
            UsageAggregate diario
        """
        if not day:
            day = self._get_current_period(AggregationPeriod.DAILY.value)
        return self.aggregate_period(tenant_id, day, AggregationPeriod.DAILY.value)

    def aggregate_monthly(self, tenant_id: str, month: str = None) -> UsageAggregate:
        """
        Agrega uso por mes.

        Args:
            tenant_id: ID do tenant
            month: Mes no formato YYYY-MM (atual se None)

        Returns:
            UsageAggregate mensal
        """
        if not month:
            month = self._get_current_period(AggregationPeriod.MONTHLY.value)
        return self.aggregate_period(tenant_id, month, AggregationPeriod.MONTHLY.value)

    def _parse_period_range(self, period: str, period_type: str) -> Tuple[datetime, datetime]:
        """
        Converte periodo em range de datas.

        Args:
            period: String do periodo
            period_type: Tipo do periodo

        Returns:
            Tupla (start_date, end_date)
        """
        if period_type == AggregationPeriod.HOURLY.value:
            # YYYY-MM-DD-HH
            start = datetime.strptime(period, "%Y-%m-%d-%H")
            end = start + timedelta(hours=1)
        elif period_type == AggregationPeriod.WEEKLY.value:
            # YYYY-WNN
            year, week = period.split("-W")
            start = datetime.strptime(f"{year}-W{week}-1", "%Y-W%W-%w")
            end = start + timedelta(days=7)
        elif period_type == AggregationPeriod.MONTHLY.value:
            # YYYY-MM
            start = datetime.strptime(f"{period}-01", "%Y-%m-%d")
            if start.month == 12:
                end = start.replace(year=start.year + 1, month=1)
            else:
                end = start.replace(month=start.month + 1)
        else:  # daily
            # YYYY-MM-DD
            start = datetime.strptime(period, "%Y-%m-%d")
            end = start + timedelta(days=1)

        return start, end

    # =========================================================================
    # USAGE SUMMARY & REPORTS
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
            period_type: hourly, daily, monthly

        Returns:
            Dict com resumo de uso e custos
        """
        period = self._get_current_period(period_type)

        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == period,
                UsageAggregate.period_type == period_type
            )
        ).first()

        # Obter limites do plano
        pricing = self._get_tenant_pricing(tenant_id)

        if aggregate:
            summary = aggregate.to_dict()
        else:
            summary = {
                "tenant_id": tenant_id,
                "period": period,
                "period_type": period_type,
                "metrics": {
                    "api_calls": 0,
                    "llm_tokens_input": 0,
                    "llm_tokens_output": 0,
                    "llm_tokens_total": 0,
                    "storage_bytes": 0,
                    "storage_mb": 0,
                    "compute_seconds": 0,
                    "compute_minutes": 0,
                    "active_users": 0,
                    "logins_count": 0,
                    "file_uploads": 0,
                    "file_downloads": 0,
                },
                "costs": {
                    "api_cents": 0,
                    "llm_cents": 0,
                    "storage_cents": 0,
                    "compute_cents": 0,
                    "total_cents": 0,
                    "total_formatted": "R$ 0.00",
                }
            }

        # Adicionar limites e porcentagens
        if pricing:
            summary["limits"] = {
                "api_calls": pricing.included_api_calls,
                "tokens": pricing.included_tokens,
                "storage_mb": pricing.included_storage_mb,
                "compute_minutes": pricing.included_compute_minutes,
            }

            metrics = summary.get("metrics", {})
            summary["usage_percent"] = {
                "api_calls": self._calc_percent(metrics.get("api_calls", 0), pricing.included_api_calls),
                "tokens": self._calc_percent(
                    metrics.get("llm_tokens_total", metrics.get("llm_tokens_input", 0) + metrics.get("llm_tokens_output", 0)),
                    pricing.included_tokens
                ),
                "storage": self._calc_percent(
                    metrics.get("storage_mb", metrics.get("storage_bytes", 0) / (1024 * 1024)),
                    pricing.included_storage_mb
                ),
                "compute": self._calc_percent(
                    metrics.get("compute_minutes", metrics.get("compute_seconds", 0) / 60),
                    pricing.included_compute_minutes
                ),
            }

        return summary

    def get_usage_history(
        self,
        tenant_id: str,
        period_type: str = "daily",
        periods: int = 30
    ) -> List[Dict[str, Any]]:
        """
        Obtem historico de uso.

        Args:
            tenant_id: ID do tenant
            period_type: Tipo de periodo (hourly, daily, monthly)
            periods: Numero de periodos

        Returns:
            Lista de agregacoes ordenadas por periodo
        """
        aggregates = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period_type == period_type
            )
        ).order_by(UsageAggregate.period.desc()).limit(periods).all()

        return [a.to_dict() for a in reversed(aggregates)]

    def get_usage_breakdown(
        self,
        tenant_id: str,
        month: str = None
    ) -> Dict[str, Any]:
        """
        Obtem breakdown detalhado de uso por categoria.

        Args:
            tenant_id: ID do tenant
            month: Mes no formato YYYY-MM (atual se None)

        Returns:
            Dict com breakdown detalhado
        """
        if not month:
            month = self._get_current_period(AggregationPeriod.MONTHLY.value)

        # Reagregar para garantir dados atualizados
        aggregate = self.aggregate_monthly(tenant_id, month)
        pricing = self._get_tenant_pricing(tenant_id)

        breakdown = {
            "tenant_id": tenant_id,
            "month": month,
            "summary": aggregate.to_dict() if aggregate else {},
            "breakdown": {
                "api_calls": {
                    "used": aggregate.api_calls if aggregate else 0,
                    "included": pricing.included_api_calls if pricing else 0,
                    "overage": max(0, (aggregate.api_calls if aggregate else 0) - (pricing.included_api_calls if pricing else 0)),
                    "cost_cents": aggregate.cost_api_cents if aggregate else 0,
                },
                "llm_tokens": {
                    "input": aggregate.llm_tokens_input if aggregate else 0,
                    "output": aggregate.llm_tokens_output if aggregate else 0,
                    "total": (aggregate.llm_tokens_input + aggregate.llm_tokens_output) if aggregate else 0,
                    "included": pricing.included_tokens if pricing else 0,
                    "overage": max(0, ((aggregate.llm_tokens_input + aggregate.llm_tokens_output) if aggregate else 0) - (pricing.included_tokens if pricing else 0)),
                    "cost_cents": aggregate.cost_llm_cents if aggregate else 0,
                },
                "storage": {
                    "used_bytes": aggregate.storage_bytes if aggregate else 0,
                    "used_mb": round((aggregate.storage_bytes if aggregate else 0) / (1024 * 1024), 2),
                    "included_mb": pricing.included_storage_mb if pricing else 0,
                    "overage_mb": max(0, round(((aggregate.storage_bytes if aggregate else 0) / (1024 * 1024)) - (pricing.included_storage_mb if pricing else 0), 2)),
                    "cost_cents": aggregate.cost_storage_cents if aggregate else 0,
                },
                "compute": {
                    "used_seconds": aggregate.compute_seconds if aggregate else 0,
                    "used_minutes": round((aggregate.compute_seconds if aggregate else 0) / 60, 2),
                    "included_minutes": pricing.included_compute_minutes if pricing else 0,
                    "overage_minutes": max(0, round(((aggregate.compute_seconds if aggregate else 0) / 60) - (pricing.included_compute_minutes if pricing else 0), 2)),
                    "cost_cents": aggregate.cost_compute_cents if aggregate else 0,
                },
                "activity": {
                    "active_users": aggregate.active_users if aggregate else 0,
                    "logins_count": aggregate.logins_count if aggregate else 0,
                    "file_uploads": aggregate.file_uploads if aggregate else 0,
                    "file_downloads": aggregate.file_downloads if aggregate else 0,
                },
            },
            "total_cost_cents": aggregate.cost_total_cents if aggregate else 0,
            "total_cost_formatted": f"R$ {(aggregate.cost_total_cents if aggregate else 0) / 100:.2f}",
        }

        return breakdown

    def get_top_endpoints(
        self,
        tenant_id: str,
        period: str = None,
        limit: int = 10
    ) -> List[Dict[str, Any]]:
        """
        Obtem endpoints mais utilizados.

        Args:
            tenant_id: ID do tenant
            period: Periodo (ex: "2025-01") ou None para mes atual
            limit: Limite de resultados

        Returns:
            Lista de endpoints com contagem
        """
        if not period:
            period = self._get_current_period(AggregationPeriod.MONTHLY.value)

        start_date, end_date = self._parse_period_range(period, AggregationPeriod.MONTHLY.value)

        results = self.db.query(
            UsageEvent.resource,
            func.count(UsageEvent.id).label("count"),
            func.avg(
                func.json_extract(UsageEvent.event_data, "$.duration_ms")
            ).label("avg_duration_ms")
        ).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.event_type == UsageEventType.API_CALL.value,
                UsageEvent.timestamp >= start_date,
                UsageEvent.timestamp < end_date
            )
        ).group_by(UsageEvent.resource).order_by(func.count(UsageEvent.id).desc()).limit(limit).all()

        return [
            {
                "endpoint": r.resource,
                "count": r.count,
                "avg_duration_ms": round(r.avg_duration_ms, 2) if r.avg_duration_ms else 0
            }
            for r in results
        ]

    def get_usage_by_project(
        self,
        tenant_id: str,
        period: str = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem uso agrupado por projeto.

        Args:
            tenant_id: ID do tenant
            period: Periodo ou None para mes atual

        Returns:
            Lista de projetos com uso
        """
        if not period:
            period = self._get_current_period(AggregationPeriod.MONTHLY.value)

        start_date, end_date = self._parse_period_range(period, AggregationPeriod.MONTHLY.value)

        results = self.db.query(
            UsageEvent.project_id,
            func.count(UsageEvent.id).label("events"),
            func.sum(UsageEvent.quantity).label("total_quantity"),
            func.sum(UsageEvent.cost_cents).label("total_cost")
        ).filter(
            and_(
                UsageEvent.tenant_id == tenant_id,
                UsageEvent.timestamp >= start_date,
                UsageEvent.timestamp < end_date,
                UsageEvent.project_id.isnot(None)
            )
        ).group_by(UsageEvent.project_id).order_by(func.sum(UsageEvent.cost_cents).desc()).all()

        return [
            {
                "project_id": r.project_id,
                "events": r.events,
                "total_quantity": r.total_quantity,
                "total_cost_cents": r.total_cost,
                "total_cost_formatted": f"R$ {r.total_cost / 100:.2f}"
            }
            for r in results
        ]

    # =========================================================================
    # ALERTS
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
        usage_percent = summary.get("usage_percent", {})

        # Verificar cada metrica
        metrics_labels = {
            "api_calls": "Chamadas de API",
            "tokens": "Tokens LLM",
            "storage": "Armazenamento",
            "compute": "Tempo de Compute",
        }

        for metric, label in metrics_labels.items():
            percent = usage_percent.get(metric, 0)

            if percent >= 100:
                alerts.append({
                    "level": MeteringAlert.EXCEEDED.value,
                    "metric": metric,
                    "label": label,
                    "message": f"Limite de {label} excedido ({percent:.1f}%)",
                    "percentage": percent,
                    "action": "upgrade_plan"
                })
            elif percent >= 90:
                alerts.append({
                    "level": MeteringAlert.CRITICAL.value,
                    "metric": metric,
                    "label": label,
                    "message": f"{label} em {percent:.1f}% do limite - critico",
                    "percentage": percent,
                    "action": "monitor_closely"
                })
            elif percent >= 75:
                alerts.append({
                    "level": MeteringAlert.WARNING.value,
                    "metric": metric,
                    "label": label,
                    "message": f"{label} em {percent:.1f}% do limite",
                    "percentage": percent,
                    "action": "review_usage"
                })
            elif percent >= 50:
                alerts.append({
                    "level": MeteringAlert.INFO.value,
                    "metric": metric,
                    "label": label,
                    "message": f"{label} em {percent:.1f}% do limite",
                    "percentage": percent,
                    "action": "none"
                })

        return sorted(alerts, key=lambda x: x["percentage"], reverse=True)

    def get_tenants_approaching_limits(
        self,
        threshold_percent: float = 80.0
    ) -> List[Dict[str, Any]]:
        """
        Lista tenants proximos de atingir limites.

        Args:
            threshold_percent: Percentual minimo de uso

        Returns:
            Lista de tenants com alertas
        """
        month = self._get_current_period(AggregationPeriod.MONTHLY.value)

        # Buscar todas as agregacoes do mes
        aggregates = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.period == month,
                UsageAggregate.period_type == AggregationPeriod.MONTHLY.value
            )
        ).all()

        results = []
        for agg in aggregates:
            alerts = self.check_usage_alerts(agg.tenant_id)
            critical_alerts = [a for a in alerts if a["percentage"] >= threshold_percent]

            if critical_alerts:
                results.append({
                    "tenant_id": agg.tenant_id,
                    "alerts": critical_alerts,
                    "max_percentage": max(a["percentage"] for a in critical_alerts)
                })

        return sorted(results, key=lambda x: x["max_percentage"], reverse=True)

    # =========================================================================
    # COST CALCULATION
    # =========================================================================

    def _get_tenant_pricing(self, tenant_id: str) -> Optional[PricingTier]:
        """
        Obtem pricing tier do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            PricingTier ou None
        """
        # Buscar subscription ativa
        subscription = self.db.query(Subscription).filter(
            and_(
                Subscription.tenant_id == tenant_id,
                Subscription.status.in_([
                    SubscriptionStatus.ACTIVE.value,
                    SubscriptionStatus.TRIALING.value
                ])
            )
        ).first()

        if subscription:
            # Buscar pricing do plano
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

    def _calculate_api_cost(self, pricing: Optional[PricingTier], calls: int) -> int:
        """Calcula custo de chamadas de API"""
        if not pricing:
            return int(calls * DEFAULT_PRICING["api_calls"]["price_per_1k"] / 1000)
        return int(calls * pricing.price_per_1k_api_calls_cents / 1000)

    def _calculate_llm_cost(
        self,
        pricing: Optional[PricingTier],
        model: str,
        input_tokens: int,
        output_tokens: int
    ) -> int:
        """Calcula custo de tokens LLM"""
        if not pricing:
            input_cost = input_tokens * DEFAULT_PRICING["llm_tokens"]["input_per_1k"] / 1000
            output_cost = output_tokens * DEFAULT_PRICING["llm_tokens"]["output_per_1k"] / 1000
            return int(input_cost + output_cost)

        # Verificar preco especifico do modelo
        input_price = pricing.get_llm_price(model, "input")
        output_price = pricing.get_llm_price(model, "output")

        return int(
            (input_tokens * input_price / 1000) +
            (output_tokens * output_price / 1000)
        )

    def _calculate_storage_cost(self, pricing: Optional[PricingTier], bytes_count: int) -> int:
        """Calcula custo de storage"""
        gb = bytes_count / (1024 * 1024 * 1024)
        if not pricing:
            return int(gb * DEFAULT_PRICING["storage"]["price_per_gb"])
        return int(gb * pricing.price_per_gb_storage_cents)

    def _calculate_compute_cost(self, pricing: Optional[PricingTier], seconds: int) -> int:
        """Calcula custo de compute"""
        hours = seconds / 3600
        if not pricing:
            return int(hours * DEFAULT_PRICING["compute"]["price_per_hour"])
        return int(hours * pricing.price_per_compute_hour_cents)

    def _calculate_worker_cost(self, pricing: Optional[PricingTier], executions: int) -> int:
        """Calcula custo de execucoes de worker"""
        if not pricing:
            return int(executions * DEFAULT_PRICING["workers"]["price_per_execution"])
        # Workers usam pricing de compute por padrao
        return 0  # Custo ja incluso no compute

    def _check_token_limit(self, tenant_id: str) -> bool:
        """Verifica se tenant excedeu limite de tokens"""
        month = self._get_current_period(AggregationPeriod.MONTHLY.value)

        aggregate = self.db.query(UsageAggregate).filter(
            and_(
                UsageAggregate.tenant_id == tenant_id,
                UsageAggregate.period == month,
                UsageAggregate.period_type == AggregationPeriod.MONTHLY.value
            )
        ).first()

        if not aggregate:
            return False

        total_tokens = aggregate.llm_tokens_input + aggregate.llm_tokens_output

        pricing = self._get_tenant_pricing(tenant_id)
        if not pricing or pricing.included_tokens == 0:
            return False

        return total_tokens >= pricing.included_tokens

    def _calc_percent(self, used: float, limit: int) -> float:
        """Calcula percentual de uso"""
        if not limit or limit == 0:
            return 0.0
        return round((used / limit) * 100, 2)

    # =========================================================================
    # MAINTENANCE
    # =========================================================================

    def cleanup_old_events(self, days: int = 90) -> int:
        """
        Remove eventos antigos para manter o banco leve.

        Args:
            days: Dias para manter

        Returns:
            Numero de eventos removidos
        """
        cutoff_date = datetime.utcnow() - timedelta(days=days)

        count = self.db.query(UsageEvent).filter(
            UsageEvent.timestamp < cutoff_date
        ).delete(synchronize_session=False)

        self.db.commit()
        logger.info(f"Removidos {count} eventos de uso antigos (> {days} dias)")

        return count

    def recalculate_aggregates(
        self,
        tenant_id: str,
        start_month: str,
        end_month: str
    ) -> int:
        """
        Recalcula agregacoes para um range de meses.

        Args:
            tenant_id: ID do tenant
            start_month: Mes inicial (YYYY-MM)
            end_month: Mes final (YYYY-MM)

        Returns:
            Numero de agregacoes recalculadas
        """
        count = 0
        current = datetime.strptime(f"{start_month}-01", "%Y-%m-%d")
        end = datetime.strptime(f"{end_month}-01", "%Y-%m-%d")

        while current <= end:
            month = current.strftime("%Y-%m")
            self.aggregate_monthly(tenant_id, month)
            count += 1

            # Proximo mes
            if current.month == 12:
                current = current.replace(year=current.year + 1, month=1)
            else:
                current = current.replace(month=current.month + 1)

        logger.info(f"Recalculadas {count} agregacoes mensais para tenant {tenant_id}")
        return count


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "MeteringService",
    "AggregationPeriod",
    "MeteringAlert",
    "DEFAULT_PRICING",
]
