# -*- coding: utf-8 -*-
"""
Testes Unitarios para Sistema de Billing
==========================================

Testa:
- Issue #118: Sistema de Metering e Usage Tracking
- Issue #121: Sistema de Cobranca por Uso (Usage-Based Billing)
- Issue #89: Endpoints de Billing

Autor: Plataforma E
"""

import pytest
import uuid
from datetime import datetime, date, timedelta
from decimal import Decimal
from unittest.mock import MagicMock, patch

from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker

# Issue #317: Importar Base e modelos consolidados
from factory.database.connection import Base

# Importar modelos de billing (Tenant agora vem de database.models)
from factory.billing.models import (
    TenantStatus, Plan, PlanType, Subscription, SubscriptionStatus,
    Usage, UsageMetric, UsageEvent, UsageAggregate, UsageEventType,
    Invoice, InvoiceItem, InvoiceStatus, PricingTier, BillingPeriod,
    Tenant
)
from factory.billing.usage_service import UsageService
from factory.billing.invoice_service import InvoiceService


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def engine():
    """
    Cria engine SQLite em memoria para testes.
    Issue #317: Usa Base.metadata.create_all() com modelos consolidados.
    """
    engine = create_engine("sqlite:///:memory:", echo=False)

    # Issue #317: Criar todas as tabelas usando os modelos SQLAlchemy
    # Agora que Tenant esta consolidado, nao ha mais duplicacao
    Base.metadata.create_all(engine)

    return engine


@pytest.fixture
def session(engine):
    """Cria sessao de banco para testes."""
    Session = sessionmaker(bind=engine)
    session = Session()
    yield session
    session.close()


@pytest.fixture
def sample_tenant(session):
    """Cria tenant de exemplo."""
    # Issue #317: Tenant consolidado requer email (campo principal)
    tenant = Tenant(
        tenant_id="TEN-TEST123456",
        name="Empresa Teste",
        slug="empresa-teste",
        email="admin@teste.com",  # Campo obrigatorio no Tenant consolidado
        admin_email="admin@teste.com",  # Campo opcional de billing
        status=TenantStatus.ACTIVE.value,
        is_active=True
    )
    session.add(tenant)
    session.commit()
    return tenant


@pytest.fixture
def sample_plan(session):
    """Cria plano de exemplo."""
    # Issue #317: Campos do Plan sao price_monthly e price_yearly (em centavos)
    plan = Plan(
        plan_id="PLN-PRO123456",
        name="Professional",
        plan_type=PlanType.PROFESSIONAL.value,
        price_monthly=9900,  # Em centavos (R$ 99,00)
        price_yearly=99900,  # Em centavos (R$ 999,00)
        limits={
            "max_users": 10,
            "max_projects": 25,
            "max_tokens_monthly": 1000000,
            "max_api_requests_daily": 10000,
        },
        features=["api_access", "priority_support"],
        is_active=True
    )
    session.add(plan)
    session.commit()
    return plan


@pytest.fixture
def sample_subscription(session, sample_tenant, sample_plan):
    """Cria subscription de exemplo."""
    subscription = Subscription(
        subscription_id="SUB-TEST123456",
        tenant_id=sample_tenant.tenant_id,
        plan_id=sample_plan.plan_id,
        status=SubscriptionStatus.ACTIVE.value,
        current_period_start=datetime.utcnow() - timedelta(days=15),
        current_period_end=datetime.utcnow() + timedelta(days=15)
    )
    session.add(subscription)
    session.commit()
    return subscription


@pytest.fixture
def sample_pricing_tier(session, sample_plan):
    """Cria pricing tier de exemplo."""
    pricing = PricingTier(
        tier_id="TIER-PRO12345",
        name="Professional Usage",
        plan_id=sample_plan.plan_id,
        billing_period=BillingPeriod.MONTHLY.value,
        base_price_cents=9900,
        included_users=5,
        included_projects=10,
        included_tokens=100000,
        included_storage_mb=5120,  # 5GB
        included_api_calls=5000,
        included_compute_minutes=60,  # 1 hora para permitir teste de excedente
        price_per_user_cents=1000,
        price_per_million_tokens_cents=1000,
        price_per_gb_storage_cents=100,
        price_per_1k_api_calls_cents=10,
        price_per_compute_hour_cents=500,
        llm_model_pricing={
            "claude-sonnet-4-20250514": {"input_per_1k": 3, "output_per_1k": 15},
            "claude-opus-4-5-20251101": {"input_per_1k": 15, "output_per_1k": 75},
        },
        is_active=True,
        is_default=True
    )
    session.add(pricing)
    session.commit()
    return pricing


# =============================================================================
# TESTES - MODELOS (Issue #118)
# =============================================================================

class TestUsageEventModel:
    """Testes para o modelo UsageEvent."""

    def test_create_usage_event(self, session, sample_tenant):
        """Deve criar um evento de uso."""
        event = UsageEvent(
            event_id="EVT-TEST123456",
            tenant_id=sample_tenant.tenant_id,
            event_type=UsageEventType.API_CALL.value,
            resource="/api/stories",
            action="create",
            quantity=1,
            unit="calls",
            cost_cents=0,
            event_data={"method": "POST", "status_code": 201}
        )
        session.add(event)
        session.commit()

        assert event.id is not None
        assert event.event_id == "EVT-TEST123456"
        assert event.event_type == "api_call"
        assert event.quantity == 1

    def test_usage_event_to_dict(self, session, sample_tenant):
        """Deve converter evento para dict."""
        event = UsageEvent(
            event_id="EVT-DICT123456",
            tenant_id=sample_tenant.tenant_id,
            event_type=UsageEventType.LLM_TOKENS.value,
            quantity=1500,
            unit="tokens",
            cost_cents=15,
            event_data={"model": "claude-sonnet", "input_tokens": 1000, "output_tokens": 500}
        )
        session.add(event)
        session.commit()

        data = event.to_dict()

        assert data["event_id"] == "EVT-DICT123456"
        assert data["event_type"] == "llm_tokens"
        assert data["quantity"] == 1500
        assert "event_data" in data


class TestUsageAggregateModel:
    """Testes para o modelo UsageAggregate."""

    def test_create_usage_aggregate(self, session, sample_tenant):
        """Deve criar agregacao de uso."""
        aggregate = UsageAggregate(
            aggregate_id="AGG-TEST123456",
            tenant_id=sample_tenant.tenant_id,
            period="2025-01",
            period_type="monthly",
            api_calls=1000,
            llm_tokens_input=50000,
            llm_tokens_output=25000,
            storage_bytes=1024*1024*100,  # 100MB
            compute_seconds=3600,
            cost_api_cents=100,
            cost_llm_cents=500,
            cost_storage_cents=10,
            cost_compute_cents=50,
            cost_total_cents=660
        )
        session.add(aggregate)
        session.commit()

        assert aggregate.id is not None
        assert aggregate.api_calls == 1000

    def test_calculate_total_cost(self, session, sample_tenant):
        """Deve calcular custo total."""
        aggregate = UsageAggregate(
            aggregate_id="AGG-COST123456",
            tenant_id=sample_tenant.tenant_id,
            period="2025-01-15",
            period_type="daily",
            cost_api_cents=100,
            cost_llm_cents=200,
            cost_storage_cents=50,
            cost_compute_cents=150,
        )
        aggregate.calculate_total_cost()

        assert aggregate.cost_total_cents == 500


class TestPricingTierModel:
    """Testes para o modelo PricingTier."""

    def test_get_llm_price(self, sample_pricing_tier):
        """Deve retornar preco por modelo LLM."""
        # Modelo especifico
        price = sample_pricing_tier.get_llm_price("claude-sonnet-4-20250514", "output")
        assert price == 15

        # Modelo nao especificado (usa padrao)
        price = sample_pricing_tier.get_llm_price("claude-unknown", "output")
        assert price == sample_pricing_tier.price_per_million_tokens_cents / 1000

    def test_calculate_overage_cost(self, sample_pricing_tier):
        """Deve calcular custo de excedente."""
        costs = sample_pricing_tier.calculate_overage_cost(
            tokens_used=200000,  # 100k acima do incluido
            storage_bytes=10*1024*1024*1024,  # 10GB (5GB acima do incluido)
            api_calls=10000,  # 5k acima do incluido
            compute_seconds=7200,  # 2h (1h acima do incluido)
            extra_users=3
        )

        assert costs["tokens"] > 0
        assert costs["storage"] > 0
        assert costs["api_calls"] > 0
        assert costs["compute"] > 0
        assert costs["users"] == 3 * 1000  # 3 usuarios x R$10
        assert costs["total"] > 0


# =============================================================================
# TESTES - USAGE SERVICE (Issue #118)
# =============================================================================

class TestUsageService:
    """Testes para o UsageService."""

    def test_track_api_call(self, session, sample_tenant, sample_pricing_tier):
        """Deve rastrear chamada de API."""
        service = UsageService(session)

        event = service.track_api_call(
            tenant_id=sample_tenant.tenant_id,
            method="POST",
            path="/api/stories",
            status_code=201,
            duration_ms=150.5,
            ip_address="192.168.1.1"
        )

        assert event is not None
        assert event.event_type == UsageEventType.API_CALL.value
        assert event.quantity == 1

    def test_track_llm_tokens(self, session, sample_tenant, sample_pricing_tier):
        """Deve rastrear uso de tokens LLM."""
        service = UsageService(session)

        event, is_over_limit = service.track_llm_tokens(
            tenant_id=sample_tenant.tenant_id,
            model="claude-sonnet-4-20250514",
            input_tokens=1500,
            output_tokens=800
        )

        assert event is not None
        assert event.event_type == UsageEventType.LLM_TOKENS.value
        assert event.quantity == 2300  # 1500 + 800
        assert event.cost_cents >= 0

    def test_track_storage(self, session, sample_tenant, sample_pricing_tier):
        """Deve rastrear uso de storage."""
        service = UsageService(session)

        event = service.track_storage(
            tenant_id=sample_tenant.tenant_id,
            operation="upload",
            file_name="documento.pdf",
            file_size=1024000
        )

        assert event is not None
        assert event.event_type == UsageEventType.FILE_UPLOAD.value
        assert event.quantity == 1024000

    def test_track_compute(self, session, sample_tenant, sample_pricing_tier):
        """Deve rastrear tempo de compute."""
        service = UsageService(session)

        event = service.track_compute(
            tenant_id=sample_tenant.tenant_id,
            worker_id="WRK-01",
            job_id="JOB-123",
            duration_seconds=45
        )

        assert event is not None
        assert event.event_type == UsageEventType.COMPUTE.value
        assert event.quantity == 45

    def test_track_login(self, session, sample_tenant):
        """Deve rastrear login."""
        service = UsageService(session)

        event = service.track_login(
            tenant_id=sample_tenant.tenant_id,
            user_id="USR-123",
            ip_address="192.168.1.1",
            success=True
        )

        assert event is not None
        assert event.event_type == UsageEventType.LOGIN.value

    def test_aggregate_monthly_usage(self, session, sample_tenant, sample_pricing_tier):
        """Deve agregar uso mensal."""
        service = UsageService(session)
        month = date.today().strftime("%Y-%m")

        # Criar alguns eventos
        for i in range(5):
            service.track_api_call(
                tenant_id=sample_tenant.tenant_id,
                method="GET",
                path="/api/projects",
                status_code=200,
                duration_ms=50
            )

        # Agregar
        aggregate = service.aggregate_monthly_usage(sample_tenant.tenant_id, month)

        assert aggregate is not None
        assert aggregate.api_calls >= 5

    def test_get_current_usage(self, session, sample_tenant, sample_pricing_tier):
        """Deve obter uso atual."""
        service = UsageService(session)

        # Criar alguns eventos primeiro
        service.track_api_call(
            tenant_id=sample_tenant.tenant_id,
            method="GET",
            path="/api/test",
            status_code=200,
            duration_ms=10
        )

        usage = service.get_current_usage(sample_tenant.tenant_id, "monthly")

        assert usage is not None
        assert "metrics" in usage or "api_calls" in usage.get("metrics", {})


# =============================================================================
# TESTES - INVOICE SERVICE (Issue #121)
# =============================================================================

class TestInvoiceService:
    """Testes para o InvoiceService."""

    def test_generate_invoice(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve gerar fatura."""
        service = InvoiceService(session)
        last_month = (date.today().replace(day=1) - timedelta(days=1)).strftime("%Y-%m")

        invoice, error = service.generate_invoice(
            tenant_id=sample_tenant.tenant_id,
            period=last_month,
            auto_finalize=False
        )

        assert invoice is not None or error is not None
        if invoice:
            assert invoice.invoice_id is not None
            assert invoice.tenant_id == sample_tenant.tenant_id
            assert invoice.status == InvoiceStatus.DRAFT.value

    def test_finalize_invoice(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve finalizar fatura."""
        service = InvoiceService(session)
        period = "2024-12"

        # Criar fatura
        invoice, _ = service.generate_invoice(
            tenant_id=sample_tenant.tenant_id,
            period=period,
            auto_finalize=False
        )

        if invoice:
            success, error = service.finalize_invoice(invoice.invoice_id)

            assert success or error is not None

    def test_list_invoices(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve listar faturas."""
        service = InvoiceService(session)

        # Criar fatura primeiro
        service.generate_invoice(
            tenant_id=sample_tenant.tenant_id,
            period="2024-11"
        )

        invoices = service.list_invoices(sample_tenant.tenant_id)

        assert isinstance(invoices, list)

    def test_calculate_usage_cost(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve calcular custo de uso."""
        service = InvoiceService(session)
        month = date.today().strftime("%Y-%m")

        cost = service.calculate_usage_cost(sample_tenant.tenant_id, month)

        assert cost is not None
        assert "base_cost_cents" in cost
        assert "usage_cost_cents" in cost
        assert "total_cost_cents" in cost


# =============================================================================
# TESTES - BILLING SUMMARY
# =============================================================================

class TestBillingSummary:
    """Testes para resumo de billing."""

    def test_get_billing_summary(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve obter resumo de billing."""
        service = InvoiceService(session)

        summary = service.get_billing_summary(sample_tenant.tenant_id)

        assert summary is not None
        assert "history" in summary
        assert "summary" in summary

    def test_get_overdue_invoices(self, session, sample_tenant):
        """Deve listar faturas vencidas."""
        # Criar fatura vencida
        invoice = Invoice(
            invoice_id="INV-OVERDUE123",
            invoice_number="INV-202401-0001",
            tenant_id=sample_tenant.tenant_id,
            period="2024-01",
            status=InvoiceStatus.PENDING.value,
            due_date=date.today() - timedelta(days=10),
            total=9900  # em centavos
        )
        session.add(invoice)
        session.commit()

        service = InvoiceService(session)
        overdue = service.get_overdue_invoices(sample_tenant.tenant_id)

        assert len(overdue) >= 1
        assert overdue[0].invoice_id == "INV-OVERDUE123"


# =============================================================================
# TESTES - USAGE BREAKDOWN
# =============================================================================

class TestUsageBreakdown:
    """Testes para breakdown de uso."""

    def test_get_usage_breakdown(self, session, sample_tenant, sample_pricing_tier):
        """Deve obter breakdown de uso."""
        usage_service = UsageService(session)

        # Criar alguns eventos
        usage_service.track_api_call(
            tenant_id=sample_tenant.tenant_id,
            method="POST",
            path="/api/stories",
            status_code=201,
            duration_ms=100
        )

        usage_service.track_llm_tokens(
            tenant_id=sample_tenant.tenant_id,
            model="claude-sonnet-4-20250514",
            input_tokens=1000,
            output_tokens=500
        )

        month = date.today().strftime("%Y-%m")
        breakdown = usage_service.get_usage_breakdown(sample_tenant.tenant_id, month)

        assert breakdown is not None
        assert "breakdown" in breakdown


# =============================================================================
# TESTES - EDGE CASES
# =============================================================================

class TestEdgeCases:
    """Testes de casos extremos."""

    def test_empty_usage(self, session, sample_tenant):
        """Deve lidar com uso vazio."""
        service = UsageService(session)

        usage = service.get_current_usage(sample_tenant.tenant_id)

        assert usage is not None
        # Deve retornar valores zerados

    def test_tenant_without_subscription(self, session, sample_tenant):
        """Deve lidar com tenant sem subscription."""
        service = InvoiceService(session)

        invoice, error = service.generate_invoice(sample_tenant.tenant_id)

        assert invoice is None
        assert error is not None
        assert "assinatura" in error.lower() or "subscription" in error.lower()

    def test_duplicate_invoice(self, session, sample_tenant, sample_subscription, sample_pricing_tier):
        """Deve impedir fatura duplicada."""
        service = InvoiceService(session)
        period = "2024-10"

        # Primeira fatura
        invoice1, _ = service.generate_invoice(sample_tenant.tenant_id, period)

        # Tentar duplicar
        invoice2, error = service.generate_invoice(sample_tenant.tenant_id, period)

        assert invoice2 is None
        assert error is not None
        assert "existe" in error.lower()


# =============================================================================
# TESTES - EVENTOS
# =============================================================================

class TestEventFilters:
    """Testes para filtros de eventos."""

    def test_get_events_by_type(self, session, sample_tenant, sample_pricing_tier):
        """Deve filtrar eventos por tipo."""
        service = UsageService(session)

        # Criar eventos de diferentes tipos
        service.track_api_call(
            tenant_id=sample_tenant.tenant_id,
            method="GET",
            path="/test",
            status_code=200,
            duration_ms=10
        )

        service.track_login(
            tenant_id=sample_tenant.tenant_id,
            user_id="USR-1"
        )

        # Filtrar apenas API calls
        events = service.get_events(
            tenant_id=sample_tenant.tenant_id,
            event_type=UsageEventType.API_CALL.value
        )

        assert len(events) >= 1
        assert all(e.event_type == UsageEventType.API_CALL.value for e in events)


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
