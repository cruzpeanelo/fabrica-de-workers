# -*- coding: utf-8 -*-
"""
Modelos de Dados para Multi-Tenancy e Billing
==============================================

Implementa os modelos SQLAlchemy para:
- Tenant: Organizacao/empresa cliente
- Plan: Planos de assinatura
- Subscription: Assinaturas ativas
- Usage: Metricas de uso
- Invoice: Faturas
- PaymentMethod: Metodos de pagamento

Autor: Fabrica de Agentes
"""

from sqlalchemy import (
    Column, Integer, String, Text, DateTime, JSON, ForeignKey,
    Boolean, Float, Numeric, Date, Index, UniqueConstraint
)
from sqlalchemy.orm import relationship
from datetime import datetime, date
from decimal import Decimal
from enum import Enum
from typing import Optional, List, Dict, Any

# Import Base e Tenant consolidado do projeto (Issue #317)
try:
    from ..database.connection import Base
    from ..database.models import Tenant, TenantStatus as BaseTenantStatus
except ImportError:
    from factory.database.connection import Base
    from factory.database.models import Tenant, TenantStatus as BaseTenantStatus


# =============================================================================
# ENUMS
# =============================================================================

# Issue #317: TenantStatus importado de database.models como BaseTenantStatus
# Alias para compatibilidade retroativa
TenantStatus = BaseTenantStatus


class PlanType(str, Enum):
    """Tipos de Plano"""
    STARTER = "starter"
    PROFESSIONAL = "professional"
    ENTERPRISE = "enterprise"
    CUSTOM = "custom"


class SubscriptionStatus(str, Enum):
    """Status da Assinatura"""
    ACTIVE = "active"
    TRIALING = "trialing"
    PAST_DUE = "past_due"
    CANCELED = "canceled"
    UNPAID = "unpaid"
    INCOMPLETE = "incomplete"


class UsageMetric(str, Enum):
    """Metricas de Uso Rastreadas"""
    LLM_TOKENS = "llm_tokens"
    LLM_REQUESTS = "llm_requests"
    PROJECTS_CREATED = "projects_created"
    STORIES_CREATED = "stories_created"
    TASKS_COMPLETED = "tasks_completed"
    JOBS_EXECUTED = "jobs_executed"
    STORAGE_BYTES = "storage_bytes"
    API_REQUESTS = "api_requests"
    AGENTS_ACTIVE = "agents_active"
    USERS_ACTIVE = "users_active"


class InvoiceStatus(str, Enum):
    """Status da Fatura"""
    DRAFT = "draft"
    OPEN = "open"
    PAID = "paid"
    VOID = "void"
    UNCOLLECTIBLE = "uncollectible"


class PaymentMethodType(str, Enum):
    """Tipos de Metodo de Pagamento"""
    CARD = "card"
    BOLETO = "boleto"
    PIX = "pix"
    BANK_TRANSFER = "bank_transfer"


class UsageEventType(str, Enum):
    """Tipos de Evento de Uso (Issue #118)"""
    API_CALL = "api_call"
    LLM_TOKENS = "llm_tokens"
    STORAGE = "storage"
    COMPUTE = "compute"
    LOGIN = "login"
    SESSION = "session"
    WEBHOOK = "webhook"
    FILE_UPLOAD = "file_upload"
    FILE_DOWNLOAD = "file_download"


class BillingPeriod(str, Enum):
    """Periodos de Cobranca"""
    MONTHLY = "monthly"
    YEARLY = "yearly"
    CUSTOM = "custom"


# =============================================================================
# TENANT - Issue #317: Consolidado em factory/database/models.py
# =============================================================================
# O modelo Tenant agora é importado de factory.database.models para evitar
# duplicação de tabelas e conflitos de MetaData no SQLAlchemy.
#
# Campos de billing adicionados ao Tenant principal:
# - legal_name, document, admin_email, admin_name, stripe_customer_id
# - trial_started_at, custom_limits
# - Relacionamentos: subscriptions, usages, invoices, payment_methods
# =============================================================================


# =============================================================================
# PLAN - Planos de Assinatura
# =============================================================================

class Plan(Base):
    """
    Modelo para Planos de Assinatura

    Planos padrao:
    - Starter (R$ 299/mes): 3 usuarios, 5 projetos, 10k tokens
    - Professional (R$ 999/mes): 10 usuarios, 20 projetos, 100k tokens
    - Enterprise (sob consulta): Ilimitado
    """
    __tablename__ = "plans"

    id = Column(Integer, primary_key=True, autoincrement=True)
    plan_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(100), nullable=False)
    plan_type = Column(String(30), default=PlanType.STARTER.value, index=True)
    description = Column(Text, nullable=True)

    # Precos (em centavos para evitar problemas de float)
    price_monthly = Column(Integer, nullable=False)  # em centavos (29900 = R$ 299,00)
    price_yearly = Column(Integer, nullable=True)    # em centavos (com desconto anual)
    currency = Column(String(3), default="BRL")

    # Stripe Price IDs
    stripe_price_monthly_id = Column(String(100), nullable=True)
    stripe_price_yearly_id = Column(String(100), nullable=True)
    stripe_product_id = Column(String(100), nullable=True)

    # Limites do Plano
    limits = Column(JSON, nullable=False, default=dict)
    """
    Exemplo de limits:
    {
        "max_users": 3,
        "max_projects": 5,
        "max_stories": 100,
        "max_agents": 2,
        "max_tokens_monthly": 10000,
        "max_storage_gb": 1,
        "max_api_requests_daily": 1000
    }
    """

    # Features incluidas
    features = Column(JSON, default=list)
    """
    Exemplo de features:
    [
        "kanban_board",
        "basic_reports",
        "email_support"
    ]
    """

    # Features excluidas (para UI)
    excluded_features = Column(JSON, default=list)

    # Billing
    billing_period = Column(String(20), default="monthly")  # monthly, yearly
    trial_days = Column(Integer, default=14)

    # Status
    is_active = Column(Boolean, default=True, index=True)
    is_public = Column(Boolean, default=True)  # Se aparece na pagina de precos
    is_default = Column(Boolean, default=False)  # Plano padrao para novos tenants

    # Ordem de exibicao
    display_order = Column(Integer, default=0)

    # Metadados extras (nao usar 'metadata' - reservado pelo SQLAlchemy)
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relacionamentos
    subscriptions = relationship("Subscription", back_populates="plan")

    def to_dict(self) -> Dict[str, Any]:
        return {
            "plan_id": self.plan_id,
            "name": self.name,
            "plan_type": self.plan_type,
            "description": self.description,
            "price_monthly": self.price_monthly,
            "price_monthly_formatted": f"R$ {self.price_monthly / 100:.2f}",
            "price_yearly": self.price_yearly,
            "price_yearly_formatted": f"R$ {self.price_yearly / 100:.2f}" if self.price_yearly else None,
            "currency": self.currency,
            "limits": self.limits or {},
            "features": self.features or [],
            "excluded_features": self.excluded_features or [],
            "trial_days": self.trial_days,
            "is_active": self.is_active,
            "is_public": self.is_public,
            "display_order": self.display_order,
        }

    def get_limit(self, limit_name: str, default: int = 0) -> int:
        """Retorna um limite especifico do plano"""
        return (self.limits or {}).get(limit_name, default)

    def has_feature(self, feature_name: str) -> bool:
        """Verifica se o plano inclui uma feature"""
        return feature_name in (self.features or [])

    def __repr__(self):
        return f"<Plan {self.plan_id}: {self.name} R${self.price_monthly/100:.2f}/mes>"


# =============================================================================
# SUBSCRIPTION - Assinaturas
# =============================================================================

class Subscription(Base):
    """
    Modelo para Assinaturas
    Relaciona um Tenant a um Plano com periodo de cobranca.
    """
    __tablename__ = "subscriptions"

    id = Column(Integer, primary_key=True, autoincrement=True)
    subscription_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamentos
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant")  # Issue #317: sem back_populates para evitar imports circulares

    plan_id = Column(String(50), ForeignKey("plans.plan_id"), nullable=False, index=True)
    plan = relationship("Plan", back_populates="subscriptions")

    # Status
    status = Column(String(20), default=SubscriptionStatus.ACTIVE.value, index=True)

    # Stripe
    stripe_subscription_id = Column(String(100), nullable=True, unique=True, index=True)
    stripe_price_id = Column(String(100), nullable=True)

    # Periodo de Billing
    billing_period = Column(String(20), default="monthly")  # monthly, yearly
    billing_anchor_day = Column(Integer, default=1)  # Dia do mes para cobranca

    # Periodo Atual
    current_period_start = Column(DateTime, nullable=True)
    current_period_end = Column(DateTime, nullable=True)

    # Trial
    trial_start = Column(DateTime, nullable=True)
    trial_end = Column(DateTime, nullable=True)

    # Cancelamento
    cancel_at_period_end = Column(Boolean, default=False)
    canceled_at = Column(DateTime, nullable=True)
    cancellation_reason = Column(Text, nullable=True)

    # Quantidade (para planos por usuario)
    quantity = Column(Integer, default=1)

    # Desconto
    discount_percent = Column(Float, default=0.0)
    discount_amount = Column(Integer, default=0)  # em centavos
    coupon_code = Column(String(50), nullable=True)

    # Metadados extras (nao usar 'metadata' - reservado pelo SQLAlchemy)
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    ended_at = Column(DateTime, nullable=True)

    # Indices
    __table_args__ = (
        Index('ix_subscription_tenant_status', 'tenant_id', 'status'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "subscription_id": self.subscription_id,
            "tenant_id": self.tenant_id,
            "plan_id": self.plan_id,
            "status": self.status,
            "billing_period": self.billing_period,
            "current_period_start": self.current_period_start.isoformat() if self.current_period_start else None,
            "current_period_end": self.current_period_end.isoformat() if self.current_period_end else None,
            "trial_start": self.trial_start.isoformat() if self.trial_start else None,
            "trial_end": self.trial_end.isoformat() if self.trial_end else None,
            "cancel_at_period_end": self.cancel_at_period_end,
            "canceled_at": self.canceled_at.isoformat() if self.canceled_at else None,
            "quantity": self.quantity,
            "discount_percent": self.discount_percent,
            "discount_amount": self.discount_amount,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def is_active(self) -> bool:
        """Verifica se a assinatura esta ativa"""
        return self.status in [
            SubscriptionStatus.ACTIVE.value,
            SubscriptionStatus.TRIALING.value
        ]

    def is_in_trial(self) -> bool:
        """Verifica se esta no periodo de trial"""
        if self.status != SubscriptionStatus.TRIALING.value:
            return False
        if not self.trial_end:
            return False
        return datetime.utcnow() < self.trial_end

    def days_until_renewal(self) -> int:
        """Dias ate a proxima renovacao"""
        if not self.current_period_end:
            return 0
        delta = self.current_period_end - datetime.utcnow()
        return max(0, delta.days)

    def __repr__(self):
        return f"<Subscription {self.subscription_id}: {self.tenant_id} -> {self.plan_id} [{self.status}]>"


# =============================================================================
# USAGE - Metricas de Uso
# =============================================================================

class Usage(Base):
    """
    Modelo para Metricas de Uso
    Rastreia o consumo de recursos por tenant/periodo.

    Metricas rastreadas:
    - LLM tokens consumidos
    - Projetos criados
    - Stories criadas
    - Jobs executados
    - Armazenamento usado
    - Requisicoes de API
    """
    __tablename__ = "usages"

    id = Column(Integer, primary_key=True, autoincrement=True)
    usage_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant")  # Issue #317: sem back_populates para evitar imports circulares

    # Periodo (YYYY-MM-DD ou YYYY-MM para agregacao mensal)
    period = Column(Date, nullable=False, index=True)
    period_type = Column(String(10), default="daily")  # daily, monthly

    # Metrica
    metric = Column(String(50), nullable=False, index=True)

    # Valor
    value = Column(Integer, nullable=False, default=0)

    # Limites (snapshot do momento)
    limit_value = Column(Integer, nullable=True)  # Limite do plano no momento

    # Contexto adicional
    context = Column(JSON, default=dict)
    """
    Exemplo:
    {
        "project_id": "PROJ-001",
        "job_id": "JOB-123",
        "model": "claude-sonnet-4-20250514"
    }
    """

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Indices compostos
    __table_args__ = (
        Index('ix_usage_tenant_period_metric', 'tenant_id', 'period', 'metric'),
        UniqueConstraint('tenant_id', 'period', 'metric', 'period_type', name='uq_usage_tenant_period_metric'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "usage_id": self.usage_id,
            "tenant_id": self.tenant_id,
            "period": self.period.isoformat() if self.period else None,
            "period_type": self.period_type,
            "metric": self.metric,
            "value": self.value,
            "limit_value": self.limit_value,
            "usage_percent": round((self.value / self.limit_value) * 100, 2) if self.limit_value else None,
            "context": self.context or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def is_over_limit(self) -> bool:
        """Verifica se esta acima do limite"""
        if not self.limit_value:
            return False
        return self.value >= self.limit_value

    def usage_percentage(self) -> float:
        """Retorna percentual de uso"""
        if not self.limit_value or self.limit_value == 0:
            return 0.0
        return min(100.0, (self.value / self.limit_value) * 100)

    def __repr__(self):
        return f"<Usage {self.tenant_id}: {self.metric}={self.value} ({self.period})>"


# =============================================================================
# INVOICE - Faturas
# =============================================================================

class Invoice(Base):
    """
    Modelo para Faturas
    Representa cobranças efetuadas ou pendentes.
    """
    __tablename__ = "invoices"

    id = Column(Integer, primary_key=True, autoincrement=True)
    invoice_id = Column(String(50), unique=True, nullable=False, index=True)
    invoice_number = Column(String(50), nullable=True)  # Numero fiscal

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant")  # Issue #317: sem back_populates para evitar imports circulares

    subscription_id = Column(String(50), ForeignKey("subscriptions.subscription_id"), nullable=True)

    # Stripe
    stripe_invoice_id = Column(String(100), nullable=True, unique=True, index=True)
    stripe_payment_intent_id = Column(String(100), nullable=True)

    # Status
    status = Column(String(20), default=InvoiceStatus.DRAFT.value, index=True)

    # Valores (em centavos)
    subtotal = Column(Integer, nullable=False, default=0)
    discount = Column(Integer, default=0)
    tax = Column(Integer, default=0)
    total = Column(Integer, nullable=False, default=0)
    amount_paid = Column(Integer, default=0)
    amount_due = Column(Integer, default=0)
    currency = Column(String(3), default="BRL")

    # Itens da fatura
    line_items = Column(JSON, default=list)
    """
    Exemplo:
    [
        {
            "description": "Plano Professional - Janeiro 2025",
            "quantity": 1,
            "unit_amount": 99900,
            "amount": 99900
        },
        {
            "description": "Tokens LLM adicionais (50k)",
            "quantity": 5,
            "unit_amount": 1000,
            "amount": 5000
        }
    ]
    """

    # Periodo de referencia
    period_start = Column(DateTime, nullable=True)
    period_end = Column(DateTime, nullable=True)

    # Datas
    issue_date = Column(DateTime, nullable=True)
    due_date = Column(DateTime, nullable=True)
    paid_at = Column(DateTime, nullable=True)
    voided_at = Column(DateTime, nullable=True)

    # Metodo de pagamento usado
    payment_method_id = Column(String(50), nullable=True)
    payment_method_type = Column(String(20), nullable=True)

    # PDF/URLs
    invoice_pdf_url = Column(String(500), nullable=True)
    hosted_invoice_url = Column(String(500), nullable=True)

    # Notas
    notes = Column(Text, nullable=True)

    # Metadados extras (nao usar 'metadata' - reservado pelo SQLAlchemy)
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Indices
    __table_args__ = (
        Index('ix_invoice_tenant_status', 'tenant_id', 'status'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "invoice_id": self.invoice_id,
            "invoice_number": self.invoice_number,
            "tenant_id": self.tenant_id,
            "subscription_id": self.subscription_id,
            "status": self.status,
            "subtotal": self.subtotal,
            "subtotal_formatted": f"R$ {self.subtotal / 100:.2f}",
            "discount": self.discount,
            "tax": self.tax,
            "total": self.total,
            "total_formatted": f"R$ {self.total / 100:.2f}",
            "amount_paid": self.amount_paid,
            "amount_due": self.amount_due,
            "currency": self.currency,
            "line_items": self.line_items or [],
            "period_start": self.period_start.isoformat() if self.period_start else None,
            "period_end": self.period_end.isoformat() if self.period_end else None,
            "issue_date": self.issue_date.isoformat() if self.issue_date else None,
            "due_date": self.due_date.isoformat() if self.due_date else None,
            "paid_at": self.paid_at.isoformat() if self.paid_at else None,
            "invoice_pdf_url": self.invoice_pdf_url,
            "hosted_invoice_url": self.hosted_invoice_url,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def is_paid(self) -> bool:
        """Verifica se a fatura foi paga"""
        return self.status == InvoiceStatus.PAID.value

    def is_overdue(self) -> bool:
        """Verifica se a fatura esta vencida"""
        if self.status != InvoiceStatus.OPEN.value:
            return False
        if not self.due_date:
            return False
        return datetime.utcnow() > self.due_date

    def __repr__(self):
        return f"<Invoice {self.invoice_id}: R${self.total/100:.2f} [{self.status}]>"


# =============================================================================
# INVOICE_ITEM - Itens de Fatura (Issue #317)
# =============================================================================

class InvoiceItem(Base):
    """
    Modelo para Itens de Fatura - Issue #317
    Representa itens individuais dentro de uma fatura.
    """
    __tablename__ = "invoice_items"

    id = Column(Integer, primary_key=True, autoincrement=True)
    item_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com Invoice
    invoice_id = Column(String(50), ForeignKey("invoices.invoice_id"), nullable=False, index=True)
    invoice = relationship("Invoice", backref="items")

    # Descricao do item
    description = Column(String(500), nullable=False)
    item_type = Column(String(50), default="usage")  # usage, subscription, discount, tax

    # Valores
    quantity = Column(Integer, default=1)
    unit_price_cents = Column(Integer, default=0)  # Preco unitario em centavos
    amount_cents = Column(Integer, default=0)  # Total do item em centavos
    discount_cents = Column(Integer, default=0)

    # Metadados extras
    metadata_json = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    # Indices
    __table_args__ = (
        Index('ix_invoice_item_invoice', 'invoice_id'),
    )

    def __init__(self, **kwargs):
        # Suporte para 'metadata' como alias
        if 'metadata' in kwargs:
            kwargs['metadata_json'] = kwargs.pop('metadata')
        super().__init__(**kwargs)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "item_id": self.item_id,
            "invoice_id": self.invoice_id,
            "description": self.description,
            "item_type": self.item_type,
            "quantity": self.quantity,
            "unit_price_cents": self.unit_price_cents,
            "unit_price_formatted": f"R$ {self.unit_price_cents / 100:.2f}",
            "amount_cents": self.amount_cents,
            "amount_formatted": f"R$ {self.amount_cents / 100:.2f}",
            "discount_cents": self.discount_cents,
            "metadata": self.metadata_json or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def __repr__(self):
        return f"<InvoiceItem {self.item_id}: {self.description} R${self.amount_cents/100:.2f}>"


# =============================================================================
# PAYMENT_METHOD - Metodos de Pagamento
# =============================================================================

class PaymentMethod(Base):
    """
    Modelo para Metodos de Pagamento
    Armazena informacoes sobre cartoes, boletos, etc.
    """
    __tablename__ = "payment_methods"

    id = Column(Integer, primary_key=True, autoincrement=True)
    payment_method_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant")  # Issue #317: sem back_populates para evitar imports circulares

    # Stripe
    stripe_payment_method_id = Column(String(100), nullable=True, unique=True)

    # Tipo
    method_type = Column(String(20), nullable=False, index=True)  # card, boleto, pix

    # Status
    is_default = Column(Boolean, default=False)
    is_active = Column(Boolean, default=True)

    # Dados do Cartao (mascarados)
    card_brand = Column(String(20), nullable=True)  # visa, mastercard, etc
    card_last_four = Column(String(4), nullable=True)
    card_exp_month = Column(Integer, nullable=True)
    card_exp_year = Column(Integer, nullable=True)

    # Titular
    billing_name = Column(String(200), nullable=True)
    billing_email = Column(String(255), nullable=True)
    billing_address = Column(JSON, default=dict)

    # Metadados extras (nao usar 'metadata' - reservado pelo SQLAlchemy)
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        result = {
            "payment_method_id": self.payment_method_id,
            "tenant_id": self.tenant_id,
            "method_type": self.method_type,
            "is_default": self.is_default,
            "is_active": self.is_active,
            "billing_name": self.billing_name,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

        if self.method_type == PaymentMethodType.CARD.value:
            result.update({
                "card_brand": self.card_brand,
                "card_last_four": self.card_last_four,
                "card_display": f"{self.card_brand} **** {self.card_last_four}" if self.card_brand else None,
                "card_exp": f"{self.card_exp_month:02d}/{self.card_exp_year}" if self.card_exp_month else None,
            })

        return result

    def is_card_expired(self) -> bool:
        """Verifica se o cartao esta expirado"""
        if self.method_type != PaymentMethodType.CARD.value:
            return False
        if not self.card_exp_month or not self.card_exp_year:
            return False
        now = datetime.utcnow()
        return (self.card_exp_year < now.year or
                (self.card_exp_year == now.year and self.card_exp_month < now.month))

    def __repr__(self):
        if self.method_type == PaymentMethodType.CARD.value:
            return f"<PaymentMethod {self.payment_method_id}: {self.card_brand} ****{self.card_last_four}>"
        return f"<PaymentMethod {self.payment_method_id}: {self.method_type}>"


# =============================================================================
# USAGE_EVENT - Eventos de Uso Detalhados (Issue #118)
# =============================================================================

class UsageEvent(Base):
    """
    Modelo para Eventos de Uso Granulares
    Rastreia cada evento individual de uso (API calls, tokens, storage, compute)

    Implementa Issue #118 - Sistema de Metering e Usage Tracking Completo
    """
    __tablename__ = "usage_events"

    id = Column(Integer, primary_key=True, autoincrement=True)
    event_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-tenant
    tenant_id = Column(String(50), nullable=False, index=True)
    user_id = Column(String(50), nullable=True, index=True)
    project_id = Column(String(50), nullable=True, index=True)

    # Tipo e recurso
    event_type = Column(String(30), nullable=False, index=True)  # api_call, llm_tokens, storage, compute
    resource = Column(String(200), nullable=True)  # /api/stories, claude-sonnet, uploads
    action = Column(String(30), nullable=True)  # create, read, update, delete

    # Quantidades
    quantity = Column(Integer, nullable=False, default=1)
    unit = Column(String(30), nullable=True)  # calls, tokens, bytes, seconds

    # Custo calculado (em centavos)
    cost_cents = Column(Integer, default=0)

    # Metadados detalhados (nao usar 'metadata' - reservado pelo SQLAlchemy)
    event_data = Column(JSON, default=dict)
    """
    Exemplos de event_data por tipo:

    api_call: {
        "method": "POST",
        "path": "/api/stories",
        "status_code": 201,
        "duration_ms": 150,
        "user_agent": "..."
    }

    llm_tokens: {
        "model": "claude-sonnet-4-20250514",
        "input_tokens": 1500,
        "output_tokens": 800,
        "job_id": "JOB-123"
    }

    storage: {
        "file_name": "documento.pdf",
        "file_size": 1024000,
        "operation": "upload"
    }

    compute: {
        "worker_id": "WRK-01",
        "job_id": "JOB-123",
        "duration_seconds": 45
    }
    """

    # IP e contexto de request
    ip_address = Column(String(50), nullable=True)
    user_agent = Column(String(500), nullable=True)
    correlation_id = Column(String(50), nullable=True, index=True)

    # Timestamp
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)

    # Indices compostos
    __table_args__ = (
        Index('ix_usage_event_tenant_type_time', 'tenant_id', 'event_type', 'timestamp'),
        Index('ix_usage_event_tenant_user_time', 'tenant_id', 'user_id', 'timestamp'),
        Index('ix_usage_event_project_time', 'project_id', 'timestamp'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "event_id": self.event_id,
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "project_id": self.project_id,
            "event_type": self.event_type,
            "resource": self.resource,
            "action": self.action,
            "quantity": self.quantity,
            "unit": self.unit,
            "cost_cents": self.cost_cents,
            "cost_formatted": f"R$ {self.cost_cents / 100:.4f}" if self.cost_cents else None,
            "event_data": self.event_data or {},
            "ip_address": self.ip_address,
            "correlation_id": self.correlation_id,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None,
        }

    def __repr__(self):
        return f"<UsageEvent {self.event_id}: {self.event_type} {self.quantity} {self.unit}>"


# =============================================================================
# USAGE_AGGREGATE - Agregacao de Uso por Periodo (Issue #118)
# =============================================================================

class UsageAggregate(Base):
    """
    Modelo para Agregacao de Uso por Periodo
    Consolida uso diario/mensal para billing e reports

    Implementa Issue #118 - Sistema de Metering
    """
    __tablename__ = "usage_aggregates"

    id = Column(Integer, primary_key=True, autoincrement=True)
    aggregate_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-tenant
    tenant_id = Column(String(50), nullable=False, index=True)

    # Periodo
    period = Column(String(20), nullable=False, index=True)  # 2025-01, 2025-01-15
    period_type = Column(String(10), nullable=False, default="daily")  # daily, monthly

    # Metricas agregadas
    api_calls = Column(Integer, default=0)
    llm_tokens_input = Column(Integer, default=0)
    llm_tokens_output = Column(Integer, default=0)
    storage_bytes = Column(Integer, default=0)  # BigInteger para storage grande
    compute_seconds = Column(Integer, default=0)

    # Contadores de sessao/usuario
    active_users = Column(Integer, default=0)
    active_sessions = Column(Integer, default=0)
    logins_count = Column(Integer, default=0)

    # Contadores de arquivos
    file_uploads = Column(Integer, default=0)
    file_downloads = Column(Integer, default=0)

    # Custo total do periodo (em centavos, precisao 4 casas decimais)
    cost_api_cents = Column(Integer, default=0)
    cost_llm_cents = Column(Integer, default=0)
    cost_storage_cents = Column(Integer, default=0)
    cost_compute_cents = Column(Integer, default=0)
    cost_total_cents = Column(Integer, default=0)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Indices compostos
    __table_args__ = (
        Index('ix_usage_aggregate_tenant_period', 'tenant_id', 'period', 'period_type'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "aggregate_id": self.aggregate_id,
            "tenant_id": self.tenant_id,
            "period": self.period,
            "period_type": self.period_type,
            "metrics": {
                "api_calls": self.api_calls,
                "llm_tokens_input": self.llm_tokens_input,
                "llm_tokens_output": self.llm_tokens_output,
                "llm_tokens_total": self.llm_tokens_input + self.llm_tokens_output,
                "storage_bytes": self.storage_bytes,
                "storage_mb": round(self.storage_bytes / (1024 * 1024), 2),
                "compute_seconds": self.compute_seconds,
                "compute_minutes": round(self.compute_seconds / 60, 2),
                "active_users": self.active_users,
                "active_sessions": self.active_sessions,
                "logins_count": self.logins_count,
                "file_uploads": self.file_uploads,
                "file_downloads": self.file_downloads,
            },
            "costs": {
                "api_cents": self.cost_api_cents,
                "llm_cents": self.cost_llm_cents,
                "storage_cents": self.cost_storage_cents,
                "compute_cents": self.cost_compute_cents,
                "total_cents": self.cost_total_cents,
                "total_formatted": f"R$ {self.cost_total_cents / 100:.2f}",
            },
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
        }

    def calculate_total_cost(self):
        """Recalcula custo total baseado nas partes"""
        self.cost_total_cents = (
            self.cost_api_cents +
            self.cost_llm_cents +
            self.cost_storage_cents +
            self.cost_compute_cents
        )

    def __repr__(self):
        return f"<UsageAggregate {self.tenant_id}: {self.period} R${self.cost_total_cents/100:.2f}>"


# =============================================================================
# PRICING_TIER - Precos por Uso (Issue #121)
# =============================================================================

class PricingTier(Base):
    """
    Modelo para Tiers de Precificacao por Uso
    Define precos unitarios para recursos alem do incluido no plano

    Implementa Issue #121 - Sistema de Cobranca por Uso
    """
    __tablename__ = "pricing_tiers"

    id = Column(Integer, primary_key=True, autoincrement=True)
    tier_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(100), nullable=False)
    description = Column(Text, nullable=True)

    # Plano base associado (opcional - para tiers especificos de plano)
    plan_id = Column(String(50), ForeignKey("plans.plan_id"), nullable=True, index=True)

    # Periodo de cobranca
    billing_period = Column(String(20), default=BillingPeriod.MONTHLY.value)

    # Preco base do tier (em centavos)
    base_price_cents = Column(Integer, default=0)

    # Inclusoes no preco base
    included_users = Column(Integer, default=1)
    included_projects = Column(Integer, default=1)
    included_tokens = Column(Integer, default=10000)
    included_storage_mb = Column(Integer, default=1024)  # 1GB
    included_api_calls = Column(Integer, default=1000)
    included_compute_minutes = Column(Integer, default=60)

    # Precos unitarios excedentes (em centavos)
    price_per_user_cents = Column(Integer, default=0)  # Por usuario adicional
    price_per_million_tokens_cents = Column(Integer, default=0)  # Por milhao de tokens
    price_per_gb_storage_cents = Column(Integer, default=0)  # Por GB adicional
    price_per_1k_api_calls_cents = Column(Integer, default=0)  # Por 1000 chamadas
    price_per_compute_hour_cents = Column(Integer, default=0)  # Por hora de compute

    # Precos por modelo LLM especifico (JSON)
    llm_model_pricing = Column(JSON, default=dict)
    """
    Exemplo:
    {
        "claude-sonnet-4-20250514": {"input_per_1k": 3, "output_per_1k": 15},
        "claude-opus-4-5-20251101": {"input_per_1k": 15, "output_per_1k": 75},
        "claude-3-haiku": {"input_per_1k": 0.25, "output_per_1k": 1.25}
    }
    """

    # Limites maximos (0 = ilimitado)
    max_users = Column(Integer, default=0)
    max_tokens_monthly = Column(Integer, default=0)
    max_storage_gb = Column(Integer, default=0)
    max_api_calls_daily = Column(Integer, default=0)

    # Status
    is_active = Column(Boolean, default=True, index=True)
    is_default = Column(Boolean, default=False)

    # Validade (para promocoes)
    valid_from = Column(DateTime, nullable=True)
    valid_until = Column(DateTime, nullable=True)

    # Metadados
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tier_id": self.tier_id,
            "name": self.name,
            "description": self.description,
            "plan_id": self.plan_id,
            "billing_period": self.billing_period,
            "base_price_cents": self.base_price_cents,
            "base_price_formatted": f"R$ {self.base_price_cents / 100:.2f}",
            "included": {
                "users": self.included_users,
                "projects": self.included_projects,
                "tokens": self.included_tokens,
                "storage_mb": self.included_storage_mb,
                "api_calls": self.included_api_calls,
                "compute_minutes": self.included_compute_minutes,
            },
            "overage_pricing": {
                "per_user_cents": self.price_per_user_cents,
                "per_million_tokens_cents": self.price_per_million_tokens_cents,
                "per_gb_storage_cents": self.price_per_gb_storage_cents,
                "per_1k_api_calls_cents": self.price_per_1k_api_calls_cents,
                "per_compute_hour_cents": self.price_per_compute_hour_cents,
            },
            "llm_model_pricing": self.llm_model_pricing or {},
            "limits": {
                "max_users": self.max_users,
                "max_tokens_monthly": self.max_tokens_monthly,
                "max_storage_gb": self.max_storage_gb,
                "max_api_calls_daily": self.max_api_calls_daily,
            },
            "is_active": self.is_active,
            "is_default": self.is_default,
            "valid_from": self.valid_from.isoformat() if self.valid_from else None,
            "valid_until": self.valid_until.isoformat() if self.valid_until else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def get_llm_price(self, model: str, token_type: str = "output") -> float:
        """
        Retorna preco por 1K tokens para um modelo especifico

        Args:
            model: Nome do modelo (ex: claude-sonnet-4-20250514)
            token_type: "input" ou "output"

        Returns:
            Preco em centavos por 1K tokens
        """
        if not self.llm_model_pricing:
            # Preco padrao
            return self.price_per_million_tokens_cents / 1000

        model_pricing = self.llm_model_pricing.get(model, {})
        key = f"{token_type}_per_1k"
        return model_pricing.get(key, self.price_per_million_tokens_cents / 1000)

    def calculate_overage_cost(
        self,
        tokens_used: int = 0,
        storage_bytes: int = 0,
        api_calls: int = 0,
        compute_seconds: int = 0,
        extra_users: int = 0
    ) -> Dict[str, int]:
        """
        Calcula custo de excedente

        Returns:
            Dict com custos por categoria em centavos
        """
        costs = {
            "tokens": 0,
            "storage": 0,
            "api_calls": 0,
            "compute": 0,
            "users": 0,
            "total": 0
        }

        # Tokens excedentes
        tokens_over = max(0, tokens_used - self.included_tokens)
        if tokens_over > 0:
            costs["tokens"] = int(tokens_over * self.price_per_million_tokens_cents / 1_000_000)

        # Storage excedente (converter bytes para MB)
        storage_mb = storage_bytes / (1024 * 1024)
        storage_over = max(0, storage_mb - self.included_storage_mb)
        if storage_over > 0:
            storage_gb = storage_over / 1024
            costs["storage"] = int(storage_gb * self.price_per_gb_storage_cents)

        # API calls excedentes
        api_over = max(0, api_calls - self.included_api_calls)
        if api_over > 0:
            costs["api_calls"] = int(api_over * self.price_per_1k_api_calls_cents / 1000)

        # Compute excedente (converter segundos para minutos)
        compute_minutes = compute_seconds / 60
        compute_over = max(0, compute_minutes - self.included_compute_minutes)
        if compute_over > 0:
            compute_hours = compute_over / 60
            costs["compute"] = int(compute_hours * self.price_per_compute_hour_cents)

        # Usuarios excedentes
        if extra_users > 0:
            costs["users"] = extra_users * self.price_per_user_cents

        costs["total"] = sum(costs.values()) - costs["total"]  # Nao somar total
        return costs

    def __repr__(self):
        return f"<PricingTier {self.tier_id}: {self.name} R${self.base_price_cents/100:.2f}/mes>"
