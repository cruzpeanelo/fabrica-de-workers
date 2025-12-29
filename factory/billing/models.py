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

# Import Base do projeto
try:
    from ..database.connection import Base
except ImportError:
    from factory.database.connection import Base


# =============================================================================
# ENUMS
# =============================================================================

class TenantStatus(str, Enum):
    """Status do Tenant"""
    ACTIVE = "active"
    TRIAL = "trial"
    SUSPENDED = "suspended"
    CANCELLED = "cancelled"
    PENDING = "pending"


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


# =============================================================================
# TENANT - Organizacao/Empresa Cliente
# =============================================================================

class Tenant(Base):
    """
    Modelo para Tenant (Inquilino)
    Representa uma organizacao/empresa cliente da plataforma SaaS.

    Cada tenant tem:
    - Dados isolados (projetos, usuarios, agentes)
    - Plano de assinatura
    - Limites de uso
    - Configuracoes proprias

    Exemplo de subdominio: empresa.fabricadeagentes.com
    """
    __tablename__ = "tenants"

    id = Column(Integer, primary_key=True, autoincrement=True)
    tenant_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(200), nullable=False)
    slug = Column(String(100), unique=True, nullable=False, index=True)  # subdominio
    legal_name = Column(String(300), nullable=True)  # Razao social
    document = Column(String(20), nullable=True)  # CNPJ/CPF

    # Status
    status = Column(String(20), default=TenantStatus.PENDING.value, index=True)
    is_active = Column(Boolean, default=True, index=True)

    # Contato Principal
    admin_email = Column(String(255), nullable=False, index=True)
    admin_name = Column(String(200), nullable=True)
    phone = Column(String(20), nullable=True)

    # Endereco
    address = Column(JSON, default=dict)  # {street, city, state, zip, country}

    # Stripe
    stripe_customer_id = Column(String(100), nullable=True, unique=True, index=True)

    # Configuracoes do Tenant
    config = Column(JSON, default=dict)
    """
    Exemplo de config:
    {
        "branding": {"logo_url": "...", "primary_color": "#003B4A"},
        "features": ["custom_agents", "api_access"],
        "notifications": {"email": true, "slack": false},
        "security": {"mfa_required": false, "ip_whitelist": []}
    }
    """

    # Limites customizados (override do plano)
    custom_limits = Column(JSON, default=dict)
    """
    Exemplo:
    {
        "max_users": 50,  # Override do plano
        "max_tokens": 500000  # Override do plano
    }
    """

    # Metadados
    metadata = Column(JSON, default=dict)

    # Trial
    trial_started_at = Column(DateTime, nullable=True)
    trial_ends_at = Column(DateTime, nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    activated_at = Column(DateTime, nullable=True)
    suspended_at = Column(DateTime, nullable=True)

    # Relacionamentos
    subscriptions = relationship("Subscription", back_populates="tenant", cascade="all, delete-orphan")
    usages = relationship("Usage", back_populates="tenant", cascade="all, delete-orphan")
    invoices = relationship("Invoice", back_populates="tenant", cascade="all, delete-orphan")
    payment_methods = relationship("PaymentMethod", back_populates="tenant", cascade="all, delete-orphan")

    # Indices compostos
    __table_args__ = (
        Index('ix_tenant_status_active', 'status', 'is_active'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "name": self.name,
            "slug": self.slug,
            "legal_name": self.legal_name,
            "document": self.document,
            "status": self.status,
            "is_active": self.is_active,
            "admin_email": self.admin_email,
            "admin_name": self.admin_name,
            "phone": self.phone,
            "address": self.address or {},
            "stripe_customer_id": self.stripe_customer_id,
            "config": self.config or {},
            "custom_limits": self.custom_limits or {},
            "trial_started_at": self.trial_started_at.isoformat() if self.trial_started_at else None,
            "trial_ends_at": self.trial_ends_at.isoformat() if self.trial_ends_at else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "activated_at": self.activated_at.isoformat() if self.activated_at else None,
        }

    def is_trial_active(self) -> bool:
        """Verifica se o trial ainda esta ativo"""
        if self.status != TenantStatus.TRIAL.value:
            return False
        if not self.trial_ends_at:
            return False
        return datetime.utcnow() < self.trial_ends_at

    def get_effective_limit(self, limit_name: str, plan_limits: Dict) -> int:
        """Retorna o limite efetivo (custom_limits tem precedencia)"""
        if self.custom_limits and limit_name in self.custom_limits:
            return self.custom_limits[limit_name]
        return plan_limits.get(limit_name, 0)

    def __repr__(self):
        return f"<Tenant {self.tenant_id}: {self.name} [{self.status}]>"


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

    # Metadados
    metadata = Column(JSON, default=dict)

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
    tenant = relationship("Tenant", back_populates="subscriptions")

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

    # Metadados
    metadata = Column(JSON, default=dict)

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
    tenant = relationship("Tenant", back_populates="usages")

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
    tenant = relationship("Tenant", back_populates="invoices")

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

    # Metadados
    metadata = Column(JSON, default=dict)

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
    tenant = relationship("Tenant", back_populates="payment_methods")

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

    # Metadados
    metadata = Column(JSON, default=dict)

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
