# -*- coding: utf-8 -*-
"""
Modulo de Billing e Multi-Tenancy - Fabrica de Agentes SaaS
============================================================

Este modulo implementa:
- Modelo Multi-Tenant com isolamento de dados
- Planos de Subscription (Starter, Professional, Enterprise)
- Integracao com Stripe para pagamentos
- Metricas de uso (Usage Metering)
- Faturamento e invoicing
- Usage-Based Billing (Issue #118, #121, #89)

Autor: Fabrica de Agentes
Versao: 2.0.0
"""

from .models import (
    # Tenant e Planos
    Tenant,
    TenantStatus,
    Plan,
    PlanType,
    Subscription,
    SubscriptionStatus,
    # Usage
    Usage,
    UsageMetric,
    UsageEvent,
    UsageAggregate,
    UsageEventType,
    # Billing
    Invoice,
    InvoiceItem,
    InvoiceStatus,
    PricingTier,
    BillingPeriod,
    PaymentMethod,
)

from .service import BillingService
from .tenant_service import TenantService
from .usage_service import UsageService
from .invoice_service import InvoiceService
from .middleware import TenantMiddleware, get_current_tenant, require_tenant
from .limits import PlanLimitsChecker

# Usage Tracking Middleware (Issue #118)
from .usage_middleware import (
    UsageTrackingMiddleware,
    LLMUsageTracker,
    StorageUsageTracker,
    ComputeUsageTracker,
    AuthUsageTracker,
    track_api_usage,
    track_llm_usage,
)

__all__ = [
    # Modelos - Tenant
    "Tenant",
    "TenantStatus",
    "Plan",
    "PlanType",
    "Subscription",
    "SubscriptionStatus",
    # Modelos - Usage (Issue #118)
    "Usage",
    "UsageMetric",
    "UsageEvent",
    "UsageAggregate",
    "UsageEventType",
    # Modelos - Billing (Issue #121)
    "Invoice",
    "InvoiceItem",
    "InvoiceStatus",
    "PricingTier",
    "BillingPeriod",
    "PaymentMethod",
    # Servicos
    "BillingService",
    "TenantService",
    "UsageService",
    "InvoiceService",
    # Middleware
    "TenantMiddleware",
    "get_current_tenant",
    "require_tenant",
    # Usage Tracking Middleware (Issue #118)
    "UsageTrackingMiddleware",
    "LLMUsageTracker",
    "StorageUsageTracker",
    "ComputeUsageTracker",
    "AuthUsageTracker",
    "track_api_usage",
    "track_llm_usage",
    # Utilidades
    "PlanLimitsChecker",
]

__version__ = "2.0.0"
