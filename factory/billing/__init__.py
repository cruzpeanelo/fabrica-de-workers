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

Autor: Fabrica de Agentes
Versao: 1.0.0
"""

from .models import (
    Tenant,
    TenantStatus,
    Plan,
    PlanType,
    Subscription,
    SubscriptionStatus,
    Usage,
    UsageMetric,
    Invoice,
    InvoiceStatus,
    PaymentMethod,
)

from .service import BillingService
from .tenant_service import TenantService
from .usage_service import UsageService
from .middleware import TenantMiddleware, get_current_tenant, require_tenant
from .limits import PlanLimitsChecker

__all__ = [
    # Modelos
    "Tenant",
    "TenantStatus",
    "Plan",
    "PlanType",
    "Subscription",
    "SubscriptionStatus",
    "Usage",
    "UsageMetric",
    "Invoice",
    "InvoiceStatus",
    "PaymentMethod",
    # Servicos
    "BillingService",
    "TenantService",
    "UsageService",
    # Middleware
    "TenantMiddleware",
    "get_current_tenant",
    "require_tenant",
    # Utilidades
    "PlanLimitsChecker",
]

__version__ = "1.0.0"
