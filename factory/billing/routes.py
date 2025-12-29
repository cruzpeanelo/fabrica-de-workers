# -*- coding: utf-8 -*-
"""
Rotas da API de Billing
========================

Endpoints para:
- Gerenciamento de tenants
- Planos e precos
- Assinaturas
- Faturas
- Webhooks do Stripe
- Portal de billing

Autor: Fabrica de Agentes
"""

import logging
from typing import Optional, List, Dict, Any
from datetime import datetime

from fastapi import APIRouter, Depends, HTTPException, Request, Header, Body
from fastapi.responses import JSONResponse, RedirectResponse
from pydantic import BaseModel, EmailStr, Field
from sqlalchemy.orm import Session

from ..database.connection import get_db
from .models import (
    Tenant, Plan, Subscription, Invoice, PaymentMethod, Usage,
    TenantStatus, PlanType, SubscriptionStatus
)
from .service import BillingService
from .tenant_service import TenantService
from .usage_service import UsageService
from .limits import PlanLimitsChecker
from .middleware import get_current_tenant, get_tenant_dependency

# Configurar logging
logger = logging.getLogger(__name__)

# Router principal
router = APIRouter(prefix="/api/billing", tags=["billing"])


# =============================================================================
# SCHEMAS (Pydantic)
# =============================================================================

class TenantCreateRequest(BaseModel):
    """Schema para criacao de tenant"""
    name: str = Field(..., min_length=2, max_length=200)
    admin_email: EmailStr
    admin_name: Optional[str] = None
    legal_name: Optional[str] = None
    document: Optional[str] = None
    plan_id: Optional[str] = None
    start_trial: bool = True

    class Config:
        json_schema_extra = {
            "example": {
                "name": "Minha Empresa",
                "admin_email": "admin@empresa.com",
                "admin_name": "Joao Silva",
                "plan_id": "PLAN-STARTER"
            }
        }


class TenantUpdateRequest(BaseModel):
    """Schema para atualizacao de tenant"""
    name: Optional[str] = None
    legal_name: Optional[str] = None
    document: Optional[str] = None
    admin_name: Optional[str] = None
    phone: Optional[str] = None
    address: Optional[Dict] = None
    config: Optional[Dict] = None


class SubscriptionCreateRequest(BaseModel):
    """Schema para criacao de assinatura"""
    plan_id: str
    trial_days: Optional[int] = None
    coupon_code: Optional[str] = None


class CheckoutRequest(BaseModel):
    """Schema para criacao de checkout session"""
    plan_id: str
    billing_period: str = "monthly"  # monthly ou yearly
    success_url: str
    cancel_url: str


class OnboardingRequest(BaseModel):
    """Schema para dados de onboarding"""
    company_info: Optional[Dict] = None
    admin_info: Optional[Dict] = None
    preferences: Optional[Dict] = None


# =============================================================================
# TENANTS
# =============================================================================

@router.post("/tenants", response_model=Dict[str, Any])
async def create_tenant(
    request: TenantCreateRequest,
    db: Session = Depends(get_db)
):
    """
    Cria um novo tenant (organizacao/empresa).

    - Cria o tenant no banco de dados
    - Cria customer no Stripe
    - Inicia trial se solicitado
    """
    service = TenantService(db)

    tenant, error = service.create_tenant(
        name=request.name,
        admin_email=request.admin_email,
        admin_name=request.admin_name,
        legal_name=request.legal_name,
        document=request.document,
        plan_id=request.plan_id,
        start_trial=request.start_trial
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {
        "success": True,
        "tenant": tenant.to_dict(),
        "message": "Tenant criado com sucesso"
    }


@router.get("/tenants/{tenant_id}", response_model=Dict[str, Any])
async def get_tenant(
    tenant_id: str,
    db: Session = Depends(get_db)
):
    """
    Obtem detalhes de um tenant.
    """
    service = TenantService(db)
    tenant = service.get_tenant(tenant_id)

    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant nao encontrado")

    stats = service.get_tenant_stats(tenant_id)

    return {
        "tenant": tenant.to_dict(),
        "stats": stats
    }


@router.put("/tenants/{tenant_id}", response_model=Dict[str, Any])
async def update_tenant(
    tenant_id: str,
    request: TenantUpdateRequest,
    db: Session = Depends(get_db)
):
    """
    Atualiza dados de um tenant.
    """
    service = TenantService(db)

    tenant, error = service.update_tenant(
        tenant_id,
        **request.model_dump(exclude_none=True)
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {
        "success": True,
        "tenant": tenant.to_dict()
    }


@router.post("/tenants/{tenant_id}/onboarding", response_model=Dict[str, Any])
async def complete_onboarding(
    tenant_id: str,
    request: OnboardingRequest,
    db: Session = Depends(get_db)
):
    """
    Completa o processo de onboarding do tenant.
    """
    service = TenantService(db)

    success, error = service.complete_onboarding(
        tenant_id,
        request.model_dump(exclude_none=True)
    )

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Onboarding completo"}


@router.post("/tenants/{tenant_id}/suspend", response_model=Dict[str, Any])
async def suspend_tenant(
    tenant_id: str,
    reason: Optional[str] = Body(None),
    db: Session = Depends(get_db)
):
    """
    Suspende um tenant.
    """
    service = TenantService(db)
    success, error = service.suspend_tenant(tenant_id, reason)

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Tenant suspenso"}


@router.post("/tenants/{tenant_id}/reactivate", response_model=Dict[str, Any])
async def reactivate_tenant(
    tenant_id: str,
    db: Session = Depends(get_db)
):
    """
    Reativa um tenant suspenso.
    """
    service = TenantService(db)
    success, error = service.reactivate_tenant(tenant_id)

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Tenant reativado"}


# =============================================================================
# PLANS
# =============================================================================

@router.get("/plans", response_model=List[Dict[str, Any]])
async def list_plans(
    public_only: bool = True,
    db: Session = Depends(get_db)
):
    """
    Lista planos disponiveis.

    - public_only: Se True, retorna apenas planos publicos
    """
    query = db.query(Plan).filter(Plan.is_active == True)

    if public_only:
        query = query.filter(Plan.is_public == True)

    plans = query.order_by(Plan.display_order).all()

    return [plan.to_dict() for plan in plans]


@router.get("/plans/{plan_id}", response_model=Dict[str, Any])
async def get_plan(
    plan_id: str,
    db: Session = Depends(get_db)
):
    """
    Obtem detalhes de um plano.
    """
    plan = db.query(Plan).filter(Plan.plan_id == plan_id).first()

    if not plan:
        raise HTTPException(status_code=404, detail="Plano nao encontrado")

    return plan.to_dict()


# =============================================================================
# SUBSCRIPTIONS
# =============================================================================

@router.get("/subscriptions", response_model=Dict[str, Any])
async def get_current_subscription(
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem a assinatura atual do tenant.
    """
    subscription = db.query(Subscription).filter(
        Subscription.tenant_id == tenant.tenant_id,
        Subscription.status.in_([
            SubscriptionStatus.ACTIVE.value,
            SubscriptionStatus.TRIALING.value
        ])
    ).first()

    if not subscription:
        return {"subscription": None, "message": "Sem assinatura ativa"}

    plan = db.query(Plan).filter(Plan.plan_id == subscription.plan_id).first()

    return {
        "subscription": subscription.to_dict(),
        "plan": plan.to_dict() if plan else None
    }


@router.post("/subscriptions", response_model=Dict[str, Any])
async def create_subscription(
    request: SubscriptionCreateRequest,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Cria uma nova assinatura para o tenant.
    """
    plan = db.query(Plan).filter(Plan.plan_id == request.plan_id).first()
    if not plan:
        raise HTTPException(status_code=404, detail="Plano nao encontrado")

    billing_service = BillingService(db)
    subscription, error = billing_service.create_subscription(
        tenant=tenant,
        plan=plan,
        trial_days=request.trial_days,
        coupon_code=request.coupon_code
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {
        "success": True,
        "subscription": subscription.to_dict()
    }


@router.post("/subscriptions/cancel", response_model=Dict[str, Any])
async def cancel_subscription(
    at_period_end: bool = True,
    reason: Optional[str] = None,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Cancela a assinatura atual.

    - at_period_end: Se True, cancela no fim do periodo atual
    """
    subscription = db.query(Subscription).filter(
        Subscription.tenant_id == tenant.tenant_id,
        Subscription.status.in_([
            SubscriptionStatus.ACTIVE.value,
            SubscriptionStatus.TRIALING.value
        ])
    ).first()

    if not subscription:
        raise HTTPException(status_code=404, detail="Assinatura nao encontrada")

    billing_service = BillingService(db)
    success, error = billing_service.cancel_subscription(
        subscription, at_period_end, reason
    )

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {
        "success": True,
        "message": "Assinatura cancelada" + (" no fim do periodo" if at_period_end else "")
    }


@router.post("/subscriptions/upgrade", response_model=Dict[str, Any])
async def upgrade_subscription(
    new_plan_id: str = Body(...),
    prorate: bool = Body(True),
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Faz upgrade/downgrade de plano.
    """
    subscription = db.query(Subscription).filter(
        Subscription.tenant_id == tenant.tenant_id,
        Subscription.status.in_([
            SubscriptionStatus.ACTIVE.value,
            SubscriptionStatus.TRIALING.value
        ])
    ).first()

    if not subscription:
        raise HTTPException(status_code=404, detail="Assinatura nao encontrada")

    new_plan = db.query(Plan).filter(Plan.plan_id == new_plan_id).first()
    if not new_plan:
        raise HTTPException(status_code=404, detail="Plano nao encontrado")

    billing_service = BillingService(db)
    updated_sub, error = billing_service.upgrade_subscription(
        subscription, new_plan, prorate
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {
        "success": True,
        "subscription": updated_sub.to_dict()
    }


# =============================================================================
# CHECKOUT & PORTAL
# =============================================================================

@router.post("/checkout", response_model=Dict[str, Any])
async def create_checkout(
    request: CheckoutRequest,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Cria uma sessao de checkout do Stripe.
    Retorna URL para redirecionar o usuario.
    """
    plan = db.query(Plan).filter(Plan.plan_id == request.plan_id).first()
    if not plan:
        raise HTTPException(status_code=404, detail="Plano nao encontrado")

    billing_service = BillingService(db)
    checkout_url, error = billing_service.create_checkout_session(
        tenant=tenant,
        plan=plan,
        success_url=request.success_url,
        cancel_url=request.cancel_url,
        billing_period=request.billing_period
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {
        "checkout_url": checkout_url,
        "message": "Redirecione o usuario para a URL de checkout"
    }


@router.get("/portal", response_model=Dict[str, Any])
async def get_billing_portal(
    return_url: str,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem URL do portal de billing do Stripe.
    Permite ao cliente gerenciar assinatura, metodos de pagamento, etc.
    """
    billing_service = BillingService(db)
    portal_url, error = billing_service.create_billing_portal_session(
        tenant, return_url
    )

    if error:
        raise HTTPException(status_code=400, detail=error)

    return {"portal_url": portal_url}


# =============================================================================
# INVOICES
# =============================================================================

@router.get("/invoices", response_model=List[Dict[str, Any]])
async def list_invoices(
    status: Optional[str] = None,
    limit: int = 10,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Lista faturas do tenant.
    """
    billing_service = BillingService(db)
    invoices = billing_service.get_invoices(tenant.tenant_id, status, limit)

    return [invoice.to_dict() for invoice in invoices]


@router.get("/invoices/{invoice_id}", response_model=Dict[str, Any])
async def get_invoice(
    invoice_id: str,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem detalhes de uma fatura.
    """
    invoice = db.query(Invoice).filter(
        Invoice.invoice_id == invoice_id,
        Invoice.tenant_id == tenant.tenant_id
    ).first()

    if not invoice:
        raise HTTPException(status_code=404, detail="Fatura nao encontrada")

    return invoice.to_dict()


# =============================================================================
# USAGE
# =============================================================================

@router.get("/usage", response_model=Dict[str, Any])
async def get_usage_summary(
    period_type: str = "monthly",
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem resumo de uso do tenant.
    """
    usage_service = UsageService(db)
    summary = usage_service.get_usage_summary(tenant.tenant_id, period_type)

    return summary


@router.get("/usage/{metric}", response_model=Dict[str, Any])
async def get_usage_history(
    metric: str,
    days: int = 30,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem historico de uso de uma metrica.
    """
    usage_service = UsageService(db)
    history = usage_service.get_usage_history(tenant.tenant_id, metric, days)

    return {
        "metric": metric,
        "days": days,
        "history": history
    }


@router.get("/usage/alerts", response_model=List[Dict[str, Any]])
async def get_usage_alerts(
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem alertas de uso (limite proximo ou excedido).
    """
    usage_service = UsageService(db)
    alerts = usage_service.check_usage_alerts(tenant.tenant_id)

    return alerts


# =============================================================================
# LIMITS
# =============================================================================

@router.get("/limits", response_model=Dict[str, Any])
async def get_limits_summary(
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Obtem resumo de limites do plano atual.
    """
    checker = PlanLimitsChecker(db)
    summary = checker.get_limits_summary(tenant.tenant_id)

    return summary


@router.get("/limits/check/{limit_name}", response_model=Dict[str, Any])
async def check_specific_limit(
    limit_name: str,
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Verifica um limite especifico.
    """
    checker = PlanLimitsChecker(db)

    # Mapear para funcoes especificas
    check_functions = {
        "projects": checker.can_create_project,
        "users": checker.can_add_user,
        "stories": checker.can_create_story,
        "jobs": checker.can_execute_job,
        "api": checker.can_make_api_request,
    }

    check_func = check_functions.get(limit_name)
    if not check_func:
        # Verificacao generica
        limit = checker.get_effective_limit(tenant.tenant_id, f"max_{limit_name}")
        return {
            "limit_name": limit_name,
            "limit": limit,
            "can_proceed": True if limit is None else True  # Sem verificacao de uso atual
        }

    can_proceed, message = check_func(tenant.tenant_id)

    return {
        "limit_name": limit_name,
        "can_proceed": can_proceed,
        "message": message
    }


# =============================================================================
# PAYMENT METHODS
# =============================================================================

@router.get("/payment-methods", response_model=List[Dict[str, Any]])
async def list_payment_methods(
    tenant: Tenant = Depends(get_tenant_dependency),
    db: Session = Depends(get_db)
):
    """
    Lista metodos de pagamento do tenant.
    """
    billing_service = BillingService(db)
    methods = billing_service.sync_payment_methods(tenant)

    return [m.to_dict() for m in methods]


# =============================================================================
# WEBHOOKS
# =============================================================================

@router.post("/webhooks/stripe")
async def stripe_webhook(
    request: Request,
    stripe_signature: str = Header(None, alias="Stripe-Signature"),
    db: Session = Depends(get_db)
):
    """
    Endpoint para receber webhooks do Stripe.
    """
    if not stripe_signature:
        raise HTTPException(status_code=400, detail="Missing Stripe-Signature header")

    payload = await request.body()

    billing_service = BillingService(db)
    success, message = billing_service.handle_webhook(payload, stripe_signature)

    if not success:
        logger.error(f"Webhook error: {message}")
        raise HTTPException(status_code=400, detail=message)

    return {"received": True, "message": message}


# =============================================================================
# ADMIN ENDPOINTS
# =============================================================================

admin_router = APIRouter(prefix="/api/admin/billing", tags=["billing-admin"])


@admin_router.get("/tenants", response_model=Dict[str, Any])
async def admin_list_tenants(
    status: Optional[str] = None,
    is_active: Optional[bool] = None,
    limit: int = 50,
    offset: int = 0,
    db: Session = Depends(get_db)
):
    """
    [ADMIN] Lista todos os tenants.
    """
    service = TenantService(db)
    tenants = service.list_tenants(status, is_active, limit, offset)

    return {
        "tenants": [t.to_dict() for t in tenants],
        "count": len(tenants),
        "limit": limit,
        "offset": offset
    }


@admin_router.get("/usage/top-consumers/{metric}", response_model=List[Dict[str, Any]])
async def admin_top_consumers(
    metric: str,
    limit: int = 10,
    db: Session = Depends(get_db)
):
    """
    [ADMIN] Lista tenants que mais consomem uma metrica.
    """
    usage_service = UsageService(db)
    consumers = usage_service.get_top_consumers(metric, "monthly", limit)

    return consumers


@admin_router.get("/usage/near-limit/{metric}", response_model=List[Dict[str, Any]])
async def admin_near_limit(
    metric: str,
    threshold: float = 80.0,
    db: Session = Depends(get_db)
):
    """
    [ADMIN] Lista tenants proximos do limite.
    """
    usage_service = UsageService(db)
    near_limit = usage_service.get_tenants_near_limit(metric, threshold)

    return near_limit


# Incluir router admin no principal
router.include_router(admin_router)
