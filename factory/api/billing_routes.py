# -*- coding: utf-8 -*-
"""
Billing API Routes
==================

Endpoints para gestao de billing e usage:
- GET /api/v1/billing/current-usage - Uso atual do tenant
- GET /api/v1/billing/invoices - Lista de faturas
- GET /api/v1/billing/invoices/:id - Detalhes de uma fatura
- POST /api/v1/admin/billing/generate-invoice - Gerar fatura (admin)

Issue #89 - Painel de Billing e Usage

Autor: Fabrica de Agentes
"""

import logging
from datetime import date, datetime
from typing import Optional, List

from fastapi import APIRouter, Depends, HTTPException, Query, Path
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session

from ..database.connection import get_db
from ..billing.middleware import get_current_tenant_id, get_tenant_dependency

# Import authentication for admin endpoints (Issue #136)
from .auth import get_current_user, TokenData
from ..billing.models import (
    Tenant, Invoice, UsageAggregate, PricingTier,
    InvoiceStatus, UsageEvent
)
from ..billing.usage_service import UsageService
from ..billing.invoice_service import InvoiceService

# Configurar logging
logger = logging.getLogger(__name__)

# Router para endpoints de billing
router = APIRouter(prefix="/api/v1/billing", tags=["billing"])

# Router para endpoints administrativos
admin_router = APIRouter(prefix="/api/v1/admin/billing", tags=["admin-billing"])


# =============================================================================
# SCHEMAS
# =============================================================================

class UsageMetricsResponse(BaseModel):
    """Resposta com metricas de uso"""
    api_calls: int = 0
    llm_tokens_input: int = 0
    llm_tokens_output: int = 0
    llm_tokens_total: int = 0
    storage_bytes: int = 0
    storage_mb: float = 0
    compute_seconds: int = 0
    compute_minutes: float = 0
    active_users: int = 0
    logins_count: int = 0
    file_uploads: int = 0
    file_downloads: int = 0


class UsageCostsResponse(BaseModel):
    """Resposta com custos de uso"""
    api_cents: int = 0
    llm_cents: int = 0
    storage_cents: int = 0
    compute_cents: int = 0
    total_cents: int = 0
    total_formatted: str = "R$ 0.00"


class CurrentUsageResponse(BaseModel):
    """Resposta de uso atual"""
    tenant_id: str
    period: str
    period_type: str
    metrics: UsageMetricsResponse
    costs: UsageCostsResponse
    limits: Optional[dict] = None


class UsageBreakdownResponse(BaseModel):
    """Resposta com breakdown de uso"""
    tenant_id: str
    month: str
    breakdown: dict
    total_cost_cents: int
    total_cost_formatted: str


class InvoiceItemResponse(BaseModel):
    """Item de fatura"""
    item_id: str
    description: str
    quantity: float
    unit_price_cents: int
    amount_cents: int
    item_type: str


class InvoiceResponse(BaseModel):
    """Resposta de fatura"""
    invoice_id: str
    invoice_number: str
    tenant_id: str
    period: str
    period_start: Optional[str] = None
    period_end: Optional[str] = None
    status: str
    subtotal_cents: int = 0
    discount_cents: int = 0
    tax_cents: int = 0
    total_cents: int = 0
    total_formatted: str = "R$ 0.00"
    due_date: Optional[str] = None
    paid_at: Optional[str] = None
    items: Optional[List[InvoiceItemResponse]] = None
    created_at: Optional[str] = None


class InvoiceListResponse(BaseModel):
    """Lista de faturas"""
    invoices: List[InvoiceResponse]
    total: int
    page: int
    limit: int


class GenerateInvoiceRequest(BaseModel):
    """Request para gerar fatura"""
    tenant_id: str
    period: Optional[str] = None
    auto_finalize: bool = False


class GenerateInvoiceResponse(BaseModel):
    """Resposta de geracao de fatura"""
    success: bool
    invoice: Optional[InvoiceResponse] = None
    error: Optional[str] = None


class BillingSummaryResponse(BaseModel):
    """Resumo de billing"""
    tenant_id: str
    current_month: str
    current_usage: dict
    pending_amount_cents: int
    pending_amount_formatted: str
    last_invoice: Optional[InvoiceResponse] = None


class UsageEventResponse(BaseModel):
    """Evento de uso"""
    event_id: str
    event_type: str
    resource: Optional[str] = None
    action: Optional[str] = None
    quantity: int
    unit: Optional[str] = None
    cost_cents: int = 0
    timestamp: str
    metadata: Optional[dict] = None


class UsageEventsListResponse(BaseModel):
    """Lista de eventos"""
    events: List[UsageEventResponse]
    total: int
    page: int
    limit: int


# =============================================================================
# ENDPOINTS - USAGE
# =============================================================================

@router.get("/current-usage", response_model=CurrentUsageResponse)
async def get_current_usage(
    period_type: str = Query("monthly", description="daily ou monthly"),
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Obtem uso atual do tenant no periodo.

    - **period_type**: daily para uso diario, monthly para mensal
    """
    usage_service = UsageService(db)

    # Obter uso
    usage_data = usage_service.get_current_usage(
        tenant_id=tenant.tenant_id,
        period_type=period_type
    )

    # Obter limites do plano
    pricing = usage_service._get_tenant_pricing(tenant.tenant_id)
    limits = None
    if pricing:
        limits = {
            "api_calls": pricing.included_api_calls,
            "tokens": pricing.included_tokens,
            "storage_mb": pricing.included_storage_mb,
            "compute_minutes": pricing.included_compute_minutes,
        }

    return CurrentUsageResponse(
        tenant_id=usage_data.get("tenant_id", tenant.tenant_id),
        period=usage_data.get("period", ""),
        period_type=usage_data.get("period_type", period_type),
        metrics=UsageMetricsResponse(**usage_data.get("metrics", {})),
        costs=UsageCostsResponse(**usage_data.get("costs", {})),
        limits=limits
    )


@router.get("/usage-breakdown", response_model=UsageBreakdownResponse)
async def get_usage_breakdown(
    month: Optional[str] = Query(None, description="Mes no formato YYYY-MM"),
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Obtem breakdown detalhado de uso e custos.

    - **month**: Mes no formato YYYY-MM (atual se nao informado)
    """
    usage_service = UsageService(db)

    if not month:
        month = date.today().strftime("%Y-%m")

    breakdown = usage_service.get_usage_breakdown(
        tenant_id=tenant.tenant_id,
        month=month
    )

    return UsageBreakdownResponse(
        tenant_id=breakdown.get("tenant_id", tenant.tenant_id),
        month=month,
        breakdown=breakdown.get("breakdown", {}),
        total_cost_cents=breakdown.get("total_cost_cents", 0),
        total_cost_formatted=breakdown.get("total_cost_formatted", "R$ 0.00")
    )


@router.get("/usage-events", response_model=UsageEventsListResponse)
async def list_usage_events(
    event_type: Optional[str] = Query(None, description="Filtrar por tipo"),
    start_date: Optional[str] = Query(None, description="Data inicial YYYY-MM-DD"),
    end_date: Optional[str] = Query(None, description="Data final YYYY-MM-DD"),
    page: int = Query(1, ge=1),
    limit: int = Query(50, ge=1, le=200),
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Lista eventos de uso do tenant.

    - **event_type**: api_call, llm_tokens, storage, compute, login
    - **start_date**: Filtrar a partir de
    - **end_date**: Filtrar ate
    """
    usage_service = UsageService(db)

    # Parse dates
    start_dt = None
    end_dt = None
    if start_date:
        start_dt = datetime.strptime(start_date, "%Y-%m-%d")
    if end_date:
        end_dt = datetime.strptime(end_date, "%Y-%m-%d")

    offset = (page - 1) * limit

    events = usage_service.get_events(
        tenant_id=tenant.tenant_id,
        event_type=event_type,
        start_date=start_dt,
        end_date=end_dt,
        limit=limit,
        offset=offset
    )

    # Contar total (query simples)
    total = db.query(UsageEvent).filter(
        UsageEvent.tenant_id == tenant.tenant_id
    ).count()

    return UsageEventsListResponse(
        events=[
            UsageEventResponse(
                event_id=e.event_id,
                event_type=e.event_type,
                resource=e.resource,
                action=e.action,
                quantity=e.quantity,
                unit=e.unit,
                cost_cents=e.cost_cents or 0,
                timestamp=e.timestamp.isoformat() if e.timestamp else "",
                metadata=e.event_data
            )
            for e in events
        ],
        total=total,
        page=page,
        limit=limit
    )


# =============================================================================
# ENDPOINTS - INVOICES
# =============================================================================

@router.get("/invoices", response_model=InvoiceListResponse)
async def list_invoices(
    status: Optional[str] = Query(None, description="Filtrar por status"),
    page: int = Query(1, ge=1),
    limit: int = Query(20, ge=1, le=100),
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Lista faturas do tenant.

    - **status**: draft, pending, paid, void, uncollectible
    """
    invoice_service = InvoiceService(db)

    offset = (page - 1) * limit

    invoices = invoice_service.list_invoices(
        tenant_id=tenant.tenant_id,
        status=status,
        limit=limit,
        offset=offset
    )

    total = db.query(Invoice).filter(
        Invoice.tenant_id == tenant.tenant_id
    ).count()

    return InvoiceListResponse(
        invoices=[
            InvoiceResponse(
                invoice_id=inv.invoice_id,
                invoice_number=inv.invoice_number,
                tenant_id=inv.tenant_id,
                period=inv.period,
                period_start=inv.period_start.isoformat() if inv.period_start else None,
                period_end=inv.period_end.isoformat() if inv.period_end else None,
                status=inv.status,
                subtotal_cents=inv.subtotal_cents or 0,
                discount_cents=inv.discount_cents or 0,
                tax_cents=inv.tax_cents or 0,
                total_cents=inv.total_cents or 0,
                total_formatted=f"R$ {(inv.total_cents or 0) / 100:.2f}",
                due_date=inv.due_date.isoformat() if inv.due_date else None,
                paid_at=inv.paid_at.isoformat() if inv.paid_at else None,
                created_at=inv.created_at.isoformat() if inv.created_at else None,
            )
            for inv in invoices
        ],
        total=total,
        page=page,
        limit=limit
    )


@router.get("/invoices/{invoice_id}", response_model=InvoiceResponse)
async def get_invoice(
    invoice_id: str = Path(..., description="ID da fatura"),
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Obtem detalhes de uma fatura especifica.

    Inclui itens da fatura.
    """
    invoice_service = InvoiceService(db)

    invoice = invoice_service.get_invoice(invoice_id)

    if not invoice:
        raise HTTPException(status_code=404, detail="Fatura nao encontrada")

    # Verificar se pertence ao tenant
    if invoice.tenant_id != tenant.tenant_id:
        raise HTTPException(status_code=403, detail="Acesso negado")

    # Buscar itens
    items = [
        InvoiceItemResponse(
            item_id=item.item_id,
            description=item.description,
            quantity=item.quantity or 1,
            unit_price_cents=item.unit_price_cents or 0,
            amount_cents=item.amount_cents or 0,
            item_type=item.item_type or "subscription"
        )
        for item in invoice.items
    ]

    return InvoiceResponse(
        invoice_id=invoice.invoice_id,
        invoice_number=invoice.invoice_number,
        tenant_id=invoice.tenant_id,
        period=invoice.period,
        period_start=invoice.period_start.isoformat() if invoice.period_start else None,
        period_end=invoice.period_end.isoformat() if invoice.period_end else None,
        status=invoice.status,
        subtotal_cents=invoice.subtotal_cents or 0,
        discount_cents=invoice.discount_cents or 0,
        tax_cents=invoice.tax_cents or 0,
        total_cents=invoice.total_cents or 0,
        total_formatted=f"R$ {(invoice.total_cents or 0) / 100:.2f}",
        due_date=invoice.due_date.isoformat() if invoice.due_date else None,
        paid_at=invoice.paid_at.isoformat() if invoice.paid_at else None,
        items=items,
        created_at=invoice.created_at.isoformat() if invoice.created_at else None,
    )


@router.get("/summary", response_model=BillingSummaryResponse)
async def get_billing_summary(
    db: Session = Depends(get_db),
    tenant: Tenant = Depends(get_tenant_dependency)
):
    """
    Obtem resumo de billing do tenant.

    Inclui uso atual, valor pendente e ultima fatura.
    """
    usage_service = UsageService(db)
    invoice_service = InvoiceService(db)

    current_month = date.today().strftime("%Y-%m")

    # Uso atual
    current_usage = usage_service.get_current_usage(
        tenant_id=tenant.tenant_id,
        period_type="monthly"
    )

    # Faturas pendentes
    pending_invoices = invoice_service.list_invoices(
        tenant_id=tenant.tenant_id,
        status=InvoiceStatus.PENDING.value
    )

    pending_amount = sum(inv.total_cents or 0 for inv in pending_invoices)

    # Ultima fatura
    last_invoices = invoice_service.list_invoices(
        tenant_id=tenant.tenant_id,
        limit=1
    )

    last_invoice = None
    if last_invoices:
        inv = last_invoices[0]
        last_invoice = InvoiceResponse(
            invoice_id=inv.invoice_id,
            invoice_number=inv.invoice_number,
            tenant_id=inv.tenant_id,
            period=inv.period,
            status=inv.status,
            total_cents=inv.total_cents or 0,
            total_formatted=f"R$ {(inv.total_cents or 0) / 100:.2f}",
            paid_at=inv.paid_at.isoformat() if inv.paid_at else None,
        )

    return BillingSummaryResponse(
        tenant_id=tenant.tenant_id,
        current_month=current_month,
        current_usage=current_usage,
        pending_amount_cents=pending_amount,
        pending_amount_formatted=f"R$ {pending_amount / 100:.2f}",
        last_invoice=last_invoice
    )


# =============================================================================
# ENDPOINTS ADMINISTRATIVOS
# =============================================================================

@admin_router.post("/generate-invoice", response_model=GenerateInvoiceResponse)
async def admin_generate_invoice(
    request: GenerateInvoiceRequest,
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Gera fatura para um tenant especifico.

    Endpoint administrativo - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    invoice, error = invoice_service.generate_invoice(
        tenant_id=request.tenant_id,
        period=request.period,
        auto_finalize=request.auto_finalize
    )

    if error:
        return GenerateInvoiceResponse(
            success=False,
            error=error
        )

    return GenerateInvoiceResponse(
        success=True,
        invoice=InvoiceResponse(
            invoice_id=invoice.invoice_id,
            invoice_number=invoice.invoice_number,
            tenant_id=invoice.tenant_id,
            period=invoice.period,
            status=invoice.status,
            total_cents=invoice.total_cents or 0,
            total_formatted=f"R$ {(invoice.total_cents or 0) / 100:.2f}",
        )
    )


@admin_router.post("/generate-monthly-invoices")
async def admin_generate_monthly_invoices(
    period: Optional[str] = Query(None, description="Periodo YYYY-MM"),
    auto_finalize: bool = Query(True),
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Gera faturas para todos os tenants ativos.

    Endpoint administrativo para processamento em lote - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    results = invoice_service.generate_monthly_invoices(
        period=period,
        auto_finalize=auto_finalize
    )

    return results


@admin_router.post("/process-overdue")
async def admin_process_overdue(
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Processa faturas vencidas.

    Suspende tenants com faturas vencidas ha mais de 30 dias - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    results = invoice_service.process_overdue_invoices()

    return results


@admin_router.post("/invoices/{invoice_id}/finalize")
async def admin_finalize_invoice(
    invoice_id: str = Path(...),
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Finaliza uma fatura (draft -> pending) - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    success, error = invoice_service.finalize_invoice(invoice_id)

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Fatura finalizada"}


@admin_router.post("/invoices/{invoice_id}/mark-paid")
async def admin_mark_invoice_paid(
    invoice_id: str = Path(...),
    payment_id: Optional[str] = Query(None),
    payment_method: Optional[str] = Query(None),
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Marca uma fatura como paga - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    success, error = invoice_service.mark_as_paid(
        invoice_id=invoice_id,
        payment_id=payment_id,
        payment_method=payment_method
    )

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Fatura marcada como paga"}


@admin_router.post("/invoices/{invoice_id}/void")
async def admin_void_invoice(
    invoice_id: str = Path(...),
    reason: Optional[str] = Query(None),
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Anula uma fatura - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    invoice_service = InvoiceService(db)

    success, error = invoice_service.void_invoice(
        invoice_id=invoice_id,
        reason=reason
    )

    if not success:
        raise HTTPException(status_code=400, detail=error)

    return {"success": True, "message": "Fatura anulada"}


@admin_router.get("/usage/{tenant_id}")
async def admin_get_tenant_usage(
    tenant_id: str = Path(...),
    month: Optional[str] = Query(None),
    db: Session = Depends(get_db),
    current_user: TokenData = Depends(get_current_user)
):
    """
    Obtem uso detalhado de um tenant especifico - requer role ADMIN.
    """
    # Issue #136: Verificar permissao de admin
    if current_user.role != "ADMIN":
        raise HTTPException(status_code=403, detail="Acesso negado. Requer role ADMIN.")

    usage_service = UsageService(db)

    if not month:
        month = date.today().strftime("%Y-%m")

    breakdown = usage_service.get_usage_breakdown(
        tenant_id=tenant_id,
        month=month
    )

    return breakdown


# =============================================================================
# FUNCAO PARA REGISTRAR ROUTERS
# =============================================================================

def include_billing_routes(app):
    """
    Registra routers de billing na aplicacao.

    Args:
        app: Aplicacao FastAPI
    """
    app.include_router(router)
    app.include_router(admin_router)
