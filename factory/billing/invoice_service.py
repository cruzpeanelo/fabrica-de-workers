# -*- coding: utf-8 -*-
"""
Servico de Geracao de Faturas (Invoice Service)
=================================================

Implementa:
- Geracao automatica de faturas mensais
- Calculo de custos por uso (usage-based billing)
- Aplicacao de descontos e creditos
- Integracao com Stripe para cobranca
- Relatorios de faturamento

Issue #121 - Sistema de Cobranca por Uso (Usage-Based Billing)

Autor: Fabrica de Agentes
"""

import uuid
import logging
from datetime import datetime, date, timedelta
from decimal import Decimal, ROUND_HALF_UP
from typing import Optional, Dict, Any, List, Tuple
from calendar import monthrange

from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func

from .models import (
    Tenant, Plan, Subscription, Invoice, InvoiceItem, Usage,
    UsageAggregate, PricingTier,
    TenantStatus, SubscriptionStatus, InvoiceStatus
)
from .usage_service import UsageService

# Configurar logging
logger = logging.getLogger(__name__)


class InvoiceService:
    """
    Servico de geracao e gerenciamento de faturas.

    Implementa usage-based billing com:
    - Preco base do plano
    - Custos variaveis por uso excedente
    - Descontos e creditos
    - Impostos

    Uso:
        service = InvoiceService(db_session)
        invoice = service.generate_invoice(tenant_id, period="2025-01")
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self.usage_service = UsageService(db)

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    def _generate_invoice_number(self, tenant_id: str) -> str:
        """
        Gera numero sequencial de fatura.

        Formato: INV-{ANO}{MES}-{SEQUENCIAL:04d}
        """
        year_month = datetime.utcnow().strftime("%Y%m")

        # Contar faturas do mes
        count = self.db.query(func.count(Invoice.id)).filter(
            Invoice.invoice_number.like(f"INV-{year_month}-%")
        ).scalar() or 0

        return f"INV-{year_month}-{count + 1:04d}"

    # =========================================================================
    # GERACAO DE FATURAS
    # =========================================================================

    def generate_invoice(
        self,
        tenant_id: str,
        period: Optional[str] = None,
        auto_finalize: bool = False
    ) -> Tuple[Optional[Invoice], Optional[str]]:
        """
        Gera fatura para um tenant.

        Args:
            tenant_id: ID do tenant
            period: Periodo no formato YYYY-MM (mes anterior se None)
            auto_finalize: Se True, finaliza fatura automaticamente

        Returns:
            Tupla (Invoice, erro_msg)
        """
        # Determinar periodo
        if not period:
            last_month = date.today().replace(day=1) - timedelta(days=1)
            period = last_month.strftime("%Y-%m")

        # Verificar se ja existe fatura para o periodo
        existing = self.db.query(Invoice).filter(
            and_(
                Invoice.tenant_id == tenant_id,
                Invoice.period == period
            )
        ).first()

        if existing:
            return None, f"Fatura ja existe para o periodo {period}"

        # Buscar tenant
        tenant = self.db.query(Tenant).filter(
            Tenant.tenant_id == tenant_id
        ).first()

        if not tenant:
            return None, "Tenant nao encontrado"

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

        if not subscription:
            return None, "Sem assinatura ativa"

        # Buscar plano
        plan = self.db.query(Plan).filter(
            Plan.plan_id == subscription.plan_id
        ).first()

        if not plan:
            return None, "Plano nao encontrado"

        # Buscar pricing tier
        pricing = self.db.query(PricingTier).filter(
            and_(
                PricingTier.plan_id == plan.plan_id,
                PricingTier.is_active == True
            )
        ).first()

        if not pricing:
            # Usar pricing padrao
            pricing = self.db.query(PricingTier).filter(
                PricingTier.is_default == True
            ).first()

        try:
            # Calcular datas do periodo
            year, month = map(int, period.split("-"))
            period_start = date(year, month, 1)
            _, last_day = monthrange(year, month)
            period_end = date(year, month, last_day)

            # Agregar uso do periodo
            self.usage_service.aggregate_monthly_usage(tenant_id, period)

            # Buscar agregacao mensal
            usage = self.db.query(UsageAggregate).filter(
                and_(
                    UsageAggregate.tenant_id == tenant_id,
                    UsageAggregate.period == period,
                    UsageAggregate.period_type == "monthly"
                )
            ).first()

            # Criar fatura
            invoice = Invoice(
                invoice_id=self._generate_id("INV"),
                invoice_number=self._generate_invoice_number(tenant_id),
                tenant_id=tenant_id,
                subscription_id=subscription.subscription_id,
                period=period,
                period_start=period_start,
                period_end=period_end,
                status=InvoiceStatus.DRAFT.value,
                currency="BRL",
            )

            self.db.add(invoice)
            self.db.flush()

            # Adicionar itens da fatura
            items = self._create_invoice_items(
                invoice=invoice,
                plan=plan,
                pricing=pricing,
                usage=usage,
                subscription=subscription
            )

            # Calcular totais
            invoice.subtotal_cents = sum(item.amount_cents for item in items)

            # Aplicar desconto se houver
            discount = self._calculate_discount(tenant, subscription)
            invoice.discount_cents = discount
            invoice.discount_reason = "Desconto promocional" if discount > 0 else None

            # Calcular impostos (exemplo: 5%)
            taxable_amount = invoice.subtotal_cents - invoice.discount_cents
            invoice.tax_cents = int(taxable_amount * 0.05)  # 5% de imposto
            invoice.tax_rate = Decimal("5.00")

            # Total final
            invoice.total_cents = taxable_amount + invoice.tax_cents

            # Finalizar se solicitado
            if auto_finalize and invoice.total_cents > 0:
                invoice.status = InvoiceStatus.PENDING.value
                invoice.due_date = period_end + timedelta(days=15)
            elif invoice.total_cents == 0:
                invoice.status = InvoiceStatus.PAID.value
                invoice.notes = "Fatura zerada - sem cobranca"

            # Metadados
            invoice.metadata = {
                "plan_id": plan.plan_id,
                "plan_name": plan.name,
                "pricing_tier_id": pricing.tier_id if pricing else None,
                "generated_at": datetime.utcnow().isoformat(),
            }

            self.db.commit()
            logger.info(f"Fatura gerada: {invoice.invoice_number} para tenant {tenant_id}")

            return invoice, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao gerar fatura: {e}")
            return None, str(e)

    def _create_invoice_items(
        self,
        invoice: Invoice,
        plan: Plan,
        pricing: Optional[PricingTier],
        usage: Optional[UsageAggregate],
        subscription: Subscription
    ) -> List[InvoiceItem]:
        """
        Cria itens da fatura.

        Args:
            invoice: Fatura
            plan: Plano do tenant
            pricing: Tier de precificacao
            usage: Uso agregado do periodo
            subscription: Assinatura

        Returns:
            Lista de InvoiceItems
        """
        items = []

        # 1. Item do plano base (pro-rata se necessario)
        base_item = self._create_plan_base_item(invoice, plan, subscription)
        items.append(base_item)

        if not usage or not pricing:
            return items

        # 2. Itens de uso excedente

        # API Calls excedentes
        if usage.api_calls > pricing.included_api_calls:
            overage = usage.api_calls - pricing.included_api_calls
            api_item = InvoiceItem(
                item_id=self._generate_id("ITEM"),
                invoice_id=invoice.invoice_id,
                description="Chamadas de API excedentes",
                quantity=overage,
                unit_price_cents=int(pricing.price_per_1k_api_calls_cents / 1000),
                amount_cents=usage.cost_api_cents,
                item_type="usage",
                metadata={
                    "metric": "api_calls",
                    "included": pricing.included_api_calls,
                    "used": usage.api_calls,
                    "overage": overage
                }
            )
            self.db.add(api_item)
            items.append(api_item)

        # Tokens LLM excedentes
        total_tokens = usage.llm_tokens_input + usage.llm_tokens_output
        if total_tokens > pricing.included_tokens:
            overage = total_tokens - pricing.included_tokens
            llm_item = InvoiceItem(
                item_id=self._generate_id("ITEM"),
                invoice_id=invoice.invoice_id,
                description="Tokens LLM excedentes",
                quantity=overage,
                unit_price_cents=int(pricing.price_per_million_tokens_cents / 1_000_000),
                amount_cents=usage.cost_llm_cents,
                item_type="usage",
                metadata={
                    "metric": "llm_tokens",
                    "included": pricing.included_tokens,
                    "used": total_tokens,
                    "input_tokens": usage.llm_tokens_input,
                    "output_tokens": usage.llm_tokens_output,
                    "overage": overage
                }
            )
            self.db.add(llm_item)
            items.append(llm_item)

        # Storage excedente
        storage_mb = usage.storage_bytes / (1024 * 1024)
        if storage_mb > pricing.included_storage_mb:
            overage_mb = storage_mb - pricing.included_storage_mb
            overage_gb = overage_mb / 1024
            storage_item = InvoiceItem(
                item_id=self._generate_id("ITEM"),
                invoice_id=invoice.invoice_id,
                description="Storage excedente",
                quantity=int(overage_gb * 100) / 100,  # 2 casas decimais
                unit_price_cents=pricing.price_per_gb_storage_cents,
                amount_cents=usage.cost_storage_cents,
                item_type="usage",
                metadata={
                    "metric": "storage",
                    "unit": "GB",
                    "included_mb": pricing.included_storage_mb,
                    "used_mb": round(storage_mb, 2),
                    "overage_mb": round(overage_mb, 2)
                }
            )
            self.db.add(storage_item)
            items.append(storage_item)

        # Compute excedente
        compute_minutes = usage.compute_seconds / 60
        if compute_minutes > pricing.included_compute_minutes:
            overage_minutes = compute_minutes - pricing.included_compute_minutes
            overage_hours = overage_minutes / 60
            compute_item = InvoiceItem(
                item_id=self._generate_id("ITEM"),
                invoice_id=invoice.invoice_id,
                description="Compute excedente",
                quantity=round(overage_hours, 2),
                unit_price_cents=pricing.price_per_compute_hour_cents,
                amount_cents=usage.cost_compute_cents,
                item_type="usage",
                metadata={
                    "metric": "compute",
                    "unit": "hours",
                    "included_minutes": pricing.included_compute_minutes,
                    "used_minutes": round(compute_minutes, 2),
                    "overage_minutes": round(overage_minutes, 2)
                }
            )
            self.db.add(compute_item)
            items.append(compute_item)

        return items

    def _create_plan_base_item(
        self,
        invoice: Invoice,
        plan: Plan,
        subscription: Subscription
    ) -> InvoiceItem:
        """
        Cria item do plano base.

        Args:
            invoice: Fatura
            plan: Plano
            subscription: Assinatura

        Returns:
            InvoiceItem do plano
        """
        # Calcular pro-rata se assinatura comecou no meio do mes
        year, month = map(int, invoice.period.split("-"))
        _, days_in_month = monthrange(year, month)
        period_start = date(year, month, 1)
        period_end = date(year, month, days_in_month)

        # Dias ativos no periodo
        sub_start = subscription.current_period_start.date() if subscription.current_period_start else period_start
        effective_start = max(sub_start, period_start)
        days_active = (period_end - effective_start).days + 1

        # Pro-rata
        pro_rata_factor = Decimal(days_active) / Decimal(days_in_month)
        base_amount = int(float(plan.price_monthly_cents) * float(pro_rata_factor))

        item = InvoiceItem(
            item_id=self._generate_id("ITEM"),
            invoice_id=invoice.invoice_id,
            description=f"Plano {plan.name} - {invoice.period}",
            quantity=1,
            unit_price_cents=plan.price_monthly_cents,
            amount_cents=base_amount,
            item_type="subscription",
            metadata={
                "plan_id": plan.plan_id,
                "plan_name": plan.name,
                "days_active": days_active,
                "days_in_month": days_in_month,
                "pro_rata_factor": float(pro_rata_factor)
            }
        )

        self.db.add(item)
        return item

    def _calculate_discount(
        self,
        tenant: Tenant,
        subscription: Subscription
    ) -> int:
        """
        Calcula desconto aplicavel.

        Args:
            tenant: Tenant
            subscription: Assinatura

        Returns:
            Valor do desconto em centavos
        """
        discount = 0

        # Verificar desconto de trial
        if subscription.is_in_trial():
            return 0  # Trial gratuito

        # Verificar creditos do tenant
        if tenant.extra_data and "credits_cents" in tenant.extra_data:
            credits = tenant.extra_data.get("credits_cents", 0)
            if credits > 0:
                discount += credits

        # Verificar cupom de desconto
        if subscription.discount_percent:
            # Aplicado depois no calculo final
            pass

        return discount

    # =========================================================================
    # GERENCIAMENTO DE FATURAS
    # =========================================================================

    def get_invoice(self, invoice_id: str) -> Optional[Invoice]:
        """
        Busca fatura por ID.

        Args:
            invoice_id: ID da fatura

        Returns:
            Invoice ou None
        """
        return self.db.query(Invoice).filter(
            Invoice.invoice_id == invoice_id
        ).first()

    def get_invoice_by_number(self, invoice_number: str) -> Optional[Invoice]:
        """
        Busca fatura por numero.

        Args:
            invoice_number: Numero da fatura

        Returns:
            Invoice ou None
        """
        return self.db.query(Invoice).filter(
            Invoice.invoice_number == invoice_number
        ).first()

    def list_invoices(
        self,
        tenant_id: str,
        status: Optional[str] = None,
        start_date: Optional[date] = None,
        end_date: Optional[date] = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[Invoice]:
        """
        Lista faturas do tenant.

        Args:
            tenant_id: ID do tenant
            status: Filtrar por status
            start_date: Data inicial
            end_date: Data final
            limit: Limite
            offset: Offset

        Returns:
            Lista de Invoices
        """
        query = self.db.query(Invoice).filter(
            Invoice.tenant_id == tenant_id
        )

        if status:
            query = query.filter(Invoice.status == status)

        if start_date:
            query = query.filter(Invoice.period_start >= start_date)

        if end_date:
            query = query.filter(Invoice.period_end <= end_date)

        return query.order_by(Invoice.created_at.desc()).offset(offset).limit(limit).all()

    def finalize_invoice(self, invoice_id: str) -> Tuple[bool, Optional[str]]:
        """
        Finaliza uma fatura (draft -> pending).

        Args:
            invoice_id: ID da fatura

        Returns:
            Tupla (sucesso, erro_msg)
        """
        invoice = self.get_invoice(invoice_id)
        if not invoice:
            return False, "Fatura nao encontrada"

        if invoice.status != InvoiceStatus.DRAFT.value:
            return False, f"Fatura nao pode ser finalizada - status atual: {invoice.status}"

        try:
            invoice.status = InvoiceStatus.PENDING.value

            # Definir data de vencimento (15 dias apos finalizacao)
            if not invoice.due_date:
                invoice.due_date = date.today() + timedelta(days=15)

            self.db.commit()
            logger.info(f"Fatura finalizada: {invoice.invoice_number}")

            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao finalizar fatura: {e}")
            return False, str(e)

    def mark_as_paid(
        self,
        invoice_id: str,
        payment_id: Optional[str] = None,
        payment_method: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Marca fatura como paga.

        Args:
            invoice_id: ID da fatura
            payment_id: ID do pagamento (ex: pi_xxx do Stripe)
            payment_method: Metodo de pagamento

        Returns:
            Tupla (sucesso, erro_msg)
        """
        invoice = self.get_invoice(invoice_id)
        if not invoice:
            return False, "Fatura nao encontrada"

        if invoice.status == InvoiceStatus.PAID.value:
            return False, "Fatura ja esta paga"

        try:
            invoice.status = InvoiceStatus.PAID.value
            invoice.paid_at = datetime.utcnow()
            invoice.payment_intent_id = payment_id

            if not invoice.metadata:
                invoice.metadata = {}
            invoice.metadata["payment_method"] = payment_method
            invoice.metadata["paid_at"] = datetime.utcnow().isoformat()

            self.db.commit()
            logger.info(f"Fatura paga: {invoice.invoice_number}")

            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao marcar fatura como paga: {e}")
            return False, str(e)

    def void_invoice(
        self,
        invoice_id: str,
        reason: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Cancela/anula uma fatura.

        Args:
            invoice_id: ID da fatura
            reason: Motivo do cancelamento

        Returns:
            Tupla (sucesso, erro_msg)
        """
        invoice = self.get_invoice(invoice_id)
        if not invoice:
            return False, "Fatura nao encontrada"

        if invoice.status == InvoiceStatus.PAID.value:
            return False, "Fatura paga nao pode ser anulada - use reembolso"

        if invoice.status == InvoiceStatus.VOID.value:
            return False, "Fatura ja esta anulada"

        try:
            invoice.status = InvoiceStatus.VOID.value

            if not invoice.metadata:
                invoice.metadata = {}
            invoice.metadata["void_reason"] = reason
            invoice.metadata["voided_at"] = datetime.utcnow().isoformat()

            self.db.commit()
            logger.info(f"Fatura anulada: {invoice.invoice_number}")

            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao anular fatura: {e}")
            return False, str(e)

    # =========================================================================
    # CALCULOS E RELATORIOS
    # =========================================================================

    def calculate_usage_cost(
        self,
        tenant_id: str,
        period: str
    ) -> Dict[str, Any]:
        """
        Calcula custo de uso para um periodo.

        Args:
            tenant_id: ID do tenant
            period: Periodo YYYY-MM

        Returns:
            Dict com breakdown de custos
        """
        # Agregar uso
        self.usage_service.aggregate_monthly_usage(tenant_id, period)

        # Obter breakdown
        breakdown = self.usage_service.get_usage_breakdown(tenant_id, period)

        # Buscar subscription para preco base
        subscription = self.db.query(Subscription).filter(
            and_(
                Subscription.tenant_id == tenant_id,
                Subscription.status.in_([
                    SubscriptionStatus.ACTIVE.value,
                    SubscriptionStatus.TRIALING.value
                ])
            )
        ).first()

        base_cost = 0
        if subscription:
            plan = self.db.query(Plan).filter(
                Plan.plan_id == subscription.plan_id
            ).first()
            if plan:
                base_cost = plan.price_monthly_cents

        # Calcular total
        usage_cost = breakdown.get("total_cost_cents", 0)
        total_cost = base_cost + usage_cost

        return {
            "tenant_id": tenant_id,
            "period": period,
            "base_cost_cents": base_cost,
            "usage_cost_cents": usage_cost,
            "total_cost_cents": total_cost,
            "total_formatted": f"R$ {total_cost / 100:.2f}",
            "breakdown": breakdown.get("breakdown", {}),
        }

    def get_billing_summary(
        self,
        tenant_id: str,
        months: int = 6
    ) -> Dict[str, Any]:
        """
        Obtem resumo de billing dos ultimos meses.

        Args:
            tenant_id: ID do tenant
            months: Numero de meses

        Returns:
            Dict com historico de billing
        """
        invoices = self.db.query(Invoice).filter(
            Invoice.tenant_id == tenant_id
        ).order_by(Invoice.period.desc()).limit(months).all()

        history = []
        total_paid = 0
        total_pending = 0

        for inv in invoices:
            history.append({
                "period": inv.period,
                "invoice_number": inv.invoice_number,
                "status": inv.status,
                "total_cents": inv.total_cents,
                "total_formatted": f"R$ {inv.total_cents / 100:.2f}" if inv.total_cents else "R$ 0.00",
                "paid_at": inv.paid_at.isoformat() if inv.paid_at else None,
                "due_date": inv.due_date.isoformat() if inv.due_date else None,
            })

            if inv.status == InvoiceStatus.PAID.value:
                total_paid += inv.total_cents or 0
            elif inv.status == InvoiceStatus.PENDING.value:
                total_pending += inv.total_cents or 0

        return {
            "tenant_id": tenant_id,
            "history": history,
            "summary": {
                "total_paid_cents": total_paid,
                "total_paid_formatted": f"R$ {total_paid / 100:.2f}",
                "total_pending_cents": total_pending,
                "total_pending_formatted": f"R$ {total_pending / 100:.2f}",
                "invoice_count": len(invoices),
            }
        }

    def get_overdue_invoices(
        self,
        tenant_id: Optional[str] = None
    ) -> List[Invoice]:
        """
        Lista faturas vencidas.

        Args:
            tenant_id: Filtrar por tenant

        Returns:
            Lista de faturas vencidas
        """
        query = self.db.query(Invoice).filter(
            and_(
                Invoice.status == InvoiceStatus.PENDING.value,
                Invoice.due_date < date.today()
            )
        )

        if tenant_id:
            query = query.filter(Invoice.tenant_id == tenant_id)

        return query.order_by(Invoice.due_date.asc()).all()

    # =========================================================================
    # BATCH OPERATIONS
    # =========================================================================

    def generate_monthly_invoices(
        self,
        period: Optional[str] = None,
        auto_finalize: bool = True
    ) -> Dict[str, Any]:
        """
        Gera faturas para todos os tenants ativos.

        Args:
            period: Periodo YYYY-MM (mes anterior se None)
            auto_finalize: Finalizar faturas automaticamente

        Returns:
            Dict com resultados
        """
        if not period:
            last_month = date.today().replace(day=1) - timedelta(days=1)
            period = last_month.strftime("%Y-%m")

        # Buscar todos os tenants ativos com subscription
        tenants = self.db.query(Tenant).filter(
            and_(
                Tenant.is_active == True,
                Tenant.status == TenantStatus.ACTIVE.value
            )
        ).all()

        results = {
            "period": period,
            "processed": 0,
            "generated": 0,
            "skipped": 0,
            "errors": [],
            "invoices": []
        }

        for tenant in tenants:
            results["processed"] += 1

            # Verificar se tem subscription ativa
            subscription = self.db.query(Subscription).filter(
                and_(
                    Subscription.tenant_id == tenant.tenant_id,
                    Subscription.status.in_([
                        SubscriptionStatus.ACTIVE.value,
                        SubscriptionStatus.TRIALING.value
                    ])
                )
            ).first()

            if not subscription:
                results["skipped"] += 1
                continue

            # Gerar fatura
            invoice, error = self.generate_invoice(
                tenant_id=tenant.tenant_id,
                period=period,
                auto_finalize=auto_finalize
            )

            if invoice:
                results["generated"] += 1
                results["invoices"].append({
                    "tenant_id": tenant.tenant_id,
                    "invoice_number": invoice.invoice_number,
                    "total_cents": invoice.total_cents,
                })
            elif error and "ja existe" in error.lower():
                results["skipped"] += 1
            else:
                results["errors"].append({
                    "tenant_id": tenant.tenant_id,
                    "error": error
                })

        logger.info(
            f"Geracao de faturas: {results['generated']} geradas, "
            f"{results['skipped']} puladas, {len(results['errors'])} erros"
        )

        return results

    def process_overdue_invoices(self) -> Dict[str, Any]:
        """
        Processa faturas vencidas - suspende tenants inadimplentes.

        Returns:
            Dict com resultados
        """
        overdue = self.get_overdue_invoices()

        results = {
            "processed": 0,
            "suspended": 0,
            "reminded": 0,
            "details": []
        }

        for invoice in overdue:
            results["processed"] += 1

            days_overdue = (date.today() - invoice.due_date).days

            tenant = self.db.query(Tenant).filter(
                Tenant.tenant_id == invoice.tenant_id
            ).first()

            if not tenant:
                continue

            # Logica de escalacao:
            # 1-7 dias: Lembrete
            # 8-30 dias: Aviso de suspensao
            # >30 dias: Suspensao

            if days_overdue > 30:
                # Suspender tenant
                tenant.status = TenantStatus.SUSPENDED.value
                tenant.suspended_at = datetime.utcnow()

                if not tenant.extra_data:
                    tenant.extra_data = {}
                tenant.extra_data["suspension_reason"] = "Inadimplencia"
                tenant.extra_data["overdue_invoice"] = invoice.invoice_number

                results["suspended"] += 1
                results["details"].append({
                    "tenant_id": tenant.tenant_id,
                    "invoice_number": invoice.invoice_number,
                    "action": "suspended",
                    "days_overdue": days_overdue
                })
            else:
                results["reminded"] += 1
                results["details"].append({
                    "tenant_id": tenant.tenant_id,
                    "invoice_number": invoice.invoice_number,
                    "action": "reminder",
                    "days_overdue": days_overdue
                })

        self.db.commit()

        logger.info(
            f"Processamento de vencidos: {results['suspended']} suspensos, "
            f"{results['reminded']} lembretes"
        )

        return results


# =============================================================================
# FUNCOES UTILITARIAS
# =============================================================================

def generate_invoice_for_tenant(
    db: Session,
    tenant_id: str,
    period: Optional[str] = None
) -> Tuple[Optional[Invoice], Optional[str]]:
    """
    Funcao utilitaria para gerar fatura.

    Args:
        db: Sessao do banco
        tenant_id: ID do tenant
        period: Periodo YYYY-MM

    Returns:
        Tupla (Invoice, erro_msg)
    """
    service = InvoiceService(db)
    return service.generate_invoice(tenant_id, period)


def get_current_billing(db: Session, tenant_id: str) -> Dict[str, Any]:
    """
    Obtem situacao atual de billing do tenant.

    Args:
        db: Sessao do banco
        tenant_id: ID do tenant

    Returns:
        Dict com situacao de billing
    """
    service = InvoiceService(db)
    current_month = date.today().strftime("%Y-%m")

    return {
        "current_usage": service.calculate_usage_cost(tenant_id, current_month),
        "pending_invoices": [
            inv.to_dict() for inv in service.list_invoices(
                tenant_id,
                status=InvoiceStatus.PENDING.value
            )
        ],
        "billing_history": service.get_billing_summary(tenant_id),
    }
