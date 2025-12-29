# -*- coding: utf-8 -*-
"""
Sistema de Cobranca por Uso (Usage-Based Billing) - Issue #121
================================================================

Implementa:
- Integracao com Stripe para cobranca automatica
- Planos: Free, Pro, Enterprise
- Cobranca por excedente (overage)
- Invoices automaticos
- Proration de upgrades/downgrades
- Cupons de desconto
- Recomendacoes de planos

Autor: Fabrica de Agentes
"""

import os
import uuid
import logging
from datetime import datetime, date, timedelta
from typing import Optional, Dict, Any, List, Tuple
from decimal import Decimal

from sqlalchemy.orm import Session
from sqlalchemy import and_, func

# Configurar logging
logger = logging.getLogger(__name__)

# Stripe - carregamento condicional
try:
    import stripe
    STRIPE_AVAILABLE = True
except ImportError:
    stripe = None
    STRIPE_AVAILABLE = False


# =============================================================================
# PLANOS
# =============================================================================

# Definicao de planos disponíveis
BILLING_PLANS = {
    "free": {
        "name": "Free",
        "description": "Para experimentar a plataforma",
        "price_monthly": 0,
        "price_yearly": 0,
        "features": {
            "projects": 1,
            "stories_per_project": 10,
            "agents": 1,
            "workers": 1,
            "storage_mb": 100,
            "api_calls_daily": 100,
            "tokens_monthly": 10000,
            "support": "community",
        },
        "limits": {
            "max_projects": 1,
            "max_stories": 10,
            "max_agents": 1,
            "max_workers": 1,
            "max_storage_bytes": 100 * 1024 * 1024,
            "max_api_requests_daily": 100,
            "max_tokens_monthly": 10000,
        },
        "overage_allowed": False,
    },
    "pro": {
        "name": "Pro",
        "description": "Para equipes pequenas e medias",
        "price_monthly": 9900,  # R$ 99.00
        "price_yearly": 99000,  # R$ 990.00 (2 meses gratis)
        "features": {
            "projects": 10,
            "stories_per_project": 100,
            "agents": 5,
            "workers": 3,
            "storage_gb": 10,
            "api_calls_daily": 10000,
            "tokens_monthly": 500000,
            "support": "email",
            "custom_branding": True,
        },
        "limits": {
            "max_projects": 10,
            "max_stories": 1000,
            "max_agents": 5,
            "max_workers": 3,
            "max_storage_bytes": 10 * 1024 * 1024 * 1024,
            "max_api_requests_daily": 10000,
            "max_tokens_monthly": 500000,
        },
        "overage_allowed": True,
        "overage_rates": {
            "api_calls_per_1k": 10,  # R$ 0.10 por 1000 calls
            "tokens_per_1k": 5,  # R$ 0.05 por 1000 tokens
            "storage_per_gb": 50,  # R$ 0.50 por GB
        },
    },
    "enterprise": {
        "name": "Enterprise",
        "description": "Para grandes organizacoes",
        "price_monthly": 49900,  # R$ 499.00
        "price_yearly": 499000,  # R$ 4990.00 (2 meses gratis)
        "features": {
            "projects": "unlimited",
            "stories_per_project": "unlimited",
            "agents": "unlimited",
            "workers": 10,
            "storage_gb": 100,
            "api_calls_daily": "unlimited",
            "tokens_monthly": 5000000,
            "support": "priority",
            "custom_branding": True,
            "sso": True,
            "dedicated_support": True,
            "sla_99_9": True,
        },
        "limits": {
            "max_projects": None,  # Unlimited
            "max_stories": None,
            "max_agents": None,
            "max_workers": 10,
            "max_storage_bytes": 100 * 1024 * 1024 * 1024,
            "max_api_requests_daily": None,
            "max_tokens_monthly": 5000000,
        },
        "overage_allowed": True,
        "overage_rates": {
            "api_calls_per_1k": 5,  # R$ 0.05 por 1000 calls
            "tokens_per_1k": 3,  # R$ 0.03 por 1000 tokens
            "storage_per_gb": 30,  # R$ 0.30 por GB
        },
    },
}


# =============================================================================
# USAGE BILLING SERVICE
# =============================================================================

class UsageBillingService:
    """
    Servico de cobranca baseada em uso.

    Responsabilidades:
    - Calcular cobranças baseadas em uso
    - Criar invoices automaticamente
    - Integrar com Stripe para pagamentos
    - Gerenciar planos e subscriptions
    - Aplicar cupons de desconto
    - Recomendar planos baseado no uso

    Uso:
        service = UsageBillingService(db_session)

        # Calcular uso do mes
        charges = service.calculate_monthly_charges(tenant_id)

        # Criar invoice
        invoice = service.create_invoice(tenant_id)

        # Processar billing para todos os tenants
        results = service.process_monthly_billing()
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self._init_stripe()

    def _init_stripe(self):
        """Inicializa configuracao do Stripe"""
        if STRIPE_AVAILABLE:
            stripe_key = os.getenv("STRIPE_SECRET_KEY")
            if stripe_key:
                stripe.api_key = stripe_key
                self.stripe_configured = True
            else:
                self.stripe_configured = False
        else:
            self.stripe_configured = False

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    # =========================================================================
    # PLANS
    # =========================================================================

    def get_available_plans(self, include_enterprise: bool = True) -> List[Dict[str, Any]]:
        """
        Lista planos disponiveis.

        Args:
            include_enterprise: Incluir plano enterprise

        Returns:
            Lista de planos
        """
        plans = []
        for plan_id, plan_data in BILLING_PLANS.items():
            if plan_id == "enterprise" and not include_enterprise:
                continue

            plans.append({
                "plan_id": plan_id,
                **plan_data,
                "price_monthly_formatted": f"R$ {plan_data['price_monthly'] / 100:.2f}",
                "price_yearly_formatted": f"R$ {plan_data['price_yearly'] / 100:.2f}",
            })

        return plans

    def get_plan(self, plan_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem detalhes de um plano.

        Args:
            plan_id: ID do plano

        Returns:
            Dict com plano ou None
        """
        plan = BILLING_PLANS.get(plan_id)
        if plan:
            return {"plan_id": plan_id, **plan}
        return None

    # =========================================================================
    # USAGE CALCULATION
    # =========================================================================

    def calculate_monthly_charges(
        self,
        tenant_id: str,
        billing_period: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Calcula cobrancas mensais para um tenant.

        Args:
            tenant_id: ID do tenant
            billing_period: Periodo no formato YYYY-MM (atual se None)

        Returns:
            Dict com breakdown de custos
        """
        from .metering import MeteringService

        if not billing_period:
            billing_period = date.today().strftime("%Y-%m")

        # Obter uso do periodo
        metering = MeteringService(self.db)
        breakdown = metering.get_usage_breakdown(tenant_id, billing_period)

        # Obter plano do tenant
        tenant_plan = self._get_tenant_plan(tenant_id)
        plan_data = BILLING_PLANS.get(tenant_plan, BILLING_PLANS["free"])

        # Base price
        base_price = plan_data["price_monthly"]

        # Calcular overage
        overage_charges = self._calculate_overage(breakdown, plan_data)

        # Total
        total = base_price + overage_charges["total"]

        return {
            "tenant_id": tenant_id,
            "billing_period": billing_period,
            "plan": {
                "plan_id": tenant_plan,
                "name": plan_data["name"],
                "base_price_cents": base_price,
            },
            "usage": breakdown.get("breakdown", {}),
            "overage": overage_charges,
            "total": {
                "base_cents": base_price,
                "overage_cents": overage_charges["total"],
                "total_cents": total,
                "total_formatted": f"R$ {total / 100:.2f}",
            },
            "calculated_at": datetime.utcnow().isoformat(),
        }

    def _calculate_overage(
        self,
        usage_breakdown: Dict[str, Any],
        plan_data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Calcula custos de excedente.

        Args:
            usage_breakdown: Breakdown de uso do metering
            plan_data: Dados do plano

        Returns:
            Dict com overage por recurso
        """
        if not plan_data.get("overage_allowed", False):
            return {"total": 0, "details": {}}

        limits = plan_data.get("limits", {})
        rates = plan_data.get("overage_rates", {})
        breakdown = usage_breakdown.get("breakdown", {})

        overage = {"total": 0, "details": {}}

        # API Calls
        api_used = breakdown.get("api_calls", {}).get("used", 0)
        api_limit = limits.get("max_api_requests_daily", 0)
        if api_limit and api_used > api_limit:
            api_over = api_used - api_limit
            api_cost = int(api_over * rates.get("api_calls_per_1k", 10) / 1000)
            overage["details"]["api_calls"] = {
                "over": api_over,
                "rate_per_1k": rates.get("api_calls_per_1k", 10),
                "cost_cents": api_cost,
            }
            overage["total"] += api_cost

        # Tokens
        tokens_used = breakdown.get("llm_tokens", {}).get("total", 0)
        tokens_limit = limits.get("max_tokens_monthly", 0)
        if tokens_limit and tokens_used > tokens_limit:
            tokens_over = tokens_used - tokens_limit
            tokens_cost = int(tokens_over * rates.get("tokens_per_1k", 5) / 1000)
            overage["details"]["llm_tokens"] = {
                "over": tokens_over,
                "rate_per_1k": rates.get("tokens_per_1k", 5),
                "cost_cents": tokens_cost,
            }
            overage["total"] += tokens_cost

        # Storage
        storage_used_bytes = breakdown.get("storage", {}).get("used_bytes", 0)
        storage_limit_bytes = limits.get("max_storage_bytes", 0)
        if storage_limit_bytes and storage_used_bytes > storage_limit_bytes:
            storage_over_gb = (storage_used_bytes - storage_limit_bytes) / (1024 * 1024 * 1024)
            storage_cost = int(storage_over_gb * rates.get("storage_per_gb", 50))
            overage["details"]["storage"] = {
                "over_gb": round(storage_over_gb, 2),
                "rate_per_gb": rates.get("storage_per_gb", 50),
                "cost_cents": storage_cost,
            }
            overage["total"] += storage_cost

        return overage

    def _get_tenant_plan(self, tenant_id: str) -> str:
        """Obtem plano do tenant"""
        try:
            from .models import Subscription, SubscriptionStatus

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
                return subscription.plan_id

        except Exception as e:
            logger.error(f"Erro ao obter plano do tenant: {e}")

        return "free"

    # =========================================================================
    # INVOICING
    # =========================================================================

    def create_invoice(
        self,
        tenant_id: str,
        billing_period: Optional[str] = None,
        auto_charge: bool = False
    ) -> Tuple[Optional[Dict[str, Any]], Optional[str]]:
        """
        Cria invoice para o tenant.

        Args:
            tenant_id: ID do tenant
            billing_period: Periodo de cobranca
            auto_charge: Cobrar automaticamente

        Returns:
            Tupla (invoice_data, error_message)
        """
        from .models import Invoice, InvoiceStatus

        if not billing_period:
            billing_period = date.today().strftime("%Y-%m")

        # Calcular cobrancas
        charges = self.calculate_monthly_charges(tenant_id, billing_period)

        total_cents = charges["total"]["total_cents"]
        if total_cents <= 0:
            return None, "Nenhum valor a cobrar"

        # Criar invoice
        invoice_id = self._generate_id("INV")
        invoice_number = f"INV-{billing_period.replace('-', '')}-{tenant_id[-8:].upper()}"

        # Montar line items
        line_items = []

        # Item do plano base
        if charges["plan"]["base_price_cents"] > 0:
            line_items.append({
                "description": f"Plano {charges['plan']['name']} - {billing_period}",
                "quantity": 1,
                "unit_amount": charges["plan"]["base_price_cents"],
                "amount": charges["plan"]["base_price_cents"],
            })

        # Items de overage
        for resource, overage_data in charges["overage"].get("details", {}).items():
            if overage_data.get("cost_cents", 0) > 0:
                line_items.append({
                    "description": f"{resource.replace('_', ' ').title()} excedente",
                    "quantity": 1,
                    "unit_amount": overage_data["cost_cents"],
                    "amount": overage_data["cost_cents"],
                })

        # Criar no banco
        try:
            invoice = Invoice(
                invoice_id=invoice_id,
                invoice_number=invoice_number,
                tenant_id=tenant_id,
                status=InvoiceStatus.DRAFT.value,
                subtotal=total_cents,
                discount=0,
                tax=0,
                total=total_cents,
                amount_due=total_cents,
                currency="BRL",
                line_items=line_items,
                period_start=datetime.strptime(f"{billing_period}-01", "%Y-%m-%d"),
                period_end=self._get_period_end(billing_period),
                issue_date=datetime.utcnow(),
                due_date=datetime.utcnow() + timedelta(days=7),
                extra_data={"charges": charges},
            )

            self.db.add(invoice)
            self.db.commit()

            # Criar no Stripe se configurado
            stripe_url = None
            if self.stripe_configured:
                stripe_result = self._create_stripe_invoice(tenant_id, invoice, line_items, auto_charge)
                if stripe_result:
                    invoice.stripe_invoice_id = stripe_result.get("stripe_invoice_id")
                    stripe_url = stripe_result.get("hosted_invoice_url")
                    self.db.commit()

            invoice_data = {
                "invoice_id": invoice_id,
                "invoice_number": invoice_number,
                "tenant_id": tenant_id,
                "billing_period": billing_period,
                "total_cents": total_cents,
                "total_formatted": f"R$ {total_cents / 100:.2f}",
                "status": invoice.status,
                "line_items": line_items,
                "stripe_url": stripe_url,
                "created_at": datetime.utcnow().isoformat(),
            }

            logger.info(f"Invoice criada: {invoice_id} - R$ {total_cents/100:.2f}")
            return invoice_data, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao criar invoice: {e}")
            return None, str(e)

    def _create_stripe_invoice(
        self,
        tenant_id: str,
        invoice,
        line_items: List[Dict],
        auto_charge: bool
    ) -> Optional[Dict[str, Any]]:
        """Cria invoice no Stripe"""
        if not self.stripe_configured:
            return None

        try:
            # Obter customer ID do tenant
            from .models import Tenant
            tenant = self.db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()

            if not tenant or not tenant.stripe_customer_id:
                return None

            # Criar invoice
            stripe_invoice = stripe.Invoice.create(
                customer=tenant.stripe_customer_id,
                auto_advance=True,
                collection_method="charge_automatically" if auto_charge else "send_invoice",
                days_until_due=7 if not auto_charge else None,
                metadata={
                    "tenant_id": tenant_id,
                    "invoice_id": invoice.invoice_id,
                },
            )

            # Adicionar line items
            for item in line_items:
                stripe.InvoiceItem.create(
                    customer=tenant.stripe_customer_id,
                    invoice=stripe_invoice.id,
                    description=item["description"],
                    amount=item["amount"],
                    currency="brl",
                )

            # Finalizar
            stripe_invoice = stripe.Invoice.finalize_invoice(stripe_invoice.id)

            # Cobrar se auto_charge
            if auto_charge:
                try:
                    stripe_invoice = stripe_invoice.pay()
                except Exception as e:
                    logger.warning(f"Falha ao cobrar automaticamente: {e}")

            return {
                "stripe_invoice_id": stripe_invoice.id,
                "hosted_invoice_url": stripe_invoice.hosted_invoice_url,
                "pdf_url": stripe_invoice.invoice_pdf,
                "status": stripe_invoice.status,
            }

        except Exception as e:
            logger.error(f"Erro ao criar Stripe invoice: {e}")
            return None

    def _get_period_end(self, billing_period: str) -> datetime:
        """Calcula fim do periodo"""
        year, month = map(int, billing_period.split("-"))
        if month == 12:
            return datetime(year + 1, 1, 1) - timedelta(seconds=1)
        return datetime(year, month + 1, 1) - timedelta(seconds=1)

    # =========================================================================
    # BATCH PROCESSING
    # =========================================================================

    def process_monthly_billing(
        self,
        auto_charge: bool = True,
        dry_run: bool = False
    ) -> Dict[str, Any]:
        """
        Processa billing mensal para todos os tenants.

        Args:
            auto_charge: Cobrar automaticamente
            dry_run: Se True, apenas simula

        Returns:
            Dict com resultados
        """
        from .models import Tenant, TenantStatus

        # Periodo anterior
        today = date.today()
        if today.month == 1:
            billing_period = f"{today.year - 1}-12"
        else:
            billing_period = f"{today.year}-{today.month - 1:02d}"

        results = {
            "billing_period": billing_period,
            "processed_at": datetime.utcnow().isoformat(),
            "dry_run": dry_run,
            "summary": {
                "tenants_processed": 0,
                "invoices_created": 0,
                "total_amount_cents": 0,
                "errors": 0,
            },
            "invoices": [],
            "errors": [],
        }

        # Buscar tenants ativos
        tenants = self.db.query(Tenant).filter(
            Tenant.status.in_([TenantStatus.ACTIVE.value, TenantStatus.TRIAL.value])
        ).all()

        for tenant in tenants:
            try:
                results["summary"]["tenants_processed"] += 1

                if dry_run:
                    # Apenas calcular
                    charges = self.calculate_monthly_charges(tenant.tenant_id, billing_period)
                    if charges["total"]["total_cents"] > 0:
                        results["invoices"].append({
                            "tenant_id": tenant.tenant_id,
                            "amount_cents": charges["total"]["total_cents"],
                            "dry_run": True,
                        })
                        results["summary"]["invoices_created"] += 1
                        results["summary"]["total_amount_cents"] += charges["total"]["total_cents"]
                else:
                    # Criar invoice
                    invoice_data, error = self.create_invoice(
                        tenant.tenant_id,
                        billing_period,
                        auto_charge
                    )

                    if invoice_data:
                        results["invoices"].append(invoice_data)
                        results["summary"]["invoices_created"] += 1
                        results["summary"]["total_amount_cents"] += invoice_data["total_cents"]

                    if error:
                        results["errors"].append({
                            "tenant_id": tenant.tenant_id,
                            "error": error,
                        })
                        results["summary"]["errors"] += 1

            except Exception as e:
                results["errors"].append({
                    "tenant_id": tenant.tenant_id,
                    "error": str(e),
                })
                results["summary"]["errors"] += 1

        logger.info(
            f"Billing {'simulado' if dry_run else 'processado'}: "
            f"{results['summary']['invoices_created']} invoices, "
            f"R$ {results['summary']['total_amount_cents']/100:.2f}"
        )

        return results

    # =========================================================================
    # PLAN MANAGEMENT
    # =========================================================================

    def upgrade_plan(
        self,
        tenant_id: str,
        new_plan_id: str,
        prorate: bool = True
    ) -> Tuple[bool, Optional[str]]:
        """
        Faz upgrade de plano.

        Args:
            tenant_id: ID do tenant
            new_plan_id: ID do novo plano
            prorate: Aplicar prorata

        Returns:
            Tupla (sucesso, mensagem)
        """
        from .models import Subscription, Plan

        # Validar plano
        new_plan = BILLING_PLANS.get(new_plan_id)
        if not new_plan:
            return False, "Plano nao encontrado"

        # Obter subscription atual
        subscription = self.db.query(Subscription).filter(
            Subscription.tenant_id == tenant_id
        ).first()

        if not subscription:
            return False, "Nenhuma assinatura encontrada"

        old_plan_id = subscription.plan_id
        old_plan = BILLING_PLANS.get(old_plan_id, BILLING_PLANS["free"])

        # Calcular prorata
        proration_amount = 0
        if prorate:
            proration = self.calculate_proration(tenant_id, new_plan_id)
            proration_amount = proration.get("amount_due_cents", 0)

        # Atualizar subscription
        try:
            subscription.plan_id = new_plan_id
            subscription.updated_at = datetime.utcnow()

            # Atualizar no Stripe
            if self.stripe_configured and subscription.stripe_subscription_id:
                self._update_stripe_subscription(subscription, new_plan_id)

            self.db.commit()

            logger.info(f"Tenant {tenant_id} upgrade: {old_plan_id} -> {new_plan_id}")

            return True, f"Upgrade realizado para {new_plan['name']}"

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro no upgrade: {e}")
            return False, str(e)

    def downgrade_plan(
        self,
        tenant_id: str,
        new_plan_id: str
    ) -> Tuple[bool, Optional[str]]:
        """
        Faz downgrade de plano (aplica no proximo ciclo).

        Args:
            tenant_id: ID do tenant
            new_plan_id: ID do novo plano

        Returns:
            Tupla (sucesso, mensagem)
        """
        from .models import Subscription

        # Validar plano
        new_plan = BILLING_PLANS.get(new_plan_id)
        if not new_plan:
            return False, "Plano nao encontrado"

        subscription = self.db.query(Subscription).filter(
            Subscription.tenant_id == tenant_id
        ).first()

        if not subscription:
            return False, "Nenhuma assinatura encontrada"

        try:
            # Agendar downgrade para o fim do ciclo
            if not subscription.extra_data:
                subscription.extra_data = {}

            subscription.extra_data["scheduled_plan_change"] = {
                "new_plan_id": new_plan_id,
                "effective_date": subscription.current_period_end.isoformat() if subscription.current_period_end else None,
                "scheduled_at": datetime.utcnow().isoformat(),
            }

            self.db.commit()

            logger.info(f"Tenant {tenant_id} downgrade agendado para {new_plan_id}")
            return True, f"Downgrade agendado para {new_plan['name']} no proximo ciclo"

        except Exception as e:
            self.db.rollback()
            return False, str(e)

    def calculate_proration(
        self,
        tenant_id: str,
        new_plan_id: str
    ) -> Dict[str, Any]:
        """
        Calcula prorata para mudanca de plano.

        Args:
            tenant_id: ID do tenant
            new_plan_id: ID do novo plano

        Returns:
            Dict com calculo de prorata
        """
        from .models import Subscription

        subscription = self.db.query(Subscription).filter(
            Subscription.tenant_id == tenant_id
        ).first()

        current_plan = BILLING_PLANS.get(
            subscription.plan_id if subscription else "free",
            BILLING_PLANS["free"]
        )
        new_plan = BILLING_PLANS.get(new_plan_id, BILLING_PLANS["free"])

        # Calcular dias restantes
        days_in_month = 30
        days_remaining = days_in_month

        if subscription and subscription.current_period_end:
            delta = subscription.current_period_end - datetime.utcnow()
            days_remaining = max(0, min(days_in_month, delta.days))

        # Calcular credito e debito
        current_daily = current_plan["price_monthly"] / days_in_month
        new_daily = new_plan["price_monthly"] / days_in_month

        credit = int(current_daily * days_remaining)
        debit = int(new_daily * days_remaining)
        amount_due = max(0, debit - credit)

        return {
            "current_plan": {
                "plan_id": subscription.plan_id if subscription else "free",
                "name": current_plan["name"],
                "price_monthly": current_plan["price_monthly"],
            },
            "new_plan": {
                "plan_id": new_plan_id,
                "name": new_plan["name"],
                "price_monthly": new_plan["price_monthly"],
            },
            "proration": {
                "days_remaining": days_remaining,
                "credit_cents": credit,
                "debit_cents": debit,
                "amount_due_cents": amount_due,
                "amount_due_formatted": f"R$ {amount_due / 100:.2f}",
            },
            "monthly_change": {
                "old_price": current_plan["price_monthly"],
                "new_price": new_plan["price_monthly"],
                "difference": new_plan["price_monthly"] - current_plan["price_monthly"],
            },
        }

    def _update_stripe_subscription(self, subscription, new_plan_id: str):
        """Atualiza subscription no Stripe"""
        if not self.stripe_configured:
            return

        try:
            new_plan = BILLING_PLANS.get(new_plan_id)
            if not new_plan:
                return

            # Buscar ou criar Price no Stripe
            stripe_price_id = self._get_or_create_stripe_price(new_plan_id, new_plan)

            # Atualizar subscription
            stripe.Subscription.modify(
                subscription.stripe_subscription_id,
                items=[{
                    "id": subscription.stripe_subscription_item_id,
                    "price": stripe_price_id,
                }],
                proration_behavior="create_prorations",
            )

        except Exception as e:
            logger.error(f"Erro ao atualizar Stripe subscription: {e}")

    def _get_or_create_stripe_price(self, plan_id: str, plan_data: Dict) -> str:
        """Obtem ou cria Price no Stripe"""
        # Implementacao simplificada - em producao usar lookup key
        try:
            # Tentar buscar existente
            prices = stripe.Price.list(
                lookup_keys=[f"plan_{plan_id}_monthly"],
                limit=1
            )

            if prices.data:
                return prices.data[0].id

            # Criar novo
            product = stripe.Product.create(
                name=plan_data["name"],
                description=plan_data["description"],
                metadata={"plan_id": plan_id},
            )

            price = stripe.Price.create(
                product=product.id,
                unit_amount=plan_data["price_monthly"],
                currency="brl",
                recurring={"interval": "month"},
                lookup_key=f"plan_{plan_id}_monthly",
            )

            return price.id

        except Exception as e:
            logger.error(f"Erro ao criar Stripe price: {e}")
            raise

    # =========================================================================
    # COUPONS
    # =========================================================================

    def apply_coupon(
        self,
        tenant_id: str,
        coupon_code: str
    ) -> Tuple[bool, Optional[str]]:
        """
        Aplica cupom de desconto.

        Args:
            tenant_id: ID do tenant
            coupon_code: Codigo do cupom

        Returns:
            Tupla (sucesso, mensagem)
        """
        from .models import Subscription

        # Validar cupom
        coupon = self._validate_coupon(coupon_code)
        if not coupon:
            return False, "Cupom invalido ou expirado"

        subscription = self.db.query(Subscription).filter(
            Subscription.tenant_id == tenant_id
        ).first()

        if not subscription:
            return False, "Nenhuma assinatura encontrada"

        try:
            # Aplicar no Stripe
            if self.stripe_configured and subscription.stripe_subscription_id:
                stripe.Subscription.modify(
                    subscription.stripe_subscription_id,
                    coupon=coupon_code
                )

            # Atualizar local
            subscription.coupon_code = coupon_code
            subscription.discount_percent = coupon.get("percent_off", 0)
            subscription.discount_amount = coupon.get("amount_off", 0)

            if not subscription.extra_data:
                subscription.extra_data = {}
            subscription.extra_data["coupon_applied_at"] = datetime.utcnow().isoformat()

            self.db.commit()

            logger.info(f"Cupom {coupon_code} aplicado ao tenant {tenant_id}")
            return True, f"Cupom aplicado: {coupon.get('name', coupon_code)}"

        except Exception as e:
            self.db.rollback()
            return False, str(e)

    def _validate_coupon(self, coupon_code: str) -> Optional[Dict[str, Any]]:
        """Valida cupom"""
        # Validar no Stripe
        if self.stripe_configured:
            try:
                coupon = stripe.Coupon.retrieve(coupon_code)
                if coupon.valid:
                    return {
                        "id": coupon.id,
                        "name": coupon.name,
                        "percent_off": coupon.percent_off,
                        "amount_off": coupon.amount_off,
                        "duration": coupon.duration,
                    }
            except Exception:
                pass

        # Cupons locais
        local_coupons = {
            "WELCOME20": {"name": "Boas-vindas 20%", "percent_off": 20, "amount_off": 0},
            "ANNUAL50": {"name": "Desconto Anual 50%", "percent_off": 50, "amount_off": 0},
            "PROMO100": {"name": "R$100 de credito", "percent_off": 0, "amount_off": 10000},
        }

        return local_coupons.get(coupon_code.upper())

    # =========================================================================
    # RECOMMENDATIONS
    # =========================================================================

    def get_plan_recommendations(
        self,
        tenant_id: str
    ) -> List[Dict[str, Any]]:
        """
        Recomenda planos baseado no uso.

        Args:
            tenant_id: ID do tenant

        Returns:
            Lista de planos recomendados
        """
        # Obter uso atual
        charges = self.calculate_monthly_charges(tenant_id)
        current_plan_id = self._get_tenant_plan(tenant_id)

        recommendations = []
        overage = charges["overage"]["total"]

        for plan_id, plan_data in BILLING_PLANS.items():
            recommendation = {
                "plan_id": plan_id,
                "name": plan_data["name"],
                "price_monthly": plan_data["price_monthly"],
                "price_formatted": f"R$ {plan_data['price_monthly'] / 100:.2f}",
                "is_current": plan_id == current_plan_id,
                "savings": 0,
                "reasons": [],
                "score": 0,
            }

            # Se tem overage e plano superior poderia eliminar
            if overage > 0:
                current_price = BILLING_PLANS.get(current_plan_id, {}).get("price_monthly", 0)

                if plan_data["price_monthly"] > current_price:
                    # Verificar se overage seria menor
                    potential_savings = overage  # Simplificado
                    upgrade_cost = plan_data["price_monthly"] - current_price

                    if potential_savings > upgrade_cost:
                        recommendation["savings"] = potential_savings - upgrade_cost
                        recommendation["reasons"].append(
                            f"Economia potencial de R$ {(potential_savings - upgrade_cost) / 100:.2f}/mes"
                        )
                        recommendation["score"] = (potential_savings - upgrade_cost) / 100

            if recommendation["is_current"]:
                if overage == 0:
                    recommendation["reasons"].append("Plano adequado ao seu uso")
                    recommendation["score"] = 50
                else:
                    recommendation["reasons"].append("Considere upgrade para reduzir custos")

            recommendations.append(recommendation)

        # Ordenar por score
        recommendations.sort(key=lambda x: x["score"], reverse=True)

        return recommendations

    def get_billing_summary(
        self,
        tenant_id: str,
        include_history: bool = True
    ) -> Dict[str, Any]:
        """
        Obtem resumo de billing do tenant.

        Args:
            tenant_id: ID do tenant
            include_history: Incluir historico de invoices

        Returns:
            Dict com resumo
        """
        from .models import Subscription, Invoice, InvoiceStatus

        # Subscription atual
        subscription = self.db.query(Subscription).filter(
            Subscription.tenant_id == tenant_id
        ).first()

        # Plano
        plan_id = subscription.plan_id if subscription else "free"
        plan_data = BILLING_PLANS.get(plan_id, BILLING_PLANS["free"])

        # Uso atual
        current_charges = self.calculate_monthly_charges(tenant_id)

        summary = {
            "tenant_id": tenant_id,
            "plan": {
                "plan_id": plan_id,
                "name": plan_data["name"],
                "price_monthly": plan_data["price_monthly"],
                "price_formatted": f"R$ {plan_data['price_monthly'] / 100:.2f}",
            },
            "current_period": {
                "period": date.today().strftime("%Y-%m"),
                "base_charges": current_charges["total"]["base_cents"],
                "overage_charges": current_charges["total"]["overage_cents"],
                "estimated_total": current_charges["total"]["total_cents"],
                "estimated_formatted": current_charges["total"]["total_formatted"],
            },
            "subscription": subscription.to_dict() if subscription else None,
        }

        if include_history:
            invoices = self.db.query(Invoice).filter(
                Invoice.tenant_id == tenant_id
            ).order_by(Invoice.created_at.desc()).limit(12).all()

            total_paid = sum(
                i.amount_paid or 0
                for i in invoices
                if i.status == InvoiceStatus.PAID.value
            )

            summary["invoices"] = {
                "recent": [i.to_dict() for i in invoices],
                "total_paid_cents": total_paid,
                "total_paid_formatted": f"R$ {total_paid / 100:.2f}",
            }

        return summary


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "UsageBillingService",
    "BILLING_PLANS",
]
