# -*- coding: utf-8 -*-
"""
Servico de Billing com Integracao Stripe
=========================================

Implementa:
- Criacao de clientes no Stripe
- Gerenciamento de assinaturas
- Processamento de pagamentos
- Webhooks do Stripe
- Checkout sessions
- Portal do cliente

Autor: Fabrica de Agentes
"""

import os
import uuid
import logging
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Tuple
from enum import Enum

try:
    import stripe
    STRIPE_AVAILABLE = True
except ImportError:
    STRIPE_AVAILABLE = False
    stripe = None

from sqlalchemy.orm import Session
from sqlalchemy import select, and_

from .models import (
    Tenant, Plan, Subscription, Invoice, PaymentMethod,
    TenantStatus, SubscriptionStatus, InvoiceStatus,
    PlanType, PaymentMethodType
)

# Configurar logging
logger = logging.getLogger(__name__)


class StripeWebhookEvent(str, Enum):
    """Eventos de webhook do Stripe que processamos"""
    CHECKOUT_COMPLETED = "checkout.session.completed"
    SUBSCRIPTION_CREATED = "customer.subscription.created"
    SUBSCRIPTION_UPDATED = "customer.subscription.updated"
    SUBSCRIPTION_DELETED = "customer.subscription.deleted"
    INVOICE_PAID = "invoice.paid"
    INVOICE_PAYMENT_FAILED = "invoice.payment_failed"
    INVOICE_FINALIZED = "invoice.finalized"
    PAYMENT_METHOD_ATTACHED = "payment_method.attached"
    PAYMENT_METHOD_DETACHED = "payment_method.detached"
    CUSTOMER_UPDATED = "customer.updated"


class BillingService:
    """
    Servico principal de Billing integrado com Stripe.

    Uso:
        service = BillingService(db_session)
        customer = await service.create_customer(tenant)
        subscription = await service.create_subscription(tenant, plan)
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico de billing.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self._init_stripe()

    def _init_stripe(self):
        """Configura a API do Stripe"""
        if not STRIPE_AVAILABLE:
            logger.warning("Stripe nao esta instalado. Execute: pip install stripe")
            return

        self.stripe_api_key = os.getenv("STRIPE_API_KEY", "")
        self.stripe_webhook_secret = os.getenv("STRIPE_WEBHOOK_SECRET", "")

        if self.stripe_api_key:
            stripe.api_key = self.stripe_api_key
            logger.info("Stripe configurado com sucesso")
        else:
            logger.warning("STRIPE_API_KEY nao configurada. Billing em modo simulacao.")

    def _is_stripe_configured(self) -> bool:
        """Verifica se o Stripe esta configurado"""
        return STRIPE_AVAILABLE and bool(self.stripe_api_key)

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    # =========================================================================
    # CUSTOMER / TENANT
    # =========================================================================

    def create_stripe_customer(self, tenant: Tenant) -> Optional[str]:
        """
        Cria um cliente no Stripe para o tenant.

        Args:
            tenant: Objeto Tenant

        Returns:
            stripe_customer_id ou None se falhar
        """
        if not self._is_stripe_configured():
            # Modo simulacao - retorna ID fake
            fake_id = f"cus_sim_{tenant.tenant_id}"
            logger.info(f"[Simulacao] Cliente Stripe criado: {fake_id}")
            return fake_id

        try:
            customer = stripe.Customer.create(
                email=tenant.admin_email,
                name=tenant.name,
                metadata={
                    "tenant_id": tenant.tenant_id,
                    "slug": tenant.slug,
                },
                description=f"Tenant: {tenant.name}",
            )

            # Atualizar tenant com stripe_customer_id
            tenant.stripe_customer_id = customer.id
            self.db.commit()

            logger.info(f"Cliente Stripe criado: {customer.id} para tenant {tenant.tenant_id}")
            return customer.id

        except stripe.error.StripeError as e:
            logger.error(f"Erro ao criar cliente Stripe: {e}")
            self.db.rollback()
            return None

    def update_stripe_customer(self, tenant: Tenant) -> bool:
        """
        Atualiza dados do cliente no Stripe.

        Args:
            tenant: Objeto Tenant com dados atualizados

        Returns:
            True se sucesso
        """
        if not self._is_stripe_configured() or not tenant.stripe_customer_id:
            return True  # Sucesso em modo simulacao

        try:
            stripe.Customer.modify(
                tenant.stripe_customer_id,
                email=tenant.admin_email,
                name=tenant.name,
                metadata={
                    "tenant_id": tenant.tenant_id,
                    "slug": tenant.slug,
                    "legal_name": tenant.legal_name or "",
                    "document": tenant.document or "",
                },
            )
            logger.info(f"Cliente Stripe atualizado: {tenant.stripe_customer_id}")
            return True

        except stripe.error.StripeError as e:
            logger.error(f"Erro ao atualizar cliente Stripe: {e}")
            return False

    # =========================================================================
    # SUBSCRIPTION
    # =========================================================================

    def create_subscription(
        self,
        tenant: Tenant,
        plan: Plan,
        trial_days: Optional[int] = None,
        coupon_code: Optional[str] = None
    ) -> Tuple[Optional[Subscription], Optional[str]]:
        """
        Cria uma assinatura para o tenant.

        Args:
            tenant: Objeto Tenant
            plan: Objeto Plan
            trial_days: Dias de trial (sobrescreve o padrao do plano)
            coupon_code: Codigo de cupom de desconto

        Returns:
            Tupla (Subscription, erro_msg)
        """
        # Verificar se ja tem assinatura ativa
        existing = self.db.query(Subscription).filter(
            and_(
                Subscription.tenant_id == tenant.tenant_id,
                Subscription.status.in_([
                    SubscriptionStatus.ACTIVE.value,
                    SubscriptionStatus.TRIALING.value
                ])
            )
        ).first()

        if existing:
            return None, "Tenant ja possui assinatura ativa"

        # Garantir que tenant tem stripe_customer_id
        if not tenant.stripe_customer_id:
            customer_id = self.create_stripe_customer(tenant)
            if not customer_id:
                return None, "Falha ao criar cliente no Stripe"

        # Criar assinatura no Stripe
        stripe_sub_id = None
        trial_end = None
        period_start = datetime.utcnow()
        period_end = period_start + timedelta(days=30)

        effective_trial_days = trial_days if trial_days is not None else plan.trial_days

        if self._is_stripe_configured() and plan.stripe_price_monthly_id:
            try:
                sub_params = {
                    "customer": tenant.stripe_customer_id,
                    "items": [{"price": plan.stripe_price_monthly_id}],
                    "metadata": {
                        "tenant_id": tenant.tenant_id,
                        "plan_id": plan.plan_id,
                    },
                }

                # Trial
                if effective_trial_days and effective_trial_days > 0:
                    trial_end = datetime.utcnow() + timedelta(days=effective_trial_days)
                    sub_params["trial_end"] = int(trial_end.timestamp())

                # Cupom
                if coupon_code:
                    sub_params["coupon"] = coupon_code

                stripe_sub = stripe.Subscription.create(**sub_params)
                stripe_sub_id = stripe_sub.id
                period_start = datetime.fromtimestamp(stripe_sub.current_period_start)
                period_end = datetime.fromtimestamp(stripe_sub.current_period_end)

                logger.info(f"Assinatura Stripe criada: {stripe_sub_id}")

            except stripe.error.StripeError as e:
                logger.error(f"Erro ao criar assinatura Stripe: {e}")
                return None, f"Erro Stripe: {str(e)}"

        else:
            # Modo simulacao
            stripe_sub_id = f"sub_sim_{self._generate_id('SIM')}"
            if effective_trial_days:
                trial_end = datetime.utcnow() + timedelta(days=effective_trial_days)

        # Criar assinatura no banco
        subscription = Subscription(
            subscription_id=self._generate_id("SUB"),
            tenant_id=tenant.tenant_id,
            plan_id=plan.plan_id,
            status=SubscriptionStatus.TRIALING.value if trial_end else SubscriptionStatus.ACTIVE.value,
            stripe_subscription_id=stripe_sub_id,
            stripe_price_id=plan.stripe_price_monthly_id,
            billing_period="monthly",
            current_period_start=period_start,
            current_period_end=period_end,
            trial_start=datetime.utcnow() if trial_end else None,
            trial_end=trial_end,
            coupon_code=coupon_code,
        )

        try:
            self.db.add(subscription)

            # Atualizar status do tenant
            if trial_end:
                tenant.status = TenantStatus.TRIAL.value
                tenant.trial_started_at = datetime.utcnow()
                tenant.trial_ends_at = trial_end
            else:
                tenant.status = TenantStatus.ACTIVE.value
                tenant.activated_at = datetime.utcnow()

            self.db.commit()
            logger.info(f"Assinatura criada: {subscription.subscription_id}")
            return subscription, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao salvar assinatura: {e}")
            return None, f"Erro ao salvar: {str(e)}"

    def cancel_subscription(
        self,
        subscription: Subscription,
        at_period_end: bool = True,
        reason: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Cancela uma assinatura.

        Args:
            subscription: Objeto Subscription
            at_period_end: Se True, cancela no fim do periodo. Se False, imediatamente.
            reason: Motivo do cancelamento

        Returns:
            Tupla (sucesso, erro_msg)
        """
        if not subscription.is_active():
            return False, "Assinatura nao esta ativa"

        # Cancelar no Stripe
        if self._is_stripe_configured() and subscription.stripe_subscription_id:
            try:
                if at_period_end:
                    stripe.Subscription.modify(
                        subscription.stripe_subscription_id,
                        cancel_at_period_end=True
                    )
                else:
                    stripe.Subscription.cancel(subscription.stripe_subscription_id)

                logger.info(f"Assinatura Stripe cancelada: {subscription.stripe_subscription_id}")

            except stripe.error.StripeError as e:
                logger.error(f"Erro ao cancelar assinatura Stripe: {e}")
                return False, f"Erro Stripe: {str(e)}"

        # Atualizar no banco
        try:
            subscription.canceled_at = datetime.utcnow()
            subscription.cancellation_reason = reason
            subscription.cancel_at_period_end = at_period_end

            if not at_period_end:
                subscription.status = SubscriptionStatus.CANCELED.value
                subscription.ended_at = datetime.utcnow()

                # Atualizar tenant
                tenant = subscription.tenant
                tenant.status = TenantStatus.CANCELLED.value

            self.db.commit()
            logger.info(f"Assinatura cancelada: {subscription.subscription_id}")
            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao cancelar assinatura: {e}")
            return False, str(e)

    def upgrade_subscription(
        self,
        subscription: Subscription,
        new_plan: Plan,
        prorate: bool = True
    ) -> Tuple[Optional[Subscription], Optional[str]]:
        """
        Faz upgrade/downgrade de plano.

        Args:
            subscription: Assinatura atual
            new_plan: Novo plano
            prorate: Se True, calcula pro-rata

        Returns:
            Tupla (Subscription atualizada, erro_msg)
        """
        if not subscription.is_active():
            return None, "Assinatura nao esta ativa"

        if subscription.plan_id == new_plan.plan_id:
            return None, "Mesmo plano atual"

        # Atualizar no Stripe
        if self._is_stripe_configured() and subscription.stripe_subscription_id:
            try:
                stripe_sub = stripe.Subscription.retrieve(subscription.stripe_subscription_id)

                stripe.Subscription.modify(
                    subscription.stripe_subscription_id,
                    items=[{
                        "id": stripe_sub["items"]["data"][0].id,
                        "price": new_plan.stripe_price_monthly_id,
                    }],
                    proration_behavior="create_prorations" if prorate else "none"
                )

                logger.info(f"Assinatura Stripe atualizada para plano: {new_plan.plan_id}")

            except stripe.error.StripeError as e:
                logger.error(f"Erro ao atualizar assinatura Stripe: {e}")
                return None, f"Erro Stripe: {str(e)}"

        # Atualizar no banco
        try:
            old_plan_id = subscription.plan_id
            subscription.plan_id = new_plan.plan_id
            subscription.stripe_price_id = new_plan.stripe_price_monthly_id

            # Registrar mudanca nos metadados
            if not subscription.metadata:
                subscription.metadata = {}
            if "plan_changes" not in subscription.metadata:
                subscription.metadata["plan_changes"] = []
            subscription.metadata["plan_changes"].append({
                "from": old_plan_id,
                "to": new_plan.plan_id,
                "date": datetime.utcnow().isoformat()
            })

            self.db.commit()
            logger.info(f"Assinatura atualizada: {old_plan_id} -> {new_plan.plan_id}")
            return subscription, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao atualizar assinatura: {e}")
            return None, str(e)

    # =========================================================================
    # CHECKOUT / PORTAL
    # =========================================================================

    def create_checkout_session(
        self,
        tenant: Tenant,
        plan: Plan,
        success_url: str,
        cancel_url: str,
        billing_period: str = "monthly"
    ) -> Tuple[Optional[str], Optional[str]]:
        """
        Cria uma sessao de checkout do Stripe.

        Args:
            tenant: Tenant
            plan: Plano selecionado
            success_url: URL de sucesso
            cancel_url: URL de cancelamento
            billing_period: monthly ou yearly

        Returns:
            Tupla (checkout_url, erro_msg)
        """
        if not self._is_stripe_configured():
            # Simular checkout
            simulated_url = f"{success_url}?session_id=sim_{tenant.tenant_id}"
            return simulated_url, None

        # Garantir customer
        if not tenant.stripe_customer_id:
            customer_id = self.create_stripe_customer(tenant)
            if not customer_id:
                return None, "Falha ao criar cliente"

        price_id = (
            plan.stripe_price_yearly_id if billing_period == "yearly"
            else plan.stripe_price_monthly_id
        )

        if not price_id:
            return None, f"Plano {plan.name} nao tem preco configurado no Stripe"

        try:
            session = stripe.checkout.Session.create(
                customer=tenant.stripe_customer_id,
                mode="subscription",
                payment_method_types=["card"],
                line_items=[{
                    "price": price_id,
                    "quantity": 1,
                }],
                success_url=success_url + "?session_id={CHECKOUT_SESSION_ID}",
                cancel_url=cancel_url,
                subscription_data={
                    "trial_period_days": plan.trial_days if plan.trial_days else None,
                    "metadata": {
                        "tenant_id": tenant.tenant_id,
                        "plan_id": plan.plan_id,
                    },
                },
                metadata={
                    "tenant_id": tenant.tenant_id,
                    "plan_id": plan.plan_id,
                },
                allow_promotion_codes=True,
            )

            logger.info(f"Checkout session criada: {session.id}")
            return session.url, None

        except stripe.error.StripeError as e:
            logger.error(f"Erro ao criar checkout session: {e}")
            return None, str(e)

    def create_billing_portal_session(
        self,
        tenant: Tenant,
        return_url: str
    ) -> Tuple[Optional[str], Optional[str]]:
        """
        Cria sessao do portal de billing do Stripe.
        Permite ao cliente gerenciar assinatura, metodos de pagamento, etc.

        Args:
            tenant: Tenant
            return_url: URL de retorno

        Returns:
            Tupla (portal_url, erro_msg)
        """
        if not self._is_stripe_configured():
            return f"{return_url}?portal=simulated", None

        if not tenant.stripe_customer_id:
            return None, "Tenant nao tem customer no Stripe"

        try:
            session = stripe.billing_portal.Session.create(
                customer=tenant.stripe_customer_id,
                return_url=return_url,
            )

            logger.info(f"Portal session criada para tenant: {tenant.tenant_id}")
            return session.url, None

        except stripe.error.StripeError as e:
            logger.error(f"Erro ao criar portal session: {e}")
            return None, str(e)

    # =========================================================================
    # INVOICE
    # =========================================================================

    def create_invoice_from_stripe(
        self,
        stripe_invoice: Dict[str, Any],
        tenant: Tenant
    ) -> Optional[Invoice]:
        """
        Cria um registro de fatura a partir de um invoice do Stripe.

        Args:
            stripe_invoice: Dados do invoice do Stripe
            tenant: Tenant associado

        Returns:
            Invoice criado ou None
        """
        try:
            # Verificar se ja existe
            existing = self.db.query(Invoice).filter(
                Invoice.stripe_invoice_id == stripe_invoice["id"]
            ).first()

            if existing:
                return existing

            # Mapear status
            status_map = {
                "draft": InvoiceStatus.DRAFT.value,
                "open": InvoiceStatus.OPEN.value,
                "paid": InvoiceStatus.PAID.value,
                "void": InvoiceStatus.VOID.value,
                "uncollectible": InvoiceStatus.UNCOLLECTIBLE.value,
            }

            # Construir line items
            line_items = []
            for item in stripe_invoice.get("lines", {}).get("data", []):
                line_items.append({
                    "description": item.get("description", ""),
                    "quantity": item.get("quantity", 1),
                    "unit_amount": item.get("price", {}).get("unit_amount", 0),
                    "amount": item.get("amount", 0),
                })

            invoice = Invoice(
                invoice_id=self._generate_id("INV"),
                invoice_number=stripe_invoice.get("number"),
                tenant_id=tenant.tenant_id,
                stripe_invoice_id=stripe_invoice["id"],
                stripe_payment_intent_id=stripe_invoice.get("payment_intent"),
                status=status_map.get(stripe_invoice["status"], InvoiceStatus.DRAFT.value),
                subtotal=stripe_invoice.get("subtotal", 0),
                discount=stripe_invoice.get("total_discount_amounts", [{}])[0].get("amount", 0) if stripe_invoice.get("total_discount_amounts") else 0,
                tax=stripe_invoice.get("tax", 0),
                total=stripe_invoice.get("total", 0),
                amount_paid=stripe_invoice.get("amount_paid", 0),
                amount_due=stripe_invoice.get("amount_due", 0),
                currency=stripe_invoice.get("currency", "brl").upper(),
                line_items=line_items,
                period_start=datetime.fromtimestamp(stripe_invoice["period_start"]) if stripe_invoice.get("period_start") else None,
                period_end=datetime.fromtimestamp(stripe_invoice["period_end"]) if stripe_invoice.get("period_end") else None,
                issue_date=datetime.fromtimestamp(stripe_invoice["created"]) if stripe_invoice.get("created") else None,
                due_date=datetime.fromtimestamp(stripe_invoice["due_date"]) if stripe_invoice.get("due_date") else None,
                paid_at=datetime.fromtimestamp(stripe_invoice["status_transitions"]["paid_at"]) if stripe_invoice.get("status_transitions", {}).get("paid_at") else None,
                invoice_pdf_url=stripe_invoice.get("invoice_pdf"),
                hosted_invoice_url=stripe_invoice.get("hosted_invoice_url"),
            )

            self.db.add(invoice)
            self.db.commit()

            logger.info(f"Invoice criado: {invoice.invoice_id} do Stripe {stripe_invoice['id']}")
            return invoice

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao criar invoice: {e}")
            return None

    def get_invoices(
        self,
        tenant_id: str,
        status: Optional[str] = None,
        limit: int = 10
    ) -> List[Invoice]:
        """
        Lista faturas de um tenant.

        Args:
            tenant_id: ID do tenant
            status: Filtrar por status
            limit: Limite de resultados

        Returns:
            Lista de Invoices
        """
        query = self.db.query(Invoice).filter(Invoice.tenant_id == tenant_id)

        if status:
            query = query.filter(Invoice.status == status)

        return query.order_by(Invoice.created_at.desc()).limit(limit).all()

    # =========================================================================
    # PAYMENT METHODS
    # =========================================================================

    def sync_payment_methods(self, tenant: Tenant) -> List[PaymentMethod]:
        """
        Sincroniza metodos de pagamento do Stripe.

        Args:
            tenant: Tenant

        Returns:
            Lista de PaymentMethods sincronizados
        """
        if not self._is_stripe_configured() or not tenant.stripe_customer_id:
            return []

        try:
            stripe_methods = stripe.PaymentMethod.list(
                customer=tenant.stripe_customer_id,
                type="card"
            )

            synced = []
            for sm in stripe_methods.data:
                # Verificar se ja existe
                existing = self.db.query(PaymentMethod).filter(
                    PaymentMethod.stripe_payment_method_id == sm.id
                ).first()

                if existing:
                    # Atualizar
                    existing.card_brand = sm.card.brand
                    existing.card_last_four = sm.card.last4
                    existing.card_exp_month = sm.card.exp_month
                    existing.card_exp_year = sm.card.exp_year
                    synced.append(existing)
                else:
                    # Criar novo
                    pm = PaymentMethod(
                        payment_method_id=self._generate_id("PM"),
                        tenant_id=tenant.tenant_id,
                        stripe_payment_method_id=sm.id,
                        method_type=PaymentMethodType.CARD.value,
                        card_brand=sm.card.brand,
                        card_last_four=sm.card.last4,
                        card_exp_month=sm.card.exp_month,
                        card_exp_year=sm.card.exp_year,
                        billing_name=sm.billing_details.name,
                        billing_email=sm.billing_details.email,
                    )
                    self.db.add(pm)
                    synced.append(pm)

            self.db.commit()
            logger.info(f"Sincronizados {len(synced)} metodos de pagamento")
            return synced

        except stripe.error.StripeError as e:
            logger.error(f"Erro ao sincronizar payment methods: {e}")
            return []

    # =========================================================================
    # WEBHOOKS
    # =========================================================================

    def handle_webhook(self, payload: bytes, signature: str) -> Tuple[bool, str]:
        """
        Processa webhook do Stripe.

        Args:
            payload: Corpo da requisicao
            signature: Header Stripe-Signature

        Returns:
            Tupla (sucesso, mensagem)
        """
        if not self._is_stripe_configured():
            return True, "Webhook ignorado (modo simulacao)"

        try:
            event = stripe.Webhook.construct_event(
                payload,
                signature,
                self.stripe_webhook_secret
            )
        except ValueError as e:
            logger.error(f"Payload invalido: {e}")
            return False, "Payload invalido"
        except stripe.error.SignatureVerificationError as e:
            logger.error(f"Assinatura invalida: {e}")
            return False, "Assinatura invalida"

        event_type = event.type
        data = event.data.object

        logger.info(f"Webhook recebido: {event_type}")

        handlers = {
            StripeWebhookEvent.CHECKOUT_COMPLETED.value: self._handle_checkout_completed,
            StripeWebhookEvent.SUBSCRIPTION_CREATED.value: self._handle_subscription_created,
            StripeWebhookEvent.SUBSCRIPTION_UPDATED.value: self._handle_subscription_updated,
            StripeWebhookEvent.SUBSCRIPTION_DELETED.value: self._handle_subscription_deleted,
            StripeWebhookEvent.INVOICE_PAID.value: self._handle_invoice_paid,
            StripeWebhookEvent.INVOICE_PAYMENT_FAILED.value: self._handle_invoice_payment_failed,
        }

        handler = handlers.get(event_type)
        if handler:
            try:
                handler(data)
                return True, f"Evento {event_type} processado"
            except Exception as e:
                logger.error(f"Erro ao processar {event_type}: {e}")
                return False, str(e)
        else:
            logger.info(f"Evento {event_type} ignorado (sem handler)")
            return True, f"Evento {event_type} ignorado"

    def _handle_checkout_completed(self, data: Dict):
        """Processa checkout.session.completed"""
        tenant_id = data.get("metadata", {}).get("tenant_id")
        if not tenant_id:
            return

        tenant = self.db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
        if not tenant:
            logger.warning(f"Tenant nao encontrado: {tenant_id}")
            return

        # Se o checkout incluiu subscription, ela sera criada pelo evento subscription.created
        logger.info(f"Checkout completado para tenant {tenant_id}")

    def _handle_subscription_created(self, data: Dict):
        """Processa customer.subscription.created"""
        tenant_id = data.get("metadata", {}).get("tenant_id")
        plan_id = data.get("metadata", {}).get("plan_id")

        if not tenant_id:
            # Tentar buscar pelo customer
            customer_id = data.get("customer")
            tenant = self.db.query(Tenant).filter(
                Tenant.stripe_customer_id == customer_id
            ).first()
            if tenant:
                tenant_id = tenant.tenant_id

        if not tenant_id:
            logger.warning("Subscription criada sem tenant_id nos metadados")
            return

        tenant = self.db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
        plan = self.db.query(Plan).filter(Plan.plan_id == plan_id).first() if plan_id else None

        if not plan:
            # Buscar pelo stripe_price_id
            price_id = data["items"]["data"][0]["price"]["id"]
            plan = self.db.query(Plan).filter(
                Plan.stripe_price_monthly_id == price_id
            ).first()

        if not plan:
            logger.warning(f"Plano nao encontrado para subscription")
            return

        # Criar subscription no banco se nao existe
        existing = self.db.query(Subscription).filter(
            Subscription.stripe_subscription_id == data["id"]
        ).first()

        if not existing:
            subscription = Subscription(
                subscription_id=self._generate_id("SUB"),
                tenant_id=tenant_id,
                plan_id=plan.plan_id,
                status=data["status"],
                stripe_subscription_id=data["id"],
                stripe_price_id=data["items"]["data"][0]["price"]["id"],
                billing_period="monthly",
                current_period_start=datetime.fromtimestamp(data["current_period_start"]),
                current_period_end=datetime.fromtimestamp(data["current_period_end"]),
            )

            if data.get("trial_start"):
                subscription.trial_start = datetime.fromtimestamp(data["trial_start"])
            if data.get("trial_end"):
                subscription.trial_end = datetime.fromtimestamp(data["trial_end"])

            self.db.add(subscription)

        # Atualizar tenant
        tenant.status = TenantStatus.ACTIVE.value if data["status"] == "active" else TenantStatus.TRIAL.value
        tenant.activated_at = datetime.utcnow()

        self.db.commit()
        logger.info(f"Subscription criada/atualizada para tenant {tenant_id}")

    def _handle_subscription_updated(self, data: Dict):
        """Processa customer.subscription.updated"""
        subscription = self.db.query(Subscription).filter(
            Subscription.stripe_subscription_id == data["id"]
        ).first()

        if not subscription:
            logger.warning(f"Subscription nao encontrada: {data['id']}")
            return

        subscription.status = data["status"]
        subscription.current_period_start = datetime.fromtimestamp(data["current_period_start"])
        subscription.current_period_end = datetime.fromtimestamp(data["current_period_end"])
        subscription.cancel_at_period_end = data.get("cancel_at_period_end", False)

        if data.get("canceled_at"):
            subscription.canceled_at = datetime.fromtimestamp(data["canceled_at"])

        self.db.commit()
        logger.info(f"Subscription atualizada: {subscription.subscription_id}")

    def _handle_subscription_deleted(self, data: Dict):
        """Processa customer.subscription.deleted"""
        subscription = self.db.query(Subscription).filter(
            Subscription.stripe_subscription_id == data["id"]
        ).first()

        if not subscription:
            return

        subscription.status = SubscriptionStatus.CANCELED.value
        subscription.ended_at = datetime.utcnow()

        # Atualizar tenant
        tenant = subscription.tenant
        if tenant:
            tenant.status = TenantStatus.CANCELLED.value

        self.db.commit()
        logger.info(f"Subscription cancelada: {subscription.subscription_id}")

    def _handle_invoice_paid(self, data: Dict):
        """Processa invoice.paid"""
        customer_id = data.get("customer")
        tenant = self.db.query(Tenant).filter(
            Tenant.stripe_customer_id == customer_id
        ).first()

        if not tenant:
            logger.warning(f"Tenant nao encontrado para customer: {customer_id}")
            return

        # Criar/atualizar invoice
        self.create_invoice_from_stripe(data, tenant)

        # Garantir que tenant esta ativo
        if tenant.status == TenantStatus.SUSPENDED.value:
            tenant.status = TenantStatus.ACTIVE.value

        self.db.commit()
        logger.info(f"Invoice paga processada para tenant {tenant.tenant_id}")

    def _handle_invoice_payment_failed(self, data: Dict):
        """Processa invoice.payment_failed"""
        customer_id = data.get("customer")
        tenant = self.db.query(Tenant).filter(
            Tenant.stripe_customer_id == customer_id
        ).first()

        if not tenant:
            return

        # Atualizar invoice se existir
        invoice = self.db.query(Invoice).filter(
            Invoice.stripe_invoice_id == data["id"]
        ).first()

        if invoice:
            invoice.status = InvoiceStatus.OPEN.value

        # Podemos notificar o tenant sobre falha no pagamento
        logger.warning(f"Pagamento falhou para tenant {tenant.tenant_id}")

        self.db.commit()
