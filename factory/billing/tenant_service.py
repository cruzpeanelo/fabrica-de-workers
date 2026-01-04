# -*- coding: utf-8 -*-
"""
Servico de Gerenciamento de Tenants
====================================

Implementa:
- CRUD de tenants
- Onboarding de novos clientes
- Gerenciamento de configuracoes
- Suspensao e reativacao

Autor: Plataforma E
"""

import uuid
import re
import logging
import unicodedata
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Tuple

# Tentar importar python-slugify, se nao disponivel, usar versao simples
try:
    from slugify import slugify
except ImportError:
    def slugify(text: str, lowercase: bool = True, max_length: int = 100) -> str:
        """Versao simplificada de slugify"""
        # Normalizar Unicode
        text = unicodedata.normalize('NFKD', str(text))
        text = text.encode('ascii', 'ignore').decode('ascii')
        # Remover caracteres nao alfanumericos
        text = re.sub(r'[^\w\s-]', '', text)
        # Substituir espacos por hifens
        text = re.sub(r'[\s_]+', '-', text)
        # Remover hifens duplicados
        text = re.sub(r'-+', '-', text).strip('-')
        if lowercase:
            text = text.lower()
        return text[:max_length]

from sqlalchemy.orm import Session
from sqlalchemy import and_, or_

from .models import (
    Tenant, Plan, Subscription,
    TenantStatus, SubscriptionStatus, PlanType
)
from .service import BillingService

# Configurar logging
logger = logging.getLogger(__name__)


class TenantService:
    """
    Servico de gerenciamento de Tenants.

    Uso:
        service = TenantService(db_session)
        tenant = await service.create_tenant(name, email, plan_id)
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self.billing_service = BillingService(db)

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    def _generate_slug(self, name: str) -> str:
        """Gera um slug unico a partir do nome"""
        base_slug = slugify(name, lowercase=True, max_length=50)

        # Verificar se ja existe
        existing = self.db.query(Tenant).filter(
            Tenant.slug == base_slug
        ).first()

        if not existing:
            return base_slug

        # Adicionar sufixo numerico
        counter = 1
        while True:
            new_slug = f"{base_slug}-{counter}"
            existing = self.db.query(Tenant).filter(
                Tenant.slug == new_slug
            ).first()
            if not existing:
                return new_slug
            counter += 1

    def _validate_email(self, email: str) -> bool:
        """Valida formato de email"""
        pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
        return bool(re.match(pattern, email))

    # =========================================================================
    # CRUD
    # =========================================================================

    def create_tenant(
        self,
        name: str,
        admin_email: str,
        plan_id: Optional[str] = None,
        admin_name: Optional[str] = None,
        legal_name: Optional[str] = None,
        document: Optional[str] = None,
        config: Optional[Dict] = None,
        start_trial: bool = True
    ) -> Tuple[Optional[Tenant], Optional[str]]:
        """
        Cria um novo tenant.

        Args:
            name: Nome da empresa/organizacao
            admin_email: Email do administrador
            plan_id: ID do plano (usa padrao se None)
            admin_name: Nome do administrador
            legal_name: Razao social
            document: CNPJ/CPF
            config: Configuracoes customizadas
            start_trial: Se True, inicia trial automaticamente

        Returns:
            Tupla (Tenant, erro_msg)
        """
        # Validacoes
        if not name or len(name) < 2:
            return None, "Nome deve ter pelo menos 2 caracteres"

        if not self._validate_email(admin_email):
            return None, "Email invalido"

        # Verificar email duplicado
        existing = self.db.query(Tenant).filter(
            Tenant.admin_email == admin_email
        ).first()
        if existing:
            return None, "Email ja cadastrado"

        # Buscar plano
        plan = None
        if plan_id:
            plan = self.db.query(Plan).filter(Plan.plan_id == plan_id).first()
            if not plan:
                return None, f"Plano nao encontrado: {plan_id}"
        else:
            # Buscar plano padrao
            plan = self.db.query(Plan).filter(Plan.is_default == True).first()
            if not plan:
                # Buscar Starter como fallback
                plan = self.db.query(Plan).filter(
                    Plan.plan_type == PlanType.STARTER.value
                ).first()

        try:
            # Criar tenant
            tenant = Tenant(
                tenant_id=self._generate_id("TEN"),
                name=name,
                slug=self._generate_slug(name),
                admin_email=admin_email,
                admin_name=admin_name,
                legal_name=legal_name,
                document=document,
                status=TenantStatus.PENDING.value,
                config=config or {},
            )

            self.db.add(tenant)
            self.db.flush()  # Para obter o ID

            # Criar customer no Stripe
            self.billing_service.create_stripe_customer(tenant)

            # Criar assinatura com trial se solicitado
            if plan and start_trial:
                subscription, error = self.billing_service.create_subscription(
                    tenant=tenant,
                    plan=plan,
                    trial_days=plan.trial_days
                )
                if error:
                    logger.warning(f"Falha ao criar assinatura: {error}")

            self.db.commit()
            logger.info(f"Tenant criado: {tenant.tenant_id} ({tenant.name})")

            return tenant, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao criar tenant: {e}")
            return None, str(e)

    def get_tenant(self, tenant_id: str) -> Optional[Tenant]:
        """
        Busca tenant por ID.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tenant ou None
        """
        return self.db.query(Tenant).filter(
            Tenant.tenant_id == tenant_id
        ).first()

    def get_tenant_by_slug(self, slug: str) -> Optional[Tenant]:
        """
        Busca tenant por slug.

        Args:
            slug: Slug do tenant

        Returns:
            Tenant ou None
        """
        return self.db.query(Tenant).filter(
            Tenant.slug == slug
        ).first()

    def get_tenant_by_email(self, email: str) -> Optional[Tenant]:
        """
        Busca tenant por email do admin.

        Args:
            email: Email do administrador

        Returns:
            Tenant ou None
        """
        return self.db.query(Tenant).filter(
            Tenant.admin_email == email
        ).first()

    def list_tenants(
        self,
        status: Optional[str] = None,
        is_active: Optional[bool] = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[Tenant]:
        """
        Lista tenants com filtros.

        Args:
            status: Filtrar por status
            is_active: Filtrar por ativo/inativo
            limit: Limite de resultados
            offset: Offset para paginacao

        Returns:
            Lista de Tenants
        """
        query = self.db.query(Tenant)

        if status:
            query = query.filter(Tenant.status == status)

        if is_active is not None:
            query = query.filter(Tenant.is_active == is_active)

        return query.order_by(Tenant.created_at.desc()).offset(offset).limit(limit).all()

    def update_tenant(
        self,
        tenant_id: str,
        **updates
    ) -> Tuple[Optional[Tenant], Optional[str]]:
        """
        Atualiza dados de um tenant.

        Args:
            tenant_id: ID do tenant
            **updates: Campos a atualizar

        Returns:
            Tupla (Tenant atualizado, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return None, "Tenant nao encontrado"

        # Campos permitidos para atualizacao
        allowed_fields = {
            "name", "legal_name", "document", "admin_email",
            "admin_name", "phone", "address", "config", "custom_limits"
        }

        try:
            for field, value in updates.items():
                if field in allowed_fields:
                    setattr(tenant, field, value)

            # Atualizar slug se nome mudou
            if "name" in updates:
                tenant.slug = self._generate_slug(updates["name"])

            self.db.commit()

            # Sincronizar com Stripe
            self.billing_service.update_stripe_customer(tenant)

            logger.info(f"Tenant atualizado: {tenant_id}")
            return tenant, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao atualizar tenant: {e}")
            return None, str(e)

    def delete_tenant(self, tenant_id: str, hard_delete: bool = False) -> Tuple[bool, Optional[str]]:
        """
        Remove um tenant.

        Args:
            tenant_id: ID do tenant
            hard_delete: Se True, remove permanentemente

        Returns:
            Tupla (sucesso, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return False, "Tenant nao encontrado"

        try:
            if hard_delete:
                # Cancelar assinaturas no Stripe primeiro
                for sub in tenant.subscriptions:
                    if sub.is_active():
                        self.billing_service.cancel_subscription(sub, at_period_end=False)

                self.db.delete(tenant)
                logger.warning(f"Tenant deletado permanentemente: {tenant_id}")
            else:
                # Soft delete - apenas desativar
                tenant.is_active = False
                tenant.status = TenantStatus.CANCELLED.value

                # Cancelar assinaturas
                for sub in tenant.subscriptions:
                    if sub.is_active():
                        self.billing_service.cancel_subscription(sub, at_period_end=False)

                logger.info(f"Tenant desativado: {tenant_id}")

            self.db.commit()
            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao deletar tenant: {e}")
            return False, str(e)

    # =========================================================================
    # SUSPENSAO E REATIVACAO
    # =========================================================================

    def suspend_tenant(
        self,
        tenant_id: str,
        reason: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Suspende um tenant (por falta de pagamento, abuso, etc).

        Args:
            tenant_id: ID do tenant
            reason: Motivo da suspensao

        Returns:
            Tupla (sucesso, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return False, "Tenant nao encontrado"

        if tenant.status == TenantStatus.SUSPENDED.value:
            return False, "Tenant ja esta suspenso"

        try:
            tenant.status = TenantStatus.SUSPENDED.value
            tenant.suspended_at = datetime.utcnow()

            # Registrar motivo nos metadados
            if not tenant.extra_data:
                tenant.extra_data = {}
            tenant.extra_data["suspension_reason"] = reason
            tenant.extra_data["suspended_at"] = datetime.utcnow().isoformat()

            self.db.commit()
            logger.warning(f"Tenant suspenso: {tenant_id} - {reason}")

            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao suspender tenant: {e}")
            return False, str(e)

    def reactivate_tenant(self, tenant_id: str) -> Tuple[bool, Optional[str]]:
        """
        Reativa um tenant suspenso.

        Args:
            tenant_id: ID do tenant

        Returns:
            Tupla (sucesso, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return False, "Tenant nao encontrado"

        if tenant.status != TenantStatus.SUSPENDED.value:
            return False, "Tenant nao esta suspenso"

        try:
            # Verificar se tem assinatura ativa
            active_sub = self.db.query(Subscription).filter(
                and_(
                    Subscription.tenant_id == tenant_id,
                    Subscription.status.in_([
                        SubscriptionStatus.ACTIVE.value,
                        SubscriptionStatus.TRIALING.value
                    ])
                )
            ).first()

            if active_sub:
                tenant.status = TenantStatus.ACTIVE.value
            else:
                tenant.status = TenantStatus.PENDING.value

            tenant.suspended_at = None

            # Registrar reativacao
            if not tenant.extra_data:
                tenant.extra_data = {}
            tenant.extra_data["reactivated_at"] = datetime.utcnow().isoformat()

            self.db.commit()
            logger.info(f"Tenant reativado: {tenant_id}")

            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao reativar tenant: {e}")
            return False, str(e)

    # =========================================================================
    # ONBOARDING
    # =========================================================================

    def complete_onboarding(
        self,
        tenant_id: str,
        onboarding_data: Dict[str, Any]
    ) -> Tuple[bool, Optional[str]]:
        """
        Completa o processo de onboarding de um tenant.

        Args:
            tenant_id: ID do tenant
            onboarding_data: Dados do onboarding

        Returns:
            Tupla (sucesso, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return False, "Tenant nao encontrado"

        try:
            # Atualizar dados do tenant
            if "company_info" in onboarding_data:
                company = onboarding_data["company_info"]
                tenant.legal_name = company.get("legal_name", tenant.legal_name)
                tenant.document = company.get("document", tenant.document)
                tenant.phone = company.get("phone", tenant.phone)
                tenant.address = company.get("address", tenant.address)

            if "admin_info" in onboarding_data:
                admin = onboarding_data["admin_info"]
                tenant.admin_name = admin.get("name", tenant.admin_name)

            if "preferences" in onboarding_data:
                prefs = onboarding_data["preferences"]
                if not tenant.config:
                    tenant.config = {}
                tenant.config.update(prefs)

            # Marcar onboarding como completo
            if not tenant.extra_data:
                tenant.extra_data = {}
            tenant.extra_data["onboarding_completed"] = True
            tenant.extra_data["onboarding_completed_at"] = datetime.utcnow().isoformat()

            self.db.commit()

            # Sincronizar com Stripe
            self.billing_service.update_stripe_customer(tenant)

            logger.info(f"Onboarding completo para tenant: {tenant_id}")
            return True, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro no onboarding: {e}")
            return False, str(e)

    # =========================================================================
    # CONFIGURACOES
    # =========================================================================

    def update_config(
        self,
        tenant_id: str,
        config_updates: Dict[str, Any]
    ) -> Tuple[Optional[Dict], Optional[str]]:
        """
        Atualiza configuracoes do tenant.

        Args:
            tenant_id: ID do tenant
            config_updates: Configuracoes a atualizar

        Returns:
            Tupla (config atualizado, erro_msg)
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return None, "Tenant nao encontrado"

        try:
            if not tenant.config:
                tenant.config = {}

            # Merge das configuracoes
            def deep_merge(base: Dict, updates: Dict) -> Dict:
                for key, value in updates.items():
                    if key in base and isinstance(base[key], dict) and isinstance(value, dict):
                        deep_merge(base[key], value)
                    else:
                        base[key] = value
                return base

            deep_merge(tenant.config, config_updates)

            self.db.commit()
            logger.info(f"Config atualizado para tenant: {tenant_id}")

            return tenant.config, None

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao atualizar config: {e}")
            return None, str(e)

    def get_config(self, tenant_id: str, key: Optional[str] = None) -> Optional[Any]:
        """
        Obtem configuracao do tenant.

        Args:
            tenant_id: ID do tenant
            key: Chave especifica (opcional)

        Returns:
            Configuracao ou None
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return None

        config = tenant.config or {}

        if key:
            return config.get(key)

        return config

    # =========================================================================
    # ESTATISTICAS
    # =========================================================================

    def get_tenant_stats(self, tenant_id: str) -> Dict[str, Any]:
        """
        Obtem estatisticas do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dicionario com estatisticas
        """
        tenant = self.get_tenant(tenant_id)
        if not tenant:
            return {}

        # Buscar assinatura ativa
        active_sub = self.db.query(Subscription).filter(
            and_(
                Subscription.tenant_id == tenant_id,
                Subscription.status.in_([
                    SubscriptionStatus.ACTIVE.value,
                    SubscriptionStatus.TRIALING.value
                ])
            )
        ).first()

        # Buscar plano
        plan = None
        if active_sub:
            plan = self.db.query(Plan).filter(
                Plan.plan_id == active_sub.plan_id
            ).first()

        return {
            "tenant_id": tenant_id,
            "name": tenant.name,
            "status": tenant.status,
            "is_active": tenant.is_active,
            "created_at": tenant.created_at.isoformat() if tenant.created_at else None,
            "days_since_creation": (datetime.utcnow() - tenant.created_at).days if tenant.created_at else 0,
            "subscription": {
                "id": active_sub.subscription_id if active_sub else None,
                "status": active_sub.status if active_sub else None,
                "plan": plan.name if plan else None,
                "plan_type": plan.plan_type if plan else None,
                "trial_ends": active_sub.trial_end.isoformat() if active_sub and active_sub.trial_end else None,
                "is_trial": active_sub.is_in_trial() if active_sub else False,
                "days_until_renewal": active_sub.days_until_renewal() if active_sub else None,
            } if active_sub else None,
            "plan_limits": plan.limits if plan else {},
            "custom_limits": tenant.custom_limits or {},
        }
