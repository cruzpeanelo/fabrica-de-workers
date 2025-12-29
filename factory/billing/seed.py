# -*- coding: utf-8 -*-
"""
Seed de Dados para Billing
===========================

Popula o banco de dados com:
- Planos padrao (Starter, Professional, Enterprise)
- Configuracoes iniciais

Uso:
    python -m factory.billing.seed

Autor: Fabrica de Agentes
"""

import logging
from datetime import datetime
from sqlalchemy.orm import Session

from .models import Plan, PlanType

# Configurar logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# PLANOS PADRAO
# =============================================================================

DEFAULT_PLANS = [
    {
        "plan_id": "PLAN-STARTER",
        "name": "Starter",
        "plan_type": PlanType.STARTER.value,
        "description": "Ideal para pequenas equipes e startups. Comece a automatizar seu desenvolvimento com recursos essenciais.",
        "price_monthly": 29900,  # R$ 299,00
        "price_yearly": 287000,  # R$ 2.870,00 (desconto ~20%)
        "currency": "BRL",
        "limits": {
            "max_users": 3,
            "max_projects": 5,
            "max_stories": 100,
            "max_agents": 2,
            "max_tokens_monthly": 10000,
            "max_storage_gb": 1,
            "max_api_requests_daily": 1000,
            "max_jobs_daily": 20
        },
        "features": [
            "kanban_board",
            "basic_stories",
            "basic_reports",
            "email_support",
            "template_mode"
        ],
        "excluded_features": [
            "intelligent_mode",
            "advanced_analytics",
            "custom_integrations",
            "sso_saml",
            "priority_support",
            "api_access",
            "white_label"
        ],
        "trial_days": 14,
        "is_active": True,
        "is_public": True,
        "is_default": True,
        "display_order": 1
    },
    {
        "plan_id": "PLAN-PROFESSIONAL",
        "name": "Professional",
        "plan_type": PlanType.PROFESSIONAL.value,
        "description": "Para equipes em crescimento que precisam de mais poder. Desbloqueie o modo inteligente com Claude AI.",
        "price_monthly": 99900,  # R$ 999,00
        "price_yearly": 959000,  # R$ 9.590,00 (desconto ~20%)
        "currency": "BRL",
        "limits": {
            "max_users": 10,
            "max_projects": 20,
            "max_stories": 500,
            "max_agents": 5,
            "max_tokens_monthly": 100000,
            "max_storage_gb": 10,
            "max_api_requests_daily": 10000,
            "max_jobs_daily": 100
        },
        "features": [
            "kanban_board",
            "basic_stories",
            "advanced_stories",
            "basic_reports",
            "advanced_analytics",
            "email_support",
            "priority_support",
            "template_mode",
            "intelligent_mode",
            "api_access",
            "basic_integrations",
            "code_review_ai"
        ],
        "excluded_features": [
            "custom_integrations",
            "sso_saml",
            "on_premises",
            "white_label",
            "dedicated_support"
        ],
        "trial_days": 14,
        "is_active": True,
        "is_public": True,
        "is_default": False,
        "display_order": 2
    },
    {
        "plan_id": "PLAN-ENTERPRISE",
        "name": "Enterprise",
        "plan_type": PlanType.ENTERPRISE.value,
        "description": "Solucao completa para grandes empresas. Recursos ilimitados, SSO, suporte dedicado e opcao on-premises.",
        "price_monthly": 0,  # Sob consulta
        "price_yearly": 0,
        "currency": "BRL",
        "limits": {
            "max_users": -1,  # -1 = ilimitado
            "max_projects": -1,
            "max_stories": -1,
            "max_agents": -1,
            "max_tokens_monthly": -1,
            "max_storage_gb": -1,
            "max_api_requests_daily": -1,
            "max_jobs_daily": -1
        },
        "features": [
            "kanban_board",
            "basic_stories",
            "advanced_stories",
            "basic_reports",
            "advanced_analytics",
            "custom_reports",
            "email_support",
            "priority_support",
            "dedicated_support",
            "template_mode",
            "intelligent_mode",
            "api_access",
            "basic_integrations",
            "custom_integrations",
            "code_review_ai",
            "sso_saml",
            "on_premises",
            "white_label",
            "audit_logs",
            "compliance_reports",
            "sla_99_9"
        ],
        "excluded_features": [],
        "trial_days": 30,
        "is_active": True,
        "is_public": True,
        "is_default": False,
        "display_order": 3,
        "metadata": {
            "contact_required": True,
            "custom_pricing": True,
            "contact_email": "enterprise@fabricadeagentes.com"
        }
    }
]


def seed_plans(db: Session) -> int:
    """
    Popula os planos padrao no banco de dados.

    Args:
        db: Sessao do banco de dados

    Returns:
        Numero de planos criados/atualizados
    """
    count = 0

    for plan_data in DEFAULT_PLANS:
        plan_id = plan_data["plan_id"]

        # Verificar se ja existe
        existing = db.query(Plan).filter(Plan.plan_id == plan_id).first()

        if existing:
            # Atualizar plano existente
            for key, value in plan_data.items():
                if hasattr(existing, key):
                    setattr(existing, key, value)
            logger.info(f"Plano atualizado: {plan_id}")
        else:
            # Criar novo plano
            plan = Plan(**plan_data)
            db.add(plan)
            logger.info(f"Plano criado: {plan_id}")

        count += 1

    db.commit()
    return count


def seed_demo_tenant(db: Session) -> bool:
    """
    Cria um tenant de demonstracao.

    Args:
        db: Sessao do banco de dados

    Returns:
        True se criado com sucesso
    """
    from .models import Tenant, Subscription, TenantStatus, SubscriptionStatus
    from datetime import timedelta
    import uuid

    # Verificar se ja existe
    existing = db.query(Tenant).filter(Tenant.slug == "demo").first()
    if existing:
        logger.info("Tenant demo ja existe")
        return False

    # Criar tenant demo
    tenant = Tenant(
        tenant_id=f"TEN-DEMO-{uuid.uuid4().hex[:8].upper()}",
        name="Demo Company",
        slug="demo",
        admin_email="demo@fabricadeagentes.com",
        admin_name="Usuario Demo",
        status=TenantStatus.ACTIVE.value,
        is_active=True,
        config={
            "branding": {
                "primary_color": "#003B4A",
                "logo_url": None
            },
            "notifications": {
                "email": True,
                "slack": False
            }
        },
        activated_at=datetime.utcnow()
    )

    db.add(tenant)
    db.flush()

    # Criar assinatura com plano Professional
    plan = db.query(Plan).filter(Plan.plan_id == "PLAN-PROFESSIONAL").first()

    if plan:
        subscription = Subscription(
            subscription_id=f"SUB-DEMO-{uuid.uuid4().hex[:8].upper()}",
            tenant_id=tenant.tenant_id,
            plan_id=plan.plan_id,
            status=SubscriptionStatus.ACTIVE.value,
            billing_period="monthly",
            current_period_start=datetime.utcnow(),
            current_period_end=datetime.utcnow() + timedelta(days=30),
        )
        db.add(subscription)

    db.commit()
    logger.info(f"Tenant demo criado: {tenant.tenant_id}")

    return True


def run_seed(db: Session):
    """
    Executa todo o processo de seed.

    Args:
        db: Sessao do banco de dados
    """
    logger.info("Iniciando seed de billing...")

    # Seed de planos
    plans_count = seed_plans(db)
    logger.info(f"Planos criados/atualizados: {plans_count}")

    # Seed de tenant demo (opcional)
    # seed_demo_tenant(db)

    logger.info("Seed de billing concluido!")


if __name__ == "__main__":
    # Executar seed diretamente
    from ..database.connection import SessionLocal

    db = SessionLocal()
    try:
        run_seed(db)
    finally:
        db.close()
