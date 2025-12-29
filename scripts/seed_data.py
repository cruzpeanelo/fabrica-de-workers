# -*- coding: utf-8 -*-
"""
Seed Data Script for Multi-Tenancy Development
===============================================

Issue #124 - Dados de Teste e Seed para Multi-tenancy

This script creates test data for development and testing:
- Test tenants (demo, enterprise, free)
- Users per tenant
- Sample projects and stories
- Billing data

Usage:
    python scripts/seed_data.py
    python scripts/seed_data.py --clean  # Clear and reseed
    python scripts/seed_data.py --tenant demo  # Seed specific tenant
"""

import os
import sys
import argparse
import hashlib
import secrets
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Any, Optional
from decimal import Decimal
import uuid

# Add project root to path
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Import database dependencies
try:
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker
    from factory.database.connection import Base, get_db
    from factory.database.models import (
        User, Project, Job, Story, StoryTask, Epic, Sprint,
        Worker, Task
    )
    from factory.database.tenant_models import (
        Tenant, TenantSettings, BrandingConfig, TenantMember,
        TenantStatus, TenantPlan, MemberRole
    )
    from factory.billing.models import (
        Plan, Subscription, Invoice, PaymentMethod, Usage,
        PlanType, SubscriptionStatus, InvoiceStatus, PaymentMethodType
    )
    from factory.config import DATABASE_URL
except ImportError as e:
    print(f"Error importing dependencies: {e}")
    print("Make sure you're in the project root and dependencies are installed.")
    sys.exit(1)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Password hashing (simple for development - use bcrypt in production)
def hash_password(password: str) -> str:
    """Simple password hash for development."""
    return hashlib.sha256(password.encode()).hexdigest()


def generate_id(prefix: str) -> str:
    """Generate unique ID with prefix."""
    return f"{prefix}-{uuid.uuid4().hex[:8].upper()}"


# =============================================================================
# TEST DATA DEFINITIONS
# =============================================================================

TEST_TENANTS = [
    {
        "tenant_id": "DEMO-001",
        "name": "Demo Company",
        "slug": "demo",
        "plan": TenantPlan.PROFESSIONAL.value,
        "status": TenantStatus.ACTIVE.value,
        "email": "admin@demo.fabricadeagentes.com",
        "users": [
            {"username": "demo_admin", "email": "admin@demo.com", "role": "ADMIN", "password": "demo123"},
            {"username": "demo_dev", "email": "dev@demo.com", "role": "DEVELOPER", "password": "demo123"},
            {"username": "demo_viewer", "email": "viewer@demo.com", "role": "VIEWER", "password": "demo123"},
        ],
        "branding": {
            "primary_color": "#003B4A",
            "secondary_color": "#FF6C00",
            "logo_url": "/static/logos/demo-logo.png",
            "display_name": "Demo Company Platform",
        },
        "settings": {
            "max_projects": 20,
            "max_stories_per_project": 200,
            "max_members": 10,
            "max_tokens_per_month": 500000,
        }
    },
    {
        "tenant_id": "ENT-001",
        "name": "Enterprise Corp",
        "slug": "enterprise",
        "plan": TenantPlan.ENTERPRISE.value,
        "status": TenantStatus.ACTIVE.value,
        "email": "admin@enterprise.example.com",
        "users": [
            {"username": "ent_admin", "email": "admin@enterprise.com", "role": "ADMIN", "password": "ent123"},
            {"username": "ent_lead", "email": "lead@enterprise.com", "role": "ADMIN", "password": "ent123"},
            {"username": "ent_dev1", "email": "dev1@enterprise.com", "role": "DEVELOPER", "password": "ent123"},
            {"username": "ent_dev2", "email": "dev2@enterprise.com", "role": "DEVELOPER", "password": "ent123"},
            {"username": "ent_dev3", "email": "dev3@enterprise.com", "role": "DEVELOPER", "password": "ent123"},
            {"username": "ent_qa", "email": "qa@enterprise.com", "role": "TESTER", "password": "ent123"},
        ],
        "branding": {
            "primary_color": "#1E3A5F",
            "secondary_color": "#00A3E0",
            "logo_url": "/static/logos/enterprise-logo.png",
            "display_name": "Enterprise Development Hub",
        },
        "settings": {
            "max_projects": 100,
            "max_stories_per_project": 1000,
            "max_members": 50,
            "max_tokens_per_month": 5000000,
        }
    },
    {
        "tenant_id": "FREE-001",
        "name": "Free Tier User",
        "slug": "free",
        "plan": TenantPlan.FREE.value,
        "status": TenantStatus.TRIAL.value,
        "email": "user@free.example.com",
        "trial_ends_at": datetime.utcnow() + timedelta(days=14),
        "users": [
            {"username": "free_user", "email": "user@free.com", "role": "ADMIN", "password": "free123"},
        ],
        "branding": None,  # Uses default branding
        "settings": {
            "max_projects": 3,
            "max_stories_per_project": 50,
            "max_members": 2,
            "max_tokens_per_month": 10000,
        }
    },
]

TEST_PLANS = [
    {
        "plan_id": "PLAN-FREE",
        "name": "Free",
        "plan_type": PlanType.STARTER.value,
        "price_monthly": 0,  # Free
        "price_yearly": 0,
        "limits": {
            "max_users": 2,
            "max_projects": 3,
            "max_stories": 50,
            "max_agents": 1,
            "max_tokens_monthly": 10000,
            "max_storage_gb": 0.5,
            "max_api_requests_daily": 100
        },
        "features": ["kanban_board", "basic_stories", "email_support"],
        "trial_days": 0,
        "is_public": True,
        "is_default": True,
    },
    {
        "plan_id": "PLAN-STARTER",
        "name": "Starter",
        "plan_type": PlanType.STARTER.value,
        "price_monthly": 29900,  # R$ 299,00
        "price_yearly": 287040,  # R$ 2.870,40 (20% desconto)
        "limits": {
            "max_users": 5,
            "max_projects": 10,
            "max_stories": 200,
            "max_agents": 2,
            "max_tokens_monthly": 100000,
            "max_storage_gb": 5,
            "max_api_requests_daily": 1000
        },
        "features": [
            "kanban_board", "stories", "sprints", "epics",
            "basic_reports", "api_access", "email_support"
        ],
        "trial_days": 14,
        "is_public": True,
    },
    {
        "plan_id": "PLAN-PROFESSIONAL",
        "name": "Professional",
        "plan_type": PlanType.PROFESSIONAL.value,
        "price_monthly": 99900,  # R$ 999,00
        "price_yearly": 958080,  # R$ 9.580,80 (20% desconto)
        "limits": {
            "max_users": 20,
            "max_projects": 50,
            "max_stories": 1000,
            "max_agents": 5,
            "max_tokens_monthly": 500000,
            "max_storage_gb": 50,
            "max_api_requests_daily": 10000
        },
        "features": [
            "kanban_board", "stories", "sprints", "epics",
            "advanced_reports", "api_access", "webhooks",
            "custom_fields", "audit_logs", "priority_support"
        ],
        "trial_days": 14,
        "is_public": True,
    },
    {
        "plan_id": "PLAN-ENTERPRISE",
        "name": "Enterprise",
        "plan_type": PlanType.ENTERPRISE.value,
        "price_monthly": 299900,  # R$ 2.999,00
        "price_yearly": 2879040,  # R$ 28.790,40 (20% desconto)
        "limits": {
            "max_users": 0,  # Unlimited
            "max_projects": 0,  # Unlimited
            "max_stories": 0,  # Unlimited
            "max_agents": 0,  # Unlimited
            "max_tokens_monthly": 0,  # Unlimited
            "max_storage_gb": 0,  # Unlimited
            "max_api_requests_daily": 0  # Unlimited
        },
        "features": [
            "kanban_board", "stories", "sprints", "epics",
            "advanced_reports", "api_access", "webhooks",
            "custom_fields", "audit_logs", "sso", "sla",
            "dedicated_support", "custom_branding", "on_premise"
        ],
        "trial_days": 30,
        "is_public": True,
    },
]

# Additional test tenants for multi-tenancy testing (Issue #124)
ADDITIONAL_TEST_TENANTS = [
    {
        "tenant_id": "STARTUP-001",
        "name": "Tech Startup Inc",
        "slug": "startup",
        "plan": TenantPlan.STARTER.value,
        "status": TenantStatus.ACTIVE.value,
        "email": "admin@techstartup.io",
        "users": [
            {"username": "startup_founder", "email": "founder@techstartup.io", "role": "ADMIN", "password": "startup123"},
            {"username": "startup_dev", "email": "dev@techstartup.io", "role": "DEVELOPER", "password": "startup123"},
        ],
        "branding": {
            "primary_color": "#6366F1",
            "secondary_color": "#EC4899",
            "logo_url": "/static/logos/startup-logo.png",
            "display_name": "Tech Startup Platform",
        },
        "settings": {
            "max_projects": 10,
            "max_stories_per_project": 200,
            "max_members": 5,
            "max_tokens_per_month": 100000,
        }
    },
    {
        "tenant_id": "AGENCY-001",
        "name": "Digital Agency",
        "slug": "agency",
        "plan": TenantPlan.PROFESSIONAL.value,
        "status": TenantStatus.ACTIVE.value,
        "email": "admin@digitalagency.co",
        "users": [
            {"username": "agency_admin", "email": "admin@digitalagency.co", "role": "ADMIN", "password": "agency123"},
            {"username": "agency_pm", "email": "pm@digitalagency.co", "role": "ADMIN", "password": "agency123"},
            {"username": "agency_dev1", "email": "dev1@digitalagency.co", "role": "DEVELOPER", "password": "agency123"},
        ],
        "branding": {
            "primary_color": "#059669",
            "secondary_color": "#F59E0B",
            "logo_url": "/static/logos/agency-logo.png",
            "display_name": "Digital Agency Hub",
        },
        "settings": {
            "max_projects": 50,
            "max_stories_per_project": 500,
            "max_members": 20,
            "max_tokens_per_month": 500000,
        }
    },
    {
        "tenant_id": "TRIAL-001",
        "name": "Trial User Company",
        "slug": "trial",
        "plan": TenantPlan.PROFESSIONAL.value,
        "status": TenantStatus.TRIAL.value,
        "email": "trial@example.com",
        "trial_ends_at": datetime.utcnow() + timedelta(days=7),
        "users": [
            {"username": "trial_user", "email": "trial@example.com", "role": "ADMIN", "password": "trial123"},
        ],
        "branding": None,
        "settings": {
            "max_projects": 5,
            "max_stories_per_project": 100,
            "max_members": 3,
            "max_tokens_per_month": 50000,
        }
    },
]

SAMPLE_PROJECTS = [
    {
        "name": "E-commerce Platform",
        "description": "Full-featured e-commerce platform with product catalog, cart, and checkout",
        "project_type": "web-app",
        "stories": [
            {
                "title": "User Registration and Authentication",
                "persona": "new customer",
                "action": "register and login to the platform",
                "benefit": "I can track my orders and save my preferences",
                "story_points": 8,
                "tasks": [
                    {"title": "Create user registration form", "task_type": "development"},
                    {"title": "Implement JWT authentication", "task_type": "development"},
                    {"title": "Add password reset functionality", "task_type": "development"},
                    {"title": "Write unit tests for auth", "task_type": "test"},
                ]
            },
            {
                "title": "Product Catalog",
                "persona": "customer",
                "action": "browse products by category and search",
                "benefit": "I can easily find products I want to buy",
                "story_points": 13,
                "tasks": [
                    {"title": "Design product listing page", "task_type": "design"},
                    {"title": "Implement product search", "task_type": "development"},
                    {"title": "Add category filters", "task_type": "development"},
                    {"title": "Optimize database queries", "task_type": "development"},
                ]
            },
            {
                "title": "Shopping Cart",
                "persona": "customer",
                "action": "add products to cart and manage quantities",
                "benefit": "I can collect items before checkout",
                "story_points": 5,
                "tasks": [
                    {"title": "Create cart component", "task_type": "development"},
                    {"title": "Implement cart persistence", "task_type": "development"},
                ]
            },
        ]
    },
    {
        "name": "Internal Dashboard",
        "description": "Analytics dashboard for internal KPI tracking",
        "project_type": "data-analysis",
        "stories": [
            {
                "title": "Sales Dashboard",
                "persona": "sales manager",
                "action": "view real-time sales metrics",
                "benefit": "I can make data-driven decisions",
                "story_points": 8,
                "tasks": [
                    {"title": "Design dashboard layout", "task_type": "design"},
                    {"title": "Create sales API endpoints", "task_type": "development"},
                    {"title": "Build chart components", "task_type": "development"},
                ]
            },
            {
                "title": "Export Reports",
                "persona": "manager",
                "action": "export reports to PDF and Excel",
                "benefit": "I can share reports with stakeholders",
                "story_points": 5,
                "tasks": [
                    {"title": "Implement PDF generation", "task_type": "development"},
                    {"title": "Implement Excel export", "task_type": "development"},
                ]
            },
        ]
    },
]


# =============================================================================
# SEEDING FUNCTIONS
# =============================================================================

class DataSeeder:
    """Main class for seeding test data."""

    def __init__(self, db_url: str = None):
        """Initialize seeder with database connection."""
        self.db_url = db_url or DATABASE_URL
        self.engine = create_engine(self.db_url)
        self.Session = sessionmaker(bind=self.engine)
        self.session = None
        self.created_users: Dict[str, User] = {}
        self.created_tenants: Dict[str, Tenant] = {}
        self.created_projects: Dict[str, Project] = {}

    def __enter__(self):
        """Context manager entry."""
        self.session = self.Session()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        if exc_type:
            self.session.rollback()
        else:
            self.session.commit()
        self.session.close()

    def clean_data(self):
        """Remove all seeded test data."""
        print("Cleaning existing test data...")

        # Delete in order of dependencies
        tables_to_clean = [
            StoryTask, Story, Task, Job, Project,
            TenantMember, TenantSettings, BrandingConfig, Tenant,
            Invoice, Subscription, PaymentMethod, Usage, Plan,
            User
        ]

        for table in tables_to_clean:
            try:
                self.session.query(table).delete()
                print(f"  - Cleaned {table.__tablename__}")
            except Exception as e:
                print(f"  - Warning cleaning {table.__tablename__}: {e}")

        self.session.commit()
        print("Cleanup complete!\n")

    def seed_plans(self) -> List[Plan]:
        """Create subscription plans."""
        print("Seeding Plans...")
        plans = []

        for plan_data in TEST_PLANS:
            plan = Plan(
                plan_id=plan_data["plan_id"],
                name=plan_data["name"],
                plan_type=plan_data["plan_type"],
                price_monthly=plan_data["price_monthly"],
                price_yearly=plan_data.get("price_yearly"),
                limits=plan_data["limits"],
                features=plan_data["features"],
                trial_days=plan_data.get("trial_days", 14),
                is_public=plan_data.get("is_public", True),
                is_default=plan_data.get("is_default", False),
                is_active=True,
            )
            self.session.add(plan)
            plans.append(plan)
            print(f"  + Plan: {plan.name} (R$ {plan.price_monthly/100:.2f}/mo)")

        self.session.flush()
        return plans

    def seed_tenant(self, tenant_data: dict) -> Tenant:
        """Create a single tenant with all related data."""
        print(f"\nSeeding Tenant: {tenant_data['name']}...")

        # Create tenant
        tenant = Tenant(
            tenant_id=tenant_data["tenant_id"],
            name=tenant_data["name"],
            slug=tenant_data["slug"],
            status=tenant_data["status"],
            plan=tenant_data["plan"],
            email=tenant_data["email"],
            trial_ends_at=tenant_data.get("trial_ends_at"),
            activated_at=datetime.utcnow() if tenant_data["status"] == TenantStatus.ACTIVE.value else None,
        )
        self.session.add(tenant)
        self.session.flush()
        self.created_tenants[tenant.tenant_id] = tenant
        print(f"  + Tenant: {tenant.name} [{tenant.plan}]")

        # Create tenant settings
        settings_data = tenant_data.get("settings", {})
        settings = TenantSettings(
            tenant_id=tenant.tenant_id,
            max_projects=settings_data.get("max_projects", 10),
            max_stories_per_project=settings_data.get("max_stories_per_project", 100),
            max_members=settings_data.get("max_members", 5),
            max_tokens_per_month=settings_data.get("max_tokens_per_month", 100000),
        )
        self.session.add(settings)
        print(f"  + Settings: max_projects={settings.max_projects}")

        # Create branding if provided
        branding_data = tenant_data.get("branding")
        if branding_data:
            branding = BrandingConfig(
                tenant_id=tenant.tenant_id,
                display_name=branding_data.get("display_name"),
                logo_url=branding_data.get("logo_url"),
                colors={
                    "primary": branding_data.get("primary_color", "#003B4A"),
                    "secondary": branding_data.get("secondary_color", "#FF6C00"),
                }
            )
            self.session.add(branding)
            print(f"  + Branding: {branding.display_name}")

        # Create users and members
        for user_data in tenant_data.get("users", []):
            user = self._create_user(user_data)
            member = TenantMember(
                tenant_id=tenant.tenant_id,
                user_id=user.id,
                tenant_role=MemberRole.OWNER.value if user_data["role"] == "ADMIN" else MemberRole.MEMBER.value,
            )
            self.session.add(member)
            print(f"  + User: {user.username} ({user_data['role']})")

        self.session.flush()
        return tenant

    def _create_user(self, user_data: dict) -> User:
        """Create a single user."""
        user = User(
            username=user_data["username"],
            email=user_data["email"],
            password_hash=hash_password(user_data["password"]),
            role=user_data.get("role", "MEMBER"),
            active=True,
        )
        self.session.add(user)
        self.session.flush()
        self.created_users[user.username] = user
        return user

    def seed_projects(self, tenant_id: str) -> List[Project]:
        """Create sample projects for a tenant."""
        print(f"\n  Seeding Projects for {tenant_id}...")
        projects = []

        for proj_data in SAMPLE_PROJECTS:
            project_id = generate_id("PROJ")
            project = Project(
                project_id=project_id,
                tenant_id=tenant_id,
                name=proj_data["name"],
                description=proj_data["description"],
                project_type=proj_data["project_type"],
                status="IN_PROGRESS",
                progress=25.0,
                folder_path=f"projects/{project_id}",
            )
            self.session.add(project)
            self.session.flush()
            self.created_projects[project_id] = project
            print(f"    + Project: {project.name}")

            # Create epic for the project
            epic = Epic(
                epic_id=generate_id("EPIC"),
                project_id=project_id,
                title=f"{proj_data['name']} - MVP",
                description="First release with core features",
                color="#003B4A",
            )
            self.session.add(epic)
            self.session.flush()

            # Create sprint
            sprint = Sprint(
                sprint_id=generate_id("SPR"),
                project_id=project_id,
                name="Sprint 1 - Foundation",
                goal="Implement core features and infrastructure",
                start_date=datetime.utcnow(),
                end_date=datetime.utcnow() + timedelta(days=14),
                status="active",
            )
            self.session.add(sprint)
            self.session.flush()

            # Create stories
            for idx, story_data in enumerate(proj_data.get("stories", [])):
                story = Story(
                    story_id=generate_id("STR"),
                    tenant_id=tenant_id,
                    project_id=project_id,
                    title=story_data["title"],
                    persona=story_data.get("persona"),
                    action=story_data.get("action"),
                    benefit=story_data.get("benefit"),
                    story_points=story_data.get("story_points", 3),
                    status="ready" if idx == 0 else "backlog",
                    priority="high" if idx == 0 else "medium",
                    epic_id=epic.epic_id,
                    sprint_id=sprint.sprint_id if idx < 2 else None,
                )
                self.session.add(story)
                self.session.flush()
                print(f"      + Story: {story.title} ({story.story_points} pts)")

                # Create tasks for story
                for task_data in story_data.get("tasks", []):
                    task = StoryTask(
                        task_id=generate_id("TSK"),
                        story_id=story.story_id,
                        title=task_data["title"],
                        task_type=task_data.get("task_type", "development"),
                        status="pending",
                    )
                    self.session.add(task)

            projects.append(project)

        self.session.flush()
        return projects

    def seed_billing(self, tenant_id: str, plan_id: str):
        """Create billing data for a tenant."""
        print(f"\n  Seeding Billing for {tenant_id}...")

        # Create subscription
        subscription = Subscription(
            subscription_id=generate_id("SUB"),
            tenant_id=tenant_id,
            plan_id=plan_id,
            status=SubscriptionStatus.ACTIVE.value,
            billing_period="monthly",
            current_period_start=datetime.utcnow().replace(day=1),
            current_period_end=datetime.utcnow().replace(day=1) + timedelta(days=30),
        )
        self.session.add(subscription)
        print(f"    + Subscription: {subscription.subscription_id}")

        # Create payment method
        payment = PaymentMethod(
            payment_method_id=generate_id("PM"),
            tenant_id=tenant_id,
            method_type=PaymentMethodType.CARD.value,
            is_default=True,
            card_brand="visa",
            card_last_four="4242",
            card_exp_month=12,
            card_exp_year=2028,
            billing_name="Test User",
        )
        self.session.add(payment)
        print(f"    + Payment Method: **** {payment.card_last_four}")

        # Create sample invoice
        invoice = Invoice(
            invoice_id=generate_id("INV"),
            tenant_id=tenant_id,
            subscription_id=subscription.subscription_id,
            status=InvoiceStatus.PAID.value,
            subtotal=29900,
            total=29900,
            amount_paid=29900,
            amount_due=0,
            currency="BRL",
            period_start=datetime.utcnow().replace(day=1) - timedelta(days=30),
            period_end=datetime.utcnow().replace(day=1),
            issue_date=datetime.utcnow() - timedelta(days=25),
            paid_at=datetime.utcnow() - timedelta(days=23),
            line_items=[
                {
                    "description": "Professional Plan - December 2024",
                    "quantity": 1,
                    "unit_amount": 29900,
                    "amount": 29900
                }
            ]
        )
        self.session.add(invoice)
        print(f"    + Invoice: {invoice.invoice_id} (R$ {invoice.total/100:.2f})")

        # Create usage records
        from datetime import date
        today = date.today()
        usage = Usage(
            usage_id=generate_id("USG"),
            tenant_id=tenant_id,
            period=today,
            period_type="daily",
            metric="llm_tokens",
            value=15000,
            limit_value=100000,
        )
        self.session.add(usage)
        print(f"    + Usage: {usage.metric} = {usage.value}")

        self.session.flush()

    def seed_all(self, clean: bool = False, tenant_filter: str = None):
        """Seed all test data."""
        print("=" * 60)
        print("Fabrica de Agentes - Data Seeder")
        print("=" * 60)

        if clean:
            self.clean_data()

        # Seed plans first
        self.seed_plans()

        # Combine all test tenants (Issue #124)
        all_tenants = TEST_TENANTS + ADDITIONAL_TEST_TENANTS

        # Filter tenants if specified
        tenants_to_seed = all_tenants
        if tenant_filter:
            tenants_to_seed = [t for t in all_tenants if t["slug"] == tenant_filter]
            if not tenants_to_seed:
                print(f"Warning: No tenant found with slug '{tenant_filter}'")
                return

        # Seed each tenant
        for tenant_data in tenants_to_seed:
            tenant = self.seed_tenant(tenant_data)
            self.seed_projects(tenant.tenant_id)

            # Only seed billing for paid plans
            if tenant_data["plan"] != TenantPlan.FREE.value:
                plan_mapping = {
                    TenantPlan.STARTER.value: "PLAN-STARTER",
                    TenantPlan.PROFESSIONAL.value: "PLAN-PROFESSIONAL",
                    TenantPlan.ENTERPRISE.value: "PLAN-ENTERPRISE",
                }
                plan_id = plan_mapping.get(tenant_data["plan"], "PLAN-STARTER")
                self.seed_billing(tenant.tenant_id, plan_id)

        print("\n" + "=" * 60)
        print("Seeding Complete!")
        print("=" * 60)
        print(f"\nCreated:")
        print(f"  - {len(TEST_PLANS)} Plans")
        print(f"  - {len(self.created_tenants)} Tenants")
        print(f"  - {len(self.created_users)} Users")
        print(f"  - {len(self.created_projects)} Projects")
        print("\nTest Credentials:")
        for tenant_data in tenants_to_seed:
            print(f"\n  {tenant_data['name']} ({tenant_data['slug']}):")
            for user_data in tenant_data.get("users", []):
                print(f"    - {user_data['username']}: {user_data['password']}")


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """Main entry point for CLI."""
    parser = argparse.ArgumentParser(
        description="Seed test data for Fabrica de Agentes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python scripts/seed_data.py                  # Seed all data
  python scripts/seed_data.py --clean          # Clean and reseed
  python scripts/seed_data.py --tenant demo    # Seed only demo tenant
  python scripts/seed_data.py --db-url postgresql://...  # Custom database
        """
    )

    parser.add_argument(
        "--clean", "-c",
        action="store_true",
        help="Clean existing data before seeding"
    )

    parser.add_argument(
        "--tenant", "-t",
        type=str,
        help="Only seed specific tenant by slug (demo, enterprise, free)"
    )

    parser.add_argument(
        "--db-url",
        type=str,
        help="Database URL (defaults to DATABASE_URL env var)"
    )

    args = parser.parse_args()

    try:
        with DataSeeder(db_url=args.db_url) as seeder:
            seeder.seed_all(
                clean=args.clean,
                tenant_filter=args.tenant
            )
    except Exception as e:
        print(f"\nError during seeding: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
