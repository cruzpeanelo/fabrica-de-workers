# -*- coding: utf-8 -*-
"""
Demo Seed - Multi-Tenant com White Label
=========================================

Cria dados para demonstrar a plataforma multi-tenant:
- BELGO-001: Belgo Arames (recebe projetos existentes)
- TECH-001: TechCorp Solutions (dados fictícios)
- STARTUP-001: StartupX (dados fictícios)

Cada tenant tem white label distinto (cores, logo).

Uso:
    python factory/database/demo_seed.py

Credenciais:
    platform_admin / admin123  - Super Admin (vê tudo)
    belgo_admin / belgo123     - Admin do Belgo
    belgo_pm / belgo123        - PM do Belgo
    tech_admin / tech123       - Admin TechCorp
    tech_dev / tech123         - Dev TechCorp
    startup_dev / startup123   - Dev StartupX
    consultor / consul123      - Multi-tenant (Belgo + TechCorp)

Author: Plataforma E - Terminal 4
"""

import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Setup path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from passlib.context import CryptContext

# Password hashing
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")


def hash_password(password: str) -> str:
    return pwd_context.hash(password)


def create_demo_data():
    """Create all demo data in PostgreSQL"""

    from factory.database.connection import SessionLocal, engine, Base
    from factory.database.models import (
        Tenant, TenantSettings, TenantMember, BrandingConfig,
        User, Project, Story, StoryTask,
        MemberRole
    )

    # Create tables
    Base.metadata.create_all(bind=engine)

    db = SessionLocal()

    try:
        print("\n" + "="*60)
        print("DEMO SEED - Multi-Tenant com White Label")
        print("="*60)

        # Check existing data
        existing_tenants = db.query(Tenant).count()
        existing_projects = db.query(Project).count()
        existing_stories = db.query(Story).count()

        print(f"\n[Estado Atual]")
        print(f"  Tenants: {existing_tenants}")
        print(f"  Projetos: {existing_projects}")
        print(f"  Stories: {existing_stories}")

        if existing_tenants > 0:
            print(f"\n[!] Ja existem dados no banco.")
            response = input("Deseja limpar usuarios/tenants e manter projetos? (s/N): ").strip().lower()
            if response != 's':
                print("Operacao cancelada.")
                return

            # Clear tenant-related data but keep projects/stories
            print("\n[*] Limpando tenants e usuarios...")
            db.query(TenantMember).delete()
            db.query(BrandingConfig).delete()
            db.query(TenantSettings).delete()
            db.query(Tenant).delete()
            db.query(User).delete()
            db.commit()
            print("[OK] Tenants e usuarios limpos.")

        # =====================================================================
        # 1. CREATE TENANTS WITH WHITE LABELS
        # =====================================================================
        print("\n[1/6] Criando Tenants com White Label...")

        tenants_data = [
            {
                "tenant_id": "BELGO-001",
                "name": "Belgo Arames",
                "slug": "belgo",
                "email": "admin@belgo.com.br",
                "plan": "enterprise",
                "status": "active",
                "features": {
                    "stories": True,
                    "kanban": True,
                    "workers": True,
                    "chat_assistant": True,
                    "custom_branding": True,
                    "sso": True,
                    "api_access": True,
                    "audit_logs": True,
                    "custom_domain": True
                },
                "branding": {
                    "primary_color": "#003B4A",
                    "secondary_color": "#FF6C00",
                    "accent_color": "#10B981",
                    "background_color": "#F8FAFC",
                    "text_color": "#1E293B",
                    "logo_url": "/static/logos/belgo.png",
                    "favicon_url": "/static/favicons/belgo.ico",
                    "display_name": "Belgo Arames",
                    "tagline": "Solucoes em Arame"
                }
            },
            {
                "tenant_id": "TECH-001",
                "name": "TechCorp Solutions",
                "slug": "techcorp",
                "email": "admin@techcorp.io",
                "plan": "professional",
                "status": "active",
                "features": {
                    "stories": True,
                    "kanban": True,
                    "workers": True,
                    "chat_assistant": True,
                    "custom_branding": True,
                    "api_access": True
                },
                "branding": {
                    "primary_color": "#6366F1",
                    "secondary_color": "#EC4899",
                    "accent_color": "#8B5CF6",
                    "background_color": "#F5F3FF",
                    "text_color": "#1E1B4B",
                    "logo_url": "/static/logos/techcorp.png",
                    "favicon_url": "/static/favicons/techcorp.ico",
                    "display_name": "TechCorp",
                    "tagline": "Innovation First"
                }
            },
            {
                "tenant_id": "STARTUP-001",
                "name": "StartupX",
                "slug": "startupx",
                "email": "team@startupx.co",
                "plan": "starter",
                "status": "trial",
                "features": {
                    "stories": True,
                    "kanban": True,
                    "workers": False,
                    "chat_assistant": True
                },
                "branding": {
                    "primary_color": "#10B981",
                    "secondary_color": "#F59E0B",
                    "accent_color": "#06B6D4",
                    "background_color": "#ECFDF5",
                    "text_color": "#064E3B",
                    "logo_url": "/static/logos/startupx.png",
                    "favicon_url": "/static/favicons/startupx.ico",
                    "display_name": "StartupX",
                    "tagline": "Move Fast"
                }
            }
        ]

        for t_data in tenants_data:
            # Create Tenant
            tenant = Tenant(
                tenant_id=t_data["tenant_id"],
                name=t_data["name"],
                slug=t_data["slug"],
                email=t_data["email"],
                plan=t_data["plan"],
                status=t_data["status"],
                features=t_data["features"]
            )
            db.add(tenant)
            db.flush()

            # Create TenantSettings
            settings = TenantSettings(
                tenant_id=t_data["tenant_id"],
                max_projects=100 if t_data["plan"] == "enterprise" else 20,
                max_stories_per_project=1000 if t_data["plan"] == "enterprise" else 200,
                max_members=50 if t_data["plan"] == "enterprise" else 10,
                preferred_model="claude-sonnet-4-20250514"
            )
            db.add(settings)

            # Create BrandingConfig (White Label)
            branding = t_data["branding"]
            brand_config = BrandingConfig(
                tenant_id=t_data["tenant_id"],
                display_name=branding["display_name"],
                tagline=branding.get("tagline", ""),
                logo_url=branding["logo_url"],
                favicon_url=branding["favicon_url"],
                colors={
                    "primary": branding["primary_color"],
                    "primary_hover": branding["primary_color"],
                    "primary_light": branding["background_color"],
                    "secondary": branding["secondary_color"],
                    "secondary_hover": branding["secondary_color"],
                    "success": "#10B981",
                    "warning": "#F59E0B",
                    "error": "#EF4444",
                    "info": "#3B82F6",
                    "background": branding["background_color"],
                    "surface": "#FFFFFF",
                    "text_primary": branding["text_color"],
                    "text_secondary": "#6B7280",
                    "border": "#E5E7EB",
                    "header_bg": branding["primary_color"],
                    "header_text": "#FFFFFF"
                },
                footer={
                    "show_footer": True,
                    "text": branding.get("tagline", ""),
                    "show_powered_by": t_data["plan"] != "enterprise"
                }
            )
            db.add(brand_config)

            print(f"    [+] {t_data['name']}")
            print(f"        Cores: {branding['primary_color']} / {branding['secondary_color']}")

        db.commit()

        # =====================================================================
        # 2. CREATE USERS
        # =====================================================================
        print("\n[2/6] Criando Usuarios...")

        users_data = [
            # Super Admin (vê tudo)
            {
                "username": "platform_admin",
                "email": "admin@fabrica.com",
                "password": "Platform@2025!Adm",
                "role": "SUPER_ADMIN",
                "tenants": ["BELGO-001", "TECH-001", "STARTUP-001"],
                "tenant_role": "owner"
            },
            # Belgo
            {
                "username": "belgo_admin",
                "email": "admin@belgo.com.br",
                "password": "Belgo@Admin#2025",
                "role": "ADMIN",
                "tenants": ["BELGO-001"],
                "tenant_role": "admin"
            },
            {
                "username": "belgo_pm",
                "email": "pm@belgo.com.br",
                "password": "Belgo@PM#2025",
                "role": "PROJECT_MANAGER",
                "tenants": ["BELGO-001"],
                "tenant_role": "member"
            },
            # TechCorp
            {
                "username": "tech_admin",
                "email": "admin@techcorp.io",
                "password": "TechCorp@Admin#2025",
                "role": "ADMIN",
                "tenants": ["TECH-001"],
                "tenant_role": "admin"
            },
            {
                "username": "tech_dev",
                "email": "dev@techcorp.io",
                "password": "TechCorp@Dev#2025",
                "role": "DEVELOPER",
                "tenants": ["TECH-001"],
                "tenant_role": "member"
            },
            # StartupX
            {
                "username": "startup_dev",
                "email": "dev@startupx.co",
                "password": "StartupX@Dev#2025",
                "role": "DEVELOPER",
                "tenants": ["STARTUP-001"],
                "tenant_role": "member"
            },
            # Consultor (multi-tenant)
            {
                "username": "consultor",
                "email": "consultor@external.com",
                "password": "Consultor@Multi#2025",
                "role": "DEVELOPER",
                "tenants": ["BELGO-001", "TECH-001"],
                "tenant_role": "member"
            }
        ]

        user_ids = {}
        for u_data in users_data:
            user = User(
                username=u_data["username"],
                email=u_data["email"],
                password_hash=hash_password(u_data["password"]),
                role=u_data["role"],
                active=True
            )
            db.add(user)
            db.flush()
            user_ids[u_data["username"]] = user.id

            # Create tenant memberships
            for tenant_id in u_data["tenants"]:
                role = "owner" if u_data["role"] == "SUPER_ADMIN" else u_data["tenant_role"]
                member = TenantMember(
                    tenant_id=tenant_id,
                    user_id=user.id,
                    tenant_role=role,
                    status="active"
                )
                db.add(member)

            tenants_str = ", ".join(u_data["tenants"])
            print(f"    [+] {u_data['username']} ({u_data['role']}) -> {tenants_str}")

        db.commit()

        # =====================================================================
        # 3. ASSOCIATE EXISTING PROJECTS TO BELGO
        # =====================================================================
        print("\n[3/6] Associando projetos existentes ao Belgo...")

        # Update existing projects without tenant_id
        projects_updated = db.query(Project).filter(
            (Project.tenant_id == None) | (Project.tenant_id == '')
        ).update({"tenant_id": "BELGO-001"}, synchronize_session=False)

        stories_updated = db.query(Story).filter(
            (Story.tenant_id == None) | (Story.tenant_id == '')
        ).update({"tenant_id": "BELGO-001"}, synchronize_session=False)

        db.commit()

        print(f"    [+] {projects_updated} projetos atualizados para BELGO-001")
        print(f"    [+] {stories_updated} stories atualizadas para BELGO-001")

        # =====================================================================
        # 4. CREATE FICTIONAL PROJECTS FOR TECHCORP
        # =====================================================================
        print("\n[4/6] Criando projetos ficticios para TechCorp...")

        tech_projects = [
            {
                "project_id": "TECH-SAAS",
                "tenant_id": "TECH-001",
                "name": "SaaS Platform",
                "description": "Cloud-based SaaS platform with multi-tenant architecture",
                "project_type": "web-app",
                "status": "IN_PROGRESS",
                "created_by": "tech_admin"
            },
            {
                "project_id": "TECH-MOBILE",
                "tenant_id": "TECH-001",
                "name": "Mobile App",
                "description": "Cross-platform mobile application with React Native",
                "project_type": "mobile",
                "status": "IN_PROGRESS",
                "created_by": "tech_admin"
            }
        ]

        for p_data in tech_projects:
            project = Project(
                project_id=p_data["project_id"],
                tenant_id=p_data["tenant_id"],
                name=p_data["name"],
                description=p_data["description"],
                project_type=p_data["project_type"],
                status=p_data["status"],
                created_by=p_data["created_by"]
            )
            db.add(project)
            print(f"    [+] {p_data['name']} (TECH-001)")

        db.commit()

        # Create stories for TechCorp projects
        tech_stories = [
            # TECH-SAAS
            {"project": "TECH-SAAS", "title": "User Dashboard", "status": "done", "points": 8},
            {"project": "TECH-SAAS", "title": "Billing Integration", "status": "in_progress", "points": 13},
            {"project": "TECH-SAAS", "title": "Analytics Module", "status": "ready", "points": 8},
            {"project": "TECH-SAAS", "title": "Admin Panel", "status": "backlog", "points": 13},
            {"project": "TECH-SAAS", "title": "API Gateway", "status": "backlog", "points": 8},
            # TECH-MOBILE
            {"project": "TECH-MOBILE", "title": "Login Screen", "status": "done", "points": 5},
            {"project": "TECH-MOBILE", "title": "Home Feed", "status": "in_progress", "points": 8},
            {"project": "TECH-MOBILE", "title": "Push Notifications", "status": "ready", "points": 5},
            {"project": "TECH-MOBILE", "title": "Offline Mode", "status": "backlog", "points": 13},
        ]

        story_counter = db.query(Story).count() + 1
        for s_data in tech_stories:
            story = Story(
                story_id=f"STR-{story_counter:04d}",
                tenant_id="TECH-001",
                project_id=s_data["project"],
                title=s_data["title"],
                persona="user",
                action=f"use {s_data['title'].lower()}",
                benefit="improve productivity",
                story_points=s_data["points"],
                status=s_data["status"],
                priority="medium",
                acceptance_criteria=["Feature works as expected", "Tests pass"],
                definition_of_done=["Code reviewed", "Tests passing", "Documented"]
            )
            db.add(story)
            story_counter += 1

        print(f"    [+] {len(tech_stories)} stories criadas para TechCorp")
        db.commit()

        # =====================================================================
        # 5. CREATE FICTIONAL PROJECT FOR STARTUPX
        # =====================================================================
        print("\n[5/6] Criando projeto ficticio para StartupX...")

        startup_project = Project(
            project_id="STARTUP-MVP",
            tenant_id="STARTUP-001",
            name="MVP Product",
            description="Minimum viable product for market validation",
            project_type="web-app",
            status="PLANNING",
            created_by="startup_dev"
        )
        db.add(startup_project)
        db.flush()
        print(f"    [+] MVP Product (STARTUP-001)")

        startup_stories = [
            {"title": "Landing Page", "status": "done", "points": 5},
            {"title": "Sign Up Flow", "status": "in_progress", "points": 5},
            {"title": "Core Feature", "status": "ready", "points": 8},
            {"title": "Payment Integration", "status": "backlog", "points": 8},
            {"title": "User Dashboard", "status": "backlog", "points": 8},
        ]

        for s_data in startup_stories:
            story = Story(
                story_id=f"STR-{story_counter:04d}",
                tenant_id="STARTUP-001",
                project_id="STARTUP-MVP",
                title=s_data["title"],
                persona="user",
                action=f"use {s_data['title'].lower()}",
                benefit="get value from product",
                story_points=s_data["points"],
                status=s_data["status"],
                priority="high",
                acceptance_criteria=["Works correctly", "User can complete task"],
                definition_of_done=["Code complete", "Tested"]
            )
            db.add(story)
            story_counter += 1

        print(f"    [+] {len(startup_stories)} stories criadas para StartupX")
        db.commit()

        # =====================================================================
        # 6. CREATE TASKS FOR NEW STORIES
        # =====================================================================
        print("\n[6/6] Criando tasks para novas stories...")

        # Get stories without tasks
        stories_without_tasks = db.query(Story).outerjoin(
            StoryTask, Story.story_id == StoryTask.story_id
        ).filter(StoryTask.task_id == None).all()

        task_counter = db.query(StoryTask).count() + 1
        task_templates = [
            {"title": "Implementation", "type": "development"},
            {"title": "Code Review", "type": "review"},
            {"title": "Testing", "type": "test"}
        ]

        for story in stories_without_tasks:
            if story.status == "done":
                task_statuses = ["completed", "completed", "completed"]
                task_progresses = [100, 100, 100]
            elif story.status == "testing":
                task_statuses = ["completed", "completed", "in_progress"]
                task_progresses = [100, 100, 50]
            elif story.status == "in_progress":
                task_statuses = ["in_progress", "pending", "pending"]
                task_progresses = [60, 0, 0]
            else:
                task_statuses = ["pending", "pending", "pending"]
                task_progresses = [0, 0, 0]

            for i, t_data in enumerate(task_templates):
                task = StoryTask(
                    task_id=f"TSK-{task_counter:04d}",
                    story_id=story.story_id,
                    title=f"{t_data['title']} - {story.title}",
                    task_type=t_data["type"],
                    status=task_statuses[i],
                    progress=task_progresses[i]
                )
                db.add(task)
                task_counter += 1

        db.commit()
        print(f"    [+] Tasks criadas para {len(stories_without_tasks)} stories")

        # =====================================================================
        # SUMMARY
        # =====================================================================
        print("\n" + "="*60)
        print("SEED COMPLETO!")
        print("="*60)

        # Count by tenant
        for tenant_id in ["BELGO-001", "TECH-001", "STARTUP-001"]:
            tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
            projects = db.query(Project).filter(Project.tenant_id == tenant_id).count()
            stories = db.query(Story).filter(Story.tenant_id == tenant_id).count()
            members = db.query(TenantMember).filter(TenantMember.tenant_id == tenant_id).count()
            branding = db.query(BrandingConfig).filter(BrandingConfig.tenant_id == tenant_id).first()

            print(f"\n[{tenant.name}]")
            colors = branding.colors if branding else {}
            print(f"  Cor primaria: {colors.get('primary', 'N/A') if colors else 'N/A'}")
            print(f"  Projetos: {projects}")
            print(f"  Stories: {stories}")
            print(f"  Membros: {members}")

        print("\n[Credenciais de Teste]")
        print("-" * 50)
        print("  platform_admin / Platform@2025!Adm   -> Super Admin (todos)")
        print("  belgo_admin / Belgo@Admin#2025       -> Admin Belgo (azul)")
        print("  belgo_pm / Belgo@PM#2025             -> PM Belgo")
        print("  tech_admin / TechCorp@Admin#2025     -> Admin TechCorp (roxo)")
        print("  tech_dev / TechCorp@Dev#2025         -> Dev TechCorp")
        print("  startup_dev / StartupX@Dev#2025      -> Dev StartupX (verde)")
        print("  consultor / Consultor@Multi#2025     -> Multi-tenant")
        print("-" * 50)

        print("\n[Proximos passos]")
        print("  1. python factory/dashboard/app_v6_agile.py")
        print("  2. Acessar http://localhost:9001/login")
        print("  3. Testar com diferentes usuarios")
        print()

    except Exception as e:
        db.rollback()
        print(f"\n[ERRO] {e}")
        import traceback
        traceback.print_exc()
        raise
    finally:
        db.close()


if __name__ == "__main__":
    create_demo_data()
