# -*- coding: utf-8 -*-
"""
Demonstracao Completa - Plataforma E com White Label Belgo
==========================================================

Este script:
1. Faz seed de dados ficticios completos com branding Belgo
2. Abre browser via Playwright
3. Testa todas as telas desde o login
4. Mostra diferentes perfis de usuario
5. Cria issues no GitHub para erros encontrados

Uso: python test_complete_demo.py
"""

import asyncio
import json
import os
import sys
import requests
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Any
import hashlib

# Add project root
sys.path.insert(0, str(Path(__file__).parent))

# =============================================================================
# CONFIGURACAO BELGO WHITE LABEL
# =============================================================================

BELGO_BRANDING = {
    "tenant_id": "BELGO-001",
    "name": "Belgo Arames - Plataforma E",
    "slug": "belgo",
    "display_name": "Belgo Arames - Fabrica de Agentes",
    "logo_url": "/static/logos/belgo-logo.png",
    "logo_dark_url": "/static/logos/belgo-logo-white.png",
    "favicon_url": "/static/favicon-belgo.ico",
    "colors": {
        "primary": "#003B4A",      # Azul Belgo
        "secondary": "#FF6C00",    # Laranja Belgo
        "accent": "#10B981",       # Verde para sucesso
        "background": "#F3F4F6",   # Cinza claro
        "surface": "#FFFFFF",      # Branco
        "error": "#EF4444",
        "warning": "#F59E0B",
        "info": "#3B82F6",
        "success": "#10B981",
        "text_primary": "#1F2937",
        "text_secondary": "#6B7280",
    },
    "fonts": {
        "heading": "Inter, sans-serif",
        "body": "Inter, sans-serif",
    },
    "custom_css": """
        .sidebar { background: linear-gradient(180deg, #003B4A 0%, #002530 100%); }
        .btn-primary { background-color: #FF6C00; }
        .btn-primary:hover { background-color: #E56000; }
        .header { border-bottom: 3px solid #FF6C00; }
        .story-card.priority-high { border-left: 4px solid #FF6C00; }
    """,
    "features": {
        "show_logo": True,
        "show_footer": True,
        "footer_text": "Belgo Arames - Plataforma E v6.5",
        "custom_login_message": "Acesse a Fabrica de Agentes IA",
    }
}

# =============================================================================
# USUARIOS DE TESTE COM DIFERENTES PERFIS
# =============================================================================

BELGO_USERS = [
    {
        "username": "belgo_admin",
        "password": "Belgo@2024!",
        "email": "admin@belgo.com.br",
        "role": "ADMIN",
        "display_name": "Administrador Belgo",
        "avatar": "admin",
        "permissions": ["all"],
        "description": "Acesso total ao sistema"
    },
    {
        "username": "belgo_gerente",
        "password": "Gerente@2024!",
        "email": "gerente@belgo.com.br",
        "role": "MANAGER",
        "display_name": "Gerente de Projetos",
        "avatar": "manager",
        "permissions": ["projects", "stories", "reports", "team"],
        "description": "Gerencia projetos e equipes"
    },
    {
        "username": "belgo_dev",
        "password": "Dev@2024!",
        "email": "desenvolvedor@belgo.com.br",
        "role": "DEVELOPER",
        "display_name": "Desenvolvedor Senior",
        "avatar": "developer",
        "permissions": ["stories", "tasks", "code"],
        "description": "Desenvolve e implementa stories"
    },
    {
        "username": "belgo_qa",
        "password": "QA@2024!",
        "email": "qa@belgo.com.br",
        "role": "TESTER",
        "display_name": "Analista de Qualidade",
        "avatar": "tester",
        "permissions": ["stories", "tasks", "testing"],
        "description": "Testa e valida implementacoes"
    },
    {
        "username": "belgo_po",
        "password": "PO@2024!",
        "email": "po@belgo.com.br",
        "role": "PRODUCT_OWNER",
        "display_name": "Product Owner",
        "avatar": "po",
        "permissions": ["stories", "backlog", "priorities"],
        "description": "Define e prioriza stories"
    },
    {
        "username": "belgo_viewer",
        "password": "Viewer@2024!",
        "email": "viewer@belgo.com.br",
        "role": "VIEWER",
        "display_name": "Stakeholder",
        "avatar": "viewer",
        "permissions": ["view_only"],
        "description": "Visualiza progresso dos projetos"
    },
]

# =============================================================================
# PROJETOS E STORIES FICTICIOS BELGO
# =============================================================================

BELGO_PROJECTS = [
    {
        "project_id": "BELGO-PORTAL-001",
        "name": "Portal do Cliente Belgo",
        "description": "Plataforma digital para clientes acompanharem pedidos, notas fiscais e suporte",
        "project_type": "web-app",
        "status": "IN_PROGRESS",
        "progress": 65.0,
        "tech_stack": ["Python", "FastAPI", "Vue.js", "PostgreSQL"],
        "epics": [
            {
                "title": "Autenticacao e Seguranca",
                "color": "#003B4A",
                "stories": [
                    {
                        "title": "Login SSO com Active Directory",
                        "persona": "cliente corporativo",
                        "action": "fazer login usando minhas credenciais corporativas",
                        "benefit": "nao preciso lembrar de mais uma senha",
                        "story_points": 13,
                        "status": "done",
                        "priority": "high",
                        "tasks": [
                            {"title": "Integrar LDAP/AD", "status": "completed", "progress": 100},
                            {"title": "Implementar OAuth 2.0", "status": "completed", "progress": 100},
                            {"title": "Criar tela de login", "status": "completed", "progress": 100},
                        ]
                    },
                    {
                        "title": "Recuperacao de Senha",
                        "persona": "cliente",
                        "action": "recuperar minha senha por email",
                        "benefit": "consiga acessar mesmo se esquecer a senha",
                        "story_points": 5,
                        "status": "done",
                        "priority": "medium",
                    },
                    {
                        "title": "Autenticacao Multi-Fator (MFA)",
                        "persona": "cliente",
                        "action": "habilitar autenticacao em duas etapas",
                        "benefit": "minha conta fique mais segura",
                        "story_points": 8,
                        "status": "in_progress",
                        "priority": "high",
                        "tasks": [
                            {"title": "Implementar TOTP", "status": "in_progress", "progress": 60},
                            {"title": "Integrar SMS", "status": "pending", "progress": 0},
                        ]
                    },
                ]
            },
            {
                "title": "Gestao de Pedidos",
                "color": "#FF6C00",
                "stories": [
                    {
                        "title": "Dashboard de Pedidos",
                        "persona": "cliente",
                        "action": "visualizar todos os meus pedidos em um dashboard",
                        "benefit": "tenha visao geral rapida do status",
                        "story_points": 8,
                        "status": "in_progress",
                        "priority": "high",
                    },
                    {
                        "title": "Rastreamento de Entregas",
                        "persona": "cliente",
                        "action": "rastrear a entrega dos meus pedidos em tempo real",
                        "benefit": "saiba exatamente quando minha mercadoria chegara",
                        "story_points": 13,
                        "status": "ready",
                        "priority": "high",
                    },
                    {
                        "title": "Historico de Pedidos",
                        "persona": "cliente",
                        "action": "consultar historico completo de pedidos",
                        "benefit": "tenha registro de todas as compras",
                        "story_points": 5,
                        "status": "backlog",
                        "priority": "medium",
                    },
                ]
            },
            {
                "title": "Notas Fiscais",
                "color": "#10B981",
                "stories": [
                    {
                        "title": "Download de NFe em XML e PDF",
                        "persona": "contador do cliente",
                        "action": "baixar notas fiscais em XML e PDF",
                        "benefit": "importe facilmente para o sistema contabil",
                        "story_points": 5,
                        "status": "testing",
                        "priority": "high",
                    },
                    {
                        "title": "Busca Avancada de NFe",
                        "persona": "contador",
                        "action": "buscar notas por periodo, valor ou numero",
                        "benefit": "encontre rapidamente a nota que preciso",
                        "story_points": 8,
                        "status": "review",
                        "priority": "medium",
                    },
                ]
            }
        ],
        "sprints": [
            {
                "name": "Sprint 1 - Fundacao",
                "goal": "Autenticacao e estrutura base",
                "start_date": datetime.now() - timedelta(days=28),
                "end_date": datetime.now() - timedelta(days=14),
                "status": "completed",
                "velocity": 26
            },
            {
                "name": "Sprint 2 - Pedidos",
                "goal": "Dashboard e gestao de pedidos",
                "start_date": datetime.now() - timedelta(days=14),
                "end_date": datetime.now(),
                "status": "active",
                "velocity": 21
            },
            {
                "name": "Sprint 3 - Notas Fiscais",
                "goal": "Modulo completo de NFe",
                "start_date": datetime.now(),
                "end_date": datetime.now() + timedelta(days=14),
                "status": "planned",
                "velocity": 0
            }
        ]
    },
    {
        "project_id": "BELGO-APP-001",
        "name": "App Mobile Vendedores",
        "description": "Aplicativo mobile para equipe de vendas consultar clientes e fazer pedidos",
        "project_type": "mobile-app",
        "status": "IN_PROGRESS",
        "progress": 35.0,
        "tech_stack": ["React Native", "Node.js", "MongoDB"],
        "epics": [
            {
                "title": "Catalogo de Produtos",
                "color": "#6366F1",
                "stories": [
                    {
                        "title": "Listagem de Produtos com Filtros",
                        "persona": "vendedor",
                        "action": "buscar produtos por categoria, codigo ou nome",
                        "benefit": "encontre rapidamente o que o cliente precisa",
                        "story_points": 8,
                        "status": "in_progress",
                        "priority": "high",
                    },
                    {
                        "title": "Ficha Tecnica do Produto",
                        "persona": "vendedor",
                        "action": "visualizar especificacoes tecnicas detalhadas",
                        "benefit": "tire duvidas do cliente na hora",
                        "story_points": 5,
                        "status": "ready",
                        "priority": "medium",
                    },
                ]
            },
            {
                "title": "Gestao de Clientes",
                "color": "#EC4899",
                "stories": [
                    {
                        "title": "Cadastro de Novos Clientes",
                        "persona": "vendedor",
                        "action": "cadastrar novos clientes pelo app",
                        "benefit": "agilize o processo de venda",
                        "story_points": 8,
                        "status": "backlog",
                        "priority": "high",
                    },
                    {
                        "title": "Historico de Compras do Cliente",
                        "persona": "vendedor",
                        "action": "consultar historico de compras do cliente",
                        "benefit": "ofereca produtos relevantes",
                        "story_points": 5,
                        "status": "backlog",
                        "priority": "medium",
                    },
                ]
            }
        ]
    },
    {
        "project_id": "BELGO-BI-001",
        "name": "Dashboard BI Executivo",
        "description": "Painel de Business Intelligence para diretoria com KPIs e metricas de negocio",
        "project_type": "data-analysis",
        "status": "PLANNING",
        "progress": 10.0,
        "tech_stack": ["Python", "Apache Superset", "PostgreSQL", "dbt"],
        "epics": [
            {
                "title": "KPIs de Vendas",
                "color": "#F59E0B",
                "stories": [
                    {
                        "title": "Dashboard de Vendas por Regiao",
                        "persona": "diretor comercial",
                        "action": "visualizar vendas por regiao em mapa interativo",
                        "benefit": "identifique oportunidades geograficas",
                        "story_points": 13,
                        "status": "backlog",
                        "priority": "high",
                    },
                    {
                        "title": "Comparativo Mensal de Metas",
                        "persona": "gerente de vendas",
                        "action": "comparar realizad vs meta por vendedor",
                        "benefit": "acompanhe performance da equipe",
                        "story_points": 8,
                        "status": "backlog",
                        "priority": "high",
                    },
                ]
            }
        ]
    }
]

# =============================================================================
# PAGINAS PARA TESTAR
# =============================================================================

PAGES_TO_TEST = [
    {"path": "/login", "name": "Tela de Login", "requires_auth": False},
    {"path": "/", "name": "Dashboard Principal", "requires_auth": True},
    {"path": "/projects", "name": "Lista de Projetos", "requires_auth": True},
    {"path": "/executive", "name": "Dashboard Executivo", "requires_auth": True},
    {"path": "/analytics", "name": "Analytics", "requires_auth": True},
    {"path": "/billing", "name": "Faturamento", "requires_auth": True},
    {"path": "/admin", "name": "Admin Panel", "requires_auth": True, "role": "ADMIN"},
    {"path": "/admin/users", "name": "Gestao de Usuarios", "requires_auth": True, "role": "ADMIN"},
    {"path": "/admin/portal", "name": "Portal Admin", "requires_auth": True, "role": "ADMIN"},
    {"path": "/security", "name": "Seguranca", "requires_auth": True, "role": "ADMIN"},
    {"path": "/integrations", "name": "Integracoes", "requires_auth": True},
    {"path": "/workers", "name": "Workers", "requires_auth": True},
    {"path": "/docs", "name": "Documentacao", "requires_auth": True},
]

# =============================================================================
# SEED DATA
# =============================================================================

BASE_URL = "http://localhost:9001"

def hash_password(password: str) -> str:
    """Hash password with bcrypt if available, else SHA256"""
    try:
        from passlib.context import CryptContext
        pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
        return pwd_context.hash(password)
    except ImportError:
        return hashlib.sha256(password.encode()).hexdigest()


def seed_belgo_data():
    """Seed all Belgo data"""
    print("\n" + "=" * 70)
    print("  SEED - Dados Ficticios Belgo")
    print("=" * 70)

    try:
        from factory.database.connection import SessionLocal, init_db
        from factory.database.models import (
            User, Project, Story, StoryTask, Epic, Sprint, Worker,
            Tenant, TenantSettings, BrandingConfig, TenantMember
        )

        # Initialize database
        init_db()
        db = SessionLocal()

        try:
            # 1. Create Tenant
            print("\n[1/5] Criando Tenant Belgo...")
            existing_tenant = db.query(Tenant).filter(Tenant.tenant_id == BELGO_BRANDING["tenant_id"]).first()
            if existing_tenant:
                print(f"  - Tenant {BELGO_BRANDING['tenant_id']} ja existe, atualizando...")
                tenant = existing_tenant
            else:
                tenant = Tenant(
                    tenant_id=BELGO_BRANDING["tenant_id"],
                    name=BELGO_BRANDING["name"],
                    slug=BELGO_BRANDING["slug"],
                    status="active",
                    plan="enterprise",
                    email="admin@belgo.com.br",
                )
                db.add(tenant)
                db.flush()
                print(f"  + Tenant {BELGO_BRANDING['tenant_id']} criado")

            # 2. Create Branding
            print("\n[2/5] Configurando White Label...")
            existing_branding = db.query(BrandingConfig).filter(
                BrandingConfig.tenant_id == BELGO_BRANDING["tenant_id"]
            ).first()

            if existing_branding:
                existing_branding.display_name = BELGO_BRANDING["display_name"]
                existing_branding.logo_url = BELGO_BRANDING["logo_url"]
                existing_branding.logo_dark_url = BELGO_BRANDING["logo_dark_url"]
                existing_branding.favicon_url = BELGO_BRANDING["favicon_url"]
                existing_branding.colors = BELGO_BRANDING["colors"]
                existing_branding.fonts = BELGO_BRANDING["fonts"]
                existing_branding.custom_css = BELGO_BRANDING["custom_css"]
                print("  ~ Branding atualizado")
            else:
                branding = BrandingConfig(
                    tenant_id=BELGO_BRANDING["tenant_id"],
                    display_name=BELGO_BRANDING["display_name"],
                    logo_url=BELGO_BRANDING["logo_url"],
                    logo_dark_url=BELGO_BRANDING["logo_dark_url"],
                    favicon_url=BELGO_BRANDING["favicon_url"],
                    colors=BELGO_BRANDING["colors"],
                    fonts=BELGO_BRANDING["fonts"],
                    custom_css=BELGO_BRANDING["custom_css"],
                )
                db.add(branding)
                print("  + Branding configurado:")

            print(f"    - Primary: {BELGO_BRANDING['colors']['primary']}")
            print(f"    - Secondary: {BELGO_BRANDING['colors']['secondary']}")
            print(f"    - Logo: {BELGO_BRANDING['logo_url']}")

            # 3. Create Users
            print("\n[3/5] Criando Usuarios...")
            for user_data in BELGO_USERS:
                existing_user = db.query(User).filter(User.username == user_data["username"]).first()
                if existing_user:
                    print(f"  - {user_data['username']} ja existe")
                    user = existing_user
                else:
                    user = User(
                        username=user_data["username"],
                        password_hash=hash_password(user_data["password"]),
                        email=user_data["email"],
                        role=user_data["role"],
                        active=True,
                        force_password_change=False,
                    )
                    db.add(user)
                    db.flush()
                    print(f"  + {user_data['username']} ({user_data['role']}) - senha: {user_data['password']}")

                    # Add as tenant member
                    member = TenantMember(
                        tenant_id=BELGO_BRANDING["tenant_id"],
                        user_id=user.id,
                        tenant_role="owner" if user_data["role"] == "ADMIN" else "member",
                    )
                    db.add(member)

            # 4. Create Projects and Stories
            print("\n[4/5] Criando Projetos e Stories...")
            story_count = 0
            task_count = 0

            for proj_data in BELGO_PROJECTS:
                existing_proj = db.query(Project).filter(Project.project_id == proj_data["project_id"]).first()
                if existing_proj:
                    print(f"  - Projeto {proj_data['project_id']} ja existe")
                    project = existing_proj
                else:
                    project = Project(
                        project_id=proj_data["project_id"],
                        tenant_id=BELGO_BRANDING["tenant_id"],
                        name=proj_data["name"],
                        description=proj_data["description"],
                        project_type=proj_data["project_type"],
                        status=proj_data["status"],
                        progress=proj_data["progress"],
                        config={"tech_stack": proj_data["tech_stack"]},
                    )
                    db.add(project)
                    db.flush()
                    print(f"  + Projeto: {proj_data['name']}")

                # Create Epics and Stories
                for epic_data in proj_data.get("epics", []):
                    epic_id = f"EPIC-{proj_data['project_id'][-3:]}-{epic_data['title'][:3].upper()}"
                    existing_epic = db.query(Epic).filter(Epic.epic_id == epic_id).first()

                    if not existing_epic:
                        epic = Epic(
                            epic_id=epic_id,
                            project_id=proj_data["project_id"],
                            title=epic_data["title"],
                            color=epic_data["color"],
                        )
                        db.add(epic)
                        db.flush()
                    else:
                        epic = existing_epic

                    # Create Stories
                    for story_data in epic_data.get("stories", []):
                        story_id = f"STR-{story_count + 1:04d}"
                        existing_story = db.query(Story).filter(Story.story_id == story_id).first()

                        if not existing_story:
                            story = Story(
                                story_id=story_id,
                                tenant_id=BELGO_BRANDING["tenant_id"],
                                project_id=proj_data["project_id"],
                                title=story_data["title"],
                                persona=story_data.get("persona"),
                                action=story_data.get("action"),
                                benefit=story_data.get("benefit"),
                                story_points=story_data.get("story_points", 3),
                                status=story_data.get("status", "backlog"),
                                priority=story_data.get("priority", "medium"),
                                epic_id=epic_id,
                            )
                            db.add(story)
                            db.flush()
                            story_count += 1

                            # Create Tasks
                            for task_data in story_data.get("tasks", []):
                                task_id = f"TSK-{task_count + 1:04d}"
                                task = StoryTask(
                                    task_id=task_id,
                                    tenant_id=BELGO_BRANDING["tenant_id"],
                                    story_id=story_id,
                                    title=task_data["title"],
                                    status=task_data.get("status", "pending"),
                                    progress=task_data.get("progress", 0),
                                )
                                db.add(task)
                                task_count += 1

                # Create Sprints
                for sprint_data in proj_data.get("sprints", []):
                    sprint_id = f"SPR-{proj_data['project_id'][-3:]}-{sprint_data['name'][:5].upper()}"
                    existing_sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()

                    if not existing_sprint:
                        sprint = Sprint(
                            sprint_id=sprint_id,
                            project_id=proj_data["project_id"],
                            name=sprint_data["name"],
                            goal=sprint_data["goal"],
                            start_date=sprint_data["start_date"],
                            end_date=sprint_data["end_date"],
                            status=sprint_data["status"],
                        )
                        db.add(sprint)

            # 5. Create Workers
            print("\n[5/5] Criando Workers...")
            workers_config = [
                {"worker_id": "worker-belgo-001", "model": "claude-sonnet-4-20250514", "hostname": "belgo-worker-1"},
                {"worker_id": "worker-belgo-002", "model": "claude-sonnet-4-20250514", "hostname": "belgo-worker-2"},
                {"worker_id": "worker-belgo-003", "model": "claude-opus-4-20250514", "hostname": "belgo-worker-3"},
            ]

            for w in workers_config:
                existing = db.query(Worker).filter(Worker.worker_id == w["worker_id"]).first()
                if not existing:
                    worker = Worker(
                        worker_id=w["worker_id"],
                        status="idle",
                        model=w["model"],
                        hostname=w["hostname"],
                        mcp_tools=["filesystem", "git", "bash"],
                    )
                    db.add(worker)
                    print(f"  + Worker {w['worker_id']}")
                else:
                    print(f"  - Worker {w['worker_id']} ja existe")

            db.commit()

            # Stats
            print("\n" + "=" * 70)
            print("  SEED CONCLUIDO!")
            print("=" * 70)
            print(f"\n  Estatisticas:")
            print(f"    - Usuarios: {len(BELGO_USERS)}")
            print(f"    - Projetos: {len(BELGO_PROJECTS)}")
            print(f"    - Stories: {story_count}")
            print(f"    - Tasks: {task_count}")
            print(f"    - Workers: 3")

            print(f"\n  Credenciais de Teste:")
            for u in BELGO_USERS:
                print(f"    - {u['username']}: {u['password']} ({u['role']})")

            return True

        except Exception as e:
            print(f"\n  [ERRO] {e}")
            import traceback
            traceback.print_exc()
            db.rollback()
            return False
        finally:
            db.close()

    except ImportError as e:
        print(f"\n  [ERRO] Dependencia nao encontrada: {e}")
        return False


# =============================================================================
# PLAYWRIGHT TESTS
# =============================================================================

async def run_complete_demo():
    """Run complete demo with Playwright"""
    from playwright.async_api import async_playwright

    print("\n" + "=" * 70)
    print("  DEMONSTRACAO COMPLETA - Plataforma E Belgo")
    print("=" * 70)

    # Results storage
    results = {
        "pages": [],
        "login_tests": [],
        "issues": [],
        "screenshots": []
    }

    os.makedirs('screenshots', exist_ok=True)

    async with async_playwright() as p:
        # Launch browser - NOT headless
        browser = await p.chromium.launch(
            headless=False,
            slow_mo=300  # Slower for demo visibility
        )
        context = await browser.new_context(
            viewport={'width': 1920, 'height': 1080},
            locale='pt-BR'
        )
        page = await context.new_page()

        # ========== TEST 1: LOGIN PAGE ==========
        print("\n" + "=" * 60)
        print("  1. TELA DE LOGIN")
        print("=" * 60)

        await page.goto(f"{BASE_URL}/login")
        await page.wait_for_load_state('networkidle', timeout=10000)
        await asyncio.sleep(1)

        # Screenshot login page
        await page.screenshot(path='screenshots/01_login.png')
        print("  [OK] Tela de login carregada")
        print("  [OK] Screenshot: screenshots/01_login.png")

        # Check login form elements
        login_form = await page.query_selector('form, .login-form, [class*="login"]')
        username_input = await page.query_selector('input[name="username"], input[type="text"], #username')
        password_input = await page.query_selector('input[name="password"], input[type="password"], #password')
        login_button = await page.query_selector('button[type="submit"], .btn-login, button:has-text("Login"), button:has-text("Entrar")')

        if username_input and password_input and login_button:
            print("  [OK] Formulario de login encontrado")
        else:
            print("  [WARN] Elementos de login podem estar diferentes")
            results["issues"].append({
                "type": "ui",
                "page": "/login",
                "error": "Login form elements not found as expected"
            })

        # ========== TEST 2: LOGIN WITH DIFFERENT USERS ==========
        print("\n" + "=" * 60)
        print("  2. TESTE DE LOGIN COM DIFERENTES PERFIS")
        print("=" * 60)

        for user in BELGO_USERS[:3]:  # Test first 3 users
            print(f"\n  >> Testando login: {user['username']} ({user['role']})")

            try:
                # Navigate to login
                await page.goto(f"{BASE_URL}/login")
                await page.wait_for_load_state('networkidle', timeout=10000)
                await asyncio.sleep(0.5)

                # Fill login form
                username_input = await page.query_selector('input[name="username"], input[type="text"], #username')
                password_input = await page.query_selector('input[name="password"], input[type="password"], #password')

                if username_input and password_input:
                    await username_input.fill(user['username'])
                    await password_input.fill(user['password'])
                    await asyncio.sleep(0.3)

                    # Submit
                    login_button = await page.query_selector('button[type="submit"], .btn-login, button:has-text("Login"), button:has-text("Entrar")')
                    if login_button:
                        await login_button.click()
                        await asyncio.sleep(2)

                        # Check if redirected to dashboard
                        current_url = page.url
                        if "/login" not in current_url or "dashboard" in current_url or current_url == f"{BASE_URL}/":
                            print(f"     [OK] Login bem-sucedido para {user['username']}")
                            results["login_tests"].append({
                                "user": user["username"],
                                "role": user["role"],
                                "success": True
                            })

                            # Screenshot after login
                            await page.screenshot(path=f'screenshots/02_dashboard_{user["role"].lower()}.png')
                            print(f"     [OK] Screenshot: screenshots/02_dashboard_{user['role'].lower()}.png")
                        else:
                            print(f"     [WARN] Pode nao ter redirecionado corretamente")
                            results["login_tests"].append({
                                "user": user["username"],
                                "role": user["role"],
                                "success": False,
                                "error": "No redirect after login"
                            })
                else:
                    print(f"     [ERRO] Campos de login nao encontrados")
                    results["issues"].append({
                        "type": "login",
                        "user": user["username"],
                        "error": "Login fields not found"
                    })

            except Exception as e:
                print(f"     [ERRO] {str(e)[:50]}")
                results["issues"].append({
                    "type": "login",
                    "user": user["username"],
                    "error": str(e)[:100]
                })

        # ========== TEST 3: ALL PAGES ==========
        print("\n" + "=" * 60)
        print("  3. TESTE DE TODAS AS PAGINAS")
        print("=" * 60)

        # Login as admin for full access
        await page.goto(f"{BASE_URL}/login")
        await page.wait_for_load_state('networkidle', timeout=10000)

        username_input = await page.query_selector('input[name="username"], input[type="text"], #username')
        password_input = await page.query_selector('input[name="password"], input[type="password"], #password')
        if username_input and password_input:
            await username_input.fill("belgo_admin")
            await password_input.fill("Belgo@2024!")
            login_button = await page.query_selector('button[type="submit"], .btn-login, button:has-text("Login"), button:has-text("Entrar")')
            if login_button:
                await login_button.click()
                await asyncio.sleep(2)

        for idx, pg in enumerate(PAGES_TO_TEST):
            if pg["path"] == "/login":
                continue  # Skip login page

            print(f"\n  >> [{idx+1}/{len(PAGES_TO_TEST)}] {pg['name']} ({pg['path']})")

            try:
                response = await page.goto(f"{BASE_URL}{pg['path']}", timeout=15000)
                await page.wait_for_load_state('domcontentloaded', timeout=10000)
                await asyncio.sleep(1)

                status = response.status if response else "N/A"

                # Check content
                body = await page.query_selector('body')
                body_text = await body.text_content() if body else ""
                has_content = len(body_text.strip()) > 100

                # Screenshot
                screenshot_name = pg['path'].replace('/', '_').strip('_') or 'home'
                screenshot_path = f'screenshots/{idx+1:02d}_{screenshot_name}.png'
                await page.screenshot(path=screenshot_path)

                result = {
                    "path": pg['path'],
                    "name": pg['name'],
                    "status": status,
                    "has_content": has_content,
                    "screenshot": screenshot_path
                }
                results["pages"].append(result)
                results["screenshots"].append(screenshot_path)

                if status == 200 and has_content:
                    print(f"     [OK] Status: {status}")
                elif status == 200:
                    print(f"     [WARN] Status: {status} | Pouco conteudo")
                elif status == 404:
                    print(f"     [ERRO] Status: 404 - Pagina nao encontrada")
                    results["issues"].append({
                        "type": "page",
                        "path": pg['path'],
                        "error": "404 Not Found"
                    })
                else:
                    print(f"     [ERRO] Status: {status}")
                    results["issues"].append({
                        "type": "page",
                        "path": pg['path'],
                        "error": f"HTTP {status}"
                    })

            except Exception as e:
                print(f"     [ERRO] {str(e)[:50]}")
                results["issues"].append({
                    "type": "page",
                    "path": pg['path'],
                    "error": str(e)[:100]
                })

        # ========== TEST 4: KANBAN INTERACTIONS ==========
        print("\n" + "=" * 60)
        print("  4. TESTE DO KANBAN")
        print("=" * 60)

        try:
            await page.goto(f"{BASE_URL}/")
            await page.wait_for_load_state('networkidle', timeout=10000)
            await asyncio.sleep(2)

            # Check for Kanban columns
            kanban_columns = await page.query_selector_all('.kanban-column, [class*="column"], .column')
            story_cards = await page.query_selector_all('.story-card, [class*="story"], .card')

            print(f"     [OK] Colunas Kanban: {len(kanban_columns)}")
            print(f"     [OK] Story Cards: {len(story_cards)}")

            await page.screenshot(path='screenshots/kanban_view.png')
            results["screenshots"].append('screenshots/kanban_view.png')

        except Exception as e:
            print(f"     [ERRO] {str(e)[:50]}")
            results["issues"].append({
                "type": "kanban",
                "error": str(e)[:100]
            })

        # ========== KEEP BROWSER OPEN ==========
        print("\n" + "=" * 60)
        print("  DEMONSTRACAO COMPLETA")
        print("=" * 60)
        print("\n  Browser permanecera aberto por 30 segundos...")
        print("  Explore as telas manualmente!\n")

        await asyncio.sleep(30)
        await browser.close()

    return results


# =============================================================================
# CREATE GITHUB ISSUES
# =============================================================================

def create_github_issues(results: Dict[str, Any]):
    """Create GitHub issues for found errors"""
    issues = results.get("issues", [])

    if not issues:
        print("\n  [OK] Nenhum issue para criar!")
        return

    print("\n" + "=" * 60)
    print("  CRIANDO ISSUES NO GITHUB")
    print("=" * 60)

    try:
        import subprocess

        for issue in issues[:5]:  # Limit to 5 issues
            title = f"[Auto-Test] {issue['type'].upper()}: {issue.get('path', issue.get('user', 'Unknown'))}"
            body = f"""## Issue Detectado por Teste Automatizado

**Tipo:** {issue['type']}
**Path/Usuario:** {issue.get('path', issue.get('user', 'N/A'))}
**Erro:** {issue.get('error', 'N/A')}

**Data:** {datetime.now().isoformat()}

---
_Issue criado automaticamente pelo script de teste E2E_
"""

            # Create issue using gh CLI
            result = subprocess.run(
                ["gh", "issue", "create", "--title", title, "--body", body, "--label", "bug,automated-test"],
                capture_output=True,
                text=True,
                cwd=str(Path(__file__).parent)
            )

            if result.returncode == 0:
                print(f"  [OK] Issue criado: {title[:50]}...")
            else:
                print(f"  [WARN] Falha ao criar issue: {result.stderr[:50]}")

    except Exception as e:
        print(f"  [ERRO] Nao foi possivel criar issues: {e}")


# =============================================================================
# MAIN
# =============================================================================

def print_summary(results: Dict[str, Any]):
    """Print test summary"""
    print("\n" + "=" * 70)
    print("  RESUMO FINAL")
    print("=" * 70)

    pages_ok = sum(1 for p in results.get("pages", []) if p.get("status") == 200)
    pages_total = len(results.get("pages", []))

    logins_ok = sum(1 for l in results.get("login_tests", []) if l.get("success"))
    logins_total = len(results.get("login_tests", []))

    issues_count = len(results.get("issues", []))

    print(f"\n  Paginas: {pages_ok}/{pages_total} OK")
    print(f"  Logins: {logins_ok}/{logins_total} OK")
    print(f"  Issues encontrados: {issues_count}")
    print(f"  Screenshots: {len(results.get('screenshots', []))}")

    if results.get("issues"):
        print("\n  ISSUES:")
        for issue in results["issues"]:
            print(f"    - [{issue['type'].upper()}] {issue.get('path', issue.get('user', 'N/A'))}: {issue.get('error', 'N/A')[:50]}")

    print("\n  Screenshots salvos em: screenshots/")


def main():
    """Main entry point"""
    import sys

    # Configure encoding
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(encoding='utf-8')

    print("\n" + "+" + "=" * 68 + "+")
    print("|  PLATAFORMA E - DEMONSTRACAO COMPLETA COM WHITE LABEL BELGO       |")
    print("+" + "=" * 68 + "+")
    print(f"\n  Data: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"  URL: {BASE_URL}")

    # Step 1: Seed data
    if not seed_belgo_data():
        print("\n  [ERRO] Falha no seed de dados. Verifique os logs acima.")
        # Continue anyway to test existing data

    # Step 2: Run Playwright tests
    try:
        results = asyncio.run(run_complete_demo())
    except Exception as e:
        print(f"\n  [ERRO] Falha nos testes Playwright: {e}")
        import traceback
        traceback.print_exc()
        results = {"pages": [], "login_tests": [], "issues": [{"type": "playwright", "error": str(e)}], "screenshots": []}

    # Step 3: Print summary
    print_summary(results)

    # Step 4: Create GitHub issues
    if results.get("issues"):
        create_github_issues(results)

    # Save results
    with open('test_demo_results.json', 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, default=str, ensure_ascii=False)
    print("\n  Resultados salvos em: test_demo_results.json")

    print("\n" + "=" * 70)
    print("  DEMONSTRACAO FINALIZADA!")
    print("=" * 70 + "\n")


if __name__ == "__main__":
    main()
