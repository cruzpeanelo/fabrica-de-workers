# -*- coding: utf-8 -*-
"""
[QA] Teste Completo - Multi-Tenancy, UX/UI, Segregacao de Dados
================================================================

Agente: QA
Responsabilidades:
- Testar todas as telas e funcionalidades
- Verificar segregacao de dados entre tenants
- Avaliar UX/UI (overlays, aproveitamento de tela)
- Criar issues para agentes corretos ([FRONT], [BACK], [SEC], [DEVOPS])
"""

import asyncio
import json
import os
import sys
import hashlib
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, field

sys.path.insert(0, str(Path(__file__).parent.parent))

# =============================================================================
# CONFIGURACAO DE TENANTS E USUARIOS
# =============================================================================

TENANTS_CONFIG = [
    {
        "tenant_id": "BELGO-001",
        "name": "Belgo Arames",
        "slug": "belgo",
        "plan": "enterprise",
        "branding": {
            "primary_color": "#003B4A",
            "secondary_color": "#FF6C00",
            "logo_url": "/static/logos/belgo.png"
        }
    },
    {
        "tenant_id": "TECH-001",
        "name": "TechCorp Solutions",
        "slug": "techcorp",
        "plan": "professional",
        "branding": {
            "primary_color": "#1E3A5F",
            "secondary_color": "#00A3E0",
            "logo_url": "/static/logos/techcorp.png"
        }
    },
    {
        "tenant_id": "STARTUP-001",
        "name": "StartupX",
        "slug": "startupx",
        "plan": "starter",
        "branding": {
            "primary_color": "#6366F1",
            "secondary_color": "#EC4899",
            "logo_url": "/static/logos/startupx.png"
        }
    }
]

USERS_CONFIG = [
    # SUPER_ADMIN - Ve todos os tenants
    {"username": "platform_admin", "password": "Platform@2024!", "role": "SUPER_ADMIN", "tenant_id": None, "can_see_all": True},

    # BELGO Users
    {"username": "belgo_admin", "password": "Belgo@2024!", "role": "ADMIN", "tenant_id": "BELGO-001", "can_see_all": False},
    {"username": "belgo_pm", "password": "BelgoPM@2024!", "role": "PROJECT_MANAGER", "tenant_id": "BELGO-001", "can_see_all": False},
    {"username": "belgo_dev", "password": "BelgoDev@2024!", "role": "DEVELOPER", "tenant_id": "BELGO-001", "can_see_all": False},
    {"username": "belgo_viewer", "password": "BelgoView@2024!", "role": "VIEWER", "tenant_id": "BELGO-001", "can_see_all": False},

    # TECH Users
    {"username": "tech_admin", "password": "Tech@2024!", "role": "ADMIN", "tenant_id": "TECH-001", "can_see_all": False},
    {"username": "tech_dev", "password": "TechDev@2024!", "role": "DEVELOPER", "tenant_id": "TECH-001", "can_see_all": False},

    # STARTUP Users
    {"username": "startup_admin", "password": "Startup@2024!", "role": "ADMIN", "tenant_id": "STARTUP-001", "can_see_all": False},
    {"username": "startup_dev", "password": "StartupDev@2024!", "role": "DEVELOPER", "tenant_id": "STARTUP-001", "can_see_all": False},
]

# Telas para testar
SCREENS_TO_TEST = [
    {"path": "/login", "name": "Login", "requires_auth": False, "min_role": None},
    {"path": "/", "name": "Dashboard/Kanban", "requires_auth": True, "min_role": "VIEWER"},
    {"path": "/projects", "name": "Projetos", "requires_auth": True, "min_role": "VIEWER"},
    {"path": "/executive", "name": "Dashboard Executivo", "requires_auth": True, "min_role": "VIEWER"},
    {"path": "/analytics", "name": "Analytics", "requires_auth": True, "min_role": "PROJECT_MANAGER"},
    {"path": "/billing", "name": "Faturamento", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/admin", "name": "Admin Panel", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/admin/users", "name": "Gestao Usuarios", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/admin/tenants", "name": "Gestao Tenants", "requires_auth": True, "min_role": "SUPER_ADMIN"},
    {"path": "/workers", "name": "Workers", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/integrations", "name": "Integracoes", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/security", "name": "Seguranca", "requires_auth": True, "min_role": "ADMIN"},
    {"path": "/docs", "name": "Documentacao", "requires_auth": True, "min_role": "VIEWER"},
]

# Criterios UX/UI
UX_CRITERIA = {
    "max_overlays_visible": 1,  # Maximo de overlays visiveis ao mesmo tempo
    "max_modals_open": 1,  # Maximo de modais abertos
    "min_content_area_percent": 60,  # Minimo de area util para conteudo
    "max_sidebar_width_percent": 25,  # Maximo de largura da sidebar
    "max_loading_time_ms": 3000,  # Tempo maximo de carregamento
    "min_clickable_size_px": 44,  # Tamanho minimo de elementos clicaveis (WCAG)
    "max_z_index_layers": 10,  # Maximo de camadas z-index
}

@dataclass
class TestResult:
    """Resultado de um teste"""
    test_name: str
    screen: str
    user: str
    tenant: str
    status: str  # passed, failed, warning
    category: str  # ui, ux, security, data, api
    message: str
    details: Dict = field(default_factory=dict)
    screenshot: Optional[str] = None
    agent_responsible: str = "QA"  # FRONT, BACK, SEC, DEVOPS, QA

@dataclass
class IssueToCreate:
    """Issue para criar no GitHub"""
    title: str
    body: str
    labels: List[str]
    agent: str  # Prefixo do agente
    priority: str  # P0, P1, P2, P3

# =============================================================================
# SEED DE DADOS
# =============================================================================

def hash_password(password: str) -> str:
    try:
        from passlib.context import CryptContext
        pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
        return pwd_context.hash(password)
    except ImportError:
        return hashlib.sha256(password.encode()).hexdigest()

def seed_test_data():
    """Cria dados de teste para todos os tenants"""
    print("\n" + "="*70)
    print("  [QA] SEED DE DADOS DE TESTE")
    print("="*70)

    from factory.database.connection import SessionLocal, init_db
    from factory.database.models import (
        User, Tenant, TenantSettings, BrandingConfig, TenantMember,
        Project, Story, StoryTask, Epic, Sprint
    )

    init_db()
    db = SessionLocal()

    try:
        # Criar Tenants
        print("\n[1/4] Criando/Atualizando Tenants...")
        for t_config in TENANTS_CONFIG:
            tenant = db.query(Tenant).filter(Tenant.tenant_id == t_config["tenant_id"]).first()
            if not tenant:
                tenant = Tenant(
                    tenant_id=t_config["tenant_id"],
                    name=t_config["name"],
                    slug=t_config["slug"],
                    plan=t_config["plan"],
                    status="active"
                )
                db.add(tenant)
                print(f"  + Tenant: {t_config['name']}")
            else:
                print(f"  ~ Tenant existe: {t_config['name']}")

        db.flush()

        # Criar Users
        print("\n[2/4] Criando/Atualizando Users...")
        for u_config in USERS_CONFIG:
            user = db.query(User).filter(User.username == u_config["username"]).first()
            if not user:
                user = User(
                    username=u_config["username"],
                    password_hash=hash_password(u_config["password"]),
                    email=f"{u_config['username']}@test.local",
                    role=u_config["role"],
                    active=True,
                    force_password_change=False
                )
                db.add(user)
                db.flush()
                print(f"  + User: {u_config['username']} ({u_config['role']})")

                # Associar ao tenant
                if u_config["tenant_id"]:
                    member = TenantMember(
                        tenant_id=u_config["tenant_id"],
                        user_id=user.id,
                        tenant_role="owner" if u_config["role"] == "ADMIN" else "member"
                    )
                    db.add(member)
            else:
                print(f"  ~ User existe: {u_config['username']}")

        db.flush()

        # Criar Projects e Stories para cada tenant
        print("\n[3/4] Criando Projects e Stories...")

        projects_per_tenant = {
            "BELGO-001": [
                {"id": "BELGO-ERP", "name": "Sistema ERP Integrado", "stories": 15},
                {"id": "BELGO-PORTAL", "name": "Portal do Cliente", "stories": 10},
                {"id": "BELGO-APP", "name": "App Mobile Vendas", "stories": 8},
            ],
            "TECH-001": [
                {"id": "TECH-SAAS", "name": "Plataforma SaaS", "stories": 12},
                {"id": "TECH-API", "name": "API Gateway", "stories": 6},
            ],
            "STARTUP-001": [
                {"id": "STARTUP-MVP", "name": "MVP Produto", "stories": 5},
            ]
        }

        story_count = 0
        for tenant_id, projects in projects_per_tenant.items():
            for proj in projects:
                existing = db.query(Project).filter(Project.project_id == proj["id"]).first()
                if not existing:
                    project = Project(
                        project_id=proj["id"],
                        tenant_id=tenant_id,
                        name=proj["name"],
                        description=f"Projeto {proj['name']} do tenant {tenant_id}",
                        status="IN_PROGRESS",
                        progress=30.0
                    )
                    db.add(project)
                    db.flush()
                    print(f"  + Project: {proj['name']} ({tenant_id})")

                    # Criar stories
                    statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
                    for i in range(proj["stories"]):
                        story_id = f"STR-{proj['id']}-{i+1:03d}"
                        existing_story = db.query(Story).filter(Story.story_id == story_id).first()
                        if not existing_story:
                            story = Story(
                                story_id=story_id,
                                tenant_id=tenant_id,
                                project_id=proj["id"],
                                title=f"Story {i+1} - {proj['name']}",
                                persona="usuario",
                                action=f"realizar acao {i+1}",
                                benefit="ter beneficio esperado",
                                story_points=[1,2,3,5,8,13][i % 6],
                                status=statuses[i % len(statuses)],
                                priority=["low", "medium", "high"][i % 3]
                            )
                            db.add(story)
                            story_count += 1
                else:
                    print(f"  ~ Project existe: {proj['name']}")

        print(f"  + {story_count} stories criadas")

        # Criar Branding para cada tenant
        print("\n[4/4] Configurando Branding...")
        for t_config in TENANTS_CONFIG:
            branding = db.query(BrandingConfig).filter(
                BrandingConfig.tenant_id == t_config["tenant_id"]
            ).first()

            if not branding:
                branding = BrandingConfig(
                    tenant_id=t_config["tenant_id"],
                    display_name=t_config["name"],
                    logo_url=t_config["branding"]["logo_url"],
                    colors={
                        "primary": t_config["branding"]["primary_color"],
                        "secondary": t_config["branding"]["secondary_color"]
                    }
                )
                db.add(branding)
                print(f"  + Branding: {t_config['name']}")
            else:
                print(f"  ~ Branding existe: {t_config['name']}")

        db.commit()

        # Estatisticas finais
        print("\n" + "="*70)
        print("  DADOS DE TESTE CRIADOS")
        print("="*70)
        print(f"  Tenants: {db.query(Tenant).count()}")
        print(f"  Users: {db.query(User).count()}")
        print(f"  Projects: {db.query(Project).count()}")
        print(f"  Stories: {db.query(Story).count()}")

        print("\n  Credenciais de Teste:")
        for u in USERS_CONFIG:
            print(f"    {u['username']}: {u['password']} ({u['role']}) - Tenant: {u['tenant_id'] or 'ALL'}")

        return True

    except Exception as e:
        print(f"\n  [ERRO] {e}")
        import traceback
        traceback.print_exc()
        db.rollback()
        return False
    finally:
        db.close()

# =============================================================================
# TESTES
# =============================================================================

BASE_URL = "http://localhost:9001"

async def run_complete_qa_tests():
    """Executa suite completa de testes QA"""
    from playwright.async_api import async_playwright

    print("\n" + "="*70)
    print("  [QA] SUITE COMPLETA DE TESTES")
    print("="*70)

    results: List[TestResult] = []
    issues: List[IssueToCreate] = []

    os.makedirs('tests/results', exist_ok=True)
    os.makedirs('tests/screenshots', exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)

        # =====================================================================
        # TESTE 1: LOGIN E SEGREGACAO DE DADOS
        # =====================================================================
        print("\n" + "-"*60)
        print("  TESTE 1: Login e Segregacao de Dados")
        print("-"*60)

        for user_config in USERS_CONFIG[:5]:  # Testar primeiros 5 usuarios
            context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
            page = await context.new_page()

            username = user_config["username"]
            password = user_config["password"]
            tenant_id = user_config["tenant_id"]
            can_see_all = user_config["can_see_all"]

            print(f"\n  >> Testando: {username} ({user_config['role']})")

            try:
                # Login
                await page.goto(f"{BASE_URL}/login")
                await page.wait_for_load_state('networkidle', timeout=10000)

                # Preencher formulario
                username_input = await page.query_selector('input[name="username"], #username, input[type="text"]')
                password_input = await page.query_selector('input[name="password"], #password, input[type="password"]')

                if username_input and password_input:
                    await username_input.fill(username)
                    await password_input.fill(password)

                    # Submit
                    submit_btn = await page.query_selector('button[type="submit"], .btn-login, button:has-text("Entrar")')
                    if submit_btn:
                        await submit_btn.click()
                        await asyncio.sleep(2)
                        await page.wait_for_load_state('networkidle', timeout=10000)

                        # Verificar se logou
                        current_url = page.url
                        if "/login" not in current_url:
                            print(f"     [OK] Login bem-sucedido")

                            # Screenshot pos-login
                            screenshot_path = f"tests/screenshots/login_{username}.png"
                            await page.screenshot(path=screenshot_path)

                            results.append(TestResult(
                                test_name="Login",
                                screen="/login",
                                user=username,
                                tenant=tenant_id or "PLATFORM",
                                status="passed",
                                category="security",
                                message="Login realizado com sucesso",
                                screenshot=screenshot_path
                            ))

                            # TESTE DE SEGREGACAO: Verificar projetos visiveis
                            await page.goto(f"{BASE_URL}/")
                            await asyncio.sleep(2)

                            # Buscar projetos na sidebar
                            sidebar_text = await page.evaluate('''
                                () => {
                                    const sidebar = document.querySelector('.sidebar, [class*="sidebar"], nav');
                                    return sidebar ? sidebar.textContent : '';
                                }
                            ''')

                            # Verificar se ve apenas dados do seu tenant
                            if not can_see_all:
                                # Usuario normal - deve ver apenas seu tenant
                                other_tenants = [t["tenant_id"] for t in TENANTS_CONFIG if t["tenant_id"] != tenant_id]

                                for other_tenant in other_tenants:
                                    # Verificar se dados de outro tenant aparecem
                                    other_projects = await page.query_selector_all(f'[data-tenant="{other_tenant}"]')

                                    if other_projects and len(other_projects) > 0:
                                        print(f"     [ERRO] Usuario ve dados de outro tenant: {other_tenant}")
                                        results.append(TestResult(
                                            test_name="Segregacao de Dados",
                                            screen="/",
                                            user=username,
                                            tenant=tenant_id,
                                            status="failed",
                                            category="security",
                                            message=f"Usuario consegue ver dados do tenant {other_tenant}",
                                            agent_responsible="SEC"
                                        ))

                                        issues.append(IssueToCreate(
                                            title=f"[SEC] Falha na segregacao de dados - {username} ve dados de {other_tenant}",
                                            body=f"""## Bug de Seguranca

**Severidade:** CRITICA

**Descricao:**
Usuario `{username}` do tenant `{tenant_id}` consegue visualizar dados do tenant `{other_tenant}`.

**Impacto:**
Vazamento de dados entre tenants - violacao de isolamento multi-tenant.

**Passos para Reproduzir:**
1. Login como `{username}` / `{password}`
2. Acessar dashboard principal
3. Verificar projetos visiveis na sidebar

**Comportamento Esperado:**
Usuario deve ver APENAS dados do seu tenant ({tenant_id}).

---
_Issue gerado automaticamente pelo Agente QA_
""",
                                            labels=["bug", "security", "P0", "critical"],
                                            agent="SEC",
                                            priority="P0"
                                        ))
                                    else:
                                        print(f"     [OK] Nao ve dados de {other_tenant}")

                            else:
                                # SUPER_ADMIN deve ver todos
                                print(f"     [OK] SUPER_ADMIN - acesso total esperado")

                        else:
                            print(f"     [ERRO] Falha no login - ainda em /login")
                            results.append(TestResult(
                                test_name="Login",
                                screen="/login",
                                user=username,
                                tenant=tenant_id or "PLATFORM",
                                status="failed",
                                category="security",
                                message="Login falhou - nao redirecionou",
                                agent_responsible="BACK"
                            ))
                else:
                    print(f"     [ERRO] Campos de login nao encontrados")
                    results.append(TestResult(
                        test_name="Login Form",
                        screen="/login",
                        user=username,
                        tenant=tenant_id or "PLATFORM",
                        status="failed",
                        category="ui",
                        message="Campos de login nao encontrados",
                        agent_responsible="FRONT"
                    ))

            except Exception as e:
                print(f"     [ERRO] {str(e)[:50]}")
                results.append(TestResult(
                    test_name="Login",
                    screen="/login",
                    user=username,
                    tenant=tenant_id or "PLATFORM",
                    status="failed",
                    category="api",
                    message=str(e)[:100],
                    agent_responsible="BACK"
                ))

            await context.close()

        # =====================================================================
        # TESTE 2: UX/UI - OVERLAYS E APROVEITAMENTO DE TELA
        # =====================================================================
        print("\n" + "-"*60)
        print("  TESTE 2: UX/UI - Overlays e Aproveitamento de Tela")
        print("-"*60)

        # Login como admin para testar todas as telas
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        # Capturar erros de console
        console_errors = []
        page.on("console", lambda msg: console_errors.append(msg.text) if msg.type == "error" else None)

        # Login
        await page.goto(f"{BASE_URL}/login")
        await page.wait_for_load_state('networkidle', timeout=10000)

        username_input = await page.query_selector('input[name="username"], #username, input[type="text"]')
        password_input = await page.query_selector('input[name="password"], #password, input[type="password"]')

        if username_input and password_input:
            await username_input.fill("platform_admin")
            await password_input.fill("Platform@2024!")
            submit_btn = await page.query_selector('button[type="submit"]')
            if submit_btn:
                await submit_btn.click()
                await asyncio.sleep(3)

        for screen in SCREENS_TO_TEST:
            if screen["path"] == "/login":
                continue

            print(f"\n  >> Testando UX/UI: {screen['name']} ({screen['path']})")
            console_errors.clear()

            try:
                await page.goto(f"{BASE_URL}{screen['path']}", timeout=15000)
                await page.wait_for_load_state('networkidle', timeout=10000)
                await asyncio.sleep(1)

                # Analise UX/UI
                ux_analysis = await page.evaluate('''
                    () => {
                        const result = {
                            overlays: 0,
                            modals: 0,
                            zIndexLayers: new Set(),
                            contentAreaPercent: 0,
                            sidebarWidthPercent: 0,
                            clickableElements: [],
                            loadingIndicators: 0,
                            emptyStates: 0,
                            errors: []
                        };

                        // Contar overlays/modals visiveis
                        const overlays = document.querySelectorAll('[class*="overlay"], [class*="modal"], [class*="backdrop"]');
                        overlays.forEach(el => {
                            const style = window.getComputedStyle(el);
                            if (style.display !== 'none' && style.visibility !== 'hidden' && style.opacity !== '0') {
                                if (el.classList.toString().includes('modal')) {
                                    result.modals++;
                                } else {
                                    result.overlays++;
                                }
                            }
                        });

                        // Analisar z-index
                        document.querySelectorAll('*').forEach(el => {
                            const zIndex = window.getComputedStyle(el).zIndex;
                            if (zIndex !== 'auto' && parseInt(zIndex) > 0) {
                                result.zIndexLayers.add(parseInt(zIndex));
                            }
                        });
                        result.zIndexLayers = result.zIndexLayers.size;

                        // Calcular area de conteudo
                        const viewport = { width: window.innerWidth, height: window.innerHeight };
                        const sidebar = document.querySelector('.sidebar, [class*="sidebar"], nav');
                        const header = document.querySelector('header, [class*="header"]');

                        let usedWidth = viewport.width;
                        let usedHeight = viewport.height;

                        if (sidebar) {
                            const rect = sidebar.getBoundingClientRect();
                            result.sidebarWidthPercent = Math.round((rect.width / viewport.width) * 100);
                            usedWidth -= rect.width;
                        }

                        if (header) {
                            const rect = header.getBoundingClientRect();
                            usedHeight -= rect.height;
                        }

                        result.contentAreaPercent = Math.round((usedWidth * usedHeight) / (viewport.width * viewport.height) * 100);

                        // Verificar elementos clicaveis pequenos
                        const clickables = document.querySelectorAll('button, a, [onclick], [role="button"]');
                        clickables.forEach(el => {
                            const rect = el.getBoundingClientRect();
                            if (rect.width > 0 && rect.height > 0 && (rect.width < 44 || rect.height < 44)) {
                                result.clickableElements.push({
                                    tag: el.tagName,
                                    width: rect.width,
                                    height: rect.height,
                                    text: el.textContent?.substring(0, 20)
                                });
                            }
                        });

                        // Loading indicators
                        result.loadingIndicators = document.querySelectorAll('[class*="loading"], [class*="spinner"], .skeleton').length;

                        // Empty states
                        result.emptyStates = document.querySelectorAll('[class*="empty"], [class*="no-data"]').length;

                        // Verificar Vue templates nao renderizados
                        if (document.body.textContent.includes('{{') && document.body.textContent.includes('}}')) {
                            result.errors.push('Vue templates {{ }} nao renderizados');
                        }

                        return result;
                    }
                ''')

                # Screenshot
                screenshot_path = f"tests/screenshots/ux_{screen['path'].replace('/', '_').strip('_') or 'home'}.png"
                await page.screenshot(path=screenshot_path)

                # Avaliar resultados
                ux_issues = []

                # Overlays
                if ux_analysis["overlays"] > UX_CRITERIA["max_overlays_visible"]:
                    ux_issues.append(f"Muitos overlays visiveis: {ux_analysis['overlays']}")

                # Modals
                if ux_analysis["modals"] > UX_CRITERIA["max_modals_open"]:
                    ux_issues.append(f"Muitos modais abertos: {ux_analysis['modals']}")

                # Area de conteudo
                if ux_analysis["contentAreaPercent"] < UX_CRITERIA["min_content_area_percent"]:
                    ux_issues.append(f"Area de conteudo pequena: {ux_analysis['contentAreaPercent']}%")

                # Sidebar
                if ux_analysis["sidebarWidthPercent"] > UX_CRITERIA["max_sidebar_width_percent"]:
                    ux_issues.append(f"Sidebar muito larga: {ux_analysis['sidebarWidthPercent']}%")

                # Z-index layers
                if ux_analysis["zIndexLayers"] > UX_CRITERIA["max_z_index_layers"]:
                    ux_issues.append(f"Muitas camadas z-index: {ux_analysis['zIndexLayers']}")

                # Elementos clicaveis pequenos
                small_elements = len(ux_analysis["clickableElements"])
                if small_elements > 5:
                    ux_issues.append(f"{small_elements} elementos clicaveis menores que 44px (WCAG)")

                # Vue errors
                if ux_analysis.get("errors"):
                    ux_issues.extend(ux_analysis["errors"])

                # Console errors
                if console_errors:
                    ux_issues.append(f"{len(console_errors)} erros no console")

                # Registrar resultado
                if ux_issues:
                    print(f"     [WARN] {len(ux_issues)} problemas UX/UI encontrados")
                    for issue in ux_issues[:3]:
                        print(f"            - {issue}")

                    results.append(TestResult(
                        test_name="UX/UI Analysis",
                        screen=screen["path"],
                        user="platform_admin",
                        tenant="PLATFORM",
                        status="warning",
                        category="ux",
                        message="; ".join(ux_issues),
                        details=ux_analysis,
                        screenshot=screenshot_path,
                        agent_responsible="FRONT"
                    ))

                    # Criar issue se houver problemas graves
                    if any("Vue templates" in i for i in ux_issues):
                        issues.append(IssueToCreate(
                            title=f"[FRONT] Vue nao renderizando em {screen['name']}",
                            body=f"""## Bug de Frontend

**Tela:** {screen['name']} ({screen['path']})

**Problema:**
Templates Vue ({{ }}) estao visiveis na tela, indicando que o Vue.js nao montou corretamente.

**Impacto:**
Interface completamente quebrada para o usuario.

**Screenshot:** {screenshot_path}

---
_Issue gerado automaticamente pelo Agente QA_
""",
                            labels=["bug", "frontend", "P0"],
                            agent="FRONT",
                            priority="P0"
                        ))
                else:
                    print(f"     [OK] UX/UI aprovado")
                    print(f"         Content: {ux_analysis['contentAreaPercent']}% | Sidebar: {ux_analysis['sidebarWidthPercent']}%")

                    results.append(TestResult(
                        test_name="UX/UI Analysis",
                        screen=screen["path"],
                        user="platform_admin",
                        tenant="PLATFORM",
                        status="passed",
                        category="ux",
                        message="UX/UI aprovado",
                        details=ux_analysis,
                        screenshot=screenshot_path
                    ))

            except Exception as e:
                print(f"     [ERRO] {str(e)[:50]}")
                results.append(TestResult(
                    test_name="UX/UI Analysis",
                    screen=screen["path"],
                    user="platform_admin",
                    tenant="PLATFORM",
                    status="failed",
                    category="api",
                    message=str(e)[:100],
                    agent_responsible="BACK" if "timeout" in str(e).lower() else "FRONT"
                ))

        # =====================================================================
        # TESTE 3: DADOS DO BANCO
        # =====================================================================
        print("\n" + "-"*60)
        print("  TESTE 3: Verificacao de Dados do Banco")
        print("-"*60)

        await page.goto(f"{BASE_URL}/")
        await asyncio.sleep(2)

        # Verificar se ha stories no kanban
        story_cards = await page.query_selector_all('.story-card, [class*="story"], [class*="card"]')
        print(f"  Story cards encontrados: {len(story_cards)}")

        if len(story_cards) == 0:
            print("  [ERRO] Nenhum story card encontrado - dados nao estao carregando")
            results.append(TestResult(
                test_name="Dados do Banco",
                screen="/",
                user="platform_admin",
                tenant="PLATFORM",
                status="failed",
                category="data",
                message="Nenhum story card encontrado no kanban",
                agent_responsible="BACK"
            ))

            issues.append(IssueToCreate(
                title="[BACK] Stories nao carregando no Kanban",
                body="""## Bug de Backend

**Problema:**
Nenhum story card esta sendo exibido no Kanban board, mesmo havendo dados no banco.

**Verificacao:**
- Banco tem stories? SIM
- API retorna dados? VERIFICAR
- Frontend renderiza? NAO

**Impacto:**
Kanban completamente vazio - funcionalidade principal quebrada.

---
_Issue gerado automaticamente pelo Agente QA_
""",
                labels=["bug", "backend", "P1", "api"],
                agent="BACK",
                priority="P1"
            ))
        else:
            print(f"  [OK] {len(story_cards)} stories encontrados no kanban")
            results.append(TestResult(
                test_name="Dados do Banco",
                screen="/",
                user="platform_admin",
                tenant="PLATFORM",
                status="passed",
                category="data",
                message=f"{len(story_cards)} stories carregadas do banco"
            ))

        # Verificar projetos na sidebar
        projects_in_sidebar = await page.query_selector_all('.sidebar [class*="project"], nav [class*="project"]')
        print(f"  Projetos na sidebar: {len(projects_in_sidebar)}")

        # Manter browser aberto para visualizacao
        print("\n  Browser aberto por 20s para inspecao manual...")
        await asyncio.sleep(20)

        await context.close()
        await browser.close()

    return results, issues

def create_github_issues(issues: List[IssueToCreate]):
    """Cria issues no GitHub com prefixos corretos"""
    print("\n" + "="*70)
    print("  [QA] CRIANDO ISSUES NO GITHUB")
    print("="*70)

    import subprocess

    created = []
    for issue in issues:
        try:
            labels = ",".join(issue.labels)
            result = subprocess.run(
                ["gh", "issue", "create",
                 "--title", issue.title,
                 "--body", issue.body,
                 "--label", labels],
                capture_output=True,
                text=True,
                cwd=str(Path(__file__).parent.parent)
            )

            if result.returncode == 0:
                url = result.stdout.strip()
                print(f"  [OK] {issue.agent}: {issue.title[:50]}...")
                print(f"       URL: {url}")
                created.append({"title": issue.title, "url": url, "agent": issue.agent})
            else:
                print(f"  [ERRO] {issue.title[:50]}: {result.stderr[:50]}")

        except Exception as e:
            print(f"  [ERRO] {issue.title[:50]}: {str(e)[:50]}")

    return created

def generate_report(results: List[TestResult], issues: List[IssueToCreate], created_issues: List):
    """Gera relatorio de testes"""

    report = {
        "timestamp": datetime.now().isoformat(),
        "summary": {
            "total_tests": len(results),
            "passed": len([r for r in results if r.status == "passed"]),
            "failed": len([r for r in results if r.status == "failed"]),
            "warnings": len([r for r in results if r.status == "warning"]),
        },
        "by_category": {},
        "by_agent": {},
        "issues_created": created_issues,
        "results": [vars(r) for r in results]
    }

    # Agrupar por categoria
    for r in results:
        if r.category not in report["by_category"]:
            report["by_category"][r.category] = {"passed": 0, "failed": 0, "warning": 0}
        report["by_category"][r.category][r.status] = report["by_category"][r.category].get(r.status, 0) + 1

    # Agrupar por agente
    for r in results:
        if r.status in ["failed", "warning"]:
            agent = r.agent_responsible
            if agent not in report["by_agent"]:
                report["by_agent"][agent] = []
            report["by_agent"][agent].append(r.message[:100])

    # Salvar JSON
    with open("tests/results/qa_report.json", "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False, default=str)

    # Imprimir resumo
    print("\n" + "="*70)
    print("  [QA] RELATORIO FINAL")
    print("="*70)
    print(f"""
  Total de Testes: {report['summary']['total_tests']}
  ----------------------------
  Passed:   {report['summary']['passed']}
  Failed:   {report['summary']['failed']}
  Warnings: {report['summary']['warnings']}

  Por Categoria:
""")
    for cat, stats in report["by_category"].items():
        print(f"    {cat}: {stats}")

    if report["by_agent"]:
        print("\n  Issues por Agente:")
        for agent, msgs in report["by_agent"].items():
            print(f"    [{agent}]: {len(msgs)} problema(s)")

    if created_issues:
        print(f"\n  Issues Criadas: {len(created_issues)}")
        for issue in created_issues:
            print(f"    - [{issue['agent']}] {issue['url']}")

    print(f"\n  Relatorio salvo em: tests/results/qa_report.json")
    print("="*70)

    return report

# =============================================================================
# MAIN
# =============================================================================

def main():
    print("\n" + "+"*70)
    print("+  [QA] SUITE COMPLETA DE TESTES - PLATAFORMA E")
    print("+  Multi-Tenancy | UX/UI | Segregacao de Dados")
    print("+"*70)
    print(f"\n  Inicio: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    # 1. Seed de dados
    if not seed_test_data():
        print("\n  [ERRO] Falha no seed de dados")

    # 2. Executar testes
    try:
        results, issues = asyncio.run(run_complete_qa_tests())
    except Exception as e:
        print(f"\n  [ERRO] Falha nos testes: {e}")
        import traceback
        traceback.print_exc()
        results, issues = [], []

    # 3. Criar issues no GitHub
    created_issues = []
    if issues:
        created_issues = create_github_issues(issues)

    # 4. Gerar relatorio
    report = generate_report(results, issues, created_issues)

    # 5. Status final
    print("\n" + "="*70)
    print("  STATUS: completed")
    print(f"  HANDOFF: [ORCH] Orquestrador - {len(created_issues)} issues criadas")
    print("  ARQUIVOS: tests/results/qa_report.json, tests/screenshots/")
    print("="*70)

if __name__ == "__main__":
    main()
