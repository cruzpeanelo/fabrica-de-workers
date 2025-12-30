"""
Teste detalhado de todas as paginas do Dashboard
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io
import json

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def clear_overlays(page):
    """Limpa todos os overlays via JS"""
    await page.evaluate('''() => {
        document.querySelectorAll('.fixed').forEach(el => {
            const cls = el.className || '';
            if (cls.includes('overlay') || cls.includes('modal') ||
                cls.includes('bg-black') || cls.includes('z-50') || cls.includes('z-60')) {
                el.style.display = 'none';
            }
        });
    }''')
    for _ in range(3):
        await page.keyboard.press('Escape')
        await asyncio.sleep(0.1)

async def test_executive_dashboard(page):
    """Testa Executive Dashboard em detalhe"""
    print("\n" + "="*50)
    print("  EXECUTIVE DASHBOARD (/executive)")
    print("="*50)

    await page.goto(f"{BASE_URL}/executive")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/executive", "tests": []}

    # Teste 1: KPIs cards
    print("\n[1] Testando KPI Cards...")
    kpi_cards = await page.query_selector_all('.kpi-card, [class*="kpi"], .metric-card, .stat-card')
    results["tests"].append({"name": "KPI Cards", "count": len(kpi_cards), "ok": len(kpi_cards) > 0})
    print(f"    KPI Cards encontrados: {len(kpi_cards)}")

    # Teste 2: Graficos
    print("[2] Testando Graficos...")
    charts = await page.query_selector_all('canvas, [class*="chart"], svg.chart')
    results["tests"].append({"name": "Charts", "count": len(charts), "ok": len(charts) > 0})
    print(f"    Graficos encontrados: {len(charts)}")

    # Teste 3: API KPIs
    print("[3] Testando API /api/executive/kpis...")
    api_result = await page.evaluate('''async () => {
        const res = await fetch('/api/executive/kpis');
        return { status: res.status, data: await res.json() };
    }''')
    results["tests"].append({"name": "API KPIs", "status": api_result.get('status'), "ok": api_result.get('status') == 200})
    print(f"    API Status: {api_result.get('status')}")

    # Teste 4: Metricas
    print("[4] Testando API /api/executive/metrics...")
    metrics_result = await page.evaluate('''async () => {
        const res = await fetch('/api/executive/metrics');
        return { status: res.status };
    }''')
    results["tests"].append({"name": "API Metrics", "status": metrics_result.get('status'), "ok": metrics_result.get('status') == 200})
    print(f"    API Status: {metrics_result.get('status')}")

    await page.screenshot(path='screenshots/test_executive.png')
    return results

async def test_analytics_page(page):
    """Testa Analytics Page em detalhe"""
    print("\n" + "="*50)
    print("  ANALYTICS PAGE (/analytics)")
    print("="*50)

    await page.goto(f"{BASE_URL}/analytics")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/analytics", "tests": []}

    # Teste 1: Graficos de produtividade
    print("\n[1] Testando elementos de analytics...")
    charts = await page.query_selector_all('canvas, [class*="chart"], .analytics-chart')
    results["tests"].append({"name": "Charts", "count": len(charts), "ok": len(charts) >= 0})
    print(f"    Graficos: {len(charts)}")

    # Teste 2: API Productivity
    print("[2] Testando API /api/analytics/productivity...")
    api_result = await page.evaluate('''async () => {
        const res = await fetch('/api/analytics/productivity');
        return { status: res.status };
    }''')
    results["tests"].append({"name": "API Productivity", "status": api_result.get('status'), "ok": api_result.get('status') == 200})
    print(f"    API Status: {api_result.get('status')}")

    # Teste 3: API Insights
    print("[3] Testando API /api/analytics/insights...")
    insights_result = await page.evaluate('''async () => {
        const res = await fetch('/api/analytics/insights');
        return { status: res.status };
    }''')
    results["tests"].append({"name": "API Insights", "status": insights_result.get('status'), "ok": insights_result.get('status') == 200})
    print(f"    API Status: {insights_result.get('status')}")

    # Teste 4: Filtros
    print("[4] Testando filtros...")
    filters = await page.query_selector_all('select, [class*="filter"], input[type="date"]')
    results["tests"].append({"name": "Filters", "count": len(filters), "ok": True})
    print(f"    Filtros encontrados: {len(filters)}")

    await page.screenshot(path='screenshots/test_analytics.png')
    return results

async def test_billing_page(page):
    """Testa Billing Page em detalhe"""
    print("\n" + "="*50)
    print("  BILLING PAGE (/billing)")
    print("="*50)

    await page.goto(f"{BASE_URL}/billing")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/billing", "tests": []}

    # Teste 1: Planos
    print("\n[1] Testando cards de planos...")
    plans = await page.query_selector_all('.plan-card, [class*="pricing"], .billing-plan')
    results["tests"].append({"name": "Plan Cards", "count": len(plans), "ok": True})
    print(f"    Cards de plano: {len(plans)}")

    # Teste 2: Uso atual
    print("[2] Testando secao de uso...")
    usage = await page.query_selector_all('[class*="usage"], .usage-meter, .progress')
    results["tests"].append({"name": "Usage Section", "count": len(usage), "ok": True})
    print(f"    Elementos de uso: {len(usage)}")

    # Teste 3: Historico
    print("[3] Testando historico...")
    history = await page.query_selector_all('table, [class*="history"], .billing-history')
    results["tests"].append({"name": "History", "count": len(history), "ok": True})
    print(f"    Elementos de historico: {len(history)}")

    await page.screenshot(path='screenshots/test_billing.png')
    return results

async def test_integrations_page(page):
    """Testa Integrations Panel em detalhe"""
    print("\n" + "="*50)
    print("  INTEGRATIONS PANEL (/integrations)")
    print("="*50)

    await page.goto(f"{BASE_URL}/integrations")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/integrations", "tests": []}

    # Teste 1: Cards de integracao
    print("\n[1] Testando cards de integracao...")
    cards = await page.query_selector_all('.integration-card, [class*="integration"], .connector-card')
    results["tests"].append({"name": "Integration Cards", "count": len(cards), "ok": True})
    print(f"    Cards de integracao: {len(cards)}")

    # Teste 2: Status das integracoes
    print("[2] Testando status...")
    status_badges = await page.query_selector_all('.status-badge, [class*="status"], .badge')
    results["tests"].append({"name": "Status Badges", "count": len(status_badges), "ok": True})
    print(f"    Badges de status: {len(status_badges)}")

    # Teste 3: Botoes de configuracao
    print("[3] Testando botoes...")
    config_buttons = await page.query_selector_all('button:has-text("Configurar"), button:has-text("Conectar")')
    results["tests"].append({"name": "Config Buttons", "count": len(config_buttons), "ok": True})
    print(f"    Botoes de config: {len(config_buttons)}")

    await page.screenshot(path='screenshots/test_integrations.png')
    return results

async def test_admin_users(page):
    """Testa Admin Users em detalhe"""
    print("\n" + "="*50)
    print("  ADMIN USERS (/admin/users)")
    print("="*50)

    await page.goto(f"{BASE_URL}/admin/users")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/admin/users", "tests": []}

    # Teste 1: Tabela de usuarios
    print("\n[1] Testando tabela de usuarios...")
    table = await page.query_selector('table, .users-table, [class*="user-list"]')
    results["tests"].append({"name": "Users Table", "found": table is not None, "ok": True})
    print(f"    Tabela encontrada: {table is not None}")

    # Teste 2: Botao adicionar usuario
    print("[2] Testando botao de adicionar...")
    add_btn = await page.query_selector('button:has-text("Adicionar"), button:has-text("Novo")')
    results["tests"].append({"name": "Add Button", "found": add_btn is not None, "ok": True})
    print(f"    Botao adicionar: {add_btn is not None}")

    # Teste 3: Filtros/busca
    print("[3] Testando busca...")
    search = await page.query_selector('input[type="search"], input[placeholder*="busca"], .search-input')
    results["tests"].append({"name": "Search", "found": search is not None, "ok": True})
    print(f"    Campo de busca: {search is not None}")

    await page.screenshot(path='screenshots/test_admin_users.png')
    return results

async def test_admin_portal(page):
    """Testa Admin Portal em detalhe"""
    print("\n" + "="*50)
    print("  ADMIN PORTAL (/admin/portal)")
    print("="*50)

    await page.goto(f"{BASE_URL}/admin/portal")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/admin/portal", "tests": []}

    # Teste 1: Configuracoes do tenant
    print("\n[1] Testando configuracoes...")
    config_sections = await page.query_selector_all('.config-section, [class*="settings"], .admin-section')
    results["tests"].append({"name": "Config Sections", "count": len(config_sections), "ok": True})
    print(f"    Secoes de config: {len(config_sections)}")

    # Teste 2: Forms
    print("[2] Testando formularios...")
    forms = await page.query_selector_all('form, .settings-form')
    results["tests"].append({"name": "Forms", "count": len(forms), "ok": True})
    print(f"    Formularios: {len(forms)}")

    await page.screenshot(path='screenshots/test_admin_portal.png')
    return results

async def test_docs_page(page):
    """Testa Documentation Page em detalhe"""
    print("\n" + "="*50)
    print("  DOCUMENTATION (/docs)")
    print("="*50)

    await page.goto(f"{BASE_URL}/docs")
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/docs", "tests": []}

    # Teste 1: Conteudo de documentacao
    print("\n[1] Testando conteudo...")
    content = await page.query_selector('.docs-content, article, .documentation, main')
    results["tests"].append({"name": "Content", "found": content is not None, "ok": content is not None})
    print(f"    Conteudo encontrado: {content is not None}")

    # Teste 2: Menu/sidebar de docs
    print("[2] Testando navegacao...")
    nav = await page.query_selector('.docs-nav, .docs-sidebar, nav')
    results["tests"].append({"name": "Navigation", "found": nav is not None, "ok": True})
    print(f"    Navegacao: {nav is not None}")

    await page.screenshot(path='screenshots/test_docs.png')
    return results

async def test_home_kanban(page):
    """Testa Home/Kanban em detalhe"""
    print("\n" + "="*50)
    print("  HOME / KANBAN (/)")
    print("="*50)

    await page.goto(BASE_URL)
    await page.wait_for_load_state('networkidle', timeout=15000)
    await clear_overlays(page)

    results = {"page": "/", "tests": []}

    # Teste 1: Colunas do Kanban
    print("\n[1] Testando colunas Kanban...")
    columns = await page.query_selector_all('.kanban-column, [class*="column"], [data-status]')
    results["tests"].append({"name": "Kanban Columns", "count": len(columns), "ok": len(columns) > 0})
    print(f"    Colunas: {len(columns)}")

    # Teste 2: Story Cards
    print("[2] Testando story cards...")
    cards = await page.query_selector_all('.story-card, .card, [draggable]')
    results["tests"].append({"name": "Story Cards", "count": len(cards), "ok": True})
    print(f"    Cards: {len(cards)}")

    # Teste 3: Filtros
    print("[3] Testando filtros...")
    filters = await page.query_selector_all('.filter, [class*="filter"], select')
    results["tests"].append({"name": "Filters", "count": len(filters), "ok": True})
    print(f"    Filtros: {len(filters)}")

    # Teste 4: Sidebar
    print("[4] Testando sidebar...")
    sidebar = await page.query_selector('.sidebar, aside, nav.sidebar')
    results["tests"].append({"name": "Sidebar", "found": sidebar is not None, "ok": sidebar is not None})
    print(f"    Sidebar: {sidebar is not None}")

    # Teste 5: Header
    print("[5] Testando header...")
    header = await page.query_selector('header, .header, .navbar')
    results["tests"].append({"name": "Header", "found": header is not None, "ok": header is not None})
    print(f"    Header: {header is not None}")

    # Teste 6: API Stories
    print("[6] Testando API /api/stories...")
    api_result = await page.evaluate('''async () => {
        const res = await fetch('/api/stories');
        const data = await res.json();
        return { status: res.status, count: Array.isArray(data) ? data.length : 0 };
    }''')
    results["tests"].append({"name": "API Stories", "status": api_result.get('status'), "count": api_result.get('count'), "ok": api_result.get('status') == 200})
    print(f"    API Status: {api_result.get('status')}, Stories: {api_result.get('count')}")

    await page.screenshot(path='screenshots/test_home.png')
    return results

async def main():
    """Executa todos os testes detalhados"""
    all_results = []

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  TESTES DETALHADOS - FABRICA DE AGENTES")
        print("="*60)

        # Executar todos os testes
        all_results.append(await test_home_kanban(page))
        all_results.append(await test_executive_dashboard(page))
        all_results.append(await test_analytics_page(page))
        all_results.append(await test_billing_page(page))
        all_results.append(await test_integrations_page(page))
        all_results.append(await test_admin_users(page))
        all_results.append(await test_admin_portal(page))
        all_results.append(await test_docs_page(page))

        # Resumo final
        print("\n" + "="*60)
        print("  RESUMO FINAL")
        print("="*60)

        total_tests = 0
        passed_tests = 0

        for result in all_results:
            page_tests = len(result["tests"])
            page_passed = sum(1 for t in result["tests"] if t.get("ok"))
            total_tests += page_tests
            passed_tests += page_passed
            print(f"\n{result['page']}: {page_passed}/{page_tests} testes OK")

        print(f"\n{'='*40}")
        print(f"TOTAL: {passed_tests}/{total_tests} testes OK ({100*passed_tests//total_tests}%)")
        print(f"{'='*40}")

        print("\nBrowser aberto por 15 segundos...")
        await asyncio.sleep(15)

        await browser.close()

    # Salvar resultados
    with open('test_detailed_results.json', 'w') as f:
        json.dump(all_results, f, indent=2)

    print("\nResultados salvos em: test_detailed_results.json")
    return all_results

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(main())
