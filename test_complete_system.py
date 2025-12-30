"""
Teste COMPLETO de todo o sistema - APIs e Paginas
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io
import json

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

# TODAS as paginas HTML
PAGES = [
    "/",
    "/admin",
    "/admin/users/",
    "/admin/portal/",
    "/admin/portal/platform",
    "/analytics",
    "/billing",
    "/executive",
    "/integrations",
    "/security",
    "/workers",
    "/projects",
    "/docs",
    "/login",
    "/register",
    "/forgot-password",
    "/settings",
    "/profile",
    "/notifications",
    "/help",
    "/onboarding",
]

# TODAS as APIs
APIS = [
    # Stories
    {"path": "/api/stories", "method": "GET", "name": "List Stories"},
    {"path": "/api/projects", "method": "GET", "name": "List Projects"},

    # Tenant/Auth
    {"path": "/api/tenants", "method": "GET", "name": "List Tenants"},
    {"path": "/api/tenant/current", "method": "GET", "name": "Current Tenant"},

    # Executive
    {"path": "/api/executive/kpis", "method": "GET", "name": "Executive KPIs"},
    {"path": "/api/executive/metrics", "method": "GET", "name": "Executive Metrics"},

    # Analytics
    {"path": "/api/analytics/productivity", "method": "GET", "name": "Analytics Productivity"},
    {"path": "/api/analytics/insights", "method": "GET", "name": "Analytics Insights"},
    {"path": "/api/analytics/velocity-history", "method": "GET", "name": "Velocity History"},

    # Chat
    {"path": "/api/chat/history", "method": "GET", "name": "Chat History"},

    # Search
    {"path": "/api/search?q=test", "method": "GET", "name": "Global Search"},

    # Templates
    {"path": "/api/templates", "method": "GET", "name": "Story Templates"},

    # Gamification
    {"path": "/api/gamification/leaderboard", "method": "GET", "name": "Leaderboard"},
    {"path": "/api/gamification/badges", "method": "GET", "name": "Badges"},
    {"path": "/api/gamification/levels", "method": "GET", "name": "Levels"},
    {"path": "/api/gamification/points", "method": "GET", "name": "Points"},

    # Focus Mode
    {"path": "/api/focus/history", "method": "GET", "name": "Focus History"},
    {"path": "/api/focus/stats", "method": "GET", "name": "Focus Stats"},

    # My Work
    {"path": "/api/my-work/summary", "method": "GET", "name": "My Work Summary"},
    {"path": "/api/my-work/deadlines", "method": "GET", "name": "Deadlines"},
    {"path": "/api/my-work/activity", "method": "GET", "name": "Activity"},
    {"path": "/api/my-work/metrics", "method": "GET", "name": "My Metrics"},

    # Calendar
    {"path": "/api/calendar/stories", "method": "GET", "name": "Calendar Stories"},
    {"path": "/api/calendar/sprints", "method": "GET", "name": "Calendar Sprints"},

    # Monitoring
    {"path": "/api/monitoring/dashboard", "method": "GET", "name": "Monitoring Dashboard"},
    {"path": "/api/monitoring/workers", "method": "GET", "name": "Workers Status"},
    {"path": "/api/monitoring/stats", "method": "GET", "name": "Monitoring Stats"},
    {"path": "/api/monitoring/logs", "method": "GET", "name": "Monitoring Logs"},
    {"path": "/api/monitoring/timeline", "method": "GET", "name": "Timeline"},

    # Integrations
    {"path": "/api/integrations/status", "method": "GET", "name": "Integrations Status"},
    {"path": "/api/integrations/all-status", "method": "GET", "name": "All Integrations Status"},
    {"path": "/api/integrations/config", "method": "GET", "name": "Integrations Config"},

    # Audit
    {"path": "/api/audit/logs", "method": "GET", "name": "Audit Logs"},
    {"path": "/api/audit/summary", "method": "GET", "name": "Audit Summary"},
    {"path": "/api/audit/alerts", "method": "GET", "name": "Audit Alerts"},

    # Help Center
    {"path": "/api/help/articles", "method": "GET", "name": "Help Articles"},
    {"path": "/api/help/faq", "method": "GET", "name": "FAQ"},
    {"path": "/api/help/search?q=como", "method": "GET", "name": "Help Search"},

    # Themes
    {"path": "/api/themes", "method": "GET", "name": "Themes"},

    # Webhooks
    {"path": "/api/webhooks/list", "method": "GET", "name": "Webhooks List"},

    # Export
    {"path": "/api/export/stories/csv", "method": "GET", "name": "Export Stories CSV"},

    # Status
    {"path": "/api/status", "method": "GET", "name": "System Status"},
    {"path": "/health", "method": "GET", "name": "Health Check"},

    # Epics/Sprints
    {"path": "/api/epics", "method": "GET", "name": "Epics"},
]

async def test_all_pages(page):
    """Testa todas as paginas HTML"""
    results = []

    print("\n" + "="*60)
    print("  TESTE DE TODAS AS PAGINAS HTML")
    print("="*60)

    for path in PAGES:
        try:
            response = await page.goto(f"{BASE_URL}{path}", timeout=10000)
            status = response.status if response else "N/A"

            # Verificar conteudo
            body = await page.query_selector('body')
            body_text = await body.text_content() if body else ""
            has_content = len(body_text.strip()) > 50

            result = {
                "path": path,
                "status": status,
                "has_content": has_content,
                "ok": status in [200, 307]
            }
            results.append(result)

            icon = "[OK]" if status == 200 else "[307]" if status == 307 else "[404]" if status == 404 else "[ERR]"
            print(f"  {icon} {path}: {status}")

        except Exception as e:
            results.append({"path": path, "status": "ERROR", "error": str(e)[:50], "ok": False})
            print(f"  [ERR] {path}: {str(e)[:40]}")

    return results

async def test_all_apis(page):
    """Testa todas as APIs"""
    results = []

    print("\n" + "="*60)
    print("  TESTE DE TODAS AS APIs")
    print("="*60)

    for api in APIS:
        try:
            api_result = await page.evaluate(f'''async () => {{
                try {{
                    const res = await fetch('{api["path"]}');
                    let data = null;
                    try {{ data = await res.json(); }} catch {{}}
                    return {{
                        status: res.status,
                        ok: res.ok,
                        hasData: data !== null
                    }};
                }} catch(e) {{
                    return {{ error: e.message }};
                }}
            }}''')

            result = {
                "path": api["path"],
                "name": api["name"],
                **api_result
            }
            results.append(result)

            status = api_result.get('status', 'ERR')
            if status == 200:
                icon = "[OK]"
            elif status == 401:
                icon = "[401]"
            elif status == 404:
                icon = "[404]"
            elif status == 500:
                icon = "[500]"
            else:
                icon = f"[{status}]"

            print(f"  {icon} {api['name']}: {api['path']}")

        except Exception as e:
            results.append({"path": api["path"], "name": api["name"], "error": str(e)[:50]})
            print(f"  [ERR] {api['name']}: {str(e)[:40]}")

    return results

async def main():
    """Executa todos os testes"""
    all_results = {"pages": [], "apis": [], "issues": []}

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=100)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        # Limpar overlays
        await page.goto(BASE_URL)
        await page.wait_for_load_state('domcontentloaded', timeout=10000)
        await page.evaluate('''() => {
            document.querySelectorAll('.fixed').forEach(el => {
                const cls = el.className || '';
                if (cls.includes('overlay') || cls.includes('modal') || cls.includes('bg-black')) {
                    el.style.display = 'none';
                }
            });
        }''')
        for _ in range(3):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.1)

        # Testar paginas
        all_results["pages"] = await test_all_pages(page)

        # Testar APIs
        await page.goto(BASE_URL)
        await asyncio.sleep(0.5)
        all_results["apis"] = await test_all_apis(page)

        # Resumo
        print("\n" + "="*60)
        print("  RESUMO FINAL")
        print("="*60)

        pages_ok = sum(1 for p in all_results["pages"] if p.get("ok"))
        pages_404 = sum(1 for p in all_results["pages"] if p.get("status") == 404)
        pages_total = len(all_results["pages"])

        apis_ok = sum(1 for a in all_results["apis"] if a.get("status") == 200)
        apis_401 = sum(1 for a in all_results["apis"] if a.get("status") == 401)
        apis_404 = sum(1 for a in all_results["apis"] if a.get("status") == 404)
        apis_500 = sum(1 for a in all_results["apis"] if a.get("status") == 500)
        apis_total = len(all_results["apis"])

        print(f"\n  PAGINAS: {pages_ok}/{pages_total} OK ({pages_404} 404s)")
        print(f"  APIs: {apis_ok}/{apis_total} OK ({apis_401} 401s, {apis_404} 404s, {apis_500} 500s)")

        # Listar problemas
        print("\n  PAGINAS COM PROBLEMA:")
        for p in all_results["pages"]:
            if p.get("status") not in [200, 307]:
                print(f"    - {p['path']}: {p.get('status')}")
                all_results["issues"].append({"type": "page", **p})

        print("\n  APIs COM PROBLEMA (404/500):")
        for a in all_results["apis"]:
            if a.get("status") in [404, 500]:
                print(f"    - {a['name']}: {a.get('status')}")
                all_results["issues"].append({"type": "api", **a})

        print("\n  APIs QUE REQUEREM AUTH (401):")
        for a in all_results["apis"]:
            if a.get("status") == 401:
                print(f"    - {a['name']}")

        print(f"\n  Total de issues: {len(all_results['issues'])}")

        print("\n  Browser aberto por 10 segundos...")
        await asyncio.sleep(10)

        await browser.close()

    # Salvar resultados
    with open('test_complete_results.json', 'w') as f:
        json.dump(all_results, f, indent=2, default=str)

    print("\n  Resultados salvos em: test_complete_results.json")
    return all_results

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(main())
