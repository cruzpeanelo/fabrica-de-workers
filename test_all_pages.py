"""
Teste de todas as paginas do Dashboard Fabrica de Agentes
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

# Lista de todas as paginas/rotas para testar
PAGES = [
    {"path": "/", "name": "Home/Dashboard"},
    {"path": "/executive", "name": "Executive Dashboard"},
    {"path": "/analytics", "name": "Analytics Page"},
    {"path": "/billing", "name": "Billing Page"},
    {"path": "/admin", "name": "Admin Panel"},
    {"path": "/admin/users", "name": "Admin Users"},
    {"path": "/admin/portal", "name": "Admin Portal"},
    {"path": "/security", "name": "Security Settings"},
    {"path": "/integrations", "name": "Integrations Panel"},
    {"path": "/workers", "name": "Workers Monitor"},
    {"path": "/projects", "name": "Projects List"},
    {"path": "/docs", "name": "Documentation"},
]

# Endpoints de API para testar
API_ENDPOINTS = [
    {"path": "/api/stories", "name": "Stories API"},
    {"path": "/api/projects", "name": "Projects API"},
    {"path": "/api/jobs", "name": "Jobs API"},
    {"path": "/api/executive/kpis", "name": "Executive KPIs API"},
    {"path": "/api/executive/metrics", "name": "Executive Metrics API"},
    {"path": "/api/analytics/productivity", "name": "Analytics Productivity API"},
    {"path": "/api/analytics/insights", "name": "Analytics Insights API"},
    {"path": "/api/workers/status", "name": "Workers Status API"},
    {"path": "/api/integrations", "name": "Integrations API"},
    {"path": "/health", "name": "Health Check"},
]

async def test_all_pages():
    """Testa todas as paginas e APIs"""
    results = {"pages": [], "apis": [], "issues": []}

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        # Fechar onboarding se existir
        print("\n=== Preparando ambiente ===")
        try:
            await page.goto(BASE_URL)
            await page.wait_for_load_state('domcontentloaded', timeout=10000)
            await asyncio.sleep(1)

            onboarding = await page.query_selector('.onboarding-overlay')
            if onboarding:
                await onboarding.click()
                await asyncio.sleep(0.5)
                print("[OK] Onboarding fechado")
        except Exception as e:
            print(f"[WARN] Preparacao: {e}")

        # ========== TESTE DE PAGINAS ==========
        print("\n" + "="*60)
        print("  TESTE DE PAGINAS")
        print("="*60)

        for pg in PAGES:
            try:
                print(f"\n>> Testando: {pg['name']} ({pg['path']})")

                response = await page.goto(f"{BASE_URL}{pg['path']}", timeout=15000)
                await page.wait_for_load_state('domcontentloaded', timeout=10000)

                status = response.status if response else "N/A"
                title = await page.title()

                # Verificar conteudo basico
                body = await page.query_selector('body')
                body_text = await body.text_content() if body else ""
                has_content = len(body_text.strip()) > 100

                # Verificar erros visiveis
                error_elements = await page.query_selector_all('.error, .alert-danger, [class*="error"]')
                has_visible_error = False
                for err in error_elements:
                    if await err.is_visible():
                        has_visible_error = True
                        break

                # Screenshot
                screenshot_name = pg['path'].replace('/', '_').strip('_') or 'home'
                await page.screenshot(path=f'screenshots/page_{screenshot_name}.png')

                result = {
                    "path": pg['path'],
                    "name": pg['name'],
                    "status": status,
                    "has_content": has_content,
                    "has_error": has_visible_error,
                    "title": title[:50] if title else "N/A"
                }
                results["pages"].append(result)

                if status == 200 and has_content:
                    print(f"   [OK] Status: {status} | Conteudo: OK")
                elif status == 200:
                    print(f"   [WARN] Status: {status} | Conteudo: Pouco/Vazio")
                elif status == 404:
                    print(f"   [WARN] Status: 404 - Pagina nao encontrada")
                    results["issues"].append({"type": "page", "path": pg['path'], "error": "404 Not Found"})
                else:
                    print(f"   [ERRO] Status: {status}")
                    results["issues"].append({"type": "page", "path": pg['path'], "error": f"Status {status}"})

            except Exception as e:
                error_msg = str(e)[:80]
                print(f"   [ERRO] {error_msg}")
                results["pages"].append({
                    "path": pg['path'],
                    "name": pg['name'],
                    "status": "ERROR",
                    "error": error_msg
                })
                results["issues"].append({"type": "page", "path": pg['path'], "error": error_msg})

        # ========== TESTE DE APIs ==========
        print("\n" + "="*60)
        print("  TESTE DE APIs")
        print("="*60)

        for api in API_ENDPOINTS:
            try:
                print(f"\n>> Testando: {api['name']} ({api['path']})")

                api_result = await page.evaluate(f'''async () => {{
                    try {{
                        const res = await fetch('{api["path"]}');
                        const text = await res.text();
                        let json = null;
                        try {{ json = JSON.parse(text); }} catch {{}}
                        return {{
                            status: res.status,
                            ok: res.ok,
                            hasData: text.length > 2,
                            isJson: json !== null,
                            preview: text.substring(0, 100)
                        }};
                    }} catch(e) {{
                        return {{ error: e.message }};
                    }}
                }}''')

                results["apis"].append({
                    "path": api['path'],
                    "name": api['name'],
                    **api_result
                })

                if api_result.get('error'):
                    print(f"   [ERRO] {api_result['error']}")
                    results["issues"].append({"type": "api", "path": api['path'], "error": api_result['error']})
                elif api_result.get('status') == 200:
                    print(f"   [OK] Status: 200 | JSON: {api_result.get('isJson')} | Data: {api_result.get('hasData')}")
                elif api_result.get('status') == 500:
                    print(f"   [ERRO] Status: 500 - Internal Server Error")
                    results["issues"].append({"type": "api", "path": api['path'], "error": "500 Internal Server Error"})
                elif api_result.get('status') == 404:
                    print(f"   [WARN] Status: 404 - Endpoint nao encontrado")
                else:
                    print(f"   [INFO] Status: {api_result.get('status')}")

            except Exception as e:
                error_msg = str(e)[:80]
                print(f"   [ERRO] {error_msg}")
                results["issues"].append({"type": "api", "path": api['path'], "error": error_msg})

        # ========== TESTE DE ELEMENTOS UI ==========
        print("\n" + "="*60)
        print("  TESTE DE ELEMENTOS UI")
        print("="*60)

        await page.goto(BASE_URL)
        await page.wait_for_load_state('domcontentloaded', timeout=10000)
        await asyncio.sleep(1)

        ui_tests = [
            {"selector": ".sidebar, aside, nav.sidebar", "name": "Sidebar"},
            {"selector": "header, .header, .navbar", "name": "Header"},
            {"selector": ".kanban-column, .column, [class*='column']", "name": "Kanban Columns"},
            {"selector": "button, .btn", "name": "Buttons"},
            {"selector": "input, textarea, select", "name": "Form Elements"},
            {"selector": ".modal, [role='dialog']", "name": "Modals"},
            {"selector": ".notification, .toast, .alert", "name": "Notifications"},
            {"selector": ".breadcrumb, nav[aria-label='breadcrumb']", "name": "Breadcrumb"},
            {"selector": ".search, [type='search'], input[placeholder*='search']", "name": "Search Box"},
            {"selector": ".fab, .floating-button, [class*='floating']", "name": "FAB Button"},
        ]

        print()
        for ui in ui_tests:
            try:
                elements = await page.query_selector_all(ui['selector'])
                visible_count = 0
                for el in elements:
                    try:
                        if await el.is_visible():
                            visible_count += 1
                    except:
                        pass

                if visible_count > 0:
                    print(f"   [OK] {ui['name']}: {visible_count} elemento(s) visivel(is)")
                else:
                    print(f"   [--] {ui['name']}: Nao encontrado/visivel")

            except Exception as e:
                print(f"   [ERRO] {ui['name']}: {str(e)[:50]}")

        # ========== TESTE DE ATALHOS ==========
        print("\n" + "="*60)
        print("  TESTE DE ATALHOS DE TECLADO")
        print("="*60)

        shortcuts = [
            {"keys": "Control+k", "name": "Command Palette", "expect": ".command-palette, [role='dialog']"},
            {"keys": "Escape", "name": "Fechar Modal", "expect": None},
            {"keys": "?", "name": "Help/Shortcuts", "expect": ".shortcuts-modal, .help-modal"},
        ]

        for shortcut in shortcuts:
            try:
                await page.keyboard.press(shortcut['keys'])
                await asyncio.sleep(0.5)

                if shortcut['expect']:
                    element = await page.query_selector(shortcut['expect'])
                    if element and await element.is_visible():
                        print(f"   [OK] {shortcut['keys']}: {shortcut['name']} funcionou")
                        await page.keyboard.press('Escape')
                    else:
                        print(f"   [--] {shortcut['keys']}: {shortcut['name']} - elemento nao apareceu")
                else:
                    print(f"   [OK] {shortcut['keys']}: {shortcut['name']} executado")

            except Exception as e:
                print(f"   [ERRO] {shortcut['keys']}: {str(e)[:50]}")

        # ========== RESUMO ==========
        print("\n" + "="*60)
        print("  RESUMO FINAL")
        print("="*60)

        pages_ok = sum(1 for p in results["pages"] if p.get("status") == 200)
        pages_total = len(results["pages"])
        apis_ok = sum(1 for a in results["apis"] if a.get("status") == 200)
        apis_total = len(results["apis"])

        print(f"\n   Paginas: {pages_ok}/{pages_total} OK")
        print(f"   APIs: {apis_ok}/{apis_total} OK")
        print(f"   Issues encontradas: {len(results['issues'])}")

        if results["issues"]:
            print("\n   ISSUES:")
            for issue in results["issues"]:
                print(f"   - [{issue['type'].upper()}] {issue['path']}: {issue['error'][:60]}")

        print("\n   Screenshots salvos em: screenshots/")
        print("\n   Browser permanecera aberto por 20 segundos...")
        await asyncio.sleep(20)

        await browser.close()

    return results

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    results = asyncio.run(test_all_pages())

    # Salvar resultados em JSON
    import json
    with open('test_results.json', 'w') as f:
        json.dump(results, f, indent=2, default=str)
    print("\nResultados salvos em: test_results.json")
