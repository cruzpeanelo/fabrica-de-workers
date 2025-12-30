"""
Teste rapido do status atual do sistema
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_quick_status():
    """Teste rapido do status"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  STATUS RAPIDO DO SISTEMA")
        print("="*60)

        # 1. Home/Kanban
        print("\n[1] HOME/KANBAN...")
        await page.goto(BASE_URL)
        await page.wait_for_load_state('domcontentloaded', timeout=15000)
        await asyncio.sleep(2)

        # Fechar overlays via Escape
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.2)

        # Verificar elementos
        sidebar = await page.query_selector('.sidebar, aside, nav')
        header = await page.query_selector('header, .header')
        kanban = await page.query_selector('[class*="kanban"], .board')

        print(f"    Sidebar: {'OK' if sidebar else 'NAO'}")
        print(f"    Header: {'OK' if header else 'NAO'}")
        print(f"    Kanban: {'OK' if kanban else 'NAO'}")

        # 2. Testar navegacao
        print("\n[2] NAVEGACAO...")
        pages_status = []

        test_pages = [
            ("/executive", "Executive"),
            ("/analytics", "Analytics"),
            ("/billing", "Billing"),
            ("/admin/users", "Admin Users"),
            ("/login", "Login"),
            ("/settings", "Settings"),
        ]

        for path, name in test_pages:
            try:
                await page.goto(f"{BASE_URL}{path}", timeout=10000)
                await asyncio.sleep(0.5)
                body = await page.query_selector('body')
                text = await body.text_content() if body else ""
                has_content = len(text.strip()) > 100
                pages_status.append((name, "OK" if has_content else "VAZIO"))
                print(f"    {name}: {'OK' if has_content else 'VAZIO'}")
            except Exception as e:
                pages_status.append((name, "ERRO"))
                print(f"    {name}: ERRO - {str(e)[:30]}")

        # 3. Testar APIs criticas
        print("\n[3] APIs CRITICAS...")
        await page.goto(BASE_URL)
        await asyncio.sleep(1)

        apis = [
            ("/api/stories", "Stories"),
            ("/api/projects", "Projects"),
            ("/api/chat/history", "Chat"),
            ("/api/executive/kpis", "KPIs"),
            ("/api/search?q=test", "Search"),
        ]

        for path, name in apis:
            result = await page.evaluate(f'''async () => {{
                try {{
                    const res = await fetch('{path}');
                    return {{ status: res.status }};
                }} catch(e) {{
                    return {{ error: e.message }};
                }}
            }}''')
            status = result.get('status', 'ERR')
            icon = "[OK]" if status == 200 else f"[{status}]"
            print(f"    {icon} {name}")

        # 4. Testar interacao
        print("\n[4] INTERACAO...")
        await page.goto(BASE_URL)
        await asyncio.sleep(2)

        # Fechar overlays
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.2)

        # Tentar clicar via JS
        try:
            clicked = await page.evaluate('''() => {
                const btn = document.querySelector('button');
                if (btn) {
                    btn.click();
                    return true;
                }
                return false;
            }''')
            print(f"    Click JS: {'OK' if clicked else 'NAO ENCONTROU BOTAO'}")
        except:
            print("    Click JS: FALHOU")

        # Screenshot
        await page.screenshot(path='screenshots/status_atual.png')
        print("\n    Screenshot: screenshots/status_atual.png")

        print("\n" + "="*60)
        print("  RESUMO")
        print("="*60)
        ok_count = sum(1 for _, s in pages_status if s == "OK")
        print(f"  Paginas OK: {ok_count}/{len(pages_status)}")
        print("="*60)

        print("\nBrowser aberto por 8 segundos...")
        await asyncio.sleep(8)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_quick_status())
