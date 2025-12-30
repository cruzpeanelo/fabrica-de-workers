"""
Teste de paginas adicionais - Security, Workers, Projects
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_page(page, path, name):
    """Testa uma pagina especifica"""
    print(f"\n[{name}] Testando {path}...")
    try:
        await page.goto(f"{BASE_URL}{path}", timeout=15000)
        await page.wait_for_load_state('domcontentloaded', timeout=10000)
        await asyncio.sleep(1)

        # Fechar overlays
        for _ in range(3):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.1)

        # Verificar elementos
        body = await page.query_selector('body')
        text = await body.text_content() if body else ""
        text_len = len(text.strip())

        cards = await page.query_selector_all('[class*="card"], .card')
        tables = await page.query_selector_all('table')
        buttons = await page.query_selector_all('button')

        print(f"    Texto: {text_len} chars")
        print(f"    Cards: {len(cards)}")
        print(f"    Tabelas: {len(tables)}")
        print(f"    Botoes: {len(buttons)}")

        # Screenshot
        await page.screenshot(path=f'screenshots/{name.lower().replace(" ", "_")}.png')

        return {
            "page": path,
            "name": name,
            "ok": text_len > 100,
            "text_len": text_len,
            "cards": len(cards),
            "tables": len(tables)
        }
    except Exception as e:
        print(f"    ERRO: {str(e)[:50]}")
        return {"page": path, "name": name, "ok": False, "error": str(e)[:50]}

async def main():
    """Testa varias paginas"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  TESTE DE PAGINAS ADICIONAIS")
        print("="*60)

        results = []

        # Testar paginas
        pages_to_test = [
            ("/security", "Security"),
            ("/workers", "Workers"),
            ("/projects", "Projects"),
            ("/integrations", "Integrations"),
            ("/notifications", "Notifications"),
            ("/help", "Help"),
        ]

        for path, name in pages_to_test:
            result = await test_page(page, path, name)
            results.append(result)

        # Resumo
        print("\n" + "="*60)
        print("  RESUMO")
        print("="*60)
        for r in results:
            status = "OK" if r.get("ok") else "PROBLEMA"
            print(f"  {r['name']}: {status} ({r.get('text_len', 0)} chars, {r.get('cards', 0)} cards)")
        print("="*60)

        print("\nBrowser aberto por 5 segundos...")
        await asyncio.sleep(5)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(main())
