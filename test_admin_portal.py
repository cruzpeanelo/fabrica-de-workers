"""
Teste do Admin Portal - Issue #296
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_admin_portal():
    """Testa Admin Portal"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  TESTE: ADMIN PORTAL (#296)")
        print("="*60)

        # 1. Acessar Admin Portal
        print("\n[1] Acessando /admin/portal...")
        await page.goto(f"{BASE_URL}/admin/portal")
        await page.wait_for_load_state('domcontentloaded', timeout=15000)
        await asyncio.sleep(2)

        # Fechar overlays
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.2)

        # 2. Verificar elementos
        print("\n[2] Verificando elementos...")

        # Titulo/Header
        header = await page.query_selector('h1, h2, .page-title, [class*="title"]')
        if header:
            text = await header.text_content()
            print(f"    Titulo: {text.strip()[:50] if text else 'N/A'}")
        else:
            print("    Titulo: NAO ENCONTRADO")

        # Cards/Secoes
        cards = await page.query_selector_all('.card, [class*="card"], .section, [class*="section"]')
        print(f"    Cards/Secoes: {len(cards)}")

        # Forms
        forms = await page.query_selector_all('form, [class*="form"]')
        print(f"    Formularios: {len(forms)}")

        # Inputs
        inputs = await page.query_selector_all('input, textarea, select')
        print(f"    Inputs: {len(inputs)}")

        # Buttons
        buttons = await page.query_selector_all('button')
        print(f"    Botoes: {len(buttons)}")

        # 3. Verificar conteudo visivel
        print("\n[3] Verificando conteudo visivel...")
        body = await page.query_selector('body')
        body_text = await body.text_content() if body else ""
        text_length = len(body_text.strip())
        print(f"    Texto total: {text_length} caracteres")

        # Palavras-chave esperadas
        keywords = ['configurar', 'config', 'settings', 'admin', 'portal', 'tenant', 'empresa']
        found_keywords = [kw for kw in keywords if kw.lower() in body_text.lower()]
        print(f"    Keywords encontradas: {found_keywords}")

        # 4. Screenshot
        await page.screenshot(path='screenshots/admin_portal.png')
        print("\n    Screenshot: screenshots/admin_portal.png")

        # 5. Testar Admin Users tambem
        print("\n[4] Testando /admin/users...")
        await page.goto(f"{BASE_URL}/admin/users")
        await page.wait_for_load_state('domcontentloaded', timeout=10000)
        await asyncio.sleep(1)

        # Tabela
        table = await page.query_selector('table')
        print(f"    Tabela: {'OK' if table else 'NAO'}")

        # Botao adicionar
        add_btn = await page.query_selector('button:has-text("Adicionar"), button:has-text("Novo"), button:has-text("+")')
        print(f"    Botao adicionar: {'OK' if add_btn else 'NAO'}")

        # Campo busca
        search = await page.query_selector('input[type="search"], input[placeholder*="busca"], input[placeholder*="search"]')
        print(f"    Campo busca: {'OK' if search else 'NAO'}")

        await page.screenshot(path='screenshots/admin_users.png')
        print("    Screenshot: screenshots/admin_users.png")

        # Resumo
        print("\n" + "="*60)
        print("  RESUMO")
        print("="*60)
        portal_ok = len(cards) > 0 or len(forms) > 0 or len(found_keywords) > 0
        users_ok = table is not None
        print(f"  Admin Portal: {'OK' if portal_ok else 'VAZIO'}")
        print(f"  Admin Users: {'OK' if users_ok else 'INCOMPLETO'}")
        print("="*60)

        print("\nBrowser aberto por 8 segundos...")
        await asyncio.sleep(8)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_admin_portal())
