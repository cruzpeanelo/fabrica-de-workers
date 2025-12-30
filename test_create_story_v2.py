"""
Teste de criacao de Story via UI - versao 2
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_create_story():
    """Testa criacao de story"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  TESTE: CRIAR NOVA STORY")
        print("="*60)

        # 1. Acessar dashboard
        print("\n[1] Acessando dashboard...")
        await page.goto(BASE_URL)
        await page.wait_for_load_state('networkidle', timeout=15000)
        await asyncio.sleep(2)

        # 2. Fechar overlays
        print("[2] Fechando overlays...")
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.2)

        # Forcar fechamento via JS
        await page.evaluate('''() => {
            document.querySelectorAll('.fixed').forEach(el => {
                const cls = el.className || '';
                if (cls.includes('bg-black') || cls.includes('z-50') || cls.includes('modal')) {
                    el.style.display = 'none';
                }
            });
        }''')
        await asyncio.sleep(0.5)
        print("    Overlays fechados")

        # 3. Procurar botao de criar
        print("[3] Procurando botao de criar story...")
        create_btn = None

        selectors = [
            'button:has-text("Nova Story")',
            'button:has-text("Nova")',
            'button:has-text("Criar")',
            '[data-action="create"]',
            '.create-btn',
            '.fab-button',
        ]

        for sel in selectors:
            try:
                btn = await page.query_selector(sel)
                if btn:
                    visible = await btn.is_visible()
                    if visible:
                        create_btn = btn
                        text = await btn.text_content()
                        print(f"    Encontrado: {text.strip()[:30] if text else sel}")
                        break
            except:
                continue

        if not create_btn:
            # Tentar encontrar qualquer botao com icone de +
            all_buttons = await page.query_selector_all('button')
            for btn in all_buttons:
                try:
                    text = await btn.text_content()
                    if text and ('+' in text or 'add' in text.lower() or 'nova' in text.lower()):
                        if await btn.is_visible():
                            create_btn = btn
                            print(f"    Encontrado via busca: {text.strip()[:30]}")
                            break
                except:
                    continue

        if not create_btn:
            print("    [ERRO] Botao de criar nao encontrado!")
            await page.screenshot(path='screenshots/create_story_no_btn.png')
            await asyncio.sleep(10)
            await browser.close()
            return

        # 4. Clicar no botao
        print("[4] Clicando no botao...")
        try:
            # Click via JS para evitar interceptacao
            await create_btn.evaluate('el => el.click()')
            await asyncio.sleep(1)
            print("    Click executado")
        except Exception as e:
            print(f"    [ERRO] Click falhou: {str(e)[:40]}")
            await page.screenshot(path='screenshots/create_story_click_fail.png')
            await asyncio.sleep(10)
            await browser.close()
            return

        # 5. Verificar modal/form
        print("[5] Verificando modal/form...")
        await asyncio.sleep(1)

        modal = await page.query_selector('.modal, [role="dialog"], .story-form, form')
        if modal:
            visible = await modal.is_visible()
            print(f"    Modal encontrado: visivel={visible}")
            await page.screenshot(path='screenshots/create_story_modal.png')
        else:
            print("    [WARN] Modal nao detectado")
            await page.screenshot(path='screenshots/create_story_no_modal.png')

        # 6. Procurar campos do formulario
        print("[6] Procurando campos do formulario...")

        # Titulo
        title_input = await page.query_selector('input[name="title"], input[placeholder*="titulo"], input[placeholder*="itle"], #title, [name="title"]')
        if title_input:
            await title_input.fill("Story de Teste Automatizado v2")
            print("    [OK] Titulo preenchido")

        # Descricao/Action
        desc_input = await page.query_selector('textarea, input[name="description"], [name="action"]')
        if desc_input:
            await desc_input.fill("Como testador, quero criar stories automaticamente")
            print("    [OK] Descricao preenchida")

        await page.screenshot(path='screenshots/create_story_form.png')

        # 7. Salvar
        print("[7] Procurando botao salvar...")
        save_selectors = [
            'button:has-text("Salvar")',
            'button:has-text("Criar")',
            'button:has-text("Save")',
            'button[type="submit"]',
            '.save-btn',
        ]

        save_btn = None
        for sel in save_selectors:
            try:
                btn = await page.query_selector(sel)
                if btn and await btn.is_visible():
                    save_btn = btn
                    text = await btn.text_content()
                    print(f"    Encontrado: {text.strip()[:20] if text else sel}")
                    break
            except:
                continue

        if save_btn:
            print("[8] Salvando story...")
            await save_btn.evaluate('el => el.click()')
            await asyncio.sleep(2)
            print("    [OK] Botao salvar clicado")
            await page.screenshot(path='screenshots/create_story_saved.png')
        else:
            print("    [WARN] Botao salvar nao encontrado")

        # 9. Verificar resultado
        print("[9] Verificando resultado...")
        await page.goto(BASE_URL)
        await asyncio.sleep(2)

        # Buscar a story criada via API
        result = await page.evaluate('''async () => {
            const res = await fetch('/api/stories');
            const data = await res.json();
            return {
                total: data.length,
                latest: data.length > 0 ? data[0].title : null
            };
        }''')
        print(f"    Total de stories: {result.get('total')}")
        print(f"    Ultima story: {result.get('latest', 'N/A')[:40] if result.get('latest') else 'N/A'}")

        print("\n" + "="*60)
        print("  TESTE FINALIZADO")
        print("="*60)

        print("\nBrowser aberto por 10 segundos...")
        await asyncio.sleep(10)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_create_story())
