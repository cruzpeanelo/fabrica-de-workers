"""
Teste dos fixes recentes - #291 (overlays) e #294 (story cards)
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_recent_fixes():
    """Testa os fixes recentes"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  TESTE DOS FIXES RECENTES")
        print("="*60)

        # Teste 1: Verificar overlays (#291)
        print("\n[1] TESTANDO FIX #291 - Overlays bloqueando interacao...")
        await page.goto(BASE_URL)
        await page.wait_for_load_state('domcontentloaded', timeout=15000)
        await asyncio.sleep(2)

        # Verificar se tem overlays visíveis bloqueando
        overlays = await page.evaluate('''() => {
            const fixed = document.querySelectorAll('.fixed');
            const blocking = [];
            fixed.forEach(el => {
                const cls = el.className || '';
                const style = window.getComputedStyle(el);
                if (style.display !== 'none' && style.visibility !== 'hidden') {
                    if (cls.includes('overlay') || cls.includes('modal') ||
                        cls.includes('bg-black') || cls.includes('z-50')) {
                        blocking.push({
                            class: cls.substring(0, 100),
                            display: style.display,
                            zIndex: style.zIndex
                        });
                    }
                }
            });
            return blocking;
        }''')

        if len(overlays) == 0:
            print("    [OK] Nenhum overlay bloqueante detectado!")
        else:
            print(f"    [WARN] {len(overlays)} overlays encontrados:")
            for o in overlays[:3]:
                print(f"       - {o.get('class', '')[:50]}...")

        # Teste 2: Verificar story cards (#294)
        print("\n[2] TESTANDO FIX #294 - Story cards no Kanban...")

        # Primeiro verificar API de stories
        api_result = await page.evaluate('''async () => {
            const res = await fetch('/api/stories');
            const data = await res.json();
            return { status: res.status, count: Array.isArray(data) ? data.length : 0 };
        }''')
        print(f"    API /api/stories: {api_result.get('status')} ({api_result.get('count')} stories)")

        # Verificar se cards aparecem no DOM
        await asyncio.sleep(1)
        cards = await page.query_selector_all('.story-card, [draggable="true"], .kanban-card, [class*="card"]')
        print(f"    Cards no DOM: {len(cards)}")

        # Verificar colunas do Kanban
        columns = await page.query_selector_all('.kanban-column, [data-status], [class*="column"]')
        print(f"    Colunas Kanban: {len(columns)}")

        # Verificar conteúdo dentro das colunas
        column_content = await page.evaluate('''() => {
            const cols = document.querySelectorAll('.kanban-column, [data-status]');
            const result = [];
            cols.forEach(col => {
                const cards = col.querySelectorAll('.story-card, [draggable="true"]');
                result.push({
                    status: col.getAttribute('data-status') || col.className.substring(0, 30),
                    cardCount: cards.length
                });
            });
            return result;
        }''')

        if column_content:
            print("    Conteudo das colunas:")
            for col in column_content:
                print(f"       - {col.get('status')}: {col.get('cardCount')} cards")

        # Teste 3: Verificar se consegue clicar em botoes
        print("\n[3] TESTANDO INTERACAO COM BOTOES...")

        try:
            # Tentar encontrar botao de criar story
            create_btn = await page.query_selector('button:has-text("Nova Story"), button:has-text("Nova"), button:has-text("+")')
            if create_btn:
                is_visible = await create_btn.is_visible()
                is_enabled = await create_btn.is_enabled()
                print(f"    Botao 'Nova Story': visivel={is_visible}, habilitado={is_enabled}")

                if is_visible and is_enabled:
                    # Tentar clicar
                    await create_btn.click(timeout=3000)
                    print("    [OK] Click no botao funcionou!")
                    await asyncio.sleep(1)

                    # Verificar se modal abriu
                    modal = await page.query_selector('.modal, [role="dialog"], .story-modal')
                    if modal and await modal.is_visible():
                        print("    [OK] Modal abriu corretamente!")
                    else:
                        print("    [WARN] Modal nao detectado apos click")

                    # Fechar modal
                    await page.keyboard.press('Escape')
            else:
                print("    [WARN] Botao de criar story nao encontrado")
        except Exception as e:
            print(f"    [ERRO] Falha na interacao: {str(e)[:50]}")

        # Screenshot final
        await page.screenshot(path='screenshots/test_recent_fixes.png')
        print("\n    Screenshot salvo: screenshots/test_recent_fixes.png")

        # Resumo
        print("\n" + "="*60)
        print("  RESUMO DOS FIXES")
        print("="*60)
        print(f"  #291 (Overlays): {'[OK]' if len(overlays) == 0 else '[PENDENTE]'}")
        print(f"  #294 (Story Cards): {'[OK]' if api_result.get('count', 0) > 0 and len(cards) > 0 else '[VERIFICAR]'}")
        print("="*60)

        print("\nBrowser aberto por 10 segundos...")
        await asyncio.sleep(10)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_recent_fixes())
