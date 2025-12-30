"""
Debug do Modal de Criar Story - Issue #308
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_modal_debug():
    """Debug do modal"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*60)
        print("  DEBUG: MODAL CRIAR STORY")
        print("="*60)

        await page.goto(BASE_URL)
        await page.wait_for_load_state('networkidle', timeout=15000)
        await asyncio.sleep(2)

        # Fechar overlays
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.2)

        # 1. Verificar estado do Vue
        print("\n[1] Verificando estado Vue...")
        vue_state = await page.evaluate('''() => {
            try {
                const app = document.querySelector('#app');
                if (!app || !app.__vue_app__) return { error: "Vue app not found" };

                const instance = app.__vue_app__._instance;
                if (!instance) return { error: "Vue instance not found" };

                const proxy = instance.proxy || instance.ctx;
                if (!proxy) return { error: "Vue proxy not found" };

                // Listar propriedades relevantes
                const props = {};
                const keys = Object.keys(proxy).filter(k =>
                    k.includes('show') || k.includes('modal') || k.includes('Story')
                );
                keys.forEach(k => {
                    try { props[k] = proxy[k]; } catch {}
                });

                return { props, keys };
            } catch (e) {
                return { error: e.message };
            }
        }''')
        print(f"    Estado Vue: {vue_state}")

        # 2. Tentar abrir modal via Vue
        print("\n[2] Tentando abrir modal via Vue...")
        result = await page.evaluate('''() => {
            try {
                const app = document.querySelector('#app');
                const proxy = app.__vue_app__._instance?.proxy || app.__vue_app__._instance?.ctx;

                if (proxy) {
                    // Tentar setar showStoryModal
                    if ('showStoryModal' in proxy) {
                        proxy.showStoryModal = true;
                        return { success: true, method: "showStoryModal" };
                    }
                    if ('showCreateStoryModal' in proxy) {
                        proxy.showCreateStoryModal = true;
                        return { success: true, method: "showCreateStoryModal" };
                    }
                    if ('showNewStoryModal' in proxy) {
                        proxy.showNewStoryModal = true;
                        return { success: true, method: "showNewStoryModal" };
                    }
                    // Listar todas as props que contém modal
                    const modalProps = Object.keys(proxy).filter(k => k.toLowerCase().includes('modal'));
                    return { success: false, modalProps };
                }
                return { success: false, error: "proxy not found" };
            } catch (e) {
                return { error: e.message };
            }
        }''')
        print(f"    Resultado: {result}")

        await asyncio.sleep(1)

        # 3. Verificar se modal apareceu
        print("\n[3] Verificando modais no DOM...")
        modals = await page.evaluate('''() => {
            const allModals = [];
            document.querySelectorAll('[class*="modal"], [role="dialog"], .modal').forEach(el => {
                const style = window.getComputedStyle(el);
                allModals.push({
                    class: el.className.substring(0, 50),
                    display: style.display,
                    visibility: style.visibility,
                    zIndex: style.zIndex
                });
            });
            return allModals;
        }''')
        print(f"    Modais encontrados: {len(modals)}")
        for m in modals[:5]:
            print(f"       - {m}")

        # 4. Screenshot
        await page.screenshot(path='screenshots/modal_debug.png')
        print("\n    Screenshot: screenshots/modal_debug.png")

        print("\n" + "="*60)
        print("Browser aberto por 10 segundos...")
        await asyncio.sleep(10)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_modal_debug())
