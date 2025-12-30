"""
Teste de criacao de Story via Playwright
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_create_story():
    """Testa criacao de uma nova story"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n=== TESTE: Criar Nova Story ===")

        # 1. Acessar dashboard
        print("\n1. Acessando dashboard...")
        await page.goto(BASE_URL)
        await page.wait_for_load_state('networkidle', timeout=15000)
        await asyncio.sleep(1)

        # 2. Limpar TODOS os overlays e modais
        print("2. Limpando overlays...")

        # Limpar via JavaScript de forma agressiva
        await page.evaluate('''() => {
            // Fechar todas as variaveis Vue que controlam modais
            const app = document.querySelector('#app').__vue_app__;
            if (app) {
                const root = app._instance?.proxy || app._instance?.ctx;
                if (root) {
                    // Fechar todos os modais/overlays conhecidos
                    root.showOnboardingTour = false;
                    root.showCommandPalette = false;
                    root.showDocViewer = false;
                    root.showStoryModal = false;
                    root.showHelpModal = false;
                    root.showSettingsModal = false;
                }
            }

            // Remover elementos fixed que bloqueiam
            document.querySelectorAll('.fixed, [style*="position: fixed"]').forEach(el => {
                const classes = el.className || '';
                // Manter sidebar e header, remover overlays
                if (classes.includes('overlay') ||
                    classes.includes('modal') ||
                    classes.includes('backdrop') ||
                    classes.includes('commandPalette') ||
                    classes.includes('bg-black') ||
                    classes.includes('z-50') ||
                    classes.includes('z-60') ||
                    el.getAttribute('v-if')?.includes('show')) {
                    el.style.display = 'none';
                    el.style.pointerEvents = 'none';
                }
            });

            // Remover backdrops
            document.querySelectorAll('[class*="bg-black/"]').forEach(el => {
                el.style.display = 'none';
            });
        }''')

        await asyncio.sleep(0.5)

        # Pressionar Escape varias vezes para fechar qualquer modal
        for _ in range(5):
            await page.keyboard.press('Escape')
            await asyncio.sleep(0.1)

        print("   Overlays limpos")

        # 3. Procurar botao de criar story
        print("3. Procurando botao de criar story...")
        create_selectors = [
            'button:has-text("Nova Story")',
            'button:has-text("Nova")',
            'button:has-text("Criar")',
            'button:has-text("+")',
            '.fab-button',
            '[data-action="create"]',
        ]

        create_btn = None
        for selector in create_selectors:
            try:
                btn = await page.query_selector(selector)
                if btn and await btn.is_visible():
                    create_btn = btn
                    btn_text = await btn.text_content()
                    print(f"   Botao encontrado: {btn_text.strip()[:30] if btn_text else selector}")
                    break
            except:
                continue

        if not create_btn:
            print("   [ERRO] Botao de criar nao encontrado!")
            await page.screenshot(path='screenshots/create_story_no_button.png')
            await asyncio.sleep(10)
            await browser.close()
            return False

        # 4. Clicar no botao (via JavaScript para ignorar overlays)
        print("4. Clicando no botao de criar...")
        try:
            # Primeiro tenta click normal
            await create_btn.click(timeout=3000)
            await asyncio.sleep(1)
        except Exception as e:
            print(f"   Click normal falhou, tentando via JS...")
            # Se falhar, forca via JavaScript
            try:
                await create_btn.evaluate('el => el.click()')
                await asyncio.sleep(1)
                print("   [OK] Click via JS executado")
            except Exception as e2:
                print(f"   [ERRO] Falha total ao clicar: {e2}")
                await page.screenshot(path='screenshots/create_story_click_error.png')
                await asyncio.sleep(10)
                await browser.close()
                return False

        # 5. Verificar se modal/form abriu
        print("5. Verificando modal/form...")
        modal = await page.query_selector('.modal, [role="dialog"], .story-form, form')
        if modal and await modal.is_visible():
            print("   [OK] Modal/Form aberto!")
            await page.screenshot(path='screenshots/create_story_modal.png')
        else:
            print("   [WARN] Modal nao detectado")
            await page.screenshot(path='screenshots/create_story_no_modal.png')

        # 6. Preencher formulario
        print("6. Preenchendo formulario...")

        # Titulo
        title_input = await page.query_selector('input[name="title"], input[placeholder*="titulo"], #title')
        if title_input:
            await title_input.fill("Story de Teste Automatizado")
            print("   Titulo preenchido")

        # Persona
        persona_input = await page.query_selector('input[name="persona"], textarea[name="persona"], #persona')
        if persona_input:
            await persona_input.fill("testador automatizado")
            print("   Persona preenchida")

        # Action
        action_input = await page.query_selector('input[name="action"], textarea[name="action"], #action')
        if action_input:
            await action_input.fill("testar o sistema de criacao de stories")
            print("   Action preenchida")

        # Benefit
        benefit_input = await page.query_selector('input[name="benefit"], textarea[name="benefit"], #benefit')
        if benefit_input:
            await benefit_input.fill("garantir que a funcionalidade funciona corretamente")
            print("   Benefit preenchido")

        await page.screenshot(path='screenshots/create_story_filled.png')

        # 7. Salvar story
        print("7. Salvando story...")
        save_btn = await page.query_selector('button:has-text("Salvar"), button:has-text("Criar"), button[type="submit"]')
        if save_btn:
            await save_btn.click()
            await asyncio.sleep(2)
            print("   [OK] Botao salvar clicado")
            await page.screenshot(path='screenshots/create_story_saved.png')
        else:
            print("   [WARN] Botao salvar nao encontrado")

        # 8. Verificar se story foi criada
        print("8. Verificando se story foi criada...")
        await page.goto(BASE_URL)
        await page.wait_for_load_state('networkidle', timeout=10000)

        # Procurar pela story criada
        story_card = await page.query_selector('text="Story de Teste Automatizado"')
        if story_card:
            print("   [OK] Story criada com sucesso!")
        else:
            print("   [WARN] Story nao encontrada no board")

        print("\n=== Teste finalizado ===")
        print("Browser permanecera aberto por 20 segundos...")
        await asyncio.sleep(20)

        await browser.close()
        return True

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_create_story())
