"""
Script de teste interativo da Fabrica de Agentes
Usa Playwright com browser visivel para acompanhamento
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

# Fix encoding for Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def test_dashboard():
    """Testa a plataforma com browser visivel"""
    issues = []

    async with async_playwright() as p:
        # Browser visivel (headless=False)
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n=== TESTE 1: Pagina Inicial ===")
        try:
            response = await page.goto(BASE_URL)
            await page.wait_for_load_state('networkidle')
            title = await page.title()
            print(f"[OK] Titulo: {title}")
            print(f"[OK] Status: {response.status}")

            # Verificar elementos principais
            header = await page.query_selector('header, .header, nav, .navbar')
            sidebar = await page.query_selector('.sidebar, aside, nav.sidebar')
            print(f"[OK] Header encontrado: {header is not None}")
            print(f"[OK] Sidebar encontrada: {sidebar is not None}")

            # Screenshot
            await page.screenshot(path='screenshots/01_home.png')
            print("[OK] Screenshot salvo: screenshots/01_home.png")

            # Verificar erros no console
            console_errors = []
            page.on('console', lambda msg: console_errors.append(msg.text) if msg.type == 'error' else None)

        except Exception as e:
            print(f"[ERRO] Pagina inicial: {e}")
            issues.append({"test": "home", "error": str(e)})

        print("\n=== TESTE 1.5: Fechar Onboarding Tour ===")
        try:
            # Tentar fechar o onboarding overlay se existir
            onboarding = await page.query_selector('.onboarding-overlay')
            if onboarding:
                print("  Onboarding detectado, fechando...")
                await onboarding.click()
                await asyncio.sleep(0.5)
                print("[OK] Onboarding fechado")
            else:
                print("  Nenhum onboarding ativo")
        except Exception as e:
            print(f"[WARN] Erro ao fechar onboarding: {e}")

        print("\n=== TESTE 2: Navegacao e Links ===")
        try:
            # Procurar links de navegacao
            nav_links = await page.query_selector_all('a[href], button, .nav-link, .sidebar-link')
            print(f"  Encontrados {len(nav_links)} elementos de navegacao")

            visible_links = []
            for link in nav_links:
                try:
                    if await link.is_visible():
                        text = await link.text_content()
                        if text and text.strip():
                            visible_links.append(text.strip()[:50])
                except:
                    pass

            print(f"  Links visiveis: {len(visible_links)}")
            for link in visible_links[:15]:
                print(f"    - {link}")

        except Exception as e:
            print(f"[ERRO] Navegacao: {e}")
            issues.append({"test": "navigation", "error": str(e)})

        print("\n=== TESTE 3: Kanban Board ===")
        try:
            # Procurar colunas do Kanban
            columns = await page.query_selector_all('.kanban-column, .column, [class*="column"], [data-status]')
            print(f"  Colunas encontradas: {len(columns)}")

            # Procurar cards
            cards = await page.query_selector_all('.story-card, .card, [draggable="true"], .kanban-card')
            print(f"  Cards encontrados: {len(cards)}")

            if len(cards) > 0:
                # Testar interacao com card
                first_card = cards[0]
                await first_card.hover()
                await asyncio.sleep(0.5)
                print("[OK] Hover no card funcionou")

            await page.screenshot(path='screenshots/02_kanban.png')

        except Exception as e:
            print(f"[ERRO] Kanban: {e}")
            issues.append({"test": "kanban", "error": str(e)})

        print("\n=== TESTE 4: Criar Story ===")
        try:
            # Procurar botao de criar story
            create_selectors = [
                'button:has-text("Nova Story")',
                'button:has-text("Criar")',
                'button:has-text("Add Story")',
                'button:has-text("+")',
                '.btn-create',
                '[data-action="create"]',
                'button.primary'
            ]

            create_btn = None
            for selector in create_selectors:
                try:
                    btn = await page.query_selector(selector)
                    if btn and await btn.is_visible():
                        create_btn = btn
                        print(f"[OK] Botao encontrado: {selector}")
                        break
                except:
                    continue

            if create_btn:
                await create_btn.click()
                await asyncio.sleep(1)

                # Verificar se modal/form abriu
                modal = await page.query_selector('.modal, [role="dialog"], .form-container, form')
                if modal:
                    print("[OK] Modal/Form de criacao abriu")
                    await page.screenshot(path='screenshots/03_create_form.png')
                else:
                    print("[WARN] Modal nao detectado apos clique")
                    issues.append({"test": "create_story", "error": "Modal nao abriu apos clique no botao"})
            else:
                print("[WARN] Botao de criacao nao encontrado")
                issues.append({"test": "create_story", "error": "Botao de criar story nao encontrado"})

        except Exception as e:
            print(f"[ERRO] Criar story: {e}")
            issues.append({"test": "create_story", "error": str(e)})

        print("\n=== TESTE 5: Responsividade ===")
        try:
            # Testar diferentes tamanhos
            viewports = [
                {'width': 1920, 'height': 1080, 'name': 'Desktop'},
                {'width': 1024, 'height': 768, 'name': 'Tablet'},
                {'width': 375, 'height': 667, 'name': 'Mobile'}
            ]

            for vp in viewports:
                await page.set_viewport_size({'width': vp['width'], 'height': vp['height']})
                await asyncio.sleep(0.5)
                await page.screenshot(path=f'screenshots/responsive_{vp["name"].lower()}.png')
                print(f"[OK] {vp['name']}: {vp['width']}x{vp['height']}")

            # Voltar para desktop
            await page.set_viewport_size({'width': 1920, 'height': 1080})

        except Exception as e:
            print(f"[ERRO] Responsividade: {e}")
            issues.append({"test": "responsive", "error": str(e)})

        print("\n=== TESTE 6: Command Palette (Ctrl+K) ===")
        try:
            await page.keyboard.press('Control+k')
            await asyncio.sleep(0.5)

            palette = await page.query_selector('.command-palette, [role="dialog"], .modal')
            if palette and await palette.is_visible():
                print("[OK] Command Palette abriu com Ctrl+K")
                await page.screenshot(path='screenshots/05_command_palette.png')
                # Fechar palette
                await page.keyboard.press('Escape')
            else:
                print("[WARN] Command Palette nao abriu")
                issues.append({"test": "command_palette", "error": "Command Palette nao abriu com Ctrl+K"})

        except Exception as e:
            print(f"[ERRO] Command Palette: {e}")
            issues.append({"test": "command_palette", "error": str(e)})

        print("\n=== TESTE 7: Dark Mode Toggle ===")
        try:
            # Procurar toggle de dark mode
            dark_toggle = await page.query_selector('[title*="modo"], [title*="theme"], button:has-text("Dark"), .dark-toggle')
            if dark_toggle:
                await dark_toggle.click()
                await asyncio.sleep(0.5)
                await page.screenshot(path='screenshots/06_dark_mode.png')
                print("[OK] Dark mode toggle encontrado e clicado")
            else:
                # Tentar pelo icone
                moon_icon = await page.query_selector('button:has-text("lua"), button:has-text("moon")')
                if moon_icon:
                    await moon_icon.click()
                    await asyncio.sleep(0.5)
                    print("[OK] Toggle de tema encontrado")
                else:
                    print("[WARN] Toggle de dark mode nao encontrado")

        except Exception as e:
            print(f"[ERRO] Dark Mode: {e}")

        print("\n=== TESTE 8: Breadcrumb Navigation ===")
        try:
            breadcrumb = await page.query_selector('.breadcrumb, nav[aria-label="breadcrumb"], .breadcrumbs')
            if breadcrumb:
                print("[OK] Breadcrumb encontrado")
                crumbs = await breadcrumb.query_selector_all('a, span, li')
                print(f"  {len(crumbs)} itens no breadcrumb")
            else:
                print("[WARN] Breadcrumb nao encontrado")
                issues.append({"test": "breadcrumb", "error": "Navegacao breadcrumb nao encontrada"})

        except Exception as e:
            print(f"[ERRO] Breadcrumb: {e}")

        print("\n=== TESTE 9: API Health Check ===")
        try:
            # Testar endpoint de health
            api_response = await page.evaluate('''async () => {
                try {
                    const res = await fetch('/api/stories');
                    return { status: res.status, ok: res.ok };
                } catch(e) {
                    return { error: e.message };
                }
            }''')
            print(f"  API /api/stories: {api_response}")

        except Exception as e:
            print(f"[ERRO] API: {e}")

        print("\n=== TESTE 10: Console Errors ===")
        try:
            errors = await page.evaluate('''() => {
                return window.__consoleErrors || [];
            }''')
            if errors:
                print(f"[WARN] {len(errors)} erros no console")
                for err in errors[:5]:
                    print(f"  - {err}")
            else:
                print("[OK] Nenhum erro critico no console detectado")

        except Exception as e:
            print(f"  Nao foi possivel verificar console: {e}")

        print("\n=== RESUMO ===")
        print(f"Issues encontradas: {len(issues)}")
        for issue in issues:
            print(f"  - {issue['test']}: {issue['error'][:80]}")

        print("\nBrowser permanecera aberto por 30 segundos para inspecao...")
        await asyncio.sleep(30)

        await browser.close()

    return issues

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    result = asyncio.run(test_dashboard())
    print(f"\n\nTotal de issues: {len(result)}")
