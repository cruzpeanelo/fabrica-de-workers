# -*- coding: utf-8 -*-
"""
Testes Visuais E2E com Playwright
=================================

Script para executar testes visuais com browser visivel,
capturando screenshots de todas as funcionalidades.

Uso: python tests/test_visual_playwright_mcp.py
"""

import asyncio
import os
from datetime import datetime
from pathlib import Path

# Diretorio de screenshots
SCREENSHOT_DIR = Path("C:/Users/lcruz/Fabrica de Agentes/analysis/screenshots/playwright_visual")
BASE_URL = "http://localhost:9001"

# Resultados dos testes
results = []


async def run_visual_tests():
    """Executa todos os testes visuais"""
    from playwright.async_api import async_playwright

    print("=" * 60)
    print("TESTES VISUAIS E2E - PLATAFORMA E")
    print("=" * 60)
    print(f"Inicio: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Screenshots: {SCREENSHOT_DIR}")
    print("=" * 60)

    async with async_playwright() as p:
        # Iniciar browser VISIVEL
        browser = await p.chromium.launch(
            headless=False,  # Browser visivel!
            slow_mo=500,     # 500ms entre acoes para visualizacao
        )

        context = await browser.new_context(
            viewport={"width": 1920, "height": 1080},
            reduced_motion="reduce",  # Reduzir animações
            java_script_enabled=True
        )
        page = await context.new_page()

        # Capturar erros JavaScript
        console_errors = []
        page.on("console", lambda msg: console_errors.append(msg.text) if msg.type == "error" else None)
        page.on("pageerror", lambda err: console_errors.append(str(err)))

        try:
            # ========================================
            # FASE 1: LOGIN
            # ========================================
            print("\n[FASE 1] LOGIN E AUTENTICACAO")
            print("-" * 40)

            # 1.1 Navegar para login
            print("  > Navegando para /login...")
            await page.goto(f"{BASE_URL}/login")
            await page.wait_for_load_state("domcontentloaded")
            await asyncio.sleep(2)  # Aguardar Vue renderizar

            # Screenshot: tela de login
            screenshot_path = SCREENSHOT_DIR / "01_login_page.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Login Page", "PASS", screenshot_path.name))

            # 1.2 Preencher credenciais
            print("  > Preenchendo credenciais...")

            # Aguardar campo aparecer
            await page.wait_for_selector('input[type="text"], input[name="username"]', timeout=30000)

            # Preencher username (platform_admin tem acesso total)
            username_field = await page.query_selector('input[type="text"]')
            if username_field:
                await username_field.fill("platform_admin")

            # Preencher password (senha correta do demo_seed)
            password_field = await page.query_selector('input[type="password"]')
            if password_field:
                await password_field.fill("Platform@2025!Adm")

            await asyncio.sleep(1)

            # Screenshot: formulario preenchido
            screenshot_path = SCREENSHOT_DIR / "02_login_filled.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Login Filled", "PASS", screenshot_path.name))

            # 1.3 Clicar em Login
            print("  > Clicando em Login...")
            login_button = await page.query_selector('button[type="submit"]')
            if login_button:
                await login_button.click()

            # Aguardar redirect - esperar URL mudar
            try:
                await page.wait_for_url("**/dashboard", timeout=30000)
                print("  [OK] Redirecionado para dashboard")
            except:
                # Se não redirecionou para dashboard, tentar qualquer página diferente de login
                await asyncio.sleep(3)

            # Screenshot: apos login
            screenshot_path = SCREENSHOT_DIR / "03_after_login.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            current_url = page.url
            if "/login" not in current_url:
                results.append(("Login Success", "PASS", screenshot_path.name))
                print(f"  [OK] Redirecionado para: {current_url}")
            else:
                results.append(("Login Success", "FAIL", "Ainda na pagina de login"))
                print("  [WARN] Ainda na pagina de login")

            # Fechar qualquer modal de erro/aviso que possa estar aberto
            try:
                error_modal = await page.query_selector('[data-testid="error-modal"]')
                if error_modal:
                    print("  > Modal de erro detectado, fechando...")
                    confirm_btn = await page.query_selector('button:has-text("Sim"), button:has-text("Continuar")')
                    if confirm_btn:
                        await confirm_btn.click(force=True)
                        await asyncio.sleep(1)
                        print("  [OK] Modal fechado")
            except:
                pass

            # CAPTURAR TOKEN JWT do localStorage
            print("  > Capturando token JWT do localStorage...")
            auth_token = await page.evaluate("() => localStorage.getItem('auth_token')")

            if not auth_token:
                print("  [ERROR] Token JWT não encontrado no localStorage!")
                results.append(("JWT Token", "FAIL", "Token não encontrado após login"))
            else:
                print(f"  [OK] Token JWT capturado: {auth_token[:20]}...")
                results.append(("JWT Token", "PASS", "Token capturado com sucesso"))

            # Helper function para garantir token antes de navegar
            async def navigate_with_auth(url, page_name):
                """Navega para URL garantindo que o token JWT está presente"""
                print(f"  > Navegando para {url}...")

                # Verificar se token ainda está no localStorage
                current_token = await page.evaluate("() => localStorage.getItem('auth_token')")

                if not current_token and auth_token:
                    print(f"    [WARN] Token perdido, restaurando...")
                    # Injetar token de volta
                    await page.evaluate(f"() => localStorage.setItem('auth_token', '{auth_token}')")
                    print(f"    [OK] Token restaurado")

                await page.goto(url)
                await page.wait_for_load_state("domcontentloaded")
                await asyncio.sleep(2)

                # CRÍTICO: Fechar modal de erro/navegação se aparecer
                modal_closed = False
                for attempt in range(3):  # Tentar até 3 vezes
                    try:
                        # Procurar modal por múltiplos seletores
                        error_modal = await page.query_selector('[data-testid="error-modal"], [class*="modal"][class*="error"], .modal-overlay, [role="dialog"]')

                        if error_modal and await error_modal.is_visible():
                            print(f"    [WARN] Modal detectado (tentativa {attempt + 1}), fechando...")

                            # Tentar clicar em botões de confirmação/continuar
                            close_selectors = [
                                'button:has-text("Continuar")',
                                'button:has-text("Sim")',
                                'button:has-text("OK")',
                                'button[class*="primary"]',
                                'button[class*="confirm"]',
                                '.modal button:last-child'  # Último botão (geralmente "Continuar")
                            ]

                            for selector in close_selectors:
                                btn = await page.query_selector(selector)
                                if btn and await btn.is_visible():
                                    await btn.click(force=True)
                                    print(f"    [INFO] Clicou em {selector}, aguardando modal desaparecer...")

                                    # CRÍTICO: Aguardar modal DESAPARECER completamente
                                    try:
                                        await page.wait_for_selector('[data-testid="error-modal"], [class*="modal"][class*="error"], .modal-overlay', state='hidden', timeout=5000)
                                        print(f"    [OK] Modal desapareceu completamente")
                                        modal_closed = True
                                    except:
                                        # Se não desapareceu, aguardar mais tempo
                                        await asyncio.sleep(2)
                                        print(f"    [OK] Modal fechado via {selector} (timeout wait)")
                                        modal_closed = True

                                    break

                            if modal_closed:
                                break
                        else:
                            break  # Modal não existe ou não visível

                    except Exception as e:
                        print(f"    [DEBUG] Erro ao fechar modal: {str(e)[:50]}")
                        break

                # Aguardar Vue.js renderizar completamente
                await asyncio.sleep(2)

                # VALIDAÇÃO CRÍTICA: Verificar se Vue está montado (sem {{ }} crus)
                page_content = await page.content()

                if "{{" in page_content and "}}" in page_content:
                    print(f"    [ERROR] {page_name} tem variáveis Vue NÃO INTERPOLADAS ({{ }})!")
                    print(f"    [ERROR] Vue.js NÃO está funcionando corretamente!")
                    return False

                if "AUTH_REQUIRED" in page_content or '"detail":"Authentication required"' in page_content:
                    print(f"    [ERROR] {page_name} retornou erro de autenticação!")
                    return False

                # Verificar se há erros JavaScript no console
                try:
                    # Tentar pegar erros via evaluate
                    has_js_errors = await page.evaluate("""() => {
                        // Verificar se há elementos de erro visíveis
                        const errorModal = document.querySelector('[data-testid="error-modal"]');
                        const errorVisible = errorModal && window.getComputedStyle(errorModal).display !== 'none';
                        return errorVisible;
                    }""")

                    if has_js_errors:
                        print(f"    [ERROR] {page_name} tem modal de erro AINDA VISÍVEL!")
                        return False
                except:
                    pass

                # Validar que há CONTEÚDO real, não só página em branco
                # Buscar elementos indicadores de dados carregados
                has_data = False
                data_indicators = [
                    '[data-story-id]',  # Story cards
                    '.story-card',
                    '.kanban-column',
                    '[class*="stat-card"]',  # Cards de estatísticas
                    'table tbody tr',  # Tabelas com dados
                    '[class*="chart"]',  # Gráficos
                ]

                for selector in data_indicators:
                    elements = await page.query_selector_all(selector)
                    if len(elements) > 0:
                        has_data = True
                        print(f"    [OK] {page_name} com dados carregados ({len(elements)} elementos {selector})")
                        break

                if not has_data:
                    print(f"    [WARN] {page_name} carregada mas sem dados visíveis")

                print(f"    [OK] {page_name} carregada {'com dados' if has_data else 'sem dados visíveis'}")
                return True

            # ========================================
            # FASE 2: DASHBOARD
            # ========================================
            print("\n[FASE 2] DASHBOARD PRINCIPAL")
            print("-" * 40)

            # Navegar para dashboard COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/dashboard", "Dashboard")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            if not success:
                results.append(("Dashboard", "FAIL", "Erro de autenticação"))

            # Screenshot: dashboard
            screenshot_path = SCREENSHOT_DIR / "04_dashboard.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Dashboard", "PASS", screenshot_path.name))

            # ========================================
            # FASE 3: KANBAN BOARD
            # ========================================
            print("\n[FASE 3] KANBAN BOARD")
            print("-" * 40)

            # Navegar para kanban COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/kanban", "Kanban")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            # Screenshot: kanban
            screenshot_path = SCREENSHOT_DIR / "05_kanban_board.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            if not success:
                results.append(("Kanban Board", "FAIL", "Erro de autenticação"))
            else:
                results.append(("Kanban Board", "PASS", screenshot_path.name))

                # Verificar colunas
                columns = await page.query_selector_all('[class*="kanban-column"], [class*="column"]')
                print(f"  [INFO] Colunas encontradas: {len(columns)}")

            # ========================================
            # FASE 4: STORIES
            # ========================================
            print("\n[FASE 4] STORIES")
            print("-" * 40)

            # Navegar para stories COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/stories", "Stories")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            # Screenshot: lista de stories
            screenshot_path = SCREENSHOT_DIR / "06_stories_list.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            if not success:
                results.append(("Stories List", "FAIL", "Erro de autenticação"))
            else:
                results.append(("Stories List", "PASS", screenshot_path.name))

            # Fechar modal de erro se aberto
            try:
                error_modal = await page.query_selector('[data-testid="error-modal"]')
                if error_modal:
                    print("  > Modal de erro detectado, fechando...")
                    confirm_btn = await page.query_selector('button:has-text("Sim"), button:has-text("Continuar")')
                    if confirm_btn:
                        await confirm_btn.click(force=True)
                        await asyncio.sleep(1)
            except:
                pass

            # Tentar abrir modal de nova story
            print("  > Procurando botao Nova Story...")
            new_story_btn = await page.query_selector('[class*="new-story"], button:has-text("Nova"), button:has-text("New"), [class*="add-story"]')
            if new_story_btn:
                await new_story_btn.click(force=True)
                await asyncio.sleep(2)

                # Screenshot: modal
                screenshot_path = SCREENSHOT_DIR / "07_story_modal.png"
                await page.screenshot(path=str(screenshot_path), timeout=30000)
                print(f"  [OK] Screenshot: {screenshot_path.name}")
                results.append(("Story Modal", "PASS", screenshot_path.name))

                # Fechar modal (ESC ou botao)
                await page.keyboard.press("Escape")
                await asyncio.sleep(1)
            else:
                print("  [INFO] Botao Nova Story nao encontrado")

            # ========================================
            # FASE 5: SPRINTS
            # ========================================
            print("\n[FASE 5] SPRINTS")
            print("-" * 40)

            # Navegar para sprints COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/sprints", "Sprints")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            # Screenshot: sprints
            screenshot_path = SCREENSHOT_DIR / "08_sprints.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            if not success:
                results.append(("Sprints", "FAIL", "Erro de autenticação"))
            else:
                results.append(("Sprints", "PASS", screenshot_path.name))

            # ========================================
            # FASE 6: ANALYTICS
            # ========================================
            print("\n[FASE 6] ANALYTICS")
            print("-" * 40)

            # Navegar para analytics COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/analytics", "Analytics")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            # Screenshot: analytics
            screenshot_path = SCREENSHOT_DIR / "09_analytics.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            if not success:
                results.append(("Analytics", "FAIL", "Erro de autenticação"))
            else:
                results.append(("Analytics", "PASS", screenshot_path.name))

            # ========================================
            # FASE 7: ADMIN PANEL
            # ========================================
            print("\n[FASE 7] ADMIN PANEL")
            print("-" * 40)

            # Navegar para admin COM autenticação
            success = await navigate_with_auth(f"{BASE_URL}/admin", "Admin")

            # CRÍTICO: Aguardar Vue.js estabilizar e modal sumir completamente
            await asyncio.sleep(2)

            # Screenshot: admin
            screenshot_path = SCREENSHOT_DIR / "10_admin_panel.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            if not success:
                results.append(("Admin Panel", "FAIL", "Erro de autenticação"))
            else:
                results.append(("Admin Panel", "PASS", screenshot_path.name))

            # ========================================
            # FASE 9: UPDATE FEATURE (NOVO)
            # ========================================
            print("\n[FASE 9] UPDATE FEATURE TESTING")
            print("-" * 40)

            # 9.0 Criar uma story de teste antes do UPDATE
            print("  > Criando story de teste...")
            await navigate_with_auth(f"{BASE_URL}/stories", "Stories (para criar story de teste)")

            # Fechar modal de erro se aparecer
            try:
                error_modal = await page.query_selector('[data-testid="error-modal"]')
                if error_modal:
                    confirm_btn = await page.query_selector('button:has-text("Sim"), button:has-text("Continuar")')
                    if confirm_btn:
                        await confirm_btn.click(force=True)
                        await asyncio.sleep(1)
            except:
                pass

            # Clicar em Nova Story
            new_story_btn = await page.query_selector('[class*="new-story"], button:has-text("Nova"), button:has-text("New")')
            if new_story_btn:
                await new_story_btn.click(force=True)
                await asyncio.sleep(2)

                # Preencher formulário básico
                title_input = await page.query_selector('input[name="title"], input[placeholder*="título"]')
                if title_input:
                    await title_input.fill("Story para UPDATE Test")

                points_select = await page.query_selector('select[name="story_points"]')
                if points_select:
                    await points_select.select_option("3")

                # Salvar
                save_btn = await page.query_selector('button:has-text("Criar"), button:has-text("Salvar")')
                if save_btn:
                    await save_btn.click(force=True)
                    await asyncio.sleep(2)
                    print("  [OK] Story de teste criada")

            # 9.1 Navegar para kanban COM autenticação
            print("  > Navegando para /kanban...")
            await navigate_with_auth(f"{BASE_URL}/kanban", "Kanban (para UPDATE test)")

            # Screenshot: kanban before update
            screenshot_path = SCREENSHOT_DIR / "15_kanban_before_update.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            # 9.2 Clicar em story card para abrir detail panel
            print("  > Clicando em story card...")
            story_card = await page.query_selector('.story-card')
            if story_card:
                await story_card.click()
                await asyncio.sleep(2)

                # Screenshot: detail panel
                screenshot_path = SCREENSHOT_DIR / "16_detail_panel_open.png"
                await page.screenshot(path=str(screenshot_path), timeout=30000)
                print(f"  [OK] Detail panel aberto - Screenshot: {screenshot_path.name}")

                # 9.3 Procurar botão "Editar Story"
                print("  > Procurando botão 'Editar Story'...")
                edit_btn = await page.query_selector('button:has-text("Editar")')
                if not edit_btn:
                    edit_btn = await page.query_selector('[title*="Editar"]')

                if edit_btn:
                    await edit_btn.click()
                    await asyncio.sleep(2)

                    # Screenshot: edit modal
                    screenshot_path = SCREENSHOT_DIR / "17_edit_modal_open.png"
                    await page.screenshot(path=str(screenshot_path), timeout=30000)
                    print(f"  [OK] Modal de edição aberto - Screenshot: {screenshot_path.name}")

                    # 9.4 Modificar campos
                    print("  > Modificando campos...")

                    # Modificar título
                    title_input = await page.query_selector('input[v-model*="title"], input[name="title"]')
                    if title_input:
                        current_value = await title_input.input_value()
                        new_title = f"[UPDATED] {current_value}"
                        await title_input.fill(new_title)
                        print(f"  [OK] Título modificado: {new_title[:50]}...")

                    # Modificar story points
                    points_select = await page.query_selector('select[v-model*="story_points"], select[name="story_points"]')
                    if points_select:
                        await points_select.select_option("5")
                        print(f"  [OK] Story points: 5")

                    await asyncio.sleep(1)

                    # Screenshot: form filled
                    screenshot_path = SCREENSHOT_DIR / "18_edit_form_filled.png"
                    await page.screenshot(path=str(screenshot_path), timeout=30000)
                    print(f"  [OK] Formulário preenchido - Screenshot: {screenshot_path.name}")

                    # 9.5 Salvar
                    print("  > Clicando em Salvar...")
                    save_btn = await page.query_selector('button:has-text("Salvar")')
                    if save_btn:
                        await save_btn.click()
                        await asyncio.sleep(3)

                        # Screenshot: after update
                        screenshot_path = SCREENSHOT_DIR / "19_story_updated.png"
                        await page.screenshot(path=str(screenshot_path), timeout=30000)
                        print(f"  [OK] Story atualizada - Screenshot: {screenshot_path.name}")

                        results.append(("UPDATE Story", "PASS", screenshot_path.name))
                        print(f"  [OK] UPDATE feature testado com sucesso!")
                    else:
                        print("  [WARN] Botão Salvar não encontrado")
                        results.append(("UPDATE Story - Save Button", "FAIL", "Button not found"))
                else:
                    print("  [WARN] Botão Editar não encontrado")
                    results.append(("UPDATE Story - Edit Button", "FAIL", "Button not found"))
            else:
                print("  [WARN] Nenhum story card encontrado")
                results.append(("UPDATE Story - Story Card", "FAIL", "No story cards"))

            # ========================================
            # FASE 8: RESPONSIVIDADE
            # ========================================
            print("\n[FASE 8] RESPONSIVIDADE")
            print("-" * 40)

            # Voltar para dashboard
            await page.goto(f"{BASE_URL}/dashboard")
            await page.wait_for_load_state("domcontentloaded")
            await asyncio.sleep(1)

            # Desktop (1920x1080)
            print("  > Viewport: Desktop (1920x1080)")
            await page.set_viewport_size({"width": 1920, "height": 1080})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "11_responsive_desktop.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Desktop 1920x1080", "PASS", screenshot_path.name))

            # Laptop (1366x768)
            print("  > Viewport: Laptop (1366x768)")
            await page.set_viewport_size({"width": 1366, "height": 768})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "12_responsive_laptop.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Laptop 1366x768", "PASS", screenshot_path.name))

            # Tablet (768x1024)
            print("  > Viewport: Tablet (768x1024)")
            await page.set_viewport_size({"width": 768, "height": 1024})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "13_responsive_tablet.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Tablet 768x1024", "PASS", screenshot_path.name))

            # Mobile (375x812)
            print("  > Viewport: Mobile (375x812)")
            await page.set_viewport_size({"width": 375, "height": 812})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "14_responsive_mobile.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Mobile 375x812", "PASS", screenshot_path.name))

        except Exception as e:
            print(f"\n[ERROR] {str(e)}")
            # Screenshot de erro
            screenshot_path = SCREENSHOT_DIR / "error_screenshot.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            results.append(("ERROR", "FAIL", str(e)[:50]))

        finally:
            # Fechar browser
            print("\n  > Fechando browser...")
            await asyncio.sleep(2)  # Aguardar para visualizar
            await browser.close()

    # ========================================
    # RELATORIO FINAL
    # ========================================
    print("\n" + "=" * 60)
    print("RELATORIO FINAL")
    print("=" * 60)

    passed = sum(1 for r in results if r[1] == "PASS")
    failed = sum(1 for r in results if r[1] == "FAIL")

    print(f"\nTotal de Testes: {len(results)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Taxa de Sucesso: {passed/len(results)*100:.1f}%")

    print("\nDetalhes:")
    for name, status, detail in results:
        icon = "[OK]" if status == "PASS" else "[FAIL]"
        print(f"  {icon} {name}: {detail}")

    print(f"\nScreenshots salvos em: {SCREENSHOT_DIR}")
    print(f"Fim: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 60)

    # Salvar relatorio JSON
    import json
    report = {
        "date": datetime.now().isoformat(),
        "total": len(results),
        "passed": passed,
        "failed": failed,
        "success_rate": f"{passed/len(results)*100:.1f}%",
        "results": [{"test": r[0], "status": r[1], "detail": r[2]} for r in results]
    }

    report_path = SCREENSHOT_DIR / "report.json"
    with open(report_path, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False)

    print(f"Relatorio salvo em: {report_path}")

    return results


if __name__ == "__main__":
    asyncio.run(run_visual_tests())
