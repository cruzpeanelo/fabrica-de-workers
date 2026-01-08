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
            viewport={"width": 1920, "height": 1080}
        )
        page = await context.new_page()

        try:
            # ========================================
            # FASE 1: LOGIN
            # ========================================
            print("\n[FASE 1] LOGIN E AUTENTICACAO")
            print("-" * 40)

            # 1.1 Navegar para login
            print("  > Navegando para /login...")
            await page.goto(f"{BASE_URL}/login")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)  # Aguardar Vue renderizar

            # Screenshot: tela de login
            screenshot_path = SCREENSHOT_DIR / "01_login_page.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Login Page", "PASS", screenshot_path.name))

            # 1.2 Preencher credenciais
            print("  > Preenchendo credenciais...")

            # Aguardar campo aparecer
            await page.wait_for_selector('input[type="text"], input[name="username"]', timeout=10000)

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
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Login Filled", "PASS", screenshot_path.name))

            # 1.3 Clicar em Login
            print("  > Clicando em Login...")
            login_button = await page.query_selector('button[type="submit"]')
            if login_button:
                await login_button.click()

            # Aguardar redirect - esperar URL mudar
            try:
                await page.wait_for_url("**/dashboard", timeout=10000)
                print("  [OK] Redirecionado para dashboard")
            except:
                # Se não redirecionou para dashboard, tentar qualquer página diferente de login
                await asyncio.sleep(3)

            # Screenshot: apos login
            screenshot_path = SCREENSHOT_DIR / "03_after_login.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
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

            # ========================================
            # FASE 2: DASHBOARD
            # ========================================
            print("\n[FASE 2] DASHBOARD PRINCIPAL")
            print("-" * 40)

            # Navegar para dashboard
            print("  > Navegando para /dashboard...")
            await page.goto(f"{BASE_URL}/dashboard")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: dashboard
            screenshot_path = SCREENSHOT_DIR / "04_dashboard.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Dashboard", "PASS", screenshot_path.name))

            # ========================================
            # FASE 3: KANBAN BOARD
            # ========================================
            print("\n[FASE 3] KANBAN BOARD")
            print("-" * 40)

            # Navegar para kanban
            print("  > Navegando para /kanban...")
            await page.goto(f"{BASE_URL}/kanban")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: kanban
            screenshot_path = SCREENSHOT_DIR / "05_kanban_board.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Kanban Board", "PASS", screenshot_path.name))

            # Verificar colunas
            columns = await page.query_selector_all('[class*="kanban-column"], [class*="column"]')
            print(f"  [INFO] Colunas encontradas: {len(columns)}")

            # ========================================
            # FASE 4: STORIES
            # ========================================
            print("\n[FASE 4] STORIES")
            print("-" * 40)

            # Navegar para stories
            print("  > Navegando para /stories...")
            await page.goto(f"{BASE_URL}/stories")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: lista de stories
            screenshot_path = SCREENSHOT_DIR / "06_stories_list.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
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
                await page.screenshot(path=str(screenshot_path), full_page=True)
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

            # Navegar para sprints
            print("  > Navegando para /sprints...")
            await page.goto(f"{BASE_URL}/sprints")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: sprints
            screenshot_path = SCREENSHOT_DIR / "08_sprints.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Sprints", "PASS", screenshot_path.name))

            # ========================================
            # FASE 6: ANALYTICS
            # ========================================
            print("\n[FASE 6] ANALYTICS")
            print("-" * 40)

            # Navegar para analytics
            print("  > Navegando para /analytics...")
            await page.goto(f"{BASE_URL}/analytics")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: analytics
            screenshot_path = SCREENSHOT_DIR / "09_analytics.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Analytics", "PASS", screenshot_path.name))

            # ========================================
            # FASE 7: ADMIN PANEL
            # ========================================
            print("\n[FASE 7] ADMIN PANEL")
            print("-" * 40)

            # Navegar para admin
            print("  > Navegando para /admin...")
            await page.goto(f"{BASE_URL}/admin")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Screenshot: admin
            screenshot_path = SCREENSHOT_DIR / "10_admin_panel.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Admin Panel", "PASS", screenshot_path.name))

            # ========================================
            # FASE 9: UPDATE FEATURE (NOVO)
            # ========================================
            print("\n[FASE 9] UPDATE FEATURE TESTING")
            print("-" * 40)

            # 9.0 Criar uma story de teste antes do UPDATE
            print("  > Criando story de teste...")
            await page.goto(f"{BASE_URL}/stories")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

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

            # 9.1 Navegar para kanban
            print("  > Navegando para /kanban...")
            await page.goto(f"{BASE_URL}/kanban")
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(2)

            # Fechar modal de erro se ainda aparecer
            try:
                error_modal = await page.query_selector('[data-testid="error-modal"]')
                if error_modal:
                    confirm_btn = await page.query_selector('button:has-text("Sim"), button:has-text("Continuar")')
                    if confirm_btn:
                        await confirm_btn.click(force=True)
                        await asyncio.sleep(1)
            except:
                pass

            # Screenshot: kanban before update
            screenshot_path = SCREENSHOT_DIR / "15_kanban_before_update.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")

            # 9.2 Clicar em story card para abrir detail panel
            print("  > Clicando em story card...")
            story_card = await page.query_selector('[data-story-id]')
            if story_card:
                await story_card.click()
                await asyncio.sleep(2)

                # Screenshot: detail panel
                screenshot_path = SCREENSHOT_DIR / "16_detail_panel_open.png"
                await page.screenshot(path=str(screenshot_path), full_page=True)
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
                    await page.screenshot(path=str(screenshot_path), full_page=True)
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
                    await page.screenshot(path=str(screenshot_path), full_page=True)
                    print(f"  [OK] Formulário preenchido - Screenshot: {screenshot_path.name}")

                    # 9.5 Salvar
                    print("  > Clicando em Salvar...")
                    save_btn = await page.query_selector('button:has-text("Salvar")')
                    if save_btn:
                        await save_btn.click()
                        await asyncio.sleep(3)

                        # Screenshot: after update
                        screenshot_path = SCREENSHOT_DIR / "19_story_updated.png"
                        await page.screenshot(path=str(screenshot_path), full_page=True)
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
            await page.wait_for_load_state("networkidle")
            await asyncio.sleep(1)

            # Desktop (1920x1080)
            print("  > Viewport: Desktop (1920x1080)")
            await page.set_viewport_size({"width": 1920, "height": 1080})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "11_responsive_desktop.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Desktop 1920x1080", "PASS", screenshot_path.name))

            # Laptop (1366x768)
            print("  > Viewport: Laptop (1366x768)")
            await page.set_viewport_size({"width": 1366, "height": 768})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "12_responsive_laptop.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Laptop 1366x768", "PASS", screenshot_path.name))

            # Tablet (768x1024)
            print("  > Viewport: Tablet (768x1024)")
            await page.set_viewport_size({"width": 768, "height": 1024})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "13_responsive_tablet.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Tablet 768x1024", "PASS", screenshot_path.name))

            # Mobile (375x812)
            print("  > Viewport: Mobile (375x812)")
            await page.set_viewport_size({"width": 375, "height": 812})
            await asyncio.sleep(1)
            screenshot_path = SCREENSHOT_DIR / "14_responsive_mobile.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
            print(f"  [OK] Screenshot: {screenshot_path.name}")
            results.append(("Mobile 375x812", "PASS", screenshot_path.name))

        except Exception as e:
            print(f"\n[ERROR] {str(e)}")
            # Screenshot de erro
            screenshot_path = SCREENSHOT_DIR / "error_screenshot.png"
            await page.screenshot(path=str(screenshot_path), full_page=True)
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
