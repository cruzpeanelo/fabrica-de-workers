# -*- coding: utf-8 -*-
"""
Teste SIMPLES de Validação Vue - SEM PASSES FALSOS
====================================================
Valida que Vue.js está funcionando e não há {{ }} crus
"""

import asyncio
from pathlib import Path

BASE_URL = "http://localhost:9001"
SCREENSHOT_DIR = Path("C:/Users/lcruz/Fabrica de Agentes/analysis/screenshots/vue_validation")
SCREENSHOT_DIR.mkdir(parents=True, exist_ok=True)

async def test_page_validation():
    from playwright.async_api import async_playwright

    print("=" * 60)
    print("VALIDAÇÃO VUE.JS - TESTE RIGOROSO")
    print("=" * 60)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=500)
        context = await browser.new_context(viewport={"width": 1920, "height": 1080})
        page = await context.new_page()

        # Capturar erros
        errors = []
        page.on("pageerror", lambda err: errors.append(str(err)))

        try:
            # LOGIN
            print("\n[1] LOGIN")
            await page.goto(f"{BASE_URL}/login")
            await page.wait_for_load_state("domcontentloaded")
            await asyncio.sleep(2)

            await page.fill('input[type="text"]', "platform_admin")
            await page.fill('input[type="password"]', "Platform@2025!Adm")
            await page.click('button[type="submit"]')

            await asyncio.sleep(5)  # Aguardar login

            # Capturar token
            token = await page.evaluate("() => localStorage.getItem('auth_token')")
            if not token:
                print("  [FAIL] Token não encontrado!")
                return

            print(f"  [OK] Token: {token[:20]}...")

            # TESTAR KANBAN
            print("\n[2] KANBAN - Validação Rigorosa")
            await page.goto(f"{BASE_URL}/kanban")
            await page.wait_for_load_state("domcontentloaded")
            await asyncio.sleep(3)

            # Aguardar Vue montar
            for i in range(10):  # Tentar até 10 segundos
                content = await page.content()

                # VALIDAÇÃO 1: Não pode ter {{ }}
                if "{{" in content and "}}" in content:
                    print(f"  [{i+1}] Vue não montado ainda (tem {{{{}}}})")
                    await asyncio.sleep(1)
                    continue

                # VALIDAÇÃO 2: Não pode ter modal de erro
                try:
                    modal = await page.query_selector('[data-testid="error-modal"]')
                    if modal:
                        is_visible = await modal.is_visible()
                        if is_visible:
                            print(f"  [{i+1}] Modal de erro visível, fechando...")
                            # Clicar em "Continuar" ou "Sim"
                            btn = await page.query_selector('button:has-text("Continuar"), button:has-text("Sim")')
                            if btn:
                                await btn.click(force=True)
                                await asyncio.sleep(2)
                                continue
                except:
                    pass

                # Se chegou aqui, página está OK
                print(f"  [OK] Vue montado em {i+1} segundos")
                break
            else:
                print("  [FAIL] Vue não montou em 10 segundos!")

            # VALIDAÇÃO FINAL
            final_content = await page.content()

            # Verificar {{ }}
            if "{{" in final_content and "}}" in final_content:
                print("  [FAIL] Página tem variáveis Vue NÃO INTERPOLADAS!")
                # Mostrar amostra
                import re
                matches = re.findall(r'\{\{[^}]+\}\}', final_content)
                print(f"  [FAIL] Exemplos: {matches[:5]}")
            else:
                print("  [OK] Nenhum {{}} cru encontrado")

            # Verificar erros JS
            if errors:
                print(f"  [FAIL] {len(errors)} erros JavaScript:")
                for err in errors[:5]:
                    print(f"    - {err[:80]}")
            else:
                print("  [OK] Nenhum erro JavaScript")

            # Verificar dados
            story_cards = await page.query_selector_all('[data-story-id]')
            print(f"  [INFO] Story cards encontrados: {len(story_cards)}")

            kanban_columns = await page.query_selector_all('.kanban-column, [class*="column"]')
            print(f"  [INFO] Colunas Kanban: {len(kanban_columns)}")

            # Screenshot FINAL
            await asyncio.sleep(2)
            screenshot_path = SCREENSHOT_DIR / "kanban_final.png"
            await page.screenshot(path=str(screenshot_path), timeout=30000)
            print(f"  [OK] Screenshot: {screenshot_path}")

        except Exception as e:
            print(f"[ERROR] {e}")
        finally:
            await browser.close()

    print("\n" + "=" * 60)
    print("TESTE CONCLUÍDO")
    print("=" * 60)


if __name__ == "__main__":
    asyncio.run(test_page_validation())
