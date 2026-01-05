# -*- coding: utf-8 -*-
"""
Analise detalhada de cada tela da Plataforma E
"""
import asyncio
import os
import json
from datetime import datetime
from playwright.async_api import async_playwright

BASE_URL = "http://localhost:9001"

# Telas para analisar
SCREENS = [
    {"path": "/login", "name": "Login", "check_elements": ["form", "input", "button"]},
    {"path": "/", "name": "Dashboard Principal", "check_elements": [".kanban-column", ".story-card", ".sidebar"]},
    {"path": "/projects", "name": "Lista de Projetos", "check_elements": [".project-card", "table", ".grid"]},
    {"path": "/executive", "name": "Dashboard Executivo", "check_elements": [".chart", ".kpi", ".metric"]},
    {"path": "/analytics", "name": "Analytics", "check_elements": [".chart", ".graph", "canvas"]},
    {"path": "/billing", "name": "Faturamento", "check_elements": [".invoice", ".plan", ".subscription"]},
    {"path": "/admin", "name": "Admin Panel", "check_elements": [".admin", ".settings", "form"]},
    {"path": "/admin/users", "name": "Gestao Usuarios", "check_elements": ["table", ".user", "tr"]},
    {"path": "/workers", "name": "Workers", "check_elements": [".worker", ".status", ".queue"]},
    {"path": "/integrations", "name": "Integracoes", "check_elements": [".integration", ".card", ".connection"]},
]

async def analyze_screens():
    """Analisa cada tela e reporta problemas"""
    os.makedirs('analysis', exist_ok=True)

    results = {"screens": [], "issues": [], "timestamp": datetime.now().isoformat()}

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=500)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        # Capturar erros de console
        console_errors = []
        page.on("console", lambda msg: console_errors.append({"type": msg.type, "text": msg.text}) if msg.type == "error" else None)

        for idx, screen in enumerate(SCREENS):
            print(f"\n{'='*60}")
            print(f"  [{idx+1}/{len(SCREENS)}] Analisando: {screen['name']}")
            print(f"  URL: {BASE_URL}{screen['path']}")
            print(f"{'='*60}")

            console_errors.clear()
            screen_result = {
                "name": screen["name"],
                "path": screen["path"],
                "status": "unknown",
                "issues": [],
                "elements_found": [],
                "elements_missing": [],
                "console_errors": [],
            }

            try:
                # Navegar
                response = await page.goto(f"{BASE_URL}{screen['path']}", timeout=15000)
                await page.wait_for_load_state('networkidle', timeout=10000)
                await asyncio.sleep(1)

                http_status = response.status if response else 0
                screen_result["http_status"] = http_status

                print(f"  HTTP Status: {http_status}")

                if http_status != 200:
                    screen_result["status"] = "error"
                    screen_result["issues"].append(f"HTTP {http_status}")
                    print(f"  [ERRO] Status HTTP {http_status}")

                # Verificar erros de console
                if console_errors:
                    screen_result["console_errors"] = console_errors.copy()
                    for err in console_errors[:3]:
                        print(f"  [CONSOLE] {err['text'][:80]}")
                        screen_result["issues"].append(f"Console: {err['text'][:100]}")

                # Verificar elementos esperados
                for selector in screen.get("check_elements", []):
                    try:
                        elements = await page.query_selector_all(selector)
                        if elements:
                            visible_count = 0
                            for el in elements:
                                if await el.is_visible():
                                    visible_count += 1
                            if visible_count > 0:
                                screen_result["elements_found"].append(f"{selector}: {visible_count}")
                                print(f"  [OK] {selector}: {visible_count} elementos")
                            else:
                                screen_result["elements_missing"].append(selector)
                                print(f"  [WARN] {selector}: existem mas nao visiveis")
                        else:
                            screen_result["elements_missing"].append(selector)
                            print(f"  [MISS] {selector}: nao encontrado")
                    except Exception as e:
                        print(f"  [ERR] {selector}: {str(e)[:50]}")

                # Verificar textos {{ }} nao renderizados (Vue não montou)
                body = await page.query_selector('body')
                body_text = await body.text_content() if body else ""

                if "{{" in body_text and "}}" in body_text:
                    screen_result["issues"].append("Vue templates {{ }} nao renderizados")
                    print(f"  [ERRO] Templates Vue {{ }} visiveis - Vue nao montou!")

                # Verificar se tem conteudo
                content_length = len(body_text.strip())
                if content_length < 100:
                    screen_result["issues"].append("Pouco conteudo na pagina")
                    print(f"  [WARN] Pouco conteudo: {content_length} caracteres")
                else:
                    print(f"  [OK] Conteudo: {content_length} caracteres")

                # Verificar elementos de erro visiveis
                error_selectors = [".error", ".alert-danger", "[class*='error']", ".error-message"]
                for err_sel in error_selectors:
                    try:
                        err_elements = await page.query_selector_all(err_sel)
                        for err_el in err_elements:
                            if await err_el.is_visible():
                                err_text = await err_el.text_content()
                                if err_text and len(err_text.strip()) > 0:
                                    screen_result["issues"].append(f"Erro visivel: {err_text[:50]}")
                                    print(f"  [ERRO] Mensagem de erro: {err_text[:50]}")
                    except:
                        pass

                # Screenshot
                screenshot_path = f"analysis/{idx+1:02d}_{screen['path'].replace('/', '_').strip('_') or 'home'}.png"
                await page.screenshot(path=screenshot_path)
                screen_result["screenshot"] = screenshot_path
                print(f"  [OK] Screenshot: {screenshot_path}")

                # Status final
                if not screen_result["issues"]:
                    screen_result["status"] = "ok"
                elif len(screen_result["issues"]) <= 2:
                    screen_result["status"] = "warning"
                else:
                    screen_result["status"] = "error"

            except Exception as e:
                screen_result["status"] = "error"
                screen_result["issues"].append(str(e)[:100])
                print(f"  [ERRO] Excecao: {str(e)[:80]}")

            results["screens"].append(screen_result)
            results["issues"].extend([{"screen": screen["name"], "issue": i} for i in screen_result["issues"]])

        # Manter browser aberto para inspecao
        print("\n" + "="*60)
        print("  ANALISE COMPLETA - Browser aberto por 15s")
        print("="*60)
        await asyncio.sleep(15)
        await browser.close()

    # Resumo
    print("\n" + "="*60)
    print("  RESUMO DA ANALISE")
    print("="*60)

    ok_count = sum(1 for s in results["screens"] if s["status"] == "ok")
    warn_count = sum(1 for s in results["screens"] if s["status"] == "warning")
    error_count = sum(1 for s in results["screens"] if s["status"] == "error")

    print(f"\n  OK: {ok_count} | Warnings: {warn_count} | Errors: {error_count}")
    print(f"  Total Issues: {len(results['issues'])}")

    if results["issues"]:
        print("\n  PROBLEMAS ENCONTRADOS:")
        for issue in results["issues"]:
            print(f"    - [{issue['screen']}] {issue['issue'][:60]}")

    # Salvar resultados
    with open('analysis/results.json', 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False, default=str)

    print(f"\n  Resultados salvos em: analysis/results.json")
    print(f"  Screenshots em: analysis/")

    return results

if __name__ == "__main__":
    import sys
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(encoding='utf-8')
    asyncio.run(analyze_screens())
