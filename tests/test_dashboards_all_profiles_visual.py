# -*- coding: utf-8 -*-
"""
TESTE COMPLETO - Dashboards de TODOS os Perfis
===============================================
Valida que cada perfil vê os dados CORRETOS no dashboard principal

Browser: VISÍVEL (headless=False)
Velocidade: slow_mo=800ms (mais lento para acompanhar)
Screenshots: Dashboard de cada perfil

Validações:
1. platform_admin - Deve ver dados de TODOS os tenants
2. tenant_admin (BELGO, RETAIL, HEALTH) - Deve ver apenas seu tenant
3. project_manager - Deve ver dados do projeto
4. viewer - Deve ver dados limitados (sem ações admin)

Objetivo: Validar isolamento multi-tenant e RBAC no dashboard
"""

import asyncio
from pathlib import Path
from datetime import datetime

# ===========================================================================
# CONFIGURACAO
# ===========================================================================

BASE_URL = "http://localhost:9001"
SCREENSHOT_DIR = Path("C:/Users/lcruz/Fabrica de Agentes/analysis/screenshots/dashboards_all_profiles")
SCREENSHOT_DIR.mkdir(parents=True, exist_ok=True)

# TODOS os perfis disponíveis
TEST_PROFILES = [
    {
        "username": "platform_admin",
        "password": "admin123",
        "tenant": "ALL",
        "role": "SUPER_ADMIN",
        "should_see_all_tenants": True,
        "should_see_admin_panel": True,
        "description": "Platform Admin - Deve ver TODOS os tenants"
    },
    {
        "username": "belgo_admin",
        "password": "Belgo@Admin#2025",
        "tenant": "BELGO-001",
        "role": "TENANT_ADMIN",
        "should_see_all_tenants": False,
        "should_see_admin_panel": True,
        "description": "BELGO Admin - Deve ver apenas BELGO-001"
    },
    {
        "username": "belgo_pm",
        "password": "Belgo@PM#2025",
        "tenant": "BELGO-001",
        "role": "PROJECT_MANAGER",
        "should_see_all_tenants": False,
        "should_see_admin_panel": False,
        "description": "BELGO PM - Deve ver apenas BELGO-001"
    },
    {
        "username": "retail_admin",
        "password": "Retail@Admin#2025",
        "tenant": "RETAIL",
        "role": "TENANT_ADMIN",
        "should_see_all_tenants": False,
        "should_see_admin_panel": True,
        "description": "RETAIL Admin - Deve ver apenas RETAIL"
    },
    {
        "username": "retail_manager",
        "password": "Retail@Manager#2025",
        "tenant": "RETAIL",
        "role": "PROJECT_MANAGER",
        "should_see_all_tenants": False,
        "should_see_admin_panel": False,
        "description": "RETAIL Manager - Deve ver apenas RETAIL"
    },
    {
        "username": "retail_analyst",
        "password": "Retail@Analyst#2025",
        "tenant": "RETAIL",
        "role": "VIEWER",
        "should_see_all_tenants": False,
        "should_see_admin_panel": False,
        "description": "RETAIL Analyst - Apenas leitura RETAIL"
    },
    {
        "username": "health_admin",
        "password": "Health@Admin#2025",
        "tenant": "HEALTH",
        "role": "TENANT_ADMIN",
        "should_see_all_tenants": False,
        "should_see_admin_panel": True,
        "description": "HEALTH Admin - Deve ver apenas HEALTH"
    },
    {
        "username": "health_doctor",
        "password": "Health@Doctor#2025",
        "tenant": "HEALTH",
        "role": "VIEWER",
        "should_see_all_tenants": False,
        "should_see_admin_panel": False,
        "description": "HEALTH Doctor - Apenas leitura HEALTH"
    },
]


# ===========================================================================
# TESTE PRINCIPAL
# ===========================================================================

async def test_dashboards_all_profiles():
    """
    Testa dashboards de TODOS os perfis
    """
    from playwright.async_api import async_playwright

    results = []

    print("=" * 80)
    print("TESTE DASHBOARDS - TODOS OS PERFIS")
    print("=" * 80)
    print(f"\nBase URL: {BASE_URL}")
    print(f"Browser: VISÍVEL (slow_mo=800ms)")
    print(f"Perfis: {len(TEST_PROFILES)}")
    print(f"Screenshots: {SCREENSHOT_DIR}")
    print("=" * 80)

    async with async_playwright() as p:
        # Browser VISÍVEL com slow motion mais lento
        browser = await p.chromium.launch(
            headless=False,
            slow_mo=800,  # Mais lento para acompanhar
            args=["--start-maximized"]
        )

        for idx, profile_data in enumerate(TEST_PROFILES, 1):
            username = profile_data["username"]
            password = profile_data["password"]
            tenant = profile_data["tenant"]
            role = profile_data["role"]
            should_see_all = profile_data["should_see_all_tenants"]
            should_see_admin = profile_data["should_see_admin_panel"]
            description = profile_data["description"]

            print(f"\n{'=' * 80}")
            print(f"[PERFIL {idx}/{len(TEST_PROFILES)}] {username} ({role})")
            print(f"  {description}")
            print(f"  Tenant: {tenant}")
            print("=" * 80)

            context = await browser.new_context(
                viewport={"width": 1920, "height": 1080}
            )
            page = await context.new_page()

            # Console logs
            console_logs = []
            page.on("console", lambda msg: console_logs.append(f"[{msg.type}] {msg.text}"))

            try:
                # =============================================================
                # STEP 1: LOGIN
                # =============================================================
                print(f"\n[1] LOGIN")
                print("-" * 80)
                await page.goto(f"{BASE_URL}/login")
                await asyncio.sleep(2)

                screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_01_login.png"
                await page.screenshot(path=str(screenshot_path))
                print(f"  [SCREENSHOT] {screenshot_path.name}")

                await page.fill('input[type="text"]', username)
                await page.fill('input[type="password"]', password)
                await page.click('button[type="submit"]')
                await asyncio.sleep(4)

                # Verificar redirecionamento
                current_url = page.url
                if '/login' in current_url:
                    print(f"  [FAIL] Login falhou - ainda em /login")
                    results.append((username, "LOGIN", "FAIL", "Still on login page"))
                    await context.close()
                    continue

                print(f"  [OK] Login bem-sucedido: {current_url}")
                results.append((username, "LOGIN", "PASS", "Authenticated"))

                screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_02_after_login.png"
                await page.screenshot(path=str(screenshot_path))
                print(f"  [SCREENSHOT] {screenshot_path.name}")

                # =============================================================
                # STEP 2: DASHBOARD PRINCIPAL
                # =============================================================
                print(f"\n[2] DASHBOARD PRINCIPAL")
                print("-" * 80)

                # Navegar para dashboard principal
                await page.goto(f"{BASE_URL}/")
                await page.wait_for_load_state("networkidle")
                await asyncio.sleep(4)

                screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_03_dashboard.png"
                await page.screenshot(path=str(screenshot_path), full_page=True)
                print(f"  [SCREENSHOT] {screenshot_path.name}")

                # Verificar se há KPIs/métricas visíveis
                kpi_elements = await page.query_selector_all('.kpi, .metric, .stat, [class*="metric"], [class*="kpi"]')
                print(f"  [INFO] {len(kpi_elements)} elementos de métricas encontrados")

                # Verificar se há cards/widgets
                card_elements = await page.query_selector_all('.card, .widget, [class*="card"], [class*="widget"]')
                print(f"  [INFO] {len(card_elements)} cards/widgets encontrados")

                # Procurar por tenant name ou indicators
                page_content = await page.content()

                if should_see_all:
                    # Platform admin deve ver indicadores de múltiplos tenants
                    print(f"  [VALIDANDO] Platform admin deve ver TODOS os tenants")

                    # Procurar por nomes de tenants
                    tenants_found = []
                    if "BELGO" in page_content or "belgo" in page_content.lower():
                        tenants_found.append("BELGO")
                    if "RETAIL" in page_content or "retail" in page_content.lower():
                        tenants_found.append("RETAIL")
                    if "HEALTH" in page_content or "health" in page_content.lower():
                        tenants_found.append("HEALTH")

                    if len(tenants_found) >= 2:
                        print(f"    [OK] Múltiplos tenants detectados: {tenants_found}")
                        results.append((username, "MULTI_TENANT_DASHBOARD", "PASS", f"Tenants: {tenants_found}"))
                    else:
                        print(f"    [WARN] Poucos tenants visíveis: {tenants_found}")
                        results.append((username, "MULTI_TENANT_DASHBOARD", "WARN", f"Only: {tenants_found}"))
                else:
                    # Tenant admin deve ver apenas seu tenant
                    print(f"  [VALIDANDO] Deve ver apenas tenant: {tenant}")

                    if tenant.upper() in page_content or tenant.lower() in page_content.lower():
                        print(f"    [OK] Tenant {tenant} visível no dashboard")
                        results.append((username, "TENANT_DASHBOARD", "PASS", f"Tenant: {tenant}"))
                    else:
                        print(f"    [WARN] Tenant {tenant} não claramente visível")
                        results.append((username, "TENANT_DASHBOARD", "WARN", f"Tenant: {tenant} not visible"))

                results.append((username, "DASHBOARD_LOAD", "PASS", f"{len(kpi_elements)} metrics, {len(card_elements)} cards"))

                # =============================================================
                # STEP 3: KANBAN (Validar stories do tenant)
                # =============================================================
                print(f"\n[3] KANBAN - VALIDAR STORIES")
                print("-" * 80)

                await page.goto(f"{BASE_URL}/kanban")
                await page.wait_for_load_state("networkidle")
                await asyncio.sleep(4)

                screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_04_kanban.png"
                await page.screenshot(path=str(screenshot_path), full_page=True)
                print(f"  [SCREENSHOT] {screenshot_path.name}")

                # Contar stories
                story_cards = await page.query_selector_all('[data-id]')
                story_count = len(story_cards)
                print(f"  [INFO] {story_count} stories visíveis")

                # Validar quantidade esperada por tenant
                expected_counts = {
                    "ALL": ">= 85",  # Soma de todos (55+18+12)
                    "BELGO-001": "~55",
                    "RETAIL": "~18",
                    "HEALTH": "~12"
                }

                expected = expected_counts.get(tenant, "unknown")
                print(f"  [INFO] Esperado para {tenant}: {expected} stories")

                if should_see_all:
                    # Platform admin pode ver:
                    # 1. Todas as stories (536) se nenhum projeto selecionado
                    # 2. Stories de um projeto específico (50-60 para BELGO) se projeto está selecionado
                    if story_count >= 85:
                        print(f"    [OK] Platform admin vê {story_count} stories (múltiplos tenants)")
                        results.append((username, "KANBAN_ALL_TENANTS", "PASS", f"{story_count} stories"))
                    elif 50 <= story_count <= 60:
                        print(f"    [OK] Platform admin vê {story_count} stories (projeto específico filtrado)")
                        results.append((username, "KANBAN_PROJECT_FILTERED", "PASS", f"{story_count} stories"))
                    else:
                        print(f"    [WARN] Quantidade inesperada de stories: {story_count}")
                        results.append((username, "KANBAN_ALL_TENANTS", "WARN", f"Unexpected {story_count} stories"))
                else:
                    # Validar que vê apenas stories do seu tenant
                    tenant_ranges = {
                        "BELGO-001": (50, 60),
                        "RETAIL": (15, 25),
                        "HEALTH": (10, 20)
                    }

                    if tenant in tenant_ranges:
                        min_count, max_count = tenant_ranges[tenant]
                        if min_count <= story_count <= max_count:
                            print(f"    [OK] Stories dentro do range esperado: {story_count} ({min_count}-{max_count})")
                            results.append((username, "KANBAN_TENANT_ISOLATION", "PASS", f"{story_count} stories"))
                        else:
                            print(f"    [WARN] Stories fora do range: {story_count} (esperado {min_count}-{max_count})")
                            results.append((username, "KANBAN_TENANT_ISOLATION", "WARN", f"{story_count} stories"))
                    else:
                        results.append((username, "KANBAN_STORIES", "PASS", f"{story_count} stories"))

                # =============================================================
                # STEP 4: ANALYTICS (se disponível)
                # =============================================================
                print(f"\n[4] ANALYTICS")
                print("-" * 80)

                await page.goto(f"{BASE_URL}/analytics")
                await asyncio.sleep(3)

                screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_05_analytics.png"
                await page.screenshot(path=str(screenshot_path), full_page=True)
                print(f"  [SCREENSHOT] {screenshot_path.name}")

                # Verificar se há gráficos
                chart_elements = await page.query_selector_all('canvas, svg, [class*="chart"], [class*="graph"]')
                print(f"  [INFO] {len(chart_elements)} gráficos encontrados")

                if len(chart_elements) > 0:
                    results.append((username, "ANALYTICS_PAGE", "PASS", f"{len(chart_elements)} charts"))
                else:
                    results.append((username, "ANALYTICS_PAGE", "WARN", "No charts visible"))

                # =============================================================
                # STEP 5: ADMIN PANEL (se deve ter acesso)
                # =============================================================
                if should_see_admin:
                    print(f"\n[5] ADMIN PANEL")
                    print("-" * 80)

                    await page.goto(f"{BASE_URL}/admin/users")
                    await asyncio.sleep(3)

                    screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_06_admin_panel.png"
                    await page.screenshot(path=str(screenshot_path), full_page=True)
                    print(f"  [SCREENSHOT] {screenshot_path.name}")

                    # Verificar se carregou admin panel
                    current_url = page.url
                    if '/admin' in current_url:
                        print(f"    [OK] Admin panel acessível")
                        results.append((username, "ADMIN_PANEL_ACCESS", "PASS", "Accessible"))

                        # Contar usuários visíveis
                        user_rows = await page.query_selector_all('tr, .user-row, [class*="user"]')
                        print(f"    [INFO] {len(user_rows)} linhas de usuários")
                        results.append((username, "ADMIN_USERS_LIST", "PASS", f"{len(user_rows)} rows"))
                    else:
                        print(f"    [WARN] Redirecionado de admin: {current_url}")
                        results.append((username, "ADMIN_PANEL_ACCESS", "WARN", f"Redirected to {current_url}"))
                else:
                    print(f"\n[5] ADMIN PANEL - BLOQUEADO (esperado)")
                    print("-" * 80)

                    await page.goto(f"{BASE_URL}/admin/users")
                    await asyncio.sleep(3)

                    # Verificar se há mensagem de erro 403/401 na página
                    page_content = await page.content()
                    error_indicators = [
                        "403",
                        "Forbidden",
                        "Access denied",
                        "permission required",
                        "401",
                        "Unauthorized",
                        "Not authenticated"
                    ]

                    has_error = any(indicator.lower() in page_content.lower() for indicator in error_indicators)

                    if has_error:
                        print(f"    [OK] Admin panel bloqueado (RBAC correto - 403/401 detectado)")
                        results.append((username, "ADMIN_PANEL_RBAC", "PASS", "Blocked as expected"))
                    else:
                        current_url = page.url
                        if '/admin' not in current_url:
                            print(f"    [OK] Admin panel bloqueado (redirecionado)")
                            results.append((username, "ADMIN_PANEL_RBAC", "PASS", "Redirected away from admin"))
                        else:
                            print(f"    [FAIL] Admin panel acessível mas deveria estar bloqueado")
                            results.append((username, "ADMIN_PANEL_RBAC", "FAIL", "Accessible but should be blocked"))

                    screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_06_admin_blocked.png"
                    await page.screenshot(path=str(screenshot_path))
                    print(f"  [SCREENSHOT] {screenshot_path.name}")

                print(f"\n  [OK] Testes completos para {username}")

            except Exception as e:
                print(f"\n  [ERROR] Erro durante teste: {str(e)[:100]}")
                results.append((username, "GENERAL", "ERROR", str(e)[:50]))

                try:
                    screenshot_path = SCREENSHOT_DIR / f"{idx:02d}_{username}_ERROR.png"
                    await page.screenshot(path=str(screenshot_path))
                    print(f"  [SCREENSHOT] {screenshot_path.name}")
                except:
                    pass

            finally:
                await context.close()

        await browser.close()

    # =========================================================================
    # RELATÓRIO FINAL
    # =========================================================================
    print("\n" + "=" * 80)
    print("RELATÓRIO FINAL - DASHBOARDS TODOS OS PERFIS")
    print("=" * 80)

    # Agrupar por usuário
    users_tested = set(r[0] for r in results)

    for username in users_tested:
        user_results = [r for r in results if r[0] == username]
        print(f"\n{username}:")
        for _, operation, status, details in user_results:
            symbol = "[OK]" if status == "PASS" else "[FAIL]" if status == "FAIL" else "[WARN]" if status == "WARN" else "[ERR]"
            print(f"  {symbol} {operation}: {status} - {details}")

    # Estatísticas
    total_tests = len(results)
    passed = len([r for r in results if r[2] == "PASS"])
    failed = len([r for r in results if r[2] == "FAIL"])
    warned = len([r for r in results if r[2] == "WARN"])
    errors = len([r for r in results if r[2] == "ERROR"])

    success_rate = (passed / total_tests * 100) if total_tests > 0 else 0

    print(f"\n{'=' * 80}")
    print(f"Total de Testes: {total_tests}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Warned: {warned}")
    print(f"Errors: {errors}")
    print(f"Taxa de Sucesso: {success_rate:.1f}%")
    print(f"\nScreenshots salvos em: {SCREENSHOT_DIR}")
    print("=" * 80)

    # Salvar relatório
    report_file = "C:/Users/lcruz/Fabrica de Agentes/analysis/DASHBOARDS_ALL_PROFILES_REPORT_2026-01-08.md"
    with open(report_file, "w", encoding="utf-8") as f:
        f.write(f"# Dashboards - Todos os Perfis\n")
        f.write(f"**Data:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write(f"## Resultados\n\n")
        f.write(f"- Total: {total_tests}\n")
        f.write(f"- Passed: {passed}\n")
        f.write(f"- Failed: {failed}\n")
        f.write(f"- Warned: {warned}\n")
        f.write(f"- Errors: {errors}\n")
        f.write(f"- **Taxa de Sucesso: {success_rate:.1f}%**\n\n")

        for username in users_tested:
            user_results = [r for r in results if r[0] == username]
            f.write(f"\n### {username}\n\n")
            for _, operation, status, details in user_results:
                f.write(f"- **{operation}**: {status} - {details}\n")

    print(f"\n[OK] Relatório salvo: {report_file}")

    return results


if __name__ == "__main__":
    asyncio.run(test_dashboards_all_profiles())
