# -*- coding: utf-8 -*-
"""
Teste de Isolamento Multi-Tenant
=================================
Valida que dados de diferentes tenants estão completamente isolados
"""
import asyncio
from pathlib import Path

BASE_URL = "http://localhost:9001"
SCREENSHOT_DIR = Path("C:/Users/lcruz/Fabrica de Agentes/analysis/screenshots/multi_tenant")
SCREENSHOT_DIR.mkdir(parents=True, exist_ok=True)

# Usuários de diferentes tenants
TENANTS = [
    {
        "name": "BELGO",
        "username": "belgo_admin",
        "password": "Belgo@Admin#2025",
        "tenant_id": "BELGO-001"
    },
    {
        "name": "RETAIL",
        "username": "retail_admin",
        "password": "admin123",
        "tenant_id": "RETAIL"
    },
    {
        "name": "HEALTH",
        "username": "health_admin",
        "password": "admin123",
        "tenant_id": "HEALTH"
    }
]

async def test_tenant_isolation():
    from playwright.async_api import async_playwright

    results = []
    tenant_data = {}

    print("=" * 70)
    print("TESTE DE ISOLAMENTO MULTI-TENANT")
    print("=" * 70)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=500)

        for tenant in TENANTS:
            print(f"\n[TENANT: {tenant['name']}]")
            print("-" * 70)

            context = await browser.new_context(viewport={"width": 1920, "height": 1080})
            page = await context.new_page()

            try:
                # 1. LOGIN
                print(f"  [1] Login como {tenant['username']}...")
                await page.goto(f"{BASE_URL}/login")
                await asyncio.sleep(2)

                await page.fill('input[type="text"]', tenant['username'])
                await page.fill('input[type="password"]', tenant['password'])
                await page.click('button[type="submit"]')
                await asyncio.sleep(5)

                # Verificar login
                token = await page.evaluate("() => localStorage.getItem('auth_token')")
                if not token:
                    print(f"    [ERROR] Login falhou para {tenant['username']}")
                    results.append((f"{tenant['name']} - Login", "FAIL", "Sem token"))
                    await context.close()
                    continue

                print(f"    [OK] Login bem-sucedido")

                # Screenshot login
                screenshot_path = SCREENSHOT_DIR / f"{tenant['name']}_01_login.png"
                await page.screenshot(path=str(screenshot_path))

                # 2. NAVEGAR PARA KANBAN
                print(f"  [2] Navegando para Kanban...")
                await page.goto(f"{BASE_URL}/kanban")
                await asyncio.sleep(3)

                # Aguardar Vue montar
                await asyncio.sleep(2)

                # 3. CONTAR STORIES
                story_cards = await page.query_selector_all('.story-card')
                story_count = len(story_cards)
                print(f"    [INFO] Stories encontrados: {story_count}")

                # Screenshot kanban
                screenshot_path = SCREENSHOT_DIR / f"{tenant['name']}_02_kanban.png"
                await page.screenshot(path=str(screenshot_path))

                # 4. COLETAR STORY IDs
                story_ids = []
                for card in story_cards[:10]:  # Primeiros 10
                    story_id = await card.get_attribute('data-id')
                    if story_id and story_id != 'story.story_id':  # Ignorar template
                        # Pegar texto do card
                        text = await card.inner_text()
                        first_line = text.split('\n')[0] if '\n' in text else text
                        story_ids.append(first_line[:50])

                tenant_data[tenant['name']] = {
                    'count': story_count,
                    'story_ids': story_ids,
                    'username': tenant['username']
                }

                print(f"    [INFO] Story IDs (amostra): {story_ids[:3]}")

                # 5. NAVEGAR PARA STORIES LIST
                print(f"  [3] Navegando para Stories List...")
                await page.goto(f"{BASE_URL}/stories")
                await asyncio.sleep(3)

                # Contar linhas na tabela
                table_rows = await page.query_selector_all('table tbody tr')
                table_count = len(table_rows)
                print(f"    [INFO] Stories na tabela: {table_count}")

                # Screenshot stories list
                screenshot_path = SCREENSHOT_DIR / f"{tenant['name']}_03_stories.png"
                await page.screenshot(path=str(screenshot_path))

                # 6. VERIFICAR CONSISTÊNCIA
                if story_count > 0 and table_count > 0:
                    # Aceitar pequenas diferenças devido a filtros
                    diff = abs(story_count - table_count)
                    if diff <= 5:  # Tolerância de 5 stories
                        print(f"    [OK] Contagens consistentes (diff: {diff})")
                        results.append((f"{tenant['name']} - Consistência", "PASS", f"{story_count} stories"))
                    else:
                        print(f"    [WARN] Contagens diferentes: Kanban={story_count}, Tabela={table_count}")
                        results.append((f"{tenant['name']} - Consistência", "WARN", f"Diff: {diff}"))
                else:
                    print(f"    [OK] Tenant sem dados (normal para novos tenants)")
                    results.append((f"{tenant['name']} - Dados", "PASS", "Sem dados"))

                results.append((f"{tenant['name']} - Login", "PASS", f"{story_count} stories"))

            except Exception as e:
                print(f"    [ERROR] {str(e)[:100]}")
                results.append((f"{tenant['name']}", "FAIL", str(e)[:50]))

            finally:
                await context.close()

        await browser.close()

    # ========================================
    # ANÁLISE DE ISOLAMENTO
    # ========================================
    print("\n" + "=" * 70)
    print("ANÁLISE DE ISOLAMENTO MULTI-TENANT")
    print("=" * 70)

    for tenant_name, data in tenant_data.items():
        print(f"\n{tenant_name}:")
        print(f"  Username: {data['username']}")
        print(f"  Stories: {data['count']}")
        print(f"  Amostra IDs: {data['story_ids'][:3]}")

    # Verificar isolamento
    print("\n" + "-" * 70)
    print("VERIFICAÇÃO DE ISOLAMENTO:")
    print("-" * 70)

    tenant_names = list(tenant_data.keys())
    isolation_ok = True

    for i, tenant1 in enumerate(tenant_names):
        for tenant2 in tenant_names[i+1:]:
            data1 = tenant_data[tenant1]
            data2 = tenant_data[tenant2]

            # Verificar se há IDs em comum
            common_ids = set(data1['story_ids']) & set(data2['story_ids'])

            if common_ids:
                print(f"[FAIL] VAZAMENTO! {tenant1} e {tenant2} têm IDs em comum: {common_ids}")
                isolation_ok = False
                results.append(("Isolamento", "FAIL", f"Vazamento entre {tenant1} e {tenant2}"))
            else:
                print(f"[OK] OK - {tenant1} e {tenant2} isolados (sem IDs em comum)")

    if isolation_ok:
        print("\n[OK] ISOLAMENTO MULTI-TENANT: OK")
        results.append(("Isolamento Multi-Tenant", "PASS", "Todos os tenants isolados"))
    else:
        print("\n[FAIL] ISOLAMENTO MULTI-TENANT: FALHOU")

    # ========================================
    # RELATÓRIO FINAL
    # ========================================
    print("\n" + "=" * 70)
    print("RELATÓRIO FINAL")
    print("=" * 70)

    passed = sum(1 for _, status, _ in results if status == "PASS")
    failed = sum(1 for _, status, _ in results if status == "FAIL")
    warned = sum(1 for _, status, _ in results if status == "WARN")
    total = len(results)

    print(f"\nTotal de Testes: {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Warned: {warned}")
    print(f"Taxa de Sucesso: {(passed/total)*100:.1f}%")

    print("\nDetalhes:")
    for test, status, detail in results:
        symbol = "[OK]" if status == "PASS" else "[FAIL]" if status == "FAIL" else "[WARN]"
        print(f"  {symbol} {test}: {detail}")

    print(f"\nScreenshots salvos em: {SCREENSHOT_DIR}")
    print("=" * 70)

if __name__ == "__main__":
    asyncio.run(test_tenant_isolation())
