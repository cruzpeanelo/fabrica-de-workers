"""
Teste do Chat/Assistente IA
"""
import asyncio
from playwright.async_api import async_playwright
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

BASE_URL = "http://localhost:9001"

async def clear_overlays(page):
    """Limpa overlays"""
    await page.evaluate('''() => {
        document.querySelectorAll('.fixed').forEach(el => {
            const cls = el.className || '';
            if (cls.includes('overlay') || cls.includes('modal') || cls.includes('bg-black')) {
                el.style.display = 'none';
            }
        });
    }''')
    for _ in range(3):
        await page.keyboard.press('Escape')
        await asyncio.sleep(0.1)

async def test_chat():
    """Testa funcionalidade do Chat"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=300)
        context = await browser.new_context(viewport={'width': 1920, 'height': 1080})
        page = await context.new_page()

        print("\n" + "="*50)
        print("  TESTE DO CHAT / ASSISTENTE IA")
        print("="*50)

        await page.goto(BASE_URL)
        await page.wait_for_load_state('networkidle', timeout=15000)
        await clear_overlays(page)

        # 1. Procurar botao/icone do chat
        print("\n[1] Procurando icone do chat...")
        chat_selectors = [
            'button:has-text("Chat")',
            '[class*="chat"]',
            '.chat-button',
            '.chat-icon',
            '[aria-label*="chat"]',
            'button:has-text("Assistente")',
            '.assistant-btn',
            '.ai-chat',
        ]

        chat_btn = None
        for selector in chat_selectors:
            try:
                btn = await page.query_selector(selector)
                if btn and await btn.is_visible():
                    chat_btn = btn
                    print(f"    Chat encontrado: {selector}")
                    break
            except:
                continue

        if not chat_btn:
            print("    [WARN] Botao de chat nao encontrado")
            # Tentar encontrar na sidebar
            sidebar_items = await page.query_selector_all('.sidebar a, .sidebar button, nav a')
            for item in sidebar_items:
                text = await item.text_content()
                if text and ('chat' in text.lower() or 'assist' in text.lower()):
                    chat_btn = item
                    print(f"    Chat na sidebar: {text.strip()}")
                    break

        # 2. Verificar API de chat
        print("\n[2] Testando API de chat...")
        api_result = await page.evaluate('''async () => {
            try {
                const res = await fetch('/api/chat/history');
                return { status: res.status, ok: res.ok };
            } catch(e) {
                return { error: e.message };
            }
        }''')
        print(f"    /api/chat/history: {api_result}")

        # 3. Tentar abrir chat
        if chat_btn:
            print("\n[3] Abrindo chat...")
            try:
                await chat_btn.evaluate('el => el.click()')
                await asyncio.sleep(1)

                # Verificar se painel de chat abriu
                chat_panel = await page.query_selector('.chat-panel, .chat-window, [class*="chat"]')
                if chat_panel and await chat_panel.is_visible():
                    print("    [OK] Painel de chat aberto")
                    await page.screenshot(path='screenshots/test_chat_open.png')

                    # 4. Tentar enviar mensagem
                    print("\n[4] Testando envio de mensagem...")
                    input_field = await page.query_selector('input[placeholder*="mensagem"], textarea[placeholder*="mensagem"], .chat-input')
                    if input_field:
                        await input_field.fill("Ola, isso e um teste automatizado")
                        print("    Mensagem digitada")

                        send_btn = await page.query_selector('button:has-text("Enviar"), button[type="submit"], .send-btn')
                        if send_btn:
                            await send_btn.evaluate('el => el.click()')
                            await asyncio.sleep(2)
                            print("    Mensagem enviada")
                            await page.screenshot(path='screenshots/test_chat_sent.png')
                else:
                    print("    [WARN] Painel de chat nao detectado")
            except Exception as e:
                print(f"    [ERRO] {str(e)[:50]}")
        else:
            print("\n[3] Chat nao disponivel para teste")

        # 5. Verificar WebSocket
        print("\n[5] Verificando WebSocket...")
        ws_result = await page.evaluate('''() => {
            const wsConnections = window.__wsConnections || [];
            return {
                hasWebSocket: typeof WebSocket !== 'undefined',
                activeConnections: wsConnections.length
            };
        }''')
        print(f"    WebSocket disponivel: {ws_result.get('hasWebSocket')}")

        print("\n" + "="*50)
        print("  TESTE DO CHAT FINALIZADO")
        print("="*50)

        print("\nBrowser aberto por 10 segundos...")
        await asyncio.sleep(10)

        await browser.close()

if __name__ == "__main__":
    import os
    os.makedirs('screenshots', exist_ok=True)
    asyncio.run(test_chat())
