# -*- coding: utf-8 -*-
"""
Capturar Erros JavaScript do Console
=====================================
"""
import asyncio

async def capture_console_errors():
    from playwright.async_api import async_playwright

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=200)
        page = await browser.new_page()

        # Capturar TODOS os logs do console
        console_messages = []
        page.on("console", lambda msg: console_messages.append({
            "type": msg.type,
            "text": msg.text,
            "location": msg.location
        }))

        # Capturar erros de página
        page_errors = []
        page.on("pageerror", lambda err: page_errors.append(str(err)))

        try:
            # Login
            await page.goto("http://localhost:9001/login")
            await asyncio.sleep(2)

            await page.fill('input[type="text"]', "platform_admin")
            await page.fill('input[type="password"]', "Platform@2025!Adm")
            await page.click('button[type="submit"]')

            await asyncio.sleep(5)

            # Navegar para Kanban
            await page.goto("http://localhost:9001/kanban")
            await asyncio.sleep(10)  # Aguardar erros aparecerem

            print("=" * 60)
            print("CONSOLE MESSAGES")
            print("=" * 60)
            for msg in console_messages[-20:]:  # Últimos 20
                print(f"[{msg['type'].upper()}] {msg['text']}")
                if msg['location']:
                    print(f"  Location: {msg['location']}")

            print("\n" + "=" * 60)
            print("PAGE ERRORS")
            print("=" * 60)
            for err in page_errors:
                print(f"[ERROR] {err}")

            await asyncio.sleep(5)

        finally:
            await browser.close()


if __name__ == "__main__":
    asyncio.run(capture_console_errors())
