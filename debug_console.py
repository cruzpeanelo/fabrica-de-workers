# -*- coding: utf-8 -*-
"""Debug console errors"""
import asyncio
from playwright.async_api import async_playwright

async def debug():
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()

        errors = []
        warnings = []
        logs = []

        page.on("console", lambda msg: (
            errors.append(msg.text) if msg.type == "error" else
            warnings.append(msg.text) if msg.type == "warning" else
            logs.append(msg.text) if msg.type == "log" else None
        ))

        page.on("pageerror", lambda err: errors.append(f"PAGE ERROR: {err}"))

        print("Navegando para http://localhost:9001/")
        await page.goto("http://localhost:9001/")
        await asyncio.sleep(5)

        print("\n=== ERRORS ===")
        for e in errors:
            print(f"  {e[:200]}")

        print("\n=== WARNINGS ===")
        for w in warnings[:10]:
            print(f"  {w[:200]}")

        print("\n=== LOGS ===")
        for l in logs[:20]:
            print(f"  {l[:100]}")

        # Check if Vue loaded
        vue_check = await page.evaluate("typeof Vue !== 'undefined'")
        print(f"\n=== Vue loaded: {vue_check} ===")

        # Check if app mounted
        app_check = await page.evaluate("document.querySelector('#app').__vue_app__ !== undefined")
        print(f"=== App mounted: {app_check} ===")

        print("\nBrowser aberto por 30s para inspeção...")
        await asyncio.sleep(30)
        await browser.close()

if __name__ == "__main__":
    asyncio.run(debug())
