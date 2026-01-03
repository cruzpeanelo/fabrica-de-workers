#!/usr/bin/env python3
"""
Playwright MCP Server - Servidor MCP para automacao de browser.

Este servidor expoe ferramentas do Playwright para o Claude Code:
- browser_navigate: Navega para uma URL
- browser_click: Clica em um elemento
- browser_fill: Preenche um campo
- browser_screenshot: Tira screenshot
- browser_get_text: Obtem texto de um elemento
- browser_wait: Aguarda elemento aparecer
- e2e_test: Executa um arquivo de teste

Uso:
    python server.py [--headless] [--port 8765]
"""

import os
import sys
import json
import asyncio
import argparse
from pathlib import Path
from typing import Optional, Dict, Any, List
from datetime import datetime

# Tentar importar playwright
try:
    from playwright.async_api import async_playwright, Browser, Page, BrowserContext
    PLAYWRIGHT_AVAILABLE = True
except ImportError:
    PLAYWRIGHT_AVAILABLE = False
    print("[PlaywrightMCP] Playwright nao instalado. Execute: pip install playwright && playwright install")


class PlaywrightMCPServer:
    """Servidor MCP para Playwright."""

    def __init__(self, headless: bool = True):
        """
        Inicializa o servidor.

        Args:
            headless: Executar browser sem interface grafica
        """
        self.headless = headless
        self.playwright = None
        self.browser: Optional[Browser] = None
        self.context: Optional[BrowserContext] = None
        self.page: Optional[Page] = None
        self.screenshots_path = Path("factory/state/screenshots")
        self.screenshots_path.mkdir(parents=True, exist_ok=True)

    async def start(self):
        """Inicia o browser."""
        if not PLAYWRIGHT_AVAILABLE:
            raise RuntimeError("Playwright nao esta instalado")

        self.playwright = await async_playwright().start()
        self.browser = await self.playwright.chromium.launch(headless=self.headless)
        self.context = await self.browser.new_context()
        self.page = await self.context.new_page()
        print(f"[PlaywrightMCP] Browser iniciado (headless={self.headless})")

    async def stop(self):
        """Para o browser."""
        if self.page:
            await self.page.close()
        if self.context:
            await self.context.close()
        if self.browser:
            await self.browser.close()
        if self.playwright:
            await self.playwright.stop()
        print("[PlaywrightMCP] Browser encerrado")

    async def navigate(self, url: str) -> Dict[str, Any]:
        """
        Navega para uma URL.

        Args:
            url: URL para navegar

        Returns:
            Resultado da operacao
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            response = await self.page.goto(url, wait_until="networkidle")
            return {
                "success": True,
                "url": self.page.url,
                "title": await self.page.title(),
                "status": response.status if response else None
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def click(self, selector: str) -> Dict[str, Any]:
        """
        Clica em um elemento.

        Args:
            selector: Seletor CSS do elemento

        Returns:
            Resultado da operacao
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            await self.page.click(selector)
            return {"success": True, "selector": selector}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def fill(self, selector: str, value: str) -> Dict[str, Any]:
        """
        Preenche um campo.

        Args:
            selector: Seletor CSS do campo
            value: Valor a preencher

        Returns:
            Resultado da operacao
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            await self.page.fill(selector, value)
            return {"success": True, "selector": selector, "value": value}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def screenshot(self, name: Optional[str] = None, full_page: bool = False) -> Dict[str, Any]:
        """
        Tira screenshot da pagina.

        Args:
            name: Nome do arquivo (opcional)
            full_page: Capturar pagina inteira

        Returns:
            Resultado com caminho do arquivo
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            if not name:
                name = f"screenshot_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

            path = self.screenshots_path / f"{name}.png"
            await self.page.screenshot(path=str(path), full_page=full_page)
            return {"success": True, "path": str(path)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def get_text(self, selector: str) -> Dict[str, Any]:
        """
        Obtem texto de um elemento.

        Args:
            selector: Seletor CSS do elemento

        Returns:
            Texto do elemento
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            text = await self.page.text_content(selector)
            return {"success": True, "selector": selector, "text": text}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def wait_for(self, selector: str, timeout: int = 30000) -> Dict[str, Any]:
        """
        Aguarda um elemento aparecer.

        Args:
            selector: Seletor CSS do elemento
            timeout: Timeout em ms

        Returns:
            Resultado da operacao
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            await self.page.wait_for_selector(selector, timeout=timeout)
            return {"success": True, "selector": selector}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def evaluate(self, script: str) -> Dict[str, Any]:
        """
        Executa JavaScript na pagina.

        Args:
            script: Codigo JavaScript

        Returns:
            Resultado da execucao
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            result = await self.page.evaluate(script)
            return {"success": True, "result": result}
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def get_page_info(self) -> Dict[str, Any]:
        """
        Retorna informacoes da pagina atual.

        Returns:
            Informacoes da pagina
        """
        if not self.page:
            return {"success": False, "error": "Browser nao iniciado"}

        try:
            return {
                "success": True,
                "url": self.page.url,
                "title": await self.page.title(),
                "viewport": self.page.viewport_size
            }
        except Exception as e:
            return {"success": False, "error": str(e)}


# Funcoes para uso como MCP tools
_server: Optional[PlaywrightMCPServer] = None


async def get_server() -> PlaywrightMCPServer:
    """Retorna instancia do servidor."""
    global _server
    if _server is None:
        headless = os.environ.get("HEADLESS", "true").lower() == "true"
        _server = PlaywrightMCPServer(headless=headless)
        await _server.start()
    return _server


async def browser_navigate(url: str) -> Dict[str, Any]:
    """MCP Tool: Navega para URL."""
    server = await get_server()
    return await server.navigate(url)


async def browser_click(selector: str) -> Dict[str, Any]:
    """MCP Tool: Clica em elemento."""
    server = await get_server()
    return await server.click(selector)


async def browser_fill(selector: str, value: str) -> Dict[str, Any]:
    """MCP Tool: Preenche campo."""
    server = await get_server()
    return await server.fill(selector, value)


async def browser_screenshot(name: str = None, full_page: bool = False) -> Dict[str, Any]:
    """MCP Tool: Tira screenshot."""
    server = await get_server()
    return await server.screenshot(name, full_page)


async def browser_get_text(selector: str) -> Dict[str, Any]:
    """MCP Tool: Obtem texto."""
    server = await get_server()
    return await server.get_text(selector)


async def browser_wait(selector: str, timeout: int = 30000) -> Dict[str, Any]:
    """MCP Tool: Aguarda elemento."""
    server = await get_server()
    return await server.wait_for(selector, timeout)


async def main():
    """Funcao principal para teste."""
    parser = argparse.ArgumentParser(description="Playwright MCP Server")
    parser.add_argument("--headless", action="store_true", default=True, help="Modo headless")
    parser.add_argument("--port", type=int, default=8765, help="Porta do servidor")
    args = parser.parse_args()

    server = PlaywrightMCPServer(headless=args.headless)

    try:
        await server.start()
        print("[PlaywrightMCP] Servidor pronto. Aguardando comandos...")

        # Exemplo de uso
        result = await server.navigate("https://example.com")
        print(f"Navigate: {result}")

        result = await server.screenshot("test")
        print(f"Screenshot: {result}")

        # Manter rodando
        while True:
            await asyncio.sleep(1)

    except KeyboardInterrupt:
        print("\n[PlaywrightMCP] Encerrando...")
    finally:
        await server.stop()


if __name__ == "__main__":
    asyncio.run(main())
