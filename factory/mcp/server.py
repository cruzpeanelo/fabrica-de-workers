# -*- coding: utf-8 -*-
"""
MCP Server - Servidor MCP para a Plataforma E

Este modulo implementa um servidor MCP (Model Context Protocol) que permite
que Claude Desktop e outros clientes MCP interajam com a Plataforma E.

O servidor expoe ferramentas para:
- Gerenciar User Stories e Tasks
- Criar e monitorar Jobs
- Consultar projetos e metricas
- Executar operacoes no sistema

Uso:
    python -m factory.mcp.server

Ou via Claude Desktop com configuracao em claude_desktop_config.json
"""

import asyncio
import json
import sys
import os
from typing import Any, Dict, List, Optional, Sequence
from datetime import datetime
import logging

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from dotenv import load_dotenv
load_dotenv()

# Importar tools
from factory.mcp.tools import (
    get_all_tools,
    get_tool_handlers,
    ToolDefinition,
    StoryTools,
    TaskTools,
    WorkerTools,
    ProjectTools,
    SystemTools,
)

# Configurar logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("mcp-server")


# =============================================================================
# MCP PROTOCOL IMPLEMENTATION
# =============================================================================

class MCPServer:
    """
    Servidor MCP para a Plataforma E

    Implementa o protocolo MCP (Model Context Protocol) para comunicacao
    com Claude Desktop e outros clientes compatíveis.

    O servidor roda via stdio (stdin/stdout) e responde a mensagens JSON-RPC.
    """

    def __init__(self):
        self.tools = get_all_tools()
        self.handlers = get_tool_handlers()
        self.server_info = {
            "name": "fabrica-agentes-mcp",
            "version": "1.0.0",
            "description": "MCP Server para a Plataforma E - Sistema de Desenvolvimento Autonomo"
        }
        self._running = False

    def get_tools_list(self) -> List[Dict]:
        """Retorna lista de tools no formato MCP"""
        return [tool.to_mcp_schema() for tool in self.tools]

    async def handle_tool_call(self, tool_name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """
        Executa uma ferramenta MCP

        Args:
            tool_name: Nome da ferramenta
            arguments: Argumentos da ferramenta

        Returns:
            Resultado da execucao
        """
        if tool_name not in self.handlers:
            return {
                "error": f"Ferramenta '{tool_name}' nao encontrada",
                "available_tools": [t.name for t in self.tools]
            }

        handler = self.handlers[tool_name]

        try:
            result = await handler(**arguments)
            return result
        except Exception as e:
            logger.error(f"Erro ao executar tool {tool_name}: {e}")
            return {
                "error": str(e),
                "tool": tool_name,
                "arguments": arguments
            }

    async def handle_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """
        Processa uma mensagem JSON-RPC

        Args:
            message: Mensagem recebida

        Returns:
            Resposta JSON-RPC
        """
        method = message.get("method", "")
        params = message.get("params", {})
        msg_id = message.get("id")

        logger.info(f"Recebido: {method}")

        # Initialize
        if method == "initialize":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {
                        "tools": {},
                        "resources": {},
                        "prompts": {}
                    },
                    "serverInfo": self.server_info
                }
            }

        # Initialized notification
        elif method == "notifications/initialized":
            logger.info("Cliente inicializado com sucesso")
            return None  # Notificacoes nao requerem resposta

        # List tools
        elif method == "tools/list":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "tools": self.get_tools_list()
                }
            }

        # Call tool
        elif method == "tools/call":
            tool_name = params.get("name")
            arguments = params.get("arguments", {})

            result = await self.handle_tool_call(tool_name, arguments)

            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "content": [
                        {
                            "type": "text",
                            "text": json.dumps(result, indent=2, ensure_ascii=False)
                        }
                    ]
                }
            }

        # List resources (nao implementado ainda)
        elif method == "resources/list":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "resources": []
                }
            }

        # List prompts (nao implementado ainda)
        elif method == "prompts/list":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "prompts": []
                }
            }

        # Ping
        elif method == "ping":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {}
            }

        # Metodo desconhecido
        else:
            logger.warning(f"Metodo desconhecido: {method}")
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "error": {
                    "code": -32601,
                    "message": f"Metodo nao encontrado: {method}"
                }
            }

    async def run_stdio(self):
        """
        Executa o servidor via stdio (stdin/stdout)

        Este e o modo padrao para integracao com Claude Desktop.
        """
        self._running = True
        logger.info("Servidor MCP iniciado (modo stdio)")

        # Usar asyncio para ler stdin de forma nao-bloqueante
        loop = asyncio.get_event_loop()
        reader = asyncio.StreamReader()
        protocol = asyncio.StreamReaderProtocol(reader)

        await loop.connect_read_pipe(lambda: protocol, sys.stdin)

        while self._running:
            try:
                # Ler linha do stdin
                line = await reader.readline()

                if not line:
                    break

                line = line.decode('utf-8').strip()

                if not line:
                    continue

                # Parse JSON
                try:
                    message = json.loads(line)
                except json.JSONDecodeError as e:
                    logger.error(f"Erro ao parsear JSON: {e}")
                    continue

                # Processar mensagem
                response = await self.handle_message(message)

                # Enviar resposta
                if response is not None:
                    response_str = json.dumps(response, ensure_ascii=False) + "\n"
                    sys.stdout.write(response_str)
                    sys.stdout.flush()

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Erro no loop principal: {e}")

        logger.info("Servidor MCP encerrado")

    def stop(self):
        """Para o servidor"""
        self._running = False


# =============================================================================
# MCP CLIENT (para testes e integracao com workers)
# =============================================================================

class MCPClient:
    """
    Cliente MCP para uso interno

    Permite que workers e outros componentes da fabrica
    utilizem as ferramentas MCP de forma programatica.
    """

    def __init__(self):
        self.server = MCPServer()

    async def call_tool(self, tool_name: str, **kwargs) -> Dict[str, Any]:
        """
        Chama uma ferramenta MCP

        Args:
            tool_name: Nome da ferramenta
            **kwargs: Argumentos da ferramenta

        Returns:
            Resultado da execucao
        """
        return await self.server.handle_tool_call(tool_name, kwargs)

    def list_tools(self) -> List[str]:
        """Lista nomes das ferramentas disponiveis"""
        return [tool.name for tool in self.server.tools]

    def get_tool_schema(self, tool_name: str) -> Optional[Dict]:
        """Retorna schema de uma ferramenta"""
        for tool in self.server.tools:
            if tool.name == tool_name:
                return tool.to_mcp_schema()
        return None


# =============================================================================
# CLI
# =============================================================================

def run_server():
    """Funcao principal para iniciar o servidor"""
    server = MCPServer()

    try:
        asyncio.run(server.run_stdio())
    except KeyboardInterrupt:
        logger.info("Servidor interrompido pelo usuario")
    except Exception as e:
        logger.error(f"Erro fatal: {e}")
        sys.exit(1)


async def test_tools():
    """Testa as ferramentas MCP"""
    client = MCPClient()

    print("\n=== Ferramentas MCP Disponiveis ===\n")
    for tool_name in client.list_tools():
        schema = client.get_tool_schema(tool_name)
        print(f"- {tool_name}")
        print(f"  {schema.get('description', '')}\n")

    print("\n=== Testando system_health ===\n")
    result = await client.call_tool("system_health")
    print(json.dumps(result, indent=2, ensure_ascii=False))

    print("\n=== Testando get_metrics ===\n")
    result = await client.call_tool("get_metrics")
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="MCP Server para Plataforma E")
    parser.add_argument("--test", action="store_true", help="Testar ferramentas")
    parser.add_argument("--list-tools", action="store_true", help="Listar ferramentas")

    args = parser.parse_args()

    if args.test:
        asyncio.run(test_tools())
    elif args.list_tools:
        server = MCPServer()
        print("\n=== Ferramentas MCP Disponiveis ===\n")
        for tool in server.tools:
            print(f"[{tool.category.value}] {tool.name}")
            print(f"    {tool.description}\n")
    else:
        run_server()
