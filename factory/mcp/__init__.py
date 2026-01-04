# -*- coding: utf-8 -*-
"""
MCP (Model Context Protocol) Server para a Plataforma E v4.0

Este modulo implementa um servidor MCP que expoe ferramentas da fabrica
para uso com Claude Desktop e outros clientes MCP.

Ferramentas disponiveis:
- Gerenciamento de Stories (criar, listar, atualizar, mover)
- Gerenciamento de Tasks (criar, listar, atualizar)
- Execucao de Workers (criar jobs, monitorar status)
- Consulta ao sistema (projetos, metricas, logs)

Uso com Claude Desktop:
    Adicionar ao arquivo claude_desktop_config.json:
    {
        "mcpServers": {
            "fabrica-agentes": {
                "command": "python",
                "args": ["-m", "factory.mcp"]
            }
        }
    }

Uso Programatico:
    from factory.mcp.client import MCPToolsClient, get_claude_tools

    # Cliente async
    client = MCPToolsClient()
    result = await client.create_story(project_id="PROJ-001", title="...")

    # Com Claude API
    tools = get_claude_tools()
    response = anthropic.messages.create(..., tools=tools)
"""

from .server import MCPServer, run_server, MCPClient
from .tools import (
    StoryTools,
    TaskTools,
    WorkerTools,
    ProjectTools,
    SystemTools,
    get_all_tools,
    get_tool_handlers,
)
from .client import (
    MCPToolsClient,
    SyncMCPClient,
    ClaudeToolsConversation,
    get_claude_tools,
    get_claude_tools_by_category,
    execute_tool_call,
    format_tool_result_for_claude,
)

__all__ = [
    # Servidor
    "MCPServer",
    "MCPClient",
    "run_server",
    # Tools
    "StoryTools",
    "TaskTools",
    "WorkerTools",
    "ProjectTools",
    "SystemTools",
    "get_all_tools",
    "get_tool_handlers",
    # Clientes
    "MCPToolsClient",
    "SyncMCPClient",
    "ClaudeToolsConversation",
    # Claude API
    "get_claude_tools",
    "get_claude_tools_by_category",
    "execute_tool_call",
    "format_tool_result_for_claude",
]

__version__ = "1.0.0"
