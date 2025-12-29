# -*- coding: utf-8 -*-
"""
MCP (Model Context Protocol) Server para a Fabrica de Agentes v4.0

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
                "args": ["-m", "factory.mcp.server"]
            }
        }
    }
"""

from .server import MCPServer, run_server
from .tools import (
    StoryTools,
    TaskTools,
    WorkerTools,
    ProjectTools,
    SystemTools,
)

__all__ = [
    "MCPServer",
    "run_server",
    "StoryTools",
    "TaskTools",
    "WorkerTools",
    "ProjectTools",
    "SystemTools",
]

__version__ = "1.0.0"
