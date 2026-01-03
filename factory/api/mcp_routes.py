"""
MCP Routes - API REST para gerenciamento de servidores MCP.

Endpoints:
- GET  /api/mcp/servers       - Listar servidores MCP
- POST /api/mcp/start         - Iniciar servidor MCP
- POST /api/mcp/stop          - Parar servidor MCP
- GET  /api/mcp/status/{name} - Status de um servidor
- POST /api/mcp/register      - Registrar novo servidor
- DELETE /api/mcp/{name}      - Remover servidor
"""

from flask import Blueprint, request, jsonify
from typing import Optional
import sys
import os
from dataclasses import asdict

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.mcp.mcp_manager import (
    get_mcp_manager, MCPServerConfig
)

mcp_bp = Blueprint('mcp', __name__, url_prefix='/api/mcp')


@mcp_bp.route('/servers', methods=['GET'])
def list_servers():
    """Lista todos os servidores MCP configurados."""
    manager = get_mcp_manager()

    servers = []
    for name, config in manager.servers.items():
        status = manager.get_status(name)
        servers.append({
            "name": config.name,
            "command": config.command,
            "args": config.args,
            "enabled": config.enabled,
            "auto_start": config.auto_start,
            "description": config.description,
            "running": status.running,
            "pid": status.pid
        })

    return jsonify({
        "success": True,
        "data": servers
    })


@mcp_bp.route('/start', methods=['POST'])
def start_server():
    """Inicia um servidor MCP."""
    data = request.get_json() or {}
    name = data.get("name")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    manager = get_mcp_manager()

    if name not in manager.servers:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' nao encontrado"
        }), 404

    success = manager.start_server(name)

    if success:
        status = manager.get_status(name)
        return jsonify({
            "success": True,
            "message": f"Servidor '{name}' iniciado",
            "data": {
                "name": name,
                "pid": status.pid,
                "running": status.running
            }
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Falha ao iniciar servidor '{name}'"
        }), 500


@mcp_bp.route('/stop', methods=['POST'])
def stop_server():
    """Para um servidor MCP."""
    data = request.get_json() or {}
    name = data.get("name")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    manager = get_mcp_manager()

    if name not in manager.servers:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' nao encontrado"
        }), 404

    success = manager.stop_server(name)

    if success:
        return jsonify({
            "success": True,
            "message": f"Servidor '{name}' parado"
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Falha ao parar servidor '{name}'"
        }), 500


@mcp_bp.route('/status/<name>', methods=['GET'])
def get_server_status(name: str):
    """Retorna status de um servidor MCP."""
    manager = get_mcp_manager()

    if name not in manager.servers:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' nao encontrado"
        }), 404

    status = manager.get_status(name)
    config = manager.servers[name]

    return jsonify({
        "success": True,
        "data": {
            "name": name,
            "running": status.running,
            "pid": status.pid,
            "error": status.error,
            "config": {
                "command": config.command,
                "args": config.args,
                "env": config.env,
                "enabled": config.enabled,
                "auto_start": config.auto_start,
                "description": config.description
            }
        }
    })


@mcp_bp.route('/register', methods=['POST'])
def register_server():
    """Registra um novo servidor MCP."""
    data = request.get_json() or {}

    name = data.get("name")
    command = data.get("command")
    args = data.get("args", [])
    env = data.get("env", {})
    cwd = data.get("cwd")
    auto_start = data.get("auto_start", False)
    enabled = data.get("enabled", True)
    description = data.get("description", "")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    if not command:
        return jsonify({"success": False, "error": "command e obrigatorio"}), 400

    config = MCPServerConfig(
        name=name,
        command=command,
        args=args,
        env=env,
        cwd=cwd,
        auto_start=auto_start,
        enabled=enabled,
        description=description
    )

    manager = get_mcp_manager()
    success = manager.register_server(config)

    if success:
        return jsonify({
            "success": True,
            "message": f"Servidor '{name}' registrado",
            "data": asdict(config)
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' ja existe"
        }), 400


@mcp_bp.route('/<name>', methods=['DELETE'])
def unregister_server(name: str):
    """Remove um servidor MCP."""
    manager = get_mcp_manager()

    if name not in manager.servers:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' nao encontrado"
        }), 404

    success = manager.unregister_server(name)

    if success:
        return jsonify({
            "success": True,
            "message": f"Servidor '{name}' removido"
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Falha ao remover servidor '{name}'"
        }), 500


@mcp_bp.route('/restart', methods=['POST'])
def restart_server():
    """Reinicia um servidor MCP."""
    data = request.get_json() or {}
    name = data.get("name")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    manager = get_mcp_manager()

    if name not in manager.servers:
        return jsonify({
            "success": False,
            "error": f"Servidor '{name}' nao encontrado"
        }), 404

    success = manager.restart_server(name)

    if success:
        status = manager.get_status(name)
        return jsonify({
            "success": True,
            "message": f"Servidor '{name}' reiniciado",
            "data": {
                "name": name,
                "pid": status.pid,
                "running": status.running
            }
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Falha ao reiniciar servidor '{name}'"
        }), 500


def register_mcp_routes(app):
    """Registra as rotas de MCP no app Flask."""
    app.register_blueprint(mcp_bp)
