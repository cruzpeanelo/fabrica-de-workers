"""
Orchestrator Routes - API REST para controle do Orquestrador.

Endpoints:
- GET  /api/orchestrator/status     - Status do orquestrador
- POST /api/orchestrator/start      - Iniciar orquestrador
- POST /api/orchestrator/stop       - Parar orquestrador
- GET  /api/orchestrator/agents     - Listar agentes ativos
- POST /api/orchestrator/spawn      - Spawnar agente
- POST /api/orchestrator/terminate  - Terminar agente
- GET  /api/orchestrator/mode       - Modo atual
- PUT  /api/orchestrator/mode       - Alterar modo
"""

from flask import Blueprint, request, jsonify
from typing import Optional
import sys
import os

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.core.terminal_spawner import get_spawner, AgentType

orchestrator_bp = Blueprint('orchestrator', __name__, url_prefix='/api/orchestrator')

# Estado global do orquestrador
_orchestrator_state = {
    "running": False,
    "mode": "supervised",
    "started_at": None,
}


@orchestrator_bp.route('/status', methods=['GET'])
def get_status():
    """Retorna status do orquestrador."""
    spawner = get_spawner()

    return jsonify({
        "success": True,
        "data": {
            "running": _orchestrator_state["running"],
            "mode": _orchestrator_state["mode"],
            "started_at": _orchestrator_state["started_at"],
            "active_agents": len(spawner.terminals),
            "agents": list(spawner.terminals.keys())
        }
    })


@orchestrator_bp.route('/start', methods=['POST'])
def start_orchestrator():
    """Inicia o orquestrador."""
    from datetime import datetime

    data = request.get_json() or {}
    mode = data.get("mode", "supervised")

    if mode not in ["autonomous", "supervised", "interactive"]:
        return jsonify({
            "success": False,
            "error": "Modo invalido. Use: autonomous, supervised, interactive"
        }), 400

    _orchestrator_state["running"] = True
    _orchestrator_state["mode"] = mode
    _orchestrator_state["started_at"] = datetime.now().isoformat()

    return jsonify({
        "success": True,
        "message": f"Orquestrador iniciado em modo {mode}",
        "data": _orchestrator_state
    })


@orchestrator_bp.route('/stop', methods=['POST'])
def stop_orchestrator():
    """Para o orquestrador e todos os agentes."""
    spawner = get_spawner()

    # Terminar todos os agentes
    for agent_type in list(spawner.terminals.keys()):
        spawner.terminate_terminal(agent_type)

    _orchestrator_state["running"] = False
    _orchestrator_state["started_at"] = None

    return jsonify({
        "success": True,
        "message": "Orquestrador parado"
    })


@orchestrator_bp.route('/agents', methods=['GET'])
def list_agents():
    """Lista todos os agentes ativos."""
    spawner = get_spawner()

    agents = []
    for agent_type, info in spawner.terminals.items():
        agents.append({
            "type": agent_type,
            "pid": info.pid,
            "started_at": info.started_at
        })

    # Lista de todos os tipos de agentes
    all_agent_types = [
        {"type": "ORCH", "name": "Orquestrador", "description": "Coordena todos os agentes"},
        {"type": "ARCH", "name": "Arquiteto", "description": "Design de sistemas"},
        {"type": "BACK", "name": "Backend", "description": "APIs e logica de negocio"},
        {"type": "FRONT", "name": "Frontend", "description": "Interface do usuario"},
        {"type": "DEVOPS", "name": "DevOps", "description": "Infraestrutura e CI/CD"},
        {"type": "SEC", "name": "Seguranca", "description": "Auditoria e seguranca"},
        {"type": "QA", "name": "Quality", "description": "Testes e qualidade"},
        {"type": "PROD", "name": "Produto", "description": "Gestao de produto"},
        {"type": "INOV", "name": "Inovacao", "description": "Novas tecnologias"},
        {"type": "FIN", "name": "Financeiro", "description": "Metricas financeiras"},
        {"type": "GROWTH", "name": "Growth", "description": "Crescimento e metricas"},
    ]

    return jsonify({
        "success": True,
        "data": {
            "active": agents,
            "available": all_agent_types
        }
    })


@orchestrator_bp.route('/spawn', methods=['POST'])
def spawn_agent():
    """Spawna um novo agente."""
    data = request.get_json() or {}
    agent_type = data.get("agent_type", "").upper()
    task_description = data.get("task", "")

    if not agent_type:
        return jsonify({
            "success": False,
            "error": "agent_type e obrigatorio"
        }), 400

    # Validar tipo de agente
    valid_types = ["ORCH", "ARCH", "BACK", "FRONT", "DEVOPS", "SEC", "QA", "PROD", "INOV", "FIN", "GROWTH"]
    if agent_type not in valid_types:
        return jsonify({
            "success": False,
            "error": f"Tipo invalido. Use: {', '.join(valid_types)}"
        }), 400

    spawner = get_spawner()

    # Criar task message
    from factory.core.terminal_spawner import TaskMessage
    from datetime import datetime

    task = TaskMessage(
        task_id=f"TASK-{datetime.now().strftime('%Y%m%d%H%M%S')}",
        agent_type=agent_type,
        description=task_description or f"Task para {agent_type}",
        context={"source": "dashboard", "mode": _orchestrator_state["mode"]}
    )

    info = spawner.spawn_terminal(agent_type, task)

    if info:
        return jsonify({
            "success": True,
            "message": f"Agente {agent_type} spawneado",
            "data": {
                "type": agent_type,
                "pid": info.pid,
                "started_at": info.started_at
            }
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Falha ao spawnar agente {agent_type}"
        }), 500


@orchestrator_bp.route('/terminate', methods=['POST'])
def terminate_agent():
    """Termina um agente."""
    data = request.get_json() or {}
    agent_type = data.get("agent_type", "").upper()

    if not agent_type:
        return jsonify({
            "success": False,
            "error": "agent_type e obrigatorio"
        }), 400

    spawner = get_spawner()
    success = spawner.terminate_terminal(agent_type)

    if success:
        return jsonify({
            "success": True,
            "message": f"Agente {agent_type} terminado"
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Agente {agent_type} nao encontrado ou ja terminado"
        }), 404


@orchestrator_bp.route('/mode', methods=['GET'])
def get_mode():
    """Retorna modo atual do orquestrador."""
    return jsonify({
        "success": True,
        "data": {
            "mode": _orchestrator_state["mode"],
            "description": {
                "autonomous": "Agentes trabalham sem intervencao humana",
                "supervised": "Pede confirmacao antes de acoes criticas",
                "interactive": "Controle manual de cada acao"
            }.get(_orchestrator_state["mode"], "")
        }
    })


@orchestrator_bp.route('/mode', methods=['PUT'])
def set_mode():
    """Altera modo do orquestrador."""
    data = request.get_json() or {}
    mode = data.get("mode", "")

    if mode not in ["autonomous", "supervised", "interactive"]:
        return jsonify({
            "success": False,
            "error": "Modo invalido. Use: autonomous, supervised, interactive"
        }), 400

    _orchestrator_state["mode"] = mode

    return jsonify({
        "success": True,
        "message": f"Modo alterado para {mode}",
        "data": {"mode": mode}
    })


def register_orchestrator_routes(app):
    """Registra as rotas do orquestrador no app Flask."""
    app.register_blueprint(orchestrator_bp)
