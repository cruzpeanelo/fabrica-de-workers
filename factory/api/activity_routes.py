"""
Activity Routes - API REST para monitoramento de atividades em tempo real.

Endpoints:
- GET  /api/activity/recent      - Atividades recentes
- GET  /api/activity/timeline    - Timeline de atividades
- GET  /api/activity/agents      - Status de todos os agentes
- GET  /api/activity/stream      - SSE stream em tempo real
- POST /api/activity/log         - Registrar atividade (para agentes)
"""

from flask import Blueprint, request, jsonify, Response
import sys
import os
import json
import time
from pathlib import Path

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.core.activity_logger import get_activity_logger, log_activity

activity_bp = Blueprint('activity', __name__, url_prefix='/api/activity')


@activity_bp.route('/recent', methods=['GET'])
def get_recent_activities():
    """Retorna atividades recentes."""
    limit = request.args.get('limit', 50, type=int)
    agent_type = request.args.get('agent')

    logger = get_activity_logger()
    activities = logger.get_recent(limit=limit, agent_type=agent_type)

    return jsonify({
        "success": True,
        "data": activities,
        "count": len(activities)
    })


@activity_bp.route('/timeline', methods=['GET'])
def get_timeline():
    """Retorna timeline de atividades."""
    minutes = request.args.get('minutes', 30, type=int)

    logger = get_activity_logger()
    activities = logger.get_timeline(minutes=minutes)

    return jsonify({
        "success": True,
        "data": activities,
        "count": len(activities),
        "period_minutes": minutes
    })


@activity_bp.route('/agents', methods=['GET'])
def get_agents_status():
    """Retorna status atual de cada agente."""
    logger = get_activity_logger()
    status = logger.get_agent_status()

    # Adicionar info de terminais ativos
    try:
        from factory.core.terminal_spawner import get_spawner
        spawner = get_spawner()

        for agent_type, info in spawner.terminals.items():
            if agent_type in status:
                status[agent_type]["terminal_pid"] = info.pid
                status[agent_type]["terminal_status"] = info.status
            else:
                status[agent_type] = {
                    "agent_type": agent_type,
                    "terminal_pid": info.pid,
                    "terminal_status": info.status,
                    "current_task": info.task_id,
                    "status": "active"
                }
    except:
        pass

    return jsonify({
        "success": True,
        "data": status
    })


@activity_bp.route('/stream')
def stream_activities():
    """
    Server-Sent Events (SSE) stream de atividades em tempo real.

    Uso no frontend:
        const es = new EventSource('/api/activity/stream');
        es.onmessage = (e) => {
            const activity = JSON.parse(e.data);
            console.log(activity);
        };
    """
    def generate():
        logger = get_activity_logger()
        q = logger.subscribe()

        # Enviar heartbeat inicial
        yield f"data: {json.dumps({'type': 'connected'})}\n\n"

        try:
            while True:
                try:
                    # Esperar nova atividade (timeout de 30s para heartbeat)
                    activity = q.get(timeout=30)
                    yield f"data: {json.dumps(activity.to_dict())}\n\n"
                except:
                    # Timeout - enviar heartbeat
                    yield f"data: {json.dumps({'type': 'heartbeat'})}\n\n"
        finally:
            logger.unsubscribe(q)

    return Response(
        generate(),
        mimetype='text/event-stream',
        headers={
            'Cache-Control': 'no-cache',
            'Connection': 'keep-alive',
            'X-Accel-Buffering': 'no'
        }
    )


@activity_bp.route('/log', methods=['POST'])
def log_agent_activity():
    """
    Endpoint para agentes registrarem atividades.

    Body:
        {
            "agent_type": "BACK",
            "activity_type": "code_generate",
            "title": "Gerando endpoint",
            "description": "Criando endpoint de usuarios",
            "task_id": "TASK-001"
        }
    """
    data = request.get_json() or {}

    agent_type = data.get("agent_type")
    activity_type = data.get("activity_type")
    title = data.get("title")

    if not all([agent_type, activity_type, title]):
        return jsonify({
            "success": False,
            "error": "agent_type, activity_type e title sao obrigatorios"
        }), 400

    log_activity(
        agent_type=agent_type,
        activity_type=activity_type,
        title=title,
        description=data.get("description", ""),
        details=data.get("details"),
        task_id=data.get("task_id"),
        duration_ms=data.get("duration_ms")
    )

    return jsonify({
        "success": True,
        "message": "Atividade registrada"
    })


@activity_bp.route('/summary', methods=['GET'])
def get_summary():
    """Retorna resumo das atividades."""
    logger = get_activity_logger()
    recent = logger.get_recent(limit=100)

    # Contar por tipo
    by_type = {}
    by_agent = {}

    for activity in recent:
        # Por tipo
        atype = activity.get("activity_type", "unknown")
        by_type[atype] = by_type.get(atype, 0) + 1

        # Por agente
        agent = activity.get("agent_type", "unknown")
        by_agent[agent] = by_agent.get(agent, 0) + 1

    return jsonify({
        "success": True,
        "data": {
            "total_activities": len(recent),
            "by_type": by_type,
            "by_agent": by_agent,
            "agents_active": len(by_agent)
        }
    })


def register_activity_routes(app):
    """Registra as rotas de atividades no app Flask."""
    app.register_blueprint(activity_bp)
