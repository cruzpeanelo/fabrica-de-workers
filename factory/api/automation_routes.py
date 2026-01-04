# -*- coding: utf-8 -*-
"""
Automation API Routes - Plataforma E
==========================================
Endpoints para controlar o sistema de automacao autonoma.

Permite iniciar/parar orquestrador, visualizar status,
aprovar/rejeitar tarefas, e configurar modo de operacao.

Author: Plataforma E
"""

import asyncio
import logging
from flask import Blueprint, jsonify, request
from typing import Dict, Any, Optional

logger = logging.getLogger(__name__)

# Blueprint para rotas de automacao
automation_bp = Blueprint('automation', __name__, url_prefix='/api/automation')

# Referencia global ao orquestrador (sera setada pelo app principal)
_orchestrator = None
_orchestrator_task = None


def set_orchestrator(orchestrator):
    """Define o orquestrador global."""
    global _orchestrator
    _orchestrator = orchestrator


def get_orchestrator():
    """Retorna o orquestrador global."""
    return _orchestrator


# ==============================================================================
# CONTROLE DO ORQUESTRADOR
# ==============================================================================


@automation_bp.route('/start', methods=['POST'])
def start_automation():
    """
    Inicia o sistema de automacao.

    Body (opcional):
        mode: "autonomous" | "supervised" | "manual"

    Returns:
        Status da operacao
    """
    global _orchestrator_task

    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    if _orchestrator.state.status == "running":
        return jsonify({
            "success": False,
            "error": "Orquestrador ja esta rodando"
        }), 400

    data = request.get_json() or {}
    mode = data.get("mode", "supervised")

    try:
        # Atualizar modo se especificado
        if mode in ["autonomous", "supervised", "manual"]:
            _orchestrator.mode = mode
            _orchestrator.config.mode = mode
            _orchestrator.config.save()

        # Iniciar em background
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)

        import threading
        def run_orchestrator():
            loop.run_until_complete(_orchestrator.run_forever())

        thread = threading.Thread(target=run_orchestrator, daemon=True)
        thread.start()

        logger.info(f"Orquestrador iniciado em modo {mode}")

        return jsonify({
            "success": True,
            "message": f"Orquestrador iniciado em modo {mode}",
            "status": _orchestrator.get_status()
        })

    except Exception as e:
        logger.error(f"Erro ao iniciar orquestrador: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@automation_bp.route('/stop', methods=['POST'])
def stop_automation():
    """
    Para o sistema de automacao.

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    if _orchestrator.state.status != "running":
        return jsonify({
            "success": False,
            "error": "Orquestrador nao esta rodando"
        }), 400

    try:
        # Criar loop para executar stop async
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(_orchestrator.stop())

        logger.info("Orquestrador parado")

        return jsonify({
            "success": True,
            "message": "Orquestrador parado",
            "status": _orchestrator.get_status()
        })

    except Exception as e:
        logger.error(f"Erro ao parar orquestrador: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@automation_bp.route('/pause', methods=['POST'])
def pause_automation():
    """
    Pausa o sistema de automacao.

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    _orchestrator.pause()

    return jsonify({
        "success": True,
        "message": "Orquestrador pausado",
        "status": _orchestrator.get_status()
    })


@automation_bp.route('/resume', methods=['POST'])
def resume_automation():
    """
    Retoma o sistema de automacao.

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    _orchestrator.resume()

    return jsonify({
        "success": True,
        "message": "Orquestrador retomado",
        "status": _orchestrator.get_status()
    })


# ==============================================================================
# STATUS E MONITORAMENTO
# ==============================================================================


@automation_bp.route('/status', methods=['GET'])
def get_status():
    """
    Retorna status do sistema de automacao.

    Returns:
        Status completo do orquestrador e agentes
    """
    if not _orchestrator:
        return jsonify({
            "status": "not_initialized",
            "message": "Orquestrador nao inicializado"
        })

    return jsonify(_orchestrator.get_status())


@automation_bp.route('/agents', methods=['GET'])
def get_agents():
    """
    Retorna status de todos os agentes.

    Returns:
        Lista de status dos agentes
    """
    if not _orchestrator:
        return jsonify({
            "agents": [],
            "error": "Orquestrador nao inicializado"
        })

    return jsonify({
        "agents": _orchestrator.agent_pool.get_all_status()
    })


@automation_bp.route('/agents/<agent_id>', methods=['GET'])
def get_agent(agent_id: str):
    """
    Retorna status de um agente especifico.

    Args:
        agent_id: ID do agente (ex: "BACK")

    Returns:
        Status do agente
    """
    if not _orchestrator:
        return jsonify({
            "error": "Orquestrador nao inicializado"
        }), 500

    runner = _orchestrator.agent_pool.get_runner(agent_id)
    if not runner:
        return jsonify({
            "error": f"Agente {agent_id} nao encontrado"
        }), 404

    return jsonify(runner.get_status())


@automation_bp.route('/handoffs', methods=['GET'])
def get_handoffs():
    """
    Retorna handoffs pendentes e historico.

    Query params:
        status: Filtrar por status (pending, in_progress, completed)
        agent: Filtrar por agente destino

    Returns:
        Lista de handoffs
    """
    if not _orchestrator:
        return jsonify({
            "handoffs": [],
            "error": "Orquestrador nao inicializado"
        })

    status_filter = request.args.get('status')
    agent_filter = request.args.get('agent')

    handoffs = _orchestrator.handoff_manager.handoffs

    if status_filter:
        handoffs = [h for h in handoffs if h.status == status_filter]

    if agent_filter:
        handoffs = [h for h in handoffs if h.to_agent == f"[{agent_filter.upper()}]"]

    return jsonify({
        "handoffs": [
            {
                "id": h.handoff_id,
                "from_agent": h.from_agent,
                "to_agent": h.to_agent,
                "task": h.task_description,
                "issue_number": h.issue_number,
                "status": h.status,
                "created_at": h.created_at,
                "completed_at": h.completed_at
            }
            for h in handoffs
        ],
        "stats": _orchestrator.handoff_manager.get_stats()
    })


@automation_bp.route('/context', methods=['GET'])
def get_context_usage():
    """
    Retorna uso de contexto de todos os agentes.

    Returns:
        Mapa de agente -> percentual de uso
    """
    if not _orchestrator:
        return jsonify({
            "context": {},
            "error": "Orquestrador nao inicializado"
        })

    return jsonify({
        "context": _orchestrator.context_manager.get_all_usage(),
        "threshold": _orchestrator.context_manager.COMPACT_THRESHOLD,
        "max_tokens": _orchestrator.context_manager.MAX_CONTEXT_TOKENS
    })


# ==============================================================================
# CONFIGURACAO
# ==============================================================================


@automation_bp.route('/config', methods=['GET'])
def get_config():
    """
    Retorna configuracao atual do sistema.

    Returns:
        Configuracao de automacao
    """
    if not _orchestrator:
        return jsonify({
            "error": "Orquestrador nao inicializado"
        }), 500

    config = _orchestrator.config
    return jsonify({
        "mode": config.mode,
        "poll_interval": config.poll_interval,
        "max_concurrent_tasks": config.max_concurrent_tasks,
        "auto_compact_enabled": config.auto_compact_enabled,
        "context_threshold": config.context_threshold,
        "notify_on_error": config.notify_on_error,
        "notify_on_complete": config.notify_on_complete,
        "enabled_agents": config.enabled_agents,
        "task_timeout": config.task_timeout,
        "max_retries": config.max_retries
    })


@automation_bp.route('/config', methods=['PATCH'])
def update_config():
    """
    Atualiza configuracao do sistema.

    Body:
        Campos a atualizar (mode, poll_interval, etc)

    Returns:
        Configuracao atualizada
    """
    if not _orchestrator:
        return jsonify({
            "error": "Orquestrador nao inicializado"
        }), 500

    data = request.get_json()
    if not data:
        return jsonify({
            "error": "Body vazio"
        }), 400

    config = _orchestrator.config

    # Atualizar campos permitidos
    allowed_fields = [
        "mode", "poll_interval", "max_concurrent_tasks",
        "auto_compact_enabled", "context_threshold",
        "notify_on_error", "notify_on_complete",
        "enabled_agents", "task_timeout", "max_retries"
    ]

    for field in allowed_fields:
        if field in data:
            setattr(config, field, data[field])

    # Salvar
    config.save()

    # Atualizar modo do orquestrador
    if "mode" in data:
        _orchestrator.mode = data["mode"]

    logger.info(f"Configuracao atualizada: {data}")

    return jsonify({
        "success": True,
        "config": {
            "mode": config.mode,
            "poll_interval": config.poll_interval,
            "max_concurrent_tasks": config.max_concurrent_tasks
        }
    })


# ==============================================================================
# APROVACOES (MODO SUPERVISIONADO)
# ==============================================================================


@automation_bp.route('/pending-approvals', methods=['GET'])
def get_pending_approvals():
    """
    Retorna tarefas pendentes de aprovacao (modo supervisionado).

    Returns:
        Lista de tarefas aguardando aprovacao
    """
    if not _orchestrator:
        return jsonify({
            "approvals": [],
            "error": "Orquestrador nao inicializado"
        })

    # Por enquanto, retorna handoffs pendentes como proxy
    pending = _orchestrator.handoff_manager.get_all_pending()

    return jsonify({
        "approvals": [
            {
                "id": h.handoff_id,
                "type": "handoff",
                "from_agent": h.from_agent,
                "to_agent": h.to_agent,
                "description": h.task_description,
                "issue_number": h.issue_number,
                "created_at": h.created_at
            }
            for h in pending
        ]
    })


@automation_bp.route('/approve/<task_id>', methods=['POST'])
def approve_task(task_id: str):
    """
    Aprova uma tarefa pendente.

    Args:
        task_id: ID da tarefa/handoff

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    # Marcar como em progresso (aprovado)
    handoff = _orchestrator.handoff_manager.mark_in_progress(task_id)

    if handoff:
        logger.info(f"Tarefa {task_id} aprovada")
        return jsonify({
            "success": True,
            "message": f"Tarefa {task_id} aprovada"
        })

    return jsonify({
        "success": False,
        "error": f"Tarefa {task_id} nao encontrada"
    }), 404


@automation_bp.route('/reject/<task_id>', methods=['POST'])
def reject_task(task_id: str):
    """
    Rejeita uma tarefa pendente.

    Args:
        task_id: ID da tarefa/handoff

    Body (opcional):
        reason: Motivo da rejeicao

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    data = request.get_json() or {}
    reason = data.get("reason", "Rejeitado pelo usuario")

    # Marcar como falho (rejeitado)
    handoff = _orchestrator.handoff_manager.mark_failed(task_id, reason)

    if handoff:
        logger.info(f"Tarefa {task_id} rejeitada: {reason}")
        return jsonify({
            "success": True,
            "message": f"Tarefa {task_id} rejeitada"
        })

    return jsonify({
        "success": False,
        "error": f"Tarefa {task_id} nao encontrada"
    }), 404


# ==============================================================================
# ACOES MANUAIS
# ==============================================================================


@automation_bp.route('/trigger-cycle', methods=['POST'])
def trigger_cycle():
    """
    Dispara um ciclo manual do orquestrador.

    Util para testar ou forcar verificacao de issues.

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    try:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(_orchestrator.run_cycle())

        return jsonify({
            "success": True,
            "message": "Ciclo executado",
            "status": _orchestrator.get_status()
        })

    except Exception as e:
        logger.error(f"Erro ao executar ciclo: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@automation_bp.route('/assign-task', methods=['POST'])
def assign_task():
    """
    Atribui tarefa manualmente a um agente.

    Body:
        agent_id: ID do agente (ex: "BACK")
        task: Descricao da tarefa
        issue_number: Numero da issue (opcional)

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    data = request.get_json()
    if not data:
        return jsonify({
            "success": False,
            "error": "Body vazio"
        }), 400

    agent_id = data.get("agent_id")
    task = data.get("task")
    issue_number = data.get("issue_number")

    if not agent_id or not task:
        return jsonify({
            "success": False,
            "error": "agent_id e task sao obrigatorios"
        }), 400

    try:
        runner = _orchestrator.agent_pool.get_runner(agent_id)
        if not runner:
            return jsonify({
                "success": False,
                "error": f"Agente {agent_id} nao encontrado"
            }), 404

        # Executar tarefa em background
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)

        import threading
        def run_task():
            loop.run_until_complete(runner.run_task(task))

        thread = threading.Thread(target=run_task, daemon=True)
        thread.start()

        logger.info(f"Tarefa atribuida a {agent_id}: {task[:50]}...")

        return jsonify({
            "success": True,
            "message": f"Tarefa atribuida a {agent_id}",
            "agent_status": runner.get_status()
        })

    except Exception as e:
        logger.error(f"Erro ao atribuir tarefa: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@automation_bp.route('/compact-context/<agent_id>', methods=['POST'])
def compact_agent_context(agent_id: str):
    """
    Forca compactacao de contexto de um agente.

    Args:
        agent_id: ID do agente

    Returns:
        Status da operacao
    """
    if not _orchestrator:
        return jsonify({
            "success": False,
            "error": "Orquestrador nao inicializado"
        }), 500

    try:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)

        runner = _orchestrator.agent_pool.get_runner(agent_id)
        success = loop.run_until_complete(
            _orchestrator.context_manager.compact_agent_context(
                agent_id=agent_id,
                current_summary=f"Compactacao manual para {agent_id}",
                agent_runner=runner
            )
        )

        if success:
            return jsonify({
                "success": True,
                "message": f"Contexto de {agent_id} compactado"
            })
        else:
            return jsonify({
                "success": False,
                "error": "Falha na compactacao"
            }), 500

    except Exception as e:
        logger.error(f"Erro ao compactar contexto: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


# ==============================================================================
# LOGS E METRICAS
# ==============================================================================


@automation_bp.route('/logs', methods=['GET'])
def get_logs():
    """
    Retorna logs recentes do sistema.

    Query params:
        agent: Filtrar por agente
        limit: Numero de logs (default 100)

    Returns:
        Lista de logs
    """
    # Por enquanto, retorna historico dos agentes
    if not _orchestrator:
        return jsonify({
            "logs": []
        })

    agent_filter = request.args.get('agent')
    limit = int(request.args.get('limit', 100))

    logs = []
    for agent_id, runner in _orchestrator.agent_pool.runners.items():
        if agent_filter and agent_id != agent_filter.upper():
            continue

        for entry in runner.state.history[-limit:]:
            logs.append({
                "agent": agent_id,
                "timestamp": entry.get("timestamp"),
                "action": entry.get("action"),
                "details": entry.get("details")
            })

    # Ordenar por timestamp
    logs.sort(key=lambda x: x.get("timestamp", ""), reverse=True)

    return jsonify({
        "logs": logs[:limit]
    })


@automation_bp.route('/metrics', methods=['GET'])
def get_metrics():
    """
    Retorna metricas do sistema de automacao.

    Returns:
        Metricas agregadas
    """
    if not _orchestrator:
        return jsonify({
            "metrics": {}
        })

    status = _orchestrator.get_status()

    # Calcular metricas
    total_completed = sum(
        a.get("tasks_completed", 0)
        for a in status.get("agents", {}).values()
    )
    total_failed = sum(
        a.get("tasks_failed", 0)
        for a in status.get("agents", {}).values()
    )

    return jsonify({
        "metrics": {
            "orchestrator": {
                "cycles": status.get("current_cycle", 0),
                "issues_processed": status.get("issues_processed", 0),
                "tasks_distributed": status.get("tasks_distributed", 0),
                "errors": status.get("errors_count", 0)
            },
            "agents": {
                "total_completed": total_completed,
                "total_failed": total_failed,
                "success_rate": (
                    total_completed / (total_completed + total_failed) * 100
                    if (total_completed + total_failed) > 0 else 0
                )
            },
            "handoffs": status.get("handoffs", {})
        }
    })
