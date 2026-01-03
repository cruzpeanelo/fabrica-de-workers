"""
Hooks Routes - API REST para gerenciamento de Hooks.

Endpoints:
- GET  /api/hooks/            - Listar hooks registrados
- POST /api/hooks/            - Registrar novo hook
- GET  /api/hooks/events      - Listar eventos disponiveis
- GET  /api/hooks/{id}        - Detalhes de um hook
- DELETE /api/hooks/{id}      - Remover hook
- POST /api/hooks/trigger     - Disparar evento manualmente
- GET  /api/hooks/logs        - Historico de execucoes
"""

from flask import Blueprint, request, jsonify
from typing import Optional, List
import sys
import os
from pathlib import Path

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.hooks.hook_manager import (
    get_hook_manager, Hook, HookEvent, HookPriority
)

hooks_bp = Blueprint('hooks', __name__, url_prefix='/api/hooks')


@hooks_bp.route('/', methods=['GET'])
def list_hooks():
    """Lista todos os hooks registrados."""
    manager = get_hook_manager()

    hooks = []
    for hook_list in manager.hooks.values():
        for hook in hook_list:
            hooks.append({
                "id": hook.id,
                "name": hook.name,
                "event": hook.event.value if isinstance(hook.event, HookEvent) else hook.event,
                "priority": hook.priority.value if isinstance(hook.priority, HookPriority) else hook.priority,
                "enabled": hook.enabled,
                "description": hook.description
            })

    return jsonify({
        "success": True,
        "data": hooks
    })


@hooks_bp.route('/', methods=['POST'])
def register_hook():
    """Registra um novo hook."""
    data = request.get_json() or {}

    name = data.get("name")
    event = data.get("event")
    callback_path = data.get("callback_path")  # Caminho para arquivo Python
    description = data.get("description", "")
    priority = data.get("priority", "normal")
    custom_event = data.get("custom_event")

    if not name:
        return jsonify({"success": False, "error": "name e obrigatorio"}), 400

    if not event:
        return jsonify({"success": False, "error": "event e obrigatorio"}), 400

    # Converter string para HookEvent
    try:
        hook_event = HookEvent(event)
    except ValueError:
        # Se nao for evento padrao, usar CUSTOM
        hook_event = HookEvent.CUSTOM

    # Converter priority
    try:
        hook_priority = HookPriority(priority)
    except ValueError:
        hook_priority = HookPriority.NORMAL

    # Criar callback dinamico se callback_path fornecido
    callback = None
    if callback_path:
        callback_file = Path(callback_path)
        if callback_file.exists():
            # Importar modulo dinamicamente
            import importlib.util
            spec = importlib.util.spec_from_file_location("custom_hook", callback_file)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            if hasattr(module, 'execute'):
                callback = module.execute
            else:
                return jsonify({
                    "success": False,
                    "error": "Arquivo de callback deve ter funcao 'execute'"
                }), 400
        else:
            return jsonify({
                "success": False,
                "error": f"Arquivo nao encontrado: {callback_path}"
            }), 404
    else:
        # Callback padrao que apenas loga
        def default_callback(ctx):
            print(f"[Hook:{name}] Evento {event} disparado")
        callback = default_callback

    # Criar e registrar hook
    hook = Hook(
        name=name,
        event=hook_event,
        callback=callback,
        priority=hook_priority,
        enabled=True,
        description=description,
        custom_event=custom_event
    )

    manager = get_hook_manager()
    success = manager.register(hook)

    if success:
        return jsonify({
            "success": True,
            "message": f"Hook '{name}' registrado com sucesso",
            "data": {
                "id": hook.id,
                "name": hook.name,
                "event": event
            }
        })
    else:
        return jsonify({
            "success": False,
            "error": f"Hook '{name}' ja existe"
        }), 400


@hooks_bp.route('/events', methods=['GET'])
def list_events():
    """Lista todos os eventos disponiveis."""
    events = [
        {"name": e.value, "description": _get_event_description(e)}
        for e in HookEvent
    ]

    return jsonify({
        "success": True,
        "data": events
    })


def _get_event_description(event: HookEvent) -> str:
    """Retorna descricao do evento."""
    descriptions = {
        HookEvent.PRE_COMMIT: "Executado antes de um git commit",
        HookEvent.POST_COMMIT: "Executado depois de um git commit",
        HookEvent.PRE_TASK: "Executado antes de iniciar uma task",
        HookEvent.POST_TASK: "Executado depois de finalizar uma task",
        HookEvent.PRE_HANDOFF: "Executado antes de transferir para outro agente",
        HookEvent.POST_HANDOFF: "Executado depois de transferir para outro agente",
        HookEvent.ON_SPAWN: "Executado quando um terminal e spawneado",
        HookEvent.ON_TERMINATE: "Executado quando um terminal e terminado",
        HookEvent.ON_ERROR: "Executado quando ocorre um erro",
        HookEvent.ON_CYCLE: "Executado a cada ciclo do orquestrador",
        HookEvent.ON_INTERRUPT: "Executado quando o sistema e interrompido",
        HookEvent.CUSTOM: "Evento customizado definido pelo usuario",
    }
    return descriptions.get(event, "")


@hooks_bp.route('/<hook_id>', methods=['GET'])
def get_hook(hook_id: str):
    """Retorna detalhes de um hook."""
    manager = get_hook_manager()

    for hook_list in manager.hooks.values():
        for hook in hook_list:
            if hook.id == hook_id:
                return jsonify({
                    "success": True,
                    "data": {
                        "id": hook.id,
                        "name": hook.name,
                        "event": hook.event.value if isinstance(hook.event, HookEvent) else hook.event,
                        "priority": hook.priority.value if isinstance(hook.priority, HookPriority) else hook.priority,
                        "enabled": hook.enabled,
                        "description": hook.description
                    }
                })

    return jsonify({
        "success": False,
        "error": f"Hook {hook_id} nao encontrado"
    }), 404


@hooks_bp.route('/<hook_id>', methods=['DELETE'])
def delete_hook(hook_id: str):
    """Remove um hook."""
    manager = get_hook_manager()

    # Encontrar e remover hook
    for event, hook_list in manager.hooks.items():
        for hook in hook_list:
            if hook.id == hook_id:
                manager.unregister(hook.name, event)
                return jsonify({
                    "success": True,
                    "message": f"Hook {hook_id} removido"
                })

    return jsonify({
        "success": False,
        "error": f"Hook {hook_id} nao encontrado"
    }), 404


@hooks_bp.route('/trigger', methods=['POST'])
def trigger_event():
    """Dispara um evento manualmente."""
    data = request.get_json() or {}
    event = data.get("event")
    context = data.get("context", {})

    if not event:
        return jsonify({"success": False, "error": "event e obrigatorio"}), 400

    manager = get_hook_manager()

    # Adicionar info de trigger manual
    context["manual_trigger"] = True
    context["source"] = "dashboard"

    manager.trigger(event, context)

    return jsonify({
        "success": True,
        "message": f"Evento '{event}' disparado"
    })


@hooks_bp.route('/logs', methods=['GET'])
def get_logs():
    """Retorna historico de execucoes de hooks."""
    log_file = Path("factory/state/hooks.log")

    if not log_file.exists():
        return jsonify({
            "success": True,
            "data": []
        })

    import json

    logs = []
    limit = request.args.get("limit", 100, type=int)

    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        for line in lines[-limit:]:
            try:
                log_entry = json.loads(line.strip())
                logs.append(log_entry)
            except:
                continue

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

    return jsonify({
        "success": True,
        "data": logs
    })


def register_hooks_routes(app):
    """Registra as rotas de hooks no app Flask."""
    app.register_blueprint(hooks_bp)
