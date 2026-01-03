"""
Notify Hook - Notifica sobre eventos importantes.
"""

from typing import Dict, Any


def execute(context: Dict[str, Any]):
    """
    Notifica sobre o evento.

    Args:
        context: Contexto do evento
    """
    event = context.get("event", "unknown")

    # Eventos criticos
    if event == "on_error":
        error = context.get("error", "Erro desconhecido")
        agent = context.get("agent_type", "ORCH")
        print(f"\n[ALERTA] Erro no agente [{agent}]: {error}")

    elif event == "on_interrupt":
        reason = context.get("reason", "unknown")
        print(f"\n[ALERTA] Sistema interrompido: {reason}")

    elif event == "post_task":
        status = context.get("status", "unknown")
        if status == "failed":
            task_id = context.get("task_id", "unknown")
            print(f"\n[ALERTA] Task falhou: {task_id}")
