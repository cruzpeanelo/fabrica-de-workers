"""
Log Hook - Loga eventos do sistema.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict, Any


LOG_FILE = Path("factory/state/hooks.log")


def execute(context: Dict[str, Any]):
    """
    Loga o evento.

    Args:
        context: Contexto do evento
    """
    LOG_FILE.parent.mkdir(parents=True, exist_ok=True)

    timestamp = context.get("timestamp", datetime.now().isoformat())
    event = context.get("event", "unknown")

    log_entry = {
        "timestamp": timestamp,
        "event": event,
        "context": {k: v for k, v in context.items() if k not in ["timestamp", "event"]}
    }

    with open(LOG_FILE, 'a', encoding='utf-8') as f:
        f.write(json.dumps(log_entry, ensure_ascii=False) + "\n")

    print(f"[LOG] {event} @ {timestamp}")
