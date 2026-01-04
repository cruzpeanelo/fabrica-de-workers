#!/usr/bin/env python3
"""
CLI para registro de atividades - Plataforma E
Permite registrar atividades dos agentes no banco de dados

Uso:
    python factory/log_activity.py --agent 08 --action task_start --message "Iniciando tarefa"
    python factory/log_activity.py -a 08 -t info -m "Processando dados"
"""
import argparse
import sys
from pathlib import Path
from datetime import datetime

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

from factory.database.connection import SessionLocal, init_db
from factory.database.models import ActivityLog, Agent, FactoryEvent


# Mapeamento de acoes para tipos de evento e niveis
ACTION_MAP = {
    "info": {"event_type": "info", "level": "INFO"},
    "warning": {"event_type": "warning", "level": "WARNING"},
    "error": {"event_type": "error", "level": "ERROR"},
    "debug": {"event_type": "debug", "level": "DEBUG"},
    "task_start": {"event_type": "task_started", "level": "INFO"},
    "task_complete": {"event_type": "task_completed", "level": "INFO"},
    "task_fail": {"event_type": "task_failed", "level": "ERROR"},
    "status": {"event_type": "status_changed", "level": "INFO"},
    "project_start": {"event_type": "project_started", "level": "INFO"},
    "project_complete": {"event_type": "project_completed", "level": "INFO"},
    "story_start": {"event_type": "story_started", "level": "INFO"},
    "story_complete": {"event_type": "story_completed", "level": "INFO"},
    "code_gen": {"event_type": "code_generated", "level": "INFO"},
    "decision": {"event_type": "decision_made", "level": "INFO"},
    "skill_exec": {"event_type": "skill_executed", "level": "INFO"},
}


def log_activity(
    agent_id: str,
    action: str,
    message: str,
    project_id: str = None,
    story_id: str = None,
    task_id: str = None,
    result: str = None,
    file_path: str = None,
    details: dict = None
):
    """Registra uma atividade no banco de dados"""

    # Garante que o banco existe
    init_db()

    db = SessionLocal()
    try:
        # Determina tipo e nivel
        action_config = ACTION_MAP.get(action, {"event_type": action, "level": "INFO"})

        # Monta detalhes adicionais
        extra_details = details or {}
        if result:
            extra_details["result"] = result
        if file_path:
            extra_details["file"] = file_path

        # Cria log de atividade
        log = ActivityLog(
            source=f"agent_{agent_id}",
            source_id=agent_id,
            project_id=project_id,
            agent_id=agent_id,
            task_id=task_id,
            story_id=story_id,
            level=action_config["level"],
            event_type=action_config["event_type"],
            message=message,
            details=extra_details,
            timestamp=datetime.utcnow()
        )
        db.add(log)

        # Atualiza status do agente se necessario
        if action in ["task_start", "task_complete", "task_fail", "status"]:
            agent = db.query(Agent).filter(Agent.agent_id == agent_id).first()
            if agent:
                if action == "task_start":
                    agent.status = "EXECUTING"
                    agent.current_task_id = task_id
                    agent.current_project_id = project_id
                    agent.current_story_id = story_id
                elif action == "task_complete":
                    agent.status = "STANDBY"
                    agent.current_task_id = None
                    agent.tasks_completed += 1
                elif action == "task_fail":
                    agent.status = "ERROR"
                    agent.tasks_failed += 1
                agent.last_activity = datetime.utcnow()

        # Cria evento se for acao significativa
        if action in ["task_start", "task_complete", "task_fail", "project_start",
                      "project_complete", "story_start", "story_complete", "decision"]:
            event = FactoryEvent(
                event_type=action_config["event_type"],
                project_id=project_id,
                task_id=task_id,
                story_id=story_id,
                agent_id=agent_id,
                description=message,
                event_data=extra_details,
                success=action != "task_fail",
                timestamp=datetime.utcnow()
            )
            db.add(event)

        db.commit()
        print(f"[LOG] {action_config['level']}: {message}")
        return True

    except Exception as e:
        print(f"[ERRO] Falha ao registrar atividade: {e}")
        db.rollback()
        return False
    finally:
        db.close()


def main():
    parser = argparse.ArgumentParser(
        description="Registra atividades dos agentes no banco de dados"
    )

    parser.add_argument(
        "-a", "--agent",
        required=True,
        help="ID do agente (01-19)"
    )

    parser.add_argument(
        "-t", "--action",
        required=True,
        choices=list(ACTION_MAP.keys()),
        help="Tipo de acao"
    )

    parser.add_argument(
        "-m", "--message",
        required=True,
        help="Mensagem descritiva"
    )

    parser.add_argument(
        "-p", "--project",
        default=None,
        help="ID do projeto (PRJ-001)"
    )

    parser.add_argument(
        "-s", "--story",
        default=None,
        help="ID da story (US-001)"
    )

    parser.add_argument(
        "--task",
        default=None,
        help="ID da tarefa (TASK-001)"
    )

    parser.add_argument(
        "-r", "--result",
        default=None,
        help="Resultado da acao"
    )

    parser.add_argument(
        "-f", "--file",
        default=None,
        help="Arquivo relacionado"
    )

    args = parser.parse_args()

    success = log_activity(
        agent_id=args.agent,
        action=args.action,
        message=args.message,
        project_id=args.project,
        story_id=args.story,
        task_id=args.task,
        result=args.result,
        file_path=args.file
    )

    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
