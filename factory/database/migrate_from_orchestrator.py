"""
Migracao de Dados do Orchestrator Antigo para Plataforma E
Migra stories, tasks, logs e eventos do projeto Gestao Estrategica
"""
import sqlite3
import json
from datetime import datetime
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import SessionLocal, init_db
from factory.database.models import (
    Project, Story, Task, ActivityLog, FactoryEvent, Agent
)

# Paths
OLD_DB = Path(r"C:\Users\lcruz\Estoque Ageing\orchestrator\database\orchestrator.db")
PROJECT_PATH = Path(r"C:\Users\lcruz\Plataforma E\projects\gestao-estrategica")


def get_old_connection():
    """Conecta ao banco antigo"""
    return sqlite3.connect(OLD_DB)


def migrate_project(db):
    """Cria o projeto Gestao Estrategica"""
    print("\n[1/5] Criando projeto Gestao Estrategica...")

    # Verifica se ja existe
    existing = db.query(Project).filter(Project.name == "Gestao Estrategica").first()
    if existing:
        print(f"  - Projeto ja existe: {existing.project_id}")
        return existing

    project = Project(
        project_id="PRJ-001",
        name="Gestao Estrategica",
        description="Sistema de analise de estoque ageing. Dashboard executivo com KPIs, analise por faixa de ageing, sistema de oportunidades e acoes, previsao de demanda com ML.",
        project_type="web-app",
        folder_path=str(PROJECT_PATH),
        status="IN_PROGRESS",
        progress=85.0,
        github_url="https://github.com/cruzpeanelo/gestao-estrategica",
        config={
            "stack": {
                "backend": "FastAPI + SQLAlchemy",
                "frontend": "React + TypeScript + Vite",
                "database": "SQLite (3.2GB)",
                "styling": "TailwindCSS"
            },
            "features": [
                "Dashboard executivo",
                "Analise de estoque",
                "Sistema de oportunidades",
                "Previsao ML",
                "API GraphQL",
                "Integracao B2B"
            ]
        }
    )
    db.add(project)
    db.commit()
    db.refresh(project)
    print(f"  + Projeto criado: {project.project_id}")
    return project


def migrate_stories(db, project):
    """Migra stories do banco antigo"""
    print("\n[2/5] Migrando stories...")

    old_conn = get_old_connection()
    cursor = old_conn.cursor()

    cursor.execute("""
        SELECT story_id, title, description, status, points,
               acceptance_criteria, tasks, agents, created_at, sprint
        FROM stories
    """)

    count = 0
    for row in cursor.fetchall():
        story_id, title, description, status, points, criteria, tasks_json, agents_json, created_at, sprint = row

        # Verifica se ja existe
        existing = db.query(Story).filter(Story.story_id == story_id).first()
        if existing:
            continue

        # Mapeia status
        status_map = {
            "DONE": "DONE",
            "IN_PROGRESS": "IN_PROGRESS",
            "TODO": "TO_DO",
            "BLOCKED": "BLOCKED"
        }

        story = Story(
            story_id=story_id,
            title=title,
            description=description,
            project_id=project.project_id,
            status=status_map.get(status, status),
            sprint=sprint or 1,
            points=points or 0,
            priority=5,
            acceptance_criteria=json.loads(criteria) if criteria else [],
            agents=json.loads(agents_json) if agents_json else []
        )
        db.add(story)
        count += 1

    db.commit()
    old_conn.close()
    print(f"  + {count} stories migradas")
    return count


def migrate_tasks(db, project):
    """Migra tasks do banco antigo"""
    print("\n[3/5] Migrando tasks...")

    old_conn = get_old_connection()
    cursor = old_conn.cursor()

    cursor.execute("""
        SELECT task_id, task_type, agent_id, story_id, priority,
               status, result, started_at, completed_at, created_at
        FROM tasks
    """)

    count = 0
    for row in cursor.fetchall():
        task_id, task_type, agent_id, story_id, priority, status, result, started_at, completed_at, created_at = row

        # Verifica se ja existe
        existing = db.query(Task).filter(Task.task_id == task_id).first()
        if existing:
            continue

        # Mapeia status
        status_map = {
            "COMPLETED": "COMPLETED",
            "IN_PROGRESS": "IN_PROGRESS",
            "PENDING": "PENDING",
            "FAILED": "FAILED"
        }

        task = Task(
            task_id=task_id,
            task_type=task_type or "development",
            title=f"Task {task_type or 'development'}",
            description=result or "",
            project_id=project.project_id,
            story_id=story_id,
            agent_id=agent_id,
            status=status_map.get(status, "PENDING"),
            priority=priority or 5
        )
        db.add(task)
        count += 1

    db.commit()
    old_conn.close()
    print(f"  + {count} tasks migradas")
    return count


def migrate_activity_logs(db, project):
    """Migra logs de atividades"""
    print("\n[4/5] Migrando activity logs...")

    old_conn = get_old_connection()
    cursor = old_conn.cursor()

    cursor.execute("""
        SELECT source, level, event_type, task_id, story_id,
               agent_id, message, details, timestamp
        FROM activity_logs
        ORDER BY timestamp
    """)

    count = 0
    batch = []
    for row in cursor.fetchall():
        source, level, event_type, task_id, story_id, agent_id, message, details, timestamp = row

        log = ActivityLog(
            project_id=project.project_id,
            story_id=story_id,
            task_id=task_id,
            agent_id=agent_id,
            source=source or "orchestrator",
            level=level or "INFO",
            event_type=event_type or "info",
            message=message or "",
            details=json.loads(details) if details else {}
        )

        # Set timestamp manually
        if timestamp:
            try:
                log.timestamp = datetime.fromisoformat(timestamp.replace("Z", "+00:00").replace(" ", "T"))
            except:
                pass

        batch.append(log)
        count += 1

        # Batch insert
        if len(batch) >= 500:
            db.bulk_save_objects(batch)
            db.commit()
            batch = []
            print(f"    ... {count} logs processados")

    # Insert remaining
    if batch:
        db.bulk_save_objects(batch)
        db.commit()

    old_conn.close()
    print(f"  + {count} logs migrados")
    return count


def migrate_events(db, project):
    """Migra eventos do orchestrator"""
    print("\n[5/5] Migrando eventos...")

    old_conn = get_old_connection()
    cursor = old_conn.cursor()

    cursor.execute("""
        SELECT event_type, description, event_data, timestamp
        FROM orchestrator_events
        ORDER BY timestamp
    """)

    count = 0
    batch = []
    for row in cursor.fetchall():
        event_type, description, data, timestamp = row

        event = FactoryEvent(
            project_id=project.project_id,
            event_type=event_type or "system",
            description=description or "",
            event_data=json.loads(data) if data else {}
        )

        if timestamp:
            try:
                event.timestamp = datetime.fromisoformat(timestamp.replace("Z", "+00:00").replace(" ", "T"))
            except:
                pass

        batch.append(event)
        count += 1

        if len(batch) >= 200:
            db.bulk_save_objects(batch)
            db.commit()
            batch = []

    if batch:
        db.bulk_save_objects(batch)
        db.commit()

    old_conn.close()
    print(f"  + {count} eventos migrados")
    return count


def update_agent_metrics(db):
    """Atualiza metricas dos agentes com dados historicos"""
    print("\n[Extra] Atualizando metricas dos agentes...")

    old_conn = get_old_connection()
    cursor = old_conn.cursor()

    cursor.execute("""
        SELECT agent_id, tasks_completed, tasks_failed, total_execution_time, last_activity
        FROM agents
    """)

    for row in cursor.fetchall():
        agent_id, completed, failed, exec_time, last_activity = row

        agent = db.query(Agent).filter(Agent.agent_id == agent_id).first()
        if agent:
            agent.metrics = {
                "tasks_completed": completed or 0,
                "tasks_failed": failed or 0,
                "total_execution_time": exec_time or 0,
                "code_lines_written": 0,
                "files_modified": 0
            }
            if last_activity:
                try:
                    agent.last_activity = datetime.fromisoformat(last_activity.replace(" ", "T"))
                except:
                    pass

    db.commit()
    old_conn.close()
    print("  + Metricas atualizadas")


def run_migration():
    """Executa migracao completa"""
    print("=" * 60)
    print("MIGRACAO - Orchestrator -> Plataforma E")
    print("=" * 60)

    if not OLD_DB.exists():
        print(f"[ERRO] Banco antigo nao encontrado: {OLD_DB}")
        return

    init_db()
    db = SessionLocal()

    try:
        # 1. Criar projeto
        project = migrate_project(db)

        # 2. Migrar stories
        stories_count = migrate_stories(db, project)

        # 3. Migrar tasks
        tasks_count = migrate_tasks(db, project)

        # 4. Migrar logs
        logs_count = migrate_activity_logs(db, project)

        # 5. Migrar eventos
        events_count = migrate_events(db, project)

        # 6. Atualizar metricas
        update_agent_metrics(db)

        print("\n" + "=" * 60)
        print("MIGRACAO CONCLUIDA!")
        print("=" * 60)
        print(f"  Projeto: {project.name}")
        print(f"  Stories: {stories_count}")
        print(f"  Tasks: {tasks_count}")
        print(f"  Logs: {logs_count}")
        print(f"  Eventos: {events_count}")
        print("=" * 60)

    except Exception as e:
        print(f"[ERRO] {e}")
        db.rollback()
        raise
    finally:
        db.close()


if __name__ == "__main__":
    run_migration()
