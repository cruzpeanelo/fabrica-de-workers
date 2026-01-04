# -*- coding: utf-8 -*-
"""
Seed do Banco de Dados - Plataforma E v4.0
=================================================

Arquitetura MVP Worker-based:
- Workers (Claude Agent SDK instances)
- Jobs (unidade de trabalho principal)
- Users (autenticacao e quotas)

"""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import SessionLocal, init_db
from factory.database.models import User, Worker, Project
from datetime import datetime

try:
    from passlib.context import CryptContext
    pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
    HAS_PASSLIB = True
except ImportError:
    HAS_PASSLIB = False


def seed_admin_user(db):
    """Cria usuario admin padrao"""
    print("\n[Seed] Criando usuario admin...")

    existing = db.query(User).filter(User.username == "admin").first()
    if existing:
        print("  - Usuario admin ja existe")
        return

    if HAS_PASSLIB:
        password_hash = pwd_context.hash("admin123")
    else:
        # Fallback simples se passlib nao estiver instalado
        import hashlib
        password_hash = hashlib.sha256("admin123".encode()).hexdigest()

    admin = User(
        username="admin",
        password_hash=password_hash,
        email="admin@fabrica.local",
        role="ADMIN",
        active=True,
        quotas={
            "max_jobs_per_day": 100,
            "max_concurrent_jobs": 10,
            "max_projects": 100,
            "api_tier": "admin"
        },
        billing={
            "plan": "enterprise",
            "tokens_used": 0,
            "tokens_limit": 10000000,
            "cost_accumulated": 0.0,
            "budget_limit": 1000.0
        }
    )
    db.add(admin)
    db.commit()
    print("  + Usuario admin criado (senha: admin123)")


def seed_demo_user(db):
    """Cria usuario demo para testes"""
    print("\n[Seed] Criando usuario demo...")

    existing = db.query(User).filter(User.username == "demo").first()
    if existing:
        print("  - Usuario demo ja existe")
        return

    if HAS_PASSLIB:
        password_hash = pwd_context.hash("demo123")
    else:
        import hashlib
        password_hash = hashlib.sha256("demo123".encode()).hexdigest()

    demo = User(
        username="demo",
        password_hash=password_hash,
        email="demo@fabrica.local",
        role="USER",
        active=True,
        quotas={
            "max_jobs_per_day": 10,
            "max_concurrent_jobs": 2,
            "max_projects": 20,
            "api_tier": "free"
        },
        billing={
            "plan": "free",
            "tokens_used": 0,
            "tokens_limit": 100000,
            "cost_accumulated": 0.0,
            "budget_limit": 50.0
        }
    )
    db.add(demo)
    db.commit()
    print("  + Usuario demo criado (senha: demo123)")


def seed_workers(db):
    """Cria workers iniciais"""
    print("\n[Seed] Criando workers...")

    workers_config = [
        {
            "worker_id": "worker-001",
            "model": "claude-sonnet-4-20250514",
            "mcp_tools": ["filesystem", "git", "bash"],
            "hostname": "local-worker-1"
        },
        {
            "worker_id": "worker-002",
            "model": "claude-sonnet-4-20250514",
            "mcp_tools": ["filesystem", "git", "bash"],
            "hostname": "local-worker-2"
        },
        {
            "worker_id": "worker-003",
            "model": "claude-sonnet-4-20250514",
            "mcp_tools": ["filesystem", "git", "bash", "playwright"],
            "hostname": "local-worker-3"
        }
    ]

    count = 0
    for config in workers_config:
        existing = db.query(Worker).filter(Worker.worker_id == config["worker_id"]).first()
        if existing:
            print(f"  - Worker {config['worker_id']} ja existe")
            continue

        worker = Worker(
            worker_id=config["worker_id"],
            status="idle",
            model=config["model"],
            mcp_tools=config["mcp_tools"],
            hostname=config["hostname"],
            last_heartbeat=datetime.utcnow()
        )
        db.add(worker)
        count += 1
        print(f"  + Worker {config['worker_id']} criado")

    db.commit()
    print(f"[Seed] {count} workers criados")


def seed_demo_project(db):
    """Cria projeto demo"""
    print("\n[Seed] Criando projeto demo...")

    existing = db.query(Project).filter(Project.project_id == "PRJ-DEMO-001").first()
    if existing:
        print("  - Projeto demo ja existe")
        return

    project = Project(
        project_id="PRJ-DEMO-001",
        name="Projeto Demo",
        description="Projeto de demonstracao da Plataforma E v4.0",
        project_type="web-app",
        status="PLANNING",
        progress=0.0,
        config={
            "tech_stack": "Python + FastAPI + Vue.js",
            "features": ["Dashboard", "API REST", "Autenticacao"]
        },
        tags=["demo", "web-app", "fastapi"]
    )
    db.add(project)
    db.commit()
    print("  + Projeto PRJ-DEMO-001 criado")


def run_seed():
    """Executa todo o seed"""
    print("=" * 70)
    print("SEED - Plataforma E v4.0")
    print("Arquitetura MVP: Jobs + Workers + Claude Agent SDK")
    print("=" * 70)

    # Inicializa banco (cria tabelas)
    init_db()

    # Cria sessao
    db = SessionLocal()

    try:
        # Usuarios
        seed_admin_user(db)
        seed_demo_user(db)

        # Workers
        seed_workers(db)

        # Projeto demo
        seed_demo_project(db)

        # Estatisticas finais
        total_users = db.query(User).count()
        total_workers = db.query(Worker).count()
        total_projects = db.query(Project).count()

        print("\n" + "=" * 70)
        print("SEED CONCLUIDO COM SUCESSO!")
        print("=" * 70)
        print(f"\nEstatisticas:")
        print(f"  - Usuarios: {total_users}")
        print(f"  - Workers: {total_workers}")
        print(f"  - Projetos: {total_projects}")
        print("\nArquitetura v4.0:")
        print("  - Jobs sao a unidade de trabalho principal")
        print("  - Workers processam jobs usando Claude Agent SDK")
        print("  - Loop autonomo: Generate -> Lint -> Test -> Fix")
        print("=" * 70)

    except Exception as e:
        print(f"\n[ERRO] {e}")
        import traceback
        traceback.print_exc()
        db.rollback()
        raise
    finally:
        db.close()


if __name__ == "__main__":
    run_seed()
