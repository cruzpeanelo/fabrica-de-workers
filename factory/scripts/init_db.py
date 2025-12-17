#!/usr/bin/env python
"""
Database Initialization Script
Fabrica de Agentes v4.0

Cria tabelas e dados iniciais.

Usage:
    python factory/scripts/init_db.py [--drop-existing] [--seed]
"""
import asyncio
import argparse
import sys
from pathlib import Path

# Adicionar raiz ao path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent))

from factory.config import DATABASE_URL, FACTORY_DB


async def init_database(drop_existing: bool = False, seed: bool = False):
    """
    Inicializa o banco de dados

    Args:
        drop_existing: Se True, dropa tabelas existentes
        seed: Se True, insere dados iniciais
    """
    print("=" * 60)
    print("  FABRICA DE AGENTES v4.0 - Database Init")
    print("=" * 60)
    print(f"  Database URL: {DATABASE_URL[:50]}...")
    print()

    # Importar apos path setup
    from factory.database.connection import engine, async_engine, Base
    from factory.database import models  # Importa modelos para registrar

    try:
        # Detectar tipo de banco
        is_postgresql = "postgresql" in DATABASE_URL
        is_async = async_engine is not None

        if is_async and is_postgresql:
            print("[DB] Usando PostgreSQL async...")
            async with async_engine.begin() as conn:
                if drop_existing:
                    print("[DB] Dropando tabelas existentes...")
                    await conn.run_sync(Base.metadata.drop_all)

                print("[DB] Criando tabelas...")
                await conn.run_sync(Base.metadata.create_all)
        else:
            print("[DB] Usando SQLite sync...")
            if drop_existing:
                print("[DB] Dropando tabelas existentes...")
                Base.metadata.drop_all(bind=engine)

            print("[DB] Criando tabelas...")
            Base.metadata.create_all(bind=engine)

        print("[DB] Tabelas criadas com sucesso!")

        # Listar tabelas criadas
        tables = list(Base.metadata.tables.keys())
        print(f"\n[DB] Tabelas ({len(tables)}):")
        for table in sorted(tables):
            print(f"  - {table}")

        # Seed data
        if seed:
            print("\n[DB] Inserindo dados iniciais...")
            await seed_data()

        print("\n[DB] Inicializacao completa!")

    except Exception as e:
        print(f"\n[ERROR] Erro ao inicializar banco: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


async def seed_data():
    """Insere dados iniciais"""
    from factory.database.connection import SessionLocal
    from factory.database.models import User, Project
    from factory.api.auth import get_password_hash
    from datetime import datetime
    import uuid

    db = SessionLocal()

    try:
        # Verificar se ja existe admin
        admin = db.query(User).filter(User.username == "admin").first()

        if not admin:
            print("[Seed] Criando usuario admin...")
            admin = User(
                username="admin",
                email="admin@fabrica.local",
                password_hash=get_password_hash("admin123"),
                role="ADMIN",
                active=True
            )
            db.add(admin)
            db.commit()
            print("[Seed] Usuario admin criado (admin/admin123)")
        else:
            print("[Seed] Usuario admin ja existe")

        # Verificar se existe projeto de exemplo
        example_project = db.query(Project).filter(
            Project.name == "Projeto Exemplo"
        ).first()

        if not example_project:
            print("[Seed] Criando projeto de exemplo...")
            project_id = f"PRJ-{datetime.now().strftime('%Y%m%d%H%M%S')}"
            project = Project(
                project_id=project_id,
                name="Projeto Exemplo",
                description="Projeto de exemplo para demonstracao da Fabrica v4.0",
                project_type="api-service",
                status="PLANNING"
            )
            db.add(project)
            db.commit()
            print(f"[Seed] Projeto exemplo criado: {project.project_id}")
        else:
            print("[Seed] Projeto exemplo ja existe")

    except Exception as e:
        db.rollback()
        print(f"[Seed] Erro: {e}")
        raise
    finally:
        db.close()


def cli():
    """CLI"""
    parser = argparse.ArgumentParser(description="Inicializa banco de dados")

    parser.add_argument(
        "--drop-existing",
        action="store_true",
        help="Dropa tabelas existentes antes de criar"
    )

    parser.add_argument(
        "--seed",
        action="store_true",
        help="Insere dados iniciais (admin user, etc)"
    )

    args = parser.parse_args()

    asyncio.run(init_database(
        drop_existing=args.drop_existing,
        seed=args.seed
    ))


if __name__ == "__main__":
    cli()
