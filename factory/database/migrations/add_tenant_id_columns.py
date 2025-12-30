# -*- coding: utf-8 -*-
"""
Migration: Add Tenant ID Columns
================================
Issue #241: Adiciona coluna tenant_id para multi-tenancy nas tabelas que nao tem.

Tabelas afetadas:
- stories
- story_tasks
- story_documentation
- story_designs
- projects
- jobs
- tasks
- epics
- sprints
- chat_messages
- attachments
- execution_logs
- activity_logs

Uso:
    python -m factory.database.migrations.add_tenant_id_columns

Para verificar apenas:
    python -m factory.database.migrations.add_tenant_id_columns --check
"""

import sys
import os
import logging
from pathlib import Path

# Adiciona o diretorio raiz ao path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from sqlalchemy import text, inspect
from factory.database.connection import get_engine

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


# Tabelas que precisam de tenant_id
TABLES_WITH_TENANT_ID = [
    "stories",
    "story_tasks",
    "story_documentation",
    "story_designs",
    "projects",
    "jobs",
    "tasks",
    "epics",
    "sprints",
    "chat_messages",
    "attachments",
    "execution_logs",
    "activity_logs",
]


def get_database_type(engine) -> str:
    """Detecta o tipo de banco de dados"""
    dialect_name = engine.dialect.name
    return dialect_name  # 'sqlite', 'postgresql', 'mysql', etc.


def column_exists(conn, table_name: str, column_name: str, db_type: str) -> bool:
    """Verifica se uma coluna existe na tabela"""
    try:
        if db_type == 'sqlite':
            result = conn.execute(text(f"PRAGMA table_info({table_name})"))
            columns = {row[1] for row in result.fetchall()}
            return column_name in columns
        elif db_type == 'postgresql':
            result = conn.execute(text("""
                SELECT column_name
                FROM information_schema.columns
                WHERE table_name = :table AND column_name = :column
            """), {"table": table_name, "column": column_name})
            return result.fetchone() is not None
        else:
            # Fallback generico usando inspect
            inspector = inspect(conn)
            columns = [col['name'] for col in inspector.get_columns(table_name)]
            return column_name in columns
    except Exception as e:
        logger.warning(f"Erro ao verificar coluna {column_name} em {table_name}: {e}")
        return False


def table_exists(conn, table_name: str, db_type: str) -> bool:
    """Verifica se uma tabela existe"""
    try:
        if db_type == 'sqlite':
            result = conn.execute(text(
                "SELECT name FROM sqlite_master WHERE type='table' AND name=:name"
            ), {"name": table_name})
            return result.fetchone() is not None
        elif db_type == 'postgresql':
            result = conn.execute(text("""
                SELECT table_name
                FROM information_schema.tables
                WHERE table_name = :name AND table_schema = 'public'
            """), {"name": table_name})
            return result.fetchone() is not None
        else:
            inspector = inspect(conn)
            return table_name in inspector.get_table_names()
    except Exception as e:
        logger.warning(f"Erro ao verificar tabela {table_name}: {e}")
        return False


def add_tenant_id_column(conn, table_name: str, db_type: str) -> bool:
    """Adiciona a coluna tenant_id a uma tabela"""
    try:
        if db_type == 'sqlite':
            conn.execute(text(f"""
                ALTER TABLE {table_name} ADD COLUMN tenant_id VARCHAR(50)
            """))
        elif db_type == 'postgresql':
            conn.execute(text(f"""
                ALTER TABLE {table_name} ADD COLUMN IF NOT EXISTS tenant_id VARCHAR(50)
            """))
        else:
            conn.execute(text(f"""
                ALTER TABLE {table_name} ADD COLUMN tenant_id VARCHAR(50)
            """))
        return True
    except Exception as e:
        logger.error(f"Erro ao adicionar tenant_id em {table_name}: {e}")
        return False


def add_tenant_id_index(conn, table_name: str, db_type: str) -> bool:
    """Adiciona indice para tenant_id"""
    index_name = f"ix_{table_name}_tenant_id"
    try:
        if db_type == 'sqlite':
            conn.execute(text(f"""
                CREATE INDEX IF NOT EXISTS {index_name} ON {table_name} (tenant_id)
            """))
        elif db_type == 'postgresql':
            # PostgreSQL nao tem CREATE INDEX IF NOT EXISTS em versoes antigas
            conn.execute(text(f"""
                CREATE INDEX IF NOT EXISTS {index_name} ON {table_name} (tenant_id)
            """))
        return True
    except Exception as e:
        logger.warning(f"Aviso ao criar indice {index_name}: {e}")
        return False


def check_only():
    """Apenas verifica o estado atual sem fazer alteracoes"""
    engine = get_engine()
    db_type = get_database_type(engine)

    logger.info(f"Database type: {db_type}")
    logger.info("Verificando tabelas...")

    missing = []
    existing = []

    with engine.connect() as conn:
        for table in TABLES_WITH_TENANT_ID:
            if not table_exists(conn, table, db_type):
                logger.info(f"  {table}: TABELA NAO EXISTE")
                continue

            has_tenant_id = column_exists(conn, table, "tenant_id", db_type)
            if has_tenant_id:
                existing.append(table)
                logger.info(f"  {table}: tenant_id OK")
            else:
                missing.append(table)
                logger.warning(f"  {table}: tenant_id FALTANDO")

    logger.info(f"\nResumo: {len(existing)} tabelas OK, {len(missing)} precisam de migracao")
    if missing:
        logger.info(f"Tabelas sem tenant_id: {', '.join(missing)}")

    return len(missing) == 0


def upgrade():
    """Adiciona tenant_id em todas as tabelas necessarias"""
    engine = get_engine()
    db_type = get_database_type(engine)

    logger.info(f"Database type: {db_type}")
    logger.info("Iniciando migracao para adicionar tenant_id...")

    added = 0
    skipped = 0
    errors = 0

    with engine.connect() as conn:
        for table in TABLES_WITH_TENANT_ID:
            if not table_exists(conn, table, db_type):
                logger.info(f"Tabela {table} nao existe, pulando...")
                skipped += 1
                continue

            if column_exists(conn, table, "tenant_id", db_type):
                logger.info(f"Tabela {table} ja tem tenant_id, pulando...")
                skipped += 1
                continue

            logger.info(f"Adicionando tenant_id em {table}...")
            if add_tenant_id_column(conn, table, db_type):
                added += 1
                # Adiciona indice
                add_tenant_id_index(conn, table, db_type)
                logger.info(f"  {table}: tenant_id adicionado com sucesso")
            else:
                errors += 1
                logger.error(f"  {table}: ERRO ao adicionar tenant_id")

        conn.commit()

    logger.info(f"\nMigracao concluida!")
    logger.info(f"  Adicionados: {added}")
    logger.info(f"  Pulados: {skipped}")
    logger.info(f"  Erros: {errors}")

    return errors == 0


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == "--check":
            success = check_only()
            sys.exit(0 if success else 1)
        elif sys.argv[1] == "--help":
            print(__doc__)
            sys.exit(0)

    success = upgrade()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
