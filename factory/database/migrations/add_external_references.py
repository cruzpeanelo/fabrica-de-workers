# -*- coding: utf-8 -*-
"""
Migration: Add external_references column to stories
=====================================================
Issue #401: Adiciona coluna external_references para sincronização com sistemas externos.

Coluna adicionada:
- external_references (JSONB/JSON): Formato {"jira": {"key": "PROJ-123", "synced_at": "..."}, ...}

Uso:
    python -m factory.database.migrations.add_external_references

Suporta: PostgreSQL (JSONB) e SQLite (TEXT como JSON)
"""

import sys
import os
import logging
from pathlib import Path

# Adiciona o diretorio raiz ao path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


def get_database_type():
    """Detecta o tipo de banco de dados"""
    db_url = os.getenv("DATABASE_URL", "")
    if "postgresql" in db_url:
        return "postgresql"
    return "sqlite"


def upgrade():
    """Adiciona coluna external_references à tabela stories"""
    from sqlalchemy import text
    from factory.database.connection import sync_engine as engine

    db_type = get_database_type()
    logger.info(f"Detectado banco de dados: {db_type}")

    with engine.connect() as conn:
        # Verificar se a coluna já existe
        if db_type == "postgresql":
            result = conn.execute(text("""
                SELECT column_name
                FROM information_schema.columns
                WHERE table_name = 'stories' AND column_name = 'external_references'
            """))
            column_exists = result.fetchone() is not None
        else:
            result = conn.execute(text("PRAGMA table_info(stories)"))
            columns = {row[1] for row in result.fetchall()}
            column_exists = 'external_references' in columns

        if column_exists:
            logger.info("Coluna 'external_references' já existe - nada a fazer")
            return

        # Adicionar coluna conforme o banco
        try:
            if db_type == "postgresql":
                # PostgreSQL: usar JSONB com default vazio
                conn.execute(text("""
                    ALTER TABLE stories
                    ADD COLUMN external_references JSONB DEFAULT '{}'::jsonb
                """))
                logger.info("Coluna 'external_references' (JSONB) adicionada ao PostgreSQL")
            else:
                # SQLite: usar TEXT (JSON serializado)
                conn.execute(text("""
                    ALTER TABLE stories
                    ADD COLUMN external_references TEXT DEFAULT '{}'
                """))
                logger.info("Coluna 'external_references' (TEXT) adicionada ao SQLite")

            conn.commit()
            logger.info("Migration concluída com sucesso!")

        except Exception as e:
            logger.error(f"Erro ao adicionar coluna: {e}")
            raise


def check_other_missing_columns():
    """Verifica e adiciona outras colunas que podem estar faltando"""
    from sqlalchemy import text
    from factory.database.connection import sync_engine as engine

    db_type = get_database_type()

    # Lista de colunas que podem estar faltando (adicionar conforme necessário)
    columns_to_check = [
        # (nome_coluna, tipo_postgres, tipo_sqlite, default)
        ("external_references", "JSONB", "TEXT", "'{}'"),
    ]

    with engine.connect() as conn:
        for col_name, pg_type, sqlite_type, default in columns_to_check:
            # Verificar se existe
            if db_type == "postgresql":
                result = conn.execute(text(f"""
                    SELECT column_name
                    FROM information_schema.columns
                    WHERE table_name = 'stories' AND column_name = '{col_name}'
                """))
                exists = result.fetchone() is not None
            else:
                result = conn.execute(text("PRAGMA table_info(stories)"))
                columns = {row[1] for row in result.fetchall()}
                exists = col_name in columns

            if not exists:
                col_type = pg_type if db_type == "postgresql" else sqlite_type
                default_clause = f"DEFAULT {default}" if default else ""
                try:
                    if db_type == "postgresql" and pg_type == "JSONB":
                        conn.execute(text(f"""
                            ALTER TABLE stories
                            ADD COLUMN {col_name} {col_type} {default_clause}::jsonb
                        """))
                    else:
                        conn.execute(text(f"""
                            ALTER TABLE stories
                            ADD COLUMN {col_name} {col_type} {default_clause}
                        """))
                    logger.info(f"Coluna '{col_name}' adicionada")
                except Exception as e:
                    logger.warning(f"Erro ao adicionar '{col_name}': {e}")

        conn.commit()


def main():
    """Função principal"""
    logger.info("=== Migration: Add external_references ===")

    try:
        upgrade()
        logger.info("=== Migration concluída com sucesso! ===")
        return 0
    except Exception as e:
        logger.error(f"Migration falhou: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
