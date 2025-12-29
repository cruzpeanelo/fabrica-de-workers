# -*- coding: utf-8 -*-
"""
Migration: Add External Integration Fields
==========================================
Adiciona campos para integracao com sistemas externos (Jira, Azure DevOps, GitHub).

Campos adicionados na tabela 'stories':
- external_id: ID no sistema externo
- external_system: Nome do sistema (jira, azure_devops, github)
- external_url: URL para o item no sistema externo
- external_data: JSON com dados adicionais do sistema externo
- last_sync_at: Data/hora da ultima sincronizacao

Uso:
    python -m factory.database.migrations.add_external_integration_fields

Para reverter:
    python -m factory.database.migrations.add_external_integration_fields --rollback
"""

import sys
import logging
from pathlib import Path

# Adiciona o diretorio raiz ao path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from sqlalchemy import text
from factory.database.connection import get_engine, get_session

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def upgrade():
    """Adiciona campos de integracao externa"""
    engine = get_engine()

    # Campos a adicionar
    new_columns = [
        ("external_id", "VARCHAR(100)"),
        ("external_system", "VARCHAR(50)"),
        ("external_url", "VARCHAR(500)"),
        ("external_data", "TEXT"),  # JSON armazenado como TEXT no SQLite
        ("last_sync_at", "DATETIME")
    ]

    with engine.connect() as conn:
        # Verifica quais colunas ja existem
        result = conn.execute(text("PRAGMA table_info(stories)"))
        existing_columns = {row[1] for row in result.fetchall()}

        for col_name, col_type in new_columns:
            if col_name not in existing_columns:
                try:
                    conn.execute(text(f"ALTER TABLE stories ADD COLUMN {col_name} {col_type}"))
                    logger.info(f"Coluna '{col_name}' adicionada com sucesso")
                except Exception as e:
                    logger.warning(f"Erro ao adicionar coluna '{col_name}': {e}")
            else:
                logger.info(f"Coluna '{col_name}' ja existe")

        # Cria indice para external_id se nao existir
        try:
            conn.execute(text(
                "CREATE INDEX IF NOT EXISTS ix_stories_external_id ON stories (external_id)"
            ))
            logger.info("Indice 'ix_stories_external_id' criado/verificado")
        except Exception as e:
            logger.warning(f"Erro ao criar indice: {e}")

        conn.commit()

    logger.info("Migracao concluida com sucesso!")


def downgrade():
    """Remove campos de integracao externa (SQLite nao suporta DROP COLUMN facilmente)"""
    logger.warning(
        "SQLite nao suporta DROP COLUMN de forma nativa. "
        "Para reverter, seria necessario recriar a tabela."
    )
    logger.info("Para reverter manualmente:")
    logger.info("1. Crie uma nova tabela sem as colunas de integracao")
    logger.info("2. Copie os dados da tabela antiga")
    logger.info("3. Remova a tabela antiga e renomeie a nova")


def main():
    if len(sys.argv) > 1 and sys.argv[1] == "--rollback":
        downgrade()
    else:
        upgrade()


if __name__ == "__main__":
    main()
