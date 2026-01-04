#!/usr/bin/env python3
"""
Plataforma E - Script de Migracao SQLite -> PostgreSQL

Este script migra todos os dados do SQLite para o PostgreSQL.

Uso:
    1. Certifique-se de que o PostgreSQL esta rodando:
       docker-compose up -d postgres

    2. Execute o script:
       python migrate_sqlite_to_postgres.py

Requisitos:
    - psycopg2-binary (pip install psycopg2-binary)
    - SQLite database existente em factory/database/factory.db
"""

import os
import sys

# Configurar encoding antes de importar psycopg2
os.environ['PGCLIENTENCODING'] = 'UTF8'
os.environ['PYTHONIOENCODING'] = 'utf-8'

# Fix para Windows cp1252
if sys.platform == 'win32':
    import locale
    locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

import sqlite3
import pg8000.dbapi as pg8000
from pathlib import Path
from datetime import datetime

# Configuracoes
SQLITE_PATH = Path(__file__).parent / "factory" / "database" / "factory.db"
POSTGRES_CONFIG = {
    "host": "localhost",
    "port": 5433,
    "database": "fabrica_db",
    "user": "fabrica",
    "password": "fabrica_secret",
    "client_encoding": "utf8"
}

# Ordem das tabelas para migrar (respeitando foreign keys)
TABLES_ORDER = [
    "projects",
    "epics",
    "sprints",
    "stories",
    "story_tasks",
    "story_documentation",
    "agents",
    "skills",
    "agent_skills",
    "activity_logs",
    "chat_messages",
    "attachments",
    "job_queue",
]

def get_sqlite_connection():
    """Conecta ao SQLite"""
    return sqlite3.connect(SQLITE_PATH)

def get_postgres_connection():
    """Conecta ao PostgreSQL usando pg8000"""
    return pg8000.connect(
        user=POSTGRES_CONFIG['user'],
        password=POSTGRES_CONFIG['password'],
        host=POSTGRES_CONFIG['host'],
        port=POSTGRES_CONFIG['port'],
        database=POSTGRES_CONFIG['database']
    )

def get_table_columns(cursor, table_name, is_postgres=False):
    """Obtem colunas de uma tabela"""
    if is_postgres:
        cursor.execute("""
            SELECT column_name
            FROM information_schema.columns
            WHERE table_name = %s
            ORDER BY ordinal_position
        """, (table_name,))
        return [row[0] for row in cursor.fetchall()]
    else:
        cursor.execute(f"PRAGMA table_info({table_name})")
        return [row[1] for row in cursor.fetchall()]

def check_table_exists(cursor, table_name, is_postgres=False):
    """Verifica se tabela existe"""
    if is_postgres:
        cursor.execute("""
            SELECT EXISTS (
                SELECT FROM information_schema.tables
                WHERE table_name = %s
            )
        """, (table_name,))
        return cursor.fetchone()[0]
    else:
        cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name=?",
            (table_name,)
        )
        return cursor.fetchone() is not None

def create_postgres_tables(pg_conn):
    """Cria tabelas no PostgreSQL usando SQLAlchemy"""
    print("\n[1/4] Criando tabelas no PostgreSQL...")

    # Importar e criar tabelas via SQLAlchemy
    import os
    os.environ["DATABASE_URL"] = f"postgresql+asyncpg://{POSTGRES_CONFIG['user']}:{POSTGRES_CONFIG['password']}@{POSTGRES_CONFIG['host']}:{POSTGRES_CONFIG['port']}/{POSTGRES_CONFIG['database']}"
    os.environ["DATABASE_URL_SYNC"] = f"postgresql://{POSTGRES_CONFIG['user']}:{POSTGRES_CONFIG['password']}@{POSTGRES_CONFIG['host']}:{POSTGRES_CONFIG['port']}/{POSTGRES_CONFIG['database']}"

    from factory.database.connection import sync_engine, Base
    from factory.database import models  # noqa: F401 - importa os modelos

    # Drop all tables first to ensure clean state
    Base.metadata.drop_all(bind=sync_engine)
    Base.metadata.create_all(bind=sync_engine)

    print("   Tabelas criadas com sucesso!")

def migrate_table(sqlite_conn, pg_conn, table_name):
    """Migra dados de uma tabela"""
    sqlite_cursor = sqlite_conn.cursor()
    pg_cursor = pg_conn.cursor()

    # Verificar se tabela existe no SQLite
    if not check_table_exists(sqlite_cursor, table_name, is_postgres=False):
        print(f"   - {table_name}: SKIP (nao existe no SQLite)")
        return 0

    # Verificar se tabela existe no PostgreSQL
    if not check_table_exists(pg_cursor, table_name, is_postgres=True):
        print(f"   - {table_name}: SKIP (nao existe no PostgreSQL)")
        return 0

    # Obter colunas comuns
    sqlite_cols = set(get_table_columns(sqlite_cursor, table_name, is_postgres=False))
    pg_cols = set(get_table_columns(pg_cursor, table_name, is_postgres=True))
    common_cols = sqlite_cols & pg_cols

    if not common_cols:
        print(f"   - {table_name}: SKIP (sem colunas comuns)")
        return 0

    cols_list = list(common_cols)
    cols_str = ", ".join(cols_list)

    # Buscar dados do SQLite
    sqlite_cursor.execute(f"SELECT {cols_str} FROM {table_name}")
    rows = sqlite_cursor.fetchall()

    if not rows:
        print(f"   - {table_name}: 0 registros (tabela vazia)")
        return 0

    # Inserir no PostgreSQL
    placeholders = ", ".join(["%s"] * len(cols_list))
    insert_sql = f"INSERT INTO {table_name} ({cols_str}) VALUES ({placeholders})"

    # Limpar tabela antes de inserir
    pg_cursor.execute(f"TRUNCATE TABLE {table_name} CASCADE")

    success_count = 0
    error_count = 0

    for row in rows:
        try:
            # Converter valores None para NULL e ajustar tipos
            converted_row = []
            for val in row:
                if isinstance(val, str) and val.startswith('{') and val.endswith('}'):
                    # Possivelmente JSON armazenado como string
                    converted_row.append(val)
                else:
                    converted_row.append(val)

            pg_cursor.execute(insert_sql, converted_row)
            success_count += 1
        except Exception as e:
            error_count += 1
            if error_count <= 3:  # Mostrar apenas primeiros 3 erros
                print(f"      Erro: {str(e)[:100]}")

    pg_conn.commit()

    status = "OK" if error_count == 0 else f"OK ({error_count} erros)"
    print(f"   - {table_name}: {success_count} registros {status}")

    return success_count

def reset_sequences(pg_conn):
    """Reseta sequences do PostgreSQL para continuar apos ultimo ID"""
    print("\n[3/4] Ajustando sequences...")

    pg_cursor = pg_conn.cursor()

    # Tabelas com sequences (normalmente tabelas com id serial/autoincrement)
    tables_with_sequences = [
        ("projects", "id"),
        ("epics", "id"),
        ("sprints", "id"),
        ("stories", "id"),
        ("story_tasks", "id"),
        ("story_documentation", "id"),
        ("agents", "id"),
        ("skills", "id"),
        ("activity_logs", "id"),
        ("chat_messages", "id"),
        ("attachments", "id"),
        ("job_queue", "id"),
    ]

    for table, pk_col in tables_with_sequences:
        try:
            # Verificar se tabela existe
            if not check_table_exists(pg_cursor, table, is_postgres=True):
                continue

            # Obter max ID
            pg_cursor.execute(f"SELECT MAX({pk_col}) FROM {table}")
            max_id = pg_cursor.fetchone()[0]

            if max_id:
                # Descobrir nome da sequence
                pg_cursor.execute(f"""
                    SELECT pg_get_serial_sequence('{table}', '{pk_col}')
                """)
                seq_name = pg_cursor.fetchone()[0]

                if seq_name:
                    pg_cursor.execute(f"SELECT setval('{seq_name}', {max_id})")
                    print(f"   - {table}: sequence ajustada para {max_id}")
        except Exception as e:
            print(f"   - {table}: SKIP ({str(e)[:50]})")

    pg_conn.commit()

def verify_migration(sqlite_conn, pg_conn):
    """Verifica se migracao foi bem sucedida"""
    print("\n[4/4] Verificando migracao...")

    sqlite_cursor = sqlite_conn.cursor()
    pg_cursor = pg_conn.cursor()

    all_ok = True

    for table in TABLES_ORDER:
        if not check_table_exists(sqlite_cursor, table, is_postgres=False):
            continue
        if not check_table_exists(pg_cursor, table, is_postgres=True):
            continue

        sqlite_cursor.execute(f"SELECT COUNT(*) FROM {table}")
        sqlite_count = sqlite_cursor.fetchone()[0]

        pg_cursor.execute(f"SELECT COUNT(*) FROM {table}")
        pg_count = pg_cursor.fetchone()[0]

        status = "OK" if sqlite_count == pg_count else "DIFF"
        if sqlite_count != pg_count:
            all_ok = False

        print(f"   - {table}: SQLite={sqlite_count}, PostgreSQL={pg_count} [{status}]")

    return all_ok

def main():
    print("=" * 60)
    print("  MIGRACAO SQLite -> PostgreSQL")
    print("  Plataforma E v4.0")
    print("=" * 60)

    # Verificar SQLite
    if not SQLITE_PATH.exists():
        print(f"\nERRO: Banco SQLite nao encontrado: {SQLITE_PATH}")
        return False

    print(f"\nOrigem: {SQLITE_PATH}")
    print(f"Destino: postgresql://{POSTGRES_CONFIG['host']}:{POSTGRES_CONFIG['port']}/{POSTGRES_CONFIG['database']}")

    try:
        # Conectar aos bancos
        print("\nConectando aos bancos de dados...")
        sqlite_conn = get_sqlite_connection()
        pg_conn = get_postgres_connection()
        print("   Conexoes estabelecidas!")

        # Criar tabelas no PostgreSQL
        create_postgres_tables(pg_conn)

        # Reconectar apos criar tabelas
        pg_conn.close()
        pg_conn = get_postgres_connection()

        # Migrar dados
        print("\n[2/4] Migrando dados...")
        total_records = 0
        for table in TABLES_ORDER:
            count = migrate_table(sqlite_conn, pg_conn, table)
            total_records += count

        print(f"\n   Total de registros migrados: {total_records}")

        # Reset sequences
        reset_sequences(pg_conn)

        # Verificar
        success = verify_migration(sqlite_conn, pg_conn)

        # Fechar conexoes
        sqlite_conn.close()
        pg_conn.close()

        print("\n" + "=" * 60)
        if success:
            print("  MIGRACAO CONCLUIDA COM SUCESSO!")
            print("  ")
            print("  Proximos passos:")
            print("  1. Teste a aplicacao com PostgreSQL")
            print("  2. Mantenha o SQLite como backup")
        else:
            print("  MIGRACAO CONCLUIDA COM DIFERENCAS")
            print("  Verifique os logs acima para detalhes.")
        print("=" * 60)

        return success

    except pg8000.InterfaceError as e:
        print(f"\nERRO de conexao PostgreSQL: {e}")
        print("\nVerifique se o PostgreSQL esta rodando:")
        print("  docker-compose up -d postgres")
        return False
    except Exception as e:
        print(f"\nERRO durante migracao: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
