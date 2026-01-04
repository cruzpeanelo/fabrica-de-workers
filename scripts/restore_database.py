#!/usr/bin/env python3
"""
Plataforma E - Database Restore Script
=============================================
Issue #97: Backup Automatizado Database e PVCs

Este script restaura backup do banco de dados PostgreSQL,
com suporte a:
- Restauracao de backup local ou remoto (S3/Azure/GCS)
- Verificacao de integridade (checksum)
- Restauracao em banco diferente (para testes)
- Modo dry-run

Uso:
    # Restaurar ultimo backup local
    python scripts/restore_database.py

    # Restaurar backup especifico
    python scripts/restore_database.py --file backup_fabrica_db_20241229_120000.sql.gz

    # Restaurar de S3
    python scripts/restore_database.py --from-s3 s3://bucket/db/backup.sql.gz

    # Restaurar em banco de teste
    python scripts/restore_database.py --target-db fabrica_db_test

Variaveis de ambiente:
    DB_HOST: Host do banco de dados
    DB_PORT: Porta do banco (default: 5432)
    DB_USER: Usuario do banco
    DB_PASSWORD: Senha do banco
    DB_NAME: Nome do banco (target)
    BACKUP_DIR: Diretorio de backup (default: /backups)

ATENCAO: Este script APAGA todos os dados do banco de destino!
"""

import os
import sys
import subprocess
import gzip
import shutil
import logging
import argparse
import hashlib
import json
from datetime import datetime
from pathlib import Path
from typing import Optional, Tuple
import tempfile

# Configuracao de logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


# =============================================================================
# Configuration
# =============================================================================

class RestoreConfig:
    """Configuracao de restauracao."""

    def __init__(self):
        self.db_host = os.getenv('DB_HOST', 'localhost')
        self.db_port = os.getenv('DB_PORT', '5432')
        self.db_user = os.getenv('DB_USER', 'postgres')
        self.db_password = os.getenv('DB_PASSWORD', '')
        self.db_name = os.getenv('DB_NAME', 'fabrica_db')
        self.backup_dir = Path(os.getenv('BACKUP_DIR', '/backups'))

    def validate(self) -> bool:
        """Valida configuracao."""
        if not self.db_password:
            logger.warning("DB_PASSWORD not set, using passwordless auth")
        return True


# =============================================================================
# Download Functions
# =============================================================================

def download_from_s3(s3_path: str, local_path: Path) -> bool:
    """
    Download backup do S3.

    Args:
        s3_path: Caminho S3 (s3://bucket/key)
        local_path: Path local para salvar

    Returns:
        True se sucesso
    """
    try:
        import boto3

        # Parse S3 path
        parts = s3_path.replace('s3://', '').split('/', 1)
        bucket = parts[0]
        key = parts[1] if len(parts) > 1 else ''

        s3_client = boto3.client('s3')
        logger.info(f"Downloading from S3: {s3_path}")

        s3_client.download_file(bucket, key, str(local_path))

        logger.info(f"Download completed: {local_path}")
        return True

    except ImportError:
        logger.error("boto3 not installed. Run: pip install boto3")
        return False
    except Exception as e:
        logger.error(f"S3 download failed: {e}")
        return False


def download_from_azure(azure_path: str, local_path: Path) -> bool:
    """
    Download backup do Azure Blob Storage.

    Args:
        azure_path: Caminho Azure (container/blob)
        local_path: Path local para salvar

    Returns:
        True se sucesso
    """
    try:
        from azure.storage.blob import BlobServiceClient

        connection_string = os.getenv('AZURE_STORAGE_CONNECTION_STRING')
        if not connection_string:
            logger.error("AZURE_STORAGE_CONNECTION_STRING not set")
            return False

        parts = azure_path.split('/', 1)
        container = parts[0]
        blob_name = parts[1] if len(parts) > 1 else ''

        blob_service = BlobServiceClient.from_connection_string(connection_string)
        blob_client = blob_service.get_blob_client(container, blob_name)

        logger.info(f"Downloading from Azure: {azure_path}")

        with open(local_path, "wb") as f:
            download_stream = blob_client.download_blob()
            f.write(download_stream.readall())

        logger.info(f"Download completed: {local_path}")
        return True

    except ImportError:
        logger.error("azure-storage-blob not installed")
        return False
    except Exception as e:
        logger.error(f"Azure download failed: {e}")
        return False


# =============================================================================
# Validation Functions
# =============================================================================

def verify_checksum(backup_path: Path) -> bool:
    """
    Verifica checksum do backup usando arquivo de metadata.

    Args:
        backup_path: Path do arquivo de backup

    Returns:
        True se checksum valido
    """
    metadata_path = backup_path.with_suffix('.json')

    if not metadata_path.exists():
        logger.warning("Metadata file not found, skipping checksum verification")
        return True

    try:
        with open(metadata_path) as f:
            metadata = json.load(f)

        expected_checksum = metadata.get('sha256')
        if not expected_checksum:
            logger.warning("No checksum in metadata, skipping verification")
            return True

        logger.info("Verifying backup integrity...")

        sha256_hash = hashlib.sha256()
        with open(backup_path, "rb") as f:
            for byte_block in iter(lambda: f.read(4096), b""):
                sha256_hash.update(byte_block)

        actual_checksum = sha256_hash.hexdigest()

        if actual_checksum != expected_checksum:
            logger.error(f"Checksum mismatch!")
            logger.error(f"Expected: {expected_checksum}")
            logger.error(f"Actual: {actual_checksum}")
            return False

        logger.info("Checksum verified successfully")
        return True

    except Exception as e:
        logger.error(f"Checksum verification failed: {e}")
        return False


def find_latest_backup(backup_dir: Path) -> Optional[Path]:
    """
    Encontra o backup mais recente no diretorio.

    Args:
        backup_dir: Diretorio de backups

    Returns:
        Path do backup mais recente ou None
    """
    backups = list(backup_dir.glob("backup_*.sql.gz"))

    if not backups:
        logger.error(f"No backups found in {backup_dir}")
        return None

    # Ordenar por data de modificacao
    backups.sort(key=lambda x: x.stat().st_mtime, reverse=True)

    latest = backups[0]
    logger.info(f"Found latest backup: {latest.name}")
    return latest


# =============================================================================
# Restore Functions
# =============================================================================

def decompress_backup(compressed_path: Path) -> Path:
    """
    Descomprime arquivo de backup.

    Args:
        compressed_path: Path do arquivo comprimido

    Returns:
        Path do arquivo descomprimido
    """
    decompressed_path = compressed_path.with_suffix('')  # Remove .gz

    logger.info(f"Decompressing: {compressed_path.name}")

    with gzip.open(compressed_path, 'rb') as f_in:
        with open(decompressed_path, 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)

    logger.info(f"Decompressed to: {decompressed_path.name}")
    return decompressed_path


def drop_existing_connections(config: RestoreConfig, target_db: str):
    """
    Termina conexoes existentes no banco de destino.

    Args:
        config: Configuracao
        target_db: Nome do banco de destino
    """
    logger.info(f"Terminating existing connections to {target_db}")

    env = os.environ.copy()
    env['PGPASSWORD'] = config.db_password

    # Conectar ao postgres para terminar conexoes
    terminate_sql = f"""
    SELECT pg_terminate_backend(pid)
    FROM pg_stat_activity
    WHERE datname = '{target_db}'
    AND pid <> pg_backend_pid();
    """

    cmd = [
        'psql',
        '-h', config.db_host,
        '-p', config.db_port,
        '-U', config.db_user,
        '-d', 'postgres',
        '-c', terminate_sql
    ]

    try:
        subprocess.run(cmd, env=env, capture_output=True, timeout=30)
    except Exception as e:
        logger.warning(f"Could not terminate connections: {e}")


def restore_postgres(
    config: RestoreConfig,
    backup_path: Path,
    target_db: str,
    drop_existing: bool = True
) -> bool:
    """
    Restaura backup no PostgreSQL.

    Args:
        config: Configuracao
        backup_path: Path do arquivo de backup (descomprimido)
        target_db: Nome do banco de destino
        drop_existing: Se deve dropar banco existente

    Returns:
        True se sucesso
    """
    env = os.environ.copy()
    env['PGPASSWORD'] = config.db_password

    # Terminar conexoes existentes
    drop_existing_connections(config, target_db)

    try:
        if drop_existing:
            # Dropar banco existente
            logger.info(f"Dropping existing database: {target_db}")
            drop_cmd = [
                'dropdb',
                '-h', config.db_host,
                '-p', config.db_port,
                '-U', config.db_user,
                '--if-exists',
                target_db
            ]
            subprocess.run(drop_cmd, env=env, capture_output=True)

        # Criar banco novo
        logger.info(f"Creating database: {target_db}")
        create_cmd = [
            'createdb',
            '-h', config.db_host,
            '-p', config.db_port,
            '-U', config.db_user,
            target_db
        ]
        result = subprocess.run(create_cmd, env=env, capture_output=True, text=True)

        if result.returncode != 0 and 'already exists' not in result.stderr:
            logger.error(f"Failed to create database: {result.stderr}")
            return False

        # Restaurar backup
        logger.info(f"Restoring backup to {target_db}...")
        restore_cmd = [
            'psql',
            '-h', config.db_host,
            '-p', config.db_port,
            '-U', config.db_user,
            '-d', target_db,
            '-f', str(backup_path),
            '-v', 'ON_ERROR_STOP=1'
        ]

        result = subprocess.run(
            restore_cmd,
            env=env,
            capture_output=True,
            text=True,
            timeout=3600  # 1 hour timeout
        )

        if result.returncode != 0:
            logger.error(f"Restore failed: {result.stderr}")
            return False

        logger.info("Restore completed successfully!")
        return True

    except subprocess.TimeoutExpired:
        logger.error("Restore timed out after 1 hour")
        return False
    except Exception as e:
        logger.error(f"Restore failed: {e}")
        return False


def verify_restore(config: RestoreConfig, target_db: str) -> bool:
    """
    Verifica se a restauracao foi bem sucedida.

    Args:
        config: Configuracao
        target_db: Nome do banco restaurado

    Returns:
        True se verificacao passou
    """
    logger.info("Verifying restored database...")

    env = os.environ.copy()
    env['PGPASSWORD'] = config.db_password

    # Verificar se consegue conectar e listar tabelas
    verify_sql = """
    SELECT schemaname, tablename
    FROM pg_tables
    WHERE schemaname = 'public'
    ORDER BY tablename;
    """

    cmd = [
        'psql',
        '-h', config.db_host,
        '-p', config.db_port,
        '-U', config.db_user,
        '-d', target_db,
        '-c', verify_sql
    ]

    try:
        result = subprocess.run(cmd, env=env, capture_output=True, text=True, timeout=30)

        if result.returncode != 0:
            logger.error(f"Verification failed: {result.stderr}")
            return False

        # Contar tabelas
        lines = [l for l in result.stdout.split('\n') if l.strip() and '|' in l]
        table_count = len(lines) - 1  # Excluir header

        if table_count > 0:
            logger.info(f"Verification passed: {table_count} tables found")
            return True
        else:
            logger.warning("No tables found in restored database")
            return False

    except Exception as e:
        logger.error(f"Verification failed: {e}")
        return False


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description='Plataforma E - Database Restore Script'
    )
    parser.add_argument(
        '--file',
        type=str,
        help='Specific backup file to restore'
    )
    parser.add_argument(
        '--from-s3',
        type=str,
        help='S3 path to download backup from (s3://bucket/key)'
    )
    parser.add_argument(
        '--from-azure',
        type=str,
        help='Azure path to download backup from (container/blob)'
    )
    parser.add_argument(
        '--target-db',
        type=str,
        help='Target database name (default: use DB_NAME)'
    )
    parser.add_argument(
        '--no-verify',
        action='store_true',
        help='Skip checksum verification'
    )
    parser.add_argument(
        '--keep-existing',
        action='store_true',
        help='Do not drop existing database'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without doing it'
    )
    parser.add_argument(
        '-y', '--yes',
        action='store_true',
        help='Skip confirmation prompt'
    )

    args = parser.parse_args()

    logger.info("=" * 60)
    logger.info("Plataforma E - Database Restore")
    logger.info("=" * 60)

    config = RestoreConfig()

    if not config.validate():
        logger.error("Configuration validation failed")
        sys.exit(1)

    target_db = args.target_db or config.db_name

    # Determinar arquivo de backup
    backup_path = None
    temp_dir = None

    try:
        if args.from_s3:
            temp_dir = tempfile.mkdtemp()
            backup_path = Path(temp_dir) / "backup.sql.gz"
            if not download_from_s3(args.from_s3, backup_path):
                sys.exit(1)

        elif args.from_azure:
            temp_dir = tempfile.mkdtemp()
            backup_path = Path(temp_dir) / "backup.sql.gz"
            if not download_from_azure(args.from_azure, backup_path):
                sys.exit(1)

        elif args.file:
            backup_path = config.backup_dir / args.file
            if not backup_path.exists():
                backup_path = Path(args.file)
            if not backup_path.exists():
                logger.error(f"Backup file not found: {args.file}")
                sys.exit(1)

        else:
            backup_path = find_latest_backup(config.backup_dir)
            if not backup_path:
                sys.exit(1)

        logger.info(f"Backup file: {backup_path}")
        logger.info(f"Target database: {target_db}")
        logger.info(f"Target host: {config.db_host}:{config.db_port}")

        if args.dry_run:
            logger.info("[DRY RUN] Would restore backup to database")
            sys.exit(0)

        # Confirmacao
        if not args.yes:
            logger.warning("")
            logger.warning("=" * 60)
            logger.warning("WARNING: This will ERASE all data in the target database!")
            logger.warning("=" * 60)
            logger.warning("")

            confirm = input(f"Are you sure you want to restore to '{target_db}'? (yes/no): ")
            if confirm.lower() != 'yes':
                logger.info("Restore cancelled")
                sys.exit(0)

        # Verificar checksum
        if not args.no_verify:
            if not verify_checksum(backup_path):
                logger.error("Checksum verification failed, aborting")
                sys.exit(1)

        # Descomprimir se necessario
        if backup_path.suffix == '.gz':
            sql_path = decompress_backup(backup_path)
            cleanup_sql = True
        else:
            sql_path = backup_path
            cleanup_sql = False

        try:
            # Restaurar
            if not restore_postgres(
                config,
                sql_path,
                target_db,
                drop_existing=not args.keep_existing
            ):
                sys.exit(1)

            # Verificar
            if not verify_restore(config, target_db):
                logger.warning("Restore verification had issues")

        finally:
            # Limpar arquivo descomprimido
            if cleanup_sql and sql_path.exists():
                sql_path.unlink()

        logger.info("=" * 60)
        logger.info("Restore completed successfully!")
        logger.info("=" * 60)

    finally:
        # Limpar diretorio temporario
        if temp_dir and Path(temp_dir).exists():
            shutil.rmtree(temp_dir)


if __name__ == '__main__':
    main()
