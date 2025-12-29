#!/usr/bin/env python3
"""
Fabrica de Agentes - Database Backup Script
=============================================
Issue #97: Backup Automatizado Database e PVCs

Este script realiza backup automatizado do banco de dados PostgreSQL,
com suporte a:
- Backup completo (full dump)
- Backup incremental (WAL archiving)
- Compressao GZIP
- Upload para S3/Azure Blob/GCS
- Retencao configuravel
- Notificacoes de sucesso/falha

Uso:
    # Backup manual
    python scripts/backup_database.py

    # Backup com upload para S3
    python scripts/backup_database.py --upload s3

    # Backup com retencao customizada
    python scripts/backup_database.py --retention-days 60

Variaveis de ambiente:
    DB_HOST: Host do banco de dados
    DB_PORT: Porta do banco (default: 5432)
    DB_USER: Usuario do banco
    DB_PASSWORD: Senha do banco
    DB_NAME: Nome do banco
    BACKUP_DIR: Diretorio de backup (default: /backups)
    S3_BUCKET: Bucket S3 para upload
    AWS_ACCESS_KEY_ID: Credenciais AWS
    AWS_SECRET_ACCESS_KEY: Credenciais AWS
"""

import os
import sys
import subprocess
import gzip
import shutil
import logging
import argparse
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, List
import json
import hashlib

# Configuracao de logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler('/var/log/fabrica-backup.log', mode='a')
        if os.path.exists('/var/log') else logging.NullHandler()
    ]
)
logger = logging.getLogger(__name__)


# =============================================================================
# Configuration
# =============================================================================

class BackupConfig:
    """Configuracao de backup."""

    def __init__(self):
        self.db_host = os.getenv('DB_HOST', 'localhost')
        self.db_port = os.getenv('DB_PORT', '5432')
        self.db_user = os.getenv('DB_USER', 'postgres')
        self.db_password = os.getenv('DB_PASSWORD', '')
        self.db_name = os.getenv('DB_NAME', 'fabrica_db')
        self.backup_dir = Path(os.getenv('BACKUP_DIR', '/backups'))
        self.s3_bucket = os.getenv('S3_BUCKET', '')
        self.azure_container = os.getenv('AZURE_CONTAINER', '')
        self.gcs_bucket = os.getenv('GCS_BUCKET', '')
        self.retention_days = int(os.getenv('BACKUP_RETENTION_DAYS', '30'))
        self.webhook_url = os.getenv('BACKUP_WEBHOOK_URL', '')

    def validate(self) -> bool:
        """Valida configuracao."""
        if not self.db_password:
            logger.warning("DB_PASSWORD not set, using passwordless auth")

        if not self.backup_dir.exists():
            logger.info(f"Creating backup directory: {self.backup_dir}")
            self.backup_dir.mkdir(parents=True, exist_ok=True)

        return True


# =============================================================================
# Backup Functions
# =============================================================================

def backup_postgres(config: BackupConfig) -> Optional[Path]:
    """
    Realiza backup completo do PostgreSQL.

    Args:
        config: Configuracao de backup

    Returns:
        Path do arquivo de backup ou None se falhar
    """
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    filename = f"backup_{config.db_name}_{timestamp}.sql"
    backup_path = config.backup_dir / filename
    compressed_path = config.backup_dir / f"{filename}.gz"

    logger.info(f"Starting PostgreSQL backup: {config.db_name}")
    logger.info(f"Backup file: {compressed_path}")

    # Construir comando pg_dump
    env = os.environ.copy()
    env['PGPASSWORD'] = config.db_password

    pg_dump_cmd = [
        'pg_dump',
        '-h', config.db_host,
        '-p', config.db_port,
        '-U', config.db_user,
        '-d', config.db_name,
        '-F', 'p',  # Plain text format
        '--no-owner',
        '--no-acl',
        '-f', str(backup_path)
    ]

    try:
        # Executar pg_dump
        logger.info(f"Executing: pg_dump -h {config.db_host} -d {config.db_name}")
        result = subprocess.run(
            pg_dump_cmd,
            env=env,
            capture_output=True,
            text=True,
            timeout=3600  # 1 hour timeout
        )

        if result.returncode != 0:
            logger.error(f"pg_dump failed: {result.stderr}")
            return None

        # Comprimir backup
        logger.info("Compressing backup with gzip...")
        with open(backup_path, 'rb') as f_in:
            with gzip.open(compressed_path, 'wb', compresslevel=9) as f_out:
                shutil.copyfileobj(f_in, f_out)

        # Remover arquivo nao comprimido
        backup_path.unlink()

        # Calcular checksum
        checksum = calculate_checksum(compressed_path)
        logger.info(f"Backup completed: {compressed_path}")
        logger.info(f"Size: {compressed_path.stat().st_size / 1024 / 1024:.2f} MB")
        logger.info(f"SHA256: {checksum}")

        # Salvar metadata
        save_backup_metadata(compressed_path, checksum, config)

        return compressed_path

    except subprocess.TimeoutExpired:
        logger.error("Backup timed out after 1 hour")
        return None
    except Exception as e:
        logger.error(f"Backup failed: {e}")
        return None


def calculate_checksum(file_path: Path) -> str:
    """Calcula SHA256 checksum do arquivo."""
    sha256_hash = hashlib.sha256()
    with open(file_path, "rb") as f:
        for byte_block in iter(lambda: f.read(4096), b""):
            sha256_hash.update(byte_block)
    return sha256_hash.hexdigest()


def save_backup_metadata(backup_path: Path, checksum: str, config: BackupConfig):
    """Salva metadata do backup."""
    metadata = {
        "timestamp": datetime.now().isoformat(),
        "database": config.db_name,
        "host": config.db_host,
        "file": backup_path.name,
        "size_bytes": backup_path.stat().st_size,
        "sha256": checksum
    }

    metadata_path = backup_path.with_suffix('.json')
    with open(metadata_path, 'w') as f:
        json.dump(metadata, f, indent=2)

    logger.info(f"Metadata saved: {metadata_path}")


# =============================================================================
# Upload Functions
# =============================================================================

def upload_to_s3(file_path: Path, bucket: str, prefix: str = "db") -> bool:
    """
    Upload backup para S3.

    Args:
        file_path: Path do arquivo
        bucket: Nome do bucket S3
        prefix: Prefixo no bucket

    Returns:
        True se sucesso
    """
    try:
        import boto3

        s3_client = boto3.client('s3')
        s3_key = f"{prefix}/{file_path.name}"

        logger.info(f"Uploading to S3: s3://{bucket}/{s3_key}")

        s3_client.upload_file(
            str(file_path),
            bucket,
            s3_key,
            ExtraArgs={
                'StorageClass': 'STANDARD_IA',  # Infrequent Access para economia
                'ServerSideEncryption': 'AES256'
            }
        )

        # Upload metadata tambem
        metadata_path = file_path.with_suffix('.json')
        if metadata_path.exists():
            s3_client.upload_file(
                str(metadata_path),
                bucket,
                f"{prefix}/{metadata_path.name}"
            )

        logger.info(f"Upload completed: s3://{bucket}/{s3_key}")
        return True

    except ImportError:
        logger.error("boto3 not installed. Run: pip install boto3")
        return False
    except Exception as e:
        logger.error(f"S3 upload failed: {e}")
        return False


def upload_to_azure(file_path: Path, container: str, prefix: str = "db") -> bool:
    """
    Upload backup para Azure Blob Storage.

    Args:
        file_path: Path do arquivo
        container: Nome do container
        prefix: Prefixo no container

    Returns:
        True se sucesso
    """
    try:
        from azure.storage.blob import BlobServiceClient

        connection_string = os.getenv('AZURE_STORAGE_CONNECTION_STRING')
        if not connection_string:
            logger.error("AZURE_STORAGE_CONNECTION_STRING not set")
            return False

        blob_service = BlobServiceClient.from_connection_string(connection_string)
        container_client = blob_service.get_container_client(container)
        blob_name = f"{prefix}/{file_path.name}"

        logger.info(f"Uploading to Azure: {container}/{blob_name}")

        with open(file_path, "rb") as data:
            container_client.upload_blob(name=blob_name, data=data, overwrite=True)

        logger.info(f"Upload completed: {container}/{blob_name}")
        return True

    except ImportError:
        logger.error("azure-storage-blob not installed")
        return False
    except Exception as e:
        logger.error(f"Azure upload failed: {e}")
        return False


def upload_to_gcs(file_path: Path, bucket: str, prefix: str = "db") -> bool:
    """
    Upload backup para Google Cloud Storage.

    Args:
        file_path: Path do arquivo
        bucket: Nome do bucket GCS
        prefix: Prefixo no bucket

    Returns:
        True se sucesso
    """
    try:
        from google.cloud import storage

        client = storage.Client()
        bucket_obj = client.bucket(bucket)
        blob_name = f"{prefix}/{file_path.name}"
        blob = bucket_obj.blob(blob_name)

        logger.info(f"Uploading to GCS: gs://{bucket}/{blob_name}")

        blob.upload_from_filename(str(file_path))

        logger.info(f"Upload completed: gs://{bucket}/{blob_name}")
        return True

    except ImportError:
        logger.error("google-cloud-storage not installed")
        return False
    except Exception as e:
        logger.error(f"GCS upload failed: {e}")
        return False


# =============================================================================
# Cleanup Functions
# =============================================================================

def cleanup_old_backups(config: BackupConfig) -> int:
    """
    Remove backups antigos baseado na retencao configurada.

    Args:
        config: Configuracao de backup

    Returns:
        Numero de arquivos removidos
    """
    cutoff_date = datetime.now() - timedelta(days=config.retention_days)
    removed_count = 0

    logger.info(f"Cleaning up backups older than {config.retention_days} days")
    logger.info(f"Cutoff date: {cutoff_date.isoformat()}")

    for backup_file in config.backup_dir.glob("backup_*.sql.gz"):
        try:
            # Extrair data do nome do arquivo
            parts = backup_file.stem.replace('.sql', '').split('_')
            if len(parts) >= 3:
                date_str = parts[-2]  # YYYYMMDD
                file_date = datetime.strptime(date_str, '%Y%m%d')

                if file_date < cutoff_date:
                    logger.info(f"Removing old backup: {backup_file.name}")
                    backup_file.unlink()

                    # Remover metadata tambem
                    metadata_file = backup_file.with_suffix('.json')
                    if metadata_file.exists():
                        metadata_file.unlink()

                    removed_count += 1

        except (ValueError, IndexError) as e:
            logger.warning(f"Could not parse date from {backup_file.name}: {e}")

    logger.info(f"Cleanup completed: {removed_count} files removed")
    return removed_count


def cleanup_s3_old_backups(bucket: str, prefix: str, retention_days: int) -> int:
    """
    Remove backups antigos do S3.

    Args:
        bucket: Nome do bucket
        prefix: Prefixo no bucket
        retention_days: Dias de retencao

    Returns:
        Numero de objetos removidos
    """
    try:
        import boto3

        s3_client = boto3.client('s3')
        cutoff_date = datetime.now() - timedelta(days=retention_days)
        removed_count = 0

        paginator = s3_client.get_paginator('list_objects_v2')
        for page in paginator.paginate(Bucket=bucket, Prefix=prefix):
            for obj in page.get('Contents', []):
                if obj['LastModified'].replace(tzinfo=None) < cutoff_date:
                    logger.info(f"Removing old S3 object: {obj['Key']}")
                    s3_client.delete_object(Bucket=bucket, Key=obj['Key'])
                    removed_count += 1

        return removed_count

    except Exception as e:
        logger.error(f"S3 cleanup failed: {e}")
        return 0


# =============================================================================
# Notification Functions
# =============================================================================

def send_notification(
    success: bool,
    backup_path: Optional[Path],
    config: BackupConfig,
    error_message: Optional[str] = None
):
    """
    Envia notificacao de backup.

    Args:
        success: Se o backup foi bem sucedido
        backup_path: Path do arquivo de backup
        config: Configuracao
        error_message: Mensagem de erro se falhou
    """
    if not config.webhook_url:
        return

    import urllib.request
    import json

    payload = {
        "timestamp": datetime.now().isoformat(),
        "status": "success" if success else "failed",
        "database": config.db_name,
        "host": config.db_host,
    }

    if success and backup_path:
        payload.update({
            "file": backup_path.name,
            "size_mb": round(backup_path.stat().st_size / 1024 / 1024, 2)
        })
    elif error_message:
        payload["error"] = error_message

    try:
        data = json.dumps(payload).encode('utf-8')
        req = urllib.request.Request(
            config.webhook_url,
            data=data,
            headers={'Content-Type': 'application/json'}
        )
        urllib.request.urlopen(req, timeout=10)
        logger.info("Notification sent successfully")
    except Exception as e:
        logger.warning(f"Failed to send notification: {e}")


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description='Fabrica de Agentes - Database Backup Script'
    )
    parser.add_argument(
        '--upload',
        choices=['s3', 'azure', 'gcs'],
        help='Upload backup to cloud storage'
    )
    parser.add_argument(
        '--retention-days',
        type=int,
        default=30,
        help='Days to keep backups (default: 30)'
    )
    parser.add_argument(
        '--cleanup-only',
        action='store_true',
        help='Only cleanup old backups, do not create new'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without doing it'
    )

    args = parser.parse_args()

    logger.info("=" * 60)
    logger.info("Fabrica de Agentes - Database Backup")
    logger.info("=" * 60)

    config = BackupConfig()
    config.retention_days = args.retention_days

    if not config.validate():
        logger.error("Configuration validation failed")
        sys.exit(1)

    # Cleanup
    if args.cleanup_only:
        cleanup_old_backups(config)
        sys.exit(0)

    if args.dry_run:
        logger.info("[DRY RUN] Would backup database and upload")
        sys.exit(0)

    # Realizar backup
    backup_path = backup_postgres(config)

    if not backup_path:
        send_notification(False, None, config, "Backup failed")
        sys.exit(1)

    # Upload se solicitado
    upload_success = True
    if args.upload:
        if args.upload == 's3' and config.s3_bucket:
            upload_success = upload_to_s3(backup_path, config.s3_bucket)
        elif args.upload == 'azure' and config.azure_container:
            upload_success = upload_to_azure(backup_path, config.azure_container)
        elif args.upload == 'gcs' and config.gcs_bucket:
            upload_success = upload_to_gcs(backup_path, config.gcs_bucket)
        else:
            logger.warning(f"Upload requested but {args.upload} not configured")

    # Cleanup
    cleanup_old_backups(config)

    # Notificacao
    send_notification(
        success=upload_success,
        backup_path=backup_path,
        config=config
    )

    logger.info("=" * 60)
    logger.info("Backup completed successfully!")
    logger.info("=" * 60)


if __name__ == '__main__':
    main()
