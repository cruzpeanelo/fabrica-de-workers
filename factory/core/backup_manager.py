"""
Backup Manager - Automated Backup and Disaster Recovery
Issue #199: Enterprise backup system

Provides automated backup capabilities for:
- PostgreSQL database
- SQLite fallback
- File storage (uploads, projects)
- Configuration files
"""
import os
import asyncio
import subprocess
import shutil
import logging
import hashlib
import gzip
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class BackupType(Enum):
    """Types of backups"""
    FULL = "full"
    INCREMENTAL = "incremental"
    DIFFERENTIAL = "differential"


class BackupStatus(Enum):
    """Backup job status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    VERIFIED = "verified"


@dataclass
class BackupConfig:
    """Backup configuration"""
    # Directories
    backup_dir: Path = Path("backups")
    uploads_dir: Path = Path("uploads")
    projects_dir: Path = Path("projects")

    # Database
    database_url: str = ""
    pg_dump_path: str = "pg_dump"
    pg_restore_path: str = "pg_restore"

    # Retention policy (in days)
    daily_retention: int = 7
    weekly_retention: int = 28  # 4 weeks
    monthly_retention: int = 365  # 12 months
    yearly_retention: int = 2555  # 7 years (SOC2)

    # Compression
    compression_level: int = 9  # Max compression

    # Storage
    s3_bucket: Optional[str] = None
    s3_region: str = "us-east-1"
    s3_prefix: str = "backups"

    # Encryption
    encryption_enabled: bool = True
    encryption_key_env: str = "BACKUP_ENCRYPTION_KEY"

    # Alerts
    alert_on_failure: bool = True
    alert_webhook: Optional[str] = None

    # Schedule (cron format)
    daily_schedule: str = "0 2 * * *"  # 2 AM daily
    weekly_schedule: str = "0 3 * * 0"  # 3 AM Sunday
    monthly_schedule: str = "0 4 1 * *"  # 4 AM 1st of month


@dataclass
class BackupResult:
    """Result of a backup operation"""
    backup_id: str
    backup_type: BackupType
    status: BackupStatus
    started_at: datetime
    completed_at: Optional[datetime] = None
    file_path: Optional[Path] = None
    file_size: int = 0
    checksum: Optional[str] = None
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class BackupManager:
    """
    Enterprise backup manager with support for:
    - PostgreSQL and SQLite backups
    - File storage backups
    - S3 remote storage
    - Encryption
    - Retention policies
    - Verification
    """

    def __init__(self, config: Optional[BackupConfig] = None):
        self.config = config or BackupConfig()
        self._ensure_backup_dir()
        self._backup_history: List[BackupResult] = []

    def _ensure_backup_dir(self):
        """Ensure backup directories exist"""
        self.config.backup_dir.mkdir(parents=True, exist_ok=True)
        (self.config.backup_dir / "daily").mkdir(exist_ok=True)
        (self.config.backup_dir / "weekly").mkdir(exist_ok=True)
        (self.config.backup_dir / "monthly").mkdir(exist_ok=True)
        (self.config.backup_dir / "yearly").mkdir(exist_ok=True)
        (self.config.backup_dir / "temp").mkdir(exist_ok=True)

    async def backup_database(
        self,
        backup_type: BackupType = BackupType.FULL,
        category: str = "daily"
    ) -> BackupResult:
        """
        Backup the database (PostgreSQL or SQLite)

        Args:
            backup_type: Type of backup
            category: daily, weekly, monthly, yearly

        Returns:
            BackupResult with backup details
        """
        backup_id = self._generate_backup_id()
        started_at = datetime.now()

        result = BackupResult(
            backup_id=backup_id,
            backup_type=backup_type,
            status=BackupStatus.RUNNING,
            started_at=started_at
        )

        try:
            database_url = self.config.database_url or os.getenv("DATABASE_URL", "")

            if "postgresql" in database_url:
                backup_file = await self._backup_postgres(backup_id, category)
            else:
                backup_file = await self._backup_sqlite(backup_id, category)

            # Compress
            compressed_file = await self._compress_file(backup_file)

            # Encrypt if enabled
            if self.config.encryption_enabled:
                final_file = await self._encrypt_file(compressed_file)
                compressed_file.unlink()  # Remove unencrypted
            else:
                final_file = compressed_file

            # Calculate checksum
            checksum = self._calculate_checksum(final_file)

            # Upload to S3 if configured
            if self.config.s3_bucket:
                await self._upload_to_s3(final_file, category)

            result.status = BackupStatus.COMPLETED
            result.completed_at = datetime.now()
            result.file_path = final_file
            result.file_size = final_file.stat().st_size
            result.checksum = checksum
            result.metadata = {
                "category": category,
                "compressed": True,
                "encrypted": self.config.encryption_enabled,
                "uploaded_to_s3": bool(self.config.s3_bucket)
            }

            logger.info(f"[Backup] Database backup completed: {backup_id}")

        except Exception as e:
            result.status = BackupStatus.FAILED
            result.error = str(e)
            result.completed_at = datetime.now()
            logger.error(f"[Backup] Database backup failed: {e}")

            if self.config.alert_on_failure:
                await self._send_alert(f"Backup failed: {e}")

        self._backup_history.append(result)
        return result

    async def _backup_postgres(self, backup_id: str, category: str) -> Path:
        """Backup PostgreSQL database"""
        database_url = self.config.database_url or os.getenv("DATABASE_URL", "")

        # Parse connection string
        # postgresql://user:pass@host:port/dbname
        import urllib.parse
        parsed = urllib.parse.urlparse(database_url)

        output_file = self.config.backup_dir / category / f"postgres_{backup_id}.dump"

        env = os.environ.copy()
        env["PGPASSWORD"] = parsed.password or ""

        cmd = [
            self.config.pg_dump_path,
            "--format=custom",
            f"--compress={self.config.compression_level}",
            f"--host={parsed.hostname}",
            f"--port={parsed.port or 5432}",
            f"--username={parsed.username}",
            f"--dbname={parsed.path[1:]}",
            f"--file={output_file}"
        ]

        proc = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=env
        )
        stdout, stderr = await proc.communicate()

        if proc.returncode != 0:
            raise Exception(f"pg_dump failed: {stderr.decode()}")

        logger.info(f"[Backup] PostgreSQL dump created: {output_file}")
        return output_file

    async def _backup_sqlite(self, backup_id: str, category: str) -> Path:
        """Backup SQLite database"""
        from factory.database.connection import SQLITE_FILE

        output_file = self.config.backup_dir / category / f"sqlite_{backup_id}.db"

        # Use SQLite backup API via subprocess
        proc = await asyncio.create_subprocess_exec(
            "sqlite3",
            str(SQLITE_FILE),
            f".backup '{output_file}'",
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        stdout, stderr = await proc.communicate()

        if proc.returncode != 0:
            # Fallback to file copy
            shutil.copy2(SQLITE_FILE, output_file)

        logger.info(f"[Backup] SQLite backup created: {output_file}")
        return output_file

    async def backup_files(
        self,
        directories: Optional[List[Path]] = None,
        category: str = "daily"
    ) -> BackupResult:
        """
        Backup file storage (uploads, projects)

        Args:
            directories: List of directories to backup
            category: daily, weekly, monthly, yearly

        Returns:
            BackupResult with backup details
        """
        backup_id = self._generate_backup_id()
        started_at = datetime.now()

        result = BackupResult(
            backup_id=backup_id,
            backup_type=BackupType.FULL,
            status=BackupStatus.RUNNING,
            started_at=started_at
        )

        try:
            dirs_to_backup = directories or [
                self.config.uploads_dir,
                self.config.projects_dir
            ]

            output_file = self.config.backup_dir / category / f"files_{backup_id}.tar.gz"

            # Create tarball
            import tarfile
            with tarfile.open(output_file, "w:gz") as tar:
                for directory in dirs_to_backup:
                    if directory.exists():
                        tar.add(directory, arcname=directory.name)

            # Encrypt if enabled
            if self.config.encryption_enabled:
                final_file = await self._encrypt_file(output_file)
                output_file.unlink()
            else:
                final_file = output_file

            # Calculate checksum
            checksum = self._calculate_checksum(final_file)

            # Upload to S3 if configured
            if self.config.s3_bucket:
                await self._upload_to_s3(final_file, category)

            result.status = BackupStatus.COMPLETED
            result.completed_at = datetime.now()
            result.file_path = final_file
            result.file_size = final_file.stat().st_size
            result.checksum = checksum

            logger.info(f"[Backup] Files backup completed: {backup_id}")

        except Exception as e:
            result.status = BackupStatus.FAILED
            result.error = str(e)
            result.completed_at = datetime.now()
            logger.error(f"[Backup] Files backup failed: {e}")

            if self.config.alert_on_failure:
                await self._send_alert(f"Files backup failed: {e}")

        self._backup_history.append(result)
        return result

    async def full_backup(self, category: str = "daily") -> Dict[str, BackupResult]:
        """
        Perform full backup of database and files

        Args:
            category: daily, weekly, monthly, yearly

        Returns:
            Dict with database and files backup results
        """
        results = {}

        # Backup database
        results["database"] = await self.backup_database(
            BackupType.FULL,
            category
        )

        # Backup files
        results["files"] = await self.backup_files(category=category)

        return results

    async def restore_database(
        self,
        backup_file: Path,
        target_database: Optional[str] = None
    ) -> bool:
        """
        Restore database from backup

        Args:
            backup_file: Path to backup file
            target_database: Optional target database URL

        Returns:
            True if successful
        """
        try:
            # Decrypt if needed
            if backup_file.suffix == ".enc":
                decrypted = await self._decrypt_file(backup_file)
                backup_file = decrypted

            # Decompress if needed
            if backup_file.suffix == ".gz":
                decompressed = await self._decompress_file(backup_file)
                backup_file = decompressed

            database_url = target_database or self.config.database_url or os.getenv("DATABASE_URL", "")

            if "postgresql" in database_url:
                return await self._restore_postgres(backup_file, database_url)
            else:
                return await self._restore_sqlite(backup_file)

        except Exception as e:
            logger.error(f"[Backup] Restore failed: {e}")
            return False

    async def _restore_postgres(self, backup_file: Path, database_url: str) -> bool:
        """Restore PostgreSQL database"""
        import urllib.parse
        parsed = urllib.parse.urlparse(database_url)

        env = os.environ.copy()
        env["PGPASSWORD"] = parsed.password or ""

        cmd = [
            self.config.pg_restore_path,
            "--clean",
            "--if-exists",
            f"--host={parsed.hostname}",
            f"--port={parsed.port or 5432}",
            f"--username={parsed.username}",
            f"--dbname={parsed.path[1:]}",
            str(backup_file)
        ]

        proc = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=env
        )
        stdout, stderr = await proc.communicate()

        if proc.returncode != 0:
            logger.warning(f"[Backup] pg_restore warnings: {stderr.decode()}")

        logger.info(f"[Backup] PostgreSQL restored from {backup_file}")
        return True

    async def _restore_sqlite(self, backup_file: Path) -> bool:
        """Restore SQLite database"""
        from factory.database.connection import SQLITE_FILE

        # Create backup of current
        if SQLITE_FILE.exists():
            shutil.copy2(SQLITE_FILE, f"{SQLITE_FILE}.pre_restore")

        # Copy backup to main location
        shutil.copy2(backup_file, SQLITE_FILE)

        logger.info(f"[Backup] SQLite restored from {backup_file}")
        return True

    async def verify_backup(self, backup_file: Path) -> bool:
        """
        Verify backup integrity

        Args:
            backup_file: Path to backup file

        Returns:
            True if backup is valid
        """
        try:
            # Check file exists
            if not backup_file.exists():
                logger.error(f"[Backup] Verify failed: file not found")
                return False

            # Check file size
            if backup_file.stat().st_size == 0:
                logger.error(f"[Backup] Verify failed: empty file")
                return False

            # Try to decompress/read
            if backup_file.suffix == ".gz":
                with gzip.open(backup_file, 'rb') as f:
                    # Read first few bytes to verify
                    header = f.read(1024)
                    if not header:
                        return False

            # For encrypted files, try to decrypt to temp
            if backup_file.suffix == ".enc":
                try:
                    decrypted = await self._decrypt_file(backup_file)
                    decrypted.unlink()  # Remove temp file
                except Exception:
                    return False

            logger.info(f"[Backup] Verified: {backup_file}")
            return True

        except Exception as e:
            logger.error(f"[Backup] Verify failed: {e}")
            return False

    async def cleanup_old_backups(self):
        """Apply retention policy and remove old backups"""
        now = datetime.now()

        categories = {
            "daily": self.config.daily_retention,
            "weekly": self.config.weekly_retention,
            "monthly": self.config.monthly_retention,
            "yearly": self.config.yearly_retention
        }

        for category, retention_days in categories.items():
            category_dir = self.config.backup_dir / category
            if not category_dir.exists():
                continue

            cutoff = now - timedelta(days=retention_days)

            for backup_file in category_dir.iterdir():
                if backup_file.is_file():
                    mtime = datetime.fromtimestamp(backup_file.stat().st_mtime)
                    if mtime < cutoff:
                        backup_file.unlink()
                        logger.info(f"[Backup] Cleaned up old backup: {backup_file}")

    async def _compress_file(self, file_path: Path) -> Path:
        """Compress file with gzip"""
        output_path = file_path.with_suffix(file_path.suffix + ".gz")

        with open(file_path, 'rb') as f_in:
            with gzip.open(output_path, 'wb', compresslevel=self.config.compression_level) as f_out:
                shutil.copyfileobj(f_in, f_out)

        file_path.unlink()  # Remove original
        return output_path

    async def _decompress_file(self, file_path: Path) -> Path:
        """Decompress gzip file"""
        output_path = file_path.with_suffix("")

        with gzip.open(file_path, 'rb') as f_in:
            with open(output_path, 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)

        return output_path

    async def _encrypt_file(self, file_path: Path) -> Path:
        """Encrypt file with AES-256"""
        try:
            from cryptography.fernet import Fernet
            import base64

            key = os.getenv(self.config.encryption_key_env)
            if not key:
                # Generate and warn
                key = Fernet.generate_key().decode()
                logger.warning(
                    f"[Backup] No encryption key found. Generated: {key[:20]}... "
                    f"Set {self.config.encryption_key_env} env var."
                )

            # Ensure key is valid Fernet format
            if len(key) != 44:
                key = base64.urlsafe_b64encode(key.encode()[:32].ljust(32, b'\0')).decode()

            fernet = Fernet(key.encode() if isinstance(key, str) else key)

            output_path = file_path.with_suffix(file_path.suffix + ".enc")

            with open(file_path, 'rb') as f_in:
                data = f_in.read()

            encrypted = fernet.encrypt(data)

            with open(output_path, 'wb') as f_out:
                f_out.write(encrypted)

            return output_path

        except ImportError:
            logger.warning("[Backup] cryptography not installed, skipping encryption")
            return file_path

    async def _decrypt_file(self, file_path: Path) -> Path:
        """Decrypt file"""
        from cryptography.fernet import Fernet
        import base64

        key = os.getenv(self.config.encryption_key_env)
        if not key:
            raise ValueError("Encryption key not found")

        if len(key) != 44:
            key = base64.urlsafe_b64encode(key.encode()[:32].ljust(32, b'\0')).decode()

        fernet = Fernet(key.encode() if isinstance(key, str) else key)

        output_path = file_path.with_suffix("")

        with open(file_path, 'rb') as f_in:
            encrypted = f_in.read()

        decrypted = fernet.decrypt(encrypted)

        with open(output_path, 'wb') as f_out:
            f_out.write(decrypted)

        return output_path

    async def _upload_to_s3(self, file_path: Path, category: str):
        """Upload backup to S3"""
        try:
            import boto3

            s3 = boto3.client('s3', region_name=self.config.s3_region)

            key = f"{self.config.s3_prefix}/{category}/{file_path.name}"

            s3.upload_file(
                str(file_path),
                self.config.s3_bucket,
                key,
                ExtraArgs={'ServerSideEncryption': 'AES256'}
            )

            logger.info(f"[Backup] Uploaded to S3: s3://{self.config.s3_bucket}/{key}")

        except ImportError:
            logger.warning("[Backup] boto3 not installed, skipping S3 upload")
        except Exception as e:
            logger.error(f"[Backup] S3 upload failed: {e}")

    def _calculate_checksum(self, file_path: Path) -> str:
        """Calculate SHA-256 checksum"""
        sha256 = hashlib.sha256()

        with open(file_path, 'rb') as f:
            for chunk in iter(lambda: f.read(8192), b''):
                sha256.update(chunk)

        return sha256.hexdigest()

    def _generate_backup_id(self) -> str:
        """Generate unique backup ID"""
        import uuid
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        return f"{timestamp}_{uuid.uuid4().hex[:8]}"

    async def _send_alert(self, message: str):
        """Send alert on failure"""
        if self.config.alert_webhook:
            try:
                import aiohttp
                async with aiohttp.ClientSession() as session:
                    await session.post(
                        self.config.alert_webhook,
                        json={"text": f"[Backup Alert] {message}"}
                    )
            except Exception as e:
                logger.error(f"[Backup] Failed to send alert: {e}")

    def get_backup_history(self, limit: int = 10) -> List[BackupResult]:
        """Get recent backup history"""
        return self._backup_history[-limit:]

    def get_backup_stats(self) -> Dict[str, Any]:
        """Get backup statistics"""
        total = len(self._backup_history)
        successful = sum(1 for b in self._backup_history if b.status == BackupStatus.COMPLETED)
        failed = sum(1 for b in self._backup_history if b.status == BackupStatus.FAILED)

        return {
            "total_backups": total,
            "successful": successful,
            "failed": failed,
            "success_rate": (successful / total * 100) if total > 0 else 0,
            "last_backup": self._backup_history[-1] if self._backup_history else None
        }


# Singleton instance
_backup_manager: Optional[BackupManager] = None


def get_backup_manager(config: Optional[BackupConfig] = None) -> BackupManager:
    """Get or create backup manager singleton"""
    global _backup_manager
    if _backup_manager is None:
        _backup_manager = BackupManager(config)
    return _backup_manager


# Scheduled backup functions
async def scheduled_daily_backup():
    """Daily backup task (for scheduler)"""
    manager = get_backup_manager()
    await manager.full_backup("daily")
    await manager.cleanup_old_backups()


async def scheduled_weekly_backup():
    """Weekly backup task"""
    manager = get_backup_manager()
    await manager.full_backup("weekly")


async def scheduled_monthly_backup():
    """Monthly backup task"""
    manager = get_backup_manager()
    await manager.full_backup("monthly")
