# -*- coding: utf-8 -*-
"""
Local File Storage
==================
Implementacao de storage local para backups.

Issue #362 - Terminal A

Uso recomendado para desenvolvimento e testes.
Para producao, use S3Storage ou AzureBlobStorage.
"""

import json
import logging
import os
import shutil
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from .base import BackupStorage, StorageConfig, StorageMetadata

logger = logging.getLogger(__name__)


class LocalFileStorage(BackupStorage):
    """
    Storage de backups em arquivos locais.

    Estrutura de diretorios:
    {base_path}/
    ├── {tenant_id}/
    │   ├── {integration}/
    │   │   ├── {environment}/
    │   │   │   ├── {backup_id}/
    │   │   │   │   ├── data.bin
    │   │   │   │   └── metadata.json

    Exemplo:
        config = StorageConfig(
            storage_type="local",
            base_path="./backups",
            retention_days=30
        )
        storage = LocalFileStorage(config)
        await storage.initialize()

        # Salvar backup
        metadata = await storage.save_backup(
            backup_id="bkp-001",
            tenant_id="tenant-1",
            request_id="req-123",
            integration="sap_s4",
            environment="production",
            data=backup_data
        )

        # Buscar backup
        data = await storage.fetch_backup("bkp-001", "tenant-1")
    """

    def __init__(self, config: StorageConfig):
        super().__init__(config)
        self._base_path = Path(config.base_path)

    async def initialize(self) -> bool:
        """Inicializa storage local"""
        try:
            self._base_path.mkdir(parents=True, exist_ok=True)
            self._initialized = True
            logger.info(f"LocalFileStorage initialized at {self._base_path}")
            return True
        except Exception as e:
            logger.error(f"Failed to initialize LocalFileStorage: {e}")
            return False

    async def save_backup(
        self,
        backup_id: str,
        tenant_id: str,
        request_id: str,
        integration: str,
        environment: str,
        data: bytes,
        metadata: Optional[Dict[str, Any]] = None
    ) -> StorageMetadata:
        """Salva backup em arquivo local"""
        # Verifica tamanho
        size_bytes = len(data)
        max_bytes = self.config.max_backup_size_mb * 1024 * 1024
        if size_bytes > max_bytes:
            raise ValueError(
                f"Backup size {size_bytes} exceeds limit {max_bytes}"
            )

        # Constroi caminho
        backup_path = self._get_backup_path(tenant_id, integration, environment, backup_id)
        backup_path.mkdir(parents=True, exist_ok=True)

        # Comprime se configurado
        if self.config.compression_enabled:
            data = self._compress_data(data)

        # Calcula checksum
        checksum = self._calculate_checksum(data)

        # Salva dados
        data_file = backup_path / "data.bin"
        data_file.write_bytes(data)

        # Cria metadados
        storage_metadata = StorageMetadata(
            backup_id=backup_id,
            tenant_id=tenant_id,
            request_id=request_id,
            integration=integration,
            environment=environment,
            storage_path=str(backup_path),
            size_bytes=len(data),
            checksum=checksum,
            created_at=datetime.utcnow(),
            expires_at=self._get_expiration_date(),
            compressed=self.config.compression_enabled,
            encrypted=self.config.encryption_enabled,
            metadata=metadata or {}
        )

        # Salva metadados
        metadata_file = backup_path / "metadata.json"
        metadata_file.write_text(json.dumps(storage_metadata.to_dict(), indent=2))

        logger.info(f"Backup {backup_id} saved to {backup_path}")
        return storage_metadata

    async def fetch_backup(self, backup_id: str, tenant_id: str) -> Optional[bytes]:
        """Busca backup por ID"""
        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return None

        backup_path = Path(metadata.storage_path)
        data_file = backup_path / "data.bin"

        if not data_file.exists():
            logger.warning(f"Backup data file not found: {data_file}")
            return None

        data = data_file.read_bytes()

        # Verifica checksum
        if self._calculate_checksum(data) != metadata.checksum:
            logger.error(f"Backup {backup_id} checksum mismatch")
            return None

        # Descomprime se necessario
        if metadata.compressed:
            data = self._decompress_data(data)

        return data

    async def get_metadata(self, backup_id: str, tenant_id: str) -> Optional[StorageMetadata]:
        """Busca metadados de um backup"""
        # Procura em todos os diretorios do tenant
        tenant_path = self._base_path / tenant_id
        if not tenant_path.exists():
            return None

        for integration_dir in tenant_path.iterdir():
            if not integration_dir.is_dir():
                continue
            for env_dir in integration_dir.iterdir():
                if not env_dir.is_dir():
                    continue
                backup_path = env_dir / backup_id
                if backup_path.exists():
                    metadata_file = backup_path / "metadata.json"
                    if metadata_file.exists():
                        data = json.loads(metadata_file.read_text())
                        return StorageMetadata.from_dict(data)

        return None

    async def list_backups(
        self,
        tenant_id: str,
        integration: Optional[str] = None,
        environment: Optional[str] = None,
        limit: int = 100
    ) -> List[StorageMetadata]:
        """Lista backups do tenant"""
        backups = []
        tenant_path = self._base_path / tenant_id

        if not tenant_path.exists():
            return backups

        for integration_dir in tenant_path.iterdir():
            if not integration_dir.is_dir():
                continue
            if integration and integration_dir.name != integration:
                continue

            for env_dir in integration_dir.iterdir():
                if not env_dir.is_dir():
                    continue
                if environment and env_dir.name != environment:
                    continue

                for backup_dir in env_dir.iterdir():
                    if not backup_dir.is_dir():
                        continue

                    metadata_file = backup_dir / "metadata.json"
                    if metadata_file.exists():
                        try:
                            data = json.loads(metadata_file.read_text())
                            backups.append(StorageMetadata.from_dict(data))
                        except Exception as e:
                            logger.warning(f"Error reading metadata {metadata_file}: {e}")

                    if len(backups) >= limit:
                        break
                if len(backups) >= limit:
                    break
            if len(backups) >= limit:
                break

        # Ordena por data de criacao (mais recente primeiro)
        backups.sort(key=lambda x: x.created_at, reverse=True)
        return backups[:limit]

    async def delete_backup(self, backup_id: str, tenant_id: str) -> bool:
        """Deleta backup"""
        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return False

        try:
            backup_path = Path(metadata.storage_path)
            if backup_path.exists():
                shutil.rmtree(backup_path)
                logger.info(f"Backup {backup_id} deleted")
                return True
        except Exception as e:
            logger.error(f"Error deleting backup {backup_id}: {e}")

        return False

    async def cleanup_expired(self) -> int:
        """Remove backups expirados"""
        removed = 0
        now = datetime.utcnow()

        if not self._base_path.exists():
            return 0

        for tenant_dir in self._base_path.iterdir():
            if not tenant_dir.is_dir():
                continue

            for integration_dir in tenant_dir.iterdir():
                if not integration_dir.is_dir():
                    continue

                for env_dir in integration_dir.iterdir():
                    if not env_dir.is_dir():
                        continue

                    for backup_dir in env_dir.iterdir():
                        if not backup_dir.is_dir():
                            continue

                        metadata_file = backup_dir / "metadata.json"
                        if metadata_file.exists():
                            try:
                                data = json.loads(metadata_file.read_text())
                                metadata = StorageMetadata.from_dict(data)

                                if metadata.expires_at and now > metadata.expires_at:
                                    shutil.rmtree(backup_dir)
                                    removed += 1
                                    logger.info(f"Expired backup {metadata.backup_id} removed")
                            except Exception as e:
                                logger.warning(f"Error checking backup {backup_dir}: {e}")

        return removed

    def _get_backup_path(
        self,
        tenant_id: str,
        integration: str,
        environment: str,
        backup_id: str
    ) -> Path:
        """Constroi caminho do backup"""
        return self._base_path / tenant_id / integration / environment / backup_id

    async def get_storage_info(self) -> Dict[str, Any]:
        """Retorna informacoes do storage local"""
        info = await super().get_storage_info()

        # Calcula uso de disco
        total_size = 0
        backup_count = 0

        if self._base_path.exists():
            for file in self._base_path.rglob("data.bin"):
                total_size += file.stat().st_size
                backup_count += 1

        info["disk_usage_bytes"] = total_size
        info["disk_usage_mb"] = round(total_size / (1024 * 1024), 2)
        info["backup_count"] = backup_count
        info["base_path"] = str(self._base_path)

        return info
