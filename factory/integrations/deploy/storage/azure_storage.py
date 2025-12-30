# -*- coding: utf-8 -*-
"""
Azure Blob Storage
==================
Implementacao de storage em Azure Blob Storage para backups.

Issue #362 - Terminal A

Requer: azure-storage-blob
pip install azure-storage-blob
"""

import json
import logging
from datetime import datetime
from typing import Any, Dict, List, Optional

from .base import BackupStorage, StorageConfig, StorageMetadata

logger = logging.getLogger(__name__)


class AzureBlobStorage(BackupStorage):
    """
    Storage de backups em Azure Blob Storage.

    Estrutura de blobs:
    {container}/
    ├── backups/
    │   ├── {tenant_id}/
    │   │   ├── {integration}/
    │   │   │   ├── {environment}/
    │   │   │   │   ├── {backup_id}/
    │   │   │   │   │   ├── data.bin
    │   │   │   │   │   └── metadata.json

    Exemplo:
        config = StorageConfig(
            storage_type="azure",
            azure_connection_string="DefaultEndpointsProtocol=https;...",
            azure_container="backups",
            retention_days=30
        )
        storage = AzureBlobStorage(config)
        await storage.initialize()

        # Salvar backup
        metadata = await storage.save_backup(...)
    """

    def __init__(self, config: StorageConfig):
        super().__init__(config)
        self._client = None
        self._container_client = None
        self._container = config.azure_container
        self._prefix = "backups"

    async def initialize(self) -> bool:
        """Inicializa cliente Azure Blob Storage"""
        try:
            from azure.storage.blob import BlobServiceClient

            self._client = BlobServiceClient.from_connection_string(
                self.config.azure_connection_string
            )
            self._container_client = self._client.get_container_client(self._container)

            # Cria container se nao existir
            try:
                self._container_client.create_container()
            except Exception:
                pass  # Container ja existe

            self._initialized = True
            logger.info(f"AzureBlobStorage initialized with container {self._container}")
            return True

        except ImportError:
            logger.error("azure-storage-blob not installed. Run: pip install azure-storage-blob")
            return False
        except Exception as e:
            logger.error(f"Failed to initialize AzureBlobStorage: {e}")
            return False

    def _get_blob_name(
        self,
        tenant_id: str,
        integration: str,
        environment: str,
        backup_id: str,
        filename: str
    ) -> str:
        """Constroi nome do blob"""
        return f"{self._prefix}/{tenant_id}/{integration}/{environment}/{backup_id}/{filename}"

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
        """Salva backup no Azure Blob Storage"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        # Verifica tamanho
        size_bytes = len(data)
        max_bytes = self.config.max_backup_size_mb * 1024 * 1024
        if size_bytes > max_bytes:
            raise ValueError(f"Backup size {size_bytes} exceeds limit {max_bytes}")

        # Comprime se configurado
        if self.config.compression_enabled:
            data = self._compress_data(data)

        # Calcula checksum
        checksum = self._calculate_checksum(data)

        # Upload dos dados
        data_blob_name = self._get_blob_name(tenant_id, integration, environment, backup_id, "data.bin")
        blob_client = self._container_client.get_blob_client(data_blob_name)
        blob_client.upload_blob(
            data,
            overwrite=True,
            metadata={
                "backup_id": backup_id,
                "tenant_id": tenant_id,
                "checksum": checksum
            }
        )

        # Cria metadados
        storage_path = f"azure://{self._container}/{data_blob_name}"
        storage_metadata = StorageMetadata(
            backup_id=backup_id,
            tenant_id=tenant_id,
            request_id=request_id,
            integration=integration,
            environment=environment,
            storage_path=storage_path,
            size_bytes=len(data),
            checksum=checksum,
            created_at=datetime.utcnow(),
            expires_at=self._get_expiration_date(),
            compressed=self.config.compression_enabled,
            encrypted=self.config.encryption_enabled,
            metadata=metadata or {}
        )

        # Upload dos metadados
        metadata_blob_name = self._get_blob_name(tenant_id, integration, environment, backup_id, "metadata.json")
        metadata_blob = self._container_client.get_blob_client(metadata_blob_name)
        metadata_blob.upload_blob(
            json.dumps(storage_metadata.to_dict()),
            overwrite=True,
            content_settings={"content_type": "application/json"}
        )

        logger.info(f"Backup {backup_id} saved to azure://{self._container}/{data_blob_name}")
        return storage_metadata

    async def fetch_backup(self, backup_id: str, tenant_id: str) -> Optional[bytes]:
        """Busca backup do Azure Blob Storage"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return None

        try:
            # Extrai blob name do storage_path
            blob_name = metadata.storage_path.replace(f"azure://{self._container}/", "")
            blob_client = self._container_client.get_blob_client(blob_name)

            download = blob_client.download_blob()
            data = download.readall()

            # Verifica checksum
            if self._calculate_checksum(data) != metadata.checksum:
                logger.error(f"Backup {backup_id} checksum mismatch")
                return None

            # Descomprime se necessario
            if metadata.compressed:
                data = self._decompress_data(data)

            return data

        except Exception as e:
            logger.error(f"Error fetching backup {backup_id}: {e}")
            return None

    async def get_metadata(self, backup_id: str, tenant_id: str) -> Optional[StorageMetadata]:
        """Busca metadados de um backup"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        prefix = f"{self._prefix}/{tenant_id}/"

        try:
            blob_list = self._container_client.list_blobs(name_starts_with=prefix)
            for blob in blob_list:
                if blob.name.endswith(f"/{backup_id}/metadata.json"):
                    blob_client = self._container_client.get_blob_client(blob.name)
                    download = blob_client.download_blob()
                    data = json.loads(download.readall().decode())
                    return StorageMetadata.from_dict(data)

        except Exception as e:
            logger.error(f"Error getting metadata for {backup_id}: {e}")

        return None

    async def list_backups(
        self,
        tenant_id: str,
        integration: Optional[str] = None,
        environment: Optional[str] = None,
        limit: int = 100
    ) -> List[StorageMetadata]:
        """Lista backups no Azure Blob Storage"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        backups = []
        prefix = f"{self._prefix}/{tenant_id}/"

        if integration:
            prefix = f"{prefix}{integration}/"
            if environment:
                prefix = f"{prefix}{environment}/"

        try:
            blob_list = self._container_client.list_blobs(name_starts_with=prefix)
            for blob in blob_list:
                if blob.name.endswith("/metadata.json"):
                    try:
                        blob_client = self._container_client.get_blob_client(blob.name)
                        download = blob_client.download_blob()
                        data = json.loads(download.readall().decode())
                        backups.append(StorageMetadata.from_dict(data))

                        if len(backups) >= limit:
                            break

                    except Exception as e:
                        logger.warning(f"Error reading metadata {blob.name}: {e}")

        except Exception as e:
            logger.error(f"Error listing backups: {e}")

        # Ordena por data de criacao (mais recente primeiro)
        backups.sort(key=lambda x: x.created_at, reverse=True)
        return backups[:limit]

    async def delete_backup(self, backup_id: str, tenant_id: str) -> bool:
        """Deleta backup do Azure Blob Storage"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return False

        try:
            # Extrai prefixo do backup
            data_blob = metadata.storage_path.replace(f"azure://{self._container}/", "")
            metadata_blob = data_blob.replace("/data.bin", "/metadata.json")

            # Deleta ambos blobs
            data_client = self._container_client.get_blob_client(data_blob)
            data_client.delete_blob()

            metadata_client = self._container_client.get_blob_client(metadata_blob)
            metadata_client.delete_blob()

            logger.info(f"Backup {backup_id} deleted from Azure")
            return True

        except Exception as e:
            logger.error(f"Error deleting backup {backup_id}: {e}")
            return False

    async def cleanup_expired(self) -> int:
        """Remove backups expirados do Azure Blob Storage"""
        if not self._container_client:
            raise RuntimeError("AzureBlobStorage not initialized")

        removed = 0
        now = datetime.utcnow()

        try:
            blob_list = self._container_client.list_blobs(name_starts_with=self._prefix)
            for blob in blob_list:
                if blob.name.endswith("/metadata.json"):
                    try:
                        blob_client = self._container_client.get_blob_client(blob.name)
                        download = blob_client.download_blob()
                        data = json.loads(download.readall().decode())
                        metadata = StorageMetadata.from_dict(data)

                        if metadata.expires_at and now > metadata.expires_at:
                            # Deleta backup expirado
                            data_blob = blob.name.replace("/metadata.json", "/data.bin")

                            data_client = self._container_client.get_blob_client(data_blob)
                            data_client.delete_blob()

                            blob_client.delete_blob()
                            removed += 1
                            logger.info(f"Expired backup {metadata.backup_id} removed")

                    except Exception as e:
                        logger.warning(f"Error checking backup {blob.name}: {e}")

        except Exception as e:
            logger.error(f"Error during cleanup: {e}")

        return removed

    async def get_storage_info(self) -> Dict[str, Any]:
        """Retorna informacoes do storage Azure"""
        info = await super().get_storage_info()
        info["container"] = self._container
        info["prefix"] = self._prefix

        if self._container_client:
            try:
                # Conta blobs e tamanho total
                total_size = 0
                backup_count = 0

                blob_list = self._container_client.list_blobs(
                    name_starts_with=self._prefix,
                    include=["metadata"]
                )
                for blob in blob_list:
                    if blob.name.endswith("/data.bin"):
                        total_size += blob.size
                        backup_count += 1

                info["total_size_bytes"] = total_size
                info["total_size_mb"] = round(total_size / (1024 * 1024), 2)
                info["backup_count"] = backup_count

            except Exception as e:
                logger.warning(f"Error getting Azure stats: {e}")

        return info
