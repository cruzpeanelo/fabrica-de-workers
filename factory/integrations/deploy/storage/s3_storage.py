# -*- coding: utf-8 -*-
"""
AWS S3 Storage
==============
Implementacao de storage em AWS S3 para backups.

Issue #362 - Terminal A

Requer: boto3
pip install boto3
"""

import json
import logging
from datetime import datetime
from typing import Any, Dict, List, Optional

from .base import BackupStorage, StorageConfig, StorageMetadata

logger = logging.getLogger(__name__)


class S3Storage(BackupStorage):
    """
    Storage de backups em AWS S3.

    Estrutura de objetos:
    {bucket}/
    ├── backups/
    │   ├── {tenant_id}/
    │   │   ├── {integration}/
    │   │   │   ├── {environment}/
    │   │   │   │   ├── {backup_id}/
    │   │   │   │   │   ├── data.bin
    │   │   │   │   │   └── metadata.json

    Exemplo:
        config = StorageConfig(
            storage_type="s3",
            aws_region="us-east-1",
            aws_bucket="my-backups",
            aws_access_key="AKIAXXXXXXXXX",
            aws_secret_key="secret",
            retention_days=30
        )
        storage = S3Storage(config)
        await storage.initialize()

        # Salvar backup
        metadata = await storage.save_backup(...)
    """

    def __init__(self, config: StorageConfig):
        super().__init__(config)
        self._client = None
        self._bucket = config.aws_bucket
        self._prefix = "backups"

    async def initialize(self) -> bool:
        """Inicializa cliente S3"""
        try:
            import boto3
            from botocore.config import Config

            boto_config = Config(
                region_name=self.config.aws_region,
                retries={"max_attempts": 3, "mode": "standard"}
            )

            self._client = boto3.client(
                "s3",
                aws_access_key_id=self.config.aws_access_key,
                aws_secret_access_key=self.config.aws_secret_key,
                config=boto_config
            )

            # Verifica se bucket existe
            self._client.head_bucket(Bucket=self._bucket)
            self._initialized = True
            logger.info(f"S3Storage initialized with bucket {self._bucket}")
            return True

        except ImportError:
            logger.error("boto3 not installed. Run: pip install boto3")
            return False
        except Exception as e:
            logger.error(f"Failed to initialize S3Storage: {e}")
            return False

    def _get_s3_key(
        self,
        tenant_id: str,
        integration: str,
        environment: str,
        backup_id: str,
        filename: str
    ) -> str:
        """Constroi chave S3"""
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
        """Salva backup no S3"""
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

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
        data_key = self._get_s3_key(tenant_id, integration, environment, backup_id, "data.bin")
        self._client.put_object(
            Bucket=self._bucket,
            Key=data_key,
            Body=data,
            ContentType="application/octet-stream",
            Metadata={
                "backup_id": backup_id,
                "tenant_id": tenant_id,
                "checksum": checksum
            }
        )

        # Cria metadados
        storage_path = f"s3://{self._bucket}/{data_key}"
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
        metadata_key = self._get_s3_key(tenant_id, integration, environment, backup_id, "metadata.json")
        self._client.put_object(
            Bucket=self._bucket,
            Key=metadata_key,
            Body=json.dumps(storage_metadata.to_dict()),
            ContentType="application/json"
        )

        logger.info(f"Backup {backup_id} saved to s3://{self._bucket}/{data_key}")
        return storage_metadata

    async def fetch_backup(self, backup_id: str, tenant_id: str) -> Optional[bytes]:
        """Busca backup do S3"""
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return None

        try:
            # Extrai key do storage_path
            # s3://bucket/prefix/... -> prefix/...
            key = metadata.storage_path.replace(f"s3://{self._bucket}/", "")

            response = self._client.get_object(Bucket=self._bucket, Key=key)
            data = response["Body"].read()

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
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

        # Procura em todos os prefixos do tenant
        prefix = f"{self._prefix}/{tenant_id}/"

        try:
            paginator = self._client.get_paginator("list_objects_v2")
            for page in paginator.paginate(Bucket=self._bucket, Prefix=prefix):
                for obj in page.get("Contents", []):
                    key = obj["Key"]
                    if key.endswith(f"/{backup_id}/metadata.json"):
                        response = self._client.get_object(Bucket=self._bucket, Key=key)
                        data = json.loads(response["Body"].read().decode())
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
        """Lista backups no S3"""
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

        backups = []
        prefix = f"{self._prefix}/{tenant_id}/"

        if integration:
            prefix = f"{prefix}{integration}/"
            if environment:
                prefix = f"{prefix}{environment}/"

        try:
            paginator = self._client.get_paginator("list_objects_v2")
            for page in paginator.paginate(Bucket=self._bucket, Prefix=prefix):
                for obj in page.get("Contents", []):
                    key = obj["Key"]
                    if key.endswith("/metadata.json"):
                        try:
                            response = self._client.get_object(Bucket=self._bucket, Key=key)
                            data = json.loads(response["Body"].read().decode())
                            backups.append(StorageMetadata.from_dict(data))

                            if len(backups) >= limit:
                                break
                        except Exception as e:
                            logger.warning(f"Error reading metadata {key}: {e}")

                if len(backups) >= limit:
                    break

        except Exception as e:
            logger.error(f"Error listing backups: {e}")

        # Ordena por data de criacao (mais recente primeiro)
        backups.sort(key=lambda x: x.created_at, reverse=True)
        return backups[:limit]

    async def delete_backup(self, backup_id: str, tenant_id: str) -> bool:
        """Deleta backup do S3"""
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

        metadata = await self.get_metadata(backup_id, tenant_id)
        if not metadata:
            return False

        try:
            # Extrai prefixo do backup
            data_key = metadata.storage_path.replace(f"s3://{self._bucket}/", "")
            metadata_key = data_key.replace("/data.bin", "/metadata.json")

            # Deleta ambos objetos
            self._client.delete_objects(
                Bucket=self._bucket,
                Delete={
                    "Objects": [
                        {"Key": data_key},
                        {"Key": metadata_key}
                    ]
                }
            )

            logger.info(f"Backup {backup_id} deleted from S3")
            return True

        except Exception as e:
            logger.error(f"Error deleting backup {backup_id}: {e}")
            return False

    async def cleanup_expired(self) -> int:
        """Remove backups expirados do S3"""
        if not self._client:
            raise RuntimeError("S3Storage not initialized")

        removed = 0
        now = datetime.utcnow()

        try:
            paginator = self._client.get_paginator("list_objects_v2")
            for page in paginator.paginate(Bucket=self._bucket, Prefix=self._prefix):
                for obj in page.get("Contents", []):
                    key = obj["Key"]
                    if key.endswith("/metadata.json"):
                        try:
                            response = self._client.get_object(Bucket=self._bucket, Key=key)
                            data = json.loads(response["Body"].read().decode())
                            metadata = StorageMetadata.from_dict(data)

                            if metadata.expires_at and now > metadata.expires_at:
                                # Deleta backup expirado
                                data_key = key.replace("/metadata.json", "/data.bin")
                                self._client.delete_objects(
                                    Bucket=self._bucket,
                                    Delete={
                                        "Objects": [
                                            {"Key": key},
                                            {"Key": data_key}
                                        ]
                                    }
                                )
                                removed += 1
                                logger.info(f"Expired backup {metadata.backup_id} removed")

                        except Exception as e:
                            logger.warning(f"Error checking backup {key}: {e}")

        except Exception as e:
            logger.error(f"Error during cleanup: {e}")

        return removed

    async def get_storage_info(self) -> Dict[str, Any]:
        """Retorna informacoes do storage S3"""
        info = await super().get_storage_info()
        info["bucket"] = self._bucket
        info["region"] = self.config.aws_region
        info["prefix"] = self._prefix

        if self._client:
            try:
                # Conta objetos e tamanho total
                total_size = 0
                backup_count = 0

                paginator = self._client.get_paginator("list_objects_v2")
                for page in paginator.paginate(Bucket=self._bucket, Prefix=self._prefix):
                    for obj in page.get("Contents", []):
                        if obj["Key"].endswith("/data.bin"):
                            total_size += obj["Size"]
                            backup_count += 1

                info["total_size_bytes"] = total_size
                info["total_size_mb"] = round(total_size / (1024 * 1024), 2)
                info["backup_count"] = backup_count

            except Exception as e:
                logger.warning(f"Error getting S3 stats: {e}")

        return info
