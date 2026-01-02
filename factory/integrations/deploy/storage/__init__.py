# -*- coding: utf-8 -*-
"""
Backup Storage Module
=====================
Sistema de storage persistente para backups de deploy.

Issue #362 - Terminal A

Implementacoes:
- LocalFileStorage: Arquivos locais (desenvolvimento)
- S3Storage: AWS S3
- AzureBlobStorage: Azure Blob Storage
"""

from .base import BackupStorage, StorageConfig, StorageMetadata
from .local_storage import LocalFileStorage
from .s3_storage import S3Storage
from .azure_storage import AzureBlobStorage

# Factory function
def get_storage(
    storage_type: str = "local",
    config: StorageConfig = None,
    **kwargs
) -> BackupStorage:
    """
    Factory para obter instancia de storage.

    Args:
        storage_type: Tipo de storage ("local", "s3", "azure")
        config: Configuracao de storage (opcional)
        **kwargs: Argumentos para criar StorageConfig

    Returns:
        Instancia de BackupStorage
    """
    if config is None:
        config = StorageConfig(storage_type=storage_type, **kwargs)

    if storage_type == "local":
        return LocalFileStorage(config)
    elif storage_type == "s3":
        return S3Storage(config)
    elif storage_type == "azure":
        return AzureBlobStorage(config)
    else:
        raise ValueError(f"Tipo de storage desconhecido: {storage_type}")


__all__ = [
    "BackupStorage",
    "StorageConfig",
    "StorageMetadata",
    "LocalFileStorage",
    "S3Storage",
    "AzureBlobStorage",
    "get_storage"
]
