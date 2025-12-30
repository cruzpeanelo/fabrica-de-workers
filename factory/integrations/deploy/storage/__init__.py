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

from .base import BackupStorage, StorageConfig
from .local_storage import LocalFileStorage
from .s3_storage import S3Storage
from .azure_storage import AzureBlobStorage

__all__ = [
    "BackupStorage",
    "StorageConfig",
    "LocalFileStorage",
    "S3Storage",
    "AzureBlobStorage"
]
