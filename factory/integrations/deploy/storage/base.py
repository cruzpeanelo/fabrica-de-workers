# -*- coding: utf-8 -*-
"""
Base Storage Interface
======================
Interface abstrata para storage de backups.

Issue #362 - Terminal A
"""

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


@dataclass
class StorageConfig:
    """
    Configuracao de storage.

    Attributes:
        storage_type: Tipo de storage (local, s3, azure)
        tenant_id: ID do tenant para isolamento
        base_path: Caminho base para armazenamento
        retention_days: Dias de retencao de backups
        max_backup_size_mb: Tamanho maximo por backup
        encryption_enabled: Se criptografia esta habilitada
        compression_enabled: Se compressao esta habilitada
    """
    storage_type: str = "local"
    tenant_id: str = ""
    base_path: str = "./backups"
    retention_days: int = 30
    max_backup_size_mb: int = 100
    encryption_enabled: bool = False
    compression_enabled: bool = True

    # Configuracoes especificas por provider
    aws_region: str = ""
    aws_bucket: str = ""
    aws_access_key: str = ""
    aws_secret_key: str = ""

    azure_connection_string: str = ""
    azure_container: str = ""

    extra_config: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "storage_type": self.storage_type,
            "tenant_id": self.tenant_id,
            "base_path": self.base_path,
            "retention_days": self.retention_days,
            "max_backup_size_mb": self.max_backup_size_mb,
            "encryption_enabled": self.encryption_enabled,
            "compression_enabled": self.compression_enabled
        }


@dataclass
class StorageMetadata:
    """Metadados de um backup armazenado"""
    backup_id: str
    tenant_id: str
    request_id: str
    integration: str
    environment: str
    storage_path: str
    size_bytes: int
    checksum: str
    created_at: datetime
    expires_at: Optional[datetime]
    compressed: bool = False
    encrypted: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "backup_id": self.backup_id,
            "tenant_id": self.tenant_id,
            "request_id": self.request_id,
            "integration": self.integration,
            "environment": self.environment,
            "storage_path": self.storage_path,
            "size_bytes": self.size_bytes,
            "checksum": self.checksum,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "compressed": self.compressed,
            "encrypted": self.encrypted,
            "metadata": self.metadata
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "StorageMetadata":
        """Cria instancia a partir de dicionario"""
        return cls(
            backup_id=data["backup_id"],
            tenant_id=data["tenant_id"],
            request_id=data["request_id"],
            integration=data["integration"],
            environment=data["environment"],
            storage_path=data["storage_path"],
            size_bytes=data["size_bytes"],
            checksum=data["checksum"],
            created_at=datetime.fromisoformat(data["created_at"]),
            expires_at=datetime.fromisoformat(data["expires_at"]) if data.get("expires_at") else None,
            compressed=data.get("compressed", False),
            encrypted=data.get("encrypted", False),
            metadata=data.get("metadata", {})
        )


class BackupStorage(ABC):
    """
    Interface abstrata para storage de backups.

    Implementacoes devem fornecer:
    - save_backup: Salvar backup
    - fetch_backup: Buscar backup por ID
    - list_backups: Listar backups
    - delete_backup: Deletar backup
    - cleanup_expired: Limpar backups expirados

    Exemplo:
        storage = LocalFileStorage(config)

        # Salvar backup
        metadata = await storage.save_backup(backup)

        # Buscar backup
        backup = await storage.fetch_backup(backup_id)

        # Listar backups
        backups = await storage.list_backups(tenant_id)

        # Limpar expirados
        await storage.cleanup_expired()
    """

    def __init__(self, config: StorageConfig):
        self.config = config
        self._initialized = False

    @property
    def storage_type(self) -> str:
        """Retorna tipo de storage"""
        return self.config.storage_type

    @abstractmethod
    async def initialize(self) -> bool:
        """
        Inicializa o storage.

        Returns:
            True se inicializacao bem sucedida
        """
        pass

    @abstractmethod
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
        """
        Salva backup no storage.

        Args:
            backup_id: ID unico do backup
            tenant_id: ID do tenant
            request_id: ID do deploy request
            integration: Nome da integracao
            environment: Ambiente (dev, staging, prod)
            data: Dados do backup em bytes
            metadata: Metadados adicionais

        Returns:
            Metadados do backup salvo
        """
        pass

    @abstractmethod
    async def fetch_backup(self, backup_id: str, tenant_id: str) -> Optional[bytes]:
        """
        Busca backup pelo ID.

        Args:
            backup_id: ID do backup
            tenant_id: ID do tenant

        Returns:
            Dados do backup ou None se nao encontrado
        """
        pass

    @abstractmethod
    async def get_metadata(self, backup_id: str, tenant_id: str) -> Optional[StorageMetadata]:
        """
        Busca metadados de um backup.

        Args:
            backup_id: ID do backup
            tenant_id: ID do tenant

        Returns:
            Metadados do backup ou None se nao encontrado
        """
        pass

    @abstractmethod
    async def list_backups(
        self,
        tenant_id: str,
        integration: Optional[str] = None,
        environment: Optional[str] = None,
        limit: int = 100
    ) -> List[StorageMetadata]:
        """
        Lista backups disponíveis.

        Args:
            tenant_id: ID do tenant
            integration: Filtrar por integracao
            environment: Filtrar por ambiente
            limit: Numero maximo de resultados

        Returns:
            Lista de metadados de backups
        """
        pass

    @abstractmethod
    async def delete_backup(self, backup_id: str, tenant_id: str) -> bool:
        """
        Deleta um backup.

        Args:
            backup_id: ID do backup
            tenant_id: ID do tenant

        Returns:
            True se deletado com sucesso
        """
        pass

    @abstractmethod
    async def cleanup_expired(self) -> int:
        """
        Remove backups expirados.

        Returns:
            Numero de backups removidos
        """
        pass

    async def get_storage_info(self) -> Dict[str, Any]:
        """
        Retorna informacoes do storage.

        Returns:
            Dicionario com informacoes do storage
        """
        return {
            "storage_type": self.storage_type,
            "initialized": self._initialized,
            "config": self.config.to_dict()
        }

    def _calculate_checksum(self, data: bytes) -> str:
        """Calcula checksum SHA256 dos dados"""
        import hashlib
        return hashlib.sha256(data).hexdigest()

    def _compress_data(self, data: bytes) -> bytes:
        """Comprime dados usando gzip"""
        import gzip
        return gzip.compress(data)

    def _decompress_data(self, data: bytes) -> bytes:
        """Descomprime dados gzip"""
        import gzip
        return gzip.decompress(data)

    def _get_expiration_date(self) -> Optional[datetime]:
        """Calcula data de expiracao baseado na retencao"""
        if self.config.retention_days > 0:
            return datetime.utcnow() + timedelta(days=self.config.retention_days)
        return None

    def _build_storage_path(
        self,
        tenant_id: str,
        integration: str,
        environment: str,
        backup_id: str
    ) -> str:
        """
        Constroi caminho de armazenamento.

        Estrutura: {base_path}/{tenant_id}/{integration}/{environment}/{backup_id}
        """
        return f"{self.config.base_path}/{tenant_id}/{integration}/{environment}/{backup_id}"
