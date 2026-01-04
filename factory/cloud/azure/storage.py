# -*- coding: utf-8 -*-
"""
Azure Blob Storage Manager
==========================
Gerenciador de Blob Storage Azure para a Plataforma E.

Funcionalidades:
- Criacao de Storage Accounts
- Gerenciamento de Containers
- Upload/Download de blobs
- Geracao de SAS tokens
"""

from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
import logging
import mimetypes
import os

logger = logging.getLogger(__name__)

# Precos Azure Storage (East US) - por GB/mes
STORAGE_PRICING = {
    "hot": 0.0184,           # Hot tier
    "cool": 0.01,            # Cool tier
    "archive": 0.00099,      # Archive tier
    "premium": 0.15,         # Premium SSD
}


class StorageManager:
    """
    Gerenciador de Azure Blob Storage.

    Responsavel por todas as operacoes relacionadas a Storage:
    - Criacao de storage accounts
    - Gerenciamento de containers
    - Upload e download de blobs
    """

    def __init__(self, storage_client, blob_service_client, config):
        """
        Inicializa o gerenciador de Storage.

        Args:
            storage_client: Cliente Azure Storage Management
            blob_service_client: Cliente Blob Service (para operacoes de dados)
            config: Configuracao Azure
        """
        self.storage_client = storage_client
        self.blob_service_client = blob_service_client
        self.config = config
        self.resource_group = config.resource_group
        self.location = config.location

    async def create_storage_account(
        self,
        name: str,
        sku: str = "Standard_LRS",
        kind: str = "StorageV2",
        access_tier: str = "Hot",
        https_only: bool = True,
        allow_blob_public_access: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova Storage Account.

        Args:
            name: Nome da storage account (3-24 caracteres, lowercase)
            sku: SKU (Standard_LRS, Standard_GRS, Premium_LRS)
            kind: Tipo (StorageV2, BlobStorage)
            access_tier: Tier de acesso (Hot, Cool)
            https_only: Permitir apenas HTTPS
            allow_blob_public_access: Permitir acesso publico a blobs
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes da storage account
        """
        try:
            storage_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                storage_tags.update(tags)

            # Validar nome (deve ser unico globalmente)
            name = name.lower().replace("-", "").replace("_", "")[:24]

            params = {
                "location": self.location,
                "sku": {"name": sku},
                "kind": kind,
                "tags": storage_tags,
                "properties": {
                    "supportsHttpsTrafficOnly": https_only,
                    "allowBlobPublicAccess": allow_blob_public_access,
                    "accessTier": access_tier,
                    "minimumTlsVersion": "TLS1_2",
                }
            }

            poller = self.storage_client.storage_accounts.begin_create(
                self.resource_group,
                name,
                params
            )
            account = poller.result()

            # Obter chaves
            keys = self.storage_client.storage_accounts.list_keys(
                self.resource_group, name
            )
            primary_key = keys.keys[0].value

            logger.info(f"Storage Account criada: {name}")

            return {
                "success": True,
                "account_name": name,
                "account_id": account.id,
                "location": account.location,
                "primary_endpoint": f"https://{name}.blob.core.windows.net",
                "primary_key": primary_key,
                "sku": sku,
                "kind": kind,
            }

        except Exception as e:
            logger.error(f"Erro ao criar Storage Account: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_storage_account(self, name: str) -> bool:
        """
        Deleta uma Storage Account.

        Args:
            name: Nome da storage account

        Returns:
            True se deletou com sucesso
        """
        try:
            self.storage_client.storage_accounts.delete(
                self.resource_group, name
            )
            logger.info(f"Storage Account {name} deletada")
            return True
        except Exception as e:
            logger.error(f"Erro ao deletar Storage Account: {e}")
            return False

    async def create_container(
        self,
        account_name: str,
        container_name: str,
        public_access: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria um container em uma Storage Account.

        Args:
            account_name: Nome da storage account
            container_name: Nome do container
            public_access: Nivel de acesso publico (None, "blob", "container")

        Returns:
            Dicionario com informacoes do container
        """
        try:
            # Obter blob service client para esta account
            container_client = self.blob_service_client.get_container_client(container_name)

            container_client.create_container(
                public_access=public_access
            )

            logger.info(f"Container {container_name} criado")

            return {
                "success": True,
                "container_name": container_name,
                "url": f"https://{account_name}.blob.core.windows.net/{container_name}",
                "public_access": public_access,
            }

        except Exception as e:
            # Verificar se ja existe
            if "ContainerAlreadyExists" in str(e):
                return {
                    "success": True,
                    "container_name": container_name,
                    "already_exists": True,
                }
            logger.error(f"Erro ao criar container: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_container(
        self,
        container_name: str,
        force: bool = False
    ) -> bool:
        """
        Deleta um container.

        Args:
            container_name: Nome do container
            force: Deletar mesmo com blobs dentro

        Returns:
            True se deletou com sucesso
        """
        try:
            container_client = self.blob_service_client.get_container_client(container_name)

            if force:
                # Deletar todos os blobs primeiro
                blobs = container_client.list_blobs()
                for blob in blobs:
                    container_client.delete_blob(blob.name)

            container_client.delete_container()
            logger.info(f"Container {container_name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar container: {e}")
            return False

    async def upload_blob(
        self,
        container_name: str,
        blob_name: str,
        file_path: Optional[str] = None,
        data: Optional[bytes] = None,
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Faz upload de um blob.

        Args:
            container_name: Nome do container
            blob_name: Nome do blob
            file_path: Caminho do arquivo local
            data: Dados em bytes (alternativa a file_path)
            content_type: Tipo MIME
            metadata: Metadados adicionais

        Returns:
            Dicionario com informacoes do upload
        """
        try:
            container_client = self.blob_service_client.get_container_client(container_name)
            blob_client = container_client.get_blob_client(blob_name)

            # Determinar content type
            if not content_type:
                if file_path:
                    content_type, _ = mimetypes.guess_type(file_path)
                content_type = content_type or "application/octet-stream"

            # Upload
            if file_path:
                with open(file_path, "rb") as f:
                    blob_client.upload_blob(
                        f,
                        content_settings={"content_type": content_type},
                        metadata=metadata,
                        overwrite=True
                    )
            elif data:
                blob_client.upload_blob(
                    data,
                    content_settings={"content_type": content_type},
                    metadata=metadata,
                    overwrite=True
                )
            else:
                return {"success": False, "error": "Nenhum dado fornecido"}

            logger.info(f"Blob {blob_name} enviado para {container_name}")

            return {
                "success": True,
                "container_name": container_name,
                "blob_name": blob_name,
                "url": blob_client.url,
                "content_type": content_type,
            }

        except Exception as e:
            logger.error(f"Erro ao fazer upload: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def download_blob(
        self,
        container_name: str,
        blob_name: str,
        local_path: str
    ) -> bool:
        """
        Faz download de um blob.

        Args:
            container_name: Nome do container
            blob_name: Nome do blob
            local_path: Caminho local para salvar

        Returns:
            True se baixou com sucesso
        """
        try:
            container_client = self.blob_service_client.get_container_client(container_name)
            blob_client = container_client.get_blob_client(blob_name)

            with open(local_path, "wb") as f:
                download_stream = blob_client.download_blob()
                f.write(download_stream.readall())

            logger.info(f"Blob {blob_name} baixado para {local_path}")
            return True

        except Exception as e:
            logger.error(f"Erro ao fazer download: {e}")
            return False

    async def delete_blob(
        self,
        container_name: str,
        blob_name: str
    ) -> bool:
        """
        Deleta um blob.

        Args:
            container_name: Nome do container
            blob_name: Nome do blob

        Returns:
            True se deletou com sucesso
        """
        try:
            container_client = self.blob_service_client.get_container_client(container_name)
            blob_client = container_client.get_blob_client(blob_name)
            blob_client.delete_blob()

            logger.info(f"Blob {blob_name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar blob: {e}")
            return False

    async def list_blobs(
        self,
        container_name: str,
        prefix: Optional[str] = None,
        max_results: int = 1000
    ) -> List[Dict[str, Any]]:
        """
        Lista blobs em um container.

        Args:
            container_name: Nome do container
            prefix: Prefixo para filtrar
            max_results: Numero maximo de resultados

        Returns:
            Lista de blobs
        """
        try:
            container_client = self.blob_service_client.get_container_client(container_name)

            blobs = []
            blob_list = container_client.list_blobs(name_starts_with=prefix)

            count = 0
            for blob in blob_list:
                if count >= max_results:
                    break
                blobs.append({
                    "name": blob.name,
                    "size": blob.size,
                    "content_type": blob.content_settings.content_type if blob.content_settings else None,
                    "last_modified": blob.last_modified,
                    "etag": blob.etag,
                })
                count += 1

            return blobs

        except Exception as e:
            logger.error(f"Erro ao listar blobs: {e}")
            return []

    async def list_containers(self) -> List[Dict[str, Any]]:
        """
        Lista todos os containers.

        Returns:
            Lista de containers
        """
        try:
            containers = []
            container_list = self.blob_service_client.list_containers()

            for container in container_list:
                containers.append({
                    "name": container.name,
                    "last_modified": container.last_modified,
                })

            return containers

        except Exception as e:
            logger.error(f"Erro ao listar containers: {e}")
            return []

    async def list_storage_accounts(self) -> List[Dict[str, Any]]:
        """
        Lista todas as Storage Accounts do resource group.

        Returns:
            Lista de storage accounts
        """
        try:
            accounts = []
            account_list = self.storage_client.storage_accounts.list_by_resource_group(
                self.resource_group
            )

            for account in account_list:
                accounts.append({
                    "name": account.name,
                    "location": account.location,
                    "sku": account.sku.name,
                    "kind": account.kind,
                    "id": account.id,
                })

            return accounts

        except Exception as e:
            logger.error(f"Erro ao listar storage accounts: {e}")
            return []

    async def generate_sas_url(
        self,
        container_name: str,
        blob_name: str,
        expiry_hours: int = 1,
        permission: str = "r"
    ) -> Optional[str]:
        """
        Gera URL com SAS token para acesso temporario.

        Args:
            container_name: Nome do container
            blob_name: Nome do blob
            expiry_hours: Horas ate expirar
            permission: Permissoes (r=read, w=write, d=delete)

        Returns:
            URL com SAS token ou None se erro
        """
        try:
            from azure.storage.blob import generate_blob_sas, BlobSasPermissions

            container_client = self.blob_service_client.get_container_client(container_name)
            blob_client = container_client.get_blob_client(blob_name)

            # Obter chave da account
            account_name = self.blob_service_client.account_name
            keys = self.storage_client.storage_accounts.list_keys(
                self.resource_group, account_name
            )
            account_key = keys.keys[0].value

            # Gerar SAS token
            permissions = BlobSasPermissions(
                read="r" in permission,
                write="w" in permission,
                delete="d" in permission,
            )

            sas_token = generate_blob_sas(
                account_name=account_name,
                container_name=container_name,
                blob_name=blob_name,
                account_key=account_key,
                permission=permissions,
                expiry=datetime.utcnow() + timedelta(hours=expiry_hours),
            )

            return f"{blob_client.url}?{sas_token}"

        except Exception as e:
            logger.error(f"Erro ao gerar SAS URL: {e}")
            return None

    def get_storage_pricing(self, tier: str = "hot") -> float:
        """
        Retorna preco por GB/mes de um tier.

        Args:
            tier: Tier de storage

        Returns:
            Preco por GB/mes em USD
        """
        return STORAGE_PRICING.get(tier.lower(), STORAGE_PRICING["hot"])

    def estimate_monthly_cost(
        self,
        storage_gb: float,
        tier: str = "hot"
    ) -> float:
        """
        Estima custo mensal para um volume de storage.

        Args:
            storage_gb: Volume em GB
            tier: Tier de storage

        Returns:
            Custo mensal estimado em USD
        """
        price_per_gb = self.get_storage_pricing(tier)
        return round(storage_gb * price_per_gb, 2)
