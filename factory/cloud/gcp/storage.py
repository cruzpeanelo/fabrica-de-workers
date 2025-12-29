# -*- coding: utf-8 -*-
"""
GCP Cloud Storage Manager
=========================
Gerenciador de Cloud Storage para a Fabrica de Agentes.

Funcionalidades:
- Criacao de buckets
- Upload/Download de objetos
- Gerenciamento de permissoes
- Signed URLs
"""

from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
import logging
import mimetypes
import os

logger = logging.getLogger(__name__)

# Precos Cloud Storage (us-central1) - por GB/mes
STORAGE_PRICING = {
    "standard": 0.020,
    "nearline": 0.010,
    "coldline": 0.004,
    "archive": 0.0012,
}


class CloudStorageManager:
    """
    Gerenciador de Cloud Storage.

    Responsavel por todas as operacoes relacionadas ao Cloud Storage:
    - Criacao e configuracao de buckets
    - Upload e download de objetos
    - Gerenciamento de acesso
    """

    def __init__(self, storage_client, config):
        """
        Inicializa o gerenciador de Storage.

        Args:
            storage_client: Cliente Google Cloud Storage
            config: Configuracao GCP
        """
        self.client = storage_client
        self.config = config
        self.project_id = config.project_id

    async def create_bucket(
        self,
        name: str,
        location: Optional[str] = None,
        storage_class: str = "STANDARD",
        uniform_access: bool = True,
        versioning: bool = False,
        lifecycle_rules: Optional[List[Dict]] = None,
        labels: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um novo bucket.

        Args:
            name: Nome do bucket (globalmente unico)
            location: Localizacao (US, EU, us-central1, etc)
            storage_class: Classe de storage (STANDARD, NEARLINE, etc)
            uniform_access: Usar uniform bucket-level access
            versioning: Habilitar versionamento
            lifecycle_rules: Regras de lifecycle
            labels: Labels do bucket

        Returns:
            Dicionario com informacoes do bucket criado
        """
        try:
            bucket_labels = {"project": "fabrica-de-agentes"}
            if labels:
                bucket_labels.update(labels)

            bucket = self.client.bucket(name)
            bucket.location = location or self.config.region
            bucket.storage_class = storage_class
            bucket.labels = bucket_labels

            # Configurar uniform access
            bucket.iam_configuration.uniform_bucket_level_access_enabled = uniform_access

            # Configurar versionamento
            if versioning:
                bucket.versioning_enabled = True

            # Configurar lifecycle
            if lifecycle_rules:
                bucket.lifecycle_rules = lifecycle_rules

            # Criar bucket
            bucket = self.client.create_bucket(bucket, project=self.project_id)

            logger.info(f"Bucket GCS criado: {name}")

            return {
                "success": True,
                "bucket_name": name,
                "location": bucket.location,
                "storage_class": bucket.storage_class,
                "url": f"gs://{name}",
                "public_url": f"https://storage.googleapis.com/{name}",
            }

        except Exception as e:
            # Verificar se ja existe
            if "409" in str(e):
                return {
                    "success": True,
                    "bucket_name": name,
                    "already_exists": True,
                }
            logger.error(f"Erro ao criar bucket: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_bucket(
        self,
        bucket_name: str,
        force: bool = False
    ) -> bool:
        """
        Deleta um bucket.

        Args:
            bucket_name: Nome do bucket
            force: Deletar todos os objetos antes

        Returns:
            True se deletou com sucesso
        """
        try:
            bucket = self.client.bucket(bucket_name)

            if force:
                # Deletar todos os objetos primeiro
                blobs = bucket.list_blobs()
                for blob in blobs:
                    blob.delete()

            bucket.delete()
            logger.info(f"Bucket {bucket_name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar bucket: {e}")
            return False

    async def upload_file(
        self,
        bucket_name: str,
        file_path: str,
        blob_name: Optional[str] = None,
        content_type: Optional[str] = None,
        public: bool = False,
        metadata: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Faz upload de um arquivo para o bucket.

        Args:
            bucket_name: Nome do bucket
            file_path: Caminho local do arquivo
            blob_name: Nome do blob (usa nome do arquivo se None)
            content_type: Tipo MIME
            public: Tornar publico
            metadata: Metadados adicionais

        Returns:
            Dicionario com informacoes do upload
        """
        try:
            if not blob_name:
                blob_name = os.path.basename(file_path)

            if not content_type:
                content_type, _ = mimetypes.guess_type(file_path)
                content_type = content_type or "application/octet-stream"

            bucket = self.client.bucket(bucket_name)
            blob = bucket.blob(blob_name)

            if metadata:
                blob.metadata = metadata

            blob.upload_from_filename(file_path, content_type=content_type)

            if public:
                blob.make_public()

            logger.info(f"Arquivo {blob_name} enviado para {bucket_name}")

            return {
                "success": True,
                "bucket_name": bucket_name,
                "blob_name": blob_name,
                "url": f"gs://{bucket_name}/{blob_name}",
                "public_url": blob.public_url if public else None,
                "media_link": blob.media_link,
                "content_type": content_type,
                "size": blob.size,
            }

        except Exception as e:
            logger.error(f"Erro ao fazer upload: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def upload_string(
        self,
        bucket_name: str,
        content: str,
        blob_name: str,
        content_type: str = "text/plain",
        public: bool = False
    ) -> Dict[str, Any]:
        """
        Faz upload de uma string para o bucket.

        Args:
            bucket_name: Nome do bucket
            content: Conteudo a enviar
            blob_name: Nome do blob
            content_type: Tipo MIME
            public: Tornar publico

        Returns:
            Dicionario com informacoes do upload
        """
        try:
            bucket = self.client.bucket(bucket_name)
            blob = bucket.blob(blob_name)

            blob.upload_from_string(content, content_type=content_type)

            if public:
                blob.make_public()

            return {
                "success": True,
                "bucket_name": bucket_name,
                "blob_name": blob_name,
                "url": f"gs://{bucket_name}/{blob_name}",
                "public_url": blob.public_url if public else None,
            }

        except Exception as e:
            logger.error(f"Erro ao fazer upload de string: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def download_file(
        self,
        bucket_name: str,
        blob_name: str,
        local_path: str
    ) -> bool:
        """
        Faz download de um arquivo do bucket.

        Args:
            bucket_name: Nome do bucket
            blob_name: Nome do blob
            local_path: Caminho local para salvar

        Returns:
            True se baixou com sucesso
        """
        try:
            bucket = self.client.bucket(bucket_name)
            blob = bucket.blob(blob_name)

            blob.download_to_filename(local_path)

            logger.info(f"Arquivo {blob_name} baixado para {local_path}")
            return True

        except Exception as e:
            logger.error(f"Erro ao fazer download: {e}")
            return False

    async def delete_blob(
        self,
        bucket_name: str,
        blob_name: str
    ) -> bool:
        """
        Deleta um blob.

        Args:
            bucket_name: Nome do bucket
            blob_name: Nome do blob

        Returns:
            True se deletou com sucesso
        """
        try:
            bucket = self.client.bucket(bucket_name)
            blob = bucket.blob(blob_name)
            blob.delete()

            logger.info(f"Blob {blob_name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar blob: {e}")
            return False

    async def list_blobs(
        self,
        bucket_name: str,
        prefix: Optional[str] = None,
        max_results: int = 1000
    ) -> List[Dict[str, Any]]:
        """
        Lista blobs em um bucket.

        Args:
            bucket_name: Nome do bucket
            prefix: Prefixo para filtrar
            max_results: Numero maximo de resultados

        Returns:
            Lista de blobs
        """
        try:
            bucket = self.client.bucket(bucket_name)
            blobs = bucket.list_blobs(prefix=prefix, max_results=max_results)

            blob_list = []
            for blob in blobs:
                blob_list.append({
                    "name": blob.name,
                    "size": blob.size,
                    "content_type": blob.content_type,
                    "updated": blob.updated,
                    "md5_hash": blob.md5_hash,
                    "storage_class": blob.storage_class,
                })

            return blob_list

        except Exception as e:
            logger.error(f"Erro ao listar blobs: {e}")
            return []

    async def list_buckets(self) -> List[Dict[str, Any]]:
        """
        Lista todos os buckets do projeto.

        Returns:
            Lista de buckets
        """
        try:
            buckets = []
            for bucket in self.client.list_buckets():
                buckets.append({
                    "name": bucket.name,
                    "location": bucket.location,
                    "storage_class": bucket.storage_class,
                    "created": bucket.time_created,
                    "labels": dict(bucket.labels) if bucket.labels else {},
                })

            return buckets

        except Exception as e:
            logger.error(f"Erro ao listar buckets: {e}")
            return []

    async def get_bucket_info(self, bucket_name: str) -> Dict[str, Any]:
        """
        Obtem informacoes detalhadas de um bucket.

        Args:
            bucket_name: Nome do bucket

        Returns:
            Dicionario com informacoes do bucket
        """
        try:
            bucket = self.client.get_bucket(bucket_name)

            # Contar objetos e tamanho total
            blobs = list(bucket.list_blobs())
            total_size = sum(blob.size or 0 for blob in blobs)
            object_count = len(blobs)

            return {
                "name": bucket.name,
                "location": bucket.location,
                "storage_class": bucket.storage_class,
                "created": bucket.time_created,
                "versioning_enabled": bucket.versioning_enabled,
                "labels": dict(bucket.labels) if bucket.labels else {},
                "object_count": object_count,
                "total_size_bytes": total_size,
                "total_size_mb": round(total_size / (1024 * 1024), 2),
            }

        except Exception as e:
            logger.error(f"Erro ao obter info do bucket: {e}")
            return {"error": str(e)}

    async def generate_signed_url(
        self,
        bucket_name: str,
        blob_name: str,
        expiration_minutes: int = 60,
        method: str = "GET"
    ) -> Optional[str]:
        """
        Gera URL assinada para acesso temporario.

        Args:
            bucket_name: Nome do bucket
            blob_name: Nome do blob
            expiration_minutes: Minutos ate expirar
            method: Metodo HTTP (GET, PUT)

        Returns:
            URL assinada ou None se erro
        """
        try:
            bucket = self.client.bucket(bucket_name)
            blob = bucket.blob(blob_name)

            url = blob.generate_signed_url(
                version="v4",
                expiration=timedelta(minutes=expiration_minutes),
                method=method,
            )

            return url

        except Exception as e:
            logger.error(f"Erro ao gerar URL assinada: {e}")
            return None

    async def make_bucket_public(self, bucket_name: str) -> bool:
        """
        Torna um bucket publico.

        Args:
            bucket_name: Nome do bucket

        Returns:
            True se configurou com sucesso
        """
        try:
            bucket = self.client.bucket(bucket_name)

            # Configurar IAM para acesso publico
            policy = bucket.get_iam_policy(requested_policy_version=3)
            policy.bindings.append({
                "role": "roles/storage.objectViewer",
                "members": ["allUsers"],
            })
            bucket.set_iam_policy(policy)

            logger.info(f"Bucket {bucket_name} tornado publico")
            return True

        except Exception as e:
            logger.error(f"Erro ao tornar bucket publico: {e}")
            return False

    def get_storage_pricing(self, storage_class: str = "standard") -> float:
        """
        Retorna preco por GB/mes de uma classe de storage.

        Args:
            storage_class: Classe de storage

        Returns:
            Preco por GB/mes em USD
        """
        return STORAGE_PRICING.get(storage_class.lower(), STORAGE_PRICING["standard"])

    def estimate_monthly_cost(
        self,
        storage_gb: float,
        storage_class: str = "standard"
    ) -> float:
        """
        Estima custo mensal para um volume de storage.

        Args:
            storage_gb: Volume em GB
            storage_class: Classe de storage

        Returns:
            Custo mensal estimado em USD
        """
        price_per_gb = self.get_storage_pricing(storage_class)
        return round(storage_gb * price_per_gb, 2)
