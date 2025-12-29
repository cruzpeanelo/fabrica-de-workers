# -*- coding: utf-8 -*-
"""
AWS S3 Manager
==============
Gerenciador de buckets e objetos S3 para a Fabrica de Agentes.

Funcionalidades:
- Criacao e gerenciamento de buckets
- Upload/Download de arquivos
- Configuracao de permissoes
- Versionamento e lifecycle
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import mimetypes
import os

logger = logging.getLogger(__name__)

# Precos S3 (us-east-1) - por GB/mes
S3_PRICING = {
    "standard": 0.023,           # Primeiros 50 TB
    "standard_ia": 0.0125,       # Infrequent Access
    "one_zone_ia": 0.01,         # One Zone IA
    "glacier": 0.004,            # Glacier
    "glacier_deep": 0.00099,     # Glacier Deep Archive
}


class S3Manager:
    """
    Gerenciador de buckets e objetos S3.

    Responsavel por todas as operacoes relacionadas ao S3:
    - Criacao e configuracao de buckets
    - Upload e download de arquivos
    - Gerenciamento de permissoes
    - Versionamento e lifecycle policies
    """

    def __init__(self, s3_client, s3_resource, config):
        """
        Inicializa o gerenciador S3.

        Args:
            s3_client: Cliente boto3 S3
            s3_resource: Resource boto3 S3
            config: Configuracao AWS
        """
        self.client = s3_client
        self.resource = s3_resource
        self.config = config
        self.region = config.region

    async def create_bucket(
        self,
        name: str,
        public: bool = False,
        versioning: bool = False,
        encryption: bool = True,
        cors_enabled: bool = False,
        lifecycle_rules: Optional[List[Dict]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um novo bucket S3.

        Args:
            name: Nome do bucket (deve ser unico globalmente)
            public: Se o bucket deve ser publico
            versioning: Habilitar versionamento
            encryption: Habilitar criptografia server-side
            cors_enabled: Habilitar CORS para web
            lifecycle_rules: Regras de lifecycle
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes do bucket criado
        """
        try:
            # Configurar criacao do bucket
            create_params = {"Bucket": name}

            # Para regioes diferentes de us-east-1, precisa especificar LocationConstraint
            if self.region != "us-east-1":
                create_params["CreateBucketConfiguration"] = {
                    "LocationConstraint": self.region
                }

            # Criar bucket
            self.client.create_bucket(**create_params)
            logger.info(f"Bucket S3 criado: {name}")

            # Configurar block public access (padrao: bloqueado)
            if not public:
                self.client.put_public_access_block(
                    Bucket=name,
                    PublicAccessBlockConfiguration={
                        "BlockPublicAcls": True,
                        "IgnorePublicAcls": True,
                        "BlockPublicPolicy": True,
                        "RestrictPublicBuckets": True
                    }
                )
            else:
                # Desabilitar block public access para buckets publicos
                self.client.put_public_access_block(
                    Bucket=name,
                    PublicAccessBlockConfiguration={
                        "BlockPublicAcls": False,
                        "IgnorePublicAcls": False,
                        "BlockPublicPolicy": False,
                        "RestrictPublicBuckets": False
                    }
                )

            # Habilitar versionamento
            if versioning:
                self.client.put_bucket_versioning(
                    Bucket=name,
                    VersioningConfiguration={"Status": "Enabled"}
                )

            # Habilitar criptografia
            if encryption:
                self.client.put_bucket_encryption(
                    Bucket=name,
                    ServerSideEncryptionConfiguration={
                        "Rules": [
                            {
                                "ApplyServerSideEncryptionByDefault": {
                                    "SSEAlgorithm": "AES256"
                                }
                            }
                        ]
                    }
                )

            # Configurar CORS
            if cors_enabled:
                cors_config = {
                    "CORSRules": [
                        {
                            "AllowedHeaders": ["*"],
                            "AllowedMethods": ["GET", "PUT", "POST", "DELETE", "HEAD"],
                            "AllowedOrigins": ["*"],
                            "ExposeHeaders": ["ETag"],
                            "MaxAgeSeconds": 3000
                        }
                    ]
                }
                self.client.put_bucket_cors(
                    Bucket=name,
                    CORSConfiguration=cors_config
                )

            # Adicionar tags
            bucket_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                bucket_tags.update(tags)

            self.client.put_bucket_tagging(
                Bucket=name,
                Tagging={
                    "TagSet": [
                        {"Key": k, "Value": v}
                        for k, v in bucket_tags.items()
                    ]
                }
            )

            # Configurar lifecycle rules
            if lifecycle_rules:
                self.client.put_bucket_lifecycle_configuration(
                    Bucket=name,
                    LifecycleConfiguration={"Rules": lifecycle_rules}
                )

            # Construir URL do bucket
            if self.region == "us-east-1":
                bucket_url = f"https://{name}.s3.amazonaws.com"
            else:
                bucket_url = f"https://{name}.s3.{self.region}.amazonaws.com"

            return {
                "success": True,
                "bucket_name": name,
                "bucket_url": bucket_url,
                "region": self.region,
                "public": public,
                "versioning": versioning,
                "encryption": encryption,
            }

        except self.client.exceptions.BucketAlreadyExists:
            logger.error(f"Bucket {name} ja existe globalmente")
            return {
                "success": False,
                "error": f"Bucket {name} ja existe globalmente"
            }
        except self.client.exceptions.BucketAlreadyOwnedByYou:
            logger.info(f"Bucket {name} ja pertence a esta conta")
            return {
                "success": True,
                "bucket_name": name,
                "already_exists": True
            }
        except Exception as e:
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
        Deleta um bucket S3.

        Args:
            bucket_name: Nome do bucket
            force: Se True, deleta todos os objetos antes

        Returns:
            True se deletou com sucesso
        """
        try:
            if force:
                # Deletar todos os objetos primeiro
                bucket = self.resource.Bucket(bucket_name)
                bucket.objects.all().delete()

                # Deletar versoes (se versionamento habilitado)
                bucket.object_versions.delete()

            # Deletar bucket
            self.client.delete_bucket(Bucket=bucket_name)
            logger.info(f"Bucket {bucket_name} deletado")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar bucket: {e}")
            return False

    async def upload_file(
        self,
        bucket_name: str,
        file_path: str,
        object_key: Optional[str] = None,
        content_type: Optional[str] = None,
        public: bool = False,
        metadata: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Faz upload de um arquivo para o bucket.

        Args:
            bucket_name: Nome do bucket
            file_path: Caminho local do arquivo
            object_key: Chave do objeto (usa nome do arquivo se None)
            content_type: Tipo MIME (detecta automaticamente se None)
            public: Se o objeto deve ser publico
            metadata: Metadados adicionais

        Returns:
            Dicionario com informacoes do upload
        """
        try:
            # Determinar object key
            if not object_key:
                object_key = os.path.basename(file_path)

            # Detectar content type
            if not content_type:
                content_type, _ = mimetypes.guess_type(file_path)
                if not content_type:
                    content_type = "application/octet-stream"

            # Preparar parametros extras
            extra_args = {"ContentType": content_type}

            if public:
                extra_args["ACL"] = "public-read"

            if metadata:
                extra_args["Metadata"] = metadata

            # Fazer upload
            self.client.upload_file(
                file_path,
                bucket_name,
                object_key,
                ExtraArgs=extra_args
            )

            # Construir URL
            if self.region == "us-east-1":
                object_url = f"https://{bucket_name}.s3.amazonaws.com/{object_key}"
            else:
                object_url = f"https://{bucket_name}.s3.{self.region}.amazonaws.com/{object_key}"

            # Obter tamanho do arquivo
            file_size = os.path.getsize(file_path)

            logger.info(f"Arquivo {object_key} enviado para {bucket_name}")

            return {
                "success": True,
                "bucket_name": bucket_name,
                "object_key": object_key,
                "object_url": object_url,
                "content_type": content_type,
                "size_bytes": file_size,
                "public": public,
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
        object_key: str,
        content_type: str = "text/plain",
        public: bool = False
    ) -> Dict[str, Any]:
        """
        Faz upload de uma string para o bucket.

        Args:
            bucket_name: Nome do bucket
            content: Conteudo a enviar
            object_key: Chave do objeto
            content_type: Tipo MIME
            public: Se o objeto deve ser publico

        Returns:
            Dicionario com informacoes do upload
        """
        try:
            extra_args = {"ContentType": content_type}

            if public:
                extra_args["ACL"] = "public-read"

            self.client.put_object(
                Bucket=bucket_name,
                Key=object_key,
                Body=content.encode("utf-8"),
                **extra_args
            )

            # Construir URL
            if self.region == "us-east-1":
                object_url = f"https://{bucket_name}.s3.amazonaws.com/{object_key}"
            else:
                object_url = f"https://{bucket_name}.s3.{self.region}.amazonaws.com/{object_key}"

            return {
                "success": True,
                "bucket_name": bucket_name,
                "object_key": object_key,
                "object_url": object_url,
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
        object_key: str,
        local_path: str
    ) -> bool:
        """
        Faz download de um arquivo do bucket.

        Args:
            bucket_name: Nome do bucket
            object_key: Chave do objeto
            local_path: Caminho local para salvar

        Returns:
            True se baixou com sucesso
        """
        try:
            self.client.download_file(bucket_name, object_key, local_path)
            logger.info(f"Arquivo {object_key} baixado para {local_path}")
            return True

        except Exception as e:
            logger.error(f"Erro ao fazer download: {e}")
            return False

    async def delete_object(
        self,
        bucket_name: str,
        object_key: str
    ) -> bool:
        """
        Deleta um objeto do bucket.

        Args:
            bucket_name: Nome do bucket
            object_key: Chave do objeto

        Returns:
            True se deletou com sucesso
        """
        try:
            self.client.delete_object(Bucket=bucket_name, Key=object_key)
            logger.info(f"Objeto {object_key} deletado de {bucket_name}")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar objeto: {e}")
            return False

    async def list_objects(
        self,
        bucket_name: str,
        prefix: Optional[str] = None,
        max_keys: int = 1000
    ) -> List[Dict[str, Any]]:
        """
        Lista objetos em um bucket.

        Args:
            bucket_name: Nome do bucket
            prefix: Prefixo para filtrar
            max_keys: Numero maximo de objetos

        Returns:
            Lista de objetos
        """
        try:
            params = {
                "Bucket": bucket_name,
                "MaxKeys": max_keys
            }

            if prefix:
                params["Prefix"] = prefix

            response = self.client.list_objects_v2(**params)

            objects = []
            for obj in response.get("Contents", []):
                objects.append({
                    "key": obj["Key"],
                    "size": obj["Size"],
                    "last_modified": obj["LastModified"],
                    "etag": obj["ETag"],
                    "storage_class": obj.get("StorageClass", "STANDARD"),
                })

            return objects

        except Exception as e:
            logger.error(f"Erro ao listar objetos: {e}")
            return []

    async def list_buckets(self) -> List[Dict[str, Any]]:
        """
        Lista todos os buckets da conta.

        Returns:
            Lista de buckets
        """
        try:
            response = self.client.list_buckets()

            buckets = []
            for bucket in response.get("Buckets", []):
                buckets.append({
                    "name": bucket["Name"],
                    "creation_date": bucket["CreationDate"],
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
            # Verificar localizacao
            try:
                location = self.client.get_bucket_location(Bucket=bucket_name)
                region = location.get("LocationConstraint") or "us-east-1"
            except Exception:
                region = "unknown"

            # Verificar versionamento
            try:
                versioning = self.client.get_bucket_versioning(Bucket=bucket_name)
                versioning_enabled = versioning.get("Status") == "Enabled"
            except Exception:
                versioning_enabled = False

            # Obter tamanho total (aproximado)
            objects = await self.list_objects(bucket_name)
            total_size = sum(obj.get("size", 0) for obj in objects)
            object_count = len(objects)

            return {
                "name": bucket_name,
                "region": region,
                "versioning_enabled": versioning_enabled,
                "object_count": object_count,
                "total_size_bytes": total_size,
                "total_size_mb": round(total_size / (1024 * 1024), 2),
            }

        except Exception as e:
            logger.error(f"Erro ao obter info do bucket: {e}")
            return {"error": str(e)}

    async def generate_presigned_url(
        self,
        bucket_name: str,
        object_key: str,
        expiration: int = 3600,
        method: str = "get_object"
    ) -> Optional[str]:
        """
        Gera URL pre-assinada para acesso temporario.

        Args:
            bucket_name: Nome do bucket
            object_key: Chave do objeto
            expiration: Tempo de expiracao em segundos
            method: Metodo (get_object ou put_object)

        Returns:
            URL pre-assinada ou None se erro
        """
        try:
            url = self.client.generate_presigned_url(
                method,
                Params={"Bucket": bucket_name, "Key": object_key},
                ExpiresIn=expiration
            )
            return url

        except Exception as e:
            logger.error(f"Erro ao gerar URL pre-assinada: {e}")
            return None

    def get_storage_pricing(self, storage_class: str = "standard") -> float:
        """
        Retorna preco por GB/mes de uma classe de storage.

        Args:
            storage_class: Classe de storage

        Returns:
            Preco por GB/mes em USD
        """
        return S3_PRICING.get(storage_class.lower(), S3_PRICING["standard"])

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
