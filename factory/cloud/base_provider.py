# -*- coding: utf-8 -*-
"""
Cloud Provider Base Module
==========================
Interface base para provedores cloud (AWS, Azure, GCP).

Define as classes e interfaces que todos os provedores devem implementar
para garantir uma API consistente e unificada.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union
import logging
import os

logger = logging.getLogger(__name__)


class ProviderType(str, Enum):
    """Tipos de provedores cloud suportados"""
    AWS = "aws"
    AZURE = "azure"
    GCP = "gcp"


class ResourceType(str, Enum):
    """Tipos de recursos cloud"""
    # Computacao
    VM = "vm"                        # Maquina virtual
    CONTAINER = "container"          # Container
    KUBERNETES = "kubernetes"        # Cluster Kubernetes
    SERVERLESS = "serverless"        # Funcao serverless

    # Banco de dados
    DATABASE = "database"            # Banco SQL gerenciado
    NOSQL = "nosql"                  # Banco NoSQL
    CACHE = "cache"                  # Cache (Redis, Memcached)

    # Storage
    OBJECT_STORAGE = "object_storage"  # S3, Blob, GCS
    FILE_STORAGE = "file_storage"      # EFS, Azure Files
    BLOCK_STORAGE = "block_storage"    # EBS, Discos

    # Rede
    VPC = "vpc"                      # Virtual Private Cloud
    SUBNET = "subnet"                # Sub-rede
    LOAD_BALANCER = "load_balancer"  # Balanceador de carga
    CDN = "cdn"                      # Content Delivery Network
    DNS = "dns"                      # Servico de DNS

    # Seguranca
    FIREWALL = "firewall"            # Firewall/Security Group
    IAM = "iam"                      # Identity and Access Management
    SECRET = "secret"                # Gerenciador de segredos


class ResourceStatus(str, Enum):
    """Status de um recurso cloud"""
    PENDING = "pending"              # Aguardando criacao
    CREATING = "creating"            # Em processo de criacao
    RUNNING = "running"              # Ativo e funcionando
    UPDATING = "updating"            # Sendo atualizado
    STOPPING = "stopping"            # Parando
    STOPPED = "stopped"              # Parado
    DELETING = "deleting"            # Sendo deletado
    DELETED = "deleted"              # Deletado
    ERROR = "error"                  # Erro
    UNKNOWN = "unknown"              # Estado desconhecido


class StackType(str, Enum):
    """Tipos de stack de infraestrutura"""
    SIMPLE = "simple"                # Monolito simples
    MICROSERVICES = "microservices"  # Arquitetura de microservicos
    SERVERLESS = "serverless"        # Arquitetura serverless
    KUBERNETES = "kubernetes"        # Kubernetes nativo


@dataclass
class CloudConfig:
    """Configuracao base para provedores cloud"""
    provider: ProviderType
    region: str
    credentials: Dict[str, str] = field(default_factory=dict)
    tags: Dict[str, str] = field(default_factory=dict)
    default_vpc: Optional[str] = None
    default_subnet: Optional[str] = None

    def validate(self) -> bool:
        """Valida se a configuracao esta completa"""
        if not self.region:
            raise ValueError("Regiao e obrigatoria")
        if not self.credentials:
            raise ValueError("Credenciais sao obrigatorias")
        return True

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario (sem credenciais sensiveis)"""
        return {
            "provider": self.provider.value,
            "region": self.region,
            "tags": self.tags,
            "default_vpc": self.default_vpc,
            "default_subnet": self.default_subnet,
            "has_credentials": bool(self.credentials)
        }


@dataclass
class AWSConfig(CloudConfig):
    """Configuracao especifica para AWS"""
    access_key_id: str = ""
    secret_access_key: str = ""
    session_token: Optional[str] = None
    profile_name: Optional[str] = None

    def __post_init__(self):
        self.provider = ProviderType.AWS
        # Carregar de variaveis de ambiente se nao fornecido
        if not self.access_key_id:
            self.access_key_id = os.getenv("AWS_ACCESS_KEY_ID", "")
        if not self.secret_access_key:
            self.secret_access_key = os.getenv("AWS_SECRET_ACCESS_KEY", "")
        if not self.region:
            self.region = os.getenv("AWS_DEFAULT_REGION", "us-east-1")

        self.credentials = {
            "access_key_id": self.access_key_id,
            "secret_access_key": self.secret_access_key,
        }
        if self.session_token:
            self.credentials["session_token"] = self.session_token


@dataclass
class AzureConfig(CloudConfig):
    """Configuracao especifica para Azure"""
    subscription_id: str = ""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    resource_group: str = ""
    location: str = ""

    def __post_init__(self):
        self.provider = ProviderType.AZURE
        # Carregar de variaveis de ambiente se nao fornecido
        if not self.subscription_id:
            self.subscription_id = os.getenv("AZURE_SUBSCRIPTION_ID", "")
        if not self.tenant_id:
            self.tenant_id = os.getenv("AZURE_TENANT_ID", "")
        if not self.client_id:
            self.client_id = os.getenv("AZURE_CLIENT_ID", "")
        if not self.client_secret:
            self.client_secret = os.getenv("AZURE_CLIENT_SECRET", "")
        if not self.resource_group:
            self.resource_group = os.getenv("AZURE_RESOURCE_GROUP", "rg-fabrica")
        if not self.location:
            self.location = os.getenv("AZURE_LOCATION", "eastus")

        self.region = self.location
        self.credentials = {
            "subscription_id": self.subscription_id,
            "tenant_id": self.tenant_id,
            "client_id": self.client_id,
            "client_secret": self.client_secret,
        }


@dataclass
class GCPConfig(CloudConfig):
    """Configuracao especifica para GCP"""
    project_id: str = ""
    credentials_file: str = ""
    zone: str = ""

    def __post_init__(self):
        self.provider = ProviderType.GCP
        # Carregar de variaveis de ambiente se nao fornecido
        if not self.project_id:
            self.project_id = os.getenv("GCP_PROJECT_ID", "")
        if not self.credentials_file:
            self.credentials_file = os.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
        if not self.region:
            self.region = os.getenv("GCP_REGION", "us-central1")
        if not self.zone:
            self.zone = os.getenv("GCP_ZONE", "us-central1-a")

        self.credentials = {
            "project_id": self.project_id,
            "credentials_file": self.credentials_file,
        }


@dataclass
class CloudResource:
    """Representa um recurso cloud provisionado"""
    resource_id: str
    name: str
    resource_type: ResourceType
    provider: ProviderType
    region: str
    status: ResourceStatus = ResourceStatus.PENDING

    # Metadados
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None
    tags: Dict[str, str] = field(default_factory=dict)

    # Informacoes do recurso
    public_ip: Optional[str] = None
    private_ip: Optional[str] = None
    public_dns: Optional[str] = None
    endpoint: Optional[str] = None

    # Detalhes especificos do recurso
    details: Dict[str, Any] = field(default_factory=dict)

    # Custos
    hourly_cost: float = 0.0
    monthly_cost: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Converte recurso para dicionario"""
        return {
            "resource_id": self.resource_id,
            "name": self.name,
            "resource_type": self.resource_type.value,
            "provider": self.provider.value,
            "region": self.region,
            "status": self.status.value,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "tags": self.tags,
            "public_ip": self.public_ip,
            "private_ip": self.private_ip,
            "public_dns": self.public_dns,
            "endpoint": self.endpoint,
            "details": self.details,
            "hourly_cost": self.hourly_cost,
            "monthly_cost": self.monthly_cost,
        }


@dataclass
class CostEstimate:
    """Estimativa de custo para recursos cloud"""
    provider: ProviderType
    hourly_cost: float = 0.0
    daily_cost: float = 0.0
    monthly_cost: float = 0.0
    yearly_cost: float = 0.0
    currency: str = "USD"
    breakdown: Dict[str, float] = field(default_factory=dict)
    notes: List[str] = field(default_factory=list)

    def calculate_totals(self):
        """Calcula custos totais baseado no custo por hora"""
        self.daily_cost = self.hourly_cost * 24
        self.monthly_cost = self.daily_cost * 30
        self.yearly_cost = self.monthly_cost * 12

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "provider": self.provider.value,
            "hourly_cost": round(self.hourly_cost, 4),
            "daily_cost": round(self.daily_cost, 2),
            "monthly_cost": round(self.monthly_cost, 2),
            "yearly_cost": round(self.yearly_cost, 2),
            "currency": self.currency,
            "breakdown": {k: round(v, 4) for k, v in self.breakdown.items()},
            "notes": self.notes
        }


@dataclass
class DeploymentResult:
    """Resultado de um deployment"""
    success: bool
    provider: ProviderType
    stack_type: StackType
    resources: List[CloudResource] = field(default_factory=list)

    # URLs e endpoints
    application_url: Optional[str] = None
    api_endpoint: Optional[str] = None
    database_endpoint: Optional[str] = None

    # Informacoes de deploy
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    duration_seconds: float = 0.0

    # Custos
    cost_estimate: Optional[CostEstimate] = None

    # Logs e erros
    logs: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    # Terraform/IaC
    terraform_state: Optional[str] = None
    terraform_plan: Optional[str] = None

    def add_log(self, message: str):
        """Adiciona mensagem de log"""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.logs.append(f"[{timestamp}] {message}")

    def add_error(self, message: str):
        """Adiciona mensagem de erro"""
        self.errors.append(message)
        self.add_log(f"ERRO: {message}")

    def add_warning(self, message: str):
        """Adiciona mensagem de aviso"""
        self.warnings.append(message)
        self.add_log(f"AVISO: {message}")

    def add_resource(self, resource: CloudResource):
        """Adiciona recurso ao resultado"""
        self.resources.append(resource)
        self.add_log(f"Recurso criado: {resource.name} ({resource.resource_type.value})")

    def to_dict(self) -> Dict[str, Any]:
        """Converte resultado para dicionario"""
        return {
            "success": self.success,
            "provider": self.provider.value,
            "stack_type": self.stack_type.value,
            "resources": [r.to_dict() for r in self.resources],
            "application_url": self.application_url,
            "api_endpoint": self.api_endpoint,
            "database_endpoint": self.database_endpoint,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_seconds": self.duration_seconds,
            "cost_estimate": self.cost_estimate.to_dict() if self.cost_estimate else None,
            "logs": self.logs,
            "errors": self.errors,
            "warnings": self.warnings,
        }


class CloudProvider(ABC):
    """
    Interface base abstrata para provedores cloud.

    Todos os provedores (AWS, Azure, GCP) devem implementar esta interface
    para garantir uma API consistente e unificada.
    """

    def __init__(self, config: CloudConfig):
        """
        Inicializa o provider com a configuracao.

        Args:
            config: Configuracao do provider (credenciais, regiao, etc)
        """
        self.config = config
        self._connected = False
        self._last_error: Optional[str] = None

    @property
    def provider_type(self) -> ProviderType:
        """Retorna o tipo do provider"""
        return self.config.provider

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado ao provider"""
        return self._connected

    @property
    def last_error(self) -> Optional[str]:
        """Retorna ultimo erro ocorrido"""
        return self._last_error

    # =========================================================================
    # Conexao e Autenticacao
    # =========================================================================

    @abstractmethod
    async def connect(self) -> bool:
        """
        Conecta ao provider cloud.

        Returns:
            True se conectou com sucesso, False caso contrario
        """
        pass

    @abstractmethod
    async def disconnect(self) -> bool:
        """
        Desconecta do provider cloud.

        Returns:
            True se desconectou com sucesso
        """
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """
        Testa a conexao com o provider.

        Returns:
            True se a conexao esta funcionando
        """
        pass

    # =========================================================================
    # Computacao
    # =========================================================================

    @abstractmethod
    async def create_vm(
        self,
        name: str,
        instance_type: str,
        image_id: str,
        key_name: Optional[str] = None,
        security_groups: Optional[List[str]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria uma maquina virtual.

        Args:
            name: Nome da VM
            instance_type: Tipo da instancia (t2.micro, Standard_B1s, etc)
            image_id: ID da imagem (AMI, etc)
            key_name: Nome da chave SSH
            security_groups: Security groups a aplicar
            tags: Tags a aplicar

        Returns:
            CloudResource representando a VM criada
        """
        pass

    @abstractmethod
    async def start_vm(self, resource_id: str) -> bool:
        """Inicia uma VM parada"""
        pass

    @abstractmethod
    async def stop_vm(self, resource_id: str) -> bool:
        """Para uma VM em execucao"""
        pass

    @abstractmethod
    async def terminate_vm(self, resource_id: str) -> bool:
        """Termina (deleta) uma VM"""
        pass

    @abstractmethod
    async def get_vm_status(self, resource_id: str) -> ResourceStatus:
        """Obtem status de uma VM"""
        pass

    # =========================================================================
    # Serverless
    # =========================================================================

    @abstractmethod
    async def create_function(
        self,
        name: str,
        runtime: str,
        handler: str,
        code_path: str,
        memory_mb: int = 256,
        timeout_seconds: int = 30,
        environment: Optional[Dict[str, str]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria uma funcao serverless.

        Args:
            name: Nome da funcao
            runtime: Runtime (python3.9, nodejs18.x, etc)
            handler: Handler da funcao
            code_path: Caminho para o codigo (zip)
            memory_mb: Memoria em MB
            timeout_seconds: Timeout em segundos
            environment: Variaveis de ambiente
            tags: Tags a aplicar

        Returns:
            CloudResource representando a funcao criada
        """
        pass

    @abstractmethod
    async def invoke_function(
        self,
        function_name: str,
        payload: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Invoca uma funcao serverless"""
        pass

    @abstractmethod
    async def delete_function(self, function_name: str) -> bool:
        """Deleta uma funcao serverless"""
        pass

    # =========================================================================
    # Storage
    # =========================================================================

    @abstractmethod
    async def create_bucket(
        self,
        name: str,
        public: bool = False,
        versioning: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um bucket de armazenamento de objetos.

        Args:
            name: Nome do bucket (deve ser unico globalmente)
            public: Se o bucket deve ser publico
            versioning: Habilitar versionamento
            tags: Tags a aplicar

        Returns:
            CloudResource representando o bucket criado
        """
        pass

    @abstractmethod
    async def upload_file(
        self,
        bucket_name: str,
        file_path: str,
        object_key: str,
        content_type: Optional[str] = None
    ) -> str:
        """
        Faz upload de um arquivo para o bucket.

        Returns:
            URL do objeto
        """
        pass

    @abstractmethod
    async def delete_bucket(self, bucket_name: str, force: bool = False) -> bool:
        """Deleta um bucket (force=True deleta objetos dentro)"""
        pass

    @abstractmethod
    async def list_buckets(self) -> List[CloudResource]:
        """Lista todos os buckets"""
        pass

    # =========================================================================
    # Banco de Dados
    # =========================================================================

    @abstractmethod
    async def create_database(
        self,
        name: str,
        engine: str,
        engine_version: str,
        instance_class: str,
        storage_gb: int = 20,
        username: str = "admin",
        password: Optional[str] = None,
        publicly_accessible: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um banco de dados gerenciado.

        Args:
            name: Nome do banco
            engine: Engine (postgres, mysql, etc)
            engine_version: Versao do engine
            instance_class: Classe da instancia
            storage_gb: Tamanho do storage em GB
            username: Usuario admin
            password: Senha (gerada automaticamente se None)
            publicly_accessible: Se acessivel publicamente
            tags: Tags a aplicar

        Returns:
            CloudResource representando o banco criado
        """
        pass

    @abstractmethod
    async def delete_database(
        self,
        database_id: str,
        skip_final_snapshot: bool = False
    ) -> bool:
        """Deleta um banco de dados"""
        pass

    @abstractmethod
    async def get_database_status(self, database_id: str) -> ResourceStatus:
        """Obtem status do banco de dados"""
        pass

    # =========================================================================
    # Rede
    # =========================================================================

    @abstractmethod
    async def create_security_group(
        self,
        name: str,
        description: str,
        vpc_id: Optional[str] = None,
        rules: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um security group/firewall.

        Args:
            name: Nome do security group
            description: Descricao
            vpc_id: VPC onde criar (usa default se None)
            rules: Regras de ingress/egress
            tags: Tags a aplicar

        Returns:
            CloudResource representando o security group
        """
        pass

    @abstractmethod
    async def delete_security_group(self, security_group_id: str) -> bool:
        """Deleta um security group"""
        pass

    # =========================================================================
    # Deploy Completo
    # =========================================================================

    @abstractmethod
    async def deploy_stack(
        self,
        project_name: str,
        stack_type: StackType,
        config: Dict[str, Any]
    ) -> DeploymentResult:
        """
        Faz deploy de uma stack completa.

        Args:
            project_name: Nome do projeto
            stack_type: Tipo da stack (simple, microservices, serverless)
            config: Configuracoes especificas da stack

        Returns:
            DeploymentResult com todos os recursos criados
        """
        pass

    @abstractmethod
    async def teardown_stack(
        self,
        project_name: str,
        force: bool = False
    ) -> bool:
        """
        Desfaz uma stack completa.

        Args:
            project_name: Nome do projeto
            force: Forcar remocao mesmo com erros

        Returns:
            True se removeu com sucesso
        """
        pass

    # =========================================================================
    # Custos
    # =========================================================================

    @abstractmethod
    async def estimate_cost(
        self,
        stack_type: StackType,
        config: Dict[str, Any]
    ) -> CostEstimate:
        """
        Estima custo de uma stack antes do deploy.

        Args:
            stack_type: Tipo da stack
            config: Configuracoes da stack

        Returns:
            CostEstimate com estimativas de custo
        """
        pass

    @abstractmethod
    async def get_current_costs(self) -> Dict[str, float]:
        """
        Obtem custos atuais dos recursos.

        Returns:
            Dicionario com custos por servico
        """
        pass

    # =========================================================================
    # Listagem e Monitoramento
    # =========================================================================

    @abstractmethod
    async def list_resources(
        self,
        resource_type: Optional[ResourceType] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> List[CloudResource]:
        """
        Lista recursos do provider.

        Args:
            resource_type: Filtrar por tipo de recurso
            tags: Filtrar por tags

        Returns:
            Lista de CloudResource
        """
        pass

    @abstractmethod
    async def get_resource(self, resource_id: str) -> Optional[CloudResource]:
        """
        Obtem um recurso especifico.

        Args:
            resource_id: ID do recurso

        Returns:
            CloudResource ou None se nao encontrado
        """
        pass

    # =========================================================================
    # Metodos de Utilidade
    # =========================================================================

    def get_status(self) -> Dict[str, Any]:
        """Retorna status do provider"""
        return {
            "provider": self.provider_type.value,
            "connected": self.is_connected,
            "region": self.config.region,
            "last_error": self._last_error,
        }

    def _log(self, message: str, level: str = "info"):
        """Log interno do provider"""
        log_func = getattr(logger, level, logger.info)
        log_func(f"[{self.provider_type.value.upper()}] {message}")

    def _set_error(self, error: str):
        """Define ultimo erro"""
        self._last_error = error
        self._log(error, "error")
