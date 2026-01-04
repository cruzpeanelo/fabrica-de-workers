# -*- coding: utf-8 -*-
"""
Azure Cloud Provider
====================
Implementacao do provider Azure para a Plataforma E.

Integra todos os servicos Azure:
- Virtual Machines (Computacao)
- Blob Storage (Storage)
- Functions (Serverless)
- Database for PostgreSQL/MySQL (Banco de dados)
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

from ..base_provider import (
    CloudProvider,
    CloudResource,
    ResourceType,
    ResourceStatus,
    DeploymentResult,
    CostEstimate,
    StackType,
    AzureConfig,
    ProviderType,
)
from .virtual_machines import VMManager
from .storage import StorageManager
from .functions import FunctionsManager
from .database import DatabaseManager

logger = logging.getLogger(__name__)

# Flag para indicar se Azure SDK esta disponivel
AZURE_SDK_AVAILABLE = False
try:
    from azure.identity import ClientSecretCredential
    from azure.mgmt.compute import ComputeManagementClient
    from azure.mgmt.network import NetworkManagementClient
    from azure.mgmt.storage import StorageManagementClient
    from azure.mgmt.web import WebSiteManagementClient
    from azure.mgmt.rdbms.postgresql_flexibleservers import PostgreSQLManagementClient
    from azure.mgmt.rdbms.mysql_flexibleservers import MySQLManagementClient
    from azure.storage.blob import BlobServiceClient
    AZURE_SDK_AVAILABLE = True
except ImportError:
    logger.warning(
        "Azure SDK nao instalado. Instale com: "
        "pip install azure-identity azure-mgmt-compute azure-mgmt-network "
        "azure-mgmt-storage azure-mgmt-web azure-mgmt-rdbms azure-storage-blob"
    )


class AzureProvider(CloudProvider):
    """
    Provider Azure para a Plataforma E.

    Implementa a interface CloudProvider para Azure,
    integrando todos os servicos necessarios.
    """

    def __init__(self, config: AzureConfig):
        """
        Inicializa o provider Azure.

        Args:
            config: Configuracao Azure com credenciais
        """
        super().__init__(config)
        self.config: AzureConfig = config

        # Clientes Azure (inicializados em connect)
        self._credential = None
        self._compute_client = None
        self._network_client = None
        self._storage_client = None
        self._web_client = None
        self._postgresql_client = None
        self._mysql_client = None
        self._blob_service_client = None

        # Managers de servicos
        self.vm: Optional[VMManager] = None
        self.storage: Optional[StorageManager] = None
        self.functions: Optional[FunctionsManager] = None
        self.database: Optional[DatabaseManager] = None

    # =========================================================================
    # Conexao e Autenticacao
    # =========================================================================

    async def connect(self) -> bool:
        """
        Conecta ao Azure usando as credenciais configuradas.

        Returns:
            True se conectou com sucesso
        """
        if not AZURE_SDK_AVAILABLE:
            self._set_error("Azure SDK nao esta instalado")
            return False

        try:
            # Criar credencial
            self._credential = ClientSecretCredential(
                tenant_id=self.config.tenant_id,
                client_id=self.config.client_id,
                client_secret=self.config.client_secret
            )

            subscription_id = self.config.subscription_id

            # Criar clientes de gerenciamento
            self._compute_client = ComputeManagementClient(
                self._credential, subscription_id
            )
            self._network_client = NetworkManagementClient(
                self._credential, subscription_id
            )
            self._storage_client = StorageManagementClient(
                self._credential, subscription_id
            )
            self._web_client = WebSiteManagementClient(
                self._credential, subscription_id
            )
            self._postgresql_client = PostgreSQLManagementClient(
                self._credential, subscription_id
            )
            self._mysql_client = MySQLManagementClient(
                self._credential, subscription_id
            )

            # Testar conexao listando locations
            list(self._compute_client.virtual_machines.list_all())

            # Inicializar managers
            self.vm = VMManager(
                self._compute_client,
                self._network_client,
                self.config
            )

            # Storage manager precisa de blob service client
            # (sera inicializado quando criar storage account)
            self.storage = StorageManager(
                self._storage_client,
                None,  # Blob client sera configurado depois
                self.config
            )

            self.functions = FunctionsManager(
                self._web_client,
                self._storage_client,
                self.config
            )

            self.database = DatabaseManager(
                self._postgresql_client,
                self._mysql_client,
                self.config
            )

            self._connected = True
            self._log("Conectado ao Azure com sucesso")
            return True

        except Exception as e:
            self._set_error(f"Erro ao conectar ao Azure: {e}")
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do Azure.

        Returns:
            True sempre
        """
        self._credential = None
        self._compute_client = None
        self._network_client = None
        self._storage_client = None
        self._web_client = None
        self._postgresql_client = None
        self._mysql_client = None
        self._blob_service_client = None

        self.vm = None
        self.storage = None
        self.functions = None
        self.database = None

        self._connected = False
        self._log("Desconectado do Azure")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o Azure.

        Returns:
            True se a conexao esta funcionando
        """
        if not self._connected:
            return await self.connect()

        try:
            # Testar listando VMs
            list(self._compute_client.virtual_machines.list_all())
            return True
        except Exception as e:
            self._set_error(f"Falha no teste de conexao: {e}")
            return False

    # =========================================================================
    # Computacao (Virtual Machines)
    # =========================================================================

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
        Cria uma VM Azure.

        Args:
            name: Nome da VM
            instance_type: Tamanho (Standard_B1s, etc)
            image_id: Referencia da imagem
            key_name: Chave SSH publica
            security_groups: NSGs (nao usado diretamente)
            tags: Tags adicionais

        Returns:
            CloudResource representando a VM
        """
        if not self.vm:
            raise RuntimeError("Provider nao conectado")

        result = await self.vm.create_vm(
            name=name,
            vm_size=instance_type,
            ssh_public_key=key_name,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["vm_id"],
            name=name,
            resource_type=ResourceType.VM,
            provider=ProviderType.AZURE,
            region=self.config.location,
            status=ResourceStatus.RUNNING,
            public_ip=result.get("public_ip"),
            private_ip=result.get("private_ip"),
            hourly_cost=result.get("hourly_cost", 0),
            monthly_cost=result.get("hourly_cost", 0) * 24 * 30,
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "vm_size": instance_type,
                "admin_username": result.get("admin_username"),
            }
        )

    async def start_vm(self, resource_id: str) -> bool:
        """Inicia uma VM"""
        if not self.vm:
            return False
        name = resource_id.split("/")[-1]
        return await self.vm.start_vm(name)

    async def stop_vm(self, resource_id: str) -> bool:
        """Para uma VM"""
        if not self.vm:
            return False
        name = resource_id.split("/")[-1]
        return await self.vm.stop_vm(name)

    async def terminate_vm(self, resource_id: str) -> bool:
        """Deleta uma VM"""
        if not self.vm:
            return False
        name = resource_id.split("/")[-1]
        return await self.vm.delete_vm(name)

    async def get_vm_status(self, resource_id: str) -> ResourceStatus:
        """Obtem status de uma VM"""
        if not self.vm:
            return ResourceStatus.UNKNOWN

        name = resource_id.split("/")[-1]
        info = await self.vm.get_vm(name)
        status_map = {
            "running": ResourceStatus.RUNNING,
            "starting": ResourceStatus.CREATING,
            "stopping": ResourceStatus.STOPPING,
            "stopped": ResourceStatus.STOPPED,
            "deallocated": ResourceStatus.STOPPED,
            "deallocating": ResourceStatus.STOPPING,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Serverless (Functions)
    # =========================================================================

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
        Cria uma Azure Function App.

        Args:
            name: Nome da Function App
            runtime: Runtime (python, node)
            handler: Handler (nao usado - Azure usa convencoes)
            code_path: Caminho do codigo (para deploy posterior)
            memory_mb: Memoria (nao configuravel em consumption)
            timeout_seconds: Timeout
            environment: Variaveis de ambiente
            tags: Tags adicionais

        Returns:
            CloudResource representando a Function App
        """
        if not self.functions:
            raise RuntimeError("Provider nao conectado")

        # Mapear runtime
        runtime_map = {
            "python3.9": ("python", "3.9"),
            "python3.10": ("python", "3.10"),
            "python3.11": ("python", "3.11"),
            "nodejs18.x": ("node", "18"),
            "nodejs20.x": ("node", "20"),
        }
        runtime_name, runtime_version = runtime_map.get(
            runtime, ("python", "3.11")
        )

        result = await self.functions.create_function_app(
            name=name,
            runtime=runtime_name,
            runtime_version=runtime_version,
            app_settings=environment,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["app_id"],
            name=name,
            resource_type=ResourceType.SERVERLESS,
            provider=ProviderType.AZURE,
            region=self.config.location,
            status=ResourceStatus.RUNNING,
            endpoint=result.get("url"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "runtime": runtime_name,
                "runtime_version": runtime_version,
            }
        )

    async def invoke_function(
        self,
        function_name: str,
        payload: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Invoca uma Azure Function"""
        if not self.functions:
            return {"error": "Provider nao conectado"}
        # Azure Functions sao invocadas por HTTP
        return await self.functions.invoke_function(function_name, function_name, payload)

    async def delete_function(self, function_name: str) -> bool:
        """Deleta uma Function App"""
        if not self.functions:
            return False
        return await self.functions.delete_function_app(function_name)

    # =========================================================================
    # Storage (Blob Storage)
    # =========================================================================

    async def create_bucket(
        self,
        name: str,
        public: bool = False,
        versioning: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um container no Blob Storage.

        Args:
            name: Nome do container
            public: Se publico
            versioning: Versionamento (precisa de storage account)
            tags: Tags adicionais

        Returns:
            CloudResource representando o container
        """
        if not self.storage:
            raise RuntimeError("Provider nao conectado")

        # Primeiro criar storage account se nao existir
        account_name = f"fabrica{name.lower().replace('-', '')}"[:24]

        account_result = await self.storage.create_storage_account(
            name=account_name,
            allow_blob_public_access=public,
            tags=tags
        )

        if not account_result.get("success"):
            raise RuntimeError(account_result.get("error", "Erro desconhecido"))

        # Configurar blob service client
        connection_string = (
            f"DefaultEndpointsProtocol=https;"
            f"AccountName={account_name};"
            f"AccountKey={account_result.get('primary_key')};"
            f"EndpointSuffix=core.windows.net"
        )
        self._blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        self.storage.blob_service_client = self._blob_service_client

        # Criar container
        container_result = await self.storage.create_container(
            account_name,
            name,
            public_access="blob" if public else None
        )

        return CloudResource(
            resource_id=account_result["account_id"],
            name=name,
            resource_type=ResourceType.OBJECT_STORAGE,
            provider=ProviderType.AZURE,
            region=self.config.location,
            status=ResourceStatus.RUNNING,
            endpoint=account_result.get("primary_endpoint"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "account_name": account_name,
                "container_name": name,
                "public": public,
            }
        )

    async def upload_file(
        self,
        bucket_name: str,
        file_path: str,
        object_key: str,
        content_type: Optional[str] = None
    ) -> str:
        """Faz upload de arquivo para Blob Storage"""
        if not self.storage or not self.storage.blob_service_client:
            raise RuntimeError("Storage nao configurado")

        result = await self.storage.upload_blob(
            container_name=bucket_name,
            blob_name=object_key,
            file_path=file_path,
            content_type=content_type
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return result.get("url", "")

    async def delete_bucket(self, bucket_name: str, force: bool = False) -> bool:
        """Deleta um container"""
        if not self.storage:
            return False
        return await self.storage.delete_container(bucket_name, force)

    async def list_buckets(self) -> List[CloudResource]:
        """Lista containers"""
        if not self.storage:
            return []

        accounts = await self.storage.list_storage_accounts()
        resources = []

        for account in accounts:
            resources.append(CloudResource(
                resource_id=account["id"],
                name=account["name"],
                resource_type=ResourceType.OBJECT_STORAGE,
                provider=ProviderType.AZURE,
                region=account.get("location", self.config.location),
                status=ResourceStatus.RUNNING,
            ))

        return resources

    # =========================================================================
    # Banco de Dados
    # =========================================================================

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
        Cria servidor de banco de dados Azure.

        Args:
            name: Nome do servidor
            engine: Engine (postgres, mysql)
            engine_version: Versao
            instance_class: SKU
            storage_gb: Storage em GB
            username: Usuario admin
            password: Senha
            publicly_accessible: Acesso publico
            tags: Tags adicionais

        Returns:
            CloudResource representando o banco
        """
        if not self.database:
            raise RuntimeError("Provider nao conectado")

        if engine in ["postgres", "postgresql"]:
            result = await self.database.create_postgresql_server(
                name=name,
                sku_name=instance_class,
                storage_gb=storage_gb,
                version=engine_version,
                admin_username=username,
                admin_password=password,
                public_access=publicly_accessible,
                tags=tags
            )
        else:  # mysql
            result = await self.database.create_mysql_server(
                name=name,
                sku_name=instance_class,
                storage_gb=storage_gb,
                version=engine_version,
                admin_username=username,
                admin_password=password,
                public_access=publicly_accessible,
                tags=tags
            )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["server_id"],
            name=name,
            resource_type=ResourceType.DATABASE,
            provider=ProviderType.AZURE,
            region=self.config.location,
            status=ResourceStatus.CREATING,
            endpoint=result.get("fqdn"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "engine": engine,
                "engine_version": engine_version,
                "sku": instance_class,
                "storage_gb": storage_gb,
                "username": username,
                "password": result.get("admin_password"),
                "port": result.get("port"),
                "connection_string": result.get("connection_string"),
            }
        )

    async def delete_database(
        self,
        database_id: str,
        skip_final_snapshot: bool = False
    ) -> bool:
        """Deleta servidor de banco"""
        if not self.database:
            return False
        name = database_id.split("/")[-1]
        # Detectar engine pelo nome ou parametro
        return await self.database.delete_server(name, "postgresql")

    async def get_database_status(self, database_id: str) -> ResourceStatus:
        """Obtem status do banco"""
        if not self.database:
            return ResourceStatus.UNKNOWN

        name = database_id.split("/")[-1]
        info = await self.database.get_server(name)
        status_map = {
            "Ready": ResourceStatus.RUNNING,
            "Creating": ResourceStatus.CREATING,
            "Deleting": ResourceStatus.DELETING,
            "Updating": ResourceStatus.UPDATING,
            "Starting": ResourceStatus.CREATING,
            "Stopping": ResourceStatus.STOPPING,
            "Stopped": ResourceStatus.STOPPED,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Rede (NSG)
    # =========================================================================

    async def create_security_group(
        self,
        name: str,
        description: str,
        vpc_id: Optional[str] = None,
        rules: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """Cria Network Security Group"""
        if not self.vm:
            raise RuntimeError("Provider nao conectado")

        result = await self.vm.create_nsg(
            name=name,
            rules=rules,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["nsg_id"],
            name=name,
            resource_type=ResourceType.FIREWALL,
            provider=ProviderType.AZURE,
            region=self.config.location,
            status=ResourceStatus.RUNNING,
            created_at=datetime.now(),
            tags=tags or {},
        )

    async def delete_security_group(self, security_group_id: str) -> bool:
        """Deleta NSG"""
        if not self.vm:
            return False
        name = security_group_id.split("/")[-1]
        return await self.vm.delete_nsg(name)

    # =========================================================================
    # Deploy de Stack
    # =========================================================================

    async def deploy_stack(
        self,
        project_name: str,
        stack_type: StackType,
        config: Dict[str, Any]
    ) -> DeploymentResult:
        """
        Faz deploy de uma stack completa no Azure.

        Args:
            project_name: Nome do projeto
            stack_type: Tipo da stack
            config: Configuracoes

        Returns:
            DeploymentResult com recursos criados
        """
        result = DeploymentResult(
            success=False,
            provider=ProviderType.AZURE,
            stack_type=stack_type,
            started_at=datetime.now()
        )

        if not self._connected:
            result.add_error("Provider nao conectado")
            return result

        try:
            if stack_type == StackType.SIMPLE:
                await self._deploy_simple_stack(project_name, config, result)
            elif stack_type == StackType.SERVERLESS:
                await self._deploy_serverless_stack(project_name, config, result)
            else:
                result.add_error(f"Tipo de stack nao suportado: {stack_type}")
                return result

            result.success = len(result.errors) == 0
            result.completed_at = datetime.now()
            result.duration_seconds = (
                result.completed_at - result.started_at
            ).total_seconds()

            result.cost_estimate = await self.estimate_cost(stack_type, config)

        except Exception as e:
            result.add_error(f"Erro no deploy: {e}")

        return result

    async def _deploy_simple_stack(
        self,
        project_name: str,
        config: Dict[str, Any],
        result: DeploymentResult
    ):
        """Deploy de stack simples no Azure"""
        result.add_log(f"Iniciando deploy simples para {project_name}")

        # 1. Criar Storage Account
        try:
            storage = await self.create_bucket(
                name=f"{project_name}-storage",
                public=True,
                tags={"Project": project_name}
            )
            result.add_resource(storage)
        except Exception as e:
            result.add_warning(f"Storage nao criado: {e}")

        # 2. Criar NSG
        try:
            nsg = await self.create_security_group(
                name=f"{project_name}-nsg",
                description=f"NSG para {project_name}",
                tags={"Project": project_name}
            )
            result.add_resource(nsg)
        except Exception as e:
            result.add_warning(f"NSG nao criado: {e}")

        # 3. Criar VM
        vm_size = config.get("vm_size", "Standard_B1s")
        result.add_log(f"Criando VM: {vm_size}")

        try:
            vm = await self.create_vm(
                name=f"{project_name}-server",
                instance_type=vm_size,
                image_id="",
                tags={"Project": project_name}
            )
            result.add_resource(vm)
            result.application_url = f"http://{vm.public_ip}"
        except Exception as e:
            result.add_error(f"Falha ao criar VM: {e}")
            return

        # 4. Criar banco se configurado
        if config.get("database"):
            db_config = config["database"]
            try:
                db = await self.create_database(
                    name=f"{project_name}-db",
                    engine=db_config.get("engine", "postgres"),
                    engine_version=db_config.get("version", "15"),
                    instance_class=db_config.get("sku", "Standard_B1ms"),
                    storage_gb=db_config.get("storage_gb", 32),
                    tags={"Project": project_name}
                )
                result.add_resource(db)
                result.database_endpoint = db.endpoint
            except Exception as e:
                result.add_warning(f"Banco nao criado: {e}")

        result.add_log("Deploy simples concluido")

    async def _deploy_serverless_stack(
        self,
        project_name: str,
        config: Dict[str, Any],
        result: DeploymentResult
    ):
        """Deploy de stack serverless no Azure"""
        result.add_log(f"Iniciando deploy serverless para {project_name}")

        # Criar Function App
        try:
            func = await self.create_function(
                name=f"{project_name}-func",
                runtime=config.get("runtime", "python3.11"),
                handler="",
                code_path="",
                environment=config.get("environment"),
                tags={"Project": project_name}
            )
            result.add_resource(func)
            result.api_endpoint = func.endpoint
        except Exception as e:
            result.add_error(f"Falha ao criar Function App: {e}")

        result.add_log("Deploy serverless concluido")

    async def teardown_stack(
        self,
        project_name: str,
        force: bool = False
    ) -> bool:
        """Remove todos os recursos de uma stack"""
        if not self._connected:
            return False

        success = True
        resources = await self.list_resources(tags={"Project": project_name})

        self._log(f"Removendo {len(resources)} recursos do projeto {project_name}")

        for resource in resources:
            try:
                if resource.resource_type == ResourceType.VM:
                    await self.terminate_vm(resource.resource_id)
                elif resource.resource_type == ResourceType.OBJECT_STORAGE:
                    await self.delete_bucket(resource.name, force)
                elif resource.resource_type == ResourceType.SERVERLESS:
                    await self.delete_function(resource.name)
                elif resource.resource_type == ResourceType.DATABASE:
                    await self.delete_database(resource.resource_id)
                elif resource.resource_type == ResourceType.FIREWALL:
                    await self.delete_security_group(resource.resource_id)
            except Exception as e:
                self._log(f"Erro ao remover {resource.name}: {e}", "warning")
                success = False

        return success

    # =========================================================================
    # Custos
    # =========================================================================

    async def estimate_cost(
        self,
        stack_type: StackType,
        config: Dict[str, Any]
    ) -> CostEstimate:
        """Estima custo de uma stack"""
        estimate = CostEstimate(provider=ProviderType.AZURE)

        if stack_type == StackType.SIMPLE:
            vm_size = config.get("vm_size", "Standard_B1s")
            vm_hourly = self.vm.get_vm_pricing(vm_size) if self.vm else 0.02
            estimate.breakdown["vm"] = vm_hourly
            estimate.hourly_cost += vm_hourly

            # Storage
            storage_monthly = 2.0
            estimate.breakdown["storage"] = storage_monthly / (24 * 30)
            estimate.hourly_cost += storage_monthly / (24 * 30)

            # Database
            if config.get("database"):
                db_sku = config["database"].get("sku", "Standard_B1ms")
                db_hourly = self.database.get_sku_pricing(db_sku) if self.database else 0.03
                estimate.breakdown["database"] = db_hourly
                estimate.hourly_cost += db_hourly

        elif stack_type == StackType.SERVERLESS:
            if self.functions:
                func_cost = self.functions.estimate_cost(
                    config.get("monthly_invocations", 100000),
                    config.get("memory_mb", 256),
                    config.get("avg_duration_ms", 200)
                )
                estimate.breakdown["functions"] = func_cost["total_monthly_cost"] / (24 * 30)
                estimate.hourly_cost += func_cost["total_monthly_cost"] / (24 * 30)

        estimate.calculate_totals()
        estimate.notes.append("Estimativa baseada em precos da regiao East US")

        return estimate

    async def get_current_costs(self) -> Dict[str, float]:
        """Obtem custos atuais"""
        return {
            "note": "Custos em tempo real requerem Azure Cost Management API",
            "estimated_monthly": 0.0,
        }

    # =========================================================================
    # Listagem
    # =========================================================================

    async def list_resources(
        self,
        resource_type: Optional[ResourceType] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> List[CloudResource]:
        """Lista recursos Azure"""
        resources = []

        if not self._connected:
            return resources

        # VMs
        if not resource_type or resource_type == ResourceType.VM:
            if self.vm:
                vms = await self.vm.list_vms(tags=tags)
                for vm in vms:
                    resources.append(CloudResource(
                        resource_id=vm["vm_id"],
                        name=vm["name"],
                        resource_type=ResourceType.VM,
                        provider=ProviderType.AZURE,
                        region=vm.get("location", self.config.location),
                        status=ResourceStatus.RUNNING,
                        tags=vm.get("tags", {}),
                    ))

        # Storage
        if not resource_type or resource_type == ResourceType.OBJECT_STORAGE:
            storage_resources = await self.list_buckets()
            resources.extend(storage_resources)

        # Functions
        if not resource_type or resource_type == ResourceType.SERVERLESS:
            if self.functions:
                funcs = await self.functions.list_function_apps(tags=tags)
                for func in funcs:
                    resources.append(CloudResource(
                        resource_id=func["app_id"],
                        name=func["app_name"],
                        resource_type=ResourceType.SERVERLESS,
                        provider=ProviderType.AZURE,
                        region=func.get("location", self.config.location),
                        status=ResourceStatus.RUNNING,
                        endpoint=func.get("url"),
                    ))

        # Databases
        if not resource_type or resource_type == ResourceType.DATABASE:
            if self.database:
                dbs = await self.database.list_servers("postgresql")
                for db in dbs:
                    resources.append(CloudResource(
                        resource_id=db.get("server_id", db["server_name"]),
                        name=db["server_name"],
                        resource_type=ResourceType.DATABASE,
                        provider=ProviderType.AZURE,
                        region=self.config.location,
                        status=ResourceStatus.RUNNING,
                        endpoint=db.get("fqdn"),
                    ))

        return resources

    async def get_resource(self, resource_id: str) -> Optional[CloudResource]:
        """Obtem um recurso especifico"""
        resources = await self.list_resources()
        for resource in resources:
            if resource.resource_id == resource_id:
                return resource
        return None
