# -*- coding: utf-8 -*-
"""
GCP Cloud Provider
==================
Implementacao do provider Google Cloud Platform para a Plataforma E.

Integra todos os servicos GCP:
- Compute Engine (Computacao)
- Cloud Storage (Storage)
- Cloud Functions (Serverless)
- Cloud SQL (Banco de dados)
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import os

from ..base_provider import (
    CloudProvider,
    CloudResource,
    ResourceType,
    ResourceStatus,
    DeploymentResult,
    CostEstimate,
    StackType,
    GCPConfig,
    ProviderType,
)
from .compute import ComputeManager
from .storage import CloudStorageManager
from .functions import CloudFunctionsManager
from .sql import CloudSQLManager

logger = logging.getLogger(__name__)

# Flag para indicar se GCP SDK esta disponivel
GCP_SDK_AVAILABLE = False
try:
    from google.oauth2 import service_account
    from googleapiclient.discovery import build
    from google.cloud import storage
    from google.cloud import functions_v2
    GCP_SDK_AVAILABLE = True
except ImportError:
    logger.warning(
        "GCP SDK nao instalado. Instale com: "
        "pip install google-cloud-storage google-cloud-functions "
        "google-api-python-client google-auth"
    )


class GCPProvider(CloudProvider):
    """
    Provider GCP para a Plataforma E.

    Implementa a interface CloudProvider para Google Cloud Platform,
    integrando todos os servicos necessarios.
    """

    def __init__(self, config: GCPConfig):
        """
        Inicializa o provider GCP.

        Args:
            config: Configuracao GCP com credenciais
        """
        super().__init__(config)
        self.config: GCPConfig = config

        # Clientes GCP (inicializados em connect)
        self._credentials = None
        self._compute_client = None
        self._storage_client = None
        self._functions_client = None
        self._sql_admin_client = None

        # Managers de servicos
        self.compute: Optional[ComputeManager] = None
        self.storage: Optional[CloudStorageManager] = None
        self.functions: Optional[CloudFunctionsManager] = None
        self.sql: Optional[CloudSQLManager] = None

    # =========================================================================
    # Conexao e Autenticacao
    # =========================================================================

    async def connect(self) -> bool:
        """
        Conecta ao GCP usando as credenciais configuradas.

        Returns:
            True se conectou com sucesso
        """
        if not GCP_SDK_AVAILABLE:
            self._set_error("GCP SDK nao esta instalado")
            return False

        try:
            # Carregar credenciais do arquivo ou variavel de ambiente
            if self.config.credentials_file and os.path.exists(self.config.credentials_file):
                self._credentials = service_account.Credentials.from_service_account_file(
                    self.config.credentials_file,
                    scopes=[
                        "https://www.googleapis.com/auth/cloud-platform",
                        "https://www.googleapis.com/auth/compute",
                        "https://www.googleapis.com/auth/devstorage.full_control",
                        "https://www.googleapis.com/auth/sqlservice.admin",
                    ]
                )
            else:
                # Tentar usar Application Default Credentials
                from google.auth import default
                self._credentials, _ = default()

            # Criar clientes de API
            self._compute_client = build(
                "compute", "v1",
                credentials=self._credentials,
                cache_discovery=False
            )

            self._storage_client = storage.Client(
                project=self.config.project_id,
                credentials=self._credentials
            )

            self._functions_client = functions_v2.FunctionServiceClient(
                credentials=self._credentials
            )

            self._sql_admin_client = build(
                "sqladmin", "v1beta4",
                credentials=self._credentials,
                cache_discovery=False
            )

            # Testar conexao
            self._compute_client.projects().get(
                project=self.config.project_id
            ).execute()

            # Inicializar managers
            self.compute = ComputeManager(self._compute_client, self.config)
            self.storage = CloudStorageManager(self._storage_client, self.config)
            self.functions = CloudFunctionsManager(self._functions_client, self.config)
            self.sql = CloudSQLManager(self._sql_admin_client, self.config)

            self._connected = True
            self._log("Conectado ao GCP com sucesso")
            return True

        except Exception as e:
            self._set_error(f"Erro ao conectar ao GCP: {e}")
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do GCP.

        Returns:
            True sempre
        """
        self._credentials = None
        self._compute_client = None
        self._storage_client = None
        self._functions_client = None
        self._sql_admin_client = None

        self.compute = None
        self.storage = None
        self.functions = None
        self.sql = None

        self._connected = False
        self._log("Desconectado do GCP")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o GCP.

        Returns:
            True se a conexao esta funcionando
        """
        if not self._connected:
            return await self.connect()

        try:
            self._compute_client.projects().get(
                project=self.config.project_id
            ).execute()
            return True
        except Exception as e:
            self._set_error(f"Falha no teste de conexao: {e}")
            return False

    # =========================================================================
    # Computacao (Compute Engine)
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
        Cria uma instancia Compute Engine.

        Args:
            name: Nome da instancia
            instance_type: Tipo de maquina (e2-micro, etc)
            image_id: URL da imagem
            key_name: Nao usado (use metadata ssh-keys)
            security_groups: Tags de rede
            tags: Labels

        Returns:
            CloudResource representando a instancia
        """
        if not self.compute:
            raise RuntimeError("Provider nao conectado")

        result = await self.compute.create_instance(
            name=name,
            machine_type=instance_type,
            image=image_id if image_id else None,
            tags=security_groups,
            labels=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result.get("instance_id", name),
            name=name,
            resource_type=ResourceType.VM,
            provider=ProviderType.GCP,
            region=self.config.zone,
            status=ResourceStatus.RUNNING,
            public_ip=result.get("external_ip"),
            private_ip=result.get("internal_ip"),
            hourly_cost=result.get("hourly_cost", 0),
            monthly_cost=result.get("hourly_cost", 0) * 24 * 30,
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "machine_type": instance_type,
                "zone": self.config.zone,
            }
        )

    async def start_vm(self, resource_id: str) -> bool:
        """Inicia uma instancia"""
        if not self.compute:
            return False
        return await self.compute.start_instance(resource_id)

    async def stop_vm(self, resource_id: str) -> bool:
        """Para uma instancia"""
        if not self.compute:
            return False
        return await self.compute.stop_instance(resource_id)

    async def terminate_vm(self, resource_id: str) -> bool:
        """Deleta uma instancia"""
        if not self.compute:
            return False
        return await self.compute.delete_instance(resource_id)

    async def get_vm_status(self, resource_id: str) -> ResourceStatus:
        """Obtem status de uma instancia"""
        if not self.compute:
            return ResourceStatus.UNKNOWN

        info = await self.compute.get_instance(resource_id)
        status_map = {
            "RUNNING": ResourceStatus.RUNNING,
            "STAGING": ResourceStatus.CREATING,
            "PROVISIONING": ResourceStatus.CREATING,
            "STOPPING": ResourceStatus.STOPPING,
            "STOPPED": ResourceStatus.STOPPED,
            "TERMINATED": ResourceStatus.DELETED,
            "SUSPENDING": ResourceStatus.STOPPING,
            "SUSPENDED": ResourceStatus.STOPPED,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Serverless (Cloud Functions)
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
        Cria uma Cloud Function.

        Args:
            name: Nome da funcao
            runtime: Runtime (python311, nodejs18)
            handler: Entry point
            code_path: Caminho do codigo
            memory_mb: Memoria em MB
            timeout_seconds: Timeout
            environment: Variaveis de ambiente
            tags: Labels

        Returns:
            CloudResource representando a funcao
        """
        if not self.functions:
            raise RuntimeError("Provider nao conectado")

        result = await self.functions.create_function(
            name=name,
            runtime=runtime,
            entry_point=handler,
            memory_mb=memory_mb,
            timeout_seconds=timeout_seconds,
            environment_variables=environment,
            labels=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result.get("full_name", name),
            name=name,
            resource_type=ResourceType.SERVERLESS,
            provider=ProviderType.GCP,
            region=self.config.region,
            status=ResourceStatus.RUNNING,
            endpoint=result.get("url"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "runtime": runtime,
                "entry_point": handler,
                "memory_mb": memory_mb,
            }
        )

    async def invoke_function(
        self,
        function_name: str,
        payload: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Invoca uma Cloud Function"""
        if not self.functions:
            return {"error": "Provider nao conectado"}
        return await self.functions.invoke_function(function_name, payload)

    async def delete_function(self, function_name: str) -> bool:
        """Deleta uma Cloud Function"""
        if not self.functions:
            return False
        return await self.functions.delete_function(function_name)

    # =========================================================================
    # Storage (Cloud Storage)
    # =========================================================================

    async def create_bucket(
        self,
        name: str,
        public: bool = False,
        versioning: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um bucket Cloud Storage.

        Args:
            name: Nome do bucket
            public: Se publico
            versioning: Habilitar versionamento
            tags: Labels

        Returns:
            CloudResource representando o bucket
        """
        if not self.storage:
            raise RuntimeError("Provider nao conectado")

        result = await self.storage.create_bucket(
            name=name,
            versioning=versioning,
            labels=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        # Tornar publico se solicitado
        if public:
            await self.storage.make_bucket_public(name)

        return CloudResource(
            resource_id=name,
            name=name,
            resource_type=ResourceType.OBJECT_STORAGE,
            provider=ProviderType.GCP,
            region=result.get("location", self.config.region),
            status=ResourceStatus.RUNNING,
            endpoint=result.get("public_url"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "storage_class": result.get("storage_class", "STANDARD"),
                "versioning": versioning,
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
        """Faz upload de arquivo para Cloud Storage"""
        if not self.storage:
            raise RuntimeError("Provider nao conectado")

        result = await self.storage.upload_file(
            bucket_name=bucket_name,
            file_path=file_path,
            blob_name=object_key,
            content_type=content_type
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return result.get("public_url") or result.get("url", "")

    async def delete_bucket(self, bucket_name: str, force: bool = False) -> bool:
        """Deleta um bucket"""
        if not self.storage:
            return False
        return await self.storage.delete_bucket(bucket_name, force)

    async def list_buckets(self) -> List[CloudResource]:
        """Lista buckets"""
        if not self.storage:
            return []

        buckets = await self.storage.list_buckets()
        resources = []

        for bucket in buckets:
            resources.append(CloudResource(
                resource_id=bucket["name"],
                name=bucket["name"],
                resource_type=ResourceType.OBJECT_STORAGE,
                provider=ProviderType.GCP,
                region=bucket.get("location", self.config.region),
                status=ResourceStatus.RUNNING,
                created_at=bucket.get("created"),
                tags=bucket.get("labels", {}),
            ))

        return resources

    # =========================================================================
    # Banco de Dados (Cloud SQL)
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
        Cria instancia Cloud SQL.

        Args:
            name: Nome da instancia
            engine: Engine (postgres, mysql)
            engine_version: Versao
            instance_class: Tier
            storage_gb: Storage em GB
            username: Usuario (ignorado - usa root)
            password: Senha root
            publicly_accessible: IP publico
            tags: Labels

        Returns:
            CloudResource representando o banco
        """
        if not self.sql:
            raise RuntimeError("Provider nao conectado")

        # Mapear versao
        if engine in ["postgres", "postgresql"]:
            db_version = f"POSTGRES_{engine_version.replace('.', '_').split('_')[0]}"
        else:
            db_version = f"MYSQL_{engine_version.replace('.', '_')}"

        result = await self.sql.create_instance(
            name=name,
            database_version=db_version,
            tier=instance_class,
            storage_gb=storage_gb,
            root_password=password,
            public_ip=publicly_accessible,
            labels=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=name,
            name=name,
            resource_type=ResourceType.DATABASE,
            provider=ProviderType.GCP,
            region=result.get("region", self.config.region),
            status=ResourceStatus.RUNNING,
            endpoint=result.get("public_ip"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "database_version": db_version,
                "tier": instance_class,
                "storage_gb": storage_gb,
                "root_password": result.get("root_password"),
                "port": result.get("port"),
                "connection_name": result.get("connection_name"),
            }
        )

    async def delete_database(
        self,
        database_id: str,
        skip_final_snapshot: bool = False
    ) -> bool:
        """Deleta instancia Cloud SQL"""
        if not self.sql:
            return False
        return await self.sql.delete_instance(database_id)

    async def get_database_status(self, database_id: str) -> ResourceStatus:
        """Obtem status do banco"""
        if not self.sql:
            return ResourceStatus.UNKNOWN

        info = await self.sql.get_instance(database_id)
        status_map = {
            "RUNNABLE": ResourceStatus.RUNNING,
            "PENDING_CREATE": ResourceStatus.CREATING,
            "MAINTENANCE": ResourceStatus.UPDATING,
            "SUSPENDED": ResourceStatus.STOPPED,
            "FAILED": ResourceStatus.ERROR,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Rede (Firewall Rules)
    # =========================================================================

    async def create_security_group(
        self,
        name: str,
        description: str,
        vpc_id: Optional[str] = None,
        rules: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """Cria regra de firewall"""
        if not self.compute:
            raise RuntimeError("Provider nao conectado")

        # Converter regras para formato GCP
        allowed = []
        if rules:
            for rule in rules:
                allowed.append({
                    "IPProtocol": rule.get("protocol", "tcp"),
                    "ports": [str(rule.get("port", 80))],
                })
        else:
            # Regras padrao: HTTP, HTTPS, SSH
            allowed = [
                {"IPProtocol": "tcp", "ports": ["22"]},
                {"IPProtocol": "tcp", "ports": ["80"]},
                {"IPProtocol": "tcp", "ports": ["443"]},
            ]

        result = await self.compute.create_firewall_rule(
            name=name,
            allowed=allowed,
            description=description,
            target_tags=[name]
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=name,
            name=name,
            resource_type=ResourceType.FIREWALL,
            provider=ProviderType.GCP,
            region="global",
            status=ResourceStatus.RUNNING,
            created_at=datetime.now(),
            tags=tags or {},
        )

    async def delete_security_group(self, security_group_id: str) -> bool:
        """Deleta regra de firewall"""
        if not self.compute:
            return False
        return await self.compute.delete_firewall_rule(security_group_id)

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
        Faz deploy de uma stack completa no GCP.

        Args:
            project_name: Nome do projeto
            stack_type: Tipo da stack
            config: Configuracoes

        Returns:
            DeploymentResult com recursos criados
        """
        result = DeploymentResult(
            success=False,
            provider=ProviderType.GCP,
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
        """Deploy de stack simples no GCP"""
        result.add_log(f"Iniciando deploy simples para {project_name}")

        # 1. Criar bucket
        try:
            bucket = await self.create_bucket(
                name=f"{project_name}-assets".lower().replace("_", "-"),
                public=True,
                tags={"project": project_name}
            )
            result.add_resource(bucket)
        except Exception as e:
            result.add_warning(f"Bucket nao criado: {e}")

        # 2. Criar firewall rule
        try:
            firewall = await self.create_security_group(
                name=f"{project_name}-allow-web",
                description=f"Firewall rules para {project_name}",
                tags={"project": project_name}
            )
            result.add_resource(firewall)
        except Exception as e:
            result.add_warning(f"Firewall nao criado: {e}")

        # 3. Criar instancia
        machine_type = config.get("machine_type", "e2-micro")
        result.add_log(f"Criando instancia: {machine_type}")

        try:
            vm = await self.create_vm(
                name=f"{project_name}-server",
                instance_type=machine_type,
                image_id="",
                tags={"project": project_name}
            )
            result.add_resource(vm)
            result.application_url = f"http://{vm.public_ip}"
        except Exception as e:
            result.add_error(f"Falha ao criar instancia: {e}")
            return

        # 4. Criar banco se configurado
        if config.get("database"):
            db_config = config["database"]
            try:
                db = await self.create_database(
                    name=f"{project_name}-db".lower().replace("_", "-"),
                    engine=db_config.get("engine", "postgres"),
                    engine_version=db_config.get("version", "15"),
                    instance_class=db_config.get("tier", "db-f1-micro"),
                    storage_gb=db_config.get("storage_gb", 10),
                    publicly_accessible=True,
                    tags={"project": project_name}
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
        """Deploy de stack serverless no GCP"""
        result.add_log(f"Iniciando deploy serverless para {project_name}")

        # 1. Criar bucket para codigo
        try:
            bucket = await self.create_bucket(
                name=f"{project_name}-functions".lower().replace("_", "-"),
                tags={"project": project_name}
            )
            result.add_resource(bucket)
        except Exception as e:
            result.add_warning(f"Bucket nao criado: {e}")

        # 2. Criar funcoes
        functions_config = config.get("functions", [{"name": "api"}])
        for func_config in functions_config:
            func_name = f"{project_name}-{func_config.get('name', 'api')}"
            try:
                func = await self.create_function(
                    name=func_name,
                    runtime=func_config.get("runtime", "python311"),
                    handler=func_config.get("handler", "main"),
                    code_path="",
                    memory_mb=func_config.get("memory_mb", 256),
                    environment=func_config.get("environment"),
                    tags={"project": project_name}
                )
                result.add_resource(func)
                result.api_endpoint = func.endpoint
            except Exception as e:
                result.add_error(f"Falha ao criar funcao {func_name}: {e}")

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
        resources = await self.list_resources(tags={"project": project_name})

        self._log(f"Removendo {len(resources)} recursos do projeto {project_name}")

        for resource in resources:
            try:
                if resource.resource_type == ResourceType.VM:
                    await self.terminate_vm(resource.name)
                elif resource.resource_type == ResourceType.OBJECT_STORAGE:
                    await self.delete_bucket(resource.name, force)
                elif resource.resource_type == ResourceType.SERVERLESS:
                    await self.delete_function(resource.name)
                elif resource.resource_type == ResourceType.DATABASE:
                    await self.delete_database(resource.name)
                elif resource.resource_type == ResourceType.FIREWALL:
                    await self.delete_security_group(resource.name)
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
        estimate = CostEstimate(provider=ProviderType.GCP)

        if stack_type == StackType.SIMPLE:
            machine_type = config.get("machine_type", "e2-micro")
            vm_hourly = self.compute.get_machine_pricing(machine_type) if self.compute else 0.01
            estimate.breakdown["compute"] = vm_hourly
            estimate.hourly_cost += vm_hourly

            # Storage
            storage_monthly = 1.0
            estimate.breakdown["storage"] = storage_monthly / (24 * 30)
            estimate.hourly_cost += storage_monthly / (24 * 30)

            # Database
            if config.get("database"):
                tier = config["database"].get("tier", "db-f1-micro")
                db_hourly = self.sql.get_tier_pricing(tier) if self.sql else 0.01
                estimate.breakdown["cloud_sql"] = db_hourly
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
        estimate.notes.append("Estimativa baseada em precos da regiao us-central1")

        return estimate

    async def get_current_costs(self) -> Dict[str, float]:
        """Obtem custos atuais"""
        return {
            "note": "Custos em tempo real requerem Cloud Billing API",
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
        """Lista recursos GCP"""
        resources = []

        if not self._connected:
            return resources

        # Instancias
        if not resource_type or resource_type == ResourceType.VM:
            if self.compute:
                instances = await self.compute.list_instances(labels=tags)
                for inst in instances:
                    resources.append(CloudResource(
                        resource_id=inst.get("id", inst["name"]),
                        name=inst["name"],
                        resource_type=ResourceType.VM,
                        provider=ProviderType.GCP,
                        region=self.config.zone,
                        status=ResourceStatus.RUNNING if inst["status"] == "RUNNING" else ResourceStatus.STOPPED,
                        public_ip=inst.get("external_ip"),
                        tags=inst.get("labels", {}),
                    ))

        # Storage
        if not resource_type or resource_type == ResourceType.OBJECT_STORAGE:
            storage_resources = await self.list_buckets()
            # Filtrar por labels
            if tags:
                storage_resources = [
                    r for r in storage_resources
                    if all(r.tags.get(k) == v for k, v in tags.items())
                ]
            resources.extend(storage_resources)

        # Functions
        if not resource_type or resource_type == ResourceType.SERVERLESS:
            if self.functions:
                funcs = await self.functions.list_functions(labels=tags)
                for func in funcs:
                    resources.append(CloudResource(
                        resource_id=func["full_name"],
                        name=func["function_name"],
                        resource_type=ResourceType.SERVERLESS,
                        provider=ProviderType.GCP,
                        region=self.config.region,
                        status=ResourceStatus.RUNNING,
                        endpoint=func.get("url"),
                        tags=func.get("labels", {}),
                    ))

        # Cloud SQL
        if not resource_type or resource_type == ResourceType.DATABASE:
            if self.sql:
                dbs = await self.sql.list_instances()
                for db in dbs:
                    resources.append(CloudResource(
                        resource_id=db["instance_name"],
                        name=db["instance_name"],
                        resource_type=ResourceType.DATABASE,
                        provider=ProviderType.GCP,
                        region=db.get("region", self.config.region),
                        status=ResourceStatus.RUNNING if db["status"] == "RUNNABLE" else ResourceStatus.CREATING,
                        endpoint=db.get("public_ip"),
                    ))

        return resources

    async def get_resource(self, resource_id: str) -> Optional[CloudResource]:
        """Obtem um recurso especifico"""
        resources = await self.list_resources()
        for resource in resources:
            if resource.resource_id == resource_id or resource.name == resource_id:
                return resource
        return None
