# -*- coding: utf-8 -*-
"""
AWS Cloud Provider
==================
Implementacao do provider AWS para a Plataforma E.

Integra todos os servicos AWS:
- EC2 (Computacao)
- S3 (Storage)
- Lambda (Serverless)
- RDS (Banco de dados)
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
    AWSConfig,
    ProviderType,
)
from .ec2 import EC2Manager
from .s3 import S3Manager
from .lambda_manager import LambdaManager
from .rds import RDSManager

logger = logging.getLogger(__name__)

# Flag para indicar se boto3 esta disponivel
BOTO3_AVAILABLE = False
try:
    import boto3
    from botocore.exceptions import ClientError, NoCredentialsError
    BOTO3_AVAILABLE = True
except ImportError:
    logger.warning("boto3 nao instalado. Instale com: pip install boto3")


class AWSProvider(CloudProvider):
    """
    Provider AWS para a Plataforma E.

    Implementa a interface CloudProvider para AWS,
    integrando todos os servicos necessarios.
    """

    def __init__(self, config: AWSConfig):
        """
        Inicializa o provider AWS.

        Args:
            config: Configuracao AWS com credenciais
        """
        super().__init__(config)
        self.config: AWSConfig = config

        # Clientes boto3 (inicializados em connect)
        self._session = None
        self._ec2_client = None
        self._ec2_resource = None
        self._s3_client = None
        self._s3_resource = None
        self._lambda_client = None
        self._rds_client = None
        self._iam_client = None

        # Managers de servicos
        self.ec2: Optional[EC2Manager] = None
        self.s3: Optional[S3Manager] = None
        self.lambda_manager: Optional[LambdaManager] = None
        self.rds: Optional[RDSManager] = None

    # =========================================================================
    # Conexao e Autenticacao
    # =========================================================================

    async def connect(self) -> bool:
        """
        Conecta a AWS usando as credenciais configuradas.

        Returns:
            True se conectou com sucesso
        """
        if not BOTO3_AVAILABLE:
            self._set_error("boto3 nao esta instalado")
            return False

        try:
            # Criar sessao boto3
            session_params = {
                "region_name": self.config.region,
            }

            if self.config.access_key_id:
                session_params["aws_access_key_id"] = self.config.access_key_id
                session_params["aws_secret_access_key"] = self.config.secret_access_key

            if self.config.session_token:
                session_params["aws_session_token"] = self.config.session_token

            if self.config.profile_name:
                session_params["profile_name"] = self.config.profile_name

            self._session = boto3.Session(**session_params)

            # Criar clientes para cada servico
            self._ec2_client = self._session.client("ec2")
            self._ec2_resource = self._session.resource("ec2")
            self._s3_client = self._session.client("s3")
            self._s3_resource = self._session.resource("s3")
            self._lambda_client = self._session.client("lambda")
            self._rds_client = self._session.client("rds")
            self._iam_client = self._session.client("iam")

            # Testar conexao
            self._ec2_client.describe_regions()

            # Inicializar managers
            self.ec2 = EC2Manager(
                self._ec2_client,
                self._ec2_resource,
                self.config
            )
            self.s3 = S3Manager(
                self._s3_client,
                self._s3_resource,
                self.config
            )
            self.lambda_manager = LambdaManager(
                self._lambda_client,
                self._iam_client,
                self.config
            )
            self.rds = RDSManager(self._rds_client, self.config)

            self._connected = True
            self._log("Conectado a AWS com sucesso")
            return True

        except NoCredentialsError:
            self._set_error("Credenciais AWS nao configuradas")
            return False

        except ClientError as e:
            self._set_error(f"Erro de autenticacao AWS: {e}")
            return False

        except Exception as e:
            self._set_error(f"Erro ao conectar a AWS: {e}")
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta da AWS.

        Returns:
            True sempre (nao ha sessao persistente)
        """
        self._session = None
        self._ec2_client = None
        self._ec2_resource = None
        self._s3_client = None
        self._s3_resource = None
        self._lambda_client = None
        self._rds_client = None
        self._iam_client = None

        self.ec2 = None
        self.s3 = None
        self.lambda_manager = None
        self.rds = None

        self._connected = False
        self._log("Desconectado da AWS")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com a AWS.

        Returns:
            True se a conexao esta funcionando
        """
        if not self._connected:
            return await self.connect()

        try:
            self._ec2_client.describe_regions()
            return True
        except Exception as e:
            self._set_error(f"Falha no teste de conexao: {e}")
            return False

    # =========================================================================
    # Computacao (EC2)
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
        Cria uma instancia EC2.

        Args:
            name: Nome da instancia
            instance_type: Tipo (t3.micro, etc)
            image_id: AMI ID
            key_name: Nome do key pair
            security_groups: Security group IDs
            tags: Tags adicionais

        Returns:
            CloudResource representando a instancia
        """
        if not self.ec2:
            raise RuntimeError("Provider nao conectado")

        result = await self.ec2.create_instance(
            name=name,
            instance_type=instance_type,
            image_id=image_id,
            key_name=key_name,
            security_group_ids=security_groups,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["instance_id"],
            name=name,
            resource_type=ResourceType.VM,
            provider=ProviderType.AWS,
            region=self.config.region,
            status=ResourceStatus.RUNNING,
            public_ip=result.get("public_ip"),
            private_ip=result.get("private_ip"),
            public_dns=result.get("public_dns"),
            hourly_cost=result.get("hourly_cost", 0),
            monthly_cost=result.get("hourly_cost", 0) * 24 * 30,
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "instance_type": instance_type,
                "image_id": image_id,
            }
        )

    async def start_vm(self, resource_id: str) -> bool:
        """Inicia uma instancia EC2"""
        if not self.ec2:
            return False
        return await self.ec2.start_instance(resource_id)

    async def stop_vm(self, resource_id: str) -> bool:
        """Para uma instancia EC2"""
        if not self.ec2:
            return False
        return await self.ec2.stop_instance(resource_id)

    async def terminate_vm(self, resource_id: str) -> bool:
        """Termina uma instancia EC2"""
        if not self.ec2:
            return False
        return await self.ec2.terminate_instance(resource_id)

    async def get_vm_status(self, resource_id: str) -> ResourceStatus:
        """Obtem status de uma instancia EC2"""
        if not self.ec2:
            return ResourceStatus.UNKNOWN

        info = await self.ec2.get_instance(resource_id)
        status_map = {
            "pending": ResourceStatus.CREATING,
            "running": ResourceStatus.RUNNING,
            "stopping": ResourceStatus.STOPPING,
            "stopped": ResourceStatus.STOPPED,
            "shutting-down": ResourceStatus.DELETING,
            "terminated": ResourceStatus.DELETED,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Serverless (Lambda)
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
        Cria uma funcao Lambda.

        Args:
            name: Nome da funcao
            runtime: Runtime (python3.11, nodejs18.x)
            handler: Handler (arquivo.funcao)
            code_path: Caminho do codigo
            memory_mb: Memoria em MB
            timeout_seconds: Timeout
            environment: Variaveis de ambiente
            tags: Tags adicionais

        Returns:
            CloudResource representando a funcao
        """
        if not self.lambda_manager:
            raise RuntimeError("Provider nao conectado")

        result = await self.lambda_manager.create_function(
            name=name,
            runtime=runtime,
            handler=handler,
            code_path=code_path,
            memory_mb=memory_mb,
            timeout_seconds=timeout_seconds,
            environment=environment,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["function_arn"],
            name=name,
            resource_type=ResourceType.SERVERLESS,
            provider=ProviderType.AWS,
            region=self.config.region,
            status=ResourceStatus.RUNNING,
            endpoint=result.get("function_arn"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "runtime": runtime,
                "handler": handler,
                "memory_mb": memory_mb,
                "timeout": timeout_seconds,
            }
        )

    async def invoke_function(
        self,
        function_name: str,
        payload: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Invoca uma funcao Lambda"""
        if not self.lambda_manager:
            return {"error": "Provider nao conectado"}
        return await self.lambda_manager.invoke_function(function_name, payload)

    async def delete_function(self, function_name: str) -> bool:
        """Deleta uma funcao Lambda"""
        if not self.lambda_manager:
            return False
        return await self.lambda_manager.delete_function(function_name)

    # =========================================================================
    # Storage (S3)
    # =========================================================================

    async def create_bucket(
        self,
        name: str,
        public: bool = False,
        versioning: bool = False,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """
        Cria um bucket S3.

        Args:
            name: Nome do bucket
            public: Se publico
            versioning: Habilitar versionamento
            tags: Tags adicionais

        Returns:
            CloudResource representando o bucket
        """
        if not self.s3:
            raise RuntimeError("Provider nao conectado")

        result = await self.s3.create_bucket(
            name=name,
            public=public,
            versioning=versioning,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=name,
            name=name,
            resource_type=ResourceType.OBJECT_STORAGE,
            provider=ProviderType.AWS,
            region=self.config.region,
            status=ResourceStatus.RUNNING,
            endpoint=result.get("bucket_url"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "public": public,
                "versioning": versioning,
            }
        )

    async def upload_file(
        self,
        bucket_name: str,
        file_path: str,
        object_key: str,
        content_type: Optional[str] = None
    ) -> str:
        """Faz upload de arquivo para S3"""
        if not self.s3:
            raise RuntimeError("Provider nao conectado")

        result = await self.s3.upload_file(
            bucket_name=bucket_name,
            file_path=file_path,
            object_key=object_key,
            content_type=content_type
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return result.get("object_url", "")

    async def delete_bucket(self, bucket_name: str, force: bool = False) -> bool:
        """Deleta um bucket S3"""
        if not self.s3:
            return False
        return await self.s3.delete_bucket(bucket_name, force)

    async def list_buckets(self) -> List[CloudResource]:
        """Lista buckets S3"""
        if not self.s3:
            return []

        buckets = await self.s3.list_buckets()
        resources = []

        for bucket in buckets:
            resources.append(CloudResource(
                resource_id=bucket["name"],
                name=bucket["name"],
                resource_type=ResourceType.OBJECT_STORAGE,
                provider=ProviderType.AWS,
                region=self.config.region,
                status=ResourceStatus.RUNNING,
                created_at=bucket.get("creation_date"),
            ))

        return resources

    # =========================================================================
    # Banco de Dados (RDS)
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
        Cria instancia RDS.

        Args:
            name: Nome do banco
            engine: Engine (postgres, mysql)
            engine_version: Versao
            instance_class: Classe da instancia
            storage_gb: Storage em GB
            username: Usuario admin
            password: Senha
            publicly_accessible: Se publico
            tags: Tags adicionais

        Returns:
            CloudResource representando o banco
        """
        if not self.rds:
            raise RuntimeError("Provider nao conectado")

        result = await self.rds.create_database(
            name=name,
            engine=engine,
            engine_version=engine_version,
            instance_class=instance_class,
            storage_gb=storage_gb,
            username=username,
            password=password,
            publicly_accessible=publicly_accessible,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["db_instance_id"],
            name=name,
            resource_type=ResourceType.DATABASE,
            provider=ProviderType.AWS,
            region=self.config.region,
            status=ResourceStatus.CREATING,
            endpoint=result.get("endpoint"),
            created_at=datetime.now(),
            tags=tags or {},
            details={
                "engine": engine,
                "engine_version": engine_version,
                "instance_class": instance_class,
                "storage_gb": storage_gb,
                "username": username,
                "password": result.get("password"),
                "port": result.get("port"),
            }
        )

    async def delete_database(
        self,
        database_id: str,
        skip_final_snapshot: bool = False
    ) -> bool:
        """Deleta instancia RDS"""
        if not self.rds:
            return False
        return await self.rds.delete_database(database_id, skip_final_snapshot)

    async def get_database_status(self, database_id: str) -> ResourceStatus:
        """Obtem status do banco RDS"""
        if not self.rds:
            return ResourceStatus.UNKNOWN

        info = await self.rds.get_database(database_id)
        status_map = {
            "available": ResourceStatus.RUNNING,
            "creating": ResourceStatus.CREATING,
            "deleting": ResourceStatus.DELETING,
            "modifying": ResourceStatus.UPDATING,
            "starting": ResourceStatus.CREATING,
            "stopping": ResourceStatus.STOPPING,
            "stopped": ResourceStatus.STOPPED,
        }
        return status_map.get(info.get("status", ""), ResourceStatus.UNKNOWN)

    # =========================================================================
    # Rede (Security Groups)
    # =========================================================================

    async def create_security_group(
        self,
        name: str,
        description: str,
        vpc_id: Optional[str] = None,
        rules: Optional[List[Dict[str, Any]]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> CloudResource:
        """Cria Security Group"""
        if not self.ec2:
            raise RuntimeError("Provider nao conectado")

        result = await self.ec2.create_security_group(
            name=name,
            description=description,
            vpc_id=vpc_id,
            ingress_rules=rules,
            tags=tags
        )

        if not result.get("success"):
            raise RuntimeError(result.get("error", "Erro desconhecido"))

        return CloudResource(
            resource_id=result["group_id"],
            name=name,
            resource_type=ResourceType.FIREWALL,
            provider=ProviderType.AWS,
            region=self.config.region,
            status=ResourceStatus.RUNNING,
            created_at=datetime.now(),
            tags=tags or {},
        )

    async def delete_security_group(self, security_group_id: str) -> bool:
        """Deleta Security Group"""
        if not self.ec2:
            return False
        return await self.ec2.delete_security_group(security_group_id)

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
        Faz deploy de uma stack completa na AWS.

        Args:
            project_name: Nome do projeto
            stack_type: Tipo da stack
            config: Configuracoes

        Returns:
            DeploymentResult com recursos criados
        """
        result = DeploymentResult(
            success=False,
            provider=ProviderType.AWS,
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
            elif stack_type == StackType.MICROSERVICES:
                await self._deploy_microservices_stack(project_name, config, result)
            else:
                result.add_error(f"Tipo de stack nao suportado: {stack_type}")
                return result

            result.success = len(result.errors) == 0
            result.completed_at = datetime.now()
            result.duration_seconds = (
                result.completed_at - result.started_at
            ).total_seconds()

            # Calcular estimativa de custo
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
        """Deploy de stack simples (monolito)"""
        result.add_log(f"Iniciando deploy simples para {project_name}")

        # 1. Criar bucket S3 para arquivos estaticos
        bucket_name = f"{project_name}-assets".lower().replace("_", "-")
        result.add_log(f"Criando bucket S3: {bucket_name}")

        try:
            bucket = await self.create_bucket(
                name=bucket_name,
                public=True,
                tags={"Project": project_name}
            )
            result.add_resource(bucket)
        except Exception as e:
            result.add_warning(f"Bucket nao criado: {e}")

        # 2. Criar Security Group
        sg_name = f"{project_name}-sg"
        result.add_log(f"Criando Security Group: {sg_name}")

        try:
            sg = await self.create_security_group(
                name=sg_name,
                description=f"Security Group para {project_name}",
                tags={"Project": project_name}
            )
            result.add_resource(sg)
            sg_id = sg.resource_id
        except Exception as e:
            result.add_warning(f"Security Group nao criado: {e}")
            sg_id = None

        # 3. Criar instancia EC2
        instance_type = config.get("instance_type", "t3.micro")
        result.add_log(f"Criando instancia EC2: {instance_type}")

        try:
            vm = await self.create_vm(
                name=f"{project_name}-server",
                instance_type=instance_type,
                image_id=config.get("image_id", "ami-0c7217cdde317cfec"),
                security_groups=[sg_id] if sg_id else None,
                tags={"Project": project_name}
            )
            result.add_resource(vm)
            result.application_url = f"http://{vm.public_ip}"
        except Exception as e:
            result.add_error(f"Falha ao criar instancia: {e}")
            return

        # 4. Criar banco de dados (se configurado)
        if config.get("database"):
            db_config = config["database"]
            db_name = f"{project_name}-db".lower().replace("_", "-")
            result.add_log(f"Criando banco de dados: {db_name}")

            try:
                db = await self.create_database(
                    name=db_name,
                    engine=db_config.get("engine", "postgres"),
                    engine_version=db_config.get("version", "15.4"),
                    instance_class=db_config.get("instance_class", "db.t3.micro"),
                    storage_gb=db_config.get("storage_gb", 20),
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
        """Deploy de stack serverless"""
        result.add_log(f"Iniciando deploy serverless para {project_name}")

        # 1. Criar bucket S3
        bucket_name = f"{project_name}-serverless".lower().replace("_", "-")
        try:
            bucket = await self.create_bucket(
                name=bucket_name,
                public=True,
                tags={"Project": project_name}
            )
            result.add_resource(bucket)
        except Exception as e:
            result.add_warning(f"Bucket nao criado: {e}")

        # 2. Criar funcoes Lambda
        functions = config.get("functions", [])
        for func_config in functions:
            func_name = f"{project_name}-{func_config['name']}"
            result.add_log(f"Criando funcao Lambda: {func_name}")

            try:
                func = await self.create_function(
                    name=func_name,
                    runtime=func_config.get("runtime", "python3.11"),
                    handler=func_config.get("handler", "handler.main"),
                    code_path=func_config.get("code_path", "."),
                    memory_mb=func_config.get("memory_mb", 256),
                    timeout_seconds=func_config.get("timeout", 30),
                    environment=func_config.get("environment"),
                    tags={"Project": project_name}
                )
                result.add_resource(func)

                # Adicionar URL publica se configurado
                if func_config.get("public_url"):
                    url_result = await self.lambda_manager.add_function_url(func_name)
                    if url_result.get("success"):
                        result.api_endpoint = url_result.get("function_url")
            except Exception as e:
                result.add_error(f"Falha ao criar funcao {func_name}: {e}")

        result.add_log("Deploy serverless concluido")

    async def _deploy_microservices_stack(
        self,
        project_name: str,
        config: Dict[str, Any],
        result: DeploymentResult
    ):
        """Deploy de stack de microservicos"""
        result.add_log(f"Iniciando deploy de microservicos para {project_name}")
        result.add_warning("Deploy de microservicos requer ECS/EKS - implementacao simplificada")

        # Deploy simplificado usando EC2
        services = config.get("services", [])
        for service_config in services:
            service_name = f"{project_name}-{service_config['name']}"
            result.add_log(f"Criando servico: {service_name}")

            try:
                vm = await self.create_vm(
                    name=service_name,
                    instance_type=service_config.get("instance_type", "t3.micro"),
                    image_id=config.get("image_id", "ami-0c7217cdde317cfec"),
                    tags={"Project": project_name, "Service": service_config['name']}
                )
                result.add_resource(vm)
            except Exception as e:
                result.add_error(f"Falha ao criar servico {service_name}: {e}")

        result.add_log("Deploy de microservicos concluido")

    async def teardown_stack(
        self,
        project_name: str,
        force: bool = False
    ) -> bool:
        """
        Remove todos os recursos de uma stack.

        Args:
            project_name: Nome do projeto
            force: Forcar remocao

        Returns:
            True se removeu com sucesso
        """
        if not self._connected:
            return False

        success = True

        # Listar todos os recursos do projeto
        resources = await self.list_resources(
            tags={"Project": project_name}
        )

        self._log(f"Removendo {len(resources)} recursos do projeto {project_name}")

        for resource in resources:
            try:
                if resource.resource_type == ResourceType.VM:
                    await self.terminate_vm(resource.resource_id)
                elif resource.resource_type == ResourceType.OBJECT_STORAGE:
                    await self.delete_bucket(resource.resource_id, force)
                elif resource.resource_type == ResourceType.SERVERLESS:
                    await self.delete_function(resource.name)
                elif resource.resource_type == ResourceType.DATABASE:
                    await self.delete_database(resource.resource_id, skip_final_snapshot=force)
                elif resource.resource_type == ResourceType.FIREWALL:
                    await self.delete_security_group(resource.resource_id)

                self._log(f"Recurso {resource.name} removido")
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
        """
        Estima custo de uma stack antes do deploy.

        Args:
            stack_type: Tipo da stack
            config: Configuracoes

        Returns:
            CostEstimate com estimativas
        """
        estimate = CostEstimate(provider=ProviderType.AWS)

        if stack_type == StackType.SIMPLE:
            # EC2
            instance_type = config.get("instance_type", "t3.micro")
            ec2_hourly = self.ec2.get_instance_type_pricing(instance_type) if self.ec2 else 0.02
            estimate.breakdown["ec2"] = ec2_hourly
            estimate.hourly_cost += ec2_hourly

            # S3 (estimativa minima)
            s3_monthly = 1.0  # ~50GB estimado
            estimate.breakdown["s3"] = s3_monthly / (24 * 30)
            estimate.hourly_cost += s3_monthly / (24 * 30)

            # RDS (se configurado)
            if config.get("database"):
                db_config = config["database"]
                instance_class = db_config.get("instance_class", "db.t3.micro")
                rds_hourly = self.rds.get_instance_pricing(instance_class) if self.rds else 0.02
                estimate.breakdown["rds"] = rds_hourly
                estimate.hourly_cost += rds_hourly

        elif stack_type == StackType.SERVERLESS:
            # Lambda (estimativa baseada em uso)
            invocations = config.get("monthly_invocations", 100000)
            avg_duration = config.get("avg_duration_ms", 200)
            memory = config.get("memory_mb", 256)

            if self.lambda_manager:
                lambda_cost = self.lambda_manager.estimate_cost(
                    memory, avg_duration, invocations
                )
                estimate.breakdown["lambda"] = lambda_cost["total_monthly_cost"] / (24 * 30)
                estimate.hourly_cost += lambda_cost["total_monthly_cost"] / (24 * 30)

        estimate.calculate_totals()

        estimate.notes.append("Estimativa baseada em precos da regiao us-east-1")
        estimate.notes.append("Custos reais podem variar baseado no uso")

        return estimate

    async def get_current_costs(self) -> Dict[str, float]:
        """
        Obtem custos atuais (requer Cost Explorer).

        Returns:
            Dicionario com custos por servico
        """
        # Cost Explorer requer permissoes especiais
        # Retornando estimativa baseada em recursos
        return {
            "note": "Custos em tempo real requerem AWS Cost Explorer",
            "estimated_monthly": 0.0,
        }

    # =========================================================================
    # Listagem e Monitoramento
    # =========================================================================

    async def list_resources(
        self,
        resource_type: Optional[ResourceType] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> List[CloudResource]:
        """
        Lista recursos AWS.

        Args:
            resource_type: Filtrar por tipo
            tags: Filtrar por tags

        Returns:
            Lista de CloudResource
        """
        resources = []

        if not self._connected:
            return resources

        # Listar EC2
        if not resource_type or resource_type == ResourceType.VM:
            if self.ec2:
                instances = await self.ec2.list_instances(tags=tags)
                for inst in instances:
                    resources.append(CloudResource(
                        resource_id=inst["instance_id"],
                        name=inst["name"],
                        resource_type=ResourceType.VM,
                        provider=ProviderType.AWS,
                        region=self.config.region,
                        status=ResourceStatus.RUNNING if inst["status"] == "running" else ResourceStatus.STOPPED,
                        public_ip=inst.get("public_ip"),
                        private_ip=inst.get("private_ip"),
                        tags=inst.get("tags", {}),
                    ))

        # Listar S3
        if not resource_type or resource_type == ResourceType.OBJECT_STORAGE:
            s3_resources = await self.list_buckets()
            resources.extend(s3_resources)

        # Listar Lambda
        if not resource_type or resource_type == ResourceType.SERVERLESS:
            if self.lambda_manager:
                functions = await self.lambda_manager.list_functions()
                for func in functions:
                    resources.append(CloudResource(
                        resource_id=func["function_arn"],
                        name=func["function_name"],
                        resource_type=ResourceType.SERVERLESS,
                        provider=ProviderType.AWS,
                        region=self.config.region,
                        status=ResourceStatus.RUNNING,
                    ))

        # Listar RDS
        if not resource_type or resource_type == ResourceType.DATABASE:
            if self.rds:
                databases = await self.rds.list_databases()
                for db in databases:
                    resources.append(CloudResource(
                        resource_id=db["db_instance_id"],
                        name=db["db_instance_id"],
                        resource_type=ResourceType.DATABASE,
                        provider=ProviderType.AWS,
                        region=self.config.region,
                        status=ResourceStatus.RUNNING if db["status"] == "available" else ResourceStatus.CREATING,
                        endpoint=db.get("endpoint"),
                    ))

        return resources

    async def get_resource(self, resource_id: str) -> Optional[CloudResource]:
        """
        Obtem um recurso especifico.

        Args:
            resource_id: ID do recurso

        Returns:
            CloudResource ou None
        """
        resources = await self.list_resources()
        for resource in resources:
            if resource.resource_id == resource_id:
                return resource
        return None
