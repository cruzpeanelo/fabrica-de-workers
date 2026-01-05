# -*- coding: utf-8 -*-
"""
App Cloud Deployer - Deploy 1-Click para Cloud
===============================================

Modulo para deploy automatizado de aplicacoes geradas pela Plataforma E
para diversos provedores cloud com um unico clique.

Provedores suportados:
- AWS Lambda (via Lambda Function URL)
- Vercel (via Vercel API)
- GCP Cloud Run (via gcloud)
- Azure Functions (via Azure Functions Core Tools)

Inspirado no Base44.app - Deploy 1-Click
"""

import asyncio
import json
import logging
import os
import shutil
import subprocess
import tempfile
import zipfile
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional
import uuid
import httpx

logger = logging.getLogger(__name__)


class DeployProvider(str, Enum):
    """Provedores de deploy suportados"""
    AWS_LAMBDA = "aws_lambda"
    VERCEL = "vercel"
    GCP_CLOUD_RUN = "gcp_cloud_run"
    AZURE_FUNCTIONS = "azure_functions"
    LOCAL = "local"  # Para testes


class DeployStatus(str, Enum):
    """Status do deployment"""
    PENDING = "pending"
    PACKAGING = "packaging"
    UPLOADING = "uploading"
    DEPLOYING = "deploying"
    CONFIGURING = "configuring"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class DeployConfig:
    """Configuracao de deploy"""
    provider: DeployProvider
    project_id: str
    project_name: str

    # Configuracoes opcionais
    region: str = "us-east-1"
    memory_mb: int = 256
    timeout_seconds: int = 30
    environment: Dict[str, str] = field(default_factory=dict)

    # Custom domain (opcional)
    custom_domain: Optional[str] = None

    # Provider-specific config
    aws_config: Optional[Dict[str, str]] = None
    vercel_config: Optional[Dict[str, str]] = None
    gcp_config: Optional[Dict[str, str]] = None
    azure_config: Optional[Dict[str, str]] = None


@dataclass
class DeploymentInfo:
    """Informacoes de um deployment"""
    deployment_id: str
    project_id: str
    provider: DeployProvider
    status: DeployStatus

    # URLs
    public_url: Optional[str] = None
    api_url: Optional[str] = None
    logs_url: Optional[str] = None

    # Timestamps
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    completed_at: Optional[datetime] = None

    # Progress
    progress: int = 0
    current_step: str = ""
    logs: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    # Resources created
    resources: Dict[str, Any] = field(default_factory=dict)

    # Cost estimate
    estimated_monthly_cost: float = 0.0

    def add_log(self, message: str):
        """Adiciona log"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.logs.append(f"[{timestamp}] {message}")
        self.updated_at = datetime.now()

    def add_error(self, message: str):
        """Adiciona erro"""
        self.errors.append(message)
        self.add_log(f"ERRO: {message}")

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "deployment_id": self.deployment_id,
            "project_id": self.project_id,
            "provider": self.provider.value,
            "status": self.status.value,
            "public_url": self.public_url,
            "api_url": self.api_url,
            "logs_url": self.logs_url,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "progress": self.progress,
            "current_step": self.current_step,
            "logs": self.logs[-20:],  # Ultimos 20 logs
            "errors": self.errors,
            "resources": self.resources,
            "estimated_monthly_cost": self.estimated_monthly_cost,
        }


class AppCloudDeployer:
    """
    Gerenciador de deploy para cloud.

    Responsavel por:
    - Empacotar aplicacoes para deploy
    - Deploy para diferentes provedores
    - Monitorar status de deployments
    - Gerenciar URLs publicas
    """

    def __init__(self, projects_dir: str = "projects"):
        """
        Inicializa o deployer.

        Args:
            projects_dir: Diretorio base dos projetos
        """
        self.projects_dir = Path(projects_dir)
        self.deployments: Dict[str, DeploymentInfo] = {}

        # Vercel API
        self.vercel_token = os.getenv("VERCEL_TOKEN", "")
        self.vercel_team_id = os.getenv("VERCEL_TEAM_ID", "")

        # AWS config (carrega de environment)
        self.aws_access_key = os.getenv("AWS_ACCESS_KEY_ID", "")
        self.aws_secret_key = os.getenv("AWS_SECRET_ACCESS_KEY", "")
        self.aws_region = os.getenv("AWS_DEFAULT_REGION", "us-east-1")

    def get_available_providers(self) -> List[Dict[str, Any]]:
        """
        Retorna provedores disponiveis com status de configuracao.
        """
        return [
            {
                "id": "aws_lambda",
                "name": "AWS Lambda",
                "icon": "aws",
                "configured": bool(self.aws_access_key and self.aws_secret_key),
                "description": "Serverless com auto-scaling",
                "free_tier": "1M requests/mes gratis",
                "setup_url": "https://console.aws.amazon.com/lambda"
            },
            {
                "id": "vercel",
                "name": "Vercel",
                "icon": "vercel",
                "configured": bool(self.vercel_token),
                "description": "Deploy instantaneo com Edge Network",
                "free_tier": "100GB bandwidth/mes",
                "setup_url": "https://vercel.com/account/tokens"
            },
            {
                "id": "gcp_cloud_run",
                "name": "GCP Cloud Run",
                "icon": "gcp",
                "configured": bool(os.getenv("GCP_PROJECT_ID")),
                "description": "Containers serverless",
                "free_tier": "2M requests/mes gratis",
                "setup_url": "https://console.cloud.google.com/run"
            },
            {
                "id": "azure_functions",
                "name": "Azure Functions",
                "icon": "azure",
                "configured": bool(os.getenv("AZURE_SUBSCRIPTION_ID")),
                "description": "Serverless com integracao Microsoft",
                "free_tier": "1M execucoes/mes gratis",
                "setup_url": "https://portal.azure.com"
            },
            {
                "id": "local",
                "name": "Local (Simulacao)",
                "icon": "local",
                "configured": True,
                "description": "Simula deploy para testes",
                "free_tier": "Gratuito",
                "setup_url": None
            }
        ]

    async def deploy(self, config: DeployConfig) -> DeploymentInfo:
        """
        Inicia deploy de uma aplicacao.

        Args:
            config: Configuracao do deploy

        Returns:
            DeploymentInfo com status do deploy
        """
        # Criar deployment info
        deployment_id = f"deploy-{uuid.uuid4().hex[:8]}"
        deployment = DeploymentInfo(
            deployment_id=deployment_id,
            project_id=config.project_id,
            provider=config.provider,
            status=DeployStatus.PENDING
        )

        self.deployments[deployment_id] = deployment

        try:
            # Executar deploy baseado no provider
            if config.provider == DeployProvider.AWS_LAMBDA:
                await self._deploy_to_aws_lambda(deployment, config)
            elif config.provider == DeployProvider.VERCEL:
                await self._deploy_to_vercel(deployment, config)
            elif config.provider == DeployProvider.GCP_CLOUD_RUN:
                await self._deploy_to_gcp_cloud_run(deployment, config)
            elif config.provider == DeployProvider.AZURE_FUNCTIONS:
                await self._deploy_to_azure_functions(deployment, config)
            elif config.provider == DeployProvider.LOCAL:
                await self._deploy_local_simulation(deployment, config)
            else:
                raise ValueError(f"Provider nao suportado: {config.provider}")

        except Exception as e:
            deployment.status = DeployStatus.FAILED
            deployment.add_error(str(e))
            logger.error(f"Deploy falhou: {e}")

        return deployment

    async def _deploy_to_aws_lambda(self, deployment: DeploymentInfo, config: DeployConfig):
        """Deploy para AWS Lambda com Function URL"""
        deployment.status = DeployStatus.PACKAGING
        deployment.current_step = "Empacotando aplicacao..."
        deployment.progress = 10
        deployment.add_log("Iniciando deploy para AWS Lambda")

        project_path = self.projects_dir / config.project_id

        # 1. Verificar se existe main.py ou app.py
        main_file = None
        for candidate in ["main.py", "app.py", "lambda_function.py"]:
            if (project_path / candidate).exists():
                main_file = candidate
                break

        if not main_file:
            # Criar wrapper para FastAPI
            deployment.add_log("Criando wrapper Lambda para FastAPI...")
            await self._create_lambda_wrapper(project_path)
            main_file = "lambda_handler.py"

        deployment.progress = 20

        # 2. Criar pacote zip
        deployment.current_step = "Criando pacote de deploy..."
        deployment.add_log("Criando arquivo ZIP...")

        zip_path = await self._create_deploy_package(project_path)
        deployment.progress = 40

        # 3. Deploy usando boto3 (se configurado)
        if self.aws_access_key and self.aws_secret_key:
            deployment.status = DeployStatus.DEPLOYING
            deployment.current_step = "Fazendo deploy para AWS Lambda..."
            deployment.add_log("Conectando ao AWS Lambda...")

            try:
                import boto3

                lambda_client = boto3.client(
                    'lambda',
                    region_name=config.region or self.aws_region,
                    aws_access_key_id=self.aws_access_key,
                    aws_secret_access_key=self.aws_secret_key
                )

                function_name = f"plataformae-{config.project_id}"

                # Ler zip
                with open(zip_path, 'rb') as f:
                    zip_content = f.read()

                deployment.progress = 60

                # Criar ou atualizar funcao
                try:
                    # Tentar atualizar
                    response = lambda_client.update_function_code(
                        FunctionName=function_name,
                        ZipFile=zip_content
                    )
                    deployment.add_log(f"Funcao {function_name} atualizada")
                except lambda_client.exceptions.ResourceNotFoundException:
                    # Criar nova
                    deployment.add_log(f"Criando funcao {function_name}...")

                    # Primeiro criar role (simplificado)
                    iam_client = boto3.client(
                        'iam',
                        aws_access_key_id=self.aws_access_key,
                        aws_secret_access_key=self.aws_secret_key
                    )

                    role_name = f"plataformae-{config.project_id}-role"

                    try:
                        role_response = iam_client.create_role(
                            RoleName=role_name,
                            AssumeRolePolicyDocument=json.dumps({
                                "Version": "2012-10-17",
                                "Statement": [{
                                    "Effect": "Allow",
                                    "Principal": {"Service": "lambda.amazonaws.com"},
                                    "Action": "sts:AssumeRole"
                                }]
                            })
                        )
                        role_arn = role_response["Role"]["Arn"]

                        iam_client.attach_role_policy(
                            RoleName=role_name,
                            PolicyArn="arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
                        )

                        # Aguardar propagacao
                        await asyncio.sleep(10)

                    except iam_client.exceptions.EntityAlreadyExistsException:
                        role_response = iam_client.get_role(RoleName=role_name)
                        role_arn = role_response["Role"]["Arn"]

                    # Criar funcao
                    response = lambda_client.create_function(
                        FunctionName=function_name,
                        Runtime="python3.11",
                        Role=role_arn,
                        Handler="lambda_handler.handler",
                        Code={"ZipFile": zip_content},
                        MemorySize=config.memory_mb,
                        Timeout=config.timeout_seconds,
                        Environment={"Variables": config.environment} if config.environment else {},
                        Tags={"Project": "PlataformaE", "ProjectId": config.project_id}
                    )

                deployment.progress = 80

                # 4. Criar Function URL
                deployment.current_step = "Configurando URL publica..."
                deployment.add_log("Criando URL publica...")

                try:
                    url_response = lambda_client.create_function_url_config(
                        FunctionName=function_name,
                        AuthType="NONE"
                    )
                    public_url = url_response["FunctionUrl"]
                except lambda_client.exceptions.ResourceConflictException:
                    url_response = lambda_client.get_function_url_config(
                        FunctionName=function_name
                    )
                    public_url = url_response["FunctionUrl"]

                # Adicionar permissao publica
                try:
                    lambda_client.add_permission(
                        FunctionName=function_name,
                        StatementId="FunctionURLAllowPublicAccess",
                        Action="lambda:InvokeFunctionUrl",
                        Principal="*",
                        FunctionUrlAuthType="NONE"
                    )
                except lambda_client.exceptions.ResourceConflictException:
                    pass  # Permissao ja existe

                deployment.public_url = public_url
                deployment.api_url = public_url
                deployment.status = DeployStatus.COMPLETED
                deployment.completed_at = datetime.now()
                deployment.progress = 100
                deployment.current_step = "Deploy concluido!"
                deployment.add_log(f"Deploy concluido! URL: {public_url}")

                # Estimar custo
                deployment.estimated_monthly_cost = 0.50  # Estimativa basica

                deployment.resources = {
                    "function_name": function_name,
                    "function_arn": response.get("FunctionArn", ""),
                    "runtime": "python3.11",
                    "memory_mb": config.memory_mb,
                    "region": config.region or self.aws_region
                }

            except ImportError:
                deployment.add_error("boto3 nao instalado. Execute: pip install boto3")
                deployment.status = DeployStatus.FAILED

            except Exception as e:
                deployment.add_error(f"Erro no deploy AWS: {str(e)}")
                deployment.status = DeployStatus.FAILED
        else:
            # Simular deploy se nao tiver credenciais
            deployment.add_log("AWS nao configurado - simulando deploy...")
            await self._simulate_deploy(deployment, config, "aws-lambda")

        # Limpar zip temporario
        if zip_path and os.path.exists(zip_path):
            os.unlink(zip_path)

    async def _deploy_to_vercel(self, deployment: DeploymentInfo, config: DeployConfig):
        """Deploy para Vercel"""
        deployment.status = DeployStatus.PACKAGING
        deployment.current_step = "Preparando para Vercel..."
        deployment.progress = 10
        deployment.add_log("Iniciando deploy para Vercel")

        project_path = self.projects_dir / config.project_id

        if not self.vercel_token:
            deployment.add_log("Vercel nao configurado - simulando deploy...")
            await self._simulate_deploy(deployment, config, "vercel")
            return

        try:
            # 1. Criar vercel.json se nao existir
            vercel_config = {
                "version": 2,
                "builds": [
                    {
                        "src": "main.py",
                        "use": "@vercel/python"
                    }
                ],
                "routes": [
                    {"src": "/(.*)", "dest": "main.py"}
                ]
            }

            vercel_json_path = project_path / "vercel.json"
            with open(vercel_json_path, "w") as f:
                json.dump(vercel_config, f, indent=2)

            deployment.progress = 30

            # 2. Criar deployment via API
            deployment.status = DeployStatus.DEPLOYING
            deployment.current_step = "Fazendo upload para Vercel..."
            deployment.add_log("Enviando arquivos para Vercel...")

            # Preparar arquivos
            files = []
            for file_path in project_path.rglob("*"):
                if file_path.is_file() and not file_path.name.startswith("."):
                    rel_path = file_path.relative_to(project_path)
                    with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                        try:
                            content = f.read()
                            files.append({
                                "file": str(rel_path).replace("\\", "/"),
                                "data": content
                            })
                        except:
                            pass

            deployment.progress = 50

            # Criar deployment
            async with httpx.AsyncClient() as client:
                headers = {
                    "Authorization": f"Bearer {self.vercel_token}",
                    "Content-Type": "application/json"
                }

                payload = {
                    "name": f"plataformae-{config.project_id}",
                    "files": files,
                    "projectSettings": {
                        "framework": None,
                        "buildCommand": None,
                        "outputDirectory": None
                    }
                }

                if self.vercel_team_id:
                    params = {"teamId": self.vercel_team_id}
                else:
                    params = {}

                response = await client.post(
                    "https://api.vercel.com/v13/deployments",
                    headers=headers,
                    json=payload,
                    params=params,
                    timeout=60.0
                )

                if response.status_code in [200, 201]:
                    data = response.json()

                    deployment.progress = 80
                    deployment.current_step = "Aguardando build..."
                    deployment.add_log(f"Deployment criado: {data.get('id')}")

                    # Aguardar deploy ficar pronto
                    deploy_url = data.get("url", "")
                    if deploy_url and not deploy_url.startswith("http"):
                        deploy_url = f"https://{deploy_url}"

                    deployment.public_url = deploy_url
                    deployment.api_url = deploy_url
                    deployment.status = DeployStatus.COMPLETED
                    deployment.completed_at = datetime.now()
                    deployment.progress = 100
                    deployment.current_step = "Deploy concluido!"
                    deployment.add_log(f"Deploy concluido! URL: {deploy_url}")

                    deployment.resources = {
                        "deployment_id": data.get("id"),
                        "url": deploy_url,
                        "ready_state": data.get("readyState")
                    }
                else:
                    error_msg = response.text
                    deployment.add_error(f"Erro Vercel API: {error_msg}")
                    deployment.status = DeployStatus.FAILED

        except Exception as e:
            deployment.add_error(f"Erro no deploy Vercel: {str(e)}")
            deployment.status = DeployStatus.FAILED

    async def _deploy_to_gcp_cloud_run(self, deployment: DeploymentInfo, config: DeployConfig):
        """Deploy para GCP Cloud Run"""
        deployment.add_log("Iniciando deploy para GCP Cloud Run")

        gcp_project = os.getenv("GCP_PROJECT_ID", "")

        if not gcp_project:
            deployment.add_log("GCP nao configurado - simulando deploy...")
            await self._simulate_deploy(deployment, config, "gcp-cloud-run")
            return

        # TODO: Implementar deploy real com gcloud CLI ou API
        await self._simulate_deploy(deployment, config, "gcp-cloud-run")

    async def _deploy_to_azure_functions(self, deployment: DeploymentInfo, config: DeployConfig):
        """Deploy para Azure Functions"""
        deployment.add_log("Iniciando deploy para Azure Functions")

        azure_subscription = os.getenv("AZURE_SUBSCRIPTION_ID", "")

        if not azure_subscription:
            deployment.add_log("Azure nao configurado - simulando deploy...")
            await self._simulate_deploy(deployment, config, "azure-functions")
            return

        # TODO: Implementar deploy real com Azure CLI ou API
        await self._simulate_deploy(deployment, config, "azure-functions")

    async def _deploy_local_simulation(self, deployment: DeploymentInfo, config: DeployConfig):
        """Simulacao local de deploy"""
        await self._simulate_deploy(deployment, config, "local")

    async def _simulate_deploy(self, deployment: DeploymentInfo, config: DeployConfig, provider: str):
        """Simula um deploy (para testes ou quando credenciais nao estao configuradas)"""
        deployment.status = DeployStatus.PACKAGING
        deployment.current_step = "Empacotando aplicacao..."
        deployment.progress = 10
        deployment.add_log(f"[SIMULACAO] Iniciando deploy para {provider}")

        await asyncio.sleep(0.5)
        deployment.progress = 30
        deployment.current_step = "Verificando dependencias..."
        deployment.add_log("[SIMULACAO] Verificando requirements.txt")

        await asyncio.sleep(0.5)
        deployment.status = DeployStatus.UPLOADING
        deployment.progress = 50
        deployment.current_step = "Enviando arquivos..."
        deployment.add_log("[SIMULACAO] Upload de arquivos")

        await asyncio.sleep(0.5)
        deployment.status = DeployStatus.DEPLOYING
        deployment.progress = 70
        deployment.current_step = "Configurando ambiente..."
        deployment.add_log("[SIMULACAO] Configurando runtime")

        await asyncio.sleep(0.5)
        deployment.progress = 90
        deployment.current_step = "Iniciando aplicacao..."
        deployment.add_log("[SIMULACAO] Aplicacao iniciada")

        await asyncio.sleep(0.3)

        # Gerar URL simulada
        simulated_url = f"https://{config.project_id}.{provider}.plataformae.app"

        deployment.public_url = simulated_url
        deployment.api_url = simulated_url
        deployment.status = DeployStatus.COMPLETED
        deployment.completed_at = datetime.now()
        deployment.progress = 100
        deployment.current_step = "Deploy concluido! (simulacao)"
        deployment.add_log(f"[SIMULACAO] URL: {simulated_url}")

        deployment.resources = {
            "simulated": True,
            "provider": provider,
            "url": simulated_url
        }

        deployment.estimated_monthly_cost = 0.0

    async def _create_lambda_wrapper(self, project_path: Path):
        """Cria wrapper Lambda para FastAPI"""
        wrapper_code = '''# Lambda handler for FastAPI
import json
from mangum import Mangum

# Import your FastAPI app
try:
    from main import app
except ImportError:
    try:
        from app import app
    except ImportError:
        from fastapi import FastAPI
        app = FastAPI()

        @app.get("/")
        def root():
            return {"message": "App not configured"}

# Create Lambda handler
handler = Mangum(app, lifespan="off")
'''

        wrapper_path = project_path / "lambda_handler.py"
        with open(wrapper_path, "w") as f:
            f.write(wrapper_code)

        # Adicionar mangum ao requirements
        req_path = project_path / "requirements.txt"
        if req_path.exists():
            with open(req_path, "r") as f:
                content = f.read()
            if "mangum" not in content.lower():
                with open(req_path, "a") as f:
                    f.write("\nmangum>=0.17.0\n")
        else:
            with open(req_path, "w") as f:
                f.write("fastapi>=0.100.0\nmangum>=0.17.0\nuvicorn>=0.20.0\n")

    async def _create_deploy_package(self, project_path: Path) -> str:
        """Cria pacote zip para deploy"""
        with tempfile.NamedTemporaryFile(suffix=".zip", delete=False) as tmp:
            zip_path = tmp.name

        with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
            for file_path in project_path.rglob("*"):
                if file_path.is_file():
                    # Ignorar arquivos desnecessarios
                    if any(x in str(file_path) for x in ["__pycache__", ".git", ".pyc", "venv", "node_modules"]):
                        continue

                    rel_path = file_path.relative_to(project_path)
                    zf.write(file_path, rel_path)

        return zip_path

    def get_deployment(self, deployment_id: str) -> Optional[DeploymentInfo]:
        """Obtem informacoes de um deployment"""
        return self.deployments.get(deployment_id)

    def get_project_deployments(self, project_id: str) -> List[DeploymentInfo]:
        """Lista deployments de um projeto"""
        return [
            d for d in self.deployments.values()
            if d.project_id == project_id
        ]

    def get_all_deployments(self) -> List[DeploymentInfo]:
        """Lista todos os deployments"""
        return list(self.deployments.values())

    async def cancel_deployment(self, deployment_id: str) -> bool:
        """Cancela um deployment em andamento"""
        deployment = self.deployments.get(deployment_id)
        if not deployment:
            return False

        if deployment.status not in [DeployStatus.COMPLETED, DeployStatus.FAILED, DeployStatus.CANCELLED]:
            deployment.status = DeployStatus.CANCELLED
            deployment.add_log("Deployment cancelado pelo usuario")
            return True

        return False

    async def delete_deployment(self, deployment_id: str) -> bool:
        """Remove recursos de um deployment"""
        deployment = self.deployments.get(deployment_id)
        if not deployment:
            return False

        # TODO: Implementar remocao de recursos no provider

        del self.deployments[deployment_id]
        return True


# Instancia global
app_cloud_deployer = AppCloudDeployer()
