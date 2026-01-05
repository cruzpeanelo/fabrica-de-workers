# -*- coding: utf-8 -*-
"""
Deploy API Routes - Deploy 1-Click Cloud
=========================================

Endpoints REST para deploy de aplicacoes para cloud.
Inspirado no Base44.app - Deploy 1-Click.
"""

from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel, Field
from typing import Dict, List, Optional, Any

from factory.core.app_cloud_deployer import (
    app_cloud_deployer,
    DeployConfig,
    DeployProvider,
    DeployStatus
)


router = APIRouter(prefix="/api/deploy", tags=["Deploy Cloud"])


# === Request/Response Models ===

class DeployRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    provider: str = Field(..., description="Provider de deploy (aws_lambda, vercel, gcp_cloud_run, azure_functions, local)")
    project_name: Optional[str] = Field(None, description="Nome do projeto")
    region: Optional[str] = Field("us-east-1", description="Regiao do deploy")
    memory_mb: Optional[int] = Field(256, description="Memoria em MB")
    timeout_seconds: Optional[int] = Field(30, description="Timeout em segundos")
    environment: Optional[Dict[str, str]] = Field(None, description="Variaveis de ambiente")
    custom_domain: Optional[str] = Field(None, description="Dominio customizado")


class DeployResponse(BaseModel):
    deployment_id: str
    status: str
    message: str


# === Endpoints ===

@router.get("/providers", response_model=List[Dict])
async def list_providers():
    """
    Lista provedores de deploy disponiveis.

    Retorna informacoes sobre cada provider incluindo:
    - Se esta configurado
    - Descricao
    - Free tier disponivel
    """
    return app_cloud_deployer.get_available_providers()


@router.post("/start", response_model=DeployResponse)
async def start_deploy(request: DeployRequest, background_tasks: BackgroundTasks):
    """
    Inicia um novo deploy para cloud.

    O deploy e executado em background e o status pode ser
    consultado via GET /api/deploy/{deployment_id}.
    """
    try:
        # Validar provider
        try:
            provider = DeployProvider(request.provider)
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Provider invalido: {request.provider}. Use: aws_lambda, vercel, gcp_cloud_run, azure_functions, local"
            )

        # Criar configuracao
        config = DeployConfig(
            provider=provider,
            project_id=request.project_id,
            project_name=request.project_name or request.project_id,
            region=request.region or "us-east-1",
            memory_mb=request.memory_mb or 256,
            timeout_seconds=request.timeout_seconds or 30,
            environment=request.environment or {},
            custom_domain=request.custom_domain
        )

        # Iniciar deploy em background
        async def run_deploy():
            await app_cloud_deployer.deploy(config)

        # Criar deployment inicial
        import uuid
        deployment_id = f"deploy-{uuid.uuid4().hex[:8]}"

        # Executar deploy (sincrono para agora, pode ser background depois)
        deployment = await app_cloud_deployer.deploy(config)

        return DeployResponse(
            deployment_id=deployment.deployment_id,
            status=deployment.status.value,
            message=f"Deploy iniciado para {provider.value}"
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{deployment_id}", response_model=Dict)
async def get_deployment_status(deployment_id: str):
    """
    Retorna status de um deployment.

    Inclui:
    - Status atual
    - Progresso (0-100%)
    - Logs
    - URL publica (quando disponivel)
    """
    deployment = app_cloud_deployer.get_deployment(deployment_id)

    if not deployment:
        raise HTTPException(status_code=404, detail="Deployment nao encontrado")

    return deployment.to_dict()


@router.get("/project/{project_id}/list", response_model=List[Dict])
async def list_project_deployments(project_id: str):
    """Lista todos os deployments de um projeto"""
    deployments = app_cloud_deployer.get_project_deployments(project_id)
    return [d.to_dict() for d in deployments]


@router.get("/", response_model=List[Dict])
async def list_all_deployments():
    """Lista todos os deployments"""
    deployments = app_cloud_deployer.get_all_deployments()
    return [d.to_dict() for d in deployments]


@router.post("/{deployment_id}/cancel")
async def cancel_deployment(deployment_id: str):
    """Cancela um deployment em andamento"""
    success = await app_cloud_deployer.cancel_deployment(deployment_id)

    if not success:
        raise HTTPException(
            status_code=400,
            detail="Nao foi possivel cancelar o deployment"
        )

    return {"message": "Deployment cancelado", "deployment_id": deployment_id}


@router.delete("/{deployment_id}")
async def delete_deployment(deployment_id: str):
    """Remove um deployment e seus recursos"""
    success = await app_cloud_deployer.delete_deployment(deployment_id)

    if not success:
        raise HTTPException(status_code=404, detail="Deployment nao encontrado")

    return {"message": "Deployment removido", "deployment_id": deployment_id}


@router.post("/quick/{project_id}/{provider}", response_model=Dict)
async def quick_deploy(project_id: str, provider: str):
    """
    Deploy rapido com configuracoes padrao.

    Endpoint simplificado para deploy 1-click.
    """
    try:
        provider_enum = DeployProvider(provider)
    except ValueError:
        raise HTTPException(
            status_code=400,
            detail=f"Provider invalido: {provider}"
        )

    config = DeployConfig(
        provider=provider_enum,
        project_id=project_id,
        project_name=project_id
    )

    deployment = await app_cloud_deployer.deploy(config)

    return deployment.to_dict()


@router.get("/estimate/{project_id}", response_model=Dict)
async def estimate_cost(project_id: str, provider: str = "aws_lambda"):
    """
    Estima custo mensal de deploy para um projeto.

    Baseado em:
    - Tamanho do projeto
    - Provider selecionado
    - Uso estimado (1000 requests/dia)
    """
    try:
        provider_enum = DeployProvider(provider)
    except ValueError:
        raise HTTPException(status_code=400, detail=f"Provider invalido: {provider}")

    # Estimativas basicas por provider
    estimates = {
        DeployProvider.AWS_LAMBDA: {
            "provider": "AWS Lambda",
            "monthly_cost_usd": 0.50,
            "free_tier": "1M requests/mes gratis",
            "notes": "Custo estimado para ~1000 requests/dia"
        },
        DeployProvider.VERCEL: {
            "provider": "Vercel",
            "monthly_cost_usd": 0.0,
            "free_tier": "100GB bandwidth, unlimited deployments",
            "notes": "Hobby tier gratuito para projetos pessoais"
        },
        DeployProvider.GCP_CLOUD_RUN: {
            "provider": "GCP Cloud Run",
            "monthly_cost_usd": 0.40,
            "free_tier": "2M requests/mes gratis",
            "notes": "Custo estimado para ~1000 requests/dia"
        },
        DeployProvider.AZURE_FUNCTIONS: {
            "provider": "Azure Functions",
            "monthly_cost_usd": 0.45,
            "free_tier": "1M execucoes/mes gratis",
            "notes": "Custo estimado para ~1000 requests/dia"
        },
        DeployProvider.LOCAL: {
            "provider": "Local",
            "monthly_cost_usd": 0.0,
            "free_tier": "Gratuito (simulacao)",
            "notes": "Apenas para testes"
        }
    }

    return {
        "project_id": project_id,
        **estimates.get(provider_enum, estimates[DeployProvider.LOCAL])
    }
