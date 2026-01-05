# -*- coding: utf-8 -*-
"""
Domain API Routes - Custom Domains Management
==============================================

Endpoints REST para gerenciamento de dominios customizados.
Inspirado no Base44.app - Custom Domains.
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field, validator
from typing import Dict, List, Optional, Any
import re

from factory.core.domain_manager import domain_manager, DomainType


router = APIRouter(prefix="/api/domains", tags=["Custom Domains"])


# === Request/Response Models ===

class GenerateSubdomainRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    project_name: Optional[str] = Field(None, description="Nome do projeto para subdomain legivel")


class AddCustomDomainRequest(BaseModel):
    project_id: str = Field(..., description="ID do projeto")
    domain: str = Field(..., description="Dominio customizado (ex: app.meusite.com.br)")
    deployment_id: Optional[str] = Field(None, description="ID do deployment associado")

    @validator('domain')
    def validate_domain(cls, v):
        pattern = r'^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,}$'
        if not re.match(pattern, v):
            raise ValueError('Formato de dominio invalido')
        return v.lower()


class LinkDeploymentRequest(BaseModel):
    deployment_id: str = Field(..., description="ID do deployment")
    target_url: str = Field(..., description="URL do deployment")


class SubdomainSuggestionsRequest(BaseModel):
    project_name: str = Field(..., description="Nome do projeto")
    count: Optional[int] = Field(5, description="Numero de sugestoes")


# === Endpoints ===

@router.post("/subdomain", response_model=Dict)
async def generate_subdomain(request: GenerateSubdomainRequest):
    """
    Gera um subdominio automatico para o projeto.

    O subdominio sera no formato: {nome}.plataformae.app
    Certificado SSL e gerado automaticamente.
    """
    try:
        config = domain_manager.generate_subdomain(
            project_id=request.project_id,
            project_name=request.project_name
        )

        return {
            "success": True,
            "domain": config.domain,
            "subdomain": config.subdomain,
            "domain_id": config.domain_id,
            "status": config.status.value,
            "ssl": config.ssl_certificate.to_dict() if config.ssl_certificate else None,
            "message": f"Subdominio {config.domain} criado e ativo!"
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/custom", response_model=Dict)
async def add_custom_domain(request: AddCustomDomainRequest):
    """
    Adiciona um dominio customizado.

    Retorna instrucoes de configuracao DNS que devem ser
    aplicadas no provedor de DNS do usuario.
    """
    try:
        config = domain_manager.add_custom_domain(
            project_id=request.project_id,
            domain=request.domain,
            deployment_id=request.deployment_id
        )

        instructions = domain_manager.get_dns_instructions(config.domain_id)

        return {
            "success": True,
            "domain_id": config.domain_id,
            "domain": config.domain,
            "status": config.status.value,
            "verification_token": config.verification_token,
            "dns_instructions": instructions,
            "message": "Dominio adicionado. Configure os registros DNS e clique em verificar."
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{domain_id}", response_model=Dict)
async def get_domain(domain_id: str):
    """Retorna informacoes de um dominio"""
    config = domain_manager.get_domain(domain_id)

    if not config:
        raise HTTPException(status_code=404, detail="Dominio nao encontrado")

    return config.to_dict()


@router.get("/project/{project_id}/list", response_model=List[Dict])
async def list_project_domains(project_id: str):
    """Lista todos os dominios de um projeto"""
    domains = domain_manager.get_project_domains(project_id)
    return [d.to_dict() for d in domains]


@router.get("/", response_model=List[Dict])
async def list_all_domains():
    """Lista todos os dominios"""
    domains = domain_manager.get_all_domains()
    return [d.to_dict() for d in domains]


@router.post("/{domain_id}/verify", response_model=Dict)
async def verify_dns(domain_id: str):
    """
    Verifica se os registros DNS estao configurados corretamente.

    Apos verificacao bem-sucedida, o certificado SSL e emitido automaticamente.
    """
    result = await domain_manager.verify_dns(domain_id)

    if not result.get("success") and "error" in result:
        raise HTTPException(status_code=404, detail=result["error"])

    return result


@router.get("/{domain_id}/instructions", response_model=Dict)
async def get_dns_instructions(domain_id: str):
    """Retorna instrucoes detalhadas de configuracao DNS"""
    instructions = domain_manager.get_dns_instructions(domain_id)

    if "error" in instructions:
        raise HTTPException(status_code=404, detail=instructions["error"])

    return instructions


@router.post("/{domain_id}/link", response_model=Dict)
async def link_deployment(domain_id: str, request: LinkDeploymentRequest):
    """Associa um deployment a um dominio"""
    success = domain_manager.link_deployment(
        domain_id=domain_id,
        deployment_id=request.deployment_id,
        target_url=request.target_url
    )

    if not success:
        raise HTTPException(status_code=404, detail="Dominio nao encontrado")

    return {
        "success": True,
        "message": "Deployment associado ao dominio",
        "domain_id": domain_id,
        "deployment_id": request.deployment_id
    }


@router.delete("/{domain_id}")
async def delete_domain(domain_id: str):
    """Remove um dominio"""
    success = await domain_manager.delete_domain(domain_id)

    if not success:
        raise HTTPException(status_code=404, detail="Dominio nao encontrado")

    return {"success": True, "message": "Dominio removido"}


@router.post("/suggestions", response_model=Dict)
async def get_subdomain_suggestions(request: SubdomainSuggestionsRequest):
    """Sugere subdomains disponiveis baseado no nome do projeto"""
    suggestions = domain_manager.get_available_subdomains(
        project_name=request.project_name,
        count=request.count or 5
    )

    return {
        "project_name": request.project_name,
        "suggestions": suggestions,
        "base_domain": domain_manager.BASE_DOMAIN
    }


@router.get("/status/overview", response_model=Dict)
async def get_domains_overview():
    """Retorna visao geral dos dominios"""
    domains = domain_manager.get_all_domains()

    active = sum(1 for d in domains if d.status.value == "active")
    pending = sum(1 for d in domains if "pending" in d.status.value)
    subdomains = sum(1 for d in domains if d.domain_type == DomainType.SUBDOMAIN)
    custom = sum(1 for d in domains if d.domain_type == DomainType.CUSTOM)

    # Dominios com SSL expirando em 30 dias
    expiring_soon = sum(
        1 for d in domains
        if d.ssl_certificate and d.ssl_certificate.days_until_expiry() < 30
    )

    return {
        "total": len(domains),
        "active": active,
        "pending": pending,
        "subdomains": subdomains,
        "custom_domains": custom,
        "ssl_expiring_soon": expiring_soon,
        "base_domain": domain_manager.BASE_DOMAIN
    }
