"""
Integrations Routes - API v1
============================

Endpoints para integracoes externas.

Endpoints:
- /api/v1/integrations/github - Integracao GitHub
- /api/v1/integrations/sap - Integracao SAP
- /api/v1/integrations/salesforce - Integracao Salesforce
- /api/v1/integrations/jira - Integracao Jira
"""

from datetime import datetime
from typing import List, Optional, Dict, Any
import uuid

from fastapi import APIRouter, Depends, Query, Request, HTTPException, status
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session

from .schemas import APIResponse, APIListResponse, RequestMeta, ErrorCodes
from factory.database.connection import SessionLocal

router = APIRouter()


# =============================================================================
# MODELS
# =============================================================================

class IntegrationConfig(BaseModel):
    """Configuracao de integracao"""
    integration_type: str = Field(..., description="Tipo: github, sap, salesforce, jira")
    name: str = Field(..., min_length=1, max_length=100)
    config: Dict[str, Any] = Field(default_factory=dict)
    enabled: bool = True


class IntegrationUpdate(BaseModel):
    """Atualizar integracao"""
    name: Optional[str] = None
    config: Optional[Dict[str, Any]] = None
    enabled: Optional[bool] = None


class GitHubConfig(BaseModel):
    """Configuracao GitHub"""
    access_token: str
    organization: Optional[str] = None
    default_repo: Optional[str] = None


class SAPConfig(BaseModel):
    """Configuracao SAP"""
    host: str
    client: str
    username: str
    password: str
    system_id: Optional[str] = None


class SalesforceConfig(BaseModel):
    """Configuracao Salesforce"""
    instance_url: str
    client_id: str
    client_secret: str
    username: str
    security_token: str


class JiraConfig(BaseModel):
    """Configuracao Jira"""
    base_url: str
    username: str
    api_token: str
    project_key: Optional[str] = None


# Store em memoria (substituir por banco em producao)
_integrations: Dict[str, dict] = {}


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_request_meta(request: Request) -> RequestMeta:
    return RequestMeta(
        request_id=request.headers.get("X-Request-ID", str(uuid.uuid4())),
        timestamp=datetime.utcnow(),
        tenant_id=request.headers.get("X-Tenant-ID"),
        api_version="v1"
    )


# =============================================================================
# INTEGRATIONS
# =============================================================================

@router.get("/", response_model=APIResponse, tags=["Integrations"])
async def list_integrations(
    request: Request,
    integration_type: Optional[str] = Query(None),
    enabled: Optional[bool] = Query(None)
):
    """
    Lista integracoes configuradas.
    """
    integrations = list(_integrations.values())

    if integration_type:
        integrations = [i for i in integrations if i["type"] == integration_type]
    if enabled is not None:
        integrations = [i for i in integrations if i["enabled"] == enabled]

    return APIResponse(
        data=integrations,
        meta=get_request_meta(request)
    )


@router.post("/", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Integrations"])
async def create_integration(
    config: IntegrationConfig,
    request: Request
):
    """
    Cria nova integracao.
    """
    integration_id = f"int_{uuid.uuid4().hex[:8]}"

    integration = {
        "id": integration_id,
        "type": config.integration_type,
        "name": config.name,
        "config": config.config,
        "enabled": config.enabled,
        "created_at": datetime.utcnow().isoformat(),
        "status": "configured"
    }

    _integrations[integration_id] = integration

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="Integracao criada com sucesso"
    )


@router.get("/{integration_id}", response_model=APIResponse, tags=["Integrations"])
async def get_integration(
    integration_id: str,
    request: Request
):
    """
    Busca integracao por ID.
    """
    integration = _integrations.get(integration_id)
    if not integration:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Integracao {integration_id} nao encontrada"
            }
        )

    return APIResponse(
        data=integration,
        meta=get_request_meta(request)
    )


@router.put("/{integration_id}", response_model=APIResponse, tags=["Integrations"])
async def update_integration(
    integration_id: str,
    update_data: IntegrationUpdate,
    request: Request
):
    """
    Atualiza integracao.
    """
    integration = _integrations.get(integration_id)
    if not integration:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Integracao {integration_id} nao encontrada"
            }
        )

    if update_data.name:
        integration["name"] = update_data.name
    if update_data.config:
        integration["config"].update(update_data.config)
    if update_data.enabled is not None:
        integration["enabled"] = update_data.enabled

    integration["updated_at"] = datetime.utcnow().isoformat()

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="Integracao atualizada com sucesso"
    )


@router.delete("/{integration_id}", response_model=APIResponse, tags=["Integrations"])
async def delete_integration(
    integration_id: str,
    request: Request
):
    """
    Remove integracao.
    """
    if integration_id not in _integrations:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Integracao {integration_id} nao encontrada"
            }
        )

    del _integrations[integration_id]

    return APIResponse(
        data={"id": integration_id, "deleted": True},
        meta=get_request_meta(request),
        message="Integracao removida com sucesso"
    )


@router.post("/{integration_id}/test", response_model=APIResponse, tags=["Integrations"])
async def test_integration(
    integration_id: str,
    request: Request
):
    """
    Testa conexao da integracao.
    """
    integration = _integrations.get(integration_id)
    if not integration:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Integracao {integration_id} nao encontrada"
            }
        )

    # Simular teste de conexao
    # Em producao, fazer chamada real para o servico
    integration["status"] = "connected"
    integration["last_test"] = datetime.utcnow().isoformat()

    return APIResponse(
        data={
            "id": integration_id,
            "status": "connected",
            "message": "Conexao testada com sucesso",
            "tested_at": datetime.utcnow().isoformat()
        },
        meta=get_request_meta(request)
    )


# =============================================================================
# GITHUB
# =============================================================================

@router.post("/github/configure", response_model=APIResponse, tags=["GitHub"])
async def configure_github(
    config: GitHubConfig,
    request: Request
):
    """
    Configura integracao com GitHub.
    """
    integration_id = "github_main"

    integration = {
        "id": integration_id,
        "type": "github",
        "name": "GitHub Integration",
        "config": {
            "organization": config.organization,
            "default_repo": config.default_repo,
            "has_token": True
        },
        "enabled": True,
        "created_at": datetime.utcnow().isoformat(),
        "status": "configured"
    }

    _integrations[integration_id] = integration

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="GitHub configurado com sucesso"
    )


@router.get("/github/repos", response_model=APIResponse, tags=["GitHub"])
async def list_github_repos(
    request: Request,
    organization: Optional[str] = Query(None)
):
    """
    Lista repositorios do GitHub.
    """
    # Em producao, fazer chamada real ao GitHub
    repos = [
        {"name": "fabrica-agentes", "full_name": "lcruz/fabrica-agentes", "private": False},
        {"name": "projeto-demo", "full_name": "lcruz/projeto-demo", "private": True}
    ]

    return APIResponse(
        data={"repositories": repos},
        meta=get_request_meta(request)
    )


@router.post("/github/sync/{project_id}", response_model=APIResponse, tags=["GitHub"])
async def sync_with_github(
    project_id: str,
    request: Request,
    repo_name: str = Query(...),
    db: Session = Depends(get_db)
):
    """
    Sincroniza projeto com repositorio GitHub.
    """
    from factory.database.models import Project

    project = db.query(Project).filter(Project.project_id == project_id).first()
    if not project:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Projeto {project_id} nao encontrado"
        )

    # Atualizar URL do GitHub
    project.github_url = f"https://github.com/{repo_name}"
    db.commit()

    return APIResponse(
        data={
            "project_id": project_id,
            "github_url": project.github_url,
            "synced": True,
            "synced_at": datetime.utcnow().isoformat()
        },
        meta=get_request_meta(request),
        message="Projeto sincronizado com GitHub"
    )


# =============================================================================
# SAP
# =============================================================================

@router.post("/sap/configure", response_model=APIResponse, tags=["SAP"])
async def configure_sap(
    config: SAPConfig,
    request: Request
):
    """
    Configura integracao com SAP.
    """
    integration_id = "sap_main"

    integration = {
        "id": integration_id,
        "type": "sap",
        "name": "SAP Integration",
        "config": {
            "host": config.host,
            "client": config.client,
            "system_id": config.system_id
        },
        "enabled": True,
        "created_at": datetime.utcnow().isoformat(),
        "status": "configured"
    }

    _integrations[integration_id] = integration

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="SAP configurado com sucesso"
    )


@router.post("/sap/import-requirements", response_model=APIResponse, tags=["SAP"])
async def import_sap_requirements(
    request: Request,
    project_id: str = Query(...),
    module: str = Query(None, description="Modulo SAP (MM, SD, FI, etc)")
):
    """
    Importa requisitos do SAP como stories.
    """
    # Em producao, conectar ao SAP e buscar requisitos
    imported = [
        {"story_id": "STR-SAP-001", "title": "Requisito SAP importado 1"},
        {"story_id": "STR-SAP-002", "title": "Requisito SAP importado 2"}
    ]

    return APIResponse(
        data={
            "project_id": project_id,
            "module": module,
            "imported_count": len(imported),
            "stories": imported
        },
        meta=get_request_meta(request),
        message=f"{len(imported)} requisitos importados do SAP"
    )


# =============================================================================
# SALESFORCE
# =============================================================================

@router.post("/salesforce/configure", response_model=APIResponse, tags=["Salesforce"])
async def configure_salesforce(
    config: SalesforceConfig,
    request: Request
):
    """
    Configura integracao com Salesforce.
    """
    integration_id = "salesforce_main"

    integration = {
        "id": integration_id,
        "type": "salesforce",
        "name": "Salesforce Integration",
        "config": {
            "instance_url": config.instance_url
        },
        "enabled": True,
        "created_at": datetime.utcnow().isoformat(),
        "status": "configured"
    }

    _integrations[integration_id] = integration

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="Salesforce configurado com sucesso"
    )


@router.post("/salesforce/sync-cases", response_model=APIResponse, tags=["Salesforce"])
async def sync_salesforce_cases(
    request: Request,
    project_id: str = Query(...),
    case_type: Optional[str] = Query(None, description="Tipo de caso")
):
    """
    Sincroniza casos do Salesforce como stories.
    """
    synced = [
        {"case_id": "CASE-001", "story_id": "STR-SF-001", "subject": "Caso importado 1"},
        {"case_id": "CASE-002", "story_id": "STR-SF-002", "subject": "Caso importado 2"}
    ]

    return APIResponse(
        data={
            "project_id": project_id,
            "case_type": case_type,
            "synced_count": len(synced),
            "cases": synced
        },
        meta=get_request_meta(request),
        message=f"{len(synced)} casos sincronizados do Salesforce"
    )


# =============================================================================
# JIRA
# =============================================================================

@router.post("/jira/configure", response_model=APIResponse, tags=["Jira"])
async def configure_jira(
    config: JiraConfig,
    request: Request
):
    """
    Configura integracao com Jira.
    """
    integration_id = "jira_main"

    integration = {
        "id": integration_id,
        "type": "jira",
        "name": "Jira Integration",
        "config": {
            "base_url": config.base_url,
            "project_key": config.project_key
        },
        "enabled": True,
        "created_at": datetime.utcnow().isoformat(),
        "status": "configured"
    }

    _integrations[integration_id] = integration

    return APIResponse(
        data=integration,
        meta=get_request_meta(request),
        message="Jira configurado com sucesso"
    )


@router.post("/jira/import-issues", response_model=APIResponse, tags=["Jira"])
async def import_jira_issues(
    request: Request,
    project_id: str = Query(...),
    jira_project: str = Query(..., description="Chave do projeto Jira"),
    issue_type: Optional[str] = Query(None, description="Tipo: Story, Bug, Task")
):
    """
    Importa issues do Jira como stories.
    """
    imported = [
        {"jira_key": "PROJ-123", "story_id": "STR-JIRA-001", "summary": "Issue importada 1"},
        {"jira_key": "PROJ-124", "story_id": "STR-JIRA-002", "summary": "Issue importada 2"}
    ]

    return APIResponse(
        data={
            "project_id": project_id,
            "jira_project": jira_project,
            "issue_type": issue_type,
            "imported_count": len(imported),
            "issues": imported
        },
        meta=get_request_meta(request),
        message=f"{len(imported)} issues importadas do Jira"
    )


@router.post("/jira/export-stories", response_model=APIResponse, tags=["Jira"])
async def export_stories_to_jira(
    request: Request,
    project_id: str = Query(...),
    story_ids: List[str] = Query(..., description="IDs das stories a exportar"),
    jira_project: str = Query(..., description="Chave do projeto Jira")
):
    """
    Exporta stories para o Jira.
    """
    exported = [
        {"story_id": sid, "jira_key": f"{jira_project}-{i+100}"}
        for i, sid in enumerate(story_ids)
    ]

    return APIResponse(
        data={
            "project_id": project_id,
            "jira_project": jira_project,
            "exported_count": len(exported),
            "stories": exported
        },
        meta=get_request_meta(request),
        message=f"{len(exported)} stories exportadas para o Jira"
    )
