"""
Integrations Routes - API v1
============================

Endpoints para integracoes externas.

Endpoints:
- /api/v1/integrations/github - Integracao GitHub
- /api/v1/integrations/sap - Integracao SAP
- /api/v1/integrations/salesforce - Integracao Salesforce
- /api/v1/integrations/jira - Integracao Jira
- /api/v1/integrations/health - Health Check (Issue #360)
- /api/v1/integrations/metrics - Metricas (Issue #360)
- /api/v1/integrations/alerts - Alertas (Issue #360)
- /api/v1/integrations/circuit-breakers - Circuit Breakers (Issue #360)
"""

from datetime import datetime
from typing import List, Optional, Dict, Any
import uuid
import logging

from fastapi import APIRouter, Depends, Query, Request, HTTPException, status
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session

from .schemas import APIResponse, APIListResponse, RequestMeta, ErrorCodes
from factory.database.connection import SessionLocal

logger = logging.getLogger(__name__)

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


# =============================================================================
# HEALTH CHECK - Issue #360
# =============================================================================

# Instancias globais lazy para monitoring
_health_checker = None
_metrics_collector = None
_alert_manager = None


def _get_health_checker():
    """Obtem instancia do health checker (lazy load)"""
    global _health_checker
    if _health_checker is None:
        try:
            from factory.integrations.monitoring import HealthChecker
            _health_checker = HealthChecker()
        except ImportError:
            logger.warning("HealthChecker not available")
            return None
    return _health_checker


def _get_metrics_collector():
    """Obtem instancia do metrics collector (lazy load)"""
    global _metrics_collector
    if _metrics_collector is None:
        try:
            from factory.integrations.monitoring import MetricsCollector
            _metrics_collector = MetricsCollector()
        except ImportError:
            logger.warning("MetricsCollector not available")
            return None
    return _metrics_collector


def _get_alert_manager():
    """Obtem instancia do alert manager (lazy load)"""
    global _alert_manager
    if _alert_manager is None:
        try:
            from factory.integrations.monitoring import AlertManager
            _alert_manager = AlertManager()
        except ImportError:
            logger.warning("AlertManager not available")
            return None
    return _alert_manager


@router.get("/health", response_model=APIResponse, tags=["Health Check"])
async def get_all_integrations_health(request: Request):
    """
    Retorna status de saude de todas as integracoes.

    Verifica conexao com:
    - Jira
    - Azure DevOps
    - Salesforce
    - SAP S/4 HANA
    """
    checker = _get_health_checker()
    if not checker:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Health checker not available"
        )

    try:
        result = await checker.check_all()
        return APIResponse(
            data=result.to_dict(),
            meta=get_request_meta(request)
        )
    except Exception as e:
        logger.error(f"Health check error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.get("/health/{integration_name}", response_model=APIResponse, tags=["Health Check"])
async def get_integration_health(
    integration_name: str,
    request: Request
):
    """
    Retorna status de saude de uma integracao especifica.

    Args:
        integration_name: Nome da integracao (jira, azure_devops, salesforce, sap_s4)
    """
    checker = _get_health_checker()
    if not checker:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Health checker not available"
        )

    valid_integrations = ["jira", "azure_devops", "salesforce", "sap_s4"]
    if integration_name not in valid_integrations:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={
                "error_code": ErrorCodes.INVALID_INPUT,
                "message": f"Integracao invalida. Use: {', '.join(valid_integrations)}"
            }
        )

    try:
        health = await checker.check_integration(integration_name)
        return APIResponse(
            data=health.to_dict(),
            meta=get_request_meta(request)
        )
    except Exception as e:
        logger.error(f"Health check error for {integration_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/health/start-background", response_model=APIResponse, tags=["Health Check"])
async def start_background_health_checks(request: Request):
    """
    Inicia verificacoes periodicas de saude em background.
    """
    checker = _get_health_checker()
    if not checker:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Health checker not available"
        )

    try:
        await checker.start_background_checks()
        return APIResponse(
            data={"status": "started", "interval_seconds": checker.check_interval},
            meta=get_request_meta(request),
            message="Background health checks iniciados"
        )
    except Exception as e:
        logger.error(f"Error starting background checks: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/health/stop-background", response_model=APIResponse, tags=["Health Check"])
async def stop_background_health_checks(request: Request):
    """
    Para verificacoes periodicas de saude em background.
    """
    checker = _get_health_checker()
    if not checker:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Health checker not available"
        )

    try:
        await checker.stop_background_checks()
        return APIResponse(
            data={"status": "stopped"},
            meta=get_request_meta(request),
            message="Background health checks parados"
        )
    except Exception as e:
        logger.error(f"Error stopping background checks: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


# =============================================================================
# METRICS - Issue #360
# =============================================================================

@router.get("/metrics", response_model=APIResponse, tags=["Metrics"])
async def get_all_integrations_metrics(
    request: Request,
    period_seconds: Optional[int] = Query(3600, description="Periodo em segundos")
):
    """
    Retorna metricas agregadas de todas as integracoes.

    Metricas incluem:
    - Latencia (avg, min, max, p95, p99)
    - Taxa de erro
    - Requests por minuto
    - Uptime
    """
    collector = _get_metrics_collector()
    if not collector:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Metrics collector not available"
        )

    try:
        metrics = collector.get_aggregated_metrics(period_seconds)
        return APIResponse(
            data={
                name: m.to_dict() for name, m in metrics.items()
            },
            meta=get_request_meta(request)
        )
    except Exception as e:
        logger.error(f"Metrics error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.get("/metrics/{integration_name}", response_model=APIResponse, tags=["Metrics"])
async def get_integration_metrics(
    integration_name: str,
    request: Request,
    period_seconds: Optional[int] = Query(3600, description="Periodo em segundos")
):
    """
    Retorna metricas de uma integracao especifica.

    Args:
        integration_name: Nome da integracao
        period_seconds: Periodo de analise em segundos
    """
    collector = _get_metrics_collector()
    if not collector:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Metrics collector not available"
        )

    try:
        metrics = collector.get_metrics(integration_name, period_seconds)
        return APIResponse(
            data=metrics.to_dict(),
            meta=get_request_meta(request)
        )
    except Exception as e:
        logger.error(f"Metrics error for {integration_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


class RecordRequestModel(BaseModel):
    """Modelo para registrar request"""
    latency_ms: float = Field(..., description="Latencia em milissegundos")
    success: bool = Field(..., description="Se a requisicao foi bem sucedida")
    metadata: Optional[Dict[str, Any]] = Field(default=None)


@router.post("/metrics/{integration_name}/record", response_model=APIResponse, tags=["Metrics"])
async def record_integration_request(
    integration_name: str,
    data: RecordRequestModel,
    request: Request
):
    """
    Registra uma requisicao para metricas.

    Use este endpoint para registrar manualmente requisicoes
    e calcular metricas.
    """
    collector = _get_metrics_collector()
    if not collector:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Metrics collector not available"
        )

    try:
        collector.record_request(
            integration_name,
            data.latency_ms,
            data.success,
            data.metadata
        )
        return APIResponse(
            data={
                "integration": integration_name,
                "recorded": True,
                "latency_ms": data.latency_ms,
                "success": data.success
            },
            meta=get_request_meta(request),
            message="Request registrado"
        )
    except Exception as e:
        logger.error(f"Record error for {integration_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.delete("/metrics/{integration_name}", response_model=APIResponse, tags=["Metrics"])
async def reset_integration_metrics(
    integration_name: str,
    request: Request
):
    """
    Reseta metricas de uma integracao.
    """
    collector = _get_metrics_collector()
    if not collector:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Metrics collector not available"
        )

    try:
        collector.reset(integration_name)
        return APIResponse(
            data={"integration": integration_name, "reset": True},
            meta=get_request_meta(request),
            message=f"Metricas de {integration_name} resetadas"
        )
    except Exception as e:
        logger.error(f"Reset error for {integration_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


# =============================================================================
# ALERTS - Issue #360
# =============================================================================

@router.get("/alerts", response_model=APIResponse, tags=["Alerts"])
async def get_active_alerts(
    request: Request,
    integration_name: Optional[str] = Query(None, description="Filtrar por integracao")
):
    """
    Retorna alertas ativos (nao resolvidos).
    """
    manager = _get_alert_manager()
    if not manager:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Alert manager not available"
        )

    try:
        if integration_name:
            alerts = manager.get_alerts_by_integration(integration_name)
            alerts = [a for a in alerts if not a.is_resolved]
        else:
            alerts = manager.get_active_alerts()

        return APIResponse(
            data={
                "alerts": [a.to_dict() for a in alerts],
                "count": len(alerts)
            },
            meta=get_request_meta(request)
        )
    except Exception as e:
        logger.error(f"Alerts error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.get("/alerts/{alert_id}", response_model=APIResponse, tags=["Alerts"])
async def get_alert(
    alert_id: str,
    request: Request
):
    """
    Retorna detalhes de um alerta.
    """
    manager = _get_alert_manager()
    if not manager:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Alert manager not available"
        )

    alert = manager.get_alert(alert_id)
    if not alert:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Alerta {alert_id} nao encontrado"
            }
        )

    return APIResponse(
        data=alert.to_dict(),
        meta=get_request_meta(request)
    )


class AcknowledgeAlertModel(BaseModel):
    """Modelo para reconhecer alerta"""
    user: str = Field(..., min_length=1, description="Usuario que reconhece")


@router.post("/alerts/{alert_id}/acknowledge", response_model=APIResponse, tags=["Alerts"])
async def acknowledge_alert(
    alert_id: str,
    data: AcknowledgeAlertModel,
    request: Request
):
    """
    Reconhece um alerta.

    O alerta permanece ativo mas marcado como reconhecido
    pelo usuario especificado.
    """
    manager = _get_alert_manager()
    if not manager:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Alert manager not available"
        )

    success = manager.acknowledge_alert(alert_id, data.user)
    if not success:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Alerta {alert_id} nao encontrado"
            }
        )

    alert = manager.get_alert(alert_id)
    return APIResponse(
        data=alert.to_dict() if alert else {"alert_id": alert_id, "acknowledged": True},
        meta=get_request_meta(request),
        message=f"Alerta reconhecido por {data.user}"
    )


@router.post("/alerts/{alert_id}/resolve", response_model=APIResponse, tags=["Alerts"])
async def resolve_alert(
    alert_id: str,
    request: Request
):
    """
    Resolve um alerta.

    O alerta e marcado como resolvido e nao aparecera mais
    na lista de alertas ativos.
    """
    manager = _get_alert_manager()
    if not manager:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Alert manager not available"
        )

    success = manager.resolve_alert(alert_id)
    if not success:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Alerta {alert_id} nao encontrado"
            }
        )

    alert = manager.get_alert(alert_id)
    return APIResponse(
        data=alert.to_dict() if alert else {"alert_id": alert_id, "resolved": True},
        meta=get_request_meta(request),
        message="Alerta resolvido"
    )


@router.delete("/alerts/old", response_model=APIResponse, tags=["Alerts"])
async def clear_old_alerts(
    request: Request,
    days: int = Query(7, description="Remover alertas mais antigos que X dias")
):
    """
    Remove alertas antigos resolvidos.
    """
    manager = _get_alert_manager()
    if not manager:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Alert manager not available"
        )

    try:
        manager.clear_old_alerts(days)
        return APIResponse(
            data={"cleared": True, "days": days},
            meta=get_request_meta(request),
            message=f"Alertas resolvidos mais antigos que {days} dias removidos"
        )
    except Exception as e:
        logger.error(f"Clear alerts error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


# =============================================================================
# CIRCUIT BREAKERS - Issue #360
# =============================================================================

@router.get("/circuit-breakers", response_model=APIResponse, tags=["Circuit Breakers"])
async def get_all_circuit_breakers(request: Request):
    """
    Retorna status de todos os circuit breakers.

    Estados possiveis:
    - closed: Operacao normal
    - open: Bloqueando requisicoes
    - half_open: Testando recuperacao
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        status_data = circuit_registry.get_all_status()
        return APIResponse(
            data={
                "circuit_breakers": status_data,
                "count": len(status_data)
            },
            meta=get_request_meta(request)
        )
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Circuit breakers error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.get("/circuit-breakers/{name}", response_model=APIResponse, tags=["Circuit Breakers"])
async def get_circuit_breaker(
    name: str,
    request: Request
):
    """
    Retorna status de um circuit breaker especifico.
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        breaker = circuit_registry.get(name)
        if not breaker:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={
                    "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                    "message": f"Circuit breaker {name} nao encontrado"
                }
            )

        return APIResponse(
            data=breaker.get_status(),
            meta=get_request_meta(request)
        )
    except HTTPException:
        raise
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Circuit breaker error for {name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/circuit-breakers/{name}/reset", response_model=APIResponse, tags=["Circuit Breakers"])
async def reset_circuit_breaker(
    name: str,
    request: Request
):
    """
    Reseta um circuit breaker para estado fechado.
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        breaker = circuit_registry.get(name)
        if not breaker:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={
                    "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                    "message": f"Circuit breaker {name} nao encontrado"
                }
            )

        breaker.reset()
        return APIResponse(
            data=breaker.get_status(),
            meta=get_request_meta(request),
            message=f"Circuit breaker {name} resetado"
        )
    except HTTPException:
        raise
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Reset circuit breaker error for {name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/circuit-breakers/{name}/force-open", response_model=APIResponse, tags=["Circuit Breakers"])
async def force_open_circuit_breaker(
    name: str,
    request: Request
):
    """
    Forca abertura de um circuit breaker.

    Util para manutencao ou quando se sabe que o servico
    esta indisponivel.
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        breaker = circuit_registry.get(name)
        if not breaker:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={
                    "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                    "message": f"Circuit breaker {name} nao encontrado"
                }
            )

        breaker.force_open()
        return APIResponse(
            data=breaker.get_status(),
            meta=get_request_meta(request),
            message=f"Circuit breaker {name} aberto forcadamente"
        )
    except HTTPException:
        raise
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Force open circuit breaker error for {name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/circuit-breakers/{name}/force-close", response_model=APIResponse, tags=["Circuit Breakers"])
async def force_close_circuit_breaker(
    name: str,
    request: Request
):
    """
    Forca fechamento de um circuit breaker.

    Util quando se sabe que o servico foi restaurado e se
    deseja retomar operacoes imediatamente.
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        breaker = circuit_registry.get(name)
        if not breaker:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail={
                    "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                    "message": f"Circuit breaker {name} nao encontrado"
                }
            )

        breaker.force_close()
        return APIResponse(
            data=breaker.get_status(),
            meta=get_request_meta(request),
            message=f"Circuit breaker {name} fechado forcadamente"
        )
    except HTTPException:
        raise
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Force close circuit breaker error for {name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )


@router.post("/circuit-breakers/reset-all", response_model=APIResponse, tags=["Circuit Breakers"])
async def reset_all_circuit_breakers(request: Request):
    """
    Reseta todos os circuit breakers.
    """
    try:
        from factory.integrations.monitoring.circuit_breaker import circuit_registry

        circuit_registry.reset_all()
        return APIResponse(
            data={"reset": True},
            meta=get_request_meta(request),
            message="Todos os circuit breakers resetados"
        )
    except ImportError:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Circuit breaker not available"
        )
    except Exception as e:
        logger.error(f"Reset all circuit breakers error: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )
