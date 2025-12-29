# -*- coding: utf-8 -*-
"""
Integration Gateway Routes
==========================
Endpoints para o Gateway de Integracoes com isolamento de tenant.

Endpoints:
- GET  /api/v1/integrations - Lista integracoes disponiveis
- GET  /api/v1/integrations/status - Status de todas as integracoes
- POST /api/v1/integrations/{type}/configure - Configura integracao
- POST /api/v1/integrations/{type}/connect - Conecta integracao
- POST /api/v1/integrations/{type}/disconnect - Desconecta integracao
- POST /api/v1/integrations/proxy - Proxy para APIs externas
- GET  /api/v1/integrations/health - Health check de integracoes
"""

import logging
from datetime import datetime
from typing import Optional, Dict, Any, List

from fastapi import APIRouter, HTTPException, Request, Depends, BackgroundTasks
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1/integrations", tags=["Integration Gateway"])


# =============================================================================
# MODELS
# =============================================================================

class IntegrationConfigRequest(BaseModel):
    """Request para configurar integracao"""
    credentials: Dict[str, Any] = Field(..., description="Credenciais de acesso")
    settings: Optional[Dict[str, Any]] = Field(None, description="Configuracoes adicionais")
    enabled: bool = Field(True, description="Ativar integracao")


class ProxyRequest(BaseModel):
    """Request para proxy de integracao"""
    integration: str = Field(..., description="Tipo de integracao (github, gitlab, jira, etc)")
    endpoint: str = Field(..., description="Endpoint da API")
    method: str = Field("GET", description="Metodo HTTP")
    data: Optional[Dict[str, Any]] = Field(None, description="Dados para POST/PUT")
    params: Optional[Dict[str, Any]] = Field(None, description="Query parameters")
    headers: Optional[Dict[str, str]] = Field(None, description="Headers adicionais")
    timeout: int = Field(30, description="Timeout em segundos")


class IntegrationStatus(BaseModel):
    """Status de uma integracao"""
    type: str
    name: str
    enabled: bool
    configured: bool
    connected: bool = False
    last_sync: Optional[str] = None
    rate_limit: Optional[Dict[str, Any]] = None


# =============================================================================
# HELPERS
# =============================================================================

def get_tenant_id(request: Request) -> str:
    """Extrai tenant_id do request"""
    tenant_id = request.headers.get("X-Tenant-ID")
    if tenant_id:
        return tenant_id

    # Default para desenvolvimento
    return "default"


def validate_integration_type(integration_type: str) -> str:
    """Valida tipo de integracao"""
    from factory.integrations.gateway import IntegrationType

    valid_types = [t.value for t in IntegrationType]
    if integration_type.lower() not in valid_types:
        raise HTTPException(
            status_code=400,
            detail=f"Tipo de integracao invalido: {integration_type}. Validos: {valid_types}"
        )
    return integration_type.lower()


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("")
async def list_integrations(request: Request):
    """
    Lista todas as integracoes disponiveis para o tenant.

    Retorna:
    - Tipo e nome da integracao
    - Se esta habilitada
    - Se esta configurada
    - Rate limit atual
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.gateway import get_integration_gateway

        gateway = get_integration_gateway(tenant_id)
        integrations = gateway.get_available_integrations()

        return {
            "tenant_id": tenant_id,
            "integrations": integrations,
            "count": len(integrations)
        }

    except Exception as e:
        logger.error(f"Erro ao listar integracoes: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/status")
async def get_all_status(request: Request):
    """
    Retorna status de todas as integracoes configuradas.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.gateway import get_integration_gateway

        gateway = get_integration_gateway(tenant_id)
        health = await gateway.health_check()

        return health

    except Exception as e:
        logger.error(f"Erro ao buscar status: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/health")
async def health_check(request: Request):
    """
    Health check de todas as integracoes.

    Testa conexao com cada sistema externo configurado.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.gateway import get_integration_gateway

        gateway = get_integration_gateway(tenant_id)
        health = await gateway.health_check()

        # Determinar status HTTP baseado na saude
        if health["gateway_status"] == "unhealthy":
            return health
        return health

    except Exception as e:
        logger.error(f"Erro no health check: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{integration_type}")
async def get_integration_status(
    request: Request,
    integration_type: str
):
    """
    Retorna status de uma integracao especifica.
    """
    tenant_id = get_tenant_id(request)
    integration_type = validate_integration_type(integration_type)

    try:
        from factory.integrations.gateway import get_integration_gateway, IntegrationType

        gateway = get_integration_gateway(tenant_id)
        int_type = IntegrationType(integration_type)

        health = await gateway.health_check(int_type)

        return {
            "tenant_id": tenant_id,
            "integration": integration_type,
            **health.get("integrations", {}).get(integration_type, {})
        }

    except Exception as e:
        logger.error(f"Erro ao buscar status de {integration_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{integration_type}/configure")
async def configure_integration(
    request: Request,
    integration_type: str,
    config: IntegrationConfigRequest
):
    """
    Configura credenciais e settings de uma integracao.

    As credenciais sao armazenadas de forma segura e
    associadas ao tenant.
    """
    tenant_id = get_tenant_id(request)
    integration_type = validate_integration_type(integration_type)

    try:
        from factory.integrations.gateway import get_integration_gateway, IntegrationType

        gateway = get_integration_gateway(tenant_id)
        int_type = IntegrationType(integration_type)

        success = gateway.configure_integration(
            integration_type=int_type,
            credentials=config.credentials,
            settings=config.settings,
            enabled=config.enabled
        )

        if success:
            return {
                "success": True,
                "message": f"Integracao {integration_type} configurada com sucesso",
                "integration": integration_type,
                "enabled": config.enabled
            }
        else:
            raise HTTPException(
                status_code=400,
                detail=f"Falha ao configurar integracao {integration_type}"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao configurar {integration_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{integration_type}/connect")
async def connect_integration(
    request: Request,
    integration_type: str
):
    """
    Conecta a uma integracao ja configurada.

    Valida credenciais e estabelece conexao.
    """
    tenant_id = get_tenant_id(request)
    integration_type = validate_integration_type(integration_type)

    try:
        from factory.integrations.gateway import get_integration_gateway, IntegrationType

        gateway = get_integration_gateway(tenant_id)
        int_type = IntegrationType(integration_type)

        connector = gateway.get_connector(int_type)
        if not connector:
            raise HTTPException(
                status_code=400,
                detail=f"Integracao {integration_type} nao configurada"
            )

        # Verificar se tem metodo connect
        if hasattr(connector, "connect"):
            success = await connector.connect()
            if success:
                return {
                    "success": True,
                    "message": f"Conectado a {integration_type}",
                    "status": connector.get_status() if hasattr(connector, "get_status") else {}
                }
            else:
                error = connector.last_error if hasattr(connector, "last_error") else "Falha desconhecida"
                raise HTTPException(
                    status_code=400,
                    detail=f"Falha ao conectar: {error}"
                )
        else:
            return {
                "success": True,
                "message": f"Conector {integration_type} nao requer conexao explicita"
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao conectar {integration_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{integration_type}/disconnect")
async def disconnect_integration(
    request: Request,
    integration_type: str
):
    """
    Desconecta de uma integracao.
    """
    tenant_id = get_tenant_id(request)
    integration_type = validate_integration_type(integration_type)

    try:
        from factory.integrations.gateway import get_integration_gateway, IntegrationType

        gateway = get_integration_gateway(tenant_id)
        int_type = IntegrationType(integration_type)

        connector = gateway.get_connector(int_type)
        if connector and hasattr(connector, "disconnect"):
            await connector.disconnect()

        return {
            "success": True,
            "message": f"Desconectado de {integration_type}"
        }

    except Exception as e:
        logger.error(f"Erro ao desconectar {integration_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/proxy")
async def proxy_request(
    request: Request,
    proxy_req: ProxyRequest
):
    """
    Proxy seguro para APIs externas.

    Roteia requisicao atraves do gateway com:
    - Autenticacao automatica
    - Rate limiting
    - Logging
    - Timeout
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.gateway import (
            get_integration_gateway,
            IntegrationType,
            ProxyRequest as GatewayProxyRequest
        )

        gateway = get_integration_gateway(tenant_id)

        # Validar tipo de integracao
        try:
            int_type = IntegrationType(proxy_req.integration.lower())
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Tipo de integracao invalido: {proxy_req.integration}"
            )

        # Criar request do gateway
        gw_request = GatewayProxyRequest(
            integration=int_type,
            endpoint=proxy_req.endpoint,
            method=proxy_req.method,
            data=proxy_req.data,
            headers=proxy_req.headers,
            params=proxy_req.params,
            timeout=proxy_req.timeout
        )

        # Executar proxy
        response = await gateway.proxy_request(gw_request)

        if response.success:
            return {
                "success": True,
                "status_code": response.status_code,
                "data": response.data,
                "duration_ms": response.duration_ms,
                "cached": response.cached
            }
        else:
            return {
                "success": False,
                "status_code": response.status_code,
                "error": response.error,
                "duration_ms": response.duration_ms
            }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no proxy: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{integration_type}/sync")
async def sync_integration(
    request: Request,
    integration_type: str,
    background_tasks: BackgroundTasks,
    project_id: str,
    direction: str = "from_external"
):
    """
    Sincroniza dados com sistema externo.

    Direcoes:
    - from_external: Importar do sistema externo
    - to_external: Exportar para o sistema externo
    """
    tenant_id = get_tenant_id(request)
    integration_type = validate_integration_type(integration_type)

    try:
        from factory.integrations.gateway import get_integration_gateway, IntegrationType

        gateway = get_integration_gateway(tenant_id)
        int_type = IntegrationType(integration_type)

        connector = gateway.get_connector(int_type)
        if not connector:
            raise HTTPException(
                status_code=400,
                detail=f"Integracao {integration_type} nao configurada"
            )

        # Verificar se tem metodos de sync
        if direction == "from_external" and hasattr(connector, "sync_from_external"):
            result = await connector.sync_from_external(project_id)
            return {
                "success": result.success,
                "items_synced": result.items_synced,
                "items_created": result.items_created,
                "items_updated": result.items_updated,
                "items_failed": result.items_failed,
                "errors": result.errors,
                "warnings": result.warnings,
                "details": result.details
            }

        elif direction == "to_external" and hasattr(connector, "sync_to_external"):
            # Buscar stories do projeto
            from factory.core.analytics_service import get_analytics_service
            service = get_analytics_service(tenant_id)
            stories = service.get_stories(project_id=project_id)

            result = await connector.sync_to_external(stories)
            return {
                "success": result.success,
                "items_synced": result.items_synced,
                "items_created": result.items_created,
                "items_updated": result.items_updated,
                "items_failed": result.items_failed,
                "errors": result.errors
            }

        else:
            raise HTTPException(
                status_code=400,
                detail=f"Direcao {direction} nao suportada para {integration_type}"
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro na sincronizacao {integration_type}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# BI INTEGRATION SHORTCUTS
# =============================================================================

@router.get("/bi/powerbi/config")
async def get_powerbi_config(request: Request, project_id: str):
    """
    Retorna configuracao para Power BI DirectQuery.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.powerbi_connector import get_powerbi_connector

        connector = get_powerbi_connector(tenant_id)
        config = connector.export_for_direct_query(project_id)

        return config

    except Exception as e:
        logger.error(f"Erro ao gerar config Power BI: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/bi/tableau/wdc/{project_id}")
async def get_tableau_wdc(request: Request, project_id: str):
    """
    Retorna HTML do Web Data Connector para Tableau.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.tableau_connector import get_tableau_connector
        from fastapi.responses import HTMLResponse

        connector = get_tableau_connector(tenant_id)
        html = connector.get_wdc_html(project_id)

        return HTMLResponse(content=html)

    except Exception as e:
        logger.error(f"Erro ao gerar WDC Tableau: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/bi/export/excel")
async def export_excel(
    request: Request,
    project_id: str,
    include_tasks: bool = True,
    include_charts: bool = True
):
    """
    Exporta projeto para Excel.
    """
    tenant_id = get_tenant_id(request)

    try:
        from factory.integrations.excel_exporter import get_excel_exporter
        from fastapi.responses import StreamingResponse
        import io

        exporter = get_excel_exporter(tenant_id)
        excel_bytes = exporter.export_project(
            project_id=project_id,
            include_tasks=include_tasks,
            include_charts=include_charts
        )

        filename = f"projeto_{project_id}_{datetime.utcnow().strftime('%Y%m%d')}.xlsx"

        return StreamingResponse(
            io.BytesIO(excel_bytes),
            media_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            headers={"Content-Disposition": f"attachment; filename={filename}"}
        )

    except Exception as e:
        logger.error(f"Erro ao exportar Excel: {e}")
        raise HTTPException(status_code=500, detail=str(e))
