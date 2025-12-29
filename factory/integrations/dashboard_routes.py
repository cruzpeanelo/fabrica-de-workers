# -*- coding: utf-8 -*-
"""
Integration Dashboard Routes
============================
Endpoints de integracoes para o Dashboard Agile v6.
Este modulo pode ser incluido no dashboard principal.

Uso:
    from factory.integrations.dashboard_routes import register_integration_routes
    register_integration_routes(app)
"""

import logging
from typing import Optional
from pydantic import BaseModel
from fastapi import FastAPI, HTTPException, Request

logger = logging.getLogger(__name__)


class JiraConnectRequest(BaseModel):
    url: str
    email: str
    api_token: str
    project_key: str
    auto_sync: bool = False


class AzureDevOpsConnectRequest(BaseModel):
    organization: str
    project: str
    pat: str
    team: Optional[str] = None
    area_path: Optional[str] = None


class IntegrationSyncRequest(BaseModel):
    project_id: str
    direction: str = "from_external"


def register_integration_routes(app: FastAPI, SessionLocal, StoryRepository, Story, notify):
    """
    Registra rotas de integracao no app FastAPI.

    Args:
        app: Instancia FastAPI
        SessionLocal: Factory de sessao do banco
        StoryRepository: Repositorio de stories
        Story: Model de Story
        notify: Funcao de notificacao
    """

    # Import integration modules
    try:
        from factory.integrations.jira import get_jira_integration
        from factory.integrations.azure_devops import get_azure_devops_integration
        HAS_INTEGRATIONS = True
    except ImportError:
        HAS_INTEGRATIONS = False
        logger.warning("Integration modules not available")
        return

    @app.get("/api/integrations/status")
    async def get_integrations_status():
        """Retorna status de todas as integracoes"""
        if not HAS_INTEGRATIONS:
            return {"jira": {"status": "unavailable"}, "azure_devops": {"status": "unavailable"}}

        jira = get_jira_integration()
        azure = get_azure_devops_integration()

        return {
            "jira": jira.get_status(),
            "azure_devops": azure.get_status()
        }

    @app.post("/api/integrations/jira/connect")
    async def jira_connect(request: JiraConnectRequest):
        """Conecta ao Jira"""
        jira = get_jira_integration()
        jira.config.url = request.url
        jira.config.email = request.email
        jira.config.api_token = request.api_token
        jira.config.project_key = request.project_key
        jira.config.auto_sync = request.auto_sync
        jira.config.enabled = True

        success = await jira.connect()

        if success:
            notify("integration_connected", {"system": "jira", "project_key": request.project_key})
            return {"success": True, "message": "Conectado ao Jira", "status": jira.get_status()}
        else:
            raise HTTPException(400, f"Falha ao conectar: {jira.last_error}")

    @app.post("/api/integrations/jira/disconnect")
    async def jira_disconnect():
        """Desconecta do Jira"""
        jira = get_jira_integration()
        await jira.disconnect()
        jira.config.enabled = False
        notify("integration_disconnected", {"system": "jira"})
        return {"success": True, "message": "Desconectado do Jira"}

    @app.get("/api/integrations/jira/status")
    async def jira_status():
        """Status da integracao Jira"""
        jira = get_jira_integration()
        return jira.get_status()

    @app.post("/api/integrations/jira/sync")
    async def jira_sync(request: IntegrationSyncRequest):
        """Sincroniza com Jira"""
        jira = get_jira_integration()
        if not jira.is_connected:
            raise HTTPException(400, "Jira nao conectado")

        result = await jira.sync_from_external(request.project_id)

        if result.success:
            db = SessionLocal()
            try:
                repo = StoryRepository(db)
                created_count = 0
                for story_data in result.details.get("stories", []):
                    existing = db.query(Story).filter(
                        Story.project_id == request.project_id,
                        Story.title == story_data.get("title")
                    ).first()

                    if not existing:
                        story_data["project_id"] = request.project_id
                        repo.create(story_data)
                        created_count += 1

                notify("sync_completed", {
                    "system": "jira",
                    "items_synced": result.items_synced,
                    "items_created": created_count
                })
            finally:
                db.close()

        return result.to_dict()

    @app.get("/api/integrations/jira/projects")
    async def jira_projects():
        """Lista projetos do Jira"""
        jira = get_jira_integration()
        if not jira.is_connected:
            raise HTTPException(400, "Jira nao conectado")

        projects = await jira.get_projects()
        return {"projects": [{"key": p.get("key"), "name": p.get("name")} for p in projects]}

    @app.post("/api/integrations/jira/webhook")
    async def jira_webhook(request: Request):
        """Webhook do Jira"""
        jira = get_jira_integration()
        payload = await request.json()
        await jira.handle_webhook(payload)
        return {"status": "processed"}

    @app.post("/api/integrations/azure-devops/connect")
    async def azure_devops_connect(request: AzureDevOpsConnectRequest):
        """Conecta ao Azure DevOps"""
        azure = get_azure_devops_integration()
        azure.config.organization = request.organization
        azure.config.project = request.project
        azure.config.pat = request.pat
        azure.config.team = request.team or ""
        azure.config.area_path = request.area_path or ""
        azure.config.enabled = True

        success = await azure.connect()

        if success:
            notify("integration_connected", {"system": "azure_devops", "project": request.project})
            return {"success": True, "message": "Conectado ao Azure DevOps", "status": azure.get_status()}
        else:
            raise HTTPException(400, f"Falha ao conectar: {azure.last_error}")

    @app.post("/api/integrations/azure-devops/disconnect")
    async def azure_devops_disconnect():
        """Desconecta do Azure DevOps"""
        azure = get_azure_devops_integration()
        await azure.disconnect()
        azure.config.enabled = False
        notify("integration_disconnected", {"system": "azure_devops"})
        return {"success": True, "message": "Desconectado do Azure DevOps"}

    @app.get("/api/integrations/azure-devops/status")
    async def azure_devops_status():
        """Status da integracao Azure DevOps"""
        azure = get_azure_devops_integration()
        return azure.get_status()

    @app.post("/api/integrations/azure-devops/sync")
    async def azure_devops_sync(request: IntegrationSyncRequest):
        """Sincroniza com Azure DevOps"""
        azure = get_azure_devops_integration()
        if not azure.is_connected:
            raise HTTPException(400, "Azure DevOps nao conectado")

        result = await azure.sync_from_external(request.project_id)

        if result.success:
            db = SessionLocal()
            try:
                repo = StoryRepository(db)
                created_count = 0
                for story_data in result.details.get("stories", []):
                    existing = db.query(Story).filter(
                        Story.project_id == request.project_id,
                        Story.title == story_data.get("title")
                    ).first()

                    if not existing:
                        story_data["project_id"] = request.project_id
                        repo.create(story_data)
                        created_count += 1

                notify("sync_completed", {
                    "system": "azure_devops",
                    "items_synced": result.items_synced,
                    "items_created": created_count
                })
            finally:
                db.close()

        return result.to_dict()

    @app.get("/api/integrations/azure-devops/iterations")
    async def azure_devops_iterations():
        """Lista iterations do Azure DevOps"""
        azure = get_azure_devops_integration()
        if not azure.is_connected:
            raise HTTPException(400, "Azure DevOps nao conectado")

        iterations = await azure.get_iterations()
        return {"iterations": iterations}

    @app.post("/api/integrations/azure-devops/webhook")
    async def azure_devops_webhook(request: Request):
        """Webhook do Azure DevOps"""
        azure = get_azure_devops_integration()
        payload = await request.json()
        await azure.handle_webhook(payload)
        return {"status": "processed"}

    logger.info("Integration routes registered successfully")
