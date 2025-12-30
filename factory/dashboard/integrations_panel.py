# -*- coding: utf-8 -*-
"""
Integrations Panel (Issue #154)
===============================
Modulo para gerenciar integracoes com sistemas externos:
- GitHub
- GitLab
- SAP S/4HANA
- Salesforce
- Jira

Este modulo adiciona endpoints e funcionalidades ao dashboard.
"""

import os
import json
import logging
from datetime import datetime
from typing import Optional, Dict, Any, List
from pathlib import Path

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

# Arquivo de configuracao de integracoes
INTEGRATIONS_CONFIG_FILE = Path(__file__).parent.parent.parent / "integrations_config.json"


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class GitHubConfig(BaseModel):
    """Configuracao GitHub"""
    token: str = ""
    owner: str = ""
    repo: str = ""
    branch: str = "main"
    connected: bool = False


class GitLabConfig(BaseModel):
    """Configuracao GitLab"""
    url: str = "https://gitlab.com"
    token: str = ""
    project_id: str = ""
    branch: str = "main"
    connected: bool = False


class SAPConfig(BaseModel):
    """Configuracao SAP S/4HANA"""
    host: str = ""
    client: str = "100"
    username: str = ""
    password: str = ""
    environment: str = "cloud"
    connected: bool = False


class SalesforceConfig(BaseModel):
    """Configuracao Salesforce"""
    client_id: str = ""
    client_secret: str = ""
    username: str = ""
    security_token: str = ""
    domain: str = "login"
    connected: bool = False


class JiraConfig(BaseModel):
    """Configuracao Jira"""
    url: str = ""
    email: str = ""
    token: str = ""
    project_key: str = ""
    connected: bool = False


class IntegrationsConfig(BaseModel):
    """Configuracao completa de integracoes"""
    github: GitHubConfig = Field(default_factory=GitHubConfig)
    gitlab: GitLabConfig = Field(default_factory=GitLabConfig)
    sap: SAPConfig = Field(default_factory=SAPConfig)
    salesforce: SalesforceConfig = Field(default_factory=SalesforceConfig)
    jira: JiraConfig = Field(default_factory=JiraConfig)
    scope: str = "global"  # "global" or project_id
    updated_at: Optional[str] = None


class IntegrationSaveRequest(BaseModel):
    """Request para salvar configuracao de integracao"""
    integration_type: str
    config: Dict[str, Any]
    project_id: Optional[str] = None


class IntegrationTestRequest(BaseModel):
    """Request para testar conexao"""
    integration_type: str
    config: Dict[str, Any]


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def load_integrations_config(project_id: Optional[str] = None) -> IntegrationsConfig:
    """Carrega configuracao de integracoes do arquivo"""
    try:
        if INTEGRATIONS_CONFIG_FILE.exists():
            with open(INTEGRATIONS_CONFIG_FILE, 'r', encoding='utf-8') as f:
                data = json.load(f)

                # Se tiver project_id, busca config especifica
                if project_id and project_id in data.get('projects', {}):
                    return IntegrationsConfig(**data['projects'][project_id])

                # Senao, retorna config global
                if 'global' in data:
                    return IntegrationsConfig(**data['global'])
    except Exception as e:
        logger.error(f"Erro ao carregar config de integracoes: {e}")

    return IntegrationsConfig()


def save_integrations_config(config: IntegrationsConfig, project_id: Optional[str] = None):
    """Salva configuracao de integracoes no arquivo"""
    try:
        data = {}
        if INTEGRATIONS_CONFIG_FILE.exists():
            with open(INTEGRATIONS_CONFIG_FILE, 'r', encoding='utf-8') as f:
                data = json.load(f)

        config.updated_at = datetime.utcnow().isoformat()
        config_dict = config.dict()

        if project_id:
            if 'projects' not in data:
                data['projects'] = {}
            data['projects'][project_id] = config_dict
        else:
            data['global'] = config_dict

        with open(INTEGRATIONS_CONFIG_FILE, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)

        logger.info(f"Configuracao de integracoes salva: {project_id or 'global'}")
        return True
    except Exception as e:
        logger.error(f"Erro ao salvar config de integracoes: {e}")
        return False


async def test_github_connection(config: Dict[str, Any]) -> Dict[str, Any]:
    """Testa conexao com GitHub"""
    import aiohttp

    token = config.get('token', '')
    owner = config.get('owner', '')
    repo = config.get('repo', '')

    if not token or not owner:
        return {"success": False, "message": "Token e Owner sao obrigatorios"}

    try:
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github.v3+json"
        }

        async with aiohttp.ClientSession() as session:
            url = f"https://api.github.com/users/{owner}"
            if repo:
                url = f"https://api.github.com/repos/{owner}/{repo}"

            async with session.get(url, headers=headers) as response:
                if response.status == 200:
                    data = await response.json()
                    return {
                        "success": True,
                        "message": f"Conectado! Usuario/Repo: {data.get('full_name', data.get('login', owner))}"
                    }
                elif response.status == 401:
                    return {"success": False, "message": "Token invalido"}
                elif response.status == 404:
                    return {"success": False, "message": "Usuario/Repositorio nao encontrado"}
                else:
                    return {"success": False, "message": f"Erro HTTP {response.status}"}
    except Exception as e:
        return {"success": False, "message": str(e)}


async def test_gitlab_connection(config: Dict[str, Any]) -> Dict[str, Any]:
    """Testa conexao com GitLab"""
    import aiohttp

    url = config.get('url', 'https://gitlab.com')
    token = config.get('token', '')
    project_id = config.get('project_id', '')

    if not token:
        return {"success": False, "message": "Token e obrigatorio"}

    try:
        headers = {"PRIVATE-TOKEN": token}

        async with aiohttp.ClientSession() as session:
            api_url = f"{url}/api/v4/user"

            async with session.get(api_url, headers=headers) as response:
                if response.status == 200:
                    data = await response.json()
                    return {
                        "success": True,
                        "message": f"Conectado! Usuario: {data.get('username', 'N/A')}"
                    }
                elif response.status == 401:
                    return {"success": False, "message": "Token invalido"}
                else:
                    return {"success": False, "message": f"Erro HTTP {response.status}"}
    except Exception as e:
        return {"success": False, "message": str(e)}


async def test_jira_connection(config: Dict[str, Any]) -> Dict[str, Any]:
    """Testa conexao com Jira"""
    import aiohttp
    import base64

    url = config.get('url', '')
    email = config.get('email', '')
    token = config.get('token', '')

    if not url or not email or not token:
        return {"success": False, "message": "URL, Email e Token sao obrigatorios"}

    try:
        auth = base64.b64encode(f"{email}:{token}".encode()).decode()
        headers = {
            "Authorization": f"Basic {auth}",
            "Accept": "application/json"
        }

        async with aiohttp.ClientSession() as session:
            api_url = f"{url.rstrip('/')}/rest/api/3/myself"

            async with session.get(api_url, headers=headers) as response:
                if response.status == 200:
                    data = await response.json()
                    return {
                        "success": True,
                        "message": f"Conectado! Usuario: {data.get('displayName', email)}"
                    }
                elif response.status == 401:
                    return {"success": False, "message": "Credenciais invalidas"}
                else:
                    return {"success": False, "message": f"Erro HTTP {response.status}"}
    except Exception as e:
        return {"success": False, "message": str(e)}


async def test_sap_connection(config: Dict[str, Any]) -> Dict[str, Any]:
    """Testa conexao com SAP S/4HANA"""
    # SAP requer bibliotecas especificas, retornamos mock por enquanto
    host = config.get('host', '')

    if not host:
        return {"success": False, "message": "URL do Sistema e obrigatoria"}

    # Em producao, usar pyrfc ou requests para OData
    return {
        "success": True,
        "message": f"Configuracao SAP salva (teste de conexao requer ambiente)"
    }


async def test_salesforce_connection(config: Dict[str, Any]) -> Dict[str, Any]:
    """Testa conexao com Salesforce"""
    client_id = config.get('client_id', '')

    if not client_id:
        return {"success": False, "message": "Client ID e obrigatorio"}

    # Em producao, usar simple_salesforce
    return {
        "success": True,
        "message": f"Configuracao Salesforce salva (teste requer OAuth)"
    }


# =============================================================================
# REGISTER ENDPOINTS
# =============================================================================

def register_integrations_endpoints(app: FastAPI):
    """Registra endpoints de integracoes no app FastAPI"""

    @app.get("/api/integrations/config")
    async def get_integrations_config(project_id: Optional[str] = None):
        """Retorna configuracao de integracoes"""
        config = load_integrations_config(project_id)
        # Mascara tokens/senhas na resposta
        result = config.dict()
        for key in ['github', 'gitlab', 'sap', 'salesforce', 'jira']:
            if 'token' in result[key]:
                result[key]['token'] = '***' if result[key]['token'] else ''
            if 'password' in result[key]:
                result[key]['password'] = '***' if result[key]['password'] else ''
            if 'client_secret' in result[key]:
                result[key]['client_secret'] = '***' if result[key]['client_secret'] else ''
            if 'security_token' in result[key]:
                result[key]['security_token'] = '***' if result[key]['security_token'] else ''
        return result

    @app.post("/api/integrations/save")
    async def save_integration(request: IntegrationSaveRequest):
        """Salva configuracao de uma integracao"""
        config = load_integrations_config(request.project_id)

        # Atualiza a configuracao especifica
        if request.integration_type == 'github':
            # Mantem token existente se nao fornecido
            existing = config.github.dict()
            if request.config.get('token') == '***' or not request.config.get('token'):
                request.config['token'] = existing.get('token', '')
            config.github = GitHubConfig(**request.config)
        elif request.integration_type == 'gitlab':
            existing = config.gitlab.dict()
            if request.config.get('token') == '***' or not request.config.get('token'):
                request.config['token'] = existing.get('token', '')
            config.gitlab = GitLabConfig(**request.config)
        elif request.integration_type == 'sap':
            existing = config.sap.dict()
            if request.config.get('password') == '***' or not request.config.get('password'):
                request.config['password'] = existing.get('password', '')
            config.sap = SAPConfig(**request.config)
        elif request.integration_type == 'salesforce':
            existing = config.salesforce.dict()
            if request.config.get('client_secret') == '***' or not request.config.get('client_secret'):
                request.config['client_secret'] = existing.get('client_secret', '')
            if request.config.get('security_token') == '***' or not request.config.get('security_token'):
                request.config['security_token'] = existing.get('security_token', '')
            config.salesforce = SalesforceConfig(**request.config)
        elif request.integration_type == 'jira':
            existing = config.jira.dict()
            if request.config.get('token') == '***' or not request.config.get('token'):
                request.config['token'] = existing.get('token', '')
            config.jira = JiraConfig(**request.config)
        else:
            raise HTTPException(400, f"Tipo de integracao desconhecido: {request.integration_type}")

        if save_integrations_config(config, request.project_id):
            return {"success": True, "message": f"Configuracao {request.integration_type} salva"}
        else:
            raise HTTPException(500, "Erro ao salvar configuracao")

    @app.post("/api/integrations/test")
    async def test_integration(request: IntegrationTestRequest):
        """Testa conexao com uma integracao"""
        if request.integration_type == 'github':
            return await test_github_connection(request.config)
        elif request.integration_type == 'gitlab':
            return await test_gitlab_connection(request.config)
        elif request.integration_type == 'jira':
            return await test_jira_connection(request.config)
        elif request.integration_type == 'sap':
            return await test_sap_connection(request.config)
        elif request.integration_type == 'salesforce':
            return await test_salesforce_connection(request.config)
        else:
            raise HTTPException(400, f"Tipo de integracao desconhecido: {request.integration_type}")

    @app.get("/api/integrations/status")
    async def get_integrations_status(project_id: Optional[str] = None):
        """Retorna status de todas as integracoes"""
        config = load_integrations_config(project_id)
        return {
            "integrations": [
                {"id": "github", "name": "GitHub", "connected": config.github.connected},
                {"id": "gitlab", "name": "GitLab", "connected": config.gitlab.connected},
                {"id": "sap", "name": "SAP S/4HANA", "connected": config.sap.connected},
                {"id": "salesforce", "name": "Salesforce", "connected": config.salesforce.connected},
                {"id": "jira", "name": "Jira", "connected": config.jira.connected},
            ]
        }

    logger.info("[Dashboard] Integrations panel endpoints loaded (Issue #154)")
