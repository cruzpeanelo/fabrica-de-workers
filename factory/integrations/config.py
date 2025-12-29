# -*- coding: utf-8 -*-
"""
Integration Configuration Module
=================================
Configuracoes para integracoes com Jira e Azure DevOps.

Variaveis de ambiente:

JIRA:
    JIRA_ENABLED=false
    JIRA_URL=https://empresa.atlassian.net
    JIRA_EMAIL=usuario@empresa.com
    JIRA_API_TOKEN=your-api-token
    JIRA_PROJECT_KEY=PROJ
    JIRA_DEFAULT_ISSUE_TYPE=Story
    JIRA_SYNC_COMMENTS=true
    JIRA_SYNC_ATTACHMENTS=false
    JIRA_AUTO_SYNC=false
    JIRA_SYNC_INTERVAL=30
    JIRA_WEBHOOK_SECRET=

AZURE DEVOPS:
    AZURE_DEVOPS_ENABLED=false
    AZURE_DEVOPS_ORG=sua-organizacao
    AZURE_DEVOPS_PROJECT=seu-projeto
    AZURE_DEVOPS_PAT=your-personal-access-token
    AZURE_DEVOPS_TEAM=
    AZURE_DEVOPS_WORK_ITEM_TYPE=User Story
    AZURE_DEVOPS_AREA_PATH=
    AZURE_DEVOPS_ITERATION_PATH=
    AZURE_DEVOPS_SYNC_COMMENTS=true
    AZURE_DEVOPS_SYNC_ATTACHMENTS=false
    AZURE_DEVOPS_AUTO_SYNC=false
    AZURE_DEVOPS_SYNC_INTERVAL=30
    AZURE_DEVOPS_WEBHOOK_SECRET=
"""

import os
from dotenv import load_dotenv

# Carregar variaveis de ambiente
load_dotenv()


# =============================================================================
# JIRA CONFIGURATION
# =============================================================================

JIRA_ENABLED = os.getenv("JIRA_ENABLED", "false").lower() == "true"
JIRA_URL = os.getenv("JIRA_URL", "")
JIRA_EMAIL = os.getenv("JIRA_EMAIL", "")
JIRA_API_TOKEN = os.getenv("JIRA_API_TOKEN", "")
JIRA_PROJECT_KEY = os.getenv("JIRA_PROJECT_KEY", "")
JIRA_DEFAULT_ISSUE_TYPE = os.getenv("JIRA_DEFAULT_ISSUE_TYPE", "Story")
JIRA_SYNC_COMMENTS = os.getenv("JIRA_SYNC_COMMENTS", "true").lower() == "true"
JIRA_SYNC_ATTACHMENTS = os.getenv("JIRA_SYNC_ATTACHMENTS", "false").lower() == "true"
JIRA_AUTO_SYNC = os.getenv("JIRA_AUTO_SYNC", "false").lower() == "true"
JIRA_SYNC_INTERVAL = int(os.getenv("JIRA_SYNC_INTERVAL", "30"))
JIRA_WEBHOOK_SECRET = os.getenv("JIRA_WEBHOOK_SECRET", "")


# =============================================================================
# AZURE DEVOPS CONFIGURATION
# =============================================================================

AZURE_DEVOPS_ENABLED = os.getenv("AZURE_DEVOPS_ENABLED", "false").lower() == "true"
AZURE_DEVOPS_ORG = os.getenv("AZURE_DEVOPS_ORG", "")
AZURE_DEVOPS_PROJECT = os.getenv("AZURE_DEVOPS_PROJECT", "")
AZURE_DEVOPS_PAT = os.getenv("AZURE_DEVOPS_PAT", "")
AZURE_DEVOPS_TEAM = os.getenv("AZURE_DEVOPS_TEAM", "")
AZURE_DEVOPS_WORK_ITEM_TYPE = os.getenv("AZURE_DEVOPS_WORK_ITEM_TYPE", "User Story")
AZURE_DEVOPS_AREA_PATH = os.getenv("AZURE_DEVOPS_AREA_PATH", "")
AZURE_DEVOPS_ITERATION_PATH = os.getenv("AZURE_DEVOPS_ITERATION_PATH", "")
AZURE_DEVOPS_SYNC_COMMENTS = os.getenv("AZURE_DEVOPS_SYNC_COMMENTS", "true").lower() == "true"
AZURE_DEVOPS_SYNC_ATTACHMENTS = os.getenv("AZURE_DEVOPS_SYNC_ATTACHMENTS", "false").lower() == "true"
AZURE_DEVOPS_AUTO_SYNC = os.getenv("AZURE_DEVOPS_AUTO_SYNC", "false").lower() == "true"
AZURE_DEVOPS_SYNC_INTERVAL = int(os.getenv("AZURE_DEVOPS_SYNC_INTERVAL", "30"))
AZURE_DEVOPS_WEBHOOK_SECRET = os.getenv("AZURE_DEVOPS_WEBHOOK_SECRET", "")


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_jira_config() -> dict:
    """Retorna configuracao do Jira."""
    return {
        "enabled": JIRA_ENABLED,
        "url": JIRA_URL,
        "email": JIRA_EMAIL,
        "project_key": JIRA_PROJECT_KEY,
        "default_issue_type": JIRA_DEFAULT_ISSUE_TYPE,
        "sync_comments": JIRA_SYNC_COMMENTS,
        "sync_attachments": JIRA_SYNC_ATTACHMENTS,
        "auto_sync": JIRA_AUTO_SYNC,
        "sync_interval": JIRA_SYNC_INTERVAL,
        "configured": bool(JIRA_URL and JIRA_EMAIL and JIRA_API_TOKEN)
    }


def get_azure_devops_config() -> dict:
    """Retorna configuracao do Azure DevOps."""
    return {
        "enabled": AZURE_DEVOPS_ENABLED,
        "organization": AZURE_DEVOPS_ORG,
        "project": AZURE_DEVOPS_PROJECT,
        "team": AZURE_DEVOPS_TEAM,
        "work_item_type": AZURE_DEVOPS_WORK_ITEM_TYPE,
        "area_path": AZURE_DEVOPS_AREA_PATH,
        "iteration_path": AZURE_DEVOPS_ITERATION_PATH,
        "sync_comments": AZURE_DEVOPS_SYNC_COMMENTS,
        "sync_attachments": AZURE_DEVOPS_SYNC_ATTACHMENTS,
        "auto_sync": AZURE_DEVOPS_AUTO_SYNC,
        "sync_interval": AZURE_DEVOPS_SYNC_INTERVAL,
        "configured": bool(AZURE_DEVOPS_ORG and AZURE_DEVOPS_PROJECT and AZURE_DEVOPS_PAT)
    }


def get_integration_config(integration: str) -> dict:
    """
    Retorna configuracao de uma integracao.

    Args:
        integration: Nome da integracao (jira, azure_devops)

    Returns:
        Dict com configuracoes
    """
    if integration == "jira":
        return get_jira_config()
    elif integration == "azure_devops":
        return get_azure_devops_config()
    return {}


def get_all_integrations_config() -> dict:
    """Retorna configuracao de todas as integracoes."""
    return {
        "jira": get_jira_config(),
        "azure_devops": get_azure_devops_config()
    }


def is_jira_configured() -> bool:
    """Verifica se Jira esta configurado."""
    return bool(JIRA_URL and JIRA_EMAIL and JIRA_API_TOKEN)


def is_azure_devops_configured() -> bool:
    """Verifica se Azure DevOps esta configurado."""
    return bool(AZURE_DEVOPS_ORG and AZURE_DEVOPS_PROJECT and AZURE_DEVOPS_PAT)
