# -*- coding: utf-8 -*-
"""
Factory Integrations Module
===========================
Integracoes com sistemas externos.

Incluido:
- Jira e Azure DevOps (Project Management)
- GitHub e GitLab (Version Control)
- Power BI, Tableau, Excel (BI Tools)
- Gateway centralizado com isolamento por tenant
"""

# Base
from .base import IntegrationBase, IntegrationStatus, SyncResult

# Project Management
from .jira import JiraIntegration, JiraConfig
from .azure_devops import AzureDevOpsIntegration, AzureDevOpsConfig

# Version Control
from .github import GitHubIntegration, GitHubConfig
from .gitlab_integration import GitLabIntegration, GitLabConfig

# BI Tools
from .powerbi_connector import PowerBIConnector, get_powerbi_connector
from .tableau_connector import TableauConnector, get_tableau_connector
from .excel_exporter import ExcelExporter, get_excel_exporter

# Gateway
from .gateway import (
    IntegrationGateway,
    IntegrationType,
    get_integration_gateway,
    get_tenant_integrations
)

__all__ = [
    # Base
    'IntegrationBase',
    'IntegrationStatus',
    'SyncResult',

    # Project Management
    'JiraIntegration',
    'JiraConfig',
    'AzureDevOpsIntegration',
    'AzureDevOpsConfig',

    # Version Control
    'GitHubIntegration',
    'GitHubConfig',
    'GitLabIntegration',
    'GitLabConfig',

    # BI Tools
    'PowerBIConnector',
    'get_powerbi_connector',
    'TableauConnector',
    'get_tableau_connector',
    'ExcelExporter',
    'get_excel_exporter',

    # Gateway
    'IntegrationGateway',
    'IntegrationType',
    'get_integration_gateway',
    'get_tenant_integrations'
]
