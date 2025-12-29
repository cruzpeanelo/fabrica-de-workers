# -*- coding: utf-8 -*-
"""
Factory Integrations Module
===========================
Integracoes com sistemas externos como Jira e Azure DevOps.
"""

from .jira import JiraIntegration, JiraConfig
from .azure_devops import AzureDevOpsIntegration, AzureDevOpsConfig
from .base import IntegrationBase, IntegrationStatus, SyncResult

__all__ = [
    'JiraIntegration',
    'JiraConfig',
    'AzureDevOpsIntegration',
    'AzureDevOpsConfig',
    'IntegrationBase',
    'IntegrationStatus',
    'SyncResult'
]
