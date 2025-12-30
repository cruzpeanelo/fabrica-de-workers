# -*- coding: utf-8 -*-
"""
Azure DevOps Integration Package
================================
Pacote completo para integracao com Azure DevOps.

Inclui:
- AzureDevOpsIntegration: Cliente principal de integracao
- Skills: DevOpsReadSkill, DevOpsPipelineSkill, DevOpsSyncSkill

Uso:
    from factory.integrations.azure_devops import (
        AzureDevOpsIntegration,
        AzureDevOpsConfig,
        get_azure_devops_integration
    )

    # Ou importar skills diretamente
    from factory.integrations.azure_devops.skills import (
        DevOpsReadSkill,
        DevOpsPipelineSkill,
        DevOpsSyncSkill
    )
"""

# Re-exporta do modulo principal
from ..azure_devops import (
    AzureDevOpsIntegration,
    AzureDevOpsConfig,
    AzureWorkItemType,
    get_azure_devops_integration,
    init_azure_devops_integration
)

# Exporta skills
from .skills import (
    DevOpsReadSkill,
    DevOpsPipelineSkill,
    DevOpsSyncSkill,
    SkillResult,
    SyncResult,
    BuildStatus,
    BuildResult,
    SyncDirection,
    SyncStatus
)

__all__ = [
    # Integration
    'AzureDevOpsIntegration',
    'AzureDevOpsConfig',
    'AzureWorkItemType',
    'get_azure_devops_integration',
    'init_azure_devops_integration',
    # Skills
    'DevOpsReadSkill',
    'DevOpsPipelineSkill',
    'DevOpsSyncSkill',
    # Types
    'SkillResult',
    'SyncResult',
    'BuildStatus',
    'BuildResult',
    'SyncDirection',
    'SyncStatus'
]
