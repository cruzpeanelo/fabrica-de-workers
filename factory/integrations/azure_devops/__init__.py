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

import sys
import os

# Adiciona o diretorio pai ao path temporariamente para resolver o arquivo azure_devops.py
_parent_dir = os.path.dirname(os.path.dirname(__file__))

# Importa o modulo azure_devops.py diretamente usando exec
_azure_devops_path = os.path.join(_parent_dir, 'azure_devops.py')

# Cria namespace para o modulo
_module_globals = {
    '__name__': 'factory.integrations.azure_devops_file',
    '__file__': _azure_devops_path,
    '__package__': 'factory.integrations',
}

# Importa base primeiro (necessario para azure_devops.py)
from ..base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult,
    map_status_to_internal,
    map_status_to_external,
    map_priority_to_internal,
    map_priority_to_external
)

# Importa diretamente as classes necessarias do arquivo usando importacao dinamica
# que preserva o contexto do pacote
import importlib.machinery
import importlib.util
import types

def _load_azure_devops_module():
    """Carrega o modulo azure_devops.py preservando contexto de pacote."""
    loader = importlib.machinery.SourceFileLoader(
        'factory.integrations._azure_devops_impl',
        _azure_devops_path
    )
    spec = importlib.util.spec_from_loader(
        'factory.integrations._azure_devops_impl',
        loader,
        origin=_azure_devops_path
    )
    module = importlib.util.module_from_spec(spec)

    # Define o package para que importacoes relativas funcionem
    module.__package__ = 'factory.integrations'

    # Registra no sys.modules antes de executar
    sys.modules['factory.integrations._azure_devops_impl'] = module

    # Executa o modulo
    spec.loader.exec_module(module)

    return module

_azure_module = _load_azure_devops_module()

# Re-exporta as classes do modulo carregado
AzureDevOpsIntegration = _azure_module.AzureDevOpsIntegration
AzureDevOpsConfig = _azure_module.AzureDevOpsConfig
AzureWorkItemType = _azure_module.AzureWorkItemType
get_azure_devops_integration = _azure_module.get_azure_devops_integration
init_azure_devops_integration = _azure_module.init_azure_devops_integration

# Exporta skills
from .skills import (
    DevOpsReadSkill,
    DevOpsPipelineSkill,
    DevOpsSyncSkill,
    SkillResult,
    SyncResult as SkillSyncResult,
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
    'SkillSyncResult',
    'BuildStatus',
    'BuildResult',
    'SyncDirection',
    'SyncStatus'
]
