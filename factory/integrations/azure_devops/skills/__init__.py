# -*- coding: utf-8 -*-
"""
Azure DevOps Skills
===================
Skills para agentes especializados em Azure DevOps.

Disponibiliza:
- DevOpsReadSkill: Leitura de work items, projetos e repositorios
- DevOpsPipelineSkill: Gerenciamento de pipelines e builds
- DevOpsSyncSkill: Sincronizacao bidirecional com stories

Uso:
    from factory.integrations.azure_devops.skills import (
        DevOpsReadSkill,
        DevOpsPipelineSkill,
        DevOpsSyncSkill
    )

    # Inicializa com cliente Azure DevOps autenticado
    from factory.integrations.azure_devops import get_azure_devops_integration

    azure_client = get_azure_devops_integration()
    await azure_client.connect()

    # Skills de leitura
    read_skill = DevOpsReadSkill(azure_client)
    result = await read_skill.get_work_item(123)

    # Skills de pipeline
    pipeline_skill = DevOpsPipelineSkill(azure_client)
    result = await pipeline_skill.trigger_build(pipeline_id=456)

    # Skills de sincronizacao
    sync_skill = DevOpsSyncSkill(azure_client)
    result = await sync_skill.sync_story(story=my_story)
"""

from .devops_read_skill import DevOpsReadSkill, SkillResult
from .devops_pipeline_skill import DevOpsPipelineSkill, BuildStatus, BuildResult
from .devops_sync_skill import DevOpsSyncSkill, SyncDirection, SyncStatus, SyncResult

__all__ = [
    # Skills
    'DevOpsReadSkill',
    'DevOpsPipelineSkill',
    'DevOpsSyncSkill',
    # Result types
    'SkillResult',
    'SyncResult',
    # Enums
    'BuildStatus',
    'BuildResult',
    'SyncDirection',
    'SyncStatus'
]
