# -*- coding: utf-8 -*-
"""
Plataforma E SDK
======================

SDK oficial Python para integracao com a Plataforma E.

Instalacao:
    pip install fabrica-sdk

Uso basico:
    from fabrica import FabricaClient

    client = FabricaClient(api_key='sk-fab_xxx_yyy')

    # Criar projeto
    project = client.projects.create(
        name='Meu App',
        project_type='web-app'
    )

    # Criar story
    story = client.stories.create(
        project_id=project.id,
        title='Login com Google',
        persona='usuario',
        action='fazer login com minha conta Google',
        benefit='nao precise criar nova senha'
    )

    # Executar desenvolvimento
    job = client.stories.execute(story.id)
    job.wait_for_completion()

    print(f'Arquivos gerados: {job.files_created}')

Documentacao completa: https://docs.fabricadeagentes.com/sdk/python
"""

from factory.sdk.client import FabricaClient
from factory.sdk.models import (
    Project,
    Story,
    Job,
    Webhook,
    APIKeyInfo,
)
from factory.sdk.exceptions import (
    FabricaError,
    AuthenticationError,
    RateLimitError,
    NotFoundError,
    ValidationError,
)

__version__ = "1.0.0"
__author__ = "Plataforma E"
__email__ = "support@fabricadeagentes.com"

__all__ = [
    # Cliente principal
    "FabricaClient",

    # Modelos
    "Project",
    "Story",
    "Job",
    "Webhook",
    "APIKeyInfo",

    # Excecoes
    "FabricaError",
    "AuthenticationError",
    "RateLimitError",
    "NotFoundError",
    "ValidationError",

    # Metadata
    "__version__",
]
