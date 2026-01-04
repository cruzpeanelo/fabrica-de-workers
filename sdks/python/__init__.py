"""
Plataforma E - Python SDK
===============================

Cliente Python oficial para a API da Plataforma E.

Instalacao:
    pip install fabrica-agentes-sdk

Uso:
    from fabrica_client import FabricaClient

    client = FabricaClient(api_key="sua-api-key")
    projects = client.projects.list()
"""

from .fabrica_client import (
    FabricaClient,
    create_client,
    # Enums
    JobStatus,
    StoryStatus,
    Priority,
    Complexity,
    # Models
    Project,
    Story,
    Job,
    Worker,
    Agent,
    PaginatedResponse,
    # Errors
    FabricaError,
    APIError,
    AuthenticationError,
    RateLimitError,
    ValidationError,
    NotFoundError,
    TimeoutError,
)

__version__ = "1.0.0"
__all__ = [
    "FabricaClient",
    "create_client",
    "JobStatus",
    "StoryStatus",
    "Priority",
    "Complexity",
    "Project",
    "Story",
    "Job",
    "Worker",
    "Agent",
    "PaginatedResponse",
    "FabricaError",
    "APIError",
    "AuthenticationError",
    "RateLimitError",
    "ValidationError",
    "NotFoundError",
    "TimeoutError",
]
