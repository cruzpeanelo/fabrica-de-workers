# -*- coding: utf-8 -*-
"""
Cloud Integration Module
========================
Modulo de integracao multi-cloud para a Fabrica de Agentes.

Suporta os seguintes provedores:
- AWS (Amazon Web Services)
- Azure (Microsoft Azure)
- GCP (Google Cloud Platform)

Este modulo fornece uma interface unificada para provisionamento
de infraestrutura em qualquer um dos provedores suportados.
"""

from .base_provider import (
    CloudProvider,
    CloudConfig,
    CloudResource,
    ResourceType,
    ResourceStatus,
    DeploymentResult,
    CostEstimate,
    StackType,
)
from .provisioner import CloudProvisioner

__all__ = [
    # Base classes
    "CloudProvider",
    "CloudConfig",
    "CloudResource",
    "ResourceType",
    "ResourceStatus",
    "DeploymentResult",
    "CostEstimate",
    "StackType",
    # Provisioner
    "CloudProvisioner",
]

__version__ = "1.0.0"
