# -*- coding: utf-8 -*-
"""
GCP Cloud Provider Module
=========================
Implementacao do provider Google Cloud Platform para a Fabrica de Agentes.

Recursos suportados:
- Compute Engine (Maquinas virtuais)
- Cloud Storage (Storage)
- Cloud Functions (Serverless)
- Cloud SQL (Banco de dados gerenciado)
"""

from .provider import GCPProvider
from .compute import ComputeManager
from .storage import CloudStorageManager
from .functions import CloudFunctionsManager
from .sql import CloudSQLManager

__all__ = [
    "GCPProvider",
    "ComputeManager",
    "CloudStorageManager",
    "CloudFunctionsManager",
    "CloudSQLManager",
]
