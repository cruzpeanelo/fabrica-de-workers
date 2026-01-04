# -*- coding: utf-8 -*-
"""
Azure Cloud Provider Module
===========================
Implementacao do provider Azure para a Plataforma E.

Recursos suportados:
- Virtual Machines (Computacao)
- Blob Storage (Storage)
- Functions (Serverless)
- Azure SQL / PostgreSQL (Banco de dados)
"""

from .provider import AzureProvider
from .virtual_machines import VMManager
from .storage import StorageManager
from .functions import FunctionsManager
from .database import DatabaseManager

__all__ = [
    "AzureProvider",
    "VMManager",
    "StorageManager",
    "FunctionsManager",
    "DatabaseManager",
]
