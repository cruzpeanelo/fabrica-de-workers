# -*- coding: utf-8 -*-
"""
Salesforce Integration Module
=============================
Integracao completa com Salesforce CRM para a Fabrica de Agentes.

Este modulo fornece:
- Cliente REST API para operacoes CRUD
- Cliente Metadata API para deploy de componentes
- Cliente Tooling API para desenvolvimento Apex
- Bulk API para operacoes em massa
- Analisadores de objetos, codigo Apex e fluxos
- Geradores de codigo Apex e Lightning Web Components
- Skills para agentes especializados Salesforce

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient, SalesforceConfig

    config = SalesforceConfig(
        username="user@empresa.com",
        password="senha123",
        security_token="token_seguranca",
        domain="login"  # ou "test" para sandbox
    )

    client = SalesforceClient(config)
    await client.connect()

    # Consultar contas
    accounts = await client.query("SELECT Id, Name FROM Account LIMIT 10")

    # Criar registro
    result = await client.create("Account", {"Name": "Nova Empresa"})
"""

from .config import SalesforceConfig, SalesforceOAuthConfig
from .client import SalesforceClient
from .metadata_client import MetadataClient
from .tooling_client import ToolingClient
from .bulk_client import BulkClient

__all__ = [
    'SalesforceConfig',
    'SalesforceOAuthConfig',
    'SalesforceClient',
    'MetadataClient',
    'ToolingClient',
    'BulkClient'
]

__version__ = '1.0.0'
__author__ = 'Fabrica de Agentes'
