# -*- coding: utf-8 -*-
"""
SAP Cloud Platform Integration (CPI) Module
============================================

Integracao completa com SAP CPI para gerenciamento de iFlows,
packages, mapeamentos e scripts Groovy.

Funcionalidades:
- Conexao via OAuth 2.0 com SAP CPI OData API
- Gerenciamento de Integration Packages
- Gerenciamento de Integration Flows (iFlows)
- Analise de mapeamentos e scripts Groovy
- Deploy e monitoramento de integracoes
- Geracao automatica de iFlows

Configuracao via variaveis de ambiente:
- SAP_CPI_TENANT_URL: URL do tenant CPI
- SAP_CPI_TOKEN_URL: URL do OAuth token endpoint
- SAP_CPI_CLIENT_ID: Client ID OAuth
- SAP_CPI_CLIENT_SECRET: Client Secret OAuth
- SAP_CPI_ENABLED: Habilitar integracao (true/false)

Exemplo de uso:
```python
from factory.integrations.sap_cpi import SAPCPIIntegration, SAPCPIConfig

config = SAPCPIConfig.from_env()
cpi = SAPCPIIntegration(config)

if await cpi.connect():
    # Listar packages
    packages = await cpi.package_manager.list_packages()

    # Listar iFlows de um package
    iflows = await cpi.iflow_manager.list_iflows("MyPackage")

    # Deploy de iFlow
    await cpi.deployer.deploy_iflow("MyPackage", "MyIFlow")
```
"""

from .client import SAPCPIClient, SAPCPIConfig
from .sap_cpi import SAPCPIIntegration, get_sap_cpi_integration, init_sap_cpi_integration
from .package_manager import PackageManager
from .iflow_manager import IFlowManager
from .deployer import CPIDeployer
from .analyzers import IFlowAnalyzer, MappingAnalyzer, ScriptAnalyzer
from .generators import IFlowGenerator, MappingGenerator, GroovyGenerator

__all__ = [
    # Principal
    'SAPCPIIntegration',
    'SAPCPIConfig',
    'SAPCPIClient',
    'get_sap_cpi_integration',
    'init_sap_cpi_integration',

    # Managers
    'PackageManager',
    'IFlowManager',
    'CPIDeployer',

    # Analyzers
    'IFlowAnalyzer',
    'MappingAnalyzer',
    'ScriptAnalyzer',

    # Generators
    'IFlowGenerator',
    'MappingGenerator',
    'GroovyGenerator',
]
