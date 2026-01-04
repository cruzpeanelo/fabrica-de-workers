# -*- coding: utf-8 -*-
"""
SAP ECC Integration Module
==========================
Integracao com SAP ECC (ERP Central Component).

Este modulo fornece:
- Cliente RFC para conexao direta via PyRFC
- Cliente OData para SAP Gateway
- Analisadores de codigo ABAP, tabelas, BADIs e configuracoes
- Geradores de codigo ABAP, Function Modules, Classes OO e IDocs
- Skills especializadas para agentes SAP

Requisitos:
-----------
- SAP NW RFC SDK instalado no sistema
- PyRFC: pip install pyrfc
- Requests: pip install requests

Configuracao:
-------------
Configure as credenciais em factory/config.py ou via variaveis de ambiente:
    SAP_RFC_ASHOST, SAP_RFC_SYSNR, SAP_RFC_CLIENT, etc.

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc import SAPECCIntegration, SAPECCConfig

config = SAPECCConfig(
    rfc_host="sap-server.company.com",
    rfc_sysnr="00",
    rfc_client="100",
    rfc_user="RFC_USER",
    rfc_password="xxx"
)

sap = SAPECCIntegration(config)
await sap.connect()

# Listar programas ABAP
programs = await sap.list_abap_programs(prefix="Z*")

# Analisar codigo ABAP
analysis = await sap.analyze_abap_code("ZSAMPLE_REPORT")
```

Autor: Plataforma E
Versao: 1.0.0
"""

from .rfc_client import (
    SAPRFCClient,
    SAPRFCConfig,
    RFCConnectionError,
    RFCExecutionError
)

from .odata_client import (
    SAPODataClient,
    SAPODataConfig,
    ODataRequestError
)

from .sap_ecc_integration import (
    SAPECCIntegration,
    SAPECCConfig,
    SAPModule,
    SAPConnectionStatus
)

from .analyzers import (
    ABAPAnalyzer,
    TableAnalyzer,
    BADIAnalyzer,
    ConfigAnalyzer,
    ABAPAnalysisResult,
    TableStructure,
    BADIInfo,
    IMGConfig
)

from .generators import (
    ABAPGenerator,
    FunctionGenerator,
    ClassGenerator,
    IDocGenerator,
    ABAPProgram,
    FunctionModule,
    ABAPClass,
    IDocDefinition
)

from .skills import (
    SAPReadSkill,
    SAPABAPSkill,
    SAPConfigSkill
)

__all__ = [
    # Cliente RFC
    'SAPRFCClient',
    'SAPRFCConfig',
    'RFCConnectionError',
    'RFCExecutionError',

    # Cliente OData
    'SAPODataClient',
    'SAPODataConfig',
    'ODataRequestError',

    # Integracao Principal
    'SAPECCIntegration',
    'SAPECCConfig',
    'SAPModule',
    'SAPConnectionStatus',

    # Analisadores
    'ABAPAnalyzer',
    'TableAnalyzer',
    'BADIAnalyzer',
    'ConfigAnalyzer',
    'ABAPAnalysisResult',
    'TableStructure',
    'BADIInfo',
    'IMGConfig',

    # Geradores
    'ABAPGenerator',
    'FunctionGenerator',
    'ClassGenerator',
    'IDocGenerator',
    'ABAPProgram',
    'FunctionModule',
    'ABAPClass',
    'IDocDefinition',

    # Skills
    'SAPReadSkill',
    'SAPABAPSkill',
    'SAPConfigSkill'
]

__version__ = '1.0.0'
__author__ = 'Plataforma E'
