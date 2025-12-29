# -*- coding: utf-8 -*-
"""
SAP S/4HANA Integration Module
==============================
Integracao completa com SAP S/4HANA para desenvolvimento, analise e modificacao
usando APIs modernas OData V4, SAP Graph e tecnologias S/4.

Funcionalidades:
- Conexao via OData V4 e SAP Graph
- Analise de CDS Views, Fiori Apps e RAP
- Geracao de CDS Views e Fiori Elements
- Integracao com SAP BTP

Autor: Fabrica de Agentes
Versao: 1.0.0
"""

from .odata_v4_client import (
    ODataV4Client,
    ODataConfig,
    ODataError,
    ODataAuthError,
    ODataQueryBuilder
)

from .graph_client import (
    SAPGraphClient,
    GraphConfig,
    GraphError
)

from .sap_auth import (
    SAPAuthenticator,
    SAPOAuthConfig,
    SAPBasicAuthConfig,
    SAPAuthError
)

from .sap_config import (
    SAPS4Config,
    SAPS4Environment,
    get_default_config,
    load_config_from_env
)

# Analisadores
from .analyzers.cds_analyzer import CDSAnalyzer, CDSViewInfo
from .analyzers.fiori_analyzer import FioriAnalyzer, FioriAppInfo
from .analyzers.rap_analyzer import RAPAnalyzer, RAPServiceInfo

# Geradores
from .generators.cds_generator import CDSGenerator, CDSViewSpec
from .generators.fiori_generator import FioriGenerator, FioriAppSpec
from .generators.rap_generator import RAPGenerator, RAPServiceSpec

# Skills
from .skills.s4_read_skill import S4ReadSkill
from .skills.s4_cds_skill import S4CDSSkill
from .skills.s4_fiori_skill import S4FioriSkill

__version__ = "1.0.0"
__author__ = "Fabrica de Agentes"

__all__ = [
    # Cliente OData
    'ODataV4Client',
    'ODataConfig',
    'ODataError',
    'ODataAuthError',
    'ODataQueryBuilder',

    # Cliente Graph
    'SAPGraphClient',
    'GraphConfig',
    'GraphError',

    # Autenticacao
    'SAPAuthenticator',
    'SAPOAuthConfig',
    'SAPBasicAuthConfig',
    'SAPAuthError',

    # Configuracao
    'SAPS4Config',
    'SAPS4Environment',
    'get_default_config',
    'load_config_from_env',

    # Analisadores
    'CDSAnalyzer',
    'CDSViewInfo',
    'FioriAnalyzer',
    'FioriAppInfo',
    'RAPAnalyzer',
    'RAPServiceInfo',

    # Geradores
    'CDSGenerator',
    'CDSViewSpec',
    'FioriGenerator',
    'FioriAppSpec',
    'RAPGenerator',
    'RAPServiceSpec',

    # Skills
    'S4ReadSkill',
    'S4CDSSkill',
    'S4FioriSkill'
]
