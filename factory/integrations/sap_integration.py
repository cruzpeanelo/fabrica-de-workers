# -*- coding: utf-8 -*-
"""
SAP Integration Facade
======================
Modulo de entrada unico para integracao SAP S/4HANA.

Use este modulo para importar todos os componentes da integracao SAP:

```python
from factory.integrations.sap_integration import (
    SAPS4HanaIntegration,
    connect_sap_s4hana,
    ODataV4Client,
    CDSGenerator,
    FioriGenerator
)
```

Ou importe diretamente:
```python
from factory.integrations.sap_s4hana import SAPS4HanaIntegration
from factory.integrations.sap_s4 import ODataV4Client
```
"""

# Main Integration
from .sap_s4hana import (
    SAPS4HanaIntegration,
    ConnectionStatus,
    connect_sap_s4hana,
    create_sap_s4hana_integration
)

# OData Client
from .sap_s4.odata_v4_client import (
    ODataV4Client,
    ODataConfig,
    ODataError,
    ODataAuthError,
    ODataQueryBuilder
)

# SAP Graph Client
from .sap_s4.graph_client import (
    SAPGraphClient,
    GraphConfig,
    GraphError
)

# Authentication
from .sap_s4.sap_auth import (
    SAPAuthenticator,
    SAPOAuthConfig,
    SAPBasicAuthConfig,
    SAPAuthError
)

# Configuration
from .sap_s4.sap_config import (
    SAPS4Config,
    SAPS4Environment,
    get_default_config,
    load_config_from_env
)

# Analyzers
from .sap_s4.analyzers import (
    CDSAnalyzer,
    CDSViewInfo,
    FioriAnalyzer,
    FioriAppInfo,
    RAPAnalyzer,
    RAPServiceInfo
)

# Generators
from .sap_s4.generators import (
    CDSGenerator,
    CDSViewSpec,
    FioriGenerator,
    FioriAppSpec,
    RAPGenerator,
    RAPServiceSpec
)

# Skills
from .sap_s4.skills import (
    S4ReadSkill,
    S4CDSSkill,
    S4FioriSkill
)

__all__ = [
    # Main
    'SAPS4HanaIntegration',
    'ConnectionStatus',
    'connect_sap_s4hana',
    'create_sap_s4hana_integration',

    # OData
    'ODataV4Client',
    'ODataConfig',
    'ODataError',
    'ODataAuthError',
    'ODataQueryBuilder',

    # Graph
    'SAPGraphClient',
    'GraphConfig',
    'GraphError',

    # Auth
    'SAPAuthenticator',
    'SAPOAuthConfig',
    'SAPBasicAuthConfig',
    'SAPAuthError',

    # Config
    'SAPS4Config',
    'SAPS4Environment',
    'get_default_config',
    'load_config_from_env',

    # Analyzers
    'CDSAnalyzer',
    'CDSViewInfo',
    'FioriAnalyzer',
    'FioriAppInfo',
    'RAPAnalyzer',
    'RAPServiceInfo',

    # Generators
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
