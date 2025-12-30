# -*- coding: utf-8 -*-
"""
SharePoint Integration Module
=============================
Integracao com Microsoft SharePoint via Graph API.
Permite leitura e sincronizacao de documentos, listas e sites.

Terminal 5 - Issue #298
"""

from .config import SharePointConfig, SharePointScope
from .graph_client import SharePointGraphClient
from .site_client import SiteClient
from .list_client import ListClient
from .document_client import DocumentClient

__all__ = [
    "SharePointConfig",
    "SharePointScope",
    "SharePointGraphClient",
    "SiteClient",
    "ListClient",
    "DocumentClient",
]
