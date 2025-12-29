# -*- coding: utf-8 -*-
"""
SAP Integration Package
=======================
Conectores para SAP S/4HANA e ECC.
"""

from .s4hana_connector import (
    S4HANAConnector,
    S4HANAConfig,
    BusinessPartnerService,
    SalesOrderService,
    S4HANAAuthType
)

__all__ = [
    "S4HANAConnector",
    "S4HANAConfig",
    "BusinessPartnerService",
    "SalesOrderService",
    "S4HANAAuthType"
]
