# -*- coding: utf-8 -*-
"""
API Middleware Module
=====================
Middlewares para a API da Fabrica de Agentes.

Terminal 5 - Issue #301
"""

from .tenant_filter import (
    TenantContext,
    TenantMiddleware,
    get_tenant_context,
    require_tenant,
    tenant_filter_dependency,
)

__all__ = [
    "TenantContext",
    "TenantMiddleware",
    "get_tenant_context",
    "require_tenant",
    "tenant_filter_dependency",
]
