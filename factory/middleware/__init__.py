# -*- coding: utf-8 -*-
"""
Middleware Module for Plataforma E
=========================================

This module contains middleware implementations for:
- Plan Limits Enforcement (Issue #104)
- Global Tenant Context (Issue #105)

Author: Plataforma E
"""

from .plan_limits import (
    PlanLimitsMiddleware,
    PlanLimitChecker,
    enforce_plan_limit,
    get_plan_limit_checker,
)

from .tenant_middleware import (
    GlobalTenantMiddleware,
    TenantContextDependency,
    get_tenant_context,
    get_required_tenant,
    require_admin_tenant_switch,
)

__all__ = [
    # Plan Limits (Issue #104)
    "PlanLimitsMiddleware",
    "PlanLimitChecker",
    "enforce_plan_limit",
    "get_plan_limit_checker",
    # Tenant Middleware (Issue #105)
    "GlobalTenantMiddleware",
    "TenantContextDependency",
    "get_tenant_context",
    "get_required_tenant",
    "require_admin_tenant_switch",
]
