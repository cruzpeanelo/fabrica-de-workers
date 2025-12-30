# -*- coding: utf-8 -*-
"""
Jira Integration Package
========================
Sub-package for Jira integration modules with tenant isolation.

Issue #314 - Tenant isolation for Jira integration.
Issue #310 - Jira Skills for agent system.

Funcionalidades:
- JiraConfig: Configuracao com tenant_id e integracao com SecretsManager
- JiraIntegration: Cliente Jira com isolamento por tenant
- Per-tenant session caching
- Audit logging with tenant context
- Webhook validation with tenant support
"""

import sys
import os
import importlib.machinery
import importlib.util

from .config import JiraConfig
from .skills import JiraReadSkill, JiraSyncSkill, JiraAgileSkill

# Re-export JiraIntegration from parent module for backwards compatibility
# The main JiraIntegration class is in factory/integrations/jira.py
_parent_dir = os.path.dirname(os.path.dirname(__file__))
_jira_file = os.path.join(_parent_dir, "jira.py")


def _load_jira_module():
    """Carrega o modulo jira.py preservando contexto de pacote."""
    loader = importlib.machinery.SourceFileLoader(
        'factory.integrations._jira_impl',
        _jira_file
    )
    spec = importlib.util.spec_from_loader(
        'factory.integrations._jira_impl',
        loader,
        origin=_jira_file
    )
    module = importlib.util.module_from_spec(spec)

    # Define o package para que importacoes relativas funcionem
    module.__package__ = 'factory.integrations'

    # Registra no sys.modules antes de executar
    sys.modules['factory.integrations._jira_impl'] = module

    # Executa o modulo
    spec.loader.exec_module(module)

    return module


# Carrega o modulo jira.py
try:
    _jira_mod = _load_jira_module()

    JiraIntegration = _jira_mod.JiraIntegration
    JiraAuditEntry = _jira_mod.JiraAuditEntry
    JiraIssueType = _jira_mod.JiraIssueType
    get_jira_integration = _jira_mod.get_jira_integration
    get_jira_integration_for_tenant = _jira_mod.get_jira_integration_for_tenant
    init_jira_integration = _jira_mod.init_jira_integration
    cleanup_jira_integrations = _jira_mod.cleanup_jira_integrations
except Exception as e:
    import logging
    logging.getLogger(__name__).warning(f"Erro ao carregar jira.py: {e}")
    JiraIntegration = None
    JiraAuditEntry = None
    JiraIssueType = None
    get_jira_integration = None
    get_jira_integration_for_tenant = None
    init_jira_integration = None
    cleanup_jira_integrations = None

__all__ = [
    # Configuration
    "JiraConfig",
    # Main integration class
    "JiraIntegration",
    "JiraAuditEntry",
    "JiraIssueType",
    # Factory functions
    "get_jira_integration",
    "get_jira_integration_for_tenant",
    "init_jira_integration",
    "cleanup_jira_integrations",
    # Skills
    "JiraReadSkill",
    "JiraSyncSkill",
    "JiraAgileSkill"
]
