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

from .config import JiraConfig
from .skills import JiraReadSkill, JiraSyncSkill, JiraAgileSkill

# Re-export JiraIntegration from parent module for backwards compatibility
# The main JiraIntegration class is in factory/integrations/jira.py
import sys
import os

# Import JiraIntegration from the parent jira.py file
_parent_dir = os.path.dirname(os.path.dirname(__file__))
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

try:
    # Import from the jira.py file in factory/integrations/
    from importlib.util import spec_from_file_location, module_from_spec
    _jira_file = os.path.join(_parent_dir, "jira.py")
    if os.path.exists(_jira_file):
        _spec = spec_from_file_location("jira_module", _jira_file)
        _jira_mod = module_from_spec(_spec)
        _spec.loader.exec_module(_jira_mod)
        JiraIntegration = _jira_mod.JiraIntegration
        JiraAuditEntry = _jira_mod.JiraAuditEntry
        JiraIssueType = _jira_mod.JiraIssueType
        get_jira_integration = _jira_mod.get_jira_integration
        get_jira_integration_for_tenant = _jira_mod.get_jira_integration_for_tenant
        init_jira_integration = _jira_mod.init_jira_integration
        cleanup_jira_integrations = _jira_mod.cleanup_jira_integrations
    else:
        JiraIntegration = None
        JiraAuditEntry = None
        JiraIssueType = None
        get_jira_integration = None
        get_jira_integration_for_tenant = None
        init_jira_integration = None
        cleanup_jira_integrations = None
except ImportError:
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
