# -*- coding: utf-8 -*-
"""
Screen Registry - Registro de telas e funcionalidades para teste.

Define todas as telas, elementos e acoes disponiveis na plataforma
para execucao de testes automatizados completos.
"""

from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum


class ElementType(str, Enum):
    """Tipos de elementos na pagina."""
    BUTTON = "button"
    INPUT = "input"
    SELECT = "select"
    CHECKBOX = "checkbox"
    RADIO = "radio"
    LINK = "link"
    TABLE = "table"
    CARD = "card"
    MODAL = "modal"
    FORM = "form"
    MENU = "menu"
    TAB = "tab"
    DRAG_DROP = "drag_drop"


class ActionType(str, Enum):
    """Tipos de acoes de teste."""
    CLICK = "click"
    TYPE = "type"
    SELECT = "select"
    CHECK = "check"
    UNCHECK = "uncheck"
    DRAG_DROP = "drag_drop"
    UPLOAD = "upload"
    DOWNLOAD = "download"
    HOVER = "hover"
    SCROLL = "scroll"
    WAIT = "wait"
    ASSERT = "assert"
    NAVIGATE = "navigate"


# =============================================================================
# SCREENS - Registro de todas as telas da plataforma
# =============================================================================

SCREENS = {
    # -------------------------------------------------------------------------
    # AUTH - Autenticacao
    # -------------------------------------------------------------------------
    "login": {
        "url": "/login",
        "title": "Login",
        "description": "Tela de autenticacao do usuario",
        "elements": {
            "email_input": {"selector": "#email", "type": ElementType.INPUT, "required": True},
            "password_input": {"selector": "#password", "type": ElementType.INPUT, "required": True},
            "login_button": {"selector": "#btn-login", "type": ElementType.BUTTON},
            "forgot_password": {"selector": "a[href*='forgot']", "type": ElementType.LINK},
            "remember_me": {"selector": "#remember-me", "type": ElementType.CHECKBOX},
            "error_message": {"selector": ".error-message", "type": ElementType.CARD}
        },
        "actions": ["login", "forgot_password", "remember_me"],
        "personas": ["all"],
        "test_cases": [
            {"name": "login_success", "priority": "critical"},
            {"name": "login_invalid_email", "priority": "high"},
            {"name": "login_invalid_password", "priority": "high"},
            {"name": "login_empty_fields", "priority": "medium"},
        ]
    },

    "forgot_password": {
        "url": "/forgot-password",
        "title": "Recuperar Senha",
        "description": "Tela de recuperacao de senha",
        "elements": {
            "email_input": {"selector": "#email", "type": ElementType.INPUT, "required": True},
            "submit_button": {"selector": "#btn-submit", "type": ElementType.BUTTON},
            "back_link": {"selector": "a[href*='login']", "type": ElementType.LINK},
            "success_message": {"selector": ".success-message", "type": ElementType.CARD}
        },
        "actions": ["submit_email", "back_to_login"],
        "personas": ["all"],
        "test_cases": [
            {"name": "recovery_valid_email", "priority": "high"},
            {"name": "recovery_invalid_email", "priority": "medium"},
        ]
    },

    # -------------------------------------------------------------------------
    # DASHBOARD - Principal
    # -------------------------------------------------------------------------
    "dashboard": {
        "url": "/",
        "title": "Dashboard",
        "description": "Dashboard principal com metricas e visao geral",
        "elements": {
            "stats_cards": {"selector": ".stats-card", "type": ElementType.CARD},
            "projects_summary": {"selector": ".projects-summary", "type": ElementType.CARD},
            "activity_feed": {"selector": ".activity-feed", "type": ElementType.CARD},
            "quick_actions": {"selector": ".quick-actions", "type": ElementType.MENU},
            "notifications": {"selector": ".notifications-bell", "type": ElementType.BUTTON},
            "user_menu": {"selector": ".user-menu", "type": ElementType.MENU}
        },
        "actions": ["view_stats", "view_projects", "quick_create", "notifications"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD", "DEVELOPER", "QA_ENGINEER"],
        "test_cases": [
            {"name": "dashboard_loads", "priority": "critical"},
            {"name": "stats_displayed", "priority": "high"},
            {"name": "quick_actions_work", "priority": "medium"},
        ]
    },

    # -------------------------------------------------------------------------
    # KANBAN - Quadro Kanban
    # -------------------------------------------------------------------------
    "kanban": {
        "url": "/kanban",
        "title": "Kanban Board",
        "description": "Quadro Kanban para gestao de stories e tasks",
        "elements": {
            "columns": {"selector": ".kanban-column", "type": ElementType.CARD},
            "story_cards": {"selector": ".story-card", "type": ElementType.CARD},
            "drag_handles": {"selector": ".drag-handle", "type": ElementType.DRAG_DROP},
            "add_story_btn": {"selector": ".add-story-btn", "type": ElementType.BUTTON},
            "filter_input": {"selector": "#kanban-filter", "type": ElementType.INPUT},
            "column_headers": {"selector": ".column-header", "type": ElementType.CARD}
        },
        "actions": ["drag_drop", "create_story", "edit_story", "filter", "view_details"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD", "DEVELOPER", "QA_ENGINEER"],
        "test_cases": [
            {"name": "kanban_loads", "priority": "critical"},
            {"name": "drag_story_between_columns", "priority": "critical"},
            {"name": "create_new_story", "priority": "high"},
            {"name": "filter_stories", "priority": "medium"},
        ]
    },

    # -------------------------------------------------------------------------
    # STORIES - User Stories
    # -------------------------------------------------------------------------
    "stories": {
        "url": "/stories",
        "title": "User Stories",
        "description": "Lista e gestao de User Stories",
        "elements": {
            "story_list": {"selector": ".story-list", "type": ElementType.TABLE},
            "story_detail": {"selector": ".story-detail", "type": ElementType.MODAL},
            "create_btn": {"selector": "#btn-create-story", "type": ElementType.BUTTON},
            "search_input": {"selector": "#search-stories", "type": ElementType.INPUT},
            "filter_status": {"selector": "#filter-status", "type": ElementType.SELECT},
            "filter_priority": {"selector": "#filter-priority", "type": ElementType.SELECT},
            "acceptance_criteria": {"selector": ".acceptance-criteria", "type": ElementType.CARD},
            "story_tasks": {"selector": ".story-tasks", "type": ElementType.TABLE}
        },
        "actions": ["create", "edit", "delete", "move_status", "add_criteria", "add_task"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD", "DEVELOPER", "QA_ENGINEER"],
        "test_cases": [
            {"name": "list_stories", "priority": "critical"},
            {"name": "create_story", "priority": "critical"},
            {"name": "edit_story", "priority": "high"},
            {"name": "delete_story", "priority": "high"},
            {"name": "filter_stories", "priority": "medium"},
        ]
    },

    "story_detail": {
        "url": "/stories/{id}",
        "title": "Detalhes da Story",
        "description": "Visualizacao e edicao detalhada de uma story",
        "elements": {
            "title": {"selector": ".story-title", "type": ElementType.INPUT},
            "persona": {"selector": "#story-persona", "type": ElementType.INPUT},
            "action": {"selector": "#story-action", "type": ElementType.INPUT},
            "benefit": {"selector": "#story-benefit", "type": ElementType.INPUT},
            "status_select": {"selector": "#story-status", "type": ElementType.SELECT},
            "priority_select": {"selector": "#story-priority", "type": ElementType.SELECT},
            "points_select": {"selector": "#story-points", "type": ElementType.SELECT},
            "acceptance_criteria": {"selector": ".criteria-list", "type": ElementType.TABLE},
            "add_criteria_btn": {"selector": "#btn-add-criteria", "type": ElementType.BUTTON},
            "tasks_section": {"selector": ".tasks-section", "type": ElementType.CARD},
            "attachments": {"selector": ".attachments", "type": ElementType.CARD},
            "comments": {"selector": ".comments", "type": ElementType.CARD}
        },
        "actions": ["edit", "save", "add_criteria", "add_task", "upload_attachment", "add_comment"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD", "DEVELOPER", "QA_ENGINEER"],
        "test_cases": [
            {"name": "view_story_details", "priority": "critical"},
            {"name": "edit_narrative", "priority": "high"},
            {"name": "add_acceptance_criteria", "priority": "high"},
            {"name": "change_status", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # PROJECTS - Projetos
    # -------------------------------------------------------------------------
    "projects": {
        "url": "/projects",
        "title": "Projetos",
        "description": "Lista e gestao de projetos",
        "elements": {
            "project_list": {"selector": ".project-list", "type": ElementType.TABLE},
            "project_cards": {"selector": ".project-card", "type": ElementType.CARD},
            "create_btn": {"selector": "#btn-create-project", "type": ElementType.BUTTON},
            "search_input": {"selector": "#search-projects", "type": ElementType.INPUT},
            "filter_status": {"selector": "#filter-status", "type": ElementType.SELECT}
        },
        "actions": ["create", "edit", "delete", "view", "archive"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD"],
        "test_cases": [
            {"name": "list_projects", "priority": "critical"},
            {"name": "create_project", "priority": "critical"},
            {"name": "edit_project", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # USERS - Gestao de Usuarios
    # -------------------------------------------------------------------------
    "users": {
        "url": "/users",
        "title": "Usuarios",
        "description": "Gestao de usuarios do sistema",
        "elements": {
            "user_list": {"selector": ".user-list", "type": ElementType.TABLE},
            "create_btn": {"selector": "#btn-create-user", "type": ElementType.BUTTON},
            "search_input": {"selector": "#search-users", "type": ElementType.INPUT},
            "filter_role": {"selector": "#filter-role", "type": ElementType.SELECT},
            "filter_status": {"selector": "#filter-status", "type": ElementType.SELECT}
        },
        "actions": ["create", "edit", "deactivate", "change_role", "reset_password"],
        "personas": ["ADMIN", "SUPER_ADMIN"],
        "test_cases": [
            {"name": "list_users", "priority": "critical"},
            {"name": "create_user", "priority": "critical"},
            {"name": "change_user_role", "priority": "high"},
            {"name": "deactivate_user", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # TENANTS - Multi-tenant (Admin)
    # -------------------------------------------------------------------------
    "tenants": {
        "url": "/admin/tenants",
        "title": "Tenants",
        "description": "Gestao de tenants (multi-tenant)",
        "elements": {
            "tenant_list": {"selector": ".tenant-list", "type": ElementType.TABLE},
            "create_btn": {"selector": "#btn-create-tenant", "type": ElementType.BUTTON},
            "branding_section": {"selector": ".branding-config", "type": ElementType.FORM},
            "logo_upload": {"selector": "#logo-upload", "type": ElementType.INPUT},
            "color_primary": {"selector": "#color-primary", "type": ElementType.INPUT},
            "color_secondary": {"selector": "#color-secondary", "type": ElementType.INPUT}
        },
        "actions": ["create", "edit", "configure_branding", "upload_logo", "deactivate"],
        "personas": ["SUPER_ADMIN"],
        "test_cases": [
            {"name": "list_tenants", "priority": "critical"},
            {"name": "create_tenant", "priority": "critical"},
            {"name": "configure_branding", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # INTEGRATIONS - Integracoes Corporativas
    # -------------------------------------------------------------------------
    "integrations": {
        "url": "/settings/integrations",
        "title": "Integracoes",
        "description": "Configuracao de integracoes corporativas",
        "elements": {
            "integration_list": {"selector": ".integration-list", "type": ElementType.TABLE},
            "sap_config": {"selector": "#sap-integration", "type": ElementType.FORM},
            "salesforce_config": {"selector": "#salesforce-integration", "type": ElementType.FORM},
            "jira_config": {"selector": "#jira-integration", "type": ElementType.FORM},
            "azure_devops_config": {"selector": "#azdo-integration", "type": ElementType.FORM},
            "teams_config": {"selector": "#teams-integration", "type": ElementType.FORM},
            "test_connection_btn": {"selector": ".btn-test-connection", "type": ElementType.BUTTON}
        },
        "actions": ["configure", "test_connection", "enable", "disable", "sync"],
        "personas": ["ADMIN", "SUPER_ADMIN"],
        "test_cases": [
            {"name": "list_integrations", "priority": "high"},
            {"name": "configure_sap", "priority": "high"},
            {"name": "configure_jira", "priority": "high"},
            {"name": "test_connection", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # REPORTS - Relatorios
    # -------------------------------------------------------------------------
    "reports": {
        "url": "/reports",
        "title": "Relatorios",
        "description": "Relatorios e metricas do projeto",
        "elements": {
            "report_types": {"selector": ".report-type-selector", "type": ElementType.TAB},
            "date_range": {"selector": "#date-range", "type": ElementType.INPUT},
            "chart_area": {"selector": ".chart-container", "type": ElementType.CARD},
            "export_btn": {"selector": "#btn-export", "type": ElementType.BUTTON},
            "filter_project": {"selector": "#filter-project", "type": ElementType.SELECT}
        },
        "actions": ["view", "filter", "export_pdf", "export_excel", "share"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "STAKEHOLDER"],
        "test_cases": [
            {"name": "view_reports", "priority": "high"},
            {"name": "export_pdf", "priority": "medium"},
            {"name": "filter_by_date", "priority": "medium"},
        ]
    },

    # -------------------------------------------------------------------------
    # WORKERS - Workers Claude (Agentes)
    # -------------------------------------------------------------------------
    "workers": {
        "url": "/workers",
        "title": "Workers",
        "description": "Gestao de workers Claude",
        "elements": {
            "worker_list": {"selector": ".worker-list", "type": ElementType.TABLE},
            "status_indicators": {"selector": ".worker-status", "type": ElementType.CARD},
            "job_queue": {"selector": ".job-queue", "type": ElementType.TABLE},
            "create_job_btn": {"selector": "#btn-create-job", "type": ElementType.BUTTON}
        },
        "actions": ["view_status", "start_job", "stop_job", "view_logs"],
        "personas": ["ADMIN", "TECH_LEAD", "DEVELOPER"],
        "test_cases": [
            {"name": "list_workers", "priority": "high"},
            {"name": "view_worker_status", "priority": "high"},
            {"name": "create_job", "priority": "high"},
        ]
    },

    # -------------------------------------------------------------------------
    # ACTIVITY MONITOR - Monitor de Atividades
    # -------------------------------------------------------------------------
    "activity_monitor": {
        "url": "/activity",
        "title": "Monitor de Atividades",
        "description": "Monitor em tempo real das atividades dos agentes",
        "elements": {
            "activity_feed": {"selector": ".activity-feed", "type": ElementType.TABLE},
            "agent_status": {"selector": ".agent-status-chips", "type": ElementType.CARD},
            "filter_agent": {"selector": "#filter-agent", "type": ElementType.SELECT},
            "filter_type": {"selector": "#filter-type", "type": ElementType.SELECT},
            "timeline": {"selector": ".activity-timeline", "type": ElementType.CARD}
        },
        "actions": ["view", "filter", "expand_details"],
        "personas": ["ADMIN", "PROJECT_MANAGER", "TECH_LEAD"],
        "test_cases": [
            {"name": "view_activities", "priority": "high"},
            {"name": "filter_by_agent", "priority": "medium"},
            {"name": "realtime_updates", "priority": "high"},
        ]
    },
}


# =============================================================================
# FEATURES - Funcionalidades para teste
# =============================================================================

FEATURES = {
    "story_crud": {
        "name": "CRUD de Stories",
        "operations": ["create", "read", "update", "delete"],
        "screens": ["stories", "story_detail", "kanban"],
        "critical": True
    },
    "kanban_drag": {
        "name": "Drag-Drop Kanban",
        "operations": ["drag_story", "change_status", "reorder"],
        "screens": ["kanban"],
        "critical": True
    },
    "sprint_management": {
        "name": "Gestao de Sprints",
        "operations": ["create_sprint", "add_stories", "close_sprint", "view_burndown"],
        "screens": ["sprints", "kanban"],
        "critical": False
    },
    "user_management": {
        "name": "Gestao de Usuarios",
        "operations": ["invite_user", "change_role", "deactivate", "reset_password"],
        "screens": ["users"],
        "critical": True
    },
    "white_label": {
        "name": "White-Label",
        "operations": ["upload_logo", "change_colors", "custom_domain", "email_templates"],
        "screens": ["tenants", "settings"],
        "critical": False
    },
    "integrations": {
        "name": "Integracoes",
        "operations": ["configure", "test_connection", "sync_data"],
        "screens": ["integrations"],
        "critical": False
    },
    "reports": {
        "name": "Relatorios",
        "operations": ["view", "filter", "export"],
        "screens": ["reports"],
        "critical": False
    },
    "authentication": {
        "name": "Autenticacao",
        "operations": ["login", "logout", "forgot_password", "change_password"],
        "screens": ["login", "forgot_password"],
        "critical": True
    }
}


# =============================================================================
# INTEGRATIONS - Configuracoes de integracoes corporativas
# =============================================================================

INTEGRATIONS = {
    "sap": {
        "name": "SAP S/4HANA",
        "type": "erp",
        "test_endpoints": [
            {"path": "/api/sap/materials", "method": "GET"},
            {"path": "/api/sap/orders", "method": "GET"},
            {"path": "/api/sap/invoices", "method": "GET"}
        ],
        "required_config": ["client_id", "client_secret", "base_url"],
        "test_scenarios": [
            "connection_test",
            "fetch_materials",
            "create_order",
            "sync_data"
        ]
    },
    "salesforce": {
        "name": "Salesforce",
        "type": "crm",
        "test_endpoints": [
            {"path": "/api/sf/accounts", "method": "GET"},
            {"path": "/api/sf/opportunities", "method": "GET"},
            {"path": "/api/sf/contacts", "method": "GET"}
        ],
        "required_config": ["client_id", "client_secret", "instance_url"],
        "test_scenarios": [
            "connection_test",
            "fetch_accounts",
            "create_opportunity",
            "sync_contacts"
        ]
    },
    "jira": {
        "name": "Jira",
        "type": "project_management",
        "test_endpoints": [
            {"path": "/api/jira/issues", "method": "GET"},
            {"path": "/api/jira/projects", "method": "GET"},
            {"path": "/api/jira/boards", "method": "GET"}
        ],
        "required_config": ["api_token", "base_url", "project_key"],
        "test_scenarios": [
            "connection_test",
            "fetch_issues",
            "create_issue",
            "sync_stories"
        ]
    },
    "azure_devops": {
        "name": "Azure DevOps",
        "type": "devops",
        "test_endpoints": [
            {"path": "/api/azdo/workitems", "method": "GET"},
            {"path": "/api/azdo/pipelines", "method": "GET"},
            {"path": "/api/azdo/repos", "method": "GET"}
        ],
        "required_config": ["pat_token", "organization", "project"],
        "test_scenarios": [
            "connection_test",
            "fetch_workitems",
            "trigger_pipeline",
            "sync_repos"
        ]
    },
    "teams": {
        "name": "Microsoft Teams",
        "type": "collaboration",
        "test_endpoints": [
            {"path": "/api/teams/messages", "method": "GET"},
            {"path": "/api/teams/channels", "method": "GET"}
        ],
        "required_config": ["tenant_id", "client_id", "client_secret"],
        "test_scenarios": [
            "connection_test",
            "send_message",
            "create_channel",
            "fetch_files"
        ]
    },
    "outlook": {
        "name": "Outlook/Exchange",
        "type": "email",
        "test_endpoints": [
            {"path": "/api/outlook/messages", "method": "GET"},
            {"path": "/api/outlook/calendar", "method": "GET"}
        ],
        "required_config": ["tenant_id", "client_id", "client_secret"],
        "test_scenarios": [
            "connection_test",
            "fetch_emails",
            "send_email",
            "sync_calendar"
        ]
    }
}


# =============================================================================
# DOCUMENT_TYPES - Tipos de documentos para teste
# =============================================================================

DOCUMENT_TYPES = {
    "office": {
        "name": "Microsoft Office",
        "formats": ["docx", "xlsx", "pptx"],
        "test_operations": ["upload", "parse", "extract_text", "analyze"],
        "sample_files": [
            "sample_contract.docx",
            "sample_report.xlsx",
            "sample_presentation.pptx"
        ]
    },
    "pdf": {
        "name": "PDF Documents",
        "formats": ["pdf"],
        "test_operations": ["upload", "parse", "extract_text", "ocr", "analyze"],
        "sample_files": [
            "sample_invoice.pdf",
            "sample_manual.pdf"
        ]
    },
    "teams": {
        "name": "Microsoft Teams",
        "formats": ["teams_chat", "teams_file"],
        "test_operations": ["fetch", "parse", "analyze_conversation"],
        "sample_files": [
            "sample_chat_export.json"
        ]
    },
    "outlook": {
        "name": "Outlook Messages",
        "formats": ["msg", "eml"],
        "test_operations": ["upload", "parse", "extract_attachments", "analyze"],
        "sample_files": [
            "sample_email.msg",
            "sample_email.eml"
        ]
    },
    "whatsapp": {
        "name": "WhatsApp Export",
        "formats": ["txt", "json"],
        "test_operations": ["upload", "parse", "analyze_conversation", "extract_media"],
        "sample_files": [
            "whatsapp_chat_export.txt"
        ]
    }
}


@dataclass
class ScreenInfo:
    """Informacoes completas de uma tela."""
    name: str
    url: str
    title: str
    description: str
    elements: Dict[str, Dict]
    actions: List[str]
    personas: List[str]
    test_cases: List[Dict]


class ScreenRegistry:
    """
    Registro central de telas e funcionalidades.

    Permite consultar telas, elementos e acoes para testes.
    """

    def __init__(self):
        self.screens = SCREENS
        self.features = FEATURES
        self.integrations = INTEGRATIONS
        self.document_types = DOCUMENT_TYPES

    def get_screen(self, screen_name: str) -> Optional[ScreenInfo]:
        """
        Retorna informacoes de uma tela.

        Args:
            screen_name: Nome da tela

        Returns:
            ScreenInfo ou None
        """
        screen_data = self.screens.get(screen_name)
        if not screen_data:
            return None

        return ScreenInfo(
            name=screen_name,
            url=screen_data["url"],
            title=screen_data["title"],
            description=screen_data["description"],
            elements=screen_data.get("elements", {}),
            actions=screen_data.get("actions", []),
            personas=screen_data.get("personas", []),
            test_cases=screen_data.get("test_cases", [])
        )

    def get_all_screens(self) -> List[str]:
        """Retorna lista de todas as telas."""
        return list(self.screens.keys())

    def get_screens_for_persona(self, persona: str) -> List[str]:
        """
        Retorna telas acessiveis por uma persona.

        Args:
            persona: Tipo da persona

        Returns:
            Lista de nomes de telas
        """
        screens = []
        for name, data in self.screens.items():
            personas = data.get("personas", [])
            if "all" in personas or persona in personas:
                screens.append(name)
        return screens

    def get_critical_screens(self) -> List[str]:
        """Retorna telas com testes criticos."""
        critical = []
        for name, data in self.screens.items():
            test_cases = data.get("test_cases", [])
            has_critical = any(tc.get("priority") == "critical" for tc in test_cases)
            if has_critical:
                critical.append(name)
        return critical

    def get_test_cases_for_screen(self, screen_name: str) -> List[Dict]:
        """
        Retorna casos de teste de uma tela.

        Args:
            screen_name: Nome da tela

        Returns:
            Lista de casos de teste
        """
        screen_data = self.screens.get(screen_name, {})
        return screen_data.get("test_cases", [])

    def get_all_test_cases(self) -> List[Dict]:
        """Retorna todos os casos de teste."""
        all_cases = []
        for screen_name, data in self.screens.items():
            for test_case in data.get("test_cases", []):
                all_cases.append({
                    "screen": screen_name,
                    **test_case
                })
        return all_cases

    def get_feature(self, feature_name: str) -> Optional[Dict]:
        """
        Retorna informacoes de uma feature.

        Args:
            feature_name: Nome da feature

        Returns:
            Dict com informacoes ou None
        """
        return self.features.get(feature_name)

    def get_critical_features(self) -> List[str]:
        """Retorna features criticas."""
        return [name for name, data in self.features.items() if data.get("critical")]

    def get_integration(self, integration_name: str) -> Optional[Dict]:
        """
        Retorna configuracao de uma integracao.

        Args:
            integration_name: Nome da integracao

        Returns:
            Dict com configuracao ou None
        """
        return self.integrations.get(integration_name)

    def get_all_integrations(self) -> List[str]:
        """Retorna lista de todas as integracoes."""
        return list(self.integrations.keys())


# Instancia global
_screen_registry: Optional[ScreenRegistry] = None


def get_screen_registry() -> ScreenRegistry:
    """Retorna instancia global do ScreenRegistry."""
    global _screen_registry
    if _screen_registry is None:
        _screen_registry = ScreenRegistry()
    return _screen_registry


if __name__ == "__main__":
    # Demo: Listar telas e funcionalidades
    registry = ScreenRegistry()

    print("\n=== DEMO: Screen Registry ===\n")

    print(f"Total de telas: {len(registry.get_all_screens())}")
    print(f"Telas criticas: {len(registry.get_critical_screens())}")
    print(f"Features criticas: {registry.get_critical_features()}")
    print(f"Integracoes: {registry.get_all_integrations()}")

    print("\nTelas disponiveis:")
    for screen in registry.get_all_screens():
        info = registry.get_screen(screen)
        print(f"  - {screen}: {info.title} ({len(info.test_cases)} testes)")

    print("\nTelas por persona (DEVELOPER):")
    for screen in registry.get_screens_for_persona("DEVELOPER"):
        print(f"  - {screen}")
