# -*- coding: utf-8 -*-
"""
Constantes de Lookup - Plataforma E
===================================

Valores de fallback usados quando o banco de dados esta indisponivel
ou quando as tabelas de lookup ainda nao foram populadas.

IMPORTANTE: Estes valores devem ser identicos aos populados nas
tabelas de lookup do banco de dados. Sao usados apenas como fallback.

Author: Plataforma E
"""

from typing import Dict, List, Any


# =============================================================================
# FIBONACCI POINTS - Valores validos de Story Points
# =============================================================================

FIBONACCI_POINTS: List[int] = [0, 1, 2, 3, 5, 8, 13, 21]

# Labels opcionais para cada valor
FIBONACCI_LABELS: Dict[int, str] = {
    0: "0 - Sem esforco",
    1: "1 - Minimo",
    2: "2 - Pequeno",
    3: "3 - Medio-Pequeno",
    5: "5 - Medio",
    8: "8 - Grande",
    13: "13 - Muito Grande",
    21: "21 - Enorme"
}


# =============================================================================
# STORY STATUS - Status de User Stories
# =============================================================================

STORY_STATUS_CODES: List[str] = [
    "backlog",
    "ready",
    "in_progress",
    "review",
    "testing",
    "done"
]

STORY_STATUS: List[Dict[str, Any]] = [
    {
        "code": "backlog",
        "label": "Backlog",
        "label_en": "Backlog",
        "color": "#6B7280",
        "sort_order": 1,
        "is_initial": True,
        "is_final": False
    },
    {
        "code": "ready",
        "label": "Pronto",
        "label_en": "Ready",
        "color": "#3B82F6",
        "sort_order": 2,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "in_progress",
        "label": "Em Desenvolvimento",
        "label_en": "In Progress",
        "color": "#F59E0B",
        "sort_order": 3,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "review",
        "label": "Em Revisao",
        "label_en": "In Review",
        "color": "#8B5CF6",
        "sort_order": 4,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "testing",
        "label": "Em Testes",
        "label_en": "Testing",
        "color": "#EC4899",
        "sort_order": 5,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "done",
        "label": "Concluido",
        "label_en": "Done",
        "color": "#10B981",
        "sort_order": 6,
        "is_initial": False,
        "is_final": True
    }
]


# =============================================================================
# TASK STATUS - Status de Tasks
# =============================================================================

TASK_STATUS_CODES: List[str] = [
    "pending",
    "in_progress",
    "completed",
    "blocked",
    "cancelled"
]

TASK_STATUS: List[Dict[str, Any]] = [
    {
        "code": "pending",
        "label": "Pendente",
        "label_en": "Pending",
        "color": "#6B7280",
        "sort_order": 1,
        "is_initial": True,
        "is_final": False
    },
    {
        "code": "in_progress",
        "label": "Em Progresso",
        "label_en": "In Progress",
        "color": "#F59E0B",
        "sort_order": 2,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "completed",
        "label": "Completo",
        "label_en": "Completed",
        "color": "#10B981",
        "sort_order": 3,
        "is_initial": False,
        "is_final": True
    },
    {
        "code": "blocked",
        "label": "Bloqueado",
        "label_en": "Blocked",
        "color": "#EF4444",
        "sort_order": 4,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "cancelled",
        "label": "Cancelado",
        "label_en": "Cancelled",
        "color": "#9CA3AF",
        "sort_order": 5,
        "is_initial": False,
        "is_final": True
    }
]


# =============================================================================
# PROJECT STATUS - Status de Projetos
# =============================================================================

PROJECT_STATUS_CODES: List[str] = [
    "planning",
    "in_progress",
    "paused",
    "completed",
    "archived"
]

PROJECT_STATUS: List[Dict[str, Any]] = [
    {
        "code": "planning",
        "label": "Planejamento",
        "label_en": "Planning",
        "color": "#3B82F6",
        "sort_order": 1,
        "is_initial": True,
        "is_final": False
    },
    {
        "code": "in_progress",
        "label": "Em Progresso",
        "label_en": "In Progress",
        "color": "#F59E0B",
        "sort_order": 2,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "paused",
        "label": "Pausado",
        "label_en": "Paused",
        "color": "#9CA3AF",
        "sort_order": 3,
        "is_initial": False,
        "is_final": False
    },
    {
        "code": "completed",
        "label": "Completo",
        "label_en": "Completed",
        "color": "#10B981",
        "sort_order": 4,
        "is_initial": False,
        "is_final": True
    },
    {
        "code": "archived",
        "label": "Arquivado",
        "label_en": "Archived",
        "color": "#6B7280",
        "sort_order": 5,
        "is_initial": False,
        "is_final": True
    }
]


# =============================================================================
# PRIORITIES - Prioridades
# =============================================================================

PRIORITY_CODES: List[str] = ["low", "medium", "high", "urgent"]

PRIORITIES: List[Dict[str, Any]] = [
    {
        "code": "low",
        "label": "Baixa",
        "label_en": "Low",
        "color": "#10B981",
        "numeric_value": 3,
        "sort_order": 1,
        "is_default": False
    },
    {
        "code": "medium",
        "label": "Media",
        "label_en": "Medium",
        "color": "#F59E0B",
        "numeric_value": 5,
        "sort_order": 2,
        "is_default": True
    },
    {
        "code": "high",
        "label": "Alta",
        "label_en": "High",
        "color": "#EF4444",
        "numeric_value": 7,
        "sort_order": 3,
        "is_default": False
    },
    {
        "code": "urgent",
        "label": "Urgente",
        "label_en": "Urgent",
        "color": "#DC2626",
        "numeric_value": 9,
        "sort_order": 4,
        "is_default": False
    }
]


# =============================================================================
# COMPLEXITY - Complexidade
# =============================================================================

COMPLEXITY_CODES: List[str] = ["low", "medium", "high", "very_high"]

COMPLEXITY: List[Dict[str, Any]] = [
    {
        "code": "low",
        "label": "Baixa",
        "label_en": "Low",
        "color": "#10B981",
        "min_points": 0,
        "max_points": 3,
        "estimated_hours_min": 1,
        "estimated_hours_max": 4,
        "sort_order": 1
    },
    {
        "code": "medium",
        "label": "Media",
        "label_en": "Medium",
        "color": "#F59E0B",
        "min_points": 4,
        "max_points": 5,
        "estimated_hours_min": 4,
        "estimated_hours_max": 8,
        "sort_order": 2
    },
    {
        "code": "high",
        "label": "Alta",
        "label_en": "High",
        "color": "#EF4444",
        "min_points": 6,
        "max_points": 8,
        "estimated_hours_min": 8,
        "estimated_hours_max": 16,
        "sort_order": 3
    },
    {
        "code": "very_high",
        "label": "Muito Alta",
        "label_en": "Very High",
        "color": "#DC2626",
        "min_points": 9,
        "max_points": 21,
        "estimated_hours_min": 16,
        "estimated_hours_max": 40,
        "sort_order": 4
    }
]


# =============================================================================
# TASK TYPES - Tipos de Task
# =============================================================================

TASK_TYPE_CODES: List[str] = [
    "development",
    "review",
    "test",
    "documentation",
    "design",
    "research",
    "bug_fix",
    "deployment"
]

TASK_TYPES: List[Dict[str, Any]] = [
    {
        "code": "development",
        "label": "Desenvolvimento",
        "label_en": "Development",
        "color": "#3B82F6",
        "icon": "code",
        "sort_order": 1,
        "is_default": True
    },
    {
        "code": "review",
        "label": "Revisao de Codigo",
        "label_en": "Code Review",
        "color": "#8B5CF6",
        "icon": "eye",
        "sort_order": 2,
        "is_default": False
    },
    {
        "code": "test",
        "label": "Testes",
        "label_en": "Testing",
        "color": "#EC4899",
        "icon": "check-circle",
        "sort_order": 3,
        "is_default": False
    },
    {
        "code": "documentation",
        "label": "Documentacao",
        "label_en": "Documentation",
        "color": "#6B7280",
        "icon": "document-text",
        "sort_order": 4,
        "is_default": False
    },
    {
        "code": "design",
        "label": "Design",
        "label_en": "Design",
        "color": "#F59E0B",
        "icon": "color-swatch",
        "sort_order": 5,
        "is_default": False
    },
    {
        "code": "research",
        "label": "Pesquisa",
        "label_en": "Research",
        "color": "#10B981",
        "icon": "search",
        "sort_order": 6,
        "is_default": False
    },
    {
        "code": "bug_fix",
        "label": "Correcao de Bug",
        "label_en": "Bug Fix",
        "color": "#EF4444",
        "icon": "bug",
        "sort_order": 7,
        "is_default": False
    },
    {
        "code": "deployment",
        "label": "Deploy",
        "label_en": "Deployment",
        "color": "#14B8A6",
        "icon": "cloud-upload",
        "sort_order": 8,
        "is_default": False
    }
]


# =============================================================================
# USER ROLES - Papeis de Usuario
# =============================================================================

ROLE_CODES: List[str] = [
    "admin",
    "super_admin",
    "developer",
    "manager",
    "viewer"
]

ROLES: List[Dict[str, Any]] = [
    {
        "code": "super_admin",
        "label": "Super Admin",
        "label_en": "Super Admin",
        "permissions": ["all"],
        "is_system": True,
        "sort_order": 1
    },
    {
        "code": "admin",
        "label": "Administrador",
        "label_en": "Administrator",
        "permissions": ["read", "write", "delete", "manage_users", "manage_tenants"],
        "is_system": True,
        "sort_order": 2
    },
    {
        "code": "manager",
        "label": "Gerente",
        "label_en": "Manager",
        "permissions": ["read", "write", "delete", "manage_team"],
        "is_system": True,
        "sort_order": 3
    },
    {
        "code": "developer",
        "label": "Desenvolvedor",
        "label_en": "Developer",
        "permissions": ["read", "write"],
        "is_system": True,
        "sort_order": 4
    },
    {
        "code": "viewer",
        "label": "Visualizador",
        "label_en": "Viewer",
        "permissions": ["read"],
        "is_system": True,
        "sort_order": 5
    }
]


# =============================================================================
# WIP LIMITS - Limites Work In Progress
# =============================================================================

WIP_LIMITS: Dict[str, Dict[str, int]] = {
    "story": {
        "in_progress": 3,
        "review": 2,
        "testing": 2
    },
    "task": {
        "in_progress": 5,
        "review": 3
    }
}

DEFAULT_WIP_LIMIT: int = 5


# =============================================================================
# AGENT SKILLS - Habilidades dos Agentes
# =============================================================================

AGENT_SKILLS: Dict[str, List[str]] = {
    "SEC": [
        "security", "auth", "login", "jwt", "csrf", "xss",
        "cors", "permission", "rbac", "vulnerability", "encryption",
        "mfa", "2fa", "token", "password", "oauth"
    ],
    "DEVOPS": [
        "docker", "kubernetes", "k8s", "ci/cd", "deploy",
        "infra", "monitoring", "terraform", "helm", "redis",
        "postgres", "health check", "migration", "scaling"
    ],
    "FRONT": [
        "ui", "ux", "frontend", "component", "mobile",
        "pwa", "css", "theme", "dark mode", "responsive",
        "animation", "modal", "button", "form"
    ],
    "QA": [
        "test", "pytest", "coverage", "validar", "testar",
        "e2e", "unit test", "quality"
    ],
    "ARCH": [
        "architect", "design", "pattern", "refactor",
        "structure", "reorganiz"
    ],
    "PROD": [
        "feature", "user story", "roadmap", "backlog",
        "requirement", "acceptance"
    ],
    "INOV": [
        "research", "poc", "experiment", "ml", "ai",
        "nlp", "benchmark", "innovation"
    ],
    "FIN": [
        "cost", "pricing", "billing", "subscription",
        "revenue", "financial", "metering"
    ],
    "GROWTH": [
        "marketing", "launch", "go-to-market", "sales",
        "acquisition", "retention"
    ],
    "ORCH": [
        "coordinate", "review", "validate", "approve"
    ],
    "BACK": [
        "api", "backend", "database", "endpoint", "rest",
        "graphql", "sql", "model", "service"
    ]
}

DEFAULT_AGENT: str = "BACK"


# =============================================================================
# SYSTEM CONFIG DEFAULTS - Configuracoes Padrao do Sistema
# =============================================================================

SYSTEM_CONFIG_DEFAULTS: Dict[str, Dict[str, Any]] = {
    "kanban.wip_limit_in_progress": {
        "value": "3",
        "data_type": "number",
        "category": "kanban",
        "description": "Limite WIP para coluna In Progress"
    },
    "kanban.wip_limit_review": {
        "value": "2",
        "data_type": "number",
        "category": "kanban",
        "description": "Limite WIP para coluna Review"
    },
    "story.default_points": {
        "value": "5",
        "data_type": "number",
        "category": "story",
        "description": "Story points padrao para novas stories"
    },
    "story.default_priority": {
        "value": "medium",
        "data_type": "string",
        "category": "story",
        "description": "Prioridade padrao para novas stories"
    },
    "notification.enable_websocket": {
        "value": "true",
        "data_type": "boolean",
        "category": "notification",
        "description": "Habilitar notificacoes via WebSocket"
    },
    "notification.toast_duration_ms": {
        "value": "5000",
        "data_type": "number",
        "category": "notification",
        "description": "Duracao do toast de notificacao em milissegundos"
    },
    "orchestrator.poll_interval_seconds": {
        "value": "30",
        "data_type": "number",
        "category": "orchestrator",
        "description": "Intervalo de polling do orquestrador em segundos"
    },
    "orchestrator.max_concurrent_agents": {
        "value": "5",
        "data_type": "number",
        "category": "orchestrator",
        "description": "Maximo de agentes rodando simultaneamente"
    }
}


# =============================================================================
# BELGO COLORS - Identidade Visual Belgo
# =============================================================================

BELGO_COLORS: Dict[str, str] = {
    "primary": "#003B4A",       # Azul Belgo
    "secondary": "#FF6C00",     # Laranja Belgo
    "success": "#10B981",       # Verde
    "warning": "#F59E0B",       # Amarelo
    "error": "#EF4444",         # Vermelho
    "info": "#3B82F6",          # Azul Info
    "background": "#F3F4F6",    # Cinza Claro
    "surface": "#FFFFFF",       # Branco
    "text_primary": "#1F2937",  # Cinza Escuro
    "text_secondary": "#6B7280" # Cinza Medio
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_status_by_code(entity_type: str, code: str) -> Dict[str, Any]:
    """Retorna status pelo codigo."""
    status_map = {
        "story": STORY_STATUS,
        "task": TASK_STATUS,
        "project": PROJECT_STATUS
    }

    status_list = status_map.get(entity_type, [])
    for status in status_list:
        if status["code"] == code:
            return status
    return {}


def get_priority_by_code(code: str) -> Dict[str, Any]:
    """Retorna prioridade pelo codigo."""
    for priority in PRIORITIES:
        if priority["code"] == code:
            return priority
    return {}


def get_complexity_by_points(points: int) -> Dict[str, Any]:
    """Retorna complexidade baseada nos story points."""
    for complexity in COMPLEXITY:
        if complexity["min_points"] <= points <= complexity["max_points"]:
            return complexity
    return COMPLEXITY[-1]  # Retorna very_high como fallback


def get_agent_for_keyword(keyword: str) -> str:
    """Retorna o agente responsavel por uma keyword."""
    keyword = keyword.lower()
    for agent, keywords in AGENT_SKILLS.items():
        if any(k in keyword for k in keywords):
            return agent
    return DEFAULT_AGENT


def validate_fibonacci_points(points: int) -> bool:
    """Valida se um valor de story points e valido (Fibonacci)."""
    return points in FIBONACCI_POINTS
