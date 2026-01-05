"""
Agent Knowledge Base - Base de Conhecimento dos Agentes

Este modulo contem todo o conhecimento centralizado da Plataforma E.
Os agentes consultam este arquivo para evitar duplicacao de funcionalidades
e manter-se atualizados sobre o estado da plataforma.

Versao: 7.0
Ultima atualizacao: 2026-01-04
"""

from datetime import datetime
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from enum import Enum
import json
import os

# ==============================================================================
# CONSTANTES E CONFIGURACOES
# ==============================================================================

PLATFORM_VERSION = "7.0"
LAST_SYNC = datetime.now().isoformat()
KNOWLEDGE_FILE = os.path.join(os.path.dirname(__file__), "knowledge_data.json")


# ==============================================================================
# ENUMS E TIPOS
# ==============================================================================

class AgentType(str, Enum):
    """Tipos de agentes disponiveis."""
    ORCH = "ORCH"      # Orquestrador
    ARCH = "ARCH"      # Arquiteto
    BACK = "BACK"      # Backend
    FRONT = "FRONT"    # Frontend
    DEVOPS = "DEVOPS"  # DevOps
    SEC = "SEC"        # Security
    QA = "QA"          # Quality Assurance
    PROD = "PROD"      # Produto
    INOV = "INOV"      # Inovacao
    FIN = "FIN"        # Financeiro
    GROWTH = "GROWTH"  # Growth


class IssueStatus(str, Enum):
    """Status dos issues."""
    OPEN = "OPEN"
    CLOSED = "CLOSED"
    IN_PROGRESS = "IN_PROGRESS"


class PersonaLevel(int, Enum):
    """Niveis de hierarquia das personas."""
    SUPER_ADMIN = 0
    ADMIN = 10
    PROJECT_MANAGER = 30
    TECH_LEAD = 40
    DEVELOPER = 50
    QA_ENGINEER = 60
    API_CLIENT = 70
    STAKEHOLDER = 80
    VIEWER = 100


# ==============================================================================
# ESTRUTURA DE CONHECIMENTO DA PLATAFORMA
# ==============================================================================

PLATFORM_KNOWLEDGE: Dict[str, Any] = {
    "version": PLATFORM_VERSION,
    "last_sync": LAST_SYNC,
    "name": "Plataforma E",
    "description": "Sistema de Desenvolvimento Autonomo com Agentes IA",

    # =========================================================================
    # ARQUITETURA
    # =========================================================================
    "architecture": {
        "layers": [
            "Apresentacao (Dashboard Vue.js 3)",
            "API (FastAPI)",
            "Servicos (Core)",
            "Repositorios (DAL)",
            "Database (SQLAlchemy)"
        ],
        "patterns": ["Repository", "Factory", "Strategy", "Observer", "Singleton"],
        "stack": {
            "backend": "FastAPI + SQLAlchemy + PostgreSQL/SQLite",
            "frontend": "Vue.js 3 + Tailwind CSS",
            "ia": "Claude API (Opus/Sonnet/Haiku)",
            "queue": "Redis",
            "auth": "JWT + OAuth2 + SSO + MFA"
        }
    },

    # =========================================================================
    # MODULOS DA API (70+ endpoints)
    # =========================================================================
    "api_modules": {
        "core_routes.py": {
            "prefix": "/api/v1",
            "endpoints": [
                "GET /jobs", "POST /jobs", "GET /jobs/{id}",
                "GET /queue", "POST /queue/clear",
                "GET /workers", "POST /workers/start", "POST /workers/stop"
            ],
            "description": "Jobs, filas Redis, workers Claude"
        },
        "admin_routes.py": {
            "prefix": "/api/admin",
            "endpoints": [
                "GET /users", "POST /users", "PUT /users/{id}", "DELETE /users/{id}",
                "GET /roles", "POST /roles", "PUT /roles/{id}", "DELETE /roles/{id}"
            ],
            "description": "Gestao de usuarios e roles"
        },
        "tenant_routes.py": {
            "prefix": "/api/admin/tenants",
            "endpoints": [
                "GET /", "POST /", "GET /{id}", "PUT /{id}", "DELETE /{id}",
                "GET /{id}/members", "POST /{id}/members", "DELETE /{id}/members/{uid}",
                "GET /{id}/settings", "PUT /{id}/settings",
                "POST /{id}/invite", "GET /{id}/invites"
            ],
            "description": "Multi-tenancy, settings, membros"
        },
        "branding_routes.py": {
            "prefix": "/api/tenant",
            "endpoints": [
                "GET /{id}/branding", "PUT /{id}/branding",
                "GET /slug/{slug}/branding", "GET /by-domain/{domain}",
                "GET /{id}/branding/css"
            ],
            "description": "White-label, customizacao visual"
        },
        "analytics_routes.py": {
            "prefix": "/api/analytics",
            "endpoints": [
                "GET /productivity", "GET /velocity", "GET /burndown",
                "GET /ab-tests", "POST /ab-tests", "GET /ab-tests/{id}/results"
            ],
            "description": "Metricas, produtividade, AB tests"
        },
        "auth.py": {
            "prefix": "/api/auth",
            "endpoints": [
                "POST /login", "POST /logout", "POST /refresh",
                "POST /forgot-password", "POST /reset-password",
                "GET /me", "PUT /me"
            ],
            "description": "Autenticacao JWT"
        },
        "story_routes.py": {
            "prefix": "/api/stories",
            "endpoints": [
                "GET /", "POST /", "GET /{id}", "PUT /{id}", "DELETE /{id}",
                "PATCH /{id}/move", "GET /{id}/tasks", "POST /{id}/tasks",
                "GET /{id}/docs", "POST /{id}/docs"
            ],
            "description": "User Stories Agile"
        },
        "kanban_routes.py": {
            "prefix": "/api/kanban",
            "endpoints": [
                "GET /board", "GET /columns", "PATCH /move",
                "GET /wip-limits", "PUT /wip-limits"
            ],
            "description": "Kanban board, WIP limits"
        },
        "security_routes.py": {
            "prefix": "/api/security",
            "endpoints": [
                "GET /csrf-token", "POST /validate-csrf",
                "GET /sessions", "DELETE /sessions/{id}",
                "GET /audit-log"
            ],
            "description": "CSRF, autenticacao, autorizacao"
        },
        "chat_routes.py": {
            "prefix": "/api/chat",
            "endpoints": [
                "GET /history", "POST /message", "DELETE /history"
            ],
            "description": "Chat assistente IA"
        },
        "health_routes.py": {
            "prefix": "/health",
            "endpoints": [
                "GET /", "GET /detailed", "GET /ready", "GET /live"
            ],
            "description": "Health checks"
        },
        "audit_routes.py": {
            "prefix": "/api/audit",
            "endpoints": [
                "GET /logs", "GET /logs/{id}", "GET /stats",
                "GET /export", "GET /user/{user_id}"
            ],
            "description": "Logs de auditoria"
        },
        "tag_routes.py": {
            "prefix": "/api/tags",
            "endpoints": [
                "GET /", "POST /", "PUT /{id}", "DELETE /{id}",
                "POST /bulk", "GET /cloud"
            ],
            "description": "Sistema de tags"
        },
        "report_routes.py": {
            "prefix": "/api/reports",
            "endpoints": [
                "GET /", "POST /generate", "GET /{id}",
                "GET /{id}/download", "POST /schedule"
            ],
            "description": "Geracao de relatorios"
        },
        "cache_routes.py": {
            "prefix": "/api/cache",
            "endpoints": [
                "GET /stats", "POST /clear", "DELETE /{key}"
            ],
            "description": "Gestao de cache"
        },
        "okr_routes.py": {
            "prefix": "/api/okr",
            "endpoints": [
                "GET /objectives", "POST /objectives", "PUT /objectives/{id}",
                "GET /key-results", "POST /key-results", "PUT /key-results/{id}"
            ],
            "description": "OKR (Objectives & Key Results)"
        },
        "marketplace_routes.py": {
            "prefix": "/api/marketplace",
            "endpoints": [
                "GET /items", "GET /items/{id}", "POST /items/{id}/download",
                "GET /reviews", "POST /reviews"
            ],
            "description": "Marketplace de templates"
        },
        "webhook_routes.py": {
            "prefix": "/api/webhooks",
            "endpoints": [
                "GET /", "POST /", "PUT /{id}", "DELETE /{id}",
                "POST /{id}/test"
            ],
            "description": "Webhooks customizados"
        }
    },

    # =========================================================================
    # MODELOS DE DADOS (70+ modelos)
    # =========================================================================
    "models": {
        "projetos": ["Project", "ProjectStatus", "ProjectMember", "ProjectRole"],
        "stories": ["Story", "StoryStatus", "StoryComplexity", "StoryCategory", "StoryPriority"],
        "tasks": ["StoryTask", "StoryTaskType", "StoryTaskStatus", "TaskPriority"],
        "documentacao": ["StoryDocumentation", "DocType", "StoryDesign", "DesignType"],
        "chat": ["ChatMessage", "MessageRole", "Attachment"],
        "planejamento": ["Epic", "Sprint", "TimeEntry", "KanbanPolicy", "WipLimit"],
        "usuarios": ["User", "Role", "Permission", "UserRole", "ProjectRole"],
        "auditoria": ["AuditLog", "ActivityLog", "TenantAuditLog"],
        "multi_tenant": ["Tenant", "TenantSettings", "BrandingConfig", "TenantMember", "TenantInvite"],
        "billing": ["TenantPlan", "TenantUsageLog", "Subscription"],
        "agentes": ["Agent", "AgentStatus", "Skill", "SkillType"],
        "metricas": ["ProductivityMetric", "ProductivitySnapshot", "FlowMetric"],
        "okr": ["Objective", "KeyResult", "OKRStatus", "OKRPeriod"],
        "ab_testing": ["ABTest", "ABTestVariant", "ABTestStatus"],
        "execucao": ["ExecutionLog", "ExecutionStatus", "CodeVersion", "CodeBranch"],
        "marketplace": ["MarketplaceItem", "MarketplaceReview", "MarketplaceDownload"],
        "planejamento_agil": ["PlanningSession", "Retrospective", "RetroActionItem"]
    },

    # =========================================================================
    # SERVICOS CORE (55+ servicos)
    # =========================================================================
    "services": {
        "autonomous_loop.py": "Loop geracao -> linting -> testes -> correcao",
        "job_queue.py": "Fila Redis com workers",
        "agent_runner.py": "Executar agentes autonomos",
        "app_generator.py": "Gerar FastAPI apps testaveis",
        "story_generator.py": "Geracao de user stories com IA",
        "code_reviewer.py": "Code review automatico",
        "chatbot_builder.py": "Builder para chatbots IA",
        "audit_logger.py": "Logging centralizado de auditoria",
        "approval_workflow.py": "Fluxos de aprovacao",
        "analytics.py": "Analise de metricas",
        "capacity_planning.py": "Planejamento de capacidade",
        "tech_debt_analyzer.py": "Analise de debito tecnico",
        "ab_testing.py": "A/B testing framework",
        "marketplace.py": "Gerenciamento do marketplace",
        "okr_manager.py": "Gestao de OKRs",
        "multi_tenant.py": "Servicos multi-tenant",
        "tenant_isolation.py": "Isolamento de dados por tenant",
        "backup_manager.py": "Backup automatico",
        "version_control.py": "Controle de versao",
        "workflow_engine.py": "Motor de workflows customizados",
        "activity_logger.py": "Log de atividades do usuario",
        "model_selector.py": "Selecao automatica de modelo Claude",
        "preview_manager.py": "Preview de projetos",
        "sandbox_executor.py": "Executar codigo em sandbox",
        "docker_generator.py": "Gerar Dockerfiles",
        "secrets_manager.py": "Gestao de secrets",
        "resource_limiter.py": "Limitador de recursos",
        "pair_programming.py": "Pair programming em tempo real",
        "refactoring_engine.py": "Motor de refatoracao",
        "productivity_analytics.py": "Analytics de produtividade",
        "runtime_manager.py": "Gerenciador de runtime",
        "execution_recorder.py": "Gravar execucoes",
        "execution_replayer.py": "Replay de execucoes"
    },

    # =========================================================================
    # DASHBOARDS
    # =========================================================================
    "dashboards": {
        "agile_v6": {
            "file": "app_v6_agile.py",
            "port": 9001,
            "features": ["Stories", "Tasks", "Chat IA", "Docs", "App Testing"]
        },
        "kanban_v5": {
            "file": "app_v5_kanban.py",
            "port": 9001,
            "features": ["Kanban simples", "Drag/Drop"]
        },
        "workers_v4": {
            "file": "app.py",
            "port": 9000,
            "features": ["Fila de jobs", "Monitoramento workers"]
        }
    },

    # =========================================================================
    # PERSONAS E RBAC
    # =========================================================================
    "personas": {
        "SUPER_ADMIN": {
            "level": 0,
            "permissions": ["*:*"],
            "description": "Acesso total a todos os tenants"
        },
        "ADMIN": {
            "level": 10,
            "permissions": [
                "projects:*", "users:*", "roles:*", "tenants:*", "billing:*"
            ],
            "description": "Gestao completa do tenant"
        },
        "PROJECT_MANAGER": {
            "level": 30,
            "permissions": [
                "stories:*", "tasks:*", "epics:*", "sprints:*", "team:*"
            ],
            "description": "Gestao de projetos"
        },
        "TECH_LEAD": {
            "level": 40,
            "permissions": [
                "stories:*", "tasks:*", "code:*", "reviews:*", "architecture:*"
            ],
            "description": "Lideranca tecnica"
        },
        "DEVELOPER": {
            "level": 50,
            "permissions": [
                "tasks:read", "tasks:update", "jobs:*", "code:*", "docs:read"
            ],
            "description": "Desenvolvimento"
        },
        "QA_ENGINEER": {
            "level": 60,
            "permissions": [
                "tasks:read", "tasks:update", "tests:*", "code:read", "docs:*"
            ],
            "description": "Quality Assurance"
        },
        "API_CLIENT": {
            "level": 70,
            "permissions": [
                "api:read", "api:execute"
            ],
            "description": "Acesso via API"
        },
        "STAKEHOLDER": {
            "level": 80,
            "permissions": [
                "reports:read", "metrics:read", "dashboards:read"
            ],
            "description": "Visao executiva"
        },
        "VIEWER": {
            "level": 100,
            "permissions": [
                "*:read"
            ],
            "description": "Apenas visualizacao"
        }
    },

    # =========================================================================
    # RECURSOS E ACOES (RBAC)
    # =========================================================================
    "rbac_resources": [
        "projects", "stories", "tasks", "epics", "sprints",
        "documentation", "users", "roles", "workers", "jobs",
        "chat", "audit", "settings", "tenants"
    ],
    "rbac_actions": [
        "create", "read", "update", "delete", "execute", "assign", "manage"
    ],

    # =========================================================================
    # SISTEMA MULTI-TENANT
    # =========================================================================
    "multi_tenant": {
        "isolation": {
            "model": "TenantMixin em todos os modelos principais",
            "index": "Indice composto (tenant_id + campo) para performance",
            "filter": "Filtros automaticos em queries"
        },
        "branding": {
            "fields": [
                "logo_light", "logo_dark", "favicon",
                "primary_color", "secondary_color", "accent_color",
                "background_color", "text_color",
                "font_primary", "font_secondary",
                "footer_text", "display_name", "tagline"
            ],
            "cache_ttl": 300
        },
        "plans": ["Free", "Pro", "Enterprise"],
        "status": ["active", "suspended", "archived"]
    },

    # =========================================================================
    # INTEGRACOES CORPORATIVAS
    # =========================================================================
    "integrations": {
        "SAP": {
            "modules": ["ECC", "S/4 HANA", "CPI"],
            "features": ["RFC", "OData", "iFlows"]
        },
        "Salesforce": {
            "modules": ["Sales Cloud", "Service Cloud", "Marketing Cloud"],
            "features": ["Object analysis", "Apex", "LWC", "Flow"]
        },
        "Microsoft": {
            "modules": ["Teams", "Azure DevOps", "Graph API"],
            "features": ["Notifications", "DM", "Pipelines", "Sync"]
        },
        "BI": {
            "tools": ["Power BI", "Tableau", "Excel"],
            "features": ["Export", "Dashboards", "Reports"]
        }
    },

    # =========================================================================
    # ISSUES DO GITHUB (atualizado via sync)
    # =========================================================================
    "issues": {
        "implemented": [
            {"number": 447, "title": "Cache Manager", "points": 5, "agent": "BACK"},
            {"number": 446, "title": "Report Generator", "points": 8, "agent": "BACK"},
            {"number": 445, "title": "CLI Tool", "points": 8, "agent": "DEVOPS"},
            {"number": 444, "title": "APM - Application Performance Monitoring", "points": 13, "agent": "DEVOPS"},
            {"number": 443, "title": "Plugin System", "points": 13, "agent": "ARCH"},
            {"number": 442, "title": "Health Check Endpoints", "points": 5, "agent": "BACK"},
            {"number": 441, "title": "Sistema de Tags", "points": 5, "agent": "BACK"},
            {"number": 439, "title": "API de Auditoria", "points": 13, "agent": "SEC"},
            {"number": 438, "title": "Dashboard de Velocidade", "points": 8, "agent": "FRONT"},
            {"number": 437, "title": "Fix test fixtures", "points": 3, "agent": "QA"},
            {"number": 436, "title": "Fix hardcoded paths", "points": 2, "agent": "DEVOPS"},
            {"number": 435, "title": "Backup & Disaster Recovery", "points": 5, "agent": "DEVOPS"},
            {"number": 434, "title": "Analytics Dashboard", "points": 8, "agent": "FRONT"},
            {"number": 433, "title": "RBAC - Role-Based Access Control", "points": 13, "agent": "SEC"},
            {"number": 432, "title": "Sistema de Notificacoes", "points": 13, "agent": "BACK"},
            {"number": 431, "title": "Fix 28 testes falhando", "points": 8, "agent": "QA"}
        ],
        "pending": [
            {"number": 450, "title": "Dashboard de Performance APM", "points": 5, "agent": "FRONT"},
            {"number": 449, "title": "Vector Search para Stories", "points": 5, "agent": "INOV"},
            {"number": 448, "title": "Event Sourcing para Audit Trail", "points": 5, "agent": "INOV"},
            {"number": 440, "title": "Componente de Comentarios", "points": 8, "agent": "FRONT"}
        ]
    },

    # =========================================================================
    # STATUS DE TESTES
    # =========================================================================
    "tests": {
        "summary": {
            "passed": 408,
            "failed": 28,
            "skipped": 7,
            "errors": 8
        },
        "failing_categories": {
            "multi_tenant": {
                "count": 11,
                "issues": ["Enums ADMIN/USER/GUEST", "to_dict()", "has_permission()"]
            },
            "e2e_dashboard": {
                "count": 9,
                "issues": ["Fixtures faltando", "pytest-asyncio config"]
            },
            "hardcoded_paths": {
                "count": 4,
                "issues": ["C:\\Users\\lcruz\\Plataforma E"]
            }
        },
        "coverage_target": 80
    },

    # =========================================================================
    # IDENTIDADE VISUAL BELGO
    # =========================================================================
    "branding_defaults": {
        "primary_color": "#003B4A",
        "secondary_color": "#FF6C00",
        "success_color": "#10B981",
        "background_color": "#F3F4F6",
        "text_color": "#1F2937"
    }
}


# ==============================================================================
# CONHECIMENTO ESPECIFICO POR AGENTE
# ==============================================================================

AGENT_KNOWLEDGE: Dict[str, Dict[str, Any]] = {
    "ORCH": {
        "name": "Orquestrador",
        "role": "Tech Lead",
        "responsibilities": [
            "Coordenar os 11 agentes da equipe",
            "Distribuir tarefas baseado em especializacao",
            "Fazer code review e aprovar PRs",
            "Resolver conflitos entre agentes",
            "Garantir qualidade do codigo"
        ],
        "managed_files": [
            "factory/core/agent_supervisor.py",
            "factory/core/orchestrator.py",
            "factory/core/handoff_manager.py"
        ],
        "team": [
            "ARCH (Arquiteto)", "BACK (Backend)", "FRONT (Frontend)",
            "DEVOPS (DevOps)", "SEC (Security)", "QA (Quality)",
            "PROD (Produto)", "INOV (Inovacao)", "FIN (Financeiro)",
            "GROWTH (Growth)"
        ],
        "handoff_rules": {
            "QA": "Apos qualquer implementacao",
            "SEC": "Se envolver seguranca ou auth",
            "DEVOPS": "Para deploy ou infraestrutura"
        }
    },

    "ARCH": {
        "name": "Arquiteto",
        "role": "System Architect",
        "responsibilities": [
            "Definir arquitetura do sistema",
            "Tomar decisoes tecnicas estrategicas",
            "Estabelecer padroes de codigo",
            "Organizar estrutura de diretorios",
            "Planejar refatoracoes"
        ],
        "managed_files": [
            "factory/database/models.py",
            "docs/ARQUITETURA.md",
            "factory/config.py"
        ],
        "patterns": ["Repository", "Factory", "Strategy", "Observer"],
        "key_decisions": {
            "multi_tenant": "TenantMixin em todos os modelos",
            "soft_delete": "SoftDeleteMixin com auditoria",
            "layers": "Apresentacao -> API -> Servicos -> Repositorios -> DB"
        }
    },

    "BACK": {
        "name": "Backend",
        "role": "Backend Engineer",
        "responsibilities": [
            "Desenvolver APIs REST",
            "Implementar logica de negocio",
            "Gerenciar banco de dados",
            "Otimizar performance"
        ],
        "managed_files": [
            "factory/api/",
            "factory/core/",
            "factory/database/"
        ],
        "implemented_features": [
            "#447 Cache Manager", "#446 Report Generator",
            "#442 Health Checks", "#441 Tags", "#432 Notificacoes"
        ],
        "pending_features": [
            "#448 Event Sourcing"
        ],
        "do_not_duplicate": [
            "/api/v1/jobs", "/api/v1/stories", "/api/v1/users",
            "/api/admin/tenants", "/api/auth/login"
        ]
    },

    "FRONT": {
        "name": "Frontend",
        "role": "Frontend Engineer",
        "responsibilities": [
            "Desenvolver interfaces de usuario",
            "Criar componentes reutilizaveis",
            "Implementar responsividade",
            "Garantir acessibilidade"
        ],
        "managed_files": [
            "factory/dashboard/",
            "factory/dashboard/static/"
        ],
        "dashboards": {
            "agile_v6": {"port": 9001, "file": "app_v6_agile.py"},
            "kanban_v5": {"port": 9001, "file": "app_v5_kanban.py"},
            "workers_v4": {"port": 9000, "file": "app.py"}
        },
        "branding": {
            "primary": "#003B4A",
            "secondary": "#FF6C00"
        },
        "implemented_features": [
            "#438 Dashboard Velocidade", "#434 Analytics Dashboard"
        ],
        "pending_features": [
            "#450 APM Dashboard", "#440 Comentarios"
        ]
    },

    "DEVOPS": {
        "name": "DevOps",
        "role": "Platform Engineer",
        "responsibilities": [
            "Gerenciar infraestrutura",
            "Configurar CI/CD",
            "Containerizar aplicacoes",
            "Monitorar sistemas"
        ],
        "managed_files": [
            "docker-compose.yml",
            "k8s/",
            ".github/workflows/",
            "factory/health/",
            "factory/observability/"
        ],
        "implemented_features": [
            "#445 CLI Tool", "#444 APM", "#435 Backup & DR", "#436 Fix paths"
        ],
        "monitoring_endpoints": [
            "/health", "/health/detailed", "/health/ready", "/health/live"
        ]
    },

    "SEC": {
        "name": "Security",
        "role": "Security Engineer",
        "responsibilities": [
            "Implementar autenticacao e autorizacao",
            "Identificar vulnerabilidades",
            "Garantir compliance",
            "Realizar auditorias de seguranca"
        ],
        "managed_files": [
            "factory/auth/",
            "factory/audit/",
            "factory/middleware/security_headers.py"
        ],
        "implemented_features": [
            "#433 RBAC", "#439 Audit API"
        ],
        "security_features": {
            "auth": ["JWT", "OAuth2", "SSO", "MFA", "API Keys"],
            "headers": ["HSTS", "CSP", "X-Frame-Options", "X-Content-Type-Options"],
            "rbac": {"resources": 14, "actions": 7, "personas": 9}
        }
    },

    "QA": {
        "name": "QA",
        "role": "QA Engineer",
        "responsibilities": [
            "Escrever testes automatizados",
            "Garantir qualidade do codigo",
            "Criar documentacao tecnica",
            "Validar implementacoes"
        ],
        "managed_files": [
            "tests/",
            "tests/unit/",
            "tests/integration/"
        ],
        "test_status": {
            "passed": 408,
            "failed": 28,
            "target_coverage": 80
        },
        "failing_tests": {
            "multi_tenant": 11,
            "e2e_dashboard": 9,
            "hardcoded_paths": 4
        },
        "implemented_features": [
            "#437 Fix fixtures", "#431 Fix testes"
        ]
    },

    "PROD": {
        "name": "Produto",
        "role": "Product Manager",
        "responsibilities": [
            "Definir features e requisitos",
            "Gerenciar backlog e roadmap",
            "Escrever user stories",
            "Priorizar entregas"
        ],
        "managed_files": [
            "docs/ROADMAP.md",
            "docs/VISAO_NEGOCIOS.md"
        ],
        "backlog": {
            "implemented": 41,
            "pending": 14
        },
        "agile_artifacts": {
            "stories": "6 colunas (backlog -> done)",
            "epics": "Agrupamento de stories",
            "sprints": "Ciclos de desenvolvimento"
        },
        "okr_support": "factory/core/okr_manager.py"
    },

    "INOV": {
        "name": "Inovacao",
        "role": "R&D/Innovation",
        "responsibilities": [
            "Pesquisar novas tecnologias",
            "Criar provas de conceito",
            "Buscar projetos GitHub para incorporar",
            "Fazer benchmarks"
        ],
        "managed_files": [
            "factory/agents/specialized_agents.py",
            "factory/agents/skills/"
        ],
        "research_topics": [
            "#449 Vector Search (FAISS)",
            "#448 Event Sourcing"
        ],
        "specialized_agents": 53,
        "agent_categories": [
            "Frontend (5)", "Backend (5)", "SAP ECC (11)",
            "SAP S/4 (6)", "Salesforce (5)", "Power BI (3)",
            "Azure (5)", "Databricks (4)"
        ]
    },

    "FIN": {
        "name": "Financeiro",
        "role": "FinOps/CFO",
        "responsibilities": [
            "Analisar custos e rentabilidade",
            "Definir pricing",
            "Monitorar metricas financeiras",
            "Garantir escalabilidade economica"
        ],
        "managed_files": [
            "factory/billing/"
        ],
        "pricing_models": {
            "plans": ["Free", "Pro", "Enterprise"],
            "models": ["TenantPlan", "TenantUsageLog", "Subscription"]
        },
        "api_quotas": "API Keys com rate limiting por tier"
    },

    "GROWTH": {
        "name": "Growth",
        "role": "Go-to-Market",
        "responsibilities": [
            "Planejar lancamentos",
            "Estrategias de marketing",
            "Aquisicao e retencao de usuarios",
            "Go-to-market"
        ],
        "managed_files": [
            "factory/core/marketplace.py",
            "factory/core/analytics.py",
            "factory/core/ab_testing.py"
        ],
        "features": {
            "marketplace": "Templates e extensoes",
            "ab_testing": "Experimentos controlados",
            "analytics": "Metricas de engajamento"
        },
        "white_label": "Customizacao por cliente"
    }
}


# ==============================================================================
# HISTORICO DE APRENDIZADO
# ==============================================================================

LEARNING_HISTORY: Dict[str, List[Dict[str, Any]]] = {
    agent: [] for agent in AgentType
}


# ==============================================================================
# FUNCOES DE ACESSO
# ==============================================================================

def get_platform_knowledge() -> Dict[str, Any]:
    """Retorna conhecimento completo da plataforma."""
    return PLATFORM_KNOWLEDGE


def get_agent_knowledge(agent_id: str) -> Dict[str, Any]:
    """Retorna conhecimento especifico de um agente."""
    if agent_id not in AGENT_KNOWLEDGE:
        raise ValueError(f"Agente desconhecido: {agent_id}")
    return {
        **AGENT_KNOWLEDGE[agent_id],
        "platform": PLATFORM_KNOWLEDGE,
        "learning_history": LEARNING_HISTORY.get(agent_id, [])
    }


def get_agent_prompt_context(agent_id: str) -> str:
    """Gera contexto de prompt para um agente."""
    knowledge = get_agent_knowledge(agent_id)
    platform = knowledge["platform"]

    # Formata issues implementados relevantes
    implemented = [
        f"#{i['number']} {i['title']}"
        for i in platform["issues"]["implemented"]
        if i.get("agent") == agent_id
    ]

    # Formata issues pendentes relevantes
    pending = [
        f"#{i['number']} {i['title']}"
        for i in platform["issues"]["pending"]
        if i.get("agent") == agent_id
    ]

    return f"""
# PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## SEU PAPEL: {knowledge['name']} ({knowledge['role']})

## RESPONSABILIDADES
{chr(10).join(f"- {r}" for r in knowledge['responsibilities'])}

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## FUNCIONALIDADES JA IMPLEMENTADAS (NAO DUPLICAR)
{chr(10).join(f"- {i}" for i in implemented) if implemented else "Nenhuma especifica para este agente"}

## ISSUES PENDENTES (OPORTUNIDADES)
{chr(10).join(f"- {i}" for i in pending) if pending else "Nenhum pendente para este agente"}

## SISTEMA MULTI-TENANT
- TenantMixin em todos os modelos principais
- Isolamento de dados por tenant_id
- White-label com BrandingConfig

## PERSONAS (9 tipos)
{chr(10).join(f"- {p} (Nivel {v['level']}): {v['description']}" for p, v in platform['personas'].items())}

## STATUS DE TESTES
- Passando: {platform['tests']['summary']['passed']}
- Falhando: {platform['tests']['summary']['failed']}
- Cobertura alvo: {platform['tests']['coverage_target']}%

## REGRAS DE NAO-DUPLICACAO
- Antes de criar endpoint, verificar em factory/api/
- Antes de criar modelo, verificar em factory/database/models.py
- Antes de criar servico, verificar em factory/core/
"""


def add_learning(agent_id: str, learning: Dict[str, Any]) -> None:
    """Adiciona aprendizado ao historico de um agente."""
    if agent_id not in LEARNING_HISTORY:
        LEARNING_HISTORY[agent_id] = []

    learning["timestamp"] = datetime.now().isoformat()
    LEARNING_HISTORY[agent_id].append(learning)


def save_knowledge_to_file() -> None:
    """Salva conhecimento em arquivo JSON."""
    data = {
        "platform": PLATFORM_KNOWLEDGE,
        "agents": AGENT_KNOWLEDGE,
        "learning": LEARNING_HISTORY,
        "saved_at": datetime.now().isoformat()
    }
    with open(KNOWLEDGE_FILE, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)


def load_knowledge_from_file() -> bool:
    """Carrega conhecimento de arquivo JSON."""
    global PLATFORM_KNOWLEDGE, AGENT_KNOWLEDGE, LEARNING_HISTORY

    if not os.path.exists(KNOWLEDGE_FILE):
        return False

    try:
        with open(KNOWLEDGE_FILE, 'r', encoding='utf-8') as f:
            data = json.load(f)
            PLATFORM_KNOWLEDGE.update(data.get("platform", {}))
            AGENT_KNOWLEDGE.update(data.get("agents", {}))
            LEARNING_HISTORY.update(data.get("learning", {}))
            return True
    except Exception:
        return False


# Carrega conhecimento salvo na inicializacao
load_knowledge_from_file()
