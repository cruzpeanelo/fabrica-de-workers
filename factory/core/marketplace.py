# -*- coding: utf-8 -*-
"""
Marketplace de Templates e Skills - Fabrica de Agentes
========================================================

Hub centralizado para compartilhar e reutilizar:
- Templates de Projetos (Starter Kits, Arquiteturas, Dominios)
- Templates de Stories (Features comuns, Pacotes pre-definidos)
- Skills Customizados (Comunidade, Versionamento)
- Configuracoes de Agentes (Perfis especializados, Prompts otimizados)

Versao: 1.0.0
"""

import uuid
import json
from datetime import datetime, timezone
from typing import List, Dict, Optional, Any
from dataclasses import dataclass, field, asdict
from enum import Enum


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class MarketplaceCategory(str, Enum):
    """Categorias do Marketplace"""
    PROJECT_TEMPLATE = "project_template"
    STORY_TEMPLATE = "story_template"
    SKILL = "skill"
    AGENT_CONFIG = "agent_config"


class TemplateType(str, Enum):
    """Tipos de templates de projeto"""
    STARTER_KIT = "starter_kit"
    ARCHITECTURE = "architecture"
    DOMAIN = "domain"


class StoryPackType(str, Enum):
    """Tipos de pacotes de stories"""
    AUTH = "auth"
    CRUD = "crud"
    DASHBOARD = "dashboard"
    REPORTS = "reports"
    INTEGRATIONS = "integrations"
    NOTIFICATIONS = "notifications"
    PAYMENTS = "payments"
    ANALYTICS = "analytics"


class SkillCategory(str, Enum):
    """Categorias de skills"""
    CODE_GENERATION = "code_generation"
    TESTING = "testing"
    DOCUMENTATION = "documentation"
    DESIGN = "design"
    DEVOPS = "devops"
    DATABASE = "database"
    API = "api"
    SECURITY = "security"


# =============================================================================
# MODELOS DE DADOS
# =============================================================================

@dataclass
class MarketplaceItem:
    """Item base do Marketplace"""
    item_id: str
    name: str
    description: str
    category: str
    author: str
    version: str
    tags: List[str] = field(default_factory=list)

    # Metricas
    downloads: int = 0
    rating: float = 0.0
    reviews_count: int = 0

    # Status
    verified: bool = False
    featured: bool = False
    published: bool = True

    # Timestamps
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    updated_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    # Imagem/thumbnail
    thumbnail: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class ProjectTemplate(MarketplaceItem):
    """Template de projeto completo"""
    template_type: str = TemplateType.STARTER_KIT.value

    # Stack tecnologico
    tech_stack: Dict[str, str] = field(default_factory=dict)  # {"frontend": "React", "backend": "FastAPI", ...}

    # Estrutura de pastas
    folder_structure: List[str] = field(default_factory=list)

    # Arquivos iniciais (path -> conteudo ou template)
    initial_files: Dict[str, str] = field(default_factory=dict)

    # Dependencias
    dependencies: Dict[str, List[str]] = field(default_factory=dict)  # {"npm": ["react", ...], "pip": [...]}

    # Configuracoes recomendadas
    recommended_config: Dict[str, Any] = field(default_factory=dict)

    # Stories iniciais incluidas
    included_stories: List[str] = field(default_factory=list)  # IDs de story templates


@dataclass
class StoryTemplate(MarketplaceItem):
    """Template de User Story reutilizavel"""
    pack_type: str = StoryPackType.CRUD.value

    # Narrativa
    title_template: str = ""
    persona: str = ""
    action: str = ""
    benefit: str = ""

    # Criterios
    acceptance_criteria: List[str] = field(default_factory=list)
    definition_of_done: List[str] = field(default_factory=list)
    business_rules: List[str] = field(default_factory=list)

    # Configuracoes
    default_points: int = 3
    default_complexity: str = "medium"
    default_priority: str = "medium"
    suggested_category: str = "feature"

    # Tasks padrao
    default_tasks: List[Dict[str, Any]] = field(default_factory=list)

    # Variaveis para customizacao (ex: {entity_name}, {field_list})
    variables: List[str] = field(default_factory=list)


@dataclass
class Skill(MarketplaceItem):
    """Skill reutilizavel para agentes"""
    skill_category: str = SkillCategory.CODE_GENERATION.value

    # Definicao do skill
    prompt_template: str = ""
    system_instructions: str = ""

    # Parametros de entrada
    input_params: List[Dict[str, Any]] = field(default_factory=list)  # [{"name": "...", "type": "...", "required": True}]

    # Exemplos de uso
    examples: List[Dict[str, str]] = field(default_factory=list)  # [{"input": "...", "output": "..."}]

    # Compatibilidade
    compatible_agents: List[str] = field(default_factory=list)  # IDs de agentes compativeis
    required_tools: List[str] = field(default_factory=list)  # MCP tools necessarias

    # Versionamento
    changelog: List[Dict[str, str]] = field(default_factory=list)  # [{"version": "1.0", "changes": "..."}]


@dataclass
class AgentConfig(MarketplaceItem):
    """Configuracao de agente especializado"""
    agent_type: str = ""  # "backend_dev", "frontend_dev", "qa", etc

    # Prompt base
    system_prompt: str = ""

    # Instrucoes especificas
    instructions: List[str] = field(default_factory=list)

    # Skills atrelados
    skills: List[str] = field(default_factory=list)  # IDs de skills

    # Ferramentas MCP
    mcp_tools: List[str] = field(default_factory=list)

    # Configuracoes do modelo
    model_config: Dict[str, Any] = field(default_factory=lambda: {
        "model": "claude-sonnet-4-20250514",
        "max_tokens": 4096,
        "temperature": 0.7
    })

    # Especialidades
    specialties: List[str] = field(default_factory=list)


@dataclass
class Review:
    """Review/avaliacao de um item do marketplace"""
    review_id: str
    item_id: str
    user_id: str
    rating: int  # 1-5
    comment: str
    helpful_count: int = 0
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


# =============================================================================
# CATALOGO DE TEMPLATES PRE-DEFINIDOS
# =============================================================================

# Templates de Projetos
PROJECT_TEMPLATES_CATALOG: Dict[str, ProjectTemplate] = {
    "saas-react-fastapi": ProjectTemplate(
        item_id="PROJ-TPL-001",
        name="SaaS Starter Kit - React + FastAPI",
        description="Template completo para aplicacoes SaaS com React no frontend, FastAPI no backend, PostgreSQL como banco de dados e autenticacao JWT integrada.",
        category=MarketplaceCategory.PROJECT_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["saas", "react", "fastapi", "postgresql", "jwt", "docker"],
        downloads=1234,
        rating=4.8,
        reviews_count=156,
        verified=True,
        featured=True,
        template_type=TemplateType.STARTER_KIT.value,
        tech_stack={
            "frontend": "React 18 + TypeScript",
            "backend": "FastAPI + Python 3.11",
            "database": "PostgreSQL 15",
            "cache": "Redis",
            "auth": "JWT + OAuth2"
        },
        folder_structure=[
            "frontend/",
            "frontend/src/",
            "frontend/src/components/",
            "frontend/src/pages/",
            "frontend/src/hooks/",
            "frontend/src/services/",
            "backend/",
            "backend/app/",
            "backend/app/api/",
            "backend/app/models/",
            "backend/app/services/",
            "docker/",
            "docs/"
        ],
        dependencies={
            "npm": ["react", "react-router-dom", "axios", "tailwindcss", "@tanstack/react-query"],
            "pip": ["fastapi", "uvicorn", "sqlalchemy", "pydantic", "python-jose", "passlib"]
        },
        recommended_config={
            "deploy": "docker-compose",
            "ci_cd": "github-actions",
            "monitoring": "prometheus + grafana"
        },
        included_stories=["STORY-TPL-001", "STORY-TPL-002", "STORY-TPL-003"]
    ),
    "vue-django-ecommerce": ProjectTemplate(
        item_id="PROJ-TPL-002",
        name="E-commerce Kit - Vue + Django",
        description="Plataforma de e-commerce completa com Vue.js 3, Django REST Framework, sistema de pagamentos e gestao de produtos.",
        category=MarketplaceCategory.PROJECT_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["ecommerce", "vue", "django", "payments", "products"],
        downloads=856,
        rating=4.6,
        reviews_count=98,
        verified=True,
        featured=True,
        template_type=TemplateType.DOMAIN.value,
        tech_stack={
            "frontend": "Vue.js 3 + Pinia",
            "backend": "Django 5 + DRF",
            "database": "PostgreSQL",
            "payments": "Stripe",
            "storage": "S3"
        },
        folder_structure=[
            "frontend/",
            "backend/",
            "backend/products/",
            "backend/orders/",
            "backend/payments/",
            "backend/users/"
        ],
        dependencies={
            "npm": ["vue", "pinia", "vue-router", "axios"],
            "pip": ["django", "djangorestframework", "stripe", "boto3"]
        }
    ),
    "microservices-python": ProjectTemplate(
        item_id="PROJ-TPL-003",
        name="Microservices Architecture - Python",
        description="Arquitetura de microservicos com FastAPI, RabbitMQ para mensageria, API Gateway e service discovery.",
        category=MarketplaceCategory.PROJECT_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["microservices", "rabbitmq", "docker", "kubernetes", "api-gateway"],
        downloads=567,
        rating=4.7,
        reviews_count=67,
        verified=True,
        template_type=TemplateType.ARCHITECTURE.value,
        tech_stack={
            "services": "FastAPI",
            "messaging": "RabbitMQ",
            "gateway": "Kong",
            "discovery": "Consul",
            "container": "Docker + Kubernetes"
        },
        folder_structure=[
            "gateway/",
            "services/",
            "services/user-service/",
            "services/order-service/",
            "services/notification-service/",
            "infrastructure/",
            "infrastructure/docker/",
            "infrastructure/k8s/"
        ],
        dependencies={
            "pip": ["fastapi", "pika", "consul", "uvicorn"]
        }
    ),
    "mobile-react-native": ProjectTemplate(
        item_id="PROJ-TPL-004",
        name="Mobile App - React Native + Expo",
        description="Aplicativo mobile multiplataforma com React Native, Expo, navegacao e integracao com API REST.",
        category=MarketplaceCategory.PROJECT_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["mobile", "react-native", "expo", "ios", "android"],
        downloads=789,
        rating=4.5,
        reviews_count=89,
        verified=True,
        template_type=TemplateType.STARTER_KIT.value,
        tech_stack={
            "framework": "React Native + Expo",
            "navigation": "React Navigation",
            "state": "Zustand",
            "api": "Axios + React Query"
        },
        folder_structure=[
            "src/",
            "src/screens/",
            "src/components/",
            "src/navigation/",
            "src/services/",
            "src/hooks/",
            "assets/"
        ],
        dependencies={
            "npm": ["expo", "react-native", "@react-navigation/native", "zustand", "axios"]
        }
    )
}

# Templates de Stories
STORY_TEMPLATES_CATALOG: Dict[str, StoryTemplate] = {
    "auth-login": StoryTemplate(
        item_id="STORY-TPL-001",
        name="Autenticacao - Login",
        description="Template completo para implementar login de usuarios com email/senha, validacao e tratamento de erros.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["auth", "login", "security"],
        downloads=2156,
        rating=4.9,
        reviews_count=234,
        verified=True,
        featured=True,
        pack_type=StoryPackType.AUTH.value,
        title_template="[Auth] Implementar login com {auth_method}",
        persona="usuario do sistema",
        action="fazer login com meu email e senha",
        benefit="acesse minhas informacoes de forma segura",
        acceptance_criteria=[
            "Usuario pode fazer login com email e senha validos",
            "Senha deve ter minimo 8 caracteres",
            "Mensagem de erro clara para credenciais invalidas",
            "Token JWT gerado apos login bem-sucedido",
            "Redirecionamento para pagina principal apos login"
        ],
        definition_of_done=[
            "Endpoint POST /api/auth/login implementado",
            "Validacao de entrada com Pydantic",
            "Testes unitarios com cobertura > 80%",
            "Tela de login responsiva",
            "Tratamento de erros no frontend",
            "Documentacao da API atualizada"
        ],
        default_points=5,
        default_complexity="medium",
        default_priority="high",
        default_tasks=[
            {"title": "Criar modelo de Usuario", "task_type": "development", "agent_id": "AGT-07", "estimated_hours": 2},
            {"title": "Implementar endpoint de login", "task_type": "development", "agent_id": "AGT-08", "estimated_hours": 4},
            {"title": "Criar componente de login", "task_type": "development", "agent_id": "AGT-09", "estimated_hours": 3},
            {"title": "Testes de autenticacao", "task_type": "test", "agent_id": "AGT-15", "estimated_hours": 2}
        ],
        variables=["{auth_method}"]
    ),
    "auth-register": StoryTemplate(
        item_id="STORY-TPL-002",
        name="Autenticacao - Registro",
        description="Template para cadastro de novos usuarios com validacao de dados e confirmacao de email.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["auth", "register", "signup"],
        downloads=1890,
        rating=4.8,
        reviews_count=189,
        verified=True,
        pack_type=StoryPackType.AUTH.value,
        title_template="[Auth] Implementar registro de usuarios",
        persona="visitante do sistema",
        action="criar uma conta com meus dados",
        benefit="possa acessar as funcionalidades do sistema",
        acceptance_criteria=[
            "Formulario de registro com campos obrigatorios",
            "Validacao de email unico",
            "Validacao de forca da senha",
            "Email de confirmacao enviado",
            "Mensagens de erro claras"
        ],
        definition_of_done=[
            "Endpoint POST /api/auth/register implementado",
            "Envio de email de confirmacao",
            "Testes unitarios e de integracao",
            "Tela de registro responsiva"
        ],
        default_points=5,
        default_complexity="medium",
        default_priority="high"
    ),
    "crud-entity": StoryTemplate(
        item_id="STORY-TPL-003",
        name="CRUD Completo - Entidade Generica",
        description="Template para CRUD completo de uma entidade com listagem paginada, filtros, criacao, edicao e exclusao.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["crud", "entity", "backend", "frontend"],
        downloads=3456,
        rating=4.7,
        reviews_count=312,
        verified=True,
        featured=True,
        pack_type=StoryPackType.CRUD.value,
        title_template="[CRUD] Gerenciamento de {entity_name}",
        persona="administrador do sistema",
        action="gerenciar {entity_name} (criar, listar, editar, excluir)",
        benefit="mantenha os dados do sistema atualizados",
        acceptance_criteria=[
            "Listagem paginada de {entity_name}",
            "Filtros e busca por campos principais",
            "Formulario de criacao com validacao",
            "Formulario de edicao",
            "Confirmacao antes de excluir",
            "Mensagens de sucesso e erro"
        ],
        definition_of_done=[
            "Modelo de dados implementado",
            "Endpoints REST completos (GET, POST, PUT, DELETE)",
            "Testes com cobertura > 80%",
            "UI responsiva com Tailwind",
            "Validacao no frontend e backend"
        ],
        default_points=8,
        default_complexity="medium",
        default_priority="medium",
        default_tasks=[
            {"title": "Criar modelo e migration", "task_type": "development", "agent_id": "AGT-07", "estimated_hours": 2},
            {"title": "Implementar endpoints REST", "task_type": "development", "agent_id": "AGT-08", "estimated_hours": 4},
            {"title": "Criar componentes de listagem", "task_type": "development", "agent_id": "AGT-09", "estimated_hours": 4},
            {"title": "Criar formularios de edicao", "task_type": "development", "agent_id": "AGT-09", "estimated_hours": 3},
            {"title": "Testes de CRUD", "task_type": "test", "agent_id": "AGT-15", "estimated_hours": 3}
        ],
        variables=["{entity_name}"]
    ),
    "dashboard-metrics": StoryTemplate(
        item_id="STORY-TPL-004",
        name="Dashboard - Metricas e KPIs",
        description="Template para dashboard com cards de metricas, graficos e indicadores de performance.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["dashboard", "metrics", "charts", "kpi"],
        downloads=1567,
        rating=4.6,
        reviews_count=145,
        verified=True,
        pack_type=StoryPackType.DASHBOARD.value,
        title_template="[Dashboard] Visualizacao de {metric_type}",
        persona="gestor",
        action="visualizar metricas e KPIs do sistema",
        benefit="tome decisoes baseadas em dados",
        acceptance_criteria=[
            "Cards com metricas principais",
            "Graficos interativos (linha, barra, pizza)",
            "Filtro por periodo",
            "Atualizacao em tempo real opcional",
            "Responsivo para mobile"
        ],
        definition_of_done=[
            "API de agregacao de dados implementada",
            "Componentes de graficos funcionando",
            "Cache de dados para performance",
            "Testes de UI"
        ],
        default_points=8,
        default_complexity="high",
        default_priority="medium",
        variables=["{metric_type}"]
    ),
    "reports-export": StoryTemplate(
        item_id="STORY-TPL-005",
        name="Relatorios - Exportacao",
        description="Template para geracao e exportacao de relatorios em PDF e Excel.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["reports", "export", "pdf", "excel"],
        downloads=987,
        rating=4.5,
        reviews_count=78,
        verified=True,
        pack_type=StoryPackType.REPORTS.value,
        title_template="[Relatorio] Exportar {report_type}",
        persona="usuario do sistema",
        action="exportar relatorios em PDF e Excel",
        benefit="possa analisar dados offline",
        acceptance_criteria=[
            "Selecao de formato de exportacao",
            "Filtros aplicados no relatorio",
            "Download automatico do arquivo",
            "Preview do relatorio antes de exportar"
        ],
        definition_of_done=[
            "Geracao de PDF implementada",
            "Geracao de Excel implementada",
            "Endpoint de download seguro",
            "Testes de exportacao"
        ],
        default_points=5,
        default_complexity="medium",
        default_priority="low",
        variables=["{report_type}"]
    ),
    "notification-system": StoryTemplate(
        item_id="STORY-TPL-006",
        name="Notificacoes - Sistema Completo",
        description="Template para sistema de notificacoes com push, email e in-app notifications.",
        category=MarketplaceCategory.STORY_TEMPLATE.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["notifications", "push", "email", "realtime"],
        downloads=876,
        rating=4.7,
        reviews_count=92,
        verified=True,
        pack_type=StoryPackType.NOTIFICATIONS.value,
        title_template="[Notificacao] Sistema de avisos para {notification_target}",
        persona="usuario do sistema",
        action="receber notificacoes sobre eventos importantes",
        benefit="esteja sempre atualizado",
        acceptance_criteria=[
            "Notificacoes in-app em tempo real",
            "Notificacoes por email configuraveis",
            "Central de notificacoes com historico",
            "Marcar como lido/nao lido",
            "Preferencias de notificacao por usuario"
        ],
        definition_of_done=[
            "WebSocket para tempo real implementado",
            "Servico de email configurado",
            "UI de notificacoes responsiva",
            "Testes de integracao"
        ],
        default_points=8,
        default_complexity="high",
        default_priority="medium",
        variables=["{notification_target}"]
    )
}

# Skills
SKILLS_CATALOG: Dict[str, Skill] = {
    "code-review-ai": Skill(
        item_id="SKILL-001",
        name="Code Review Inteligente",
        description="Skill para revisao automatica de codigo com analise de qualidade, seguranca e boas praticas.",
        category=MarketplaceCategory.SKILL.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["code-review", "quality", "security"],
        downloads=567,
        rating=4.8,
        reviews_count=45,
        verified=True,
        featured=True,
        skill_category=SkillCategory.CODE_GENERATION.value,
        prompt_template="""Analise o seguinte codigo e forneca:
1. Score de qualidade (0-100)
2. Problemas encontrados
3. Sugestoes de melhoria
4. Vulnerabilidades de seguranca
5. Boas praticas nao seguidas

Codigo:
{code}

Linguagem: {language}
Contexto: {context}""",
        system_instructions="Voce e um especialista em revisao de codigo com foco em qualidade, seguranca e boas praticas.",
        input_params=[
            {"name": "code", "type": "string", "required": True, "description": "Codigo a ser revisado"},
            {"name": "language", "type": "string", "required": True, "description": "Linguagem de programacao"},
            {"name": "context", "type": "string", "required": False, "description": "Contexto do codigo"}
        ],
        examples=[
            {"input": "def sum(a,b): return a+b", "output": "Score: 65. Sugestoes: adicionar type hints, docstring..."}
        ],
        compatible_agents=["AGT-13"],
        required_tools=["read_file", "write_file"]
    ),
    "test-generator": Skill(
        item_id="SKILL-002",
        name="Gerador de Testes Automatico",
        description="Gera testes unitarios e de integracao automaticamente baseado no codigo fonte.",
        category=MarketplaceCategory.SKILL.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["testing", "automation", "tdd"],
        downloads=456,
        rating=4.6,
        reviews_count=38,
        verified=True,
        skill_category=SkillCategory.TESTING.value,
        prompt_template="""Gere testes para o seguinte codigo:

{code}

Requisitos:
- Framework: {test_framework}
- Tipo: {test_type}
- Cobertura minima: {coverage}%

Inclua:
1. Testes de casos felizes
2. Testes de casos de erro
3. Testes de edge cases
4. Mocks quando necessario""",
        input_params=[
            {"name": "code", "type": "string", "required": True},
            {"name": "test_framework", "type": "string", "required": True},
            {"name": "test_type", "type": "string", "required": False, "default": "unit"},
            {"name": "coverage", "type": "number", "required": False, "default": 80}
        ],
        compatible_agents=["AGT-15", "AGT-16"]
    ),
    "api-docs-generator": Skill(
        item_id="SKILL-003",
        name="Gerador de Documentacao de API",
        description="Gera documentacao OpenAPI/Swagger automaticamente a partir do codigo.",
        category=MarketplaceCategory.SKILL.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["api", "documentation", "openapi", "swagger"],
        downloads=345,
        rating=4.7,
        reviews_count=29,
        verified=True,
        skill_category=SkillCategory.DOCUMENTATION.value,
        prompt_template="""Gere documentacao OpenAPI para os seguintes endpoints:

{endpoints}

Inclua:
- Descricoes claras
- Exemplos de request/response
- Schemas de dados
- Codigos de status
- Autenticacao necessaria""",
        input_params=[
            {"name": "endpoints", "type": "string", "required": True}
        ],
        compatible_agents=["AGT-17", "AGT-08"]
    ),
    "sql-optimizer": Skill(
        item_id="SKILL-004",
        name="Otimizador de SQL",
        description="Analisa e otimiza queries SQL para melhor performance.",
        category=MarketplaceCategory.SKILL.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["sql", "database", "performance", "optimization"],
        downloads=234,
        rating=4.5,
        reviews_count=21,
        verified=True,
        skill_category=SkillCategory.DATABASE.value,
        prompt_template="""Analise e otimize a seguinte query SQL:

{query}

Database: {database_type}
Tabelas envolvidas: {tables}

Forneca:
1. Analise do plano de execucao
2. Indices recomendados
3. Query otimizada
4. Explicacao das melhorias""",
        input_params=[
            {"name": "query", "type": "string", "required": True},
            {"name": "database_type", "type": "string", "required": True},
            {"name": "tables", "type": "string", "required": False}
        ],
        compatible_agents=["AGT-07", "AGT-06"]
    ),
    "security-scanner": Skill(
        item_id="SKILL-005",
        name="Scanner de Seguranca",
        description="Identifica vulnerabilidades de seguranca no codigo e configuracoes.",
        category=MarketplaceCategory.SKILL.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["security", "vulnerabilities", "owasp"],
        downloads=389,
        rating=4.9,
        reviews_count=34,
        verified=True,
        featured=True,
        skill_category=SkillCategory.SECURITY.value,
        prompt_template="""Analise o seguinte codigo em busca de vulnerabilidades:

{code}

Verifique:
1. OWASP Top 10
2. Injection attacks
3. XSS
4. CSRF
5. Exposicao de dados sensiveis
6. Configuracoes inseguras

Forneca severidade e remediacoes.""",
        input_params=[
            {"name": "code", "type": "string", "required": True}
        ],
        compatible_agents=["AGT-10"]
    )
}

# Configuracoes de Agentes
AGENT_CONFIGS_CATALOG: Dict[str, AgentConfig] = {
    "fullstack-developer": AgentConfig(
        item_id="AGENT-CFG-001",
        name="Full Stack Developer",
        description="Configuracao de agente para desenvolvimento full stack com foco em React e FastAPI.",
        category=MarketplaceCategory.AGENT_CONFIG.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["fullstack", "react", "fastapi", "typescript"],
        downloads=678,
        rating=4.7,
        reviews_count=56,
        verified=True,
        featured=True,
        agent_type="fullstack_dev",
        system_prompt="""Voce e um desenvolvedor full stack experiente especializado em:
- Frontend: React, TypeScript, Tailwind CSS
- Backend: FastAPI, Python, SQLAlchemy
- Database: PostgreSQL, Redis
- DevOps: Docker, GitHub Actions

Sempre siga boas praticas de codigo, escreva testes e documente suas solucoes.""",
        instructions=[
            "Sempre use TypeScript no frontend",
            "Valide dados com Pydantic no backend",
            "Escreva testes unitarios para toda logica de negocio",
            "Use componentes funcionais no React",
            "Implemente tratamento de erros adequado"
        ],
        skills=["SKILL-001", "SKILL-002"],
        mcp_tools=["read_file", "write_file", "execute_command", "web_search"],
        specialties=["React", "FastAPI", "PostgreSQL", "Docker"]
    ),
    "qa-engineer": AgentConfig(
        item_id="AGENT-CFG-002",
        name="QA Engineer",
        description="Configuracao de agente especializado em qualidade e testes automatizados.",
        category=MarketplaceCategory.AGENT_CONFIG.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["qa", "testing", "automation", "playwright"],
        downloads=345,
        rating=4.6,
        reviews_count=32,
        verified=True,
        agent_type="qa_engineer",
        system_prompt="""Voce e um engenheiro de QA especializado em:
- Testes unitarios com pytest e Jest
- Testes E2E com Playwright
- Testes de API com requests/httpx
- Automacao de testes e CI/CD

Sempre escreva testes claros, mantiveis e com boa cobertura.""",
        instructions=[
            "Use arrange-act-assert pattern",
            "Escreva testes independentes",
            "Implemente fixtures reutilizaveis",
            "Documente casos de teste",
            "Priorize testes de regressao"
        ],
        skills=["SKILL-002"],
        mcp_tools=["read_file", "write_file", "execute_command"],
        specialties=["pytest", "Jest", "Playwright", "CI/CD"]
    ),
    "devops-engineer": AgentConfig(
        item_id="AGENT-CFG-003",
        name="DevOps Engineer",
        description="Configuracao de agente para infraestrutura, CI/CD e containers.",
        category=MarketplaceCategory.AGENT_CONFIG.value,
        author="Fabrica de Agentes",
        version="1.0.0",
        tags=["devops", "docker", "kubernetes", "cicd"],
        downloads=456,
        rating=4.8,
        reviews_count=41,
        verified=True,
        agent_type="devops_engineer",
        system_prompt="""Voce e um engenheiro DevOps especializado em:
- Containerizacao com Docker
- Orquestracao com Kubernetes
- CI/CD com GitHub Actions
- Infraestrutura como codigo (Terraform)
- Monitoramento (Prometheus, Grafana)

Sempre siga principios de seguranca e automacao.""",
        instructions=[
            "Use multi-stage builds no Docker",
            "Implemente health checks",
            "Configure limites de recursos",
            "Use secrets de forma segura",
            "Implemente rollback automatico"
        ],
        skills=[],
        mcp_tools=["execute_command", "read_file", "write_file"],
        specialties=["Docker", "Kubernetes", "Terraform", "GitHub Actions"]
    )
}


# =============================================================================
# GERENCIADOR DO MARKETPLACE
# =============================================================================

class MarketplaceManager:
    """Gerenciador do Marketplace de Templates e Skills"""

    def __init__(self):
        # Catalogos em memoria (em producao seria banco de dados)
        self.project_templates = dict(PROJECT_TEMPLATES_CATALOG)
        self.story_templates = dict(STORY_TEMPLATES_CATALOG)
        self.skills = dict(SKILLS_CATALOG)
        self.agent_configs = dict(AGENT_CONFIGS_CATALOG)
        self.reviews: Dict[str, List[Review]] = {}
        self.user_downloads: Dict[str, List[str]] = {}  # user_id -> [item_ids]

    # =========================================================================
    # BUSCA E LISTAGEM
    # =========================================================================

    def search(
        self,
        query: str = "",
        category: Optional[str] = None,
        tags: Optional[List[str]] = None,
        verified_only: bool = False,
        featured_only: bool = False,
        sort_by: str = "downloads",
        limit: int = 20
    ) -> List[Dict[str, Any]]:
        """Busca itens no marketplace"""
        results = []

        # Combinar todos os catalogos
        all_items = []
        all_items.extend(self.project_templates.values())
        all_items.extend(self.story_templates.values())
        all_items.extend(self.skills.values())
        all_items.extend(self.agent_configs.values())

        for item in all_items:
            # Filtro por categoria
            if category and item.category != category:
                continue

            # Filtro por verificado
            if verified_only and not item.verified:
                continue

            # Filtro por destaque
            if featured_only and not item.featured:
                continue

            # Filtro por tags
            if tags:
                if not any(tag in item.tags for tag in tags):
                    continue

            # Filtro por texto
            if query:
                query_lower = query.lower()
                searchable = f"{item.name} {item.description} {' '.join(item.tags)}".lower()
                if query_lower not in searchable:
                    continue

            results.append(item.to_dict())

        # Ordenar
        if sort_by == "downloads":
            results.sort(key=lambda x: x.get("downloads", 0), reverse=True)
        elif sort_by == "rating":
            results.sort(key=lambda x: x.get("rating", 0), reverse=True)
        elif sort_by == "recent":
            results.sort(key=lambda x: x.get("created_at", ""), reverse=True)
        elif sort_by == "name":
            results.sort(key=lambda x: x.get("name", ""))

        return results[:limit]

    def get_by_category(self, category: str) -> List[Dict[str, Any]]:
        """Lista itens por categoria"""
        return self.search(category=category)

    def get_featured(self) -> List[Dict[str, Any]]:
        """Lista itens em destaque"""
        return self.search(featured_only=True)

    def get_popular(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Lista itens mais populares"""
        return self.search(sort_by="downloads", limit=limit)

    # =========================================================================
    # DETALHES E DOWNLOAD
    # =========================================================================

    def get_item(self, item_id: str) -> Optional[Dict[str, Any]]:
        """Busca item por ID"""
        # Buscar em todos os catalogos
        if item_id in self.project_templates:
            return self.project_templates[item_id].to_dict()
        if item_id in self.story_templates:
            return self.story_templates[item_id].to_dict()
        if item_id in self.skills:
            return self.skills[item_id].to_dict()
        if item_id in self.agent_configs:
            return self.agent_configs[item_id].to_dict()
        return None

    def download_item(self, item_id: str, user_id: str = "anonymous") -> Optional[Dict[str, Any]]:
        """Baixa/instala um item (incrementa contador)"""
        item = None

        # Encontrar e incrementar downloads
        if item_id in self.project_templates:
            self.project_templates[item_id].downloads += 1
            item = self.project_templates[item_id]
        elif item_id in self.story_templates:
            self.story_templates[item_id].downloads += 1
            item = self.story_templates[item_id]
        elif item_id in self.skills:
            self.skills[item_id].downloads += 1
            item = self.skills[item_id]
        elif item_id in self.agent_configs:
            self.agent_configs[item_id].downloads += 1
            item = self.agent_configs[item_id]

        if item:
            # Registrar download do usuario
            if user_id not in self.user_downloads:
                self.user_downloads[user_id] = []
            if item_id not in self.user_downloads[user_id]:
                self.user_downloads[user_id].append(item_id)

            return item.to_dict()

        return None

    def get_user_downloads(self, user_id: str) -> List[Dict[str, Any]]:
        """Lista itens baixados por um usuario"""
        if user_id not in self.user_downloads:
            return []

        items = []
        for item_id in self.user_downloads[user_id]:
            item = self.get_item(item_id)
            if item:
                items.append(item)

        return items

    # =========================================================================
    # REVIEWS E RATINGS
    # =========================================================================

    def add_review(
        self,
        item_id: str,
        user_id: str,
        rating: int,
        comment: str
    ) -> Optional[Review]:
        """Adiciona review a um item"""
        if not 1 <= rating <= 5:
            return None

        item = self.get_item(item_id)
        if not item:
            return None

        review = Review(
            review_id=f"REV-{uuid.uuid4().hex[:8].upper()}",
            item_id=item_id,
            user_id=user_id,
            rating=rating,
            comment=comment
        )

        if item_id not in self.reviews:
            self.reviews[item_id] = []
        self.reviews[item_id].append(review)

        # Atualizar rating do item
        self._update_item_rating(item_id)

        return review

    def get_reviews(self, item_id: str) -> List[Dict[str, Any]]:
        """Lista reviews de um item"""
        if item_id not in self.reviews:
            return []
        return [r.to_dict() for r in self.reviews[item_id]]

    def _update_item_rating(self, item_id: str):
        """Atualiza rating medio do item"""
        if item_id not in self.reviews:
            return

        reviews = self.reviews[item_id]
        if not reviews:
            return

        avg_rating = sum(r.rating for r in reviews) / len(reviews)

        # Atualizar no catalogo correto
        if item_id in self.project_templates:
            self.project_templates[item_id].rating = round(avg_rating, 1)
            self.project_templates[item_id].reviews_count = len(reviews)
        elif item_id in self.story_templates:
            self.story_templates[item_id].rating = round(avg_rating, 1)
            self.story_templates[item_id].reviews_count = len(reviews)
        elif item_id in self.skills:
            self.skills[item_id].rating = round(avg_rating, 1)
            self.skills[item_id].reviews_count = len(reviews)
        elif item_id in self.agent_configs:
            self.agent_configs[item_id].rating = round(avg_rating, 1)
            self.agent_configs[item_id].reviews_count = len(reviews)

    # =========================================================================
    # PUBLICACAO DE NOVOS ITENS
    # =========================================================================

    def publish_story_template(
        self,
        name: str,
        description: str,
        author: str,
        template_data: Dict[str, Any]
    ) -> StoryTemplate:
        """Publica um novo template de story"""
        item_id = f"STORY-TPL-{uuid.uuid4().hex[:6].upper()}"

        template = StoryTemplate(
            item_id=item_id,
            name=name,
            description=description,
            category=MarketplaceCategory.STORY_TEMPLATE.value,
            author=author,
            version="1.0.0",
            tags=template_data.get("tags", []),
            pack_type=template_data.get("pack_type", StoryPackType.CRUD.value),
            title_template=template_data.get("title_template", ""),
            persona=template_data.get("persona", ""),
            action=template_data.get("action", ""),
            benefit=template_data.get("benefit", ""),
            acceptance_criteria=template_data.get("acceptance_criteria", []),
            definition_of_done=template_data.get("definition_of_done", []),
            default_points=template_data.get("default_points", 3),
            default_complexity=template_data.get("default_complexity", "medium"),
            default_priority=template_data.get("default_priority", "medium"),
            default_tasks=template_data.get("default_tasks", []),
            variables=template_data.get("variables", [])
        )

        self.story_templates[item_id] = template
        return template

    def publish_skill(
        self,
        name: str,
        description: str,
        author: str,
        skill_data: Dict[str, Any]
    ) -> Skill:
        """Publica um novo skill"""
        item_id = f"SKILL-{uuid.uuid4().hex[:6].upper()}"

        skill = Skill(
            item_id=item_id,
            name=name,
            description=description,
            category=MarketplaceCategory.SKILL.value,
            author=author,
            version="1.0.0",
            tags=skill_data.get("tags", []),
            skill_category=skill_data.get("skill_category", SkillCategory.CODE_GENERATION.value),
            prompt_template=skill_data.get("prompt_template", ""),
            system_instructions=skill_data.get("system_instructions", ""),
            input_params=skill_data.get("input_params", []),
            examples=skill_data.get("examples", []),
            compatible_agents=skill_data.get("compatible_agents", []),
            required_tools=skill_data.get("required_tools", [])
        )

        self.skills[item_id] = skill
        return skill

    # =========================================================================
    # APLICACAO DE TEMPLATES
    # =========================================================================

    def apply_story_template(
        self,
        template_id: str,
        project_id: str,
        variables: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """Aplica um template de story gerando dados para criacao"""
        if template_id not in self.story_templates:
            return {"error": "Template not found"}

        template = self.story_templates[template_id]
        variables = variables or {}

        # Substituir variaveis
        def replace_vars(text: str) -> str:
            for var, value in variables.items():
                text = text.replace(f"{{{var}}}", value)
            return text

        # Gerar dados da story
        story_data = {
            "project_id": project_id,
            "title": replace_vars(template.title_template),
            "persona": replace_vars(template.persona),
            "action": replace_vars(template.action),
            "benefit": replace_vars(template.benefit),
            "acceptance_criteria": [replace_vars(c) for c in template.acceptance_criteria],
            "definition_of_done": template.definition_of_done,
            "story_points": template.default_points,
            "complexity": template.default_complexity,
            "priority": template.default_priority,
            "category": template.suggested_category,
            "tasks": []
        }

        # Gerar tasks
        for task_template in template.default_tasks:
            story_data["tasks"].append({
                "title": replace_vars(task_template.get("title", "")),
                "task_type": task_template.get("task_type", "development"),
                "agent_id": task_template.get("agent_id"),
                "estimated_hours": task_template.get("estimated_hours", 0)
            })

        # Incrementar downloads
        template.downloads += 1

        return story_data

    def get_statistics(self) -> Dict[str, Any]:
        """Retorna estatisticas do marketplace"""
        total_downloads = 0
        total_items = 0

        for template in self.project_templates.values():
            total_downloads += template.downloads
            total_items += 1

        for template in self.story_templates.values():
            total_downloads += template.downloads
            total_items += 1

        for skill in self.skills.values():
            total_downloads += skill.downloads
            total_items += 1

        for config in self.agent_configs.values():
            total_downloads += config.downloads
            total_items += 1

        return {
            "total_items": total_items,
            "total_downloads": total_downloads,
            "project_templates": len(self.project_templates),
            "story_templates": len(self.story_templates),
            "skills": len(self.skills),
            "agent_configs": len(self.agent_configs),
            "total_reviews": sum(len(reviews) for reviews in self.reviews.values())
        }


# Instancia global do gerenciador
marketplace = MarketplaceManager()


# =============================================================================
# FUNCOES DE CONVENIENCIA
# =============================================================================

def get_marketplace() -> MarketplaceManager:
    """Retorna instancia do gerenciador de marketplace"""
    return marketplace


def search_marketplace(query: str = "", **kwargs) -> List[Dict[str, Any]]:
    """Busca no marketplace"""
    return marketplace.search(query, **kwargs)


def get_marketplace_item(item_id: str) -> Optional[Dict[str, Any]]:
    """Busca item por ID"""
    return marketplace.get_item(item_id)


def install_marketplace_item(item_id: str, user_id: str = "anonymous") -> Optional[Dict[str, Any]]:
    """Instala um item do marketplace"""
    return marketplace.download_item(item_id, user_id)


def apply_template(template_id: str, project_id: str, variables: Dict[str, str] = None) -> Dict[str, Any]:
    """Aplica um template de story"""
    return marketplace.apply_story_template(template_id, project_id, variables)


# Exportar classes e funcoes principais
__all__ = [
    # Classes
    "MarketplaceManager",
    "MarketplaceItem",
    "ProjectTemplate",
    "StoryTemplate",
    "Skill",
    "AgentConfig",
    "Review",
    # Enums
    "MarketplaceCategory",
    "TemplateType",
    "StoryPackType",
    "SkillCategory",
    # Funcoes
    "get_marketplace",
    "search_marketplace",
    "get_marketplace_item",
    "install_marketplace_item",
    "apply_template",
    # Catalogos
    "PROJECT_TEMPLATES_CATALOG",
    "STORY_TEMPLATES_CATALOG",
    "SKILLS_CATALOG",
    "AGENT_CONFIGS_CATALOG"
]
