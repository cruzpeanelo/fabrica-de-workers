# -*- coding: utf-8 -*-
"""
Personas - Sistema de Perfis e Personas
=========================================

Issue #112 - Sistema de Perfis e Personas

Este modulo define personas de usuario com permissoes granulares:
- Admin: Acesso total ao sistema
- ProjectManager: Gestao de projetos, sprints e equipe
- Developer: Acesso a codigo, tarefas e execucao
- Stakeholder: Visao executiva e KPIs
- Viewer: Apenas visualizacao

Funcionalidades:
- Definicao de permissoes por recurso e acao
- Heranca de permissoes entre perfis
- Verificacao de acesso granular
- Middleware de autorizacao
"""

from enum import Enum
from typing import Dict, List, Set, Optional, Any
from dataclasses import dataclass, field
from datetime import datetime


# =============================================================================
# PERMISSION DEFINITIONS
# =============================================================================

class Resource(str, Enum):
    """Recursos do sistema"""
    # Core Resources
    PROJECTS = "projects"
    STORIES = "stories"
    TASKS = "tasks"
    JOBS = "jobs"
    WORKERS = "workers"

    # User Management
    USERS = "users"
    ROLES = "roles"
    PERMISSIONS = "permissions"

    # Tenant Management
    TENANTS = "tenants"
    TENANT_SETTINGS = "tenant_settings"
    BILLING = "billing"

    # Content
    DOCUMENTS = "documents"
    ATTACHMENTS = "attachments"
    CODE = "code"
    DESIGNS = "designs"

    # Analytics
    METRICS = "metrics"
    REPORTS = "reports"
    AUDIT_LOGS = "audit_logs"

    # System
    SYSTEM_SETTINGS = "system_settings"
    API_KEYS = "api_keys"
    WEBHOOKS = "webhooks"
    INTEGRATIONS = "integrations"


class Action(str, Enum):
    """Acoes permitidas em recursos"""
    CREATE = "create"
    READ = "read"
    UPDATE = "update"
    DELETE = "delete"
    EXECUTE = "execute"      # Para jobs/workers
    APPROVE = "approve"      # Para reviews
    EXPORT = "export"        # Para relatorios
    IMPORT = "import"        # Para bulk operations
    MANAGE = "manage"        # Acesso total ao recurso
    VIEW_ALL = "view_all"    # Ver todos os itens (cross-tenant para admin)


@dataclass
class Permission:
    """Define uma permissao especifica"""
    resource: Resource
    action: Action
    conditions: Dict[str, Any] = field(default_factory=dict)

    def __str__(self) -> str:
        return f"{self.resource.value}:{self.action.value}"

    def matches(self, resource: str, action: str) -> bool:
        """Verifica se a permissao corresponde ao recurso e acao"""
        return self.resource.value == resource and self.action.value == action


# =============================================================================
# PERSONA DEFINITIONS
# =============================================================================

class PersonaType(str, Enum):
    """Tipos de persona no sistema"""
    SUPER_ADMIN = "super_admin"      # Platform level
    ADMIN = "admin"                   # Tenant level
    PROJECT_MANAGER = "project_manager"
    TECH_LEAD = "tech_lead"
    DEVELOPER = "developer"
    QA_ENGINEER = "qa_engineer"
    STAKEHOLDER = "stakeholder"
    VIEWER = "viewer"
    API_CLIENT = "api_client"        # Para integracao via API


@dataclass
class Persona:
    """
    Define uma persona com suas permissoes e configuracoes

    Attributes:
        persona_type: Tipo da persona
        name: Nome amigavel
        description: Descricao da persona
        permissions: Lista de permissoes
        inherits_from: Persona da qual herda permissoes
        level: Nivel hierarquico (0 = mais alto)
        dashboard_type: Tipo de dashboard padrao
        allowed_features: Features habilitadas
        restrictions: Restricoes especificas
    """
    persona_type: PersonaType
    name: str
    description: str
    permissions: List[Permission] = field(default_factory=list)
    inherits_from: Optional['Persona'] = None
    level: int = 100
    dashboard_type: str = "default"
    allowed_features: List[str] = field(default_factory=list)
    restrictions: Dict[str, Any] = field(default_factory=dict)

    def get_all_permissions(self) -> Set[str]:
        """Retorna todas as permissoes, incluindo herdadas"""
        perms = {str(p) for p in self.permissions}

        if self.inherits_from:
            perms |= self.inherits_from.get_all_permissions()

        return perms

    def has_permission(self, resource: str, action: str) -> bool:
        """Verifica se a persona tem a permissao especificada"""
        perm_key = f"{resource}:{action}"
        all_perms = self.get_all_permissions()

        # Verificar permissao exata
        if perm_key in all_perms:
            return True

        # Verificar permissao wildcard de recurso
        if f"{resource}:*" in all_perms:
            return True

        # Verificar permissao wildcard total
        if "*:*" in all_perms:
            return True

        # Issue #353: 'manage' inclui todas as acoes basicas (create, read, update, delete)
        if f"{resource}:manage" in all_perms:
            if action in ["create", "read", "update", "delete", "manage"]:
                return True

        return False

    def can_access_feature(self, feature: str) -> bool:
        """Verifica se a persona pode acessar uma feature"""
        return feature in self.allowed_features or "*" in self.allowed_features

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "persona_type": self.persona_type.value,
            "name": self.name,
            "description": self.description,
            "permissions": [str(p) for p in self.permissions],
            "all_permissions": list(self.get_all_permissions()),
            "inherits_from": self.inherits_from.persona_type.value if self.inherits_from else None,
            "level": self.level,
            "dashboard_type": self.dashboard_type,
            "allowed_features": self.allowed_features,
            "restrictions": self.restrictions
        }


# =============================================================================
# PERSONA REGISTRY
# =============================================================================

class PersonaRegistry:
    """
    Registro central de personas do sistema

    Gerencia todas as personas disponiveis e suas configuracoes.
    """

    def __init__(self):
        self._personas: Dict[PersonaType, Persona] = {}
        self._initialize_default_personas()

    def _initialize_default_personas(self):
        """Inicializa personas padrao do sistema"""

        # VIEWER - Nivel mais basico (apenas visualizacao)
        viewer = Persona(
            persona_type=PersonaType.VIEWER,
            name="Viewer",
            description="Usuario com acesso somente leitura ao projeto",
            permissions=[
                Permission(Resource.PROJECTS, Action.READ),
                Permission(Resource.STORIES, Action.READ),
                Permission(Resource.TASKS, Action.READ),
                Permission(Resource.DOCUMENTS, Action.READ),
                Permission(Resource.METRICS, Action.READ),
            ],
            level=100,
            dashboard_type="viewer",
            allowed_features=["view_projects", "view_stories", "view_progress"],
            restrictions={"can_edit": False, "can_delete": False}
        )
        self._personas[PersonaType.VIEWER] = viewer

        # STAKEHOLDER - Herda de Viewer + KPIs e relatorios
        stakeholder = Persona(
            persona_type=PersonaType.STAKEHOLDER,
            name="Stakeholder",
            description="Stakeholder do projeto com visao executiva",
            permissions=[
                Permission(Resource.REPORTS, Action.READ),
                Permission(Resource.REPORTS, Action.EXPORT),
                Permission(Resource.METRICS, Action.VIEW_ALL),
            ],
            inherits_from=viewer,
            level=80,
            dashboard_type="stakeholder",
            allowed_features=[
                "view_projects", "view_stories", "view_progress",
                "view_kpis", "export_reports", "view_budget"
            ],
            restrictions={"can_edit": False, "can_comment": True}
        )
        self._personas[PersonaType.STAKEHOLDER] = stakeholder

        # QA_ENGINEER - Focado em testes
        qa_engineer = Persona(
            persona_type=PersonaType.QA_ENGINEER,
            name="QA Engineer",
            description="Engenheiro de qualidade com foco em testes",
            permissions=[
                Permission(Resource.PROJECTS, Action.READ),
                Permission(Resource.STORIES, Action.READ),
                Permission(Resource.STORIES, Action.UPDATE),  # Para status de teste
                Permission(Resource.TASKS, Action.READ),
                Permission(Resource.TASKS, Action.UPDATE),
                Permission(Resource.TASKS, Action.CREATE),
                Permission(Resource.DOCUMENTS, Action.READ),
                Permission(Resource.DOCUMENTS, Action.CREATE),
                Permission(Resource.CODE, Action.READ),
                Permission(Resource.JOBS, Action.READ),
                Permission(Resource.JOBS, Action.EXECUTE),
            ],
            level=60,
            dashboard_type="developer",
            allowed_features=[
                "view_projects", "view_stories", "edit_stories",
                "view_tasks", "create_tasks", "run_tests",
                "view_code", "create_test_docs"
            ],
            restrictions={"can_delete_stories": False}
        )
        self._personas[PersonaType.QA_ENGINEER] = qa_engineer

        # DEVELOPER - Desenvolvimento completo
        developer = Persona(
            persona_type=PersonaType.DEVELOPER,
            name="Developer",
            description="Desenvolvedor com acesso a codigo e execucao",
            permissions=[
                Permission(Resource.PROJECTS, Action.READ),
                Permission(Resource.STORIES, Action.READ),
                Permission(Resource.STORIES, Action.UPDATE),
                Permission(Resource.TASKS, Action.MANAGE),
                Permission(Resource.DOCUMENTS, Action.MANAGE),
                Permission(Resource.CODE, Action.MANAGE),
                Permission(Resource.DESIGNS, Action.READ),
                Permission(Resource.JOBS, Action.READ),
                Permission(Resource.JOBS, Action.EXECUTE),
                Permission(Resource.WORKERS, Action.READ),
                Permission(Resource.ATTACHMENTS, Action.MANAGE),
            ],
            level=50,
            dashboard_type="developer",
            allowed_features=[
                "view_projects", "view_stories", "edit_stories",
                "manage_tasks", "manage_code", "execute_jobs",
                "view_workers", "upload_files", "create_docs"
            ],
            restrictions={"can_delete_projects": False, "can_manage_users": False}
        )
        self._personas[PersonaType.DEVELOPER] = developer

        # TECH_LEAD - Herda de Developer + gestao tecnica
        tech_lead = Persona(
            persona_type=PersonaType.TECH_LEAD,
            name="Tech Lead",
            description="Lider tecnico com gestao de equipe de desenvolvimento",
            permissions=[
                Permission(Resource.STORIES, Action.CREATE),
                Permission(Resource.STORIES, Action.DELETE),
                Permission(Resource.STORIES, Action.APPROVE),
                Permission(Resource.WORKERS, Action.MANAGE),
                Permission(Resource.METRICS, Action.MANAGE),
                Permission(Resource.REPORTS, Action.READ),
                Permission(Resource.REPORTS, Action.CREATE),
            ],
            inherits_from=developer,
            level=40,
            dashboard_type="manager",
            allowed_features=[
                "view_projects", "manage_stories", "manage_tasks",
                "manage_code", "manage_workers", "create_reports",
                "view_metrics", "code_review"
            ],
            restrictions={"can_delete_projects": False}
        )
        self._personas[PersonaType.TECH_LEAD] = tech_lead

        # PROJECT_MANAGER - Gestao completa do projeto
        project_manager = Persona(
            persona_type=PersonaType.PROJECT_MANAGER,
            name="Project Manager",
            description="Gestor de projeto com controle total do projeto",
            permissions=[
                Permission(Resource.PROJECTS, Action.MANAGE),
                Permission(Resource.STORIES, Action.MANAGE),
                Permission(Resource.TASKS, Action.MANAGE),
                Permission(Resource.DOCUMENTS, Action.MANAGE),
                Permission(Resource.METRICS, Action.MANAGE),
                Permission(Resource.REPORTS, Action.MANAGE),
                Permission(Resource.JOBS, Action.READ),
                Permission(Resource.WORKERS, Action.READ),
                Permission(Resource.USERS, Action.READ),  # Ver equipe
            ],
            level=30,
            dashboard_type="manager",
            allowed_features=[
                "manage_projects", "manage_stories", "manage_tasks",
                "manage_docs", "manage_team", "view_reports",
                "export_reports", "manage_sprints", "manage_epics"
            ],
            restrictions={"scope": "project"}  # Apenas no escopo do projeto
        )
        self._personas[PersonaType.PROJECT_MANAGER] = project_manager

        # ADMIN - Administrador do tenant
        admin = Persona(
            persona_type=PersonaType.ADMIN,
            name="Admin",
            description="Administrador do tenant com acesso completo",
            permissions=[
                Permission(Resource.PROJECTS, Action.MANAGE),
                Permission(Resource.STORIES, Action.MANAGE),
                Permission(Resource.TASKS, Action.MANAGE),
                Permission(Resource.JOBS, Action.MANAGE),
                Permission(Resource.WORKERS, Action.MANAGE),
                Permission(Resource.USERS, Action.MANAGE),
                Permission(Resource.ROLES, Action.MANAGE),
                Permission(Resource.DOCUMENTS, Action.MANAGE),
                Permission(Resource.CODE, Action.MANAGE),
                Permission(Resource.DESIGNS, Action.MANAGE),
                Permission(Resource.METRICS, Action.MANAGE),
                Permission(Resource.REPORTS, Action.MANAGE),
                Permission(Resource.AUDIT_LOGS, Action.READ),
                Permission(Resource.TENANT_SETTINGS, Action.MANAGE),
                Permission(Resource.BILLING, Action.READ),
                Permission(Resource.API_KEYS, Action.MANAGE),
                Permission(Resource.WEBHOOKS, Action.MANAGE),
                Permission(Resource.INTEGRATIONS, Action.MANAGE),
            ],
            level=10,
            dashboard_type="admin",
            allowed_features=["*"],  # Todas as features
            restrictions={"scope": "tenant"}  # Apenas no escopo do tenant
        )
        self._personas[PersonaType.ADMIN] = admin

        # SUPER_ADMIN - Administrador da plataforma
        super_admin = Persona(
            persona_type=PersonaType.SUPER_ADMIN,
            name="Super Admin",
            description="Administrador da plataforma com acesso total",
            permissions=[
                # Wildcard permission - acesso total
                Permission(Resource.PROJECTS, Action.MANAGE),
                Permission(Resource.TENANTS, Action.MANAGE),
                Permission(Resource.SYSTEM_SETTINGS, Action.MANAGE),
                Permission(Resource.AUDIT_LOGS, Action.MANAGE),
                Permission(Resource.BILLING, Action.MANAGE),
            ],
            inherits_from=admin,
            level=0,
            dashboard_type="admin",
            allowed_features=["*"],
            restrictions={}  # Sem restricoes
        )
        self._personas[PersonaType.SUPER_ADMIN] = super_admin

        # API_CLIENT - Para integracao via API
        api_client = Persona(
            persona_type=PersonaType.API_CLIENT,
            name="API Client",
            description="Cliente de API com acesso programatico",
            permissions=[
                Permission(Resource.PROJECTS, Action.READ),
                Permission(Resource.STORIES, Action.READ),
                Permission(Resource.STORIES, Action.CREATE),
                Permission(Resource.STORIES, Action.UPDATE),
                Permission(Resource.TASKS, Action.READ),
                Permission(Resource.TASKS, Action.CREATE),
                Permission(Resource.TASKS, Action.UPDATE),
                Permission(Resource.JOBS, Action.READ),
                Permission(Resource.JOBS, Action.CREATE),
                Permission(Resource.WEBHOOKS, Action.READ),
            ],
            level=70,
            dashboard_type="developer",
            allowed_features=["api_access"],
            restrictions={"ui_access": False, "rate_limited": True}
        )
        self._personas[PersonaType.API_CLIENT] = api_client

    def get_persona(self, persona_type: PersonaType) -> Optional[Persona]:
        """Retorna uma persona pelo tipo"""
        return self._personas.get(persona_type)

    def get_all_personas(self) -> List[Persona]:
        """Retorna todas as personas"""
        return list(self._personas.values())

    def get_personas_by_level(self, max_level: int) -> List[Persona]:
        """Retorna personas com nivel <= max_level"""
        return [p for p in self._personas.values() if p.level <= max_level]

    def register_persona(self, persona: Persona):
        """Registra uma nova persona"""
        self._personas[persona.persona_type] = persona


# =============================================================================
# PERMISSION CHECKER
# =============================================================================

class PermissionChecker:
    """
    Verificador de permissoes

    Verifica se um usuario tem permissao para realizar uma acao
    em um recurso especifico, considerando:
    - Persona do usuario
    - Escopo (tenant, projeto)
    - Condicoes adicionais
    """

    def __init__(self, registry: PersonaRegistry = None):
        self.registry = registry or PersonaRegistry()

    def check_permission(
        self,
        persona_type: PersonaType,
        resource: str,
        action: str,
        context: Dict[str, Any] = None
    ) -> bool:
        """
        Verifica se a persona tem permissao

        Args:
            persona_type: Tipo da persona
            resource: Recurso sendo acessado
            action: Acao sendo realizada
            context: Contexto adicional (tenant_id, project_id, etc)

        Returns:
            True se tem permissao
        """
        persona = self.registry.get_persona(persona_type)
        if not persona:
            return False

        # Verificar permissao basica
        if not persona.has_permission(resource, action):
            return False

        # Verificar restricoes de escopo
        if context and persona.restrictions:
            scope = persona.restrictions.get("scope")

            if scope == "tenant":
                # Deve ter tenant_id no contexto
                if not context.get("tenant_id"):
                    return False

            elif scope == "project":
                # Deve ter project_id no contexto
                if not context.get("project_id"):
                    return False

        return True

    def get_allowed_actions(
        self,
        persona_type: PersonaType,
        resource: str
    ) -> List[str]:
        """
        Retorna lista de acoes permitidas para um recurso

        Args:
            persona_type: Tipo da persona
            resource: Recurso

        Returns:
            Lista de acoes permitidas
        """
        persona = self.registry.get_persona(persona_type)
        if not persona:
            return []

        allowed = []
        for action in Action:
            if persona.has_permission(resource, action.value):
                allowed.append(action.value)

        return allowed

    def filter_by_permission(
        self,
        persona_type: PersonaType,
        items: List[Dict],
        resource: str,
        action: str = "read"
    ) -> List[Dict]:
        """
        Filtra itens baseado em permissoes

        Args:
            persona_type: Tipo da persona
            items: Lista de itens a filtrar
            resource: Recurso dos itens
            action: Acao necessaria

        Returns:
            Lista filtrada
        """
        if self.check_permission(persona_type, resource, action):
            return items
        return []


# =============================================================================
# USER PERSONA ASSIGNMENT
# =============================================================================

@dataclass
class UserPersonaAssignment:
    """
    Atribuicao de persona a um usuario

    Permite atribuir personas em diferentes escopos:
    - Global: Em todo o sistema
    - Tenant: Em um tenant especifico
    - Project: Em um projeto especifico
    """
    user_id: int
    persona_type: PersonaType
    scope_type: str = "global"  # global, tenant, project
    scope_id: Optional[str] = None
    assigned_by: Optional[str] = None
    assigned_at: datetime = field(default_factory=datetime.utcnow)
    expires_at: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def is_valid(self) -> bool:
        """Verifica se a atribuicao ainda e valida"""
        if self.expires_at and datetime.utcnow() > self.expires_at:
            return False
        return True

    def matches_scope(self, scope_type: str, scope_id: str = None) -> bool:
        """Verifica se a atribuicao corresponde ao escopo"""
        if self.scope_type == "global":
            return True
        if self.scope_type == scope_type:
            if scope_id is None or self.scope_id == scope_id:
                return True
        return False

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "user_id": self.user_id,
            "persona_type": self.persona_type.value,
            "scope_type": self.scope_type,
            "scope_id": self.scope_id,
            "assigned_by": self.assigned_by,
            "assigned_at": self.assigned_at.isoformat() if self.assigned_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "metadata": self.metadata
        }


# =============================================================================
# GLOBAL INSTANCES
# =============================================================================

# Registro global de personas
persona_registry = PersonaRegistry()

# Verificador global de permissoes
permission_checker = PermissionChecker(persona_registry)


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_persona(persona_type: str) -> Optional[Persona]:
    """
    Retorna uma persona pelo nome

    Args:
        persona_type: Nome do tipo (ex: "admin", "developer")

    Returns:
        Persona ou None
    """
    try:
        ptype = PersonaType(persona_type.lower())
        return persona_registry.get_persona(ptype)
    except ValueError:
        return None


def check_access(
    persona_type: str,
    resource: str,
    action: str,
    context: Dict[str, Any] = None
) -> bool:
    """
    Verifica acesso de forma simplificada

    Args:
        persona_type: Nome do tipo (ex: "admin")
        resource: Recurso (ex: "stories")
        action: Acao (ex: "create")
        context: Contexto opcional

    Returns:
        True se tem acesso
    """
    try:
        ptype = PersonaType(persona_type.lower())
        return permission_checker.check_permission(ptype, resource, action, context)
    except ValueError:
        return False


def get_persona_for_role(role: str) -> Optional[Persona]:
    """
    Mapeia role do sistema para persona

    Issue #144: Mapeamento completo com aliases comuns.

    Args:
        role: Role do usuario (ADMIN, VIEWER, etc)

    Returns:
        Persona correspondente
    """
    # Issue #144: Mapeamento expandido com todos os aliases comuns
    role_mapping = {
        # Super Admin
        "SUPER_ADMIN": PersonaType.SUPER_ADMIN,
        "SUPERADMIN": PersonaType.SUPER_ADMIN,
        "PLATFORM_ADMIN": PersonaType.SUPER_ADMIN,

        # Admin
        "ADMIN": PersonaType.ADMIN,
        "ADMINISTRATOR": PersonaType.ADMIN,
        "OWNER": PersonaType.ADMIN,
        "TENANT_ADMIN": PersonaType.ADMIN,

        # Project Manager
        "PROJECT_MANAGER": PersonaType.PROJECT_MANAGER,
        "MANAGER": PersonaType.PROJECT_MANAGER,
        "PM": PersonaType.PROJECT_MANAGER,
        "PRODUCT_MANAGER": PersonaType.PROJECT_MANAGER,
        "SCRUM_MASTER": PersonaType.PROJECT_MANAGER,
        "AGILE_COACH": PersonaType.PROJECT_MANAGER,

        # Tech Lead
        "TECH_LEAD": PersonaType.TECH_LEAD,
        "TECHLEAD": PersonaType.TECH_LEAD,
        "TECHNICAL_LEAD": PersonaType.TECH_LEAD,
        "LEAD_DEVELOPER": PersonaType.TECH_LEAD,
        "SENIOR_DEVELOPER": PersonaType.TECH_LEAD,
        "ARCHITECT": PersonaType.TECH_LEAD,

        # Developer
        "DEVELOPER": PersonaType.DEVELOPER,
        "DEV": PersonaType.DEVELOPER,
        "ENGINEER": PersonaType.DEVELOPER,
        "SOFTWARE_ENGINEER": PersonaType.DEVELOPER,
        "MEMBER": PersonaType.DEVELOPER,
        "CONTRIBUTOR": PersonaType.DEVELOPER,

        # QA Engineer
        "QA_ENGINEER": PersonaType.QA_ENGINEER,
        "QA": PersonaType.QA_ENGINEER,
        "TESTER": PersonaType.QA_ENGINEER,
        "QA_TESTER": PersonaType.QA_ENGINEER,
        "QUALITY_ASSURANCE": PersonaType.QA_ENGINEER,
        "TEST_ENGINEER": PersonaType.QA_ENGINEER,

        # Stakeholder
        "STAKEHOLDER": PersonaType.STAKEHOLDER,
        "BUSINESS_ANALYST": PersonaType.STAKEHOLDER,
        "PRODUCT_OWNER": PersonaType.STAKEHOLDER,
        "PO": PersonaType.STAKEHOLDER,
        "CLIENT": PersonaType.STAKEHOLDER,
        "CUSTOMER": PersonaType.STAKEHOLDER,

        # Viewer
        "VIEWER": PersonaType.VIEWER,
        "GUEST": PersonaType.VIEWER,
        "OBSERVER": PersonaType.VIEWER,
        "READONLY": PersonaType.VIEWER,
        "READ_ONLY": PersonaType.VIEWER,
    }

    persona_type = role_mapping.get(role.upper())
    if persona_type:
        return persona_registry.get_persona(persona_type)
    return persona_registry.get_persona(PersonaType.VIEWER)


# =============================================================================
# API ENDPOINTS
# =============================================================================

def register_personas_endpoints(app):
    """
    Registra endpoints de personas no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter, HTTPException

    router = APIRouter(prefix="/api/personas", tags=["personas"])

    @router.get("/")
    async def list_personas():
        """Lista todas as personas disponiveis"""
        return {
            "personas": [p.to_dict() for p in persona_registry.get_all_personas()]
        }

    @router.get("/{persona_type}")
    async def get_persona_details(persona_type: str):
        """Retorna detalhes de uma persona"""
        persona = get_persona(persona_type)
        if not persona:
            raise HTTPException(status_code=404, detail="Persona not found")
        return {"persona": persona.to_dict()}

    @router.get("/{persona_type}/permissions")
    async def get_persona_permissions(persona_type: str):
        """Retorna todas as permissoes de uma persona"""
        persona = get_persona(persona_type)
        if not persona:
            raise HTTPException(status_code=404, detail="Persona not found")
        return {
            "persona_type": persona_type,
            "permissions": list(persona.get_all_permissions())
        }

    @router.post("/check")
    async def check_permission_endpoint(
        persona_type: str,
        resource: str,
        action: str,
        context: Dict = None
    ):
        """Verifica se uma persona tem permissao"""
        has_access = check_access(persona_type, resource, action, context)
        return {
            "persona_type": persona_type,
            "resource": resource,
            "action": action,
            "has_permission": has_access
        }

    app.include_router(router)
    print("[Auth] Personas endpoints registered")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    # Demo: Listar todas as personas
    print("\n=== PERSONAS DO SISTEMA ===\n")

    for persona in persona_registry.get_all_personas():
        print(f"[{persona.level:3d}] {persona.name}")
        print(f"      Tipo: {persona.persona_type.value}")
        print(f"      Dashboard: {persona.dashboard_type}")
        print(f"      Permissoes: {len(persona.get_all_permissions())}")
        if persona.inherits_from:
            print(f"      Herda de: {persona.inherits_from.name}")
        print()

    # Demo: Testar permissoes
    print("\n=== TESTE DE PERMISSOES ===\n")

    tests = [
        ("admin", "stories", "create"),
        ("admin", "users", "manage"),
        ("developer", "stories", "create"),
        ("developer", "users", "manage"),
        ("viewer", "stories", "read"),
        ("viewer", "stories", "create"),
    ]

    for persona_type, resource, action in tests:
        result = check_access(persona_type, resource, action)
        status = "Permitido" if result else "Negado"
        print(f"  {persona_type:20s} | {resource}:{action:10s} -> {status}")
