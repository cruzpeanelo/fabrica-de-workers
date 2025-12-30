# -*- coding: utf-8 -*-
"""
Custom Persona Creation - Issue #112 Extension
"""
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any
from datetime import datetime
from enum import Enum

# Issue #178: Valid permission resources and actions
VALID_RESOURCES = [
    "projects", "stories", "tasks", "epics", "sprints",
    "documentation", "designs", "users", "roles", "workers",
    "jobs", "chat", "settings", "audit",
    # Extended resources for custom personas
    "code", "reports", "metrics", "analytics", "billing",
    "tenants", "integrations", "webhooks", "api_keys",
    "*"  # Wildcard for all resources
]

VALID_ACTIONS = [
    "create", "read", "update", "delete", "manage",
    "execute", "assign", "*"
]


class PermissionScope(str, Enum):
    GLOBAL = "global"
    TENANT = "tenant"
    PROJECT = "project"


@dataclass
class AdvancedPermission:
    resource: str
    action: str
    scope: PermissionScope = PermissionScope.GLOBAL
    conditions: Dict[str, Any] = field(default_factory=dict)

    def matches(self, resource: str, action: str, context: Dict = None) -> bool:
        if self.resource != resource and self.resource != "*":
            return False
        if self.action != action and self.action != "*":
            return False
        return True

    def to_dict(self) -> Dict:
        return {"resource": self.resource, "action": self.action, "scope": self.scope.value}


@dataclass
class CustomPersona:
    id: str
    name: str
    description: str
    permissions: List[AdvancedPermission] = field(default_factory=list)
    parent_persona_id: Optional[str] = None
    level: int = 100
    dashboard_type: str = "default"
    features: List[str] = field(default_factory=list)
    tenant_id: Optional[str] = None

    def add_permission(self, resource: str, action: str, **kwargs):
        self.permissions.append(AdvancedPermission(resource=resource, action=action, **kwargs))

    def has_permission(self, resource: str, action: str, context: Dict = None) -> bool:
        return any(p.matches(resource, action, context) for p in self.permissions)

    def to_dict(self) -> Dict:
        return {
            "id": self.id, "name": self.name, "description": self.description,
            "permissions": [p.to_dict() for p in self.permissions],
            "parent_persona_id": self.parent_persona_id, "level": self.level,
            "dashboard_type": self.dashboard_type, "features": self.features
        }


class PersonaBuilder:
    """Builder para criação de personas customizadas"""

    def __init__(self, id: str, name: str):
        self._persona = CustomPersona(id=id, name=name, description="")

    def with_description(self, desc: str):
        self._persona.description = desc
        return self

    def with_level(self, level: int):
        self._persona.level = level
        return self

    def with_dashboard(self, dash: str):
        self._persona.dashboard_type = dash
        return self

    def with_parent(self, parent_id: str):
        self._persona.parent_persona_id = parent_id
        return self

    def with_permission(self, resource: str, action: str):
        """
        Adiciona permissão validando formato - Issue #178

        Args:
            resource: recurso (projects, stories, tasks, etc. ou *)
            action: ação (create, read, update, delete, manage, execute, assign ou *)

        Raises:
            ValueError: se resource ou action for inválido
        """
        if resource not in VALID_RESOURCES:
            raise ValueError(
                f"Invalid resource: '{resource}'. "
                f"Valid resources: {', '.join(VALID_RESOURCES)}"
            )
        if action not in VALID_ACTIONS:
            raise ValueError(
                f"Invalid action: '{action}'. "
                f"Valid actions: {', '.join(VALID_ACTIONS)}"
            )
        self._persona.add_permission(resource, action)
        return self

    def with_feature(self, feature: str):
        self._persona.features.append(feature)
        return self

    def for_tenant(self, tenant_id: str):
        self._persona.tenant_id = tenant_id
        return self

    def build(self) -> CustomPersona:
        return self._persona


class PermissionInheritanceResolver:
    def __init__(self): self._personas = {}

    def register(self, persona: CustomPersona): self._personas[persona.id] = persona

    def get_effective_permissions(self, persona_id: str) -> List[AdvancedPermission]:
        persona = self._personas.get(persona_id)
        if not persona: return []
        permissions = list(persona.permissions)
        if persona.parent_persona_id:
            parent_perms = self.get_effective_permissions(persona.parent_persona_id)
            for perm in parent_perms:
                if not any(p.resource == perm.resource and p.action == perm.action for p in permissions):
                    permissions.append(perm)
        return permissions

    def check_permission(self, persona_id: str, resource: str, action: str, context: Dict = None) -> bool:
        return any(p.matches(resource, action, context) for p in self.get_effective_permissions(persona_id))


class PersonaTemplates:
    @staticmethod
    def developer():
        return (PersonaBuilder("template_developer", "Developer")
            .with_description("Developer permissions").with_level(50).with_dashboard("developer")
            .with_permission("tasks", "*").with_permission("code", "*")
            .with_feature("manage_tasks").build())

    @staticmethod
    def manager():
        return (PersonaBuilder("template_manager", "Manager")
            .with_description("Manager permissions").with_level(30).with_dashboard("manager")
            .with_permission("projects", "*").with_permission("stories", "*")
            .with_feature("manage_projects").build())

    @staticmethod
    def executive():
        return (PersonaBuilder("template_executive", "Executive")
            .with_description("Executive permissions").with_level(20).with_dashboard("executive")
            .with_permission("reports", "*").with_permission("metrics", "*")
            .with_feature("view_portfolio").build())


persona_inheritance_resolver = PermissionInheritanceResolver()

def create_custom_persona(id, name, description="", template=None, parent_id=None, tenant_id=None):
    templates = {"developer": PersonaTemplates.developer, "manager": PersonaTemplates.manager, "executive": PersonaTemplates.executive}
    if template and template in templates:
        persona = templates[template]()
        persona.id = id
        persona.name = name
        persona.description = description or persona.description
        persona.tenant_id = tenant_id
        persona_inheritance_resolver.register(persona)
        return persona
    builder = PersonaBuilder(id, name).with_description(description)
    if parent_id: builder.with_parent(parent_id)
    if tenant_id: builder.for_tenant(tenant_id)
    persona = builder.build()
    persona_inheritance_resolver.register(persona)
    return persona


def register_custom_personas_endpoints(app):
    from fastapi import APIRouter, HTTPException
    router = APIRouter(prefix="/api/personas/custom", tags=["custom-personas"])
    custom_personas = {}

    @router.post("/")
    async def create_persona_endpoint(data: dict):
        persona = create_custom_persona(data["id"], data["name"], data.get("description", ""),
            data.get("template"), data.get("parent_id"), data.get("tenant_id"))
        custom_personas[persona.id] = persona
        return {"success": True, "persona": persona.to_dict()}

    @router.get("/{persona_id}")
    async def get_persona(persona_id: str):
        persona = custom_personas.get(persona_id)
        if not persona: raise HTTPException(status_code=404, detail="Persona not found")
        return {"persona": persona.to_dict()}

    @router.get("/{persona_id}/permissions")
    async def get_effective_permissions(persona_id: str):
        return {"permissions": [p.to_dict() for p in persona_inheritance_resolver.get_effective_permissions(persona_id)]}

    @router.get("/templates")
    async def list_templates():
        return {"templates": ["developer", "manager", "executive"]}

    app.include_router(router)
    print("[Auth] Custom Personas endpoints registered")
