# -*- coding: utf-8 -*-
"""
RBAC - Role-Based Access Control
Fabrica de Agentes v6.1

Sistema completo de controle de acesso baseado em roles (papeis).
Implementa:
- Roles hierarquicas (ADMIN > MANAGER > DEVELOPER > VIEWER)
- Permissoes granulares por recurso e acao
- Decorators para protecao de endpoints
- Escopo por projeto (opcional)
- Auditoria de acoes
"""

import uuid
import asyncio
from datetime import datetime
from enum import Enum
from functools import wraps
from typing import Optional, List, Dict, Any, Callable, Union

from fastapi import APIRouter, HTTPException, Depends, Request, Header, Query
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

# Database imports
from factory.database.connection import SessionLocal

# Permission Audit imports
from factory.auth.permission_audit import (
    PermissionAuditLogger,
    PermissionAuditAction,
    PermissionResourceType,
    permission_audit_logger,
    audit_action
)


# =============================================================================
# CONSTANTS - Resources and Actions
# =============================================================================

# Recursos do sistema
RESOURCES = [
    "projects",      # Projetos
    "stories",       # User Stories
    "tasks",         # Story Tasks
    "epics",         # Epicos
    "sprints",       # Sprints
    "documentation", # Documentacao
    "designs",       # Designs/Mockups
    "users",         # Usuarios
    "roles",         # Roles
    "workers",       # Workers
    "jobs",          # Jobs
    "chat",          # Chat/Assistente
    "settings",      # Configuracoes
    "audit",         # Logs de auditoria
]

# Acoes permitidas
ACTIONS = [
    "create",   # Criar
    "read",     # Ler/Visualizar
    "update",   # Atualizar
    "delete",   # Deletar
    "manage",   # Gerenciar (inclui todas acoes)
    "execute",  # Executar (jobs, workers)
    "assign",   # Atribuir (roles, tasks)
]


# =============================================================================
# DEFAULT ROLES - Roles padrao do sistema
# =============================================================================

DEFAULT_ROLES = {
    "ADMIN": {
        "role_id": "ROLE-ADMIN",
        "name": "ADMIN",
        "description": "Administrador com acesso total ao sistema",
        "level": 100,
        "is_system": True,
        "permissions": ["*:*"]  # Wildcard: todas as permissoes
    },
    "MANAGER": {
        "role_id": "ROLE-MANAGER",
        "name": "MANAGER",
        "description": "Gerente de projetos - pode criar e gerenciar stories, epics, sprints",
        "level": 75,
        "is_system": True,
        "permissions": [
            # Issue #143: Wildcards + explicit delete permissions for clarity
            "projects:*",
            "projects:delete",  # Issue #143: Explicit delete
            "stories:*",
            "stories:delete",   # Issue #143: Explicit delete
            "tasks:*",
            "tasks:delete",     # Issue #143: Explicit delete
            "epics:*",
            "sprints:*",
            "documentation:*",
            "designs:*",
            "chat:*",
            "workers:read",
            "jobs:read",
            "jobs:execute",
            "users:read",
            "audit:read",
        ]
    },
    "DEVELOPER": {
        "role_id": "ROLE-DEVELOPER",
        "name": "DEVELOPER",
        "description": "Desenvolvedor - pode criar e editar tasks, executar jobs",
        "level": 50,
        "is_system": True,
        "permissions": [
            "projects:read",
            "stories:read",
            "stories:update",
            "tasks:create",
            "tasks:read",
            "tasks:update",
            "epics:read",
            "sprints:read",
            "documentation:create",
            "documentation:read",
            "documentation:update",
            "designs:create",
            "designs:read",
            "designs:update",
            "chat:*",
            "workers:read",
            "jobs:read",
            "jobs:create",
            "jobs:execute",
        ]
    },
    "VIEWER": {
        "role_id": "ROLE-VIEWER",
        "name": "VIEWER",
        "description": "Visualizador - apenas leitura",
        "level": 10,
        "is_system": True,
        "permissions": [
            "projects:read",
            "stories:read",
            "tasks:read",
            "epics:read",
            "sprints:read",
            "documentation:read",
            "designs:read",
            "chat:read",
            "workers:read",
            "jobs:read",
        ]
    }
}


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class RoleCreate(BaseModel):
    name: str
    description: Optional[str] = None
    permissions: List[str] = []
    level: Optional[int] = 0


class RoleUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    permissions: Optional[List[str]] = None
    level: Optional[int] = None
    active: Optional[bool] = None


class UserRoleAssign(BaseModel):
    user_id: int
    role_id: str
    project_id: Optional[str] = None
    expires_at: Optional[str] = None


class PermissionCheck(BaseModel):
    resource: str
    action: str
    project_id: Optional[str] = None


class UserContext(BaseModel):
    """Contexto do usuario atual na requisicao"""
    user_id: Optional[int] = None
    username: Optional[str] = None
    roles: List[str] = []
    permissions: List[str] = []
    is_authenticated: bool = False
    is_admin: bool = False


# =============================================================================
# RBAC MANAGER - Gerenciador principal
# =============================================================================

class RBACManager:
    """
    Gerenciador central de RBAC
    Responsavel por verificar permissoes e gerenciar roles
    """

    def __init__(self, db_session=None):
        self.db = db_session or SessionLocal()
        # Issue #145: Cache agora usa chave composta user_id:project_id
        self._permission_cache: Dict[str, List[str]] = {}

    def close(self):
        if self.db:
            self.db.close()

    # -------------------------------------------------------------------------
    # Role Management
    # -------------------------------------------------------------------------

    def get_all_roles(self) -> List[Dict]:
        """Lista todas as roles"""
        from factory.database.models import Role
        roles = self.db.query(Role).filter(Role.active == True).all()
        return [r.to_dict() for r in roles]

    def get_role(self, role_id: str) -> Optional[Dict]:
        """Busca role por ID"""
        from factory.database.models import Role
        role = self.db.query(Role).filter(Role.role_id == role_id).first()
        return role.to_dict() if role else None

    def create_role(self, data: dict) -> Dict:
        """Cria nova role"""
        from factory.database.models import Role

        role = Role(
            role_id=f"ROLE-{uuid.uuid4().hex[:8].upper()}",
            name=data["name"],
            description=data.get("description"),
            permissions=data.get("permissions", []),
            level=data.get("level", 0),
            is_system=False,
            active=True
        )
        self.db.add(role)
        self.db.commit()
        self.db.refresh(role)
        return role.to_dict()

    def update_role(self, role_id: str, data: dict) -> Optional[Dict]:
        """Atualiza role"""
        from factory.database.models import Role

        role = self.db.query(Role).filter(Role.role_id == role_id).first()
        if not role:
            return None

        if role.is_system and "permissions" in data:
            # Nao permite alterar permissoes de roles do sistema
            raise HTTPException(403, "Cannot modify system role permissions")

        for key, value in data.items():
            if hasattr(role, key) and value is not None:
                setattr(role, key, value)

        role.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(role)

        # Limpar cache
        self._permission_cache.clear()

        return role.to_dict()

    def delete_role(self, role_id: str) -> bool:
        """Remove role (soft delete se sistema)"""
        from factory.database.models import Role

        role = self.db.query(Role).filter(Role.role_id == role_id).first()
        if not role:
            return False

        if role.is_system:
            raise HTTPException(403, "Cannot delete system role")

        self.db.delete(role)
        self.db.commit()

        # Limpar cache
        self._permission_cache.clear()

        return True

    # -------------------------------------------------------------------------
    # User Role Management
    # -------------------------------------------------------------------------

    def get_user_roles(self, user_id: int, project_id: Optional[str] = None) -> List[Dict]:
        """Lista roles de um usuario"""
        from factory.database.models import UserRole

        query = self.db.query(UserRole).filter(UserRole.user_id == user_id)

        if project_id:
            # Buscar roles globais e especificas do projeto
            query = query.filter(
                (UserRole.project_id == None) | (UserRole.project_id == project_id)
            )

        user_roles = query.all()
        return [ur.to_dict() for ur in user_roles if ur.is_valid()]

    def assign_role(self, data: dict, assigned_by: str = "system") -> Dict:
        """Atribui role a usuario"""
        from factory.database.models import UserRole, Role, User

        # Verificar se role existe
        role = self.db.query(Role).filter(Role.role_id == data["role_id"]).first()
        if not role:
            raise HTTPException(404, f"Role {data['role_id']} not found")

        # Verificar se usuario existe
        user = self.db.query(User).filter(User.id == data["user_id"]).first()
        if not user:
            raise HTTPException(404, f"User {data['user_id']} not found")

        # Verificar se ja existe atribuicao
        existing = self.db.query(UserRole).filter(
            UserRole.user_id == data["user_id"],
            UserRole.role_id == data["role_id"],
            UserRole.project_id == data.get("project_id")
        ).first()

        if existing:
            raise HTTPException(400, "Role already assigned to user")

        # Criar atribuicao
        expires_at = None
        if data.get("expires_at"):
            expires_at = datetime.fromisoformat(data["expires_at"])

        user_role = UserRole(
            user_id=data["user_id"],
            role_id=data["role_id"],
            project_id=data.get("project_id"),
            assigned_by=assigned_by,
            expires_at=expires_at
        )
        self.db.add(user_role)
        self.db.commit()
        self.db.refresh(user_role)

        # Issue #145: Limpar cache do usuario (todas as entradas)
        self._clear_user_cache(data["user_id"])

        return user_role.to_dict()

    def revoke_role(self, user_id: int, role_id: str, project_id: Optional[str] = None) -> bool:
        """Remove role de usuario"""
        from factory.database.models import UserRole

        query = self.db.query(UserRole).filter(
            UserRole.user_id == user_id,
            UserRole.role_id == role_id
        )

        if project_id:
            query = query.filter(UserRole.project_id == project_id)
        else:
            query = query.filter(UserRole.project_id == None)

        user_role = query.first()
        if not user_role:
            return False

        self.db.delete(user_role)
        self.db.commit()

        # Issue #145: Limpar todas as entradas de cache do usuario
        self._clear_user_cache(user_id)

        return True

    def _clear_user_cache(self, user_id: int):
        """
        Issue #145: Limpa todas as entradas de cache de um usuario.
        Remove entradas para todos os projetos (chaves user_id:*).
        """
        keys_to_remove = [
            key for key in self._permission_cache.keys()
            if key.startswith(f"{user_id}:")
        ]
        for key in keys_to_remove:
            del self._permission_cache[key]

    # -------------------------------------------------------------------------
    # Permission Checking
    # -------------------------------------------------------------------------

    def get_user_permissions(self, user_id: int, project_id: Optional[str] = None) -> List[str]:
        """
        Retorna todas as permissoes de um usuario.

        Issue #145: Cache agora usa chave composta user_id:project_id
        """
        # Issue #145: Usar chave composta para cache
        cache_key = f"{user_id}:{project_id or 'global'}"
        if cache_key in self._permission_cache:
            return self._permission_cache[cache_key]

        from factory.database.models import UserRole, Role

        # Buscar roles do usuario
        query = self.db.query(UserRole).filter(UserRole.user_id == user_id)

        if project_id:
            query = query.filter(
                (UserRole.project_id == None) | (UserRole.project_id == project_id)
            )

        user_roles = query.all()

        # Coletar permissoes unicas
        permissions = set()
        for ur in user_roles:
            if ur.is_valid() and ur.role:
                for perm in ur.role.permissions or []:
                    permissions.add(perm)

        result = list(permissions)
        # Issue #145: Salvar com chave composta
        self._permission_cache[cache_key] = result
        return result

    def check_permission(
        self,
        user_id: int,
        resource: str,
        action: str,
        project_id: Optional[str] = None
    ) -> bool:
        """
        Verifica se usuario tem permissao

        Args:
            user_id: ID do usuario
            resource: Recurso (stories, tasks, etc)
            action: Acao (create, read, update, delete)
            project_id: Escopo do projeto (opcional)

        Returns:
            True se tem permissao, False caso contrario
        """
        permissions = self.get_user_permissions(user_id, project_id)

        permission_key = f"{resource}:{action}"

        # Verificar permissao exata
        if permission_key in permissions:
            return True

        # Verificar wildcard de recurso (ex: stories:*)
        if f"{resource}:*" in permissions:
            return True

        # Verificar manage (inclui todas acoes)
        if f"{resource}:manage" in permissions:
            return True

        # Verificar wildcard total (admin)
        if "*:*" in permissions:
            return True

        return False

    def is_admin(self, user_id: int) -> bool:
        """Verifica se usuario e admin"""
        permissions = self.get_user_permissions(user_id)
        return "*:*" in permissions

    # -------------------------------------------------------------------------
    # Permission Matrix
    # -------------------------------------------------------------------------

    def get_permission_matrix(self) -> Dict[str, Dict[str, List[str]]]:
        """
        Retorna matriz de permissoes para UI
        {
            "ADMIN": {"stories": ["create", "read", "update", "delete"], ...},
            "MANAGER": {...},
            ...
        }
        """
        from factory.database.models import Role

        roles = self.db.query(Role).filter(Role.active == True).all()
        matrix = {}

        for role in roles:
            matrix[role.name] = {}
            for resource in RESOURCES:
                matrix[role.name][resource] = []
                for action in ACTIONS:
                    if role.has_permission(resource, action):
                        matrix[role.name][resource].append(action)

        return matrix

    # -------------------------------------------------------------------------
    # Audit Logging
    # -------------------------------------------------------------------------

    def log_action(
        self,
        user_id: Optional[int],
        username: Optional[str],
        action: str,
        resource: str,
        resource_id: Optional[str] = None,
        details: Optional[dict] = None,
        ip_address: Optional[str] = None,
        success: bool = True,
        error_message: Optional[str] = None
    ):
        """Registra acao no log de auditoria"""
        from factory.database.models import AuditLog

        log = AuditLog(
            user_id=user_id,
            username=username,
            action=action,
            resource=resource,
            resource_id=resource_id,
            details=details or {},
            ip_address=ip_address,
            success=success,
            error_message=error_message
        )
        self.db.add(log)
        self.db.commit()

    def get_audit_logs(
        self,
        user_id: Optional[int] = None,
        resource: Optional[str] = None,
        action: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict]:
        """Busca logs de auditoria"""
        from factory.database.models import AuditLog

        query = self.db.query(AuditLog)

        if user_id:
            query = query.filter(AuditLog.user_id == user_id)
        if resource:
            query = query.filter(AuditLog.resource == resource)
        if action:
            query = query.filter(AuditLog.action == action)

        logs = query.order_by(AuditLog.timestamp.desc()).limit(limit).all()
        return [log.to_dict() for log in logs]

    # -------------------------------------------------------------------------
    # Initialize Default Roles
    # -------------------------------------------------------------------------

    def init_default_roles(self):
        """Inicializa roles padrao no banco"""
        from factory.database.models import Role

        for role_name, role_data in DEFAULT_ROLES.items():
            existing = self.db.query(Role).filter(Role.role_id == role_data["role_id"]).first()
            if not existing:
                role = Role(
                    role_id=role_data["role_id"],
                    name=role_data["name"],
                    description=role_data["description"],
                    permissions=role_data["permissions"],
                    level=role_data["level"],
                    is_system=role_data["is_system"],
                    active=True
                )
                self.db.add(role)

        self.db.commit()
        print(f"[RBAC] Default roles initialized: {list(DEFAULT_ROLES.keys())}")


# =============================================================================
# DEPENDENCY INJECTION - Obter usuario atual
# =============================================================================

# Security
security = HTTPBearer(auto_error=False)


async def get_current_user(
    request: Request,
    credentials: Optional[HTTPAuthorizationCredentials] = Depends(security),
    x_user_id: Optional[str] = Header(None),
    x_username: Optional[str] = Header(None)
) -> UserContext:
    """
    Dependency para obter usuario atual da requisicao

    Suporta:
    - JWT Bearer Token (quando disponivel)
    - Headers X-User-Id e X-Username (para desenvolvimento)
    - Usuario anonimo (para endpoints publicos)
    """
    user_context = UserContext()

    # Tentar obter de headers (desenvolvimento)
    if x_user_id:
        try:
            user_context.user_id = int(x_user_id)
            user_context.username = x_username or f"user_{x_user_id}"
            user_context.is_authenticated = True

            # Buscar roles do usuario
            db = SessionLocal()
            try:
                rbac = RBACManager(db)
                permissions = rbac.get_user_permissions(user_context.user_id)
                user_context.permissions = permissions
                user_context.is_admin = rbac.is_admin(user_context.user_id)

                # Extrair nomes das roles
                user_roles = rbac.get_user_roles(user_context.user_id)
                user_context.roles = [ur["role"]["name"] for ur in user_roles if ur.get("role")]
            finally:
                db.close()

        except (ValueError, TypeError):
            pass

    # TODO: Implementar validacao de JWT quando disponivel
    # if credentials:
    #     token = credentials.credentials
    #     # Validar JWT e extrair user_id
    #     pass

    return user_context


def check_permission(resource: str, action: str, project_id: Optional[str] = None):
    """
    Dependency factory para verificar permissao

    Uso:
        @app.get("/api/stories")
        def list_stories(
            _: bool = Depends(check_permission("stories", "read"))
        ):
            ...
    """
    async def permission_checker(
        user: UserContext = Depends(get_current_user),
        request: Request = None
    ) -> bool:
        if not user.is_authenticated:
            raise HTTPException(401, "Authentication required")

        # Verificar permissao
        permission_key = f"{resource}:{action}"

        # Admin tem todas as permissoes
        if user.is_admin:
            return True

        # Verificar permissao exata
        if permission_key in user.permissions:
            return True

        # Verificar wildcard de recurso
        if f"{resource}:*" in user.permissions:
            return True

        # Verificar manage
        if f"{resource}:manage" in user.permissions:
            return True

        # Permissao negada
        raise HTTPException(
            403,
            f"Permission denied: {permission_key}"
        )

    return permission_checker


# =============================================================================
# DECORATORS - Para proteger funcoes/endpoints
# =============================================================================

def require_permission(resource: str, action: str):
    """
    Dependency para requerer permissao em endpoint (Issue #177: versao melhorada)

    Uso com FastAPI Depends:
        @app.post("/api/stories")
        def create_story(
            story: StoryCreate,
            user: UserContext = Depends(require_permission("stories", "create"))
        ):
            ...

    Retorna o UserContext se autorizado, ou levanta HTTPException.
    """
    async def permission_checker(
        user: UserContext = Depends(get_current_user)
    ) -> UserContext:
        if not user:
            raise HTTPException(401, "Authentication required")

        if not user.is_authenticated:
            raise HTTPException(401, "Authentication required")

        permission_key = f"{resource}:{action}"

        # Admin tem acesso total
        if user.is_admin:
            return user

        # Verificar permissao especifica
        if permission_key in user.permissions:
            return user

        # Verificar wildcard de recurso
        if f"{resource}:*" in user.permissions:
            return user

        # Verificar manage
        if f"{resource}:manage" in user.permissions:
            return user

        raise HTTPException(403, f"Permission denied: {permission_key}")

    return permission_checker


def require_role(role_name: str):
    """
    Decorator para requerer role especifica

    Uso:
        @app.delete("/api/users/{id}")
        @require_role("ADMIN")
        def delete_user(id: int, user: UserContext = Depends(get_current_user)):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = kwargs.get("user") or kwargs.get("current_user")

            if not user or not isinstance(user, UserContext):
                for arg in args:
                    if isinstance(arg, UserContext):
                        user = arg
                        break

            if not user or not user.is_authenticated:
                raise HTTPException(401, "Authentication required")

            if role_name not in user.roles and not user.is_admin:
                raise HTTPException(403, f"Role required: {role_name}")

            import asyncio
            return await func(*args, **kwargs) if asyncio.iscoroutinefunction(func) else func(*args, **kwargs)

        return wrapper
    return decorator


# =============================================================================
# API ROUTER - Endpoints RBAC
# =============================================================================

rbac_router = APIRouter(prefix="/api/rbac", tags=["RBAC"])


@rbac_router.get("/roles")
def list_roles():
    """Lista todas as roles disponiveis"""
    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        return rbac.get_all_roles()
    finally:
        db.close()


@rbac_router.get("/roles/{role_id}")
def get_role(role_id: str):
    """Busca role por ID"""
    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        role = rbac.get_role(role_id)
        if not role:
            raise HTTPException(404, "Role not found")
        return role
    finally:
        db.close()


@rbac_router.post("/roles")
def create_role(data: RoleCreate, user: UserContext = Depends(get_current_user), request: Request = None):
    """Cria nova role (requer ADMIN)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")
    if not user.is_admin:
        raise HTTPException(403, "Admin role required")

    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        result = rbac.create_role(data.dict())

        # Audit logging
        ip_address = request.client.host if request and request.client else None
        permission_audit_logger.log_role_created(
            actor_user_id=user.user_id,
            actor_username=user.username,
            role_data=result,
            ip_address=ip_address
        )

        return result
    finally:
        db.close()


@rbac_router.put("/roles/{role_id}")
def update_role(role_id: str, data: RoleUpdate, user: UserContext = Depends(get_current_user), request: Request = None):
    """Atualiza role (requer ADMIN)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")
    if not user.is_admin:
        raise HTTPException(403, "Admin role required")

    db = SessionLocal()
    try:
        rbac = RBACManager(db)

        # Capturar valor antigo para audit
        old_role = rbac.get_role(role_id)
        if not old_role:
            raise HTTPException(404, "Role not found")

        new_role = rbac.update_role(role_id, data.dict(exclude_none=True))
        if not new_role:
            raise HTTPException(404, "Role not found")

        # Audit logging
        ip_address = request.client.host if request and request.client else None
        permission_audit_logger.log_role_updated(
            actor_user_id=user.user_id,
            actor_username=user.username,
            role_id=role_id,
            old_data=old_role,
            new_data=new_role,
            ip_address=ip_address
        )

        return new_role
    finally:
        db.close()


@rbac_router.delete("/roles/{role_id}")
def delete_role(role_id: str, user: UserContext = Depends(get_current_user), request: Request = None):
    """Remove role (requer ADMIN)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")
    if not user.is_admin:
        raise HTTPException(403, "Admin role required")

    db = SessionLocal()
    try:
        rbac = RBACManager(db)

        # Capturar valor antigo para audit
        old_role = rbac.get_role(role_id)
        if not old_role:
            raise HTTPException(404, "Role not found")

        if rbac.delete_role(role_id):
            # Audit logging
            ip_address = request.client.host if request and request.client else None
            permission_audit_logger.log_role_deleted(
                actor_user_id=user.user_id,
                actor_username=user.username,
                role_id=role_id,
                role_data=old_role,
                ip_address=ip_address
            )
            return {"message": "Role deleted"}
        raise HTTPException(404, "Role not found")
    finally:
        db.close()


@rbac_router.get("/users/{user_id}/roles")
def get_user_roles(user_id: int, project_id: Optional[str] = None):
    """Lista roles de um usuario"""
    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        return rbac.get_user_roles(user_id, project_id)
    finally:
        db.close()


@rbac_router.post("/users/{user_id}/roles")
def assign_role_to_user(
    user_id: int,
    data: UserRoleAssign,
    user: UserContext = Depends(get_current_user),
    request: Request = None
):
    """Atribui role a usuario (requer ADMIN ou MANAGER)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")

    # ADMIN ou MANAGER podem atribuir roles
    if not user.is_admin and "MANAGER" not in user.roles:
        raise HTTPException(403, "Admin or Manager role required")

    data.user_id = user_id  # Garantir que user_id do path e usado

    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        result = rbac.assign_role(data.dict(), assigned_by=user.username)

        # Audit logging
        ip_address = request.client.host if request and request.client else None
        permission_audit_logger.log_role_assigned(
            actor_user_id=user.user_id,
            actor_username=user.username,
            role_id=data.role_id,
            affected_user_id=user_id,
            assignment_data=result,
            ip_address=ip_address
        )

        return result
    finally:
        db.close()


@rbac_router.delete("/users/{user_id}/roles/{role_id}")
def revoke_role_from_user(
    user_id: int,
    role_id: str,
    project_id: Optional[str] = None,
    user: UserContext = Depends(get_current_user),
    request: Request = None
):
    """Remove role de usuario (requer ADMIN)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")
    if not user.is_admin:
        raise HTTPException(403, "Admin role required")

    db = SessionLocal()
    try:
        rbac = RBACManager(db)

        # Capturar dados para audit antes de revogar
        user_roles = rbac.get_user_roles(user_id, project_id)
        revoked_role_data = next(
            (ur for ur in user_roles if ur.get("role", {}).get("role_id") == role_id),
            None
        )

        if rbac.revoke_role(user_id, role_id, project_id):
            # Audit logging
            ip_address = request.client.host if request and request.client else None
            permission_audit_logger.log_role_revoked(
                actor_user_id=user.user_id,
                actor_username=user.username,
                role_id=role_id,
                affected_user_id=user_id,
                revocation_data=revoked_role_data,
                ip_address=ip_address
            )
            return {"message": "Role revoked"}
        raise HTTPException(404, "User role not found")
    finally:
        db.close()


@rbac_router.get("/permissions")
def list_permissions():
    """Lista todos os recursos e acoes disponiveis"""
    permissions = []
    for resource in RESOURCES:
        for action in ACTIONS:
            permissions.append({
                "permission_id": f"{resource}:{action}",
                "resource": resource,
                "action": action
            })
    return {
        "resources": RESOURCES,
        "actions": ACTIONS,
        "permissions": permissions
    }


@rbac_router.get("/permissions/matrix")
def get_permission_matrix():
    """Retorna matriz de permissoes por role"""
    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        return rbac.get_permission_matrix()
    finally:
        db.close()


@rbac_router.post("/permissions/check")
def check_user_permission(
    data: PermissionCheck,
    user: UserContext = Depends(get_current_user)
):
    """Verifica se usuario atual tem permissao"""
    if not user.is_authenticated:
        return {"has_permission": False, "reason": "Not authenticated"}

    permission_key = f"{data.resource}:{data.action}"

    if user.is_admin:
        return {"has_permission": True, "reason": "Admin"}

    if permission_key in user.permissions:
        return {"has_permission": True, "reason": "Direct permission"}

    if f"{data.resource}:*" in user.permissions:
        return {"has_permission": True, "reason": "Resource wildcard"}

    if f"{data.resource}:manage" in user.permissions:
        return {"has_permission": True, "reason": "Manage permission"}

    return {"has_permission": False, "reason": "Permission denied"}


@rbac_router.get("/audit")
def get_audit_logs(
    user_id: Optional[int] = None,
    resource: Optional[str] = None,
    action: Optional[str] = None,
    limit: int = 100,
    user: UserContext = Depends(get_current_user)
):
    """Lista logs de auditoria (requer ADMIN ou audit:read)"""
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")

    if not user.is_admin and "audit:read" not in user.permissions:
        raise HTTPException(403, "Permission denied: audit:read")

    db = SessionLocal()
    try:
        rbac = RBACManager(db)
        return rbac.get_audit_logs(user_id, resource, action, limit)
    finally:
        db.close()


@rbac_router.get("/audit/permissions")
def get_permission_audit_logs(
    actor_user_id: Optional[int] = Query(None, description="Filtrar por quem fez a acao"),
    affected_user_id: Optional[int] = Query(None, description="Filtrar por usuario afetado"),
    resource_type: Optional[str] = Query(None, description="Tipo de recurso (role, permission, user_role)"),
    action: Optional[str] = Query(None, description="Tipo de acao (CREATE, UPDATE, DELETE, ASSIGN, REVOKE)"),
    start_date: Optional[str] = Query(None, description="Data inicial (ISO format)"),
    end_date: Optional[str] = Query(None, description="Data final (ISO format)"),
    limit: int = Query(100, ge=1, le=1000, description="Limite de resultados"),
    offset: int = Query(0, ge=0, description="Offset para paginacao"),
    user: UserContext = Depends(get_current_user)
):
    """
    Lista logs de auditoria especificos de mudancas de permissoes/roles.

    Retorna historico detalhado de:
    - Criacao, atualizacao e delecao de roles
    - Atribuicao e revogacao de roles a usuarios
    - Quem fez cada mudanca, quando, e o que mudou

    Requer: ADMIN ou permissao audit:read
    """
    if not user.is_authenticated:
        raise HTTPException(401, "Authentication required")

    if not user.is_admin and "audit:read" not in user.permissions:
        raise HTTPException(403, "Permission denied: audit:read")

    # Converter datas se fornecidas
    parsed_start_date = None
    parsed_end_date = None
    if start_date:
        try:
            parsed_start_date = datetime.fromisoformat(start_date)
        except ValueError:
            raise HTTPException(400, "Invalid start_date format. Use ISO format (YYYY-MM-DDTHH:MM:SS)")
    if end_date:
        try:
            parsed_end_date = datetime.fromisoformat(end_date)
        except ValueError:
            raise HTTPException(400, "Invalid end_date format. Use ISO format (YYYY-MM-DDTHH:MM:SS)")

    logs = permission_audit_logger.get_permission_audit_logs(
        actor_user_id=actor_user_id,
        affected_user_id=affected_user_id,
        resource_type=resource_type,
        action=action,
        start_date=parsed_start_date,
        end_date=parsed_end_date,
        limit=limit,
        offset=offset
    )

    return {
        "total": len(logs),
        "limit": limit,
        "offset": offset,
        "logs": logs
    }


@rbac_router.post("/init")
def initialize_default_roles(user: UserContext = Depends(get_current_user)):
    """Inicializa roles padrao (requer ADMIN ou primeira execucao)"""
    db = SessionLocal()
    try:
        rbac = RBACManager(db)

        # Se nao ha roles, permitir inicializacao
        roles = rbac.get_all_roles()
        if len(roles) == 0:
            rbac.init_default_roles()
            return {"message": "Default roles initialized", "roles": list(DEFAULT_ROLES.keys())}

        # Se ha roles, requer admin
        if not user.is_authenticated or not user.is_admin:
            raise HTTPException(403, "Admin role required")

        rbac.init_default_roles()
        return {"message": "Default roles re-initialized", "roles": list(DEFAULT_ROLES.keys())}
    finally:
        db.close()
