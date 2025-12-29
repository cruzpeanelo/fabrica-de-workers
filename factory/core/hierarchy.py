# -*- coding: utf-8 -*-
"""
Hierarquia Tenant > Projeto > Acesso (Issue #120)
===================================================

Implementa:
- Modelo de hierarquia: Tenant > Projects > Resources
- Permissoes em cada nivel
- Heranca de permissoes
- Audit de acesso
- Controle de acesso baseado em roles (RBAC)
- Controle de acesso baseado em atributos (ABAC)

Arquitetura:
    Tenant (Organizacao)
    └── Projects (Projetos)
        └── Resources (Stories, Tasks, Agents, etc)
            └── Actions (create, read, update, delete)

Autor: Fabrica de Agentes
"""

import uuid
import json
import logging
from datetime import datetime
from typing import Optional, Dict, Any, List, Set, Tuple
from enum import Enum
from dataclasses import dataclass, field, asdict
from functools import wraps

from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean
from sqlalchemy.orm import Session

# Configurar logging
logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS
# =============================================================================

class ResourceType(str, Enum):
    """Tipos de recursos no sistema"""
    TENANT = "tenant"
    PROJECT = "project"
    STORY = "story"
    TASK = "task"
    AGENT = "agent"
    WORKER = "worker"
    JOB = "job"
    DOCUMENT = "document"
    FILE = "file"
    CHAT = "chat"
    REPORT = "report"
    SETTINGS = "settings"
    BILLING = "billing"
    TEAM = "team"
    INVITE = "invite"


class Action(str, Enum):
    """Acoes possiveis sobre recursos"""
    CREATE = "create"
    READ = "read"
    UPDATE = "update"
    DELETE = "delete"
    EXECUTE = "execute"
    APPROVE = "approve"
    ASSIGN = "assign"
    EXPORT = "export"
    IMPORT = "import"
    SHARE = "share"
    MANAGE = "manage"  # Gerenciar permissoes
    ADMIN = "admin"    # Acesso total


class Role(str, Enum):
    """Roles no sistema"""
    # Roles de Tenant
    OWNER = "owner"           # Dono do tenant - acesso total
    ADMIN = "admin"           # Administrador - gerencia usuarios e configuracoes
    MANAGER = "manager"       # Gerente - gerencia projetos e equipes
    DEVELOPER = "developer"   # Desenvolvedor - cria e edita recursos
    VIEWER = "viewer"         # Visualizador - apenas leitura
    GUEST = "guest"           # Convidado - acesso limitado

    # Roles de Projeto
    PROJECT_OWNER = "project_owner"
    PROJECT_ADMIN = "project_admin"
    PROJECT_MEMBER = "project_member"
    PROJECT_VIEWER = "project_viewer"


class PermissionScope(str, Enum):
    """Escopo de permissao"""
    GLOBAL = "global"     # Aplica a todo o tenant
    PROJECT = "project"   # Aplica a um projeto especifico
    RESOURCE = "resource" # Aplica a um recurso especifico


class AuditAction(str, Enum):
    """Acoes auditaveis"""
    ACCESS_GRANTED = "access_granted"
    ACCESS_DENIED = "access_denied"
    PERMISSION_CHANGED = "permission_changed"
    ROLE_ASSIGNED = "role_assigned"
    ROLE_REMOVED = "role_removed"
    RESOURCE_CREATED = "resource_created"
    RESOURCE_UPDATED = "resource_updated"
    RESOURCE_DELETED = "resource_deleted"
    LOGIN = "login"
    LOGOUT = "logout"


# =============================================================================
# PERMISSION DEFINITIONS
# =============================================================================

# Mapeamento de roles para permissoes padrao
DEFAULT_ROLE_PERMISSIONS: Dict[str, Dict[str, List[str]]] = {
    Role.OWNER.value: {
        "*": ["*"],  # Acesso total a tudo
    },
    Role.ADMIN.value: {
        ResourceType.TENANT.value: [Action.READ.value, Action.UPDATE.value, Action.MANAGE.value],
        ResourceType.PROJECT.value: ["*"],
        ResourceType.STORY.value: ["*"],
        ResourceType.TASK.value: ["*"],
        ResourceType.AGENT.value: ["*"],
        ResourceType.WORKER.value: ["*"],
        ResourceType.JOB.value: ["*"],
        ResourceType.DOCUMENT.value: ["*"],
        ResourceType.TEAM.value: ["*"],
        ResourceType.INVITE.value: ["*"],
        ResourceType.SETTINGS.value: [Action.READ.value, Action.UPDATE.value],
        ResourceType.BILLING.value: [Action.READ.value],
    },
    Role.MANAGER.value: {
        ResourceType.PROJECT.value: [Action.CREATE.value, Action.READ.value, Action.UPDATE.value, Action.ASSIGN.value],
        ResourceType.STORY.value: ["*"],
        ResourceType.TASK.value: ["*"],
        ResourceType.AGENT.value: [Action.READ.value, Action.EXECUTE.value],
        ResourceType.WORKER.value: [Action.READ.value, Action.EXECUTE.value],
        ResourceType.JOB.value: [Action.READ.value, Action.CREATE.value, Action.EXECUTE.value],
        ResourceType.DOCUMENT.value: ["*"],
        ResourceType.TEAM.value: [Action.READ.value, Action.ASSIGN.value],
        ResourceType.REPORT.value: [Action.READ.value, Action.EXPORT.value],
    },
    Role.DEVELOPER.value: {
        ResourceType.PROJECT.value: [Action.READ.value, Action.UPDATE.value],
        ResourceType.STORY.value: [Action.READ.value, Action.UPDATE.value],
        ResourceType.TASK.value: ["*"],
        ResourceType.AGENT.value: [Action.READ.value, Action.EXECUTE.value],
        ResourceType.WORKER.value: [Action.READ.value],
        ResourceType.JOB.value: [Action.READ.value, Action.CREATE.value],
        ResourceType.DOCUMENT.value: [Action.CREATE.value, Action.READ.value, Action.UPDATE.value],
        ResourceType.FILE.value: [Action.CREATE.value, Action.READ.value, Action.DELETE.value],
        ResourceType.CHAT.value: [Action.READ.value, Action.CREATE.value],
    },
    Role.VIEWER.value: {
        ResourceType.PROJECT.value: [Action.READ.value],
        ResourceType.STORY.value: [Action.READ.value],
        ResourceType.TASK.value: [Action.READ.value],
        ResourceType.DOCUMENT.value: [Action.READ.value],
        ResourceType.REPORT.value: [Action.READ.value],
    },
    Role.GUEST.value: {
        ResourceType.PROJECT.value: [Action.READ.value],
        ResourceType.STORY.value: [Action.READ.value],
    },
}


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class Permission:
    """Representa uma permissao"""
    resource_type: str
    action: str
    scope: str = PermissionScope.GLOBAL.value
    resource_id: Optional[str] = None
    conditions: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)

    def matches(self, resource_type: str, action: str, resource_id: Optional[str] = None) -> bool:
        """Verifica se a permissao corresponde"""
        # Wildcard match
        if self.resource_type == "*" or self.action == "*":
            return True

        # Resource type match
        if self.resource_type != resource_type:
            return False

        # Action match
        if self.action != action and self.action != "*":
            return False

        # Resource ID match (se especificado)
        if self.resource_id and resource_id and self.resource_id != resource_id:
            return False

        return True


@dataclass
class RoleAssignment:
    """Atribuicao de role a usuario"""
    user_id: str
    role: str
    tenant_id: str
    project_id: Optional[str] = None  # None = role global no tenant
    assigned_by: Optional[str] = None
    assigned_at: datetime = field(default_factory=datetime.utcnow)
    expires_at: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "user_id": self.user_id,
            "role": self.role,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "assigned_by": self.assigned_by,
            "assigned_at": self.assigned_at.isoformat() if self.assigned_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "metadata": self.metadata,
        }

    def is_active(self) -> bool:
        """Verifica se a atribuicao esta ativa"""
        if self.expires_at and datetime.utcnow() > self.expires_at:
            return False
        return True


@dataclass
class AccessRequest:
    """Requisicao de acesso a ser avaliada"""
    user_id: str
    tenant_id: str
    resource_type: str
    action: str
    resource_id: Optional[str] = None
    project_id: Optional[str] = None
    context: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class AccessDecision:
    """Resultado da avaliacao de acesso"""
    allowed: bool
    reason: str
    matched_permission: Optional[Permission] = None
    matched_role: Optional[str] = None
    evaluated_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "allowed": self.allowed,
            "reason": self.reason,
            "matched_permission": self.matched_permission.to_dict() if self.matched_permission else None,
            "matched_role": self.matched_role,
            "evaluated_at": self.evaluated_at.isoformat(),
        }


@dataclass
class AuditEntry:
    """Entrada de auditoria"""
    audit_id: str
    tenant_id: str
    user_id: str
    action: str
    resource_type: str
    resource_id: Optional[str] = None
    project_id: Optional[str] = None
    details: Dict[str, Any] = field(default_factory=dict)
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    timestamp: datetime = field(default_factory=datetime.utcnow)
    success: bool = True

    def to_dict(self) -> Dict[str, Any]:
        return {
            "audit_id": self.audit_id,
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "action": self.action,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "project_id": self.project_id,
            "details": self.details,
            "ip_address": self.ip_address,
            "user_agent": self.user_agent,
            "timestamp": self.timestamp.isoformat(),
            "success": self.success,
        }


# =============================================================================
# HIERARCHY SERVICE
# =============================================================================

class HierarchyService:
    """
    Servico de gerenciamento de hierarquia e permissoes.

    Responsabilidades:
    - Gerenciar hierarquia Tenant > Project > Resource
    - Avaliar permissoes de acesso
    - Atribuir e revogar roles
    - Registrar auditoria de acessos
    - Heranca de permissoes

    Uso:
        service = HierarchyService(db_session)

        # Verificar acesso
        if service.can_access(user_id, tenant_id, "project", "create"):
            # Permitir acao

        # Atribuir role
        service.assign_role(user_id, tenant_id, Role.DEVELOPER)

        # Listar permissoes
        permissions = service.get_user_permissions(user_id, tenant_id)
    """

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self._role_permissions_cache: Dict[str, Dict[str, List[str]]] = {}
        self._user_roles_cache: Dict[str, List[RoleAssignment]] = {}

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    def _cache_key(self, user_id: str, tenant_id: str, project_id: Optional[str] = None) -> str:
        """Gera chave de cache"""
        return f"{tenant_id}:{project_id or 'global'}:{user_id}"

    def _clear_user_cache(self, user_id: str, tenant_id: str):
        """Limpa cache do usuario"""
        for key in list(self._user_roles_cache.keys()):
            if key.endswith(f":{user_id}") and key.startswith(tenant_id):
                del self._user_roles_cache[key]

    # =========================================================================
    # ROLE MANAGEMENT
    # =========================================================================

    def assign_role(
        self,
        user_id: str,
        tenant_id: str,
        role: str,
        project_id: Optional[str] = None,
        assigned_by: Optional[str] = None,
        expires_at: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> RoleAssignment:
        """
        Atribui uma role a um usuario.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            role: Role a atribuir
            project_id: ID do projeto (None = role global)
            assigned_by: ID do usuario que atribuiu
            expires_at: Data de expiracao
            metadata: Metadados adicionais

        Returns:
            RoleAssignment criado
        """
        assignment = RoleAssignment(
            user_id=user_id,
            role=role,
            tenant_id=tenant_id,
            project_id=project_id,
            assigned_by=assigned_by,
            expires_at=expires_at,
            metadata=metadata or {},
        )

        # Salvar no banco
        try:
            self.db.execute(
                """INSERT INTO user_roles
                   (user_id, tenant_id, project_id, role, assigned_by, assigned_at, expires_at, metadata)
                   VALUES (:user_id, :tenant_id, :project_id, :role, :assigned_by, :assigned_at, :expires_at, :metadata)
                   ON CONFLICT (user_id, tenant_id, COALESCE(project_id, '')) DO UPDATE
                   SET role = :role, assigned_by = :assigned_by, assigned_at = :assigned_at,
                       expires_at = :expires_at, metadata = :metadata""",
                {
                    "user_id": user_id,
                    "tenant_id": tenant_id,
                    "project_id": project_id,
                    "role": role,
                    "assigned_by": assigned_by,
                    "assigned_at": datetime.utcnow(),
                    "expires_at": expires_at,
                    "metadata": json.dumps(metadata or {}),
                }
            )
            self.db.commit()

            # Limpar cache
            self._clear_user_cache(user_id, tenant_id)

            # Registrar auditoria
            self._audit(
                tenant_id=tenant_id,
                user_id=assigned_by or "system",
                action=AuditAction.ROLE_ASSIGNED.value,
                resource_type=ResourceType.TEAM.value,
                resource_id=user_id,
                details={
                    "role": role,
                    "project_id": project_id,
                    "target_user_id": user_id,
                }
            )

            logger.info(f"Role {role} atribuida ao usuario {user_id} no tenant {tenant_id}")

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao atribuir role: {e}")
            raise

        return assignment

    def revoke_role(
        self,
        user_id: str,
        tenant_id: str,
        role: Optional[str] = None,
        project_id: Optional[str] = None,
        revoked_by: Optional[str] = None
    ) -> bool:
        """
        Revoga role de um usuario.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            role: Role especifica a revogar (None = todas)
            project_id: ID do projeto
            revoked_by: ID do usuario que revogou

        Returns:
            True se revogou alguma role
        """
        try:
            query = "DELETE FROM user_roles WHERE user_id = :user_id AND tenant_id = :tenant_id"
            params = {"user_id": user_id, "tenant_id": tenant_id}

            if role:
                query += " AND role = :role"
                params["role"] = role

            if project_id:
                query += " AND project_id = :project_id"
                params["project_id"] = project_id

            result = self.db.execute(query, params)
            self.db.commit()

            # Limpar cache
            self._clear_user_cache(user_id, tenant_id)

            # Registrar auditoria
            self._audit(
                tenant_id=tenant_id,
                user_id=revoked_by or "system",
                action=AuditAction.ROLE_REMOVED.value,
                resource_type=ResourceType.TEAM.value,
                resource_id=user_id,
                details={
                    "role": role,
                    "project_id": project_id,
                    "target_user_id": user_id,
                }
            )

            logger.info(f"Role {role or 'all'} revogada do usuario {user_id}")
            return result.rowcount > 0

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao revogar role: {e}")
            raise

    def get_user_roles(
        self,
        user_id: str,
        tenant_id: str,
        project_id: Optional[str] = None,
        include_expired: bool = False
    ) -> List[RoleAssignment]:
        """
        Obtem roles do usuario.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            project_id: ID do projeto (None = todas)
            include_expired: Incluir roles expiradas

        Returns:
            Lista de RoleAssignment
        """
        cache_key = self._cache_key(user_id, tenant_id, project_id)

        if cache_key in self._user_roles_cache:
            assignments = self._user_roles_cache[cache_key]
            if not include_expired:
                assignments = [a for a in assignments if a.is_active()]
            return assignments

        try:
            query = """SELECT user_id, tenant_id, project_id, role, assigned_by, assigned_at, expires_at, metadata
                       FROM user_roles
                       WHERE user_id = :user_id AND tenant_id = :tenant_id"""
            params = {"user_id": user_id, "tenant_id": tenant_id}

            if project_id:
                query += " AND (project_id = :project_id OR project_id IS NULL)"
                params["project_id"] = project_id

            if not include_expired:
                query += " AND (expires_at IS NULL OR expires_at > :now)"
                params["now"] = datetime.utcnow()

            results = self.db.execute(query, params).fetchall()

            assignments = []
            for row in results:
                metadata = json.loads(row[7]) if row[7] else {}
                assignments.append(RoleAssignment(
                    user_id=row[0],
                    tenant_id=row[1],
                    project_id=row[2],
                    role=row[3],
                    assigned_by=row[4],
                    assigned_at=row[5],
                    expires_at=row[6],
                    metadata=metadata,
                ))

            # Cachear resultado
            self._user_roles_cache[cache_key] = assignments

            return assignments

        except Exception as e:
            logger.error(f"Erro ao buscar roles: {e}")
            return []

    # =========================================================================
    # PERMISSION EVALUATION
    # =========================================================================

    def can_access(
        self,
        user_id: str,
        tenant_id: str,
        resource_type: str,
        action: str,
        resource_id: Optional[str] = None,
        project_id: Optional[str] = None,
        context: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Verifica se usuario pode realizar acao.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            resource_type: Tipo do recurso
            action: Acao desejada
            resource_id: ID do recurso especifico
            project_id: ID do projeto
            context: Contexto adicional

        Returns:
            True se permitido
        """
        decision = self.evaluate_access(AccessRequest(
            user_id=user_id,
            tenant_id=tenant_id,
            resource_type=resource_type,
            action=action,
            resource_id=resource_id,
            project_id=project_id,
            context=context or {},
        ))

        return decision.allowed

    def evaluate_access(self, request: AccessRequest) -> AccessDecision:
        """
        Avalia requisicao de acesso.

        Args:
            request: Requisicao de acesso

        Returns:
            AccessDecision com resultado
        """
        # Obter roles do usuario
        roles = self.get_user_roles(
            request.user_id,
            request.tenant_id,
            request.project_id
        )

        if not roles:
            decision = AccessDecision(
                allowed=False,
                reason="Usuario nao possui roles no tenant"
            )
            self._audit_access(request, decision)
            return decision

        # Avaliar cada role
        for assignment in roles:
            if not assignment.is_active():
                continue

            # Obter permissoes da role
            permissions = self._get_role_permissions(assignment.role)

            # Verificar se alguma permissao permite o acesso
            for resource, actions in permissions.items():
                # Wildcard de recurso
                if resource == "*" or resource == request.resource_type:
                    # Wildcard de acao
                    if "*" in actions or request.action in actions:
                        permission = Permission(
                            resource_type=resource,
                            action=request.action,
                            scope=PermissionScope.PROJECT.value if assignment.project_id else PermissionScope.GLOBAL.value,
                            resource_id=request.resource_id,
                        )

                        decision = AccessDecision(
                            allowed=True,
                            reason=f"Permitido pela role {assignment.role}",
                            matched_permission=permission,
                            matched_role=assignment.role,
                        )
                        self._audit_access(request, decision)
                        return decision

        # Nenhuma permissao correspondente
        decision = AccessDecision(
            allowed=False,
            reason=f"Nenhuma permissao para {request.action} em {request.resource_type}"
        )
        self._audit_access(request, decision)
        return decision

    def _get_role_permissions(self, role: str) -> Dict[str, List[str]]:
        """Obtem permissoes de uma role"""
        if role in self._role_permissions_cache:
            return self._role_permissions_cache[role]

        # Buscar permissoes customizadas do banco
        try:
            result = self.db.execute(
                "SELECT permissions FROM role_permissions WHERE role = :role",
                {"role": role}
            ).fetchone()

            if result and result[0]:
                permissions = json.loads(result[0]) if isinstance(result[0], str) else result[0]
                self._role_permissions_cache[role] = permissions
                return permissions

        except Exception as e:
            logger.debug(f"Permissoes customizadas nao encontradas para {role}: {e}")

        # Usar permissoes padrao
        permissions = DEFAULT_ROLE_PERMISSIONS.get(role, {})
        self._role_permissions_cache[role] = permissions
        return permissions

    def get_user_permissions(
        self,
        user_id: str,
        tenant_id: str,
        project_id: Optional[str] = None
    ) -> Dict[str, List[str]]:
        """
        Obtem todas as permissoes efetivas do usuario.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            project_id: ID do projeto

        Returns:
            Dict com recursos e acoes permitidas
        """
        roles = self.get_user_roles(user_id, tenant_id, project_id)
        effective_permissions: Dict[str, Set[str]] = {}

        for assignment in roles:
            if not assignment.is_active():
                continue

            permissions = self._get_role_permissions(assignment.role)

            for resource, actions in permissions.items():
                if resource not in effective_permissions:
                    effective_permissions[resource] = set()

                if "*" in actions:
                    effective_permissions[resource].add("*")
                else:
                    effective_permissions[resource].update(actions)

        # Converter sets para listas
        return {k: list(v) for k, v in effective_permissions.items()}

    # =========================================================================
    # HIERARCHY NAVIGATION
    # =========================================================================

    def get_tenant_projects(
        self,
        tenant_id: str,
        user_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista projetos do tenant (filtrado por acesso do usuario).

        Args:
            tenant_id: ID do tenant
            user_id: ID do usuario (filtra projetos acessiveis)

        Returns:
            Lista de projetos
        """
        try:
            if user_id:
                # Verificar se usuario tem acesso global
                roles = self.get_user_roles(user_id, tenant_id)
                global_roles = [r for r in roles if r.project_id is None]

                if any(r.role in [Role.OWNER.value, Role.ADMIN.value, Role.MANAGER.value] for r in global_roles):
                    # Acesso a todos os projetos
                    query = "SELECT project_id, name, description, created_at FROM projects WHERE tenant_id = :tenant_id"
                    results = self.db.execute(query, {"tenant_id": tenant_id}).fetchall()
                else:
                    # Apenas projetos com role atribuida
                    project_ids = [r.project_id for r in roles if r.project_id]
                    if not project_ids:
                        return []

                    query = """SELECT project_id, name, description, created_at
                               FROM projects
                               WHERE tenant_id = :tenant_id AND project_id IN :project_ids"""
                    results = self.db.execute(query, {
                        "tenant_id": tenant_id,
                        "project_ids": tuple(project_ids)
                    }).fetchall()
            else:
                query = "SELECT project_id, name, description, created_at FROM projects WHERE tenant_id = :tenant_id"
                results = self.db.execute(query, {"tenant_id": tenant_id}).fetchall()

            return [
                {
                    "project_id": row[0],
                    "name": row[1],
                    "description": row[2],
                    "created_at": row[3].isoformat() if row[3] else None,
                }
                for row in results
            ]

        except Exception as e:
            logger.error(f"Erro ao listar projetos: {e}")
            return []

    def get_project_members(
        self,
        tenant_id: str,
        project_id: str
    ) -> List[Dict[str, Any]]:
        """
        Lista membros de um projeto.

        Args:
            tenant_id: ID do tenant
            project_id: ID do projeto

        Returns:
            Lista de membros com roles
        """
        try:
            # Membros com role global ou especifica do projeto
            query = """SELECT ur.user_id, ur.role, ur.assigned_at, u.name, u.email
                       FROM user_roles ur
                       LEFT JOIN users u ON u.id = ur.user_id
                       WHERE ur.tenant_id = :tenant_id
                       AND (ur.project_id = :project_id OR ur.project_id IS NULL)"""

            results = self.db.execute(query, {
                "tenant_id": tenant_id,
                "project_id": project_id
            }).fetchall()

            members = {}
            for row in results:
                user_id = row[0]
                if user_id not in members:
                    members[user_id] = {
                        "user_id": user_id,
                        "name": row[3],
                        "email": row[4],
                        "roles": [],
                        "assigned_at": row[2].isoformat() if row[2] else None,
                    }
                members[user_id]["roles"].append(row[1])

            return list(members.values())

        except Exception as e:
            logger.error(f"Erro ao listar membros do projeto: {e}")
            return []

    def get_resource_hierarchy(
        self,
        tenant_id: str,
        resource_type: str,
        resource_id: str
    ) -> Dict[str, Any]:
        """
        Obtem hierarquia completa de um recurso.

        Args:
            tenant_id: ID do tenant
            resource_type: Tipo do recurso
            resource_id: ID do recurso

        Returns:
            Dict com hierarquia (tenant -> project -> resource)
        """
        hierarchy = {
            "tenant_id": tenant_id,
            "resource_type": resource_type,
            "resource_id": resource_id,
            "path": [],
        }

        try:
            # Determinar projeto pai baseado no tipo de recurso
            project_id = None

            if resource_type in [ResourceType.STORY.value, ResourceType.TASK.value]:
                result = self.db.execute(
                    f"SELECT project_id FROM {resource_type}s WHERE {resource_type}_id = :id",
                    {"id": resource_id}
                ).fetchone()
                if result:
                    project_id = result[0]

            elif resource_type == ResourceType.PROJECT.value:
                project_id = resource_id

            if project_id:
                hierarchy["project_id"] = project_id
                hierarchy["path"] = [tenant_id, project_id, resource_id]
            else:
                hierarchy["path"] = [tenant_id, resource_id]

        except Exception as e:
            logger.error(f"Erro ao obter hierarquia: {e}")

        return hierarchy

    # =========================================================================
    # PERMISSION INHERITANCE
    # =========================================================================

    def check_inherited_permission(
        self,
        user_id: str,
        tenant_id: str,
        resource_type: str,
        resource_id: str,
        action: str
    ) -> AccessDecision:
        """
        Verifica permissao considerando heranca.

        Ordem de verificacao:
        1. Permissao especifica no recurso
        2. Permissao no projeto pai
        3. Permissao global no tenant

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            resource_type: Tipo do recurso
            resource_id: ID do recurso
            action: Acao desejada

        Returns:
            AccessDecision
        """
        hierarchy = self.get_resource_hierarchy(tenant_id, resource_type, resource_id)
        project_id = hierarchy.get("project_id")

        # 1. Verificar permissao especifica no projeto
        if project_id:
            decision = self.evaluate_access(AccessRequest(
                user_id=user_id,
                tenant_id=tenant_id,
                resource_type=resource_type,
                action=action,
                resource_id=resource_id,
                project_id=project_id,
            ))

            if decision.allowed:
                return decision

        # 2. Verificar permissao global no tenant
        return self.evaluate_access(AccessRequest(
            user_id=user_id,
            tenant_id=tenant_id,
            resource_type=resource_type,
            action=action,
            resource_id=resource_id,
        ))

    # =========================================================================
    # CUSTOM PERMISSIONS
    # =========================================================================

    def grant_permission(
        self,
        user_id: str,
        tenant_id: str,
        resource_type: str,
        action: str,
        resource_id: Optional[str] = None,
        project_id: Optional[str] = None,
        granted_by: Optional[str] = None
    ) -> bool:
        """
        Concede permissao especifica a um usuario.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            resource_type: Tipo do recurso
            action: Acao a permitir
            resource_id: ID do recurso especifico
            project_id: ID do projeto
            granted_by: ID do usuario que concedeu

        Returns:
            True se concedeu
        """
        try:
            self.db.execute(
                """INSERT INTO user_permissions
                   (user_id, tenant_id, resource_type, action, resource_id, project_id, granted_by, granted_at)
                   VALUES (:user_id, :tenant_id, :resource_type, :action, :resource_id, :project_id, :granted_by, :granted_at)""",
                {
                    "user_id": user_id,
                    "tenant_id": tenant_id,
                    "resource_type": resource_type,
                    "action": action,
                    "resource_id": resource_id,
                    "project_id": project_id,
                    "granted_by": granted_by,
                    "granted_at": datetime.utcnow(),
                }
            )
            self.db.commit()

            # Registrar auditoria
            self._audit(
                tenant_id=tenant_id,
                user_id=granted_by or "system",
                action=AuditAction.PERMISSION_CHANGED.value,
                resource_type=resource_type,
                resource_id=resource_id,
                details={
                    "target_user_id": user_id,
                    "permission": action,
                    "granted": True,
                }
            )

            logger.info(f"Permissao {action} em {resource_type} concedida a {user_id}")
            return True

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao conceder permissao: {e}")
            raise

    def revoke_permission(
        self,
        user_id: str,
        tenant_id: str,
        resource_type: str,
        action: str,
        resource_id: Optional[str] = None,
        revoked_by: Optional[str] = None
    ) -> bool:
        """
        Revoga permissao especifica.

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant
            resource_type: Tipo do recurso
            action: Acao a revogar
            resource_id: ID do recurso
            revoked_by: ID do usuario que revogou

        Returns:
            True se revogou
        """
        try:
            query = """DELETE FROM user_permissions
                       WHERE user_id = :user_id AND tenant_id = :tenant_id
                       AND resource_type = :resource_type AND action = :action"""
            params = {
                "user_id": user_id,
                "tenant_id": tenant_id,
                "resource_type": resource_type,
                "action": action,
            }

            if resource_id:
                query += " AND resource_id = :resource_id"
                params["resource_id"] = resource_id

            result = self.db.execute(query, params)
            self.db.commit()

            # Registrar auditoria
            self._audit(
                tenant_id=tenant_id,
                user_id=revoked_by or "system",
                action=AuditAction.PERMISSION_CHANGED.value,
                resource_type=resource_type,
                resource_id=resource_id,
                details={
                    "target_user_id": user_id,
                    "permission": action,
                    "granted": False,
                }
            )

            return result.rowcount > 0

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao revogar permissao: {e}")
            raise

    # =========================================================================
    # AUDIT
    # =========================================================================

    def _audit(
        self,
        tenant_id: str,
        user_id: str,
        action: str,
        resource_type: str,
        resource_id: Optional[str] = None,
        project_id: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        success: bool = True
    ):
        """Registra entrada de auditoria"""
        try:
            audit_id = self._generate_id("AUD")

            self.db.execute(
                """INSERT INTO audit_log
                   (audit_id, tenant_id, user_id, action, resource_type, resource_id, project_id,
                    details, ip_address, user_agent, timestamp, success)
                   VALUES (:audit_id, :tenant_id, :user_id, :action, :resource_type, :resource_id,
                           :project_id, :details, :ip_address, :user_agent, :timestamp, :success)""",
                {
                    "audit_id": audit_id,
                    "tenant_id": tenant_id,
                    "user_id": user_id,
                    "action": action,
                    "resource_type": resource_type,
                    "resource_id": resource_id,
                    "project_id": project_id,
                    "details": json.dumps(details or {}),
                    "ip_address": ip_address,
                    "user_agent": user_agent,
                    "timestamp": datetime.utcnow(),
                    "success": success,
                }
            )
            # Nao fazer commit aqui - deixar para a transacao principal

        except Exception as e:
            logger.error(f"Erro ao registrar auditoria: {e}")

    def _audit_access(self, request: AccessRequest, decision: AccessDecision):
        """Registra auditoria de acesso"""
        self._audit(
            tenant_id=request.tenant_id,
            user_id=request.user_id,
            action=AuditAction.ACCESS_GRANTED.value if decision.allowed else AuditAction.ACCESS_DENIED.value,
            resource_type=request.resource_type,
            resource_id=request.resource_id,
            project_id=request.project_id,
            details={
                "action_requested": request.action,
                "allowed": decision.allowed,
                "reason": decision.reason,
                "matched_role": decision.matched_role,
            },
            success=decision.allowed,
        )

    def get_audit_log(
        self,
        tenant_id: str,
        user_id: Optional[str] = None,
        resource_type: Optional[str] = None,
        action: Optional[str] = None,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[AuditEntry]:
        """
        Consulta log de auditoria.

        Args:
            tenant_id: ID do tenant
            user_id: Filtrar por usuario
            resource_type: Filtrar por tipo de recurso
            action: Filtrar por acao
            start_date: Data inicial
            end_date: Data final
            limit: Limite de resultados
            offset: Offset para paginacao

        Returns:
            Lista de AuditEntry
        """
        try:
            query = "SELECT * FROM audit_log WHERE tenant_id = :tenant_id"
            params: Dict[str, Any] = {"tenant_id": tenant_id}

            if user_id:
                query += " AND user_id = :user_id"
                params["user_id"] = user_id

            if resource_type:
                query += " AND resource_type = :resource_type"
                params["resource_type"] = resource_type

            if action:
                query += " AND action = :action"
                params["action"] = action

            if start_date:
                query += " AND timestamp >= :start_date"
                params["start_date"] = start_date

            if end_date:
                query += " AND timestamp <= :end_date"
                params["end_date"] = end_date

            query += " ORDER BY timestamp DESC LIMIT :limit OFFSET :offset"
            params["limit"] = limit
            params["offset"] = offset

            results = self.db.execute(query, params).fetchall()

            entries = []
            for row in results:
                details = json.loads(row[7]) if row[7] else {}
                entries.append(AuditEntry(
                    audit_id=row[0],
                    tenant_id=row[1],
                    user_id=row[2],
                    action=row[3],
                    resource_type=row[4],
                    resource_id=row[5],
                    project_id=row[6],
                    details=details,
                    ip_address=row[8],
                    user_agent=row[9],
                    timestamp=row[10],
                    success=row[11],
                ))

            return entries

        except Exception as e:
            logger.error(f"Erro ao consultar audit log: {e}")
            return []


# =============================================================================
# DECORATORS
# =============================================================================

def require_permission(resource_type: str, action: str):
    """
    Decorator para verificar permissao antes de executar funcao.

    Uso:
        @require_permission("project", "create")
        async def create_project(request: Request):
            ...
    """
    def decorator(func):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            # Obter contexto do request
            from factory.core.multi_tenant import get_current_tenant_id, get_current_user

            tenant_id = get_current_tenant_id()
            user = get_current_user()

            if not tenant_id or not user:
                from fastapi import HTTPException
                raise HTTPException(status_code=401, detail="Nao autenticado")

            user_id = user.get("user_id")

            # Verificar permissao
            from factory.database.connection import SessionLocal
            db = SessionLocal()
            try:
                service = HierarchyService(db)
                if not service.can_access(user_id, tenant_id, resource_type, action):
                    from fastapi import HTTPException
                    raise HTTPException(
                        status_code=403,
                        detail=f"Permissao negada: {action} em {resource_type}"
                    )
            finally:
                db.close()

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def audit_action(resource_type: str, action: str):
    """
    Decorator para registrar auditoria de acao.

    Uso:
        @audit_action("project", "create")
        async def create_project(request: Request):
            ...
    """
    def decorator(func):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            from factory.core.multi_tenant import get_current_tenant_id, get_current_user

            result = await func(*args, **kwargs)

            # Registrar auditoria
            tenant_id = get_current_tenant_id()
            user = get_current_user()

            if tenant_id and user:
                from factory.database.connection import SessionLocal
                db = SessionLocal()
                try:
                    service = HierarchyService(db)
                    service._audit(
                        tenant_id=tenant_id,
                        user_id=user.get("user_id"),
                        action=action,
                        resource_type=resource_type,
                        details={"result": "success"},
                    )
                    db.commit()
                finally:
                    db.close()

            return result
        return wrapper
    return decorator


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "HierarchyService",
    "ResourceType",
    "Action",
    "Role",
    "PermissionScope",
    "AuditAction",
    "Permission",
    "RoleAssignment",
    "AccessRequest",
    "AccessDecision",
    "AuditEntry",
    "DEFAULT_ROLE_PERMISSIONS",
    "require_permission",
    "audit_action",
]
