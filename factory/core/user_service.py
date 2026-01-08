# -*- coding: utf-8 -*-
"""
User Service - Servico de Gerenciamento de Usuarios
====================================================

Issues #87, #113 - Admin Panels

Este modulo fornece servicos para gerenciamento de usuarios:
- CRUD de usuarios
- Atribuicao de roles
- Convites por email
- Filtros e paginacao

Niveis de administracao:
- Platform Admin: Gerencia todos os tenants
- Tenant Admin: Gerencia usuarios do tenant
- Project Admin: Gerencia membros do projeto
"""

import secrets
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from uuid import uuid4

# Database
from factory.database.connection import SessionLocal
from factory.database.models import User
from factory.database.tenant_models import (
    Tenant, TenantMember, TenantInvite, TenantSettings,
    MemberRole, MemberStatus, InviteStatus
)
from factory.api.auth import get_password_hash


class UserService:
    """
    Servico de gerenciamento de usuarios

    Fornece operacoes CRUD para usuarios, com suporte a multi-tenancy.
    """

    def __init__(self, db=None):
        """
        Inicializa o servico

        Args:
            db: Sessao do banco de dados (opcional, cria nova se nao fornecida)
        """
        self._db = db
        self._own_db = db is None

    @property
    def db(self):
        """Obtem sessao do banco de dados"""
        if self._db is None:
            self._db = SessionLocal()
        return self._db

    def close(self):
        """Fecha sessao do banco se criada internamente"""
        if self._own_db and self._db:
            self._db.close()
            self._db = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    # =========================================================================
    # USER CRUD
    # =========================================================================

    def list_users(
        self,
        tenant_id: Optional[str] = None,
        page: int = 1,
        limit: int = 20,
        filters: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Lista usuarios com filtros e paginacao

        Args:
            tenant_id: ID do tenant (se None, lista todos - apenas platform admin)
            page: Numero da pagina (1-indexed)
            limit: Itens por pagina
            filters: Filtros opcionais {status, role, search}

        Returns:
            Dict com users, total, page, limit
        """
        filters = filters or {}
        offset = (page - 1) * limit

        # Query base
        query = self.db.query(User)

        # Filtrar por tenant se especificado
        if tenant_id:
            # Buscar users que sao membros do tenant
            member_user_ids = self.db.query(TenantMember.user_id).filter(
                TenantMember.tenant_id == tenant_id
            ).subquery()
            query = query.filter(User.id.in_(member_user_ids))

        # Aplicar filtros
        if filters.get("status"):
            if filters["status"] == "active":
                query = query.filter(User.active == True)
            elif filters["status"] == "inactive":
                query = query.filter(User.active == False)
            elif filters["status"] == "pending":
                # Users que nunca logaram
                query = query.filter(User.last_login == None)

        if filters.get("role"):
            query = query.filter(User.role == filters["role"])

        if filters.get("search"):
            search_term = f"%{filters['search']}%"
            query = query.filter(
                (User.username.ilike(search_term)) |
                (User.email.ilike(search_term))
            )

        # Contar total
        total = query.count()

        # Ordenar e paginar
        users = query.order_by(User.created_at.desc()).offset(offset).limit(limit).all()

        # Enriquecer com dados de tenant se especificado
        result_users = []
        for user in users:
            user_dict = user.to_dict()

            if tenant_id:
                # Buscar role no tenant
                member = self.db.query(TenantMember).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.user_id == user.id
                ).first()

                if member:
                    user_dict["tenant_role"] = member.tenant_role
                    user_dict["member_status"] = member.status
                    user_dict["joined_at"] = member.joined_at.isoformat() if member.joined_at else None
                    user_dict["last_active_at"] = member.last_active_at.isoformat() if member.last_active_at else None

            result_users.append(user_dict)

        return {
            "users": result_users,
            "total": total,
            "page": page,
            "limit": limit,
            "pages": (total + limit - 1) // limit
        }

    def get_user(self, user_id: int) -> Optional[Dict[str, Any]]:
        """
        Busca usuario por ID

        Args:
            user_id: ID do usuario

        Returns:
            Dict com dados do usuario ou None
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return None
        return user.to_dict()

    def get_user_by_username(self, username: str) -> Optional[Dict[str, Any]]:
        """
        Busca usuario por username

        Args:
            username: Nome de usuario

        Returns:
            Dict com dados do usuario ou None
        """
        user = self.db.query(User).filter(User.username == username).first()
        if not user:
            return None
        return user.to_dict()

    def get_user_by_email(self, email: str) -> Optional[Dict[str, Any]]:
        """
        Busca usuario por email

        Args:
            email: Email do usuario

        Returns:
            Dict com dados do usuario ou None
        """
        user = self.db.query(User).filter(User.email == email).first()
        if not user:
            return None
        return user.to_dict()

    def create_user(
        self,
        tenant_id: str,
        username: str,
        email: str,
        password: str,
        role: str = "VIEWER",
        tenant_role: str = "member"
    ) -> Dict[str, Any]:
        """
        Cria novo usuario

        Args:
            tenant_id: ID do tenant
            username: Nome de usuario
            email: Email
            password: Senha
            role: Role global (ADMIN, DEVELOPER, VIEWER)
            tenant_role: Role no tenant (owner, admin, member, viewer)

        Returns:
            Dict com dados do usuario criado

        Raises:
            ValueError: Se usuario ja existe
        """
        # Verificar se usuario ja existe
        existing = self.db.query(User).filter(
            (User.username == username) | (User.email == email)
        ).first()

        if existing:
            if existing.username == username:
                raise ValueError(f"Usuario '{username}' ja existe")
            raise ValueError(f"Email '{email}' ja esta em uso")

        # Criar usuario
        password_hash = get_password_hash(password)

        user = User(
            username=username,
            email=email,
            password_hash=password_hash,
            role=role,
            active=True
        )

        self.db.add(user)
        self.db.flush()  # Para obter o ID

        # Adicionar ao tenant
        member = TenantMember(
            tenant_id=tenant_id,
            user_id=user.id,
            tenant_role=tenant_role,
            status=MemberStatus.ACTIVE.value,
            active=True,
            joined_at=datetime.utcnow()
        )

        self.db.add(member)
        self.db.commit()
        self.db.refresh(user)

        result = user.to_dict()
        result["tenant_role"] = tenant_role

        return result

    def update_user(
        self,
        user_id: int,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza usuario

        Args:
            user_id: ID do usuario
            **kwargs: Campos a atualizar (email, role, active, quotas, billing)

        Returns:
            Dict com dados atualizados ou None
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return None

        # Campos permitidos para atualizacao
        allowed_fields = ["email", "role", "active", "quotas", "billing"]

        for field, value in kwargs.items():
            if field in allowed_fields and hasattr(user, field):
                setattr(user, field, value)

        # Se mudando senha
        if "password" in kwargs and kwargs["password"]:
            user.password_hash = get_password_hash(kwargs["password"])

        user.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(user)

        return user.to_dict()

    def deactivate_user(self, user_id: int) -> bool:
        """
        Desativa usuario (soft delete)

        Args:
            user_id: ID do usuario

        Returns:
            True se desativado, False se nao encontrado
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return False

        user.active = False
        user.updated_at = datetime.utcnow()

        # Desativar em todos os tenants
        self.db.query(TenantMember).filter(
            TenantMember.user_id == user_id
        ).update({
            "active": False,
            "status": MemberStatus.DEACTIVATED.value
        })

        self.db.commit()
        return True

    def activate_user(self, user_id: int) -> bool:
        """
        Reativa usuario

        Args:
            user_id: ID do usuario

        Returns:
            True se ativado, False se nao encontrado
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return False

        user.active = True
        user.updated_at = datetime.utcnow()
        self.db.commit()

        return True

    def delete_user(self, user_id: int, hard_delete: bool = False) -> bool:
        """
        Deleta usuario

        Args:
            user_id: ID do usuario
            hard_delete: Se True, remove permanentemente

        Returns:
            True se deletado, False se nao encontrado
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return False

        if hard_delete:
            # Remover memberships
            self.db.query(TenantMember).filter(
                TenantMember.user_id == user_id
            ).delete()

            # Remover usuario
            self.db.delete(user)
        else:
            # Soft delete
            return self.deactivate_user(user_id)

        self.db.commit()
        return True

    # =========================================================================
    # ROLE MANAGEMENT
    # =========================================================================

    def assign_role(
        self,
        user_id: int,
        role: str,
        tenant_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Atribui role ao usuario

        Args:
            user_id: ID do usuario
            role: Nova role
            tenant_id: Se especificado, atribui role no tenant

        Returns:
            Dict com resultado

        Raises:
            ValueError: Se usuario ou tenant nao encontrado
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            raise ValueError(f"Usuario {user_id} nao encontrado")

        if tenant_id:
            # Atribuir role no tenant
            member = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id
            ).first()

            if not member:
                raise ValueError(f"Usuario nao e membro do tenant {tenant_id}")

            old_role = member.tenant_role
            member.tenant_role = role
            self.db.commit()

            return {
                "user_id": user_id,
                "tenant_id": tenant_id,
                "old_role": old_role,
                "new_role": role
            }
        else:
            # Atribuir role global
            old_role = user.role
            user.role = role
            user.updated_at = datetime.utcnow()
            self.db.commit()

            return {
                "user_id": user_id,
                "old_role": old_role,
                "new_role": role
            }

    def get_user_roles(self, user_id: int) -> Dict[str, Any]:
        """
        Obtem todas as roles de um usuario

        Args:
            user_id: ID do usuario

        Returns:
            Dict com global_role e tenant_roles
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return None

        # Buscar roles em todos os tenants
        memberships = self.db.query(TenantMember).filter(
            TenantMember.user_id == user_id
        ).all()

        tenant_roles = {}
        for m in memberships:
            tenant_roles[m.tenant_id] = {
                "role": m.tenant_role,
                "status": m.status,
                "active": m.active
            }

        return {
            "user_id": user_id,
            "username": user.username,
            "global_role": user.role,
            "tenant_roles": tenant_roles
        }

    # =========================================================================
    # INVITE MANAGEMENT
    # =========================================================================

    def send_invite(
        self,
        tenant_id: str,
        email: str,
        role: str = "member",
        message: Optional[str] = None,
        invited_by: str = "admin",
        expires_in_days: int = 7
    ) -> Dict[str, Any]:
        """
        Envia convite por email

        Args:
            tenant_id: ID do tenant
            email: Email do convidado
            role: Role a ser atribuida
            message: Mensagem personalizada
            invited_by: Quem esta convidando
            expires_in_days: Dias ate expirar

        Returns:
            Dict com dados do convite

        Raises:
            ValueError: Se email ja convidado ou ja membro
        """
        # Verificar se tenant existe
        tenant = self.db.query(Tenant).filter(
            Tenant.tenant_id == tenant_id
        ).first()

        if not tenant:
            raise ValueError(f"Tenant {tenant_id} nao encontrado")

        # Verificar se ja e membro
        existing_user = self.db.query(User).filter(User.email == email).first()
        if existing_user:
            existing_member = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == existing_user.id
            ).first()
            if existing_member:
                raise ValueError(f"Email '{email}' ja e membro do tenant")

        # Verificar se ja tem convite pendente
        existing_invite = self.db.query(TenantInvite).filter(
            TenantInvite.tenant_id == tenant_id,
            TenantInvite.email == email,
            TenantInvite.status == InviteStatus.PENDING.value
        ).first()

        if existing_invite:
            raise ValueError(f"Ja existe convite pendente para '{email}'")

        # Criar convite
        invite_id = f"INV-{datetime.utcnow().strftime('%Y%m%d%H%M%S')}-{uuid4().hex[:6].upper()}"
        token = secrets.token_urlsafe(32)

        invite = TenantInvite(
            invite_id=invite_id,
            tenant_id=tenant_id,
            email=email,
            role=role,
            token=token,
            status=InviteStatus.PENDING.value,
            expires_at=datetime.utcnow() + timedelta(days=expires_in_days),
            invited_by=invited_by,
            message=message
        )

        self.db.add(invite)
        self.db.commit()
        self.db.refresh(invite)

        # TODO: Enviar email de convite
        # email_service.send_invite_email(email, tenant.name, invite.token)

        return invite.to_dict()

    def list_invites(
        self,
        tenant_id: str,
        status: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista convites do tenant

        Args:
            tenant_id: ID do tenant
            status: Filtrar por status

        Returns:
            Lista de convites
        """
        query = self.db.query(TenantInvite).filter(
            TenantInvite.tenant_id == tenant_id
        )

        if status:
            query = query.filter(TenantInvite.status == status)

        invites = query.order_by(TenantInvite.created_at.desc()).all()
        return [i.to_dict() for i in invites]

    def revoke_invite(self, invite_id: str) -> bool:
        """
        Revoga convite

        Args:
            invite_id: ID do convite

        Returns:
            True se revogado
        """
        invite = self.db.query(TenantInvite).filter(
            TenantInvite.invite_id == invite_id
        ).first()

        if not invite:
            return False

        invite.status = InviteStatus.REVOKED.value
        self.db.commit()

        return True

    def accept_invite(self, token: str, user_id: int) -> Dict[str, Any]:
        """
        Aceita convite

        Args:
            token: Token do convite
            user_id: ID do usuario que esta aceitando

        Returns:
            Dict com resultado

        Raises:
            ValueError: Se convite invalido
        """
        invite = self.db.query(TenantInvite).filter(
            TenantInvite.token == token
        ).first()

        if not invite:
            raise ValueError("Convite nao encontrado")

        if not invite.is_valid():
            raise ValueError("Convite expirado ou ja utilizado")

        # Adicionar usuario como membro
        member = TenantMember(
            tenant_id=invite.tenant_id,
            user_id=user_id,
            tenant_role=invite.role,
            status=MemberStatus.ACTIVE.value,
            active=True,
            invited_by=invite.invited_by,
            invited_at=invite.created_at,
            joined_at=datetime.utcnow()
        )

        self.db.add(member)

        # Marcar convite como aceito
        invite.status = InviteStatus.ACCEPTED.value
        invite.accepted_at = datetime.utcnow()

        self.db.commit()

        return {
            "tenant_id": invite.tenant_id,
            "role": invite.role,
            "member": member.to_dict()
        }

    # =========================================================================
    # STATISTICS
    # =========================================================================

    def get_user_stats(self, tenant_id: Optional[str] = None) -> Dict[str, Any]:
        """
        Obtem estatisticas de usuarios

        Args:
            tenant_id: Se especificado, estatisticas do tenant

        Returns:
            Dict com estatisticas
        """
        if tenant_id:
            # Stats do tenant
            members = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id
            ).all()

            total = len(members)
            active = sum(1 for m in members if m.status == MemberStatus.ACTIVE.value)
            pending = sum(1 for m in members if m.status == MemberStatus.INVITED.value)
            suspended = sum(1 for m in members if m.status == MemberStatus.SUSPENDED.value)

            # Roles
            roles = {}
            for m in members:
                role = m.tenant_role
                roles[role] = roles.get(role, 0) + 1

            # Convites pendentes
            pending_invites = self.db.query(TenantInvite).filter(
                TenantInvite.tenant_id == tenant_id,
                TenantInvite.status == InviteStatus.PENDING.value
            ).count()

            return {
                "total": total,
                "active": active,
                "pending": pending,
                "suspended": suspended,
                "by_role": roles,
                "pending_invites": pending_invites
            }
        else:
            # Stats globais
            users = self.db.query(User).all()

            total = len(users)
            active = sum(1 for u in users if u.active)
            inactive = total - active

            # Roles
            roles = {}
            for u in users:
                role = u.role
                roles[role] = roles.get(role, 0) + 1

            return {
                "total": total,
                "active": active,
                "inactive": inactive,
                "by_role": roles
            }


# =============================================================================
# RBAC SERVICE
# =============================================================================

class RBACService:
    """
    Servico de Role-Based Access Control

    Gerencia permissoes baseadas em roles.
    """

    # Definicao de permissoes por role global
    GLOBAL_PERMISSIONS = {
        "ADMIN": ["*"],  # Todas as permissoes
        "DEVELOPER": [
            "create_project", "edit_project",
            "create_story", "edit_story", "delete_story",
            "run_workers", "view_logs"
        ],
        "VIEWER": ["view"]
    }

    # Definicao de permissoes por role de tenant
    TENANT_PERMISSIONS = {
        "owner": ["*"],  # Todas as permissoes
        "admin": [
            "manage_members", "manage_projects", "manage_settings",
            "view_billing", "create_stories", "delete_stories",
            "invite_users", "remove_users"
        ],
        "member": [
            "create_projects", "create_stories", "edit_stories",
            "run_workers"
        ],
        "viewer": ["view"],
        "billing": ["view_billing", "manage_billing"]
    }

    def __init__(self, db=None):
        self._db = db
        self._own_db = db is None

    @property
    def db(self):
        if self._db is None:
            self._db = SessionLocal()
        return self._db

    def close(self):
        if self._own_db and self._db:
            self._db.close()
            self._db = None

    def check_permission(
        self,
        user_id: int,
        permission: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """
        Verifica se usuario tem permissao

        Args:
            user_id: ID do usuario
            permission: Permissao a verificar
            tenant_id: ID do tenant (se aplicavel)

        Returns:
            True se tem permissao
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user or not user.active:
            return False

        # Verificar permissao global
        global_perms = self.GLOBAL_PERMISSIONS.get(user.role, [])
        if "*" in global_perms or permission in global_perms:
            return True

        # Verificar permissao no tenant
        if tenant_id:
            member = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id,
                TenantMember.active == True
            ).first()

            if member:
                tenant_perms = self.TENANT_PERMISSIONS.get(member.tenant_role, [])
                if "*" in tenant_perms or permission in tenant_perms:
                    return True

        return False

    def require_permission(
        self,
        user_id: int,
        permission: str,
        tenant_id: Optional[str] = None
    ) -> None:
        """
        Requer permissao (lanca excecao se nao tiver)

        Args:
            user_id: ID do usuario
            permission: Permissao requerida
            tenant_id: ID do tenant

        Raises:
            PermissionError: Se nao tiver permissao
        """
        if not self.check_permission(user_id, permission, tenant_id):
            raise PermissionError(
                f"Permissao '{permission}' negada para usuario {user_id}"
            )

    def get_user_permissions(
        self,
        user_id: int,
        tenant_id: Optional[str] = None
    ) -> List[str]:
        """
        Obtem todas as permissoes de um usuario

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant

        Returns:
            Lista de permissoes
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user or not user.active:
            return []

        permissions = set()

        # Permissoes globais
        global_perms = self.GLOBAL_PERMISSIONS.get(user.role, [])
        if "*" in global_perms:
            permissions.add("*")
        else:
            permissions.update(global_perms)

        # Permissoes do tenant
        if tenant_id:
            member = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id,
                TenantMember.active == True
            ).first()

            if member:
                tenant_perms = self.TENANT_PERMISSIONS.get(member.tenant_role, [])
                if "*" in tenant_perms:
                    permissions.add("*")
                else:
                    permissions.update(tenant_perms)

        return list(permissions)

    def is_admin(self, user_id: int, tenant_id: Optional[str] = None) -> bool:
        """
        Verifica se usuario e admin

        Args:
            user_id: ID do usuario
            tenant_id: ID do tenant

        Returns:
            True se for admin
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        if not user:
            return False

        # Admin global
        if user.role == "ADMIN":
            return True

        # Admin de tenant
        if tenant_id:
            member = self.db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id,
                TenantMember.active == True
            ).first()

            if member and member.tenant_role in ["owner", "admin"]:
                return True

        return False

    def is_platform_admin(self, user_id: int) -> bool:
        """
        Verifica se usuario e platform admin (super admin)

        Args:
            user_id: ID do usuario

        Returns:
            True se for platform admin
        """
        user = self.db.query(User).filter(User.id == user_id).first()
        return user and user.role == "ADMIN"


# =============================================================================
# MULTI-PROFILE SYSTEM (Issue #TBD - Multiple Profiles per User)
# =============================================================================

def assign_profiles(
    user_id: int,
    profile_ids: List[str],
    scope: str = "global",
    scope_id: Optional[str] = None,
    assigned_by: Optional[int] = None,
    db=None
) -> Dict[str, Any]:
    """
    Atribui multiplos perfis a um usuario

    Este é o metodo principal para gerenciar perfis de usuarios no novo sistema.
    Substitui perfis existentes no mesmo escopo pelos novos.

    Args:
        user_id: ID do usuario
        profile_ids: Lista de profile_ids para atribuir (ex: ["dev_frontend", "qa_manual"])
        scope: 'global', 'tenant', 'project'
        scope_id: ID do tenant ou projeto (obrigatorio para scope != 'global')
        assigned_by: ID do usuario que esta atribuindo (para auditoria)
        db: Sessao do banco (opcional, cria nova se nao fornecida)

    Returns:
        Dict com resultado da operacao

    Raises:
        ValueError: Se usuario ou perfis nao encontrados

    Example:
        # Atribuir perfis globais
        assign_profiles(user_id=5, profile_ids=["dev_frontend", "qa_manual"])

        # Atribuir perfis em projeto especifico (override)
        assign_profiles(
            user_id=5,
            profile_ids=["tech_lead"],
            scope="project",
            scope_id="PRJ-001"
        )
    """
    from factory.database.models import Profile, UserProfile

    # Criar sessao se nao fornecida
    own_db = db is None
    if own_db:
        db = SessionLocal()

    try:
        # Verificar se usuario existe
        user = db.query(User).filter(User.id == user_id).first()
        if not user:
            raise ValueError(f"Usuario {user_id} nao encontrado")

        # Validar scope_id
        if scope in ["tenant", "project"] and not scope_id:
            raise ValueError(f"scope_id e obrigatorio para scope '{scope}'")

        # Verificar se todos os perfis existem
        profiles = db.query(Profile).filter(
            Profile.profile_id.in_(profile_ids),
            Profile.is_active == True
        ).all()

        if len(profiles) != len(profile_ids):
            found_ids = {p.profile_id for p in profiles}
            missing_ids = set(profile_ids) - found_ids
            raise ValueError(f"Perfis nao encontrados: {', '.join(missing_ids)}")

        # Remove perfis antigos do mesmo escopo
        deleted = db.query(UserProfile).filter(
            UserProfile.user_id == user_id,
            UserProfile.scope == scope,
            UserProfile.scope_id == scope_id if scope_id else UserProfile.scope_id.is_(None)
        ).delete(synchronize_session=False)

        # Adiciona novos perfis
        created = []
        for idx, profile_id in enumerate(profile_ids):
            user_profile = UserProfile(
                user_id=user_id,
                profile_id=profile_id,
                scope=scope,
                scope_id=scope_id,
                is_primary=(idx == 0),  # Primeiro perfil e o primario
                active=True,
                assigned_by=assigned_by,
                assigned_at=datetime.utcnow()
            )
            db.add(user_profile)
            created.append(profile_id)

        db.commit()

        return {
            "user_id": user_id,
            "scope": scope,
            "scope_id": scope_id,
            "profiles_removed": deleted,
            "profiles_added": created,
            "primary_profile": profile_ids[0] if profile_ids else None
        }

    except Exception as e:
        db.rollback()
        raise
    finally:
        if own_db:
            db.close()


def get_user_profiles(
    user_id: int,
    scope: str = "global",
    scope_id: Optional[str] = None,
    include_inactive: bool = False,
    db=None
) -> List[Dict[str, Any]]:
    """
    Retorna perfis de um usuario

    Args:
        user_id: ID do usuario
        scope: 'global', 'tenant', 'project' (None = todos)
        scope_id: ID do tenant ou projeto
        include_inactive: Se True, inclui perfis inativos
        db: Sessao do banco (opcional)

    Returns:
        Lista de dicts com dados dos perfis

    Example:
        # Perfis globais
        profiles = get_user_profiles(user_id=5)

        # Perfis de um projeto
        profiles = get_user_profiles(user_id=5, scope="project", scope_id="PRJ-001")

        # Todos os perfis (qualquer escopo)
        profiles = get_user_profiles(user_id=5, scope=None)
    """
    from factory.database.models import UserProfile

    # Criar sessao se nao fornecida
    own_db = db is None
    if own_db:
        db = SessionLocal()

    try:
        # Query base
        query = db.query(UserProfile).filter(UserProfile.user_id == user_id)

        # Filtrar por scope se especificado
        if scope is not None:
            query = query.filter(UserProfile.scope == scope)

            # Filtrar por scope_id se fornecido
            if scope_id:
                query = query.filter(UserProfile.scope_id == scope_id)
            else:
                query = query.filter(UserProfile.scope_id.is_(None))

        # Filtrar ativos
        if not include_inactive:
            query = query.filter(UserProfile.active == True)

        # Executar query
        user_profiles = query.all()

        # Converter para dicts
        result = []
        for up in user_profiles:
            # Verificar se ainda e valido (expires_at)
            if not include_inactive and not up.is_valid():
                continue

            result.append(up.to_dict())

        return result

    finally:
        if own_db:
            db.close()


def remove_user_profile(
    user_id: int,
    profile_id: str,
    scope: str = "global",
    scope_id: Optional[str] = None,
    db=None
) -> bool:
    """
    Remove um perfil especifico de um usuario

    Args:
        user_id: ID do usuario
        profile_id: ID do perfil a remover
        scope: 'global', 'tenant', 'project'
        scope_id: ID do tenant ou projeto
        db: Sessao do banco (opcional)

    Returns:
        True se removido, False se nao encontrado

    Example:
        # Remover perfil global
        remove_user_profile(user_id=5, profile_id="qa_manual")

        # Remover perfil de projeto
        remove_user_profile(
            user_id=5,
            profile_id="tech_lead",
            scope="project",
            scope_id="PRJ-001"
        )
    """
    from factory.database.models import UserProfile

    own_db = db is None
    if own_db:
        db = SessionLocal()

    try:
        # Buscar UserProfile
        query = db.query(UserProfile).filter(
            UserProfile.user_id == user_id,
            UserProfile.profile_id == profile_id,
            UserProfile.scope == scope
        )

        if scope_id:
            query = query.filter(UserProfile.scope_id == scope_id)
        else:
            query = query.filter(UserProfile.scope_id.is_(None))

        user_profile = query.first()

        if not user_profile:
            return False

        db.delete(user_profile)
        db.commit()

        return True

    except Exception as e:
        db.rollback()
        raise
    finally:
        if own_db:
            db.close()
