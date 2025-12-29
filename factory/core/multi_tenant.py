# -*- coding: utf-8 -*-
"""
Multi-tenancy e White Label para a Fabrica de Agentes v5.0
============================================================

Este modulo implementa:
1. TenantContext: Contexto global do tenant atual (thread-safe)
2. TenantMiddleware: Middleware FastAPI para identificacao do tenant
3. TenantService: Servico de gestao de tenants
4. BrandingService: Servico de personalizacao visual
5. TenantQueryFilter: Filtro automatico de queries por tenant

Arquitetura:
- Cada request HTTP identifica o tenant via:
  - Header X-Tenant-ID
  - Subdominio (acme.app.com -> tenant "acme")
  - Dominio customizado (app.acme.com)
- Contexto e armazenado em ContextVar (thread-safe)
- Todas as queries sao automaticamente filtradas por tenant_id

Uso:
    from factory.core.multi_tenant import (
        get_current_tenant,
        TenantMiddleware,
        TenantService
    )

    # No middleware
    app.add_middleware(TenantMiddleware)

    # Em qualquer lugar do codigo
    tenant = get_current_tenant()
    if tenant:
        print(f"Tenant atual: {tenant.name}")
"""

import os
import re
import uuid
import secrets
from datetime import datetime, timedelta
from contextvars import ContextVar
from typing import Optional, Dict, Any, List, Callable
from dataclasses import dataclass, field

from fastapi import Request, Response, HTTPException
from starlette.middleware.base import BaseHTTPMiddleware


# =============================================================================
# CONTEXT VARIABLES (Thread-Safe)
# =============================================================================

# Contexto do tenant atual
_current_tenant: ContextVar[Optional[Dict[str, Any]]] = ContextVar("current_tenant", default=None)
_current_user: ContextVar[Optional[Dict[str, Any]]] = ContextVar("current_user", default=None)
_current_branding: ContextVar[Optional[Dict[str, Any]]] = ContextVar("current_branding", default=None)


def get_current_tenant() -> Optional[Dict[str, Any]]:
    """Retorna o tenant do contexto atual"""
    return _current_tenant.get()


def get_current_tenant_id() -> Optional[str]:
    """Retorna apenas o tenant_id do contexto atual"""
    tenant = _current_tenant.get()
    return tenant.get("tenant_id") if tenant else None


def get_current_user() -> Optional[Dict[str, Any]]:
    """Retorna o usuario do contexto atual"""
    return _current_user.get()


def get_current_branding() -> Optional[Dict[str, Any]]:
    """Retorna configuracao de branding do contexto atual"""
    return _current_branding.get()


def set_tenant_context(tenant: Optional[Dict[str, Any]]) -> None:
    """Define o tenant no contexto atual"""
    _current_tenant.set(tenant)


def set_user_context(user: Optional[Dict[str, Any]]) -> None:
    """Define o usuario no contexto atual"""
    _current_user.set(user)


def set_branding_context(branding: Optional[Dict[str, Any]]) -> None:
    """Define o branding no contexto atual"""
    _current_branding.set(branding)


def clear_context() -> None:
    """Limpa todo o contexto"""
    _current_tenant.set(None)
    _current_user.set(None)
    _current_branding.set(None)


# =============================================================================
# TENANT DATA CLASS
# =============================================================================

@dataclass
class TenantInfo:
    """Informacoes do tenant para contexto"""
    tenant_id: str
    name: str
    slug: str
    plan: str = "free"
    status: str = "active"
    features: Dict[str, bool] = field(default_factory=dict)
    settings: Dict[str, Any] = field(default_factory=dict)
    branding: Dict[str, Any] = field(default_factory=dict)

    def has_feature(self, feature_name: str) -> bool:
        """Verifica se tenant tem feature habilitada"""
        return self.features.get(feature_name, False)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "name": self.name,
            "slug": self.slug,
            "plan": self.plan,
            "status": self.status,
            "features": self.features,
            "settings": self.settings
        }


# =============================================================================
# TENANT MIDDLEWARE
# =============================================================================

class TenantMiddleware(BaseHTTPMiddleware):
    """
    Middleware FastAPI para identificacao e contexto do tenant

    Identificacao por ordem de prioridade:
    1. Header X-Tenant-ID (para APIs)
    2. Query parameter ?tenant_id= (para testes)
    3. Subdominio (acme.fabrica.com -> "acme")
    4. Dominio customizado (via lookup no banco)
    5. Cookie tenant_id (para sessoes web)

    Exemplo:
        app.add_middleware(TenantMiddleware)
    """

    def __init__(self, app, tenant_resolver: Callable = None):
        super().__init__(app)
        self.tenant_resolver = tenant_resolver or self._default_resolver
        self.domain_pattern = os.getenv("TENANT_DOMAIN_PATTERN", r"^(\w+)\.")

    async def dispatch(self, request: Request, call_next) -> Response:
        """Processa request identificando o tenant"""

        # Limpar contexto anterior
        clear_context()

        # Rotas publicas que nao precisam de tenant
        public_paths = ["/health", "/docs", "/openapi.json", "/redoc", "/api/public"]
        if any(request.url.path.startswith(p) for p in public_paths):
            return await call_next(request)

        # Identificar tenant
        tenant_id = await self._identify_tenant(request)

        if tenant_id:
            # Resolver dados completos do tenant
            tenant_data = await self.tenant_resolver(tenant_id)

            if tenant_data:
                # Verificar se tenant esta ativo
                if tenant_data.get("status") not in ["active", "trial"]:
                    raise HTTPException(
                        status_code=403,
                        detail=f"Tenant {tenant_id} esta {tenant_data.get('status', 'inativo')}"
                    )

                # Definir contexto
                set_tenant_context(tenant_data)

                # Carregar branding se disponivel
                if tenant_data.get("branding"):
                    set_branding_context(tenant_data["branding"])

        # Processar request
        response = await call_next(request)

        # Adicionar header com tenant_id na resposta (para debug)
        if tenant_id:
            response.headers["X-Tenant-ID"] = tenant_id

        # Limpar contexto apos request
        clear_context()

        return response

    async def _identify_tenant(self, request: Request) -> Optional[str]:
        """Identifica o tenant da request"""

        # 1. Header X-Tenant-ID (APIs)
        tenant_id = request.headers.get("X-Tenant-ID")
        if tenant_id:
            return tenant_id

        # 2. Query parameter (testes/debug)
        tenant_id = request.query_params.get("tenant_id")
        if tenant_id:
            return tenant_id

        # 3. Subdominio
        host = request.headers.get("host", "")
        match = re.match(self.domain_pattern, host)
        if match:
            subdomain = match.group(1)
            # Ignorar subdominios padrao
            if subdomain not in ["www", "api", "app", "localhost"]:
                return subdomain

        # 4. Cookie
        tenant_id = request.cookies.get("tenant_id")
        if tenant_id:
            return tenant_id

        # 5. Tenant padrao (development)
        default_tenant = os.getenv("DEFAULT_TENANT_ID")
        if default_tenant:
            return default_tenant

        return None

    async def _default_resolver(self, tenant_id: str) -> Optional[Dict[str, Any]]:
        """
        Resolver padrao - busca tenant no banco

        Substitua por seu proprio resolver se necessario.
        """
        try:
            from factory.database.connection import SessionLocal
            from factory.database.tenant_models import Tenant, TenantSettings, BrandingConfig

            db = SessionLocal()
            try:
                # Buscar por tenant_id ou slug
                tenant = db.query(Tenant).filter(
                    (Tenant.tenant_id == tenant_id) | (Tenant.slug == tenant_id)
                ).first()

                if not tenant:
                    return None

                # Montar dados do tenant
                tenant_data = tenant.to_dict()

                # Adicionar settings
                if tenant.settings:
                    tenant_data["settings"] = tenant.settings.to_dict()

                # Adicionar branding
                if tenant.branding:
                    tenant_data["branding"] = tenant.branding.to_dict()

                return tenant_data

            finally:
                db.close()

        except Exception as e:
            print(f"[TenantMiddleware] Erro ao resolver tenant {tenant_id}: {e}")
            # Em desenvolvimento, retornar tenant mock
            if os.getenv("ENV", "development") == "development":
                return self._mock_tenant(tenant_id)
            return None

    def _mock_tenant(self, tenant_id: str) -> Dict[str, Any]:
        """Tenant mock para desenvolvimento"""
        return {
            "tenant_id": tenant_id,
            "name": f"Tenant {tenant_id}",
            "slug": tenant_id.lower(),
            "plan": "professional",
            "status": "active",
            "features": {
                "stories": True,
                "kanban": True,
                "workers": True,
                "chat_assistant": True,
                "custom_branding": True,
                "sso": False,
                "api_access": True
            },
            "settings": {
                "max_projects": 100,
                "max_members": 50,
                "preferred_model": "claude-sonnet-4-20250514"
            },
            "branding": {
                "display_name": f"Plataforma {tenant_id}",
                "colors": {
                    "primary": "#003B4A",
                    "secondary": "#FF6C00"
                }
            }
        }


# =============================================================================
# TENANT SERVICE
# =============================================================================

class TenantService:
    """
    Servico para gestao de tenants

    Responsabilidades:
    - CRUD de tenants
    - Gestao de membros
    - Verificacao de limites/quotas
    - Geracao de convites
    """

    def __init__(self, db_session=None):
        self.db = db_session

    def _get_db(self):
        """Obtem sessao do banco"""
        if self.db:
            return self.db
        from factory.database.connection import SessionLocal
        return SessionLocal()

    def create_tenant(
        self,
        name: str,
        email: str,
        plan: str = "trial",
        owner_user_id: int = None,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Cria um novo tenant

        Args:
            name: Nome da organizacao
            email: Email de contato
            plan: Plano (free, starter, professional, enterprise)
            owner_user_id: ID do usuario owner

        Returns:
            Dados do tenant criado
        """
        from factory.database.tenant_models import (
            Tenant, TenantSettings, BrandingConfig, TenantMember, MemberRole, TenantStatus
        )

        db = self._get_db()
        try:
            # Gerar IDs unicos
            tenant_id = kwargs.get("tenant_id") or f"TEN-{uuid.uuid4().hex[:8].upper()}"
            slug = kwargs.get("slug") or self._generate_slug(name)

            # Verificar unicidade do slug
            existing = db.query(Tenant).filter(Tenant.slug == slug).first()
            if existing:
                slug = f"{slug}-{uuid.uuid4().hex[:4]}"

            # Criar tenant
            tenant = Tenant(
                tenant_id=tenant_id,
                name=name,
                slug=slug,
                email=email,
                plan=plan,
                status=TenantStatus.TRIAL.value if plan == "trial" else TenantStatus.ACTIVE.value,
                trial_ends_at=datetime.utcnow() + timedelta(days=14) if plan == "trial" else None,
                **{k: v for k, v in kwargs.items() if k not in ["tenant_id", "slug"]}
            )
            db.add(tenant)

            # Criar settings padrao
            settings = TenantSettings(tenant_id=tenant_id)
            db.add(settings)

            # Criar branding padrao
            branding = BrandingConfig(
                tenant_id=tenant_id,
                display_name=name
            )
            db.add(branding)

            # Adicionar owner se fornecido
            if owner_user_id:
                member = TenantMember(
                    tenant_id=tenant_id,
                    user_id=owner_user_id,
                    role=MemberRole.OWNER.value
                )
                db.add(member)

            db.commit()
            db.refresh(tenant)

            return tenant.to_dict()

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def get_tenant(self, tenant_id: str, include_settings: bool = True) -> Optional[Dict[str, Any]]:
        """Busca tenant por ID ou slug"""
        from factory.database.tenant_models import Tenant

        db = self._get_db()
        try:
            tenant = db.query(Tenant).filter(
                (Tenant.tenant_id == tenant_id) | (Tenant.slug == tenant_id)
            ).first()

            if not tenant:
                return None

            data = tenant.to_dict(include_sensitive=include_settings)

            if include_settings and tenant.settings:
                data["settings"] = tenant.settings.to_dict()

            if include_settings and tenant.branding:
                data["branding"] = tenant.branding.to_dict()

            return data

        finally:
            if not self.db:
                db.close()

    def update_tenant(self, tenant_id: str, **kwargs) -> Optional[Dict[str, Any]]:
        """Atualiza dados do tenant"""
        from factory.database.tenant_models import Tenant

        db = self._get_db()
        try:
            tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
            if not tenant:
                return None

            for key, value in kwargs.items():
                if hasattr(tenant, key):
                    setattr(tenant, key, value)

            tenant.updated_at = datetime.utcnow()
            db.commit()
            db.refresh(tenant)

            return tenant.to_dict()

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def delete_tenant(self, tenant_id: str, soft_delete: bool = True) -> bool:
        """Deleta ou desativa tenant"""
        from factory.database.tenant_models import Tenant, TenantStatus

        db = self._get_db()
        try:
            tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
            if not tenant:
                return False

            if soft_delete:
                tenant.status = TenantStatus.CANCELLED.value
                tenant.updated_at = datetime.utcnow()
            else:
                db.delete(tenant)

            db.commit()
            return True

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def check_quota(self, tenant_id: str, quota_name: str, current_value: int = None) -> Dict[str, Any]:
        """
        Verifica quota do tenant

        Returns:
            {
                "allowed": bool,
                "limit": int,
                "current": int,
                "remaining": int
            }
        """
        from factory.database.tenant_models import TenantSettings

        db = self._get_db()
        try:
            settings = db.query(TenantSettings).filter(
                TenantSettings.tenant_id == tenant_id
            ).first()

            if not settings:
                return {"allowed": True, "limit": None, "current": 0, "remaining": None}

            limit = getattr(settings, quota_name, None)
            if limit is None:
                return {"allowed": True, "limit": None, "current": current_value or 0, "remaining": None}

            current = current_value or 0
            remaining = limit - current

            return {
                "allowed": remaining > 0,
                "limit": limit,
                "current": current,
                "remaining": max(0, remaining)
            }

        finally:
            if not self.db:
                db.close()

    def add_member(
        self,
        tenant_id: str,
        user_id: int,
        role: str = "member",
        invited_by: str = None
    ) -> Dict[str, Any]:
        """Adiciona membro ao tenant"""
        from factory.database.tenant_models import TenantMember

        db = self._get_db()
        try:
            # Verificar se ja e membro
            existing = db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id
            ).first()

            if existing:
                raise ValueError(f"Usuario {user_id} ja e membro do tenant {tenant_id}")

            member = TenantMember(
                tenant_id=tenant_id,
                user_id=user_id,
                role=role,
                invited_by=invited_by
            )
            db.add(member)
            db.commit()
            db.refresh(member)

            return member.to_dict()

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def remove_member(self, tenant_id: str, user_id: int) -> bool:
        """Remove membro do tenant"""
        from factory.database.tenant_models import TenantMember, MemberRole

        db = self._get_db()
        try:
            member = db.query(TenantMember).filter(
                TenantMember.tenant_id == tenant_id,
                TenantMember.user_id == user_id
            ).first()

            if not member:
                return False

            # Nao permitir remover ultimo owner
            if member.role == MemberRole.OWNER.value:
                owners_count = db.query(TenantMember).filter(
                    TenantMember.tenant_id == tenant_id,
                    TenantMember.role == MemberRole.OWNER.value
                ).count()

                if owners_count <= 1:
                    raise ValueError("Nao e possivel remover o unico owner do tenant")

            db.delete(member)
            db.commit()
            return True

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def create_invite(
        self,
        tenant_id: str,
        email: str,
        role: str = "member",
        invited_by: str = None,
        message: str = None,
        expires_in_days: int = 7
    ) -> Dict[str, Any]:
        """Cria convite para novo membro"""
        from factory.database.tenant_models import TenantInvite

        db = self._get_db()
        try:
            invite = TenantInvite(
                invite_id=f"INV-{uuid.uuid4().hex[:8].upper()}",
                tenant_id=tenant_id,
                email=email,
                role=role,
                token=secrets.token_urlsafe(32),
                expires_at=datetime.utcnow() + timedelta(days=expires_in_days),
                invited_by=invited_by or "system",
                message=message
            )
            db.add(invite)
            db.commit()
            db.refresh(invite)

            return {
                **invite.to_dict(),
                "invite_url": f"/join/{invite.token}"
            }

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def _generate_slug(self, name: str) -> str:
        """Gera slug a partir do nome"""
        slug = name.lower()
        slug = re.sub(r'[^a-z0-9]+', '-', slug)
        slug = re.sub(r'-+', '-', slug)
        slug = slug.strip('-')
        return slug[:50]  # Limitar tamanho


# =============================================================================
# BRANDING SERVICE
# =============================================================================

class BrandingService:
    """
    Servico para personalizacao visual (White Label)

    Responsabilidades:
    - Gerenciar configuracoes de branding
    - Gerar CSS customizado
    - Upload de logos
    """

    def __init__(self, db_session=None):
        self.db = db_session

    def _get_db(self):
        """Obtem sessao do banco"""
        if self.db:
            return self.db
        from factory.database.connection import SessionLocal
        return SessionLocal()

    def get_branding(self, tenant_id: str) -> Optional[Dict[str, Any]]:
        """Retorna configuracoes de branding do tenant"""
        from factory.database.tenant_models import BrandingConfig

        db = self._get_db()
        try:
            branding = db.query(BrandingConfig).filter(
                BrandingConfig.tenant_id == tenant_id
            ).first()

            return branding.to_dict() if branding else None

        finally:
            if not self.db:
                db.close()

    def update_branding(self, tenant_id: str, **kwargs) -> Dict[str, Any]:
        """Atualiza configuracoes de branding"""
        from factory.database.tenant_models import BrandingConfig

        db = self._get_db()
        try:
            branding = db.query(BrandingConfig).filter(
                BrandingConfig.tenant_id == tenant_id
            ).first()

            if not branding:
                branding = BrandingConfig(tenant_id=tenant_id)
                db.add(branding)

            for key, value in kwargs.items():
                if hasattr(branding, key):
                    setattr(branding, key, value)

            branding.updated_at = datetime.utcnow()
            db.commit()
            db.refresh(branding)

            return branding.to_dict()

        except Exception as e:
            db.rollback()
            raise e
        finally:
            if not self.db:
                db.close()

    def generate_css(self, tenant_id: str, dark_mode: bool = False) -> str:
        """Gera CSS customizado para o tenant"""
        from factory.database.tenant_models import BrandingConfig

        db = self._get_db()
        try:
            branding = db.query(BrandingConfig).filter(
                BrandingConfig.tenant_id == tenant_id
            ).first()

            if not branding:
                return self._default_css()

            return branding.generate_css_variables(dark_mode)

        finally:
            if not self.db:
                db.close()

    def _default_css(self) -> str:
        """CSS padrao da plataforma"""
        return """
:root {
  --color-primary: #003B4A;
  --color-primary-hover: #00526A;
  --color-primary-light: #E6F0F2;
  --color-secondary: #FF6C00;
  --color-secondary-hover: #E65C00;
  --color-success: #10B981;
  --color-warning: #F59E0B;
  --color-error: #EF4444;
  --color-info: #3B82F6;
  --color-background: #F3F4F6;
  --color-surface: #FFFFFF;
  --color-surface-hover: #F9FAFB;
  --color-text-primary: #1F2937;
  --color-text-secondary: #6B7280;
  --color-text-muted: #9CA3AF;
  --color-text-on-primary: #FFFFFF;
  --color-border: #E5E7EB;
  --color-border-hover: #D1D5DB;
  --color-header-bg: #003B4A;
  --color-header-text: #FFFFFF;
  --color-sidebar-bg: #FFFFFF;
  --color-sidebar-text: #374151;
  --font-primary: 'Inter', 'Segoe UI', sans-serif;
  --font-heading: 'Inter', 'Segoe UI', sans-serif;
  --font-monospace: 'JetBrains Mono', 'Fira Code', monospace;
  --border-radius: 8px;
  --border-radius-sm: 4px;
  --border-radius-lg: 12px;
  --shadow-sm: 0 1px 2px rgba(0,0,0,0.05);
  --shadow: 0 4px 6px rgba(0,0,0,0.1);
  --shadow-lg: 0 10px 15px rgba(0,0,0,0.1);
}
"""


# =============================================================================
# TENANT QUERY FILTER
# =============================================================================

class TenantQueryFilter:
    """
    Filtro automatico de queries por tenant

    Uso com SQLAlchemy:
        filter = TenantQueryFilter()
        query = db.query(Project)
        query = filter.apply(query, Project)
    """

    @staticmethod
    def apply(query, model_class):
        """Aplica filtro de tenant a query"""
        tenant_id = get_current_tenant_id()

        if not tenant_id:
            # Sem tenant no contexto - retornar query vazia ou todos
            # Depende da politica de seguranca
            return query.filter(False)  # Retorna vazio por seguranca

        if hasattr(model_class, 'tenant_id'):
            return query.filter(model_class.tenant_id == tenant_id)

        return query

    @staticmethod
    def set_tenant_id(entity):
        """Define tenant_id em nova entidade"""
        tenant_id = get_current_tenant_id()

        if tenant_id and hasattr(entity, 'tenant_id'):
            entity.tenant_id = tenant_id

        return entity


# =============================================================================
# DECORATORS
# =============================================================================

def require_tenant(func):
    """
    Decorator que exige tenant no contexto

    Uso:
        @require_tenant
        async def my_endpoint():
            tenant = get_current_tenant()
            ...
    """
    async def wrapper(*args, **kwargs):
        tenant = get_current_tenant()
        if not tenant:
            raise HTTPException(
                status_code=400,
                detail="Tenant nao identificado. Envie header X-Tenant-ID."
            )
        return await func(*args, **kwargs)
    return wrapper


def require_feature(feature_name: str):
    """
    Decorator que exige feature habilitada no tenant

    Uso:
        @require_feature("custom_branding")
        async def my_endpoint():
            ...
    """
    def decorator(func):
        async def wrapper(*args, **kwargs):
            tenant = get_current_tenant()
            if not tenant:
                raise HTTPException(
                    status_code=400,
                    detail="Tenant nao identificado"
                )

            features = tenant.get("features", {})
            if not features.get(feature_name, False):
                raise HTTPException(
                    status_code=403,
                    detail=f"Feature '{feature_name}' nao disponivel no seu plano"
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


def require_role(allowed_roles: List[str]):
    """
    Decorator que exige role especifica no tenant

    Uso:
        @require_role(["owner", "admin"])
        async def admin_endpoint():
            ...
    """
    def decorator(func):
        async def wrapper(*args, **kwargs):
            user = get_current_user()
            if not user:
                raise HTTPException(
                    status_code=401,
                    detail="Usuario nao autenticado"
                )

            role = user.get("role", "viewer")
            if role not in allowed_roles:
                raise HTTPException(
                    status_code=403,
                    detail=f"Permissao negada. Role '{role}' nao autorizada."
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Context functions
    "get_current_tenant",
    "get_current_tenant_id",
    "get_current_user",
    "get_current_branding",
    "set_tenant_context",
    "set_user_context",
    "set_branding_context",
    "clear_context",
    # Classes
    "TenantInfo",
    "TenantMiddleware",
    "TenantService",
    "BrandingService",
    "TenantQueryFilter",
    # Decorators
    "require_tenant",
    "require_feature",
    "require_role"
]
