# -*- coding: utf-8 -*-
"""
Modelos de Multi-tenancy e White Label para a Fabrica de Agentes v5.0

Este modulo define as entidades necessarias para suporte a multi-tenancy:
- Tenant: Organizacao/Empresa que usa a plataforma
- TenantSettings: Configuracoes especificas do tenant (limites, modelos IA)
- BrandingConfig: Personalizacao visual completa (white label)
- TenantMember: Membros e roles por tenant (Owner, Admin, Member, Viewer)
- SSOConfig: Configuracao de SSO/SAML por tenant
- TenantInvite: Convites pendentes para membros
- TenantUsageLog: Registro de uso para billing

Isolamento de Dados:
- Cada entidade principal (Project, Story, Job, etc.) possui tenant_id
- Queries sao filtradas automaticamente pelo TenantMiddleware
- Dados de um tenant nunca sao acessiveis por outro
"""

from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float, UniqueConstraint
from sqlalchemy.orm import relationship
from datetime import datetime
from enum import Enum
from typing import Optional, Dict, Any

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# ENUMS MULTI-TENANCY
# =============================================================================

class TenantStatus(str, Enum):
    """Status do Tenant"""
    ACTIVE = "active"           # Tenant ativo e funcionando
    SUSPENDED = "suspended"     # Suspenso por falta de pagamento
    TRIAL = "trial"             # Periodo de teste
    CANCELLED = "cancelled"     # Cancelado pelo usuario
    PENDING = "pending"         # Aguardando ativacao


class TenantPlan(str, Enum):
    """Planos disponiveis"""
    FREE = "free"               # Plano gratuito com limites
    STARTER = "starter"         # Plano inicial pago
    PROFESSIONAL = "professional"  # Plano profissional
    ENTERPRISE = "enterprise"   # Plano enterprise com SLA
    CUSTOM = "custom"           # Plano customizado


class MemberRole(str, Enum):
    """Roles de membros do tenant"""
    OWNER = "owner"             # Dono do tenant (pode deletar)
    ADMIN = "admin"             # Administrador (gerencia membros)
    MEMBER = "member"           # Membro padrao (cria projetos)
    VIEWER = "viewer"           # Apenas visualizacao
    BILLING = "billing"         # Acesso a faturamento


class SSOProvider(str, Enum):
    """Provedores de SSO suportados"""
    SAML = "saml"               # SAML 2.0 generico
    AZURE_AD = "azure_ad"       # Azure Active Directory
    GOOGLE = "google"           # Google Workspace
    OKTA = "okta"               # Okta
    ONELOGIN = "onelogin"       # OneLogin
    AUTH0 = "auth0"             # Auth0
    CUSTOM_OIDC = "custom_oidc" # OpenID Connect customizado


class InviteStatus(str, Enum):
    """Status do convite"""
    PENDING = "pending"         # Aguardando aceite
    ACCEPTED = "accepted"       # Aceito
    EXPIRED = "expired"         # Expirado
    REVOKED = "revoked"         # Revogado


# =============================================================================
# TENANT - Organizacao/Empresa Principal
# =============================================================================

class Tenant(Base):
    """
    Modelo para Tenant - Organizacao/Empresa que usa a plataforma

    Cada tenant tem isolamento completo de dados e pode personalizar
    a aparencia da plataforma (white label).

    Exemplo:
        tenant = Tenant(
            tenant_id="ACME-001",
            name="Acme Corporation",
            slug="acme",
            plan="professional"
        )
    """
    __tablename__ = "tenants"

    id = Column(Integer, primary_key=True, autoincrement=True)
    tenant_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(200), nullable=False)
    slug = Column(String(100), unique=True, nullable=False, index=True)  # URL-friendly
    description = Column(Text, nullable=True)

    # Dominio customizado (white label)
    custom_domain = Column(String(255), unique=True, nullable=True)  # Ex: app.empresa.com
    primary_domain = Column(String(255), nullable=True)  # Dominio principal da empresa

    # Contato
    email = Column(String(255), nullable=False)
    phone = Column(String(50), nullable=True)

    # Endereco (para faturamento)
    address = Column(JSON, default=dict)  # {street, city, state, zip, country}

    # Status e Plano
    status = Column(String(30), default=TenantStatus.TRIAL.value, index=True)
    plan = Column(String(30), default=TenantPlan.FREE.value)

    # Trial
    trial_ends_at = Column(DateTime, nullable=True)

    # Configuracoes
    timezone = Column(String(50), default="America/Sao_Paulo")
    locale = Column(String(10), default="pt-BR")

    # Features habilitadas
    features = Column(JSON, default=lambda: {
        "stories": True,
        "kanban": True,
        "workers": True,
        "chat_assistant": True,
        "custom_branding": False,
        "sso": False,
        "api_access": True,
        "webhooks": False,
        "audit_logs": False,
        "custom_domain": False
    })

    # Metadados
    tenant_metadata = Column(JSON, default=dict)  # Renamed from 'metadata' (SQLAlchemy reserved)
    tags = Column(JSON, default=list)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    activated_at = Column(DateTime, nullable=True)
    suspended_at = Column(DateTime, nullable=True)

    # Relacionamentos
    settings = relationship("TenantSettings", back_populates="tenant", uselist=False, cascade="all, delete-orphan")
    branding = relationship("BrandingConfig", back_populates="tenant", uselist=False, cascade="all, delete-orphan")
    members = relationship("TenantMember", back_populates="tenant", cascade="all, delete-orphan")
    sso_config = relationship("SSOConfig", back_populates="tenant", uselist=False, cascade="all, delete-orphan")
    invites = relationship("TenantInvite", back_populates="tenant", cascade="all, delete-orphan")
    usage_logs = relationship("TenantUsageLog", back_populates="tenant", cascade="all, delete-orphan")

    def to_dict(self, include_sensitive: bool = False) -> Dict[str, Any]:
        """Converte para dicionario"""
        data = {
            "tenant_id": self.tenant_id,
            "name": self.name,
            "slug": self.slug,
            "description": self.description,
            "custom_domain": self.custom_domain,
            "email": self.email,
            "status": self.status,
            "plan": self.plan,
            "trial_ends_at": self.trial_ends_at.isoformat() if self.trial_ends_at else None,
            "timezone": self.timezone,
            "locale": self.locale,
            "features": self.features or {},
            "tags": self.tags or [],
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "members_count": len(self.members) if self.members else 0
        }

        if include_sensitive:
            data["phone"] = self.phone
            data["address"] = self.address
            data["primary_domain"] = self.primary_domain
            data["tenant_metadata"] = self.tenant_metadata

        return data

    def is_active(self) -> bool:
        """Verifica se tenant esta ativo"""
        return self.status in [TenantStatus.ACTIVE.value, TenantStatus.TRIAL.value]

    def has_feature(self, feature_name: str) -> bool:
        """Verifica se tenant tem feature habilitada"""
        if not self.features:
            return False
        return self.features.get(feature_name, False)

    def __repr__(self):
        return f"<Tenant {self.tenant_id}: {self.name} [{self.plan}]>"


# =============================================================================
# TENANT_SETTINGS - Configuracoes do Tenant
# =============================================================================

class TenantSettings(Base):
    """
    Modelo para Configuracoes especificas do Tenant

    Define limites, modelos de IA preferidos, integracoes ativas, etc.
    """
    __tablename__ = "tenant_settings"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), unique=True, nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="settings")

    # Limites de Uso
    max_projects = Column(Integer, default=10)
    max_stories_per_project = Column(Integer, default=100)
    max_members = Column(Integer, default=5)
    max_storage_gb = Column(Float, default=5.0)
    max_api_calls_per_day = Column(Integer, default=1000)
    max_tokens_per_month = Column(Integer, default=100000)
    max_concurrent_workers = Column(Integer, default=2)

    # Modelo Claude Preferido
    preferred_model = Column(String(50), default="claude-sonnet-4-20250514")
    allowed_models = Column(JSON, default=lambda: [
        "claude-sonnet-4-20250514",
        "claude-3-5-sonnet-20241022",
        "claude-3-haiku-20240307"
    ])

    # Configuracoes de IA
    ai_settings = Column(JSON, default=lambda: {
        "temperature": 0.7,
        "max_tokens": 4096,
        "auto_suggest_stories": True,
        "auto_estimate_points": True,
        "code_review_enabled": True
    })

    # Integracoes Ativas
    integrations = Column(JSON, default=lambda: {
        "github": {"enabled": False, "org": None, "token": None},
        "slack": {"enabled": False, "webhook_url": None},
        "jira": {"enabled": False, "url": None, "token": None},
        "azure_devops": {"enabled": False, "org": None, "token": None}
    })

    # Webhooks
    webhooks = Column(JSON, default=lambda: {
        "story_created": None,
        "story_completed": None,
        "task_completed": None,
        "project_completed": None
    })

    # Notificacoes
    notifications = Column(JSON, default=lambda: {
        "email_on_story_complete": True,
        "email_on_task_blocked": True,
        "email_daily_summary": False,
        "slack_notifications": False
    })

    # Personalizacao do Workflow
    workflow_settings = Column(JSON, default=lambda: {
        "story_statuses": ["backlog", "ready", "in_progress", "review", "testing", "done"],
        "require_review": True,
        "require_testing": True,
        "auto_move_on_complete": True
    })

    # Seguranca
    security_settings = Column(JSON, default=lambda: {
        "require_2fa": False,
        "session_timeout_minutes": 480,
        "ip_whitelist": [],
        "password_policy": {
            "min_length": 8,
            "require_uppercase": True,
            "require_numbers": True,
            "require_special": False
        }
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self, include_sensitive: bool = False) -> Dict[str, Any]:
        """Converte para dicionario"""
        data = {
            "tenant_id": self.tenant_id,
            "max_projects": self.max_projects,
            "max_stories_per_project": self.max_stories_per_project,
            "max_members": self.max_members,
            "max_storage_gb": self.max_storage_gb,
            "max_api_calls_per_day": self.max_api_calls_per_day,
            "max_tokens_per_month": self.max_tokens_per_month,
            "max_concurrent_workers": self.max_concurrent_workers,
            "preferred_model": self.preferred_model,
            "allowed_models": self.allowed_models or [],
            "ai_settings": self.ai_settings or {},
            "workflow_settings": self.workflow_settings or {},
            "notifications": self.notifications or {},
            "security_settings": self.security_settings or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

        if include_sensitive:
            data["integrations"] = self.integrations
            data["webhooks"] = self.webhooks

        return data

    def check_limit(self, limit_name: str, current_value: int) -> bool:
        """Verifica se ainda tem quota disponivel"""
        limit = getattr(self, limit_name, None)
        if limit is None:
            return True
        return current_value < limit

    def __repr__(self):
        return f"<TenantSettings {self.tenant_id}>"


# =============================================================================
# BRANDING_CONFIG - Personalizacao Visual (White Label)
# =============================================================================

class BrandingConfig(Base):
    """
    Modelo para Personalizacao Visual do Tenant (White Label)

    Permite customizar completamente a aparencia da plataforma:
    - Logo
    - Cores (CSS variables)
    - Fontes
    - Footer
    - Emails
    """
    __tablename__ = "branding_configs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), unique=True, nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="branding")

    # Logo e Favicon
    logo_url = Column(String(500), nullable=True)  # URL do logo principal
    logo_dark_url = Column(String(500), nullable=True)  # Logo para dark mode
    favicon_url = Column(String(500), nullable=True)  # Favicon
    logo_width = Column(Integer, default=180)  # Largura do logo em px
    logo_height = Column(Integer, default=50)  # Altura do logo em px

    # Nome exibido (remove mencoes a Fabrica)
    display_name = Column(String(200), nullable=True)  # Nome customizado
    tagline = Column(String(300), nullable=True)  # Slogan

    # Cores Principais (CSS Variables)
    colors = Column(JSON, default=lambda: {
        # Cores primarias
        "primary": "#003B4A",           # Azul principal
        "primary_hover": "#00526A",     # Azul hover
        "primary_light": "#E6F0F2",     # Azul claro

        # Cores secundarias
        "secondary": "#FF6C00",         # Laranja
        "secondary_hover": "#E65C00",   # Laranja hover

        # Cores de estado
        "success": "#10B981",           # Verde sucesso
        "warning": "#F59E0B",           # Amarelo aviso
        "error": "#EF4444",             # Vermelho erro
        "info": "#3B82F6",              # Azul info

        # Cores de fundo
        "background": "#F3F4F6",        # Fundo principal
        "surface": "#FFFFFF",           # Cards e paineis
        "surface_hover": "#F9FAFB",     # Hover de cards

        # Cores de texto
        "text_primary": "#1F2937",      # Texto principal
        "text_secondary": "#6B7280",    # Texto secundario
        "text_muted": "#9CA3AF",        # Texto apagado
        "text_on_primary": "#FFFFFF",   # Texto sobre cor primaria

        # Bordas
        "border": "#E5E7EB",            # Borda padrao
        "border_hover": "#D1D5DB",      # Borda hover

        # Header e Sidebar
        "header_bg": "#003B4A",         # Fundo do header
        "header_text": "#FFFFFF",       # Texto do header
        "sidebar_bg": "#FFFFFF",        # Fundo da sidebar
        "sidebar_text": "#374151"       # Texto da sidebar
    })

    # Dark Mode
    dark_mode_enabled = Column(Boolean, default=True)
    dark_colors = Column(JSON, default=lambda: {
        "primary": "#00A3CC",
        "primary_hover": "#008FB3",
        "primary_light": "#1A3A42",
        "secondary": "#FF8533",
        "background": "#111827",
        "surface": "#1F2937",
        "surface_hover": "#374151",
        "text_primary": "#F9FAFB",
        "text_secondary": "#D1D5DB",
        "text_muted": "#9CA3AF",
        "border": "#374151",
        "header_bg": "#1F2937",
        "header_text": "#F9FAFB",
        "sidebar_bg": "#1F2937",
        "sidebar_text": "#D1D5DB"
    })

    # Tipografia
    fonts = Column(JSON, default=lambda: {
        "primary": "'Inter', 'Segoe UI', sans-serif",
        "heading": "'Inter', 'Segoe UI', sans-serif",
        "monospace": "'JetBrains Mono', 'Fira Code', monospace",
        "size_base": "14px",
        "size_sm": "12px",
        "size_lg": "16px",
        "size_xl": "18px",
        "size_2xl": "24px",
        "size_3xl": "30px"
    })

    # Espacamento e Bordas
    spacing = Column(JSON, default=lambda: {
        "border_radius": "8px",
        "border_radius_sm": "4px",
        "border_radius_lg": "12px",
        "border_radius_full": "9999px",
        "shadow_sm": "0 1px 2px rgba(0,0,0,0.05)",
        "shadow": "0 4px 6px rgba(0,0,0,0.1)",
        "shadow_lg": "0 10px 15px rgba(0,0,0,0.1)"
    })

    # Footer
    footer = Column(JSON, default=lambda: {
        "show_footer": True,
        "text": None,  # Texto customizado
        "links": [],   # [{"label": "Termos", "url": "/termos"}, ...]
        "copyright": None,  # "2024 Empresa. Todos os direitos reservados."
        "show_powered_by": True  # Mostrar "Powered by Fabrica de Agentes"
    })

    # Emails (White Label)
    email_branding = Column(JSON, default=lambda: {
        "from_name": None,           # Nome do remetente
        "from_email": None,          # Email do remetente (validado)
        "reply_to": None,            # Reply-to
        "header_logo": None,         # Logo no header do email
        "footer_text": None,         # Texto no footer
        "primary_color": "#003B4A",  # Cor primaria dos botoes
        "template_style": "modern"   # modern, minimal, classic
    })

    # CSS Customizado (avancado)
    custom_css = Column(Text, nullable=True)  # CSS adicional

    # JavaScript Customizado (apenas enterprise)
    custom_js = Column(Text, nullable=True)   # JS adicional (com restricoes)

    # Pagina de Login
    login_config = Column(JSON, default=lambda: {
        "background_image": None,
        "background_color": "#F3F4F6",
        "show_logo": True,
        "welcome_text": "Bem-vindo de volta",
        "subtitle": "Faca login para continuar"
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "logo_url": self.logo_url,
            "logo_dark_url": self.logo_dark_url,
            "favicon_url": self.favicon_url,
            "logo_width": self.logo_width,
            "logo_height": self.logo_height,
            "display_name": self.display_name,
            "tagline": self.tagline,
            "colors": self.colors or {},
            "dark_mode_enabled": self.dark_mode_enabled,
            "dark_colors": self.dark_colors or {},
            "fonts": self.fonts or {},
            "spacing": self.spacing or {},
            "footer": self.footer or {},
            "login_config": self.login_config or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def generate_css_variables(self, dark_mode: bool = False) -> str:
        """Gera CSS variables para o tema do tenant"""
        colors = self.dark_colors if dark_mode and self.dark_mode_enabled else self.colors
        fonts = self.fonts or {}
        spacing = self.spacing or {}

        css_vars = [":root {"]

        # Cores
        for key, value in (colors or {}).items():
            css_key = key.replace("_", "-")
            css_vars.append(f"  --color-{css_key}: {value};")

        # Fontes
        for key, value in fonts.items():
            css_key = key.replace("_", "-")
            css_vars.append(f"  --font-{css_key}: {value};")

        # Espacamento
        for key, value in spacing.items():
            css_key = key.replace("_", "-")
            css_vars.append(f"  --{css_key}: {value};")

        css_vars.append("}")

        # Adicionar CSS customizado
        if self.custom_css:
            css_vars.append("\n/* Custom CSS */")
            css_vars.append(self.custom_css)

        return "\n".join(css_vars)

    def __repr__(self):
        return f"<BrandingConfig {self.tenant_id}>"


# =============================================================================
# MEMBER STATUS ENUM
# =============================================================================

class MemberStatus(str, Enum):
    """Status do membro no tenant"""
    ACTIVE = "active"           # Membro ativo
    INVITED = "invited"         # Convite enviado, aguardando aceite
    SUSPENDED = "suspended"     # Suspenso temporariamente
    DEACTIVATED = "deactivated" # Desativado permanentemente


# =============================================================================
# TENANT_MEMBER - Membros do Tenant
# =============================================================================

class TenantMember(Base):
    """
    Modelo para Membros de um Tenant

    Define quem tem acesso ao tenant e com qual role.
    Um usuario pode ser membro de multiplos tenants.

    Issue #120: Expandido com status e campos adicionais
    """
    __tablename__ = "tenant_members"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamentos
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="members")

    user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)

    # Role no tenant (admin, member, viewer)
    tenant_role = Column(String(30), default=MemberRole.MEMBER.value, nullable=False)

    # Alias para compatibilidade
    @property
    def role(self):
        return self.tenant_role

    @role.setter
    def role(self, value):
        self.tenant_role = value

    # Status do membro (Issue #120)
    status = Column(String(30), default=MemberStatus.ACTIVE.value, index=True)

    # Campo legacy para compatibilidade
    active = Column(Boolean, default=True)

    # Permissoes customizadas (override de role)
    custom_permissions = Column(JSON, default=dict)  # {"can_delete_projects": True, ...}

    # Configuracoes do membro
    settings = Column(JSON, default=lambda: {
        "notifications_enabled": True,
        "email_digest": "daily",
        "default_project": None
    })

    # Timestamps
    joined_at = Column(DateTime, default=datetime.utcnow)
    last_active_at = Column(DateTime, nullable=True)
    invited_by = Column(String(100), nullable=True)

    # Campos adicionais para gestao
    invited_at = Column(DateTime, nullable=True)
    suspended_at = Column(DateTime, nullable=True)
    suspension_reason = Column(Text, nullable=True)

    # Constraint para evitar duplicatas
    __table_args__ = (
        UniqueConstraint('tenant_id', 'user_id', name='uix_tenant_member'),
    )

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "tenant_role": self.tenant_role,
            "role": self.tenant_role,  # Alias para compatibilidade
            "status": self.status,
            "active": self.active,
            "custom_permissions": self.custom_permissions or {},
            "settings": self.settings or {},
            "joined_at": self.joined_at.isoformat() if self.joined_at else None,
            "last_active_at": self.last_active_at.isoformat() if self.last_active_at else None,
            "invited_by": self.invited_by,
            "invited_at": self.invited_at.isoformat() if self.invited_at else None,
            "suspended_at": self.suspended_at.isoformat() if self.suspended_at else None,
            "suspension_reason": self.suspension_reason
        }

    def is_active_member(self) -> bool:
        """Verifica se o membro esta ativo"""
        return self.status == MemberStatus.ACTIVE.value and self.active

    def suspend(self, reason: str = None) -> None:
        """Suspende o membro"""
        self.status = MemberStatus.SUSPENDED.value
        self.active = False
        self.suspended_at = datetime.utcnow()
        self.suspension_reason = reason

    def activate(self) -> None:
        """Ativa/reativa o membro"""
        self.status = MemberStatus.ACTIVE.value
        self.active = True
        self.suspended_at = None
        self.suspension_reason = None

    def has_permission(self, permission: str) -> bool:
        """Verifica se membro tem permissao especifica"""
        # Permissoes por role
        role_permissions = {
            MemberRole.OWNER.value: ["*"],  # Todas as permissoes
            MemberRole.ADMIN.value: [
                "manage_members", "manage_projects", "manage_settings",
                "view_billing", "create_stories", "delete_stories"
            ],
            MemberRole.MEMBER.value: [
                "create_projects", "create_stories", "edit_stories"
            ],
            MemberRole.VIEWER.value: ["view"],
            MemberRole.BILLING.value: ["view_billing", "manage_billing"]
        }

        # Owner tem todas as permissoes
        if self.role == MemberRole.OWNER.value:
            return True

        # Verificar permissoes customizadas primeiro
        if self.custom_permissions and permission in self.custom_permissions:
            return self.custom_permissions[permission]

        # Verificar permissoes do role
        permissions = role_permissions.get(self.role, [])
        return permission in permissions

    def __repr__(self):
        return f"<TenantMember {self.tenant_id}:{self.user_id} [{self.tenant_role}]>"


# =============================================================================
# PROJECT ROLE ENUM
# =============================================================================

class ProjectRole(str, Enum):
    """Roles de membros em um projeto (Issue #120)"""
    OWNER = "owner"             # Dono do projeto (pode deletar)
    ADMIN = "admin"             # Administrador (gerencia membros do projeto)
    DEVELOPER = "developer"     # Desenvolvedor (cria stories, tasks)
    VIEWER = "viewer"           # Apenas visualizacao


# =============================================================================
# PROJECT_MEMBER - Membros de um Projeto (Issue #120)
# =============================================================================

class ProjectMember(Base):
    """
    Modelo para Membros de um Projeto

    Issue #120: Define quem tem acesso a um projeto especifico e com qual role.
    Permite controle granular de acesso por projeto dentro de um tenant.

    Hierarquia:
    - Tenant -> TenantMember (acesso ao tenant)
    - Project -> ProjectMember (acesso ao projeto)

    Um usuario pode ter roles diferentes em projetos diferentes do mesmo tenant.
    """
    __tablename__ = "project_members"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)

    # Relacionamento com usuario
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)

    # Role no projeto (owner, admin, developer, viewer)
    project_role = Column(String(30), default=ProjectRole.DEVELOPER.value, nullable=False)

    # Permissoes customizadas JSON (override de role)
    permissions = Column(JSON, default=lambda: {
        "can_create_stories": True,
        "can_edit_stories": True,
        "can_delete_stories": False,
        "can_manage_sprints": False,
        "can_manage_members": False,
        "can_export_data": False,
        "can_run_workers": True
    })

    # Status
    status = Column(String(30), default=MemberStatus.ACTIVE.value, index=True)
    active = Column(Boolean, default=True)

    # Timestamps
    added_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    added_by = Column(String(100), nullable=True)

    # Constraint para evitar duplicatas
    __table_args__ = (
        UniqueConstraint('project_id', 'user_id', name='uix_project_member'),
    )

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "project_id": self.project_id,
            "user_id": self.user_id,
            "project_role": self.project_role,
            "permissions": self.permissions or {},
            "status": self.status,
            "active": self.active,
            "added_at": self.added_at.isoformat() if self.added_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "added_by": self.added_by
        }

    def has_permission(self, permission: str) -> bool:
        """Verifica se membro tem permissao especifica no projeto"""
        # Permissoes por role do projeto
        role_permissions = {
            ProjectRole.OWNER.value: ["*"],  # Todas as permissoes
            ProjectRole.ADMIN.value: [
                "can_create_stories", "can_edit_stories", "can_delete_stories",
                "can_manage_sprints", "can_manage_members", "can_export_data", "can_run_workers"
            ],
            ProjectRole.DEVELOPER.value: [
                "can_create_stories", "can_edit_stories", "can_run_workers"
            ],
            ProjectRole.VIEWER.value: []  # Apenas visualizacao
        }

        # Owner tem todas as permissoes
        if self.project_role == ProjectRole.OWNER.value:
            return True

        # Verificar permissoes customizadas primeiro
        if self.permissions and permission in self.permissions:
            return self.permissions[permission]

        # Verificar permissoes do role
        permissions = role_permissions.get(self.project_role, [])
        return permission in permissions

    def is_owner(self) -> bool:
        """Verifica se e owner do projeto"""
        return self.project_role == ProjectRole.OWNER.value

    def is_admin_or_owner(self) -> bool:
        """Verifica se e admin ou owner"""
        return self.project_role in [ProjectRole.OWNER.value, ProjectRole.ADMIN.value]

    def __repr__(self):
        return f"<ProjectMember {self.project_id}:{self.user_id} [{self.project_role}]>"


# =============================================================================
# SSO_CONFIG - Configuracao de SSO/SAML
# =============================================================================

class SSOConfig(Base):
    """
    Modelo para Configuracao de SSO/SAML do Tenant

    Permite integracao com:
    - Azure Active Directory
    - Google Workspace
    - Okta
    - SAML 2.0 generico
    - OpenID Connect
    """
    __tablename__ = "sso_configs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), unique=True, nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="sso_config")

    # Status
    enabled = Column(Boolean, default=False)
    provider = Column(String(30), default=SSOProvider.SAML.value)

    # SAML Config
    saml_config = Column(JSON, default=lambda: {
        "entity_id": None,
        "sso_url": None,
        "slo_url": None,
        "certificate": None,
        "name_id_format": "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress",
        "authn_context": "urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport"
    })

    # OAuth/OIDC Config
    oauth_config = Column(JSON, default=lambda: {
        "client_id": None,
        "client_secret": None,  # Criptografado
        "authorization_url": None,
        "token_url": None,
        "userinfo_url": None,
        "scopes": ["openid", "email", "profile"]
    })

    # Mapeamento de Atributos
    attribute_mapping = Column(JSON, default=lambda: {
        "email": "email",
        "first_name": "given_name",
        "last_name": "family_name",
        "display_name": "name",
        "groups": "groups"
    })

    # Provisionamento Automatico (SCIM)
    scim_enabled = Column(Boolean, default=False)
    scim_config = Column(JSON, default=lambda: {
        "endpoint": None,
        "token": None,  # Criptografado
        "auto_create_users": True,
        "auto_deactivate_users": True,
        "sync_groups": False
    })

    # Regras de Provisionamento
    provisioning_rules = Column(JSON, default=lambda: {
        "default_role": "member",
        "domain_restriction": [],  # ["empresa.com"]
        "group_role_mapping": {}   # {"Admins": "admin", "Devs": "member"}
    })

    # Configuracoes Adicionais
    settings = Column(JSON, default=lambda: {
        "force_sso": False,         # Obrigar login via SSO
        "allow_password_login": True,  # Permitir login com senha
        "jit_provisioning": True,   # Just-in-time provisioning
        "session_timeout": 480      # Timeout em minutos
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    last_sync_at = Column(DateTime, nullable=True)

    def to_dict(self, include_sensitive: bool = False) -> Dict[str, Any]:
        """Converte para dicionario"""
        data = {
            "tenant_id": self.tenant_id,
            "enabled": self.enabled,
            "provider": self.provider,
            "attribute_mapping": self.attribute_mapping or {},
            "scim_enabled": self.scim_enabled,
            "provisioning_rules": self.provisioning_rules or {},
            "settings": self.settings or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "last_sync_at": self.last_sync_at.isoformat() if self.last_sync_at else None
        }

        if include_sensitive:
            data["saml_config"] = self.saml_config
            data["oauth_config"] = self.oauth_config
            data["scim_config"] = self.scim_config

        return data

    def __repr__(self):
        return f"<SSOConfig {self.tenant_id}: {self.provider} [{'enabled' if self.enabled else 'disabled'}]>"


# =============================================================================
# TENANT_INVITE - Convites para Membros
# =============================================================================

class TenantInvite(Base):
    """
    Modelo para Convites de Membros

    Gerencia convites pendentes para novos membros do tenant.
    """
    __tablename__ = "tenant_invites"

    id = Column(Integer, primary_key=True, autoincrement=True)
    invite_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="invites")

    # Convite
    email = Column(String(255), nullable=False, index=True)
    role = Column(String(30), default=MemberRole.MEMBER.value)

    # Token (para link de convite)
    token = Column(String(255), unique=True, nullable=False)

    # Status
    status = Column(String(30), default=InviteStatus.PENDING.value)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    expires_at = Column(DateTime, nullable=False)
    accepted_at = Column(DateTime, nullable=True)

    # Quem convidou
    invited_by = Column(String(100), nullable=False)

    # Mensagem personalizada
    message = Column(Text, nullable=True)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "invite_id": self.invite_id,
            "tenant_id": self.tenant_id,
            "email": self.email,
            "role": self.role,
            "status": self.status,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "accepted_at": self.accepted_at.isoformat() if self.accepted_at else None,
            "invited_by": self.invited_by
        }

    def is_expired(self) -> bool:
        """Verifica se convite expirou"""
        return datetime.utcnow() > self.expires_at

    def is_valid(self) -> bool:
        """Verifica se convite e valido"""
        return self.status == InviteStatus.PENDING.value and not self.is_expired()

    def __repr__(self):
        return f"<TenantInvite {self.invite_id}: {self.email} [{self.status}]>"


# =============================================================================
# TENANT_USAGE_LOG - Registro de Uso para Billing
# =============================================================================

class TenantUsageLog(Base):
    """
    Modelo para Registro de Uso do Tenant

    Registra o consumo de recursos para faturamento:
    - Tokens de IA consumidos
    - Chamadas de API
    - Storage utilizado
    - Workers executados
    """
    __tablename__ = "tenant_usage_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="usage_logs")

    # Periodo
    period_start = Column(DateTime, nullable=False, index=True)
    period_end = Column(DateTime, nullable=False)

    # Metricas de Uso
    tokens_input = Column(Integer, default=0)   # Tokens de input
    tokens_output = Column(Integer, default=0)  # Tokens de output
    api_calls = Column(Integer, default=0)      # Chamadas de API
    storage_bytes = Column(Integer, default=0)  # Bytes armazenados
    workers_executed = Column(Integer, default=0)  # Execucoes de workers
    stories_created = Column(Integer, default=0)   # Stories criadas
    projects_created = Column(Integer, default=0)  # Projetos criados

    # Custo Calculado
    cost_tokens = Column(Float, default=0.0)    # Custo de tokens
    cost_storage = Column(Float, default=0.0)   # Custo de storage
    cost_workers = Column(Float, default=0.0)   # Custo de workers
    cost_total = Column(Float, default=0.0)     # Custo total

    # Detalhes
    details = Column(JSON, default=dict)  # Detalhes granulares

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "period_start": self.period_start.isoformat() if self.period_start else None,
            "period_end": self.period_end.isoformat() if self.period_end else None,
            "tokens_input": self.tokens_input,
            "tokens_output": self.tokens_output,
            "tokens_total": self.tokens_input + self.tokens_output,
            "api_calls": self.api_calls,
            "storage_bytes": self.storage_bytes,
            "storage_gb": round(self.storage_bytes / (1024**3), 2),
            "workers_executed": self.workers_executed,
            "stories_created": self.stories_created,
            "projects_created": self.projects_created,
            "cost_tokens": self.cost_tokens,
            "cost_storage": self.cost_storage,
            "cost_workers": self.cost_workers,
            "cost_total": self.cost_total,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<TenantUsageLog {self.tenant_id}: {self.period_start.date()} - ${self.cost_total:.2f}>"


# =============================================================================
# AUDIT_LOG - Logs de Auditoria por Tenant
# =============================================================================

class AuditAction(str, Enum):
    """Tipos de acoes auditadas"""
    CREATE = "create"
    UPDATE = "update"
    DELETE = "delete"
    LOGIN = "login"
    LOGOUT = "logout"
    EXPORT = "export"
    SHARE = "share"
    INVITE = "invite"
    SETTINGS_CHANGE = "settings_change"
    PERMISSION_CHANGE = "permission_change"


class TenantAuditLog(Base):
    """
    Modelo para Logs de Auditoria do Tenant

    Registra todas as acoes importantes para compliance e seguranca.
    """
    __tablename__ = "tenant_audit_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id"), nullable=False, index=True)

    # Quem fez a acao
    user_id = Column(Integer, nullable=True)
    user_email = Column(String(255), nullable=True)
    user_ip = Column(String(50), nullable=True)
    user_agent = Column(String(500), nullable=True)

    # Acao
    action = Column(String(50), nullable=False, index=True)
    resource_type = Column(String(50), nullable=False, index=True)  # project, story, member, etc.
    resource_id = Column(String(50), nullable=True, index=True)

    # Detalhes
    description = Column(Text, nullable=True)
    old_value = Column(JSON, default=dict)  # Valor anterior (para updates)
    new_value = Column(JSON, default=dict)  # Novo valor
    audit_metadata = Column(JSON, default=dict)   # Dados adicionais (renamed from metadata)

    # Resultado
    success = Column(Boolean, default=True)
    error_message = Column(Text, nullable=True)

    # Timestamp
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "id": self.id,
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "user_email": self.user_email,
            "action": self.action,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "description": self.description,
            "old_value": self.old_value,
            "new_value": self.new_value,
            "success": self.success,
            "error_message": self.error_message,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<TenantAuditLog {self.tenant_id}: {self.action} {self.resource_type}>"
