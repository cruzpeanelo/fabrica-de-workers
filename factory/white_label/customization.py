# -*- coding: utf-8 -*-
"""
White Label Sistema Completo de Personalizacao (Issue #119)
============================================================

Implementa:
- Logo personalizado por tenant
- Cores personalizadas (tema completo)
- Dominio customizado
- Email templates personalizados
- Favicon customizado
- CSS customizado
- Configuracoes de idioma e timezone

Autor: Plataforma E
"""

import os
import re
import uuid
import json
import hashlib
import logging
from datetime import datetime
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field, asdict
from pathlib import Path
from enum import Enum

from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean
from sqlalchemy.orm import Session, relationship

# Configurar logging
logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS
# =============================================================================

class ThemeMode(str, Enum):
    """Modos de tema"""
    LIGHT = "light"
    DARK = "dark"
    AUTO = "auto"  # Segue preferencia do sistema


class EmailTemplateType(str, Enum):
    """Tipos de templates de email"""
    WELCOME = "welcome"
    INVITE = "invite"
    PASSWORD_RESET = "password_reset"
    INVOICE = "invoice"
    USAGE_ALERT = "usage_alert"
    TASK_COMPLETED = "task_completed"
    STORY_COMPLETED = "story_completed"
    REPORT_WEEKLY = "report_weekly"
    REPORT_MONTHLY = "report_monthly"


class FontFamily(str, Enum):
    """Familias de fontes disponiveis"""
    INTER = "Inter"
    ROBOTO = "Roboto"
    OPEN_SANS = "Open Sans"
    LATO = "Lato"
    POPPINS = "Poppins"
    MONTSERRAT = "Montserrat"
    NUNITO = "Nunito"
    SYSTEM = "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class ColorPalette:
    """Paleta de cores para tema"""
    primary: str = "#003B4A"
    primary_hover: str = "#00526A"
    primary_light: str = "#E6F0F2"
    secondary: str = "#FF6C00"
    secondary_hover: str = "#E65C00"
    success: str = "#10B981"
    warning: str = "#F59E0B"
    error: str = "#EF4444"
    info: str = "#3B82F6"
    background: str = "#F3F4F6"
    surface: str = "#FFFFFF"
    surface_hover: str = "#F9FAFB"
    text_primary: str = "#1F2937"
    text_secondary: str = "#6B7280"
    text_muted: str = "#9CA3AF"
    text_on_primary: str = "#FFFFFF"
    border: str = "#E5E7EB"
    border_hover: str = "#D1D5DB"
    header_bg: str = "#003B4A"
    header_text: str = "#FFFFFF"
    sidebar_bg: str = "#FFFFFF"
    sidebar_text: str = "#374151"

    def to_dict(self) -> Dict[str, str]:
        return asdict(self)

    def to_css_variables(self) -> str:
        """Gera variaveis CSS"""
        lines = []
        for key, value in self.to_dict().items():
            css_key = key.replace("_", "-")
            lines.append(f"  --color-{css_key}: {value};")
        return "\n".join(lines)


@dataclass
class DarkColorPalette(ColorPalette):
    """Paleta de cores para tema escuro"""
    primary: str = "#60A5FA"
    primary_hover: str = "#93C5FD"
    primary_light: str = "#1E3A5F"
    secondary: str = "#FB923C"
    secondary_hover: str = "#FDBA74"
    success: str = "#34D399"
    warning: str = "#FBBF24"
    error: str = "#F87171"
    info: str = "#60A5FA"
    background: str = "#111827"
    surface: str = "#1F2937"
    surface_hover: str = "#374151"
    text_primary: str = "#F9FAFB"
    text_secondary: str = "#D1D5DB"
    text_muted: str = "#9CA3AF"
    text_on_primary: str = "#1F2937"
    border: str = "#374151"
    border_hover: str = "#4B5563"
    header_bg: str = "#1F2937"
    header_text: str = "#F9FAFB"
    sidebar_bg: str = "#1F2937"
    sidebar_text: str = "#D1D5DB"


@dataclass
class ThemeConfig:
    """Configuracao de tema"""
    mode: str = ThemeMode.LIGHT.value
    light_colors: ColorPalette = field(default_factory=ColorPalette)
    dark_colors: ColorPalette = field(default_factory=DarkColorPalette)
    font_primary: str = FontFamily.INTER.value
    font_heading: str = FontFamily.INTER.value
    font_monospace: str = "'JetBrains Mono', 'Fira Code', monospace"
    border_radius: str = "8px"
    border_radius_sm: str = "4px"
    border_radius_lg: str = "12px"
    shadow_sm: str = "0 1px 2px rgba(0,0,0,0.05)"
    shadow: str = "0 4px 6px rgba(0,0,0,0.1)"
    shadow_lg: str = "0 10px 15px rgba(0,0,0,0.1)"
    header_height: str = "64px"
    sidebar_width: str = "256px"
    content_max_width: str = "1280px"

    def to_dict(self) -> Dict[str, Any]:
        result = {
            "mode": self.mode,
            "light_colors": self.light_colors.to_dict() if isinstance(self.light_colors, ColorPalette) else self.light_colors,
            "dark_colors": self.dark_colors.to_dict() if isinstance(self.dark_colors, ColorPalette) else self.dark_colors,
            "font_primary": self.font_primary,
            "font_heading": self.font_heading,
            "font_monospace": self.font_monospace,
            "border_radius": self.border_radius,
            "border_radius_sm": self.border_radius_sm,
            "border_radius_lg": self.border_radius_lg,
            "shadow_sm": self.shadow_sm,
            "shadow": self.shadow,
            "shadow_lg": self.shadow_lg,
            "header_height": self.header_height,
            "sidebar_width": self.sidebar_width,
            "content_max_width": self.content_max_width,
        }
        return result

    def generate_css(self, dark_mode: bool = False) -> str:
        """Gera CSS completo para o tema"""
        colors = self.dark_colors if dark_mode else self.light_colors
        if isinstance(colors, dict):
            colors = ColorPalette(**colors) if not dark_mode else DarkColorPalette(**colors)

        css = f""":root {{
{colors.to_css_variables()}
  --font-primary: '{self.font_primary}', sans-serif;
  --font-heading: '{self.font_heading}', sans-serif;
  --font-monospace: {self.font_monospace};
  --border-radius: {self.border_radius};
  --border-radius-sm: {self.border_radius_sm};
  --border-radius-lg: {self.border_radius_lg};
  --shadow-sm: {self.shadow_sm};
  --shadow: {self.shadow};
  --shadow-lg: {self.shadow_lg};
  --header-height: {self.header_height};
  --sidebar-width: {self.sidebar_width};
  --content-max-width: {self.content_max_width};
}}
"""
        return css


@dataclass
class BrandingConfig:
    """Configuracao de branding"""
    tenant_id: str = ""
    display_name: str = ""
    tagline: str = ""
    logo_url: str = ""
    logo_dark_url: str = ""  # Logo para tema escuro
    favicon_url: str = ""
    apple_touch_icon_url: str = ""
    og_image_url: str = ""  # Open Graph image para compartilhamento
    theme: ThemeConfig = field(default_factory=ThemeConfig)
    custom_css: str = ""
    custom_js: str = ""
    footer_text: str = ""
    footer_links: List[Dict[str, str]] = field(default_factory=list)
    support_email: str = ""
    support_phone: str = ""
    support_url: str = ""
    docs_url: str = ""
    privacy_policy_url: str = ""
    terms_of_service_url: str = ""
    social_links: Dict[str, str] = field(default_factory=dict)
    meta_title_suffix: str = ""
    meta_description: str = ""
    google_analytics_id: str = ""
    intercom_app_id: str = ""

    def to_dict(self) -> Dict[str, Any]:
        result = {
            "tenant_id": self.tenant_id,
            "display_name": self.display_name,
            "tagline": self.tagline,
            "logo_url": self.logo_url,
            "logo_dark_url": self.logo_dark_url,
            "favicon_url": self.favicon_url,
            "apple_touch_icon_url": self.apple_touch_icon_url,
            "og_image_url": self.og_image_url,
            "theme": self.theme.to_dict() if isinstance(self.theme, ThemeConfig) else self.theme,
            "custom_css": self.custom_css,
            "custom_js": self.custom_js,
            "footer_text": self.footer_text,
            "footer_links": self.footer_links,
            "support_email": self.support_email,
            "support_phone": self.support_phone,
            "support_url": self.support_url,
            "docs_url": self.docs_url,
            "privacy_policy_url": self.privacy_policy_url,
            "terms_of_service_url": self.terms_of_service_url,
            "social_links": self.social_links,
            "meta_title_suffix": self.meta_title_suffix,
            "meta_description": self.meta_description,
            "google_analytics_id": self.google_analytics_id,
            "intercom_app_id": self.intercom_app_id,
        }
        return result


@dataclass
class DomainConfig:
    """Configuracao de dominio customizado"""
    tenant_id: str = ""
    domain: str = ""
    subdomain: str = ""
    ssl_enabled: bool = True
    ssl_certificate: str = ""
    ssl_private_key: str = ""
    ssl_auto_renew: bool = True
    dns_verified: bool = False
    dns_verification_token: str = ""
    dns_verification_method: str = "cname"  # cname, txt
    cloudflare_zone_id: str = ""
    redirect_www: bool = True
    force_https: bool = True
    custom_headers: Dict[str, str] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)
    verified_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "domain": self.domain,
            "subdomain": self.subdomain,
            "ssl_enabled": self.ssl_enabled,
            "ssl_auto_renew": self.ssl_auto_renew,
            "dns_verified": self.dns_verified,
            "dns_verification_method": self.dns_verification_method,
            "redirect_www": self.redirect_www,
            "force_https": self.force_https,
            "custom_headers": self.custom_headers,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "verified_at": self.verified_at.isoformat() if self.verified_at else None,
        }

    def get_full_domain(self) -> str:
        """Retorna dominio completo"""
        if self.subdomain:
            return f"{self.subdomain}.{self.domain}"
        return self.domain


@dataclass
class EmailTemplateConfig:
    """Configuracao de template de email"""
    tenant_id: str = ""
    template_type: str = ""
    subject: str = ""
    body_html: str = ""
    body_text: str = ""
    from_name: str = ""
    from_email: str = ""
    reply_to: str = ""
    header_logo_url: str = ""
    header_background_color: str = "#003B4A"
    header_text_color: str = "#FFFFFF"
    footer_text: str = ""
    footer_background_color: str = "#F3F4F6"
    button_color: str = "#FF6C00"
    button_text_color: str = "#FFFFFF"
    variables: Dict[str, str] = field(default_factory=dict)
    is_active: bool = True
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "template_type": self.template_type,
            "subject": self.subject,
            "body_html": self.body_html,
            "body_text": self.body_text,
            "from_name": self.from_name,
            "from_email": self.from_email,
            "reply_to": self.reply_to,
            "header_logo_url": self.header_logo_url,
            "header_background_color": self.header_background_color,
            "header_text_color": self.header_text_color,
            "footer_text": self.footer_text,
            "footer_background_color": self.footer_background_color,
            "button_color": self.button_color,
            "button_text_color": self.button_text_color,
            "variables": self.variables,
            "is_active": self.is_active,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
        }

    def render(self, context: Dict[str, Any]) -> Dict[str, str]:
        """
        Renderiza o template com contexto.

        Args:
            context: Dicionario com variaveis para substituir

        Returns:
            Dict com subject, body_html e body_text renderizados
        """
        def replace_vars(text: str, ctx: Dict[str, Any]) -> str:
            if not text:
                return text
            for key, value in ctx.items():
                placeholder = f"{{{{{key}}}}}"
                text = text.replace(placeholder, str(value))
            return text

        # Merge context com variaveis padrao
        full_context = {**self.variables, **context}

        return {
            "subject": replace_vars(self.subject, full_context),
            "body_html": replace_vars(self.body_html, full_context),
            "body_text": replace_vars(self.body_text, full_context),
            "from_name": replace_vars(self.from_name, full_context),
            "from_email": self.from_email,
            "reply_to": self.reply_to,
        }


# =============================================================================
# WHITE LABEL SERVICE
# =============================================================================

class WhiteLabelService:
    """
    Servico principal de White Label.

    Responsabilidades:
    - Gerenciar configuracoes de branding por tenant
    - Upload e processamento de logos
    - Geracao de CSS personalizado
    - Gerenciamento de dominios customizados
    - Templates de email personalizados

    Uso:
        service = WhiteLabelService(db_session)

        # Atualizar branding
        service.update_branding(tenant_id, logo_url="...", primary_color="#FF0000")

        # Obter CSS personalizado
        css = service.get_custom_css(tenant_id)

        # Configurar dominio customizado
        service.setup_custom_domain(tenant_id, "app.empresa.com")
    """

    # Diretorio para uploads
    UPLOADS_DIR = Path("uploads/white_label")

    # Tamanho maximo de logo (em bytes)
    MAX_LOGO_SIZE = 2 * 1024 * 1024  # 2MB

    # Formatos aceitos
    ALLOWED_IMAGE_TYPES = ["image/png", "image/jpeg", "image/svg+xml", "image/webp"]

    def __init__(self, db: Session):
        """
        Inicializa o servico.

        Args:
            db: Sessao do banco de dados SQLAlchemy
        """
        self.db = db
        self._ensure_uploads_dir()

    def _ensure_uploads_dir(self):
        """Garante que diretorio de uploads existe"""
        self.UPLOADS_DIR.mkdir(parents=True, exist_ok=True)

    def _generate_id(self, prefix: str) -> str:
        """Gera um ID unico com prefixo"""
        return f"{prefix}-{uuid.uuid4().hex[:12].upper()}"

    # =========================================================================
    # BRANDING
    # =========================================================================

    def get_branding(self, tenant_id: str) -> Optional[BrandingConfig]:
        """
        Obtem configuracoes de branding do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            BrandingConfig ou None
        """
        try:
            # Tentar buscar do banco
            from factory.database.connection import Base

            result = self.db.execute(
                f"SELECT config FROM tenant_branding WHERE tenant_id = :tenant_id",
                {"tenant_id": tenant_id}
            ).fetchone()

            if result and result[0]:
                config_data = json.loads(result[0]) if isinstance(result[0], str) else result[0]
                return self._dict_to_branding_config(config_data)

        except Exception as e:
            logger.debug(f"Branding nao encontrado no banco para {tenant_id}: {e}")

        # Retornar configuracao padrao
        return BrandingConfig(
            tenant_id=tenant_id,
            display_name=f"Tenant {tenant_id}",
        )

    def update_branding(
        self,
        tenant_id: str,
        **kwargs
    ) -> BrandingConfig:
        """
        Atualiza configuracoes de branding.

        Args:
            tenant_id: ID do tenant
            **kwargs: Campos a atualizar

        Returns:
            BrandingConfig atualizado
        """
        # Obter config atual
        current = self.get_branding(tenant_id) or BrandingConfig(tenant_id=tenant_id)

        # Atualizar campos
        for key, value in kwargs.items():
            if hasattr(current, key):
                if key == "theme" and isinstance(value, dict):
                    # Converter dict para ThemeConfig
                    current.theme = self._dict_to_theme_config(value)
                else:
                    setattr(current, key, value)

        # Salvar no banco
        try:
            config_json = json.dumps(current.to_dict())

            # Verificar se existe
            existing = self.db.execute(
                "SELECT id FROM tenant_branding WHERE tenant_id = :tenant_id",
                {"tenant_id": tenant_id}
            ).fetchone()

            if existing:
                self.db.execute(
                    """UPDATE tenant_branding
                       SET config = :config, updated_at = :updated_at
                       WHERE tenant_id = :tenant_id""",
                    {
                        "tenant_id": tenant_id,
                        "config": config_json,
                        "updated_at": datetime.utcnow()
                    }
                )
            else:
                self.db.execute(
                    """INSERT INTO tenant_branding (tenant_id, config, created_at, updated_at)
                       VALUES (:tenant_id, :config, :created_at, :updated_at)""",
                    {
                        "tenant_id": tenant_id,
                        "config": config_json,
                        "created_at": datetime.utcnow(),
                        "updated_at": datetime.utcnow()
                    }
                )

            self.db.commit()
            logger.info(f"Branding atualizado para tenant {tenant_id}")

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao salvar branding: {e}")
            raise

        return current

    def upload_logo(
        self,
        tenant_id: str,
        file_content: bytes,
        file_name: str,
        content_type: str,
        logo_type: str = "logo"  # logo, logo_dark, favicon
    ) -> str:
        """
        Faz upload de logo.

        Args:
            tenant_id: ID do tenant
            file_content: Conteudo do arquivo
            file_name: Nome do arquivo
            content_type: Tipo MIME
            logo_type: Tipo de logo

        Returns:
            URL do logo salvo

        Raises:
            ValueError: Se arquivo invalido
        """
        # Validar tipo
        if content_type not in self.ALLOWED_IMAGE_TYPES:
            raise ValueError(f"Tipo de arquivo nao permitido: {content_type}")

        # Validar tamanho
        if len(file_content) > self.MAX_LOGO_SIZE:
            raise ValueError(f"Arquivo muito grande. Maximo: {self.MAX_LOGO_SIZE // (1024*1024)}MB")

        # Gerar nome unico
        ext = Path(file_name).suffix or self._get_extension(content_type)
        hash_name = hashlib.md5(file_content).hexdigest()[:12]
        new_name = f"{tenant_id}_{logo_type}_{hash_name}{ext}"

        # Criar diretorio do tenant
        tenant_dir = self.UPLOADS_DIR / tenant_id
        tenant_dir.mkdir(parents=True, exist_ok=True)

        # Salvar arquivo
        file_path = tenant_dir / new_name
        file_path.write_bytes(file_content)

        # Gerar URL
        url = f"/uploads/white_label/{tenant_id}/{new_name}"

        # Atualizar branding
        field_map = {
            "logo": "logo_url",
            "logo_dark": "logo_dark_url",
            "favicon": "favicon_url",
            "og_image": "og_image_url",
        }
        field_name = field_map.get(logo_type, "logo_url")
        self.update_branding(tenant_id, **{field_name: url})

        logger.info(f"Logo {logo_type} salvo para tenant {tenant_id}: {url}")
        return url

    def _get_extension(self, content_type: str) -> str:
        """Retorna extensao para content type"""
        extensions = {
            "image/png": ".png",
            "image/jpeg": ".jpg",
            "image/svg+xml": ".svg",
            "image/webp": ".webp",
        }
        return extensions.get(content_type, ".png")

    # =========================================================================
    # THEME & CSS
    # =========================================================================

    def update_theme(
        self,
        tenant_id: str,
        primary_color: Optional[str] = None,
        secondary_color: Optional[str] = None,
        mode: Optional[str] = None,
        font_primary: Optional[str] = None,
        **extra_colors
    ) -> ThemeConfig:
        """
        Atualiza tema do tenant.

        Args:
            tenant_id: ID do tenant
            primary_color: Cor primaria
            secondary_color: Cor secundaria
            mode: Modo do tema (light/dark/auto)
            font_primary: Fonte primaria
            **extra_colors: Outras cores

        Returns:
            ThemeConfig atualizado
        """
        branding = self.get_branding(tenant_id)
        theme = branding.theme if branding else ThemeConfig()

        if isinstance(theme, dict):
            theme = self._dict_to_theme_config(theme)

        # Atualizar cores light
        light_colors = theme.light_colors
        if isinstance(light_colors, dict):
            light_colors = ColorPalette(**light_colors)

        if primary_color:
            light_colors.primary = primary_color
            # Gerar cores derivadas
            light_colors.primary_hover = self._adjust_color(primary_color, 10)
            light_colors.primary_light = self._adjust_color(primary_color, 80)
            light_colors.header_bg = primary_color

        if secondary_color:
            light_colors.secondary = secondary_color
            light_colors.secondary_hover = self._adjust_color(secondary_color, -10)

        # Aplicar cores extras
        for color_name, color_value in extra_colors.items():
            if hasattr(light_colors, color_name):
                setattr(light_colors, color_name, color_value)

        theme.light_colors = light_colors

        # Atualizar outras propriedades
        if mode:
            theme.mode = mode
        if font_primary:
            theme.font_primary = font_primary

        # Salvar
        self.update_branding(tenant_id, theme=theme.to_dict())

        return theme

    def get_custom_css(self, tenant_id: str, dark_mode: bool = False) -> str:
        """
        Gera CSS customizado para o tenant.

        Args:
            tenant_id: ID do tenant
            dark_mode: Se deve gerar para modo escuro

        Returns:
            CSS string
        """
        branding = self.get_branding(tenant_id)
        if not branding:
            return self._default_css()

        theme = branding.theme
        if isinstance(theme, dict):
            theme = self._dict_to_theme_config(theme)

        # Gerar CSS do tema
        css = theme.generate_css(dark_mode)

        # Adicionar CSS customizado
        if branding.custom_css:
            css += f"\n/* Custom CSS */\n{branding.custom_css}\n"

        return css

    def _default_css(self) -> str:
        """CSS padrao da plataforma"""
        return ThemeConfig().generate_css()

    def _adjust_color(self, hex_color: str, percent: int) -> str:
        """
        Ajusta brilho de uma cor.

        Args:
            hex_color: Cor em hexadecimal (#RRGGBB)
            percent: Porcentagem de ajuste (-100 a 100)

        Returns:
            Cor ajustada em hex
        """
        # Remover # se presente
        hex_color = hex_color.lstrip('#')

        # Converter para RGB
        r = int(hex_color[0:2], 16)
        g = int(hex_color[2:4], 16)
        b = int(hex_color[4:6], 16)

        # Ajustar
        factor = percent / 100
        if factor > 0:
            # Clarear
            r = int(r + (255 - r) * factor)
            g = int(g + (255 - g) * factor)
            b = int(b + (255 - b) * factor)
        else:
            # Escurecer
            r = int(r * (1 + factor))
            g = int(g * (1 + factor))
            b = int(b * (1 + factor))

        # Limitar valores
        r = max(0, min(255, r))
        g = max(0, min(255, g))
        b = max(0, min(255, b))

        return f"#{r:02X}{g:02X}{b:02X}"

    # =========================================================================
    # CUSTOM DOMAIN
    # =========================================================================

    def setup_custom_domain(
        self,
        tenant_id: str,
        domain: str,
        subdomain: str = ""
    ) -> DomainConfig:
        """
        Configura dominio customizado para o tenant.

        Args:
            tenant_id: ID do tenant
            domain: Dominio principal (ex: empresa.com)
            subdomain: Subdominio (ex: app)

        Returns:
            DomainConfig com instrucoes de verificacao
        """
        # Validar dominio
        if not self._is_valid_domain(domain):
            raise ValueError(f"Dominio invalido: {domain}")

        # Gerar token de verificacao
        verification_token = uuid.uuid4().hex

        config = DomainConfig(
            tenant_id=tenant_id,
            domain=domain,
            subdomain=subdomain,
            dns_verification_token=verification_token,
            dns_verified=False,
        )

        # Salvar no banco
        try:
            self.db.execute(
                """INSERT INTO tenant_domains (tenant_id, domain, subdomain, dns_verification_token, created_at)
                   VALUES (:tenant_id, :domain, :subdomain, :token, :created_at)
                   ON CONFLICT (tenant_id, domain) DO UPDATE
                   SET subdomain = :subdomain, dns_verification_token = :token, updated_at = :created_at""",
                {
                    "tenant_id": tenant_id,
                    "domain": domain,
                    "subdomain": subdomain,
                    "token": verification_token,
                    "created_at": datetime.utcnow()
                }
            )
            self.db.commit()
        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao salvar dominio: {e}")
            raise

        logger.info(f"Dominio configurado para tenant {tenant_id}: {config.get_full_domain()}")
        return config

    def verify_domain(self, tenant_id: str, domain: str) -> bool:
        """
        Verifica DNS do dominio customizado.

        Args:
            tenant_id: ID do tenant
            domain: Dominio a verificar

        Returns:
            True se verificado com sucesso
        """
        # Buscar configuracao
        result = self.db.execute(
            """SELECT dns_verification_token FROM tenant_domains
               WHERE tenant_id = :tenant_id AND domain = :domain""",
            {"tenant_id": tenant_id, "domain": domain}
        ).fetchone()

        if not result:
            raise ValueError(f"Dominio nao configurado: {domain}")

        expected_token = result[0]

        # Verificar DNS (CNAME ou TXT)
        try:
            import socket
            # Simplificado - em producao usar DNS resolver adequado

            # Verificar se CNAME aponta para nosso servidor
            # ou se TXT record contem o token
            verified = True  # Placeholder - implementar verificacao real

            if verified:
                self.db.execute(
                    """UPDATE tenant_domains
                       SET dns_verified = 1, verified_at = :verified_at
                       WHERE tenant_id = :tenant_id AND domain = :domain""",
                    {
                        "tenant_id": tenant_id,
                        "domain": domain,
                        "verified_at": datetime.utcnow()
                    }
                )
                self.db.commit()
                logger.info(f"Dominio verificado: {domain}")
                return True

        except Exception as e:
            logger.error(f"Erro ao verificar dominio {domain}: {e}")

        return False

    def get_domain_config(self, tenant_id: str) -> Optional[DomainConfig]:
        """
        Obtem configuracao de dominio do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            DomainConfig ou None
        """
        try:
            result = self.db.execute(
                """SELECT domain, subdomain, dns_verified, dns_verification_token, created_at, verified_at
                   FROM tenant_domains WHERE tenant_id = :tenant_id""",
                {"tenant_id": tenant_id}
            ).fetchone()

            if result:
                return DomainConfig(
                    tenant_id=tenant_id,
                    domain=result[0],
                    subdomain=result[1] or "",
                    dns_verified=bool(result[2]),
                    dns_verification_token=result[3],
                    created_at=result[4],
                    verified_at=result[5],
                )
        except Exception as e:
            logger.debug(f"Dominio nao encontrado para {tenant_id}: {e}")

        return None

    def _is_valid_domain(self, domain: str) -> bool:
        """Valida formato do dominio"""
        pattern = r'^([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,}$'
        return bool(re.match(pattern, domain))

    # =========================================================================
    # EMAIL TEMPLATES
    # =========================================================================

    def get_email_template(
        self,
        tenant_id: str,
        template_type: str
    ) -> EmailTemplateConfig:
        """
        Obtem template de email personalizado.

        Args:
            tenant_id: ID do tenant
            template_type: Tipo do template (welcome, invite, etc)

        Returns:
            EmailTemplateConfig
        """
        try:
            result = self.db.execute(
                """SELECT config FROM tenant_email_templates
                   WHERE tenant_id = :tenant_id AND template_type = :type""",
                {"tenant_id": tenant_id, "type": template_type}
            ).fetchone()

            if result and result[0]:
                config_data = json.loads(result[0]) if isinstance(result[0], str) else result[0]
                return EmailTemplateConfig(**config_data)

        except Exception as e:
            logger.debug(f"Template {template_type} nao encontrado para {tenant_id}: {e}")

        # Retornar template padrao
        return self._get_default_email_template(tenant_id, template_type)

    def update_email_template(
        self,
        tenant_id: str,
        template_type: str,
        **kwargs
    ) -> EmailTemplateConfig:
        """
        Atualiza template de email.

        Args:
            tenant_id: ID do tenant
            template_type: Tipo do template
            **kwargs: Campos a atualizar

        Returns:
            EmailTemplateConfig atualizado
        """
        # Obter template atual
        current = self.get_email_template(tenant_id, template_type)

        # Atualizar campos
        for key, value in kwargs.items():
            if hasattr(current, key):
                setattr(current, key, value)

        current.updated_at = datetime.utcnow()

        # Salvar
        try:
            config_json = json.dumps(current.to_dict())

            self.db.execute(
                """INSERT INTO tenant_email_templates (tenant_id, template_type, config, created_at, updated_at)
                   VALUES (:tenant_id, :type, :config, :created_at, :updated_at)
                   ON CONFLICT (tenant_id, template_type) DO UPDATE
                   SET config = :config, updated_at = :updated_at""",
                {
                    "tenant_id": tenant_id,
                    "type": template_type,
                    "config": config_json,
                    "created_at": datetime.utcnow(),
                    "updated_at": datetime.utcnow()
                }
            )
            self.db.commit()
            logger.info(f"Template {template_type} atualizado para tenant {tenant_id}")

        except Exception as e:
            self.db.rollback()
            logger.error(f"Erro ao salvar template: {e}")
            raise

        return current

    def render_email(
        self,
        tenant_id: str,
        template_type: str,
        context: Dict[str, Any]
    ) -> Dict[str, str]:
        """
        Renderiza template de email com contexto.

        Args:
            tenant_id: ID do tenant
            template_type: Tipo do template
            context: Variaveis para substituir

        Returns:
            Dict com subject, body_html, body_text renderizados
        """
        template = self.get_email_template(tenant_id, template_type)
        branding = self.get_branding(tenant_id)

        # Adicionar variaveis de branding ao contexto
        branding_context = {
            "company_name": branding.display_name if branding else "Plataforma E",
            "logo_url": branding.logo_url if branding else "",
            "support_email": branding.support_email if branding else "suporte@fabricadeagentes.com",
            "support_url": branding.support_url if branding else "",
            "current_year": datetime.utcnow().year,
        }

        full_context = {**branding_context, **context}

        return template.render(full_context)

    def get_all_email_templates(self, tenant_id: str) -> List[EmailTemplateConfig]:
        """
        Lista todos os templates de email do tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            Lista de EmailTemplateConfig
        """
        templates = []

        for template_type in EmailTemplateType:
            template = self.get_email_template(tenant_id, template_type.value)
            templates.append(template)

        return templates

    def _get_default_email_template(
        self,
        tenant_id: str,
        template_type: str
    ) -> EmailTemplateConfig:
        """Retorna template de email padrao"""
        branding = self.get_branding(tenant_id)
        company = branding.display_name if branding else "Plataforma E"

        templates = {
            EmailTemplateType.WELCOME.value: EmailTemplateConfig(
                tenant_id=tenant_id,
                template_type=template_type,
                subject=f"Bem-vindo ao {company}!",
                body_html=f"""
                <html>
                <body>
                <h1>Bem-vindo, {{{{user_name}}}}!</h1>
                <p>Sua conta no {company} foi criada com sucesso.</p>
                <p>Clique no botao abaixo para acessar:</p>
                <a href="{{{{login_url}}}}" style="background: #FF6C00; color: white; padding: 12px 24px; text-decoration: none; border-radius: 6px;">Acessar</a>
                </body>
                </html>
                """,
                body_text=f"Bem-vindo, {{{{user_name}}}}! Sua conta no {company} foi criada.",
                from_name=company,
            ),
            EmailTemplateType.INVITE.value: EmailTemplateConfig(
                tenant_id=tenant_id,
                template_type=template_type,
                subject=f"Voce foi convidado para {company}",
                body_html=f"""
                <html>
                <body>
                <h1>Convite para {company}</h1>
                <p>{{{{inviter_name}}}} convidou voce para fazer parte do time.</p>
                <a href="{{{{invite_url}}}}" style="background: #FF6C00; color: white; padding: 12px 24px; text-decoration: none; border-radius: 6px;">Aceitar Convite</a>
                </body>
                </html>
                """,
                body_text=f"{{{{inviter_name}}}} convidou voce para {company}. Acesse: {{{{invite_url}}}}",
                from_name=company,
            ),
            EmailTemplateType.PASSWORD_RESET.value: EmailTemplateConfig(
                tenant_id=tenant_id,
                template_type=template_type,
                subject=f"Redefinir senha - {company}",
                body_html=f"""
                <html>
                <body>
                <h1>Redefinir Senha</h1>
                <p>Recebemos uma solicitacao para redefinir sua senha.</p>
                <a href="{{{{reset_url}}}}" style="background: #FF6C00; color: white; padding: 12px 24px; text-decoration: none; border-radius: 6px;">Redefinir Senha</a>
                <p>Se voce nao solicitou, ignore este email.</p>
                </body>
                </html>
                """,
                body_text=f"Para redefinir sua senha, acesse: {{{{reset_url}}}}",
                from_name=company,
            ),
            EmailTemplateType.USAGE_ALERT.value: EmailTemplateConfig(
                tenant_id=tenant_id,
                template_type=template_type,
                subject=f"Alerta de uso - {company}",
                body_html=f"""
                <html>
                <body>
                <h1>Alerta de Uso</h1>
                <p>Seu uso de {{{{metric_name}}}} atingiu {{{{percentage}}}}% do limite.</p>
                <p>Uso atual: {{{{current_usage}}}} / {{{{limit}}}}</p>
                <a href="{{{{upgrade_url}}}}" style="background: #FF6C00; color: white; padding: 12px 24px; text-decoration: none; border-radius: 6px;">Fazer Upgrade</a>
                </body>
                </html>
                """,
                body_text=f"Alerta: Seu uso de {{{{metric_name}}}} atingiu {{{{percentage}}}}% do limite.",
                from_name=company,
            ),
        }

        return templates.get(template_type, EmailTemplateConfig(
            tenant_id=tenant_id,
            template_type=template_type,
            subject="Notificacao",
            body_html="<p>{{{{message}}}}</p>",
            body_text="{{{{message}}}}",
            from_name=company,
        ))

    # =========================================================================
    # HELPERS
    # =========================================================================

    def _dict_to_branding_config(self, data: Dict[str, Any]) -> BrandingConfig:
        """Converte dict para BrandingConfig"""
        theme_data = data.pop("theme", None)
        config = BrandingConfig(**{k: v for k, v in data.items() if hasattr(BrandingConfig, k)})

        if theme_data:
            config.theme = self._dict_to_theme_config(theme_data)

        return config

    def _dict_to_theme_config(self, data: Dict[str, Any]) -> ThemeConfig:
        """Converte dict para ThemeConfig"""
        light_colors = data.pop("light_colors", None)
        dark_colors = data.pop("dark_colors", None)

        config = ThemeConfig(**{k: v for k, v in data.items() if hasattr(ThemeConfig, k)})

        if light_colors:
            config.light_colors = ColorPalette(**light_colors) if isinstance(light_colors, dict) else light_colors

        if dark_colors:
            config.dark_colors = DarkColorPalette(**dark_colors) if isinstance(dark_colors, dict) else dark_colors

        return config

    def export_branding(self, tenant_id: str) -> Dict[str, Any]:
        """
        Exporta todas as configuracoes de branding.

        Args:
            tenant_id: ID do tenant

        Returns:
            Dict com todas as configuracoes
        """
        branding = self.get_branding(tenant_id)
        domain = self.get_domain_config(tenant_id)
        templates = self.get_all_email_templates(tenant_id)

        return {
            "tenant_id": tenant_id,
            "exported_at": datetime.utcnow().isoformat(),
            "branding": branding.to_dict() if branding else None,
            "domain": domain.to_dict() if domain else None,
            "email_templates": [t.to_dict() for t in templates],
        }

    def import_branding(
        self,
        tenant_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """
        Importa configuracoes de branding.

        Args:
            tenant_id: ID do tenant
            data: Dados exportados

        Returns:
            True se sucesso
        """
        try:
            # Importar branding
            if data.get("branding"):
                branding_data = data["branding"]
                branding_data.pop("tenant_id", None)
                self.update_branding(tenant_id, **branding_data)

            # Importar templates
            for template_data in data.get("email_templates", []):
                template_type = template_data.pop("template_type", None)
                template_data.pop("tenant_id", None)
                if template_type:
                    self.update_email_template(tenant_id, template_type, **template_data)

            logger.info(f"Branding importado para tenant {tenant_id}")
            return True

        except Exception as e:
            logger.error(f"Erro ao importar branding: {e}")
            raise


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "WhiteLabelService",
    "BrandingConfig",
    "ThemeConfig",
    "ColorPalette",
    "DarkColorPalette",
    "DomainConfig",
    "EmailTemplateConfig",
    "ThemeMode",
    "EmailTemplateType",
    "FontFamily",
]
