# -*- coding: utf-8 -*-
"""
White Label - Sistema de Personalizacao por Tenant
==================================================

Modulos:
- customization: Servico principal de personalizacao
"""

from .customization import (
    WhiteLabelService,
    BrandingConfig,
    EmailTemplateConfig,
    DomainConfig,
    ThemeConfig,
)

__all__ = [
    "WhiteLabelService",
    "BrandingConfig",
    "EmailTemplateConfig",
    "DomainConfig",
    "ThemeConfig",
]
