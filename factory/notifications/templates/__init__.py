# -*- coding: utf-8 -*-
"""
Templates de Notificacao - Plataforma E
"""

from .default_templates import (
    EMAIL_TEMPLATES,
    SLACK_TEMPLATES,
    TEAMS_TEMPLATES,
    get_template,
    list_available_templates,
    get_template_variables
)

__all__ = [
    "EMAIL_TEMPLATES",
    "SLACK_TEMPLATES",
    "TEAMS_TEMPLATES",
    "get_template",
    "list_available_templates",
    "get_template_variables"
]
