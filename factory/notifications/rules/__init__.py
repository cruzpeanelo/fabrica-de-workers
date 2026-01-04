# -*- coding: utf-8 -*-
"""
Motor de Regras de Notificacao - Plataforma E
"""

from .rule_engine import RuleEngine
from .default_rules import DEFAULT_RULES, create_default_rules

__all__ = ["RuleEngine", "DEFAULT_RULES", "create_default_rules"]
