# -*- coding: utf-8 -*-
"""
Factory Testing Module - Sistema de Testes Autonomos com Geracao de Issues.

Este modulo expande o agente QA para realizar testes completos e autonomos:
- Geracao automatica de dados de teste (factories)
- Testes por persona/perfil
- Testes de todas as telas e funcionalidades
- Testes regressivos automatizados
- Geracao automatica de issues para bugs encontrados
- Integracao com orquestrador para atribuicao de issues
"""

from .data_factory import TestDataFactory, get_data_factory
from .persona_fixtures import PersonaFixtures, get_persona_fixtures
from .screen_registry import (
    SCREENS, FEATURES, INTEGRATIONS, DOCUMENT_TYPES,
    ScreenRegistry, get_screen_registry
)
from .regression_runner import (
    RegressionTestRunner, TestReport, TestResult,
    get_regression_runner
)
from .issue_generator import (
    TestFailure, IssueGenerator, get_issue_generator
)
from .qa_orchestrator_bridge import (
    QAOrchestratorBridge, get_qa_bridge
)
from .whitelabel_tests import (
    WhiteLabelTestSuite, get_whitelabel_suite
)

__all__ = [
    # Data Factory
    "TestDataFactory",
    "get_data_factory",

    # Persona Fixtures
    "PersonaFixtures",
    "get_persona_fixtures",

    # Screen Registry
    "SCREENS",
    "FEATURES",
    "INTEGRATIONS",
    "DOCUMENT_TYPES",
    "ScreenRegistry",
    "get_screen_registry",

    # Regression Runner
    "RegressionTestRunner",
    "TestReport",
    "TestResult",
    "get_regression_runner",

    # Issue Generator
    "TestFailure",
    "IssueGenerator",
    "get_issue_generator",

    # QA Bridge
    "QAOrchestratorBridge",
    "get_qa_bridge",

    # WhiteLabel Tests
    "WhiteLabelTestSuite",
    "get_whitelabel_suite",
]

__version__ = "1.0.0"
