"""
Orquestrador Autonomo da Plataforma E
============================================

Sistema de orquestracao que processa projetos de forma autonoma,
gerando historias, acionando agentes e respeitando hierarquias.
"""

from .autonomous_orchestrator import AutonomousOrchestrator
from .project_processor import ProjectProcessor
from .story_generator import StoryGenerator

__all__ = ['AutonomousOrchestrator', 'ProjectProcessor', 'StoryGenerator']
