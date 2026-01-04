"""
Plataforma E - Sistema de Agentes Autonomos com Aprendizado
================================================================

Arquitetura:
- core/: Motor de execucao autonoma dos agentes
- knowledge/: Base de conhecimento vetorial (RAG)
- memory/: Memoria de longo prazo por agente
- learning/: Sistema de feedback e aprendizado

Cada agente possui:
1. Conhecimento profundo do seu dominio
2. Memoria persistente de decisoes e resultados
3. Capacidade de aprender com execucoes passadas
4. Compartilhamento de conhecimento entre agentes
"""

from .core.autonomous_agent import AutonomousAgent
from .knowledge.knowledge_base import KnowledgeBase
from .memory.agent_memory import AgentMemory
from .learning.feedback_system import FeedbackSystem

__all__ = [
    'AutonomousAgent',
    'KnowledgeBase',
    'AgentMemory',
    'FeedbackSystem'
]
