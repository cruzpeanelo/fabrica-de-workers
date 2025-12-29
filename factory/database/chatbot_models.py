# -*- coding: utf-8 -*-
"""
Modelos SQLAlchemy para Chatbots de Suporte (Issue #67)
=======================================================

Modelos para o sistema de Chatbot Builder:
- Chatbot: Configuracao principal do chatbot
- ChatbotConversation: Historico de conversas
- ChatbotKnowledgeDocument: Base de conhecimento

Versao: 1.0.0
"""
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean
from sqlalchemy.orm import relationship
from datetime import datetime
from enum import Enum

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# ENUMS
# =============================================================================

class ChatbotStatus(str, Enum):
    """Status do Chatbot"""
    DRAFT = "draft"
    ACTIVE = "active"
    PAUSED = "paused"
    ARCHIVED = "archived"


class ChatbotChannel(str, Enum):
    """Canais de deploy do Chatbot"""
    WIDGET = "widget"
    WHATSAPP = "whatsapp"
    TELEGRAM = "telegram"
    SLACK = "slack"
    TEAMS = "teams"
    API = "api"


class EscalationTrigger(str, Enum):
    """Gatilhos para escalar para humano"""
    FRUSTRATION = "frustration"
    SENSITIVE_TOPIC = "sensitive_topic"
    EXPLICIT_REQUEST = "explicit_request"
    LOW_CONFIDENCE = "low_confidence"
    MAX_ATTEMPTS = "max_attempts"


# =============================================================================
# CHATBOT - Modelo Principal
# =============================================================================

class Chatbot(Base):
    """
    Modelo para Chatbots de Suporte ao Cliente
    Agente especializado construido com o Chatbot Builder
    """
    __tablename__ = "chatbots"

    id = Column(Integer, primary_key=True, autoincrement=True)
    chatbot_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=True, index=True)

    # Dados principais
    name = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)
    avatar_url = Column(String(500), nullable=True)

    # Status
    status = Column(String(30), default=ChatbotStatus.DRAFT.value, index=True)

    # Configuracao de Personalidade
    personality = Column(JSON, default=lambda: {
        "name": "Assistente",
        "tone": "professional",
        "language": "pt-BR",
        "greeting": "Ola! Como posso ajudar voce hoje?",
        "farewell": "Obrigado pelo contato! Ate mais!",
        "fallback": "Desculpe, nao entendi. Pode reformular sua pergunta?",
        "transfer_message": "Vou transferir voce para um de nossos atendentes."
    })

    # Configuracao do Modelo de IA
    ai_config = Column(JSON, default=lambda: {
        "model": "claude-sonnet-4-20250514",
        "temperature": 0.7,
        "max_tokens": 1024,
        "system_prompt": "",
        "context_window": 10
    })

    # Knowledge Base (Base de Conhecimento)
    knowledge_base = Column(JSON, default=lambda: {
        "documents": [],
        "faqs": [],
        "urls": [],
        "embeddings_status": "pending"
    })

    # Templates de Resposta
    response_templates = Column(JSON, default=list)

    # Fluxos de Conversa (Decision Trees)
    conversation_flows = Column(JSON, default=list)

    # Configuracao de Escalacao
    escalation_config = Column(JSON, default=lambda: {
        "enabled": True,
        "triggers": ["frustration", "explicit_request", "sensitive_topic"],
        "max_attempts_before_escalation": 3,
        "sensitive_topics": ["reclamacao", "reembolso", "cancelamento"],
        "frustration_keywords": ["falar com atendente", "nao funciona", "problema"],
        "queue_enabled": False,
        "queue_webhook": ""
    })

    # Canais de Deploy
    channels = Column(JSON, default=lambda: {
        "widget": {"enabled": False, "config": {}},
        "whatsapp": {"enabled": False, "config": {}},
        "telegram": {"enabled": False, "config": {}},
        "slack": {"enabled": False, "config": {}},
        "teams": {"enabled": False, "config": {}},
        "api": {"enabled": True, "config": {"api_key": ""}}
    })

    # Integracoes
    integrations = Column(JSON, default=lambda: {
        "crm": {"enabled": False, "type": "", "config": {}},
        "ticketing": {"enabled": False, "type": "", "config": {}},
        "analytics": {"enabled": False, "type": "", "config": {}}
    })

    # Metricas e Analytics
    metrics = Column(JSON, default=lambda: {
        "total_conversations": 0,
        "resolved_conversations": 0,
        "escalated_conversations": 0,
        "avg_response_time": 0,
        "avg_resolution_time": 0,
        "csat_score": 0,
        "top_questions": [],
        "feedback_positive": 0,
        "feedback_negative": 0
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    published_at = Column(DateTime, nullable=True)

    # Criado por
    created_by = Column(String(100), default="system")

    # Relacionamentos
    conversations = relationship("ChatbotConversation", back_populates="chatbot", cascade="all, delete-orphan")
    knowledge_documents = relationship("ChatbotKnowledgeDocument", back_populates="chatbot", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "chatbot_id": self.chatbot_id,
            "project_id": self.project_id,
            "name": self.name,
            "description": self.description,
            "avatar_url": self.avatar_url,
            "status": self.status,
            "personality": self.personality or {},
            "ai_config": self.ai_config or {},
            "knowledge_base": self.knowledge_base or {},
            "response_templates": self.response_templates or [],
            "conversation_flows": self.conversation_flows or [],
            "escalation_config": self.escalation_config or {},
            "channels": self.channels or {},
            "integrations": self.integrations or {},
            "metrics": self.metrics or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "published_at": self.published_at.isoformat() if self.published_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<Chatbot {self.chatbot_id}: {self.name} [{self.status}]>"


# =============================================================================
# CHATBOT_CONVERSATION - Historico de Conversas
# =============================================================================

class ChatbotConversation(Base):
    """
    Modelo para Conversas do Chatbot
    Armazena historico de conversas para analytics e aprendizado
    """
    __tablename__ = "chatbot_conversations"

    id = Column(Integer, primary_key=True, autoincrement=True)
    conversation_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com chatbot
    chatbot_id = Column(String(50), ForeignKey("chatbots.chatbot_id"), nullable=False, index=True)
    chatbot = relationship("Chatbot", back_populates="conversations")

    # Identificacao do usuario
    user_id = Column(String(100), nullable=True, index=True)
    user_name = Column(String(200), nullable=True)
    user_email = Column(String(200), nullable=True)
    user_phone = Column(String(50), nullable=True)

    # Canal
    channel = Column(String(30), default=ChatbotChannel.API.value)
    channel_session_id = Column(String(100), nullable=True)

    # Mensagens da conversa
    messages = Column(JSON, default=list)

    # Status da conversa
    status = Column(String(30), default="active")
    resolution_type = Column(String(30), nullable=True)

    # Escalacao
    escalated = Column(Boolean, default=False)
    escalation_reason = Column(String(50), nullable=True)
    escalated_to = Column(String(100), nullable=True)
    escalated_at = Column(DateTime, nullable=True)

    # Feedback
    feedback_score = Column(Integer, nullable=True)
    feedback_comment = Column(Text, nullable=True)

    # Metricas da conversa
    messages_count = Column(Integer, default=0)
    response_times = Column(JSON, default=list)
    topics_detected = Column(JSON, default=list)
    sentiment_scores = Column(JSON, default=list)

    # Contexto
    context_data = Column(JSON, default=dict)

    # Timestamps
    started_at = Column(DateTime, default=datetime.utcnow)
    last_message_at = Column(DateTime, default=datetime.utcnow)
    ended_at = Column(DateTime, nullable=True)

    def to_dict(self):
        return {
            "conversation_id": self.conversation_id,
            "chatbot_id": self.chatbot_id,
            "user_id": self.user_id,
            "user_name": self.user_name,
            "user_email": self.user_email,
            "channel": self.channel,
            "messages": self.messages or [],
            "status": self.status,
            "resolution_type": self.resolution_type,
            "escalated": self.escalated,
            "escalation_reason": self.escalation_reason,
            "escalated_to": self.escalated_to,
            "feedback_score": self.feedback_score,
            "feedback_comment": self.feedback_comment,
            "messages_count": self.messages_count,
            "topics_detected": self.topics_detected or [],
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "last_message_at": self.last_message_at.isoformat() if self.last_message_at else None,
            "ended_at": self.ended_at.isoformat() if self.ended_at else None
        }

    def __repr__(self):
        return f"<ChatbotConversation {self.conversation_id}: [{self.status}] {self.messages_count} msgs>"


# =============================================================================
# CHATBOT_KNOWLEDGE_DOCUMENT - Base de Conhecimento
# =============================================================================

class ChatbotKnowledgeDocument(Base):
    """
    Modelo para Documentos da Knowledge Base do Chatbot
    Armazena documentos processados para RAG
    """
    __tablename__ = "chatbot_knowledge_documents"

    id = Column(Integer, primary_key=True, autoincrement=True)
    document_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com chatbot
    chatbot_id = Column(String(50), ForeignKey("chatbots.chatbot_id"), nullable=False, index=True)
    chatbot = relationship("Chatbot", back_populates="knowledge_documents")

    # Dados do documento
    title = Column(String(300), nullable=False)
    content = Column(Text, nullable=False)
    doc_type = Column(String(30), default="text")

    # Fonte original
    source_url = Column(String(500), nullable=True)
    source_file = Column(String(300), nullable=True)

    # Processamento
    chunks = Column(JSON, default=list)
    processed = Column(Boolean, default=False)
    processed_at = Column(DateTime, nullable=True)

    # Metadados
    metadata = Column(JSON, default=dict)
    tags = Column(JSON, default=list)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "document_id": self.document_id,
            "chatbot_id": self.chatbot_id,
            "title": self.title,
            "content": self.content[:500] + "..." if len(self.content) > 500 else self.content,
            "doc_type": self.doc_type,
            "source_url": self.source_url,
            "source_file": self.source_file,
            "chunks_count": len(self.chunks) if self.chunks else 0,
            "processed": self.processed,
            "tags": self.tags or [],
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<ChatbotKnowledgeDocument {self.document_id}: {self.title[:30]}>"


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "ChatbotStatus",
    "ChatbotChannel",
    "EscalationTrigger",
    "Chatbot",
    "ChatbotConversation",
    "ChatbotKnowledgeDocument"
]
