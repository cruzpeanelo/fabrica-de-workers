# -*- coding: utf-8 -*-
"""
Rotas da API do Chatbot Builder (Issue #67)
============================================

Endpoints para gerenciamento de chatbots de suporte:
- CRUD de chatbots
- Gerenciamento de FAQs e documentos
- Chat em tempo real
- Analytics e exportacao

Versao: 1.0.0
"""

import os
import sys
import uuid
from datetime import datetime
from typing import Optional, List, Dict, Any

# Path configurado via imports relativos

from fastapi import APIRouter, HTTPException, UploadFile, File
from pydantic import BaseModel

# Database
from factory.database.connection import SessionLocal

# Chatbot models e builder
try:
    from factory.database.chatbot_models import (
        Chatbot, ChatbotConversation, ChatbotKnowledgeDocument, ChatbotStatus
    )
    from factory.core.chatbot_builder import (
        ChatbotBuilder, create_chatbot, PERSONALITY_TONES, RESPONSE_TEMPLATES
    )
    HAS_CHATBOT = True
except ImportError as e:
    print(f"[ChatbotRoutes] Chatbot module not available: {e}")
    HAS_CHATBOT = False


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/chatbots", tags=["Chatbot Builder"])


# =============================================================================
# SCHEMAS
# =============================================================================

class ChatbotCreate(BaseModel):
    """Schema para criacao de chatbot"""
    name: str
    description: Optional[str] = ""
    project_id: Optional[str] = None
    personality_tone: Optional[str] = "professional"
    language: Optional[str] = "pt-BR"


class ChatbotUpdate(BaseModel):
    """Schema para atualizacao de chatbot"""
    name: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    personality: Optional[Dict] = None
    ai_config: Optional[Dict] = None
    escalation_config: Optional[Dict] = None
    channels: Optional[Dict] = None


class ChatbotMessageRequest(BaseModel):
    """Schema para mensagem do chatbot"""
    message: str
    conversation_id: Optional[str] = None
    user_id: Optional[str] = None
    context: Optional[Dict] = None


class FAQCreate(BaseModel):
    """Schema para criacao de FAQ"""
    question: str
    answer: str
    category: Optional[str] = "geral"
    keywords: Optional[List[str]] = None


class KnowledgeDocCreate(BaseModel):
    """Schema para criacao de documento"""
    title: str
    content: str
    doc_type: Optional[str] = "text"
    source: Optional[str] = ""


# =============================================================================
# ENDPOINTS - CRUD CHATBOT
# =============================================================================

@router.get("")
def list_chatbots(project_id: Optional[str] = None):
    """Lista todos os chatbots"""
    if not HAS_CHATBOT:
        return []

    db = SessionLocal()
    try:
        query = db.query(Chatbot)
        if project_id:
            query = query.filter(Chatbot.project_id == project_id)
        chatbots = query.order_by(Chatbot.updated_at.desc()).all()
        return [c.to_dict() for c in chatbots]
    finally:
        db.close()


@router.post("")
def create_chatbot_endpoint(data: ChatbotCreate):
    """Cria novo chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        # Usa o builder para criar o chatbot
        builder = create_chatbot(
            name=data.name,
            description=data.description or "",
            tone=data.personality_tone or "professional"
        )

        # Carrega templates padrao
        builder.load_default_templates(["greeting", "farewell", "fallback", "support"])

        # Configura personalidade
        builder.set_personality(
            name=data.name,
            tone=data.personality_tone or "professional",
            language=data.language or "pt-BR"
        )

        config = builder.build()

        # Salva no banco
        chatbot = Chatbot(
            chatbot_id=config.chatbot_id,
            project_id=data.project_id,
            name=config.name,
            description=config.description,
            status=ChatbotStatus.DRAFT.value,
            personality=config.personality,
            ai_config=config.ai_config,
            response_templates=[t.__dict__ if hasattr(t, '__dict__') else t for t in config.response_templates],
            escalation_config=config.escalation_config,
            channels=config.channels
        )

        db.add(chatbot)
        db.commit()
        db.refresh(chatbot)

        return chatbot.to_dict()
    finally:
        db.close()


@router.get("/{chatbot_id}")
def get_chatbot(chatbot_id: str):
    """Busca chatbot por ID"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")
        return chatbot.to_dict()
    finally:
        db.close()


@router.put("/{chatbot_id}")
def update_chatbot(chatbot_id: str, data: ChatbotUpdate):
    """Atualiza chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        if data.name:
            chatbot.name = data.name
        if data.description is not None:
            chatbot.description = data.description
        if data.status:
            chatbot.status = data.status
        if data.personality:
            chatbot.personality = data.personality
        if data.ai_config:
            chatbot.ai_config = data.ai_config
        if data.escalation_config:
            chatbot.escalation_config = data.escalation_config
        if data.channels:
            chatbot.channels = data.channels

        db.commit()
        db.refresh(chatbot)
        return chatbot.to_dict()
    finally:
        db.close()


@router.delete("/{chatbot_id}")
def delete_chatbot(chatbot_id: str):
    """Deleta chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        db.delete(chatbot)
        db.commit()
        return {"status": "deleted", "chatbot_id": chatbot_id}
    finally:
        db.close()


@router.patch("/{chatbot_id}/publish")
def publish_chatbot(chatbot_id: str):
    """Publica chatbot (ativa para uso)"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        chatbot.status = ChatbotStatus.ACTIVE.value
        chatbot.published_at = datetime.utcnow()
        db.commit()
        db.refresh(chatbot)

        return chatbot.to_dict()
    finally:
        db.close()


@router.patch("/{chatbot_id}/pause")
def pause_chatbot(chatbot_id: str):
    """Pausa chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        chatbot.status = ChatbotStatus.PAUSED.value
        db.commit()
        db.refresh(chatbot)

        return chatbot.to_dict()
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - KNOWLEDGE BASE
# =============================================================================

@router.post("/{chatbot_id}/faqs")
def add_chatbot_faq(chatbot_id: str, faq: FAQCreate):
    """Adiciona FAQ ao chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        # Atualiza knowledge_base com a nova FAQ
        kb = chatbot.knowledge_base or {"faqs": [], "documents": [], "urls": []}
        kb["faqs"].append({
            "id": f"FAQ-{uuid.uuid4().hex[:8].upper()}",
            "question": faq.question,
            "answer": faq.answer,
            "category": faq.category or "geral",
            "keywords": faq.keywords or [],
            "created_at": datetime.utcnow().isoformat()
        })
        chatbot.knowledge_base = kb

        db.commit()
        db.refresh(chatbot)
        return {"status": "success", "faqs_count": len(kb["faqs"])}
    finally:
        db.close()


@router.get("/{chatbot_id}/faqs")
def list_chatbot_faqs(chatbot_id: str):
    """Lista FAQs do chatbot"""
    if not HAS_CHATBOT:
        return []

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        kb = chatbot.knowledge_base or {}
        return kb.get("faqs", [])
    finally:
        db.close()


@router.delete("/{chatbot_id}/faqs/{faq_id}")
def delete_chatbot_faq(chatbot_id: str, faq_id: str):
    """Remove FAQ do chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        kb = chatbot.knowledge_base or {"faqs": []}
        kb["faqs"] = [f for f in kb.get("faqs", []) if f.get("id") != faq_id]
        chatbot.knowledge_base = kb

        db.commit()
        return {"status": "deleted", "faq_id": faq_id}
    finally:
        db.close()


@router.post("/{chatbot_id}/documents")
def add_chatbot_document(chatbot_id: str, doc: KnowledgeDocCreate):
    """Adiciona documento a base de conhecimento"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        # Cria documento na tabela separada
        knowledge_doc = ChatbotKnowledgeDocument(
            document_id=f"DOC-{uuid.uuid4().hex[:8].upper()}",
            chatbot_id=chatbot_id,
            title=doc.title,
            content=doc.content,
            doc_type=doc.doc_type or "text",
            source_url=doc.source if doc.source and doc.source.startswith("http") else None,
            source_file=doc.source if doc.source and not doc.source.startswith("http") else None
        )

        db.add(knowledge_doc)
        db.commit()
        db.refresh(knowledge_doc)
        return knowledge_doc.to_dict()
    finally:
        db.close()


@router.get("/{chatbot_id}/documents")
def list_chatbot_documents(chatbot_id: str):
    """Lista documentos do chatbot"""
    if not HAS_CHATBOT:
        return []

    db = SessionLocal()
    try:
        docs = db.query(ChatbotKnowledgeDocument).filter(
            ChatbotKnowledgeDocument.chatbot_id == chatbot_id
        ).all()
        return [d.to_dict() for d in docs]
    finally:
        db.close()


@router.delete("/{chatbot_id}/documents/{document_id}")
def delete_chatbot_document(chatbot_id: str, document_id: str):
    """Remove documento do chatbot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        doc = db.query(ChatbotKnowledgeDocument).filter(
            ChatbotKnowledgeDocument.document_id == document_id,
            ChatbotKnowledgeDocument.chatbot_id == chatbot_id
        ).first()
        if not doc:
            raise HTTPException(404, "Document not found")

        db.delete(doc)
        db.commit()
        return {"status": "deleted", "document_id": document_id}
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - CHAT
# =============================================================================

@router.post("/{chatbot_id}/chat")
async def chatbot_chat(chatbot_id: str, request: ChatbotMessageRequest):
    """Endpoint principal de chat com o bot"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        # Busca ou cria conversa
        conversation = None
        if request.conversation_id:
            conversation = db.query(ChatbotConversation).filter(
                ChatbotConversation.conversation_id == request.conversation_id
            ).first()

        if not conversation:
            conversation = ChatbotConversation(
                conversation_id=f"CONV-{uuid.uuid4().hex[:12].upper()}",
                chatbot_id=chatbot_id,
                user_id=request.user_id,
                messages=[],
                context_data=request.context or {}
            )
            db.add(conversation)
            db.commit()
            db.refresh(conversation)

        # Adiciona mensagem do usuario
        messages = conversation.messages or []
        messages.append({
            "role": "user",
            "content": request.message,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Cria builder temporario para gerar resposta
        builder = ChatbotBuilder(chatbot_id=chatbot_id)
        builder.config.personality = chatbot.personality or {}
        builder.config.ai_config = chatbot.ai_config or {}

        # Carrega FAQs do banco
        kb = chatbot.knowledge_base or {}
        for faq_data in kb.get("faqs", []):
            builder.add_faq(
                question=faq_data.get("question", ""),
                answer=faq_data.get("answer", ""),
                category=faq_data.get("category", "geral")
            )

        builder.set_escalation_config(**(chatbot.escalation_config or {}))

        # Gera resposta
        result = await builder.generate_response(
            user_message=request.message,
            conversation_history=messages[:-1],
            context=request.context
        )

        # Adiciona resposta do bot
        messages.append({
            "role": "assistant",
            "content": result.get("response", ""),
            "timestamp": datetime.utcnow().isoformat(),
            "metadata": {
                "confidence": result.get("confidence", 0),
                "should_escalate": result.get("should_escalate", False)
            }
        })

        # Atualiza conversa
        conversation.messages = messages
        conversation.messages_count = len(messages)
        conversation.last_message_at = datetime.utcnow()

        if result.get("should_escalate"):
            conversation.escalated = True
            conversation.escalation_reason = result.get("escalation_reason", "unknown")

        db.commit()

        # Atualiza metricas do chatbot
        metrics = chatbot.metrics or {}
        metrics["total_conversations"] = metrics.get("total_conversations", 0) + 1
        chatbot.metrics = metrics
        db.commit()

        return {
            "conversation_id": conversation.conversation_id,
            "response": result.get("response", ""),
            "should_escalate": result.get("should_escalate", False),
            "escalation_reason": result.get("escalation_reason"),
            "confidence": result.get("confidence", 0)
        }
    finally:
        db.close()


@router.get("/{chatbot_id}/conversations")
def list_chatbot_conversations(chatbot_id: str, limit: int = 50):
    """Lista conversas do chatbot"""
    if not HAS_CHATBOT:
        return []

    db = SessionLocal()
    try:
        conversations = db.query(ChatbotConversation).filter(
            ChatbotConversation.chatbot_id == chatbot_id
        ).order_by(ChatbotConversation.last_message_at.desc()).limit(limit).all()
        return [c.to_dict() for c in conversations]
    finally:
        db.close()


@router.get("/{chatbot_id}/conversations/{conversation_id}")
def get_conversation(chatbot_id: str, conversation_id: str):
    """Busca conversa especifica"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        conversation = db.query(ChatbotConversation).filter(
            ChatbotConversation.conversation_id == conversation_id,
            ChatbotConversation.chatbot_id == chatbot_id
        ).first()
        if not conversation:
            raise HTTPException(404, "Conversation not found")
        return conversation.to_dict()
    finally:
        db.close()


@router.post("/{chatbot_id}/conversations/{conversation_id}/feedback")
def submit_feedback(chatbot_id: str, conversation_id: str, score: int, comment: Optional[str] = None):
    """Submete feedback de uma conversa"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        conversation = db.query(ChatbotConversation).filter(
            ChatbotConversation.conversation_id == conversation_id
        ).first()
        if not conversation:
            raise HTTPException(404, "Conversation not found")

        conversation.feedback_score = score
        conversation.feedback_comment = comment
        conversation.status = "resolved"
        conversation.ended_at = datetime.utcnow()

        # Atualiza metricas do chatbot
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if chatbot:
            metrics = chatbot.metrics or {}
            if score >= 4:
                metrics["feedback_positive"] = metrics.get("feedback_positive", 0) + 1
            else:
                metrics["feedback_negative"] = metrics.get("feedback_negative", 0) + 1
            metrics["resolved_conversations"] = metrics.get("resolved_conversations", 0) + 1
            chatbot.metrics = metrics

        db.commit()
        return {"status": "success", "feedback_score": score}
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - ANALYTICS
# =============================================================================

@router.get("/{chatbot_id}/analytics")
def get_chatbot_analytics(chatbot_id: str):
    """Obtem analytics do chatbot"""
    if not HAS_CHATBOT:
        return {}

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        # Conta conversas
        total_convs = db.query(ChatbotConversation).filter(
            ChatbotConversation.chatbot_id == chatbot_id
        ).count()

        escalated_convs = db.query(ChatbotConversation).filter(
            ChatbotConversation.chatbot_id == chatbot_id,
            ChatbotConversation.escalated == True
        ).count()

        resolved_convs = db.query(ChatbotConversation).filter(
            ChatbotConversation.chatbot_id == chatbot_id,
            ChatbotConversation.status == "resolved"
        ).count()

        # Conta documentos
        docs_count = db.query(ChatbotKnowledgeDocument).filter(
            ChatbotKnowledgeDocument.chatbot_id == chatbot_id
        ).count()

        kb = chatbot.knowledge_base or {}
        faqs_count = len(kb.get("faqs", []))

        return {
            "chatbot_id": chatbot_id,
            "total_conversations": total_convs,
            "escalated_conversations": escalated_convs,
            "resolved_conversations": resolved_convs,
            "escalation_rate": round((escalated_convs / total_convs * 100), 2) if total_convs > 0 else 0,
            "resolution_rate": round((resolved_convs / total_convs * 100), 2) if total_convs > 0 else 0,
            "knowledge_base": {
                "documents": docs_count,
                "faqs": faqs_count
            },
            "metrics": chatbot.metrics or {}
        }
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - EXPORT
# =============================================================================

@router.get("/{chatbot_id}/export/widget")
def export_chatbot_widget(chatbot_id: str, api_endpoint: str = ""):
    """Exporta codigo do widget embarcavel"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        builder = ChatbotBuilder(chatbot_id=chatbot_id)
        builder.config.name = chatbot.name
        builder.config.personality = chatbot.personality or {}

        widget_code = builder.export_widget_code(
            api_endpoint=api_endpoint or f"/api/chatbots/{chatbot_id}/chat"
        )

        return {
            "chatbot_id": chatbot_id,
            "widget_code": widget_code,
            "instructions": "Cole este codigo HTML no seu site para adicionar o widget de chat."
        }
    finally:
        db.close()


@router.get("/{chatbot_id}/export/json")
def export_chatbot_json(chatbot_id: str):
    """Exporta configuracao do chatbot em JSON"""
    if not HAS_CHATBOT:
        raise HTTPException(500, "Chatbot module not available")

    db = SessionLocal()
    try:
        chatbot = db.query(Chatbot).filter(Chatbot.chatbot_id == chatbot_id).first()
        if not chatbot:
            raise HTTPException(404, "Chatbot not found")

        # Busca documentos
        docs = db.query(ChatbotKnowledgeDocument).filter(
            ChatbotKnowledgeDocument.chatbot_id == chatbot_id
        ).all()

        export_data = chatbot.to_dict()
        export_data["documents"] = [d.to_dict() for d in docs]
        export_data["exported_at"] = datetime.utcnow().isoformat()
        export_data["version"] = "1.0.0"

        return export_data
    finally:
        db.close()


# =============================================================================
# ENDPOINTS - TEMPLATES
# =============================================================================

@router.get("/templates/personality")
def get_personality_templates():
    """Retorna tons de personalidade disponiveis"""
    if not HAS_CHATBOT:
        return {}
    return PERSONALITY_TONES


@router.get("/templates/responses")
def get_response_templates():
    """Retorna templates de resposta disponiveis"""
    if not HAS_CHATBOT:
        return {}
    return RESPONSE_TEMPLATES


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = ["router"]
