# -*- coding: utf-8 -*-
"""
Chatbot Builder - Construtor de Chatbots de Suporte ao Cliente
================================================================

Este modulo implementa o sistema completo de criacao de chatbots:
- Knowledge Base Builder (upload de docs, FAQs, crawling)
- Treinamento do Agente (exemplos, templates, personalidade)
- Integracao com Claude API
- Exportacao para multiplos canais
- Analytics e Continuous Learning

Versao: 1.0.0
Autor: Plataforma E
"""

import os
import sys
import uuid
import json
import re
import asyncio
from datetime import datetime, timezone
from typing import List, Dict, Optional, Any, Tuple
from dataclasses import dataclass, field, asdict
from pathlib import Path
from enum import Enum

# Configurar encoding UTF-8
if hasattr(sys.stdout, 'reconfigure'):
    sys.stdout.reconfigure(encoding='utf-8')

# Adicionar path do projeto
sys.path.insert(0, r'C:\Users\lcruz\Plataforma E')

from dotenv import load_dotenv
load_dotenv()


# =============================================================================
# CONSTANTES E CONFIGURACOES
# =============================================================================

# Diretorio base para exports
EXPORT_DIR = Path(r'C:\Users\lcruz\Plataforma E\exports\chatbots')
EXPORT_DIR.mkdir(parents=True, exist_ok=True)

# Tons de personalidade disponiveis
PERSONALITY_TONES = {
    "professional": {
        "name": "Profissional",
        "description": "Tom formal e objetivo, ideal para B2B",
        "style_hints": ["Use linguagem formal", "Seja direto", "Evite girias"]
    },
    "friendly": {
        "name": "Amigavel",
        "description": "Tom caloroso e acessivel, bom para B2C",
        "style_hints": ["Seja simpatico", "Use emojis com moderacao", "Personalize"]
    },
    "casual": {
        "name": "Casual",
        "description": "Tom descontraido, ideal para publico jovem",
        "style_hints": ["Use linguagem informal", "Seja criativo", "Use emojis"]
    },
    "technical": {
        "name": "Tecnico",
        "description": "Tom tecnico para suporte especializado",
        "style_hints": ["Use termos tecnicos", "Seja preciso", "Inclua referencias"]
    }
}

# Templates de resposta por categoria
RESPONSE_TEMPLATES = {
    "greeting": [
        {
            "id": "greeting_01",
            "trigger": "inicio",
            "template": "Ola! Bem-vindo ao {company_name}. Como posso ajudar voce hoje?",
            "category": "greeting"
        },
        {
            "id": "greeting_02",
            "trigger": "bom_dia",
            "template": "Bom dia! Eu sou {bot_name}, assistente virtual do {company_name}. Em que posso ajudar?",
            "category": "greeting"
        }
    ],
    "farewell": [
        {
            "id": "farewell_01",
            "trigger": "fim",
            "template": "Obrigado pelo contato! Se precisar de mais alguma coisa, estou por aqui. Ate mais!",
            "category": "farewell"
        },
        {
            "id": "farewell_02",
            "trigger": "tchau",
            "template": "Foi um prazer ajudar! Tenha um otimo dia!",
            "category": "farewell"
        }
    ],
    "fallback": [
        {
            "id": "fallback_01",
            "trigger": "nao_entendi",
            "template": "Desculpe, nao entendi sua pergunta. Pode reformular de outra forma?",
            "category": "fallback"
        },
        {
            "id": "fallback_02",
            "trigger": "sem_resposta",
            "template": "Essa e uma otima pergunta! Infelizmente nao tenho essa informacao no momento. Posso transferir voce para um atendente?",
            "category": "fallback"
        }
    ],
    "escalation": [
        {
            "id": "escalation_01",
            "trigger": "transferir",
            "template": "Entendo. Vou transferir voce para um de nossos especialistas que podera ajudar melhor. Um momento, por favor.",
            "category": "escalation"
        },
        {
            "id": "escalation_02",
            "trigger": "humano",
            "template": "Claro! Estou encaminhando voce para nosso time de atendimento. Tempo estimado de espera: {wait_time}.",
            "category": "escalation"
        }
    ],
    "support": [
        {
            "id": "support_01",
            "trigger": "problema",
            "template": "Sinto muito pelo inconveniente. Vamos resolver isso juntos! Pode me contar mais detalhes sobre o problema?",
            "category": "support"
        },
        {
            "id": "support_02",
            "trigger": "bug",
            "template": "Obrigado por reportar. Vou registrar esse problema para nossa equipe tecnica. Pode me informar quando isso comecou?",
            "category": "support"
        }
    ],
    "sales": [
        {
            "id": "sales_01",
            "trigger": "preco",
            "template": "Otima pergunta! Nossos planos comecam a partir de {price}. Quer que eu explique as opcoes disponiveis?",
            "category": "sales"
        },
        {
            "id": "sales_02",
            "trigger": "comprar",
            "template": "Que bom que voce tem interesse! Posso ajudar voce a escolher a melhor opcao para suas necessidades. Qual e seu principal objetivo?",
            "category": "sales"
        }
    ]
}

# Fluxos de conversa pre-definidos
CONVERSATION_FLOW_TEMPLATES = {
    "support_ticket": {
        "id": "flow_support",
        "name": "Abertura de Chamado",
        "description": "Fluxo para coletar informacoes e abrir ticket de suporte",
        "nodes": [
            {"id": "start", "type": "start", "text": "Ola! Vou ajudar voce a abrir um chamado."},
            {"id": "ask_issue", "type": "question", "text": "Qual e o problema que voce esta enfrentando?"},
            {"id": "ask_urgency", "type": "choice", "text": "Qual a urgencia?", "options": ["Alta", "Media", "Baixa"]},
            {"id": "ask_email", "type": "input", "text": "Por favor, informe seu email para acompanhamento:"},
            {"id": "confirm", "type": "confirm", "text": "Confirma a abertura do chamado?"},
            {"id": "end", "type": "end", "text": "Chamado #{ticket_id} criado com sucesso!"}
        ],
        "edges": [
            {"from": "start", "to": "ask_issue"},
            {"from": "ask_issue", "to": "ask_urgency"},
            {"from": "ask_urgency", "to": "ask_email"},
            {"from": "ask_email", "to": "confirm"},
            {"from": "confirm", "to": "end", "condition": "yes"},
            {"from": "confirm", "to": "start", "condition": "no"}
        ]
    },
    "product_inquiry": {
        "id": "flow_product",
        "name": "Consulta de Produto",
        "description": "Fluxo para ajudar cliente a encontrar produto ideal",
        "nodes": [
            {"id": "start", "type": "start", "text": "Vou ajudar voce a encontrar o produto ideal!"},
            {"id": "ask_category", "type": "choice", "text": "Qual categoria de interesse?", "options": []},
            {"id": "ask_budget", "type": "choice", "text": "Qual sua faixa de orcamento?", "options": ["Ate R$100", "R$100-500", "Acima de R$500"]},
            {"id": "show_options", "type": "api_call", "text": "Buscando opcoes...", "api": "/products/search"},
            {"id": "end", "type": "end", "text": "Espero ter ajudado! Quer ver mais opcoes?"}
        ],
        "edges": [
            {"from": "start", "to": "ask_category"},
            {"from": "ask_category", "to": "ask_budget"},
            {"from": "ask_budget", "to": "show_options"},
            {"from": "show_options", "to": "end"}
        ]
    },
    "faq_navigation": {
        "id": "flow_faq",
        "name": "Navegacao FAQ",
        "description": "Fluxo para navegar pelas perguntas frequentes",
        "nodes": [
            {"id": "start", "type": "start", "text": "Vou ajudar voce a encontrar a resposta!"},
            {"id": "ask_topic", "type": "choice", "text": "Sobre qual assunto e sua duvida?", "options": ["Pagamento", "Entrega", "Devolucao", "Conta", "Outro"]},
            {"id": "show_faqs", "type": "faq_list", "text": "Aqui estao as perguntas frequentes sobre {topic}:"},
            {"id": "ask_solved", "type": "choice", "text": "Sua duvida foi resolvida?", "options": ["Sim", "Nao, preciso de ajuda"]},
            {"id": "end_success", "type": "end", "text": "Que bom! Posso ajudar em mais alguma coisa?"},
            {"id": "escalate", "type": "escalate", "text": "Vou transferir para um atendente..."}
        ],
        "edges": [
            {"from": "start", "to": "ask_topic"},
            {"from": "ask_topic", "to": "show_faqs"},
            {"from": "show_faqs", "to": "ask_solved"},
            {"from": "ask_solved", "to": "end_success", "condition": "Sim"},
            {"from": "ask_solved", "to": "escalate", "condition": "Nao, preciso de ajuda"}
        ]
    }
}


# =============================================================================
# DATACLASSES
# =============================================================================

@dataclass
class FAQ:
    """Representa uma pergunta frequente"""
    id: str
    question: str
    answer: str
    category: str = "geral"
    keywords: List[str] = field(default_factory=list)
    views: int = 0
    helpful_votes: int = 0
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())


@dataclass
class KnowledgeDocument:
    """Representa um documento da base de conhecimento"""
    id: str
    title: str
    content: str
    doc_type: str  # text, pdf, markdown, url
    source: str = ""
    chunks: List[Dict] = field(default_factory=list)
    metadata: Dict = field(default_factory=dict)
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())


@dataclass
class ResponseTemplate:
    """Template de resposta configuravel"""
    id: str
    trigger: str
    template: str
    category: str
    variables: List[str] = field(default_factory=list)
    conditions: Dict = field(default_factory=dict)
    priority: int = 0


@dataclass
class ConversationFlow:
    """Fluxo de conversa (arvore de decisao)"""
    id: str
    name: str
    description: str = ""
    nodes: List[Dict] = field(default_factory=list)
    edges: List[Dict] = field(default_factory=list)
    entry_keywords: List[str] = field(default_factory=list)
    active: bool = True


@dataclass
class ChatbotConfig:
    """Configuracao completa de um chatbot"""
    chatbot_id: str
    name: str
    description: str = ""

    # Personalidade
    personality: Dict = field(default_factory=lambda: {
        "name": "Assistente",
        "tone": "professional",
        "language": "pt-BR",
        "greeting": "Ola! Como posso ajudar?",
        "farewell": "Ate mais!",
        "fallback": "Desculpe, nao entendi."
    })

    # Configuracao de IA
    ai_config: Dict = field(default_factory=lambda: {
        "model": "claude-sonnet-4-20250514",
        "temperature": 0.7,
        "max_tokens": 1024,
        "system_prompt": ""
    })

    # Base de conhecimento
    faqs: List[FAQ] = field(default_factory=list)
    documents: List[KnowledgeDocument] = field(default_factory=list)

    # Templates e fluxos
    response_templates: List[ResponseTemplate] = field(default_factory=list)
    conversation_flows: List[ConversationFlow] = field(default_factory=list)

    # Configuracao de escalacao
    escalation_config: Dict = field(default_factory=lambda: {
        "enabled": True,
        "max_attempts": 3,
        "triggers": ["frustration", "explicit_request"]
    })

    # Canais habilitados
    channels: Dict = field(default_factory=lambda: {
        "widget": False,
        "api": True
    })

    # Metadados
    status: str = "draft"
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    updated_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())


# =============================================================================
# CHATBOT BUILDER
# =============================================================================

class ChatbotBuilder:
    """
    Construtor de Chatbots de Suporte ao Cliente

    Permite criar, configurar e exportar chatbots completos com:
    - Base de conhecimento
    - Templates de resposta
    - Fluxos de conversa
    - Integracao com Claude
    """

    def __init__(self, chatbot_id: Optional[str] = None):
        """
        Inicializa o builder

        Args:
            chatbot_id: ID do chatbot (gera automaticamente se nao informado)
        """
        self.chatbot_id = chatbot_id or self._generate_id()
        self.config = ChatbotConfig(
            chatbot_id=self.chatbot_id,
            name="Novo Chatbot"
        )
        self._claude_client = None

    def _generate_id(self) -> str:
        """Gera ID unico para o chatbot"""
        timestamp = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
        unique = str(uuid.uuid4())[:6].upper()
        return f"BOT-{timestamp}-{unique}"

    # =========================================================================
    # CONFIGURACAO BASICA
    # =========================================================================

    def set_name(self, name: str) -> 'ChatbotBuilder':
        """Define nome do chatbot"""
        self.config.name = name
        return self

    def set_description(self, description: str) -> 'ChatbotBuilder':
        """Define descricao do chatbot"""
        self.config.description = description
        return self

    def set_personality(
        self,
        name: str = "Assistente",
        tone: str = "professional",
        language: str = "pt-BR",
        greeting: Optional[str] = None,
        farewell: Optional[str] = None,
        fallback: Optional[str] = None
    ) -> 'ChatbotBuilder':
        """
        Configura personalidade do chatbot

        Args:
            name: Nome do assistente
            tone: Tom (professional, friendly, casual, technical)
            language: Idioma (pt-BR, en-US, etc)
            greeting: Mensagem de saudacao
            farewell: Mensagem de despedida
            fallback: Mensagem quando nao entende
        """
        self.config.personality = {
            "name": name,
            "tone": tone,
            "language": language,
            "greeting": greeting or f"Ola! Sou {name}, seu assistente virtual. Como posso ajudar?",
            "farewell": farewell or "Obrigado pelo contato! Ate mais!",
            "fallback": fallback or "Desculpe, nao entendi. Pode reformular sua pergunta?"
        }
        return self

    def set_ai_config(
        self,
        model: str = "claude-sonnet-4-20250514",
        temperature: float = 0.7,
        max_tokens: int = 1024,
        system_prompt: str = ""
    ) -> 'ChatbotBuilder':
        """
        Configura modelo de IA

        Args:
            model: Modelo Claude a usar
            temperature: Temperatura (0-1)
            max_tokens: Maximo de tokens na resposta
            system_prompt: Prompt de sistema customizado
        """
        self.config.ai_config = {
            "model": model,
            "temperature": temperature,
            "max_tokens": max_tokens,
            "system_prompt": system_prompt or self._generate_system_prompt()
        }
        return self

    def _generate_system_prompt(self) -> str:
        """Gera system prompt baseado na configuracao"""
        personality = self.config.personality
        tone_info = PERSONALITY_TONES.get(personality.get("tone", "professional"), {})
        style_hints = tone_info.get("style_hints", [])

        prompt = f"""Voce e {personality.get('name', 'um assistente')}, um assistente virtual de suporte ao cliente.

PERSONALIDADE:
- Tom: {tone_info.get('name', 'Profissional')}
- Idioma: {personality.get('language', 'pt-BR')}
- Estilo: {', '.join(style_hints)}

DIRETRIZES:
1. Responda sempre no idioma {personality.get('language', 'pt-BR')}
2. Seja util, claro e objetivo
3. Se nao souber a resposta, seja honesto e ofereca alternativas
4. Nunca invente informacoes
5. Mantenha o tom {personality.get('tone', 'profissional')} em todas as respostas

MENSAGENS PADRAO:
- Saudacao: {personality.get('greeting', 'Ola!')}
- Despedida: {personality.get('farewell', 'Ate mais!')}
- Quando nao entender: {personality.get('fallback', 'Desculpe, nao entendi.')}
"""
        return prompt

    # =========================================================================
    # BASE DE CONHECIMENTO
    # =========================================================================

    def add_faq(
        self,
        question: str,
        answer: str,
        category: str = "geral",
        keywords: Optional[List[str]] = None
    ) -> 'ChatbotBuilder':
        """
        Adiciona FAQ a base de conhecimento

        Args:
            question: Pergunta
            answer: Resposta
            category: Categoria da FAQ
            keywords: Palavras-chave para busca
        """
        faq = FAQ(
            id=f"FAQ-{str(uuid.uuid4())[:8].upper()}",
            question=question,
            answer=answer,
            category=category,
            keywords=keywords or self._extract_keywords(question)
        )
        self.config.faqs.append(faq)
        return self

    def add_faqs_bulk(self, faqs: List[Dict]) -> 'ChatbotBuilder':
        """
        Adiciona multiplas FAQs de uma vez

        Args:
            faqs: Lista de dicionarios com question, answer, category
        """
        for faq_data in faqs:
            self.add_faq(
                question=faq_data.get("question", ""),
                answer=faq_data.get("answer", ""),
                category=faq_data.get("category", "geral"),
                keywords=faq_data.get("keywords")
            )
        return self

    def add_document(
        self,
        title: str,
        content: str,
        doc_type: str = "text",
        source: str = ""
    ) -> 'ChatbotBuilder':
        """
        Adiciona documento a base de conhecimento

        Args:
            title: Titulo do documento
            content: Conteudo (texto)
            doc_type: Tipo (text, markdown, pdf, url)
            source: Fonte original
        """
        doc = KnowledgeDocument(
            id=f"DOC-{str(uuid.uuid4())[:8].upper()}",
            title=title,
            content=content,
            doc_type=doc_type,
            source=source,
            chunks=self._chunk_content(content)
        )
        self.config.documents.append(doc)
        return self

    def _extract_keywords(self, text: str) -> List[str]:
        """Extrai palavras-chave de um texto"""
        # Remove pontuacao e converte para minusculo
        words = re.findall(r'\b[a-zA-ZaeiouAEIOU\u00e0-\u00fa]+\b', text.lower())

        # Remove stopwords comuns
        stopwords = {
            'o', 'a', 'os', 'as', 'um', 'uma', 'uns', 'umas',
            'de', 'da', 'do', 'das', 'dos', 'em', 'no', 'na',
            'por', 'para', 'com', 'sem', 'e', 'ou', 'que',
            'como', 'quando', 'onde', 'qual', 'quais', 'quem'
        }

        keywords = [w for w in words if len(w) > 2 and w not in stopwords]
        return list(set(keywords))[:10]  # Maximo 10 keywords

    def _chunk_content(self, content: str, chunk_size: int = 500) -> List[Dict]:
        """Divide conteudo em chunks para processamento"""
        chunks = []
        paragraphs = content.split('\n\n')

        current_chunk = ""
        chunk_id = 0

        for para in paragraphs:
            if len(current_chunk) + len(para) < chunk_size:
                current_chunk += para + "\n\n"
            else:
                if current_chunk:
                    chunks.append({
                        "chunk_id": f"chunk_{chunk_id}",
                        "text": current_chunk.strip(),
                        "size": len(current_chunk)
                    })
                    chunk_id += 1
                current_chunk = para + "\n\n"

        if current_chunk:
            chunks.append({
                "chunk_id": f"chunk_{chunk_id}",
                "text": current_chunk.strip(),
                "size": len(current_chunk)
            })

        return chunks

    # =========================================================================
    # TEMPLATES DE RESPOSTA
    # =========================================================================

    def add_response_template(
        self,
        trigger: str,
        template: str,
        category: str = "general",
        variables: Optional[List[str]] = None,
        priority: int = 0
    ) -> 'ChatbotBuilder':
        """
        Adiciona template de resposta

        Args:
            trigger: Gatilho para usar o template
            template: Texto do template (com {variaveis})
            category: Categoria do template
            variables: Lista de variaveis esperadas
            priority: Prioridade (maior = mais importante)
        """
        # Extrai variaveis do template
        if not variables:
            variables = re.findall(r'\{(\w+)\}', template)

        response_template = ResponseTemplate(
            id=f"TPL-{str(uuid.uuid4())[:8].upper()}",
            trigger=trigger,
            template=template,
            category=category,
            variables=variables,
            priority=priority
        )
        self.config.response_templates.append(response_template)
        return self

    def load_default_templates(self, categories: Optional[List[str]] = None) -> 'ChatbotBuilder':
        """
        Carrega templates padrao

        Args:
            categories: Categorias a carregar (todas se None)
        """
        categories = categories or list(RESPONSE_TEMPLATES.keys())

        for category in categories:
            templates = RESPONSE_TEMPLATES.get(category, [])
            for tpl in templates:
                self.add_response_template(
                    trigger=tpl["trigger"],
                    template=tpl["template"],
                    category=tpl["category"]
                )

        return self

    # =========================================================================
    # FLUXOS DE CONVERSA
    # =========================================================================

    def add_conversation_flow(
        self,
        name: str,
        nodes: List[Dict],
        edges: List[Dict],
        description: str = "",
        entry_keywords: Optional[List[str]] = None
    ) -> 'ChatbotBuilder':
        """
        Adiciona fluxo de conversa

        Args:
            name: Nome do fluxo
            nodes: Lista de nos (estados)
            edges: Lista de arestas (transicoes)
            description: Descricao do fluxo
            entry_keywords: Keywords que ativam o fluxo
        """
        flow = ConversationFlow(
            id=f"FLOW-{str(uuid.uuid4())[:8].upper()}",
            name=name,
            description=description,
            nodes=nodes,
            edges=edges,
            entry_keywords=entry_keywords or []
        )
        self.config.conversation_flows.append(flow)
        return self

    def load_flow_template(self, template_name: str) -> 'ChatbotBuilder':
        """
        Carrega template de fluxo pre-definido

        Args:
            template_name: Nome do template (support_ticket, product_inquiry, faq_navigation)
        """
        template = CONVERSATION_FLOW_TEMPLATES.get(template_name)
        if template:
            self.add_conversation_flow(
                name=template["name"],
                nodes=template["nodes"],
                edges=template["edges"],
                description=template.get("description", "")
            )
        return self

    # =========================================================================
    # CONFIGURACAO DE ESCALACAO
    # =========================================================================

    def set_escalation_config(
        self,
        enabled: bool = True,
        max_attempts: int = 3,
        triggers: Optional[List[str]] = None,
        sensitive_topics: Optional[List[str]] = None,
        frustration_keywords: Optional[List[str]] = None,
        webhook_url: str = ""
    ) -> 'ChatbotBuilder':
        """
        Configura regras de escalacao para humano

        Args:
            enabled: Se escalacao esta habilitada
            max_attempts: Maximo de tentativas antes de escalar
            triggers: Gatilhos de escalacao
            sensitive_topics: Topicos que devem escalar
            frustration_keywords: Palavras que indicam frustracao
            webhook_url: URL para notificar escalacao
        """
        self.config.escalation_config = {
            "enabled": enabled,
            "max_attempts": max_attempts,
            "triggers": triggers or ["frustration", "explicit_request", "sensitive_topic"],
            "sensitive_topics": sensitive_topics or ["reclamacao", "reembolso", "cancelamento"],
            "frustration_keywords": frustration_keywords or [
                "falar com atendente", "nao funciona", "problema",
                "nao resolve", "pessimo", "horrivel"
            ],
            "webhook_url": webhook_url
        }
        return self

    # =========================================================================
    # CANAIS DE DEPLOY
    # =========================================================================

    def enable_channel(self, channel: str, config: Optional[Dict] = None) -> 'ChatbotBuilder':
        """
        Habilita canal de deploy

        Args:
            channel: Canal (widget, whatsapp, telegram, slack, teams, api)
            config: Configuracao especifica do canal
        """
        self.config.channels[channel] = {
            "enabled": True,
            "config": config or {}
        }
        return self

    def disable_channel(self, channel: str) -> 'ChatbotBuilder':
        """Desabilita canal de deploy"""
        if channel in self.config.channels:
            self.config.channels[channel]["enabled"] = False
        return self

    # =========================================================================
    # INTEGRACAO COM CLAUDE
    # =========================================================================

    def get_claude_client(self):
        """Obtem cliente Claude (lazy loading)"""
        if self._claude_client is None:
            try:
                import anthropic
                api_key = os.getenv("ANTHROPIC_API_KEY")
                if api_key:
                    self._claude_client = anthropic.Anthropic(api_key=api_key)
            except ImportError:
                print("[ChatbotBuilder] anthropic nao instalado")
        return self._claude_client

    async def generate_response(
        self,
        user_message: str,
        conversation_history: Optional[List[Dict]] = None,
        context: Optional[Dict] = None
    ) -> Dict:
        """
        Gera resposta usando Claude

        Args:
            user_message: Mensagem do usuario
            conversation_history: Historico da conversa
            context: Contexto adicional (pagina, usuario, etc)

        Returns:
            Dict com response, confidence, should_escalate, etc
        """
        client = self.get_claude_client()
        if not client:
            return {
                "response": self.config.personality.get("fallback", "Desculpe, nao entendi."),
                "confidence": 0,
                "error": "Claude client not available"
            }

        # Verifica escalacao
        escalation_check = self._check_escalation(user_message)
        if escalation_check["should_escalate"]:
            return {
                "response": self.config.personality.get("transfer_message",
                    "Vou transferir voce para um atendente."),
                "should_escalate": True,
                "escalation_reason": escalation_check["reason"]
            }

        # Busca contexto relevante na base de conhecimento
        relevant_context = self._find_relevant_context(user_message)

        # Monta mensagens
        messages = []

        # Adiciona historico
        if conversation_history:
            for msg in conversation_history[-10:]:  # Ultimas 10 mensagens
                messages.append({
                    "role": msg.get("role", "user"),
                    "content": msg.get("content", "")
                })

        # Adiciona mensagem atual com contexto
        enhanced_message = user_message
        if relevant_context:
            enhanced_message = f"""Pergunta do usuario: {user_message}

Contexto relevante da base de conhecimento:
{relevant_context}

Responda baseando-se no contexto acima quando aplicavel."""

        messages.append({"role": "user", "content": enhanced_message})

        try:
            # Chama Claude
            response = client.messages.create(
                model=self.config.ai_config.get("model", "claude-sonnet-4-20250514"),
                max_tokens=self.config.ai_config.get("max_tokens", 1024),
                system=self.config.ai_config.get("system_prompt", self._generate_system_prompt()),
                messages=messages
            )

            response_text = response.content[0].text

            return {
                "response": response_text,
                "confidence": 0.9,
                "should_escalate": False,
                "model_used": response.model,
                "tokens_used": response.usage.output_tokens
            }

        except Exception as e:
            return {
                "response": self.config.personality.get("fallback", "Desculpe, ocorreu um erro."),
                "confidence": 0,
                "error": str(e)
            }

    def _check_escalation(self, message: str) -> Dict:
        """Verifica se deve escalar para humano"""
        if not self.config.escalation_config.get("enabled", True):
            return {"should_escalate": False}

        message_lower = message.lower()

        # Verifica palavras de frustracao
        frustration_keywords = self.config.escalation_config.get("frustration_keywords", [])
        for keyword in frustration_keywords:
            if keyword.lower() in message_lower:
                return {"should_escalate": True, "reason": "frustration"}

        # Verifica topicos sensiveis
        sensitive_topics = self.config.escalation_config.get("sensitive_topics", [])
        for topic in sensitive_topics:
            if topic.lower() in message_lower:
                return {"should_escalate": True, "reason": "sensitive_topic"}

        # Verifica pedido explicito
        explicit_requests = ["falar com humano", "atendente", "pessoa real", "supervisor"]
        for request in explicit_requests:
            if request in message_lower:
                return {"should_escalate": True, "reason": "explicit_request"}

        return {"should_escalate": False}

    def _find_relevant_context(self, message: str) -> str:
        """Busca contexto relevante na base de conhecimento"""
        message_lower = message.lower()
        relevant_parts = []

        # Busca em FAQs
        for faq in self.config.faqs:
            # Verifica keywords
            for keyword in faq.keywords:
                if keyword.lower() in message_lower:
                    relevant_parts.append(f"FAQ: {faq.question}\nResposta: {faq.answer}")
                    break
            # Verifica pergunta
            if any(word in faq.question.lower() for word in message_lower.split()):
                if faq.question not in [p.split('\n')[0].replace('FAQ: ', '') for p in relevant_parts]:
                    relevant_parts.append(f"FAQ: {faq.question}\nResposta: {faq.answer}")

        # Busca em documentos (chunks relevantes)
        for doc in self.config.documents:
            for chunk in doc.chunks:
                chunk_text = chunk.get("text", "").lower()
                # Conta palavras em comum
                words_in_common = sum(1 for word in message_lower.split() if word in chunk_text)
                if words_in_common >= 2:
                    relevant_parts.append(f"Documento '{doc.title}':\n{chunk.get('text', '')}")
                    break

        return "\n\n---\n\n".join(relevant_parts[:3])  # Maximo 3 contextos

    # =========================================================================
    # EXPORTACAO E DEPLOY
    # =========================================================================

    def build(self) -> ChatbotConfig:
        """
        Finaliza e retorna a configuracao do chatbot

        Returns:
            ChatbotConfig completo
        """
        self.config.updated_at = datetime.now(timezone.utc).isoformat()
        self.config.status = "ready"

        # Gera system prompt final se nao definido
        if not self.config.ai_config.get("system_prompt"):
            self.config.ai_config["system_prompt"] = self._generate_system_prompt()

        return self.config

    def export_to_json(self, filepath: Optional[str] = None) -> str:
        """
        Exporta configuracao para JSON

        Args:
            filepath: Caminho do arquivo (gera automatico se nao informado)

        Returns:
            Caminho do arquivo exportado
        """
        config = self.build()

        if not filepath:
            filepath = EXPORT_DIR / f"{self.chatbot_id}.json"

        export_data = {
            "chatbot_id": config.chatbot_id,
            "name": config.name,
            "description": config.description,
            "personality": config.personality,
            "ai_config": config.ai_config,
            "faqs": [asdict(faq) for faq in config.faqs],
            "documents": [asdict(doc) for doc in config.documents],
            "response_templates": [asdict(tpl) for tpl in config.response_templates],
            "conversation_flows": [asdict(flow) for flow in config.conversation_flows],
            "escalation_config": config.escalation_config,
            "channels": config.channels,
            "status": config.status,
            "created_at": config.created_at,
            "updated_at": config.updated_at,
            "version": "1.0.0",
            "exported_at": datetime.now(timezone.utc).isoformat()
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(export_data, f, ensure_ascii=False, indent=2)

        return str(filepath)

    def export_widget_code(self, api_endpoint: str = "") -> str:
        """
        Gera codigo HTML/JS para widget embarcavel

        Args:
            api_endpoint: URL da API do chatbot

        Returns:
            Codigo HTML/JS do widget
        """
        personality = self.config.personality
        api_endpoint = api_endpoint or f"/api/chatbot/{self.chatbot_id}/chat"

        widget_code = f'''<!-- Chatbot Widget - {self.config.name} -->
<style>
.chatbot-widget {{
    position: fixed;
    bottom: 20px;
    right: 20px;
    z-index: 9999;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}}

.chatbot-toggle {{
    width: 60px;
    height: 60px;
    border-radius: 50%;
    background: #003B4A;
    border: none;
    cursor: pointer;
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    display: flex;
    align-items: center;
    justify-content: center;
    transition: transform 0.2s;
}}

.chatbot-toggle:hover {{
    transform: scale(1.05);
}}

.chatbot-toggle svg {{
    width: 28px;
    height: 28px;
    fill: white;
}}

.chatbot-window {{
    position: absolute;
    bottom: 70px;
    right: 0;
    width: 380px;
    height: 520px;
    background: white;
    border-radius: 16px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.15);
    display: none;
    flex-direction: column;
    overflow: hidden;
}}

.chatbot-window.open {{
    display: flex;
}}

.chatbot-header {{
    background: #003B4A;
    color: white;
    padding: 16px;
    display: flex;
    align-items: center;
    gap: 12px;
}}

.chatbot-avatar {{
    width: 40px;
    height: 40px;
    border-radius: 50%;
    background: #FF6C00;
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: bold;
}}

.chatbot-info h3 {{
    margin: 0;
    font-size: 16px;
}}

.chatbot-info p {{
    margin: 2px 0 0;
    font-size: 12px;
    opacity: 0.8;
}}

.chatbot-messages {{
    flex: 1;
    padding: 16px;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 12px;
}}

.message {{
    max-width: 80%;
    padding: 12px 16px;
    border-radius: 16px;
    font-size: 14px;
    line-height: 1.4;
}}

.message.bot {{
    background: #f0f0f0;
    align-self: flex-start;
    border-bottom-left-radius: 4px;
}}

.message.user {{
    background: #003B4A;
    color: white;
    align-self: flex-end;
    border-bottom-right-radius: 4px;
}}

.chatbot-input {{
    padding: 16px;
    border-top: 1px solid #eee;
    display: flex;
    gap: 8px;
}}

.chatbot-input input {{
    flex: 1;
    padding: 12px 16px;
    border: 1px solid #ddd;
    border-radius: 24px;
    font-size: 14px;
    outline: none;
}}

.chatbot-input input:focus {{
    border-color: #003B4A;
}}

.chatbot-input button {{
    width: 44px;
    height: 44px;
    border-radius: 50%;
    background: #FF6C00;
    border: none;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
}}

.chatbot-input button svg {{
    width: 20px;
    height: 20px;
    fill: white;
}}

.typing-indicator {{
    display: flex;
    gap: 4px;
    padding: 12px 16px;
    background: #f0f0f0;
    border-radius: 16px;
    align-self: flex-start;
    width: fit-content;
}}

.typing-indicator span {{
    width: 8px;
    height: 8px;
    background: #999;
    border-radius: 50%;
    animation: typing 1.4s infinite;
}}

.typing-indicator span:nth-child(2) {{ animation-delay: 0.2s; }}
.typing-indicator span:nth-child(3) {{ animation-delay: 0.4s; }}

@keyframes typing {{
    0%, 60%, 100% {{ transform: translateY(0); }}
    30% {{ transform: translateY(-4px); }}
}}
</style>

<div class="chatbot-widget" id="chatbot-widget-{self.chatbot_id}">
    <div class="chatbot-window" id="chatbot-window">
        <div class="chatbot-header">
            <div class="chatbot-avatar">{personality.get('name', 'A')[0].upper()}</div>
            <div class="chatbot-info">
                <h3>{personality.get('name', 'Assistente')}</h3>
                <p>Online agora</p>
            </div>
        </div>
        <div class="chatbot-messages" id="chatbot-messages">
            <div class="message bot">{personality.get('greeting', 'Ola! Como posso ajudar?')}</div>
        </div>
        <div class="chatbot-input">
            <input type="text" id="chatbot-input" placeholder="Digite sua mensagem..." />
            <button onclick="sendChatMessage()">
                <svg viewBox="0 0 24 24"><path d="M2.01 21L23 12 2.01 3 2 10l15 2-15 2z"/></svg>
            </button>
        </div>
    </div>
    <button class="chatbot-toggle" onclick="toggleChatbot()">
        <svg viewBox="0 0 24 24"><path d="M20 2H4c-1.1 0-2 .9-2 2v18l4-4h14c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 14H6l-2 2V4h16v12z"/></svg>
    </button>
</div>

<script>
(function() {{
    const chatbotId = '{self.chatbot_id}';
    const apiEndpoint = '{api_endpoint}';
    let conversationId = null;

    window.toggleChatbot = function() {{
        const window = document.getElementById('chatbot-window');
        window.classList.toggle('open');
        if (window.classList.contains('open')) {{
            document.getElementById('chatbot-input').focus();
        }}
    }};

    window.sendChatMessage = async function() {{
        const input = document.getElementById('chatbot-input');
        const message = input.value.trim();
        if (!message) return;

        const messagesDiv = document.getElementById('chatbot-messages');

        // Adiciona mensagem do usuario
        messagesDiv.innerHTML += `<div class="message user">${{message}}</div>`;
        input.value = '';
        messagesDiv.scrollTop = messagesDiv.scrollHeight;

        // Mostra indicador de digitacao
        const typingId = 'typing-' + Date.now();
        messagesDiv.innerHTML += `<div class="typing-indicator" id="${{typingId}}"><span></span><span></span><span></span></div>`;
        messagesDiv.scrollTop = messagesDiv.scrollHeight;

        try {{
            const response = await fetch(apiEndpoint, {{
                method: 'POST',
                headers: {{ 'Content-Type': 'application/json' }},
                body: JSON.stringify({{
                    chatbot_id: chatbotId,
                    conversation_id: conversationId,
                    message: message
                }})
            }});

            const data = await response.json();
            conversationId = data.conversation_id;

            // Remove indicador de digitacao
            document.getElementById(typingId)?.remove();

            // Adiciona resposta do bot
            messagesDiv.innerHTML += `<div class="message bot">${{data.response}}</div>`;
            messagesDiv.scrollTop = messagesDiv.scrollHeight;

        }} catch (error) {{
            document.getElementById(typingId)?.remove();
            messagesDiv.innerHTML += '<div class="message bot">Desculpe, ocorreu um erro. Tente novamente.</div>';
        }}
    }};

    // Enter para enviar
    document.getElementById('chatbot-input')?.addEventListener('keypress', function(e) {{
        if (e.key === 'Enter') sendChatMessage();
    }});
}})();
</script>
'''
        return widget_code

    def export_api_spec(self) -> Dict:
        """
        Gera especificacao da API do chatbot

        Returns:
            Dict com endpoints e schemas
        """
        return {
            "openapi": "3.0.0",
            "info": {
                "title": f"API do Chatbot: {self.config.name}",
                "version": "1.0.0",
                "description": self.config.description
            },
            "paths": {
                f"/api/chatbot/{self.chatbot_id}/chat": {
                    "post": {
                        "summary": "Enviar mensagem para o chatbot",
                        "requestBody": {
                            "required": True,
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "message": {"type": "string"},
                                            "conversation_id": {"type": "string"},
                                            "user_id": {"type": "string"},
                                            "context": {"type": "object"}
                                        },
                                        "required": ["message"]
                                    }
                                }
                            }
                        },
                        "responses": {
                            "200": {
                                "description": "Resposta do chatbot",
                                "content": {
                                    "application/json": {
                                        "schema": {
                                            "type": "object",
                                            "properties": {
                                                "response": {"type": "string"},
                                                "conversation_id": {"type": "string"},
                                                "should_escalate": {"type": "boolean"},
                                                "confidence": {"type": "number"}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                f"/api/chatbot/{self.chatbot_id}/conversations": {
                    "get": {
                        "summary": "Listar conversas",
                        "responses": {
                            "200": {
                                "description": "Lista de conversas"
                            }
                        }
                    }
                },
                f"/api/chatbot/{self.chatbot_id}/analytics": {
                    "get": {
                        "summary": "Obter metricas e analytics",
                        "responses": {
                            "200": {
                                "description": "Metricas do chatbot"
                            }
                        }
                    }
                }
            }
        }

    def __repr__(self) -> str:
        return f"<ChatbotBuilder {self.chatbot_id}: {self.config.name}>"


# =============================================================================
# FUNCOES UTILITARIAS
# =============================================================================

def create_chatbot(
    name: str,
    description: str = "",
    tone: str = "professional",
    faqs: Optional[List[Dict]] = None,
    templates: Optional[List[str]] = None
) -> ChatbotBuilder:
    """
    Funcao helper para criar chatbot rapidamente

    Args:
        name: Nome do chatbot
        description: Descricao
        tone: Tom (professional, friendly, casual, technical)
        faqs: Lista de FAQs [{question, answer, category}]
        templates: Categorias de templates a carregar

    Returns:
        ChatbotBuilder configurado
    """
    builder = ChatbotBuilder()
    builder.set_name(name)
    builder.set_description(description)
    builder.set_personality(name=name, tone=tone)

    if faqs:
        builder.add_faqs_bulk(faqs)

    if templates:
        builder.load_default_templates(templates)

    return builder


def load_chatbot_from_json(filepath: str) -> ChatbotBuilder:
    """
    Carrega chatbot de arquivo JSON

    Args:
        filepath: Caminho do arquivo

    Returns:
        ChatbotBuilder com configuracao carregada
    """
    with open(filepath, 'r', encoding='utf-8') as f:
        data = json.load(f)

    builder = ChatbotBuilder(chatbot_id=data.get("chatbot_id"))
    builder.set_name(data.get("name", "Chatbot"))
    builder.set_description(data.get("description", ""))

    # Carrega personalidade
    personality = data.get("personality", {})
    builder.set_personality(**personality)

    # Carrega AI config
    ai_config = data.get("ai_config", {})
    builder.set_ai_config(**ai_config)

    # Carrega FAQs
    for faq_data in data.get("faqs", []):
        builder.add_faq(
            question=faq_data.get("question", ""),
            answer=faq_data.get("answer", ""),
            category=faq_data.get("category", "geral"),
            keywords=faq_data.get("keywords")
        )

    # Carrega templates
    for tpl_data in data.get("response_templates", []):
        builder.add_response_template(
            trigger=tpl_data.get("trigger", ""),
            template=tpl_data.get("template", ""),
            category=tpl_data.get("category", "general")
        )

    # Carrega fluxos
    for flow_data in data.get("conversation_flows", []):
        builder.add_conversation_flow(
            name=flow_data.get("name", ""),
            nodes=flow_data.get("nodes", []),
            edges=flow_data.get("edges", []),
            description=flow_data.get("description", "")
        )

    # Carrega escalation config
    builder.set_escalation_config(**data.get("escalation_config", {}))

    return builder


# =============================================================================
# EXPORTACOES
# =============================================================================

__all__ = [
    # Classes principais
    "ChatbotBuilder",
    "ChatbotConfig",
    "FAQ",
    "KnowledgeDocument",
    "ResponseTemplate",
    "ConversationFlow",

    # Funcoes utilitarias
    "create_chatbot",
    "load_chatbot_from_json",

    # Constantes
    "PERSONALITY_TONES",
    "RESPONSE_TEMPLATES",
    "CONVERSATION_FLOW_TEMPLATES",
    "EXPORT_DIR"
]


# =============================================================================
# EXEMPLO DE USO
# =============================================================================

if __name__ == "__main__":
    # Exemplo: Criar chatbot de suporte para e-commerce
    print("=" * 60)
    print("  Exemplo: Criando Chatbot de Suporte E-commerce")
    print("=" * 60)

    # Cria builder
    builder = create_chatbot(
        name="Loja Virtual",
        description="Assistente de suporte para clientes da loja",
        tone="friendly",
        templates=["greeting", "farewell", "support", "sales"]
    )

    # Adiciona FAQs
    builder.add_faqs_bulk([
        {
            "question": "Qual o prazo de entrega?",
            "answer": "O prazo de entrega varia de 3 a 10 dias uteis, dependendo da regiao.",
            "category": "entrega"
        },
        {
            "question": "Como faco para trocar um produto?",
            "answer": "Voce tem ate 30 dias para solicitar troca. Basta acessar Meus Pedidos e clicar em Trocar.",
            "category": "trocas"
        },
        {
            "question": "Quais formas de pagamento aceitas?",
            "answer": "Aceitamos cartao de credito (ate 12x), boleto bancario, PIX e PayPal.",
            "category": "pagamento"
        }
    ])

    # Adiciona fluxo de abertura de chamado
    builder.load_flow_template("support_ticket")

    # Configura escalacao
    builder.set_escalation_config(
        enabled=True,
        max_attempts=3,
        sensitive_topics=["reembolso", "cancelar", "reclamacao"],
        frustration_keywords=["absurdo", "nao funciona", "pessimo"]
    )

    # Habilita canais
    builder.enable_channel("widget", {"position": "bottom-right"})
    builder.enable_channel("whatsapp", {"phone": "+5511999999999"})

    # Exporta
    config = builder.build()
    json_path = builder.export_to_json()

    print(f"\nChatbot criado: {config.name}")
    print(f"ID: {config.chatbot_id}")
    print(f"FAQs: {len(config.faqs)}")
    print(f"Templates: {len(config.response_templates)}")
    print(f"Fluxos: {len(config.conversation_flows)}")
    print(f"\nExportado para: {json_path}")

    # Gera widget
    widget_code = builder.export_widget_code("/api/chatbot/chat")
    widget_path = EXPORT_DIR / f"{config.chatbot_id}_widget.html"
    with open(widget_path, 'w', encoding='utf-8') as f:
        f.write(widget_code)
    print(f"Widget salvo em: {widget_path}")

    print("\n" + "=" * 60)
