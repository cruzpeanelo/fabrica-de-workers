"""
WhatsApp Integration - Requirements Capture via WhatsApp
Plataforma E v6.5 - Issue #129

Uses Twilio WhatsApp API for receiving messages and Claude for
conversational requirements extraction.
"""

import os
import json
import logging
import asyncio
from typing import Optional, List, Dict, Any, Callable
from datetime import datetime, timedelta
from collections import defaultdict
from enum import Enum

try:
    from twilio.rest import Client as TwilioClient
    from twilio.twiml.messaging_response import MessagingResponse
    TWILIO_AVAILABLE = True
except ImportError:
    TWILIO_AVAILABLE = False
    TwilioClient = None
    MessagingResponse = None

try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    Anthropic = None

from factory.config import ANTHROPIC_API_KEY, CLAUDE_MODEL

logger = logging.getLogger(__name__)


class ConversationState(str, Enum):
    """States for requirement gathering conversation"""
    GREETING = "greeting"
    COLLECTING = "collecting"
    CONFIRMING = "confirming"
    COMPLETED = "completed"
    IDLE = "idle"


class WhatsAppError(Exception):
    """Custom exception for WhatsApp integration errors"""
    pass


class ConversationContext:
    """Manages conversation state and history for a user."""

    # Maximum conversation history size
    MAX_HISTORY_SIZE = 20

    # Conversation timeout in minutes
    CONVERSATION_TIMEOUT = 30

    def __init__(self, phone_number: str, tenant_id: str):
        self.phone_number = phone_number
        self.tenant_id = tenant_id
        self.messages: List[Dict[str, str]] = []
        self.state = ConversationState.GREETING
        self.extracted_stories: List[Dict] = []
        self.last_activity = datetime.utcnow()
        self.created_at = datetime.utcnow()
        self.context_data: Dict[str, Any] = {}

    def add_message(self, role: str, content: str) -> None:
        """Add a message to conversation history."""
        self.messages.append({
            "role": role,
            "content": content,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Trim history if too long
        if len(self.messages) > self.MAX_HISTORY_SIZE:
            self.messages = self.messages[-self.MAX_HISTORY_SIZE:]

        self.last_activity = datetime.utcnow()

    def get_messages_for_claude(self) -> List[Dict[str, str]]:
        """Get messages formatted for Claude API."""
        return [
            {"role": msg["role"], "content": msg["content"]}
            for msg in self.messages
        ]

    def is_expired(self) -> bool:
        """Check if conversation has expired."""
        timeout = timedelta(minutes=self.CONVERSATION_TIMEOUT)
        return datetime.utcnow() - self.last_activity > timeout

    def reset(self) -> None:
        """Reset conversation state."""
        self.messages = []
        self.state = ConversationState.GREETING
        self.extracted_stories = []
        self.last_activity = datetime.utcnow()

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "phone_number": self.phone_number,
            "tenant_id": self.tenant_id,
            "state": self.state.value,
            "message_count": len(self.messages),
            "stories_extracted": len(self.extracted_stories),
            "last_activity": self.last_activity.isoformat(),
            "created_at": self.created_at.isoformat()
        }


class WhatsAppIntegration:
    """
    WhatsApp integration for conversational requirements capture.

    Uses Twilio WhatsApp Business API for messaging and Claude for
    intelligent conversation management and requirements extraction.

    Example:
        whatsapp = WhatsAppIntegration()
        response = await whatsapp.handle_message(
            from_number="+5511999999999",
            message="Preciso de um sistema de vendas",
            tenant_id="tenant-123"
        )
    """

    # System prompt for requirement gathering
    SYSTEM_PROMPT = """Voce e um assistente especializado em coleta de requisitos de software
via conversas naturais. Sua funcao e:

1. Entender as necessidades do usuario atraves de perguntas claras
2. Extrair requisitos funcionais e nao-funcionais
3. Formular User Stories quando tiver informacoes suficientes

Diretrizes:
- Seja amigavel e profissional
- Faca perguntas para esclarecer requisitos vagos
- Confirme seu entendimento antes de prosseguir
- Quando tiver informacoes suficientes para uma User Story, sinalize com [STORY_READY]
- Respostas devem ser curtas (maximo 300 caracteres) para WhatsApp
- Use emojis moderadamente para tornar a conversa mais amigavel

Quando identificar uma User Story completa, responda com:
[STORY_READY]
{
    "title": "...",
    "persona": "...",
    "action": "...",
    "benefit": "...",
    "acceptance_criteria": ["...", "...", "..."]
}

Apos enviar uma story, pergunte se ha mais requisitos a coletar."""

    def __init__(
        self,
        twilio_sid: Optional[str] = None,
        twilio_token: Optional[str] = None,
        twilio_phone: Optional[str] = None,
        claude_model: Optional[str] = None
    ):
        """
        Initialize WhatsApp integration.

        Args:
            twilio_sid: Twilio Account SID (or from env)
            twilio_token: Twilio Auth Token (or from env)
            twilio_phone: Twilio WhatsApp phone number
            claude_model: Claude model to use
        """
        self.twilio_sid = twilio_sid or os.getenv("TWILIO_SID")
        self.twilio_token = twilio_token or os.getenv("TWILIO_TOKEN")
        self.twilio_phone = twilio_phone or os.getenv("TWILIO_WHATSAPP_NUMBER")
        self.claude_model = claude_model or CLAUDE_MODEL

        self.twilio_client = None
        self.claude = None
        self._initialized = False

        # In-memory conversation storage
        # In production, this should use Redis or a database
        self.conversations: Dict[str, ConversationContext] = {}

        # Callbacks for story extraction
        self.on_story_extracted: Optional[Callable] = None

    def _ensure_initialized(self) -> None:
        """Lazily initialize Twilio and Claude clients."""
        if self._initialized:
            return

        # Initialize Twilio (optional - can work without it for webhooks)
        if self.twilio_sid and self.twilio_token:
            if not TWILIO_AVAILABLE:
                logger.warning(
                    "Twilio SDK not installed. Install with: pip install twilio"
                )
            else:
                try:
                    self.twilio_client = TwilioClient(
                        self.twilio_sid,
                        self.twilio_token
                    )
                    logger.info("Twilio client initialized")
                except Exception as e:
                    logger.error(f"Failed to initialize Twilio: {e}")
        else:
            logger.warning(
                "Twilio credentials not configured. "
                "Outbound messaging will not be available."
            )

        # Initialize Claude
        if not ANTHROPIC_AVAILABLE:
            raise WhatsAppError(
                "Anthropic SDK is not installed. Install with: pip install anthropic"
            )

        if not ANTHROPIC_API_KEY:
            raise WhatsAppError(
                "ANTHROPIC_API_KEY is not configured."
            )

        try:
            self.claude = Anthropic(api_key=ANTHROPIC_API_KEY)
        except Exception as e:
            raise WhatsAppError(f"Failed to initialize Claude: {e}")

        self._initialized = True
        logger.info("WhatsAppIntegration initialized")

    def _get_conversation_key(self, phone_number: str, tenant_id: str) -> str:
        """Generate unique key for conversation."""
        return f"{tenant_id}:{phone_number}"

    def get_or_create_conversation(
        self,
        phone_number: str,
        tenant_id: str
    ) -> ConversationContext:
        """Get existing conversation or create new one."""
        key = self._get_conversation_key(phone_number, tenant_id)

        if key in self.conversations:
            context = self.conversations[key]
            # Check if conversation expired
            if context.is_expired():
                logger.info(f"Conversation expired for {phone_number}, resetting")
                context.reset()
            return context

        # Create new conversation
        context = ConversationContext(phone_number, tenant_id)
        self.conversations[key] = context
        logger.info(f"New conversation started for {phone_number}")
        return context

    def _extract_story_from_response(
        self,
        response_text: str
    ) -> Optional[Dict[str, Any]]:
        """Extract User Story JSON from Claude response if present."""
        if "[STORY_READY]" not in response_text:
            return None

        try:
            # Find JSON after the marker
            marker_pos = response_text.find("[STORY_READY]")
            json_start = response_text.find("{", marker_pos)
            json_end = response_text.rfind("}") + 1

            if json_start > 0 and json_end > json_start:
                json_str = response_text[json_start:json_end]
                story = json.loads(json_str)
                story["source"] = "whatsapp"
                story["extracted_at"] = datetime.utcnow().isoformat()
                return story

        except json.JSONDecodeError as e:
            logger.warning(f"Failed to parse story JSON: {e}")

        return None

    def _clean_response_for_whatsapp(self, response_text: str) -> str:
        """Clean Claude response for WhatsApp (remove JSON, limit length)."""
        # Remove story JSON if present
        if "[STORY_READY]" in response_text:
            marker_pos = response_text.find("[STORY_READY]")
            json_start = response_text.find("{", marker_pos)
            json_end = response_text.rfind("}") + 1

            if json_start > 0 and json_end > json_start:
                # Remove the JSON part
                before = response_text[:marker_pos].strip()
                after = response_text[json_end:].strip()
                response_text = f"{before}\n\n{after}".strip()

                # Add confirmation message
                if not after:
                    response_text += "\n\nRequisito capturado! Ha mais alguma coisa que precisa?"

        # Limit length for WhatsApp
        if len(response_text) > 1500:
            response_text = response_text[:1497] + "..."

        return response_text.strip()

    async def process_with_claude(
        self,
        context: ConversationContext
    ) -> Dict[str, Any]:
        """
        Process conversation with Claude.

        Args:
            context: Conversation context

        Returns:
            Dictionary with response and extracted story (if any)
        """
        self._ensure_initialized()

        messages = context.get_messages_for_claude()

        try:
            response = self.claude.messages.create(
                model=self.claude_model,
                max_tokens=500,  # Keep responses short for WhatsApp
                system=self.SYSTEM_PROMPT,
                messages=messages
            )

            response_text = response.content[0].text

            # Check for extracted story
            story = self._extract_story_from_response(response_text)

            # Clean response for WhatsApp
            clean_response = self._clean_response_for_whatsapp(response_text)

            return {
                "response": clean_response,
                "story": story,
                "raw_response": response_text
            }

        except Exception as e:
            logger.error(f"Claude processing failed: {e}")
            raise WhatsAppError(f"Failed to process message: {e}")

    async def handle_message(
        self,
        from_number: str,
        message: str,
        tenant_id: str,
        media_url: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Process incoming WhatsApp message.

        Args:
            from_number: Sender's phone number
            message: Message content
            tenant_id: Tenant identifier
            media_url: Optional media URL (for future voice note support)

        Returns:
            Dictionary with response and any extracted stories
        """
        self._ensure_initialized()

        # Get or create conversation context
        context = self.get_or_create_conversation(from_number, tenant_id)

        # Handle special commands
        if message.lower().strip() in ["/reset", "/reiniciar", "/novo"]:
            context.reset()
            return {
                "response": "Conversa reiniciada! Como posso ajudar com seus requisitos?",
                "stories": [],
                "conversation_reset": True
            }

        if message.lower().strip() in ["/status", "/resumo"]:
            return {
                "response": self._get_status_message(context),
                "stories": context.extracted_stories,
                "conversation_reset": False
            }

        # Add user message to history
        context.add_message("user", message)

        # Process with Claude
        result = await self.process_with_claude(context)

        # Add assistant response to history
        context.add_message("assistant", result["response"])

        # Handle extracted story
        if result.get("story"):
            story = result["story"]
            story["phone_number"] = from_number
            story["tenant_id"] = tenant_id
            context.extracted_stories.append(story)

            # Call callback if registered
            if self.on_story_extracted:
                try:
                    await self.on_story_extracted(story, context)
                except Exception as e:
                    logger.error(f"Story callback failed: {e}")

        return {
            "response": result["response"],
            "stories": [result["story"]] if result.get("story") else [],
            "all_stories": context.extracted_stories,
            "conversation_state": context.state.value
        }

    def _get_status_message(self, context: ConversationContext) -> str:
        """Generate status message for conversation."""
        story_count = len(context.extracted_stories)
        msg_count = len(context.messages)

        return (
            f"Status da conversa:\n"
            f"- Mensagens trocadas: {msg_count}\n"
            f"- Requisitos capturados: {story_count}\n"
            f"- Iniciada em: {context.created_at.strftime('%H:%M')}\n\n"
            f"Comandos disponiveis:\n"
            f"/reset - Reiniciar conversa\n"
            f"/resumo - Ver este status"
        )

    async def send_whatsapp_message(
        self,
        to_number: str,
        message: str
    ) -> Optional[str]:
        """
        Send WhatsApp message via Twilio.

        Args:
            to_number: Recipient phone number
            message: Message content

        Returns:
            Message SID if successful, None otherwise
        """
        if not self.twilio_client or not self.twilio_phone:
            logger.warning("Twilio not configured, cannot send message")
            return None

        try:
            # Format numbers for WhatsApp
            if not to_number.startswith("whatsapp:"):
                to_number = f"whatsapp:{to_number}"
            if not self.twilio_phone.startswith("whatsapp:"):
                from_number = f"whatsapp:{self.twilio_phone}"
            else:
                from_number = self.twilio_phone

            msg = self.twilio_client.messages.create(
                body=message,
                from_=from_number,
                to=to_number
            )

            logger.info(f"Message sent to {to_number}: {msg.sid}")
            return msg.sid

        except Exception as e:
            logger.error(f"Failed to send WhatsApp message: {e}")
            return None

    def create_webhook_response(self, message: str) -> str:
        """
        Create TwiML response for webhook.

        Args:
            message: Response message

        Returns:
            TwiML XML string
        """
        if not TWILIO_AVAILABLE:
            # Return basic XML if Twilio not installed
            return f'<?xml version="1.0" encoding="UTF-8"?><Response><Message>{message}</Message></Response>'

        response = MessagingResponse()
        response.message(message)
        return str(response)

    def extract_tenant_from_number(self, phone_number: str) -> str:
        """
        Extract tenant ID from phone number.

        In production, this would look up the tenant based on the
        phone number or use a default tenant.

        Args:
            phone_number: Sender's phone number

        Returns:
            Tenant ID
        """
        # Default implementation - use phone number as tenant
        # In production, implement proper tenant resolution
        clean_number = phone_number.replace("whatsapp:", "").replace("+", "")
        return f"tenant-{clean_number[-4:]}"

    def get_conversation_history(
        self,
        phone_number: str,
        tenant_id: str
    ) -> Optional[ConversationContext]:
        """Get conversation history for a user."""
        key = self._get_conversation_key(phone_number, tenant_id)
        return self.conversations.get(key)

    def get_all_conversations(
        self,
        tenant_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """Get all active conversations, optionally filtered by tenant."""
        results = []
        for key, context in self.conversations.items():
            if tenant_id and context.tenant_id != tenant_id:
                continue
            results.append(context.to_dict())
        return results

    def cleanup_expired_conversations(self) -> int:
        """Remove expired conversations. Returns count of removed."""
        expired_keys = [
            key for key, context in self.conversations.items()
            if context.is_expired()
        ]
        for key in expired_keys:
            del self.conversations[key]
        return len(expired_keys)

    def get_status(self) -> Dict[str, Any]:
        """Get current status of the WhatsApp integration."""
        return {
            "initialized": self._initialized,
            "twilio_configured": bool(self.twilio_sid and self.twilio_token),
            "twilio_available": TWILIO_AVAILABLE,
            "anthropic_available": ANTHROPIC_AVAILABLE,
            "claude_model": self.claude_model,
            "active_conversations": len(self.conversations),
            "total_stories_extracted": sum(
                len(c.extracted_stories) for c in self.conversations.values()
            )
        }
