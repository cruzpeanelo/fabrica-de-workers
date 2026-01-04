# -*- coding: utf-8 -*-
"""
Teams Bot Handler
=================
Handler principal do bot do Microsoft Teams.
Processa mensagens recebidas e envia respostas.

Baseado no Microsoft Bot Framework.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Awaitable
from enum import Enum

from .commands import BotCommandHandler, CommandContext, CommandResult
from .cards import BotCardBuilder

logger = logging.getLogger(__name__)


class ActivityType(str, Enum):
    """Tipos de atividade do Bot Framework"""
    MESSAGE = "message"
    CONVERSATION_UPDATE = "conversationUpdate"
    MESSAGE_REACTION = "messageReaction"
    INVOKE = "invoke"


class InvokeType(str, Enum):
    """Tipos de invoke"""
    ADAPTIVE_CARD_ACTION = "adaptiveCard/action"
    MESSAGING_EXTENSION = "composeExtension/query"
    TASK_FETCH = "task/fetch"
    TASK_SUBMIT = "task/submit"


@dataclass
class BotConfig:
    """Configuracao do bot"""
    app_id: str = ""
    app_password: str = ""
    # Endpoint para receber mensagens
    endpoint: str = "/api/teams/messages"
    # Timeout em segundos
    timeout: int = 30
    # Ativar log de atividades
    log_activities: bool = True
    # Responder em threads
    reply_in_thread: bool = True


@dataclass
class ConversationReference:
    """Referencia para conversa"""
    activity_id: str
    user_id: str
    user_name: str
    conversation_id: str
    conversation_type: str  # personal, groupChat, channel
    channel_id: str = ""
    team_id: str = ""
    service_url: str = ""
    bot_id: str = ""
    bot_name: str = ""


class TeamsBotHandler:
    """
    Handler principal do bot do Microsoft Teams.

    Processa atividades recebidas do Bot Framework e
    retorna respostas apropriadas.

    Exemplo:
        config = BotConfig(
            app_id="xxx",
            app_password="xxx"
        )

        bot = TeamsBotHandler(config)

        # Processa atividade recebida
        response = await bot.process_activity(activity)
    """

    def __init__(self, config: BotConfig):
        """
        Inicializa o handler.

        Args:
            config: Configuracao do bot
        """
        self.config = config
        self._command_handler = BotCommandHandler()
        self._card_builder = BotCardBuilder()
        self._activity_handlers: Dict[str, Callable] = {}
        self._conversation_state: Dict[str, Dict] = {}

        # Registra handlers padrao
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Registra handlers de atividade padrao"""
        self._activity_handlers[ActivityType.MESSAGE.value] = self._on_message
        self._activity_handlers[ActivityType.CONVERSATION_UPDATE.value] = self._on_conversation_update
        self._activity_handlers[ActivityType.MESSAGE_REACTION.value] = self._on_message_reaction
        self._activity_handlers[ActivityType.INVOKE.value] = self._on_invoke

    def register_activity_handler(
        self,
        activity_type: str,
        handler: Callable[[Dict], Awaitable[Dict]]
    ):
        """
        Registra handler customizado para tipo de atividade.

        Args:
            activity_type: Tipo de atividade
            handler: Funcao handler
        """
        self._activity_handlers[activity_type] = handler

    def set_action_callback(
        self,
        action: str,
        callback: Callable[..., Awaitable[Any]]
    ):
        """
        Define callback para acoes de comando.

        Args:
            action: Nome da acao
            callback: Funcao callback
        """
        self._command_handler.set_action_callback(action, callback)

    async def process_activity(
        self,
        activity: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Processa atividade recebida.

        Args:
            activity: Atividade do Bot Framework

        Returns:
            Resposta a enviar ou None
        """
        activity_type = activity.get("type", "")

        if self.config.log_activities:
            logger.info(f"Atividade recebida: {activity_type}")
            logger.debug(f"Activity: {activity}")

        handler = self._activity_handlers.get(activity_type)

        if handler:
            try:
                return await handler(activity)
            except Exception as e:
                logger.error(f"Erro ao processar atividade: {e}")
                return self._create_error_response(
                    activity,
                    f"Erro ao processar: {str(e)}"
                )

        logger.warning(f"Tipo de atividade nao suportado: {activity_type}")
        return None

    def _extract_context(self, activity: Dict) -> CommandContext:
        """Extrai contexto da atividade"""
        conversation = activity.get("conversation", {})
        from_user = activity.get("from", {})
        channel_data = activity.get("channelData", {})

        return CommandContext(
            user_id=from_user.get("id", ""),
            user_name=from_user.get("name", ""),
            team_id=channel_data.get("team", {}).get("id", ""),
            channel_id=channel_data.get("channel", {}).get("id", ""),
            chat_id=conversation.get("id", ""),
            is_channel=conversation.get("conversationType") == "channel",
            mentioned_users=self._extract_mentions(activity)
        )

    def _extract_mentions(self, activity: Dict) -> List[str]:
        """Extrai mencoes da atividade"""
        entities = activity.get("entities", [])
        mentions = []

        for entity in entities:
            if entity.get("type") == "mention":
                mentioned = entity.get("mentioned", {})
                if mentioned.get("id"):
                    mentions.append(mentioned["id"])

        return mentions

    def _create_response(
        self,
        activity: Dict,
        text: str = "",
        card: Dict = None,
        attachments: List[Dict] = None
    ) -> Dict[str, Any]:
        """Cria resposta para atividade"""
        response = {
            "type": "message",
            "from": activity.get("recipient"),
            "recipient": activity.get("from"),
            "conversation": activity.get("conversation"),
            "replyToId": activity.get("id")
        }

        if text:
            response["text"] = text

        if card:
            response["attachments"] = [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "content": card
                }
            ]
        elif attachments:
            response["attachments"] = attachments

        return response

    def _create_error_response(
        self,
        activity: Dict,
        error_message: str
    ) -> Dict[str, Any]:
        """Cria resposta de erro"""
        card = self._card_builder.create_error_card(
            title="Erro",
            message=error_message,
            suggestion="Tente novamente ou digite '/fabrica ajuda'"
        )

        return self._create_response(activity, card=card)

    # =========================================================================
    # Handlers de Atividade
    # =========================================================================

    async def _on_message(self, activity: Dict) -> Optional[Dict]:
        """Handler de mensagem"""
        text = activity.get("text", "").strip()

        if not text:
            return None

        context = self._extract_context(activity)

        # Verifica se ha value (submit de card)
        value = activity.get("value")
        if value:
            result = await self._command_handler.process_action(value, context)
        else:
            result = await self._command_handler.process(text, context)

        # Cria resposta
        if result.card:
            return self._create_response(activity, card=result.card)
        elif result.message:
            return self._create_response(activity, text=result.message)
        elif result.error:
            card = self._card_builder.create_error_card(
                title="Erro",
                message=result.error,
                suggestion=result.data.get("suggestion", "")
            )
            return self._create_response(activity, card=card)

        return None

    async def _on_conversation_update(self, activity: Dict) -> Optional[Dict]:
        """Handler de atualizacao de conversa"""
        members_added = activity.get("membersAdded", [])
        recipient = activity.get("recipient", {})

        for member in members_added:
            # Se o bot foi adicionado
            if member.get("id") == recipient.get("id"):
                context = self._extract_context(activity)
                card = self._card_builder.create_welcome_card(context.user_name)
                return self._create_response(activity, card=card)

        return None

    async def _on_message_reaction(self, activity: Dict) -> Optional[Dict]:
        """Handler de reacao a mensagem"""
        # Por enquanto, apenas loga
        reactions_added = activity.get("reactionsAdded", [])
        reactions_removed = activity.get("reactionsRemoved", [])

        if reactions_added:
            logger.info(f"Reacoes adicionadas: {reactions_added}")
        if reactions_removed:
            logger.info(f"Reacoes removidas: {reactions_removed}")

        return None

    async def _on_invoke(self, activity: Dict) -> Optional[Dict]:
        """Handler de invoke (acoes de card, etc)"""
        invoke_name = activity.get("name", "")

        if invoke_name == InvokeType.ADAPTIVE_CARD_ACTION.value:
            return await self._handle_card_action(activity)
        elif invoke_name == InvokeType.TASK_FETCH.value:
            return await self._handle_task_fetch(activity)
        elif invoke_name == InvokeType.TASK_SUBMIT.value:
            return await self._handle_task_submit(activity)

        logger.warning(f"Invoke nao suportado: {invoke_name}")
        return None

    async def _handle_card_action(self, activity: Dict) -> Optional[Dict]:
        """Handler de acao de card adaptativo"""
        value = activity.get("value", {})
        action = value.get("action", {})

        context = self._extract_context(activity)

        # Processa acao
        data = action.get("data", value)
        result = await self._command_handler.process_action(data, context)

        # Retorna resposta de invoke
        response_body = {}

        if result.card:
            response_body = {
                "statusCode": 200,
                "type": "application/vnd.microsoft.card.adaptive",
                "value": result.card
            }
        elif result.message:
            response_body = {
                "statusCode": 200,
                "type": "application/vnd.microsoft.activity.message",
                "value": result.message
            }

        return {
            "status": 200,
            "body": response_body
        }

    async def _handle_task_fetch(self, activity: Dict) -> Optional[Dict]:
        """Handler de task module fetch"""
        data = activity.get("value", {}).get("data", {})

        # Cria card para o task module
        card = self._card_builder.create_input_card(
            title="Nova Acao",
            fields=[
                {"id": "input", "label": "Entrada", "required": True}
            ]
        )

        return {
            "status": 200,
            "body": {
                "task": {
                    "type": "continue",
                    "value": {
                        "title": "Plataforma E",
                        "card": {
                            "contentType": "application/vnd.microsoft.card.adaptive",
                            "content": card
                        }
                    }
                }
            }
        }

    async def _handle_task_submit(self, activity: Dict) -> Optional[Dict]:
        """Handler de task module submit"""
        data = activity.get("value", {}).get("data", {})
        context = self._extract_context(activity)

        result = await self._command_handler.process_action(data, context)

        if result.success:
            return {
                "status": 200,
                "body": {
                    "task": {
                        "type": "message",
                        "value": result.message or "Acao concluida!"
                    }
                }
            }

        return {
            "status": 200,
            "body": {
                "task": {
                    "type": "message",
                    "value": result.error or "Erro ao processar"
                }
            }
        }

    # =========================================================================
    # Proactive Messaging
    # =========================================================================

    def save_conversation_reference(
        self,
        activity: Dict
    ) -> ConversationReference:
        """
        Salva referencia de conversa para mensagens proativas.

        Args:
            activity: Atividade recebida

        Returns:
            Referencia salva
        """
        conversation = activity.get("conversation", {})
        from_user = activity.get("from", {})
        recipient = activity.get("recipient", {})
        channel_data = activity.get("channelData", {})

        ref = ConversationReference(
            activity_id=activity.get("id", ""),
            user_id=from_user.get("id", ""),
            user_name=from_user.get("name", ""),
            conversation_id=conversation.get("id", ""),
            conversation_type=conversation.get("conversationType", "personal"),
            channel_id=channel_data.get("channel", {}).get("id", ""),
            team_id=channel_data.get("team", {}).get("id", ""),
            service_url=activity.get("serviceUrl", ""),
            bot_id=recipient.get("id", ""),
            bot_name=recipient.get("name", "")
        )

        # Salva no estado
        key = f"{ref.user_id}:{ref.conversation_id}"
        self._conversation_state[key] = {
            "reference": ref,
            "last_activity": datetime.utcnow()
        }

        return ref

    def get_conversation_reference(
        self,
        user_id: str,
        conversation_id: str = None
    ) -> Optional[ConversationReference]:
        """
        Obtem referencia de conversa salva.

        Args:
            user_id: ID do usuario
            conversation_id: ID da conversa (opcional)

        Returns:
            Referencia ou None
        """
        if conversation_id:
            key = f"{user_id}:{conversation_id}"
            state = self._conversation_state.get(key)
            if state:
                return state.get("reference")
        else:
            # Busca qualquer conversa do usuario
            for key, state in self._conversation_state.items():
                if key.startswith(f"{user_id}:"):
                    return state.get("reference")

        return None

    def create_proactive_activity(
        self,
        reference: ConversationReference,
        text: str = "",
        card: Dict = None
    ) -> Dict[str, Any]:
        """
        Cria atividade para envio proativo.

        Args:
            reference: Referencia da conversa
            text: Texto da mensagem
            card: Card adaptativo

        Returns:
            Atividade para envio
        """
        activity = {
            "type": "message",
            "serviceUrl": reference.service_url,
            "channelId": "msteams",
            "from": {
                "id": reference.bot_id,
                "name": reference.bot_name
            },
            "conversation": {
                "id": reference.conversation_id,
                "conversationType": reference.conversation_type
            },
            "recipient": {
                "id": reference.user_id,
                "name": reference.user_name
            }
        }

        if text:
            activity["text"] = text

        if card:
            activity["attachments"] = [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "content": card
                }
            ]

        return activity
