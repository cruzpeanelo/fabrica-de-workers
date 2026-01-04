# -*- coding: utf-8 -*-
"""
Teams Bot Skill
===============
Skill para interacao com bot do Microsoft Teams.
Permite que agentes interajam com usuarios via bot.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Awaitable

from ..bot.bot_handler import TeamsBotHandler, BotConfig, ConversationReference
from ..bot.cards import BotCardBuilder
from ..notifications.dm_notifier import DMNotifier, DMNotifierConfig, UserMapping
from ..graph_client import GraphConfig

logger = logging.getLogger(__name__)


@dataclass
class TeamsBotSkillConfig:
    """Configuracao do skill de bot"""
    # Configuracao do bot
    app_id: str = ""
    app_password: str = ""
    # Configuracao do Graph (para DMs)
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    # Mapeamento de usuarios
    users: Dict[str, UserMapping] = field(default_factory=dict)
    # Habilitar skill
    enabled: bool = True
    # Permitir interacao proativa
    allow_proactive: bool = True


class TeamsBotSkill:
    """
    Skill para interacao com bot do Microsoft Teams.

    Este skill permite que os agentes da Fabrica:
    - Enviem mensagens proativas para usuarios
    - Solicitem aprovacoes
    - Coletem feedback
    - Notifiquem usuarios sobre eventos

    Exemplo:
        config = TeamsBotSkillConfig(
            app_id="xxx",
            app_password="xxx",
            tenant_id="xxx",
            client_id="xxx",
            client_secret="xxx"
        )

        skill = TeamsBotSkill(config)

        # Solicitar aprovacao
        await skill.request_approval(
            user_key="joao",
            title="Aprovar Deploy",
            description="Deploy do projeto X para producao"
        )

        # Notificar usuario
        await skill.notify_user(
            user_key="maria",
            message="Sua tarefa foi concluida!"
        )
    """

    skill_name = "teams_bot"
    skill_description = "Interage com usuarios via bot do Microsoft Teams"

    def __init__(self, config: TeamsBotSkillConfig):
        """
        Inicializa o skill.

        Args:
            config: Configuracao do skill
        """
        self.config = config
        self._card_builder = BotCardBuilder()

        # Configura bot handler
        bot_config = BotConfig(
            app_id=config.app_id,
            app_password=config.app_password
        )
        self._bot_handler = TeamsBotHandler(bot_config)

        # Configura DM notifier (se credenciais disponiveis)
        if config.tenant_id and config.client_id and config.client_secret:
            graph_config = GraphConfig(
                tenant_id=config.tenant_id,
                client_id=config.client_id,
                client_secret=config.client_secret
            )
            dm_config = DMNotifierConfig(
                graph_config=graph_config,
                users=config.users
            )
            self._dm_notifier = DMNotifier(dm_config)
        else:
            self._dm_notifier = None

        # Callbacks para acoes
        self._approval_callbacks: Dict[str, Callable] = {}
        self._feedback_callbacks: Dict[str, Callable] = {}

    async def close(self) -> None:
        """Fecha conexoes"""
        if self._dm_notifier:
            await self._dm_notifier.close()

    def set_approval_callback(
        self,
        request_id: str,
        callback: Callable[[str, bool, str], Awaitable[None]]
    ):
        """
        Define callback para aprovacao.

        Args:
            request_id: ID da solicitacao
            callback: Funcao (user_id, approved, comment) -> None
        """
        self._approval_callbacks[request_id] = callback

    def set_feedback_callback(
        self,
        request_id: str,
        callback: Callable[[str, str, int], Awaitable[None]]
    ):
        """
        Define callback para feedback.

        Args:
            request_id: ID da solicitacao
            callback: Funcao (user_id, feedback, rating) -> None
        """
        self._feedback_callbacks[request_id] = callback

    # =========================================================================
    # Mensagens
    # =========================================================================

    async def notify_user(
        self,
        user_key: str,
        message: str,
        title: str = ""
    ) -> bool:
        """
        Envia notificacao para usuario.

        Args:
            user_key: Chave do usuario
            message: Mensagem
            title: Titulo opcional

        Returns:
            True se enviado com sucesso
        """
        if not self.config.enabled or not self._dm_notifier:
            return False

        return await self._dm_notifier.send_dm(user_key, message, title)

    async def notify_user_card(
        self,
        user_key: str,
        card: Dict[str, Any]
    ) -> bool:
        """
        Envia card para usuario.

        Args:
            user_key: Chave do usuario
            card: Card Adaptativo

        Returns:
            True se enviado com sucesso
        """
        if not self.config.enabled or not self._dm_notifier:
            return False

        return await self._dm_notifier.send_dm_card(user_key, card)

    async def broadcast_message(
        self,
        message: str,
        title: str = "",
        user_keys: List[str] = None
    ) -> Dict[str, bool]:
        """
        Envia mensagem para multiplos usuarios.

        Args:
            message: Mensagem
            title: Titulo
            user_keys: Lista de usuarios (None = todos)

        Returns:
            Resultados por usuario
        """
        if not self.config.enabled or not self._dm_notifier:
            return {}

        return await self._dm_notifier.broadcast(message, title, user_keys)

    # =========================================================================
    # Aprovacoes
    # =========================================================================

    async def request_approval(
        self,
        user_key: str,
        request_id: str,
        title: str,
        description: str,
        details: List[tuple] = None,
        callback: Callable = None
    ) -> bool:
        """
        Solicita aprovacao de usuario.

        Args:
            user_key: Usuario aprovador
            request_id: ID da solicitacao
            title: Titulo
            description: Descricao
            details: Detalhes adicionais
            callback: Funcao callback para resposta

        Returns:
            True se enviado com sucesso
        """
        if not self.config.enabled or not self._dm_notifier:
            return False

        if callback:
            self.set_approval_callback(request_id, callback)

        return await self._dm_notifier.notify_approval_request(
            user_key=user_key,
            title=title,
            description=description,
            requester="Plataforma E",
            approval_data={
                "request_id": request_id,
                "type": "approval"
            }
        )

    async def handle_approval_response(
        self,
        request_id: str,
        user_id: str,
        approved: bool,
        comment: str = ""
    ) -> bool:
        """
        Processa resposta de aprovacao.

        Args:
            request_id: ID da solicitacao
            user_id: Usuario que respondeu
            approved: Se aprovou
            comment: Comentario opcional

        Returns:
            True se processado com sucesso
        """
        callback = self._approval_callbacks.get(request_id)

        if callback:
            try:
                await callback(user_id, approved, comment)
                del self._approval_callbacks[request_id]
                return True
            except Exception as e:
                logger.error(f"Erro no callback de aprovacao: {e}")
                return False

        logger.warning(f"Callback nao encontrado para: {request_id}")
        return False

    # =========================================================================
    # Feedback
    # =========================================================================

    async def request_feedback(
        self,
        user_key: str,
        request_id: str,
        title: str,
        message: str,
        allow_rating: bool = True,
        callback: Callable = None
    ) -> bool:
        """
        Solicita feedback de usuario.

        Args:
            user_key: Usuario
            request_id: ID da solicitacao
            title: Titulo
            message: Mensagem
            allow_rating: Permitir nota
            callback: Funcao callback

        Returns:
            True se enviado com sucesso
        """
        if not self.config.enabled or not self._dm_notifier:
            return False

        if callback:
            self.set_feedback_callback(request_id, callback)

        # Cria card de feedback
        self._card_builder._builder.clear()
        self._card_builder._builder.add_heading(title)
        self._card_builder._builder.add_text(message)

        body = self._card_builder._builder._body.copy()

        # Campo de comentario
        body.append({
            "type": "TextBlock",
            "text": "Seu feedback:",
            "weight": "Bolder"
        })
        body.append({
            "type": "Input.Text",
            "id": "feedback",
            "placeholder": "Digite seu feedback...",
            "isMultiline": True
        })

        # Rating (se habilitado)
        if allow_rating:
            body.append({
                "type": "TextBlock",
                "text": "Avaliacao:",
                "weight": "Bolder"
            })
            body.append({
                "type": "Input.ChoiceSet",
                "id": "rating",
                "style": "expanded",
                "choices": [
                    {"title": "1 - Ruim", "value": "1"},
                    {"title": "2 - Regular", "value": "2"},
                    {"title": "3 - Bom", "value": "3"},
                    {"title": "4 - Muito Bom", "value": "4"},
                    {"title": "5 - Excelente", "value": "5"}
                ]
            })

        card = {
            "type": "AdaptiveCard",
            "$schema": "http://adaptivecards.io/schemas/adaptive-card.json",
            "version": "1.4",
            "body": body,
            "actions": [
                {
                    "type": "Action.Submit",
                    "title": "Enviar Feedback",
                    "data": {
                        "request_id": request_id,
                        "type": "feedback"
                    },
                    "style": "positive"
                }
            ]
        }

        return await self._dm_notifier.send_dm_card(user_key, card)

    async def handle_feedback_response(
        self,
        request_id: str,
        user_id: str,
        feedback: str,
        rating: int = 0
    ) -> bool:
        """
        Processa resposta de feedback.

        Args:
            request_id: ID da solicitacao
            user_id: Usuario
            feedback: Texto do feedback
            rating: Nota (1-5)

        Returns:
            True se processado
        """
        callback = self._feedback_callbacks.get(request_id)

        if callback:
            try:
                await callback(user_id, feedback, rating)
                del self._feedback_callbacks[request_id]
                return True
            except Exception as e:
                logger.error(f"Erro no callback de feedback: {e}")
                return False

        logger.warning(f"Callback nao encontrado para: {request_id}")
        return False

    # =========================================================================
    # Notificacoes Especificas
    # =========================================================================

    async def notify_task_assigned(
        self,
        user_key: str,
        task_id: str,
        task_title: str,
        story_id: str,
        story_title: str,
        url: str = ""
    ) -> bool:
        """
        Notifica atribuicao de tarefa.

        Args:
            user_key: Usuario
            task_id: ID da tarefa
            task_title: Titulo
            story_id: ID da story
            story_title: Titulo da story
            url: URL
        """
        if not self._dm_notifier:
            return False

        return await self._dm_notifier.notify_task_assigned(
            user_key=user_key,
            task_id=task_id,
            task_title=task_title,
            story_id=story_id,
            story_title=story_title,
            url=url
        )

    async def notify_deadline_reminder(
        self,
        user_key: str,
        item_type: str,
        item_id: str,
        item_title: str,
        deadline: datetime,
        days_remaining: int,
        url: str = ""
    ) -> bool:
        """
        Envia lembrete de prazo.

        Args:
            user_key: Usuario
            item_type: Tipo do item
            item_id: ID
            item_title: Titulo
            deadline: Data limite
            days_remaining: Dias restantes
            url: URL
        """
        if not self._dm_notifier:
            return False

        return await self._dm_notifier.notify_deadline_reminder(
            user_key=user_key,
            item_type=item_type,
            item_id=item_id,
            item_title=item_title,
            deadline=deadline,
            days_remaining=days_remaining,
            url=url
        )

    async def notify_error(
        self,
        user_key: str,
        error_type: str,
        error_message: str,
        context: str = "",
        url: str = ""
    ) -> bool:
        """
        Notifica erro.

        Args:
            user_key: Usuario
            error_type: Tipo
            error_message: Mensagem
            context: Contexto
            url: URL
        """
        if not self._dm_notifier:
            return False

        return await self._dm_notifier.notify_error(
            user_key=user_key,
            error_type=error_type,
            error_message=error_message,
            context=context,
            url=url
        )

    # =========================================================================
    # Skill Interface
    # =========================================================================

    async def execute(
        self,
        action: str,
        params: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Executa acao do skill.

        Args:
            action: Acao
            params: Parametros

        Returns:
            Resultado
        """
        actions = {
            "notify": self._action_notify,
            "broadcast": self._action_broadcast,
            "request_approval": self._action_request_approval,
            "request_feedback": self._action_request_feedback,
            "notify_task": self._action_notify_task,
            "notify_deadline": self._action_notify_deadline,
            "notify_error": self._action_notify_error
        }

        handler = actions.get(action)
        if not handler:
            return {
                "success": False,
                "error": f"Acao desconhecida: {action}"
            }

        try:
            result = await handler(params)
            return {"success": result}
        except Exception as e:
            logger.error(f"Erro ao executar skill: {e}")
            return {"success": False, "error": str(e)}

    async def _action_notify(self, params: Dict) -> bool:
        """Acao de notificacao"""
        return await self.notify_user(
            user_key=params.get("user_key", ""),
            message=params.get("message", ""),
            title=params.get("title", "")
        )

    async def _action_broadcast(self, params: Dict) -> Dict[str, bool]:
        """Acao de broadcast"""
        return await self.broadcast_message(
            message=params.get("message", ""),
            title=params.get("title", ""),
            user_keys=params.get("user_keys")
        )

    async def _action_request_approval(self, params: Dict) -> bool:
        """Acao de solicitacao de aprovacao"""
        return await self.request_approval(
            user_key=params.get("user_key", ""),
            request_id=params.get("request_id", ""),
            title=params.get("title", ""),
            description=params.get("description", ""),
            details=params.get("details")
        )

    async def _action_request_feedback(self, params: Dict) -> bool:
        """Acao de solicitacao de feedback"""
        return await self.request_feedback(
            user_key=params.get("user_key", ""),
            request_id=params.get("request_id", ""),
            title=params.get("title", ""),
            message=params.get("message", ""),
            allow_rating=params.get("allow_rating", True)
        )

    async def _action_notify_task(self, params: Dict) -> bool:
        """Acao de notificacao de tarefa"""
        return await self.notify_task_assigned(
            user_key=params.get("user_key", ""),
            task_id=params.get("task_id", ""),
            task_title=params.get("task_title", ""),
            story_id=params.get("story_id", ""),
            story_title=params.get("story_title", ""),
            url=params.get("url", "")
        )

    async def _action_notify_deadline(self, params: Dict) -> bool:
        """Acao de lembrete de prazo"""
        deadline = params.get("deadline")
        if isinstance(deadline, str):
            deadline = datetime.fromisoformat(deadline)
        elif not isinstance(deadline, datetime):
            deadline = datetime.now()

        return await self.notify_deadline_reminder(
            user_key=params.get("user_key", ""),
            item_type=params.get("item_type", ""),
            item_id=params.get("item_id", ""),
            item_title=params.get("item_title", ""),
            deadline=deadline,
            days_remaining=params.get("days_remaining", 0),
            url=params.get("url", "")
        )

    async def _action_notify_error(self, params: Dict) -> bool:
        """Acao de notificacao de erro"""
        return await self.notify_error(
            user_key=params.get("user_key", ""),
            error_type=params.get("error_type", ""),
            error_message=params.get("error_message", ""),
            context=params.get("context", ""),
            url=params.get("url", "")
        )
