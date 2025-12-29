# -*- coding: utf-8 -*-
"""
DM Notifier
===========
Notificador de mensagens diretas do Microsoft Teams.
Permite enviar notificacoes diretamente para usuarios.
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional

from ..graph_client import GraphClient, GraphConfig
from .card_builder import AdaptiveCardBuilder

logger = logging.getLogger(__name__)


@dataclass
class UserMapping:
    """Mapeamento de usuario"""
    name: str
    user_id: str  # Azure AD User ID
    email: str = ""
    chat_id: str = ""  # ID do chat existente (se houver)


@dataclass
class DMNotifierConfig:
    """Configuracao do notificador de DMs"""
    # Configuracao do Graph API (obrigatorio para DMs)
    graph_config: GraphConfig = field(default_factory=GraphConfig)
    # Mapeamento de usuarios
    users: Dict[str, UserMapping] = field(default_factory=dict)
    # Cache de chats criados
    chat_cache: Dict[str, str] = field(default_factory=dict)


class DMNotifier:
    """
    Notificador de mensagens diretas do Microsoft Teams.

    Permite enviar notificacoes diretamente para usuarios
    via Microsoft Graph API.

    Requer:
    - Registro de app no Azure AD
    - Permissoes: Chat.ReadWrite, User.Read

    Exemplo:
        config = DMNotifierConfig(
            graph_config=GraphConfig(
                tenant_id="xxx",
                client_id="xxx",
                client_secret="xxx"
            ),
            users={
                "joao": UserMapping(
                    name="Joao Silva",
                    user_id="user-id-xxx",
                    email="joao@empresa.com"
                )
            }
        )

        notifier = DMNotifier(config)
        await notifier.send_dm("joao", "Sua tarefa foi concluida!")
    """

    def __init__(self, config: DMNotifierConfig):
        """
        Inicializa o notificador.

        Args:
            config: Configuracao do notificador
        """
        self.config = config
        self._graph_client = GraphClient(config.graph_config)
        self._card_builder = AdaptiveCardBuilder()

    async def close(self) -> None:
        """Fecha conexoes"""
        await self._graph_client.close()

    def _get_user(self, user_key: str) -> Optional[UserMapping]:
        """Obtem mapeamento de usuario"""
        return self.config.users.get(user_key)

    async def _get_or_create_chat(self, user: UserMapping) -> Optional[str]:
        """Obtem ou cria chat com usuario"""
        # Verifica cache
        if user.user_id in self.config.chat_cache:
            return self.config.chat_cache[user.user_id]

        # Verifica se tem chat_id configurado
        if user.chat_id:
            self.config.chat_cache[user.user_id] = user.chat_id
            return user.chat_id

        # Cria novo chat
        try:
            result = await self._graph_client.create_chat([user.user_id])
            if result and "id" in result:
                chat_id = result["id"]
                self.config.chat_cache[user.user_id] = chat_id
                return chat_id
        except Exception as e:
            logger.error(f"Erro ao criar chat com {user.name}: {e}")

        return None

    async def send_dm(
        self,
        user_key: str,
        message: str,
        title: str = ""
    ) -> bool:
        """
        Envia mensagem direta para usuario.

        Args:
            user_key: Chave do usuario no mapeamento
            message: Mensagem
            title: Titulo opcional

        Returns:
            True se enviado com sucesso
        """
        user = self._get_user(user_key)
        if not user:
            logger.error(f"Usuario nao encontrado: {user_key}")
            return False

        chat_id = await self._get_or_create_chat(user)
        if not chat_id:
            logger.error(f"Nao foi possivel obter chat para: {user_key}")
            return False

        content = f"<h3>{title}</h3>{message}" if title else message

        try:
            result = await self._graph_client.send_chat_message(chat_id, content)
            return result is not None
        except Exception as e:
            logger.error(f"Erro ao enviar DM para {user_key}: {e}")
            return False

    async def send_dm_card(
        self,
        user_key: str,
        card: Dict[str, Any]
    ) -> bool:
        """
        Envia card adaptativo para usuario.

        Args:
            user_key: Chave do usuario
            card: Card Adaptativo

        Returns:
            True se enviado com sucesso
        """
        user = self._get_user(user_key)
        if not user:
            logger.error(f"Usuario nao encontrado: {user_key}")
            return False

        chat_id = await self._get_or_create_chat(user)
        if not chat_id:
            logger.error(f"Nao foi possivel obter chat para: {user_key}")
            return False

        try:
            result = await self._graph_client.send_chat_card(chat_id, card)
            return result is not None
        except Exception as e:
            logger.error(f"Erro ao enviar card para {user_key}: {e}")
            return False

    async def send_dm_by_email(
        self,
        email: str,
        message: str,
        title: str = ""
    ) -> bool:
        """
        Envia mensagem direta por email.

        Args:
            email: Email do usuario
            message: Mensagem
            title: Titulo opcional

        Returns:
            True se enviado com sucesso
        """
        # Procura usuario pelo email
        user_key = None
        for key, user in self.config.users.items():
            if user.email.lower() == email.lower():
                user_key = key
                break

        if not user_key:
            # Tenta obter usuario via Graph API
            try:
                user_data = await self._graph_client.get_user(email)
                if user_data and "id" in user_data:
                    # Cria mapeamento temporario
                    temp_user = UserMapping(
                        name=user_data.get("displayName", email),
                        user_id=user_data["id"],
                        email=email
                    )
                    self.config.users[email] = temp_user
                    user_key = email
            except Exception as e:
                logger.error(f"Erro ao buscar usuario {email}: {e}")
                return False

        if user_key:
            return await self.send_dm(user_key, message, title)

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
            user_key: Usuario a notificar
            task_id: ID da tarefa
            task_title: Titulo da tarefa
            story_id: ID da story
            story_title: Titulo da story
            url: URL da tarefa
        """
        self._card_builder.clear()

        self._card_builder.add_heading("Nova Tarefa Atribuida")
        self._card_builder.add_text(f"**{task_id}**: {task_title}")

        self._card_builder.add_fact_set([
            ("Story", f"{story_id}: {story_title}"),
            ("Atribuido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
        ], separator=True)

        if url:
            self._card_builder.add_action_url("Ver Tarefa", url)

        card = self._card_builder.build()
        return await self.send_dm_card(user_key, card)

    async def notify_mention(
        self,
        user_key: str,
        mentioned_by: str,
        context: str,
        message: str,
        url: str = ""
    ) -> bool:
        """
        Notifica mencao de usuario.

        Args:
            user_key: Usuario mencionado
            mentioned_by: Quem mencionou
            context: Contexto (story, tarefa, etc)
            message: Mensagem
            url: URL do contexto
        """
        self._card_builder.clear()

        self._card_builder.add_heading("Voce foi mencionado")
        self._card_builder.add_text(f"**{mentioned_by}** mencionou voce em {context}")

        self._card_builder.add_container([
            {"type": "TextBlock", "text": message, "wrap": True}
        ], separator=True)

        if url:
            self._card_builder.add_action_url("Ver Contexto", url)

        card = self._card_builder.build()
        return await self.send_dm_card(user_key, card)

    async def notify_approval_request(
        self,
        user_key: str,
        title: str,
        description: str,
        requester: str,
        approval_data: Dict[str, Any] = None
    ) -> bool:
        """
        Notifica solicitacao de aprovacao.

        Args:
            user_key: Usuario aprovador
            title: Titulo
            description: Descricao
            requester: Solicitante
            approval_data: Dados para acao
        """
        card = self._card_builder.create_approval_card(
            title=title,
            description=description,
            requester=requester,
            details=[
                ("Solicitado em", datetime.now().strftime("%d/%m/%Y %H:%M"))
            ],
            approval_data=approval_data
        )

        return await self.send_dm_card(user_key, card)

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
        Notifica lembrete de prazo.

        Args:
            user_key: Usuario a notificar
            item_type: Tipo (Story, Tarefa, etc)
            item_id: ID do item
            item_title: Titulo
            deadline: Data limite
            days_remaining: Dias restantes
            url: URL do item
        """
        alert_type = "info"
        if days_remaining <= 1:
            alert_type = "error"
        elif days_remaining <= 3:
            alert_type = "warning"

        card = self._card_builder.create_alert_card(
            alert_type=alert_type,
            title=f"Lembrete de Prazo - {item_type}",
            message=f"**{item_id}**: {item_title}",
            details=[
                ("Prazo", deadline.strftime("%d/%m/%Y")),
                ("Dias Restantes", str(days_remaining))
            ],
            action_url=url,
            action_title=f"Ver {item_type}"
        )

        return await self.send_dm_card(user_key, card)

    async def notify_story_completed(
        self,
        user_key: str,
        story_id: str,
        story_title: str,
        tasks_completed: int,
        files_generated: int,
        url: str = ""
    ) -> bool:
        """
        Notifica conclusao de story ao responsavel.

        Args:
            user_key: Usuario responsavel
            story_id: ID da story
            story_title: Titulo
            tasks_completed: Tarefas concluidas
            files_generated: Arquivos gerados
            url: URL da story
        """
        card = self._card_builder.create_alert_card(
            alert_type="success",
            title="Sua Story foi Concluida!",
            message=f"**{story_id}**: {story_title}",
            details=[
                ("Tarefas Concluidas", str(tasks_completed)),
                ("Arquivos Gerados", str(files_generated)),
                ("Concluido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
            ],
            action_url=url,
            action_title="Ver Story"
        )

        return await self.send_dm_card(user_key, card)

    async def notify_error(
        self,
        user_key: str,
        error_type: str,
        error_message: str,
        context: str = "",
        url: str = ""
    ) -> bool:
        """
        Notifica erro ao responsavel.

        Args:
            user_key: Usuario a notificar
            error_type: Tipo de erro
            error_message: Mensagem de erro
            context: Contexto do erro
            url: URL para detalhes
        """
        details = [
            ("Erro", error_message),
            ("Ocorrido em", datetime.now().strftime("%d/%m/%Y %H:%M"))
        ]

        if context:
            details.insert(0, ("Contexto", context))

        card = self._card_builder.create_alert_card(
            alert_type="error",
            title=f"Erro: {error_type}",
            message="Ocorreu um erro que requer sua atencao.",
            details=details,
            action_url=url,
            action_title="Ver Detalhes"
        )

        return await self.send_dm_card(user_key, card)

    # =========================================================================
    # Broadcast
    # =========================================================================

    async def broadcast(
        self,
        message: str,
        title: str = "",
        user_keys: List[str] = None
    ) -> Dict[str, bool]:
        """
        Envia mensagem para multiplos usuarios.

        Args:
            message: Mensagem
            title: Titulo opcional
            user_keys: Lista de usuarios (None = todos)

        Returns:
            Dicionario com resultado por usuario
        """
        if user_keys is None:
            user_keys = list(self.config.users.keys())

        results = {}
        for user_key in user_keys:
            results[user_key] = await self.send_dm(user_key, message, title)

        return results

    async def broadcast_card(
        self,
        card: Dict[str, Any],
        user_keys: List[str] = None
    ) -> Dict[str, bool]:
        """
        Envia card para multiplos usuarios.

        Args:
            card: Card Adaptativo
            user_keys: Lista de usuarios (None = todos)

        Returns:
            Dicionario com resultado por usuario
        """
        if user_keys is None:
            user_keys = list(self.config.users.keys())

        results = {}
        for user_key in user_keys:
            results[user_key] = await self.send_dm_card(user_key, card)

        return results
