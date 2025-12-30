# -*- coding: utf-8 -*-
"""
Slack Integration Module - Bidirectional Integration
=====================================================
Integracao bidirecional completa com Slack.

Funcionalidades:
- Enviar notificacoes para canais
- Receber slash commands (/story, /status)
- Criar stories a partir de mensagens do Slack
- Atualizar status de stories via reacoes
- Webhooks para eventos do Slack
- OAuth 2.0 para autenticacao
- Isolamento por tenant

Configuracao via variaveis de ambiente:
- SLACK_ENABLED: Habilitar integracao
- SLACK_BOT_TOKEN: Token do bot (xoxb-...)
- SLACK_SIGNING_SECRET: Secret para validacao de requests
- SLACK_CLIENT_ID: Client ID para OAuth
- SLACK_CLIENT_SECRET: Client Secret para OAuth
- SLACK_DEFAULT_CHANNEL: Canal padrao para notificacoes

Issue #263 - Integracao com Slack bidirecional
"""

import os
import hmac
import hashlib
import logging
import asyncio
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional
from enum import Enum

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult
)

logger = logging.getLogger(__name__)


class SlackEventType(str, Enum):
    """Tipos de eventos Slack"""
    MESSAGE = "message"
    APP_MENTION = "app_mention"
    REACTION_ADDED = "reaction_added"
    REACTION_REMOVED = "reaction_removed"
    CHANNEL_CREATED = "channel_created"
    MEMBER_JOINED_CHANNEL = "member_joined_channel"
    URL_VERIFICATION = "url_verification"


class SlackReaction(str, Enum):
    """Reacoes mapeadas para status de stories"""
    EYES = "eyes"  # in_progress
    ROCKET = "rocket"  # ready
    WHITE_CHECK_MARK = "white_check_mark"  # done
    HEAVY_CHECK_MARK = "heavy_check_mark"  # done
    HOURGLASS = "hourglass"  # review
    BUG = "bug"  # testing
    X = "x"  # blocked


# Mapeamento de reacoes para status
REACTION_TO_STATUS = {
    "eyes": "in_progress",
    "rocket": "ready",
    "white_check_mark": "done",
    "heavy_check_mark": "done",
    "hourglass": "review",
    "hourglass_flowing_sand": "review",
    "bug": "testing",
    "test_tube": "testing",
    "x": "blocked",
    "octagonal_sign": "blocked",
    "bookmark": "backlog"
}

# Mapeamento de status para emojis
STATUS_TO_EMOJI = {
    "backlog": ":bookmark:",
    "ready": ":rocket:",
    "in_progress": ":eyes:",
    "review": ":hourglass:",
    "testing": ":bug:",
    "done": ":white_check_mark:",
    "blocked": ":x:"
}


@dataclass
class SlackMessage:
    """Mensagem do Slack"""
    channel: str
    user: str
    text: str
    ts: str
    thread_ts: Optional[str] = None
    team: Optional[str] = None
    blocks: Optional[List[Dict]] = None

    @property
    def message_id(self) -> str:
        return f"{self.channel}:{self.ts}"


@dataclass
class SlackUser:
    """Usuario do Slack"""
    id: str
    name: str
    real_name: Optional[str] = None
    email: Optional[str] = None
    is_bot: bool = False


@dataclass
class SlackIntegrationConfig(IntegrationConfig):
    """Configuracao estendida para Slack"""
    bot_token: str = ""
    signing_secret: str = ""
    client_id: str = ""
    client_secret: str = ""
    default_channel: str = ""
    # OAuth
    access_token: str = ""
    refresh_token: str = ""
    team_id: str = ""
    team_name: str = ""
    # Configuracoes de comportamento
    notify_on_story_create: bool = True
    notify_on_status_change: bool = True
    notify_on_task_complete: bool = True
    create_story_from_message: bool = True
    update_status_from_reaction: bool = True
    # Mapeamento de canais
    channel_mapping: Dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_env(cls) -> "SlackIntegrationConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("SLACK_ENABLED", "false").lower() == "true",
            bot_token=os.getenv("SLACK_BOT_TOKEN", ""),
            signing_secret=os.getenv("SLACK_SIGNING_SECRET", ""),
            client_id=os.getenv("SLACK_CLIENT_ID", ""),
            client_secret=os.getenv("SLACK_CLIENT_SECRET", ""),
            default_channel=os.getenv("SLACK_DEFAULT_CHANNEL", ""),
            notify_on_story_create=os.getenv("SLACK_NOTIFY_STORY_CREATE", "true").lower() == "true",
            notify_on_status_change=os.getenv("SLACK_NOTIFY_STATUS_CHANGE", "true").lower() == "true",
            notify_on_task_complete=os.getenv("SLACK_NOTIFY_TASK_COMPLETE", "true").lower() == "true",
            create_story_from_message=os.getenv("SLACK_CREATE_STORY_FROM_MESSAGE", "true").lower() == "true",
            update_status_from_reaction=os.getenv("SLACK_UPDATE_STATUS_FROM_REACTION", "true").lower() == "true",
            auto_sync=os.getenv("SLACK_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("SLACK_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        return bool(self.bot_token or (self.client_id and self.client_secret))


class SlackClient:
    """
    Cliente para API do Slack.

    Fornece:
    - Envio de mensagens e notificacoes
    - Gerenciamento de canais
    - Busca de usuarios
    - Rate limiting automatico

    Exemplo:
    ```python
    client = SlackClient(config)
    await client.connect()

    # Enviar mensagem
    await client.send_message(
        channel="#general",
        text="Hello from Fabrica de Agentes!"
    )

    # Enviar notificacao de story
    await client.notify_story_created(story)
    ```
    """

    API_BASE = "https://slack.com/api"

    def __init__(self, config: SlackIntegrationConfig, tenant_id: Optional[str] = None):
        self.config = config
        self.tenant_id = tenant_id
        self._session: Optional[aiohttp.ClientSession] = None
        self._team_info: Optional[Dict] = None
        self._bot_info: Optional[Dict] = None
        self._rate_limit_remaining: int = 100
        self._rate_limit_reset: Optional[datetime] = None

    @property
    def token(self) -> str:
        """Retorna token de autenticacao"""
        return self.config.access_token or self.config.bot_token

    def _get_headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.token}",
            "Content-Type": "application/json; charset=utf-8"
        }

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(headers=self._get_headers())
        return self._session

    async def _request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None
    ) -> Dict:
        """Faz requisicao para API do Slack com rate limiting"""
        if not AIOHTTP_AVAILABLE:
            logger.error("aiohttp nao disponivel")
            return {"ok": False, "error": "aiohttp_not_available"}

        # Rate limiting
        if self._rate_limit_remaining <= 0 and self._rate_limit_reset:
            wait_time = (self._rate_limit_reset - datetime.utcnow()).total_seconds()
            if wait_time > 0:
                logger.warning(f"Rate limited, aguardando {wait_time:.1f}s")
                await asyncio.sleep(wait_time)

        session = await self._ensure_session()
        url = f"{self.API_BASE}/{endpoint}"

        try:
            if method.upper() == "GET":
                async with session.get(url, params=params) as response:
                    self._update_rate_limit(response.headers)
                    return await response.json()
            else:
                async with session.post(url, json=data, params=params) as response:
                    self._update_rate_limit(response.headers)
                    return await response.json()
        except Exception as e:
            logger.error(f"Erro na requisicao Slack: {e}")
            return {"ok": False, "error": str(e)}

    def _update_rate_limit(self, headers):
        """Atualiza informacoes de rate limit"""
        if "X-RateLimit-Remaining" in headers:
            self._rate_limit_remaining = int(headers["X-RateLimit-Remaining"])
        if "X-RateLimit-Reset" in headers:
            self._rate_limit_reset = datetime.fromtimestamp(
                int(headers["X-RateLimit-Reset"])
            )

    async def connect(self) -> bool:
        """Testa conexao com Slack"""
        try:
            result = await self._request("GET", "auth.test")
            if result.get("ok"):
                self._bot_info = result
                logger.info(f"Conectado ao Slack como {result.get('user')}")
                return True
            else:
                logger.error(f"Falha ao conectar: {result.get('error')}")
                return False
        except Exception as e:
            logger.error(f"Erro ao conectar ao Slack: {e}")
            return False

    async def disconnect(self):
        """Fecha conexao"""
        if self._session and not self._session.closed:
            await self._session.close()
        self._session = None

    # =========================================================================
    # MENSAGENS
    # =========================================================================

    async def send_message(
        self,
        channel: str,
        text: str,
        blocks: Optional[List[Dict]] = None,
        thread_ts: Optional[str] = None,
        attachments: Optional[List[Dict]] = None,
        unfurl_links: bool = False,
        unfurl_media: bool = True
    ) -> Optional[Dict]:
        """
        Envia mensagem para um canal.

        Args:
            channel: ID ou nome do canal
            text: Texto da mensagem (fallback para blocks)
            blocks: Blocos Block Kit (opcional)
            thread_ts: Timestamp da thread para responder
            attachments: Attachments legados
            unfurl_links: Expandir links
            unfurl_media: Expandir midia

        Returns:
            Dict com resposta ou None se falhar
        """
        data = {
            "channel": channel,
            "text": text,
            "unfurl_links": unfurl_links,
            "unfurl_media": unfurl_media
        }

        if blocks:
            data["blocks"] = blocks
        if thread_ts:
            data["thread_ts"] = thread_ts
        if attachments:
            data["attachments"] = attachments

        result = await self._request("POST", "chat.postMessage", data=data)

        if result.get("ok"):
            return result
        else:
            logger.error(f"Erro ao enviar mensagem: {result.get('error')}")
            return None

    async def update_message(
        self,
        channel: str,
        ts: str,
        text: str,
        blocks: Optional[List[Dict]] = None
    ) -> bool:
        """Atualiza mensagem existente"""
        data = {
            "channel": channel,
            "ts": ts,
            "text": text
        }

        if blocks:
            data["blocks"] = blocks

        result = await self._request("POST", "chat.update", data=data)
        return result.get("ok", False)

    async def delete_message(self, channel: str, ts: str) -> bool:
        """Deleta mensagem"""
        result = await self._request(
            "POST", "chat.delete",
            data={"channel": channel, "ts": ts}
        )
        return result.get("ok", False)

    async def add_reaction(self, channel: str, ts: str, reaction: str) -> bool:
        """Adiciona reacao a mensagem"""
        result = await self._request(
            "POST", "reactions.add",
            data={"channel": channel, "timestamp": ts, "name": reaction}
        )
        return result.get("ok", False)

    async def remove_reaction(self, channel: str, ts: str, reaction: str) -> bool:
        """Remove reacao de mensagem"""
        result = await self._request(
            "POST", "reactions.remove",
            data={"channel": channel, "timestamp": ts, "name": reaction}
        )
        return result.get("ok", False)

    # =========================================================================
    # CANAIS
    # =========================================================================

    async def list_channels(
        self,
        types: str = "public_channel,private_channel",
        limit: int = 100
    ) -> List[Dict]:
        """Lista canais"""
        result = await self._request(
            "GET", "conversations.list",
            params={"types": types, "limit": limit}
        )
        return result.get("channels", []) if result.get("ok") else []

    async def get_channel_info(self, channel: str) -> Optional[Dict]:
        """Busca informacoes de canal"""
        result = await self._request(
            "GET", "conversations.info",
            params={"channel": channel}
        )
        return result.get("channel") if result.get("ok") else None

    async def join_channel(self, channel: str) -> bool:
        """Entra em canal"""
        result = await self._request(
            "POST", "conversations.join",
            data={"channel": channel}
        )
        return result.get("ok", False)

    # =========================================================================
    # USUARIOS
    # =========================================================================

    async def get_user_info(self, user_id: str) -> Optional[SlackUser]:
        """Busca informacoes de usuario"""
        result = await self._request(
            "GET", "users.info",
            params={"user": user_id}
        )

        if result.get("ok"):
            user = result.get("user", {})
            return SlackUser(
                id=user.get("id", ""),
                name=user.get("name", ""),
                real_name=user.get("real_name"),
                email=user.get("profile", {}).get("email"),
                is_bot=user.get("is_bot", False)
            )
        return None

    async def list_users(self, limit: int = 100) -> List[SlackUser]:
        """Lista usuarios do workspace"""
        result = await self._request(
            "GET", "users.list",
            params={"limit": limit}
        )

        users = []
        if result.get("ok"):
            for member in result.get("members", []):
                users.append(SlackUser(
                    id=member.get("id", ""),
                    name=member.get("name", ""),
                    real_name=member.get("real_name"),
                    email=member.get("profile", {}).get("email"),
                    is_bot=member.get("is_bot", False)
                ))
        return users

    # =========================================================================
    # NOTIFICACOES DE STORIES
    # =========================================================================

    async def notify_story_created(
        self,
        story: Dict,
        channel: Optional[str] = None
    ) -> Optional[Dict]:
        """
        Envia notificacao de nova story.

        Args:
            story: Dados da story
            channel: Canal (usa default se nao especificado)

        Returns:
            Resposta do Slack
        """
        target_channel = channel or self.config.default_channel
        if not target_channel:
            logger.warning("Canal nao especificado para notificacao")
            return None

        blocks = self._build_story_blocks(story, "created")
        text = f"Nova Story: {story.get('title', 'Sem titulo')}"

        return await self.send_message(
            channel=target_channel,
            text=text,
            blocks=blocks
        )

    async def notify_story_updated(
        self,
        story: Dict,
        changes: Dict,
        channel: Optional[str] = None
    ) -> Optional[Dict]:
        """Envia notificacao de story atualizada"""
        target_channel = channel or self.config.default_channel
        if not target_channel:
            return None

        # Determinar tipo de mudanca
        if "status" in changes:
            action = "status_changed"
            old_status = changes.get("old_status", "")
            new_status = changes.get("status", "")
            text = f"Story *{story.get('story_id')}* mudou de {old_status} para {new_status}"
        else:
            action = "updated"
            text = f"Story *{story.get('story_id')}* foi atualizada"

        blocks = self._build_story_blocks(story, action, changes)

        return await self.send_message(
            channel=target_channel,
            text=text,
            blocks=blocks
        )

    async def notify_task_completed(
        self,
        task: Dict,
        story: Optional[Dict] = None,
        channel: Optional[str] = None
    ) -> Optional[Dict]:
        """Envia notificacao de task completada"""
        target_channel = channel or self.config.default_channel
        if not target_channel:
            return None

        blocks = [
            {
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": f":white_check_mark: *Task Completada*\n{task.get('title', 'Sem titulo')}"
                }
            }
        ]

        if story:
            blocks.append({
                "type": "context",
                "elements": [
                    {
                        "type": "mrkdwn",
                        "text": f"Story: {story.get('story_id')} - {story.get('title', '')}"
                    }
                ]
            })

        return await self.send_message(
            channel=target_channel,
            text=f"Task completada: {task.get('title')}",
            blocks=blocks
        )

    def _build_story_blocks(
        self,
        story: Dict,
        action: str,
        extra: Optional[Dict] = None
    ) -> List[Dict]:
        """Constroi blocos Block Kit para story"""
        status = story.get("status", "backlog")
        emoji = STATUS_TO_EMOJI.get(status, ":bookmark:")
        priority = story.get("priority", "medium")

        priority_emoji = {
            "urgent": ":rotating_light:",
            "high": ":fire:",
            "medium": ":large_blue_circle:",
            "low": ":white_circle:"
        }.get(priority, ":large_blue_circle:")

        # Header
        if action == "created":
            header_text = ":sparkles: Nova Story Criada"
        elif action == "status_changed":
            header_text = f"{emoji} Status Atualizado"
        else:
            header_text = ":pencil2: Story Atualizada"

        blocks = [
            {
                "type": "header",
                "text": {"type": "plain_text", "text": header_text, "emoji": True}
            },
            {
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": f"*<#{story.get('story_id')}|{story.get('story_id')}>* - {story.get('title', 'Sem titulo')}"
                },
                "accessory": {
                    "type": "button",
                    "text": {"type": "plain_text", "text": "Ver Detalhes", "emoji": True},
                    "action_id": "view_story",
                    "value": story.get("story_id", "")
                }
            },
            {
                "type": "context",
                "elements": [
                    {"type": "mrkdwn", "text": f"{emoji} *Status:* {status}"},
                    {"type": "mrkdwn", "text": f"{priority_emoji} *Prioridade:* {priority}"}
                ]
            }
        ]

        # Pontos se disponivel
        points = story.get("story_points")
        if points:
            blocks.append({
                "type": "context",
                "elements": [
                    {"type": "mrkdwn", "text": f":1234: *Story Points:* {points}"}
                ]
            })

        # Descricao resumida
        description = story.get("description", "")
        if description and len(description) > 200:
            description = description[:200] + "..."
        if description:
            blocks.append({
                "type": "section",
                "text": {"type": "mrkdwn", "text": description}
            })

        # Mudancas especificas
        if extra and action == "status_changed":
            old_status = extra.get("old_status", "")
            new_status = extra.get("status", status)
            old_emoji = STATUS_TO_EMOJI.get(old_status, ":grey_question:")
            new_emoji = STATUS_TO_EMOJI.get(new_status, ":grey_question:")

            blocks.append({
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": f"{old_emoji} {old_status} :arrow_right: {new_emoji} {new_status}"
                }
            })

        blocks.append({"type": "divider"})

        return blocks

    # =========================================================================
    # CRIAR STORY A PARTIR DE MENSAGEM
    # =========================================================================

    def parse_story_from_message(self, message: SlackMessage) -> Optional[Dict]:
        """
        Extrai dados de story a partir de mensagem.

        Formatos suportados:
        - /story Titulo da story
        - /story [EPIC-01] Titulo da story
        - Mensagem com emoji :story: no inicio

        Args:
            message: Mensagem do Slack

        Returns:
            Dict com dados da story ou None
        """
        text = message.text.strip()

        # Verificar prefixos
        if not any([
            text.startswith("/story"),
            text.startswith(":story:"),
            text.startswith(":bookmark:")
        ]):
            return None

        # Remover prefixo
        for prefix in ["/story ", ":story: ", ":bookmark: "]:
            if text.startswith(prefix):
                text = text[len(prefix):].strip()
                break

        if not text:
            return None

        # Extrair epic se presente
        epic_id = None
        if text.startswith("["):
            end = text.find("]")
            if end > 0:
                epic_id = text[1:end]
                text = text[end + 1:].strip()

        # Extrair prioridade se presente
        priority = "medium"
        priority_markers = {
            "!!!!": "urgent",
            "!!!": "urgent",
            "!!": "high",
            "!": "medium"
        }
        for marker, p in priority_markers.items():
            if text.endswith(marker):
                priority = p
                text = text[:-len(marker)].strip()
                break

        return {
            "title": text,
            "epic_id": epic_id,
            "priority": priority,
            "slack_channel": message.channel,
            "slack_ts": message.ts,
            "slack_user": message.user,
            "created_via": "slack"
        }

    # =========================================================================
    # ATUALIZAR STATUS VIA REACAO
    # =========================================================================

    def get_status_from_reaction(self, reaction: str) -> Optional[str]:
        """
        Mapeia reacao para status de story.

        Args:
            reaction: Nome da reacao (sem :)

        Returns:
            Status ou None se nao mapeado
        """
        return REACTION_TO_STATUS.get(reaction)


class SlackEventHandler:
    """
    Handler para eventos e webhooks do Slack.

    Processa:
    - Verificacao de URL
    - Eventos de mensagem
    - Reacoes
    - Slash commands

    Exemplo:
    ```python
    handler = SlackEventHandler(signing_secret="xxx")

    @handler.on("message")
    async def on_message(event):
        print(f"Mensagem: {event['text']}")

    @handler.on("reaction_added")
    async def on_reaction(event):
        print(f"Reacao: {event['reaction']}")
    ```
    """

    def __init__(self, signing_secret: str = ""):
        self.signing_secret = signing_secret
        self._handlers: Dict[str, List[Callable]] = {}

    def on(self, event_type: str) -> Callable:
        """Decorator para registrar handler de evento"""
        def decorator(func: Callable) -> Callable:
            if event_type not in self._handlers:
                self._handlers[event_type] = []
            self._handlers[event_type].append(func)
            return func
        return decorator

    def register(self, event_type: str, handler: Callable):
        """Registra handler programaticamente"""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)

    def verify_signature(
        self,
        body: bytes,
        timestamp: str,
        signature: str
    ) -> bool:
        """
        Verifica assinatura do request.

        Args:
            body: Body do request em bytes
            timestamp: Header X-Slack-Request-Timestamp
            signature: Header X-Slack-Signature

        Returns:
            True se valido
        """
        if not self.signing_secret:
            logger.warning("Signing secret nao configurado")
            return True

        # Verificar timestamp (anti-replay)
        current_time = int(time.time())
        request_time = int(timestamp)
        if abs(current_time - request_time) > 300:
            logger.warning("Timestamp muito antigo")
            return False

        # Calcular assinatura esperada
        sig_basestring = f"v0:{timestamp}:{body.decode()}"
        expected_sig = "v0=" + hmac.new(
            self.signing_secret.encode(),
            sig_basestring.encode(),
            hashlib.sha256
        ).hexdigest()

        return hmac.compare_digest(expected_sig, signature)

    async def process_event(self, payload: Dict) -> Optional[Dict]:
        """
        Processa evento do Slack.

        Args:
            payload: Payload do evento

        Returns:
            Resposta para o Slack
        """
        # URL Verification challenge
        if payload.get("type") == "url_verification":
            return {"challenge": payload.get("challenge")}

        # Processar evento
        event = payload.get("event", {})
        event_type = event.get("type", "")

        if event_type in self._handlers:
            for handler in self._handlers[event_type]:
                try:
                    if asyncio.iscoroutinefunction(handler):
                        await handler(event)
                    else:
                        handler(event)
                except Exception as e:
                    logger.error(f"Erro no handler {handler.__name__}: {e}")

        return {"ok": True}

    async def process_slash_command(self, payload: Dict) -> Dict:
        """
        Processa slash command.

        Args:
            payload: Dados do comando

        Returns:
            Resposta para o Slack
        """
        command = payload.get("command", "")
        handler_key = f"command_{command.lstrip('/')}"

        if handler_key in self._handlers:
            for handler in self._handlers[handler_key]:
                try:
                    if asyncio.iscoroutinefunction(handler):
                        result = await handler(payload)
                    else:
                        result = handler(payload)
                    if result:
                        return result
                except Exception as e:
                    logger.error(f"Erro no handler de comando: {e}")
                    return {
                        "response_type": "ephemeral",
                        "text": f"Erro ao processar comando: {str(e)}"
                    }

        return {
            "response_type": "ephemeral",
            "text": f"Comando {command} nao reconhecido"
        }


class SlackIntegration(IntegrationBase):
    """
    Integracao completa com Slack.

    Fornece:
    - Cliente para API do Slack
    - Handler de eventos e webhooks
    - Notificacoes automaticas
    - Criacao de stories via Slack
    - Atualizacao de status via reacoes
    - Isolamento por tenant

    Exemplo:
    ```python
    config = SlackIntegrationConfig.from_env()
    slack = SlackIntegration(config)

    await slack.connect()

    # Enviar notificacao
    await slack.notify_story_created(story)

    # Processar evento
    await slack.handle_webhook(payload)
    ```
    """

    def __init__(self, config: SlackIntegrationConfig, tenant_id: Optional[str] = None):
        super().__init__(config)
        self.config: SlackIntegrationConfig = config
        self.tenant_id = tenant_id
        self.client = SlackClient(config, tenant_id)
        self.event_handler = SlackEventHandler(config.signing_secret)

        # Registrar handlers padrao
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Registra handlers padrao para eventos"""
        @self.event_handler.on("reaction_added")
        async def on_reaction_added(event: Dict):
            if self.config.update_status_from_reaction:
                await self._handle_reaction(event)

        @self.event_handler.on("app_mention")
        async def on_mention(event: Dict):
            # Responder mencoes
            channel = event.get("channel")
            if channel:
                await self.client.send_message(
                    channel=channel,
                    text="Ola! Use /story para criar uma nova story ou /status para verificar o status.",
                    thread_ts=event.get("ts")
                )

    async def _handle_reaction(self, event: Dict):
        """Processa reacao para atualizar status"""
        reaction = event.get("reaction", "")
        new_status = self.client.get_status_from_reaction(reaction)

        if not new_status:
            return

        # Buscar mensagem original
        item = event.get("item", {})
        channel = item.get("channel")
        ts = item.get("ts")

        logger.info(
            f"Reacao {reaction} detectada no canal {channel}, "
            f"ts={ts}, novo status={new_status}"
        )

        # TODO: Implementar busca e atualizacao de story associada

    async def connect(self) -> bool:
        """Conecta ao Slack"""
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida"
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING

        try:
            if await self.client.connect():
                self.status = IntegrationStatus.CONNECTED
                return True
            else:
                self._last_error = "Falha ao conectar"
                self.status = IntegrationStatus.ERROR
                return False
        except Exception as e:
            self._last_error = str(e)
            self.status = IntegrationStatus.ERROR
            return False

    async def disconnect(self) -> bool:
        """Desconecta do Slack"""
        await self.client.disconnect()
        self.status = IntegrationStatus.DISCONNECTED
        return True

    async def test_connection(self) -> bool:
        """Testa conexao"""
        return await self.client.connect()

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Envia notificacoes de stories para Slack"""
        result = SyncResult(success=True, started_at=datetime.utcnow())

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado")
            return result

        for story in stories:
            try:
                response = await self.client.notify_story_created(story)
                if response:
                    result.items_synced += 1
                else:
                    result.items_failed += 1
            except Exception as e:
                result.items_failed += 1
                result.errors.append(str(e))

        result.completed_at = datetime.utcnow()
        return result

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Slack nao suporta sync de stories (apenas criacao via comandos)"""
        return SyncResult(
            success=True,
            started_at=datetime.utcnow(),
            completed_at=datetime.utcnow(),
            details={"message": "Slack sync nao suportado, use slash commands"}
        )

    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook do Slack"""
        try:
            await self.event_handler.process_event(payload)
            return True
        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        status = super().get_status()
        status.update({
            "system": "slack",
            "team_id": self.config.team_id,
            "team_name": self.config.team_name,
            "default_channel": self.config.default_channel,
            "notify_on_story_create": self.config.notify_on_story_create,
            "notify_on_status_change": self.config.notify_on_status_change,
            "create_story_from_message": self.config.create_story_from_message,
            "update_status_from_reaction": self.config.update_status_from_reaction
        })
        return status


# =============================================================================
# SINGLETON E HELPERS
# =============================================================================

_slack_instances: Dict[str, SlackIntegration] = {}


def get_slack_integration(tenant_id: str = "default") -> SlackIntegration:
    """Retorna instancia do Slack para o tenant"""
    global _slack_instances

    if tenant_id not in _slack_instances:
        config = SlackIntegrationConfig.from_env()
        _slack_instances[tenant_id] = SlackIntegration(config, tenant_id)

    return _slack_instances[tenant_id]


async def init_slack_integration(tenant_id: str = "default") -> Optional[SlackIntegration]:
    """Inicializa e conecta integracao Slack"""
    slack = get_slack_integration(tenant_id)

    if slack.config.is_valid() and slack.config.enabled:
        if await slack.connect():
            return slack

    return None
