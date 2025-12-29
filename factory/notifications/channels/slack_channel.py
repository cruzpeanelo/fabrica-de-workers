# -*- coding: utf-8 -*-
"""
Canal de Slack (Webhook/API)
Fabrica de Agentes v6.0

Implementa envio de notificacoes para Slack usando:
- Webhooks Incoming
- Slack API com Bot Token (opcional)

Suporta:
- Mensagens formatadas com Blocks
- Anexos (attachments)
- Acoes interativas
"""

import time
import logging
import json
from typing import Dict, List, Any, Optional
from datetime import datetime

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    import urllib.request
    import urllib.error

from .base_channel import BaseChannel, NotificationMessage, ChannelResponse, MessageFormat

logger = logging.getLogger(__name__)


class SlackChannel(BaseChannel):
    """
    Canal de notificacao via Slack.

    Configuracao necessaria (webhook):
    - webhook_url: URL do Incoming Webhook

    Configuracao opcional:
    - channel: Canal padrao (#geral)
    - username: Nome do bot
    - icon_emoji: Emoji do bot (:robot_face:)
    - bot_token: Token do bot (para API completa)
    """

    def _get_channel_type(self) -> str:
        return "slack"

    def _get_required_config_keys(self) -> List[str]:
        return ["webhook_url"]

    async def send(self, message: NotificationMessage) -> ChannelResponse:
        """
        Envia mensagem para Slack via webhook.

        Args:
            message: Mensagem de notificacao

        Returns:
            ChannelResponse com resultado do envio
        """
        start_time = time.time()

        try:
            # Formatar payload
            payload = self.format_message(message)

            # Enviar para webhook
            response = await self._send_webhook(payload)

            latency_ms = int((time.time() - start_time) * 1000)

            if response["success"]:
                logger.info(f"Mensagem Slack enviada: {message.notification_id}")
                return ChannelResponse(
                    success=True,
                    channel_type=self.channel_type,
                    notification_id=message.notification_id,
                    provider_id=f"slack-{message.notification_id}",
                    raw_response=response,
                    latency_ms=latency_ms
                )
            else:
                logger.error(f"Falha ao enviar Slack: {response.get('error')}")
                return ChannelResponse(
                    success=False,
                    channel_type=self.channel_type,
                    notification_id=message.notification_id,
                    error_message=response.get("error", "Erro desconhecido"),
                    error_code=response.get("error_code", "SLACK_ERROR"),
                    raw_response=response,
                    latency_ms=latency_ms
                )

        except Exception as e:
            logger.error(f"Erro ao enviar Slack: {e}")
            return ChannelResponse(
                success=False,
                channel_type=self.channel_type,
                notification_id=message.notification_id,
                error_message=str(e),
                error_code="UNKNOWN_ERROR",
                latency_ms=int((time.time() - start_time) * 1000)
            )

    async def test_connection(self) -> bool:
        """Testa conexao com Slack enviando mensagem de teste"""
        try:
            test_message = NotificationMessage(
                notification_id="test-connection",
                event_type="test",
                subject="Teste de Conexao",
                body="Conexao com Fabrica de Agentes testada com sucesso!",
                recipients=[]
            )

            response = await self.send(test_message)
            return response.success

        except Exception as e:
            logger.error(f"Falha no teste de conexao Slack: {e}")
            return False

    def format_message(self, message: NotificationMessage) -> Dict[str, Any]:
        """
        Converte NotificationMessage para formato Slack (Blocks).

        Args:
            message: Mensagem padronizada

        Returns:
            Payload no formato Slack
        """
        emoji = self.get_event_emoji(message.event_type)
        priority_color = self.get_priority_color(message.priority)

        # Blocos principais
        blocks = []

        # Header
        blocks.append({
            "type": "header",
            "text": {
                "type": "plain_text",
                "text": f"{emoji} {message.subject}",
                "emoji": True
            }
        })

        # Divider
        blocks.append({"type": "divider"})

        # Corpo da mensagem
        blocks.append({
            "type": "section",
            "text": {
                "type": "mrkdwn",
                "text": message.body
            }
        })

        # Dados adicionais em campos
        if message.data:
            fields = []
            for key, value in list(message.data.items())[:10]:  # Max 10 campos
                if key not in ["_internal", "raw"]:
                    label = key.replace("_", " ").title()
                    fields.append({
                        "type": "mrkdwn",
                        "text": f"*{label}:*\n{value}"
                    })

            if fields:
                # Slack permite max 10 campos, 2 por linha
                for i in range(0, len(fields), 2):
                    block_fields = fields[i:i+2]
                    blocks.append({
                        "type": "section",
                        "fields": block_fields
                    })

        # Acoes (botoes)
        if message.actions:
            action_elements = []
            for action in message.actions[:5]:  # Max 5 botoes
                if "url" in action:
                    action_elements.append({
                        "type": "button",
                        "text": {
                            "type": "plain_text",
                            "text": action.get("text", "Clique aqui"),
                            "emoji": True
                        },
                        "url": action["url"],
                        "style": "primary" if action.get("style") == "primary" else None
                    })

            if action_elements:
                # Remover Nones do style
                for elem in action_elements:
                    if elem.get("style") is None:
                        del elem["style"]

                blocks.append({
                    "type": "actions",
                    "elements": action_elements
                })

        # Divider
        blocks.append({"type": "divider"})

        # Contexto (rodape)
        blocks.append({
            "type": "context",
            "elements": [
                {
                    "type": "mrkdwn",
                    "text": f":factory: Fabrica de Agentes | `{message.event_type}` | {message.timestamp.strftime('%d/%m/%Y %H:%M')}"
                }
            ]
        })

        # Montar payload
        payload = {
            "blocks": blocks,
            "text": f"{emoji} {message.subject}",  # Fallback para notificacoes
        }

        # Attachments para cor de prioridade
        payload["attachments"] = [{
            "color": priority_color,
            "blocks": []  # Vazio, so pra cor aparecer
        }]

        # Configuracoes opcionais
        if self.config.get("channel"):
            payload["channel"] = self.config["channel"]

        if self.config.get("username"):
            payload["username"] = self.config["username"]

        if self.config.get("icon_emoji"):
            payload["icon_emoji"] = self.config["icon_emoji"]

        return payload

    async def _send_webhook(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        """
        Envia payload para webhook do Slack.

        Args:
            payload: Dados a enviar

        Returns:
            Resposta do webhook
        """
        webhook_url = self.config["webhook_url"]
        json_payload = json.dumps(payload)

        if AIOHTTP_AVAILABLE:
            return await self._send_with_aiohttp(webhook_url, json_payload)
        else:
            return self._send_with_urllib(webhook_url, json_payload)

    async def _send_with_aiohttp(self, url: str, payload: str) -> Dict[str, Any]:
        """Envia usando aiohttp (async)"""
        async with aiohttp.ClientSession() as session:
            async with session.post(
                url,
                data=payload,
                headers={"Content-Type": "application/json"}
            ) as response:
                text = await response.text()

                if response.status == 200:
                    return {"success": True, "response": text}
                else:
                    return {
                        "success": False,
                        "error": text,
                        "error_code": f"HTTP_{response.status}"
                    }

    def _send_with_urllib(self, url: str, payload: str) -> Dict[str, Any]:
        """Envia usando urllib (sync fallback)"""
        try:
            req = urllib.request.Request(
                url,
                data=payload.encode("utf-8"),
                headers={"Content-Type": "application/json"}
            )

            with urllib.request.urlopen(req, timeout=30) as response:
                text = response.read().decode("utf-8")
                return {"success": True, "response": text}

        except urllib.error.HTTPError as e:
            return {
                "success": False,
                "error": e.read().decode("utf-8"),
                "error_code": f"HTTP_{e.code}"
            }
        except urllib.error.URLError as e:
            return {
                "success": False,
                "error": str(e.reason),
                "error_code": "URL_ERROR"
            }

    def create_simple_message(
        self,
        text: str,
        channel: Optional[str] = None,
        emoji: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria uma mensagem simples (sem blocks).

        Args:
            text: Texto da mensagem
            channel: Canal de destino
            emoji: Emoji do bot

        Returns:
            Payload simples
        """
        payload = {"text": text}

        if channel:
            payload["channel"] = channel
        if emoji:
            payload["icon_emoji"] = emoji

        return payload
