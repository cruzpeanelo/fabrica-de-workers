# -*- coding: utf-8 -*-
"""
Canal de Microsoft Teams (Webhook)
Fabrica de Agentes v6.0

Implementa envio de notificacoes para Microsoft Teams usando:
- Webhooks Incoming (Adaptive Cards)

Suporta:
- Adaptive Cards com layout rico
- Botoes e acoes
- Cores por prioridade
- Imagens e icones
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


class TeamsChannel(BaseChannel):
    """
    Canal de notificacao via Microsoft Teams.

    Configuracao necessaria:
    - webhook_url: URL do Incoming Webhook do Teams

    Configuracao opcional:
    - theme_color: Cor padrao (hex sem #)
    - summary_max_length: Tamanho max do resumo
    """

    def _get_channel_type(self) -> str:
        return "teams"

    def _get_required_config_keys(self) -> List[str]:
        return ["webhook_url"]

    async def send(self, message: NotificationMessage) -> ChannelResponse:
        """
        Envia mensagem para Teams via webhook.

        Args:
            message: Mensagem de notificacao

        Returns:
            ChannelResponse com resultado do envio
        """
        start_time = time.time()

        try:
            # Formatar payload (Adaptive Card)
            payload = self.format_message(message)

            # Enviar para webhook
            response = await self._send_webhook(payload)

            latency_ms = int((time.time() - start_time) * 1000)

            if response["success"]:
                logger.info(f"Mensagem Teams enviada: {message.notification_id}")
                return ChannelResponse(
                    success=True,
                    channel_type=self.channel_type,
                    notification_id=message.notification_id,
                    provider_id=f"teams-{message.notification_id}",
                    raw_response=response,
                    latency_ms=latency_ms
                )
            else:
                logger.error(f"Falha ao enviar Teams: {response.get('error')}")
                return ChannelResponse(
                    success=False,
                    channel_type=self.channel_type,
                    notification_id=message.notification_id,
                    error_message=response.get("error", "Erro desconhecido"),
                    error_code=response.get("error_code", "TEAMS_ERROR"),
                    raw_response=response,
                    latency_ms=latency_ms
                )

        except Exception as e:
            logger.error(f"Erro ao enviar Teams: {e}")
            return ChannelResponse(
                success=False,
                channel_type=self.channel_type,
                notification_id=message.notification_id,
                error_message=str(e),
                error_code="UNKNOWN_ERROR",
                latency_ms=int((time.time() - start_time) * 1000)
            )

    async def test_connection(self) -> bool:
        """Testa conexao com Teams enviando mensagem de teste"""
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
            logger.error(f"Falha no teste de conexao Teams: {e}")
            return False

    def format_message(self, message: NotificationMessage) -> Dict[str, Any]:
        """
        Converte NotificationMessage para formato Adaptive Card do Teams.

        Args:
            message: Mensagem padronizada

        Returns:
            Payload no formato Adaptive Card
        """
        emoji = self.get_event_emoji(message.event_type)
        priority_color = self.get_priority_color(message.priority).replace("#", "")

        # Construir corpo do Adaptive Card
        body = []

        # Header com titulo
        body.append({
            "type": "TextBlock",
            "size": "Large",
            "weight": "Bolder",
            "text": f"{emoji} {message.subject}",
            "wrap": True
        })

        # Linha de separacao
        body.append({
            "type": "TextBlock",
            "text": "",
            "separator": True
        })

        # Corpo da mensagem
        body.append({
            "type": "TextBlock",
            "text": message.body,
            "wrap": True
        })

        # Dados adicionais em FactSet
        if message.data:
            facts = []
            for key, value in list(message.data.items())[:8]:  # Max 8 fatos
                if key not in ["_internal", "raw"]:
                    label = key.replace("_", " ").title()
                    facts.append({
                        "title": f"{label}:",
                        "value": str(value)
                    })

            if facts:
                body.append({
                    "type": "FactSet",
                    "facts": facts
                })

        # Timestamp no rodape
        body.append({
            "type": "TextBlock",
            "text": f"Fabrica de Agentes | {message.event_type} | {message.timestamp.strftime('%d/%m/%Y %H:%M')}",
            "wrap": True,
            "size": "Small",
            "color": "Light",
            "separator": True
        })

        # Acoes (botoes)
        actions = []
        if message.actions:
            for action in message.actions[:5]:  # Max 5 botoes
                if "url" in action:
                    actions.append({
                        "type": "Action.OpenUrl",
                        "title": action.get("text", "Clique aqui"),
                        "url": action["url"]
                    })

        # Montar Adaptive Card
        adaptive_card = {
            "type": "AdaptiveCard",
            "body": body,
            "$schema": "http://adaptivecards.io/schemas/adaptive-card.json",
            "version": "1.4"
        }

        if actions:
            adaptive_card["actions"] = actions

        # Payload final para Teams (Message Card wrapper)
        # O Teams aceita tanto MessageCard quanto AdaptiveCard
        # Usamos o formato de attachment para Adaptive Cards
        payload = {
            "type": "message",
            "attachments": [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "contentUrl": None,
                    "content": adaptive_card
                }
            ]
        }

        return payload

    def format_message_card(self, message: NotificationMessage) -> Dict[str, Any]:
        """
        Formato alternativo usando MessageCard (mais simples).
        Mantido para compatibilidade com conectores antigos.

        Args:
            message: Mensagem padronizada

        Returns:
            Payload no formato MessageCard (O365 Connector)
        """
        emoji = self.get_event_emoji(message.event_type)
        priority_color = self.get_priority_color(message.priority).replace("#", "")

        # Secoes
        sections = []

        # Secao principal
        main_section = {
            "activityTitle": f"{emoji} {message.subject}",
            "activitySubtitle": f"Fabrica de Agentes - {message.event_type}",
            "text": message.body,
            "markdown": True
        }

        # Fatos adicionais
        if message.data:
            facts = []
            for key, value in list(message.data.items())[:6]:
                if key not in ["_internal", "raw"]:
                    label = key.replace("_", " ").title()
                    facts.append({
                        "name": label,
                        "value": str(value)
                    })
            if facts:
                main_section["facts"] = facts

        sections.append(main_section)

        # Acoes potenciais
        potential_actions = []
        if message.actions:
            for action in message.actions[:4]:
                if "url" in action:
                    potential_actions.append({
                        "@type": "OpenUri",
                        "name": action.get("text", "Clique aqui"),
                        "targets": [
                            {"os": "default", "uri": action["url"]}
                        ]
                    })

        # Montar MessageCard
        payload = {
            "@type": "MessageCard",
            "@context": "http://schema.org/extensions",
            "themeColor": priority_color,
            "summary": self.truncate_text(message.subject, 100),
            "sections": sections
        }

        if potential_actions:
            payload["potentialAction"] = potential_actions

        return payload

    async def _send_webhook(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        """
        Envia payload para webhook do Teams.

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

                # Teams retorna "1" quando sucesso
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

    def create_hero_card(
        self,
        title: str,
        subtitle: str,
        text: str,
        image_url: Optional[str] = None,
        buttons: Optional[List[Dict[str, str]]] = None
    ) -> Dict[str, Any]:
        """
        Cria um Hero Card para mensagens mais visuais.

        Args:
            title: Titulo principal
            subtitle: Subtitulo
            text: Texto do corpo
            image_url: URL de imagem (opcional)
            buttons: Lista de botoes [{text, url}]

        Returns:
            Payload de Hero Card
        """
        body = [
            {
                "type": "TextBlock",
                "size": "Large",
                "weight": "Bolder",
                "text": title
            },
            {
                "type": "TextBlock",
                "size": "Medium",
                "text": subtitle,
                "isSubtle": True
            }
        ]

        if image_url:
            body.append({
                "type": "Image",
                "url": image_url,
                "size": "Large"
            })

        body.append({
            "type": "TextBlock",
            "text": text,
            "wrap": True
        })

        actions = []
        if buttons:
            for btn in buttons[:5]:
                actions.append({
                    "type": "Action.OpenUrl",
                    "title": btn.get("text", "Clique"),
                    "url": btn.get("url", "#")
                })

        card = {
            "type": "AdaptiveCard",
            "$schema": "http://adaptivecards.io/schemas/adaptive-card.json",
            "version": "1.4",
            "body": body
        }

        if actions:
            card["actions"] = actions

        return {
            "type": "message",
            "attachments": [{
                "contentType": "application/vnd.microsoft.card.adaptive",
                "content": card
            }]
        }
