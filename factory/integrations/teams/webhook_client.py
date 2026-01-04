# -*- coding: utf-8 -*-
"""
Microsoft Teams Webhook Client
==============================
Cliente para envio de mensagens via Incoming Webhooks do Teams.
Metodo simples e direto para enviar notificacoes sem necessidade de Azure AD.

Documentacao: https://docs.microsoft.com/microsoftteams/platform/webhooks-and-connectors/
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

import aiohttp

logger = logging.getLogger(__name__)


class MessageColor(str, Enum):
    """Cores para mensagens de webhook"""
    SUCCESS = "00FF00"  # Verde
    WARNING = "FFFF00"  # Amarelo
    ERROR = "FF0000"    # Vermelho
    INFO = "0078D7"     # Azul
    DEFAULT = "003B4A"  # Azul Belgo


class ThemeColor(str, Enum):
    """Cores do tema Belgo"""
    PRIMARY = "003B4A"    # Azul Belgo
    SECONDARY = "FF6C00"  # Laranja Belgo
    SUCCESS = "28A745"    # Verde
    WARNING = "FFC107"    # Amarelo
    DANGER = "DC3545"     # Vermelho
    INFO = "17A2B8"       # Azul Info


@dataclass
class WebhookConfig:
    """Configuracao de webhooks do Teams"""
    # URLs dos webhooks por canal
    webhooks: Dict[str, str] = field(default_factory=dict)
    # Webhook padrao
    default_webhook: str = ""
    # Configuracoes de rede
    timeout_seconds: int = 30
    max_retries: int = 3
    retry_delay_seconds: float = 1.0
    # Formatacao
    default_color: str = ThemeColor.PRIMARY.value

    def get_webhook(self, channel: str = None) -> str:
        """Obtem URL do webhook para um canal"""
        if channel and channel in self.webhooks:
            return self.webhooks[channel]
        return self.default_webhook


@dataclass
class WebhookMessage:
    """Mensagem para envio via webhook"""
    title: str
    text: str
    theme_color: str = ThemeColor.PRIMARY.value
    sections: List[Dict[str, Any]] = field(default_factory=list)
    potential_actions: List[Dict[str, Any]] = field(default_factory=list)
    summary: str = ""

    def to_dict(self) -> Dict[str, Any]:
        """Converte para formato MessageCard"""
        card = {
            "@type": "MessageCard",
            "@context": "http://schema.org/extensions",
            "themeColor": self.theme_color,
            "summary": self.summary or self.title,
            "title": self.title,
            "text": self.text
        }

        if self.sections:
            card["sections"] = self.sections

        if self.potential_actions:
            card["potentialAction"] = self.potential_actions

        return card


class WebhookClient:
    """
    Cliente para Incoming Webhooks do Microsoft Teams.

    Permite enviar mensagens e cards para canais do Teams de forma simples,
    sem necessidade de autenticacao OAuth2 ou Azure AD.

    Exemplo:
        config = WebhookConfig(
            webhooks={
                "dev": "https://outlook.office.com/webhook/xxx",
                "alerts": "https://outlook.office.com/webhook/yyy"
            },
            default_webhook="https://outlook.office.com/webhook/xxx"
        )
        client = WebhookClient(config)

        # Mensagem simples
        await client.send_message("Projeto concluido!", channel="dev")

        # Card rico
        await client.send_project_card(
            title="Projeto X",
            status="Concluido",
            files_count=45,
            duration="2 minutos"
        )
    """

    def __init__(self, config: WebhookConfig):
        """
        Inicializa o cliente de webhooks.

        Args:
            config: Configuracao dos webhooks
        """
        self.config = config
        self._session: Optional[aiohttp.ClientSession] = None

    async def _get_session(self) -> aiohttp.ClientSession:
        """Obtem ou cria sessao HTTP"""
        if self._session is None or self._session.closed:
            timeout = aiohttp.ClientTimeout(total=self.config.timeout_seconds)
            self._session = aiohttp.ClientSession(timeout=timeout)
        return self._session

    async def close(self) -> None:
        """Fecha a sessao HTTP"""
        if self._session and not self._session.closed:
            await self._session.close()

    async def _send(
        self,
        webhook_url: str,
        payload: Dict[str, Any]
    ) -> bool:
        """
        Envia payload para webhook.

        Args:
            webhook_url: URL do webhook
            payload: Dados a enviar

        Returns:
            True se enviado com sucesso
        """
        if not webhook_url:
            logger.error("URL do webhook nao configurada")
            return False

        session = await self._get_session()

        for attempt in range(self.config.max_retries):
            try:
                async with session.post(
                    webhook_url,
                    json=payload,
                    headers={"Content-Type": "application/json"}
                ) as response:
                    if response.status == 200:
                        logger.info("Mensagem enviada com sucesso para Teams")
                        return True

                    if response.status == 429:
                        # Rate limiting
                        retry_after = int(response.headers.get("Retry-After", 5))
                        logger.warning(f"Rate limit. Aguardando {retry_after}s...")
                        await asyncio.sleep(retry_after)
                        continue

                    error = await response.text()
                    logger.error(f"Erro ao enviar webhook: {response.status} - {error}")
                    return False

            except asyncio.TimeoutError:
                logger.warning(f"Timeout na tentativa {attempt + 1}/{self.config.max_retries}")
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(self.config.retry_delay_seconds)
                continue

            except Exception as e:
                logger.error(f"Erro ao enviar webhook: {e}")
                return False

        return False

    async def send_message(
        self,
        text: str,
        title: str = "",
        color: str = None,
        channel: str = None
    ) -> bool:
        """
        Envia mensagem simples.

        Args:
            text: Texto da mensagem
            title: Titulo opcional
            color: Cor do tema (hex sem #)
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        webhook_url = self.config.get_webhook(channel)
        payload = {
            "@type": "MessageCard",
            "@context": "http://schema.org/extensions",
            "themeColor": color or self.config.default_color,
            "summary": title or text[:50],
            "text": text
        }

        if title:
            payload["title"] = title

        return await self._send(webhook_url, payload)

    async def send_card(
        self,
        message: WebhookMessage,
        channel: str = None
    ) -> bool:
        """
        Envia MessageCard.

        Args:
            message: Mensagem estruturada
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        webhook_url = self.config.get_webhook(channel)
        return await self._send(webhook_url, message.to_dict())

    async def send_adaptive_card(
        self,
        card: Dict[str, Any],
        channel: str = None
    ) -> bool:
        """
        Envia Adaptive Card.

        Args:
            card: Card Adaptativo em formato JSON
            channel: Canal de destino

        Returns:
            True se enviado com sucesso
        """
        webhook_url = self.config.get_webhook(channel)

        # Encapsula o Adaptive Card no formato esperado pelo webhook
        payload = {
            "type": "message",
            "attachments": [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "contentUrl": None,
                    "content": card
                }
            ]
        }

        return await self._send(webhook_url, payload)

    # =========================================================================
    # Mensagens Pre-formatadas
    # =========================================================================

    async def send_success(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia mensagem de sucesso"""
        return await self.send_message(
            text=f"✅ {message}",
            title=title,
            color=ThemeColor.SUCCESS.value,
            channel=channel
        )

    async def send_warning(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia mensagem de aviso"""
        return await self.send_message(
            text=f"⚠️ {message}",
            title=title,
            color=ThemeColor.WARNING.value,
            channel=channel
        )

    async def send_error(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia mensagem de erro"""
        return await self.send_message(
            text=f"❌ {message}",
            title=title,
            color=ThemeColor.DANGER.value,
            channel=channel
        )

    async def send_info(
        self,
        title: str,
        message: str,
        channel: str = None
    ) -> bool:
        """Envia mensagem informativa"""
        return await self.send_message(
            text=f"ℹ️ {message}",
            title=title,
            color=ThemeColor.INFO.value,
            channel=channel
        )

    # =========================================================================
    # Cards de Projeto
    # =========================================================================

    async def send_project_started(
        self,
        project_name: str,
        project_id: str,
        description: str = "",
        assigned_to: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica inicio de projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            description: Descricao
            assigned_to: Responsavel
            channel: Canal de destino
        """
        sections = [
            {
                "activityTitle": f"🚀 Projeto Iniciado: {project_name}",
                "activitySubtitle": f"ID: {project_id}",
                "facts": [
                    {"name": "Status", "value": "Em Desenvolvimento"},
                    {"name": "Iniciado em", "value": datetime.now().strftime("%d/%m/%Y %H:%M")}
                ]
            }
        ]

        if description:
            sections[0]["facts"].append({"name": "Descricao", "value": description})
        if assigned_to:
            sections[0]["facts"].append({"name": "Responsavel", "value": assigned_to})

        message = WebhookMessage(
            title="Plataforma E",
            text="Um novo projeto foi iniciado",
            theme_color=ThemeColor.PRIMARY.value,
            sections=sections
        )

        return await self.send_card(message, channel)

    async def send_project_completed(
        self,
        project_name: str,
        project_id: str,
        files_count: int,
        duration: str,
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            files_count: Numero de arquivos gerados
            duration: Duracao do desenvolvimento
            url: URL do projeto
            channel: Canal de destino
        """
        sections = [
            {
                "activityTitle": f"✅ Projeto Concluido: {project_name}",
                "activitySubtitle": f"ID: {project_id}",
                "facts": [
                    {"name": "Status", "value": "Concluido"},
                    {"name": "Arquivos Gerados", "value": str(files_count)},
                    {"name": "Duracao", "value": duration},
                    {"name": "Concluido em", "value": datetime.now().strftime("%d/%m/%Y %H:%M")}
                ]
            }
        ]

        potential_actions = []
        if url:
            potential_actions.append({
                "@type": "OpenUri",
                "name": "Ver Projeto",
                "targets": [{"os": "default", "uri": url}]
            })

        message = WebhookMessage(
            title="Plataforma E",
            text="Projeto finalizado com sucesso",
            theme_color=ThemeColor.SUCCESS.value,
            sections=sections,
            potential_actions=potential_actions
        )

        return await self.send_card(message, channel)

    async def send_project_error(
        self,
        project_name: str,
        project_id: str,
        error_message: str,
        step: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica erro em projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            error_message: Mensagem de erro
            step: Etapa onde ocorreu o erro
            channel: Canal de destino
        """
        sections = [
            {
                "activityTitle": f"❌ Erro no Projeto: {project_name}",
                "activitySubtitle": f"ID: {project_id}",
                "facts": [
                    {"name": "Status", "value": "Erro"},
                    {"name": "Erro", "value": error_message},
                    {"name": "Ocorrido em", "value": datetime.now().strftime("%d/%m/%Y %H:%M")}
                ]
            }
        ]

        if step:
            sections[0]["facts"].insert(1, {"name": "Etapa", "value": step})

        message = WebhookMessage(
            title="Plataforma E",
            text="Ocorreu um erro no desenvolvimento",
            theme_color=ThemeColor.DANGER.value,
            sections=sections
        )

        return await self.send_card(message, channel)

    # =========================================================================
    # Cards de Story
    # =========================================================================

    async def send_story_status_change(
        self,
        story_id: str,
        story_title: str,
        old_status: str,
        new_status: str,
        assigned_to: str = "",
        url: str = "",
        channel: str = None
    ) -> bool:
        """
        Notifica mudanca de status de story.

        Args:
            story_id: ID da story
            story_title: Titulo da story
            old_status: Status anterior
            new_status: Novo status
            assigned_to: Responsavel
            url: URL da story
            channel: Canal de destino
        """
        # Define cor baseada no novo status
        status_colors = {
            "backlog": ThemeColor.INFO.value,
            "ready": ThemeColor.PRIMARY.value,
            "in_progress": ThemeColor.SECONDARY.value,
            "review": ThemeColor.WARNING.value,
            "testing": ThemeColor.INFO.value,
            "done": ThemeColor.SUCCESS.value
        }

        color = status_colors.get(new_status.lower(), ThemeColor.PRIMARY.value)

        sections = [
            {
                "activityTitle": f"📋 {story_id}: {story_title}",
                "facts": [
                    {"name": "De", "value": old_status},
                    {"name": "Para", "value": new_status},
                    {"name": "Atualizado em", "value": datetime.now().strftime("%d/%m/%Y %H:%M")}
                ]
            }
        ]

        if assigned_to:
            sections[0]["facts"].append({"name": "Responsavel", "value": assigned_to})

        potential_actions = []
        if url:
            potential_actions.append({
                "@type": "OpenUri",
                "name": "Ver Story",
                "targets": [{"os": "default", "uri": url}]
            })

        message = WebhookMessage(
            title="Story Atualizada",
            text=f"Status alterado para: {new_status}",
            theme_color=color,
            sections=sections,
            potential_actions=potential_actions
        )

        return await self.send_card(message, channel)

    async def send_task_completed(
        self,
        task_id: str,
        task_title: str,
        story_id: str,
        story_title: str,
        files_created: List[str] = None,
        channel: str = None
    ) -> bool:
        """
        Notifica conclusao de tarefa.

        Args:
            task_id: ID da tarefa
            task_title: Titulo da tarefa
            story_id: ID da story
            story_title: Titulo da story
            files_created: Arquivos criados
            channel: Canal de destino
        """
        facts = [
            {"name": "Tarefa", "value": task_title},
            {"name": "Story", "value": f"{story_id}: {story_title}"},
            {"name": "Concluido em", "value": datetime.now().strftime("%d/%m/%Y %H:%M")}
        ]

        if files_created:
            facts.append({
                "name": "Arquivos",
                "value": f"{len(files_created)} arquivos criados"
            })

        sections = [
            {
                "activityTitle": f"✅ Tarefa Concluida: {task_id}",
                "facts": facts
            }
        ]

        message = WebhookMessage(
            title="Tarefa Concluida",
            text=f"A tarefa {task_id} foi finalizada com sucesso",
            theme_color=ThemeColor.SUCCESS.value,
            sections=sections
        )

        return await self.send_card(message, channel)

    # =========================================================================
    # Cards de Sistema
    # =========================================================================

    async def send_daily_summary(
        self,
        date: datetime,
        stories_completed: int,
        stories_in_progress: int,
        tasks_completed: int,
        files_generated: int,
        top_contributors: List[Dict[str, Any]] = None,
        channel: str = None
    ) -> bool:
        """
        Envia resumo diario.

        Args:
            date: Data do resumo
            stories_completed: Stories concluidas
            stories_in_progress: Stories em progresso
            tasks_completed: Tarefas concluidas
            files_generated: Arquivos gerados
            top_contributors: Top contribuidores
            channel: Canal de destino
        """
        facts = [
            {"name": "Stories Concluidas", "value": str(stories_completed)},
            {"name": "Stories em Progresso", "value": str(stories_in_progress)},
            {"name": "Tarefas Concluidas", "value": str(tasks_completed)},
            {"name": "Arquivos Gerados", "value": str(files_generated)}
        ]

        sections = [
            {
                "activityTitle": f"📊 Resumo do Dia - {date.strftime('%d/%m/%Y')}",
                "facts": facts
            }
        ]

        if top_contributors:
            contributors_text = "\n".join([
                f"• {c['name']}: {c['count']} contribuicoes"
                for c in top_contributors[:5]
            ])
            sections.append({
                "activityTitle": "🏆 Top Contribuidores",
                "text": contributors_text
            })

        message = WebhookMessage(
            title="Plataforma E - Resumo Diario",
            text="Confira o resumo das atividades de hoje",
            theme_color=ThemeColor.PRIMARY.value,
            sections=sections
        )

        return await self.send_card(message, channel)
