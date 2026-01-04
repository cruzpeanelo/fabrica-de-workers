# -*- coding: utf-8 -*-
"""
Bot Card Builder
================
Construtor de cards especificos para respostas do bot.
"""

from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

from ..notifications.card_builder import (
    AdaptiveCardBuilder,
    TextSize,
    TextWeight,
    TextColor,
    ContainerStyle,
    ActionStyle
)

logger = logging.getLogger(__name__)


class BotCardBuilder:
    """
    Construtor de cards para respostas do bot do Teams.

    Fornece cards pre-definidos para:
    - Ajuda e comandos disponiveis
    - Status da plataforma
    - Listagem de projetos/stories
    - Confirmacoes de acoes
    - Erros e avisos
    """

    def __init__(self):
        self._builder = AdaptiveCardBuilder()

    # =========================================================================
    # Cards de Ajuda
    # =========================================================================

    def create_help_card(self, commands: List[Dict[str, str]]) -> Dict[str, Any]:
        """
        Cria card de ajuda com comandos disponiveis.

        Args:
            commands: Lista de comandos [{command, description}]

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading("Plataforma E - Comandos")
        self._builder.add_text(
            "Aqui estao os comandos disponiveis:",
            color=TextColor.DARK
        )

        # Formata comandos como tabela
        for cmd in commands:
            self._builder.add_two_columns(
                f"**{cmd['command']}**",
                cmd['description']
            )

        self._builder.add_separator()
        self._builder.add_text(
            "Dica: Digite um comando para executar a acao.",
            size=TextSize.SMALL,
            color=TextColor.LIGHT
        )

        return self._builder.build()

    def create_welcome_card(
        self,
        user_name: str = "",
        quick_actions: List[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Cria card de boas-vindas.

        Args:
            user_name: Nome do usuario
            quick_actions: Acoes rapidas

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        greeting = f"Ola, {user_name}!" if user_name else "Ola!"
        self._builder.add_heading(greeting)
        self._builder.add_text(
            "Sou o assistente da Plataforma E. "
            "Posso ajudar voce a gerenciar projetos, stories e tarefas."
        )

        self._builder.add_text(
            "O que voce gostaria de fazer?",
            weight=TextWeight.BOLDER,
            separator=True
        )

        # Acoes rapidas padrao
        if not quick_actions:
            quick_actions = [
                {"title": "Ver Status", "data": {"command": "status"}},
                {"title": "Listar Projetos", "data": {"command": "listar"}},
                {"title": "Ajuda", "data": {"command": "ajuda"}}
            ]

        for action in quick_actions:
            self._builder.add_action_submit(
                action["title"],
                action["data"],
                ActionStyle.POSITIVE if action["title"] == "Ajuda" else ActionStyle.DEFAULT
            )

        return self._builder.build()

    # =========================================================================
    # Cards de Status
    # =========================================================================

    def create_status_card(
        self,
        status: str,
        stories_count: int,
        tasks_in_progress: int,
        recent_completions: int,
        uptime: str = "",
        version: str = ""
    ) -> Dict[str, Any]:
        """
        Cria card de status da plataforma.

        Args:
            status: Status geral (online, offline, etc)
            stories_count: Total de stories
            tasks_in_progress: Tarefas em andamento
            recent_completions: Conclusoes recentes
            uptime: Tempo online
            version: Versao da plataforma

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading("Status da Plataforma E")
        self._builder.add_status_indicator(status)

        facts = [
            ("Stories Totais", str(stories_count)),
            ("Tarefas em Andamento", str(tasks_in_progress)),
            ("Conclusoes Recentes", str(recent_completions))
        ]

        if uptime:
            facts.append(("Uptime", uptime))
        if version:
            facts.append(("Versao", version))

        self._builder.add_fact_set(facts, separator=True)

        self._builder.add_text(
            datetime.now().strftime("Atualizado em: %d/%m/%Y %H:%M"),
            size=TextSize.SMALL,
            color=TextColor.LIGHT
        )

        return self._builder.build()

    def create_project_status_card(
        self,
        project_id: str,
        project_name: str,
        status: str,
        stories_done: int,
        stories_total: int,
        progress: int,
        last_activity: datetime = None,
        url: str = ""
    ) -> Dict[str, Any]:
        """
        Cria card de status de projeto.

        Args:
            project_id: ID do projeto
            project_name: Nome do projeto
            status: Status atual
            stories_done: Stories concluidas
            stories_total: Total de stories
            progress: Progresso (0-100)
            last_activity: Ultima atividade
            url: URL do projeto

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading(project_name)
        self._builder.add_text(f"ID: {project_id}", size=TextSize.SMALL)
        self._builder.add_status_indicator(status)

        self._builder.add_progress_bar(progress, "Progresso Geral")

        self._builder.add_fact_set([
            ("Stories", f"{stories_done}/{stories_total}"),
            ("Progresso", f"{progress}%")
        ], separator=True)

        if last_activity:
            self._builder.add_text(
                f"Ultima atividade: {last_activity.strftime('%d/%m/%Y %H:%M')}",
                size=TextSize.SMALL,
                color=TextColor.LIGHT
            )

        if url:
            self._builder.add_action_url("Ver Projeto", url)

        return self._builder.build()

    # =========================================================================
    # Cards de Listagem
    # =========================================================================

    def create_project_list_card(
        self,
        projects: List[Dict[str, Any]],
        total_count: int = 0
    ) -> Dict[str, Any]:
        """
        Cria card de listagem de projetos.

        Args:
            projects: Lista de projetos
            total_count: Total de projetos

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading("Projetos")

        if total_count:
            self._builder.add_text(
                f"Mostrando {len(projects)} de {total_count} projetos",
                size=TextSize.SMALL
            )

        for project in projects[:10]:  # Limite de 10
            self._builder.add_container([
                {
                    "type": "TextBlock",
                    "text": f"**{project.get('id', '')}**: {project.get('name', '')}",
                    "wrap": True
                },
                {
                    "type": "TextBlock",
                    "text": f"Status: {project.get('status', 'N/A')} | "
                           f"Stories: {project.get('stories_count', 0)}",
                    "size": "Small",
                    "color": "Dark"
                }
            ], style=ContainerStyle.EMPHASIS, separator=True)

        if len(projects) > 10:
            self._builder.add_text(
                f"... e mais {len(projects) - 10} projetos",
                size=TextSize.SMALL,
                color=TextColor.LIGHT
            )

        return self._builder.build()

    def create_story_list_card(
        self,
        stories: List[Dict[str, Any]],
        project_name: str = ""
    ) -> Dict[str, Any]:
        """
        Cria card de listagem de stories.

        Args:
            stories: Lista de stories
            project_name: Nome do projeto (opcional)

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        title = f"Stories - {project_name}" if project_name else "Stories"
        self._builder.add_heading(title)

        for story in stories[:10]:  # Limite de 10
            status_color = TextColor.ACCENT
            status = story.get("status", "").lower()
            if status == "done":
                status_color = TextColor.GOOD
            elif status == "in_progress":
                status_color = TextColor.WARNING

            self._builder.add_container([
                {
                    "type": "TextBlock",
                    "text": f"**{story.get('id', '')}**: {story.get('title', '')}",
                    "wrap": True
                },
                {
                    "type": "TextBlock",
                    "text": f"● {story.get('status', 'N/A')} | "
                           f"{story.get('story_points', 0)} pts",
                    "size": "Small",
                    "color": status_color.value
                }
            ], separator=True)

        if len(stories) > 10:
            self._builder.add_text(
                f"... e mais {len(stories) - 10} stories",
                size=TextSize.SMALL,
                color=TextColor.LIGHT
            )

        return self._builder.build()

    # =========================================================================
    # Cards de Acao
    # =========================================================================

    def create_confirmation_card(
        self,
        action: str,
        target: str,
        details: List[tuple] = None,
        confirm_data: Dict[str, Any] = None,
        cancel_data: Dict[str, Any] = None
    ) -> Dict[str, Any]:
        """
        Cria card de confirmacao de acao.

        Args:
            action: Acao a confirmar
            target: Alvo da acao
            details: Detalhes
            confirm_data: Dados para confirmar
            cancel_data: Dados para cancelar

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading(f"Confirmar {action}?")
        self._builder.add_text(f"Voce esta prestes a **{action}** {target}.")

        if details:
            self._builder.add_fact_set(details, separator=True)

        self._builder.add_action_submit(
            "Confirmar",
            confirm_data or {"action": "confirm"},
            ActionStyle.POSITIVE
        )
        self._builder.add_action_submit(
            "Cancelar",
            cancel_data or {"action": "cancel"},
            ActionStyle.DESTRUCTIVE
        )

        return self._builder.build()

    def create_success_card(
        self,
        title: str,
        message: str,
        details: List[tuple] = None,
        next_action: Dict[str, Any] = None
    ) -> Dict[str, Any]:
        """
        Cria card de sucesso.

        Args:
            title: Titulo
            message: Mensagem
            details: Detalhes
            next_action: Proxima acao sugerida

        Returns:
            Card Adaptativo
        """
        card = self._builder.create_alert_card(
            alert_type="success",
            title=title,
            message=message,
            details=details
        )

        if next_action:
            self._builder._actions.append({
                "type": "Action.Submit",
                "title": next_action.get("title", "Continuar"),
                "data": next_action.get("data", {})
            })
            card = self._builder.build()

        return card

    def create_error_card(
        self,
        title: str,
        message: str,
        suggestion: str = "",
        retry_data: Dict[str, Any] = None
    ) -> Dict[str, Any]:
        """
        Cria card de erro.

        Args:
            title: Titulo
            message: Mensagem de erro
            suggestion: Sugestao de correcao
            retry_data: Dados para tentar novamente

        Returns:
            Card Adaptativo
        """
        self._builder.clear()

        self._builder.add_heading(f"❌ {title}", color=TextColor.ATTENTION)
        self._builder.add_text(message)

        if suggestion:
            self._builder.add_container([
                {
                    "type": "TextBlock",
                    "text": f"💡 **Sugestao**: {suggestion}",
                    "wrap": True
                }
            ], style=ContainerStyle.EMPHASIS, separator=True)

        if retry_data:
            self._builder.add_action_submit("Tentar Novamente", retry_data)

        self._builder.add_action_submit("Ajuda", {"command": "ajuda"})

        return self._builder.build()

    # =========================================================================
    # Cards de Input
    # =========================================================================

    def create_input_card(
        self,
        title: str,
        fields: List[Dict[str, Any]],
        submit_title: str = "Enviar",
        submit_data: Dict[str, Any] = None
    ) -> Dict[str, Any]:
        """
        Cria card com campos de input.

        Args:
            title: Titulo
            fields: Lista de campos [{id, label, placeholder, required}]
            submit_title: Titulo do botao
            submit_data: Dados adicionais para submit

        Returns:
            Card Adaptativo
        """
        self._builder.clear()
        self._builder.add_heading(title)

        body = self._builder._body.copy()

        for field in fields:
            body.append({
                "type": "TextBlock",
                "text": field.get("label", ""),
                "weight": "Bolder"
            })
            body.append({
                "type": "Input.Text",
                "id": field.get("id", ""),
                "placeholder": field.get("placeholder", ""),
                "isRequired": field.get("required", False)
            })

        card = {
            "type": "AdaptiveCard",
            "$schema": self._builder.schema,
            "version": self._builder.version,
            "body": body,
            "actions": [
                {
                    "type": "Action.Submit",
                    "title": submit_title,
                    "data": submit_data or {},
                    "style": "positive"
                }
            ]
        }

        return card

    def create_choice_card(
        self,
        title: str,
        message: str,
        choices: List[Dict[str, str]],
        choice_id: str = "choice"
    ) -> Dict[str, Any]:
        """
        Cria card com selecao de opcoes.

        Args:
            title: Titulo
            message: Mensagem
            choices: Lista de opcoes [{title, value}]
            choice_id: ID do campo de selecao

        Returns:
            Card Adaptativo
        """
        self._builder.clear()
        self._builder.add_heading(title)
        self._builder.add_text(message)

        body = self._builder._body.copy()

        body.append({
            "type": "Input.ChoiceSet",
            "id": choice_id,
            "style": "expanded",
            "choices": [
                {"title": c["title"], "value": c["value"]}
                for c in choices
            ]
        })

        card = {
            "type": "AdaptiveCard",
            "$schema": self._builder.schema,
            "version": self._builder.version,
            "body": body,
            "actions": [
                {
                    "type": "Action.Submit",
                    "title": "Selecionar",
                    "style": "positive"
                }
            ]
        }

        return card
