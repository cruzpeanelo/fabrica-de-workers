# -*- coding: utf-8 -*-
"""
Adaptive Card Builder
=====================
Construtor de Cards Adaptativos para Microsoft Teams.
Permite criar cards ricos e interativos de forma programatica.

Documentacao: https://adaptivecards.io/
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union
import logging

logger = logging.getLogger(__name__)


class CardType(str, Enum):
    """Tipos de card pre-definidos"""
    PROJECT_STARTED = "project_started"
    PROJECT_COMPLETED = "project_completed"
    PROJECT_ERROR = "project_error"
    STORY_UPDATE = "story_update"
    TASK_COMPLETED = "task_completed"
    DAILY_SUMMARY = "daily_summary"
    ALERT = "alert"
    NOTIFICATION = "notification"
    APPROVAL_REQUEST = "approval_request"


class TextWeight(str, Enum):
    """Peso do texto"""
    DEFAULT = "Default"
    LIGHTER = "Lighter"
    BOLDER = "Bolder"


class TextSize(str, Enum):
    """Tamanho do texto"""
    DEFAULT = "Default"
    SMALL = "Small"
    MEDIUM = "Medium"
    LARGE = "Large"
    EXTRA_LARGE = "ExtraLarge"


class TextColor(str, Enum):
    """Cores do texto"""
    DEFAULT = "Default"
    DARK = "Dark"
    LIGHT = "Light"
    ACCENT = "Accent"
    GOOD = "Good"
    WARNING = "Warning"
    ATTENTION = "Attention"


class ContainerStyle(str, Enum):
    """Estilos de container"""
    DEFAULT = "default"
    EMPHASIS = "emphasis"
    GOOD = "good"
    ATTENTION = "attention"
    WARNING = "warning"
    ACCENT = "accent"


class ActionStyle(str, Enum):
    """Estilos de acao"""
    DEFAULT = "default"
    POSITIVE = "positive"
    DESTRUCTIVE = "destructive"


@dataclass
class AdaptiveCardBuilder:
    """
    Construtor de Cards Adaptativos para Microsoft Teams.

    Permite criar cards ricos com textos, imagens, fatos, acoes
    e containers de forma fluente e programatica.

    Exemplo:
        builder = AdaptiveCardBuilder()
        card = (builder
            .set_version("1.4")
            .add_heading("Projeto Concluido")
            .add_text("O projeto foi finalizado com sucesso")
            .add_fact_set([
                ("Status", "Concluido"),
                ("Arquivos", "45"),
                ("Duracao", "2 minutos")
            ])
            .add_action_url("Ver Projeto", "https://app.com/project/123")
            .build())
    """

    version: str = "1.4"
    schema: str = "http://adaptivecards.io/schemas/adaptive-card.json"
    _body: List[Dict[str, Any]] = field(default_factory=list)
    _actions: List[Dict[str, Any]] = field(default_factory=list)
    _fallback_text: str = ""

    def set_version(self, version: str) -> 'AdaptiveCardBuilder':
        """Define versao do card"""
        self.version = version
        return self

    def set_fallback_text(self, text: str) -> 'AdaptiveCardBuilder':
        """Define texto de fallback"""
        self._fallback_text = text
        return self

    def clear(self) -> 'AdaptiveCardBuilder':
        """Limpa o builder"""
        self._body = []
        self._actions = []
        self._fallback_text = ""
        return self

    # =========================================================================
    # Elementos de Texto
    # =========================================================================

    def add_text(
        self,
        text: str,
        size: TextSize = TextSize.DEFAULT,
        weight: TextWeight = TextWeight.DEFAULT,
        color: TextColor = TextColor.DEFAULT,
        wrap: bool = True,
        separator: bool = False
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona bloco de texto.

        Args:
            text: Texto a exibir
            size: Tamanho do texto
            weight: Peso do texto
            color: Cor do texto
            wrap: Se deve quebrar linha
            separator: Se deve adicionar separador antes
        """
        element = {
            "type": "TextBlock",
            "text": text,
            "wrap": wrap
        }

        if size != TextSize.DEFAULT:
            element["size"] = size.value
        if weight != TextWeight.DEFAULT:
            element["weight"] = weight.value
        if color != TextColor.DEFAULT:
            element["color"] = color.value
        if separator:
            element["separator"] = True

        self._body.append(element)
        return self

    def add_heading(
        self,
        text: str,
        size: TextSize = TextSize.LARGE,
        color: TextColor = TextColor.DEFAULT
    ) -> 'AdaptiveCardBuilder':
        """Adiciona titulo"""
        return self.add_text(
            text,
            size=size,
            weight=TextWeight.BOLDER,
            color=color
        )

    def add_subheading(
        self,
        text: str,
        color: TextColor = TextColor.DEFAULT
    ) -> 'AdaptiveCardBuilder':
        """Adiciona subtitulo"""
        return self.add_text(
            text,
            size=TextSize.MEDIUM,
            weight=TextWeight.BOLDER,
            color=color
        )

    # =========================================================================
    # Fatos e Dados
    # =========================================================================

    def add_fact_set(
        self,
        facts: List[tuple],
        separator: bool = False
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona conjunto de fatos (pares titulo/valor).

        Args:
            facts: Lista de tuplas (titulo, valor)
            separator: Se deve adicionar separador antes
        """
        element = {
            "type": "FactSet",
            "facts": [
                {"title": title, "value": str(value)}
                for title, value in facts
            ]
        }

        if separator:
            element["separator"] = True

        self._body.append(element)
        return self

    def add_column_set(
        self,
        columns: List[Dict[str, Any]],
        separator: bool = False
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona conjunto de colunas.

        Args:
            columns: Lista de definicoes de coluna
            separator: Se deve adicionar separador antes
        """
        element = {
            "type": "ColumnSet",
            "columns": columns
        }

        if separator:
            element["separator"] = True

        self._body.append(element)
        return self

    def add_two_columns(
        self,
        left_content: str,
        right_content: str,
        left_weight: int = 1,
        right_weight: int = 1
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona duas colunas simples.

        Args:
            left_content: Conteudo da coluna esquerda
            right_content: Conteudo da coluna direita
            left_weight: Peso da coluna esquerda
            right_weight: Peso da coluna direita
        """
        columns = [
            {
                "type": "Column",
                "width": left_weight,
                "items": [
                    {"type": "TextBlock", "text": left_content, "wrap": True}
                ]
            },
            {
                "type": "Column",
                "width": right_weight,
                "items": [
                    {"type": "TextBlock", "text": right_content, "wrap": True}
                ]
            }
        ]
        return self.add_column_set(columns)

    # =========================================================================
    # Imagens e Midia
    # =========================================================================

    def add_image(
        self,
        url: str,
        alt_text: str = "",
        size: str = "auto",
        horizontal_alignment: str = "center",
        separator: bool = False
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona imagem.

        Args:
            url: URL da imagem
            alt_text: Texto alternativo
            size: Tamanho (auto, stretch, small, medium, large)
            horizontal_alignment: Alinhamento (left, center, right)
            separator: Se deve adicionar separador antes
        """
        element = {
            "type": "Image",
            "url": url,
            "size": size,
            "horizontalAlignment": horizontal_alignment
        }

        if alt_text:
            element["altText"] = alt_text
        if separator:
            element["separator"] = True

        self._body.append(element)
        return self

    def add_icon_with_text(
        self,
        icon_url: str,
        text: str,
        icon_size: str = "small"
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona icone com texto ao lado.

        Args:
            icon_url: URL do icone
            text: Texto ao lado
            icon_size: Tamanho do icone
        """
        columns = [
            {
                "type": "Column",
                "width": "auto",
                "items": [
                    {
                        "type": "Image",
                        "url": icon_url,
                        "size": icon_size
                    }
                ]
            },
            {
                "type": "Column",
                "width": "stretch",
                "items": [
                    {"type": "TextBlock", "text": text, "wrap": True}
                ],
                "verticalContentAlignment": "center"
            }
        ]
        return self.add_column_set(columns)

    # =========================================================================
    # Containers
    # =========================================================================

    def add_container(
        self,
        items: List[Dict[str, Any]],
        style: ContainerStyle = ContainerStyle.DEFAULT,
        separator: bool = False
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona container.

        Args:
            items: Itens do container
            style: Estilo do container
            separator: Se deve adicionar separador antes
        """
        element = {
            "type": "Container",
            "items": items
        }

        if style != ContainerStyle.DEFAULT:
            element["style"] = style.value
        if separator:
            element["separator"] = True

        self._body.append(element)
        return self

    def add_separator(self) -> 'AdaptiveCardBuilder':
        """Adiciona separador visual"""
        self._body.append({
            "type": "TextBlock",
            "text": "",
            "separator": True
        })
        return self

    # =========================================================================
    # Progresso e Status
    # =========================================================================

    def add_progress_bar(
        self,
        progress: int,
        label: str = "",
        show_percentage: bool = True
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona barra de progresso visual.

        Args:
            progress: Valor de 0 a 100
            label: Label opcional
            show_percentage: Mostrar porcentagem
        """
        # Calcula blocos preenchidos (10 blocos no total)
        filled = min(10, max(0, progress // 10))
        empty = 10 - filled

        progress_text = "█" * filled + "░" * empty
        if show_percentage:
            progress_text += f" {progress}%"

        if label:
            self.add_text(label, size=TextSize.SMALL)

        return self.add_text(
            progress_text,
            size=TextSize.MEDIUM,
            color=TextColor.ACCENT
        )

    def add_status_indicator(
        self,
        status: str,
        color: TextColor = None
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona indicador de status.

        Args:
            status: Texto do status
            color: Cor do indicador
        """
        # Auto-detecta cor baseado no status
        if color is None:
            status_lower = status.lower()
            if status_lower in ["done", "completed", "success", "concluido"]:
                color = TextColor.GOOD
            elif status_lower in ["error", "failed", "erro", "falha"]:
                color = TextColor.ATTENTION
            elif status_lower in ["warning", "pending", "aviso", "pendente"]:
                color = TextColor.WARNING
            else:
                color = TextColor.ACCENT

        return self.add_text(
            f"● {status}",
            weight=TextWeight.BOLDER,
            color=color
        )

    # =========================================================================
    # Acoes
    # =========================================================================

    def add_action_url(
        self,
        title: str,
        url: str,
        style: ActionStyle = ActionStyle.DEFAULT
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona acao de abrir URL.

        Args:
            title: Titulo do botao
            url: URL a abrir
            style: Estilo do botao
        """
        action = {
            "type": "Action.OpenUrl",
            "title": title,
            "url": url
        }

        if style != ActionStyle.DEFAULT:
            action["style"] = style.value

        self._actions.append(action)
        return self

    def add_action_submit(
        self,
        title: str,
        data: Dict[str, Any],
        style: ActionStyle = ActionStyle.DEFAULT
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona acao de submit.

        Args:
            title: Titulo do botao
            data: Dados a enviar
            style: Estilo do botao
        """
        action = {
            "type": "Action.Submit",
            "title": title,
            "data": data
        }

        if style != ActionStyle.DEFAULT:
            action["style"] = style.value

        self._actions.append(action)
        return self

    def add_action_show_card(
        self,
        title: str,
        card_body: List[Dict[str, Any]],
        card_actions: List[Dict[str, Any]] = None
    ) -> 'AdaptiveCardBuilder':
        """
        Adiciona acao de mostrar card.

        Args:
            title: Titulo do botao
            card_body: Corpo do card interno
            card_actions: Acoes do card interno
        """
        action = {
            "type": "Action.ShowCard",
            "title": title,
            "card": {
                "type": "AdaptiveCard",
                "body": card_body
            }
        }

        if card_actions:
            action["card"]["actions"] = card_actions

        self._actions.append(action)
        return self

    # =========================================================================
    # Cards Pre-definidos
    # =========================================================================

    def create_project_card(
        self,
        project_name: str,
        project_id: str,
        status: str,
        description: str = "",
        facts: List[tuple] = None,
        url: str = ""
    ) -> Dict[str, Any]:
        """
        Cria card de projeto.

        Args:
            project_name: Nome do projeto
            project_id: ID do projeto
            status: Status atual
            description: Descricao
            facts: Fatos adicionais
            url: URL do projeto

        Returns:
            Card Adaptativo em formato JSON
        """
        self.clear()

        # Determina cor do status
        status_color = TextColor.ACCENT
        if status.lower() in ["done", "completed", "concluido"]:
            status_color = TextColor.GOOD
        elif status.lower() in ["error", "erro"]:
            status_color = TextColor.ATTENTION

        # Header
        self.add_heading(project_name)
        self.add_text(f"ID: {project_id}", size=TextSize.SMALL, color=TextColor.DARK)

        # Status
        self.add_status_indicator(status, status_color)

        # Descricao
        if description:
            self.add_text(description, separator=True)

        # Fatos
        if facts:
            self.add_fact_set(facts, separator=True)

        # Acao
        if url:
            self.add_action_url("Ver Projeto", url, ActionStyle.POSITIVE)

        return self.build()

    def create_story_card(
        self,
        story_id: str,
        title: str,
        persona: str,
        action: str,
        benefit: str,
        status: str,
        progress: int = 0,
        story_points: int = 0,
        url: str = ""
    ) -> Dict[str, Any]:
        """
        Cria card de User Story.

        Args:
            story_id: ID da story
            title: Titulo
            persona: "Como um..."
            action: "Eu quero..."
            benefit: "Para que..."
            status: Status atual
            progress: Progresso (0-100)
            story_points: Story points
            url: URL da story

        Returns:
            Card Adaptativo em formato JSON
        """
        self.clear()

        # Header
        self.add_heading(f"{story_id}: {title}")
        self.add_status_indicator(status)

        # Narrativa
        self.add_container([
            {"type": "TextBlock", "text": f"**Como** {persona}", "wrap": True},
            {"type": "TextBlock", "text": f"**Eu quero** {action}", "wrap": True},
            {"type": "TextBlock", "text": f"**Para que** {benefit}", "wrap": True}
        ], style=ContainerStyle.EMPHASIS, separator=True)

        # Progresso
        if progress > 0:
            self.add_progress_bar(progress, "Progresso", True)

        # Story Points
        if story_points > 0:
            self.add_text(
                f"Story Points: {story_points}",
                size=TextSize.SMALL,
                color=TextColor.ACCENT
            )

        # Acao
        if url:
            self.add_action_url("Ver Story", url)

        return self.build()

    def create_alert_card(
        self,
        alert_type: str,
        title: str,
        message: str,
        details: List[tuple] = None,
        action_url: str = "",
        action_title: str = "Ver Detalhes"
    ) -> Dict[str, Any]:
        """
        Cria card de alerta.

        Args:
            alert_type: Tipo (success, warning, error, info)
            title: Titulo
            message: Mensagem
            details: Detalhes adicionais
            action_url: URL de acao
            action_title: Titulo do botao

        Returns:
            Card Adaptativo em formato JSON
        """
        self.clear()

        # Determina cor e icone
        type_config = {
            "success": (TextColor.GOOD, "✅"),
            "warning": (TextColor.WARNING, "⚠️"),
            "error": (TextColor.ATTENTION, "❌"),
            "info": (TextColor.ACCENT, "ℹ️")
        }

        color, icon = type_config.get(alert_type.lower(), (TextColor.DEFAULT, "📢"))

        # Header com icone
        self.add_heading(f"{icon} {title}", color=color)

        # Mensagem
        self.add_text(message)

        # Detalhes
        if details:
            self.add_fact_set(details, separator=True)

        # Timestamp
        self.add_text(
            datetime.now().strftime("%d/%m/%Y %H:%M"),
            size=TextSize.SMALL,
            color=TextColor.LIGHT
        )

        # Acao
        if action_url:
            self.add_action_url(action_title, action_url)

        return self.build()

    def create_approval_card(
        self,
        title: str,
        description: str,
        requester: str,
        details: List[tuple] = None,
        approval_data: Dict[str, Any] = None
    ) -> Dict[str, Any]:
        """
        Cria card de aprovacao.

        Args:
            title: Titulo
            description: Descricao
            requester: Solicitante
            details: Detalhes
            approval_data: Dados para acoes de aprovacao

        Returns:
            Card Adaptativo em formato JSON
        """
        self.clear()

        # Header
        self.add_heading(f"📋 {title}")
        self.add_text(description)

        # Solicitante
        self.add_text(
            f"Solicitado por: {requester}",
            size=TextSize.SMALL,
            color=TextColor.DARK
        )

        # Detalhes
        if details:
            self.add_fact_set(details, separator=True)

        # Acoes de aprovacao
        data = approval_data or {}
        self.add_action_submit(
            "Aprovar",
            {**data, "action": "approve"},
            ActionStyle.POSITIVE
        )
        self.add_action_submit(
            "Rejeitar",
            {**data, "action": "reject"},
            ActionStyle.DESTRUCTIVE
        )

        return self.build()

    # =========================================================================
    # Build
    # =========================================================================

    def build(self) -> Dict[str, Any]:
        """
        Constroi o card final.

        Returns:
            Card Adaptativo em formato JSON
        """
        card = {
            "type": "AdaptiveCard",
            "$schema": self.schema,
            "version": self.version,
            "body": self._body.copy()
        }

        if self._actions:
            card["actions"] = self._actions.copy()

        if self._fallback_text:
            card["fallbackText"] = self._fallback_text

        return card

    def build_attachment(self) -> Dict[str, Any]:
        """
        Constroi o card como attachment para API.

        Returns:
            Attachment com card adaptativo
        """
        return {
            "contentType": "application/vnd.microsoft.card.adaptive",
            "content": self.build()
        }
