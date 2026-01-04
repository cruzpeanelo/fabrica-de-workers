# -*- coding: utf-8 -*-
"""
Accessibility Middleware - Plataforma E v6.5
===================================================

Middleware e utilidades para garantir acessibilidade WCAG 2.1 AA.
Inclui: ARIA labels, skip links, screen reader helpers, validacao.

Uso:
1. Adicionar middleware: app.add_middleware(AccessibilityMiddleware)
2. Usar helpers: AccessibilityHelper.add_aria_labels(html)

Issue #80 - Acessibilidade WCAG 2.1 AA
"""

import re
from typing import Optional, Dict, List, Any, Callable
from dataclasses import dataclass
from enum import Enum
import html as html_lib
from functools import wraps


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class AriaRole(str, Enum):
    """Roles ARIA padrao."""
    ALERT = "alert"
    ALERTDIALOG = "alertdialog"
    APPLICATION = "application"
    ARTICLE = "article"
    BANNER = "banner"
    BUTTON = "button"
    CELL = "cell"
    CHECKBOX = "checkbox"
    COLUMNHEADER = "columnheader"
    COMBOBOX = "combobox"
    COMPLEMENTARY = "complementary"
    CONTENTINFO = "contentinfo"
    DIALOG = "dialog"
    DIRECTORY = "directory"
    DOCUMENT = "document"
    FEED = "feed"
    FIGURE = "figure"
    FORM = "form"
    GRID = "grid"
    GRIDCELL = "gridcell"
    GROUP = "group"
    HEADING = "heading"
    IMG = "img"
    LINK = "link"
    LIST = "list"
    LISTBOX = "listbox"
    LISTITEM = "listitem"
    LOG = "log"
    MAIN = "main"
    MARQUEE = "marquee"
    MATH = "math"
    MENU = "menu"
    MENUBAR = "menubar"
    MENUITEM = "menuitem"
    MENUITEMCHECKBOX = "menuitemcheckbox"
    MENUITEMRADIO = "menuitemradio"
    NAVIGATION = "navigation"
    NONE = "none"
    NOTE = "note"
    OPTION = "option"
    PRESENTATION = "presentation"
    PROGRESSBAR = "progressbar"
    RADIO = "radio"
    RADIOGROUP = "radiogroup"
    REGION = "region"
    ROW = "row"
    ROWGROUP = "rowgroup"
    ROWHEADER = "rowheader"
    SCROLLBAR = "scrollbar"
    SEARCH = "search"
    SEARCHBOX = "searchbox"
    SEPARATOR = "separator"
    SLIDER = "slider"
    SPINBUTTON = "spinbutton"
    STATUS = "status"
    SWITCH = "switch"
    TAB = "tab"
    TABLE = "table"
    TABLIST = "tablist"
    TABPANEL = "tabpanel"
    TERM = "term"
    TEXTBOX = "textbox"
    TIMER = "timer"
    TOOLBAR = "toolbar"
    TOOLTIP = "tooltip"
    TREE = "tree"
    TREEGRID = "treegrid"
    TREEITEM = "treeitem"


class AriaLive(str, Enum):
    """Valores de aria-live para regioes dinamicas."""
    OFF = "off"
    POLITE = "polite"
    ASSERTIVE = "assertive"


# Constantes de contraste WCAG
WCAG_CONTRAST_AA_NORMAL = 4.5  # Texto normal
WCAG_CONTRAST_AA_LARGE = 3.0  # Texto grande (18pt+ ou 14pt bold)
WCAG_CONTRAST_AAA_NORMAL = 7.0  # AAA texto normal
WCAG_CONTRAST_AAA_LARGE = 4.5  # AAA texto grande


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class AccessibilityIssue:
    """Representa um problema de acessibilidade encontrado."""
    severity: str  # 'error', 'warning', 'info'
    rule: str  # Identificador da regra WCAG
    message: str  # Descricao do problema
    element: str  # Elemento HTML afetado
    suggestion: str  # Sugestao de correcao
    wcag_criterion: str  # Criterio WCAG (ex: "1.1.1")


@dataclass
class SkipLink:
    """Configuracao de um skip link."""
    target_id: str
    label: str
    order: int = 0


# =============================================================================
# SKIP LINKS GENERATOR
# =============================================================================

class SkipLinksGenerator:
    """
    Gera HTML para skip links de navegacao.

    Skip links permitem usuarios de teclado/screen reader pular para
    areas importantes da pagina.
    """

    DEFAULT_LINKS = [
        SkipLink("main-content", "Pular para conteudo principal", 1),
        SkipLink("main-nav", "Pular para navegacao", 2),
        SkipLink("search", "Pular para busca", 3),
    ]

    @classmethod
    def generate(cls, links: Optional[List[SkipLink]] = None) -> str:
        """
        Gera HTML dos skip links.

        Args:
            links: Lista de SkipLink customizados. Se None, usa DEFAULT_LINKS.

        Returns:
            HTML string com os skip links.
        """
        if links is None:
            links = cls.DEFAULT_LINKS

        # Ordenar por prioridade
        sorted_links = sorted(links, key=lambda x: x.order)

        html_parts = [
            '<nav class="skip-links" aria-label="Links de navegacao rapida">'
        ]

        for link in sorted_links:
            html_parts.append(
                f'<a href="#{link.target_id}" class="skip-link">{html_lib.escape(link.label)}</a>'
            )

        html_parts.append('</nav>')

        return '\n'.join(html_parts)

    @classmethod
    def inject_into_html(cls, html: str, links: Optional[List[SkipLink]] = None) -> str:
        """
        Injeta skip links no inicio do body.

        Args:
            html: HTML completo da pagina.
            links: Lista de SkipLink customizados.

        Returns:
            HTML com skip links injetados.
        """
        skip_links_html = cls.generate(links)

        # Encontrar tag <body> e inserir skip links apos ela
        body_pattern = re.compile(r'(<body[^>]*>)', re.IGNORECASE)
        match = body_pattern.search(html)

        if match:
            insert_pos = match.end()
            return html[:insert_pos] + '\n' + skip_links_html + '\n' + html[insert_pos:]

        return html


# =============================================================================
# ARIA HELPER
# =============================================================================

class AriaHelper:
    """
    Helper para adicionar atributos ARIA a elementos HTML.
    """

    @staticmethod
    def label(text: str) -> str:
        """Retorna atributo aria-label."""
        return f'aria-label="{html_lib.escape(text)}"'

    @staticmethod
    def labelledby(*ids: str) -> str:
        """Retorna atributo aria-labelledby."""
        return f'aria-labelledby="{" ".join(ids)}"'

    @staticmethod
    def describedby(*ids: str) -> str:
        """Retorna atributo aria-describedby."""
        return f'aria-describedby="{" ".join(ids)}"'

    @staticmethod
    def role(role: AriaRole) -> str:
        """Retorna atributo role."""
        return f'role="{role.value}"'

    @staticmethod
    def live(mode: AriaLive = AriaLive.POLITE, atomic: bool = True) -> str:
        """Retorna atributos para live region."""
        attrs = [f'aria-live="{mode.value}"']
        if atomic:
            attrs.append('aria-atomic="true"')
        return ' '.join(attrs)

    @staticmethod
    def hidden(hidden: bool = True) -> str:
        """Retorna atributo aria-hidden."""
        return f'aria-hidden="{str(hidden).lower()}"'

    @staticmethod
    def expanded(expanded: bool) -> str:
        """Retorna atributo aria-expanded."""
        return f'aria-expanded="{str(expanded).lower()}"'

    @staticmethod
    def selected(selected: bool) -> str:
        """Retorna atributo aria-selected."""
        return f'aria-selected="{str(selected).lower()}"'

    @staticmethod
    def checked(checked: bool) -> str:
        """Retorna atributo aria-checked."""
        return f'aria-checked="{str(checked).lower()}"'

    @staticmethod
    def disabled(disabled: bool = True) -> str:
        """Retorna atributo aria-disabled."""
        return f'aria-disabled="{str(disabled).lower()}"'

    @staticmethod
    def current(value: str = "page") -> str:
        """Retorna atributo aria-current."""
        return f'aria-current="{value}"'

    @staticmethod
    def invalid(invalid: bool = True, error_id: Optional[str] = None) -> str:
        """Retorna atributos para campo invalido."""
        attrs = [f'aria-invalid="{str(invalid).lower()}"']
        if error_id:
            attrs.append(f'aria-errormessage="{error_id}"')
        return ' '.join(attrs)

    @staticmethod
    def progress(value: int, min_val: int = 0, max_val: int = 100) -> str:
        """Retorna atributos para barra de progresso."""
        return f'role="progressbar" aria-valuenow="{value}" aria-valuemin="{min_val}" aria-valuemax="{max_val}"'

    @staticmethod
    def tab(selected: bool = False, controls: Optional[str] = None) -> str:
        """Retorna atributos para tab."""
        attrs = [
            'role="tab"',
            f'aria-selected="{str(selected).lower()}"',
            f'tabindex="{0 if selected else -1}"'
        ]
        if controls:
            attrs.append(f'aria-controls="{controls}"')
        return ' '.join(attrs)

    @staticmethod
    def tabpanel(labelledby: str, hidden: bool = False) -> str:
        """Retorna atributos para tabpanel."""
        attrs = [
            'role="tabpanel"',
            f'aria-labelledby="{labelledby}"',
            f'tabindex="0"'
        ]
        if hidden:
            attrs.append('hidden')
        return ' '.join(attrs)

    @staticmethod
    def modal(title_id: str, describedby: Optional[str] = None) -> str:
        """Retorna atributos para modal/dialog."""
        attrs = [
            'role="dialog"',
            'aria-modal="true"',
            f'aria-labelledby="{title_id}"'
        ]
        if describedby:
            attrs.append(f'aria-describedby="{describedby}"')
        return ' '.join(attrs)


# =============================================================================
# LIVE REGION HELPER
# =============================================================================

class LiveRegion:
    """
    Helper para criar regioes de anuncio dinamico para screen readers.
    """

    @staticmethod
    def status() -> str:
        """Cria regiao de status (polite)."""
        return '<div role="status" aria-live="polite" aria-atomic="true" class="sr-only"></div>'

    @staticmethod
    def alert() -> str:
        """Cria regiao de alerta (assertive)."""
        return '<div role="alert" aria-live="assertive" aria-atomic="true" class="sr-only"></div>'

    @staticmethod
    def log() -> str:
        """Cria regiao de log (polite, atomic=false)."""
        return '<div role="log" aria-live="polite" aria-atomic="false" class="sr-only"></div>'

    @staticmethod
    def announcer(id: str = "a11y-announcer") -> str:
        """
        Cria um announcer para anuncios programaticos.

        Uso em JavaScript:
        document.getElementById('a11y-announcer').textContent = 'Mensagem';
        """
        return f'''
<div id="{id}"
     role="status"
     aria-live="polite"
     aria-atomic="true"
     class="sr-only">
</div>
'''


# =============================================================================
# HTML ACCESSIBILITY ENHANCER
# =============================================================================

class AccessibilityEnhancer:
    """
    Melhora acessibilidade de HTML automaticamente.
    """

    @classmethod
    def enhance_html(cls, html: str) -> str:
        """
        Aplica melhorias de acessibilidade ao HTML.

        Args:
            html: HTML original.

        Returns:
            HTML melhorado.
        """
        html = cls._add_img_alt_placeholders(html)
        html = cls._add_button_aria_labels(html)
        html = cls._add_input_labels(html)
        html = cls._add_link_improvements(html)
        html = cls._add_table_improvements(html)
        html = cls._add_form_improvements(html)
        return html

    @classmethod
    def _add_img_alt_placeholders(cls, html: str) -> str:
        """Adiciona alt vazio para imagens sem alt (decorativas)."""
        # Encontra imagens sem atributo alt
        pattern = r'<img(?![^>]*\balt\b)([^>]*)>'
        replacement = r'<img alt=""\1>'
        return re.sub(pattern, replacement, html, flags=re.IGNORECASE)

    @classmethod
    def _add_button_aria_labels(cls, html: str) -> str:
        """Adiciona aria-label para botoes apenas com icones."""
        # Padrao para botoes com apenas icone SVG ou i
        pattern = r'<button([^>]*)>(\s*<(?:svg|i)[^>]*>[^<]*</(?:svg|i)>\s*)</button>'

        def add_label(match):
            attrs = match.group(1)
            content = match.group(2)

            # Se ja tem aria-label, retorna original
            if 'aria-label' in attrs:
                return match.group(0)

            # Adiciona aria-label generico (deve ser substituido manualmente)
            return f'<button{attrs} aria-label="Acao">{content}</button>'

        return re.sub(pattern, add_label, html, flags=re.IGNORECASE | re.DOTALL)

    @classmethod
    def _add_input_labels(cls, html: str) -> str:
        """Verifica e adiciona aria-label para inputs sem label associado."""
        # Encontra inputs com placeholder mas sem label
        pattern = r'<input([^>]*placeholder="([^"]*)"[^>]*)>'

        def add_label(match):
            attrs = match.group(1)
            placeholder = match.group(2)

            # Se ja tem aria-label ou aria-labelledby, retorna original
            if 'aria-label' in attrs or 'aria-labelledby' in attrs:
                return match.group(0)

            # Usa placeholder como aria-label
            return f'<input{attrs} aria-label="{placeholder}">'

        return re.sub(pattern, add_label, html, flags=re.IGNORECASE)

    @classmethod
    def _add_link_improvements(cls, html: str) -> str:
        """Melhora links que abrem em nova janela."""
        # Adiciona aviso para links target="_blank"
        pattern = r'<a([^>]*target="_blank"[^>]*)>'

        def improve_link(match):
            attrs = match.group(1)

            # Adiciona rel="noopener noreferrer" se nao tiver
            if 'rel=' not in attrs:
                attrs += ' rel="noopener noreferrer"'

            return f'<a{attrs}>'

        return re.sub(pattern, improve_link, html, flags=re.IGNORECASE)

    @classmethod
    def _add_table_improvements(cls, html: str) -> str:
        """Adiciona scope aos headers de tabela."""
        # Adiciona scope="col" aos th dentro de thead
        pattern = r'(<thead[^>]*>.*?)(<th)([^>]*>)'

        def add_scope(match):
            before = match.group(1)
            th = match.group(2)
            attrs = match.group(3)

            if 'scope=' not in attrs:
                return f'{before}{th} scope="col"{attrs}'
            return match.group(0)

        html = re.sub(pattern, add_scope, html, flags=re.IGNORECASE | re.DOTALL)
        return html

    @classmethod
    def _add_form_improvements(cls, html: str) -> str:
        """Adiciona atributos de acessibilidade a formularios."""
        # Adiciona novalidate para usar validacao custom
        pattern = r'<form([^>]*)>'

        def improve_form(match):
            attrs = match.group(1)

            # Nao adiciona se ja tiver novalidate
            if 'novalidate' in attrs:
                return match.group(0)

            # Nao adiciona novalidate por padrao, deixa para o dev decidir
            return match.group(0)

        return re.sub(pattern, improve_form, html, flags=re.IGNORECASE)


# =============================================================================
# CONTRAST CHECKER
# =============================================================================

class ContrastChecker:
    """
    Verifica contraste de cores conforme WCAG 2.1.
    """

    @staticmethod
    def hex_to_rgb(hex_color: str) -> tuple:
        """Converte cor hex para RGB."""
        hex_color = hex_color.lstrip('#')
        if len(hex_color) == 3:
            hex_color = ''.join([c*2 for c in hex_color])
        return tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))

    @staticmethod
    def get_luminance(rgb: tuple) -> float:
        """
        Calcula luminancia relativa de uma cor.

        Conforme WCAG 2.1: https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html
        """
        def adjust(c):
            c = c / 255.0
            return c / 12.92 if c <= 0.03928 else ((c + 0.055) / 1.055) ** 2.4

        r, g, b = rgb
        return 0.2126 * adjust(r) + 0.7152 * adjust(g) + 0.0722 * adjust(b)

    @classmethod
    def get_contrast_ratio(cls, color1: str, color2: str) -> float:
        """
        Calcula razao de contraste entre duas cores.

        Args:
            color1: Cor em formato hex (#RRGGBB ou #RGB)
            color2: Cor em formato hex

        Returns:
            Razao de contraste (1 a 21)
        """
        rgb1 = cls.hex_to_rgb(color1)
        rgb2 = cls.hex_to_rgb(color2)

        lum1 = cls.get_luminance(rgb1)
        lum2 = cls.get_luminance(rgb2)

        lighter = max(lum1, lum2)
        darker = min(lum1, lum2)

        return (lighter + 0.05) / (darker + 0.05)

    @classmethod
    def check_wcag_aa(cls, foreground: str, background: str, is_large_text: bool = False) -> bool:
        """
        Verifica se combinacao de cores passa WCAG AA.

        Args:
            foreground: Cor do texto (hex)
            background: Cor do fundo (hex)
            is_large_text: Se e texto grande (18pt+ ou 14pt bold)

        Returns:
            True se passa WCAG AA
        """
        ratio = cls.get_contrast_ratio(foreground, background)
        required = WCAG_CONTRAST_AA_LARGE if is_large_text else WCAG_CONTRAST_AA_NORMAL
        return ratio >= required

    @classmethod
    def check_wcag_aaa(cls, foreground: str, background: str, is_large_text: bool = False) -> bool:
        """Verifica se combinacao de cores passa WCAG AAA."""
        ratio = cls.get_contrast_ratio(foreground, background)
        required = WCAG_CONTRAST_AAA_LARGE if is_large_text else WCAG_CONTRAST_AAA_NORMAL
        return ratio >= required

    @classmethod
    def suggest_color(cls, background: str, target_ratio: float = WCAG_CONTRAST_AA_NORMAL) -> str:
        """
        Sugere cor de texto com contraste adequado.

        Args:
            background: Cor de fundo (hex)
            target_ratio: Razao de contraste desejada

        Returns:
            Cor sugerida (preto ou branco)
        """
        white_ratio = cls.get_contrast_ratio("#FFFFFF", background)
        black_ratio = cls.get_contrast_ratio("#000000", background)

        # Retorna a cor com maior contraste
        return "#FFFFFF" if white_ratio > black_ratio else "#000000"


# =============================================================================
# ACCESSIBILITY VALIDATOR
# =============================================================================

class AccessibilityValidator:
    """
    Valida HTML para problemas de acessibilidade.
    """

    @classmethod
    def validate(cls, html: str) -> List[AccessibilityIssue]:
        """
        Valida HTML e retorna lista de problemas encontrados.

        Args:
            html: HTML a ser validado.

        Returns:
            Lista de AccessibilityIssue
        """
        issues = []

        # Verificar imagens sem alt
        issues.extend(cls._check_img_alt(html))

        # Verificar links vazios
        issues.extend(cls._check_empty_links(html))

        # Verificar headings em ordem
        issues.extend(cls._check_heading_order(html))

        # Verificar formularios sem labels
        issues.extend(cls._check_form_labels(html))

        # Verificar tabelas sem headers
        issues.extend(cls._check_table_headers(html))

        # Verificar botoes sem texto
        issues.extend(cls._check_button_text(html))

        # Verificar iframes sem title
        issues.extend(cls._check_iframe_title(html))

        return issues

    @classmethod
    def _check_img_alt(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica imagens sem alt."""
        issues = []
        pattern = r'<img(?![^>]*\balt\s*=)[^>]*>'
        matches = re.findall(pattern, html, re.IGNORECASE)

        for match in matches:
            issues.append(AccessibilityIssue(
                severity="error",
                rule="img-alt",
                message="Imagem sem atributo alt",
                element=match[:100],
                suggestion="Adicione alt='descricao' ou alt='' para imagens decorativas",
                wcag_criterion="1.1.1"
            ))

        return issues

    @classmethod
    def _check_empty_links(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica links vazios ou apenas com imagem."""
        issues = []

        # Links completamente vazios
        pattern = r'<a[^>]*>\s*</a>'
        matches = re.findall(pattern, html, re.IGNORECASE)

        for match in matches:
            issues.append(AccessibilityIssue(
                severity="error",
                rule="link-name",
                message="Link sem texto acessivel",
                element=match[:100],
                suggestion="Adicione texto ou aria-label ao link",
                wcag_criterion="2.4.4"
            ))

        return issues

    @classmethod
    def _check_heading_order(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica se headings estao em ordem correta."""
        issues = []

        # Encontra todos os headings
        pattern = r'<h([1-6])[^>]*>'
        matches = re.findall(pattern, html, re.IGNORECASE)

        if not matches:
            return issues

        prev_level = 0
        for level in matches:
            current_level = int(level)

            # Verifica se pulou niveis (ex: h1 -> h3)
            if current_level > prev_level + 1 and prev_level > 0:
                issues.append(AccessibilityIssue(
                    severity="warning",
                    rule="heading-order",
                    message=f"Heading h{current_level} pula nivel (anterior era h{prev_level})",
                    element=f"<h{current_level}>",
                    suggestion=f"Use h{prev_level + 1} antes de h{current_level}",
                    wcag_criterion="1.3.1"
                ))

            prev_level = current_level

        return issues

    @classmethod
    def _check_form_labels(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica inputs sem labels associados."""
        issues = []

        # Encontra inputs sem aria-label, aria-labelledby ou id para label
        pattern = r'<input(?![^>]*(?:aria-label|aria-labelledby|type=["\'](?:submit|reset|button|hidden)["\']))[^>]*>'
        matches = re.findall(pattern, html, re.IGNORECASE)

        for match in matches:
            # Verifica se tem id
            id_match = re.search(r'id=["\']([^"\']+)["\']', match)
            if id_match:
                input_id = id_match.group(1)
                # Verifica se existe label com for correspondente
                label_pattern = rf'<label[^>]*for=["\']{ re.escape(input_id)}["\'][^>]*>'
                if re.search(label_pattern, html, re.IGNORECASE):
                    continue

            issues.append(AccessibilityIssue(
                severity="error",
                rule="label",
                message="Input sem label associado",
                element=match[:100],
                suggestion="Adicione <label for='id'> ou aria-label ao input",
                wcag_criterion="1.3.1"
            ))

        return issues

    @classmethod
    def _check_table_headers(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica tabelas sem headers."""
        issues = []

        # Encontra tabelas
        table_pattern = r'<table[^>]*>.*?</table>'
        tables = re.findall(table_pattern, html, re.IGNORECASE | re.DOTALL)

        for table in tables:
            # Verifica se tem th ou role="columnheader"
            if not re.search(r'<th|role=["\']columnheader["\']', table, re.IGNORECASE):
                issues.append(AccessibilityIssue(
                    severity="warning",
                    rule="th-has-data-cells",
                    message="Tabela sem headers (th)",
                    element="<table>",
                    suggestion="Use <th> para celulas de cabecalho",
                    wcag_criterion="1.3.1"
                ))

        return issues

    @classmethod
    def _check_button_text(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica botoes sem texto acessivel."""
        issues = []

        # Botoes vazios ou apenas com icone
        pattern = r'<button(?![^>]*aria-label)[^>]*>(\s*<(?:svg|i|img)[^>]*>(?:</(?:svg|i)>)?\s*)</button>'
        matches = re.findall(pattern, html, re.IGNORECASE | re.DOTALL)

        for match in matches:
            issues.append(AccessibilityIssue(
                severity="error",
                rule="button-name",
                message="Botao sem texto acessivel",
                element="<button> com apenas icone",
                suggestion="Adicione aria-label ou texto visivel ao botao",
                wcag_criterion="4.1.2"
            ))

        return issues

    @classmethod
    def _check_iframe_title(cls, html: str) -> List[AccessibilityIssue]:
        """Verifica iframes sem title."""
        issues = []

        pattern = r'<iframe(?![^>]*title)[^>]*>'
        matches = re.findall(pattern, html, re.IGNORECASE)

        for match in matches:
            issues.append(AccessibilityIssue(
                severity="error",
                rule="frame-title",
                message="Iframe sem atributo title",
                element=match[:100],
                suggestion="Adicione title='descricao do conteudo'",
                wcag_criterion="2.4.1"
            ))

        return issues


# =============================================================================
# FASTAPI MIDDLEWARE
# =============================================================================

try:
    from starlette.middleware.base import BaseHTTPMiddleware
    from starlette.requests import Request
    from starlette.responses import Response
    import io

    class AccessibilityMiddleware(BaseHTTPMiddleware):
        """
        Middleware FastAPI para melhorias automaticas de acessibilidade.

        Uso:
            app.add_middleware(AccessibilityMiddleware,
                               inject_skip_links=True,
                               enhance_html=True,
                               validate=False)
        """

        def __init__(
            self,
            app,
            inject_skip_links: bool = True,
            enhance_html: bool = True,
            validate: bool = False,
            skip_links: Optional[List[SkipLink]] = None,
            inject_announcer: bool = True
        ):
            super().__init__(app)
            self.inject_skip_links = inject_skip_links
            self.enhance_html = enhance_html
            self.validate = validate
            self.skip_links = skip_links
            self.inject_announcer = inject_announcer

        async def dispatch(self, request: Request, call_next) -> Response:
            response = await call_next(request)

            # So processa respostas HTML
            content_type = response.headers.get("content-type", "")
            if "text/html" not in content_type:
                return response

            # Le o corpo da resposta
            body = b""
            async for chunk in response.body_iterator:
                body += chunk

            html = body.decode("utf-8")

            # Aplica melhorias
            if self.inject_skip_links:
                html = SkipLinksGenerator.inject_into_html(html, self.skip_links)

            if self.inject_announcer:
                # Injeta announcer antes de </body>
                announcer = LiveRegion.announcer()
                html = html.replace("</body>", f"{announcer}\n</body>")

            if self.enhance_html:
                html = AccessibilityEnhancer.enhance_html(html)

            if self.validate:
                issues = AccessibilityValidator.validate(html)
                if issues:
                    # Adiciona comentario HTML com issues (em desenvolvimento)
                    comment = "\n<!-- Accessibility Issues:\n"
                    for issue in issues:
                        comment += f"  [{issue.severity}] {issue.message} - WCAG {issue.wcag_criterion}\n"
                    comment += "-->\n"
                    html = html.replace("</body>", f"{comment}</body>")

            # Retorna resposta modificada
            return Response(
                content=html.encode("utf-8"),
                status_code=response.status_code,
                headers=dict(response.headers),
                media_type=response.media_type
            )

except ImportError:
    # Starlette nao disponivel
    class AccessibilityMiddleware:
        """Placeholder quando Starlette nao esta disponivel."""
        def __init__(self, *args, **kwargs):
            raise ImportError("Starlette/FastAPI required for AccessibilityMiddleware")


# =============================================================================
# DECORATORS PARA ROTAS
# =============================================================================

def accessible_response(
    inject_skip_links: bool = True,
    enhance: bool = True
) -> Callable:
    """
    Decorator para aplicar melhorias de acessibilidade a respostas HTML.

    Uso:
        @app.get("/")
        @accessible_response()
        async def home():
            return HTMLResponse("<html>...")
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            response = await func(*args, **kwargs)

            # So processa HTMLResponse
            if hasattr(response, 'body'):
                html = response.body.decode('utf-8')

                if inject_skip_links:
                    html = SkipLinksGenerator.inject_into_html(html)

                if enhance:
                    html = AccessibilityEnhancer.enhance_html(html)

                response.body = html.encode('utf-8')

            return response

        return wrapper
    return decorator


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Enums
    'AriaRole',
    'AriaLive',

    # Classes
    'SkipLink',
    'AccessibilityIssue',
    'SkipLinksGenerator',
    'AriaHelper',
    'LiveRegion',
    'AccessibilityEnhancer',
    'ContrastChecker',
    'AccessibilityValidator',
    'AccessibilityMiddleware',

    # Decorators
    'accessible_response',

    # Constantes
    'WCAG_CONTRAST_AA_NORMAL',
    'WCAG_CONTRAST_AA_LARGE',
    'WCAG_CONTRAST_AAA_NORMAL',
    'WCAG_CONTRAST_AAA_LARGE',
]


# =============================================================================
# DOCUMENTACAO DE USO
# =============================================================================
"""
GUIA DE USO - Accessibility Module

1. ADICIONAR MIDDLEWARE (Recomendado):

   from factory.dashboard.accessibility import AccessibilityMiddleware

   app.add_middleware(
       AccessibilityMiddleware,
       inject_skip_links=True,
       enhance_html=True,
       validate=False,  # True em desenvolvimento
       inject_announcer=True
   )

2. USAR ARIA HELPER:

   from factory.dashboard.accessibility import AriaHelper, AriaRole

   # Em templates
   <button {AriaHelper.label("Adicionar item")}>+</button>
   <div {AriaHelper.role(AriaRole.DIALOG)} {AriaHelper.modal("modal-title")}>
   <div {AriaHelper.live()}>Conteudo dinamico</div>

3. GERAR SKIP LINKS:

   from factory.dashboard.accessibility import SkipLinksGenerator, SkipLink

   skip_links = [
       SkipLink("main", "Pular para conteudo", 1),
       SkipLink("nav", "Pular para navegacao", 2),
   ]
   html = SkipLinksGenerator.generate(skip_links)

4. VERIFICAR CONTRASTE:

   from factory.dashboard.accessibility import ContrastChecker

   # Verificar se passa WCAG AA
   passes = ContrastChecker.check_wcag_aa("#FFFFFF", "#003B4A")

   # Obter razao de contraste
   ratio = ContrastChecker.get_contrast_ratio("#FFFFFF", "#003B4A")

   # Sugerir cor de texto
   text_color = ContrastChecker.suggest_color("#003B4A")

5. VALIDAR HTML:

   from factory.dashboard.accessibility import AccessibilityValidator

   issues = AccessibilityValidator.validate(html)
   for issue in issues:
       print(f"[{issue.severity}] {issue.message} - WCAG {issue.wcag_criterion}")

6. CRIAR LIVE REGIONS:

   from factory.dashboard.accessibility import LiveRegion

   # Adicionar ao template
   html += LiveRegion.announcer()
   html += LiveRegion.status()
   html += LiveRegion.alert()

7. MELHORAR HTML AUTOMATICAMENTE:

   from factory.dashboard.accessibility import AccessibilityEnhancer

   improved_html = AccessibilityEnhancer.enhance_html(original_html)
"""
