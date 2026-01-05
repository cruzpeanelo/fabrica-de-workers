"""
AI Design Generator - Prompt-to-App

Gera interfaces UI a partir de descricoes em linguagem natural.
Inspirado no Base44.app para criacao de apps com IA.

Exemplos de uso:
- "Crie um formulario de login com email, senha e botao entrar"
- "Preciso de uma tabela de usuarios com nome, email e status"
- "Faca um dashboard com cards de metricas e grafico"
"""

import os
import json
import re
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from enum import Enum

# Tenta importar Claude
try:
    import anthropic
    HAS_CLAUDE = True
except ImportError:
    HAS_CLAUDE = False

from factory.ui_components.component_registry import ComponentRegistry


class DesignIntent(str, Enum):
    """Tipos de intencao de design detectados"""
    FORM = "form"
    TABLE = "table"
    DASHBOARD = "dashboard"
    LIST = "list"
    NAVIGATION = "navigation"
    LANDING = "landing"
    MODAL = "modal"
    CARD = "card"
    CRUD = "crud"
    AUTH = "auth"
    UNKNOWN = "unknown"


@dataclass
class GeneratedComponent:
    """Componente gerado pela IA"""
    component_id: str
    props: Dict[str, Any]
    position: Dict[str, int]
    children: List['GeneratedComponent'] = None

    def to_dict(self) -> Dict:
        return {
            'component_id': self.component_id,
            'props': self.props,
            'position': self.position,
            'children': [c.to_dict() for c in (self.children or [])]
        }


@dataclass
class DesignResult:
    """Resultado da geracao de design"""
    intent: DesignIntent
    title: str
    description: str
    components: List[GeneratedComponent]
    layout_suggestion: str
    confidence: float

    def to_dict(self) -> Dict:
        return {
            'intent': self.intent.value,
            'title': self.title,
            'description': self.description,
            'components': [c.to_dict() for c in self.components],
            'layout_suggestion': self.layout_suggestion,
            'confidence': self.confidence
        }


class AIDesignGenerator:
    """
    Gerador de design baseado em IA.

    Interpreta prompts em linguagem natural e gera
    componentes UI apropriados para o Visual Builder.
    """

    # Mapeamento de palavras-chave para componentes
    KEYWORD_COMPONENTS = {
        # Formularios
        'formulario': ['form'],
        'form': ['form'],
        'cadastro': ['form'],
        'registro': ['form'],

        # Campos
        'email': ['input'],
        'senha': ['input'],
        'password': ['input'],
        'nome': ['input'],
        'telefone': ['input'],
        'endereco': ['textarea'],
        'descricao': ['textarea'],
        'comentario': ['textarea'],
        'mensagem': ['textarea'],
        'selecionar': ['select'],
        'opcao': ['select'],
        'checkbox': ['checkbox'],
        'aceito': ['checkbox'],
        'termos': ['checkbox'],
        'ativar': ['switch'],
        'toggle': ['switch'],

        # Botoes
        'botao': ['button'],
        'button': ['button'],
        'enviar': ['button'],
        'submit': ['button'],
        'salvar': ['button'],
        'cancelar': ['button'],
        'entrar': ['button'],
        'login': ['button'],

        # Dados
        'tabela': ['table'],
        'table': ['table'],
        'lista': ['list'],
        'list': ['list'],
        'card': ['card'],
        'cards': ['card'],
        'avatar': ['avatar'],
        'badge': ['badge'],
        'status': ['badge'],
        'progresso': ['progress'],

        # Layout
        'grid': ['grid'],
        'colunas': ['grid'],
        'container': ['container'],
        'sidebar': ['sidebar'],
        'menu': ['navbar'],
        'navegacao': ['navbar'],
        'abas': ['tabs'],
        'tabs': ['tabs'],

        # Feedback
        'alerta': ['alert'],
        'aviso': ['alert'],
        'modal': ['modal'],
        'popup': ['modal'],
        'loading': ['spinner'],
        'carregando': ['spinner'],
        'tooltip': ['tooltip'],
        'dica': ['tooltip'],

        # Texto
        'titulo': ['text'],
        'texto': ['text'],
        'paragrafo': ['text'],
        'label': ['text'],
        'link': ['link'],

        # Imagens
        'imagem': ['image'],
        'foto': ['image'],
        'icone': ['icon'],
    }

    # Mapeamento de intencoes para layouts
    INTENT_LAYOUTS = {
        DesignIntent.FORM: "vertical",
        DesignIntent.TABLE: "full-width",
        DesignIntent.DASHBOARD: "grid",
        DesignIntent.LIST: "vertical",
        DesignIntent.NAVIGATION: "horizontal",
        DesignIntent.LANDING: "sections",
        DesignIntent.MODAL: "centered",
        DesignIntent.CARD: "grid",
        DesignIntent.CRUD: "split",
        DesignIntent.AUTH: "centered",
    }

    def __init__(self):
        self.registry = ComponentRegistry()
        self.claude_client = None

        if HAS_CLAUDE:
            api_key = os.environ.get('ANTHROPIC_API_KEY')
            if api_key:
                self.claude_client = anthropic.Anthropic(api_key=api_key)

    def generate_from_prompt(self, prompt: str, use_ai: bool = True) -> DesignResult:
        """
        Gera design a partir de um prompt em linguagem natural.

        Args:
            prompt: Descricao do que o usuario quer criar
            use_ai: Se True, usa Claude para interpretacao avancada

        Returns:
            DesignResult com componentes gerados
        """
        # Normaliza o prompt
        prompt_lower = prompt.lower().strip()

        # Detecta intencao
        intent = self._detect_intent(prompt_lower)

        # Tenta usar IA se disponivel
        if use_ai and self.claude_client:
            try:
                return self._generate_with_ai(prompt, intent)
            except Exception as e:
                print(f"[AI Design] Fallback para regras: {e}")

        # Fallback: usa regras baseadas em keywords
        return self._generate_with_rules(prompt, intent)

    def _detect_intent(self, prompt: str) -> DesignIntent:
        """Detecta a intencao principal do prompt"""
        intent_keywords = {
            DesignIntent.FORM: ['formulario', 'form', 'cadastro', 'registro', 'entrada'],
            DesignIntent.TABLE: ['tabela', 'table', 'listagem', 'dados', 'registros'],
            DesignIntent.DASHBOARD: ['dashboard', 'painel', 'metricas', 'kpi', 'grafico'],
            DesignIntent.LIST: ['lista', 'list', 'items', 'itens'],
            DesignIntent.NAVIGATION: ['menu', 'navegacao', 'nav', 'sidebar'],
            DesignIntent.LANDING: ['landing', 'home', 'pagina inicial', 'hero'],
            DesignIntent.MODAL: ['modal', 'popup', 'dialogo', 'janela'],
            DesignIntent.CARD: ['card', 'cartao', 'bloco'],
            DesignIntent.CRUD: ['crud', 'criar', 'editar', 'deletar', 'gerenciar'],
            DesignIntent.AUTH: ['login', 'autenticacao', 'entrar', 'senha', 'auth'],
        }

        for intent, keywords in intent_keywords.items():
            for kw in keywords:
                if kw in prompt:
                    return intent

        return DesignIntent.UNKNOWN

    def _generate_with_ai(self, prompt: str, intent: DesignIntent) -> DesignResult:
        """Gera design usando Claude AI"""
        system_prompt = """Voce e um especialista em design de interfaces.
Dado um prompt do usuario, gere uma lista de componentes UI em JSON.

Componentes disponiveis:
- Primitivos: button, text, image, icon, link, divider
- Formularios: input, textarea, select, checkbox, radio, switch, form
- Layout: container, card, grid, flex, spacer
- Dados: table, list, badge, avatar, progress
- Feedback: alert, modal, spinner, tooltip
- Navegacao: navbar, tabs, breadcrumb, sidebar, pagination

Para cada componente, especifique:
- component_id: ID do componente (ex: "button", "input")
- props: Propriedades do componente (ex: {"text": "Enviar", "variant": "primary"})
- position: Posicao no canvas {"x": 0, "y": 0, "width": 200, "height": 40}

Responda APENAS com JSON valido no formato:
{
  "title": "Titulo do design",
  "description": "Descricao breve",
  "components": [
    {"component_id": "...", "props": {...}, "position": {...}}
  ]
}"""

        response = self.claude_client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=2000,
            system=system_prompt,
            messages=[
                {"role": "user", "content": f"Crie uma interface para: {prompt}"}
            ]
        )

        # Parse resposta
        text = response.content[0].text
        # Extrai JSON da resposta
        json_match = re.search(r'\{[\s\S]*\}', text)
        if json_match:
            data = json.loads(json_match.group())

            components = []
            for comp_data in data.get('components', []):
                components.append(GeneratedComponent(
                    component_id=comp_data['component_id'],
                    props=comp_data.get('props', {}),
                    position=comp_data.get('position', {'x': 0, 'y': 0, 'width': 200, 'height': 40})
                ))

            return DesignResult(
                intent=intent,
                title=data.get('title', 'Design Gerado'),
                description=data.get('description', ''),
                components=components,
                layout_suggestion=self.INTENT_LAYOUTS.get(intent, 'vertical'),
                confidence=0.9
            )

        raise ValueError("Nao foi possivel extrair JSON da resposta da IA")

    def _generate_with_rules(self, prompt: str, intent: DesignIntent) -> DesignResult:
        """Gera design usando regras baseadas em keywords"""
        prompt_lower = prompt.lower()
        components = []
        y_offset = 20

        # Detecta componentes baseado em keywords
        detected_components = []
        for keyword, comp_ids in self.KEYWORD_COMPONENTS.items():
            if keyword in prompt_lower:
                for comp_id in comp_ids:
                    if comp_id not in [c[0] for c in detected_components]:
                        detected_components.append((comp_id, keyword))

        # Se nenhum componente detectado, usa defaults baseado na intencao
        if not detected_components:
            detected_components = self._get_default_components(intent)

        # Gera componentes com posicoes
        for comp_id, keyword in detected_components:
            props = self._get_default_props(comp_id, keyword, prompt_lower)
            height = self._get_component_height(comp_id)

            components.append(GeneratedComponent(
                component_id=comp_id,
                props=props,
                position={'x': 20, 'y': y_offset, 'width': 400, 'height': height}
            ))
            y_offset += height + 16

        # Gera titulo baseado na intencao
        title = self._generate_title(intent, prompt)

        return DesignResult(
            intent=intent,
            title=title,
            description=f"Design gerado a partir de: {prompt[:100]}",
            components=components,
            layout_suggestion=self.INTENT_LAYOUTS.get(intent, 'vertical'),
            confidence=0.7
        )

    def _get_default_components(self, intent: DesignIntent) -> List[Tuple[str, str]]:
        """Retorna componentes padrao para uma intencao"""
        defaults = {
            DesignIntent.FORM: [('text', 'titulo'), ('input', 'campo'), ('button', 'botao')],
            DesignIntent.TABLE: [('text', 'titulo'), ('table', 'tabela')],
            DesignIntent.DASHBOARD: [('text', 'titulo'), ('grid', 'layout'), ('card', 'metrica')],
            DesignIntent.LIST: [('text', 'titulo'), ('list', 'lista')],
            DesignIntent.AUTH: [('text', 'titulo'), ('input', 'email'), ('input', 'senha'), ('button', 'entrar')],
            DesignIntent.CARD: [('card', 'card')],
            DesignIntent.MODAL: [('modal', 'modal')],
        }
        return defaults.get(intent, [('container', 'container'), ('text', 'titulo')])

    def _get_default_props(self, comp_id: str, keyword: str, prompt: str) -> Dict[str, Any]:
        """Gera props padrao para um componente"""
        props = {}

        if comp_id == 'button':
            if 'enviar' in prompt or 'submit' in prompt:
                props = {'text': 'Enviar', 'variant': 'primary'}
            elif 'entrar' in prompt or 'login' in prompt:
                props = {'text': 'Entrar', 'variant': 'primary'}
            elif 'cancelar' in prompt:
                props = {'text': 'Cancelar', 'variant': 'outline'}
            elif 'salvar' in prompt:
                props = {'text': 'Salvar', 'variant': 'primary'}
            else:
                props = {'text': 'Clique aqui', 'variant': 'primary'}

        elif comp_id == 'input':
            if 'email' in keyword or 'email' in prompt:
                props = {'label': 'Email', 'type': 'email', 'placeholder': 'seu@email.com'}
            elif 'senha' in keyword or 'password' in prompt:
                props = {'label': 'Senha', 'type': 'password', 'placeholder': '********'}
            elif 'nome' in keyword or 'nome' in prompt:
                props = {'label': 'Nome', 'type': 'text', 'placeholder': 'Seu nome'}
            elif 'telefone' in keyword:
                props = {'label': 'Telefone', 'type': 'tel', 'placeholder': '(11) 99999-9999'}
            else:
                props = {'label': 'Campo', 'type': 'text', 'placeholder': 'Digite aqui...'}

        elif comp_id == 'textarea':
            props = {'label': 'Descricao', 'placeholder': 'Digite sua mensagem...', 'rows': 4}

        elif comp_id == 'text':
            if 'titulo' in keyword:
                # Extrai titulo do prompt se possivel
                title = self._extract_title_from_prompt(prompt)
                props = {'content': title, 'variant': 'h2'}
            else:
                props = {'content': 'Texto aqui', 'variant': 'body'}

        elif comp_id == 'select':
            props = {
                'label': 'Selecione',
                'options': [
                    {'value': '1', 'label': 'Opcao 1'},
                    {'value': '2', 'label': 'Opcao 2'},
                    {'value': '3', 'label': 'Opcao 3'}
                ]
            }

        elif comp_id == 'checkbox':
            if 'termos' in prompt:
                props = {'label': 'Aceito os termos de uso'}
            else:
                props = {'label': 'Marque esta opcao'}

        elif comp_id == 'switch':
            props = {'label': 'Ativar'}

        elif comp_id == 'table':
            props = {
                'columns': [
                    {'key': 'id', 'label': 'ID'},
                    {'key': 'nome', 'label': 'Nome'},
                    {'key': 'email', 'label': 'Email'},
                    {'key': 'status', 'label': 'Status'}
                ],
                'data': [],
                'striped': True
            }

        elif comp_id == 'card':
            props = {'title': 'Card', 'shadow': 'md', 'padding': '16px'}

        elif comp_id == 'alert':
            props = {'message': 'Esta e uma mensagem importante.', 'variant': 'info'}

        elif comp_id == 'badge':
            props = {'text': 'Ativo', 'variant': 'success'}

        elif comp_id == 'progress':
            props = {'value': 75, 'max': 100, 'showLabel': True}

        elif comp_id == 'avatar':
            props = {'name': 'Usuario', 'size': 'md'}

        elif comp_id == 'navbar':
            props = {
                'brand': 'Meu App',
                'items': [
                    {'label': 'Home', 'href': '/'},
                    {'label': 'Sobre', 'href': '/sobre'}
                ]
            }

        elif comp_id == 'tabs':
            props = {
                'tabs': [
                    {'id': 'tab1', 'label': 'Aba 1'},
                    {'id': 'tab2', 'label': 'Aba 2'}
                ],
                'activeTab': 'tab1'
            }

        elif comp_id == 'form':
            props = {'title': 'Formulario', 'submitText': 'Enviar'}

        elif comp_id == 'container':
            props = {'maxWidth': 'lg', 'padding': '24px'}

        elif comp_id == 'grid':
            props = {'columns': 2, 'gap': '16px'}

        elif comp_id == 'list':
            props = {
                'items': [
                    {'title': 'Item 1'},
                    {'title': 'Item 2'},
                    {'title': 'Item 3'}
                ],
                'variant': 'simple'
            }

        elif comp_id == 'modal':
            props = {'title': 'Modal', 'size': 'md', 'open': True}

        elif comp_id == 'image':
            props = {'src': 'https://via.placeholder.com/300x200', 'alt': 'Imagem'}

        elif comp_id == 'link':
            props = {'text': 'Clique aqui', 'href': '#'}

        elif comp_id == 'divider':
            props = {'orientation': 'horizontal'}

        elif comp_id == 'spacer':
            props = {'size': '24px'}

        return props

    def _get_component_height(self, comp_id: str) -> int:
        """Retorna altura padrao de um componente"""
        heights = {
            'button': 40,
            'input': 70,
            'textarea': 120,
            'select': 70,
            'checkbox': 30,
            'switch': 30,
            'radio': 80,
            'text': 40,
            'table': 300,
            'list': 200,
            'card': 150,
            'alert': 60,
            'modal': 400,
            'navbar': 60,
            'tabs': 200,
            'form': 300,
            'container': 200,
            'grid': 200,
            'badge': 30,
            'avatar': 50,
            'progress': 40,
            'image': 200,
            'divider': 20,
            'spacer': 24,
        }
        return heights.get(comp_id, 50)

    def _generate_title(self, intent: DesignIntent, prompt: str) -> str:
        """Gera titulo baseado na intencao"""
        titles = {
            DesignIntent.FORM: 'Formulario',
            DesignIntent.TABLE: 'Tabela de Dados',
            DesignIntent.DASHBOARD: 'Dashboard',
            DesignIntent.LIST: 'Lista',
            DesignIntent.NAVIGATION: 'Navegacao',
            DesignIntent.LANDING: 'Pagina Inicial',
            DesignIntent.MODAL: 'Modal',
            DesignIntent.CARD: 'Card',
            DesignIntent.CRUD: 'Gerenciamento',
            DesignIntent.AUTH: 'Login',
        }

        base_title = titles.get(intent, 'Design')

        # Tenta extrair contexto do prompt
        context = self._extract_title_from_prompt(prompt)
        if context and context != base_title:
            return f"{base_title} - {context}"

        return base_title

    def _extract_title_from_prompt(self, prompt: str) -> str:
        """Extrai um titulo significativo do prompt"""
        # Remove palavras comuns
        stop_words = [
            'crie', 'criar', 'faca', 'fazer', 'preciso', 'quero', 'gostaria',
            'um', 'uma', 'de', 'para', 'com', 'e', 'ou', 'que', 'o', 'a',
            'formulario', 'form', 'tabela', 'table', 'lista', 'list',
            'botao', 'button', 'campo', 'input'
        ]

        words = prompt.lower().split()
        meaningful = [w for w in words if w not in stop_words and len(w) > 2]

        if meaningful:
            # Capitaliza primeira palavra
            return ' '.join(meaningful[:3]).title()

        return 'Design'

    # === Templates predefinidos ===

    def get_template(self, template_name: str) -> Optional[DesignResult]:
        """Retorna um template predefinido"""
        templates = {
            'login': self._template_login(),
            'register': self._template_register(),
            'contact': self._template_contact(),
            'dashboard': self._template_dashboard(),
            'crud': self._template_crud(),
            'profile': self._template_profile(),
        }
        return templates.get(template_name)

    def list_templates(self) -> List[Dict]:
        """Lista templates disponiveis"""
        return [
            {'id': 'login', 'name': 'Login', 'description': 'Formulario de login com email e senha'},
            {'id': 'register', 'name': 'Registro', 'description': 'Formulario de cadastro de usuario'},
            {'id': 'contact', 'name': 'Contato', 'description': 'Formulario de contato'},
            {'id': 'dashboard', 'name': 'Dashboard', 'description': 'Dashboard com cards de metricas'},
            {'id': 'crud', 'name': 'CRUD', 'description': 'Tabela com acoes de criar/editar/deletar'},
            {'id': 'profile', 'name': 'Perfil', 'description': 'Pagina de perfil de usuario'},
        ]

    def _template_login(self) -> DesignResult:
        """Template de login"""
        return DesignResult(
            intent=DesignIntent.AUTH,
            title='Login',
            description='Formulario de autenticacao',
            components=[
                GeneratedComponent('text', {'content': 'Entrar', 'variant': 'h2', 'align': 'center'},
                                 {'x': 100, 'y': 20, 'width': 200, 'height': 50}),
                GeneratedComponent('input', {'label': 'Email', 'type': 'email', 'placeholder': 'seu@email.com'},
                                 {'x': 50, 'y': 90, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Senha', 'type': 'password', 'placeholder': '********'},
                                 {'x': 50, 'y': 170, 'width': 300, 'height': 70}),
                GeneratedComponent('checkbox', {'label': 'Lembrar de mim'},
                                 {'x': 50, 'y': 250, 'width': 200, 'height': 30}),
                GeneratedComponent('button', {'text': 'Entrar', 'variant': 'primary', 'fullWidth': True},
                                 {'x': 50, 'y': 300, 'width': 300, 'height': 45}),
                GeneratedComponent('link', {'text': 'Esqueci minha senha', 'href': '#'},
                                 {'x': 120, 'y': 360, 'width': 150, 'height': 30}),
            ],
            layout_suggestion='centered',
            confidence=1.0
        )

    def _template_register(self) -> DesignResult:
        """Template de registro"""
        return DesignResult(
            intent=DesignIntent.FORM,
            title='Criar Conta',
            description='Formulario de cadastro',
            components=[
                GeneratedComponent('text', {'content': 'Criar Conta', 'variant': 'h2'},
                                 {'x': 50, 'y': 20, 'width': 300, 'height': 50}),
                GeneratedComponent('input', {'label': 'Nome completo', 'type': 'text', 'placeholder': 'Seu nome'},
                                 {'x': 50, 'y': 80, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Email', 'type': 'email', 'placeholder': 'seu@email.com'},
                                 {'x': 50, 'y': 160, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Senha', 'type': 'password', 'placeholder': '********'},
                                 {'x': 50, 'y': 240, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Confirmar senha', 'type': 'password', 'placeholder': '********'},
                                 {'x': 50, 'y': 320, 'width': 300, 'height': 70}),
                GeneratedComponent('checkbox', {'label': 'Aceito os termos de uso'},
                                 {'x': 50, 'y': 400, 'width': 250, 'height': 30}),
                GeneratedComponent('button', {'text': 'Criar conta', 'variant': 'primary', 'fullWidth': True},
                                 {'x': 50, 'y': 450, 'width': 300, 'height': 45}),
            ],
            layout_suggestion='centered',
            confidence=1.0
        )

    def _template_contact(self) -> DesignResult:
        """Template de contato"""
        return DesignResult(
            intent=DesignIntent.FORM,
            title='Contato',
            description='Formulario de contato',
            components=[
                GeneratedComponent('text', {'content': 'Entre em Contato', 'variant': 'h2'},
                                 {'x': 50, 'y': 20, 'width': 300, 'height': 50}),
                GeneratedComponent('text', {'content': 'Preencha o formulario abaixo', 'variant': 'body', 'color': '#6b7280'},
                                 {'x': 50, 'y': 70, 'width': 300, 'height': 30}),
                GeneratedComponent('input', {'label': 'Nome', 'type': 'text'},
                                 {'x': 50, 'y': 110, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Email', 'type': 'email'},
                                 {'x': 50, 'y': 190, 'width': 300, 'height': 70}),
                GeneratedComponent('select', {'label': 'Assunto', 'options': [
                    {'value': 'duvida', 'label': 'Duvida'},
                    {'value': 'sugestao', 'label': 'Sugestao'},
                    {'value': 'reclamacao', 'label': 'Reclamacao'},
                    {'value': 'outro', 'label': 'Outro'}
                ]}, {'x': 50, 'y': 270, 'width': 300, 'height': 70}),
                GeneratedComponent('textarea', {'label': 'Mensagem', 'rows': 5},
                                 {'x': 50, 'y': 350, 'width': 300, 'height': 140}),
                GeneratedComponent('button', {'text': 'Enviar mensagem', 'variant': 'primary'},
                                 {'x': 50, 'y': 510, 'width': 300, 'height': 45}),
            ],
            layout_suggestion='vertical',
            confidence=1.0
        )

    def _template_dashboard(self) -> DesignResult:
        """Template de dashboard"""
        return DesignResult(
            intent=DesignIntent.DASHBOARD,
            title='Dashboard',
            description='Painel com metricas',
            components=[
                GeneratedComponent('text', {'content': 'Dashboard', 'variant': 'h1'},
                                 {'x': 20, 'y': 20, 'width': 200, 'height': 50}),
                GeneratedComponent('card', {'title': 'Usuarios', 'shadow': 'md'},
                                 {'x': 20, 'y': 90, 'width': 200, 'height': 120}),
                GeneratedComponent('card', {'title': 'Vendas', 'shadow': 'md'},
                                 {'x': 240, 'y': 90, 'width': 200, 'height': 120}),
                GeneratedComponent('card', {'title': 'Receita', 'shadow': 'md'},
                                 {'x': 460, 'y': 90, 'width': 200, 'height': 120}),
                GeneratedComponent('card', {'title': 'Conversao', 'shadow': 'md'},
                                 {'x': 680, 'y': 90, 'width': 200, 'height': 120}),
                GeneratedComponent('table', {
                    'columns': [
                        {'key': 'nome', 'label': 'Cliente'},
                        {'key': 'valor', 'label': 'Valor'},
                        {'key': 'status', 'label': 'Status'}
                    ],
                    'striped': True
                }, {'x': 20, 'y': 230, 'width': 860, 'height': 300}),
            ],
            layout_suggestion='grid',
            confidence=1.0
        )

    def _template_crud(self) -> DesignResult:
        """Template CRUD"""
        return DesignResult(
            intent=DesignIntent.CRUD,
            title='Gerenciamento',
            description='CRUD de registros',
            components=[
                GeneratedComponent('text', {'content': 'Gerenciar Registros', 'variant': 'h2'},
                                 {'x': 20, 'y': 20, 'width': 300, 'height': 50}),
                GeneratedComponent('button', {'text': '+ Novo', 'variant': 'primary'},
                                 {'x': 700, 'y': 20, 'width': 100, 'height': 40}),
                GeneratedComponent('input', {'placeholder': 'Buscar...', 'type': 'text'},
                                 {'x': 20, 'y': 80, 'width': 300, 'height': 45}),
                GeneratedComponent('table', {
                    'columns': [
                        {'key': 'id', 'label': 'ID'},
                        {'key': 'nome', 'label': 'Nome'},
                        {'key': 'email', 'label': 'Email'},
                        {'key': 'status', 'label': 'Status'},
                        {'key': 'acoes', 'label': 'Acoes'}
                    ],
                    'striped': True,
                    'pagination': True
                }, {'x': 20, 'y': 140, 'width': 780, 'height': 400}),
            ],
            layout_suggestion='full-width',
            confidence=1.0
        )

    def _template_profile(self) -> DesignResult:
        """Template de perfil"""
        return DesignResult(
            intent=DesignIntent.FORM,
            title='Meu Perfil',
            description='Pagina de perfil do usuario',
            components=[
                GeneratedComponent('avatar', {'name': 'Usuario', 'size': 'xl'},
                                 {'x': 150, 'y': 20, 'width': 100, 'height': 100}),
                GeneratedComponent('text', {'content': 'Meu Perfil', 'variant': 'h2', 'align': 'center'},
                                 {'x': 100, 'y': 130, 'width': 200, 'height': 40}),
                GeneratedComponent('divider', {},
                                 {'x': 50, 'y': 180, 'width': 300, 'height': 20}),
                GeneratedComponent('input', {'label': 'Nome', 'type': 'text'},
                                 {'x': 50, 'y': 210, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Email', 'type': 'email'},
                                 {'x': 50, 'y': 290, 'width': 300, 'height': 70}),
                GeneratedComponent('input', {'label': 'Telefone', 'type': 'tel'},
                                 {'x': 50, 'y': 370, 'width': 300, 'height': 70}),
                GeneratedComponent('textarea', {'label': 'Bio', 'rows': 3},
                                 {'x': 50, 'y': 450, 'width': 300, 'height': 100}),
                GeneratedComponent('button', {'text': 'Salvar alteracoes', 'variant': 'primary'},
                                 {'x': 50, 'y': 570, 'width': 300, 'height': 45}),
            ],
            layout_suggestion='centered',
            confidence=1.0
        )


# Instancia global
ai_design_generator = AIDesignGenerator()
