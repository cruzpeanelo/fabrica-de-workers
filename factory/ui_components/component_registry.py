"""
Component Registry - Registro central de componentes UI

Gerencia todos os componentes disponiveis no Visual Builder,
suas propriedades, eventos e templates de codigo.
"""

from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum


class PropType(str, Enum):
    """Tipos de propriedades suportadas"""
    STRING = "string"
    NUMBER = "number"
    BOOLEAN = "boolean"
    COLOR = "color"
    SELECT = "select"
    ICON = "icon"
    SIZE = "size"
    SPACING = "spacing"
    EVENT = "event"
    ARRAY = "array"
    OBJECT = "object"


@dataclass
class PropDefinition:
    """Definicao de uma propriedade de componente"""
    name: str
    type: PropType
    label: str
    default: Any = None
    required: bool = False
    options: List[str] = field(default_factory=list)  # Para SELECT
    description: str = ""
    group: str = "general"  # general, style, advanced


@dataclass
class EventDefinition:
    """Definicao de um evento de componente"""
    name: str
    label: str
    description: str = ""
    payload_type: str = "void"


@dataclass
class ComponentDefinition:
    """Definicao completa de um componente UI"""
    id: str
    name: str
    category: str
    icon: str
    description: str
    props: List[PropDefinition] = field(default_factory=list)
    events: List[EventDefinition] = field(default_factory=list)
    children_allowed: bool = False
    default_children: List[str] = field(default_factory=list)
    template_react: str = ""
    template_vue: str = ""
    template_html: str = ""
    preview_html: str = ""

    def to_dict(self) -> Dict:
        """Converte para dicionario (para API)"""
        return {
            'id': self.id,
            'name': self.name,
            'category': self.category,
            'icon': self.icon,
            'description': self.description,
            'props': [
                {
                    'name': p.name,
                    'type': p.type.value,
                    'label': p.label,
                    'default': p.default,
                    'required': p.required,
                    'options': p.options,
                    'description': p.description,
                    'group': p.group
                } for p in self.props
            ],
            'events': [
                {
                    'name': e.name,
                    'label': e.label,
                    'description': e.description
                } for e in self.events
            ],
            'children_allowed': self.children_allowed,
            'preview_html': self.preview_html
        }


class ComponentRegistry:
    """Registro central de todos os componentes disponiveis"""

    _instance = None
    _components: Dict[str, ComponentDefinition] = {}

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._load_default_components()
        return cls._instance

    def _load_default_components(self):
        """Carrega componentes padrao"""
        # Primitivos
        self._register_primitives()
        # Forms
        self._register_forms()
        # Layout
        self._register_layout()
        # Data
        self._register_data()
        # Feedback
        self._register_feedback()
        # Navigation
        self._register_navigation()

    def _register_primitives(self):
        """Registra componentes primitivos"""

        # Button
        self.register(ComponentDefinition(
            id='button',
            name='Button',
            category='primitives',
            icon='cursor-click',
            description='Botao clicavel com diferentes estilos',
            props=[
                PropDefinition('text', PropType.STRING, 'Texto', 'Clique aqui', True),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'primary',
                              options=['primary', 'secondary', 'outline', 'ghost', 'danger']),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['sm', 'md', 'lg']),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
                PropDefinition('fullWidth', PropType.BOOLEAN, 'Largura Total', False),
                PropDefinition('icon', PropType.ICON, 'Icone', '', group='style'),
            ],
            events=[
                EventDefinition('onClick', 'Ao Clicar', 'Executado quando o botao e clicado'),
            ],
            template_react='<Button variant="{variant}" size="{size}" onClick={{onClick}}>{text}</Button>',
            template_vue='<Button :variant="{variant}" :size="{size}" @click="onClick">{text}</Button>',
            preview_html='<button class="btn btn-{variant} btn-{size}">{text}</button>'
        ))

        # Text/Label
        self.register(ComponentDefinition(
            id='text',
            name='Text',
            category='primitives',
            icon='font',
            description='Texto ou paragrafo',
            props=[
                PropDefinition('content', PropType.STRING, 'Conteudo', 'Texto aqui', True),
                PropDefinition('variant', PropType.SELECT, 'Tipo', 'body',
                              options=['h1', 'h2', 'h3', 'h4', 'body', 'caption', 'label']),
                PropDefinition('color', PropType.COLOR, 'Cor', '#1f2937', group='style'),
                PropDefinition('align', PropType.SELECT, 'Alinhamento', 'left',
                              options=['left', 'center', 'right', 'justify']),
                PropDefinition('weight', PropType.SELECT, 'Peso', 'normal',
                              options=['light', 'normal', 'medium', 'semibold', 'bold']),
            ],
            template_react='<Text variant="{variant}" color="{color}" align="{align}">{content}</Text>',
            template_vue='<Text :variant="{variant}" :color="{color}" :align="{align}">{content}</Text>',
            preview_html='<span class="text-{variant}" style="color:{color};text-align:{align}">{content}</span>'
        ))

        # Image
        self.register(ComponentDefinition(
            id='image',
            name='Image',
            category='primitives',
            icon='photo',
            description='Imagem com suporte a diferentes formatos',
            props=[
                PropDefinition('src', PropType.STRING, 'URL da Imagem', 'https://via.placeholder.com/150', True),
                PropDefinition('alt', PropType.STRING, 'Texto Alternativo', 'Imagem'),
                PropDefinition('width', PropType.STRING, 'Largura', '100%', group='style'),
                PropDefinition('height', PropType.STRING, 'Altura', 'auto', group='style'),
                PropDefinition('objectFit', PropType.SELECT, 'Ajuste', 'cover',
                              options=['cover', 'contain', 'fill', 'none']),
                PropDefinition('borderRadius', PropType.STRING, 'Borda Arredondada', '0', group='style'),
            ],
            template_react='<img src="{src}" alt="{alt}" style={{width: "{width}", height: "{height}", objectFit: "{objectFit}"}} />',
            template_vue='<img :src="{src}" :alt="{alt}" :style="{width: \'{width}\', height: \'{height}\', objectFit: \'{objectFit}\'}" />',
            preview_html='<img src="{src}" alt="{alt}" style="width:{width};height:{height};object-fit:{objectFit};border-radius:{borderRadius}" />'
        ))

        # Icon
        self.register(ComponentDefinition(
            id='icon',
            name='Icon',
            category='primitives',
            icon='star',
            description='Icone de biblioteca (Heroicons)',
            props=[
                PropDefinition('name', PropType.ICON, 'Icone', 'star', True),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['xs', 'sm', 'md', 'lg', 'xl']),
                PropDefinition('color', PropType.COLOR, 'Cor', '#6b7280', group='style'),
            ],
            template_react='<Icon name="{name}" size="{size}" color="{color}" />',
            template_vue='<Icon :name="{name}" :size="{size}" :color="{color}" />',
            preview_html='<span class="icon icon-{name}" style="font-size:{size};color:{color}">*</span>'
        ))

        # Link
        self.register(ComponentDefinition(
            id='link',
            name='Link',
            category='primitives',
            icon='link',
            description='Link para navegacao',
            props=[
                PropDefinition('text', PropType.STRING, 'Texto', 'Clique aqui', True),
                PropDefinition('href', PropType.STRING, 'URL', '#', True),
                PropDefinition('target', PropType.SELECT, 'Abrir em', '_self',
                              options=['_self', '_blank']),
                PropDefinition('color', PropType.COLOR, 'Cor', '#3b82f6', group='style'),
            ],
            events=[
                EventDefinition('onClick', 'Ao Clicar', 'Executado quando o link e clicado'),
            ],
            template_react='<a href="{href}" target="{target}" style={{color: "{color}"}}>{text}</a>',
            template_vue='<a :href="{href}" :target="{target}" :style="{color: \'{color}\'}">{text}</a>',
            preview_html='<a href="{href}" target="{target}" style="color:{color}">{text}</a>'
        ))

        # Divider
        self.register(ComponentDefinition(
            id='divider',
            name='Divider',
            category='primitives',
            icon='minus',
            description='Linha divisoria',
            props=[
                PropDefinition('orientation', PropType.SELECT, 'Orientacao', 'horizontal',
                              options=['horizontal', 'vertical']),
                PropDefinition('color', PropType.COLOR, 'Cor', '#e5e7eb', group='style'),
                PropDefinition('thickness', PropType.STRING, 'Espessura', '1px', group='style'),
                PropDefinition('margin', PropType.STRING, 'Margem', '16px 0', group='style'),
            ],
            template_react='<Divider orientation="{orientation}" />',
            template_vue='<Divider :orientation="{orientation}" />',
            preview_html='<hr style="border-color:{color};border-width:{thickness};margin:{margin}" />'
        ))

    def _register_forms(self):
        """Registra componentes de formulario"""

        # Input
        self.register(ComponentDefinition(
            id='input',
            name='Input',
            category='forms',
            icon='pencil',
            description='Campo de entrada de texto',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Campo'),
                PropDefinition('placeholder', PropType.STRING, 'Placeholder', 'Digite aqui...'),
                PropDefinition('type', PropType.SELECT, 'Tipo', 'text',
                              options=['text', 'email', 'password', 'number', 'tel', 'url']),
                PropDefinition('required', PropType.BOOLEAN, 'Obrigatorio', False),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
                PropDefinition('helperText', PropType.STRING, 'Texto de Ajuda', ''),
                PropDefinition('error', PropType.STRING, 'Mensagem de Erro', ''),
            ],
            events=[
                EventDefinition('onChange', 'Ao Alterar', 'Valor do campo alterado'),
                EventDefinition('onBlur', 'Ao Sair', 'Foco removido do campo'),
            ],
            template_react='<Input label="{label}" type="{type}" placeholder="{placeholder}" onChange={{onChange}} />',
            template_vue='<Input :label="{label}" :type="{type}" :placeholder="{placeholder}" @change="onChange" />',
            preview_html='''<div class="form-group">
                <label>{label}</label>
                <input type="{type}" placeholder="{placeholder}" class="form-control" />
                <small class="helper-text">{helperText}</small>
            </div>'''
        ))

        # Textarea
        self.register(ComponentDefinition(
            id='textarea',
            name='Textarea',
            category='forms',
            icon='document-text',
            description='Area de texto multilinha',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Descricao'),
                PropDefinition('placeholder', PropType.STRING, 'Placeholder', 'Digite sua mensagem...'),
                PropDefinition('rows', PropType.NUMBER, 'Linhas', 4),
                PropDefinition('required', PropType.BOOLEAN, 'Obrigatorio', False),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
                PropDefinition('maxLength', PropType.NUMBER, 'Max Caracteres', 0),
            ],
            events=[
                EventDefinition('onChange', 'Ao Alterar', 'Valor alterado'),
            ],
            template_react='<Textarea label="{label}" rows={{{rows}}} placeholder="{placeholder}" />',
            template_vue='<Textarea :label="{label}" :rows="{rows}" :placeholder="{placeholder}" />',
            preview_html='''<div class="form-group">
                <label>{label}</label>
                <textarea rows="{rows}" placeholder="{placeholder}" class="form-control"></textarea>
            </div>'''
        ))

        # Select/Dropdown
        self.register(ComponentDefinition(
            id='select',
            name='Select',
            category='forms',
            icon='selector',
            description='Menu dropdown de selecao',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Selecione'),
                PropDefinition('placeholder', PropType.STRING, 'Placeholder', 'Escolha uma opcao'),
                PropDefinition('options', PropType.ARRAY, 'Opcoes',
                              default=[{'value': '1', 'label': 'Opcao 1'}, {'value': '2', 'label': 'Opcao 2'}]),
                PropDefinition('required', PropType.BOOLEAN, 'Obrigatorio', False),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
                PropDefinition('multiple', PropType.BOOLEAN, 'Multipla Selecao', False),
            ],
            events=[
                EventDefinition('onChange', 'Ao Selecionar', 'Opcao selecionada'),
            ],
            template_react='<Select label="{label}" options={{options}} onChange={{onChange}} />',
            template_vue='<Select :label="{label}" :options="options" @change="onChange" />',
            preview_html='''<div class="form-group">
                <label>{label}</label>
                <select class="form-control">
                    <option value="">{placeholder}</option>
                </select>
            </div>'''
        ))

        # Checkbox
        self.register(ComponentDefinition(
            id='checkbox',
            name='Checkbox',
            category='forms',
            icon='check-circle',
            description='Caixa de selecao',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Aceito os termos'),
                PropDefinition('checked', PropType.BOOLEAN, 'Marcado', False),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
            ],
            events=[
                EventDefinition('onChange', 'Ao Alterar', 'Estado alterado'),
            ],
            template_react='<Checkbox label="{label}" checked={{{checked}}} onChange={{onChange}} />',
            template_vue='<Checkbox :label="{label}" :checked="{checked}" @change="onChange" />',
            preview_html='<label class="checkbox"><input type="checkbox" /> {label}</label>'
        ))

        # Radio Group
        self.register(ComponentDefinition(
            id='radio',
            name='Radio Group',
            category='forms',
            icon='stop-circle',
            description='Grupo de opcoes exclusivas',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Escolha uma opcao'),
                PropDefinition('options', PropType.ARRAY, 'Opcoes',
                              default=[{'value': 'a', 'label': 'Opcao A'}, {'value': 'b', 'label': 'Opcao B'}]),
                PropDefinition('direction', PropType.SELECT, 'Direcao', 'vertical',
                              options=['vertical', 'horizontal']),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
            ],
            events=[
                EventDefinition('onChange', 'Ao Selecionar', 'Opcao alterada'),
            ],
            template_react='<RadioGroup label="{label}" options={{options}} onChange={{onChange}} />',
            template_vue='<RadioGroup :label="{label}" :options="options" @change="onChange" />',
            preview_html='''<div class="radio-group">
                <label>{label}</label>
                <div><input type="radio" name="radio" /> Opcao A</div>
                <div><input type="radio" name="radio" /> Opcao B</div>
            </div>'''
        ))

        # Switch/Toggle
        self.register(ComponentDefinition(
            id='switch',
            name='Switch',
            category='forms',
            icon='switch-horizontal',
            description='Interruptor on/off',
            props=[
                PropDefinition('label', PropType.STRING, 'Label', 'Ativar'),
                PropDefinition('checked', PropType.BOOLEAN, 'Ativo', False),
                PropDefinition('disabled', PropType.BOOLEAN, 'Desabilitado', False),
            ],
            events=[
                EventDefinition('onChange', 'Ao Alterar', 'Estado alterado'),
            ],
            template_react='<Switch label="{label}" checked={{{checked}}} onChange={{onChange}} />',
            template_vue='<Switch :label="{label}" :checked="{checked}" @change="onChange" />',
            preview_html='<label class="switch"><input type="checkbox" /><span class="slider"></span> {label}</label>'
        ))

        # Form Container
        self.register(ComponentDefinition(
            id='form',
            name='Form',
            category='forms',
            icon='clipboard-list',
            description='Container de formulario',
            props=[
                PropDefinition('title', PropType.STRING, 'Titulo', 'Formulario'),
                PropDefinition('submitText', PropType.STRING, 'Texto do Botao', 'Enviar'),
                PropDefinition('showReset', PropType.BOOLEAN, 'Mostrar Reset', False),
            ],
            events=[
                EventDefinition('onSubmit', 'Ao Enviar', 'Formulario enviado'),
                EventDefinition('onReset', 'Ao Limpar', 'Formulario resetado'),
            ],
            children_allowed=True,
            template_react='<Form onSubmit={{onSubmit}}>{children}</Form>',
            template_vue='<Form @submit="onSubmit">{children}</Form>',
            preview_html='<form class="form"><h3>{title}</h3><div class="form-children">[Campos aqui]</div><button type="submit">{submitText}</button></form>'
        ))

    def _register_layout(self):
        """Registra componentes de layout"""

        # Container
        self.register(ComponentDefinition(
            id='container',
            name='Container',
            category='layout',
            icon='template',
            description='Container com largura maxima',
            props=[
                PropDefinition('maxWidth', PropType.SELECT, 'Largura Maxima', 'lg',
                              options=['sm', 'md', 'lg', 'xl', 'full']),
                PropDefinition('padding', PropType.STRING, 'Padding', '16px', group='style'),
                PropDefinition('centered', PropType.BOOLEAN, 'Centralizado', True),
            ],
            children_allowed=True,
            template_react='<Container maxWidth="{maxWidth}">{children}</Container>',
            template_vue='<Container :maxWidth="{maxWidth}">{children}</Container>',
            preview_html='<div class="container container-{maxWidth}" style="padding:{padding}">[Conteudo]</div>'
        ))

        # Card
        self.register(ComponentDefinition(
            id='card',
            name='Card',
            category='layout',
            icon='rectangle-group',
            description='Cartao com sombra e bordas',
            props=[
                PropDefinition('title', PropType.STRING, 'Titulo', ''),
                PropDefinition('subtitle', PropType.STRING, 'Subtitulo', ''),
                PropDefinition('shadow', PropType.SELECT, 'Sombra', 'md',
                              options=['none', 'sm', 'md', 'lg', 'xl']),
                PropDefinition('padding', PropType.STRING, 'Padding', '16px', group='style'),
                PropDefinition('borderRadius', PropType.STRING, 'Borda', '8px', group='style'),
                PropDefinition('hoverable', PropType.BOOLEAN, 'Hover Effect', False),
            ],
            children_allowed=True,
            template_react='<Card title="{title}" shadow="{shadow}">{children}</Card>',
            template_vue='<Card :title="{title}" :shadow="{shadow}">{children}</Card>',
            preview_html='''<div class="card shadow-{shadow}" style="padding:{padding};border-radius:{borderRadius}">
                <div class="card-header">{title}</div>
                <div class="card-body">[Conteudo]</div>
            </div>'''
        ))

        # Grid
        self.register(ComponentDefinition(
            id='grid',
            name='Grid',
            category='layout',
            icon='view-grid',
            description='Grid responsivo de colunas',
            props=[
                PropDefinition('columns', PropType.NUMBER, 'Colunas', 2),
                PropDefinition('gap', PropType.STRING, 'Espacamento', '16px', group='style'),
                PropDefinition('columnsMobile', PropType.NUMBER, 'Colunas (Mobile)', 1),
            ],
            children_allowed=True,
            template_react='<Grid columns={{{columns}}} gap="{gap}">{children}</Grid>',
            template_vue='<Grid :columns="{columns}" :gap="{gap}">{children}</Grid>',
            preview_html='<div class="grid" style="display:grid;grid-template-columns:repeat({columns},1fr);gap:{gap}">[Items]</div>'
        ))

        # Flex
        self.register(ComponentDefinition(
            id='flex',
            name='Flex',
            category='layout',
            icon='view-list',
            description='Container flexbox',
            props=[
                PropDefinition('direction', PropType.SELECT, 'Direcao', 'row',
                              options=['row', 'column', 'row-reverse', 'column-reverse']),
                PropDefinition('justify', PropType.SELECT, 'Justify', 'flex-start',
                              options=['flex-start', 'center', 'flex-end', 'space-between', 'space-around']),
                PropDefinition('align', PropType.SELECT, 'Align', 'stretch',
                              options=['flex-start', 'center', 'flex-end', 'stretch', 'baseline']),
                PropDefinition('gap', PropType.STRING, 'Gap', '8px', group='style'),
                PropDefinition('wrap', PropType.BOOLEAN, 'Wrap', False),
            ],
            children_allowed=True,
            template_react='<Flex direction="{direction}" justify="{justify}" align="{align}">{children}</Flex>',
            template_vue='<Flex :direction="{direction}" :justify="{justify}" :align="{align}">{children}</Flex>',
            preview_html='<div style="display:flex;flex-direction:{direction};justify-content:{justify};align-items:{align};gap:{gap}">[Items]</div>'
        ))

        # Spacer
        self.register(ComponentDefinition(
            id='spacer',
            name='Spacer',
            category='layout',
            icon='arrows-expand',
            description='Espacador flexivel',
            props=[
                PropDefinition('size', PropType.STRING, 'Tamanho', '16px'),
                PropDefinition('direction', PropType.SELECT, 'Direcao', 'vertical',
                              options=['vertical', 'horizontal', 'flex']),
            ],
            template_react='<Spacer size="{size}" />',
            template_vue='<Spacer :size="{size}" />',
            preview_html='<div class="spacer" style="height:{size};width:{size}"></div>'
        ))

    def _register_data(self):
        """Registra componentes de dados"""

        # Table
        self.register(ComponentDefinition(
            id='table',
            name='Table',
            category='data',
            icon='table',
            description='Tabela de dados',
            props=[
                PropDefinition('columns', PropType.ARRAY, 'Colunas',
                              default=[{'key': 'name', 'label': 'Nome'}, {'key': 'email', 'label': 'Email'}]),
                PropDefinition('data', PropType.ARRAY, 'Dados', default=[]),
                PropDefinition('striped', PropType.BOOLEAN, 'Listrado', True),
                PropDefinition('hoverable', PropType.BOOLEAN, 'Hover', True),
                PropDefinition('pagination', PropType.BOOLEAN, 'Paginacao', False),
                PropDefinition('pageSize', PropType.NUMBER, 'Items por Pagina', 10),
            ],
            events=[
                EventDefinition('onRowClick', 'Ao Clicar Linha', 'Linha clicada'),
            ],
            template_react='<Table columns={{columns}} data={{data}} />',
            template_vue='<Table :columns="columns" :data="data" />',
            preview_html='''<table class="table table-striped">
                <thead><tr><th>Nome</th><th>Email</th></tr></thead>
                <tbody><tr><td>Exemplo</td><td>exemplo@email.com</td></tr></tbody>
            </table>'''
        ))

        # List
        self.register(ComponentDefinition(
            id='list',
            name='List',
            category='data',
            icon='list-bullet',
            description='Lista de items',
            props=[
                PropDefinition('items', PropType.ARRAY, 'Items',
                              default=[{'title': 'Item 1'}, {'title': 'Item 2'}]),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'simple',
                              options=['simple', 'bordered', 'card']),
                PropDefinition('clickable', PropType.BOOLEAN, 'Clicavel', False),
            ],
            events=[
                EventDefinition('onItemClick', 'Ao Clicar Item', 'Item clicado'),
            ],
            template_react='<List items={{items}} variant="{variant}" />',
            template_vue='<List :items="items" :variant="{variant}" />',
            preview_html='<ul class="list list-{variant}"><li>Item 1</li><li>Item 2</li></ul>'
        ))

        # Badge
        self.register(ComponentDefinition(
            id='badge',
            name='Badge',
            category='data',
            icon='tag',
            description='Badge/Tag de status',
            props=[
                PropDefinition('text', PropType.STRING, 'Texto', 'Novo', True),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'default',
                              options=['default', 'primary', 'success', 'warning', 'danger', 'info']),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['sm', 'md', 'lg']),
                PropDefinition('rounded', PropType.BOOLEAN, 'Arredondado', True),
            ],
            template_react='<Badge variant="{variant}">{text}</Badge>',
            template_vue='<Badge :variant="{variant}">{text}</Badge>',
            preview_html='<span class="badge badge-{variant}">{text}</span>'
        ))

        # Avatar
        self.register(ComponentDefinition(
            id='avatar',
            name='Avatar',
            category='data',
            icon='user-circle',
            description='Avatar de usuario',
            props=[
                PropDefinition('src', PropType.STRING, 'URL da Imagem', ''),
                PropDefinition('name', PropType.STRING, 'Nome', 'Usuario'),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['xs', 'sm', 'md', 'lg', 'xl']),
                PropDefinition('shape', PropType.SELECT, 'Forma', 'circle',
                              options=['circle', 'square', 'rounded']),
            ],
            template_react='<Avatar src="{src}" name="{name}" size="{size}" />',
            template_vue='<Avatar :src="{src}" :name="{name}" :size="{size}" />',
            preview_html='<div class="avatar avatar-{size} avatar-{shape}">{name[0]}</div>'
        ))

        # Progress
        self.register(ComponentDefinition(
            id='progress',
            name='Progress',
            category='data',
            icon='chart-bar',
            description='Barra de progresso',
            props=[
                PropDefinition('value', PropType.NUMBER, 'Valor', 50),
                PropDefinition('max', PropType.NUMBER, 'Maximo', 100),
                PropDefinition('showLabel', PropType.BOOLEAN, 'Mostrar Label', True),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'primary',
                              options=['primary', 'success', 'warning', 'danger']),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['sm', 'md', 'lg']),
            ],
            template_react='<Progress value={{{value}}} max={{{max}}} />',
            template_vue='<Progress :value="{value}" :max="{max}" />',
            preview_html='<div class="progress"><div class="progress-bar bg-{variant}" style="width:{value}%">{value}%</div></div>'
        ))

    def _register_feedback(self):
        """Registra componentes de feedback"""

        # Alert
        self.register(ComponentDefinition(
            id='alert',
            name='Alert',
            category='feedback',
            icon='exclamation-circle',
            description='Mensagem de alerta',
            props=[
                PropDefinition('title', PropType.STRING, 'Titulo', ''),
                PropDefinition('message', PropType.STRING, 'Mensagem', 'Esta e uma mensagem de alerta.', True),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'info',
                              options=['info', 'success', 'warning', 'error']),
                PropDefinition('closable', PropType.BOOLEAN, 'Fechavel', False),
                PropDefinition('icon', PropType.BOOLEAN, 'Mostrar Icone', True),
            ],
            events=[
                EventDefinition('onClose', 'Ao Fechar', 'Alerta fechado'),
            ],
            template_react='<Alert variant="{variant}" title="{title}">{message}</Alert>',
            template_vue='<Alert :variant="{variant}" :title="{title}">{message}</Alert>',
            preview_html='<div class="alert alert-{variant}"><strong>{title}</strong> {message}</div>'
        ))

        # Modal
        self.register(ComponentDefinition(
            id='modal',
            name='Modal',
            category='feedback',
            icon='document-duplicate',
            description='Janela modal',
            props=[
                PropDefinition('title', PropType.STRING, 'Titulo', 'Modal', True),
                PropDefinition('open', PropType.BOOLEAN, 'Aberto', False),
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['sm', 'md', 'lg', 'xl', 'full']),
                PropDefinition('closable', PropType.BOOLEAN, 'Fechavel', True),
                PropDefinition('closeOnOverlay', PropType.BOOLEAN, 'Fechar no Overlay', True),
            ],
            events=[
                EventDefinition('onClose', 'Ao Fechar', 'Modal fechado'),
                EventDefinition('onConfirm', 'Ao Confirmar', 'Acao confirmada'),
            ],
            children_allowed=True,
            template_react='<Modal open={{{open}}} title="{title}" onClose={{onClose}}>{children}</Modal>',
            template_vue='<Modal :open="{open}" :title="{title}" @close="onClose">{children}</Modal>',
            preview_html='''<div class="modal-backdrop">
                <div class="modal modal-{size}">
                    <div class="modal-header"><h4>{title}</h4></div>
                    <div class="modal-body">[Conteudo]</div>
                </div>
            </div>'''
        ))

        # Spinner/Loading
        self.register(ComponentDefinition(
            id='spinner',
            name='Spinner',
            category='feedback',
            icon='arrow-path',
            description='Indicador de carregamento',
            props=[
                PropDefinition('size', PropType.SELECT, 'Tamanho', 'md',
                              options=['sm', 'md', 'lg']),
                PropDefinition('color', PropType.COLOR, 'Cor', '#3b82f6', group='style'),
                PropDefinition('label', PropType.STRING, 'Label', ''),
            ],
            template_react='<Spinner size="{size}" />',
            template_vue='<Spinner :size="{size}" />',
            preview_html='<div class="spinner spinner-{size}" style="border-color:{color}"></div>'
        ))

        # Tooltip
        self.register(ComponentDefinition(
            id='tooltip',
            name='Tooltip',
            category='feedback',
            icon='information-circle',
            description='Dica ao passar o mouse',
            props=[
                PropDefinition('content', PropType.STRING, 'Conteudo', 'Dica aqui', True),
                PropDefinition('position', PropType.SELECT, 'Posicao', 'top',
                              options=['top', 'bottom', 'left', 'right']),
                PropDefinition('trigger', PropType.SELECT, 'Gatilho', 'hover',
                              options=['hover', 'click', 'focus']),
            ],
            children_allowed=True,
            template_react='<Tooltip content="{content}" position="{position}">{children}</Tooltip>',
            template_vue='<Tooltip :content="{content}" :position="{position}">{children}</Tooltip>',
            preview_html='<span class="tooltip" data-tip="{content}">[Elemento]</span>'
        ))

    def _register_navigation(self):
        """Registra componentes de navegacao"""

        # Navbar
        self.register(ComponentDefinition(
            id='navbar',
            name='Navbar',
            category='navigation',
            icon='menu-alt-2',
            description='Barra de navegacao',
            props=[
                PropDefinition('brand', PropType.STRING, 'Logo/Brand', 'Meu App'),
                PropDefinition('brandLink', PropType.STRING, 'Link do Brand', '/'),
                PropDefinition('items', PropType.ARRAY, 'Items do Menu',
                              default=[{'label': 'Home', 'href': '/'}, {'label': 'Sobre', 'href': '/sobre'}]),
                PropDefinition('sticky', PropType.BOOLEAN, 'Fixo no Topo', False),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'light',
                              options=['light', 'dark', 'transparent']),
            ],
            template_react='<Navbar brand="{brand}" items={{items}} />',
            template_vue='<Navbar :brand="{brand}" :items="items" />',
            preview_html='''<nav class="navbar navbar-{variant}">
                <a class="navbar-brand" href="{brandLink}">{brand}</a>
                <ul class="navbar-nav"><li><a href="/">Home</a></li></ul>
            </nav>'''
        ))

        # Tabs
        self.register(ComponentDefinition(
            id='tabs',
            name='Tabs',
            category='navigation',
            icon='collection',
            description='Abas de navegacao',
            props=[
                PropDefinition('tabs', PropType.ARRAY, 'Abas',
                              default=[{'id': 'tab1', 'label': 'Aba 1'}, {'id': 'tab2', 'label': 'Aba 2'}]),
                PropDefinition('activeTab', PropType.STRING, 'Aba Ativa', 'tab1'),
                PropDefinition('variant', PropType.SELECT, 'Variante', 'line',
                              options=['line', 'enclosed', 'pills']),
            ],
            events=[
                EventDefinition('onTabChange', 'Ao Trocar Aba', 'Aba alterada'),
            ],
            children_allowed=True,
            template_react='<Tabs tabs={{tabs}} activeTab="{activeTab}" onTabChange={{onTabChange}}>{children}</Tabs>',
            template_vue='<Tabs :tabs="tabs" :activeTab="{activeTab}" @tabChange="onTabChange">{children}</Tabs>',
            preview_html='''<div class="tabs tabs-{variant}">
                <div class="tab-list"><button class="tab active">Aba 1</button><button class="tab">Aba 2</button></div>
                <div class="tab-panel">[Conteudo]</div>
            </div>'''
        ))

        # Breadcrumb
        self.register(ComponentDefinition(
            id='breadcrumb',
            name='Breadcrumb',
            category='navigation',
            icon='chevron-right',
            description='Navegacao hierarquica',
            props=[
                PropDefinition('items', PropType.ARRAY, 'Items',
                              default=[{'label': 'Home', 'href': '/'}, {'label': 'Pagina Atual'}]),
                PropDefinition('separator', PropType.STRING, 'Separador', '/'),
            ],
            template_react='<Breadcrumb items={{items}} separator="{separator}" />',
            template_vue='<Breadcrumb :items="items" :separator="{separator}" />',
            preview_html='<nav class="breadcrumb">Home {separator} Pagina Atual</nav>'
        ))

        # Sidebar
        self.register(ComponentDefinition(
            id='sidebar',
            name='Sidebar',
            category='navigation',
            icon='view-boards',
            description='Menu lateral',
            props=[
                PropDefinition('items', PropType.ARRAY, 'Items do Menu',
                              default=[{'icon': 'home', 'label': 'Home', 'href': '/'}]),
                PropDefinition('collapsed', PropType.BOOLEAN, 'Colapsado', False),
                PropDefinition('width', PropType.STRING, 'Largura', '250px', group='style'),
                PropDefinition('position', PropType.SELECT, 'Posicao', 'left',
                              options=['left', 'right']),
            ],
            events=[
                EventDefinition('onItemClick', 'Ao Clicar Item', 'Item clicado'),
            ],
            template_react='<Sidebar items={{items}} collapsed={{{collapsed}}} />',
            template_vue='<Sidebar :items="items" :collapsed="{collapsed}" />',
            preview_html='''<aside class="sidebar" style="width:{width}">
                <nav class="sidebar-nav"><a href="/">Home</a></nav>
            </aside>'''
        ))

        # Pagination
        self.register(ComponentDefinition(
            id='pagination',
            name='Pagination',
            category='navigation',
            icon='dots-horizontal',
            description='Navegacao de paginas',
            props=[
                PropDefinition('total', PropType.NUMBER, 'Total de Items', 100),
                PropDefinition('pageSize', PropType.NUMBER, 'Items por Pagina', 10),
                PropDefinition('currentPage', PropType.NUMBER, 'Pagina Atual', 1),
                PropDefinition('showPageSize', PropType.BOOLEAN, 'Mostrar Seletor', False),
            ],
            events=[
                EventDefinition('onPageChange', 'Ao Mudar Pagina', 'Pagina alterada'),
            ],
            template_react='<Pagination total={{{total}}} pageSize={{{pageSize}}} current={{{currentPage}}} onChange={{onPageChange}} />',
            template_vue='<Pagination :total="{total}" :pageSize="{pageSize}" :current="{currentPage}" @change="onPageChange" />',
            preview_html='<nav class="pagination"><button>&lt;</button><span>1 / 10</span><button>&gt;</button></nav>'
        ))

    def register(self, component: ComponentDefinition):
        """Registra um novo componente"""
        self._components[component.id] = component

    def get(self, component_id: str) -> Optional[ComponentDefinition]:
        """Busca um componente por ID"""
        return self._components.get(component_id)

    def get_all(self) -> List[ComponentDefinition]:
        """Retorna todos os componentes"""
        return list(self._components.values())

    def get_by_category(self, category: str) -> List[ComponentDefinition]:
        """Retorna componentes de uma categoria"""
        return [c for c in self._components.values() if c.category == category]

    def get_categories(self) -> Dict[str, List[ComponentDefinition]]:
        """Retorna componentes agrupados por categoria"""
        categories = {}
        for component in self._components.values():
            if component.category not in categories:
                categories[component.category] = []
            categories[component.category].append(component)
        return categories

    def to_dict(self) -> Dict:
        """Exporta todos os componentes como dicionario"""
        return {
            'components': [c.to_dict() for c in self._components.values()],
            'categories': list(set(c.category for c in self._components.values()))
        }


# Instancia global
registry = ComponentRegistry()
