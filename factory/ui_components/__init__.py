"""
UI Components Library - Plataforma E Visual Builder

Biblioteca de componentes reutilizaveis para o Visual Builder.
Inspirado no Base44.app para criacao de apps com IA.
"""

from .component_registry import ComponentRegistry, ComponentDefinition
from .component_renderer import ComponentRenderer

__all__ = [
    'ComponentRegistry',
    'ComponentDefinition',
    'ComponentRenderer',
    'COMPONENT_CATEGORIES'
]

# Categorias de componentes disponiveis
COMPONENT_CATEGORIES = {
    'primitives': {
        'name': 'Primitivos',
        'icon': 'cube',
        'description': 'Componentes basicos: Button, Input, Text, Image'
    },
    'forms': {
        'name': 'Formularios',
        'icon': 'clipboard',
        'description': 'Form, campos de entrada, validacao'
    },
    'layout': {
        'name': 'Layout',
        'icon': 'grid',
        'description': 'Grid, Flex, Card, Container'
    },
    'data': {
        'name': 'Dados',
        'icon': 'table',
        'description': 'Table, List, Chart'
    },
    'feedback': {
        'name': 'Feedback',
        'icon': 'bell',
        'description': 'Modal, Toast, Alert, Loading'
    },
    'navigation': {
        'name': 'Navegacao',
        'icon': 'menu',
        'description': 'Navbar, Sidebar, Tabs, Breadcrumb'
    }
}
