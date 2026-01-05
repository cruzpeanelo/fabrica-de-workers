"""
Component Renderer - Renderiza componentes para diferentes frameworks

Gera codigo React, Vue ou HTML puro a partir da definicao do componente
e suas propriedades configuradas.
"""

import json
import re
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from enum import Enum

from .component_registry import ComponentRegistry, ComponentDefinition


class OutputFormat(str, Enum):
    """Formatos de saida suportados"""
    REACT = "react"
    VUE = "vue"
    HTML = "html"
    JSON = "json"


@dataclass
class ComponentInstance:
    """Instancia de um componente no canvas"""
    id: str
    component_id: str
    props: Dict[str, Any]
    children: List['ComponentInstance'] = None
    position: Dict[str, int] = None  # {x, y, width, height}
    parent_id: Optional[str] = None

    def __post_init__(self):
        if self.children is None:
            self.children = []
        if self.position is None:
            self.position = {'x': 0, 'y': 0, 'width': 200, 'height': 50}

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'component_id': self.component_id,
            'props': self.props,
            'children': [c.to_dict() for c in self.children],
            'position': self.position,
            'parent_id': self.parent_id
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'ComponentInstance':
        children = [cls.from_dict(c) for c in data.get('children', [])]
        return cls(
            id=data['id'],
            component_id=data['component_id'],
            props=data.get('props', {}),
            children=children,
            position=data.get('position'),
            parent_id=data.get('parent_id')
        )


class ComponentRenderer:
    """Renderiza componentes para diferentes formatos de saida"""

    def __init__(self):
        self.registry = ComponentRegistry()

    def render(self, instance: ComponentInstance, format: OutputFormat = OutputFormat.REACT) -> str:
        """Renderiza uma instancia de componente"""
        if format == OutputFormat.REACT:
            return self._render_react(instance)
        elif format == OutputFormat.VUE:
            return self._render_vue(instance)
        elif format == OutputFormat.HTML:
            return self._render_html(instance)
        elif format == OutputFormat.JSON:
            return json.dumps(instance.to_dict(), indent=2)
        else:
            raise ValueError(f"Formato nao suportado: {format}")

    def render_tree(self, instances: List[ComponentInstance], format: OutputFormat = OutputFormat.REACT) -> str:
        """Renderiza uma arvore de componentes"""
        # Encontra componentes raiz (sem parent)
        roots = [i for i in instances if i.parent_id is None]

        if format == OutputFormat.REACT:
            return self._render_react_tree(roots)
        elif format == OutputFormat.VUE:
            return self._render_vue_tree(roots)
        elif format == OutputFormat.HTML:
            return self._render_html_tree(roots)
        else:
            return json.dumps([i.to_dict() for i in instances], indent=2)

    def _render_react(self, instance: ComponentInstance, indent: int = 0) -> str:
        """Renderiza para React/JSX"""
        definition = self.registry.get(instance.component_id)
        if not definition:
            return f"{{/* Componente desconhecido: {instance.component_id} */}}"

        indent_str = "  " * indent
        props_str = self._format_react_props(instance.props, definition)

        if definition.children_allowed and instance.children:
            children_str = "\n".join([
                self._render_react(child, indent + 1)
                for child in instance.children
            ])
            return f"""{indent_str}<{self._pascal_case(instance.component_id)}{props_str}>
{children_str}
{indent_str}</{self._pascal_case(instance.component_id)}>"""
        else:
            return f"{indent_str}<{self._pascal_case(instance.component_id)}{props_str} />"

    def _render_vue(self, instance: ComponentInstance, indent: int = 0) -> str:
        """Renderiza para Vue template"""
        definition = self.registry.get(instance.component_id)
        if not definition:
            return f"<!-- Componente desconhecido: {instance.component_id} -->"

        indent_str = "  " * indent
        props_str = self._format_vue_props(instance.props, definition)

        if definition.children_allowed and instance.children:
            children_str = "\n".join([
                self._render_vue(child, indent + 1)
                for child in instance.children
            ])
            return f"""{indent_str}<{self._pascal_case(instance.component_id)}{props_str}>
{children_str}
{indent_str}</{self._pascal_case(instance.component_id)}>"""
        else:
            return f"{indent_str}<{self._pascal_case(instance.component_id)}{props_str} />"

    def _render_html(self, instance: ComponentInstance, indent: int = 0) -> str:
        """Renderiza para HTML puro"""
        definition = self.registry.get(instance.component_id)
        if not definition:
            return f"<!-- Componente desconhecido: {instance.component_id} -->"

        indent_str = "  " * indent

        # Usa o template HTML e substitui as variaveis
        html = definition.preview_html
        for key, value in instance.props.items():
            html = html.replace(f"{{{key}}}", str(value) if value else "")

        # Substitui placeholders de children
        if definition.children_allowed and instance.children:
            children_html = "\n".join([
                self._render_html(child, indent + 1)
                for child in instance.children
            ])
            html = html.replace("[Conteudo]", children_html)
            html = html.replace("[Campos aqui]", children_html)
            html = html.replace("[Items]", children_html)
            html = html.replace("{children}", children_html)

        # Adiciona indentacao
        lines = html.split("\n")
        return "\n".join([f"{indent_str}{line}" for line in lines])

    def _render_react_tree(self, roots: List[ComponentInstance]) -> str:
        """Renderiza arvore completa para React"""
        imports = self._generate_react_imports(roots)
        components = "\n".join([self._render_react(r, 2) for r in roots])

        return f"""import React from 'react';
{imports}

export default function GeneratedApp() {{
  return (
    <div className="app-container">
{components}
    </div>
  );
}}
"""

    def _render_vue_tree(self, roots: List[ComponentInstance]) -> str:
        """Renderiza arvore completa para Vue"""
        imports = self._generate_vue_imports(roots)
        components = "\n".join([self._render_vue(r, 2) for r in roots])

        return f"""<template>
  <div class="app-container">
{components}
  </div>
</template>

<script setup>
{imports}
</script>

<style scoped>
.app-container {{
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}}
</style>
"""

    def _render_html_tree(self, roots: List[ComponentInstance]) -> str:
        """Renderiza arvore completa para HTML"""
        components = "\n".join([self._render_html(r, 2) for r in roots])

        return f"""<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>App Gerado - Plataforma E</title>
  <link href="https://cdn.jsdelivr.net/npm/tailwindcss@2/dist/tailwind.min.css" rel="stylesheet">
  <style>
    .app-container {{ max-width: 1200px; margin: 0 auto; padding: 20px; }}
  </style>
</head>
<body>
  <div class="app-container">
{components}
  </div>
</body>
</html>
"""

    def _format_react_props(self, props: Dict[str, Any], definition: ComponentDefinition) -> str:
        """Formata props para React"""
        if not props:
            return ""

        parts = []
        for key, value in props.items():
            if value is None:
                continue

            prop_def = next((p for p in definition.props if p.name == key), None)

            if isinstance(value, bool):
                if value:
                    parts.append(f"{key}")
                # False props sao omitidas em React
            elif isinstance(value, (int, float)):
                parts.append(f"{key}={{{value}}}")
            elif isinstance(value, (list, dict)):
                parts.append(f"{key}={{{json.dumps(value)}}}")
            else:
                parts.append(f'{key}="{value}"')

        if parts:
            return " " + " ".join(parts)
        return ""

    def _format_vue_props(self, props: Dict[str, Any], definition: ComponentDefinition) -> str:
        """Formata props para Vue"""
        if not props:
            return ""

        parts = []
        for key, value in props.items():
            if value is None:
                continue

            if isinstance(value, bool):
                if value:
                    parts.append(f":{key}=\"true\"")
                else:
                    parts.append(f":{key}=\"false\"")
            elif isinstance(value, (int, float)):
                parts.append(f":{key}=\"{value}\"")
            elif isinstance(value, (list, dict)):
                parts.append(f":{key}=\"{json.dumps(value)}\"")
            else:
                parts.append(f'{key}="{value}"')

        if parts:
            return " " + " ".join(parts)
        return ""

    def _generate_react_imports(self, instances: List[ComponentInstance]) -> str:
        """Gera imports para React"""
        component_ids = set()
        self._collect_component_ids(instances, component_ids)

        components = [self._pascal_case(cid) for cid in component_ids]
        if components:
            return f"import {{ {', '.join(sorted(components))} }} from './components';"
        return ""

    def _generate_vue_imports(self, instances: List[ComponentInstance]) -> str:
        """Gera imports para Vue"""
        component_ids = set()
        self._collect_component_ids(instances, component_ids)

        imports = []
        for cid in sorted(component_ids):
            name = self._pascal_case(cid)
            imports.append(f"import {name} from './components/{name}.vue';")

        return "\n".join(imports)

    def _collect_component_ids(self, instances: List[ComponentInstance], ids: set):
        """Coleta todos os IDs de componentes recursivamente"""
        for instance in instances:
            ids.add(instance.component_id)
            if instance.children:
                self._collect_component_ids(instance.children, ids)

    def _pascal_case(self, text: str) -> str:
        """Converte para PascalCase"""
        return ''.join(word.capitalize() for word in text.replace('-', '_').split('_'))

    # === Code Generation ===

    def generate_full_react_app(self, instances: List[ComponentInstance], app_name: str = "GeneratedApp") -> Dict[str, str]:
        """Gera um app React completo"""
        files = {}

        # App principal
        files['src/App.jsx'] = self._render_react_tree(
            [i for i in instances if i.parent_id is None]
        )

        # Componentes individuais
        for instance in instances:
            definition = self.registry.get(instance.component_id)
            if definition:
                comp_name = self._pascal_case(instance.component_id)
                files[f'src/components/{comp_name}.jsx'] = self._generate_react_component(definition)

        # Index e package.json
        files['src/index.jsx'] = """import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import './index.css';

ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
"""

        files['package.json'] = json.dumps({
            "name": app_name.lower().replace(' ', '-'),
            "version": "1.0.0",
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "vite build",
                "preview": "vite preview"
            },
            "dependencies": {
                "react": "^18.2.0",
                "react-dom": "^18.2.0"
            },
            "devDependencies": {
                "@vitejs/plugin-react": "^4.0.0",
                "vite": "^4.4.0"
            }
        }, indent=2)

        files['vite.config.js'] = """import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
});
"""

        files['src/index.css'] = """@tailwind base;
@tailwind components;
@tailwind utilities;

.app-container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}
"""

        return files

    def generate_full_vue_app(self, instances: List[ComponentInstance], app_name: str = "GeneratedApp") -> Dict[str, str]:
        """Gera um app Vue completo"""
        files = {}

        # App principal
        files['src/App.vue'] = self._render_vue_tree(
            [i for i in instances if i.parent_id is None]
        )

        # Componentes individuais
        for instance in instances:
            definition = self.registry.get(instance.component_id)
            if definition:
                comp_name = self._pascal_case(instance.component_id)
                files[f'src/components/{comp_name}.vue'] = self._generate_vue_component(definition)

        # Main e package.json
        files['src/main.js'] = """import { createApp } from 'vue';
import App from './App.vue';
import './style.css';

createApp(App).mount('#app');
"""

        files['package.json'] = json.dumps({
            "name": app_name.lower().replace(' ', '-'),
            "version": "1.0.0",
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "vite build",
                "preview": "vite preview"
            },
            "dependencies": {
                "vue": "^3.3.0"
            },
            "devDependencies": {
                "@vitejs/plugin-vue": "^4.0.0",
                "vite": "^4.4.0"
            }
        }, indent=2)

        files['vite.config.js'] = """import { defineConfig } from 'vite';
import vue from '@vitejs/plugin-vue';

export default defineConfig({
  plugins: [vue()],
});
"""

        files['src/style.css'] = """@tailwind base;
@tailwind components;
@tailwind utilities;
"""

        return files

    def _generate_react_component(self, definition: ComponentDefinition) -> str:
        """Gera arquivo de componente React"""
        name = self._pascal_case(definition.id)
        props_interface = self._generate_props_interface(definition)

        return f"""import React from 'react';

{props_interface}

export function {name}({{ {', '.join(p.name for p in definition.props)} }}) {{
  return (
    {definition.preview_html}
  );
}}

export default {name};
"""

    def _generate_vue_component(self, definition: ComponentDefinition) -> str:
        """Gera arquivo de componente Vue"""
        name = self._pascal_case(definition.id)
        props_def = ", ".join([
            f"'{p.name}'" for p in definition.props
        ])

        return f"""<template>
  {definition.preview_html}
</template>

<script setup>
defineProps([{props_def}]);
</script>

<style scoped>
/* Estilos do componente */
</style>
"""

    def _generate_props_interface(self, definition: ComponentDefinition) -> str:
        """Gera interface TypeScript para props"""
        props = []
        for prop in definition.props:
            ts_type = "string"
            if prop.type.value == "number":
                ts_type = "number"
            elif prop.type.value == "boolean":
                ts_type = "boolean"
            elif prop.type.value in ["array", "object"]:
                ts_type = "any"

            optional = "?" if not prop.required else ""
            props.append(f"  {prop.name}{optional}: {ts_type};")

        name = self._pascal_case(definition.id)
        return f"""interface {name}Props {{
{chr(10).join(props)}
}}"""


# Instancia global
renderer = ComponentRenderer()
