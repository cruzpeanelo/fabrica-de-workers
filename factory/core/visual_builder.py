"""
Visual Builder Engine - Plataforma E

Engine para o editor visual drag-and-drop de componentes UI.
Inspirado no Base44.app para criacao de apps com IA.

Responsabilidades:
- Gerenciar estado do canvas (componentes, posicoes, selecao)
- Operacoes CRUD em componentes
- Undo/Redo
- Export para codigo
- Sincronizacao com banco de dados
"""

import uuid
import json
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum
from copy import deepcopy

from factory.ui_components.component_registry import ComponentRegistry
from factory.ui_components.component_renderer import (
    ComponentRenderer,
    ComponentInstance,
    OutputFormat
)


class CanvasAction(str, Enum):
    """Tipos de acoes no canvas"""
    ADD = "add"
    REMOVE = "remove"
    UPDATE = "update"
    MOVE = "move"
    RESIZE = "resize"
    REORDER = "reorder"
    GROUP = "group"
    UNGROUP = "ungroup"


@dataclass
class CanvasHistoryEntry:
    """Entrada no historico de acoes"""
    action: CanvasAction
    timestamp: datetime
    component_id: str
    before_state: Optional[Dict] = None
    after_state: Optional[Dict] = None

    def to_dict(self) -> Dict:
        return {
            'action': self.action.value,
            'timestamp': self.timestamp.isoformat(),
            'component_id': self.component_id,
            'before_state': self.before_state,
            'after_state': self.after_state
        }


@dataclass
class CanvasState:
    """Estado completo do canvas"""
    id: str
    project_id: str
    name: str
    components: Dict[str, ComponentInstance] = field(default_factory=dict)
    selected_ids: List[str] = field(default_factory=list)
    history: List[CanvasHistoryEntry] = field(default_factory=list)
    history_index: int = -1
    canvas_width: int = 1200
    canvas_height: int = 800
    grid_size: int = 8
    snap_to_grid: bool = True
    show_guides: bool = True
    zoom: float = 1.0
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'project_id': self.project_id,
            'name': self.name,
            'components': {k: v.to_dict() for k, v in self.components.items()},
            'selected_ids': self.selected_ids,
            'canvas_width': self.canvas_width,
            'canvas_height': self.canvas_height,
            'grid_size': self.grid_size,
            'snap_to_grid': self.snap_to_grid,
            'show_guides': self.show_guides,
            'zoom': self.zoom,
            'created_at': self.created_at.isoformat(),
            'updated_at': self.updated_at.isoformat()
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'CanvasState':
        components = {
            k: ComponentInstance.from_dict(v)
            for k, v in data.get('components', {}).items()
        }
        return cls(
            id=data['id'],
            project_id=data['project_id'],
            name=data.get('name', 'Canvas'),
            components=components,
            selected_ids=data.get('selected_ids', []),
            canvas_width=data.get('canvas_width', 1200),
            canvas_height=data.get('canvas_height', 800),
            grid_size=data.get('grid_size', 8),
            snap_to_grid=data.get('snap_to_grid', True),
            show_guides=data.get('show_guides', True),
            zoom=data.get('zoom', 1.0),
            created_at=datetime.fromisoformat(data.get('created_at', datetime.now().isoformat())),
            updated_at=datetime.fromisoformat(data.get('updated_at', datetime.now().isoformat()))
        )


class VisualBuilder:
    """
    Engine principal do Visual Builder.

    Gerencia o estado do canvas, operacoes em componentes,
    historico de acoes e exportacao para codigo.
    """

    MAX_HISTORY = 50

    def __init__(self):
        self.registry = ComponentRegistry()
        self.renderer = ComponentRenderer()
        self._canvases: Dict[str, CanvasState] = {}

    # === Canvas Management ===

    def create_canvas(self, project_id: str, name: str = "Canvas Principal") -> CanvasState:
        """Cria um novo canvas"""
        canvas = CanvasState(
            id=str(uuid.uuid4()),
            project_id=project_id,
            name=name
        )
        self._canvases[canvas.id] = canvas
        return canvas

    def get_canvas(self, canvas_id: str) -> Optional[CanvasState]:
        """Busca um canvas por ID"""
        return self._canvases.get(canvas_id)

    def get_canvases_by_project(self, project_id: str) -> List[CanvasState]:
        """Lista canvases de um projeto"""
        return [c for c in self._canvases.values() if c.project_id == project_id]

    def save_canvas(self, canvas: CanvasState) -> CanvasState:
        """Salva o estado do canvas"""
        canvas.updated_at = datetime.now()
        self._canvases[canvas.id] = canvas
        return canvas

    def load_canvas(self, data: Dict) -> CanvasState:
        """Carrega canvas de dados JSON"""
        canvas = CanvasState.from_dict(data)
        self._canvases[canvas.id] = canvas
        return canvas

    def delete_canvas(self, canvas_id: str) -> bool:
        """Remove um canvas"""
        if canvas_id in self._canvases:
            del self._canvases[canvas_id]
            return True
        return False

    # === Component Operations ===

    def add_component(
        self,
        canvas_id: str,
        component_id: str,
        props: Dict[str, Any] = None,
        position: Dict[str, int] = None,
        parent_id: str = None
    ) -> Optional[ComponentInstance]:
        """Adiciona um componente ao canvas"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return None

        definition = self.registry.get(component_id)
        if not definition:
            return None

        # Aplica props padrao
        default_props = {}
        for prop in definition.props:
            if prop.default is not None:
                default_props[prop.name] = prop.default

        merged_props = {**default_props, **(props or {})}

        # Snap to grid se habilitado
        if canvas.snap_to_grid and position:
            position = self._snap_position(position, canvas.grid_size)

        instance = ComponentInstance(
            id=str(uuid.uuid4()),
            component_id=component_id,
            props=merged_props,
            position=position or {'x': 50, 'y': 50, 'width': 200, 'height': 50},
            parent_id=parent_id
        )

        # Se tem parent, adiciona como filho
        if parent_id and parent_id in canvas.components:
            parent = canvas.components[parent_id]
            parent.children.append(instance)
        else:
            canvas.components[instance.id] = instance

        # Registra no historico
        self._add_history(canvas, CanvasAction.ADD, instance.id, None, instance.to_dict())

        return instance

    def remove_component(self, canvas_id: str, component_instance_id: str) -> bool:
        """Remove um componente do canvas"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return False

        if component_instance_id not in canvas.components:
            return False

        component = canvas.components[component_instance_id]
        before_state = component.to_dict()

        # Remove da selecao
        if component_instance_id in canvas.selected_ids:
            canvas.selected_ids.remove(component_instance_id)

        # Remove componente e filhos
        del canvas.components[component_instance_id]

        self._add_history(canvas, CanvasAction.REMOVE, component_instance_id, before_state, None)
        return True

    def update_component(
        self,
        canvas_id: str,
        component_instance_id: str,
        props: Dict[str, Any] = None,
        position: Dict[str, int] = None
    ) -> Optional[ComponentInstance]:
        """Atualiza propriedades/posicao de um componente"""
        canvas = self.get_canvas(canvas_id)
        if not canvas or component_instance_id not in canvas.components:
            return None

        component = canvas.components[component_instance_id]
        before_state = component.to_dict()

        if props:
            component.props.update(props)

        if position:
            if canvas.snap_to_grid:
                position = self._snap_position(position, canvas.grid_size)
            component.position.update(position)

        self._add_history(canvas, CanvasAction.UPDATE, component_instance_id, before_state, component.to_dict())
        return component

    def move_component(
        self,
        canvas_id: str,
        component_instance_id: str,
        x: int,
        y: int
    ) -> Optional[ComponentInstance]:
        """Move um componente para nova posicao"""
        return self.update_component(canvas_id, component_instance_id, position={'x': x, 'y': y})

    def resize_component(
        self,
        canvas_id: str,
        component_instance_id: str,
        width: int,
        height: int
    ) -> Optional[ComponentInstance]:
        """Redimensiona um componente"""
        return self.update_component(canvas_id, component_instance_id, position={'width': width, 'height': height})

    def duplicate_component(self, canvas_id: str, component_instance_id: str) -> Optional[ComponentInstance]:
        """Duplica um componente"""
        canvas = self.get_canvas(canvas_id)
        if not canvas or component_instance_id not in canvas.components:
            return None

        original = canvas.components[component_instance_id]

        # Copia com nova posicao (offset de 20px)
        new_position = {
            'x': original.position['x'] + 20,
            'y': original.position['y'] + 20,
            'width': original.position['width'],
            'height': original.position['height']
        }

        return self.add_component(
            canvas_id,
            original.component_id,
            props=deepcopy(original.props),
            position=new_position,
            parent_id=original.parent_id
        )

    # === Selection ===

    def select_component(self, canvas_id: str, component_instance_id: str, multi: bool = False) -> List[str]:
        """Seleciona um componente"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return []

        if not multi:
            canvas.selected_ids = []

        if component_instance_id in canvas.components:
            if component_instance_id not in canvas.selected_ids:
                canvas.selected_ids.append(component_instance_id)

        return canvas.selected_ids

    def deselect_component(self, canvas_id: str, component_instance_id: str) -> List[str]:
        """Remove selecao de um componente"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return []

        if component_instance_id in canvas.selected_ids:
            canvas.selected_ids.remove(component_instance_id)

        return canvas.selected_ids

    def clear_selection(self, canvas_id: str) -> List[str]:
        """Limpa toda a selecao"""
        canvas = self.get_canvas(canvas_id)
        if canvas:
            canvas.selected_ids = []
        return []

    def select_all(self, canvas_id: str) -> List[str]:
        """Seleciona todos os componentes"""
        canvas = self.get_canvas(canvas_id)
        if canvas:
            canvas.selected_ids = list(canvas.components.keys())
        return canvas.selected_ids if canvas else []

    # === History (Undo/Redo) ===

    def _add_history(
        self,
        canvas: CanvasState,
        action: CanvasAction,
        component_id: str,
        before: Optional[Dict],
        after: Optional[Dict]
    ):
        """Adiciona entrada no historico"""
        # Remove historico apos o indice atual (para novo branch)
        if canvas.history_index < len(canvas.history) - 1:
            canvas.history = canvas.history[:canvas.history_index + 1]

        entry = CanvasHistoryEntry(
            action=action,
            timestamp=datetime.now(),
            component_id=component_id,
            before_state=before,
            after_state=after
        )

        canvas.history.append(entry)
        canvas.history_index = len(canvas.history) - 1

        # Limita tamanho do historico
        if len(canvas.history) > self.MAX_HISTORY:
            canvas.history = canvas.history[-self.MAX_HISTORY:]
            canvas.history_index = len(canvas.history) - 1

    def undo(self, canvas_id: str) -> bool:
        """Desfaz a ultima acao"""
        canvas = self.get_canvas(canvas_id)
        if not canvas or canvas.history_index < 0:
            return False

        entry = canvas.history[canvas.history_index]

        # Reverte a acao
        if entry.action == CanvasAction.ADD:
            # Remove o componente adicionado
            if entry.component_id in canvas.components:
                del canvas.components[entry.component_id]

        elif entry.action == CanvasAction.REMOVE:
            # Restaura o componente removido
            if entry.before_state:
                canvas.components[entry.component_id] = ComponentInstance.from_dict(entry.before_state)

        elif entry.action in [CanvasAction.UPDATE, CanvasAction.MOVE, CanvasAction.RESIZE]:
            # Restaura estado anterior
            if entry.before_state and entry.component_id in canvas.components:
                canvas.components[entry.component_id] = ComponentInstance.from_dict(entry.before_state)

        canvas.history_index -= 1
        return True

    def redo(self, canvas_id: str) -> bool:
        """Refaz a ultima acao desfeita"""
        canvas = self.get_canvas(canvas_id)
        if not canvas or canvas.history_index >= len(canvas.history) - 1:
            return False

        canvas.history_index += 1
        entry = canvas.history[canvas.history_index]

        # Reaplica a acao
        if entry.action == CanvasAction.ADD:
            if entry.after_state:
                canvas.components[entry.component_id] = ComponentInstance.from_dict(entry.after_state)

        elif entry.action == CanvasAction.REMOVE:
            if entry.component_id in canvas.components:
                del canvas.components[entry.component_id]

        elif entry.action in [CanvasAction.UPDATE, CanvasAction.MOVE, CanvasAction.RESIZE]:
            if entry.after_state and entry.component_id in canvas.components:
                canvas.components[entry.component_id] = ComponentInstance.from_dict(entry.after_state)

        return True

    def can_undo(self, canvas_id: str) -> bool:
        """Verifica se pode desfazer"""
        canvas = self.get_canvas(canvas_id)
        return canvas is not None and canvas.history_index >= 0

    def can_redo(self, canvas_id: str) -> bool:
        """Verifica se pode refazer"""
        canvas = self.get_canvas(canvas_id)
        return canvas is not None and canvas.history_index < len(canvas.history) - 1

    # === Code Export ===

    def export_code(
        self,
        canvas_id: str,
        format: OutputFormat = OutputFormat.REACT,
        include_project: bool = False
    ) -> Dict[str, str]:
        """Exporta canvas para codigo"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return {}

        instances = list(canvas.components.values())

        if include_project:
            if format == OutputFormat.REACT:
                return self.renderer.generate_full_react_app(instances, canvas.name)
            elif format == OutputFormat.VUE:
                return self.renderer.generate_full_vue_app(instances, canvas.name)

        # Apenas componente principal
        code = self.renderer.render_tree(instances, format)
        return {'app': code}

    def export_json(self, canvas_id: str) -> Optional[str]:
        """Exporta canvas como JSON"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return None
        return json.dumps(canvas.to_dict(), indent=2)

    # === Helpers ===

    def _snap_position(self, position: Dict[str, int], grid_size: int) -> Dict[str, int]:
        """Ajusta posicao para o grid"""
        return {
            'x': round(position.get('x', 0) / grid_size) * grid_size,
            'y': round(position.get('y', 0) / grid_size) * grid_size,
            'width': position.get('width', 200),
            'height': position.get('height', 50)
        }

    def get_component_tree(self, canvas_id: str) -> List[Dict]:
        """Retorna arvore de componentes formatada para UI"""
        canvas = self.get_canvas(canvas_id)
        if not canvas:
            return []

        def build_tree(components: Dict[str, ComponentInstance], parent_id: str = None) -> List[Dict]:
            result = []
            for comp in components.values():
                if comp.parent_id == parent_id:
                    definition = self.registry.get(comp.component_id)
                    item = {
                        'id': comp.id,
                        'type': comp.component_id,
                        'name': definition.name if definition else comp.component_id,
                        'icon': definition.icon if definition else 'cube',
                        'selected': comp.id in canvas.selected_ids,
                        'children': [build_tree({c.id: c for c in comp.children}, comp.id) if comp.children else []]
                    }
                    result.append(item)
            return result

        return build_tree(canvas.components)

    def get_available_components(self) -> Dict:
        """Retorna componentes disponiveis para a paleta"""
        return self.registry.to_dict()

    def get_component_props(self, component_id: str) -> Optional[Dict]:
        """Retorna definicao de props de um componente"""
        definition = self.registry.get(component_id)
        if not definition:
            return None
        return definition.to_dict()


# Instancia global
visual_builder = VisualBuilder()
