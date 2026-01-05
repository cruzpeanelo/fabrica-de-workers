"""
Canvas Routes - API do Visual Builder

Endpoints REST para gerenciamento do canvas visual,
componentes e exportacao de codigo.
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import Dict, List, Optional, Any

from factory.core.visual_builder import visual_builder, CanvasState
from factory.ui_components.component_renderer import OutputFormat


router = APIRouter(prefix="/api/canvas", tags=["Visual Builder"])


# === Request/Response Models ===

class CreateCanvasRequest(BaseModel):
    project_id: str
    name: str = "Canvas Principal"


class AddComponentRequest(BaseModel):
    component_id: str
    props: Optional[Dict[str, Any]] = None
    position: Optional[Dict[str, int]] = None
    parent_id: Optional[str] = None


class UpdateComponentRequest(BaseModel):
    props: Optional[Dict[str, Any]] = None
    position: Optional[Dict[str, int]] = None


class MoveComponentRequest(BaseModel):
    x: int
    y: int


class ResizeComponentRequest(BaseModel):
    width: int
    height: int


class SelectRequest(BaseModel):
    component_ids: List[str]
    multi: bool = False


class ExportRequest(BaseModel):
    format: str = "react"  # react, vue, html, json
    include_project: bool = False


class CanvasSettingsRequest(BaseModel):
    canvas_width: Optional[int] = None
    canvas_height: Optional[int] = None
    grid_size: Optional[int] = None
    snap_to_grid: Optional[bool] = None
    show_guides: Optional[bool] = None
    zoom: Optional[float] = None


# === Canvas Endpoints ===

@router.post("/", response_model=Dict)
async def create_canvas(request: CreateCanvasRequest):
    """Cria um novo canvas"""
    canvas = visual_builder.create_canvas(request.project_id, request.name)
    return canvas.to_dict()


@router.get("/{canvas_id}", response_model=Dict)
async def get_canvas(canvas_id: str):
    """Busca um canvas por ID"""
    canvas = visual_builder.get_canvas(canvas_id)
    if not canvas:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")
    return canvas.to_dict()


@router.get("/project/{project_id}", response_model=List[Dict])
async def get_project_canvases(project_id: str):
    """Lista canvases de um projeto"""
    canvases = visual_builder.get_canvases_by_project(project_id)
    return [c.to_dict() for c in canvases]


@router.put("/{canvas_id}/settings", response_model=Dict)
async def update_canvas_settings(canvas_id: str, request: CanvasSettingsRequest):
    """Atualiza configuracoes do canvas"""
    canvas = visual_builder.get_canvas(canvas_id)
    if not canvas:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")

    if request.canvas_width is not None:
        canvas.canvas_width = request.canvas_width
    if request.canvas_height is not None:
        canvas.canvas_height = request.canvas_height
    if request.grid_size is not None:
        canvas.grid_size = request.grid_size
    if request.snap_to_grid is not None:
        canvas.snap_to_grid = request.snap_to_grid
    if request.show_guides is not None:
        canvas.show_guides = request.show_guides
    if request.zoom is not None:
        canvas.zoom = request.zoom

    visual_builder.save_canvas(canvas)
    return canvas.to_dict()


@router.delete("/{canvas_id}")
async def delete_canvas(canvas_id: str):
    """Remove um canvas"""
    if not visual_builder.delete_canvas(canvas_id):
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")
    return {"message": "Canvas removido com sucesso"}


# === Component Endpoints ===

@router.post("/{canvas_id}/components", response_model=Dict)
async def add_component(canvas_id: str, request: AddComponentRequest):
    """Adiciona um componente ao canvas"""
    component = visual_builder.add_component(
        canvas_id,
        request.component_id,
        request.props,
        request.position,
        request.parent_id
    )
    if not component:
        raise HTTPException(status_code=400, detail="Falha ao adicionar componente")
    return component.to_dict()


@router.put("/{canvas_id}/components/{component_id}", response_model=Dict)
async def update_component(canvas_id: str, component_id: str, request: UpdateComponentRequest):
    """Atualiza um componente"""
    component = visual_builder.update_component(
        canvas_id,
        component_id,
        request.props,
        request.position
    )
    if not component:
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return component.to_dict()


@router.put("/{canvas_id}/components/{component_id}/move", response_model=Dict)
async def move_component(canvas_id: str, component_id: str, request: MoveComponentRequest):
    """Move um componente"""
    component = visual_builder.move_component(canvas_id, component_id, request.x, request.y)
    if not component:
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return component.to_dict()


@router.put("/{canvas_id}/components/{component_id}/resize", response_model=Dict)
async def resize_component(canvas_id: str, component_id: str, request: ResizeComponentRequest):
    """Redimensiona um componente"""
    component = visual_builder.resize_component(canvas_id, component_id, request.width, request.height)
    if not component:
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return component.to_dict()


@router.post("/{canvas_id}/components/{component_id}/duplicate", response_model=Dict)
async def duplicate_component(canvas_id: str, component_id: str):
    """Duplica um componente"""
    component = visual_builder.duplicate_component(canvas_id, component_id)
    if not component:
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return component.to_dict()


@router.delete("/{canvas_id}/components/{component_id}")
async def remove_component(canvas_id: str, component_id: str):
    """Remove um componente"""
    if not visual_builder.remove_component(canvas_id, component_id):
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return {"message": "Componente removido"}


# === Selection Endpoints ===

@router.post("/{canvas_id}/select", response_model=Dict)
async def select_components(canvas_id: str, request: SelectRequest):
    """Seleciona componentes"""
    selected = []
    for comp_id in request.component_ids:
        selected = visual_builder.select_component(canvas_id, comp_id, request.multi)
    return {"selected_ids": selected}


@router.post("/{canvas_id}/deselect/{component_id}", response_model=Dict)
async def deselect_component(canvas_id: str, component_id: str):
    """Remove selecao de um componente"""
    selected = visual_builder.deselect_component(canvas_id, component_id)
    return {"selected_ids": selected}


@router.post("/{canvas_id}/clear-selection", response_model=Dict)
async def clear_selection(canvas_id: str):
    """Limpa toda selecao"""
    visual_builder.clear_selection(canvas_id)
    return {"selected_ids": []}


@router.post("/{canvas_id}/select-all", response_model=Dict)
async def select_all(canvas_id: str):
    """Seleciona todos os componentes"""
    selected = visual_builder.select_all(canvas_id)
    return {"selected_ids": selected}


# === History Endpoints ===

@router.post("/{canvas_id}/undo")
async def undo(canvas_id: str):
    """Desfaz ultima acao"""
    if not visual_builder.undo(canvas_id):
        raise HTTPException(status_code=400, detail="Nada para desfazer")
    canvas = visual_builder.get_canvas(canvas_id)
    return {
        "message": "Acao desfeita",
        "can_undo": visual_builder.can_undo(canvas_id),
        "can_redo": visual_builder.can_redo(canvas_id),
        "components": {k: v.to_dict() for k, v in canvas.components.items()} if canvas else {}
    }


@router.post("/{canvas_id}/redo")
async def redo(canvas_id: str):
    """Refaz ultima acao"""
    if not visual_builder.redo(canvas_id):
        raise HTTPException(status_code=400, detail="Nada para refazer")
    canvas = visual_builder.get_canvas(canvas_id)
    return {
        "message": "Acao refeita",
        "can_undo": visual_builder.can_undo(canvas_id),
        "can_redo": visual_builder.can_redo(canvas_id),
        "components": {k: v.to_dict() for k, v in canvas.components.items()} if canvas else {}
    }


@router.get("/{canvas_id}/history-status")
async def get_history_status(canvas_id: str):
    """Retorna status do historico"""
    return {
        "can_undo": visual_builder.can_undo(canvas_id),
        "can_redo": visual_builder.can_redo(canvas_id)
    }


# === Export Endpoints ===

@router.post("/{canvas_id}/export", response_model=Dict)
async def export_canvas(canvas_id: str, request: ExportRequest):
    """Exporta canvas para codigo"""
    format_map = {
        "react": OutputFormat.REACT,
        "vue": OutputFormat.VUE,
        "html": OutputFormat.HTML,
        "json": OutputFormat.JSON
    }

    output_format = format_map.get(request.format.lower())
    if not output_format:
        raise HTTPException(status_code=400, detail=f"Formato invalido: {request.format}")

    files = visual_builder.export_code(canvas_id, output_format, request.include_project)
    if not files:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")

    return {
        "format": request.format,
        "files": files,
        "file_count": len(files)
    }


@router.get("/{canvas_id}/export/json")
async def export_json(canvas_id: str):
    """Exporta canvas como JSON"""
    json_str = visual_builder.export_json(canvas_id)
    if not json_str:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")
    return {"json": json_str}


# === Component Library Endpoints ===

@router.get("/library/components", response_model=Dict)
async def get_available_components():
    """Lista todos os componentes disponiveis"""
    return visual_builder.get_available_components()


@router.get("/library/components/{component_id}", response_model=Dict)
async def get_component_definition(component_id: str):
    """Retorna definicao de um componente"""
    definition = visual_builder.get_component_props(component_id)
    if not definition:
        raise HTTPException(status_code=404, detail="Componente nao encontrado")
    return definition


@router.get("/{canvas_id}/tree", response_model=List[Dict])
async def get_component_tree(canvas_id: str):
    """Retorna arvore de componentes do canvas"""
    tree = visual_builder.get_component_tree(canvas_id)
    return tree


# === Bulk Operations ===

@router.post("/{canvas_id}/bulk/delete", response_model=Dict)
async def bulk_delete_components(canvas_id: str, component_ids: List[str]):
    """Remove multiplos componentes"""
    deleted = 0
    for comp_id in component_ids:
        if visual_builder.remove_component(canvas_id, comp_id):
            deleted += 1
    return {"deleted": deleted}


@router.post("/{canvas_id}/import", response_model=Dict)
async def import_canvas(canvas_id: str, data: Dict):
    """Importa dados para um canvas existente"""
    canvas = visual_builder.get_canvas(canvas_id)
    if not canvas:
        raise HTTPException(status_code=404, detail="Canvas nao encontrado")

    # Importa componentes do JSON
    try:
        canvas = visual_builder.load_canvas({**data, 'id': canvas_id})
        return canvas.to_dict()
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Erro ao importar: {str(e)}")
