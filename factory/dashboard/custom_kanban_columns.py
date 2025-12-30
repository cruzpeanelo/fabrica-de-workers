# -*- coding: utf-8 -*-
"""
Custom Kanban Columns Module (Issue #252)
=========================================
Colunas customizaveis no Kanban por projeto.

Funcionalidades:
- Criar/editar/excluir colunas
- Reordenar colunas
- Cores e icones customizaveis
- WIP limits por coluna
- Mapeamento de status
- Templates de colunas
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import uuid

router = APIRouter(prefix="/api/kanban-columns", tags=["Kanban Columns"])

# Column configurations storage (in production, use database)
project_columns: Dict[str, List[dict]] = {}

# Default columns
DEFAULT_COLUMNS = [
    {
        "id": "backlog",
        "name": "Backlog",
        "status": "backlog",
        "color": "#6B7280",
        "icon": "inbox",
        "position": 0,
        "wip_limit": None,
        "is_start": True,
        "is_end": False,
        "description": "Stories aguardando priorizacao"
    },
    {
        "id": "ready",
        "name": "Ready",
        "status": "ready",
        "color": "#3B82F6",
        "icon": "check-circle",
        "position": 1,
        "wip_limit": 10,
        "is_start": False,
        "is_end": False,
        "description": "Stories prontas para desenvolvimento"
    },
    {
        "id": "in_progress",
        "name": "In Progress",
        "status": "in_progress",
        "color": "#F59E0B",
        "icon": "play",
        "position": 2,
        "wip_limit": 5,
        "is_start": False,
        "is_end": False,
        "description": "Stories em desenvolvimento"
    },
    {
        "id": "review",
        "name": "Review",
        "status": "review",
        "color": "#8B5CF6",
        "icon": "eye",
        "position": 3,
        "wip_limit": 3,
        "is_start": False,
        "is_end": False,
        "description": "Aguardando revisao de codigo"
    },
    {
        "id": "testing",
        "name": "Testing",
        "status": "testing",
        "color": "#EC4899",
        "icon": "beaker",
        "position": 4,
        "wip_limit": 3,
        "is_start": False,
        "is_end": False,
        "description": "Em teste"
    },
    {
        "id": "done",
        "name": "Done",
        "status": "done",
        "color": "#10B981",
        "icon": "check",
        "position": 5,
        "wip_limit": None,
        "is_start": False,
        "is_end": True,
        "description": "Stories concluidas"
    }
]

# Column templates
COLUMN_TEMPLATES = {
    "default": {
        "name": "Padrao",
        "description": "Backlog -> Ready -> In Progress -> Review -> Testing -> Done",
        "columns": DEFAULT_COLUMNS
    },
    "simple": {
        "name": "Simples",
        "description": "To Do -> Doing -> Done",
        "columns": [
            {"id": "todo", "name": "To Do", "status": "backlog", "color": "#6B7280", "position": 0, "is_start": True},
            {"id": "doing", "name": "Doing", "status": "in_progress", "color": "#F59E0B", "position": 1, "wip_limit": 5},
            {"id": "done", "name": "Done", "status": "done", "color": "#10B981", "position": 2, "is_end": True}
        ]
    },
    "devops": {
        "name": "DevOps",
        "description": "Backlog -> Dev -> Build -> Deploy -> Prod",
        "columns": [
            {"id": "backlog", "name": "Backlog", "status": "backlog", "color": "#6B7280", "position": 0, "is_start": True},
            {"id": "dev", "name": "Development", "status": "in_progress", "color": "#3B82F6", "position": 1, "wip_limit": 5},
            {"id": "build", "name": "Build/Test", "status": "testing", "color": "#F59E0B", "position": 2, "wip_limit": 3},
            {"id": "deploy", "name": "Deploy", "status": "review", "color": "#8B5CF6", "position": 3, "wip_limit": 2},
            {"id": "prod", "name": "Production", "status": "done", "color": "#10B981", "position": 4, "is_end": True}
        ]
    },
    "scrum": {
        "name": "Scrum",
        "description": "Product Backlog -> Sprint Backlog -> In Progress -> Done",
        "columns": [
            {"id": "product_backlog", "name": "Product Backlog", "status": "backlog", "color": "#6B7280", "position": 0, "is_start": True},
            {"id": "sprint_backlog", "name": "Sprint Backlog", "status": "ready", "color": "#3B82F6", "position": 1},
            {"id": "in_progress", "name": "In Progress", "status": "in_progress", "color": "#F59E0B", "position": 2, "wip_limit": 4},
            {"id": "review", "name": "Review", "status": "review", "color": "#8B5CF6", "position": 3, "wip_limit": 2},
            {"id": "done", "name": "Done", "status": "done", "color": "#10B981", "position": 4, "is_end": True}
        ]
    }
}


class ColumnCreate(BaseModel):
    name: str
    status: str
    color: Optional[str] = "#6B7280"
    icon: Optional[str] = "folder"
    position: Optional[int] = None
    wip_limit: Optional[int] = None
    is_start: bool = False
    is_end: bool = False
    description: Optional[str] = None


class ColumnUpdate(BaseModel):
    name: Optional[str] = None
    status: Optional[str] = None
    color: Optional[str] = None
    icon: Optional[str] = None
    position: Optional[int] = None
    wip_limit: Optional[int] = None
    is_start: Optional[bool] = None
    is_end: Optional[bool] = None
    description: Optional[str] = None


class ColumnsReorder(BaseModel):
    column_ids: List[str]


@router.get("/templates")
async def list_templates():
    """Lista templates de colunas disponiveis."""
    return {
        "templates": [
            {
                "id": k,
                "name": v["name"],
                "description": v["description"],
                "column_count": len(v["columns"])
            }
            for k, v in COLUMN_TEMPLATES.items()
        ]
    }


@router.get("/{project_id}")
async def get_columns(project_id: str):
    """Retorna colunas do projeto."""
    columns = project_columns.get(project_id, None)

    if columns is None:
        # Return default columns for new project
        columns = [dict(c) for c in DEFAULT_COLUMNS]  # Deep copy

    return {
        "project_id": project_id,
        "columns": sorted(columns, key=lambda c: c.get("position", 0))
    }


@router.post("/{project_id}")
async def create_column(project_id: str, column: ColumnCreate):
    """Cria uma nova coluna."""
    if project_id not in project_columns:
        project_columns[project_id] = [dict(c) for c in DEFAULT_COLUMNS]

    columns = project_columns[project_id]

    # Generate unique ID
    column_id = f"col_{uuid.uuid4().hex[:8]}"

    # Calculate position if not provided
    if column.position is None:
        column.position = len(columns)

    new_column = {
        "id": column_id,
        "name": column.name,
        "status": column.status,
        "color": column.color,
        "icon": column.icon,
        "position": column.position,
        "wip_limit": column.wip_limit,
        "is_start": column.is_start,
        "is_end": column.is_end,
        "description": column.description,
        "created_at": datetime.now().isoformat()
    }

    columns.append(new_column)

    # Re-sort positions
    columns.sort(key=lambda c: c.get("position", 0))
    for i, c in enumerate(columns):
        c["position"] = i

    return {
        "success": True,
        "column": new_column
    }


@router.put("/{project_id}/{column_id}")
async def update_column(project_id: str, column_id: str, column: ColumnUpdate):
    """Atualiza uma coluna."""
    if project_id not in project_columns:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    columns = project_columns[project_id]
    col = next((c for c in columns if c["id"] == column_id), None)

    if not col:
        raise HTTPException(status_code=404, detail="Coluna nao encontrada")

    # Update fields
    updates = column.dict(exclude_none=True)
    col.update(updates)
    col["updated_at"] = datetime.now().isoformat()

    return {
        "success": True,
        "column": col
    }


@router.delete("/{project_id}/{column_id}")
async def delete_column(project_id: str, column_id: str):
    """Exclui uma coluna."""
    if project_id not in project_columns:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    columns = project_columns[project_id]
    col = next((c for c in columns if c["id"] == column_id), None)

    if not col:
        raise HTTPException(status_code=404, detail="Coluna nao encontrada")

    # Cannot delete start or end columns
    if col.get("is_start") or col.get("is_end"):
        raise HTTPException(status_code=400, detail="Nao e possivel excluir colunas de inicio/fim")

    # Remove column
    project_columns[project_id] = [c for c in columns if c["id"] != column_id]

    # Re-sort positions
    for i, c in enumerate(project_columns[project_id]):
        c["position"] = i

    return {
        "success": True,
        "message": "Coluna excluida"
    }


@router.post("/{project_id}/reorder")
async def reorder_columns(project_id: str, order: ColumnsReorder):
    """Reordena colunas."""
    if project_id not in project_columns:
        project_columns[project_id] = [dict(c) for c in DEFAULT_COLUMNS]

    columns = project_columns[project_id]
    column_map = {c["id"]: c for c in columns}

    # Update positions based on new order
    for i, column_id in enumerate(order.column_ids):
        if column_id in column_map:
            column_map[column_id]["position"] = i

    # Sort and save
    project_columns[project_id] = sorted(columns, key=lambda c: c.get("position", 0))

    return {
        "success": True,
        "columns": project_columns[project_id]
    }


@router.post("/{project_id}/apply-template/{template_id}")
async def apply_template(project_id: str, template_id: str):
    """Aplica um template de colunas ao projeto."""
    if template_id not in COLUMN_TEMPLATES:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    template = COLUMN_TEMPLATES[template_id]

    # Deep copy template columns
    project_columns[project_id] = [
        {**c, "id": f"col_{uuid.uuid4().hex[:8]}"} for c in template["columns"]
    ]

    return {
        "success": True,
        "message": f"Template '{template['name']}' aplicado",
        "columns": project_columns[project_id]
    }


@router.post("/{project_id}/reset")
async def reset_columns(project_id: str):
    """Reseta colunas para o padrao."""
    project_columns[project_id] = [dict(c) for c in DEFAULT_COLUMNS]

    return {
        "success": True,
        "columns": project_columns[project_id]
    }


@router.get("/{project_id}/wip-status")
async def get_wip_status(project_id: str):
    """Retorna status dos limites WIP."""
    columns = project_columns.get(project_id, DEFAULT_COLUMNS)

    # In production, count actual stories in each column
    wip_status = []
    for col in columns:
        wip_status.append({
            "column_id": col["id"],
            "column_name": col["name"],
            "wip_limit": col.get("wip_limit"),
            "current_count": 0,  # Would be actual count
            "over_limit": False,
            "at_limit": False
        })

    return {"wip_status": wip_status}


def get_column_editor_html():
    """Retorna o HTML do editor de colunas."""
    return '''
    <!-- Kanban Column Editor (Issue #252) -->
    <div v-if="showColumnEditor"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showColumnEditor = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-3xl mx-4 max-h-[85vh] overflow-hidden flex flex-col">
            <div class="px-6 py-4 border-b flex items-center justify-between">
                <h2 class="text-lg font-bold">Configurar Colunas</h2>
                <button @click="showColumnEditor = false" class="text-gray-400 hover:text-gray-600">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <div class="flex-1 overflow-y-auto p-6">
                <!-- Templates -->
                <div class="mb-6">
                    <h3 class="font-semibold mb-3">Templates</h3>
                    <div class="flex gap-2 flex-wrap">
                        <button v-for="tpl in columnTemplates" :key="tpl.id"
                                @click="applyColumnTemplate(tpl.id)"
                                class="px-3 py-2 border rounded-lg hover:bg-gray-50 text-sm">
                            {{ tpl.name }}
                            <span class="text-gray-400 ml-1">({{ tpl.column_count }})</span>
                        </button>
                    </div>
                </div>

                <!-- Columns List -->
                <div class="space-y-3" ref="columnsList">
                    <div v-for="(col, index) in kanbanColumns" :key="col.id"
                         class="flex items-center gap-3 p-3 bg-gray-50 rounded-lg"
                         draggable="true"
                         @dragstart="dragColumn(index)"
                         @dragover.prevent
                         @drop="dropColumn(index)">
                        <!-- Drag Handle -->
                        <div class="cursor-move text-gray-400">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M4 8h16M4 16h16"/>
                            </svg>
                        </div>

                        <!-- Color Picker -->
                        <input type="color" v-model="col.color"
                               @change="updateColumn(col)"
                               class="w-8 h-8 rounded cursor-pointer border-0">

                        <!-- Name -->
                        <input type="text" v-model="col.name"
                               @blur="updateColumn(col)"
                               class="flex-1 px-3 py-1 border rounded-lg text-sm">

                        <!-- WIP Limit -->
                        <div class="flex items-center gap-1">
                            <span class="text-xs text-gray-500">WIP:</span>
                            <input type="number" v-model.number="col.wip_limit"
                                   @blur="updateColumn(col)"
                                   placeholder="-"
                                   class="w-12 px-2 py-1 border rounded text-sm text-center">
                        </div>

                        <!-- Badges -->
                        <div class="flex gap-1">
                            <span v-if="col.is_start" class="text-xs bg-green-100 text-green-600 px-1.5 rounded">
                                Inicio
                            </span>
                            <span v-if="col.is_end" class="text-xs bg-blue-100 text-blue-600 px-1.5 rounded">
                                Fim
                            </span>
                        </div>

                        <!-- Delete -->
                        <button @click="deleteColumn(col.id)"
                                :disabled="col.is_start || col.is_end"
                                class="text-gray-400 hover:text-red-600 disabled:opacity-30 disabled:cursor-not-allowed">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <!-- Add Column -->
                <div class="mt-4 p-4 border-2 border-dashed border-gray-300 rounded-lg text-center">
                    <button @click="showAddColumnForm = true"
                            class="text-blue-600 hover:text-blue-700 flex items-center gap-2 mx-auto">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/>
                        </svg>
                        Adicionar Coluna
                    </button>
                </div>

                <!-- Add Column Form -->
                <div v-if="showAddColumnForm" class="mt-4 p-4 bg-blue-50 rounded-lg">
                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium mb-1">Nome</label>
                            <input type="text" v-model="newColumn.name"
                                   class="w-full px-3 py-2 border rounded-lg">
                        </div>
                        <div>
                            <label class="block text-sm font-medium mb-1">Cor</label>
                            <input type="color" v-model="newColumn.color"
                                   class="w-full h-10 rounded cursor-pointer">
                        </div>
                        <div>
                            <label class="block text-sm font-medium mb-1">Status Mapeado</label>
                            <select v-model="newColumn.status" class="w-full px-3 py-2 border rounded-lg">
                                <option value="backlog">Backlog</option>
                                <option value="ready">Ready</option>
                                <option value="in_progress">In Progress</option>
                                <option value="review">Review</option>
                                <option value="testing">Testing</option>
                                <option value="done">Done</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium mb-1">Limite WIP</label>
                            <input type="number" v-model.number="newColumn.wip_limit"
                                   placeholder="Sem limite"
                                   class="w-full px-3 py-2 border rounded-lg">
                        </div>
                    </div>
                    <div class="mt-4 flex justify-end gap-2">
                        <button @click="showAddColumnForm = false"
                                class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">
                            Cancelar
                        </button>
                        <button @click="addColumn"
                                class="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
                            Adicionar
                        </button>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-3 bg-gray-50 border-t flex justify-between">
                <button @click="resetColumns"
                        class="text-sm text-gray-600 hover:underline">
                    Restaurar Padrao
                </button>
                <button @click="showColumnEditor = false"
                        class="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
                    Concluir
                </button>
            </div>
        </div>
    </div>
    '''


def get_column_editor_js():
    """Retorna o JavaScript para o editor de colunas."""
    return '''
    // Kanban Column Editor State
    showColumnEditor: false,
    showAddColumnForm: false,
    kanbanColumns: [],
    columnTemplates: [],
    newColumn: { name: '', status: 'in_progress', color: '#6B7280', wip_limit: null },
    draggedColumnIndex: null,

    // Column Editor Methods
    async loadKanbanColumns() {
        try {
            const response = await fetch(`/api/kanban-columns/${this.currentProject}`);
            const data = await response.json();
            this.kanbanColumns = data.columns;
        } catch (e) {
            console.error('Error loading columns:', e);
        }
    },

    async loadColumnTemplates() {
        try {
            const response = await fetch('/api/kanban-columns/templates');
            const data = await response.json();
            this.columnTemplates = data.templates;
        } catch (e) {
            console.error('Error loading templates:', e);
        }
    },

    async addColumn() {
        if (!this.newColumn.name) return;

        try {
            const response = await fetch(`/api/kanban-columns/${this.currentProject}`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(this.newColumn)
            });

            if (response.ok) {
                const data = await response.json();
                this.kanbanColumns.push(data.column);
                this.newColumn = { name: '', status: 'in_progress', color: '#6B7280', wip_limit: null };
                this.showAddColumnForm = false;
            }
        } catch (e) {
            this.showNotification('Erro ao adicionar coluna', 'error');
        }
    },

    async updateColumn(column) {
        try {
            await fetch(`/api/kanban-columns/${this.currentProject}/${column.id}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(column)
            });
        } catch (e) {
            this.showNotification('Erro ao atualizar coluna', 'error');
        }
    },

    async deleteColumn(columnId) {
        if (!confirm('Excluir esta coluna?')) return;

        try {
            await fetch(`/api/kanban-columns/${this.currentProject}/${columnId}`, {
                method: 'DELETE'
            });
            this.kanbanColumns = this.kanbanColumns.filter(c => c.id !== columnId);
        } catch (e) {
            this.showNotification('Erro ao excluir coluna', 'error');
        }
    },

    async applyColumnTemplate(templateId) {
        if (!confirm('Isso substituira todas as colunas atuais. Continuar?')) return;

        try {
            const response = await fetch(`/api/kanban-columns/${this.currentProject}/apply-template/${templateId}`, {
                method: 'POST'
            });
            const data = await response.json();
            this.kanbanColumns = data.columns;
            this.showNotification('Template aplicado', 'success');
        } catch (e) {
            this.showNotification('Erro ao aplicar template', 'error');
        }
    },

    async resetColumns() {
        if (!confirm('Restaurar colunas para o padrao?')) return;

        try {
            const response = await fetch(`/api/kanban-columns/${this.currentProject}/reset`, {
                method: 'POST'
            });
            const data = await response.json();
            this.kanbanColumns = data.columns;
            this.showNotification('Colunas restauradas', 'success');
        } catch (e) {
            this.showNotification('Erro ao restaurar colunas', 'error');
        }
    },

    dragColumn(index) {
        this.draggedColumnIndex = index;
    },

    async dropColumn(targetIndex) {
        if (this.draggedColumnIndex === null || this.draggedColumnIndex === targetIndex) return;

        // Reorder locally
        const [removed] = this.kanbanColumns.splice(this.draggedColumnIndex, 1);
        this.kanbanColumns.splice(targetIndex, 0, removed);

        // Update positions
        this.kanbanColumns.forEach((col, i) => col.position = i);

        // Save to server
        try {
            await fetch(`/api/kanban-columns/${this.currentProject}/reorder`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    column_ids: this.kanbanColumns.map(c => c.id)
                })
            });
        } catch (e) {
            console.error('Error reordering columns:', e);
        }

        this.draggedColumnIndex = null;
    },

    openColumnEditor() {
        this.loadKanbanColumns();
        this.loadColumnTemplates();
        this.showColumnEditor = true;
    }
    '''


def register_custom_columns(app):
    """Registra os endpoints de colunas customizaveis no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Custom Kanban Columns endpoints loaded: /api/kanban-columns/*")
