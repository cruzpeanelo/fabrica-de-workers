# -*- coding: utf-8 -*-
"""
Story Templates Module (Issue #254)
====================================
Templates de story por projeto para reutilizacao.

Funcionalidades:
- Templates pre-definidos por tipo
- Templates customizados por projeto
- Campos pre-preenchidos
- Checklist padrao de criterios
- Import/Export de templates
- Aplicar template na criacao
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import uuid
import json

router = APIRouter(prefix="/api/templates", tags=["Story Templates"])

# Template storage (in production, use database)
templates: Dict[str, dict] = {}

# Default templates
DEFAULT_TEMPLATES = [
    {
        "id": "tpl_feature",
        "name": "Feature",
        "description": "Template para novas funcionalidades",
        "category": "feature",
        "icon": "sparkles",
        "color": "#3B82F6",
        "is_system": True,
        "fields": {
            "persona": "usuario",
            "priority": "medium",
            "complexity": "medium"
        },
        "acceptance_criteria": [
            "A funcionalidade esta acessivel na interface",
            "A funcionalidade funciona conforme especificado",
            "A funcionalidade tem tratamento de erros adequado",
            "A funcionalidade foi testada manualmente"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Testes unitarios escritos",
            "Documentacao atualizada",
            "Deploy em staging"
        ],
        "tags": ["feature"]
    },
    {
        "id": "tpl_bugfix",
        "name": "Bug Fix",
        "description": "Template para correcao de bugs",
        "category": "bug",
        "icon": "bug",
        "color": "#EF4444",
        "is_system": True,
        "fields": {
            "priority": "high",
            "complexity": "low"
        },
        "acceptance_criteria": [
            "O bug foi reproduzido e documentado",
            "A correcao resolve o problema reportado",
            "Nao introduz novos bugs",
            "Testado no ambiente onde o bug foi encontrado"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Teste de regressao passou",
            "Bug reportado como resolvido"
        ],
        "tags": ["bug", "fix"]
    },
    {
        "id": "tpl_tech_debt",
        "name": "Technical Debt",
        "description": "Template para divida tecnica",
        "category": "tech_debt",
        "icon": "wrench",
        "color": "#F59E0B",
        "is_system": True,
        "fields": {
            "priority": "low",
            "complexity": "medium"
        },
        "acceptance_criteria": [
            "Codigo refatorado segue padroes do projeto",
            "Performance nao foi degradada",
            "Testes existentes continuam passando"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Metricas de qualidade melhoradas",
            "Documentacao tecnica atualizada"
        ],
        "tags": ["tech-debt", "refactor"]
    },
    {
        "id": "tpl_spike",
        "name": "Spike/Research",
        "description": "Template para investigacao tecnica",
        "category": "spike",
        "icon": "magnifying-glass",
        "color": "#8B5CF6",
        "is_system": True,
        "fields": {
            "story_points": 3,
            "priority": "medium",
            "complexity": "low"
        },
        "acceptance_criteria": [
            "Documento de pesquisa criado",
            "Alternativas avaliadas",
            "Recomendacao documentada",
            "Proximos passos definidos"
        ],
        "definition_of_done": [
            "Apresentacao para o time",
            "Documentacao no wiki"
        ],
        "tags": ["spike", "research"]
    },
    {
        "id": "tpl_api",
        "name": "API Endpoint",
        "description": "Template para novos endpoints de API",
        "category": "feature",
        "icon": "code",
        "color": "#10B981",
        "is_system": True,
        "fields": {
            "priority": "medium",
            "complexity": "medium"
        },
        "acceptance_criteria": [
            "Endpoint documentado no Swagger/OpenAPI",
            "Validacao de entrada implementada",
            "Tratamento de erros com codigos HTTP corretos",
            "Rate limiting configurado",
            "Autenticacao/autorizacao implementada"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Testes de integracao escritos",
            "Documentacao da API atualizada",
            "Postman collection atualizada"
        ],
        "tags": ["api", "backend"]
    },
    {
        "id": "tpl_ui",
        "name": "UI Component",
        "description": "Template para componentes de interface",
        "category": "feature",
        "icon": "squares",
        "color": "#EC4899",
        "is_system": True,
        "fields": {
            "priority": "medium",
            "complexity": "medium"
        },
        "acceptance_criteria": [
            "Componente segue design system",
            "Responsivo em todas as telas",
            "Acessivel (WCAG 2.1 AA)",
            "Estados de loading/error implementados"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Storybook atualizado",
            "Testes de componente escritos",
            "Review de design aprovado"
        ],
        "tags": ["ui", "frontend", "component"]
    },
    {
        "id": "tpl_integration",
        "name": "Integration",
        "description": "Template para integracoes externas",
        "category": "feature",
        "icon": "link",
        "color": "#06B6D4",
        "is_system": True,
        "fields": {
            "priority": "medium",
            "complexity": "high"
        },
        "acceptance_criteria": [
            "Integracao funciona em ambiente de sandbox",
            "Tratamento de falhas e retry implementado",
            "Logs de auditoria configurados",
            "Credenciais armazenadas de forma segura"
        ],
        "definition_of_done": [
            "Codigo revisado",
            "Testes de integracao escritos",
            "Runbook de operacao criado",
            "Monitoramento configurado"
        ],
        "tags": ["integration", "external"]
    },
    {
        "id": "tpl_security",
        "name": "Security",
        "description": "Template para itens de seguranca",
        "category": "security",
        "icon": "shield",
        "color": "#DC2626",
        "is_system": True,
        "fields": {
            "priority": "urgent",
            "complexity": "high"
        },
        "acceptance_criteria": [
            "Vulnerabilidade corrigida",
            "Testes de seguranca passam",
            "Nao introduz novas vulnerabilidades",
            "Revisao de seguranca aprovada"
        ],
        "definition_of_done": [
            "Codigo revisado por security champion",
            "Penetration test passou",
            "Documentacao de seguranca atualizada"
        ],
        "tags": ["security", "vulnerability"]
    }
]

# Initialize default templates
for tpl in DEFAULT_TEMPLATES:
    templates[tpl["id"]] = tpl


class TemplateCreate(BaseModel):
    name: str
    description: Optional[str] = None
    category: str = "feature"
    icon: Optional[str] = "document"
    color: Optional[str] = "#6B7280"
    project_id: Optional[str] = None
    fields: Optional[Dict[str, Any]] = {}
    acceptance_criteria: Optional[List[str]] = []
    definition_of_done: Optional[List[str]] = []
    tags: Optional[List[str]] = []


class TemplateUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    category: Optional[str] = None
    icon: Optional[str] = None
    color: Optional[str] = None
    fields: Optional[Dict[str, Any]] = None
    acceptance_criteria: Optional[List[str]] = None
    definition_of_done: Optional[List[str]] = None
    tags: Optional[List[str]] = None


@router.get("/")
async def list_templates(
    project_id: Optional[str] = Query(None),
    category: Optional[str] = Query(None),
    include_system: bool = Query(True)
):
    """Lista templates disponiveis."""
    result = []

    for tpl in templates.values():
        # Filter by project
        if project_id and tpl.get("project_id") and tpl["project_id"] != project_id:
            continue

        # Filter by category
        if category and tpl.get("category") != category:
            continue

        # Filter system templates
        if not include_system and tpl.get("is_system"):
            continue

        result.append(tpl)

    # Sort: custom templates first, then system
    result.sort(key=lambda x: (x.get("is_system", False), x.get("name", "")))

    return {
        "templates": result,
        "categories": list(set(t.get("category", "other") for t in templates.values()))
    }


@router.get("/{template_id}")
async def get_template(template_id: str):
    """Retorna um template especifico."""
    if template_id not in templates:
        raise HTTPException(status_code=404, detail="Template nao encontrado")
    return templates[template_id]


@router.post("/")
async def create_template(template: TemplateCreate):
    """Cria um novo template customizado."""
    template_id = f"tpl_{uuid.uuid4().hex[:8]}"

    new_template = {
        "id": template_id,
        "name": template.name,
        "description": template.description,
        "category": template.category,
        "icon": template.icon,
        "color": template.color,
        "project_id": template.project_id,
        "is_system": False,
        "fields": template.fields,
        "acceptance_criteria": template.acceptance_criteria,
        "definition_of_done": template.definition_of_done,
        "tags": template.tags,
        "created_at": datetime.now().isoformat(),
        "usage_count": 0
    }

    templates[template_id] = new_template

    return {
        "success": True,
        "template": new_template
    }


@router.put("/{template_id}")
async def update_template(template_id: str, template: TemplateUpdate):
    """Atualiza um template customizado."""
    if template_id not in templates:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    tpl = templates[template_id]

    if tpl.get("is_system"):
        raise HTTPException(status_code=400, detail="Templates do sistema nao podem ser editados")

    # Update fields
    updates = template.dict(exclude_none=True)
    tpl.update(updates)
    tpl["updated_at"] = datetime.now().isoformat()

    return {
        "success": True,
        "template": tpl
    }


@router.delete("/{template_id}")
async def delete_template(template_id: str):
    """Exclui um template customizado."""
    if template_id not in templates:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    if templates[template_id].get("is_system"):
        raise HTTPException(status_code=400, detail="Templates do sistema nao podem ser excluidos")

    del templates[template_id]

    return {
        "success": True,
        "message": "Template excluido"
    }


@router.post("/{template_id}/apply")
async def apply_template(template_id: str, overrides: Optional[Dict[str, Any]] = None):
    """Aplica um template para criar uma nova story."""
    if template_id not in templates:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    tpl = templates[template_id]

    # Increment usage count
    tpl["usage_count"] = tpl.get("usage_count", 0) + 1

    # Build story data from template
    story_data = {
        "title": "",
        "description": "",
        "category": tpl.get("category", "feature"),
        **tpl.get("fields", {}),
        "acceptance_criteria": tpl.get("acceptance_criteria", []),
        "definition_of_done": tpl.get("definition_of_done", []),
        "tags": tpl.get("tags", []),
        "template_id": template_id
    }

    # Apply overrides
    if overrides:
        story_data.update(overrides)

    return {
        "success": True,
        "story_data": story_data,
        "template_name": tpl["name"]
    }


@router.post("/{template_id}/duplicate")
async def duplicate_template(template_id: str, new_name: Optional[str] = None):
    """Duplica um template."""
    if template_id not in templates:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    original = templates[template_id]
    new_id = f"tpl_{uuid.uuid4().hex[:8]}"

    duplicated = {
        **original,
        "id": new_id,
        "name": new_name or f"{original['name']} (Copy)",
        "is_system": False,
        "created_at": datetime.now().isoformat(),
        "usage_count": 0
    }

    templates[new_id] = duplicated

    return {
        "success": True,
        "template": duplicated
    }


@router.get("/export/all")
async def export_templates(project_id: Optional[str] = Query(None)):
    """Exporta todos os templates customizados."""
    custom_templates = [
        t for t in templates.values()
        if not t.get("is_system") and (not project_id or t.get("project_id") == project_id)
    ]

    return {
        "templates": custom_templates,
        "exported_at": datetime.now().isoformat(),
        "count": len(custom_templates)
    }


@router.post("/import")
async def import_templates(data: Dict[str, Any]):
    """Importa templates de um arquivo."""
    imported = 0

    for tpl in data.get("templates", []):
        # Generate new ID to avoid conflicts
        new_id = f"tpl_{uuid.uuid4().hex[:8]}"

        templates[new_id] = {
            **tpl,
            "id": new_id,
            "is_system": False,
            "imported_at": datetime.now().isoformat()
        }
        imported += 1

    return {
        "success": True,
        "imported": imported
    }


def get_templates_html():
    """Retorna o HTML do seletor de templates."""
    return '''
    <!-- Story Templates (Issue #254) -->
    <div v-if="showTemplateSelector"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showTemplateSelector = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-3xl mx-4 max-h-[80vh] overflow-hidden flex flex-col">
            <!-- Header -->
            <div class="px-6 py-4 border-b flex items-center justify-between">
                <div>
                    <h2 class="text-lg font-bold">Escolha um Template</h2>
                    <p class="text-sm text-gray-500">Selecione um template para criar sua story</p>
                </div>
                <button @click="showTemplateSelector = false" class="text-gray-400 hover:text-gray-600">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <!-- Categories -->
            <div class="px-6 py-3 border-b flex gap-2 overflow-x-auto">
                <button @click="templateCategory = ''"
                        :class="['px-3 py-1 rounded-full text-sm', !templateCategory ? 'bg-blue-600 text-white' : 'bg-gray-100']">
                    Todos
                </button>
                <button v-for="cat in templateCategories" :key="cat"
                        @click="templateCategory = cat"
                        :class="['px-3 py-1 rounded-full text-sm whitespace-nowrap',
                                 templateCategory === cat ? 'bg-blue-600 text-white' : 'bg-gray-100']">
                    {{ cat }}
                </button>
            </div>

            <!-- Templates Grid -->
            <div class="flex-1 overflow-y-auto p-6">
                <div class="grid grid-cols-2 md:grid-cols-3 gap-4">
                    <!-- Start from Scratch -->
                    <div @click="createStoryFromScratch"
                         class="p-4 border-2 border-dashed border-gray-300 rounded-xl hover:border-blue-400 hover:bg-blue-50 cursor-pointer transition-all">
                        <div class="w-10 h-10 bg-gray-100 rounded-lg flex items-center justify-center mb-3">
                            <svg class="w-6 h-6 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/>
                            </svg>
                        </div>
                        <h3 class="font-semibold">Em Branco</h3>
                        <p class="text-xs text-gray-500 mt-1">Comecar do zero</p>
                    </div>

                    <!-- Template Cards -->
                    <div v-for="tpl in filteredTemplates" :key="tpl.id"
                         @click="selectTemplate(tpl)"
                         class="p-4 border-2 border-gray-200 rounded-xl hover:border-blue-400 hover:shadow cursor-pointer transition-all">
                        <div class="w-10 h-10 rounded-lg flex items-center justify-center mb-3"
                             :style="{ backgroundColor: tpl.color + '20' }">
                            <span class="text-lg">{{ getTemplateIcon(tpl.icon) }}</span>
                        </div>
                        <h3 class="font-semibold">{{ tpl.name }}</h3>
                        <p class="text-xs text-gray-500 mt-1 line-clamp-2">{{ tpl.description }}</p>
                        <div class="flex items-center gap-2 mt-2">
                            <span v-if="tpl.is_system" class="text-xs bg-gray-100 text-gray-500 px-2 py-0.5 rounded">
                                Sistema
                            </span>
                            <span v-else class="text-xs bg-blue-100 text-blue-600 px-2 py-0.5 rounded">
                                Customizado
                            </span>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-3 bg-gray-50 border-t flex justify-between">
                <button @click="openTemplateManager"
                        class="text-sm text-blue-600 hover:underline">
                    Gerenciar Templates
                </button>
                <button @click="showTemplateSelector = false"
                        class="px-4 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded-lg">
                    Cancelar
                </button>
            </div>
        </div>
    </div>

    <!-- Template Manager Modal -->
    <div v-if="showTemplateManager"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showTemplateManager = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-4xl mx-4 max-h-[85vh] overflow-hidden flex flex-col">
            <div class="px-6 py-4 border-b flex items-center justify-between">
                <h2 class="text-lg font-bold">Gerenciar Templates</h2>
                <div class="flex gap-2">
                    <button @click="exportAllTemplates"
                            class="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 rounded">
                        Exportar
                    </button>
                    <button @click="$refs.importTemplatesInput.click()"
                            class="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 rounded">
                        Importar
                    </button>
                    <input type="file" ref="importTemplatesInput" @change="importTemplates" class="hidden" accept=".json">
                    <button @click="showNewTemplateForm = true"
                            class="px-3 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700">
                        Novo Template
                    </button>
                    <button @click="showTemplateManager = false" class="text-gray-400 hover:text-gray-600 ml-2">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
            </div>

            <div class="flex-1 overflow-y-auto p-6">
                <table class="w-full">
                    <thead class="bg-gray-50">
                        <tr>
                            <th class="px-4 py-2 text-left text-sm font-medium">Nome</th>
                            <th class="px-4 py-2 text-left text-sm font-medium">Categoria</th>
                            <th class="px-4 py-2 text-left text-sm font-medium">Tipo</th>
                            <th class="px-4 py-2 text-left text-sm font-medium">Usos</th>
                            <th class="px-4 py-2 text-left text-sm font-medium">Acoes</th>
                        </tr>
                    </thead>
                    <tbody class="divide-y">
                        <tr v-for="tpl in allTemplates" :key="tpl.id" class="hover:bg-gray-50">
                            <td class="px-4 py-3">
                                <div class="flex items-center gap-2">
                                    <div class="w-6 h-6 rounded flex items-center justify-center text-sm"
                                         :style="{ backgroundColor: tpl.color + '20' }">
                                        {{ getTemplateIcon(tpl.icon) }}
                                    </div>
                                    <span class="font-medium">{{ tpl.name }}</span>
                                </div>
                            </td>
                            <td class="px-4 py-3 text-sm text-gray-500">{{ tpl.category }}</td>
                            <td class="px-4 py-3">
                                <span :class="['text-xs px-2 py-0.5 rounded',
                                               tpl.is_system ? 'bg-gray-100 text-gray-600' : 'bg-blue-100 text-blue-600']">
                                    {{ tpl.is_system ? 'Sistema' : 'Customizado' }}
                                </span>
                            </td>
                            <td class="px-4 py-3 text-sm text-gray-500">{{ tpl.usage_count || 0 }}</td>
                            <td class="px-4 py-3">
                                <div class="flex gap-1">
                                    <button @click="duplicateTemplate(tpl)"
                                            class="p-1 text-gray-400 hover:text-blue-600" title="Duplicar">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                                        </svg>
                                    </button>
                                    <button v-if="!tpl.is_system" @click="editTemplate(tpl)"
                                            class="p-1 text-gray-400 hover:text-blue-600" title="Editar">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z"/>
                                        </svg>
                                    </button>
                                    <button v-if="!tpl.is_system" @click="deleteTemplate(tpl.id)"
                                            class="p-1 text-gray-400 hover:text-red-600" title="Excluir">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                        </svg>
                                    </button>
                                </div>
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>
    </div>
    '''


def get_templates_js():
    """Retorna o JavaScript para templates."""
    return '''
    // Templates State
    showTemplateSelector: false,
    showTemplateManager: false,
    showNewTemplateForm: false,
    templateCategory: '',
    templateCategories: [],
    allTemplates: [],

    // Template Methods
    async loadTemplates() {
        try {
            const response = await fetch(`/api/templates/?project_id=${this.currentProject}`);
            const data = await response.json();
            this.allTemplates = data.templates;
            this.templateCategories = data.categories;
        } catch (e) {
            console.error('Error loading templates:', e);
        }
    },

    get filteredTemplates() {
        if (!this.templateCategory) return this.allTemplates;
        return this.allTemplates.filter(t => t.category === this.templateCategory);
    },

    getTemplateIcon(icon) {
        const icons = {
            'sparkles': '✨',
            'bug': '🐛',
            'wrench': '🔧',
            'magnifying-glass': '🔍',
            'code': '💻',
            'squares': '🎨',
            'link': '🔗',
            'shield': '🛡️',
            'document': '📄'
        };
        return icons[icon] || '📋';
    },

    openTemplateSelector() {
        this.loadTemplates();
        this.showTemplateSelector = true;
    },

    async selectTemplate(template) {
        try {
            const response = await fetch(`/api/templates/${template.id}/apply`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({})
            });
            const data = await response.json();

            // Pre-fill story form with template data
            this.newStory = {
                ...this.newStory,
                ...data.story_data,
                title: ''  // User should fill title
            };

            this.showTemplateSelector = false;
            this.showNewStoryModal = true;

            this.showNotification(`Template "${template.name}" aplicado`, 'success');
        } catch (e) {
            this.showNotification('Erro ao aplicar template', 'error');
        }
    },

    createStoryFromScratch() {
        this.newStory = {
            project_id: this.currentProject,
            title: '',
            description: '',
            acceptance_criteria: [],
            definition_of_done: [],
            tags: []
        };
        this.showTemplateSelector = false;
        this.showNewStoryModal = true;
    },

    openTemplateManager() {
        this.showTemplateSelector = false;
        this.showTemplateManager = true;
    },

    async duplicateTemplate(template) {
        try {
            const response = await fetch(`/api/templates/${template.id}/duplicate`, {
                method: 'POST'
            });
            const data = await response.json();
            this.allTemplates.push(data.template);
            this.showNotification('Template duplicado', 'success');
        } catch (e) {
            this.showNotification('Erro ao duplicar template', 'error');
        }
    },

    async deleteTemplate(templateId) {
        if (!confirm('Excluir este template?')) return;

        try {
            await fetch(`/api/templates/${templateId}`, { method: 'DELETE' });
            this.allTemplates = this.allTemplates.filter(t => t.id !== templateId);
            this.showNotification('Template excluido', 'success');
        } catch (e) {
            this.showNotification('Erro ao excluir template', 'error');
        }
    },

    async exportAllTemplates() {
        try {
            const response = await fetch(`/api/templates/export/all?project_id=${this.currentProject}`);
            const data = await response.json();

            const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'story_templates.json';
            a.click();
        } catch (e) {
            this.showNotification('Erro ao exportar templates', 'error');
        }
    },

    async importTemplates(event) {
        const file = event.target.files[0];
        if (!file) return;

        try {
            const text = await file.text();
            const data = JSON.parse(text);

            const response = await fetch('/api/templates/import', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data)
            });

            const result = await response.json();
            this.loadTemplates();
            this.showNotification(`${result.imported} templates importados`, 'success');
        } catch (e) {
            this.showNotification('Erro ao importar templates', 'error');
        }
    }
    '''


def register_story_templates(app):
    """Registra os endpoints de templates no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Story Templates endpoints loaded: /api/templates/*")
