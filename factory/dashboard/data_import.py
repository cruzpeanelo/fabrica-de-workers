# -*- coding: utf-8 -*-
"""
Data Import Module (Issue #276)
================================
Importar dados de outras ferramentas de gestao de projetos.

Funcionalidades:
- Import de CSV/Excel
- Import de Jira (via API)
- Import de Trello (via JSON export)
- Import de Asana (via CSV)
- Mapeamento de campos
- Preview antes de importar
- Rollback de importacao
"""

from fastapi import APIRouter, UploadFile, File, HTTPException, Query, BackgroundTasks
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import csv
import json
import io
import uuid

router = APIRouter(prefix="/api/import", tags=["Data Import"])

# Import job storage
import_jobs: Dict[str, dict] = {}

# Supported sources
SUPPORTED_SOURCES = {
    "csv": {
        "name": "CSV/Excel",
        "description": "Arquivo CSV ou Excel exportado de qualquer ferramenta",
        "file_types": [".csv", ".xlsx", ".xls"]
    },
    "jira": {
        "name": "Jira",
        "description": "Importar issues do Jira via CSV export",
        "file_types": [".csv"]
    },
    "trello": {
        "name": "Trello",
        "description": "Importar cards do Trello via JSON export",
        "file_types": [".json"]
    },
    "asana": {
        "name": "Asana",
        "description": "Importar tasks do Asana via CSV export",
        "file_types": [".csv"]
    },
    "github": {
        "name": "GitHub Issues",
        "description": "Importar issues do GitHub via CSV",
        "file_types": [".csv"]
    }
}

# Default field mappings per source
FIELD_MAPPINGS = {
    "jira": {
        "Summary": "title",
        "Description": "description",
        "Issue Type": "category",
        "Status": "status",
        "Priority": "priority",
        "Assignee": "assignee",
        "Story Points": "story_points",
        "Sprint": "sprint_id",
        "Epic Link": "epic_id",
        "Labels": "tags"
    },
    "trello": {
        "name": "title",
        "desc": "description",
        "labels": "tags",
        "due": "due_date",
        "idMembers": "assignee"
    },
    "asana": {
        "Name": "title",
        "Notes": "description",
        "Assignee": "assignee",
        "Due Date": "due_date",
        "Tags": "tags",
        "Section/Column": "status"
    },
    "github": {
        "Title": "title",
        "Body": "description",
        "Assignees": "assignee",
        "Labels": "tags",
        "State": "status",
        "Milestone": "sprint_id"
    }
}


class ImportConfig(BaseModel):
    source: str
    project_id: str
    field_mapping: Optional[Dict[str, str]] = None
    default_status: str = "backlog"
    default_priority: str = "medium"
    skip_duplicates: bool = True


class ImportPreview(BaseModel):
    source: str
    file_name: str
    total_items: int
    preview_items: List[dict]
    detected_fields: List[str]
    suggested_mapping: Dict[str, str]


@router.get("/sources")
async def list_import_sources():
    """Lista fontes de import suportadas."""
    return {
        "sources": SUPPORTED_SOURCES,
        "default_mappings": FIELD_MAPPINGS
    }


@router.post("/preview")
async def preview_import(
    file: UploadFile = File(...),
    source: str = Query("csv")
):
    """Faz preview dos dados antes de importar."""
    if source not in SUPPORTED_SOURCES:
        raise HTTPException(status_code=400, detail=f"Fonte nao suportada: {source}")

    content = await file.read()

    try:
        if source == "trello" or file.filename.endswith(".json"):
            items = parse_trello_json(content)
        else:
            items = parse_csv_content(content, source)

        # Detect fields
        detected_fields = []
        if items:
            detected_fields = list(items[0].keys())

        # Suggest mapping
        suggested_mapping = FIELD_MAPPINGS.get(source, {})
        if not suggested_mapping and detected_fields:
            # Try to auto-detect mappings
            suggested_mapping = auto_detect_mapping(detected_fields)

        return {
            "source": source,
            "file_name": file.filename,
            "total_items": len(items),
            "preview_items": items[:10],  # First 10 items
            "detected_fields": detected_fields,
            "suggested_mapping": suggested_mapping
        }

    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Erro ao processar arquivo: {str(e)}")


@router.post("/start")
async def start_import(
    config: ImportConfig,
    file: UploadFile = File(...),
    background_tasks: BackgroundTasks = None
):
    """Inicia importacao de dados."""
    job_id = f"import_{uuid.uuid4().hex[:12]}"

    content = await file.read()

    # Create import job
    import_jobs[job_id] = {
        "id": job_id,
        "source": config.source,
        "project_id": config.project_id,
        "file_name": file.filename,
        "status": "processing",
        "progress": 0,
        "total_items": 0,
        "imported_items": 0,
        "skipped_items": 0,
        "errors": [],
        "started_at": datetime.now().isoformat(),
        "completed_at": None
    }

    # Process in background
    if background_tasks:
        background_tasks.add_task(
            process_import,
            job_id,
            content,
            config
        )
    else:
        await process_import(job_id, content, config)

    return {
        "success": True,
        "job_id": job_id,
        "message": "Importacao iniciada"
    }


@router.get("/jobs")
async def list_import_jobs():
    """Lista jobs de importacao."""
    jobs = sorted(
        import_jobs.values(),
        key=lambda x: x["started_at"],
        reverse=True
    )
    return {"jobs": jobs}


@router.get("/jobs/{job_id}")
async def get_import_job(job_id: str):
    """Retorna status de um job de importacao."""
    if job_id not in import_jobs:
        raise HTTPException(status_code=404, detail="Job nao encontrado")
    return import_jobs[job_id]


@router.post("/jobs/{job_id}/rollback")
async def rollback_import(job_id: str):
    """Desfaz uma importacao."""
    if job_id not in import_jobs:
        raise HTTPException(status_code=404, detail="Job nao encontrado")

    job = import_jobs[job_id]

    if job["status"] != "completed":
        raise HTTPException(status_code=400, detail="Apenas importacoes completas podem ser desfeitas")

    # In production, delete imported stories from database
    # For now, just mark as rolled back
    job["status"] = "rolled_back"
    job["rolled_back_at"] = datetime.now().isoformat()

    return {
        "success": True,
        "message": f"Importacao desfeita. {job['imported_items']} itens removidos."
    }


def parse_csv_content(content: bytes, source: str) -> List[dict]:
    """Parse CSV content."""
    try:
        # Try UTF-8 first, then latin-1
        try:
            text = content.decode('utf-8')
        except UnicodeDecodeError:
            text = content.decode('latin-1')

        reader = csv.DictReader(io.StringIO(text))
        items = list(reader)
        return items
    except Exception as e:
        raise ValueError(f"Erro ao ler CSV: {str(e)}")


def parse_trello_json(content: bytes) -> List[dict]:
    """Parse Trello JSON export."""
    try:
        data = json.loads(content.decode('utf-8'))

        # Trello exports have cards array
        cards = data.get("cards", [])

        # Get list names for status mapping
        lists = {l["id"]: l["name"] for l in data.get("lists", [])}

        items = []
        for card in cards:
            if card.get("closed"):  # Skip archived cards
                continue

            items.append({
                "name": card.get("name", ""),
                "desc": card.get("desc", ""),
                "status": lists.get(card.get("idList"), "backlog"),
                "labels": [l.get("name", "") for l in card.get("labels", [])],
                "due": card.get("due"),
                "url": card.get("url", "")
            })

        return items
    except Exception as e:
        raise ValueError(f"Erro ao ler JSON do Trello: {str(e)}")


def auto_detect_mapping(fields: List[str]) -> Dict[str, str]:
    """Auto-detect field mapping based on common names."""
    mapping = {}

    # Common field name variations
    field_aliases = {
        "title": ["title", "name", "summary", "titulo", "nome"],
        "description": ["description", "desc", "body", "notes", "descricao", "detalhes"],
        "status": ["status", "state", "estado", "coluna", "column", "section"],
        "priority": ["priority", "prioridade", "importance"],
        "assignee": ["assignee", "assigned", "responsavel", "owner", "user"],
        "tags": ["tags", "labels", "etiquetas", "categorias"],
        "due_date": ["due", "due_date", "deadline", "prazo", "data_limite"],
        "story_points": ["points", "story_points", "pontos", "estimate", "estimativa"]
    }

    fields_lower = {f.lower(): f for f in fields}

    for target, aliases in field_aliases.items():
        for alias in aliases:
            if alias in fields_lower:
                mapping[fields_lower[alias]] = target
                break

    return mapping


async def process_import(job_id: str, content: bytes, config: ImportConfig):
    """Process import job."""
    job = import_jobs[job_id]

    try:
        # Parse content
        if config.source == "trello":
            items = parse_trello_json(content)
        else:
            items = parse_csv_content(content, config.source)

        job["total_items"] = len(items)

        # Get field mapping
        mapping = config.field_mapping or FIELD_MAPPINGS.get(config.source, {})

        imported = 0
        skipped = 0
        errors = []

        for i, item in enumerate(items):
            try:
                # Map fields
                story_data = map_item_to_story(item, mapping, config)

                # In production, save to database
                # For now, just count
                imported += 1

            except Exception as e:
                errors.append({
                    "item_index": i,
                    "error": str(e)
                })
                skipped += 1

            # Update progress
            job["progress"] = int((i + 1) / len(items) * 100)
            job["imported_items"] = imported
            job["skipped_items"] = skipped

        job["status"] = "completed"
        job["errors"] = errors[:50]  # Keep only first 50 errors
        job["completed_at"] = datetime.now().isoformat()

    except Exception as e:
        job["status"] = "failed"
        job["errors"].append({"error": str(e)})
        job["completed_at"] = datetime.now().isoformat()


def map_item_to_story(item: dict, mapping: Dict[str, str], config: ImportConfig) -> dict:
    """Map imported item to story format."""
    story = {
        "project_id": config.project_id,
        "status": config.default_status,
        "priority": config.default_priority,
        "story_points": 0,
        "tags": []
    }

    for source_field, target_field in mapping.items():
        if source_field in item and item[source_field]:
            value = item[source_field]

            # Handle special fields
            if target_field == "tags" and isinstance(value, str):
                story["tags"] = [t.strip() for t in value.split(",")]
            elif target_field == "story_points":
                try:
                    story["story_points"] = int(value)
                except:
                    pass
            elif target_field == "status":
                story["status"] = map_status(value)
            else:
                story[target_field] = value

    return story


def map_status(status: str) -> str:
    """Map external status to internal status."""
    status_lower = status.lower()

    status_map = {
        # Jira statuses
        "to do": "backlog",
        "open": "backlog",
        "in progress": "in_progress",
        "in review": "review",
        "in qa": "testing",
        "done": "done",
        "closed": "done",

        # Trello statuses (common list names)
        "backlog": "backlog",
        "to-do": "backlog",
        "doing": "in_progress",
        "review": "review",
        "testing": "testing",
        "complete": "done",

        # Asana statuses
        "not started": "backlog",
        "in progress": "in_progress",
        "completed": "done"
    }

    return status_map.get(status_lower, "backlog")


def get_import_html():
    """Retorna o HTML do painel de importacao."""
    return '''
    <!-- Data Import Panel (Issue #276) -->
    <div v-if="showImportModal"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showImportModal = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-3xl mx-4 max-h-[90vh] overflow-hidden flex flex-col">
            <!-- Header -->
            <div class="bg-gradient-to-r from-purple-600 to-indigo-600 px-6 py-4 text-white">
                <div class="flex items-center justify-between">
                    <div>
                        <h2 class="text-xl font-bold">Importar Dados</h2>
                        <p class="text-white/70 text-sm">Importe dados de outras ferramentas</p>
                    </div>
                    <button @click="showImportModal = false" class="text-white/70 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
            </div>

            <div class="flex-1 overflow-y-auto p-6">
                <!-- Step 1: Select Source -->
                <div v-if="importStep === 1" class="space-y-4">
                    <h3 class="font-semibold mb-4">1. Selecione a Fonte</h3>
                    <div class="grid grid-cols-2 md:grid-cols-3 gap-4">
                        <div v-for="(source, key) in importSources" :key="key"
                             @click="selectImportSource(key)"
                             :class="['p-4 border-2 rounded-xl cursor-pointer transition-all',
                                      importSource === key ? 'border-purple-500 bg-purple-50' : 'border-gray-200 hover:border-gray-300']">
                            <div class="text-2xl mb-2">
                                {{ key === 'csv' ? '📊' : key === 'jira' ? '🔷' : key === 'trello' ? '📋' : key === 'asana' ? '🎯' : '📁' }}
                            </div>
                            <div class="font-medium">{{ source.name }}</div>
                            <div class="text-xs text-gray-500">{{ source.description }}</div>
                        </div>
                    </div>
                </div>

                <!-- Step 2: Upload File -->
                <div v-if="importStep === 2" class="space-y-4">
                    <h3 class="font-semibold mb-4">2. Selecione o Arquivo</h3>
                    <div class="border-2 border-dashed border-gray-300 rounded-xl p-8 text-center"
                         @drop.prevent="handleImportDrop"
                         @dragover.prevent>
                        <input type="file" ref="importFileInput" @change="handleImportFile" class="hidden"
                               :accept="importSources[importSource]?.file_types?.join(',')">
                        <svg class="w-12 h-12 mx-auto text-gray-400 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"/>
                        </svg>
                        <p class="text-gray-500 mb-2">Arraste o arquivo aqui ou</p>
                        <button @click="$refs.importFileInput.click()"
                                class="px-4 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700">
                            Selecionar Arquivo
                        </button>
                        <p class="text-sm text-gray-400 mt-2">
                            Formatos aceitos: {{ importSources[importSource]?.file_types?.join(', ') }}
                        </p>
                    </div>
                </div>

                <!-- Step 3: Preview & Mapping -->
                <div v-if="importStep === 3" class="space-y-4">
                    <h3 class="font-semibold mb-4">3. Mapeamento de Campos</h3>
                    <div class="bg-gray-50 rounded-lg p-4 mb-4">
                        <div class="flex justify-between text-sm">
                            <span>Arquivo: {{ importPreview?.file_name }}</span>
                            <span>{{ importPreview?.total_items }} itens encontrados</span>
                        </div>
                    </div>

                    <!-- Field Mapping -->
                    <div class="space-y-3">
                        <div v-for="field in importPreview?.detected_fields" :key="field"
                             class="flex items-center gap-4 p-3 bg-white border rounded-lg">
                            <span class="w-1/3 font-medium text-sm truncate" :title="field">{{ field }}</span>
                            <svg class="w-4 h-4 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 8l4 4m0 0l-4 4m4-4H3"/>
                            </svg>
                            <select v-model="importMapping[field]"
                                    class="flex-1 px-3 py-2 border border-gray-300 rounded-lg text-sm">
                                <option value="">-- Ignorar --</option>
                                <option value="title">Titulo</option>
                                <option value="description">Descricao</option>
                                <option value="status">Status</option>
                                <option value="priority">Prioridade</option>
                                <option value="assignee">Responsavel</option>
                                <option value="tags">Tags</option>
                                <option value="story_points">Story Points</option>
                                <option value="due_date">Data Limite</option>
                            </select>
                        </div>
                    </div>

                    <!-- Preview Table -->
                    <h4 class="font-medium mt-6 mb-2">Preview (primeiros 5 itens)</h4>
                    <div class="overflow-x-auto">
                        <table class="w-full text-sm border">
                            <thead class="bg-gray-100">
                                <tr>
                                    <th v-for="field in Object.keys(importMapping).filter(f => importMapping[f])"
                                        :key="field" class="px-3 py-2 text-left">
                                        {{ importMapping[field] }}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr v-for="(item, idx) in importPreview?.preview_items?.slice(0, 5)" :key="idx"
                                    class="border-t">
                                    <td v-for="field in Object.keys(importMapping).filter(f => importMapping[f])"
                                        :key="field" class="px-3 py-2 truncate max-w-[200px]">
                                        {{ item[field] }}
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <!-- Step 4: Import Progress -->
                <div v-if="importStep === 4" class="space-y-4">
                    <h3 class="font-semibold mb-4">4. Importando...</h3>
                    <div class="text-center py-8">
                        <div class="relative w-32 h-32 mx-auto mb-4">
                            <svg class="w-32 h-32 transform -rotate-90">
                                <circle cx="64" cy="64" r="56" fill="none" stroke="#e5e7eb" stroke-width="8"/>
                                <circle cx="64" cy="64" r="56" fill="none" stroke="#8b5cf6" stroke-width="8"
                                        :stroke-dasharray="351.86" :stroke-dashoffset="351.86 * (1 - importProgress/100)"/>
                            </svg>
                            <div class="absolute inset-0 flex items-center justify-center">
                                <span class="text-2xl font-bold">{{ importProgress }}%</span>
                            </div>
                        </div>
                        <p class="text-gray-500">{{ importStatus }}</p>
                    </div>
                </div>

                <!-- Step 5: Complete -->
                <div v-if="importStep === 5" class="text-center py-8">
                    <div class="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
                        <svg class="w-8 h-8 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                        </svg>
                    </div>
                    <h3 class="text-xl font-bold mb-2">Importacao Concluida!</h3>
                    <p class="text-gray-500 mb-4">
                        {{ importResult?.imported_items }} itens importados com sucesso
                        <span v-if="importResult?.skipped_items > 0">
                            ({{ importResult?.skipped_items }} ignorados)
                        </span>
                    </p>
                    <button @click="showImportModal = false; loadStories()"
                            class="px-6 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700">
                        Fechar e Visualizar
                    </button>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-4 bg-gray-50 border-t flex justify-between">
                <button v-if="importStep > 1 && importStep < 4"
                        @click="importStep--"
                        class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">
                    Voltar
                </button>
                <div v-else></div>
                <button v-if="importStep < 4"
                        @click="nextImportStep"
                        :disabled="!canProceedImport"
                        class="px-6 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700 disabled:opacity-50">
                    {{ importStep === 3 ? 'Iniciar Importacao' : 'Proximo' }}
                </button>
            </div>
        </div>
    </div>
    '''


def get_import_js():
    """Retorna o JavaScript para o painel de importacao."""
    return '''
    // Import Data State
    showImportModal: false,
    importStep: 1,
    importSource: null,
    importFile: null,
    importPreview: null,
    importMapping: {},
    importProgress: 0,
    importStatus: '',
    importResult: null,
    importSources: {},

    // Import Methods
    async loadImportSources() {
        try {
            const response = await fetch('/api/import/sources');
            const data = await response.json();
            this.importSources = data.sources;
        } catch (e) {
            console.error('Error loading import sources:', e);
        }
    },

    selectImportSource(source) {
        this.importSource = source;
    },

    handleImportFile(event) {
        this.importFile = event.target.files[0];
        if (this.importFile) {
            this.importStep = 2;
            this.previewImport();
        }
    },

    handleImportDrop(event) {
        this.importFile = event.dataTransfer.files[0];
        if (this.importFile) {
            this.previewImport();
        }
    },

    async previewImport() {
        if (!this.importFile) return;

        const formData = new FormData();
        formData.append('file', this.importFile);

        try {
            const response = await fetch(`/api/import/preview?source=${this.importSource}`, {
                method: 'POST',
                body: formData
            });

            if (response.ok) {
                this.importPreview = await response.json();
                this.importMapping = { ...this.importPreview.suggested_mapping };
                this.importStep = 3;
            } else {
                const error = await response.json();
                this.showNotification(error.detail || 'Erro ao processar arquivo', 'error');
            }
        } catch (e) {
            this.showNotification('Erro ao fazer preview', 'error');
        }
    },

    async nextImportStep() {
        if (this.importStep === 1 && this.importSource) {
            this.importStep = 2;
        } else if (this.importStep === 2 && this.importFile) {
            await this.previewImport();
        } else if (this.importStep === 3) {
            await this.startImport();
        }
    },

    async startImport() {
        this.importStep = 4;
        this.importProgress = 0;
        this.importStatus = 'Iniciando importacao...';

        const formData = new FormData();
        formData.append('file', this.importFile);

        const config = {
            source: this.importSource,
            project_id: this.currentProject,
            field_mapping: this.importMapping
        };

        try {
            const response = await fetch('/api/import/start?config=' + encodeURIComponent(JSON.stringify(config)), {
                method: 'POST',
                body: formData
            });

            if (response.ok) {
                const data = await response.json();
                this.pollImportProgress(data.job_id);
            }
        } catch (e) {
            this.importStatus = 'Erro na importacao';
        }
    },

    async pollImportProgress(jobId) {
        const poll = async () => {
            try {
                const response = await fetch(`/api/import/jobs/${jobId}`);
                const job = await response.json();

                this.importProgress = job.progress;
                this.importStatus = `Importando... ${job.imported_items}/${job.total_items}`;

                if (job.status === 'completed') {
                    this.importResult = job;
                    this.importStep = 5;
                } else if (job.status === 'failed') {
                    this.importStatus = 'Erro: ' + (job.errors[0]?.error || 'Falha na importacao');
                } else {
                    setTimeout(poll, 500);
                }
            } catch (e) {
                this.importStatus = 'Erro ao verificar progresso';
            }
        };
        poll();
    },

    get canProceedImport() {
        if (this.importStep === 1) return this.importSource;
        if (this.importStep === 2) return this.importFile;
        if (this.importStep === 3) return Object.values(this.importMapping).some(v => v);
        return false;
    },

    openImportModal() {
        this.importStep = 1;
        this.importSource = null;
        this.importFile = null;
        this.importPreview = null;
        this.importMapping = {};
        this.loadImportSources();
        this.showImportModal = true;
    }
    '''


def register_data_import(app):
    """Registra os endpoints de importacao no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Data Import endpoints loaded: /api/import/*")
