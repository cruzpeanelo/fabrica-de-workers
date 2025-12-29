# -*- coding: utf-8 -*-
"""
Project Preview Dashboard - Issue #73
=====================================
Visao unificada do projeto com:
- Overview do projeto
- Sumario de stories
- Arvore de arquivos gerados
- Links de documentacao
- Resumo de testes
- Acoes rapidas
"""

from pathlib import Path
from typing import Dict, List
from fastapi import APIRouter, HTTPException
from fastapi.responses import HTMLResponse

from factory.database.connection import SessionLocal
from factory.database.repositories import ProjectRepository, StoryRepository
from factory.core.app_generator import analyze_project

router = APIRouter()


def get_project_files_tree(project_path: Path, max_depth: int = 3) -> List[Dict]:
    """Retorna arvore de arquivos do projeto."""
    if not project_path or not project_path.exists():
        return []

    def scan_dir(path: Path, depth: int = 0):
        if depth > max_depth:
            return []
        items = []
        try:
            for item in sorted(path.iterdir()):
                if item.name.startswith('.') or item.name in ['__pycache__', 'node_modules', '.git', 'venv', '.venv']:
                    continue
                entry = {
                    "name": item.name,
                    "path": str(item.relative_to(project_path)),
                    "type": "directory" if item.is_dir() else "file"
                }
                if item.is_dir():
                    entry["children"] = scan_dir(item, depth + 1)
                else:
                    entry["extension"] = item.suffix.lower()
                    entry["size"] = item.stat().st_size
                items.append(entry)
        except PermissionError:
            pass
        return items

    return scan_dir(project_path)


def get_project_docs(project_path: Path) -> List[Dict]:
    """Lista documentacoes do projeto."""
    if not project_path or not project_path.exists():
        return []
    docs = []
    doc_extensions = ['.md', '.rst', '.txt']
    doc_names = ['readme', 'changelog', 'contributing', 'license', 'docs', 'doc', 'documentation']
    for item in project_path.rglob('*'):
        if item.is_file():
            name_lower = item.stem.lower()
            ext_lower = item.suffix.lower()
            if ext_lower in doc_extensions or name_lower in doc_names:
                docs.append({
                    "name": item.name,
                    "path": str(item.relative_to(project_path)),
                    "type": "documentation",
                    "size": item.stat().st_size
                })
    return docs[:20]


def get_test_results_summary(project_path: Path) -> Dict:
    """Retorna resumo dos resultados de testes."""
    if not project_path or not project_path.exists():
        return {"status": "not_found", "total": 0, "passed": 0, "failed": 0, "skipped": 0}
    test_files = list(project_path.rglob("test_*.py")) + list(project_path.rglob("*_test.py"))
    test_files += list(project_path.rglob("*.test.js")) + list(project_path.rglob("*.spec.js"))
    test_files += list(project_path.rglob("*.test.ts")) + list(project_path.rglob("*.spec.ts"))
    return {
        "status": "available" if test_files else "no_tests",
        "test_files_count": len(test_files),
        "total": 0,
        "passed": 0,
        "failed": 0,
        "skipped": 0,
        "coverage": None
    }


@router.get("/api/projects/{project_id}/preview-data")
def get_project_preview_data(project_id: str):
    """Retorna dados consolidados para o Project Preview Dashboard."""
    db = SessionLocal()
    try:
        project_repo = ProjectRepository(db)
        project = project_repo.get_by_id(project_id)
        if not project:
            raise HTTPException(404, "Projeto nao encontrado")

        project_data = project.to_dict()

        story_repo = StoryRepository(db)
        stories = story_repo.get_all(project_id=project_id)

        story_summary = {
            "total": len(stories),
            "backlog": sum(1 for s in stories if s.status.value in ['backlog', 'ready']),
            "in_progress": sum(1 for s in stories if s.status.value in ['in_progress', 'review']),
            "testing": sum(1 for s in stories if s.status.value == 'testing'),
            "done": sum(1 for s in stories if s.status.value == 'done'),
            "total_points": sum(s.story_points or 0 for s in stories),
            "completed_points": sum(s.story_points or 0 for s in stories if s.status.value == 'done')
        }

        if story_summary["total"] > 0:
            completed = story_summary["done"]
            in_progress = story_summary["in_progress"] * 0.5 + story_summary["testing"] * 0.8
            story_summary["progress"] = round(((completed + in_progress) / story_summary["total"]) * 100)
        else:
            story_summary["progress"] = 0

        try:
            app_analysis = analyze_project(project_id)
        except Exception:
            app_analysis = {"status": "not_found", "ready_to_test": False}

        project_path = None
        if app_analysis.get("project_path"):
            project_path = Path(app_analysis["project_path"])

        files_tree = get_project_files_tree(project_path) if project_path else []
        docs = get_project_docs(project_path) if project_path else []
        test_results = get_test_results_summary(project_path) if project_path else {"status": "not_found"}

        recent_stories = [
            {
                "story_id": s.story_id,
                "title": s.title,
                "status": s.status.value,
                "progress": s.progress or 0,
                "story_points": s.story_points or 0
            }
            for s in sorted(stories, key=lambda x: x.updated_at or x.created_at, reverse=True)[:5]
        ]

        return {
            "project": project_data,
            "stories_summary": story_summary,
            "recent_stories": recent_stories,
            "app_status": app_analysis,
            "files_tree": files_tree,
            "documentation": docs,
            "test_results": test_results,
            "quick_actions": {
                "can_generate_app": app_analysis.get("can_generate_app", False),
                "can_run_tests": test_results.get("test_files_count", 0) > 0,
                "can_view_docs": len(docs) > 0,
                "ready_to_test": app_analysis.get("ready_to_test", False)
            }
        }
    finally:
        db.close()


@router.get("/project-preview/{project_id}", response_class=HTMLResponse)
def project_preview_page(project_id: str):
    """Pagina de Preview unificado do projeto - Issue #73"""
    return PROJECT_PREVIEW_TEMPLATE.replace("{{PROJECT_ID}}", project_id)


# =============================================================================
# PROJECT PREVIEW HTML TEMPLATE
# =============================================================================

PROJECT_PREVIEW_TEMPLATE = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Project Preview - Fabrica de Agentes</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <style>
        * { font-family: 'Inter', sans-serif; }
        :root { --belgo-blue: #003B4A; --belgo-orange: #FF6C00; --belgo-light: #E8F4F7; }
        .belgo-blue { background-color: var(--belgo-blue); }
        .belgo-orange { background-color: var(--belgo-orange); }
        .text-belgo-blue { color: var(--belgo-blue); }
        .text-belgo-orange { color: var(--belgo-orange); }
        .file-tree-item { transition: all 0.2s; }
        .file-tree-item:hover { background-color: #f3f4f6; }
        .card-hover { transition: transform 0.2s, box-shadow 0.2s; }
        .card-hover:hover { transform: translateY(-2px); box-shadow: 0 8px 25px rgba(0,0,0,0.1); }
        .progress-ring { transform: rotate(-90deg); }
        .status-badge { padding: 4px 12px; border-radius: 9999px; font-size: 12px; font-weight: 500; }
        .status-backlog { background: #E5E7EB; color: #374151; }
        .status-in_progress { background: #DBEAFE; color: #1D4ED8; }
        .status-review { background: #FEF3C7; color: #D97706; }
        .status-testing { background: #E0E7FF; color: #4338CA; }
        .status-done { background: #D1FAE5; color: #059669; }
        .loading-pulse { animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite; }
        @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: .5; } }
        .slide-in { animation: slideIn 0.3s ease-out; }
        @keyframes slideIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <div id="app">
        <!-- Header -->
        <header class="belgo-blue text-white shadow-lg sticky top-0 z-50">
            <div class="container mx-auto px-6 py-4">
                <div class="flex items-center justify-between">
                    <div class="flex items-center gap-4">
                        <a href="/" class="flex items-center gap-2 hover:opacity-80 transition">
                            <div class="w-10 h-10 bg-white rounded-lg flex items-center justify-center">
                                <span class="text-belgo-blue font-bold text-lg">FA</span>
                            </div>
                            <div>
                                <span class="font-semibold text-lg">{{ project?.name || 'Carregando...' }}</span>
                                <span class="text-gray-300 text-sm block">Project Preview</span>
                            </div>
                        </a>
                    </div>
                    <div class="flex items-center gap-4">
                        <span :class="['status-badge', getStatusClass(appStatus?.status)]">
                            {{ getStatusText(appStatus?.status) }}
                        </span>
                        <a href="/" class="bg-white/10 hover:bg-white/20 px-4 py-2 rounded-lg text-sm transition">
                            Voltar ao Dashboard
                        </a>
                    </div>
                </div>
            </div>
        </header>

        <!-- Loading State -->
        <div v-if="loading" class="flex items-center justify-center h-96">
            <div class="text-center">
                <div class="w-16 h-16 border-4 border-gray-200 border-t-[#003B4A] rounded-full animate-spin mx-auto mb-4"></div>
                <p class="text-gray-500">Carregando dados do projeto...</p>
            </div>
        </div>

        <!-- Main Content -->
        <main v-else class="container mx-auto px-6 py-8">
            <!-- Project Overview Section -->
            <section class="mb-8 slide-in">
                <div class="bg-white rounded-xl shadow-sm p-6">
                    <div class="flex flex-col lg:flex-row gap-8">
                        <!-- Progress Circle -->
                        <div class="flex-shrink-0 flex flex-col items-center">
                            <div class="relative w-40 h-40">
                                <svg class="w-40 h-40 progress-ring" viewBox="0 0 100 100">
                                    <circle cx="50" cy="50" r="45" fill="none" stroke="#E5E7EB" stroke-width="8"/>
                                    <circle cx="50" cy="50" r="45" fill="none" stroke="#10B981" stroke-width="8"
                                            :stroke-dasharray="circumference"
                                            :stroke-dashoffset="progressOffset"
                                            stroke-linecap="round"/>
                                </svg>
                                <div class="absolute inset-0 flex flex-col items-center justify-center">
                                    <span class="text-4xl font-bold text-gray-800">{{ storiesSummary?.progress || 0 }}%</span>
                                    <span class="text-sm text-gray-500">Concluido</span>
                                </div>
                            </div>
                            <p class="mt-2 text-sm text-gray-500">Progresso Geral</p>
                        </div>

                        <!-- Project Info -->
                        <div class="flex-1">
                            <h1 class="text-2xl font-bold text-gray-800 mb-2">{{ project?.name }}</h1>
                            <p class="text-gray-600 mb-4">{{ project?.description || 'Sem descricao' }}</p>

                            <!-- Stats Grid -->
                            <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
                                <div class="bg-gray-50 rounded-lg p-4 text-center">
                                    <div class="text-2xl font-bold text-gray-800">{{ storiesSummary?.total || 0 }}</div>
                                    <div class="text-sm text-gray-500">Total Stories</div>
                                </div>
                                <div class="bg-blue-50 rounded-lg p-4 text-center">
                                    <div class="text-2xl font-bold text-blue-600">{{ storiesSummary?.in_progress || 0 }}</div>
                                    <div class="text-sm text-gray-500">Em Progresso</div>
                                </div>
                                <div class="bg-purple-50 rounded-lg p-4 text-center">
                                    <div class="text-2xl font-bold text-purple-600">{{ storiesSummary?.testing || 0 }}</div>
                                    <div class="text-sm text-gray-500">Em Teste</div>
                                </div>
                                <div class="bg-green-50 rounded-lg p-4 text-center">
                                    <div class="text-2xl font-bold text-green-600">{{ storiesSummary?.done || 0 }}</div>
                                    <div class="text-sm text-gray-500">Concluidas</div>
                                </div>
                            </div>

                            <!-- Timeline -->
                            <div class="flex items-center gap-2">
                                <template v-for="(step, index) in projectSteps" :key="index">
                                    <div class="flex items-center gap-2">
                                        <div :class="['w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium',
                                                     step.completed ? 'bg-green-500 text-white' :
                                                     step.current ? 'bg-blue-500 text-white' : 'bg-gray-200 text-gray-500']">
                                            <span v-if="step.completed">&#10003;</span>
                                            <span v-else-if="step.current">&#9679;</span>
                                            <span v-else>&#9675;</span>
                                        </div>
                                        <span :class="['text-sm', step.completed || step.current ? 'text-gray-700' : 'text-gray-400']">
                                            {{ step.name }}
                                        </span>
                                    </div>
                                    <div v-if="index < projectSteps.length - 1"
                                         :class="['flex-1 h-0.5', step.completed ? 'bg-green-500' : 'bg-gray-200']"></div>
                                </template>
                            </div>
                        </div>
                    </div>
                </div>
            </section>

            <!-- Main Grid -->
            <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
                <!-- Left Column - Files & Docs -->
                <div class="lg:col-span-2 space-y-6">
                    <!-- Preview & App Testing -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <div class="flex items-center justify-between mb-4">
                            <h2 class="text-lg font-semibold text-gray-800 flex items-center gap-2">
                                <span class="text-2xl">&#128187;</span> Preview da Aplicacao
                            </h2>
                            <div class="flex gap-2">
                                <button v-if="quickActions?.can_generate_app && !quickActions?.ready_to_test"
                                        @click="generateApp"
                                        :disabled="generatingApp"
                                        class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg text-sm hover:bg-orange-600 disabled:opacity-50 transition flex items-center gap-2">
                                    <span v-if="generatingApp" class="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin"></span>
                                    <span v-else>&#9881;</span>
                                    {{ generatingApp ? 'Gerando...' : 'Gerar App' }}
                                </button>
                                <button v-if="quickActions?.ready_to_test"
                                        @click="startApp"
                                        :disabled="startingApp"
                                        class="px-4 py-2 bg-green-500 text-white rounded-lg text-sm hover:bg-green-600 disabled:opacity-50 transition flex items-center gap-2">
                                    <span v-if="startingApp" class="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin"></span>
                                    <span v-else>&#9654;</span>
                                    {{ startingApp ? 'Iniciando...' : 'Testar App' }}
                                </button>
                            </div>
                        </div>

                        <!-- App Status Card -->
                        <div :class="['rounded-lg p-5', getAppStatusBgClass()]">
                            <div class="flex items-start gap-4">
                                <div class="text-4xl">{{ getAppStatusIcon() }}</div>
                                <div class="flex-1">
                                    <h4 :class="['font-semibold text-lg', getAppStatusTextClass()]">
                                        {{ getAppStatusTitle() }}
                                    </h4>
                                    <p :class="['mt-1', getAppStatusTextClass()]">{{ appStatus?.message || 'Verificando status do projeto...' }}</p>
                                    <div v-if="appStatus?.models?.length" class="mt-3">
                                        <p class="text-xs text-gray-500 mb-1">Modelos detectados:</p>
                                        <div class="flex flex-wrap gap-1">
                                            <span v-for="model in appStatus.models.slice(0, 6)" :key="model.name"
                                                  class="px-2 py-1 bg-white/50 rounded text-xs">{{ model.name }}</span>
                                            <span v-if="appStatus.models.length > 6" class="px-2 py-1 bg-white/30 rounded text-xs">
                                                +{{ appStatus.models.length - 6 }}
                                            </span>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </section>

                    <!-- Files Tree -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <div class="flex items-center justify-between mb-4">
                            <h2 class="text-lg font-semibold text-gray-800 flex items-center gap-2">
                                <span class="text-2xl">&#128193;</span> Arquivos do Projeto
                            </h2>
                            <span class="text-sm text-gray-500">{{ countFiles(filesTree) }} arquivos</span>
                        </div>
                        <div v-if="filesTree.length" class="max-h-96 overflow-y-auto border rounded-lg">
                            <div v-for="item in filesTree" :key="item.path">
                                <file-tree-item :item="item" :level="0"></file-tree-item>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500">
                            <span class="text-4xl block mb-2">&#128194;</span>
                            <p>Nenhum arquivo encontrado</p>
                            <p class="text-sm">Os arquivos aparecerao aqui quando os workers gerarem codigo</p>
                        </div>
                    </section>

                    <!-- Test Results -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <div class="flex items-center justify-between mb-4">
                            <h2 class="text-lg font-semibold text-gray-800 flex items-center gap-2">
                                <span class="text-2xl">&#129514;</span> Resultados de Testes
                            </h2>
                            <button v-if="quickActions?.can_run_tests"
                                    @click="runTests"
                                    class="px-4 py-2 bg-blue-500 text-white rounded-lg text-sm hover:bg-blue-600 transition">
                                Executar Testes
                            </button>
                        </div>
                        <div v-if="testResults?.test_files_count > 0" class="space-y-4">
                            <div class="grid grid-cols-4 gap-4 text-center">
                                <div class="bg-gray-50 rounded-lg p-3">
                                    <div class="text-xl font-bold text-gray-700">{{ testResults.test_files_count }}</div>
                                    <div class="text-xs text-gray-500">Arquivos</div>
                                </div>
                                <div class="bg-green-50 rounded-lg p-3">
                                    <div class="text-xl font-bold text-green-600">{{ testResults.passed }}</div>
                                    <div class="text-xs text-gray-500">Passou</div>
                                </div>
                                <div class="bg-red-50 rounded-lg p-3">
                                    <div class="text-xl font-bold text-red-600">{{ testResults.failed }}</div>
                                    <div class="text-xs text-gray-500">Falhou</div>
                                </div>
                                <div class="bg-yellow-50 rounded-lg p-3">
                                    <div class="text-xl font-bold text-yellow-600">{{ testResults.skipped }}</div>
                                    <div class="text-xs text-gray-500">Ignorado</div>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500">
                            <span class="text-4xl block mb-2">&#128203;</span>
                            <p>Nenhum teste encontrado</p>
                            <p class="text-sm">Testes aparecerao aqui quando forem criados</p>
                        </div>
                    </section>
                </div>

                <!-- Right Column - Documentation & Stories -->
                <div class="space-y-6">
                    <!-- Quick Actions -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <h2 class="text-lg font-semibold text-gray-800 mb-4 flex items-center gap-2">
                            <span class="text-2xl">&#9889;</span> Acoes Rapidas
                        </h2>
                        <div class="space-y-3">
                            <button @click="generateApp"
                                    :disabled="!quickActions?.can_generate_app || generatingApp"
                                    class="w-full flex items-center gap-3 p-3 rounded-lg border border-gray-200 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition text-left">
                                <span class="text-2xl">&#9881;</span>
                                <div>
                                    <div class="font-medium text-gray-800">Gerar App Testavel</div>
                                    <div class="text-sm text-gray-500">Cria aplicacao para teste</div>
                                </div>
                            </button>
                            <button @click="runTests"
                                    :disabled="!quickActions?.can_run_tests"
                                    class="w-full flex items-center gap-3 p-3 rounded-lg border border-gray-200 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition text-left">
                                <span class="text-2xl">&#129514;</span>
                                <div>
                                    <div class="font-medium text-gray-800">Rodar Testes</div>
                                    <div class="text-sm text-gray-500">Executa testes automatizados</div>
                                </div>
                            </button>
                            <button @click="openDocs"
                                    :disabled="!quickActions?.can_view_docs"
                                    class="w-full flex items-center gap-3 p-3 rounded-lg border border-gray-200 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition text-left">
                                <span class="text-2xl">&#128214;</span>
                                <div>
                                    <div class="font-medium text-gray-800">Ver Documentacao</div>
                                    <div class="text-sm text-gray-500">{{ documentation?.length || 0 }} documentos</div>
                                </div>
                            </button>
                            <a href="/"
                               class="w-full flex items-center gap-3 p-3 rounded-lg border border-gray-200 hover:bg-gray-50 transition text-left">
                                <span class="text-2xl">&#128200;</span>
                                <div>
                                    <div class="font-medium text-gray-800">Kanban Board</div>
                                    <div class="text-sm text-gray-500">Ver todas as stories</div>
                                </div>
                            </a>
                        </div>
                    </section>

                    <!-- Documentation -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <h2 class="text-lg font-semibold text-gray-800 mb-4 flex items-center gap-2">
                            <span class="text-2xl">&#128214;</span> Documentacao
                        </h2>
                        <div v-if="documentation?.length" class="space-y-2">
                            <div v-for="doc in documentation.slice(0, 8)" :key="doc.path"
                                 class="flex items-center gap-3 p-2 rounded-lg hover:bg-gray-50 cursor-pointer transition">
                                <span class="text-lg">{{ getDocIcon(doc.name) }}</span>
                                <div class="flex-1 min-w-0">
                                    <div class="font-medium text-sm text-gray-800 truncate">{{ doc.name }}</div>
                                    <div class="text-xs text-gray-500">{{ formatFileSize(doc.size) }}</div>
                                </div>
                            </div>
                            <div v-if="documentation.length > 8" class="text-center pt-2">
                                <button class="text-sm text-blue-600 hover:underline">Ver todos ({{ documentation.length }})</button>
                            </div>
                        </div>
                        <div v-else class="text-center py-6 text-gray-500">
                            <span class="text-3xl block mb-2">&#128196;</span>
                            <p class="text-sm">Sem documentacao</p>
                        </div>
                    </section>

                    <!-- Recent Stories -->
                    <section class="bg-white rounded-xl shadow-sm p-6 card-hover slide-in">
                        <h2 class="text-lg font-semibold text-gray-800 mb-4 flex items-center gap-2">
                            <span class="text-2xl">&#128203;</span> Stories Recentes
                        </h2>
                        <div v-if="recentStories?.length" class="space-y-3">
                            <div v-for="story in recentStories" :key="story.story_id"
                                 class="p-3 rounded-lg border border-gray-100 hover:border-gray-200 transition">
                                <div class="flex items-center justify-between mb-2">
                                    <span class="text-xs text-gray-500">{{ story.story_id }}</span>
                                    <span :class="['status-badge', 'status-' + story.status]">{{ getStatusLabel(story.status) }}</span>
                                </div>
                                <div class="font-medium text-sm text-gray-800 line-clamp-2">{{ story.title }}</div>
                                <div class="mt-2 flex items-center gap-2">
                                    <div class="flex-1 h-1.5 bg-gray-100 rounded-full overflow-hidden">
                                        <div class="h-full bg-green-500 rounded-full" :style="{width: story.progress + '%'}"></div>
                                    </div>
                                    <span class="text-xs text-gray-500">{{ story.progress }}%</span>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-6 text-gray-500">
                            <span class="text-3xl block mb-2">&#128221;</span>
                            <p class="text-sm">Nenhuma story ainda</p>
                        </div>
                    </section>
                </div>
            </div>
        </main>

        <!-- Toast Notifications -->
        <div class="fixed bottom-6 right-6 z-50 space-y-2">
            <div v-for="toast in toasts" :key="toast.id"
                 :class="['p-4 rounded-lg shadow-lg flex items-center gap-3 min-w-80 slide-in',
                         toast.type === 'success' ? 'bg-green-500 text-white' :
                         toast.type === 'error' ? 'bg-red-500 text-white' : 'bg-blue-500 text-white']">
                <span class="text-xl">{{ toast.type === 'success' ? '&#10003;' : toast.type === 'error' ? '&#10007;' : '&#8505;' }}</span>
                <div class="flex-1">
                    <div class="font-medium">{{ toast.title }}</div>
                    <div class="text-sm opacity-90">{{ toast.message }}</div>
                </div>
                <button @click="removeToast(toast.id)" class="opacity-70 hover:opacity-100">&times;</button>
            </div>
        </div>
    </div>

    <script>
    const FileTreeItem = {
        name: 'FileTreeItem',
        props: ['item', 'level'],
        template: `
            <div>
                <div :class="['file-tree-item flex items-center gap-2 px-3 py-2 cursor-pointer']"
                     :style="{ paddingLeft: (level * 16 + 12) + 'px' }"
                     @click="toggle">
                    <span v-if="item.type === 'directory'" class="text-sm w-4">{{ expanded ? '&#9660;' : '&#9654;' }}</span>
                    <span v-else class="text-sm w-4"></span>
                    <span class="text-lg">{{ getIcon() }}</span>
                    <span class="text-sm text-gray-700">{{ item.name }}</span>
                    <span v-if="item.size" class="text-xs text-gray-400 ml-auto">{{ formatSize(item.size) }}</span>
                </div>
                <div v-if="item.type === 'directory' && expanded && item.children">
                    <file-tree-item v-for="child in item.children" :key="child.path" :item="child" :level="level + 1"></file-tree-item>
                </div>
            </div>
        `,
        data() { return { expanded: this.level < 1 } },
        methods: {
            toggle() { if (this.item.type === 'directory') this.expanded = !this.expanded; },
            getIcon() {
                if (this.item.type === 'directory') return '&#128193;';
                const ext = this.item.extension || '';
                const icons = { '.py': '&#128013;', '.js': '&#127312;', '.ts': '&#127827;', '.html': '&#127760;', '.css': '&#127912;', '.json': '&#128196;', '.md': '&#128221;', '.txt': '&#128196;', '.yml': '&#9881;', '.yaml': '&#9881;' };
                return icons[ext] || '&#128196;';
            },
            formatSize(bytes) {
                if (bytes < 1024) return bytes + ' B';
                if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            }
        }
    };

    const { createApp, ref, computed, onMounted } = Vue;

    createApp({
        components: { FileTreeItem },
        setup() {
            const projectId = '{{PROJECT_ID}}';
            const loading = ref(true);
            const project = ref(null);
            const storiesSummary = ref(null);
            const recentStories = ref([]);
            const appStatus = ref(null);
            const filesTree = ref([]);
            const documentation = ref([]);
            const testResults = ref(null);
            const quickActions = ref(null);
            const toasts = ref([]);
            const generatingApp = ref(false);
            const startingApp = ref(false);

            const circumference = 2 * Math.PI * 45;
            const progressOffset = computed(() => {
                const progress = storiesSummary.value?.progress || 0;
                return circumference - (progress / 100) * circumference;
            });

            const projectSteps = computed(() => {
                const progress = storiesSummary.value?.progress || 0;
                return [
                    { name: 'Planejamento', completed: progress > 0, current: progress === 0 },
                    { name: 'Desenvolvimento', completed: progress >= 30, current: progress > 0 && progress < 30 },
                    { name: 'Revisao', completed: progress >= 60, current: progress >= 30 && progress < 60 },
                    { name: 'Testes', completed: progress >= 80, current: progress >= 60 && progress < 80 },
                    { name: 'Entrega', completed: progress >= 100, current: progress >= 80 && progress < 100 }
                ];
            });

            const loadData = async () => {
                try {
                    const res = await fetch(`/api/projects/${projectId}/preview-data`);
                    if (!res.ok) throw new Error('Erro ao carregar dados');
                    const data = await res.json();
                    project.value = data.project;
                    storiesSummary.value = data.stories_summary;
                    recentStories.value = data.recent_stories;
                    appStatus.value = data.app_status;
                    filesTree.value = data.files_tree;
                    documentation.value = data.documentation;
                    testResults.value = data.test_results;
                    quickActions.value = data.quick_actions;
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao carregar dados do projeto');
                } finally {
                    loading.value = false;
                }
            };

            const addToast = (type, title, message) => {
                const id = Date.now();
                toasts.value.push({ id, type, title, message });
                setTimeout(() => removeToast(id), 5000);
            };

            const removeToast = (id) => { toasts.value = toasts.value.filter(t => t.id !== id); };

            const generateApp = async () => {
                generatingApp.value = true;
                try {
                    addToast('info', 'Gerando aplicacao', 'Preparando ambiente de teste...');
                    const res = await fetch(`/api/projects/${projectId}/generate-app`, { method: 'POST' });
                    const data = await res.json();
                    if (data.status === 'success') {
                        addToast('success', 'Sucesso', 'Aplicacao gerada com sucesso!');
                        await loadData();
                    } else {
                        addToast('error', 'Erro', data.message || 'Falha ao gerar aplicacao');
                    }
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao gerar aplicacao');
                } finally {
                    generatingApp.value = false;
                }
            };

            const startApp = async () => {
                startingApp.value = true;
                try {
                    addToast('info', 'Iniciando', 'Iniciando servidor de testes...');
                    const res = await fetch(`/api/projects/${projectId}/start-app`, { method: 'POST' });
                    const data = await res.json();
                    if (data.status === 'success' || data.status === 'already_running') {
                        addToast('success', 'Servidor iniciado', 'Abrindo aplicacao...');
                        if (data.app_url) setTimeout(() => window.open(data.app_url, '_blank'), 1500);
                    } else {
                        addToast('error', 'Erro', data.message || 'Falha ao iniciar servidor');
                    }
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao iniciar servidor');
                } finally {
                    startingApp.value = false;
                }
            };

            const runTests = () => { addToast('info', 'Testes', 'Funcionalidade em desenvolvimento'); };
            const openDocs = () => { if (documentation.value?.length) addToast('info', 'Documentacao', 'Abrindo documentacao...'); };

            const getStatusClass = (status) => {
                const classes = { 'ready_to_test': 'bg-green-100 text-green-700', 'can_generate_app': 'bg-blue-100 text-blue-700', 'analyzed': 'bg-yellow-100 text-yellow-700', 'not_found': 'bg-gray-100 text-gray-600' };
                return classes[status] || 'bg-gray-100 text-gray-600';
            };
            const getStatusText = (status) => {
                const texts = { 'ready_to_test': 'Pronto para Teste', 'can_generate_app': 'Pode Gerar App', 'analyzed': 'Em Desenvolvimento', 'not_found': 'Aguardando' };
                return texts[status] || 'Verificando...';
            };
            const getAppStatusBgClass = () => {
                if (appStatus.value?.ready_to_test) return 'bg-green-50 border border-green-200';
                if (appStatus.value?.can_generate_app) return 'bg-blue-50 border border-blue-200';
                return 'bg-amber-50 border border-amber-200';
            };
            const getAppStatusTextClass = () => {
                if (appStatus.value?.ready_to_test) return 'text-green-800';
                if (appStatus.value?.can_generate_app) return 'text-blue-800';
                return 'text-amber-800';
            };
            const getAppStatusIcon = () => {
                if (appStatus.value?.ready_to_test) return '&#10004;';
                if (appStatus.value?.can_generate_app) return '&#9881;';
                return '&#128679;';
            };
            const getAppStatusTitle = () => {
                if (appStatus.value?.ready_to_test) return 'Pronto para Testar!';
                if (appStatus.value?.can_generate_app) return 'Codigo Pronto - Gerar App';
                return 'Em Desenvolvimento';
            };
            const getStatusLabel = (status) => {
                const labels = { 'backlog': 'Backlog', 'ready': 'Ready', 'in_progress': 'Em Progresso', 'review': 'Revisao', 'testing': 'Teste', 'done': 'Concluido' };
                return labels[status] || status;
            };
            const getDocIcon = (name) => {
                const n = name.toLowerCase();
                if (n.includes('readme')) return '&#128214;';
                if (n.includes('api')) return '&#128279;';
                if (n.includes('test')) return '&#129514;';
                return '&#128196;';
            };
            const formatFileSize = (bytes) => {
                if (!bytes) return '0 B';
                if (bytes < 1024) return bytes + ' B';
                if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            };
            const countFiles = (tree) => {
                let count = 0;
                const traverse = (items) => { items.forEach(item => { if (item.type === 'file') count++; else if (item.children) traverse(item.children); }); };
                traverse(tree);
                return count;
            };

            onMounted(() => { loadData(); });

            return {
                loading, project, storiesSummary, recentStories, appStatus,
                filesTree, documentation, testResults, quickActions, toasts,
                generatingApp, startingApp, circumference, progressOffset, projectSteps,
                loadData, addToast, removeToast, generateApp, startApp, runTests, openDocs,
                getStatusClass, getStatusText, getAppStatusBgClass, getAppStatusTextClass,
                getAppStatusIcon, getAppStatusTitle, getStatusLabel, getDocIcon,
                formatFileSize, countFiles
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
"""
