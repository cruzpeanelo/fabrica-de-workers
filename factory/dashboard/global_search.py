# -*- coding: utf-8 -*-
"""
Global Search Module (Issue #271)
=================================
Sistema de busca global avancada com filtros combinaveis.

Funcionalidades:
- Busca full-text em todos os campos
- Filtros por status, prioridade, assignee, datas
- Busca por ID de story
- Resultados agrupados por tipo
- Historico de buscas recentes
"""

from fastapi import APIRouter, Query, HTTPException
from fastapi.responses import HTMLResponse
from typing import Optional, List
from datetime import datetime, timedelta
from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask, Epic, Sprint

router = APIRouter(prefix="/api/search", tags=["Global Search"])


@router.get("/global")
async def global_search(
    q: str = Query(..., min_length=1, description="Termo de busca"),
    type: Optional[str] = Query(None, description="Tipo: stories, tasks, epics, sprints, all"),
    status: Optional[str] = Query(None, description="Filtro por status"),
    priority: Optional[str] = Query(None, description="Filtro por prioridade"),
    assignee: Optional[str] = Query(None, description="Filtro por responsavel"),
    date_from: Optional[str] = Query(None, description="Data inicial (YYYY-MM-DD)"),
    date_to: Optional[str] = Query(None, description="Data final (YYYY-MM-DD)"),
    limit: int = Query(50, ge=1, le=200, description="Limite de resultados")
):
    """
    Busca global em stories, tasks, epics e sprints.
    Suporta busca por ID (ex: STR-001) ou texto livre.
    """
    db = SessionLocal()
    try:
        results = {
            "stories": [],
            "tasks": [],
            "epics": [],
            "sprints": [],
            "total": 0
        }

        search_term = q.strip().lower()
        search_types = [type] if type and type != "all" else ["stories", "tasks", "epics", "sprints"]

        # Parse dates
        from_date = None
        to_date = None
        if date_from:
            try:
                from_date = datetime.strptime(date_from, "%Y-%m-%d")
            except:
                pass
        if date_to:
            try:
                to_date = datetime.strptime(date_to, "%Y-%m-%d") + timedelta(days=1)
            except:
                pass

        # Search Stories
        if "stories" in search_types:
            query = db.query(Story)

            # Check if searching by ID
            if search_term.startswith("str-") or search_term.startswith("story-"):
                query = query.filter(Story.story_id.ilike(f"%{search_term}%"))
            else:
                # Full text search
                query = query.filter(
                    (Story.title.ilike(f"%{search_term}%")) |
                    (Story.description.ilike(f"%{search_term}%")) |
                    (Story.persona.ilike(f"%{search_term}%")) |
                    (Story.action.ilike(f"%{search_term}%")) |
                    (Story.benefit.ilike(f"%{search_term}%")) |
                    (Story.story_id.ilike(f"%{search_term}%"))
                )

            # Apply filters
            if status:
                query = query.filter(Story.status == status)
            if priority:
                query = query.filter(Story.priority == priority)
            if assignee:
                query = query.filter(Story.assignee.ilike(f"%{assignee}%"))
            if from_date:
                query = query.filter(Story.created_at >= from_date)
            if to_date:
                query = query.filter(Story.created_at < to_date)

            stories = query.order_by(Story.updated_at.desc()).limit(limit).all()
            results["stories"] = [
                {
                    "id": s.story_id,
                    "title": s.title,
                    "status": s.status.value if hasattr(s.status, 'value') else s.status,
                    "priority": s.priority.value if hasattr(s.priority, 'value') else s.priority,
                    "story_points": s.story_points,
                    "assignee": s.assignee,
                    "updated_at": s.updated_at.isoformat() if s.updated_at else None,
                    "type": "story"
                }
                for s in stories
            ]

        # Search Tasks
        if "tasks" in search_types:
            query = db.query(StoryTask)

            if search_term.startswith("stsk-") or search_term.startswith("task-"):
                query = query.filter(StoryTask.task_id.ilike(f"%{search_term}%"))
            else:
                query = query.filter(
                    (StoryTask.title.ilike(f"%{search_term}%")) |
                    (StoryTask.description.ilike(f"%{search_term}%")) |
                    (StoryTask.task_id.ilike(f"%{search_term}%"))
                )

            if status:
                query = query.filter(StoryTask.status == status)

            tasks = query.order_by(StoryTask.updated_at.desc()).limit(limit).all()
            results["tasks"] = [
                {
                    "id": t.task_id,
                    "title": t.title,
                    "status": t.status.value if hasattr(t.status, 'value') else t.status,
                    "story_id": t.story_id,
                    "progress": t.progress,
                    "type": "task"
                }
                for t in tasks
            ]

        # Search Epics
        if "epics" in search_types:
            query = db.query(Epic).filter(
                (Epic.title.ilike(f"%{search_term}%")) |
                (Epic.description.ilike(f"%{search_term}%"))
            )

            epics = query.limit(limit).all()
            results["epics"] = [
                {
                    "id": e.epic_id,
                    "title": e.title,
                    "color": e.color,
                    "type": "epic"
                }
                for e in epics
            ]

        # Search Sprints
        if "sprints" in search_types:
            query = db.query(Sprint).filter(
                (Sprint.name.ilike(f"%{search_term}%")) |
                (Sprint.goal.ilike(f"%{search_term}%"))
            )

            sprints = query.limit(limit).all()
            results["sprints"] = [
                {
                    "id": s.sprint_id,
                    "name": s.name,
                    "goal": s.goal,
                    "start_date": s.start_date.isoformat() if s.start_date else None,
                    "end_date": s.end_date.isoformat() if s.end_date else None,
                    "type": "sprint"
                }
                for s in sprints
            ]

        # Calculate total
        results["total"] = (
            len(results["stories"]) +
            len(results["tasks"]) +
            len(results["epics"]) +
            len(results["sprints"])
        )

        return results

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        db.close()


@router.get("/suggestions")
async def search_suggestions(q: str = Query(..., min_length=2)):
    """
    Retorna sugestoes de busca baseado no termo parcial.
    """
    db = SessionLocal()
    try:
        suggestions = []
        search_term = q.strip().lower()

        # Get story titles
        stories = db.query(Story.title, Story.story_id).filter(
            Story.title.ilike(f"%{search_term}%")
        ).limit(5).all()

        for s in stories:
            suggestions.append({
                "text": s.title,
                "type": "story",
                "id": s.story_id
            })

        # Get epic titles
        epics = db.query(Epic.title, Epic.epic_id).filter(
            Epic.title.ilike(f"%{search_term}%")
        ).limit(3).all()

        for e in epics:
            suggestions.append({
                "text": e.title,
                "type": "epic",
                "id": e.epic_id
            })

        return suggestions

    finally:
        db.close()


def get_search_component_html():
    """Retorna o HTML do componente de busca global."""
    return '''
    <!-- Global Search Component (Issue #271) -->
    <div id="global-search-overlay"
         v-if="showGlobalSearch"
         class="fixed inset-0 bg-black/50 z-50 flex items-start justify-center pt-20"
         @click.self="showGlobalSearch = false">
        <div class="bg-white rounded-xl shadow-2xl w-full max-w-2xl mx-4 overflow-hidden">
            <!-- Search Input -->
            <div class="flex items-center border-b border-gray-200 px-4">
                <svg class="w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                          d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                </svg>
                <input type="text"
                       v-model="globalSearchQuery"
                       @input="performGlobalSearch"
                       @keydown.escape="showGlobalSearch = false"
                       @keydown.down.prevent="navigateSearchResults(1)"
                       @keydown.up.prevent="navigateSearchResults(-1)"
                       @keydown.enter="selectSearchResult"
                       ref="globalSearchInput"
                       placeholder="Buscar stories, tasks, epics..."
                       class="flex-1 px-3 py-4 text-lg focus:outline-none">
                <kbd class="hidden sm:inline-block px-2 py-1 bg-gray-100 text-gray-500 text-xs rounded">ESC</kbd>
            </div>

            <!-- Filters -->
            <div class="px-4 py-2 bg-gray-50 border-b border-gray-200 flex items-center gap-2 flex-wrap">
                <select v-model="searchFilters.type" @change="performGlobalSearch"
                        class="text-sm border border-gray-300 rounded px-2 py-1">
                    <option value="all">Todos os tipos</option>
                    <option value="stories">Stories</option>
                    <option value="tasks">Tasks</option>
                    <option value="epics">Epics</option>
                    <option value="sprints">Sprints</option>
                </select>
                <select v-model="searchFilters.status" @change="performGlobalSearch"
                        class="text-sm border border-gray-300 rounded px-2 py-1">
                    <option value="">Qualquer status</option>
                    <option value="backlog">Backlog</option>
                    <option value="ready">Ready</option>
                    <option value="in_progress">In Progress</option>
                    <option value="review">Review</option>
                    <option value="testing">Testing</option>
                    <option value="done">Done</option>
                </select>
                <select v-model="searchFilters.priority" @change="performGlobalSearch"
                        class="text-sm border border-gray-300 rounded px-2 py-1">
                    <option value="">Qualquer prioridade</option>
                    <option value="urgent">Urgente</option>
                    <option value="high">Alta</option>
                    <option value="medium">Media</option>
                    <option value="low">Baixa</option>
                </select>
            </div>

            <!-- Results -->
            <div class="max-h-96 overflow-y-auto">
                <!-- Loading -->
                <div v-if="searchLoading" class="p-8 text-center text-gray-500">
                    <svg class="animate-spin h-6 w-6 mx-auto mb-2" fill="none" viewBox="0 0 24 24">
                        <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"/>
                        <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"/>
                    </svg>
                    Buscando...
                </div>

                <!-- No Results -->
                <div v-else-if="globalSearchQuery && searchResults.total === 0" class="p-8 text-center text-gray-500">
                    <svg class="w-12 h-12 mx-auto mb-2 text-gray-300" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                              d="M9.172 16.172a4 4 0 015.656 0M9 10h.01M15 10h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                    </svg>
                    Nenhum resultado encontrado
                </div>

                <!-- Results List -->
                <div v-else>
                    <!-- Stories -->
                    <div v-if="searchResults.stories.length > 0">
                        <div class="px-4 py-2 bg-gray-100 text-xs font-semibold text-gray-500 uppercase">
                            Stories ({{ searchResults.stories.length }})
                        </div>
                        <div v-for="(item, idx) in searchResults.stories" :key="item.id"
                             @click="openSearchResult(item)"
                             :class="['px-4 py-3 hover:bg-blue-50 cursor-pointer border-b border-gray-100',
                                      searchSelectedIndex === idx ? 'bg-blue-50' : '']">
                            <div class="flex items-center justify-between">
                                <div class="flex items-center gap-2">
                                    <span class="text-xs font-mono text-gray-400">{{ item.id }}</span>
                                    <span class="font-medium text-gray-800">{{ item.title }}</span>
                                </div>
                                <div class="flex items-center gap-2">
                                    <span :class="'px-2 py-0.5 rounded text-xs ' + getStatusClass(item.status)">
                                        {{ item.status }}
                                    </span>
                                    <span class="text-xs text-gray-500">{{ item.story_points }} pts</span>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Tasks -->
                    <div v-if="searchResults.tasks.length > 0">
                        <div class="px-4 py-2 bg-gray-100 text-xs font-semibold text-gray-500 uppercase">
                            Tasks ({{ searchResults.tasks.length }})
                        </div>
                        <div v-for="item in searchResults.tasks" :key="item.id"
                             @click="openSearchResult(item)"
                             class="px-4 py-3 hover:bg-blue-50 cursor-pointer border-b border-gray-100">
                            <div class="flex items-center justify-between">
                                <div class="flex items-center gap-2">
                                    <span class="text-xs font-mono text-gray-400">{{ item.id }}</span>
                                    <span class="font-medium text-gray-800">{{ item.title }}</span>
                                </div>
                                <span class="text-xs text-gray-500">{{ item.progress }}%</span>
                            </div>
                        </div>
                    </div>

                    <!-- Epics -->
                    <div v-if="searchResults.epics.length > 0">
                        <div class="px-4 py-2 bg-gray-100 text-xs font-semibold text-gray-500 uppercase">
                            Epics ({{ searchResults.epics.length }})
                        </div>
                        <div v-for="item in searchResults.epics" :key="item.id"
                             @click="openSearchResult(item)"
                             class="px-4 py-3 hover:bg-blue-50 cursor-pointer border-b border-gray-100">
                            <div class="flex items-center gap-2">
                                <span class="w-3 h-3 rounded-full" :style="{backgroundColor: item.color}"></span>
                                <span class="font-medium text-gray-800">{{ item.title }}</span>
                            </div>
                        </div>
                    </div>

                    <!-- Sprints -->
                    <div v-if="searchResults.sprints.length > 0">
                        <div class="px-4 py-2 bg-gray-100 text-xs font-semibold text-gray-500 uppercase">
                            Sprints ({{ searchResults.sprints.length }})
                        </div>
                        <div v-for="item in searchResults.sprints" :key="item.id"
                             @click="openSearchResult(item)"
                             class="px-4 py-3 hover:bg-blue-50 cursor-pointer border-b border-gray-100">
                            <div class="flex items-center justify-between">
                                <span class="font-medium text-gray-800">{{ item.name }}</span>
                                <span class="text-xs text-gray-500">{{ item.start_date }} - {{ item.end_date }}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-4 py-2 bg-gray-50 border-t border-gray-200 flex items-center justify-between text-xs text-gray-500">
                <div class="flex items-center gap-4">
                    <span><kbd class="px-1.5 py-0.5 bg-gray-200 rounded">↑↓</kbd> navegar</span>
                    <span><kbd class="px-1.5 py-0.5 bg-gray-200 rounded">↵</kbd> selecionar</span>
                    <span><kbd class="px-1.5 py-0.5 bg-gray-200 rounded">esc</kbd> fechar</span>
                </div>
                <span v-if="searchResults.total > 0">{{ searchResults.total }} resultados</span>
            </div>
        </div>
    </div>
    '''


def get_search_js_code():
    """Retorna o codigo JavaScript para a busca global."""
    return '''
    // Global Search State (Issue #271)
    const showGlobalSearch = ref(false);
    const globalSearchQuery = ref('');
    const searchFilters = ref({ type: 'all', status: '', priority: '' });
    const searchResults = ref({ stories: [], tasks: [], epics: [], sprints: [], total: 0 });
    const searchLoading = ref(false);
    const searchSelectedIndex = ref(0);
    const searchDebounce = ref(null);

    // Open global search with Ctrl+K / Cmd+K
    document.addEventListener('keydown', (e) => {
        if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
            e.preventDefault();
            showGlobalSearch.value = true;
            setTimeout(() => {
                const input = document.querySelector('[ref="globalSearchInput"]');
                if (input) input.focus();
            }, 100);
        }
    });

    const performGlobalSearch = async () => {
        if (searchDebounce.value) clearTimeout(searchDebounce.value);

        searchDebounce.value = setTimeout(async () => {
            if (!globalSearchQuery.value || globalSearchQuery.value.length < 2) {
                searchResults.value = { stories: [], tasks: [], epics: [], sprints: [], total: 0 };
                return;
            }

            searchLoading.value = true;
            try {
                const params = new URLSearchParams({
                    q: globalSearchQuery.value,
                    type: searchFilters.value.type,
                    status: searchFilters.value.status,
                    priority: searchFilters.value.priority
                });

                const response = await fetch('/api/search/global?' + params);
                if (response.ok) {
                    searchResults.value = await response.json();
                    searchSelectedIndex.value = 0;
                }
            } catch (e) {
                console.error('Search error:', e);
            } finally {
                searchLoading.value = false;
            }
        }, 300);
    };

    const navigateSearchResults = (direction) => {
        const total = searchResults.value.total;
        if (total === 0) return;

        searchSelectedIndex.value = (searchSelectedIndex.value + direction + total) % total;
    };

    const selectSearchResult = () => {
        const allResults = [
            ...searchResults.value.stories,
            ...searchResults.value.tasks,
            ...searchResults.value.epics,
            ...searchResults.value.sprints
        ];

        if (allResults[searchSelectedIndex.value]) {
            openSearchResult(allResults[searchSelectedIndex.value]);
        }
    };

    const openSearchResult = (item) => {
        showGlobalSearch.value = false;
        globalSearchQuery.value = '';

        if (item.type === 'story') {
            // Find and open story
            const story = stories.value.find(s => s.story_id === item.id);
            if (story) openStoryDetail(story);
        } else if (item.type === 'task') {
            // Navigate to task's story
            const story = stories.value.find(s => s.story_id === item.story_id);
            if (story) {
                openStoryDetail(story);
                // TODO: Scroll to task
            }
        }
        // Handle epics and sprints as needed
    };

    const getStatusClass = (status) => {
        const classes = {
            'backlog': 'bg-gray-100 text-gray-700',
            'ready': 'bg-blue-100 text-blue-700',
            'in_progress': 'bg-yellow-100 text-yellow-700',
            'review': 'bg-purple-100 text-purple-700',
            'testing': 'bg-orange-100 text-orange-700',
            'done': 'bg-green-100 text-green-700'
        };
        return classes[status] || 'bg-gray-100 text-gray-700';
    };
    '''


def register_global_search(app):
    """Registra os endpoints de busca global no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Global Search endpoints loaded: /api/search/*")
