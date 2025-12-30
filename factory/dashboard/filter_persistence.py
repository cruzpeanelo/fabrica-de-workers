"""
Filter Persistence Module - Issue #235
=====================================
Salva estado dos filtros entre sessões e permite criar filtros salvos.

Terminal C - UI/UX (localStorage + URL)
Backend (API + DB) pendente - Terminal D
"""

from fastapi import APIRouter
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict, Any

router = APIRouter(prefix="/api/filters", tags=["Filter Persistence"])


class SavedFilter(BaseModel):
    """Saved filter configuration."""
    id: str
    name: str
    icon: str = "⭐"
    view: str  # kanban, analytics, stories
    filters: Dict[str, Any]
    is_default: bool = False


# In-memory storage (TODO: move to database - Terminal D)
_saved_filters: List[SavedFilter] = []


@router.get("")
async def get_saved_filters(view: Optional[str] = None):
    """Get all saved filters, optionally filtered by view."""
    filters = _saved_filters
    if view:
        filters = [f for f in filters if f.view == view]
    return {"filters": filters}


@router.post("")
async def create_saved_filter(filter_data: SavedFilter):
    """Create a new saved filter."""
    import uuid
    filter_data.id = str(uuid.uuid4())[:8]
    _saved_filters.append(filter_data)
    return {"success": True, "filter": filter_data}


@router.delete("/{filter_id}")
async def delete_saved_filter(filter_id: str):
    """Delete a saved filter."""
    global _saved_filters
    _saved_filters = [f for f in _saved_filters if f.id != filter_id]
    return {"success": True}


def get_filter_panel_html():
    """Returns the filter panel HTML component."""
    return '''
    <!-- Filter Panel Component (Issue #235) -->
    <div v-if="showFilterPanel" class="fixed inset-0 bg-black/30 z-50 flex justify-end"
         @click.self="showFilterPanel = false">
        <div class="bg-white dark:bg-gray-800 w-full max-w-md h-full shadow-2xl overflow-hidden flex flex-col animate-slide-in-right">
            <!-- Header -->
            <div class="bg-gradient-to-r from-[#003B4A] to-[#005566] px-6 py-4 text-white flex items-center justify-between flex-shrink-0">
                <div class="flex items-center gap-3">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 4a1 1 0 011-1h16a1 1 0 011 1v2.586a1 1 0 01-.293.707l-6.414 6.414a1 1 0 00-.293.707V17l-4 4v-6.586a1 1 0 00-.293-.707L3.293 7.293A1 1 0 013 6.586V4z"/>
                    </svg>
                    <h2 class="text-lg font-semibold">Filtros</h2>
                </div>
                <button @click="showFilterPanel = false" class="text-white/70 hover:text-white p-1 rounded-lg hover:bg-white/10 transition-colors">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <!-- Content -->
            <div class="flex-1 overflow-y-auto p-6 space-y-6">
                <!-- Active Filters -->
                <div v-if="Object.keys(activeFilters).length > 0">
                    <h3 class="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mb-3">
                        Filtros Ativos
                    </h3>
                    <div class="flex flex-wrap gap-2">
                        <span v-for="(value, key) in activeFilters" :key="key"
                              class="inline-flex items-center gap-2 px-3 py-1.5 bg-[#003B4A]/10 text-[#003B4A] dark:bg-[#003B4A]/30 dark:text-[#00A3CC] rounded-full text-sm font-medium">
                            <span>{{ formatFilterLabel(key) }}: {{ formatFilterValue(value) }}</span>
                            <button @click="removeFilter(key)" class="hover:text-red-500 transition-colors">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                                </svg>
                            </button>
                        </span>
                    </div>
                    <button @click="clearAllFilters" class="mt-3 text-sm text-gray-500 hover:text-red-500 transition-colors">
                        Limpar todos os filtros
                    </button>
                </div>

                <!-- No Active Filters -->
                <div v-else class="text-center py-8">
                    <svg class="w-12 h-12 mx-auto text-gray-300 dark:text-gray-600 mb-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M3 4a1 1 0 011-1h16a1 1 0 011 1v2.586a1 1 0 01-.293.707l-6.414 6.414a1 1 0 00-.293.707V17l-4 4v-6.586a1 1 0 00-.293-.707L3.293 7.293A1 1 0 013 6.586V4z"/>
                    </svg>
                    <p class="text-gray-500 dark:text-gray-400">Nenhum filtro ativo</p>
                </div>

                <!-- Divider -->
                <hr class="border-gray-200 dark:border-gray-700">

                <!-- Saved Filters -->
                <div>
                    <div class="flex items-center justify-between mb-3">
                        <h3 class="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                            Filtros Salvos
                        </h3>
                        <button @click="showSaveFilterModal = true"
                                v-if="Object.keys(activeFilters).length > 0"
                                class="text-sm text-[#FF6C00] hover:text-[#e55c00] font-medium flex items-center gap-1">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"/>
                            </svg>
                            Salvar atual
                        </button>
                    </div>

                    <!-- Saved Filters List -->
                    <div v-if="savedFilters.length > 0" class="space-y-2">
                        <div v-for="filter in savedFilters" :key="filter.id"
                             class="flex items-center justify-between p-3 bg-gray-50 dark:bg-gray-700/50 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors group">
                            <div class="flex items-center gap-3">
                                <span class="text-lg">{{ filter.icon }}</span>
                                <div>
                                    <div class="font-medium text-gray-700 dark:text-gray-200">{{ filter.name }}</div>
                                    <div class="text-xs text-gray-500 dark:text-gray-400">
                                        {{ Object.keys(filter.filters).length }} filtros
                                    </div>
                                </div>
                            </div>
                            <div class="flex items-center gap-2">
                                <button @click="applySavedFilter(filter)"
                                        class="px-3 py-1.5 bg-[#003B4A] text-white text-sm rounded-lg hover:bg-[#00506a] transition-colors">
                                    Aplicar
                                </button>
                                <button @click="deleteSavedFilter(filter.id)"
                                        class="p-1.5 text-gray-400 hover:text-red-500 opacity-0 group-hover:opacity-100 transition-all">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                    </svg>
                                </button>
                            </div>
                        </div>
                    </div>

                    <!-- No Saved Filters -->
                    <div v-else class="text-center py-6 bg-gray-50 dark:bg-gray-700/30 rounded-lg">
                        <p class="text-gray-500 dark:text-gray-400 text-sm">
                            Nenhum filtro salvo ainda
                        </p>
                        <p class="text-gray-400 dark:text-gray-500 text-xs mt-1">
                            Aplique filtros e clique em "Salvar atual"
                        </p>
                    </div>
                </div>

                <!-- Quick Filters -->
                <div>
                    <h3 class="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mb-3">
                        Filtros Rapidos
                    </h3>
                    <div class="grid grid-cols-2 gap-2">
                        <button @click="applyQuickFilter('my-stories')"
                                class="p-3 text-left bg-gray-50 dark:bg-gray-700/50 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors">
                            <div class="flex items-center gap-2 text-gray-700 dark:text-gray-200 font-medium text-sm">
                                <span>👤</span> Minhas Stories
                            </div>
                        </button>
                        <button @click="applyQuickFilter('in-progress')"
                                class="p-3 text-left bg-gray-50 dark:bg-gray-700/50 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors">
                            <div class="flex items-center gap-2 text-gray-700 dark:text-gray-200 font-medium text-sm">
                                <span>🔄</span> Em Progresso
                            </div>
                        </button>
                        <button @click="applyQuickFilter('high-priority')"
                                class="p-3 text-left bg-gray-50 dark:bg-gray-700/50 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors">
                            <div class="flex items-center gap-2 text-gray-700 dark:text-gray-200 font-medium text-sm">
                                <span>🔴</span> Alta Prioridade
                            </div>
                        </button>
                        <button @click="applyQuickFilter('current-sprint')"
                                class="p-3 text-left bg-gray-50 dark:bg-gray-700/50 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors">
                            <div class="flex items-center gap-2 text-gray-700 dark:text-gray-200 font-medium text-sm">
                                <span>📅</span> Sprint Atual
                            </div>
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <!-- Save Filter Modal -->
    <div v-if="showSaveFilterModal" class="fixed inset-0 bg-black/50 z-[60] flex items-center justify-center"
         @click.self="showSaveFilterModal = false">
        <div class="bg-white dark:bg-gray-800 rounded-xl shadow-2xl w-full max-w-md mx-4 p-6">
            <h3 class="text-lg font-semibold text-gray-800 dark:text-white mb-4">Salvar Filtro</h3>

            <div class="space-y-4">
                <div>
                    <label class="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Nome</label>
                    <input v-model="newFilterName" type="text"
                           class="w-full px-4 py-2 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-800 dark:text-white focus:ring-2 focus:ring-[#003B4A] focus:border-transparent"
                           placeholder="Ex: Minhas stories em progresso">
                </div>

                <div>
                    <label class="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Icone</label>
                    <div class="flex gap-2 flex-wrap">
                        <button v-for="icon in ['⭐', '🔴', '🔵', '🟢', '📋', '🎯', '🚀', '🐛', '💡', '⚡']"
                                :key="icon"
                                @click="newFilterIcon = icon"
                                :class="['w-10 h-10 rounded-lg text-xl flex items-center justify-center transition-all',
                                         newFilterIcon === icon ? 'bg-[#003B4A] shadow-lg' : 'bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600']">
                            {{ icon }}
                        </button>
                    </div>
                </div>
            </div>

            <div class="flex justify-end gap-3 mt-6">
                <button @click="showSaveFilterModal = false"
                        class="px-4 py-2 text-gray-600 dark:text-gray-400 hover:text-gray-800 dark:hover:text-gray-200 font-medium">
                    Cancelar
                </button>
                <button @click="saveCurrentFilter"
                        class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-[#e55c00] font-medium transition-colors">
                    Salvar Filtro
                </button>
            </div>
        </div>
    </div>
    '''


def get_filter_panel_css():
    """Returns CSS for filter panel."""
    return '''
    /* Filter Panel Styles (Issue #235) */
    @keyframes slide-in-right {
        from {
            transform: translateX(100%);
            opacity: 0;
        }
        to {
            transform: translateX(0);
            opacity: 1;
        }
    }

    .animate-slide-in-right {
        animation: slide-in-right 0.2s ease-out;
    }
    '''


def get_filter_panel_js():
    """Returns JavaScript for filter persistence."""
    return '''
    // ========== ISSUE #235 - FILTER PERSISTENCE ==========

    // Filter state
    const showFilterPanel = ref(false);
    const showSaveFilterModal = ref(false);
    const activeFilters = ref({});
    const savedFilters = ref([]);
    const newFilterName = ref('');
    const newFilterIcon = ref('⭐');

    // Filter label mapping
    const filterLabels = {
        'project': 'Projeto',
        'sprint': 'Sprint',
        'assignee': 'Responsavel',
        'priority': 'Prioridade',
        'status': 'Status',
        'epic': 'Epic',
        'type': 'Tipo',
        'complexity': 'Complexidade'
    };

    // Format filter label
    const formatFilterLabel = (key) => filterLabels[key] || key;

    // Format filter value
    const formatFilterValue = (value) => {
        if (Array.isArray(value)) return value.join(', ');
        if (typeof value === 'object') return JSON.stringify(value);
        return value;
    };

    // Load filters from localStorage
    const loadFiltersFromStorage = () => {
        try {
            const stored = localStorage.getItem('factory-filters');
            if (stored) {
                activeFilters.value = JSON.parse(stored);
            }

            const savedStored = localStorage.getItem('factory-saved-filters');
            if (savedStored) {
                savedFilters.value = JSON.parse(savedStored);
            }
        } catch (e) {
            console.error('Error loading filters:', e);
        }
    };

    // Save filters to localStorage
    const saveFiltersToStorage = () => {
        try {
            localStorage.setItem('factory-filters', JSON.stringify(activeFilters.value));
            localStorage.setItem('factory-saved-filters', JSON.stringify(savedFilters.value));
        } catch (e) {
            console.error('Error saving filters:', e);
        }
    };

    // Update URL with filters
    const updateURLWithFilters = () => {
        const params = new URLSearchParams();
        for (const [key, value] of Object.entries(activeFilters.value)) {
            if (Array.isArray(value)) {
                params.set(key, value.join(','));
            } else if (value) {
                params.set(key, value);
            }
        }
        const newURL = params.toString() ? `?${params.toString()}` : window.location.pathname;
        history.replaceState(null, '', newURL);
    };

    // Load filters from URL
    const loadFiltersFromURL = () => {
        const params = new URLSearchParams(window.location.search);
        const urlFilters = {};
        for (const [key, value] of params.entries()) {
            if (value.includes(',')) {
                urlFilters[key] = value.split(',');
            } else {
                urlFilters[key] = value;
            }
        }
        if (Object.keys(urlFilters).length > 0) {
            activeFilters.value = urlFilters;
        }
    };

    // Apply a filter
    const applyFilter = (key, value) => {
        activeFilters.value[key] = value;
        saveFiltersToStorage();
        updateURLWithFilters();
        // Trigger re-filter of data
        filterStories();
    };

    // Remove a filter
    const removeFilter = (key) => {
        delete activeFilters.value[key];
        saveFiltersToStorage();
        updateURLWithFilters();
        filterStories();
    };

    // Clear all filters
    const clearAllFilters = () => {
        activeFilters.value = {};
        saveFiltersToStorage();
        updateURLWithFilters();
        filterStories();
    };

    // Apply saved filter
    const applySavedFilter = (filter) => {
        activeFilters.value = { ...filter.filters };
        saveFiltersToStorage();
        updateURLWithFilters();
        filterStories();
        showFilterPanel.value = false;
    };

    // Save current filter
    const saveCurrentFilter = async () => {
        if (!newFilterName.value.trim()) return;

        const newFilter = {
            id: Date.now().toString(),
            name: newFilterName.value.trim(),
            icon: newFilterIcon.value,
            view: 'kanban',
            filters: { ...activeFilters.value }
        };

        savedFilters.value.push(newFilter);
        saveFiltersToStorage();

        // Also save to server if available
        try {
            await fetch('/api/filters', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(newFilter)
            });
        } catch (e) {
            // Server save optional
        }

        newFilterName.value = '';
        newFilterIcon.value = '⭐';
        showSaveFilterModal.value = false;
    };

    // Delete saved filter
    const deleteSavedFilter = (filterId) => {
        savedFilters.value = savedFilters.value.filter(f => f.id !== filterId);
        saveFiltersToStorage();

        // Also delete from server if available
        try {
            fetch(`/api/filters/${filterId}`, { method: 'DELETE' });
        } catch (e) {
            // Server delete optional
        }
    };

    // Quick filters
    const applyQuickFilter = (type) => {
        switch (type) {
            case 'my-stories':
                activeFilters.value = { assignee: currentUser.value?.username };
                break;
            case 'in-progress':
                activeFilters.value = { status: ['in_progress', 'review'] };
                break;
            case 'high-priority':
                activeFilters.value = { priority: ['urgent', 'high'] };
                break;
            case 'current-sprint':
                activeFilters.value = { sprint: 'current' };
                break;
        }
        saveFiltersToStorage();
        updateURLWithFilters();
        filterStories();
    };

    // Filter stories based on active filters
    const filterStories = () => {
        // This will be called by the main app
        // Emit event or call parent function
        if (typeof applyActiveFilters === 'function') {
            applyActiveFilters(activeFilters.value);
        }
    };

    // Initialize filters on mount
    onMounted(() => {
        loadFiltersFromStorage();
        loadFiltersFromURL();
    });

    // Watch for filter changes
    watch(activeFilters, () => {
        saveFiltersToStorage();
        updateURLWithFilters();
    }, { deep: true });
    '''


def register_filter_persistence(app):
    """Register filter persistence endpoints with the FastAPI app."""
    app.include_router(router)
    print("[Dashboard] Filter Persistence loaded: /api/filters")
