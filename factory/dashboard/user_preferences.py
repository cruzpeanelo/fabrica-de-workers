# -*- coding: utf-8 -*-
"""
User Preferences Module (Issue #253)
=====================================
Preferencias de usuario persistentes.

Funcionalidades:
- Preferencias de visualizacao (tema, densidade, idioma)
- Preferencias de notificacao
- Dashboard customizado (widgets)
- Filtros salvos
- Atalhos de teclado customizados
- Projetos favoritos
- Layout de Kanban
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import json

router = APIRouter(prefix="/api/preferences", tags=["User Preferences"])

# User preferences storage (in production, use database)
user_preferences: Dict[str, dict] = {}

# Default preferences
DEFAULT_PREFERENCES = {
    "display": {
        "theme": "light",  # light, dark, system
        "density": "comfortable",  # compact, comfortable, spacious
        "language": "pt-BR",
        "date_format": "DD/MM/YYYY",
        "time_format": "24h",
        "sidebar_collapsed": False,
        "show_story_previews": True,
        "card_size": "medium"  # small, medium, large
    },
    "notifications": {
        "email_enabled": True,
        "push_enabled": True,
        "desktop_enabled": True,
        "sound_enabled": True,
        "notify_on_assignment": True,
        "notify_on_mention": True,
        "notify_on_comment": True,
        "notify_on_status_change": True,
        "notify_on_due_date": True,
        "daily_digest": False,
        "weekly_digest": True
    },
    "kanban": {
        "default_view": "board",  # board, list, table
        "show_avatars": True,
        "show_story_points": True,
        "show_due_dates": True,
        "show_tags": True,
        "column_order": ["backlog", "ready", "in_progress", "review", "testing", "done"],
        "hidden_columns": [],
        "wip_limits": {},
        "auto_refresh": True,
        "refresh_interval": 30  # seconds
    },
    "dashboard": {
        "widgets": [
            {"id": "velocity", "position": 0, "size": "half", "visible": True},
            {"id": "burndown", "position": 1, "size": "half", "visible": True},
            {"id": "my_tasks", "position": 2, "size": "full", "visible": True},
            {"id": "recent_activity", "position": 3, "size": "half", "visible": True},
            {"id": "team_stats", "position": 4, "size": "half", "visible": True}
        ],
        "default_project": None,
        "show_welcome": True
    },
    "saved_filters": [],
    "favorites": {
        "projects": [],
        "stories": [],
        "boards": []
    },
    "keyboard_shortcuts": {
        "custom": {},  # Override default shortcuts
        "enabled": True
    },
    "editor": {
        "font_size": 14,
        "tab_size": 2,
        "word_wrap": True,
        "show_line_numbers": True,
        "auto_save": True,
        "auto_save_interval": 30  # seconds
    }
}


class PreferencesUpdate(BaseModel):
    display: Optional[Dict[str, Any]] = None
    notifications: Optional[Dict[str, Any]] = None
    kanban: Optional[Dict[str, Any]] = None
    dashboard: Optional[Dict[str, Any]] = None
    saved_filters: Optional[List[Dict[str, Any]]] = None
    favorites: Optional[Dict[str, List[str]]] = None
    keyboard_shortcuts: Optional[Dict[str, Any]] = None
    editor: Optional[Dict[str, Any]] = None


class SavedFilter(BaseModel):
    name: str
    description: Optional[str] = None
    filters: Dict[str, Any]
    is_default: bool = False


class DashboardWidget(BaseModel):
    id: str
    position: int
    size: str = "half"  # half, full, third
    visible: bool = True
    config: Optional[Dict[str, Any]] = None


@router.get("/")
async def get_preferences(user_id: str = Query("default")):
    """Retorna todas as preferencias do usuario."""
    prefs = user_preferences.get(user_id, None)

    if not prefs:
        # Return defaults for new user
        prefs = json.loads(json.dumps(DEFAULT_PREFERENCES))  # Deep copy
        user_preferences[user_id] = prefs

    return {
        "user_id": user_id,
        "preferences": prefs,
        "last_updated": prefs.get("_updated_at")
    }


@router.put("/")
async def update_preferences(
    updates: PreferencesUpdate,
    user_id: str = Query("default")
):
    """Atualiza preferencias do usuario."""
    if user_id not in user_preferences:
        user_preferences[user_id] = json.loads(json.dumps(DEFAULT_PREFERENCES))

    prefs = user_preferences[user_id]

    # Update each category if provided
    if updates.display:
        prefs["display"].update(updates.display)
    if updates.notifications:
        prefs["notifications"].update(updates.notifications)
    if updates.kanban:
        prefs["kanban"].update(updates.kanban)
    if updates.dashboard:
        prefs["dashboard"].update(updates.dashboard)
    if updates.saved_filters is not None:
        prefs["saved_filters"] = updates.saved_filters
    if updates.favorites:
        prefs["favorites"].update(updates.favorites)
    if updates.keyboard_shortcuts:
        prefs["keyboard_shortcuts"].update(updates.keyboard_shortcuts)
    if updates.editor:
        prefs["editor"].update(updates.editor)

    prefs["_updated_at"] = datetime.now().isoformat()

    return {
        "success": True,
        "preferences": prefs
    }


@router.post("/reset")
async def reset_preferences(
    user_id: str = Query("default"),
    category: Optional[str] = Query(None)
):
    """Reseta preferencias para padrao."""
    if category:
        if category not in DEFAULT_PREFERENCES:
            raise HTTPException(status_code=400, detail=f"Categoria invalida: {category}")

        if user_id in user_preferences:
            user_preferences[user_id][category] = json.loads(
                json.dumps(DEFAULT_PREFERENCES[category])
            )
    else:
        user_preferences[user_id] = json.loads(json.dumps(DEFAULT_PREFERENCES))

    return {
        "success": True,
        "message": f"Preferencias {'de ' + category if category else ''} resetadas"
    }


# Saved Filters
@router.get("/filters")
async def get_saved_filters(user_id: str = Query("default")):
    """Retorna filtros salvos do usuario."""
    prefs = user_preferences.get(user_id, DEFAULT_PREFERENCES)
    return {
        "filters": prefs.get("saved_filters", [])
    }


@router.post("/filters")
async def save_filter(filter_data: SavedFilter, user_id: str = Query("default")):
    """Salva um novo filtro."""
    if user_id not in user_preferences:
        user_preferences[user_id] = json.loads(json.dumps(DEFAULT_PREFERENCES))

    filters = user_preferences[user_id].get("saved_filters", [])

    # If setting as default, unset others
    if filter_data.is_default:
        for f in filters:
            f["is_default"] = False

    new_filter = {
        "id": f"filter_{len(filters) + 1}",
        "name": filter_data.name,
        "description": filter_data.description,
        "filters": filter_data.filters,
        "is_default": filter_data.is_default,
        "created_at": datetime.now().isoformat()
    }

    filters.append(new_filter)
    user_preferences[user_id]["saved_filters"] = filters

    return {
        "success": True,
        "filter": new_filter
    }


@router.delete("/filters/{filter_id}")
async def delete_filter(filter_id: str, user_id: str = Query("default")):
    """Remove um filtro salvo."""
    if user_id not in user_preferences:
        raise HTTPException(status_code=404, detail="Usuario nao encontrado")

    filters = user_preferences[user_id].get("saved_filters", [])
    user_preferences[user_id]["saved_filters"] = [
        f for f in filters if f.get("id") != filter_id
    ]

    return {"success": True}


# Favorites
@router.get("/favorites")
async def get_favorites(user_id: str = Query("default")):
    """Retorna favoritos do usuario."""
    prefs = user_preferences.get(user_id, DEFAULT_PREFERENCES)
    return prefs.get("favorites", {"projects": [], "stories": [], "boards": []})


@router.post("/favorites/{type}/{item_id}")
async def add_favorite(type: str, item_id: str, user_id: str = Query("default")):
    """Adiciona item aos favoritos."""
    if type not in ["projects", "stories", "boards"]:
        raise HTTPException(status_code=400, detail="Tipo invalido")

    if user_id not in user_preferences:
        user_preferences[user_id] = json.loads(json.dumps(DEFAULT_PREFERENCES))

    favorites = user_preferences[user_id].get("favorites", {})
    if type not in favorites:
        favorites[type] = []

    if item_id not in favorites[type]:
        favorites[type].append(item_id)

    user_preferences[user_id]["favorites"] = favorites

    return {"success": True, "favorites": favorites}


@router.delete("/favorites/{type}/{item_id}")
async def remove_favorite(type: str, item_id: str, user_id: str = Query("default")):
    """Remove item dos favoritos."""
    if user_id not in user_preferences:
        return {"success": True}

    favorites = user_preferences[user_id].get("favorites", {})
    if type in favorites and item_id in favorites[type]:
        favorites[type].remove(item_id)

    return {"success": True}


# Dashboard Widgets
@router.get("/dashboard/widgets")
async def get_dashboard_widgets(user_id: str = Query("default")):
    """Retorna configuracao de widgets do dashboard."""
    prefs = user_preferences.get(user_id, DEFAULT_PREFERENCES)
    return {
        "widgets": prefs.get("dashboard", {}).get("widgets", DEFAULT_PREFERENCES["dashboard"]["widgets"])
    }


@router.put("/dashboard/widgets")
async def update_dashboard_widgets(
    widgets: List[DashboardWidget],
    user_id: str = Query("default")
):
    """Atualiza configuracao de widgets do dashboard."""
    if user_id not in user_preferences:
        user_preferences[user_id] = json.loads(json.dumps(DEFAULT_PREFERENCES))

    user_preferences[user_id]["dashboard"]["widgets"] = [
        w.dict() for w in widgets
    ]

    return {"success": True}


# Export/Import
@router.get("/export")
async def export_preferences(user_id: str = Query("default")):
    """Exporta preferencias do usuario."""
    prefs = user_preferences.get(user_id, DEFAULT_PREFERENCES)

    return {
        "preferences": prefs,
        "exported_at": datetime.now().isoformat(),
        "version": "1.0"
    }


@router.post("/import")
async def import_preferences(data: Dict[str, Any], user_id: str = Query("default")):
    """Importa preferencias de um arquivo."""
    if "preferences" not in data:
        raise HTTPException(status_code=400, detail="Formato invalido")

    prefs = data["preferences"]
    prefs["_imported_at"] = datetime.now().isoformat()

    user_preferences[user_id] = prefs

    return {"success": True}


def get_preferences_html():
    """Retorna o HTML do painel de preferencias."""
    return '''
    <!-- User Preferences Panel (Issue #253) -->
    <div v-if="showPreferencesModal"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showPreferencesModal = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-4xl mx-4 max-h-[85vh] overflow-hidden flex">
            <!-- Sidebar -->
            <div class="w-48 bg-gray-50 border-r p-4">
                <nav class="space-y-1">
                    <button v-for="tab in preferenceTabs" :key="tab.id"
                            @click="activePreferenceTab = tab.id"
                            :class="['w-full px-3 py-2 text-left rounded-lg text-sm',
                                     activePreferenceTab === tab.id ? 'bg-blue-100 text-blue-700' : 'hover:bg-gray-100']">
                        {{ tab.label }}
                    </button>
                </nav>
            </div>

            <!-- Content -->
            <div class="flex-1 flex flex-col">
                <div class="px-6 py-4 border-b flex items-center justify-between">
                    <h2 class="text-lg font-bold">Preferencias</h2>
                    <button @click="showPreferencesModal = false" class="text-gray-400 hover:text-gray-600">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>

                <div class="flex-1 overflow-y-auto p-6">
                    <!-- Display Settings -->
                    <div v-if="activePreferenceTab === 'display'" class="space-y-6">
                        <h3 class="font-semibold">Visualizacao</h3>

                        <div class="grid grid-cols-2 gap-4">
                            <div>
                                <label class="block text-sm font-medium mb-1">Tema</label>
                                <select v-model="userPrefs.display.theme" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option value="light">Claro</option>
                                    <option value="dark">Escuro</option>
                                    <option value="system">Sistema</option>
                                </select>
                            </div>

                            <div>
                                <label class="block text-sm font-medium mb-1">Densidade</label>
                                <select v-model="userPrefs.display.density" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option value="compact">Compacto</option>
                                    <option value="comfortable">Confortavel</option>
                                    <option value="spacious">Espacoso</option>
                                </select>
                            </div>

                            <div>
                                <label class="block text-sm font-medium mb-1">Idioma</label>
                                <select v-model="userPrefs.display.language" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option value="pt-BR">Portugues (Brasil)</option>
                                    <option value="en-US">English (US)</option>
                                    <option value="es">Espanol</option>
                                </select>
                            </div>

                            <div>
                                <label class="block text-sm font-medium mb-1">Formato de Data</label>
                                <select v-model="userPrefs.display.date_format" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option value="DD/MM/YYYY">DD/MM/YYYY</option>
                                    <option value="MM/DD/YYYY">MM/DD/YYYY</option>
                                    <option value="YYYY-MM-DD">YYYY-MM-DD</option>
                                </select>
                            </div>
                        </div>

                        <div class="space-y-3">
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.display.sidebar_collapsed"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Menu lateral colapsado por padrao</span>
                            </label>
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.display.show_story_previews"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Mostrar preview de stories</span>
                            </label>
                        </div>
                    </div>

                    <!-- Notification Settings -->
                    <div v-if="activePreferenceTab === 'notifications'" class="space-y-6">
                        <h3 class="font-semibold">Notificacoes</h3>

                        <div class="space-y-3">
                            <label class="flex items-center justify-between">
                                <span class="text-sm">Notificacoes por Email</span>
                                <input type="checkbox" v-model="userPrefs.notifications.email_enabled"
                                       @change="savePreferences" class="rounded">
                            </label>
                            <label class="flex items-center justify-between">
                                <span class="text-sm">Notificacoes Push</span>
                                <input type="checkbox" v-model="userPrefs.notifications.push_enabled"
                                       @change="savePreferences" class="rounded">
                            </label>
                            <label class="flex items-center justify-between">
                                <span class="text-sm">Som de Notificacao</span>
                                <input type="checkbox" v-model="userPrefs.notifications.sound_enabled"
                                       @change="savePreferences" class="rounded">
                            </label>
                        </div>

                        <h4 class="font-medium text-sm text-gray-500 uppercase mt-6">Notificar quando</h4>
                        <div class="space-y-3">
                            <label class="flex items-center justify-between">
                                <span class="text-sm">For atribuido a mim</span>
                                <input type="checkbox" v-model="userPrefs.notifications.notify_on_assignment"
                                       @change="savePreferences" class="rounded">
                            </label>
                            <label class="flex items-center justify-between">
                                <span class="text-sm">For mencionado</span>
                                <input type="checkbox" v-model="userPrefs.notifications.notify_on_mention"
                                       @change="savePreferences" class="rounded">
                            </label>
                            <label class="flex items-center justify-between">
                                <span class="text-sm">Novo comentario</span>
                                <input type="checkbox" v-model="userPrefs.notifications.notify_on_comment"
                                       @change="savePreferences" class="rounded">
                            </label>
                            <label class="flex items-center justify-between">
                                <span class="text-sm">Mudanca de status</span>
                                <input type="checkbox" v-model="userPrefs.notifications.notify_on_status_change"
                                       @change="savePreferences" class="rounded">
                            </label>
                        </div>
                    </div>

                    <!-- Kanban Settings -->
                    <div v-if="activePreferenceTab === 'kanban'" class="space-y-6">
                        <h3 class="font-semibold">Kanban</h3>

                        <div class="grid grid-cols-2 gap-4">
                            <div>
                                <label class="block text-sm font-medium mb-1">Visualizacao Padrao</label>
                                <select v-model="userPrefs.kanban.default_view" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option value="board">Board</option>
                                    <option value="list">Lista</option>
                                    <option value="table">Tabela</option>
                                </select>
                            </div>
                            <div>
                                <label class="block text-sm font-medium mb-1">Intervalo de Atualizacao</label>
                                <select v-model="userPrefs.kanban.refresh_interval" @change="savePreferences"
                                        class="w-full px-3 py-2 border rounded-lg">
                                    <option :value="10">10 segundos</option>
                                    <option :value="30">30 segundos</option>
                                    <option :value="60">1 minuto</option>
                                    <option :value="0">Desabilitado</option>
                                </select>
                            </div>
                        </div>

                        <div class="space-y-3">
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.kanban.show_avatars"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Mostrar avatares</span>
                            </label>
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.kanban.show_story_points"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Mostrar story points</span>
                            </label>
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.kanban.show_due_dates"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Mostrar datas de entrega</span>
                            </label>
                            <label class="flex items-center gap-2">
                                <input type="checkbox" v-model="userPrefs.kanban.show_tags"
                                       @change="savePreferences" class="rounded">
                                <span class="text-sm">Mostrar tags</span>
                            </label>
                        </div>
                    </div>

                    <!-- Dashboard Settings -->
                    <div v-if="activePreferenceTab === 'dashboard'" class="space-y-6">
                        <h3 class="font-semibold">Dashboard</h3>

                        <div class="space-y-4">
                            <h4 class="text-sm font-medium">Widgets</h4>
                            <div class="space-y-2">
                                <div v-for="widget in userPrefs.dashboard.widgets" :key="widget.id"
                                     class="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                                    <div class="flex items-center gap-3">
                                        <input type="checkbox" v-model="widget.visible"
                                               @change="savePreferences" class="rounded">
                                        <span class="text-sm">{{ getWidgetLabel(widget.id) }}</span>
                                    </div>
                                    <select v-model="widget.size" @change="savePreferences"
                                            class="px-2 py-1 border rounded text-sm">
                                        <option value="half">Metade</option>
                                        <option value="full">Inteiro</option>
                                        <option value="third">Um terco</option>
                                    </select>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Saved Filters -->
                    <div v-if="activePreferenceTab === 'filters'" class="space-y-6">
                        <div class="flex items-center justify-between">
                            <h3 class="font-semibold">Filtros Salvos</h3>
                            <button @click="showSaveFilterModal = true"
                                    class="px-3 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700">
                                Novo Filtro
                            </button>
                        </div>

                        <div class="space-y-2">
                            <div v-for="filter in userPrefs.saved_filters" :key="filter.id"
                                 class="flex items-center justify-between p-3 border rounded-lg">
                                <div>
                                    <span class="font-medium">{{ filter.name }}</span>
                                    <span v-if="filter.is_default" class="ml-2 text-xs bg-blue-100 text-blue-600 px-2 py-0.5 rounded">
                                        Padrao
                                    </span>
                                </div>
                                <div class="flex gap-2">
                                    <button @click="applyFilter(filter)" class="text-sm text-blue-600">Aplicar</button>
                                    <button @click="deleteFilter(filter.id)" class="text-sm text-red-600">Excluir</button>
                                </div>
                            </div>
                            <div v-if="!userPrefs.saved_filters?.length" class="text-gray-400 text-center py-4">
                                Nenhum filtro salvo
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Footer -->
                <div class="px-6 py-3 bg-gray-50 border-t flex justify-between">
                    <button @click="resetPreferences(activePreferenceTab)"
                            class="text-sm text-gray-600 hover:underline">
                        Restaurar Padrao
                    </button>
                    <div class="flex gap-2">
                        <button @click="exportPreferences"
                                class="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 rounded">
                            Exportar
                        </button>
                        <button @click="showPreferencesModal = false"
                                class="px-4 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700">
                            Fechar
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>
    '''


def get_preferences_js():
    """Retorna o JavaScript para preferencias."""
    return '''
    // User Preferences State
    showPreferencesModal: false,
    activePreferenceTab: 'display',
    userPrefs: {
        display: { theme: 'light', density: 'comfortable', language: 'pt-BR', date_format: 'DD/MM/YYYY', sidebar_collapsed: false, show_story_previews: true },
        notifications: { email_enabled: true, push_enabled: true, sound_enabled: true, notify_on_assignment: true, notify_on_mention: true, notify_on_comment: true, notify_on_status_change: true },
        kanban: { default_view: 'board', show_avatars: true, show_story_points: true, show_due_dates: true, show_tags: true, refresh_interval: 30 },
        dashboard: { widgets: [] },
        saved_filters: []
    },
    preferenceTabs: [
        { id: 'display', label: 'Visualizacao' },
        { id: 'notifications', label: 'Notificacoes' },
        { id: 'kanban', label: 'Kanban' },
        { id: 'dashboard', label: 'Dashboard' },
        { id: 'filters', label: 'Filtros Salvos' }
    ],

    // Preferences Methods
    async loadUserPreferences() {
        try {
            const userId = this.currentUser || 'default';
            const response = await fetch(`/api/preferences/?user_id=${userId}`);
            const data = await response.json();
            this.userPrefs = { ...this.userPrefs, ...data.preferences };
            this.applyPreferences();
        } catch (e) {
            console.error('Error loading preferences:', e);
        }
    },

    async savePreferences() {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/preferences/?user_id=${userId}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(this.userPrefs)
            });
            this.applyPreferences();
        } catch (e) {
            console.error('Error saving preferences:', e);
        }
    },

    applyPreferences() {
        // Apply theme
        document.documentElement.classList.toggle('dark', this.userPrefs.display.theme === 'dark');

        // Apply density
        document.body.dataset.density = this.userPrefs.display.density;

        // Store in localStorage for persistence
        localStorage.setItem('userPreferences', JSON.stringify(this.userPrefs));
    },

    async resetPreferences(category) {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/preferences/reset?user_id=${userId}&category=${category}`, {
                method: 'POST'
            });
            await this.loadUserPreferences();
            this.showNotification('Preferencias restauradas', 'success');
        } catch (e) {
            this.showNotification('Erro ao restaurar preferencias', 'error');
        }
    },

    async exportPreferences() {
        const userId = this.currentUser || 'default';
        const response = await fetch(`/api/preferences/export?user_id=${userId}`);
        const data = await response.json();

        const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'user_preferences.json';
        a.click();
    },

    getWidgetLabel(widgetId) {
        const labels = {
            'velocity': 'Velocity',
            'burndown': 'Burndown Chart',
            'my_tasks': 'Minhas Tarefas',
            'recent_activity': 'Atividade Recente',
            'team_stats': 'Stats do Time'
        };
        return labels[widgetId] || widgetId;
    },

    async deleteFilter(filterId) {
        const userId = this.currentUser || 'default';
        await fetch(`/api/preferences/filters/${filterId}?user_id=${userId}`, {
            method: 'DELETE'
        });
        await this.loadUserPreferences();
    }
    '''


def register_user_preferences(app):
    """Registra os endpoints de preferencias no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] User Preferences endpoints loaded: /api/preferences/*")
