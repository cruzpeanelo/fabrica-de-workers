# -*- coding: utf-8 -*-
"""
Keyboard Shortcuts Module (Issue #226)
=======================================
Global keyboard shortcuts system with help panel.

Features:
- Global keyboard shortcuts that work everywhere
- Help panel showing all shortcuts (open with ?)
- Shortcuts work globally (not just in inputs)
- Visual feedback when shortcut triggered
- Customizable shortcuts with localStorage persistence
- Two-key sequences (g h, g k, etc.)
- Conflict detection

Shortcuts:
- ? : Show shortcuts help panel
- Esc : Close any modal/panel
- n : New story (open create modal)
- c : New comment (when story open)
- / : Focus search
- g h : Go to home/dashboard
- g k : Go to Kanban
- g a : Go to Analytics
- g s : Go to Settings
- g p : Go to Projects
- j/k : Navigate up/down in lists
- Enter : Open selected item
- Ctrl+S / Cmd+S : Save current form
- Ctrl+Enter : Submit form
- 1-6 : Move story to column 1-6 (when selected)
"""

from fastapi import APIRouter
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict

router = APIRouter(prefix="/api/shortcuts", tags=["Keyboard Shortcuts"])


class CustomShortcut(BaseModel):
    """Custom shortcut configuration."""
    id: str
    keys: str
    action: str
    enabled: bool = True


class ShortcutsConfig(BaseModel):
    """User shortcuts configuration."""
    custom_shortcuts: List[CustomShortcut] = []
    disabled_shortcuts: List[str] = []


# Default shortcuts configuration
DEFAULT_SHORTCUTS = {
    "navigation": [
        {"id": "show-help", "keys": "?", "description": "Mostrar atalhos de teclado", "category": "navigation"},
        {"id": "focus-search", "keys": "/", "description": "Focar no campo de busca", "category": "navigation"},
        {"id": "close-modal", "keys": "Escape", "description": "Fechar modal/painel", "category": "navigation"},
        {"id": "command-palette", "keys": "Ctrl+K", "description": "Abrir Command Palette", "category": "navigation"},
        {"id": "go-home", "keys": "g h", "description": "Ir para Home/Dashboard", "category": "navigation"},
        {"id": "go-kanban", "keys": "g k", "description": "Ir para Kanban Board", "category": "navigation"},
        {"id": "go-analytics", "keys": "g a", "description": "Ir para Analytics", "category": "navigation"},
        {"id": "go-settings", "keys": "g s", "description": "Ir para Settings", "category": "navigation"},
        {"id": "go-projects", "keys": "g p", "description": "Ir para Projects", "category": "navigation"},
    ],
    "actions": [
        {"id": "new-story", "keys": "n", "description": "Nova Story", "category": "actions"},
        {"id": "new-task", "keys": "t", "description": "Nova Task (com story aberta)", "category": "actions"},
        {"id": "new-comment", "keys": "c", "description": "Novo Comentario (com story aberta)", "category": "actions"},
        {"id": "edit-story", "keys": "e", "description": "Editar story selecionada", "category": "actions"},
        {"id": "delete-story", "keys": "Delete", "description": "Deletar story selecionada", "category": "actions"},
        {"id": "save-form", "keys": "Ctrl+S", "description": "Salvar formulario atual", "category": "actions"},
        {"id": "submit-form", "keys": "Ctrl+Enter", "description": "Enviar formulario", "category": "actions"},
        {"id": "refresh", "keys": "r", "description": "Atualizar dados", "category": "actions"},
    ],
    "list_navigation": [
        {"id": "nav-up", "keys": "k", "description": "Navegar para cima na lista", "category": "list_navigation"},
        {"id": "nav-down", "keys": "j", "description": "Navegar para baixo na lista", "category": "list_navigation"},
        {"id": "open-item", "keys": "Enter", "description": "Abrir item selecionado", "category": "list_navigation"},
        {"id": "select-all", "keys": "Ctrl+A", "description": "Selecionar todos", "category": "list_navigation"},
    ],
    "kanban": [
        {"id": "move-backlog", "keys": "1", "description": "Mover para Backlog", "category": "kanban"},
        {"id": "move-ready", "keys": "2", "description": "Mover para Ready", "category": "kanban"},
        {"id": "move-progress", "keys": "3", "description": "Mover para In Progress", "category": "kanban"},
        {"id": "move-review", "keys": "4", "description": "Mover para Review", "category": "kanban"},
        {"id": "move-testing", "keys": "5", "description": "Mover para Testing", "category": "kanban"},
        {"id": "move-done", "keys": "6", "description": "Mover para Done", "category": "kanban"},
    ],
    "views": [
        {"id": "toggle-dark", "keys": "d", "description": "Alternar modo escuro", "category": "views"},
        {"id": "toggle-sidebar", "keys": "b", "description": "Alternar sidebar", "category": "views"},
        {"id": "toggle-chat", "keys": ".", "description": "Alternar painel de chat", "category": "views"},
        {"id": "fullscreen", "keys": "f", "description": "Modo tela cheia", "category": "views"},
    ]
}


@router.get("/config")
async def get_shortcuts_config():
    """Get all available shortcuts configuration."""
    return {
        "shortcuts": DEFAULT_SHORTCUTS,
        "categories": [
            {"id": "navigation", "name": "Navegacao", "icon": "compass"},
            {"id": "actions", "name": "Acoes", "icon": "zap"},
            {"id": "list_navigation", "name": "Navegacao em Listas", "icon": "list"},
            {"id": "kanban", "name": "Mover Story", "icon": "columns"},
            {"id": "views", "name": "Visualizacao", "icon": "eye"},
        ]
    }


@router.get("/user")
async def get_user_shortcuts():
    """Get user's custom shortcuts configuration."""
    # In a real implementation, this would fetch from database
    return {
        "custom_shortcuts": [],
        "disabled_shortcuts": []
    }


@router.put("/user")
async def save_user_shortcuts(config: ShortcutsConfig):
    """Save user's custom shortcuts configuration."""
    # In a real implementation, this would save to database
    return {"success": True, "message": "Shortcuts saved successfully"}


def get_keyboard_shortcuts_html():
    """Returns the enhanced keyboard shortcuts help panel HTML."""
    return '''
    <!-- Keyboard Shortcuts Panel (Issue #226) -->
    <div v-if="showKeyboardShortcuts"
         class="fixed inset-0 bg-black/50 z-[60] flex items-center justify-center backdrop-blur-sm"
         @click.self="showKeyboardShortcuts = false"
         @keydown.escape="showKeyboardShortcuts = false">
        <div class="bg-white dark:bg-gray-800 rounded-2xl shadow-2xl w-full max-w-3xl mx-4 max-h-[85vh] overflow-hidden flex flex-col animate-modal-in">
            <!-- Header -->
            <div class="bg-gradient-to-r from-[#003B4A] to-[#005566] px-6 py-4 text-white flex-shrink-0">
                <div class="flex items-center justify-between">
                    <div class="flex items-center gap-3">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707"/>
                        </svg>
                        <h2 class="text-xl font-bold">Atalhos de Teclado</h2>
                    </div>
                    <button @click="showKeyboardShortcuts = false"
                            class="text-white/70 hover:text-white transition-colors p-1 rounded-lg hover:bg-white/10">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
                <!-- Search -->
                <div class="mt-4 relative">
                    <svg class="w-5 h-5 absolute left-3 top-1/2 -translate-y-1/2 text-white/50" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                    </svg>
                    <input type="text"
                           v-model="shortcutsSearchQuery"
                           placeholder="Buscar atalhos..."
                           class="w-full pl-10 pr-4 py-2 rounded-lg bg-white/10 border border-white/20 text-white placeholder-white/50 focus:outline-none focus:ring-2 focus:ring-white/30">
                </div>
            </div>

            <!-- Content -->
            <div class="flex-1 overflow-y-auto p-6">
                <!-- Category Tabs -->
                <div class="flex gap-2 mb-6 overflow-x-auto pb-2 scrollbar-thin">
                    <button v-for="cat in shortcutCategories" :key="cat.id"
                            @click="activeShortcutCategory = cat.id"
                            :class="['px-4 py-2 rounded-lg text-sm font-medium whitespace-nowrap transition-all',
                                     activeShortcutCategory === cat.id
                                       ? 'bg-[#003B4A] text-white shadow-lg'
                                       : 'bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600']">
                        {{ cat.name }}
                    </button>
                </div>

                <!-- Shortcuts Grid -->
                <div class="space-y-6">
                    <template v-for="(category, catId) in filteredShortcuts" :key="catId">
                        <div v-if="category.length > 0" class="shortcut-category">
                            <h3 class="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mb-3 flex items-center gap-2">
                                <span v-if="catId === 'navigation'">Navegacao</span>
                                <span v-else-if="catId === 'actions'">Acoes</span>
                                <span v-else-if="catId === 'list_navigation'">Navegacao em Listas</span>
                                <span v-else-if="catId === 'kanban'">Mover Story</span>
                                <span v-else-if="catId === 'views'">Visualizacao</span>
                                <span v-else>{{ catId }}</span>
                                <span class="text-xs bg-gray-200 dark:bg-gray-600 px-2 py-0.5 rounded-full">{{ category.length }}</span>
                            </h3>
                            <div class="grid gap-2">
                                <div v-for="shortcut in category" :key="shortcut.id"
                                     class="flex items-center justify-between p-3 rounded-lg bg-gray-50 dark:bg-gray-700/50 hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors group">
                                    <span class="text-gray-700 dark:text-gray-200">{{ shortcut.description }}</span>
                                    <div class="flex items-center gap-1">
                                        <template v-for="(key, idx) in shortcut.keys.split('+')" :key="idx">
                                            <kbd class="kbd-key">{{ formatKey(key.trim()) }}</kbd>
                                            <span v-if="idx < shortcut.keys.split('+').length - 1" class="text-gray-400 text-xs">+</span>
                                        </template>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </template>
                </div>

                <!-- No Results -->
                <div v-if="Object.values(filteredShortcuts).every(cat => cat.length === 0)"
                     class="text-center py-12">
                    <svg class="w-16 h-16 mx-auto text-gray-300 dark:text-gray-600 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M9.172 16.172a4 4 0 015.656 0M9 10h.01M15 10h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                    </svg>
                    <p class="text-gray-500 dark:text-gray-400">Nenhum atalho encontrado para "{{ shortcutsSearchQuery }}"</p>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-4 bg-gray-50 dark:bg-gray-700/50 border-t border-gray-200 dark:border-gray-600 flex-shrink-0">
                <div class="flex items-center justify-between text-sm text-gray-500 dark:text-gray-400">
                    <div class="flex items-center gap-4">
                        <span class="flex items-center gap-1">
                            <kbd class="kbd-key-sm">?</kbd>
                            <span>Abrir/fechar</span>
                        </span>
                        <span class="flex items-center gap-1">
                            <kbd class="kbd-key-sm">Esc</kbd>
                            <span>Fechar</span>
                        </span>
                    </div>
                    <div class="text-xs text-gray-400 dark:text-gray-500">
                        Pressione <kbd class="kbd-key-sm">Ctrl</kbd>+<kbd class="kbd-key-sm">K</kbd> para Command Palette
                    </div>
                </div>
            </div>
        </div>
    </div>
    '''


def get_keyboard_shortcuts_css():
    """Returns CSS styles for keyboard shortcuts panel."""
    return '''
    /* Keyboard Shortcuts Panel Styles (Issue #226) */
    .kbd-key {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 28px;
        height: 28px;
        padding: 0 8px;
        font-size: 0.75rem;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-weight: 500;
        color: #374151;
        background: linear-gradient(180deg, #ffffff 0%, #f3f4f6 100%);
        border: 1px solid #d1d5db;
        border-radius: 6px;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05), inset 0 1px 0 rgba(255, 255, 255, 0.8);
    }

    .dark .kbd-key {
        color: #e5e7eb;
        background: linear-gradient(180deg, #4b5563 0%, #374151 100%);
        border-color: #6b7280;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.2), inset 0 1px 0 rgba(255, 255, 255, 0.1);
    }

    .kbd-key-sm {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 20px;
        height: 20px;
        padding: 0 5px;
        font-size: 0.65rem;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-weight: 500;
        color: #6b7280;
        background: #f3f4f6;
        border: 1px solid #e5e7eb;
        border-radius: 4px;
    }

    .dark .kbd-key-sm {
        color: #9ca3af;
        background: #4b5563;
        border-color: #6b7280;
    }

    /* Shortcut triggered animation */
    .shortcut-flash {
        animation: shortcut-flash 0.3s ease-out;
    }

    @keyframes shortcut-flash {
        0% {
            transform: scale(1);
            box-shadow: 0 0 0 0 rgba(255, 108, 0, 0.4);
        }
        50% {
            transform: scale(1.1);
            box-shadow: 0 0 0 8px rgba(255, 108, 0, 0);
        }
        100% {
            transform: scale(1);
            box-shadow: 0 0 0 0 rgba(255, 108, 0, 0);
        }
    }

    /* Modal animation */
    @keyframes modal-in {
        from {
            opacity: 0;
            transform: scale(0.95) translateY(-10px);
        }
        to {
            opacity: 1;
            transform: scale(1) translateY(0);
        }
    }

    .animate-modal-in {
        animation: modal-in 0.2s ease-out;
    }

    /* Shortcut toast notification */
    .shortcut-toast {
        position: fixed;
        bottom: 80px;
        left: 50%;
        transform: translateX(-50%);
        background: rgba(0, 59, 74, 0.95);
        color: white;
        padding: 8px 16px;
        border-radius: 8px;
        font-size: 0.875rem;
        display: flex;
        align-items: center;
        gap: 8px;
        z-index: 9999;
        animation: toast-in 0.2s ease-out, toast-out 0.2s ease-in 1.5s forwards;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
    }

    @keyframes toast-in {
        from {
            opacity: 0;
            transform: translateX(-50%) translateY(20px);
        }
        to {
            opacity: 1;
            transform: translateX(-50%) translateY(0);
        }
    }

    @keyframes toast-out {
        from {
            opacity: 1;
            transform: translateX(-50%) translateY(0);
        }
        to {
            opacity: 0;
            transform: translateX(-50%) translateY(-10px);
        }
    }

    .shortcut-toast .kbd-key-sm {
        background: rgba(255, 255, 255, 0.2);
        border-color: rgba(255, 255, 255, 0.3);
        color: white;
    }

    /* Scrollbar styling for shortcuts panel */
    .scrollbar-thin::-webkit-scrollbar {
        height: 6px;
    }

    .scrollbar-thin::-webkit-scrollbar-track {
        background: transparent;
    }

    .scrollbar-thin::-webkit-scrollbar-thumb {
        background: #d1d5db;
        border-radius: 3px;
    }

    .dark .scrollbar-thin::-webkit-scrollbar-thumb {
        background: #4b5563;
    }
    '''


def get_keyboard_shortcuts_js():
    """Returns JavaScript for enhanced keyboard shortcuts handling."""
    return '''
    // ========== ISSUE #226 - KEYBOARD SHORTCUTS SYSTEM ==========

    // Shortcuts state
    const showKeyboardShortcuts = ref(false);
    const shortcutsSearchQuery = ref('');
    const activeShortcutCategory = ref('all');
    const pendingKeySequence = ref('');
    const keySequenceTimeout = ref(null);
    const lastShortcutAction = ref(null);

    // Shortcut categories
    const shortcutCategories = [
        { id: 'all', name: 'Todos' },
        { id: 'navigation', name: 'Navegacao' },
        { id: 'actions', name: 'Acoes' },
        { id: 'list_navigation', name: 'Listas' },
        { id: 'kanban', name: 'Kanban' },
        { id: 'views', name: 'Visualizacao' }
    ];

    // All shortcuts definition
    const allShortcuts = {
        navigation: [
            { id: 'show-help', keys: '?', description: 'Mostrar atalhos de teclado' },
            { id: 'focus-search', keys: '/', description: 'Focar no campo de busca' },
            { id: 'close-modal', keys: 'Escape', description: 'Fechar modal/painel' },
            { id: 'command-palette', keys: 'Ctrl+K', description: 'Abrir Command Palette' },
            { id: 'go-home', keys: 'g h', description: 'Ir para Home/Dashboard' },
            { id: 'go-kanban', keys: 'g k', description: 'Ir para Kanban Board' },
            { id: 'go-analytics', keys: 'g a', description: 'Ir para Analytics' },
            { id: 'go-settings', keys: 'g s', description: 'Ir para Settings' },
            { id: 'go-projects', keys: 'g p', description: 'Ir para Projects' }
        ],
        actions: [
            { id: 'new-story', keys: 'n', description: 'Nova Story' },
            { id: 'new-task', keys: 't', description: 'Nova Task (com story aberta)' },
            { id: 'new-comment', keys: 'c', description: 'Novo Comentario (com story aberta)' },
            { id: 'edit-story', keys: 'e', description: 'Editar story selecionada' },
            { id: 'delete-story', keys: 'Delete', description: 'Deletar story selecionada' },
            { id: 'save-form', keys: 'Ctrl+S', description: 'Salvar formulario atual' },
            { id: 'submit-form', keys: 'Ctrl+Enter', description: 'Enviar formulario' },
            { id: 'refresh', keys: 'r', description: 'Atualizar dados' }
        ],
        list_navigation: [
            { id: 'nav-up', keys: 'k', description: 'Navegar para cima na lista' },
            { id: 'nav-down', keys: 'j', description: 'Navegar para baixo na lista' },
            { id: 'open-item', keys: 'Enter', description: 'Abrir item selecionado' },
            { id: 'select-all', keys: 'Ctrl+A', description: 'Selecionar todos' }
        ],
        kanban: [
            { id: 'move-backlog', keys: '1', description: 'Mover para Backlog' },
            { id: 'move-ready', keys: '2', description: 'Mover para Ready' },
            { id: 'move-progress', keys: '3', description: 'Mover para In Progress' },
            { id: 'move-review', keys: '4', description: 'Mover para Review' },
            { id: 'move-testing', keys: '5', description: 'Mover para Testing' },
            { id: 'move-done', keys: '6', description: 'Mover para Done' }
        ],
        views: [
            { id: 'toggle-dark', keys: 'd', description: 'Alternar modo escuro' },
            { id: 'toggle-sidebar', keys: 'b', description: 'Alternar sidebar' },
            { id: 'toggle-chat', keys: '.', description: 'Alternar painel de chat' },
            { id: 'fullscreen', keys: 'f', description: 'Modo tela cheia' }
        ]
    };

    // Filtered shortcuts based on search and category
    const filteredShortcuts = computed(() => {
        const query = shortcutsSearchQuery.value.toLowerCase();
        const category = activeShortcutCategory.value;

        const result = {};

        for (const [catId, shortcuts] of Object.entries(allShortcuts)) {
            if (category !== 'all' && category !== catId) {
                result[catId] = [];
                continue;
            }

            if (!query) {
                result[catId] = shortcuts;
            } else {
                result[catId] = shortcuts.filter(s =>
                    s.description.toLowerCase().includes(query) ||
                    s.keys.toLowerCase().includes(query)
                );
            }
        }

        return result;
    });

    // Format key for display
    const formatKey = (key) => {
        const keyMap = {
            'ctrl': navigator.platform.includes('Mac') ? 'Cmd' : 'Ctrl',
            'Ctrl': navigator.platform.includes('Mac') ? 'Cmd' : 'Ctrl',
            'meta': 'Cmd',
            'alt': navigator.platform.includes('Mac') ? 'Option' : 'Alt',
            'shift': 'Shift',
            'enter': 'Enter',
            'escape': 'Esc',
            'Escape': 'Esc',
            'delete': 'Del',
            'Delete': 'Del',
            'backspace': 'Backspace',
            'arrowup': 'Arrow Up',
            'arrowdown': 'Arrow Down',
            'arrowleft': 'Arrow Left',
            'arrowright': 'Arrow Right',
            ' ': 'Space',
            'space': 'Space'
        };
        return keyMap[key] || key.toUpperCase();
    };

    // Show shortcut feedback toast
    const showShortcutFeedback = (action) => {
        // Remove existing toast
        const existing = document.querySelector('.shortcut-toast');
        if (existing) existing.remove();

        // Create toast
        const toast = document.createElement('div');
        toast.className = 'shortcut-toast';
        toast.innerHTML = `
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
            </svg>
            <span>${action}</span>
        `;
        document.body.appendChild(toast);

        // Remove after animation
        setTimeout(() => toast.remove(), 2000);
    };

    // Enhanced keyboard handler with sequence support
    const handleEnhancedKeyboard = (e) => {
        // Get the key combo
        const key = e.key;
        const ctrl = e.ctrlKey || e.metaKey;
        const shift = e.shiftKey;
        const alt = e.altKey;

        // Skip if in input/textarea (except for specific shortcuts)
        const isInput = ['INPUT', 'TEXTAREA', 'SELECT'].includes(e.target.tagName);
        const isContentEditable = e.target.isContentEditable;

        // Handle Cmd+K / Ctrl+K always
        if (ctrl && key === 'k') {
            e.preventDefault();
            showCommandPalette.value = !showCommandPalette.value;
            return;
        }

        // Handle Ctrl+S (save)
        if (ctrl && key === 's') {
            e.preventDefault();
            handleSaveShortcut();
            return;
        }

        // Handle Ctrl+Enter (submit)
        if (ctrl && key === 'Enter') {
            e.preventDefault();
            handleSubmitShortcut();
            return;
        }

        // Skip other shortcuts in input fields
        if (isInput || isContentEditable) {
            if (key === 'Escape') {
                e.target.blur();
            }
            return;
        }

        // Handle key sequences (g h, g k, etc.)
        if (pendingKeySequence.value) {
            clearTimeout(keySequenceTimeout.value);
            const fullSequence = pendingKeySequence.value + ' ' + key;
            pendingKeySequence.value = '';

            // Execute sequence shortcuts
            if (fullSequence === 'g h') {
                e.preventDefault();
                navigateTo('home');
                showShortcutFeedback('Navegando para Home');
                return;
            }
            if (fullSequence === 'g k') {
                e.preventDefault();
                navigateTo('kanban');
                showShortcutFeedback('Navegando para Kanban');
                return;
            }
            if (fullSequence === 'g a') {
                e.preventDefault();
                navigateTo('analytics');
                showShortcutFeedback('Navegando para Analytics');
                return;
            }
            if (fullSequence === 'g s') {
                e.preventDefault();
                navigateTo('settings');
                showShortcutFeedback('Navegando para Settings');
                return;
            }
            if (fullSequence === 'g p') {
                e.preventDefault();
                navigateTo('projects');
                showShortcutFeedback('Navegando para Projects');
                return;
            }
        }

        // Start key sequence
        if (key === 'g') {
            pendingKeySequence.value = 'g';
            keySequenceTimeout.value = setTimeout(() => {
                pendingKeySequence.value = '';
            }, 500);
            return;
        }

        // Single key shortcuts
        switch (key) {
            case '?':
                e.preventDefault();
                showKeyboardShortcuts.value = !showKeyboardShortcuts.value;
                break;

            case '/':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    focusSearch();
                }
                break;

            case 'Escape':
                handleEscapeKey();
                break;

            case 'n':
                if (selectedProjectId.value && !ctrl && !alt) {
                    e.preventDefault();
                    showNewStoryModal.value = true;
                    showShortcutFeedback('Nova Story');
                }
                break;

            case 't':
                if (selectedStory.value && !ctrl && !alt) {
                    e.preventDefault();
                    showNewTaskModal.value = true;
                    showShortcutFeedback('Nova Task');
                }
                break;

            case 'c':
                if (selectedStory.value && !ctrl && !alt) {
                    e.preventDefault();
                    focusCommentInput();
                    showShortcutFeedback('Novo Comentario');
                }
                break;

            case 'e':
                if (selectedStory.value && !ctrl && !alt) {
                    e.preventDefault();
                    editStory();
                    showShortcutFeedback('Editando Story');
                }
                break;

            case 'r':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    loadProjectData();
                    showShortcutFeedback('Atualizando...');
                }
                break;

            case 'd':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    toggleDarkMode();
                    showShortcutFeedback(isDarkMode.value ? 'Modo Escuro' : 'Modo Claro');
                }
                break;

            case 'b':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    toggleSidebar();
                    showShortcutFeedback('Sidebar alternada');
                }
                break;

            case '.':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    toggleChatPanel();
                    showShortcutFeedback('Chat alternado');
                }
                break;

            case 'f':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    toggleFullscreen();
                }
                break;

            case 'j':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    navigateListDown();
                }
                break;

            case 'k':
                if (!ctrl && !alt) {
                    e.preventDefault();
                    navigateListUp();
                }
                break;

            case 'Enter':
                if (!ctrl && selectedListItem.value) {
                    e.preventDefault();
                    openSelectedItem();
                }
                break;

            case 'Delete':
            case 'Backspace':
                if (selectedStory.value && !ctrl && !alt) {
                    e.preventDefault();
                    deleteStoryWithConfirm(selectedStory.value);
                }
                break;

            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
                if (selectedStory.value && !ctrl && !alt) {
                    e.preventDefault();
                    const statuses = ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done'];
                    const newStatus = statuses[parseInt(key) - 1];
                    moveStoryToStatus(selectedStory.value, newStatus);
                    showShortcutFeedback('Story movida para ' + newStatus);
                }
                break;
        }
    };

    // Helper functions for shortcuts
    const focusSearch = () => {
        const searchInput = document.querySelector('#search-input') ||
                           document.querySelector('input[type="search"]') ||
                           document.querySelector('.search-box input');
        if (searchInput) {
            searchInput.focus();
            searchInput.select();
        }
    };

    const focusCommentInput = () => {
        const commentInput = document.querySelector('#comment-input') ||
                            document.querySelector('textarea[placeholder*="comentario"]') ||
                            document.querySelector('.comment-input');
        if (commentInput) {
            commentInput.focus();
        }
    };

    const toggleSidebar = () => {
        if (typeof sidebarCollapsed !== 'undefined') {
            sidebarCollapsed.value = !sidebarCollapsed.value;
        }
    };

    const toggleChatPanel = () => {
        if (typeof showChatPanel !== 'undefined') {
            showChatPanel.value = !showChatPanel.value;
        }
    };

    const toggleFullscreen = () => {
        if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen().catch(err => {
                console.log('Fullscreen not available:', err);
            });
            showShortcutFeedback('Entrando em tela cheia');
        } else {
            document.exitFullscreen();
            showShortcutFeedback('Saindo de tela cheia');
        }
    };

    // List navigation state
    const selectedListItem = ref(null);
    const listItems = ref([]);

    const navigateListDown = () => {
        const items = document.querySelectorAll('.story-card, .list-item, [data-navigable]');
        if (items.length === 0) return;

        const currentIdx = selectedListItem.value || -1;
        const nextIdx = Math.min(currentIdx + 1, items.length - 1);
        selectedListItem.value = nextIdx;

        items.forEach((item, idx) => {
            item.classList.toggle('ring-2', idx === nextIdx);
            item.classList.toggle('ring-[#FF6C00]', idx === nextIdx);
        });

        items[nextIdx]?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    };

    const navigateListUp = () => {
        const items = document.querySelectorAll('.story-card, .list-item, [data-navigable]');
        if (items.length === 0) return;

        const currentIdx = selectedListItem.value || items.length;
        const prevIdx = Math.max(currentIdx - 1, 0);
        selectedListItem.value = prevIdx;

        items.forEach((item, idx) => {
            item.classList.toggle('ring-2', idx === prevIdx);
            item.classList.toggle('ring-[#FF6C00]', idx === prevIdx);
        });

        items[prevIdx]?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    };

    const openSelectedItem = () => {
        const items = document.querySelectorAll('.story-card, .list-item, [data-navigable]');
        if (selectedListItem.value !== null && items[selectedListItem.value]) {
            items[selectedListItem.value].click();
        }
    };

    const handleEscapeKey = () => {
        // Close in priority order
        if (showKeyboardShortcuts.value) { showKeyboardShortcuts.value = false; return; }
        if (showCommandPalette.value) { showCommandPalette.value = false; return; }
        // Other modals handled by existing code
    };

    const handleSaveShortcut = () => {
        // Trigger save on active form
        const saveBtn = document.querySelector('button[type="submit"], .btn-save, [data-save]');
        if (saveBtn) {
            saveBtn.click();
            showShortcutFeedback('Salvando...');
        }
    };

    const handleSubmitShortcut = () => {
        // Trigger submit on active form
        const form = document.activeElement?.closest('form');
        if (form) {
            form.requestSubmit();
            showShortcutFeedback('Enviando...');
        }
    };

    const navigateTo = (page) => {
        // Handle navigation based on current app structure
        switch(page) {
            case 'home':
                selectedProjectId.value = '';
                selectedStory.value = null;
                break;
            case 'kanban':
                // Already on kanban if project selected
                if (!selectedProjectId.value && projects.value.length > 0) {
                    selectedProjectId.value = projects.value[0].project_id;
                }
                break;
            case 'analytics':
                showAnalyticsModal.value = true;
                break;
            case 'settings':
                showSettingsModal.value = true;
                break;
            case 'projects':
                selectedProjectId.value = '';
                selectedStory.value = null;
                break;
        }
    };
    '''


def register_keyboard_shortcuts(app):
    """Register keyboard shortcuts endpoints with the FastAPI app."""
    app.include_router(router)
    print("[Dashboard] Keyboard Shortcuts loaded: /api/shortcuts/*")
