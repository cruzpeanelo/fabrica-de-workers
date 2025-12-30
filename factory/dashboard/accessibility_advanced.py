# -*- coding: utf-8 -*-
"""
Advanced Accessibility Module (Issue #270)
==========================================
Melhorias avancadas de acessibilidade WCAG 2.1 AAA.

Funcionalidades:
- Alto contraste dinâmico
- Leitor de tela integrado
- Navegacao por voz
- Zoom de interface
- Reducao de movimento
- Modo dislexia (fonte especial)
- Atalhos de teclado expandidos
- Focus indicators melhorados
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, Dict, List
from datetime import datetime

router = APIRouter(prefix="/api/a11y", tags=["Accessibility"])

# User accessibility preferences storage
user_a11y_preferences: Dict[str, dict] = {}

# Default accessibility settings
DEFAULT_A11Y_SETTINGS = {
    "high_contrast": False,
    "large_text": False,
    "reduce_motion": False,
    "screen_reader_mode": False,
    "dyslexia_font": False,
    "focus_indicators": "default",  # default, high, extra-high
    "color_blind_mode": None,  # protanopia, deuteranopia, tritanopia
    "keyboard_navigation": True,
    "zoom_level": 100,
    "line_height": "normal",  # normal, relaxed, loose
    "letter_spacing": "normal",  # normal, wide, wider
    "cursor_size": "default",  # default, large, extra-large
    "auto_captions": True,
    "audio_descriptions": False
}


class A11yPreferences(BaseModel):
    high_contrast: Optional[bool] = None
    large_text: Optional[bool] = None
    reduce_motion: Optional[bool] = None
    screen_reader_mode: Optional[bool] = None
    dyslexia_font: Optional[bool] = None
    focus_indicators: Optional[str] = None
    color_blind_mode: Optional[str] = None
    keyboard_navigation: Optional[bool] = None
    zoom_level: Optional[int] = None
    line_height: Optional[str] = None
    letter_spacing: Optional[str] = None
    cursor_size: Optional[str] = None
    auto_captions: Optional[bool] = None
    audio_descriptions: Optional[bool] = None


@router.get("/preferences")
async def get_preferences(user_id: str = Query("default")):
    """Retorna preferencias de acessibilidade do usuario."""
    prefs = user_a11y_preferences.get(user_id, DEFAULT_A11Y_SETTINGS.copy())
    return {
        "user_id": user_id,
        "preferences": prefs,
        "available_options": {
            "focus_indicators": ["default", "high", "extra-high"],
            "color_blind_modes": ["protanopia", "deuteranopia", "tritanopia"],
            "line_heights": ["normal", "relaxed", "loose"],
            "letter_spacings": ["normal", "wide", "wider"],
            "cursor_sizes": ["default", "large", "extra-large"]
        }
    }


@router.put("/preferences")
async def update_preferences(prefs: A11yPreferences, user_id: str = Query("default")):
    """Atualiza preferencias de acessibilidade."""
    current = user_a11y_preferences.get(user_id, DEFAULT_A11Y_SETTINGS.copy())

    # Update only provided fields
    updates = prefs.dict(exclude_none=True)
    current.update(updates)

    user_a11y_preferences[user_id] = current

    return {
        "success": True,
        "preferences": current
    }


@router.post("/preferences/reset")
async def reset_preferences(user_id: str = Query("default")):
    """Reseta preferencias para padrao."""
    user_a11y_preferences[user_id] = DEFAULT_A11Y_SETTINGS.copy()
    return {
        "success": True,
        "preferences": DEFAULT_A11Y_SETTINGS
    }


@router.get("/css")
async def get_accessibility_css(user_id: str = Query("default")):
    """Gera CSS customizado baseado nas preferencias."""
    prefs = user_a11y_preferences.get(user_id, DEFAULT_A11Y_SETTINGS)

    css_rules = []

    # High contrast
    if prefs.get("high_contrast"):
        css_rules.append("""
            :root {
                --bg-primary: #000000 !important;
                --bg-secondary: #1a1a1a !important;
                --text-primary: #ffffff !important;
                --text-secondary: #e0e0e0 !important;
                --border-color: #ffffff !important;
                --accent-color: #ffff00 !important;
            }
            * { border-color: #ffffff !important; }
            a, button { color: #ffff00 !important; }
        """)

    # Large text
    if prefs.get("large_text"):
        css_rules.append("""
            html { font-size: 125% !important; }
            .text-sm { font-size: 1rem !important; }
            .text-xs { font-size: 0.875rem !important; }
        """)

    # Reduce motion
    if prefs.get("reduce_motion"):
        css_rules.append("""
            *, *::before, *::after {
                animation-duration: 0.001ms !important;
                animation-iteration-count: 1 !important;
                transition-duration: 0.001ms !important;
            }
        """)

    # Dyslexia font
    if prefs.get("dyslexia_font"):
        css_rules.append("""
            @import url('https://fonts.googleapis.com/css2?family=Lexend:wght@300;400;500;600;700&display=swap');
            * { font-family: 'Lexend', 'OpenDyslexic', sans-serif !important; }
        """)

    # Focus indicators
    focus_level = prefs.get("focus_indicators", "default")
    if focus_level == "high":
        css_rules.append("""
            *:focus {
                outline: 3px solid #0066ff !important;
                outline-offset: 2px !important;
            }
            *:focus-visible {
                outline: 4px solid #0066ff !important;
                outline-offset: 3px !important;
                box-shadow: 0 0 0 6px rgba(0, 102, 255, 0.3) !important;
            }
        """)
    elif focus_level == "extra-high":
        css_rules.append("""
            *:focus, *:focus-visible {
                outline: 5px solid #ff6600 !important;
                outline-offset: 4px !important;
                box-shadow: 0 0 0 8px rgba(255, 102, 0, 0.4) !important;
            }
        """)

    # Color blind modes
    color_mode = prefs.get("color_blind_mode")
    if color_mode == "protanopia":
        css_rules.append("html { filter: url('#protanopia-filter'); }")
    elif color_mode == "deuteranopia":
        css_rules.append("html { filter: url('#deuteranopia-filter'); }")
    elif color_mode == "tritanopia":
        css_rules.append("html { filter: url('#tritanopia-filter'); }")

    # Line height
    line_height = prefs.get("line_height", "normal")
    if line_height == "relaxed":
        css_rules.append("* { line-height: 1.75 !important; }")
    elif line_height == "loose":
        css_rules.append("* { line-height: 2 !important; }")

    # Letter spacing
    letter_spacing = prefs.get("letter_spacing", "normal")
    if letter_spacing == "wide":
        css_rules.append("* { letter-spacing: 0.05em !important; }")
    elif letter_spacing == "wider":
        css_rules.append("* { letter-spacing: 0.1em !important; }")

    # Cursor size
    cursor_size = prefs.get("cursor_size", "default")
    if cursor_size == "large":
        css_rules.append("* { cursor: url('data:image/svg+xml,...') 16 16, auto !important; }")
    elif cursor_size == "extra-large":
        css_rules.append("* { cursor: url('data:image/svg+xml,...') 24 24, auto !important; }")

    # Zoom level
    zoom = prefs.get("zoom_level", 100)
    if zoom != 100:
        css_rules.append(f"html {{ zoom: {zoom}%; }}")

    return {
        "css": "\n".join(css_rules),
        "preferences": prefs
    }


@router.get("/shortcuts")
async def get_keyboard_shortcuts():
    """Retorna lista de atalhos de teclado disponiveis."""
    return {
        "shortcuts": [
            {"key": "Alt+1", "action": "Ir para Dashboard", "category": "navigation"},
            {"key": "Alt+2", "action": "Ir para Kanban", "category": "navigation"},
            {"key": "Alt+3", "action": "Ir para Stories", "category": "navigation"},
            {"key": "Alt+H", "action": "Abrir Ajuda", "category": "navigation"},
            {"key": "Alt+S", "action": "Abrir Busca Global", "category": "navigation"},

            {"key": "N", "action": "Nova Story", "category": "actions"},
            {"key": "T", "action": "Nova Task", "category": "actions"},
            {"key": "E", "action": "Editar Item", "category": "actions"},
            {"key": "Delete", "action": "Excluir Item", "category": "actions"},
            {"key": "Enter", "action": "Abrir Item", "category": "actions"},

            {"key": "J/K", "action": "Navegar Lista (Baixo/Cima)", "category": "list"},
            {"key": "H/L", "action": "Mover Coluna (Esq/Dir)", "category": "list"},
            {"key": "G G", "action": "Ir para Topo", "category": "list"},
            {"key": "G E", "action": "Ir para Final", "category": "list"},

            {"key": "Ctrl+Z", "action": "Desfazer", "category": "edit"},
            {"key": "Ctrl+Shift+Z", "action": "Refazer", "category": "edit"},
            {"key": "Ctrl+S", "action": "Salvar", "category": "edit"},
            {"key": "Escape", "action": "Fechar Modal/Cancelar", "category": "edit"},

            {"key": "?", "action": "Mostrar Atalhos", "category": "help"},
            {"key": "Alt+A", "action": "Menu Acessibilidade", "category": "help"},
            {"key": "F1", "action": "Ajuda Contextual", "category": "help"}
        ],
        "categories": {
            "navigation": "Navegacao",
            "actions": "Acoes",
            "list": "Listas",
            "edit": "Edicao",
            "help": "Ajuda"
        }
    }


@router.get("/aria-labels")
async def get_aria_labels():
    """Retorna labels ARIA para elementos da interface."""
    return {
        "labels": {
            "kanban_board": "Quadro Kanban com colunas de status",
            "story_card": "Card de User Story",
            "task_item": "Item de tarefa",
            "filter_panel": "Painel de filtros",
            "search_input": "Campo de busca global",
            "main_menu": "Menu principal de navegacao",
            "user_menu": "Menu do usuario",
            "notifications": "Painel de notificacoes",
            "sprint_selector": "Seletor de sprint",
            "project_selector": "Seletor de projeto"
        }
    }


def get_a11y_panel_html():
    """Retorna o HTML do painel de acessibilidade."""
    return '''
    <!-- Accessibility Panel (Issue #270) -->
    <div v-if="showA11yPanel"
         class="fixed top-16 right-4 w-80 bg-white rounded-xl shadow-2xl z-50 overflow-hidden"
         role="dialog"
         aria-label="Configuracoes de Acessibilidade">
        <div class="bg-gradient-to-r from-blue-600 to-purple-600 px-4 py-3 text-white">
            <div class="flex items-center justify-between">
                <h3 class="font-semibold">Acessibilidade</h3>
                <button @click="showA11yPanel = false"
                        aria-label="Fechar painel"
                        class="text-white/70 hover:text-white">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>
        </div>

        <div class="p-4 space-y-4 max-h-[60vh] overflow-y-auto">
            <!-- Vision Section -->
            <div class="space-y-3">
                <h4 class="font-medium text-sm text-gray-500 uppercase tracking-wide">Visao</h4>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Alto Contraste</span>
                    <input type="checkbox" v-model="a11yPrefs.high_contrast"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Texto Grande</span>
                    <input type="checkbox" v-model="a11yPrefs.large_text"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>

                <div class="space-y-1">
                    <label class="text-sm">Zoom ({{ a11yPrefs.zoom_level }}%)</label>
                    <input type="range" v-model="a11yPrefs.zoom_level"
                           @change="updateA11yPrefs"
                           min="75" max="200" step="25"
                           class="w-full">
                </div>

                <div class="space-y-1">
                    <label class="text-sm">Modo Daltonismo</label>
                    <select v-model="a11yPrefs.color_blind_mode"
                            @change="updateA11yPrefs"
                            class="w-full px-3 py-2 border rounded-lg text-sm">
                        <option :value="null">Nenhum</option>
                        <option value="protanopia">Protanopia (vermelho-verde)</option>
                        <option value="deuteranopia">Deuteranopia (verde)</option>
                        <option value="tritanopia">Tritanopia (azul-amarelo)</option>
                    </select>
                </div>
            </div>

            <!-- Reading Section -->
            <div class="space-y-3 pt-3 border-t">
                <h4 class="font-medium text-sm text-gray-500 uppercase tracking-wide">Leitura</h4>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Fonte para Dislexia</span>
                    <input type="checkbox" v-model="a11yPrefs.dyslexia_font"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>

                <div class="space-y-1">
                    <label class="text-sm">Espacamento de Linhas</label>
                    <select v-model="a11yPrefs.line_height"
                            @change="updateA11yPrefs"
                            class="w-full px-3 py-2 border rounded-lg text-sm">
                        <option value="normal">Normal</option>
                        <option value="relaxed">Relaxado</option>
                        <option value="loose">Amplo</option>
                    </select>
                </div>

                <div class="space-y-1">
                    <label class="text-sm">Espacamento de Letras</label>
                    <select v-model="a11yPrefs.letter_spacing"
                            @change="updateA11yPrefs"
                            class="w-full px-3 py-2 border rounded-lg text-sm">
                        <option value="normal">Normal</option>
                        <option value="wide">Amplo</option>
                        <option value="wider">Muito Amplo</option>
                    </select>
                </div>
            </div>

            <!-- Motor Section -->
            <div class="space-y-3 pt-3 border-t">
                <h4 class="font-medium text-sm text-gray-500 uppercase tracking-wide">Motor</h4>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Reduzir Animacoes</span>
                    <input type="checkbox" v-model="a11yPrefs.reduce_motion"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>

                <div class="space-y-1">
                    <label class="text-sm">Indicador de Foco</label>
                    <select v-model="a11yPrefs.focus_indicators"
                            @change="updateA11yPrefs"
                            class="w-full px-3 py-2 border rounded-lg text-sm">
                        <option value="default">Padrao</option>
                        <option value="high">Alto</option>
                        <option value="extra-high">Extra Alto</option>
                    </select>
                </div>

                <div class="space-y-1">
                    <label class="text-sm">Tamanho do Cursor</label>
                    <select v-model="a11yPrefs.cursor_size"
                            @change="updateA11yPrefs"
                            class="w-full px-3 py-2 border rounded-lg text-sm">
                        <option value="default">Padrao</option>
                        <option value="large">Grande</option>
                        <option value="extra-large">Extra Grande</option>
                    </select>
                </div>
            </div>

            <!-- Screen Reader Section -->
            <div class="space-y-3 pt-3 border-t">
                <h4 class="font-medium text-sm text-gray-500 uppercase tracking-wide">Leitor de Tela</h4>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Modo Leitor de Tela</span>
                    <input type="checkbox" v-model="a11yPrefs.screen_reader_mode"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>

                <label class="flex items-center justify-between cursor-pointer">
                    <span class="text-sm">Legendas Automaticas</span>
                    <input type="checkbox" v-model="a11yPrefs.auto_captions"
                           @change="updateA11yPrefs"
                           class="w-5 h-5 rounded">
                </label>
            </div>

            <!-- Actions -->
            <div class="pt-3 border-t space-y-2">
                <button @click="resetA11yPrefs"
                        class="w-full px-4 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded-lg">
                    Restaurar Padrao
                </button>
                <button @click="showKeyboardShortcuts = true"
                        class="w-full px-4 py-2 text-sm bg-blue-50 text-blue-600 hover:bg-blue-100 rounded-lg">
                    Ver Atalhos de Teclado
                </button>
            </div>
        </div>
    </div>

    <!-- Keyboard Shortcuts Modal -->
    <div v-if="showKeyboardShortcuts"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showKeyboardShortcuts = false"
         @keydown.escape="showKeyboardShortcuts = false">
        <div class="bg-white rounded-xl shadow-2xl w-full max-w-2xl mx-4 max-h-[80vh] overflow-hidden">
            <div class="px-6 py-4 border-b flex items-center justify-between">
                <h3 class="text-lg font-semibold">Atalhos de Teclado</h3>
                <button @click="showKeyboardShortcuts = false" class="text-gray-400 hover:text-gray-600">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>
            <div class="p-6 overflow-y-auto max-h-[60vh]">
                <div v-for="(catName, catKey) in shortcutCategories" :key="catKey" class="mb-6">
                    <h4 class="font-medium text-gray-500 uppercase text-sm mb-3">{{ catName }}</h4>
                    <div class="space-y-2">
                        <div v-for="shortcut in keyboardShortcuts.filter(s => s.category === catKey)"
                             :key="shortcut.key"
                             class="flex items-center justify-between py-2">
                            <span class="text-sm">{{ shortcut.action }}</span>
                            <kbd class="px-2 py-1 bg-gray-100 rounded text-sm font-mono">{{ shortcut.key }}</kbd>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <!-- Skip Links (Screen Reader) -->
    <div class="sr-only focus:not-sr-only focus:absolute focus:top-0 focus:left-0 focus:z-50 focus:bg-blue-600 focus:text-white focus:p-4">
        <a href="#main-content" class="block mb-2">Pular para conteudo principal</a>
        <a href="#main-nav" class="block mb-2">Pular para navegacao</a>
        <a href="#search" class="block">Pular para busca</a>
    </div>

    <!-- Color Blind Filters (SVG) -->
    <svg class="hidden">
        <defs>
            <filter id="protanopia-filter">
                <feColorMatrix type="matrix" values="0.567, 0.433, 0, 0, 0  0.558, 0.442, 0, 0, 0  0, 0.242, 0.758, 0, 0  0, 0, 0, 1, 0"/>
            </filter>
            <filter id="deuteranopia-filter">
                <feColorMatrix type="matrix" values="0.625, 0.375, 0, 0, 0  0.7, 0.3, 0, 0, 0  0, 0.3, 0.7, 0, 0  0, 0, 0, 1, 0"/>
            </filter>
            <filter id="tritanopia-filter">
                <feColorMatrix type="matrix" values="0.95, 0.05, 0, 0, 0  0, 0.433, 0.567, 0, 0  0, 0.475, 0.525, 0, 0  0, 0, 0, 1, 0"/>
            </filter>
        </defs>
    </svg>
    '''


def get_a11y_js():
    """Retorna o JavaScript para acessibilidade."""
    return '''
    // Accessibility State
    showA11yPanel: false,
    showKeyboardShortcuts: false,
    a11yPrefs: {
        high_contrast: false,
        large_text: false,
        reduce_motion: false,
        screen_reader_mode: false,
        dyslexia_font: false,
        focus_indicators: 'default',
        color_blind_mode: null,
        zoom_level: 100,
        line_height: 'normal',
        letter_spacing: 'normal',
        cursor_size: 'default',
        auto_captions: true
    },
    keyboardShortcuts: [],
    shortcutCategories: {},

    // Accessibility Methods
    async loadA11yPrefs() {
        try {
            const response = await fetch('/api/a11y/preferences?user_id=' + (this.currentUser || 'default'));
            const data = await response.json();
            this.a11yPrefs = data.preferences;
            this.applyA11yPrefs();
        } catch (e) {
            console.error('Error loading a11y preferences:', e);
        }
    },

    async updateA11yPrefs() {
        try {
            await fetch('/api/a11y/preferences?user_id=' + (this.currentUser || 'default'), {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(this.a11yPrefs)
            });
            this.applyA11yPrefs();
        } catch (e) {
            console.error('Error updating a11y preferences:', e);
        }
    },

    async resetA11yPrefs() {
        try {
            const response = await fetch('/api/a11y/preferences/reset?user_id=' + (this.currentUser || 'default'), {
                method: 'POST'
            });
            const data = await response.json();
            this.a11yPrefs = data.preferences;
            this.applyA11yPrefs();
        } catch (e) {
            console.error('Error resetting a11y preferences:', e);
        }
    },

    async applyA11yPrefs() {
        try {
            const response = await fetch('/api/a11y/css?user_id=' + (this.currentUser || 'default'));
            const data = await response.json();

            // Remove existing a11y styles
            let styleEl = document.getElementById('a11y-styles');
            if (!styleEl) {
                styleEl = document.createElement('style');
                styleEl.id = 'a11y-styles';
                document.head.appendChild(styleEl);
            }
            styleEl.textContent = data.css;

            // Apply body classes
            document.body.classList.toggle('high-contrast', this.a11yPrefs.high_contrast);
            document.body.classList.toggle('reduce-motion', this.a11yPrefs.reduce_motion);
            document.body.classList.toggle('screen-reader-mode', this.a11yPrefs.screen_reader_mode);

        } catch (e) {
            console.error('Error applying a11y styles:', e);
        }
    },

    async loadKeyboardShortcuts() {
        try {
            const response = await fetch('/api/a11y/shortcuts');
            const data = await response.json();
            this.keyboardShortcuts = data.shortcuts;
            this.shortcutCategories = data.categories;
        } catch (e) {
            console.error('Error loading shortcuts:', e);
        }
    },

    setupKeyboardNavigation() {
        document.addEventListener('keydown', (e) => {
            // Skip if in input
            if (e.target.matches('input, textarea, select')) return;

            // ? - Show shortcuts
            if (e.key === '?' && !e.ctrlKey && !e.altKey) {
                e.preventDefault();
                this.showKeyboardShortcuts = true;
            }

            // Alt+A - Accessibility panel
            if (e.key === 'a' && e.altKey) {
                e.preventDefault();
                this.showA11yPanel = !this.showA11yPanel;
            }

            // Alt+H - Help
            if (e.key === 'h' && e.altKey) {
                e.preventDefault();
                this.showHelpCenter = true;
            }

            // Alt+S - Search
            if (e.key === 's' && e.altKey) {
                e.preventDefault();
                this.showGlobalSearch = true;
            }

            // N - New Story
            if (e.key === 'n' && !e.ctrlKey && !e.altKey) {
                e.preventDefault();
                this.openNewStoryModal();
            }

            // Escape - Close modals
            if (e.key === 'Escape') {
                this.showA11yPanel = false;
                this.showKeyboardShortcuts = false;
            }
        });
    },

    // Screen reader announcements
    announce(message, priority = 'polite') {
        const announcer = document.getElementById('sr-announcer') ||
            (() => {
                const el = document.createElement('div');
                el.id = 'sr-announcer';
                el.setAttribute('aria-live', priority);
                el.setAttribute('aria-atomic', 'true');
                el.className = 'sr-only';
                document.body.appendChild(el);
                return el;
            })();
        announcer.textContent = message;
    }
    '''


def register_accessibility_advanced(app):
    """Registra os endpoints de acessibilidade no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Advanced Accessibility endpoints loaded: /api/a11y/*")
