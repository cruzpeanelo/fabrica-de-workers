# -*- coding: utf-8 -*-
"""
Dark Mode Module (Issue #217)
=============================
Implementa Dark Mode toggle visivel e persistente.

Funcionalidades:
- Toggle button no header (icone sol/lua com animacao)
- Transicao suave entre modos
- Persistencia de preferencia em localStorage
- Respeita preferencia do sistema (prefers-color-scheme)
- CSS variables para temas
- Aplica a todas as paginas do dashboard
- API endpoints para salvar preferencia do usuario
"""

from fastapi import APIRouter, HTTPException, Request, Response
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, Dict
import json

router = APIRouter(prefix="/api/user/preferences", tags=["Dark Mode"])

# In-memory storage (in production, use database)
user_theme_preferences: Dict[str, str] = {}


class ThemePreference(BaseModel):
    """Schema para preferencia de tema."""
    theme: str  # 'light', 'dark', or 'system'


@router.get("/theme")
async def get_theme_preference(request: Request):
    """
    Retorna a preferencia de tema do usuario.

    Returns:
        theme: 'light', 'dark', or 'system'
    """
    # Get user ID from session/cookie (simplified for demo)
    user_id = request.cookies.get("user_id", "anonymous")

    theme = user_theme_preferences.get(user_id, "system")

    return {
        "success": True,
        "theme": theme,
        "message": f"Preferencia atual: {theme}"
    }


@router.put("/theme")
async def save_theme_preference(preference: ThemePreference, request: Request, response: Response):
    """
    Salva a preferencia de tema do usuario.

    Args:
        preference: ThemePreference com 'light', 'dark', ou 'system'
    """
    if preference.theme not in ['light', 'dark', 'system']:
        raise HTTPException(
            status_code=400,
            detail="Tema invalido. Use 'light', 'dark' ou 'system'"
        )

    # Get user ID from session/cookie
    user_id = request.cookies.get("user_id", "anonymous")

    # Save preference
    user_theme_preferences[user_id] = preference.theme

    # Also set in cookie for persistence
    response.set_cookie(
        key="theme_preference",
        value=preference.theme,
        max_age=31536000,  # 1 year
        httponly=False,  # Allow JS access
        samesite="lax"
    )

    return {
        "success": True,
        "theme": preference.theme,
        "message": f"Tema alterado para: {preference.theme}"
    }


def get_dark_mode_css() -> str:
    """
    Retorna o CSS completo para o Dark Mode.
    Inclui CSS variables e estilos de transicao.
    """
    return '''
    /* =================================================================
       DARK MODE CSS VARIABLES (Issue #217)
       ================================================================= */

    /* Light Mode (Default) */
    :root {
        /* Background colors */
        --bg-primary: #ffffff;
        --bg-secondary: #f3f4f6;
        --bg-tertiary: #e5e7eb;
        --bg-card: #ffffff;
        --bg-hover: #f9fafb;
        --bg-active: #f3f4f6;

        /* Text colors */
        --text-primary: #1f2937;
        --text-secondary: #6b7280;
        --text-tertiary: #9ca3af;
        --text-inverse: #ffffff;

        /* Border colors */
        --border-primary: #e5e7eb;
        --border-secondary: #d1d5db;
        --border-focus: #003B4A;

        /* Brand colors (Belgo) */
        --accent-primary: #003B4A;
        --accent-secondary: #FF6C00;
        --accent-primary-hover: #004d61;
        --accent-secondary-hover: #e65f00;

        /* Status colors */
        --color-success: #10B981;
        --color-success-bg: #D1FAE5;
        --color-warning: #F59E0B;
        --color-warning-bg: #FEF3C7;
        --color-error: #EF4444;
        --color-error-bg: #FEE2E2;
        --color-info: #3B82F6;
        --color-info-bg: #DBEAFE;

        /* Shadow */
        --shadow-sm: 0 1px 2px 0 rgb(0 0 0 / 0.05);
        --shadow-md: 0 4px 6px -1px rgb(0 0 0 / 0.1);
        --shadow-lg: 0 10px 15px -3px rgb(0 0 0 / 0.1);

        /* Transitions */
        --transition-colors: color 0.2s ease, background-color 0.2s ease, border-color 0.2s ease;
    }

    /* Dark Mode Variables */
    [data-theme="dark"],
    .dark,
    html.dark {
        /* Background colors */
        --bg-primary: #111827;
        --bg-secondary: #1f2937;
        --bg-tertiary: #374151;
        --bg-card: #1f2937;
        --bg-hover: #374151;
        --bg-active: #4b5563;

        /* Text colors */
        --text-primary: #f9fafb;
        --text-secondary: #d1d5db;
        --text-tertiary: #9ca3af;
        --text-inverse: #1f2937;

        /* Border colors */
        --border-primary: #374151;
        --border-secondary: #4b5563;
        --border-focus: #60a5fa;

        /* Brand colors adjusted for dark */
        --accent-primary: #60a5fa;
        --accent-secondary: #f97316;
        --accent-primary-hover: #93c5fd;
        --accent-secondary-hover: #fb923c;

        /* Status colors adjusted for dark */
        --color-success: #34D399;
        --color-success-bg: rgba(16, 185, 129, 0.2);
        --color-warning: #FBBF24;
        --color-warning-bg: rgba(245, 158, 11, 0.2);
        --color-error: #F87171;
        --color-error-bg: rgba(239, 68, 68, 0.2);
        --color-info: #60A5FA;
        --color-info-bg: rgba(59, 130, 246, 0.2);

        /* Shadow adjusted for dark */
        --shadow-sm: 0 1px 2px 0 rgb(0 0 0 / 0.3);
        --shadow-md: 0 4px 6px -1px rgb(0 0 0 / 0.4);
        --shadow-lg: 0 10px 15px -3px rgb(0 0 0 / 0.5);
    }

    /* =================================================================
       DARK MODE TOGGLE BUTTON STYLES
       ================================================================= */

    .dark-mode-toggle {
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        width: 40px;
        height: 40px;
        padding: 0;
        border: none;
        border-radius: 10px;
        background: rgba(255, 255, 255, 0.1);
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        overflow: hidden;
    }

    .dark-mode-toggle:hover {
        background: rgba(255, 255, 255, 0.2);
        transform: scale(1.05);
    }

    .dark-mode-toggle:active {
        transform: scale(0.95);
    }

    .dark-mode-toggle:focus-visible {
        outline: 2px solid #FF6C00;
        outline-offset: 2px;
    }

    /* Icon container with animation */
    .dark-mode-icon-wrapper {
        position: relative;
        width: 24px;
        height: 24px;
        display: flex;
        align-items: center;
        justify-content: center;
    }

    /* Sun icon (Light mode) */
    .sun-icon {
        position: absolute;
        width: 20px;
        height: 20px;
        color: #FCD34D;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        transform-origin: center;
    }

    .dark .sun-icon,
    [data-theme="dark"] .sun-icon {
        transform: rotate(90deg) scale(0);
        opacity: 0;
    }

    /* Moon icon (Dark mode) */
    .moon-icon {
        position: absolute;
        width: 18px;
        height: 18px;
        color: #93C5FD;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        transform-origin: center;
        transform: rotate(-90deg) scale(0);
        opacity: 0;
    }

    .dark .moon-icon,
    [data-theme="dark"] .moon-icon {
        transform: rotate(0) scale(1);
        opacity: 1;
    }

    /* Tooltip */
    .dark-mode-toggle[title]:hover::after {
        content: attr(title);
        position: absolute;
        bottom: -32px;
        left: 50%;
        transform: translateX(-50%);
        padding: 4px 8px;
        background: var(--bg-card);
        color: var(--text-primary);
        font-size: 11px;
        border-radius: 4px;
        white-space: nowrap;
        box-shadow: var(--shadow-md);
        z-index: 100;
        border: 1px solid var(--border-primary);
    }

    /* =================================================================
       GLOBAL TRANSITION FOR THEME CHANGE
       ================================================================= */

    /* Smooth transitions when theme changes */
    *, *::before, *::after {
        transition: background-color 0.3s ease,
                    border-color 0.3s ease,
                    box-shadow 0.3s ease;
    }

    /* Disable transitions for specific elements that shouldn't animate */
    .no-theme-transition,
    .no-theme-transition * {
        transition: none !important;
    }

    /* =================================================================
       APPLY CSS VARIABLES TO COMMON ELEMENTS
       ================================================================= */

    /* Body background */
    body {
        background-color: var(--bg-secondary);
        color: var(--text-primary);
    }

    /* Cards and panels */
    .card, .panel, .bg-white {
        background-color: var(--bg-card);
        border-color: var(--border-primary);
    }

    /* Dark mode specific overrides */
    .dark body, html.dark body {
        background-color: var(--bg-primary) !important;
    }

    .dark .bg-white {
        background-color: var(--bg-card) !important;
    }

    .dark .bg-gray-100,
    .dark .bg-gray-50 {
        background-color: var(--bg-secondary) !important;
    }

    .dark .text-gray-900 {
        color: var(--text-primary) !important;
    }

    .dark .text-gray-700,
    .dark .text-gray-600,
    .dark .text-gray-500 {
        color: var(--text-secondary) !important;
    }

    .dark .border-gray-200,
    .dark .border-gray-300 {
        border-color: var(--border-primary) !important;
    }

    /* Forms in dark mode */
    .dark input,
    .dark textarea,
    .dark select {
        background-color: var(--bg-secondary) !important;
        border-color: var(--border-secondary) !important;
        color: var(--text-primary) !important;
    }

    .dark input::placeholder,
    .dark textarea::placeholder {
        color: var(--text-tertiary) !important;
    }

    .dark input:focus,
    .dark textarea:focus,
    .dark select:focus {
        border-color: var(--border-focus) !important;
        box-shadow: 0 0 0 3px rgba(96, 165, 250, 0.2) !important;
    }

    /* Story cards in dark mode */
    .dark .story-card {
        background-color: var(--bg-card) !important;
        border-color: var(--border-primary) !important;
    }

    .dark .story-card:hover {
        border-color: var(--accent-primary) !important;
    }

    /* Kanban columns */
    .dark .kanban-column {
        background-color: var(--bg-secondary) !important;
    }

    /* Modals */
    .dark .modal-content,
    .dark [class*="modal"] > div {
        background-color: var(--bg-card) !important;
        border-color: var(--border-primary) !important;
    }

    /* Dropdowns */
    .dark .dropdown-menu,
    .dark [class*="dropdown"] {
        background-color: var(--bg-card) !important;
        border-color: var(--border-primary) !important;
    }

    /* Tables */
    .dark table {
        background-color: var(--bg-card);
    }

    .dark th {
        background-color: var(--bg-secondary);
        color: var(--text-primary);
    }

    .dark td {
        border-color: var(--border-primary);
    }

    .dark tr:hover {
        background-color: var(--bg-hover);
    }

    /* Buttons */
    .dark .btn-secondary,
    .dark button.secondary {
        background-color: var(--bg-tertiary);
        border-color: var(--border-secondary);
        color: var(--text-primary);
    }

    /* Scrollbar for dark mode */
    .dark ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
    }

    .dark ::-webkit-scrollbar-track {
        background: var(--bg-secondary);
    }

    .dark ::-webkit-scrollbar-thumb {
        background: var(--bg-tertiary);
        border-radius: 4px;
    }

    .dark ::-webkit-scrollbar-thumb:hover {
        background: #4b5563;
    }
    '''


def get_dark_mode_html() -> str:
    """
    Retorna o HTML do toggle de Dark Mode para o header.
    Usa icones SVG para sol e lua com animacao.
    """
    return '''
    <!-- Dark Mode Toggle (Issue #217) -->
    <button @click="toggleDarkMode"
            class="dark-mode-toggle"
            :title="isDarkMode ? 'Mudar para Modo Claro' : 'Mudar para Modo Escuro'"
            :aria-label="isDarkMode ? 'Ativar modo claro' : 'Ativar modo escuro'"
            aria-live="polite">
        <div class="dark-mode-icon-wrapper">
            <!-- Sun Icon (visible in light mode) -->
            <svg class="sun-icon" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" clip-rule="evenodd"/>
            </svg>
            <!-- Moon Icon (visible in dark mode) -->
            <svg class="moon-icon" fill="currentColor" viewBox="0 0 20 20">
                <path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"/>
            </svg>
        </div>
    </button>
    '''


def get_dark_mode_js() -> str:
    """
    Retorna o codigo JavaScript para gerenciar o Dark Mode.
    Inclui deteccao de preferencia do sistema e persistencia.
    """
    return '''
    // ===================================================================
    // DARK MODE MANAGEMENT (Issue #217)
    // ===================================================================

    const isDarkMode = ref(false);
    const themePreference = ref('system'); // 'light', 'dark', or 'system'

    // Detect system preference
    const getSystemPreference = () => {
        if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
            return 'dark';
        }
        return 'light';
    };

    // Apply theme to document
    const applyTheme = (theme) => {
        const effectiveTheme = theme === 'system' ? getSystemPreference() : theme;
        isDarkMode.value = effectiveTheme === 'dark';

        // Apply to HTML element
        document.documentElement.classList.toggle('dark', isDarkMode.value);
        document.documentElement.setAttribute('data-theme', effectiveTheme);

        // Update meta theme-color for mobile browsers
        const metaTheme = document.querySelector('meta[name="theme-color"]');
        if (metaTheme) {
            metaTheme.setAttribute('content', isDarkMode.value ? '#111827' : '#003B4A');
        }
    };

    // Toggle dark mode
    const toggleDarkMode = async () => {
        // Cycle through: current -> opposite
        const newTheme = isDarkMode.value ? 'light' : 'dark';
        themePreference.value = newTheme;

        // Apply immediately
        applyTheme(newTheme);

        // Save to localStorage
        localStorage.setItem('themePreference', newTheme);

        // Save to server (optional)
        try {
            await fetch('/api/user/preferences/theme', {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ theme: newTheme })
            });
        } catch (e) {
            console.log('Theme preference saved locally only');
        }

        // Show toast notification
        addToast('info', isDarkMode.value ? 'Modo Escuro Ativado' : 'Modo Claro Ativado', 'Preferencia salva');
    };

    // Load dark mode preference on mount
    const loadDarkModePreference = () => {
        // Check localStorage first
        const savedPreference = localStorage.getItem('themePreference');

        if (savedPreference) {
            themePreference.value = savedPreference;
            applyTheme(savedPreference);
        } else {
            // Check cookie
            const cookiePreference = document.cookie
                .split('; ')
                .find(row => row.startsWith('theme_preference='))
                ?.split('=')[1];

            if (cookiePreference) {
                themePreference.value = cookiePreference;
                applyTheme(cookiePreference);
            } else {
                // Use system preference
                themePreference.value = 'system';
                applyTheme('system');
            }
        }
    };

    // Listen for system preference changes
    const setupSystemPreferenceListener = () => {
        if (window.matchMedia) {
            const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
            mediaQuery.addEventListener('change', (e) => {
                if (themePreference.value === 'system') {
                    applyTheme('system');
                }
            });
        }
    };

    // Keyboard shortcut for dark mode (D key when not in input)
    const handleDarkModeShortcut = (e) => {
        if (e.key === 'd' || e.key === 'D') {
            const activeElement = document.activeElement;
            const isInput = activeElement.tagName === 'INPUT' ||
                           activeElement.tagName === 'TEXTAREA' ||
                           activeElement.isContentEditable;

            if (!isInput && !e.ctrlKey && !e.metaKey && !e.altKey) {
                toggleDarkMode();
            }
        }
    };

    // Initialize on mount
    onMounted(() => {
        loadDarkModePreference();
        setupSystemPreferenceListener();
        document.addEventListener('keydown', handleDarkModeShortcut);
    });

    // Cleanup
    onUnmounted(() => {
        document.removeEventListener('keydown', handleDarkModeShortcut);
    });
    '''


def register_dark_mode(app):
    """
    Registra os endpoints do Dark Mode no app FastAPI.

    Endpoints:
    - GET /api/user/preferences/theme - Obter preferencia de tema
    - PUT /api/user/preferences/theme - Salvar preferencia de tema
    """
    app.include_router(router)
    print("[Dashboard] Dark Mode endpoints loaded: /api/user/preferences/theme")


# Export functions for integration with main dashboard
__all__ = [
    'router',
    'register_dark_mode',
    'get_dark_mode_css',
    'get_dark_mode_html',
    'get_dark_mode_js'
]
