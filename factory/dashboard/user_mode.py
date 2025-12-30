# -*- coding: utf-8 -*-
"""
User Mode Manager - Issue #204
==============================
Gerenciamento de modo Basico/Avancado para usuarios nao tecnicos.
Integra com o glossario para traduzir termos automaticamente.
"""

from typing import Optional, Dict, Any
from fastapi import APIRouter, Cookie, Response
from fastapi.responses import JSONResponse
from pydantic import BaseModel
import json

# =============================================================================
# ROUTER
# =============================================================================

user_mode_router = APIRouter(prefix="/api/user-mode", tags=["User Mode"])


# =============================================================================
# MODELS
# =============================================================================

class UserModePreference(BaseModel):
    mode: str = "basic"  # basic or advanced
    show_tooltips: bool = True
    show_technical_terms: bool = False
    compact_view: bool = False
    language: str = "pt-BR"


# =============================================================================
# DEFAULT PREFERENCES
# =============================================================================

DEFAULT_PREFERENCES = {
    "mode": "basic",
    "show_tooltips": True,
    "show_technical_terms": False,
    "compact_view": False,
    "language": "pt-BR",
    "completed_tour": False,
    "hidden_features": [
        "api_keys",
        "webhooks",
        "advanced_metrics",
        "raw_logs",
        "debug_panel"
    ]
}


# =============================================================================
# MODE CONFIGURATIONS
# =============================================================================

MODE_CONFIG = {
    "basic": {
        "name": "Basico",
        "description": "Interface simplificada para usuarios de negocio",
        "icon": "user",
        "features": {
            "show_kanban": True,
            "show_stories": True,
            "show_progress": True,
            "show_chat": True,
            "show_docs": True,
            "show_metrics": False,
            "show_api_panel": False,
            "show_logs": False,
            "show_debug": False,
            "show_webhooks": False,
            "show_integrations": False,
            "terminology": "simplified"
        },
        "labels": {
            "stories": "Requisitos",
            "tasks": "Atividades",
            "sprint": "Ciclo de Trabalho",
            "backlog": "Lista de Espera",
            "kanban": "Quadro de Tarefas",
            "deploy": "Publicar",
            "worker": "Aplicacao Automatica",
            "job": "Execucao",
            "bug": "Problema",
            "feature": "Funcionalidade"
        }
    },
    "advanced": {
        "name": "Avancado",
        "description": "Interface completa para desenvolvedores",
        "icon": "code",
        "features": {
            "show_kanban": True,
            "show_stories": True,
            "show_progress": True,
            "show_chat": True,
            "show_docs": True,
            "show_metrics": True,
            "show_api_panel": True,
            "show_logs": True,
            "show_debug": True,
            "show_webhooks": True,
            "show_integrations": True,
            "terminology": "technical"
        },
        "labels": {
            "stories": "User Stories",
            "tasks": "Tasks",
            "sprint": "Sprint",
            "backlog": "Backlog",
            "kanban": "Kanban Board",
            "deploy": "Deploy",
            "worker": "Worker",
            "job": "Job",
            "bug": "Bug",
            "feature": "Feature"
        }
    }
}


# =============================================================================
# IN-MEMORY STORAGE (use database in production)
# =============================================================================

user_preferences_store: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_user_preferences(user_id: str) -> Dict[str, Any]:
    """Retorna preferencias do usuario ou defaults"""
    if user_id not in user_preferences_store:
        user_preferences_store[user_id] = DEFAULT_PREFERENCES.copy()
    return user_preferences_store[user_id]


def get_mode_config(mode: str) -> Dict[str, Any]:
    """Retorna configuracao do modo"""
    return MODE_CONFIG.get(mode, MODE_CONFIG["basic"])


def get_label(term: str, mode: str = "basic") -> str:
    """Retorna o label traduzido para o termo"""
    config = get_mode_config(mode)
    return config["labels"].get(term, term)


def should_show_feature(feature: str, mode: str = "basic") -> bool:
    """Verifica se uma feature deve ser exibida no modo atual"""
    config = get_mode_config(mode)
    return config["features"].get(feature, False)


def translate_term(term: str, mode: str = "basic") -> str:
    """Traduz um termo baseado no modo do usuario"""
    config = get_mode_config(mode)
    return config["labels"].get(term.lower(), term)


# =============================================================================
# API ENDPOINTS
# =============================================================================

@user_mode_router.get("/preferences")
async def get_preferences(user_id: str = "default"):
    """Retorna preferencias do usuario"""
    prefs = get_user_preferences(user_id)
    mode_config = get_mode_config(prefs.get("mode", "basic"))

    return {
        "preferences": prefs,
        "mode_config": mode_config,
        "available_modes": list(MODE_CONFIG.keys())
    }


@user_mode_router.post("/preferences")
async def update_preferences(
    user_id: str = "default",
    mode: Optional[str] = None,
    show_tooltips: Optional[bool] = None,
    show_technical_terms: Optional[bool] = None,
    compact_view: Optional[bool] = None,
    language: Optional[str] = None
):
    """Atualiza preferencias do usuario"""
    prefs = get_user_preferences(user_id)

    if mode is not None and mode in MODE_CONFIG:
        prefs["mode"] = mode
    if show_tooltips is not None:
        prefs["show_tooltips"] = show_tooltips
    if show_technical_terms is not None:
        prefs["show_technical_terms"] = show_technical_terms
    if compact_view is not None:
        prefs["compact_view"] = compact_view
    if language is not None:
        prefs["language"] = language

    user_preferences_store[user_id] = prefs

    return {
        "success": True,
        "preferences": prefs
    }


@user_mode_router.post("/toggle-mode")
async def toggle_mode(user_id: str = "default"):
    """Alterna entre modo basico e avancado"""
    prefs = get_user_preferences(user_id)
    current = prefs.get("mode", "basic")
    new_mode = "advanced" if current == "basic" else "basic"
    prefs["mode"] = new_mode
    user_preferences_store[user_id] = prefs

    return {
        "success": True,
        "previous_mode": current,
        "new_mode": new_mode,
        "mode_config": get_mode_config(new_mode)
    }


@user_mode_router.get("/labels")
async def get_all_labels(user_id: str = "default"):
    """Retorna todos os labels traduzidos para o modo do usuario"""
    prefs = get_user_preferences(user_id)
    mode = prefs.get("mode", "basic")
    config = get_mode_config(mode)

    return {
        "mode": mode,
        "labels": config["labels"]
    }


@user_mode_router.get("/translate/{term}")
async def translate_single_term(term: str, user_id: str = "default"):
    """Traduz um termo para o modo do usuario"""
    prefs = get_user_preferences(user_id)
    mode = prefs.get("mode", "basic")

    return {
        "original": term,
        "translated": translate_term(term, mode),
        "mode": mode
    }


@user_mode_router.get("/features")
async def get_visible_features(user_id: str = "default"):
    """Retorna lista de features visiveis para o usuario"""
    prefs = get_user_preferences(user_id)
    mode = prefs.get("mode", "basic")
    config = get_mode_config(mode)

    visible = [k for k, v in config["features"].items() if v is True]
    hidden = [k for k, v in config["features"].items() if v is False]

    return {
        "mode": mode,
        "visible_features": visible,
        "hidden_features": hidden
    }


@user_mode_router.post("/complete-tour")
async def complete_tour(user_id: str = "default"):
    """Marca o tour como completado"""
    prefs = get_user_preferences(user_id)
    prefs["completed_tour"] = True
    user_preferences_store[user_id] = prefs

    return {"success": True, "completed_tour": True}


@user_mode_router.get("/tour-status")
async def get_tour_status(user_id: str = "default"):
    """Verifica se usuario completou o tour"""
    prefs = get_user_preferences(user_id)
    return {
        "completed_tour": prefs.get("completed_tour", False),
        "should_show_tour": not prefs.get("completed_tour", False)
    }


# =============================================================================
# REGISTRATION
# =============================================================================

def register_user_mode_endpoints(app):
    """Registra endpoints de user mode no app FastAPI"""
    app.include_router(user_mode_router)
    print("[Dashboard] User Mode endpoints registered")


# =============================================================================
# JAVASCRIPT INTEGRATION SNIPPET
# =============================================================================

USER_MODE_JS = '''
// User Mode Manager - Frontend Integration
const UserMode = {
    preferences: null,

    async init() {
        await this.loadPreferences();
        this.applyMode();
        this.setupToggle();
    },

    async loadPreferences() {
        try {
            const response = await fetch('/api/user-mode/preferences?user_id=default');
            const data = await response.json();
            this.preferences = data.preferences;
            localStorage.setItem('user_mode', JSON.stringify(this.preferences));
        } catch (e) {
            // Fallback to localStorage
            const saved = localStorage.getItem('user_mode');
            this.preferences = saved ? JSON.parse(saved) : {mode: 'basic'};
        }
    },

    async toggleMode() {
        try {
            const response = await fetch('/api/user-mode/toggle-mode?user_id=default', {
                method: 'POST'
            });
            const data = await response.json();
            this.preferences.mode = data.new_mode;
            localStorage.setItem('user_mode', JSON.stringify(this.preferences));
            this.applyMode();
            return data;
        } catch (e) {
            console.error('Failed to toggle mode:', e);
        }
    },

    applyMode() {
        const mode = this.preferences?.mode || 'basic';
        document.body.classList.remove('mode-basic', 'mode-advanced');
        document.body.classList.add(`mode-${mode}`);

        // Hide/show elements based on mode
        document.querySelectorAll('[data-mode]').forEach(el => {
            const requiredMode = el.dataset.mode;
            el.style.display = (requiredMode === mode || requiredMode === 'all') ? '' : 'none';
        });

        // Update labels
        document.querySelectorAll('[data-term]').forEach(el => {
            const term = el.dataset.term;
            el.textContent = this.getLabel(term);
        });

        // Dispatch event
        window.dispatchEvent(new CustomEvent('user-mode-changed', {
            detail: { mode, preferences: this.preferences }
        }));
    },

    getLabel(term) {
        const labels = {
            basic: {
                stories: 'Requisitos',
                tasks: 'Atividades',
                sprint: 'Ciclo de Trabalho',
                backlog: 'Lista de Espera',
                kanban: 'Quadro de Tarefas',
                deploy: 'Publicar',
                worker: 'Aplicacao',
                job: 'Execucao',
                bug: 'Problema',
                feature: 'Funcionalidade'
            },
            advanced: {
                stories: 'User Stories',
                tasks: 'Tasks',
                sprint: 'Sprint',
                backlog: 'Backlog',
                kanban: 'Kanban Board',
                deploy: 'Deploy',
                worker: 'Worker',
                job: 'Job',
                bug: 'Bug',
                feature: 'Feature'
            }
        };
        const mode = this.preferences?.mode || 'basic';
        return labels[mode]?.[term] || term;
    },

    setupToggle() {
        const toggle = document.getElementById('mode-toggle');
        if (toggle) {
            toggle.addEventListener('click', () => this.toggleMode());
        }
    },

    isBasicMode() {
        return this.preferences?.mode === 'basic';
    },

    isAdvancedMode() {
        return this.preferences?.mode === 'advanced';
    }
};

// Auto-initialize
document.addEventListener('DOMContentLoaded', () => UserMode.init());
'''
