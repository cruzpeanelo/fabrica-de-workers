# -*- coding: utf-8 -*-
"""
Theme Editor Module (Issue #277)
================================
Editor visual para criar temas customizados.

Funcionalidades:
- Color picker para cores principais
- Preview em tempo real
- Salvar temas customizados
- Compartilhar temas com time
- Importar/exportar tema
"""

from fastapi import APIRouter, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict
import json

router = APIRouter(prefix="/api/themes", tags=["Theme Editor"])

# Default themes
DEFAULT_THEMES = {
    "light": {
        "id": "light",
        "name": "Claro",
        "colors": {
            "primary": "#003B4A",
            "secondary": "#FF6C00",
            "background": "#F3F4F6",
            "surface": "#FFFFFF",
            "text": "#1F2937",
            "textSecondary": "#6B7280",
            "success": "#10B981",
            "warning": "#F59E0B",
            "error": "#EF4444",
            "info": "#3B82F6"
        },
        "is_default": True
    },
    "dark": {
        "id": "dark",
        "name": "Escuro",
        "colors": {
            "primary": "#60A5FA",
            "secondary": "#F97316",
            "background": "#111827",
            "surface": "#1F2937",
            "text": "#F9FAFB",
            "textSecondary": "#9CA3AF",
            "success": "#34D399",
            "warning": "#FBBF24",
            "error": "#F87171",
            "info": "#60A5FA"
        },
        "is_default": True
    },
    "high-contrast": {
        "id": "high-contrast",
        "name": "Alto Contraste",
        "colors": {
            "primary": "#FFFF00",
            "secondary": "#00FFFF",
            "background": "#000000",
            "surface": "#1A1A1A",
            "text": "#FFFFFF",
            "textSecondary": "#CCCCCC",
            "success": "#00FF00",
            "warning": "#FFFF00",
            "error": "#FF0000",
            "info": "#00FFFF"
        },
        "is_default": True
    }
}

# Custom themes storage (in production, use database)
custom_themes: Dict[str, dict] = {}


class ThemeColors(BaseModel):
    primary: str
    secondary: str
    background: str
    surface: str
    text: str
    textSecondary: str
    success: str
    warning: str
    error: str
    info: str


class ThemeCreate(BaseModel):
    name: str
    colors: ThemeColors
    base_theme: Optional[str] = "light"


class ThemeUpdate(BaseModel):
    name: Optional[str] = None
    colors: Optional[ThemeColors] = None


@router.get("/")
async def list_themes():
    """Lista todos os temas disponiveis."""
    all_themes = list(DEFAULT_THEMES.values()) + list(custom_themes.values())
    return {"themes": all_themes}


@router.get("/{theme_id}")
async def get_theme(theme_id: str):
    """Retorna um tema especifico."""
    if theme_id in DEFAULT_THEMES:
        return DEFAULT_THEMES[theme_id]
    if theme_id in custom_themes:
        return custom_themes[theme_id]
    raise HTTPException(status_code=404, detail="Tema nao encontrado")


@router.post("/")
async def create_theme(theme: ThemeCreate):
    """Cria um novo tema customizado."""
    import uuid
    theme_id = f"custom-{uuid.uuid4().hex[:8]}"

    new_theme = {
        "id": theme_id,
        "name": theme.name,
        "colors": theme.colors.dict(),
        "base_theme": theme.base_theme,
        "is_default": False,
        "created_by": "current_user"
    }

    custom_themes[theme_id] = new_theme

    return {
        "success": True,
        "theme": new_theme
    }


@router.put("/{theme_id}")
async def update_theme(theme_id: str, theme: ThemeUpdate):
    """Atualiza um tema customizado."""
    if theme_id in DEFAULT_THEMES:
        raise HTTPException(status_code=400, detail="Nao e possivel editar temas padrao")

    if theme_id not in custom_themes:
        raise HTTPException(status_code=404, detail="Tema nao encontrado")

    if theme.name:
        custom_themes[theme_id]["name"] = theme.name
    if theme.colors:
        custom_themes[theme_id]["colors"] = theme.colors.dict()

    return {
        "success": True,
        "theme": custom_themes[theme_id]
    }


@router.delete("/{theme_id}")
async def delete_theme(theme_id: str):
    """Exclui um tema customizado."""
    if theme_id in DEFAULT_THEMES:
        raise HTTPException(status_code=400, detail="Nao e possivel excluir temas padrao")

    if theme_id not in custom_themes:
        raise HTTPException(status_code=404, detail="Tema nao encontrado")

    del custom_themes[theme_id]

    return {"success": True, "message": "Tema excluido"}


@router.get("/{theme_id}/export")
async def export_theme(theme_id: str):
    """Exporta um tema como JSON."""
    theme = None
    if theme_id in DEFAULT_THEMES:
        theme = DEFAULT_THEMES[theme_id]
    elif theme_id in custom_themes:
        theme = custom_themes[theme_id]
    else:
        raise HTTPException(status_code=404, detail="Tema nao encontrado")

    return {
        "theme_json": json.dumps(theme, indent=2),
        "filename": f"theme_{theme_id}.json"
    }


@router.post("/import")
async def import_theme(theme_json: str):
    """Importa um tema de JSON."""
    try:
        theme_data = json.loads(theme_json)

        # Validate required fields
        if "name" not in theme_data or "colors" not in theme_data:
            raise HTTPException(status_code=400, detail="JSON invalido: faltam campos obrigatorios")

        import uuid
        theme_id = f"imported-{uuid.uuid4().hex[:8]}"

        new_theme = {
            "id": theme_id,
            "name": theme_data["name"],
            "colors": theme_data["colors"],
            "is_default": False,
            "imported": True
        }

        custom_themes[theme_id] = new_theme

        return {
            "success": True,
            "theme": new_theme
        }

    except json.JSONDecodeError:
        raise HTTPException(status_code=400, detail="JSON invalido")


@router.get("/{theme_id}/css")
async def get_theme_css(theme_id: str):
    """Gera CSS variables para um tema."""
    theme = None
    if theme_id in DEFAULT_THEMES:
        theme = DEFAULT_THEMES[theme_id]
    elif theme_id in custom_themes:
        theme = custom_themes[theme_id]
    else:
        raise HTTPException(status_code=404, detail="Tema nao encontrado")

    colors = theme["colors"]
    css = f'''
:root[data-theme="{theme_id}"] {{
    --color-primary: {colors["primary"]};
    --color-secondary: {colors["secondary"]};
    --color-background: {colors["background"]};
    --color-surface: {colors["surface"]};
    --color-text: {colors["text"]};
    --color-text-secondary: {colors["textSecondary"]};
    --color-success: {colors["success"]};
    --color-warning: {colors["warning"]};
    --color-error: {colors["error"]};
    --color-info: {colors["info"]};
}}
'''
    return {"css": css.strip()}


def get_theme_editor_html():
    """Retorna o HTML do editor de temas."""
    return '''
    <!-- Theme Editor Modal (Issue #277) -->
    <div v-if="showThemeEditor"
         class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
         @click.self="showThemeEditor = false">
        <div class="bg-white rounded-2xl shadow-2xl w-full max-w-3xl mx-4 max-h-[90vh] overflow-hidden flex flex-col">
            <!-- Header -->
            <div class="px-6 py-4 border-b border-gray-200 flex items-center justify-between">
                <h2 class="text-xl font-bold">Editor de Temas</h2>
                <button @click="showThemeEditor = false" class="text-gray-400 hover:text-gray-600">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>

            <div class="flex-1 overflow-y-auto p-6">
                <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <!-- Color Pickers -->
                    <div>
                        <h3 class="font-semibold mb-4">Cores do Tema</h3>
                        <div class="space-y-4">
                            <div v-for="(color, key) in themeEditor.colors" :key="key" class="flex items-center gap-3">
                                <input type="color"
                                       v-model="themeEditor.colors[key]"
                                       @input="previewTheme"
                                       class="w-10 h-10 rounded cursor-pointer border-2 border-gray-200">
                                <div class="flex-1">
                                    <label class="text-sm font-medium text-gray-700 capitalize">{{ formatColorName(key) }}</label>
                                    <input type="text"
                                           v-model="themeEditor.colors[key]"
                                           @input="previewTheme"
                                           class="block w-full text-xs text-gray-500 border border-gray-200 rounded px-2 py-1 mt-1">
                                </div>
                            </div>
                        </div>

                        <!-- Theme Name -->
                        <div class="mt-6">
                            <label class="block text-sm font-medium text-gray-700 mb-1">Nome do Tema</label>
                            <input type="text"
                                   v-model="themeEditor.name"
                                   placeholder="Meu Tema Custom"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2">
                        </div>

                        <!-- Actions -->
                        <div class="mt-6 flex gap-3">
                            <button @click="saveCustomTheme"
                                    class="flex-1 bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg font-medium">
                                Salvar Tema
                            </button>
                            <button @click="resetThemeEditor"
                                    class="px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50">
                                Resetar
                            </button>
                        </div>
                    </div>

                    <!-- Preview -->
                    <div>
                        <h3 class="font-semibold mb-4">Preview</h3>
                        <div class="rounded-lg overflow-hidden border border-gray-200"
                             :style="{backgroundColor: themeEditor.colors.background}">
                            <!-- Preview Header -->
                            <div class="px-4 py-3"
                                 :style="{backgroundColor: themeEditor.colors.primary, color: '#fff'}">
                                <span class="font-semibold">Header da Aplicacao</span>
                            </div>

                            <!-- Preview Content -->
                            <div class="p-4 space-y-3">
                                <!-- Card -->
                                <div class="rounded-lg p-3"
                                     :style="{backgroundColor: themeEditor.colors.surface}">
                                    <h4 class="font-medium" :style="{color: themeEditor.colors.text}">
                                        Titulo do Card
                                    </h4>
                                    <p class="text-sm" :style="{color: themeEditor.colors.textSecondary}">
                                        Texto secundario de exemplo
                                    </p>
                                </div>

                                <!-- Buttons -->
                                <div class="flex gap-2">
                                    <button class="px-3 py-1 rounded text-sm text-white"
                                            :style="{backgroundColor: themeEditor.colors.primary}">
                                        Primario
                                    </button>
                                    <button class="px-3 py-1 rounded text-sm text-white"
                                            :style="{backgroundColor: themeEditor.colors.secondary}">
                                        Secundario
                                    </button>
                                </div>

                                <!-- Status badges -->
                                <div class="flex gap-2">
                                    <span class="px-2 py-1 rounded text-xs text-white"
                                          :style="{backgroundColor: themeEditor.colors.success}">Success</span>
                                    <span class="px-2 py-1 rounded text-xs text-white"
                                          :style="{backgroundColor: themeEditor.colors.warning}">Warning</span>
                                    <span class="px-2 py-1 rounded text-xs text-white"
                                          :style="{backgroundColor: themeEditor.colors.error}">Error</span>
                                    <span class="px-2 py-1 rounded text-xs text-white"
                                          :style="{backgroundColor: themeEditor.colors.info}">Info</span>
                                </div>
                            </div>
                        </div>

                        <!-- Saved Themes -->
                        <div class="mt-6">
                            <h3 class="font-semibold mb-3">Temas Salvos</h3>
                            <div class="space-y-2">
                                <div v-for="theme in savedThemes" :key="theme.id"
                                     class="flex items-center justify-between p-2 bg-gray-50 rounded-lg">
                                    <div class="flex items-center gap-2">
                                        <div class="w-6 h-6 rounded-full"
                                             :style="{backgroundColor: theme.colors.primary}"></div>
                                        <span class="font-medium text-sm">{{ theme.name }}</span>
                                    </div>
                                    <div class="flex gap-1">
                                        <button @click="applyTheme(theme)"
                                                class="text-xs px-2 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200">
                                            Aplicar
                                        </button>
                                        <button @click="loadThemeToEditor(theme)"
                                                class="text-xs px-2 py-1 bg-gray-100 text-gray-700 rounded hover:bg-gray-200">
                                            Editar
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class="px-6 py-3 bg-gray-50 border-t border-gray-200 flex justify-between">
                <button @click="exportTheme"
                        class="text-sm text-gray-600 hover:text-gray-800">
                    Exportar JSON
                </button>
                <button @click="importThemePrompt"
                        class="text-sm text-blue-600 hover:text-blue-800">
                    Importar Tema
                </button>
            </div>
        </div>
    </div>
    '''


def get_theme_editor_js():
    """Retorna o codigo JavaScript para o editor de temas."""
    return '''
    // Theme Editor State (Issue #277)
    const showThemeEditor = ref(false);
    const themeEditor = ref({
        name: 'Meu Tema',
        colors: {
            primary: '#003B4A',
            secondary: '#FF6C00',
            background: '#F3F4F6',
            surface: '#FFFFFF',
            text: '#1F2937',
            textSecondary: '#6B7280',
            success: '#10B981',
            warning: '#F59E0B',
            error: '#EF4444',
            info: '#3B82F6'
        }
    });
    const savedThemes = ref([]);

    const loadThemes = async () => {
        try {
            const response = await fetch('/api/themes/');
            if (response.ok) {
                const data = await response.json();
                savedThemes.value = data.themes;
            }
        } catch (e) {
            console.error('Failed to load themes:', e);
        }
    };

    const previewTheme = () => {
        // Apply preview colors to CSS variables
        const root = document.documentElement;
        Object.entries(themeEditor.value.colors).forEach(([key, value]) => {
            root.style.setProperty('--preview-' + key, value);
        });
    };

    const saveCustomTheme = async () => {
        try {
            const response = await fetch('/api/themes/', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    name: themeEditor.value.name,
                    colors: themeEditor.value.colors
                })
            });

            if (response.ok) {
                addToast('success', 'Tema Salvo', 'Seu tema foi salvo com sucesso');
                loadThemes();
            }
        } catch (e) {
            addToast('error', 'Erro', 'Falha ao salvar tema');
        }
    };

    const applyTheme = (theme) => {
        const root = document.documentElement;
        Object.entries(theme.colors).forEach(([key, value]) => {
            root.style.setProperty('--color-' + key, value);
        });
        localStorage.setItem('selectedTheme', JSON.stringify(theme));
        addToast('success', 'Tema Aplicado', theme.name);
    };

    const loadThemeToEditor = (theme) => {
        themeEditor.value.name = theme.name;
        themeEditor.value.colors = { ...theme.colors };
    };

    const resetThemeEditor = () => {
        themeEditor.value = {
            name: 'Meu Tema',
            colors: {
                primary: '#003B4A',
                secondary: '#FF6C00',
                background: '#F3F4F6',
                surface: '#FFFFFF',
                text: '#1F2937',
                textSecondary: '#6B7280',
                success: '#10B981',
                warning: '#F59E0B',
                error: '#EF4444',
                info: '#3B82F6'
            }
        };
    };

    const formatColorName = (key) => {
        return key.replace(/([A-Z])/g, ' $1').trim();
    };

    const exportTheme = () => {
        const json = JSON.stringify(themeEditor.value, null, 2);
        const blob = new Blob([json], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'theme.json';
        a.click();
    };

    const importThemePrompt = () => {
        const json = prompt('Cole o JSON do tema:');
        if (json) {
            try {
                const theme = JSON.parse(json);
                themeEditor.value = theme;
                addToast('success', 'Tema Importado', 'Tema carregado no editor');
            } catch (e) {
                addToast('error', 'Erro', 'JSON invalido');
            }
        }
    };

    // Load themes on mount
    onMounted(() => {
        loadThemes();
    });
    '''


def register_theme_editor(app):
    """Registra os endpoints do editor de temas no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Theme Editor endpoints loaded: /api/themes/*")
