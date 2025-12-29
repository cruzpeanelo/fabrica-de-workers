/**
 * Design Tokens JavaScript - Fabrica de Agentes v6.5
 * ===================================================
 *
 * Sistema de gerenciamento dinamico de Design Tokens.
 * Inclui: alternancia de temas, exportacao de tokens, customizacao.
 *
 * Uso:
 * 1. Incluir no HTML: <script src="/static/design-tokens.js"></script>
 * 2. Usar API: DesignTokens.setTheme('dark')
 *
 * Issue #79 - Sistema de Design Tokens Completo
 */

(function(global) {
    'use strict';

    // =============================================================================
    // DESIGN TOKENS MANAGER
    // =============================================================================

    const DesignTokens = {
        // -----------------------------------------------------------------
        // CONFIGURACAO
        // -----------------------------------------------------------------
        config: {
            storageKey: 'fabrica-theme',
            defaultTheme: 'light',
            themes: ['light', 'dark', 'high-contrast'],
            transitionDuration: 200
        },

        // -----------------------------------------------------------------
        // TOKENS EXPORTADOS (JavaScript Object)
        // -----------------------------------------------------------------
        tokens: {
            colors: {
                primary: {
                    50: '#E6F0F2',
                    100: '#CCE0E5',
                    200: '#99C2CB',
                    300: '#66A3B1',
                    400: '#338597',
                    500: '#003B4A',
                    600: '#00526A',
                    700: '#003D50',
                    800: '#002835',
                    900: '#00141B'
                },
                secondary: {
                    50: '#FFF0E6',
                    100: '#FFE0CC',
                    200: '#FFC299',
                    300: '#FFA366',
                    400: '#FF8533',
                    500: '#FF6C00',
                    600: '#E65C00',
                    700: '#CC5200',
                    800: '#B34700',
                    900: '#993D00'
                },
                neutral: {
                    0: '#FFFFFF',
                    50: '#F9FAFB',
                    100: '#F3F4F6',
                    200: '#E5E7EB',
                    300: '#D1D5DB',
                    400: '#9CA3AF',
                    500: '#6B7280',
                    600: '#4B5563',
                    700: '#374151',
                    800: '#1F2937',
                    900: '#111827',
                    950: '#030712'
                },
                success: {
                    50: '#ECFDF5',
                    100: '#D1FAE5',
                    200: '#A7F3D0',
                    300: '#6EE7B7',
                    400: '#34D399',
                    500: '#10B981',
                    600: '#059669',
                    700: '#047857',
                    800: '#065F46',
                    900: '#064E3B'
                },
                warning: {
                    50: '#FFFBEB',
                    100: '#FEF3C7',
                    200: '#FDE68A',
                    300: '#FCD34D',
                    400: '#FBBF24',
                    500: '#F59E0B',
                    600: '#D97706',
                    700: '#B45309',
                    800: '#92400E',
                    900: '#78350F'
                },
                error: {
                    50: '#FEF2F2',
                    100: '#FEE2E2',
                    200: '#FECACA',
                    300: '#FCA5A5',
                    400: '#F87171',
                    500: '#EF4444',
                    600: '#DC2626',
                    700: '#B91C1C',
                    800: '#991B1B',
                    900: '#7F1D1D'
                },
                info: {
                    50: '#EFF6FF',
                    100: '#DBEAFE',
                    200: '#BFDBFE',
                    300: '#93C5FD',
                    400: '#60A5FA',
                    500: '#3B82F6',
                    600: '#2563EB',
                    700: '#1D4ED8',
                    800: '#1E40AF',
                    900: '#1E3A8A'
                },
                kanban: {
                    backlog: '#9CA3AF',
                    ready: '#3B82F6',
                    inProgress: '#F59E0B',
                    review: '#8B5CF6',
                    testing: '#06B6D4',
                    done: '#10B981'
                }
            },
            spacing: {
                0: '0',
                px: '1px',
                0.5: '0.125rem',
                1: '0.25rem',
                1.5: '0.375rem',
                2: '0.5rem',
                2.5: '0.625rem',
                3: '0.75rem',
                3.5: '0.875rem',
                4: '1rem',
                5: '1.25rem',
                6: '1.5rem',
                7: '1.75rem',
                8: '2rem',
                9: '2.25rem',
                10: '2.5rem',
                11: '2.75rem',
                12: '3rem',
                14: '3.5rem',
                16: '4rem',
                20: '5rem',
                24: '6rem',
                28: '7rem',
                32: '8rem'
            },
            typography: {
                fontFamily: {
                    sans: "'Inter', 'Segoe UI', -apple-system, BlinkMacSystemFont, 'Roboto', sans-serif",
                    serif: "'Georgia', 'Cambria', 'Times New Roman', serif",
                    mono: "'JetBrains Mono', 'Fira Code', 'Consolas', monospace"
                },
                fontSize: {
                    '2xs': '0.625rem',
                    xs: '0.75rem',
                    sm: '0.875rem',
                    base: '1rem',
                    lg: '1.125rem',
                    xl: '1.25rem',
                    '2xl': '1.5rem',
                    '3xl': '1.875rem',
                    '4xl': '2.25rem',
                    '5xl': '3rem'
                },
                fontWeight: {
                    thin: 100,
                    light: 300,
                    normal: 400,
                    medium: 500,
                    semibold: 600,
                    bold: 700,
                    extrabold: 800
                },
                lineHeight: {
                    none: 1,
                    tight: 1.25,
                    snug: 1.375,
                    normal: 1.5,
                    relaxed: 1.625,
                    loose: 2
                }
            },
            shadows: {
                none: 'none',
                xs: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
                sm: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px -1px rgba(0, 0, 0, 0.1)',
                md: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -2px rgba(0, 0, 0, 0.1)',
                lg: '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -4px rgba(0, 0, 0, 0.1)',
                xl: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 8px 10px -6px rgba(0, 0, 0, 0.1)',
                '2xl': '0 25px 50px -12px rgba(0, 0, 0, 0.25)',
                inner: 'inset 0 2px 4px 0 rgba(0, 0, 0, 0.05)'
            },
            borders: {
                radius: {
                    none: '0',
                    sm: '0.125rem',
                    default: '0.25rem',
                    md: '0.375rem',
                    lg: '0.5rem',
                    xl: '0.75rem',
                    '2xl': '1rem',
                    '3xl': '1.5rem',
                    full: '9999px'
                },
                width: {
                    0: '0',
                    1: '1px',
                    2: '2px',
                    4: '4px',
                    8: '8px'
                }
            },
            transitions: {
                duration: {
                    0: '0ms',
                    75: '75ms',
                    100: '100ms',
                    150: '150ms',
                    200: '200ms',
                    300: '300ms',
                    500: '500ms',
                    700: '700ms',
                    1000: '1000ms'
                },
                easing: {
                    linear: 'linear',
                    in: 'cubic-bezier(0.4, 0, 1, 1)',
                    out: 'cubic-bezier(0, 0, 0.2, 1)',
                    inOut: 'cubic-bezier(0.4, 0, 0.2, 1)',
                    bounce: 'cubic-bezier(0.68, -0.55, 0.265, 1.55)'
                }
            },
            breakpoints: {
                xs: '320px',
                sm: '640px',
                md: '768px',
                lg: '1024px',
                xl: '1280px',
                '2xl': '1536px'
            },
            sizes: {
                headerHeight: '64px',
                sidebarWidth: '256px',
                sidebarCollapsed: '64px',
                footerHeight: '48px',
                buttonSm: '32px',
                buttonMd: '40px',
                buttonLg: '48px',
                inputHeight: '40px'
            }
        },

        // -----------------------------------------------------------------
        // ESTADO ATUAL
        // -----------------------------------------------------------------
        _currentTheme: null,
        _observers: [],

        // -----------------------------------------------------------------
        // INICIALIZACAO
        // -----------------------------------------------------------------

        /**
         * Inicializa o sistema de Design Tokens
         * Carrega tema salvo ou detecta preferencia do sistema
         */
        init: function() {
            // Carregar tema salvo ou detectar preferencia
            const savedTheme = this.getSavedTheme();
            const systemTheme = this.getSystemTheme();
            const theme = savedTheme || systemTheme || this.config.defaultTheme;

            // Aplicar tema inicial
            this.setTheme(theme, false);

            // Observar mudancas na preferencia do sistema
            this.observeSystemTheme();

            // Log de inicializacao
            console.log('[DesignTokens] Initialized with theme:', this._currentTheme);

            return this;
        },

        // -----------------------------------------------------------------
        // GESTAO DE TEMAS
        // -----------------------------------------------------------------

        /**
         * Define o tema atual
         * @param {string} theme - Nome do tema ('light', 'dark', 'high-contrast')
         * @param {boolean} save - Se deve salvar no localStorage (default: true)
         */
        setTheme: function(theme, save = true) {
            if (!this.config.themes.includes(theme)) {
                console.warn('[DesignTokens] Invalid theme:', theme);
                return this;
            }

            // Adicionar classe de transicao
            document.documentElement.classList.add('theme-transitioning');

            // Atualizar atributo de tema
            document.documentElement.setAttribute('data-theme', theme);

            // Remover classes de tema antigas
            this.config.themes.forEach(t => {
                document.documentElement.classList.remove(`theme-${t}`);
            });

            // Adicionar classe do novo tema
            document.documentElement.classList.add(`theme-${theme}`);

            // Atualizar meta tag de cor do tema (para mobile)
            this.updateThemeColor(theme);

            // Salvar preferencia
            if (save) {
                this.saveTheme(theme);
            }

            // Atualizar estado
            this._currentTheme = theme;

            // Notificar observers
            this.notifyObservers(theme);

            // Remover classe de transicao apos animacao
            setTimeout(() => {
                document.documentElement.classList.remove('theme-transitioning');
            }, this.config.transitionDuration);

            return this;
        },

        /**
         * Obtem o tema atual
         * @returns {string} Nome do tema atual
         */
        getTheme: function() {
            return this._currentTheme ||
                   document.documentElement.getAttribute('data-theme') ||
                   this.config.defaultTheme;
        },

        /**
         * Alterna entre tema claro e escuro
         */
        toggleTheme: function() {
            const current = this.getTheme();
            const newTheme = current === 'dark' ? 'light' : 'dark';
            this.setTheme(newTheme);
            return newTheme;
        },

        /**
         * Detecta preferencia de tema do sistema
         * @returns {string|null} 'dark' ou 'light' ou null
         */
        getSystemTheme: function() {
            if (window.matchMedia) {
                if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
                    return 'dark';
                }
                if (window.matchMedia('(prefers-color-scheme: light)').matches) {
                    return 'light';
                }
                if (window.matchMedia('(prefers-contrast: more)').matches) {
                    return 'high-contrast';
                }
            }
            return null;
        },

        /**
         * Observa mudancas na preferencia de tema do sistema
         */
        observeSystemTheme: function() {
            if (!window.matchMedia) return;

            const darkQuery = window.matchMedia('(prefers-color-scheme: dark)');
            const contrastQuery = window.matchMedia('(prefers-contrast: more)');

            const handleChange = () => {
                // So aplicar se nao houver tema salvo pelo usuario
                if (!this.getSavedTheme()) {
                    const systemTheme = this.getSystemTheme();
                    if (systemTheme) {
                        this.setTheme(systemTheme, false);
                    }
                }
            };

            // Compatibilidade com navegadores antigos
            if (darkQuery.addEventListener) {
                darkQuery.addEventListener('change', handleChange);
                contrastQuery.addEventListener('change', handleChange);
            } else if (darkQuery.addListener) {
                darkQuery.addListener(handleChange);
                contrastQuery.addListener(handleChange);
            }
        },

        /**
         * Atualiza a meta tag theme-color
         * @param {string} theme - Nome do tema
         */
        updateThemeColor: function(theme) {
            let metaThemeColor = document.querySelector('meta[name="theme-color"]');

            if (!metaThemeColor) {
                metaThemeColor = document.createElement('meta');
                metaThemeColor.name = 'theme-color';
                document.head.appendChild(metaThemeColor);
            }

            const colors = {
                light: '#003B4A',
                dark: '#1F2937',
                'high-contrast': '#000000'
            };

            metaThemeColor.content = colors[theme] || colors.light;
        },

        // -----------------------------------------------------------------
        // PERSISTENCIA
        // -----------------------------------------------------------------

        /**
         * Salva tema no localStorage
         * @param {string} theme - Nome do tema
         */
        saveTheme: function(theme) {
            try {
                localStorage.setItem(this.config.storageKey, theme);
            } catch (e) {
                console.warn('[DesignTokens] Could not save theme to localStorage:', e);
            }
        },

        /**
         * Obtem tema salvo do localStorage
         * @returns {string|null} Nome do tema salvo
         */
        getSavedTheme: function() {
            try {
                return localStorage.getItem(this.config.storageKey);
            } catch (e) {
                return null;
            }
        },

        /**
         * Remove tema salvo do localStorage
         */
        clearSavedTheme: function() {
            try {
                localStorage.removeItem(this.config.storageKey);
            } catch (e) {
                // Ignore
            }
        },

        // -----------------------------------------------------------------
        // OBSERVERS (Padrao Observer)
        // -----------------------------------------------------------------

        /**
         * Registra um observer para mudancas de tema
         * @param {function} callback - Funcao chamada quando tema muda
         * @returns {function} Funcao para remover o observer
         */
        onThemeChange: function(callback) {
            if (typeof callback !== 'function') {
                console.warn('[DesignTokens] Observer must be a function');
                return () => {};
            }

            this._observers.push(callback);

            // Retorna funcao para remover observer
            return () => {
                const index = this._observers.indexOf(callback);
                if (index > -1) {
                    this._observers.splice(index, 1);
                }
            };
        },

        /**
         * Notifica todos os observers sobre mudanca de tema
         * @param {string} theme - Novo tema
         */
        notifyObservers: function(theme) {
            this._observers.forEach(callback => {
                try {
                    callback(theme);
                } catch (e) {
                    console.error('[DesignTokens] Observer error:', e);
                }
            });
        },

        // -----------------------------------------------------------------
        // ACESSO A TOKENS
        // -----------------------------------------------------------------

        /**
         * Obtem um token pelo caminho
         * @param {string} path - Caminho do token (ex: 'colors.primary.500')
         * @returns {*} Valor do token
         */
        getToken: function(path) {
            const parts = path.split('.');
            let value = this.tokens;

            for (const part of parts) {
                if (value && typeof value === 'object' && part in value) {
                    value = value[part];
                } else {
                    console.warn('[DesignTokens] Token not found:', path);
                    return undefined;
                }
            }

            return value;
        },

        /**
         * Obtem valor de variavel CSS
         * @param {string} varName - Nome da variavel CSS (ex: '--dt-color-primary-500')
         * @returns {string} Valor computado da variavel
         */
        getCSSVar: function(varName) {
            return getComputedStyle(document.documentElement)
                .getPropertyValue(varName)
                .trim();
        },

        /**
         * Define valor de variavel CSS
         * @param {string} varName - Nome da variavel CSS
         * @param {string} value - Novo valor
         */
        setCSSVar: function(varName, value) {
            document.documentElement.style.setProperty(varName, value);
        },

        // -----------------------------------------------------------------
        // CUSTOMIZACAO DE TEMA
        // -----------------------------------------------------------------

        /**
         * Aplica customizacoes de cores
         * @param {object} customColors - Objeto com cores customizadas
         */
        applyCustomColors: function(customColors) {
            if (!customColors || typeof customColors !== 'object') return;

            Object.entries(customColors).forEach(([key, value]) => {
                if (typeof value === 'string') {
                    this.setCSSVar(`--dt-color-${key}`, value);
                } else if (typeof value === 'object') {
                    Object.entries(value).forEach(([shade, color]) => {
                        this.setCSSVar(`--dt-color-${key}-${shade}`, color);
                    });
                }
            });
        },

        /**
         * Reseta todas as customizacoes
         */
        resetCustomizations: function() {
            // Remove todos os estilos inline
            document.documentElement.removeAttribute('style');

            // Reaplica o tema atual
            const currentTheme = this.getTheme();
            this.setTheme(currentTheme);
        },

        // -----------------------------------------------------------------
        // EXPORTACAO
        // -----------------------------------------------------------------

        /**
         * Exporta tokens como JSON
         * @returns {string} JSON stringificado dos tokens
         */
        exportJSON: function() {
            return JSON.stringify(this.tokens, null, 2);
        },

        /**
         * Exporta tokens como variaveis CSS
         * @returns {string} String com variaveis CSS
         */
        exportCSS: function() {
            let css = ':root {\n';

            const processObject = (obj, prefix = '') => {
                Object.entries(obj).forEach(([key, value]) => {
                    const varName = prefix ? `${prefix}-${key}` : key;

                    if (typeof value === 'object' && value !== null) {
                        processObject(value, varName);
                    } else {
                        css += `  --dt-${varName}: ${value};\n`;
                    }
                });
            };

            processObject(this.tokens);
            css += '}\n';

            return css;
        },

        /**
         * Exporta tokens como SCSS variables
         * @returns {string} String com variaveis SCSS
         */
        exportSCSS: function() {
            let scss = '';

            const processObject = (obj, prefix = '') => {
                Object.entries(obj).forEach(([key, value]) => {
                    const varName = prefix ? `${prefix}-${key}` : key;

                    if (typeof value === 'object' && value !== null) {
                        processObject(value, varName);
                    } else {
                        scss += `$dt-${varName}: ${value};\n`;
                    }
                });
            };

            processObject(this.tokens);

            return scss;
        },

        // -----------------------------------------------------------------
        // UTILIDADES
        // -----------------------------------------------------------------

        /**
         * Verifica se o tema atual e escuro
         * @returns {boolean}
         */
        isDarkTheme: function() {
            return this.getTheme() === 'dark';
        },

        /**
         * Verifica se o tema atual e de alto contraste
         * @returns {boolean}
         */
        isHighContrastTheme: function() {
            return this.getTheme() === 'high-contrast';
        },

        /**
         * Cria um seletor de tema
         * @param {HTMLElement} container - Elemento container
         */
        createThemeSelector: function(container) {
            if (!container) return;

            const selector = document.createElement('div');
            selector.className = 'dt-theme-selector';
            selector.setAttribute('role', 'radiogroup');
            selector.setAttribute('aria-label', 'Selecionar tema');

            const themes = [
                { id: 'light', label: 'Claro', icon: 'sun' },
                { id: 'dark', label: 'Escuro', icon: 'moon' },
                { id: 'high-contrast', label: 'Alto Contraste', icon: 'contrast' }
            ];

            themes.forEach(theme => {
                const button = document.createElement('button');
                button.type = 'button';
                button.className = 'dt-theme-btn';
                button.setAttribute('role', 'radio');
                button.setAttribute('aria-checked', this.getTheme() === theme.id);
                button.setAttribute('aria-label', `Tema ${theme.label}`);
                button.dataset.theme = theme.id;

                button.innerHTML = `
                    <span class="dt-theme-icon" aria-hidden="true">${this.getThemeIcon(theme.icon)}</span>
                    <span class="dt-theme-label">${theme.label}</span>
                `;

                button.addEventListener('click', () => {
                    this.setTheme(theme.id);

                    // Atualizar aria-checked
                    selector.querySelectorAll('.dt-theme-btn').forEach(btn => {
                        btn.setAttribute('aria-checked', btn.dataset.theme === theme.id);
                    });
                });

                selector.appendChild(button);
            });

            container.appendChild(selector);

            // Observar mudancas de tema para atualizar UI
            this.onThemeChange((newTheme) => {
                selector.querySelectorAll('.dt-theme-btn').forEach(btn => {
                    btn.setAttribute('aria-checked', btn.dataset.theme === newTheme);
                });
            });

            return selector;
        },

        /**
         * Retorna icone SVG para o tema
         * @param {string} type - Tipo do icone
         * @returns {string} SVG do icone
         */
        getThemeIcon: function(type) {
            const icons = {
                sun: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/><line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/><line x1="1" y1="12" x2="3" y2="12"/><line x1="21" y1="12" x2="23" y2="12"/><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/></svg>',
                moon: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"/></svg>',
                contrast: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><path d="M12 2v20" fill="currentColor"/><path d="M12 2a10 10 0 0 1 0 20" fill="currentColor"/></svg>'
            };
            return icons[type] || '';
        }
    };


    // =============================================================================
    // ESTILOS DO SELETOR DE TEMA
    // =============================================================================

    const themeSelectorStyles = `
        .dt-theme-selector {
            display: inline-flex;
            gap: var(--dt-space-1, 0.25rem);
            padding: var(--dt-space-1, 0.25rem);
            background: var(--dt-bg-surface-sunken, #F9FAFB);
            border-radius: var(--dt-border-radius-lg, 0.5rem);
            border: var(--dt-border-default, 1px solid #E5E7EB);
        }

        .dt-theme-btn {
            display: inline-flex;
            align-items: center;
            gap: var(--dt-space-2, 0.5rem);
            padding: var(--dt-space-2, 0.5rem) var(--dt-space-3, 0.75rem);
            font-size: var(--dt-font-size-sm, 0.875rem);
            font-weight: var(--dt-font-weight-medium, 500);
            color: var(--dt-text-secondary, #6B7280);
            background: transparent;
            border: none;
            border-radius: var(--dt-border-radius-md, 0.375rem);
            cursor: pointer;
            transition: all var(--dt-duration-150, 150ms) var(--dt-ease-in-out, ease);
        }

        .dt-theme-btn:hover {
            color: var(--dt-text-primary, #1F2937);
            background: var(--dt-bg-interactive-hover, #F3F4F6);
        }

        .dt-theme-btn:focus-visible {
            outline: 2px solid var(--dt-color-primary-500, #003B4A);
            outline-offset: 2px;
        }

        .dt-theme-btn[aria-checked="true"] {
            color: var(--dt-color-primary-500, #003B4A);
            background: var(--dt-bg-surface, #FFFFFF);
            box-shadow: var(--dt-shadow-sm, 0 1px 3px rgba(0,0,0,0.1));
        }

        .dt-theme-icon {
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .dt-theme-icon svg {
            width: 1.25rem;
            height: 1.25rem;
        }

        /* Transicao suave de tema */
        .theme-transitioning,
        .theme-transitioning * {
            transition: background-color var(--dt-duration-200, 200ms) var(--dt-ease-in-out, ease),
                        color var(--dt-duration-200, 200ms) var(--dt-ease-in-out, ease),
                        border-color var(--dt-duration-200, 200ms) var(--dt-ease-in-out, ease) !important;
        }
    `;

    // Injetar estilos
    const styleEl = document.createElement('style');
    styleEl.textContent = themeSelectorStyles;
    document.head.appendChild(styleEl);


    // =============================================================================
    // AUTO-INICIALIZACAO
    // =============================================================================

    // Inicializar quando DOM estiver pronto
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => DesignTokens.init());
    } else {
        DesignTokens.init();
    }


    // =============================================================================
    // EXPORTAR PARA GLOBAL
    // =============================================================================

    global.DesignTokens = DesignTokens;

    // Suporte a CommonJS/AMD
    if (typeof module !== 'undefined' && module.exports) {
        module.exports = DesignTokens;
    } else if (typeof define === 'function' && define.amd) {
        define([], function() { return DesignTokens; });
    }

})(typeof window !== 'undefined' ? window : this);


/* =============================================================================
   DOCUMENTACAO DE USO
   =============================================================================

   INICIALIZACAO (automatica):
   - O sistema inicializa automaticamente ao carregar o script
   - Carrega tema salvo ou detecta preferencia do sistema

   ALTERNAR TEMA:
   DesignTokens.setTheme('dark');
   DesignTokens.setTheme('light');
   DesignTokens.setTheme('high-contrast');
   DesignTokens.toggleTheme();

   OBTER TEMA ATUAL:
   const theme = DesignTokens.getTheme();

   OBTER TOKENS:
   const primaryColor = DesignTokens.getToken('colors.primary.500');
   const spacing = DesignTokens.getToken('spacing.4');

   OBTER VARIAVEL CSS:
   const color = DesignTokens.getCSSVar('--dt-color-primary-500');

   DEFINIR VARIAVEL CSS:
   DesignTokens.setCSSVar('--dt-color-primary-500', '#ff0000');

   CUSTOMIZAR CORES:
   DesignTokens.applyCustomColors({
       primary: { 500: '#ff0000' },
       secondary: { 500: '#00ff00' }
   });

   RESETAR CUSTOMIZACOES:
   DesignTokens.resetCustomizations();

   OBSERVAR MUDANCAS:
   const unsubscribe = DesignTokens.onThemeChange((theme) => {
       console.log('Theme changed to:', theme);
   });
   // Parar de observar:
   unsubscribe();

   CRIAR SELETOR DE TEMA:
   DesignTokens.createThemeSelector(document.getElementById('theme-container'));

   EXPORTAR TOKENS:
   const json = DesignTokens.exportJSON();
   const css = DesignTokens.exportCSS();
   const scss = DesignTokens.exportSCSS();

   ============================================================================= */
