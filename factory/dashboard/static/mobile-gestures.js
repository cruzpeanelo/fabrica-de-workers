/**
 * Mobile Gestures e Navegacao - Issue #36
 * =========================================
 * Este arquivo contem a logica JavaScript para:
 * - Touch gestures (swipe entre colunas do Kanban)
 * - Navegacao mobile (dots, indicadores)
 * - Pull to refresh
 * - Deteccao de scroll do Kanban
 *
 * Deve ser incluido apos o Vue.js no dashboard.
 */

// Funcoes de utilidade para mobile
const MobileGestures = {
    // Constantes
    SWIPE_THRESHOLD: 50,
    COLUMN_NAMES: {
        'backlog': 'Backlog',
        'ready': 'Ready',
        'in_progress': 'Em Progresso',
        'review': 'Review',
        'testing': 'Testing',
        'done': 'Done'
    },

    // Estado
    touchStartX: 0,
    touchStartY: 0,
    isSwiping: false,
    currentColumn: 0,
    columnIndicatorTimeout: null,
    swipeHintShown: false,

    /**
     * Inicializa os event listeners para mobile
     * @param {Object} vueInstance - Instancia do Vue com refs necessarios
     */
    init(vueInstance) {
        if (!this.isMobile()) return;

        this.vueInstance = vueInstance;
        this.setupTouchEvents();
        this.setupScrollObserver();
        this.checkFirstVisit();

        console.log('[MobileGestures] Inicializado para dispositivo mobile');
    },

    /**
     * Verifica se eh dispositivo mobile
     */
    isMobile() {
        return window.innerWidth <= 768 ||
               ('ontouchstart' in window) ||
               (navigator.maxTouchPoints > 0);
    },

    /**
     * Configura eventos de touch
     */
    setupTouchEvents() {
        const kanbanContainer = document.querySelector('.kanban-container');
        if (!kanbanContainer) return;

        kanbanContainer.addEventListener('touchstart', (e) => this.onTouchStart(e), { passive: true });
        kanbanContainer.addEventListener('touchmove', (e) => this.onTouchMove(e), { passive: true });
        kanbanContainer.addEventListener('touchend', (e) => this.onTouchEnd(e), { passive: true });

        // Pull to refresh
        document.addEventListener('touchstart', (e) => this.onPullStart(e), { passive: true });
        document.addEventListener('touchmove', (e) => this.onPullMove(e), { passive: false });
        document.addEventListener('touchend', (e) => this.onPullEnd(e), { passive: true });
    },

    /**
     * Configura observer de scroll do Kanban
     */
    setupScrollObserver() {
        const kanbanContainer = document.querySelector('.kanban-container');
        if (!kanbanContainer) return;

        let scrollTimeout;
        kanbanContainer.addEventListener('scroll', () => {
            clearTimeout(scrollTimeout);
            scrollTimeout = setTimeout(() => {
                this.updateCurrentColumn(kanbanContainer);
            }, 100);
        }, { passive: true });
    },

    /**
     * Atualiza a coluna atual baseado na posicao do scroll
     */
    updateCurrentColumn(container) {
        const columns = container.querySelectorAll('.kanban-column-container');
        if (!columns.length) return;

        const containerRect = container.getBoundingClientRect();
        const containerCenter = containerRect.left + containerRect.width / 2;

        let closestColumn = 0;
        let closestDistance = Infinity;

        columns.forEach((col, index) => {
            const colRect = col.getBoundingClientRect();
            const colCenter = colRect.left + colRect.width / 2;
            const distance = Math.abs(colCenter - containerCenter);

            if (distance < closestDistance) {
                closestDistance = distance;
                closestColumn = index;
            }
        });

        if (this.currentColumn !== closestColumn) {
            this.currentColumn = closestColumn;
            this.showColumnIndicator(columns[closestColumn]);
            this.updateNavigationDots(closestColumn);
        }
    },

    /**
     * Mostra o indicador da coluna atual
     */
    showColumnIndicator(columnElement) {
        const columnId = columnElement.querySelector('.kanban-column')?.id;
        if (!columnId) return;

        const status = columnId.replace('column-', '');
        const columnName = this.COLUMN_NAMES[status] || status;

        // Atualiza Vue refs se disponivel
        if (this.vueInstance) {
            this.vueInstance.currentKanbanColumn = this.currentColumn;
            this.vueInstance.currentColumnName = columnName;
            this.vueInstance.showColumnIndicator = true;

            clearTimeout(this.columnIndicatorTimeout);
            this.columnIndicatorTimeout = setTimeout(() => {
                this.vueInstance.showColumnIndicator = false;
            }, 2000);
        }
    },

    /**
     * Atualiza os dots de navegacao
     */
    updateNavigationDots(activeIndex) {
        const dots = document.querySelectorAll('.kanban-nav-dot');
        dots.forEach((dot, index) => {
            dot.classList.toggle('active', index === activeIndex);
        });
    },

    /**
     * Scrolla para uma coluna especifica
     */
    scrollToColumn(index) {
        const kanbanContainer = document.querySelector('.kanban-container');
        const columns = kanbanContainer?.querySelectorAll('.kanban-column-container');

        if (columns && columns[index]) {
            columns[index].scrollIntoView({
                behavior: 'smooth',
                block: 'nearest',
                inline: 'center'
            });
        }
    },

    /**
     * Handlers de touch
     */
    onTouchStart(e) {
        this.touchStartX = e.touches[0].clientX;
        this.touchStartY = e.touches[0].clientY;
        this.isSwiping = true;
    },

    onTouchMove(e) {
        if (!this.isSwiping) return;
        // Permite scroll natural
    },

    onTouchEnd(e) {
        if (!this.isSwiping) return;
        this.isSwiping = false;

        const touchEndX = e.changedTouches[0].clientX;
        const touchEndY = e.changedTouches[0].clientY;
        const deltaX = touchEndX - this.touchStartX;
        const deltaY = touchEndY - this.touchStartY;

        // Verifica se foi um swipe horizontal
        if (Math.abs(deltaX) > this.SWIPE_THRESHOLD && Math.abs(deltaX) > Math.abs(deltaY)) {
            if (deltaX < 0) {
                // Swipe para esquerda - proxima coluna
                this.scrollToColumn(this.currentColumn + 1);
            } else {
                // Swipe para direita - coluna anterior
                this.scrollToColumn(this.currentColumn - 1);
            }
        }
    },

    /**
     * Pull to refresh handlers
     */
    pullStartY: 0,
    isPulling: false,

    onPullStart(e) {
        if (window.scrollY === 0) {
            this.pullStartY = e.touches[0].clientY;
            this.isPulling = true;
        }
    },

    onPullMove(e) {
        if (!this.isPulling) return;

        const pullDistance = e.touches[0].clientY - this.pullStartY;
        if (pullDistance > 60 && window.scrollY === 0) {
            if (this.vueInstance) {
                this.vueInstance.isPullingToRefresh = true;
            }
        }
    },

    onPullEnd(e) {
        if (!this.isPulling) return;
        this.isPulling = false;

        const pullDistance = e.changedTouches[0].clientY - this.pullStartY;
        if (pullDistance > 80 && window.scrollY === 0) {
            // Executa refresh
            if (this.vueInstance && this.vueInstance.loadProjectData) {
                this.vueInstance.loadProjectData();
            }
        }

        if (this.vueInstance) {
            setTimeout(() => {
                this.vueInstance.isPullingToRefresh = false;
            }, 500);
        }
    },

    /**
     * Verifica primeira visita para mostrar hint de swipe
     */
    checkFirstVisit() {
        const hasSeenHint = localStorage.getItem('kanban_swipe_hint_seen');
        if (!hasSeenHint && this.vueInstance) {
            setTimeout(() => {
                this.vueInstance.showSwipeHint = true;
                setTimeout(() => {
                    this.vueInstance.showSwipeHint = false;
                    localStorage.setItem('kanban_swipe_hint_seen', 'true');
                }, 5000);
            }, 2000);
        }
    }
};

// Funcoes auxiliares para serem adicionadas ao Vue
const mobileHelperFunctions = {
    // Toggle do menu mobile
    toggleMobileMenu() {
        this.mobileMenuOpen = !this.mobileMenuOpen;
        this.mobileChatOpen = false;
    },

    // Toggle do chat mobile
    toggleMobileChat() {
        this.mobileChatOpen = !this.mobileChatOpen;
        this.mobileMenuOpen = false;
    },

    // Navega para o Kanban
    goToKanban() {
        this.mobileMenuOpen = false;
        this.mobileChatOpen = false;
        // Se tiver sidebar aberta, fecha
        if (MobileGestures.isMobile()) {
            MobileGestures.scrollToColumn(0);
        }
    },

    // Refresh dos dados com feedback visual
    refreshData() {
        this.isRefreshing = true;
        this.loadProjectData().finally(() => {
            setTimeout(() => {
                this.isRefreshing = false;
            }, 500);
        });
    },

    // Scrolla para coluna especifica do Kanban
    scrollToKanbanColumn(index) {
        MobileGestures.scrollToColumn(index);
    }
};

// Estado adicional para mobile (adicionar ao setup do Vue)
const mobileState = {
    currentKanbanColumn: 0,
    currentColumnName: 'Backlog',
    showColumnIndicator: false,
    showSwipeHint: false,
    isRefreshing: false
};

// Exporta para uso global
window.MobileGestures = MobileGestures;
window.mobileHelperFunctions = mobileHelperFunctions;
window.mobileState = mobileState;

// Auto-inicializa quando o DOM estiver pronto
document.addEventListener('DOMContentLoaded', () => {
    // Inicializa apos um pequeno delay para garantir que Vue ja montou
    setTimeout(() => {
        const vueApp = document.getElementById('app')?.__vue_app__;
        if (vueApp) {
            MobileGestures.init(vueApp._instance?.proxy);
        }
    }, 1000);
});
