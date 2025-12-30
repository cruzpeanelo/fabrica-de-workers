# -*- coding: utf-8 -*-
"""
Onboarding Tour - Issue #204
============================
Tour guiado para novos usuarios conhecerem a plataforma.
Destaca elementos importantes e explica cada secao.
"""

from typing import Optional, List, Dict, Any
from fastapi import APIRouter
from fastapi.responses import HTMLResponse
from pydantic import BaseModel

# =============================================================================
# ROUTER
# =============================================================================

tour_router = APIRouter(prefix="/api/tour", tags=["Onboarding Tour"])


# =============================================================================
# TOUR STEPS
# =============================================================================

TOUR_STEPS = [
    {
        "id": "welcome",
        "title": "Bem-vindo a Fabrica de Agentes!",
        "description": "Vamos fazer um tour rapido para voce conhecer a plataforma. Leva menos de 2 minutos!",
        "target": None,
        "position": "center",
        "icon": "rocket",
        "order": 0
    },
    {
        "id": "sidebar",
        "title": "Menu de Navegacao",
        "description": "Aqui voce encontra todas as secoes da plataforma: Dashboard, Requisitos, Quadro de Tarefas e mais.",
        "target": ".sidebar, #sidebar, nav",
        "position": "right",
        "icon": "menu",
        "order": 1
    },
    {
        "id": "kanban",
        "title": "Quadro de Tarefas",
        "description": "Veja o progresso de todas as tarefas. Arraste os cartoes entre as colunas para atualizar o status.",
        "target": ".kanban-board, #kanban, .board-container",
        "position": "bottom",
        "icon": "columns",
        "order": 2
    },
    {
        "id": "stories",
        "title": "Lista de Requisitos",
        "description": "Todos os requisitos do projeto ficam aqui. Clique em um cartao para ver detalhes e acompanhar o progresso.",
        "target": ".stories-list, #stories, .story-cards",
        "position": "bottom",
        "icon": "file-text",
        "order": 3
    },
    {
        "id": "create-story",
        "title": "Criar Novo Requisito",
        "description": "Clique aqui para adicionar um novo requisito ao projeto. Descreva o que o usuario precisa de forma simples.",
        "target": "#btn-create-story, .btn-new-story, [data-action='create-story']",
        "position": "bottom",
        "icon": "plus-circle",
        "order": 4
    },
    {
        "id": "chat",
        "title": "Assistente Virtual",
        "description": "Precisa de ajuda? O assistente pode responder duvidas, sugerir melhorias e ate gerar codigo automaticamente!",
        "target": ".chat-toggle, #chat-btn, .ai-assistant",
        "position": "left",
        "icon": "message-circle",
        "order": 5
    },
    {
        "id": "progress",
        "title": "Barra de Progresso",
        "description": "Acompanhe o progresso geral do projeto. A barra mostra quantos requisitos ja foram concluidos.",
        "target": ".progress-bar, #progress, .project-progress",
        "position": "bottom",
        "icon": "trending-up",
        "order": 6
    },
    {
        "id": "mode-toggle",
        "title": "Modo de Visualizacao",
        "description": "Alterne entre Modo Basico (simplificado) e Modo Avancado (termos tecnicos) conforme sua preferencia.",
        "target": "#mode-toggle, .mode-switch, [data-action='toggle-mode']",
        "position": "bottom",
        "icon": "toggle-left",
        "order": 7
    },
    {
        "id": "complete",
        "title": "Pronto para Comecar!",
        "description": "Voce ja conhece o basico. Explore a plataforma e clique no icone de ajuda (?) quando precisar de mais informacoes.",
        "target": None,
        "position": "center",
        "icon": "check-circle",
        "order": 8
    }
]


# =============================================================================
# USER TOUR STATE (use database in production)
# =============================================================================

user_tour_state: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# API ENDPOINTS
# =============================================================================

@tour_router.get("/steps")
async def get_tour_steps(user_id: str = "default"):
    """Retorna todos os passos do tour"""
    state = user_tour_state.get(user_id, {
        "completed": False,
        "current_step": 0,
        "skipped": False
    })

    return {
        "steps": TOUR_STEPS,
        "total_steps": len(TOUR_STEPS),
        "user_state": state
    }


@tour_router.get("/step/{step_id}")
async def get_step(step_id: str):
    """Retorna um passo especifico do tour"""
    step = next((s for s in TOUR_STEPS if s["id"] == step_id), None)
    if not step:
        return {"error": "Step not found"}
    return step


@tour_router.post("/start")
async def start_tour(user_id: str = "default"):
    """Inicia o tour para o usuario"""
    user_tour_state[user_id] = {
        "completed": False,
        "current_step": 0,
        "skipped": False,
        "started_at": "now"
    }
    return {
        "success": True,
        "first_step": TOUR_STEPS[0]
    }


@tour_router.post("/next")
async def next_step(user_id: str = "default"):
    """Avanca para o proximo passo"""
    state = user_tour_state.get(user_id, {"current_step": 0})
    current = state.get("current_step", 0)
    next_idx = current + 1

    if next_idx >= len(TOUR_STEPS):
        state["completed"] = True
        state["current_step"] = len(TOUR_STEPS) - 1
        user_tour_state[user_id] = state
        return {
            "success": True,
            "completed": True,
            "step": TOUR_STEPS[-1]
        }

    state["current_step"] = next_idx
    user_tour_state[user_id] = state

    return {
        "success": True,
        "step": TOUR_STEPS[next_idx],
        "progress": f"{next_idx + 1}/{len(TOUR_STEPS)}"
    }


@tour_router.post("/previous")
async def previous_step(user_id: str = "default"):
    """Volta para o passo anterior"""
    state = user_tour_state.get(user_id, {"current_step": 0})
    current = state.get("current_step", 0)
    prev_idx = max(0, current - 1)

    state["current_step"] = prev_idx
    user_tour_state[user_id] = state

    return {
        "success": True,
        "step": TOUR_STEPS[prev_idx],
        "progress": f"{prev_idx + 1}/{len(TOUR_STEPS)}"
    }


@tour_router.post("/skip")
async def skip_tour(user_id: str = "default"):
    """Pula o tour"""
    user_tour_state[user_id] = {
        "completed": True,
        "skipped": True,
        "current_step": len(TOUR_STEPS) - 1
    }
    return {"success": True, "skipped": True}


@tour_router.post("/complete")
async def complete_tour(user_id: str = "default"):
    """Marca o tour como completado"""
    user_tour_state[user_id] = {
        "completed": True,
        "skipped": False,
        "current_step": len(TOUR_STEPS) - 1
    }
    return {"success": True, "completed": True}


@tour_router.get("/status")
async def get_tour_status(user_id: str = "default"):
    """Verifica status do tour do usuario"""
    state = user_tour_state.get(user_id, {
        "completed": False,
        "current_step": 0,
        "skipped": False
    })

    return {
        "should_show_tour": not state.get("completed", False),
        "completed": state.get("completed", False),
        "skipped": state.get("skipped", False),
        "current_step": state.get("current_step", 0)
    }


@tour_router.post("/reset")
async def reset_tour(user_id: str = "default"):
    """Reseta o tour para o usuario poder refazer"""
    user_tour_state[user_id] = {
        "completed": False,
        "current_step": 0,
        "skipped": False
    }
    return {"success": True, "reset": True}


# =============================================================================
# REGISTRATION
# =============================================================================

def register_tour_endpoints(app):
    """Registra endpoints do tour no app FastAPI"""
    app.include_router(tour_router)
    print("[Dashboard] Tour endpoints registered")


# =============================================================================
# CSS STYLES
# =============================================================================

TOUR_CSS = '''
/* Tour Overlay */
.tour-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.7);
    z-index: 9998;
    transition: opacity 0.3s ease;
}

/* Tour Spotlight */
.tour-spotlight {
    position: absolute;
    box-shadow: 0 0 0 9999px rgba(0, 0, 0, 0.7);
    border-radius: 8px;
    z-index: 9999;
    transition: all 0.3s ease;
}

/* Tour Tooltip */
.tour-tooltip {
    position: absolute;
    background: white;
    border-radius: 16px;
    padding: 24px;
    max-width: 400px;
    min-width: 300px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    z-index: 10000;
    animation: tourSlideIn 0.3s ease;
}

@keyframes tourSlideIn {
    from {
        opacity: 0;
        transform: translateY(10px);
    }
    to {
        opacity: 1;
        transform: translateY(0);
    }
}

.tour-tooltip-arrow {
    position: absolute;
    width: 16px;
    height: 16px;
    background: white;
    transform: rotate(45deg);
}

.tour-tooltip-arrow.top {
    bottom: -8px;
    left: 50%;
    margin-left: -8px;
}

.tour-tooltip-arrow.bottom {
    top: -8px;
    left: 50%;
    margin-left: -8px;
}

.tour-tooltip-arrow.left {
    right: -8px;
    top: 50%;
    margin-top: -8px;
}

.tour-tooltip-arrow.right {
    left: -8px;
    top: 50%;
    margin-top: -8px;
}

/* Tour Header */
.tour-header {
    display: flex;
    align-items: center;
    gap: 12px;
    margin-bottom: 16px;
}

.tour-icon {
    width: 48px;
    height: 48px;
    background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
    border-radius: 12px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
}

.tour-title {
    font-size: 18px;
    font-weight: 600;
    color: #1F2937;
    margin: 0;
}

/* Tour Content */
.tour-description {
    font-size: 15px;
    color: #6B7280;
    line-height: 1.6;
    margin-bottom: 20px;
}

/* Tour Footer */
.tour-footer {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
}

.tour-progress {
    font-size: 13px;
    color: #9CA3AF;
}

.tour-progress-bar {
    width: 100px;
    height: 4px;
    background: #E5E7EB;
    border-radius: 2px;
    overflow: hidden;
    margin-top: 4px;
}

.tour-progress-fill {
    height: 100%;
    background: linear-gradient(90deg, #003B4A, #FF6C00);
    border-radius: 2px;
    transition: width 0.3s ease;
}

.tour-buttons {
    display: flex;
    gap: 8px;
}

.tour-btn {
    padding: 10px 20px;
    border-radius: 10px;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.2s;
    border: none;
}

.tour-btn-skip {
    background: transparent;
    color: #9CA3AF;
}

.tour-btn-skip:hover {
    color: #6B7280;
}

.tour-btn-prev {
    background: #F3F4F6;
    color: #4B5563;
}

.tour-btn-prev:hover {
    background: #E5E7EB;
}

.tour-btn-next {
    background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
    color: white;
}

.tour-btn-next:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(0, 59, 74, 0.3);
}

.tour-btn-finish {
    background: linear-gradient(135deg, #FF6C00 0%, #FF8533 100%);
    color: white;
}

.tour-btn-finish:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(255, 108, 0, 0.3);
}

/* Center Position (Welcome/Complete screens) */
.tour-tooltip.center {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    text-align: center;
}

.tour-tooltip.center .tour-header {
    justify-content: center;
    flex-direction: column;
}

.tour-tooltip.center .tour-icon {
    width: 72px;
    height: 72px;
    margin-bottom: 8px;
}

.tour-tooltip.center .tour-title {
    font-size: 24px;
}

.tour-tooltip.center .tour-footer {
    justify-content: center;
}

/* Responsive */
@media (max-width: 640px) {
    .tour-tooltip {
        max-width: calc(100vw - 32px);
        min-width: auto;
        margin: 16px;
    }

    .tour-tooltip.center {
        width: calc(100vw - 32px);
    }
}
'''


# =============================================================================
# JAVASCRIPT
# =============================================================================

TOUR_JS = '''
// Onboarding Tour Manager
const OnboardingTour = {
    steps: [],
    currentStep: 0,
    overlay: null,
    tooltip: null,
    spotlight: null,

    async init() {
        // Check if should show tour
        const status = await this.checkStatus();
        if (status.should_show_tour) {
            await this.loadSteps();
            this.createElements();
            this.start();
        }
    },

    async checkStatus() {
        try {
            const res = await fetch('/api/tour/status?user_id=default');
            return await res.json();
        } catch (e) {
            const completed = localStorage.getItem('tour_completed');
            return { should_show_tour: !completed };
        }
    },

    async loadSteps() {
        try {
            const res = await fetch('/api/tour/steps?user_id=default');
            const data = await res.json();
            this.steps = data.steps;
        } catch (e) {
            console.error('Failed to load tour steps:', e);
        }
    },

    createElements() {
        // Overlay
        this.overlay = document.createElement('div');
        this.overlay.className = 'tour-overlay';
        this.overlay.style.display = 'none';
        document.body.appendChild(this.overlay);

        // Spotlight
        this.spotlight = document.createElement('div');
        this.spotlight.className = 'tour-spotlight';
        this.spotlight.style.display = 'none';
        document.body.appendChild(this.spotlight);

        // Tooltip
        this.tooltip = document.createElement('div');
        this.tooltip.className = 'tour-tooltip';
        this.tooltip.style.display = 'none';
        document.body.appendChild(this.tooltip);
    },

    start() {
        this.currentStep = 0;
        this.overlay.style.display = 'block';
        this.showStep(0);
        fetch('/api/tour/start?user_id=default', { method: 'POST' });
    },

    showStep(index) {
        if (index < 0 || index >= this.steps.length) return;

        const step = this.steps[index];
        this.currentStep = index;

        // Update tooltip content
        this.tooltip.innerHTML = this.renderTooltip(step, index);
        this.tooltip.style.display = 'block';

        // Position tooltip
        if (step.target && step.position !== 'center') {
            const target = document.querySelector(step.target);
            if (target) {
                this.highlightElement(target);
                this.positionTooltip(target, step.position);
            } else {
                this.centerTooltip();
            }
        } else {
            this.spotlight.style.display = 'none';
            this.centerTooltip();
        }

        // Bind buttons
        this.bindButtons();
    },

    renderTooltip(step, index) {
        const isFirst = index === 0;
        const isLast = index === this.steps.length - 1;
        const progress = ((index + 1) / this.steps.length * 100).toFixed(0);

        return `
            <div class="tour-header">
                <div class="tour-icon">
                    <i class="lucide-${step.icon}" style="width: 24px; height: 24px;"></i>
                </div>
                <h3 class="tour-title">${step.title}</h3>
            </div>
            <p class="tour-description">${step.description}</p>
            <div class="tour-footer">
                <div class="tour-progress">
                    <span>${index + 1} de ${this.steps.length}</span>
                    <div class="tour-progress-bar">
                        <div class="tour-progress-fill" style="width: ${progress}%"></div>
                    </div>
                </div>
                <div class="tour-buttons">
                    ${!isFirst ? '<button class="tour-btn tour-btn-skip" id="tour-skip">Pular</button>' : ''}
                    ${!isFirst ? '<button class="tour-btn tour-btn-prev" id="tour-prev">Anterior</button>' : ''}
                    ${isLast
                        ? '<button class="tour-btn tour-btn-finish" id="tour-next">Comecar!</button>'
                        : '<button class="tour-btn tour-btn-next" id="tour-next">Proximo</button>'
                    }
                </div>
            </div>
        `;
    },

    highlightElement(element) {
        const rect = element.getBoundingClientRect();
        const padding = 8;

        this.spotlight.style.display = 'block';
        this.spotlight.style.top = (rect.top - padding + window.scrollY) + 'px';
        this.spotlight.style.left = (rect.left - padding) + 'px';
        this.spotlight.style.width = (rect.width + padding * 2) + 'px';
        this.spotlight.style.height = (rect.height + padding * 2) + 'px';

        // Scroll into view if needed
        element.scrollIntoView({ behavior: 'smooth', block: 'center' });
    },

    positionTooltip(target, position) {
        const rect = target.getBoundingClientRect();
        const tooltip = this.tooltip;
        const gap = 16;

        tooltip.classList.remove('center');

        switch (position) {
            case 'bottom':
                tooltip.style.top = (rect.bottom + gap + window.scrollY) + 'px';
                tooltip.style.left = Math.max(16, rect.left) + 'px';
                tooltip.style.transform = 'none';
                break;
            case 'top':
                tooltip.style.top = (rect.top - gap + window.scrollY) + 'px';
                tooltip.style.left = Math.max(16, rect.left) + 'px';
                tooltip.style.transform = 'translateY(-100%)';
                break;
            case 'left':
                tooltip.style.top = (rect.top + window.scrollY) + 'px';
                tooltip.style.left = (rect.left - gap) + 'px';
                tooltip.style.transform = 'translateX(-100%)';
                break;
            case 'right':
                tooltip.style.top = (rect.top + window.scrollY) + 'px';
                tooltip.style.left = (rect.right + gap) + 'px';
                tooltip.style.transform = 'none';
                break;
        }
    },

    centerTooltip() {
        this.tooltip.classList.add('center');
        this.tooltip.style.top = '50%';
        this.tooltip.style.left = '50%';
        this.tooltip.style.transform = 'translate(-50%, -50%)';
    },

    bindButtons() {
        const nextBtn = document.getElementById('tour-next');
        const prevBtn = document.getElementById('tour-prev');
        const skipBtn = document.getElementById('tour-skip');

        if (nextBtn) {
            nextBtn.onclick = () => {
                if (this.currentStep >= this.steps.length - 1) {
                    this.complete();
                } else {
                    this.next();
                }
            };
        }

        if (prevBtn) {
            prevBtn.onclick = () => this.prev();
        }

        if (skipBtn) {
            skipBtn.onclick = () => this.skip();
        }
    },

    next() {
        if (this.currentStep < this.steps.length - 1) {
            this.showStep(this.currentStep + 1);
            fetch('/api/tour/next?user_id=default', { method: 'POST' });
        }
    },

    prev() {
        if (this.currentStep > 0) {
            this.showStep(this.currentStep - 1);
            fetch('/api/tour/previous?user_id=default', { method: 'POST' });
        }
    },

    skip() {
        this.end();
        fetch('/api/tour/skip?user_id=default', { method: 'POST' });
        localStorage.setItem('tour_completed', 'true');
    },

    complete() {
        this.end();
        fetch('/api/tour/complete?user_id=default', { method: 'POST' });
        localStorage.setItem('tour_completed', 'true');
    },

    end() {
        this.overlay.style.display = 'none';
        this.spotlight.style.display = 'none';
        this.tooltip.style.display = 'none';
    },

    // Manual trigger to restart tour
    restart() {
        localStorage.removeItem('tour_completed');
        fetch('/api/tour/reset?user_id=default', { method: 'POST' });
        this.start();
    }
};

// Auto-initialize
document.addEventListener('DOMContentLoaded', () => {
    // Small delay to ensure page is fully loaded
    setTimeout(() => OnboardingTour.init(), 1000);
});

// Expose for manual trigger
window.OnboardingTour = OnboardingTour;
'''


# =============================================================================
# HTML COMPONENT
# =============================================================================

def get_tour_html():
    """Retorna HTML/CSS/JS do tour para incluir no dashboard"""
    return f'''
    <style>{TOUR_CSS}</style>
    <script>{TOUR_JS}</script>
    '''


# =============================================================================
# TRIGGER BUTTON COMPONENT
# =============================================================================

TOUR_TRIGGER_BUTTON = '''
<button
    onclick="OnboardingTour.restart()"
    style="
        position: fixed;
        bottom: 80px;
        right: 24px;
        width: 48px;
        height: 48px;
        border-radius: 50%;
        background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
        color: white;
        border: none;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 4px 12px rgba(0, 59, 74, 0.3);
        z-index: 1000;
        transition: all 0.2s;
    "
    title="Refazer Tour"
    aria-label="Refazer tour de onboarding"
>
    <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
        <circle cx="12" cy="12" r="10"></circle>
        <path d="M12 16v-4"></path>
        <path d="M12 8h.01"></path>
    </svg>
</button>
'''
