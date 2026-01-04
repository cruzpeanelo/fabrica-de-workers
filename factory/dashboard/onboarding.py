# -*- coding: utf-8 -*-
"""
Onboarding Guiado - Issue #132
==============================
Sistema de onboarding interativo para novos usuarios:
- Tour interativo com tooltips
- Checklist de primeiros passos
- Progress tracker
- Skip option
"""

from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import APIRouter
from fastapi.responses import HTMLResponse, JSONResponse
from pydantic import BaseModel


# =============================================================================
# ROUTER
# =============================================================================

onboarding_router = APIRouter(prefix="/api/onboarding", tags=["Onboarding"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class OnboardingProgress(BaseModel):
    user_id: str
    completed_steps: List[str] = []
    current_step: int = 0
    skipped: bool = False
    started_at: Optional[str] = None
    completed_at: Optional[str] = None


class OnboardingStep(BaseModel):
    step_id: str
    title: str
    description: str
    element_selector: str  # CSS selector for tooltip target
    position: str = "bottom"  # top, bottom, left, right
    action_type: str = "click"  # click, hover, input, none
    action_target: Optional[str] = None
    next_condition: str = "click"  # click, action, auto


# =============================================================================
# ONBOARDING STEPS CONFIGURATION
# =============================================================================

ONBOARDING_STEPS = [
    {
        "step_id": "welcome",
        "title": "Bem-vindo a Plataforma E!",
        "description": "Este sistema ajuda voce a criar e gerenciar projetos de software de forma simples e organizada. Vamos fazer um tour rapido?",
        "element_selector": ".welcome-banner",
        "position": "center",
        "action_type": "none",
        "next_condition": "click"
    },
    {
        "step_id": "kanban",
        "title": "Quadro de Tarefas",
        "description": "Aqui voce ve todas as suas tarefas organizadas em colunas. Arraste os cards para mover tarefas entre as etapas.",
        "element_selector": ".kanban-board",
        "position": "bottom",
        "action_type": "hover",
        "next_condition": "click"
    },
    {
        "step_id": "create_story",
        "title": "Criar Nova Tarefa",
        "description": "Clique neste botao para criar uma nova tarefa. Voce pode descrever o que precisa ser feito de forma simples.",
        "element_selector": "#btn-new-story",
        "position": "left",
        "action_type": "click",
        "next_condition": "action"
    },
    {
        "step_id": "story_form",
        "title": "Formulario de Tarefa",
        "description": "Preencha o titulo e a descricao da tarefa. Nao se preocupe com termos tecnicos - use suas proprias palavras!",
        "element_selector": "#story-modal",
        "position": "right",
        "action_type": "input",
        "next_condition": "click"
    },
    {
        "step_id": "progress",
        "title": "Acompanhe o Progresso",
        "description": "Veja em tempo real o andamento do seu projeto. As barras mostram quanto ja foi concluido.",
        "element_selector": ".progress-container",
        "position": "bottom",
        "action_type": "hover",
        "next_condition": "click"
    },
    {
        "step_id": "chat",
        "title": "Assistente Virtual",
        "description": "Tem duvidas? Clique aqui para conversar com nosso assistente. Ele pode ajudar a criar tarefas e responder perguntas.",
        "element_selector": "#chat-button",
        "position": "left",
        "action_type": "click",
        "next_condition": "click"
    },
    {
        "step_id": "test_app",
        "title": "Testar Aplicacao",
        "description": "Quando suas tarefas estiverem prontas, use este botao para testar a aplicacao gerada. E simples assim!",
        "element_selector": ".fab-test-app",
        "position": "left",
        "action_type": "hover",
        "next_condition": "click"
    },
    {
        "step_id": "complete",
        "title": "Tudo Pronto!",
        "description": "Parabens! Voce conheceu as principais funcoes. Lembre-se: pode repetir este tour a qualquer momento no menu de ajuda.",
        "element_selector": ".welcome-banner",
        "position": "center",
        "action_type": "none",
        "next_condition": "click"
    }
]


CHECKLIST_ITEMS = [
    {
        "item_id": "create_project",
        "title": "Criar seu primeiro projeto",
        "description": "De um nome ao seu projeto e comece a organizar suas ideias",
        "icon": "folder-plus",
        "action_url": "#",
        "action_text": "Criar Projeto"
    },
    {
        "item_id": "create_story",
        "title": "Criar sua primeira tarefa",
        "description": "Descreva algo que voce precisa fazer ou uma funcionalidade desejada",
        "icon": "file-text",
        "action_url": "#",
        "action_text": "Nova Tarefa"
    },
    {
        "item_id": "move_task",
        "title": "Mover uma tarefa no quadro",
        "description": "Arraste um card de uma coluna para outra",
        "icon": "move",
        "action_url": None,
        "action_text": None
    },
    {
        "item_id": "use_chat",
        "title": "Conversar com o assistente",
        "description": "Faca uma pergunta ou peca ajuda ao assistente virtual",
        "icon": "message-circle",
        "action_url": "#chat",
        "action_text": "Abrir Chat"
    },
    {
        "item_id": "view_progress",
        "title": "Ver o progresso do projeto",
        "description": "Confira quanto do projeto ja foi concluido",
        "icon": "bar-chart-2",
        "action_url": "#progress",
        "action_text": "Ver Progresso"
    }
]


# In-memory storage for demo (use database in production)
user_progress: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# API ENDPOINTS
# =============================================================================

@onboarding_router.get("/steps")
async def get_onboarding_steps():
    """Retorna todos os passos do onboarding"""
    return {"steps": ONBOARDING_STEPS, "total": len(ONBOARDING_STEPS)}


@onboarding_router.get("/checklist")
async def get_checklist():
    """Retorna o checklist de primeiros passos"""
    return {"items": CHECKLIST_ITEMS, "total": len(CHECKLIST_ITEMS)}


@onboarding_router.get("/progress/{user_id}")
async def get_user_progress(user_id: str):
    """Retorna o progresso do usuario no onboarding"""
    if user_id not in user_progress:
        user_progress[user_id] = {
            "completed_steps": [],
            "completed_checklist": [],
            "current_step": 0,
            "skipped": False,
            "started_at": None,
            "completed_at": None
        }
    return user_progress[user_id]


@onboarding_router.post("/progress/{user_id}/step/{step_id}")
async def complete_step(user_id: str, step_id: str):
    """Marca um passo do onboarding como completo"""
    if user_id not in user_progress:
        user_progress[user_id] = {
            "completed_steps": [],
            "completed_checklist": [],
            "current_step": 0,
            "skipped": False,
            "started_at": datetime.utcnow().isoformat(),
            "completed_at": None
        }

    if step_id not in user_progress[user_id]["completed_steps"]:
        user_progress[user_id]["completed_steps"].append(step_id)
        user_progress[user_id]["current_step"] = len(user_progress[user_id]["completed_steps"])

    # Check if all steps completed
    if len(user_progress[user_id]["completed_steps"]) >= len(ONBOARDING_STEPS):
        user_progress[user_id]["completed_at"] = datetime.utcnow().isoformat()

    return user_progress[user_id]


@onboarding_router.post("/progress/{user_id}/checklist/{item_id}")
async def complete_checklist_item(user_id: str, item_id: str):
    """Marca um item do checklist como completo"""
    if user_id not in user_progress:
        user_progress[user_id] = {
            "completed_steps": [],
            "completed_checklist": [],
            "current_step": 0,
            "skipped": False,
            "started_at": datetime.utcnow().isoformat(),
            "completed_at": None
        }

    if item_id not in user_progress[user_id]["completed_checklist"]:
        user_progress[user_id]["completed_checklist"].append(item_id)

    return user_progress[user_id]


@onboarding_router.post("/progress/{user_id}/skip")
async def skip_onboarding(user_id: str):
    """Pula o onboarding"""
    if user_id not in user_progress:
        user_progress[user_id] = {
            "completed_steps": [],
            "completed_checklist": [],
            "current_step": 0,
            "skipped": True,
            "started_at": datetime.utcnow().isoformat(),
            "completed_at": None
        }
    else:
        user_progress[user_id]["skipped"] = True

    return user_progress[user_id]


@onboarding_router.post("/progress/{user_id}/reset")
async def reset_onboarding(user_id: str):
    """Reinicia o onboarding do usuario"""
    user_progress[user_id] = {
        "completed_steps": [],
        "completed_checklist": [],
        "current_step": 0,
        "skipped": False,
        "started_at": datetime.utcnow().isoformat(),
        "completed_at": None
    }
    return user_progress[user_id]


# =============================================================================
# HTML TEMPLATE
# =============================================================================

ONBOARDING_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Onboarding - Plataforma E</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://unpkg.com/lucide-static@latest/font/lucide.css">
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
            background: #F3F4F6;
            min-height: 100vh;
        }

        /* Overlay for tour */
        .onboarding-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.5);
            z-index: 9998;
            display: none;
        }

        .onboarding-overlay.active {
            display: block;
        }

        /* Spotlight effect */
        .onboarding-spotlight {
            position: absolute;
            box-shadow: 0 0 0 9999px rgba(0, 0, 0, 0.5);
            border-radius: 8px;
            z-index: 9999;
            transition: all 0.3s ease;
        }

        /* Tooltip */
        .onboarding-tooltip {
            position: absolute;
            background: white;
            border-radius: 12px;
            padding: 24px;
            max-width: 360px;
            box-shadow: 0 20px 40px rgba(0, 0, 0, 0.2);
            z-index: 10000;
            animation: tooltipFadeIn 0.3s ease;
        }

        @keyframes tooltipFadeIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }

        .onboarding-tooltip::before {
            content: '';
            position: absolute;
            width: 16px;
            height: 16px;
            background: white;
            transform: rotate(45deg);
        }

        .onboarding-tooltip.position-bottom::before {
            top: -8px;
            left: 50%;
            margin-left: -8px;
        }

        .onboarding-tooltip.position-top::before {
            bottom: -8px;
            left: 50%;
            margin-left: -8px;
        }

        .onboarding-tooltip.position-left::before {
            right: -8px;
            top: 50%;
            margin-top: -8px;
        }

        .onboarding-tooltip.position-right::before {
            left: -8px;
            top: 50%;
            margin-top: -8px;
        }

        .tooltip-step {
            display: flex;
            align-items: center;
            gap: 8px;
            margin-bottom: 12px;
            color: #003B4A;
            font-size: 13px;
            font-weight: 500;
        }

        .tooltip-step-number {
            background: #FF6C00;
            color: white;
            width: 24px;
            height: 24px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 12px;
            font-weight: 600;
        }

        .tooltip-title {
            font-size: 18px;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 8px;
        }

        .tooltip-description {
            color: #6B7280;
            line-height: 1.6;
            margin-bottom: 20px;
        }

        .tooltip-actions {
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .tooltip-skip {
            color: #9CA3AF;
            background: none;
            border: none;
            cursor: pointer;
            font-size: 14px;
            padding: 8px 16px;
            border-radius: 6px;
            transition: all 0.2s;
        }

        .tooltip-skip:hover {
            color: #6B7280;
            background: #F3F4F6;
        }

        .tooltip-next {
            background: #003B4A;
            color: white;
            border: none;
            padding: 10px 24px;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .tooltip-next:hover {
            background: #004d5e;
            transform: translateY(-1px);
        }

        /* Progress dots */
        .tooltip-progress {
            display: flex;
            gap: 6px;
            margin-top: 16px;
            justify-content: center;
        }

        .progress-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            background: #E5E7EB;
            transition: all 0.2s;
        }

        .progress-dot.active {
            background: #FF6C00;
            width: 24px;
            border-radius: 4px;
        }

        .progress-dot.completed {
            background: #10B981;
        }

        /* Checklist Panel */
        .checklist-panel {
            position: fixed;
            bottom: 100px;
            right: 24px;
            width: 360px;
            background: white;
            border-radius: 16px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.15);
            z-index: 1000;
            overflow: hidden;
            transform: translateY(20px);
            opacity: 0;
            transition: all 0.3s ease;
            pointer-events: none;
        }

        .checklist-panel.active {
            transform: translateY(0);
            opacity: 1;
            pointer-events: auto;
        }

        .checklist-header {
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            color: white;
            padding: 20px;
        }

        .checklist-header h3 {
            font-size: 16px;
            font-weight: 600;
            margin-bottom: 4px;
        }

        .checklist-header p {
            font-size: 13px;
            opacity: 0.8;
        }

        .checklist-progress {
            margin-top: 12px;
            background: rgba(255, 255, 255, 0.2);
            border-radius: 4px;
            height: 6px;
            overflow: hidden;
        }

        .checklist-progress-bar {
            height: 100%;
            background: #FF6C00;
            border-radius: 4px;
            transition: width 0.5s ease;
        }

        .checklist-items {
            padding: 12px;
            max-height: 320px;
            overflow-y: auto;
        }

        .checklist-item {
            display: flex;
            align-items: flex-start;
            gap: 12px;
            padding: 12px;
            border-radius: 10px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .checklist-item:hover {
            background: #F9FAFB;
        }

        .checklist-item.completed {
            opacity: 0.6;
        }

        .checklist-checkbox {
            width: 24px;
            height: 24px;
            border: 2px solid #D1D5DB;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            flex-shrink: 0;
            transition: all 0.2s;
        }

        .checklist-item.completed .checklist-checkbox {
            background: #10B981;
            border-color: #10B981;
            color: white;
        }

        .checklist-icon {
            width: 36px;
            height: 36px;
            background: #F3F4F6;
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            color: #003B4A;
            flex-shrink: 0;
        }

        .checklist-content {
            flex: 1;
        }

        .checklist-content h4 {
            font-size: 14px;
            font-weight: 500;
            color: #1F2937;
            margin-bottom: 2px;
        }

        .checklist-content p {
            font-size: 12px;
            color: #6B7280;
        }

        .checklist-item.completed .checklist-content h4 {
            text-decoration: line-through;
        }

        /* Checklist Toggle Button */
        .checklist-toggle {
            position: fixed;
            bottom: 24px;
            right: 24px;
            width: 56px;
            height: 56px;
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            cursor: pointer;
            box-shadow: 0 4px 20px rgba(0, 59, 74, 0.3);
            z-index: 999;
            transition: all 0.3s;
        }

        .checklist-toggle:hover {
            transform: scale(1.05);
            box-shadow: 0 6px 24px rgba(0, 59, 74, 0.4);
        }

        .checklist-toggle .badge {
            position: absolute;
            top: -4px;
            right: -4px;
            background: #FF6C00;
            color: white;
            font-size: 11px;
            font-weight: 600;
            width: 22px;
            height: 22px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        /* Welcome Modal */
        .welcome-modal {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.6);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 10001;
        }

        .welcome-modal.hidden {
            display: none;
        }

        .welcome-content {
            background: white;
            border-radius: 20px;
            padding: 40px;
            max-width: 480px;
            text-align: center;
            animation: modalFadeIn 0.4s ease;
        }

        @keyframes modalFadeIn {
            from { opacity: 0; transform: scale(0.9); }
            to { opacity: 1; transform: scale(1); }
        }

        .welcome-icon {
            width: 80px;
            height: 80px;
            background: linear-gradient(135deg, #FF6C00 0%, #FF8C40 100%);
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            margin: 0 auto 24px;
            color: white;
            font-size: 36px;
        }

        .welcome-content h2 {
            font-size: 24px;
            font-weight: 700;
            color: #1F2937;
            margin-bottom: 12px;
        }

        .welcome-content p {
            color: #6B7280;
            line-height: 1.6;
            margin-bottom: 32px;
        }

        .welcome-actions {
            display: flex;
            flex-direction: column;
            gap: 12px;
        }

        .btn-start-tour {
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            color: white;
            border: none;
            padding: 14px 32px;
            border-radius: 10px;
            font-size: 15px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }

        .btn-start-tour:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 20px rgba(0, 59, 74, 0.3);
        }

        .btn-skip-tour {
            background: none;
            border: none;
            color: #9CA3AF;
            font-size: 14px;
            cursor: pointer;
            padding: 8px;
        }

        .btn-skip-tour:hover {
            color: #6B7280;
        }

        /* Help button always visible */
        .help-button {
            position: fixed;
            bottom: 90px;
            right: 24px;
            background: white;
            border: 2px solid #E5E7EB;
            border-radius: 50%;
            width: 40px;
            height: 40px;
            display: flex;
            align-items: center;
            justify-content: center;
            cursor: pointer;
            z-index: 998;
            transition: all 0.2s;
            color: #6B7280;
        }

        .help-button:hover {
            border-color: #003B4A;
            color: #003B4A;
            transform: scale(1.05);
        }
    </style>
</head>
<body>
    <!-- Welcome Modal -->
    <div class="welcome-modal" id="welcomeModal">
        <div class="welcome-content">
            <div class="welcome-icon">
                <i class="lucide-rocket"></i>
            </div>
            <h2>Bem-vindo a Plataforma E!</h2>
            <p>
                Estamos felizes em te-lo aqui! Este sistema foi criado para facilitar
                o gerenciamento dos seus projetos. Que tal fazer um tour rapido para
                conhecer as principais funcoes?
            </p>
            <div class="welcome-actions">
                <button class="btn-start-tour" onclick="startTour()">
                    Comecar Tour Guiado
                </button>
                <button class="btn-skip-tour" onclick="skipTour()">
                    Pular por agora
                </button>
            </div>
        </div>
    </div>

    <!-- Onboarding Overlay -->
    <div class="onboarding-overlay" id="onboardingOverlay"></div>

    <!-- Tooltip (dynamically positioned) -->
    <div class="onboarding-tooltip" id="onboardingTooltip" style="display: none;">
        <div class="tooltip-step">
            <span class="tooltip-step-number" id="stepNumber">1</span>
            <span>Passo <span id="currentStep">1</span> de <span id="totalSteps">8</span></span>
        </div>
        <h3 class="tooltip-title" id="tooltipTitle">Titulo</h3>
        <p class="tooltip-description" id="tooltipDescription">Descricao</p>
        <div class="tooltip-actions">
            <button class="tooltip-skip" onclick="skipTour()">Pular Tour</button>
            <button class="tooltip-next" onclick="nextStep()">
                Proximo <i class="lucide-arrow-right" style="width: 16px; height: 16px;"></i>
            </button>
        </div>
        <div class="tooltip-progress" id="tooltipProgress"></div>
    </div>

    <!-- Checklist Panel -->
    <div class="checklist-panel" id="checklistPanel">
        <div class="checklist-header">
            <h3>Primeiros Passos</h3>
            <p>Complete estas tarefas para comecar</p>
            <div class="checklist-progress">
                <div class="checklist-progress-bar" id="checklistProgressBar" style="width: 0%"></div>
            </div>
        </div>
        <div class="checklist-items" id="checklistItems">
            <!-- Items populated by JS -->
        </div>
    </div>

    <!-- Checklist Toggle -->
    <div class="checklist-toggle" id="checklistToggle" onclick="toggleChecklist()">
        <i class="lucide-list-checks" style="width: 24px; height: 24px;"></i>
        <span class="badge" id="checklistBadge">5</span>
    </div>

    <!-- Help Button -->
    <div class="help-button" onclick="showWelcome()" title="Ajuda">
        <i class="lucide-help-circle" style="width: 20px; height: 20px;"></i>
    </div>

    <script>
        // State
        let currentStepIndex = 0;
        let tourActive = false;
        let checklistOpen = false;
        const userId = 'demo-user-' + Date.now();

        // Onboarding steps from server
        const steps = ''' + str(ONBOARDING_STEPS).replace("'", '"') + ''';

        // Checklist items from server
        const checklistItems = ''' + str(CHECKLIST_ITEMS).replace("'", '"') + ''';

        // Check if first visit
        function checkFirstVisit() {
            const visited = localStorage.getItem('fabrica_onboarding_completed');
            if (!visited) {
                document.getElementById('welcomeModal').classList.remove('hidden');
            } else {
                document.getElementById('welcomeModal').classList.add('hidden');
            }
        }

        // Start tour
        function startTour() {
            document.getElementById('welcomeModal').classList.add('hidden');
            tourActive = true;
            currentStepIndex = 0;
            showStep(currentStepIndex);

            fetch(`/api/onboarding/progress/${userId}/reset`, { method: 'POST' });
        }

        // Skip tour
        function skipTour() {
            document.getElementById('welcomeModal').classList.add('hidden');
            document.getElementById('onboardingOverlay').classList.remove('active');
            document.getElementById('onboardingTooltip').style.display = 'none';
            tourActive = false;
            localStorage.setItem('fabrica_onboarding_completed', 'skipped');

            fetch(`/api/onboarding/progress/${userId}/skip`, { method: 'POST' });
        }

        // Show specific step
        function showStep(index) {
            if (index >= steps.length) {
                completeTour();
                return;
            }

            const step = steps[index];
            const overlay = document.getElementById('onboardingOverlay');
            const tooltip = document.getElementById('onboardingTooltip');

            // Update tooltip content
            document.getElementById('stepNumber').textContent = index + 1;
            document.getElementById('currentStep').textContent = index + 1;
            document.getElementById('totalSteps').textContent = steps.length;
            document.getElementById('tooltipTitle').textContent = step.title;
            document.getElementById('tooltipDescription').textContent = step.description;

            // Update button text for last step
            const nextBtn = tooltip.querySelector('.tooltip-next');
            if (index === steps.length - 1) {
                nextBtn.innerHTML = 'Concluir <i class="lucide-check" style="width: 16px; height: 16px;"></i>';
            } else {
                nextBtn.innerHTML = 'Proximo <i class="lucide-arrow-right" style="width: 16px; height: 16px;"></i>';
            }

            // Update progress dots
            const progressContainer = document.getElementById('tooltipProgress');
            progressContainer.innerHTML = steps.map((s, i) => {
                let className = 'progress-dot';
                if (i < index) className += ' completed';
                if (i === index) className += ' active';
                return `<div class="${className}"></div>`;
            }).join('');

            // Show overlay and tooltip
            overlay.classList.add('active');
            tooltip.style.display = 'block';

            // Position tooltip (demo: center of screen for now)
            if (step.position === 'center') {
                tooltip.style.top = '50%';
                tooltip.style.left = '50%';
                tooltip.style.transform = 'translate(-50%, -50%)';
                tooltip.className = 'onboarding-tooltip';
            } else {
                // In real implementation, position relative to target element
                tooltip.style.top = '30%';
                tooltip.style.left = '50%';
                tooltip.style.transform = 'translateX(-50%)';
                tooltip.className = 'onboarding-tooltip position-' + step.position;
            }

            // Track step view
            fetch(`/api/onboarding/progress/${userId}/step/${step.step_id}`, { method: 'POST' });
        }

        // Next step
        function nextStep() {
            currentStepIndex++;
            showStep(currentStepIndex);
        }

        // Complete tour
        function completeTour() {
            document.getElementById('onboardingOverlay').classList.remove('active');
            document.getElementById('onboardingTooltip').style.display = 'none';
            tourActive = false;
            localStorage.setItem('fabrica_onboarding_completed', 'true');

            // Show checklist
            setTimeout(() => {
                toggleChecklist();
            }, 500);
        }

        // Show welcome modal again
        function showWelcome() {
            document.getElementById('welcomeModal').classList.remove('hidden');
        }

        // Toggle checklist panel
        function toggleChecklist() {
            checklistOpen = !checklistOpen;
            document.getElementById('checklistPanel').classList.toggle('active', checklistOpen);
        }

        // Initialize checklist
        function initChecklist() {
            const container = document.getElementById('checklistItems');
            const completed = JSON.parse(localStorage.getItem('fabrica_checklist_completed') || '[]');

            container.innerHTML = checklistItems.map(item => {
                const isCompleted = completed.includes(item.item_id);
                return `
                    <div class="checklist-item ${isCompleted ? 'completed' : ''}"
                         data-id="${item.item_id}"
                         onclick="toggleChecklistItem('${item.item_id}')">
                        <div class="checklist-checkbox">
                            ${isCompleted ? '<i class="lucide-check" style="width: 14px; height: 14px;"></i>' : ''}
                        </div>
                        <div class="checklist-icon">
                            <i class="lucide-${item.icon}" style="width: 18px; height: 18px;"></i>
                        </div>
                        <div class="checklist-content">
                            <h4>${item.title}</h4>
                            <p>${item.description}</p>
                        </div>
                    </div>
                `;
            }).join('');

            updateChecklistProgress();
        }

        // Toggle checklist item
        function toggleChecklistItem(itemId) {
            let completed = JSON.parse(localStorage.getItem('fabrica_checklist_completed') || '[]');

            if (completed.includes(itemId)) {
                completed = completed.filter(id => id !== itemId);
            } else {
                completed.push(itemId);
                fetch(`/api/onboarding/progress/${userId}/checklist/${itemId}`, { method: 'POST' });
            }

            localStorage.setItem('fabrica_checklist_completed', JSON.stringify(completed));
            initChecklist();
        }

        // Update checklist progress
        function updateChecklistProgress() {
            const completed = JSON.parse(localStorage.getItem('fabrica_checklist_completed') || '[]');
            const percentage = (completed.length / checklistItems.length) * 100;

            document.getElementById('checklistProgressBar').style.width = percentage + '%';
            document.getElementById('checklistBadge').textContent = checklistItems.length - completed.length;

            if (completed.length === checklistItems.length) {
                document.getElementById('checklistBadge').style.display = 'none';
            }
        }

        // Initialize on load
        document.addEventListener('DOMContentLoaded', () => {
            checkFirstVisit();
            initChecklist();
        });
    </script>
</body>
</html>'''


@onboarding_router.get("/", response_class=HTMLResponse)
async def get_onboarding_page():
    """Retorna a pagina de onboarding"""
    return HTMLResponse(content=ONBOARDING_TEMPLATE)


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_onboarding_endpoints(app):
    """Registra os endpoints de onboarding no app FastAPI"""
    app.include_router(onboarding_router)

    @app.get("/onboarding", response_class=HTMLResponse)
    async def onboarding_page():
        return HTMLResponse(content=ONBOARDING_TEMPLATE)

    print("[Dashboard] Onboarding endpoints registered")
