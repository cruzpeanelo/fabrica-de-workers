# -*- coding: utf-8 -*-
"""
Glossario e Interface Sem Jargoes - Issue #133
==============================================
Sistema de traducao de termos tecnicos para linguagem simples:
- Traducao de termos tecnicos
- Tooltips explicativos em hover
- Modo "Simplificado" vs "Tecnico"
"""

from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import APIRouter, Query
from fastapi.responses import HTMLResponse, JSONResponse
from pydantic import BaseModel


# =============================================================================
# ROUTER
# =============================================================================

glossary_router = APIRouter(prefix="/api/glossary", tags=["Glossary"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class GlossaryTerm(BaseModel):
    term_id: str
    technical_term: str
    simple_term: str
    description: str
    category: str
    example: Optional[str] = None
    icon: Optional[str] = None


class UserPreference(BaseModel):
    user_id: str
    mode: str = "simplified"  # simplified or technical
    show_tooltips: bool = True


# =============================================================================
# GLOSSARY DATA
# =============================================================================

GLOSSARY_TERMS: Dict[str, Dict[str, Any]] = {
    # Status Terms
    "backlog": {
        "term_id": "backlog",
        "technical_term": "Backlog",
        "simple_term": "Lista de Espera",
        "description": "Tarefas que ainda nao foram iniciadas e aguardam para serem trabalhadas",
        "category": "status",
        "example": "Todas as ideias de novas funcionalidades estao na Lista de Espera",
        "icon": "inbox"
    },
    "ready": {
        "term_id": "ready",
        "technical_term": "Ready",
        "simple_term": "Pronto para Comecar",
        "description": "Tarefas que ja foram planejadas e podem ser iniciadas",
        "category": "status",
        "example": "A tarefa 'Criar pagina de login' esta Pronta para Comecar",
        "icon": "check-circle"
    },
    "in_progress": {
        "term_id": "in_progress",
        "technical_term": "In Progress",
        "simple_term": "Em Andamento",
        "description": "Tarefas que estao sendo trabalhadas agora",
        "category": "status",
        "example": "O desenvolvedor esta trabalhando na tarefa de cadastro",
        "icon": "loader"
    },
    "review": {
        "term_id": "review",
        "technical_term": "Code Review",
        "simple_term": "Em Revisao",
        "description": "O trabalho foi feito e esta sendo verificado por outra pessoa",
        "category": "status",
        "example": "O supervisor esta verificando se o trabalho ficou correto",
        "icon": "eye"
    },
    "testing": {
        "term_id": "testing",
        "technical_term": "Testing/QA",
        "simple_term": "Em Teste",
        "description": "A funcionalidade esta sendo testada para garantir que funciona bem",
        "category": "status",
        "example": "Estamos testando se o botao de compra funciona corretamente",
        "icon": "flask-conical"
    },
    "done": {
        "term_id": "done",
        "technical_term": "Done",
        "simple_term": "Concluido",
        "description": "A tarefa foi completada com sucesso",
        "category": "status",
        "example": "A pagina de contato esta Concluida e funcionando",
        "icon": "check-check"
    },
    "blocked": {
        "term_id": "blocked",
        "technical_term": "Blocked",
        "simple_term": "Impedido",
        "description": "A tarefa nao pode continuar porque depende de algo que ainda nao esta pronto",
        "category": "status",
        "example": "Nao podemos fazer o pagamento porque o contrato ainda nao foi assinado",
        "icon": "alert-circle"
    },

    # Agile Terms
    "sprint": {
        "term_id": "sprint",
        "technical_term": "Sprint",
        "simple_term": "Ciclo de Trabalho",
        "description": "Um periodo fixo (geralmente 2 semanas) onde a equipe trabalha em um conjunto de tarefas",
        "category": "agile",
        "example": "Neste Ciclo de Trabalho vamos entregar 5 funcionalidades novas",
        "icon": "timer"
    },
    "story": {
        "term_id": "story",
        "technical_term": "User Story",
        "simple_term": "Requisito",
        "description": "Uma descricao de algo que o usuario precisa, escrita de forma simples",
        "category": "agile",
        "example": "Como cliente, quero poder salvar meus produtos favoritos",
        "icon": "file-text"
    },
    "epic": {
        "term_id": "epic",
        "technical_term": "Epic",
        "simple_term": "Projeto Grande",
        "description": "Um conjunto de tarefas relacionadas que formam uma funcionalidade maior",
        "category": "agile",
        "example": "O Projeto Grande 'Sistema de Pagamentos' inclui cartao, boleto e PIX",
        "icon": "layers"
    },
    "story_points": {
        "term_id": "story_points",
        "technical_term": "Story Points",
        "simple_term": "Pontos de Esforco",
        "description": "Uma medida de quanto trabalho uma tarefa vai dar (numeros maiores = mais trabalho)",
        "category": "agile",
        "example": "Uma tarefa simples tem 1-2 pontos, uma complexa pode ter 8-13 pontos",
        "icon": "hash"
    },
    "velocity": {
        "term_id": "velocity",
        "technical_term": "Velocity",
        "simple_term": "Ritmo da Equipe",
        "description": "Quantos pontos de trabalho a equipe consegue entregar em cada ciclo",
        "category": "agile",
        "example": "Nossa equipe consegue entregar em media 30 pontos por ciclo",
        "icon": "trending-up"
    },
    "kanban": {
        "term_id": "kanban",
        "technical_term": "Kanban Board",
        "simple_term": "Quadro de Tarefas",
        "description": "Um painel visual que mostra todas as tarefas organizadas por etapa",
        "category": "agile",
        "example": "No Quadro de Tarefas voce ve o que esta parado, em andamento e pronto",
        "icon": "columns"
    },
    "standup": {
        "term_id": "standup",
        "technical_term": "Daily Standup",
        "simple_term": "Reuniao Rapida",
        "description": "Uma reuniao curta (15 min) todo dia para alinhar o que cada um esta fazendo",
        "category": "agile",
        "example": "Na Reuniao Rapida cada um diz o que fez ontem e o que vai fazer hoje",
        "icon": "users"
    },
    "retrospective": {
        "term_id": "retrospective",
        "technical_term": "Retrospective",
        "simple_term": "Reuniao de Melhoria",
        "description": "Uma reuniao no fim do ciclo para discutir o que funcionou e o que pode melhorar",
        "category": "agile",
        "example": "Na Reuniao de Melhoria vamos decidir como evitar atrasos no proximo ciclo",
        "icon": "message-square"
    },

    # Technical Terms
    "deploy": {
        "term_id": "deploy",
        "technical_term": "Deploy",
        "simple_term": "Publicar",
        "description": "Colocar a aplicacao no ar para os usuarios poderem usar",
        "category": "technical",
        "example": "Vamos Publicar a nova versao do site na sexta-feira",
        "icon": "rocket"
    },
    "bug": {
        "term_id": "bug",
        "technical_term": "Bug",
        "simple_term": "Problema/Erro",
        "description": "Algo que nao esta funcionando como deveria",
        "category": "technical",
        "example": "Encontramos um Problema: o botao de login nao funciona no celular",
        "icon": "bug"
    },
    "feature": {
        "term_id": "feature",
        "technical_term": "Feature",
        "simple_term": "Funcionalidade",
        "description": "Uma capacidade nova que o sistema vai ter",
        "category": "technical",
        "example": "A nova Funcionalidade permite exportar relatorios em PDF",
        "icon": "star"
    },
    "api": {
        "term_id": "api",
        "technical_term": "API",
        "simple_term": "Conexao entre Sistemas",
        "description": "Uma forma de um sistema conversar com outro automaticamente",
        "category": "technical",
        "example": "A Conexao com o banco permite consultar o saldo do cliente",
        "icon": "plug"
    },
    "database": {
        "term_id": "database",
        "technical_term": "Database",
        "simple_term": "Banco de Dados",
        "description": "Onde todas as informacoes do sistema ficam guardadas",
        "category": "technical",
        "example": "Os dados dos clientes ficam salvos no Banco de Dados",
        "icon": "database"
    },
    "frontend": {
        "term_id": "frontend",
        "technical_term": "Frontend",
        "simple_term": "Tela/Interface",
        "description": "A parte visual do sistema que o usuario ve e interage",
        "category": "technical",
        "example": "A Tela de cadastro precisa de um campo para telefone",
        "icon": "monitor"
    },
    "backend": {
        "term_id": "backend",
        "technical_term": "Backend",
        "simple_term": "Motor do Sistema",
        "description": "A parte invisivel que processa os dados e faz o sistema funcionar",
        "category": "technical",
        "example": "O Motor do Sistema calcula o frete baseado no CEP",
        "icon": "server"
    },
    "commit": {
        "term_id": "commit",
        "technical_term": "Commit",
        "simple_term": "Salvar Versao",
        "description": "Salvar uma versao do codigo com uma descricao do que foi alterado",
        "category": "technical",
        "example": "Vou Salvar Versao com a mensagem 'Corrigido erro no login'",
        "icon": "save"
    },
    "merge": {
        "term_id": "merge",
        "technical_term": "Merge",
        "simple_term": "Juntar Alteracoes",
        "description": "Combinar o trabalho de diferentes pessoas em um so",
        "category": "technical",
        "example": "Precisamos Juntar as Alteracoes do Joao com as da Maria",
        "icon": "git-merge"
    },
    "pull_request": {
        "term_id": "pull_request",
        "technical_term": "Pull Request",
        "simple_term": "Pedido de Revisao",
        "description": "Pedir para alguem revisar seu trabalho antes de juntar ao projeto principal",
        "category": "technical",
        "example": "Criei um Pedido de Revisao para a nova tela de produtos",
        "icon": "git-pull-request"
    },

    # Metrics Terms
    "lead_time": {
        "term_id": "lead_time",
        "technical_term": "Lead Time",
        "simple_term": "Tempo Total",
        "description": "Quanto tempo passa desde que a tarefa foi criada ate ser entregue",
        "category": "metrics",
        "example": "O Tempo Total medio das nossas tarefas e de 5 dias",
        "icon": "clock"
    },
    "cycle_time": {
        "term_id": "cycle_time",
        "technical_term": "Cycle Time",
        "simple_term": "Tempo de Trabalho",
        "description": "Quanto tempo a equipe realmente trabalha em uma tarefa",
        "category": "metrics",
        "example": "O Tempo de Trabalho foi de 2 dias para essa funcionalidade",
        "icon": "activity"
    },
    "throughput": {
        "term_id": "throughput",
        "technical_term": "Throughput",
        "simple_term": "Entregas por Periodo",
        "description": "Quantas tarefas a equipe entrega em um periodo de tempo",
        "category": "metrics",
        "example": "Nossa equipe faz 8 Entregas por Semana em media",
        "icon": "bar-chart"
    },
    "burndown": {
        "term_id": "burndown",
        "technical_term": "Burndown Chart",
        "simple_term": "Grafico de Progresso",
        "description": "Um grafico que mostra quanto trabalho falta para terminar o ciclo",
        "category": "metrics",
        "example": "O Grafico de Progresso mostra que estamos no caminho certo",
        "icon": "trending-down"
    },

    # Priority Terms
    "urgent": {
        "term_id": "urgent",
        "technical_term": "Urgent/Critical",
        "simple_term": "Urgente",
        "description": "Precisa ser resolvido imediatamente, esta afetando usuarios",
        "category": "priority",
        "example": "O site fora do ar e Urgente e precisa ser corrigido agora",
        "icon": "alert-triangle"
    },
    "high": {
        "term_id": "high",
        "technical_term": "High Priority",
        "simple_term": "Alta Prioridade",
        "description": "Muito importante, deve ser feito logo depois dos urgentes",
        "category": "priority",
        "example": "Corrigir o calculo de frete e Alta Prioridade",
        "icon": "arrow-up"
    },
    "medium": {
        "term_id": "medium",
        "technical_term": "Medium Priority",
        "simple_term": "Prioridade Normal",
        "description": "Importante mas pode aguardar as urgentes serem resolvidas",
        "category": "priority",
        "example": "Melhorar a aparencia do botao e Prioridade Normal",
        "icon": "minus"
    },
    "low": {
        "term_id": "low",
        "technical_term": "Low Priority",
        "simple_term": "Pode Esperar",
        "description": "Seria bom fazer, mas nao e urgente",
        "category": "priority",
        "example": "Adicionar animacao no menu Pode Esperar",
        "icon": "arrow-down"
    }
}


# User preferences storage (use database in production)
user_preferences: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# API ENDPOINTS
# =============================================================================

@glossary_router.get("/terms")
async def get_all_terms(category: Optional[str] = None):
    """Retorna todos os termos do glossario"""
    terms = list(GLOSSARY_TERMS.values())

    if category:
        terms = [t for t in terms if t["category"] == category]

    return {
        "terms": terms,
        "total": len(terms),
        "categories": list(set(t["category"] for t in GLOSSARY_TERMS.values()))
    }


@glossary_router.get("/terms/{term_id}")
async def get_term(term_id: str):
    """Retorna um termo especifico"""
    if term_id not in GLOSSARY_TERMS:
        return JSONResponse(
            status_code=404,
            content={"error": "Termo nao encontrado"}
        )
    return GLOSSARY_TERMS[term_id]


@glossary_router.get("/translate/{technical_term}")
async def translate_term(technical_term: str):
    """Traduz um termo tecnico para simples"""
    term_lower = technical_term.lower().replace(" ", "_").replace("-", "_")

    if term_lower in GLOSSARY_TERMS:
        term = GLOSSARY_TERMS[term_lower]
        return {
            "technical": term["technical_term"],
            "simple": term["simple_term"],
            "description": term["description"]
        }

    # Try partial match
    for key, term in GLOSSARY_TERMS.items():
        if term_lower in key or key in term_lower:
            return {
                "technical": term["technical_term"],
                "simple": term["simple_term"],
                "description": term["description"]
            }

    return {
        "technical": technical_term,
        "simple": technical_term,
        "description": "Termo nao encontrado no glossario"
    }


@glossary_router.get("/preferences/{user_id}")
async def get_user_preferences(user_id: str):
    """Retorna as preferencias do usuario"""
    if user_id not in user_preferences:
        user_preferences[user_id] = {
            "mode": "simplified",
            "show_tooltips": True
        }
    return user_preferences[user_id]


@glossary_router.post("/preferences/{user_id}")
async def update_user_preferences(user_id: str, mode: str = "simplified", show_tooltips: bool = True):
    """Atualiza as preferencias do usuario"""
    user_preferences[user_id] = {
        "mode": mode,
        "show_tooltips": show_tooltips
    }
    return user_preferences[user_id]


@glossary_router.get("/categories")
async def get_categories():
    """Retorna todas as categorias de termos"""
    categories = {}
    for term in GLOSSARY_TERMS.values():
        cat = term["category"]
        if cat not in categories:
            categories[cat] = {"name": cat, "count": 0, "terms": []}
        categories[cat]["count"] += 1
        categories[cat]["terms"].append(term["term_id"])

    return {"categories": list(categories.values())}


# =============================================================================
# HTML TEMPLATE
# =============================================================================

GLOSSARY_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Glossario - Plataforma E</title>
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

        /* Header */
        .header {
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            color: white;
            padding: 24px 32px;
        }

        .header h1 {
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 8px;
        }

        .header p {
            opacity: 0.8;
            font-size: 14px;
        }

        /* Mode Toggle */
        .mode-toggle {
            display: flex;
            gap: 8px;
            margin-top: 16px;
        }

        .mode-btn {
            padding: 8px 16px;
            border-radius: 20px;
            border: none;
            cursor: pointer;
            font-size: 13px;
            font-weight: 500;
            transition: all 0.2s;
        }

        .mode-btn.active {
            background: #FF6C00;
            color: white;
        }

        .mode-btn:not(.active) {
            background: rgba(255, 255, 255, 0.2);
            color: white;
        }

        .mode-btn:not(.active):hover {
            background: rgba(255, 255, 255, 0.3);
        }

        /* Main Content */
        .main {
            padding: 32px;
            max-width: 1200px;
            margin: 0 auto;
        }

        /* Search */
        .search-container {
            margin-bottom: 24px;
        }

        .search-input {
            width: 100%;
            padding: 14px 20px;
            padding-left: 48px;
            border: 2px solid #E5E7EB;
            border-radius: 12px;
            font-size: 15px;
            transition: all 0.2s;
            background: white;
        }

        .search-input:focus {
            outline: none;
            border-color: #003B4A;
        }

        .search-container {
            position: relative;
        }

        .search-icon {
            position: absolute;
            left: 16px;
            top: 50%;
            transform: translateY(-50%);
            color: #9CA3AF;
        }

        /* Category Tabs */
        .category-tabs {
            display: flex;
            gap: 8px;
            margin-bottom: 24px;
            flex-wrap: wrap;
        }

        .category-tab {
            padding: 10px 20px;
            background: white;
            border: 2px solid #E5E7EB;
            border-radius: 10px;
            cursor: pointer;
            font-size: 13px;
            font-weight: 500;
            color: #6B7280;
            transition: all 0.2s;
        }

        .category-tab:hover {
            border-color: #003B4A;
            color: #003B4A;
        }

        .category-tab.active {
            background: #003B4A;
            border-color: #003B4A;
            color: white;
        }

        .category-tab .count {
            background: rgba(0, 0, 0, 0.1);
            padding: 2px 8px;
            border-radius: 10px;
            margin-left: 8px;
            font-size: 11px;
        }

        .category-tab.active .count {
            background: rgba(255, 255, 255, 0.2);
        }

        /* Terms Grid */
        .terms-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
            gap: 20px;
        }

        /* Term Card */
        .term-card {
            background: white;
            border-radius: 16px;
            padding: 24px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
            transition: all 0.2s;
            border: 2px solid transparent;
        }

        .term-card:hover {
            box-shadow: 0 8px 24px rgba(0, 0, 0, 0.08);
            border-color: #E5E7EB;
        }

        .term-header {
            display: flex;
            align-items: flex-start;
            gap: 16px;
            margin-bottom: 16px;
        }

        .term-icon {
            width: 48px;
            height: 48px;
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            border-radius: 12px;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            flex-shrink: 0;
        }

        .term-titles {
            flex: 1;
        }

        .term-simple {
            font-size: 18px;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 4px;
        }

        .term-technical {
            font-size: 13px;
            color: #9CA3AF;
            font-family: monospace;
            background: #F3F4F6;
            padding: 2px 8px;
            border-radius: 4px;
            display: inline-block;
        }

        .term-description {
            color: #6B7280;
            line-height: 1.6;
            margin-bottom: 16px;
        }

        .term-example {
            background: #F0FDF4;
            border-left: 3px solid #10B981;
            padding: 12px 16px;
            border-radius: 0 8px 8px 0;
            font-size: 13px;
            color: #166534;
        }

        .term-example-label {
            font-weight: 600;
            margin-bottom: 4px;
            display: block;
        }

        .term-category {
            display: inline-block;
            padding: 4px 10px;
            background: #EEF2FF;
            color: #4F46E5;
            border-radius: 6px;
            font-size: 11px;
            font-weight: 500;
            text-transform: uppercase;
            margin-top: 12px;
        }

        /* Tooltip Styles (for integration) */
        .glossary-tooltip {
            position: absolute;
            background: white;
            border-radius: 12px;
            padding: 16px;
            max-width: 300px;
            box-shadow: 0 10px 30px rgba(0, 0, 0, 0.15);
            z-index: 10000;
            animation: tooltipFade 0.2s ease;
        }

        @keyframes tooltipFade {
            from { opacity: 0; transform: translateY(5px); }
            to { opacity: 1; transform: translateY(0); }
        }

        .glossary-tooltip .tooltip-arrow {
            position: absolute;
            width: 12px;
            height: 12px;
            background: white;
            transform: rotate(45deg);
        }

        .glossary-tooltip h4 {
            font-size: 14px;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 6px;
        }

        .glossary-tooltip p {
            font-size: 13px;
            color: #6B7280;
            line-height: 1.5;
        }

        /* Empty State */
        .empty-state {
            text-align: center;
            padding: 48px;
            color: #9CA3AF;
        }

        .empty-state-icon {
            font-size: 48px;
            margin-bottom: 16px;
            opacity: 0.5;
        }

        /* Settings Panel */
        .settings-panel {
            background: white;
            border-radius: 12px;
            padding: 20px;
            margin-bottom: 24px;
            display: flex;
            align-items: center;
            justify-content: space-between;
            flex-wrap: wrap;
            gap: 16px;
        }

        .settings-label {
            display: flex;
            align-items: center;
            gap: 12px;
            cursor: pointer;
        }

        .settings-label input[type="checkbox"] {
            width: 20px;
            height: 20px;
            accent-color: #003B4A;
        }

        .settings-label span {
            font-size: 14px;
            color: #4B5563;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Glossario de Termos</h1>
        <p>Entenda os termos tecnicos de forma simples</p>

        <div class="mode-toggle">
            <button class="mode-btn active" id="simplifiedBtn" onclick="setMode('simplified')">
                Modo Simplificado
            </button>
            <button class="mode-btn" id="technicalBtn" onclick="setMode('technical')">
                Modo Tecnico
            </button>
        </div>
    </div>

    <div class="main">
        <div class="settings-panel">
            <label class="settings-label">
                <input type="checkbox" id="showTooltips" checked onchange="toggleTooltips()">
                <span>Mostrar dicas ao passar o mouse sobre termos tecnicos</span>
            </label>
        </div>

        <div class="search-container">
            <i class="lucide-search search-icon" style="width: 20px; height: 20px;"></i>
            <input type="text" class="search-input" id="searchInput"
                   placeholder="Buscar termo..." oninput="filterTerms()">
        </div>

        <div class="category-tabs" id="categoryTabs">
            <button class="category-tab active" data-category="all" onclick="filterByCategory('all')">
                Todos <span class="count" id="count-all">0</span>
            </button>
        </div>

        <div class="terms-grid" id="termsGrid">
            <!-- Terms populated by JS -->
        </div>

        <div class="empty-state" id="emptyState" style="display: none;">
            <div class="empty-state-icon"><i class="lucide-search-x"></i></div>
            <p>Nenhum termo encontrado</p>
        </div>
    </div>

    <script>
        // State
        let currentMode = 'simplified';
        let currentCategory = 'all';
        let showTooltips = true;
        const terms = ''' + str(list(GLOSSARY_TERMS.values())).replace("'", '"').replace("None", "null") + ''';

        // Category translations
        const categoryNames = {
            'status': 'Status',
            'agile': 'Metodologia Agil',
            'technical': 'Termos Tecnicos',
            'metrics': 'Metricas',
            'priority': 'Prioridade'
        };

        // Initialize
        document.addEventListener('DOMContentLoaded', () => {
            loadPreferences();
            initCategories();
            renderTerms();
        });

        // Load user preferences
        function loadPreferences() {
            const savedMode = localStorage.getItem('glossary_mode');
            const savedTooltips = localStorage.getItem('glossary_tooltips');

            if (savedMode) setMode(savedMode);
            if (savedTooltips !== null) {
                showTooltips = savedTooltips === 'true';
                document.getElementById('showTooltips').checked = showTooltips;
            }
        }

        // Set display mode
        function setMode(mode) {
            currentMode = mode;
            localStorage.setItem('glossary_mode', mode);

            document.getElementById('simplifiedBtn').classList.toggle('active', mode === 'simplified');
            document.getElementById('technicalBtn').classList.toggle('active', mode === 'technical');

            renderTerms();
        }

        // Toggle tooltips
        function toggleTooltips() {
            showTooltips = document.getElementById('showTooltips').checked;
            localStorage.setItem('glossary_tooltips', showTooltips);
        }

        // Initialize category tabs
        function initCategories() {
            const categories = {};
            terms.forEach(term => {
                const cat = term.category;
                if (!categories[cat]) categories[cat] = 0;
                categories[cat]++;
            });

            const tabsContainer = document.getElementById('categoryTabs');
            document.getElementById('count-all').textContent = terms.length;

            Object.entries(categories).forEach(([cat, count]) => {
                const btn = document.createElement('button');
                btn.className = 'category-tab';
                btn.dataset.category = cat;
                btn.onclick = () => filterByCategory(cat);
                btn.innerHTML = `${categoryNames[cat] || cat} <span class="count">${count}</span>`;
                tabsContainer.appendChild(btn);
            });
        }

        // Filter by category
        function filterByCategory(category) {
            currentCategory = category;

            document.querySelectorAll('.category-tab').forEach(tab => {
                tab.classList.toggle('active', tab.dataset.category === category);
            });

            renderTerms();
        }

        // Filter by search
        function filterTerms() {
            renderTerms();
        }

        // Render terms
        function renderTerms() {
            const searchQuery = document.getElementById('searchInput').value.toLowerCase();
            const grid = document.getElementById('termsGrid');
            const emptyState = document.getElementById('emptyState');

            let filteredTerms = terms;

            // Filter by category
            if (currentCategory !== 'all') {
                filteredTerms = filteredTerms.filter(t => t.category === currentCategory);
            }

            // Filter by search
            if (searchQuery) {
                filteredTerms = filteredTerms.filter(t =>
                    t.technical_term.toLowerCase().includes(searchQuery) ||
                    t.simple_term.toLowerCase().includes(searchQuery) ||
                    t.description.toLowerCase().includes(searchQuery)
                );
            }

            if (filteredTerms.length === 0) {
                grid.innerHTML = '';
                emptyState.style.display = 'block';
                return;
            }

            emptyState.style.display = 'none';

            grid.innerHTML = filteredTerms.map(term => {
                const primaryTerm = currentMode === 'simplified' ? term.simple_term : term.technical_term;
                const secondaryTerm = currentMode === 'simplified' ? term.technical_term : term.simple_term;

                return `
                    <div class="term-card">
                        <div class="term-header">
                            <div class="term-icon">
                                <i class="lucide-${term.icon || 'file-text'}" style="width: 24px; height: 24px;"></i>
                            </div>
                            <div class="term-titles">
                                <div class="term-simple">${primaryTerm}</div>
                                <div class="term-technical">${secondaryTerm}</div>
                            </div>
                        </div>
                        <p class="term-description">${term.description}</p>
                        ${term.example ? `
                            <div class="term-example">
                                <span class="term-example-label">Exemplo:</span>
                                ${term.example}
                            </div>
                        ` : ''}
                        <span class="term-category">${categoryNames[term.category] || term.category}</span>
                    </div>
                `;
            }).join('');
        }

        // Global tooltip function for integration
        window.showGlossaryTooltip = function(element, termId) {
            if (!showTooltips) return;

            const term = terms.find(t => t.term_id === termId);
            if (!term) return;

            const tooltip = document.createElement('div');
            tooltip.className = 'glossary-tooltip';
            tooltip.innerHTML = `
                <h4>${currentMode === 'simplified' ? term.simple_term : term.technical_term}</h4>
                <p>${term.description}</p>
            `;

            const rect = element.getBoundingClientRect();
            tooltip.style.left = rect.left + 'px';
            tooltip.style.top = (rect.bottom + 8) + 'px';

            document.body.appendChild(tooltip);

            element.addEventListener('mouseleave', () => {
                tooltip.remove();
            }, { once: true });
        };
    </script>
</body>
</html>'''


@glossary_router.get("/", response_class=HTMLResponse)
async def get_glossary_page():
    """Retorna a pagina do glossario"""
    return HTMLResponse(content=GLOSSARY_TEMPLATE)


# =============================================================================
# HELPER FUNCTIONS FOR INTEGRATION
# =============================================================================

def get_simple_term(technical_term: str) -> str:
    """Retorna o termo simples para um termo tecnico"""
    term_lower = technical_term.lower().replace(" ", "_").replace("-", "_")
    if term_lower in GLOSSARY_TERMS:
        return GLOSSARY_TERMS[term_lower]["simple_term"]
    return technical_term


def translate_text(text: str, mode: str = "simplified") -> str:
    """Traduz um texto substituindo termos tecnicos"""
    if mode != "simplified":
        return text

    result = text
    for term_id, term in GLOSSARY_TERMS.items():
        # Replace technical terms with simple ones
        tech = term["technical_term"]
        simple = term["simple_term"]
        result = result.replace(tech, simple)

    return result


def get_tooltip_data(term_id: str) -> Optional[Dict[str, str]]:
    """Retorna dados para tooltip de um termo"""
    if term_id not in GLOSSARY_TERMS:
        return None

    term = GLOSSARY_TERMS[term_id]
    return {
        "title": term["simple_term"],
        "description": term["description"],
        "icon": term.get("icon", "info")
    }


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_glossary_endpoints(app):
    """Registra os endpoints do glossario no app FastAPI"""
    app.include_router(glossary_router)

    @app.get("/glossary", response_class=HTMLResponse)
    async def glossary_page():
        return HTMLResponse(content=GLOSSARY_TEMPLATE)

    print("[Dashboard] Glossary endpoints registered")
