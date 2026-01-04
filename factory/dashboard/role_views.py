# -*- coding: utf-8 -*-
"""
Role Views - Visoes e Dashboards por Perfil de Usuario
========================================================

Issue #111 - Visoes e Dashboards por Perfil de Usuario

Este modulo fornece dashboards personalizados por role:
- Admin: Visao completa com todas as metricas e controles
- Manager: Gestao de projetos, sprints e equipe
- Developer: Tarefas, codigo e execucao
- Viewer: Apenas visualizacao de progresso

Componentes:
- get_dashboard_for_role(): Retorna dashboard baseado na role
- admin_dashboard(): Dashboard completo para admins
- manager_dashboard(): Dashboard de gestao
- developer_dashboard(): Dashboard de desenvolvimento
- viewer_dashboard(): Dashboard somente leitura
"""

from datetime import datetime
from typing import Dict, Any, List, Optional

# Database
from factory.database.connection import SessionLocal


# =============================================================================
# WIDGET CONFIGURATIONS
# =============================================================================

# Widgets disponiveis por role
ROLE_WIDGETS = {
    "ADMIN": [
        {"id": "platform_health", "title": "Saude da Plataforma", "type": "status", "size": "full"},
        {"id": "users_stats", "title": "Usuarios", "type": "metric", "size": "quarter"},
        {"id": "workers_status", "title": "Workers", "type": "metric", "size": "quarter"},
        {"id": "billing_summary", "title": "Billing", "type": "metric", "size": "quarter"},
        {"id": "storage_usage", "title": "Storage", "type": "metric", "size": "quarter"},
        {"id": "tenants_list", "title": "Tenants Ativos", "type": "table", "size": "half"},
        {"id": "audit_logs", "title": "Logs de Auditoria", "type": "table", "size": "half"},
        {"id": "system_alerts", "title": "Alertas do Sistema", "type": "list", "size": "half"},
        {"id": "api_usage", "title": "Uso da API", "type": "chart", "size": "half"},
    ],
    "MANAGER": [
        {"id": "sprint_progress", "title": "Progresso do Sprint", "type": "progress", "size": "full"},
        {"id": "stories_by_status", "title": "Stories por Status", "type": "kanban", "size": "full"},
        {"id": "velocity_chart", "title": "Velocidade", "type": "chart", "size": "half"},
        {"id": "burndown", "title": "Burndown", "type": "chart", "size": "half"},
        {"id": "team_workload", "title": "Carga da Equipe", "type": "table", "size": "half"},
        {"id": "blockers", "title": "Blockers", "type": "list", "size": "half"},
        {"id": "upcoming_deadlines", "title": "Prazos", "type": "list", "size": "third"},
        {"id": "story_points", "title": "Story Points", "type": "metric", "size": "third"},
        {"id": "completion_rate", "title": "Taxa de Conclusao", "type": "metric", "size": "third"},
    ],
    "DEVELOPER": [
        {"id": "my_tasks", "title": "Minhas Tarefas", "type": "kanban", "size": "full"},
        {"id": "current_story", "title": "Story Atual", "type": "detail", "size": "half"},
        {"id": "recent_code", "title": "Codigo Recente", "type": "code", "size": "half"},
        {"id": "test_results", "title": "Resultados de Testes", "type": "status", "size": "third"},
        {"id": "lint_issues", "title": "Issues de Lint", "type": "list", "size": "third"},
        {"id": "pr_status", "title": "Pull Requests", "type": "list", "size": "third"},
        {"id": "job_history", "title": "Historico de Jobs", "type": "table", "size": "full"},
    ],
    "VIEWER": [
        {"id": "project_progress", "title": "Progresso do Projeto", "type": "progress", "size": "full"},
        {"id": "stories_summary", "title": "Resumo de Stories", "type": "metrics", "size": "full"},
        {"id": "timeline", "title": "Timeline", "type": "timeline", "size": "full"},
        {"id": "recent_updates", "title": "Atualizacoes Recentes", "type": "list", "size": "half"},
        {"id": "milestones", "title": "Marcos", "type": "list", "size": "half"},
    ],
    "STAKEHOLDER": [
        {"id": "executive_summary", "title": "Resumo Executivo", "type": "summary", "size": "full"},
        {"id": "kpis", "title": "KPIs", "type": "metrics", "size": "full"},
        {"id": "budget_vs_actual", "title": "Orcamento vs Real", "type": "chart", "size": "half"},
        {"id": "risk_matrix", "title": "Matriz de Riscos", "type": "matrix", "size": "half"},
        {"id": "deliverables", "title": "Entregaveis", "type": "list", "size": "full"},
    ]
}


# =============================================================================
# DATA FUNCTIONS
# =============================================================================

def get_widgets_for_role(role: str) -> List[Dict[str, Any]]:
    """
    Retorna widgets configurados para a role

    Args:
        role: Nome da role (ADMIN, MANAGER, DEVELOPER, VIEWER)

    Returns:
        Lista de widgets configurados
    """
    return ROLE_WIDGETS.get(role.upper(), ROLE_WIDGETS["VIEWER"])


def get_user_preferences(user_id: int) -> Dict[str, Any]:
    """
    Retorna preferencias de dashboard do usuario

    Args:
        user_id: ID do usuario

    Returns:
        Dict com preferencias
    """
    # Em producao, buscaria do banco de dados
    return {
        "theme": "light",
        "layout": "default",
        "widgets_order": [],
        "hidden_widgets": [],
        "refresh_interval": 30
    }


def save_user_preferences(user_id: int, preferences: Dict[str, Any]) -> bool:
    """
    Salva preferencias de dashboard do usuario

    Args:
        user_id: ID do usuario
        preferences: Dict com preferencias

    Returns:
        True se salvo com sucesso
    """
    # Em producao, salvaria no banco de dados
    return True


def get_dashboard_data(role: str, project_id: Optional[str] = None) -> Dict[str, Any]:
    """
    Retorna dados para o dashboard baseado na role

    Args:
        role: Nome da role
        project_id: ID do projeto (opcional)

    Returns:
        Dict com dados do dashboard
    """
    if role.upper() == "ADMIN":
        return get_admin_dashboard_data()
    elif role.upper() == "MANAGER":
        return get_manager_dashboard_data(project_id)
    elif role.upper() == "DEVELOPER":
        return get_developer_dashboard_data(project_id)
    elif role.upper() == "STAKEHOLDER":
        return get_stakeholder_dashboard_data(project_id)
    else:
        return get_viewer_dashboard_data(project_id)


def get_admin_dashboard_data() -> Dict[str, Any]:
    """Retorna dados para dashboard de admin"""
    return {
        "platform_health": {
            "status": "healthy",
            "uptime_pct": 99.9,
            "services": [
                {"name": "API", "status": "up"},
                {"name": "Workers", "status": "up"},
                {"name": "Database", "status": "up"},
                {"name": "Redis", "status": "up"}
            ]
        },
        "users_stats": {
            "total": 156,
            "active": 142,
            "new_this_week": 8
        },
        "workers_status": {
            "total": 5,
            "idle": 3,
            "busy": 2,
            "offline": 0
        },
        "billing_summary": {
            "mtd": 2450.80,
            "projected": 3200.00,
            "vs_last_month": 12.5
        },
        "storage_usage": {
            "used_gb": 125.6,
            "total_gb": 500,
            "pct": 25.1
        },
        "system_alerts": [
            {"level": "warning", "message": "Worker-03 com alta latencia"},
            {"level": "info", "message": "Backup diario concluido"}
        ]
    }


def get_manager_dashboard_data(project_id: Optional[str] = None) -> Dict[str, Any]:
    """Retorna dados para dashboard de manager"""
    return {
        "sprint_progress": {
            "name": "Sprint 23",
            "start_date": "2024-12-16",
            "end_date": "2024-12-30",
            "total_points": 34,
            "completed_points": 21,
            "pct": 62
        },
        "stories_by_status": {
            "backlog": 5,
            "ready": 3,
            "in_progress": 4,
            "review": 2,
            "testing": 1,
            "done": 8
        },
        "team_workload": [
            {"name": "Dev-01", "assigned": 13, "completed": 8},
            {"name": "Dev-02", "assigned": 11, "completed": 7},
            {"name": "AI-Agent", "assigned": 10, "completed": 6}
        ],
        "blockers": [
            {"story_id": "STR-0045", "reason": "Aguardando API externa"},
            {"story_id": "STR-0048", "reason": "Dependencia de design"}
        ],
        "velocity_history": [21, 24, 19, 26, 23, 28],
        "burndown": {
            "ideal": [34, 30, 26, 22, 18, 14, 10, 6, 2, 0],
            "actual": [34, 31, 28, 24, 21, 18, 13]
        }
    }


def get_developer_dashboard_data(project_id: Optional[str] = None) -> Dict[str, Any]:
    """Retorna dados para dashboard de developer"""
    return {
        "my_tasks": [
            {"task_id": "STSK-0101", "title": "Implementar autenticacao", "status": "in_progress", "story_id": "STR-0045"},
            {"task_id": "STSK-0102", "title": "Criar testes unitarios", "status": "pending", "story_id": "STR-0045"},
            {"task_id": "STSK-0103", "title": "Documentar API", "status": "pending", "story_id": "STR-0046"}
        ],
        "current_story": {
            "story_id": "STR-0045",
            "title": "Sistema de Login",
            "progress": 60,
            "remaining_tasks": 2
        },
        "test_results": {
            "total": 45,
            "passed": 42,
            "failed": 2,
            "skipped": 1
        },
        "lint_issues": [
            {"file": "auth.py", "line": 45, "message": "Line too long"},
            {"file": "models.py", "line": 12, "message": "Missing docstring"}
        ],
        "recent_code": {
            "files_modified": 5,
            "lines_added": 234,
            "lines_removed": 45
        },
        "job_history": [
            {"job_id": "JOB-0234", "status": "completed", "duration": "3m 45s"},
            {"job_id": "JOB-0233", "status": "completed", "duration": "2m 12s"},
            {"job_id": "JOB-0232", "status": "failed", "duration": "5m 30s"}
        ]
    }


def get_viewer_dashboard_data(project_id: Optional[str] = None) -> Dict[str, Any]:
    """Retorna dados para dashboard de viewer"""
    return {
        "project_progress": {
            "overall": 65,
            "phases": [
                {"name": "Planejamento", "pct": 100},
                {"name": "Desenvolvimento", "pct": 75},
                {"name": "Testes", "pct": 30},
                {"name": "Deploy", "pct": 0}
            ]
        },
        "stories_summary": {
            "total": 23,
            "done": 15,
            "in_progress": 5,
            "remaining": 3
        },
        "recent_updates": [
            {"date": "2024-12-28", "message": "Story STR-0045 concluida"},
            {"date": "2024-12-27", "message": "Sprint 23 iniciado"},
            {"date": "2024-12-26", "message": "Novo milestone definido"}
        ],
        "milestones": [
            {"name": "MVP", "date": "2025-01-15", "status": "on_track"},
            {"name": "Beta", "date": "2025-02-01", "status": "on_track"},
            {"name": "Launch", "date": "2025-03-01", "status": "planned"}
        ]
    }


def get_stakeholder_dashboard_data(project_id: Optional[str] = None) -> Dict[str, Any]:
    """Retorna dados para dashboard de stakeholder"""
    return {
        "executive_summary": {
            "status": "on_track",
            "progress": 65,
            "budget_status": "within_budget",
            "risk_level": "low"
        },
        "kpis": [
            {"name": "Velocidade", "value": 24, "target": 25, "unit": "pts/sprint"},
            {"name": "Qualidade", "value": 98, "target": 95, "unit": "%"},
            {"name": "Time to Market", "value": 45, "target": 60, "unit": "dias"}
        ],
        "budget_vs_actual": {
            "budget": 50000,
            "spent": 28500,
            "remaining": 21500,
            "projected_total": 48000
        },
        "deliverables": [
            {"name": "Modulo Autenticacao", "due": "2025-01-10", "status": "completed"},
            {"name": "Dashboard Usuario", "due": "2025-01-20", "status": "in_progress"},
            {"name": "API Integracao", "due": "2025-02-01", "status": "planned"}
        ]
    }


# =============================================================================
# UI COMPONENTS (HTML/JavaScript)
# =============================================================================

def generate_role_dashboard_html(role: str, user_id: int = None, project_id: str = None) -> str:
    """
    Gera HTML do dashboard baseado na role

    Args:
        role: Nome da role
        user_id: ID do usuario
        project_id: ID do projeto

    Returns:
        HTML completo
    """
    widgets = get_widgets_for_role(role)

    return f'''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Dashboard - Plataforma E</title>
    <style>
        :root {{
            --primary: #003B4A;
            --secondary: #FF6C00;
            --success: #10B981;
            --warning: #F59E0B;
            --danger: #EF4444;
            --info: #3B82F6;
            --bg: #F3F4F6;
            --surface: #FFFFFF;
            --text: #1F2937;
            --text-secondary: #6B7280;
            --border: #E5E7EB;
        }}

        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}

        body {{
            font-family: 'Inter', 'Segoe UI', sans-serif;
            background: var(--bg);
            color: var(--text);
            line-height: 1.5;
        }}

        .dashboard {{
            max-width: 1600px;
            margin: 0 auto;
            padding: 24px;
        }}

        .header {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }}

        .header-left {{
            display: flex;
            align-items: center;
            gap: 16px;
        }}

        .header h1 {{
            font-size: 24px;
            color: var(--primary);
        }}

        .role-badge {{
            padding: 6px 12px;
            background: var(--primary);
            color: white;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }}

        .header-actions {{
            display: flex;
            gap: 12px;
        }}

        .btn {{
            display: inline-flex;
            align-items: center;
            gap: 8px;
            padding: 10px 20px;
            border: none;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }}

        .btn-primary {{
            background: var(--primary);
            color: white;
        }}

        .btn-outline {{
            background: transparent;
            border: 1px solid var(--border);
            color: var(--text);
        }}

        /* Widget Grid */
        .widget-grid {{
            display: grid;
            grid-template-columns: repeat(12, 1fr);
            gap: 20px;
        }}

        .widget {{
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            overflow: hidden;
        }}

        .widget.full {{ grid-column: span 12; }}
        .widget.half {{ grid-column: span 6; }}
        .widget.third {{ grid-column: span 4; }}
        .widget.quarter {{ grid-column: span 3; }}

        .widget-header {{
            padding: 16px 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .widget-title {{
            font-size: 14px;
            font-weight: 600;
            color: var(--text);
        }}

        .widget-actions {{
            display: flex;
            gap: 8px;
        }}

        .widget-action {{
            width: 28px;
            height: 28px;
            border: none;
            background: transparent;
            border-radius: 4px;
            cursor: pointer;
            color: var(--text-secondary);
            display: flex;
            align-items: center;
            justify-content: center;
        }}

        .widget-action:hover {{
            background: var(--bg);
        }}

        .widget-body {{
            padding: 20px;
        }}

        /* Metric Widget */
        .metric-value {{
            font-size: 36px;
            font-weight: 700;
            color: var(--primary);
            margin-bottom: 4px;
        }}

        .metric-label {{
            font-size: 14px;
            color: var(--text-secondary);
        }}

        .metric-trend {{
            display: inline-flex;
            align-items: center;
            gap: 4px;
            font-size: 12px;
            margin-top: 8px;
            padding: 4px 8px;
            border-radius: 12px;
        }}

        .metric-trend.up {{
            background: #D1FAE5;
            color: #065F46;
        }}

        .metric-trend.down {{
            background: #FEE2E2;
            color: #991B1B;
        }}

        /* Progress Widget */
        .progress-container {{
            margin-bottom: 20px;
        }}

        .progress-header {{
            display: flex;
            justify-content: space-between;
            margin-bottom: 8px;
        }}

        .progress-title {{
            font-weight: 500;
        }}

        .progress-value {{
            font-weight: 600;
            color: var(--primary);
        }}

        .progress-bar {{
            height: 12px;
            background: var(--bg);
            border-radius: 6px;
            overflow: hidden;
        }}

        .progress-fill {{
            height: 100%;
            background: var(--primary);
            border-radius: 6px;
            transition: width 0.5s;
        }}

        /* Status Widget */
        .status-grid {{
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 12px;
        }}

        .status-item {{
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px;
            background: var(--bg);
            border-radius: 8px;
        }}

        .status-dot {{
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }}

        .status-dot.up {{ background: var(--success); }}
        .status-dot.down {{ background: var(--danger); }}
        .status-dot.warning {{ background: var(--warning); }}

        .status-name {{
            flex: 1;
            font-size: 14px;
        }}

        .status-value {{
            font-weight: 500;
        }}

        /* List Widget */
        .list-items {{
            display: flex;
            flex-direction: column;
            gap: 12px;
        }}

        .list-item {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 12px;
            background: var(--bg);
            border-radius: 8px;
        }}

        .list-item-content {{
            flex: 1;
        }}

        .list-item-title {{
            font-weight: 500;
            margin-bottom: 4px;
        }}

        .list-item-subtitle {{
            font-size: 12px;
            color: var(--text-secondary);
        }}

        /* Table Widget */
        .data-table {{
            width: 100%;
            border-collapse: collapse;
        }}

        .data-table th,
        .data-table td {{
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }}

        .data-table th {{
            font-weight: 600;
            font-size: 12px;
            color: var(--text-secondary);
            text-transform: uppercase;
        }}

        /* Kanban Preview */
        .kanban-preview {{
            display: flex;
            gap: 16px;
            overflow-x: auto;
            padding-bottom: 8px;
        }}

        .kanban-column {{
            flex: 0 0 120px;
            text-align: center;
        }}

        .kanban-count {{
            font-size: 28px;
            font-weight: 700;
            color: var(--primary);
        }}

        .kanban-label {{
            font-size: 12px;
            color: var(--text-secondary);
        }}

        /* Customize Button */
        .customize-panel {{
            display: none;
            position: fixed;
            top: 0;
            right: 0;
            width: 320px;
            height: 100vh;
            background: var(--surface);
            box-shadow: -4px 0 20px rgba(0,0,0,0.15);
            z-index: 1000;
            padding: 24px;
            overflow-y: auto;
        }}

        .customize-panel.active {{
            display: block;
        }}

        .customize-header {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }}

        .customize-title {{
            font-size: 18px;
            font-weight: 600;
        }}

        .widget-toggle {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 12px;
            background: var(--bg);
            border-radius: 8px;
            margin-bottom: 8px;
        }}

        .widget-toggle-label {{
            font-size: 14px;
        }}

        .toggle-switch {{
            width: 44px;
            height: 24px;
            background: var(--border);
            border-radius: 12px;
            position: relative;
            cursor: pointer;
            transition: all 0.2s;
        }}

        .toggle-switch.active {{
            background: var(--primary);
        }}

        .toggle-switch::after {{
            content: '';
            width: 20px;
            height: 20px;
            background: white;
            border-radius: 50%;
            position: absolute;
            top: 2px;
            left: 2px;
            transition: all 0.2s;
        }}

        .toggle-switch.active::after {{
            left: 22px;
        }}

        /* Responsive */
        @media (max-width: 1200px) {{
            .widget.half {{ grid-column: span 12; }}
            .widget.third {{ grid-column: span 6; }}
            .widget.quarter {{ grid-column: span 6; }}
        }}

        @media (max-width: 768px) {{
            .widget.third {{ grid-column: span 12; }}
            .widget.quarter {{ grid-column: span 12; }}
        }}
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <div class="header-left">
                <h1>Dashboard</h1>
                <span class="role-badge">{role.upper()}</span>
            </div>
            <div class="header-actions">
                <button class="btn btn-outline" onclick="toggleCustomize()">
                    Personalizar
                </button>
                <button class="btn btn-outline" onclick="refreshDashboard()">
                    Atualizar
                </button>
            </div>
        </div>

        <div class="widget-grid" id="widget-grid">
            <!-- Widgets will be rendered by JavaScript -->
        </div>
    </div>

    <!-- Customize Panel -->
    <div class="customize-panel" id="customize-panel">
        <div class="customize-header">
            <h3 class="customize-title">Personalizar Dashboard</h3>
            <button class="widget-action" onclick="toggleCustomize()">X</button>
        </div>
        <div id="widget-toggles">
            <!-- Widget toggles will be rendered by JavaScript -->
        </div>
        <button class="btn btn-primary" style="width: 100%; margin-top: 20px;" onclick="savePreferences()">
            Salvar Preferencias
        </button>
    </div>

    <script>
        const ROLE = "{role.upper()}";
        const USER_ID = {user_id or 0};
        const PROJECT_ID = "{project_id or ''}";

        const WIDGETS = {json.dumps([w for w in widgets])};

        let activeWidgets = WIDGETS.map(w => w.id);

        // Load dashboard on page load
        document.addEventListener('DOMContentLoaded', () => {{
            renderWidgets();
            renderWidgetToggles();
            loadDashboardData();
        }});

        function renderWidgets() {{
            const grid = document.getElementById('widget-grid');
            grid.innerHTML = WIDGETS.filter(w => activeWidgets.includes(w.id)).map(widget => `
                <div class="widget ${{widget.size}}" id="widget-${{widget.id}}" data-type="${{widget.type}}">
                    <div class="widget-header">
                        <h3 class="widget-title">${{widget.title}}</h3>
                        <div class="widget-actions">
                            <button class="widget-action" title="Atualizar" onclick="refreshWidget('${{widget.id}}')">
                                <svg width="16" height="16" fill="currentColor" viewBox="0 0 20 20">
                                    <path fill-rule="evenodd" d="M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1z" clip-rule="evenodd"/>
                                </svg>
                            </button>
                        </div>
                    </div>
                    <div class="widget-body" id="widget-body-${{widget.id}}">
                        <div style="text-align: center; color: var(--text-secondary);">Carregando...</div>
                    </div>
                </div>
            `).join('');
        }}

        function renderWidgetToggles() {{
            const container = document.getElementById('widget-toggles');
            container.innerHTML = WIDGETS.map(widget => `
                <div class="widget-toggle">
                    <span class="widget-toggle-label">${{widget.title}}</span>
                    <div class="toggle-switch ${{activeWidgets.includes(widget.id) ? 'active' : ''}}"
                         onclick="toggleWidget('${{widget.id}}')"></div>
                </div>
            `).join('');
        }}

        function toggleWidget(widgetId) {{
            if (activeWidgets.includes(widgetId)) {{
                activeWidgets = activeWidgets.filter(id => id !== widgetId);
            }} else {{
                activeWidgets.push(widgetId);
            }}
            renderWidgets();
            renderWidgetToggles();
            loadDashboardData();
        }}

        function toggleCustomize() {{
            document.getElementById('customize-panel').classList.toggle('active');
        }}

        async function loadDashboardData() {{
            try {{
                const res = await fetch(`/api/dashboard/data?role=${{ROLE}}&project_id=${{PROJECT_ID}}`);
                const data = await res.json();
                updateWidgets(data);
            }} catch (error) {{
                console.error('Error loading dashboard data:', error);
                // Use mock data
                updateWidgets(getMockData());
            }}
        }}

        function getMockData() {{
            return {{
                users_stats: {{ total: 156, active: 142, new_this_week: 8 }},
                workers_status: {{ total: 5, idle: 3, busy: 2, offline: 0 }},
                billing_summary: {{ mtd: 2450.80, projected: 3200, vs_last_month: 12.5 }},
                storage_usage: {{ used_gb: 125.6, total_gb: 500, pct: 25.1 }},
                sprint_progress: {{ name: 'Sprint 23', total_points: 34, completed_points: 21, pct: 62 }},
                stories_by_status: {{ backlog: 5, ready: 3, in_progress: 4, review: 2, testing: 1, done: 8 }},
                project_progress: {{ overall: 65 }},
                stories_summary: {{ total: 23, done: 15, in_progress: 5, remaining: 3 }}
            }};
        }}

        function updateWidgets(data) {{
            // Update each widget based on its type
            WIDGETS.forEach(widget => {{
                const body = document.getElementById(`widget-body-${{widget.id}}`);
                if (!body) return;

                switch (widget.type) {{
                    case 'metric':
                        renderMetricWidget(body, widget.id, data);
                        break;
                    case 'progress':
                        renderProgressWidget(body, widget.id, data);
                        break;
                    case 'status':
                        renderStatusWidget(body, widget.id, data);
                        break;
                    case 'kanban':
                        renderKanbanWidget(body, widget.id, data);
                        break;
                    case 'list':
                        renderListWidget(body, widget.id, data);
                        break;
                    case 'table':
                        renderTableWidget(body, widget.id, data);
                        break;
                    default:
                        body.innerHTML = '<div style="color: var(--text-secondary);">Widget em desenvolvimento</div>';
                }}
            }});
        }}

        function renderMetricWidget(container, widgetId, data) {{
            let value = '-';
            let label = '';
            let trend = '';

            switch (widgetId) {{
                case 'users_stats':
                    value = data.users_stats?.total || 0;
                    label = 'usuarios totais';
                    trend = `+${{data.users_stats?.new_this_week || 0}} esta semana`;
                    break;
                case 'workers_status':
                    value = (data.workers_status?.idle || 0) + (data.workers_status?.busy || 0);
                    label = 'workers ativos';
                    break;
                case 'billing_summary':
                    value = `R$ ${{(data.billing_summary?.mtd || 0).toLocaleString()}}`;
                    label = 'gasto no mes';
                    trend = `${{data.billing_summary?.vs_last_month > 0 ? '+' : ''}}${{data.billing_summary?.vs_last_month || 0}}%`;
                    break;
                case 'storage_usage':
                    value = `${{(data.storage_usage?.used_gb || 0).toFixed(1)}} GB`;
                    label = 'armazenamento';
                    break;
                case 'story_points':
                    value = data.sprint_progress?.completed_points || 0;
                    label = 'story points entregues';
                    break;
                case 'completion_rate':
                    value = `${{data.sprint_progress?.pct || 0}}%`;
                    label = 'taxa de conclusao';
                    break;
            }}

            container.innerHTML = `
                <div class="metric-value">${{value}}</div>
                <div class="metric-label">${{label}}</div>
                ${{trend ? `<div class="metric-trend up">${{trend}}</div>` : ''}}
            `;
        }}

        function renderProgressWidget(container, widgetId, data) {{
            let progress = data.sprint_progress || data.project_progress || {{ pct: 0 }};

            container.innerHTML = `
                <div class="progress-container">
                    <div class="progress-header">
                        <span class="progress-title">${{progress.name || 'Progresso'}}</span>
                        <span class="progress-value">${{progress.pct || progress.overall || 0}}%</span>
                    </div>
                    <div class="progress-bar">
                        <div class="progress-fill" style="width: ${{progress.pct || progress.overall || 0}}%"></div>
                    </div>
                </div>
            `;
        }}

        function renderStatusWidget(container, widgetId, data) {{
            let items = [];

            if (widgetId === 'platform_health') {{
                items = [
                    {{ name: 'API', status: 'up', value: 'Online' }},
                    {{ name: 'Workers', status: 'up', value: 'Online' }},
                    {{ name: 'Database', status: 'up', value: 'Online' }},
                    {{ name: 'Redis', status: 'up', value: 'Online' }}
                ];
            }} else if (widgetId === 'test_results') {{
                const tests = data.test_results || {{ passed: 42, failed: 2, total: 45 }};
                items = [
                    {{ name: 'Passou', status: 'up', value: tests.passed }},
                    {{ name: 'Falhou', status: tests.failed > 0 ? 'down' : 'up', value: tests.failed }}
                ];
            }}

            container.innerHTML = `
                <div class="status-grid">
                    ${{items.map(item => `
                        <div class="status-item">
                            <div class="status-dot ${{item.status}}"></div>
                            <span class="status-name">${{item.name}}</span>
                            <span class="status-value">${{item.value}}</span>
                        </div>
                    `).join('')}}
                </div>
            `;
        }}

        function renderKanbanWidget(container, widgetId, data) {{
            const statuses = data.stories_by_status || {{ backlog: 5, ready: 3, in_progress: 4, review: 2, testing: 1, done: 8 }};

            container.innerHTML = `
                <div class="kanban-preview">
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.backlog || 0}}</div>
                        <div class="kanban-label">Backlog</div>
                    </div>
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.ready || 0}}</div>
                        <div class="kanban-label">Ready</div>
                    </div>
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.in_progress || 0}}</div>
                        <div class="kanban-label">In Progress</div>
                    </div>
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.review || 0}}</div>
                        <div class="kanban-label">Review</div>
                    </div>
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.testing || 0}}</div>
                        <div class="kanban-label">Testing</div>
                    </div>
                    <div class="kanban-column">
                        <div class="kanban-count">${{statuses.done || 0}}</div>
                        <div class="kanban-label">Done</div>
                    </div>
                </div>
            `;
        }}

        function renderListWidget(container, widgetId, data) {{
            let items = [];

            if (widgetId === 'system_alerts') {{
                items = [
                    {{ title: 'Worker-03 com alta latencia', subtitle: 'Ha 5 minutos' }},
                    {{ title: 'Backup diario concluido', subtitle: 'Ha 2 horas' }}
                ];
            }} else if (widgetId === 'blockers') {{
                items = [
                    {{ title: 'STR-0045', subtitle: 'Aguardando API externa' }},
                    {{ title: 'STR-0048', subtitle: 'Dependencia de design' }}
                ];
            }} else if (widgetId === 'recent_updates') {{
                items = [
                    {{ title: 'Story STR-0045 concluida', subtitle: '28/12/2024' }},
                    {{ title: 'Sprint 23 iniciado', subtitle: '27/12/2024' }}
                ];
            }}

            container.innerHTML = `
                <div class="list-items">
                    ${{items.map(item => `
                        <div class="list-item">
                            <div class="list-item-content">
                                <div class="list-item-title">${{item.title}}</div>
                                <div class="list-item-subtitle">${{item.subtitle}}</div>
                            </div>
                        </div>
                    `).join('')}}
                </div>
            `;
        }}

        function renderTableWidget(container, widgetId, data) {{
            container.innerHTML = `
                <table class="data-table">
                    <thead>
                        <tr>
                            <th>Item</th>
                            <th>Status</th>
                            <th>Valor</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td colspan="3" style="text-align: center; color: var(--text-secondary);">
                                Dados nao disponiveis
                            </td>
                        </tr>
                    </tbody>
                </table>
            `;
        }}

        function refreshWidget(widgetId) {{
            loadDashboardData();
        }}

        function refreshDashboard() {{
            loadDashboardData();
        }}

        async function savePreferences() {{
            try {{
                await fetch('/api/dashboard/preferences', {{
                    method: 'POST',
                    headers: {{ 'Content-Type': 'application/json' }},
                    body: JSON.stringify({{
                        user_id: USER_ID,
                        active_widgets: activeWidgets
                    }})
                }});
                alert('Preferencias salvas!');
                toggleCustomize();
            }} catch (error) {{
                console.error('Error saving preferences:', error);
                alert('Preferencias salvas localmente');
                toggleCustomize();
            }}
        }}
    </script>
</body>
</html>
'''


# =============================================================================
# API ENDPOINTS
# =============================================================================

def register_role_views_endpoints(app):
    """
    Registra endpoints de role views no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter
    from fastapi.responses import HTMLResponse
    import json

    router = APIRouter(prefix="/api/dashboard", tags=["role-views"])

    @router.get("/role/{role}", response_class=HTMLResponse)
    async def get_role_dashboard(role: str, user_id: int = None, project_id: str = None):
        """Retorna dashboard HTML para a role"""
        return generate_role_dashboard_html(role, user_id, project_id)

    @router.get("/widgets/{role}")
    async def get_role_widgets(role: str):
        """Retorna widgets configurados para a role"""
        return {"widgets": get_widgets_for_role(role)}

    @router.get("/data")
    async def get_dashboard_data_endpoint(role: str, project_id: str = None):
        """Retorna dados para o dashboard"""
        return get_dashboard_data(role, project_id)

    @router.get("/preferences/{user_id}")
    async def get_preferences(user_id: int):
        """Retorna preferencias do usuario"""
        return get_user_preferences(user_id)

    @router.post("/preferences")
    async def save_preferences(data: dict):
        """Salva preferencias do usuario"""
        success = save_user_preferences(data.get("user_id", 0), data)
        return {"success": success}

    app.include_router(router)
    print("[Dashboard] Role Views endpoints registered")


# Need to import json for the HTML template
import json


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI
    from fastapi.responses import HTMLResponse

    app = FastAPI(title="Role Views Dashboard")

    register_role_views_endpoints(app)

    @app.get("/", response_class=HTMLResponse)
    async def root():
        return generate_role_dashboard_html("manager")

    @app.get("/admin", response_class=HTMLResponse)
    async def admin():
        return generate_role_dashboard_html("admin")

    @app.get("/developer", response_class=HTMLResponse)
    async def developer():
        return generate_role_dashboard_html("developer")

    @app.get("/viewer", response_class=HTMLResponse)
    async def viewer():
        return generate_role_dashboard_html("viewer")

    print("[Role Views] Starting standalone server on port 9006...")
    uvicorn.run(app, host="0.0.0.0", port=9006)
