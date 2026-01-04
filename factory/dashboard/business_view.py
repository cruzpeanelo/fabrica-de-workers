# -*- coding: utf-8 -*-
"""
Dashboard Simplificado para Usuarios de Negocio - Issue #135
============================================================
Visao simplificada focada em metricas de negocio:
- Visao focada em metricas de negocio
- Sem detalhes tecnicos
- Graficos e KPIs simples
- Export para PDF/Excel
"""

from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from io import StringIO, BytesIO
import csv
import json

from fastapi import APIRouter, Query
from fastapi.responses import HTMLResponse, Response, StreamingResponse
from pydantic import BaseModel


# =============================================================================
# ROUTER
# =============================================================================

business_router = APIRouter(prefix="/api/business", tags=["Business View"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class BusinessKPI(BaseModel):
    name: str
    value: Any
    unit: str = ""
    trend: Optional[str] = None  # up, down, stable
    trend_value: Optional[float] = None
    status: str = "normal"  # good, warning, critical, normal
    description: str = ""


class ProjectSummary(BaseModel):
    project_id: str
    name: str
    progress: float
    status: str
    tasks_total: int
    tasks_done: int
    days_remaining: Optional[int] = None
    health: str = "healthy"  # healthy, at_risk, critical


# =============================================================================
# MOCK DATA (Replace with real database queries in production)
# =============================================================================

def get_mock_business_data():
    """Retorna dados mock para demonstracao"""
    return {
        "kpis": [
            {
                "name": "Projetos em Andamento",
                "value": 5,
                "unit": "",
                "trend": "up",
                "trend_value": 2,
                "status": "good",
                "description": "Projetos ativos sendo desenvolvidos"
            },
            {
                "name": "Tarefas Concluidas",
                "value": 47,
                "unit": "esta semana",
                "trend": "up",
                "trend_value": 15,
                "status": "good",
                "description": "Tarefas finalizadas nos ultimos 7 dias"
            },
            {
                "name": "Taxa de Conclusao",
                "value": 78,
                "unit": "%",
                "trend": "stable",
                "trend_value": 0,
                "status": "good",
                "description": "Percentual de tarefas entregues no prazo"
            },
            {
                "name": "Tempo Medio de Entrega",
                "value": 3.5,
                "unit": "dias",
                "trend": "down",
                "trend_value": -0.5,
                "status": "good",
                "description": "Tempo medio para completar uma tarefa"
            }
        ],
        "projects": [
            {
                "project_id": "proj-001",
                "name": "Sistema de Vendas",
                "progress": 75,
                "status": "Em Andamento",
                "tasks_total": 24,
                "tasks_done": 18,
                "days_remaining": 15,
                "health": "healthy"
            },
            {
                "project_id": "proj-002",
                "name": "App Mobile Cliente",
                "progress": 45,
                "status": "Em Andamento",
                "tasks_total": 36,
                "tasks_done": 16,
                "days_remaining": 30,
                "health": "healthy"
            },
            {
                "project_id": "proj-003",
                "name": "Portal do Fornecedor",
                "progress": 90,
                "status": "Revisao Final",
                "tasks_total": 18,
                "tasks_done": 16,
                "days_remaining": 5,
                "health": "healthy"
            },
            {
                "project_id": "proj-004",
                "name": "Integracao ERP",
                "progress": 30,
                "status": "Em Risco",
                "tasks_total": 42,
                "tasks_done": 13,
                "days_remaining": 20,
                "health": "at_risk"
            },
            {
                "project_id": "proj-005",
                "name": "Dashboard BI",
                "progress": 15,
                "status": "Iniciando",
                "tasks_total": 30,
                "tasks_done": 5,
                "days_remaining": 45,
                "health": "healthy"
            }
        ],
        "timeline": [
            {"date": "2024-01-01", "tasks_done": 5, "tasks_created": 8},
            {"date": "2024-01-02", "tasks_done": 7, "tasks_created": 4},
            {"date": "2024-01-03", "tasks_done": 3, "tasks_created": 6},
            {"date": "2024-01-04", "tasks_done": 9, "tasks_created": 5},
            {"date": "2024-01-05", "tasks_done": 6, "tasks_created": 7},
            {"date": "2024-01-06", "tasks_done": 8, "tasks_created": 3},
            {"date": "2024-01-07", "tasks_done": 9, "tasks_created": 4}
        ],
        "distribution": {
            "concluido": 47,
            "em_andamento": 23,
            "planejado": 35,
            "atrasado": 8
        }
    }


# =============================================================================
# API ENDPOINTS
# =============================================================================

@business_router.get("/kpis")
async def get_business_kpis(
    project_id: Optional[str] = None,
    period: str = Query("week", enum=["week", "month", "quarter"])
):
    """Retorna KPIs de negocio simplificados"""
    data = get_mock_business_data()
    return {"kpis": data["kpis"], "period": period}


@business_router.get("/projects")
async def get_projects_summary():
    """Retorna resumo de todos os projetos"""
    data = get_mock_business_data()
    return {
        "projects": data["projects"],
        "total": len(data["projects"]),
        "healthy": len([p for p in data["projects"] if p["health"] == "healthy"]),
        "at_risk": len([p for p in data["projects"] if p["health"] == "at_risk"]),
        "critical": len([p for p in data["projects"] if p["health"] == "critical"])
    }


@business_router.get("/projects/{project_id}")
async def get_project_details(project_id: str):
    """Retorna detalhes de um projeto"""
    data = get_mock_business_data()
    project = next((p for p in data["projects"] if p["project_id"] == project_id), None)

    if not project:
        return {"error": "Projeto nao encontrado"}

    return {
        "project": project,
        "timeline": data["timeline"],
        "distribution": data["distribution"]
    }


@business_router.get("/timeline")
async def get_timeline(
    period: str = Query("week", enum=["week", "month", "quarter"])
):
    """Retorna timeline de atividades"""
    data = get_mock_business_data()
    return {"timeline": data["timeline"], "period": period}


@business_router.get("/distribution")
async def get_distribution():
    """Retorna distribuicao de status"""
    data = get_mock_business_data()
    return {"distribution": data["distribution"]}


@business_router.get("/export/csv")
async def export_csv():
    """Exporta dados para CSV"""
    data = get_mock_business_data()

    output = StringIO()
    writer = csv.writer(output)

    # Header
    writer.writerow(["Projeto", "Progresso", "Status", "Tarefas Totais", "Tarefas Concluidas", "Dias Restantes", "Saude"])

    # Data
    for project in data["projects"]:
        writer.writerow([
            project["name"],
            f"{project['progress']}%",
            project["status"],
            project["tasks_total"],
            project["tasks_done"],
            project["days_remaining"] or "-",
            project["health"]
        ])

    output.seek(0)
    return StreamingResponse(
        iter([output.getvalue()]),
        media_type="text/csv",
        headers={"Content-Disposition": "attachment; filename=relatorio_projetos.csv"}
    )


@business_router.get("/export/excel")
async def export_excel():
    """Exporta dados para Excel (formato CSV compativel)"""
    # Simplified Excel export using CSV format
    data = get_mock_business_data()

    output = StringIO()
    writer = csv.writer(output, delimiter=';')  # Use semicolon for Excel PT-BR compatibility

    # Header
    writer.writerow(["Projeto", "Progresso (%)", "Status", "Tarefas Totais", "Tarefas Concluidas", "Dias Restantes", "Saude do Projeto"])

    # Data
    for project in data["projects"]:
        writer.writerow([
            project["name"],
            project["progress"],
            project["status"],
            project["tasks_total"],
            project["tasks_done"],
            project["days_remaining"] or "",
            "Saudavel" if project["health"] == "healthy" else ("Em Risco" if project["health"] == "at_risk" else "Critico")
        ])

    # KPIs section
    writer.writerow([])
    writer.writerow(["Indicadores Chave de Desempenho"])
    writer.writerow(["Indicador", "Valor", "Unidade", "Tendencia"])

    for kpi in data["kpis"]:
        trend_text = "Subindo" if kpi["trend"] == "up" else ("Caindo" if kpi["trend"] == "down" else "Estavel")
        writer.writerow([
            kpi["name"],
            kpi["value"],
            kpi["unit"],
            trend_text
        ])

    output.seek(0)
    return StreamingResponse(
        iter([output.getvalue()]),
        media_type="application/vnd.ms-excel",
        headers={"Content-Disposition": "attachment; filename=relatorio_projetos.xls"}
    )


@business_router.get("/export/pdf")
async def export_pdf():
    """Retorna dados formatados para geracao de PDF no frontend"""
    data = get_mock_business_data()

    return {
        "title": "Relatorio de Projetos",
        "generated_at": datetime.utcnow().isoformat(),
        "kpis": data["kpis"],
        "projects": data["projects"],
        "summary": {
            "total_projects": len(data["projects"]),
            "total_tasks": sum(p["tasks_total"] for p in data["projects"]),
            "total_done": sum(p["tasks_done"] for p in data["projects"]),
            "avg_progress": round(sum(p["progress"] for p in data["projects"]) / len(data["projects"]), 1)
        }
    }


# =============================================================================
# HTML TEMPLATE
# =============================================================================

BUSINESS_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Visao de Negocios - Plataforma E</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://unpkg.com/lucide-static@latest/font/lucide.css">
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
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
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .header-left h1 {
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 4px;
        }

        .header-left p {
            opacity: 0.8;
            font-size: 14px;
        }

        .header-actions {
            display: flex;
            gap: 12px;
        }

        .btn {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 10px 20px;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
            text-decoration: none;
        }

        .btn-export {
            background: rgba(255, 255, 255, 0.15);
            border: none;
            color: white;
        }

        .btn-export:hover {
            background: rgba(255, 255, 255, 0.25);
        }

        /* Main Content */
        .main {
            padding: 32px;
            max-width: 1400px;
            margin: 0 auto;
        }

        /* Period Selector */
        .period-selector {
            display: flex;
            gap: 8px;
            margin-bottom: 24px;
        }

        .period-btn {
            padding: 10px 20px;
            background: white;
            border: 2px solid #E5E7EB;
            border-radius: 8px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            color: #6B7280;
            transition: all 0.2s;
        }

        .period-btn:hover {
            border-color: #003B4A;
            color: #003B4A;
        }

        .period-btn.active {
            background: #003B4A;
            border-color: #003B4A;
            color: white;
        }

        /* KPI Cards */
        .kpi-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 20px;
            margin-bottom: 32px;
        }

        .kpi-card {
            background: white;
            border-radius: 16px;
            padding: 24px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
        }

        .kpi-header {
            display: flex;
            justify-content: space-between;
            align-items: flex-start;
            margin-bottom: 16px;
        }

        .kpi-title {
            font-size: 14px;
            color: #6B7280;
            font-weight: 500;
        }

        .kpi-icon {
            width: 40px;
            height: 40px;
            border-radius: 10px;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
        }

        .kpi-icon.good { background: linear-gradient(135deg, #10B981 0%, #059669 100%); }
        .kpi-icon.warning { background: linear-gradient(135deg, #F59E0B 0%, #D97706 100%); }
        .kpi-icon.critical { background: linear-gradient(135deg, #EF4444 0%, #DC2626 100%); }
        .kpi-icon.normal { background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%); }

        .kpi-value {
            font-size: 36px;
            font-weight: 700;
            color: #1F2937;
            margin-bottom: 4px;
        }

        .kpi-unit {
            font-size: 14px;
            color: #9CA3AF;
            font-weight: 400;
        }

        .kpi-trend {
            display: flex;
            align-items: center;
            gap: 6px;
            font-size: 13px;
            font-weight: 500;
            margin-top: 12px;
        }

        .kpi-trend.up { color: #10B981; }
        .kpi-trend.down { color: #EF4444; }
        .kpi-trend.stable { color: #6B7280; }

        .kpi-description {
            font-size: 12px;
            color: #9CA3AF;
            margin-top: 8px;
        }

        /* Section Title */
        .section-title {
            font-size: 18px;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 20px;
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .section-title-icon {
            width: 32px;
            height: 32px;
            background: #EEF2FF;
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            color: #4F46E5;
        }

        /* Charts Grid */
        .charts-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
            gap: 24px;
            margin-bottom: 32px;
        }

        .chart-card {
            background: white;
            border-radius: 16px;
            padding: 24px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
        }

        .chart-title {
            font-size: 16px;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 20px;
        }

        .chart-container {
            position: relative;
            height: 250px;
        }

        /* Projects List */
        .projects-list {
            background: white;
            border-radius: 16px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
            overflow: hidden;
        }

        .projects-header {
            padding: 20px 24px;
            border-bottom: 1px solid #E5E7EB;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .projects-header h3 {
            font-size: 16px;
            font-weight: 600;
            color: #1F2937;
        }

        .project-row {
            display: grid;
            grid-template-columns: 2fr 1fr 1fr 1fr 80px;
            padding: 16px 24px;
            border-bottom: 1px solid #F3F4F6;
            align-items: center;
            transition: all 0.2s;
        }

        .project-row:hover {
            background: #F9FAFB;
        }

        .project-name {
            font-weight: 500;
            color: #1F2937;
        }

        .project-status {
            display: inline-flex;
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }

        .project-status.em-andamento {
            background: #DBEAFE;
            color: #1D4ED8;
        }

        .project-status.revisao-final {
            background: #FEF3C7;
            color: #B45309;
        }

        .project-status.em-risco {
            background: #FEE2E2;
            color: #DC2626;
        }

        .project-status.iniciando {
            background: #E5E7EB;
            color: #4B5563;
        }

        .project-progress {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .progress-bar {
            flex: 1;
            height: 8px;
            background: #E5E7EB;
            border-radius: 4px;
            overflow: hidden;
        }

        .progress-fill {
            height: 100%;
            border-radius: 4px;
            transition: width 0.3s ease;
        }

        .progress-fill.healthy { background: linear-gradient(90deg, #10B981 0%, #059669 100%); }
        .progress-fill.at-risk { background: linear-gradient(90deg, #F59E0B 0%, #D97706 100%); }
        .progress-fill.critical { background: linear-gradient(90deg, #EF4444 0%, #DC2626 100%); }

        .progress-value {
            font-size: 14px;
            font-weight: 600;
            color: #1F2937;
            min-width: 45px;
            text-align: right;
        }

        .project-tasks {
            font-size: 14px;
            color: #6B7280;
        }

        .project-days {
            font-size: 14px;
            color: #6B7280;
            display: flex;
            align-items: center;
            gap: 6px;
        }

        .project-health {
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }

        .project-health.healthy { background: #10B981; }
        .project-health.at-risk { background: #F59E0B; }
        .project-health.critical { background: #EF4444; }

        /* Summary Cards */
        .summary-grid {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 16px;
            margin-top: 24px;
        }

        .summary-card {
            background: white;
            border-radius: 12px;
            padding: 20px;
            text-align: center;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
        }

        .summary-value {
            font-size: 28px;
            font-weight: 700;
            color: #1F2937;
        }

        .summary-label {
            font-size: 13px;
            color: #6B7280;
            margin-top: 4px;
        }

        /* Print Styles */
        @media print {
            body { background: white; }
            .header-actions { display: none; }
            .period-selector { display: none; }
            .main { padding: 0; }
        }

        /* Responsive */
        @media (max-width: 768px) {
            .header {
                flex-direction: column;
                gap: 16px;
            }

            .project-row {
                grid-template-columns: 1fr;
                gap: 12px;
            }

            .summary-grid {
                grid-template-columns: repeat(2, 1fr);
            }

            .charts-grid {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-left">
            <h1>Visao de Negocios</h1>
            <p>Acompanhe o progresso dos seus projetos de forma simples</p>
        </div>
        <div class="header-actions">
            <a href="/api/business/export/csv" class="btn btn-export" download>
                <i class="lucide-file-spreadsheet" style="width: 18px; height: 18px;"></i>
                Exportar CSV
            </a>
            <a href="/api/business/export/excel" class="btn btn-export" download>
                <i class="lucide-file-spreadsheet" style="width: 18px; height: 18px;"></i>
                Exportar Excel
            </a>
            <button class="btn btn-export" onclick="window.print()">
                <i class="lucide-printer" style="width: 18px; height: 18px;"></i>
                Imprimir
            </button>
        </div>
    </div>

    <div class="main">
        <!-- Period Selector -->
        <div class="period-selector">
            <button class="period-btn active" data-period="week" onclick="setPeriod('week')">Esta Semana</button>
            <button class="period-btn" data-period="month" onclick="setPeriod('month')">Este Mes</button>
            <button class="period-btn" data-period="quarter" onclick="setPeriod('quarter')">Este Trimestre</button>
        </div>

        <!-- KPIs -->
        <div class="kpi-grid" id="kpiGrid">
            <!-- KPIs populated by JS -->
        </div>

        <!-- Charts -->
        <div class="charts-grid">
            <div class="chart-card">
                <h4 class="chart-title">Tarefas por Dia</h4>
                <div class="chart-container">
                    <canvas id="timelineChart"></canvas>
                </div>
            </div>
            <div class="chart-card">
                <h4 class="chart-title">Status das Tarefas</h4>
                <div class="chart-container">
                    <canvas id="distributionChart"></canvas>
                </div>
            </div>
        </div>

        <!-- Projects -->
        <div class="section-title">
            <div class="section-title-icon">
                <i class="lucide-folder-kanban" style="width: 18px; height: 18px;"></i>
            </div>
            Projetos em Andamento
        </div>

        <div class="projects-list">
            <div class="projects-header">
                <h3>Todos os Projetos</h3>
            </div>
            <div id="projectsList">
                <!-- Projects populated by JS -->
            </div>
        </div>

        <!-- Summary -->
        <div class="summary-grid" id="summaryGrid">
            <!-- Summary populated by JS -->
        </div>
    </div>

    <script>
        let currentPeriod = 'week';
        let timelineChart = null;
        let distributionChart = null;

        // Initialize
        document.addEventListener('DOMContentLoaded', () => {
            loadData();
        });

        // Set period
        function setPeriod(period) {
            currentPeriod = period;
            document.querySelectorAll('.period-btn').forEach(btn => {
                btn.classList.toggle('active', btn.dataset.period === period);
            });
            loadData();
        }

        // Load all data
        async function loadData() {
            try {
                const [kpisRes, projectsRes, timelineRes, distRes] = await Promise.all([
                    fetch(`/api/business/kpis?period=${currentPeriod}`),
                    fetch('/api/business/projects'),
                    fetch(`/api/business/timeline?period=${currentPeriod}`),
                    fetch('/api/business/distribution')
                ]);

                const kpisData = await kpisRes.json();
                const projectsData = await projectsRes.json();
                const timelineData = await timelineRes.json();
                const distData = await distRes.json();

                renderKPIs(kpisData.kpis);
                renderProjects(projectsData.projects);
                renderTimeline(timelineData.timeline);
                renderDistribution(distData.distribution);
                renderSummary(projectsData);
            } catch (error) {
                console.error('Error loading data:', error);
            }
        }

        // Render KPIs
        function renderKPIs(kpis) {
            const container = document.getElementById('kpiGrid');
            const icons = {
                'Projetos em Andamento': 'folder-kanban',
                'Tarefas Concluidas': 'check-circle-2',
                'Taxa de Conclusao': 'target',
                'Tempo Medio de Entrega': 'clock'
            };

            container.innerHTML = kpis.map(kpi => {
                const icon = icons[kpi.name] || 'activity';
                const trendIcon = kpi.trend === 'up' ? 'trending-up' :
                                  kpi.trend === 'down' ? 'trending-down' : 'minus';
                const trendText = kpi.trend === 'up' ? `+${kpi.trend_value}` :
                                  kpi.trend === 'down' ? `${kpi.trend_value}` : 'Estavel';

                return `
                    <div class="kpi-card">
                        <div class="kpi-header">
                            <span class="kpi-title">${kpi.name}</span>
                            <div class="kpi-icon ${kpi.status}">
                                <i class="lucide-${icon}" style="width: 20px; height: 20px;"></i>
                            </div>
                        </div>
                        <div class="kpi-value">
                            ${kpi.value}<span class="kpi-unit">${kpi.unit}</span>
                        </div>
                        <div class="kpi-trend ${kpi.trend}">
                            <i class="lucide-${trendIcon}" style="width: 16px; height: 16px;"></i>
                            <span>${trendText} vs periodo anterior</span>
                        </div>
                        <p class="kpi-description">${kpi.description}</p>
                    </div>
                `;
            }).join('');
        }

        // Render projects
        function renderProjects(projects) {
            const container = document.getElementById('projectsList');
            container.innerHTML = projects.map(project => {
                const statusClass = project.status.toLowerCase()
                    .replace(/ /g, '-')
                    .replace(/[^a-z-]/g, '');

                return `
                    <div class="project-row">
                        <div class="project-name">${project.name}</div>
                        <span class="project-status ${statusClass}">${project.status}</span>
                        <div class="project-progress">
                            <div class="progress-bar">
                                <div class="progress-fill ${project.health}" style="width: ${project.progress}%"></div>
                            </div>
                            <span class="progress-value">${project.progress}%</span>
                        </div>
                        <div class="project-tasks">${project.tasks_done}/${project.tasks_total} tarefas</div>
                        <div class="project-days">
                            <div class="project-health ${project.health}"></div>
                            ${project.days_remaining ? project.days_remaining + 'd' : '-'}
                        </div>
                    </div>
                `;
            }).join('');
        }

        // Render timeline chart
        function renderTimeline(timeline) {
            const ctx = document.getElementById('timelineChart').getContext('2d');

            if (timelineChart) timelineChart.destroy();

            timelineChart = new Chart(ctx, {
                type: 'line',
                data: {
                    labels: timeline.map(t => {
                        const date = new Date(t.date);
                        return date.toLocaleDateString('pt-BR', { weekday: 'short', day: 'numeric' });
                    }),
                    datasets: [
                        {
                            label: 'Concluidas',
                            data: timeline.map(t => t.tasks_done),
                            borderColor: '#10B981',
                            backgroundColor: 'rgba(16, 185, 129, 0.1)',
                            fill: true,
                            tension: 0.4
                        },
                        {
                            label: 'Criadas',
                            data: timeline.map(t => t.tasks_created),
                            borderColor: '#003B4A',
                            backgroundColor: 'rgba(0, 59, 74, 0.1)',
                            fill: true,
                            tension: 0.4
                        }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            position: 'bottom'
                        }
                    },
                    scales: {
                        y: {
                            beginAtZero: true
                        }
                    }
                }
            });
        }

        // Render distribution chart
        function renderDistribution(distribution) {
            const ctx = document.getElementById('distributionChart').getContext('2d');

            if (distributionChart) distributionChart.destroy();

            const labels = {
                concluido: 'Concluido',
                em_andamento: 'Em Andamento',
                planejado: 'Planejado',
                atrasado: 'Atrasado'
            };

            const colors = {
                concluido: '#10B981',
                em_andamento: '#3B82F6',
                planejado: '#9CA3AF',
                atrasado: '#EF4444'
            };

            distributionChart = new Chart(ctx, {
                type: 'doughnut',
                data: {
                    labels: Object.keys(distribution).map(k => labels[k] || k),
                    datasets: [{
                        data: Object.values(distribution),
                        backgroundColor: Object.keys(distribution).map(k => colors[k] || '#6B7280'),
                        borderWidth: 0
                    }]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            position: 'bottom'
                        }
                    },
                    cutout: '60%'
                }
            });
        }

        // Render summary
        function renderSummary(data) {
            const container = document.getElementById('summaryGrid');
            const totalTasks = data.projects.reduce((sum, p) => sum + p.tasks_total, 0);
            const totalDone = data.projects.reduce((sum, p) => sum + p.tasks_done, 0);
            const avgProgress = Math.round(data.projects.reduce((sum, p) => sum + p.progress, 0) / data.projects.length);

            container.innerHTML = `
                <div class="summary-card">
                    <div class="summary-value">${data.total}</div>
                    <div class="summary-label">Projetos Ativos</div>
                </div>
                <div class="summary-card">
                    <div class="summary-value">${totalTasks}</div>
                    <div class="summary-label">Tarefas Totais</div>
                </div>
                <div class="summary-card">
                    <div class="summary-value">${totalDone}</div>
                    <div class="summary-label">Tarefas Concluidas</div>
                </div>
                <div class="summary-card">
                    <div class="summary-value">${avgProgress}%</div>
                    <div class="summary-label">Progresso Medio</div>
                </div>
            `;
        }
    </script>
</body>
</html>'''


@business_router.get("/", response_class=HTMLResponse)
async def get_business_page():
    """Retorna a pagina de visao de negocios"""
    return HTMLResponse(content=BUSINESS_TEMPLATE)


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_business_view_endpoints(app):
    """Registra os endpoints da visao de negocios no app FastAPI"""
    app.include_router(business_router)

    @app.get("/business", response_class=HTMLResponse)
    async def business_page():
        return HTMLResponse(content=BUSINESS_TEMPLATE)

    print("[Dashboard] Business View endpoints registered")
