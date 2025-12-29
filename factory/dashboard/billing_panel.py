# -*- coding: utf-8 -*-
"""
Billing Panel - Painel de Billing e Usage
==========================================

Issue #89 - Painel de Billing e Usage

Este modulo fornece componentes UI para visualizacao de billing:
- Visualizacao de consumo por tenant
- Historico de faturas
- Projecoes de custo
- Alertas de limite de uso

Componentes:
- billing_panel(): Painel principal
- usage_chart(): Grafico de uso
- invoice_table(): Tabela de faturas
- cost_projections(): Projecoes de custo
"""

from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional
import json

# Database
from factory.database.connection import SessionLocal


# =============================================================================
# DATA FUNCTIONS
# =============================================================================

def get_usage_stats(tenant_id: str, period: str = "month") -> Dict[str, Any]:
    """
    Retorna estatisticas de uso do tenant

    Args:
        tenant_id: ID do tenant
        period: Periodo (day, week, month, year)

    Returns:
        Dict com metricas de uso
    """
    db = SessionLocal()
    try:
        # Simulacao de dados - em producao buscaria do billing service
        now = datetime.utcnow()

        if period == "day":
            days = 1
        elif period == "week":
            days = 7
        elif period == "month":
            days = 30
        else:
            days = 365

        return {
            "period": period,
            "period_start": (now - timedelta(days=days)).isoformat(),
            "period_end": now.isoformat(),
            "api_calls": {
                "total": 15420,
                "by_endpoint": {
                    "stories": 5200,
                    "tasks": 4100,
                    "jobs": 3200,
                    "workers": 2920
                }
            },
            "tokens_used": {
                "total": 2450000,
                "input_tokens": 1200000,
                "output_tokens": 1250000
            },
            "storage": {
                "total_gb": 12.5,
                "projects_gb": 8.2,
                "attachments_gb": 3.1,
                "backups_gb": 1.2
            },
            "compute": {
                "worker_hours": 156.5,
                "job_executions": 342,
                "avg_job_duration_min": 4.2
            },
            "cost": {
                "total": 245.80,
                "api_calls": 45.60,
                "tokens": 122.50,
                "storage": 25.00,
                "compute": 52.70
            }
        }
    finally:
        db.close()


def get_invoices(tenant_id: str, limit: int = 12) -> List[Dict[str, Any]]:
    """
    Retorna historico de faturas do tenant

    Args:
        tenant_id: ID do tenant
        limit: Numero de faturas a retornar

    Returns:
        Lista de faturas
    """
    # Simulacao de dados
    invoices = []
    now = datetime.utcnow()

    for i in range(limit):
        month_date = now - timedelta(days=30 * i)
        base_amount = 200 + (i * 15)  # Simular crescimento

        invoices.append({
            "invoice_id": f"INV-{month_date.strftime('%Y%m')}-{tenant_id[:6]}",
            "period": month_date.strftime("%B %Y"),
            "period_start": (month_date.replace(day=1)).isoformat(),
            "period_end": month_date.isoformat(),
            "status": "paid" if i > 0 else "pending",
            "amount": round(base_amount + (base_amount * 0.1 * (i % 3)), 2),
            "currency": "BRL",
            "items": [
                {"description": "API Calls", "amount": round(base_amount * 0.2, 2)},
                {"description": "Token Usage", "amount": round(base_amount * 0.5, 2)},
                {"description": "Storage", "amount": round(base_amount * 0.1, 2)},
                {"description": "Compute", "amount": round(base_amount * 0.2, 2)}
            ],
            "due_date": (month_date + timedelta(days=15)).isoformat(),
            "paid_at": (month_date + timedelta(days=10)).isoformat() if i > 0 else None,
            "payment_method": "credit_card" if i > 0 else None,
            "pdf_url": f"/api/billing/invoices/{tenant_id}/INV-{month_date.strftime('%Y%m')}.pdf"
        })

    return invoices


def get_cost_projections(tenant_id: str) -> Dict[str, Any]:
    """
    Retorna projecoes de custo para o tenant

    Args:
        tenant_id: ID do tenant

    Returns:
        Dict com projecoes
    """
    now = datetime.utcnow()
    current_usage = get_usage_stats(tenant_id, "month")

    # Simular projecao baseada no uso atual
    daily_avg = current_usage["cost"]["total"] / 30

    return {
        "current_month": {
            "spent": current_usage["cost"]["total"],
            "projected_total": round(daily_avg * 30, 2),
            "days_remaining": 30 - now.day,
            "projected_remaining": round(daily_avg * (30 - now.day), 2)
        },
        "next_month": {
            "projected_total": round(daily_avg * 30 * 1.1, 2),  # 10% growth
            "breakdown": {
                "api_calls": round(daily_avg * 30 * 0.2 * 1.1, 2),
                "tokens": round(daily_avg * 30 * 0.5 * 1.1, 2),
                "storage": round(daily_avg * 30 * 0.1 * 1.1, 2),
                "compute": round(daily_avg * 30 * 0.2 * 1.1, 2)
            }
        },
        "quarterly": {
            "projected_total": round(daily_avg * 90 * 1.15, 2),
            "avg_monthly": round(daily_avg * 30 * 1.15, 2)
        },
        "yearly": {
            "projected_total": round(daily_avg * 365 * 1.2, 2),
            "avg_monthly": round(daily_avg * 30 * 1.2, 2)
        },
        "budget": {
            "monthly_limit": 500.00,
            "current_usage_pct": round((current_usage["cost"]["total"] / 500) * 100, 1),
            "alert_threshold": 80,
            "is_over_threshold": (current_usage["cost"]["total"] / 500) * 100 > 80
        },
        "savings_opportunities": [
            {
                "type": "optimize_tokens",
                "description": "Otimizar prompts para reduzir uso de tokens",
                "potential_savings": 25.00,
                "difficulty": "medium"
            },
            {
                "type": "batch_jobs",
                "description": "Agrupar jobs para reduzir tempo de compute",
                "potential_savings": 15.00,
                "difficulty": "easy"
            },
            {
                "type": "cleanup_storage",
                "description": "Limpar arquivos temporarios e backups antigos",
                "potential_savings": 8.00,
                "difficulty": "easy"
            }
        ]
    }


def get_usage_timeline(tenant_id: str, days: int = 30) -> List[Dict[str, Any]]:
    """
    Retorna timeline de uso para grafico

    Args:
        tenant_id: ID do tenant
        days: Numero de dias

    Returns:
        Lista de dados por dia
    """
    now = datetime.utcnow()
    timeline = []

    for i in range(days, -1, -1):
        day = now - timedelta(days=i)
        base = 8 + (i % 5)  # Variacao

        timeline.append({
            "date": day.strftime("%Y-%m-%d"),
            "api_calls": 500 + (base * 50),
            "tokens": 80000 + (base * 10000),
            "cost": round(base + (base * 0.2), 2)
        })

    return timeline


# =============================================================================
# UI COMPONENTS (HTML/JavaScript)
# =============================================================================

def generate_billing_panel_html(tenant_id: str) -> str:
    """
    Gera HTML do painel de billing

    Args:
        tenant_id: ID do tenant

    Returns:
        HTML completo
    """
    return f'''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Billing e Usage - Fabrica de Agentes</title>
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
            max-width: 1400px;
            margin: 0 auto;
            padding: 24px;
        }}

        .header {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }}

        .header h1 {{
            font-size: 24px;
            color: var(--primary);
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
            text-decoration: none;
        }}

        .btn-primary {{
            background: var(--primary);
            color: white;
        }}

        .btn-primary:hover {{
            background: #00526A;
        }}

        .btn-outline {{
            background: transparent;
            border: 1px solid var(--border);
            color: var(--text);
        }}

        .btn-outline:hover {{
            background: var(--bg);
        }}

        /* Summary Cards */
        .summary-grid {{
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 20px;
            margin-bottom: 24px;
        }}

        .summary-card {{
            background: var(--surface);
            border-radius: 12px;
            padding: 24px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }}

        .summary-card.highlight {{
            background: linear-gradient(135deg, var(--primary), #00526A);
            color: white;
        }}

        .summary-header {{
            display: flex;
            justify-content: space-between;
            align-items: flex-start;
            margin-bottom: 12px;
        }}

        .summary-icon {{
            width: 44px;
            height: 44px;
            border-radius: 10px;
            background: var(--bg);
            display: flex;
            align-items: center;
            justify-content: center;
        }}

        .summary-card.highlight .summary-icon {{
            background: rgba(255,255,255,0.2);
        }}

        .summary-trend {{
            font-size: 12px;
            padding: 4px 8px;
            border-radius: 12px;
            font-weight: 500;
        }}

        .summary-trend.up {{
            background: #D1FAE5;
            color: #065F46;
        }}

        .summary-trend.down {{
            background: #FEE2E2;
            color: #991B1B;
        }}

        .summary-card.highlight .summary-trend {{
            background: rgba(255,255,255,0.2);
            color: white;
        }}

        .summary-value {{
            font-size: 32px;
            font-weight: 700;
            margin-bottom: 4px;
        }}

        .summary-label {{
            font-size: 14px;
            color: var(--text-secondary);
        }}

        .summary-card.highlight .summary-label {{
            color: rgba(255,255,255,0.8);
        }}

        /* Main Grid */
        .main-grid {{
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 24px;
            margin-bottom: 24px;
        }}

        /* Panel */
        .panel {{
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            overflow: hidden;
        }}

        .panel-header {{
            padding: 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .panel-title {{
            font-size: 16px;
            font-weight: 600;
        }}

        .panel-body {{
            padding: 20px;
        }}

        /* Chart Container */
        .chart-container {{
            height: 300px;
            position: relative;
        }}

        /* Usage Breakdown */
        .usage-breakdown {{
            display: flex;
            flex-direction: column;
            gap: 16px;
        }}

        .usage-item {{
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .usage-item-left {{
            display: flex;
            align-items: center;
            gap: 12px;
        }}

        .usage-item-dot {{
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }}

        .usage-item-label {{
            font-size: 14px;
        }}

        .usage-item-value {{
            font-weight: 600;
        }}

        .usage-bar {{
            height: 8px;
            background: var(--bg);
            border-radius: 4px;
            margin-top: 8px;
            overflow: hidden;
        }}

        .usage-bar-fill {{
            height: 100%;
            border-radius: 4px;
            transition: width 0.3s;
        }}

        /* Invoices Table */
        .invoices-table {{
            width: 100%;
            border-collapse: collapse;
        }}

        .invoices-table th,
        .invoices-table td {{
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }}

        .invoices-table th {{
            font-weight: 600;
            font-size: 13px;
            color: var(--text-secondary);
            text-transform: uppercase;
            background: var(--bg);
        }}

        .invoices-table tr:hover {{
            background: var(--bg);
        }}

        /* Badge */
        .badge {{
            display: inline-block;
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }}

        .badge-paid {{
            background: #D1FAE5;
            color: #065F46;
        }}

        .badge-pending {{
            background: #FEF3C7;
            color: #92400E;
        }}

        .badge-overdue {{
            background: #FEE2E2;
            color: #991B1B;
        }}

        /* Projections */
        .projections-grid {{
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 16px;
        }}

        .projection-card {{
            background: var(--bg);
            border-radius: 8px;
            padding: 16px;
            text-align: center;
        }}

        .projection-label {{
            font-size: 12px;
            color: var(--text-secondary);
            margin-bottom: 8px;
        }}

        .projection-value {{
            font-size: 24px;
            font-weight: 700;
            color: var(--primary);
        }}

        /* Budget Gauge */
        .budget-gauge {{
            text-align: center;
            padding: 20px;
        }}

        .gauge-container {{
            width: 200px;
            height: 100px;
            margin: 0 auto;
            position: relative;
            overflow: hidden;
        }}

        .gauge-bg {{
            width: 200px;
            height: 200px;
            border-radius: 50%;
            background: conic-gradient(
                var(--bg) 0deg,
                var(--bg) 180deg
            );
            position: absolute;
            top: 0;
        }}

        .gauge-fill {{
            width: 200px;
            height: 200px;
            border-radius: 50%;
            position: absolute;
            top: 0;
            transition: all 0.5s;
        }}

        .gauge-center {{
            width: 140px;
            height: 140px;
            background: var(--surface);
            border-radius: 50%;
            position: absolute;
            top: 30px;
            left: 30px;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
        }}

        .gauge-value {{
            font-size: 28px;
            font-weight: 700;
            color: var(--primary);
        }}

        .gauge-label {{
            font-size: 12px;
            color: var(--text-secondary);
        }}

        /* Savings */
        .savings-list {{
            display: flex;
            flex-direction: column;
            gap: 12px;
        }}

        .savings-item {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 12px;
            background: var(--bg);
            border-radius: 8px;
        }}

        .savings-item-left {{
            flex: 1;
        }}

        .savings-description {{
            font-size: 14px;
            margin-bottom: 4px;
        }}

        .savings-difficulty {{
            font-size: 12px;
            color: var(--text-secondary);
        }}

        .savings-value {{
            font-weight: 600;
            color: var(--success);
        }}

        /* Period Selector */
        .period-selector {{
            display: flex;
            gap: 4px;
            background: var(--bg);
            padding: 4px;
            border-radius: 8px;
        }}

        .period-btn {{
            padding: 8px 16px;
            border: none;
            background: transparent;
            border-radius: 6px;
            font-size: 13px;
            cursor: pointer;
            transition: all 0.2s;
        }}

        .period-btn.active {{
            background: var(--surface);
            font-weight: 500;
            box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        }}

        /* Responsive */
        @media (max-width: 1200px) {{
            .summary-grid {{
                grid-template-columns: repeat(2, 1fr);
            }}
            .main-grid {{
                grid-template-columns: 1fr;
            }}
            .projections-grid {{
                grid-template-columns: 1fr;
            }}
        }}
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <h1>Billing e Usage</h1>
            <div class="header-actions">
                <div class="period-selector">
                    <button class="period-btn" onclick="changePeriod('day')">Dia</button>
                    <button class="period-btn" onclick="changePeriod('week')">Semana</button>
                    <button class="period-btn active" onclick="changePeriod('month')">Mes</button>
                    <button class="period-btn" onclick="changePeriod('year')">Ano</button>
                </div>
                <button class="btn btn-outline" onclick="downloadReport()">
                    Baixar Relatorio
                </button>
            </div>
        </div>

        <!-- Summary Cards -->
        <div class="summary-grid">
            <div class="summary-card highlight">
                <div class="summary-header">
                    <div class="summary-icon">
                        <svg width="24" height="24" fill="white" viewBox="0 0 20 20">
                            <path d="M8.433 7.418c.155-.103.346-.196.567-.267v1.698a2.305 2.305 0 01-.567-.267C8.07 8.34 8 8.114 8 8c0-.114.07-.34.433-.582zM11 12.849v-1.698c.22.071.412.164.567.267.364.243.433.468.433.582 0 .114-.07.34-.433.582a2.305 2.305 0 01-.567.267z"/>
                            <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-13a1 1 0 10-2 0v.092a4.535 4.535 0 00-1.676.662C6.602 6.234 6 7.009 6 8c0 .99.602 1.765 1.324 2.246.48.32 1.054.545 1.676.662v1.941c-.391-.127-.68-.317-.843-.504a1 1 0 10-1.51 1.31c.562.649 1.413 1.076 2.353 1.253V15a1 1 0 102 0v-.092a4.535 4.535 0 001.676-.662C13.398 13.766 14 12.991 14 12c0-.99-.602-1.765-1.324-2.246A4.535 4.535 0 0011 9.092V7.151c.391.127.68.317.843.504a1 1 0 101.511-1.31c-.563-.649-1.413-1.076-2.354-1.253V5z" clip-rule="evenodd"/>
                        </svg>
                    </div>
                    <span class="summary-trend up">+12%</span>
                </div>
                <div class="summary-value" id="total-cost">R$ 0,00</div>
                <div class="summary-label">Custo Total do Periodo</div>
            </div>

            <div class="summary-card">
                <div class="summary-header">
                    <div class="summary-icon">
                        <svg width="24" height="24" fill="var(--primary)" viewBox="0 0 20 20">
                            <path fill-rule="evenodd" d="M2 5a2 2 0 012-2h12a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V5zm3.293 1.293a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 01-1.414-1.414L7.586 10 5.293 7.707a1 1 0 010-1.414z" clip-rule="evenodd"/>
                        </svg>
                    </div>
                    <span class="summary-trend up">+8%</span>
                </div>
                <div class="summary-value" id="api-calls">0</div>
                <div class="summary-label">Chamadas de API</div>
            </div>

            <div class="summary-card">
                <div class="summary-header">
                    <div class="summary-icon">
                        <svg width="24" height="24" fill="var(--primary)" viewBox="0 0 20 20">
                            <path d="M3 3a1 1 0 000 2v8a2 2 0 002 2h2.586l-1.293 1.293a1 1 0 101.414 1.414L10 15.414l2.293 2.293a1 1 0 001.414-1.414L12.414 15H15a2 2 0 002-2V5a1 1 0 100-2H3zm11 4a1 1 0 10-2 0v4a1 1 0 102 0V7zm-3 1a1 1 0 10-2 0v3a1 1 0 102 0V8zM8 9a1 1 0 00-2 0v2a1 1 0 102 0V9z"/>
                        </svg>
                    </div>
                </div>
                <div class="summary-value" id="tokens-used">0</div>
                <div class="summary-label">Tokens Utilizados</div>
            </div>

            <div class="summary-card">
                <div class="summary-header">
                    <div class="summary-icon">
                        <svg width="24" height="24" fill="var(--primary)" viewBox="0 0 20 20">
                            <path d="M2 6a2 2 0 012-2h12a2 2 0 012 2v2a2 2 0 100 4v2a2 2 0 01-2 2H4a2 2 0 01-2-2v-2a2 2 0 100-4V6z"/>
                        </svg>
                    </div>
                </div>
                <div class="summary-value" id="storage-used">0 GB</div>
                <div class="summary-label">Armazenamento</div>
            </div>
        </div>

        <!-- Main Grid -->
        <div class="main-grid">
            <!-- Usage Chart -->
            <div class="panel">
                <div class="panel-header">
                    <h3 class="panel-title">Uso ao Longo do Tempo</h3>
                </div>
                <div class="panel-body">
                    <div class="chart-container">
                        <canvas id="usage-chart"></canvas>
                    </div>
                </div>
            </div>

            <!-- Usage Breakdown -->
            <div class="panel">
                <div class="panel-header">
                    <h3 class="panel-title">Distribuicao de Custos</h3>
                </div>
                <div class="panel-body">
                    <div class="usage-breakdown" id="cost-breakdown">
                        <!-- Populated by JS -->
                    </div>
                </div>
            </div>
        </div>

        <!-- Budget and Projections -->
        <div class="main-grid">
            <!-- Budget Gauge -->
            <div class="panel">
                <div class="panel-header">
                    <h3 class="panel-title">Projecoes de Custo</h3>
                </div>
                <div class="panel-body">
                    <div class="projections-grid" id="projections">
                        <!-- Populated by JS -->
                    </div>
                </div>
            </div>

            <!-- Savings Opportunities -->
            <div class="panel">
                <div class="panel-header">
                    <h3 class="panel-title">Oportunidades de Economia</h3>
                </div>
                <div class="panel-body">
                    <div class="savings-list" id="savings-list">
                        <!-- Populated by JS -->
                    </div>
                </div>
            </div>
        </div>

        <!-- Invoices -->
        <div class="panel" style="margin-top: 24px;">
            <div class="panel-header">
                <h3 class="panel-title">Historico de Faturas</h3>
            </div>
            <div class="panel-body" style="padding: 0;">
                <table class="invoices-table">
                    <thead>
                        <tr>
                            <th>Fatura</th>
                            <th>Periodo</th>
                            <th>Valor</th>
                            <th>Status</th>
                            <th>Vencimento</th>
                            <th>Acoes</th>
                        </tr>
                    </thead>
                    <tbody id="invoices-tbody">
                        <tr>
                            <td colspan="6" style="text-align: center; padding: 40px; color: var(--text-secondary);">
                                Carregando faturas...
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>
    </div>

    <script>
        const TENANT_ID = "{tenant_id}";
        let currentPeriod = "month";

        // Load data on page load
        document.addEventListener('DOMContentLoaded', () => {{
            loadUsageStats();
            loadInvoices();
            loadProjections();
        }});

        function changePeriod(period) {{
            currentPeriod = period;
            document.querySelectorAll('.period-btn').forEach(btn => btn.classList.remove('active'));
            event.target.classList.add('active');
            loadUsageStats();
        }}

        async function loadUsageStats() {{
            try {{
                const res = await fetch(`/api/billing/usage?tenant_id=${{TENANT_ID}}&period=${{currentPeriod}}`);
                const data = await res.json();

                // Update summary cards
                document.getElementById('total-cost').textContent = `R$ ${{data.cost.total.toFixed(2)}}`;
                document.getElementById('api-calls').textContent = data.api_calls.total.toLocaleString();
                document.getElementById('tokens-used').textContent = (data.tokens_used.total / 1000000).toFixed(2) + 'M';
                document.getElementById('storage-used').textContent = data.storage.total_gb.toFixed(1) + ' GB';

                // Update cost breakdown
                renderCostBreakdown(data.cost);

                // Draw chart
                drawUsageChart();
            }} catch (error) {{
                console.error('Error loading usage:', error);
                // Use mock data
                document.getElementById('total-cost').textContent = 'R$ 245,80';
                document.getElementById('api-calls').textContent = '15.420';
                document.getElementById('tokens-used').textContent = '2.45M';
                document.getElementById('storage-used').textContent = '12.5 GB';
                renderCostBreakdown({{
                    api_calls: 45.60,
                    tokens: 122.50,
                    storage: 25.00,
                    compute: 52.70
                }});
                drawUsageChart();
            }}
        }}

        function renderCostBreakdown(cost) {{
            const container = document.getElementById('cost-breakdown');
            const items = [
                {{ label: 'Tokens/IA', value: cost.tokens || 122.50, color: '#003B4A' }},
                {{ label: 'Compute', value: cost.compute || 52.70, color: '#FF6C00' }},
                {{ label: 'API Calls', value: cost.api_calls || 45.60, color: '#10B981' }},
                {{ label: 'Storage', value: cost.storage || 25.00, color: '#3B82F6' }}
            ];

            const total = items.reduce((sum, i) => sum + i.value, 0);

            container.innerHTML = items.map(item => `
                <div class="usage-item">
                    <div class="usage-item-left">
                        <div class="usage-item-dot" style="background: ${{item.color}}"></div>
                        <span class="usage-item-label">${{item.label}}</span>
                    </div>
                    <span class="usage-item-value">R$ ${{item.value.toFixed(2)}}</span>
                </div>
                <div class="usage-bar">
                    <div class="usage-bar-fill" style="width: ${{(item.value / total * 100).toFixed(1)}}%; background: ${{item.color}}"></div>
                </div>
            `).join('');
        }}

        function drawUsageChart() {{
            const canvas = document.getElementById('usage-chart');
            const ctx = canvas.getContext('2d');

            const width = canvas.parentElement.clientWidth;
            const height = 280;
            canvas.width = width;
            canvas.height = height;

            // Mock data for chart
            const days = 30;
            const data = [];
            for (let i = 0; i < days; i++) {{
                data.push(8 + Math.random() * 4);
            }}

            const maxValue = Math.max(...data) * 1.1;
            const barWidth = (width - 60) / days;
            const chartHeight = height - 50;

            // Clear
            ctx.fillStyle = '#FFFFFF';
            ctx.fillRect(0, 0, width, height);

            // Draw bars
            data.forEach((value, i) => {{
                const x = 50 + i * barWidth;
                const barHeight = (value / maxValue) * chartHeight;

                ctx.fillStyle = '#003B4A';
                ctx.fillRect(x + 2, chartHeight - barHeight + 10, barWidth - 4, barHeight);
            }});

            // Y-axis labels
            ctx.fillStyle = '#6B7280';
            ctx.font = '11px Inter, sans-serif';
            ctx.textAlign = 'right';
            ctx.fillText('R$ ' + maxValue.toFixed(0), 45, 20);
            ctx.fillText('R$ 0', 45, chartHeight + 10);

            // X-axis label
            ctx.textAlign = 'center';
            ctx.fillText('Ultimos 30 dias', width / 2, height - 5);
        }}

        async function loadProjections() {{
            try {{
                const res = await fetch(`/api/billing/projections?tenant_id=${{TENANT_ID}}`);
                const data = await res.json();
                renderProjections(data);
                renderSavings(data.savings_opportunities || []);
            }} catch (error) {{
                console.error('Error loading projections:', error);
                // Use mock data
                renderProjections({{
                    current_month: {{ projected_total: 280.00 }},
                    next_month: {{ projected_total: 308.00 }},
                    quarterly: {{ projected_total: 870.00 }}
                }});
                renderSavings([
                    {{ description: 'Otimizar prompts', potential_savings: 25.00, difficulty: 'medium' }},
                    {{ description: 'Agrupar jobs', potential_savings: 15.00, difficulty: 'easy' }},
                    {{ description: 'Limpar storage', potential_savings: 8.00, difficulty: 'easy' }}
                ]);
            }}
        }}

        function renderProjections(data) {{
            const container = document.getElementById('projections');
            container.innerHTML = `
                <div class="projection-card">
                    <div class="projection-label">Projecao Este Mes</div>
                    <div class="projection-value">R$ ${{(data.current_month?.projected_total || 280).toFixed(2)}}</div>
                </div>
                <div class="projection-card">
                    <div class="projection-label">Projecao Proximo Mes</div>
                    <div class="projection-value">R$ ${{(data.next_month?.projected_total || 308).toFixed(2)}}</div>
                </div>
                <div class="projection-card">
                    <div class="projection-label">Projecao Trimestre</div>
                    <div class="projection-value">R$ ${{(data.quarterly?.projected_total || 870).toFixed(2)}}</div>
                </div>
            `;
        }}

        function renderSavings(opportunities) {{
            const container = document.getElementById('savings-list');
            container.innerHTML = opportunities.map(opp => `
                <div class="savings-item">
                    <div class="savings-item-left">
                        <div class="savings-description">${{opp.description}}</div>
                        <div class="savings-difficulty">Dificuldade: ${{opp.difficulty}}</div>
                    </div>
                    <div class="savings-value">-R$ ${{opp.potential_savings.toFixed(2)}}</div>
                </div>
            `).join('');
        }}

        async function loadInvoices() {{
            try {{
                const res = await fetch(`/api/billing/invoices?tenant_id=${{TENANT_ID}}`);
                const data = await res.json();
                renderInvoices(data.invoices || []);
            }} catch (error) {{
                console.error('Error loading invoices:', error);
                // Use mock data
                renderInvoices([
                    {{ invoice_id: 'INV-202412', period: 'Dezembro 2024', amount: 245.80, status: 'pending', due_date: '2025-01-15' }},
                    {{ invoice_id: 'INV-202411', period: 'Novembro 2024', amount: 232.50, status: 'paid', due_date: '2024-12-15' }},
                    {{ invoice_id: 'INV-202410', period: 'Outubro 2024', amount: 218.30, status: 'paid', due_date: '2024-11-15' }}
                ]);
            }}
        }}

        function renderInvoices(invoices) {{
            const tbody = document.getElementById('invoices-tbody');

            if (!invoices || invoices.length === 0) {{
                tbody.innerHTML = '<tr><td colspan="6" style="text-align: center; padding: 40px; color: var(--text-secondary);">Nenhuma fatura encontrada</td></tr>';
                return;
            }}

            tbody.innerHTML = invoices.map(inv => `
                <tr>
                    <td><strong>${{inv.invoice_id}}</strong></td>
                    <td>${{inv.period}}</td>
                    <td>R$ ${{inv.amount.toFixed(2)}}</td>
                    <td>
                        <span class="badge badge-${{inv.status}}">
                            ${{inv.status === 'paid' ? 'Pago' : inv.status === 'pending' ? 'Pendente' : 'Vencido'}}
                        </span>
                    </td>
                    <td>${{new Date(inv.due_date).toLocaleDateString()}}</td>
                    <td>
                        <button class="btn btn-outline" style="padding: 6px 12px; font-size: 12px;" onclick="downloadInvoice('${{inv.invoice_id}}')">
                            PDF
                        </button>
                    </td>
                </tr>
            `).join('');
        }}

        function downloadInvoice(invoiceId) {{
            alert(`Download da fatura ${{invoiceId}} iniciado`);
        }}

        function downloadReport() {{
            alert('Download do relatorio iniciado');
        }}

        // Resize chart on window resize
        window.addEventListener('resize', drawUsageChart);
    </script>
</body>
</html>
'''


# =============================================================================
# API ENDPOINTS
# =============================================================================

def register_billing_endpoints(app):
    """
    Registra endpoints de billing no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter
    from fastapi.responses import HTMLResponse

    router = APIRouter(prefix="/api/billing", tags=["billing"])

    @router.get("/dashboard", response_class=HTMLResponse)
    async def billing_dashboard(tenant_id: str = "default"):
        """Retorna pagina HTML do painel de billing"""
        return generate_billing_panel_html(tenant_id)

    @router.get("/usage")
    async def get_usage(tenant_id: str, period: str = "month"):
        """Retorna estatisticas de uso"""
        return get_usage_stats(tenant_id, period)

    @router.get("/invoices")
    async def get_invoices_list(tenant_id: str, limit: int = 12):
        """Retorna lista de faturas"""
        return {"invoices": get_invoices(tenant_id, limit)}

    @router.get("/projections")
    async def get_projections(tenant_id: str):
        """Retorna projecoes de custo"""
        return get_cost_projections(tenant_id)

    @router.get("/timeline")
    async def get_timeline(tenant_id: str, days: int = 30):
        """Retorna timeline de uso"""
        return {"timeline": get_usage_timeline(tenant_id, days)}

    app.include_router(router)
    print("[Dashboard] Billing Panel endpoints registered")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI
    from fastapi.responses import HTMLResponse

    app = FastAPI(title="Billing Panel")

    register_billing_endpoints(app)

    @app.get("/", response_class=HTMLResponse)
    async def root():
        return generate_billing_panel_html("default")

    print("[Billing Panel] Starting standalone server on port 9005...")
    uvicorn.run(app, host="0.0.0.0", port=9005)
