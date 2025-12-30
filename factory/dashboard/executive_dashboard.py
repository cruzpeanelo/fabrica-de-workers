# -*- coding: utf-8 -*-
"""
Executive Dashboard - Business Intelligence
============================================
Dashboard executivo com metricas de alto nivel para gestores:
- KPIs principais (Velocity, Lead Time, Cycle Time, Throughput)
- Graficos interativos (Chart.js)
- Filtros por projeto, periodo e epic
- Export PDF e CSV
"""

from datetime import datetime, timedelta
from typing import Optional
from io import StringIO
import csv

from fastapi import APIRouter
from fastapi.responses import HTMLResponse, Response

# Create router
executive_router = APIRouter(prefix="/api/executive", tags=["Executive"])


def register_executive_endpoints(app, SessionLocal, Story, Sprint):
    """Registra os endpoints do dashboard executivo no app FastAPI.

    Endpoints registrados:
    - GET /executive - Pagina do Dashboard Executivo
    - GET /api/executive/kpis - KPIs de alto nivel com tendencias
    - GET /api/executive/metrics - Metricas detalhadas
    - GET /api/executive/export/csv - Export de stories
    """
    from datetime import timedelta
    from fastapi import HTTPException

    @app.get("/api/executive/kpis")
    def get_executive_kpis(project_id: Optional[str] = None, days: int = 30):
        """
        Retorna KPIs executivos de alto nivel para o dashboard executivo.
        Inclui metricas principais, tendencias e comparacao com periodo anterior.
        """
        db = SessionLocal()
        try:
            end_date = datetime.utcnow()
            start_date = end_date - timedelta(days=days)
            prev_start = start_date - timedelta(days=days)

            # Query stories
            query = db.query(Story)
            if project_id:
                query = query.filter(Story.project_id == project_id)
            all_stories = query.all()

            # Periodo atual
            current_completed = [s for s in all_stories if s.status == 'done' and s.completed_at and s.completed_at >= start_date]
            current_in_progress = [s for s in all_stories if s.status == 'in_progress']

            # Periodo anterior
            prev_completed = [s for s in all_stories if s.status == 'done' and s.completed_at and prev_start <= s.completed_at < start_date]

            # KPIs principais
            total_stories = len(all_stories)
            completed_stories = len([s for s in all_stories if s.status == 'done'])
            completion_rate = (completed_stories / total_stories * 100) if total_stories > 0 else 0

            # Velocity - pontos entregues
            current_points = sum(s.story_points or 0 for s in current_completed)
            prev_points = sum(s.story_points or 0 for s in prev_completed)
            total_points = sum(s.story_points or 0 for s in all_stories)
            done_points = sum(s.story_points or 0 for s in all_stories if s.status == 'done')

            # Velocity media por sprint
            sprints = db.query(Sprint).filter(Sprint.status == 'completed').order_by(Sprint.end_date.desc()).limit(5).all()
            velocities = [s.velocity for s in sprints if s.velocity and s.velocity > 0]
            avg_velocity = sum(velocities) / len(velocities) if velocities else (current_points / max(1, days / 7))

            # Lead time medio (dias desde criacao ate conclusao)
            lead_times = []
            for s in [st for st in all_stories if st.status == 'done' and st.completed_at and st.created_at]:
                lead_times.append((s.completed_at - s.created_at).days)
            avg_lead_time = sum(lead_times) / len(lead_times) if lead_times else 0

            # Cycle time medio (dias desde inicio ate conclusao)
            cycle_times = []
            for s in [st for st in all_stories if st.status == 'done' and st.completed_at and st.started_at]:
                cycle_times.append((s.completed_at - s.started_at).days)
            avg_cycle_time = sum(cycle_times) / len(cycle_times) if cycle_times else 0

            # Throughput (stories por semana)
            weeks = max(1, days / 7)
            throughput = len(current_completed) / weeks
            prev_throughput = len(prev_completed) / weeks if prev_completed else 0

            # Comparacao com periodo anterior
            velocity_change = ((current_points - prev_points) / prev_points * 100) if prev_points > 0 else (100 if current_points > 0 else 0)
            throughput_change = ((throughput - prev_throughput) / prev_throughput * 100) if prev_throughput > 0 else (100 if throughput > 0 else 0)

            # Health score (0-100)
            health_factors = []
            if completion_rate > 0:
                health_factors.append(min(100, completion_rate))
            if avg_cycle_time > 0:
                health_factors.append(max(0, 100 - (avg_cycle_time - 5) * 5))
            if len(current_in_progress) <= 5:
                health_factors.append(100)
            else:
                health_factors.append(max(0, 100 - (len(current_in_progress) - 5) * 10))
            health_score = sum(health_factors) / len(health_factors) if health_factors else 50

            # Status distribution
            status_dist = {}
            for s in all_stories:
                status_dist[s.status] = status_dist.get(s.status, 0) + 1

            # Trend data (ultimos N dias agrupados por semana)
            trend_data = []
            for i in range(min(12, max(1, days // 7))):
                week_start = end_date - timedelta(days=(i + 1) * 7)
                week_end = end_date - timedelta(days=i * 7)
                week_stories = [s for s in all_stories if s.status == 'done' and s.completed_at and week_start <= s.completed_at < week_end]
                week_points = sum(s.story_points or 0 for s in week_stories)
                trend_data.insert(0, {
                    "week": i + 1,
                    "label": week_start.strftime("%d/%m"),
                    "stories": len(week_stories),
                    "points": week_points
                })

            return {
                "kpis": {
                    "completion_rate": round(completion_rate, 1),
                    "avg_velocity": round(avg_velocity, 1),
                    "avg_lead_time": round(avg_lead_time, 1),
                    "avg_cycle_time": round(avg_cycle_time, 1),
                    "throughput": round(throughput, 2),
                    "health_score": round(health_score, 0),
                    "total_stories": total_stories,
                    "completed_stories": completed_stories,
                    "in_progress": len(current_in_progress),
                    "total_points": total_points,
                    "done_points": done_points,
                    "wip_count": len(current_in_progress)
                },
                "comparison": {
                    "velocity_change": round(velocity_change, 1),
                    "throughput_change": round(throughput_change, 1),
                    "current_period_points": current_points,
                    "previous_period_points": prev_points,
                    "current_period_stories": len(current_completed),
                    "previous_period_stories": len(prev_completed)
                },
                "distribution": status_dist,
                "trend": trend_data,
                "period": {
                    "days": days,
                    "start_date": start_date.isoformat(),
                    "end_date": end_date.isoformat()
                },
                "generated_at": datetime.utcnow().isoformat()
            }
        except Exception as e:
            raise HTTPException(500, f"Erro ao calcular KPIs: {str(e)}")
        finally:
            db.close()

    @app.get("/api/executive/metrics")
    def get_executive_metrics(project_id: Optional[str] = None, period: str = "month", epic_id: Optional[str] = None):
        """Retorna metricas executivas detalhadas."""
        db = SessionLocal()
        try:
            return get_executive_metrics_data(db, Story, Sprint, project_id, period, epic_id)
        finally:
            db.close()

    @app.get("/api/executive/export/csv")
    def export_csv(project_id: Optional[str] = None):
        """Exporta stories para CSV."""
        db = SessionLocal()
        try:
            csv_content = export_stories_csv(db, Story, project_id)
            return Response(
                content=csv_content,
                media_type="text/csv",
                headers={"Content-Disposition": f"attachment; filename=stories_{datetime.utcnow().strftime('%Y%m%d')}.csv"}
            )
        finally:
            db.close()

    @app.get("/executive", response_class=HTMLResponse)
    def executive_dashboard_page():
        """Pagina do Dashboard Executivo com KPIs de alto nivel"""
        return EXECUTIVE_KPI_TEMPLATE

    print("[Dashboard] Executive Dashboard endpoints loaded: /executive, /api/executive/kpis, /api/executive/metrics")


def get_executive_metrics_data(db, Story, Sprint, project_id=None, period="month", epic_id=None):
    """Calcula metricas executivas"""
    now = datetime.utcnow()
    if period == "week":
        start_date = now - timedelta(days=7)
    elif period == "quarter":
        start_date = now - timedelta(days=90)
    else:
        start_date = now - timedelta(days=30)

    query = db.query(Story)
    if project_id:
        query = query.filter(Story.project_id == project_id)
    if epic_id:
        query = query.filter(Story.epic_id == epic_id)

    all_stories = query.all()
    done_stories = [s for s in all_stories if s.status == "done"]
    done_in_period = [s for s in done_stories if s.completed_at and s.completed_at >= start_date]

    total_points = sum(s.story_points or 0 for s in all_stories)
    completed_points = sum(s.story_points or 0 for s in done_stories)
    points_in_period = sum(s.story_points or 0 for s in done_in_period)

    status_counts = {}
    status_points = {}
    for s in all_stories:
        status = s.status or "backlog"
        status_counts[status] = status_counts.get(status, 0) + 1
        status_points[status] = status_points.get(status, 0) + (s.story_points or 0)

    lead_times = []
    for s in done_stories:
        if s.created_at and s.completed_at:
            lead_times.append((s.completed_at - s.created_at).total_seconds() / 86400)
    avg_lead_time = round(sum(lead_times) / len(lead_times), 1) if lead_times else 0

    cycle_times = []
    for s in done_stories:
        if s.started_at and s.completed_at:
            cycle_times.append((s.completed_at - s.started_at).total_seconds() / 86400)
    avg_cycle_time = round(sum(cycle_times) / len(cycle_times), 1) if cycle_times else 0

    weeks = max(1, (now - start_date).days / 7)
    throughput = round(len(done_in_period) / weeks, 1)

    sprints = db.query(Sprint)
    if project_id:
        sprints = sprints.filter(Sprint.project_id == project_id)
    sprints = sprints.order_by(Sprint.created_at.desc()).limit(10).all()

    velocity_data = []
    for sprint in reversed(sprints):
        sprint_stories = [s for s in done_stories if s.sprint_id == sprint.sprint_id]
        sprint_points = sum(s.story_points or 0 for s in sprint_stories)
        velocity_data.append({
            "sprint": sprint.name,
            "sprint_id": sprint.sprint_id,
            "velocity": sprint_points,
            "capacity": sprint.capacity or 0,
            "stories_count": len(sprint_stories)
        })

    velocities = [v["velocity"] for v in velocity_data if v["velocity"] > 0]
    avg_velocity = round(sum(velocities) / len(velocities), 1) if velocities else 0

    category_counts = {}
    for s in all_stories:
        cat = s.category or "feature"
        category_counts[cat] = category_counts.get(cat, 0) + 1

    priority_counts = {}
    for s in all_stories:
        prio = s.priority or "medium"
        priority_counts[prio] = priority_counts.get(prio, 0) + 1

    active_sprint = db.query(Sprint).filter(Sprint.status == "active")
    if project_id:
        active_sprint = active_sprint.filter(Sprint.project_id == project_id)
    active_sprint = active_sprint.first()

    burndown_data = []
    if active_sprint and active_sprint.start_date and active_sprint.end_date:
        sprint_stories = [s for s in all_stories if s.sprint_id == active_sprint.sprint_id]
        total_sprint_points = sum(s.story_points or 0 for s in sprint_stories)
        current_date = active_sprint.start_date
        end_date = min(active_sprint.end_date, now)
        while current_date <= end_date:
            completed_by_date = sum(
                s.story_points or 0
                for s in sprint_stories
                if s.completed_at and s.completed_at.date() <= current_date.date()
            )
            remaining = total_sprint_points - completed_by_date
            total_days = (active_sprint.end_date - active_sprint.start_date).days
            days_elapsed = (current_date - active_sprint.start_date).days
            ideal = total_sprint_points * (1 - days_elapsed / total_days) if total_days > 0 else 0
            burndown_data.append({
                "date": current_date.strftime("%Y-%m-%d"),
                "remaining": remaining,
                "ideal": round(ideal, 1)
            })
            current_date += timedelta(days=1)

    completed_timeline = []
    for i in range(30):
        date = now - timedelta(days=29-i)
        count = len([s for s in done_stories if s.completed_at and s.completed_at.date() == date.date()])
        points = sum(s.story_points or 0 for s in done_stories if s.completed_at and s.completed_at.date() == date.date())
        completed_timeline.append({"date": date.strftime("%Y-%m-%d"), "count": count, "points": points})

    return {
        "summary": {
            "total_stories": len(all_stories),
            "done_stories": len(done_stories),
            "in_progress_stories": status_counts.get("in_progress", 0),
            "total_points": total_points,
            "completed_points": completed_points,
            "completion_rate": round(len(done_stories) / len(all_stories) * 100, 1) if all_stories else 0
        },
        "kpis": {
            "avg_velocity": avg_velocity,
            "avg_lead_time": avg_lead_time,
            "avg_cycle_time": avg_cycle_time,
            "throughput": throughput,
            "points_per_period": points_in_period,
            "stories_per_period": len(done_in_period)
        },
        "status_distribution": status_counts,
        "status_points": status_points,
        "category_distribution": category_counts,
        "priority_distribution": priority_counts,
        "velocity_trend": velocity_data,
        "burndown": burndown_data,
        "completed_timeline": completed_timeline,
        "active_sprint": active_sprint.to_dict() if active_sprint else None,
        "period": period,
        "start_date": start_date.isoformat(),
        "end_date": now.isoformat()
    }


def export_stories_csv(db, Story, project_id=None):
    """Exporta stories para CSV"""
    query = db.query(Story)
    if project_id:
        query = query.filter(Story.project_id == project_id)
    stories = query.all()

    output = StringIO()
    writer = csv.writer(output)
    writer.writerow([
        "Story ID", "Titulo", "Status", "Prioridade", "Story Points",
        "Epic", "Sprint", "Assignee", "Criado Em", "Iniciado Em",
        "Completado Em", "Lead Time (dias)", "Cycle Time (dias)"
    ])

    for s in stories:
        lead_time = round((s.completed_at - s.created_at).total_seconds() / 86400, 1) if s.created_at and s.completed_at else ""
        cycle_time = round((s.completed_at - s.started_at).total_seconds() / 86400, 1) if s.started_at and s.completed_at else ""
        writer.writerow([
            s.story_id, s.title, s.status, s.priority, s.story_points,
            s.epic_id or "", s.sprint_id or "", s.assignee or "",
            s.created_at.strftime("%Y-%m-%d %H:%M") if s.created_at else "",
            s.started_at.strftime("%Y-%m-%d %H:%M") if s.started_at else "",
            s.completed_at.strftime("%Y-%m-%d %H:%M") if s.completed_at else "",
            lead_time, cycle_time
        ])

    return output.getvalue()


# HTML Template for Executive Dashboard
EXECUTIVE_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Executive Dashboard - Fabrica de Agentes</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/vue@3.4.5/dist/vue.global.prod.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/html2pdf.js/0.10.1/html2pdf.bundle.min.js"></script>
    <style>
        :root { --belgo-blue: #003B4A; --belgo-orange: #FF6C00; --gray-50: #F9FAFB; --gray-100: #F3F4F6; --gray-200: #E5E7EB; --gray-300: #D1D5DB; --gray-400: #9CA3AF; --gray-500: #6B7280; --gray-600: #4B5563; --gray-700: #374151; --gray-800: #1F2937; --gray-900: #111827; --green-500: #10B981; --green-600: #059669; --red-500: #EF4444; --yellow-500: #F59E0B; --blue-500: #3B82F6; --purple-500: #8B5CF6; }
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; background: var(--gray-100); color: var(--gray-800); min-height: 100vh; }
        .dark body, body.dark { background: var(--gray-900); color: var(--gray-100); }
        .header { background: linear-gradient(135deg, var(--belgo-blue) 0%, #00546B 100%); color: white; padding: 1.5rem 2rem; display: flex; justify-content: space-between; align-items: center; }
        .header h1 { font-size: 1.5rem; font-weight: 600; display: flex; align-items: center; gap: 0.75rem; }
        .header-actions { display: flex; gap: 1rem; align-items: center; }
        .btn { padding: 0.5rem 1rem; border-radius: 0.5rem; font-weight: 500; cursor: pointer; transition: all 0.2s; display: inline-flex; align-items: center; gap: 0.5rem; border: none; font-size: 0.875rem; }
        .btn-primary { background: var(--belgo-orange); color: white; }
        .btn-primary:hover { background: #E55A00; }
        .btn-secondary { background: rgba(255,255,255,0.1); color: white; border: 1px solid rgba(255,255,255,0.2); }
        .btn-secondary:hover { background: rgba(255,255,255,0.2); }
        .btn-outline { background: white; color: var(--belgo-blue); border: 1px solid var(--gray-300); }
        .filters { background: white; padding: 1rem 2rem; display: flex; gap: 1.5rem; align-items: center; border-bottom: 1px solid var(--gray-200); flex-wrap: wrap; }
        .dark .filters { background: var(--gray-800); border-color: var(--gray-700); }
        .filter-group { display: flex; flex-direction: column; gap: 0.25rem; }
        .filter-group label { font-size: 0.75rem; font-weight: 600; color: var(--gray-500); text-transform: uppercase; }
        .filter-group select { padding: 0.5rem 2rem 0.5rem 0.75rem; border: 1px solid var(--gray-300); border-radius: 0.375rem; background: white; font-size: 0.875rem; min-width: 180px; }
        .dark .filter-group select { background: var(--gray-700); border-color: var(--gray-600); color: var(--gray-100); }
        .main { padding: 2rem; max-width: 1600px; margin: 0 auto; }
        .kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 1.5rem; margin-bottom: 2rem; }
        .kpi-card { background: white; border-radius: 1rem; padding: 1.5rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); transition: transform 0.2s; }
        .kpi-card:hover { transform: translateY(-2px); }
        .dark .kpi-card { background: var(--gray-800); }
        .kpi-card .icon { width: 48px; height: 48px; border-radius: 0.75rem; display: flex; align-items: center; justify-content: center; margin-bottom: 1rem; }
        .kpi-card .icon.blue { background: rgba(59, 130, 246, 0.1); color: var(--blue-500); }
        .kpi-card .icon.green { background: rgba(16, 185, 129, 0.1); color: var(--green-500); }
        .kpi-card .icon.orange { background: rgba(255, 108, 0, 0.1); color: var(--belgo-orange); }
        .kpi-card .icon.purple { background: rgba(139, 92, 246, 0.1); color: var(--purple-500); }
        .kpi-card .value { font-size: 2.5rem; font-weight: 700; color: var(--gray-900); line-height: 1; margin-bottom: 0.5rem; }
        .dark .kpi-card .value { color: white; }
        .kpi-card .label { font-size: 0.875rem; color: var(--gray-500); font-weight: 500; }
        .kpi-card .trend { display: flex; align-items: center; gap: 0.25rem; font-size: 0.75rem; margin-top: 0.75rem; padding-top: 0.75rem; border-top: 1px solid var(--gray-100); }
        .dark .kpi-card .trend { border-color: var(--gray-700); }
        .kpi-card .trend.up { color: var(--green-500); }
        .chart-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 1.5rem; margin-bottom: 2rem; }
        @media (max-width: 1200px) { .chart-grid { grid-template-columns: 1fr; } }
        .chart-card { background: white; border-radius: 1rem; padding: 1.5rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
        .dark .chart-card { background: var(--gray-800); }
        .chart-card h3 { font-size: 1rem; font-weight: 600; margin-bottom: 1rem; color: var(--gray-700); display: flex; align-items: center; gap: 0.5rem; }
        .dark .chart-card h3 { color: var(--gray-300); }
        .chart-container { position: relative; height: 300px; }
        .chart-card.full-width { grid-column: span 2; }
        @media (max-width: 1200px) { .chart-card.full-width { grid-column: span 1; } }
        .summary-section { background: white; border-radius: 1rem; padding: 1.5rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 2rem; }
        .dark .summary-section { background: var(--gray-800); }
        .summary-section h3 { font-size: 1rem; font-weight: 600; margin-bottom: 1rem; color: var(--gray-700); }
        .dark .summary-section h3 { color: var(--gray-300); }
        .status-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 1rem; }
        .status-item { text-align: center; padding: 1rem; border-radius: 0.5rem; background: var(--gray-50); }
        .dark .status-item { background: var(--gray-700); }
        .status-item .count { font-size: 1.75rem; font-weight: 700; }
        .status-item .label { font-size: 0.75rem; color: var(--gray-500); text-transform: uppercase; margin-top: 0.25rem; }
        .status-item.backlog .count { color: var(--gray-500); }
        .status-item.ready .count { color: var(--blue-500); }
        .status-item.in_progress .count { color: var(--yellow-500); }
        .status-item.review .count { color: var(--purple-500); }
        .status-item.testing .count { color: var(--belgo-orange); }
        .status-item.done .count { color: var(--green-500); }
        .progress-bar-container { margin-top: 1.5rem; }
        .progress-bar-label { display: flex; justify-content: space-between; font-size: 0.875rem; margin-bottom: 0.5rem; }
        .progress-bar { height: 12px; background: var(--gray-200); border-radius: 6px; overflow: hidden; }
        .dark .progress-bar { background: var(--gray-700); }
        .progress-bar-fill { height: 100%; background: linear-gradient(90deg, var(--green-500), var(--green-600)); border-radius: 6px; transition: width 0.5s ease; }
        .loading { display: flex; align-items: center; justify-content: center; padding: 4rem; }
        .spinner { width: 40px; height: 40px; border: 3px solid var(--gray-200); border-top-color: var(--belgo-orange); border-radius: 50%; animation: spin 1s linear infinite; }
        @keyframes spin { to { transform: rotate(360deg); } }
        .back-link { color: rgba(255,255,255,0.8); text-decoration: none; display: flex; align-items: center; gap: 0.5rem; font-size: 0.875rem; }
        .back-link:hover { color: white; }
        .sprint-info { background: linear-gradient(135deg, var(--belgo-blue), #00546B); color: white; padding: 1.5rem; border-radius: 1rem; margin-bottom: 2rem; }
        .sprint-info h2 { font-size: 1.25rem; margin-bottom: 0.5rem; }
        .sprint-info .goal { font-size: 0.875rem; opacity: 0.9; margin-bottom: 1rem; }
        .sprint-info .dates { display: flex; gap: 2rem; font-size: 0.875rem; flex-wrap: wrap; }
        .sprint-info .dates span { display: flex; align-items: center; gap: 0.5rem; }
        @media print { .no-print { display: none !important; } }
    </style>
</head>
<body :class="{ dark: isDarkMode }">
    <div id="app">
        <header class="header">
            <div style="display: flex; align-items: center; gap: 2rem;">
                <a href="/" class="back-link no-print"><svg width="20" height="20" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M9.707 16.707a1 1 0 01-1.414 0l-6-6a1 1 0 010-1.414l6-6a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l4.293 4.293a1 1 0 010 1.414z" clip-rule="evenodd"/></svg> Voltar</a>
                <h1><svg width="28" height="28" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M3 3v18h18"/><path d="M18 17V9"/><path d="M13 17V5"/><path d="M8 17v-3"/></svg> Executive Dashboard</h1>
            </div>
            <div class="header-actions no-print">
                <button class="btn btn-secondary" @click="toggleDarkMode"><svg v-if="!isDarkMode" width="20" height="20" viewBox="0 0 20 20" fill="currentColor"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"/></svg><svg v-else width="20" height="20" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" clip-rule="evenodd"/></svg></button>
                <button class="btn btn-secondary" @click="exportPDF"><svg width="20" height="20" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm5 6a1 1 0 10-2 0v3.586l-1.293-1.293a1 1 0 10-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L11 11.586V8z" clip-rule="evenodd"/></svg> PDF</button>
                <button class="btn btn-primary" @click="exportCSV"><svg width="20" height="20" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm3.293-7.707a1 1 0 011.414 0L9 10.586V3a1 1 0 112 0v7.586l1.293-1.293a1 1 0 111.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z" clip-rule="evenodd"/></svg> CSV</button>
            </div>
        </header>
        <div class="filters no-print">
            <div class="filter-group"><label>Projeto</label><select v-model="selectedProject" @change="loadMetrics"><option value="">Todos</option><option v-for="p in projects" :key="p.project_id" :value="p.project_id">{{ p.name }}</option></select></div>
            <div class="filter-group"><label>Periodo</label><select v-model="selectedPeriod" @change="loadMetrics"><option value="week">Semana</option><option value="month">Mes</option><option value="quarter">Trimestre</option></select></div>
            <div class="filter-group"><label>Epic</label><select v-model="selectedEpic" @change="loadMetrics"><option value="">Todos</option><option v-for="e in epics" :key="e.epic_id" :value="e.epic_id">{{ e.title }}</option></select></div>
            <button class="btn btn-outline" @click="loadMetrics" style="margin-top: 1rem;"><svg width="16" height="16" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1z" clip-rule="evenodd"/></svg> Atualizar</button>
        </div>
        <main class="main" id="dashboard-content">
            <div v-if="loading" class="loading"><div class="spinner"></div></div>
            <template v-else>
                <div v-if="metrics.active_sprint" class="sprint-info">
                    <h2>{{ metrics.active_sprint.name }}</h2>
                    <p class="goal" v-if="metrics.active_sprint.goal">{{ metrics.active_sprint.goal }}</p>
                    <div class="dates"><span>Inicio: {{ formatDate(metrics.active_sprint.start_date) }}</span><span>Fim: {{ formatDate(metrics.active_sprint.end_date) }}</span><span>Capacidade: {{ metrics.active_sprint.capacity || 0 }} pts</span></div>
                </div>
                <div class="kpi-grid">
                    <div class="kpi-card"><div class="icon blue"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/></svg></div><div class="value">{{ metrics.kpis?.avg_velocity || 0 }}</div><div class="label">Velocity Medio (pts/sprint)</div><div class="trend up" v-if="metrics.velocity_trend?.length > 1">Baseado em {{ metrics.velocity_trend.length }} sprints</div></div>
                    <div class="kpi-card"><div class="icon green"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><polyline points="12,6 12,12 16,14"/></svg></div><div class="value">{{ metrics.kpis?.avg_lead_time || 0 }}d</div><div class="label">Lead Time Medio</div><div class="trend">Backlog ate Done</div></div>
                    <div class="kpi-card"><div class="icon orange"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M12 2v20M17 5H9.5a3.5 3.5 0 000 7h5a3.5 3.5 0 010 7H6"/></svg></div><div class="value">{{ metrics.kpis?.avg_cycle_time || 0 }}d</div><div class="label">Cycle Time Medio</div><div class="trend">In Progress ate Done</div></div>
                    <div class="kpi-card"><div class="icon purple"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M22 12h-4l-3 9L9 3l-3 9H2"/></svg></div><div class="value">{{ metrics.kpis?.throughput || 0 }}</div><div class="label">Throughput (stories/semana)</div><div class="trend">{{ metrics.kpis?.stories_per_period || 0 }} no periodo</div></div>
                    <div class="kpi-card"><div class="icon green"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M9 12l2 2 4-4"/><circle cx="12" cy="12" r="10"/></svg></div><div class="value">{{ metrics.summary?.done_stories || 0 }}</div><div class="label">Stories Completas</div><div class="trend up">{{ metrics.summary?.completed_points || 0 }} pts</div></div>
                    <div class="kpi-card"><div class="icon blue"><svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M16 21v-2a4 4 0 00-4-4H5a4 4 0 00-4 4v2"/><circle cx="8.5" cy="7" r="4"/><line x1="20" y1="8" x2="20" y2="14"/><line x1="23" y1="11" x2="17" y2="11"/></svg></div><div class="value">{{ metrics.summary?.total_stories || 0 }}</div><div class="label">Total de Stories</div><div class="trend">{{ metrics.summary?.total_points || 0 }} pts totais</div></div>
                </div>
                <div class="summary-section">
                    <h3>Distribuicao por Status</h3>
                    <div class="status-grid">
                        <div class="status-item backlog"><div class="count">{{ metrics.status_distribution?.backlog || 0 }}</div><div class="label">Backlog</div></div>
                        <div class="status-item ready"><div class="count">{{ metrics.status_distribution?.ready || 0 }}</div><div class="label">Ready</div></div>
                        <div class="status-item in_progress"><div class="count">{{ metrics.status_distribution?.in_progress || 0 }}</div><div class="label">In Progress</div></div>
                        <div class="status-item review"><div class="count">{{ metrics.status_distribution?.review || 0 }}</div><div class="label">Review</div></div>
                        <div class="status-item testing"><div class="count">{{ metrics.status_distribution?.testing || 0 }}</div><div class="label">Testing</div></div>
                        <div class="status-item done"><div class="count">{{ metrics.status_distribution?.done || 0 }}</div><div class="label">Done</div></div>
                    </div>
                    <div class="progress-bar-container"><div class="progress-bar-label"><span>Progresso Geral</span><span>{{ metrics.summary?.completion_rate || 0 }}%</span></div><div class="progress-bar"><div class="progress-bar-fill" :style="{ width: (metrics.summary?.completion_rate || 0) + '%' }"></div></div></div>
                </div>
                <div class="chart-grid">
                    <div class="chart-card"><h3>Velocity por Sprint</h3><div class="chart-container"><canvas ref="velocityChart"></canvas></div></div>
                    <div class="chart-card"><h3>Stories por Status</h3><div class="chart-container"><canvas ref="statusChart"></canvas></div></div>
                    <div class="chart-card"><h3>Capacity vs Velocity</h3><div class="chart-container"><canvas ref="pointsChart"></canvas></div></div>
                    <div class="chart-card"><h3>Burndown Sprint</h3><div class="chart-container"><canvas ref="burndownChart"></canvas></div></div>
                    <div class="chart-card full-width"><h3>Burnup - 30 Dias</h3><div class="chart-container"><canvas ref="burnupChart"></canvas></div></div>
                </div>
            </template>
        </main>
    </div>
    <script>
    const { createApp, ref, onMounted, watch, nextTick } = Vue;
    createApp({
        setup() {
            const loading = ref(true), isDarkMode = ref(false), metrics = ref({}), projects = ref([]), epics = ref([]);
            const selectedProject = ref(''), selectedPeriod = ref('month'), selectedEpic = ref('');
            const velocityChart = ref(null), statusChart = ref(null), pointsChart = ref(null), burndownChart = ref(null), burnupChart = ref(null);
            let vCI = null, sCI = null, pCI = null, bCI = null, buCI = null;
            const loadProjects = async () => { try { projects.value = await (await fetch('/api/projects')).json(); } catch(e) {} };
            const loadEpics = async () => { try { epics.value = selectedProject.value ? await (await fetch('/api/projects/'+selectedProject.value+'/epics')).json() : []; } catch(e) {} };
            const loadMetrics = async () => {
                loading.value = true;
                try {
                    const p = new URLSearchParams();
                    if (selectedProject.value) p.append('project_id', selectedProject.value);
                    if (selectedPeriod.value) p.append('period', selectedPeriod.value);
                    if (selectedEpic.value) p.append('epic_id', selectedEpic.value);
                    metrics.value = await (await fetch('/api/executive/metrics?'+p)).json();
                    await nextTick(); renderCharts();
                } catch(e) {} finally { loading.value = false; }
            };
            const renderCharts = () => {
                const dk = isDarkMode.value, gc = dk ? 'rgba(255,255,255,0.1)' : 'rgba(0,0,0,0.1)', tc = dk ? '#9CA3AF' : '#6B7280';
                if (vCI) vCI.destroy(); if (sCI) sCI.destroy(); if (pCI) pCI.destroy(); if (bCI) bCI.destroy(); if (buCI) buCI.destroy();
                if (velocityChart.value && metrics.value.velocity_trend?.length) {
                    vCI = new Chart(velocityChart.value.getContext('2d'), { type: 'line', data: { labels: metrics.value.velocity_trend.map(v=>v.sprint), datasets: [{ label: 'Velocity', data: metrics.value.velocity_trend.map(v=>v.velocity), borderColor: '#3B82F6', backgroundColor: 'rgba(59,130,246,0.1)', fill: true, tension: 0.4 }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { display: false } }, scales: { y: { beginAtZero: true, grid: { color: gc }, ticks: { color: tc } }, x: { grid: { color: gc }, ticks: { color: tc } } } } });
                }
                if (statusChart.value && metrics.value.status_distribution) {
                    const sd = metrics.value.status_distribution, lb = Object.keys(sd), dt = Object.values(sd);
                    const cl = { backlog: '#9CA3AF', ready: '#3B82F6', in_progress: '#F59E0B', review: '#8B5CF6', testing: '#FF6C00', done: '#10B981' };
                    sCI = new Chart(statusChart.value.getContext('2d'), { type: 'doughnut', data: { labels: lb.map(l=>l.replace('_',' ').toUpperCase()), datasets: [{ data: dt, backgroundColor: lb.map(l=>cl[l]||'#6B7280'), borderWidth: 2, borderColor: dk ? '#1F2937' : '#FFF' }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { position: 'right', labels: { color: tc } } } } });
                }
                if (pointsChart.value && metrics.value.velocity_trend?.length) {
                    pCI = new Chart(pointsChart.value.getContext('2d'), { type: 'bar', data: { labels: metrics.value.velocity_trend.map(v=>v.sprint), datasets: [{ label: 'Capacity', data: metrics.value.velocity_trend.map(v=>v.capacity), backgroundColor: 'rgba(59,130,246,0.5)', borderColor: '#3B82F6', borderWidth: 1 }, { label: 'Velocity', data: metrics.value.velocity_trend.map(v=>v.velocity), backgroundColor: 'rgba(16,185,129,0.5)', borderColor: '#10B981', borderWidth: 1 }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { position: 'top', labels: { color: tc } } }, scales: { y: { beginAtZero: true, grid: { color: gc }, ticks: { color: tc } }, x: { grid: { color: gc }, ticks: { color: tc } } } } });
                }
                if (burndownChart.value) {
                    if (metrics.value.burndown?.length) {
                        bCI = new Chart(burndownChart.value.getContext('2d'), { type: 'line', data: { labels: metrics.value.burndown.map(b=>b.date.slice(5)), datasets: [{ label: 'Ideal', data: metrics.value.burndown.map(b=>b.ideal), borderColor: '#9CA3AF', borderDash: [5,5], fill: false, pointRadius: 0 }, { label: 'Remaining', data: metrics.value.burndown.map(b=>b.remaining), borderColor: '#FF6C00', backgroundColor: 'rgba(255,108,0,0.1)', fill: true, tension: 0.2 }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { position: 'top', labels: { color: tc } } }, scales: { y: { beginAtZero: true, grid: { color: gc }, ticks: { color: tc } }, x: { grid: { color: gc }, ticks: { color: tc } } } } });
                    } else {
                        bCI = new Chart(burndownChart.value.getContext('2d'), { type: 'line', data: { labels: ['Sem dados'], datasets: [{ data: [0], borderColor: '#9CA3AF' }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { display: false }, title: { display: true, text: 'Nenhum sprint ativo', color: tc } } } });
                    }
                }
                if (burnupChart.value && metrics.value.completed_timeline?.length) {
                    let cum = 0; const cd = metrics.value.completed_timeline.map(t => { cum += t.points; return cum; });
                    buCI = new Chart(burnupChart.value.getContext('2d'), { type: 'line', data: { labels: metrics.value.completed_timeline.map(t=>t.date.slice(5)), datasets: [{ label: 'Pontos Acumulados', data: cd, borderColor: '#10B981', backgroundColor: 'rgba(16,185,129,0.1)', fill: true, tension: 0.4, pointRadius: 2 }] }, options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { position: 'top', labels: { color: tc } } }, scales: { y: { beginAtZero: true, grid: { color: gc }, ticks: { color: tc } }, x: { grid: { color: gc }, ticks: { color: tc, maxTicksLimit: 15 } } } } });
                }
            };
            const toggleDarkMode = () => { isDarkMode.value = !isDarkMode.value; localStorage.setItem('exec_dark', isDarkMode.value); nextTick(renderCharts); };
            const formatDate = (s) => s ? new Date(s).toLocaleDateString('pt-BR') : '-';
            const exportPDF = () => { html2pdf().set({ margin: 10, filename: 'executive_'+new Date().toISOString().slice(0,10)+'.pdf', image: { type: 'jpeg', quality: 0.98 }, html2canvas: { scale: 2 }, jsPDF: { unit: 'mm', format: 'a4', orientation: 'landscape' } }).from(document.getElementById('dashboard-content')).save(); };
            const exportCSV = () => { const p = new URLSearchParams(); if (selectedProject.value) p.append('project_id', selectedProject.value); window.location.href = '/api/executive/export/csv?'+p; };
            watch(selectedProject, () => { loadEpics(); selectedEpic.value = ''; });
            onMounted(async () => { isDarkMode.value = localStorage.getItem('exec_dark') === 'true'; await loadProjects(); await loadMetrics(); });
            return { loading, isDarkMode, metrics, projects, epics, selectedProject, selectedPeriod, selectedEpic, velocityChart, statusChart, pointsChart, burndownChart, burnupChart, loadMetrics, toggleDarkMode, formatDate, exportPDF, exportCSV };
        }
    }).mount('#app');
    </script>
</body>
</html>'''


def get_executive_template():
    """Returns the executive dashboard HTML template"""
    return EXECUTIVE_TEMPLATE


# Template HTML do Dashboard Executivo com KPIs de Alto Nivel
EXECUTIVE_KPI_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Dashboard Executivo - Fabrica de Agentes</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <style>
        * { font-family: 'Inter', sans-serif; }
        :root { --belgo-blue: #003B4A; --belgo-orange: #FF6C00; }
        body { background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); min-height: 100vh; }
        .kpi-card { background: white; border-radius: 16px; padding: 24px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1); transition: all 0.3s ease; position: relative; overflow: hidden; }
        .kpi-card:hover { transform: translateY(-4px); box-shadow: 0 12px 20px -5px rgba(0, 0, 0, 0.15); }
        .kpi-card::before { content: ''; position: absolute; top: 0; left: 0; right: 0; height: 4px; }
        .kpi-card.primary::before { background: linear-gradient(90deg, #003B4A, #005566); }
        .kpi-card.success::before { background: linear-gradient(90deg, #10B981, #059669); }
        .kpi-card.warning::before { background: linear-gradient(90deg, #F59E0B, #D97706); }
        .kpi-card.info::before { background: linear-gradient(90deg, #3B82F6, #2563EB); }
        .kpi-card.orange::before { background: linear-gradient(90deg, #FF6C00, #E56000); }
        .kpi-value { font-size: 2.5rem; font-weight: 700; color: #003B4A; line-height: 1; }
        .kpi-label { font-size: 0.875rem; color: #6B7280; margin-top: 8px; }
        .kpi-change { display: inline-flex; align-items: center; gap: 4px; font-size: 0.75rem; padding: 4px 8px; border-radius: 12px; margin-top: 12px; }
        .kpi-change.positive { background: #D1FAE5; color: #059669; }
        .kpi-change.negative { background: #FEE2E2; color: #DC2626; }
        .kpi-change.neutral { background: #F3F4F6; color: #6B7280; }
        .chart-container { background: white; border-radius: 16px; padding: 24px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1); }
        .chart-title { font-size: 1rem; font-weight: 600; color: #374151; margin-bottom: 16px; display: flex; align-items: center; gap: 8px; }
        .health-indicator { width: 100px; height: 100px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 1.5rem; font-weight: 700; position: relative; }
        .health-indicator::after { content: ''; position: absolute; inset: 6px; border-radius: 50%; background: white; }
        .health-indicator span { position: relative; z-index: 1; color: #003B4A; }
        .health-good { background: conic-gradient(#10B981 var(--health-pct), #E5E7EB 0); }
        .health-warning { background: conic-gradient(#F59E0B var(--health-pct), #E5E7EB 0); }
        .health-danger { background: conic-gradient(#EF4444 var(--health-pct), #E5E7EB 0); }
        .period-selector { display: flex; gap: 8px; background: white; padding: 4px; border-radius: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
        .period-btn { padding: 8px 16px; border-radius: 8px; font-size: 0.875rem; font-weight: 500; color: #6B7280; cursor: pointer; transition: all 0.2s; border: none; background: transparent; }
        .period-btn:hover { background: #F3F4F6; }
        .period-btn.active { background: #003B4A; color: white; }
        .comparison-row { display: flex; justify-content: space-between; padding: 12px 0; border-bottom: 1px solid #E5E7EB; }
        .comparison-row:last-child { border-bottom: none; }
    </style>
</head>
<body>
    <div id="app" class="min-h-screen">
        <header class="bg-gradient-to-r from-[#003B4A] to-[#005566] text-white py-6 px-8 shadow-lg">
            <div class="max-w-7xl mx-auto flex items-center justify-between flex-wrap gap-4">
                <div>
                    <h1 class="text-2xl font-bold">Dashboard Executivo</h1>
                    <p class="text-sm opacity-80 mt-1">Visao de alto nivel do projeto</p>
                </div>
                <div class="flex items-center gap-4 flex-wrap">
                    <div class="period-selector">
                        <button :class="['period-btn', selectedDays === 7 ? 'active' : '']" @click="loadKPIs(7)">7 dias</button>
                        <button :class="['period-btn', selectedDays === 14 ? 'active' : '']" @click="loadKPIs(14)">14 dias</button>
                        <button :class="['period-btn', selectedDays === 30 ? 'active' : '']" @click="loadKPIs(30)">30 dias</button>
                        <button :class="['period-btn', selectedDays === 90 ? 'active' : '']" @click="loadKPIs(90)">90 dias</button>
                    </div>
                    <a href="/" class="flex items-center gap-2 bg-white/10 hover:bg-white/20 px-4 py-2 rounded-lg transition">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"/></svg>
                        Voltar
                    </a>
                </div>
            </div>
        </header>
        <div v-if="loading" class="flex items-center justify-center py-20">
            <div class="animate-spin rounded-full h-12 w-12 border-4 border-[#003B4A] border-t-transparent"></div>
        </div>
        <main v-else class="max-w-7xl mx-auto px-8 py-8">
            <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-6 gap-6 mb-8">
                <div class="kpi-card primary">
                    <div class="kpi-value">{{ kpis.completion_rate }}%</div>
                    <div class="kpi-label">Taxa de Conclusao</div>
                    <div class="text-xs text-gray-400 mt-2">{{ kpis.completed_stories }} de {{ kpis.total_stories }} stories</div>
                </div>
                <div class="kpi-card orange">
                    <div class="kpi-value">{{ kpis.avg_velocity }}</div>
                    <div class="kpi-label">Velocity Media</div>
                    <div :class="['kpi-change', comparison.velocity_change > 0 ? 'positive' : comparison.velocity_change < 0 ? 'negative' : 'neutral']">
                        <span v-if="comparison.velocity_change > 0">+</span>{{ comparison.velocity_change }}% vs anterior
                    </div>
                </div>
                <div class="kpi-card info">
                    <div class="kpi-value">{{ kpis.avg_lead_time }}</div>
                    <div class="kpi-label">Lead Time Medio</div>
                    <div class="text-xs text-gray-400 mt-2">dias (criacao ate entrega)</div>
                </div>
                <div class="kpi-card warning">
                    <div class="kpi-value">{{ kpis.avg_cycle_time }}</div>
                    <div class="kpi-label">Cycle Time Medio</div>
                    <div class="text-xs text-gray-400 mt-2">dias (inicio ate entrega)</div>
                </div>
                <div class="kpi-card success">
                    <div class="kpi-value">{{ kpis.throughput }}</div>
                    <div class="kpi-label">Throughput</div>
                    <div :class="['kpi-change', comparison.throughput_change > 0 ? 'positive' : comparison.throughput_change < 0 ? 'negative' : 'neutral']">
                        <span v-if="comparison.throughput_change > 0">+</span>{{ comparison.throughput_change }}% vs anterior
                    </div>
                </div>
                <div class="kpi-card primary">
                    <div class="flex items-center gap-4">
                        <div :class="['health-indicator', kpis.health_score >= 70 ? 'health-good' : kpis.health_score >= 40 ? 'health-warning' : 'health-danger']" :style="{'--health-pct': kpis.health_score + '%'}">
                            <span>{{ kpis.health_score }}</span>
                        </div>
                        <div>
                            <div class="kpi-label">Health Score</div>
                            <div class="text-xs text-gray-400 mt-1">{{ kpis.health_score >= 70 ? 'Saudavel' : kpis.health_score >= 40 ? 'Atencao' : 'Critico' }}</div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
                <div class="chart-container">
                    <div class="chart-title">
                        <svg class="w-5 h-5 text-[#FF6C00]" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 7h8m0 0v8m0-8l-8 8-4-4-6 6"/></svg>
                        Tendencia de Velocity
                    </div>
                    <canvas id="velocityChart" height="250"></canvas>
                </div>
                <div class="chart-container">
                    <div class="chart-title">
                        <svg class="w-5 h-5 text-[#10B981]" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/></svg>
                        Stories Completadas por Semana
                    </div>
                    <canvas id="completionChart" height="250"></canvas>
                </div>
            </div>
            <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
                <div class="chart-container">
                    <div class="chart-title">
                        <svg class="w-5 h-5 text-[#003B4A]" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 3.055A9.001 9.001 0 1020.945 13H11V3.055z"/></svg>
                        Distribuicao por Status
                    </div>
                    <canvas id="statusChart" height="200"></canvas>
                </div>
                <div class="chart-container">
                    <div class="chart-title">
                        <svg class="w-5 h-5 text-[#3B82F6]" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/></svg>
                        Comparacao com Periodo Anterior
                    </div>
                    <div class="space-y-3 mt-4">
                        <div class="comparison-row"><span class="text-gray-600">Stories Completadas</span><div class="flex items-center gap-2"><span class="font-semibold">{{ comparison.current_period_stories }}</span><span class="text-gray-400 text-sm">vs {{ comparison.previous_period_stories }}</span></div></div>
                        <div class="comparison-row"><span class="text-gray-600">Pontos Entregues</span><div class="flex items-center gap-2"><span class="font-semibold">{{ comparison.current_period_points }}</span><span class="text-gray-400 text-sm">vs {{ comparison.previous_period_points }}</span></div></div>
                        <div class="comparison-row"><span class="text-gray-600">Variacao Velocity</span><span :class="['font-semibold', comparison.velocity_change > 0 ? 'text-green-600' : comparison.velocity_change < 0 ? 'text-red-600' : 'text-gray-600']">{{ comparison.velocity_change > 0 ? '+' : '' }}{{ comparison.velocity_change }}%</span></div>
                        <div class="comparison-row"><span class="text-gray-600">Variacao Throughput</span><span :class="['font-semibold', comparison.throughput_change > 0 ? 'text-green-600' : comparison.throughput_change < 0 ? 'text-red-600' : 'text-gray-600']">{{ comparison.throughput_change > 0 ? '+' : '' }}{{ comparison.throughput_change }}%</span></div>
                    </div>
                </div>
                <div class="chart-container">
                    <div class="chart-title">
                        <svg class="w-5 h-5 text-[#FF6C00]" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01"/></svg>
                        Resumo Rapido
                    </div>
                    <div class="space-y-4 mt-4">
                        <div class="flex items-center justify-between p-3 bg-gray-50 rounded-lg"><span class="text-gray-600">Total de Pontos</span><span class="font-bold text-[#003B4A]">{{ kpis.total_points }}</span></div>
                        <div class="flex items-center justify-between p-3 bg-green-50 rounded-lg"><span class="text-gray-600">Pontos Entregues</span><span class="font-bold text-green-600">{{ kpis.done_points }}</span></div>
                        <div class="flex items-center justify-between p-3 bg-yellow-50 rounded-lg"><span class="text-gray-600">Em Progresso (WIP)</span><span class="font-bold text-yellow-600">{{ kpis.wip_count }}</span></div>
                        <div class="flex items-center justify-between p-3 bg-blue-50 rounded-lg"><span class="text-gray-600">Periodo Analisado</span><span class="font-bold text-blue-600">{{ selectedDays }} dias</span></div>
                    </div>
                </div>
            </div>
            <div class="mt-8 text-center text-sm text-gray-500"><p>Ultima atualizacao: {{ lastUpdate }}</p></div>
        </main>
    </div>
    <script>
    const { createApp, ref, onMounted, nextTick } = Vue;
    createApp({
        setup() {
            const loading = ref(true);
            const selectedDays = ref(30);
            const kpis = ref({ completion_rate: 0, avg_velocity: 0, avg_lead_time: 0, avg_cycle_time: 0, throughput: 0, health_score: 0, total_stories: 0, completed_stories: 0, in_progress: 0, total_points: 0, done_points: 0, wip_count: 0 });
            const comparison = ref({ velocity_change: 0, throughput_change: 0, current_period_points: 0, previous_period_points: 0, current_period_stories: 0, previous_period_stories: 0 });
            const distribution = ref({});
            const trend = ref([]);
            const lastUpdate = ref('');
            let velocityChart = null, completionChart = null, statusChart = null;
            const loadKPIs = async (days) => {
                loading.value = true;
                selectedDays.value = days;
                try {
                    const res = await fetch(`/api/executive/kpis?days=${days}`);
                    const data = await res.json();
                    kpis.value = data.kpis;
                    comparison.value = data.comparison;
                    distribution.value = data.distribution;
                    trend.value = data.trend;
                    lastUpdate.value = new Date(data.generated_at).toLocaleString('pt-BR');
                    await nextTick();
                    renderCharts();
                } catch (e) { console.error('Error loading KPIs:', e); }
                finally { loading.value = false; }
            };
            const renderCharts = () => {
                const velocityCtx = document.getElementById('velocityChart');
                if (velocityCtx) {
                    if (velocityChart) velocityChart.destroy();
                    velocityChart = new Chart(velocityCtx, { type: 'line', data: { labels: trend.value.map(t => t.label), datasets: [{ label: 'Pontos', data: trend.value.map(t => t.points), borderColor: '#FF6C00', backgroundColor: 'rgba(255, 108, 0, 0.1)', fill: true, tension: 0.4 }] }, options: { responsive: true, plugins: { legend: { display: false } }, scales: { y: { beginAtZero: true } } } });
                }
                const completionCtx = document.getElementById('completionChart');
                if (completionCtx) {
                    if (completionChart) completionChart.destroy();
                    completionChart = new Chart(completionCtx, { type: 'bar', data: { labels: trend.value.map(t => t.label), datasets: [{ label: 'Stories', data: trend.value.map(t => t.stories), backgroundColor: '#10B981', borderRadius: 8 }] }, options: { responsive: true, plugins: { legend: { display: false } }, scales: { y: { beginAtZero: true } } } });
                }
                const statusCtx = document.getElementById('statusChart');
                if (statusCtx) {
                    if (statusChart) statusChart.destroy();
                    const statusLabels = { 'backlog': 'Backlog', 'ready': 'Ready', 'in_progress': 'Em Progresso', 'review': 'Revisao', 'testing': 'Testes', 'done': 'Concluido' };
                    const statusColors = { 'backlog': '#9CA3AF', 'ready': '#3B82F6', 'in_progress': '#F59E0B', 'review': '#8B5CF6', 'testing': '#EC4899', 'done': '#10B981' };
                    statusChart = new Chart(statusCtx, { type: 'doughnut', data: { labels: Object.keys(distribution.value).map(k => statusLabels[k] || k), datasets: [{ data: Object.values(distribution.value), backgroundColor: Object.keys(distribution.value).map(k => statusColors[k] || '#6B7280'), borderWidth: 0 }] }, options: { responsive: true, plugins: { legend: { position: 'bottom', labels: { padding: 16 } } } } });
                }
            };
            onMounted(() => { loadKPIs(30); });
            return { loading, selectedDays, kpis, comparison, distribution, trend, lastUpdate, loadKPIs };
        }
    }).mount('#app');
    </script>
</body>
</html>'''
