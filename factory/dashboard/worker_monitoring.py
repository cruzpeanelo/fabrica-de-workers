# -*- coding: utf-8 -*-
"""
Worker Monitoring Dashboard - Painel de Monitoramento de Workers
=================================================================

Issue #88 - Dashboard de Monitoramento de Workers

Este modulo fornece componentes para monitorar workers em tempo real:
- Status dos workers (idle, busy, offline)
- Jobs na fila
- Jobs processados
- Taxa de erro
- Graficos de jobs por hora
- Logs recentes

Componentes:
- worker_monitoring_dashboard(): Dashboard principal
- status_card(): Cards de status
- workers_table(): Tabela de workers
- jobs_timeline(): Grafico de jobs
- worker_logs(): Logs recentes
"""

import asyncio
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional

# Database
from factory.database.connection import SessionLocal
from factory.database.models import Worker, Job, JobStatus


# =============================================================================
# DATA FUNCTIONS
# =============================================================================

def get_active_workers_count() -> int:
    """Retorna quantidade de workers ativos (nao offline)"""
    db = SessionLocal()
    try:
        count = db.query(Worker).filter(Worker.status != "offline").count()
        return count
    finally:
        db.close()


def get_queue_size() -> int:
    """Retorna quantidade de jobs na fila (pending)"""
    db = SessionLocal()
    try:
        count = db.query(Job).filter(Job.status == "pending").count()
        return count
    finally:
        db.close()


def get_processed_count(hours: int = 24) -> int:
    """Retorna quantidade de jobs processados nas ultimas N horas"""
    db = SessionLocal()
    try:
        since = datetime.utcnow() - timedelta(hours=hours)
        count = db.query(Job).filter(
            Job.status == "completed",
            Job.completed_at >= since
        ).count()
        return count
    finally:
        db.close()


def get_error_rate(hours: int = 24) -> float:
    """Retorna taxa de erro nas ultimas N horas (0-100%)"""
    db = SessionLocal()
    try:
        since = datetime.utcnow() - timedelta(hours=hours)

        total = db.query(Job).filter(
            Job.status.in_(["completed", "failed"]),
            Job.completed_at >= since
        ).count()

        if total == 0:
            return 0.0

        failed = db.query(Job).filter(
            Job.status == "failed",
            Job.completed_at >= since
        ).count()

        return round((failed / total) * 100, 1)
    finally:
        db.close()


def get_workers_status() -> List[Dict[str, Any]]:
    """Retorna lista de workers com status"""
    db = SessionLocal()
    try:
        workers = db.query(Worker).all()

        result = []
        for w in workers:
            worker_dict = w.to_dict()

            # Calcular uptime
            if w.started_at:
                uptime_seconds = (datetime.utcnow() - w.started_at).total_seconds()
                hours = int(uptime_seconds // 3600)
                minutes = int((uptime_seconds % 3600) // 60)
                worker_dict["uptime"] = f"{hours}h {minutes}m"
            else:
                worker_dict["uptime"] = "N/A"

            result.append(worker_dict)

        return result
    finally:
        db.close()


def get_jobs_timeline(hours: int = 24) -> Dict[str, Any]:
    """Retorna dados para grafico de jobs por hora"""
    db = SessionLocal()
    try:
        now = datetime.utcnow()
        timeline = []

        for i in range(hours, -1, -1):
            hour_start = now.replace(minute=0, second=0, microsecond=0) - timedelta(hours=i)
            hour_end = hour_start + timedelta(hours=1)

            completed = db.query(Job).filter(
                Job.status == "completed",
                Job.completed_at >= hour_start,
                Job.completed_at < hour_end
            ).count()

            failed = db.query(Job).filter(
                Job.status == "failed",
                Job.completed_at >= hour_start,
                Job.completed_at < hour_end
            ).count()

            timeline.append({
                "hour": hour_start.strftime("%H:%M"),
                "completed": completed,
                "failed": failed,
                "total": completed + failed
            })

        return {
            "timeline": timeline,
            "period_hours": hours
        }
    finally:
        db.close()


def get_recent_logs(limit: int = 50) -> List[Dict[str, Any]]:
    """Retorna logs recentes dos workers"""
    db = SessionLocal()
    try:
        jobs = db.query(Job).filter(
            Job.status.in_(["completed", "failed", "running"])
        ).order_by(Job.updated_at.desc()).limit(limit).all()

        logs = []
        for job in jobs:
            step_logs = job.step_logs or []
            for log in step_logs[-5:]:  # Ultimos 5 logs de cada job
                logs.append({
                    "job_id": job.job_id,
                    "worker_id": job.worker_id,
                    "step": log.get("step"),
                    "message": log.get("message"),
                    "success": log.get("success", True),
                    "timestamp": log.get("timestamp")
                })

        # Ordenar por timestamp
        logs.sort(key=lambda x: x.get("timestamp", ""), reverse=True)
        return logs[:limit]
    finally:
        db.close()


def get_worker_stats() -> Dict[str, Any]:
    """Retorna estatisticas gerais dos workers"""
    db = SessionLocal()
    try:
        workers = db.query(Worker).all()

        total = len(workers)
        idle = sum(1 for w in workers if w.status == "idle")
        busy = sum(1 for w in workers if w.status == "busy")
        offline = sum(1 for w in workers if w.status == "offline")

        total_completed = sum(w.jobs_completed or 0 for w in workers)
        total_failed = sum(w.jobs_failed or 0 for w in workers)

        avg_duration = 0
        if workers:
            durations = [w.avg_job_duration for w in workers if w.avg_job_duration]
            if durations:
                avg_duration = sum(durations) / len(durations)

        return {
            "total": total,
            "idle": idle,
            "busy": busy,
            "offline": offline,
            "total_jobs_completed": total_completed,
            "total_jobs_failed": total_failed,
            "avg_job_duration_seconds": round(avg_duration, 1)
        }
    finally:
        db.close()


# =============================================================================
# UI COMPONENTS (HTML/JavaScript)
# =============================================================================

def generate_worker_monitoring_html() -> str:
    """
    Gera HTML do dashboard de monitoramento de workers

    Retorna HTML completo com estilos e scripts para monitoramento em tempo real.
    """
    return '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Worker Monitoring - Plataforma E</title>
    <style>
        :root {
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
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', 'Segoe UI', sans-serif;
            background: var(--bg);
            color: var(--text);
            line-height: 1.5;
        }

        .dashboard {
            max-width: 1400px;
            margin: 0 auto;
            padding: 24px;
        }

        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }

        .header h1 {
            font-size: 24px;
            color: var(--primary);
        }

        .refresh-info {
            display: flex;
            align-items: center;
            gap: 8px;
            color: var(--text-secondary);
            font-size: 14px;
        }

        .refresh-indicator {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            background: var(--success);
            animation: pulse 2s infinite;
        }

        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }

        /* Status Cards */
        .status-cards {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 16px;
            margin-bottom: 24px;
        }

        .status-card {
            background: var(--surface);
            border-radius: 12px;
            padding: 20px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            border-left: 4px solid var(--primary);
        }

        .status-card.success { border-color: var(--success); }
        .status-card.warning { border-color: var(--warning); }
        .status-card.danger { border-color: var(--danger); }
        .status-card.info { border-color: var(--info); }

        .status-card-value {
            font-size: 32px;
            font-weight: 700;
            color: var(--primary);
            margin-bottom: 4px;
        }

        .status-card.success .status-card-value { color: var(--success); }
        .status-card.warning .status-card-value { color: var(--warning); }
        .status-card.danger .status-card-value { color: var(--danger); }
        .status-card.info .status-card-value { color: var(--info); }

        .status-card-label {
            font-size: 14px;
            color: var(--text-secondary);
        }

        /* Grid Layout */
        .main-grid {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 24px;
        }

        /* Panel */
        .panel {
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            overflow: hidden;
        }

        .panel-header {
            padding: 16px 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .panel-title {
            font-size: 16px;
            font-weight: 600;
        }

        .panel-body {
            padding: 20px;
        }

        /* Workers Table */
        .workers-table {
            width: 100%;
            border-collapse: collapse;
        }

        .workers-table th,
        .workers-table td {
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }

        .workers-table th {
            background: var(--bg);
            font-weight: 600;
            font-size: 13px;
            color: var(--text-secondary);
            text-transform: uppercase;
        }

        .workers-table tr:hover {
            background: var(--bg);
        }

        .status-badge {
            display: inline-block;
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }

        .status-idle {
            background: #E0F2FE;
            color: #0369A1;
        }

        .status-busy {
            background: #FEF3C7;
            color: #B45309;
        }

        .status-offline {
            background: #FEE2E2;
            color: #DC2626;
        }

        /* Chart Container */
        .chart-container {
            height: 300px;
            position: relative;
        }

        /* Logs */
        .logs-container {
            max-height: 400px;
            overflow-y: auto;
            font-family: 'JetBrains Mono', 'Fira Code', monospace;
            font-size: 12px;
            background: #1F2937;
            color: #E5E7EB;
            border-radius: 8px;
            padding: 16px;
        }

        .log-entry {
            padding: 4px 0;
            border-bottom: 1px solid rgba(255,255,255,0.1);
        }

        .log-entry:last-child {
            border-bottom: none;
        }

        .log-time {
            color: #9CA3AF;
        }

        .log-job {
            color: #60A5FA;
        }

        .log-worker {
            color: #A78BFA;
        }

        .log-step {
            color: #34D399;
        }

        .log-message {
            color: #F3F4F6;
        }

        .log-error {
            color: #F87171;
        }

        /* Responsive */
        @media (max-width: 1200px) {
            .status-cards {
                grid-template-columns: repeat(2, 1fr);
            }
            .main-grid {
                grid-template-columns: 1fr;
            }
        }

        @media (max-width: 600px) {
            .status-cards {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <h1>Monitoramento de Workers</h1>
            <div class="refresh-info">
                <div class="refresh-indicator"></div>
                <span>Atualizando a cada 5s</span>
            </div>
        </div>

        <!-- Status Cards -->
        <div class="status-cards" id="status-cards">
            <div class="status-card success">
                <div class="status-card-value" id="active-workers">-</div>
                <div class="status-card-label">Workers Ativos</div>
            </div>
            <div class="status-card warning">
                <div class="status-card-value" id="queue-size">-</div>
                <div class="status-card-label">Jobs na Fila</div>
            </div>
            <div class="status-card info">
                <div class="status-card-value" id="processed-count">-</div>
                <div class="status-card-label">Jobs Processados (24h)</div>
            </div>
            <div class="status-card danger">
                <div class="status-card-value" id="error-rate">-</div>
                <div class="status-card-label">Taxa de Erro</div>
            </div>
        </div>

        <!-- Main Grid -->
        <div class="main-grid">
            <!-- Left Column -->
            <div>
                <!-- Workers Table -->
                <div class="panel" style="margin-bottom: 24px;">
                    <div class="panel-header">
                        <h3 class="panel-title">Workers</h3>
                    </div>
                    <div class="panel-body" style="padding: 0;">
                        <table class="workers-table">
                            <thead>
                                <tr>
                                    <th>ID</th>
                                    <th>Status</th>
                                    <th>Job Atual</th>
                                    <th>Uptime</th>
                                    <th>Jobs</th>
                                </tr>
                            </thead>
                            <tbody id="workers-tbody">
                                <tr>
                                    <td colspan="5" style="text-align: center; color: var(--text-secondary);">
                                        Carregando...
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <!-- Jobs Timeline Chart -->
                <div class="panel">
                    <div class="panel-header">
                        <h3 class="panel-title">Jobs nas Ultimas 24h</h3>
                    </div>
                    <div class="panel-body">
                        <div class="chart-container" id="jobs-chart">
                            <!-- Chart will be rendered here -->
                            <canvas id="timeline-canvas"></canvas>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Right Column -->
            <div>
                <!-- Logs -->
                <div class="panel">
                    <div class="panel-header">
                        <h3 class="panel-title">Logs Recentes</h3>
                    </div>
                    <div class="panel-body" style="padding: 0;">
                        <div class="logs-container" id="worker-logs">
                            <div class="log-entry">Carregando logs...</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script>
        // Auto-refresh
        const REFRESH_INTERVAL = 5000;

        // Fetch and update data
        async function updateDashboard() {
            try {
                // Fetch workers status
                const workersRes = await fetch('/api/monitoring/workers');
                const workersData = await workersRes.json();
                updateWorkersTable(workersData.workers);

                // Fetch stats
                const statsRes = await fetch('/api/monitoring/stats');
                const statsData = await statsRes.json();
                updateStatusCards(statsData.stats);

                // Fetch logs
                const logsRes = await fetch('/api/monitoring/logs');
                const logsData = await logsRes.json();
                updateLogs(logsData.logs);

                // Fetch timeline
                const timelineRes = await fetch('/api/monitoring/timeline');
                const timelineData = await timelineRes.json();
                updateChart(timelineData.timeline);

            } catch (error) {
                console.error('Error updating dashboard:', error);
            }
        }

        function updateStatusCards(stats) {
            document.getElementById('active-workers').textContent = stats.idle + stats.busy;
            document.getElementById('queue-size').textContent = stats.queue_size || 0;
            document.getElementById('processed-count').textContent = stats.total_jobs_completed || 0;
            document.getElementById('error-rate').textContent = (stats.error_rate || 0) + '%';
        }

        function updateWorkersTable(workers) {
            const tbody = document.getElementById('workers-tbody');

            if (!workers || workers.length === 0) {
                tbody.innerHTML = '<tr><td colspan="5" style="text-align: center; color: var(--text-secondary);">Nenhum worker registrado</td></tr>';
                return;
            }

            tbody.innerHTML = workers.map(w => `
                <tr>
                    <td><strong>${w.worker_id}</strong></td>
                    <td>
                        <span class="status-badge status-${w.status}">
                            ${w.status.toUpperCase()}
                        </span>
                    </td>
                    <td>${w.current_job_id || '-'}</td>
                    <td>${w.uptime || '-'}</td>
                    <td>${w.jobs_completed || 0} / ${w.jobs_failed || 0}</td>
                </tr>
            `).join('');
        }

        function updateLogs(logs) {
            const container = document.getElementById('worker-logs');

            if (!logs || logs.length === 0) {
                container.innerHTML = '<div class="log-entry" style="color: var(--text-secondary);">Nenhum log disponivel</div>';
                return;
            }

            container.innerHTML = logs.slice(0, 50).map(log => `
                <div class="log-entry">
                    <span class="log-time">[${log.timestamp ? new Date(log.timestamp).toLocaleTimeString() : '--:--'}]</span>
                    <span class="log-job">${log.job_id || ''}</span>
                    <span class="log-worker">${log.worker_id || ''}</span>
                    <span class="log-step">${log.step || ''}</span>
                    <span class="${log.success ? 'log-message' : 'log-error'}">${log.message || ''}</span>
                </div>
            `).join('');
        }

        function updateChart(timeline) {
            const canvas = document.getElementById('timeline-canvas');
            const ctx = canvas.getContext('2d');

            // Simple bar chart
            const width = canvas.parentElement.clientWidth;
            const height = 280;
            canvas.width = width;
            canvas.height = height;

            if (!timeline || timeline.length === 0) return;

            const maxValue = Math.max(...timeline.map(t => t.total), 1);
            const barWidth = (width - 60) / timeline.length;
            const chartHeight = height - 40;

            // Clear
            ctx.fillStyle = '#FFFFFF';
            ctx.fillRect(0, 0, width, height);

            // Draw bars
            timeline.forEach((data, i) => {
                const x = 40 + i * barWidth;
                const completedHeight = (data.completed / maxValue) * chartHeight;
                const failedHeight = (data.failed / maxValue) * chartHeight;

                // Completed (green)
                ctx.fillStyle = '#10B981';
                ctx.fillRect(x + 2, chartHeight - completedHeight + 10, barWidth - 4, completedHeight);

                // Failed (red, stacked)
                ctx.fillStyle = '#EF4444';
                ctx.fillRect(x + 2, chartHeight - completedHeight - failedHeight + 10, barWidth - 4, failedHeight);

                // Label (every 4th)
                if (i % 4 === 0) {
                    ctx.fillStyle = '#6B7280';
                    ctx.font = '10px Inter, sans-serif';
                    ctx.textAlign = 'center';
                    ctx.fillText(data.hour, x + barWidth / 2, height - 5);
                }
            });

            // Y-axis labels
            ctx.fillStyle = '#6B7280';
            ctx.font = '10px Inter, sans-serif';
            ctx.textAlign = 'right';
            ctx.fillText(maxValue.toString(), 35, 20);
            ctx.fillText('0', 35, chartHeight + 10);
        }

        // Initial load
        updateDashboard();

        // Auto-refresh
        setInterval(updateDashboard, REFRESH_INTERVAL);

        // Resize chart on window resize
        window.addEventListener('resize', () => {
            fetch('/api/monitoring/timeline')
                .then(r => r.json())
                .then(data => updateChart(data.timeline));
        });
    </script>
</body>
</html>
'''


# =============================================================================
# API ENDPOINTS (para integrar no dashboard principal)
# =============================================================================

def register_monitoring_endpoints(app):
    """
    Registra endpoints de monitoramento no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter
    from fastapi.responses import HTMLResponse

    router = APIRouter(prefix="/api/monitoring", tags=["monitoring"])

    @router.get("/workers")
    async def get_workers():
        """Retorna status de todos os workers"""
        workers = get_workers_status()
        return {"workers": workers}

    @router.get("/stats")
    async def get_stats():
        """Retorna estatisticas gerais"""
        stats = get_worker_stats()
        stats["queue_size"] = get_queue_size()
        stats["error_rate"] = get_error_rate()
        return {"stats": stats}

    @router.get("/logs")
    async def get_logs(limit: int = 50):
        """Retorna logs recentes"""
        logs = get_recent_logs(limit)
        return {"logs": logs}

    @router.get("/timeline")
    async def get_timeline(hours: int = 24):
        """Retorna dados do grafico de timeline"""
        timeline = get_jobs_timeline(hours)
        return timeline

    @router.get("/dashboard", response_class=HTMLResponse)
    async def monitoring_dashboard():
        """Retorna pagina HTML do dashboard de monitoramento"""
        return generate_worker_monitoring_html()

    app.include_router(router)
    print("[Dashboard] Worker Monitoring endpoints registered")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI
    from fastapi.responses import HTMLResponse

    app = FastAPI(title="Worker Monitoring")

    register_monitoring_endpoints(app)

    @app.get("/", response_class=HTMLResponse)
    async def root():
        return generate_worker_monitoring_html()

    print("[Worker Monitoring] Starting standalone server on port 9002...")
    uvicorn.run(app, host="0.0.0.0", port=9002)
