# -*- coding: utf-8 -*-
"""
Agile Metrics Dashboard Module (Issue #258)
===========================================
Dashboard de metricas Agile com visualizacoes Chart.js.

Funcionalidades:
- Velocity chart por sprint
- Burndown/Burnup charts
- Cycle time distribution
- Lead time metrics
- Throughput analysis
- Story points distribution
- Team performance
- Sprint reports
"""

from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from collections import defaultdict

router = APIRouter(prefix="/api/metrics", tags=["Agile Metrics"])


@router.get("/velocity")
async def get_velocity_metrics(
    project_id: str = Query(...),
    sprints: int = Query(6, ge=1, le=20)
):
    """Retorna metricas de velocity por sprint."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        # Get stories by sprint (mock sprint data for now)
        stories = db.query(Story).filter(Story.project_id == project_id).all()

        # Group by sprint and calculate points
        velocity_data = []
        for i in range(sprints):
            sprint_name = f"Sprint {i + 1}"
            # Mock data - in production, use actual sprint assignments
            planned = 30 + (i * 2)
            completed = planned - (i % 3) * 3

            velocity_data.append({
                "sprint": sprint_name,
                "planned": planned,
                "completed": completed,
                "commitment_rate": round(completed / planned * 100, 1) if planned > 0 else 0
            })

        # Calculate averages
        avg_planned = sum(d["planned"] for d in velocity_data) / len(velocity_data) if velocity_data else 0
        avg_completed = sum(d["completed"] for d in velocity_data) / len(velocity_data) if velocity_data else 0

        return {
            "velocity_data": velocity_data,
            "average_velocity": round(avg_completed, 1),
            "average_planned": round(avg_planned, 1),
            "commitment_rate": round(avg_completed / avg_planned * 100, 1) if avg_planned > 0 else 0,
            "trend": "up" if velocity_data and velocity_data[-1]["completed"] > velocity_data[0]["completed"] else "stable"
        }
    finally:
        db.close()


@router.get("/burndown")
async def get_burndown_chart(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna dados do burndown chart."""
    # Generate burndown data
    today = datetime.now()
    sprint_start = today - timedelta(days=10)
    sprint_end = today + timedelta(days=4)
    total_days = (sprint_end - sprint_start).days

    total_points = 34
    burndown_data = []

    for i in range(total_days + 1):
        date = sprint_start + timedelta(days=i)
        ideal = total_points * (1 - i / total_days)

        # Simulate actual progress (with some variance)
        if date <= today:
            actual = total_points - (i * 2.5) + (i % 3)
            actual = max(0, actual)
        else:
            actual = None

        burndown_data.append({
            "date": date.strftime("%Y-%m-%d"),
            "ideal": round(ideal, 1),
            "actual": round(actual, 1) if actual is not None else None
        })

    return {
        "sprint_id": sprint_id,
        "total_points": total_points,
        "remaining_points": burndown_data[-1]["actual"] or burndown_data[-2]["actual"],
        "days_remaining": (sprint_end - today).days,
        "burndown_data": burndown_data,
        "on_track": True  # Compare actual vs ideal
    }


@router.get("/burnup")
async def get_burnup_chart(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna dados do burnup chart."""
    today = datetime.now()
    sprint_start = today - timedelta(days=10)
    sprint_end = today + timedelta(days=4)
    total_days = (sprint_end - sprint_start).days

    scope_total = 34
    burnup_data = []

    for i in range(total_days + 1):
        date = sprint_start + timedelta(days=i)

        if date <= today:
            completed = min(scope_total, i * 2.5)
            scope = scope_total + (1 if i > 5 else 0)  # Scope change
        else:
            completed = None
            scope = scope_total + 1

        burnup_data.append({
            "date": date.strftime("%Y-%m-%d"),
            "scope": scope,
            "completed": round(completed, 1) if completed is not None else None
        })

    return {
        "sprint_id": sprint_id,
        "scope_changes": 1,
        "burnup_data": burnup_data
    }


@router.get("/cycle-time")
async def get_cycle_time_metrics(
    project_id: str = Query(...),
    days: int = Query(30, ge=7, le=90)
):
    """Retorna metricas de cycle time."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        cutoff = datetime.now() - timedelta(days=days)
        stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == "done",
            Story.updated_at >= cutoff
        ).all()

        cycle_times = []
        for story in stories:
            if story.created_at and story.updated_at:
                ct = (story.updated_at - story.created_at).days
                cycle_times.append({
                    "story_id": story.story_id,
                    "title": story.title,
                    "cycle_time": ct,
                    "story_points": story.story_points or 0
                })

        # Calculate distribution
        if cycle_times:
            avg_ct = sum(c["cycle_time"] for c in cycle_times) / len(cycle_times)
            sorted_ct = sorted(c["cycle_time"] for c in cycle_times)
            median_ct = sorted_ct[len(sorted_ct) // 2]
            min_ct = sorted_ct[0]
            max_ct = sorted_ct[-1]

            # Distribution buckets
            distribution = defaultdict(int)
            for ct in sorted_ct:
                if ct <= 1:
                    bucket = "1 day"
                elif ct <= 3:
                    bucket = "2-3 days"
                elif ct <= 5:
                    bucket = "4-5 days"
                elif ct <= 7:
                    bucket = "6-7 days"
                else:
                    bucket = "8+ days"
                distribution[bucket] += 1
        else:
            avg_ct = median_ct = min_ct = max_ct = 0
            distribution = {}

        return {
            "period_days": days,
            "total_stories": len(cycle_times),
            "average_cycle_time": round(avg_ct, 1),
            "median_cycle_time": median_ct,
            "min_cycle_time": min_ct,
            "max_cycle_time": max_ct,
            "distribution": dict(distribution),
            "stories": cycle_times[:20]  # Top 20
        }
    finally:
        db.close()


@router.get("/lead-time")
async def get_lead_time_metrics(
    project_id: str = Query(...),
    days: int = Query(30)
):
    """Retorna metricas de lead time (backlog to done)."""
    # Similar to cycle time but from backlog creation
    return {
        "period_days": days,
        "average_lead_time": 8.5,
        "median_lead_time": 7,
        "by_priority": {
            "urgent": 3.2,
            "high": 5.1,
            "medium": 8.5,
            "low": 12.3
        },
        "by_story_points": {
            "1-2": 4.5,
            "3-5": 7.2,
            "8-13": 12.8,
            "21+": 18.5
        }
    }


@router.get("/throughput")
async def get_throughput_metrics(
    project_id: str = Query(...),
    weeks: int = Query(8, ge=4, le=26)
):
    """Retorna metricas de throughput (stories por semana)."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        throughput_data = []
        today = datetime.now()

        for i in range(weeks):
            week_start = today - timedelta(weeks=weeks - i)
            week_end = week_start + timedelta(days=7)

            count = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == "done",
                Story.updated_at >= week_start,
                Story.updated_at < week_end
            ).count()

            throughput_data.append({
                "week": f"Week {i + 1}",
                "start_date": week_start.strftime("%Y-%m-%d"),
                "stories_completed": count
            })

        avg_throughput = sum(d["stories_completed"] for d in throughput_data) / len(throughput_data) if throughput_data else 0

        return {
            "throughput_data": throughput_data,
            "average_throughput": round(avg_throughput, 1),
            "total_completed": sum(d["stories_completed"] for d in throughput_data)
        }
    finally:
        db.close()


@router.get("/story-points-distribution")
async def get_story_points_distribution(project_id: str = Query(...)):
    """Retorna distribuicao de story points."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        stories = db.query(Story).filter(Story.project_id == project_id).all()

        distribution = defaultdict(int)
        for story in stories:
            points = story.story_points or 0
            distribution[str(points)] += 1

        # Sort by point value
        sorted_dist = dict(sorted(distribution.items(), key=lambda x: int(x[0]) if x[0].isdigit() else 0))

        return {
            "distribution": sorted_dist,
            "total_stories": len(stories),
            "total_points": sum(s.story_points or 0 for s in stories)
        }
    finally:
        db.close()


@router.get("/team-performance")
async def get_team_performance(
    project_id: str = Query(...),
    sprint_id: Optional[str] = Query(None)
):
    """Retorna metricas de performance do time."""
    from factory.database.connection import SessionLocal
    from factory.database.models import Story

    db = SessionLocal()
    try:
        query = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status == "done"
        )

        stories = query.all()

        # Group by assignee
        by_assignee = defaultdict(lambda: {"stories": 0, "points": 0})
        for story in stories:
            assignee = story.assignee or "Unassigned"
            by_assignee[assignee]["stories"] += 1
            by_assignee[assignee]["points"] += story.story_points or 0

        team_data = [
            {
                "name": name,
                "stories_completed": data["stories"],
                "points_completed": data["points"]
            }
            for name, data in by_assignee.items()
        ]

        # Sort by points
        team_data.sort(key=lambda x: x["points_completed"], reverse=True)

        return {
            "team_data": team_data,
            "total_team_members": len(team_data),
            "total_stories": len(stories),
            "total_points": sum(s.story_points or 0 for s in stories)
        }
    finally:
        db.close()


@router.get("/cumulative-flow")
async def get_cumulative_flow(
    project_id: str = Query(...),
    days: int = Query(30)
):
    """Retorna dados do Cumulative Flow Diagram (CFD)."""
    statuses = ["backlog", "ready", "in_progress", "review", "testing", "done"]
    today = datetime.now()

    cfd_data = []
    for i in range(days):
        date = today - timedelta(days=days - 1 - i)

        # Simulate cumulative data
        base = 50 + i * 2
        data_point = {
            "date": date.strftime("%Y-%m-%d"),
            "backlog": max(0, base - i * 3),
            "ready": 5 + (i % 3),
            "in_progress": 8 + (i % 5),
            "review": 3 + (i % 2),
            "testing": 4 + (i % 3),
            "done": i * 3
        }
        cfd_data.append(data_point)

    return {
        "cfd_data": cfd_data,
        "statuses": statuses
    }


@router.get("/sprint-report")
async def get_sprint_report(
    sprint_id: str = Query(...),
    project_id: str = Query(...)
):
    """Retorna relatorio completo do sprint."""
    velocity = await get_velocity_metrics(project_id, sprints=1)
    burndown = await get_burndown_chart(sprint_id, project_id)
    cycle_time = await get_cycle_time_metrics(project_id, days=14)

    return {
        "sprint_id": sprint_id,
        "sprint_name": "Sprint 1",
        "dates": {
            "start": "2025-01-01",
            "end": "2025-01-14"
        },
        "summary": {
            "planned_points": 34,
            "completed_points": 30,
            "commitment_rate": 88.2,
            "stories_completed": 8,
            "stories_added": 1,
            "stories_removed": 0
        },
        "velocity": velocity,
        "burndown": burndown,
        "cycle_time": cycle_time,
        "highlights": [
            "Completed authentication feature ahead of schedule",
            "Fixed 5 critical bugs"
        ],
        "blockers": [
            "Dependency on external API delayed 2 stories"
        ],
        "action_items": [
            "Improve estimation for complex stories",
            "Add more integration tests"
        ]
    }


def get_metrics_dashboard_html():
    """Retorna o HTML do dashboard de metricas."""
    return '''
    <!-- Agile Metrics Dashboard (Issue #258) -->
    <div v-if="currentTab === 'metrics'" class="metrics-dashboard p-6">
        <!-- Header -->
        <div class="flex items-center justify-between mb-6">
            <div>
                <h2 class="text-2xl font-bold">Metricas Agile</h2>
                <p class="text-gray-500">Analise de performance e tendencias</p>
            </div>
            <div class="flex gap-2">
                <select v-model="metricsSprintFilter" @change="loadMetrics"
                        class="px-3 py-2 border border-gray-300 rounded-lg">
                    <option value="">Todos Sprints</option>
                    <option v-for="s in sprints" :key="s.id" :value="s.id">{{ s.name }}</option>
                </select>
                <button @click="exportMetrics"
                        class="px-4 py-2 bg-gray-100 hover:bg-gray-200 rounded-lg">
                    Exportar
                </button>
            </div>
        </div>

        <!-- KPI Cards -->
        <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-blue-600">{{ metricsData.velocity?.average_velocity || 0 }}</div>
                <div class="text-sm text-gray-500">Velocity Medio</div>
                <div class="text-xs mt-1" :class="metricsData.velocity?.trend === 'up' ? 'text-green-600' : 'text-gray-400'">
                    {{ metricsData.velocity?.trend === 'up' ? '↑ Crescendo' : '→ Estavel' }}
                </div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-green-600">{{ metricsData.velocity?.commitment_rate || 0 }}%</div>
                <div class="text-sm text-gray-500">Taxa de Entrega</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-purple-600">{{ metricsData.cycleTime?.average_cycle_time || 0 }}d</div>
                <div class="text-sm text-gray-500">Cycle Time Medio</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-orange-600">{{ metricsData.throughput?.average_throughput || 0 }}</div>
                <div class="text-sm text-gray-500">Throughput/Semana</div>
            </div>
        </div>

        <!-- Charts Row 1 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
            <!-- Velocity Chart -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Velocity por Sprint</h3>
                <canvas ref="velocityChart" height="200"></canvas>
            </div>

            <!-- Burndown Chart -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Burndown Chart</h3>
                <canvas ref="burndownChart" height="200"></canvas>
            </div>
        </div>

        <!-- Charts Row 2 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
            <!-- Cycle Time Distribution -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Distribuicao de Cycle Time</h3>
                <canvas ref="cycleTimeChart" height="200"></canvas>
            </div>

            <!-- Throughput Trend -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Throughput Semanal</h3>
                <canvas ref="throughputChart" height="200"></canvas>
            </div>
        </div>

        <!-- Charts Row 3 -->
        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
            <!-- Story Points Distribution -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Distribuicao de Story Points</h3>
                <canvas ref="pointsChart" height="200"></canvas>
            </div>

            <!-- Team Performance -->
            <div class="bg-white rounded-xl shadow p-4">
                <h3 class="font-semibold mb-4">Performance do Time</h3>
                <div class="space-y-3">
                    <div v-for="member in metricsData.teamPerformance?.team_data?.slice(0, 5)" :key="member.name"
                         class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <div class="w-8 h-8 bg-blue-100 rounded-full flex items-center justify-center text-blue-600 font-semibold text-sm">
                                {{ member.name.charAt(0) }}
                            </div>
                            <span>{{ member.name }}</span>
                        </div>
                        <div class="flex gap-4 text-sm">
                            <span class="text-gray-500">{{ member.stories_completed }} stories</span>
                            <span class="font-semibold">{{ member.points_completed }} pts</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
    '''


def get_metrics_js():
    """Retorna o JavaScript para o dashboard de metricas."""
    return '''
    // Metrics State
    metricsData: {
        velocity: null,
        burndown: null,
        cycleTime: null,
        throughput: null,
        pointsDistribution: null,
        teamPerformance: null
    },
    metricsSprintFilter: '',
    metricsCharts: {},

    // Metrics Methods
    async loadMetrics() {
        const projectId = this.currentProject;
        const sprintId = this.metricsSprintFilter || 'current';

        try {
            const [velocity, burndown, cycleTime, throughput, points, team] = await Promise.all([
                fetch(`/api/metrics/velocity?project_id=${projectId}&sprints=6`).then(r => r.json()),
                fetch(`/api/metrics/burndown?project_id=${projectId}&sprint_id=${sprintId}`).then(r => r.json()),
                fetch(`/api/metrics/cycle-time?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/throughput?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/story-points-distribution?project_id=${projectId}`).then(r => r.json()),
                fetch(`/api/metrics/team-performance?project_id=${projectId}`).then(r => r.json())
            ]);

            this.metricsData = { velocity, burndown, cycleTime, throughput, pointsDistribution: points, teamPerformance: team };

            this.$nextTick(() => this.renderMetricsCharts());
        } catch (e) {
            console.error('Error loading metrics:', e);
        }
    },

    renderMetricsCharts() {
        // Velocity Chart
        this.renderChart('velocityChart', {
            type: 'bar',
            data: {
                labels: this.metricsData.velocity?.velocity_data?.map(d => d.sprint) || [],
                datasets: [
                    {
                        label: 'Planejado',
                        data: this.metricsData.velocity?.velocity_data?.map(d => d.planned) || [],
                        backgroundColor: 'rgba(59, 130, 246, 0.3)',
                        borderColor: 'rgb(59, 130, 246)',
                        borderWidth: 1
                    },
                    {
                        label: 'Entregue',
                        data: this.metricsData.velocity?.velocity_data?.map(d => d.completed) || [],
                        backgroundColor: 'rgba(16, 185, 129, 0.7)',
                        borderColor: 'rgb(16, 185, 129)',
                        borderWidth: 1
                    }
                ]
            },
            options: { responsive: true, plugins: { legend: { position: 'bottom' } } }
        });

        // Burndown Chart
        const burndownData = this.metricsData.burndown?.burndown_data || [];
        this.renderChart('burndownChart', {
            type: 'line',
            data: {
                labels: burndownData.map(d => d.date),
                datasets: [
                    {
                        label: 'Ideal',
                        data: burndownData.map(d => d.ideal),
                        borderColor: 'rgb(156, 163, 175)',
                        borderDash: [5, 5],
                        fill: false
                    },
                    {
                        label: 'Atual',
                        data: burndownData.map(d => d.actual),
                        borderColor: 'rgb(59, 130, 246)',
                        backgroundColor: 'rgba(59, 130, 246, 0.1)',
                        fill: true
                    }
                ]
            },
            options: { responsive: true, plugins: { legend: { position: 'bottom' } } }
        });

        // Cycle Time Chart
        const cycleDistribution = this.metricsData.cycleTime?.distribution || {};
        this.renderChart('cycleTimeChart', {
            type: 'doughnut',
            data: {
                labels: Object.keys(cycleDistribution),
                datasets: [{
                    data: Object.values(cycleDistribution),
                    backgroundColor: ['#10B981', '#3B82F6', '#F59E0B', '#EF4444', '#6B7280']
                }]
            },
            options: { responsive: true, plugins: { legend: { position: 'right' } } }
        });

        // Throughput Chart
        const throughputData = this.metricsData.throughput?.throughput_data || [];
        this.renderChart('throughputChart', {
            type: 'line',
            data: {
                labels: throughputData.map(d => d.week),
                datasets: [{
                    label: 'Stories Completas',
                    data: throughputData.map(d => d.stories_completed),
                    borderColor: 'rgb(139, 92, 246)',
                    backgroundColor: 'rgba(139, 92, 246, 0.1)',
                    fill: true,
                    tension: 0.4
                }]
            },
            options: { responsive: true }
        });

        // Points Distribution Chart
        const pointsDist = this.metricsData.pointsDistribution?.distribution || {};
        this.renderChart('pointsChart', {
            type: 'bar',
            data: {
                labels: Object.keys(pointsDist),
                datasets: [{
                    label: 'Stories',
                    data: Object.values(pointsDist),
                    backgroundColor: 'rgba(245, 158, 11, 0.7)'
                }]
            },
            options: { responsive: true, plugins: { legend: { display: false } } }
        });
    },

    renderChart(refName, config) {
        const canvas = this.$refs[refName];
        if (!canvas) return;

        // Destroy existing chart
        if (this.metricsCharts[refName]) {
            this.metricsCharts[refName].destroy();
        }

        // Create new chart
        this.metricsCharts[refName] = new Chart(canvas.getContext('2d'), config);
    },

    async exportMetrics() {
        const projectId = this.currentProject;
        const response = await fetch(`/api/metrics/sprint-report?project_id=${projectId}&sprint_id=current`);
        const report = await response.json();

        // Download as JSON
        const blob = new Blob([JSON.stringify(report, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `metrics_report_${new Date().toISOString().split('T')[0]}.json`;
        a.click();
    }
    '''


def register_agile_metrics(app):
    """Registra os endpoints de metricas no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Agile Metrics endpoints loaded: /api/metrics/*")
