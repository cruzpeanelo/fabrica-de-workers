# -*- coding: utf-8 -*-
"""
Analytics Page - Charts with Chart.js (Issue #65 Enhancement)
==============================================================
Pagina dedicada de analytics com graficos interativos.
"""
from fastapi import FastAPI
from fastapi.responses import HTMLResponse


ANALYTICS_TEMPLATE = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Analytics - Plataforma E</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <style>
        * { font-family: 'Inter', sans-serif; }
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
        }
        .chart-container {
            position: relative;
            height: 300px;
            width: 100%;
        }
        .metric-card {
            background: linear-gradient(135deg, var(--belgo-blue) 0%, #005566 100%);
            border-radius: 12px;
            padding: 20px;
            color: white;
            transition: transform 0.2s;
        }
        .metric-card:hover {
            transform: translateY(-4px);
        }
        .chart-card {
            background: white;
            border-radius: 12px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            padding: 24px;
        }
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <div id="app">
        <!-- Header -->
        <header class="bg-[#003B4A] text-white py-4 px-6 shadow-lg">
            <div class="max-w-7xl mx-auto flex justify-between items-center">
                <div>
                    <h1 class="text-2xl font-bold">Analytics Dashboard</h1>
                    <p class="text-blue-200 text-sm">Metricas e Graficos de Produtividade</p>
                </div>
                <div class="flex items-center gap-4">
                    <select v-model="selectedProjectId" @change="loadAllData"
                            class="bg-white/10 text-white border border-white/20 rounded-lg px-4 py-2">
                        <option value="" class="text-gray-800">Todos os Projetos</option>
                        <option v-for="p in projects" :key="p.project_id" :value="p.project_id" class="text-gray-800">
                            {{ p.name }}
                        </option>
                    </select>
                    <select v-model="days" @change="loadAllData"
                            class="bg-white/10 text-white border border-white/20 rounded-lg px-4 py-2">
                        <option value="7" class="text-gray-800">7 dias</option>
                        <option value="30" class="text-gray-800">30 dias</option>
                        <option value="90" class="text-gray-800">90 dias</option>
                    </select>
                    <a href="/" class="bg-[#FF6C00] hover:bg-orange-600 px-4 py-2 rounded-lg font-medium transition">
                        Voltar ao Dashboard
                    </a>
                </div>
            </div>
        </header>

        <main class="max-w-7xl mx-auto py-8 px-6">
            <!-- Loading -->
            <div v-if="loading" class="flex items-center justify-center py-20">
                <div class="animate-spin rounded-full h-12 w-12 border-b-2 border-[#003B4A]"></div>
                <span class="ml-4 text-gray-600">Carregando dados...</span>
            </div>

            <div v-else>
                <!-- Metrics Cards -->
                <div class="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4 mb-8">
                    <div class="metric-card">
                        <div class="text-3xl font-bold">{{ metrics.total_stories || 0 }}</div>
                        <div class="text-sm opacity-80">Total Stories</div>
                    </div>
                    <div class="metric-card" style="background: linear-gradient(135deg, #10B981 0%, #059669 100%);">
                        <div class="text-3xl font-bold">{{ metrics.stories_completed || 0 }}</div>
                        <div class="text-sm opacity-80">Concluidas</div>
                    </div>
                    <div class="metric-card" style="background: linear-gradient(135deg, #8B5CF6 0%, #7C3AED 100%);">
                        <div class="text-3xl font-bold">{{ metrics.points_delivered || 0 }}</div>
                        <div class="text-sm opacity-80">Pontos Entregues</div>
                    </div>
                    <div class="metric-card" style="background: linear-gradient(135deg, #F59E0B 0%, #D97706 100%);">
                        <div class="text-3xl font-bold">{{ (metrics.avg_velocity || 0).toFixed(1) }}</div>
                        <div class="text-sm opacity-80">Velocity Media</div>
                    </div>
                    <div class="metric-card" style="background: linear-gradient(135deg, #06B6D4 0%, #0891B2 100%);">
                        <div class="text-3xl font-bold">{{ (metrics.avg_cycle_time_days || 0).toFixed(1) }}d</div>
                        <div class="text-sm opacity-80">Cycle Time</div>
                    </div>
                    <div class="metric-card" style="background: linear-gradient(135deg, #EC4899 0%, #DB2777 100%);">
                        <div class="text-3xl font-bold">{{ (metrics.predictability_score || 0).toFixed(0) }}%</div>
                        <div class="text-sm opacity-80">Predictability</div>
                    </div>
                </div>

                <!-- Charts Grid -->
                <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
                    <!-- Velocity por Sprint -->
                    <div class="chart-card">
                        <h3 class="text-lg font-semibold text-gray-800 mb-4">Velocity por Sprint</h3>
                        <div class="chart-container">
                            <canvas id="velocityChart"></canvas>
                        </div>
                    </div>

                    <!-- Distribuicao por Status -->
                    <div class="chart-card">
                        <h3 class="text-lg font-semibold text-gray-800 mb-4">Distribuicao por Status</h3>
                        <div class="chart-container">
                            <canvas id="statusChart"></canvas>
                        </div>
                    </div>

                    <!-- Burndown Chart -->
                    <div class="chart-card">
                        <h3 class="text-lg font-semibold text-gray-800 mb-4">Burndown Chart</h3>
                        <div class="chart-container">
                            <canvas id="burndownChart"></canvas>
                        </div>
                    </div>

                    <!-- Throughput Semanal -->
                    <div class="chart-card">
                        <h3 class="text-lg font-semibold text-gray-800 mb-4">Throughput Semanal</h3>
                        <div class="chart-container">
                            <canvas id="throughputChart"></canvas>
                        </div>
                    </div>
                </div>

                <!-- Top Contributors -->
                <div class="chart-card mb-8" v-if="topContributors.length">
                    <h3 class="text-lg font-semibold text-gray-800 mb-4">Top Contribuidores</h3>
                    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-5 gap-4">
                        <div v-for="(dev, i) in topContributors" :key="dev.assignee"
                             class="flex items-center gap-4 p-4 bg-gray-50 rounded-lg">
                            <div class="w-10 h-10 rounded-full flex items-center justify-center text-white font-bold"
                                 :style="{backgroundColor: ['#3B82F6', '#10B981', '#F59E0B', '#8B5CF6', '#EC4899'][i]}">
                                {{ i + 1 }}
                            </div>
                            <div class="flex-1">
                                <div class="font-medium text-gray-800">{{ dev.assignee }}</div>
                                <div class="text-sm text-gray-500">{{ dev.points_delivered }} pts</div>
                            </div>
                            <div class="text-lg font-bold text-green-600">{{ dev.completion_rate }}%</div>
                        </div>
                    </div>
                </div>

                <!-- Alerts -->
                <div v-if="alerts.length" class="chart-card">
                    <h3 class="text-lg font-semibold text-gray-800 mb-4">Alertas e Insights</h3>
                    <div class="space-y-3">
                        <div v-for="(alert, i) in alerts" :key="i"
                             :class="['p-4 rounded-lg border-l-4',
                                      alert.type === 'danger' ? 'bg-red-50 border-red-500' :
                                      alert.type === 'warning' ? 'bg-yellow-50 border-yellow-500' :
                                      'bg-blue-50 border-blue-500']">
                            <h4 class="font-semibold" :class="alert.type === 'danger' ? 'text-red-800' : alert.type === 'warning' ? 'text-yellow-800' : 'text-blue-800'">
                                {{ alert.title }}
                            </h4>
                            <p class="text-sm" :class="alert.type === 'danger' ? 'text-red-600' : alert.type === 'warning' ? 'text-yellow-600' : 'text-blue-600'">
                                {{ alert.message }}
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </main>
    </div>

    <script>
    const { createApp, ref, onMounted, watch } = Vue;

    createApp({
        setup() {
            const loading = ref(true);
            const projects = ref([]);
            const selectedProjectId = ref('');
            const days = ref(30);

            const metrics = ref({});
            const velocityHistory = ref([]);
            const statusDistribution = ref({});
            const topContributors = ref([]);
            const alerts = ref([]);

            let velocityChart = null;
            let statusChart = null;
            let burndownChart = null;
            let throughputChart = null;

            const loadProjects = async () => {
                try {
                    const res = await fetch('/api/projects');
                    if (res.ok) projects.value = await res.json();
                } catch (e) { console.error(e); }
            };

            const loadAllData = async () => {
                loading.value = true;
                try {
                    const projectParam = selectedProjectId.value ? `project_id=${selectedProjectId.value}&` : '';

                    const [prodRes, velocityRes] = await Promise.all([
                        fetch(`/api/analytics/productivity?${projectParam}days=${days.value}`),
                        fetch(`/api/analytics/velocity-history?${projectParam}limit=10`)
                    ]);

                    if (prodRes.ok) {
                        const data = await prodRes.json();
                        metrics.value = data.team_metrics || {};
                        statusDistribution.value = data.team_metrics?.status_distribution || {};
                        topContributors.value = data.top_contributors || [];
                        alerts.value = data.alerts || [];
                    }

                    if (velocityRes.ok) {
                        const data = await velocityRes.json();
                        velocityHistory.value = data.history || [];
                    }

                    await new Promise(r => setTimeout(r, 100));
                    renderCharts();
                } catch (e) { console.error(e); }
                finally { loading.value = false; }
            };

            const renderCharts = () => {
                renderVelocityChart();
                renderStatusChart();
                renderBurndownChart();
                renderThroughputChart();
            };

            const renderVelocityChart = () => {
                const ctx = document.getElementById('velocityChart');
                if (!ctx) return;

                if (velocityChart) velocityChart.destroy();

                const labels = velocityHistory.value.map(v => v.sprint_name || 'Sprint');
                const data = velocityHistory.value.map(v => v.velocity || 0);
                const capacity = velocityHistory.value.map(v => v.capacity || 0);

                velocityChart = new Chart(ctx, {
                    type: 'bar',
                    data: {
                        labels: labels,
                        datasets: [
                            {
                                label: 'Velocity (Story Points)',
                                data: data,
                                backgroundColor: '#003B4A',
                                borderRadius: 6
                            },
                            {
                                label: 'Capacidade',
                                data: capacity,
                                backgroundColor: '#FF6C00',
                                borderRadius: 6
                            }
                        ]
                    },
                    options: {
                        responsive: true,
                        maintainAspectRatio: false,
                        plugins: {
                            legend: { position: 'bottom' }
                        },
                        scales: {
                            y: { beginAtZero: true }
                        }
                    }
                });
            };

            const renderStatusChart = () => {
                const ctx = document.getElementById('statusChart');
                if (!ctx) return;

                if (statusChart) statusChart.destroy();

                const statusNames = {
                    'backlog': 'Backlog',
                    'ready': 'Pronto',
                    'in_progress': 'Em Progresso',
                    'review': 'Em Revisao',
                    'testing': 'Em Teste',
                    'done': 'Concluido'
                };

                const statusColors = {
                    'backlog': '#9CA3AF',
                    'ready': '#3B82F6',
                    'in_progress': '#F59E0B',
                    'review': '#8B5CF6',
                    'testing': '#06B6D4',
                    'done': '#10B981'
                };

                const labels = Object.keys(statusDistribution.value).map(k => statusNames[k] || k);
                const data = Object.values(statusDistribution.value);
                const colors = Object.keys(statusDistribution.value).map(k => statusColors[k] || '#6B7280');

                statusChart = new Chart(ctx, {
                    type: 'doughnut',
                    data: {
                        labels: labels,
                        datasets: [{
                            data: data,
                            backgroundColor: colors,
                            borderWidth: 2,
                            borderColor: '#ffffff'
                        }]
                    },
                    options: {
                        responsive: true,
                        maintainAspectRatio: false,
                        plugins: {
                            legend: { position: 'right' }
                        }
                    }
                });
            };

            const renderBurndownChart = () => {
                const ctx = document.getElementById('burndownChart');
                if (!ctx) return;

                if (burndownChart) burndownChart.destroy();

                // Generate burndown data based on metrics
                const totalPoints = metrics.value.total_points || 0;
                const idealBurndown = [];
                const actualBurndown = [];
                const labels = [];

                for (let i = 0; i <= 14; i++) {
                    labels.push('Dia ' + i);
                    idealBurndown.push(Math.max(0, totalPoints - (totalPoints / 14) * i));
                    // Simulate actual progress with some variance
                    const delivered = metrics.value.points_delivered || 0;
                    const progress = Math.min(1, (i / 14) * 1.2);
                    actualBurndown.push(Math.max(0, totalPoints - delivered * progress));
                }

                burndownChart = new Chart(ctx, {
                    type: 'line',
                    data: {
                        labels: labels,
                        datasets: [
                            {
                                label: 'Ideal',
                                data: idealBurndown,
                                borderColor: '#9CA3AF',
                                borderDash: [5, 5],
                                fill: false,
                                tension: 0
                            },
                            {
                                label: 'Real',
                                data: actualBurndown,
                                borderColor: '#003B4A',
                                backgroundColor: 'rgba(0, 59, 74, 0.1)',
                                fill: true,
                                tension: 0.3
                            }
                        ]
                    },
                    options: {
                        responsive: true,
                        maintainAspectRatio: false,
                        plugins: {
                            legend: { position: 'bottom' }
                        },
                        scales: {
                            y: {
                                beginAtZero: true,
                                title: { display: true, text: 'Story Points' }
                            }
                        }
                    }
                });
            };

            const renderThroughputChart = () => {
                const ctx = document.getElementById('throughputChart');
                if (!ctx) return;

                if (throughputChart) throughputChart.destroy();

                // Generate throughput data from velocity history
                const labels = velocityHistory.value.map(v => v.sprint_name || 'Sprint');
                const stories = velocityHistory.value.map(v => v.stories_completed || 0);

                throughputChart = new Chart(ctx, {
                    type: 'line',
                    data: {
                        labels: labels,
                        datasets: [{
                            label: 'Stories Concluidas',
                            data: stories,
                            borderColor: '#10B981',
                            backgroundColor: 'rgba(16, 185, 129, 0.1)',
                            fill: true,
                            tension: 0.3,
                            pointRadius: 6,
                            pointBackgroundColor: '#10B981'
                        }]
                    },
                    options: {
                        responsive: true,
                        maintainAspectRatio: false,
                        plugins: {
                            legend: { position: 'bottom' }
                        },
                        scales: {
                            y: {
                                beginAtZero: true,
                                title: { display: true, text: 'Quantidade' }
                            }
                        }
                    }
                });
            };

            onMounted(async () => {
                await loadProjects();
                await loadAllData();
            });

            return {
                loading, projects, selectedProjectId, days,
                metrics, velocityHistory, statusDistribution,
                topContributors, alerts, loadAllData
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
"""


def register_analytics_page(app: FastAPI):
    """Registra a pagina de analytics no app FastAPI."""

    @app.get("/analytics", response_class=HTMLResponse)
    def analytics_page():
        """Pagina de Analytics com graficos Chart.js"""
        return ANALYTICS_TEMPLATE

    print("[Analytics Page] Pagina /analytics registrada com sucesso")
