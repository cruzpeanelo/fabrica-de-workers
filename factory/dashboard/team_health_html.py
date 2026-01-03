# -*- coding: utf-8 -*-
"""
Team Health Dashboard HTML/JS Module (Issue #228)
================================================
Generates HTML and JavaScript for the Team Health Dashboard.
"""

def get_team_health_html():
    """Returns the HTML template for Team Health Dashboard"""
    return '''
    <!-- Team Health Dashboard (Issue #228) -->
    <div v-if="currentView === 'team-health'" class="team-health-dashboard p-6" role="main" aria-label="Team Health Dashboard">
        <!-- Header -->
        <div class="flex flex-col md:flex-row md:items-center md:justify-between mb-6 gap-4">
            <div>
                <h1 class="text-3xl font-bold text-gray-900">👥 Team Health Dashboard</h1>
                <p class="text-gray-500 mt-1">Métricas de saúde, velocidade e bem-estar da equipe</p>
            </div>
            <div class="flex gap-2 flex-wrap">
                <select v-model="teamHealthSprintFilter" @change="loadTeamHealth"
                        class="px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500"
                        aria-label="Filtrar por sprint">
                    <option value="">Todos os Sprints</option>
                    <option v-for="s in sprints" :key="s.sprint_id" :value="s.sprint_id">{{ s.name }}</option>
                </select>
                <button @click="exportTeamHealth"
                        class="px-4 py-2 bg-white border border-gray-300 hover:bg-gray-50 rounded-lg flex items-center gap-2"
                        aria-label="Exportar relatório">
                    <span>📊</span>
                    <span>Exportar</span>
                </button>
            </div>
        </div>

        <!-- KPI Cards -->
        <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6" role="region" aria-label="KPIs principais">
            <!-- Velocity KPI -->
            <div class="bg-white rounded-xl shadow p-6 hover:shadow-lg transition-shadow">
                <div class="flex items-start justify-between mb-2">
                    <div class="text-4xl" aria-hidden="true">📈</div>
                    <span :class="[
                        'px-2 py-1 rounded text-xs font-semibold',
                        teamHealthKPIs.velocity?.trend === 'up' ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-600'
                    ]">
                        {{ teamHealthKPIs.velocity?.trend === 'up' ? '↑' : '→' }}
                        {{ Math.abs(teamHealthKPIs.velocity?.change_percentage || 0) }}%
                    </span>
                </div>
                <div class="text-3xl font-bold text-blue-600">{{ teamHealthKPIs.velocity?.value || 0 }}</div>
                <div class="text-sm text-gray-500 mt-1">Velocity (pts/sprint)</div>
            </div>

            <!-- Throughput KPI -->
            <div class="bg-white rounded-xl shadow p-6 hover:shadow-lg transition-shadow">
                <div class="flex items-start justify-between mb-2">
                    <div class="text-4xl" aria-hidden="true">⚡</div>
                    <span :class="[
                        'px-2 py-1 rounded text-xs font-semibold',
                        teamHealthKPIs.throughput?.trend === 'up' ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-600'
                    ]">
                        {{ teamHealthKPIs.throughput?.trend === 'up' ? '↑' : '→' }}
                        {{ Math.abs(teamHealthKPIs.throughput?.change_percentage || 0) }}%
                    </span>
                </div>
                <div class="text-3xl font-bold text-green-600">{{ teamHealthKPIs.throughput?.value || 0 }}</div>
                <div class="text-sm text-gray-500 mt-1">Throughput (stories/semana)</div>
            </div>

            <!-- Predictability KPI -->
            <div class="bg-white rounded-xl shadow p-6 hover:shadow-lg transition-shadow">
                <div class="flex items-start justify-between mb-2">
                    <div class="text-4xl" aria-hidden="true">🎯</div>
                    <span :class="[
                        'px-2 py-1 rounded text-xs font-semibold',
                        teamHealthKPIs.predictability?.value >= 90 ? 'bg-green-100 text-green-700' : 'bg-yellow-100 text-yellow-700'
                    ]">
                        {{ teamHealthKPIs.predictability?.trend === 'stable' ? '→' : '↓' }}
                        {{ Math.abs(teamHealthKPIs.predictability?.change_percentage || 0) }}%
                    </span>
                </div>
                <div class="text-3xl font-bold text-purple-600">{{ teamHealthKPIs.predictability?.value || 0 }}%</div>
                <div class="text-sm text-gray-500 mt-1">Taxa de Previsibilidade</div>
            </div>

            <!-- Health Score KPI -->
            <div class="bg-white rounded-xl shadow p-6 hover:shadow-lg transition-shadow">
                <div class="flex items-start justify-between mb-2">
                    <div class="text-4xl" aria-hidden="true">😊</div>
                    <span :class="[
                        'px-2 py-1 rounded text-xs font-semibold',
                        teamHealthKPIs.health_score?.grade?.includes('A') ? 'bg-green-100 text-green-700' : 'bg-blue-100 text-blue-700'
                    ]">
                        {{ teamHealthKPIs.health_score?.grade || 'A-' }}
                    </span>
                </div>
                <div class="text-3xl font-bold text-orange-600">{{ teamHealthKPIs.health_score?.value?.toFixed(1) || '4.2' }}</div>
                <div class="text-sm text-gray-500 mt-1">Health Score (/5.0)</div>
            </div>
        </div>

        <!-- Charts Row 1: Velocity & Workload -->
        <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
            <!-- Velocity Trend Chart -->
            <div class="bg-white rounded-xl shadow p-6">
                <h3 class="font-semibold text-lg mb-4 flex items-center gap-2">
                    <span aria-hidden="true">📊</span>
                    <span>Velocity por Sprint</span>
                </h3>
                <div class="relative" style="height: 300px;">
                    <canvas ref="velocityTrendChart" role="img" aria-label="Gráfico de velocity por sprint"></canvas>
                </div>
            </div>

            <!-- Workload Distribution -->
            <div class="bg-white rounded-xl shadow p-6">
                <h3 class="font-semibold text-lg mb-4 flex items-center gap-2">
                    <span aria-hidden="true">⚖️</span>
                    <span>Distribuição de Carga</span>
                </h3>
                <div class="space-y-3">
                    <div v-for="member in workloadData.members" :key="member.name"
                         class="flex items-center gap-3">
                        <div class="flex-shrink-0 w-24 text-sm font-medium text-gray-700">{{ member.name }}</div>
                        <div class="flex-1">
                            <div class="relative h-8 bg-gray-100 rounded-lg overflow-hidden">
                                <div :class="[
                                    'absolute top-0 left-0 h-full flex items-center justify-center text-xs font-semibold text-white transition-all',
                                    member.status === 'overloaded' ? 'bg-red-500' :
                                    member.status === 'underutilized' ? 'bg-yellow-500' :
                                    'bg-green-500'
                                ]"
                                :style="{width: member.workload_percentage + '%'}">
                                    {{ member.workload_percentage }}%
                                </div>
                            </div>
                        </div>
                        <div class="flex-shrink-0 w-12 text-right text-sm">
                            <span v-if="member.status === 'overloaded'" class="text-red-600" role="img" aria-label="Alerta de sobrecarga">⚠️</span>
                            <span v-else-if="member.status === 'ok'" class="text-green-600" role="img" aria-label="Carga adequada">✓</span>
                            <span v-else class="text-yellow-600" role="img" aria-label="Subcarga">⚠️</span>
                        </div>
                    </div>
                </div>
                <!-- Workload Warnings -->
                <div v-if="workloadData.warnings && workloadData.warnings.length > 0" class="mt-4 pt-4 border-t border-gray-200">
                    <div v-for="warning in workloadData.warnings" :key="warning.message"
                         :class="[
                             'p-3 rounded-lg text-sm mb-2',
                             warning.type === 'warning' ? 'bg-yellow-50 text-yellow-800' : 'bg-blue-50 text-blue-800'
                         ]"
                         role="alert">
                        <div class="font-semibold">{{ warning.message }}</div>
                        <div class="text-xs mt-1">{{ warning.recommendation }}</div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Charts Row 2: Lead Time & Cycle Time -->
        <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
            <!-- Lead Time Breakdown -->
            <div class="bg-white rounded-xl shadow p-6">
                <h3 class="font-semibold text-lg mb-4 flex items-center gap-2">
                    <span aria-hidden="true">⏱️</span>
                    <span>Lead Time por Etapa</span>
                </h3>
                <div class="space-y-3">
                    <div class="border-l-4 border-blue-500 pl-4 py-2">
                        <div class="flex justify-between items-center">
                            <span class="text-sm text-gray-600">Backlog → Ready</span>
                            <span class="font-semibold">2.3 dias</span>
                        </div>
                    </div>
                    <div class="border-l-4 border-yellow-500 pl-4 py-2">
                        <div class="flex justify-between items-center">
                            <span class="text-sm text-gray-600">Ready → In Progress</span>
                            <span class="font-semibold">0.8 dias</span>
                        </div>
                    </div>
                    <div class="border-l-4 border-orange-500 pl-4 py-2">
                        <div class="flex justify-between items-center">
                            <span class="text-sm text-gray-600">In Progress → Review</span>
                            <span class="font-semibold">3.2 dias</span>
                        </div>
                    </div>
                    <div class="border-l-4 border-purple-500 pl-4 py-2">
                        <div class="flex justify-between items-center">
                            <span class="text-sm text-gray-600">Review → Done</span>
                            <span class="font-semibold">1.1 dias</span>
                        </div>
                    </div>
                    <div class="border-t-2 border-gray-200 pt-3 mt-3">
                        <div class="flex justify-between items-center">
                            <span class="font-semibold text-gray-900">Total Lead Time</span>
                            <span class="text-2xl font-bold text-blue-600">7.4 dias</span>
                        </div>
                        <div class="flex justify-between items-center mt-2 text-sm text-gray-500">
                            <span>Cycle Time (WIP)</span>
                            <span>4.3 dias</span>
                        </div>
                        <div class="mt-2 text-xs text-green-600 flex items-center gap-1">
                            <span>↓</span>
                            <span>Tendência: -15% vs sprint anterior</span>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Team Mood Tracker -->
            <div class="bg-white rounded-xl shadow p-6">
                <h3 class="font-semibold text-lg mb-4 flex items-center gap-2">
                    <span aria-hidden="true">😊</span>
                    <span>Como está o time esta semana?</span>
                </h3>
                <div class="space-y-3">
                    <div v-for="(pct, mood) in teamMood.mood_distribution" :key="mood"
                         class="flex items-center gap-3">
                        <div class="flex-shrink-0 w-8 text-2xl" aria-hidden="true">
                            {{ mood === 'excellent' ? '😄' : mood === 'good' ? '😊' : mood === 'neutral' ? '😐' : mood === 'concerned' ? '😟' : '😫' }}
                        </div>
                        <div class="flex-shrink-0 w-24 text-sm text-gray-600">
                            {{ mood === 'excellent' ? 'Excelente' : mood === 'good' ? 'Bom' : mood === 'neutral' ? 'Neutro' : mood === 'concerned' ? 'Preocupado' : 'Estressado' }}
                        </div>
                        <div class="flex-1">
                            <div class="relative h-6 bg-gray-100 rounded-full overflow-hidden">
                                <div :class="[
                                    'absolute top-0 left-0 h-full transition-all',
                                    mood === 'excellent' || mood === 'good' ? 'bg-green-500' :
                                    mood === 'neutral' ? 'bg-yellow-500' :
                                    'bg-red-500'
                                ]"
                                :style="{width: pct + '%'}"></div>
                            </div>
                        </div>
                        <div class="flex-shrink-0 w-12 text-right text-sm font-semibold">{{ pct }}%</div>
                    </div>
                </div>
                <div class="mt-6 p-4 bg-green-50 rounded-lg">
                    <div class="flex items-start gap-3">
                        <span class="text-2xl" aria-hidden="true">📊</span>
                        <div>
                            <div class="font-semibold text-green-800">Score Médio: {{ teamMood.average_score || 4.2 }}/5.0</div>
                            <div class="text-sm text-green-700 mt-1">
                                <span class="inline-flex items-center gap-1">
                                    <span>{{ teamMood.trend === 'improving' ? '↑' : teamMood.trend === 'declining' ? '↓' : '→' }}</span>
                                    <span>{{ teamMood.message || 'Time demonstra moral elevado' }}</span>
                                </span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Burnout Indicators -->
        <div class="bg-white rounded-xl shadow p-6 mb-6">
            <h3 class="font-semibold text-lg mb-4 flex items-center gap-2">
                <span aria-hidden="true">🔥</span>
                <span>Indicadores de Risco e Saúde</span>
            </h3>
            <div class="space-y-3">
                <div v-for="indicator in burnoutIndicators.indicators" :key="indicator.message"
                     :class="[
                         'p-4 rounded-lg flex items-start gap-3',
                         indicator.type === 'alert' ? 'bg-red-50 border-l-4 border-red-500' :
                         indicator.type === 'warning' ? 'bg-yellow-50 border-l-4 border-yellow-500' :
                         'bg-green-50 border-l-4 border-green-500'
                     ]"
                     role="alert"
                     :aria-live="indicator.type === 'alert' ? 'assertive' : 'polite'">
                    <span class="text-2xl flex-shrink-0" aria-hidden="true">{{ indicator.icon }}</span>
                    <div class="flex-1">
                        <div :class="[
                            'font-semibold',
                            indicator.type === 'alert' ? 'text-red-800' :
                            indicator.type === 'warning' ? 'text-yellow-800' :
                            'text-green-800'
                        ]">
                            {{ indicator.message }}
                        </div>
                        <div v-if="indicator.recommendation" :class="[
                            'text-sm mt-1',
                            indicator.type === 'alert' ? 'text-red-700' :
                            indicator.type === 'warning' ? 'text-yellow-700' :
                            'text-green-700'
                        ]">
                            {{ indicator.recommendation }}
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Loading State -->
        <div v-if="teamHealthLoading" class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div class="bg-white rounded-lg p-6 flex items-center gap-3">
                <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
                <span>Carregando métricas...</span>
            </div>
        </div>
    </div>
    '''


def get_team_health_js():
    """Returns JavaScript for Team Health Dashboard"""
    return '''
    // Team Health State
    teamHealthKPIs: {
        velocity: null,
        throughput: null,
        predictability: null,
        health_score: null
    },
    workloadData: {
        members: [],
        warnings: []
    },
    burnoutIndicators: {
        indicators: []
    },
    teamMood: {
        mood_distribution: {},
        average_score: 4.2,
        trend: 'stable',
        message: ''
    },
    teamHealthSprintFilter: '',
    teamHealthLoading: false,
    teamHealthCharts: {},

    // Team Health Methods
    async loadTeamHealth() {
        const projectId = this.currentProject;
        const sprintId = this.teamHealthSprintFilter || null;

        this.teamHealthLoading = true;

        try {
            // Load all team health data in parallel
            const [kpis, workload, burnout, mood] = await Promise.all([
                fetch(`/api/team-health/kpis-summary?project_id=${projectId}${sprintId ? '&sprint_id=' + sprintId : ''}`).then(r => r.json()),
                fetch(`/api/team-health/workload-distribution?project_id=${projectId}${sprintId ? '&sprint_id=' + sprintId : ''}`).then(r => r.json()),
                fetch(`/api/team-health/burnout-indicators?project_id=${projectId}${sprintId ? '&sprint_id=' + sprintId : ''}`).then(r => r.json()),
                fetch(`/api/team-health/team-mood?project_id=${projectId}&days=7`).then(r => r.json())
            ]);

            this.teamHealthKPIs = kpis;
            this.workloadData = workload;
            this.burnoutIndicators = burnout;
            this.teamMood = mood;

            this.$nextTick(() => this.renderTeamHealthCharts());
        } catch (e) {
            console.error('Error loading team health:', e);
            this.showNotification('Erro ao carregar team health', 'error');
        } finally {
            this.teamHealthLoading = false;
        }
    },

    renderTeamHealthCharts() {
        // Velocity Trend Chart
        if (this.$refs.velocityTrendChart) {
            const projectId = this.currentProject;

            // Fetch velocity data
            fetch(`/api/metrics/velocity?project_id=${projectId}&sprints=6`)
                .then(r => r.json())
                .then(data => {
                    const velocityData = data.velocity_data || [];

                    this.renderChart('velocityTrendChart', {
                        type: 'line',
                        data: {
                            labels: velocityData.map(d => d.sprint),
                            datasets: [
                                {
                                    label: 'Velocity Realizada',
                                    data: velocityData.map(d => d.completed),
                                    borderColor: 'rgb(59, 130, 246)',
                                    backgroundColor: 'rgba(59, 130, 246, 0.1)',
                                    fill: true,
                                    tension: 0.4
                                },
                                {
                                    label: 'Média',
                                    data: velocityData.map(() => data.average_velocity),
                                    borderColor: 'rgb(156, 163, 175)',
                                    borderDash: [5, 5],
                                    fill: false
                                }
                            ]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: {
                                legend: { position: 'bottom' },
                                tooltip: {
                                    callbacks: {
                                        label: function(context) {
                                            return context.dataset.label + ': ' + context.parsed.y + ' pts';
                                        }
                                    }
                                }
                            },
                            scales: {
                                y: {
                                    beginAtZero: true,
                                    title: {
                                        display: true,
                                        text: 'Story Points'
                                    }
                                }
                            }
                        }
                    });
                })
                .catch(e => console.error('Error loading velocity chart:', e));
        }
    },

    renderChart(refName, config) {
        const canvas = this.$refs[refName];
        if (!canvas) return;

        // Destroy existing chart
        if (this.teamHealthCharts[refName]) {
            this.teamHealthCharts[refName].destroy();
        }

        // Create new chart
        this.teamHealthCharts[refName] = new Chart(canvas.getContext('2d'), config);
    },

    async exportTeamHealth() {
        const projectId = this.currentProject;
        const sprintId = this.teamHealthSprintFilter || 'current';

        try {
            // Gather all data
            const report = {
                project_id: projectId,
                sprint_id: sprintId,
                generated_at: new Date().toISOString(),
                kpis: this.teamHealthKPIs,
                workload: this.workloadData,
                burnout_indicators: this.burnoutIndicators,
                team_mood: this.teamMood
            };

            // Download as JSON
            const blob = new Blob([JSON.stringify(report, null, 2)], { type: 'application/json' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `team_health_report_${new Date().toISOString().split('T')[0]}.json`;
            a.click();
            URL.revokeObjectURL(url);

            this.showNotification('Relatório exportado com sucesso!', 'success');
        } catch (e) {
            console.error('Error exporting team health:', e);
            this.showNotification('Erro ao exportar relatório', 'error');
        }
    }
    '''
