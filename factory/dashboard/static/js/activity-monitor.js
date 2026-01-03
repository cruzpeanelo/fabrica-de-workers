/**
 * Activity Monitor - Painel de monitoramento em tempo real dos agentes
 *
 * Mostra o que cada agente esta fazendo em tempo real, mesmo em modo autonomo.
 * Usa Server-Sent Events (SSE) para atualizacoes instantaneas.
 */

class ActivityMonitor {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.eventSource = null;
        this.activities = [];
        this.maxActivities = 100;
        this.agents = {};
        this.paused = false;

        if (this.container) {
            this.init();
        }
    }

    async init() {
        this.render();
        await this.fetchInitialData();
        this.connectSSE();
        this.startPolling();
    }

    render() {
        this.container.innerHTML = `
            <div class="activity-monitor">
                <div class="monitor-header">
                    <h3><i class="fas fa-tv"></i> Monitor de Atividades</h3>
                    <div class="header-controls">
                        <span class="connection-status" id="conn-status">
                            <span class="status-dot"></span>
                            <span class="status-text">Conectando...</span>
                        </span>
                        <button class="btn-icon" id="btn-pause" onclick="activityMonitor.togglePause()" title="Pausar">
                            <i class="fas fa-pause"></i>
                        </button>
                        <button class="btn-icon" onclick="activityMonitor.clearActivities()" title="Limpar">
                            <i class="fas fa-trash"></i>
                        </button>
                    </div>
                </div>

                <div class="monitor-body">
                    <!-- Agentes ativos -->
                    <div class="agents-strip" id="agents-strip">
                        <div class="no-agents">Nenhum agente ativo</div>
                    </div>

                    <!-- Timeline de atividades -->
                    <div class="activity-timeline" id="activity-timeline">
                        <div class="loading">
                            <i class="fas fa-spinner fa-spin"></i> Carregando atividades...
                        </div>
                    </div>
                </div>

                <!-- Barra de filtros -->
                <div class="filter-bar">
                    <select id="filter-agent" onchange="activityMonitor.applyFilter()">
                        <option value="">Todos os agentes</option>
                    </select>
                    <select id="filter-type" onchange="activityMonitor.applyFilter()">
                        <option value="">Todos os tipos</option>
                        <option value="code_generate">Codigo</option>
                        <option value="file_write">Arquivos</option>
                        <option value="test_run">Testes</option>
                        <option value="git_commit">Git</option>
                        <option value="thinking">Pensamento</option>
                        <option value="error">Erros</option>
                    </select>
                </div>
            </div>
        `;

        this.addStyles();
    }

    addStyles() {
        if (document.getElementById('activity-monitor-styles')) return;

        const styles = document.createElement('style');
        styles.id = 'activity-monitor-styles';
        styles.textContent = `
            .activity-monitor {
                background: #1a1a2e;
                border-radius: 8px;
                overflow: hidden;
                color: #e0e0e0;
                font-family: 'Fira Code', 'Consolas', monospace;
            }

            .monitor-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 12px 16px;
                background: #16213e;
                border-bottom: 1px solid #0f3460;
            }

            .monitor-header h3 {
                margin: 0;
                font-size: 1rem;
                color: #e94560;
            }

            .header-controls {
                display: flex;
                align-items: center;
                gap: 12px;
            }

            .connection-status {
                display: flex;
                align-items: center;
                gap: 6px;
                font-size: 0.8rem;
            }

            .connection-status .status-dot {
                width: 8px;
                height: 8px;
                border-radius: 50%;
                background: #ffc107;
            }

            .connection-status.connected .status-dot {
                background: #4caf50;
                animation: pulse 2s infinite;
            }

            .connection-status.disconnected .status-dot {
                background: #f44336;
            }

            .btn-icon {
                background: transparent;
                border: 1px solid #0f3460;
                color: #e0e0e0;
                padding: 6px 10px;
                border-radius: 4px;
                cursor: pointer;
            }

            .btn-icon:hover {
                background: #0f3460;
            }

            .btn-icon.active {
                background: #e94560;
                border-color: #e94560;
            }

            .agents-strip {
                display: flex;
                gap: 10px;
                padding: 12px 16px;
                background: #16213e;
                border-bottom: 1px solid #0f3460;
                overflow-x: auto;
            }

            .agent-chip {
                display: flex;
                align-items: center;
                gap: 8px;
                padding: 8px 14px;
                background: #1a1a2e;
                border-radius: 20px;
                border: 1px solid #0f3460;
                white-space: nowrap;
            }

            .agent-chip.active {
                border-color: #4caf50;
                background: rgba(76, 175, 80, 0.1);
            }

            .agent-chip .agent-icon {
                width: 24px;
                height: 24px;
                border-radius: 50%;
                display: flex;
                align-items: center;
                justify-content: center;
                font-size: 0.7rem;
                font-weight: bold;
            }

            .agent-chip .agent-status {
                font-size: 0.75rem;
                color: #888;
            }

            .agent-chip .status-indicator {
                width: 6px;
                height: 6px;
                border-radius: 50%;
                background: #888;
            }

            .agent-chip.active .status-indicator {
                background: #4caf50;
                animation: pulse 1.5s infinite;
            }

            @keyframes pulse {
                0%, 100% { opacity: 1; }
                50% { opacity: 0.4; }
            }

            .activity-timeline {
                max-height: 500px;
                overflow-y: auto;
                padding: 16px;
            }

            .activity-item {
                display: flex;
                gap: 12px;
                padding: 10px 0;
                border-bottom: 1px solid #0f3460;
                animation: slideIn 0.3s ease;
            }

            @keyframes slideIn {
                from {
                    opacity: 0;
                    transform: translateX(-20px);
                }
                to {
                    opacity: 1;
                    transform: translateX(0);
                }
            }

            .activity-time {
                font-size: 0.75rem;
                color: #666;
                min-width: 60px;
            }

            .activity-agent {
                font-size: 0.8rem;
                font-weight: bold;
                min-width: 50px;
                padding: 2px 8px;
                border-radius: 4px;
                text-align: center;
            }

            .activity-agent.BACK { background: #1976d2; color: white; }
            .activity-agent.FRONT { background: #7b1fa2; color: white; }
            .activity-agent.DEVOPS { background: #388e3c; color: white; }
            .activity-agent.QA { background: #f57c00; color: white; }
            .activity-agent.SEC { background: #d32f2f; color: white; }
            .activity-agent.ARCH { background: #5d4037; color: white; }
            .activity-agent.ORCH { background: #e94560; color: white; }
            .activity-agent.PROD { background: #00796b; color: white; }
            .activity-agent.INOV { background: #512da8; color: white; }
            .activity-agent.FIN { background: #fbc02d; color: #333; }
            .activity-agent.GROWTH { background: #c2185b; color: white; }

            .activity-content {
                flex: 1;
            }

            .activity-title {
                font-size: 0.9rem;
                margin-bottom: 4px;
            }

            .activity-desc {
                font-size: 0.8rem;
                color: #888;
            }

            .activity-icon {
                font-size: 1rem;
                min-width: 24px;
            }

            .activity-icon.code_generate { color: #4caf50; }
            .activity-icon.file_write { color: #2196f3; }
            .activity-icon.file_edit { color: #ff9800; }
            .activity-icon.test_run { color: #9c27b0; }
            .activity-icon.test_pass { color: #4caf50; }
            .activity-icon.test_fail { color: #f44336; }
            .activity-icon.git_commit { color: #f44336; }
            .activity-icon.thinking { color: #ffc107; }
            .activity-icon.error { color: #f44336; }
            .activity-icon.handoff_out { color: #00bcd4; }
            .activity-icon.task_start { color: #4caf50; }
            .activity-icon.task_complete { color: #8bc34a; }

            .filter-bar {
                display: flex;
                gap: 10px;
                padding: 10px 16px;
                background: #16213e;
                border-top: 1px solid #0f3460;
            }

            .filter-bar select {
                background: #1a1a2e;
                color: #e0e0e0;
                border: 1px solid #0f3460;
                padding: 6px 10px;
                border-radius: 4px;
                font-size: 0.8rem;
            }

            .no-agents, .loading {
                color: #666;
                font-style: italic;
                padding: 10px;
            }

            .activity-timeline::-webkit-scrollbar {
                width: 8px;
            }

            .activity-timeline::-webkit-scrollbar-track {
                background: #1a1a2e;
            }

            .activity-timeline::-webkit-scrollbar-thumb {
                background: #0f3460;
                border-radius: 4px;
            }
        `;
        document.head.appendChild(styles);
    }

    async fetchInitialData() {
        try {
            // Buscar atividades recentes
            const activitiesResp = await fetch('/api/activity/recent?limit=50');
            const activitiesData = await activitiesResp.json();

            if (activitiesData.success) {
                this.activities = activitiesData.data;
            }

            // Buscar status dos agentes
            const agentsResp = await fetch('/api/activity/agents');
            const agentsData = await agentsResp.json();

            if (agentsData.success) {
                this.agents = agentsData.data;
            }

            this.updateUI();
        } catch (error) {
            console.error('Erro ao buscar dados:', error);
        }
    }

    connectSSE() {
        if (this.eventSource) {
            this.eventSource.close();
        }

        try {
            this.eventSource = new EventSource('/api/activity/stream');

            this.eventSource.onopen = () => {
                this.setConnectionStatus('connected');
            };

            this.eventSource.onmessage = (event) => {
                const data = JSON.parse(event.data);

                if (data.type === 'heartbeat' || data.type === 'connected') {
                    return;
                }

                if (!this.paused) {
                    this.addActivity(data);
                }
            };

            this.eventSource.onerror = () => {
                this.setConnectionStatus('disconnected');
                // Tentar reconectar em 5s
                setTimeout(() => this.connectSSE(), 5000);
            };
        } catch (error) {
            console.error('Erro ao conectar SSE:', error);
            this.setConnectionStatus('disconnected');
        }
    }

    setConnectionStatus(status) {
        const statusEl = document.getElementById('conn-status');
        statusEl.className = `connection-status ${status}`;

        const textEl = statusEl.querySelector('.status-text');
        textEl.textContent = {
            'connected': 'Conectado',
            'disconnected': 'Desconectado',
            'connecting': 'Conectando...'
        }[status] || status;
    }

    addActivity(activity) {
        // Adicionar no inicio
        this.activities.unshift(activity);

        // Limitar quantidade
        if (this.activities.length > this.maxActivities) {
            this.activities.pop();
        }

        // Atualizar status do agente
        if (activity.agent_type) {
            this.agents[activity.agent_type] = {
                agent_type: activity.agent_type,
                last_activity: activity.timestamp,
                current_action: activity.title,
                status: this.inferStatus(activity.activity_type)
            };
        }

        this.updateUI();
    }

    inferStatus(activityType) {
        const statusMap = {
            'agent_stop': 'idle',
            'task_complete': 'idle',
            'error': 'error',
            'thinking': 'thinking',
            'file_write': 'coding',
            'file_edit': 'coding',
            'code_generate': 'coding',
            'test_run': 'testing',
            'handoff_out': 'handoff'
        };
        return statusMap[activityType] || 'working';
    }

    updateUI() {
        this.updateAgentsStrip();
        this.updateTimeline();
        this.updateFilters();
    }

    updateAgentsStrip() {
        const strip = document.getElementById('agents-strip');

        if (Object.keys(this.agents).length === 0) {
            strip.innerHTML = '<div class="no-agents">Nenhum agente ativo</div>';
            return;
        }

        strip.innerHTML = Object.entries(this.agents).map(([type, info]) => {
            const isActive = info.status !== 'idle';
            const statusText = {
                'working': 'Trabalhando',
                'coding': 'Codificando',
                'testing': 'Testando',
                'thinking': 'Analisando',
                'error': 'Erro',
                'handoff': 'Handoff',
                'idle': 'Parado'
            }[info.status] || info.status;

            return `
                <div class="agent-chip ${isActive ? 'active' : ''}">
                    <div class="agent-icon" style="background: ${this.getAgentColor(type)}">
                        ${type.substring(0, 2)}
                    </div>
                    <div>
                        <div style="font-weight: bold; font-size: 0.85rem">${type}</div>
                        <div class="agent-status">${statusText}</div>
                    </div>
                    <div class="status-indicator"></div>
                </div>
            `;
        }).join('');
    }

    getAgentColor(type) {
        const colors = {
            'BACK': '#1976d2',
            'FRONT': '#7b1fa2',
            'DEVOPS': '#388e3c',
            'QA': '#f57c00',
            'SEC': '#d32f2f',
            'ARCH': '#5d4037',
            'ORCH': '#e94560',
            'PROD': '#00796b',
            'INOV': '#512da8',
            'FIN': '#fbc02d',
            'GROWTH': '#c2185b'
        };
        return colors[type] || '#666';
    }

    updateTimeline() {
        const timeline = document.getElementById('activity-timeline');

        // Aplicar filtros
        let filtered = this.activities;

        const agentFilter = document.getElementById('filter-agent')?.value;
        const typeFilter = document.getElementById('filter-type')?.value;

        if (agentFilter) {
            filtered = filtered.filter(a => a.agent_type === agentFilter);
        }

        if (typeFilter) {
            filtered = filtered.filter(a => a.activity_type === typeFilter);
        }

        if (filtered.length === 0) {
            timeline.innerHTML = '<div class="no-agents">Nenhuma atividade registrada</div>';
            return;
        }

        timeline.innerHTML = filtered.map(activity => {
            const time = new Date(activity.timestamp).toLocaleTimeString('pt-BR', {
                hour: '2-digit',
                minute: '2-digit',
                second: '2-digit'
            });

            return `
                <div class="activity-item">
                    <span class="activity-time">${time}</span>
                    <span class="activity-agent ${activity.agent_type}">${activity.agent_type}</span>
                    <span class="activity-icon ${activity.activity_type}">
                        <i class="fas ${this.getActivityIcon(activity.activity_type)}"></i>
                    </span>
                    <div class="activity-content">
                        <div class="activity-title">${activity.title}</div>
                        ${activity.description ? `<div class="activity-desc">${activity.description}</div>` : ''}
                    </div>
                </div>
            `;
        }).join('');
    }

    getActivityIcon(type) {
        const icons = {
            'agent_start': 'fa-play',
            'agent_stop': 'fa-stop',
            'task_start': 'fa-rocket',
            'task_progress': 'fa-spinner',
            'task_complete': 'fa-check-circle',
            'task_error': 'fa-exclamation-circle',
            'file_read': 'fa-file-alt',
            'file_write': 'fa-file-code',
            'file_edit': 'fa-edit',
            'code_generate': 'fa-code',
            'code_review': 'fa-search',
            'test_run': 'fa-vial',
            'test_pass': 'fa-check',
            'test_fail': 'fa-times',
            'git_commit': 'fa-code-branch',
            'git_push': 'fa-cloud-upload-alt',
            'handoff_out': 'fa-exchange-alt',
            'handoff_in': 'fa-sign-in-alt',
            'thinking': 'fa-brain',
            'decision': 'fa-lightbulb',
            'error': 'fa-exclamation-triangle',
            'warning': 'fa-exclamation',
            'info': 'fa-info-circle'
        };
        return icons[type] || 'fa-circle';
    }

    updateFilters() {
        const agentSelect = document.getElementById('filter-agent');
        const currentAgent = agentSelect.value;

        // Manter valor atual e adicionar novos agentes
        const agents = [...new Set(this.activities.map(a => a.agent_type))];

        // Preservar opcao "Todos"
        const options = '<option value="">Todos os agentes</option>' +
            agents.map(a => `<option value="${a}" ${a === currentAgent ? 'selected' : ''}>${a}</option>`).join('');

        agentSelect.innerHTML = options;
    }

    togglePause() {
        this.paused = !this.paused;
        const btn = document.getElementById('btn-pause');
        btn.classList.toggle('active', this.paused);
        btn.innerHTML = this.paused ?
            '<i class="fas fa-play"></i>' :
            '<i class="fas fa-pause"></i>';
    }

    clearActivities() {
        this.activities = [];
        this.updateTimeline();
    }

    applyFilter() {
        this.updateTimeline();
    }

    startPolling() {
        // Polling backup para atualizar status dos agentes
        setInterval(async () => {
            try {
                const resp = await fetch('/api/activity/agents');
                const data = await resp.json();
                if (data.success) {
                    this.agents = data.data;
                    this.updateAgentsStrip();
                }
            } catch (e) {
                // Ignore
            }
        }, 10000);
    }

    destroy() {
        if (this.eventSource) {
            this.eventSource.close();
        }
    }
}

// Inicializar
let activityMonitor;
document.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById('activity-monitor-container')) {
        activityMonitor = new ActivityMonitor('activity-monitor-container');
    }
});
