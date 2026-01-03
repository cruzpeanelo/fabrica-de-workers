/**
 * Orchestrator Panel - Painel de controle do Orquestrador de Agentes
 *
 * Funcionalidades:
 * - Iniciar/Parar orquestrador
 * - Selecionar modo de operacao
 * - Visualizar agentes ativos
 * - Spawnar/Terminar agentes
 */

class OrchestratorPanel {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.state = {
            running: false,
            mode: 'supervised',
            agents: [],
            availableAgents: []
        };
        this.pollInterval = null;

        if (this.container) {
            this.init();
        }
    }

    async init() {
        this.render();
        await this.fetchStatus();
        this.startPolling();
    }

    render() {
        this.container.innerHTML = `
            <div class="orchestrator-panel">
                <div class="panel-header">
                    <h3><i class="fas fa-network-wired"></i> Orquestrador de Agentes</h3>
                    <div class="status-badge" id="orch-status">
                        <span class="status-dot"></span>
                        <span class="status-text">Desconectado</span>
                    </div>
                </div>

                <div class="panel-body">
                    <!-- Controles -->
                    <div class="controls-section">
                        <div class="mode-selector">
                            <label>Modo de Operacao:</label>
                            <select id="orch-mode" onchange="orchestratorPanel.changeMode(this.value)">
                                <option value="autonomous">Autonomo</option>
                                <option value="supervised" selected>Supervisionado</option>
                                <option value="interactive">Interativo</option>
                            </select>
                        </div>

                        <div class="action-buttons">
                            <button class="btn btn-success" id="btn-start" onclick="orchestratorPanel.start()">
                                <i class="fas fa-play"></i> Iniciar
                            </button>
                            <button class="btn btn-danger" id="btn-stop" onclick="orchestratorPanel.stop()" disabled>
                                <i class="fas fa-stop"></i> Parar
                            </button>
                        </div>
                    </div>

                    <!-- Agentes Ativos -->
                    <div class="agents-section">
                        <h4>Agentes Ativos</h4>
                        <div class="agents-grid" id="active-agents">
                            <div class="no-agents">Nenhum agente ativo</div>
                        </div>
                    </div>

                    <!-- Spawnar Agente -->
                    <div class="spawn-section">
                        <h4>Spawnar Agente</h4>
                        <div class="spawn-form">
                            <select id="agent-type">
                                <option value="">Selecione o tipo...</option>
                            </select>
                            <input type="text" id="agent-task" placeholder="Descricao da task (opcional)">
                            <button class="btn btn-primary" onclick="orchestratorPanel.spawnAgent()">
                                <i class="fas fa-plus"></i> Spawnar
                            </button>
                        </div>
                    </div>
                </div>
            </div>
        `;

        this.addStyles();
    }

    addStyles() {
        if (document.getElementById('orchestrator-styles')) return;

        const styles = document.createElement('style');
        styles.id = 'orchestrator-styles';
        styles.textContent = `
            .orchestrator-panel {
                background: #fff;
                border-radius: 8px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                margin-bottom: 20px;
            }

            .orchestrator-panel .panel-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid #e9ecef;
                background: #003B4A;
                color: white;
                border-radius: 8px 8px 0 0;
            }

            .orchestrator-panel .panel-header h3 {
                margin: 0;
                font-size: 1.1rem;
            }

            .status-badge {
                display: flex;
                align-items: center;
                gap: 8px;
                padding: 5px 12px;
                background: rgba(255,255,255,0.1);
                border-radius: 20px;
                font-size: 0.85rem;
            }

            .status-dot {
                width: 10px;
                height: 10px;
                border-radius: 50%;
                background: #dc3545;
            }

            .status-badge.running .status-dot {
                background: #28a745;
                animation: pulse 2s infinite;
            }

            @keyframes pulse {
                0%, 100% { opacity: 1; }
                50% { opacity: 0.5; }
            }

            .orchestrator-panel .panel-body {
                padding: 20px;
            }

            .controls-section {
                display: flex;
                justify-content: space-between;
                align-items: center;
                margin-bottom: 20px;
                padding-bottom: 20px;
                border-bottom: 1px solid #e9ecef;
            }

            .mode-selector {
                display: flex;
                align-items: center;
                gap: 10px;
            }

            .mode-selector select {
                padding: 8px 12px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                font-size: 0.9rem;
            }

            .action-buttons {
                display: flex;
                gap: 10px;
            }

            .action-buttons .btn {
                padding: 8px 20px;
                border: none;
                border-radius: 4px;
                cursor: pointer;
                display: flex;
                align-items: center;
                gap: 8px;
                font-size: 0.9rem;
            }

            .btn-success { background: #28a745; color: white; }
            .btn-danger { background: #dc3545; color: white; }
            .btn-primary { background: #FF6C00; color: white; }
            .btn:disabled { opacity: 0.5; cursor: not-allowed; }

            .agents-section h4,
            .spawn-section h4 {
                margin: 0 0 15px 0;
                font-size: 1rem;
                color: #495057;
            }

            .agents-grid {
                display: grid;
                grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
                gap: 10px;
                margin-bottom: 20px;
            }

            .agent-card {
                background: #f8f9fa;
                border: 1px solid #e9ecef;
                border-radius: 6px;
                padding: 12px;
                text-align: center;
                position: relative;
            }

            .agent-card.active {
                border-color: #28a745;
                background: #d4edda;
            }

            .agent-card .agent-type {
                font-weight: bold;
                font-size: 1.1rem;
                color: #003B4A;
            }

            .agent-card .agent-name {
                font-size: 0.8rem;
                color: #6c757d;
                margin-top: 4px;
            }

            .agent-card .agent-pid {
                font-size: 0.7rem;
                color: #adb5bd;
                margin-top: 4px;
            }

            .agent-card .terminate-btn {
                position: absolute;
                top: 5px;
                right: 5px;
                background: none;
                border: none;
                color: #dc3545;
                cursor: pointer;
                font-size: 0.8rem;
                padding: 2px 6px;
            }

            .no-agents {
                grid-column: 1 / -1;
                text-align: center;
                color: #6c757d;
                padding: 20px;
                font-style: italic;
            }

            .spawn-form {
                display: flex;
                gap: 10px;
                align-items: center;
            }

            .spawn-form select,
            .spawn-form input {
                padding: 8px 12px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                font-size: 0.9rem;
            }

            .spawn-form select { width: 180px; }
            .spawn-form input { flex: 1; }
        `;
        document.head.appendChild(styles);
    }

    async fetchStatus() {
        try {
            const response = await fetch('/api/orchestrator/status');
            const data = await response.json();

            if (data.success) {
                this.state.running = data.data.running;
                this.state.mode = data.data.mode;
                this.state.agents = data.data.agents || [];
            }

            // Buscar agentes disponiveis
            const agentsResp = await fetch('/api/orchestrator/agents');
            const agentsData = await agentsResp.json();

            if (agentsData.success) {
                this.state.availableAgents = agentsData.data.available || [];
                this.state.activeAgents = agentsData.data.active || [];
            }

            this.updateUI();
        } catch (error) {
            console.error('Erro ao buscar status:', error);
        }
    }

    updateUI() {
        // Status
        const statusBadge = document.getElementById('orch-status');
        const statusText = statusBadge.querySelector('.status-text');

        if (this.state.running) {
            statusBadge.classList.add('running');
            statusText.textContent = 'Rodando';
        } else {
            statusBadge.classList.remove('running');
            statusText.textContent = 'Parado';
        }

        // Botoes
        document.getElementById('btn-start').disabled = this.state.running;
        document.getElementById('btn-stop').disabled = !this.state.running;

        // Modo
        document.getElementById('orch-mode').value = this.state.mode;

        // Agentes ativos
        const agentsGrid = document.getElementById('active-agents');

        if (this.state.activeAgents && this.state.activeAgents.length > 0) {
            agentsGrid.innerHTML = this.state.activeAgents.map(agent => `
                <div class="agent-card active">
                    <button class="terminate-btn" onclick="orchestratorPanel.terminateAgent('${agent.type}')" title="Terminar">
                        <i class="fas fa-times"></i>
                    </button>
                    <div class="agent-type">${agent.type}</div>
                    <div class="agent-name">${this.getAgentName(agent.type)}</div>
                    <div class="agent-pid">PID: ${agent.pid}</div>
                </div>
            `).join('');
        } else {
            agentsGrid.innerHTML = '<div class="no-agents">Nenhum agente ativo</div>';
        }

        // Select de agentes
        const agentSelect = document.getElementById('agent-type');
        agentSelect.innerHTML = '<option value="">Selecione o tipo...</option>' +
            this.state.availableAgents.map(a =>
                `<option value="${a.type}">${a.type} - ${a.name}</option>`
            ).join('');
    }

    getAgentName(type) {
        const names = {
            'ORCH': 'Orquestrador',
            'ARCH': 'Arquiteto',
            'BACK': 'Backend',
            'FRONT': 'Frontend',
            'DEVOPS': 'DevOps',
            'SEC': 'Seguranca',
            'QA': 'Quality',
            'PROD': 'Produto',
            'INOV': 'Inovacao',
            'FIN': 'Financeiro',
            'GROWTH': 'Growth'
        };
        return names[type] || type;
    }

    async start() {
        const mode = document.getElementById('orch-mode').value;

        try {
            const response = await fetch('/api/orchestrator/start', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ mode })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Orquestrador iniciado!', 'success');
                await this.fetchStatus();
            } else {
                this.showToast(data.error || 'Erro ao iniciar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async stop() {
        try {
            const response = await fetch('/api/orchestrator/stop', {
                method: 'POST'
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Orquestrador parado!', 'success');
                await this.fetchStatus();
            } else {
                this.showToast(data.error || 'Erro ao parar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async changeMode(mode) {
        try {
            const response = await fetch('/api/orchestrator/mode', {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ mode })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Modo alterado para ${mode}`, 'success');
                this.state.mode = mode;
            }
        } catch (error) {
            this.showToast('Erro ao alterar modo', 'error');
        }
    }

    async spawnAgent() {
        const type = document.getElementById('agent-type').value;
        const task = document.getElementById('agent-task').value;

        if (!type) {
            this.showToast('Selecione o tipo de agente', 'warning');
            return;
        }

        try {
            const response = await fetch('/api/orchestrator/spawn', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ agent_type: type, task })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Agente ${type} spawneado!`, 'success');
                document.getElementById('agent-task').value = '';
                await this.fetchStatus();
            } else {
                this.showToast(data.error || 'Erro ao spawnar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async terminateAgent(type) {
        try {
            const response = await fetch('/api/orchestrator/terminate', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ agent_type: type })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Agente ${type} terminado!`, 'success');
                await this.fetchStatus();
            } else {
                this.showToast(data.error || 'Erro ao terminar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    startPolling() {
        this.pollInterval = setInterval(() => this.fetchStatus(), 5000);
    }

    stopPolling() {
        if (this.pollInterval) {
            clearInterval(this.pollInterval);
        }
    }

    showToast(message, type = 'info') {
        // Usar toast existente ou criar simples
        if (window.showToast) {
            window.showToast(message, type);
        } else {
            console.log(`[${type}] ${message}`);
            alert(message);
        }
    }
}

// Inicializar automaticamente se container existir
let orchestratorPanel;
document.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById('orchestrator-container')) {
        orchestratorPanel = new OrchestratorPanel('orchestrator-container');
    }
});
