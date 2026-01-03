/**
 * Hooks Config - Configuracao e gerenciamento de Hooks
 *
 * Funcionalidades:
 * - Listar hooks registrados
 * - Registrar novos hooks
 * - Remover hooks
 * - Disparar eventos manualmente
 * - Ver historico de execucoes
 */

class HooksConfig {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.state = {
            hooks: [],
            events: [],
            logs: []
        };

        if (this.container) {
            this.init();
        }
    }

    async init() {
        this.render();
        await this.fetchData();
    }

    render() {
        this.container.innerHTML = `
            <div class="hooks-panel">
                <div class="panel-header">
                    <h3><i class="fas fa-plug"></i> Hooks do Sistema</h3>
                    <button class="btn btn-sm btn-primary" onclick="hooksConfig.showAddModal()">
                        <i class="fas fa-plus"></i> Novo Hook
                    </button>
                </div>

                <div class="panel-body">
                    <!-- Tabs -->
                    <div class="tabs">
                        <button class="tab active" data-tab="hooks" onclick="hooksConfig.switchTab('hooks')">
                            Hooks Registrados
                        </button>
                        <button class="tab" data-tab="events" onclick="hooksConfig.switchTab('events')">
                            Eventos
                        </button>
                        <button class="tab" data-tab="logs" onclick="hooksConfig.switchTab('logs')">
                            Historico
                        </button>
                    </div>

                    <!-- Conteudo -->
                    <div class="tab-content" id="tab-hooks">
                        <div class="hooks-list" id="hooks-list">
                            <div class="loading">Carregando...</div>
                        </div>
                    </div>

                    <div class="tab-content" id="tab-events" style="display:none">
                        <div class="events-list" id="events-list"></div>
                    </div>

                    <div class="tab-content" id="tab-logs" style="display:none">
                        <div class="logs-list" id="logs-list"></div>
                    </div>
                </div>
            </div>

            <!-- Modal Adicionar Hook -->
            <div class="modal" id="add-hook-modal" style="display:none">
                <div class="modal-content">
                    <div class="modal-header">
                        <h4>Novo Hook</h4>
                        <button class="close-btn" onclick="hooksConfig.hideModal()">&times;</button>
                    </div>
                    <div class="modal-body">
                        <div class="form-group">
                            <label>Nome:</label>
                            <input type="text" id="hook-name" placeholder="meu_hook">
                        </div>
                        <div class="form-group">
                            <label>Evento:</label>
                            <select id="hook-event"></select>
                        </div>
                        <div class="form-group">
                            <label>Prioridade:</label>
                            <select id="hook-priority">
                                <option value="low">Baixa</option>
                                <option value="normal" selected>Normal</option>
                                <option value="high">Alta</option>
                                <option value="critical">Critica</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Descricao:</label>
                            <textarea id="hook-description" rows="2"></textarea>
                        </div>
                        <div class="form-group">
                            <label>Arquivo de Callback (opcional):</label>
                            <input type="text" id="hook-callback" placeholder="factory/hooks/custom/meu_hook.py">
                        </div>
                    </div>
                    <div class="modal-footer">
                        <button class="btn btn-secondary" onclick="hooksConfig.hideModal()">Cancelar</button>
                        <button class="btn btn-primary" onclick="hooksConfig.saveHook()">Salvar</button>
                    </div>
                </div>
            </div>
        `;

        this.addStyles();
    }

    addStyles() {
        if (document.getElementById('hooks-styles')) return;

        const styles = document.createElement('style');
        styles.id = 'hooks-styles';
        styles.textContent = `
            .hooks-panel {
                background: #fff;
                border-radius: 8px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                margin-bottom: 20px;
            }

            .hooks-panel .panel-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid #e9ecef;
                background: #003B4A;
                color: white;
                border-radius: 8px 8px 0 0;
            }

            .hooks-panel .panel-header h3 {
                margin: 0;
                font-size: 1.1rem;
            }

            .btn-sm { padding: 5px 12px; font-size: 0.85rem; }

            .tabs {
                display: flex;
                border-bottom: 1px solid #e9ecef;
                padding: 0 20px;
            }

            .tab {
                padding: 12px 20px;
                border: none;
                background: none;
                cursor: pointer;
                color: #6c757d;
                border-bottom: 2px solid transparent;
                transition: all 0.2s;
            }

            .tab:hover { color: #003B4A; }
            .tab.active {
                color: #003B4A;
                border-bottom-color: #FF6C00;
            }

            .tab-content {
                padding: 20px;
            }

            .hook-item {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 12px 15px;
                border: 1px solid #e9ecef;
                border-radius: 6px;
                margin-bottom: 10px;
                transition: all 0.2s;
            }

            .hook-item:hover {
                border-color: #FF6C00;
                background: #fffaf5;
            }

            .hook-info {
                flex: 1;
            }

            .hook-name {
                font-weight: bold;
                color: #003B4A;
            }

            .hook-event {
                font-size: 0.85rem;
                color: #6c757d;
                margin-top: 4px;
            }

            .hook-badge {
                display: inline-block;
                padding: 2px 8px;
                border-radius: 12px;
                font-size: 0.75rem;
                margin-left: 8px;
            }

            .badge-enabled { background: #d4edda; color: #155724; }
            .badge-disabled { background: #f8d7da; color: #721c24; }
            .badge-low { background: #e2e3e5; color: #383d41; }
            .badge-normal { background: #cce5ff; color: #004085; }
            .badge-high { background: #fff3cd; color: #856404; }
            .badge-critical { background: #f8d7da; color: #721c24; }

            .hook-actions {
                display: flex;
                gap: 8px;
            }

            .hook-actions button {
                padding: 5px 10px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                background: white;
                cursor: pointer;
                font-size: 0.8rem;
            }

            .hook-actions button:hover {
                background: #f8f9fa;
            }

            .event-item {
                padding: 12px;
                border: 1px solid #e9ecef;
                border-radius: 6px;
                margin-bottom: 10px;
            }

            .event-name {
                font-weight: bold;
                color: #003B4A;
            }

            .event-desc {
                font-size: 0.85rem;
                color: #6c757d;
                margin-top: 4px;
            }

            .event-trigger {
                margin-top: 8px;
            }

            .log-item {
                padding: 10px;
                border-left: 3px solid #FF6C00;
                background: #f8f9fa;
                margin-bottom: 8px;
                font-size: 0.85rem;
            }

            .log-time {
                color: #6c757d;
                font-size: 0.75rem;
            }

            .log-event {
                font-weight: bold;
                color: #003B4A;
            }

            /* Modal */
            .modal {
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background: rgba(0,0,0,0.5);
                display: flex;
                align-items: center;
                justify-content: center;
                z-index: 1000;
            }

            .modal-content {
                background: white;
                border-radius: 8px;
                width: 500px;
                max-width: 90%;
            }

            .modal-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid #e9ecef;
            }

            .modal-header h4 { margin: 0; }

            .close-btn {
                border: none;
                background: none;
                font-size: 1.5rem;
                cursor: pointer;
                color: #6c757d;
            }

            .modal-body { padding: 20px; }

            .modal-footer {
                padding: 15px 20px;
                border-top: 1px solid #e9ecef;
                display: flex;
                justify-content: flex-end;
                gap: 10px;
            }

            .form-group { margin-bottom: 15px; }
            .form-group label {
                display: block;
                margin-bottom: 5px;
                font-weight: 500;
            }
            .form-group input,
            .form-group select,
            .form-group textarea {
                width: 100%;
                padding: 8px 12px;
                border: 1px solid #ced4da;
                border-radius: 4px;
            }

            .btn-secondary { background: #6c757d; color: white; border: none; }
        `;
        document.head.appendChild(styles);
    }

    async fetchData() {
        try {
            // Hooks
            const hooksResp = await fetch('/api/hooks/');
            const hooksData = await hooksResp.json();
            if (hooksData.success) this.state.hooks = hooksData.data;

            // Eventos
            const eventsResp = await fetch('/api/hooks/events');
            const eventsData = await eventsResp.json();
            if (eventsData.success) this.state.events = eventsData.data;

            // Logs
            const logsResp = await fetch('/api/hooks/logs?limit=50');
            const logsData = await logsResp.json();
            if (logsData.success) this.state.logs = logsData.data;

            this.updateUI();
        } catch (error) {
            console.error('Erro ao buscar dados:', error);
        }
    }

    updateUI() {
        // Hooks
        const hooksList = document.getElementById('hooks-list');
        if (this.state.hooks.length === 0) {
            hooksList.innerHTML = '<div class="no-data">Nenhum hook registrado</div>';
        } else {
            hooksList.innerHTML = this.state.hooks.map(hook => `
                <div class="hook-item">
                    <div class="hook-info">
                        <div class="hook-name">
                            ${hook.name}
                            <span class="hook-badge badge-${hook.enabled ? 'enabled' : 'disabled'}">
                                ${hook.enabled ? 'Ativo' : 'Inativo'}
                            </span>
                            <span class="hook-badge badge-${hook.priority}">
                                ${hook.priority}
                            </span>
                        </div>
                        <div class="hook-event">
                            <i class="fas fa-bolt"></i> ${hook.event}
                            ${hook.description ? ' - ' + hook.description : ''}
                        </div>
                    </div>
                    <div class="hook-actions">
                        <button onclick="hooksConfig.deleteHook('${hook.id}')" title="Remover">
                            <i class="fas fa-trash"></i>
                        </button>
                    </div>
                </div>
            `).join('');
        }

        // Eventos
        const eventsList = document.getElementById('events-list');
        eventsList.innerHTML = this.state.events.map(event => `
            <div class="event-item">
                <div class="event-name">${event.name}</div>
                <div class="event-desc">${event.description}</div>
                <div class="event-trigger">
                    <button class="btn btn-sm btn-primary" onclick="hooksConfig.triggerEvent('${event.name}')">
                        <i class="fas fa-play"></i> Disparar
                    </button>
                </div>
            </div>
        `).join('');

        // Logs
        const logsList = document.getElementById('logs-list');
        if (this.state.logs.length === 0) {
            logsList.innerHTML = '<div class="no-data">Nenhum log disponivel</div>';
        } else {
            logsList.innerHTML = this.state.logs.reverse().map(log => `
                <div class="log-item">
                    <span class="log-time">${log.timestamp}</span>
                    <span class="log-event">${log.event}</span>
                </div>
            `).join('');
        }

        // Preencher select de eventos no modal
        const eventSelect = document.getElementById('hook-event');
        if (eventSelect) {
            eventSelect.innerHTML = this.state.events.map(e =>
                `<option value="${e.name}">${e.name}</option>`
            ).join('');
        }
    }

    switchTab(tab) {
        // Atualizar tabs
        document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
        document.querySelector(`[data-tab="${tab}"]`).classList.add('active');

        // Mostrar conteudo
        document.querySelectorAll('.tab-content').forEach(c => c.style.display = 'none');
        document.getElementById(`tab-${tab}`).style.display = 'block';
    }

    showAddModal() {
        document.getElementById('add-hook-modal').style.display = 'flex';
    }

    hideModal() {
        document.getElementById('add-hook-modal').style.display = 'none';
        // Limpar form
        document.getElementById('hook-name').value = '';
        document.getElementById('hook-description').value = '';
        document.getElementById('hook-callback').value = '';
    }

    async saveHook() {
        const name = document.getElementById('hook-name').value;
        const event = document.getElementById('hook-event').value;
        const priority = document.getElementById('hook-priority').value;
        const description = document.getElementById('hook-description').value;
        const callback = document.getElementById('hook-callback').value;

        if (!name) {
            this.showToast('Nome e obrigatorio', 'warning');
            return;
        }

        try {
            const response = await fetch('/api/hooks/', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    name, event, priority, description,
                    callback_path: callback || null
                })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Hook criado!', 'success');
                this.hideModal();
                await this.fetchData();
            } else {
                this.showToast(data.error || 'Erro ao criar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async deleteHook(id) {
        if (!confirm('Remover este hook?')) return;

        try {
            const response = await fetch(`/api/hooks/${id}`, {
                method: 'DELETE'
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Hook removido!', 'success');
                await this.fetchData();
            } else {
                this.showToast(data.error || 'Erro ao remover', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async triggerEvent(event) {
        try {
            const response = await fetch('/api/hooks/trigger', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ event, context: {} })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Evento ${event} disparado!`, 'success');
                // Atualizar logs
                setTimeout(() => this.fetchData(), 1000);
            } else {
                this.showToast(data.error || 'Erro ao disparar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    showToast(message, type = 'info') {
        if (window.showToast) {
            window.showToast(message, type);
        } else {
            console.log(`[${type}] ${message}`);
            alert(message);
        }
    }
}

// Inicializar
let hooksConfig;
document.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById('hooks-container')) {
        hooksConfig = new HooksConfig('hooks-container');
    }
});
