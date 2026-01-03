/**
 * MCP Status - Painel de status dos servidores MCP
 *
 * Funcionalidades:
 * - Listar servidores MCP configurados
 * - Iniciar/Parar servidores
 * - Registrar novos servidores
 * - Monitorar status em tempo real
 */

class MCPStatus {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.state = {
            servers: []
        };
        this.pollInterval = null;

        if (this.container) {
            this.init();
        }
    }

    async init() {
        this.render();
        await this.fetchServers();
        this.startPolling();
    }

    render() {
        this.container.innerHTML = `
            <div class="mcp-panel">
                <div class="panel-header">
                    <h3><i class="fas fa-server"></i> Servidores MCP</h3>
                    <button class="btn btn-sm btn-primary" onclick="mcpStatus.showAddModal()">
                        <i class="fas fa-plus"></i> Novo Servidor
                    </button>
                </div>

                <div class="panel-body">
                    <div class="servers-grid" id="servers-grid">
                        <div class="loading">Carregando...</div>
                    </div>
                </div>
            </div>

            <!-- Modal Adicionar -->
            <div class="modal" id="add-mcp-modal" style="display:none">
                <div class="modal-content">
                    <div class="modal-header">
                        <h4>Novo Servidor MCP</h4>
                        <button class="close-btn" onclick="mcpStatus.hideModal()">&times;</button>
                    </div>
                    <div class="modal-body">
                        <div class="form-group">
                            <label>Nome:</label>
                            <input type="text" id="mcp-name" placeholder="meu_servidor">
                        </div>
                        <div class="form-group">
                            <label>Comando:</label>
                            <input type="text" id="mcp-command" placeholder="python">
                        </div>
                        <div class="form-group">
                            <label>Argumentos (separados por virgula):</label>
                            <input type="text" id="mcp-args" placeholder="server.py, --port, 8080">
                        </div>
                        <div class="form-group">
                            <label>Descricao:</label>
                            <input type="text" id="mcp-description" placeholder="Servidor para...">
                        </div>
                        <div class="form-group">
                            <label>
                                <input type="checkbox" id="mcp-autostart"> Auto-iniciar
                            </label>
                        </div>
                    </div>
                    <div class="modal-footer">
                        <button class="btn btn-secondary" onclick="mcpStatus.hideModal()">Cancelar</button>
                        <button class="btn btn-primary" onclick="mcpStatus.saveServer()">Salvar</button>
                    </div>
                </div>
            </div>
        `;

        this.addStyles();
    }

    addStyles() {
        if (document.getElementById('mcp-styles')) return;

        const styles = document.createElement('style');
        styles.id = 'mcp-styles';
        styles.textContent = `
            .mcp-panel {
                background: #fff;
                border-radius: 8px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                margin-bottom: 20px;
            }

            .mcp-panel .panel-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid #e9ecef;
                background: #003B4A;
                color: white;
                border-radius: 8px 8px 0 0;
            }

            .mcp-panel .panel-header h3 {
                margin: 0;
                font-size: 1.1rem;
            }

            .servers-grid {
                display: grid;
                grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
                gap: 15px;
                padding: 20px;
            }

            .server-card {
                border: 1px solid #e9ecef;
                border-radius: 8px;
                padding: 15px;
                transition: all 0.2s;
            }

            .server-card:hover {
                border-color: #FF6C00;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
            }

            .server-card.running {
                border-left: 4px solid #28a745;
            }

            .server-card.stopped {
                border-left: 4px solid #dc3545;
            }

            .server-header {
                display: flex;
                justify-content: space-between;
                align-items: flex-start;
                margin-bottom: 10px;
            }

            .server-name {
                font-weight: bold;
                font-size: 1.1rem;
                color: #003B4A;
            }

            .server-status {
                display: flex;
                align-items: center;
                gap: 5px;
                font-size: 0.8rem;
            }

            .status-dot {
                width: 8px;
                height: 8px;
                border-radius: 50%;
            }

            .status-dot.running { background: #28a745; }
            .status-dot.stopped { background: #dc3545; }

            .server-info {
                font-size: 0.85rem;
                color: #6c757d;
                margin-bottom: 10px;
            }

            .server-info code {
                background: #f8f9fa;
                padding: 2px 6px;
                border-radius: 3px;
                font-size: 0.8rem;
            }

            .server-desc {
                font-size: 0.85rem;
                color: #495057;
                margin-bottom: 12px;
            }

            .server-actions {
                display: flex;
                gap: 8px;
            }

            .server-actions button {
                flex: 1;
                padding: 8px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                background: white;
                cursor: pointer;
                font-size: 0.8rem;
                display: flex;
                align-items: center;
                justify-content: center;
                gap: 5px;
            }

            .server-actions button:hover {
                background: #f8f9fa;
            }

            .server-actions .btn-start { color: #28a745; border-color: #28a745; }
            .server-actions .btn-stop { color: #dc3545; border-color: #dc3545; }
            .server-actions .btn-restart { color: #ffc107; border-color: #ffc107; }
            .server-actions .btn-delete { color: #6c757d; }

            .no-servers {
                grid-column: 1 / -1;
                text-align: center;
                color: #6c757d;
                padding: 40px;
                font-style: italic;
            }

            .server-pid {
                font-size: 0.75rem;
                color: #adb5bd;
            }
        `;
        document.head.appendChild(styles);
    }

    async fetchServers() {
        try {
            const response = await fetch('/api/mcp/servers');
            const data = await response.json();

            if (data.success) {
                this.state.servers = data.data;
                this.updateUI();
            }
        } catch (error) {
            console.error('Erro ao buscar servidores:', error);
        }
    }

    updateUI() {
        const grid = document.getElementById('servers-grid');

        if (this.state.servers.length === 0) {
            grid.innerHTML = `
                <div class="no-servers">
                    <i class="fas fa-server" style="font-size:2rem;margin-bottom:10px;display:block;"></i>
                    Nenhum servidor MCP configurado
                </div>
            `;
            return;
        }

        grid.innerHTML = this.state.servers.map(server => `
            <div class="server-card ${server.running ? 'running' : 'stopped'}">
                <div class="server-header">
                    <div>
                        <div class="server-name">${server.name}</div>
                        ${server.pid ? `<div class="server-pid">PID: ${server.pid}</div>` : ''}
                    </div>
                    <div class="server-status">
                        <span class="status-dot ${server.running ? 'running' : 'stopped'}"></span>
                        ${server.running ? 'Rodando' : 'Parado'}
                    </div>
                </div>

                <div class="server-info">
                    <code>${server.command} ${(server.args || []).join(' ')}</code>
                </div>

                ${server.description ? `<div class="server-desc">${server.description}</div>` : ''}

                <div class="server-actions">
                    ${server.running ? `
                        <button class="btn-stop" onclick="mcpStatus.stopServer('${server.name}')">
                            <i class="fas fa-stop"></i> Parar
                        </button>
                        <button class="btn-restart" onclick="mcpStatus.restartServer('${server.name}')">
                            <i class="fas fa-sync"></i> Reiniciar
                        </button>
                    ` : `
                        <button class="btn-start" onclick="mcpStatus.startServer('${server.name}')">
                            <i class="fas fa-play"></i> Iniciar
                        </button>
                    `}
                    <button class="btn-delete" onclick="mcpStatus.deleteServer('${server.name}')" title="Remover">
                        <i class="fas fa-trash"></i>
                    </button>
                </div>
            </div>
        `).join('');
    }

    async startServer(name) {
        try {
            const response = await fetch('/api/mcp/start', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Servidor ${name} iniciado!`, 'success');
                await this.fetchServers();
            } else {
                this.showToast(data.error || 'Erro ao iniciar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async stopServer(name) {
        try {
            const response = await fetch('/api/mcp/stop', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Servidor ${name} parado!`, 'success');
                await this.fetchServers();
            } else {
                this.showToast(data.error || 'Erro ao parar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async restartServer(name) {
        try {
            const response = await fetch('/api/mcp/restart', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Servidor ${name} reiniciado!`, 'success');
                await this.fetchServers();
            } else {
                this.showToast(data.error || 'Erro ao reiniciar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async deleteServer(name) {
        if (!confirm(`Remover servidor ${name}?`)) return;

        try {
            const response = await fetch(`/api/mcp/${name}`, {
                method: 'DELETE'
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Servidor ${name} removido!`, 'success');
                await this.fetchServers();
            } else {
                this.showToast(data.error || 'Erro ao remover', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    showAddModal() {
        document.getElementById('add-mcp-modal').style.display = 'flex';
    }

    hideModal() {
        document.getElementById('add-mcp-modal').style.display = 'none';
        // Limpar form
        document.getElementById('mcp-name').value = '';
        document.getElementById('mcp-command').value = '';
        document.getElementById('mcp-args').value = '';
        document.getElementById('mcp-description').value = '';
        document.getElementById('mcp-autostart').checked = false;
    }

    async saveServer() {
        const name = document.getElementById('mcp-name').value;
        const command = document.getElementById('mcp-command').value;
        const argsStr = document.getElementById('mcp-args').value;
        const description = document.getElementById('mcp-description').value;
        const autoStart = document.getElementById('mcp-autostart').checked;

        if (!name || !command) {
            this.showToast('Nome e comando sao obrigatorios', 'warning');
            return;
        }

        const args = argsStr ? argsStr.split(',').map(a => a.trim()) : [];

        try {
            const response = await fetch('/api/mcp/register', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    name, command, args, description,
                    auto_start: autoStart,
                    enabled: true
                })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Servidor registrado!', 'success');
                this.hideModal();
                await this.fetchServers();
            } else {
                this.showToast(data.error || 'Erro ao registrar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    startPolling() {
        this.pollInterval = setInterval(() => this.fetchServers(), 10000);
    }

    stopPolling() {
        if (this.pollInterval) {
            clearInterval(this.pollInterval);
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
let mcpStatus;
document.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById('mcp-container')) {
        mcpStatus = new MCPStatus('mcp-container');
    }
});
