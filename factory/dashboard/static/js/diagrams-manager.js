/**
 * Diagrams Manager - Gerenciador de diagramas Draw.io
 *
 * Funcionalidades:
 * - Listar diagramas
 * - Criar novos diagramas a partir de templates
 * - Abrir no VS Code
 * - Exportar para PNG/SVG/PDF
 * - Gerar diagramas automaticamente do codigo
 */

class DiagramsManager {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.state = {
            diagrams: [],
            templates: []
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
            <div class="diagrams-panel">
                <div class="panel-header">
                    <h3><i class="fas fa-project-diagram"></i> Diagramas</h3>
                    <div class="header-actions">
                        <button class="btn btn-sm btn-secondary" onclick="diagramsManager.generateFromCode()">
                            <i class="fas fa-magic"></i> Auto-gerar
                        </button>
                        <button class="btn btn-sm btn-primary" onclick="diagramsManager.showCreateModal()">
                            <i class="fas fa-plus"></i> Novo
                        </button>
                    </div>
                </div>

                <div class="panel-body">
                    <!-- Templates -->
                    <div class="templates-section">
                        <h4>Templates Disponiveis</h4>
                        <div class="templates-grid" id="templates-grid">
                            <div class="loading">Carregando...</div>
                        </div>
                    </div>

                    <!-- Diagramas -->
                    <div class="diagrams-section">
                        <h4>Meus Diagramas</h4>
                        <div class="diagrams-grid" id="diagrams-grid">
                            <div class="loading">Carregando...</div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Modal Criar -->
            <div class="modal" id="create-diagram-modal" style="display:none">
                <div class="modal-content">
                    <div class="modal-header">
                        <h4>Novo Diagrama</h4>
                        <button class="close-btn" onclick="diagramsManager.hideModal()">&times;</button>
                    </div>
                    <div class="modal-body">
                        <div class="form-group">
                            <label>Nome:</label>
                            <input type="text" id="diagram-name" placeholder="meu_diagrama">
                        </div>
                        <div class="form-group">
                            <label>Tipo:</label>
                            <select id="diagram-type"></select>
                        </div>
                        <div class="form-group">
                            <label>Descricao:</label>
                            <textarea id="diagram-description" rows="2"></textarea>
                        </div>
                    </div>
                    <div class="modal-footer">
                        <button class="btn btn-secondary" onclick="diagramsManager.hideModal()">Cancelar</button>
                        <button class="btn btn-primary" onclick="diagramsManager.createDiagram()">Criar</button>
                    </div>
                </div>
            </div>

            <!-- Modal Auto-gerar -->
            <div class="modal" id="generate-diagram-modal" style="display:none">
                <div class="modal-content">
                    <div class="modal-header">
                        <h4>Gerar Diagrama do Codigo</h4>
                        <button class="close-btn" onclick="diagramsManager.hideGenerateModal()">&times;</button>
                    </div>
                    <div class="modal-body">
                        <div class="form-group">
                            <label>Caminho do Codigo:</label>
                            <input type="text" id="generate-path" placeholder="factory/core">
                        </div>
                        <div class="form-group">
                            <label>Nome do Diagrama:</label>
                            <input type="text" id="generate-name" placeholder="architecture_auto">
                        </div>
                    </div>
                    <div class="modal-footer">
                        <button class="btn btn-secondary" onclick="diagramsManager.hideGenerateModal()">Cancelar</button>
                        <button class="btn btn-primary" onclick="diagramsManager.doGenerate()">Gerar</button>
                    </div>
                </div>
            </div>
        `;

        this.addStyles();
    }

    addStyles() {
        if (document.getElementById('diagrams-styles')) return;

        const styles = document.createElement('style');
        styles.id = 'diagrams-styles';
        styles.textContent = `
            .diagrams-panel {
                background: #fff;
                border-radius: 8px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                margin-bottom: 20px;
            }

            .diagrams-panel .panel-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid #e9ecef;
                background: #003B4A;
                color: white;
                border-radius: 8px 8px 0 0;
            }

            .diagrams-panel .panel-header h3 {
                margin: 0;
                font-size: 1.1rem;
            }

            .header-actions {
                display: flex;
                gap: 8px;
            }

            .templates-section,
            .diagrams-section {
                padding: 20px;
                border-bottom: 1px solid #e9ecef;
            }

            .diagrams-section { border-bottom: none; }

            .templates-section h4,
            .diagrams-section h4 {
                margin: 0 0 15px 0;
                font-size: 1rem;
                color: #495057;
            }

            .templates-grid {
                display: flex;
                gap: 10px;
                flex-wrap: wrap;
            }

            .template-card {
                border: 1px solid #e9ecef;
                border-radius: 6px;
                padding: 10px 15px;
                cursor: pointer;
                transition: all 0.2s;
                display: flex;
                align-items: center;
                gap: 8px;
            }

            .template-card:hover {
                border-color: #FF6C00;
                background: #fffaf5;
            }

            .template-card i {
                color: #FF6C00;
            }

            .template-name {
                font-weight: 500;
            }

            .diagrams-grid {
                display: grid;
                grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
                gap: 15px;
            }

            .diagram-card {
                border: 1px solid #e9ecef;
                border-radius: 8px;
                overflow: hidden;
                transition: all 0.2s;
            }

            .diagram-card:hover {
                border-color: #FF6C00;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
            }

            .diagram-preview {
                height: 120px;
                background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
                display: flex;
                align-items: center;
                justify-content: center;
                font-size: 3rem;
                color: #adb5bd;
            }

            .diagram-info {
                padding: 15px;
            }

            .diagram-name {
                font-weight: bold;
                color: #003B4A;
                margin-bottom: 4px;
            }

            .diagram-meta {
                font-size: 0.8rem;
                color: #6c757d;
            }

            .diagram-actions {
                display: flex;
                gap: 5px;
                margin-top: 10px;
            }

            .diagram-actions button {
                flex: 1;
                padding: 6px;
                border: 1px solid #ced4da;
                border-radius: 4px;
                background: white;
                cursor: pointer;
                font-size: 0.75rem;
            }

            .diagram-actions button:hover {
                background: #f8f9fa;
            }

            .no-diagrams {
                grid-column: 1 / -1;
                text-align: center;
                color: #6c757d;
                padding: 40px;
            }
        `;
        document.head.appendChild(styles);
    }

    async fetchData() {
        try {
            // Templates
            const templatesResp = await fetch('/api/diagrams/templates');
            const templatesData = await templatesResp.json();
            if (templatesData.success) this.state.templates = templatesData.data;

            // Diagramas
            const diagramsResp = await fetch('/api/diagrams/');
            const diagramsData = await diagramsResp.json();
            if (diagramsData.success) this.state.diagrams = diagramsData.data;

            this.updateUI();
        } catch (error) {
            console.error('Erro ao buscar dados:', error);
        }
    }

    updateUI() {
        // Templates
        const templatesGrid = document.getElementById('templates-grid');
        templatesGrid.innerHTML = this.state.templates.map(t => `
            <div class="template-card" onclick="diagramsManager.createFromTemplate('${t.type}')" title="${t.description}">
                <i class="fas ${this.getTypeIcon(t.type)}"></i>
                <span class="template-name">${t.name}</span>
            </div>
        `).join('');

        // Diagramas
        const diagramsGrid = document.getElementById('diagrams-grid');

        if (this.state.diagrams.length === 0) {
            diagramsGrid.innerHTML = `
                <div class="no-diagrams">
                    <i class="fas fa-project-diagram" style="font-size:2rem;margin-bottom:10px;display:block;"></i>
                    Nenhum diagrama criado
                </div>
            `;
        } else {
            diagramsGrid.innerHTML = this.state.diagrams.map(d => `
                <div class="diagram-card">
                    <div class="diagram-preview">
                        <i class="fas ${this.getTypeIcon(d.type)}"></i>
                    </div>
                    <div class="diagram-info">
                        <div class="diagram-name">${d.name}</div>
                        <div class="diagram-meta">
                            Atualizado: ${new Date(d.updated_at).toLocaleDateString()}
                        </div>
                        <div class="diagram-actions">
                            <button onclick="diagramsManager.openInVSCode('${d.name}')" title="Abrir no VS Code">
                                <i class="fas fa-external-link-alt"></i> VS Code
                            </button>
                            <button onclick="diagramsManager.exportDiagram('${d.name}')" title="Exportar">
                                <i class="fas fa-download"></i> Exportar
                            </button>
                            <button onclick="diagramsManager.deleteDiagram('${d.name}')" title="Deletar">
                                <i class="fas fa-trash"></i>
                            </button>
                        </div>
                    </div>
                </div>
            `).join('');
        }

        // Select de tipos no modal
        const typeSelect = document.getElementById('diagram-type');
        if (typeSelect) {
            typeSelect.innerHTML = this.state.templates.map(t =>
                `<option value="${t.type}">${t.name}</option>`
            ).join('');
        }
    }

    getTypeIcon(type) {
        const icons = {
            'architecture': 'fa-sitemap',
            'flowchart': 'fa-stream',
            'sequence': 'fa-exchange-alt',
            'er_diagram': 'fa-database',
            'class_diagram': 'fa-cubes',
            'component': 'fa-puzzle-piece',
            'deployment': 'fa-cloud-upload-alt',
            'custom': 'fa-shapes'
        };
        return icons[type] || 'fa-project-diagram';
    }

    createFromTemplate(type) {
        document.getElementById('diagram-type').value = type;
        this.showCreateModal();
    }

    showCreateModal() {
        document.getElementById('create-diagram-modal').style.display = 'flex';
    }

    hideModal() {
        document.getElementById('create-diagram-modal').style.display = 'none';
        document.getElementById('diagram-name').value = '';
        document.getElementById('diagram-description').value = '';
    }

    async createDiagram() {
        const name = document.getElementById('diagram-name').value;
        const type = document.getElementById('diagram-type').value;
        const description = document.getElementById('diagram-description').value;

        if (!name) {
            this.showToast('Nome e obrigatorio', 'warning');
            return;
        }

        try {
            const response = await fetch('/api/diagrams/', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name, type, description })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Diagrama criado!', 'success');
                this.hideModal();
                await this.fetchData();

                // Perguntar se quer abrir
                if (confirm('Deseja abrir o diagrama no VS Code?')) {
                    this.openInVSCode(name);
                }
            } else {
                this.showToast(data.error || 'Erro ao criar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async openInVSCode(name) {
        try {
            const response = await fetch('/api/diagrams/open', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Abrindo no VS Code...', 'success');
            } else {
                this.showToast(data.error || 'Erro ao abrir', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async exportDiagram(name) {
        const format = prompt('Formato de exportacao (png, svg, pdf):', 'png');
        if (!format) return;

        try {
            const response = await fetch('/api/diagrams/export', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ name, format })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast(`Exportado para ${format}!`, 'success');
            } else {
                this.showToast(data.error || 'Erro ao exportar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    async deleteDiagram(name) {
        if (!confirm(`Deletar diagrama ${name}?`)) return;

        try {
            const response = await fetch(`/api/diagrams/${name}`, {
                method: 'DELETE'
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Diagrama deletado!', 'success');
                await this.fetchData();
            } else {
                this.showToast(data.error || 'Erro ao deletar', 'error');
            }
        } catch (error) {
            this.showToast('Erro de conexao', 'error');
        }
    }

    generateFromCode() {
        document.getElementById('generate-diagram-modal').style.display = 'flex';
    }

    hideGenerateModal() {
        document.getElementById('generate-diagram-modal').style.display = 'none';
        document.getElementById('generate-path').value = '';
        document.getElementById('generate-name').value = '';
    }

    async doGenerate() {
        const sourcePath = document.getElementById('generate-path').value;
        const outputName = document.getElementById('generate-name').value || 'architecture_auto';

        if (!sourcePath) {
            this.showToast('Caminho e obrigatorio', 'warning');
            return;
        }

        try {
            const response = await fetch('/api/diagrams/generate', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ source_path: sourcePath, output_name: outputName })
            });

            const data = await response.json();

            if (data.success) {
                this.showToast('Diagrama gerado!', 'success');
                this.hideGenerateModal();
                await this.fetchData();
            } else {
                this.showToast(data.error || 'Erro ao gerar', 'error');
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
let diagramsManager;
document.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById('diagrams-container')) {
        diagramsManager = new DiagramsManager('diagrams-container');
    }
});
