# -*- coding: utf-8 -*-
"""
Sandbox Terminal UI - Issue #202
================================
Interface web para terminal de execucao sandbox.
Componente visual com editor de codigo, inputs e console.

NOTA: Este modulo implementa apenas a UI.
Os endpoints de execucao devem ser implementados em:
- factory/api/sandbox_routes.py (Terminal B)
- factory/core/sandbox_executor.py (Terminal D)
"""

from typing import Optional, List, Dict, Any
from fastapi import APIRouter
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from datetime import datetime

# =============================================================================
# ROUTER
# =============================================================================

sandbox_terminal_router = APIRouter(prefix="/api/sandbox-ui", tags=["Sandbox Terminal"])


# =============================================================================
# MODELS
# =============================================================================

class ExecutionRequest(BaseModel):
    code: str
    inputs: Optional[Dict[str, Any]] = {}
    language: str = "python"
    timeout: int = 30


class ExecutionResult(BaseModel):
    execution_id: str
    status: str  # pending, running, success, error, timeout
    output: str
    error: Optional[str] = None
    duration_ms: int
    memory_mb: float
    created_at: str


class ExecutionHistory(BaseModel):
    executions: List[ExecutionResult]
    total: int


# =============================================================================
# MOCK DATA (substituir por chamadas reais ao sandbox_executor)
# =============================================================================

mock_history: List[Dict] = []


# =============================================================================
# API ENDPOINTS (UI apenas - execucao real em sandbox_routes.py)
# =============================================================================

@sandbox_terminal_router.get("/history")
async def get_execution_history(limit: int = 10):
    """Retorna historico de execucoes (mock)"""
    return {
        "executions": mock_history[-limit:],
        "total": len(mock_history)
    }


@sandbox_terminal_router.delete("/history")
async def clear_history():
    """Limpa historico de execucoes"""
    mock_history.clear()
    return {"success": True, "message": "Historico limpo"}


@sandbox_terminal_router.get("/templates")
async def get_code_templates():
    """Retorna templates de codigo para o editor"""
    return {
        "templates": [
            {
                "id": "hello",
                "name": "Hello World",
                "language": "python",
                "code": 'print("Hello, World!")',
                "inputs": {}
            },
            {
                "id": "process_data",
                "name": "Processar Dados",
                "language": "python",
                "code": '''def process(data):
    """Processa os dados de entrada"""
    result = data.get("value", 0) * 2
    return {"resultado": result}

# Executar
output = process(inputs)
print(f"Resultado: {output}")''',
                "inputs": {"value": 42}
            },
            {
                "id": "api_call",
                "name": "Chamada API",
                "language": "python",
                "code": '''import json

def fetch_data(url):
    """Simula chamada API"""
    # Em sandbox real, requests estaria disponivel
    return {"status": "ok", "data": [1, 2, 3]}

result = fetch_data(inputs.get("url", ""))
print(json.dumps(result, indent=2))''',
                "inputs": {"url": "https://api.example.com/data"}
            },
            {
                "id": "data_transform",
                "name": "Transformar Lista",
                "language": "python",
                "code": '''# Transformar lista de dados
data = inputs.get("items", [])

# Aplicar transformacao
result = [item.upper() if isinstance(item, str) else item * 2 for item in data]

print(f"Original: {data}")
print(f"Transformado: {result}")''',
                "inputs": {"items": ["hello", "world", 10, 20]}
            }
        ]
    }


@sandbox_terminal_router.get("/config")
async def get_sandbox_config():
    """Retorna configuracoes do sandbox"""
    return {
        "limits": {
            "timeout_seconds": 30,
            "memory_mb": 256,
            "cpu_cores": 0.5,
            "network": "localhost_only"
        },
        "languages": [
            {"id": "python", "name": "Python 3.11", "extension": ".py"},
            {"id": "javascript", "name": "Node.js 20", "extension": ".js"}
        ],
        "features": {
            "syntax_highlighting": True,
            "auto_complete": False,
            "live_output": True,
            "file_upload": False
        }
    }


# =============================================================================
# HTML TEMPLATE
# =============================================================================

SANDBOX_TERMINAL_HTML = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Sandbox de Teste - Fabrica de Agentes</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/codemirror.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/theme/dracula.min.css">
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', -apple-system, sans-serif;
            background: #111827;
            color: #F9FAFB;
            min-height: 100vh;
        }

        /* Header */
        .header {
            background: linear-gradient(135deg, #003B4A 0%, #004d5e 100%);
            padding: 20px 24px;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .header-left {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .header-icon {
            width: 40px;
            height: 40px;
            background: rgba(255, 255, 255, 0.15);
            border-radius: 10px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 20px;
        }

        .header h1 {
            font-size: 20px;
            font-weight: 600;
        }

        .header-subtitle {
            font-size: 13px;
            opacity: 0.8;
        }

        .run-btn {
            display: flex;
            align-items: center;
            gap: 8px;
            background: #FF6C00;
            color: white;
            border: none;
            padding: 12px 24px;
            border-radius: 10px;
            font-size: 15px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.2s;
        }

        .run-btn:hover {
            background: #FF8533;
            transform: translateY(-1px);
        }

        .run-btn:disabled {
            opacity: 0.6;
            cursor: not-allowed;
        }

        .run-btn.running {
            background: #6B7280;
        }

        /* Main Layout */
        .main {
            display: grid;
            grid-template-columns: 1fr 350px;
            grid-template-rows: 1fr 300px;
            gap: 1px;
            background: #374151;
            height: calc(100vh - 80px);
        }

        .panel {
            background: #1F2937;
            display: flex;
            flex-direction: column;
        }

        .panel-header {
            padding: 12px 16px;
            background: #111827;
            border-bottom: 1px solid #374151;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .panel-title {
            font-size: 13px;
            font-weight: 600;
            color: #9CA3AF;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .panel-actions {
            display: flex;
            gap: 8px;
        }

        .panel-btn {
            background: #374151;
            border: none;
            color: #9CA3AF;
            padding: 6px 12px;
            border-radius: 6px;
            font-size: 12px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .panel-btn:hover {
            background: #4B5563;
            color: #F9FAFB;
        }

        .panel-content {
            flex: 1;
            overflow: auto;
        }

        /* Code Editor */
        .editor-panel {
            grid-row: span 2;
        }

        .CodeMirror {
            height: 100%;
            font-family: 'JetBrains Mono', monospace;
            font-size: 14px;
        }

        /* Inputs Panel */
        .inputs-textarea {
            width: 100%;
            height: 100%;
            background: transparent;
            border: none;
            color: #F9FAFB;
            font-family: 'JetBrains Mono', monospace;
            font-size: 13px;
            padding: 16px;
            resize: none;
            outline: none;
        }

        .inputs-textarea::placeholder {
            color: #6B7280;
        }

        /* Console Output */
        .console-panel {
            grid-column: span 2;
        }

        .console-output {
            padding: 16px;
            font-family: 'JetBrains Mono', monospace;
            font-size: 13px;
            line-height: 1.6;
            white-space: pre-wrap;
            color: #D1D5DB;
        }

        .console-line {
            padding: 2px 0;
        }

        .console-line.info {
            color: #60A5FA;
        }

        .console-line.success {
            color: #34D399;
        }

        .console-line.error {
            color: #F87171;
        }

        .console-line.warning {
            color: #FBBF24;
        }

        .console-prompt {
            color: #9CA3AF;
        }

        /* Status Bar */
        .status-bar {
            background: #111827;
            padding: 8px 16px;
            display: flex;
            align-items: center;
            justify-content: space-between;
            font-size: 12px;
            color: #6B7280;
            border-top: 1px solid #374151;
        }

        .status-item {
            display: flex;
            align-items: center;
            gap: 6px;
        }

        .status-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            background: #10B981;
        }

        .status-dot.error {
            background: #EF4444;
        }

        .status-dot.running {
            background: #F59E0B;
            animation: pulse 1s infinite;
        }

        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }

        /* Templates Dropdown */
        .templates-dropdown {
            position: relative;
        }

        .templates-menu {
            position: absolute;
            top: 100%;
            left: 0;
            background: #1F2937;
            border: 1px solid #374151;
            border-radius: 8px;
            padding: 8px;
            min-width: 200px;
            z-index: 100;
            display: none;
        }

        .templates-menu.open {
            display: block;
        }

        .template-item {
            padding: 10px 12px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 13px;
            transition: all 0.2s;
        }

        .template-item:hover {
            background: #374151;
        }

        /* Responsive */
        @media (max-width: 900px) {
            .main {
                grid-template-columns: 1fr;
                grid-template-rows: 1fr 200px 250px;
            }

            .editor-panel {
                grid-row: span 1;
            }

            .console-panel {
                grid-column: span 1;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-left">
            <div class="header-icon">&#129514;</div>
            <div>
                <h1>Sandbox de Teste</h1>
                <div class="header-subtitle">Execute codigo em ambiente isolado</div>
            </div>
        </div>
        <div style="display: flex; gap: 12px; align-items: center;">
            <div class="templates-dropdown">
                <button class="panel-btn" onclick="toggleTemplates()">
                    Templates &#9662;
                </button>
                <div class="templates-menu" id="templatesMenu">
                    <div class="template-item" onclick="loadTemplate('hello')">Hello World</div>
                    <div class="template-item" onclick="loadTemplate('process_data')">Processar Dados</div>
                    <div class="template-item" onclick="loadTemplate('api_call')">Chamada API</div>
                    <div class="template-item" onclick="loadTemplate('data_transform')">Transformar Lista</div>
                </div>
            </div>
            <button class="run-btn" id="runBtn" onclick="executeCode()">
                <span>&#9654;</span> Executar
            </button>
        </div>
    </div>

    <div class="main">
        <!-- Editor Panel -->
        <div class="panel editor-panel">
            <div class="panel-header">
                <span class="panel-title">Codigo</span>
                <div class="panel-actions">
                    <button class="panel-btn" onclick="formatCode()">Formatar</button>
                    <button class="panel-btn" onclick="clearCode()">Limpar</button>
                </div>
            </div>
            <div class="panel-content">
                <textarea id="codeEditor"># Digite seu codigo Python aqui
def hello():
    print("Hello, Sandbox!")

hello()</textarea>
            </div>
        </div>

        <!-- Inputs Panel -->
        <div class="panel">
            <div class="panel-header">
                <span class="panel-title">Inputs (JSON)</span>
                <button class="panel-btn" onclick="formatInputs()">Formatar</button>
            </div>
            <div class="panel-content">
                <textarea
                    class="inputs-textarea"
                    id="inputsEditor"
                    placeholder='{"key": "value"}'
>{}</textarea>
            </div>
        </div>

        <!-- Console Panel -->
        <div class="panel console-panel">
            <div class="panel-header">
                <span class="panel-title">Console</span>
                <div class="panel-actions">
                    <span id="executionTime" style="color: #6B7280; margin-right: 12px;"></span>
                    <button class="panel-btn" onclick="clearConsole()">Limpar</button>
                </div>
            </div>
            <div class="panel-content">
                <div class="console-output" id="consoleOutput">
                    <div class="console-line info">&#9432; Pronto para executar</div>
                    <div class="console-line">Clique em "Executar" ou pressione Ctrl+Enter</div>
                </div>
            </div>
        </div>
    </div>

    <div class="status-bar">
        <div class="status-item">
            <div class="status-dot" id="statusDot"></div>
            <span id="statusText">Pronto</span>
        </div>
        <div style="display: flex; gap: 24px;">
            <div class="status-item">
                <span>Timeout: 30s</span>
            </div>
            <div class="status-item">
                <span>Memoria: 256MB</span>
            </div>
            <div class="status-item">
                <span>Python 3.11</span>
            </div>
        </div>
    </div>

    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/codemirror.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.2/mode/python/python.min.js"></script>
    <script>
        // Templates cache
        let templates = {};

        // Initialize CodeMirror
        const codeEditor = CodeMirror.fromTextArea(document.getElementById('codeEditor'), {
            mode: 'python',
            theme: 'dracula',
            lineNumbers: true,
            indentUnit: 4,
            tabSize: 4,
            indentWithTabs: false,
            lineWrapping: true,
            autoCloseBrackets: true,
        });

        // Load templates on init
        fetch('/api/sandbox-ui/templates')
            .then(res => res.json())
            .then(data => {
                templates = {};
                data.templates.forEach(t => {
                    templates[t.id] = t;
                });
            });

        function toggleTemplates() {
            const menu = document.getElementById('templatesMenu');
            menu.classList.toggle('open');
        }

        function loadTemplate(id) {
            const template = templates[id];
            if (template) {
                codeEditor.setValue(template.code);
                document.getElementById('inputsEditor').value = JSON.stringify(template.inputs, null, 2);
            }
            document.getElementById('templatesMenu').classList.remove('open');
        }

        function clearCode() {
            codeEditor.setValue('');
        }

        function formatCode() {
            // Basic Python formatting (indent fix)
            const code = codeEditor.getValue();
            codeEditor.setValue(code);
        }

        function formatInputs() {
            try {
                const input = document.getElementById('inputsEditor').value;
                const parsed = JSON.parse(input);
                document.getElementById('inputsEditor').value = JSON.stringify(parsed, null, 2);
            } catch (e) {
                addConsoleLine('Erro ao formatar JSON: ' + e.message, 'error');
            }
        }

        function clearConsole() {
            document.getElementById('consoleOutput').innerHTML =
                '<div class="console-line info">&#9432; Console limpo</div>';
            document.getElementById('executionTime').textContent = '';
        }

        function addConsoleLine(text, type = '') {
            const output = document.getElementById('consoleOutput');
            const line = document.createElement('div');
            line.className = 'console-line ' + type;
            line.textContent = text;
            output.appendChild(line);
            output.scrollTop = output.scrollHeight;
        }

        function setStatus(status, text) {
            const dot = document.getElementById('statusDot');
            const statusText = document.getElementById('statusText');

            dot.className = 'status-dot ' + status;
            statusText.textContent = text;
        }

        async function executeCode() {
            const btn = document.getElementById('runBtn');
            const code = codeEditor.getValue();
            let inputs = {};

            try {
                inputs = JSON.parse(document.getElementById('inputsEditor').value || '{}');
            } catch (e) {
                addConsoleLine('Erro: JSON de inputs invalido', 'error');
                return;
            }

            // Clear and show running state
            document.getElementById('consoleOutput').innerHTML = '';
            addConsoleLine('> Iniciando execucao...', 'info');
            btn.disabled = true;
            btn.classList.add('running');
            btn.innerHTML = '<span>&#9632;</span> Executando...';
            setStatus('running', 'Executando...');

            const startTime = Date.now();

            try {
                // Chamar API real de execucao (implementar em sandbox_routes.py)
                const response = await fetch('/api/sandbox/execute', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ code, inputs, language: 'python', timeout: 30 })
                });

                const result = await response.json();
                const duration = Date.now() - startTime;

                if (result.error) {
                    addConsoleLine('Erro: ' + result.error, 'error');
                    setStatus('error', 'Erro');
                } else {
                    if (result.output) {
                        result.output.split('\\n').forEach(line => {
                            addConsoleLine(line);
                        });
                    }
                    addConsoleLine('', '');
                    addConsoleLine('&#10003; Execucao concluida', 'success');
                    setStatus('', 'Concluido');
                }

                document.getElementById('executionTime').textContent =
                    `Tempo: ${result.duration_ms || duration}ms | Memoria: ${result.memory_mb || '?'}MB`;

            } catch (error) {
                addConsoleLine('Erro de conexao: ' + error.message, 'error');
                addConsoleLine('', '');
                addConsoleLine('Nota: O endpoint /api/sandbox/execute precisa ser implementado', 'warning');
                addConsoleLine('Ver: factory/api/sandbox_routes.py', 'info');
                setStatus('error', 'Erro');

                document.getElementById('executionTime').textContent =
                    `Tempo: ${Date.now() - startTime}ms`;
            }

            btn.disabled = false;
            btn.classList.remove('running');
            btn.innerHTML = '<span>&#9654;</span> Executar';
        }

        // Keyboard shortcut
        document.addEventListener('keydown', (e) => {
            if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
                executeCode();
            }
        });

        // Close templates menu on outside click
        document.addEventListener('click', (e) => {
            if (!e.target.closest('.templates-dropdown')) {
                document.getElementById('templatesMenu').classList.remove('open');
            }
        });
    </script>
</body>
</html>'''


@sandbox_terminal_router.get("/", response_class=HTMLResponse)
async def get_sandbox_terminal():
    """Retorna a pagina do terminal sandbox"""
    return HTMLResponse(content=SANDBOX_TERMINAL_HTML)


# =============================================================================
# REGISTRATION
# =============================================================================

def register_sandbox_terminal(app):
    """Registra endpoints do sandbox terminal no app FastAPI"""
    app.include_router(sandbox_terminal_router)

    @app.get("/sandbox", response_class=HTMLResponse)
    async def sandbox_page():
        return HTMLResponse(content=SANDBOX_TERMINAL_HTML)

    print("[Dashboard] Sandbox Terminal UI registered: /sandbox")
