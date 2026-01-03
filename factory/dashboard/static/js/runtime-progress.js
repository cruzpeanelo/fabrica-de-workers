/**
 * Runtime Progress - Dashboard Component
 *
 * Exibe barra de progresso e controles de tempo de execucao do orquestrador.
 *
 * Uso:
 *   <div id="runtime-widget"></div>
 *   <script src="/static/js/runtime-progress.js"></script>
 *   <script>
 *     RuntimeProgress.init('#runtime-widget');
 *   </script>
 */

const RuntimeProgress = (function () {
    'use strict';

    // Configuracoes
    const CONFIG = {
        updateInterval: 5000,  // Atualizar a cada 5 segundos
        warningThreshold: 300, // Aviso em 5 minutos restantes
        criticalThreshold: 60, // Critico em 1 minuto
        apiBase: '/api/orchestrator/runtime'
    };

    // Estado
    let state = {
        container: null,
        intervalId: null,
        lastStatus: null
    };

    /**
     * Formata segundos para string legivel.
     */
    function formatDuration(seconds) {
        if (seconds === null || seconds === undefined) {
            return '--:--:--';
        }

        if (seconds <= 0) {
            return '00:00:00';
        }

        const hours = Math.floor(seconds / 3600);
        const minutes = Math.floor((seconds % 3600) / 60);
        const secs = seconds % 60;

        return `${String(hours).padStart(2, '0')}:${String(minutes).padStart(2, '0')}:${String(secs).padStart(2, '0')}`;
    }

    /**
     * Formata duracao para exibicao amigavel.
     */
    function formatDurationFriendly(seconds) {
        if (seconds === null || seconds === undefined) {
            return 'ilimitado';
        }

        const hours = Math.floor(seconds / 3600);
        const minutes = Math.floor((seconds % 3600) / 60);

        if (hours > 0 && minutes > 0) {
            return `${hours}h ${minutes}m`;
        } else if (hours > 0) {
            return `${hours}h`;
        } else if (minutes > 0) {
            return `${minutes}m`;
        } else {
            return `${seconds}s`;
        }
    }

    /**
     * Busca status do runtime via API.
     */
    async function fetchStatus() {
        try {
            const response = await fetch(`${CONFIG.apiBase}/status`);
            const data = await response.json();

            if (data.success) {
                return data.data;
            }
            return null;
        } catch (error) {
            console.error('Erro ao buscar status do runtime:', error);
            return null;
        }
    }

    /**
     * Estende tempo de execucao.
     */
    async function extendRuntime(duration) {
        try {
            const response = await fetch(`${CONFIG.apiBase}/extend`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ duration })
            });
            const data = await response.json();

            if (data.success) {
                updateWidget();
                showNotification('success', `Tempo estendido em ${duration}`);
            } else {
                showNotification('error', data.error || 'Erro ao estender tempo');
            }
        } catch (error) {
            console.error('Erro ao estender runtime:', error);
            showNotification('error', 'Erro de conexao');
        }
    }

    /**
     * Para execucao graciosamente.
     */
    async function stopRuntime() {
        try {
            const response = await fetch(`${CONFIG.apiBase}/stop`, {
                method: 'POST'
            });
            const data = await response.json();

            if (data.success) {
                updateWidget();
                showNotification('success', 'Orquestrador parando...');
            } else {
                showNotification('error', data.error || 'Erro ao parar');
            }
        } catch (error) {
            console.error('Erro ao parar runtime:', error);
            showNotification('error', 'Erro de conexao');
        }
    }

    /**
     * Mostra notificacao.
     */
    function showNotification(type, message) {
        // Usar sistema de notificacao existente se disponivel
        if (window.showToast) {
            window.showToast(message, type);
        } else if (window.notyf) {
            window.notyf[type](message);
        } else {
            console.log(`[${type.toUpperCase()}] ${message}`);
        }
    }

    /**
     * Renderiza widget de runtime.
     */
    function renderWidget(status) {
        if (!state.container) return;

        const isRunning = status && status.status !== 'stopped';
        const isUnlimited = status && status.is_unlimited;
        const progress = status ? (status.progress || 0) : 0;
        const remaining = status ? status.remaining_seconds : null;
        const elapsed = status ? status.elapsed_seconds : 0;

        // Determinar cor baseado no tempo restante
        let progressColor = '#10B981'; // verde
        let statusClass = 'normal';

        if (remaining !== null) {
            if (remaining <= CONFIG.criticalThreshold) {
                progressColor = '#EF4444'; // vermelho
                statusClass = 'critical';
            } else if (remaining <= CONFIG.warningThreshold) {
                progressColor = '#F59E0B'; // amarelo
                statusClass = 'warning';
            }
        }

        // Gerar HTML
        const html = `
            <div class="runtime-widget ${statusClass}" style="
                background: white;
                border-radius: 8px;
                padding: 16px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                margin-bottom: 16px;
            ">
                <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;">
                    <div style="display: flex; align-items: center; gap: 8px;">
                        <span style="
                            width: 8px;
                            height: 8px;
                            border-radius: 50%;
                            background: ${isRunning ? '#10B981' : '#9CA3AF'};
                            animation: ${isRunning ? 'pulse 2s infinite' : 'none'};
                        "></span>
                        <span style="font-weight: 600; color: #1F2937;">
                            ${isRunning ? 'Orquestrador em execucao' : 'Orquestrador parado'}
                        </span>
                    </div>
                    <div style="display: flex; gap: 8px;">
                        ${isRunning && !isUnlimited ? `
                            <button onclick="RuntimeProgress.extend('1h')" style="
                                padding: 4px 12px;
                                border: 1px solid #D1D5DB;
                                border-radius: 4px;
                                background: white;
                                cursor: pointer;
                                font-size: 12px;
                            ">+1h</button>
                            <button onclick="RuntimeProgress.extend('30m')" style="
                                padding: 4px 12px;
                                border: 1px solid #D1D5DB;
                                border-radius: 4px;
                                background: white;
                                cursor: pointer;
                                font-size: 12px;
                            ">+30m</button>
                        ` : ''}
                        ${isRunning ? `
                            <button onclick="RuntimeProgress.stop()" style="
                                padding: 4px 12px;
                                border: 1px solid #EF4444;
                                border-radius: 4px;
                                background: white;
                                color: #EF4444;
                                cursor: pointer;
                                font-size: 12px;
                            ">Parar</button>
                        ` : ''}
                    </div>
                </div>

                ${isRunning && !isUnlimited ? `
                    <div style="margin-bottom: 8px;">
                        <div style="
                            width: 100%;
                            height: 8px;
                            background: #E5E7EB;
                            border-radius: 4px;
                            overflow: hidden;
                        ">
                            <div style="
                                width: ${progress * 100}%;
                                height: 100%;
                                background: ${progressColor};
                                border-radius: 4px;
                                transition: width 0.3s ease;
                            "></div>
                        </div>
                    </div>
                    <div style="display: flex; justify-content: space-between; font-size: 12px; color: #6B7280;">
                        <span>Decorrido: ${formatDuration(elapsed)}</span>
                        <span style="font-weight: 600; color: ${progressColor};">
                            Restante: ${formatDuration(remaining)}
                        </span>
                    </div>
                    ${status.end_time ? `
                        <div style="font-size: 11px; color: #9CA3AF; margin-top: 4px;">
                            Termino previsto: ${new Date(status.end_time).toLocaleString()}
                        </div>
                    ` : ''}
                ` : isRunning ? `
                    <div style="font-size: 12px; color: #6B7280;">
                        <span>Modo ilimitado</span>
                        <span style="margin-left: 16px;">Decorrido: ${formatDuration(elapsed)}</span>
                    </div>
                ` : `
                    <div style="font-size: 12px; color: #9CA3AF;">
                        Inicie o orquestrador para monitorar o tempo de execucao.
                    </div>
                `}

                ${status && status.stats ? `
                    <div style="
                        display: flex;
                        gap: 16px;
                        margin-top: 12px;
                        padding-top: 12px;
                        border-top: 1px solid #E5E7EB;
                        font-size: 11px;
                        color: #6B7280;
                    ">
                        <span>Ciclos: ${status.stats.cycles_completed || 0}</span>
                        <span>Tasks: ${status.stats.tasks_processed || 0}</span>
                        <span>Erros: ${status.stats.errors_count || 0}</span>
                    </div>
                ` : ''}
            </div>

            <style>
                @keyframes pulse {
                    0%, 100% { opacity: 1; }
                    50% { opacity: 0.5; }
                }
                .runtime-widget.warning {
                    border-left: 4px solid #F59E0B;
                }
                .runtime-widget.critical {
                    border-left: 4px solid #EF4444;
                    animation: criticalPulse 1s infinite;
                }
                @keyframes criticalPulse {
                    0%, 100% { background: white; }
                    50% { background: #FEF2F2; }
                }
            </style>
        `;

        state.container.innerHTML = html;
    }

    /**
     * Atualiza widget com dados da API.
     */
    async function updateWidget() {
        const status = await fetchStatus();
        state.lastStatus = status;
        renderWidget(status);

        // Verificar avisos
        if (status && status.remaining_seconds !== null) {
            if (status.remaining_seconds <= CONFIG.criticalThreshold && status.remaining_seconds > 0) {
                showNotification('warning', `Orquestrador encerrando em ${formatDuration(status.remaining_seconds)}`);
            }
        }
    }

    /**
     * Inicializa o widget.
     */
    function init(selector) {
        state.container = document.querySelector(selector);
        if (!state.container) {
            console.error('RuntimeProgress: Container nao encontrado:', selector);
            return;
        }

        // Primeira atualizacao
        updateWidget();

        // Agendar atualizacoes periodicas
        state.intervalId = setInterval(updateWidget, CONFIG.updateInterval);

        console.log('RuntimeProgress inicializado');
    }

    /**
     * Destroi o widget.
     */
    function destroy() {
        if (state.intervalId) {
            clearInterval(state.intervalId);
            state.intervalId = null;
        }
        if (state.container) {
            state.container.innerHTML = '';
        }
    }

    // API publica
    return {
        init,
        destroy,
        updateWidget,
        extend: extendRuntime,
        stop: stopRuntime,
        getStatus: () => state.lastStatus
    };
})();

// Auto-inicializar se elemento existir
document.addEventListener('DOMContentLoaded', function () {
    const widget = document.getElementById('runtime-widget');
    if (widget) {
        RuntimeProgress.init('#runtime-widget');
    }
});
