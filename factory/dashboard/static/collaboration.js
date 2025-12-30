/**
 * Colaboracao em Tempo Real - Issue #242
 * =======================================
 *
 * Implementa colaboracao estilo Figma/Google Docs:
 * - Indicadores de presenca (avatares no header)
 * - Cursores em tempo real
 * - Viewing indicators (quem esta vendo cada story)
 * - Locks otimistas (quem esta editando)
 *
 * Autor: Terminal D - Features Agile e AI
 */

class CollaborationManager {
    constructor() {
        this.ws = null;
        this.projectId = null;
        this.userId = null;
        this.username = null;
        this.avatarUrl = null;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectDelay = 1000;

        // Estado
        this.users = new Map();
        this.locks = new Map();
        this.cursors = new Map();
        this.currentViewing = null;
        this.currentEditing = null;

        // Callbacks
        this.onUsersChange = null;
        this.onCursorMove = null;
        this.onLockChange = null;
        this.onViewerChange = null;
        this.onEditConflict = null;

        // Throttle cursor updates
        this.lastCursorUpdate = 0;
        this.cursorThrottleMs = 50;

        // DOM elements
        this.cursorContainer = null;
        this.presenceContainer = null;

        // Bind event handlers
        this._handleMouseMove = this._handleMouseMove.bind(this);
    }

    /**
     * Inicializa a colaboracao
     */
    init(config) {
        this.projectId = config.projectId;
        this.userId = config.userId || `user_${Date.now()}`;
        this.username = config.username || 'Usuario';
        this.avatarUrl = config.avatarUrl || null;

        // Criar containers
        this._createCursorContainer();
        this._createPresenceContainer();

        // Conectar WebSocket
        this.connect();

        // Event listeners
        document.addEventListener('mousemove', this._handleMouseMove);

        console.log('[Collaboration] Inicializado para projeto:', this.projectId);
    }

    /**
     * Conecta ao WebSocket de colaboracao
     */
    connect() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const host = window.location.host;
        const params = new URLSearchParams({
            user_id: this.userId,
            username: this.username
        });
        if (this.avatarUrl) {
            params.append('avatar_url', this.avatarUrl);
        }

        const url = `${protocol}//${host}/ws/collaboration/${this.projectId}?${params}`;

        try {
            this.ws = new WebSocket(url);

            this.ws.onopen = () => {
                console.log('[Collaboration] Conectado');
                this.reconnectAttempts = 0;
                this._updateConnectionStatus(true);
            };

            this.ws.onmessage = (event) => {
                this._handleMessage(JSON.parse(event.data));
            };

            this.ws.onclose = () => {
                console.log('[Collaboration] Desconectado');
                this._updateConnectionStatus(false);
                this._scheduleReconnect();
            };

            this.ws.onerror = (error) => {
                console.error('[Collaboration] Erro WebSocket:', error);
            };

        } catch (error) {
            console.error('[Collaboration] Erro ao conectar:', error);
            this._scheduleReconnect();
        }
    }

    /**
     * Desconecta do WebSocket
     */
    disconnect() {
        document.removeEventListener('mousemove', this._handleMouseMove);

        if (this.ws) {
            this.ws.close();
            this.ws = null;
        }

        this._removeCursorContainer();
        this._removePresenceContainer();
    }

    /**
     * Tenta reconectar apos desconexao
     */
    _scheduleReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            console.warn('[Collaboration] Max tentativas de reconexao atingido');
            return;
        }

        this.reconnectAttempts++;
        const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);

        console.log(`[Collaboration] Reconectando em ${delay}ms (tentativa ${this.reconnectAttempts})`);

        setTimeout(() => this.connect(), delay);
    }

    /**
     * Processa mensagem do WebSocket
     */
    _handleMessage(msg) {
        switch (msg.type) {
            case 'sync':
                this._handleSync(msg);
                break;
            case 'join':
                this._handleJoin(msg);
                break;
            case 'leave':
                this._handleLeave(msg);
                break;
            case 'cursor_move':
                this._handleCursorMove(msg);
                break;
            case 'view_start':
            case 'view_end':
                this._handleViewChange(msg);
                break;
            case 'edit_start':
            case 'edit_end':
                this._handleEditChange(msg);
                break;
            case 'edit_conflict':
                this._handleEditConflict(msg);
                break;
            case 'status_change':
                this._handleStatusChange(msg);
                break;
            case 'ping':
                this._sendPong();
                break;
        }
    }

    /**
     * Sincroniza estado inicial
     */
    _handleSync(msg) {
        // Limpar estado atual
        this.users.clear();
        this.locks.clear();

        // Carregar usuarios
        msg.users.forEach(user => {
            this.users.set(user.user_id, user);
        });

        // Carregar locks
        msg.locks.forEach(lock => {
            const key = `${lock.entity_type}:${lock.entity_id}`;
            this.locks.set(key, lock);
        });

        this._renderPresence();
        this._renderLocks();

        if (this.onUsersChange) {
            this.onUsersChange(Array.from(this.users.values()));
        }
    }

    /**
     * Usuario entrou
     */
    _handleJoin(msg) {
        this.users.set(msg.user.user_id, msg.user);
        this._renderPresence();
        this._showNotification(`${msg.user.username} entrou`, 'join');

        if (this.onUsersChange) {
            this.onUsersChange(Array.from(this.users.values()));
        }
    }

    /**
     * Usuario saiu
     */
    _handleLeave(msg) {
        const user = this.users.get(msg.user.user_id);
        if (user) {
            this.users.delete(msg.user.user_id);
            this.cursors.delete(msg.user.user_id);
            this._removeCursor(msg.user.user_id);
            this._renderPresence();
            this._showNotification(`${user.username} saiu`, 'leave');

            if (this.onUsersChange) {
                this.onUsersChange(Array.from(this.users.values()));
            }
        }
    }

    /**
     * Cursor moveu
     */
    _handleCursorMove(msg) {
        if (msg.user_id === this.userId) return;

        const user = this.users.get(msg.user_id);
        if (!user) return;

        this.cursors.set(msg.user_id, {
            ...msg.cursor,
            user: user
        });

        this._renderCursor(msg.user_id, msg.cursor, user);

        if (this.onCursorMove) {
            this.onCursorMove(msg.user_id, msg.cursor);
        }
    }

    /**
     * Viewing mudou
     */
    _handleViewChange(msg) {
        const user = this.users.get(msg.user_id);
        if (user) {
            if (msg.type === 'view_start') {
                user.viewing = `${msg.entity_type}:${msg.entity_id}`;
            } else {
                user.viewing = null;
            }
            this.users.set(msg.user_id, user);
            this._updateViewingIndicators();

            if (this.onViewerChange) {
                this.onViewerChange(msg.entity_type, msg.entity_id, this.getViewers(msg.entity_type, msg.entity_id));
            }
        }
    }

    /**
     * Editing mudou
     */
    _handleEditChange(msg) {
        const key = `${msg.entity_type || msg.lock?.entity_type}:${msg.entity_id || msg.lock?.entity_id}`;

        if (msg.type === 'edit_start' && msg.lock) {
            this.locks.set(key, msg.lock);
        } else if (msg.type === 'edit_end') {
            this.locks.delete(key);
        }

        this._renderLocks();

        if (this.onLockChange) {
            this.onLockChange(key, this.locks.get(key));
        }
    }

    /**
     * Conflito de edicao
     */
    _handleEditConflict(msg) {
        this._showNotification(`${msg.message}`, 'conflict');

        if (this.onEditConflict) {
            this.onEditConflict(msg.lock);
        }
    }

    /**
     * Status do usuario mudou
     */
    _handleStatusChange(msg) {
        const user = this.users.get(msg.user_id);
        if (user) {
            user.status = msg.status;
            this.users.set(msg.user_id, user);
            this._renderPresence();
        }
    }

    /**
     * Envia movimento do cursor
     */
    _handleMouseMove(event) {
        const now = Date.now();
        if (now - this.lastCursorUpdate < this.cursorThrottleMs) return;
        this.lastCursorUpdate = now;

        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

        // Identificar elemento alvo
        let elementId = null;
        const target = event.target;
        if (target.closest('.story-card')) {
            elementId = target.closest('.story-card').dataset.storyId;
        } else if (target.closest('.task-item')) {
            elementId = target.closest('.task-item').dataset.taskId;
        }

        this.ws.send(JSON.stringify({
            type: 'cursor_move',
            x: event.clientX,
            y: event.clientY,
            element_id: elementId
        }));
    }

    /**
     * Comecar a visualizar uma entidade
     */
    startViewing(entityType, entityId) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

        this.currentViewing = `${entityType}:${entityId}`;

        this.ws.send(JSON.stringify({
            type: 'view_start',
            entity_type: entityType,
            entity_id: entityId
        }));
    }

    /**
     * Parar de visualizar
     */
    stopViewing() {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

        this.currentViewing = null;

        this.ws.send(JSON.stringify({
            type: 'view_end'
        }));
    }

    /**
     * Comecar a editar (adquirir lock)
     */
    startEditing(entityType, entityId) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

        this.currentEditing = `${entityType}:${entityId}`;

        this.ws.send(JSON.stringify({
            type: 'edit_start',
            entity_type: entityType,
            entity_id: entityId
        }));
    }

    /**
     * Parar de editar (liberar lock)
     */
    stopEditing(entityType, entityId) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

        this.currentEditing = null;

        this.ws.send(JSON.stringify({
            type: 'edit_end',
            entity_type: entityType,
            entity_id: entityId
        }));
    }

    /**
     * Verifica se entidade esta bloqueada
     */
    isLocked(entityType, entityId) {
        const key = `${entityType}:${entityId}`;
        const lock = this.locks.get(key);
        return lock && lock.user_id !== this.userId;
    }

    /**
     * Retorna lock de uma entidade
     */
    getLock(entityType, entityId) {
        const key = `${entityType}:${entityId}`;
        return this.locks.get(key);
    }

    /**
     * Retorna viewers de uma entidade
     */
    getViewers(entityType, entityId) {
        const viewingKey = `${entityType}:${entityId}`;
        return Array.from(this.users.values())
            .filter(u => u.viewing === viewingKey && u.user_id !== this.userId);
    }

    /**
     * Envia pong em resposta ao ping
     */
    _sendPong() {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send('ping');
        }
    }

    // ==========================================================================
    // RENDERING
    // ==========================================================================

    /**
     * Cria container para cursores
     */
    _createCursorContainer() {
        if (this.cursorContainer) return;

        this.cursorContainer = document.createElement('div');
        this.cursorContainer.id = 'collab-cursors';
        this.cursorContainer.style.cssText = `
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            pointer-events: none;
            z-index: 9999;
        `;
        document.body.appendChild(this.cursorContainer);
    }

    /**
     * Remove container de cursores
     */
    _removeCursorContainer() {
        if (this.cursorContainer) {
            this.cursorContainer.remove();
            this.cursorContainer = null;
        }
    }

    /**
     * Renderiza cursor de um usuario
     */
    _renderCursor(userId, cursor, user) {
        let cursorEl = document.getElementById(`cursor-${userId}`);

        if (!cursorEl) {
            cursorEl = document.createElement('div');
            cursorEl.id = `cursor-${userId}`;
            cursorEl.innerHTML = `
                <svg width="20" height="20" viewBox="0 0 24 24" fill="${user.color}" style="filter: drop-shadow(0 1px 2px rgba(0,0,0,0.3));">
                    <path d="M5.5 2l12 12-5 1-3.5 6L5.5 2z"/>
                </svg>
                <span style="
                    position: absolute;
                    left: 16px;
                    top: 16px;
                    background: ${user.color};
                    color: white;
                    padding: 2px 6px;
                    border-radius: 4px;
                    font-size: 11px;
                    white-space: nowrap;
                    box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                ">${user.username}</span>
            `;
            cursorEl.style.cssText = `
                position: fixed;
                pointer-events: none;
                transition: transform 0.1s ease-out;
                z-index: 10000;
            `;
            this.cursorContainer.appendChild(cursorEl);
        }

        cursorEl.style.transform = `translate(${cursor.x}px, ${cursor.y}px)`;
    }

    /**
     * Remove cursor de um usuario
     */
    _removeCursor(userId) {
        const cursorEl = document.getElementById(`cursor-${userId}`);
        if (cursorEl) {
            cursorEl.remove();
        }
    }

    /**
     * Cria container de presenca no header
     */
    _createPresenceContainer() {
        if (this.presenceContainer) return;

        // Procurar header
        const header = document.querySelector('header') || document.querySelector('.header') || document.querySelector('nav');
        if (!header) return;

        this.presenceContainer = document.createElement('div');
        this.presenceContainer.id = 'collab-presence';
        this.presenceContainer.style.cssText = `
            display: flex;
            align-items: center;
            gap: 4px;
            margin-left: 16px;
        `;

        // Adicionar antes de outros elementos do header
        const firstChild = header.querySelector('.header-actions') || header.lastChild;
        if (firstChild) {
            header.insertBefore(this.presenceContainer, firstChild);
        } else {
            header.appendChild(this.presenceContainer);
        }
    }

    /**
     * Remove container de presenca
     */
    _removePresenceContainer() {
        if (this.presenceContainer) {
            this.presenceContainer.remove();
            this.presenceContainer = null;
        }
    }

    /**
     * Renderiza avatares de usuarios online
     */
    _renderPresence() {
        if (!this.presenceContainer) return;

        const users = Array.from(this.users.values())
            .filter(u => u.user_id !== this.userId);

        if (users.length === 0) {
            this.presenceContainer.innerHTML = '';
            return;
        }

        const maxVisible = 5;
        const visible = users.slice(0, maxVisible);
        const hidden = users.length - maxVisible;

        let html = visible.map(user => `
            <div class="collab-avatar"
                 title="${user.username} (${user.status})"
                 style="
                    width: 32px;
                    height: 32px;
                    border-radius: 50%;
                    border: 2px solid ${user.color};
                    background: ${user.avatar_url ? `url(${user.avatar_url})` : user.color};
                    background-size: cover;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    color: white;
                    font-size: 12px;
                    font-weight: bold;
                    position: relative;
                    cursor: pointer;
                 ">
                ${!user.avatar_url ? user.username.charAt(0).toUpperCase() : ''}
                <span style="
                    position: absolute;
                    bottom: -2px;
                    right: -2px;
                    width: 10px;
                    height: 10px;
                    border-radius: 50%;
                    border: 2px solid white;
                    background: ${user.status === 'online' ? '#22C55E' : user.status === 'away' ? '#F59E0B' : '#6B7280'};
                "></span>
            </div>
        `).join('');

        if (hidden > 0) {
            html += `
                <div class="collab-avatar-more" style="
                    width: 32px;
                    height: 32px;
                    border-radius: 50%;
                    background: #6B7280;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    color: white;
                    font-size: 11px;
                    font-weight: bold;
                ">+${hidden}</div>
            `;
        }

        this.presenceContainer.innerHTML = html;
    }

    /**
     * Atualiza indicadores de viewing nas stories
     */
    _updateViewingIndicators() {
        // Remover indicadores existentes
        document.querySelectorAll('.collab-viewing-indicator').forEach(el => el.remove());

        // Agrupar usuarios por entidade
        const viewersByEntity = new Map();
        this.users.forEach(user => {
            if (user.viewing && user.user_id !== this.userId) {
                if (!viewersByEntity.has(user.viewing)) {
                    viewersByEntity.set(user.viewing, []);
                }
                viewersByEntity.get(user.viewing).push(user);
            }
        });

        // Adicionar indicadores
        viewersByEntity.forEach((viewers, entityKey) => {
            const [entityType, entityId] = entityKey.split(':');
            const selector = entityType === 'story'
                ? `.story-card[data-story-id="${entityId}"]`
                : `.task-item[data-task-id="${entityId}"]`;

            const element = document.querySelector(selector);
            if (!element) return;

            const indicator = document.createElement('div');
            indicator.className = 'collab-viewing-indicator';
            indicator.style.cssText = `
                position: absolute;
                top: -8px;
                right: -8px;
                display: flex;
                gap: 2px;
                z-index: 10;
            `;

            indicator.innerHTML = viewers.slice(0, 3).map(v => `
                <div title="${v.username} esta visualizando" style="
                    width: 24px;
                    height: 24px;
                    border-radius: 50%;
                    border: 2px solid white;
                    background: ${v.color};
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    color: white;
                    font-size: 10px;
                    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
                ">${v.username.charAt(0).toUpperCase()}</div>
            `).join('');

            if (viewers.length > 3) {
                indicator.innerHTML += `
                    <div style="
                        width: 24px;
                        height: 24px;
                        border-radius: 50%;
                        background: #6B7280;
                        display: flex;
                        align-items: center;
                        justify-content: center;
                        color: white;
                        font-size: 10px;
                    ">+${viewers.length - 3}</div>
                `;
            }

            element.style.position = 'relative';
            element.appendChild(indicator);
        });
    }

    /**
     * Renderiza indicadores de lock
     */
    _renderLocks() {
        // Remover indicadores existentes
        document.querySelectorAll('.collab-lock-indicator').forEach(el => el.remove());

        // Adicionar indicadores de lock
        this.locks.forEach((lock, key) => {
            if (lock.user_id === this.userId) return;

            const [entityType, entityId] = key.split(':');
            const selector = entityType === 'story'
                ? `.story-card[data-story-id="${entityId}"]`
                : `.task-item[data-task-id="${entityId}"]`;

            const element = document.querySelector(selector);
            if (!element) return;

            const user = this.users.get(lock.user_id);
            const color = user?.color || '#EF4444';

            const indicator = document.createElement('div');
            indicator.className = 'collab-lock-indicator';
            indicator.title = `${lock.username} esta editando`;
            indicator.style.cssText = `
                position: absolute;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                border: 3px solid ${color};
                border-radius: inherit;
                pointer-events: none;
                z-index: 5;
                animation: pulse-border 2s infinite;
            `;

            // Adicionar badge de lock
            const badge = document.createElement('div');
            badge.style.cssText = `
                position: absolute;
                top: -12px;
                left: 50%;
                transform: translateX(-50%);
                background: ${color};
                color: white;
                padding: 2px 8px;
                border-radius: 12px;
                font-size: 11px;
                white-space: nowrap;
                box-shadow: 0 2px 4px rgba(0,0,0,0.2);
                display: flex;
                align-items: center;
                gap: 4px;
            `;
            badge.innerHTML = `
                <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
                    <path d="M18 8h-1V6c0-2.76-2.24-5-5-5S7 3.24 7 6v2H6c-1.1 0-2 .9-2 2v10c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V10c0-1.1-.9-2-2-2zm-6 9c-1.1 0-2-.9-2-2s.9-2 2-2 2 .9 2 2-.9 2-2 2zm3.1-9H8.9V6c0-1.71 1.39-3.1 3.1-3.1 1.71 0 3.1 1.39 3.1 3.1v2z"/>
                </svg>
                ${lock.username}
            `;
            indicator.appendChild(badge);

            element.style.position = 'relative';
            element.appendChild(indicator);
        });

        // Adicionar CSS de animacao se nao existir
        if (!document.getElementById('collab-lock-styles')) {
            const style = document.createElement('style');
            style.id = 'collab-lock-styles';
            style.textContent = `
                @keyframes pulse-border {
                    0%, 100% { opacity: 1; }
                    50% { opacity: 0.5; }
                }
            `;
            document.head.appendChild(style);
        }
    }

    /**
     * Atualiza status de conexao na UI
     */
    _updateConnectionStatus(connected) {
        // Emitir evento customizado
        window.dispatchEvent(new CustomEvent('collaboration-status', {
            detail: { connected }
        }));
    }

    /**
     * Mostra notificacao de colaboracao
     */
    _showNotification(message, type) {
        // Usar sistema de toast existente se disponivel
        if (window.Alpine && typeof addToast === 'function') {
            addToast(message, type === 'conflict' ? 'warning' : 'info');
        } else {
            console.log(`[Collaboration] ${message}`);
        }
    }
}

// Singleton global
window.collaborationManager = new CollaborationManager();

// Auto-init quando o DOM estiver pronto e houver projeto selecionado
document.addEventListener('DOMContentLoaded', () => {
    // Verificar se ha projeto selecionado no Alpine store
    const checkAndInit = () => {
        if (window.Alpine) {
            const appData = Alpine.store('app') || {};
            if (appData.currentProject?.project_id) {
                window.collaborationManager.init({
                    projectId: appData.currentProject.project_id,
                    userId: appData.currentUser?.user_id || localStorage.getItem('user_id'),
                    username: appData.currentUser?.username || localStorage.getItem('username') || 'Usuario'
                });
            }
        }
    };

    // Tentar inicializar apos Alpine estar pronto
    setTimeout(checkAndInit, 1000);

    // Observar mudancas de projeto
    window.addEventListener('project-changed', (e) => {
        if (window.collaborationManager.ws) {
            window.collaborationManager.disconnect();
        }
        if (e.detail?.projectId) {
            window.collaborationManager.init({
                projectId: e.detail.projectId,
                userId: e.detail.userId || localStorage.getItem('user_id'),
                username: e.detail.username || localStorage.getItem('username') || 'Usuario'
            });
        }
    });
});

// Exportar para uso em modulos
if (typeof module !== 'undefined' && module.exports) {
    module.exports = CollaborationManager;
}
