/**
 * Offline UI Module - Fabrica de Agentes PWA
 * ============================================
 * Componentes de interface para modo offline:
 * - Indicador de status online/offline
 * - Badge de mudancas pendentes
 * - Botao de sincronizar
 */

// ============================================================================
// OFFLINE UI MANAGER
// ============================================================================

class OfflineUIManager {
  constructor() {
    this.isOnline = navigator.onLine;
    this.pendingChanges = 0;
    this.lastSyncTime = null;
    this.syncInProgress = false;

    this.init();
  }

  init() {
    // Criar elementos UI
    this.createStatusIndicator();
    this.createSyncButton();

    // Event listeners
    window.addEventListener('online', () => this.handleOnline());
    window.addEventListener('offline', () => this.handleOffline());

    // Escutar mensagens do Service Worker
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.addEventListener('message', (event) => {
        this.handleServiceWorkerMessage(event.data);
      });
    }

    // Escutar eventos do SyncManager
    if (window.SyncManager) {
      window.SyncManager.onSync((event, data) => {
        this.handleSyncEvent(event, data);
      });
    }

    // Atualizar contagem de pendencias
    this.updatePendingCount();

    // Verificar status inicial
    this.updateOnlineStatus();
  }

  // ===========================================================================
  // UI ELEMENTS
  // ===========================================================================

  createStatusIndicator() {
    const indicator = document.createElement('div');
    indicator.id = 'offline-status-indicator';
    indicator.className = 'offline-status-indicator';
    indicator.innerHTML = `
      <div class="status-dot"></div>
      <span class="status-text">Online</span>
      <div class="pending-badge" style="display: none;">
        <span class="pending-count">0</span>
      </div>
    `;
    document.body.appendChild(indicator);

    // Estilos inline para o indicador
    const style = document.createElement('style');
    style.textContent = `
      .offline-status-indicator {
        position: fixed;
        bottom: 20px;
        left: 20px;
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 10px 16px;
        background: white;
        border-radius: 24px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        z-index: 10000;
        font-family: 'Inter', sans-serif;
        font-size: 0.875rem;
        transition: all 0.3s ease;
      }

      .offline-status-indicator.offline {
        background: #FEF2F2;
        border: 1px solid #FCA5A5;
      }

      .offline-status-indicator.syncing {
        background: #FEF3C7;
        border: 1px solid #FCD34D;
      }

      .status-dot {
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: #10B981;
        transition: background 0.3s ease;
      }

      .offline-status-indicator.offline .status-dot {
        background: #EF4444;
        animation: pulse-dot 1.5s ease-in-out infinite;
      }

      .offline-status-indicator.syncing .status-dot {
        background: #F59E0B;
        animation: pulse-dot 0.8s ease-in-out infinite;
      }

      @keyframes pulse-dot {
        0%, 100% { opacity: 1; transform: scale(1); }
        50% { opacity: 0.5; transform: scale(1.2); }
      }

      .status-text {
        color: #374151;
        font-weight: 500;
      }

      .pending-badge {
        background: #FF6C00;
        color: white;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 0.75rem;
        font-weight: 600;
        margin-left: 4px;
      }

      .sync-button {
        position: fixed;
        bottom: 20px;
        left: 180px;
        display: none;
        align-items: center;
        gap: 6px;
        padding: 10px 16px;
        background: #003B4A;
        color: white;
        border: none;
        border-radius: 24px;
        font-family: 'Inter', sans-serif;
        font-size: 0.875rem;
        font-weight: 500;
        cursor: pointer;
        box-shadow: 0 4px 12px rgba(0, 59, 74, 0.3);
        z-index: 10000;
        transition: all 0.2s ease;
      }

      .sync-button:hover {
        background: #005A6E;
        transform: translateY(-2px);
      }

      .sync-button:disabled {
        opacity: 0.6;
        cursor: not-allowed;
        transform: none;
      }

      .sync-button.visible {
        display: flex;
      }

      .sync-icon {
        width: 16px;
        height: 16px;
        transition: transform 0.3s ease;
      }

      .sync-button.syncing .sync-icon {
        animation: spin 1s linear infinite;
      }

      @keyframes spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }

      /* Mobile responsiveness */
      @media screen and (max-width: 768px) {
        .offline-status-indicator {
          bottom: 70px;
          left: 16px;
          padding: 8px 12px;
          font-size: 0.75rem;
        }

        .sync-button {
          bottom: 70px;
          left: auto;
          right: 16px;
        }
      }
    `;
    document.head.appendChild(style);

    this.statusIndicator = indicator;
    this.statusDot = indicator.querySelector('.status-dot');
    this.statusText = indicator.querySelector('.status-text');
    this.pendingBadge = indicator.querySelector('.pending-badge');
    this.pendingCount = indicator.querySelector('.pending-count');
  }

  createSyncButton() {
    const button = document.createElement('button');
    button.id = 'sync-button';
    button.className = 'sync-button';
    button.innerHTML = `
      <svg class="sync-icon" fill="currentColor" viewBox="0 0 24 24">
        <path d="M12 4V1L8 5l4 4V6c3.31 0 6 2.69 6 6 0 1.01-.25 1.97-.7 2.8l1.46 1.46C19.54 15.03 20 13.57 20 12c0-4.42-3.58-8-8-8zm0 14c-3.31 0-6-2.69-6-6 0-1.01.25-1.97.7-2.8L5.24 7.74C4.46 8.97 4 10.43 4 12c0 4.42 3.58 8 8 8v3l4-4-4-4v3z"/>
      </svg>
      Sincronizar
    `;
    button.addEventListener('click', () => this.syncNow());
    document.body.appendChild(button);

    this.syncButton = button;
  }

  // ===========================================================================
  // EVENT HANDLERS
  // ===========================================================================

  handleOnline() {
    this.isOnline = true;
    this.updateOnlineStatus();

    // Auto-sync quando voltar online
    if (this.pendingChanges > 0) {
      setTimeout(() => this.syncNow(), 2000);
    }
  }

  handleOffline() {
    this.isOnline = false;
    this.updateOnlineStatus();
  }

  handleServiceWorkerMessage(message) {
    switch (message.type) {
      case 'OFFLINE_DATA':
        this.showToast('Dados carregados do cache', 'info');
        break;

      case 'DATA_UPDATED':
        this.showToast('Dados atualizados', 'success');
        break;

      case 'CACHE_CLEARED':
        this.showToast('Cache limpo', 'info');
        break;
    }
  }

  handleSyncEvent(event, data) {
    switch (event) {
      case 'sync_start':
        this.setSyncing(true);
        break;

      case 'sync_complete':
        this.setSyncing(false);
        this.updatePendingCount();
        if (data.synced > 0) {
          this.showToast(`${data.synced} alteracao(oes) sincronizada(s)`, 'success');
        }
        if (data.errors > 0) {
          this.showToast(`${data.errors} erro(s) na sincronizacao`, 'warning');
        }
        break;

      case 'sync_error':
        this.setSyncing(false);
        this.showToast('Erro na sincronizacao', 'error');
        break;

      case 'action_synced':
        this.updatePendingCount();
        break;
    }
  }

  // ===========================================================================
  // UI UPDATES
  // ===========================================================================

  updateOnlineStatus() {
    if (this.isOnline) {
      this.statusIndicator.classList.remove('offline');
      this.statusText.textContent = 'Online';
    } else {
      this.statusIndicator.classList.add('offline');
      this.statusText.textContent = 'Offline';
    }

    // Mostrar/ocultar botao de sync
    this.updateSyncButtonVisibility();
  }

  async updatePendingCount() {
    if (window.OfflineDB && window.OfflineDB.isReady) {
      try {
        this.pendingChanges = await window.OfflineDB.getPendingCount();
        this.pendingCount.textContent = this.pendingChanges;
        this.pendingBadge.style.display = this.pendingChanges > 0 ? 'block' : 'none';
        this.updateSyncButtonVisibility();
      } catch (error) {
        console.error('[OfflineUI] Error getting pending count:', error);
      }
    }
  }

  updateSyncButtonVisibility() {
    const shouldShow = this.isOnline && this.pendingChanges > 0;
    this.syncButton.classList.toggle('visible', shouldShow);
  }

  setSyncing(syncing) {
    this.syncInProgress = syncing;
    this.statusIndicator.classList.toggle('syncing', syncing);
    this.syncButton.classList.toggle('syncing', syncing);
    this.syncButton.disabled = syncing;

    if (syncing) {
      this.statusText.textContent = 'Sincronizando...';
    } else {
      this.updateOnlineStatus();
    }
  }

  // ===========================================================================
  // ACTIONS
  // ===========================================================================

  async syncNow() {
    if (this.syncInProgress || !this.isOnline) return;

    if (window.SyncManager) {
      this.setSyncing(true);
      const result = await window.SyncManager.syncAll();
      this.setSyncing(false);

      if (result.success) {
        this.lastSyncTime = Date.now();
      }
    } else {
      this.showToast('Gerenciador de sync nao disponivel', 'warning');
    }
  }

  showToast(message, type = 'info') {
    // Usar sistema de toast existente se disponivel
    if (window.app && typeof window.app.showToast === 'function') {
      window.app.showToast(message, type);
      return;
    }

    // Fallback: criar toast simples
    const toast = document.createElement('div');
    toast.className = `offline-toast offline-toast-${type}`;
    toast.textContent = message;
    toast.style.cssText = `
      position: fixed;
      bottom: 80px;
      left: 50%;
      transform: translateX(-50%);
      padding: 12px 24px;
      background: ${type === 'error' ? '#EF4444' : type === 'warning' ? '#F59E0B' : type === 'success' ? '#10B981' : '#3B82F6'};
      color: white;
      border-radius: 8px;
      font-family: 'Inter', sans-serif;
      font-size: 0.875rem;
      z-index: 10001;
      animation: toastIn 0.3s ease;
    `;

    document.body.appendChild(toast);
    setTimeout(() => {
      toast.style.animation = 'toastOut 0.3s ease forwards';
      setTimeout(() => toast.remove(), 300);
    }, 3000);
  }
}

// ============================================================================
// INITIALIZE
// ============================================================================

// Aguardar DOM estar pronto
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    window.OfflineUI = new OfflineUIManager();
  });
} else {
  window.OfflineUI = new OfflineUIManager();
}

// Adicionar animacoes de toast ao CSS
const toastStyles = document.createElement('style');
toastStyles.textContent = `
  @keyframes toastIn {
    from { opacity: 0; transform: translateX(-50%) translateY(20px); }
    to { opacity: 1; transform: translateX(-50%) translateY(0); }
  }
  @keyframes toastOut {
    from { opacity: 1; transform: translateX(-50%) translateY(0); }
    to { opacity: 0; transform: translateX(-50%) translateY(-20px); }
  }
`;
document.head.appendChild(toastStyles);
