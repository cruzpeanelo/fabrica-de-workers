/**
 * PWA Initialization - Fabrica de Agentes
 * =========================================
 * Inicializacao da Progressive Web App:
 * - Registro do Service Worker
 * - Inicializacao do IndexedDB
 * - Configuracao de eventos de sincronizacao
 */

// ============================================================================
// SERVICE WORKER REGISTRATION
// ============================================================================

async function registerServiceWorker() {
  if (!('serviceWorker' in navigator)) {
    console.log('[PWA] Service Workers not supported');
    return null;
  }

  try {
    const registration = await navigator.serviceWorker.register('/sw.js', {
      scope: '/'
    });

    console.log('[PWA] Service Worker registered:', registration.scope);

    // Verificar atualizacoes
    registration.addEventListener('updatefound', () => {
      const newWorker = registration.installing;
      console.log('[PWA] New Service Worker installing...');

      newWorker.addEventListener('statechange', () => {
        if (newWorker.state === 'installed' && navigator.serviceWorker.controller) {
          // Nova versao disponivel
          showUpdateNotification();
        }
      });
    });

    return registration;
  } catch (error) {
    console.error('[PWA] Service Worker registration failed:', error);
    return null;
  }
}

function showUpdateNotification() {
  // Criar notificacao de atualizacao
  const notification = document.createElement('div');
  notification.id = 'pwa-update-notification';
  notification.innerHTML = `
    <div style="
      position: fixed;
      top: 20px;
      left: 50%;
      transform: translateX(-50%);
      background: #003B4A;
      color: white;
      padding: 16px 24px;
      border-radius: 8px;
      box-shadow: 0 4px 20px rgba(0, 59, 74, 0.3);
      display: flex;
      align-items: center;
      gap: 16px;
      z-index: 10002;
      font-family: 'Inter', sans-serif;
    ">
      <span>Nova versao disponivel!</span>
      <button onclick="updateApp()" style="
        background: #FF6C00;
        color: white;
        border: none;
        padding: 8px 16px;
        border-radius: 6px;
        cursor: pointer;
        font-weight: 500;
      ">Atualizar</button>
      <button onclick="this.parentElement.remove()" style="
        background: transparent;
        color: white;
        border: none;
        padding: 4px;
        cursor: pointer;
        opacity: 0.7;
      ">x</button>
    </div>
  `;
  document.body.appendChild(notification);
}

async function updateApp() {
  const registration = await navigator.serviceWorker.ready;

  if (registration.waiting) {
    // Enviar mensagem para o novo SW assumir controle
    registration.waiting.postMessage({ type: 'SKIP_WAITING' });
  }

  // Recarregar pagina
  window.location.reload();
}

// ============================================================================
// OFFLINE DATA INITIALIZATION
// ============================================================================

async function initOfflineData() {
  // Aguardar OfflineDB estar pronto
  if (window.OfflineDB && !window.OfflineDB.isReady) {
    await new Promise(resolve => {
      window.OfflineDB.onReady(resolve);
    });
  }

  // Carregar dados do servidor para cache local
  if (navigator.onLine && window.SyncManager) {
    console.log('[PWA] Pulling initial data for offline cache...');
    await window.SyncManager.pullFromServer();
  }
}

// ============================================================================
// INSTALL PROMPT HANDLER
// ============================================================================

let deferredPrompt = null;

window.addEventListener('beforeinstallprompt', (e) => {
  e.preventDefault();
  deferredPrompt = e;
  showInstallButton();
});

function showInstallButton() {
  // Verificar se ja foi instalado
  if (window.matchMedia('(display-mode: standalone)').matches) {
    return;
  }

  // Criar botao de instalacao
  const installBtn = document.createElement('button');
  installBtn.id = 'pwa-install-btn';
  installBtn.innerHTML = `
    <svg width="20" height="20" fill="currentColor" viewBox="0 0 24 24">
      <path d="M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z"/>
    </svg>
    Instalar App
  `;
  installBtn.style.cssText = `
    position: fixed;
    bottom: 20px;
    right: 20px;
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 12px 20px;
    background: linear-gradient(135deg, #003B4A 0%, #005A6E 100%);
    color: white;
    border: none;
    border-radius: 24px;
    font-family: 'Inter', sans-serif;
    font-size: 0.875rem;
    font-weight: 500;
    cursor: pointer;
    box-shadow: 0 4px 20px rgba(0, 59, 74, 0.3);
    z-index: 9999;
    transition: all 0.2s ease;
  `;
  installBtn.addEventListener('mouseenter', () => {
    installBtn.style.transform = 'translateY(-2px)';
    installBtn.style.boxShadow = '0 6px 24px rgba(0, 59, 74, 0.4)';
  });
  installBtn.addEventListener('mouseleave', () => {
    installBtn.style.transform = 'translateY(0)';
    installBtn.style.boxShadow = '0 4px 20px rgba(0, 59, 74, 0.3)';
  });
  installBtn.addEventListener('click', installApp);

  document.body.appendChild(installBtn);
}

async function installApp() {
  if (!deferredPrompt) return;

  deferredPrompt.prompt();
  const { outcome } = await deferredPrompt.userChoice;

  console.log('[PWA] Install prompt outcome:', outcome);

  if (outcome === 'accepted') {
    document.getElementById('pwa-install-btn')?.remove();
  }

  deferredPrompt = null;
}

// Remover botao se instalado via outros meios
window.addEventListener('appinstalled', () => {
  console.log('[PWA] App installed');
  document.getElementById('pwa-install-btn')?.remove();
  deferredPrompt = null;
});

// ============================================================================
// BACKGROUND SYNC SETUP
// ============================================================================

async function setupBackgroundSync() {
  if (!('serviceWorker' in navigator) || !('SyncManager' in window)) {
    console.log('[PWA] Background Sync not supported');
    return;
  }

  try {
    const registration = await navigator.serviceWorker.ready;

    // Registrar sync periodico se suportado
    if ('periodicSync' in registration) {
      const status = await navigator.permissions.query({
        name: 'periodic-background-sync'
      });

      if (status.state === 'granted') {
        await registration.periodicSync.register('sync-data', {
          minInterval: 15 * 60 * 1000 // 15 minutos
        });
        console.log('[PWA] Periodic sync registered');
      }
    }
  } catch (error) {
    console.warn('[PWA] Background sync setup failed:', error);
  }
}

// ============================================================================
// INITIALIZE PWA
// ============================================================================

async function initPWA() {
  console.log('[PWA] Initializing...');

  // 1. Registrar Service Worker
  await registerServiceWorker();

  // 2. Inicializar dados offline
  await initOfflineData();

  // 3. Configurar background sync
  await setupBackgroundSync();

  console.log('[PWA] Initialization complete');
}

// Iniciar quando DOM estiver pronto
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initPWA);
} else {
  initPWA();
}

// ============================================================================
// EXPORTS
// ============================================================================

window.PWA = {
  registerServiceWorker,
  updateApp,
  installApp,
  initOfflineData
};
