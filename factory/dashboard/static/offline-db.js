/**
 * IndexedDB - Fabrica de Agentes Offline Storage
 * ================================================
 * Gerenciamento de dados offline com:
 * - Armazenamento local de stories, tasks, projects
 * - Queue de acoes pendentes para sincronizacao
 * - Sincronizacao automatica quando online
 */

const DB_NAME = 'FabricaDeAgentesDB';
const DB_VERSION = 1;

// Stores (tabelas)
const STORES = {
  STORIES: 'stories',
  TASKS: 'tasks',
  PROJECTS: 'projects',
  EPICS: 'epics',
  SPRINTS: 'sprints',
  PENDING_ACTIONS: 'pending_actions',
  SYNC_META: 'sync_meta'
};

// Tipos de acoes pendentes
const ACTION_TYPES = {
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  MOVE: 'MOVE'
};

class OfflineDB {
  constructor() {
    this.db = null;
    this.isReady = false;
    this.onReadyCallbacks = [];
  }

  /**
   * Inicializar banco de dados
   */
  async init() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION);

      request.onerror = (event) => {
        console.error('[OfflineDB] Error opening database:', event.target.error);
        reject(event.target.error);
      };

      request.onsuccess = (event) => {
        this.db = event.target.result;
        this.isReady = true;
        console.log('[OfflineDB] Database opened successfully');

        // Executar callbacks pendentes
        this.onReadyCallbacks.forEach(cb => cb());
        this.onReadyCallbacks = [];

        resolve(this.db);
      };

      request.onupgradeneeded = (event) => {
        console.log('[OfflineDB] Upgrading database...');
        const db = event.target.result;

        // Stories store
        if (!db.objectStoreNames.contains(STORES.STORIES)) {
          const storyStore = db.createObjectStore(STORES.STORIES, { keyPath: 'story_id' });
          storyStore.createIndex('project_id', 'project_id', { unique: false });
          storyStore.createIndex('status', 'status', { unique: false });
          storyStore.createIndex('sprint_id', 'sprint_id', { unique: false });
          storyStore.createIndex('epic_id', 'epic_id', { unique: false });
          storyStore.createIndex('updated_at', 'updated_at', { unique: false });
        }

        // Tasks store
        if (!db.objectStoreNames.contains(STORES.TASKS)) {
          const taskStore = db.createObjectStore(STORES.TASKS, { keyPath: 'task_id' });
          taskStore.createIndex('story_id', 'story_id', { unique: false });
          taskStore.createIndex('status', 'status', { unique: false });
        }

        // Projects store
        if (!db.objectStoreNames.contains(STORES.PROJECTS)) {
          const projectStore = db.createObjectStore(STORES.PROJECTS, { keyPath: 'project_id' });
        }

        // Epics store
        if (!db.objectStoreNames.contains(STORES.EPICS)) {
          const epicStore = db.createObjectStore(STORES.EPICS, { keyPath: 'epic_id' });
          epicStore.createIndex('project_id', 'project_id', { unique: false });
        }

        // Sprints store
        if (!db.objectStoreNames.contains(STORES.SPRINTS)) {
          const sprintStore = db.createObjectStore(STORES.SPRINTS, { keyPath: 'sprint_id' });
          sprintStore.createIndex('project_id', 'project_id', { unique: false });
        }

        // Pending actions store (queue de sincronizacao)
        if (!db.objectStoreNames.contains(STORES.PENDING_ACTIONS)) {
          const pendingStore = db.createObjectStore(STORES.PENDING_ACTIONS, {
            keyPath: 'action_id',
            autoIncrement: true
          });
          pendingStore.createIndex('entity_type', 'entity_type', { unique: false });
          pendingStore.createIndex('created_at', 'created_at', { unique: false });
        }

        // Sync metadata store
        if (!db.objectStoreNames.contains(STORES.SYNC_META)) {
          db.createObjectStore(STORES.SYNC_META, { keyPath: 'key' });
        }

        console.log('[OfflineDB] Database upgrade complete');
      };
    });
  }

  /**
   * Aguardar banco estar pronto
   */
  onReady(callback) {
    if (this.isReady) {
      callback();
    } else {
      this.onReadyCallbacks.push(callback);
    }
  }

  // ===========================================================================
  // OPERACOES GENERICAS
  // ===========================================================================

  async getAll(storeName) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readonly');
      const store = transaction.objectStore(storeName);
      const request = store.getAll();

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  async get(storeName, key) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readonly');
      const store = transaction.objectStore(storeName);
      const request = store.get(key);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  async put(storeName, data) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readwrite');
      const store = transaction.objectStore(storeName);
      const request = store.put(data);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  async delete(storeName, key) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readwrite');
      const store = transaction.objectStore(storeName);
      const request = store.delete(key);

      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }

  async clear(storeName) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readwrite');
      const store = transaction.objectStore(storeName);
      const request = store.clear();

      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }

  async getByIndex(storeName, indexName, value) {
    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction(storeName, 'readonly');
      const store = transaction.objectStore(storeName);
      const index = store.index(indexName);
      const request = index.getAll(value);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  // ===========================================================================
  // STORIES
  // ===========================================================================

  async saveStory(story) {
    story._localUpdatedAt = Date.now();
    return this.put(STORES.STORIES, story);
  }

  async getStory(storyId) {
    return this.get(STORES.STORIES, storyId);
  }

  async getAllStories() {
    return this.getAll(STORES.STORIES);
  }

  async getStoriesByProject(projectId) {
    return this.getByIndex(STORES.STORIES, 'project_id', projectId);
  }

  async getStoriesByStatus(status) {
    return this.getByIndex(STORES.STORIES, 'status', status);
  }

  async deleteStory(storyId) {
    return this.delete(STORES.STORIES, storyId);
  }

  async saveStories(stories) {
    const transaction = this.db.transaction(STORES.STORIES, 'readwrite');
    const store = transaction.objectStore(STORES.STORIES);

    return new Promise((resolve, reject) => {
      stories.forEach(story => {
        story._localUpdatedAt = Date.now();
        store.put(story);
      });

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  // ===========================================================================
  // TASKS
  // ===========================================================================

  async saveTask(task) {
    task._localUpdatedAt = Date.now();
    return this.put(STORES.TASKS, task);
  }

  async getTask(taskId) {
    return this.get(STORES.TASKS, taskId);
  }

  async getTasksByStory(storyId) {
    return this.getByIndex(STORES.TASKS, 'story_id', storyId);
  }

  async deleteTask(taskId) {
    return this.delete(STORES.TASKS, taskId);
  }

  async saveTasks(tasks) {
    const transaction = this.db.transaction(STORES.TASKS, 'readwrite');
    const store = transaction.objectStore(STORES.TASKS);

    return new Promise((resolve, reject) => {
      tasks.forEach(task => {
        task._localUpdatedAt = Date.now();
        store.put(task);
      });

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  // ===========================================================================
  // PROJECTS, EPICS, SPRINTS
  // ===========================================================================

  async saveProject(project) {
    return this.put(STORES.PROJECTS, project);
  }

  async getProject(projectId) {
    return this.get(STORES.PROJECTS, projectId);
  }

  async getAllProjects() {
    return this.getAll(STORES.PROJECTS);
  }

  async saveEpic(epic) {
    return this.put(STORES.EPICS, epic);
  }

  async getEpicsByProject(projectId) {
    return this.getByIndex(STORES.EPICS, 'project_id', projectId);
  }

  async saveSprint(sprint) {
    return this.put(STORES.SPRINTS, sprint);
  }

  async getSprintsByProject(projectId) {
    return this.getByIndex(STORES.SPRINTS, 'project_id', projectId);
  }

  // ===========================================================================
  // PENDING ACTIONS (QUEUE DE SINCRONIZACAO)
  // ===========================================================================

  /**
   * Adicionar acao a fila de sincronizacao
   */
  async queueAction(entityType, actionType, entityId, data) {
    const action = {
      entity_type: entityType,
      action_type: actionType,
      entity_id: entityId,
      data: data,
      created_at: Date.now(),
      retries: 0,
      last_error: null
    };

    return this.put(STORES.PENDING_ACTIONS, action);
  }

  /**
   * Obter todas as acoes pendentes
   */
  async getPendingActions() {
    return this.getAll(STORES.PENDING_ACTIONS);
  }

  /**
   * Obter contagem de acoes pendentes
   */
  async getPendingCount() {
    const actions = await this.getPendingActions();
    return actions.length;
  }

  /**
   * Remover acao da fila (apos sincronizar)
   */
  async removeAction(actionId) {
    return this.delete(STORES.PENDING_ACTIONS, actionId);
  }

  /**
   * Atualizar acao com erro
   */
  async updateActionError(actionId, error) {
    const action = await this.get(STORES.PENDING_ACTIONS, actionId);
    if (action) {
      action.retries++;
      action.last_error = error;
      action.last_attempt = Date.now();
      return this.put(STORES.PENDING_ACTIONS, action);
    }
  }

  /**
   * Limpar acoes pendentes
   */
  async clearPendingActions() {
    return this.clear(STORES.PENDING_ACTIONS);
  }

  // ===========================================================================
  // SYNC METADATA
  // ===========================================================================

  async setSyncMeta(key, value) {
    return this.put(STORES.SYNC_META, { key, value, updated_at: Date.now() });
  }

  async getSyncMeta(key) {
    const result = await this.get(STORES.SYNC_META, key);
    return result ? result.value : null;
  }

  async getLastSyncTime() {
    return this.getSyncMeta('lastSync');
  }

  async setLastSyncTime(timestamp = Date.now()) {
    return this.setSyncMeta('lastSync', timestamp);
  }
}

// ===========================================================================
// SYNC MANAGER
// ===========================================================================

class SyncManager {
  constructor(offlineDB) {
    this.db = offlineDB;
    this.isSyncing = false;
    this.syncCallbacks = [];
  }

  /**
   * Registrar callback para eventos de sync
   */
  onSync(callback) {
    this.syncCallbacks.push(callback);
  }

  /**
   * Notificar callbacks
   */
  notify(event, data) {
    this.syncCallbacks.forEach(cb => cb(event, data));
  }

  /**
   * Sincronizar todas as acoes pendentes
   */
  async syncAll() {
    if (this.isSyncing) {
      console.log('[SyncManager] Sync already in progress');
      return { success: false, reason: 'already_syncing' };
    }

    if (!navigator.onLine) {
      console.log('[SyncManager] Offline, cannot sync');
      return { success: false, reason: 'offline' };
    }

    this.isSyncing = true;
    this.notify('sync_start', {});

    try {
      const actions = await this.db.getPendingActions();
      console.log(`[SyncManager] Syncing ${actions.length} pending actions`);

      let successCount = 0;
      let errorCount = 0;

      for (const action of actions) {
        try {
          await this.processAction(action);
          await this.db.removeAction(action.action_id);
          successCount++;
          this.notify('action_synced', { action });
        } catch (error) {
          console.error('[SyncManager] Error syncing action:', error);
          await this.db.updateActionError(action.action_id, error.message);
          errorCount++;
          this.notify('action_error', { action, error: error.message });
        }
      }

      await this.db.setLastSyncTime();

      const result = {
        success: true,
        synced: successCount,
        errors: errorCount
      };

      this.notify('sync_complete', result);
      return result;

    } catch (error) {
      console.error('[SyncManager] Sync failed:', error);
      this.notify('sync_error', { error: error.message });
      return { success: false, error: error.message };

    } finally {
      this.isSyncing = false;
    }
  }

  /**
   * Processar uma acao individual
   */
  async processAction(action) {
    const { entity_type, action_type, entity_id, data } = action;

    let url, method, body;

    switch (entity_type) {
      case 'story':
        url = action_type === ACTION_TYPES.CREATE
          ? '/api/stories'
          : `/api/stories/${entity_id}`;
        break;

      case 'task':
        url = action_type === ACTION_TYPES.CREATE
          ? `/api/stories/${data.story_id}/tasks`
          : `/api/story-tasks/${entity_id}`;
        break;

      default:
        throw new Error(`Unknown entity type: ${entity_type}`);
    }

    switch (action_type) {
      case ACTION_TYPES.CREATE:
        method = 'POST';
        body = JSON.stringify(data);
        break;

      case ACTION_TYPES.UPDATE:
        method = 'PUT';
        body = JSON.stringify(data);
        break;

      case ACTION_TYPES.DELETE:
        method = 'DELETE';
        body = null;
        break;

      case ACTION_TYPES.MOVE:
        method = 'PATCH';
        url = `${url}/move`;
        body = JSON.stringify(data);
        break;

      default:
        throw new Error(`Unknown action type: ${action_type}`);
    }

    const response = await fetch(url, {
      method,
      headers: { 'Content-Type': 'application/json' },
      body
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.detail || `HTTP ${response.status}`);
    }

    return response.json();
  }

  /**
   * Baixar dados mais recentes do servidor
   */
  async pullFromServer(projectId = null) {
    if (!navigator.onLine) {
      console.log('[SyncManager] Offline, cannot pull');
      return { success: false, reason: 'offline' };
    }

    try {
      this.notify('pull_start', {});

      // Buscar stories
      const storiesUrl = projectId
        ? `/api/stories?project_id=${projectId}`
        : '/api/stories';

      const storiesResponse = await fetch(storiesUrl);
      if (storiesResponse.ok) {
        const stories = await storiesResponse.json();
        await this.db.saveStories(stories);
        console.log(`[SyncManager] Pulled ${stories.length} stories`);
      }

      // Buscar projetos
      const projectsResponse = await fetch('/api/projects');
      if (projectsResponse.ok) {
        const projects = await projectsResponse.json();
        for (const project of projects) {
          await this.db.saveProject(project);
        }
        console.log(`[SyncManager] Pulled ${projects.length} projects`);
      }

      await this.db.setLastSyncTime();
      this.notify('pull_complete', {});

      return { success: true };

    } catch (error) {
      console.error('[SyncManager] Pull failed:', error);
      this.notify('pull_error', { error: error.message });
      return { success: false, error: error.message };
    }
  }

  /**
   * Registrar Background Sync
   */
  async registerBackgroundSync(tag = 'sync-all') {
    if ('serviceWorker' in navigator && 'SyncManager' in window) {
      try {
        const registration = await navigator.serviceWorker.ready;
        await registration.sync.register(tag);
        console.log('[SyncManager] Background sync registered:', tag);
        return true;
      } catch (error) {
        console.warn('[SyncManager] Background sync not available:', error);
        return false;
      }
    }
    return false;
  }
}

// ===========================================================================
// INSTANCIA GLOBAL
// ===========================================================================

const offlineDB = new OfflineDB();
const syncManager = new SyncManager(offlineDB);

// Inicializar automaticamente
offlineDB.init().then(() => {
  console.log('[OfflineDB] Ready');

  // Sincronizar quando voltar online
  window.addEventListener('online', () => {
    console.log('[OfflineDB] Back online, syncing...');
    syncManager.syncAll();
  });

  // Receber mensagens do Service Worker
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.addEventListener('message', (event) => {
      if (event.data.type === 'SYNC_REQUEST') {
        syncManager.syncAll();
      }
    });
  }
});

// Exportar para uso global
window.OfflineDB = offlineDB;
window.SyncManager = syncManager;
window.STORES = STORES;
window.ACTION_TYPES = ACTION_TYPES;
