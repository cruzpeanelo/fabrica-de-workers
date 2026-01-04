# -*- coding: utf-8 -*-
"""
Offline Sync Module (Issue #260)
================================
Sistema de modo offline com sincronizacao automatica.

Funcionalidades:
- Deteccao de status online/offline
- Queue de operacoes offline
- Sincronizacao automatica ao reconectar
- Resolucao de conflitos

Endpoints:
- GET /api/offline/status - Status de conexao
- POST /api/offline/queue - Adiciona operacao a fila
- POST /api/offline/sync - Sincroniza fila
- GET /api/offline/pending - Operacoes pendentes
"""

from fastapi import APIRouter, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.responses import HTMLResponse
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime
from enum import Enum
import json
import uuid
import asyncio

from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask


router = APIRouter(prefix="/api/offline", tags=["Offline Sync"])


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class OperationType(str, Enum):
    CREATE = "create"
    UPDATE = "update"
    DELETE = "delete"
    MOVE = "move"


class EntityType(str, Enum):
    STORY = "story"
    TASK = "task"
    DOCUMENT = "document"
    COMMENT = "comment"


class ConflictStrategy(str, Enum):
    SERVER_WINS = "server_wins"
    CLIENT_WINS = "client_wins"
    MERGE = "merge"
    MANUAL = "manual"


class SyncStatus(str, Enum):
    PENDING = "pending"
    SYNCING = "syncing"
    SYNCED = "synced"
    CONFLICT = "conflict"
    FAILED = "failed"


class OfflineOperation(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    operation_type: OperationType
    entity_type: EntityType
    entity_id: str
    data: Dict[str, Any]
    timestamp: str = Field(default_factory=lambda: datetime.utcnow().isoformat())
    status: SyncStatus = SyncStatus.PENDING
    retry_count: int = 0
    error_message: Optional[str] = None
    client_version: int = 0
    server_version: Optional[int] = None


class SyncRequest(BaseModel):
    operations: List[OfflineOperation]
    client_id: str
    conflict_strategy: ConflictStrategy = ConflictStrategy.SERVER_WINS


class ConflictInfo(BaseModel):
    operation_id: str
    entity_type: EntityType
    entity_id: str
    client_data: Dict[str, Any]
    server_data: Dict[str, Any]
    client_version: int
    server_version: int
    resolved: bool = False
    resolution: Optional[str] = None


class SyncResult(BaseModel):
    success: bool
    synced_count: int
    failed_count: int
    conflicts: List[ConflictInfo]
    synced_operations: List[str]
    failed_operations: List[Dict[str, Any]]
    server_timestamp: str


# =============================================================================
# IN-MEMORY STORAGE (for demo - in production use Redis or DB)
# =============================================================================

# Store pending operations per client
pending_operations: Dict[str, List[OfflineOperation]] = {}

# Store entity versions for conflict detection
entity_versions: Dict[str, int] = {}

# Store active connections for real-time sync
active_connections: List[WebSocket] = []


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_entity_version(entity_type: EntityType, entity_id: str) -> int:
    """Get current version of an entity."""
    key = f"{entity_type.value}:{entity_id}"
    return entity_versions.get(key, 0)


def update_entity_version(entity_type: EntityType, entity_id: str) -> int:
    """Increment and return new version of an entity."""
    key = f"{entity_type.value}:{entity_id}"
    new_version = entity_versions.get(key, 0) + 1
    entity_versions[key] = new_version
    return new_version


def detect_conflict(operation: OfflineOperation) -> Optional[ConflictInfo]:
    """Detect if operation has a conflict with server state."""
    server_version = get_entity_version(operation.entity_type, operation.entity_id)

    if operation.client_version < server_version:
        return ConflictInfo(
            operation_id=operation.id,
            entity_type=operation.entity_type,
            entity_id=operation.entity_id,
            client_data=operation.data,
            server_data={},  # Would fetch from DB in production
            client_version=operation.client_version,
            server_version=server_version
        )
    return None


def apply_operation(operation: OfflineOperation) -> bool:
    """Apply an operation to the database."""
    db = SessionLocal()
    try:
        if operation.entity_type == EntityType.STORY:
            if operation.operation_type == OperationType.UPDATE:
                story = db.query(Story).filter(Story.story_id == operation.entity_id).first()
                if story:
                    for key, value in operation.data.items():
                        if hasattr(story, key):
                            setattr(story, key, value)
                    db.commit()
                    update_entity_version(operation.entity_type, operation.entity_id)
                    return True
            elif operation.operation_type == OperationType.MOVE:
                story = db.query(Story).filter(Story.story_id == operation.entity_id).first()
                if story and "status" in operation.data:
                    story.status = operation.data["status"]
                    db.commit()
                    update_entity_version(operation.entity_type, operation.entity_id)
                    return True

        elif operation.entity_type == EntityType.TASK:
            if operation.operation_type == OperationType.UPDATE:
                task = db.query(StoryTask).filter(StoryTask.task_id == operation.entity_id).first()
                if task:
                    for key, value in operation.data.items():
                        if hasattr(task, key):
                            setattr(task, key, value)
                    db.commit()
                    update_entity_version(operation.entity_type, operation.entity_id)
                    return True

        return False
    except Exception as e:
        db.rollback()
        print(f"[OfflineSync] Error applying operation: {e}")
        return False
    finally:
        db.close()


def merge_data(client_data: Dict, server_data: Dict) -> Dict:
    """Merge client and server data (simple last-write-wins per field)."""
    merged = server_data.copy()
    # In a real implementation, would use timestamps per field
    for key, value in client_data.items():
        if key not in server_data or value is not None:
            merged[key] = value
    return merged


async def broadcast_sync_update(message: Dict):
    """Broadcast sync updates to all connected clients."""
    for connection in active_connections:
        try:
            await connection.send_json(message)
        except Exception:
            pass


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_connection_status(request: Request):
    """
    Retorna status de conexao e informacoes de sincronizacao.

    Returns:
        - online: sempre True (servidor esta respondendo)
        - server_time: timestamp do servidor
        - pending_count: operacoes pendentes globais
        - version: versao do protocolo de sync
    """
    total_pending = sum(len(ops) for ops in pending_operations.values())

    return {
        "online": True,
        "server_time": datetime.utcnow().isoformat(),
        "pending_count": total_pending,
        "version": "1.0.0",
        "features": {
            "conflict_resolution": True,
            "auto_retry": True,
            "batch_sync": True,
            "websocket_push": True
        }
    }


@router.post("/queue")
async def add_to_queue(operation: OfflineOperation, client_id: str = "default"):
    """
    Adiciona uma operacao a fila offline.

    Esta operacao sera sincronizada quando o cliente chamar /sync.
    """
    if client_id not in pending_operations:
        pending_operations[client_id] = []

    # Check for duplicate
    existing_ids = [op.id for op in pending_operations[client_id]]
    if operation.id in existing_ids:
        return {
            "success": False,
            "message": "Operacao ja existe na fila",
            "operation_id": operation.id
        }

    pending_operations[client_id].append(operation)

    return {
        "success": True,
        "message": "Operacao adicionada a fila",
        "operation_id": operation.id,
        "queue_position": len(pending_operations[client_id]),
        "timestamp": datetime.utcnow().isoformat()
    }


@router.post("/sync")
async def sync_operations(request: SyncRequest):
    """
    Sincroniza todas as operacoes pendentes.

    Processo:
    1. Valida cada operacao
    2. Detecta conflitos
    3. Aplica estrategia de resolucao
    4. Executa operacoes validas
    5. Retorna resultado detalhado
    """
    synced_operations = []
    failed_operations = []
    conflicts = []

    for operation in request.operations:
        # Detect conflicts
        conflict = detect_conflict(operation)

        if conflict:
            if request.conflict_strategy == ConflictStrategy.SERVER_WINS:
                # Skip client operation, server data wins
                failed_operations.append({
                    "id": operation.id,
                    "reason": "conflict_server_wins",
                    "server_version": conflict.server_version
                })
                continue

            elif request.conflict_strategy == ConflictStrategy.CLIENT_WINS:
                # Apply client operation anyway
                pass

            elif request.conflict_strategy == ConflictStrategy.MERGE:
                # Merge data
                operation.data = merge_data(
                    operation.data,
                    conflict.server_data
                )

            elif request.conflict_strategy == ConflictStrategy.MANUAL:
                # Add to conflicts list for manual resolution
                conflicts.append(conflict)
                continue

        # Apply operation
        success = apply_operation(operation)

        if success:
            synced_operations.append(operation.id)
        else:
            failed_operations.append({
                "id": operation.id,
                "reason": "apply_failed",
                "retry_count": operation.retry_count + 1
            })

    # Clean up synced operations from pending queue
    if request.client_id in pending_operations:
        pending_operations[request.client_id] = [
            op for op in pending_operations[request.client_id]
            if op.id not in synced_operations
        ]

    # Broadcast sync update to other clients
    await broadcast_sync_update({
        "type": "sync_completed",
        "client_id": request.client_id,
        "synced_count": len(synced_operations),
        "timestamp": datetime.utcnow().isoformat()
    })

    return SyncResult(
        success=len(failed_operations) == 0 and len(conflicts) == 0,
        synced_count=len(synced_operations),
        failed_count=len(failed_operations),
        conflicts=conflicts,
        synced_operations=synced_operations,
        failed_operations=failed_operations,
        server_timestamp=datetime.utcnow().isoformat()
    )


@router.get("/pending")
async def get_pending_operations(client_id: str = "default"):
    """
    Retorna operacoes pendentes para um cliente.
    """
    operations = pending_operations.get(client_id, [])

    return {
        "client_id": client_id,
        "pending_count": len(operations),
        "operations": [op.dict() for op in operations],
        "server_time": datetime.utcnow().isoformat()
    }


@router.delete("/pending/{operation_id}")
async def remove_pending_operation(operation_id: str, client_id: str = "default"):
    """
    Remove uma operacao pendente da fila.
    """
    if client_id not in pending_operations:
        raise HTTPException(status_code=404, detail="Cliente nao encontrado")

    original_count = len(pending_operations[client_id])
    pending_operations[client_id] = [
        op for op in pending_operations[client_id]
        if op.id != operation_id
    ]

    if len(pending_operations[client_id]) == original_count:
        raise HTTPException(status_code=404, detail="Operacao nao encontrada")

    return {
        "success": True,
        "message": "Operacao removida da fila",
        "operation_id": operation_id
    }


@router.post("/resolve-conflict")
async def resolve_conflict(
    operation_id: str,
    resolution: ConflictStrategy,
    merged_data: Optional[Dict[str, Any]] = None,
    client_id: str = "default"
):
    """
    Resolve um conflito manualmente.
    """
    if client_id not in pending_operations:
        raise HTTPException(status_code=404, detail="Cliente nao encontrado")

    # Find the operation
    operation = None
    for op in pending_operations[client_id]:
        if op.id == operation_id:
            operation = op
            break

    if not operation:
        raise HTTPException(status_code=404, detail="Operacao nao encontrada")

    if resolution == ConflictStrategy.CLIENT_WINS:
        # Apply client version
        success = apply_operation(operation)
    elif resolution == ConflictStrategy.SERVER_WINS:
        # Just remove from queue
        success = True
    elif resolution == ConflictStrategy.MERGE and merged_data:
        # Apply merged data
        operation.data = merged_data
        success = apply_operation(operation)
    else:
        raise HTTPException(status_code=400, detail="Dados de merge necessarios")

    if success:
        # Remove from pending
        pending_operations[client_id] = [
            op for op in pending_operations[client_id]
            if op.id != operation_id
        ]

    return {
        "success": success,
        "resolution": resolution.value,
        "operation_id": operation_id
    }


@router.websocket("/ws")
async def websocket_sync(websocket: WebSocket):
    """
    WebSocket para sincronizacao em tempo real.

    Mensagens suportadas:
    - ping: verificar conexao
    - subscribe: inscrever para updates
    - operation: enviar operacao para sync
    """
    await websocket.accept()
    active_connections.append(websocket)

    try:
        # Send initial status
        await websocket.send_json({
            "type": "connected",
            "server_time": datetime.utcnow().isoformat(),
            "message": "Conectado ao servidor de sincronizacao"
        })

        while True:
            data = await websocket.receive_json()

            if data.get("type") == "ping":
                await websocket.send_json({
                    "type": "pong",
                    "server_time": datetime.utcnow().isoformat()
                })

            elif data.get("type") == "operation":
                # Process operation
                operation = OfflineOperation(**data.get("operation", {}))
                client_id = data.get("client_id", "default")

                # Add to queue
                if client_id not in pending_operations:
                    pending_operations[client_id] = []
                pending_operations[client_id].append(operation)

                # Try to sync immediately
                conflict = detect_conflict(operation)
                if not conflict:
                    success = apply_operation(operation)
                    if success:
                        pending_operations[client_id].remove(operation)
                        await websocket.send_json({
                            "type": "synced",
                            "operation_id": operation.id,
                            "server_time": datetime.utcnow().isoformat()
                        })
                    else:
                        await websocket.send_json({
                            "type": "queued",
                            "operation_id": operation.id,
                            "reason": "apply_failed"
                        })
                else:
                    await websocket.send_json({
                        "type": "conflict",
                        "operation_id": operation.id,
                        "conflict": conflict.dict()
                    })

    except WebSocketDisconnect:
        active_connections.remove(websocket)
    except Exception as e:
        print(f"[OfflineSync] WebSocket error: {e}")
        if websocket in active_connections:
            active_connections.remove(websocket)


# =============================================================================
# OFFLINE PAGE WITH VUE.JS COMPONENT
# =============================================================================

@router.get("/dashboard", response_class=HTMLResponse)
async def offline_dashboard():
    """
    Dashboard de sincronizacao offline com componente Vue.js.
    Inclui IndexedDB para armazenamento local.
    """
    return """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Modo Offline - Plataforma E</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <style>
        :root {
            --primary: #003B4A;
            --secondary: #FF6C00;
            --success: #10B981;
            --warning: #F59E0B;
            --danger: #EF4444;
            --bg: #F3F4F6;
            --white: #FFFFFF;
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            background: var(--bg);
            min-height: 100vh;
        }

        .header {
            background: var(--primary);
            color: white;
            padding: 1rem 2rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .header h1 {
            font-size: 1.5rem;
        }

        .status-indicator {
            display: flex;
            align-items: center;
            gap: 0.5rem;
            padding: 0.5rem 1rem;
            border-radius: 20px;
            font-weight: 600;
        }

        .status-indicator.online {
            background: var(--success);
        }

        .status-indicator.offline {
            background: var(--danger);
        }

        .status-indicator .dot {
            width: 10px;
            height: 10px;
            border-radius: 50%;
            background: white;
            animation: pulse 2s infinite;
        }

        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }

        .container {
            max-width: 1200px;
            margin: 2rem auto;
            padding: 0 1rem;
        }

        .card {
            background: var(--white);
            border-radius: 12px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
            padding: 1.5rem;
            margin-bottom: 1.5rem;
        }

        .card h2 {
            color: var(--primary);
            margin-bottom: 1rem;
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1rem;
        }

        .stat-box {
            background: var(--bg);
            padding: 1rem;
            border-radius: 8px;
            text-align: center;
        }

        .stat-box .value {
            font-size: 2rem;
            font-weight: bold;
            color: var(--primary);
        }

        .stat-box .label {
            color: #666;
            font-size: 0.9rem;
        }

        .queue-list {
            max-height: 400px;
            overflow-y: auto;
        }

        .queue-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 1rem;
            border-bottom: 1px solid #eee;
        }

        .queue-item:last-child {
            border-bottom: none;
        }

        .queue-item .info {
            flex: 1;
        }

        .queue-item .type {
            display: inline-block;
            padding: 0.25rem 0.5rem;
            border-radius: 4px;
            font-size: 0.75rem;
            font-weight: 600;
            margin-right: 0.5rem;
        }

        .queue-item .type.create { background: #D1FAE5; color: #059669; }
        .queue-item .type.update { background: #DBEAFE; color: #2563EB; }
        .queue-item .type.delete { background: #FEE2E2; color: #DC2626; }
        .queue-item .type.move { background: #FEF3C7; color: #D97706; }

        .queue-item .status {
            padding: 0.25rem 0.75rem;
            border-radius: 20px;
            font-size: 0.8rem;
        }

        .queue-item .status.pending { background: #FEF3C7; color: #D97706; }
        .queue-item .status.syncing { background: #DBEAFE; color: #2563EB; }
        .queue-item .status.synced { background: #D1FAE5; color: #059669; }
        .queue-item .status.conflict { background: #FEE2E2; color: #DC2626; }

        .btn {
            padding: 0.75rem 1.5rem;
            border: none;
            border-radius: 8px;
            cursor: pointer;
            font-weight: 600;
            transition: all 0.2s;
        }

        .btn-primary {
            background: var(--secondary);
            color: white;
        }

        .btn-primary:hover {
            background: #E55A00;
        }

        .btn-primary:disabled {
            background: #ccc;
            cursor: not-allowed;
        }

        .btn-outline {
            background: transparent;
            border: 2px solid var(--primary);
            color: var(--primary);
        }

        .btn-outline:hover {
            background: var(--primary);
            color: white;
        }

        .actions {
            display: flex;
            gap: 1rem;
            margin-top: 1rem;
        }

        .conflict-modal {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 1000;
        }

        .conflict-modal .modal-content {
            background: white;
            border-radius: 12px;
            padding: 2rem;
            max-width: 600px;
            width: 90%;
        }

        .conflict-modal h3 {
            color: var(--danger);
            margin-bottom: 1rem;
        }

        .conflict-data {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 1rem;
            margin: 1rem 0;
        }

        .conflict-data pre {
            background: var(--bg);
            padding: 1rem;
            border-radius: 8px;
            font-size: 0.8rem;
            overflow: auto;
        }

        .empty-state {
            text-align: center;
            padding: 3rem;
            color: #666;
        }

        .empty-state svg {
            width: 80px;
            height: 80px;
            margin-bottom: 1rem;
            opacity: 0.5;
        }

        .sync-progress {
            margin-top: 1rem;
        }

        .progress-bar {
            height: 8px;
            background: var(--bg);
            border-radius: 4px;
            overflow: hidden;
        }

        .progress-bar .fill {
            height: 100%;
            background: var(--success);
            transition: width 0.3s;
        }
    </style>
</head>
<body>
    <div id="app">
        <header class="header">
            <h1>Modo Offline - Sincronizacao</h1>
            <div class="status-indicator" :class="isOnline ? 'online' : 'offline'">
                <span class="dot"></span>
                {{ isOnline ? 'Online' : 'Offline' }}
            </div>
        </header>

        <div class="container">
            <!-- Stats -->
            <div class="card">
                <h2>
                    <svg width="24" height="24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                        <path d="M9 12l2 2 4-4"/>
                    </svg>
                    Status de Sincronizacao
                </h2>
                <div class="stats">
                    <div class="stat-box">
                        <div class="value">{{ pendingCount }}</div>
                        <div class="label">Pendentes</div>
                    </div>
                    <div class="stat-box">
                        <div class="value">{{ syncedCount }}</div>
                        <div class="label">Sincronizadas</div>
                    </div>
                    <div class="stat-box">
                        <div class="value">{{ conflictCount }}</div>
                        <div class="label">Conflitos</div>
                    </div>
                    <div class="stat-box">
                        <div class="value">{{ lastSync }}</div>
                        <div class="label">Ultima Sincronizacao</div>
                    </div>
                </div>

                <div class="actions">
                    <button class="btn btn-primary" @click="syncNow" :disabled="!isOnline || syncing || pendingCount === 0">
                        {{ syncing ? 'Sincronizando...' : 'Sincronizar Agora' }}
                    </button>
                    <button class="btn btn-outline" @click="clearQueue" :disabled="pendingCount === 0">
                        Limpar Fila
                    </button>
                </div>

                <div v-if="syncing" class="sync-progress">
                    <div class="progress-bar">
                        <div class="fill" :style="{width: syncProgress + '%'}"></div>
                    </div>
                    <small>{{ syncProgress }}% concluido</small>
                </div>
            </div>

            <!-- Queue -->
            <div class="card">
                <h2>
                    <svg width="24" height="24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M4 6h16M4 12h16M4 18h16"/>
                    </svg>
                    Fila de Operacoes ({{ operations.length }})
                </h2>

                <div v-if="operations.length === 0" class="empty-state">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                    </svg>
                    <p>Nenhuma operacao pendente</p>
                    <small>Todas as alteracoes estao sincronizadas</small>
                </div>

                <div v-else class="queue-list">
                    <div v-for="op in operations" :key="op.id" class="queue-item">
                        <div class="info">
                            <span class="type" :class="op.operation_type">{{ op.operation_type }}</span>
                            <strong>{{ op.entity_type }}</strong>: {{ op.entity_id }}
                            <br>
                            <small>{{ formatTime(op.timestamp) }}</small>
                        </div>
                        <span class="status" :class="op.status">{{ op.status }}</span>
                    </div>
                </div>
            </div>

            <!-- Conflicts -->
            <div v-if="conflicts.length > 0" class="card">
                <h2 style="color: var(--danger)">
                    <svg width="24" height="24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                    </svg>
                    Conflitos ({{ conflicts.length }})
                </h2>
                <p>Os seguintes itens precisam de resolucao manual:</p>
                <div v-for="conflict in conflicts" :key="conflict.operation_id" class="queue-item">
                    <div class="info">
                        <strong>{{ conflict.entity_type }}</strong>: {{ conflict.entity_id }}
                        <br>
                        <small>Versao cliente: {{ conflict.client_version }} | Versao servidor: {{ conflict.server_version }}</small>
                    </div>
                    <button class="btn btn-outline" @click="showConflictModal(conflict)">Resolver</button>
                </div>
            </div>
        </div>

        <!-- Conflict Resolution Modal -->
        <div v-if="selectedConflict" class="conflict-modal" @click.self="selectedConflict = null">
            <div class="modal-content">
                <h3>Resolver Conflito</h3>
                <p>Escolha como resolver o conflito para {{ selectedConflict.entity_type }} {{ selectedConflict.entity_id }}:</p>

                <div class="conflict-data">
                    <div>
                        <strong>Dados Locais (v{{ selectedConflict.client_version }})</strong>
                        <pre>{{ JSON.stringify(selectedConflict.client_data, null, 2) }}</pre>
                    </div>
                    <div>
                        <strong>Dados do Servidor (v{{ selectedConflict.server_version }})</strong>
                        <pre>{{ JSON.stringify(selectedConflict.server_data, null, 2) }}</pre>
                    </div>
                </div>

                <div class="actions">
                    <button class="btn btn-primary" @click="resolveConflict('client_wins')">Usar Dados Locais</button>
                    <button class="btn btn-outline" @click="resolveConflict('server_wins')">Usar Dados do Servidor</button>
                    <button class="btn btn-outline" @click="resolveConflict('merge')">Mesclar</button>
                </div>
            </div>
        </div>
    </div>

    <script>
        // IndexedDB Setup
        const DB_NAME = 'FabricaAgentesOffline';
        const DB_VERSION = 1;
        const STORE_NAME = 'operations';

        let db = null;

        async function initDB() {
            return new Promise((resolve, reject) => {
                const request = indexedDB.open(DB_NAME, DB_VERSION);

                request.onerror = () => reject(request.error);
                request.onsuccess = () => {
                    db = request.result;
                    resolve(db);
                };

                request.onupgradeneeded = (event) => {
                    const db = event.target.result;
                    if (!db.objectStoreNames.contains(STORE_NAME)) {
                        const store = db.createObjectStore(STORE_NAME, { keyPath: 'id' });
                        store.createIndex('status', 'status', { unique: false });
                        store.createIndex('timestamp', 'timestamp', { unique: false });
                    }
                };
            });
        }

        async function saveOperation(operation) {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(STORE_NAME, 'readwrite');
                const store = tx.objectStore(STORE_NAME);
                const request = store.put(operation);
                request.onsuccess = () => resolve(request.result);
                request.onerror = () => reject(request.error);
            });
        }

        async function getOperations() {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(STORE_NAME, 'readonly');
                const store = tx.objectStore(STORE_NAME);
                const request = store.getAll();
                request.onsuccess = () => resolve(request.result);
                request.onerror = () => reject(request.error);
            });
        }

        async function deleteOperation(id) {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(STORE_NAME, 'readwrite');
                const store = tx.objectStore(STORE_NAME);
                const request = store.delete(id);
                request.onsuccess = () => resolve();
                request.onerror = () => reject(request.error);
            });
        }

        async function clearOperations() {
            return new Promise((resolve, reject) => {
                const tx = db.transaction(STORE_NAME, 'readwrite');
                const store = tx.objectStore(STORE_NAME);
                const request = store.clear();
                request.onsuccess = () => resolve();
                request.onerror = () => reject(request.error);
            });
        }

        // Vue App
        const { createApp, ref, computed, onMounted, onUnmounted } = Vue;

        createApp({
            setup() {
                const isOnline = ref(navigator.onLine);
                const operations = ref([]);
                const conflicts = ref([]);
                const syncing = ref(false);
                const syncProgress = ref(0);
                const syncedCount = ref(0);
                const lastSyncTime = ref(null);
                const selectedConflict = ref(null);
                const clientId = ref('client_' + Math.random().toString(36).substr(2, 9));

                let ws = null;

                const pendingCount = computed(() =>
                    operations.value.filter(op => op.status === 'pending').length
                );

                const conflictCount = computed(() => conflicts.value.length);

                const lastSync = computed(() => {
                    if (!lastSyncTime.value) return 'Nunca';
                    const diff = Date.now() - lastSyncTime.value;
                    if (diff < 60000) return 'Agora';
                    if (diff < 3600000) return Math.floor(diff / 60000) + 'min atras';
                    return new Date(lastSyncTime.value).toLocaleTimeString();
                });

                function handleOnline() {
                    isOnline.value = true;
                    connectWebSocket();
                    if (pendingCount.value > 0) {
                        syncNow();
                    }
                }

                function handleOffline() {
                    isOnline.value = false;
                    if (ws) {
                        ws.close();
                    }
                }

                function connectWebSocket() {
                    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                    ws = new WebSocket(`${protocol}//${window.location.host}/api/offline/ws`);

                    ws.onopen = () => {
                        console.log('WebSocket connected');
                    };

                    ws.onmessage = (event) => {
                        const data = JSON.parse(event.data);
                        console.log('WS message:', data);

                        if (data.type === 'synced') {
                            const op = operations.value.find(o => o.id === data.operation_id);
                            if (op) {
                                op.status = 'synced';
                                syncedCount.value++;
                            }
                        } else if (data.type === 'conflict') {
                            conflicts.value.push(data.conflict);
                        }
                    };

                    ws.onclose = () => {
                        console.log('WebSocket disconnected');
                        if (isOnline.value) {
                            setTimeout(connectWebSocket, 5000);
                        }
                    };
                }

                async function loadOperations() {
                    try {
                        await initDB();
                        operations.value = await getOperations();
                    } catch (e) {
                        console.error('Error loading operations:', e);
                    }
                }

                async function syncNow() {
                    if (!isOnline.value || syncing.value) return;

                    syncing.value = true;
                    syncProgress.value = 0;

                    const pending = operations.value.filter(op => op.status === 'pending');
                    const total = pending.length;

                    try {
                        const response = await fetch('/api/offline/sync', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({
                                operations: pending,
                                client_id: clientId.value,
                                conflict_strategy: 'server_wins'
                            })
                        });

                        const result = await response.json();

                        // Update synced operations
                        for (const id of result.synced_operations) {
                            const op = operations.value.find(o => o.id === id);
                            if (op) {
                                op.status = 'synced';
                                await deleteOperation(id);
                            }
                        }

                        // Handle conflicts
                        conflicts.value = result.conflicts;

                        syncedCount.value += result.synced_count;
                        lastSyncTime.value = Date.now();
                        syncProgress.value = 100;

                        // Remove synced from local list
                        operations.value = operations.value.filter(
                            op => !result.synced_operations.includes(op.id)
                        );

                    } catch (e) {
                        console.error('Sync error:', e);
                    } finally {
                        syncing.value = false;
                    }
                }

                async function clearQueue() {
                    if (confirm('Tem certeza que deseja limpar a fila de operacoes pendentes?')) {
                        await clearOperations();
                        operations.value = [];
                    }
                }

                function showConflictModal(conflict) {
                    selectedConflict.value = conflict;
                }

                async function resolveConflict(strategy) {
                    if (!selectedConflict.value) return;

                    try {
                        await fetch('/api/offline/resolve-conflict?' + new URLSearchParams({
                            operation_id: selectedConflict.value.operation_id,
                            resolution: strategy,
                            client_id: clientId.value
                        }), {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' }
                        });

                        conflicts.value = conflicts.value.filter(
                            c => c.operation_id !== selectedConflict.value.operation_id
                        );
                        selectedConflict.value = null;

                    } catch (e) {
                        console.error('Resolve error:', e);
                    }
                }

                function formatTime(timestamp) {
                    return new Date(timestamp).toLocaleString();
                }

                // Expose method to add operations (for use by other components)
                window.addOfflineOperation = async function(operation) {
                    const op = {
                        id: 'op_' + Date.now(),
                        ...operation,
                        status: 'pending',
                        timestamp: new Date().toISOString(),
                        client_version: 0,
                        retry_count: 0
                    };

                    operations.value.push(op);
                    await saveOperation(op);

                    if (isOnline.value) {
                        syncNow();
                    }

                    return op.id;
                };

                onMounted(() => {
                    window.addEventListener('online', handleOnline);
                    window.addEventListener('offline', handleOffline);
                    loadOperations();
                    if (isOnline.value) {
                        connectWebSocket();
                    }
                });

                onUnmounted(() => {
                    window.removeEventListener('online', handleOnline);
                    window.removeEventListener('offline', handleOffline);
                    if (ws) ws.close();
                });

                return {
                    isOnline,
                    operations,
                    conflicts,
                    syncing,
                    syncProgress,
                    syncedCount,
                    pendingCount,
                    conflictCount,
                    lastSync,
                    selectedConflict,
                    syncNow,
                    clearQueue,
                    showConflictModal,
                    resolveConflict,
                    formatTime
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_offline_sync(app):
    """Registra os endpoints de sincronizacao offline no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Offline Sync endpoints loaded: /api/offline/*")
