# -*- coding: utf-8 -*-
"""
Webhooks Outbound Module (Issue #275)
=====================================
Sistema de webhooks outbound para notificar sistemas externos.

Funcionalidades:
- Configuracao de webhooks por evento
- Retry automatico com backoff
- Logs de tentativas e respostas
- Validacao HMAC de payload
- Teste de webhook na UI
"""

from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel, HttpUrl
from typing import Optional, List, Dict
from datetime import datetime
import hashlib
import hmac
import json
import uuid
import asyncio

router = APIRouter(prefix="/api/webhooks", tags=["Webhooks"])

# Webhook event types
EVENT_TYPES = [
    "story.created",
    "story.updated",
    "story.deleted",
    "story.status_changed",
    "task.completed",
    "sprint.started",
    "sprint.completed",
    "comment.added"
]

# In-memory storage (in production, use database)
webhooks: Dict[str, dict] = {}
webhook_logs: List[dict] = []


class WebhookCreate(BaseModel):
    name: str
    url: HttpUrl
    events: List[str]
    secret: Optional[str] = None
    headers: Optional[Dict[str, str]] = None
    enabled: bool = True


class WebhookUpdate(BaseModel):
    name: Optional[str] = None
    url: Optional[HttpUrl] = None
    events: Optional[List[str]] = None
    secret: Optional[str] = None
    headers: Optional[Dict[str, str]] = None
    enabled: Optional[bool] = None


class WebhookTest(BaseModel):
    event_type: str = "test.ping"
    payload: Optional[dict] = None


@router.get("/")
async def list_webhooks():
    """Lista todos os webhooks configurados."""
    return {
        "webhooks": list(webhooks.values()),
        "event_types": EVENT_TYPES
    }


@router.get("/{webhook_id}")
async def get_webhook(webhook_id: str):
    """Retorna um webhook especifico."""
    if webhook_id not in webhooks:
        raise HTTPException(status_code=404, detail="Webhook nao encontrado")
    return webhooks[webhook_id]


@router.post("/")
async def create_webhook(webhook: WebhookCreate):
    """Cria um novo webhook."""
    webhook_id = f"wh_{uuid.uuid4().hex[:12]}"

    # Validate events
    invalid_events = [e for e in webhook.events if e not in EVENT_TYPES]
    if invalid_events:
        raise HTTPException(
            status_code=400,
            detail=f"Eventos invalidos: {', '.join(invalid_events)}"
        )

    new_webhook = {
        "id": webhook_id,
        "name": webhook.name,
        "url": str(webhook.url),
        "events": webhook.events,
        "secret": webhook.secret or uuid.uuid4().hex,
        "headers": webhook.headers or {},
        "enabled": webhook.enabled,
        "created_at": datetime.now().isoformat(),
        "last_triggered": None,
        "success_count": 0,
        "failure_count": 0
    }

    webhooks[webhook_id] = new_webhook

    return {
        "success": True,
        "webhook": new_webhook
    }


@router.put("/{webhook_id}")
async def update_webhook(webhook_id: str, webhook: WebhookUpdate):
    """Atualiza um webhook."""
    if webhook_id not in webhooks:
        raise HTTPException(status_code=404, detail="Webhook nao encontrado")

    wh = webhooks[webhook_id]

    if webhook.name is not None:
        wh["name"] = webhook.name
    if webhook.url is not None:
        wh["url"] = str(webhook.url)
    if webhook.events is not None:
        wh["events"] = webhook.events
    if webhook.secret is not None:
        wh["secret"] = webhook.secret
    if webhook.headers is not None:
        wh["headers"] = webhook.headers
    if webhook.enabled is not None:
        wh["enabled"] = webhook.enabled

    return {
        "success": True,
        "webhook": wh
    }


@router.delete("/{webhook_id}")
async def delete_webhook(webhook_id: str):
    """Exclui um webhook."""
    if webhook_id not in webhooks:
        raise HTTPException(status_code=404, detail="Webhook nao encontrado")

    del webhooks[webhook_id]
    return {"success": True, "message": "Webhook excluido"}


@router.post("/{webhook_id}/test")
async def test_webhook(webhook_id: str, test: WebhookTest, background_tasks: BackgroundTasks):
    """Envia um evento de teste para o webhook."""
    if webhook_id not in webhooks:
        raise HTTPException(status_code=404, detail="Webhook nao encontrado")

    wh = webhooks[webhook_id]

    payload = test.payload or {
        "event": test.event_type,
        "timestamp": datetime.now().isoformat(),
        "data": {
            "message": "Este e um evento de teste",
            "webhook_id": webhook_id
        }
    }

    # Send in background
    background_tasks.add_task(send_webhook, webhook_id, test.event_type, payload)

    return {
        "success": True,
        "message": "Evento de teste enviado",
        "payload": payload
    }


@router.get("/{webhook_id}/logs")
async def get_webhook_logs(webhook_id: str, limit: int = 20):
    """Retorna logs de um webhook."""
    logs = [log for log in webhook_logs if log["webhook_id"] == webhook_id]
    return {
        "logs": logs[-limit:][::-1]  # Most recent first
    }


@router.get("/logs/all")
async def get_all_logs(limit: int = 50):
    """Retorna todos os logs de webhooks."""
    return {
        "logs": webhook_logs[-limit:][::-1]
    }


async def send_webhook(webhook_id: str, event_type: str, payload: dict, retry_count: int = 0):
    """Envia payload para um webhook com retry."""
    import aiohttp

    if webhook_id not in webhooks:
        return

    wh = webhooks[webhook_id]

    if not wh["enabled"]:
        return

    # Create signature
    payload_str = json.dumps(payload)
    signature = hmac.new(
        wh["secret"].encode(),
        payload_str.encode(),
        hashlib.sha256
    ).hexdigest()

    headers = {
        "Content-Type": "application/json",
        "X-Webhook-Signature": f"sha256={signature}",
        "X-Webhook-Event": event_type,
        "X-Webhook-ID": webhook_id,
        **wh.get("headers", {})
    }

    log_entry = {
        "id": uuid.uuid4().hex[:8],
        "webhook_id": webhook_id,
        "event_type": event_type,
        "timestamp": datetime.now().isoformat(),
        "url": wh["url"],
        "payload": payload,
        "retry_count": retry_count,
        "status": None,
        "response": None,
        "error": None
    }

    try:
        async with aiohttp.ClientSession() as session:
            async with session.post(
                wh["url"],
                json=payload,
                headers=headers,
                timeout=aiohttp.ClientTimeout(total=30)
            ) as response:
                log_entry["status"] = response.status
                log_entry["response"] = await response.text()

                if response.status >= 200 and response.status < 300:
                    wh["success_count"] += 1
                    wh["last_triggered"] = datetime.now().isoformat()
                else:
                    wh["failure_count"] += 1
                    # Retry with exponential backoff
                    if retry_count < 3:
                        await asyncio.sleep(2 ** retry_count)
                        await send_webhook(webhook_id, event_type, payload, retry_count + 1)

    except Exception as e:
        log_entry["error"] = str(e)
        wh["failure_count"] += 1

        # Retry with exponential backoff
        if retry_count < 3:
            await asyncio.sleep(2 ** retry_count)
            await send_webhook(webhook_id, event_type, payload, retry_count + 1)

    webhook_logs.append(log_entry)

    # Keep only last 1000 logs
    while len(webhook_logs) > 1000:
        webhook_logs.pop(0)


async def trigger_webhooks(event_type: str, data: dict):
    """Dispara webhooks para um tipo de evento."""
    payload = {
        "event": event_type,
        "timestamp": datetime.now().isoformat(),
        "data": data
    }

    for webhook_id, wh in webhooks.items():
        if wh["enabled"] and event_type in wh["events"]:
            asyncio.create_task(send_webhook(webhook_id, event_type, payload))


def get_webhooks_html():
    """Retorna o HTML do painel de webhooks."""
    return '''
    <!-- Webhooks Panel (Issue #275) -->
    <div v-if="currentTab === 'webhooks'" class="webhooks-panel p-6">
        <div class="flex items-center justify-between mb-6">
            <div>
                <h2 class="text-2xl font-bold">Webhooks</h2>
                <p class="text-gray-500">Configure notificacoes para sistemas externos</p>
            </div>
            <button @click="showNewWebhookModal = true"
                    class="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg flex items-center gap-2">
                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/>
                </svg>
                Novo Webhook
            </button>
        </div>

        <!-- Webhooks List -->
        <div class="space-y-4">
            <div v-for="wh in webhooksList" :key="wh.id"
                 class="bg-white rounded-xl shadow p-4">
                <div class="flex items-center justify-between">
                    <div class="flex items-center gap-3">
                        <div :class="['w-3 h-3 rounded-full', wh.enabled ? 'bg-green-500' : 'bg-gray-300']"></div>
                        <div>
                            <h3 class="font-semibold">{{ wh.name }}</h3>
                            <p class="text-sm text-gray-500 truncate max-w-md">{{ wh.url }}</p>
                        </div>
                    </div>
                    <div class="flex items-center gap-2">
                        <span class="text-xs text-gray-400">
                            {{ wh.success_count }} ok / {{ wh.failure_count }} erro
                        </span>
                        <button @click="testWebhook(wh)"
                                class="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 rounded">
                            Testar
                        </button>
                        <button @click="editWebhook(wh)"
                                class="px-3 py-1 text-sm bg-blue-100 text-blue-700 hover:bg-blue-200 rounded">
                            Editar
                        </button>
                        <button @click="deleteWebhook(wh.id)"
                                class="px-3 py-1 text-sm text-red-600 hover:bg-red-50 rounded">
                            Excluir
                        </button>
                    </div>
                </div>
                <div class="mt-2 flex flex-wrap gap-1">
                    <span v-for="event in wh.events" :key="event"
                          class="px-2 py-0.5 bg-purple-100 text-purple-700 text-xs rounded">
                        {{ event }}
                    </span>
                </div>
            </div>

            <div v-if="webhooksList.length === 0" class="text-center py-12 text-gray-400">
                Nenhum webhook configurado
            </div>
        </div>

        <!-- Logs -->
        <div class="mt-8">
            <h3 class="font-semibold mb-4">Logs Recentes</h3>
            <div class="bg-white rounded-xl shadow overflow-hidden">
                <table class="w-full text-sm">
                    <thead class="bg-gray-50">
                        <tr>
                            <th class="px-4 py-2 text-left">Webhook</th>
                            <th class="px-4 py-2 text-left">Evento</th>
                            <th class="px-4 py-2 text-left">Status</th>
                            <th class="px-4 py-2 text-left">Data</th>
                        </tr>
                    </thead>
                    <tbody class="divide-y divide-gray-100">
                        <tr v-for="log in webhookLogs" :key="log.id">
                            <td class="px-4 py-2">{{ getWebhookName(log.webhook_id) }}</td>
                            <td class="px-4 py-2">{{ log.event_type }}</td>
                            <td class="px-4 py-2">
                                <span :class="log.status >= 200 && log.status < 300 ? 'text-green-600' : 'text-red-600'">
                                    {{ log.status || log.error || 'Erro' }}
                                </span>
                            </td>
                            <td class="px-4 py-2 text-gray-500">{{ formatDate(log.timestamp) }}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>
    </div>
    '''


def register_webhooks_outbound(app):
    """Registra os endpoints de webhooks no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Webhooks Outbound endpoints loaded: /api/webhooks/*")
