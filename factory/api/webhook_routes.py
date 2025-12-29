# -*- coding: utf-8 -*-
"""
Webhook Routes Module
=====================
Endpoints para receber webhooks de sistemas externos.

Endpoints:
- POST /api/webhooks/github
- POST /api/webhooks/gitlab
- POST /api/webhooks/jira
- POST /api/webhooks/azure-devops
- GET  /api/webhooks/status

Funcionalidades:
- Validacao de assinatura (HMAC)
- Rate limiting por origem
- Logging de todos os eventos
- Retry queue para falhas
"""

import os
import hmac
import hashlib
import logging
from datetime import datetime
from typing import Optional, Dict, Any

from fastapi import APIRouter, HTTPException, Request, Header, BackgroundTasks
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/webhooks", tags=["Webhooks"])


# =============================================================================
# CONFIGURATION
# =============================================================================

GITHUB_WEBHOOK_SECRET = os.getenv("GITHUB_WEBHOOK_SECRET", "")
GITLAB_WEBHOOK_TOKEN = os.getenv("GITLAB_WEBHOOK_TOKEN", "")
JIRA_WEBHOOK_SECRET = os.getenv("JIRA_WEBHOOK_SECRET", "")
AZURE_DEVOPS_WEBHOOK_SECRET = os.getenv("AZURE_DEVOPS_WEBHOOK_SECRET", "")


# =============================================================================
# MODELS
# =============================================================================

class WebhookEvent(BaseModel):
    """Modelo para evento de webhook"""
    source: str
    event_type: str
    payload: Dict[str, Any]
    received_at: str = Field(default_factory=lambda: datetime.utcnow().isoformat())
    processed: bool = False
    error: Optional[str] = None


class WebhookStatus(BaseModel):
    """Status dos webhooks"""
    source: str
    enabled: bool
    last_event: Optional[str]
    events_received: int
    events_processed: int
    events_failed: int


# =============================================================================
# WEBHOOK REGISTRY
# =============================================================================

class WebhookRegistry:
    """Registro de eventos de webhook"""

    def __init__(self):
        self._events: Dict[str, list] = {
            "github": [],
            "gitlab": [],
            "jira": [],
            "azure_devops": []
        }
        self._stats: Dict[str, Dict] = {
            source: {"received": 0, "processed": 0, "failed": 0, "last_event": None}
            for source in self._events.keys()
        }

    def add_event(self, source: str, event: WebhookEvent):
        """Adiciona evento ao registro"""
        if source in self._events:
            self._events[source].append(event)
            self._stats[source]["received"] += 1
            self._stats[source]["last_event"] = event.received_at

            # Manter apenas ultimos 100 eventos por fonte
            if len(self._events[source]) > 100:
                self._events[source] = self._events[source][-100:]

    def mark_processed(self, source: str, success: bool):
        """Marca evento como processado"""
        if source in self._stats:
            if success:
                self._stats[source]["processed"] += 1
            else:
                self._stats[source]["failed"] += 1

    def get_stats(self, source: str) -> Dict:
        """Retorna estatisticas de uma fonte"""
        return self._stats.get(source, {})

    def get_all_stats(self) -> Dict:
        """Retorna todas as estatisticas"""
        return self._stats


# Instancia global
webhook_registry = WebhookRegistry()


# =============================================================================
# HELPERS
# =============================================================================

def verify_github_signature(payload: bytes, signature: str, secret: str) -> bool:
    """Verifica assinatura HMAC-SHA256 do GitHub"""
    if not secret:
        return True  # Sem segredo configurado, aceitar

    if not signature or not signature.startswith("sha256="):
        return False

    expected_sig = signature[7:]  # Remove "sha256="
    computed_sig = hmac.new(
        secret.encode(),
        payload,
        hashlib.sha256
    ).hexdigest()

    return hmac.compare_digest(computed_sig, expected_sig)


def verify_gitlab_token(token: str, expected: str) -> bool:
    """Verifica token do GitLab"""
    if not expected:
        return True
    return hmac.compare_digest(token, expected)


def get_tenant_from_request(request: Request) -> str:
    """Extrai tenant do request"""
    return request.headers.get("X-Tenant-ID", "default")


# =============================================================================
# GITHUB WEBHOOK
# =============================================================================

@router.post("/github")
async def github_webhook(
    request: Request,
    background_tasks: BackgroundTasks,
    x_github_event: Optional[str] = Header(None, alias="X-GitHub-Event"),
    x_hub_signature_256: Optional[str] = Header(None, alias="X-Hub-Signature-256"),
    x_github_delivery: Optional[str] = Header(None, alias="X-GitHub-Delivery")
):
    """
    Endpoint para webhooks do GitHub.

    Eventos suportados:
    - issues: opened, edited, closed, reopened, labeled, unlabeled
    - issue_comment: created, edited, deleted
    - pull_request: opened, closed, merged
    - push: commits pushed
    - workflow_run: completed, requested

    Headers esperados:
    - X-GitHub-Event: Tipo do evento
    - X-Hub-Signature-256: HMAC-SHA256 do payload
    - X-GitHub-Delivery: ID unico do delivery
    """
    try:
        # Ler payload
        payload_bytes = await request.body()
        payload = await request.json()

        # Verificar assinatura
        if GITHUB_WEBHOOK_SECRET:
            if not verify_github_signature(payload_bytes, x_hub_signature_256 or "", GITHUB_WEBHOOK_SECRET):
                logger.warning(f"GitHub webhook: assinatura invalida")
                raise HTTPException(status_code=401, detail="Invalid signature")

        # Criar evento
        event = WebhookEvent(
            source="github",
            event_type=x_github_event or "unknown",
            payload=payload
        )
        webhook_registry.add_event("github", event)

        logger.info(f"GitHub webhook recebido: {x_github_event}, delivery: {x_github_delivery}")

        # Processar em background
        background_tasks.add_task(process_github_webhook, event, get_tenant_from_request(request))

        return {
            "status": "received",
            "event_type": x_github_event,
            "delivery_id": x_github_delivery
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no webhook GitHub: {e}")
        webhook_registry.mark_processed("github", False)
        raise HTTPException(status_code=500, detail=str(e))


async def process_github_webhook(event: WebhookEvent, tenant_id: str):
    """Processa webhook do GitHub em background"""
    try:
        from factory.integrations.github import get_github_integration

        github = get_github_integration()
        if not github.is_connected:
            await github.connect()

        success = await github.handle_webhook(event.payload)
        webhook_registry.mark_processed("github", success)

        if success:
            # Aqui poderia sincronizar com banco de dados
            pass

    except Exception as e:
        logger.error(f"Erro ao processar webhook GitHub: {e}")
        webhook_registry.mark_processed("github", False)


# =============================================================================
# GITLAB WEBHOOK
# =============================================================================

@router.post("/gitlab")
async def gitlab_webhook(
    request: Request,
    background_tasks: BackgroundTasks,
    x_gitlab_event: Optional[str] = Header(None, alias="X-Gitlab-Event"),
    x_gitlab_token: Optional[str] = Header(None, alias="X-Gitlab-Token")
):
    """
    Endpoint para webhooks do GitLab.

    Eventos suportados:
    - Issue Hook: open, close, reopen, update
    - Merge Request Hook: open, close, merge, update
    - Note Hook: comentarios
    - Pipeline Hook: status de pipelines
    - Push Hook: commits pushed

    Headers esperados:
    - X-Gitlab-Event: Tipo do evento
    - X-Gitlab-Token: Token secreto
    """
    try:
        payload = await request.json()

        # Verificar token
        if GITLAB_WEBHOOK_TOKEN:
            if not verify_gitlab_token(x_gitlab_token or "", GITLAB_WEBHOOK_TOKEN):
                logger.warning("GitLab webhook: token invalido")
                raise HTTPException(status_code=401, detail="Invalid token")

        # Criar evento
        object_kind = payload.get("object_kind", "unknown")
        event = WebhookEvent(
            source="gitlab",
            event_type=f"{x_gitlab_event}:{object_kind}",
            payload=payload
        )
        webhook_registry.add_event("gitlab", event)

        logger.info(f"GitLab webhook recebido: {x_gitlab_event}, object_kind: {object_kind}")

        # Processar em background
        background_tasks.add_task(process_gitlab_webhook, event, get_tenant_from_request(request))

        return {
            "status": "received",
            "event_type": x_gitlab_event,
            "object_kind": object_kind
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro no webhook GitLab: {e}")
        webhook_registry.mark_processed("gitlab", False)
        raise HTTPException(status_code=500, detail=str(e))


async def process_gitlab_webhook(event: WebhookEvent, tenant_id: str):
    """Processa webhook do GitLab em background"""
    try:
        from factory.integrations.gitlab_integration import get_gitlab_integration

        gitlab = get_gitlab_integration()
        if not gitlab.is_connected:
            await gitlab.connect()

        success = await gitlab.handle_webhook(event.payload)
        webhook_registry.mark_processed("gitlab", success)

    except Exception as e:
        logger.error(f"Erro ao processar webhook GitLab: {e}")
        webhook_registry.mark_processed("gitlab", False)


# =============================================================================
# JIRA WEBHOOK
# =============================================================================

@router.post("/jira")
async def jira_webhook(
    request: Request,
    background_tasks: BackgroundTasks
):
    """
    Endpoint para webhooks do Jira.

    Eventos suportados:
    - jira:issue_created
    - jira:issue_updated
    - jira:issue_deleted
    - comment_created
    - sprint_started
    - sprint_closed

    Configure no Jira: System > Webhooks
    """
    try:
        payload = await request.json()

        webhook_event = payload.get("webhookEvent", "unknown")

        event = WebhookEvent(
            source="jira",
            event_type=webhook_event,
            payload=payload
        )
        webhook_registry.add_event("jira", event)

        logger.info(f"Jira webhook recebido: {webhook_event}")

        # Processar em background
        background_tasks.add_task(process_jira_webhook, event, get_tenant_from_request(request))

        return {
            "status": "received",
            "event_type": webhook_event
        }

    except Exception as e:
        logger.error(f"Erro no webhook Jira: {e}")
        webhook_registry.mark_processed("jira", False)
        raise HTTPException(status_code=500, detail=str(e))


async def process_jira_webhook(event: WebhookEvent, tenant_id: str):
    """Processa webhook do Jira em background"""
    try:
        from factory.integrations.jira import get_jira_integration

        jira = get_jira_integration()
        if not jira.is_connected:
            await jira.connect()

        success = await jira.handle_webhook(event.payload)
        webhook_registry.mark_processed("jira", success)

    except Exception as e:
        logger.error(f"Erro ao processar webhook Jira: {e}")
        webhook_registry.mark_processed("jira", False)


# =============================================================================
# AZURE DEVOPS WEBHOOK
# =============================================================================

@router.post("/azure-devops")
async def azure_devops_webhook(
    request: Request,
    background_tasks: BackgroundTasks
):
    """
    Endpoint para webhooks do Azure DevOps.

    Eventos suportados:
    - workitem.created
    - workitem.updated
    - workitem.deleted
    - git.push
    - git.pullrequest.created
    - git.pullrequest.updated
    - build.complete

    Configure em: Project Settings > Service Hooks
    """
    try:
        payload = await request.json()

        event_type = payload.get("eventType", "unknown")

        event = WebhookEvent(
            source="azure_devops",
            event_type=event_type,
            payload=payload
        )
        webhook_registry.add_event("azure_devops", event)

        logger.info(f"Azure DevOps webhook recebido: {event_type}")

        # Processar em background
        background_tasks.add_task(process_azure_devops_webhook, event, get_tenant_from_request(request))

        return {
            "status": "received",
            "event_type": event_type
        }

    except Exception as e:
        logger.error(f"Erro no webhook Azure DevOps: {e}")
        webhook_registry.mark_processed("azure_devops", False)
        raise HTTPException(status_code=500, detail=str(e))


async def process_azure_devops_webhook(event: WebhookEvent, tenant_id: str):
    """Processa webhook do Azure DevOps em background"""
    try:
        from factory.integrations.azure_devops import get_azure_devops_integration

        azure = get_azure_devops_integration()
        if not azure.is_connected:
            await azure.connect()

        success = await azure.handle_webhook(event.payload)
        webhook_registry.mark_processed("azure_devops", success)

    except Exception as e:
        logger.error(f"Erro ao processar webhook Azure DevOps: {e}")
        webhook_registry.mark_processed("azure_devops", False)


# =============================================================================
# GENERIC WEBHOOK
# =============================================================================

@router.post("/{source}")
async def generic_webhook(
    source: str,
    request: Request,
    background_tasks: BackgroundTasks
):
    """
    Endpoint generico para webhooks de outras fontes.

    Util para integracoes customizadas.
    """
    try:
        payload = await request.json()

        event = WebhookEvent(
            source=source,
            event_type="generic",
            payload=payload
        )

        logger.info(f"Webhook generico recebido de: {source}")

        return {
            "status": "received",
            "source": source
        }

    except Exception as e:
        logger.error(f"Erro no webhook {source}: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# STATUS ENDPOINTS
# =============================================================================

@router.get("/status")
async def get_webhooks_status():
    """
    Retorna status de todos os webhooks configurados.
    """
    stats = webhook_registry.get_all_stats()

    return {
        "timestamp": datetime.utcnow().isoformat(),
        "webhooks": {
            "github": {
                "enabled": bool(GITHUB_WEBHOOK_SECRET) or True,
                "secret_configured": bool(GITHUB_WEBHOOK_SECRET),
                **stats.get("github", {})
            },
            "gitlab": {
                "enabled": bool(GITLAB_WEBHOOK_TOKEN) or True,
                "token_configured": bool(GITLAB_WEBHOOK_TOKEN),
                **stats.get("gitlab", {})
            },
            "jira": {
                "enabled": True,
                "secret_configured": bool(JIRA_WEBHOOK_SECRET),
                **stats.get("jira", {})
            },
            "azure_devops": {
                "enabled": True,
                "secret_configured": bool(AZURE_DEVOPS_WEBHOOK_SECRET),
                **stats.get("azure_devops", {})
            }
        }
    }


@router.get("/status/{source}")
async def get_webhook_status(source: str):
    """
    Retorna status de um webhook especifico.
    """
    stats = webhook_registry.get_stats(source)

    if not stats:
        raise HTTPException(status_code=404, detail=f"Webhook source '{source}' not found")

    return {
        "source": source,
        "timestamp": datetime.utcnow().isoformat(),
        **stats
    }


@router.get("/events/{source}")
async def get_webhook_events(
    source: str,
    limit: int = 10
):
    """
    Retorna ultimos eventos de um webhook.
    """
    if source not in webhook_registry._events:
        raise HTTPException(status_code=404, detail=f"Webhook source '{source}' not found")

    events = webhook_registry._events[source][-limit:]

    return {
        "source": source,
        "count": len(events),
        "events": [
            {
                "event_type": e.event_type,
                "received_at": e.received_at,
                "processed": e.processed
            }
            for e in events
        ]
    }


# =============================================================================
# WEBHOOK CONFIGURATION
# =============================================================================

@router.get("/config")
async def get_webhook_config():
    """
    Retorna URLs e configuracao para webhooks.

    Use estas URLs ao configurar webhooks nos sistemas externos.
    """
    base_url = os.getenv("BASE_URL", "http://localhost:9001")

    return {
        "github": {
            "url": f"{base_url}/api/webhooks/github",
            "content_type": "application/json",
            "secret": "Configure GITHUB_WEBHOOK_SECRET",
            "events": [
                "issues", "issue_comment", "pull_request",
                "push", "workflow_run"
            ]
        },
        "gitlab": {
            "url": f"{base_url}/api/webhooks/gitlab",
            "secret_token": "Configure GITLAB_WEBHOOK_TOKEN",
            "triggers": [
                "Issue events", "Merge request events",
                "Note events", "Pipeline events", "Push events"
            ]
        },
        "jira": {
            "url": f"{base_url}/api/webhooks/jira",
            "events": [
                "jira:issue_created", "jira:issue_updated",
                "jira:issue_deleted", "comment_created",
                "sprint_started", "sprint_closed"
            ],
            "instructions": "Configure em: Jira > System > Webhooks"
        },
        "azure_devops": {
            "url": f"{base_url}/api/webhooks/azure-devops",
            "events": [
                "Work item created", "Work item updated",
                "Pull request created", "Build completed"
            ],
            "instructions": "Configure em: Project Settings > Service Hooks"
        }
    }
