# -*- coding: utf-8 -*-
"""
Inbound Webhook Routes
======================
Endpoints para receber webhooks de sistemas externos.

Issue #363 - Terminal A

Endpoints:
- POST /webhooks/jira - Recebe webhooks do Jira
- POST /webhooks/github - Recebe webhooks do GitHub
- POST /webhooks/azure-devops - Recebe webhooks do Azure DevOps
- POST /webhooks/salesforce - Recebe webhooks do Salesforce
"""

import logging
from datetime import datetime
from typing import Any, Dict, Optional

from fastapi import APIRouter, Depends, Header, HTTPException, Request, status
from pydantic import BaseModel, Field

from .signature_verifier import (
    WebhookPlatform,
    get_webhook_registry,
    VerificationResult
)
from .github_webhook import parse_github_webhook
from .azure_webhook import parse_azure_devops_webhook
from .jira_webhook import parse_jira_webhook
from .salesforce_webhook import parse_salesforce_webhook
from .processor import (
    WebhookSource,
    get_processor,
    process_webhook_event
)

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/webhooks", tags=["Inbound Webhooks"])


# =============================================================================
# MODELS
# =============================================================================

class WebhookResponse(BaseModel):
    """Resposta padrao para webhooks"""
    success: bool = True
    event_id: Optional[str] = None
    message: str = "Webhook received"
    timestamp: datetime = Field(default_factory=datetime.utcnow)


class WebhookError(BaseModel):
    """Erro de webhook"""
    error: str
    code: str
    details: Optional[Dict[str, Any]] = None


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_tenant_id(
    x_tenant_id: Optional[str] = Header(None, alias="X-Tenant-ID")
) -> str:
    """Extrai tenant ID do header"""
    return x_tenant_id or "default"


async def verify_jira_signature(request: Request) -> VerificationResult:
    """Verifica assinatura do Jira"""
    registry = get_webhook_registry()
    tenant_id = request.headers.get("X-Tenant-ID", "default")

    verifier = registry.get(WebhookPlatform.JIRA, tenant_id)
    if not verifier:
        # Se nao tem verifier configurado, aceita sem validacao
        logger.warning(f"No Jira verifier for tenant {tenant_id}")
        return VerificationResult(
            is_valid=True,
            platform=WebhookPlatform.JIRA,
            tenant_id=tenant_id,
            message="No verifier configured, accepting"
        )

    return await verifier.verify(request)


async def verify_github_signature(request: Request) -> VerificationResult:
    """Verifica assinatura do GitHub"""
    registry = get_webhook_registry()
    tenant_id = request.headers.get("X-Tenant-ID", "default")

    verifier = registry.get(WebhookPlatform.GITHUB, tenant_id)
    if not verifier:
        logger.warning(f"No GitHub verifier for tenant {tenant_id}")
        return VerificationResult(
            is_valid=True,
            platform=WebhookPlatform.GITHUB,
            tenant_id=tenant_id,
            message="No verifier configured, accepting"
        )

    return await verifier.verify(request)


async def verify_azure_signature(request: Request) -> VerificationResult:
    """Verifica assinatura do Azure DevOps"""
    registry = get_webhook_registry()
    tenant_id = request.headers.get("X-Tenant-ID", "default")

    verifier = registry.get(WebhookPlatform.AZURE_DEVOPS, tenant_id)
    if not verifier:
        logger.warning(f"No Azure DevOps verifier for tenant {tenant_id}")
        return VerificationResult(
            is_valid=True,
            platform=WebhookPlatform.AZURE_DEVOPS,
            tenant_id=tenant_id,
            message="No verifier configured, accepting"
        )

    return await verifier.verify(request)


async def verify_salesforce_signature(request: Request) -> VerificationResult:
    """Verifica assinatura do Salesforce"""
    registry = get_webhook_registry()
    tenant_id = request.headers.get("X-Tenant-ID", "default")

    verifier = registry.get(WebhookPlatform.SALESFORCE, tenant_id)
    if not verifier:
        logger.warning(f"No Salesforce verifier for tenant {tenant_id}")
        return VerificationResult(
            is_valid=True,
            platform=WebhookPlatform.SALESFORCE,
            tenant_id=tenant_id,
            message="No verifier configured, accepting"
        )

    return await verifier.verify(request)


# =============================================================================
# JIRA WEBHOOK
# =============================================================================

@router.post(
    "/jira",
    response_model=WebhookResponse,
    responses={
        401: {"model": WebhookError, "description": "Signature invalid"},
        422: {"model": WebhookError, "description": "Invalid payload"}
    }
)
async def receive_jira_webhook(
    request: Request,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Recebe webhook do Jira.

    Eventos suportados:
    - jira:issue_created
    - jira:issue_updated
    - jira:issue_deleted
    - jira:sprint_started
    - jira:sprint_closed
    - jira:comment_created
    """
    # Verifica assinatura
    verification = await verify_jira_signature(request)
    if not verification.is_valid:
        logger.warning(f"Jira webhook signature invalid: {verification.message}")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={"error": "Invalid signature", "code": "INVALID_SIGNATURE"}
        )

    try:
        # Parse payload
        body = await request.body()
        payload = await request.json()
        parsed = parse_jira_webhook(payload)

        # Determina tipo de evento
        event_type = parsed.get("webhookEvent", "unknown")

        # Enfileira para processamento
        event = await process_webhook_event(
            source=WebhookSource.JIRA,
            event_type=event_type,
            payload=parsed,
            tenant_id=tenant_id
        )

        logger.info(f"Jira webhook received: {event_type} (event_id: {event.event_id})")

        return WebhookResponse(
            success=True,
            event_id=event.event_id,
            message=f"Jira event {event_type} received"
        )

    except Exception as e:
        logger.error(f"Error processing Jira webhook: {e}")
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail={"error": str(e), "code": "PROCESSING_ERROR"}
        )


# =============================================================================
# GITHUB WEBHOOK
# =============================================================================

@router.post(
    "/github",
    response_model=WebhookResponse,
    responses={
        401: {"model": WebhookError, "description": "Signature invalid"},
        422: {"model": WebhookError, "description": "Invalid payload"}
    }
)
async def receive_github_webhook(
    request: Request,
    x_github_event: Optional[str] = Header(None, alias="X-GitHub-Event"),
    x_github_delivery: Optional[str] = Header(None, alias="X-GitHub-Delivery"),
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Recebe webhook do GitHub.

    Eventos suportados:
    - push
    - pull_request
    - issues
    - workflow_run
    - check_run
    - release
    """
    # Verifica assinatura
    verification = await verify_github_signature(request)
    if not verification.is_valid:
        logger.warning(f"GitHub webhook signature invalid: {verification.message}")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={"error": "Invalid signature", "code": "INVALID_SIGNATURE"}
        )

    try:
        # Parse payload
        payload = await request.json()
        parsed = parse_github_webhook(payload, x_github_event or "unknown")

        # Determina tipo de evento
        event_type = x_github_event or "unknown"
        action = payload.get("action")
        if action:
            event_type = f"{event_type}.{action}"

        # Enfileira para processamento
        event = await process_webhook_event(
            source=WebhookSource.GITHUB,
            event_type=event_type,
            payload=parsed,
            tenant_id=tenant_id
        )

        logger.info(f"GitHub webhook received: {event_type} (delivery: {x_github_delivery})")

        return WebhookResponse(
            success=True,
            event_id=event.event_id,
            message=f"GitHub event {event_type} received"
        )

    except Exception as e:
        logger.error(f"Error processing GitHub webhook: {e}")
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail={"error": str(e), "code": "PROCESSING_ERROR"}
        )


# =============================================================================
# AZURE DEVOPS WEBHOOK
# =============================================================================

@router.post(
    "/azure-devops",
    response_model=WebhookResponse,
    responses={
        401: {"model": WebhookError, "description": "Signature invalid"},
        422: {"model": WebhookError, "description": "Invalid payload"}
    }
)
async def receive_azure_devops_webhook(
    request: Request,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Recebe webhook do Azure DevOps.

    Eventos suportados:
    - workitem.created
    - workitem.updated
    - workitem.deleted
    - build.complete
    - release.deployment.completed
    - git.push
    - git.pullrequest.created
    """
    # Verifica assinatura
    verification = await verify_azure_signature(request)
    if not verification.is_valid:
        logger.warning(f"Azure DevOps webhook signature invalid: {verification.message}")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={"error": "Invalid signature", "code": "INVALID_SIGNATURE"}
        )

    try:
        # Parse payload
        payload = await request.json()
        parsed = parse_azure_devops_webhook(payload)

        # Determina tipo de evento
        event_type = payload.get("eventType", "unknown")

        # Enfileira para processamento
        event = await process_webhook_event(
            source=WebhookSource.AZURE_DEVOPS,
            event_type=event_type,
            payload=parsed,
            tenant_id=tenant_id
        )

        logger.info(f"Azure DevOps webhook received: {event_type}")

        return WebhookResponse(
            success=True,
            event_id=event.event_id,
            message=f"Azure DevOps event {event_type} received"
        )

    except Exception as e:
        logger.error(f"Error processing Azure DevOps webhook: {e}")
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail={"error": str(e), "code": "PROCESSING_ERROR"}
        )


# =============================================================================
# SALESFORCE WEBHOOK
# =============================================================================

@router.post(
    "/salesforce",
    response_model=WebhookResponse,
    responses={
        401: {"model": WebhookError, "description": "Signature invalid"},
        422: {"model": WebhookError, "description": "Invalid payload"}
    }
)
async def receive_salesforce_webhook(
    request: Request,
    tenant_id: str = Depends(get_tenant_id)
):
    """
    Recebe webhook do Salesforce.

    Tipos suportados:
    - Outbound Messages
    - Platform Events
    - Change Data Capture

    Eventos:
    - case.created
    - case.updated
    - opportunity.won
    - opportunity.lost
    """
    # Verifica assinatura
    verification = await verify_salesforce_signature(request)
    if not verification.is_valid:
        logger.warning(f"Salesforce webhook signature invalid: {verification.message}")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={"error": "Invalid signature", "code": "INVALID_SIGNATURE"}
        )

    try:
        # Determina tipo de conteudo
        content_type = request.headers.get("content-type", "")

        if "xml" in content_type.lower():
            # Outbound Message (SOAP/XML)
            body = await request.body()
            parsed = parse_salesforce_webhook(body.decode(), is_xml=True)
            event_type = "outbound_message"
        else:
            # Platform Event ou CDC (JSON)
            payload = await request.json()
            parsed = parse_salesforce_webhook(payload, is_xml=False)
            event_type = payload.get("event", {}).get("type", "platform_event")

        # Enfileira para processamento
        event = await process_webhook_event(
            source=WebhookSource.SALESFORCE,
            event_type=event_type,
            payload=parsed,
            tenant_id=tenant_id
        )

        logger.info(f"Salesforce webhook received: {event_type}")

        return WebhookResponse(
            success=True,
            event_id=event.event_id,
            message=f"Salesforce event {event_type} received"
        )

    except Exception as e:
        logger.error(f"Error processing Salesforce webhook: {e}")
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail={"error": str(e), "code": "PROCESSING_ERROR"}
        )


# =============================================================================
# STATUS ENDPOINT
# =============================================================================

@router.get("/status")
async def get_webhook_status():
    """
    Retorna status do processador de webhooks.
    """
    processor = get_processor()
    return {
        "status": "running" if processor._running else "stopped",
        "stats": processor.get_stats(),
        "recent_events": processor.get_event_log(limit=10)
    }


@router.post("/start")
async def start_processor():
    """
    Inicia o processador de webhooks.
    """
    processor = get_processor()
    await processor.start()
    return {"status": "started", "stats": processor.get_stats()}


@router.post("/stop")
async def stop_processor():
    """
    Para o processador de webhooks.
    """
    processor = get_processor()
    await processor.stop()
    return {"status": "stopped", "stats": processor.get_stats()}
