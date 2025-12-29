"""
Webhooks Routes - API v1
========================

Endpoints para gestao de webhooks e callbacks.

Endpoints:
- /api/v1/webhooks/ - CRUD de webhooks
- /api/v1/webhooks/events - Tipos de eventos
- /api/v1/webhooks/deliveries - Historico de entregas
"""

from datetime import datetime, timedelta
from typing import List, Optional, Dict, Any
import uuid
import hashlib
import hmac
import json

from fastapi import APIRouter, Depends, Query, Request, HTTPException, status, Header
from pydantic import BaseModel, Field, HttpUrl
from sqlalchemy.orm import Session

from .schemas import APIResponse, APIListResponse, PaginationMeta, RequestMeta, ErrorCodes
from factory.api.pagination import CursorPagination
from factory.database.connection import SessionLocal

router = APIRouter()
paginator = CursorPagination()


# =============================================================================
# MODELS
# =============================================================================

class WebhookCreate(BaseModel):
    """Criar webhook"""
    url: str = Field(..., description="URL de destino")
    events: List[str] = Field(..., min_items=1, description="Eventos a escutar")
    secret: Optional[str] = Field(None, description="Secret para assinatura HMAC")
    description: Optional[str] = None
    headers: Optional[Dict[str, str]] = Field(default_factory=dict, description="Headers customizados")
    active: bool = True


class WebhookUpdate(BaseModel):
    """Atualizar webhook"""
    url: Optional[str] = None
    events: Optional[List[str]] = None
    secret: Optional[str] = None
    description: Optional[str] = None
    headers: Optional[Dict[str, str]] = None
    active: Optional[bool] = None


class WebhookEvent(BaseModel):
    """Evento de webhook"""
    event_type: str
    payload: Dict[str, Any]
    timestamp: datetime = Field(default_factory=datetime.utcnow)


# Tipos de eventos disponiveis
WEBHOOK_EVENTS = {
    # Stories
    "story.created": "Story criada",
    "story.updated": "Story atualizada",
    "story.deleted": "Story deletada",
    "story.status_changed": "Status da story alterado",
    "story.completed": "Story concluida",

    # Tasks
    "task.created": "Task criada",
    "task.updated": "Task atualizada",
    "task.completed": "Task concluida",

    # Projects
    "project.created": "Projeto criado",
    "project.updated": "Projeto atualizado",
    "project.completed": "Projeto concluido",

    # Jobs
    "job.queued": "Job adicionado a fila",
    "job.started": "Job iniciado",
    "job.completed": "Job concluido",
    "job.failed": "Job falhou",

    # System
    "system.health_check": "Health check",
    "system.error": "Erro no sistema"
}


# Store em memoria (substituir por banco em producao)
_webhooks: Dict[str, dict] = {}
_deliveries: List[dict] = []


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_request_meta(request: Request) -> RequestMeta:
    return RequestMeta(
        request_id=request.headers.get("X-Request-ID", str(uuid.uuid4())),
        timestamp=datetime.utcnow(),
        tenant_id=request.headers.get("X-Tenant-ID"),
        api_version="v1"
    )


def generate_signature(payload: str, secret: str) -> str:
    """Gera assinatura HMAC-SHA256"""
    return hmac.new(
        secret.encode(),
        payload.encode(),
        hashlib.sha256
    ).hexdigest()


# =============================================================================
# WEBHOOKS CRUD
# =============================================================================

@router.get("/", response_model=APIListResponse, tags=["Webhooks"])
async def list_webhooks(
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    active: Optional[bool] = Query(None),
    event: Optional[str] = Query(None, description="Filtrar por tipo de evento")
):
    """
    Lista webhooks configurados.
    """
    webhooks = list(_webhooks.values())

    if active is not None:
        webhooks = [w for w in webhooks if w["active"] == active]
    if event:
        webhooks = [w for w in webhooks if event in w["events"]]

    # Paginar em memoria
    result = paginator.paginate_list(
        items=webhooks,
        cursor=cursor,
        limit=limit
    )

    return APIListResponse(
        data=result.items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            total_count=result.total_count,
            limit=limit
        )
    )


@router.post("/", response_model=APIResponse, status_code=status.HTTP_201_CREATED, tags=["Webhooks"])
async def create_webhook(
    webhook_data: WebhookCreate,
    request: Request
):
    """
    Cria novo webhook.
    """
    # Validar eventos
    invalid_events = [e for e in webhook_data.events if e not in WEBHOOK_EVENTS]
    if invalid_events:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={
                "error_code": ErrorCodes.VALIDATION_ERROR,
                "message": f"Eventos invalidos: {', '.join(invalid_events)}",
                "valid_events": list(WEBHOOK_EVENTS.keys())
            }
        )

    webhook_id = f"wh_{uuid.uuid4().hex[:12]}"

    webhook = {
        "id": webhook_id,
        "url": webhook_data.url,
        "events": webhook_data.events,
        "secret": webhook_data.secret,
        "description": webhook_data.description,
        "headers": webhook_data.headers or {},
        "active": webhook_data.active,
        "created_at": datetime.utcnow().isoformat(),
        "updated_at": None,
        "last_triggered": None,
        "delivery_count": 0,
        "failure_count": 0
    }

    _webhooks[webhook_id] = webhook

    # Nao retornar secret na resposta
    response_data = {**webhook}
    if response_data.get("secret"):
        response_data["secret"] = "***"

    return APIResponse(
        data=response_data,
        meta=get_request_meta(request),
        message="Webhook criado com sucesso"
    )


@router.get("/{webhook_id}", response_model=APIResponse, tags=["Webhooks"])
async def get_webhook(
    webhook_id: str,
    request: Request
):
    """
    Busca webhook por ID.
    """
    webhook = _webhooks.get(webhook_id)
    if not webhook:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Webhook {webhook_id} nao encontrado"
            }
        )

    response_data = {**webhook}
    if response_data.get("secret"):
        response_data["secret"] = "***"

    return APIResponse(
        data=response_data,
        meta=get_request_meta(request)
    )


@router.put("/{webhook_id}", response_model=APIResponse, tags=["Webhooks"])
async def update_webhook(
    webhook_id: str,
    webhook_data: WebhookUpdate,
    request: Request
):
    """
    Atualiza webhook.
    """
    webhook = _webhooks.get(webhook_id)
    if not webhook:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Webhook {webhook_id} nao encontrado"
            }
        )

    # Validar eventos se fornecidos
    if webhook_data.events:
        invalid_events = [e for e in webhook_data.events if e not in WEBHOOK_EVENTS]
        if invalid_events:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail={
                    "error_code": ErrorCodes.VALIDATION_ERROR,
                    "message": f"Eventos invalidos: {', '.join(invalid_events)}"
                }
            )

    update_data = webhook_data.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        webhook[field] = value

    webhook["updated_at"] = datetime.utcnow().isoformat()

    response_data = {**webhook}
    if response_data.get("secret"):
        response_data["secret"] = "***"

    return APIResponse(
        data=response_data,
        meta=get_request_meta(request),
        message="Webhook atualizado com sucesso"
    )


@router.delete("/{webhook_id}", response_model=APIResponse, tags=["Webhooks"])
async def delete_webhook(
    webhook_id: str,
    request: Request
):
    """
    Remove webhook.
    """
    if webhook_id not in _webhooks:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Webhook {webhook_id} nao encontrado"
            }
        )

    del _webhooks[webhook_id]

    return APIResponse(
        data={"id": webhook_id, "deleted": True},
        meta=get_request_meta(request),
        message="Webhook removido com sucesso"
    )


# =============================================================================
# EVENTS
# =============================================================================

@router.get("/events", response_model=APIResponse, tags=["Events"])
async def list_event_types(request: Request):
    """
    Lista tipos de eventos disponiveis para webhooks.
    """
    events = [
        {"event": event, "description": desc}
        for event, desc in WEBHOOK_EVENTS.items()
    ]

    return APIResponse(
        data={
            "events": events,
            "total": len(events)
        },
        meta=get_request_meta(request)
    )


@router.post("/{webhook_id}/test", response_model=APIResponse, tags=["Webhooks"])
async def test_webhook(
    webhook_id: str,
    request: Request
):
    """
    Envia evento de teste para o webhook.
    """
    webhook = _webhooks.get(webhook_id)
    if not webhook:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Webhook {webhook_id} nao encontrado"
            }
        )

    # Criar payload de teste
    test_payload = {
        "event": "system.health_check",
        "webhook_id": webhook_id,
        "timestamp": datetime.utcnow().isoformat(),
        "data": {
            "message": "Este e um evento de teste",
            "test": True
        }
    }

    # Simular entrega
    delivery = {
        "id": f"del_{uuid.uuid4().hex[:12]}",
        "webhook_id": webhook_id,
        "event": "system.health_check",
        "url": webhook["url"],
        "payload": test_payload,
        "status": "delivered",
        "status_code": 200,
        "response_time_ms": 150,
        "delivered_at": datetime.utcnow().isoformat()
    }

    _deliveries.append(delivery)
    webhook["delivery_count"] = webhook.get("delivery_count", 0) + 1
    webhook["last_triggered"] = datetime.utcnow().isoformat()

    return APIResponse(
        data={
            "webhook_id": webhook_id,
            "delivery_id": delivery["id"],
            "status": "delivered",
            "message": "Evento de teste enviado com sucesso"
        },
        meta=get_request_meta(request)
    )


# =============================================================================
# DELIVERIES
# =============================================================================

@router.get("/{webhook_id}/deliveries", response_model=APIListResponse, tags=["Deliveries"])
async def list_webhook_deliveries(
    webhook_id: str,
    request: Request,
    cursor: Optional[str] = Query(None),
    limit: int = Query(20, ge=1, le=100),
    status_filter: Optional[str] = Query(None, alias="status", description="Filtrar por status")
):
    """
    Lista entregas de um webhook.
    """
    webhook = _webhooks.get(webhook_id)
    if not webhook:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Webhook {webhook_id} nao encontrado"
            }
        )

    # Filtrar entregas
    deliveries = [d for d in _deliveries if d["webhook_id"] == webhook_id]

    if status_filter:
        deliveries = [d for d in deliveries if d["status"] == status_filter]

    # Ordenar por data (mais recente primeiro)
    deliveries.sort(key=lambda x: x["delivered_at"], reverse=True)

    result = paginator.paginate_list(
        items=deliveries,
        cursor=cursor,
        limit=limit
    )

    return APIListResponse(
        data=result.items,
        meta=get_request_meta(request),
        pagination=PaginationMeta(
            cursor=result.cursor,
            has_more=result.has_more,
            total_count=result.total_count,
            limit=limit
        )
    )


@router.get("/deliveries/{delivery_id}", response_model=APIResponse, tags=["Deliveries"])
async def get_delivery(
    delivery_id: str,
    request: Request
):
    """
    Busca detalhes de uma entrega.
    """
    delivery = next((d for d in _deliveries if d["id"] == delivery_id), None)
    if not delivery:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Entrega {delivery_id} nao encontrada"
            }
        )

    return APIResponse(
        data=delivery,
        meta=get_request_meta(request)
    )


@router.post("/deliveries/{delivery_id}/retry", response_model=APIResponse, tags=["Deliveries"])
async def retry_delivery(
    delivery_id: str,
    request: Request
):
    """
    Reenvia uma entrega falha.
    """
    delivery = next((d for d in _deliveries if d["id"] == delivery_id), None)
    if not delivery:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail={
                "error_code": ErrorCodes.RESOURCE_NOT_FOUND,
                "message": f"Entrega {delivery_id} nao encontrada"
            }
        )

    if delivery["status"] == "delivered":
        return APIResponse(
            data=delivery,
            meta=get_request_meta(request),
            message="Entrega ja foi bem sucedida, nao precisa reenviar"
        )

    # Simular reenvio
    delivery["status"] = "delivered"
    delivery["status_code"] = 200
    delivery["retry_count"] = delivery.get("retry_count", 0) + 1
    delivery["last_retry"] = datetime.utcnow().isoformat()

    return APIResponse(
        data=delivery,
        meta=get_request_meta(request),
        message="Entrega reenviada com sucesso"
    )


# =============================================================================
# TRIGGER (para uso interno)
# =============================================================================

async def trigger_webhooks(event_type: str, payload: Dict[str, Any]):
    """
    Dispara webhooks para um evento.

    Uso interno - chamado por outros modulos quando eventos ocorrem.

    Args:
        event_type: Tipo do evento (ex: story.created)
        payload: Dados do evento
    """
    if event_type not in WEBHOOK_EVENTS:
        return

    # Encontrar webhooks que escutam este evento
    for webhook_id, webhook in _webhooks.items():
        if not webhook["active"]:
            continue

        if event_type not in webhook["events"]:
            continue

        # Criar payload completo
        full_payload = {
            "event": event_type,
            "timestamp": datetime.utcnow().isoformat(),
            "webhook_id": webhook_id,
            "data": payload
        }

        # Gerar assinatura se tiver secret
        signature = None
        if webhook.get("secret"):
            payload_str = json.dumps(full_payload, default=str)
            signature = generate_signature(payload_str, webhook["secret"])

        # Registrar entrega
        delivery = {
            "id": f"del_{uuid.uuid4().hex[:12]}",
            "webhook_id": webhook_id,
            "event": event_type,
            "url": webhook["url"],
            "payload": full_payload,
            "signature": signature,
            "status": "pending",
            "created_at": datetime.utcnow().isoformat()
        }

        _deliveries.append(delivery)

        # Em producao, fazer chamada HTTP real aqui
        # Por agora, simular sucesso
        delivery["status"] = "delivered"
        delivery["status_code"] = 200
        delivery["response_time_ms"] = 100
        delivery["delivered_at"] = datetime.utcnow().isoformat()

        webhook["delivery_count"] = webhook.get("delivery_count", 0) + 1
        webhook["last_triggered"] = datetime.utcnow().isoformat()
