# -*- coding: utf-8 -*-
"""
Sistema de Webhooks - Fabrica de Agentes v6.5
=============================================

Sistema de notificacoes por webhook para eventos da plataforma.
Suporta:
- Assinatura de multiplos eventos
- Assinatura HMAC para verificacao
- Retry automatico com backoff exponencial
- Historico de entregas para debugging
"""
import hashlib
import hmac
import json
import time
import uuid
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Any
from concurrent.futures import ThreadPoolExecutor
import threading

import requests

from factory.database.api_models import (
    Webhook, WebhookDelivery, WebhookEventType, WebhookStatus
)


# =============================================================================
# CONFIGURACAO
# =============================================================================

WEBHOOK_TIMEOUT = 10  # segundos
WEBHOOK_MAX_RETRIES = 3
WEBHOOK_RETRY_DELAYS = [60, 300, 1800]  # 1min, 5min, 30min
WEBHOOK_MAX_PAYLOAD_SIZE = 1024 * 1024  # 1MB


# =============================================================================
# DISPATCHER DE WEBHOOKS
# =============================================================================

class WebhookDispatcher:
    """
    Dispatcher de webhooks.

    Gerencia a entrega de eventos para webhooks registrados.
    Usa thread pool para entregas assincronas.
    """

    def __init__(self, max_workers: int = 5):
        """
        Inicializa o dispatcher.

        Args:
            max_workers: Numero maximo de threads para entregas paralelas
        """
        self.executor = ThreadPoolExecutor(max_workers=max_workers)
        self._session = requests.Session()
        self._session.headers.update({
            "User-Agent": "FabricaWebhooks/1.0",
            "Content-Type": "application/json",
        })

    def dispatch(
        self,
        event_type: str,
        data: Dict,
        project_id: str = None,
    ):
        """
        Despacha evento para todos os webhooks assinados.

        Args:
            event_type: Tipo do evento (ex: story.completed)
            data: Dados do evento
            project_id: ID do projeto (para filtros)
        """
        # Buscar webhooks assinados
        webhooks = self._get_subscribed_webhooks(event_type, project_id)

        if not webhooks:
            return

        # Preparar payload
        payload = self._build_payload(event_type, data)

        # Despachar para cada webhook
        for webhook in webhooks:
            self.executor.submit(
                self._deliver_webhook,
                webhook,
                payload,
                event_type
            )

    def dispatch_sync(
        self,
        event_type: str,
        data: Dict,
        project_id: str = None,
    ) -> List[Dict]:
        """
        Despacha evento de forma sincrona (para testes).

        Returns:
            Lista de resultados das entregas
        """
        webhooks = self._get_subscribed_webhooks(event_type, project_id)
        results = []

        payload = self._build_payload(event_type, data)

        for webhook in webhooks:
            result = self._deliver_webhook(webhook, payload, event_type)
            results.append(result)

        return results

    def _get_subscribed_webhooks(
        self,
        event_type: str,
        project_id: str = None
    ) -> List[Webhook]:
        """Busca webhooks assinados para o evento"""
        try:
            from factory.database.connection import SessionLocal

            db = SessionLocal()
            try:
                query = db.query(Webhook).filter(
                    Webhook.status == WebhookStatus.ACTIVE.value
                )

                webhooks = query.all()

                # Filtrar por evento e projeto
                result = []
                for wh in webhooks:
                    if wh.should_trigger(event_type, project_id):
                        result.append(wh)

                return result
            finally:
                db.close()
        except Exception as e:
            print(f"[Webhooks] Erro ao buscar webhooks: {e}")
            return []

    def _build_payload(self, event_type: str, data: Dict) -> Dict:
        """Constroi payload do webhook"""
        return {
            "id": f"evt_{uuid.uuid4().hex[:12]}",
            "event": event_type,
            "created_at": datetime.utcnow().isoformat() + "Z",
            "data": data,
            "api_version": "2024-01-01",
        }

    def _deliver_webhook(
        self,
        webhook: Webhook,
        payload: Dict,
        event_type: str,
        attempt: int = 1
    ) -> Dict:
        """
        Entrega webhook para um destino.

        Args:
            webhook: Webhook de destino
            payload: Payload a enviar
            event_type: Tipo do evento
            attempt: Tentativa atual (para retry)

        Returns:
            Dict com resultado da entrega
        """
        delivery_id = f"dlv_{uuid.uuid4().hex[:12]}"
        start_time = time.time()

        try:
            # Serializar payload
            body = json.dumps(payload, ensure_ascii=False, default=str)

            if len(body) > WEBHOOK_MAX_PAYLOAD_SIZE:
                raise ValueError(f"Payload excede tamanho maximo ({len(body)} bytes)")

            # Calcular assinatura HMAC
            headers = self._build_headers(webhook, body, delivery_id)

            # Enviar request
            response = self._session.post(
                webhook.url,
                data=body,
                headers=headers,
                timeout=WEBHOOK_TIMEOUT,
            )

            response_time_ms = int((time.time() - start_time) * 1000)

            # Verificar sucesso (2xx)
            success = 200 <= response.status_code < 300

            # Registrar entrega
            self._record_delivery(
                webhook=webhook,
                delivery_id=delivery_id,
                event_type=event_type,
                event_id=payload.get("data", {}).get("id"),
                payload=payload,
                success=success,
                status_code=response.status_code,
                response_body=response.text[:1000] if response.text else None,
                response_time_ms=response_time_ms,
                attempt=attempt,
            )

            if success:
                self._update_webhook_success(webhook)
            else:
                self._handle_failure(
                    webhook, payload, event_type, attempt,
                    f"HTTP {response.status_code}: {response.text[:200]}"
                )

            return {
                "delivery_id": delivery_id,
                "webhook_id": webhook.webhook_id,
                "success": success,
                "status_code": response.status_code,
                "response_time_ms": response_time_ms,
            }

        except requests.exceptions.Timeout:
            return self._handle_failure(
                webhook, payload, event_type, attempt,
                "Timeout na conexao",
                delivery_id=delivery_id
            )

        except requests.exceptions.ConnectionError as e:
            return self._handle_failure(
                webhook, payload, event_type, attempt,
                f"Erro de conexao: {str(e)[:100]}",
                delivery_id=delivery_id
            )

        except Exception as e:
            return self._handle_failure(
                webhook, payload, event_type, attempt,
                f"Erro: {str(e)[:200]}",
                delivery_id=delivery_id
            )

    def _build_headers(
        self,
        webhook: Webhook,
        body: str,
        delivery_id: str
    ) -> Dict[str, str]:
        """Constroi headers com assinatura HMAC"""
        timestamp = str(int(time.time()))

        headers = {
            "X-Fabrica-Delivery-ID": delivery_id,
            "X-Fabrica-Timestamp": timestamp,
        }

        # Adicionar headers customizados
        if webhook.custom_headers:
            headers.update(webhook.custom_headers)

        # Calcular assinatura se secret definido
        if webhook.secret:
            signature = self._compute_signature(webhook.secret, timestamp, body)
            headers["X-Fabrica-Signature"] = signature
            headers["X-Fabrica-Signature-256"] = signature  # Alias

        return headers

    def _compute_signature(
        self,
        secret: str,
        timestamp: str,
        body: str
    ) -> str:
        """
        Computa assinatura HMAC-SHA256.

        Formato: sha256=<hex_digest>
        Payload assinado: timestamp.body
        """
        payload_to_sign = f"{timestamp}.{body}"
        signature = hmac.new(
            secret.encode("utf-8"),
            payload_to_sign.encode("utf-8"),
            hashlib.sha256
        ).hexdigest()
        return f"sha256={signature}"

    def _handle_failure(
        self,
        webhook: Webhook,
        payload: Dict,
        event_type: str,
        attempt: int,
        error_message: str,
        delivery_id: str = None
    ) -> Dict:
        """Trata falha na entrega e agenda retry se necessario"""
        delivery_id = delivery_id or f"dlv_{uuid.uuid4().hex[:12]}"

        # Registrar falha
        self._record_delivery(
            webhook=webhook,
            delivery_id=delivery_id,
            event_type=event_type,
            event_id=payload.get("data", {}).get("id"),
            payload=payload,
            success=False,
            status_code=None,
            error_message=error_message,
            attempt=attempt,
        )

        self._update_webhook_failure(webhook, error_message)

        # Verificar se deve retry
        if attempt < WEBHOOK_MAX_RETRIES:
            delay = WEBHOOK_RETRY_DELAYS[attempt - 1] if attempt <= len(WEBHOOK_RETRY_DELAYS) else WEBHOOK_RETRY_DELAYS[-1]

            # Agendar retry
            threading.Timer(
                delay,
                lambda: self._deliver_webhook(webhook, payload, event_type, attempt + 1)
            ).start()

        return {
            "delivery_id": delivery_id,
            "webhook_id": webhook.webhook_id,
            "success": False,
            "error": error_message,
            "will_retry": attempt < WEBHOOK_MAX_RETRIES,
        }

    def _record_delivery(
        self,
        webhook: Webhook,
        delivery_id: str,
        event_type: str,
        event_id: str,
        payload: Dict,
        success: bool,
        status_code: int = None,
        response_body: str = None,
        response_time_ms: int = None,
        error_message: str = None,
        attempt: int = 1,
    ):
        """Registra entrega no banco de dados"""
        try:
            from factory.database.connection import SessionLocal

            db = SessionLocal()
            try:
                delivery = WebhookDelivery(
                    delivery_id=delivery_id,
                    webhook_id=webhook.webhook_id,
                    event_type=event_type,
                    event_id=event_id,
                    payload=payload,
                    success=success,
                    status_code=status_code,
                    response_body=response_body,
                    response_time_ms=response_time_ms,
                    error_message=error_message,
                    attempt=attempt,
                )
                db.add(delivery)
                db.commit()
            finally:
                db.close()
        except Exception as e:
            print(f"[Webhooks] Erro ao registrar delivery: {e}")

    def _update_webhook_success(self, webhook: Webhook):
        """Atualiza webhook apos entrega bem-sucedida"""
        try:
            from factory.database.connection import SessionLocal

            db = SessionLocal()
            try:
                wh = db.query(Webhook).filter(
                    Webhook.webhook_id == webhook.webhook_id
                ).first()
                if wh:
                    wh.record_success()
                    db.commit()
            finally:
                db.close()
        except Exception as e:
            print(f"[Webhooks] Erro ao atualizar webhook: {e}")

    def _update_webhook_failure(self, webhook: Webhook, reason: str):
        """Atualiza webhook apos falha"""
        try:
            from factory.database.connection import SessionLocal

            db = SessionLocal()
            try:
                wh = db.query(Webhook).filter(
                    Webhook.webhook_id == webhook.webhook_id
                ).first()
                if wh:
                    wh.record_failure(reason)
                    db.commit()
            finally:
                db.close()
        except Exception as e:
            print(f"[Webhooks] Erro ao atualizar webhook: {e}")

    def shutdown(self, wait: bool = True):
        """Desliga o dispatcher"""
        self.executor.shutdown(wait=wait)


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_dispatcher: Optional[WebhookDispatcher] = None


def get_webhook_dispatcher() -> WebhookDispatcher:
    """Obtem instancia do dispatcher"""
    global _dispatcher
    if _dispatcher is None:
        _dispatcher = WebhookDispatcher()
    return _dispatcher


# =============================================================================
# FUNCOES DE CONVENIENCIA
# =============================================================================

def dispatch_event(event_type: str, data: Dict, project_id: str = None):
    """
    Despacha evento para webhooks assinados.

    Usage:
        from factory.api.webhooks import dispatch_event

        # Quando uma story e completada
        dispatch_event(
            "story.completed",
            {
                "story_id": "STR-0001",
                "title": "Login com Google",
                "files_created": ["auth.py", "login.html"],
                "duration_seconds": 342
            },
            project_id="PRJ-001"
        )
    """
    dispatcher = get_webhook_dispatcher()
    dispatcher.dispatch(event_type, data, project_id)


def verify_webhook_signature(
    payload: bytes,
    signature: str,
    secret: str,
    timestamp: str = None
) -> bool:
    """
    Verifica assinatura de webhook (para uso no receptor).

    Usage:
        # No seu endpoint que recebe webhooks:
        @app.post("/webhook")
        def receive_webhook(request):
            signature = request.headers.get("X-Fabrica-Signature")
            timestamp = request.headers.get("X-Fabrica-Timestamp")

            if not verify_webhook_signature(
                request.body,
                signature,
                YOUR_WEBHOOK_SECRET,
                timestamp
            ):
                raise HTTPError(401, "Assinatura invalida")

            # Processar evento...
    """
    if not signature or not signature.startswith("sha256="):
        return False

    expected_signature = signature[7:]  # Remove "sha256="

    if timestamp:
        payload_to_sign = f"{timestamp}.{payload.decode('utf-8')}"
    else:
        payload_to_sign = payload.decode("utf-8")

    computed = hmac.new(
        secret.encode("utf-8"),
        payload_to_sign.encode("utf-8"),
        hashlib.sha256
    ).hexdigest()

    return hmac.compare_digest(computed, expected_signature)


# =============================================================================
# EVENTOS DISPONIVEIS
# =============================================================================

AVAILABLE_EVENTS = {
    # Projetos
    "project.created": "Quando um projeto e criado",
    "project.updated": "Quando um projeto e atualizado",
    "project.completed": "Quando um projeto e marcado como completo",
    "project.deleted": "Quando um projeto e deletado",

    # Stories
    "story.created": "Quando uma story e criada",
    "story.updated": "Quando uma story e atualizada",
    "story.status_changed": "Quando o status de uma story muda",
    "story.completed": "Quando uma story e completada",

    # Jobs
    "job.created": "Quando um job e criado",
    "job.started": "Quando um job comeca a executar",
    "job.completed": "Quando um job e completado com sucesso",
    "job.failed": "Quando um job falha",

    # Tasks
    "task.created": "Quando uma task e criada",
    "task.completed": "Quando uma task e completada",
}


def get_available_events() -> Dict[str, str]:
    """Retorna dicionario de eventos disponiveis"""
    return AVAILABLE_EVENTS.copy()
