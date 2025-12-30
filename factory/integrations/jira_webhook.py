# -*- coding: utf-8 -*-
"""
Jira Webhook Validation Module
==============================
Funcoes para validacao de webhooks do Jira usando HMAC-SHA256 com isolamento por tenant.

Este modulo fornece funcoes para validar a autenticidade de webhooks
recebidos do Jira atraves de assinatura HMAC e validacao de tenant.

Terminal 5 - Issue #314: Tenant isolation for Jira integration.

Uso:
```python
from factory.integrations.jira_webhook import validate_jira_webhook, validate_tenant_webhook

payload = await request.body()
signature = request.headers.get("X-Hub-Signature", "")
tenant_id = request.headers.get("X-Tenant-ID", "")
secret = get_jira_webhook_secret(tenant_id)

if not validate_jira_webhook(payload, signature, secret):
    raise HTTPException(status_code=401, detail="Invalid signature")

if not validate_tenant_webhook(tenant_id, expected_tenant_id):
    raise HTTPException(status_code=403, detail="Invalid tenant")
```
"""

import os
import hmac
import hashlib
import logging
from typing import Dict, Optional

logger = logging.getLogger(__name__)

# Per-tenant webhook secrets cache
_tenant_webhook_secrets: Dict[str, str] = {}


def validate_jira_webhook(payload: bytes, signature: str, secret: str) -> bool:
    """
    Valida a assinatura HMAC-SHA256 do webhook do Jira.

    O Jira envia webhooks com o header X-Hub-Signature contendo
    a assinatura HMAC-SHA256 do payload.

    Args:
        payload: Corpo da requisicao em bytes (request body)
        signature: Valor do header X-Hub-Signature (formato: sha256=<hex>)
        secret: Secret configurado no webhook do Jira

    Returns:
        bool: True se a assinatura e valida, False caso contrario

    Example:
        ```python
        # No endpoint do webhook
        payload = await request.body()
        signature = request.headers.get("X-Hub-Signature", "")
        secret = os.getenv("JIRA_WEBHOOK_SECRET", "")

        if not validate_jira_webhook(payload, signature, secret):
            raise HTTPException(status_code=401, detail="Invalid signature")
        ```

    Security:
        - Usa hmac.compare_digest para comparacao time-constant
        - Previne ataques de timing side-channel
        - Rejeita assinaturas malformadas
    """
    # Se nao houver secret configurado, aceitar (desenvolvimento/testes)
    if not secret:
        logger.warning("JIRA_WEBHOOK_SECRET nao configurado - aceitando webhook sem validacao")
        return True

    # Verificar se a assinatura foi fornecida e esta no formato correto
    if not signature:
        logger.warning("Webhook recebido sem header X-Hub-Signature")
        return False

    if not signature.startswith("sha256="):
        logger.warning(f"Formato de assinatura invalido: {signature[:20]}...")
        return False

    # Calcular assinatura esperada
    expected = 'sha256=' + hmac.new(
        secret.encode('utf-8'),
        payload,
        hashlib.sha256
    ).hexdigest()

    # Comparacao segura contra timing attacks
    is_valid = hmac.compare_digest(expected, signature)

    if not is_valid:
        logger.warning("Assinatura HMAC do webhook Jira invalida")

    return is_valid


def get_jira_webhook_secret(tenant_id: Optional[str] = None) -> str:
    """
    Retorna o secret configurado para webhooks do Jira.

    Args:
        tenant_id: ID do tenant para buscar secret especifico

    Returns:
        str: Secret do tenant ou da variavel de ambiente JIRA_WEBHOOK_SECRET
    """
    global _tenant_webhook_secrets

    # Check tenant-specific secret first
    if tenant_id and tenant_id in _tenant_webhook_secrets:
        return _tenant_webhook_secrets[tenant_id]

    # Try tenant-specific environment variable
    if tenant_id:
        tenant_secret = os.getenv(f"JIRA_WEBHOOK_SECRET_{tenant_id.upper()}", "")
        if tenant_secret:
            _tenant_webhook_secrets[tenant_id] = tenant_secret
            return tenant_secret

    # Fallback to global secret
    return os.getenv("JIRA_WEBHOOK_SECRET", "")


def set_tenant_webhook_secret(tenant_id: str, secret: str) -> None:
    """
    Define o secret de webhook para um tenant especifico.

    Args:
        tenant_id: ID do tenant
        secret: Secret para validacao de webhooks
    """
    global _tenant_webhook_secrets
    _tenant_webhook_secrets[tenant_id] = secret
    logger.info(f"Webhook secret configurado para tenant: {tenant_id}")


def clear_tenant_webhook_secret(tenant_id: str) -> None:
    """
    Remove o secret de webhook de um tenant.

    Args:
        tenant_id: ID do tenant
    """
    global _tenant_webhook_secrets
    if tenant_id in _tenant_webhook_secrets:
        del _tenant_webhook_secrets[tenant_id]
        logger.info(f"Webhook secret removido para tenant: {tenant_id}")


def generate_jira_webhook_signature(payload: bytes, secret: str) -> str:
    """
    Gera uma assinatura HMAC-SHA256 para um payload de webhook Jira.

    Util para testes e simulacao de webhooks.

    Args:
        payload: Corpo da requisicao em bytes
        secret: Secret do webhook

    Returns:
        str: Assinatura no formato sha256=<hex>

    Example:
        ```python
        payload = b'{"webhookEvent": "jira:issue_created"}'
        secret = "meu_secret"
        signature = generate_jira_webhook_signature(payload, secret)
        # signature = "sha256=..."
        ```
    """
    return 'sha256=' + hmac.new(
        secret.encode('utf-8'),
        payload,
        hashlib.sha256
    ).hexdigest()


def validate_tenant_webhook(
    tenant_id: str,
    expected_tenant_id: Optional[str] = None,
    allow_any: bool = False
) -> bool:
    """
    Valida se o tenant_id do webhook corresponde ao esperado.

    Args:
        tenant_id: Tenant ID recebido no webhook (header X-Tenant-ID)
        expected_tenant_id: Tenant ID esperado
        allow_any: Se True, aceita qualquer tenant_id valido

    Returns:
        bool: True se o tenant e valido

    Example:
        ```python
        tenant_id = request.headers.get("X-Tenant-ID", "")
        expected = "TENANT-001"

        if not validate_tenant_webhook(tenant_id, expected):
            raise HTTPException(status_code=403, detail="Invalid tenant")
        ```
    """
    if not tenant_id:
        logger.warning("Webhook recebido sem X-Tenant-ID header")
        return False

    if allow_any:
        return True

    if expected_tenant_id and tenant_id != expected_tenant_id:
        logger.warning(
            f"Tenant ID nao corresponde: recebido={tenant_id}, esperado={expected_tenant_id}"
        )
        return False

    return True


def validate_jira_webhook_with_tenant(
    payload: bytes,
    signature: str,
    tenant_id: str,
    expected_tenant_id: Optional[str] = None
) -> bool:
    """
    Valida webhook do Jira com verificacao de assinatura e tenant.

    Args:
        payload: Corpo da requisicao em bytes
        signature: Valor do header X-Hub-Signature
        tenant_id: Tenant ID recebido no webhook
        expected_tenant_id: Tenant ID esperado (opcional)

    Returns:
        bool: True se valido

    Example:
        ```python
        payload = await request.body()
        signature = request.headers.get("X-Hub-Signature", "")
        tenant_id = request.headers.get("X-Tenant-ID", "")

        if not validate_jira_webhook_with_tenant(
            payload, signature, tenant_id, "TENANT-001"
        ):
            raise HTTPException(status_code=401, detail="Invalid webhook")
        ```
    """
    # Validate tenant first
    if not validate_tenant_webhook(tenant_id, expected_tenant_id):
        return False

    # Get tenant-specific secret
    secret = get_jira_webhook_secret(tenant_id)

    # Validate signature
    return validate_jira_webhook(payload, signature, secret)
