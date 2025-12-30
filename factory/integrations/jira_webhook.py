# -*- coding: utf-8 -*-
"""
Jira Webhook Validation Module
==============================
Funcoes para validacao de webhooks do Jira usando HMAC-SHA256.

Este modulo fornece funcoes para validar a autenticidade de webhooks
recebidos do Jira atraves de assinatura HMAC.

Uso:
```python
from factory.integrations.jira_webhook import validate_jira_webhook

payload = await request.body()
signature = request.headers.get("X-Hub-Signature", "")
secret = os.getenv("JIRA_WEBHOOK_SECRET", "")

if not validate_jira_webhook(payload, signature, secret):
    raise HTTPException(status_code=401, detail="Invalid signature")
```
"""

import os
import hmac
import hashlib
import logging

logger = logging.getLogger(__name__)


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


def get_jira_webhook_secret() -> str:
    """
    Retorna o secret configurado para webhooks do Jira.

    Returns:
        str: Secret da variavel de ambiente JIRA_WEBHOOK_SECRET ou string vazia
    """
    return os.getenv("JIRA_WEBHOOK_SECRET", "")


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
