# -*- coding: utf-8 -*-
"""
Jira Webhook Verifier
=====================
Signature verification for Jira webhooks.

Terminal 5 - Issue #303

Jira Cloud uses asymmetric JWT for webhook authentication.
Jira Server/Data Center can use:
- Shared secrets
- IP whitelisting

Documentation:
https://developer.atlassian.com/cloud/jira/platform/webhooks/
"""

import base64
import hashlib
import hmac
import logging
import time
from typing import Optional, Dict, Any, List

from fastapi import Request

from .signature_verifier import (
    WebhookConfig,
    WebhookPlatform,
    WebhookSignatureVerifier,
    VerificationResult
)

logger = logging.getLogger(__name__)


class JiraWebhookVerifier(WebhookSignatureVerifier):
    """
    Jira webhook verifier.

    For Jira Cloud, webhooks are signed using asymmetric JWT.
    For Jira Server, webhooks can be verified using shared secrets.

    Example:
        # For Jira Cloud with JWT
        verifier = JiraWebhookVerifier(
            shared_secret="atlassian-connect-secret",
            base_url="https://your-app.example.com"
        )

        # For Jira Server with simple secret
        verifier = JiraWebhookVerifier(
            shared_secret="webhook-secret",
            use_jwt=False
        )

        @app.post("/webhook/jira")
        async def jira_webhook(request: Request):
            is_valid = await verifier.verify(request)
            if not is_valid:
                raise HTTPException(status_code=401)
            ...
    """

    platform = WebhookPlatform.JIRA

    # Jira headers
    AUTHORIZATION_HEADER = "Authorization"
    ATLASSIAN_QSH_HEADER = "X-Atlassian-QSH"
    WEBHOOK_ID_HEADER = "X-Atlassian-Webhook-Identifier"

    def __init__(
        self,
        shared_secret: str,
        base_url: Optional[str] = None,
        use_jwt: bool = True,
        tenant_id: Optional[str] = None,
        allowed_issuers: Optional[List[str]] = None
    ):
        """
        Initialize Jira webhook verifier.

        Args:
            shared_secret: Shared secret from Atlassian Connect or webhook config
            base_url: Your application's base URL (required for JWT QSH)
            use_jwt: Use JWT verification (Jira Cloud) vs simple HMAC
            tenant_id: Tenant ID for multi-tenant isolation
            allowed_issuers: Allowed JWT issuers (Jira Cloud)
        """
        config = WebhookConfig(
            platform=WebhookPlatform.JIRA,
            secret=shared_secret,
            tenant_id=tenant_id
        )
        super().__init__(config=config)

        self.base_url = base_url
        self.use_jwt = use_jwt
        self.allowed_issuers = allowed_issuers or []

    def get_signature_header(self) -> str:
        """Return the header name containing the signature."""
        return self.AUTHORIZATION_HEADER

    async def verify(self, request: Request) -> bool:
        """
        Verify Jira webhook signature.

        Args:
            request: FastAPI Request object

        Returns:
            True if signature is valid
        """
        if not self.config.enabled:
            logger.warning("Jira webhook verification disabled")
            return True

        if not self.config.secret:
            self.log_verification(
                VerificationResult.MISSING_SECRET,
                request=request
            )
            return False

        # Get event type for logging
        event_type = await self._get_event_type(request)
        webhook_id = request.headers.get(self.WEBHOOK_ID_HEADER)

        if self.use_jwt:
            # JWT verification for Jira Cloud
            is_valid = await self._verify_jwt(request)
        else:
            # Simple HMAC verification for Jira Server
            is_valid = await self._verify_hmac(request)

        result = VerificationResult.VALID if is_valid else VerificationResult.INVALID
        self.log_verification(
            result,
            request=request,
            event_type=event_type,
            details={
                "webhook_id": webhook_id,
                "method": "jwt" if self.use_jwt else "hmac"
            }
        )

        return is_valid

    async def _verify_jwt(self, request: Request) -> bool:
        """
        Verify JWT token from Jira Cloud.

        Jira Cloud sends JWT in Authorization header:
        Authorization: JWT <token>
        """
        auth_header = request.headers.get(self.AUTHORIZATION_HEADER)
        if not auth_header:
            return False

        if not auth_header.startswith("JWT "):
            return False

        token = auth_header[4:]  # Remove "JWT " prefix

        try:
            # Decode JWT (we need to verify it manually for Atlassian Connect)
            parts = token.split(".")
            if len(parts) != 3:
                return False

            header_b64, payload_b64, signature_b64 = parts

            # Decode payload
            payload_json = self._base64url_decode(payload_b64)
            import json
            payload = json.loads(payload_json)

            # Verify expiration
            exp = payload.get("exp")
            if exp and time.time() > exp:
                logger.warning("JWT token expired")
                return False

            # Verify issuer
            iss = payload.get("iss")
            if self.allowed_issuers and iss not in self.allowed_issuers:
                logger.warning(f"JWT issuer not allowed: {iss}")
                return False

            # Verify Query String Hash (QSH) if base_url is set
            qsh = payload.get("qsh")
            if qsh and self.base_url:
                expected_qsh = self._compute_qsh(request)
                if qsh != expected_qsh:
                    logger.warning("QSH mismatch")
                    return False

            # Verify signature
            message = f"{header_b64}.{payload_b64}"
            expected_sig = hmac.new(
                self.config.secret.encode('utf-8'),
                message.encode('utf-8'),
                hashlib.sha256
            ).digest()
            expected_sig_b64 = self._base64url_encode(expected_sig)

            return self.constant_time_compare(signature_b64, expected_sig_b64)

        except Exception as e:
            logger.error(f"JWT verification failed: {e}")
            return False

    async def _verify_hmac(self, request: Request) -> bool:
        """
        Verify HMAC signature for Jira Server.

        Some Jira Server installations use X-Hub-Signature style headers.
        """
        # Check for X-Hub-Signature header (used by some Jira Server setups)
        signature_header = request.headers.get("X-Hub-Signature-256")
        if signature_header:
            if not signature_header.startswith("sha256="):
                return False

            expected_sig = signature_header[7:]
            payload = await self.get_body(request)
            computed_sig = self.compute_hmac(payload, self.config.secret, "sha256")
            return self.constant_time_compare(computed_sig, expected_sig)

        # Check for custom header
        signature_header = request.headers.get("X-Jira-Signature")
        if signature_header:
            payload = await self.get_body(request)
            computed_sig = self.compute_hmac(payload, self.config.secret, "sha256")
            return self.constant_time_compare(computed_sig, signature_header)

        # No signature found - check if Authorization Basic matches
        auth_header = request.headers.get(self.AUTHORIZATION_HEADER)
        if auth_header and auth_header.startswith("Basic "):
            try:
                encoded = auth_header[6:]
                decoded = base64.b64decode(encoded).decode('utf-8')
                _, password = decoded.split(":", 1)
                return self.constant_time_compare(password, self.config.secret)
            except Exception:
                pass

        return False

    async def _get_event_type(self, request: Request) -> Optional[str]:
        """Extract event type from Jira webhook payload."""
        try:
            body = await request.json()
            return body.get("webhookEvent") or body.get("issue_event_type_name")
        except Exception:
            return None

    def _compute_qsh(self, request: Request) -> str:
        """
        Compute Query String Hash for Atlassian Connect JWT.

        QSH = SHA256(METHOD&PATH&CANONICAL_QUERY_STRING)
        """
        method = request.method.upper()
        path = request.url.path

        # Sort query params
        query_params = sorted(request.query_params.items())
        query_string = "&".join(f"{k}={v}" for k, v in query_params)

        canonical = f"{method}&{path}&{query_string}"
        return hashlib.sha256(canonical.encode('utf-8')).hexdigest()

    @staticmethod
    def _base64url_decode(data: str) -> bytes:
        """Decode base64url encoded string."""
        # Add padding if needed
        padding = 4 - len(data) % 4
        if padding != 4:
            data += "=" * padding
        return base64.urlsafe_b64decode(data)

    @staticmethod
    def _base64url_encode(data: bytes) -> str:
        """Encode bytes as base64url string."""
        return base64.urlsafe_b64encode(data).rstrip(b"=").decode('utf-8')


# =============================================================================
# JIRA EVENT TYPES
# =============================================================================

class JiraEvents:
    """Jira webhook event types."""

    # Issue events
    ISSUE_CREATED = "jira:issue_created"
    ISSUE_UPDATED = "jira:issue_updated"
    ISSUE_DELETED = "jira:issue_deleted"

    # Sprint events
    SPRINT_CREATED = "sprint_created"
    SPRINT_UPDATED = "sprint_updated"
    SPRINT_DELETED = "sprint_deleted"
    SPRINT_STARTED = "sprint_started"
    SPRINT_CLOSED = "sprint_closed"

    # Board events
    BOARD_CREATED = "board_created"
    BOARD_UPDATED = "board_updated"
    BOARD_DELETED = "board_deleted"

    # Project events
    PROJECT_CREATED = "project_created"
    PROJECT_UPDATED = "project_updated"
    PROJECT_DELETED = "project_deleted"
    PROJECT_SOFT_DELETED = "project_soft_deleted"
    PROJECT_RESTORED = "project_restored"
    PROJECT_PERMANENTLY_DELETED = "project_permanently_deleted"

    # Version events
    VERSION_RELEASED = "jira:version_released"
    VERSION_UNRELEASED = "jira:version_unreleased"
    VERSION_CREATED = "jira:version_created"
    VERSION_MOVED = "jira:version_moved"
    VERSION_UPDATED = "jira:version_updated"
    VERSION_DELETED = "jira:version_deleted"

    # User events
    USER_CREATED = "user_created"
    USER_UPDATED = "user_updated"
    USER_DELETED = "user_deleted"

    # Worklog events
    WORKLOG_CREATED = "worklog_created"
    WORKLOG_UPDATED = "worklog_updated"
    WORKLOG_DELETED = "worklog_deleted"

    # Comment events
    COMMENT_CREATED = "comment_created"
    COMMENT_UPDATED = "comment_updated"
    COMMENT_DELETED = "comment_deleted"

    # Attachment events
    ATTACHMENT_CREATED = "attachment_created"
    ATTACHMENT_DELETED = "attachment_deleted"

    # Link events
    ISSUELINK_CREATED = "issuelink_created"
    ISSUELINK_DELETED = "issuelink_deleted"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def create_jira_cloud_verifier(
    shared_secret: str,
    base_url: str,
    tenant_id: Optional[str] = None,
    allowed_issuers: Optional[List[str]] = None
) -> JiraWebhookVerifier:
    """
    Create verifier for Jira Cloud webhooks.

    Args:
        shared_secret: Atlassian Connect shared secret
        base_url: Your application's base URL
        tenant_id: Optional tenant ID
        allowed_issuers: Allowed JWT issuers

    Returns:
        Configured JiraWebhookVerifier
    """
    return JiraWebhookVerifier(
        shared_secret=shared_secret,
        base_url=base_url,
        use_jwt=True,
        tenant_id=tenant_id,
        allowed_issuers=allowed_issuers
    )


def create_jira_server_verifier(
    shared_secret: str,
    tenant_id: Optional[str] = None
) -> JiraWebhookVerifier:
    """
    Create verifier for Jira Server webhooks.

    Args:
        shared_secret: Webhook shared secret
        tenant_id: Optional tenant ID

    Returns:
        Configured JiraWebhookVerifier
    """
    return JiraWebhookVerifier(
        shared_secret=shared_secret,
        use_jwt=False,
        tenant_id=tenant_id
    )


async def parse_jira_webhook(request: Request) -> dict:
    """
    Parse Jira webhook payload.

    Args:
        request: FastAPI Request object

    Returns:
        Dictionary with event info and payload
    """
    payload = await request.json()

    return {
        "event": payload.get("webhookEvent"),
        "issue_event_type": payload.get("issue_event_type_name"),
        "timestamp": payload.get("timestamp"),
        "user": payload.get("user", {}).get("displayName"),
        "issue_key": payload.get("issue", {}).get("key"),
        "issue_id": payload.get("issue", {}).get("id"),
        "changelog": payload.get("changelog"),
        "payload": payload
    }
