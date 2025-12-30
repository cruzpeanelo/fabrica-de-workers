# -*- coding: utf-8 -*-
"""
Azure DevOps Webhook Verifier
=============================
Signature verification for Azure DevOps Service Hooks.

Terminal 5 - Issue #303

Azure DevOps supports multiple authentication methods:
1. Basic Authentication (username:password in header)
2. HTTP Headers (custom header with shared secret)
3. OAuth tokens (not commonly used for webhooks)

Documentation:
https://learn.microsoft.com/en-us/azure/devops/service-hooks/overview
"""

import base64
import logging
from typing import Optional

from fastapi import Request

from .signature_verifier import (
    WebhookConfig,
    WebhookPlatform,
    WebhookSignatureVerifier,
    VerificationResult
)

logger = logging.getLogger(__name__)


class AzureDevOpsWebhookVerifier(WebhookSignatureVerifier):
    """
    Azure DevOps Service Hook verifier.

    Azure DevOps webhooks can be secured using:
    - Basic Authentication
    - Custom HTTP headers
    - Resource version matching

    Example:
        verifier = AzureDevOpsWebhookVerifier(
            username="webhook",
            password="secret123"
        )

        @app.post("/webhook/azure-devops")
        async def azure_webhook(request: Request):
            is_valid = await verifier.verify(request)
            if not is_valid:
                raise HTTPException(status_code=401)

            payload = await request.json()
            event_type = payload.get("eventType")
            ...
    """

    platform = WebhookPlatform.AZURE_DEVOPS

    # Azure DevOps headers
    AUTHORIZATION_HEADER = "Authorization"
    CUSTOM_HEADER_PREFIX = "X-Azure-DevOps-"

    def __init__(
        self,
        username: Optional[str] = None,
        password: Optional[str] = None,
        shared_secret: Optional[str] = None,
        custom_header: Optional[str] = None,
        tenant_id: Optional[str] = None
    ):
        """
        Initialize Azure DevOps webhook verifier.

        Args:
            username: Basic auth username
            password: Basic auth password
            shared_secret: Shared secret for custom header validation
            custom_header: Custom header name to check
            tenant_id: Tenant ID for multi-tenant isolation
        """
        config = WebhookConfig(
            platform=WebhookPlatform.AZURE_DEVOPS,
            secret=password or shared_secret,
            tenant_id=tenant_id
        )
        super().__init__(config=config)

        self.username = username
        self.password = password
        self.shared_secret = shared_secret
        self.custom_header = custom_header

    def get_signature_header(self) -> str:
        """Return the header name used for verification."""
        if self.custom_header:
            return self.custom_header
        return self.AUTHORIZATION_HEADER

    async def verify(self, request: Request) -> bool:
        """
        Verify Azure DevOps webhook authentication.

        Args:
            request: FastAPI Request object

        Returns:
            True if authentication is valid
        """
        if not self.config.enabled:
            logger.warning("Azure DevOps webhook verification disabled")
            return True

        # Get event type for logging
        try:
            body = await request.json()
            event_type = body.get("eventType", "unknown")
        except Exception:
            event_type = "unknown"

        # Try Basic Authentication first
        if self.username and self.password:
            is_valid = await self._verify_basic_auth(request)
            if is_valid:
                self.log_verification(
                    VerificationResult.VALID,
                    request=request,
                    event_type=event_type,
                    details={"method": "basic_auth"}
                )
                return True

        # Try custom header
        if self.shared_secret and self.custom_header:
            is_valid = await self._verify_custom_header(request)
            if is_valid:
                self.log_verification(
                    VerificationResult.VALID,
                    request=request,
                    event_type=event_type,
                    details={"method": "custom_header"}
                )
                return True

        # No valid authentication found
        self.log_verification(
            VerificationResult.INVALID,
            request=request,
            event_type=event_type,
            details={"method": "none"}
        )
        return False

    async def _verify_basic_auth(self, request: Request) -> bool:
        """Verify Basic Authentication header."""
        auth_header = request.headers.get(self.AUTHORIZATION_HEADER)
        if not auth_header:
            return False

        if not auth_header.startswith("Basic "):
            return False

        try:
            # Decode Base64 credentials
            encoded = auth_header[6:]  # Remove "Basic " prefix
            decoded = base64.b64decode(encoded).decode('utf-8')
            username, password = decoded.split(":", 1)

            return (
                self.constant_time_compare(username, self.username) and
                self.constant_time_compare(password, self.password)
            )
        except Exception as e:
            logger.debug(f"Basic auth decode failed: {e}")
            return False

    async def _verify_custom_header(self, request: Request) -> bool:
        """Verify custom header value."""
        header_value = request.headers.get(self.custom_header)
        if not header_value:
            return False

        return self.constant_time_compare(header_value, self.shared_secret)


# =============================================================================
# AZURE DEVOPS EVENT TYPES
# =============================================================================

class AzureDevOpsEvents:
    """Azure DevOps Service Hook event types."""

    # Build events
    BUILD_COMPLETED = "build.complete"

    # Release events
    RELEASE_CREATED = "ms.vss-release.release-created-event"
    RELEASE_ABANDONED = "ms.vss-release.release-abandoned-event"
    RELEASE_DEPLOYMENT_APPROVAL_COMPLETED = "ms.vss-release.deployment-approval-completed-event"
    RELEASE_DEPLOYMENT_APPROVAL_PENDING = "ms.vss-release.deployment-approval-pending-event"
    RELEASE_DEPLOYMENT_COMPLETED = "ms.vss-release.deployment-completed-event"
    RELEASE_DEPLOYMENT_STARTED = "ms.vss-release.deployment-started-event"

    # Code events
    CODE_PUSHED = "git.push"
    PULL_REQUEST_CREATED = "git.pullrequest.created"
    PULL_REQUEST_UPDATED = "git.pullrequest.updated"
    PULL_REQUEST_MERGED = "git.pullrequest.merged"

    # Work item events
    WORKITEM_CREATED = "workitem.created"
    WORKITEM_UPDATED = "workitem.updated"
    WORKITEM_DELETED = "workitem.deleted"
    WORKITEM_RESTORED = "workitem.restored"
    WORKITEM_COMMENTED = "workitem.commented"

    # Pipeline events
    PIPELINE_RUN_STATE_CHANGED = "ms.vss-pipelines.run-state-changed-event"
    PIPELINE_STAGE_STATE_CHANGED = "ms.vss-pipelines.stage-state-changed-event"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def create_azure_devops_verifier(
    username: Optional[str] = None,
    password: Optional[str] = None,
    shared_secret: Optional[str] = None,
    custom_header: str = "X-Azure-DevOps-Secret",
    tenant_id: Optional[str] = None
) -> AzureDevOpsWebhookVerifier:
    """
    Factory function to create Azure DevOps webhook verifier.

    Args:
        username: Basic auth username
        password: Basic auth password
        shared_secret: Shared secret for custom header
        custom_header: Name of custom header
        tenant_id: Optional tenant ID

    Returns:
        Configured AzureDevOpsWebhookVerifier
    """
    return AzureDevOpsWebhookVerifier(
        username=username,
        password=password,
        shared_secret=shared_secret,
        custom_header=custom_header if shared_secret else None,
        tenant_id=tenant_id
    )


async def parse_azure_devops_webhook(request: Request) -> dict:
    """
    Parse Azure DevOps webhook payload.

    Args:
        request: FastAPI Request object

    Returns:
        Dictionary with event info and payload
    """
    payload = await request.json()

    return {
        "event_type": payload.get("eventType"),
        "subscription_id": payload.get("subscriptionId"),
        "notification_id": payload.get("notificationId"),
        "publisher_id": payload.get("publisherId"),
        "message": payload.get("message", {}).get("text"),
        "resource": payload.get("resource", {}),
        "resource_version": payload.get("resourceVersion"),
        "created_date": payload.get("createdDate"),
        "payload": payload
    }
