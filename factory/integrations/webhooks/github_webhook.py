# -*- coding: utf-8 -*-
"""
GitHub Webhook Verifier
=======================
Signature verification for GitHub webhooks.

Terminal 5 - Issue #303

GitHub uses HMAC-SHA256 for webhook signatures.
Header: X-Hub-Signature-256 (or X-Hub-Signature for SHA1 legacy)

Documentation:
https://docs.github.com/en/webhooks/using-webhooks/validating-webhook-deliveries
"""

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


class GitHubWebhookVerifier(WebhookSignatureVerifier):
    """
    GitHub webhook signature verifier.

    GitHub signs webhooks using HMAC-SHA256.
    The signature is in the X-Hub-Signature-256 header.

    Example:
        verifier = GitHubWebhookVerifier(secret="your_webhook_secret")

        @app.post("/webhook/github")
        async def github_webhook(request: Request):
            is_valid = await verifier.verify(request)
            if not is_valid:
                raise HTTPException(status_code=401)

            # Process webhook
            payload = await request.json()
            event = request.headers.get("X-GitHub-Event")
            ...
    """

    platform = WebhookPlatform.GITHUB

    # GitHub signature headers
    SIGNATURE_HEADER_256 = "X-Hub-Signature-256"
    SIGNATURE_HEADER_SHA1 = "X-Hub-Signature"  # Legacy
    EVENT_HEADER = "X-GitHub-Event"
    DELIVERY_HEADER = "X-GitHub-Delivery"

    def __init__(
        self,
        secret: str,
        tenant_id: Optional[str] = None,
        allow_sha1: bool = False
    ):
        """
        Initialize GitHub webhook verifier.

        Args:
            secret: Webhook secret configured in GitHub
            tenant_id: Tenant ID for multi-tenant isolation
            allow_sha1: Allow legacy SHA1 signatures (not recommended)
        """
        config = WebhookConfig(
            platform=WebhookPlatform.GITHUB,
            secret=secret,
            tenant_id=tenant_id
        )
        super().__init__(config=config)
        self.allow_sha1 = allow_sha1

    def get_signature_header(self) -> str:
        """Return the primary signature header name."""
        return self.SIGNATURE_HEADER_256

    async def verify(self, request: Request) -> bool:
        """
        Verify GitHub webhook signature.

        Args:
            request: FastAPI Request object

        Returns:
            True if signature is valid
        """
        if not self.config.enabled:
            logger.warning("GitHub webhook verification disabled")
            return True

        if not self.config.secret:
            self.log_verification(
                VerificationResult.MISSING_SECRET,
                request=request
            )
            return False

        # Get event type for logging
        event_type = request.headers.get(self.EVENT_HEADER)
        delivery_id = request.headers.get(self.DELIVERY_HEADER)

        # Try SHA256 first (preferred)
        signature_256 = request.headers.get(self.SIGNATURE_HEADER_256)
        if signature_256:
            is_valid = await self._verify_sha256(request, signature_256)
            self.log_verification(
                VerificationResult.VALID if is_valid else VerificationResult.INVALID,
                request=request,
                event_type=event_type,
                details={"delivery_id": delivery_id, "algorithm": "sha256"}
            )
            return is_valid

        # Fall back to SHA1 if allowed
        if self.allow_sha1:
            signature_sha1 = request.headers.get(self.SIGNATURE_HEADER_SHA1)
            if signature_sha1:
                logger.warning("Using legacy SHA1 signature verification")
                is_valid = await self._verify_sha1(request, signature_sha1)
                self.log_verification(
                    VerificationResult.VALID if is_valid else VerificationResult.INVALID,
                    request=request,
                    event_type=event_type,
                    details={"delivery_id": delivery_id, "algorithm": "sha1"}
                )
                return is_valid

        # No signature found
        self.log_verification(
            VerificationResult.MISSING_SIGNATURE,
            request=request,
            event_type=event_type,
            details={"delivery_id": delivery_id}
        )
        return False

    async def _verify_sha256(self, request: Request, signature: str) -> bool:
        """Verify SHA256 signature."""
        # Signature format: sha256=<hex>
        if not signature.startswith("sha256="):
            return False

        expected_sig = signature[7:]  # Remove "sha256=" prefix
        payload = await self.get_body(request)
        computed_sig = self.compute_hmac(payload, self.config.secret, "sha256")

        return self.constant_time_compare(computed_sig, expected_sig)

    async def _verify_sha1(self, request: Request, signature: str) -> bool:
        """Verify legacy SHA1 signature."""
        # Signature format: sha1=<hex>
        if not signature.startswith("sha1="):
            return False

        expected_sig = signature[5:]  # Remove "sha1=" prefix
        payload = await self.get_body(request)
        computed_sig = self.compute_hmac(payload, self.config.secret, "sha1")

        return self.constant_time_compare(computed_sig, expected_sig)

    @staticmethod
    def get_event_type(request: Request) -> Optional[str]:
        """Get GitHub event type from request headers."""
        return request.headers.get(GitHubWebhookVerifier.EVENT_HEADER)

    @staticmethod
    def get_delivery_id(request: Request) -> Optional[str]:
        """Get GitHub delivery ID from request headers."""
        return request.headers.get(GitHubWebhookVerifier.DELIVERY_HEADER)


# =============================================================================
# GITHUB EVENT TYPES
# =============================================================================

class GitHubEvents:
    """Common GitHub webhook event types."""

    # Repository events
    PUSH = "push"
    CREATE = "create"
    DELETE = "delete"
    FORK = "fork"
    RELEASE = "release"

    # Pull request events
    PULL_REQUEST = "pull_request"
    PULL_REQUEST_REVIEW = "pull_request_review"
    PULL_REQUEST_REVIEW_COMMENT = "pull_request_review_comment"

    # Issue events
    ISSUES = "issues"
    ISSUE_COMMENT = "issue_comment"

    # CI/CD events
    CHECK_RUN = "check_run"
    CHECK_SUITE = "check_suite"
    WORKFLOW_RUN = "workflow_run"
    WORKFLOW_JOB = "workflow_job"
    DEPLOYMENT = "deployment"
    DEPLOYMENT_STATUS = "deployment_status"

    # Security events
    CODE_SCANNING_ALERT = "code_scanning_alert"
    DEPENDABOT_ALERT = "dependabot_alert"
    SECRET_SCANNING_ALERT = "secret_scanning_alert"

    # Other events
    PING = "ping"
    STAR = "star"
    WATCH = "watch"
    MEMBER = "member"
    TEAM = "team"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def create_github_verifier(
    secret: str,
    tenant_id: Optional[str] = None
) -> GitHubWebhookVerifier:
    """
    Factory function to create GitHub webhook verifier.

    Args:
        secret: Webhook secret from GitHub
        tenant_id: Optional tenant ID

    Returns:
        Configured GitHubWebhookVerifier
    """
    return GitHubWebhookVerifier(
        secret=secret,
        tenant_id=tenant_id,
        allow_sha1=False  # Only allow SHA256
    )


async def parse_github_webhook(request: Request) -> dict:
    """
    Parse GitHub webhook payload.

    Args:
        request: FastAPI Request object

    Returns:
        Dictionary with event info and payload
    """
    return {
        "event": GitHubWebhookVerifier.get_event_type(request),
        "delivery_id": GitHubWebhookVerifier.get_delivery_id(request),
        "payload": await request.json()
    }
