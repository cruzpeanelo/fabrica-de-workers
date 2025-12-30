# -*- coding: utf-8 -*-
"""
Webhook Signature Verifier
==========================
Base classes and utilities for webhook signature verification.

Terminal 5 - Issue #303

Supported platforms:
- GitHub (HMAC-SHA256)
- Azure DevOps (Basic Auth / Service Hooks)
- Jira (Asymmetric JWT)
- Salesforce (RSA-SHA256)
"""

import hashlib
import hmac
import logging
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Union

from fastapi import HTTPException, Request, status

logger = logging.getLogger(__name__)


class WebhookPlatform(str, Enum):
    """Supported webhook platforms."""
    GITHUB = "github"
    AZURE_DEVOPS = "azure_devops"
    JIRA = "jira"
    SALESFORCE = "salesforce"
    CUSTOM = "custom"


class VerificationResult(str, Enum):
    """Result of signature verification."""
    VALID = "valid"
    INVALID = "invalid"
    EXPIRED = "expired"
    MISSING_SIGNATURE = "missing_signature"
    MISSING_SECRET = "missing_secret"
    UNSUPPORTED = "unsupported"


@dataclass
class WebhookVerificationLog:
    """Log entry for webhook verification attempt."""
    timestamp: datetime
    platform: WebhookPlatform
    result: VerificationResult
    tenant_id: Optional[str]
    source_ip: Optional[str]
    event_type: Optional[str]
    request_id: Optional[str]
    details: Dict[str, Any] = field(default_factory=dict)


@dataclass
class WebhookConfig:
    """Configuration for webhook verification."""
    platform: WebhookPlatform
    secret: Optional[str] = None
    public_key: Optional[str] = None
    allowed_ips: List[str] = field(default_factory=list)
    max_age_seconds: int = 300  # 5 minutes
    tenant_id: Optional[str] = None
    enabled: bool = True

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary (without secrets)."""
        return {
            "platform": self.platform.value,
            "has_secret": bool(self.secret),
            "has_public_key": bool(self.public_key),
            "allowed_ips": self.allowed_ips,
            "max_age_seconds": self.max_age_seconds,
            "tenant_id": self.tenant_id,
            "enabled": self.enabled
        }


class WebhookSignatureVerifier(ABC):
    """
    Abstract base class for webhook signature verification.

    Each platform implements its own verification logic.

    Example:
        verifier = GitHubWebhookVerifier(secret="webhook_secret")

        # In FastAPI endpoint
        @app.post("/webhook/github")
        async def github_webhook(request: Request):
            is_valid = await verifier.verify(request)
            if not is_valid:
                raise HTTPException(status_code=401)
            # Process webhook...
    """

    platform: WebhookPlatform = WebhookPlatform.CUSTOM

    def __init__(
        self,
        config: Optional[WebhookConfig] = None,
        secret: Optional[str] = None,
        tenant_id: Optional[str] = None
    ):
        """
        Initialize verifier.

        Args:
            config: Full webhook configuration
            secret: Shared secret (shortcut if config not provided)
            tenant_id: Tenant ID for multi-tenant isolation
        """
        if config:
            self.config = config
        else:
            self.config = WebhookConfig(
                platform=self.platform,
                secret=secret,
                tenant_id=tenant_id
            )

        self._verification_logs: List[WebhookVerificationLog] = []
        self._max_logs = 1000

    @abstractmethod
    async def verify(self, request: Request) -> bool:
        """
        Verify webhook signature.

        Args:
            request: FastAPI Request object

        Returns:
            True if signature is valid
        """
        pass

    @abstractmethod
    def get_signature_header(self) -> str:
        """Return the header name containing the signature."""
        pass

    async def get_body(self, request: Request) -> bytes:
        """Get request body as bytes."""
        return await request.body()

    def compute_hmac(
        self,
        payload: bytes,
        secret: str,
        algorithm: str = "sha256"
    ) -> str:
        """
        Compute HMAC signature.

        Args:
            payload: Raw request body
            secret: Shared secret
            algorithm: Hash algorithm (sha1, sha256, sha512)

        Returns:
            Hex-encoded HMAC signature
        """
        if algorithm == "sha1":
            hash_func = hashlib.sha1
        elif algorithm == "sha256":
            hash_func = hashlib.sha256
        elif algorithm == "sha512":
            hash_func = hashlib.sha512
        else:
            raise ValueError(f"Unsupported algorithm: {algorithm}")

        mac = hmac.new(
            secret.encode('utf-8'),
            payload,
            hash_func
        )
        return mac.hexdigest()

    def constant_time_compare(self, a: str, b: str) -> bool:
        """
        Compare two strings in constant time to prevent timing attacks.

        Args:
            a: First string
            b: Second string

        Returns:
            True if strings are equal
        """
        return hmac.compare_digest(a, b)

    def log_verification(
        self,
        result: VerificationResult,
        request: Optional[Request] = None,
        event_type: Optional[str] = None,
        details: Optional[Dict] = None
    ):
        """
        Log verification attempt.

        Args:
            result: Verification result
            request: Original request
            event_type: Webhook event type
            details: Additional details
        """
        source_ip = None
        request_id = None

        if request:
            # Get client IP
            forwarded = request.headers.get("X-Forwarded-For")
            if forwarded:
                source_ip = forwarded.split(",")[0].strip()
            else:
                source_ip = request.client.host if request.client else None

            request_id = request.headers.get("X-Request-ID")

        log_entry = WebhookVerificationLog(
            timestamp=datetime.utcnow(),
            platform=self.platform,
            result=result,
            tenant_id=self.config.tenant_id,
            source_ip=source_ip,
            event_type=event_type,
            request_id=request_id,
            details=details or {}
        )

        self._verification_logs.append(log_entry)

        # Trim old logs
        if len(self._verification_logs) > self._max_logs:
            self._verification_logs = self._verification_logs[-self._max_logs:]

        # Log to standard logger
        log_level = logging.INFO if result == VerificationResult.VALID else logging.WARNING
        logger.log(
            log_level,
            f"Webhook verification [{self.platform.value}]: {result.value} "
            f"ip={source_ip} tenant={self.config.tenant_id}"
        )

    def get_verification_logs(
        self,
        limit: int = 100,
        result_filter: Optional[VerificationResult] = None
    ) -> List[WebhookVerificationLog]:
        """
        Get verification logs.

        Args:
            limit: Maximum number of logs
            result_filter: Filter by result type

        Returns:
            List of verification logs
        """
        logs = self._verification_logs

        if result_filter:
            logs = [l for l in logs if l.result == result_filter]

        return logs[-limit:]

    def get_failed_attempts(self, minutes: int = 60) -> int:
        """
        Count failed verification attempts in time window.

        Args:
            minutes: Time window in minutes

        Returns:
            Number of failed attempts
        """
        cutoff = datetime.utcnow()
        failed_results = {
            VerificationResult.INVALID,
            VerificationResult.EXPIRED,
            VerificationResult.MISSING_SIGNATURE
        }

        count = 0
        for log in reversed(self._verification_logs):
            age = (cutoff - log.timestamp).total_seconds()
            if age > minutes * 60:
                break
            if log.result in failed_results:
                count += 1

        return count


class WebhookVerifierRegistry:
    """
    Registry for webhook verifiers by platform and tenant.

    Example:
        registry = WebhookVerifierRegistry()

        # Register verifier for tenant
        registry.register(
            WebhookPlatform.GITHUB,
            "tenant-123",
            GitHubWebhookVerifier(secret="xxx")
        )

        # Get verifier
        verifier = registry.get(WebhookPlatform.GITHUB, "tenant-123")
    """

    def __init__(self):
        self._verifiers: Dict[str, WebhookSignatureVerifier] = {}
        self._default_verifiers: Dict[WebhookPlatform, WebhookSignatureVerifier] = {}

    def _make_key(self, platform: WebhookPlatform, tenant_id: str) -> str:
        """Create registry key."""
        return f"{platform.value}:{tenant_id}"

    def register(
        self,
        platform: WebhookPlatform,
        tenant_id: str,
        verifier: WebhookSignatureVerifier
    ):
        """
        Register verifier for platform and tenant.

        Args:
            platform: Webhook platform
            tenant_id: Tenant ID
            verifier: Verifier instance
        """
        key = self._make_key(platform, tenant_id)
        self._verifiers[key] = verifier
        logger.info(f"Registered webhook verifier: {key}")

    def register_default(
        self,
        platform: WebhookPlatform,
        verifier: WebhookSignatureVerifier
    ):
        """
        Register default verifier for platform.

        Args:
            platform: Webhook platform
            verifier: Verifier instance
        """
        self._default_verifiers[platform] = verifier
        logger.info(f"Registered default webhook verifier: {platform.value}")

    def get(
        self,
        platform: WebhookPlatform,
        tenant_id: Optional[str] = None
    ) -> Optional[WebhookSignatureVerifier]:
        """
        Get verifier for platform and tenant.

        Args:
            platform: Webhook platform
            tenant_id: Tenant ID (optional)

        Returns:
            Verifier or None if not found
        """
        if tenant_id:
            key = self._make_key(platform, tenant_id)
            verifier = self._verifiers.get(key)
            if verifier:
                return verifier

        return self._default_verifiers.get(platform)

    def unregister(self, platform: WebhookPlatform, tenant_id: str):
        """Remove verifier from registry."""
        key = self._make_key(platform, tenant_id)
        if key in self._verifiers:
            del self._verifiers[key]
            logger.info(f"Unregistered webhook verifier: {key}")

    def list_verifiers(self) -> List[Dict[str, Any]]:
        """List all registered verifiers."""
        result = []

        for key, verifier in self._verifiers.items():
            platform, tenant_id = key.split(":", 1)
            result.append({
                "platform": platform,
                "tenant_id": tenant_id,
                "config": verifier.config.to_dict()
            })

        for platform, verifier in self._default_verifiers.items():
            result.append({
                "platform": platform.value,
                "tenant_id": None,
                "is_default": True,
                "config": verifier.config.to_dict()
            })

        return result


# Global registry
_webhook_registry = WebhookVerifierRegistry()


def get_webhook_registry() -> WebhookVerifierRegistry:
    """Get global webhook verifier registry."""
    return _webhook_registry


# =============================================================================
# FASTAPI DEPENDENCIES
# =============================================================================

async def verify_webhook_signature(
    request: Request,
    platform: WebhookPlatform,
    tenant_id: Optional[str] = None
) -> bool:
    """
    FastAPI dependency for webhook signature verification.

    Usage:
        @app.post("/webhook/github")
        async def webhook(
            request: Request,
            verified: bool = Depends(
                lambda r: verify_webhook_signature(r, WebhookPlatform.GITHUB)
            )
        ):
            if not verified:
                raise HTTPException(status_code=401)
    """
    registry = get_webhook_registry()
    verifier = registry.get(platform, tenant_id)

    if not verifier:
        logger.warning(f"No verifier found for {platform.value}")
        return False

    return await verifier.verify(request)


def require_webhook_signature(
    platform: WebhookPlatform,
    tenant_id: Optional[str] = None
):
    """
    Factory for FastAPI dependency that requires valid signature.

    Usage:
        @app.post("/webhook/github")
        async def webhook(
            request: Request,
            _: None = Depends(require_webhook_signature(WebhookPlatform.GITHUB))
        ):
            # Only reaches here if signature is valid
            ...
    """
    async def dependency(request: Request):
        is_valid = await verify_webhook_signature(request, platform, tenant_id)
        if not is_valid:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid webhook signature"
            )
        return None

    return dependency
