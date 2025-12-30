# -*- coding: utf-8 -*-
"""
Rate Limiter - Issue #393
=========================
Rate limiting implementation for API protection.

Features:
- Per-tenant rate limits
- Per-user rate limits
- Per-endpoint rate limits
- Sliding window algorithm
- Redis-ready storage
"""

import time
import threading
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

DEFAULT_REQUESTS_PER_MINUTE = 60
DEFAULT_BURST_SIZE = 10
AUTH_ENDPOINT_LIMIT = 10  # More restrictive for auth endpoints
SENSITIVE_ENDPOINT_LIMIT = 30


# =============================================================================
# MODELS
# =============================================================================

class RateLimitScope(str, Enum):
    """Scope for rate limiting."""
    GLOBAL = "global"
    TENANT = "tenant"
    USER = "user"
    IP = "ip"
    ENDPOINT = "endpoint"


@dataclass
class RateLimitRule:
    """Rate limit rule configuration."""
    name: str
    scope: RateLimitScope
    requests_per_minute: int = DEFAULT_REQUESTS_PER_MINUTE
    burst_size: int = DEFAULT_BURST_SIZE
    enabled: bool = True
    applies_to: Optional[str] = None  # tenant_id, user_id, or endpoint pattern

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "scope": self.scope.value,
            "requests_per_minute": self.requests_per_minute,
            "burst_size": self.burst_size,
            "enabled": self.enabled,
            "applies_to": self.applies_to
        }


@dataclass
class RateLimitResult:
    """Result of a rate limit check."""
    allowed: bool
    limit: int
    remaining: int
    reset_at: datetime
    retry_after: Optional[int] = None
    scope: Optional[str] = None

    def to_headers(self) -> Dict[str, str]:
        """Convert to HTTP headers."""
        headers = {
            "X-RateLimit-Limit": str(self.limit),
            "X-RateLimit-Remaining": str(max(0, self.remaining)),
            "X-RateLimit-Reset": str(int(self.reset_at.timestamp()))
        }

        if not self.allowed and self.retry_after:
            headers["Retry-After"] = str(self.retry_after)

        return headers


@dataclass
class RateLimitBucket:
    """Token bucket for rate limiting."""
    tokens: float
    last_update: float
    requests_in_window: list = field(default_factory=list)


# =============================================================================
# STORAGE
# =============================================================================

class RateLimitStorage:
    """In-memory storage for rate limits."""

    def __init__(self):
        self._buckets: Dict[str, RateLimitBucket] = {}
        self._lock = threading.Lock()
        self._cleanup_threshold = 10000

    def get_bucket(self, key: str) -> Optional[RateLimitBucket]:
        """Get a rate limit bucket."""
        with self._lock:
            return self._buckets.get(key)

    def set_bucket(self, key: str, bucket: RateLimitBucket):
        """Set a rate limit bucket."""
        with self._lock:
            self._buckets[key] = bucket

            # Cleanup old buckets periodically
            if len(self._buckets) > self._cleanup_threshold:
                self._cleanup()

    def _cleanup(self):
        """Remove old buckets."""
        now = time.time()
        cutoff = now - 300  # 5 minutes

        keys_to_remove = [
            k for k, v in self._buckets.items()
            if v.last_update < cutoff
        ]

        for key in keys_to_remove:
            del self._buckets[key]

        if keys_to_remove:
            logger.debug(f"Cleaned up {len(keys_to_remove)} rate limit buckets")


# =============================================================================
# RATE LIMITER SERVICE
# =============================================================================

class RateLimiter:
    """
    Rate limiter using sliding window algorithm.

    Supports multiple scopes and configurable limits.
    """

    def __init__(self, storage: Optional[RateLimitStorage] = None):
        self.storage = storage or RateLimitStorage()
        self._rules: Dict[str, RateLimitRule] = {}
        self._tenant_limits: Dict[str, int] = {}
        self._user_limits: Dict[str, int] = {}
        self._endpoint_limits: Dict[str, int] = {}

        # Initialize default rules
        self._init_default_rules()

    def _init_default_rules(self):
        """Initialize default rate limit rules."""
        # Global default
        self.add_rule(RateLimitRule(
            name="global_default",
            scope=RateLimitScope.GLOBAL,
            requests_per_minute=DEFAULT_REQUESTS_PER_MINUTE,
            burst_size=DEFAULT_BURST_SIZE
        ))

        # Auth endpoints - more restrictive
        self.add_rule(RateLimitRule(
            name="auth_endpoints",
            scope=RateLimitScope.ENDPOINT,
            requests_per_minute=AUTH_ENDPOINT_LIMIT,
            burst_size=5,
            applies_to="/api/auth/*"
        ))

        # Login specifically - very restrictive
        self.add_rule(RateLimitRule(
            name="login_endpoint",
            scope=RateLimitScope.ENDPOINT,
            requests_per_minute=5,
            burst_size=3,
            applies_to="/api/auth/login"
        ))

    # -------------------------------------------------------------------------
    # RULE MANAGEMENT
    # -------------------------------------------------------------------------

    def add_rule(self, rule: RateLimitRule):
        """Add or update a rate limit rule."""
        self._rules[rule.name] = rule
        logger.info(f"Added rate limit rule: {rule.name}")

    def remove_rule(self, name: str) -> bool:
        """Remove a rate limit rule."""
        if name in self._rules:
            del self._rules[name]
            return True
        return False

    def get_rules(self) -> list:
        """Get all rate limit rules."""
        return [r.to_dict() for r in self._rules.values()]

    def set_tenant_limit(self, tenant_id: str, requests_per_minute: int):
        """Set rate limit for a specific tenant."""
        self._tenant_limits[tenant_id] = requests_per_minute

    def set_user_limit(self, user_id: str, requests_per_minute: int):
        """Set rate limit for a specific user."""
        self._user_limits[user_id] = requests_per_minute

    def set_endpoint_limit(self, endpoint: str, requests_per_minute: int):
        """Set rate limit for a specific endpoint."""
        self._endpoint_limits[endpoint] = requests_per_minute

    # -------------------------------------------------------------------------
    # LIMIT CHECKING
    # -------------------------------------------------------------------------

    def check(
        self,
        identifier: str,
        scope: RateLimitScope = RateLimitScope.IP,
        endpoint: Optional[str] = None,
        tenant_id: Optional[str] = None,
        user_id: Optional[str] = None
    ) -> RateLimitResult:
        """
        Check if request is allowed under rate limits.

        Uses sliding window algorithm with token bucket fallback.
        """
        # Determine the limit to apply
        limit = self._get_applicable_limit(
            scope, endpoint, tenant_id, user_id
        )

        # Build bucket key
        key = self._build_key(identifier, scope, endpoint)

        # Get or create bucket
        bucket = self.storage.get_bucket(key)
        now = time.time()
        window_start = now - 60  # 1 minute window

        if not bucket:
            bucket = RateLimitBucket(
                tokens=limit,
                last_update=now,
                requests_in_window=[]
            )

        # Clean old requests from window
        bucket.requests_in_window = [
            ts for ts in bucket.requests_in_window
            if ts > window_start
        ]

        # Check limit
        requests_in_window = len(bucket.requests_in_window)

        if requests_in_window >= limit:
            # Rate limited
            oldest_request = min(bucket.requests_in_window) if bucket.requests_in_window else now
            reset_at = datetime.fromtimestamp(oldest_request + 60)
            retry_after = int(oldest_request + 60 - now)

            self.storage.set_bucket(key, bucket)

            logger.warning(
                f"Rate limit exceeded for {identifier} ({scope.value}): "
                f"{requests_in_window}/{limit}"
            )

            return RateLimitResult(
                allowed=False,
                limit=limit,
                remaining=0,
                reset_at=reset_at,
                retry_after=max(1, retry_after),
                scope=scope.value
            )

        # Allow request
        bucket.requests_in_window.append(now)
        bucket.last_update = now
        self.storage.set_bucket(key, bucket)

        remaining = limit - len(bucket.requests_in_window)
        reset_at = datetime.utcnow() + timedelta(seconds=60)

        return RateLimitResult(
            allowed=True,
            limit=limit,
            remaining=remaining,
            reset_at=reset_at,
            scope=scope.value
        )

    def _get_applicable_limit(
        self,
        scope: RateLimitScope,
        endpoint: Optional[str],
        tenant_id: Optional[str],
        user_id: Optional[str]
    ) -> int:
        """Get the applicable rate limit."""
        # Check endpoint-specific limits first
        if endpoint:
            if endpoint in self._endpoint_limits:
                return self._endpoint_limits[endpoint]

            # Check pattern matches in rules
            for rule in self._rules.values():
                if rule.scope == RateLimitScope.ENDPOINT and rule.applies_to:
                    if self._matches_pattern(endpoint, rule.applies_to):
                        return rule.requests_per_minute

        # Check tenant-specific limits
        if tenant_id and tenant_id in self._tenant_limits:
            return self._tenant_limits[tenant_id]

        # Check user-specific limits
        if user_id and user_id in self._user_limits:
            return self._user_limits[user_id]

        # Return global default
        return DEFAULT_REQUESTS_PER_MINUTE

    def _matches_pattern(self, path: str, pattern: str) -> bool:
        """Check if path matches pattern (supports * wildcard)."""
        if pattern.endswith("*"):
            return path.startswith(pattern[:-1])
        return path == pattern

    def _build_key(
        self,
        identifier: str,
        scope: RateLimitScope,
        endpoint: Optional[str]
    ) -> str:
        """Build storage key for rate limit bucket."""
        parts = [scope.value, identifier]
        if endpoint:
            # Normalize endpoint for key
            parts.append(endpoint.replace("/", "_"))
        return ":".join(parts)

    # -------------------------------------------------------------------------
    # STATISTICS
    # -------------------------------------------------------------------------

    def get_stats(self) -> Dict[str, Any]:
        """Get rate limiter statistics."""
        return {
            "active_buckets": len(self.storage._buckets),
            "rules_count": len(self._rules),
            "tenant_limits": len(self._tenant_limits),
            "user_limits": len(self._user_limits),
            "endpoint_limits": len(self._endpoint_limits)
        }

    def reset_bucket(self, identifier: str, scope: RateLimitScope) -> bool:
        """Reset rate limit for an identifier."""
        key = self._build_key(identifier, scope, None)

        with self.storage._lock:
            if key in self.storage._buckets:
                del self.storage._buckets[key]
                logger.info(f"Reset rate limit bucket: {key}")
                return True

        return False


# =============================================================================
# GLOBAL INSTANCE
# =============================================================================

_rate_limiter: Optional[RateLimiter] = None


def get_rate_limiter() -> RateLimiter:
    """Get global rate limiter instance."""
    global _rate_limiter
    if _rate_limiter is None:
        _rate_limiter = RateLimiter()
    return _rate_limiter


def check_rate_limit(
    identifier: str,
    scope: RateLimitScope = RateLimitScope.IP,
    endpoint: Optional[str] = None,
    tenant_id: Optional[str] = None,
    user_id: Optional[str] = None
) -> RateLimitResult:
    """Check rate limit for a request."""
    return get_rate_limiter().check(
        identifier, scope, endpoint, tenant_id, user_id
    )
