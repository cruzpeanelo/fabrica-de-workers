# -*- coding: utf-8 -*-
"""
API Key Service - Issue #341
============================
Complete API key management service for secure integrations.

Features:
- Create, list, revoke API keys
- Scope-based access control
- Rate limiting per key
- Key rotation
- Usage logging
"""

import os
import secrets
import hashlib
import time
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any, Tuple
from functools import lru_cache

import bcrypt

from .models import APIKey, APIKeyScope, APIKeyCreateResult


# =============================================================================
# CONFIGURATION
# =============================================================================

API_KEY_PREFIX_LIVE = "pk_live_"
API_KEY_PREFIX_TEST = "pk_test_"
SECRET_LENGTH = 32  # 256 bits of entropy
DEFAULT_RATE_LIMIT = 1000  # requests per minute
DEFAULT_EXPIRY_DAYS = 365


# =============================================================================
# STORAGE (In-memory for now, production would use DB)
# =============================================================================

_api_keys_store: Dict[str, APIKey] = {}
_rate_limit_store: Dict[str, List[float]] = {}  # key_id -> list of timestamps
_usage_log: List[Dict[str, Any]] = []


# =============================================================================
# API KEY SERVICE
# =============================================================================

class APIKeyService:
    """
    Service for managing API keys.

    API keys provide secure access for external integrations without
    requiring user JWT tokens.
    """

    def __init__(self, tenant_id: Optional[str] = None):
        self.tenant_id = tenant_id

    # -------------------------------------------------------------------------
    # KEY GENERATION
    # -------------------------------------------------------------------------

    def _generate_key_id(self, is_test: bool = False) -> str:
        """Generate unique key ID with prefix."""
        prefix = API_KEY_PREFIX_TEST if is_test else API_KEY_PREFIX_LIVE
        unique = secrets.token_hex(12)  # 24 chars
        return f"{prefix}{unique}"

    def _generate_secret(self) -> str:
        """Generate cryptographically secure secret."""
        return secrets.token_urlsafe(SECRET_LENGTH)

    def _hash_secret(self, secret: str) -> str:
        """Hash secret using bcrypt."""
        return bcrypt.hashpw(secret.encode(), bcrypt.gensalt()).decode()

    def _verify_secret(self, secret: str, hashed: str) -> bool:
        """Verify secret against hash."""
        try:
            return bcrypt.checkpw(secret.encode(), hashed.encode())
        except Exception:
            return False

    # -------------------------------------------------------------------------
    # CRUD OPERATIONS
    # -------------------------------------------------------------------------

    def create_key(
        self,
        name: str,
        tenant_id: str,
        created_by: str,
        scopes: Optional[List[str]] = None,
        rate_limit: int = DEFAULT_RATE_LIMIT,
        expires_in_days: Optional[int] = DEFAULT_EXPIRY_DAYS,
        is_test: bool = False,
        metadata: Optional[Dict] = None
    ) -> APIKeyCreateResult:
        """
        Create a new API key.

        Returns the key with the secret - secret is only shown once!
        """
        # Generate key ID and secret
        key_id = self._generate_key_id(is_test)
        secret = self._generate_secret()
        key_hash = self._hash_secret(secret)

        # Calculate expiry
        expires_at = None
        if expires_in_days:
            expires_at = datetime.utcnow() + timedelta(days=expires_in_days)

        # Validate scopes
        valid_scopes = self._validate_scopes(scopes or [])

        # Create key object
        api_key = APIKey(
            id=key_id,
            tenant_id=tenant_id,
            name=name,
            key_hash=key_hash,
            scopes=valid_scopes,
            rate_limit=rate_limit,
            expires_at=expires_at,
            created_by=created_by,
            is_test=is_test,
            metadata=metadata or {}
        )

        # Store key
        _api_keys_store[key_id] = api_key

        # Log creation
        self._log_usage(key_id, "created", {"created_by": created_by})

        return APIKeyCreateResult(key=api_key, secret=secret)

    def get_key(self, key_id: str) -> Optional[APIKey]:
        """Get API key by ID (without secret)."""
        key = _api_keys_store.get(key_id)
        if key and self.tenant_id and key.tenant_id != self.tenant_id:
            return None  # Tenant isolation
        return key

    def list_keys(self, tenant_id: Optional[str] = None) -> List[APIKey]:
        """List all API keys for a tenant."""
        target_tenant = tenant_id or self.tenant_id
        if not target_tenant:
            return list(_api_keys_store.values())

        return [
            key for key in _api_keys_store.values()
            if key.tenant_id == target_tenant
        ]

    def revoke_key(self, key_id: str, revoked_by: str) -> bool:
        """Revoke (soft delete) an API key."""
        key = self.get_key(key_id)
        if not key:
            return False

        key.is_active = False
        self._log_usage(key_id, "revoked", {"revoked_by": revoked_by})
        return True

    def delete_key(self, key_id: str) -> bool:
        """Permanently delete an API key."""
        if key_id in _api_keys_store:
            key = _api_keys_store[key_id]
            if self.tenant_id and key.tenant_id != self.tenant_id:
                return False
            del _api_keys_store[key_id]
            return True
        return False

    def update_key(
        self,
        key_id: str,
        name: Optional[str] = None,
        scopes: Optional[List[str]] = None,
        rate_limit: Optional[int] = None,
        is_active: Optional[bool] = None,
        metadata: Optional[Dict] = None
    ) -> Optional[APIKey]:
        """Update API key properties."""
        key = self.get_key(key_id)
        if not key:
            return None

        if name is not None:
            key.name = name
        if scopes is not None:
            key.scopes = self._validate_scopes(scopes)
        if rate_limit is not None:
            key.rate_limit = rate_limit
        if is_active is not None:
            key.is_active = is_active
        if metadata is not None:
            key.metadata.update(metadata)

        self._log_usage(key_id, "updated", {"changes": "properties"})
        return key

    # -------------------------------------------------------------------------
    # KEY ROTATION
    # -------------------------------------------------------------------------

    def rotate_key(self, key_id: str, rotated_by: str) -> Optional[APIKeyCreateResult]:
        """
        Rotate an API key - generates new secret, keeps same ID.

        Old secret becomes invalid immediately.
        """
        key = self.get_key(key_id)
        if not key:
            return None

        # Generate new secret
        new_secret = self._generate_secret()
        key.key_hash = self._hash_secret(new_secret)

        self._log_usage(key_id, "rotated", {"rotated_by": rotated_by})

        return APIKeyCreateResult(key=key, secret=new_secret)

    # -------------------------------------------------------------------------
    # AUTHENTICATION
    # -------------------------------------------------------------------------

    def authenticate(self, full_key: str) -> Tuple[bool, Optional[APIKey], str]:
        """
        Authenticate using API key.

        Args:
            full_key: Format "pk_live_xxx.secret" or just header value

        Returns:
            (success, api_key, message)
        """
        # Parse key format
        if "." in full_key:
            key_id, secret = full_key.split(".", 1)
        else:
            # Try to find by prefix matching
            return False, None, "Invalid key format. Expected: key_id.secret"

        # Get key
        key = _api_keys_store.get(key_id)
        if not key:
            return False, None, "API key not found"

        # Check if active
        if not key.is_active:
            return False, None, "API key has been revoked"

        # Check expiry
        if key.is_expired():
            return False, None, "API key has expired"

        # Verify secret
        if not self._verify_secret(secret, key.key_hash):
            return False, None, "Invalid API key secret"

        # Check rate limit
        if not self._check_rate_limit(key_id, key.rate_limit):
            return False, None, "Rate limit exceeded"

        # Update last used
        key.last_used_at = datetime.utcnow()

        # Log usage
        self._log_usage(key_id, "authenticated", {})

        return True, key, "Authenticated successfully"

    def check_scope(self, key: APIKey, required_scope: str) -> bool:
        """Check if key has required scope."""
        return key.has_scope(required_scope)

    # -------------------------------------------------------------------------
    # RATE LIMITING
    # -------------------------------------------------------------------------

    def _check_rate_limit(self, key_id: str, limit: int) -> bool:
        """
        Check if key is within rate limit.

        Uses sliding window algorithm.
        """
        now = time.time()
        window_start = now - 60  # 1 minute window

        if key_id not in _rate_limit_store:
            _rate_limit_store[key_id] = []

        # Remove old timestamps
        _rate_limit_store[key_id] = [
            ts for ts in _rate_limit_store[key_id]
            if ts > window_start
        ]

        # Check limit
        if len(_rate_limit_store[key_id]) >= limit:
            return False

        # Add current request
        _rate_limit_store[key_id].append(now)
        return True

    def get_rate_limit_status(self, key_id: str) -> Dict[str, Any]:
        """Get current rate limit status for a key."""
        key = self.get_key(key_id)
        if not key:
            return {"error": "Key not found"}

        now = time.time()
        window_start = now - 60

        timestamps = _rate_limit_store.get(key_id, [])
        current_usage = len([ts for ts in timestamps if ts > window_start])

        return {
            "key_id": key_id,
            "limit": key.rate_limit,
            "current_usage": current_usage,
            "remaining": max(0, key.rate_limit - current_usage),
            "reset_at": datetime.utcfromtimestamp(window_start + 60).isoformat()
        }

    # -------------------------------------------------------------------------
    # SCOPE VALIDATION
    # -------------------------------------------------------------------------

    def _validate_scopes(self, scopes: List[str]) -> List[str]:
        """Validate and filter scopes."""
        valid = {s.value for s in APIKeyScope}
        return [s for s in scopes if s in valid]

    @staticmethod
    def get_available_scopes() -> List[Dict[str, str]]:
        """Get list of all available scopes."""
        return [
            {"scope": s.value, "name": s.name.replace("_", " ").title()}
            for s in APIKeyScope
        ]

    # -------------------------------------------------------------------------
    # USAGE LOGGING
    # -------------------------------------------------------------------------

    def _log_usage(self, key_id: str, action: str, details: Dict[str, Any]):
        """Log API key usage."""
        _usage_log.append({
            "key_id": key_id,
            "action": action,
            "details": details,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Keep only last 10000 entries
        if len(_usage_log) > 10000:
            _usage_log.pop(0)

    def get_usage_log(
        self,
        key_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get usage log, optionally filtered by key."""
        logs = _usage_log
        if key_id:
            logs = [l for l in logs if l["key_id"] == key_id]
        return logs[-limit:]


# =============================================================================
# SINGLETON ACCESSOR
# =============================================================================

_service_instance: Optional[APIKeyService] = None


def get_api_key_service(tenant_id: Optional[str] = None) -> APIKeyService:
    """Get API key service instance."""
    global _service_instance
    if _service_instance is None:
        _service_instance = APIKeyService()
    if tenant_id:
        return APIKeyService(tenant_id)
    return _service_instance


# =============================================================================
# MIDDLEWARE HELPER
# =============================================================================

def validate_api_key_header(
    header_value: str,
    required_scope: Optional[str] = None
) -> Tuple[bool, Optional[APIKey], str]:
    """
    Validate API key from request header.

    Usage in FastAPI:
        @app.get("/api/data")
        async def get_data(x_api_key: str = Header(...)):
            success, key, msg = validate_api_key_header(x_api_key, "data:read")
            if not success:
                raise HTTPException(401, msg)
    """
    service = get_api_key_service()
    success, key, message = service.authenticate(header_value)

    if not success:
        return False, None, message

    if required_scope and not service.check_scope(key, required_scope):
        return False, key, f"Missing required scope: {required_scope}"

    return True, key, "OK"
