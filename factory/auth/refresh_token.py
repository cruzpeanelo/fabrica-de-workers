# -*- coding: utf-8 -*-
"""
Refresh Token System - Issue #384
=================================
Secure refresh token implementation for mobile apps.

Features:
- Short-lived access tokens (15 min)
- Long-lived refresh tokens (7 days)
- Token rotation on refresh
- Revocation support
- Device binding
"""

import os
import secrets
import hashlib
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, Tuple
from dataclasses import dataclass, field
import logging
import jwt

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

JWT_SECRET = os.getenv("JWT_SECRET", "your-secret-key-change-in-production")
JWT_ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 15
REFRESH_TOKEN_EXPIRE_DAYS = 7
REFRESH_TOKEN_LENGTH = 64


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class RefreshToken:
    """Refresh token record."""
    token_hash: str
    user_id: str
    tenant_id: str
    device_id: Optional[str]
    created_at: datetime
    expires_at: datetime
    last_used_at: Optional[datetime] = None
    is_revoked: bool = False
    revoked_at: Optional[datetime] = None
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None

    def is_valid(self) -> bool:
        """Check if token is valid."""
        if self.is_revoked:
            return False
        if datetime.utcnow() > self.expires_at:
            return False
        return True

    def to_dict(self) -> Dict[str, Any]:
        return {
            "user_id": self.user_id,
            "tenant_id": self.tenant_id,
            "device_id": self.device_id,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat(),
            "last_used_at": self.last_used_at.isoformat() if self.last_used_at else None,
            "is_revoked": self.is_revoked
        }


@dataclass
class TokenPair:
    """Access and refresh token pair."""
    access_token: str
    refresh_token: str
    token_type: str = "Bearer"
    expires_in: int = ACCESS_TOKEN_EXPIRE_MINUTES * 60

    def to_dict(self) -> Dict[str, Any]:
        return {
            "access_token": self.access_token,
            "refresh_token": self.refresh_token,
            "token_type": self.token_type,
            "expires_in": self.expires_in
        }


# =============================================================================
# STORAGE
# =============================================================================

_refresh_tokens: Dict[str, RefreshToken] = {}
_user_tokens: Dict[str, set] = {}  # user_id -> set of token_hashes


# =============================================================================
# REFRESH TOKEN SERVICE
# =============================================================================

class RefreshTokenService:
    """
    Service for managing refresh tokens.
    """

    @classmethod
    def _hash_token(cls, token: str) -> str:
        """Create hash of token for storage."""
        return hashlib.sha256(token.encode()).hexdigest()

    @classmethod
    def _generate_refresh_token(cls) -> str:
        """Generate a secure refresh token."""
        return secrets.token_urlsafe(REFRESH_TOKEN_LENGTH)

    @classmethod
    def _generate_access_token(
        cls,
        user_id: str,
        tenant_id: str,
        role: str = "USER",
        extra_claims: Dict[str, Any] = None
    ) -> str:
        """Generate a JWT access token."""
        now = datetime.utcnow()
        expires = now + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)

        payload = {
            "sub": user_id,
            "tenant_id": tenant_id,
            "role": role,
            "iat": now,
            "exp": expires,
            "type": "access"
        }

        if extra_claims:
            payload.update(extra_claims)

        return jwt.encode(payload, JWT_SECRET, algorithm=JWT_ALGORITHM)

    # -------------------------------------------------------------------------
    # TOKEN CREATION
    # -------------------------------------------------------------------------

    @classmethod
    def create_token_pair(
        cls,
        user_id: str,
        tenant_id: str,
        role: str = "USER",
        device_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        extra_claims: Dict[str, Any] = None
    ) -> TokenPair:
        """
        Create a new access/refresh token pair.

        Used after successful login.
        """
        # Generate tokens
        access_token = cls._generate_access_token(
            user_id, tenant_id, role, extra_claims
        )
        refresh_token = cls._generate_refresh_token()
        token_hash = cls._hash_token(refresh_token)

        # Store refresh token
        now = datetime.utcnow()
        expires = now + timedelta(days=REFRESH_TOKEN_EXPIRE_DAYS)

        token_record = RefreshToken(
            token_hash=token_hash,
            user_id=user_id,
            tenant_id=tenant_id,
            device_id=device_id,
            created_at=now,
            expires_at=expires,
            ip_address=ip_address,
            user_agent=user_agent
        )

        _refresh_tokens[token_hash] = token_record

        # Track by user
        if user_id not in _user_tokens:
            _user_tokens[user_id] = set()
        _user_tokens[user_id].add(token_hash)

        logger.info(f"Created token pair for user {user_id}, device {device_id}")

        return TokenPair(
            access_token=access_token,
            refresh_token=refresh_token
        )

    # -------------------------------------------------------------------------
    # TOKEN REFRESH
    # -------------------------------------------------------------------------

    @classmethod
    def refresh_tokens(
        cls,
        refresh_token: str,
        ip_address: Optional[str] = None
    ) -> Tuple[Optional[TokenPair], str]:
        """
        Refresh tokens using a valid refresh token.

        Implements token rotation - old refresh token is invalidated.

        Returns:
            (token_pair, error_message)
        """
        token_hash = cls._hash_token(refresh_token)

        # Find token
        token_record = _refresh_tokens.get(token_hash)
        if not token_record:
            return None, "Invalid refresh token"

        # Validate token
        if not token_record.is_valid():
            if token_record.is_revoked:
                # Potential token theft - revoke all user tokens
                logger.warning(
                    f"Attempted use of revoked token for user {token_record.user_id}"
                )
                cls.revoke_all_user_tokens(token_record.user_id, "security")
                return None, "Token has been revoked. All sessions terminated for security."
            return None, "Refresh token expired"

        # Revoke old token (rotation)
        token_record.is_revoked = True
        token_record.revoked_at = datetime.utcnow()

        # Create new token pair
        new_pair = cls.create_token_pair(
            user_id=token_record.user_id,
            tenant_id=token_record.tenant_id,
            device_id=token_record.device_id,
            ip_address=ip_address
        )

        logger.info(f"Refreshed tokens for user {token_record.user_id}")

        return new_pair, ""

    # -------------------------------------------------------------------------
    # TOKEN REVOCATION
    # -------------------------------------------------------------------------

    @classmethod
    def revoke_token(cls, refresh_token: str) -> bool:
        """Revoke a specific refresh token."""
        token_hash = cls._hash_token(refresh_token)

        if token_hash in _refresh_tokens:
            _refresh_tokens[token_hash].is_revoked = True
            _refresh_tokens[token_hash].revoked_at = datetime.utcnow()
            logger.info(f"Revoked refresh token")
            return True

        return False

    @classmethod
    def revoke_all_user_tokens(
        cls,
        user_id: str,
        reason: str = "logout"
    ) -> int:
        """Revoke all refresh tokens for a user."""
        count = 0
        token_hashes = _user_tokens.get(user_id, set()).copy()

        for token_hash in token_hashes:
            if token_hash in _refresh_tokens:
                _refresh_tokens[token_hash].is_revoked = True
                _refresh_tokens[token_hash].revoked_at = datetime.utcnow()
                count += 1

        logger.info(f"Revoked {count} tokens for user {user_id}: {reason}")
        return count

    @classmethod
    def revoke_device_tokens(
        cls,
        user_id: str,
        device_id: str
    ) -> int:
        """Revoke all tokens for a specific device."""
        count = 0
        token_hashes = _user_tokens.get(user_id, set()).copy()

        for token_hash in token_hashes:
            token = _refresh_tokens.get(token_hash)
            if token and token.device_id == device_id:
                token.is_revoked = True
                token.revoked_at = datetime.utcnow()
                count += 1

        logger.info(f"Revoked {count} tokens for device {device_id}")
        return count

    # -------------------------------------------------------------------------
    # TOKEN QUERIES
    # -------------------------------------------------------------------------

    @classmethod
    def get_user_active_sessions(cls, user_id: str) -> list:
        """Get all active sessions for a user."""
        sessions = []
        token_hashes = _user_tokens.get(user_id, set())

        for token_hash in token_hashes:
            token = _refresh_tokens.get(token_hash)
            if token and token.is_valid():
                sessions.append({
                    "device_id": token.device_id,
                    "created_at": token.created_at.isoformat(),
                    "last_used_at": token.last_used_at.isoformat() if token.last_used_at else None,
                    "ip_address": token.ip_address,
                    "expires_at": token.expires_at.isoformat()
                })

        return sessions

    @classmethod
    def validate_access_token(cls, token: str) -> Tuple[bool, Dict[str, Any], str]:
        """
        Validate a JWT access token.

        Returns:
            (is_valid, payload, error_message)
        """
        try:
            payload = jwt.decode(token, JWT_SECRET, algorithms=[JWT_ALGORITHM])

            # Check token type
            if payload.get("type") != "access":
                return False, {}, "Invalid token type"

            return True, payload, ""

        except jwt.ExpiredSignatureError:
            return False, {}, "Token expired"
        except jwt.InvalidTokenError as e:
            return False, {}, f"Invalid token: {str(e)}"


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_tokens(
    user_id: str,
    tenant_id: str,
    role: str = "USER",
    device_id: str = None
) -> TokenPair:
    """Create a new token pair."""
    return RefreshTokenService.create_token_pair(
        user_id, tenant_id, role, device_id
    )


def refresh_tokens(refresh_token: str) -> Tuple[Optional[TokenPair], str]:
    """Refresh tokens."""
    return RefreshTokenService.refresh_tokens(refresh_token)


def revoke_all_tokens(user_id: str) -> int:
    """Revoke all user tokens."""
    return RefreshTokenService.revoke_all_user_tokens(user_id)


def validate_token(token: str) -> Tuple[bool, Dict, str]:
    """Validate access token."""
    return RefreshTokenService.validate_access_token(token)
