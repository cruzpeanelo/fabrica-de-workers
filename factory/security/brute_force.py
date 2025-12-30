# -*- coding: utf-8 -*-
"""
Brute Force Protection - Issue #402
====================================
Protection against brute force attacks on authentication endpoints.

Features:
- Login attempt tracking per IP and user
- Progressive lockout (5min, 15min, 1h, 24h)
- IP whitelist for trusted sources
- Automatic unlock after timeout
- Manual unlock by admin
"""

import time
import threading
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Tuple
from dataclasses import dataclass, field
from enum import Enum
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

MAX_ATTEMPTS = 5  # Max failed attempts before lockout
LOCKOUT_DURATIONS = [
    300,    # 5 minutes
    900,    # 15 minutes
    3600,   # 1 hour
    86400,  # 24 hours
]
ATTEMPT_WINDOW = 900  # 15 minutes - window for counting attempts
CLEANUP_INTERVAL = 300  # 5 minutes - cleanup old records


# =============================================================================
# MODELS
# =============================================================================

class LockoutReason(str, Enum):
    """Reason for lockout."""
    TOO_MANY_ATTEMPTS = "too_many_attempts"
    SUSPICIOUS_ACTIVITY = "suspicious_activity"
    ADMIN_BLOCKED = "admin_blocked"


@dataclass
class LoginAttempt:
    """Record of a login attempt."""
    identifier: str  # IP or user_id
    timestamp: float
    success: bool
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    username: Optional[str] = None


@dataclass
class LockoutRecord:
    """Record of a lockout."""
    identifier: str
    locked_at: datetime
    unlock_at: datetime
    reason: LockoutReason
    attempt_count: int
    lockout_level: int = 0  # Index into LOCKOUT_DURATIONS

    def is_active(self) -> bool:
        """Check if lockout is still active."""
        return datetime.utcnow() < self.unlock_at

    def remaining_seconds(self) -> int:
        """Get remaining lockout time in seconds."""
        if not self.is_active():
            return 0
        delta = self.unlock_at - datetime.utcnow()
        return int(delta.total_seconds())

    def to_dict(self) -> Dict[str, Any]:
        return {
            "identifier": self.identifier,
            "locked_at": self.locked_at.isoformat(),
            "unlock_at": self.unlock_at.isoformat(),
            "reason": self.reason.value,
            "attempt_count": self.attempt_count,
            "lockout_level": self.lockout_level,
            "is_active": self.is_active(),
            "remaining_seconds": self.remaining_seconds()
        }


# =============================================================================
# STORAGE
# =============================================================================

_attempts: Dict[str, List[LoginAttempt]] = {}
_lockouts: Dict[str, LockoutRecord] = {}
_whitelist: set = set()
_lock = threading.Lock()


# =============================================================================
# BRUTE FORCE SERVICE
# =============================================================================

class BruteForceProtection:
    """
    Service for protecting against brute force attacks.

    Tracks failed login attempts and implements progressive lockout.
    """

    # -------------------------------------------------------------------------
    # ATTEMPT TRACKING
    # -------------------------------------------------------------------------

    @classmethod
    def record_attempt(
        cls,
        identifier: str,
        success: bool,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        username: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Record a login attempt.

        Args:
            identifier: IP address or user ID
            success: Whether the attempt succeeded
            ip_address: Client IP address
            user_agent: Client user agent
            username: Username attempted

        Returns:
            (allowed, message) - whether future attempts are allowed
        """
        # Check whitelist
        if cls.is_whitelisted(identifier) or cls.is_whitelisted(ip_address):
            return True, None

        # Check existing lockout
        if cls.is_locked(identifier):
            lockout = _lockouts.get(identifier)
            return False, f"Account locked. Try again in {lockout.remaining_seconds()} seconds."

        with _lock:
            # Record attempt
            attempt = LoginAttempt(
                identifier=identifier,
                timestamp=time.time(),
                success=success,
                ip_address=ip_address,
                user_agent=user_agent,
                username=username
            )

            if identifier not in _attempts:
                _attempts[identifier] = []
            _attempts[identifier].append(attempt)

            # If success, clear failed attempts
            if success:
                _attempts[identifier] = [a for a in _attempts[identifier] if a.success]
                # Also clear any lockout
                if identifier in _lockouts:
                    del _lockouts[identifier]
                return True, None

            # Count recent failed attempts
            failed_count = cls._count_recent_failures(identifier)

            if failed_count >= MAX_ATTEMPTS:
                # Apply lockout
                cls._apply_lockout(identifier, failed_count)
                lockout = _lockouts[identifier]

                logger.warning(
                    f"Brute force lockout applied: {identifier} "
                    f"({failed_count} failures, level {lockout.lockout_level})"
                )

                return False, f"Too many failed attempts. Try again in {lockout.remaining_seconds()} seconds."

            remaining = MAX_ATTEMPTS - failed_count
            return True, f"{remaining} attempts remaining before lockout."

    @classmethod
    def _count_recent_failures(cls, identifier: str) -> int:
        """Count recent failed attempts within the window."""
        cutoff = time.time() - ATTEMPT_WINDOW
        attempts = _attempts.get(identifier, [])

        return sum(1 for a in attempts if not a.success and a.timestamp > cutoff)

    @classmethod
    def _apply_lockout(cls, identifier: str, attempt_count: int):
        """Apply lockout with progressive duration."""
        # Determine lockout level based on previous lockouts
        existing = _lockouts.get(identifier)
        if existing and existing.is_active():
            level = min(existing.lockout_level + 1, len(LOCKOUT_DURATIONS) - 1)
        elif existing:
            level = existing.lockout_level  # Keep same level if recently unlocked
        else:
            level = 0

        duration = LOCKOUT_DURATIONS[level]
        now = datetime.utcnow()

        _lockouts[identifier] = LockoutRecord(
            identifier=identifier,
            locked_at=now,
            unlock_at=now + timedelta(seconds=duration),
            reason=LockoutReason.TOO_MANY_ATTEMPTS,
            attempt_count=attempt_count,
            lockout_level=level
        )

    # -------------------------------------------------------------------------
    # LOCKOUT CHECKS
    # -------------------------------------------------------------------------

    @classmethod
    def is_locked(cls, identifier: str) -> bool:
        """Check if an identifier is currently locked out."""
        if identifier in _whitelist:
            return False

        lockout = _lockouts.get(identifier)
        if not lockout:
            return False

        if lockout.is_active():
            return True

        # Lockout expired - keep record but allow access
        return False

    @classmethod
    def check_access(
        cls,
        identifier: str,
        ip_address: Optional[str] = None
    ) -> Tuple[bool, Optional[str], Optional[int]]:
        """
        Check if access is allowed before login attempt.

        Returns:
            (allowed, message, remaining_seconds)
        """
        # Check whitelist
        if cls.is_whitelisted(identifier) or cls.is_whitelisted(ip_address):
            return True, None, None

        # Check IP lockout
        if ip_address and cls.is_locked(ip_address):
            lockout = _lockouts[ip_address]
            return False, "IP temporarily blocked", lockout.remaining_seconds()

        # Check identifier lockout
        if cls.is_locked(identifier):
            lockout = _lockouts[identifier]
            return False, "Account temporarily locked", lockout.remaining_seconds()

        return True, None, None

    @classmethod
    def get_lockout(cls, identifier: str) -> Optional[LockoutRecord]:
        """Get lockout record for an identifier."""
        return _lockouts.get(identifier)

    @classmethod
    def get_attempts(cls, identifier: str, limit: int = 50) -> List[Dict]:
        """Get recent login attempts for an identifier."""
        attempts = _attempts.get(identifier, [])
        return [
            {
                "timestamp": datetime.fromtimestamp(a.timestamp).isoformat(),
                "success": a.success,
                "ip_address": a.ip_address,
                "username": a.username
            }
            for a in attempts[-limit:]
        ]

    # -------------------------------------------------------------------------
    # WHITELIST MANAGEMENT
    # -------------------------------------------------------------------------

    @classmethod
    def add_to_whitelist(cls, identifier: str):
        """Add an IP or user to the whitelist."""
        _whitelist.add(identifier)
        # Remove any existing lockout
        if identifier in _lockouts:
            del _lockouts[identifier]
        logger.info(f"Added to brute force whitelist: {identifier}")

    @classmethod
    def remove_from_whitelist(cls, identifier: str) -> bool:
        """Remove from whitelist."""
        if identifier in _whitelist:
            _whitelist.discard(identifier)
            logger.info(f"Removed from brute force whitelist: {identifier}")
            return True
        return False

    @classmethod
    def is_whitelisted(cls, identifier: Optional[str]) -> bool:
        """Check if identifier is whitelisted."""
        return identifier is not None and identifier in _whitelist

    @classmethod
    def get_whitelist(cls) -> List[str]:
        """Get all whitelisted identifiers."""
        return list(_whitelist)

    # -------------------------------------------------------------------------
    # UNLOCK / ADMIN ACTIONS
    # -------------------------------------------------------------------------

    @classmethod
    def unlock(cls, identifier: str) -> bool:
        """Manually unlock an identifier."""
        if identifier in _lockouts:
            del _lockouts[identifier]
            logger.info(f"Manually unlocked: {identifier}")
            return True
        return False

    @classmethod
    def block(
        cls,
        identifier: str,
        duration_seconds: int = 86400,
        reason: str = "admin_blocked"
    ):
        """Manually block an identifier."""
        now = datetime.utcnow()

        _lockouts[identifier] = LockoutRecord(
            identifier=identifier,
            locked_at=now,
            unlock_at=now + timedelta(seconds=duration_seconds),
            reason=LockoutReason.ADMIN_BLOCKED,
            attempt_count=0,
            lockout_level=len(LOCKOUT_DURATIONS) - 1
        )

        logger.warning(f"Admin blocked: {identifier} for {duration_seconds}s")

    @classmethod
    def clear_attempts(cls, identifier: str):
        """Clear attempt history for an identifier."""
        if identifier in _attempts:
            del _attempts[identifier]
            logger.info(f"Cleared attempt history: {identifier}")

    # -------------------------------------------------------------------------
    # STATISTICS
    # -------------------------------------------------------------------------

    @classmethod
    def get_stats(cls) -> Dict[str, Any]:
        """Get brute force protection statistics."""
        active_lockouts = sum(1 for l in _lockouts.values() if l.is_active())

        return {
            "total_tracked_identifiers": len(_attempts),
            "total_lockouts": len(_lockouts),
            "active_lockouts": active_lockouts,
            "whitelisted_count": len(_whitelist),
            "max_attempts": MAX_ATTEMPTS,
            "lockout_durations": LOCKOUT_DURATIONS
        }

    @classmethod
    def get_active_lockouts(cls) -> List[Dict]:
        """Get all currently active lockouts."""
        return [
            l.to_dict() for l in _lockouts.values()
            if l.is_active()
        ]

    # -------------------------------------------------------------------------
    # CLEANUP
    # -------------------------------------------------------------------------

    @classmethod
    def cleanup(cls):
        """Clean up old attempt records."""
        cutoff = time.time() - ATTEMPT_WINDOW * 2

        with _lock:
            # Clean old attempts
            for identifier in list(_attempts.keys()):
                _attempts[identifier] = [
                    a for a in _attempts[identifier]
                    if a.timestamp > cutoff
                ]
                if not _attempts[identifier]:
                    del _attempts[identifier]

            # Clean expired lockouts (keep for a while for history)
            old_cutoff = datetime.utcnow() - timedelta(days=7)
            for identifier in list(_lockouts.keys()):
                if _lockouts[identifier].unlock_at < old_cutoff:
                    del _lockouts[identifier]


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def record_login_attempt(
    identifier: str,
    success: bool,
    ip_address: str = None
) -> Tuple[bool, Optional[str]]:
    """Record a login attempt and check if blocked."""
    return BruteForceProtection.record_attempt(
        identifier, success, ip_address
    )


def check_brute_force(
    identifier: str,
    ip_address: str = None
) -> Tuple[bool, Optional[str], Optional[int]]:
    """Check if access is allowed."""
    return BruteForceProtection.check_access(identifier, ip_address)


def is_blocked(identifier: str) -> bool:
    """Check if identifier is blocked."""
    return BruteForceProtection.is_locked(identifier)


def unlock_account(identifier: str) -> bool:
    """Unlock a blocked account."""
    return BruteForceProtection.unlock(identifier)


def whitelist_ip(ip_address: str):
    """Add IP to whitelist."""
    BruteForceProtection.add_to_whitelist(ip_address)
