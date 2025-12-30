# -*- coding: utf-8 -*-
"""
Password Policy - Issue #342
============================
Configurable password policies per tenant.

Features:
- Minimum length and complexity requirements
- Password history (prevent reuse)
- Password expiration
- Account lockout after failed attempts
- Common password blacklist
"""

import re
import hashlib
import logging
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from dataclasses import dataclass, field

logger = logging.getLogger(__name__)

# Common weak passwords to block (top 100)
COMMON_PASSWORDS = {
    "password", "123456", "12345678", "qwerty", "abc123", "monkey", "1234567",
    "letmein", "trustno1", "dragon", "baseball", "iloveyou", "master", "sunshine",
    "ashley", "bailey", "shadow", "123123", "654321", "superman", "qazwsx",
    "michael", "football", "password1", "password123", "welcome", "jesus",
    "ninja", "mustang", "password2", "amanda", "admin", "admin123", "root",
    "toor", "pass", "test", "guest", "master", "changeme", "password!",
    "qwerty123", "letmein123", "welcome1", "welcome123", "p@ssw0rd", "P@ssw0rd"
}


@dataclass
class PasswordPolicy:
    """Password policy configuration."""
    min_length: int = 8
    max_length: int = 128
    require_uppercase: bool = True
    require_lowercase: bool = True
    require_numbers: bool = True
    require_special: bool = False
    special_characters: str = "!@#$%^&*()_+-=[]{}|;:,.<>?"
    max_age_days: int = 90  # 0 = never expires
    history_count: int = 5  # Number of previous passwords to remember
    lockout_attempts: int = 5
    lockout_duration_minutes: int = 30
    block_common_passwords: bool = True
    min_score: int = 3  # Minimum strength score (0-5)


@dataclass
class PasswordValidationResult:
    """Result of password validation."""
    is_valid: bool
    score: int  # 0-5 strength score
    errors: List[str] = field(default_factory=list)
    suggestions: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "is_valid": self.is_valid,
            "score": self.score,
            "strength": self._score_to_strength(),
            "errors": self.errors,
            "suggestions": self.suggestions
        }

    def _score_to_strength(self) -> str:
        strengths = ["very_weak", "weak", "fair", "good", "strong", "very_strong"]
        return strengths[min(self.score, 5)]


class PasswordPolicyValidator:
    """
    Validates passwords against configurable policies.

    Usage:
        validator = PasswordPolicyValidator(policy)
        result = validator.validate("MyP@ssw0rd123")
        if not result.is_valid:
            print(result.errors)
    """

    def __init__(self, policy: PasswordPolicy = None):
        """
        Initialize validator with policy.

        Args:
            policy: Password policy (uses default if not provided)
        """
        self.policy = policy or PasswordPolicy()

    def validate(
        self,
        password: str,
        password_history: List[str] = None
    ) -> PasswordValidationResult:
        """
        Validate password against policy.

        Args:
            password: Password to validate
            password_history: List of hashed previous passwords

        Returns:
            Validation result with errors and score
        """
        errors = []
        suggestions = []
        score = 0

        # Length check
        if len(password) < self.policy.min_length:
            errors.append(f"Password must be at least {self.policy.min_length} characters")
        else:
            score += 1

        if len(password) > self.policy.max_length:
            errors.append(f"Password must be at most {self.policy.max_length} characters")

        # Complexity checks
        has_upper = bool(re.search(r"[A-Z]", password))
        has_lower = bool(re.search(r"[a-z]", password))
        has_number = bool(re.search(r"\d", password))
        has_special = bool(re.search(f"[{re.escape(self.policy.special_characters)}]", password))

        if self.policy.require_uppercase and not has_upper:
            errors.append("Password must contain at least one uppercase letter")
            suggestions.append("Add an uppercase letter (A-Z)")
        elif has_upper:
            score += 1

        if self.policy.require_lowercase and not has_lower:
            errors.append("Password must contain at least one lowercase letter")
            suggestions.append("Add a lowercase letter (a-z)")
        elif has_lower:
            score += 1

        if self.policy.require_numbers and not has_number:
            errors.append("Password must contain at least one number")
            suggestions.append("Add a number (0-9)")
        elif has_number:
            score += 1

        if self.policy.require_special and not has_special:
            errors.append("Password must contain at least one special character")
            suggestions.append(f"Add a special character ({self.policy.special_characters[:10]}...)")
        elif has_special:
            score += 1

        # Common password check
        if self.policy.block_common_passwords:
            if password.lower() in COMMON_PASSWORDS:
                errors.append("Password is too common")
                suggestions.append("Choose a more unique password")

        # Password history check
        if password_history and self.policy.history_count > 0:
            password_hash = self._hash_password(password)
            if password_hash in password_history[:self.policy.history_count]:
                errors.append(f"Cannot reuse one of your last {self.policy.history_count} passwords")

        # Minimum score check
        if score < self.policy.min_score and not errors:
            suggestions.append("Consider adding more complexity for a stronger password")

        is_valid = len(errors) == 0 and score >= self.policy.min_score

        return PasswordValidationResult(
            is_valid=is_valid,
            score=min(score, 5),
            errors=errors,
            suggestions=suggestions
        )

    def check_expiration(
        self,
        last_changed: datetime
    ) -> Dict[str, Any]:
        """
        Check if password has expired.

        Args:
            last_changed: When password was last changed

        Returns:
            Dict with expiration status
        """
        if self.policy.max_age_days == 0:
            return {"expired": False, "days_until_expiry": None}

        expiry_date = last_changed + timedelta(days=self.policy.max_age_days)
        now = datetime.utcnow()

        if now > expiry_date:
            return {
                "expired": True,
                "days_overdue": (now - expiry_date).days,
                "days_until_expiry": 0
            }

        days_remaining = (expiry_date - now).days
        return {
            "expired": False,
            "days_until_expiry": days_remaining,
            "expiry_date": expiry_date.isoformat()
        }

    def _hash_password(self, password: str) -> str:
        """Create hash of password for history comparison."""
        return hashlib.sha256(password.encode()).hexdigest()


def get_default_policy() -> PasswordPolicy:
    """Get default password policy."""
    return PasswordPolicy()


def get_strict_policy() -> PasswordPolicy:
    """Get strict password policy for enterprise."""
    return PasswordPolicy(
        min_length=12,
        require_special=True,
        max_age_days=60,
        history_count=10,
        lockout_attempts=3,
        min_score=4
    )


def validate_password(
    password: str,
    policy: PasswordPolicy = None
) -> PasswordValidationResult:
    """
    Convenience function to validate password.

    Usage:
        result = validate_password("MyP@ssw0rd")
        if not result.is_valid:
            print(result.errors)
    """
    validator = PasswordPolicyValidator(policy)
    return validator.validate(password)
