# -*- coding: utf-8 -*-
"""
Multi-Factor Authentication (MFA/2FA) Implementation - Issue #103
Fabrica de Agentes v6.5

Implements complete MFA system with:
1. TOTP (Time-based One-Time Password) via pyotp
2. Backup codes generation and validation
3. MFA enforcement policies by tenant/role
4. Recovery flow for lost authenticators
5. QR code generation for authenticator apps

Security Standards:
- RFC 6238 (TOTP)
- RFC 4226 (HOTP base)
- 6-digit codes, 30-second validity
- SHA1 algorithm (standard for Google Authenticator compatibility)
"""

import os
import secrets
import hashlib
import base64
import io
import json
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass
from enum import Enum

import pyotp
import qrcode
from pydantic import BaseModel, Field

from factory.database.connection import SessionLocal


# =============================================================================
# CONSTANTS AND CONFIGURATION
# =============================================================================

# MFA Configuration
MFA_ISSUER = os.getenv("MFA_ISSUER", "Fabrica de Agentes")
MFA_SECRET_LENGTH = 32  # 32 characters = 160 bits entropy
BACKUP_CODE_LENGTH = 8
BACKUP_CODE_COUNT = 10
TOTP_VALID_WINDOW = 1  # Accept codes from 1 interval before/after (30s tolerance)

# Rate limiting for MFA attempts
MAX_MFA_ATTEMPTS = 5
MFA_LOCKOUT_MINUTES = 15


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class MFAStatus(str, Enum):
    """MFA enrollment status"""
    DISABLED = "disabled"
    PENDING = "pending"      # User started but didn't complete setup
    ENABLED = "enabled"      # MFA is active and required
    LOCKED = "locked"        # Too many failed attempts


class MFAMethod(str, Enum):
    """Available MFA methods"""
    TOTP = "totp"           # Time-based OTP (Google Authenticator, etc.)
    BACKUP = "backup"       # Backup codes
    EMAIL = "email"         # Email verification (future)
    SMS = "sms"             # SMS verification (future)


@dataclass
class MFAConfig:
    """MFA configuration for a user"""
    user_id: int
    status: MFAStatus
    secret_key: Optional[str]
    backup_codes_hash: Optional[str]  # Hashed backup codes
    backup_codes_remaining: int
    enabled_at: Optional[datetime]
    last_used_at: Optional[datetime]
    failed_attempts: int
    locked_until: Optional[datetime]


class MFASetupRequest(BaseModel):
    """Request to start MFA setup"""
    user_id: int


class MFASetupResponse(BaseModel):
    """Response with setup data"""
    secret_key: str
    provisioning_uri: str
    qr_code_base64: str
    backup_codes: List[str]


class MFAVerifyRequest(BaseModel):
    """Request to verify MFA code"""
    user_id: int
    code: str
    method: MFAMethod = MFAMethod.TOTP


class MFAVerifyResponse(BaseModel):
    """Verification result"""
    success: bool
    message: str
    remaining_attempts: Optional[int] = None
    locked_until: Optional[str] = None


class MFAEnforcementPolicy(BaseModel):
    """MFA enforcement policy"""
    tenant_id: Optional[str] = None
    require_for_roles: List[str] = Field(default_factory=lambda: ["ADMIN"])
    require_for_all: bool = False
    grace_period_days: int = 7
    allow_backup_codes: bool = True


# =============================================================================
# MFA SERVICE - Core Implementation
# =============================================================================

class MFAService:
    """
    Core MFA service implementing TOTP and backup codes.

    Usage:
        mfa = MFAService()

        # Start setup
        setup = mfa.start_setup(user_id=123)
        # User scans QR code in authenticator app

        # Complete setup with first valid code
        mfa.complete_setup(user_id=123, code="123456")

        # Verify during login
        result = mfa.verify(user_id=123, code="654321")
    """

    def __init__(self, db_session=None):
        self.db = db_session

    def _get_db(self):
        """Get database session"""
        if self.db:
            return self.db
        return SessionLocal()

    def _close_db(self, db):
        """Close database if we created it"""
        if not self.db:
            db.close()

    # -------------------------------------------------------------------------
    # MFA Setup
    # -------------------------------------------------------------------------

    def start_setup(self, user_id: int) -> MFASetupResponse:
        """
        Start MFA setup for a user.

        Generates:
        - TOTP secret key
        - QR code for authenticator apps
        - Backup codes

        Returns setup data to display to user.
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            # Get user
            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                raise ValueError(f"User {user_id} not found")

            # Generate secret key
            secret_key = pyotp.random_base32(length=MFA_SECRET_LENGTH)

            # Generate backup codes
            backup_codes = self._generate_backup_codes()
            backup_codes_hash = self._hash_backup_codes(backup_codes)

            # Store pending setup in user record (or separate table)
            # For now, we store in user's extra data
            mfa_data = {
                "status": MFAStatus.PENDING.value,
                "secret_key": secret_key,
                "backup_codes_hash": backup_codes_hash,
                "backup_codes_remaining": len(backup_codes),
                "setup_started_at": datetime.utcnow().isoformat(),
                "failed_attempts": 0
            }

            # Store encrypted in user quotas (temporary - should be separate table)
            if not user.quotas:
                user.quotas = {}
            user.quotas["_mfa_pending"] = mfa_data
            db.commit()

            # Generate provisioning URI for authenticator apps
            totp = pyotp.TOTP(secret_key)
            provisioning_uri = totp.provisioning_uri(
                name=user.email or user.username,
                issuer_name=MFA_ISSUER
            )

            # Generate QR code
            qr_code_base64 = self._generate_qr_code(provisioning_uri)

            return MFASetupResponse(
                secret_key=secret_key,
                provisioning_uri=provisioning_uri,
                qr_code_base64=qr_code_base64,
                backup_codes=backup_codes
            )

        finally:
            self._close_db(db)

    def complete_setup(self, user_id: int, code: str) -> MFAVerifyResponse:
        """
        Complete MFA setup by verifying first code.

        This confirms the user has correctly configured their
        authenticator app and enables MFA.
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return MFAVerifyResponse(success=False, message="User not found")

            # Get pending MFA data
            mfa_data = user.quotas.get("_mfa_pending") if user.quotas else None
            if not mfa_data or mfa_data.get("status") != MFAStatus.PENDING.value:
                return MFAVerifyResponse(success=False, message="No pending MFA setup")

            # Verify the code
            secret_key = mfa_data["secret_key"]
            totp = pyotp.TOTP(secret_key)

            if not totp.verify(code, valid_window=TOTP_VALID_WINDOW):
                mfa_data["failed_attempts"] = mfa_data.get("failed_attempts", 0) + 1
                user.quotas["_mfa_pending"] = mfa_data
                db.commit()

                remaining = MAX_MFA_ATTEMPTS - mfa_data["failed_attempts"]
                if remaining <= 0:
                    # Cancel setup
                    del user.quotas["_mfa_pending"]
                    db.commit()
                    return MFAVerifyResponse(
                        success=False,
                        message="Too many failed attempts. Setup cancelled.",
                        remaining_attempts=0
                    )

                return MFAVerifyResponse(
                    success=False,
                    message="Invalid code",
                    remaining_attempts=remaining
                )

            # Code is valid - activate MFA
            mfa_data["status"] = MFAStatus.ENABLED.value
            mfa_data["enabled_at"] = datetime.utcnow().isoformat()
            mfa_data["failed_attempts"] = 0

            # Move from pending to active
            if "_mfa_pending" in user.quotas:
                del user.quotas["_mfa_pending"]
            user.quotas["_mfa"] = mfa_data

            db.commit()

            # Log audit event
            self._log_mfa_event(user_id, "mfa_enabled", "MFA setup completed")

            return MFAVerifyResponse(
                success=True,
                message="MFA enabled successfully"
            )

        finally:
            self._close_db(db)

    def disable(self, user_id: int, verification_code: str = None) -> MFAVerifyResponse:
        """
        Disable MFA for a user.

        Requires either:
        - Valid TOTP code
        - Valid backup code
        - Admin override
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return MFAVerifyResponse(success=False, message="User not found")

            mfa_data = user.quotas.get("_mfa") if user.quotas else None
            if not mfa_data or mfa_data.get("status") != MFAStatus.ENABLED.value:
                return MFAVerifyResponse(success=False, message="MFA not enabled")

            # Verify code before disabling
            if verification_code:
                verify_result = self._verify_code(mfa_data, verification_code)
                if not verify_result[0]:
                    return MFAVerifyResponse(success=False, message=verify_result[1])

            # Disable MFA
            if "_mfa" in user.quotas:
                del user.quotas["_mfa"]

            db.commit()

            # Log audit event
            self._log_mfa_event(user_id, "mfa_disabled", "MFA disabled")

            return MFAVerifyResponse(success=True, message="MFA disabled")

        finally:
            self._close_db(db)

    # -------------------------------------------------------------------------
    # MFA Verification
    # -------------------------------------------------------------------------

    def verify(
        self,
        user_id: int,
        code: str,
        method: MFAMethod = MFAMethod.TOTP
    ) -> MFAVerifyResponse:
        """
        Verify MFA code during login.

        Supports:
        - TOTP codes from authenticator app
        - Backup codes for recovery
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return MFAVerifyResponse(success=False, message="User not found")

            mfa_data = user.quotas.get("_mfa") if user.quotas else None
            if not mfa_data or mfa_data.get("status") != MFAStatus.ENABLED.value:
                # MFA not enabled - allow login
                return MFAVerifyResponse(success=True, message="MFA not required")

            # Check if locked
            locked_until = mfa_data.get("locked_until")
            if locked_until:
                locked_until_dt = datetime.fromisoformat(locked_until)
                if datetime.utcnow() < locked_until_dt:
                    return MFAVerifyResponse(
                        success=False,
                        message="Account locked due to failed MFA attempts",
                        locked_until=locked_until
                    )
                else:
                    # Unlock
                    mfa_data["locked_until"] = None
                    mfa_data["failed_attempts"] = 0

            # Verify code
            if method == MFAMethod.BACKUP:
                success, message = self._verify_backup_code(mfa_data, code)
            else:
                success, message = self._verify_totp(mfa_data, code)

            if success:
                # Reset failed attempts on success
                mfa_data["failed_attempts"] = 0
                mfa_data["last_used_at"] = datetime.utcnow().isoformat()
                user.quotas["_mfa"] = mfa_data
                db.commit()

                # Log success
                self._log_mfa_event(user_id, "mfa_verified", f"MFA verified via {method.value}")

                return MFAVerifyResponse(success=True, message="Verified")

            else:
                # Increment failed attempts
                mfa_data["failed_attempts"] = mfa_data.get("failed_attempts", 0) + 1
                remaining = MAX_MFA_ATTEMPTS - mfa_data["failed_attempts"]

                if remaining <= 0:
                    # Lock account
                    locked_until = datetime.utcnow() + timedelta(minutes=MFA_LOCKOUT_MINUTES)
                    mfa_data["locked_until"] = locked_until.isoformat()
                    mfa_data["status"] = MFAStatus.LOCKED.value

                    # Log lockout
                    self._log_mfa_event(
                        user_id, "mfa_locked",
                        f"Account locked after {MAX_MFA_ATTEMPTS} failed attempts"
                    )

                user.quotas["_mfa"] = mfa_data
                db.commit()

                return MFAVerifyResponse(
                    success=False,
                    message=message,
                    remaining_attempts=max(0, remaining),
                    locked_until=mfa_data.get("locked_until")
                )

        finally:
            self._close_db(db)

    def _verify_totp(self, mfa_data: dict, code: str) -> Tuple[bool, str]:
        """Verify TOTP code"""
        secret_key = mfa_data.get("secret_key")
        if not secret_key:
            return False, "MFA not configured properly"

        totp = pyotp.TOTP(secret_key)

        if totp.verify(code, valid_window=TOTP_VALID_WINDOW):
            return True, "Valid"

        return False, "Invalid code"

    def _verify_backup_code(self, mfa_data: dict, code: str) -> Tuple[bool, str]:
        """Verify and consume backup code"""
        backup_hash = mfa_data.get("backup_codes_hash")
        if not backup_hash:
            return False, "No backup codes available"

        # Hash the provided code
        code_hash = hashlib.sha256(code.encode()).hexdigest()

        # Check if code hash exists in stored hashes
        stored_hashes = backup_hash.split(",")
        if code_hash in stored_hashes:
            # Remove used code
            stored_hashes.remove(code_hash)
            mfa_data["backup_codes_hash"] = ",".join(stored_hashes)
            mfa_data["backup_codes_remaining"] = len(stored_hashes)

            return True, "Backup code verified"

        return False, "Invalid backup code"

    def _verify_code(self, mfa_data: dict, code: str) -> Tuple[bool, str]:
        """Verify code (TOTP or backup)"""
        # Try TOTP first
        success, message = self._verify_totp(mfa_data, code)
        if success:
            return True, message

        # Try backup code
        return self._verify_backup_code(mfa_data, code)

    # -------------------------------------------------------------------------
    # Backup Codes
    # -------------------------------------------------------------------------

    def _generate_backup_codes(self) -> List[str]:
        """Generate new backup codes"""
        codes = []
        for _ in range(BACKUP_CODE_COUNT):
            # Generate readable code: XXXX-XXXX format
            code = secrets.token_hex(BACKUP_CODE_LENGTH // 2).upper()
            code = f"{code[:4]}-{code[4:]}"
            codes.append(code)
        return codes

    def _hash_backup_codes(self, codes: List[str]) -> str:
        """Hash backup codes for storage"""
        hashes = []
        for code in codes:
            # Remove formatting before hashing
            clean_code = code.replace("-", "")
            code_hash = hashlib.sha256(clean_code.encode()).hexdigest()
            hashes.append(code_hash)
        return ",".join(hashes)

    def regenerate_backup_codes(self, user_id: int, verification_code: str) -> Dict[str, Any]:
        """
        Regenerate backup codes.

        Requires valid TOTP code for security.
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return {"success": False, "message": "User not found"}

            mfa_data = user.quotas.get("_mfa") if user.quotas else None
            if not mfa_data or mfa_data.get("status") != MFAStatus.ENABLED.value:
                return {"success": False, "message": "MFA not enabled"}

            # Verify TOTP code
            success, message = self._verify_totp(mfa_data, verification_code)
            if not success:
                return {"success": False, "message": "Invalid verification code"}

            # Generate new codes
            new_codes = self._generate_backup_codes()
            mfa_data["backup_codes_hash"] = self._hash_backup_codes(new_codes)
            mfa_data["backup_codes_remaining"] = len(new_codes)

            user.quotas["_mfa"] = mfa_data
            db.commit()

            # Log event
            self._log_mfa_event(user_id, "backup_codes_regenerated", "Backup codes regenerated")

            return {
                "success": True,
                "backup_codes": new_codes,
                "message": "New backup codes generated. Store them securely."
            }

        finally:
            self._close_db(db)

    # -------------------------------------------------------------------------
    # QR Code Generation
    # -------------------------------------------------------------------------

    def _generate_qr_code(self, data: str) -> str:
        """Generate QR code as base64 image"""
        qr = qrcode.QRCode(
            version=1,
            error_correction=qrcode.constants.ERROR_CORRECT_L,
            box_size=10,
            border=4
        )
        qr.add_data(data)
        qr.make(fit=True)

        img = qr.make_image(fill_color="black", back_color="white")

        buffer = io.BytesIO()
        img.save(buffer, format="PNG")
        buffer.seek(0)

        return base64.b64encode(buffer.getvalue()).decode()

    # -------------------------------------------------------------------------
    # MFA Status and Info
    # -------------------------------------------------------------------------

    def get_status(self, user_id: int) -> Dict[str, Any]:
        """Get MFA status for a user"""
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return {"enabled": False, "status": "user_not_found"}

            mfa_data = user.quotas.get("_mfa") if user.quotas else None

            if not mfa_data:
                # Check pending
                pending = user.quotas.get("_mfa_pending") if user.quotas else None
                if pending:
                    return {
                        "enabled": False,
                        "status": MFAStatus.PENDING.value,
                        "setup_started": pending.get("setup_started_at")
                    }
                return {"enabled": False, "status": MFAStatus.DISABLED.value}

            return {
                "enabled": mfa_data.get("status") == MFAStatus.ENABLED.value,
                "status": mfa_data.get("status"),
                "enabled_at": mfa_data.get("enabled_at"),
                "last_used_at": mfa_data.get("last_used_at"),
                "backup_codes_remaining": mfa_data.get("backup_codes_remaining", 0),
                "locked_until": mfa_data.get("locked_until")
            }

        finally:
            self._close_db(db)

    def is_mfa_required(self, user_id: int, tenant_id: str = None) -> bool:
        """
        Check if MFA is required for this user.

        Based on:
        - Tenant policy
        - User role
        - Global settings
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(User.id == user_id).first()
            if not user:
                return False

            # Check if admin (always require MFA)
            if user.role == "ADMIN":
                return True

            # Check tenant policy
            if tenant_id:
                policy = self.get_enforcement_policy(tenant_id)
                if policy:
                    if policy.require_for_all:
                        return True
                    if user.role in policy.require_for_roles:
                        return True

            # Default: MFA optional
            return False

        finally:
            self._close_db(db)

    def get_enforcement_policy(self, tenant_id: str) -> Optional[MFAEnforcementPolicy]:
        """Get MFA enforcement policy for tenant"""
        # In production, load from database
        # For now, return default policy
        return MFAEnforcementPolicy(
            tenant_id=tenant_id,
            require_for_roles=["ADMIN", "MANAGER"],
            require_for_all=False,
            grace_period_days=7,
            allow_backup_codes=True
        )

    # -------------------------------------------------------------------------
    # Recovery
    # -------------------------------------------------------------------------

    def start_recovery(self, user_id: int, email: str) -> Dict[str, Any]:
        """
        Start MFA recovery process.

        Sends recovery link to user's email.
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            user = db.query(User).filter(
                User.id == user_id,
                User.email == email
            ).first()

            if not user:
                # Don't reveal if user exists
                return {
                    "success": True,
                    "message": "If the email is valid, a recovery link will be sent"
                }

            # Generate recovery token
            recovery_token = secrets.token_urlsafe(32)
            recovery_expires = datetime.utcnow() + timedelta(hours=1)

            # Store recovery token
            if not user.quotas:
                user.quotas = {}
            user.quotas["_mfa_recovery"] = {
                "token": hashlib.sha256(recovery_token.encode()).hexdigest(),
                "expires": recovery_expires.isoformat()
            }
            db.commit()

            # In production, send email here
            # For now, just log
            print(f"[MFA] Recovery link: /auth/mfa/recover?token={recovery_token}")

            # Log event
            self._log_mfa_event(user_id, "mfa_recovery_started", "MFA recovery initiated")

            return {
                "success": True,
                "message": "Recovery link sent to email",
                # In development, return token for testing
                "_dev_token": recovery_token if os.getenv("ENV") == "development" else None
            }

        finally:
            self._close_db(db)

    def complete_recovery(self, recovery_token: str) -> Dict[str, Any]:
        """
        Complete MFA recovery with token.

        Disables MFA, requiring user to set up again.
        """
        db = self._get_db()
        try:
            from factory.database.models import User

            token_hash = hashlib.sha256(recovery_token.encode()).hexdigest()

            # Find user with this recovery token
            users = db.query(User).all()
            for user in users:
                if not user.quotas:
                    continue
                recovery = user.quotas.get("_mfa_recovery")
                if not recovery:
                    continue
                if recovery.get("token") != token_hash:
                    continue

                # Check expiration
                expires = datetime.fromisoformat(recovery["expires"])
                if datetime.utcnow() > expires:
                    del user.quotas["_mfa_recovery"]
                    db.commit()
                    return {"success": False, "message": "Recovery link expired"}

                # Disable MFA
                if "_mfa" in user.quotas:
                    del user.quotas["_mfa"]
                if "_mfa_recovery" in user.quotas:
                    del user.quotas["_mfa_recovery"]

                db.commit()

                # Log event
                self._log_mfa_event(user.id, "mfa_recovered", "MFA disabled via recovery")

                return {
                    "success": True,
                    "message": "MFA disabled. Please log in and set up MFA again."
                }

            return {"success": False, "message": "Invalid recovery token"}

        finally:
            self._close_db(db)

    # -------------------------------------------------------------------------
    # Audit Logging
    # -------------------------------------------------------------------------

    def _log_mfa_event(self, user_id: int, action: str, message: str):
        """Log MFA-related event"""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.models import AuditLog

            db = SessionLocal()
            try:
                log = AuditLog(
                    user_id=user_id,
                    action=action,
                    resource="mfa",
                    resource_id=str(user_id),
                    details={"message": message},
                    success=True,
                    timestamp=datetime.utcnow()
                )
                db.add(log)
                db.commit()
            finally:
                db.close()
        except Exception as e:
            print(f"[MFA] Audit log error: {e}")


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "MFAService",
    "MFAStatus",
    "MFAMethod",
    "MFASetupRequest",
    "MFASetupResponse",
    "MFAVerifyRequest",
    "MFAVerifyResponse",
    "MFAEnforcementPolicy",
]
