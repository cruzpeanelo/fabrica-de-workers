# -*- coding: utf-8 -*-
"""
Tests for MFA/2FA Implementation - Issue #103
Fabrica de Agentes v6.5

Comprehensive tests for:
1. MFA Setup flow
2. TOTP verification
3. Backup codes
4. Recovery process
5. Security edge cases
"""

import pytest
import pyotp
import hashlib
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime, timedelta


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_db():
    """Create mock database session"""
    return Mock()


@pytest.fixture
def mock_user():
    """Create mock user object"""
    user = Mock()
    user.id = 123
    user.username = "testuser"
    user.email = "test@example.com"
    user.quotas = {}
    return user


@pytest.fixture
def mfa_service(mock_db):
    """Create MFA service with mock DB"""
    from factory.auth.mfa import MFAService
    return MFAService(mock_db)


# =============================================================================
# SETUP TESTS
# =============================================================================

class TestMFASetup:
    """Tests for MFA setup process"""

    def test_start_setup_generates_secret(self, mock_db, mock_user):
        """Should generate secret key and backup codes"""
        from factory.auth.mfa import MFAService

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.start_setup(123)

        assert result.secret_key is not None
        assert len(result.secret_key) == 32  # Base32 encoded
        assert result.provisioning_uri is not None
        assert "otpauth://totp" in result.provisioning_uri
        assert result.qr_code_base64 is not None
        assert len(result.backup_codes) == 10

    def test_start_setup_stores_pending_data(self, mock_db, mock_user):
        """Should store pending MFA data in user record"""
        from factory.auth.mfa import MFAService, MFAStatus

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        service.start_setup(123)

        # Check pending data was stored
        assert "_mfa_pending" in mock_user.quotas
        pending = mock_user.quotas["_mfa_pending"]
        assert pending["status"] == MFAStatus.PENDING.value
        assert pending["secret_key"] is not None
        assert pending["backup_codes_hash"] is not None

    def test_complete_setup_with_valid_code(self, mock_db, mock_user):
        """Should complete setup with valid TOTP code"""
        from factory.auth.mfa import MFAService, MFAStatus

        # Set up pending MFA
        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa_pending": {
                "status": MFAStatus.PENDING.value,
                "secret_key": secret,
                "backup_codes_hash": "test_hash",
                "backup_codes_remaining": 10,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        # Generate valid code
        totp = pyotp.TOTP(secret)
        valid_code = totp.now()

        result = service.complete_setup(123, valid_code)

        assert result.success is True
        assert "_mfa" in mock_user.quotas
        assert mock_user.quotas["_mfa"]["status"] == MFAStatus.ENABLED.value

    def test_complete_setup_with_invalid_code(self, mock_db, mock_user):
        """Should reject invalid TOTP code"""
        from factory.auth.mfa import MFAService, MFAStatus

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa_pending": {
                "status": MFAStatus.PENDING.value,
                "secret_key": secret,
                "backup_codes_hash": "test_hash",
                "backup_codes_remaining": 10,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.complete_setup(123, "000000")

        assert result.success is False
        assert result.remaining_attempts is not None


# =============================================================================
# VERIFICATION TESTS
# =============================================================================

class TestMFAVerification:
    """Tests for MFA verification"""

    def test_verify_valid_totp(self, mock_db, mock_user):
        """Should verify valid TOTP code"""
        from factory.auth.mfa import MFAService, MFAStatus

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "",
                "backup_codes_remaining": 0,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        # Generate valid code
        totp = pyotp.TOTP(secret)
        valid_code = totp.now()

        result = service.verify(123, valid_code)

        assert result.success is True

    def test_verify_invalid_totp(self, mock_db, mock_user):
        """Should reject invalid TOTP code"""
        from factory.auth.mfa import MFAService, MFAStatus

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "",
                "backup_codes_remaining": 0,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.verify(123, "000000")

        assert result.success is False
        assert result.remaining_attempts is not None

    def test_verify_locks_after_max_attempts(self, mock_db, mock_user):
        """Should lock account after max failed attempts"""
        from factory.auth.mfa import MFAService, MFAStatus, MAX_MFA_ATTEMPTS

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "",
                "backup_codes_remaining": 0,
                "failed_attempts": MAX_MFA_ATTEMPTS - 1
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.verify(123, "000000")

        assert result.success is False
        assert result.remaining_attempts == 0
        assert result.locked_until is not None
        assert mock_user.quotas["_mfa"]["status"] == MFAStatus.LOCKED.value

    def test_verify_backup_code(self, mock_db, mock_user):
        """Should verify valid backup code"""
        from factory.auth.mfa import MFAService, MFAStatus, MFAMethod

        # Create backup code and hash it
        backup_code = "ABCD-1234"
        code_hash = hashlib.sha256(backup_code.replace("-", "").encode()).hexdigest()

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": pyotp.random_base32(),
                "backup_codes_hash": code_hash,
                "backup_codes_remaining": 1,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.verify(123, "ABCD1234", method=MFAMethod.BACKUP)

        assert result.success is True

    def test_backup_code_consumed_after_use(self, mock_db, mock_user):
        """Backup code should be removed after use"""
        from factory.auth.mfa import MFAService, MFAStatus, MFAMethod

        backup_code = "ABCD1234"
        code_hash = hashlib.sha256(backup_code.encode()).hexdigest()

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": pyotp.random_base32(),
                "backup_codes_hash": code_hash,
                "backup_codes_remaining": 1,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        # First use should work
        result = service.verify(123, backup_code, method=MFAMethod.BACKUP)
        assert result.success is True

        # Second use should fail (code consumed)
        result = service.verify(123, backup_code, method=MFAMethod.BACKUP)
        assert result.success is False


# =============================================================================
# BACKUP CODE TESTS
# =============================================================================

class TestBackupCodes:
    """Tests for backup code management"""

    def test_generate_backup_codes_format(self):
        """Should generate codes in XXXX-XXXX format"""
        from factory.auth.mfa import MFAService

        service = MFAService()
        codes = service._generate_backup_codes()

        assert len(codes) == 10
        for code in codes:
            assert len(code) == 9  # XXXX-XXXX
            assert code[4] == "-"
            assert code[:4].isalnum()
            assert code[5:].isalnum()

    def test_backup_codes_are_unique(self):
        """All backup codes should be unique"""
        from factory.auth.mfa import MFAService

        service = MFAService()
        codes = service._generate_backup_codes()

        assert len(codes) == len(set(codes))

    def test_regenerate_backup_codes(self, mock_db, mock_user):
        """Should regenerate backup codes with valid TOTP"""
        from factory.auth.mfa import MFAService, MFAStatus

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "old_hash",
                "backup_codes_remaining": 5,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        totp = pyotp.TOTP(secret)
        valid_code = totp.now()

        result = service.regenerate_backup_codes(123, valid_code)

        assert result["success"] is True
        assert len(result["backup_codes"]) == 10
        assert mock_user.quotas["_mfa"]["backup_codes_remaining"] == 10


# =============================================================================
# STATUS TESTS
# =============================================================================

class TestMFAStatus:
    """Tests for MFA status checks"""

    def test_status_disabled(self, mock_db, mock_user):
        """Should return disabled status when MFA not set up"""
        from factory.auth.mfa import MFAService, MFAStatus

        mock_user.quotas = {}
        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        status = service.get_status(123)

        assert status["enabled"] is False
        assert status["status"] == MFAStatus.DISABLED.value

    def test_status_pending(self, mock_db, mock_user):
        """Should return pending status during setup"""
        from factory.auth.mfa import MFAService, MFAStatus

        mock_user.quotas = {
            "_mfa_pending": {
                "status": MFAStatus.PENDING.value,
                "setup_started_at": datetime.utcnow().isoformat()
            }
        }
        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        status = service.get_status(123)

        assert status["enabled"] is False
        assert status["status"] == MFAStatus.PENDING.value

    def test_status_enabled(self, mock_db, mock_user):
        """Should return enabled status when MFA active"""
        from factory.auth.mfa import MFAService, MFAStatus

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "enabled_at": datetime.utcnow().isoformat(),
                "backup_codes_remaining": 8
            }
        }
        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        status = service.get_status(123)

        assert status["enabled"] is True
        assert status["status"] == MFAStatus.ENABLED.value
        assert status["backup_codes_remaining"] == 8

    def test_is_mfa_required_for_admin(self, mock_db, mock_user):
        """MFA should be required for admin users"""
        from factory.auth.mfa import MFAService

        mock_user.role = "ADMIN"
        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        result = service.is_mfa_required(123)

        assert result is True


# =============================================================================
# RECOVERY TESTS
# =============================================================================

class TestMFARecovery:
    """Tests for MFA recovery process"""

    def test_start_recovery_creates_token(self, mock_db, mock_user):
        """Should create recovery token"""
        from factory.auth.mfa import MFAService, MFAStatus

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value
            }
        }
        mock_db.query.return_value.filter.return_value.first.return_value = mock_user

        with patch.dict('os.environ', {'ENV': 'development'}):
            service = MFAService(mock_db)
            result = service.start_recovery(123, "test@example.com")

        assert result["success"] is True
        assert "_mfa_recovery" in mock_user.quotas

    def test_start_recovery_no_reveal_invalid_email(self, mock_db):
        """Should not reveal if email is invalid"""
        from factory.auth.mfa import MFAService

        mock_db.query.return_value.filter.return_value.first.return_value = None
        service = MFAService(mock_db)

        result = service.start_recovery(123, "invalid@example.com")

        # Should return success even for invalid email
        assert result["success"] is True
        assert "If the email is valid" in result["message"]

    def test_complete_recovery_disables_mfa(self, mock_db, mock_user):
        """Should disable MFA with valid recovery token"""
        from factory.auth.mfa import MFAService, MFAStatus
        import secrets

        token = secrets.token_urlsafe(32)
        token_hash = hashlib.sha256(token.encode()).hexdigest()

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value
            },
            "_mfa_recovery": {
                "token": token_hash,
                "expires": (datetime.utcnow() + timedelta(hours=1)).isoformat()
            }
        }
        mock_db.query.return_value.all.return_value = [mock_user]
        service = MFAService(mock_db)

        result = service.complete_recovery(token)

        assert result["success"] is True
        assert "_mfa" not in mock_user.quotas

    def test_complete_recovery_expired_token(self, mock_db, mock_user):
        """Should reject expired recovery token"""
        from factory.auth.mfa import MFAService, MFAStatus
        import secrets

        token = secrets.token_urlsafe(32)
        token_hash = hashlib.sha256(token.encode()).hexdigest()

        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value
            },
            "_mfa_recovery": {
                "token": token_hash,
                "expires": (datetime.utcnow() - timedelta(hours=1)).isoformat()  # Expired
            }
        }
        mock_db.query.return_value.all.return_value = [mock_user]
        service = MFAService(mock_db)

        result = service.complete_recovery(token)

        assert result["success"] is False
        assert "expired" in result["message"].lower()


# =============================================================================
# QR CODE TESTS
# =============================================================================

class TestQRCode:
    """Tests for QR code generation"""

    def test_generate_qr_code(self):
        """Should generate valid base64 QR code"""
        from factory.auth.mfa import MFAService

        service = MFAService()
        qr_data = service._generate_qr_code("otpauth://totp/Test?secret=ABC123")

        # Should be valid base64
        import base64
        try:
            decoded = base64.b64decode(qr_data)
            # Check PNG header
            assert decoded[:8] == b'\x89PNG\r\n\x1a\n'
        except Exception:
            pytest.fail("QR code is not valid base64 PNG")


# =============================================================================
# SECURITY TESTS
# =============================================================================

class TestMFASecurity:
    """Tests for MFA security measures"""

    def test_rate_limiting(self, mock_db, mock_user):
        """Should implement rate limiting"""
        from factory.auth.mfa import MFAService, MFAStatus, MAX_MFA_ATTEMPTS

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "",
                "backup_codes_remaining": 0,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        # Try multiple invalid codes
        for i in range(MAX_MFA_ATTEMPTS):
            result = service.verify(123, "000000")

        # Should be locked
        assert mock_user.quotas["_mfa"]["status"] == MFAStatus.LOCKED.value

    def test_totp_time_window(self, mock_db, mock_user):
        """Should accept codes within valid time window"""
        from factory.auth.mfa import MFAService, MFAStatus, TOTP_VALID_WINDOW

        secret = pyotp.random_base32()
        mock_user.quotas = {
            "_mfa": {
                "status": MFAStatus.ENABLED.value,
                "secret_key": secret,
                "backup_codes_hash": "",
                "backup_codes_remaining": 0,
                "failed_attempts": 0
            }
        }

        mock_db.query.return_value.filter.return_value.first.return_value = mock_user
        service = MFAService(mock_db)

        # Current code should work
        totp = pyotp.TOTP(secret)
        result = service.verify(123, totp.now())
        assert result.success is True


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
