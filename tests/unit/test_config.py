# -*- coding: utf-8 -*-
"""
Unit Tests for Config Package - Issue #210
Plataforma E v6.5

Tests for factory.config package - feature flags, security, and settings.
"""

import pytest
import os
from unittest.mock import patch, MagicMock


class TestFeatureFlags:
    """Tests for feature flag functions"""

    @pytest.mark.unit
    def test_get_current_environment_returns_string(self):
        """Should return environment string"""
        from factory.config import get_current_environment

        env = get_current_environment()

        assert isinstance(env, str)
        assert env in ["development", "staging", "production"]

    @pytest.mark.unit
    def test_is_feature_enabled_returns_bool(self):
        """Should return boolean for feature check"""
        from factory.config import is_feature_enabled

        result = is_feature_enabled("some_feature")

        assert isinstance(result, bool)

    @pytest.mark.unit
    def test_get_all_flags_returns_list(self):
        """Should return list of all flags"""
        from factory.config import get_all_flags

        flags = get_all_flags()

        assert isinstance(flags, list)
        assert len(flags) > 0

    @pytest.mark.unit
    def test_get_flags_for_tenant_returns_dict(self):
        """Should return flags for specific tenant"""
        from factory.config import get_flags_for_tenant

        flags = get_flags_for_tenant("test-tenant")

        assert isinstance(flags, dict)

    @pytest.mark.unit
    def test_get_variant_returns_string_or_none(self):
        """Should return variant string or None"""
        from factory.config import get_variant

        result = get_variant("some_feature")

        assert result is None or isinstance(result, str)


class TestFeatureFlagEnums:
    """Tests for feature flag enums and data classes"""

    @pytest.mark.unit
    def test_environment_enum_values(self):
        """Should have development, staging, production"""
        from factory.config import Environment

        assert hasattr(Environment, "DEVELOPMENT")
        assert hasattr(Environment, "STAGING")
        assert hasattr(Environment, "PRODUCTION")

    @pytest.mark.unit
    def test_flag_type_enum_exists(self):
        """Should have FlagType enum"""
        from factory.config import FlagType

        assert hasattr(FlagType, "BOOLEAN")

    @pytest.mark.unit
    def test_flag_status_enum_exists(self):
        """Should have FlagStatus enum"""
        from factory.config import FlagStatus

        assert hasattr(FlagStatus, "ACTIVE")


class TestFeatureFlagsManager:
    """Tests for FeatureFlagsManager class"""

    @pytest.mark.unit
    def test_get_manager_returns_manager(self):
        """Should return a manager instance"""
        from factory.config import get_manager, FeatureFlagsManager

        manager = get_manager()

        assert manager is not None
        assert isinstance(manager, FeatureFlagsManager)

    @pytest.mark.unit
    def test_manager_is_singleton(self):
        """Should return same instance"""
        from factory.config import get_manager

        manager1 = get_manager()
        manager2 = get_manager()

        assert manager1 is manager2


class TestPasswordValidation:
    """Tests for password validation functions"""

    @pytest.mark.unit
    def test_validate_password_strength_returns_tuple(self):
        """Should return (bool, str) tuple"""
        from factory.config import validate_password_strength

        result = validate_password_strength("testpassword")

        assert isinstance(result, tuple)
        assert len(result) == 2
        assert isinstance(result[0], bool)
        assert isinstance(result[1], str)

    @pytest.mark.unit
    def test_validate_password_strength_dev_accepts_any(self):
        """In development, any password should be accepted"""
        from factory.config import validate_password_strength

        is_valid, error = validate_password_strength("123", is_prod=False)

        assert is_valid is True
        assert error == ""

    @pytest.mark.unit
    def test_validate_password_strength_prod_rejects_short(self):
        """In production, short passwords should be rejected"""
        from factory.config import validate_password_strength, PASSWORD_MIN_LENGTH

        short_password = "a" * (PASSWORD_MIN_LENGTH - 1)
        is_valid, error = validate_password_strength(short_password, is_prod=True)

        assert is_valid is False
        assert "mínimo" in error.lower() or "minimo" in error.lower()

    @pytest.mark.unit
    def test_password_min_length_is_positive(self):
        """PASSWORD_MIN_LENGTH should be positive"""
        from factory.config import PASSWORD_MIN_LENGTH

        assert PASSWORD_MIN_LENGTH > 0


class TestCredentialBlocking:
    """Tests for credential blocking"""

    @pytest.mark.unit
    def test_is_credential_blocked_returns_bool(self):
        """Should return boolean"""
        from factory.config import is_credential_blocked

        result = is_credential_blocked("user", "password")

        assert isinstance(result, bool)

    @pytest.mark.unit
    def test_blocked_credentials_is_list(self):
        """BLOCKED_CREDENTIALS should be a list or tuple"""
        from factory.config import BLOCKED_CREDENTIALS

        assert isinstance(BLOCKED_CREDENTIALS, (list, tuple))


class TestPasswordRequirements:
    """Tests for password requirements function"""

    @pytest.mark.unit
    def test_get_password_requirements_returns_dict(self):
        """Should return requirements dict"""
        from factory.config import get_password_requirements

        reqs = get_password_requirements()

        assert isinstance(reqs, dict)

    @pytest.mark.unit
    def test_get_password_requirements_has_min_length(self):
        """Should include min_length"""
        from factory.config import get_password_requirements

        reqs = get_password_requirements()

        assert "min_length" in reqs


class TestOverrideFunctions:
    """Tests for flag override functions"""

    @pytest.mark.unit
    def test_override_flag_for_tenant_callable(self):
        """Should be callable"""
        from factory.config import override_flag_for_tenant

        assert callable(override_flag_for_tenant)

    @pytest.mark.unit
    def test_override_flag_for_user_callable(self):
        """Should be callable"""
        from factory.config import override_flag_for_user

        assert callable(override_flag_for_user)


class TestFeatureFlagDataClass:
    """Tests for FeatureFlag data class"""

    @pytest.mark.unit
    def test_feature_flag_exists(self):
        """Should have FeatureFlag class"""
        from factory.config import FeatureFlag

        assert FeatureFlag is not None

    @pytest.mark.unit
    def test_feature_flag_is_dataclass(self):
        """Should be a dataclass or class"""
        from factory.config import FeatureFlag

        # Check it's a class
        assert isinstance(FeatureFlag, type)


class TestFeatureFlagsDashboard:
    """Tests for FeatureFlagsDashboard class"""

    @pytest.mark.unit
    def test_dashboard_exists(self):
        """Should have FeatureFlagsDashboard class"""
        from factory.config import FeatureFlagsDashboard

        assert FeatureFlagsDashboard is not None

    @pytest.mark.unit
    def test_dashboard_is_class(self):
        """Should be a class"""
        from factory.config import FeatureFlagsDashboard

        assert isinstance(FeatureFlagsDashboard, type)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
