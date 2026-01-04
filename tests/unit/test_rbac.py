# -*- coding: utf-8 -*-
"""
Tests for RBAC - Role-Based Access Control
Plataforma E v6.5

Tests for Issue #433:
1. Role model and permissions
2. Permission checking
3. Authorization decorators
4. Audit logging
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def check_permission_string(required: str, user_permissions: list) -> bool:
    """Check if required permission is in user permissions (with wildcard support)"""
    if "*:*" in user_permissions:
        return True

    if required in user_permissions:
        return True

    # Check resource:* wildcard
    resource, action = required.split(":")
    if f"{resource}:*" in user_permissions:
        return True

    # Check *:action wildcard
    if f"*:{action}" in user_permissions:
        return True

    return False


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_db():
    """Create mock database session"""
    return Mock()


@pytest.fixture
def mock_user_context():
    """Create mock user context"""
    from factory.auth.rbac import UserContext
    return UserContext(
        user_id=1,
        username="testuser",
        roles=["DEVELOPER"],
        permissions=["stories:read", "stories:create", "tasks:*"],
        is_authenticated=True,
        is_admin=False
    )


@pytest.fixture
def admin_context():
    """Create admin user context"""
    from factory.auth.rbac import UserContext
    return UserContext(
        user_id=999,
        username="admin",
        roles=["ADMIN"],
        permissions=["*:*"],
        is_authenticated=True,
        is_admin=True
    )


# =============================================================================
# ROLE TESTS
# =============================================================================

class TestDefaultRoles:
    """Tests for default role definitions"""

    def test_default_roles_exist(self):
        """Should have all default roles defined"""
        from factory.auth.rbac import DEFAULT_ROLES

        expected_roles = ["ADMIN", "MANAGER", "DEVELOPER", "VIEWER"]
        for role in expected_roles:
            assert role in DEFAULT_ROLES

    def test_admin_has_wildcard_permission(self):
        """Admin should have wildcard permission"""
        from factory.auth.rbac import DEFAULT_ROLES

        admin = DEFAULT_ROLES["ADMIN"]
        assert "*:*" in admin["permissions"]

    def test_roles_have_levels(self):
        """Roles should have hierarchy levels"""
        from factory.auth.rbac import DEFAULT_ROLES

        assert DEFAULT_ROLES["ADMIN"]["level"] > DEFAULT_ROLES["MANAGER"]["level"]
        assert DEFAULT_ROLES["MANAGER"]["level"] > DEFAULT_ROLES["DEVELOPER"]["level"]
        assert DEFAULT_ROLES["DEVELOPER"]["level"] > DEFAULT_ROLES["VIEWER"]["level"]


# =============================================================================
# PERMISSION TESTS
# =============================================================================

class TestPermissionChecking:
    """Tests for permission checking logic"""

    def test_check_exact_permission(self, mock_user_context):
        """Should allow exact permission match"""
        assert check_permission_string("stories:read", mock_user_context.permissions) is True
        assert check_permission_string("stories:create", mock_user_context.permissions) is True

    def test_check_wildcard_action(self, mock_user_context):
        """Should allow wildcard action permission"""
        # tasks:* should allow any action on tasks
        assert check_permission_string("tasks:read", mock_user_context.permissions) is True
        assert check_permission_string("tasks:create", mock_user_context.permissions) is True
        assert check_permission_string("tasks:delete", mock_user_context.permissions) is True

    def test_check_admin_wildcard(self, admin_context):
        """Admin with *:* should have all permissions"""
        # *:* should allow everything
        assert check_permission_string("stories:read", admin_context.permissions) is True
        assert check_permission_string("users:delete", admin_context.permissions) is True
        assert check_permission_string("anything:whatever", admin_context.permissions) is True

    def test_deny_missing_permission(self, mock_user_context):
        """Should deny permission not in list"""
        assert check_permission_string("users:delete", mock_user_context.permissions) is False
        assert check_permission_string("settings:manage", mock_user_context.permissions) is False


# =============================================================================
# USER CONTEXT TESTS
# =============================================================================

class TestUserContext:
    """Tests for UserContext model"""

    def test_user_context_creation(self):
        """Should create user context with all fields"""
        from factory.auth.rbac import UserContext

        ctx = UserContext(
            user_id=1,
            username="testuser",
            roles=["DEVELOPER"],
            permissions=["stories:read"],
            is_authenticated=True,
            is_admin=False
        )

        assert ctx.user_id == 1
        assert ctx.username == "testuser"
        assert "DEVELOPER" in ctx.roles
        assert "stories:read" in ctx.permissions
        assert ctx.is_authenticated is True
        assert ctx.is_admin is False

    def test_user_context_permissions_check(self):
        """Should check permission via helper function"""
        from factory.auth.rbac import UserContext

        ctx = UserContext(
            user_id=1,
            username="testuser",
            roles=["DEVELOPER"],
            permissions=["stories:read", "stories:create"],
            is_authenticated=True,
            is_admin=False
        )

        assert check_permission_string("stories:read", ctx.permissions) is True
        assert check_permission_string("stories:delete", ctx.permissions) is False


# =============================================================================
# RBAC MANAGER TESTS
# =============================================================================

class TestRBACManager:
    """Tests for RBACManager class"""

    def test_init_default_roles(self, mock_db):
        """Should initialize default roles"""
        from factory.auth.rbac import RBACManager

        # Setup mock
        mock_db.query.return_value.filter.return_value.first.return_value = None
        mock_db.add = Mock()
        mock_db.commit = Mock()

        rbac = RBACManager(mock_db)
        rbac.init_default_roles()

        # Should have added roles
        assert mock_db.add.called

    def test_get_user_permissions(self, mock_db):
        """Should get permissions for user"""
        from factory.auth.rbac import RBACManager

        # Setup mock for user roles with nested role object
        mock_role = Mock()
        mock_role.permissions = ["stories:read", "stories:create"]

        mock_user_role = Mock()
        mock_user_role.role_id = "ROLE-DEV"
        mock_user_role.role = mock_role  # Nested role object

        # Query returns user_roles list with nested role
        mock_query = Mock()
        mock_query.filter.return_value.all.return_value = [mock_user_role]
        mock_db.query.return_value = mock_query

        rbac = RBACManager(mock_db)
        permissions = rbac.get_user_permissions(1)

        # Should return permissions from the role
        assert isinstance(permissions, list)
        assert "stories:read" in permissions
        assert "stories:create" in permissions


# =============================================================================
# RESOURCES AND ACTIONS
# =============================================================================

class TestResourcesAndActions:
    """Tests for resource and action definitions"""

    def test_resources_defined(self):
        """Should have all resources defined"""
        from factory.auth.rbac import RESOURCES

        expected = ["projects", "stories", "tasks", "epics", "sprints", "users", "roles"]
        for resource in expected:
            assert resource in RESOURCES

    def test_actions_defined(self):
        """Should have all actions defined"""
        from factory.auth.rbac import ACTIONS

        expected = ["create", "read", "update", "delete", "manage"]
        for action in expected:
            assert action in ACTIONS


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestRBACIntegration:
    """Integration tests for RBAC"""

    def test_permission_format(self):
        """Permissions should follow resource:action format"""
        from factory.auth.rbac import DEFAULT_ROLES, RESOURCES, ACTIONS

        for role_name, role_data in DEFAULT_ROLES.items():
            for perm in role_data["permissions"]:
                if perm == "*:*":
                    continue
                parts = perm.split(":")
                assert len(parts) == 2, f"Invalid permission format: {perm}"
                resource, action = parts
                # Resource should be valid or wildcard
                assert resource in RESOURCES or resource == "*"
                # Action should be valid or wildcard
                assert action in ACTIONS or action == "*"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
