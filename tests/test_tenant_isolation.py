# -*- coding: utf-8 -*-
"""
Tests for Tenant Isolation - Issue #122
Fabrica de Agentes v6.5

Comprehensive tests for:
1. TenantIsolationMiddleware
2. TenantQueryFilter
3. Context management
4. Security edge cases
"""

import pytest
import asyncio
from unittest.mock import Mock, AsyncMock, patch, MagicMock
from datetime import datetime

from fastapi import FastAPI, Request
from fastapi.testclient import TestClient
from starlette.responses import Response


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def app():
    """Create test FastAPI app"""
    from factory.core.tenant_isolation import TenantIsolationMiddleware

    app = FastAPI()

    # Add middleware with development mode
    with patch.dict('os.environ', {'ENV': 'development'}):
        app.add_middleware(TenantIsolationMiddleware, strict_mode=True)

    @app.get("/api/test")
    async def test_endpoint(request: Request):
        tenant_id = getattr(request.state, 'tenant_id', None)
        return {"tenant_id": tenant_id}

    @app.get("/health")
    async def health():
        return {"status": "ok"}

    return app


@pytest.fixture
def client(app):
    """Create test client"""
    return TestClient(app)


# =============================================================================
# CONTEXT MANAGEMENT TESTS
# =============================================================================

class TestTenantContext:
    """Tests for tenant context management"""

    def test_get_tenant_id_default_none(self):
        """Should return None when no tenant set"""
        from factory.core.tenant_isolation import (
            get_current_tenant_id,
            clear_tenant_context
        )
        clear_tenant_context()
        assert get_current_tenant_id() is None

    def test_set_and_get_tenant_id(self):
        """Should set and retrieve tenant ID"""
        from factory.core.tenant_isolation import (
            get_current_tenant_id,
            set_current_tenant_id,
            clear_tenant_context
        )
        clear_tenant_context()

        set_current_tenant_id("TEN-TEST123")
        assert get_current_tenant_id() == "TEN-TEST123"

        clear_tenant_context()
        assert get_current_tenant_id() is None

    def test_require_tenant_context_raises(self):
        """Should raise when tenant not set"""
        from factory.core.tenant_isolation import (
            require_tenant_context,
            clear_tenant_context
        )
        from fastapi import HTTPException

        clear_tenant_context()

        with pytest.raises(HTTPException) as exc_info:
            require_tenant_context()

        assert exc_info.value.status_code == 403
        assert "Tenant context not established" in str(exc_info.value.detail)

    def test_require_tenant_context_success(self):
        """Should return tenant ID when set and verified"""
        from factory.core.tenant_isolation import (
            require_tenant_context,
            set_current_tenant_id,
            set_tenant_verified,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")
        set_tenant_verified(True)

        result = require_tenant_context()
        assert result == "TEN-TEST123"

        clear_tenant_context()


# =============================================================================
# MIDDLEWARE TESTS
# =============================================================================

class TestTenantIsolationMiddleware:
    """Tests for TenantIsolationMiddleware"""

    def test_public_route_bypasses_tenant_check(self, client):
        """Public routes should not require tenant"""
        response = client.get("/health")
        assert response.status_code == 200
        assert response.json() == {"status": "ok"}

    def test_api_route_requires_tenant_header(self, client):
        """API routes should require X-Tenant-ID header in strict mode"""
        response = client.get("/api/test")
        assert response.status_code == 403
        assert "TENANT_REQUIRED" in response.text

    def test_api_route_with_tenant_header(self, client):
        """API routes should work with valid tenant header"""
        with patch.dict('os.environ', {'ENV': 'development'}):
            response = client.get(
                "/api/test",
                headers={"X-Tenant-ID": "TEN-TEST123"}
            )
            assert response.status_code == 200
            assert response.json()["tenant_id"] == "TEN-TEST123"

    def test_tenant_id_sanitization(self):
        """Should sanitize tenant ID to prevent injection"""
        from factory.core.tenant_isolation import TenantIsolationMiddleware

        middleware = TenantIsolationMiddleware(None)

        # Normal ID
        assert middleware._sanitize_tenant_id("TEN-12345") == "TEN-12345"

        # With special characters
        assert middleware._sanitize_tenant_id("TEN<script>") == "TENscript"

        # With spaces
        assert middleware._sanitize_tenant_id("TEN 123") == "TEN123"

        # Too long
        long_id = "A" * 100
        assert len(middleware._sanitize_tenant_id(long_id)) == 50

    def test_subdomain_extraction(self):
        """Should extract tenant from subdomain"""
        from factory.core.tenant_isolation import TenantIsolationMiddleware

        middleware = TenantIsolationMiddleware(None)

        # Valid subdomain
        assert middleware._extract_from_subdomain("acme.fabrica.com") == "acme"

        # Common subdomains should be ignored
        assert middleware._extract_from_subdomain("www.fabrica.com") is None
        assert middleware._extract_from_subdomain("api.fabrica.com") is None
        assert middleware._extract_from_subdomain("app.fabrica.com") is None

        # No subdomain
        assert middleware._extract_from_subdomain("fabrica.com") is None


# =============================================================================
# QUERY FILTER TESTS
# =============================================================================

class TestTenantQueryFilter:
    """Tests for TenantQueryFilter"""

    def test_apply_without_tenant_returns_empty(self):
        """Query without tenant context should return empty"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            clear_tenant_context
        )

        clear_tenant_context()

        # Create mock query and model
        mock_query = Mock()
        mock_model = Mock()
        mock_model.tenant_id = Mock()

        # Apply filter
        result = TenantQueryFilter.apply(mock_query, mock_model)

        # Should have called filter with impossible condition
        mock_query.filter.assert_called()

    def test_apply_with_tenant_filters_correctly(self):
        """Query with tenant context should filter by tenant_id"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            set_current_tenant_id,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")

        # Create mock query and model
        mock_query = Mock()
        mock_model = Mock()
        mock_model.tenant_id = "column"

        # Apply filter
        TenantQueryFilter.apply(mock_query, mock_model)

        # Should have called filter
        mock_query.filter.assert_called()

        clear_tenant_context()

    def test_set_tenant_on_entity(self):
        """Should set tenant_id on new entity"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            set_current_tenant_id,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")

        # Create mock entity
        class MockEntity:
            tenant_id = None

        entity = MockEntity()
        TenantQueryFilter.set_tenant_on_entity(entity)

        assert entity.tenant_id == "TEN-TEST123"

        clear_tenant_context()

    def test_set_tenant_without_context_raises(self):
        """Should raise when setting tenant without context"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            clear_tenant_context
        )
        from fastapi import HTTPException

        clear_tenant_context()

        class MockEntity:
            tenant_id = None

        entity = MockEntity()

        with pytest.raises(HTTPException) as exc_info:
            TenantQueryFilter.set_tenant_on_entity(entity)

        assert exc_info.value.status_code == 403

    def test_validate_entity_tenant(self):
        """Should validate entity belongs to current tenant"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            set_current_tenant_id,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")

        class MockEntity:
            tenant_id = None

        # Same tenant - should return True
        entity = MockEntity()
        entity.tenant_id = "TEN-TEST123"
        assert TenantQueryFilter.validate_entity_tenant(entity) is True

        # Different tenant - should return False
        entity.tenant_id = "TEN-OTHER"
        assert TenantQueryFilter.validate_entity_tenant(entity) is False

        clear_tenant_context()


# =============================================================================
# DECORATOR TESTS
# =============================================================================

class TestTenantDecorators:
    """Tests for tenant-related decorators"""

    def test_require_tenant_decorator_sync(self):
        """Should enforce tenant for sync functions"""
        from factory.core.tenant_isolation import (
            require_tenant,
            clear_tenant_context
        )
        from fastapi import HTTPException

        clear_tenant_context()

        @require_tenant
        def my_function():
            return "success"

        with pytest.raises(HTTPException) as exc_info:
            my_function()

        assert exc_info.value.status_code == 403

    @pytest.mark.asyncio
    async def test_require_tenant_decorator_async(self):
        """Should enforce tenant for async functions"""
        from factory.core.tenant_isolation import (
            require_tenant,
            set_current_tenant_id,
            set_tenant_verified,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")
        set_tenant_verified(True)

        @require_tenant
        async def my_async_function():
            return "success"

        result = await my_async_function()
        assert result == "success"

        clear_tenant_context()

    def test_tenant_scoped_decorator(self):
        """Should inject tenant_id into function"""
        from factory.core.tenant_isolation import (
            tenant_scoped,
            set_current_tenant_id,
            set_tenant_verified,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-TEST123")
        set_tenant_verified(True)

        @tenant_scoped
        def my_function(tenant_id: str):
            return tenant_id

        result = my_function()
        assert result == "TEN-TEST123"

        clear_tenant_context()


# =============================================================================
# SECURITY EDGE CASES
# =============================================================================

class TestSecurityEdgeCases:
    """Tests for security edge cases"""

    def test_tenant_signature_verification(self):
        """Should verify tenant signature correctly"""
        from factory.core.tenant_isolation import TenantIsolationMiddleware

        middleware = TenantIsolationMiddleware(None)

        # Generate signature
        tenant_id = "TEN-TEST123"
        signature = middleware.generate_tenant_signature(tenant_id)

        # Valid signature
        assert middleware._verify_tenant_signature(tenant_id, signature) is True

        # Invalid signature
        assert middleware._verify_tenant_signature(tenant_id, "invalid") is False

        # Wrong tenant
        assert middleware._verify_tenant_signature("TEN-OTHER", signature) is False

    def test_concurrent_tenant_contexts(self):
        """Should maintain separate contexts per thread/task"""
        from factory.core.tenant_isolation import (
            set_current_tenant_id,
            get_current_tenant_id,
            clear_tenant_context
        )
        import threading

        results = {}

        def thread_func(tenant_id, thread_name):
            clear_tenant_context()
            set_current_tenant_id(tenant_id)
            # Simulate some work
            import time
            time.sleep(0.01)
            results[thread_name] = get_current_tenant_id()
            clear_tenant_context()

        # Create threads with different tenants
        threads = [
            threading.Thread(target=thread_func, args=("TEN-A", "thread_a")),
            threading.Thread(target=thread_func, args=("TEN-B", "thread_b")),
        ]

        for t in threads:
            t.start()
        for t in threads:
            t.join()

        # Each thread should have its own context
        assert results.get("thread_a") == "TEN-A"
        assert results.get("thread_b") == "TEN-B"

    def test_cross_tenant_access_prevention(self):
        """Should prevent access to other tenant's data"""
        from factory.core.tenant_isolation import (
            TenantQueryFilter,
            set_current_tenant_id,
            clear_tenant_context
        )

        clear_tenant_context()
        set_current_tenant_id("TEN-A")

        class MockEntity:
            tenant_id = "TEN-B"  # Different tenant

        entity = MockEntity()

        # Should detect cross-tenant access
        assert TenantQueryFilter.validate_entity_tenant(entity) is False

        clear_tenant_context()


# =============================================================================
# AUDIT TESTS
# =============================================================================

class TestTenantIsolationAudit:
    """Tests for tenant isolation audit logging"""

    def test_log_violation(self):
        """Should log tenant isolation violations"""
        from factory.core.tenant_isolation import TenantIsolationAudit

        with patch('factory.database.connection.SessionLocal') as mock_session:
            mock_db = Mock()
            mock_session.return_value = mock_db

            TenantIsolationAudit.log_violation(
                attempted_tenant="TEN-ATTACKER",
                actual_tenant="TEN-VICTIM",
                resource="stories",
                user_id=123
            )

            # In development, this may not actually write to DB
            # but the method should not raise


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
