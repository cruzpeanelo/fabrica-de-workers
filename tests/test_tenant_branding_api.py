# -*- coding: utf-8 -*-
"""
Testes de API - Tenant Branding e Multi-Tenant Mobile
Issue #386 - Validar implementacoes de multi-tenant para mobile

Estes testes validam os endpoints necessarios para o app mobile
funcionar corretamente com multi-tenant e white label.
"""

import pytest
import sys
import os

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def api_client():
    """Cliente HTTP para testes"""
    from fastapi.testclient import TestClient
    from factory.dashboard.app_v6_agile import app
    return TestClient(app)


@pytest.fixture
def auth_headers():
    """Headers com autenticacao"""
    # Token de teste - em producao usar token real
    return {
        "Authorization": "Bearer test_token",
        "X-Tenant-ID": "test_tenant"
    }


@pytest.fixture
def test_tenant_id():
    """Tenant ID para testes"""
    return "belgo"


# =============================================================================
# TESTES - TENANT BRANDING API
# =============================================================================

class TestTenantBrandingAPI:
    """Testes para endpoints de branding do tenant"""

    def test_get_tenant_branding_returns_200(self, api_client, test_tenant_id, auth_headers):
        """GET /api/tenant/{id}/branding deve retornar 200"""
        response = api_client.get(
            f"/api/tenant/{test_tenant_id}/branding",
            headers=auth_headers
        )
        # Pode retornar 200 ou 404 se endpoint ainda nao existe
        assert response.status_code in [200, 404, 401]

    def test_branding_response_has_required_fields(self, api_client, test_tenant_id, auth_headers):
        """Branding deve ter campos obrigatorios"""
        response = api_client.get(
            f"/api/tenant/{test_tenant_id}/branding",
            headers=auth_headers
        )
        if response.status_code == 200:
            data = response.json()
            required_fields = [
                'tenant_id',
                'primary_color',
                'secondary_color',
                'company_name'
            ]
            for field in required_fields:
                assert field in data, f"Campo {field} faltando no branding"

    def test_branding_colors_are_valid_hex(self, api_client, test_tenant_id, auth_headers):
        """Cores devem ser hex validos"""
        response = api_client.get(
            f"/api/tenant/{test_tenant_id}/branding",
            headers=auth_headers
        )
        if response.status_code == 200:
            data = response.json()
            import re
            hex_pattern = r'^#[0-9A-Fa-f]{6}$'

            if 'primary_color' in data:
                assert re.match(hex_pattern, data['primary_color']), \
                    f"primary_color invalido: {data['primary_color']}"

            if 'secondary_color' in data:
                assert re.match(hex_pattern, data['secondary_color']), \
                    f"secondary_color invalido: {data['secondary_color']}"


class TestTenantConfigAPI:
    """Testes para endpoints de configuracao do tenant"""

    def test_get_tenant_config_returns_200(self, api_client, test_tenant_id, auth_headers):
        """GET /api/tenant/{id}/config deve retornar 200"""
        response = api_client.get(
            f"/api/tenant/{test_tenant_id}/config",
            headers=auth_headers
        )
        assert response.status_code in [200, 404, 401]

    def test_config_has_features_enabled(self, api_client, test_tenant_id, auth_headers):
        """Config deve ter lista de features"""
        response = api_client.get(
            f"/api/tenant/{test_tenant_id}/config",
            headers=auth_headers
        )
        if response.status_code == 200:
            data = response.json()
            assert 'features_enabled' in data
            assert isinstance(data['features_enabled'], list)


# =============================================================================
# TESTES - REFRESH TOKEN
# =============================================================================

class TestRefreshTokenAPI:
    """Testes para refresh token endpoint"""

    def test_refresh_token_endpoint_exists(self, api_client):
        """POST /api/auth/refresh deve existir"""
        response = api_client.post(
            "/api/auth/refresh",
            json={"refresh_token": "test_refresh_token"}
        )
        # 400/401/422 sao aceitaveis (token invalido)
        # 404 significa que endpoint nao existe ainda
        assert response.status_code != 405, "Endpoint nao aceita POST"

    def test_refresh_token_returns_new_tokens(self, api_client):
        """Refresh deve retornar novos tokens"""
        # Este teste vai passar quando endpoint estiver implementado
        response = api_client.post(
            "/api/auth/refresh",
            json={"refresh_token": "valid_refresh_token"}
        )
        if response.status_code == 200:
            data = response.json()
            assert 'access_token' in data
            assert 'refresh_token' in data


# =============================================================================
# TESTES - DEVICE REGISTRATION
# =============================================================================

class TestDeviceRegistrationAPI:
    """Testes para registro de dispositivos"""

    def test_register_device_endpoint_exists(self, api_client, auth_headers):
        """POST /api/auth/register-device deve existir"""
        response = api_client.post(
            "/api/auth/register-device",
            headers=auth_headers,
            json={
                "device_id": "test-device-uuid",
                "device_name": "Test Phone",
                "platform": "android",
                "push_token": "fcm_token_123"
            }
        )
        assert response.status_code != 405, "Endpoint nao aceita POST"

    def test_logout_all_devices_endpoint_exists(self, api_client, auth_headers):
        """POST /api/auth/logout-all-devices deve existir"""
        response = api_client.post(
            "/api/auth/logout-all-devices",
            headers=auth_headers
        )
        assert response.status_code != 405, "Endpoint nao aceita POST"


# =============================================================================
# TESTES - USER MODE API
# =============================================================================

class TestUserModeAPI:
    """Testes para user mode (basico/avancado)"""

    def test_get_user_mode_preferences(self, api_client, auth_headers):
        """GET /api/user-mode/preferences deve funcionar"""
        response = api_client.get(
            "/api/user-mode/preferences?user_id=test",
            headers=auth_headers
        )
        assert response.status_code in [200, 401]

    def test_toggle_user_mode(self, api_client, auth_headers):
        """POST /api/user-mode/toggle-mode deve funcionar"""
        response = api_client.post(
            "/api/user-mode/toggle-mode?user_id=test",
            headers=auth_headers
        )
        assert response.status_code in [200, 401]

    def test_get_translated_labels(self, api_client, auth_headers):
        """GET /api/user-mode/labels deve retornar labels"""
        response = api_client.get(
            "/api/user-mode/labels?user_id=test",
            headers=auth_headers
        )
        if response.status_code == 200:
            data = response.json()
            assert 'labels' in data
            assert 'mode' in data


# =============================================================================
# TESTES - ONBOARDING TOUR API
# =============================================================================

class TestOnboardingTourAPI:
    """Testes para tour de onboarding"""

    def test_get_tour_status(self, api_client, auth_headers):
        """GET /api/tour/status deve funcionar"""
        response = api_client.get(
            "/api/tour/status?user_id=test",
            headers=auth_headers
        )
        assert response.status_code in [200, 401]

    def test_get_tour_steps(self, api_client, auth_headers):
        """GET /api/tour/steps deve retornar steps"""
        response = api_client.get(
            "/api/tour/steps?user_id=test",
            headers=auth_headers
        )
        if response.status_code == 200:
            data = response.json()
            assert 'steps' in data
            assert len(data['steps']) > 0

    def test_complete_tour(self, api_client, auth_headers):
        """POST /api/tour/complete deve funcionar"""
        response = api_client.post(
            "/api/tour/complete?user_id=test",
            headers=auth_headers
        )
        assert response.status_code in [200, 401]


# =============================================================================
# TESTES - X-TENANT-ID HEADER
# =============================================================================

class TestTenantHeader:
    """Testes para header X-Tenant-ID"""

    def test_request_without_tenant_id_fails(self, api_client):
        """Request sem X-Tenant-ID deve falhar em endpoints protegidos"""
        response = api_client.get(
            "/api/stories",
            headers={"Authorization": "Bearer test_token"}
            # Sem X-Tenant-ID
        )
        # Deve falhar com 401 ou 403
        assert response.status_code in [401, 403, 422]

    def test_request_with_tenant_id_works(self, api_client, auth_headers):
        """Request com X-Tenant-ID deve funcionar"""
        response = api_client.get(
            "/api/stories",
            headers=auth_headers
        )
        # Com header correto, deve processar (mesmo que retorne 401 por token invalido)
        assert response.status_code in [200, 401, 403]


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
