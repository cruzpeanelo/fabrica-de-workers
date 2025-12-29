# -*- coding: utf-8 -*-
"""
Testes da API Publica - Fabrica de Agentes v6.5
===============================================

Testes unitarios e de integracao para a API publica.
"""
import pytest
from datetime import datetime, timedelta
from unittest.mock import patch, MagicMock


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_db_session():
    """Mock da sessao do banco de dados"""
    session = MagicMock()
    session.query.return_value.filter.return_value.first.return_value = None
    return session


@pytest.fixture
def sample_api_key():
    """API Key de exemplo"""
    return {
        "key_id": "fab_test123",
        "key_hash": "abc123hash",
        "key_prefix": "sk-fab_tes...",
        "name": "Test API Key",
        "tier": "basic",
        "status": "active",
        "scopes": ["read", "write"],
        "allowed_ips": [],
        "user_id": 1,
        "expires_at": None,
    }


# =============================================================================
# TESTES DE API KEY
# =============================================================================

class TestAPIKeyGeneration:
    """Testes de geracao de API Key"""

    def test_generate_key_format(self):
        """Testa formato da chave gerada"""
        from factory.database.api_models import APIKey

        key_id, full_key, key_hash, key_prefix = APIKey.generate_key()

        # Verificar formato
        assert key_id.startswith("fab_")
        assert full_key.startswith("sk-fab_")
        assert len(key_hash) == 64  # SHA256 hex
        assert "..." in key_prefix

    def test_generate_key_uniqueness(self):
        """Testa unicidade das chaves geradas"""
        from factory.database.api_models import APIKey

        keys = set()
        for _ in range(100):
            key_id, _, _, _ = APIKey.generate_key()
            assert key_id not in keys
            keys.add(key_id)

    def test_verify_key(self):
        """Testa verificacao de chave"""
        from factory.database.api_models import APIKey

        key_id, full_key, key_hash, _ = APIKey.generate_key()

        # Criar API Key mock
        api_key = APIKey(
            key_id=key_id,
            key_hash=key_hash,
            key_prefix="",
            name="Test"
        )

        # Verificar chave correta
        assert api_key.verify_key(full_key) is True

        # Verificar chave incorreta
        assert api_key.verify_key("sk-fab_wrong_key") is False


class TestAPIKeyTiers:
    """Testes de tiers e rate limits"""

    def test_tier_limits(self):
        """Testa limites por tier"""
        from factory.database.api_models import APIKey, APIKeyTier

        tiers = ["free", "basic", "pro", "enterprise"]

        for tier in tiers:
            api_key = APIKey(
                key_id="test",
                key_hash="hash",
                key_prefix="prefix",
                name="Test",
                tier=tier
            )
            limits = api_key.get_rate_limits()

            assert "per_minute" in limits
            assert "per_day" in limits
            assert limits["per_minute"] > 0
            assert limits["per_day"] > 0

    def test_tier_hierarchy(self):
        """Testa hierarquia de limites"""
        from factory.database.api_models import APIKey

        tiers = ["free", "basic", "pro", "enterprise"]
        previous_minute = 0
        previous_day = 0

        for tier in tiers:
            api_key = APIKey(
                key_id="test",
                key_hash="hash",
                key_prefix="prefix",
                name="Test",
                tier=tier
            )
            limits = api_key.get_rate_limits()

            # Cada tier deve ter limites maiores que o anterior
            assert limits["per_minute"] >= previous_minute
            assert limits["per_day"] >= previous_day

            previous_minute = limits["per_minute"]
            previous_day = limits["per_day"]

    def test_custom_limits_override(self):
        """Testa override de limites customizados"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            tier="free",
            custom_rate_limit=500,
            custom_daily_limit=5000
        )

        limits = api_key.get_rate_limits()

        assert limits["per_minute"] == 500
        assert limits["per_day"] == 5000


class TestAPIKeyValidation:
    """Testes de validacao de API Key"""

    def test_is_valid_active(self):
        """Testa chave ativa"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            status="active"
        )

        assert api_key.is_valid() is True

    def test_is_valid_revoked(self):
        """Testa chave revogada"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            status="revoked"
        )

        assert api_key.is_valid() is False

    def test_is_valid_expired(self):
        """Testa chave expirada"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            status="active",
            expires_at=datetime.utcnow() - timedelta(days=1)
        )

        assert api_key.is_valid() is False


class TestAPIKeyScopes:
    """Testes de scopes"""

    def test_has_scope_basic(self):
        """Testa verificacao de scope basica"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            scopes=["read", "write"]
        )

        assert api_key.has_scope("read") is True
        assert api_key.has_scope("write") is True
        assert api_key.has_scope("admin") is False

    def test_has_scope_admin(self):
        """Testa que admin tem todos os scopes"""
        from factory.database.api_models import APIKey

        api_key = APIKey(
            key_id="test",
            key_hash="hash",
            key_prefix="prefix",
            name="Test",
            scopes=["admin"]
        )

        assert api_key.has_scope("read") is True
        assert api_key.has_scope("write") is True
        assert api_key.has_scope("webhooks") is True
        assert api_key.has_scope("anything") is True


# =============================================================================
# TESTES DE WEBHOOK
# =============================================================================

class TestWebhook:
    """Testes de webhook"""

    def test_generate_secret(self):
        """Testa geracao de secret"""
        from factory.database.api_models import Webhook

        secret1 = Webhook.generate_secret()
        secret2 = Webhook.generate_secret()

        # Devem ser unicos
        assert secret1 != secret2

        # Devem ter tamanho adequado
        assert len(secret1) >= 32

    def test_should_trigger(self):
        """Testa logica de disparo"""
        from factory.database.api_models import Webhook

        webhook = Webhook(
            webhook_id="test",
            api_key_id="key",
            name="Test",
            url="https://example.com",
            events=["story.completed", "job.failed"],
            status="active"
        )

        # Eventos assinados
        assert webhook.should_trigger("story.completed") is True
        assert webhook.should_trigger("job.failed") is True

        # Eventos nao assinados
        assert webhook.should_trigger("project.created") is False

    def test_should_trigger_paused(self):
        """Testa que webhook pausado nao dispara"""
        from factory.database.api_models import Webhook

        webhook = Webhook(
            webhook_id="test",
            api_key_id="key",
            name="Test",
            url="https://example.com",
            events=["story.completed"],
            status="paused"
        )

        assert webhook.should_trigger("story.completed") is False

    def test_should_trigger_project_filter(self):
        """Testa filtro por projeto"""
        from factory.database.api_models import Webhook

        webhook = Webhook(
            webhook_id="test",
            api_key_id="key",
            name="Test",
            url="https://example.com",
            events=["story.completed"],
            status="active",
            project_ids=["PRJ-001", "PRJ-002"]
        )

        # Projetos permitidos
        assert webhook.should_trigger("story.completed", "PRJ-001") is True
        assert webhook.should_trigger("story.completed", "PRJ-002") is True

        # Projeto nao permitido
        assert webhook.should_trigger("story.completed", "PRJ-003") is False

    def test_record_success(self):
        """Testa registro de sucesso"""
        from factory.database.api_models import Webhook

        webhook = Webhook(
            webhook_id="test",
            api_key_id="key",
            name="Test",
            url="https://example.com",
            events=[],
            status="active",
            consecutive_failures=5
        )

        webhook.record_success()

        assert webhook.deliveries_total == 1
        assert webhook.consecutive_failures == 0
        assert webhook.last_delivery_at is not None

    def test_record_failure_pauses(self):
        """Testa que muitas falhas pausam o webhook"""
        from factory.database.api_models import Webhook

        webhook = Webhook(
            webhook_id="test",
            api_key_id="key",
            name="Test",
            url="https://example.com",
            events=[],
            status="active",
            consecutive_failures=9
        )

        webhook.record_failure("Connection refused")

        assert webhook.status == "failed"
        assert webhook.consecutive_failures == 10


# =============================================================================
# TESTES DE WEBHOOK SIGNATURE
# =============================================================================

class TestWebhookSignature:
    """Testes de assinatura de webhook"""

    def test_verify_signature_valid(self):
        """Testa verificacao de assinatura valida"""
        from factory.api.webhooks import verify_webhook_signature

        secret = "my-webhook-secret"
        timestamp = "1234567890"
        payload = b'{"event": "test"}'

        # Calcular assinatura esperada
        import hmac
        import hashlib
        payload_to_sign = f"{timestamp}.{payload.decode()}"
        expected = hmac.new(
            secret.encode(),
            payload_to_sign.encode(),
            hashlib.sha256
        ).hexdigest()

        signature = f"sha256={expected}"

        assert verify_webhook_signature(payload, signature, secret, timestamp) is True

    def test_verify_signature_invalid(self):
        """Testa verificacao de assinatura invalida"""
        from factory.api.webhooks import verify_webhook_signature

        secret = "my-webhook-secret"
        timestamp = "1234567890"
        payload = b'{"event": "test"}'
        signature = "sha256=invalid_signature"

        assert verify_webhook_signature(payload, signature, secret, timestamp) is False

    def test_verify_signature_wrong_format(self):
        """Testa assinatura com formato errado"""
        from factory.api.webhooks import verify_webhook_signature

        assert verify_webhook_signature(b"test", "invalid", "secret") is False
        assert verify_webhook_signature(b"test", "", "secret") is False
        assert verify_webhook_signature(b"test", None, "secret") is False


# =============================================================================
# TESTES DO SDK
# =============================================================================

class TestSDKModels:
    """Testes dos modelos do SDK"""

    def test_project_from_dict(self):
        """Testa criacao de Project a partir de dict"""
        from factory.sdk.models import Project

        data = {
            "project_id": "PRJ-001",
            "name": "Test Project",
            "description": "Description",
            "project_type": "web-app",
            "status": "IN_PROGRESS",
            "progress": 50.0,
            "created_at": "2024-01-15T10:00:00"
        }

        project = Project.from_dict(data)

        assert project.id == "PRJ-001"
        assert project.name == "Test Project"
        assert project.status == "IN_PROGRESS"
        assert project.progress == 50.0

    def test_story_from_dict(self):
        """Testa criacao de Story a partir de dict"""
        from factory.sdk.models import Story

        data = {
            "story_id": "STR-001",
            "project_id": "PRJ-001",
            "title": "Test Story",
            "persona": "user",
            "action": "do something",
            "benefit": "achieve goal",
            "status": "in_progress",
            "story_points": 5
        }

        story = Story.from_dict(data)

        assert story.id == "STR-001"
        assert story.title == "Test Story"
        assert story.story_points == 5

    def test_job_status_properties(self):
        """Testa propriedades de status do Job"""
        from factory.sdk.models import Job

        # Job completed
        job_completed = Job(
            id="JOB-001",
            description="Test",
            status="completed"
        )
        assert job_completed.is_completed is True
        assert job_completed.is_failed is False
        assert job_completed.is_running is False

        # Job failed
        job_failed = Job(
            id="JOB-002",
            description="Test",
            status="failed"
        )
        assert job_failed.is_completed is False
        assert job_failed.is_failed is True
        assert job_failed.is_running is False

        # Job running
        job_running = Job(
            id="JOB-003",
            description="Test",
            status="running"
        )
        assert job_running.is_completed is False
        assert job_running.is_failed is False
        assert job_running.is_running is True


class TestSDKExceptions:
    """Testes das excecoes do SDK"""

    def test_fabrica_error(self):
        """Testa excecao base"""
        from factory.sdk.exceptions import FabricaError

        error = FabricaError("Test error", status_code=400, error_code="test_error")

        assert str(error) == "[test_error] Test error (HTTP 400)"
        assert error.status_code == 400
        assert error.error_code == "test_error"

    def test_rate_limit_error(self):
        """Testa excecao de rate limit"""
        from factory.sdk.exceptions import RateLimitError

        error = RateLimitError(retry_after=60, limit=100)

        assert "60" in str(error)
        assert error.retry_after == 60
        assert error.limit == 100

    def test_not_found_error(self):
        """Testa excecao de nao encontrado"""
        from factory.sdk.exceptions import NotFoundError

        error = NotFoundError(
            "Projeto nao encontrado",
            resource_type="project",
            resource_id="PRJ-001"
        )

        assert error.resource_type == "project"
        assert error.resource_id == "PRJ-001"


# =============================================================================
# TESTES DE RATE LIMITING
# =============================================================================

class TestRateLimiting:
    """Testes de rate limiting"""

    def test_tier_limits_config(self):
        """Testa configuracao de limites por tier"""
        from factory.api.rate_limit_v2 import TIER_LIMITS

        assert "free" in TIER_LIMITS
        assert "basic" in TIER_LIMITS
        assert "pro" in TIER_LIMITS
        assert "enterprise" in TIER_LIMITS

        # Verificar que enterprise tem mais limites que free
        assert TIER_LIMITS["enterprise"]["per_minute"] > TIER_LIMITS["free"]["per_minute"]
        assert TIER_LIMITS["enterprise"]["per_day"] > TIER_LIMITS["free"]["per_day"]

    @pytest.mark.asyncio
    async def test_rate_limiter_allows_first_request(self):
        """Testa que primeira requisicao e permitida"""
        from factory.api.rate_limit_v2 import TieredRateLimiter

        limiter = TieredRateLimiter()

        # Primeira requisicao deve ser permitida
        allowed, info = await limiter.check_rate_limit("test_identifier", "free")

        assert allowed is True
        assert "limit_minute" in info
        assert "remaining_minute" in info


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
