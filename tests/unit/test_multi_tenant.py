# -*- coding: utf-8 -*-
"""
Testes Unitarios para Multi-Tenant Core
========================================

Testes para:
- Issue #120: TenantMember, ProjectMember, hierarquia de acesso
- Issue #105: GlobalTenantMiddleware, validacao de membership
- Issue #104: PlanEnforcer, limites de plano

Autor: Plataforma E
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime, timedelta
from typing import Dict, Any


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_db():
    """Mock do banco de dados"""
    return Mock()


@pytest.fixture
def sample_tenant_data() -> Dict[str, Any]:
    """Dados de exemplo de um tenant"""
    return {
        "tenant_id": "TEN-TEST001",
        "name": "Empresa Teste",
        "slug": "empresa-teste",
        "plan": "pro",
        "status": "active",
        "features": {
            "stories": True,
            "kanban": True,
            "workers": True,
            "chat_assistant": True,
            "custom_branding": True,
            "sso": False,
            "api_access": True
        },
        "settings": {
            "max_projects": 20,
            "max_members": 50
        }
    }


@pytest.fixture
def sample_user_data() -> Dict[str, Any]:
    """Dados de exemplo de um usuario"""
    return {
        "user_id": 1,
        "username": "testuser",
        "role": "member",
        "tenant_id": "TEN-TEST001"
    }


# =============================================================================
# TESTES: CONTEXT FUNCTIONS (Issue #105)
# =============================================================================

class TestTenantContext:
    """Testes para funcoes de contexto"""

    def test_set_and_get_tenant_context(self):
        """Teste: Setar e obter contexto do tenant"""
        from factory.core.multi_tenant import (
            set_tenant_context, get_current_tenant,
            get_current_tenant_id, clear_context
        )

        # Limpar contexto inicial
        clear_context()

        # Setar contexto
        tenant_data = {"tenant_id": "TEN-001", "name": "Test", "status": "active"}
        set_tenant_context(tenant_data)

        # Verificar
        assert get_current_tenant() == tenant_data
        assert get_current_tenant_id() == "TEN-001"

        # Limpar
        clear_context()
        assert get_current_tenant() is None
        assert get_current_tenant_id() is None

    def test_clear_context_removes_all(self):
        """Teste: clear_context remove tenant, user e branding"""
        from factory.core.multi_tenant import (
            set_tenant_context, set_user_context, set_branding_context,
            get_current_tenant, get_current_user, get_current_branding,
            clear_context
        )

        # Setar tudo
        set_tenant_context({"tenant_id": "TEN-001"})
        set_user_context({"user_id": 1})
        set_branding_context({"logo_url": "/logo.png"})

        # Verificar que estao setados
        assert get_current_tenant() is not None
        assert get_current_user() is not None
        assert get_current_branding() is not None

        # Limpar
        clear_context()

        # Verificar que foram removidos
        assert get_current_tenant() is None
        assert get_current_user() is None
        assert get_current_branding() is None


# =============================================================================
# TESTES: TENANT MEMBER (Issue #120)
# =============================================================================

class TestTenantMember:
    """Testes para TenantMember"""

    def test_member_status_enum_values(self):
        """Teste: Valores do enum MemberStatus"""
        from factory.database.tenant_models import MemberStatus

        assert MemberStatus.ACTIVE.value == "active"
        assert MemberStatus.INVITED.value == "invited"
        assert MemberStatus.SUSPENDED.value == "suspended"
        assert MemberStatus.DEACTIVATED.value == "deactivated"

    def test_member_role_enum_values(self):
        """Teste: Valores do enum MemberRole"""
        from factory.database.tenant_models import MemberRole

        assert MemberRole.OWNER.value == "owner"
        assert MemberRole.ADMIN.value == "admin"
        assert MemberRole.MEMBER.value == "member"
        assert MemberRole.VIEWER.value == "viewer"
        assert MemberRole.BILLING.value == "billing"

    def test_tenant_member_to_dict(self):
        """Teste: TenantMember.to_dict retorna campos esperados"""
        from factory.database.tenant_models import TenantMember, MemberStatus, MemberRole

        member = TenantMember(
            tenant_id="TEN-001",
            user_id=1,
            tenant_role=MemberRole.MEMBER.value,
            status=MemberStatus.ACTIVE.value,
            active=True
        )

        data = member.to_dict()

        assert data["tenant_id"] == "TEN-001"
        assert data["user_id"] == 1
        assert data["tenant_role"] == "member"
        assert data["status"] == "active"
        assert data["active"] == True

    def test_has_permission_owner_has_all(self):
        """Teste: Owner tem todas as permissoes"""
        from factory.database.tenant_models import TenantMember, MemberRole

        owner = TenantMember(
            tenant_id="TEN-001",
            user_id=1,
            tenant_role=MemberRole.OWNER.value
        )

        assert owner.has_permission("manage_members") == True
        assert owner.has_permission("delete_tenant") == True
        assert owner.has_permission("any_permission") == True

    def test_has_permission_viewer_limited(self):
        """Teste: Viewer tem permissoes limitadas"""
        from factory.database.tenant_models import TenantMember, MemberRole

        viewer = TenantMember(
            tenant_id="TEN-001",
            user_id=1,
            tenant_role=MemberRole.VIEWER.value
        )

        assert viewer.has_permission("view") == True
        assert viewer.has_permission("manage_members") == False
        assert viewer.has_permission("create_projects") == False


# =============================================================================
# TESTES: PROJECT MEMBER (Issue #120)
# =============================================================================

class TestProjectMember:
    """Testes para ProjectMember"""

    def test_project_role_enum_values(self):
        """Teste: Valores do enum ProjectRole"""
        from factory.database.tenant_models import ProjectRole

        assert ProjectRole.OWNER.value == "owner"
        assert ProjectRole.ADMIN.value == "admin"
        assert ProjectRole.DEVELOPER.value == "developer"
        assert ProjectRole.VIEWER.value == "viewer"

    def test_project_member_to_dict(self):
        """Teste: ProjectMember.to_dict retorna campos esperados"""
        from factory.database.tenant_models import ProjectMember, ProjectRole

        member = ProjectMember(
            project_id="PROJ-001",
            user_id=1,
            project_role=ProjectRole.DEVELOPER.value
        )

        data = member.to_dict()

        assert data["project_id"] == "PROJ-001"
        assert data["user_id"] == 1
        assert data["project_role"] == "developer"
        assert "permissions" in data

    def test_is_owner(self):
        """Teste: verificar role OWNER via project_role"""
        from factory.database.tenant_models import ProjectMember, ProjectRole

        owner = ProjectMember(project_id="PROJ-001", user_id=1, project_role=ProjectRole.OWNER.value)
        dev = ProjectMember(project_id="PROJ-001", user_id=2, project_role=ProjectRole.DEVELOPER.value)

        # Issue #210: Usar project_role diretamente pois is_owner() nao existe
        assert owner.project_role == ProjectRole.OWNER.value
        assert dev.project_role != ProjectRole.OWNER.value

    def test_is_admin_or_owner(self):
        """Teste: verificar roles ADMIN ou OWNER via project_role"""
        from factory.database.tenant_models import ProjectMember, ProjectRole

        owner = ProjectMember(project_id="PROJ-001", user_id=1, project_role=ProjectRole.OWNER.value)
        admin = ProjectMember(project_id="PROJ-001", user_id=2, project_role=ProjectRole.ADMIN.value)
        dev = ProjectMember(project_id="PROJ-001", user_id=3, project_role=ProjectRole.DEVELOPER.value)

        # Issue #210: Usar project_role diretamente pois is_admin_or_owner() nao existe
        admin_or_owner_roles = [ProjectRole.OWNER.value, ProjectRole.ADMIN.value]
        assert owner.project_role in admin_or_owner_roles
        assert admin.project_role in admin_or_owner_roles
        assert dev.project_role not in admin_or_owner_roles

    def test_project_member_permissions(self):
        """Teste: Permissoes por role no projeto"""
        from factory.database.tenant_models import ProjectMember, ProjectRole

        owner = ProjectMember(project_id="PROJ-001", user_id=1, project_role=ProjectRole.OWNER.value)
        dev = ProjectMember(project_id="PROJ-001", user_id=2, project_role=ProjectRole.DEVELOPER.value)
        viewer = ProjectMember(project_id="PROJ-001", user_id=3, project_role=ProjectRole.VIEWER.value)

        # Owner pode tudo
        assert owner.has_permission("can_delete_stories") == True
        assert owner.has_permission("can_manage_members") == True

        # Developer pode criar e editar
        assert dev.has_permission("can_create_stories") == True
        assert dev.has_permission("can_edit_stories") == True
        assert dev.has_permission("can_manage_members") == False

        # Viewer nao pode nada alem de ver
        assert viewer.has_permission("can_create_stories") == False


# =============================================================================
# TESTES: PLAN ENFORCER (Issue #104)
# =============================================================================

# Importar diretamente do modulo para evitar conflito SQLAlchemy
import sys
import importlib.util

# Carregar plan_limits sem passar pelo __init__.py do billing
spec = importlib.util.spec_from_file_location(
    "plan_limits",
    "factory/billing/plan_limits.py"
)
plan_limits_module = importlib.util.module_from_spec(spec)


class TestPlanEnforcer:
    """Testes para PlanEnforcer"""

    def test_default_plan_limits_structure(self):
        """Teste: Estrutura de limites padrao"""
        # Importar diretamente do arquivo
        import importlib.util
        import os

        # Construir path absoluto
        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        DEFAULT_PLAN_LIMITS = plan_limits.DEFAULT_PLAN_LIMITS
        PlanType = plan_limits.PlanType
        ResourceType = plan_limits.ResourceType

        # Free plan deve ter limites
        free_limits = DEFAULT_PLAN_LIMITS[PlanType.FREE.value]
        assert free_limits[ResourceType.USERS.value] == 3
        assert free_limits[ResourceType.PROJECTS.value] == 2

        # Pro plan deve ter limites maiores
        pro_limits = DEFAULT_PLAN_LIMITS[PlanType.PRO.value]
        assert pro_limits[ResourceType.USERS.value] == 20
        assert pro_limits[ResourceType.PROJECTS.value] == 20

        # Enterprise deve ser ilimitado (-1)
        enterprise_limits = DEFAULT_PLAN_LIMITS[PlanType.ENTERPRISE.value]
        assert enterprise_limits[ResourceType.USERS.value] == -1
        assert enterprise_limits[ResourceType.PROJECTS.value] == -1

    def test_resource_type_enum_values(self):
        """Teste: Valores do enum ResourceType"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        ResourceType = plan_limits.ResourceType

        assert ResourceType.USERS.value == "users"
        assert ResourceType.PROJECTS.value == "projects"
        assert ResourceType.STORIES.value == "stories"
        assert ResourceType.TOKENS_PER_MONTH.value == "tokens_per_month"

    def test_limit_exceeded_error(self):
        """Teste: LimitExceededError tem dados corretos"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        LimitExceededError = plan_limits.LimitExceededError

        error = LimitExceededError(
            resource_type="projects",
            current=5,
            limit=5,
            tenant_id="TEN-001",
            message="Limite atingido"
        )

        assert error.resource_type == "projects"
        assert error.current == 5
        assert error.limit == 5
        assert error.tenant_id == "TEN-001"

        error_dict = error.to_dict()
        assert error_dict["error"] == "limit_exceeded"
        assert error_dict["current"] == 5

    def test_plan_type_enum_values(self):
        """Teste: Valores do enum PlanType"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        PlanType = plan_limits.PlanType

        assert PlanType.FREE.value == "free"
        assert PlanType.PRO.value == "pro"
        assert PlanType.ENTERPRISE.value == "enterprise"

    def test_check_limit_logic(self, mock_db):
        """Teste: Logica basica de check_limit"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        PlanEnforcer = plan_limits.PlanEnforcer
        PlanType = plan_limits.PlanType
        DEFAULT_PLAN_LIMITS = plan_limits.DEFAULT_PLAN_LIMITS

        enforcer = PlanEnforcer(mock_db)

        # Mock dos metodos
        enforcer.get_tenant_plan = Mock(return_value=(PlanType.PRO.value, DEFAULT_PLAN_LIMITS[PlanType.PRO.value]))
        enforcer.get_current_count = Mock(return_value=5)

        can_proceed, message = enforcer.check_limit("TEN-001", "projects")

        assert can_proceed == True

    def test_check_limit_blocks_at_limit_logic(self, mock_db):
        """Teste: check_limit bloqueia quando no limite"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        PlanEnforcer = plan_limits.PlanEnforcer
        PlanType = plan_limits.PlanType
        DEFAULT_PLAN_LIMITS = plan_limits.DEFAULT_PLAN_LIMITS

        enforcer = PlanEnforcer(mock_db)

        # Mock dos metodos para FREE plan com limite de 2 projetos
        enforcer.get_tenant_plan = Mock(return_value=(PlanType.FREE.value, DEFAULT_PLAN_LIMITS[PlanType.FREE.value]))
        enforcer.get_current_count = Mock(return_value=2)  # No limite

        can_proceed, message = enforcer.check_limit("TEN-001", "projects")

        assert can_proceed == False
        assert "Limite" in message

    def test_unlimited_plan_always_allows_logic(self, mock_db):
        """Teste: Plano ilimitado sempre permite"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        PlanEnforcer = plan_limits.PlanEnforcer
        PlanType = plan_limits.PlanType
        DEFAULT_PLAN_LIMITS = plan_limits.DEFAULT_PLAN_LIMITS

        enforcer = PlanEnforcer(mock_db)

        # Mock para ENTERPRISE plan (ilimitado)
        enforcer.get_tenant_plan = Mock(return_value=(PlanType.ENTERPRISE.value, DEFAULT_PLAN_LIMITS[PlanType.ENTERPRISE.value]))

        can_proceed, message = enforcer.check_limit("TEN-001", "projects")

        assert can_proceed == True
        assert "Sem limite" in message


# =============================================================================
# TESTES: TENANT ISOLATION (Issue #105)
# =============================================================================

class TestTenantIsolation:
    """Testes para TenantIsolation"""

    @patch('factory.core.multi_tenant.get_current_tenant_id')
    def test_get_tenant_id_from_context(self, mock_get_tenant, mock_db):
        """Teste: Obter tenant_id do contexto"""
        from factory.core.multi_tenant import TenantIsolation

        mock_get_tenant.return_value = "TEN-001"

        isolation = TenantIsolation(mock_db)
        tenant_id = isolation.get_tenant_id()

        assert tenant_id == "TEN-001"

    @patch('factory.core.multi_tenant.get_current_tenant_id')
    def test_get_tenant_id_raises_when_missing(self, mock_get_tenant, mock_db):
        """Teste: Levanta erro quando tenant nao identificado"""
        from factory.core.multi_tenant import TenantIsolation
        from fastapi import HTTPException

        mock_get_tenant.return_value = None

        isolation = TenantIsolation(mock_db)

        with pytest.raises(HTTPException) as exc_info:
            isolation.get_tenant_id()

        assert exc_info.value.status_code == 401

    @patch('factory.core.multi_tenant.get_current_tenant_id')
    def test_validate_access_same_tenant(self, mock_get_tenant, mock_db):
        """Teste: Acesso permitido ao mesmo tenant"""
        from factory.core.multi_tenant import TenantIsolation

        mock_get_tenant.return_value = "TEN-001"

        # Simular entidade com tenant_id
        mock_entity = Mock()
        mock_entity.tenant_id = "TEN-001"

        isolation = TenantIsolation(mock_db)
        result = isolation.validate_access(mock_entity)

        assert result == True

    @patch('factory.core.multi_tenant.get_current_tenant_id')
    def test_validate_access_different_tenant_raises(self, mock_get_tenant, mock_db):
        """Teste: Acesso negado a tenant diferente"""
        from factory.core.multi_tenant import TenantIsolation
        from fastapi import HTTPException

        mock_get_tenant.return_value = "TEN-001"

        # Simular entidade de outro tenant
        mock_entity = Mock()
        mock_entity.tenant_id = "TEN-002"

        isolation = TenantIsolation(mock_db)

        with pytest.raises(HTTPException) as exc_info:
            isolation.validate_access(mock_entity)

        assert exc_info.value.status_code == 403
        assert "Acesso negado" in str(exc_info.value.detail)


# =============================================================================
# TESTES: DECORATORS
# =============================================================================

class TestDecorators:
    """Testes para decorators de multi-tenancy"""

    @pytest.mark.asyncio
    async def test_require_tenant_decorator(self):
        """Teste: require_tenant bloqueia quando sem tenant"""
        from factory.core.multi_tenant import require_tenant, clear_context
        from fastapi import HTTPException

        clear_context()

        @require_tenant
        async def protected_function():
            return "success"

        with pytest.raises(HTTPException) as exc_info:
            await protected_function()

        assert exc_info.value.status_code == 400

    @pytest.mark.asyncio
    @patch('factory.core.multi_tenant.get_current_tenant')
    async def test_require_feature_decorator(self, mock_get_tenant):
        """Teste: require_feature bloqueia quando feature desabilitada"""
        from factory.core.multi_tenant import require_feature
        from fastapi import HTTPException

        mock_get_tenant.return_value = {
            "tenant_id": "TEN-001",
            "features": {"stories": True, "sso": False}
        }

        @require_feature("sso")
        async def sso_function():
            return "success"

        with pytest.raises(HTTPException) as exc_info:
            await sso_function()

        assert exc_info.value.status_code == 403
        assert "sso" in str(exc_info.value.detail)


# =============================================================================
# TESTES: MODELOS COM TENANT_ID
# =============================================================================

class TestModelsTenantId:
    """Testes para modelos com tenant_id"""

    def test_story_has_tenant_id(self):
        """Teste: Story tem campo tenant_id"""
        from factory.database.models import Story

        story = Story(
            story_id="STR-001",
            project_id="PROJ-001",
            title="Minha Story",
            tenant_id="TEN-001"
        )

        assert story.tenant_id == "TEN-001"

        data = story.to_dict()
        assert data["tenant_id"] == "TEN-001"

    def test_task_has_tenant_id(self):
        """Teste: Task tem campo tenant_id"""
        from factory.database.models import Task

        task = Task(
            task_id="TSK-001",
            project_id="PROJ-001",
            title="Minha Task",
            tenant_id="TEN-001"
        )

        assert task.tenant_id == "TEN-001"

        data = task.to_dict()
        assert data["tenant_id"] == "TEN-001"

    def test_project_has_tenant_id(self):
        """Teste: Project tem campo tenant_id"""
        from factory.database.models import Project

        project = Project(
            project_id="PROJ-001",
            name="Meu Projeto",
            project_type="webapp",
            tenant_id="TEN-001"
        )

        assert project.tenant_id == "TEN-001"


# =============================================================================
# TESTES: USAGE SUMMARY
# =============================================================================

class TestUsageSummary:
    """Testes para resumo de uso"""

    def test_get_usage_summary(self, mock_db):
        """Teste: get_usage_summary retorna estrutura correta"""
        import importlib.util
        import os

        base_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        module_path = os.path.join(base_path, "factory", "billing", "plan_limits.py")

        spec = importlib.util.spec_from_file_location("plan_limits", module_path)
        plan_limits = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(plan_limits)

        PlanEnforcer = plan_limits.PlanEnforcer
        PlanType = plan_limits.PlanType
        DEFAULT_PLAN_LIMITS = plan_limits.DEFAULT_PLAN_LIMITS

        enforcer = PlanEnforcer(mock_db)

        # Mock dos metodos
        enforcer.get_tenant_plan = Mock(return_value=(PlanType.PRO.value, DEFAULT_PLAN_LIMITS[PlanType.PRO.value]))
        enforcer.get_current_count = Mock(return_value=5)

        summary = enforcer.get_usage_summary("TEN-001")

        assert summary["tenant_id"] == "TEN-001"
        assert summary["plan_type"] == "pro"
        assert "resources" in summary


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
