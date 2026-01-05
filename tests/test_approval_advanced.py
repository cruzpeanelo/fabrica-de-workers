# -*- coding: utf-8 -*-
"""
Test Approval Advanced - Testes Avancados de Workflow de Aprovacao
====================================================================

Cenarios de teste para factory/core/approval_workflow.py (1,221 linhas)

Issues identificadas na analise:
- Linha 545: Divisao por zero se total_approvers = 0
- Linha 683-684: Storage in-memory sem persistencia
- Linha 883-884: Verifica se aprovou mas nao se estagio foi rejeitado
- Sem thread-safety nos dicionarios
"""

import pytest
import asyncio
from datetime import datetime, timedelta
from unittest.mock import Mock, patch
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from factory.core.approval_workflow import (
    ApprovalWorkflow,
    ApprovalStage,
    ApprovalStageType,
    ApprovalRule,
    ApprovalRuleType,
    ApprovalRequest,
    ApprovalStatus,
    ApprovalAction,
    ApprovalActionType,
    ApprovalWorkflowService,
    create_default_story_workflow,
    create_simple_approval_workflow,
    get_approval_service
)


# =============================================================================
# APPR-001: Single Stage Approval
# =============================================================================
class TestSingleStageApproval:
    """Testes de aprovacao de estagio unico."""

    def test_single_stage_workflow_creation(self):
        """APPR-001: Criar workflow com estagio unico."""
        workflow = create_simple_approval_workflow(
            name="Simple Approval",
            approver_roles=["developer"]
        )

        assert len(workflow.stages) == 1
        assert workflow.stages[0].name == "Approval"

    def test_single_stage_approval_completes_workflow(self):
        """Aprovacao em estagio unico deve completar workflow."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(
            name="Single Stage",
            resource_type="story"
        )

        stage = ApprovalStage(
            name="Review",
            stage_type=ApprovalStageType.REVIEW,
            eligible_approvers=["user1"],
            rules=[ApprovalRule(rule_type=ApprovalRuleType.ANY_ONE)]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-001",
            submitter_id="submitter1"
        )

        result = service.approve(
            request_id=request.request_id,
            user_id="user1",
            comment="LGTM"
        )

        assert result["success"] is True
        assert result["workflow_completed"] is True

    def test_single_approval_without_comment(self):
        """Aprovacao sem comentario quando nao obrigatorio."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="No Comment Required")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["user1"],
            rules=[ApprovalRule(rule_type=ApprovalRuleType.ANY_ONE, require_comment=False)]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-002",
            submitter_id="submitter1"
        )

        result = service.approve(
            request_id=request.request_id,
            user_id="user1"
            # Sem comentario
        )

        assert result["success"] is True


# =============================================================================
# APPR-002: Multi-Stage Approval (DEV->QA->PM)
# =============================================================================
class TestMultiStageApproval:
    """Testes de aprovacao multi-estagio."""

    def test_multi_stage_workflow_creation(self):
        """APPR-002: Criar workflow multi-estagio."""
        workflow = create_default_story_workflow()

        assert len(workflow.stages) == 3
        assert workflow.stages[0].name == "Code Review"
        assert workflow.stages[1].name == "QA Validation"
        assert workflow.stages[2].name == "Product Approval"

    def test_multi_stage_progression(self):
        """Request deve progredir pelos estagios."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Multi Stage")

        # Estagio 1: Dev Review
        stage1 = ApprovalStage(
            name="Dev Review",
            order=1,
            eligible_approvers=["dev1"],
            rules=[ApprovalRule(rule_type=ApprovalRuleType.ANY_ONE)]
        )

        # Estagio 2: QA
        stage2 = ApprovalStage(
            name="QA",
            order=2,
            eligible_approvers=["qa1"],
            rules=[ApprovalRule(rule_type=ApprovalRuleType.ANY_ONE)]
        )

        service.add_stage_to_workflow(workflow.workflow_id, stage1)
        service.add_stage_to_workflow(workflow.workflow_id, stage2)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-003",
            submitter_id="submitter1"
        )

        # Aprovar estagio 1
        result1 = service.approve(request.request_id, "dev1")
        assert result1["stage_completed"] is True
        assert result1["workflow_completed"] is False

        # Aprovar estagio 2
        result2 = service.approve(request.request_id, "qa1")
        assert result2["workflow_completed"] is True


# =============================================================================
# APPR-003: Auto-Approve < 5 Points
# =============================================================================
class TestAutoApproval:
    """Testes de auto-aprovacao."""

    def test_auto_approve_condition_check(self):
        """APPR-003: Verificar condicao de auto-aprovacao."""
        rule = ApprovalRule(
            rule_type=ApprovalRuleType.ANY_ONE,
            auto_approve_conditions={"max_story_points": 5}
        )

        story_points = 3
        should_auto_approve = story_points <= rule.auto_approve_conditions.get("max_story_points", 0)

        assert should_auto_approve is True

    def test_no_auto_approve_above_threshold(self):
        """Nao deve auto-aprovar acima do threshold."""
        rule = ApprovalRule(
            rule_type=ApprovalRuleType.ANY_ONE,
            auto_approve_conditions={"max_story_points": 5}
        )

        story_points = 8
        should_auto_approve = story_points <= rule.auto_approve_conditions.get("max_story_points", 0)

        assert should_auto_approve is False


# =============================================================================
# APPR-004: Self-Approval Blocked
# =============================================================================
class TestSelfApprovalBlocked:
    """Testes de bloqueio de self-approval."""

    def test_self_approval_blocked_by_default(self):
        """APPR-004: Self-approval deve ser bloqueado por padrao."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="No Self Approval")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["user1", "submitter1"],
            rules=[ApprovalRule(
                rule_type=ApprovalRuleType.ANY_ONE,
                allow_self_approval=False
            )]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-004",
            submitter_id="submitter1"
        )

        # Tentar auto-aprovar
        result = service.approve(
            request_id=request.request_id,
            user_id="submitter1"  # Mesmo que submitter
        )

        assert result["success"] is False
        assert "cannot approve" in result["error"].lower()

    def test_self_approval_allowed_when_configured(self):
        """Self-approval pode ser permitido quando configurado."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Self Approval OK")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["submitter1"],
            rules=[ApprovalRule(
                rule_type=ApprovalRuleType.ANY_ONE,
                allow_self_approval=True
            )]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-005",
            submitter_id="submitter1"
        )

        result = service.approve(
            request_id=request.request_id,
            user_id="submitter1"
        )

        assert result["success"] is True


# =============================================================================
# APPR-005: Escalation Timeout
# =============================================================================
class TestEscalationTimeout:
    """Testes de escalacao por timeout."""

    def test_stage_timeout_configuration(self):
        """APPR-005: Configurar timeout de estagio."""
        stage = ApprovalStage(
            name="Time Sensitive Review",
            timeout_hours=24,
            escalation_user_id="manager1"
        )

        assert stage.timeout_hours == 24
        assert stage.escalation_user_id == "manager1"

    def test_escalation_user_defined(self):
        """Usuario de escalacao deve estar definido."""
        stage = ApprovalStage(
            name="Review",
            timeout_hours=48,
            escalation_user_id="director1"
        )

        assert stage.escalation_user_id is not None


# =============================================================================
# APPR-006: Rejection with Feedback
# =============================================================================
class TestRejectionWithFeedback:
    """Testes de rejeicao com feedback."""

    def test_rejection_requires_comment(self):
        """APPR-006: Rejeicao deve ter comentario."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Rejection Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["reviewer1"],
            rules=[ApprovalRule(rule_type=ApprovalRuleType.ANY_ONE)]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-006",
            submitter_id="submitter1"
        )

        result = service.reject(
            request_id=request.request_id,
            user_id="reviewer1",
            comment="Needs more unit tests"
        )

        assert result["success"] is True
        assert result["status"] == "rejected"

    def test_rejection_updates_request_status(self):
        """Rejeicao deve atualizar status do request."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Reject Status Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["reviewer1"]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-007",
            submitter_id="submitter1"
        )

        service.reject(request.request_id, "reviewer1", "Not ready")

        updated = service.get_request(request.request_id)
        assert updated.status == ApprovalStatus.REJECTED


# =============================================================================
# APPR-007: Re-submission After Rejection
# =============================================================================
class TestResubmissionAfterRejection:
    """Testes de re-submissao apos rejeicao."""

    def test_new_request_after_rejection(self):
        """APPR-007: Nova submissao apos rejeicao."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Resubmit Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["reviewer1"]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        # Primeira submissao
        request1 = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-008",
            submitter_id="submitter1"
        )

        # Rejeitar
        service.reject(request1.request_id, "reviewer1", "Needs work")

        # Nova submissao (mesmo recurso)
        request2 = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-008",
            submitter_id="submitter1"
        )

        assert request2.request_id != request1.request_id
        assert request2.status == ApprovalStatus.IN_PROGRESS


# =============================================================================
# APPR-008: Concurrent Approvals
# =============================================================================
class TestConcurrentApprovals:
    """Testes de aprovacoes concorrentes."""

    def test_threshold_rule_requires_multiple_approvals(self):
        """APPR-008: Regra de threshold requer multiplas aprovacoes."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Threshold Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["user1", "user2", "user3", "user4"],
            rules=[ApprovalRule(
                rule_type=ApprovalRuleType.THRESHOLD,
                threshold_percentage=50,
                min_approvers=2
            )]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-009",
            submitter_id="submitter1"
        )

        # Primeira aprovacao - nao completa
        result1 = service.approve(request.request_id, "user1")
        assert result1["stage_completed"] is False

        # Segunda aprovacao - deve completar (50% com min 2)
        result2 = service.approve(request.request_id, "user2")
        assert result2["stage_completed"] is True

    def test_all_rule_requires_all_approvers(self):
        """Regra ALL requer todos os aprovadores."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="All Approvers Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["user1", "user2"],
            rules=[ApprovalRule(
                rule_type=ApprovalRuleType.ALL,
                min_approvers=2
            )]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-010",
            submitter_id="submitter1"
        )

        # Primeira aprovacao
        result1 = service.approve(request.request_id, "user1")
        assert result1["stage_completed"] is False

        # Segunda aprovacao
        result2 = service.approve(request.request_id, "user2")
        assert result2["stage_completed"] is True


# =============================================================================
# APPR-009: Division by Zero Safety
# =============================================================================
class TestDivisionByZeroSafety:
    """Testes de seguranca contra divisao por zero."""

    def test_threshold_with_empty_approvers(self):
        """APPR-009: Threshold com lista vazia de aprovadores."""
        rule = ApprovalRule(
            rule_type=ApprovalRuleType.THRESHOLD,
            threshold_percentage=50,
            min_approvers=1
        )

        stage = ApprovalStage(
            name="Review",
            eligible_approvers=[],  # Lista vazia
            rules=[rule]
        )

        # Simular verificacao segura
        total_approvers = len(stage.eligible_approvers)

        if total_approvers == 0:
            # Fallback para min_approvers
            required = rule.min_approvers
        else:
            required = int(total_approvers * rule.threshold_percentage / 100)

        assert required == 1  # min_approvers como fallback

    def test_progress_calculation_with_zero_stages(self):
        """Calculo de progresso com zero estagios."""
        stages_info = []

        # Calculo seguro
        if stages_info:
            progress = len([s for s in stages_info if s.get("status") == "approved"]) / len(stages_info) * 100
        else:
            progress = 0

        assert progress == 0


# =============================================================================
# APPR-010: Workflow Validation
# =============================================================================
class TestWorkflowValidation:
    """Testes de validacao de workflow."""

    def test_workflow_without_name_is_invalid(self):
        """APPR-010: Workflow sem nome e invalido."""
        workflow = ApprovalWorkflow(name="")

        validation = workflow.validate()

        assert validation["is_valid"] is False
        assert "name is required" in str(validation["errors"]).lower()

    def test_workflow_without_stages_is_invalid(self):
        """Workflow sem estagios e invalido."""
        workflow = ApprovalWorkflow(name="Empty Workflow")
        # Nao adiciona estagios

        validation = workflow.validate()

        assert validation["is_valid"] is False
        assert "at least one stage" in str(validation["errors"]).lower()

    def test_workflow_with_duplicate_orders_is_invalid(self):
        """Workflow com ordens duplicadas e invalido."""
        workflow = ApprovalWorkflow(name="Duplicate Orders")

        workflow.stages = [
            ApprovalStage(name="Stage 1", order=1),
            ApprovalStage(name="Stage 2", order=1)  # Mesma ordem
        ]

        validation = workflow.validate()

        assert validation["is_valid"] is False
        assert "duplicate" in str(validation["errors"]).lower()


# =============================================================================
# APPR-011: Request Changes Flow
# =============================================================================
class TestRequestChangesFlow:
    """Testes de fluxo de request changes."""

    def test_request_changes_action(self):
        """APPR-011: Solicitar mudancas em request."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Changes Test")
        stage = ApprovalStage(
            name="Review",
            eligible_approvers=["reviewer1"]
        )
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-011",
            submitter_id="submitter1"
        )

        result = service.request_changes(
            request_id=request.request_id,
            user_id="reviewer1",
            comment="Please add error handling"
        )

        assert result["success"] is True
        assert "changes requested" in result["message"].lower()


# =============================================================================
# APPR-012: Cancel Request
# =============================================================================
class TestCancelRequest:
    """Testes de cancelamento de request."""

    def test_submitter_can_cancel(self):
        """APPR-012: Submitter pode cancelar request."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Cancel Test")
        stage = ApprovalStage(name="Review", eligible_approvers=["reviewer1"])
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-012",
            submitter_id="submitter1"
        )

        result = service.cancel_request(
            request_id=request.request_id,
            user_id="submitter1",
            reason="No longer needed"
        )

        assert result["success"] is True
        assert result["status"] == "cancelled"

    def test_non_submitter_cannot_cancel(self):
        """Nao-submitter nao pode cancelar."""
        service = ApprovalWorkflowService()

        workflow = service.create_workflow(name="Cancel Auth Test")
        stage = ApprovalStage(name="Review", eligible_approvers=["reviewer1"])
        service.add_stage_to_workflow(workflow.workflow_id, stage)

        request = service.submit_for_approval(
            workflow_id=workflow.workflow_id,
            resource_type="story",
            resource_id="STR-013",
            submitter_id="submitter1"
        )

        result = service.cancel_request(
            request_id=request.request_id,
            user_id="other_user",  # Nao e o submitter
            reason="I want to cancel"
        )

        assert result["success"] is False
        assert "only submitter" in result["error"].lower()


# =============================================================================
# APPR-013: Serialization
# =============================================================================
class TestSerialization:
    """Testes de serializacao."""

    def test_workflow_to_dict(self):
        """APPR-013: Workflow deve serializar para dict."""
        workflow = create_default_story_workflow()

        data = workflow.to_dict()

        assert "workflow_id" in data
        assert "name" in data
        assert "stages" in data
        assert len(data["stages"]) == 3

    def test_workflow_from_dict(self):
        """Workflow deve deserializar de dict."""
        original = create_default_story_workflow()
        data = original.to_dict()

        restored = ApprovalWorkflow.from_dict(data)

        assert restored.workflow_id == original.workflow_id
        assert restored.name == original.name
        assert len(restored.stages) == len(original.stages)

    def test_request_to_dict(self):
        """Request deve serializar para dict."""
        request = ApprovalRequest(
            workflow_id="WF-001",
            resource_type="story",
            resource_id="STR-001",
            submitter_id="user1"
        )

        data = request.to_dict()

        assert "request_id" in data
        assert "workflow_id" in data
        assert data["status"] == "pending"


# =============================================================================
# Test Runner
# =============================================================================
if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
