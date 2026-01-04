# -*- coding: utf-8 -*-
"""
Tests for Workflow Engine
Plataforma E v6.5

Tests for Issue #211:
1. Workflow data structures
2. Step types
3. Workflow execution
4. Context management
5. Builder API
"""

import pytest
from datetime import datetime
import asyncio


# =============================================================================
# ENUM TESTS
# =============================================================================

class TestWorkflowEnums:
    """Tests for workflow enumerations"""

    def test_trigger_types(self):
        """Should have all trigger types"""
        from factory.core.workflow_engine import TriggerType

        assert TriggerType.MANUAL.value == "manual"
        assert TriggerType.SCHEDULE.value == "schedule"
        assert TriggerType.WEBHOOK.value == "webhook"
        assert TriggerType.EVENT.value == "event"

    def test_workflow_status(self):
        """Should have all workflow statuses"""
        from factory.core.workflow_engine import WorkflowStatus

        assert WorkflowStatus.DRAFT.value == "draft"
        assert WorkflowStatus.ACTIVE.value == "active"
        assert WorkflowStatus.PAUSED.value == "paused"
        assert WorkflowStatus.ARCHIVED.value == "archived"

    def test_step_types(self):
        """Should have all step types"""
        from factory.core.workflow_engine import StepType

        assert StepType.WORKER.value == "worker"
        assert StepType.CONDITION.value == "condition"
        assert StepType.LOOP.value == "loop"
        assert StepType.PARALLEL.value == "parallel"
        assert StepType.WAIT.value == "wait"

    def test_run_status(self):
        """Should have all run statuses"""
        from factory.core.workflow_engine import RunStatus

        assert RunStatus.PENDING.value == "pending"
        assert RunStatus.RUNNING.value == "running"
        assert RunStatus.COMPLETED.value == "completed"
        assert RunStatus.FAILED.value == "failed"


# =============================================================================
# WORKFLOW STEP TESTS
# =============================================================================

class TestWorkflowStep:
    """Tests for WorkflowStep dataclass"""

    def test_step_creation(self):
        """Should create step with required fields"""
        from factory.core.workflow_engine import WorkflowStep, StepType

        step = WorkflowStep(
            step_id="step_1",
            step_type=StepType.WORKER,
            name="Process Data"
        )

        assert step.step_id == "step_1"
        assert step.step_type == StepType.WORKER
        assert step.name == "Process Data"

    def test_step_defaults(self):
        """Should have sensible defaults"""
        from factory.core.workflow_engine import WorkflowStep, StepType

        step = WorkflowStep(
            step_id="step_1",
            step_type=StepType.WORKER
        )

        assert step.timeout == 300
        assert step.retry_count == 0
        assert step.config == {}

    def test_step_to_dict(self):
        """Should convert to dictionary"""
        from factory.core.workflow_engine import WorkflowStep, StepType

        step = WorkflowStep(
            step_id="step_1",
            step_type=StepType.WORKER,
            name="Test",
            worker_id="worker_123"
        )

        result = step.to_dict()

        assert result["step_id"] == "step_1"
        assert result["step_type"] == "worker"
        assert result["worker_id"] == "worker_123"


# =============================================================================
# WORKFLOW TESTS
# =============================================================================

class TestWorkflow:
    """Tests for Workflow dataclass"""

    def test_workflow_creation(self):
        """Should create workflow with required fields"""
        from factory.core.workflow_engine import Workflow

        workflow = Workflow(
            workflow_id="wf_1",
            name="Sales Process"
        )

        assert workflow.workflow_id == "wf_1"
        assert workflow.name == "Sales Process"

    def test_workflow_defaults(self):
        """Should have sensible defaults"""
        from factory.core.workflow_engine import Workflow, TriggerType, WorkflowStatus

        workflow = Workflow(
            workflow_id="wf_1",
            name="Test"
        )

        assert workflow.trigger_type == TriggerType.MANUAL
        assert workflow.status == WorkflowStatus.DRAFT
        assert workflow.steps == []

    def test_workflow_to_dict(self):
        """Should convert to dictionary"""
        from factory.core.workflow_engine import Workflow

        workflow = Workflow(
            workflow_id="wf_1",
            name="Test",
            description="Test workflow"
        )

        result = workflow.to_dict()

        assert result["workflow_id"] == "wf_1"
        assert result["name"] == "Test"
        assert result["description"] == "Test workflow"
        assert "steps" in result


# =============================================================================
# WORKFLOW CONTEXT TESTS
# =============================================================================

class TestWorkflowContext:
    """Tests for WorkflowContext"""

    def test_context_get_from_inputs(self):
        """Should get values from inputs"""
        from factory.core.workflow_engine import WorkflowContext

        context = WorkflowContext(
            run_id="run_1",
            inputs={"name": "John", "value": 100}
        )

        assert context.get("name") == "John"
        assert context.get("value") == 100

    def test_context_get_from_variables(self):
        """Variables should override inputs"""
        from factory.core.workflow_engine import WorkflowContext

        context = WorkflowContext(
            run_id="run_1",
            inputs={"name": "John"}
        )
        context.set("name", "Jane")

        assert context.get("name") == "Jane"

    def test_context_get_default(self):
        """Should return default for missing keys"""
        from factory.core.workflow_engine import WorkflowContext

        context = WorkflowContext(run_id="run_1")

        assert context.get("missing") is None
        assert context.get("missing", "default") == "default"

    def test_context_update_step_output(self):
        """Should store step outputs"""
        from factory.core.workflow_engine import WorkflowContext

        context = WorkflowContext(run_id="run_1")
        context.update_step_output("step_1", {"result": "success"})

        assert context.step_outputs["step_1"] == {"result": "success"}


# =============================================================================
# WORKFLOW RUN TESTS
# =============================================================================

class TestWorkflowRun:
    """Tests for WorkflowRun dataclass"""

    def test_run_creation(self):
        """Should create run with required fields"""
        from factory.core.workflow_engine import WorkflowRun

        run = WorkflowRun(
            run_id="run_1",
            workflow_id="wf_1"
        )

        assert run.run_id == "run_1"
        assert run.workflow_id == "wf_1"

    def test_run_to_dict(self):
        """Should convert to dictionary"""
        from factory.core.workflow_engine import WorkflowRun, RunStatus

        run = WorkflowRun(
            run_id="run_1",
            workflow_id="wf_1",
            status=RunStatus.COMPLETED,
            inputs={"key": "value"}
        )

        result = run.to_dict()

        assert result["run_id"] == "run_1"
        assert result["status"] == "completed"
        assert result["inputs"] == {"key": "value"}


# =============================================================================
# STEP RESULT TESTS
# =============================================================================

class TestStepResult:
    """Tests for StepResult dataclass"""

    def test_result_creation(self):
        """Should create result with required fields"""
        from factory.core.workflow_engine import StepResult, StepStatus

        result = StepResult(
            step_id="step_1",
            status=StepStatus.COMPLETED
        )

        assert result.step_id == "step_1"
        assert result.status == StepStatus.COMPLETED

    def test_result_with_output(self):
        """Should store output"""
        from factory.core.workflow_engine import StepResult, StepStatus

        result = StepResult(
            step_id="step_1",
            status=StepStatus.COMPLETED,
            output={"data": [1, 2, 3]}
        )

        assert result.output == {"data": [1, 2, 3]}

    def test_result_with_error(self):
        """Should store error message"""
        from factory.core.workflow_engine import StepResult, StepStatus

        result = StepResult(
            step_id="step_1",
            status=StepStatus.FAILED,
            error="Connection refused"
        )

        assert result.status == StepStatus.FAILED
        assert result.error == "Connection refused"


# =============================================================================
# WORKFLOW ENGINE TESTS
# =============================================================================

class TestWorkflowEngine:
    """Tests for WorkflowEngine class"""

    def test_engine_creation(self):
        """Should create engine"""
        from factory.core.workflow_engine import WorkflowEngine

        engine = WorkflowEngine()

        assert engine is not None

    def test_add_workflow(self):
        """Should add workflow to engine"""
        from factory.core.workflow_engine import WorkflowEngine, Workflow

        engine = WorkflowEngine()
        workflow = Workflow(workflow_id="wf_1", name="Test")

        engine.add_workflow(workflow)

        assert engine.get_workflow("wf_1") is not None

    def test_register_worker(self):
        """Should register worker handler"""
        from factory.core.workflow_engine import WorkflowEngine

        engine = WorkflowEngine()

        async def handler(inputs):
            return inputs

        engine.register_worker("worker_1", handler)

        assert "worker_1" in engine._workers

    def test_get_runs_by_workflow(self):
        """Should get runs for workflow"""
        from factory.core.workflow_engine import WorkflowEngine, WorkflowRun

        engine = WorkflowEngine()
        run1 = WorkflowRun(run_id="run_1", workflow_id="wf_1")
        run2 = WorkflowRun(run_id="run_2", workflow_id="wf_1")
        run3 = WorkflowRun(run_id="run_3", workflow_id="wf_2")

        engine._runs["run_1"] = run1
        engine._runs["run_2"] = run2
        engine._runs["run_3"] = run3

        runs = engine.get_runs_by_workflow("wf_1")

        assert len(runs) == 2


# =============================================================================
# WORKFLOW BUILDER TESTS
# =============================================================================

class TestWorkflowBuilder:
    """Tests for WorkflowBuilder fluent API"""

    def test_builder_basic(self):
        """Should build basic workflow"""
        from factory.core.workflow_engine import WorkflowBuilder

        workflow = (
            WorkflowBuilder("My Workflow")
            .description("Test workflow")
            .build()
        )

        assert workflow.name == "My Workflow"
        assert workflow.description == "Test workflow"

    def test_builder_with_steps(self):
        """Should build workflow with steps"""
        from factory.core.workflow_engine import WorkflowBuilder, TriggerType

        workflow = (
            WorkflowBuilder("ETL Pipeline")
            .trigger(TriggerType.SCHEDULE, {"cron": "0 * * * *"})
            .worker("Extract", "extractor")
            .worker("Transform", "transformer")
            .worker("Load", "loader")
            .build()
        )

        assert workflow.trigger_type == TriggerType.SCHEDULE
        assert len(workflow.steps) == 3
        assert workflow.steps[0].name == "Extract"

    def test_builder_activate(self):
        """Should activate workflow"""
        from factory.core.workflow_engine import WorkflowBuilder, WorkflowStatus

        workflow = (
            WorkflowBuilder("Active Workflow")
            .activate()
            .build()
        )

        assert workflow.status == WorkflowStatus.ACTIVE


# =============================================================================
# CONDITION EVALUATION TESTS
# =============================================================================

class TestConditionEvaluation:
    """Tests for condition evaluation"""

    def test_condition_true(self):
        """Should evaluate true condition"""
        from factory.core.workflow_engine import WorkflowEngine, WorkflowContext

        engine = WorkflowEngine()
        context = WorkflowContext(run_id="run_1")

        # Create mock step
        from factory.core.workflow_engine import WorkflowStep, StepType
        step = WorkflowStep(
            step_id="cond_1",
            step_type=StepType.CONDITION,
            condition="true"
        )

        result = engine._evaluate_condition(step, context)
        assert result is True

    def test_condition_comparison(self):
        """Should evaluate comparison condition"""
        from factory.core.workflow_engine import WorkflowEngine, WorkflowContext

        engine = WorkflowEngine()
        context = WorkflowContext(
            run_id="run_1",
            inputs={"value": 1500}
        )

        from factory.core.workflow_engine import WorkflowStep, StepType
        step = WorkflowStep(
            step_id="cond_1",
            step_type=StepType.CONDITION,
            condition="value > 1000"
        )

        result = engine._evaluate_condition(step, context)
        assert result is True

    def test_condition_less_than(self):
        """Should evaluate less than condition"""
        from factory.core.workflow_engine import WorkflowEngine, WorkflowContext

        engine = WorkflowEngine()
        context = WorkflowContext(
            run_id="run_1",
            inputs={"value": 500}
        )

        from factory.core.workflow_engine import WorkflowStep, StepType
        step = WorkflowStep(
            step_id="cond_1",
            step_type=StepType.CONDITION,
            condition="value < 1000"
        )

        result = engine._evaluate_condition(step, context)
        assert result is True


# =============================================================================
# GLOBAL ENGINE TESTS
# =============================================================================

class TestGlobalEngine:
    """Tests for global engine singleton"""

    def test_get_workflow_engine(self):
        """Should return singleton engine"""
        from factory.core.workflow_engine import get_workflow_engine

        engine1 = get_workflow_engine()
        engine2 = get_workflow_engine()

        assert engine1 is engine2


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestWorkflowIntegration:
    """Integration tests for workflow engine"""

    def test_full_workflow_structure(self):
        """Should create complete workflow structure"""
        from factory.core.workflow_engine import (
            Workflow, WorkflowStep, StepType, TriggerType, WorkflowStatus
        )

        workflow = Workflow(
            workflow_id="wf_sales",
            name="Sales Process",
            description="Process sales orders",
            trigger_type=TriggerType.WEBHOOK,
            status=WorkflowStatus.ACTIVE,
            steps=[
                WorkflowStep(
                    step_id="validate",
                    step_type=StepType.WORKER,
                    name="Validate Order",
                    worker_id="validator",
                    on_success="check_value"
                ),
                WorkflowStep(
                    step_id="check_value",
                    step_type=StepType.CONDITION,
                    name="Check Value",
                    condition="order_value > 1000",
                    on_success="approve_manual",
                    on_failure="auto_approve"
                ),
                WorkflowStep(
                    step_id="approve_manual",
                    step_type=StepType.WORKER,
                    name="Manual Approval",
                    worker_id="approval_worker"
                ),
                WorkflowStep(
                    step_id="auto_approve",
                    step_type=StepType.WORKER,
                    name="Auto Approve",
                    worker_id="auto_approver"
                )
            ]
        )

        assert len(workflow.steps) == 4
        assert workflow.steps[1].step_type == StepType.CONDITION

    def test_step_linking(self):
        """Steps should be properly linked"""
        from factory.core.workflow_engine import WorkflowBuilder

        workflow = (
            WorkflowBuilder("Linked Workflow")
            .worker("Step 1", "worker1")
            .worker("Step 2", "worker2")
            .worker("Step 3", "worker3")
            .build()
        )

        # First step should link to second
        assert workflow.steps[0].on_success == workflow.steps[1].step_id
        assert workflow.steps[1].on_success == workflow.steps[2].step_id


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
