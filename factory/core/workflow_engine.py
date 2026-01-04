# -*- coding: utf-8 -*-
"""
Workflow Engine - Plataforma E v6.5
====================================

Generic workflow execution engine for orchestrating complex flows.
Supports: conditions, loops, parallel execution, triggers.

Issue #211: [FRONT] Implementar orquestracao de fluxos de trabalho complexos
"""

import asyncio
import uuid
import json
from datetime import datetime
from enum import Enum
from typing import Optional, List, Dict, Any, Callable
from dataclasses import dataclass, field


# =============================================================================
# ENUMS
# =============================================================================

class TriggerType(str, Enum):
    """Types of workflow triggers"""
    MANUAL = "manual"
    SCHEDULE = "schedule"
    WEBHOOK = "webhook"
    EVENT = "event"


class WorkflowStatus(str, Enum):
    """Workflow definition status"""
    DRAFT = "draft"
    ACTIVE = "active"
    PAUSED = "paused"
    ARCHIVED = "archived"


class StepType(str, Enum):
    """Types of workflow steps"""
    WORKER = "worker"
    CONDITION = "condition"
    LOOP = "loop"
    PARALLEL = "parallel"
    WAIT = "wait"
    NOTIFY = "notify"
    TRANSFORM = "transform"


class RunStatus(str, Enum):
    """Workflow run status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class StepStatus(str, Enum):
    """Individual step execution status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class WorkflowStep:
    """Represents a single step in a workflow"""
    step_id: str
    step_type: StepType
    name: str = ""
    config: Dict[str, Any] = field(default_factory=dict)
    worker_id: Optional[str] = None
    on_success: Optional[str] = None  # Next step ID
    on_failure: Optional[str] = None  # Error handling step
    timeout: int = 300  # seconds
    retry_count: int = 0
    condition: Optional[str] = None  # For condition steps

    def to_dict(self) -> dict:
        return {
            "step_id": self.step_id,
            "step_type": self.step_type.value if isinstance(self.step_type, Enum) else self.step_type,
            "name": self.name,
            "config": self.config,
            "worker_id": self.worker_id,
            "on_success": self.on_success,
            "on_failure": self.on_failure,
            "timeout": self.timeout,
            "retry_count": self.retry_count,
            "condition": self.condition
        }


@dataclass
class Workflow:
    """Workflow definition"""
    workflow_id: str
    name: str
    description: str = ""
    trigger_type: TriggerType = TriggerType.MANUAL
    trigger_config: Dict[str, Any] = field(default_factory=dict)
    status: WorkflowStatus = WorkflowStatus.DRAFT
    steps: List[WorkflowStep] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    created_by: Optional[str] = None
    project_id: Optional[str] = None

    def to_dict(self) -> dict:
        return {
            "workflow_id": self.workflow_id,
            "name": self.name,
            "description": self.description,
            "trigger_type": self.trigger_type.value if isinstance(self.trigger_type, Enum) else self.trigger_type,
            "trigger_config": self.trigger_config,
            "status": self.status.value if isinstance(self.status, Enum) else self.status,
            "steps": [s.to_dict() for s in self.steps],
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by,
            "project_id": self.project_id
        }


@dataclass
class StepResult:
    """Result of a single step execution"""
    step_id: str
    status: StepStatus
    output: Any = None
    error: Optional[str] = None
    started_at: datetime = field(default_factory=datetime.utcnow)
    completed_at: Optional[datetime] = None
    duration: float = 0.0

    def to_dict(self) -> dict:
        return {
            "step_id": self.step_id,
            "status": self.status.value if isinstance(self.status, Enum) else self.status,
            "output": self.output,
            "error": self.error,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration": self.duration
        }


@dataclass
class WorkflowRun:
    """Record of a workflow execution"""
    run_id: str
    workflow_id: str
    status: RunStatus = RunStatus.PENDING
    inputs: Dict[str, Any] = field(default_factory=dict)
    outputs: Dict[str, Any] = field(default_factory=dict)
    step_results: List[StepResult] = field(default_factory=list)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    error: Optional[str] = None

    def to_dict(self) -> dict:
        return {
            "run_id": self.run_id,
            "workflow_id": self.workflow_id,
            "status": self.status.value if isinstance(self.status, Enum) else self.status,
            "inputs": self.inputs,
            "outputs": self.outputs,
            "step_results": [s.to_dict() for s in self.step_results],
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error": self.error
        }


@dataclass
class WorkflowContext:
    """Execution context passed between steps"""
    run_id: str
    inputs: Dict[str, Any] = field(default_factory=dict)
    step_outputs: Dict[str, Any] = field(default_factory=dict)
    variables: Dict[str, Any] = field(default_factory=dict)

    def get(self, key: str, default: Any = None) -> Any:
        """Get value from context"""
        if key in self.variables:
            return self.variables[key]
        if key in self.step_outputs:
            return self.step_outputs[key]
        return self.inputs.get(key, default)

    def set(self, key: str, value: Any):
        """Set variable in context"""
        self.variables[key] = value

    def update_step_output(self, step_id: str, output: Any):
        """Store step output"""
        self.step_outputs[step_id] = output


# =============================================================================
# WORKFLOW ENGINE
# =============================================================================

class WorkflowEngine:
    """
    Engine for executing workflows as DAGs.

    Supports:
    - Sequential execution
    - Conditional branching
    - Loops
    - Parallel execution
    - Error handling
    """

    def __init__(self, db_session=None):
        self.db = db_session
        self._workflows: Dict[str, Workflow] = {}
        self._runs: Dict[str, WorkflowRun] = {}
        self._workers: Dict[str, Callable] = {}

    def register_worker(self, worker_id: str, handler: Callable):
        """Register a worker handler function"""
        self._workers[worker_id] = handler

    def add_workflow(self, workflow: Workflow):
        """Add workflow to engine"""
        self._workflows[workflow.workflow_id] = workflow

    def get_workflow(self, workflow_id: str) -> Optional[Workflow]:
        """Get workflow by ID"""
        return self._workflows.get(workflow_id)

    def get_run(self, run_id: str) -> Optional[WorkflowRun]:
        """Get workflow run by ID"""
        return self._runs.get(run_id)

    async def execute(self, workflow_id: str, inputs: Dict[str, Any] = None) -> WorkflowRun:
        """
        Execute a workflow

        Args:
            workflow_id: ID of workflow to execute
            inputs: Input parameters for the workflow

        Returns:
            WorkflowRun with results
        """
        workflow = self.get_workflow(workflow_id)
        if not workflow:
            raise ValueError(f"Workflow {workflow_id} not found")

        if workflow.status != WorkflowStatus.ACTIVE:
            raise ValueError(f"Workflow {workflow_id} is not active")

        # Create run
        run = WorkflowRun(
            run_id=str(uuid.uuid4()),
            workflow_id=workflow_id,
            inputs=inputs or {},
            status=RunStatus.RUNNING,
            started_at=datetime.utcnow()
        )
        self._runs[run.run_id] = run

        # Create context
        context = WorkflowContext(
            run_id=run.run_id,
            inputs=inputs or {}
        )

        try:
            # Execute steps in order
            step_map = {s.step_id: s for s in workflow.steps}
            current_step = workflow.steps[0] if workflow.steps else None

            while current_step:
                result = await self._execute_step(current_step, context)
                run.step_results.append(result)

                if result.status == StepStatus.FAILED:
                    # Handle failure
                    if current_step.on_failure:
                        current_step = step_map.get(current_step.on_failure)
                    else:
                        run.status = RunStatus.FAILED
                        run.error = result.error
                        break
                else:
                    # Move to next step
                    if current_step.on_success:
                        current_step = step_map.get(current_step.on_success)
                    else:
                        current_step = None

            if run.status == RunStatus.RUNNING:
                run.status = RunStatus.COMPLETED
                run.outputs = context.step_outputs

        except Exception as e:
            run.status = RunStatus.FAILED
            run.error = str(e)

        run.completed_at = datetime.utcnow()
        return run

    async def _execute_step(self, step: WorkflowStep, context: WorkflowContext) -> StepResult:
        """Execute a single step"""
        result = StepResult(
            step_id=step.step_id,
            status=StepStatus.RUNNING
        )

        try:
            if step.step_type == StepType.WORKER:
                output = await self._execute_worker(step, context)
            elif step.step_type == StepType.CONDITION:
                output = self._evaluate_condition(step, context)
            elif step.step_type == StepType.LOOP:
                output = await self._execute_loop(step, context)
            elif step.step_type == StepType.PARALLEL:
                output = await self._execute_parallel(step, context)
            elif step.step_type == StepType.WAIT:
                output = await self._execute_wait(step, context)
            elif step.step_type == StepType.TRANSFORM:
                output = self._execute_transform(step, context)
            else:
                output = None

            result.status = StepStatus.COMPLETED
            result.output = output
            context.update_step_output(step.step_id, output)

        except Exception as e:
            result.status = StepStatus.FAILED
            result.error = str(e)

        result.completed_at = datetime.utcnow()
        result.duration = (result.completed_at - result.started_at).total_seconds()

        return result

    async def _execute_worker(self, step: WorkflowStep, context: WorkflowContext) -> Any:
        """Execute a worker step"""
        if not step.worker_id:
            raise ValueError("Worker step requires worker_id")

        handler = self._workers.get(step.worker_id)
        if not handler:
            raise ValueError(f"Worker {step.worker_id} not registered")

        # Prepare inputs from config and context
        inputs = {}
        for key, value in step.config.get("inputs", {}).items():
            if isinstance(value, str) and value.startswith("$"):
                # Variable reference
                inputs[key] = context.get(value[1:])
            else:
                inputs[key] = value

        # Execute with timeout
        try:
            result = await asyncio.wait_for(
                handler(inputs),
                timeout=step.timeout
            )
            return result
        except asyncio.TimeoutError:
            raise TimeoutError(f"Worker {step.worker_id} timed out after {step.timeout}s")

    def _evaluate_condition(self, step: WorkflowStep, context: WorkflowContext) -> bool:
        """Evaluate a condition step"""
        condition = step.condition or step.config.get("condition", "true")

        # Simple evaluation (in production, use safe eval)
        # Supports: variable comparisons
        if condition == "true":
            return True
        if condition == "false":
            return False

        # Parse simple conditions like "value > 1000"
        import re
        match = re.match(r"(\w+)\s*(==|!=|>|<|>=|<=)\s*(.+)", condition)
        if match:
            var_name, operator, value = match.groups()
            var_value = context.get(var_name)
            try:
                # Convert value to same type as variable
                if isinstance(var_value, (int, float)):
                    value = float(value)
                ops = {
                    "==": lambda a, b: a == b,
                    "!=": lambda a, b: a != b,
                    ">": lambda a, b: a > b,
                    "<": lambda a, b: a < b,
                    ">=": lambda a, b: a >= b,
                    "<=": lambda a, b: a <= b
                }
                return ops[operator](var_value, value)
            except Exception:
                return False

        return True

    async def _execute_loop(self, step: WorkflowStep, context: WorkflowContext) -> List[Any]:
        """Execute a loop step"""
        items = step.config.get("items", [])
        if isinstance(items, str) and items.startswith("$"):
            items = context.get(items[1:], [])

        results = []
        loop_step_id = step.config.get("body_step")

        for i, item in enumerate(items):
            context.set("loop_index", i)
            context.set("loop_item", item)
            # In a real implementation, would execute the body step
            results.append(item)

        return results

    async def _execute_parallel(self, step: WorkflowStep, context: WorkflowContext) -> Dict[str, Any]:
        """Execute parallel steps"""
        parallel_steps = step.config.get("steps", [])
        tasks = []

        for ps in parallel_steps:
            sub_step = WorkflowStep(
                step_id=f"{step.step_id}_{ps['id']}",
                step_type=StepType(ps.get("type", "worker")),
                name=ps.get("name", ""),
                config=ps.get("config", {}),
                worker_id=ps.get("worker_id")
            )
            tasks.append(self._execute_step(sub_step, context))

        results = await asyncio.gather(*tasks, return_exceptions=True)

        return {
            ps["id"]: r.output if isinstance(r, StepResult) else str(r)
            for ps, r in zip(parallel_steps, results)
        }

    async def _execute_wait(self, step: WorkflowStep, context: WorkflowContext) -> None:
        """Execute a wait step"""
        duration = step.config.get("duration", 1)
        await asyncio.sleep(duration)

    def _execute_transform(self, step: WorkflowStep, context: WorkflowContext) -> Any:
        """Execute a data transformation step"""
        transform_type = step.config.get("type", "passthrough")
        input_key = step.config.get("input")
        input_value = context.get(input_key) if input_key else None

        if transform_type == "passthrough":
            return input_value
        elif transform_type == "map":
            # Simple key mapping
            mapping = step.config.get("mapping", {})
            if isinstance(input_value, dict):
                return {mapping.get(k, k): v for k, v in input_value.items()}
        elif transform_type == "filter":
            # Filter list items
            if isinstance(input_value, list):
                condition = step.config.get("filter_condition")
                return [item for item in input_value if item]  # Simplified

        return input_value

    def cancel_run(self, run_id: str) -> bool:
        """Cancel a running workflow"""
        run = self._runs.get(run_id)
        if run and run.status == RunStatus.RUNNING:
            run.status = RunStatus.CANCELLED
            run.completed_at = datetime.utcnow()
            return True
        return False

    def get_runs_by_workflow(self, workflow_id: str) -> List[WorkflowRun]:
        """Get all runs for a workflow"""
        return [r for r in self._runs.values() if r.workflow_id == workflow_id]


# =============================================================================
# WORKFLOW BUILDER (Fluent API)
# =============================================================================

class WorkflowBuilder:
    """Fluent builder for creating workflows"""

    def __init__(self, name: str, project_id: str = None):
        self._workflow = Workflow(
            workflow_id=str(uuid.uuid4()),
            name=name,
            project_id=project_id
        )
        self._step_order = 0

    def description(self, desc: str) -> "WorkflowBuilder":
        self._workflow.description = desc
        return self

    def trigger(self, trigger_type: TriggerType, config: dict = None) -> "WorkflowBuilder":
        self._workflow.trigger_type = trigger_type
        self._workflow.trigger_config = config or {}
        return self

    def add_step(self, step_type: StepType, name: str, **kwargs) -> "WorkflowBuilder":
        step = WorkflowStep(
            step_id=kwargs.get("step_id", f"step_{self._step_order}"),
            step_type=step_type,
            name=name,
            **{k: v for k, v in kwargs.items() if k != "step_id"}
        )

        # Auto-link to previous step
        if self._workflow.steps and not step.on_success:
            prev_step = self._workflow.steps[-1]
            prev_step.on_success = step.step_id

        self._workflow.steps.append(step)
        self._step_order += 1
        return self

    def worker(self, name: str, worker_id: str, inputs: dict = None, **kwargs) -> "WorkflowBuilder":
        return self.add_step(
            StepType.WORKER,
            name,
            worker_id=worker_id,
            config={"inputs": inputs or {}},
            **kwargs
        )

    def condition(self, name: str, condition: str, on_true: str = None, on_false: str = None) -> "WorkflowBuilder":
        step = self.add_step(StepType.CONDITION, name, condition=condition)
        # Configure branches
        if on_true:
            self._workflow.steps[-1].on_success = on_true
        if on_false:
            self._workflow.steps[-1].on_failure = on_false
        return step

    def parallel(self, name: str, steps: List[dict]) -> "WorkflowBuilder":
        return self.add_step(
            StepType.PARALLEL,
            name,
            config={"steps": steps}
        )

    def loop(self, name: str, items: str, body_step: str) -> "WorkflowBuilder":
        return self.add_step(
            StepType.LOOP,
            name,
            config={"items": items, "body_step": body_step}
        )

    def wait(self, name: str, duration: int) -> "WorkflowBuilder":
        return self.add_step(
            StepType.WAIT,
            name,
            config={"duration": duration}
        )

    def activate(self) -> "WorkflowBuilder":
        self._workflow.status = WorkflowStatus.ACTIVE
        return self

    def build(self) -> Workflow:
        return self._workflow


# Module-level singleton
_engine = None


def get_workflow_engine() -> WorkflowEngine:
    """Get the global workflow engine instance"""
    global _engine
    if _engine is None:
        _engine = WorkflowEngine()
    return _engine
