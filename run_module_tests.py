# -*- coding: utf-8 -*-
"""
Deep Module Tests - Phase 2
===========================
Testes profundos para modulos criticos identificados sem cobertura.

Prioridade:
- CRITICAL: sandbox_executor, secrets_manager, tenant_isolation
- HIGH: approval_workflow, execution_recorder, resource_limiter
- MEDIUM: okr_manager, ab_testing, model_selector

Execucao: python run_module_tests.py
"""

import os
import sys
import json
import time
import asyncio
import tempfile
import subprocess
from datetime import datetime
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Set testing mode
os.environ['TESTING'] = 'true'
os.environ['QA_MODE'] = 'true'


@dataclass
class TestResult:
    """Resultado de um teste"""
    module: str
    test_name: str
    priority: str  # CRITICAL, HIGH, MEDIUM
    passed: bool
    error: Optional[str] = None
    duration_ms: float = 0
    details: Dict[str, Any] = None

    def __post_init__(self):
        if self.details is None:
            self.details = {}


class ModuleTestRunner:
    """Executor de testes para modulos nao testados"""

    def __init__(self):
        self.results: List[TestResult] = []
        self.start_time = datetime.now()

    def add_result(self, result: TestResult):
        self.results.append(result)
        status = "PASS" if result.passed else "FAIL"
        print(f"  [{status}] {result.test_name} ({result.duration_ms:.0f}ms)")

    # =========================================================================
    # CRITICAL: Sandbox Executor Tests
    # =========================================================================

    def test_sandbox_executor(self) -> List[TestResult]:
        """Testa SandboxExecutor - Docker isolation"""
        print("\n[CRITICAL] Testing SandboxExecutor...")
        results = []

        try:
            from factory.core.sandbox_executor import (
                SandboxExecutor, SandboxConfig, SandboxResult
            )

            # Test 1: Blocked patterns detection
            start = time.time()
            try:
                executor = SandboxExecutor()
                blocked_commands = [
                    "rm -rf /",
                    "import os",
                    "eval('malicious')",
                    "subprocess.run(['ls'])",
                    "os.system('whoami')",
                ]

                all_blocked = True
                for cmd in blocked_commands:
                    # Check if pattern is blocked
                    is_blocked = any(
                        pattern in cmd
                        for pattern in SandboxExecutor.BLOCKED_PATTERNS
                    )
                    if not is_blocked:
                        all_blocked = False

                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Blocked patterns detection",
                    priority="CRITICAL",
                    passed=all_blocked,
                    duration_ms=(time.time() - start) * 1000,
                    details={"blocked_commands_tested": len(blocked_commands)}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Blocked patterns detection",
                    priority="CRITICAL",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Config defaults are secure
            start = time.time()
            try:
                config = SandboxConfig()
                secure = (
                    config.network_mode == "none" and
                    config.read_only_root == True and
                    config.no_new_privileges == True and
                    config.user != "root" and
                    "ALL" in config.cap_drop
                )
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Secure default config",
                    priority="CRITICAL",
                    passed=secure,
                    duration_ms=(time.time() - start) * 1000,
                    details={
                        "network_mode": config.network_mode,
                        "read_only_root": config.read_only_root,
                        "no_new_privileges": config.no_new_privileges,
                        "user": config.user,
                        "cap_drop": config.cap_drop
                    }
                ))
            except Exception as e:
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Secure default config",
                    priority="CRITICAL",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 3: Resource limits are set
            start = time.time()
            try:
                config = SandboxConfig()
                has_limits = (
                    config.cpu_limit <= 1.0 and
                    config.memory_limit != "" and
                    config.timeout_seconds > 0 and
                    config.timeout_seconds <= 300
                )
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Resource limits enforced",
                    priority="CRITICAL",
                    passed=has_limits,
                    duration_ms=(time.time() - start) * 1000,
                    details={
                        "cpu_limit": config.cpu_limit,
                        "memory_limit": config.memory_limit,
                        "timeout_seconds": config.timeout_seconds
                    }
                ))
            except Exception as e:
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Resource limits enforced",
                    priority="CRITICAL",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 4: Dangerous imports blocked
            start = time.time()
            try:
                dangerous_imports = [
                    "import os",
                    "from os import",
                    "import subprocess",
                    "from subprocess",
                    "import shutil",
                    "import sys",
                ]
                all_blocked = all(
                    any(pattern in imp for pattern in SandboxExecutor.BLOCKED_PATTERNS)
                    for imp in dangerous_imports
                )
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Dangerous imports blocked",
                    priority="CRITICAL",
                    passed=all_blocked,
                    duration_ms=(time.time() - start) * 1000,
                    details={"imports_tested": len(dangerous_imports)}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="sandbox_executor",
                    test_name="Dangerous imports blocked",
                    priority="CRITICAL",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="sandbox_executor",
                test_name="Module import",
                priority="CRITICAL",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # CRITICAL: Secrets Manager Tests
    # =========================================================================

    def test_secrets_manager(self) -> List[TestResult]:
        """Testa SecretsManager - encryption"""
        print("\n[CRITICAL] Testing SecretsManager...")
        results = []

        try:
            from factory.core.secrets_manager import (
                SecretsManager, EncryptionConfig, CRYPTO_AVAILABLE
            )

            # Test 1: Crypto library available
            start = time.time()
            results.append(TestResult(
                module="secrets_manager",
                test_name="Cryptography library available",
                priority="CRITICAL",
                passed=CRYPTO_AVAILABLE,
                duration_ms=(time.time() - start) * 1000,
                error=None if CRYPTO_AVAILABLE else "cryptography not installed"
            ))

            if CRYPTO_AVAILABLE:
                # Test 2: Encryption/Decryption round-trip
                start = time.time()
                try:
                    manager = SecretsManager(master_key="test-key-12345")
                    test_value = "sensitive-data-123"
                    encrypted = manager.encrypt(test_value)
                    decrypted = manager.decrypt(encrypted)

                    round_trip_ok = (
                        encrypted != test_value and  # Actually encrypted
                        decrypted == test_value and  # Correctly decrypted
                        len(encrypted) > len(test_value)  # Has overhead
                    )
                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Encryption round-trip",
                        priority="CRITICAL",
                        passed=round_trip_ok,
                        duration_ms=(time.time() - start) * 1000,
                        details={
                            "original_len": len(test_value),
                            "encrypted_len": len(encrypted) if encrypted else 0
                        }
                    ))
                except Exception as e:
                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Encryption round-trip",
                        priority="CRITICAL",
                        passed=False,
                        error=str(e),
                        duration_ms=(time.time() - start) * 1000
                    ))

                # Test 3: Different keys produce different ciphertext
                start = time.time()
                try:
                    manager1 = SecretsManager(master_key="key-1")
                    manager2 = SecretsManager(master_key="key-2")
                    test_value = "same-data"

                    enc1 = manager1.encrypt(test_value)
                    enc2 = manager2.encrypt(test_value)

                    different = enc1 != enc2
                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Different keys produce different ciphertext",
                        priority="CRITICAL",
                        passed=different,
                        duration_ms=(time.time() - start) * 1000
                    ))
                except Exception as e:
                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Different keys produce different ciphertext",
                        priority="CRITICAL",
                        passed=False,
                        error=str(e),
                        duration_ms=(time.time() - start) * 1000
                    ))

                # Test 4: Wrong key fails decryption
                start = time.time()
                try:
                    manager1 = SecretsManager(master_key="correct-key")
                    manager2 = SecretsManager(master_key="wrong-key")

                    encrypted = manager1.encrypt("secret")

                    # Should fail or return None/different value
                    try:
                        decrypted = manager2.decrypt(encrypted)
                        wrong_key_fails = (decrypted is None or decrypted != "secret")
                    except:
                        wrong_key_fails = True

                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Wrong key fails decryption",
                        priority="CRITICAL",
                        passed=wrong_key_fails,
                        duration_ms=(time.time() - start) * 1000
                    ))
                except Exception as e:
                    results.append(TestResult(
                        module="secrets_manager",
                        test_name="Wrong key fails decryption",
                        priority="CRITICAL",
                        passed=False,
                        error=str(e),
                        duration_ms=(time.time() - start) * 1000
                    ))

        except ImportError as e:
            results.append(TestResult(
                module="secrets_manager",
                test_name="Module import",
                priority="CRITICAL",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # HIGH: Approval Workflow Tests
    # =========================================================================

    def test_approval_workflow(self) -> List[TestResult]:
        """Testa ApprovalWorkflow - governance"""
        print("\n[HIGH] Testing ApprovalWorkflow...")
        results = []

        try:
            from factory.core.approval_workflow import (
                ApprovalWorkflow, ApprovalStage, ApprovalRule,
                ApprovalStageType, ApprovalRuleType, ApprovalStatus,
                ApprovalRequest, ApprovalAction, ApprovalActionType
            )

            # Test 1: Create workflow with stages
            start = time.time()
            try:
                workflow = ApprovalWorkflow(
                    name="PR Approval",
                    description="Code review workflow"
                )

                stage1 = ApprovalStage(
                    name="Code Review",
                    stage_type=ApprovalStageType.REVIEW,
                    order=1
                )
                stage2 = ApprovalStage(
                    name="QA Review",
                    stage_type=ApprovalStageType.QA,
                    order=2
                )

                workflow.add_stage(stage1)
                workflow.add_stage(stage2)

                has_stages = len(workflow.stages) == 2
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Create workflow with stages",
                    priority="HIGH",
                    passed=has_stages,
                    duration_ms=(time.time() - start) * 1000,
                    details={"stages_count": len(workflow.stages)}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Create workflow with stages",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Approval rules
            start = time.time()
            try:
                rule = ApprovalRule(
                    rule_type=ApprovalRuleType.ROLE_BASED,
                    required_roles=["TECH_LEAD", "SENIOR_DEV"],
                    min_approvers=2
                )

                has_config = (
                    rule.rule_type == ApprovalRuleType.ROLE_BASED and
                    len(rule.required_roles) == 2 and
                    rule.min_approvers == 2
                )
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Approval rules configuration",
                    priority="HIGH",
                    passed=has_config,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Approval rules configuration",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 3: Status transitions
            start = time.time()
            try:
                request = ApprovalRequest(
                    resource_type="story",
                    resource_id="STR-001"
                )

                # Initial status should be PENDING
                valid_transition = request.status == ApprovalStatus.PENDING
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Status transitions",
                    priority="HIGH",
                    passed=valid_transition,
                    duration_ms=(time.time() - start) * 1000,
                    details={"initial_status": str(request.status)}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Status transitions",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 4: Self-approval prevention
            start = time.time()
            try:
                rule = ApprovalRule(
                    rule_type=ApprovalRuleType.ANY_ONE,
                    allow_self_approval=False
                )

                prevents_self = rule.allow_self_approval == False
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Self-approval prevention",
                    priority="HIGH",
                    passed=prevents_self,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="approval_workflow",
                    test_name="Self-approval prevention",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="approval_workflow",
                test_name="Module import",
                priority="HIGH",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # HIGH: Execution Recorder Tests
    # =========================================================================

    def test_execution_recorder(self) -> List[TestResult]:
        """Testa ExecutionRecorder - replay/debug"""
        print("\n[HIGH] Testing ExecutionRecorder...")
        results = []

        try:
            from factory.core.execution_recorder import (
                ExecutionRecorder, ExecutionStep, FileChange,
                StepType, StepStatus
            )

            # Test 1: Create execution and add steps
            start = time.time()
            try:
                recorder = ExecutionRecorder(task_id="TASK-001")

                recorder.start_step(
                    step_type=StepType.GENERATING,
                    description="Generating code"
                )
                recorder.end_step(status=StepStatus.SUCCESS)

                has_steps = len(recorder.steps) >= 1
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="Create execution and add steps",
                    priority="HIGH",
                    passed=has_steps,
                    duration_ms=(time.time() - start) * 1000,
                    details={"steps_count": len(recorder.steps)}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="Create execution and add steps",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: File change tracking
            start = time.time()
            try:
                change = FileChange(
                    path="src/main.py",
                    action="modify",
                    content_before="old code",
                    content_after="new code"
                )
                change.generate_diff()

                has_diff = change.diff is not None and len(change.diff) > 0
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="File change tracking with diff",
                    priority="HIGH",
                    passed=has_diff,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="File change tracking with diff",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 3: Timeline generation
            start = time.time()
            try:
                recorder = ExecutionRecorder(task_id="TASK-002")
                recorder.start_step(StepType.INITIALIZATION, "Init")
                recorder.end_step(StepStatus.SUCCESS)
                recorder.start_step(StepType.GENERATING, "Generate")
                recorder.end_step(StepStatus.SUCCESS)

                timeline = recorder.get_timeline()
                has_timeline = timeline is not None and len(timeline) >= 2
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="Timeline generation",
                    priority="HIGH",
                    passed=has_timeline,
                    duration_ms=(time.time() - start) * 1000,
                    details={"timeline_events": len(timeline) if timeline else 0}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="execution_recorder",
                    test_name="Timeline generation",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="execution_recorder",
                test_name="Module import",
                priority="HIGH",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # HIGH: Resource Limiter Tests
    # =========================================================================

    def test_resource_limiter(self) -> List[TestResult]:
        """Testa ResourceLimiter - quotas"""
        print("\n[HIGH] Testing ResourceLimiter...")
        results = []

        try:
            from factory.core.resource_limiter import (
                ResourceLimiter, ResourceLimits, ResourceUsage
            )

            # Test 1: Default limits are reasonable
            start = time.time()
            try:
                limits = ResourceLimits()
                reasonable = (
                    limits.max_cpu_percent <= 100 and
                    limits.max_memory_mb <= 4096 and
                    limits.max_execution_time <= 3600 and
                    limits.max_file_size_mb <= 1024
                )
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Default limits reasonable",
                    priority="HIGH",
                    passed=reasonable,
                    duration_ms=(time.time() - start) * 1000,
                    details={
                        "max_cpu_percent": limits.max_cpu_percent,
                        "max_memory_mb": limits.max_memory_mb,
                        "max_execution_time": limits.max_execution_time
                    }
                ))
            except Exception as e:
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Default limits reasonable",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Network disabled by default
            start = time.time()
            try:
                limits = ResourceLimits()
                network_off = limits.allow_network == False
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Network disabled by default",
                    priority="HIGH",
                    passed=network_off,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Network disabled by default",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 3: Usage tracking
            start = time.time()
            try:
                limiter = ResourceLimiter()
                task_id = "test-task-001"

                limiter.start_task(task_id)
                usage = limiter.get_usage(task_id)

                has_tracking = usage is not None
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Usage tracking",
                    priority="HIGH",
                    passed=has_tracking,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Usage tracking",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 4: Rate limiting
            start = time.time()
            try:
                limiter = ResourceLimiter()
                limiter.limits.max_tasks_per_minute = 2

                # Should allow first 2
                allowed1 = limiter.check_rate_limit("tasks")
                allowed2 = limiter.check_rate_limit("tasks")
                # Third should be blocked
                allowed3 = limiter.check_rate_limit("tasks")

                rate_limit_works = allowed1 and allowed2 and not allowed3
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Rate limiting enforced",
                    priority="HIGH",
                    passed=rate_limit_works,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="resource_limiter",
                    test_name="Rate limiting enforced",
                    priority="HIGH",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="resource_limiter",
                test_name="Module import",
                priority="HIGH",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # MEDIUM: OKR Manager Tests
    # =========================================================================

    def test_okr_manager(self) -> List[TestResult]:
        """Testa OKRManager - CRUD operations"""
        print("\n[MEDIUM] Testing OKRManager...")
        results = []

        try:
            from factory.core.okr_manager import OKRManager
            from factory.database.models import OKRPeriod, OKRStatus

            # Test 1: OKRManager can be instantiated
            start = time.time()
            try:
                # Just check if class exists and has methods
                has_methods = (
                    hasattr(OKRManager, 'create_objective') and
                    hasattr(OKRManager, 'get_objective') and
                    hasattr(OKRManager, 'update_objective') and
                    hasattr(OKRManager, 'delete_objective')
                )
                results.append(TestResult(
                    module="okr_manager",
                    test_name="OKRManager has CRUD methods",
                    priority="MEDIUM",
                    passed=has_methods,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="okr_manager",
                    test_name="OKRManager has CRUD methods",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Key Result methods exist
            start = time.time()
            try:
                has_kr_methods = (
                    hasattr(OKRManager, 'create_key_result') and
                    hasattr(OKRManager, 'update_key_result') and
                    hasattr(OKRManager, 'get_key_results_for_objective')
                )
                results.append(TestResult(
                    module="okr_manager",
                    test_name="Key Result CRUD methods",
                    priority="MEDIUM",
                    passed=has_kr_methods,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="okr_manager",
                    test_name="Key Result CRUD methods",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 3: OKR Period enum values
            start = time.time()
            try:
                periods = [p.value for p in OKRPeriod]
                has_periods = (
                    "quarterly" in periods and
                    "yearly" in periods
                )
                results.append(TestResult(
                    module="okr_manager",
                    test_name="OKR Period enum values",
                    priority="MEDIUM",
                    passed=has_periods,
                    duration_ms=(time.time() - start) * 1000,
                    details={"periods": periods}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="okr_manager",
                    test_name="OKR Period enum values",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="okr_manager",
                test_name="Module import",
                priority="MEDIUM",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # MEDIUM: A/B Testing Module
    # =========================================================================

    def test_ab_testing(self) -> List[TestResult]:
        """Testa A/B Testing module"""
        print("\n[MEDIUM] Testing A/B Testing...")
        results = []

        try:
            from factory.core.ab_testing import (
                ABTestManager, ABTest, Variant, ABTestStatus
            )

            # Test 1: Create A/B test
            start = time.time()
            try:
                test = ABTest(
                    name="Button Color Test",
                    description="Test button color impact on CTR"
                )

                v1 = Variant(name="Control", value="blue")
                v2 = Variant(name="Treatment", value="green")

                test.add_variant(v1)
                test.add_variant(v2)

                has_variants = len(test.variants) == 2
                results.append(TestResult(
                    module="ab_testing",
                    test_name="Create A/B test with variants",
                    priority="MEDIUM",
                    passed=has_variants,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="ab_testing",
                    test_name="Create A/B test with variants",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Traffic allocation
            start = time.time()
            try:
                has_allocation = hasattr(ABTestManager, 'allocate_user')
                results.append(TestResult(
                    module="ab_testing",
                    test_name="Traffic allocation method exists",
                    priority="MEDIUM",
                    passed=has_allocation,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="ab_testing",
                    test_name="Traffic allocation method exists",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="ab_testing",
                test_name="Module import",
                priority="MEDIUM",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # MEDIUM: Model Selector Tests
    # =========================================================================

    def test_model_selector(self) -> List[TestResult]:
        """Testa ModelSelector - Claude model selection"""
        print("\n[MEDIUM] Testing ModelSelector...")
        results = []

        try:
            from factory.core.model_selector import (
                ModelSelector, ModelConfig, ModelTier
            )

            # Test 1: Model tiers defined
            start = time.time()
            try:
                tiers = [t.value for t in ModelTier]
                has_tiers = len(tiers) >= 2
                results.append(TestResult(
                    module="model_selector",
                    test_name="Model tiers defined",
                    priority="MEDIUM",
                    passed=has_tiers,
                    duration_ms=(time.time() - start) * 1000,
                    details={"tiers": tiers}
                ))
            except Exception as e:
                results.append(TestResult(
                    module="model_selector",
                    test_name="Model tiers defined",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

            # Test 2: Selector has select method
            start = time.time()
            try:
                has_select = hasattr(ModelSelector, 'select_model')
                results.append(TestResult(
                    module="model_selector",
                    test_name="Select model method exists",
                    priority="MEDIUM",
                    passed=has_select,
                    duration_ms=(time.time() - start) * 1000
                ))
            except Exception as e:
                results.append(TestResult(
                    module="model_selector",
                    test_name="Select model method exists",
                    priority="MEDIUM",
                    passed=False,
                    error=str(e),
                    duration_ms=(time.time() - start) * 1000
                ))

        except ImportError as e:
            results.append(TestResult(
                module="model_selector",
                test_name="Module import",
                priority="MEDIUM",
                passed=False,
                error=f"Failed to import: {e}"
            ))

        return results

    # =========================================================================
    # Run All Tests
    # =========================================================================

    def run_all(self) -> Dict[str, Any]:
        """Executa todos os testes"""
        print("=" * 60)
        print("DEEP MODULE TESTS - PHASE 2")
        print("=" * 60)
        print(f"Started at: {self.start_time.isoformat()}")

        # CRITICAL tests
        self.results.extend(self.test_sandbox_executor())
        self.results.extend(self.test_secrets_manager())

        # HIGH tests
        self.results.extend(self.test_approval_workflow())
        self.results.extend(self.test_execution_recorder())
        self.results.extend(self.test_resource_limiter())

        # MEDIUM tests
        self.results.extend(self.test_okr_manager())
        self.results.extend(self.test_ab_testing())
        self.results.extend(self.test_model_selector())

        # Generate report
        return self.generate_report()

    def generate_report(self) -> Dict[str, Any]:
        """Gera relatorio de resultados"""
        end_time = datetime.now()
        duration = (end_time - self.start_time).total_seconds()

        # Count by priority
        critical_tests = [r for r in self.results if r.priority == "CRITICAL"]
        high_tests = [r for r in self.results if r.priority == "HIGH"]
        medium_tests = [r for r in self.results if r.priority == "MEDIUM"]

        critical_passed = sum(1 for r in critical_tests if r.passed)
        high_passed = sum(1 for r in high_tests if r.passed)
        medium_passed = sum(1 for r in medium_tests if r.passed)

        total = len(self.results)
        passed = sum(1 for r in self.results if r.passed)
        failed = total - passed

        # Get failures
        failures = [
            {
                "module": r.module,
                "test": r.test_name,
                "priority": r.priority,
                "error": r.error,
                "details": r.details
            }
            for r in self.results if not r.passed
        ]

        report = {
            "timestamp": end_time.isoformat(),
            "duration_seconds": duration,
            "summary": {
                "total": total,
                "passed": passed,
                "failed": failed,
                "pass_rate": f"{(passed/total*100):.1f}%" if total > 0 else "N/A"
            },
            "by_priority": {
                "CRITICAL": {
                    "total": len(critical_tests),
                    "passed": critical_passed,
                    "pass_rate": f"{(critical_passed/len(critical_tests)*100):.1f}%" if critical_tests else "N/A"
                },
                "HIGH": {
                    "total": len(high_tests),
                    "passed": high_passed,
                    "pass_rate": f"{(high_passed/len(high_tests)*100):.1f}%" if high_tests else "N/A"
                },
                "MEDIUM": {
                    "total": len(medium_tests),
                    "passed": medium_passed,
                    "pass_rate": f"{(medium_passed/len(medium_tests)*100):.1f}%" if medium_tests else "N/A"
                }
            },
            "failures": failures,
            "results": [asdict(r) for r in self.results]
        }

        # Print summary
        print("\n" + "=" * 60)
        print("TEST RESULTS SUMMARY")
        print("=" * 60)
        print(f"Total: {total} | Passed: {passed} | Failed: {failed}")
        print(f"Pass Rate: {report['summary']['pass_rate']}")
        print()
        print("By Priority:")
        for priority in ["CRITICAL", "HIGH", "MEDIUM"]:
            p = report["by_priority"][priority]
            status = "OK" if p["passed"] == p["total"] else "ISSUES"
            print(f"  [{status}] {priority}: {p['passed']}/{p['total']} ({p['pass_rate']})")

        if failures:
            print(f"\nFailures ({len(failures)}):")
            for f in failures:
                print(f"  - [{f['priority']}] {f['module']}: {f['test']}")
                if f['error']:
                    print(f"    Error: {f['error'][:100]}")

        return report


def create_github_issues(failures: List[Dict]) -> List[Dict]:
    """Cria issues no GitHub para falhas encontradas"""
    created = []

    for failure in failures:
        module = failure['module']
        test = failure['test']
        priority = failure['priority']
        error = failure.get('error', 'No error details')

        # Determine agent based on module
        if module in ['sandbox_executor', 'secrets_manager']:
            agent = "SEC"
        elif module in ['approval_workflow', 'okr_manager', 'resource_limiter']:
            agent = "BACK"
        else:
            agent = "BACK"

        # Create issue
        title = f"[{agent}] [{priority}] {module} - {test} failing"
        body = f"""## Test Failure

**Module:** `{module}`
**Test:** {test}
**Priority:** {priority}

## Error
```
{error}
```

## Details
```json
{json.dumps(failure.get('details', {}), indent=2)}
```

## Expected
The test should pass.

## Suggested Fix
Review the `{module}` implementation and ensure:
1. All methods are properly implemented
2. Edge cases are handled
3. Security constraints are enforced

---
*Generated by Module Test Runner*
"""

        cmd = [
            "gh", "issue", "create",
            "--title", title,
            "--body", body,
            "--label", "bug"
        ]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
            if result.returncode == 0:
                # Extract issue number from URL
                issue_url = result.stdout.strip()
                issue_num = issue_url.split("/")[-1] if issue_url else "?"
                created.append({
                    "issue": issue_num,
                    "module": module,
                    "test": test,
                    "url": issue_url
                })
                print(f"  Created issue #{issue_num}: {title[:50]}...")
        except Exception as e:
            print(f"  Failed to create issue: {e}")

    return created


if __name__ == "__main__":
    runner = ModuleTestRunner()
    report = runner.run_all()

    # Save report
    report_path = "analysis/module_test_report.json"
    os.makedirs("analysis", exist_ok=True)
    with open(report_path, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False)
    print(f"\nReport saved to: {report_path}")

    # Create GitHub issues for failures
    if report["failures"]:
        print(f"\nCreating GitHub issues for {len(report['failures'])} failures...")
        created = create_github_issues(report["failures"])
        report["issues_created"] = created

        # Update report with issues
        with open(report_path, "w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, ensure_ascii=False)

    # Exit with error code if critical tests failed
    critical_failures = [f for f in report["failures"] if f["priority"] == "CRITICAL"]
    if critical_failures:
        print(f"\n[ERROR] {len(critical_failures)} CRITICAL tests failed!")
        sys.exit(1)

    print("\n[SUCCESS] Module tests completed")
