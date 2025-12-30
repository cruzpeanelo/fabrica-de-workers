"""
Resource Limiter - CPU, Memory, and Process Limits
Issue #198: Security sandbox for worker execution

Provides resource limiting for worker execution to prevent
resource exhaustion and ensure fair resource allocation.
"""
import os
import asyncio
import logging
from typing import Optional, Dict, Any
from dataclasses import dataclass
from datetime import datetime, timedelta
from collections import defaultdict

logger = logging.getLogger(__name__)


@dataclass
class ResourceLimits:
    """Resource limits configuration"""
    # CPU limits
    max_cpu_percent: float = 50.0  # Max CPU usage percentage
    cpu_quota_seconds: int = 300  # CPU time quota per task (5 min)

    # Memory limits
    max_memory_mb: int = 512  # Max memory in MB
    memory_warning_threshold: float = 0.8  # Warn at 80% usage

    # Process limits
    max_processes: int = 10  # Max concurrent processes
    max_threads_per_process: int = 4  # Max threads per process

    # Time limits
    max_execution_time: int = 600  # Max execution time (10 min)
    idle_timeout: int = 60  # Kill idle processes after 60s

    # I/O limits
    max_file_size_mb: int = 100  # Max file size to create
    max_files_per_task: int = 50  # Max files per task
    max_output_size_kb: int = 1024  # Max stdout/stderr (1MB)

    # Network limits (for sandbox)
    allow_network: bool = False  # Disable network by default
    allowed_hosts: list = None  # Whitelist of allowed hosts

    # Rate limits
    max_tasks_per_minute: int = 10  # Rate limit tasks
    max_api_calls_per_minute: int = 60  # Rate limit API calls


@dataclass
class ResourceUsage:
    """Current resource usage tracking"""
    cpu_time_used: float = 0.0
    memory_peak_mb: float = 0.0
    files_created: int = 0
    bytes_written: int = 0
    api_calls: int = 0
    start_time: datetime = None
    last_activity: datetime = None


class ResourceLimiter:
    """
    Manages resource limits for worker execution.

    Features:
    - Per-task resource tracking
    - Rate limiting
    - Memory monitoring
    - CPU time quotas
    - Automatic cleanup of idle resources
    """

    def __init__(self, limits: Optional[ResourceLimits] = None):
        self.limits = limits or ResourceLimits()
        self._task_usage: Dict[str, ResourceUsage] = {}
        self._rate_counters: Dict[str, list] = defaultdict(list)
        self._active_processes: Dict[str, int] = {}

    def start_task(self, task_id: str) -> bool:
        """
        Start tracking resources for a task.
        Returns False if rate limit exceeded.
        """
        # Check rate limit
        if not self._check_rate_limit("tasks", self.limits.max_tasks_per_minute):
            logger.warning(f"[ResourceLimiter] Task {task_id} rate limited")
            return False

        now = datetime.now()
        self._task_usage[task_id] = ResourceUsage(
            start_time=now,
            last_activity=now
        )
        self._active_processes[task_id] = 0

        logger.info(f"[ResourceLimiter] Started tracking task {task_id}")
        return True

    def end_task(self, task_id: str) -> Optional[ResourceUsage]:
        """End tracking and return final usage"""
        usage = self._task_usage.pop(task_id, None)
        self._active_processes.pop(task_id, None)

        if usage:
            logger.info(
                f"[ResourceLimiter] Task {task_id} completed - "
                f"CPU: {usage.cpu_time_used:.2f}s, "
                f"Memory peak: {usage.memory_peak_mb:.1f}MB, "
                f"Files: {usage.files_created}"
            )

        return usage

    def check_limits(self, task_id: str) -> Dict[str, Any]:
        """
        Check if task is within limits.
        Returns dict with exceeded limits.
        """
        usage = self._task_usage.get(task_id)
        if not usage:
            return {"error": "Task not found"}

        violations = {}

        # Check execution time
        if usage.start_time:
            elapsed = (datetime.now() - usage.start_time).total_seconds()
            if elapsed > self.limits.max_execution_time:
                violations["execution_time"] = {
                    "limit": self.limits.max_execution_time,
                    "actual": elapsed
                }

        # Check CPU time
        if usage.cpu_time_used > self.limits.cpu_quota_seconds:
            violations["cpu_time"] = {
                "limit": self.limits.cpu_quota_seconds,
                "actual": usage.cpu_time_used
            }

        # Check memory
        if usage.memory_peak_mb > self.limits.max_memory_mb:
            violations["memory"] = {
                "limit": self.limits.max_memory_mb,
                "actual": usage.memory_peak_mb
            }

        # Check files
        if usage.files_created > self.limits.max_files_per_task:
            violations["files"] = {
                "limit": self.limits.max_files_per_task,
                "actual": usage.files_created
            }

        # Check idle timeout
        if usage.last_activity:
            idle_time = (datetime.now() - usage.last_activity).total_seconds()
            if idle_time > self.limits.idle_timeout:
                violations["idle_timeout"] = {
                    "limit": self.limits.idle_timeout,
                    "actual": idle_time
                }

        return violations

    def record_activity(self, task_id: str):
        """Record activity to reset idle timeout"""
        if task_id in self._task_usage:
            self._task_usage[task_id].last_activity = datetime.now()

    def record_cpu_time(self, task_id: str, seconds: float):
        """Record CPU time used"""
        if task_id in self._task_usage:
            self._task_usage[task_id].cpu_time_used += seconds
            self.record_activity(task_id)

    def record_memory(self, task_id: str, memory_mb: float):
        """Record memory usage (keeps peak)"""
        if task_id in self._task_usage:
            usage = self._task_usage[task_id]
            usage.memory_peak_mb = max(usage.memory_peak_mb, memory_mb)
            self.record_activity(task_id)

    def record_file_created(self, task_id: str, size_bytes: int = 0) -> bool:
        """
        Record file creation. Returns False if limit exceeded.
        """
        if task_id not in self._task_usage:
            return False

        usage = self._task_usage[task_id]

        # Check file count
        if usage.files_created >= self.limits.max_files_per_task:
            logger.warning(f"[ResourceLimiter] Task {task_id} file limit exceeded")
            return False

        # Check file size
        if size_bytes > self.limits.max_file_size_mb * 1024 * 1024:
            logger.warning(f"[ResourceLimiter] Task {task_id} file size limit exceeded")
            return False

        usage.files_created += 1
        usage.bytes_written += size_bytes
        self.record_activity(task_id)

        return True

    def record_api_call(self, task_id: str) -> bool:
        """
        Record API call. Returns False if rate limit exceeded.
        """
        if not self._check_rate_limit("api", self.limits.max_api_calls_per_minute):
            logger.warning(f"[ResourceLimiter] Task {task_id} API rate limit exceeded")
            return False

        if task_id in self._task_usage:
            self._task_usage[task_id].api_calls += 1
            self.record_activity(task_id)

        return True

    def can_create_process(self, task_id: str) -> bool:
        """Check if task can create another process"""
        current = self._active_processes.get(task_id, 0)
        return current < self.limits.max_processes

    def record_process_start(self, task_id: str):
        """Record process creation"""
        if task_id in self._active_processes:
            self._active_processes[task_id] += 1

    def record_process_end(self, task_id: str):
        """Record process termination"""
        if task_id in self._active_processes:
            self._active_processes[task_id] = max(0, self._active_processes[task_id] - 1)

    def _check_rate_limit(self, key: str, max_per_minute: int) -> bool:
        """Check and update rate limit counter"""
        now = datetime.now()
        cutoff = now - timedelta(minutes=1)

        # Clean old entries
        self._rate_counters[key] = [
            t for t in self._rate_counters[key] if t > cutoff
        ]

        # Check limit
        if len(self._rate_counters[key]) >= max_per_minute:
            return False

        # Record new entry
        self._rate_counters[key].append(now)
        return True

    def get_usage_report(self, task_id: str) -> Dict[str, Any]:
        """Get detailed usage report for a task"""
        usage = self._task_usage.get(task_id)
        if not usage:
            return {"error": "Task not found"}

        elapsed = 0
        if usage.start_time:
            elapsed = (datetime.now() - usage.start_time).total_seconds()

        return {
            "task_id": task_id,
            "elapsed_seconds": elapsed,
            "cpu_time_used": usage.cpu_time_used,
            "cpu_quota_remaining": max(0, self.limits.cpu_quota_seconds - usage.cpu_time_used),
            "memory_peak_mb": usage.memory_peak_mb,
            "memory_limit_mb": self.limits.max_memory_mb,
            "files_created": usage.files_created,
            "files_limit": self.limits.max_files_per_task,
            "bytes_written": usage.bytes_written,
            "api_calls": usage.api_calls,
            "active_processes": self._active_processes.get(task_id, 0),
            "limits_exceeded": self.check_limits(task_id)
        }

    async def cleanup_idle_tasks(self) -> list:
        """Cleanup idle tasks and return list of cleaned task IDs"""
        cleaned = []
        now = datetime.now()

        for task_id, usage in list(self._task_usage.items()):
            if usage.last_activity:
                idle_time = (now - usage.last_activity).total_seconds()
                if idle_time > self.limits.idle_timeout:
                    logger.info(f"[ResourceLimiter] Cleaning up idle task {task_id}")
                    self.end_task(task_id)
                    cleaned.append(task_id)

        return cleaned


# Singleton instance
_resource_limiter: Optional[ResourceLimiter] = None


def get_resource_limiter(limits: Optional[ResourceLimits] = None) -> ResourceLimiter:
    """Get or create resource limiter singleton"""
    global _resource_limiter
    if _resource_limiter is None:
        _resource_limiter = ResourceLimiter(limits)
    return _resource_limiter


class ResourceLimitExceeded(Exception):
    """Exception raised when resource limit is exceeded"""
    def __init__(self, limit_type: str, limit: Any, actual: Any):
        self.limit_type = limit_type
        self.limit = limit
        self.actual = actual
        super().__init__(f"Resource limit exceeded: {limit_type} (limit={limit}, actual={actual})")


async def with_resource_limits(
    task_id: str,
    func,
    limits: Optional[ResourceLimits] = None
):
    """
    Decorator/wrapper to execute function with resource limits.

    Usage:
        result = await with_resource_limits(
            "task-123",
            lambda: some_async_function(),
            ResourceLimits(max_execution_time=120)
        )
    """
    limiter = get_resource_limiter(limits)

    if not limiter.start_task(task_id):
        raise ResourceLimitExceeded("rate_limit", limiter.limits.max_tasks_per_minute, "exceeded")

    try:
        # Execute with timeout
        result = await asyncio.wait_for(
            func() if asyncio.iscoroutinefunction(func) else asyncio.to_thread(func),
            timeout=limiter.limits.max_execution_time
        )

        # Check for violations
        violations = limiter.check_limits(task_id)
        if violations:
            logger.warning(f"[ResourceLimiter] Task {task_id} had violations: {violations}")

        return result

    except asyncio.TimeoutError:
        raise ResourceLimitExceeded(
            "execution_time",
            limiter.limits.max_execution_time,
            "timeout"
        )
    finally:
        limiter.end_task(task_id)
