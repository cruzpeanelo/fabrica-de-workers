# -*- coding: utf-8 -*-
"""
Health Check - Plataforma E
===========================

Health checking system for monitoring platform components.

Issue #442: Health Check Endpoints Avancados
"""

import os
import time
import psutil
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional


class HealthStatus(str, Enum):
    """Health status values."""
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    UNHEALTHY = "unhealthy"
    UNKNOWN = "unknown"


@dataclass
class ComponentHealth:
    """Health status of a single component."""
    name: str
    status: HealthStatus
    latency_ms: float = 0.0
    message: Optional[str] = None
    details: Dict[str, Any] = field(default_factory=dict)
    checked_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        result = {
            "status": self.status.value,
            "latency_ms": round(self.latency_ms, 2),
        }
        if self.message:
            result["message"] = self.message
        if self.details:
            result.update(self.details)
        return result


@dataclass
class HealthReport:
    """Complete health report."""
    status: HealthStatus
    components: Dict[str, ComponentHealth] = field(default_factory=dict)
    version: str = "6.5.0"
    uptime_seconds: float = 0.0
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "status": self.status.value,
            "components": {
                name: comp.to_dict()
                for name, comp in self.components.items()
            },
            "version": self.version,
            "uptime_seconds": round(self.uptime_seconds, 2),
            "timestamp": self.timestamp.isoformat(),
        }


class HealthChecker:
    """Main health checking class."""

    def __init__(self):
        self._start_time = time.time()
        self._checks: Dict[str, Callable[[], ComponentHealth]] = {}
        self._register_default_checks()

    def _register_default_checks(self):
        """Register default health checks."""
        self.register_check("system", self._check_system)
        self.register_check("memory", self._check_memory)
        self.register_check("disk", self._check_disk)
        self.register_check("database", self._check_database)
        self.register_check("cpu", self._check_cpu)

    def register_check(self, name: str, check_fn: Callable[[], ComponentHealth]) -> None:
        """Register a health check function."""
        self._checks[name] = check_fn

    def unregister_check(self, name: str) -> bool:
        """Unregister a health check."""
        if name in self._checks:
            del self._checks[name]
            return True
        return False

    @property
    def uptime_seconds(self) -> float:
        """Get uptime in seconds."""
        return time.time() - self._start_time

    def check_basic(self) -> Dict[str, Any]:
        """Perform basic health check (fast, for load balancers)."""
        return {
            "status": "healthy",
            "timestamp": datetime.now().isoformat(),
        }

    def check_detailed(self) -> HealthReport:
        """Perform detailed health check of all components."""
        components: Dict[str, ComponentHealth] = {}
        overall_status = HealthStatus.HEALTHY

        for name, check_fn in self._checks.items():
            try:
                start = time.time()
                health = check_fn()
                health.latency_ms = (time.time() - start) * 1000
                components[name] = health

                # Determine overall status
                if health.status == HealthStatus.UNHEALTHY:
                    overall_status = HealthStatus.UNHEALTHY
                elif health.status == HealthStatus.DEGRADED and overall_status == HealthStatus.HEALTHY:
                    overall_status = HealthStatus.DEGRADED

            except Exception as e:
                components[name] = ComponentHealth(
                    name=name,
                    status=HealthStatus.UNKNOWN,
                    message=str(e),
                )
                overall_status = HealthStatus.DEGRADED

        return HealthReport(
            status=overall_status,
            components=components,
            uptime_seconds=self.uptime_seconds,
        )

    def check_readiness(self) -> Dict[str, Any]:
        """Check if service is ready to receive traffic (Kubernetes readiness probe)."""
        report = self.check_detailed()

        # Ready if healthy or degraded (still accepting traffic)
        ready = report.status in (HealthStatus.HEALTHY, HealthStatus.DEGRADED)

        return {
            "ready": ready,
            "status": report.status.value,
            "checks_passed": sum(
                1 for c in report.components.values()
                if c.status in (HealthStatus.HEALTHY, HealthStatus.DEGRADED)
            ),
            "checks_total": len(report.components),
        }

    def check_liveness(self) -> Dict[str, Any]:
        """Check if service is alive (Kubernetes liveness probe)."""
        # Simple check - if we can respond, we're alive
        return {
            "alive": True,
            "uptime_seconds": round(self.uptime_seconds, 2),
            "timestamp": datetime.now().isoformat(),
        }

    def is_healthy(self) -> bool:
        """Quick check if system is healthy."""
        report = self.check_detailed()
        return report.status == HealthStatus.HEALTHY

    # Default check implementations

    def _check_system(self) -> ComponentHealth:
        """Check overall system status."""
        try:
            load_avg = psutil.getloadavg() if hasattr(psutil, "getloadavg") else (0, 0, 0)
            cpu_count = psutil.cpu_count()

            # High load = degraded
            load_1min = load_avg[0]
            status = HealthStatus.HEALTHY
            if cpu_count and load_1min > cpu_count * 0.8:
                status = HealthStatus.DEGRADED
            elif cpu_count and load_1min > cpu_count:
                status = HealthStatus.UNHEALTHY

            return ComponentHealth(
                name="system",
                status=status,
                details={
                    "load_average": list(load_avg),
                    "cpu_count": cpu_count,
                    "platform": os.name,
                },
            )
        except Exception as e:
            return ComponentHealth(
                name="system",
                status=HealthStatus.UNKNOWN,
                message=str(e),
            )

    def _check_memory(self) -> ComponentHealth:
        """Check memory usage."""
        try:
            mem = psutil.virtual_memory()
            used_percent = mem.percent

            status = HealthStatus.HEALTHY
            if used_percent > 90:
                status = HealthStatus.UNHEALTHY
            elif used_percent > 80:
                status = HealthStatus.DEGRADED

            return ComponentHealth(
                name="memory",
                status=status,
                details={
                    "used_percent": round(used_percent, 1),
                    "available_gb": round(mem.available / (1024**3), 2),
                    "total_gb": round(mem.total / (1024**3), 2),
                },
            )
        except Exception as e:
            return ComponentHealth(
                name="memory",
                status=HealthStatus.UNKNOWN,
                message=str(e),
            )

    def _check_disk(self) -> ComponentHealth:
        """Check disk usage."""
        try:
            disk = psutil.disk_usage("/")
            used_percent = disk.percent
            free_gb = disk.free / (1024**3)

            status = HealthStatus.HEALTHY
            if used_percent > 95 or free_gb < 1:
                status = HealthStatus.UNHEALTHY
            elif used_percent > 85 or free_gb < 5:
                status = HealthStatus.DEGRADED

            return ComponentHealth(
                name="disk",
                status=status,
                details={
                    "used_percent": round(used_percent, 1),
                    "free_gb": round(free_gb, 2),
                    "total_gb": round(disk.total / (1024**3), 2),
                },
            )
        except Exception as e:
            return ComponentHealth(
                name="disk",
                status=HealthStatus.UNKNOWN,
                message=str(e),
            )

    def _check_database(self) -> ComponentHealth:
        """Check database connectivity."""
        try:
            # Try to import and check database
            from factory.database.connection import get_db_engine

            start = time.time()
            engine = get_db_engine()

            # Simple query to check connectivity
            with engine.connect() as conn:
                conn.execute("SELECT 1")

            latency = (time.time() - start) * 1000

            status = HealthStatus.HEALTHY
            if latency > 1000:
                status = HealthStatus.DEGRADED
            elif latency > 5000:
                status = HealthStatus.UNHEALTHY

            return ComponentHealth(
                name="database",
                status=status,
                latency_ms=latency,
                details={"type": "sqlite"},
            )
        except ImportError:
            return ComponentHealth(
                name="database",
                status=HealthStatus.HEALTHY,
                message="Database module not loaded",
                details={"type": "sqlite", "mock": True},
            )
        except Exception as e:
            return ComponentHealth(
                name="database",
                status=HealthStatus.UNHEALTHY,
                message=str(e),
            )

    def _check_cpu(self) -> ComponentHealth:
        """Check CPU usage."""
        try:
            cpu_percent = psutil.cpu_percent(interval=0.1)

            status = HealthStatus.HEALTHY
            if cpu_percent > 95:
                status = HealthStatus.UNHEALTHY
            elif cpu_percent > 80:
                status = HealthStatus.DEGRADED

            return ComponentHealth(
                name="cpu",
                status=status,
                details={
                    "usage_percent": round(cpu_percent, 1),
                    "count": psutil.cpu_count(),
                    "count_logical": psutil.cpu_count(logical=True),
                },
            )
        except Exception as e:
            return ComponentHealth(
                name="cpu",
                status=HealthStatus.UNKNOWN,
                message=str(e),
            )


# Singleton instance
_checker: Optional[HealthChecker] = None


def get_health_checker() -> HealthChecker:
    """Get global health checker instance."""
    global _checker
    if _checker is None:
        _checker = HealthChecker()
    return _checker
