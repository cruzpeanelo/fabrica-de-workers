# -*- coding: utf-8 -*-
"""
Detailed Health Checks - Issue #205
===================================
Health checks for Kubernetes liveness and readiness probes.

Usage:
    from factory.api.health_detailed import router as health_router
    app.include_router(health_router)
"""

import os
import logging
import asyncio
from datetime import datetime
from typing import Dict, Any, Optional
from fastapi import APIRouter, HTTPException, Response

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/health", tags=["Health"])


async def check_database() -> Dict[str, Any]:
    """Check database connectivity."""
    try:
        from factory.database.connection import SessionLocal
        db = SessionLocal()
        try:
            db.execute("SELECT 1")
            return {"status": "healthy", "latency_ms": 0}
        finally:
            db.close()
    except Exception as e:
        logger.error(f"Database health check failed: {e}")
        return {"status": "unhealthy", "error": str(e)}


async def check_redis() -> Dict[str, Any]:
    """Check Redis connectivity."""
    try:
        import redis
        redis_url = os.getenv("REDIS_URL", "redis://localhost:6379")
        r = redis.from_url(redis_url, socket_connect_timeout=2)
        r.ping()
        return {"status": "healthy"}
    except ImportError:
        return {"status": "skipped", "reason": "redis not installed"}
    except Exception as e:
        logger.warning(f"Redis health check failed: {e}")
        return {"status": "unhealthy", "error": str(e)}


async def check_storage() -> Dict[str, Any]:
    """Check storage availability."""
    try:
        upload_dir = os.getenv("UPLOAD_DIR", "uploads")
        if os.path.exists(upload_dir) and os.access(upload_dir, os.W_OK):
            return {"status": "healthy", "path": upload_dir}
        return {"status": "unhealthy", "error": "Upload directory not writable"}
    except Exception as e:
        return {"status": "unhealthy", "error": str(e)}


async def check_claude_api() -> Dict[str, Any]:
    """Check Claude API key availability."""
    try:
        api_key = os.getenv("ANTHROPIC_API_KEY")
        if api_key and len(api_key) > 10:
            return {"status": "healthy", "key_present": True}
        return {"status": "degraded", "key_present": False}
    except Exception as e:
        return {"status": "unhealthy", "error": str(e)}


@router.get("/live")
async def liveness():
    """
    Liveness probe - indicates if the service is running.

    Kubernetes uses this to know when to restart a container.
    Should return quickly and only fail if the app is truly broken.
    """
    return {
        "status": "alive",
        "timestamp": datetime.utcnow().isoformat(),
        "service": "fabrica-de-agentes"
    }


@router.get("/ready")
async def readiness():
    """
    Readiness probe - indicates if the service can handle traffic.

    Kubernetes uses this to know when to send traffic to the pod.
    Checks all dependencies.
    """
    checks = {}

    # Run checks concurrently
    check_tasks = {
        "database": check_database(),
        "redis": check_redis(),
        "storage": check_storage(),
        "claude_api": check_claude_api()
    }

    results = await asyncio.gather(*check_tasks.values(), return_exceptions=True)

    for (name, _), result in zip(check_tasks.items(), results):
        if isinstance(result, Exception):
            checks[name] = {"status": "unhealthy", "error": str(result)}
        else:
            checks[name] = result

    # Determine overall status
    # Database is critical, others are not
    critical_healthy = checks.get("database", {}).get("status") == "healthy"
    all_healthy = all(
        c.get("status") in ("healthy", "skipped", "degraded")
        for c in checks.values()
    )

    response = {
        "status": "ready" if critical_healthy else "not_ready",
        "timestamp": datetime.utcnow().isoformat(),
        "checks": checks
    }

    if not critical_healthy:
        raise HTTPException(status_code=503, detail=response)

    return response


@router.get("/startup")
async def startup():
    """
    Startup probe - indicates if the app has started successfully.

    Used during container startup to know when the app is ready
    to start receiving liveness/readiness probes.
    """
    try:
        # Check if app initialized properly
        from factory.database.connection import engine
        if engine:
            return {
                "status": "started",
                "timestamp": datetime.utcnow().isoformat()
            }
    except Exception as e:
        raise HTTPException(status_code=503, detail={
            "status": "starting",
            "error": str(e)
        })

    raise HTTPException(status_code=503, detail={"status": "starting"})


@router.get("")
async def health_summary():
    """
    Complete health summary.

    Returns detailed status of all components.
    """
    checks = {}

    check_tasks = {
        "database": check_database(),
        "redis": check_redis(),
        "storage": check_storage(),
        "claude_api": check_claude_api()
    }

    results = await asyncio.gather(*check_tasks.values(), return_exceptions=True)

    for (name, _), result in zip(check_tasks.items(), results):
        if isinstance(result, Exception):
            checks[name] = {"status": "unhealthy", "error": str(result)}
        else:
            checks[name] = result

    healthy_count = sum(1 for c in checks.values() if c.get("status") == "healthy")
    total_count = len(checks)

    return {
        "status": "healthy" if healthy_count == total_count else "degraded",
        "timestamp": datetime.utcnow().isoformat(),
        "service": "fabrica-de-agentes",
        "version": os.getenv("APP_VERSION", "1.0.0"),
        "environment": os.getenv("ENVIRONMENT", "development"),
        "checks": checks,
        "summary": f"{healthy_count}/{total_count} checks healthy"
    }
