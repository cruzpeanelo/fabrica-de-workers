# -*- coding: utf-8 -*-
"""
Security API Routes - Analise de Seguranca (Issue #57, #359)
=============================================================
Plataforma E

Endpoints para:
- Analise de seguranca SAST/DAST (Issue #57)
- Security Event Notifications (Issue #359)
"""
from fastapi import APIRouter, HTTPException, Request, Depends
from pydantic import BaseModel, Field
from typing import Optional, List
from datetime import datetime

from factory.core.security_analyzer import (
    SecurityAnalyzer, SecurityScanConfig, ScanType,
    scan_project, get_security_badge
)
from factory.security.event_notifications import (
    SecurityEventService,
    SecurityEventType,
    AlertSeverity,
    NotificationChannel,
    NotificationConfig,
    AlertRule,
    get_security_alerts,
    get_security_stats
)

router = APIRouter(prefix="/api", tags=["security"])


class SecurityScanRequest(BaseModel):
    use_bandit: Optional[bool] = True
    include_info: Optional[bool] = False
    scan_dependencies: Optional[bool] = False


@router.post("/projects/{project_id}/security-scan")
async def run_security_scan(project_id: str, request: Optional[SecurityScanRequest] = None):
    """
    Executa analise de seguranca SAST no projeto.

    Verifica:
    - SQL Injection, XSS, Command Injection
    - Hardcoded secrets (passwords, API keys, tokens)
    - Unsafe deserialization
    - Path traversal
    - OWASP Top 10 vulnerabilities

    Returns:
        SecurityReport com score, vulnerabilidades e recomendacoes
    """
    try:
        # Configurar scan
        scan_types = [ScanType.SAST]
        if request and request.scan_dependencies:
            scan_types.append(ScanType.SCA)

        config = SecurityScanConfig(
            scan_types=scan_types,
            use_bandit=request.use_bandit if request else True,
            include_info=request.include_info if request else False,
            verbose=False
        )

        # Executar scan
        analyzer = SecurityAnalyzer(config)
        report = await analyzer.analyze(project_id)

        return report.to_dict()

    except Exception as e:
        raise HTTPException(500, f"Erro ao executar scan de seguranca: {str(e)}")


@router.get("/projects/{project_id}/security-status")
def get_security_status(project_id: str):
    """
    Retorna status rapido de seguranca do projeto (ultimo scan ou estimativa).
    """
    return {
        "project_id": project_id,
        "has_scan": False,
        "message": "Nenhum scan de seguranca realizado ainda. Execute POST /api/projects/{id}/security-scan"
    }


@router.get("/security/badge/{project_id}")
def get_project_security_badge(project_id: str, score: int = 100):
    """
    Retorna dados do badge de seguranca para exibicao no dashboard.
    """
    badge = get_security_badge(score)
    return {
        "project_id": project_id,
        "badge": badge,
        "svg": f'''<svg xmlns="http://www.w3.org/2000/svg" width="90" height="20">
            <rect width="90" height="20" fill="{badge['color']}" rx="3"/>
            <text x="45" y="14" fill="{badge['text_color']}" font-size="11" text-anchor="middle" font-family="Arial">
                {badge['label']} {score}%
            </text>
        </svg>'''
    }


# =============================================================================
# SECURITY ALERTS - Issue #359
# =============================================================================

class AlertConfigRequest(BaseModel):
    """Request to configure security alerts."""
    email_recipients: List[str] = Field(default_factory=list)
    webhook_url: Optional[str] = None
    slack_webhook: Optional[str] = None
    teams_webhook: Optional[str] = None
    event_types: List[str] = Field(
        default_factory=lambda: ["login_failed", "unauthorized_access", "suspicious_input"]
    )
    min_severity: str = Field(default="medium", description="low, medium, high, critical")
    threshold: int = Field(default=5, ge=1, description="Number of events to trigger alert")
    window_seconds: int = Field(default=300, ge=60, description="Time window for threshold")


class AlertConfigResponse(BaseModel):
    """Response for alert configuration."""
    tenant_id: str
    configured: bool
    channels: List[str]
    event_types: List[str]


@router.get("/security/alerts")
async def get_alerts(
    request: Request,
    tenant_id: Optional[str] = None,
    severity: Optional[str] = None,
    event_type: Optional[str] = None,
    limit: int = 50
):
    """
    Get security alerts (Issue #359).

    Returns recent security events matching filters.
    """
    # Parse event_type
    event_type_enum = None
    if event_type:
        try:
            event_type_enum = SecurityEventType(event_type)
        except ValueError:
            pass

    # Parse severity
    severity_enum = None
    if severity:
        try:
            severity_enum = AlertSeverity(severity)
        except ValueError:
            pass

    events = SecurityEventService.get_events(
        tenant_id=tenant_id,
        event_type=event_type_enum,
        severity=severity_enum,
        limit=limit
    )

    return {
        "alerts": events,
        "count": len(events),
        "filters": {
            "tenant_id": tenant_id,
            "severity": severity,
            "event_type": event_type
        }
    }


@router.get("/security/alerts/stats")
async def get_alert_stats(
    tenant_id: Optional[str] = None,
    hours: int = 24
):
    """
    Get security event statistics (Issue #359).

    Returns aggregated stats for the time period.
    """
    stats = get_security_stats(tenant_id, hours)

    return {
        "stats": stats,
        "tenant_id": tenant_id,
        "generated_at": datetime.utcnow().isoformat()
    }


@router.post("/security/alerts/configure", response_model=AlertConfigResponse)
async def configure_alerts(
    request: Request,
    config: AlertConfigRequest,
    tenant_id: str = "default"
):
    """
    Configure security alert notifications (Issue #359).

    Set up email, webhook, or Slack notifications for security events.
    """
    # Parse event types
    event_types = []
    for et in config.event_types:
        try:
            event_types.append(SecurityEventType(et))
        except ValueError:
            pass

    if not event_types:
        event_types = [
            SecurityEventType.LOGIN_FAILED,
            SecurityEventType.UNAUTHORIZED_ACCESS,
            SecurityEventType.SUSPICIOUS_INPUT
        ]

    # Parse severity
    try:
        min_severity = AlertSeverity(config.min_severity)
    except ValueError:
        min_severity = AlertSeverity.MEDIUM

    # Determine channels
    channels = [NotificationChannel.DASHBOARD]
    if config.email_recipients:
        channels.append(NotificationChannel.EMAIL)
    if config.webhook_url:
        channels.append(NotificationChannel.WEBHOOK)
    if config.slack_webhook:
        channels.append(NotificationChannel.SLACK)
    if config.teams_webhook:
        channels.append(NotificationChannel.TEAMS)

    # Create alert rule
    rule = AlertRule(
        event_types=event_types,
        min_severity=min_severity,
        threshold=config.threshold,
        window_seconds=config.window_seconds,
        channels=channels
    )

    # Create notification config
    notification_config = NotificationConfig(
        tenant_id=tenant_id,
        email_recipients=config.email_recipients,
        webhook_url=config.webhook_url,
        slack_webhook=config.slack_webhook,
        teams_webhook=config.teams_webhook,
        rules=[rule]
    )

    # Save config
    SecurityEventService.configure_notifications(tenant_id, notification_config)

    return AlertConfigResponse(
        tenant_id=tenant_id,
        configured=True,
        channels=[c.value for c in channels],
        event_types=[e.value for e in event_types]
    )


@router.get("/security/alerts/config/{tenant_id}")
async def get_alert_config(tenant_id: str):
    """Get current alert configuration for a tenant."""
    config = SecurityEventService.get_notification_config(tenant_id)

    if not config:
        return {
            "tenant_id": tenant_id,
            "configured": False,
            "message": "No alert configuration found"
        }

    return {
        "tenant_id": config.tenant_id,
        "configured": True,
        "email_recipients": config.email_recipients,
        "webhook_url": config.webhook_url is not None,
        "slack_webhook": config.slack_webhook is not None,
        "teams_webhook": config.teams_webhook is not None,
        "rules": [r.to_dict() for r in config.rules]
    }


# Para uso como modulo standalone
if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI

    app = FastAPI(title="Security API")
    app.include_router(router)

    uvicorn.run(app, host="0.0.0.0", port=8001)
