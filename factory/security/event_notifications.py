# -*- coding: utf-8 -*-
"""
Security Event Notifications - Issue #359
==========================================
System for monitoring and notifying security events.

Features:
- Event types: login failures, unauthorized access, IP blocks
- Notification channels: email, webhook, dashboard
- Configurable alert rules per tenant
- Event aggregation and rate limiting
"""

import os
import json
import logging
import threading
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

WEBHOOK_TIMEOUT = 10  # seconds
MAX_EVENTS_STORED = 10000
AGGREGATION_WINDOW = 300  # 5 minutes
ALERT_COOLDOWN = 600  # 10 minutes between same alerts


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class SecurityEventType(str, Enum):
    """Types of security events."""
    # Authentication
    LOGIN_FAILED = "login_failed"
    LOGIN_SUCCESS = "login_success"
    LOGOUT = "logout"
    PASSWORD_CHANGED = "password_changed"
    PASSWORD_RESET_REQUESTED = "password_reset_requested"
    MFA_ENABLED = "mfa_enabled"
    MFA_DISABLED = "mfa_disabled"
    MFA_FAILED = "mfa_failed"

    # Authorization
    UNAUTHORIZED_ACCESS = "unauthorized_access"
    PERMISSION_DENIED = "permission_denied"
    ROLE_CHANGED = "role_changed"

    # Tokens
    TOKEN_REVOKED = "token_revoked"
    TOKEN_EXPIRED = "token_expired"
    API_KEY_CREATED = "api_key_created"
    API_KEY_REVOKED = "api_key_revoked"

    # IP/Access
    IP_BLOCKED = "ip_blocked"
    NEW_DEVICE = "new_device"
    NEW_LOCATION = "new_location"
    RATE_LIMITED = "rate_limited"

    # Data
    SENSITIVE_DATA_ACCESS = "sensitive_data_access"
    DATA_EXPORT = "data_export"
    BULK_DELETE = "bulk_delete"

    # System
    SUSPICIOUS_INPUT = "suspicious_input"
    SECURITY_CONFIG_CHANGED = "security_config_changed"


class AlertSeverity(str, Enum):
    """Severity levels for alerts."""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class NotificationChannel(str, Enum):
    """Available notification channels."""
    EMAIL = "email"
    WEBHOOK = "webhook"
    DASHBOARD = "dashboard"
    SLACK = "slack"
    TEAMS = "teams"


@dataclass
class SecurityEvent:
    """A security event record."""
    id: str
    event_type: SecurityEventType
    severity: AlertSeverity
    tenant_id: Optional[str]
    user_id: Optional[str]
    client_ip: Optional[str]
    details: Dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.utcnow)
    handled: bool = False

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "event_type": self.event_type.value,
            "severity": self.severity.value,
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "client_ip": self.client_ip,
            "details": self.details,
            "timestamp": self.timestamp.isoformat(),
            "handled": self.handled
        }


@dataclass
class AlertRule:
    """Configuration for when to send alerts."""
    event_types: List[SecurityEventType]
    min_severity: AlertSeverity = AlertSeverity.MEDIUM
    threshold: int = 1  # Trigger after N events
    window_seconds: int = 300  # Within this time window
    channels: List[NotificationChannel] = field(default_factory=lambda: [NotificationChannel.DASHBOARD])
    enabled: bool = True

    def to_dict(self) -> Dict[str, Any]:
        return {
            "event_types": [e.value for e in self.event_types],
            "min_severity": self.min_severity.value,
            "threshold": self.threshold,
            "window_seconds": self.window_seconds,
            "channels": [c.value for c in self.channels],
            "enabled": self.enabled
        }


@dataclass
class NotificationConfig:
    """Notification configuration for a tenant."""
    tenant_id: str
    email_recipients: List[str] = field(default_factory=list)
    webhook_url: Optional[str] = None
    slack_webhook: Optional[str] = None
    teams_webhook: Optional[str] = None
    rules: List[AlertRule] = field(default_factory=list)


# =============================================================================
# EVENT STORAGE
# =============================================================================

_events: List[SecurityEvent] = []
_event_counts: Dict[str, Dict[str, int]] = defaultdict(lambda: defaultdict(int))
_last_alerts: Dict[str, datetime] = {}  # key -> last alert time
_notification_configs: Dict[str, NotificationConfig] = {}
_lock = threading.Lock()


def _generate_event_id() -> str:
    """Generate unique event ID."""
    import secrets
    return f"evt_{secrets.token_hex(8)}"


# =============================================================================
# EVENT SERVICE
# =============================================================================

class SecurityEventService:
    """
    Service for handling security events.
    """

    # Severity mapping for event types
    DEFAULT_SEVERITIES = {
        SecurityEventType.LOGIN_FAILED: AlertSeverity.LOW,
        SecurityEventType.UNAUTHORIZED_ACCESS: AlertSeverity.MEDIUM,
        SecurityEventType.IP_BLOCKED: AlertSeverity.MEDIUM,
        SecurityEventType.TOKEN_REVOKED: AlertSeverity.LOW,
        SecurityEventType.PASSWORD_CHANGED: AlertSeverity.LOW,
        SecurityEventType.MFA_DISABLED: AlertSeverity.MEDIUM,
        SecurityEventType.SUSPICIOUS_INPUT: AlertSeverity.HIGH,
        SecurityEventType.SECURITY_CONFIG_CHANGED: AlertSeverity.HIGH,
        SecurityEventType.BULK_DELETE: AlertSeverity.HIGH,
    }

    @classmethod
    def emit(
        cls,
        event_type: SecurityEventType,
        tenant_id: Optional[str] = None,
        user_id: Optional[str] = None,
        client_ip: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None,
        severity: Optional[AlertSeverity] = None
    ) -> SecurityEvent:
        """
        Emit a security event.

        Event is stored and checked against alert rules.
        """
        # Determine severity
        if severity is None:
            severity = cls.DEFAULT_SEVERITIES.get(event_type, AlertSeverity.LOW)

        event = SecurityEvent(
            id=_generate_event_id(),
            event_type=event_type,
            severity=severity,
            tenant_id=tenant_id,
            user_id=user_id,
            client_ip=client_ip,
            details=details or {}
        )

        # Store event
        with _lock:
            _events.append(event)
            if len(_events) > MAX_EVENTS_STORED:
                _events.pop(0)

            # Update counts for aggregation
            key = f"{tenant_id}:{event_type.value}"
            _event_counts[key][event.timestamp.strftime("%Y%m%d%H%M")] += 1

        # Log event
        logger.info(
            f"Security event: {event_type.value} | "
            f"tenant={tenant_id} user={user_id} ip={client_ip}"
        )

        # Check alert rules
        cls._check_alerts(event)

        return event

    @classmethod
    def _check_alerts(cls, event: SecurityEvent):
        """Check if event triggers any alerts."""
        config = _notification_configs.get(event.tenant_id)
        if not config:
            # Use default rules
            config = cls._get_default_config()

        for rule in config.rules:
            if not rule.enabled:
                continue

            if event.event_type not in rule.event_types:
                continue

            if cls._severity_value(event.severity) < cls._severity_value(rule.min_severity):
                continue

            # Check threshold
            if rule.threshold > 1:
                count = cls._count_recent_events(
                    event.tenant_id,
                    event.event_type,
                    rule.window_seconds
                )
                if count < rule.threshold:
                    continue

            # Check cooldown
            alert_key = f"{event.tenant_id}:{event.event_type.value}"
            if alert_key in _last_alerts:
                last = _last_alerts[alert_key]
                if (datetime.utcnow() - last).total_seconds() < ALERT_COOLDOWN:
                    continue

            # Send alert
            cls._send_alert(event, rule, config)
            _last_alerts[alert_key] = datetime.utcnow()

    @classmethod
    def _send_alert(
        cls,
        event: SecurityEvent,
        rule: AlertRule,
        config: NotificationConfig
    ):
        """Send alert through configured channels."""
        for channel in rule.channels:
            try:
                if channel == NotificationChannel.DASHBOARD:
                    cls._notify_dashboard(event)
                elif channel == NotificationChannel.WEBHOOK and config.webhook_url:
                    cls._notify_webhook(event, config.webhook_url)
                elif channel == NotificationChannel.EMAIL and config.email_recipients:
                    cls._notify_email(event, config.email_recipients)
                elif channel == NotificationChannel.SLACK and config.slack_webhook:
                    cls._notify_slack(event, config.slack_webhook)
            except Exception as e:
                logger.error(f"Failed to send {channel.value} notification: {e}")

    @classmethod
    def _notify_dashboard(cls, event: SecurityEvent):
        """Store alert for dashboard display."""
        event.handled = True
        logger.info(f"Dashboard alert: {event.event_type.value}")

    @classmethod
    def _notify_webhook(cls, event: SecurityEvent, url: str):
        """Send webhook notification."""
        import requests
        try:
            response = requests.post(
                url,
                json=event.to_dict(),
                timeout=WEBHOOK_TIMEOUT
            )
            response.raise_for_status()
            logger.info(f"Webhook sent to {url}")
        except Exception as e:
            logger.error(f"Webhook failed: {e}")

    @classmethod
    def _notify_email(cls, event: SecurityEvent, recipients: List[str]):
        """Send email notification."""
        # In production, integrate with email service
        logger.info(f"Would send email to {recipients} for {event.event_type.value}")

    @classmethod
    def _notify_slack(cls, event: SecurityEvent, webhook_url: str):
        """Send Slack notification."""
        import requests
        try:
            severity_emoji = {
                AlertSeverity.LOW: ":information_source:",
                AlertSeverity.MEDIUM: ":warning:",
                AlertSeverity.HIGH: ":rotating_light:",
                AlertSeverity.CRITICAL: ":fire:"
            }

            payload = {
                "text": f"{severity_emoji.get(event.severity, '')} Security Alert",
                "blocks": [
                    {
                        "type": "section",
                        "text": {
                            "type": "mrkdwn",
                            "text": f"*{event.event_type.value}*\n"
                                    f"Tenant: {event.tenant_id or 'N/A'}\n"
                                    f"User: {event.user_id or 'N/A'}\n"
                                    f"IP: {event.client_ip or 'N/A'}"
                        }
                    }
                ]
            }

            response = requests.post(webhook_url, json=payload, timeout=WEBHOOK_TIMEOUT)
            response.raise_for_status()
        except Exception as e:
            logger.error(f"Slack notification failed: {e}")

    @classmethod
    def _count_recent_events(
        cls,
        tenant_id: Optional[str],
        event_type: SecurityEventType,
        window_seconds: int
    ) -> int:
        """Count events within time window."""
        cutoff = datetime.utcnow() - timedelta(seconds=window_seconds)

        with _lock:
            count = sum(
                1 for e in _events
                if e.event_type == event_type
                and e.tenant_id == tenant_id
                and e.timestamp > cutoff
            )
        return count

    @classmethod
    def _severity_value(cls, severity: AlertSeverity) -> int:
        """Get numeric value for severity comparison."""
        values = {
            AlertSeverity.LOW: 1,
            AlertSeverity.MEDIUM: 2,
            AlertSeverity.HIGH: 3,
            AlertSeverity.CRITICAL: 4
        }
        return values.get(severity, 0)

    @classmethod
    def _get_default_config(cls) -> NotificationConfig:
        """Get default notification config."""
        return NotificationConfig(
            tenant_id="default",
            rules=[
                AlertRule(
                    event_types=[
                        SecurityEventType.LOGIN_FAILED,
                        SecurityEventType.UNAUTHORIZED_ACCESS,
                        SecurityEventType.SUSPICIOUS_INPUT
                    ],
                    min_severity=AlertSeverity.MEDIUM,
                    threshold=5,
                    window_seconds=300,
                    channels=[NotificationChannel.DASHBOARD]
                ),
                AlertRule(
                    event_types=[
                        SecurityEventType.SECURITY_CONFIG_CHANGED,
                        SecurityEventType.BULK_DELETE
                    ],
                    min_severity=AlertSeverity.HIGH,
                    threshold=1,
                    channels=[NotificationChannel.DASHBOARD]
                )
            ]
        )

    # -------------------------------------------------------------------------
    # QUERY METHODS
    # -------------------------------------------------------------------------

    @classmethod
    def get_events(
        cls,
        tenant_id: Optional[str] = None,
        event_type: Optional[SecurityEventType] = None,
        severity: Optional[AlertSeverity] = None,
        since: Optional[datetime] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get security events with optional filters."""
        with _lock:
            events = list(_events)

        # Apply filters
        if tenant_id:
            events = [e for e in events if e.tenant_id == tenant_id]
        if event_type:
            events = [e for e in events if e.event_type == event_type]
        if severity:
            events = [e for e in events if e.severity == severity]
        if since:
            events = [e for e in events if e.timestamp > since]

        # Sort by timestamp descending and limit
        events.sort(key=lambda e: e.timestamp, reverse=True)
        return [e.to_dict() for e in events[:limit]]

    @classmethod
    def get_alerts(
        cls,
        tenant_id: Optional[str] = None,
        limit: int = 50
    ) -> List[Dict[str, Any]]:
        """Get alerts (high severity events)."""
        return cls.get_events(
            tenant_id=tenant_id,
            severity=AlertSeverity.HIGH,
            limit=limit
        )

    @classmethod
    def get_stats(
        cls,
        tenant_id: Optional[str] = None,
        hours: int = 24
    ) -> Dict[str, Any]:
        """Get event statistics."""
        since = datetime.utcnow() - timedelta(hours=hours)

        with _lock:
            events = [
                e for e in _events
                if e.timestamp > since
                and (tenant_id is None or e.tenant_id == tenant_id)
            ]

        # Count by type
        by_type = defaultdict(int)
        by_severity = defaultdict(int)
        for e in events:
            by_type[e.event_type.value] += 1
            by_severity[e.severity.value] += 1

        return {
            "total_events": len(events),
            "by_type": dict(by_type),
            "by_severity": dict(by_severity),
            "time_range_hours": hours
        }

    # -------------------------------------------------------------------------
    # CONFIGURATION
    # -------------------------------------------------------------------------

    @classmethod
    def configure_notifications(
        cls,
        tenant_id: str,
        config: NotificationConfig
    ):
        """Configure notifications for a tenant."""
        _notification_configs[tenant_id] = config

    @classmethod
    def get_notification_config(
        cls,
        tenant_id: str
    ) -> Optional[NotificationConfig]:
        """Get notification config for a tenant."""
        return _notification_configs.get(tenant_id)


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def emit_security_event(
    event_type: SecurityEventType,
    **kwargs
) -> SecurityEvent:
    """Emit a security event."""
    return SecurityEventService.emit(event_type, **kwargs)


def get_security_alerts(tenant_id: str = None, limit: int = 50) -> List[Dict]:
    """Get security alerts."""
    return SecurityEventService.get_alerts(tenant_id, limit)


def get_security_stats(tenant_id: str = None, hours: int = 24) -> Dict:
    """Get security statistics."""
    return SecurityEventService.get_stats(tenant_id, hours)
