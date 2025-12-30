# -*- coding: utf-8 -*-
"""
Device Manager - Issue #384
===========================
Device registration and management for mobile apps.

Features:
- Device registration with push tokens
- Device trust levels
- Device-specific security policies
- Push notification integration
"""

import secrets
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
from dataclasses import dataclass, field
from enum import Enum
import logging

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class DevicePlatform(str, Enum):
    """Supported device platforms."""
    IOS = "ios"
    ANDROID = "android"
    WEB = "web"
    DESKTOP = "desktop"


class DeviceTrustLevel(str, Enum):
    """Device trust levels."""
    UNKNOWN = "unknown"
    REGISTERED = "registered"
    VERIFIED = "verified"
    TRUSTED = "trusted"


@dataclass
class Device:
    """Registered device."""
    device_id: str
    user_id: str
    tenant_id: str
    device_name: str
    platform: DevicePlatform
    push_token: Optional[str] = None
    trust_level: DeviceTrustLevel = DeviceTrustLevel.REGISTERED
    registered_at: datetime = field(default_factory=datetime.utcnow)
    last_seen_at: datetime = field(default_factory=datetime.utcnow)
    last_ip: Optional[str] = None
    is_active: bool = True
    app_version: Optional[str] = None
    os_version: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "device_id": self.device_id,
            "user_id": self.user_id,
            "tenant_id": self.tenant_id,
            "device_name": self.device_name,
            "platform": self.platform.value,
            "trust_level": self.trust_level.value,
            "registered_at": self.registered_at.isoformat(),
            "last_seen_at": self.last_seen_at.isoformat(),
            "last_ip": self.last_ip,
            "is_active": self.is_active,
            "app_version": self.app_version,
            "os_version": self.os_version
        }


@dataclass
class DeviceActivity:
    """Device activity log entry."""
    device_id: str
    user_id: str
    activity_type: str
    ip_address: Optional[str]
    timestamp: datetime = field(default_factory=datetime.utcnow)
    details: Dict[str, Any] = field(default_factory=dict)


# =============================================================================
# STORAGE
# =============================================================================

_devices: Dict[str, Device] = {}
_user_devices: Dict[str, set] = {}  # user_id -> set of device_ids
_activity_log: List[DeviceActivity] = []


# =============================================================================
# DEVICE MANAGER SERVICE
# =============================================================================

class DeviceManager:
    """
    Service for managing user devices.
    """

    # -------------------------------------------------------------------------
    # DEVICE REGISTRATION
    # -------------------------------------------------------------------------

    @classmethod
    def register_device(
        cls,
        user_id: str,
        tenant_id: str,
        device_id: str,
        device_name: str,
        platform: str,
        push_token: Optional[str] = None,
        ip_address: Optional[str] = None,
        app_version: Optional[str] = None,
        os_version: Optional[str] = None,
        metadata: Optional[Dict] = None
    ) -> Device:
        """
        Register a new device or update existing.
        """
        # Validate platform
        try:
            platform_enum = DevicePlatform(platform.lower())
        except ValueError:
            platform_enum = DevicePlatform.WEB

        # Check if device exists
        existing = _devices.get(device_id)

        if existing:
            # Update existing device
            existing.push_token = push_token or existing.push_token
            existing.last_seen_at = datetime.utcnow()
            existing.last_ip = ip_address
            existing.app_version = app_version or existing.app_version
            existing.os_version = os_version or existing.os_version
            existing.is_active = True

            if metadata:
                existing.metadata.update(metadata)

            cls._log_activity(device_id, user_id, "device_updated", ip_address)
            logger.info(f"Updated device {device_id} for user {user_id}")

            return existing

        # Create new device
        device = Device(
            device_id=device_id,
            user_id=user_id,
            tenant_id=tenant_id,
            device_name=device_name,
            platform=platform_enum,
            push_token=push_token,
            last_ip=ip_address,
            app_version=app_version,
            os_version=os_version,
            metadata=metadata or {}
        )

        _devices[device_id] = device

        # Track by user
        if user_id not in _user_devices:
            _user_devices[user_id] = set()
        _user_devices[user_id].add(device_id)

        cls._log_activity(device_id, user_id, "device_registered", ip_address)
        logger.info(f"Registered new device {device_id} for user {user_id}")

        return device

    @classmethod
    def unregister_device(cls, device_id: str, user_id: str) -> bool:
        """Unregister a device."""
        device = _devices.get(device_id)

        if not device or device.user_id != user_id:
            return False

        device.is_active = False
        cls._log_activity(device_id, user_id, "device_unregistered", None)

        # Remove from user's devices
        if user_id in _user_devices:
            _user_devices[user_id].discard(device_id)

        logger.info(f"Unregistered device {device_id}")
        return True

    # -------------------------------------------------------------------------
    # DEVICE QUERIES
    # -------------------------------------------------------------------------

    @classmethod
    def get_device(cls, device_id: str) -> Optional[Device]:
        """Get a device by ID."""
        return _devices.get(device_id)

    @classmethod
    def get_user_devices(
        cls,
        user_id: str,
        active_only: bool = True
    ) -> List[Device]:
        """Get all devices for a user."""
        device_ids = _user_devices.get(user_id, set())
        devices = [_devices.get(did) for did in device_ids if did in _devices]

        if active_only:
            devices = [d for d in devices if d and d.is_active]

        return [d for d in devices if d]

    @classmethod
    def is_device_registered(cls, device_id: str, user_id: str) -> bool:
        """Check if a device is registered for a user."""
        device = _devices.get(device_id)
        return device is not None and device.user_id == user_id and device.is_active

    # -------------------------------------------------------------------------
    # DEVICE TRUST
    # -------------------------------------------------------------------------

    @classmethod
    def verify_device(cls, device_id: str, user_id: str) -> bool:
        """Mark a device as verified."""
        device = _devices.get(device_id)

        if not device or device.user_id != user_id:
            return False

        device.trust_level = DeviceTrustLevel.VERIFIED
        cls._log_activity(device_id, user_id, "device_verified", None)

        return True

    @classmethod
    def trust_device(cls, device_id: str, user_id: str) -> bool:
        """Mark a device as trusted."""
        device = _devices.get(device_id)

        if not device or device.user_id != user_id:
            return False

        device.trust_level = DeviceTrustLevel.TRUSTED
        cls._log_activity(device_id, user_id, "device_trusted", None)

        return True

    @classmethod
    def is_device_trusted(cls, device_id: str) -> bool:
        """Check if a device is trusted."""
        device = _devices.get(device_id)
        return device is not None and device.trust_level == DeviceTrustLevel.TRUSTED

    # -------------------------------------------------------------------------
    # PUSH NOTIFICATIONS
    # -------------------------------------------------------------------------

    @classmethod
    def update_push_token(
        cls,
        device_id: str,
        push_token: str
    ) -> bool:
        """Update push notification token for a device."""
        device = _devices.get(device_id)

        if not device:
            return False

        device.push_token = push_token
        device.last_seen_at = datetime.utcnow()

        return True

    @classmethod
    def get_user_push_tokens(cls, user_id: str) -> List[Dict[str, str]]:
        """Get all push tokens for a user's devices."""
        devices = cls.get_user_devices(user_id, active_only=True)

        return [
            {"device_id": d.device_id, "push_token": d.push_token, "platform": d.platform.value}
            for d in devices
            if d.push_token
        ]

    @classmethod
    def send_push_to_user(
        cls,
        user_id: str,
        title: str,
        body: str,
        data: Optional[Dict] = None,
        exclude_device: Optional[str] = None
    ) -> int:
        """
        Send push notification to all user's devices.

        Returns number of devices notified.
        """
        tokens = cls.get_user_push_tokens(user_id)
        count = 0

        for token_info in tokens:
            if token_info["device_id"] == exclude_device:
                continue

            # In production, integrate with FCM/APNS
            logger.info(
                f"Would send push to {token_info['platform']}: {title}"
            )
            count += 1

        return count

    # -------------------------------------------------------------------------
    # ACTIVITY LOGGING
    # -------------------------------------------------------------------------

    @classmethod
    def _log_activity(
        cls,
        device_id: str,
        user_id: str,
        activity_type: str,
        ip_address: Optional[str],
        details: Dict = None
    ):
        """Log device activity."""
        activity = DeviceActivity(
            device_id=device_id,
            user_id=user_id,
            activity_type=activity_type,
            ip_address=ip_address,
            details=details or {}
        )
        _activity_log.append(activity)

        # Keep last 10000 entries
        if len(_activity_log) > 10000:
            _activity_log.pop(0)

    @classmethod
    def record_activity(
        cls,
        device_id: str,
        ip_address: Optional[str] = None
    ):
        """Record device activity (heartbeat)."""
        device = _devices.get(device_id)

        if device:
            device.last_seen_at = datetime.utcnow()
            if ip_address:
                device.last_ip = ip_address

    @classmethod
    def get_device_activity(
        cls,
        device_id: Optional[str] = None,
        user_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get device activity log."""
        activities = _activity_log

        if device_id:
            activities = [a for a in activities if a.device_id == device_id]
        if user_id:
            activities = [a for a in activities if a.user_id == user_id]

        return [
            {
                "device_id": a.device_id,
                "user_id": a.user_id,
                "activity_type": a.activity_type,
                "ip_address": a.ip_address,
                "timestamp": a.timestamp.isoformat(),
                "details": a.details
            }
            for a in activities[-limit:]
        ]

    # -------------------------------------------------------------------------
    # CLEANUP
    # -------------------------------------------------------------------------

    @classmethod
    def cleanup_inactive_devices(cls, days: int = 90) -> int:
        """Remove devices inactive for specified days."""
        cutoff = datetime.utcnow() - timedelta(days=days)
        removed = 0

        for device_id, device in list(_devices.items()):
            if device.last_seen_at < cutoff:
                device.is_active = False
                if device.user_id in _user_devices:
                    _user_devices[device.user_id].discard(device_id)
                removed += 1

        if removed:
            logger.info(f"Cleaned up {removed} inactive devices")

        return removed


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def register_device(
    user_id: str,
    tenant_id: str,
    device_id: str,
    device_name: str,
    platform: str,
    push_token: str = None
) -> Device:
    """Register a device."""
    return DeviceManager.register_device(
        user_id, tenant_id, device_id, device_name, platform, push_token
    )


def get_user_devices(user_id: str) -> List[Device]:
    """Get user's devices."""
    return DeviceManager.get_user_devices(user_id)


def notify_user_devices(
    user_id: str,
    title: str,
    body: str,
    exclude_device: str = None
) -> int:
    """Send push to user's devices."""
    return DeviceManager.send_push_to_user(user_id, title, body, exclude_device=exclude_device)
