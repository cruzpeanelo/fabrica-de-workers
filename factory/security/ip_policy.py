# -*- coding: utf-8 -*-
"""
IP Policy and Geo-blocking - Issue #343
========================================
IP whitelisting and geo-blocking per tenant.

Features:
- IP whitelist/blacklist per tenant
- CIDR range support
- Geo-blocking by country code
- Bypass for super_admin
- Block logging and notifications
"""

import ipaddress
import os
from datetime import datetime
from typing import Optional, List, Dict, Any, Set
from dataclasses import dataclass, field
from enum import Enum

# GeoIP lookup (optional - install geoip2 for production)
try:
    import geoip2.database
    import geoip2.errors
    GEOIP_AVAILABLE = True
except ImportError:
    GEOIP_AVAILABLE = False


# =============================================================================
# CONFIGURATION
# =============================================================================

GEOIP_DB_PATH = os.getenv("GEOIP_DB_PATH", "/usr/share/GeoIP/GeoLite2-Country.mmdb")
SUPER_ADMIN_BYPASS = True  # Super admins bypass IP restrictions


# =============================================================================
# ENUMS AND MODELS
# =============================================================================

class IPPolicyMode(str, Enum):
    """IP policy enforcement mode."""
    WHITELIST = "whitelist"  # Only allow listed IPs
    BLACKLIST = "blacklist"  # Block listed IPs
    DISABLED = "disabled"    # No IP restrictions


class BlockReason(str, Enum):
    """Reason for blocking a request."""
    IP_NOT_WHITELISTED = "ip_not_whitelisted"
    IP_BLACKLISTED = "ip_blacklisted"
    COUNTRY_BLOCKED = "country_blocked"
    COUNTRY_NOT_ALLOWED = "country_not_allowed"


@dataclass
class TenantIPPolicy:
    """IP policy configuration for a tenant."""
    tenant_id: str
    mode: IPPolicyMode = IPPolicyMode.DISABLED
    ip_ranges: List[str] = field(default_factory=list)  # CIDR notation
    allowed_countries: List[str] = field(default_factory=list)  # ISO 3166-1 alpha-2
    blocked_countries: List[str] = field(default_factory=list)
    enforce_on_admin_only: bool = False
    notify_on_block: bool = True
    bypass_for_api_keys: bool = False
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "tenant_id": self.tenant_id,
            "mode": self.mode.value,
            "ip_ranges": self.ip_ranges,
            "allowed_countries": self.allowed_countries,
            "blocked_countries": self.blocked_countries,
            "enforce_on_admin_only": self.enforce_on_admin_only,
            "notify_on_block": self.notify_on_block,
            "bypass_for_api_keys": self.bypass_for_api_keys,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat()
        }


@dataclass
class BlockedAttempt:
    """Record of a blocked access attempt."""
    tenant_id: str
    client_ip: str
    country_code: Optional[str]
    reason: BlockReason
    user_agent: Optional[str]
    path: str
    timestamp: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "client_ip": self.client_ip,
            "country_code": self.country_code,
            "reason": self.reason.value,
            "user_agent": self.user_agent,
            "path": self.path,
            "timestamp": self.timestamp.isoformat()
        }


# =============================================================================
# STORAGE (In-memory for now)
# =============================================================================

_ip_policies: Dict[str, TenantIPPolicy] = {}
_blocked_attempts: List[BlockedAttempt] = []


# =============================================================================
# GEOIP SERVICE
# =============================================================================

class GeoIPService:
    """Service for IP geolocation."""

    _reader = None

    @classmethod
    def get_country(cls, ip_address: str) -> Optional[str]:
        """
        Get country code for an IP address.

        Returns ISO 3166-1 alpha-2 country code or None.
        """
        if not GEOIP_AVAILABLE:
            return None

        try:
            if cls._reader is None:
                if os.path.exists(GEOIP_DB_PATH):
                    cls._reader = geoip2.database.Reader(GEOIP_DB_PATH)
                else:
                    return None

            response = cls._reader.country(ip_address)
            return response.country.iso_code

        except (geoip2.errors.AddressNotFoundError, ValueError):
            return None
        except Exception:
            return None

    @classmethod
    def is_available(cls) -> bool:
        """Check if GeoIP is available."""
        return GEOIP_AVAILABLE and os.path.exists(GEOIP_DB_PATH)


# =============================================================================
# IP POLICY SERVICE
# =============================================================================

class IPPolicyService:
    """
    Service for managing IP policies and checking access.
    """

    def __init__(self, tenant_id: Optional[str] = None):
        self.tenant_id = tenant_id

    # -------------------------------------------------------------------------
    # POLICY CRUD
    # -------------------------------------------------------------------------

    def get_policy(self, tenant_id: str) -> Optional[TenantIPPolicy]:
        """Get IP policy for a tenant."""
        return _ip_policies.get(tenant_id)

    def create_or_update_policy(
        self,
        tenant_id: str,
        mode: IPPolicyMode = IPPolicyMode.DISABLED,
        ip_ranges: Optional[List[str]] = None,
        allowed_countries: Optional[List[str]] = None,
        blocked_countries: Optional[List[str]] = None,
        enforce_on_admin_only: bool = False,
        notify_on_block: bool = True,
        bypass_for_api_keys: bool = False
    ) -> TenantIPPolicy:
        """Create or update IP policy for a tenant."""
        # Validate IP ranges
        validated_ranges = []
        if ip_ranges:
            for ip_range in ip_ranges:
                try:
                    ipaddress.ip_network(ip_range, strict=False)
                    validated_ranges.append(ip_range)
                except ValueError:
                    pass  # Skip invalid ranges

        # Validate country codes (basic check)
        valid_countries = lambda codes: [
            c.upper() for c in (codes or [])
            if len(c) == 2 and c.isalpha()
        ]

        policy = TenantIPPolicy(
            tenant_id=tenant_id,
            mode=mode,
            ip_ranges=validated_ranges,
            allowed_countries=valid_countries(allowed_countries),
            blocked_countries=valid_countries(blocked_countries),
            enforce_on_admin_only=enforce_on_admin_only,
            notify_on_block=notify_on_block,
            bypass_for_api_keys=bypass_for_api_keys,
            updated_at=datetime.utcnow()
        )

        _ip_policies[tenant_id] = policy
        return policy

    def delete_policy(self, tenant_id: str) -> bool:
        """Delete IP policy for a tenant."""
        if tenant_id in _ip_policies:
            del _ip_policies[tenant_id]
            return True
        return False

    # -------------------------------------------------------------------------
    # ACCESS CHECKING
    # -------------------------------------------------------------------------

    def check_access(
        self,
        tenant_id: str,
        client_ip: str,
        is_admin: bool = False,
        is_super_admin: bool = False,
        is_api_key: bool = False,
        path: str = "",
        user_agent: str = ""
    ) -> tuple[bool, Optional[BlockReason], Optional[str]]:
        """
        Check if an IP is allowed for a tenant.

        Returns:
            (allowed, block_reason, message)
        """
        # Super admin bypass
        if is_super_admin and SUPER_ADMIN_BYPASS:
            return True, None, None

        # Get policy
        policy = self.get_policy(tenant_id)
        if not policy or policy.mode == IPPolicyMode.DISABLED:
            return True, None, None

        # Admin-only enforcement
        if policy.enforce_on_admin_only and not is_admin:
            return True, None, None

        # API key bypass
        if policy.bypass_for_api_keys and is_api_key:
            return True, None, None

        # Check IP
        allowed, reason = self._check_ip(client_ip, policy)

        if not allowed:
            # Get country for logging
            country = GeoIPService.get_country(client_ip)

            # Log blocked attempt
            self._log_blocked_attempt(
                tenant_id=tenant_id,
                client_ip=client_ip,
                country_code=country,
                reason=reason,
                user_agent=user_agent,
                path=path
            )

            message = self._get_block_message(reason, client_ip, country)
            return False, reason, message

        return True, None, None

    def _check_ip(
        self,
        client_ip: str,
        policy: TenantIPPolicy
    ) -> tuple[bool, Optional[BlockReason]]:
        """Check IP against policy rules."""
        try:
            ip = ipaddress.ip_address(client_ip)
        except ValueError:
            return False, BlockReason.IP_NOT_WHITELISTED

        # Check IP ranges
        ip_match = self._ip_in_ranges(ip, policy.ip_ranges)

        if policy.mode == IPPolicyMode.WHITELIST:
            if not ip_match and policy.ip_ranges:
                return False, BlockReason.IP_NOT_WHITELISTED

        elif policy.mode == IPPolicyMode.BLACKLIST:
            if ip_match:
                return False, BlockReason.IP_BLACKLISTED

        # Check country
        country = GeoIPService.get_country(client_ip)

        if country:
            if policy.blocked_countries and country in policy.blocked_countries:
                return False, BlockReason.COUNTRY_BLOCKED

            if policy.allowed_countries and country not in policy.allowed_countries:
                return False, BlockReason.COUNTRY_NOT_ALLOWED

        return True, None

    def _ip_in_ranges(
        self,
        ip: ipaddress.IPv4Address | ipaddress.IPv6Address,
        ranges: List[str]
    ) -> bool:
        """Check if IP is in any of the specified ranges."""
        for range_str in ranges:
            try:
                network = ipaddress.ip_network(range_str, strict=False)
                if ip in network:
                    return True
            except ValueError:
                continue
        return False

    def _get_block_message(
        self,
        reason: BlockReason,
        client_ip: str,
        country: Optional[str]
    ) -> str:
        """Get user-friendly block message."""
        messages = {
            BlockReason.IP_NOT_WHITELISTED: f"Access denied: IP {client_ip} is not in the allowed list",
            BlockReason.IP_BLACKLISTED: f"Access denied: IP {client_ip} is blocked",
            BlockReason.COUNTRY_BLOCKED: f"Access denied: Country {country} is blocked",
            BlockReason.COUNTRY_NOT_ALLOWED: f"Access denied: Country {country} is not in the allowed list"
        }
        return messages.get(reason, "Access denied")

    # -------------------------------------------------------------------------
    # LOGGING
    # -------------------------------------------------------------------------

    def _log_blocked_attempt(
        self,
        tenant_id: str,
        client_ip: str,
        country_code: Optional[str],
        reason: BlockReason,
        user_agent: str,
        path: str
    ):
        """Log a blocked access attempt."""
        attempt = BlockedAttempt(
            tenant_id=tenant_id,
            client_ip=client_ip,
            country_code=country_code,
            reason=reason,
            user_agent=user_agent,
            path=path
        )
        _blocked_attempts.append(attempt)

        # Keep only last 10000 entries
        if len(_blocked_attempts) > 10000:
            _blocked_attempts.pop(0)

    def get_blocked_attempts(
        self,
        tenant_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get blocked attempt logs."""
        attempts = _blocked_attempts
        if tenant_id:
            attempts = [a for a in attempts if a.tenant_id == tenant_id]
        return [a.to_dict() for a in attempts[-limit:]]

    # -------------------------------------------------------------------------
    # TEST UTILITIES
    # -------------------------------------------------------------------------

    def test_ip(
        self,
        tenant_id: str,
        test_ip: str
    ) -> Dict[str, Any]:
        """Test if an IP would be allowed."""
        policy = self.get_policy(tenant_id)

        if not policy:
            return {
                "ip": test_ip,
                "allowed": True,
                "reason": "No policy configured"
            }

        country = GeoIPService.get_country(test_ip)
        allowed, reason = self._check_ip(test_ip, policy)

        return {
            "ip": test_ip,
            "country": country,
            "allowed": allowed,
            "block_reason": reason.value if reason else None,
            "policy_mode": policy.mode.value
        }


# =============================================================================
# SINGLETON ACCESSOR
# =============================================================================

_service_instance: Optional[IPPolicyService] = None


def get_ip_policy_service(tenant_id: Optional[str] = None) -> IPPolicyService:
    """Get IP policy service instance."""
    global _service_instance
    if _service_instance is None:
        _service_instance = IPPolicyService()
    if tenant_id:
        return IPPolicyService(tenant_id)
    return _service_instance
