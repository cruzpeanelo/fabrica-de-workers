# -*- coding: utf-8 -*-
"""
Audit Module - Plataforma E
===========================

Audit logging system for compliance and traceability.

Issue #439: API de Auditoria com Logs de Acoes
"""

from .models import AuditAction, AuditLog
from .audit_logger import AuditLogger, get_audit_logger

__all__ = [
    "AuditAction",
    "AuditLog",
    "AuditLogger",
    "get_audit_logger",
]
