# -*- coding: utf-8 -*-
"""
Email Senders Module
====================
Modulos para envio de emails automatizados.
"""

from .notification_sender import NotificationSender, NotificationType
from .report_sender import ReportSender, ReportType

__all__ = [
    'NotificationSender',
    'NotificationType',
    'ReportSender',
    'ReportType'
]
