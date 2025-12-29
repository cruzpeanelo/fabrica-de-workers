# -*- coding: utf-8 -*-
"""
Email Readers Module
====================
Modulos para leitura e processamento de emails.
"""

from .inbox_reader import InboxReader, EmailFilter, ProcessedEmail
from .attachment_processor import AttachmentProcessor, ProcessedAttachment

__all__ = [
    'InboxReader',
    'EmailFilter',
    'ProcessedEmail',
    'AttachmentProcessor',
    'ProcessedAttachment'
]
