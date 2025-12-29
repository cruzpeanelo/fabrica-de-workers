# -*- coding: utf-8 -*-
"""
Email Skills Module
===================
Skills de email para uso com agentes Claude.
"""

from .email_send_skill import EmailSendSkill
from .email_read_skill import EmailReadSkill

__all__ = [
    'EmailSendSkill',
    'EmailReadSkill'
]
