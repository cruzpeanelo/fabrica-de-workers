# -*- coding: utf-8 -*-
"""
Teams Skills Module
===================
Skills de integracao com Microsoft Teams.
"""

from .teams_notify_skill import TeamsNotifySkill
from .teams_bot_skill import TeamsBotSkill

__all__ = [
    'TeamsNotifySkill',
    'TeamsBotSkill'
]
