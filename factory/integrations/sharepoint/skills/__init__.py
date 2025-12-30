# -*- coding: utf-8 -*-
"""
SharePoint Skills
=================
Skills para integracao com SharePoint.

Terminal 5 - Issue #298
"""

from .sharepoint_read_skill import SharePointReadSkill
from .sharepoint_sync_skill import SharePointSyncSkill

__all__ = [
    "SharePointReadSkill",
    "SharePointSyncSkill",
]
