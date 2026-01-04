# -*- coding: utf-8 -*-
"""
Tags Module - Plataforma E
==========================

Tag and category management for stories.

Issue #441: Sistema de Tags e Categorias
"""

from .models import Tag, StoryTag, Category
from .tag_manager import TagManager, get_tag_manager

__all__ = [
    "Tag",
    "StoryTag",
    "Category",
    "TagManager",
    "get_tag_manager",
]
