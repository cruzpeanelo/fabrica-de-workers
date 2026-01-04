# -*- coding: utf-8 -*-
"""
Comments Module - Plataforma E
==============================

Real-time commenting system for stories.

Issue #440: Componente de Comentarios e Discussoes
"""

from .comment_manager import (
    Comment,
    Reaction,
    CommentManager,
    get_comment_manager,
)

__all__ = [
    "Comment",
    "Reaction",
    "CommentManager",
    "get_comment_manager",
]
