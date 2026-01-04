# -*- coding: utf-8 -*-
"""
Search Module - Plataforma E
============================

Vector search for similar stories.

Issue #449: Vector Search para Stories Similares
"""

from .story_vector_search import (
    StoryVectorSearch,
    StoryVector,
    SimilarityResult,
    SearchConfig,
    get_story_search,
)

__all__ = [
    "StoryVectorSearch",
    "StoryVector",
    "SimilarityResult",
    "SearchConfig",
    "get_story_search",
]
