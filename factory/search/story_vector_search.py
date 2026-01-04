# -*- coding: utf-8 -*-
"""
Story Vector Search - Plataforma E
==================================

Vector-based semantic search for similar stories.

Issue #449: Vector Search para Stories Similares

Features:
- Semantic similarity using embeddings
- Duplicate detection
- Related story suggestions
- Similar story clustering
"""

import hashlib
import json
import math
import re
import sqlite3
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
from threading import Lock
from enum import Enum


class SimilarityMetric(str, Enum):
    """Similarity calculation methods."""
    COSINE = "cosine"
    EUCLIDEAN = "euclidean"
    DOT_PRODUCT = "dot_product"


@dataclass
class SearchConfig:
    """Configuration for vector search."""
    dimensions: int = 512
    similarity_threshold: float = 0.7
    duplicate_threshold: float = 0.95
    metric: SimilarityMetric = SimilarityMetric.COSINE
    max_results: int = 10
    include_self: bool = False


@dataclass
class StoryVector:
    """Vectorized story representation."""
    story_id: str
    title: str
    vector: List[float]
    metadata: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "story_id": self.story_id,
            "title": self.title,
            "vector_dims": len(self.vector),
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
        }


@dataclass
class SimilarityResult:
    """Result of similarity search."""
    story_id: str
    title: str
    similarity: float
    is_duplicate: bool
    match_type: str  # 'exact', 'semantic', 'partial'
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "story_id": self.story_id,
            "title": self.title,
            "similarity": round(self.similarity, 4),
            "is_duplicate": self.is_duplicate,
            "match_type": self.match_type,
            "metadata": self.metadata,
        }


class SemanticHashEncoder:
    """
    Semantic hash-based encoder for story text.
    Works without external dependencies.
    """

    def __init__(self, dimensions: int = 512, ngram_range: Tuple[int, int] = (1, 3)):
        self._dimensions = dimensions
        self.ngram_range = ngram_range

    def _tokenize(self, text: str) -> List[str]:
        """Tokenize and generate n-grams."""
        text = text.lower()
        words = re.findall(r'\b[a-zA-Z0-9_]+\b', text)

        ngrams = []
        for n in range(self.ngram_range[0], self.ngram_range[1] + 1):
            for i in range(len(words) - n + 1):
                ngram = '_'.join(words[i:i + n])
                ngrams.append(ngram)

        # Add character n-grams for subword matching
        for word in words:
            for n in range(2, min(5, len(word) + 1)):
                for i in range(len(word) - n + 1):
                    ngrams.append(f"#{word[i:i + n]}#")

        return ngrams

    def encode(self, text: str) -> List[float]:
        """Encode text to vector."""
        ngrams = self._tokenize(text)
        vector = [0.0] * self._dimensions

        for ngram in ngrams:
            h1 = int(hashlib.md5(ngram.encode()).hexdigest(), 16)
            h2 = int(hashlib.sha256(ngram.encode()).hexdigest(), 16)

            idx = h1 % self._dimensions
            sign = 1 if (h2 % 2) == 0 else -1
            weight = 1.0 + (h2 % 100) / 100.0

            vector[idx] += sign * weight

        # L2 normalize
        norm = math.sqrt(sum(v * v for v in vector))
        if norm > 0:
            vector = [v / norm for v in vector]

        return vector

    @property
    def dimensions(self) -> int:
        return self._dimensions


class StoryVectorSearch:
    """
    Vector-based search for similar stories.

    Uses semantic embeddings to find:
    - Duplicate stories
    - Similar stories
    - Related stories by topic
    """

    def __init__(
        self,
        config: Optional[SearchConfig] = None,
        db_path: Optional[Path] = None,
    ):
        self.config = config or SearchConfig()
        self.db_path = db_path or Path("factory/database/story_vectors.db")
        self._lock = Lock()
        self._encoder = SemanticHashEncoder(dimensions=self.config.dimensions)
        self._vectors: Dict[str, StoryVector] = {}

        self._init_db()
        self._load_vectors()

    def _init_db(self):
        """Initialize database for vector storage."""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS story_vectors (
                story_id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                vector TEXT NOT NULL,
                metadata TEXT,
                created_at TEXT
            )
        """)
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_title ON story_vectors(title)
        """)
        conn.commit()
        conn.close()

    def _load_vectors(self):
        """Load vectors from database."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.execute(
            "SELECT story_id, title, vector, metadata, created_at FROM story_vectors"
        )
        for row in cursor.fetchall():
            story_id, title, vector_json, metadata_json, created_at = row
            self._vectors[story_id] = StoryVector(
                story_id=story_id,
                title=title,
                vector=json.loads(vector_json),
                metadata=json.loads(metadata_json) if metadata_json else {},
                created_at=datetime.fromisoformat(created_at) if created_at else datetime.utcnow(),
            )
        conn.close()

    def _save_vector(self, story_vector: StoryVector):
        """Save vector to database."""
        conn = sqlite3.connect(self.db_path)
        conn.execute("""
            INSERT OR REPLACE INTO story_vectors
            (story_id, title, vector, metadata, created_at)
            VALUES (?, ?, ?, ?, ?)
        """, (
            story_vector.story_id,
            story_vector.title,
            json.dumps(story_vector.vector),
            json.dumps(story_vector.metadata),
            story_vector.created_at.isoformat(),
        ))
        conn.commit()
        conn.close()

    def _story_to_text(
        self,
        title: str,
        description: Optional[str] = None,
        acceptance_criteria: Optional[List[str]] = None,
        epic: Optional[str] = None,
    ) -> str:
        """Convert story fields to searchable text."""
        parts = [title]
        if description:
            parts.append(description)
        if acceptance_criteria:
            parts.extend(acceptance_criteria)
        if epic:
            parts.append(f"epic: {epic}")
        return " ".join(parts)

    def _cosine_similarity(self, vec1: List[float], vec2: List[float]) -> float:
        """Calculate cosine similarity."""
        dot = sum(a * b for a, b in zip(vec1, vec2))
        norm1 = math.sqrt(sum(a * a for a in vec1))
        norm2 = math.sqrt(sum(b * b for b in vec2))

        if norm1 == 0 or norm2 == 0:
            return 0.0

        return dot / (norm1 * norm2)

    def _euclidean_distance(self, vec1: List[float], vec2: List[float]) -> float:
        """Calculate euclidean distance (converted to similarity)."""
        dist = math.sqrt(sum((a - b) ** 2 for a, b in zip(vec1, vec2)))
        # Convert to similarity (0-1 range)
        return 1 / (1 + dist)

    def _dot_product(self, vec1: List[float], vec2: List[float]) -> float:
        """Calculate dot product similarity."""
        return sum(a * b for a, b in zip(vec1, vec2))

    def _calculate_similarity(self, vec1: List[float], vec2: List[float]) -> float:
        """Calculate similarity based on configured metric."""
        if self.config.metric == SimilarityMetric.COSINE:
            return self._cosine_similarity(vec1, vec2)
        elif self.config.metric == SimilarityMetric.EUCLIDEAN:
            return self._euclidean_distance(vec1, vec2)
        elif self.config.metric == SimilarityMetric.DOT_PRODUCT:
            return self._dot_product(vec1, vec2)
        return self._cosine_similarity(vec1, vec2)

    # Core API

    def index_story(
        self,
        story_id: str,
        title: str,
        description: Optional[str] = None,
        acceptance_criteria: Optional[List[str]] = None,
        epic: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> StoryVector:
        """Index a story for search."""
        with self._lock:
            text = self._story_to_text(title, description, acceptance_criteria, epic)
            vector = self._encoder.encode(text)

            story_vector = StoryVector(
                story_id=story_id,
                title=title,
                vector=vector,
                metadata=metadata or {},
            )

            self._vectors[story_id] = story_vector
            self._save_vector(story_vector)

            return story_vector

    def index_stories(
        self,
        stories: List[Dict[str, Any]],
    ) -> int:
        """Index multiple stories in batch."""
        count = 0
        for story in stories:
            self.index_story(
                story_id=story["story_id"],
                title=story["title"],
                description=story.get("description"),
                acceptance_criteria=story.get("acceptance_criteria"),
                epic=story.get("epic"),
                metadata=story.get("metadata"),
            )
            count += 1
        return count

    def remove_story(self, story_id: str) -> bool:
        """Remove a story from the index."""
        with self._lock:
            if story_id not in self._vectors:
                return False

            del self._vectors[story_id]

            conn = sqlite3.connect(self.db_path)
            conn.execute("DELETE FROM story_vectors WHERE story_id = ?", (story_id,))
            conn.commit()
            conn.close()

            return True

    def find_similar(
        self,
        story_id: str,
        limit: Optional[int] = None,
    ) -> List[SimilarityResult]:
        """Find stories similar to a given story."""
        if story_id not in self._vectors:
            return []

        query_vector = self._vectors[story_id]
        return self._search_by_vector(
            query_vector.vector,
            query_vector.title,
            exclude_id=story_id if not self.config.include_self else None,
            limit=limit or self.config.max_results,
        )

    def search(
        self,
        query: str,
        limit: Optional[int] = None,
    ) -> List[SimilarityResult]:
        """Search for stories by text query."""
        query_vector = self._encoder.encode(query)
        return self._search_by_vector(
            query_vector,
            query,
            limit=limit or self.config.max_results,
        )

    def _search_by_vector(
        self,
        query_vector: List[float],
        query_text: str,
        exclude_id: Optional[str] = None,
        limit: int = 10,
    ) -> List[SimilarityResult]:
        """Search using a vector."""
        results: List[SimilarityResult] = []

        for story_id, story_vector in self._vectors.items():
            if exclude_id and story_id == exclude_id:
                continue

            similarity = self._calculate_similarity(query_vector, story_vector.vector)

            if similarity >= self.config.similarity_threshold:
                is_duplicate = similarity >= self.config.duplicate_threshold
                match_type = self._determine_match_type(
                    query_text, story_vector.title, similarity
                )

                results.append(SimilarityResult(
                    story_id=story_id,
                    title=story_vector.title,
                    similarity=similarity,
                    is_duplicate=is_duplicate,
                    match_type=match_type,
                    metadata=story_vector.metadata,
                ))

        # Sort by similarity descending
        results.sort(key=lambda r: r.similarity, reverse=True)

        return results[:limit]

    def _determine_match_type(
        self,
        query: str,
        title: str,
        similarity: float,
    ) -> str:
        """Determine the type of match."""
        query_lower = query.lower().strip()
        title_lower = title.lower().strip()

        if query_lower == title_lower:
            return "exact"
        elif similarity >= 0.95:
            return "near_exact"
        elif similarity >= 0.85:
            return "semantic"
        else:
            return "partial"

    # Duplicate Detection

    def find_duplicates(
        self,
        story_id: str,
    ) -> List[SimilarityResult]:
        """Find potential duplicate stories."""
        if story_id not in self._vectors:
            return []

        query_vector = self._vectors[story_id]
        results = self._search_by_vector(
            query_vector.vector,
            query_vector.title,
            exclude_id=story_id,
            limit=20,
        )

        # Filter to only duplicates
        return [r for r in results if r.is_duplicate]

    def check_for_duplicate(
        self,
        title: str,
        description: Optional[str] = None,
        acceptance_criteria: Optional[List[str]] = None,
    ) -> Optional[SimilarityResult]:
        """Check if a new story might be a duplicate before creating."""
        text = self._story_to_text(title, description, acceptance_criteria)
        query_vector = self._encoder.encode(text)

        results = self._search_by_vector(query_vector, title, limit=1)

        if results and results[0].is_duplicate:
            return results[0]

        return None

    # Related Stories

    def find_related(
        self,
        story_id: str,
        limit: int = 5,
    ) -> List[SimilarityResult]:
        """Find related but not duplicate stories."""
        results = self.find_similar(story_id, limit=limit * 2)

        # Filter out duplicates
        related = [r for r in results if not r.is_duplicate]

        return related[:limit]

    # Clustering

    def get_clusters(
        self,
        min_similarity: float = 0.8,
        min_cluster_size: int = 2,
    ) -> List[List[str]]:
        """Group similar stories into clusters."""
        if len(self._vectors) < 2:
            return []

        story_ids = list(self._vectors.keys())
        visited = set()
        clusters: List[List[str]] = []

        for story_id in story_ids:
            if story_id in visited:
                continue

            # Find all similar stories
            cluster = [story_id]
            visited.add(story_id)

            query_vector = self._vectors[story_id].vector

            for other_id in story_ids:
                if other_id in visited:
                    continue

                other_vector = self._vectors[other_id].vector
                similarity = self._calculate_similarity(query_vector, other_vector)

                if similarity >= min_similarity:
                    cluster.append(other_id)
                    visited.add(other_id)

            if len(cluster) >= min_cluster_size:
                clusters.append(cluster)

        return clusters

    # Statistics

    def get_statistics(self) -> Dict[str, Any]:
        """Get search index statistics."""
        total_stories = len(self._vectors)

        # Find potential duplicates
        duplicate_count = 0
        for story_id in self._vectors:
            duplicates = self.find_duplicates(story_id)
            if duplicates:
                duplicate_count += 1

        return {
            "total_indexed": total_stories,
            "potential_duplicates": duplicate_count // 2,  # Each pair counted once
            "dimensions": self.config.dimensions,
            "similarity_threshold": self.config.similarity_threshold,
            "duplicate_threshold": self.config.duplicate_threshold,
            "db_path": str(self.db_path),
        }

    def get_vector(self, story_id: str) -> Optional[StoryVector]:
        """Get vector for a story."""
        return self._vectors.get(story_id)

    def clear(self):
        """Clear all indexed vectors."""
        with self._lock:
            self._vectors.clear()

            conn = sqlite3.connect(self.db_path)
            conn.execute("DELETE FROM story_vectors")
            conn.commit()
            conn.close()


# Singleton instance
_search: Optional[StoryVectorSearch] = None


def get_story_search() -> StoryVectorSearch:
    """Get global story search instance."""
    global _search
    if _search is None:
        _search = StoryVectorSearch()
    return _search
