# -*- coding: utf-8 -*-
"""
Story Points Estimator - Plataforma E v6.5
==========================================

Intelligent story point estimation based on historical data
and text analysis. Uses heuristics and can be extended with ML.

Issue #245: [INOV] Estimativas inteligentes com Machine Learning
"""

import re
import math
from datetime import datetime
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum


# =============================================================================
# ENUMS
# =============================================================================

class ConfidenceLevel(str, Enum):
    """Confidence level of estimation"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"


class ComplexityIndicator(str, Enum):
    """Indicators of story complexity"""
    INTEGRATION = "integration"    # External system integration
    DATABASE = "database"          # Database changes
    SECURITY = "security"          # Security requirements
    UI = "ui"                      # UI/UX work
    API = "api"                    # API development
    REFACTOR = "refactor"          # Code refactoring
    NEW_FEATURE = "new_feature"    # New functionality
    BUG_FIX = "bug_fix"            # Bug fixes
    TESTING = "testing"            # Testing requirements


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class EstimationResult:
    """Result of story points estimation"""
    suggested_points: int
    confidence: ConfidenceLevel
    confidence_score: float  # 0.0 to 1.0
    range_low: int
    range_high: int
    complexity_indicators: List[ComplexityIndicator] = field(default_factory=list)
    similar_stories: List[str] = field(default_factory=list)
    reasoning: str = ""

    def to_dict(self) -> dict:
        return {
            "suggested_points": self.suggested_points,
            "confidence": self.confidence.value,
            "confidence_score": self.confidence_score,
            "range": {"low": self.range_low, "high": self.range_high},
            "complexity_indicators": [c.value for c in self.complexity_indicators],
            "similar_stories": self.similar_stories,
            "reasoning": self.reasoning
        }


@dataclass
class HistoricalStory:
    """Historical story data for learning"""
    story_id: str
    title: str
    description: str
    estimated_points: int
    actual_points: Optional[int] = None
    actual_hours: Optional[float] = None
    complexity: Optional[str] = None
    completed_at: Optional[datetime] = None
    tags: List[str] = field(default_factory=list)


@dataclass
class TeamVelocity:
    """Team velocity statistics"""
    avg_velocity: float
    std_dev: float
    sprints_analyzed: int
    points_per_hour: float = 0.25  # Default: 4 hours per point


# =============================================================================
# COMPLEXITY KEYWORDS
# =============================================================================

COMPLEXITY_KEYWORDS = {
    ComplexityIndicator.INTEGRATION: [
        "integrar", "integration", "api externa", "external", "terceiro",
        "third-party", "webhook", "oauth", "sso", "sync"
    ],
    ComplexityIndicator.DATABASE: [
        "database", "banco de dados", "migration", "schema", "table",
        "query", "sql", "index", "relationship", "foreign key"
    ],
    ComplexityIndicator.SECURITY: [
        "security", "seguranca", "auth", "autenticacao", "permission",
        "encrypt", "token", "credential", "password", "rbac", "acl"
    ],
    ComplexityIndicator.UI: [
        "frontend", "ui", "ux", "design", "layout", "component",
        "responsive", "mobile", "css", "style", "animation"
    ],
    ComplexityIndicator.API: [
        "api", "endpoint", "rest", "graphql", "route", "controller",
        "request", "response", "json", "payload"
    ],
    ComplexityIndicator.REFACTOR: [
        "refactor", "refatorar", "cleanup", "optimize", "performance",
        "legacy", "technical debt", "rewrite", "reorganize"
    ],
    ComplexityIndicator.NEW_FEATURE: [
        "novo", "new", "criar", "create", "implement", "add",
        "funcionalidade", "feature", "capability"
    ],
    ComplexityIndicator.BUG_FIX: [
        "bug", "fix", "corrigir", "error", "issue", "problem",
        "broken", "failing", "crash", "exception"
    ],
    ComplexityIndicator.TESTING: [
        "test", "teste", "qa", "coverage", "unit", "integration",
        "e2e", "regression", "automation"
    ]
}

# Fibonacci sequence for story points
FIBONACCI = [1, 2, 3, 5, 8, 13, 21, 34]


# =============================================================================
# STORY POINTS ESTIMATOR
# =============================================================================

class StoryPointsEstimator:
    """
    Intelligent story points estimator.

    Uses text analysis and historical data to suggest story points.
    Can be extended with ML models for better accuracy.
    """

    def __init__(self, db_session=None):
        self.db = db_session
        self._historical_stories: List[HistoricalStory] = []
        self._team_velocity: Optional[TeamVelocity] = None
        self._feedback_data: List[Dict] = []

    def load_historical_data(self, stories: List[HistoricalStory]):
        """Load historical stories for analysis"""
        self._historical_stories = stories

    def set_team_velocity(self, velocity: TeamVelocity):
        """Set team velocity statistics"""
        self._team_velocity = velocity

    def estimate(self, title: str, description: str = "",
                 tags: List[str] = None) -> EstimationResult:
        """
        Estimate story points for a given story.

        Args:
            title: Story title
            description: Story description
            tags: Optional list of tags

        Returns:
            EstimationResult with suggested points and confidence
        """
        # Combine text for analysis
        full_text = f"{title} {description}".lower()
        tags = tags or []

        # Detect complexity indicators
        indicators = self._detect_complexity(full_text, tags)

        # Calculate base points from text length and complexity
        base_points = self._calculate_base_points(full_text, indicators)

        # Find similar historical stories
        similar = self._find_similar_stories(title, description)

        # Adjust based on historical data
        adjusted_points, confidence_score = self._adjust_from_history(
            base_points, similar
        )

        # Convert to Fibonacci
        suggested_points = self._to_fibonacci(adjusted_points)

        # Calculate range
        range_low, range_high = self._calculate_range(suggested_points, confidence_score)

        # Determine confidence level
        confidence = self._get_confidence_level(confidence_score)

        # Generate reasoning
        reasoning = self._generate_reasoning(indicators, similar, suggested_points)

        return EstimationResult(
            suggested_points=suggested_points,
            confidence=confidence,
            confidence_score=confidence_score,
            range_low=range_low,
            range_high=range_high,
            complexity_indicators=indicators,
            similar_stories=[s.story_id for s in similar[:3]],
            reasoning=reasoning
        )

    def _detect_complexity(self, text: str, tags: List[str]) -> List[ComplexityIndicator]:
        """Detect complexity indicators from text"""
        indicators = []
        combined = f"{text} {' '.join(tags)}".lower()

        for indicator, keywords in COMPLEXITY_KEYWORDS.items():
            for keyword in keywords:
                if keyword in combined:
                    if indicator not in indicators:
                        indicators.append(indicator)
                    break

        return indicators

    def _calculate_base_points(self, text: str, indicators: List[ComplexityIndicator]) -> float:
        """Calculate base points from text and indicators"""
        # Base on text length (proxy for scope)
        word_count = len(text.split())

        if word_count < 20:
            base = 1.5
        elif word_count < 50:
            base = 3.0
        elif word_count < 100:
            base = 5.0
        else:
            base = 8.0

        # Adjust for complexity indicators
        complexity_multiplier = 1.0

        if ComplexityIndicator.INTEGRATION in indicators:
            complexity_multiplier += 0.5
        if ComplexityIndicator.SECURITY in indicators:
            complexity_multiplier += 0.4
        if ComplexityIndicator.DATABASE in indicators:
            complexity_multiplier += 0.3
        if ComplexityIndicator.REFACTOR in indicators:
            complexity_multiplier += 0.2
        if ComplexityIndicator.BUG_FIX in indicators:
            complexity_multiplier -= 0.2  # Bugs typically smaller

        return base * complexity_multiplier

    def _find_similar_stories(self, title: str, description: str) -> List[HistoricalStory]:
        """Find similar stories from history"""
        if not self._historical_stories:
            return []

        # Simple keyword matching (can be replaced with embeddings)
        keywords = set(title.lower().split() + description.lower().split())
        keywords = {w for w in keywords if len(w) > 3}  # Filter short words

        scored_stories = []
        for story in self._historical_stories:
            story_keywords = set(
                story.title.lower().split() +
                (story.description or "").lower().split()
            )
            overlap = len(keywords & story_keywords)
            if overlap > 0:
                scored_stories.append((story, overlap))

        # Sort by similarity
        scored_stories.sort(key=lambda x: x[1], reverse=True)
        return [s[0] for s in scored_stories[:5]]

    def _adjust_from_history(self, base_points: float,
                            similar: List[HistoricalStory]) -> Tuple[float, float]:
        """Adjust estimation based on similar stories"""
        if not similar:
            return base_points, 0.5  # Medium confidence without history

        # Average points of similar stories
        historical_points = [s.estimated_points for s in similar if s.estimated_points]
        if not historical_points:
            return base_points, 0.5

        avg_historical = sum(historical_points) / len(historical_points)

        # Weight: 40% base, 60% historical
        adjusted = base_points * 0.4 + avg_historical * 0.6

        # Confidence increases with more similar stories
        confidence = min(0.9, 0.5 + len(similar) * 0.1)

        # Check if historical estimates were accurate
        accurate_count = sum(
            1 for s in similar
            if s.actual_points and abs(s.actual_points - s.estimated_points) <= 2
        )
        if accurate_count > 0:
            confidence += 0.1 * (accurate_count / len(similar))

        return adjusted, min(1.0, confidence)

    def _to_fibonacci(self, points: float) -> int:
        """Convert points to nearest Fibonacci number"""
        for i, fib in enumerate(FIBONACCI):
            if points <= fib:
                return fib
            if i < len(FIBONACCI) - 1 and points < (fib + FIBONACCI[i + 1]) / 2:
                return fib
        return FIBONACCI[-1]

    def _calculate_range(self, points: int, confidence: float) -> Tuple[int, int]:
        """Calculate estimation range based on confidence"""
        # Lower confidence = wider range
        spread = int((1 - confidence) * 2 + 1)

        idx = FIBONACCI.index(points) if points in FIBONACCI else 0
        low_idx = max(0, idx - spread)
        high_idx = min(len(FIBONACCI) - 1, idx + spread)

        return FIBONACCI[low_idx], FIBONACCI[high_idx]

    def _get_confidence_level(self, score: float) -> ConfidenceLevel:
        """Convert confidence score to level"""
        if score >= 0.7:
            return ConfidenceLevel.HIGH
        if score >= 0.4:
            return ConfidenceLevel.MEDIUM
        return ConfidenceLevel.LOW

    def _generate_reasoning(self, indicators: List[ComplexityIndicator],
                           similar: List[HistoricalStory],
                           points: int) -> str:
        """Generate human-readable reasoning"""
        parts = [f"Sugestao: {points} pontos."]

        if indicators:
            indicator_names = [i.value.replace("_", " ") for i in indicators]
            parts.append(f"Complexidade detectada: {', '.join(indicator_names)}.")

        if similar:
            avg = sum(s.estimated_points for s in similar if s.estimated_points) / len(similar)
            parts.append(f"Baseado em {len(similar)} stories similares (media: {avg:.1f} pts).")

        if not similar:
            parts.append("Sem historico similar - confianca reduzida.")

        return " ".join(parts)

    def record_feedback(self, story_id: str, suggested: int, actual: int):
        """Record user feedback on estimation accuracy"""
        self._feedback_data.append({
            "story_id": story_id,
            "suggested_points": suggested,
            "actual_points": actual,
            "accuracy": 1 - abs(suggested - actual) / max(suggested, actual),
            "timestamp": datetime.utcnow().isoformat()
        })

    def get_accuracy_stats(self) -> Dict[str, Any]:
        """Get estimation accuracy statistics"""
        if not self._feedback_data:
            return {"total": 0, "avg_accuracy": 0}

        accuracies = [f["accuracy"] for f in self._feedback_data]
        return {
            "total": len(self._feedback_data),
            "avg_accuracy": sum(accuracies) / len(accuracies),
            "perfect_estimates": sum(1 for f in self._feedback_data
                                    if f["suggested_points"] == f["actual_points"]),
            "within_one": sum(1 for f in self._feedback_data
                             if abs(f["suggested_points"] - f["actual_points"]) <= 1)
        }


# =============================================================================
# MODULE FUNCTIONS
# =============================================================================

_estimator = None


def get_estimator() -> StoryPointsEstimator:
    """Get global estimator instance"""
    global _estimator
    if _estimator is None:
        _estimator = StoryPointsEstimator()
    return _estimator


def estimate_story_points(title: str, description: str = "",
                         tags: List[str] = None) -> EstimationResult:
    """Quick function to estimate story points"""
    return get_estimator().estimate(title, description, tags)
