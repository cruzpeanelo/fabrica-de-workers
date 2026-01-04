# -*- coding: utf-8 -*-
"""
Tests for Story Points Estimator
Plataforma E v6.5

Tests for Issue #245:
1. Enum values
2. EstimationResult dataclass
3. Complexity detection
4. Base points calculation
5. Historical story matching
6. Fibonacci conversion
7. Confidence calculation
"""

import pytest
from datetime import datetime


# =============================================================================
# ENUM TESTS
# =============================================================================

class TestConfidenceLevel:
    """Tests for ConfidenceLevel enum"""

    def test_confidence_values(self):
        """Should have all confidence levels"""
        from factory.ai.estimator import ConfidenceLevel

        assert ConfidenceLevel.LOW.value == "low"
        assert ConfidenceLevel.MEDIUM.value == "medium"
        assert ConfidenceLevel.HIGH.value == "high"

    def test_confidence_is_string_enum(self):
        """Should be string enum"""
        from factory.ai.estimator import ConfidenceLevel

        assert isinstance(ConfidenceLevel.LOW.value, str)


class TestComplexityIndicator:
    """Tests for ComplexityIndicator enum"""

    def test_all_indicators_exist(self):
        """Should have all complexity indicators"""
        from factory.ai.estimator import ComplexityIndicator

        indicators = [
            ComplexityIndicator.INTEGRATION,
            ComplexityIndicator.DATABASE,
            ComplexityIndicator.SECURITY,
            ComplexityIndicator.UI,
            ComplexityIndicator.API,
            ComplexityIndicator.REFACTOR,
            ComplexityIndicator.NEW_FEATURE,
            ComplexityIndicator.BUG_FIX,
            ComplexityIndicator.TESTING
        ]

        assert len(indicators) == 9

    def test_indicator_values(self):
        """Should have correct values"""
        from factory.ai.estimator import ComplexityIndicator

        assert ComplexityIndicator.INTEGRATION.value == "integration"
        assert ComplexityIndicator.DATABASE.value == "database"
        assert ComplexityIndicator.BUG_FIX.value == "bug_fix"


# =============================================================================
# ESTIMATION RESULT TESTS
# =============================================================================

class TestEstimationResult:
    """Tests for EstimationResult dataclass"""

    def test_result_creation(self):
        """Should create result with required fields"""
        from factory.ai.estimator import EstimationResult, ConfidenceLevel

        result = EstimationResult(
            suggested_points=5,
            confidence=ConfidenceLevel.HIGH,
            confidence_score=0.85,
            range_low=3,
            range_high=8
        )

        assert result.suggested_points == 5
        assert result.confidence == ConfidenceLevel.HIGH
        assert result.confidence_score == 0.85

    def test_result_defaults(self):
        """Should have sensible defaults"""
        from factory.ai.estimator import EstimationResult, ConfidenceLevel

        result = EstimationResult(
            suggested_points=3,
            confidence=ConfidenceLevel.MEDIUM,
            confidence_score=0.5,
            range_low=2,
            range_high=5
        )

        assert result.complexity_indicators == []
        assert result.similar_stories == []
        assert result.reasoning == ""

    def test_result_to_dict(self):
        """Should convert to dictionary"""
        from factory.ai.estimator import EstimationResult, ConfidenceLevel, ComplexityIndicator

        result = EstimationResult(
            suggested_points=8,
            confidence=ConfidenceLevel.HIGH,
            confidence_score=0.9,
            range_low=5,
            range_high=13,
            complexity_indicators=[ComplexityIndicator.API, ComplexityIndicator.DATABASE],
            similar_stories=["STR-001", "STR-002"],
            reasoning="Test reasoning"
        )

        d = result.to_dict()

        assert d["suggested_points"] == 8
        assert d["confidence"] == "high"
        assert d["range"]["low"] == 5
        assert d["range"]["high"] == 13
        assert "api" in d["complexity_indicators"]
        assert len(d["similar_stories"]) == 2


# =============================================================================
# HISTORICAL STORY TESTS
# =============================================================================

class TestHistoricalStory:
    """Tests for HistoricalStory dataclass"""

    def test_story_creation(self):
        """Should create historical story"""
        from factory.ai.estimator import HistoricalStory

        story = HistoricalStory(
            story_id="STR-001",
            title="Test story",
            description="Test description",
            estimated_points=5
        )

        assert story.story_id == "STR-001"
        assert story.estimated_points == 5

    def test_story_with_actual_data(self):
        """Should store actual completion data"""
        from factory.ai.estimator import HistoricalStory

        story = HistoricalStory(
            story_id="STR-001",
            title="Test",
            description="",
            estimated_points=5,
            actual_points=8,
            actual_hours=16.5
        )

        assert story.actual_points == 8
        assert story.actual_hours == 16.5


# =============================================================================
# TEAM VELOCITY TESTS
# =============================================================================

class TestTeamVelocity:
    """Tests for TeamVelocity dataclass"""

    def test_velocity_creation(self):
        """Should create team velocity stats"""
        from factory.ai.estimator import TeamVelocity

        velocity = TeamVelocity(
            avg_velocity=32.5,
            std_dev=5.2,
            sprints_analyzed=10
        )

        assert velocity.avg_velocity == 32.5
        assert velocity.sprints_analyzed == 10

    def test_velocity_defaults(self):
        """Should have default points per hour"""
        from factory.ai.estimator import TeamVelocity

        velocity = TeamVelocity(
            avg_velocity=30,
            std_dev=5,
            sprints_analyzed=5
        )

        assert velocity.points_per_hour == 0.25  # 4 hours per point


# =============================================================================
# ESTIMATOR TESTS
# =============================================================================

class TestStoryPointsEstimator:
    """Tests for StoryPointsEstimator class"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_estimator_creation(self, estimator):
        """Should create estimator"""
        assert estimator is not None

    def test_estimate_simple_story(self, estimator):
        """Should estimate simple story"""
        result = estimator.estimate(
            title="Fix typo in readme",
            description="Simple fix"
        )

        assert result.suggested_points in [1, 2, 3]
        assert result.confidence is not None

    def test_estimate_complex_story(self, estimator):
        """Should estimate complex story with higher points"""
        result = estimator.estimate(
            title="Implement OAuth2 integration with external API",
            description="""
            Need to integrate with third-party OAuth provider.
            Implement secure token storage, refresh mechanism,
            database migrations for storing credentials,
            API endpoints for authentication flow.
            """
        )

        assert result.suggested_points >= 5
        assert len(result.complexity_indicators) > 0

    def test_estimate_returns_fibonacci(self, estimator):
        """Should return Fibonacci numbers"""
        fibonacci = [1, 2, 3, 5, 8, 13, 21, 34]

        result = estimator.estimate(
            title="Test story",
            description="Some description text here"
        )

        assert result.suggested_points in fibonacci


# =============================================================================
# COMPLEXITY DETECTION TESTS
# =============================================================================

class TestComplexityDetection:
    """Tests for complexity detection"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_detect_integration(self, estimator):
        """Should detect integration complexity"""
        from factory.ai.estimator import ComplexityIndicator

        indicators = estimator._detect_complexity(
            "integrate with external webhook api",
            []
        )

        assert ComplexityIndicator.INTEGRATION in indicators

    def test_detect_database(self, estimator):
        """Should detect database complexity"""
        from factory.ai.estimator import ComplexityIndicator

        indicators = estimator._detect_complexity(
            "add new table with foreign key relationship",
            []
        )

        assert ComplexityIndicator.DATABASE in indicators

    def test_detect_security(self, estimator):
        """Should detect security complexity"""
        from factory.ai.estimator import ComplexityIndicator

        indicators = estimator._detect_complexity(
            "implement authentication with password encryption",
            []
        )

        assert ComplexityIndicator.SECURITY in indicators

    def test_detect_from_tags(self, estimator):
        """Should detect complexity from tags"""
        from factory.ai.estimator import ComplexityIndicator

        indicators = estimator._detect_complexity(
            "simple story",
            ["api", "security"]
        )

        assert ComplexityIndicator.API in indicators
        assert ComplexityIndicator.SECURITY in indicators

    def test_detect_multiple_indicators(self, estimator):
        """Should detect multiple complexity indicators"""
        indicators = estimator._detect_complexity(
            "integrate external api with database migration and security",
            []
        )

        assert len(indicators) >= 3

    def test_no_duplicates(self, estimator):
        """Should not have duplicate indicators"""
        indicators = estimator._detect_complexity(
            "api endpoint api route api controller",
            ["api"]
        )

        # Count occurrences
        from factory.ai.estimator import ComplexityIndicator
        api_count = sum(1 for i in indicators if i == ComplexityIndicator.API)
        assert api_count == 1


# =============================================================================
# BASE POINTS CALCULATION TESTS
# =============================================================================

class TestBasePointsCalculation:
    """Tests for base points calculation"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_short_text_low_points(self, estimator):
        """Short text should give low points"""
        points = estimator._calculate_base_points(
            "fix bug",
            []
        )

        assert points <= 3

    def test_long_text_higher_points(self, estimator):
        """Long text should give higher points"""
        long_text = " ".join(["word"] * 150)
        points = estimator._calculate_base_points(long_text, [])

        assert points >= 5

    def test_integration_increases_points(self, estimator):
        """Integration indicator should increase points"""
        from factory.ai.estimator import ComplexityIndicator

        base = estimator._calculate_base_points("test", [])
        with_integration = estimator._calculate_base_points(
            "test",
            [ComplexityIndicator.INTEGRATION]
        )

        assert with_integration > base

    def test_bug_fix_decreases_points(self, estimator):
        """Bug fix indicator should decrease points"""
        from factory.ai.estimator import ComplexityIndicator

        base = estimator._calculate_base_points("test text here", [])
        with_bugfix = estimator._calculate_base_points(
            "test text here",
            [ComplexityIndicator.BUG_FIX]
        )

        assert with_bugfix < base


# =============================================================================
# FIBONACCI CONVERSION TESTS
# =============================================================================

class TestFibonacciConversion:
    """Tests for Fibonacci conversion"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_exact_match(self, estimator):
        """Should return exact Fibonacci if matched"""
        assert estimator._to_fibonacci(5.0) == 5
        assert estimator._to_fibonacci(8.0) == 8

    def test_rounds_up_small(self, estimator):
        """Should round up for small values"""
        assert estimator._to_fibonacci(0.5) == 1
        assert estimator._to_fibonacci(1.5) == 2

    def test_rounds_to_nearest(self, estimator):
        """Should round to nearest Fibonacci"""
        assert estimator._to_fibonacci(4.0) == 5  # Closer to 5 than 3
        assert estimator._to_fibonacci(6.0) == 5  # Closer to 5 than 8

    def test_max_value(self, estimator):
        """Should cap at max Fibonacci"""
        assert estimator._to_fibonacci(100.0) == 34


# =============================================================================
# RANGE CALCULATION TESTS
# =============================================================================

class TestRangeCalculation:
    """Tests for estimation range calculation"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_high_confidence_narrow_range(self, estimator):
        """High confidence should give narrow range"""
        low, high = estimator._calculate_range(5, 0.9)

        assert high - low <= 5

    def test_low_confidence_wide_range(self, estimator):
        """Low confidence should give wider range"""
        low, high = estimator._calculate_range(5, 0.3)

        assert high - low > 5

    def test_range_within_fibonacci(self, estimator):
        """Range should be valid Fibonacci numbers"""
        from factory.ai.estimator import FIBONACCI

        low, high = estimator._calculate_range(5, 0.5)

        assert low in FIBONACCI
        assert high in FIBONACCI


# =============================================================================
# CONFIDENCE LEVEL TESTS
# =============================================================================

class TestConfidenceLevelMapping:
    """Tests for confidence level mapping"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_high_confidence(self, estimator):
        """Score >= 0.7 should be HIGH"""
        from factory.ai.estimator import ConfidenceLevel

        assert estimator._get_confidence_level(0.9) == ConfidenceLevel.HIGH
        assert estimator._get_confidence_level(0.7) == ConfidenceLevel.HIGH

    def test_medium_confidence(self, estimator):
        """Score 0.4-0.69 should be MEDIUM"""
        from factory.ai.estimator import ConfidenceLevel

        assert estimator._get_confidence_level(0.5) == ConfidenceLevel.MEDIUM
        assert estimator._get_confidence_level(0.4) == ConfidenceLevel.MEDIUM

    def test_low_confidence(self, estimator):
        """Score < 0.4 should be LOW"""
        from factory.ai.estimator import ConfidenceLevel

        assert estimator._get_confidence_level(0.3) == ConfidenceLevel.LOW
        assert estimator._get_confidence_level(0.1) == ConfidenceLevel.LOW


# =============================================================================
# SIMILAR STORIES TESTS
# =============================================================================

class TestSimilarStories:
    """Tests for finding similar stories"""

    @pytest.fixture
    def estimator_with_history(self):
        from factory.ai.estimator import StoryPointsEstimator, HistoricalStory

        estimator = StoryPointsEstimator()
        estimator.load_historical_data([
            HistoricalStory(
                story_id="STR-001",
                title="Implement user authentication",
                description="Login and logout",
                estimated_points=8
            ),
            HistoricalStory(
                story_id="STR-002",
                title="Add payment integration",
                description="Stripe API",
                estimated_points=13
            ),
            HistoricalStory(
                story_id="STR-003",
                title="Fix login bug",
                description="Authentication error",
                estimated_points=3
            )
        ])
        return estimator

    def test_find_similar_by_title(self, estimator_with_history):
        """Should find similar stories by title keywords"""
        similar = estimator_with_history._find_similar_stories(
            "user authentication system",
            ""
        )

        assert len(similar) > 0
        assert any(s.story_id == "STR-001" for s in similar)

    def test_no_similar_found(self):
        """Should return empty list when no history"""
        from factory.ai.estimator import StoryPointsEstimator

        estimator = StoryPointsEstimator()
        similar = estimator._find_similar_stories("test", "desc")

        assert similar == []


# =============================================================================
# HISTORY ADJUSTMENT TESTS
# =============================================================================

class TestHistoryAdjustment:
    """Tests for historical adjustment"""

    @pytest.fixture
    def estimator_with_history(self):
        from factory.ai.estimator import StoryPointsEstimator, HistoricalStory

        estimator = StoryPointsEstimator()
        estimator.load_historical_data([
            HistoricalStory(
                story_id="STR-001",
                title="Similar story one",
                description="Test",
                estimated_points=8,
                actual_points=8
            ),
            HistoricalStory(
                story_id="STR-002",
                title="Similar story two",
                description="Test",
                estimated_points=5,
                actual_points=5
            )
        ])
        return estimator

    def test_adjust_with_history(self, estimator_with_history):
        """Should adjust points based on history"""
        from factory.ai.estimator import HistoricalStory

        similar = [
            HistoricalStory("STR-001", "test", "", 8),
            HistoricalStory("STR-002", "test", "", 5)
        ]

        adjusted, confidence = estimator_with_history._adjust_from_history(
            5.0, similar
        )

        # Should be weighted average
        assert adjusted != 5.0
        assert confidence > 0.5

    def test_no_history_medium_confidence(self):
        """Should return medium confidence without history"""
        from factory.ai.estimator import StoryPointsEstimator

        estimator = StoryPointsEstimator()
        adjusted, confidence = estimator._adjust_from_history(5.0, [])

        assert adjusted == 5.0
        assert confidence == 0.5


# =============================================================================
# FEEDBACK TESTS
# =============================================================================

class TestFeedback:
    """Tests for feedback recording"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_record_feedback(self, estimator):
        """Should record feedback"""
        estimator.record_feedback("STR-001", suggested=5, actual=5)
        estimator.record_feedback("STR-002", suggested=8, actual=5)

        assert len(estimator._feedback_data) == 2

    def test_accuracy_stats(self, estimator):
        """Should calculate accuracy stats"""
        estimator.record_feedback("STR-001", suggested=5, actual=5)
        estimator.record_feedback("STR-002", suggested=8, actual=8)
        estimator.record_feedback("STR-003", suggested=5, actual=3)

        stats = estimator.get_accuracy_stats()

        assert stats["total"] == 3
        assert stats["perfect_estimates"] == 2
        assert stats["avg_accuracy"] > 0

    def test_empty_accuracy_stats(self, estimator):
        """Should handle empty feedback"""
        stats = estimator.get_accuracy_stats()

        assert stats["total"] == 0
        assert stats["avg_accuracy"] == 0


# =============================================================================
# REASONING GENERATION TESTS
# =============================================================================

class TestReasoningGeneration:
    """Tests for reasoning text generation"""

    @pytest.fixture
    def estimator(self):
        from factory.ai.estimator import StoryPointsEstimator
        return StoryPointsEstimator()

    def test_reasoning_includes_points(self, estimator):
        """Reasoning should mention suggested points"""
        from factory.ai.estimator import ComplexityIndicator

        reasoning = estimator._generate_reasoning(
            [ComplexityIndicator.API],
            [],
            5
        )

        assert "5" in reasoning

    def test_reasoning_includes_complexity(self, estimator):
        """Reasoning should mention complexity"""
        from factory.ai.estimator import ComplexityIndicator

        reasoning = estimator._generate_reasoning(
            [ComplexityIndicator.DATABASE, ComplexityIndicator.SECURITY],
            [],
            8
        )

        assert "database" in reasoning.lower() or "security" in reasoning.lower()

    def test_reasoning_no_history_note(self, estimator):
        """Should note when no history available"""
        reasoning = estimator._generate_reasoning([], [], 3)

        assert "historico" in reasoning.lower() or "confianca" in reasoning.lower()


# =============================================================================
# MODULE FUNCTIONS TESTS
# =============================================================================

class TestModuleFunctions:
    """Tests for module-level functions"""

    def test_get_estimator_singleton(self):
        """Should return singleton estimator"""
        from factory.ai.estimator import get_estimator

        est1 = get_estimator()
        est2 = get_estimator()

        assert est1 is est2

    def test_estimate_story_points_function(self):
        """Should provide quick estimation function"""
        from factory.ai.estimator import estimate_story_points

        result = estimate_story_points(
            "Add new feature",
            "Create a new button"
        )

        assert result.suggested_points > 0
        assert result.confidence is not None


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestEstimatorIntegration:
    """Integration tests for estimator"""

    def test_full_estimation_flow(self):
        """Should complete full estimation flow"""
        from factory.ai.estimator import (
            StoryPointsEstimator, HistoricalStory, TeamVelocity
        )

        # Create estimator with history
        estimator = StoryPointsEstimator()
        estimator.load_historical_data([
            HistoricalStory(
                story_id="STR-001",
                title="API endpoint implementation",
                description="REST API for users",
                estimated_points=5,
                actual_points=5
            )
        ])
        estimator.set_team_velocity(TeamVelocity(
            avg_velocity=30,
            std_dev=5,
            sprints_analyzed=10
        ))

        # Estimate similar story
        result = estimator.estimate(
            title="Implement API endpoint for products",
            description="Create REST API with CRUD operations",
            tags=["api", "backend"]
        )

        # Verify result
        assert result.suggested_points > 0
        assert len(result.complexity_indicators) > 0
        assert result.range_low <= result.suggested_points <= result.range_high

    def test_estimate_with_tags(self):
        """Should use tags in estimation"""
        from factory.ai.estimator import StoryPointsEstimator, ComplexityIndicator

        estimator = StoryPointsEstimator()
        result = estimator.estimate(
            title="Simple task",
            description="",
            tags=["integration", "security", "database"]
        )

        assert ComplexityIndicator.INTEGRATION in result.complexity_indicators
        assert ComplexityIndicator.SECURITY in result.complexity_indicators
        assert ComplexityIndicator.DATABASE in result.complexity_indicators


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
