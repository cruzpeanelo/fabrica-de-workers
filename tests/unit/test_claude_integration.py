"""
Unit Tests for Claude AI Integration
=====================================

Tests for factory.ai.claude_integration module
"""

import pytest
from unittest.mock import Mock, patch, MagicMock

from factory.ai.claude_integration import (
    ClaudeClient, ClaudeResponse, ClaudeMessage, AgentBrain,
    get_claude_client, create_agent_brain
)


class TestClaudeResponse:
    """Tests for ClaudeResponse dataclass"""

    @pytest.mark.unit
    def test_success_response(self):
        """Test creating a successful response"""
        response = ClaudeResponse(
            success=True,
            content="Test content",
            model="claude-3-opus",
            tokens_used=100
        )

        assert response.success is True
        assert response.content == "Test content"
        assert response.tokens_used == 100
        assert response.error is None

    @pytest.mark.unit
    def test_error_response(self):
        """Test creating an error response"""
        response = ClaudeResponse(
            success=False,
            content="",
            error="API Error"
        )

        assert response.success is False
        assert response.error == "API Error"


class TestClaudeMessage:
    """Tests for ClaudeMessage dataclass"""

    @pytest.mark.unit
    def test_message_creation(self):
        """Test creating a message"""
        msg = ClaudeMessage(
            role="user",
            content="Test message"
        )

        assert msg.role == "user"
        assert msg.content == "Test message"
        assert msg.timestamp is not None


class TestClaudeClient:
    """Tests for ClaudeClient class"""

    @pytest.mark.unit
    def test_client_without_api_key(self):
        """Test client initialization without API key"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            assert client.is_available() is False

    @pytest.mark.unit
    def test_client_with_api_key_no_anthropic(self):
        """Test client with API key but no anthropic library"""
        with patch('factory.ai.claude_integration.HAS_ANTHROPIC', False):
            client = ClaudeClient(api_key="test-key")
            assert client.is_available() is False

    @pytest.mark.unit
    def test_chat_unavailable(self):
        """Test chat when client is unavailable"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            response = client.chat("Test message")

            assert response.success is False
            assert "nao disponivel" in response.error.lower()

    @pytest.mark.unit
    def test_analyze_requirements_unavailable(self):
        """Test analyze_requirements when unavailable"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            response = client.analyze_requirements("Test requirements")

            assert response.success is False

    @pytest.mark.unit
    def test_generate_code_unavailable(self):
        """Test generate_code when unavailable"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            response = client.generate_code("Create a function")

            assert response.success is False

    @pytest.mark.unit
    def test_review_code_unavailable(self):
        """Test review_code when unavailable"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            response = client.review_code("def test(): pass")

            assert response.success is False

    @pytest.mark.unit
    def test_create_user_story_unavailable(self):
        """Test create_user_story when unavailable"""
        with patch.dict('os.environ', {}, clear=True):
            client = ClaudeClient(api_key=None)
            response = client.create_user_story("User requirement")

            assert response.success is False

    @pytest.mark.unit
    def test_clear_history(self):
        """Test clearing conversation history"""
        client = ClaudeClient(api_key=None)
        client.conversation_history = [
            ClaudeMessage(role="user", content="test")
        ]

        client.clear_history()

        assert len(client.conversation_history) == 0


class TestAgentBrain:
    """Tests for AgentBrain class"""

    @pytest.mark.unit
    def test_brain_creation(self):
        """Test creating an agent brain"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=["python", "fastapi"]
        )

        assert brain.agent_id == "AG-001"
        assert brain.agent_role == "Developer"
        assert "python" in brain.capabilities

    @pytest.mark.unit
    def test_brain_system_prompt(self):
        """Test brain system prompt generation"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=["python"]
        )

        assert "AG-001" in brain.system_prompt
        assert "Developer" in brain.system_prompt
        assert "python" in brain.system_prompt

    @pytest.mark.unit
    def test_think_unavailable(self):
        """Test thinking when Claude is unavailable - Issue #210"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=["python"]
        )

        # Issue #210: Mock ClaudeClient.is_available() para retornar False
        mock_claude = Mock()
        mock_claude.is_available.return_value = False
        brain.claude = mock_claude

        response = brain.think("How to solve this?")

        assert response.success is False
        assert "nao disponivel" in response.error.lower()

    @pytest.mark.unit
    def test_decide_unavailable(self):
        """Test decision making when unavailable - Issue #210"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        # Issue #210: Mock ClaudeClient.chat() para simular falha
        mock_claude = Mock()
        mock_claude.chat.return_value = ClaudeResponse(
            success=False,
            content="",
            error="API unavailable"
        )
        brain.claude = mock_claude

        response = brain.decide(["Option A", "Option B"])

        # O decide usa o retorno do chat diretamente
        assert response.success is False

    @pytest.mark.unit
    def test_learn(self):
        """Test learning from experience"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        brain.learn({
            "task": "Code generation",
            "success": True,
            "pattern": "Use type hints",
            "context": "Python development"
        })

        assert len(brain.memory) == 1
        assert brain.memory[0]["type"] == "learning"
        assert len(brain.learned_patterns) == 1

    @pytest.mark.unit
    def test_learn_without_pattern(self):
        """Test learning without successful pattern"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        brain.learn({
            "task": "Failed task",
            "success": False
        })

        assert len(brain.memory) == 1
        assert len(brain.learned_patterns) == 0

    @pytest.mark.unit
    def test_get_relevant_memories(self):
        """Test retrieving relevant memories"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        # Add some memories
        for i in range(10):
            brain.memory.append({"id": i, "data": f"Memory {i}"})

        memories = brain.get_relevant_memories("test context", limit=5)

        assert len(memories) == 5
        # Should return most recent
        assert memories[-1]["id"] == 9

    @pytest.mark.unit
    def test_should_ask_approval_high_impact(self):
        """Test approval requirement for high impact actions"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        assert brain.should_ask_approval("delete database") is True
        assert brain.should_ask_approval("deploy to production") is True
        assert brain.should_ask_approval("modify_production config") is True

    @pytest.mark.unit
    def test_should_ask_approval_low_impact(self):
        """Test approval not required for low impact actions"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        assert brain.should_ask_approval("read file", impact="low") is False
        assert brain.should_ask_approval("create test", impact="medium") is False

    @pytest.mark.unit
    def test_should_ask_approval_by_impact_level(self):
        """Test approval based on impact level"""
        brain = AgentBrain(
            agent_id="AG-001",
            agent_role="Developer",
            agent_capabilities=[]
        )

        assert brain.should_ask_approval("any action", impact="high") is True
        assert brain.should_ask_approval("any action", impact="critical") is True


class TestGlobalFunctions:
    """Tests for global helper functions"""

    @pytest.mark.unit
    def test_get_claude_client_singleton(self):
        """Test that get_claude_client returns same instance"""
        # Reset global client
        import factory.ai.claude_integration as module
        module._claude_client = None

        client1 = get_claude_client()
        client2 = get_claude_client()

        assert client1 is client2

    @pytest.mark.unit
    def test_create_agent_brain(self):
        """Test creating agent brain via helper function"""
        brain = create_agent_brain(
            agent_id="AG-002",
            agent_role="Tester",
            capabilities=["pytest", "selenium"]
        )

        assert isinstance(brain, AgentBrain)
        assert brain.agent_id == "AG-002"
        assert brain.agent_role == "Tester"
