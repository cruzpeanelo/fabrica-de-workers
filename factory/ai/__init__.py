"""
Modulo de Inteligencia Artificial da Plataforma E
=======================================================

Este pacote fornece integracao com LLMs para dar inteligencia
real aos agentes da plataforma.

Suporte a multiplos provedores:
- Claude (Anthropic)
- Azure OpenAI
- AWS Bedrock
- Google Vertex AI
"""

from .claude_integration import (
    ClaudeClient,
    ClaudeMessage,
    ClaudeResponse,
    AgentBrain,
    get_claude_client,
    create_agent_brain
)

# LLM Manager - Multi-provider support
from .llm_manager import (
    LLMClient,
    LLMConfig,
    LLMResponse,
    LLMMessage,
    LLMProvider,
    ClaudeClient as ClaudeLLMClient,
    AzureOpenAIClient,
    AWSBedrockClient,
    GoogleVertexClient,
    get_llm_client,
    get_available_providers,
    test_all_providers,
    register_provider,
    get_default_provider,
    set_default_provider,
    clear_client_cache
)

# Story Categorizer (Issue #246)
from .categorizer import (
    StoryCategorizer,
    StoryType,
    CategorizationResult,
    categorize_story,
    suggest_story_type,
    suggest_story_labels,
    get_categorizer
)

__all__ = [
    # Legacy Claude integration
    "ClaudeClient",
    "ClaudeMessage",
    "ClaudeResponse",
    "AgentBrain",
    "get_claude_client",
    "create_agent_brain",
    # LLM Manager (Multi-provider)
    "LLMClient",
    "LLMConfig",
    "LLMResponse",
    "LLMMessage",
    "LLMProvider",
    "ClaudeLLMClient",
    "AzureOpenAIClient",
    "AWSBedrockClient",
    "GoogleVertexClient",
    "get_llm_client",
    "get_available_providers",
    "test_all_providers",
    "register_provider",
    "get_default_provider",
    "set_default_provider",
    "clear_client_cache",
    # Story Categorizer (Issue #246)
    "StoryCategorizer",
    "StoryType",
    "CategorizationResult",
    "categorize_story",
    "suggest_story_type",
    "suggest_story_labels",
    "get_categorizer"
]
