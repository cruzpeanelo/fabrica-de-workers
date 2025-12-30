# -*- coding: utf-8 -*-
"""
Advanced Contextual AI Assistant (Issue #280)
==============================================

Assistente IA avancado com suporte a:
- Context-aware responses (projeto, story, usuario)
- Suggested actions based on context
- Code explanation capabilities
- Story refinement suggestions
- Semantic understanding of project state

Versao: 1.0.0
"""

import os
import re
import json
from datetime import datetime, timezone
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum

# Anthropic Claude
try:
    import anthropic
    HAS_ANTHROPIC = True
except ImportError:
    HAS_ANTHROPIC = False


# =============================================================================
# ENUMS AND TYPES
# =============================================================================

class SuggestionType(str, Enum):
    """Types of AI suggestions"""
    ACTION = "action"
    REFINEMENT = "refinement"
    WARNING = "warning"
    TIP = "tip"
    CODE = "code"


class ActionType(str, Enum):
    """Types of suggested actions"""
    CREATE_STORY = "create_story"
    UPDATE_STORY = "update_story"
    CREATE_TASK = "create_task"
    COMPLETE_TASK = "complete_task"
    ADD_ACCEPTANCE_CRITERIA = "add_acceptance_criteria"
    REFINE_STORY = "refine_story"
    GENERATE_DOCS = "generate_docs"
    GENERATE_TESTS = "generate_tests"
    CODE_REVIEW = "code_review"
    EXPLAIN_CODE = "explain_code"
    ESTIMATE_EFFORT = "estimate_effort"


# =============================================================================
# DATACLASSES
# =============================================================================

@dataclass
class AssistantContext:
    """Context for the AI assistant"""
    # User context
    user_id: Optional[str] = None
    user_name: Optional[str] = None
    user_role: Optional[str] = None

    # Project context
    project_id: Optional[str] = None
    project_name: Optional[str] = None
    project_type: Optional[str] = None
    project_status: Optional[str] = None

    # Story context
    story_id: Optional[str] = None
    story_title: Optional[str] = None
    story_status: Optional[str] = None
    story_persona: Optional[str] = None
    story_action: Optional[str] = None
    story_benefit: Optional[str] = None
    story_points: Optional[int] = None
    acceptance_criteria: List[str] = field(default_factory=list)

    # Task context
    task_id: Optional[str] = None
    task_title: Optional[str] = None
    task_status: Optional[str] = None

    # Code context
    code_snippet: Optional[str] = None
    code_language: Optional[str] = None
    code_file_path: Optional[str] = None

    # Conversation history
    conversation_history: List[Dict] = field(default_factory=list)

    # Additional context
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)

    def get_summary(self) -> str:
        """Returns a summary of the context for the system prompt"""
        parts = []

        if self.project_name:
            parts.append(f"Projeto: {self.project_name} ({self.project_status or 'N/A'})")

        if self.story_title:
            story_info = f"Story: {self.story_id} - {self.story_title}"
            if self.story_status:
                story_info += f" [{self.story_status}]"
            parts.append(story_info)

            if self.story_persona and self.story_action:
                parts.append(f"  Como {self.story_persona}, quero {self.story_action}")
                if self.story_benefit:
                    parts.append(f"  Para {self.story_benefit}")

        if self.task_title:
            parts.append(f"Task atual: {self.task_id} - {self.task_title} [{self.task_status or 'N/A'}]")

        if self.code_snippet:
            lang = self.code_language or "code"
            parts.append(f"Codigo ({lang}):\n```{lang}\n{self.code_snippet[:500]}...\n```")

        return "\n".join(parts) if parts else "Sem contexto especifico."


@dataclass
class AISuggestion:
    """An AI-generated suggestion"""
    suggestion_id: str
    suggestion_type: str
    title: str
    description: str
    action_type: Optional[str] = None
    action_data: Dict[str, Any] = field(default_factory=dict)
    priority: int = 0
    confidence: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class AssistantResponse:
    """Response from the AI assistant"""
    message: str
    suggestions: List[AISuggestion] = field(default_factory=list)
    actions_taken: List[Dict] = field(default_factory=list)
    context_used: Dict[str, Any] = field(default_factory=dict)
    confidence: float = 0.0
    tokens_used: int = 0
    model_used: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "message": self.message,
            "suggestions": [s.to_dict() for s in self.suggestions],
            "actions_taken": self.actions_taken,
            "context_used": self.context_used,
            "confidence": self.confidence,
            "tokens_used": self.tokens_used,
            "model_used": self.model_used
        }


# =============================================================================
# CONTEXTUAL AI ASSISTANT
# =============================================================================

class ContextualAIAssistant:
    """
    Advanced AI Assistant with contextual awareness.

    Features:
    - Context-aware responses based on project, story, and user context
    - Suggested actions derived from current state
    - Code explanation and review capabilities
    - Story refinement suggestions
    - Multi-turn conversation support
    """

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the AI Assistant.

        Args:
            api_key: Anthropic API key (uses ANTHROPIC_API_KEY env var if not provided)
        """
        self.api_key = api_key or os.getenv("ANTHROPIC_API_KEY")
        self._client = None
        self.model = "claude-sonnet-4-20250514"
        self.max_tokens = 2048
        self.temperature = 0.7

    @property
    def client(self):
        """Lazy-load the Anthropic client"""
        if self._client is None and HAS_ANTHROPIC and self.api_key:
            self._client = anthropic.Anthropic(api_key=self.api_key)
        return self._client

    def _build_system_prompt(self, context: AssistantContext) -> str:
        """Build a contextual system prompt"""
        context_summary = context.get_summary()

        return f"""Voce e um assistente IA avancado da Fabrica de Agentes, especializado em desenvolvimento agil de software.

CONTEXTO ATUAL:
{context_summary}

SUAS CAPACIDADES:
1. Responder perguntas sobre o projeto, stories e tasks
2. Sugerir acoes baseadas no contexto atual
3. Ajudar a refinar user stories (criterios de aceite, pontos)
4. Explicar codigo e sugerir melhorias
5. Gerar documentacao tecnica
6. Estimar esforco de desenvolvimento

DIRETRIZES:
- Sempre considere o contexto atual ao responder
- Seja proativo em sugerir acoes uteis
- Formate respostas em Markdown quando apropriado
- Use linguagem clara e objetiva
- Quando sugerir acoes, inclua o tipo de acao e dados necessarios

FORMATO DE SUGESTOES:
Quando sugerir acoes, inclua em formato JSON no final da resposta:
```json
{{"suggestions": [
  {{"type": "action|refinement|warning|tip", "title": "...", "description": "...", "action_type": "...", "action_data": {{...}}}}
]}}
```

IDIOMA: Responda sempre em Portugues do Brasil."""

    def _parse_suggestions(self, response_text: str) -> Tuple[str, List[AISuggestion]]:
        """Parse suggestions from the response text"""
        suggestions = []
        clean_message = response_text

        # Try to find JSON suggestions block
        json_match = re.search(r'```json\s*(\{.*?"suggestions".*?\})\s*```', response_text, re.DOTALL)

        if json_match:
            try:
                json_str = json_match.group(1)
                data = json.loads(json_str)

                for idx, sug in enumerate(data.get("suggestions", [])):
                    import uuid
                    suggestion = AISuggestion(
                        suggestion_id=f"SUG-{uuid.uuid4().hex[:8].upper()}",
                        suggestion_type=sug.get("type", SuggestionType.TIP.value),
                        title=sug.get("title", "Sugestao"),
                        description=sug.get("description", ""),
                        action_type=sug.get("action_type"),
                        action_data=sug.get("action_data", {}),
                        priority=sug.get("priority", idx),
                        confidence=sug.get("confidence", 0.8)
                    )
                    suggestions.append(suggestion)

                # Remove JSON block from message
                clean_message = response_text[:json_match.start()].strip()

            except json.JSONDecodeError:
                pass

        return clean_message, suggestions

    async def chat(
        self,
        message: str,
        context: Optional[AssistantContext] = None,
        include_suggestions: bool = True
    ) -> AssistantResponse:
        """
        Send a message to the AI assistant with context.

        Args:
            message: User message
            context: Context information (project, story, user, etc.)
            include_suggestions: Whether to include action suggestions

        Returns:
            AssistantResponse with message, suggestions, and metadata
        """
        if not self.client:
            return AssistantResponse(
                message="Desculpe, o assistente IA nao esta disponivel no momento. Verifique a configuracao da API.",
                confidence=0.0
            )

        context = context or AssistantContext()

        # Build messages
        messages = []

        # Add conversation history
        for hist_msg in context.conversation_history[-10:]:
            messages.append({
                "role": hist_msg.get("role", "user"),
                "content": hist_msg.get("content", "")
            })

        # Add current message
        if include_suggestions:
            enhanced_message = f"{message}\n\n(Se houver sugestoes de acoes relevantes, inclua ao final da resposta)"
        else:
            enhanced_message = message

        messages.append({"role": "user", "content": enhanced_message})

        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=self.max_tokens,
                system=self._build_system_prompt(context),
                messages=messages
            )

            response_text = response.content[0].text
            clean_message, suggestions = self._parse_suggestions(response_text)

            return AssistantResponse(
                message=clean_message,
                suggestions=suggestions,
                context_used=context.to_dict(),
                confidence=0.9,
                tokens_used=response.usage.output_tokens,
                model_used=response.model
            )

        except Exception as e:
            return AssistantResponse(
                message=f"Desculpe, ocorreu um erro ao processar sua mensagem: {str(e)}",
                confidence=0.0
            )

    async def get_suggestions(
        self,
        context: AssistantContext
    ) -> List[AISuggestion]:
        """
        Get AI suggestions based on current context.

        Args:
            context: Current context information

        Returns:
            List of AI suggestions
        """
        if not self.client:
            return []

        prompt = f"""Analise o contexto atual e sugira acoes uteis para o usuario.

Contexto:
{context.get_summary()}

Gere uma lista de 3-5 sugestoes relevantes no formato JSON:
```json
{{"suggestions": [
  {{
    "type": "action|refinement|warning|tip",
    "title": "Titulo curto",
    "description": "Descricao da sugestao",
    "action_type": "create_story|update_story|create_task|complete_task|add_acceptance_criteria|refine_story|generate_docs|generate_tests|code_review|estimate_effort",
    "action_data": {{}},
    "priority": 0,
    "confidence": 0.9
  }}
]}}
```

Considere:
- Status atual do projeto/story
- Possiveis proximos passos
- Melhorias potenciais
- Avisos importantes"""

        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=1024,
                system="Voce e um assistente especializado em sugerir acoes de desenvolvimento agil. Responda apenas com JSON valido.",
                messages=[{"role": "user", "content": prompt}]
            )

            _, suggestions = self._parse_suggestions(response.content[0].text)
            return suggestions

        except Exception:
            return []

    async def explain_code(
        self,
        code: str,
        language: Optional[str] = None,
        context: Optional[AssistantContext] = None
    ) -> AssistantResponse:
        """
        Explain a code snippet.

        Args:
            code: Code snippet to explain
            language: Programming language
            context: Optional context information

        Returns:
            AssistantResponse with code explanation
        """
        if not self.client:
            return AssistantResponse(
                message="Assistente IA nao disponivel.",
                confidence=0.0
            )

        lang = language or "code"

        prompt = f"""Explique o seguinte codigo de forma clara e didatica:

```{lang}
{code}
```

Inclua:
1. **Visao Geral**: O que o codigo faz
2. **Componentes Principais**: Funcoes, classes, variaveis importantes
3. **Fluxo de Execucao**: Como o codigo funciona passo a passo
4. **Boas Praticas**: O que esta bem feito
5. **Sugestoes de Melhoria**: Possiveis melhorias (se houver)

Se houver sugestoes de acoes, inclua ao final em formato JSON."""

        context = context or AssistantContext()
        context.code_snippet = code
        context.code_language = language

        return await self.chat(prompt, context)

    async def refine_story(
        self,
        context: AssistantContext
    ) -> AssistantResponse:
        """
        Get refinement suggestions for a user story.

        Args:
            context: Context with story information

        Returns:
            AssistantResponse with refinement suggestions
        """
        if not context.story_title:
            return AssistantResponse(
                message="Nenhuma story selecionada para refinamento.",
                confidence=0.0
            )

        prompt = f"""Analise a seguinte User Story e sugira refinamentos:

**{context.story_id}: {context.story_title}**

Narrativa:
- Como: {context.story_persona or '[nao definido]'}
- Quero: {context.story_action or '[nao definido]'}
- Para: {context.story_benefit or '[nao definido]'}

Story Points: {context.story_points or 'Nao estimado'}
Status: {context.story_status or 'N/A'}

Criterios de Aceite Atuais:
{chr(10).join('- ' + c for c in context.acceptance_criteria) if context.acceptance_criteria else '- Nenhum definido'}

Por favor, sugira:
1. **Melhorias na Narrativa**: A narrativa segue o padrao "Como/Quero/Para"? Esta clara?
2. **Criterios de Aceite**: Sugira criterios de aceite claros e testaveis
3. **Estimativa**: A estimativa de pontos parece adequada?
4. **Divisao**: A story deveria ser dividida em stories menores?
5. **Dependencias**: Ha dependencias implicitas que deveriam ser explicitas?

Inclua sugestoes de acoes ao final."""

        return await self.chat(prompt, context, include_suggestions=True)

    async def generate_tasks(
        self,
        context: AssistantContext
    ) -> AssistantResponse:
        """
        Generate task suggestions for a story.

        Args:
            context: Context with story information

        Returns:
            AssistantResponse with task suggestions
        """
        if not context.story_title:
            return AssistantResponse(
                message="Nenhuma story selecionada.",
                confidence=0.0
            )

        prompt = f"""Sugira tasks de desenvolvimento para a seguinte User Story:

**{context.story_id}: {context.story_title}**

Narrativa:
- Como: {context.story_persona or '[nao definido]'}
- Quero: {context.story_action or '[nao definido]'}
- Para: {context.story_benefit or '[nao definido]'}

Criterios de Aceite:
{chr(10).join('- ' + c for c in context.acceptance_criteria) if context.acceptance_criteria else '- Nenhum definido'}

Sugira tasks considerando:
1. **Desenvolvimento**: Implementacao do codigo
2. **Testes**: Testes unitarios e integracao
3. **Documentacao**: README, API docs, etc.
4. **Revisao**: Code review e QA

Para cada task, inclua:
- Titulo claro e objetivo
- Tipo (development, test, documentation, review, design)
- Estimativa de complexidade (baixa, media, alta)

Formato as sugestoes em JSON ao final."""

        return await self.chat(prompt, context, include_suggestions=True)


# =============================================================================
# FACTORY FUNCTION
# =============================================================================

def create_assistant(api_key: Optional[str] = None) -> ContextualAIAssistant:
    """
    Create a new AI assistant instance.

    Args:
        api_key: Optional API key (uses environment variable if not provided)

    Returns:
        ContextualAIAssistant instance
    """
    return ContextualAIAssistant(api_key=api_key)


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Main classes
    "ContextualAIAssistant",
    "AssistantContext",
    "AssistantResponse",
    "AISuggestion",

    # Enums
    "SuggestionType",
    "ActionType",

    # Factory
    "create_assistant",
]


# =============================================================================
# TEST / DEMO
# =============================================================================

if __name__ == "__main__":
    import asyncio

    async def demo():
        print("=" * 60)
        print("  Demo: Contextual AI Assistant (Issue #280)")
        print("=" * 60)

        # Create assistant
        assistant = create_assistant()

        if not assistant.client:
            print("\n[AVISO] Cliente Claude nao configurado. Defina ANTHROPIC_API_KEY.")
            return

        # Create context
        context = AssistantContext(
            project_id="PRJ-0001",
            project_name="E-commerce Platform",
            project_status="IN_PROGRESS",
            story_id="STR-0001",
            story_title="Login com Google",
            story_status="ready",
            story_persona="usuario",
            story_action="fazer login usando minha conta Google",
            story_benefit="nao precisar criar uma nova conta",
            story_points=5,
            acceptance_criteria=[
                "Botao de login com Google visivel na tela",
                "Redireciona para autenticacao Google",
                "Cria conta automaticamente se nao existir"
            ]
        )

        print("\n1. Chat contextual:")
        print("-" * 40)
        response = await assistant.chat(
            "Quais sao os proximos passos para esta story?",
            context
        )
        print(response.message)
        if response.suggestions:
            print("\nSugestoes:")
            for sug in response.suggestions:
                print(f"  - [{sug.suggestion_type}] {sug.title}")

        print("\n2. Refinamento de Story:")
        print("-" * 40)
        refinement = await assistant.refine_story(context)
        print(refinement.message[:500] + "...")

        print("\n" + "=" * 60)

    asyncio.run(demo())
