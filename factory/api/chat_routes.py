# -*- coding: utf-8 -*-
"""
Chat API Routes - Advanced Contextual AI Assistant (Issue #280)
================================================================

Endpoints para o assistente IA contextual avancado:
- Chat contextual com projeto/story
- Sugestoes baseadas no contexto
- Explicacao de codigo
- Refinamento de stories

Versao: 1.0.0
"""

import os
import sys
from datetime import datetime
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Query, Body
from pydantic import BaseModel, Field

# Database
from factory.database.connection import SessionLocal
from factory.database.repositories import (
    ChatMessageRepository,
    ProjectRepository,
    StoryRepository,
    StoryTaskRepository
)
from factory.database.models import MessageRole

# AI Assistant
try:
    from factory.core.ai_assistant import (
        ContextualAIAssistant,
        AssistantContext,
        create_assistant
    )
    HAS_AI_ASSISTANT = True
except ImportError as e:
    print(f"[ChatRoutes] AI Assistant not available: {e}")
    HAS_AI_ASSISTANT = False


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/assistant", tags=["AI Assistant"])


# =============================================================================
# SCHEMAS
# =============================================================================

class ContextualMessageRequest(BaseModel):
    """Request for contextual chat message"""
    message: str = Field(..., min_length=1, description="User message")
    project_id: Optional[str] = Field(None, description="Project ID for context")
    story_id: Optional[str] = Field(None, description="Story ID for context")
    task_id: Optional[str] = Field(None, description="Task ID for context")
    user_id: Optional[str] = Field(None, description="User ID")
    include_suggestions: bool = Field(True, description="Include action suggestions")


class CodeExplainRequest(BaseModel):
    """Request for code explanation"""
    code: str = Field(..., min_length=1, description="Code snippet to explain")
    language: Optional[str] = Field(None, description="Programming language")
    project_id: Optional[str] = Field(None, description="Project context")
    story_id: Optional[str] = Field(None, description="Story context")


class StoryRefinementRequest(BaseModel):
    """Request for story refinement"""
    story_id: str = Field(..., description="Story ID to refine")


class TaskGenerationRequest(BaseModel):
    """Request for task generation"""
    story_id: str = Field(..., description="Story ID to generate tasks for")


class SuggestionRequest(BaseModel):
    """Request for context-based suggestions"""
    project_id: Optional[str] = Field(None, description="Project ID")
    story_id: Optional[str] = Field(None, description="Story ID")
    task_id: Optional[str] = Field(None, description="Task ID")


class SuggestionResponse(BaseModel):
    """AI suggestion response"""
    suggestion_id: str
    suggestion_type: str
    title: str
    description: str
    action_type: Optional[str] = None
    action_data: Dict[str, Any] = {}
    priority: int = 0
    confidence: float = 0.0


class AssistantMessageResponse(BaseModel):
    """Response from the AI assistant"""
    message: str
    suggestions: List[SuggestionResponse] = []
    context_used: Dict[str, Any] = {}
    confidence: float = 0.0
    tokens_used: int = 0


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def build_context_from_ids(
    db,
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    task_id: Optional[str] = None,
    user_id: Optional[str] = None
) -> AssistantContext:
    """Build AssistantContext from database IDs"""
    context = AssistantContext(user_id=user_id)

    # Load project context
    if project_id:
        project_repo = ProjectRepository(db)
        project = project_repo.get_by_id(project_id)
        if project:
            context.project_id = project.project_id
            context.project_name = project.name
            context.project_type = project.project_type
            context.project_status = project.status

    # Load story context
    if story_id:
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(story_id)
        if story:
            context.story_id = story.story_id
            context.story_title = story.title
            context.story_status = story.status
            context.story_persona = story.persona
            context.story_action = story.action
            context.story_benefit = story.benefit
            context.story_points = story.story_points
            context.acceptance_criteria = story.acceptance_criteria or []

            # If project not set, get from story
            if not project_id and story.project_id:
                project_repo = ProjectRepository(db)
                project = project_repo.get_by_id(story.project_id)
                if project:
                    context.project_id = project.project_id
                    context.project_name = project.name
                    context.project_status = project.status

    # Load task context
    if task_id:
        task_repo = StoryTaskRepository(db)
        task = task_repo.get_by_id(task_id)
        if task:
            context.task_id = task.task_id
            context.task_title = task.title
            context.task_status = task.status

    # Load conversation history
    if project_id or story_id:
        chat_repo = ChatMessageRepository(db)
        history = chat_repo.get_history(
            project_id=project_id,
            story_id=story_id,
            limit=10
        )
        context.conversation_history = [
            {"role": msg.role, "content": msg.content}
            for msg in history
        ]

    return context


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/chat", response_model=AssistantMessageResponse)
async def contextual_chat(request: ContextualMessageRequest):
    """
    Send a contextual message to the AI assistant.

    The assistant will use project, story, and task context to provide
    more relevant and helpful responses.
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        # Build context from database
        context = build_context_from_ids(
            db,
            project_id=request.project_id,
            story_id=request.story_id,
            task_id=request.task_id,
            user_id=request.user_id
        )

        # Save user message
        chat_repo = ChatMessageRepository(db)
        user_msg = chat_repo.create({
            "project_id": request.project_id,
            "story_id": request.story_id,
            "role": MessageRole.USER.value,
            "content": request.message,
            "user_id": request.user_id
        })

        # Get AI response
        assistant = create_assistant()
        response = await assistant.chat(
            message=request.message,
            context=context,
            include_suggestions=request.include_suggestions
        )

        # Save assistant response
        assistant_msg = chat_repo.create({
            "project_id": request.project_id,
            "story_id": request.story_id,
            "role": MessageRole.ASSISTANT.value,
            "content": response.message,
            "actions": [s.to_dict() for s in response.suggestions]
        })

        return AssistantMessageResponse(
            message=response.message,
            suggestions=[
                SuggestionResponse(**s.to_dict())
                for s in response.suggestions
            ],
            context_used={
                "project_id": context.project_id,
                "story_id": context.story_id,
                "task_id": context.task_id
            },
            confidence=response.confidence,
            tokens_used=response.tokens_used
        )

    finally:
        db.close()


@router.post("/suggestions", response_model=List[SuggestionResponse])
async def get_suggestions(request: SuggestionRequest):
    """
    Get AI suggestions based on current context.

    Returns a list of suggested actions based on the project/story state.
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        # Build context
        context = build_context_from_ids(
            db,
            project_id=request.project_id,
            story_id=request.story_id,
            task_id=request.task_id
        )

        # Get suggestions
        assistant = create_assistant()
        suggestions = await assistant.get_suggestions(context)

        return [SuggestionResponse(**s.to_dict()) for s in suggestions]

    finally:
        db.close()


@router.post("/explain-code", response_model=AssistantMessageResponse)
async def explain_code(request: CodeExplainRequest):
    """
    Explain a code snippet using AI.

    Provides detailed explanation of the code including:
    - Overview of what the code does
    - Key components and their purpose
    - Execution flow
    - Best practices and improvement suggestions
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        # Build context
        context = build_context_from_ids(
            db,
            project_id=request.project_id,
            story_id=request.story_id
        )

        # Get code explanation
        assistant = create_assistant()
        response = await assistant.explain_code(
            code=request.code,
            language=request.language,
            context=context
        )

        return AssistantMessageResponse(
            message=response.message,
            suggestions=[
                SuggestionResponse(**s.to_dict())
                for s in response.suggestions
            ],
            confidence=response.confidence,
            tokens_used=response.tokens_used
        )

    finally:
        db.close()


@router.post("/refine-story", response_model=AssistantMessageResponse)
async def refine_story(request: StoryRefinementRequest):
    """
    Get AI suggestions for refining a user story.

    Analyzes the story and suggests:
    - Improvements to the narrative
    - Better acceptance criteria
    - Estimation adjustments
    - Potential story splitting
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        # Get story
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(request.story_id)
        if not story:
            raise HTTPException(404, f"Story {request.story_id} not found")

        # Build context
        context = build_context_from_ids(
            db,
            project_id=story.project_id,
            story_id=request.story_id
        )

        # Get refinement suggestions
        assistant = create_assistant()
        response = await assistant.refine_story(context)

        return AssistantMessageResponse(
            message=response.message,
            suggestions=[
                SuggestionResponse(**s.to_dict())
                for s in response.suggestions
            ],
            confidence=response.confidence,
            tokens_used=response.tokens_used
        )

    finally:
        db.close()


@router.post("/generate-tasks", response_model=AssistantMessageResponse)
async def generate_tasks(request: TaskGenerationRequest):
    """
    Generate task suggestions for a user story.

    Analyzes the story and suggests tasks for:
    - Development
    - Testing
    - Documentation
    - Code review
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        # Get story
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(request.story_id)
        if not story:
            raise HTTPException(404, f"Story {request.story_id} not found")

        # Build context
        context = build_context_from_ids(
            db,
            project_id=story.project_id,
            story_id=request.story_id
        )

        # Generate tasks
        assistant = create_assistant()
        response = await assistant.generate_tasks(context)

        return AssistantMessageResponse(
            message=response.message,
            suggestions=[
                SuggestionResponse(**s.to_dict())
                for s in response.suggestions
            ],
            confidence=response.confidence,
            tokens_used=response.tokens_used
        )

    finally:
        db.close()


@router.get("/context/{story_id}")
async def get_story_context(story_id: str):
    """
    Get the full context for a story.

    Returns all context information that the AI assistant
    would use when responding to queries about this story.
    """
    if not HAS_AI_ASSISTANT:
        raise HTTPException(500, "AI Assistant module not available")

    db = SessionLocal()
    try:
        context = build_context_from_ids(db, story_id=story_id)

        if not context.story_id:
            raise HTTPException(404, f"Story {story_id} not found")

        return {
            "context": context.to_dict(),
            "summary": context.get_summary()
        }

    finally:
        db.close()


@router.get("/capabilities")
async def get_assistant_capabilities():
    """
    Get information about the AI assistant capabilities.

    Returns a list of available features and their descriptions.
    """
    return {
        "name": "Contextual AI Assistant",
        "version": "1.0.0",
        "available": HAS_AI_ASSISTANT,
        "capabilities": [
            {
                "id": "contextual_chat",
                "name": "Chat Contextual",
                "description": "Converse com o assistente considerando o contexto do projeto e story atual",
                "endpoint": "/api/assistant/chat"
            },
            {
                "id": "suggestions",
                "name": "Sugestoes Inteligentes",
                "description": "Obtenha sugestoes de acoes baseadas no estado atual do projeto",
                "endpoint": "/api/assistant/suggestions"
            },
            {
                "id": "code_explanation",
                "name": "Explicacao de Codigo",
                "description": "Receba explicacoes detalhadas sobre trechos de codigo",
                "endpoint": "/api/assistant/explain-code"
            },
            {
                "id": "story_refinement",
                "name": "Refinamento de Stories",
                "description": "Obtenha sugestoes para melhorar user stories",
                "endpoint": "/api/assistant/refine-story"
            },
            {
                "id": "task_generation",
                "name": "Geracao de Tasks",
                "description": "Gere sugestoes de tasks para uma story",
                "endpoint": "/api/assistant/generate-tasks"
            }
        ]
    }


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = ["router"]
