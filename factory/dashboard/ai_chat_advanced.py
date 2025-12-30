# -*- coding: utf-8 -*-
"""
AI Chat Advanced - Issue #280
=============================

Assistente IA contextual avancado para a Fabrica de Agentes.

Funcionalidades:
1. Chat contextual que conhece o projeto atual
2. Comandos especiais (/create story, /estimate, /suggest)
3. Historico de conversa persistente
4. Sugestoes inteligentes baseadas no contexto

Endpoints:
- POST /api/ai/chat/message - Envia mensagem
- GET /api/ai/chat/history - Historico
- POST /api/ai/chat/command - Executa comando
- GET /api/ai/chat/suggestions - Sugestoes contextuais
- DELETE /api/ai/chat/history - Limpa historico
"""

import os
import re
import uuid
import json
from datetime import datetime
from typing import Optional, List, Dict, Any
from enum import Enum
from dataclasses import dataclass, field
from pydantic import BaseModel, Field

from fastapi import FastAPI, HTTPException, Query, WebSocket, WebSocketDisconnect
from fastapi.responses import HTMLResponse
from sqlalchemy.orm import Session
from sqlalchemy import desc

# Database
from factory.database.connection import SessionLocal
from factory.database.models import (
    ChatMessage, MessageRole, Story, StoryStatus,
    Project, Epic, Sprint, StoryTask
)
from factory.database.repositories import (
    ChatMessageRepository, StoryRepository, ProjectRepository
)

# Claude AI Integration
try:
    from factory.ai.claude_integration import ClaudeClient, get_claude_client
    HAS_CLAUDE = True
except ImportError:
    HAS_CLAUDE = False
    print("[AI Chat Advanced] Claude integration not available")


# =============================================================================
# MODELS E SCHEMAS
# =============================================================================

class CommandType(str, Enum):
    """Tipos de comandos especiais do chat"""
    CREATE = "create"
    ESTIMATE = "estimate"
    SEARCH = "search"
    SUGGEST = "suggest"
    HELP = "help"
    STATUS = "status"
    ANALYZE = "analyze"
    GENERATE = "generate"


class ChatSessionStatus(str, Enum):
    """Status da sessao de chat"""
    ACTIVE = "active"
    IDLE = "idle"
    CLOSED = "closed"


@dataclass
class ChatSession:
    """
    Sessao de chat com contexto do projeto
    """
    session_id: str
    user_id: str
    project_id: Optional[str] = None
    story_id: Optional[str] = None
    tenant_id: Optional[str] = None
    context: Dict[str, Any] = field(default_factory=dict)
    status: ChatSessionStatus = ChatSessionStatus.ACTIVE
    created_at: datetime = field(default_factory=datetime.utcnow)
    last_activity: datetime = field(default_factory=datetime.utcnow)
    message_count: int = 0


@dataclass
class Command:
    """
    Comando parseado do chat
    """
    type: CommandType
    args: List[str] = field(default_factory=list)
    kwargs: Dict[str, str] = field(default_factory=dict)
    raw: str = ""

    def __post_init__(self):
        if isinstance(self.type, str):
            self.type = CommandType(self.type.lower())


class ChatMessageRequest(BaseModel):
    """Request para enviar mensagem"""
    message: str = Field(..., min_length=1, max_length=10000)
    project_id: Optional[str] = None
    story_id: Optional[str] = None
    session_id: Optional[str] = None
    user_id: Optional[str] = "anonymous"
    context: Optional[Dict[str, Any]] = None


class ChatMessageResponse(BaseModel):
    """Response de mensagem do chat"""
    message_id: str
    content: str
    role: str
    actions: List[Dict[str, Any]] = []
    suggestions: List[str] = []
    created_at: str
    tokens_used: int = 0
    command_result: Optional[Dict[str, Any]] = None


class CommandRequest(BaseModel):
    """Request para executar comando"""
    command: str = Field(..., min_length=1)
    project_id: Optional[str] = None
    story_id: Optional[str] = None
    args: Optional[Dict[str, Any]] = None


class CommandResponse(BaseModel):
    """Response de comando executado"""
    success: bool
    command_type: str
    result: Dict[str, Any]
    message: str
    suggestions: List[str] = []


class SuggestionResponse(BaseModel):
    """Response de sugestoes contextuais"""
    suggestions: List[Dict[str, Any]]
    context: Dict[str, Any]


# =============================================================================
# COMMAND PARSER
# =============================================================================

class CommandParser:
    """
    Parser de comandos especiais do chat

    Comandos suportados:
    - /create story [titulo] - Cria uma nova story
    - /create task [titulo] - Cria uma nova task
    - /estimate [story_id] - Estima story points
    - /search [termo] - Busca stories/tasks
    - /suggest - Sugestoes baseadas no contexto
    - /help - Mostra comandos disponiveis
    - /status [story_id] - Status da story
    - /analyze - Analisa o projeto atual
    - /generate [tipo] - Gera conteudo (docs, tests, etc)
    """

    COMMAND_PATTERN = r'^/(\w+)(?:\s+(.*))?$'

    # Mapeamento de aliases
    ALIASES = {
        "c": "create",
        "e": "estimate",
        "s": "search",
        "h": "help",
        "?": "help",
        "st": "status",
        "a": "analyze",
        "g": "generate",
        "sg": "suggest",
    }

    @classmethod
    def is_command(cls, text: str) -> bool:
        """Verifica se o texto e um comando"""
        return text.strip().startswith("/")

    @classmethod
    def parse(cls, text: str) -> Optional[Command]:
        """
        Parseia um comando do texto

        Args:
            text: Texto contendo o comando

        Returns:
            Comando parseado ou None se invalido
        """
        text = text.strip()
        if not cls.is_command(text):
            return None

        match = re.match(cls.COMMAND_PATTERN, text, re.IGNORECASE)
        if not match:
            return None

        cmd_name = match.group(1).lower()
        args_str = match.group(2) or ""

        # Resolve aliases
        cmd_name = cls.ALIASES.get(cmd_name, cmd_name)

        # Valida tipo de comando
        try:
            cmd_type = CommandType(cmd_name)
        except ValueError:
            return None

        # Parse argumentos
        args = []
        kwargs = {}

        if args_str:
            # Parse named arguments (--key=value ou --key value)
            named_pattern = r'--(\w+)(?:=|\s+)([^\s]+)'
            for match in re.finditer(named_pattern, args_str):
                kwargs[match.group(1)] = match.group(2)

            # Remove named arguments do texto
            clean_args = re.sub(named_pattern, '', args_str).strip()

            # Parse positional arguments
            if clean_args:
                # Trata strings entre aspas como um unico argumento
                quote_pattern = r'"([^"]+)"|\'([^\']+)\'|(\S+)'
                for match in re.finditer(quote_pattern, clean_args):
                    arg = match.group(1) or match.group(2) or match.group(3)
                    if arg:
                        args.append(arg)

        return Command(
            type=cmd_type,
            args=args,
            kwargs=kwargs,
            raw=text
        )

    @classmethod
    def get_help(cls) -> str:
        """Retorna texto de ajuda dos comandos"""
        help_text = """
**Comandos Disponiveis:**

| Comando | Descricao | Exemplo |
|---------|-----------|---------|
| `/create story [titulo]` | Cria uma nova User Story | `/create story "Login com SSO"` |
| `/create task [titulo]` | Cria uma nova Task | `/create task "Implementar API"` |
| `/estimate [story_id]` | Estima story points com IA | `/estimate STR-0001` |
| `/search [termo]` | Busca em stories e tasks | `/search autenticacao` |
| `/suggest` | Sugestoes baseadas no contexto | `/suggest` |
| `/status [story_id]` | Status detalhado da story | `/status STR-0001` |
| `/analyze` | Analisa o projeto atual | `/analyze` |
| `/generate [tipo]` | Gera conteudo (docs, tests) | `/generate tests` |
| `/help` | Mostra esta ajuda | `/help` |

**Aliases:**
- `/c` = `/create`
- `/e` = `/estimate`
- `/s` = `/search`
- `/h` ou `/?` = `/help`
- `/st` = `/status`
- `/a` = `/analyze`
- `/g` = `/generate`
- `/sg` = `/suggest`

**Dicas:**
- Use aspas para argumentos com espacos: `/create story "Minha nova story"`
- Use --key=value para argumentos nomeados: `/estimate STR-0001 --method=fibonacci`
"""
        return help_text.strip()


# =============================================================================
# CONTEXT BUILDER
# =============================================================================

class ContextBuilder:
    """
    Constroi contexto do projeto para o assistente IA
    """

    def __init__(self, db: Session):
        self.db = db
        self.project_repo = ProjectRepository(db)
        self.story_repo = StoryRepository(db)

    def build_project_context(self, project_id: str) -> Dict[str, Any]:
        """
        Constroi contexto completo do projeto
        """
        project = self.project_repo.get_by_id(project_id)
        if not project:
            return {"error": "Projeto nao encontrado"}

        # Busca stories do projeto
        stories = self.story_repo.get_by_project(project_id)

        # Estatisticas
        status_counts = {}
        total_points = 0
        completed_points = 0

        for story in stories:
            status = story.status
            status_counts[status] = status_counts.get(status, 0) + 1
            points = story.story_points or 0
            total_points += points
            if status == StoryStatus.DONE.value:
                completed_points += points

        # Stories recentes
        recent_stories = sorted(stories, key=lambda s: s.updated_at or s.created_at, reverse=True)[:5]

        context = {
            "project": {
                "id": project.project_id,
                "name": project.name,
                "description": project.description,
                "status": project.status,
                "type": project.project_type,
                "progress": project.progress,
            },
            "stories": {
                "total": len(stories),
                "by_status": status_counts,
                "total_points": total_points,
                "completed_points": completed_points,
                "velocity": completed_points if total_points > 0 else 0,
            },
            "recent_stories": [
                {
                    "id": s.story_id,
                    "title": s.title,
                    "status": s.status,
                    "points": s.story_points,
                    "progress": s.progress,
                }
                for s in recent_stories
            ],
            "summary": f"Projeto '{project.name}' com {len(stories)} stories ({completed_points}/{total_points} pontos concluidos)"
        }

        return context

    def build_story_context(self, story_id: str) -> Dict[str, Any]:
        """
        Constroi contexto detalhado de uma story
        """
        story = self.story_repo.get_by_id(story_id)
        if not story:
            return {"error": "Story nao encontrada"}

        # Tasks da story
        tasks = list(story.story_tasks) if story.story_tasks else []
        tasks_completed = sum(1 for t in tasks if t.status == "completed")

        context = {
            "story": {
                "id": story.story_id,
                "title": story.title,
                "description": story.description,
                "persona": story.persona,
                "action": story.action,
                "benefit": story.benefit,
                "status": story.status,
                "priority": story.priority,
                "story_points": story.story_points,
                "complexity": story.complexity,
                "acceptance_criteria": story.acceptance_criteria or [],
                "definition_of_done": story.definition_of_done or [],
                "business_rules": story.business_rules or [],
                "technical_notes": story.technical_notes,
                "progress": story.progress,
            },
            "tasks": {
                "total": len(tasks),
                "completed": tasks_completed,
                "items": [
                    {
                        "id": t.task_id,
                        "title": t.title,
                        "status": t.status,
                        "type": t.task_type,
                        "progress": t.progress,
                    }
                    for t in tasks
                ]
            },
            "narrative": f"Como um {story.persona or 'usuario'}, eu quero {story.action or 'funcionalidade'}, para que {story.benefit or 'beneficio'}",
            "summary": f"Story '{story.title}' - {story.status} ({tasks_completed}/{len(tasks)} tasks concluidas)"
        }

        return context

    def build_full_context(self, project_id: str = None, story_id: str = None) -> Dict[str, Any]:
        """
        Constroi contexto completo baseado nos IDs fornecidos
        """
        context = {
            "timestamp": datetime.utcnow().isoformat(),
            "has_project": project_id is not None,
            "has_story": story_id is not None,
        }

        if project_id:
            context["project_context"] = self.build_project_context(project_id)

        if story_id:
            context["story_context"] = self.build_story_context(story_id)

        return context


# =============================================================================
# COMMAND EXECUTOR
# =============================================================================

class CommandExecutor:
    """
    Executor de comandos do chat
    """

    def __init__(self, db: Session, claude_client=None):
        self.db = db
        self.claude = claude_client
        self.project_repo = ProjectRepository(db)
        self.story_repo = StoryRepository(db)
        self.context_builder = ContextBuilder(db)

    def execute(self, command: Command, project_id: str = None,
                story_id: str = None, **kwargs) -> CommandResponse:
        """
        Executa um comando
        """
        handlers = {
            CommandType.CREATE: self._handle_create,
            CommandType.ESTIMATE: self._handle_estimate,
            CommandType.SEARCH: self._handle_search,
            CommandType.SUGGEST: self._handle_suggest,
            CommandType.HELP: self._handle_help,
            CommandType.STATUS: self._handle_status,
            CommandType.ANALYZE: self._handle_analyze,
            CommandType.GENERATE: self._handle_generate,
        }

        handler = handlers.get(command.type)
        if not handler:
            return CommandResponse(
                success=False,
                command_type=command.type.value,
                result={},
                message=f"Comando '{command.type.value}' nao implementado"
            )

        try:
            return handler(command, project_id, story_id, **kwargs)
        except Exception as e:
            return CommandResponse(
                success=False,
                command_type=command.type.value,
                result={"error": str(e)},
                message=f"Erro ao executar comando: {str(e)}"
            )

    def _handle_create(self, command: Command, project_id: str,
                       story_id: str, **kwargs) -> CommandResponse:
        """Handler para /create"""
        if not command.args:
            return CommandResponse(
                success=False,
                command_type="create",
                result={},
                message="Especifique o que criar: /create story [titulo] ou /create task [titulo]",
                suggestions=["/create story \"Nova Story\"", "/create task \"Nova Task\""]
            )

        create_type = command.args[0].lower()
        title = " ".join(command.args[1:]) if len(command.args) > 1 else None

        if create_type == "story":
            if not project_id:
                return CommandResponse(
                    success=False,
                    command_type="create",
                    result={},
                    message="Selecione um projeto para criar a story"
                )

            if not title:
                return CommandResponse(
                    success=False,
                    command_type="create",
                    result={},
                    message="Especifique o titulo da story: /create story \"Titulo da Story\""
                )

            # Cria a story
            story_data = {
                "project_id": project_id,
                "title": title,
                "status": StoryStatus.BACKLOG.value,
            }

            new_story = self.story_repo.create(story_data)

            return CommandResponse(
                success=True,
                command_type="create",
                result={
                    "story_id": new_story.story_id,
                    "title": new_story.title,
                    "status": new_story.status,
                },
                message=f"Story '{new_story.story_id}' criada com sucesso!",
                suggestions=[
                    f"/estimate {new_story.story_id}",
                    f"/status {new_story.story_id}",
                ]
            )

        elif create_type == "task":
            if not story_id:
                return CommandResponse(
                    success=False,
                    command_type="create",
                    result={},
                    message="Selecione uma story para criar a task"
                )

            if not title:
                return CommandResponse(
                    success=False,
                    command_type="create",
                    result={},
                    message="Especifique o titulo da task: /create task \"Titulo da Task\""
                )

            # Cria a task
            from factory.database.repositories import StoryTaskRepository
            task_repo = StoryTaskRepository(self.db)

            task_data = {
                "story_id": story_id,
                "title": title,
                "status": "pending",
                "task_type": "development",
            }

            new_task = task_repo.create(task_data)

            return CommandResponse(
                success=True,
                command_type="create",
                result={
                    "task_id": new_task.task_id,
                    "title": new_task.title,
                    "story_id": story_id,
                },
                message=f"Task '{new_task.task_id}' criada com sucesso!",
                suggestions=[f"/status {story_id}"]
            )

        return CommandResponse(
            success=False,
            command_type="create",
            result={},
            message=f"Tipo '{create_type}' nao suportado. Use 'story' ou 'task'."
        )

    def _handle_estimate(self, command: Command, project_id: str,
                         story_id: str, **kwargs) -> CommandResponse:
        """Handler para /estimate"""
        target_story_id = command.args[0] if command.args else story_id

        if not target_story_id:
            return CommandResponse(
                success=False,
                command_type="estimate",
                result={},
                message="Especifique a story: /estimate STR-0001"
            )

        story = self.story_repo.get_by_id(target_story_id)
        if not story:
            return CommandResponse(
                success=False,
                command_type="estimate",
                result={},
                message=f"Story '{target_story_id}' nao encontrada"
            )

        # Estima com IA se disponivel
        if self.claude and self.claude.is_available():
            context = self.context_builder.build_story_context(target_story_id)

            prompt = f"""Analise esta User Story e sugira Story Points (Fibonacci: 1, 2, 3, 5, 8, 13, 21):

**Story:** {story.title}
**Descricao:** {story.description or 'N/A'}
**Narrativa:** Como um {story.persona or 'usuario'}, eu quero {story.action or 'funcionalidade'}, para que {story.benefit or 'beneficio'}
**Criterios de Aceite:** {json.dumps(story.acceptance_criteria or [], ensure_ascii=False)}
**Tasks:** {len(story.story_tasks or [])} tasks

Responda em formato JSON:
{{
    "story_points": <numero>,
    "complexity": "<low|medium|high|very_high>",
    "reasoning": "<justificativa breve>",
    "confidence": <0.0-1.0>
}}
"""

            system_prompt = """Voce e um Scrum Master experiente especializado em estimativas agile.
Analise stories e forneca estimativas precisas baseadas em complexidade tecnica,
incertezas, dependencias e esforco de desenvolvimento."""

            response = self.claude.chat(prompt, system_prompt=system_prompt)

            if response.success:
                try:
                    # Parse JSON da resposta
                    json_match = re.search(r'\{[^{}]+\}', response.content, re.DOTALL)
                    if json_match:
                        estimate = json.loads(json_match.group())

                        return CommandResponse(
                            success=True,
                            command_type="estimate",
                            result={
                                "story_id": target_story_id,
                                "current_points": story.story_points,
                                "suggested_points": estimate.get("story_points"),
                                "complexity": estimate.get("complexity"),
                                "reasoning": estimate.get("reasoning"),
                                "confidence": estimate.get("confidence"),
                            },
                            message=f"Sugestao: **{estimate.get('story_points')} pontos** ({estimate.get('complexity')}) - {estimate.get('reasoning')}",
                            suggestions=[
                                f"Aceitar estimativa: atualizar story {target_story_id} com {estimate.get('story_points')} pontos"
                            ]
                        )
                except json.JSONDecodeError:
                    pass

        # Fallback sem IA
        # Estima baseado em complexidade e numero de tasks
        task_count = len(story.story_tasks or [])
        ac_count = len(story.acceptance_criteria or [])

        if task_count <= 2 and ac_count <= 3:
            suggested_points = 2
            complexity = "low"
        elif task_count <= 5 and ac_count <= 6:
            suggested_points = 5
            complexity = "medium"
        elif task_count <= 8:
            suggested_points = 8
            complexity = "high"
        else:
            suggested_points = 13
            complexity = "very_high"

        return CommandResponse(
            success=True,
            command_type="estimate",
            result={
                "story_id": target_story_id,
                "current_points": story.story_points,
                "suggested_points": suggested_points,
                "complexity": complexity,
                "method": "heuristic",
            },
            message=f"Estimativa heuristica: **{suggested_points} pontos** ({complexity}) baseado em {task_count} tasks e {ac_count} criterios de aceite",
            suggestions=["Conecte a API Claude para estimativas mais precisas"]
        )

    def _handle_search(self, command: Command, project_id: str,
                       story_id: str, **kwargs) -> CommandResponse:
        """Handler para /search"""
        if not command.args:
            return CommandResponse(
                success=False,
                command_type="search",
                result={},
                message="Especifique o termo de busca: /search [termo]"
            )

        search_term = " ".join(command.args).lower()

        # Busca em stories
        stories = self.story_repo.search(search_term, project_id=project_id, limit=10)

        results = []
        for story in stories:
            results.append({
                "type": "story",
                "id": story.story_id,
                "title": story.title,
                "status": story.status,
                "match_score": 1.0,  # Placeholder
            })

        return CommandResponse(
            success=True,
            command_type="search",
            result={
                "term": search_term,
                "count": len(results),
                "results": results,
            },
            message=f"Encontrados {len(results)} resultados para '{search_term}'",
            suggestions=[f"/status {r['id']}" for r in results[:3]]
        )

    def _handle_suggest(self, command: Command, project_id: str,
                        story_id: str, **kwargs) -> CommandResponse:
        """Handler para /suggest"""
        suggestions = []

        if project_id:
            context = self.context_builder.build_project_context(project_id)

            # Sugestoes baseadas no estado do projeto
            status_counts = context.get("stories", {}).get("by_status", {})

            backlog_count = status_counts.get("backlog", 0)
            in_progress_count = status_counts.get("in_progress", 0)
            review_count = status_counts.get("review", 0)

            if backlog_count > 10:
                suggestions.append({
                    "type": "alert",
                    "priority": "medium",
                    "message": f"Backlog grande ({backlog_count} stories). Considere priorizar e refinar.",
                    "action": "/search status:backlog"
                })

            if in_progress_count > 5:
                suggestions.append({
                    "type": "alert",
                    "priority": "high",
                    "message": f"Muitas stories em progresso ({in_progress_count}). Considere limitar WIP.",
                    "action": "/search status:in_progress"
                })

            if review_count > 3:
                suggestions.append({
                    "type": "action",
                    "priority": "high",
                    "message": f"{review_count} stories aguardando review. Priorize code review!",
                    "action": "/search status:review"
                })

            # Sugestoes de melhoria
            for story in context.get("recent_stories", []):
                if story.get("points") == 0:
                    suggestions.append({
                        "type": "improvement",
                        "priority": "low",
                        "message": f"Story '{story['id']}' sem estimativa. Considere estimar.",
                        "action": f"/estimate {story['id']}"
                    })

        if story_id:
            story_context = self.context_builder.build_story_context(story_id)
            story = story_context.get("story", {})

            if not story.get("acceptance_criteria"):
                suggestions.append({
                    "type": "improvement",
                    "priority": "high",
                    "message": "Story sem criterios de aceite. Adicione para clareza.",
                    "action": "Editar story e adicionar criterios"
                })

            if not story.get("persona"):
                suggestions.append({
                    "type": "improvement",
                    "priority": "medium",
                    "message": "Narrativa incompleta. Defina persona, acao e beneficio.",
                    "action": "Editar story e completar narrativa"
                })

        if not suggestions:
            suggestions.append({
                "type": "info",
                "priority": "low",
                "message": "Projeto em bom estado! Continue o bom trabalho.",
                "action": None
            })

        return CommandResponse(
            success=True,
            command_type="suggest",
            result={
                "suggestions": suggestions,
                "context": {
                    "project_id": project_id,
                    "story_id": story_id,
                }
            },
            message=f"{len(suggestions)} sugestoes geradas",
            suggestions=[s.get("action") for s in suggestions if s.get("action")]
        )

    def _handle_help(self, command: Command, project_id: str,
                     story_id: str, **kwargs) -> CommandResponse:
        """Handler para /help"""
        help_text = CommandParser.get_help()

        return CommandResponse(
            success=True,
            command_type="help",
            result={"help_text": help_text},
            message=help_text,
            suggestions=["/suggest", "/analyze", "/search"]
        )

    def _handle_status(self, command: Command, project_id: str,
                       story_id: str, **kwargs) -> CommandResponse:
        """Handler para /status"""
        target_story_id = command.args[0] if command.args else story_id

        if not target_story_id:
            # Status do projeto
            if project_id:
                context = self.context_builder.build_project_context(project_id)

                return CommandResponse(
                    success=True,
                    command_type="status",
                    result=context,
                    message=context.get("summary", "Status do projeto"),
                    suggestions=["/analyze", "/suggest"]
                )

            return CommandResponse(
                success=False,
                command_type="status",
                result={},
                message="Especifique a story ou selecione um projeto"
            )

        # Status da story
        context = self.context_builder.build_story_context(target_story_id)

        if "error" in context:
            return CommandResponse(
                success=False,
                command_type="status",
                result={},
                message=context["error"]
            )

        return CommandResponse(
            success=True,
            command_type="status",
            result=context,
            message=context.get("summary", "Status da story"),
            suggestions=[f"/estimate {target_story_id}", "/suggest"]
        )

    def _handle_analyze(self, command: Command, project_id: str,
                        story_id: str, **kwargs) -> CommandResponse:
        """Handler para /analyze"""
        if not project_id:
            return CommandResponse(
                success=False,
                command_type="analyze",
                result={},
                message="Selecione um projeto para analisar"
            )

        context = self.context_builder.build_project_context(project_id)

        # Analise basica
        stories = context.get("stories", {})
        analysis = {
            "health_score": 0,
            "issues": [],
            "strengths": [],
            "recommendations": [],
        }

        total_stories = stories.get("total", 0)
        status_counts = stories.get("by_status", {})

        # Calcula health score
        done_count = status_counts.get("done", 0)
        if total_stories > 0:
            completion_rate = done_count / total_stories
            analysis["health_score"] = int(completion_rate * 100)

        # Identifica issues
        backlog = status_counts.get("backlog", 0)
        in_progress = status_counts.get("in_progress", 0)

        if backlog > total_stories * 0.6:
            analysis["issues"].append("Backlog muito grande em relacao ao total")

        if in_progress > 5:
            analysis["issues"].append("Muitas stories em progresso simultaneamente")

        # Pontos fortes
        if completion_rate > 0.7:
            analysis["strengths"].append("Alta taxa de conclusao")

        if stories.get("completed_points", 0) > 20:
            analysis["strengths"].append("Boa velocidade do time")

        # Recomendacoes
        if not analysis["issues"]:
            analysis["recommendations"].append("Continue monitorando metricas")
        else:
            if "Backlog muito grande" in str(analysis["issues"]):
                analysis["recommendations"].append("Faca uma sessao de refinamento")
            if "Muitas stories em progresso" in str(analysis["issues"]):
                analysis["recommendations"].append("Implemente limites de WIP")

        return CommandResponse(
            success=True,
            command_type="analyze",
            result={
                "project_context": context,
                "analysis": analysis,
            },
            message=f"Health Score: **{analysis['health_score']}%** - {len(analysis['issues'])} issues, {len(analysis['strengths'])} pontos fortes",
            suggestions=["/suggest", "/search status:in_progress"]
        )

    def _handle_generate(self, command: Command, project_id: str,
                         story_id: str, **kwargs) -> CommandResponse:
        """Handler para /generate"""
        if not command.args:
            return CommandResponse(
                success=False,
                command_type="generate",
                result={},
                message="Especifique o que gerar: /generate [docs|tests|acceptance|tasks]",
                suggestions=["/generate docs", "/generate tests", "/generate acceptance", "/generate tasks"]
            )

        gen_type = command.args[0].lower()

        if gen_type in ["docs", "documentation"]:
            if not story_id:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message="Selecione uma story para gerar documentacao"
                )

            story = self.story_repo.get_by_id(story_id)
            if not story:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message=f"Story '{story_id}' nao encontrada"
                )

            # Gera documentacao basica
            doc_content = f"""# {story.title}

## Narrativa
Como um **{story.persona or 'usuario'}**, eu quero **{story.action or 'funcionalidade'}**, para que **{story.benefit or 'beneficio'}**.

## Descricao
{story.description or 'Sem descricao'}

## Criterios de Aceite
{chr(10).join('- ' + ac for ac in (story.acceptance_criteria or ['Nenhum criterio definido']))}

## Definition of Done
{chr(10).join('- ' + dod for dod in (story.definition_of_done or ['Nenhum DoD definido']))}

## Notas Tecnicas
{story.technical_notes or 'Sem notas tecnicas'}
"""

            return CommandResponse(
                success=True,
                command_type="generate",
                result={
                    "type": "documentation",
                    "story_id": story_id,
                    "content": doc_content,
                },
                message="Documentacao gerada com sucesso!",
                suggestions=["/generate tests", "/generate acceptance"]
            )

        elif gen_type in ["tests", "test"]:
            if not story_id:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message="Selecione uma story para gerar testes"
                )

            story = self.story_repo.get_by_id(story_id)
            if not story:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message=f"Story '{story_id}' nao encontrada"
                )

            # Gera casos de teste baseados nos criterios de aceite
            test_cases = []
            for i, ac in enumerate(story.acceptance_criteria or [], 1):
                test_cases.append({
                    "id": f"TC-{i:03d}",
                    "name": f"Teste: {ac[:50]}...",
                    "criteria": ac,
                    "steps": ["Dado que...", "Quando...", "Entao..."],
                    "expected": "Verificar que o criterio e atendido",
                })

            return CommandResponse(
                success=True,
                command_type="generate",
                result={
                    "type": "test_cases",
                    "story_id": story_id,
                    "test_cases": test_cases,
                    "count": len(test_cases),
                },
                message=f"{len(test_cases)} casos de teste gerados baseados nos criterios de aceite",
                suggestions=["/generate docs"]
            )

        elif gen_type in ["acceptance", "ac", "criteria"]:
            if not story_id:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message="Selecione uma story para gerar criterios de aceite"
                )

            story = self.story_repo.get_by_id(story_id)
            if not story:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message=f"Story '{story_id}' nao encontrada"
                )

            # Gera sugestoes de criterios baseados na narrativa
            suggested_ac = []

            if story.action:
                suggested_ac.append(f"O usuario consegue {story.action.lower()}")
                suggested_ac.append(f"O sistema valida os dados de entrada")
                suggested_ac.append(f"O sistema exibe feedback apropriado")

            if story.benefit:
                suggested_ac.append(f"O beneficio '{story.benefit}' e alcancado")

            suggested_ac.append("A funcionalidade e responsiva em dispositivos moveis")
            suggested_ac.append("A funcionalidade segue os padroes de acessibilidade WCAG")

            return CommandResponse(
                success=True,
                command_type="generate",
                result={
                    "type": "acceptance_criteria",
                    "story_id": story_id,
                    "suggested_criteria": suggested_ac,
                    "current_criteria": story.acceptance_criteria or [],
                },
                message=f"{len(suggested_ac)} criterios de aceite sugeridos",
                suggestions=["/generate tests", "/generate docs"]
            )

        elif gen_type == "tasks":
            if not story_id:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message="Selecione uma story para gerar tasks"
                )

            story = self.story_repo.get_by_id(story_id)
            if not story:
                return CommandResponse(
                    success=False,
                    command_type="generate",
                    result={},
                    message=f"Story '{story_id}' nao encontrada"
                )

            # Gera tasks baseadas no tipo de story
            suggested_tasks = [
                {"title": "Analise tecnica e design", "type": "design"},
                {"title": "Implementacao do backend", "type": "development"},
                {"title": "Implementacao do frontend", "type": "development"},
                {"title": "Testes unitarios", "type": "test"},
                {"title": "Testes de integracao", "type": "test"},
                {"title": "Code review", "type": "review"},
                {"title": "Documentacao", "type": "documentation"},
            ]

            return CommandResponse(
                success=True,
                command_type="generate",
                result={
                    "type": "tasks",
                    "story_id": story_id,
                    "suggested_tasks": suggested_tasks,
                },
                message=f"{len(suggested_tasks)} tasks sugeridas para a story",
                suggestions=["/create task \"Nome da Task\""]
            )

        return CommandResponse(
            success=False,
            command_type="generate",
            result={},
            message=f"Tipo '{gen_type}' nao suportado",
            suggestions=["/generate docs", "/generate tests", "/generate acceptance", "/generate tasks"]
        )


# =============================================================================
# AI CHAT SERVICE
# =============================================================================

class AIChatService:
    """
    Servico principal do chat IA contextual
    """

    # System prompt para o assistente
    SYSTEM_PROMPT = """Voce e um assistente de desenvolvimento Agile experiente para a Fabrica de Agentes.

Seu papel e ajudar desenvolvedores e POs com:
1. Criacao e refinamento de User Stories
2. Estimativas de Story Points
3. Sugestoes de melhorias no processo Agile
4. Resolucao de duvidas tecnicas
5. Analise de progresso do projeto

Voce tem acesso ao contexto do projeto atual, incluindo:
- Stories e seu status
- Tasks e progresso
- Metricas do sprint
- Historico de chat

Responda de forma concisa e objetiva. Use Markdown para formatacao.
Quando apropriado, sugira comandos que o usuario pode executar (ex: /estimate, /create).

Comandos disponiveis:
- /create story [titulo] - Cria nova story
- /create task [titulo] - Cria nova task
- /estimate [story_id] - Estima story points
- /search [termo] - Busca stories/tasks
- /suggest - Sugestoes contextuais
- /help - Lista de comandos
"""

    def __init__(self, db: Session):
        self.db = db
        self.chat_repo = ChatMessageRepository(db)
        self.context_builder = ContextBuilder(db)

        # Claude client
        self.claude = None
        if HAS_CLAUDE:
            try:
                self.claude = get_claude_client()
            except Exception:
                self.claude = ClaudeClient()

        self.command_executor = CommandExecutor(db, self.claude)

        # Cache de sessoes
        self._sessions: Dict[str, ChatSession] = {}

    def get_or_create_session(self, session_id: str = None, user_id: str = "anonymous",
                              project_id: str = None, story_id: str = None,
                              tenant_id: str = None) -> ChatSession:
        """
        Obtem ou cria uma sessao de chat
        """
        if session_id and session_id in self._sessions:
            session = self._sessions[session_id]
            session.last_activity = datetime.utcnow()
            return session

        # Cria nova sessao
        new_session = ChatSession(
            session_id=session_id or f"SES-{uuid.uuid4().hex[:8].upper()}",
            user_id=user_id,
            project_id=project_id,
            story_id=story_id,
            tenant_id=tenant_id,
        )

        self._sessions[new_session.session_id] = new_session
        return new_session

    def process_message(self, request: ChatMessageRequest) -> ChatMessageResponse:
        """
        Processa uma mensagem do usuario
        """
        # Obtem/cria sessao
        session = self.get_or_create_session(
            session_id=request.session_id,
            user_id=request.user_id,
            project_id=request.project_id,
            story_id=request.story_id,
        )
        session.message_count += 1

        # Salva mensagem do usuario
        user_message = self.chat_repo.create({
            "project_id": request.project_id,
            "story_id": request.story_id,
            "role": MessageRole.USER.value,
            "content": request.message,
            "user_id": request.user_id,
        })

        # Verifica se e um comando
        if CommandParser.is_command(request.message):
            command = CommandParser.parse(request.message)
            if command:
                result = self.command_executor.execute(
                    command,
                    project_id=request.project_id,
                    story_id=request.story_id,
                )

                # Salva resposta do comando
                assistant_message = self.chat_repo.create({
                    "project_id": request.project_id,
                    "story_id": request.story_id,
                    "role": MessageRole.ASSISTANT.value,
                    "content": result.message,
                    "actions": [{"type": "command", "command": command.type.value, "result": result.result}],
                })

                return ChatMessageResponse(
                    message_id=assistant_message.message_id,
                    content=result.message,
                    role=MessageRole.ASSISTANT.value,
                    actions=assistant_message.actions or [],
                    suggestions=result.suggestions,
                    created_at=assistant_message.created_at.isoformat(),
                    command_result=result.result,
                )

        # Processa com IA
        response_content = self._generate_ai_response(request, session)

        # Salva resposta do assistente
        assistant_message = self.chat_repo.create({
            "project_id": request.project_id,
            "story_id": request.story_id,
            "role": MessageRole.ASSISTANT.value,
            "content": response_content["content"],
            "actions": response_content.get("actions", []),
        })

        return ChatMessageResponse(
            message_id=assistant_message.message_id,
            content=response_content["content"],
            role=MessageRole.ASSISTANT.value,
            actions=response_content.get("actions", []),
            suggestions=response_content.get("suggestions", []),
            created_at=assistant_message.created_at.isoformat(),
            tokens_used=response_content.get("tokens_used", 0),
        )

    def _generate_ai_response(self, request: ChatMessageRequest,
                              session: ChatSession) -> Dict[str, Any]:
        """
        Gera resposta usando Claude IA
        """
        # Constroi contexto
        context = self.context_builder.build_full_context(
            project_id=request.project_id,
            story_id=request.story_id,
        )

        # Busca historico recente
        history = self.chat_repo.get_history(
            project_id=request.project_id,
            story_id=request.story_id,
            limit=10,
        )

        # Formata contexto para o prompt
        context_str = json.dumps(context, ensure_ascii=False, indent=2)

        # Monta mensagens para Claude
        messages = []
        for msg in history[-6:]:  # Ultimas 6 mensagens
            messages.append({
                "role": msg.role,
                "content": msg.content,
            })

        # Adiciona mensagem atual
        full_message = f"""Contexto Atual:
```json
{context_str}
```

Mensagem do Usuario:
{request.message}

Responda de forma util e contextualizada."""

        messages.append({
            "role": "user",
            "content": full_message,
        })

        # Chama Claude
        if self.claude and self.claude.is_available():
            response = self.claude.chat(
                message=full_message,
                system_prompt=self.SYSTEM_PROMPT,
                context=messages[:-1],  # Historico sem a mensagem atual
            )

            if response.success:
                # Extrai sugestoes da resposta
                suggestions = self._extract_suggestions(response.content)

                return {
                    "content": response.content,
                    "tokens_used": response.tokens_used,
                    "suggestions": suggestions,
                    "actions": [],
                }

        # Fallback sem IA
        return {
            "content": self._generate_fallback_response(request, context),
            "tokens_used": 0,
            "suggestions": ["/help", "/suggest"],
            "actions": [],
        }

    def _extract_suggestions(self, content: str) -> List[str]:
        """
        Extrai sugestoes de comandos da resposta
        """
        suggestions = []
        command_pattern = r'/\w+(?:\s+\S+)*'
        matches = re.findall(command_pattern, content)
        for match in matches[:3]:  # Maximo 3 sugestoes
            suggestions.append(match)

        if not suggestions:
            suggestions = ["/suggest", "/help"]

        return suggestions

    def _generate_fallback_response(self, request: ChatMessageRequest,
                                    context: Dict[str, Any]) -> str:
        """
        Gera resposta fallback quando IA nao esta disponivel
        """
        message_lower = request.message.lower()

        if "help" in message_lower or "ajuda" in message_lower:
            return CommandParser.get_help()

        if "status" in message_lower or "como esta" in message_lower:
            if "project_context" in context:
                return context["project_context"].get("summary", "Projeto em andamento")
            return "Selecione um projeto para ver o status."

        if "story" in message_lower or "historia" in message_lower:
            return "Use `/create story \"Titulo\"` para criar uma nova story, ou `/search [termo]` para buscar."

        if "estim" in message_lower:
            return "Use `/estimate [story_id]` para estimar story points de uma story."

        return f"""Desculpe, a IA nao esta disponivel no momento para responder sua pergunta de forma completa.

Voce pode usar comandos para interagir:
- `/help` - Ver todos os comandos
- `/suggest` - Obter sugestoes contextuais
- `/search [termo]` - Buscar stories
- `/status` - Ver status do projeto

Seu projeto atual: {context.get('project_context', {}).get('project', {}).get('name', 'Nenhum selecionado')}
"""

    def get_history(self, project_id: str = None, story_id: str = None,
                    tenant_id: str = None, limit: int = 50) -> List[Dict[str, Any]]:
        """
        Retorna historico de mensagens
        """
        messages = self.chat_repo.get_history(
            project_id=project_id,
            story_id=story_id,
            tenant_id=tenant_id,
            limit=limit,
        )

        return [msg.to_dict() for msg in messages]

    def clear_history(self, project_id: str = None, story_id: str = None,
                      tenant_id: str = None) -> int:
        """
        Limpa historico de mensagens
        """
        return self.chat_repo.clear_history(
            project_id=project_id,
            story_id=story_id,
            tenant_id=tenant_id,
        )

    def get_suggestions(self, project_id: str = None,
                        story_id: str = None) -> SuggestionResponse:
        """
        Retorna sugestoes contextuais
        """
        command = Command(type=CommandType.SUGGEST)
        result = self.command_executor.execute(
            command,
            project_id=project_id,
            story_id=story_id,
        )

        return SuggestionResponse(
            suggestions=result.result.get("suggestions", []),
            context=result.result.get("context", {}),
        )


# =============================================================================
# FAST API ENDPOINTS
# =============================================================================

def register_ai_chat_endpoints(app: FastAPI):
    """
    Registra os endpoints do AI Chat Advanced
    """

    @app.post("/api/ai/chat/message", response_model=ChatMessageResponse, tags=["AI Chat"])
    def send_chat_message(request: ChatMessageRequest):
        """
        Envia uma mensagem para o assistente IA

        Aceita texto livre ou comandos especiais iniciados com /
        """
        db = SessionLocal()
        try:
            service = AIChatService(db)
            return service.process_message(request)
        finally:
            db.close()

    @app.get("/api/ai/chat/history", tags=["AI Chat"])
    def get_chat_history(
        project_id: Optional[str] = Query(None, description="ID do projeto"),
        story_id: Optional[str] = Query(None, description="ID da story"),
        tenant_id: Optional[str] = Query(None, description="ID do tenant"),
        limit: int = Query(50, ge=1, le=200, description="Limite de mensagens"),
    ):
        """
        Retorna historico de mensagens do chat
        """
        db = SessionLocal()
        try:
            service = AIChatService(db)
            return {
                "messages": service.get_history(
                    project_id=project_id,
                    story_id=story_id,
                    tenant_id=tenant_id,
                    limit=limit,
                ),
                "count": limit,
            }
        finally:
            db.close()

    @app.post("/api/ai/chat/command", response_model=CommandResponse, tags=["AI Chat"])
    def execute_command(request: CommandRequest):
        """
        Executa um comando especial do chat

        Comandos disponiveis:
        - /create story [titulo]
        - /create task [titulo]
        - /estimate [story_id]
        - /search [termo]
        - /suggest
        - /help
        - /status [story_id]
        - /analyze
        - /generate [tipo]
        """
        db = SessionLocal()
        try:
            command = CommandParser.parse(request.command)
            if not command:
                raise HTTPException(status_code=400, detail=f"Comando invalido: {request.command}")

            service = AIChatService(db)
            return service.command_executor.execute(
                command,
                project_id=request.project_id,
                story_id=request.story_id,
                **(request.args or {}),
            )
        finally:
            db.close()

    @app.get("/api/ai/chat/suggestions", response_model=SuggestionResponse, tags=["AI Chat"])
    def get_suggestions(
        project_id: Optional[str] = Query(None, description="ID do projeto"),
        story_id: Optional[str] = Query(None, description="ID da story"),
    ):
        """
        Retorna sugestoes contextuais baseadas no estado atual do projeto/story
        """
        db = SessionLocal()
        try:
            service = AIChatService(db)
            return service.get_suggestions(
                project_id=project_id,
                story_id=story_id,
            )
        finally:
            db.close()

    @app.delete("/api/ai/chat/history", tags=["AI Chat"])
    def clear_chat_history(
        project_id: Optional[str] = Query(None, description="ID do projeto"),
        story_id: Optional[str] = Query(None, description="ID da story"),
        tenant_id: Optional[str] = Query(None, description="ID do tenant"),
    ):
        """
        Limpa historico de mensagens do chat
        """
        db = SessionLocal()
        try:
            service = AIChatService(db)
            count = service.clear_history(
                project_id=project_id,
                story_id=story_id,
                tenant_id=tenant_id,
            )
            return {
                "success": True,
                "deleted_count": count,
                "message": f"{count} mensagens removidas",
            }
        finally:
            db.close()

    @app.get("/api/ai/chat/commands", tags=["AI Chat"])
    def list_commands():
        """
        Lista todos os comandos disponiveis
        """
        return {
            "commands": [
                {
                    "command": "/create story [titulo]",
                    "description": "Cria uma nova User Story",
                    "aliases": ["/c story"],
                },
                {
                    "command": "/create task [titulo]",
                    "description": "Cria uma nova Task",
                    "aliases": ["/c task"],
                },
                {
                    "command": "/estimate [story_id]",
                    "description": "Estima story points com IA",
                    "aliases": ["/e"],
                },
                {
                    "command": "/search [termo]",
                    "description": "Busca em stories e tasks",
                    "aliases": ["/s"],
                },
                {
                    "command": "/suggest",
                    "description": "Sugestoes contextuais",
                    "aliases": ["/sg"],
                },
                {
                    "command": "/help",
                    "description": "Mostra comandos disponiveis",
                    "aliases": ["/h", "/?"],
                },
                {
                    "command": "/status [story_id]",
                    "description": "Status detalhado",
                    "aliases": ["/st"],
                },
                {
                    "command": "/analyze",
                    "description": "Analisa o projeto",
                    "aliases": ["/a"],
                },
                {
                    "command": "/generate [tipo]",
                    "description": "Gera conteudo (docs, tests, etc)",
                    "aliases": ["/g"],
                },
            ],
            "help_text": CommandParser.get_help(),
        }

    # Pagina HTML do chat
    @app.get("/ai-chat", response_class=HTMLResponse, tags=["AI Chat"])
    def ai_chat_page():
        """
        Pagina do assistente IA contextual
        """
        return get_chat_page_html()

    print("[AI Chat Advanced] Endpoints registrados: /api/ai/chat/*")


# =============================================================================
# VUE.JS CHAT COMPONENT (HTML INLINE)
# =============================================================================

def get_chat_page_html() -> str:
    """
    Retorna o HTML da pagina do chat com componente Vue.js flutuante
    """
    return '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>AI Chat - Fabrica de Agentes</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <style>
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
            --belgo-light: #F3F4F6;
            --success-green: #10B981;
            --warning-yellow: #F59E0B;
            --error-red: #EF4444;
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
            background: var(--belgo-light);
            min-height: 100vh;
        }

        /* Floating Chat Button */
        .chat-fab {
            position: fixed;
            bottom: 24px;
            right: 24px;
            width: 60px;
            height: 60px;
            border-radius: 50%;
            background: linear-gradient(135deg, var(--belgo-blue), #005a6f);
            border: none;
            cursor: pointer;
            box-shadow: 0 4px 20px rgba(0,59,74,0.3);
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.3s ease;
            z-index: 1000;
        }

        .chat-fab:hover {
            transform: scale(1.1);
            box-shadow: 0 6px 30px rgba(0,59,74,0.4);
        }

        .chat-fab svg {
            width: 28px;
            height: 28px;
            fill: white;
        }

        .chat-fab .badge {
            position: absolute;
            top: -4px;
            right: -4px;
            background: var(--belgo-orange);
            color: white;
            font-size: 12px;
            font-weight: 600;
            width: 20px;
            height: 20px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        /* Chat Window */
        .chat-window {
            position: fixed;
            bottom: 100px;
            right: 24px;
            width: 400px;
            height: 600px;
            max-height: calc(100vh - 140px);
            background: white;
            border-radius: 16px;
            box-shadow: 0 10px 50px rgba(0,0,0,0.15);
            display: flex;
            flex-direction: column;
            overflow: hidden;
            z-index: 999;
            animation: slideUp 0.3s ease;
        }

        @keyframes slideUp {
            from {
                opacity: 0;
                transform: translateY(20px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }

        /* Chat Header */
        .chat-header {
            background: linear-gradient(135deg, var(--belgo-blue), #005a6f);
            color: white;
            padding: 16px 20px;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .chat-header h3 {
            font-size: 16px;
            font-weight: 600;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .chat-header .status-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            background: var(--success-green);
        }

        .chat-header .close-btn {
            background: rgba(255,255,255,0.1);
            border: none;
            color: white;
            width: 32px;
            height: 32px;
            border-radius: 8px;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: background 0.2s;
        }

        .chat-header .close-btn:hover {
            background: rgba(255,255,255,0.2);
        }

        /* Context Bar */
        .context-bar {
            background: #f8fafc;
            padding: 10px 16px;
            border-bottom: 1px solid #e2e8f0;
            display: flex;
            align-items: center;
            gap: 8px;
            font-size: 12px;
            color: #64748b;
        }

        .context-tag {
            background: var(--belgo-blue);
            color: white;
            padding: 2px 8px;
            border-radius: 4px;
            font-weight: 500;
        }

        /* Messages Area */
        .chat-messages {
            flex: 1;
            overflow-y: auto;
            padding: 16px;
            display: flex;
            flex-direction: column;
            gap: 12px;
        }

        .message {
            max-width: 85%;
            padding: 12px 16px;
            border-radius: 12px;
            font-size: 14px;
            line-height: 1.5;
        }

        .message.user {
            background: var(--belgo-blue);
            color: white;
            align-self: flex-end;
            border-bottom-right-radius: 4px;
        }

        .message.assistant {
            background: #f1f5f9;
            color: #1e293b;
            align-self: flex-start;
            border-bottom-left-radius: 4px;
        }

        .message.assistant .markdown-content h1,
        .message.assistant .markdown-content h2,
        .message.assistant .markdown-content h3 {
            margin-top: 8px;
            margin-bottom: 4px;
        }

        .message.assistant .markdown-content code {
            background: #e2e8f0;
            padding: 2px 6px;
            border-radius: 4px;
            font-size: 13px;
        }

        .message.assistant .markdown-content pre {
            background: #1e293b;
            color: #e2e8f0;
            padding: 12px;
            border-radius: 8px;
            overflow-x: auto;
            margin: 8px 0;
        }

        .message.assistant .markdown-content pre code {
            background: none;
            padding: 0;
        }

        .message.assistant .markdown-content ul,
        .message.assistant .markdown-content ol {
            margin-left: 20px;
        }

        .message.assistant .markdown-content table {
            width: 100%;
            border-collapse: collapse;
            margin: 8px 0;
            font-size: 12px;
        }

        .message.assistant .markdown-content th,
        .message.assistant .markdown-content td {
            border: 1px solid #e2e8f0;
            padding: 6px 10px;
            text-align: left;
        }

        .message.assistant .markdown-content th {
            background: #f1f5f9;
            font-weight: 600;
        }

        .message-time {
            font-size: 10px;
            color: #94a3b8;
            margin-top: 4px;
        }

        .message.user .message-time {
            color: rgba(255,255,255,0.7);
        }

        /* Suggestions */
        .suggestions {
            display: flex;
            flex-wrap: wrap;
            gap: 6px;
            margin-top: 8px;
        }

        .suggestion-chip {
            background: white;
            border: 1px solid #e2e8f0;
            color: var(--belgo-blue);
            padding: 4px 10px;
            border-radius: 16px;
            font-size: 12px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .suggestion-chip:hover {
            background: var(--belgo-blue);
            color: white;
            border-color: var(--belgo-blue);
        }

        /* Typing Indicator */
        .typing-indicator {
            display: flex;
            align-items: center;
            gap: 4px;
            padding: 12px 16px;
            background: #f1f5f9;
            border-radius: 12px;
            align-self: flex-start;
        }

        .typing-indicator span {
            width: 8px;
            height: 8px;
            background: #94a3b8;
            border-radius: 50%;
            animation: typing 1.4s infinite ease-in-out;
        }

        .typing-indicator span:nth-child(2) {
            animation-delay: 0.2s;
        }

        .typing-indicator span:nth-child(3) {
            animation-delay: 0.4s;
        }

        @keyframes typing {
            0%, 60%, 100% {
                transform: translateY(0);
            }
            30% {
                transform: translateY(-6px);
            }
        }

        /* Input Area */
        .chat-input-area {
            padding: 16px;
            border-top: 1px solid #e2e8f0;
            display: flex;
            gap: 8px;
        }

        .chat-input {
            flex: 1;
            padding: 12px 16px;
            border: 1px solid #e2e8f0;
            border-radius: 24px;
            font-size: 14px;
            outline: none;
            transition: border-color 0.2s;
        }

        .chat-input:focus {
            border-color: var(--belgo-blue);
        }

        .chat-input::placeholder {
            color: #94a3b8;
        }

        .send-btn {
            width: 44px;
            height: 44px;
            border-radius: 50%;
            background: var(--belgo-orange);
            border: none;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.2s;
        }

        .send-btn:hover {
            background: #e55a00;
            transform: scale(1.05);
        }

        .send-btn:disabled {
            background: #94a3b8;
            cursor: not-allowed;
            transform: none;
        }

        .send-btn svg {
            width: 20px;
            height: 20px;
            fill: white;
        }

        /* Command Highlight */
        .command-highlight {
            background: rgba(255, 108, 0, 0.1);
            border-left: 3px solid var(--belgo-orange);
            padding: 8px 12px;
            margin: 4px 0;
            border-radius: 0 8px 8px 0;
        }

        /* Quick Commands */
        .quick-commands {
            display: flex;
            gap: 6px;
            padding: 8px 16px;
            border-top: 1px solid #e2e8f0;
            overflow-x: auto;
        }

        .quick-command {
            background: #f1f5f9;
            border: none;
            color: #475569;
            padding: 6px 12px;
            border-radius: 16px;
            font-size: 12px;
            cursor: pointer;
            white-space: nowrap;
            transition: all 0.2s;
        }

        .quick-command:hover {
            background: var(--belgo-blue);
            color: white;
        }

        /* Mobile Responsive */
        @media (max-width: 480px) {
            .chat-window {
                width: calc(100vw - 32px);
                right: 16px;
                bottom: 90px;
                height: calc(100vh - 120px);
            }
        }
    </style>
</head>
<body>
    <div id="app">
        <!-- Floating Action Button -->
        <button class="chat-fab" @click="toggleChat" :title="isOpen ? 'Fechar chat' : 'Abrir assistente IA'">
            <svg v-if="!isOpen" viewBox="0 0 24 24">
                <path d="M20 2H4c-1.1 0-2 .9-2 2v18l4-4h14c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 14H5.17L4 17.17V4h16v12z"/>
                <path d="M7 9h10v2H7zm0-3h10v2H7zm0 6h7v2H7z"/>
            </svg>
            <svg v-else viewBox="0 0 24 24">
                <path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"/>
            </svg>
            <span v-if="unreadCount > 0" class="badge">{{ unreadCount }}</span>
        </button>

        <!-- Chat Window -->
        <div v-if="isOpen" class="chat-window">
            <!-- Header -->
            <div class="chat-header">
                <h3>
                    <span class="status-dot"></span>
                    Assistente IA
                </h3>
                <button class="close-btn" @click="toggleChat">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
                        <path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"/>
                    </svg>
                </button>
            </div>

            <!-- Context Bar -->
            <div class="context-bar" v-if="currentProject || currentStory">
                <span>Contexto:</span>
                <span v-if="currentProject" class="context-tag">{{ currentProject }}</span>
                <span v-if="currentStory" class="context-tag">{{ currentStory }}</span>
            </div>

            <!-- Messages -->
            <div class="chat-messages" ref="messagesContainer">
                <div v-for="msg in messages" :key="msg.message_id"
                     :class="['message', msg.role]">
                    <div v-if="msg.role === 'assistant'" class="markdown-content" v-html="renderMarkdown(msg.content)"></div>
                    <div v-else>{{ msg.content }}</div>
                    <div class="message-time">{{ formatTime(msg.created_at) }}</div>

                    <!-- Suggestions -->
                    <div v-if="msg.suggestions && msg.suggestions.length" class="suggestions">
                        <span v-for="sug in msg.suggestions" :key="sug"
                              class="suggestion-chip" @click="sendMessage(sug)">
                            {{ sug }}
                        </span>
                    </div>
                </div>

                <!-- Typing Indicator -->
                <div v-if="isTyping" class="typing-indicator">
                    <span></span>
                    <span></span>
                    <span></span>
                </div>
            </div>

            <!-- Quick Commands -->
            <div class="quick-commands">
                <button class="quick-command" @click="sendMessage('/help')">/help</button>
                <button class="quick-command" @click="sendMessage('/suggest')">/suggest</button>
                <button class="quick-command" @click="sendMessage('/status')">/status</button>
                <button class="quick-command" @click="sendMessage('/analyze')">/analyze</button>
            </div>

            <!-- Input Area -->
            <div class="chat-input-area">
                <input
                    v-model="inputMessage"
                    @keyup.enter="handleSend"
                    class="chat-input"
                    placeholder="Digite uma mensagem ou comando..."
                    :disabled="isTyping"
                >
                <button class="send-btn" @click="handleSend" :disabled="!inputMessage.trim() || isTyping">
                    <svg viewBox="0 0 24 24">
                        <path d="M2.01 21L23 12 2.01 3 2 10l15 2-15 2z"/>
                    </svg>
                </button>
            </div>
        </div>
    </div>

    <script>
        const { createApp, ref, onMounted, nextTick, watch } = Vue;

        createApp({
            setup() {
                const isOpen = ref(false);
                const messages = ref([]);
                const inputMessage = ref('');
                const isTyping = ref(false);
                const unreadCount = ref(0);
                const messagesContainer = ref(null);

                // Context from URL params
                const urlParams = new URLSearchParams(window.location.search);
                const currentProject = ref(urlParams.get('project_id') || null);
                const currentStory = ref(urlParams.get('story_id') || null);

                const toggleChat = () => {
                    isOpen.value = !isOpen.value;
                    if (isOpen.value) {
                        unreadCount.value = 0;
                        loadHistory();
                    }
                };

                const loadHistory = async () => {
                    try {
                        const params = new URLSearchParams();
                        if (currentProject.value) params.append('project_id', currentProject.value);
                        if (currentStory.value) params.append('story_id', currentStory.value);

                        const response = await fetch(`/api/ai/chat/history?${params.toString()}`);
                        const data = await response.json();
                        messages.value = data.messages || [];
                        scrollToBottom();
                    } catch (error) {
                        console.error('Error loading history:', error);
                    }
                };

                const sendMessage = async (text) => {
                    const messageText = text || inputMessage.value.trim();
                    if (!messageText) return;

                    inputMessage.value = '';

                    // Add user message optimistically
                    const tempId = 'temp-' + Date.now();
                    messages.value.push({
                        message_id: tempId,
                        role: 'user',
                        content: messageText,
                        created_at: new Date().toISOString(),
                    });

                    scrollToBottom();
                    isTyping.value = true;

                    try {
                        const response = await fetch('/api/ai/chat/message', {
                            method: 'POST',
                            headers: {
                                'Content-Type': 'application/json',
                            },
                            body: JSON.stringify({
                                message: messageText,
                                project_id: currentProject.value,
                                story_id: currentStory.value,
                            }),
                        });

                        const data = await response.json();

                        // Add assistant response
                        messages.value.push({
                            message_id: data.message_id,
                            role: 'assistant',
                            content: data.content,
                            created_at: data.created_at,
                            suggestions: data.suggestions,
                        });

                        scrollToBottom();
                    } catch (error) {
                        console.error('Error sending message:', error);
                        messages.value.push({
                            message_id: 'error-' + Date.now(),
                            role: 'assistant',
                            content: 'Desculpe, ocorreu um erro ao processar sua mensagem. Tente novamente.',
                            created_at: new Date().toISOString(),
                        });
                    } finally {
                        isTyping.value = false;
                    }
                };

                const handleSend = () => {
                    sendMessage();
                };

                const scrollToBottom = () => {
                    nextTick(() => {
                        if (messagesContainer.value) {
                            messagesContainer.value.scrollTop = messagesContainer.value.scrollHeight;
                        }
                    });
                };

                const renderMarkdown = (content) => {
                    try {
                        return marked.parse(content);
                    } catch (e) {
                        return content;
                    }
                };

                const formatTime = (isoString) => {
                    if (!isoString) return '';
                    const date = new Date(isoString);
                    return date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
                };

                // Welcome message on first open
                watch(isOpen, (newVal) => {
                    if (newVal && messages.value.length === 0) {
                        messages.value.push({
                            message_id: 'welcome',
                            role: 'assistant',
                            content: `Ola! Sou seu assistente IA para desenvolvimento Agile.

Posso ajudar com:
- **Criar stories e tasks** - \`/create story "titulo"\`
- **Estimar story points** - \`/estimate STR-0001\`
- **Buscar informacoes** - \`/search termo\`
- **Sugestoes contextuais** - \`/suggest\`

Digite \`/help\` para ver todos os comandos disponiveis.

Como posso ajudar?`,
                            created_at: new Date().toISOString(),
                            suggestions: ['/help', '/suggest', '/status'],
                        });
                    }
                });

                return {
                    isOpen,
                    messages,
                    inputMessage,
                    isTyping,
                    unreadCount,
                    messagesContainer,
                    currentProject,
                    currentStory,
                    toggleChat,
                    sendMessage,
                    handleSend,
                    renderMarkdown,
                    formatTime,
                };
            },
        }).mount('#app');
    </script>
</body>
</html>'''


# =============================================================================
# MAIN - REGISTRO AUTOMATICO
# =============================================================================

if __name__ == "__main__":
    print("[AI Chat Advanced] Modulo carregado. Use register_ai_chat_endpoints(app) para registrar.")
