# -*- coding: utf-8 -*-
"""
MCP Tools - Ferramentas MCP para a Plataforma E

Este modulo define as ferramentas que serao expostas pelo servidor MCP.
Cada ferramenta representa uma operacao que o Claude pode executar.

Categorias de Tools:
- StoryTools: Gerenciamento de User Stories
- TaskTools: Gerenciamento de Story Tasks
- WorkerTools: Controle de Workers e Jobs
- ProjectTools: Gerenciamento de Projetos
- SystemTools: Consultas e metricas do sistema
"""

import json
import asyncio
from typing import Any, Dict, List, Optional
from datetime import datetime
from dataclasses import dataclass, field, asdict
from enum import Enum
import uuid


# =============================================================================
# TIPOS BASE
# =============================================================================

class ToolCategory(str, Enum):
    """Categorias de ferramentas MCP"""
    STORY = "story"
    TASK = "task"
    WORKER = "worker"
    PROJECT = "project"
    SYSTEM = "system"


@dataclass
class ToolParameter:
    """Parametro de uma ferramenta MCP"""
    name: str
    type: str
    description: str
    required: bool = True
    default: Any = None
    enum: Optional[List[str]] = None

    def to_schema(self) -> Dict:
        """Converte para JSON Schema"""
        schema = {
            "type": self.type,
            "description": self.description,
        }
        if self.enum:
            schema["enum"] = self.enum
        if self.default is not None:
            schema["default"] = self.default
        return schema


@dataclass
class ToolDefinition:
    """Definicao de uma ferramenta MCP"""
    name: str
    description: str
    category: ToolCategory
    parameters: List[ToolParameter] = field(default_factory=list)
    handler: Any = None  # Funcao que executa a tool

    def to_mcp_schema(self) -> Dict:
        """Converte para schema MCP"""
        properties = {}
        required = []

        for param in self.parameters:
            properties[param.name] = param.to_schema()
            if param.required:
                required.append(param.name)

        return {
            "name": self.name,
            "description": self.description,
            "inputSchema": {
                "type": "object",
                "properties": properties,
                "required": required
            }
        }


# =============================================================================
# STORY TOOLS
# =============================================================================

class StoryTools:
    """
    Ferramentas para gerenciamento de User Stories

    Operacoes:
    - create_story: Cria nova story
    - list_stories: Lista stories de um projeto
    - get_story: Busca story por ID
    - update_story: Atualiza story
    - move_story: Move story no kanban
    - delete_story: Deleta story
    - estimate_story: Estima story points com IA
    """

    def __init__(self, db_session=None):
        self.db_session = db_session
        self.tools = self._define_tools()

    def _define_tools(self) -> List[ToolDefinition]:
        """Define as ferramentas de Story"""
        return [
            ToolDefinition(
                name="create_story",
                description="Cria uma nova User Story no projeto. Use formato Agile: Como um [persona], eu quero [acao], para que [beneficio].",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("project_id", "string", "ID do projeto (ex: PROJ-001)"),
                    ToolParameter("title", "string", "Titulo da story"),
                    ToolParameter("persona", "string", "Persona (Como um...)"),
                    ToolParameter("action", "string", "Acao desejada (Eu quero...)"),
                    ToolParameter("benefit", "string", "Beneficio esperado (Para que...)"),
                    ToolParameter("acceptance_criteria", "array", "Lista de criterios de aceite", required=False),
                    ToolParameter("story_points", "integer", "Story points (1,2,3,5,8,13,21)", required=False, default=0),
                    ToolParameter("priority", "string", "Prioridade", required=False, default="medium",
                                enum=["low", "medium", "high", "urgent"]),
                    ToolParameter("epic_id", "string", "ID do Epic associado", required=False),
                    ToolParameter("sprint_id", "string", "ID do Sprint associado", required=False),
                ],
                handler=self.create_story
            ),
            ToolDefinition(
                name="list_stories",
                description="Lista User Stories de um projeto. Pode filtrar por status, sprint, epic.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("project_id", "string", "ID do projeto"),
                    ToolParameter("status", "string", "Filtrar por status", required=False,
                                enum=["backlog", "ready", "in_progress", "review", "testing", "done"]),
                    ToolParameter("sprint_id", "string", "Filtrar por sprint", required=False),
                    ToolParameter("epic_id", "string", "Filtrar por epic", required=False),
                    ToolParameter("limit", "integer", "Limite de resultados", required=False, default=50),
                ],
                handler=self.list_stories
            ),
            ToolDefinition(
                name="get_story",
                description="Busca detalhes completos de uma User Story incluindo tasks e documentacao.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story (ex: STR-0001)"),
                ],
                handler=self.get_story
            ),
            ToolDefinition(
                name="update_story",
                description="Atualiza campos de uma User Story existente.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story"),
                    ToolParameter("title", "string", "Novo titulo", required=False),
                    ToolParameter("persona", "string", "Nova persona", required=False),
                    ToolParameter("action", "string", "Nova acao", required=False),
                    ToolParameter("benefit", "string", "Novo beneficio", required=False),
                    ToolParameter("acceptance_criteria", "array", "Novos criterios de aceite", required=False),
                    ToolParameter("story_points", "integer", "Novos story points", required=False),
                    ToolParameter("priority", "string", "Nova prioridade", required=False,
                                enum=["low", "medium", "high", "urgent"]),
                    ToolParameter("assignee", "string", "Novo responsavel", required=False),
                ],
                handler=self.update_story
            ),
            ToolDefinition(
                name="move_story",
                description="Move uma User Story para outra coluna no Kanban.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story"),
                    ToolParameter("status", "string", "Novo status",
                                enum=["backlog", "ready", "in_progress", "review", "testing", "done"]),
                ],
                handler=self.move_story
            ),
            ToolDefinition(
                name="delete_story",
                description="Deleta uma User Story e todas suas tasks associadas.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story a deletar"),
                ],
                handler=self.delete_story
            ),
            ToolDefinition(
                name="estimate_story",
                description="Usa IA para estimar story points baseado na complexidade da story.",
                category=ToolCategory.STORY,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story para estimar"),
                ],
                handler=self.estimate_story
            ),
        ]

    async def create_story(self, **kwargs) -> Dict[str, Any]:
        """Cria uma nova User Story"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            # Gerar ID unico
            story_id = f"STR-{datetime.now().strftime('%Y%m%d%H%M%S')}-{str(uuid.uuid4())[:4].upper()}"

            story = Story(
                story_id=story_id,
                project_id=kwargs.get("project_id"),
                title=kwargs.get("title"),
                persona=kwargs.get("persona"),
                action=kwargs.get("action"),
                benefit=kwargs.get("benefit"),
                acceptance_criteria=kwargs.get("acceptance_criteria", []),
                story_points=kwargs.get("story_points", 0),
                priority=kwargs.get("priority", "medium"),
                epic_id=kwargs.get("epic_id"),
                sprint_id=kwargs.get("sprint_id"),
                status="backlog",
                created_by="mcp-tools"
            )

            db.add(story)
            db.commit()
            db.refresh(story)

            return {
                "success": True,
                "story_id": story.story_id,
                "message": f"Story '{story.title}' criada com sucesso",
                "data": story.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def list_stories(self, **kwargs) -> Dict[str, Any]:
        """Lista stories de um projeto"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            query = db.query(Story).filter(Story.project_id == kwargs.get("project_id"))

            if kwargs.get("status"):
                query = query.filter(Story.status == kwargs.get("status"))
            if kwargs.get("sprint_id"):
                query = query.filter(Story.sprint_id == kwargs.get("sprint_id"))
            if kwargs.get("epic_id"):
                query = query.filter(Story.epic_id == kwargs.get("epic_id"))

            limit = kwargs.get("limit", 50)
            stories = query.order_by(Story.kanban_order).limit(limit).all()

            return {
                "success": True,
                "count": len(stories),
                "stories": [s.to_dict() for s in stories]
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def get_story(self, **kwargs) -> Dict[str, Any]:
        """Busca story por ID com detalhes"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()

            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            data = story.to_dict()

            # Incluir tasks
            data["tasks"] = [t.to_dict() for t in story.story_tasks]

            # Incluir documentacao
            data["documentation"] = [d.to_dict() for d in story.documentation]

            return {
                "success": True,
                "story": data
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def update_story(self, **kwargs) -> Dict[str, Any]:
        """Atualiza uma story"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()

            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            # Atualizar campos fornecidos
            update_fields = ["title", "persona", "action", "benefit",
                           "acceptance_criteria", "story_points", "priority", "assignee"]

            for field in update_fields:
                if field in kwargs and kwargs[field] is not None:
                    setattr(story, field, kwargs[field])

            story.updated_at = datetime.utcnow()
            db.commit()
            db.refresh(story)

            return {
                "success": True,
                "message": f"Story {story.story_id} atualizada",
                "data": story.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def move_story(self, **kwargs) -> Dict[str, Any]:
        """Move story no kanban"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()

            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            old_status = story.status
            new_status = kwargs.get("status")

            story.status = new_status
            story.updated_at = datetime.utcnow()

            # Atualizar timestamps especificos
            if new_status == "in_progress" and not story.started_at:
                story.started_at = datetime.utcnow()
            elif new_status == "done":
                story.completed_at = datetime.utcnow()

            db.commit()

            return {
                "success": True,
                "message": f"Story movida de '{old_status}' para '{new_status}'",
                "story_id": story.story_id
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def delete_story(self, **kwargs) -> Dict[str, Any]:
        """Deleta uma story"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()

            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            story_id = story.story_id
            title = story.title

            db.delete(story)
            db.commit()

            return {
                "success": True,
                "message": f"Story '{title}' ({story_id}) deletada com sucesso"
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def estimate_story(self, **kwargs) -> Dict[str, Any]:
        """Estima story points usando IA"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story, StoryEstimate

        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()

            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            # Logica de estimativa baseada em complexidade
            # Fatores: tamanho do titulo, numero de criterios, complexidade da acao
            factors = []
            points = 3  # Base

            # Tamanho da descricao
            desc_len = len(story.action or "") + len(story.benefit or "")
            if desc_len > 200:
                points += 2
                factors.append("Descricao extensa indica maior complexidade")

            # Numero de criterios de aceite
            criteria_count = len(story.acceptance_criteria or [])
            if criteria_count > 5:
                points += 3
                factors.append(f"{criteria_count} criterios de aceite")
            elif criteria_count > 3:
                points += 1
                factors.append(f"{criteria_count} criterios de aceite")

            # Palavras-chave de complexidade
            complexity_keywords = ["integracao", "api", "seguranca", "performance",
                                  "migracao", "refatoracao", "arquitetura"]
            text = (story.title + " " + (story.action or "")).lower()

            for keyword in complexity_keywords:
                if keyword in text:
                    points += 1
                    factors.append(f"Detectada palavra-chave: {keyword}")

            # Ajustar para Fibonacci
            fibonacci = [1, 2, 3, 5, 8, 13, 21]
            estimated_points = min(fibonacci, key=lambda x: abs(x - points))

            # Salvar estimativa
            estimate = StoryEstimate(
                estimate_id=f"EST-{str(uuid.uuid4())[:8].upper()}",
                story_id=story.story_id,
                estimated_points=estimated_points,
                confidence=0.7 + (0.1 * len(factors)),
                justification=f"Estimativa baseada em {len(factors)} fatores de complexidade",
                factors=factors,
                estimate_type="ai",
                estimated_by="mcp-ai-estimator"
            )

            db.add(estimate)

            # Atualizar story se ainda nao tem pontos
            if story.story_points == 0:
                story.story_points = estimated_points

            db.commit()

            return {
                "success": True,
                "story_id": story.story_id,
                "estimated_points": estimated_points,
                "confidence": estimate.confidence,
                "factors": factors,
                "message": f"Story estimada em {estimated_points} pontos"
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()


# =============================================================================
# TASK TOOLS
# =============================================================================

class TaskTools:
    """
    Ferramentas para gerenciamento de Story Tasks

    Operacoes:
    - create_task: Cria nova task em uma story
    - list_tasks: Lista tasks de uma story
    - update_task: Atualiza task
    - complete_task: Marca task como completa
    """

    def __init__(self, db_session=None):
        self.db_session = db_session
        self.tools = self._define_tools()

    def _define_tools(self) -> List[ToolDefinition]:
        """Define as ferramentas de Task"""
        return [
            ToolDefinition(
                name="create_task",
                description="Cria uma nova Task dentro de uma User Story.",
                category=ToolCategory.TASK,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story pai"),
                    ToolParameter("title", "string", "Titulo da task"),
                    ToolParameter("description", "string", "Descricao da task", required=False),
                    ToolParameter("task_type", "string", "Tipo de task", required=False, default="development",
                                enum=["development", "review", "test", "documentation", "design", "research"]),
                    ToolParameter("estimated_hours", "number", "Horas estimadas", required=False, default=0),
                    ToolParameter("assignee", "string", "Responsavel pela task", required=False),
                ],
                handler=self.create_task
            ),
            ToolDefinition(
                name="list_tasks",
                description="Lista todas as tasks de uma User Story.",
                category=ToolCategory.TASK,
                parameters=[
                    ToolParameter("story_id", "string", "ID da story"),
                    ToolParameter("status", "string", "Filtrar por status", required=False,
                                enum=["pending", "in_progress", "completed", "blocked", "cancelled"]),
                ],
                handler=self.list_tasks
            ),
            ToolDefinition(
                name="update_task",
                description="Atualiza uma task existente.",
                category=ToolCategory.TASK,
                parameters=[
                    ToolParameter("task_id", "string", "ID da task"),
                    ToolParameter("title", "string", "Novo titulo", required=False),
                    ToolParameter("description", "string", "Nova descricao", required=False),
                    ToolParameter("status", "string", "Novo status", required=False,
                                enum=["pending", "in_progress", "completed", "blocked", "cancelled"]),
                    ToolParameter("progress", "integer", "Progresso (0-100)", required=False),
                    ToolParameter("code_output", "string", "Codigo gerado", required=False),
                    ToolParameter("files_created", "array", "Arquivos criados", required=False),
                ],
                handler=self.update_task
            ),
            ToolDefinition(
                name="complete_task",
                description="Marca uma task como completa e atualiza o progresso da story.",
                category=ToolCategory.TASK,
                parameters=[
                    ToolParameter("task_id", "string", "ID da task"),
                    ToolParameter("code_output", "string", "Codigo final gerado", required=False),
                    ToolParameter("files_created", "array", "Lista de arquivos criados", required=False),
                    ToolParameter("test_results", "object", "Resultados dos testes", required=False),
                ],
                handler=self.complete_task
            ),
        ]

    async def create_task(self, **kwargs) -> Dict[str, Any]:
        """Cria uma nova task"""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask, Story

        db = SessionLocal()
        try:
            # Verificar se story existe
            story = db.query(Story).filter(Story.story_id == kwargs.get("story_id")).first()
            if not story:
                return {"success": False, "error": "Story nao encontrada"}

            # Gerar ID
            task_id = f"STSK-{datetime.now().strftime('%Y%m%d%H%M%S')}-{str(uuid.uuid4())[:4].upper()}"

            # Contar tasks existentes para ordem
            task_count = db.query(StoryTask).filter(StoryTask.story_id == story.story_id).count()

            task = StoryTask(
                task_id=task_id,
                story_id=story.story_id,
                title=kwargs.get("title"),
                description=kwargs.get("description"),
                task_type=kwargs.get("task_type", "development"),
                estimated_hours=kwargs.get("estimated_hours", 0),
                assignee=kwargs.get("assignee"),
                task_order=task_count,
                status="pending"
            )

            db.add(task)

            # Atualizar contador da story
            story.tasks_total = task_count + 1
            story.update_progress()

            db.commit()
            db.refresh(task)

            return {
                "success": True,
                "task_id": task.task_id,
                "message": f"Task '{task.title}' criada na story {story.story_id}",
                "data": task.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def list_tasks(self, **kwargs) -> Dict[str, Any]:
        """Lista tasks de uma story"""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask

        db = SessionLocal()
        try:
            query = db.query(StoryTask).filter(StoryTask.story_id == kwargs.get("story_id"))

            if kwargs.get("status"):
                query = query.filter(StoryTask.status == kwargs.get("status"))

            tasks = query.order_by(StoryTask.task_order).all()

            return {
                "success": True,
                "count": len(tasks),
                "tasks": [t.to_dict() for t in tasks]
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def update_task(self, **kwargs) -> Dict[str, Any]:
        """Atualiza uma task"""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask, Story

        db = SessionLocal()
        try:
            task = db.query(StoryTask).filter(StoryTask.task_id == kwargs.get("task_id")).first()

            if not task:
                return {"success": False, "error": "Task nao encontrada"}

            # Atualizar campos
            update_fields = ["title", "description", "status", "progress",
                           "code_output", "files_created"]

            for field in update_fields:
                if field in kwargs and kwargs[field] is not None:
                    setattr(task, field, kwargs[field])

            # Se mudou para in_progress, registrar inicio
            if kwargs.get("status") == "in_progress" and not task.started_at:
                task.started_at = datetime.utcnow()

            task.updated_at = datetime.utcnow()
            db.commit()

            return {
                "success": True,
                "message": f"Task {task.task_id} atualizada",
                "data": task.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def complete_task(self, **kwargs) -> Dict[str, Any]:
        """Marca task como completa"""
        from factory.database.connection import SessionLocal
        from factory.database.models import StoryTask, Story

        db = SessionLocal()
        try:
            task = db.query(StoryTask).filter(StoryTask.task_id == kwargs.get("task_id")).first()

            if not task:
                return {"success": False, "error": "Task nao encontrada"}

            # Atualizar task
            task.status = "completed"
            task.progress = 100
            task.completed_at = datetime.utcnow()

            if kwargs.get("code_output"):
                task.code_output = kwargs["code_output"]
            if kwargs.get("files_created"):
                task.files_created = kwargs["files_created"]
            if kwargs.get("test_results"):
                task.test_results = kwargs["test_results"]

            # Atualizar story
            story = db.query(Story).filter(Story.story_id == task.story_id).first()
            if story:
                story.tasks_completed += 1
                story.update_progress()

            db.commit()

            return {
                "success": True,
                "message": f"Task {task.task_id} completada",
                "story_progress": story.progress if story else 0,
                "data": task.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()


# =============================================================================
# WORKER TOOLS
# =============================================================================

class WorkerTools:
    """
    Ferramentas para controle de Workers e Jobs

    Operacoes:
    - create_job: Cria um novo job para processamento
    - list_jobs: Lista jobs na fila
    - get_job_status: Consulta status de um job
    - cancel_job: Cancela um job pendente
    - list_workers: Lista workers ativos
    - get_queue_stats: Estatisticas da fila
    """

    def __init__(self, db_session=None):
        self.db_session = db_session
        self.tools = self._define_tools()

    def _define_tools(self) -> List[ToolDefinition]:
        """Define as ferramentas de Worker"""
        return [
            ToolDefinition(
                name="create_job",
                description="Cria um novo job para processamento autonomo por um worker Claude.",
                category=ToolCategory.WORKER,
                parameters=[
                    ToolParameter("description", "string", "Descricao do que deve ser construido"),
                    ToolParameter("tech_stack", "string", "Stack tecnologica (ex: python, fastapi, react)", required=False),
                    ToolParameter("features", "array", "Lista de features a implementar", required=False),
                    ToolParameter("project_id", "string", "ID do projeto associado", required=False),
                ],
                handler=self.create_job
            ),
            ToolDefinition(
                name="list_jobs",
                description="Lista jobs na fila de processamento.",
                category=ToolCategory.WORKER,
                parameters=[
                    ToolParameter("status", "string", "Filtrar por status", required=False,
                                enum=["pending", "queued", "running", "completed", "failed", "cancelled"]),
                    ToolParameter("limit", "integer", "Limite de resultados", required=False, default=20),
                ],
                handler=self.list_jobs
            ),
            ToolDefinition(
                name="get_job_status",
                description="Consulta o status detalhado de um job especifico.",
                category=ToolCategory.WORKER,
                parameters=[
                    ToolParameter("job_id", "string", "ID do job"),
                ],
                handler=self.get_job_status
            ),
            ToolDefinition(
                name="cancel_job",
                description="Cancela um job que ainda nao foi iniciado.",
                category=ToolCategory.WORKER,
                parameters=[
                    ToolParameter("job_id", "string", "ID do job a cancelar"),
                ],
                handler=self.cancel_job
            ),
            ToolDefinition(
                name="list_workers",
                description="Lista todos os workers registrados e seu status.",
                category=ToolCategory.WORKER,
                parameters=[
                    ToolParameter("status", "string", "Filtrar por status", required=False,
                                enum=["idle", "busy", "offline"]),
                ],
                handler=self.list_workers
            ),
            ToolDefinition(
                name="get_queue_stats",
                description="Retorna estatisticas da fila de jobs.",
                category=ToolCategory.WORKER,
                parameters=[],
                handler=self.get_queue_stats
            ),
        ]

    async def create_job(self, **kwargs) -> Dict[str, Any]:
        """Cria um novo job"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()

            job_data = {
                "description": kwargs.get("description"),
                "tech_stack": kwargs.get("tech_stack"),
                "features": kwargs.get("features", []),
                "project_id": kwargs.get("project_id"),
                "created_by": "mcp-tools"
            }

            result = await queue.enqueue(job_data)

            return {
                "success": True,
                "job_id": result.get("job_id"),
                "message": "Job criado e adicionado a fila",
                "data": result
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def list_jobs(self, **kwargs) -> Dict[str, Any]:
        """Lista jobs"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()
            jobs = await queue.list_jobs(
                status=kwargs.get("status"),
                limit=kwargs.get("limit", 20)
            )

            return {
                "success": True,
                "count": len(jobs),
                "jobs": jobs
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def get_job_status(self, **kwargs) -> Dict[str, Any]:
        """Consulta status de um job"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()
            job = await queue.get_job(kwargs.get("job_id"))

            if not job:
                return {"success": False, "error": "Job nao encontrado"}

            return {
                "success": True,
                "job": job
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def cancel_job(self, **kwargs) -> Dict[str, Any]:
        """Cancela um job"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()
            success = await queue.cancel_job(kwargs.get("job_id"))

            if success:
                return {
                    "success": True,
                    "message": f"Job {kwargs.get('job_id')} cancelado"
                }
            else:
                return {
                    "success": False,
                    "error": "Nao foi possivel cancelar o job (pode ja estar em execucao)"
                }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def list_workers(self, **kwargs) -> Dict[str, Any]:
        """Lista workers"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()
            workers = await queue.get_workers()

            if kwargs.get("status"):
                workers = [w for w in workers if w.get("status") == kwargs.get("status")]

            return {
                "success": True,
                "count": len(workers),
                "workers": workers
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def get_queue_stats(self, **kwargs) -> Dict[str, Any]:
        """Retorna estatisticas da fila"""
        try:
            from factory.core.job_queue import get_queue

            queue = await get_queue()
            stats = await queue.get_stats()

            return {
                "success": True,
                "stats": stats
            }
        except Exception as e:
            return {"success": False, "error": str(e)}


# =============================================================================
# PROJECT TOOLS
# =============================================================================

class ProjectTools:
    """
    Ferramentas para gerenciamento de Projetos

    Operacoes:
    - create_project: Cria novo projeto
    - list_projects: Lista projetos
    - get_project: Detalhes de um projeto
    - update_project: Atualiza projeto
    """

    def __init__(self, db_session=None):
        self.db_session = db_session
        self.tools = self._define_tools()

    def _define_tools(self) -> List[ToolDefinition]:
        """Define as ferramentas de Project"""
        return [
            ToolDefinition(
                name="create_project",
                description="Cria um novo projeto na fabrica.",
                category=ToolCategory.PROJECT,
                parameters=[
                    ToolParameter("name", "string", "Nome do projeto"),
                    ToolParameter("description", "string", "Descricao do projeto", required=False),
                    ToolParameter("project_type", "string", "Tipo de projeto", required=False, default="web-app",
                                enum=["web-app", "api-service", "data-analysis", "automation", "integration"]),
                ],
                handler=self.create_project
            ),
            ToolDefinition(
                name="list_projects",
                description="Lista todos os projetos.",
                category=ToolCategory.PROJECT,
                parameters=[
                    ToolParameter("status", "string", "Filtrar por status", required=False,
                                enum=["PLANNING", "IN_PROGRESS", "PAUSED", "COMPLETED", "ARCHIVED"]),
                    ToolParameter("limit", "integer", "Limite de resultados", required=False, default=50),
                ],
                handler=self.list_projects
            ),
            ToolDefinition(
                name="get_project",
                description="Busca detalhes de um projeto especifico.",
                category=ToolCategory.PROJECT,
                parameters=[
                    ToolParameter("project_id", "string", "ID do projeto"),
                ],
                handler=self.get_project
            ),
            ToolDefinition(
                name="update_project",
                description="Atualiza informacoes de um projeto.",
                category=ToolCategory.PROJECT,
                parameters=[
                    ToolParameter("project_id", "string", "ID do projeto"),
                    ToolParameter("name", "string", "Novo nome", required=False),
                    ToolParameter("description", "string", "Nova descricao", required=False),
                    ToolParameter("status", "string", "Novo status", required=False,
                                enum=["PLANNING", "IN_PROGRESS", "PAUSED", "COMPLETED", "ARCHIVED"]),
                ],
                handler=self.update_project
            ),
        ]

    async def create_project(self, **kwargs) -> Dict[str, Any]:
        """Cria um novo projeto"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            # Gerar ID
            project_id = f"PROJ-{datetime.now().strftime('%Y%m%d')}-{str(uuid.uuid4())[:4].upper()}"

            project = Project(
                project_id=project_id,
                name=kwargs.get("name"),
                description=kwargs.get("description"),
                project_type=kwargs.get("project_type", "web-app"),
                status="PLANNING",
                created_by="mcp-tools"
            )

            db.add(project)
            db.commit()
            db.refresh(project)

            return {
                "success": True,
                "project_id": project.project_id,
                "message": f"Projeto '{project.name}' criado com sucesso",
                "data": project.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def list_projects(self, **kwargs) -> Dict[str, Any]:
        """Lista projetos"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            query = db.query(Project)

            if kwargs.get("status"):
                query = query.filter(Project.status == kwargs.get("status"))

            projects = query.order_by(Project.created_at.desc()).limit(kwargs.get("limit", 50)).all()

            return {
                "success": True,
                "count": len(projects),
                "projects": [p.to_dict() for p in projects]
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def get_project(self, **kwargs) -> Dict[str, Any]:
        """Busca detalhes de um projeto"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            project = db.query(Project).filter(Project.project_id == kwargs.get("project_id")).first()

            if not project:
                return {"success": False, "error": "Projeto nao encontrado"}

            data = project.to_dict()

            # Incluir stories
            data["stories_count"] = len(project.stories) if hasattr(project, 'stories') else 0
            data["jobs_count"] = len(project.jobs) if project.jobs else 0

            return {
                "success": True,
                "project": data
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def update_project(self, **kwargs) -> Dict[str, Any]:
        """Atualiza um projeto"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            project = db.query(Project).filter(Project.project_id == kwargs.get("project_id")).first()

            if not project:
                return {"success": False, "error": "Projeto nao encontrado"}

            # Atualizar campos
            if kwargs.get("name"):
                project.name = kwargs["name"]
            if kwargs.get("description"):
                project.description = kwargs["description"]
            if kwargs.get("status"):
                project.status = kwargs["status"]

            project.updated_at = datetime.utcnow()
            db.commit()

            return {
                "success": True,
                "message": f"Projeto {project.project_id} atualizado",
                "data": project.to_dict()
            }
        except Exception as e:
            db.rollback()
            return {"success": False, "error": str(e)}
        finally:
            db.close()


# =============================================================================
# SYSTEM TOOLS
# =============================================================================

class SystemTools:
    """
    Ferramentas de consulta e metricas do sistema

    Operacoes:
    - system_health: Verifica saude do sistema
    - get_metrics: Metricas gerais
    - list_activity_logs: Logs de atividades
    """

    def __init__(self, db_session=None):
        self.db_session = db_session
        self.tools = self._define_tools()

    def _define_tools(self) -> List[ToolDefinition]:
        """Define as ferramentas de System"""
        return [
            ToolDefinition(
                name="system_health",
                description="Verifica a saude geral do sistema (database, redis, workers).",
                category=ToolCategory.SYSTEM,
                parameters=[],
                handler=self.system_health
            ),
            ToolDefinition(
                name="get_metrics",
                description="Retorna metricas gerais do sistema (jobs, stories, workers).",
                category=ToolCategory.SYSTEM,
                parameters=[],
                handler=self.get_metrics
            ),
            ToolDefinition(
                name="list_activity_logs",
                description="Lista logs de atividades recentes.",
                category=ToolCategory.SYSTEM,
                parameters=[
                    ToolParameter("level", "string", "Filtrar por nivel", required=False,
                                enum=["DEBUG", "INFO", "WARNING", "ERROR"]),
                    ToolParameter("limit", "integer", "Limite de resultados", required=False, default=50),
                ],
                handler=self.list_activity_logs
            ),
        ]

    async def system_health(self, **kwargs) -> Dict[str, Any]:
        """Verifica saude do sistema"""
        from factory.database.connection import check_db_health

        try:
            health = await check_db_health()

            # Verificar workers
            workers_online = 0
            try:
                from factory.core.job_queue import get_queue
                queue = await get_queue()
                workers = await queue.get_workers()
                workers_online = len([w for w in workers if w.get("status") in ["idle", "busy"]])
            except:
                pass

            return {
                "success": True,
                "status": "healthy" if health.get("database") == "connected" else "degraded",
                "components": {
                    "database": health.get("database", "unknown"),
                    "redis": health.get("redis", "unknown"),
                    "workers_online": workers_online
                },
                "timestamp": datetime.utcnow().isoformat()
            }
        except Exception as e:
            return {"success": False, "error": str(e)}

    async def get_metrics(self, **kwargs) -> Dict[str, Any]:
        """Retorna metricas do sistema"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Story, StoryTask, Job, Project

        db = SessionLocal()
        try:
            metrics = {
                "projects": {
                    "total": db.query(Project).count(),
                    "active": db.query(Project).filter(Project.status == "IN_PROGRESS").count()
                },
                "stories": {
                    "total": db.query(Story).count(),
                    "backlog": db.query(Story).filter(Story.status == "backlog").count(),
                    "in_progress": db.query(Story).filter(Story.status == "in_progress").count(),
                    "done": db.query(Story).filter(Story.status == "done").count()
                },
                "tasks": {
                    "total": db.query(StoryTask).count(),
                    "completed": db.query(StoryTask).filter(StoryTask.status == "completed").count()
                },
                "jobs": {
                    "total": db.query(Job).count(),
                    "pending": db.query(Job).filter(Job.status == "pending").count(),
                    "running": db.query(Job).filter(Job.status == "running").count(),
                    "completed": db.query(Job).filter(Job.status == "completed").count(),
                    "failed": db.query(Job).filter(Job.status == "failed").count()
                },
                "timestamp": datetime.utcnow().isoformat()
            }

            return {
                "success": True,
                "metrics": metrics
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()

    async def list_activity_logs(self, **kwargs) -> Dict[str, Any]:
        """Lista logs de atividades"""
        from factory.database.connection import SessionLocal
        from factory.database.models import ActivityLog

        db = SessionLocal()
        try:
            query = db.query(ActivityLog)

            if kwargs.get("level"):
                query = query.filter(ActivityLog.level == kwargs.get("level"))

            logs = query.order_by(ActivityLog.timestamp.desc()).limit(kwargs.get("limit", 50)).all()

            return {
                "success": True,
                "count": len(logs),
                "logs": [l.to_dict() for l in logs]
            }
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            db.close()


# =============================================================================
# HELPER: COLETAR TODAS AS TOOLS
# =============================================================================

def get_all_tools() -> List[ToolDefinition]:
    """Retorna todas as ferramentas disponiveis"""
    all_tools = []

    tool_classes = [
        StoryTools(),
        TaskTools(),
        WorkerTools(),
        ProjectTools(),
        SystemTools(),
    ]

    for tool_class in tool_classes:
        all_tools.extend(tool_class.tools)

    return all_tools


def get_tool_handlers() -> Dict[str, Any]:
    """Retorna mapeamento nome -> handler para todas as tools"""
    handlers = {}

    tool_classes = [
        StoryTools(),
        TaskTools(),
        WorkerTools(),
        ProjectTools(),
        SystemTools(),
    ]

    for tool_class in tool_classes:
        for tool in tool_class.tools:
            handlers[tool.name] = tool.handler

    return handlers
