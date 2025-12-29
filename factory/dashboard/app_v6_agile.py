# -*- coding: utf-8 -*-
"""
Dashboard Agile v6.0 - Fabrica de Agentes
==========================================
Sistema completo de gestao Agile com:
- User Stories detalhadas (narrativa, criterios, DoD)
- Kanban Board de Stories
- Tasks como subtarefas
- Documentacao tecnica integrada
- Assistente de Chat com IA

Porta: 9001
"""
import os
import sys
import uuid
import shutil
from datetime import datetime
from pathlib import Path
from typing import Optional, List

# Configurar encoding UTF-8
sys.stdout.reconfigure(encoding='utf-8')

# Mudar para diretorio do projeto e adicionar ao path
os.chdir(r'C:\Users\lcruz\Fabrica de Agentes')
sys.path.insert(0, r'C:\Users\lcruz\Fabrica de Agentes')

from dotenv import load_dotenv
load_dotenv()

from fastapi import FastAPI, HTTPException, UploadFile, File, Form, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
from typing import List, Optional, Dict, Any

# Database
from factory.database.connection import SessionLocal, engine, Base
from factory.database.models import (
    Project, ProjectStatus, ActivityLog,
    Story, StoryStatus, StoryCategory, StoryComplexity,
    StoryTask, StoryTaskType, StoryTaskStatus,
    StoryDocumentation, DocType,
    ChatMessage, MessageRole,
    Attachment, Epic, Sprint,
    TaskPriority
)
from factory.database.repositories import (
    ProjectRepository, ActivityLogRepository,
    StoryRepository, StoryTaskRepository, StoryDocumentationRepository,
    ChatMessageRepository, AttachmentRepository,
    EpicRepository, SprintRepository
)

# Claude AI Integration
try:
    from factory.ai.claude_integration import ClaudeClient, get_claude_client
    HAS_CLAUDE = True
except ImportError:
    HAS_CLAUDE = False
    print("[Dashboard] Claude integration not available")

# Criar tabelas
Base.metadata.create_all(bind=engine)

# App FastAPI
app = FastAPI(
    title="Fabrica de Agentes - Dashboard Agile v6.0",
    description="Sistema de gestao Agile com Stories, Tasks e Assistente",
    version="6.0.0"
)

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Diretorio de uploads
UPLOAD_DIR = Path(r'C:\Users\lcruz\Fabrica de Agentes\uploads')
UPLOAD_DIR.mkdir(exist_ok=True)


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class StoryCreate(BaseModel):
    project_id: str
    title: str
    description: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: Optional[List[str]] = []
    definition_of_done: Optional[List[str]] = []
    business_rules: Optional[List[str]] = []
    technical_notes: Optional[str] = None
    epic_id: Optional[str] = None
    sprint_id: Optional[str] = None
    category: Optional[str] = "feature"
    story_points: Optional[int] = 0
    complexity: Optional[str] = "medium"
    estimated_hours: Optional[float] = 0.0
    priority: Optional[str] = "medium"
    assignee: Optional[str] = None
    tags: Optional[List[str]] = []


class StoryUpdate(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: Optional[List[str]] = None
    definition_of_done: Optional[List[str]] = None
    business_rules: Optional[List[str]] = None
    technical_notes: Optional[str] = None
    epic_id: Optional[str] = None
    sprint_id: Optional[str] = None
    category: Optional[str] = None
    story_points: Optional[int] = None
    complexity: Optional[str] = None
    estimated_hours: Optional[float] = None
    priority: Optional[str] = None
    assignee: Optional[str] = None
    tags: Optional[List[str]] = None


class StoryMove(BaseModel):
    status: str
    order: Optional[int] = None


class StoryTaskCreate(BaseModel):
    story_id: str
    title: str
    description: Optional[str] = None
    task_type: Optional[str] = "development"
    agent_id: Optional[str] = None
    assignee: Optional[str] = None
    estimated_hours: Optional[float] = 0.0
    acceptance_criteria: Optional[List[str]] = []


class StoryTaskUpdate(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    task_type: Optional[str] = None
    status: Optional[str] = None
    agent_id: Optional[str] = None
    assignee: Optional[str] = None
    progress: Optional[int] = None
    estimated_hours: Optional[float] = None
    actual_hours: Optional[float] = None
    files_created: Optional[List[str]] = None
    files_modified: Optional[List[str]] = None
    code_output: Optional[str] = None
    test_results: Optional[dict] = None


class DocCreate(BaseModel):
    story_id: str
    task_id: Optional[str] = None
    doc_type: Optional[str] = "technical"
    title: str
    content: Optional[str] = None
    test_instructions: Optional[str] = None
    test_cases: Optional[List[dict]] = []
    deploy_instructions: Optional[str] = None


class ChatMessageCreate(BaseModel):
    project_id: Optional[str] = None
    story_id: Optional[str] = None
    content: str
    user_id: Optional[str] = "user"


class EpicCreate(BaseModel):
    project_id: str
    title: str
    description: Optional[str] = None
    color: Optional[str] = "#003B4A"


class SprintCreate(BaseModel):
    project_id: str
    name: str
    goal: Optional[str] = None
    capacity: Optional[int] = 0


# =============================================================================
# API ENDPOINTS - STORIES
# =============================================================================

@app.get("/api/stories")
def list_stories(
    project_id: Optional[str] = None,
    status: Optional[str] = None,
    epic_id: Optional[str] = None,
    sprint_id: Optional[str] = None
):
    """Lista stories com filtros"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        stories = repo.get_all(
            project_id=project_id,
            status=status,
            epic_id=epic_id,
            sprint_id=sprint_id
        )
        return [s.to_dict() for s in stories]
    finally:
        db.close()


@app.post("/api/stories")
def create_story(story: StoryCreate):
    """Cria nova story"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        new_story = repo.create(story.dict(exclude_none=True))
        return new_story.to_dict()
    finally:
        db.close()


@app.get("/api/stories/{story_id}")
def get_story(story_id: str):
    """Busca story completa com tasks"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        story = repo.get_with_tasks(story_id)
        if not story:
            raise HTTPException(404, "Story not found")
        return story
    finally:
        db.close()


@app.put("/api/stories/{story_id}")
def update_story(story_id: str, data: StoryUpdate):
    """Atualiza story"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        story = repo.update(story_id, data.dict(exclude_none=True))
        if not story:
            raise HTTPException(404, "Story not found")
        return story.to_dict()
    finally:
        db.close()


@app.delete("/api/stories/{story_id}")
def delete_story(story_id: str):
    """Remove story"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        if repo.delete(story_id):
            return {"message": "Story deleted"}
        raise HTTPException(404, "Story not found")
    finally:
        db.close()


@app.patch("/api/stories/{story_id}/move")
def move_story(story_id: str, move: StoryMove):
    """Move story no Kanban"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        story = repo.move_story(story_id, move.status, move.order)
        if not story:
            raise HTTPException(404, "Story not found")
        return story.to_dict()
    finally:
        db.close()


@app.get("/api/projects/{project_id}/story-board")
def get_story_board(project_id: str):
    """Retorna Kanban board de stories"""
    db = SessionLocal()
    try:
        repo = StoryRepository(db)
        return repo.get_story_board(project_id)
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - STORY TASKS
# =============================================================================

@app.get("/api/stories/{story_id}/tasks")
def list_story_tasks(story_id: str):
    """Lista tasks de uma story"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        tasks = repo.get_by_story(story_id)
        return [t.to_dict() for t in tasks]
    finally:
        db.close()


@app.post("/api/stories/{story_id}/tasks")
def create_story_task(story_id: str, task: StoryTaskCreate):
    """Cria task na story"""
    db = SessionLocal()
    try:
        task_data = task.dict(exclude_none=True)
        task_data["story_id"] = story_id
        repo = StoryTaskRepository(db)
        new_task = repo.create(task_data)
        return new_task.to_dict()
    finally:
        db.close()


@app.put("/api/story-tasks/{task_id}")
def update_story_task(task_id: str, data: StoryTaskUpdate):
    """Atualiza task"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.update(task_id, data.dict(exclude_none=True))
        if not task:
            raise HTTPException(404, "Task not found")
        return task.to_dict()
    finally:
        db.close()


@app.patch("/api/story-tasks/{task_id}/complete")
def complete_story_task(task_id: str, output: Optional[dict] = None):
    """Marca task como completa"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.complete(task_id, output)
        if not task:
            raise HTTPException(404, "Task not found")
        return task.to_dict()
    finally:
        db.close()


@app.delete("/api/story-tasks/{task_id}")
def delete_story_task(task_id: str):
    """Remove task"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        if repo.delete(task_id):
            return {"message": "Task deleted"}
        raise HTTPException(404, "Task not found")
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - DOCUMENTATION
# =============================================================================

@app.get("/api/stories/{story_id}/docs")
def list_story_docs(story_id: str):
    """Lista documentacao de uma story"""
    db = SessionLocal()
    try:
        repo = StoryDocumentationRepository(db)
        docs = repo.get_by_story(story_id)
        return [d.to_dict() for d in docs]
    finally:
        db.close()


@app.post("/api/stories/{story_id}/docs")
def create_story_doc(story_id: str, doc: DocCreate):
    """Cria documentacao"""
    db = SessionLocal()
    try:
        doc_data = doc.dict(exclude_none=True)
        doc_data["story_id"] = story_id
        repo = StoryDocumentationRepository(db)
        new_doc = repo.create(doc_data)
        return new_doc.to_dict()
    finally:
        db.close()


@app.put("/api/story-docs/{doc_id}")
def update_story_doc(doc_id: str, data: dict):
    """Atualiza documentacao"""
    db = SessionLocal()
    try:
        repo = StoryDocumentationRepository(db)
        doc = repo.update(doc_id, data)
        if not doc:
            raise HTTPException(404, "Doc not found")
        return doc.to_dict()
    finally:
        db.close()


@app.delete("/api/story-docs/{doc_id}")
def delete_story_doc(doc_id: str):
    """Remove documentacao"""
    db = SessionLocal()
    try:
        repo = StoryDocumentationRepository(db)
        if repo.delete(doc_id):
            return {"message": "Doc deleted"}
        raise HTTPException(404, "Doc not found")
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - CHAT (ASSISTENTE)
# =============================================================================

@app.get("/api/chat/history")
def get_chat_history(
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    limit: int = 50
):
    """Retorna historico de mensagens"""
    db = SessionLocal()
    try:
        repo = ChatMessageRepository(db)
        messages = repo.get_history(project_id, story_id, limit)
        return [m.to_dict() for m in messages]
    finally:
        db.close()


@app.post("/api/chat/message")
def send_chat_message(msg: ChatMessageCreate):
    """Envia mensagem e retorna resposta do assistente"""
    db = SessionLocal()
    try:
        repo = ChatMessageRepository(db)

        # Salva mensagem do usuario
        user_msg = repo.create({
            "project_id": msg.project_id,
            "story_id": msg.story_id,
            "role": "user",
            "content": msg.content,
            "user_id": msg.user_id
        })

        # Gera resposta do assistente (simples por enquanto)
        assistant_response = generate_assistant_response(msg.content, msg.project_id, msg.story_id, db)

        # Salva resposta
        assistant_msg = repo.create({
            "project_id": msg.project_id,
            "story_id": msg.story_id,
            "role": "assistant",
            "content": assistant_response["content"],
            "actions": assistant_response.get("actions", [])
        })

        return {
            "user_message": user_msg.to_dict(),
            "assistant_message": assistant_msg.to_dict()
        }
    finally:
        db.close()


def extract_story_id(text: str) -> Optional[str]:
    """Extrai ID de story do texto (STR-XXXX)"""
    import re
    match = re.search(r'STR-?\d{4}', text.upper())
    if match:
        story_id = match.group()
        if '-' not in story_id:
            story_id = 'STR-' + story_id[3:]
        return story_id
    return None


def format_story_details(story) -> str:
    """Formata detalhes completos de uma story"""
    narrative = ""
    if story.persona:
        narrative += f"Como {story.persona}\n"
    if story.action:
        narrative += f"Eu quero {story.action}\n"
    if story.benefit:
        narrative += f"Para que {story.benefit}\n"

    criteria = ""
    if story.acceptance_criteria:
        criteria = "\n".join([f"- {c}" for c in story.acceptance_criteria])

    dod = ""
    if story.definition_of_done:
        dod = "\n".join([f"- {d}" for d in story.definition_of_done])

    tasks_info = ""
    if story.story_tasks:
        tasks_info = "\n".join([
            f"- {t.task_id}: {t.title} [{t.status}]"
            for t in story.story_tasks
        ])

    return f"""Story: {story.story_id} - {story.title}
Status: {story.status} | Pontos: {story.story_points} pts | Complexidade: {story.complexity}
Categoria: {story.category} | Prioridade: {story.priority}

Narrativa:
{narrative if narrative else 'Nao definida'}

Descricao: {story.description or 'Sem descricao'}

Criterios de Aceite:
{criteria if criteria else 'Nenhum criterio definido'}

Definition of Done:
{dod if dod else 'Nenhum DoD definido'}

Tasks ({story.tasks_completed}/{story.tasks_total}):
{tasks_info if tasks_info else 'Nenhuma task'}

Notas Tecnicas: {story.technical_notes or 'Sem notas'}"""


def get_project_context(db, project_id: str = None) -> str:
    """Obtem contexto completo do projeto para o Claude"""
    story_repo = StoryRepository(db)

    if project_id:
        stories = story_repo.get_by_project(project_id)
    else:
        stories = story_repo.get_all()

    if not stories:
        return "Nao ha stories cadastradas no projeto."

    # Resumo do projeto
    total = len(stories)
    by_status = {}
    total_points = 0
    done_points = 0

    for s in stories:
        by_status[s.status] = by_status.get(s.status, 0) + 1
        total_points += s.story_points or 0
        if s.status == 'done':
            done_points += s.story_points or 0

    context = f"""CONTEXTO DO PROJETO:
Total de Stories: {total}
Pontos totais: {total_points} | Concluidos: {done_points}
Por status: {by_status}

LISTA DE STORIES:
"""
    for s in stories:
        context += f"\n- {s.story_id}: {s.title} [{s.status}] ({s.story_points} pts)"
        if s.persona:
            context += f"\n  Narrativa: Como {s.persona}, eu quero {s.action or '...'}, para que {s.benefit or '...'}"

    return context


def execute_assistant_action(action: dict, db) -> str:
    """Executa uma acao determinada pelo assistente"""
    story_repo = StoryRepository(db)
    task_repo = StoryTaskRepository(db)
    project_repo = ProjectRepository(db)
    action_type = action.get("action")
    result = ""

    # =========================================================================
    # ACOES DE STORY
    # =========================================================================
    if action_type == "move_story":
        story_id = action.get("story_id")
        new_status = action.get("status")
        if story_id and new_status:
            story = story_repo.get_by_id(story_id)
            if story:
                story_repo.move_story(story_id, new_status)
                result = f"Story {story_id} movida para {new_status}"

    elif action_type == "get_story_details":
        story_id = action.get("story_id")
        if story_id:
            story = story_repo.get_by_id(story_id)
            if story:
                result = format_story_details(story)

    elif action_type == "list_stories":
        project_id = action.get("project_id")
        if project_id:
            stories = story_repo.get_by_project(project_id)
        else:
            stories = story_repo.get_all()
        if stories:
            result = "\n".join([f"- {s.story_id}: {s.title} [{s.status}] ({s.story_points} pts)" for s in stories])
        else:
            result = "Nenhuma story encontrada"

    elif action_type == "update_story":
        story_id = action.get("story_id")
        updates = action.get("updates", {})
        if story_id and updates:
            story_repo.update(story_id, updates)
            result = f"Story {story_id} atualizada"

    elif action_type == "create_story":
        story_data = action.get("story_data", {})
        if story_data.get("project_id") and story_data.get("title"):
            story = story_repo.create(story_data)
            result = f"Story {story.story_id} criada: {story.title}"
        else:
            result = "Erro: project_id e title sao obrigatorios"

    # =========================================================================
    # ACOES DE EXECUCAO/STATUS
    # =========================================================================
    elif action_type == "check_execution_status":
        story_id = action.get("story_id")
        task_id = action.get("task_id")

        if task_id:
            # Status de task especifica
            task = task_repo.get_by_id(task_id)
            if task:
                result = f"Task {task_id}: {task.title}\n"
                result += f"Status: {task.status}\n"
                result += f"Progresso: {task.progress}%\n"
                if task.started_at:
                    result += f"Iniciada em: {task.started_at}\n"
                if task.completed_at:
                    result += f"Concluida em: {task.completed_at}\n"
                if task.code_output:
                    result += f"Output: {task.code_output[:200]}..."
        elif story_id:
            # Status de story e suas tasks
            story = story_repo.get_by_id(story_id)
            if story:
                result = f"Story {story_id}: {story.title}\n"
                result += f"Status: {story.status}\n"
                result += f"Progresso: {story.tasks_completed}/{story.tasks_total} tasks\n\n"
                result += "Tasks:\n"
                for t in story.story_tasks:
                    status_icon = "✅" if t.status == "completed" else "🔄" if t.status == "in_progress" else "⏳"
                    result += f"{status_icon} {t.task_id}: {t.title} [{t.status}] {t.progress}%\n"

    elif action_type == "force_execute":
        story_id = action.get("story_id")
        if story_id:
            story = story_repo.get_by_id(story_id)
            if story:
                # Mover para ready para que o watcher processe
                if story.status in ['backlog', 'done', 'testing']:
                    story_repo.move_story(story_id, 'ready')
                    result = f"Story {story_id} movida para 'ready'. O Story Watcher ira processa-la automaticamente."
                elif story.status == 'ready':
                    result = f"Story {story_id} ja esta em 'ready' e sera processada pelo Story Watcher."
                elif story.status == 'in_progress':
                    # Verificar tasks pendentes
                    pending = [t for t in story.story_tasks if t.status == 'pending']
                    if pending:
                        result = f"Story {story_id} ja esta em execucao. {len(pending)} tasks pendentes."
                    else:
                        result = f"Story {story_id} em execucao, todas as tasks ja iniciadas ou concluidas."

    # =========================================================================
    # ACOES DE PROJETO
    # =========================================================================
    elif action_type == "create_project":
        project_data = action.get("project_data", {})
        if project_data.get("name"):
            # Definir defaults
            project_data.setdefault("project_type", "web-app")
            project_data.setdefault("status", "planning")
            project = project_repo.create(project_data)
            result = f"Projeto criado: {project.project_id} - {project.name}"
        else:
            result = "Erro: nome do projeto e obrigatorio"

    elif action_type == "list_projects":
        projects = project_repo.get_all()
        if projects:
            result = "\n".join([
                f"- {p.project_id}: {p.name} [{p.status}] ({p.project_type})"
                for p in projects
            ])
        else:
            result = "Nenhum projeto encontrado"

    elif action_type == "get_project_details":
        project_id = action.get("project_id")
        if project_id:
            project = project_repo.get_by_id(project_id)
            if project:
                stories = story_repo.get_by_project(project_id)
                result = f"Projeto: {project.project_id} - {project.name}\n"
                result += f"Tipo: {project.project_type} | Status: {project.status}\n"
                result += f"Descricao: {project.description or 'N/A'}\n"
                result += f"Total de Stories: {len(stories)}\n"
                if stories:
                    by_status = {}
                    for s in stories:
                        by_status[s.status] = by_status.get(s.status, 0) + 1
                    result += f"Por status: {by_status}"

    # =========================================================================
    # ACOES DE ANALISE DE ARQUIVOS
    # =========================================================================
    elif action_type == "analyze_file_for_stories":
        file_path = action.get("file_path")
        project_id = action.get("project_id")
        file_content = action.get("file_content")

        if file_content and project_id:
            # Claude ja analisou o conteudo e esta pedindo para criar stories
            # O conteudo aqui seria as stories sugeridas pelo Claude
            result = f"Analise do arquivo para projeto {project_id} concluida. Use create_story para criar as stories sugeridas."
        elif file_path:
            # Tentar ler arquivo
            try:
                upload_path = Path(UPLOAD_DIR) / file_path
                if upload_path.exists():
                    with open(upload_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    result = f"Conteudo do arquivo ({len(content)} chars):\n{content[:2000]}"
                    if len(content) > 2000:
                        result += "\n... (truncado)"
                else:
                    result = f"Arquivo nao encontrado: {file_path}"
            except Exception as e:
                result = f"Erro ao ler arquivo: {str(e)}"

    elif action_type == "list_attachments":
        story_id = action.get("story_id")
        attachment_repo = AttachmentRepository(db)
        if story_id:
            attachments = attachment_repo.get_by_story(story_id)
        else:
            # Listar todos os uploads recentes
            attachments = db.query(Attachment).order_by(Attachment.created_at.desc()).limit(20).all()

        if attachments:
            result = "\n".join([
                f"- {a.attachment_id}: {a.original_filename} ({a.file_size} bytes)"
                for a in attachments
            ])
        else:
            result = "Nenhum arquivo encontrado"

    elif action_type == "read_attachment":
        attachment_id = action.get("attachment_id")
        attachment_repo = AttachmentRepository(db)
        attachment = attachment_repo.get_by_id(attachment_id)

        if attachment:
            try:
                file_path = Path(attachment.file_path)
                if file_path.exists():
                    # Verificar tipo de arquivo
                    if attachment.mime_type and attachment.mime_type.startswith('text'):
                        with open(file_path, 'r', encoding='utf-8') as f:
                            content = f.read()
                        result = f"Arquivo: {attachment.original_filename}\n\nConteudo:\n{content[:3000]}"
                        if len(content) > 3000:
                            result += "\n... (truncado)"
                    else:
                        result = f"Arquivo: {attachment.original_filename} ({attachment.mime_type}) - {attachment.file_size} bytes\nArquivo binario, nao pode ser exibido como texto."
                else:
                    result = f"Arquivo nao encontrado no disco: {attachment.original_filename}"
            except Exception as e:
                result = f"Erro ao ler arquivo: {str(e)}"
        else:
            result = f"Attachment {attachment_id} nao encontrado"

    return result


def generate_assistant_response(content: str, project_id: str, story_id: str, db) -> dict:
    """Gera resposta do assistente usando Claude AI"""
    import json as json_module

    story_repo = StoryRepository(db)
    actions_executed = []

    # =========================================================================
    # USAR CLAUDE AI SE DISPONIVEL
    # =========================================================================
    if HAS_CLAUDE:
        try:
            claude = get_claude_client()

            if claude.is_available():
                # Obter contexto do projeto
                project_context = get_project_context(db, project_id)

                # Obter detalhes da story atual se especificada
                current_story_context = ""
                if story_id:
                    story = story_repo.get_by_id(story_id)
                    if story:
                        current_story_context = f"\n\nSTORY ATUAL EM FOCO:\n{format_story_details(story)}"

                # System prompt para o assistente
                system_prompt = f"""Voce e o Assistente Inteligente da Fabrica de Agentes, um sistema de gestao Agile.
Seu papel e ajudar usuarios a gerenciar User Stories, Tasks, Projetos e o desenvolvimento autonomo.

SUAS CAPACIDADES:
1. Gerenciar stories (criar, editar, mover, listar, ver detalhes)
2. Verificar status de execucao de stories e tasks
3. Forcar execucao de stories (envia para processamento automatico)
4. Criar e gerenciar projetos
5. Analisar documentos/arquivos para criar stories
6. Sugerir melhorias e boas praticas Agile

=== ACOES DISPONIVEIS ===

STORIES:
- {{"action": "get_story_details", "story_id": "STR-XXXX"}}
- {{"action": "list_stories", "project_id": "PRJ-XXXX"}}  (project_id opcional)
- {{"action": "move_story", "story_id": "STR-XXXX", "status": "backlog|ready|in_progress|review|testing|done"}}
- {{"action": "update_story", "story_id": "STR-XXXX", "updates": {{"title": "...", "story_points": N, "priority": "high"}}}}
- {{"action": "create_story", "story_data": {{"project_id": "PRJ-XXXX", "title": "...", "persona": "...", "action": "...", "benefit": "...", "story_points": N, "acceptance_criteria": ["..."]}}}}

EXECUCAO E STATUS:
- {{"action": "check_execution_status", "story_id": "STR-XXXX"}}  (mostra progresso de tasks)
- {{"action": "check_execution_status", "task_id": "STSK-XXXX"}}  (status de task especifica)
- {{"action": "force_execute", "story_id": "STR-XXXX"}}  (envia para fila de execucao automatica)

PROJETOS:
- {{"action": "list_projects"}}
- {{"action": "get_project_details", "project_id": "PRJ-XXXX"}}
- {{"action": "create_project", "project_data": {{"name": "...", "description": "...", "project_type": "web-app|api-service|data-analysis"}}}}

ARQUIVOS E DOCUMENTOS:
- {{"action": "list_attachments", "story_id": "STR-XXXX"}}  (lista arquivos anexados)
- {{"action": "read_attachment", "attachment_id": "XXX"}}  (le conteudo do arquivo)

=== FORMATO DE RESPOSTA ===
- Responda de forma clara e util em portugues
- Para executar acoes, inclua o JSON entre <action> e </action> NO FINAL da resposta
- Voce pode executar MULTIPLAS acoes em uma unica resposta
- Exemplo: "Vou criar o projeto e a primeira story. <action>{{"action": "create_project", "project_data": {{"name": "Meu App"}}}}</action><action>{{"action": "create_story", "story_data": {{"project_id": "PRJ-0001", "title": "Login"}}}}</action>"

=== CONTEXTO ATUAL ===
{project_context}
{current_story_context}

=== INSTRUCOES ===
- Seja proativo: quando o usuario pedir algo, EXECUTE a acao
- Para verificar status de execucao, use check_execution_status
- Para iniciar execucao automatica de uma story, use force_execute
- Ao analisar arquivos para criar stories, primeiro use read_attachment, depois crie as stories com create_story
- Use os dados REAIS do projeto listados acima
- Se o usuario enviar um documento para analise, leia-o e sugira stories baseadas no conteudo"""

                # Chamar Claude
                response = claude.chat(
                    message=content,
                    system_prompt=system_prompt,
                    max_tokens=2048
                )

                if response.success:
                    response_text = response.content

                    # Extrair e executar acoes do response
                    import re
                    action_matches = re.findall(r'<action>(.*?)</action>', response_text, re.DOTALL)

                    for action_json in action_matches:
                        try:
                            action = json_module.loads(action_json.strip())
                            result = execute_assistant_action(action, db)
                            if result:
                                actions_executed.append({
                                    "type": action.get("action"),
                                    "result": result
                                })
                        except json_module.JSONDecodeError:
                            pass

                    # Limpar tags de acao da resposta
                    clean_response = re.sub(r'<action>.*?</action>', '', response_text, flags=re.DOTALL).strip()

                    # Se executou acoes, adicionar resultado
                    if actions_executed:
                        action_results = "\n".join([f"✓ {a['type']}: {a['result']}" for a in actions_executed])
                        clean_response += f"\n\n**Acoes executadas:**\n{action_results}"

                    return {
                        "content": clean_response,
                        "actions": actions_executed
                    }
                else:
                    # Fallback se Claude falhar
                    print(f"[Assistant] Claude error: {response.error}")

        except Exception as e:
            print(f"[Assistant] Exception: {str(e)}")

    # =========================================================================
    # FALLBACK: RESPOSTAS BASEADAS EM REGRAS (se Claude nao disponivel)
    # =========================================================================
    content_lower = content.lower()

    # Detalhes de story
    if any(word in content_lower for word in ['detalhe', 'detalhes', 'info', 'sobre']):
        mentioned_id = extract_story_id(content)
        target_id = mentioned_id or story_id
        if target_id:
            story = story_repo.get_by_id(target_id)
            if story:
                return {
                    "content": f"**{story.story_id}: {story.title}**\n\n{format_story_details(story)}",
                    "actions": [{"type": "show_story", "story_id": target_id}]
                }

    # Mover story
    if any(word in content_lower for word in ['mover', 'mova', 'move']):
        mentioned_id = extract_story_id(content)
        status_map = {'ready': 'ready', 'backlog': 'backlog', 'in_progress': 'in_progress',
                      'testing': 'testing', 'done': 'done', 'pronto': 'ready', 'pronta': 'ready'}
        new_status = None
        for key, value in status_map.items():
            if key in content_lower:
                new_status = value
                break
        if mentioned_id and new_status:
            story_repo.move_story(mentioned_id, new_status)
            return {
                "content": f"Story **{mentioned_id}** movida para **{new_status}**!",
                "actions": [{"type": "move_story", "story_id": mentioned_id, "status": new_status}]
            }

    # Listar stories
    if any(word in content_lower for word in ['listar', 'lista', 'todas', 'stories']):
        stories = story_repo.get_all()
        if stories:
            stories_list = "\n".join([f"- **{s.story_id}**: {s.title} [{s.status}]" for s in stories])
            return {"content": f"**Stories:**\n{stories_list}", "actions": []}

    # Resposta padrao
    return {
        "content": "[Claude AI indisponivel] Posso ajudar com comandos basicos:\n- 'detalhes STR-XXXX'\n- 'mover STR-XXXX para ready'\n- 'listar stories'\n\nPara respostas inteligentes, configure a API key do Claude.",
        "actions": []
    }


@app.delete("/api/chat/history")
def clear_chat_history(project_id: Optional[str] = None, story_id: Optional[str] = None):
    """Limpa historico de chat"""
    db = SessionLocal()
    try:
        repo = ChatMessageRepository(db)
        count = repo.clear_history(project_id, story_id)
        return {"cleared": count}
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - ATTACHMENTS
# =============================================================================

@app.post("/api/upload")
async def upload_file(
    file: UploadFile = File(...),
    story_id: Optional[str] = Form(None),
    task_id: Optional[str] = Form(None)
):
    """Upload de arquivo"""
    db = SessionLocal()
    try:
        # Salva arquivo
        file_id = uuid.uuid4().hex[:8]
        ext = Path(file.filename).suffix
        filename = f"{file_id}{ext}"
        file_path = UPLOAD_DIR / filename

        with open(file_path, "wb") as f:
            content = await file.read()
            f.write(content)

        # Registra no banco
        repo = AttachmentRepository(db)
        attachment = repo.create({
            "story_id": story_id,
            "task_id": task_id,
            "filename": filename,
            "original_filename": file.filename,
            "file_path": str(file_path),
            "file_size": len(content),
            "mime_type": file.content_type
        })

        return attachment.to_dict()
    finally:
        db.close()


@app.get("/api/attachments/{attachment_id}")
def get_attachment(attachment_id: str):
    """Download de arquivo"""
    db = SessionLocal()
    try:
        repo = AttachmentRepository(db)
        attachment = repo.get_by_id(attachment_id)
        if not attachment:
            raise HTTPException(404, "Attachment not found")
        return FileResponse(
            attachment.file_path,
            filename=attachment.original_filename,
            media_type=attachment.mime_type
        )
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - EPICS & SPRINTS
# =============================================================================

@app.get("/api/projects/{project_id}/epics")
def list_epics(project_id: str):
    """Lista epicos do projeto"""
    db = SessionLocal()
    try:
        repo = EpicRepository(db)
        epics = repo.get_by_project(project_id)
        return [e.to_dict() for e in epics]
    finally:
        db.close()


@app.post("/api/epics")
def create_epic(epic: EpicCreate):
    """Cria epico"""
    db = SessionLocal()
    try:
        repo = EpicRepository(db)
        new_epic = repo.create(epic.dict())
        return new_epic.to_dict()
    finally:
        db.close()


@app.get("/api/projects/{project_id}/sprints")
def list_sprints(project_id: str):
    """Lista sprints do projeto"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprints = repo.get_by_project(project_id)
        return [s.to_dict() for s in sprints]
    finally:
        db.close()


@app.post("/api/sprints")
def create_sprint(sprint: SprintCreate):
    """Cria sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        new_sprint = repo.create(sprint.dict())
        return new_sprint.to_dict()
    finally:
        db.close()


@app.patch("/api/sprints/{sprint_id}/start")
def start_sprint(sprint_id: str):
    """Inicia sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprint = repo.start_sprint(sprint_id)
        if not sprint:
            raise HTTPException(404, "Sprint not found")
        return sprint.to_dict()
    finally:
        db.close()


@app.patch("/api/sprints/{sprint_id}/complete")
def complete_sprint(sprint_id: str):
    """Finaliza sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprint = repo.complete_sprint(sprint_id)
        if not sprint:
            raise HTTPException(404, "Sprint not found")
        return sprint.to_dict()
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - PROJECTS
# =============================================================================

@app.get("/api/projects")
def list_projects():
    """Lista projetos"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        projects = repo.get_all()
        return [p.to_dict() for p in projects]
    finally:
        db.close()


@app.get("/api/projects/{project_id}")
def get_project(project_id: str):
    """Busca projeto"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        project = repo.get_by_id(project_id)
        if not project:
            raise HTTPException(404, "Project not found")
        return project.to_dict()
    finally:
        db.close()


@app.get("/api/status")
def get_status():
    """Status da API"""
    db = SessionLocal()
    try:
        project_repo = ProjectRepository(db)
        story_repo = StoryRepository(db)
        return {
            "status": "online",
            "version": "6.0.0-agile",
            "projects": len(project_repo.get_all()),
            "stories": len(story_repo.get_all()),
            "timestamp": datetime.now().isoformat()
        }
    finally:
        db.close()


# =============================================================================
# FRONTEND - HTML/Vue.js
# =============================================================================

HTML_TEMPLATE = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Fabrica de Agentes - Dashboard Agile</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <style>
        * { font-family: 'Inter', sans-serif; }
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
            --belgo-light: #E8F4F7;
            --belgo-gray: #6B7280;
        }
        .belgo-blue { background-color: var(--belgo-blue); }
        .belgo-orange { background-color: var(--belgo-orange); }
        .text-belgo-blue { color: var(--belgo-blue); }
        .text-belgo-orange { color: var(--belgo-orange); }
        .border-belgo-orange { border-color: var(--belgo-orange); }
        .hover-belgo-orange:hover { background-color: var(--belgo-orange); }

        .story-card { transition: all 0.2s; cursor: grab; }
        .story-card:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
        .story-card.sortable-ghost { opacity: 0.4; }
        .story-card.sortable-drag { cursor: grabbing; }

        .progress-bar { transition: width 0.3s ease; }

        .slide-panel {
            transition: transform 0.3s ease;
            transform: translateX(100%);
        }
        .slide-panel.open { transform: translateX(0); }

        .chat-container { height: calc(100vh - 200px); }
        .chat-messages { overflow-y: auto; scroll-behavior: smooth; }

        .tab-active { border-bottom: 2px solid var(--belgo-orange); color: var(--belgo-blue); }

        .priority-urgent { border-left: 4px solid #EF4444; }
        .priority-high { border-left: 4px solid #F59E0B; }
        .priority-medium { border-left: 4px solid #3B82F6; }
        .priority-low { border-left: 4px solid #10B981; }

        .kanban-column { min-height: 400px; }

        .narrative-box {
            background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%);
            border-left: 4px solid var(--belgo-blue);
        }

        .criteria-item:hover { background-color: #f3f4f6; }

        .markdown-content h1 { font-size: 1.5rem; font-weight: 600; margin-bottom: 0.5rem; }
        .markdown-content h2 { font-size: 1.25rem; font-weight: 600; margin-bottom: 0.5rem; }
        .markdown-content p { margin-bottom: 0.75rem; }
        .markdown-content ul { list-style: disc; padding-left: 1.5rem; margin-bottom: 0.75rem; }
        .markdown-content code { background: #f3f4f6; padding: 0.125rem 0.25rem; border-radius: 0.25rem; }

        /* Toast Notifications */
        .toast-container {
            position: fixed;
            bottom: 20px;
            right: 20px;
            z-index: 9999;
            display: flex;
            flex-direction: column;
            gap: 10px;
        }
        .toast {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 14px 20px;
            border-radius: 8px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            animation: slideIn 0.3s ease, fadeOut 0.3s ease 4.7s forwards;
            min-width: 300px;
            max-width: 450px;
        }
        .toast-success { background: #10B981; color: white; }
        .toast-error { background: #EF4444; color: white; }
        .toast-warning { background: #F59E0B; color: white; }
        .toast-info { background: #3B82F6; color: white; }
        .toast-icon { font-size: 1.25rem; }
        .toast-content { flex: 1; }
        .toast-title { font-weight: 600; font-size: 0.875rem; }
        .toast-message { font-size: 0.8rem; opacity: 0.9; }
        .toast-close {
            background: none;
            border: none;
            color: white;
            opacity: 0.7;
            cursor: pointer;
            padding: 4px;
            font-size: 1.1rem;
        }
        .toast-close:hover { opacity: 1; }
        .toast-undo {
            background: rgba(255,255,255,0.2);
            border: none;
            color: white;
            padding: 4px 12px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 0.75rem;
            font-weight: 600;
        }
        .toast-undo:hover { background: rgba(255,255,255,0.3); }
        @keyframes slideIn {
            from { transform: translateX(100%); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
        }
        @keyframes fadeOut {
            from { opacity: 1; }
            to { opacity: 0; }
        }

        /* Search Box */
        .search-box {
            position: relative;
        }
        .search-box input {
            padding-left: 36px;
            transition: all 0.2s;
        }
        .search-box input:focus {
            box-shadow: 0 0 0 3px rgba(0, 59, 74, 0.1);
        }
        .search-box .search-icon {
            position: absolute;
            left: 12px;
            top: 50%;
            transform: translateY(-50%);
            color: #9CA3AF;
        }
        .search-highlight {
            background: #FBBF24;
            padding: 0 2px;
            border-radius: 2px;
        }

        /* Confirmation Modal */
        .confirm-modal {
            animation: modalIn 0.2s ease;
        }
        @keyframes modalIn {
            from { transform: scale(0.95); opacity: 0; }
            to { transform: scale(1); opacity: 1; }
        }
        .confirm-danger {
            background: #FEE2E2;
            border: 1px solid #FCA5A5;
        }

        /* Keyboard Shortcuts */
        .kbd {
            display: inline-block;
            padding: 2px 6px;
            font-size: 0.7rem;
            font-family: monospace;
            background: #F3F4F6;
            border: 1px solid #D1D5DB;
            border-radius: 4px;
            box-shadow: 0 1px 0 #D1D5DB;
        }
        .shortcuts-modal {
            max-height: 80vh;
            overflow-y: auto;
        }
        .shortcut-group { margin-bottom: 16px; }
        .shortcut-group-title {
            font-weight: 600;
            color: #374151;
            margin-bottom: 8px;
            font-size: 0.875rem;
        }
        .shortcut-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 6px 0;
            font-size: 0.8rem;
        }
        .shortcut-desc { color: #6B7280; }

        /* Loading States */
        .loading-overlay {
            position: absolute;
            inset: 0;
            background: rgba(255, 255, 255, 0.8);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 10;
        }
        .spinner {
            width: 32px;
            height: 32px;
            border: 3px solid #E5E7EB;
            border-top-color: #003B4A;
            border-radius: 50%;
            animation: spin 0.8s linear infinite;
        }
        .spinner-sm {
            width: 16px;
            height: 16px;
            border-width: 2px;
        }
        @keyframes spin {
            to { transform: rotate(360deg); }
        }
        .btn-loading {
            position: relative;
            color: transparent !important;
            pointer-events: none;
        }
        .btn-loading::after {
            content: '';
            position: absolute;
            width: 16px;
            height: 16px;
            top: 50%;
            left: 50%;
            margin-top: -8px;
            margin-left: -8px;
            border: 2px solid rgba(255,255,255,0.3);
            border-top-color: white;
            border-radius: 50%;
            animation: spin 0.6s linear infinite;
        }
        .skeleton {
            background: linear-gradient(90deg, #F3F4F6 25%, #E5E7EB 50%, #F3F4F6 75%);
            background-size: 200% 100%;
            animation: shimmer 1.5s infinite;
            border-radius: 4px;
        }
        @keyframes shimmer {
            0% { background-position: 200% 0; }
            100% { background-position: -200% 0; }
        }
        .skeleton-card {
            height: 120px;
            margin-bottom: 8px;
            border-radius: 8px;
        }

        /* Enhanced Drag and Drop */
        .kanban-column.drop-active {
            background-color: rgba(0, 59, 74, 0.05) !important;
            border: 2px dashed #003B4A !important;
        }
        .story-card.sortable-chosen {
            box-shadow: 0 15px 35px rgba(0, 0, 0, 0.25) !important;
            transform: rotate(2deg) scale(1.02);
        }
        .story-card.sortable-ghost {
            opacity: 0.4;
            background: #E5E7EB;
        }

        /* Quick Actions */
        .story-card {
            position: relative;
        }
        .quick-actions {
            position: absolute;
            top: 4px;
            right: 4px;
            display: flex;
            gap: 2px;
            opacity: 0;
            transition: opacity 0.2s;
        }
        .story-card:hover .quick-actions {
            opacity: 1;
        }
        .quick-btn {
            width: 22px;
            height: 22px;
            border-radius: 4px;
            background: white;
            border: 1px solid #E5E7EB;
            display: flex;
            align-items: center;
            justify-content: center;
            cursor: pointer;
            font-size: 10px;
            color: #6B7280;
            transition: all 0.15s;
        }
        .quick-btn:hover {
            background: #F3F4F6;
            color: #374151;
            transform: scale(1.1);
        }
        .quick-btn.danger:hover {
            background: #FEE2E2;
            color: #DC2626;
        }
        .quick-btn.success:hover {
            background: #D1FAE5;
            color: #059669;
        }

        /* Context Menu */
        .context-menu {
            position: fixed;
            background: white;
            border-radius: 8px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
            min-width: 180px;
            padding: 4px 0;
            z-index: 9999;
            animation: menuIn 0.15s ease;
        }
        @keyframes menuIn {
            from { opacity: 0; transform: scale(0.95); }
            to { opacity: 1; transform: scale(1); }
        }
        .context-menu-item {
            display: flex;
            align-items: center;
            gap: 10px;
            padding: 8px 14px;
            cursor: pointer;
            font-size: 0.8rem;
            color: #374151;
            transition: background 0.1s;
        }
        .context-menu-item:hover {
            background: #F3F4F6;
        }
        .context-menu-item.danger {
            color: #DC2626;
        }
        .context-menu-item.danger:hover {
            background: #FEE2E2;
        }
        .context-menu-divider {
            height: 1px;
            background: #E5E7EB;
            margin: 4px 0;
        }

        /* Micro-interactions */
        .btn-animate {
            transition: transform 0.15s ease, box-shadow 0.15s ease;
        }
        .btn-animate:hover {
            transform: translateY(-1px);
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        }
        .btn-animate:active {
            transform: translateY(0);
        }
        .card-animate {
            animation: cardEnter 0.3s ease;
        }
        @keyframes cardEnter {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .progress-bar {
            transition: width 0.5s ease;
        }

        /* Dark Mode */
        .dark {
            --bg-primary: #1F2937;
            --bg-secondary: #374151;
            --bg-card: #1F2937;
            --text-primary: #F9FAFB;
            --text-secondary: #D1D5DB;
            --border-color: #4B5563;
        }
        .dark body, html.dark body { background-color: #111827 !important; }
        .dark .bg-white { background-color: var(--bg-card) !important; }
        .dark .bg-gray-100 { background-color: #111827 !important; }
        .dark .bg-gray-50 { background-color: var(--bg-secondary) !important; }
        .dark .text-gray-900 { color: var(--text-primary) !important; }
        .dark .text-gray-700 { color: var(--text-secondary) !important; }
        .dark .text-gray-600 { color: #9CA3AF !important; }
        .dark .text-gray-500 { color: #9CA3AF !important; }
        .dark .border-gray-200 { border-color: var(--border-color) !important; }
        .dark .border-gray-300 { border-color: var(--border-color) !important; }
        .dark .hover\\:bg-gray-100:hover { background-color: var(--bg-secondary) !important; }
        .dark .story-card { background-color: var(--bg-card) !important; border-color: var(--border-color) !important; }
        .dark .narrative-box { background: linear-gradient(135deg, #1e3a5f 0%, #1e293b 100%); }
        .dark .kanban-column { background-color: var(--bg-secondary) !important; }
        .dark input, .dark textarea, .dark select {
            background-color: var(--bg-secondary) !important;
            border-color: var(--border-color) !important;
            color: var(--text-primary) !important;
        }
        .dark .kbd {
            background: #374151;
            border-color: #4B5563;
            color: #E5E7EB;
        }
        .dark-mode-toggle {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 6px 12px;
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
        }
        .dark-mode-toggle:hover { background: rgba(255,255,255,0.1); }
        .dark-mode-icon { font-size: 1.1rem; }

        /* Bulk Actions */
        .bulk-toolbar {
            animation: slideUp 0.3s ease;
        }
        @keyframes slideUp {
            from { transform: translate(-50%, 100%); opacity: 0; }
            to { transform: translate(-50%, 0); opacity: 1; }
        }
        .bulk-action-btn {
            padding: 6px 12px;
            border-radius: 6px;
            font-size: 0.8rem;
            font-weight: 500;
            transition: all 0.2s;
            background: rgba(255,255,255,0.1);
        }
        .bulk-action-btn:hover {
            background: rgba(255,255,255,0.2);
        }
        .story-card {
            position: relative;
        }
        .story-card.ring-2 {
            background-color: #EFF6FF !important;
        }
    </style>
</head>
<body class="bg-gray-100">
    <div id="app" :class="{ 'dark': isDarkMode }">
        <!-- HEADER -->
        <header class="belgo-blue text-white shadow-lg">
            <div class="container mx-auto px-4">
                <div class="flex items-center justify-between h-16">
                    <div class="flex items-center gap-4">
                        <div class="flex items-center gap-2">
                            <div class="w-8 h-8 bg-white rounded flex items-center justify-center">
                                <span class="text-belgo-blue font-bold">FA</span>
                            </div>
                            <span class="font-semibold text-lg">Fabrica de Agentes</span>
                        </div>
                        <span class="text-gray-300">|</span>
                        <span class="text-sm opacity-80">Dashboard Agile v6.0</span>
                    </div>

                    <div class="flex items-center gap-4">
                        <!-- Projeto Selecionado -->
                        <select v-model="selectedProjectId" @change="loadProjectData"
                                class="bg-white/10 text-white border border-white/20 rounded px-3 py-1.5 text-sm">
                            <option value="" class="text-gray-800">Selecione um Projeto</option>
                            <option v-for="p in projects" :key="p.project_id" :value="p.project_id" class="text-gray-800">
                                {{ p.name }}
                            </option>
                        </select>

                        <!-- Sprint Ativo -->
                        <select v-if="sprints.length" v-model="selectedSprintId"
                                class="bg-white/10 text-white border border-white/20 rounded px-3 py-1.5 text-sm">
                            <option value="" class="text-gray-800">Todas as Stories</option>
                            <option v-for="s in sprints" :key="s.sprint_id" :value="s.sprint_id" class="text-gray-800">
                                {{ s.name }}
                            </option>
                        </select>

                        <!-- Campo de Busca -->
                        <div class="search-box" v-if="selectedProjectId">
                            <svg class="search-icon w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                            </svg>
                            <input v-model="searchQuery"
                                   @keyup.escape="searchQuery = ''"
                                   type="text"
                                   placeholder="Buscar stories... (pressione /)"
                                   ref="searchInput"
                                   class="bg-white/10 text-white placeholder-gray-300 border border-white/20 rounded px-3 py-1.5 text-sm w-56 focus:w-72 transition-all focus:outline-none">
                        </div>

                        <!-- Atalhos de Teclado -->
                        <button @click="showShortcutsModal = true"
                                class="text-white/70 hover:text-white p-2 transition"
                                title="Atalhos de teclado (?)">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"/>
                            </svg>
                        </button>

                        <!-- Dark Mode Toggle -->
                        <button @click="toggleDarkMode"
                                class="dark-mode-toggle text-white/70 hover:text-white"
                                :title="isDarkMode ? 'Modo Claro' : 'Modo Escuro'">
                            <span v-if="!isDarkMode" class="dark-mode-icon">🌙</span>
                            <span v-else class="dark-mode-icon">☀️</span>
                        </button>

                        <!-- Nova Story -->
                        <button @click="showNewStoryModal = true"
                                class="bg-[#FF6C00] hover:bg-orange-600 px-4 py-1.5 rounded text-sm font-medium transition">
                            + Nova Story
                        </button>
                    </div>
                </div>
            </div>
        </header>

        <div class="flex" style="height: calc(100vh - 64px);">
            <!-- SIDEBAR -->
            <aside class="w-64 bg-white border-r border-gray-200 overflow-y-auto">
                <div class="p-4">
                    <!-- Projetos -->
                    <div class="mb-6">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Projetos</h3>
                        <div v-for="p in projects" :key="p.project_id"
                             @click="selectedProjectId = p.project_id; loadProjectData()"
                             :class="['p-2 rounded cursor-pointer text-sm',
                                      selectedProjectId === p.project_id ? 'bg-blue-50 text-belgo-blue' : 'hover:bg-gray-100']">
                            <div class="font-medium">{{ p.name }}</div>
                            <div class="text-xs text-gray-500">{{ p.project_type }}</div>
                        </div>
                    </div>

                    <!-- Epics -->
                    <div class="mb-6" v-if="epics.length">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Epics</h3>
                        <div v-for="e in epics" :key="e.epic_id"
                             @click="filterByEpic(e.epic_id)"
                             :class="['p-2 rounded cursor-pointer text-sm flex items-center gap-2',
                                      selectedEpicId === e.epic_id ? 'bg-blue-50' : 'hover:bg-gray-100']">
                            <div class="w-3 h-3 rounded" :style="{backgroundColor: e.color}"></div>
                            <span>{{ e.title }}</span>
                        </div>
                    </div>

                    <!-- Metricas -->
                    <div class="mb-6" v-if="selectedProjectId">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Metricas</h3>
                        <div class="space-y-2 text-sm">
                            <div class="flex justify-between">
                                <span class="text-gray-600">Total Stories</span>
                                <span class="font-medium">{{ totalStories }}</span>
                            </div>
                            <div class="flex justify-between">
                                <span class="text-gray-600">Concluidas</span>
                                <span class="font-medium text-green-600">{{ doneStories }}</span>
                            </div>
                            <div class="flex justify-between">
                                <span class="text-gray-600">Em Progresso</span>
                                <span class="font-medium text-blue-600">{{ inProgressStories }}</span>
                            </div>
                            <div class="flex justify-between">
                                <span class="text-gray-600">Story Points</span>
                                <span class="font-medium">{{ totalPoints }}</span>
                            </div>
                        </div>
                    </div>

                    <!-- Sprint Burndown -->
                    <div class="mb-6" v-if="selectedProjectId && sprints.length">
                        <div class="flex justify-between items-center mb-2">
                            <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider">Burndown</h3>
                            <button @click="showBurndownModal = true" class="text-xs text-blue-600 hover:underline">
                                Expandir
                            </button>
                        </div>
                        <div class="bg-gray-50 rounded-lg p-2">
                            <canvas id="burndown-mini" height="100"></canvas>
                        </div>
                        <div class="mt-2 text-xs text-gray-500 text-center">
                            {{ burndownData.remaining }} pts restantes
                        </div>
                    </div>

                    <!-- Acoes -->
                    <div class="space-y-2">
                        <button @click="showNewEpicModal = true"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded">
                            + Novo Epic
                        </button>
                        <button @click="showNewSprintModal = true"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded">
                            + Novo Sprint
                        </button>
                    </div>
                </div>
            </aside>

            <!-- MAIN CONTENT - KANBAN -->
            <main class="flex-1 overflow-x-auto bg-gray-50 p-4">
                <div v-if="!selectedProjectId" class="flex items-center justify-center h-full text-gray-500">
                    <div class="text-center max-w-md">
                        <div class="text-6xl mb-4">🚀</div>
                        <h2 class="text-xl font-semibold text-gray-700 mb-2">Bem-vindo a Fabrica de Agentes!</h2>
                        <p class="text-gray-500 mb-6">Selecione um projeto na barra lateral ou crie um novo para comecar.</p>
                        <div class="space-y-3 text-left bg-white p-4 rounded-lg shadow-sm">
                            <div class="flex items-center gap-3 text-sm">
                                <span class="w-6 h-6 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-semibold">1</span>
                                <span>Crie ou selecione um projeto</span>
                            </div>
                            <div class="flex items-center gap-3 text-sm">
                                <span class="w-6 h-6 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-semibold">2</span>
                                <span>Adicione User Stories com narrativa Agile</span>
                            </div>
                            <div class="flex items-center gap-3 text-sm">
                                <span class="w-6 h-6 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-semibold">3</span>
                                <span>Arraste stories pelo Kanban</span>
                            </div>
                            <div class="flex items-center gap-3 text-sm">
                                <span class="w-6 h-6 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-semibold">4</span>
                                <span>Use o chat para comandos rapidos</span>
                            </div>
                        </div>
                        <p class="text-xs text-gray-400 mt-4">Dica: Pressione <span class="kbd">?</span> para ver atalhos de teclado</p>
                    </div>
                </div>

                <div v-else class="flex flex-col h-full">
                    <!-- Barra de Filtros -->
                    <div class="flex items-center gap-3 mb-4 flex-wrap">
                        <div class="flex items-center gap-2 bg-white px-3 py-1.5 rounded-lg shadow-sm">
                            <span class="text-xs text-gray-500">Prioridade:</span>
                            <select v-model="filterPriority" class="text-sm border-0 bg-transparent focus:ring-0 cursor-pointer">
                                <option value="">Todas</option>
                                <option value="urgent">Urgente</option>
                                <option value="high">Alta</option>
                                <option value="medium">Media</option>
                                <option value="low">Baixa</option>
                            </select>
                        </div>
                        <div class="flex items-center gap-2 bg-white px-3 py-1.5 rounded-lg shadow-sm">
                            <span class="text-xs text-gray-500">Assignee:</span>
                            <select v-model="filterAssignee" class="text-sm border-0 bg-transparent focus:ring-0 cursor-pointer">
                                <option value="">Todos</option>
                                <option value="unassigned">Sem assignee</option>
                            </select>
                        </div>
                        <div v-if="searchQuery || filterPriority || filterAssignee"
                             class="flex items-center gap-2 text-xs text-gray-500">
                            <span class="bg-blue-100 text-blue-700 px-2 py-0.5 rounded">
                                Filtros ativos
                            </span>
                            <button @click="clearFilters" class="text-gray-400 hover:text-gray-600">
                                Limpar
                            </button>
                        </div>
                        <!-- Bulk Select Toggle -->
                        <button @click="toggleBulkSelectMode"
                                :class="['px-3 py-1.5 rounded-lg text-sm font-medium transition',
                                         bulkSelectMode ? 'bg-blue-500 text-white' : 'bg-white shadow-sm text-gray-600 hover:bg-gray-50']">
                            <span v-if="bulkSelectMode">{{ selectedStories.length }} selecionadas</span>
                            <span v-else>Selecionar</span>
                        </button>
                        <div class="ml-auto text-xs text-gray-500">
                            {{ filteredStoriesCount }} stories
                        </div>
                    </div>

                    <!-- Bulk Actions Toolbar -->
                    <div v-if="bulkSelectMode && selectedStories.length > 0"
                         class="fixed bottom-6 left-1/2 transform -translate-x-1/2 z-50
                                bg-[#003B4A] text-white px-6 py-3 rounded-full shadow-xl
                                flex items-center gap-4 bulk-toolbar">
                        <span class="text-sm font-medium">{{ selectedStories.length }} stories selecionadas</span>
                        <div class="h-4 w-px bg-white/30"></div>
                        <button @click="bulkMoveStories('ready')" class="bulk-action-btn" title="Mover para Ready">
                            Ready
                        </button>
                        <button @click="bulkMoveStories('in_progress')" class="bulk-action-btn" title="Mover para In Progress">
                            In Progress
                        </button>
                        <button @click="bulkMoveStories('done')" class="bulk-action-btn" title="Mover para Done">
                            Done
                        </button>
                        <div class="h-4 w-px bg-white/30"></div>
                        <button @click="bulkDeleteStories" class="bulk-action-btn text-red-400 hover:text-red-300" title="Excluir selecionadas">
                            🗑️ Excluir
                        </button>
                        <button @click="cancelBulkSelect" class="bulk-action-btn opacity-70" title="Cancelar">
                            ✕
                        </button>
                    </div>

                    <!-- Colunas do Kanban -->
                    <div v-for="(column, status) in filteredStoryBoard" :key="status"
                         class="flex-shrink-0 w-80 bg-gray-100 rounded-lg">
                        <!-- Header da Coluna -->
                        <div class="p-3 border-b border-gray-200 bg-white rounded-t-lg">
                            <div class="flex items-center justify-between">
                                <div class="flex items-center gap-2">
                                    <span class="font-semibold text-gray-700">{{ getColumnTitle(status) }}</span>
                                    <span class="bg-gray-200 text-gray-600 text-xs px-2 py-0.5 rounded-full">
                                        {{ column.length }}
                                    </span>
                                </div>
                                <span class="text-xs text-gray-500">{{ getColumnPoints(column) }} pts</span>
                            </div>
                        </div>

                        <!-- Lista de Stories -->
                        <div :id="'column-' + status"
                             class="kanban-column p-2 space-y-2 overflow-y-auto"
                             style="max-height: calc(100vh - 200px);">
                            <!-- Story Card -->
                            <div v-for="story in column" :key="story.story_id"
                                 @click="bulkSelectMode ? toggleBulkSelect(story) : openStoryDetail(story)"
                                 @contextmenu.prevent="showContextMenu($event, story)"
                                 :data-id="story.story_id"
                                 :class="['story-card bg-white rounded-lg shadow p-3 card-animate',
                                          'priority-' + story.priority,
                                          selectedStories.includes(story.story_id) ? 'ring-2 ring-blue-500' : '']">
                                <!-- Bulk Select Checkbox -->
                                <div v-if="bulkSelectMode" class="absolute top-2 left-2" @click.stop>
                                    <input type="checkbox"
                                           :checked="selectedStories.includes(story.story_id)"
                                           @change="toggleBulkSelect(story)"
                                           class="w-4 h-4 text-blue-600 rounded border-gray-300">
                                </div>
                                <!-- Quick Actions -->
                                <div class="quick-actions" @click.stop>
                                    <button @click="moveToNextColumn(story)" class="quick-btn success" title="Mover para proxima coluna">
                                        <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                                        </svg>
                                    </button>
                                    <button @click="deleteStoryWithConfirm(story)" class="quick-btn danger" title="Excluir">
                                        <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                        </svg>
                                    </button>
                                </div>

                                <!-- Epic + Points -->
                                <div class="flex items-center justify-between mb-2">
                                    <span v-if="story.epic_id" class="text-xs px-2 py-0.5 rounded bg-blue-100 text-blue-700">
                                        {{ getEpicName(story.epic_id) }}
                                    </span>
                                    <span v-else class="text-xs text-gray-400">Sem Epic</span>
                                    <span class="text-xs font-medium text-gray-600">{{ story.story_points }} pts</span>
                                </div>

                                <!-- Titulo -->
                                <h4 class="font-medium text-gray-800 text-sm mb-2 line-clamp-2">
                                    {{ story.title }}
                                </h4>

                                <!-- Progress Bar -->
                                <div class="mb-2">
                                    <div class="flex justify-between text-xs text-gray-500 mb-1">
                                        <span>{{ story.tasks_completed }}/{{ story.tasks_total }} tasks</span>
                                        <span>{{ Math.round(story.progress) }}%</span>
                                    </div>
                                    <div class="h-1.5 bg-gray-200 rounded-full overflow-hidden">
                                        <div class="progress-bar h-full bg-green-500 rounded-full"
                                             :style="{width: story.progress + '%'}"></div>
                                    </div>
                                </div>

                                <!-- Footer -->
                                <div class="flex items-center justify-between text-xs text-gray-500">
                                    <span>{{ story.story_id }}</span>
                                    <span v-if="story.assignee" class="flex items-center gap-1">
                                        <span class="w-5 h-5 bg-gray-300 rounded-full flex items-center justify-center text-xs">
                                            {{ story.assignee.charAt(0).toUpperCase() }}
                                        </span>
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </main>

            <!-- CHAT PANEL -->
            <aside class="w-80 bg-white border-l border-gray-200 flex flex-col">
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white">
                    <div class="flex items-center gap-2">
                        <span class="text-xl">🤖</span>
                        <span class="font-semibold">Assistente</span>
                    </div>
                </div>

                <!-- Mensagens -->
                <div class="flex-1 overflow-y-auto p-4 chat-messages" ref="chatMessages">
                    <div v-for="msg in chatHistory" :key="msg.message_id"
                         :class="['mb-4', msg.role === 'user' ? 'text-right' : 'text-left']">
                        <div :class="['inline-block max-w-[90%] p-3 rounded-lg text-sm',
                                      msg.role === 'user'
                                        ? 'bg-[#003B4A] text-white rounded-br-none'
                                        : 'bg-gray-100 text-gray-800 rounded-bl-none']">
                            <div v-if="msg.role === 'assistant'" class="markdown-content" v-html="renderMarkdown(msg.content)"></div>
                            <div v-else>{{ msg.content }}</div>
                        </div>
                        <div class="text-xs text-gray-400 mt-1">
                            {{ formatTime(msg.created_at) }}
                        </div>
                    </div>
                </div>

                <!-- Input -->
                <div class="p-4 border-t border-gray-200">
                    <div class="flex gap-2">
                        <input v-model="chatInput"
                               @keyup.enter="sendMessage"
                               type="text"
                               placeholder="Digite sua mensagem..."
                               class="flex-1 border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:border-[#003B4A]">
                        <button @click="sendMessage"
                                class="bg-[#FF6C00] text-white px-4 py-2 rounded-lg hover:bg-orange-600 transition">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"/>
                            </svg>
                        </button>
                    </div>
                </div>
            </aside>
        </div>

        <!-- STORY DETAIL PANEL (Slide-over) -->
        <div v-if="selectedStory"
             class="fixed inset-0 bg-black/50 z-50"
             @click.self="selectedStory = null">
            <div :class="['slide-panel fixed right-0 top-0 h-full w-[600px] bg-white shadow-2xl',
                          selectedStory ? 'open' : '']">
                <!-- Header -->
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white">
                    <div class="flex items-center justify-between">
                        <div>
                            <span class="text-sm opacity-70">{{ selectedStory.story_id }}</span>
                            <h2 class="text-lg font-semibold">{{ selectedStory.title }}</h2>
                        </div>
                        <button @click="selectedStory = null" class="text-white/70 hover:text-white">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <!-- Tabs -->
                <div class="border-b border-gray-200">
                    <div class="flex">
                        <button v-for="tab in ['Detalhes', 'Tasks', 'Docs', 'Anexos']" :key="tab"
                                @click="activeTab = tab"
                                :class="['px-4 py-3 text-sm font-medium',
                                         activeTab === tab ? 'tab-active' : 'text-gray-500 hover:text-gray-700']">
                            {{ tab }}
                        </button>
                    </div>
                </div>

                <!-- Content -->
                <div class="overflow-y-auto" style="height: calc(100% - 140px);">
                    <!-- Tab: Detalhes -->
                    <div v-if="activeTab === 'Detalhes'" class="p-4 space-y-4">
                        <!-- Narrativa -->
                        <div class="narrative-box p-4 rounded-lg">
                            <h3 class="font-semibold text-gray-700 mb-2">Narrativa</h3>
                            <div class="space-y-1 text-sm">
                                <p><span class="text-gray-500">Como um</span>
                                   <span class="font-medium text-belgo-blue">{{ selectedStory.persona || '[persona]' }}</span></p>
                                <p><span class="text-gray-500">Eu quero</span>
                                   <span class="font-medium text-belgo-blue">{{ selectedStory.action || '[acao]' }}</span></p>
                                <p><span class="text-gray-500">Para que</span>
                                   <span class="font-medium text-belgo-blue">{{ selectedStory.benefit || '[beneficio]' }}</span></p>
                            </div>
                        </div>

                        <!-- Descricao -->
                        <div v-if="selectedStory.description">
                            <h3 class="font-semibold text-gray-700 mb-2">Descricao</h3>
                            <p class="text-sm text-gray-600">{{ selectedStory.description }}</p>
                        </div>

                        <!-- Criterios de Aceite -->
                        <div>
                            <h3 class="font-semibold text-gray-700 mb-2">Criterios de Aceite</h3>
                            <div v-if="selectedStory.acceptance_criteria?.length" class="space-y-1">
                                <div v-for="(c, i) in selectedStory.acceptance_criteria" :key="i"
                                     class="criteria-item flex items-start gap-2 p-2 rounded text-sm">
                                    <input type="checkbox" class="mt-1">
                                    <span>{{ c }}</span>
                                </div>
                            </div>
                            <p v-else class="text-sm text-gray-400 italic">Nenhum criterio definido</p>
                        </div>

                        <!-- Definition of Done -->
                        <div>
                            <h3 class="font-semibold text-gray-700 mb-2">Definition of Done</h3>
                            <div v-if="selectedStory.definition_of_done?.length" class="space-y-1">
                                <div v-for="(d, i) in selectedStory.definition_of_done" :key="i"
                                     class="criteria-item flex items-start gap-2 p-2 rounded text-sm">
                                    <input type="checkbox" class="mt-1">
                                    <span>{{ d }}</span>
                                </div>
                            </div>
                            <p v-else class="text-sm text-gray-400 italic">Nenhum item definido</p>
                        </div>

                        <!-- Metadados -->
                        <div class="grid grid-cols-2 gap-4 pt-4 border-t border-gray-200">
                            <div>
                                <span class="text-xs text-gray-500">Story Points</span>
                                <div class="font-medium">{{ selectedStory.story_points }}</div>
                            </div>
                            <div>
                                <span class="text-xs text-gray-500">Complexidade</span>
                                <div class="font-medium capitalize">{{ selectedStory.complexity }}</div>
                            </div>
                            <div>
                                <span class="text-xs text-gray-500">Prioridade</span>
                                <div class="font-medium capitalize">{{ selectedStory.priority }}</div>
                            </div>
                            <div>
                                <span class="text-xs text-gray-500">Categoria</span>
                                <div class="font-medium capitalize">{{ selectedStory.category }}</div>
                            </div>
                        </div>

                        <!-- Botoes de Acao -->
                        <div class="pt-4 space-y-2">
                            <button @click="editStory"
                                    class="w-full bg-[#003B4A] text-white py-2 rounded-lg hover:bg-opacity-90 transition">
                                Editar Story
                            </button>
                            <button @click="deleteStoryWithConfirm(selectedStory)"
                                    class="w-full bg-white text-red-600 border border-red-300 py-2 rounded-lg hover:bg-red-50 transition">
                                Excluir Story
                            </button>
                        </div>
                    </div>

                    <!-- Tab: Tasks -->
                    <div v-if="activeTab === 'Tasks'" class="p-4">
                        <div class="flex justify-between items-center mb-4">
                            <h3 class="font-semibold">Tasks ({{ selectedStory.tasks?.length || 0 }})</h3>
                            <button @click="showNewTaskModal = true"
                                    class="text-sm bg-[#FF6C00] text-white px-3 py-1 rounded hover:bg-orange-600">
                                + Nova Task
                            </button>
                        </div>

                        <div v-if="selectedStory.tasks?.length" class="space-y-2">
                            <div v-for="task in selectedStory.tasks" :key="task.task_id"
                                 class="border border-gray-200 rounded-lg p-3 hover:border-gray-300 transition">
                                <div class="flex items-start justify-between">
                                    <div class="flex items-start gap-2">
                                        <input type="checkbox"
                                               :checked="task.status === 'completed'"
                                               @change="toggleTaskComplete(task)"
                                               class="mt-1">
                                        <div>
                                            <div class="font-medium text-sm">{{ task.title }}</div>
                                            <div class="text-xs text-gray-500">{{ task.task_id }} | {{ task.task_type }}</div>
                                        </div>
                                    </div>
                                    <div class="flex items-center gap-2">
                                        <span :class="['text-xs px-2 py-0.5 rounded', getTaskStatusClass(task.status)]">
                                            {{ task.status }}
                                        </span>
                                        <button @click.stop="deleteTaskWithConfirm(task)"
                                                class="text-gray-400 hover:text-red-500 transition p-1"
                                                title="Excluir task">
                                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                            </svg>
                                        </button>
                                    </div>
                                </div>

                                <!-- Output Tecnico -->
                                <div v-if="task.files_created?.length || task.code_output"
                                     class="mt-2 pt-2 border-t border-gray-100 text-xs text-gray-600">
                                    <div v-if="task.files_created?.length">
                                        <span class="font-medium">Arquivos:</span> {{ task.files_created.join(', ') }}
                                    </div>
                                </div>
                            </div>
                        </div>
                        <p v-else class="text-gray-400 text-sm italic">Nenhuma task criada</p>
                    </div>

                    <!-- Tab: Docs -->
                    <div v-if="activeTab === 'Docs'" class="p-4">
                        <div class="flex justify-between items-center mb-4">
                            <h3 class="font-semibold">Documentacao</h3>
                            <button @click="showNewDocModal = true"
                                    class="text-sm bg-[#FF6C00] text-white px-3 py-1 rounded hover:bg-orange-600">
                                + Novo Doc
                            </button>
                        </div>

                        <div v-if="selectedStory.docs?.length" class="space-y-3">
                            <div v-for="doc in selectedStory.docs" :key="doc.doc_id"
                                 class="border border-gray-200 rounded-lg p-3">
                                <div class="flex justify-between items-start mb-2">
                                    <div>
                                        <span :class="['text-xs px-2 py-0.5 rounded mr-2', getDocTypeClass(doc.doc_type)]">
                                            {{ doc.doc_type }}
                                        </span>
                                        <span class="font-medium text-sm">{{ doc.title }}</span>
                                    </div>
                                </div>
                                <div v-if="doc.content" class="text-sm text-gray-600 markdown-content"
                                     v-html="renderMarkdown(doc.content)"></div>
                                <div v-if="doc.test_instructions" class="mt-2 pt-2 border-t border-gray-100">
                                    <span class="text-xs font-medium text-gray-500">Como Testar:</span>
                                    <p class="text-sm text-gray-600">{{ doc.test_instructions }}</p>
                                </div>
                            </div>
                        </div>
                        <p v-else class="text-gray-400 text-sm italic">Nenhuma documentacao</p>
                    </div>

                    <!-- Tab: Anexos -->
                    <div v-if="activeTab === 'Anexos'" class="p-4">
                        <div class="flex justify-between items-center mb-4">
                            <h3 class="font-semibold">Anexos</h3>
                            <label class="text-sm bg-[#FF6C00] text-white px-3 py-1 rounded hover:bg-orange-600 cursor-pointer">
                                + Upload
                                <input type="file" class="hidden" @change="uploadFile">
                            </label>
                        </div>

                        <div v-if="selectedStory.files?.length" class="space-y-2">
                            <div v-for="file in selectedStory.files" :key="file.attachment_id"
                                 class="flex items-center justify-between p-2 bg-gray-50 rounded">
                                <div class="flex items-center gap-2">
                                    <span class="text-xl">📎</span>
                                    <div>
                                        <div class="text-sm font-medium">{{ file.original_filename }}</div>
                                        <div class="text-xs text-gray-500">{{ formatFileSize(file.file_size) }}</div>
                                    </div>
                                </div>
                                <a :href="'/api/attachments/' + file.attachment_id"
                                   class="text-blue-600 text-sm hover:underline">Download</a>
                            </div>
                        </div>
                        <p v-else class="text-gray-400 text-sm italic">Nenhum anexo</p>
                    </div>
                </div>
            </div>
        </div>

        <!-- MODAL: Nova Story -->
        <div v-if="showNewStoryModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[700px] max-h-[90vh] overflow-y-auto dark:bg-gray-800">
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white rounded-t-lg flex justify-between items-center">
                    <h2 class="text-lg font-semibold">Nova User Story</h2>
                    <div class="flex items-center gap-2">
                        <span class="text-sm opacity-80">Template:</span>
                        <select v-model="selectedTemplate" @change="applyTemplate"
                                class="bg-white/20 border border-white/30 rounded px-2 py-1 text-sm text-white">
                            <option value="">Selecionar...</option>
                            <option value="feature">Feature</option>
                            <option value="bugfix">Bug Fix</option>
                            <option value="tech_debt">Tech Debt</option>
                            <option value="spike">Spike/Pesquisa</option>
                            <option value="improvement">Melhoria</option>
                        </select>
                    </div>
                </div>
                <div class="p-6 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1 dark:text-gray-300">Titulo *</label>
                        <input v-model="newStory.title" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2 dark:bg-gray-700 dark:border-gray-600 dark:text-white"
                               placeholder="Ex: Implementar login com email">
                    </div>

                    <div class="grid grid-cols-3 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Como um</label>
                            <input v-model="newStory.persona" type="text"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                   placeholder="usuario do sistema">
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Eu quero</label>
                            <input v-model="newStory.action" type="text"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                   placeholder="fazer login">
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Para que</label>
                            <input v-model="newStory.benefit" type="text"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                   placeholder="acesse minhas informacoes">
                        </div>
                    </div>

                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newStory.description" rows="3"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                  placeholder="Detalhes adicionais..."></textarea>
                    </div>

                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Criterios de Aceite (um por linha)</label>
                        <textarea v-model="newStoryCriteria" rows="3"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                  placeholder="Usuario pode fazer login com email&#10;Senha deve ter minimo 8 caracteres"></textarea>
                    </div>

                    <div class="grid grid-cols-4 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Story Points</label>
                            <select v-model="newStory.story_points" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option :value="0">0</option>
                                <option :value="1">1</option>
                                <option :value="2">2</option>
                                <option :value="3">3</option>
                                <option :value="5">5</option>
                                <option :value="8">8</option>
                                <option :value="13">13</option>
                                <option :value="21">21</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Prioridade</label>
                            <select v-model="newStory.priority" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="low">Baixa</option>
                                <option value="medium">Media</option>
                                <option value="high">Alta</option>
                                <option value="urgent">Urgente</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Complexidade</label>
                            <select v-model="newStory.complexity" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="low">Baixa</option>
                                <option value="medium">Media</option>
                                <option value="high">Alta</option>
                                <option value="very_high">Muito Alta</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Categoria</label>
                            <select v-model="newStory.category" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="feature">Feature</option>
                                <option value="bug">Bug</option>
                                <option value="tech_debt">Tech Debt</option>
                                <option value="spike">Spike</option>
                                <option value="improvement">Melhoria</option>
                            </select>
                        </div>
                    </div>

                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Epic</label>
                            <select v-model="newStory.epic_id" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="">Nenhum</option>
                                <option v-for="e in epics" :key="e.epic_id" :value="e.epic_id">{{ e.title }}</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Sprint</label>
                            <select v-model="newStory.sprint_id" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="">Nenhum</option>
                                <option v-for="s in sprints" :key="s.sprint_id" :value="s.sprint_id">{{ s.name }}</option>
                            </select>
                        </div>
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewStoryModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">
                        Cancelar
                    </button>
                    <button @click="createStory"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">
                        Criar Story
                    </button>
                </div>
            </div>
        </div>

        <!-- MODAL: Nova Task -->
        <div v-if="showNewTaskModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[500px]">
                <div class="p-4 border-b border-gray-200">
                    <h2 class="text-lg font-semibold">Nova Task</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Titulo *</label>
                        <input v-model="newTask.title" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newTask.description" rows="2"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"></textarea>
                    </div>
                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Tipo</label>
                            <select v-model="newTask.task_type" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="development">Desenvolvimento</option>
                                <option value="review">Review</option>
                                <option value="test">Teste</option>
                                <option value="documentation">Documentacao</option>
                                <option value="design">Design</option>
                                <option value="research">Pesquisa</option>
                            </select>
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Estimativa (h)</label>
                            <input v-model.number="newTask.estimated_hours" type="number"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2">
                        </div>
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewTaskModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createTask"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">Criar Task</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Novo Epic -->
        <div v-if="showNewEpicModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[400px]">
                <div class="p-4 border-b border-gray-200">
                    <h2 class="text-lg font-semibold">Novo Epic</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Titulo *</label>
                        <input v-model="newEpic.title" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newEpic.description" rows="2"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"></textarea>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Cor</label>
                        <input v-model="newEpic.color" type="color" class="w-full h-10 rounded-lg">
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewEpicModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createEpic"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">Criar Epic</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Novo Sprint -->
        <div v-if="showNewSprintModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[400px]">
                <div class="p-4 border-b border-gray-200">
                    <h2 class="text-lg font-semibold">Novo Sprint</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Nome *</label>
                        <input v-model="newSprint.name" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2"
                               placeholder="Ex: Sprint 1">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Goal</label>
                        <textarea v-model="newSprint.goal" rows="2"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"></textarea>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Capacidade (pts)</label>
                        <input v-model.number="newSprint.capacity" type="number"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2">
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewSprintModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createSprint"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">Criar Sprint</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Novo Doc -->
        <div v-if="showNewDocModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[600px]">
                <div class="p-4 border-b border-gray-200">
                    <h2 class="text-lg font-semibold">Nova Documentacao</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Titulo *</label>
                            <input v-model="newDoc.title" type="text"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2">
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Tipo</label>
                            <select v-model="newDoc.doc_type" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="technical">Tecnico</option>
                                <option value="user">Usuario</option>
                                <option value="test">Teste</option>
                                <option value="deployment">Deploy</option>
                                <option value="api">API</option>
                            </select>
                        </div>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Conteudo (Markdown)</label>
                        <textarea v-model="newDoc.content" rows="6"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2 font-mono text-sm"></textarea>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Como Testar</label>
                        <textarea v-model="newDoc.test_instructions" rows="3"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"></textarea>
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewDocModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createDoc"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">Criar Doc</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Confirmacao de Exclusao -->
        <div v-if="showConfirmModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="confirm-modal bg-white rounded-lg w-[400px] shadow-xl">
                <div class="p-4 border-b border-gray-200">
                    <div class="flex items-center gap-3">
                        <div class="w-10 h-10 rounded-full bg-red-100 flex items-center justify-center">
                            <svg class="w-5 h-5 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                            </svg>
                        </div>
                        <h2 class="text-lg font-semibold text-gray-900">{{ confirmModal.title }}</h2>
                    </div>
                </div>
                <div class="p-4">
                    <p class="text-gray-600 text-sm">{{ confirmModal.message }}</p>
                    <div v-if="confirmModal.itemName" class="mt-3 p-3 confirm-danger rounded-lg">
                        <span class="font-medium text-red-800">{{ confirmModal.itemName }}</span>
                    </div>
                    <p class="mt-3 text-xs text-gray-500">Esta acao nao pode ser desfeita.</p>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="cancelConfirm"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg transition">
                        Cancelar
                    </button>
                    <button @click="executeConfirm"
                            class="px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700 transition">
                        {{ confirmModal.confirmText || 'Excluir' }}
                    </button>
                </div>
            </div>
        </div>

        <!-- MODAL: Atalhos de Teclado -->
        <div v-if="showShortcutsModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
             @click.self="showShortcutsModal = false">
            <div class="bg-white rounded-lg w-[500px] shadow-xl shortcuts-modal">
                <div class="p-4 border-b border-gray-200 flex justify-between items-center">
                    <h2 class="text-lg font-semibold">Atalhos de Teclado</h2>
                    <button @click="showShortcutsModal = false" class="text-gray-400 hover:text-gray-600">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
                <div class="p-4">
                    <div class="shortcut-group">
                        <div class="shortcut-group-title">Navegacao</div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Focar no campo de busca</span>
                            <span class="kbd">/</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Fechar modal/painel</span>
                            <span class="kbd">Esc</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mostrar atalhos</span>
                            <span class="kbd">?</span>
                        </div>
                    </div>
                    <div class="shortcut-group">
                        <div class="shortcut-group-title">Acoes</div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Nova Story</span>
                            <span class="kbd">N</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Nova Task (com story aberta)</span>
                            <span class="kbd">T</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Editar story selecionada</span>
                            <span class="kbd">E</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Deletar story selecionada</span>
                            <span class="kbd">Del</span>
                        </div>
                    </div>
                    <div class="shortcut-group">
                        <div class="shortcut-group-title">Mover Story</div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para Backlog</span>
                            <span class="kbd">1</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para Ready</span>
                            <span class="kbd">2</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para In Progress</span>
                            <span class="kbd">3</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para Review</span>
                            <span class="kbd">4</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para Testing</span>
                            <span class="kbd">5</span>
                        </div>
                        <div class="shortcut-item">
                            <span class="shortcut-desc">Mover para Done</span>
                            <span class="kbd">6</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- MODAL: Sprint Burndown Chart -->
        <div v-if="showBurndownModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
             @click.self="showBurndownModal = false">
            <div class="bg-white rounded-lg w-[700px] shadow-xl dark:bg-gray-800">
                <div class="p-4 border-b border-gray-200 flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <h2 class="text-lg font-semibold">Sprint Burndown Chart</h2>
                    <button @click="showBurndownModal = false" class="text-white/70 hover:text-white">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
                <div class="p-6">
                    <div class="mb-4 flex items-center gap-4">
                        <div>
                            <label class="text-sm text-gray-500">Sprint:</label>
                            <select v-model="selectedSprintId" @change="updateBurndownChart"
                                    class="ml-2 border rounded px-2 py-1 text-sm">
                                <option value="">Todos</option>
                                <option v-for="s in sprints" :key="s.sprint_id" :value="s.sprint_id">{{ s.name }}</option>
                            </select>
                        </div>
                        <div class="flex-1"></div>
                        <div class="text-sm text-gray-500">
                            <span class="inline-flex items-center gap-1"><span class="w-3 h-0.5 bg-blue-500 inline-block"></span> Ideal</span>
                            <span class="ml-3 inline-flex items-center gap-1"><span class="w-3 h-0.5 bg-orange-500 inline-block"></span> Real</span>
                        </div>
                    </div>
                    <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-4" style="height: 300px;">
                        <canvas id="burndown-full"></canvas>
                    </div>
                    <div class="mt-4 grid grid-cols-4 gap-4 text-center">
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
                            <div class="text-2xl font-bold text-[#003B4A] dark:text-blue-400">{{ burndownData.total }}</div>
                            <div class="text-xs text-gray-500">Total Points</div>
                        </div>
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
                            <div class="text-2xl font-bold text-green-600">{{ burndownData.completed }}</div>
                            <div class="text-xs text-gray-500">Completos</div>
                        </div>
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
                            <div class="text-2xl font-bold text-orange-500">{{ burndownData.remaining }}</div>
                            <div class="text-xs text-gray-500">Restantes</div>
                        </div>
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
                            <div class="text-2xl font-bold" :class="burndownData.velocity >= 0 ? 'text-green-600' : 'text-red-500'">
                                {{ burndownData.velocity > 0 ? '+' : '' }}{{ burndownData.velocity }}
                            </div>
                            <div class="text-xs text-gray-500">Velocity</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- CONTEXT MENU -->
        <div v-if="contextMenu.visible"
             class="context-menu"
             :style="{top: contextMenu.y + 'px', left: contextMenu.x + 'px'}"
             @click.stop>
            <div class="context-menu-item" @click="contextMenuAction('open')">
                <span>📋</span> Abrir detalhes
            </div>
            <div class="context-menu-divider"></div>
            <div class="context-menu-item" @click="contextMenuAction('backlog')">
                <span>1️⃣</span> Mover para Backlog
            </div>
            <div class="context-menu-item" @click="contextMenuAction('ready')">
                <span>2️⃣</span> Mover para Ready
            </div>
            <div class="context-menu-item" @click="contextMenuAction('in_progress')">
                <span>3️⃣</span> Mover para In Progress
            </div>
            <div class="context-menu-item" @click="contextMenuAction('review')">
                <span>4️⃣</span> Mover para Review
            </div>
            <div class="context-menu-item" @click="contextMenuAction('testing')">
                <span>5️⃣</span> Mover para Testing
            </div>
            <div class="context-menu-item" @click="contextMenuAction('done')">
                <span>6️⃣</span> Mover para Done
            </div>
            <div class="context-menu-divider"></div>
            <div class="context-menu-item" @click="contextMenuAction('copy')">
                <span>📄</span> Copiar ID
            </div>
            <div class="context-menu-item danger" @click="contextMenuAction('delete')">
                <span>🗑️</span> Excluir
            </div>
        </div>

        <!-- TOAST CONTAINER -->
        <div class="toast-container">
            <div v-for="toast in toasts" :key="toast.id"
                 :class="['toast', 'toast-' + toast.type]">
                <span class="toast-icon">{{ getToastIcon(toast.type) }}</span>
                <div class="toast-content">
                    <div class="toast-title">{{ toast.title }}</div>
                    <div v-if="toast.message" class="toast-message">{{ toast.message }}</div>
                </div>
                <button v-if="toast.undoAction" @click="handleUndo(toast)" class="toast-undo">Desfazer</button>
                <button @click="removeToast(toast.id)" class="toast-close">&times;</button>
            </div>
        </div>
    </div>

    <script>
    const { createApp, ref, computed, onMounted, nextTick, watch } = Vue;

    createApp({
        setup() {
            // State
            const projects = ref([]);
            const selectedProjectId = ref('');
            const selectedSprintId = ref('');
            const selectedEpicId = ref('');
            const storyBoard = ref({});
            const epics = ref([]);
            const sprints = ref([]);
            const selectedStory = ref(null);
            const activeTab = ref('Detalhes');
            const chatHistory = ref([]);
            const chatInput = ref('');
            const chatMessages = ref(null);

            // Search & Filters
            const searchQuery = ref('');
            const searchInput = ref(null);
            const filterPriority = ref('');
            const filterAssignee = ref('');

            // Toast Notifications
            const toasts = ref([]);
            let toastId = 0;

            // Confirm Modal
            const showConfirmModal = ref(false);
            const confirmModal = ref({
                title: '',
                message: '',
                itemName: '',
                confirmText: 'Excluir',
                onConfirm: null
            });

            // Context Menu
            const contextMenu = ref({
                visible: false,
                x: 0,
                y: 0,
                story: null
            });

            // Loading State
            const isLoading = ref(false);

            // Modals
            const showNewStoryModal = ref(false);
            const showNewTaskModal = ref(false);
            const showNewEpicModal = ref(false);
            const showNewSprintModal = ref(false);
            const showNewDocModal = ref(false);
            const showShortcutsModal = ref(false);
            const showBurndownModal = ref(false);

            // Burndown Chart
            let burndownChart = null;
            const burndownData = ref({
                total: 0,
                completed: 0,
                remaining: 0,
                velocity: 0,
                dailyData: []
            });

            // Bulk Actions
            const bulkSelectMode = ref(false);
            const selectedStories = ref([]);

            const toggleBulkSelectMode = () => {
                bulkSelectMode.value = !bulkSelectMode.value;
                if (!bulkSelectMode.value) {
                    selectedStories.value = [];
                }
            };

            const toggleBulkSelect = (story) => {
                const idx = selectedStories.value.indexOf(story.story_id);
                if (idx >= 0) {
                    selectedStories.value.splice(idx, 1);
                } else {
                    selectedStories.value.push(story.story_id);
                }
            };

            const cancelBulkSelect = () => {
                bulkSelectMode.value = false;
                selectedStories.value = [];
            };

            const bulkMoveStories = async (newStatus) => {
                const count = selectedStories.value.length;
                try {
                    for (const storyId of selectedStories.value) {
                        await fetch(`/api/stories/${storyId}/move`, {
                            method: 'PATCH',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({ status: newStatus })
                        });
                    }
                    addToast('success', 'Stories movidas', count + ' stories movidas para ' + getColumnTitle(newStatus));
                    cancelBulkSelect();
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro', 'Nao foi possivel mover as stories');
                }
            };

            const bulkDeleteStories = async () => {
                const count = selectedStories.value.length;
                if (!confirm('Tem certeza que deseja excluir ' + count + ' stories?')) return;

                try {
                    for (const storyId of selectedStories.value) {
                        await fetch(`/api/stories/${storyId}`, { method: 'DELETE' });
                    }
                    addToast('success', 'Stories excluidas', count + ' stories foram excluidas');
                    cancelBulkSelect();
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro', 'Nao foi possivel excluir as stories');
                }
            };

            // Form data
            const newStory = ref({
                title: '', description: '', persona: '', action: '', benefit: '',
                story_points: 3, priority: 'medium', complexity: 'medium', category: 'feature',
                epic_id: '', sprint_id: ''
            });
            const newStoryCriteria = ref('');
            const newTask = ref({ title: '', description: '', task_type: 'development', estimated_hours: 0 });
            const newEpic = ref({ title: '', description: '', color: '#003B4A' });
            const newSprint = ref({ name: '', goal: '', capacity: 0 });
            const newDoc = ref({ title: '', doc_type: 'technical', content: '', test_instructions: '' });

            // Story Templates
            const selectedTemplate = ref('');
            const storyTemplates = {
                feature: {
                    title: '[Feature] ',
                    persona: 'usuario do sistema',
                    action: '',
                    benefit: 'tenha uma melhor experiencia',
                    description: '## Contexto\\n\\n## Requisitos\\n\\n## Notas tecnicas\\n',
                    criteria: 'Funcionalidade implementada conforme especificacao\\nTestes unitarios criados\\nDocumentacao atualizada',
                    story_points: 5,
                    priority: 'medium',
                    complexity: 'medium',
                    category: 'feature'
                },
                bugfix: {
                    title: '[Bug] ',
                    persona: 'usuario afetado',
                    action: 'corrigir o problema encontrado',
                    benefit: 'possa usar o sistema sem erros',
                    description: '## Problema\\n\\n## Passos para reproduzir\\n1. \\n2. \\n3. \\n\\n## Comportamento esperado\\n\\n## Comportamento atual\\n',
                    criteria: 'Bug corrigido e validado\\nTeste de regressao adicionado\\nNenhum efeito colateral identificado',
                    story_points: 3,
                    priority: 'high',
                    complexity: 'medium',
                    category: 'bug'
                },
                tech_debt: {
                    title: '[Tech Debt] ',
                    persona: 'desenvolvedor',
                    action: 'refatorar/melhorar o codigo',
                    benefit: 'o codigo seja mais mantivel',
                    description: '## Debito tecnico\\n\\n## Impacto atual\\n\\n## Solucao proposta\\n\\n## Riscos\\n',
                    criteria: 'Codigo refatorado\\nTestes passando\\nDocumentacao atualizada\\nPerformance mantida ou melhorada',
                    story_points: 5,
                    priority: 'medium',
                    complexity: 'high',
                    category: 'tech_debt'
                },
                spike: {
                    title: '[Spike] ',
                    persona: 'time de desenvolvimento',
                    action: 'pesquisar e documentar',
                    benefit: 'tenhamos informacao para decisoes',
                    description: '## Objetivo da pesquisa\\n\\n## Perguntas a responder\\n1. \\n2. \\n\\n## Timebox\\n\\n## Entregaveis\\n- Documentacao\\n- POC (se aplicavel)',
                    criteria: 'Pesquisa documentada\\nRecomendacoes claras\\nPOC funcionando (se aplicavel)\\nApresentacao para o time',
                    story_points: 3,
                    priority: 'medium',
                    complexity: 'low',
                    category: 'spike'
                },
                improvement: {
                    title: '[Melhoria] ',
                    persona: 'usuario do sistema',
                    action: 'ter acesso a funcionalidade melhorada',
                    benefit: 'tenha uma experiencia mais eficiente',
                    description: '## Situacao atual\\n\\n## Melhoria proposta\\n\\n## Beneficios esperados\\n',
                    criteria: 'Melhoria implementada\\nTestes atualizados\\nUsuarios notificados',
                    story_points: 3,
                    priority: 'medium',
                    complexity: 'medium',
                    category: 'improvement'
                }
            };

            const applyTemplate = () => {
                const template = storyTemplates[selectedTemplate.value];
                if (template) {
                    newStory.value = {
                        ...newStory.value,
                        title: template.title,
                        persona: template.persona,
                        action: template.action,
                        benefit: template.benefit,
                        description: template.description,
                        story_points: template.story_points,
                        priority: template.priority,
                        complexity: template.complexity,
                        category: template.category
                    };
                    newStoryCriteria.value = template.criteria;
                    addToast('info', 'Template aplicado', 'Formulario preenchido com template ' + selectedTemplate.value);
                }
            };

            // Dark Mode
            const isDarkMode = ref(false);
            const toggleDarkMode = () => {
                isDarkMode.value = !isDarkMode.value;
                document.documentElement.classList.toggle('dark', isDarkMode.value);
                localStorage.setItem('darkMode', isDarkMode.value);
                addToast('info', isDarkMode.value ? 'Modo escuro' : 'Modo claro', 'Tema alterado');
            };

            // Load dark mode preference
            const loadDarkMode = () => {
                const saved = localStorage.getItem('darkMode');
                if (saved === 'true') {
                    isDarkMode.value = true;
                    document.documentElement.classList.add('dark');
                }
            };

            // Computed
            const totalStories = computed(() => {
                let count = 0;
                Object.values(storyBoard.value).forEach(col => count += col.length);
                return count;
            });

            const doneStories = computed(() => storyBoard.value.done?.length || 0);
            const inProgressStories = computed(() => storyBoard.value.in_progress?.length || 0);

            const totalPoints = computed(() => {
                let points = 0;
                Object.values(storyBoard.value).forEach(col => {
                    col.forEach(s => points += s.story_points || 0);
                });
                return points;
            });

            // Filtered Story Board (for search and filters)
            const filteredStoryBoard = computed(() => {
                const hasFilters = searchQuery.value.trim() || filterPriority.value || filterAssignee.value;
                if (!hasFilters) {
                    return storyBoard.value;
                }
                const query = searchQuery.value.toLowerCase().trim();
                const filtered = {};
                Object.keys(storyBoard.value).forEach(status => {
                    filtered[status] = storyBoard.value[status].filter(story => {
                        // Search filter
                        const matchesSearch = !query ||
                            story.title?.toLowerCase().includes(query) ||
                            story.story_id?.toLowerCase().includes(query) ||
                            story.description?.toLowerCase().includes(query) ||
                            story.persona?.toLowerCase().includes(query) ||
                            story.action?.toLowerCase().includes(query);
                        // Priority filter
                        const matchesPriority = !filterPriority.value || story.priority === filterPriority.value;
                        // Assignee filter
                        const matchesAssignee = !filterAssignee.value ||
                            (filterAssignee.value === 'unassigned' && !story.assignee) ||
                            (story.assignee === filterAssignee.value);
                        return matchesSearch && matchesPriority && matchesAssignee;
                    });
                });
                return filtered;
            });

            // Filtered stories count
            const filteredStoriesCount = computed(() => {
                let count = 0;
                Object.values(filteredStoryBoard.value).forEach(col => count += col.length);
                return count;
            });

            // Clear all filters
            const clearFilters = () => {
                searchQuery.value = '';
                filterPriority.value = '';
                filterAssignee.value = '';
            };

            // Methods
            const loadProjects = async () => {
                const res = await fetch('/api/projects');
                projects.value = await res.json();
            };

            const loadProjectData = async () => {
                if (!selectedProjectId.value) return;

                // Load story board
                const boardRes = await fetch(`/api/projects/${selectedProjectId.value}/story-board`);
                storyBoard.value = await boardRes.json();

                // Load epics
                const epicsRes = await fetch(`/api/projects/${selectedProjectId.value}/epics`);
                epics.value = await epicsRes.json();

                // Load sprints
                const sprintsRes = await fetch(`/api/projects/${selectedProjectId.value}/sprints`);
                sprints.value = await sprintsRes.json();

                // Setup drag and drop
                nextTick(() => {
                    setupSortable();
                    updateBurndownData();
                });
            };

            // Burndown Chart Functions
            const updateBurndownData = () => {
                let total = 0;
                let completed = 0;
                Object.entries(storyBoard.value).forEach(([status, stories]) => {
                    stories.forEach(s => {
                        total += s.story_points || 0;
                        if (status === 'done') {
                            completed += s.story_points || 0;
                        }
                    });
                });
                burndownData.value = {
                    total,
                    completed,
                    remaining: total - completed,
                    velocity: completed,
                    dailyData: generateBurndownDays(total, completed)
                };
                nextTick(() => initMiniChart());
            };

            const generateBurndownDays = (total, completed) => {
                // Generate sample burndown data for demonstration
                const days = 10;
                const idealPerDay = total / days;
                const data = [];
                for (let i = 0; i <= days; i++) {
                    const idealRemaining = Math.max(0, total - (idealPerDay * i));
                    // Simulate actual progress with some variance
                    const actualRemaining = i === days ? (total - completed) :
                        Math.max(0, total - (completed * i / days) - Math.random() * 3);
                    data.push({
                        day: i,
                        ideal: Math.round(idealRemaining),
                        actual: Math.round(actualRemaining)
                    });
                }
                return data;
            };

            const initMiniChart = () => {
                const canvas = document.getElementById('burndown-mini');
                if (!canvas || !window.Chart) return;

                const ctx = canvas.getContext('2d');
                const data = burndownData.value.dailyData;
                if (!data.length) return;

                // Simple mini chart drawing
                ctx.clearRect(0, 0, canvas.width, canvas.height);
                const w = canvas.width;
                const h = canvas.height;
                const max = burndownData.value.total || 1;

                // Draw ideal line (blue)
                ctx.beginPath();
                ctx.strokeStyle = '#3B82F6';
                ctx.lineWidth = 2;
                data.forEach((d, i) => {
                    const x = (i / (data.length - 1)) * w;
                    const y = h - (d.ideal / max) * h;
                    if (i === 0) ctx.moveTo(x, y);
                    else ctx.lineTo(x, y);
                });
                ctx.stroke();

                // Draw actual line (orange)
                ctx.beginPath();
                ctx.strokeStyle = '#FF6C00';
                ctx.lineWidth = 2;
                data.forEach((d, i) => {
                    const x = (i / (data.length - 1)) * w;
                    const y = h - (d.actual / max) * h;
                    if (i === 0) ctx.moveTo(x, y);
                    else ctx.lineTo(x, y);
                });
                ctx.stroke();
            };

            const updateBurndownChart = () => {
                const canvas = document.getElementById('burndown-full');
                if (!canvas || !window.Chart) return;

                if (burndownChart) {
                    burndownChart.destroy();
                }

                const data = burndownData.value.dailyData;
                const labels = data.map(d => 'Dia ' + d.day);

                burndownChart = new Chart(canvas, {
                    type: 'line',
                    data: {
                        labels,
                        datasets: [
                            {
                                label: 'Ideal',
                                data: data.map(d => d.ideal),
                                borderColor: '#3B82F6',
                                backgroundColor: 'rgba(59, 130, 246, 0.1)',
                                borderDash: [5, 5],
                                tension: 0.1,
                                fill: false
                            },
                            {
                                label: 'Atual',
                                data: data.map(d => d.actual),
                                borderColor: '#FF6C00',
                                backgroundColor: 'rgba(255, 108, 0, 0.1)',
                                tension: 0.1,
                                fill: true
                            }
                        ]
                    },
                    options: {
                        responsive: true,
                        maintainAspectRatio: false,
                        plugins: {
                            legend: {
                                display: false
                            }
                        },
                        scales: {
                            y: {
                                beginAtZero: true,
                                title: {
                                    display: true,
                                    text: 'Story Points'
                                }
                            },
                            x: {
                                title: {
                                    display: true,
                                    text: 'Sprint Days'
                                }
                            }
                        }
                    }
                });
            };

            // Watch for burndown modal to init chart
            watch(showBurndownModal, (newVal) => {
                if (newVal) {
                    nextTick(() => updateBurndownChart());
                }
            });

            const setupSortable = () => {
                const statuses = ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done'];
                statuses.forEach(status => {
                    const el = document.getElementById('column-' + status);
                    if (el) {
                        new Sortable(el, {
                            group: 'stories',
                            animation: 150,
                            ghostClass: 'sortable-ghost',
                            dragClass: 'sortable-drag',
                            onEnd: async (evt) => {
                                const storyId = evt.item.dataset.id;
                                const newStatus = evt.to.id.replace('column-', '');
                                const newOrder = evt.newIndex;

                                try {
                                    await fetch(`/api/stories/${storyId}/move`, {
                                        method: 'PATCH',
                                        headers: { 'Content-Type': 'application/json' },
                                        body: JSON.stringify({ status: newStatus, order: newOrder })
                                    });
                                    addToast('success', 'Story movida', storyId + ' -> ' + getColumnTitle(newStatus));
                                    loadProjectData();
                                } catch (e) {
                                    addToast('error', 'Erro ao mover', 'Nao foi possivel mover a story');
                                    loadProjectData();
                                }
                            }
                        });
                    }
                });
            };

            const getColumnTitle = (status) => {
                const titles = {
                    'backlog': 'Backlog',
                    'ready': 'Ready',
                    'in_progress': 'In Progress',
                    'review': 'Review',
                    'testing': 'Testing',
                    'done': 'Done'
                };
                return titles[status] || status;
            };

            const getColumnPoints = (column) => {
                return column.reduce((sum, s) => sum + (s.story_points || 0), 0);
            };

            const getEpicName = (epicId) => {
                const epic = epics.value.find(e => e.epic_id === epicId);
                return epic ? epic.title : '';
            };

            const openStoryDetail = async (story) => {
                const res = await fetch(`/api/stories/${story.story_id}`);
                selectedStory.value = await res.json();
                activeTab.value = 'Detalhes';
            };

            const createStory = async () => {
                try {
                    const storyData = { ...newStory.value, project_id: selectedProjectId.value };
                    if (newStoryCriteria.value) {
                        storyData.acceptance_criteria = newStoryCriteria.value.split('\\n').filter(c => c.trim());
                    }

                    const res = await fetch('/api/stories', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(storyData)
                    });

                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Story criada', created.story_id + ': ' + created.title);
                    }

                    showNewStoryModal.value = false;
                    newStory.value = { title: '', description: '', persona: '', action: '', benefit: '',
                        story_points: 3, priority: 'medium', complexity: 'medium', category: 'feature',
                        epic_id: '', sprint_id: '' };
                    newStoryCriteria.value = '';
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro ao criar story', 'Verifique os dados e tente novamente');
                }
            };

            const createTask = async () => {
                try {
                    const res = await fetch(`/api/stories/${selectedStory.value.story_id}/tasks`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ ...newTask.value, story_id: selectedStory.value.story_id })
                    });

                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Task criada', created.task_id + ': ' + created.title);
                    }

                    showNewTaskModal.value = false;
                    newTask.value = { title: '', description: '', task_type: 'development', estimated_hours: 0 };

                    // Reload story
                    const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                    selectedStory.value = await storyRes.json();
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro ao criar task', 'Verifique os dados e tente novamente');
                }
            };

            const toggleTaskComplete = async (task) => {
                const newStatus = task.status === 'completed' ? 'pending' : 'completed';
                await fetch(`/api/story-tasks/${task.task_id}`, {
                    method: 'PUT',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ status: newStatus })
                });

                const res = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                selectedStory.value = await res.json();
                loadProjectData();

                if (newStatus === 'completed') {
                    addToast('success', 'Task completa', task.title);
                } else {
                    addToast('info', 'Task reaberta', task.title);
                }
            };

            const createEpic = async () => {
                try {
                    const res = await fetch('/api/epics', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ ...newEpic.value, project_id: selectedProjectId.value })
                    });
                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Epic criado', created.title);
                    }
                    showNewEpicModal.value = false;
                    newEpic.value = { title: '', description: '', color: '#003B4A' };
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro ao criar epic', 'Verifique os dados e tente novamente');
                }
            };

            const createSprint = async () => {
                try {
                    const res = await fetch('/api/sprints', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ ...newSprint.value, project_id: selectedProjectId.value })
                    });
                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Sprint criado', created.name);
                    }
                    showNewSprintModal.value = false;
                    newSprint.value = { name: '', goal: '', capacity: 0 };
                    loadProjectData();
                } catch (e) {
                    addToast('error', 'Erro ao criar sprint', 'Verifique os dados e tente novamente');
                }
            };

            const createDoc = async () => {
                try {
                    const res = await fetch(`/api/stories/${selectedStory.value.story_id}/docs`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ ...newDoc.value, story_id: selectedStory.value.story_id })
                    });
                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Documentacao criada', created.title);
                    }
                    showNewDocModal.value = false;
                    newDoc.value = { title: '', doc_type: 'technical', content: '', test_instructions: '' };

                    const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                    selectedStory.value = await storyRes.json();
                } catch (e) {
                    addToast('error', 'Erro ao criar documentacao', 'Verifique os dados e tente novamente');
                }
            };

            const editStory = () => {
                // TODO: Implement edit modal
                addToast('info', 'Em desenvolvimento', 'Funcionalidade de edicao em breve');
            };

            const uploadFile = async (event) => {
                const file = event.target.files[0];
                if (!file) return;

                try {
                    const formData = new FormData();
                    formData.append('file', file);
                    formData.append('story_id', selectedStory.value.story_id);

                    const res = await fetch('/api/upload', { method: 'POST', body: formData });

                    if (res.ok) {
                        addToast('success', 'Arquivo enviado', file.name);
                    }

                    const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                    selectedStory.value = await storyRes.json();
                } catch (e) {
                    addToast('error', 'Erro no upload', 'Nao foi possivel enviar o arquivo');
                }
            };

            const filterByEpic = (epicId) => {
                selectedEpicId.value = selectedEpicId.value === epicId ? '' : epicId;
                // TODO: Filter stories
            };

            // Chat
            const loadChatHistory = async () => {
                const res = await fetch(`/api/chat/history?project_id=${selectedProjectId.value}&limit=50`);
                chatHistory.value = await res.json();
                nextTick(() => scrollChatToBottom());
            };

            const sendMessage = async () => {
                if (!chatInput.value.trim()) return;

                const messageContent = chatInput.value;
                chatInput.value = ''; // Clear immediately for better UX

                try {
                    const res = await fetch('/api/chat/message', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                            project_id: selectedProjectId.value,
                            content: messageContent
                        })
                    });

                    if (!res.ok) {
                        throw new Error('Erro ao enviar mensagem');
                    }

                    const data = await res.json();
                    if (data.user_message) {
                        chatHistory.value.push(data.user_message);
                    }
                    if (data.assistant_message) {
                        chatHistory.value.push(data.assistant_message);
                    }
                    nextTick(() => scrollChatToBottom());
                } catch (error) {
                    console.error('Chat error:', error);
                    // Show error message
                    chatHistory.value.push({
                        message_id: 'error-' + Date.now(),
                        role: 'assistant',
                        content: 'Desculpe, ocorreu um erro ao processar sua mensagem. Tente novamente.',
                        created_at: new Date().toISOString()
                    });
                    nextTick(() => scrollChatToBottom());
                }
            };

            const scrollChatToBottom = () => {
                if (chatMessages.value) {
                    chatMessages.value.scrollTop = chatMessages.value.scrollHeight;
                }
            };

            // Helpers
            const getTaskStatusClass = (status) => {
                const classes = {
                    'pending': 'bg-gray-100 text-gray-700',
                    'in_progress': 'bg-blue-100 text-blue-700',
                    'completed': 'bg-green-100 text-green-700',
                    'blocked': 'bg-red-100 text-red-700'
                };
                return classes[status] || 'bg-gray-100';
            };

            const getDocTypeClass = (type) => {
                const classes = {
                    'technical': 'bg-purple-100 text-purple-700',
                    'user': 'bg-blue-100 text-blue-700',
                    'test': 'bg-green-100 text-green-700',
                    'deployment': 'bg-orange-100 text-orange-700',
                    'api': 'bg-cyan-100 text-cyan-700'
                };
                return classes[type] || 'bg-gray-100';
            };

            const formatTime = (isoString) => {
                if (!isoString) return '';
                const date = new Date(isoString);
                return date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
            };

            const formatFileSize = (bytes) => {
                if (bytes < 1024) return bytes + ' B';
                if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            };

            const renderMarkdown = (text) => {
                if (!text) return '';
                return marked.parse(text);
            };

            // Toast Functions
            const addToast = (type, title, message = '', undoAction = null) => {
                const id = ++toastId;
                toasts.value.push({ id, type, title, message, undoAction });
                setTimeout(() => removeToast(id), 5000);
                return id;
            };

            const removeToast = (id) => {
                const idx = toasts.value.findIndex(t => t.id === id);
                if (idx !== -1) toasts.value.splice(idx, 1);
            };

            const getToastIcon = (type) => {
                const icons = {
                    success: '✓',
                    error: '✕',
                    warning: '⚠',
                    info: 'ℹ'
                };
                return icons[type] || 'ℹ';
            };

            const handleUndo = async (toast) => {
                if (toast.undoAction) {
                    await toast.undoAction();
                    removeToast(toast.id);
                    addToast('info', 'Acao desfeita');
                }
            };

            // Confirm Modal Functions
            const showConfirm = (title, message, itemName, confirmText, onConfirm) => {
                confirmModal.value = { title, message, itemName, confirmText, onConfirm };
                showConfirmModal.value = true;
            };

            const cancelConfirm = () => {
                showConfirmModal.value = false;
                confirmModal.value = { title: '', message: '', itemName: '', confirmText: 'Excluir', onConfirm: null };
            };

            const executeConfirm = async () => {
                if (confirmModal.value.onConfirm) {
                    await confirmModal.value.onConfirm();
                }
                cancelConfirm();
            };

            // Delete Functions with Confirmation
            const deleteStoryWithConfirm = (story) => {
                showConfirm(
                    'Excluir Story',
                    'Tem certeza que deseja excluir esta story? Todas as tasks e documentos associados serao perdidos.',
                    story.story_id + ': ' + story.title,
                    'Excluir Story',
                    async () => {
                        try {
                            const res = await fetch('/api/stories/' + story.story_id, { method: 'DELETE' });
                            if (res.ok) {
                                addToast('success', 'Story excluida', story.story_id + ' foi removida');
                                selectedStory.value = null;
                                loadProjectData();
                            } else {
                                addToast('error', 'Erro ao excluir', 'Nao foi possivel excluir a story');
                            }
                        } catch (e) {
                            addToast('error', 'Erro de conexao', 'Verifique sua conexao');
                        }
                    }
                );
            };

            const deleteTaskWithConfirm = (task) => {
                showConfirm(
                    'Excluir Task',
                    'Tem certeza que deseja excluir esta task?',
                    task.task_id + ': ' + task.title,
                    'Excluir Task',
                    async () => {
                        try {
                            const res = await fetch('/api/story-tasks/' + task.task_id, { method: 'DELETE' });
                            if (res.ok) {
                                addToast('success', 'Task excluida', task.task_id + ' foi removida');
                                // Reload story
                                const storyRes = await fetch('/api/stories/' + selectedStory.value.story_id);
                                selectedStory.value = await storyRes.json();
                                loadProjectData();
                            } else {
                                addToast('error', 'Erro ao excluir', 'Nao foi possivel excluir a task');
                            }
                        } catch (e) {
                            addToast('error', 'Erro de conexao', 'Verifique sua conexao');
                        }
                    }
                );
            };

            // Keyboard Shortcuts
            const handleKeyboard = (e) => {
                // Ignore if in input/textarea
                if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' || e.target.tagName === 'SELECT') {
                    if (e.key === 'Escape') {
                        e.target.blur();
                        searchQuery.value = '';
                    }
                    return;
                }

                // Escape - close modals
                if (e.key === 'Escape') {
                    if (showConfirmModal.value) { cancelConfirm(); return; }
                    if (showShortcutsModal.value) { showShortcutsModal.value = false; return; }
                    if (showNewStoryModal.value) { showNewStoryModal.value = false; return; }
                    if (showNewTaskModal.value) { showNewTaskModal.value = false; return; }
                    if (showNewEpicModal.value) { showNewEpicModal.value = false; return; }
                    if (showNewSprintModal.value) { showNewSprintModal.value = false; return; }
                    if (showNewDocModal.value) { showNewDocModal.value = false; return; }
                    if (selectedStory.value) { selectedStory.value = null; return; }
                    searchQuery.value = '';
                }

                // / - focus search
                if (e.key === '/' && !e.ctrlKey && !e.metaKey) {
                    e.preventDefault();
                    if (searchInput.value) searchInput.value.focus();
                    return;
                }

                // ? - show shortcuts
                if (e.key === '?' || (e.shiftKey && e.key === '/')) {
                    e.preventDefault();
                    showShortcutsModal.value = true;
                    return;
                }

                // n - new story
                if (e.key === 'n' && selectedProjectId.value && !e.ctrlKey && !e.metaKey) {
                    e.preventDefault();
                    showNewStoryModal.value = true;
                    return;
                }

                // t - new task (with story open)
                if (e.key === 't' && selectedStory.value && !e.ctrlKey && !e.metaKey) {
                    e.preventDefault();
                    showNewTaskModal.value = true;
                    return;
                }

                // e - edit story
                if (e.key === 'e' && selectedStory.value && !e.ctrlKey && !e.metaKey) {
                    e.preventDefault();
                    editStory();
                    return;
                }

                // Delete - delete story
                if ((e.key === 'Delete' || e.key === 'Backspace') && selectedStory.value && !e.ctrlKey && !e.metaKey) {
                    e.preventDefault();
                    deleteStoryWithConfirm(selectedStory.value);
                    return;
                }

                // 1-6 - move story to column
                if (selectedStory.value && ['1','2','3','4','5','6'].includes(e.key)) {
                    e.preventDefault();
                    const statuses = ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done'];
                    const newStatus = statuses[parseInt(e.key) - 1];
                    moveStoryToStatus(selectedStory.value, newStatus);
                    return;
                }
            };

            // Move story to status (for keyboard shortcuts)
            const moveStoryToStatus = async (story, newStatus) => {
                try {
                    await fetch('/api/stories/' + story.story_id + '/move', {
                        method: 'PATCH',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ status: newStatus })
                    });
                    addToast('success', 'Story movida', story.story_id + ' -> ' + getColumnTitle(newStatus));
                    loadProjectData();
                    // Update selected story
                    const res = await fetch('/api/stories/' + story.story_id);
                    selectedStory.value = await res.json();
                } catch (e) {
                    addToast('error', 'Erro ao mover', 'Nao foi possivel mover a story');
                }
            };

            // Move to next column (quick action)
            const moveToNextColumn = async (story) => {
                const statuses = ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done'];
                const currentIndex = statuses.indexOf(story.status);
                if (currentIndex < statuses.length - 1) {
                    const newStatus = statuses[currentIndex + 1];
                    await moveStoryToStatus(story, newStatus);
                } else {
                    addToast('info', 'Ja esta em Done', 'Story ja esta na ultima coluna');
                }
            };

            // Context Menu
            const showContextMenu = (event, story) => {
                contextMenu.value = {
                    visible: true,
                    x: event.clientX,
                    y: event.clientY,
                    story: story
                };
            };

            const hideContextMenu = () => {
                contextMenu.value.visible = false;
                contextMenu.value.story = null;
            };

            const contextMenuAction = async (action) => {
                const story = contextMenu.value.story;
                hideContextMenu();

                if (!story) return;

                switch (action) {
                    case 'open':
                        openStoryDetail(story);
                        break;
                    case 'backlog':
                    case 'ready':
                    case 'in_progress':
                    case 'review':
                    case 'testing':
                    case 'done':
                        await moveStoryToStatus(story, action);
                        break;
                    case 'copy':
                        navigator.clipboard.writeText(story.story_id);
                        addToast('success', 'ID copiado', story.story_id);
                        break;
                    case 'delete':
                        deleteStoryWithConfirm(story);
                        break;
                }
            };

            // Watch project change
            watch(selectedProjectId, () => {
                loadChatHistory();
            });

            // Init
            onMounted(() => {
                loadProjects();
                loadDarkMode();

                // Setup keyboard shortcuts
                document.addEventListener('keydown', handleKeyboard);

                // Close context menu on click outside
                document.addEventListener('click', () => {
                    hideContextMenu();
                });

                // Welcome message
                chatHistory.value.push({
                    message_id: 'welcome',
                    role: 'assistant',
                    content: 'Ola! Sou o assistente da Fabrica de Agentes. Posso ajudar com:\\n\\n- **Criar stories**: Clique em "Nova Story"\\n- **Ver progresso**: Pergunte sobre o status\\n- **Documentacao**: Veja a aba Docs de cada story\\n\\nComo posso ajudar?',
                    created_at: new Date().toISOString()
                });
            });

            return {
                projects, selectedProjectId, selectedSprintId, selectedEpicId,
                storyBoard, epics, sprints, selectedStory, activeTab,
                chatHistory, chatInput, chatMessages,
                showNewStoryModal, showNewTaskModal, showNewEpicModal, showNewSprintModal, showNewDocModal,
                showShortcutsModal, showConfirmModal, confirmModal,
                contextMenu, isLoading,
                newStory, newStoryCriteria, newTask, newEpic, newSprint, newDoc,
                totalStories, doneStories, inProgressStories, totalPoints,
                filteredStoryBoard, filteredStoriesCount, searchQuery, searchInput, toasts,
                filterPriority, filterAssignee, clearFilters,
                loadProjectData, getColumnTitle, getColumnPoints, getEpicName,
                openStoryDetail, createStory, createTask, toggleTaskComplete,
                createEpic, createSprint, createDoc, editStory, uploadFile, filterByEpic,
                sendMessage, getTaskStatusClass, getDocTypeClass,
                formatTime, formatFileSize, renderMarkdown,
                addToast, removeToast, getToastIcon, handleUndo,
                cancelConfirm, executeConfirm, deleteStoryWithConfirm, deleteTaskWithConfirm,
                showContextMenu, hideContextMenu, contextMenuAction, moveToNextColumn,
                selectedTemplate, applyTemplate, isDarkMode, toggleDarkMode,
                showBurndownModal, burndownData, updateBurndownChart,
                bulkSelectMode, selectedStories, toggleBulkSelectMode, toggleBulkSelect,
                cancelBulkSelect, bulkMoveStories, bulkDeleteStories
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
"""


@app.get("/", response_class=HTMLResponse)
def index():
    """Pagina principal - Dashboard Agile"""
    return HTML_TEMPLATE


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    print("=" * 60)
    print("  FABRICA DE AGENTES - Dashboard Agile v6.0")
    print("  http://localhost:9001")
    print("=" * 60)
    uvicorn.run(app, host="0.0.0.0", port=9001)
