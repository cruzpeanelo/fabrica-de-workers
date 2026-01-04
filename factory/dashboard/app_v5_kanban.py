"""
Dashboard Kanban - Plataforma E v5.0
Gestao de Projetos estilo Monday/Jira com visual Belgo Arames

Acesse em: http://localhost:9001
"""
# Carregar variaveis de ambiente do .env
from dotenv import load_dotenv
load_dotenv()

import json
import secrets
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional
from fastapi import FastAPI, HTTPException, Depends, Query
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field
import uvicorn
from jose import JWTError, jwt
from passlib.context import CryptContext

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import init_db, SessionLocal
from factory.database.models import (
    Project, User, ActivityLog, Task, TaskStatus, TaskPriority,
    Job, JobStatus, JobStep, FailureHistory, Worker
)
from factory.database.repositories import (
    ProjectRepository, UserRepository, ActivityLogRepository, TaskRepository,
    JobRepository, FailureHistoryRepository, WorkerRepository
)
from factory.config import DASHBOARD_HOST, DASHBOARD_PORT, DASHBOARD_TITLE

# Initialize database
init_db()

# FastAPI App
app = FastAPI(
    title="Plataforma E - Kanban Dashboard",
    description="Gestao de Projetos estilo Monday/Jira com visual Belgo Arames",
    version="5.0.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# JWT Configuration
SECRET_KEY = secrets.token_urlsafe(32)
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 60 * 24

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
security = HTTPBearer(auto_error=False)


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class LoginRequest(BaseModel):
    username: str
    password: str


class ProjectCreate(BaseModel):
    name: str
    description: Optional[str] = None
    project_type: str = "kanban"
    config: Optional[dict] = None


class ProjectUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    progress: Optional[float] = None


class TaskCreate(BaseModel):
    title: str = Field(..., min_length=1, max_length=200)
    description: Optional[str] = None
    project_id: str
    status: Optional[str] = TaskStatus.BACKLOG.value
    priority: Optional[str] = TaskPriority.MEDIUM.value
    assignee: Optional[str] = None
    due_date: Optional[str] = None
    tags: Optional[List[str]] = []


class TaskUpdate(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    priority: Optional[str] = None
    assignee: Optional[str] = None
    due_date: Optional[str] = None
    tags: Optional[List[str]] = None


class TaskMove(BaseModel):
    status: str
    order: Optional[int] = None


class TaskReorder(BaseModel):
    task_ids: List[str]


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def verify_password(plain_password, hashed_password):
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password):
    return pwd_context.hash(password)


def create_access_token(data: dict, expires_delta: timedelta = None):
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=15)
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt


async def get_current_user(credentials: HTTPAuthorizationCredentials = Depends(security)):
    if not credentials:
        return None
    try:
        payload = jwt.decode(credentials.credentials, SECRET_KEY, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            return None
        return {"username": username, "role": payload.get("role", "VIEWER")}
    except JWTError:
        return None


# =============================================================================
# AUTH ROUTES
# =============================================================================

@app.post("/api/auth/login")
async def login(request: LoginRequest, db=Depends(get_db)):
    """Login and get JWT token"""
    user_repo = UserRepository(db)
    user = user_repo.get_by_username(request.username)
    if not user or not verify_password(request.password, user.password_hash):
        raise HTTPException(status_code=401, detail="Invalid credentials")

    access_token = create_access_token(
        data={"sub": user.username, "role": user.role},
        expires_delta=timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    )
    user_repo.update_last_login(user.username)
    return {"access_token": access_token, "token_type": "bearer", "role": user.role}


# =============================================================================
# PROJECT ROUTES
# =============================================================================

@app.get("/api/projects")
async def list_projects(
    status: Optional[str] = None,
    db=Depends(get_db)
):
    """Lista todos os projetos com contagem de tarefas"""
    repo = ProjectRepository(db)
    projects = repo.get_all(status=status)
    return [p.to_dict() for p in projects]


@app.post("/api/projects", status_code=201)
async def create_project(project: ProjectCreate, db=Depends(get_db)):
    """Cria novo projeto"""
    repo = ProjectRepository(db)
    new_project = repo.create({
        "name": project.name,
        "description": project.description,
        "project_type": project.project_type,
        "config": project.config or {},
        "status": "IN_PROGRESS"
    })
    return new_project.to_dict()


@app.get("/api/projects/{project_id}")
async def get_project(project_id: str, db=Depends(get_db)):
    """Busca projeto por ID"""
    repo = ProjectRepository(db)
    project = repo.get_by_id(project_id)
    if not project:
        raise HTTPException(status_code=404, detail="Project not found")
    return project.to_dict()


@app.put("/api/projects/{project_id}")
async def update_project(project_id: str, data: ProjectUpdate, db=Depends(get_db)):
    """Atualiza projeto"""
    repo = ProjectRepository(db)
    project = repo.update(project_id, data.model_dump(exclude_unset=True))
    if not project:
        raise HTTPException(status_code=404, detail="Project not found")
    return project.to_dict()


@app.delete("/api/projects/{project_id}")
async def delete_project(project_id: str, db=Depends(get_db)):
    """Remove projeto"""
    repo = ProjectRepository(db)
    success = repo.delete(project_id)
    if not success:
        raise HTTPException(status_code=404, detail="Project not found")
    return {"success": True, "message": f"Project {project_id} deleted"}


# =============================================================================
# TASK ROUTES (Kanban)
# =============================================================================

@app.get("/api/tasks")
async def list_tasks(
    project_id: Optional[str] = None,
    status: Optional[str] = None,
    limit: int = Query(100, ge=1, le=500),
    db=Depends(get_db)
):
    """Lista tarefas com filtros"""
    repo = TaskRepository(db)
    tasks = repo.get_all(project_id=project_id, status=status, limit=limit)
    return [t.to_dict() for t in tasks]


@app.post("/api/tasks", status_code=201)
async def create_task(task: TaskCreate, db=Depends(get_db)):
    """Cria nova tarefa"""
    # Verifica se projeto existe
    project_repo = ProjectRepository(db)
    project = project_repo.get_by_id(task.project_id)
    if not project:
        raise HTTPException(status_code=404, detail="Project not found")

    task_repo = TaskRepository(db)
    task_data = task.model_dump()

    # Converte due_date string para datetime
    if task_data.get("due_date"):
        try:
            task_data["due_date"] = datetime.fromisoformat(task_data["due_date"])
        except ValueError:
            task_data["due_date"] = None

    new_task = task_repo.create(task_data)
    return new_task.to_dict()


@app.get("/api/tasks/{task_id}")
async def get_task(task_id: str, db=Depends(get_db)):
    """Busca tarefa por ID"""
    repo = TaskRepository(db)
    task = repo.get_by_id(task_id)
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task.to_dict()


@app.put("/api/tasks/{task_id}")
async def update_task(task_id: str, data: TaskUpdate, db=Depends(get_db)):
    """Atualiza tarefa"""
    repo = TaskRepository(db)
    update_data = data.model_dump(exclude_unset=True)

    # Converte due_date string para datetime
    if update_data.get("due_date"):
        try:
            update_data["due_date"] = datetime.fromisoformat(update_data["due_date"])
        except ValueError:
            update_data["due_date"] = None

    task = repo.update(task_id, update_data)
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task.to_dict()


@app.delete("/api/tasks/{task_id}")
async def delete_task(task_id: str, db=Depends(get_db)):
    """Remove tarefa"""
    repo = TaskRepository(db)
    success = repo.delete(task_id)
    if not success:
        raise HTTPException(status_code=404, detail="Task not found")
    return {"success": True, "message": f"Task {task_id} deleted"}


@app.patch("/api/tasks/{task_id}/move")
async def move_task(task_id: str, move: TaskMove, db=Depends(get_db)):
    """Move tarefa para nova coluna/posicao no Kanban"""
    repo = TaskRepository(db)
    task = repo.move_task(task_id, move.status, move.order)
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task.to_dict()


@app.put("/api/tasks/reorder/{project_id}/{status}")
async def reorder_tasks(
    project_id: str,
    status: str,
    data: TaskReorder,
    db=Depends(get_db)
):
    """Reordena tarefas em uma coluna"""
    repo = TaskRepository(db)
    success = repo.reorder_tasks(project_id, status, data.task_ids)
    return {"success": success}


# =============================================================================
# KANBAN ROUTES
# =============================================================================

@app.get("/api/projects/{project_id}/kanban")
async def get_kanban_board(project_id: str, db=Depends(get_db)):
    """Retorna board Kanban completo com tarefas agrupadas por status"""
    # Verifica se projeto existe
    project_repo = ProjectRepository(db)
    project = project_repo.get_by_id(project_id)
    if not project:
        raise HTTPException(status_code=404, detail="Project not found")

    task_repo = TaskRepository(db)
    board = task_repo.get_kanban_board(project_id)

    return {
        "project": project.to_dict(),
        "board": board,
        "columns": [
            {"id": "backlog", "name": "Backlog", "color": "#636466"},
            {"id": "todo", "name": "To Do", "color": "#00B5F1"},
            {"id": "in_development", "name": "Em Desenvolvimento", "color": "#FDB913"},
            {"id": "in_testing", "name": "Em Teste", "color": "#634976"},
            {"id": "ready_to_deploy", "name": "Pronto para Deploy", "color": "#00A799"},
            {"id": "done", "name": "Concluido", "color": "#003B4A"}
        ]
    }


@app.get("/api/kanban/stats")
async def get_kanban_stats(project_id: Optional[str] = None, db=Depends(get_db)):
    """Retorna estatisticas do Kanban"""
    task_repo = TaskRepository(db)
    counts = task_repo.count_by_status(project_id)
    total = sum(counts.values())

    return {
        "counts": counts,
        "total": total,
        "by_priority": {
            "low": 0,
            "medium": 0,
            "high": 0,
            "urgent": 0
        }
    }


# =============================================================================
# HEALTH ROUTES
# =============================================================================

@app.get("/api/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "service": "fabrica-kanban",
        "version": "5.0.0",
        "timestamp": datetime.utcnow().isoformat()
    }


# =============================================================================
# DASHBOARD HTML (Kanban com Visual Belgo)
# =============================================================================

KANBAN_DASHBOARD_HTML = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Plataforma E - Kanban</title>
    <link href="https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600;700&display=swap" rel="stylesheet">
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
    <style>
        :root {
            /* Cores Belgo */
            --belgo-azul: #003B4A;
            --belgo-vermelho: #ED1C24;
            --belgo-laranja: #FF6C00;
            --belgo-amarelo: #FDB913;
            --belgo-azul-claro: #00B5F1;
            --belgo-verde: #00A799;
            --belgo-lilas: #634976;
            --belgo-cinza: #636466;
            --belgo-cinza-claro: #D3CAB7;
            --belgo-branco: #FFFFFF;
            --belgo-bg: #f5f7fa;

            /* Kanban columns */
            --kanban-backlog: var(--belgo-cinza);
            --kanban-todo: var(--belgo-azul-claro);
            --kanban-dev: var(--belgo-amarelo);
            --kanban-test: var(--belgo-lilas);
            --kanban-deploy: var(--belgo-verde);
            --kanban-done: var(--belgo-azul);
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Open Sans', Arial, sans-serif;
            background: var(--belgo-bg);
            color: var(--belgo-azul);
            min-height: 100vh;
        }

        /* Header */
        .header {
            background: var(--belgo-azul);
            color: white;
            padding: 0 24px;
            height: 60px;
            display: flex;
            align-items: center;
            justify-content: space-between;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }

        .header-logo {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .header-logo svg {
            width: 32px;
            height: 32px;
        }

        .header-title {
            font-size: 18px;
            font-weight: 700;
        }

        .header-subtitle {
            font-size: 12px;
            opacity: 0.8;
        }

        .header-actions {
            display: flex;
            align-items: center;
            gap: 16px;
        }

        .btn {
            padding: 8px 16px;
            border: none;
            border-radius: 6px;
            cursor: pointer;
            font-family: inherit;
            font-size: 14px;
            font-weight: 600;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            gap: 6px;
        }

        .btn-primary {
            background: var(--belgo-laranja);
            color: white;
        }

        .btn-primary:hover {
            background: #e55f00;
        }

        .btn-secondary {
            background: white;
            color: var(--belgo-azul);
            border: 1px solid #ddd;
        }

        .btn-secondary:hover {
            background: #f5f5f5;
        }

        /* Layout */
        .app-container {
            display: flex;
            height: calc(100vh - 60px);
        }

        /* Sidebar */
        .sidebar {
            width: 260px;
            background: white;
            border-right: 1px solid #e0e0e0;
            display: flex;
            flex-direction: column;
        }

        .sidebar-header {
            padding: 20px;
            border-bottom: 1px solid #e0e0e0;
        }

        .sidebar-title {
            font-size: 14px;
            font-weight: 600;
            color: var(--belgo-cinza);
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 12px;
        }

        .project-list {
            flex: 1;
            overflow-y: auto;
            padding: 8px;
        }

        .project-item {
            padding: 12px 16px;
            border-radius: 8px;
            cursor: pointer;
            margin-bottom: 4px;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .project-item:hover {
            background: #f5f7fa;
        }

        .project-item.active {
            background: #e8f4fd;
            border-left: 3px solid var(--belgo-azul-claro);
        }

        .project-name {
            font-weight: 600;
            font-size: 14px;
        }

        .project-count {
            background: var(--belgo-cinza);
            color: white;
            font-size: 11px;
            padding: 2px 8px;
            border-radius: 10px;
        }

        .sidebar-footer {
            padding: 16px;
            border-top: 1px solid #e0e0e0;
        }

        .btn-new-project {
            width: 100%;
            justify-content: center;
        }

        /* Main Content */
        .main-content {
            flex: 1;
            display: flex;
            flex-direction: column;
            overflow: hidden;
        }

        /* Project Header */
        .project-header {
            padding: 20px 24px;
            background: white;
            border-bottom: 1px solid #e0e0e0;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .project-info h1 {
            font-size: 24px;
            font-weight: 700;
            margin-bottom: 4px;
        }

        .project-info p {
            color: var(--belgo-cinza);
            font-size: 14px;
        }

        .project-stats {
            display: flex;
            gap: 24px;
        }

        .stat-item {
            text-align: center;
        }

        .stat-value {
            font-size: 24px;
            font-weight: 700;
        }

        .stat-label {
            font-size: 12px;
            color: var(--belgo-cinza);
        }

        /* Kanban Board */
        .kanban-board {
            flex: 1;
            display: flex;
            gap: 16px;
            padding: 20px 24px;
            overflow-x: auto;
        }

        .kanban-column {
            min-width: 280px;
            max-width: 280px;
            background: #f0f2f5;
            border-radius: 12px;
            display: flex;
            flex-direction: column;
            max-height: 100%;
        }

        .column-header {
            padding: 16px;
            border-radius: 12px 12px 0 0;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .column-header h3 {
            font-size: 14px;
            font-weight: 600;
            color: white;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .column-count {
            background: rgba(255,255,255,0.3);
            padding: 2px 8px;
            border-radius: 10px;
            font-size: 12px;
        }

        .column-header.backlog { background: var(--kanban-backlog); }
        .column-header.todo { background: var(--kanban-todo); }
        .column-header.in_development { background: var(--kanban-dev); color: var(--belgo-azul); }
        .column-header.in_development h3 { color: var(--belgo-azul); }
        .column-header.in_testing { background: var(--kanban-test); }
        .column-header.ready_to_deploy { background: var(--kanban-deploy); }
        .column-header.done { background: var(--kanban-done); }

        .column-content {
            flex: 1;
            padding: 8px;
            overflow-y: auto;
            min-height: 200px;
        }

        /* Task Card */
        .task-card {
            background: white;
            border-radius: 8px;
            padding: 12px;
            margin-bottom: 8px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            cursor: grab;
            transition: all 0.2s;
        }

        .task-card:hover {
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            transform: translateY(-2px);
        }

        .task-card.dragging {
            opacity: 0.5;
            cursor: grabbing;
        }

        .task-card-header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 8px;
        }

        .task-priority {
            width: 8px;
            height: 8px;
            border-radius: 50%;
        }

        .task-priority.low { background: var(--belgo-verde); }
        .task-priority.medium { background: var(--belgo-amarelo); }
        .task-priority.high { background: var(--belgo-laranja); }
        .task-priority.urgent { background: var(--belgo-vermelho); }

        .task-id {
            font-size: 11px;
            color: var(--belgo-cinza);
            font-weight: 600;
        }

        .task-title {
            font-size: 14px;
            font-weight: 600;
            margin-bottom: 8px;
            line-height: 1.4;
        }

        .task-footer {
            display: flex;
            align-items: center;
            justify-content: space-between;
            font-size: 12px;
            color: var(--belgo-cinza);
        }

        .task-due {
            display: flex;
            align-items: center;
            gap: 4px;
        }

        .task-assignee {
            display: flex;
            align-items: center;
            gap: 4px;
        }

        .avatar {
            width: 24px;
            height: 24px;
            border-radius: 50%;
            background: var(--belgo-azul-claro);
            color: white;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 10px;
            font-weight: 600;
        }

        /* Add Task Button */
        .add-task-btn {
            width: 100%;
            padding: 10px;
            border: 2px dashed #ccc;
            border-radius: 8px;
            background: transparent;
            color: var(--belgo-cinza);
            cursor: pointer;
            font-size: 14px;
            transition: all 0.2s;
            margin-top: 8px;
        }

        .add-task-btn:hover {
            border-color: var(--belgo-azul-claro);
            color: var(--belgo-azul-claro);
        }

        /* Modal */
        .modal-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 1000;
        }

        .modal {
            background: white;
            border-radius: 12px;
            width: 500px;
            max-width: 90vw;
            max-height: 90vh;
            overflow-y: auto;
        }

        .modal-header {
            padding: 20px 24px;
            border-bottom: 1px solid #e0e0e0;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }

        .modal-header h2 {
            font-size: 18px;
            font-weight: 700;
        }

        .modal-close {
            background: none;
            border: none;
            font-size: 24px;
            cursor: pointer;
            color: var(--belgo-cinza);
        }

        .modal-body {
            padding: 24px;
        }

        .form-group {
            margin-bottom: 20px;
        }

        .form-group label {
            display: block;
            font-size: 14px;
            font-weight: 600;
            margin-bottom: 8px;
        }

        .form-group input,
        .form-group textarea,
        .form-group select {
            width: 100%;
            padding: 10px 12px;
            border: 1px solid #ddd;
            border-radius: 6px;
            font-size: 14px;
            font-family: inherit;
        }

        .form-group textarea {
            min-height: 100px;
            resize: vertical;
        }

        .form-group input:focus,
        .form-group textarea:focus,
        .form-group select:focus {
            outline: none;
            border-color: var(--belgo-azul-claro);
        }

        .modal-footer {
            padding: 16px 24px;
            border-top: 1px solid #e0e0e0;
            display: flex;
            justify-content: flex-end;
            gap: 12px;
        }

        /* Loading */
        .loading {
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 40px;
            color: var(--belgo-cinza);
        }

        .spinner {
            width: 40px;
            height: 40px;
            border: 3px solid #f3f3f3;
            border-top: 3px solid var(--belgo-azul-claro);
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin-right: 12px;
        }

        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }

        /* Empty State */
        .empty-state {
            text-align: center;
            padding: 60px 40px;
            color: var(--belgo-cinza);
        }

        .empty-state svg {
            width: 80px;
            height: 80px;
            margin-bottom: 16px;
            opacity: 0.5;
        }

        .empty-state h3 {
            font-size: 18px;
            margin-bottom: 8px;
        }

        .empty-state p {
            font-size: 14px;
            margin-bottom: 20px;
        }

        /* Toast */
        .toast {
            position: fixed;
            bottom: 24px;
            right: 24px;
            padding: 12px 20px;
            border-radius: 8px;
            color: white;
            font-weight: 600;
            z-index: 2000;
            animation: slideIn 0.3s ease;
        }

        .toast.success { background: var(--belgo-verde); }
        .toast.error { background: var(--belgo-vermelho); }
        .toast.info { background: var(--belgo-azul-claro); }

        @keyframes slideIn {
            from { transform: translateX(100%); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
        }

        /* Sortable ghost */
        .sortable-ghost {
            opacity: 0.4;
        }

        .sortable-chosen {
            opacity: 0.8;
        }
    </style>
</head>
<body>
    <div id="app">
        <!-- Header -->
        <header class="header">
            <div class="header-logo">
                <svg viewBox="0 0 32 32" fill="none">
                    <rect width="32" height="32" rx="6" fill="#FF6C00"/>
                    <path d="M8 10h16M8 16h12M8 22h8" stroke="white" stroke-width="2.5" stroke-linecap="round"/>
                </svg>
                <div>
                    <div class="header-title">Plataforma E</div>
                    <div class="header-subtitle">Kanban Dashboard</div>
                </div>
            </div>
            <div class="header-actions">
                <button class="btn btn-primary" @click="showNewTaskModal = true" v-if="selectedProject">
                    <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                        <path d="M8 3v10M3 8h10" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
                    </svg>
                    Nova Tarefa
                </button>
            </div>
        </header>

        <div class="app-container">
            <!-- Sidebar -->
            <aside class="sidebar">
                <div class="sidebar-header">
                    <div class="sidebar-title">Projetos</div>
                </div>
                <div class="project-list">
                    <div v-if="loading" class="loading">
                        <div class="spinner"></div>
                        Carregando...
                    </div>
                    <div v-else-if="projects.length === 0" class="empty-state">
                        <p>Nenhum projeto ainda</p>
                    </div>
                    <div v-else>
                        <div
                            v-for="project in projects"
                            :key="project.project_id"
                            class="project-item"
                            :class="{ active: selectedProject?.project_id === project.project_id }"
                            @click="selectProject(project)"
                        >
                            <span class="project-name">{{ project.name }}</span>
                            <span class="project-count">{{ project.tasks_count || 0 }}</span>
                        </div>
                    </div>
                </div>
                <div class="sidebar-footer">
                    <button class="btn btn-secondary btn-new-project" @click="showNewProjectModal = true">
                        <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                            <path d="M8 3v10M3 8h10" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
                        </svg>
                        Novo Projeto
                    </button>
                </div>
            </aside>

            <!-- Main Content -->
            <main class="main-content">
                <div v-if="!selectedProject" class="empty-state">
                    <svg viewBox="0 0 80 80" fill="none">
                        <rect x="10" y="10" width="60" height="60" rx="8" stroke="#ccc" stroke-width="2"/>
                        <rect x="18" y="22" width="44" height="8" rx="2" fill="#eee"/>
                        <rect x="18" y="36" width="30" height="8" rx="2" fill="#eee"/>
                        <rect x="18" y="50" width="38" height="8" rx="2" fill="#eee"/>
                    </svg>
                    <h3>Selecione um projeto</h3>
                    <p>Escolha um projeto na barra lateral ou crie um novo</p>
                    <button class="btn btn-primary" @click="showNewProjectModal = true">Criar Projeto</button>
                </div>

                <template v-else>
                    <!-- Project Header -->
                    <div class="project-header">
                        <div class="project-info">
                            <h1>{{ selectedProject.name }}</h1>
                            <p>{{ selectedProject.description || 'Sem descricao' }}</p>
                        </div>
                        <div class="project-stats">
                            <div class="stat-item">
                                <div class="stat-value">{{ totalTasks }}</div>
                                <div class="stat-label">Total</div>
                            </div>
                            <div class="stat-item">
                                <div class="stat-value" style="color: var(--belgo-amarelo)">{{ tasksInProgress }}</div>
                                <div class="stat-label">Em Andamento</div>
                            </div>
                            <div class="stat-item">
                                <div class="stat-value" style="color: var(--belgo-verde)">{{ tasksDone }}</div>
                                <div class="stat-label">Concluidas</div>
                            </div>
                        </div>
                    </div>

                    <!-- Kanban Board -->
                    <div class="kanban-board">
                        <div
                            v-for="column in columns"
                            :key="column.id"
                            class="kanban-column"
                        >
                            <div class="column-header" :class="column.id">
                                <h3>
                                    {{ column.name }}
                                    <span class="column-count">{{ getColumnTasks(column.id).length }}</span>
                                </h3>
                            </div>
                            <div
                                class="column-content"
                                :data-status="column.id"
                                :ref="el => setColumnRef(column.id, el)"
                            >
                                <div
                                    v-for="task in getColumnTasks(column.id)"
                                    :key="task.task_id"
                                    class="task-card"
                                    :data-task-id="task.task_id"
                                    @click="editTask(task)"
                                >
                                    <div class="task-card-header">
                                        <span class="task-priority" :class="task.priority"></span>
                                        <span class="task-id">{{ task.task_id }}</span>
                                    </div>
                                    <div class="task-title">{{ task.title }}</div>
                                    <div class="task-footer">
                                        <span class="task-due" v-if="task.due_date">
                                            <svg width="12" height="12" viewBox="0 0 12 12" fill="none">
                                                <rect x="1" y="2" width="10" height="9" rx="1" stroke="currentColor"/>
                                                <path d="M1 5h10M4 1v2M8 1v2" stroke="currentColor"/>
                                            </svg>
                                            {{ formatDate(task.due_date) }}
                                        </span>
                                        <span class="task-assignee" v-if="task.assignee">
                                            <span class="avatar">{{ getInitials(task.assignee) }}</span>
                                        </span>
                                    </div>
                                </div>
                                <button class="add-task-btn" @click="openNewTaskModal(column.id)">+ Adicionar Tarefa</button>
                            </div>
                        </div>
                    </div>
                </template>
            </main>
        </div>

        <!-- New Project Modal -->
        <div class="modal-overlay" v-if="showNewProjectModal" @click.self="showNewProjectModal = false">
            <div class="modal">
                <div class="modal-header">
                    <h2>Novo Projeto</h2>
                    <button class="modal-close" @click="showNewProjectModal = false">&times;</button>
                </div>
                <div class="modal-body">
                    <div class="form-group">
                        <label>Nome do Projeto</label>
                        <input type="text" v-model="newProject.name" placeholder="Ex: Sistema de Vendas">
                    </div>
                    <div class="form-group">
                        <label>Descricao</label>
                        <textarea v-model="newProject.description" placeholder="Descreva o projeto..."></textarea>
                    </div>
                </div>
                <div class="modal-footer">
                    <button class="btn btn-secondary" @click="showNewProjectModal = false">Cancelar</button>
                    <button class="btn btn-primary" @click="createProject" :disabled="!newProject.name">Criar Projeto</button>
                </div>
            </div>
        </div>

        <!-- New/Edit Task Modal -->
        <div class="modal-overlay" v-if="showNewTaskModal || editingTask" @click.self="closeTaskModal">
            <div class="modal">
                <div class="modal-header">
                    <h2>{{ editingTask ? 'Editar Tarefa' : 'Nova Tarefa' }}</h2>
                    <button class="modal-close" @click="closeTaskModal">&times;</button>
                </div>
                <div class="modal-body">
                    <div class="form-group">
                        <label>Titulo</label>
                        <input type="text" v-model="taskForm.title" placeholder="Ex: Implementar login">
                    </div>
                    <div class="form-group">
                        <label>Descricao</label>
                        <textarea v-model="taskForm.description" placeholder="Descreva a tarefa..."></textarea>
                    </div>
                    <div class="form-group">
                        <label>Prioridade</label>
                        <select v-model="taskForm.priority">
                            <option value="low">Baixa</option>
                            <option value="medium">Media</option>
                            <option value="high">Alta</option>
                            <option value="urgent">Urgente</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label>Responsavel</label>
                        <input type="text" v-model="taskForm.assignee" placeholder="Nome do responsavel">
                    </div>
                    <div class="form-group">
                        <label>Data de Entrega</label>
                        <input type="date" v-model="taskForm.due_date">
                    </div>
                </div>
                <div class="modal-footer">
                    <button class="btn btn-secondary" v-if="editingTask" @click="deleteCurrentTask" style="margin-right: auto; color: var(--belgo-vermelho);">
                        Excluir
                    </button>
                    <button class="btn btn-secondary" @click="closeTaskModal">Cancelar</button>
                    <button class="btn btn-primary" @click="saveTask" :disabled="!taskForm.title">
                        {{ editingTask ? 'Salvar' : 'Criar Tarefa' }}
                    </button>
                </div>
            </div>
        </div>

        <!-- Toast -->
        <div class="toast" :class="toast.type" v-if="toast.show">{{ toast.message }}</div>
    </div>

    <script>
        const { createApp, ref, reactive, computed, onMounted, nextTick, watch } = Vue;

        createApp({
            setup() {
                // State
                const loading = ref(true);
                const projects = ref([]);
                const selectedProject = ref(null);
                const kanbanBoard = ref({});
                const showNewProjectModal = ref(false);
                const showNewTaskModal = ref(false);
                const editingTask = ref(null);
                const columnRefs = ref({});
                const sortableInstances = ref({});

                // Toast
                const toast = reactive({ show: false, message: '', type: 'info' });

                // Forms
                const newProject = reactive({ name: '', description: '' });
                const taskForm = reactive({
                    title: '',
                    description: '',
                    priority: 'medium',
                    assignee: '',
                    due_date: '',
                    status: 'backlog'
                });

                // Columns config
                const columns = [
                    { id: 'backlog', name: 'Backlog', color: '#636466' },
                    { id: 'todo', name: 'To Do', color: '#00B5F1' },
                    { id: 'in_development', name: 'Em Desenvolvimento', color: '#FDB913' },
                    { id: 'in_testing', name: 'Em Teste', color: '#634976' },
                    { id: 'ready_to_deploy', name: 'Pronto para Deploy', color: '#00A799' },
                    { id: 'done', name: 'Concluido', color: '#003B4A' }
                ];

                // Computed
                const totalTasks = computed(() => {
                    return Object.values(kanbanBoard.value).reduce((sum, tasks) => sum + tasks.length, 0);
                });

                const tasksInProgress = computed(() => {
                    return (kanbanBoard.value.in_development?.length || 0) +
                           (kanbanBoard.value.in_testing?.length || 0);
                });

                const tasksDone = computed(() => {
                    return kanbanBoard.value.done?.length || 0;
                });

                // Methods
                const showToast = (message, type = 'info') => {
                    toast.message = message;
                    toast.type = type;
                    toast.show = true;
                    setTimeout(() => toast.show = false, 3000);
                };

                const fetchProjects = async () => {
                    try {
                        const res = await fetch('/api/projects');
                        projects.value = await res.json();
                    } catch (error) {
                        showToast('Erro ao carregar projetos', 'error');
                    } finally {
                        loading.value = false;
                    }
                };

                const selectProject = async (project) => {
                    selectedProject.value = project;
                    await fetchKanban();
                };

                const fetchKanban = async () => {
                    if (!selectedProject.value) return;
                    try {
                        const res = await fetch(`/api/projects/${selectedProject.value.project_id}/kanban`);
                        const data = await res.json();
                        kanbanBoard.value = data.board;
                        await nextTick();
                        initSortable();
                    } catch (error) {
                        showToast('Erro ao carregar kanban', 'error');
                    }
                };

                const createProject = async () => {
                    try {
                        const res = await fetch('/api/projects', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(newProject)
                        });
                        const project = await res.json();
                        projects.value.push(project);
                        showNewProjectModal.value = false;
                        newProject.name = '';
                        newProject.description = '';
                        selectProject(project);
                        showToast('Projeto criado com sucesso!', 'success');
                    } catch (error) {
                        showToast('Erro ao criar projeto', 'error');
                    }
                };

                const openNewTaskModal = (status = 'backlog') => {
                    taskForm.status = status;
                    taskForm.title = '';
                    taskForm.description = '';
                    taskForm.priority = 'medium';
                    taskForm.assignee = '';
                    taskForm.due_date = '';
                    showNewTaskModal.value = true;
                };

                const editTask = (task) => {
                    editingTask.value = task;
                    taskForm.title = task.title;
                    taskForm.description = task.description || '';
                    taskForm.priority = task.priority;
                    taskForm.assignee = task.assignee || '';
                    taskForm.due_date = task.due_date ? task.due_date.split('T')[0] : '';
                    taskForm.status = task.status;
                };

                const closeTaskModal = () => {
                    showNewTaskModal.value = false;
                    editingTask.value = null;
                };

                const saveTask = async () => {
                    try {
                        if (editingTask.value) {
                            // Update existing task
                            const res = await fetch(`/api/tasks/${editingTask.value.task_id}`, {
                                method: 'PUT',
                                headers: { 'Content-Type': 'application/json' },
                                body: JSON.stringify({
                                    title: taskForm.title,
                                    description: taskForm.description,
                                    priority: taskForm.priority,
                                    assignee: taskForm.assignee || null,
                                    due_date: taskForm.due_date || null
                                })
                            });
                            showToast('Tarefa atualizada!', 'success');
                        } else {
                            // Create new task
                            const res = await fetch('/api/tasks', {
                                method: 'POST',
                                headers: { 'Content-Type': 'application/json' },
                                body: JSON.stringify({
                                    title: taskForm.title,
                                    description: taskForm.description,
                                    project_id: selectedProject.value.project_id,
                                    status: taskForm.status,
                                    priority: taskForm.priority,
                                    assignee: taskForm.assignee || null,
                                    due_date: taskForm.due_date || null
                                })
                            });
                            showToast('Tarefa criada!', 'success');
                        }
                        closeTaskModal();
                        await fetchKanban();
                        await fetchProjects();
                    } catch (error) {
                        showToast('Erro ao salvar tarefa', 'error');
                    }
                };

                const deleteCurrentTask = async () => {
                    if (!editingTask.value) return;
                    if (!confirm('Tem certeza que deseja excluir esta tarefa?')) return;

                    try {
                        await fetch(`/api/tasks/${editingTask.value.task_id}`, { method: 'DELETE' });
                        showToast('Tarefa excluida!', 'success');
                        closeTaskModal();
                        await fetchKanban();
                        await fetchProjects();
                    } catch (error) {
                        showToast('Erro ao excluir tarefa', 'error');
                    }
                };

                const getColumnTasks = (status) => {
                    return kanbanBoard.value[status] || [];
                };

                const setColumnRef = (columnId, el) => {
                    if (el) {
                        columnRefs.value[columnId] = el;
                    }
                };

                const initSortable = () => {
                    // Destroy existing instances
                    Object.values(sortableInstances.value).forEach(s => s.destroy());
                    sortableInstances.value = {};

                    // Create new instances for each column
                    columns.forEach(column => {
                        const el = columnRefs.value[column.id];
                        if (el) {
                            sortableInstances.value[column.id] = new Sortable(el, {
                                group: 'kanban',
                                animation: 150,
                                ghostClass: 'sortable-ghost',
                                chosenClass: 'sortable-chosen',
                                dragClass: 'task-card-dragging',
                                filter: '.add-task-btn',
                                onEnd: async (evt) => {
                                    const taskId = evt.item.dataset.taskId;
                                    const newStatus = evt.to.dataset.status;
                                    const newIndex = evt.newIndex;

                                    try {
                                        await fetch(`/api/tasks/${taskId}/move`, {
                                            method: 'PATCH',
                                            headers: { 'Content-Type': 'application/json' },
                                            body: JSON.stringify({
                                                status: newStatus,
                                                order: newIndex
                                            })
                                        });
                                        await fetchKanban();
                                    } catch (error) {
                                        showToast('Erro ao mover tarefa', 'error');
                                        await fetchKanban();
                                    }
                                }
                            });
                        }
                    });
                };

                const formatDate = (dateStr) => {
                    if (!dateStr) return '';
                    const date = new Date(dateStr);
                    return date.toLocaleDateString('pt-BR', { day: '2-digit', month: '2-digit' });
                };

                const getInitials = (name) => {
                    if (!name) return '?';
                    return name.split(' ').map(n => n[0]).join('').substring(0, 2).toUpperCase();
                };

                // Lifecycle
                onMounted(async () => {
                    await fetchProjects();
                    if (projects.value.length > 0) {
                        selectProject(projects.value[0]);
                    }
                });

                return {
                    loading,
                    projects,
                    selectedProject,
                    kanbanBoard,
                    showNewProjectModal,
                    showNewTaskModal,
                    editingTask,
                    columns,
                    newProject,
                    taskForm,
                    toast,
                    totalTasks,
                    tasksInProgress,
                    tasksDone,
                    fetchProjects,
                    selectProject,
                    createProject,
                    openNewTaskModal,
                    editTask,
                    closeTaskModal,
                    saveTask,
                    deleteCurrentTask,
                    getColumnTasks,
                    setColumnRef,
                    formatDate,
                    getInitials
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""


@app.get("/", response_class=HTMLResponse)
async def kanban_dashboard():
    """Serve the Kanban dashboard"""
    return HTMLResponse(content=KANBAN_DASHBOARD_HTML)


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import os
    host = os.getenv("DASHBOARD_HOST", "127.0.0.1")
    port = int(os.getenv("DASHBOARD_PORT", "9001"))
    print(f"\n{'='*60}")
    print(f"  Plataforma E - Kanban Dashboard v5.0")
    print(f"  Visual: Belgo Arames Brand Identity")
    print(f"  Acesse: http://{host}:{port}")
    print(f"{'='*60}\n")
    uvicorn.run(app, host=host, port=port)
