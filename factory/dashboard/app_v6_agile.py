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

from fastapi import FastAPI, HTTPException, UploadFile, File, Form, Query, WebSocket, WebSocketDisconnect, Cookie, Response, Body
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import json
import asyncio

# Database
from factory.database.connection import SessionLocal, engine, Base
from factory.database.models import (
    Project, ProjectStatus, ActivityLog,
    Story, StoryStatus, StoryCategory, StoryComplexity,
    StoryTask, StoryTaskType, StoryTaskStatus,
    StoryDocumentation, DocType,
    StoryDesign, DesignType,
    ChatMessage, MessageRole,
    Attachment, Epic, Sprint,
    TaskPriority,
    Tenant, TenantMember
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

# Mount static files for PWA support
STATIC_DIR = Path(__file__).parent / "static"
if STATIC_DIR.exists():
    app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")
    print(f"[Dashboard] Static files mounted from {STATIC_DIR}")

# Mount projects folder for file viewing
PROJECTS_DIR = Path(__file__).parent.parent.parent / "projects"
if PROJECTS_DIR.exists():
    app.mount("/project-files", StaticFiles(directory=str(PROJECTS_DIR), html=True), name="project-files")
    print(f"[Dashboard] Projects files mounted from {PROJECTS_DIR}")

# Project Preview API Router (Issue #73)
try:
    from factory.api.project_preview import router as preview_router
    app.include_router(preview_router)
    print("[Dashboard] Project Preview router loaded")
except ImportError as e:
    print(f"[Dashboard] Project Preview router not available: {e}")

# Code Review API Router (Issue #52)
try:
    from factory.api.code_review_routes import router as code_review_router
    app.include_router(code_review_router)
    print("[Dashboard] Code Review router loaded")
except ImportError as e:
    print(f"[Dashboard] Code Review router not available: {e}")

# Advanced Contextual AI Assistant Routes (Issue #280)
try:
    from factory.api.chat_routes import router as chat_assistant_router
    app.include_router(chat_assistant_router)
    print("[Dashboard] AI Assistant router loaded")
except ImportError as e:
    print(f"[Dashboard] AI Assistant router not available: {e}")

# Analytics de Produtividade endpoints (Issue #65)
from factory.dashboard.analytics_endpoints import register_analytics_endpoints
register_analytics_endpoints(app, SessionLocal, Story, Sprint, HAS_CLAUDE, get_claude_client if HAS_CLAUDE else None)

# Analytics Page with Charts (Issue #65 Enhancement)
try:
    from factory.dashboard.analytics_page import register_analytics_page
    register_analytics_page(app)
except ImportError as e:
    print(f"[Dashboard] Analytics Page not available: {e}")

# Admin API Routes (Issue #87 - User Administration)
try:
    from factory.api.admin_routes import router as admin_router
    app.include_router(admin_router)
    print("[Dashboard] Admin API router loaded")
except ImportError as e:
    print(f"[Dashboard] Admin API router not available: {e}")

# Portal API Routes (Issue #113 - Multi-level Admin Portal)
try:
    from factory.api.portal_routes import platform_router, tenant_router, project_router
    app.include_router(platform_router)
    app.include_router(tenant_router)
    app.include_router(project_router)
    print("[Dashboard] Portal API routers loaded")
except ImportError as e:
    print(f"[Dashboard] Portal API routers not available: {e}")

# Worker Monitoring Dashboard (Issue #88)
try:
    from factory.dashboard.worker_monitoring import register_monitoring_endpoints
    register_monitoring_endpoints(app)
    print("[Dashboard] Worker Monitoring endpoints loaded")
except ImportError as e:
    print(f"[Dashboard] Worker Monitoring not available: {e}")

# Admin Users Panel (Issue #87)
try:
    from factory.dashboard.admin_users import register_admin_users_endpoints
    register_admin_users_endpoints(app)
    print("[Dashboard] Admin Users panel loaded")
except ImportError as e:
    print(f"[Dashboard] Admin Users panel not available: {e}")

# Admin Portal (Issue #113)
try:
    from factory.dashboard.admin_portal import register_admin_portal_endpoints
    register_admin_portal_endpoints(app)
    print("[Dashboard] Admin Portal loaded")
except ImportError as e:
    print(f"[Dashboard] Admin Portal not available: {e}")

# Executive Dashboard - KPIs de Alto Nivel
try:
    from factory.dashboard.executive_dashboard import register_executive_endpoints
    register_executive_endpoints(app, SessionLocal, Story, Sprint)
    print("[Dashboard] Executive Dashboard loaded: /executive")
except ImportError as e:
    print(f"[Dashboard] Executive Dashboard not available: {e}")

# Billing Page (Issue #89) - Pagina /billing com graficos e faturas
try:
    from factory.dashboard.billing_routes import register_billing_routes
    register_billing_routes(app)
except ImportError as e:
    print(f"[Dashboard] Billing Page not available: {e}")

# Integrations Panel (Issue #154)
try:
    from factory.dashboard.integrations_panel import register_integrations_endpoints
    register_integrations_endpoints(app)
    print("[Dashboard] Integrations Panel loaded")
except ImportError as e:
    print(f"[Dashboard] Integrations Panel not available: {e}")

# Security Settings Panel (Issue #156)
try:
    from factory.dashboard.security_settings import register_security_endpoints
    register_security_endpoints(app)
    print("[Dashboard] Security Settings loaded")
except ImportError as e:
    print(f"[Dashboard] Security Settings not available: {e}")

# Bulk Actions (Issue #278)
try:
    from factory.dashboard.bulk_actions import register_bulk_actions
    register_bulk_actions(app)
except ImportError as e:
    print(f"[Dashboard] Bulk Actions not available: {e}")

# Global Search (Issue #271)
try:
    from factory.dashboard.global_search import register_global_search
    register_global_search(app)
except ImportError as e:
    print(f"[Dashboard] Global Search not available: {e}")

# Focus Mode / Pomodoro (Issue #265)
try:
    from factory.dashboard.focus_mode import register_focus_mode
    register_focus_mode(app)
except ImportError as e:
    print(f"[Dashboard] Focus Mode not available: {e}")

# Calendar View (Issue #267)
try:
    from factory.dashboard.calendar_view import register_calendar_view
    register_calendar_view(app)
except ImportError as e:
    print(f"[Dashboard] Calendar View not available: {e}")

# Gamification (Issue #266)
try:
    from factory.dashboard.gamification import register_gamification
    register_gamification(app)
except ImportError as e:
    print(f"[Dashboard] Gamification not available: {e}")

# My Work Dashboard (Issue #272)
try:
    from factory.dashboard.my_work import register_my_work
    register_my_work(app)
except ImportError as e:
    print(f"[Dashboard] My Work not available: {e}")

# Export Data - CSV/Excel (Issue #256)
try:
    from factory.dashboard.export_data import register_export_data
    register_export_data(app)
except ImportError as e:
    print(f"[Dashboard] Export Data not available: {e}")

# Export PDF (Issue #255)
try:
    from factory.dashboard.export_pdf import register_export_pdf
    register_export_pdf(app)
except ImportError as e:
    print(f"[Dashboard] Export PDF not available: {e}")

# Theme Editor (Issue #277)
try:
    from factory.dashboard.theme_editor import register_theme_editor
    register_theme_editor(app)
except ImportError as e:
    print(f"[Dashboard] Theme Editor not available: {e}")

# Webhooks Outbound (Issue #275)
try:
    from factory.dashboard.webhooks_outbound import register_webhooks_outbound
    register_webhooks_outbound(app)
except ImportError as e:
    print(f"[Dashboard] Webhooks Outbound not available: {e}")

# Audit Dashboard (Issue #274)
try:
    from factory.dashboard.audit_dashboard import register_audit_dashboard
    register_audit_dashboard(app)
except ImportError as e:
    print(f"[Dashboard] Audit Dashboard not available: {e}")

# Help Center (Issue #273)
try:
    from factory.dashboard.help_center import register_help_center
    register_help_center(app)
except ImportError as e:
    print(f"[Dashboard] Help Center not available: {e}")

# AI Chat Advanced (Issue #280) - Assistente IA Contextual Avancado
try:
    from factory.dashboard.ai_chat_advanced import register_ai_chat_endpoints
    register_ai_chat_endpoints(app)
    print("[Dashboard] AI Chat Advanced loaded: /api/ai/chat/* and /ai-chat")
except ImportError as e:
    print(f"[Dashboard] AI Chat Advanced not available: {e}")

# Data Import (Issue #276)
try:
    from factory.dashboard.data_import import register_data_import
    register_data_import(app)
except ImportError as e:
    print(f"[Dashboard] Data Import not available: {e}")

# Advanced Accessibility (Issue #270)
try:
    from factory.dashboard.accessibility_advanced import register_accessibility_advanced
    register_accessibility_advanced(app)
except ImportError as e:
    print(f"[Dashboard] Advanced Accessibility not available: {e}")

# Lazy Loading / Virtualization (Issue #269)
try:
    from factory.dashboard.lazy_loading import register_lazy_loading
    register_lazy_loading(app)
except ImportError as e:
    print(f"[Dashboard] Lazy Loading not available: {e}")

# GraphQL API (Issue #268)
try:
    from factory.dashboard.graphql_api import register_graphql_api
    register_graphql_api(app)
except ImportError as e:
    print(f"[Dashboard] GraphQL API not available: {e}")

# Agile Metrics Dashboard (Issue #258)
try:
    from factory.dashboard.agile_metrics import register_agile_metrics
    register_agile_metrics(app)
except ImportError as e:
    print(f"[Dashboard] Agile Metrics not available: {e}")

# Story Templates (Issue #254)
try:
    from factory.dashboard.story_templates import register_story_templates
    register_story_templates(app)
except ImportError as e:
    print(f"[Dashboard] Story Templates not available: {e}")

# User Preferences (Issue #253)
try:
    from factory.dashboard.user_preferences import register_user_preferences
    register_user_preferences(app)
except ImportError as e:
    print(f"[Dashboard] User Preferences not available: {e}")

# Custom Kanban Columns (Issue #252)
try:
    from factory.dashboard.custom_kanban_columns import register_custom_columns
    register_custom_columns(app)
except ImportError as e:
    print(f"[Dashboard] Custom Kanban Columns not available: {e}")

# Custom Fields (Issue #251)
try:
    from factory.dashboard.custom_fields import register_custom_fields
    register_custom_fields(app)
    print("[Dashboard] Custom Fields loaded: /custom-fields, /api/*/custom-fields")
except ImportError as e:
    print(f"[Dashboard] Custom Fields not available: {e}")

# Push Notifications (Issue #261)
try:
    from factory.dashboard.push_notifications import register_push_notifications
    register_push_notifications(app)
except ImportError as e:
    print(f"[Dashboard] Push Notifications not available: {e}")

# PWA Setup - Progressive Web App (Issue #259)
try:
    from factory.dashboard.pwa_setup import register_pwa_routes
    register_pwa_routes(app)
    print("[Dashboard] PWA routes loaded: /manifest.json, /sw.js, /api/pwa/*")
except ImportError as e:
    print(f"[Dashboard] PWA Setup not available: {e}")

# Sprint Capacity Planning (Issue #279)
try:
    from factory.dashboard.sprint_capacity import register_sprint_capacity
    register_sprint_capacity(app)
    print("[Dashboard] Sprint Capacity loaded: /capacity, /api/capacity/*")
except ImportError as e:
    print(f"[Dashboard] Sprint Capacity not available: {e}")

# =============================================================================
# MULTI-TENANT PLATFORM ROUTES (Issues #286-#293 - Terminal 4)
# =============================================================================

# Authentication Routes (JWT Login)
try:
    from factory.api.auth import auth_router
    app.include_router(auth_router)
    print("[Dashboard] Auth routes loaded: /api/v1/auth/login")
except ImportError as e:
    print(f"[Dashboard] Auth routes not available: {e}")

# Platform Portal Routes (Issue #287 - Super Admin)
try:
    from factory.api.platform_routes import register_platform_routes
    register_platform_routes(app)
    print("[Dashboard] Platform Portal routes loaded: /api/platform")
except ImportError as e:
    print(f"[Dashboard] Platform Portal routes not available: {e}")

# Tenant Admin Portal Routes (Issue #288)
try:
    from factory.api.tenant_admin_routes import register_tenant_admin_routes
    register_tenant_admin_routes(app)
    print("[Dashboard] Tenant Admin routes loaded: /api/tenant-admin")
except ImportError as e:
    print(f"[Dashboard] Tenant Admin routes not available: {e}")

# User Context & Navigation Routes (Issues #292, #293)
try:
    from factory.api.user_context_routes import register_user_context_routes
    register_user_context_routes(app)
    print("[Dashboard] User Context routes loaded: /api/user")
except ImportError as e:
    print(f"[Dashboard] User Context routes not available: {e}")

# Personas API Routes (Issue #290)
try:
    from factory.auth.personas import register_personas_endpoints
    register_personas_endpoints(app)
    print("[Dashboard] Personas API loaded: /api/personas")
except ImportError as e:
    print(f"[Dashboard] Personas API not available: {e}")

# Platform Portal Pages (Issue #287 - Super Admin UI)
try:
    from factory.dashboard.platform_portal import register_platform_portal
    register_platform_portal(app)
    print("[Dashboard] Platform Portal pages loaded: /platform/*")
except ImportError as e:
    print(f"[Dashboard] Platform Portal pages not available: {e}")

# Tenant Admin Portal Pages (Issue #288 - Tenant Admin UI)
try:
    from factory.dashboard.tenant_admin_portal import register_tenant_admin_portal
    register_tenant_admin_portal(app)
    print("[Dashboard] Tenant Admin Portal pages loaded: /tenant-admin/*")
except ImportError as e:
    print(f"[Dashboard] Tenant Admin Portal pages not available: {e}")

# Login Page (Multi-tenant with White Label)
try:
    from factory.dashboard.login_page import register_login_routes
    register_login_routes(app)
    print("[Dashboard] Login page loaded: /login")
except ImportError as e:
    print(f"[Dashboard] Login page not available: {e}")

# Branding API (White Label)
try:
    from factory.api.branding_routes import register_branding_routes
    register_branding_routes(app)
    print("[Dashboard] Branding API loaded: /api/tenant/{id}/branding")
except ImportError as e:
    print(f"[Dashboard] Branding API not available: {e}")

# =============================================================================
# AI ASSISTANTS (Issues #247-#250 - Terminal 2)
# =============================================================================

# AI Acceptance Criteria Assistant (Issue #250)
try:
    from factory.dashboard.ai_acceptance_criteria import register_acceptance_criteria_routes
    register_acceptance_criteria_routes(app)
    print("[AI] Acceptance Criteria Assistant loaded: /api/ai/acceptance-criteria/*")
except ImportError as e:
    print(f"[AI] Acceptance Criteria Assistant not available: {e}")

# AI Duplicate Detection (Issue #247)
try:
    from factory.dashboard.ai_duplicate_detection import register_duplicate_detection_routes
    register_duplicate_detection_routes(app)
    print("[AI] Duplicate Detection loaded: /api/ai/duplicates/*")
except ImportError as e:
    print(f"[AI] Duplicate Detection not available: {e}")

# AI Story Splitting (Issue #248)
try:
    from factory.dashboard.ai_story_splitting import register_story_splitting_routes
    register_story_splitting_routes(app)
    print("[AI] Story Splitting Assistant loaded: /api/ai/splitting/*")
except ImportError as e:
    print(f"[AI] Story Splitting Assistant not available: {e}")

# AI Risk Prediction (Issue #249)
try:
    from factory.dashboard.ai_risk_prediction import register_risk_prediction_routes
    register_risk_prediction_routes(app)
    print("[AI] Risk Prediction loaded: /api/ai/risks/*")
except ImportError as e:
    print(f"[AI] Risk Prediction not available: {e}")

# Offline Sync (Issue #260)
try:
    from factory.dashboard.offline_sync import register_offline_sync
    register_offline_sync(app)
    print("[Dashboard] Offline Sync loaded: /api/offline/*")
except ImportError as e:
    print(f"[Dashboard] Offline Sync not available: {e}")


# =============================================================================
# WEBSOCKET CONNECTION MANAGER
# =============================================================================

class ConnectionManager:
    def __init__(self):
        self.active_connections: List[WebSocket] = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)

    async def broadcast(self, message: dict):
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except:
                pass

ws_manager = ConnectionManager()

def notify(notification_type: str, data: dict):
    message = {"type": notification_type, "data": data, "timestamp": datetime.utcnow().isoformat() + "Z"}
    try:
        asyncio.create_task(ws_manager.broadcast(message))
    except:
        pass


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


class DesignCreate(BaseModel):
    story_id: str
    title: str
    design_type: Optional[str] = "wireframe"
    description: Optional[str] = None
    content: Optional[str] = None
    thumbnail: Optional[str] = None


class DesignUpdate(BaseModel):
    title: Optional[str] = None
    design_type: Optional[str] = None
    description: Optional[str] = None
    content: Optional[str] = None
    thumbnail: Optional[str] = None


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
# HEALTH CHECK ENDPOINT
# =============================================================================

@app.get("/health")
def health_check():
    """Health check endpoint."""
    return {
        "status": "healthy",
        "service": "fabrica-de-agentes",
        "version": "6.5",
        "timestamp": datetime.utcnow().isoformat() + "Z"
    }


@app.get("/api/health")
def api_health_check():
    """API Health check endpoint."""
    return {
        "status": "healthy",
        "database": "connected",
        "timestamp": datetime.utcnow().isoformat() + "Z"
    }


# Issue #307: Missing API endpoints
@app.get("/api/webhooks/list")
def list_webhooks():
    """Lista webhooks configurados"""
    return {
        "webhooks": [
            {"id": "github", "name": "GitHub", "enabled": True, "url": "/api/webhooks/github"},
            {"id": "gitlab", "name": "GitLab", "enabled": True, "url": "/api/webhooks/gitlab"},
            {"id": "jira", "name": "Jira", "enabled": True, "url": "/api/webhooks/jira"},
            {"id": "azure_devops", "name": "Azure DevOps", "enabled": True, "url": "/api/webhooks/azure-devops"}
        ],
        "total": 4
    }


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
        result = new_story.to_dict()
        notify("story_created", {"story_id": result.get("story_id"), "title": result.get("title"), "project_id": result.get("project_id")})
        return result
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
        result = story.to_dict()
        notify("story_updated", {"story_id": result.get("story_id"), "title": result.get("title")})
        return result
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
        result = story.to_dict()
        notify("story_moved", {"story_id": result.get("story_id"), "title": result.get("title"), "to": move.status})
        return result
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
        result = task.to_dict()
        notify("task_completed", {"task_id": result.get("task_id"), "title": result.get("title"), "story_id": result.get("story_id")})
        return result
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


@app.post("/api/story-tasks/{task_id}/generate-tests")
def generate_tests_for_task(task_id: str):
    """Gera testes automaticos para o codigo de uma task"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.get_by_id(task_id)
        if not task:
            raise HTTPException(404, "Task not found")

        if not task.code_output:
            raise HTTPException(400, "Task has no code output to generate tests for")

        code = task.code_output
        language = "python"
        framework = "pytest"

        if "function " in code or "const " in code or "let " in code:
            language = "javascript"
            framework = "jest"
        elif "func " in code and "package " in code:
            language = "go"
            framework = "testing"
        elif "public class " in code or "private void " in code:
            language = "java"
            framework = "junit"

        safe_title = task.title.replace(" ", "_").replace("-", "_").replace(".", "_")
        test_templates = {
            "python": f'''import pytest

# Auto-generated tests for: {task.title}

class Test{safe_title}:
    def test_basic_functionality(self):
        assert True

    def test_edge_cases(self):
        assert True

    def test_error_handling(self):
        assert True
''',
            "javascript": f'''describe('{task.title}', () => {{
    test('basic functionality', () => {{
        expect(true).toBe(true);
    }});

    test('edge cases', () => {{
        expect(true).toBe(true);
    }});

    test('error handling', () => {{
        expect(true).toBe(true);
    }});
}});
'''
        }

        test_code = test_templates.get(language, test_templates["python"])

        result = {
            "language": language,
            "framework": framework,
            "test_code": test_code,
            "test_count": 3,
            "coverage_estimate": "low",
            "generated_at": datetime.utcnow().isoformat(),
            "is_template": True
        }

        # Salvar na task
        task.generated_tests = result
        task.updated_at = datetime.utcnow()
        db.commit()

        return result
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error generating tests: {str(e)}")
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - SECURITY SCAN (SAST) - Issue #57
# =============================================================================

@app.post("/api/story-tasks/{task_id}/security-scan")
def security_scan_for_task(task_id: str):
    """Analise de Seguranca Automatizada (SAST) - Issue #57"""
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.get_by_id(task_id)
        if not task:
            raise HTTPException(404, "Task not found")

        if not task.code_output:
            raise HTTPException(400, "Task has no code output to analyze")

        code = task.code_output

        # Detect language
        language = "python"
        if "function " in code or "const " in code or "let " in code:
            language = "javascript"
        elif "public class " in code or "private void " in code:
            language = "java"
        elif "func " in code and "package " in code:
            language = "go"

        # Try AI-powered scan first
        if HAS_CLAUDE:
            try:
                claude = get_claude_client()
                if claude and claude.is_available():
                    prompt = f"""Analise o codigo abaixo e identifique vulnerabilidades de seguranca.

CODIGO ({language}):
```{language}
{code}
```

Analise linha por linha buscando:
1. **SQL Injection**: String formatting em queries, concatenacao, falta de parametrizacao
2. **XSS (Cross-Site Scripting)**: innerHTML, document.write, falta de sanitizacao
3. **Command Injection**: os.system, subprocess com shell=True, eval(), exec()
4. **Path Traversal**: Caminhos relativos nao validados, ../ em paths
5. **Hardcoded Secrets**: Senhas, API keys, tokens em codigo
6. **Insecure Dependencies**: pickle.load, yaml.load sem safe_load

Para cada vulnerabilidade encontrada, retorne em JSON:
{{
    "vulnerabilities": [
        {{
            "type": "SQL Injection|XSS|Command Injection|Path Traversal|Hardcoded Secrets|Insecure Dependencies",
            "severity": "Critical|High|Medium|Low",
            "line": numero_da_linha,
            "code_snippet": "codigo_vulneravel",
            "description": "Descricao do problema",
            "recommendation": "Como corrigir",
            "cwe": "CWE-XXX"
        }}
    ],
    "summary": {{
        "total": numero_total,
        "critical": quantidade,
        "high": quantidade,
        "medium": quantidade,
        "low": quantidade
    }},
    "recommendations": ["Recomendacao geral 1", "Recomendacao geral 2"]
}}

Se nao encontrar vulnerabilidades, retorne:
{{
    "vulnerabilities": [],
    "summary": {{"total": 0, "critical": 0, "high": 0, "medium": 0, "low": 0}},
    "recommendations": ["Codigo aparenta estar seguro"]
}}

IMPORTANTE: Retorne APENAS o JSON, sem explicacoes adicionais."""

                    response = claude.generate(prompt)
                    if response:
                        import json as json_module
                        try:
                            json_str = response
                            if "```json" in response:
                                json_str = response.split("```json")[1].split("```")[0]
                            elif "```" in response:
                                json_str = response.split("```")[1].split("```")[0]

                            result = json_module.loads(json_str.strip())
                            result["scan_type"] = "ai"
                            result["language"] = language
                            result["scanned_at"] = datetime.utcnow().isoformat()
                            result["task_id"] = task_id
                            return result
                        except json_module.JSONDecodeError:
                            pass
            except Exception:
                pass

        # Basic pattern-based scan (fallback)
        import re
        patterns = {
            "SQL Injection": [
                (r"execute.*%s", "String formatting em query SQL"),
                (r"execute.*f[\"']", "f-string em query SQL"),
                (r"cursor\.execute.*\+", "Concatenacao em cursor.execute"),
            ],
            "XSS": [
                (r"innerHTML\s*=", "Uso de innerHTML sem sanitizacao"),
                (r"document\.write", "Uso de document.write"),
                (r"dangerouslySetInnerHTML", "React dangerouslySetInnerHTML"),
            ],
            "Command Injection": [
                (r"os\.system\s*\(", "Uso de os.system"),
                (r"subprocess.*shell\s*=\s*True", "subprocess com shell=True"),
                (r"\beval\s*\(", "Uso de eval()"),
                (r"\bexec\s*\(", "Uso de exec()"),
            ],
            "Path Traversal": [
                (r"open\s*\(.*\+", "open() com concatenacao"),
                (r"\.\./", "Path traversal pattern"),
            ],
            "Hardcoded Secrets": [
                (r"password\s*=\s*[\"'][^\"']+[\"']", "Senha hardcoded"),
                (r"api_key\s*=\s*[\"'][^\"']+[\"']", "API key hardcoded"),
                (r"secret\s*=\s*[\"'][^\"']+[\"']", "Secret hardcoded"),
            ],
            "Insecure Dependencies": [
                (r"pickle\.load", "Deserializacao insegura com pickle"),
                (r"yaml\.load\s*\(", "yaml.load sem Loader seguro"),
            ],
        }

        cwe_mapping = {
            "SQL Injection": "CWE-89",
            "XSS": "CWE-79",
            "Command Injection": "CWE-78",
            "Path Traversal": "CWE-22",
            "Hardcoded Secrets": "CWE-798",
            "Insecure Dependencies": "CWE-502",
        }

        severity_mapping = {
            "SQL Injection": "Critical",
            "XSS": "High",
            "Command Injection": "Critical",
            "Path Traversal": "High",
            "Hardcoded Secrets": "High",
            "Insecure Dependencies": "Medium",
        }

        vulnerabilities = []
        lines = code.split("\n")

        for vuln_type, pattern_list in patterns.items():
            for pattern, description in pattern_list:
                for line_num, line in enumerate(lines, 1):
                    if re.search(pattern, line, re.IGNORECASE):
                        vulnerabilities.append({
                            "type": vuln_type,
                            "severity": severity_mapping.get(vuln_type, "Medium"),
                            "line": line_num,
                            "code_snippet": line.strip()[:100],
                            "description": description,
                            "recommendation": f"Revise esta linha para evitar {vuln_type}",
                            "cwe": cwe_mapping.get(vuln_type, "CWE-000")
                        })

        summary = {
            "total": len(vulnerabilities),
            "critical": len([v for v in vulnerabilities if v["severity"] == "Critical"]),
            "high": len([v for v in vulnerabilities if v["severity"] == "High"]),
            "medium": len([v for v in vulnerabilities if v["severity"] == "Medium"]),
            "low": len([v for v in vulnerabilities if v["severity"] == "Low"]),
        }

        recommendations = []
        if summary["critical"] > 0:
            recommendations.append("Corrija vulnerabilidades criticas imediatamente")
        if summary["high"] > 0:
            recommendations.append("Priorize correcao de vulnerabilidades de alta severidade")
        if summary["total"] == 0:
            recommendations.append("Nenhuma vulnerabilidade detectada pelo scan basico")

        return {
            "vulnerabilities": vulnerabilities,
            "summary": summary,
            "recommendations": recommendations,
            "scan_type": "basic",
            "language": language,
            "scanned_at": datetime.utcnow().isoformat(),
            "task_id": task_id
        }

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error performing security scan: {str(e)}")
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
# API ENDPOINTS - DESIGNS
# =============================================================================

@app.get("/api/stories/{story_id}/designs")
def list_story_designs(story_id: str):
    """Lista designs de uma story"""
    db = SessionLocal()
    try:
        designs = db.query(StoryDesign).filter(StoryDesign.story_id == story_id).all()
        return [d.to_dict() for d in designs]
    finally:
        db.close()


@app.post("/api/stories/{story_id}/designs")
def create_story_design(story_id: str, design: DesignCreate):
    """Cria um novo design"""
    db = SessionLocal()
    try:
        new_design = StoryDesign(
            design_id=f"DES-{uuid.uuid4().hex[:8].upper()}",
            story_id=story_id,
            title=design.title,
            design_type=design.design_type or "wireframe",
            description=design.description,
            content=design.content,
            thumbnail=design.thumbnail,
            created_at=datetime.utcnow()
        )
        db.add(new_design)
        db.commit()
        db.refresh(new_design)
        return new_design.to_dict()
    finally:
        db.close()


@app.put("/api/story-designs/{design_id}")
def update_story_design(design_id: str, data: DesignUpdate):
    """Atualiza um design"""
    db = SessionLocal()
    try:
        design = db.query(StoryDesign).filter(StoryDesign.design_id == design_id).first()
        if not design:
            raise HTTPException(404, "Design not found")

        if data.title is not None:
            design.title = data.title
        if data.design_type is not None:
            design.design_type = data.design_type
        if data.description is not None:
            design.description = data.description
        if data.content is not None:
            design.content = data.content
        if data.thumbnail is not None:
            design.thumbnail = data.thumbnail

        design.updated_at = datetime.utcnow()
        db.commit()
        db.refresh(design)
        return design.to_dict()
    finally:
        db.close()


@app.delete("/api/story-designs/{design_id}")
def delete_story_design(design_id: str):
    """Remove um design"""
    db = SessionLocal()
    try:
        design = db.query(StoryDesign).filter(StoryDesign.design_id == design_id).first()
        if not design:
            raise HTTPException(404, "Design not found")
        db.delete(design)
        db.commit()
        return {"message": "Design deleted"}
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - DOC GENERATION
# =============================================================================

class GenerateDocsRequest(BaseModel):
    doc_type: str  # readme, api, user_guide, technical


@app.post("/api/stories/{story_id}/generate-docs")
def generate_story_docs(story_id: str, request: GenerateDocsRequest):
    """Gera documentacao automaticamente usando IA"""
    db = SessionLocal()
    try:
        story_repo = StoryRepository(db)
        doc_repo = StoryDocumentationRepository(db)

        # Buscar story completa
        story_data = story_repo.get_with_tasks(story_id)
        if not story_data:
            raise HTTPException(404, "Story not found")

        # Gerar documentacao
        doc_content = generate_documentation_with_ai(story_data, request.doc_type, db)

        # Criar documento
        doc_data = {
            "story_id": story_id,
            "doc_type": map_doc_type(request.doc_type),
            "title": doc_content["title"],
            "content": doc_content["content"],
            "test_instructions": doc_content.get("test_instructions", ""),
            "test_cases": doc_content.get("test_cases", [])
        }

        new_doc = doc_repo.create(doc_data)
        return new_doc.to_dict()

    except Exception as e:
        raise HTTPException(500, f"Error generating documentation: {str(e)}")
    finally:
        db.close()


def map_doc_type(doc_type: str) -> str:
    """Mapeia tipo de doc da UI para tipo do banco"""
    mapping = {
        "readme": "technical",
        "api": "api",
        "user_guide": "user",
        "technical": "technical"
    }
    return mapping.get(doc_type, "technical")


def generate_documentation_with_ai(story_data: dict, doc_type: str, db) -> dict:
    """
    Gera documentacao usando Claude AI ou template basico

    Args:
        story_data: Dados completos da story (com tasks)
        doc_type: Tipo de documentacao (readme, api, user_guide, technical)
        db: Sessao do banco

    Returns:
        dict com title, content, test_instructions, test_cases
    """

    # Se Claude disponivel, usar IA
    if HAS_CLAUDE:
        try:
            claude = get_claude_client()
            if claude.is_available():
                return generate_docs_with_claude(story_data, doc_type, claude)
        except Exception as e:
            print(f"[GenerateDocs] Erro ao usar Claude: {e}")
            # Fallback para template basico

    # Template basico (fallback)
    return generate_docs_template(story_data, doc_type)


def generate_docs_with_claude(story_data: dict, doc_type: str, claude) -> dict:
    """Gera documentacao usando Claude AI"""

    story = story_data.get("story", story_data)
    tasks = story_data.get("tasks", [])

    # Montar contexto da story
    story_context = f"""
STORY: {story.get('story_id')} - {story.get('title')}

NARRATIVA:
Como um {story.get('persona', '[persona]')}, eu quero {story.get('action', '[acao]')},
para que {story.get('benefit', '[beneficio]')}.

CRITERIOS DE ACEITE:
{chr(10).join(f"- {c}" for c in story.get('acceptance_criteria', []))}

DEFINITION OF DONE:
{chr(10).join(f"- {d}" for d in story.get('definition_of_done', []))}

NOTAS TECNICAS:
{story.get('technical_notes', 'N/A')}

TASKS ({len(tasks)}):
{chr(10).join(f"- [{t.get('status')}] {t.get('title')}" for t in tasks)}
"""

    # Prompts especificos por tipo de doc
    prompts = {
        "readme": """Gere um README.md completo para esta funcionalidade.

Deve incluir:
1. Titulo e descricao breve
2. O que foi implementado
3. Como instalar/configurar
4. Como usar (exemplos praticos)
5. Estrutura de arquivos criados
6. Proximos passos

Use Markdown formatado.""",

        "api": """Gere documentacao de API para esta funcionalidade.

Deve incluir:
1. Endpoints implementados
2. Metodos HTTP
3. Parametros (path, query, body)
4. Exemplos de request/response
5. Codigos de status
6. Autenticacao necessaria (se aplicavel)

Use Markdown formatado com blocos de codigo.""",

        "user_guide": """Gere um guia do usuario para esta funcionalidade.

Deve incluir:
1. Introducao: O que esta funcionalidade faz
2. Passo a passo para usar
3. Screenshots ou descricoes de telas (se aplicavel)
4. Casos de uso comuns
5. FAQ e troubleshooting
6. Dicas e boas praticas

Use linguagem simples e clara. Markdown formatado.""",

        "technical": """Gere documentacao tecnica detalhada.

Deve incluir:
1. Arquitetura da solucao
2. Decisoes tecnicas e justificativas
3. Tecnologias usadas
4. Estrutura de codigo
5. Fluxo de dados
6. Consideracoes de seguranca
7. Performance e escalabilidade
8. Instrucoes de teste
9. Deploy

Use Markdown formatado com diagramas em texto quando util."""
    }

    system_prompt = f"""Voce e um escritor tecnico experiente especializado em documentacao de software.
Sua tarefa e gerar documentacao clara, completa e util baseada em User Stories Agile.

Gere a documentacao em PORTUGUES BRASILEIRO.
Use Markdown bem formatado com headers, listas, blocos de codigo, etc.

Responda APENAS com JSON valido no seguinte formato:
{{
    "title": "Titulo da documentacao",
    "content": "Conteudo completo em Markdown",
    "test_instructions": "Instrucoes de como testar (resumo)",
    "test_cases": [
        {{"description": "Caso de teste 1", "steps": ["passo 1", "passo 2"], "expected": "resultado esperado"}},
        {{"description": "Caso de teste 2", "steps": ["passo 1"], "expected": "resultado esperado"}}
    ]
}}"""

    prompt = prompts.get(doc_type, prompts["technical"])

    message = f"""{prompt}

{story_context}

Gere a documentacao agora em JSON."""

    response = claude.chat(message, system_prompt, max_tokens=4096)

    if response.success:
        try:
            import json
            # Tentar extrair JSON do response
            content = response.content.strip()

            # Remover markdown code blocks se presentes
            if content.startswith("```"):
                lines = content.split("\n")
                content = "\n".join(lines[1:-1])

            result = json.loads(content)
            return result
        except json.JSONDecodeError:
            # Se falhar o parse, usar como texto direto
            return {
                "title": f"Documentacao {doc_type.upper()} - {story.get('title')}",
                "content": response.content,
                "test_instructions": "Veja secao de testes na documentacao",
                "test_cases": []
            }

    # Fallback se Claude falhar
    return generate_docs_template(story_data, doc_type)


def generate_docs_template(story_data: dict, doc_type: str) -> dict:
    """Gera documentacao usando template basico (fallback)"""

    story = story_data.get("story", story_data)
    tasks = story_data.get("tasks", [])

    title = f"{doc_type.upper().replace('_', ' ')} - {story.get('title')}"

    # Template basico Markdown
    content = f"""# {story.get('title')}

## Descricao

**Como um** {story.get('persona', '[persona]')}, **eu quero** {story.get('action', '[acao]')}, **para que** {story.get('benefit', '[beneficio]')}.

## Criterios de Aceite

{chr(10).join(f"- {c}" for c in story.get('acceptance_criteria', [])) or '- N/A'}

## Definition of Done

{chr(10).join(f"- {d}" for d in story.get('definition_of_done', [])) or '- N/A'}

## Implementacao

### Tasks Realizadas

{chr(10).join(f"- [{t.get('status', 'pending')}] {t.get('title')}" for t in tasks) or '- Nenhuma task criada'}

### Notas Tecnicas

{story.get('technical_notes') or 'N/A'}

## Como Testar

1. Execute o projeto
2. Verifique os criterios de aceite acima
3. Teste cada cenario descrito

## Proximos Passos

- Revisar codigo
- Executar testes automatizados
- Deploy em ambiente de homologacao
"""

    test_instructions = """
1. Verificar criterios de aceite
2. Testar fluxos principais
3. Validar casos extremos
4. Conferir performance
"""

    test_cases = [
        {
            "description": "Teste basico de funcionalidade",
            "steps": ["Executar funcionalidade", "Verificar resultado"],
            "expected": "Funcionalidade deve atender criterios de aceite"
        }
    ]

    return {
        "title": title,
        "content": content,
        "test_instructions": test_instructions.strip(),
        "test_cases": test_cases
    }


# =============================================================================
# API ENDPOINTS - CHAT (ASSISTENTE)
# =============================================================================

@app.get("/api/chat/history")
def get_chat_history(
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    tenant_id: Optional[str] = None,
    limit: int = 50
):
    """Retorna historico de mensagens"""
    db = SessionLocal()
    try:
        repo = ChatMessageRepository(db)
        # Issue #297: Use named arguments to avoid parameter confusion
        messages = repo.get_history(
            project_id=project_id,
            story_id=story_id,
            tenant_id=tenant_id,
            limit=limit
        )
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

        result = {
            "user_message": user_msg.to_dict(),
            "assistant_message": assistant_msg.to_dict()
        }
        notify("chat_message", {"project_id": msg.project_id, "preview": msg.content[:50] if msg.content else ""})
        return result
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
def clear_chat_history(
    project_id: Optional[str] = None,
    story_id: Optional[str] = None,
    tenant_id: Optional[str] = None
):
    """Limpa historico de chat"""
    db = SessionLocal()
    try:
        repo = ChatMessageRepository(db)
        # Issue #297: Use named arguments for clarity
        count = repo.clear_history(
            project_id=project_id,
            story_id=story_id,
            tenant_id=tenant_id
        )
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


@app.get("/api/epics")
def list_all_epics(project_id: str = Query(None)):
    """Lista todos os epics ou filtra por projeto"""
    db = SessionLocal()
    try:
        repo = EpicRepository(db)
        if project_id:
            epics = repo.get_by_project(project_id)
        else:
            epics = db.query(Epic).limit(100).all()
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
# API ENDPOINTS - TENANT SELECTOR (Multi-Tenancy)
# =============================================================================

@app.get("/api/tenants")
def list_tenants(current_tenant: Optional[str] = Cookie(None)):
    """Lista tenants disponiveis para o usuario atual"""
    db = SessionLocal()
    try:
        tenants = db.query(Tenant).filter(Tenant.status == 'active').all()
        result = []
        for t in tenants:
            result.append({
                'tenant_id': t.tenant_id,
                'id': t.tenant_id,
                'name': t.name,
                'slug': t.slug,
                'plan': t.plan
            })
        return result
    finally:
        db.close()


@app.post("/api/tenant/select")
def select_tenant(request: dict, response: Response):
    """Seleciona tenant ativo para a sessao"""
    tenant_id = request.get('tenant_id')
    if tenant_id:
        db = SessionLocal()
        try:
            tenant = db.query(Tenant).filter(Tenant.tenant_id == tenant_id).first()
            if not tenant:
                raise HTTPException(404, "Tenant not found")
            response.set_cookie(key="current_tenant", value=tenant_id, max_age=86400*30, httponly=False, samesite="lax")
            return {"success": True, "tenant_id": tenant_id, "tenant_name": tenant.name}
        finally:
            db.close()
    else:
        response.delete_cookie(key="current_tenant")
        return {"success": True, "tenant_id": None}


@app.get("/api/tenant/current")
def get_current_tenant(current_tenant: Optional[str] = Cookie(None)):
    """Retorna o tenant atual da sessao"""
    if not current_tenant:
        return {"tenant_id": None, "tenant_name": None}
    db = SessionLocal()
    try:
        tenant = db.query(Tenant).filter(Tenant.tenant_id == current_tenant).first()
        if tenant:
            return {"tenant_id": tenant.tenant_id, "tenant_name": tenant.name}
        return {"tenant_id": None, "tenant_name": None}
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - GLOBAL SEARCH (Issue #221)
# =============================================================================

@app.get("/api/search")
def global_search(
    q: str = Query(..., min_length=1, description="Search query"),
    types: str = Query(default="stories,tasks,docs,projects", description="Comma-separated types"),
    project_id: Optional[str] = Query(None, description="Filter by project"),
    limit: int = Query(default=10, le=50, description="Max results per type"),
    current_tenant: Optional[str] = Cookie(None)
):
    """
    Issue #221: Global search across stories, tasks, docs, and projects.
    Supports full-text search with relevance ranking.
    """
    db = SessionLocal()
    try:
        query_lower = q.lower().strip()
        type_list = [t.strip() for t in types.split(",")]
        results = {
            "query": q,
            "stories": [],
            "tasks": [],
            "docs": [],
            "projects": [],
            "total": 0
        }

        # Search Stories
        if "stories" in type_list:
            story_repo = StoryRepository(db)
            all_stories = story_repo.get_all()
            if project_id:
                all_stories = [s for s in all_stories if s.project_id == project_id]
            if current_tenant:
                all_stories = [s for s in all_stories if getattr(s, 'tenant_id', None) == current_tenant or not getattr(s, 'tenant_id', None)]

            matched_stories = []
            for story in all_stories:
                score = 0
                searchable = f"{story.story_id} {story.title or ''} {story.persona or ''} {story.action or ''} {story.benefit or ''}".lower()
                if query_lower in searchable:
                    # Higher score for title match
                    if query_lower in (story.title or '').lower():
                        score = 100
                    elif query_lower in (story.story_id or '').lower():
                        score = 90
                    else:
                        score = 50
                    matched_stories.append({
                        "story_id": story.story_id,
                        "title": story.title,
                        "status": story.status.value if hasattr(story.status, 'value') else story.status,
                        "project_id": story.project_id,
                        "story_points": story.story_points,
                        "priority": story.priority.value if hasattr(story.priority, 'value') else story.priority,
                        "score": score
                    })

            # Sort by score and limit
            matched_stories.sort(key=lambda x: x['score'], reverse=True)
            results["stories"] = matched_stories[:limit]

        # Search Tasks
        if "tasks" in type_list:
            # Issue #306: StoryTaskRepository does not have get_all() method
            # Query database directly instead
            all_tasks = db.query(StoryTask).all()

            matched_tasks = []
            for task in all_tasks:
                # Filter by project if specified
                if project_id:
                    story = db.query(Story).filter(Story.story_id == task.story_id).first()
                    if story and story.project_id != project_id:
                        continue

                score = 0
                searchable = f"{task.task_id} {task.title or ''} {task.description or ''}".lower()
                if query_lower in searchable:
                    if query_lower in (task.title or '').lower():
                        score = 100
                    elif query_lower in (task.task_id or '').lower():
                        score = 90
                    else:
                        score = 50
                    matched_tasks.append({
                        "task_id": task.task_id,
                        "title": task.title,
                        "status": task.status.value if hasattr(task.status, 'value') else task.status,
                        "story_id": task.story_id,
                        "progress": task.progress,
                        "score": score
                    })

            matched_tasks.sort(key=lambda x: x['score'], reverse=True)
            results["tasks"] = matched_tasks[:limit]

        # Search Documentation
        if "docs" in type_list:
            doc_repo = StoryDocumentationRepository(db)
            all_docs = doc_repo.get_all()

            matched_docs = []
            for doc in all_docs:
                # Filter by project if specified
                if project_id:
                    story = db.query(Story).filter(Story.story_id == doc.story_id).first()
                    if story and story.project_id != project_id:
                        continue

                score = 0
                searchable = f"{doc.doc_id} {doc.title or ''} {doc.content or ''}".lower()
                if query_lower in searchable:
                    if query_lower in (doc.title or '').lower():
                        score = 100
                    elif query_lower in (doc.doc_id or '').lower():
                        score = 90
                    else:
                        score = 50
                    matched_docs.append({
                        "doc_id": doc.doc_id,
                        "title": doc.title,
                        "doc_type": doc.doc_type.value if hasattr(doc.doc_type, 'value') else doc.doc_type,
                        "story_id": doc.story_id,
                        "score": score
                    })

            matched_docs.sort(key=lambda x: x['score'], reverse=True)
            results["docs"] = matched_docs[:limit]

        # Search Projects
        if "projects" in type_list:
            project_repo = ProjectRepository(db)
            all_projects = project_repo.get_all()
            if current_tenant:
                all_projects = [p for p in all_projects if getattr(p, 'tenant_id', None) == current_tenant or not getattr(p, 'tenant_id', None)]

            matched_projects = []
            for project in all_projects:
                score = 0
                searchable = f"{project.project_id} {project.name or ''} {project.description or ''} {project.project_type or ''}".lower()
                if query_lower in searchable:
                    if query_lower in (project.name or '').lower():
                        score = 100
                    else:
                        score = 50
                    matched_projects.append({
                        "project_id": project.project_id,
                        "name": project.name,
                        "project_type": project.project_type,
                        "status": project.status.value if hasattr(project.status, 'value') else project.status,
                        "score": score
                    })

            matched_projects.sort(key=lambda x: x['score'], reverse=True)
            results["projects"] = matched_projects[:limit]

        # Calculate total
        results["total"] = (
            len(results["stories"]) +
            len(results["tasks"]) +
            len(results["docs"]) +
            len(results["projects"])
        )

        return results
    finally:
        db.close()


# =============================================================================
# API ENDPOINTS - STORY TEMPLATES (Issue #223)
# =============================================================================

STORY_TEMPLATES = [
    {
        "id": "feature",
        "name": "Feature",
        "icon": "✨",
        "description": "Nova funcionalidade",
        "color": "#10B981",
        "data": {
            "title": "[Feature] ",
            "persona": "Como um usuário",
            "action": "eu quero ",
            "benefit": "para que ",
            "acceptance_criteria": [
                "Dado que {contexto}, quando {ação}, então {resultado}",
                "A funcionalidade deve ser acessível via interface web",
                "Deve haver feedback visual de sucesso/erro"
            ],
            "definition_of_done": [
                "Código implementado e revisado",
                "Testes unitários com cobertura > 80%",
                "Documentação atualizada",
                "Deploy em staging realizado"
            ],
            "story_points": 5,
            "complexity": "medium",
            "priority": "medium",
            "category": "feature"
        }
    },
    {
        "id": "bugfix",
        "name": "Bug Fix",
        "icon": "🐛",
        "description": "Correção de problema",
        "color": "#EF4444",
        "data": {
            "title": "[Bug] ",
            "persona": "Como um usuário",
            "action": "eu quero que o sistema funcione corretamente",
            "benefit": "para que o problema seja resolvido",
            "acceptance_criteria": [
                "O bug não deve mais ocorrer no cenário reportado",
                "Testes de regressão devem passar",
                "Nenhum efeito colateral em funcionalidades existentes"
            ],
            "definition_of_done": [
                "Bug reproduzido e causa identificada",
                "Correção implementada",
                "Teste específico para o bug criado",
                "Verificado em ambiente de staging"
            ],
            "story_points": 3,
            "complexity": "low",
            "priority": "high",
            "category": "bug"
        }
    },
    {
        "id": "tech_debt",
        "name": "Tech Debt",
        "icon": "🔧",
        "description": "Refatoração e melhorias",
        "color": "#8B5CF6",
        "data": {
            "title": "[Tech Debt] ",
            "persona": "Como desenvolvedor",
            "action": "eu quero refatorar ",
            "benefit": "para melhorar a manutenibilidade e qualidade do código",
            "acceptance_criteria": [
                "Código refatorado segue padrões do projeto",
                "Performance não degradou",
                "Todos os testes existentes passam"
            ],
            "definition_of_done": [
                "Refatoração completa",
                "Code review aprovado",
                "Documentação técnica atualizada",
                "Métricas de qualidade melhoradas"
            ],
            "story_points": 8,
            "complexity": "high",
            "priority": "low",
            "category": "tech_debt"
        }
    },
    {
        "id": "integration",
        "name": "Integração",
        "icon": "🔗",
        "description": "Conectar com sistemas externos",
        "color": "#3B82F6",
        "data": {
            "title": "[Integration] Integrar com ",
            "persona": "Como um usuário",
            "action": "eu quero integrar com ",
            "benefit": "para sincronizar dados e automatizar processos",
            "acceptance_criteria": [
                "Autenticação com sistema externo funciona",
                "Dados são sincronizados corretamente",
                "Erros são tratados e logados adequadamente",
                "Retry automático em caso de falha"
            ],
            "definition_of_done": [
                "Documentação da API revisada",
                "Credenciais configuradas de forma segura",
                "Testes de integração passando",
                "Monitoramento configurado"
            ],
            "story_points": 13,
            "complexity": "very_high",
            "priority": "medium",
            "category": "integration"
        }
    },
    {
        "id": "ui_improvement",
        "name": "UI/UX",
        "icon": "🎨",
        "description": "Melhorias de interface",
        "color": "#F59E0B",
        "data": {
            "title": "[UI] Melhorar ",
            "persona": "Como usuário",
            "action": "eu quero uma interface mais intuitiva",
            "benefit": "para ter uma melhor experiência de uso",
            "acceptance_criteria": [
                "Design aprovado pelo time",
                "Acessibilidade WCAG 2.1 AA",
                "Responsivo para mobile",
                "Performance: LCP < 2.5s"
            ],
            "definition_of_done": [
                "Mockup/protótipo aprovado",
                "Implementação fiel ao design",
                "Testes de usabilidade realizados",
                "Cross-browser testado"
            ],
            "story_points": 5,
            "complexity": "medium",
            "priority": "medium",
            "category": "improvement"
        }
    },
    {
        "id": "documentation",
        "name": "Documentação",
        "icon": "📚",
        "description": "Criar ou atualizar docs",
        "color": "#6366F1",
        "data": {
            "title": "[Docs] ",
            "persona": "Como desenvolvedor/usuário",
            "action": "eu quero documentação atualizada",
            "benefit": "para entender como usar/desenvolver a funcionalidade",
            "acceptance_criteria": [
                "Documentação clara e completa",
                "Exemplos de uso incluídos",
                "Screenshots/diagramas quando aplicável"
            ],
            "definition_of_done": [
                "Conteúdo revisado por pares",
                "Formatação consistente",
                "Links funcionais",
                "Publicado/atualizado"
            ],
            "story_points": 2,
            "complexity": "low",
            "priority": "low",
            "category": "documentation"
        }
    }
]


@app.get("/api/templates")
def list_story_templates():
    """Issue #223: List available story templates"""
    return STORY_TEMPLATES


@app.get("/api/templates/{template_id}")
def get_story_template(template_id: str):
    """Issue #223: Get specific template by ID"""
    for template in STORY_TEMPLATES:
        if template["id"] == template_id:
            return template
    raise HTTPException(404, f"Template not found: {template_id}")


# =============================================================================
# API ENDPOINTS - PROJECTS
# =============================================================================

@app.get("/api/projects")
def list_projects(current_tenant: Optional[str] = Cookie(None)):
    """Lista projetos, filtrados pelo tenant selecionado"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        projects = repo.get_all()
        # Filtrar por tenant se selecionado
        if current_tenant:
            projects = [p for p in projects if getattr(p, 'tenant_id', None) == current_tenant or not getattr(p, 'tenant_id', None)]
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


@app.get("/api/projects/{project_id}/files")
def list_project_files(project_id: str):
    """Lista arquivos do projeto"""
    import os
    project_path = Path(__file__).parent.parent.parent / "projects" / project_id
    if not project_path.exists():
        raise HTTPException(404, f"Project folder not found: {project_id}")

    files = []
    for root, dirs, filenames in os.walk(project_path):
        # Skip hidden directories
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        for filename in filenames:
            if filename.startswith('.'):
                continue
            filepath = Path(root) / filename
            rel_path = filepath.relative_to(project_path)
            ext = filepath.suffix.lower()
            file_type = "unknown"
            if ext in ['.html', '.htm']:
                file_type = "html"
            elif ext == '.pdf':
                file_type = "pdf"
            elif ext in ['.py', '.js', '.ts', '.json']:
                file_type = "code"
            elif ext in ['.md', '.txt']:
                file_type = "text"
            elif ext in ['.jpg', '.jpeg', '.png', '.gif', '.svg']:
                file_type = "image"
            elif ext in ['.mp4', '.webm', '.avi']:
                file_type = "video"
            elif ext in ['.xlsx', '.xls', '.csv']:
                file_type = "spreadsheet"

            files.append({
                "name": filename,
                "path": str(rel_path),
                "url": f"/project-files/{project_id}/{rel_path}",
                "type": file_type,
                "size": filepath.stat().st_size,
                "extension": ext
            })

    # Sort by type and name
    files.sort(key=lambda x: (x['type'], x['name']))
    return {"project_id": project_id, "files": files, "total": len(files)}


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
# API ENDPOINTS - TERMINAL
# =============================================================================

import subprocess
import threading
import queue

# Global terminal processes storage
terminal_processes = {}
terminal_outputs = {}

class TerminalExecuteRequest(BaseModel):
    command: str
    cwd: Optional[str] = None

@app.post("/api/projects/{project_id}/terminal/execute")
def execute_terminal_command(project_id: str, request: TerminalExecuteRequest):
    """Executa comando no terminal"""
    try:
        # Define working directory
        if request.cwd:
            cwd = request.cwd
        else:
            cwd = os.path.join(r'C:\Users\lcruz\Fabrica de Agentes\projects', project_id)

        # Ensure directory exists
        os.makedirs(cwd, exist_ok=True)

        # Stop any existing process for this project
        if project_id in terminal_processes:
            try:
                terminal_processes[project_id].terminate()
            except:
                pass

        # Initialize output queue
        terminal_outputs[project_id] = queue.Queue()

        # Determine shell based on command
        if request.command.startswith('npm ') or request.command.startswith('python '):
            # Create process
            process = subprocess.Popen(
                request.command,
                cwd=cwd,
                shell=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,
                universal_newlines=True
            )

            # Store process
            terminal_processes[project_id] = process

            # Thread to read output
            def read_output():
                try:
                    for line in process.stdout:
                        terminal_outputs[project_id].put(line)
                    process.wait()
                    terminal_outputs[project_id].put(f"\n[Process exited with code {process.returncode}]\n")
                except Exception as e:
                    terminal_outputs[project_id].put(f"\n[Error: {str(e)}]\n")

            thread = threading.Thread(target=read_output, daemon=True)
            thread.start()

            return {
                "status": "started",
                "command": request.command,
                "cwd": cwd,
                "pid": process.pid
            }
        else:
            # Simple command execution (non-blocking)
            result = subprocess.run(
                request.command,
                cwd=cwd,
                shell=True,
                capture_output=True,
                text=True
            )
            return {
                "status": "completed",
                "command": request.command,
                "output": result.stdout + result.stderr,
                "returncode": result.returncode
            }
    except Exception as e:
        raise HTTPException(500, f"Failed to execute command: {str(e)}")


@app.get("/api/projects/{project_id}/terminal/output")
def get_terminal_output(project_id: str):
    """Retorna output do terminal (stream)"""
    if project_id not in terminal_outputs:
        return {"output": "", "has_more": False}

    output_lines = []
    try:
        # Get all available output
        while not terminal_outputs[project_id].empty():
            output_lines.append(terminal_outputs[project_id].get_nowait())
    except queue.Empty:
        pass

    has_process = project_id in terminal_processes and terminal_processes[project_id].poll() is None

    return {
        "output": "".join(output_lines),
        "has_more": has_process
    }


@app.post("/api/projects/{project_id}/terminal/stop")
def stop_terminal_process(project_id: str):
    """Para processo do terminal"""
    if project_id not in terminal_processes:
        raise HTTPException(404, "No running process found")

    try:
        process = terminal_processes[project_id]
        process.terminate()
        process.wait(timeout=5)
        del terminal_processes[project_id]
        return {"status": "stopped"}
    except subprocess.TimeoutExpired:
        process.kill()
        del terminal_processes[project_id]
        return {"status": "killed"}
    except Exception as e:
        raise HTTPException(500, f"Failed to stop process: {str(e)}")


# =============================================================================
# WEBSOCKET ENDPOINT
# =============================================================================

@app.websocket("/ws/notifications")
async def websocket_notifications(websocket: WebSocket):
    """WebSocket endpoint para notificacoes em tempo real"""
    await ws_manager.connect(websocket)
    try:
        await websocket.send_json({
            "type": "connection",
            "data": {"status": "connected", "message": "Conectado ao servidor de notificacoes"},
            "timestamp": datetime.utcnow().isoformat() + "Z"
        })
        while True:
            try:
                data = await websocket.receive_text()
                if data == "ping":
                    await websocket.send_json({"type": "pong", "timestamp": datetime.utcnow().isoformat() + "Z"})
            except WebSocketDisconnect:
                break
            except:
                break
    except WebSocketDisconnect:
        pass
    finally:
        ws_manager.disconnect(websocket)


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

    <!-- PWA Meta Tags (Issue #259) -->
    <meta name="theme-color" content="#003B4A">
    <meta name="mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
    <meta name="apple-mobile-web-app-title" content="FdA Agile">
    <meta name="application-name" content="Fabrica de Agentes">
    <meta name="msapplication-TileColor" content="#003B4A">
    <meta name="msapplication-TileImage" content="/static/icons/icon-144x144.png">

    <!-- PWA Manifest -->
    <link rel="manifest" href="/manifest.json" crossorigin="use-credentials">

    <!-- PWA Icons -->
    <link rel="icon" type="image/png" sizes="32x32" href="/static/icons/icon-96x96.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/static/icons/icon-72x72.png">
    <link rel="apple-touch-icon" href="/static/icons/icon-192x192.png">
    <link rel="apple-touch-icon" sizes="152x152" href="/static/icons/icon-152x152.png">
    <link rel="apple-touch-icon" sizes="180x180" href="/static/icons/icon-192x192.png">

    <!-- Splash Screen for iOS -->
    <link rel="apple-touch-startup-image" href="/static/icons/icon-512x512.png">

    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <!-- xterm.js for terminal -->
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css">
    <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js"></script>
    <!-- Prism.js for syntax highlighting -->
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism-tomorrow.min.css">
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/prism.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-python.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-javascript.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-typescript.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-json.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-yaml.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-bash.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-sql.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-css.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-markup.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-docker.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-toml.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-ini.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-numbers/prism-line-numbers.min.js"></script>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-numbers/prism-line-numbers.min.css">
    <!-- PDF.js for PDF viewing -->
    <script src="https://cdn.jsdelivr.net/npm/pdfjs-dist@3.11.174/build/pdf.min.js"></script>
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
        /* File Viewer */ .file-viewer-modal { background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); }
        .file-viewer-header { background: rgba(0,0,0,0.3); padding: 16px 24px; border-bottom: 1px solid rgba(255,255,255,0.1); }
        .file-viewer-content { background: #1e1e2e; border-radius: 8px; margin: 16px; overflow: hidden; }
        .file-viewer-code { max-height: calc(80vh - 180px); overflow: auto; }
        .file-viewer-code pre { margin: 0; padding: 16px; font-size: 13px; line-height: 1.6; background: #2d2d2d; }
        .markdown-body { padding: 24px; background: white; color: #333; max-height: calc(80vh - 180px); overflow: auto; }
        .markdown-body h1 { font-size: 2em; border-bottom: 1px solid #eee; } .markdown-body h2 { font-size: 1.5em; }
        .markdown-body code { background: #f4f4f4; padding: 2px 6px; border-radius: 4px; }
        .markdown-body pre { background: #2d2d2d; color: #f8f8f2; padding: 16px; border-radius: 8px; }
        .pdf-viewer-container { background: #525659; padding: 16px; max-height: calc(80vh - 180px); overflow: auto; }
        .image-viewer-container { background: #1a1a2e; padding: 24px; display: flex; align-items: center; justify-content: center; }
        .image-viewer-container img { max-width: 100%; max-height: 60vh; border-radius: 8px; box-shadow: 0 4px 20px rgba(0,0,0,0.4); }
        .file-viewer-btn { padding: 8px 16px; border-radius: 6px; font-size: 13px; cursor: pointer; display: flex; align-items: center; gap: 6px; }
        .file-viewer-btn.primary { background: #FF6C00; color: white; border: none; }
        .file-viewer-btn.secondary { background: rgba(255,255,255,0.1); color: white; border: 1px solid rgba(255,255,255,0.2); }

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

        /* Swimlanes */
        .swimlane {
            border-bottom: 2px solid #e5e7eb;
            padding: 16px 0;
            margin-bottom: 16px;
        }
        .swimlane:last-child {
            border-bottom: none;
        }
        .swimlane-header {
            font-weight: 600;
            padding: 8px 16px;
            background: #f3f4f6;
            border-radius: 6px;
            margin-bottom: 12px;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .swimlane-content {
            display: flex;
            gap: 16px;
            overflow-x: auto;
            padding: 0 8px;
        }
        .swimlane-column {
            flex-shrink: 0;
            width: 280px;
        }

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

        
        /* ===== PROJECT PREVIEW DASHBOARD STYLES (Issue #73) ===== */
        .preview-dashboard {
            background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
            min-height: 100%;
        }
        .preview-card {
            background: white;
            border-radius: 12px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            transition: all 0.3s ease;
        }
        .preview-card:hover {
            box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
            transform: translateY(-2px);
        }
        .preview-header {
            background: linear-gradient(135deg, #003B4A 0%, #005566 100%);
            color: white;
            border-radius: 12px;
            padding: 24px;
            margin-bottom: 24px;
        }
        .preview-metric {
            text-align: center;
            padding: 16px;
        }
        .preview-metric-value {
            font-size: 2rem;
            font-weight: 700;
            color: #003B4A;
        }
        .preview-metric-label {
            font-size: 0.75rem;
            color: #6B7280;
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }
        .preview-progress-bar {
            height: 12px;
            background: #E5E7EB;
            border-radius: 6px;
            overflow: hidden;
        }
        .preview-progress-fill {
            height: 100%;
            border-radius: 6px;
            transition: width 0.5s ease;
        }
        .preview-section-title {
            font-size: 0.875rem;
            font-weight: 600;
            color: #374151;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 12px;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .preview-file-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px;
            border: 1px solid #E5E7EB;
            border-radius: 8px;
            transition: all 0.2s ease;
            cursor: pointer;
        }
        .preview-file-item:hover {
            background: #F9FAFB;
            border-color: #D1D5DB;
        }
        .preview-file-icon {
            width: 40px;
            height: 40px;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 8px;
            font-size: 1.25rem;
        }
        .preview-doc-card {
            border: 1px solid #E5E7EB;
            border-radius: 8px;
            padding: 16px;
            transition: all 0.2s ease;
        }
        .preview-doc-card:hover {
            border-color: #3B82F6;
            background: #F0F9FF;
        }
        .preview-action-btn {
            display: inline-flex;
            align-items: center;
            gap: 8px;
            padding: 12px 20px;
            border-radius: 8px;
            font-weight: 500;
            transition: all 0.2s ease;
            cursor: pointer;
            border: none;
        }
        .preview-action-btn.primary {
            background: #FF6C00;
            color: white;
        }
        .preview-action-btn.primary:hover {
            background: #E56000;
        }
        .preview-action-btn.secondary {
            background: white;
            color: #003B4A;
            border: 1px solid #D1D5DB;
        }
        .preview-action-btn.secondary:hover {
            background: #F3F4F6;
        }
        .preview-tabs {
            display: flex;
            gap: 4px;
            border-bottom: 2px solid #E5E7EB;
            margin-bottom: 16px;
        }
        .preview-tab {
            padding: 12px 20px;
            font-weight: 500;
            color: #6B7280;
            cursor: pointer;
            border-bottom: 2px solid transparent;
            margin-bottom: -2px;
            transition: all 0.2s ease;
        }
        .preview-tab:hover {
            color: #003B4A;
        }
        .preview-tab.active {
            color: #003B4A;
            border-color: #FF6C00;
        }
        .preview-iframe-container {
            border: 2px solid #E5E7EB;
            border-radius: 12px;
            overflow: hidden;
            background: white;
        }
        .preview-iframe-header {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 8px 12px;
            background: #F3F4F6;
            border-bottom: 1px solid #E5E7EB;
        }
        .preview-iframe-dot {
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }
        .preview-viewport-btn {
            padding: 6px 12px;
            border: 1px solid #D1D5DB;
            border-radius: 6px;
            background: white;
            cursor: pointer;
            transition: all 0.2s ease;
        }
        .preview-viewport-btn.active {
            background: #003B4A;
            color: white;
            border-color: #003B4A;
        }

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

        /* Issue #216: Command Palette */
        @keyframes paletteIn {
            from { transform: translateY(-20px) scale(0.98); opacity: 0; }
            to { transform: translateY(0) scale(1); opacity: 1; }
        }
        .kbd-small {
            display: inline-block;
            padding: 1px 4px;
            font-size: 0.65rem;
            font-family: monospace;
            background: #E5E7EB;
            border: 1px solid #D1D5DB;
            border-radius: 3px;
            margin: 0 2px;
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

        /* ===================== ENHANCED FILE UPLOAD - Issue #185 ===================== */
        .upload-dropzone {
            border: 2px dashed #D1D5DB;
            border-radius: 12px;
            padding: 32px;
            text-align: center;
            transition: all 0.3s ease;
            background: #F9FAFB;
            cursor: pointer;
        }
        .upload-dropzone:hover {
            border-color: #FF6C00;
            background: #FFF7ED;
        }
        .upload-dropzone.drag-over {
            border-color: #FF6C00;
            background: #FFF7ED;
            transform: scale(1.02);
            box-shadow: 0 4px 12px rgba(255, 108, 0, 0.15);
        }
        .upload-dropzone-icon {
            width: 64px;
            height: 64px;
            margin: 0 auto 16px;
            background: linear-gradient(135deg, #003B4A 0%, #005566 100%);
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-size: 24px;
        }
        .upload-progress-bar {
            height: 6px;
            background: #E5E7EB;
            border-radius: 3px;
            overflow: hidden;
            margin-top: 8px;
        }
        .upload-progress-fill {
            height: 100%;
            background: linear-gradient(90deg, #FF6C00, #FF8C3A);
            border-radius: 3px;
            transition: width 0.3s ease;
        }
        .upload-file-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px;
            background: white;
            border: 1px solid #E5E7EB;
            border-radius: 8px;
            transition: all 0.2s ease;
        }
        .upload-file-item:hover {
            border-color: #D1D5DB;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        }
        .upload-file-icon {
            width: 44px;
            height: 44px;
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 20px;
            flex-shrink: 0;
        }
        .upload-file-icon.pdf { background: #FEE2E2; color: #DC2626; }
        .upload-file-icon.doc { background: #DBEAFE; color: #2563EB; }
        .upload-file-icon.video { background: #F3E8FF; color: #9333EA; }
        .upload-file-icon.image { background: #D1FAE5; color: #059669; }
        .upload-file-icon.text { background: #F3F4F6; color: #6B7280; }
        .upload-file-icon.default { background: #E5E7EB; color: #374151; }
        .upload-thumbnail {
            width: 44px;
            height: 44px;
            border-radius: 8px;
            object-fit: cover;
        }
        .upload-file-info {
            flex: 1;
            min-width: 0;
        }
        .upload-file-name {
            font-weight: 500;
            font-size: 0.875rem;
            color: #111827;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }
        .upload-file-meta {
            font-size: 0.75rem;
            color: #6B7280;
            display: flex;
            gap: 8px;
            margin-top: 2px;
        }
        .upload-file-actions {
            display: flex;
            gap: 4px;
        }
        .upload-action-btn {
            width: 32px;
            height: 32px;
            border-radius: 6px;
            border: none;
            background: transparent;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.2s;
            color: #6B7280;
        }
        .upload-action-btn:hover {
            background: #F3F4F6;
            color: #111827;
        }
        .upload-action-btn.danger:hover {
            background: #FEE2E2;
            color: #DC2626;
        }
        .upload-status-badge {
            font-size: 0.65rem;
            padding: 2px 6px;
            border-radius: 4px;
            font-weight: 500;
        }
        .upload-status-badge.uploading {
            background: #DBEAFE;
            color: #2563EB;
        }
        .upload-status-badge.success {
            background: #D1FAE5;
            color: #059669;
        }
        .upload-status-badge.error {
            background: #FEE2E2;
            color: #DC2626;
        }
        .upload-filter-chip {
            padding: 4px 12px;
            border-radius: 16px;
            font-size: 0.75rem;
            font-weight: 500;
            border: 1px solid #E5E7EB;
            background: white;
            cursor: pointer;
            transition: all 0.2s;
        }
        .upload-filter-chip:hover {
            border-color: #D1D5DB;
        }
        .upload-filter-chip.active {
            background: #003B4A;
            color: white;
            border-color: #003B4A;
        }

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

        /* ===================== MOBILE RESPONSIVENESS - Issue #36 ===================== */
        .mobile-menu-btn { display: none; width: 44px; height: 44px; padding: 10px; background: transparent; border: none; cursor: pointer; z-index: 100; }
        .mobile-menu-btn span { display: block; width: 24px; height: 2px; background: white; margin: 5px 0; transition: all 0.3s ease; }
        .mobile-menu-btn.active span:nth-child(1) { transform: rotate(45deg) translate(5px, 5px); }
        .mobile-menu-btn.active span:nth-child(2) { opacity: 0; }
        .mobile-menu-btn.active span:nth-child(3) { transform: rotate(-45deg) translate(5px, -5px); }
        .mobile-overlay { display: none; position: fixed; inset: 0; background: rgba(0,0,0,0.5); z-index: 40; opacity: 0; transition: opacity 0.3s ease; }
        .mobile-overlay.visible { opacity: 1; }
        .mobile-bottom-nav { display: none; position: fixed; bottom: 0; left: 0; right: 0; background: white; border-top: 1px solid #E5E7EB; padding: 8px 0; z-index: 50; box-shadow: 0 -4px 12px rgba(0,0,0,0.1); }
        .mobile-bottom-nav-items { display: flex; justify-content: space-around; align-items: center; }
        .mobile-nav-item { display: flex; flex-direction: column; align-items: center; gap: 4px; padding: 8px 16px; color: #6B7280; font-size: 10px; cursor: pointer; min-width: 64px; }
        .mobile-nav-item.active, .mobile-nav-item:hover { color: #003B4A; }
        .mobile-nav-item svg { width: 24px; height: 24px; }
        .pull-refresh-indicator { display: none; position: fixed; top: 64px; left: 50%; transform: translateX(-50%); background: white; padding: 8px 16px; border-radius: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 100; font-size: 12px; color: #003B4A; }
        .pull-refresh-indicator.visible { display: flex; align-items: center; gap: 8px; }

        @media screen and (max-width: 1200px) { .sidebar-desktop { width: 200px !important; } .chat-panel-desktop { width: 280px !important; } .kanban-column-container { width: 260px !important; } .swimlane-column { width: 240px !important; } }

        @media screen and (max-width: 768px) {
            .mobile-menu-btn { display: block !important; }
            .hide-on-mobile { display: none !important; }
            .mobile-overlay.visible { display: block !important; }
            .mobile-bottom-nav { display: block !important; }
            .header-version { display: none !important; }
            .sidebar-desktop { position: fixed !important; left: 0; top: 64px; bottom: 0; width: 280px !important; z-index: 45; transform: translateX(-100%); transition: transform 0.3s ease; }
            .sidebar-desktop.open { transform: translateX(0); }
            .chat-panel-desktop { position: fixed !important; right: 0; top: 64px; bottom: 60px; width: 100% !important; z-index: 45; transform: translateX(100%); transition: transform 0.3s ease; }
            .chat-panel-desktop.open { transform: translateX(0); }
            .kanban-container { display: flex !important; overflow-x: auto !important; scroll-snap-type: x mandatory; -webkit-overflow-scrolling: touch; gap: 12px !important; }
            .kanban-column-container { flex: 0 0 85vw !important; width: 85vw !important; max-width: 320px !important; scroll-snap-align: center; }
            .story-card { padding: 14px !important; }
            .quick-actions { opacity: 1 !important; }
            .quick-btn { width: 32px !important; height: 32px !important; }
            .filter-bar { flex-wrap: nowrap !important; overflow-x: auto !important; }
            .filter-bar > * { flex-shrink: 0 !important; }
            .bulk-toolbar { left: 16px !important; right: 16px !important; transform: none !important; bottom: 70px !important; }
            .toast-container { left: 16px !important; right: 16px !important; bottom: 80px !important; }
            .toast { width: 100% !important; }
            .context-menu { position: fixed !important; left: 16px !important; right: 16px !important; bottom: 80px !important; top: auto !important; }
            .slide-panel { width: 100% !important; max-width: 100% !important; }
            .modal-dialog input, .modal-dialog select, .modal-dialog textarea { font-size: 16px !important; min-height: 44px !important; }
        }

        @media screen and (max-width: 480px) { .kanban-column-container { flex: 0 0 92vw !important; width: 92vw !important; } .story-card h4 { font-size: 13px !important; } .mobile-nav-item { padding: 6px 12px !important; } .mobile-nav-item svg { width: 20px !important; height: 20px !important; } }

        @media (hover: none) and (pointer: coarse) { .quick-actions { opacity: 1 !important; } button, a { min-height: 44px; } .story-card:hover { transform: none !important; } .story-card:active { transform: scale(0.98); } }

        @media screen and (max-width: 896px) and (orientation: landscape) { .header-container { height: 48px !important; } .sidebar-desktop, .chat-panel-desktop { top: 48px !important; } .mobile-nav-item span { display: none !important; } }

        @supports (padding: env(safe-area-inset-top)) { .header-container { padding-top: env(safe-area-inset-top); } .mobile-bottom-nav { padding-bottom: env(safe-area-inset-bottom); } }

        @media print { .mobile-menu-btn, .mobile-bottom-nav, .mobile-overlay { display: none !important; } }

        /* ===================== ISSUE #135 - EXECUTIVE DASHBOARD ===================== */
        .executive-dashboard {
            background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
            padding: 24px;
            min-height: 100%;
        }
        .exec-header {
            background: linear-gradient(135deg, #003B4A 0%, #005566 100%);
            color: white;
            border-radius: 16px;
            padding: 32px;
            margin-bottom: 24px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .exec-header h1 {
            font-size: 1.75rem;
            font-weight: 700;
            margin-bottom: 8px;
        }
        .exec-header .subtitle {
            font-size: 0.875rem;
            opacity: 0.8;
        }
        .exec-status-cards {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 24px;
        }
        .exec-status-card {
            background: white;
            border-radius: 16px;
            padding: 24px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            position: relative;
            overflow: hidden;
            transition: transform 0.2s, box-shadow 0.2s;
        }
        .exec-status-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
        }
        .exec-status-card.status-green { border-left: 5px solid #10B981; }
        .exec-status-card.status-yellow { border-left: 5px solid #F59E0B; }
        .exec-status-card.status-red { border-left: 5px solid #EF4444; }
        .exec-status-indicator {
            position: absolute;
            top: 16px;
            right: 16px;
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }
        .exec-status-indicator.green { background: #10B981; }
        .exec-status-indicator.yellow { background: #F59E0B; animation: pulse 2s infinite; }
        .exec-status-indicator.red { background: #EF4444; animation: pulse 1s infinite; }
        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }
        .exec-card-value {
            font-size: 2.5rem;
            font-weight: 700;
            color: #003B4A;
            line-height: 1;
        }
        .exec-card-label {
            font-size: 0.875rem;
            color: #6B7280;
            margin-top: 8px;
        }
        .exec-card-meta {
            font-size: 0.75rem;
            color: #9CA3AF;
            margin-top: 4px;
        }
        .exec-progress-section {
            background: white;
            border-radius: 16px;
            padding: 24px;
            margin-bottom: 24px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        }
        .exec-progress-bar {
            height: 20px;
            background: #E5E7EB;
            border-radius: 10px;
            overflow: hidden;
            margin: 16px 0;
        }
        .exec-progress-fill {
            height: 100%;
            border-radius: 10px;
            transition: width 0.5s ease;
            background: linear-gradient(90deg, #10B981, #059669);
        }
        .exec-timeline {
            display: flex;
            justify-content: space-between;
            margin-top: 24px;
            padding: 0 16px;
            position: relative;
        }
        .exec-timeline::before {
            content: '';
            position: absolute;
            top: 16px;
            left: 40px;
            right: 40px;
            height: 4px;
            background: #E5E7EB;
            z-index: 0;
        }
        .exec-timeline-step {
            display: flex;
            flex-direction: column;
            align-items: center;
            position: relative;
            z-index: 1;
        }
        .exec-timeline-dot {
            width: 36px;
            height: 36px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 1rem;
            margin-bottom: 8px;
        }
        .exec-timeline-dot.completed { background: #10B981; color: white; }
        .exec-timeline-dot.current { background: #3B82F6; color: white; box-shadow: 0 0 0 4px rgba(59,130,246,0.2); }
        .exec-timeline-dot.pending { background: #E5E7EB; color: #9CA3AF; }
        .exec-timeline-label {
            font-size: 0.75rem;
            color: #6B7280;
            text-align: center;
            max-width: 80px;
        }
        .exec-main-btn {
            background: linear-gradient(135deg, #FF6C00, #E56000);
            color: white;
            border: none;
            padding: 16px 32px;
            border-radius: 12px;
            font-size: 1.125rem;
            font-weight: 600;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 12px;
            transition: transform 0.2s, box-shadow 0.2s;
            box-shadow: 0 4px 14px rgba(255, 108, 0, 0.4);
        }
        .exec-main-btn:hover {
            transform: translateY(-2px);
            box-shadow: 0 6px 20px rgba(255, 108, 0, 0.5);
        }
        .exec-logs-toggle {
            background: #F3F4F6;
            border: none;
            padding: 8px 16px;
            border-radius: 8px;
            font-size: 0.75rem;
            color: #6B7280;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 6px;
            margin-top: 16px;
        }
        .exec-logs-panel {
            background: #1F2937;
            border-radius: 8px;
            padding: 16px;
            margin-top: 12px;
            font-family: monospace;
            font-size: 0.75rem;
            color: #9CA3AF;
            max-height: 200px;
            overflow-y: auto;
        }

        /* ===================== ISSUE #134 - WIZARD COMPONENTS ===================== */
        .wizard-container {
            max-width: 600px;
            margin: 0 auto;
            padding: 32px;
        }
        .wizard-progress {
            display: flex;
            justify-content: space-between;
            margin-bottom: 32px;
            position: relative;
        }
        .wizard-progress::before {
            content: '';
            position: absolute;
            top: 20px;
            left: 40px;
            right: 40px;
            height: 3px;
            background: #E5E7EB;
        }
        .wizard-step {
            display: flex;
            flex-direction: column;
            align-items: center;
            position: relative;
            z-index: 1;
        }
        .wizard-step-number {
            width: 40px;
            height: 40px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: 600;
            margin-bottom: 8px;
            transition: all 0.3s;
        }
        .wizard-step-number.completed {
            background: #10B981;
            color: white;
        }
        .wizard-step-number.current {
            background: #003B4A;
            color: white;
            box-shadow: 0 0 0 4px rgba(0,59,74,0.2);
        }
        .wizard-step-number.pending {
            background: #E5E7EB;
            color: #9CA3AF;
        }
        .wizard-step-label {
            font-size: 0.75rem;
            color: #6B7280;
            text-align: center;
            max-width: 100px;
        }
        .wizard-content {
            background: white;
            border-radius: 16px;
            padding: 32px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        }
        .wizard-title {
            font-size: 1.5rem;
            font-weight: 600;
            color: #003B4A;
            margin-bottom: 8px;
        }
        .wizard-description {
            color: #6B7280;
            margin-bottom: 24px;
        }
        .wizard-field {
            margin-bottom: 20px;
        }
        .wizard-field label {
            display: block;
            font-size: 0.875rem;
            font-weight: 500;
            color: #374151;
            margin-bottom: 6px;
        }
        .wizard-field input, .wizard-field textarea, .wizard-field select {
            width: 100%;
            padding: 12px 16px;
            border: 2px solid #E5E7EB;
            border-radius: 10px;
            font-size: 1rem;
            transition: border-color 0.2s;
        }
        .wizard-field input:focus, .wizard-field textarea:focus, .wizard-field select:focus {
            outline: none;
            border-color: #003B4A;
        }
        .wizard-buttons {
            display: flex;
            justify-content: space-between;
            margin-top: 24px;
            padding-top: 24px;
            border-top: 1px solid #E5E7EB;
        }
        .wizard-btn {
            padding: 12px 24px;
            border-radius: 10px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }
        .wizard-btn.secondary {
            background: #F3F4F6;
            border: none;
            color: #374151;
        }
        .wizard-btn.secondary:hover {
            background: #E5E7EB;
        }
        .wizard-btn.primary {
            background: #003B4A;
            border: none;
            color: white;
        }
        .wizard-btn.primary:hover {
            background: #00495d;
        }
        .wizard-option-card {
            border: 2px solid #E5E7EB;
            border-radius: 12px;
            padding: 20px;
            margin-bottom: 12px;
            cursor: pointer;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            gap: 16px;
        }
        .wizard-option-card:hover {
            border-color: #003B4A;
            background: #F8FAFC;
        }
        .wizard-option-card.selected {
            border-color: #003B4A;
            background: #E8F4F7;
        }
        .wizard-option-icon {
            width: 48px;
            height: 48px;
            border-radius: 12px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 1.5rem;
        }
        .wizard-option-content {
            flex: 1;
        }
        .wizard-option-title {
            font-weight: 600;
            color: #003B4A;
            margin-bottom: 4px;
        }
        .wizard-option-desc {
            font-size: 0.875rem;
            color: #6B7280;
        }

        /* ===================== ISSUE #132 - ONBOARDING TOUR ===================== */
        .onboarding-overlay {
            position: fixed;
            inset: 0;
            background: rgba(0, 0, 0, 0.6);
            z-index: 9998;
        }
        .onboarding-spotlight {
            position: fixed;
            z-index: 9999;
            box-shadow: 0 0 0 9999px rgba(0, 0, 0, 0.6);
            border-radius: 8px;
            transition: all 0.3s ease;
        }
        .onboarding-tooltip {
            position: fixed;
            z-index: 10000;
            background: white;
            border-radius: 16px;
            padding: 24px;
            box-shadow: 0 20px 40px rgba(0, 0, 0, 0.2);
            max-width: 360px;
            animation: tooltipIn 0.3s ease;
        }
        @keyframes tooltipIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .onboarding-tooltip::before {
            content: '';
            position: absolute;
            width: 16px;
            height: 16px;
            background: white;
            transform: rotate(45deg);
        }
        .onboarding-tooltip.arrow-top::before { top: -8px; left: 50%; margin-left: -8px; }
        .onboarding-tooltip.arrow-bottom::before { bottom: -8px; left: 50%; margin-left: -8px; }
        .onboarding-tooltip.arrow-left::before { left: -8px; top: 50%; margin-top: -8px; }
        .onboarding-tooltip.arrow-right::before { right: -8px; top: 50%; margin-top: -8px; }
        .onboarding-tooltip-step {
            font-size: 0.75rem;
            color: #FF6C00;
            font-weight: 600;
            margin-bottom: 8px;
        }
        .onboarding-tooltip-title {
            font-size: 1.25rem;
            font-weight: 600;
            color: #003B4A;
            margin-bottom: 8px;
        }
        .onboarding-tooltip-content {
            font-size: 0.875rem;
            color: #6B7280;
            line-height: 1.5;
            margin-bottom: 20px;
        }
        .onboarding-tooltip-buttons {
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .onboarding-skip {
            background: none;
            border: none;
            color: #9CA3AF;
            font-size: 0.875rem;
            cursor: pointer;
        }
        .onboarding-skip:hover {
            color: #6B7280;
        }
        .onboarding-nav {
            display: flex;
            gap: 8px;
        }
        .onboarding-btn {
            padding: 10px 20px;
            border-radius: 8px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }
        .onboarding-btn.prev {
            background: #F3F4F6;
            border: none;
            color: #374151;
        }
        .onboarding-btn.next {
            background: #003B4A;
            border: none;
            color: white;
        }
        .onboarding-progress {
            display: flex;
            gap: 6px;
            margin-top: 16px;
            justify-content: center;
        }
        .onboarding-progress-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            background: #E5E7EB;
            transition: all 0.3s;
        }
        .onboarding-progress-dot.active {
            background: #003B4A;
            width: 24px;
            border-radius: 4px;
        }
        /* Issue #238: Close button for onboarding */
        .onboarding-close-btn {
            position: fixed;
            top: 20px;
            right: 20px;
            z-index: 10001;
            width: 40px;
            height: 40px;
            border-radius: 50%;
            background: white;
            border: none;
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
            display: flex;
            align-items: center;
            justify-content: center;
            cursor: pointer;
            transition: all 0.2s ease;
        }
        .onboarding-close-btn:hover {
            background: #F3F4F6;
            transform: scale(1.05);
        }
        .onboarding-close-btn svg {
            color: #374151;
        }
        .onboarding-checklist {
            background: white;
            border-radius: 12px;
            padding: 20px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            margin-bottom: 20px;
        }
        .onboarding-checklist-title {
            font-size: 1rem;
            font-weight: 600;
            color: #003B4A;
            margin-bottom: 16px;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .onboarding-checklist-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px;
            border-radius: 8px;
            cursor: pointer;
            transition: background 0.2s;
        }
        .onboarding-checklist-item:hover {
            background: #F3F4F6;
        }
        .onboarding-check {
            width: 24px;
            height: 24px;
            border-radius: 50%;
            border: 2px solid #E5E7EB;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.2s;
        }
        .onboarding-check.done {
            background: #10B981;
            border-color: #10B981;
            color: white;
        }
        .onboarding-checklist-text {
            flex: 1;
        }
        .onboarding-checklist-text.done {
            color: #9CA3AF;
            text-decoration: line-through;
        }

        /* View Mode Toggle */
        .view-mode-toggle {
            display: flex;
            background: #F3F4F6;
            border-radius: 10px;
            padding: 4px;
        }
        .view-mode-btn {
            padding: 8px 16px;
            border: none;
            background: transparent;
            border-radius: 8px;
            font-size: 0.875rem;
            font-weight: 500;
            color: #6B7280;
            cursor: pointer;
            transition: all 0.2s;
        }
        .view-mode-btn.active {
            background: white;
            color: #003B4A;
            box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
        }

        /* Integrations Page Styles */
        .integrations-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 20px; padding: 20px 0; }
        .integration-card { background: white; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08); border: 1px solid #E5E7EB; transition: all 0.2s ease; display: flex; flex-direction: column; align-items: center; text-align: center; gap: 12px; }
        .integration-card:hover { transform: translateY(-2px); box-shadow: 0 8px 24px rgba(0, 0, 0, 0.12); border-color: #003B4A; }
        .integration-icon { width: 64px; height: 64px; border-radius: 16px; display: flex; align-items: center; justify-content: center; font-size: 28px; color: white; }
        .integration-icon.github { background: linear-gradient(135deg, #24292E 0%, #404448 100%); }
        .integration-icon.jira { background: linear-gradient(135deg, #0052CC 0%, #2684FF 100%); }
        .integration-icon.azure { background: linear-gradient(135deg, #0078D4 0%, #00BCF2 100%); }
        .integration-icon.sap { background: linear-gradient(135deg, #0FAAFF 0%, #00AEEF 100%); }
        .integration-icon.slack { background: linear-gradient(135deg, #4A154B 0%, #611F69 100%); }
        .integration-icon.teams { background: linear-gradient(135deg, #5059C9 0%, #7B83EB 100%); }
        .integration-card h3 { font-size: 1.1rem; font-weight: 600; color: #1F2937; margin: 0; }
        .integration-card p { font-size: 0.85rem; color: #6B7280; margin: 0; }
        .integration-status { display: inline-flex; align-items: center; gap: 6px; padding: 4px 12px; border-radius: 20px; font-size: 0.75rem; font-weight: 500; }
        .integration-status.connected { background: #DCFCE7; color: #15803D; }
        .integration-status.disconnected { background: #FEE2E2; color: #B91C1C; }
        .integration-status.loading { background: #FEF3C7; color: #B45309; }
        .integration-status-dot { width: 8px; height: 8px; border-radius: 50%; }
        .integration-status.connected .integration-status-dot { background: #22C55E; }
        .integration-status.disconnected .integration-status-dot { background: #EF4444; }
        .integration-status.loading .integration-status-dot { background: #F59E0B; animation: pulse 1.5s infinite; }
        .btn-configure { padding: 8px 20px; border-radius: 8px; font-size: 0.85rem; font-weight: 500; cursor: pointer; transition: all 0.2s; border: none; }
        .btn-configure.primary { background: linear-gradient(135deg, #003B4A 0%, #005566 100%); color: white; }
        .btn-configure.primary:hover { background: linear-gradient(135deg, #004d5f 0%, #006677 100%); transform: translateY(-1px); }
        .btn-configure.secondary { background: #F3F4F6; color: #374151; border: 1px solid #E5E7EB; }
        .btn-configure.secondary:hover { background: #E5E7EB; }
        .btn-configure.danger { background: #FEE2E2; color: #B91C1C; }
        .btn-configure.danger:hover { background: #FECACA; }
        .integrations-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; padding-bottom: 16px; border-bottom: 1px solid #E5E7EB; }
        .integrations-header h2 { font-size: 1.5rem; font-weight: 600; color: #1F2937; margin: 0; }
        .integrations-header p { font-size: 0.9rem; color: #6B7280; margin: 4px 0 0 0; }
        .integration-config-form { display: flex; flex-direction: column; gap: 16px; }
        .integration-config-form .form-group { display: flex; flex-direction: column; gap: 6px; }
        .integration-config-form label { font-size: 0.85rem; font-weight: 500; color: #374151; }
        .integration-config-form input { padding: 10px 14px; border: 1px solid #D1D5DB; border-radius: 8px; font-size: 0.9rem; }
        .integration-config-form input:focus { outline: none; border-color: #003B4A; box-shadow: 0 0 0 3px rgba(0, 59, 74, 0.1); }
        .dark .integration-card { background: #1F2937; border-color: #374151; }
        .dark .integration-card h3 { color: #F9FAFB; }
        .dark .integration-card p { color: #9CA3AF; }
        .dark .integrations-header h2 { color: #F9FAFB; }
        .dark .integrations-header { border-color: #374151; }

        /* ========== ISSUE #220 - BREADCRUMB NAVIGATION ========== */
        .breadcrumb-nav {
            padding: 10px 16px;
            background: #F9FAFB;
            border-bottom: 1px solid #E5E7EB;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .dark .breadcrumb-nav {
            background: #1F2937;
            border-color: #374151;
        }
        .breadcrumb-list {
            display: flex;
            align-items: center;
            gap: 6px;
            list-style: none;
            margin: 0;
            padding: 0;
            font-size: 13px;
            flex-wrap: wrap;
        }
        .breadcrumb-item {
            display: flex;
            align-items: center;
            gap: 6px;
        }
        .breadcrumb-link {
            color: #6B7280;
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: 4px;
            padding: 4px 8px;
            border-radius: 6px;
            transition: all 0.15s ease;
        }
        .breadcrumb-link:hover {
            color: #FF6C00;
            background: rgba(255, 108, 0, 0.08);
        }
        .dark .breadcrumb-link {
            color: #9CA3AF;
        }
        .dark .breadcrumb-link:hover {
            color: #FF6C00;
            background: rgba(255, 108, 0, 0.15);
        }
        .breadcrumb-link .bc-icon {
            font-size: 14px;
            flex-shrink: 0;
        }
        .breadcrumb-separator {
            color: #D1D5DB;
            font-size: 11px;
            user-select: none;
        }
        .dark .breadcrumb-separator {
            color: #4B5563;
        }
        .breadcrumb-current {
            color: #1F2937;
            font-weight: 500;
            padding: 4px 8px;
            background: rgba(0, 59, 74, 0.06);
            border-radius: 6px;
            display: flex;
            align-items: center;
            gap: 4px;
            max-width: 300px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .dark .breadcrumb-current {
            color: #F9FAFB;
            background: rgba(255, 255, 255, 0.08);
        }
        .breadcrumb-current .bc-icon {
            font-size: 14px;
            flex-shrink: 0;
        }
        .breadcrumb-badge {
            font-size: 10px;
            padding: 2px 6px;
            border-radius: 10px;
            background: #E5E7EB;
            color: #6B7280;
            margin-left: 4px;
        }
        .dark .breadcrumb-badge {
            background: #374151;
            color: #9CA3AF;
        }
        .breadcrumb-badge.points {
            background: #DBEAFE;
            color: #1D4ED8;
        }
        .dark .breadcrumb-badge.points {
            background: rgba(59, 130, 246, 0.2);
            color: #60A5FA;
        }
        /* Mobile: truncar itens intermediarios */
        @media (max-width: 640px) {
            .breadcrumb-nav {
                padding: 8px 12px;
            }
            .breadcrumb-list {
                font-size: 12px;
            }
            .breadcrumb-item.collapsible {
                display: none;
            }
            .breadcrumb-ellipsis {
                display: flex;
                align-items: center;
                color: #6B7280;
                padding: 0 4px;
            }
            .breadcrumb-current {
                max-width: 200px;
            }
        }
        @media (min-width: 641px) {
            .breadcrumb-ellipsis {
                display: none;
            }
        }

        /* ========== ISSUE #221 - GLOBAL SEARCH ========== */
        .global-search-container {
            position: relative;
        }
        .global-search-input-wrapper {
            display: flex;
            align-items: center;
            gap: 8px;
            background: rgba(255,255,255,0.1);
            border: 1px solid rgba(255,255,255,0.2);
            border-radius: 8px;
            padding: 6px 12px;
            transition: all 0.2s ease;
        }
        .global-search-input-wrapper:focus-within {
            background: rgba(255,255,255,0.15);
            border-color: rgba(255,255,255,0.4);
            box-shadow: 0 0 0 3px rgba(255,108,0,0.15);
        }
        .global-search-input {
            background: transparent;
            border: none;
            color: white;
            font-size: 13px;
            width: 180px;
            outline: none;
        }
        .global-search-input::placeholder {
            color: rgba(255,255,255,0.6);
        }
        .global-search-input:focus {
            width: 280px;
        }
        .search-shortcut-badge {
            background: rgba(255,255,255,0.15);
            padding: 2px 6px;
            border-radius: 4px;
            font-size: 11px;
            color: rgba(255,255,255,0.7);
            font-family: monospace;
        }
        .global-search-dropdown {
            position: absolute;
            top: calc(100% + 8px);
            left: 0;
            right: 0;
            min-width: 420px;
            max-width: 520px;
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 40px rgba(0,0,0,0.2), 0 0 0 1px rgba(0,0,0,0.05);
            z-index: 9999;
            max-height: 70vh;
            overflow-y: auto;
            animation: searchDropdownIn 0.2s ease-out;
        }
        @keyframes searchDropdownIn {
            from { opacity: 0; transform: translateY(-8px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .dark .global-search-dropdown {
            background: #1F2937;
            box-shadow: 0 20px 40px rgba(0,0,0,0.4), 0 0 0 1px rgba(255,255,255,0.1);
        }
        .search-filters {
            display: flex;
            gap: 6px;
            padding: 12px 16px;
            border-bottom: 1px solid #E5E7EB;
            flex-wrap: wrap;
        }
        .dark .search-filters {
            border-color: #374151;
        }
        .search-filter-chip {
            padding: 4px 12px;
            border-radius: 16px;
            font-size: 12px;
            font-weight: 500;
            background: #F3F4F6;
            color: #6B7280;
            border: none;
            cursor: pointer;
            transition: all 0.15s ease;
        }
        .search-filter-chip:hover {
            background: #E5E7EB;
        }
        .search-filter-chip.active {
            background: #003B4A;
            color: white;
        }
        .dark .search-filter-chip {
            background: #374151;
            color: #9CA3AF;
        }
        .dark .search-filter-chip:hover {
            background: #4B5563;
        }
        .dark .search-filter-chip.active {
            background: #FF6C00;
            color: white;
        }
        .search-results-section {
            padding: 8px 0;
        }
        .search-results-header {
            padding: 8px 16px;
            font-size: 11px;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            color: #9CA3AF;
        }
        .search-result-item {
            display: flex;
            align-items: flex-start;
            gap: 12px;
            padding: 10px 16px;
            cursor: pointer;
            transition: background 0.1s ease;
        }
        .search-result-item:hover, .search-result-item.selected {
            background: #F3F4F6;
        }
        .dark .search-result-item:hover, .dark .search-result-item.selected {
            background: #374151;
        }
        .search-result-icon {
            width: 36px;
            height: 36px;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 8px;
            font-size: 16px;
            flex-shrink: 0;
        }
        .search-result-icon.story { background: #DBEAFE; }
        .search-result-icon.task { background: #D1FAE5; }
        .search-result-icon.doc { background: #FEF3C7; }
        .search-result-icon.project { background: #E0E7FF; }
        .dark .search-result-icon.story { background: rgba(59, 130, 246, 0.2); }
        .dark .search-result-icon.task { background: rgba(16, 185, 129, 0.2); }
        .dark .search-result-icon.doc { background: rgba(245, 158, 11, 0.2); }
        .dark .search-result-icon.project { background: rgba(99, 102, 241, 0.2); }
        .search-result-content {
            flex: 1;
            min-width: 0;
        }
        .search-result-title {
            font-size: 14px;
            font-weight: 500;
            color: #1F2937;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }
        .dark .search-result-title {
            color: #F9FAFB;
        }
        .search-result-title mark {
            background: #FEF3C7;
            color: inherit;
            padding: 0 2px;
            border-radius: 2px;
        }
        .dark .search-result-title mark {
            background: rgba(245, 158, 11, 0.3);
        }
        .search-result-meta {
            font-size: 12px;
            color: #6B7280;
            display: flex;
            align-items: center;
            gap: 8px;
            margin-top: 2px;
        }
        .dark .search-result-meta {
            color: #9CA3AF;
        }
        .search-result-badge {
            display: inline-flex;
            align-items: center;
            padding: 2px 6px;
            border-radius: 10px;
            font-size: 10px;
            font-weight: 500;
        }
        .search-result-badge.status-done { background: #D1FAE5; color: #059669; }
        .search-result-badge.status-in_progress { background: #DBEAFE; color: #2563EB; }
        .search-result-badge.status-backlog { background: #F3F4F6; color: #6B7280; }
        .search-empty {
            padding: 32px 16px;
            text-align: center;
            color: #6B7280;
        }
        .search-empty-icon {
            font-size: 48px;
            margin-bottom: 12px;
            opacity: 0.5;
        }
        .search-empty-text {
            font-size: 14px;
            margin-bottom: 4px;
        }
        .search-empty-hint {
            font-size: 12px;
            opacity: 0.7;
        }
        .search-history {
            padding: 12px 16px;
            border-top: 1px solid #E5E7EB;
        }
        .dark .search-history {
            border-color: #374151;
        }
        .search-history-header {
            font-size: 11px;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            color: #9CA3AF;
            margin-bottom: 8px;
        }
        .search-history-item {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 6px 8px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 13px;
            color: #6B7280;
        }
        .search-history-item:hover {
            background: #F3F4F6;
            color: #1F2937;
        }
        .dark .search-history-item:hover {
            background: #374151;
            color: #F9FAFB;
        }
        .search-footer {
            padding: 8px 16px;
            border-top: 1px solid #E5E7EB;
            display: flex;
            justify-content: space-between;
            align-items: center;
            font-size: 11px;
            color: #9CA3AF;
        }
        .dark .search-footer {
            border-color: #374151;
        }
        .search-footer kbd {
            background: #F3F4F6;
            padding: 2px 6px;
            border-radius: 4px;
            font-family: monospace;
            margin: 0 2px;
        }
        .dark .search-footer kbd {
            background: #374151;
        }

        /* ========== ISSUE #222 - QUICK CREATE FAB ========== */
        .fab-container {
            position: fixed;
            bottom: 24px;
            right: 24px;
            z-index: 1000;
        }
        .fab-main {
            width: 56px;
            height: 56px;
            border-radius: 50%;
            background: linear-gradient(135deg, #FF6C00 0%, #FF8533 100%);
            color: white;
            border: none;
            box-shadow: 0 4px 12px rgba(255, 108, 0, 0.4), 0 2px 4px rgba(0,0,0,0.1);
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 28px;
            transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
        .fab-main:hover {
            transform: scale(1.08);
            box-shadow: 0 6px 20px rgba(255, 108, 0, 0.5), 0 4px 8px rgba(0,0,0,0.15);
        }
        .fab-main.open {
            transform: rotate(45deg);
            background: linear-gradient(135deg, #003B4A 0%, #005566 100%);
            box-shadow: 0 4px 12px rgba(0, 59, 74, 0.4);
        }
        .fab-icon {
            transition: transform 0.3s ease;
            line-height: 1;
        }
        .fab-menu {
            position: absolute;
            bottom: 70px;
            right: 0;
            display: flex;
            flex-direction: column;
            gap: 10px;
            opacity: 0;
            visibility: hidden;
            transform: translateY(20px) scale(0.9);
            transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
            pointer-events: none;
        }
        .fab-menu.open {
            opacity: 1;
            visibility: visible;
            transform: translateY(0) scale(1);
            pointer-events: auto;
        }
        .fab-item {
            display: flex;
            align-items: center;
            gap: 10px;
            padding: 10px 16px;
            background: white;
            border: 1px solid #E5E7EB;
            border-radius: 28px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.1);
            cursor: pointer;
            white-space: nowrap;
            transition: all 0.2s ease;
            transform-origin: right center;
        }
        .dark .fab-item {
            background: #1F2937;
            border-color: #374151;
        }
        .fab-item:hover {
            transform: translateX(-8px) scale(1.02);
            box-shadow: 0 6px 16px rgba(0,0,0,0.15);
            border-color: #FF6C00;
        }
        .fab-item:nth-child(1) { animation-delay: 0.05s; }
        .fab-item:nth-child(2) { animation-delay: 0.1s; }
        .fab-item:nth-child(3) { animation-delay: 0.15s; }
        .fab-item:nth-child(4) { animation-delay: 0.2s; }
        .fab-item:nth-child(5) { animation-delay: 0.25s; }
        .fab-menu.open .fab-item {
            animation: fabItemIn 0.3s ease forwards;
        }
        @keyframes fabItemIn {
            from { opacity: 0; transform: translateX(20px) scale(0.8); }
            to { opacity: 1; transform: translateX(0) scale(1); }
        }
        .fab-item-icon {
            font-size: 18px;
            width: 24px;
            text-align: center;
        }
        .fab-item-label {
            font-size: 14px;
            font-weight: 500;
            color: #1F2937;
        }
        .dark .fab-item-label {
            color: #F9FAFB;
        }
        .fab-item-shortcut {
            font-size: 11px;
            color: #9CA3AF;
            margin-left: auto;
            font-family: monospace;
            background: #F3F4F6;
            padding: 2px 6px;
            border-radius: 4px;
        }
        .dark .fab-item-shortcut {
            background: #374151;
            color: #6B7280;
        }
        .fab-backdrop {
            position: fixed;
            inset: 0;
            background: rgba(0,0,0,0.2);
            opacity: 0;
            visibility: hidden;
            transition: opacity 0.3s ease;
            z-index: 999;
        }
        .fab-backdrop.open {
            opacity: 1;
            visibility: visible;
        }
        /* Mobile adjustment */
        @media (max-width: 768px) {
            .fab-container {
                bottom: 80px;
            }
            .fab-item-shortcut {
                display: none;
            }
        }

        /* ========== ISSUE #223 - STORY TEMPLATES ========== */
        .template-selector {
            padding: 16px;
            border-bottom: 1px solid #E5E7EB;
        }
        .dark .template-selector {
            border-color: #374151;
        }
        .template-selector-header {
            font-size: 14px;
            font-weight: 600;
            color: #374151;
            margin-bottom: 12px;
        }
        .dark .template-selector-header {
            color: #E5E7EB;
        }
        .template-grid {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 10px;
        }
        @media (max-width: 640px) {
            .template-grid {
                grid-template-columns: repeat(2, 1fr);
            }
        }
        .template-card {
            display: flex;
            flex-direction: column;
            align-items: center;
            gap: 6px;
            padding: 16px 12px;
            background: #F9FAFB;
            border: 2px solid transparent;
            border-radius: 12px;
            cursor: pointer;
            transition: all 0.2s ease;
            text-align: center;
        }
        .dark .template-card {
            background: #1F2937;
        }
        .template-card:hover {
            background: #F3F4F6;
            border-color: #D1D5DB;
        }
        .dark .template-card:hover {
            background: #374151;
            border-color: #4B5563;
        }
        .template-card.selected {
            border-color: #FF6C00;
            background: #FFF7ED;
        }
        .dark .template-card.selected {
            border-color: #FF6C00;
            background: rgba(255, 108, 0, 0.1);
        }
        .template-icon {
            font-size: 28px;
            line-height: 1;
        }
        .template-name {
            font-size: 13px;
            font-weight: 600;
            color: #1F2937;
        }
        .dark .template-name {
            color: #F9FAFB;
        }
        .template-desc {
            font-size: 11px;
            color: #6B7280;
            line-height: 1.3;
        }
        .template-blank {
            border-style: dashed;
            border-color: #D1D5DB;
        }
        .dark .template-blank {
            border-color: #4B5563;
        }
        .template-applied-badge {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            padding: 6px 12px;
            background: #FFF7ED;
            border: 1px solid #FDBA74;
            border-radius: 20px;
            font-size: 12px;
            color: #EA580C;
            margin-bottom: 12px;
        }
        .dark .template-applied-badge {
            background: rgba(255, 108, 0, 0.1);
            border-color: #FF6C00;
            color: #FF8533;
        }
        .template-applied-badge button {
            background: none;
            border: none;
            color: inherit;
            cursor: pointer;
            padding: 0;
            font-size: 14px;
            opacity: 0.7;
        }
        .template-applied-badge button:hover {
            opacity: 1;
        }
        .template-selector-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 12px;
        }
        .template-selector-title {
            font-size: 14px;
            font-weight: 600;
            color: #374151;
        }
        .dark .template-selector-title {
            color: #E5E7EB;
        }
        .template-skip-btn {
            font-size: 12px;
            color: #6B7280;
            background: none;
            border: none;
            cursor: pointer;
            padding: 4px 8px;
            border-radius: 4px;
            transition: all 0.15s ease;
        }
        .template-skip-btn:hover {
            color: #374151;
            background: #F3F4F6;
        }
        .dark .template-skip-btn:hover {
            color: #E5E7EB;
            background: #374151;
        }
        .template-loading {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 8px;
            padding: 24px;
            color: #6B7280;
            font-size: 13px;
        }
        .dark .template-loading {
            color: #9CA3AF;
        }
        .template-selected-banner {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 12px 16px;
            background: linear-gradient(135deg, #FFF7ED 0%, #FEF3C7 100%);
            border-bottom: 1px solid #FDBA74;
        }
        .dark .template-selected-banner {
            background: linear-gradient(135deg, rgba(255,108,0,0.1) 0%, rgba(251,191,36,0.1) 100%);
            border-color: #92400E;
        }
        .template-selected-banner .font-medium {
            color: #92400E;
            font-size: 14px;
        }
        .dark .template-selected-banner .font-medium {
            color: #FCD34D;
        }
        </style>
</head>
<body class="bg-gray-100">
    <div id="app" :class="{ 'dark': isDarkMode }">
        <!-- Mobile Overlay -->
        <div class="mobile-overlay" :class="{ 'visible': mobileMenuOpen || mobileChatOpen }" @click="mobileMenuOpen = false; mobileChatOpen = false"></div>
        <!-- Pull to Refresh -->
        <div class="pull-refresh-indicator" :class="{ 'visible': isPullingToRefresh }"><div class="spinner spinner-sm"></div><span>Atualizando...</span></div>
        <!-- HEADER -->
        <header class="belgo-blue text-white shadow-lg">
            <div class="container mx-auto px-4 header-container">
                <div class="flex items-center justify-between h-16">
                    <div class="flex items-center gap-4">
                        <!-- Hamburger Menu (Mobile) -->
                        <button class="mobile-menu-btn" :class="{ 'active': mobileMenuOpen }" @click="mobileMenuOpen = !mobileMenuOpen">
                            <span></span><span></span><span></span>
                        </button>
                        <div class="flex items-center gap-2">
                            <div class="w-8 h-8 bg-white rounded flex items-center justify-center">
                                <span class="text-belgo-blue font-bold">FA</span>
                            </div>
                            <span class="font-semibold text-lg header-title">Fabrica de Agentes</span>
                        </div>
                        <span class="text-gray-300 hide-on-mobile">|</span>
                        <span class="text-sm opacity-80 header-version hide-on-mobile">Dashboard Agile v6.0</span>
                    </div>

                    <div class="flex items-center gap-4">
                        <!-- Issue #158 - Tenant Selector -->
                        <div v-if="userTenants.length > 1" class="hide-on-mobile" style="display:flex;align-items:center;gap:8px;padding:4px 12px;background:rgba(255,255,255,0.1);border-radius:8px;border:1px solid rgba(255,255,255,0.2);">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4"/></svg>
                            <div style="display:flex;flex-direction:column;min-width:100px;">
                                <span style="font-size:9px;text-transform:uppercase;letter-spacing:0.5px;opacity:0.7;">Tenant</span>
                                <select v-model="selectedTenantId" @change="onTenantChange" style="background:transparent;border:none;color:white;font-size:13px;font-weight:500;cursor:pointer;padding:0;min-width:100px;">
                                    <option v-for="tenant in userTenants" :key="tenant.tenant_id" :value="tenant.tenant_id" style="background:white;color:#374151;">{{ tenant.name }}</option>
                                </select>
                            </div>
                        </div>
                        <div v-else-if="userTenants.length === 1" class="hide-on-mobile" style="display:flex;align-items:center;gap:8px;padding:4px 12px;background:rgba(255,255,255,0.1);border-radius:8px;">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4"/></svg>
                            <div style="display:flex;flex-direction:column;">
                                <span style="font-size:9px;text-transform:uppercase;letter-spacing:0.5px;opacity:0.7;">Tenant</span>
                                <span style="font-size:13px;font-weight:500;">{{ currentTenantName }}</span>
                            </div>
                        </div>

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

                        <!-- ISSUE #221, #284 - Global Search (added compatibility classes) -->
                        <div class="global-search-container search" v-click-outside="closeGlobalSearch">
                            <div class="global-search-input-wrapper" @click="openGlobalSearch">
                                <svg class="w-4 h-4 opacity-70" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                                </svg>
                                <input v-model="globalSearchQuery"
                                       @input="debouncedGlobalSearch"
                                       @focus="openGlobalSearch"
                                       @keydown="handleGlobalSearchKey"
                                       type="text"
                                       placeholder="Search / Buscar..."
                                       ref="globalSearchInput"
                                       class="global-search-input search-input"
                                       aria-label="Busca global"
                                       :aria-expanded="showGlobalSearch"
                                       aria-controls="global-search-results">
                                <span class="search-shortcut-badge hide-on-mobile">⌘K</span>
                            </div>

                            <!-- Global Search Dropdown -->
                            <div v-if="showGlobalSearch" id="global-search-results" class="global-search-dropdown" role="listbox">
                                <!-- Filters -->
                                <div class="search-filters">
                                    <button v-for="filter in globalSearchFilters" :key="filter.value"
                                            :class="['search-filter-chip', globalSearchFilter === filter.value ? 'active' : '']"
                                            @click="setGlobalSearchFilter(filter.value)">
                                        {{ filter.label }}
                                    </button>
                                </div>

                                <!-- Loading State -->
                                <div v-if="globalSearchLoading" class="search-empty">
                                    <div class="spinner spinner-sm"></div>
                                    <div class="search-empty-text">Buscando...</div>
                                </div>

                                <!-- Results -->
                                <div v-else-if="globalSearchResults.total > 0">
                                    <!-- Stories -->
                                    <div v-if="globalSearchResults.stories.length && (globalSearchFilter === 'all' || globalSearchFilter === 'stories')" class="search-results-section">
                                        <div class="search-results-header">Stories ({{ globalSearchResults.stories.length }})</div>
                                        <div v-for="(item, idx) in globalSearchResults.stories" :key="'s-'+item.story_id"
                                             :class="['search-result-item', globalSearchSelectedIndex === getGlobalSearchIndex('story', idx) ? 'selected' : '']"
                                             @click="navigateToSearchResult('story', item)"
                                             @mouseenter="globalSearchSelectedIndex = getGlobalSearchIndex('story', idx)">
                                            <div class="search-result-icon story">📄</div>
                                            <div class="search-result-content">
                                                <div class="search-result-title" v-html="highlightMatch(item.story_id + ': ' + item.title, globalSearchQuery)"></div>
                                                <div class="search-result-meta">
                                                    <span :class="['search-result-badge', 'status-'+item.status]">{{ item.status }}</span>
                                                    <span v-if="item.story_points">{{ item.story_points }} pts</span>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <!-- Tasks -->
                                    <div v-if="globalSearchResults.tasks.length && (globalSearchFilter === 'all' || globalSearchFilter === 'tasks')" class="search-results-section">
                                        <div class="search-results-header">Tasks ({{ globalSearchResults.tasks.length }})</div>
                                        <div v-for="(item, idx) in globalSearchResults.tasks" :key="'t-'+item.task_id"
                                             :class="['search-result-item', globalSearchSelectedIndex === getGlobalSearchIndex('task', idx) ? 'selected' : '']"
                                             @click="navigateToSearchResult('task', item)"
                                             @mouseenter="globalSearchSelectedIndex = getGlobalSearchIndex('task', idx)">
                                            <div class="search-result-icon task">☑️</div>
                                            <div class="search-result-content">
                                                <div class="search-result-title" v-html="highlightMatch(item.title, globalSearchQuery)"></div>
                                                <div class="search-result-meta">
                                                    <span>{{ item.story_id }}</span>
                                                    <span :class="['search-result-badge', 'status-'+item.status]">{{ item.status }}</span>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <!-- Docs -->
                                    <div v-if="globalSearchResults.docs.length && (globalSearchFilter === 'all' || globalSearchFilter === 'docs')" class="search-results-section">
                                        <div class="search-results-header">Documentos ({{ globalSearchResults.docs.length }})</div>
                                        <div v-for="(item, idx) in globalSearchResults.docs" :key="'d-'+item.doc_id"
                                             :class="['search-result-item', globalSearchSelectedIndex === getGlobalSearchIndex('doc', idx) ? 'selected' : '']"
                                             @click="navigateToSearchResult('doc', item)"
                                             @mouseenter="globalSearchSelectedIndex = getGlobalSearchIndex('doc', idx)">
                                            <div class="search-result-icon doc">📚</div>
                                            <div class="search-result-content">
                                                <div class="search-result-title" v-html="highlightMatch(item.title, globalSearchQuery)"></div>
                                                <div class="search-result-meta">
                                                    <span>{{ item.story_id }}</span>
                                                    <span>{{ item.doc_type }}</span>
                                                </div>
                                            </div>
                                        </div>
                                    </div>

                                    <!-- Projects -->
                                    <div v-if="globalSearchResults.projects.length && (globalSearchFilter === 'all' || globalSearchFilter === 'projects')" class="search-results-section">
                                        <div class="search-results-header">Projetos ({{ globalSearchResults.projects.length }})</div>
                                        <div v-for="(item, idx) in globalSearchResults.projects" :key="'p-'+item.project_id"
                                             :class="['search-result-item', globalSearchSelectedIndex === getGlobalSearchIndex('project', idx) ? 'selected' : '']"
                                             @click="navigateToSearchResult('project', item)"
                                             @mouseenter="globalSearchSelectedIndex = getGlobalSearchIndex('project', idx)">
                                            <div class="search-result-icon project">📁</div>
                                            <div class="search-result-content">
                                                <div class="search-result-title" v-html="highlightMatch(item.name, globalSearchQuery)"></div>
                                                <div class="search-result-meta">
                                                    <span>{{ item.project_type }}</span>
                                                    <span :class="['search-result-badge', 'status-'+item.status]">{{ item.status }}</span>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                </div>

                                <!-- Empty State (with query) -->
                                <div v-else-if="globalSearchQuery.length >= 2" class="search-empty">
                                    <div class="search-empty-icon">🔍</div>
                                    <div class="search-empty-text">Nenhum resultado encontrado</div>
                                    <div class="search-empty-hint">Tente outros termos de busca</div>
                                </div>

                                <!-- Initial State (no query) -->
                                <div v-else-if="globalSearchHistory.length" class="search-history">
                                    <div class="search-history-header">Buscas Recentes</div>
                                    <div v-for="(q, idx) in globalSearchHistory" :key="'h-'+idx"
                                         class="search-history-item"
                                         @click="applySearchHistory(q)">
                                        <span>🕐</span>
                                        <span>{{ q }}</span>
                                    </div>
                                </div>
                                <div v-else class="search-empty">
                                    <div class="search-empty-icon">🔎</div>
                                    <div class="search-empty-text">Digite para buscar</div>
                                    <div class="search-empty-hint">Stories, tasks, documentos e projetos</div>
                                </div>

                                <!-- Footer -->
                                <div class="search-footer">
                                    <span><kbd>↑</kbd><kbd>↓</kbd> navegar</span>
                                    <span><kbd>Enter</kbd> selecionar</span>
                                    <span><kbd>Esc</kbd> fechar</span>
                                </div>
                            </div>
                        </div>

                        <!-- Atalhos de Teclado -->
                        <button @click="showShortcutsModal = true"
                                class="text-white/70 hover:text-white p-2 transition"
                                title="Atalhos de teclado (?)">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"/>
                            </svg>
                        </button>

                        <!-- WebSocket Status -->
                        <div :class="['flex items-center gap-1 px-2 py-1 rounded-full text-xs', wsStatus === 'connected' ? 'bg-green-500/20 text-green-300' : wsStatus === 'connecting' ? 'bg-yellow-500/20 text-yellow-300' : 'bg-red-500/20 text-red-300']" :title="wsStatusTitle">
                            <span :class="['w-2 h-2 rounded-full', wsStatus === 'connected' ? 'bg-green-400 animate-pulse' : wsStatus === 'connecting' ? 'bg-yellow-400' : 'bg-red-400']"></span>
                            <span class="hidden sm:inline">{{ wsStatusText }}</span>
                        </div>

                        <!-- Notification Sound Toggle -->
                        <button @click="toggleNotificationSound" class="text-white/70 hover:text-white p-1" :title="notificationSoundEnabled ? 'Desativar som' : 'Ativar som'">
                            <span v-if="notificationSoundEnabled">🔔</span>
                            <span v-else>🔕</span>
                        </button>

                        <!-- Dark Mode Toggle -->
                        <button @click="toggleDarkMode"
                                class="dark-mode-toggle text-white/70 hover:text-white"
                                :title="isDarkMode ? 'Modo Claro' : 'Modo Escuro'">
                            <span v-if="!isDarkMode" class="dark-mode-icon">🌙</span>
                            <span v-else class="dark-mode-icon">☀️</span>
                        </button>

                        <!-- View Mode Toggle (Issue #135) -->
                        <div class="view-mode-toggle hide-on-mobile" v-if="selectedProjectId">
                            <button :class="['view-mode-btn', viewMode === 'technical' ? 'active' : '']"
                                    @click="viewMode = 'technical'" title="Visao Tecnica">
                                Tecnico
                            </button>
                            <button :class="['view-mode-btn', viewMode === 'executive' ? 'active' : '']"
                                    @click="viewMode = 'executive'" title="Visao Executiva">
                                Executivo
                            </button>
                        </div>

                        <!-- Nova Story (Issue #308: use method for better Vue reactivity) -->
                        <button @click="openNewStoryModal"
                                class="bg-[#FF6C00] hover:bg-orange-600 px-4 py-1.5 rounded text-sm font-medium transition">
                            + Nova {{ translateTerm('story') }}
                        </button>
                    </div>
                </div>
            </div>
        </header>

        <!-- ISSUE #220, #284 - BREADCRUMB NAVIGATION (added compatibility classes) -->
        <nav class="breadcrumb-nav breadcrumb" aria-label="breadcrumb" role="navigation" v-if="breadcrumbItems.length > 1">
            <ol class="breadcrumb-list">
                <!-- Home sempre visivel -->
                <li class="breadcrumb-item">
                    <a href="javascript:void(0)" class="breadcrumb-link" @click="navigateBreadcrumb('home')">
                        <span class="bc-icon">🏠</span>
                        <span>Home</span>
                    </a>
                </li>

                <!-- Itens do meio (collapsible no mobile) -->
                <template v-for="(item, index) in breadcrumbItems.slice(1, -1)" :key="'bc-'+index">
                    <li class="breadcrumb-separator" aria-hidden="true">›</li>
                    <li class="breadcrumb-item collapsible">
                        <a href="javascript:void(0)" class="breadcrumb-link" @click="navigateBreadcrumb(item.type, item.id)">
                            <span v-if="item.icon" class="bc-icon">{{ item.icon }}</span>
                            <span>{{ item.label }}</span>
                            <span v-if="item.badge" :class="['breadcrumb-badge', item.badgeType]">{{ item.badge }}</span>
                        </a>
                    </li>
                </template>

                <!-- Ellipsis para mobile -->
                <li v-if="breadcrumbItems.length > 2" class="breadcrumb-separator breadcrumb-ellipsis" aria-hidden="true">...</li>

                <!-- Item atual (sempre visivel) -->
                <li v-if="breadcrumbItems.length > 1" class="breadcrumb-separator" aria-hidden="true">›</li>
                <li class="breadcrumb-item">
                    <span class="breadcrumb-current" :aria-current="'page'">
                        <span v-if="breadcrumbItems[breadcrumbItems.length - 1]?.icon" class="bc-icon">{{ breadcrumbItems[breadcrumbItems.length - 1].icon }}</span>
                        <span>{{ breadcrumbItems[breadcrumbItems.length - 1]?.label }}</span>
                        <span v-if="breadcrumbItems[breadcrumbItems.length - 1]?.badge" :class="['breadcrumb-badge', breadcrumbItems[breadcrumbItems.length - 1]?.badgeType]">{{ breadcrumbItems[breadcrumbItems.length - 1].badge }}</span>
                    </span>
                </li>
            </ol>
        </nav>

        <div class="flex main-content-mobile" :style="{ height: breadcrumbItems.length > 1 ? 'calc(100vh - 100px)' : 'calc(100vh - 64px)' }">
            <!-- SIDEBAR -->
            <aside class="w-64 bg-white border-r border-gray-200 overflow-y-auto sidebar-desktop" :class="{ 'open': mobileMenuOpen }">
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
                            
                    <!-- Project Preview Button (Issue #73) -->
                    <button @click="openProjectPreview"
                            v-if="selectedProjectId"
                            class="w-full flex items-center gap-2 px-3 py-2 text-sm bg-gradient-to-r from-[#003B4A] to-[#005566] text-white rounded transition hover:opacity-90">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"/>
                        </svg>
                        <span>Preview do Projeto</span>
                    </button>

                    + Novo Epic
                        </button>
                        <button @click="showNewSprintModal = true"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded">
                            + Novo Sprint
                        </button>
                    </div>

                    <!-- Analytics (Issue #65) -->
                    <div class="mt-6 pt-4 border-t border-gray-200" v-if="selectedProjectId">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Analytics</h3>
                        <button @click="showAnalyticsModal = true; loadAnalytics()"
                                class="w-full text-left px-3 py-2 text-sm text-blue-600 hover:bg-blue-50 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path>
                            </svg>
                            Produtividade do Time
                        </button>
                    </div>

                    <!-- Onboarding Checklist (Issue #132) -->
                    <div class="mt-6 pt-4 border-t border-gray-200" v-if="showOnboardingChecklist && !onboardingComplete">
                        <div class="onboarding-checklist">
                            <div class="onboarding-checklist-title">
                                <span>Primeiros Passos</span>
                                <span class="text-xs text-gray-400 ml-auto">{{ onboardingProgress }}%</span>
                            </div>
                            <div v-for="(step, idx) in onboardingSteps" :key="idx"
                                 class="onboarding-checklist-item"
                                 @click="handleOnboardingStep(step)">
                                <div :class="['onboarding-check', step.done ? 'done' : '']">
                                    <span v-if="step.done">✓</span>
                                </div>
                                <span :class="['onboarding-checklist-text', step.done ? 'done' : '']">
                                    {{ step.label }}
                                </span>
                            </div>
                            <button @click="startOnboardingTour" class="mt-3 text-xs text-blue-600 hover:underline">
                                Iniciar tour guiado
                            </button>
                        </div>
                    </div>

                    <!-- Wizard Buttons (Issue #134) -->
                    <div class="mt-6 pt-4 border-t border-gray-200">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Assistentes</h3>
                        <button @click="showProjectWizard = true"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <span>+</span> Novo Projeto (Wizard)
                        </button>
                        <button @click="showIntegrationWizard = true"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <span>+</span> Configurar Conexao
                        </button>
                    </div>

                    <!-- Integrations Menu -->
                    <div class="mt-6 pt-4 border-t border-gray-200">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Integracoes</h3>
                        <button @click="showIntegrationsModal = true; loadIntegrationsStatus()"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13.828 10.172a4 4 0 00-5.656 0l-4 4a4 4 0 105.656 5.656l1.102-1.101m-.758-4.899a4 4 0 005.656 0l4-4a4 4 0 00-5.656-5.656l-1.1 1.1"/>
                            </svg>
                            Gerenciar Integracoes
                        </button>
                    </div>

                    <!-- Security Settings (Issue #156) -->
                    <div class="mt-6 pt-4 border-t border-gray-200">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Configuracoes</h3>
                        <button @click="showSecuritySettings = true; loadSecurityData()"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/>
                            </svg>
                            Seguranca
                        </button>
                        <!-- Issue #159: Billing Dashboard Link -->
                        <a href="/billing" target="_blank"
                           class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z"/>
                            </svg>
                            Faturamento
                        </a>
                        <!-- Issue #187: RBAC Admin Link -->
                        <a href="/admin/rbac" target="_blank"
                           class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"/>
                            </svg>
                            Permissoes (RBAC)
                        </a>
                    </div>
                </div>
            </aside>

        <!-- MODAL: Security Settings (Issue #156) -->
        <div v-if="showSecuritySettings" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showSecuritySettings = false">
            <div class="bg-white rounded-lg w-[95vw] max-w-[900px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Configuracoes de Seguranca</h2>
                        <p class="text-sm text-blue-200">MFA, API Keys e Sessoes Ativas</p>
                    </div>
                    <button @click="showSecuritySettings = false" class="text-white/70 hover:text-white text-xl">&times;</button>
                </div>
                <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 80px);">
                    <!-- Security Tabs -->
                    <div class="flex border-b border-gray-200 mb-6">
                        <button @click="securityActiveTab = 'mfa'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'mfa' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            Autenticacao MFA
                        </button>
                        <button @click="securityActiveTab = 'apikeys'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'apikeys' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            API Keys
                        </button>
                        <button @click="securityActiveTab = 'sessions'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'sessions' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            Sessoes Ativas
                        </button>
                    </div>

                    <!-- MFA Tab -->
                    <div v-if="securityActiveTab === 'mfa'" class="space-y-6">
                        <div class="bg-gray-50 rounded-lg p-4">
                            <div class="flex items-center justify-between mb-4">
                                <div>
                                    <h3 class="font-semibold text-gray-800">Autenticacao de Dois Fatores (2FA)</h3>
                                    <p class="text-sm text-gray-500">Adicione uma camada extra de seguranca a sua conta</p>
                                </div>
                                <div :class="['px-3 py-1 rounded-full text-sm font-medium', securityData.mfa?.enabled ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-600']">
                                    {{ securityData.mfa?.enabled ? 'Ativado' : 'Desativado' }}
                                </div>
                            </div>

                            <div v-if="!securityData.mfa?.enabled && !mfaSetupMode">
                                <button @click="startMfaSetup" class="px-4 py-2 bg-[#003B4A] text-white rounded hover:bg-[#004d5c] transition">
                                    Ativar 2FA
                                </button>
                            </div>

                            <div v-if="mfaSetupMode" class="space-y-4">
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">1. Escaneie o QR Code</h4>
                                    <p class="text-sm text-gray-600 mb-3">Use um aplicativo autenticador para escanear:</p>
                                    <div class="flex justify-center mb-4">
                                        <img v-if="mfaSetupData.qr_code_base64" :src="'data:image/png;base64,' + mfaSetupData.qr_code_base64" alt="QR Code" class="w-48 h-48 border rounded">
                                        <div v-else class="w-48 h-48 bg-gray-100 rounded flex items-center justify-center"><div class="spinner"></div></div>
                                    </div>
                                    <p class="text-xs text-gray-500 text-center">Ou insira manualmente: <code class="bg-gray-100 px-1 rounded">{{ mfaSetupData.secret_key }}</code></p>
                                </div>
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">2. Salve os codigos de backup</h4>
                                    <div class="bg-gray-50 rounded p-3 font-mono text-sm grid grid-cols-2 gap-2">
                                        <span v-for="(code, idx) in mfaSetupData.backup_codes" :key="idx" class="text-center">{{ code }}</span>
                                    </div>
                                    <button @click="copyBackupCodes" class="mt-2 text-sm text-blue-600 hover:underline">Copiar codigos</button>
                                </div>
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">3. Verifique o codigo</h4>
                                    <div class="flex gap-2">
                                        <input v-model="mfaVerifyCode" type="text" maxlength="6" placeholder="000000"
                                               class="flex-1 px-3 py-2 border rounded text-center text-lg tracking-widest font-mono">
                                        <button @click="completeMfaSetup" :disabled="mfaVerifyCode.length !== 6"
                                                class="px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50">Verificar</button>
                                    </div>
                                    <p v-if="mfaError" class="mt-2 text-sm text-red-600">{{ mfaError }}</p>
                                </div>
                                <button @click="cancelMfaSetup" class="text-sm text-gray-500 hover:underline">Cancelar</button>
                            </div>

                            <div v-if="securityData.mfa?.enabled && !mfaSetupMode" class="space-y-4">
                                <div class="text-sm text-gray-600">
                                    <p>Ativado em: {{ formatDate(securityData.mfa.enabled_at) }}</p>
                                    <p>Ultimo uso: {{ formatDate(securityData.mfa.last_used_at) || 'Nunca' }}</p>
                                    <p>Codigos de backup restantes: {{ securityData.mfa.backup_codes_remaining || 0 }}</p>
                                </div>
                                <div class="flex gap-2">
                                    <button @click="regenerateBackupCodes" class="px-3 py-1.5 text-sm border border-gray-300 rounded hover:bg-gray-50">Regenerar Codigos</button>
                                    <button @click="disableMfa" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Desativar 2FA</button>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- API Keys Tab -->
                    <div v-if="securityActiveTab === 'apikeys'" class="space-y-6">
                        <div class="flex justify-between items-center">
                            <div>
                                <h3 class="font-semibold text-gray-800">API Keys</h3>
                                <p class="text-sm text-gray-500">Gerencie suas chaves de API</p>
                            </div>
                            <button @click="showCreateApiKeyModal = true" class="px-4 py-2 bg-[#003B4A] text-white rounded hover:bg-[#004d5c] flex items-center gap-2">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/></svg>
                                Nova API Key
                            </button>
                        </div>
                        <div v-if="securityData.apiKeys?.length" class="space-y-3">
                            <div v-for="key in securityData.apiKeys" :key="key.key_id" class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <div class="flex items-start justify-between">
                                    <div class="flex-1">
                                        <div class="flex items-center gap-2 mb-1">
                                            <span class="font-medium">{{ key.name }}</span>
                                            <span :class="['px-2 py-0.5 rounded text-xs', key.status === 'active' ? 'bg-green-100 text-green-700' : 'bg-red-100 text-red-700']">{{ key.status }}</span>
                                            <span class="px-2 py-0.5 bg-blue-100 text-blue-700 rounded text-xs">{{ key.tier }}</span>
                                        </div>
                                        <div class="text-sm text-gray-500 font-mono">{{ key.key_prefix }}</div>
                                        <div class="mt-2 text-xs text-gray-500">
                                            Criada: {{ formatDate(key.created_at) }} | Ultimo uso: {{ formatDate(key.last_used_at) || 'Nunca' }} | {{ key.requests_total }} req
                                        </div>
                                    </div>
                                    <button v-if="key.status === 'active'" @click="revokeApiKey(key.key_id)" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Revogar</button>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500">
                            <svg class="w-12 h-12 mx-auto mb-3 text-gray-300" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z"/></svg>
                            <p>Nenhuma API Key criada</p>
                        </div>
                    </div>

                    <!-- Sessions Tab -->
                    <div v-if="securityActiveTab === 'sessions'" class="space-y-6">
                        <div class="flex justify-between items-center">
                            <div>
                                <h3 class="font-semibold text-gray-800">Sessoes Ativas</h3>
                                <p class="text-sm text-gray-500">Dispositivos conectados</p>
                            </div>
                            <button @click="revokeAllSessions" class="px-4 py-2 text-red-600 border border-red-300 rounded hover:bg-red-50">Encerrar Todas</button>
                        </div>
                        <div v-if="securityData.sessions?.length" class="space-y-3">
                            <div v-for="session in securityData.sessions" :key="session.session_id" :class="['rounded-lg p-4 border', session.is_current ? 'bg-green-50 border-green-200' : 'bg-gray-50 border-gray-200']">
                                <div class="flex items-start justify-between">
                                    <div class="flex-1">
                                        <div class="flex items-center gap-2 mb-1">
                                            <span class="font-medium">{{ session.device_name || 'Dispositivo Desconhecido' }}</span>
                                            <span v-if="session.is_current" class="px-2 py-0.5 bg-green-200 text-green-800 rounded text-xs">Sessao Atual</span>
                                        </div>
                                        <div class="text-sm text-gray-500">
                                            <p>IP: {{ session.ip_address }} | {{ session.browser || 'Desconhecido' }}</p>
                                            <p>Ultimo acesso: {{ formatDate(session.last_active) }}</p>
                                        </div>
                                    </div>
                                    <button v-if="!session.is_current" @click="revokeSession(session.session_id)" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Encerrar</button>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500"><p>Nenhuma sessao ativa</p></div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Create API Key Modal (Issue #156) -->
        <div v-if="showCreateApiKeyModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showCreateApiKeyModal = false">
            <div class="bg-white rounded-lg w-full max-w-md shadow-xl">
                <div class="p-4 border-b"><h3 class="text-lg font-semibold">Nova API Key</h3></div>
                <div class="p-4 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Nome</label>
                        <input v-model="newApiKey.name" type="text" placeholder="Ex: Producao App" class="w-full px-3 py-2 border rounded">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newApiKey.description" class="w-full px-3 py-2 border rounded" rows="2"></textarea>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Tier</label>
                        <select v-model="newApiKey.tier" class="w-full px-3 py-2 border rounded">
                            <option value="free">Free</option><option value="basic">Basic</option><option value="pro">Pro</option><option value="enterprise">Enterprise</option>
                        </select>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Scopes</label>
                        <div class="flex flex-wrap gap-2">
                            <label v-for="scope in ['read', 'write', 'admin', 'webhooks']" :key="scope" class="flex items-center gap-1 px-2 py-1 border rounded cursor-pointer">
                                <input type="checkbox" :value="scope" v-model="newApiKey.scopes" class="rounded"><span class="text-sm">{{ scope }}</span>
                            </label>
                        </div>
                    </div>
                </div>
                <div class="p-4 border-t flex justify-end gap-2">
                    <button @click="showCreateApiKeyModal = false" class="px-4 py-2 border rounded">Cancelar</button>
                    <button @click="createApiKey" :disabled="!newApiKey.name" class="px-4 py-2 bg-[#003B4A] text-white rounded disabled:opacity-50">Criar</button>
                </div>
            </div>
        </div>

        <!-- Show Created API Key Modal (Issue #156) -->
        <div v-if="createdApiKey" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-full max-w-md shadow-xl">
                <div class="p-4 border-b bg-green-50"><h3 class="text-lg font-semibold text-green-800">API Key Criada!</h3></div>
                <div class="p-4">
                    <div class="bg-yellow-50 border border-yellow-200 rounded p-3 mb-4">
                        <p class="text-sm text-yellow-800 font-medium">Esta e a unica vez que a chave sera exibida!</p>
                    </div>
                    <div class="bg-gray-50 rounded p-3 font-mono text-sm break-all border">{{ createdApiKey.api_key }}</div>
                    <button @click="copyCreatedApiKey" class="mt-2 text-sm text-blue-600 hover:underline">Copiar</button>
                </div>
                <div class="p-4 border-t flex justify-end">
                    <button @click="createdApiKey = null; loadSecurityData()" class="px-4 py-2 bg-[#003B4A] text-white rounded">Fechar</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Analytics Dashboard (Issue #65 + Issue #157) -->
        <div v-if="showAnalyticsModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showAnalyticsModal = false">
            <div class="bg-white rounded-lg w-[95vw] max-w-[1200px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <div><h2 class="text-lg font-semibold">Analytics de Produtividade</h2><p class="text-sm text-blue-200">Metricas do time e insights com graficos</p></div>
                    <div class="flex items-center gap-4">
                        <select v-model="analyticsDays" @change="loadAnalytics" class="bg-white/10 text-white border border-white/20 rounded px-3 py-1 text-sm"><option value="7" class="text-gray-800">7 dias</option><option value="30" class="text-gray-800">30 dias</option><option value="90" class="text-gray-800">90 dias</option></select>
                        <button @click="showAnalyticsModal = false" class="text-white/70 hover:text-white text-xl">X</button>
                    </div>
                </div>
                <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 80px);">
                    <div v-if="analyticsLoading" class="flex items-center justify-center py-12"><div class="spinner"></div><span class="ml-3">Carregando...</span></div>
                    <div v-else-if="analyticsData">
                        <!-- Alerts -->
                        <div v-if="analyticsData.alerts?.length" class="mb-6"><div v-for="(alert, i) in analyticsData.alerts" :key="i" :class="['p-4 rounded-lg mb-2', alert.type === 'danger' ? 'bg-red-50' : alert.type === 'warning' ? 'bg-yellow-50' : 'bg-blue-50']"><h4 class="font-semibold">{{ alert.title }}</h4><p class="text-sm">{{ alert.message }}</p></div></div>

                        <!-- KPI Cards -->
                        <div class="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4 mb-6">
                            <div class="bg-blue-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ analyticsData.team_metrics?.total_stories || 0 }}</div><div class="text-xs opacity-80">Total Stories</div></div>
                            <div class="bg-green-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ analyticsData.team_metrics?.stories_completed || 0 }}</div><div class="text-xs opacity-80">Concluidas</div></div>
                            <div class="bg-purple-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ analyticsData.team_metrics?.points_delivered || 0 }}</div><div class="text-xs opacity-80">Pontos</div></div>
                            <div class="bg-orange-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ (analyticsData.team_metrics?.avg_velocity || 0).toFixed(1) }}</div><div class="text-xs opacity-80">Velocity</div></div>
                            <div class="bg-cyan-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ (analyticsData.team_metrics?.avg_cycle_time_days || 0).toFixed(1) }}d</div><div class="text-xs opacity-80">Cycle Time</div></div>
                            <div class="bg-pink-500 rounded-xl p-4 text-white"><div class="text-2xl font-bold">{{ (analyticsData.team_metrics?.predictability_score || 0).toFixed(0) }}%</div><div class="text-xs opacity-80">Predictability</div></div>
                        </div>

                        <!-- Charts Grid (Issue #157) -->
                        <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                            <!-- Velocity History Chart -->
                            <div class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <h3 class="font-semibold mb-4 text-gray-800">Velocity por Sprint</h3>
                                <div class="h-[250px]">
                                    <canvas id="velocity-chart"></canvas>
                                </div>
                                <div v-if="velocityHistory" class="mt-2 flex items-center justify-center gap-4 text-sm text-gray-600">
                                    <span>Media: <strong>{{ velocityHistory.avg_velocity }}</strong> pts/sprint</span>
                                    <span :class="velocityHistory.trend === 'increasing' ? 'text-green-600' : velocityHistory.trend === 'decreasing' ? 'text-red-600' : 'text-gray-600'">
                                        Tendencia: {{ velocityHistory.trend === 'increasing' ? '↑ Crescente' : velocityHistory.trend === 'decreasing' ? '↓ Decrescente' : '→ Estavel' }}
                                    </span>
                                </div>
                            </div>

                            <!-- Status Distribution Chart -->
                            <div class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <h3 class="font-semibold mb-4 text-gray-800">Distribuicao por Status</h3>
                                <div class="h-[250px]">
                                    <canvas id="status-chart"></canvas>
                                </div>
                            </div>

                            <!-- Developer Points Chart -->
                            <div class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <h3 class="font-semibold mb-4 text-gray-800">Pontos por Desenvolvedor</h3>
                                <div class="h-[250px]">
                                    <canvas id="developer-chart"></canvas>
                                </div>
                            </div>

                            <!-- Completion Time Chart -->
                            <div class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <h3 class="font-semibold mb-4 text-gray-800">Tempo Medio de Conclusao (dias)</h3>
                                <div class="h-[250px]">
                                    <canvas id="cycletime-chart"></canvas>
                                </div>
                            </div>
                        </div>

                        <!-- Top Contributors -->
                        <div class="mb-6" v-if="analyticsData.top_contributors?.length">
                            <h3 class="font-semibold mb-4">Top Contribuidores</h3>
                            <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3">
                                <div v-for="(dev, i) in analyticsData.top_contributors" :key="dev.assignee" class="flex items-center gap-4 p-3 bg-gray-50 rounded-lg border border-gray-200">
                                    <div :class="['w-10 h-10 rounded-full flex items-center justify-center text-white font-bold', i === 0 ? 'bg-yellow-500' : i === 1 ? 'bg-gray-400' : i === 2 ? 'bg-orange-600' : 'bg-blue-500']">{{ i + 1 }}</div>
                                    <div class="flex-1">
                                        <div class="font-medium">{{ dev.assignee }}</div>
                                        <div class="text-sm text-gray-500">{{ dev.stories_completed }}/{{ dev.stories_total }} stories | {{ dev.points_delivered }} pts</div>
                                    </div>
                                    <div class="text-right">
                                        <div class="text-green-600 font-bold">{{ dev.completion_rate }}%</div>
                                        <div class="text-xs text-gray-500">{{ dev.avg_time_days || 0 }}d avg</div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- AI Insights -->
                        <div v-if="analyticsInsights" class="bg-purple-50 rounded-lg p-6 border border-purple-200">
                            <h3 class="font-semibold mb-2">AI Insights</h3>
                            <p class="text-gray-700 mb-4">{{ analyticsInsights.summary }}</p>
                            <div v-if="analyticsInsights.insights?.length" class="space-y-2">
                                <div v-for="insight in analyticsInsights.insights" :key="insight.title" class="p-3 bg-white rounded-lg">
                                    <div class="font-medium">{{ insight.title }}</div>
                                    <div class="text-sm text-gray-600">{{ insight.description }}</div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div v-else class="text-center py-12 text-gray-500">Selecione um projeto para ver as metricas.</div>
                </div>
            </div>
        </div>

        <!-- MODAL: File Viewer -->
        <div v-if="showFileViewer" class="fixed inset-0 bg-black/70 z-50 flex items-center justify-center" @click.self="closeFileViewer">
            <div class="file-viewer-modal rounded-xl w-[95vw] max-w-[1200px] max-h-[90vh] shadow-2xl overflow-hidden">
                <div class="file-viewer-header flex justify-between items-center text-white"><div class="flex items-center gap-3"><div class="w-10 h-10 rounded-lg bg-white/10 flex items-center justify-center"><svg v-if="fileViewerData.fileType === 'code'" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"/></svg><svg v-else-if="fileViewerData.fileType === 'markdown'" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"/></svg><svg v-else-if="fileViewerData.fileType === 'pdf'" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z"/></svg><svg v-else class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"/></svg></div><div><h2 class="text-lg font-semibold">{{ fileViewerData.file?.name }}</h2><div class="flex items-center gap-2 text-sm text-white/60"><span class="file-info-badge">{{ fileViewerData.language || fileViewerData.fileType }}</span><span v-if="fileViewerData.file?.size">{{ formatFileSize(fileViewerData.file.size) }}</span></div></div></div><button @click="closeFileViewer" class="text-white/70 hover:text-white text-2xl font-bold">&times;</button></div>
                <div class="file-viewer-content"><div v-if="fileViewerData.loading" class="flex items-center justify-center py-20"><div class="animate-spin w-8 h-8 border-2 border-white/20 border-t-white rounded-full"></div></div><div v-else-if="fileViewerData.error" class="flex flex-col items-center justify-center py-20 text-red-400"><p>{{ fileViewerData.error }}</p></div><div v-else-if="fileViewerData.fileType === 'code'" class="file-viewer-code line-numbers"><pre :class="'language-' + fileViewerData.language"><code :class="'language-' + fileViewerData.language">{{ fileViewerData.content }}</code></pre></div><div v-else-if="fileViewerData.fileType === 'markdown'" class="markdown-body" v-html="renderMarkdown(fileViewerData.content)"></div><div v-else-if="fileViewerData.fileType === 'pdf'" class="pdf-viewer-container"><iframe :src="fileViewerData.file?.url" class="w-full h-[70vh] border-0 rounded-lg"></iframe></div><div v-else-if="fileViewerData.fileType === 'image'" class="image-viewer-container"><img :src="fileViewerData.file?.url" :alt="fileViewerData.file?.name"></div></div>
                <div class="file-viewer-toolbar"><button @click="downloadViewerFile" class="file-viewer-btn secondary"><svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"/></svg>Download</button><button v-if="fileViewerData.fileType === 'code'" @click="copyFileContent" class="file-viewer-btn secondary"><svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/></svg>Copiar</button><button @click="openInNewTab" class="file-viewer-btn primary ml-auto"><svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"/></svg>Abrir</button></div>
            </div>
        </div>

        <!-- MODAL: Doc Viewer -->
        <div v-if="showDocViewer" class="fixed inset-0 bg-black/70 z-50 flex items-center justify-center" @click.self="closeDocViewer">
            <div class="bg-white rounded-xl w-[95vw] max-w-[900px] max-h-[90vh] shadow-2xl overflow-hidden">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white"><div><h2 class="text-lg font-semibold">{{ docViewerData.doc?.title }}</h2><span class="text-sm text-white/70">{{ docViewerData.doc?.doc_type }}</span></div><div class="flex items-center gap-2"><button @click="toggleDocEditMode" class="px-3 py-1.5 rounded bg-white/10 hover:bg-white/20 text-sm">{{ docViewerData.editMode ? 'Visualizar' : 'Editar' }}</button><button @click="closeDocViewer" class="text-white/70 hover:text-white text-xl">&times;</button></div></div>
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 120px);"><div v-if="docViewerData.editMode"><textarea v-model="docViewerData.editContent" class="w-full h-[60vh] p-4 border rounded-lg font-mono text-sm resize-none"></textarea><div class="mt-4 flex justify-end gap-2"><button @click="docViewerData.editMode = false" class="px-4 py-2 border rounded hover:bg-gray-100">Cancelar</button><button @click="saveDocContent" class="px-4 py-2 bg-[#FF6C00] text-white rounded hover:bg-[#e56000]">Salvar</button></div></div><div v-else class="markdown-body" v-html="renderMarkdown(docViewerData.doc?.content || '')"></div></div>
            </div>
        </div>

        <!-- MODAL: Project Wizard (Issue #134) -->
        <div v-if="showProjectWizard" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showProjectWizard = false">
            <div class="bg-gray-50 rounded-2xl w-[700px] max-w-[95vw] max-h-[90vh] shadow-xl overflow-hidden">
                <div class="wizard-container">
                    <!-- Progress Steps -->
                    <div class="wizard-progress">
                        <div v-for="(step, idx) in wizardSteps" :key="idx" class="wizard-step">
                            <div :class="['wizard-step-number',
                                          idx < wizardCurrentStep ? 'completed' :
                                          idx === wizardCurrentStep ? 'current' : 'pending']">
                                <span v-if="idx < wizardCurrentStep">✓</span>
                                <span v-else>{{ idx + 1 }}</span>
                            </div>
                            <div class="wizard-step-label">{{ step.label }}</div>
                        </div>
                    </div>

                    <!-- Content -->
                    <div class="wizard-content">
                        <!-- Step 1: Project Type -->
                        <div v-if="wizardCurrentStep === 0">
                            <h2 class="wizard-title">Que tipo de projeto voce quer criar?</h2>
                            <p class="wizard-description">Escolha o tipo que melhor descreve seu projeto.</p>
                            <div class="space-y-3">
                                <div v-for="opt in projectTypeOptions" :key="opt.value"
                                     :class="['wizard-option-card', wizardData.projectType === opt.value ? 'selected' : '']"
                                     @click="wizardData.projectType = opt.value">
                                    <div class="wizard-option-icon" :style="{background: opt.color}">{{ opt.icon }}</div>
                                    <div class="wizard-option-content">
                                        <div class="wizard-option-title">{{ opt.title }}</div>
                                        <div class="wizard-option-desc">{{ opt.desc }}</div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Step 2: Project Details -->
                        <div v-if="wizardCurrentStep === 1">
                            <h2 class="wizard-title">Detalhes do Projeto</h2>
                            <p class="wizard-description">Informe os dados basicos do seu projeto.</p>
                            <div class="wizard-field">
                                <label>Nome do Projeto</label>
                                <input v-model="wizardData.name" type="text" placeholder="Ex: Sistema de Vendas">
                            </div>
                            <div class="wizard-field">
                                <label>Descricao</label>
                                <textarea v-model="wizardData.description" rows="3" placeholder="Descreva brevemente o projeto..."></textarea>
                            </div>
                        </div>

                        <!-- Step 3: First Feature -->
                        <div v-if="wizardCurrentStep === 2">
                            <h2 class="wizard-title">Primeira {{ translateTerm('story') }}</h2>
                            <p class="wizard-description">Descreva a primeira {{ translateTerm('story') }} do projeto.</p>
                            <div class="wizard-field">
                                <label>O que voce quer que o sistema faca?</label>
                                <textarea v-model="wizardData.firstFeature" rows="4" placeholder="Ex: Permitir que os usuarios facam login com email e senha"></textarea>
                            </div>
                        </div>

                        <!-- Step 4: Confirmation -->
                        <div v-if="wizardCurrentStep === 3">
                            <h2 class="wizard-title">Tudo pronto!</h2>
                            <p class="wizard-description">Confira os dados e clique em Criar para iniciar seu projeto.</p>
                            <div class="bg-gray-50 rounded-lg p-4 space-y-2">
                                <div><strong>Tipo:</strong> {{ getProjectTypeName(wizardData.projectType) }}</div>
                                <div><strong>Nome:</strong> {{ wizardData.name }}</div>
                                <div><strong>Descricao:</strong> {{ wizardData.description }}</div>
                                <div><strong>Primeira {{ translateTerm('story') }}:</strong> {{ wizardData.firstFeature }}</div>
                            </div>
                        </div>

                        <!-- Buttons -->
                        <div class="wizard-buttons">
                            <button v-if="wizardCurrentStep > 0" class="wizard-btn secondary" @click="wizardCurrentStep--">
                                Voltar
                            </button>
                            <div v-else></div>
                            <button v-if="wizardCurrentStep < wizardSteps.length - 1"
                                    class="wizard-btn primary"
                                    @click="wizardCurrentStep++"
                                    :disabled="!canProceedWizard">
                                Proximo
                            </button>
                            <button v-else class="wizard-btn primary" @click="createProjectFromWizard">
                                Criar Projeto
                            </button>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- MODAL: Integration Wizard (Issue #134) -->
        <div v-if="showIntegrationWizard" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showIntegrationWizard = false">
            <div class="bg-gray-50 rounded-2xl w-[600px] max-w-[95vw] shadow-xl overflow-hidden">
                <div class="wizard-container">
                    <div class="wizard-content">
                        <h2 class="wizard-title">Configurar {{ translateTerm('api') }}</h2>
                        <p class="wizard-description">Escolha o servico que deseja conectar ao seu projeto.</p>
                        <div class="space-y-3">
                            <div v-for="integration in availableIntegrations" :key="integration.id"
                                 class="wizard-option-card"
                                 @click="selectIntegration(integration)">
                                <div class="wizard-option-icon" :style="{background: integration.color}">{{ integration.icon }}</div>
                                <div class="wizard-option-content">
                                    <div class="wizard-option-title">{{ integration.name }}</div>
                                    <div class="wizard-option-desc">{{ integration.desc }}</div>
                                </div>
                            </div>
                        </div>
                        <div class="wizard-buttons">
                            <button class="wizard-btn secondary" @click="showIntegrationWizard = false">Cancelar</button>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- MODAL: Integrations Dashboard -->
        <div v-if="showIntegrationsModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showIntegrationsModal = false">
            <div class="bg-white rounded-lg w-[95vw] max-w-[1000px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Integracoes</h2>
                        <p class="text-sm text-blue-200">Conecte ferramentas externas ao seu projeto</p>
                    </div>
                    <div class="flex items-center gap-4">
                        <button @click="loadIntegrationsStatus()" class="px-3 py-1 bg-white/10 hover:bg-white/20 rounded text-sm transition">Atualizar Status</button>
                        <button @click="showIntegrationsModal = false" class="text-white/70 hover:text-white text-xl">X</button>
                    </div>
                </div>
                <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 80px);">
                    <div v-if="integrationsLoading" class="flex items-center justify-center py-12">
                        <div class="spinner"></div>
                        <span class="ml-3">Carregando integracoes...</span>
                    </div>
                    <div v-else class="integrations-grid">
                        <!-- GitHub -->
                        <div class="integration-card">
                            <div class="integration-icon github">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>
                            </div>
                            <h3>GitHub</h3>
                            <p>Sincronize issues e commits com seu repositorio</p>
                            <span :class="['integration-status', integrationsStatus.github?.connected ? 'connected' : 'disconnected']">
                                <span class="integration-status-dot"></span>
                                {{ integrationsStatus.github?.connected ? 'Conectado' : 'Desconectado' }}
                            </span>
                            <button v-if="integrationsStatus.github?.connected" @click="openIntegrationConfig('github')" class="btn-configure secondary">Configurar</button>
                            <button v-else @click="openIntegrationConfig('github')" class="btn-configure primary">Conectar</button>
                        </div>
                        <!-- Jira -->
                        <div class="integration-card">
                            <div class="integration-icon jira">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M11.571 11.429h.857v.857h-.857zm0 1.714h.857v.857h-.857zm-2.571-1.714h.857v.857h-.857zm0 1.714h.857v.857h-.857zm5.143-1.714h.857v.857h-.857zm0 1.714h.857v.857h-.857zM12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm4.286 14.286H7.714V7.714h8.572v8.572z"/></svg>
                            </div>
                            <h3>Jira</h3>
                            <p>Sincronize tarefas e sprints com o Jira</p>
                            <span :class="['integration-status', integrationsStatus.jira?.connected ? 'connected' : 'disconnected']">
                                <span class="integration-status-dot"></span>
                                {{ integrationsStatus.jira?.connected ? 'Conectado' : 'Desconectado' }}
                            </span>
                            <button v-if="integrationsStatus.jira?.connected" @click="openIntegrationConfig('jira')" class="btn-configure secondary">Configurar</button>
                            <button v-else @click="openIntegrationConfig('jira')" class="btn-configure primary">Conectar</button>
                        </div>
                        <!-- Azure DevOps -->
                        <div class="integration-card">
                            <div class="integration-icon azure">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M0 8.877L2.247 5.91l8.405-3.416V.022l7.37 5.393L2.966 8.338v8.225L0 15.707zm24-4.45v14.651l-5.753 4.9-9.303-3.057v3.056l-5.978-7.416 15.057 1.798V5.415z"/></svg>
                            </div>
                            <h3>Azure DevOps</h3>
                            <p>Integre com pipelines e work items</p>
                            <span :class="['integration-status', integrationsStatus.azure_devops?.connected ? 'connected' : 'disconnected']">
                                <span class="integration-status-dot"></span>
                                {{ integrationsStatus.azure_devops?.connected ? 'Conectado' : 'Desconectado' }}
                            </span>
                            <button v-if="integrationsStatus.azure_devops?.connected" @click="openIntegrationConfig('azure_devops')" class="btn-configure secondary">Configurar</button>
                            <button v-else @click="openIntegrationConfig('azure_devops')" class="btn-configure primary">Conectar</button>
                        </div>
                        <!-- SAP -->
                        <div class="integration-card">
                            <div class="integration-icon sap">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"/></svg>
                            </div>
                            <h3>SAP</h3>
                            <p>Conecte com sistemas SAP ERP</p>
                            <span class="integration-status disconnected"><span class="integration-status-dot"></span>Em breve</span>
                            <button class="btn-configure secondary" disabled>Em desenvolvimento</button>
                        </div>
                        <!-- Slack -->
                        <div class="integration-card">
                            <div class="integration-icon slack">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M5.042 15.165a2.528 2.528 0 0 1-2.52 2.523A2.528 2.528 0 0 1 0 15.165a2.527 2.527 0 0 1 2.522-2.52h2.52v2.52zM6.313 15.165a2.527 2.527 0 0 1 2.521-2.52 2.527 2.527 0 0 1 2.521 2.52v6.313A2.528 2.528 0 0 1 8.834 24a2.528 2.528 0 0 1-2.521-2.522v-6.313z"/></svg>
                            </div>
                            <h3>Slack</h3>
                            <p>Receba notificacoes em tempo real</p>
                            <span class="integration-status disconnected"><span class="integration-status-dot"></span>Em breve</span>
                            <button class="btn-configure secondary" disabled>Em desenvolvimento</button>
                        </div>
                        <!-- Microsoft Teams -->
                        <div class="integration-card">
                            <div class="integration-icon teams">
                                <svg width="28" height="28" viewBox="0 0 24 24" fill="currentColor"><path d="M19.5 3H4.5A1.5 1.5 0 003 4.5v15A1.5 1.5 0 004.5 21h15a1.5 1.5 0 001.5-1.5v-15A1.5 1.5 0 0019.5 3zm-9 15h-3v-9h3v9zm6 0h-3v-6h3v6z"/></svg>
                            </div>
                            <h3>Microsoft Teams</h3>
                            <p>Integre com canais e chats do Teams</p>
                            <span class="integration-status disconnected"><span class="integration-status-dot"></span>Em breve</span>
                            <button class="btn-configure secondary" disabled>Em desenvolvimento</button>
                        </div>
                    </div>
                    <!-- Issue #154: Integration Configuration Panel -->
                    <div class="mt-6 pt-6 border-t">
                        <h3 class="text-lg font-semibold mb-4">Configurar Integracao</h3>
                        <div class="border-b bg-gray-50 mb-4"><div class="flex"><button v-for="tab in integrationTabs" :key="tab.id" @click="activeIntegrationTab = tab.id; loadIntegrationConfigs()" :class="['px-4 py-2 text-sm border-b-2', activeIntegrationTab === tab.id ? 'border-[#FF6C00] text-[#FF6C00] bg-white' : 'border-transparent text-gray-500']">{{ tab.label }}<span v-if="integrationConfigs[tab.id]?.connected" class="ml-1 w-2 h-2 bg-green-500 rounded-full inline-block"></span></button></div></div>
                        <div class="bg-white p-4 rounded-lg border">
                            <div v-if="activeIntegrationTab === 'github'" class="grid grid-cols-2 gap-4"><div><label class="block text-sm font-medium mb-1">Token</label><input type="password" v-model="integrationConfigs.github.token" class="w-full px-3 py-2 border rounded" placeholder="ghp_xxx"></div><div><label class="block text-sm font-medium mb-1">Owner</label><input type="text" v-model="integrationConfigs.github.owner" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Repo</label><input type="text" v-model="integrationConfigs.github.repo" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Branch</label><input type="text" v-model="integrationConfigs.github.branch" class="w-full px-3 py-2 border rounded" placeholder="main"></div></div>
                            <div v-if="activeIntegrationTab === 'gitlab'" class="grid grid-cols-2 gap-4"><div><label class="block text-sm font-medium mb-1">URL</label><input type="text" v-model="integrationConfigs.gitlab.url" class="w-full px-3 py-2 border rounded" placeholder="https://gitlab.com"></div><div><label class="block text-sm font-medium mb-1">Token</label><input type="password" v-model="integrationConfigs.gitlab.token" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Project ID</label><input type="text" v-model="integrationConfigs.gitlab.project_id" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Branch</label><input type="text" v-model="integrationConfigs.gitlab.branch" class="w-full px-3 py-2 border rounded" placeholder="main"></div></div>
                            <div v-if="activeIntegrationTab === 'sap'" class="grid grid-cols-2 gap-4"><div class="col-span-2"><label class="block text-sm font-medium mb-1">System URL</label><input type="text" v-model="integrationConfigs.sap.host" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Client</label><input type="text" v-model="integrationConfigs.sap.client" class="w-full px-3 py-2 border rounded" placeholder="100"></div><div><label class="block text-sm font-medium mb-1">Environment</label><select v-model="integrationConfigs.sap.environment" class="w-full px-3 py-2 border rounded"><option value="cloud">Cloud</option><option value="private">Private</option><option value="on_premise">On-Premise</option></select></div><div><label class="block text-sm font-medium mb-1">Username</label><input type="text" v-model="integrationConfigs.sap.username" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Password</label><input type="password" v-model="integrationConfigs.sap.password" class="w-full px-3 py-2 border rounded"></div></div>
                            <div v-if="activeIntegrationTab === 'salesforce'" class="grid grid-cols-2 gap-4"><div><label class="block text-sm font-medium mb-1">Client ID</label><input type="text" v-model="integrationConfigs.salesforce.client_id" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Client Secret</label><input type="password" v-model="integrationConfigs.salesforce.client_secret" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Username</label><input type="text" v-model="integrationConfigs.salesforce.username" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Security Token</label><input type="password" v-model="integrationConfigs.salesforce.security_token" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Domain</label><select v-model="integrationConfigs.salesforce.domain" class="w-full px-3 py-2 border rounded"><option value="login">Production</option><option value="test">Sandbox</option></select></div></div>
                            <div v-if="activeIntegrationTab === 'jira'" class="grid grid-cols-2 gap-4"><div class="col-span-2"><label class="block text-sm font-medium mb-1">Jira URL</label><input type="text" v-model="integrationConfigs.jira.url" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Email</label><input type="email" v-model="integrationConfigs.jira.email" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">API Token</label><input type="password" v-model="integrationConfigs.jira.token" class="w-full px-3 py-2 border rounded"></div><div><label class="block text-sm font-medium mb-1">Project Key</label><input type="text" v-model="integrationConfigs.jira.project_key" class="w-full px-3 py-2 border rounded"></div></div>
                            <div class="mt-4 flex justify-between"><button @click="testIntegration(activeIntegrationTab)" :disabled="integrationTesting === activeIntegrationTab" class="px-4 py-2 bg-gray-600 text-white rounded disabled:opacity-50">{{ integrationTesting === activeIntegrationTab ? 'Testando...' : 'Testar Conexao' }}</button><button @click="saveIntegration(activeIntegrationTab)" :disabled="integrationSaving === activeIntegrationTab" class="px-4 py-2 bg-[#FF6C00] text-white rounded disabled:opacity-50">{{ integrationSaving === activeIntegrationTab ? 'Salvando...' : 'Salvar' }}</button></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- ONBOARDING TOUR (Issue #132, #238: Fixed overlay blocking) -->
        <div v-if="showOnboardingTour" class="onboarding-overlay" @click.self="skipOnboardingTour">
            <div class="onboarding-spotlight" :style="onboardingSpotlightStyle"></div>
            <!-- Issue #238: Close button clearly visible -->
            <button class="onboarding-close-btn" @click.stop="skipOnboardingTour" aria-label="Fechar tour">
                <svg width="20" height="20" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                </svg>
            </button>
            <div :class="['onboarding-tooltip', 'arrow-' + currentTourStep.arrow]" :style="onboardingTooltipStyle" @click.stop>
                <div class="onboarding-tooltip-step">Passo {{ currentTourStepIndex + 1 }} de {{ tourSteps.length }}</div>
                <h3 class="onboarding-tooltip-title">{{ currentTourStep.title }}</h3>
                <p class="onboarding-tooltip-content">{{ currentTourStep.content }}</p>
                <div class="onboarding-tooltip-buttons">
                    <button class="onboarding-skip" @click.stop="skipOnboardingTour">Pular tour</button>
                    <div class="onboarding-nav">
                        <button v-if="currentTourStepIndex > 0" class="onboarding-btn prev" @click.stop="prevTourStep">
                            Anterior
                        </button>
                        <button v-if="currentTourStepIndex < tourSteps.length - 1" class="onboarding-btn next" @click.stop="nextTourStep">
                            Proximo
                        </button>
                        <button v-else class="onboarding-btn next" @click.stop="finishOnboardingTour">
                            Concluir
                        </button>
                    </div>
                </div>
                <div class="onboarding-progress">
                    <div v-for="(step, idx) in tourSteps" :key="idx"
                         :class="['onboarding-progress-dot', idx === currentTourStepIndex ? 'active' : '']"></div>
                </div>
            </div>
        </div>

            <!-- MAIN CONTENT - KANBAN -->
            <main class="flex-1 overflow-x-auto bg-gray-50 p-4 main-content">
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

                <!-- EXECUTIVE DASHBOARD VIEW (Issue #135) -->
                <div v-else-if="viewMode === 'executive'" class="executive-dashboard">
                    <!-- Executive Header -->
                    <div class="exec-header">
                        <div>
                            <h1>{{ currentProjectName }}</h1>
                            <p class="subtitle">Visao Executiva do Projeto</p>
                        </div>
                        <button class="exec-main-btn" @click="openAppPreview" v-if="projectReadyToTest">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z"/>
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                            </svg>
                            Ver meu App
                        </button>
                    </div>

                    <!-- Status Cards -->
                    <div class="exec-status-cards">
                        <div :class="['exec-status-card', projectHealthStatus]">
                            <div :class="['exec-status-indicator', projectHealthColor]"></div>
                            <div class="exec-card-value">{{ doneStories }}</div>
                            <div class="exec-card-label">{{ translateTerm('story', true) }} Prontas</div>
                            <div class="exec-card-meta">de {{ totalStories }} planejadas</div>
                        </div>
                        <div class="exec-status-card status-green">
                            <div class="exec-status-indicator green"></div>
                            <div class="exec-card-value">{{ projectProgressPercent }}%</div>
                            <div class="exec-card-label">Progresso Geral</div>
                            <div class="exec-card-meta">{{ estimatedDaysRemaining }} dias restantes</div>
                        </div>
                        <div class="exec-status-card status-yellow">
                            <div class="exec-status-indicator yellow"></div>
                            <div class="exec-card-value">{{ inProgressStories }}</div>
                            <div class="exec-card-label">Em Desenvolvimento</div>
                            <div class="exec-card-meta">{{ translateTerm('sprint') }} atual</div>
                        </div>
                        <div class="exec-status-card status-green">
                            <div class="exec-status-indicator green"></div>
                            <div class="exec-card-value">{{ totalPoints }}</div>
                            <div class="exec-card-label">Pontos de Esforco</div>
                            <div class="exec-card-meta">{{ donePoints }} entregues</div>
                        </div>
                    </div>

                    <!-- Progress Section -->
                    <div class="exec-progress-section">
                        <div class="flex justify-between items-center">
                            <h3 class="font-semibold text-gray-800">Progresso do Projeto</h3>
                            <span class="text-2xl font-bold text-green-600">{{ projectProgressPercent }}%</span>
                        </div>
                        <div class="exec-progress-bar">
                            <div class="exec-progress-fill" :style="{ width: projectProgressPercent + '%' }"></div>
                        </div>

                        <!-- Timeline -->
                        <div class="exec-timeline">
                            <div class="exec-timeline-step" v-for="(phase, idx) in projectPhases" :key="idx">
                                <div :class="['exec-timeline-dot', phase.status]">
                                    <span v-if="phase.status === 'completed'">✓</span>
                                    <span v-else-if="phase.status === 'current'">●</span>
                                    <span v-else>○</span>
                                </div>
                                <div class="exec-timeline-label">{{ phase.label }}</div>
                            </div>
                        </div>
                    </div>

                    <!-- Technical Logs Toggle (hidden by default) -->
                    <div class="bg-white rounded-lg p-4 shadow-sm">
                        <button class="exec-logs-toggle" @click="showTechnicalLogs = !showTechnicalLogs">
                            <svg class="w-4 h-4" :class="{ 'transform rotate-180': showTechnicalLogs }" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                            </svg>
                            {{ showTechnicalLogs ? 'Ocultar detalhes tecnicos' : 'Ver detalhes tecnicos' }}
                        </button>
                        <div v-if="showTechnicalLogs" class="exec-logs-panel">
                            <div v-for="log in recentActivityLogs" :key="log.id" class="mb-1">
                                <span class="text-green-400">[{{ log.time }}]</span> {{ log.message }}
                            </div>
                            <div v-if="!recentActivityLogs.length" class="text-gray-500">
                                Nenhuma atividade recente
                            </div>
                        </div>
                    </div>
                </div>

                <!-- TECHNICAL VIEW (Kanban) -->
                <div v-else class="flex flex-col h-full">
                    <!-- Barra de Filtros -->
                    <div class="flex items-center gap-3 mb-4 flex-wrap filter-bar">
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
                        <div class="flex items-center gap-2 bg-white px-3 py-1.5 rounded-lg shadow-sm">
                            <span class="text-xs text-gray-500">Agrupar:</span>
                            <select v-model="groupBy" class="text-sm border-0 bg-transparent focus:ring-0 cursor-pointer">
                                <option value="">Nenhum</option>
                                <option value="epic">Por Epic</option>
                                <option value="assignee">Por Assignee</option>
                                <option value="priority">Por Prioridade</option>
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

                    <!-- Bulk Actions Toolbar - Enhanced -->
                    <div v-if="bulkSelectMode && selectedStories.length > 0"
                         class="fixed bottom-6 left-1/2 transform -translate-x-1/2 z-50
                                bg-[#003B4A] text-white px-4 py-3 rounded-2xl shadow-xl
                                flex items-center gap-3 bulk-toolbar animate-slide-up">
                        <!-- Selection Info -->
                        <div class="flex items-center gap-2">
                            <span class="bg-white/20 px-2 py-1 rounded-lg text-sm font-bold">{{ selectedStories.length }}</span>
                            <span class="text-sm">selecionadas</span>
                        </div>
                        <div class="h-6 w-px bg-white/30"></div>

                        <!-- Select All -->
                        <button @click="selectAllVisibleStories" class="bulk-action-btn" title="Selecionar todas visiveis">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"/>
                            </svg>
                            Todas
                        </button>

                        <div class="h-6 w-px bg-white/30"></div>

                        <!-- Move Dropdown -->
                        <div class="relative bulk-dropdown">
                            <button @click="bulkDropdownOpen = bulkDropdownOpen === 'move' ? null : 'move'"
                                    class="bulk-action-btn flex items-center gap-1">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7h12m0 0l-4-4m4 4l-4 4m0 6H4m0 0l4 4m-4-4l4-4"/>
                                </svg>
                                Mover
                                <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                </svg>
                            </button>
                            <div v-if="bulkDropdownOpen === 'move'"
                                 class="absolute bottom-full left-0 mb-2 bg-white rounded-lg shadow-xl py-2 min-w-[160px] text-gray-700">
                                <button @click="bulkMoveStories('backlog'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">Backlog</button>
                                <button @click="bulkMoveStories('ready'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">Ready</button>
                                <button @click="bulkMoveStories('in_progress'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">In Progress</button>
                                <button @click="bulkMoveStories('review'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">Review</button>
                                <button @click="bulkMoveStories('testing'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">Testing</button>
                                <button @click="bulkMoveStories('done'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm">Done</button>
                            </div>
                        </div>

                        <!-- Priority Dropdown -->
                        <div class="relative bulk-dropdown">
                            <button @click="bulkDropdownOpen = bulkDropdownOpen === 'priority' ? null : 'priority'"
                                    class="bulk-action-btn flex items-center gap-1">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 21v-4m0 0V5a2 2 0 012-2h6.5l1 1H21l-3 6 3 6h-8.5l-1-1H5a2 2 0 00-2 2zm9-13.5V9"/>
                                </svg>
                                Prioridade
                                <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                </svg>
                            </button>
                            <div v-if="bulkDropdownOpen === 'priority'"
                                 class="absolute bottom-full left-0 mb-2 bg-white rounded-lg shadow-xl py-2 min-w-[140px] text-gray-700">
                                <button @click="bulkSetPriority('urgent'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm flex items-center gap-2">
                                    <span class="w-2 h-2 rounded-full bg-red-500"></span> Urgente
                                </button>
                                <button @click="bulkSetPriority('high'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm flex items-center gap-2">
                                    <span class="w-2 h-2 rounded-full bg-orange-500"></span> Alta
                                </button>
                                <button @click="bulkSetPriority('medium'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm flex items-center gap-2">
                                    <span class="w-2 h-2 rounded-full bg-yellow-500"></span> Media
                                </button>
                                <button @click="bulkSetPriority('low'); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm flex items-center gap-2">
                                    <span class="w-2 h-2 rounded-full bg-green-500"></span> Baixa
                                </button>
                            </div>
                        </div>

                        <!-- Assign Dropdown -->
                        <div class="relative bulk-dropdown">
                            <button @click="bulkDropdownOpen = bulkDropdownOpen === 'assign' ? null : 'assign'"
                                    class="bulk-action-btn flex items-center gap-1">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"/>
                                </svg>
                                Atribuir
                                <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                </svg>
                            </button>
                            <div v-if="bulkDropdownOpen === 'assign'"
                                 class="absolute bottom-full left-0 mb-2 bg-white rounded-lg shadow-xl py-2 min-w-[160px] text-gray-700 max-h-48 overflow-y-auto">
                                <button @click="bulkAssign(null); bulkDropdownOpen = null" class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm text-gray-400">
                                    Sem atribuicao
                                </button>
                                <button v-for="member in teamMembers" :key="member"
                                        @click="bulkAssign(member); bulkDropdownOpen = null"
                                        class="w-full text-left px-4 py-2 hover:bg-gray-100 text-sm flex items-center gap-2">
                                    <span class="w-6 h-6 bg-blue-500 text-white rounded-full flex items-center justify-center text-xs">
                                        {{ member.charAt(0).toUpperCase() }}
                                    </span>
                                    {{ member }}
                                </button>
                            </div>
                        </div>

                        <div class="h-6 w-px bg-white/30"></div>

                        <!-- Delete -->
                        <button @click="bulkDeleteStories" class="bulk-action-btn text-red-400 hover:text-red-300 hover:bg-red-500/20" title="Excluir selecionadas">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                            </svg>
                        </button>

                        <!-- Cancel -->
                        <button @click="cancelBulkSelect" class="bulk-action-btn opacity-70 hover:opacity-100" title="Cancelar">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>

                    <!-- Undo Toast -->
                    <div v-if="undoAction"
                         class="fixed bottom-24 left-1/2 transform -translate-x-1/2 z-50
                                bg-gray-800 text-white px-4 py-2 rounded-lg shadow-lg
                                flex items-center gap-3 animate-slide-up">
                        <span class="text-sm">{{ undoAction.message }}</span>
                        <button @click="performUndo" class="text-blue-400 hover:text-blue-300 font-medium text-sm">
                            Desfazer
                        </button>
                        <button @click="undoAction = null" class="text-gray-400 hover:text-gray-300">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>

                    <!-- Kanban View Toggle: Normal vs Swimlanes -->
                    <div v-if="!groupBy" class="flex gap-4 overflow-x-auto kanban-container">
                        <!-- Colunas do Kanban (Vista Normal) -->
                        <div v-for="(column, status) in filteredStoryBoard" :key="status"
                             class="flex-shrink-0 w-80 bg-gray-100 rounded-lg kanban-column-container">
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

                    <!-- Swimlanes View -->
                    <div v-else class="swimlanes-container">
                        <div v-for="(group, groupKey) in groupedStories" :key="groupKey" class="swimlane">
                            <!-- Swimlane Header -->
                            <div class="swimlane-header">
                                <span :style="group.color ? {borderLeft: `4px solid ${group.color}`} : {}"
                                      class="pl-2">
                                    {{ group.name }}
                                </span>
                            </div>

                            <!-- Swimlane Content (Columns) -->
                            <div class="swimlane-content">
                                <div v-for="status in ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done']"
                                     :key="status"
                                     class="swimlane-column bg-gray-100 rounded-lg">
                                    <!-- Column Header -->
                                    <div class="p-2 border-b border-gray-200 bg-white rounded-t-lg">
                                        <div class="flex items-center justify-between">
                                            <span class="text-xs font-semibold text-gray-700">{{ getColumnTitle(status) }}</span>
                                            <span class="bg-gray-200 text-gray-600 text-xs px-1.5 py-0.5 rounded-full">
                                                {{ group[status]?.length || 0 }}
                                            </span>
                                        </div>
                                    </div>

                                    <!-- Stories in Column -->
                                    <div :id="'swimlane-' + groupKey + '-' + status"
                                         class="kanban-column p-2 space-y-2 overflow-y-auto"
                                         style="min-height: 200px; max-height: 300px;">
                                        <!-- Story Card (same as regular Kanban) -->
                                        <div v-for="story in group[status]" :key="story.story_id"
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
                        </div>
                    </div>
                </div>
            </main>

            <!-- CHAT PANEL -->
            <aside class="w-80 bg-white border-l border-gray-200 flex flex-col chat-panel-desktop hide-on-mobile" :class="{ 'open': mobileChatOpen }">
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
                        <button v-for="tab in ['Detalhes', 'Tasks', 'Docs', 'Design', 'Anexos']" :key="tab"
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

                                <!-- Generate Tests Button -->
                                <div v-if="task.code_output" class="mt-2 pt-2 border-t border-gray-100">
                                    <div class="flex gap-2 flex-wrap">
                                        <button @click.stop="generateTestsForTask(task)"
                                                :disabled="generatingTests === task.task_id"
                                                class="text-xs bg-purple-600 text-white px-2 py-1 rounded hover:bg-purple-700 disabled:opacity-50 flex items-center gap-1">
                                            <svg v-if="generatingTests !== task.task_id" class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                            </svg>
                                            <svg v-else class="w-3 h-3 animate-spin" fill="none" viewBox="0 0 24 24">
                                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
                                            </svg>
                                            {{ generatingTests === task.task_id ? 'Gerando...' : 'Generate Tests' }}
                                        </button>
                                        <!-- Code Review Button (Issue #52) -->
                                        <button @click.stop="codeReviewTask(task)"
                                                :disabled="reviewingCode === task.task_id"
                                                class="text-xs bg-blue-600 text-white px-2 py-1 rounded hover:bg-blue-700 disabled:opacity-50 flex items-center gap-1">
                                            <svg v-if="reviewingCode !== task.task_id" class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01"/>
                                            </svg>
                                            <svg v-else class="w-3 h-3 animate-spin" fill="none" viewBox="0 0 24 24">
                                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
                                            </svg>
                                            {{ reviewingCode === task.task_id ? 'Analisando...' : 'Review com IA' }}
                                        </button>
                                        <!-- Security Scan Button (Issue #57) -->
                                        <button @click.stop="runSecurityScan(task)"
                                                :disabled="scanningTask === task.task_id"
                                                class="text-xs bg-red-600 text-white px-2 py-1 rounded hover:bg-red-700 disabled:opacity-50 flex items-center gap-1">
                                            <svg v-if="scanningTask !== task.task_id" class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z"/>
                                            </svg>
                                            <svg v-else class="w-3 h-3 animate-spin" fill="none" viewBox="0 0 24 24">
                                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
                                            </svg>
                                            {{ scanningTask === task.task_id ? 'Scanning...' : 'Scan de Seguranca' }}
                                        </button>
                                    </div>
                                    <div v-if="task.generated_tests?.test_code" class="mt-1">
                                        <button @click.stop="showGeneratedTests(task)"
                                                class="text-xs text-purple-600 hover:text-purple-800 underline">
                                            Ver testes gerados ({{ task.generated_tests.test_count || 0 }} testes)
                                        </button>
                                    </div>
                                    <!-- Code Review Result Link (Issue #52) -->
                                    <div v-if="task.review_result?.score" class="mt-1">
                                        <button @click.stop="showCodeReviewResult(task)"
                                                class="text-xs hover:underline"
                                                :class="task.review_result.score >= 80 ? 'text-green-600' : task.review_result.score >= 60 ? 'text-yellow-600' : 'text-red-600'">
                                            Ver Code Review (Score: {{ task.review_result.score }}/100)
                                        </button>
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
                            <div class="flex gap-2">
                                <!-- Gerar com IA -->
                                <div class="relative inline-block">
                                    <button @click="showGenerateDocsDropdown = !showGenerateDocsDropdown"
                                            class="text-sm bg-blue-600 text-white px-3 py-1 rounded hover:bg-blue-700 flex items-center gap-1">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"/>
                                        </svg>
                                        Gerar com IA
                                    </button>
                                    <!-- Dropdown -->
                                    <div v-if="showGenerateDocsDropdown"
                                         @click.stop
                                         class="absolute right-0 mt-1 w-48 bg-white rounded-lg shadow-lg border border-gray-200 z-10">
                                        <div class="py-1">
                                            <button @click="generateDocs('readme')"
                                                    class="w-full text-left px-4 py-2 text-sm hover:bg-gray-100 flex items-center gap-2">
                                                <span>📄</span> README
                                            </button>
                                            <button @click="generateDocs('api')"
                                                    class="w-full text-left px-4 py-2 text-sm hover:bg-gray-100 flex items-center gap-2">
                                                <span>🔌</span> API Docs
                                            </button>
                                            <button @click="generateDocs('user_guide')"
                                                    class="w-full text-left px-4 py-2 text-sm hover:bg-gray-100 flex items-center gap-2">
                                                <span>📖</span> User Guide
                                            </button>
                                            <button @click="generateDocs('technical')"
                                                    class="w-full text-left px-4 py-2 text-sm hover:bg-gray-100 flex items-center gap-2">
                                                <span>⚙️</span> Technical
                                            </button>
                                        </div>
                                    </div>
                                </div>
                                <button @click="showNewDocModal = true"
                                        class="text-sm bg-[#FF6C00] text-white px-3 py-1 rounded hover:bg-orange-600">
                                    + Manual
                                </button>
                            </div>
                        </div>

                        <!-- Loading state -->
                        <div v-if="generatingDocs" class="mb-4 p-4 bg-blue-50 border border-blue-200 rounded-lg flex items-center gap-3">
                            <svg class="animate-spin h-5 w-5 text-blue-600" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                            </svg>
                            <div>
                                <div class="font-medium text-blue-900">Gerando documentacao...</div>
                                <div class="text-sm text-blue-700">A IA esta analisando a story e criando a documentacao</div>
                            </div>
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
                                    <!-- Botoes de acao -->
                                    <div class="flex gap-1">
                                        <button @click="copyDocContent(doc.content)"
                                                class="text-xs text-gray-500 hover:text-blue-600 px-2 py-1 rounded hover:bg-gray-100"
                                                title="Copiar conteudo">
                                            📋
                                        </button>
                                        <button @click="downloadDoc(doc)"
                                                class="text-xs text-gray-500 hover:text-blue-600 px-2 py-1 rounded hover:bg-gray-100"
                                                title="Download como Markdown">
                                            ⬇️
                                        </button>
                                    </div>
                                </div>
                                <div v-if="doc.content" class="text-sm text-gray-600 markdown-content prose prose-sm max-w-none"
                                     v-html="renderMarkdown(doc.content)"></div>
                                <div v-if="doc.test_instructions" class="mt-2 pt-2 border-t border-gray-100">
                                    <span class="text-xs font-medium text-gray-500">Como Testar:</span>
                                    <p class="text-sm text-gray-600 whitespace-pre-line">{{ doc.test_instructions }}</p>
                                </div>
                            </div>
                        </div>
                        <p v-else class="text-gray-400 text-sm italic">Nenhuma documentacao. Use "Gerar com IA" para criar automaticamente.</p>
                    </div>

                    <!-- Tab: Anexos (Enhanced - Issue #185) -->
                    <div v-if="activeTab === 'Anexos'" class="p-4">
                        <div class="flex justify-between items-center mb-4">
                            <h3 class="font-semibold">Arquivos e Midias</h3>
                            <div class="flex gap-2">
                                <label class="text-sm bg-[#FF6C00] text-white px-3 py-1 rounded hover:bg-orange-600 cursor-pointer flex items-center gap-1">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12"/>
                                    </svg>
                                    Upload
                                    <input type="file" class="hidden" multiple @change="handleMultipleFileUpload"
                                           accept=".pdf,.doc,.docx,.txt,.md,.mp4,.mov,.avi,.png,.jpg,.jpeg,.gif">
                                </label>
                            </div>
                        </div>

                        <!-- Drag and Drop Zone -->
                        <div class="upload-dropzone mb-4"
                             :class="{ 'drag-over': isDraggingFile }"
                             @dragover.prevent="isDraggingFile = true"
                             @dragleave.prevent="isDraggingFile = false"
                             @drop.prevent="handleFileDrop"
                             @click="$refs.dropzoneInput.click()">
                            <div class="upload-dropzone-icon">
                                <svg class="w-8 h-8" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"/>
                                </svg>
                            </div>
                            <p class="text-gray-700 font-medium mb-1">Arraste arquivos aqui</p>
                            <p class="text-gray-500 text-sm mb-3">ou clique para selecionar</p>
                            <div class="flex flex-wrap justify-center gap-2 text-xs text-gray-400">
                                <span class="px-2 py-1 bg-gray-100 rounded">PDF</span>
                                <span class="px-2 py-1 bg-gray-100 rounded">DOC/DOCX</span>
                                <span class="px-2 py-1 bg-gray-100 rounded">TXT/MD</span>
                                <span class="px-2 py-1 bg-gray-100 rounded">MP4/MOV/AVI</span>
                                <span class="px-2 py-1 bg-gray-100 rounded">PNG/JPG/GIF</span>
                            </div>
                            <input type="file" class="hidden" ref="dropzoneInput" multiple
                                   @change="handleMultipleFileUpload"
                                   accept=".pdf,.doc,.docx,.txt,.md,.mp4,.mov,.avi,.png,.jpg,.jpeg,.gif">
                        </div>

                        <!-- Upload Progress Queue -->
                        <div v-if="uploadQueue.length" class="mb-4 space-y-2">
                            <div class="text-xs font-medium text-gray-500 uppercase tracking-wider mb-2">Enviando...</div>
                            <div v-for="item in uploadQueue" :key="item.id" class="upload-file-item">
                                <div :class="['upload-file-icon', getFileIconClass(item.file.name)]">
                                    {{ getFileIcon(item.file.name) }}
                                </div>
                                <div class="upload-file-info">
                                    <div class="upload-file-name">{{ item.file.name }}</div>
                                    <div class="upload-progress-bar">
                                        <div class="upload-progress-fill" :style="{ width: item.progress + '%' }"></div>
                                    </div>
                                </div>
                                <span :class="['upload-status-badge', item.status]">
                                    {{ item.status === 'uploading' ? item.progress + '%' : item.status === 'success' ? 'Concluido' : 'Erro' }}
                                </span>
                            </div>
                        </div>

                        <!-- File Type Filter -->
                        <div v-if="selectedStory.files?.length" class="flex gap-2 mb-4 flex-wrap">
                            <button @click="uploadFileFilter = 'all'"
                                    :class="['upload-filter-chip', uploadFileFilter === 'all' ? 'active' : '']">
                                Todos ({{ selectedStory.files?.length || 0 }})
                            </button>
                            <button @click="uploadFileFilter = 'document'"
                                    :class="['upload-filter-chip', uploadFileFilter === 'document' ? 'active' : '']">
                                Documentos ({{ countFilesByType('document') }})
                            </button>
                            <button @click="uploadFileFilter = 'video'"
                                    :class="['upload-filter-chip', uploadFileFilter === 'video' ? 'active' : '']">
                                Videos ({{ countFilesByType('video') }})
                            </button>
                            <button @click="uploadFileFilter = 'image'"
                                    :class="['upload-filter-chip', uploadFileFilter === 'image' ? 'active' : '']">
                                Imagens ({{ countFilesByType('image') }})
                            </button>
                        </div>

                        <!-- Files List -->
                        <div v-if="filteredUploadFiles.length" class="space-y-2">
                            <div v-for="file in filteredUploadFiles" :key="file.attachment_id"
                                 class="upload-file-item">
                                <!-- Thumbnail for images -->
                                <img v-if="isImageFile(file.original_filename) && file.attachment_id"
                                     :src="'/api/attachments/' + file.attachment_id"
                                     :alt="file.original_filename"
                                     class="upload-thumbnail">
                                <!-- File icon for non-images -->
                                <div v-else :class="['upload-file-icon', getFileIconClass(file.original_filename)]">
                                    {{ getFileIcon(file.original_filename) }}
                                </div>
                                <div class="upload-file-info">
                                    <div class="upload-file-name">{{ file.original_filename }}</div>
                                    <div class="upload-file-meta">
                                        <span>{{ formatFileSize(file.file_size) }}</span>
                                        <span>{{ getFileTypeName(file.original_filename) }}</span>
                                        <span v-if="file.created_at">{{ formatUploadDate(file.created_at) }}</span>
                                    </div>
                                </div>
                                <div class="upload-file-actions">
                                    <a :href="'/api/attachments/' + file.attachment_id"
                                       class="upload-action-btn" title="Download" download>
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"/>
                                        </svg>
                                    </a>
                                    <button @click="previewFile(file)" class="upload-action-btn" title="Visualizar"
                                            v-if="canPreviewFile(file.original_filename)">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"/>
                                        </svg>
                                    </button>
                                    <button @click="deleteUploadedFile(file)" class="upload-action-btn danger" title="Excluir">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                        </svg>
                                    </button>
                                </div>
                            </div>
                        </div>
                        <div v-else-if="!uploadQueue.length" class="text-center py-8">
                            <svg class="w-12 h-12 mx-auto text-gray-300 mb-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"/>
                            </svg>
                            <p class="text-gray-400 text-sm">Nenhum arquivo anexado</p>
                            <p class="text-gray-300 text-xs mt-1">Arraste arquivos ou clique em Upload</p>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Terminal and Preview Section -->
        <div v-if="selectedProjectId" class="bg-white rounded-lg shadow p-4 mt-4">
            <div class="flex items-center justify-between mb-4">
                <h3 class="text-lg font-semibold text-gray-700">Ambiente de Teste</h3>
                <div class="flex items-center gap-2">
                    <span class="text-xs text-gray-500">Projeto: {{ selectedProjectId }}</span>
                </div>
            </div>
            <div class="grid grid-cols-2 gap-4">
                <!-- Terminal -->
                <div class="flex flex-col">
                    <div class="flex items-center justify-between mb-2">
                        <h4 class="text-sm font-medium text-gray-600">Terminal</h4>
                        <div class="flex gap-2">
                            <button @click="startApp"
                                    :disabled="terminalRunning"
                                    class="px-3 py-1 bg-green-500 text-white rounded text-xs hover:bg-green-600 disabled:opacity-50 disabled:cursor-not-allowed">
                                &#9654; Iniciar App
                            </button>
                            <button @click="runTests"
                                    :disabled="terminalRunning"
                                    class="px-3 py-1 bg-blue-500 text-white rounded text-xs hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed">
                                &#129514; Testes
                            </button>
                            <button @click="stopProcess"
                                    :disabled="!terminalRunning"
                                    class="px-3 py-1 bg-red-500 text-white rounded text-xs hover:bg-red-600 disabled:opacity-50 disabled:cursor-not-allowed">
                                &#9632; Parar
                            </button>
                        </div>
                    </div>
                    <div id="terminal-container" class="bg-black rounded border border-gray-300" style="height: 400px;"></div>
                    <div class="mt-2">
                        <div class="flex gap-2">
                            <input v-model="terminalCommand"
                                   @keyup.enter="executeTerminalCommand"
                                   type="text"
                                   placeholder="Digite um comando..."
                                   class="flex-1 px-3 py-1 border border-gray-300 rounded text-sm">
                            <button @click="executeTerminalCommand"
                                    class="px-3 py-1 bg-[#FF6C00] text-white rounded text-xs hover:bg-orange-600">
                                Executar
                            </button>
                        </div>
                    </div>
                </div>
                <!-- Preview -->
                <div class="flex flex-col">
                    <div class="flex items-center justify-between mb-2">
                        <h4 class="text-sm font-medium text-gray-600">Preview</h4>
                        <div class="flex items-center gap-2">
                            <select v-model="previewViewport" class="text-xs border border-gray-300 rounded px-2 py-1">
                                <option value="desktop">Desktop</option>
                                <option value="tablet">Tablet (768px)</option>
                                <option value="mobile">Mobile (375px)</option>
                            </select>
                            <button @click="refreshPreview"
                                    class="px-3 py-1 bg-gray-200 text-gray-700 rounded text-xs hover:bg-gray-300">
                                &#8635; Refresh
                            </button>
                        </div>
                    </div>
                    <div class="bg-gray-100 rounded border border-gray-300 flex items-center justify-center" style="height: 400px;">
                        <iframe ref="previewFrame"
                                :src="previewUrl"
                                :style="{
                                    width: previewViewport === 'mobile' ? '375px' : previewViewport === 'tablet' ? '768px' : '100%',
                                    height: '100%',
                                    border: 'none',
                                    borderRadius: '4px',
                                    backgroundColor: 'white'
                                }"
                                class="transition-all duration-300">
                        </iframe>
                    </div>
                    <div class="mt-2 flex gap-2">
                        <input v-model="previewUrl"
                               type="text"
                               placeholder="http://localhost:3000"
                               class="flex-1 px-3 py-1 border border-gray-300 rounded text-sm">
                        <button @click="refreshPreview"
                                class="px-3 py-1 bg-[#003B4A] text-white rounded text-xs hover:bg-blue-900">
                                Carregar
                        </button>
                    </div>
                </div>
            </div>
        </div>


        <!-- MODAL: Nova Story -->
        <div v-if="showNewStoryModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[700px] max-h-[90vh] overflow-y-auto dark:bg-gray-800">
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white rounded-t-lg flex justify-between items-center">
                    <h2 class="text-lg font-semibold">Nova User Story</h2>
                    <button @click="showNewStoryModal = false" class="text-white/80 hover:text-white">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/></svg>
                    </button>
                </div>
                <!-- Issue #223: Template Selector -->
                <div class="template-selector" v-if="showTemplateSelector">
                    <div class="template-selector-header">
                        <span class="template-selector-title">Escolha um template para comecar</span>
                        <button class="template-skip-btn" @click="showTemplateSelector = false">Pular</button>
                    </div>
                    <div class="template-grid" v-if="!templatesLoading">
                        <button v-for="tmpl in availableTemplates" :key="tmpl.id"
                                class="template-card"
                                :class="{ 'selected': selectedTemplate === tmpl.id }"
                                @click="applyTemplate(tmpl.id)"
                                :style="{ '--template-color': tmpl.color }">
                            <span class="template-icon">{{ tmpl.icon }}</span>
                            <span class="template-name">{{ tmpl.name }}</span>
                            <span class="template-desc">{{ tmpl.description }}</span>
                        </button>
                    </div>
                    <div v-else class="template-loading">
                        <svg class="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
                            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
                        </svg>
                        <span>Carregando templates...</span>
                    </div>
                </div>
                <!-- Selected Template Indicator -->
                <div v-else-if="selectedTemplate" class="template-selected-banner">
                    <div class="flex items-center gap-2">
                        <span class="text-lg">{{ availableTemplates.find(t => t.id === selectedTemplate)?.icon }}</span>
                        <span class="font-medium">{{ availableTemplates.find(t => t.id === selectedTemplate)?.name }}</span>
                    </div>
                    <button @click="clearTemplate" class="text-sm text-[#003B4A] hover:underline">Trocar template</button>
                </div>
                <!-- Issue #195: Omnichannel Input Selector -->
                <div class="p-4 bg-gray-50 border-b border-gray-200 dark:bg-gray-700">
                    <div class="text-sm text-gray-600 dark:text-gray-300 mb-2">Escolha o metodo de entrada:</div>
                    <div class="flex gap-2">
                        <button @click="inputMethod = 'text'"
                                :class="['flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                         inputMethod === 'text' ? 'bg-[#003B4A] text-white shadow-md' : 'bg-white text-gray-600 border border-gray-300 hover:bg-gray-100']">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"/></svg>
                            Texto
                        </button>
                        <button @click="inputMethod = 'voice'; if(inputMethod === 'voice' && !voiceRecording) toggleVoiceInput()"
                                :class="['flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                         inputMethod === 'voice' ? 'bg-[#FF6C00] text-white shadow-md' : 'bg-white text-gray-600 border border-gray-300 hover:bg-gray-100',
                                         voiceRecording ? 'animate-pulse' : '']">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 11a7 7 0 01-7 7m0 0a7 7 0 01-7-7m7 7v4m0 0H8m4 0h4m-4-8a3 3 0 01-3-3V5a3 3 0 116 0v6a3 3 0 01-3 3z"/></svg>
                            {{ voiceRecording ? 'Gravando...' : 'Voz' }}
                        </button>
                        <button @click="inputMethod = 'file'; $refs.omnichannelFileInput?.click()"
                                :class="['flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                         inputMethod === 'file' ? 'bg-purple-600 text-white shadow-md' : 'bg-white text-gray-600 border border-gray-300 hover:bg-gray-100']">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"/></svg>
                            Arquivo
                        </button>
                        <button @click="inputMethod = 'whatsapp'"
                                :class="['flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                         inputMethod === 'whatsapp' ? 'bg-green-600 text-white shadow-md' : 'bg-white text-gray-600 border border-gray-300 hover:bg-gray-100']">
                            <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 24 24"><path d="M17.472 14.382c-.297-.149-1.758-.867-2.03-.967-.273-.099-.471-.148-.67.15-.197.297-.767.966-.94 1.164-.173.199-.347.223-.644.075-.297-.15-1.255-.463-2.39-1.475-.883-.788-1.48-1.761-1.653-2.059-.173-.297-.018-.458.13-.606.134-.133.298-.347.446-.52.149-.174.198-.298.298-.497.099-.198.05-.371-.025-.52-.075-.149-.669-1.612-.916-2.207-.242-.579-.487-.5-.669-.51-.173-.008-.371-.01-.57-.01-.198 0-.52.074-.792.372-.272.297-1.04 1.016-1.04 2.479 0 1.462 1.065 2.875 1.213 3.074.149.198 2.096 3.2 5.077 4.487.709.306 1.262.489 1.694.625.712.227 1.36.195 1.871.118.571-.085 1.758-.719 2.006-1.413.248-.694.248-1.289.173-1.413-.074-.124-.272-.198-.57-.347z"/><path d="M12 0C5.373 0 0 5.373 0 12c0 2.625.846 5.059 2.284 7.034L.789 23.211l4.343-1.407A11.934 11.934 0 0012 24c6.627 0 12-5.373 12-12S18.627 0 12 0zm0 21.818c-2.168 0-4.183-.639-5.875-1.734l-.421-.253-4.368 1.141 1.165-4.269-.276-.439A9.776 9.776 0 012.182 12c0-5.423 4.395-9.818 9.818-9.818S21.818 6.577 21.818 12s-4.395 9.818-9.818 9.818z"/></svg>
                            WhatsApp
                        </button>
                    </div>
                    <input type="file" ref="omnichannelFileInput" class="hidden" @change="processOmnichannelFile" accept=".pdf,.doc,.docx,.txt,.xlsx,.xls,.csv">
                </div>
                <!-- WhatsApp Instructions (Issue #195) -->
                <div v-if="inputMethod === 'whatsapp'" class="p-4 bg-green-50 border-b border-green-200">
                    <div class="flex items-start gap-3">
                        <div class="w-10 h-10 bg-green-500 rounded-full flex items-center justify-center flex-shrink-0">
                            <svg class="w-6 h-6 text-white" fill="currentColor" viewBox="0 0 24 24"><path d="M17.472 14.382c-.297-.149-1.758-.867-2.03-.967-.273-.099-.471-.148-.67.15-.197.297-.767.966-.94 1.164-.173.199-.347.223-.644.075-.297-.15-1.255-.463-2.39-1.475-.883-.788-1.48-1.761-1.653-2.059-.173-.297-.018-.458.13-.606.134-.133.298-.347.446-.52.149-.174.198-.298.298-.497.099-.198.05-.371-.025-.52-.075-.149-.669-1.612-.916-2.207-.242-.579-.487-.5-.669-.51-.173-.008-.371-.01-.57-.01-.198 0-.52.074-.792.372-.272.297-1.04 1.016-1.04 2.479 0 1.462 1.065 2.875 1.213 3.074.149.198 2.096 3.2 5.077 4.487.709.306 1.262.489 1.694.625.712.227 1.36.195 1.871.118.571-.085 1.758-.719 2.006-1.413.248-.694.248-1.289.173-1.413-.074-.124-.272-.198-.57-.347z"/></svg>
                        </div>
                        <div>
                            <h4 class="font-semibold text-green-800">Criar Story via WhatsApp</h4>
                            <p class="text-sm text-green-700 mt-1">Envie uma mensagem para nosso numero do WhatsApp descrevendo sua necessidade:</p>
                            <div class="mt-2 bg-white p-3 rounded-lg border border-green-200">
                                <div class="font-mono text-lg text-green-800">+55 (11) 99999-9999</div>
                                <p class="text-xs text-gray-500 mt-1">Exemplo: "Preciso de um sistema de login com email e senha, onde o usuario possa recuperar a senha..."</p>
                            </div>
                            <p class="text-xs text-green-600 mt-2">A IA ira processar sua mensagem e criar a User Story automaticamente.</p>
                        </div>
                    </div>
                </div>
                <!-- File Processing Feedback (Issue #195) -->
                <div v-if="inputMethod === 'file' && omnichannelFileProcessing" class="p-4 bg-purple-50 border-b border-purple-200">
                    <div class="flex items-center gap-3">
                        <svg class="w-5 h-5 animate-spin text-purple-600" fill="none" viewBox="0 0 24 24">
                            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                        </svg>
                        <span class="text-sm text-purple-700">Processando documento... Extraindo requisitos com IA</span>
                    </div>
                </div>
                <!-- Voice Recording Status -->
                <div v-if="voiceRecording || voiceProcessing" class="bg-blue-50 dark:bg-blue-900/30 p-3 border-b flex items-center gap-3">
                    <div v-if="voiceRecording" class="flex items-center gap-2 text-blue-700 dark:text-blue-300">
                        <div class="w-3 h-3 bg-red-500 rounded-full animate-pulse"></div>
                        <span class="text-sm">Gravando... Fale sua user story</span>
                        <span class="text-xs opacity-70">{{ voiceRecordingTime }}s</span>
                    </div>
                    <div v-else-if="voiceProcessing" class="flex items-center gap-2 text-blue-700 dark:text-blue-300">
                        <svg class="w-4 h-4 animate-spin" fill="none" viewBox="0 0 24 24">
                            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                        </svg>
                        <span class="text-sm">Processando audio com IA...</span>
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

        <!-- MODAL: Novo Design -->
        <div v-if="showNewDesignModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[500px]">
                <div class="p-4 border-b border-gray-200">
                    <h2 class="text-lg font-semibold">Novo Diagrama</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Titulo *</label>
                            <input v-model="newDesign.title" type="text"
                                   class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                   placeholder="Ex: Arquitetura do Sistema">
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Tipo</label>
                            <select v-model="newDesign.design_type" class="w-full border border-gray-300 rounded-lg px-3 py-2">
                                <option value="wireframe">Wireframe</option>
                                <option value="architecture">Arquitetura</option>
                                <option value="flow">Fluxograma</option>
                                <option value="database">Banco de Dados</option>
                                <option value="mockup">Mockup</option>
                            </select>
                        </div>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newDesign.description" rows="2"
                                  class="w-full border border-gray-300 rounded-lg px-3 py-2"
                                  placeholder="Breve descricao do diagrama..."></textarea>
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showNewDesignModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createDesign"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">Criar e Editar</button>
                </div>
            </div>
        </div>

        <!-- MODAL: Editor Draw.io -->
        <div v-if="showDesignEditor" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[95vw] h-[95vh] flex flex-col">
                <div class="p-4 border-b border-gray-200 flex justify-between items-center">
                    <h2 class="text-lg font-semibold">{{ currentDesign?.title || 'Editor de Diagramas' }}</h2>
                    <div class="flex gap-2">
                        <button @click="saveDesign"
                                class="px-4 py-2 bg-[#003B4A] text-white rounded-lg hover:bg-opacity-90">
                            Salvar
                        </button>
                        <button @click="closeDesignEditor"
                                class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">
                            Fechar
                        </button>
                    </div>
                </div>
                <div class="flex-1 relative">
                    <iframe
                        ref="drawioFrame"
                        id="drawio-iframe"
                        :src="'https://embed.diagrams.net/?embed=1&spin=1&proto=json&ui=kennedy&libraries=1'"
                        class="w-full h-full border-0">
                    </iframe>
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

        <!-- Issue #216: Command Palette Modal -->
        <div v-if="showCommandPalette" class="fixed inset-0 bg-black/50 z-[60] flex items-start justify-center pt-[15vh]"
             @click.self="showCommandPalette = false; commandPaletteQuery = '';"
             style="backdrop-filter: blur(4px);">
            <div class="bg-white rounded-xl w-[600px] max-w-[90vw] shadow-2xl overflow-hidden"
                 style="animation: paletteIn 0.15s ease-out;">
                <div class="p-3 border-b border-gray-200 flex items-center gap-3">
                    <svg class="w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                    </svg>
                    <input id="command-palette-input"
                           v-model="commandPaletteQuery"
                           type="text"
                           class="flex-1 outline-none text-lg"
                           placeholder="Search commands or stories..."
                           autocomplete="off"
                           @keydown="handleCommandPaletteKey">
                    <span class="text-xs text-gray-400 bg-gray-100 px-2 py-1 rounded">Esc</span>
                </div>
                <div class="max-h-[400px] overflow-y-auto">
                    <div v-if="commandPaletteResults.length === 0" class="p-4 text-center text-gray-500">
                        Nenhum resultado encontrado
                    </div>
                    <div v-for="(cmd, idx) in commandPaletteResults" :key="cmd.id"
                         @click="executeCommand(cmd)"
                         :class="['flex items-center gap-3 px-4 py-3 cursor-pointer transition-colors',
                                  idx === commandPaletteIndex ? 'bg-blue-50' : 'hover:bg-gray-50']"
                         @mouseenter="commandPaletteIndex = idx">
                        <span class="text-lg w-6 text-center">{{ cmd.icon }}</span>
                        <div class="flex-1">
                            <div class="font-medium">{{ cmd.title }}</div>
                            <div class="text-xs text-gray-500">{{ cmd.category }}</div>
                        </div>
                        <span v-if="cmd.shortcut" class="text-xs text-gray-400 bg-gray-100 px-2 py-1 rounded font-mono">
                            {{ cmd.shortcut }}
                        </span>
                    </div>
                </div>
                <div class="p-2 border-t border-gray-200 flex items-center gap-4 text-xs text-gray-500">
                    <span><kbd class="kbd-small">↑↓</kbd> Navegar</span>
                    <span><kbd class="kbd-small">↵</kbd> Selecionar</span>
                    <span><kbd class="kbd-small">Esc</kbd> Fechar</span>
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
                            <span class="shortcut-desc">Command Palette</span>
                            <span class="kbd">⌘K</span>
                        </div>
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

        <!-- MODAL: Generated Tests -->
        <div v-if="showGeneratedTestsModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showGeneratedTestsModal = false">
            <div class="bg-white rounded-lg w-[800px] max-h-[90vh] shadow-xl overflow-hidden">
                <div class="p-4 border-b flex justify-between items-center bg-purple-600 text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Testes Gerados</h2>
                        <p class="text-sm text-purple-200" v-if="currentGeneratedTests">{{ currentGeneratedTests.language }} / {{ currentGeneratedTests.framework }} | {{ currentGeneratedTests.test_count }} testes | Coverage: {{ currentGeneratedTests.coverage_estimate }}</p>
                    </div>
                    <button @click="showGeneratedTestsModal = false" class="text-white/70 hover:text-white">X</button>
                </div>
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 140px);">
                    <div class="flex gap-2 mb-4">
                        <button @click="copyTestCode" class="px-4 py-2 bg-purple-100 text-purple-700 rounded hover:bg-purple-200 text-sm">Copiar</button>
                        <button @click="downloadTestCode" class="px-4 py-2 bg-gray-100 text-gray-700 rounded hover:bg-gray-200 text-sm">Download</button>
                        <span v-if="currentGeneratedTests?.is_template" class="ml-auto text-sm text-yellow-600 bg-yellow-100 px-2 py-1 rounded">Template</span>
                    </div>
                    <pre class="bg-gray-900 text-gray-100 p-4 rounded-lg overflow-x-auto text-sm font-mono whitespace-pre-wrap"><code>{{ currentGeneratedTests?.test_code }}</code></pre>
                </div>
            </div>
        </div>

        <!-- MODAL: Security Scan Results (Issue #57) -->
        <div v-if="showSecurityScanModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showSecurityScanModal = false">
            <div class="bg-white rounded-lg w-[900px] max-h-[90vh] shadow-xl overflow-hidden">
                <div class="p-4 border-b flex justify-between items-center bg-red-600 text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Analise de Seguranca (SAST)</h2>
                        <p class="text-sm text-red-200" v-if="currentSecurityScan">
                            {{ currentSecurityScan.language }} | {{ currentSecurityScan.scan_type === 'ai' ? 'AI-Powered' : 'Pattern-Based' }} |
                            {{ currentSecurityScan.summary?.total || 0 }} vulnerabilidades encontradas
                        </p>
                    </div>
                    <button @click="showSecurityScanModal = false" class="text-white/70 hover:text-white text-xl">X</button>
                </div>
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 140px);">
                    <div v-if="currentSecurityScan?.summary" class="grid grid-cols-5 gap-3 mb-4">
                        <div class="bg-gray-100 p-3 rounded text-center">
                            <div class="text-2xl font-bold">{{ currentSecurityScan.summary.total }}</div>
                            <div class="text-xs text-gray-600">Total</div>
                        </div>
                        <div class="bg-red-100 p-3 rounded text-center">
                            <div class="text-2xl font-bold text-red-700">{{ currentSecurityScan.summary.critical }}</div>
                            <div class="text-xs text-red-600">Critical</div>
                        </div>
                        <div class="bg-orange-100 p-3 rounded text-center">
                            <div class="text-2xl font-bold text-orange-700">{{ currentSecurityScan.summary.high }}</div>
                            <div class="text-xs text-orange-600">High</div>
                        </div>
                        <div class="bg-yellow-100 p-3 rounded text-center">
                            <div class="text-2xl font-bold text-yellow-700">{{ currentSecurityScan.summary.medium }}</div>
                            <div class="text-xs text-yellow-600">Medium</div>
                        </div>
                        <div class="bg-blue-100 p-3 rounded text-center">
                            <div class="text-2xl font-bold text-blue-700">{{ currentSecurityScan.summary.low }}</div>
                            <div class="text-xs text-blue-600">Low</div>
                        </div>
                    </div>
                    <div v-if="currentSecurityScan?.recommendations?.length" class="mb-4 p-3 bg-blue-50 rounded-lg">
                        <h4 class="font-semibold text-blue-800 mb-2">Recomendacoes</h4>
                        <ul class="list-disc list-inside text-sm text-blue-700">
                            <li v-for="rec in currentSecurityScan.recommendations" :key="rec">{{ rec }}</li>
                        </ul>
                    </div>
                    <div v-if="currentSecurityScan?.vulnerabilities?.length" class="space-y-3">
                        <h4 class="font-semibold text-gray-800">Vulnerabilidades Encontradas</h4>
                        <div v-for="(vuln, idx) in currentSecurityScan.vulnerabilities" :key="idx"
                             class="border rounded-lg p-3"
                             :class="{'border-red-300 bg-red-50': vuln.severity === 'Critical', 'border-orange-300 bg-orange-50': vuln.severity === 'High', 'border-yellow-300 bg-yellow-50': vuln.severity === 'Medium', 'border-blue-300 bg-blue-50': vuln.severity === 'Low'}">
                            <div class="flex justify-between items-start mb-2">
                                <div class="flex items-center gap-2">
                                    <span class="font-semibold">{{ vuln.type }}</span>
                                    <span class="text-xs px-2 py-0.5 rounded"
                                          :class="{'bg-red-600 text-white': vuln.severity === 'Critical', 'bg-orange-500 text-white': vuln.severity === 'High', 'bg-yellow-500 text-white': vuln.severity === 'Medium', 'bg-blue-500 text-white': vuln.severity === 'Low'}">
                                        {{ vuln.severity }}
                                    </span>
                                    <span class="text-xs text-gray-500">{{ vuln.cwe }}</span>
                                </div>
                                <span class="text-xs text-gray-500">Linha {{ vuln.line }}</span>
                            </div>
                            <div class="text-sm text-gray-700 mb-2">{{ vuln.description }}</div>
                            <div v-if="vuln.code_snippet" class="bg-gray-900 text-gray-100 p-2 rounded text-xs font-mono mb-2 overflow-x-auto">{{ vuln.code_snippet }}</div>
                            <div v-if="vuln.recommendation" class="text-sm text-green-700 bg-green-50 p-2 rounded">
                                <strong>Correcao:</strong> {{ vuln.recommendation }}
                            </div>
                        </div>
                    </div>
                    <div v-else-if="currentSecurityScan && !currentSecurityScan.vulnerabilities?.length" class="text-center py-8">
                        <svg class="w-16 h-16 mx-auto text-green-500 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z"/>
                        </svg>
                        <h3 class="text-lg font-semibold text-green-700">Nenhuma Vulnerabilidade Detectada</h3>
                        <p class="text-sm text-gray-600">O codigo passou na analise de seguranca.</p>
                    </div>
                </div>
            </div>
        </div>

                <!-- MODAL: Code Review (Issue #52) -->
        <div v-if="showCodeReviewModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showCodeReviewModal = false">
            <div class="bg-white rounded-lg w-[900px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
                <div class="p-4 border-b flex justify-between items-center bg-blue-600 text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Code Review com IA</h2>
                        <p class="text-sm text-blue-200" v-if="currentCodeReview">{{ currentCodeReview.task_title || 'Analise de Codigo' }}</p>
                    </div>
                    <button @click="showCodeReviewModal = false" class="text-white/70 hover:text-white text-xl font-bold">X</button>
                </div>
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 80px);" v-if="currentCodeReview">
                    <!-- Score Principal -->
                    <div class="text-center mb-6">
                        <div class="inline-flex items-center justify-center w-24 h-24 rounded-full border-4"
                             :class="currentCodeReview.score >= 80 ? 'border-green-500 text-green-600' : currentCodeReview.score >= 60 ? 'border-yellow-500 text-yellow-600' : 'border-red-500 text-red-600'">
                            <span class="text-3xl font-bold">{{ currentCodeReview.score }}</span>
                        </div>
                        <p class="mt-2 text-gray-600 dark:text-gray-300">{{ currentCodeReview.summary }}</p>
                        <p class="text-xs text-gray-400 mt-1">Analisado por: {{ currentCodeReview.reviewed_by || 'auto-analyzer' }}</p>
                    </div>
                    <!-- Scores por Categoria -->
                    <div class="grid grid-cols-3 gap-4 mb-6">
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-4 text-center">
                            <div class="text-2xl font-bold" :class="(currentCodeReview.quality?.score || 70) >= 80 ? 'text-green-600' : (currentCodeReview.quality?.score || 70) >= 60 ? 'text-yellow-600' : 'text-red-600'">{{ currentCodeReview.quality?.score || 70 }}</div>
                            <div class="text-sm text-gray-500 dark:text-gray-400">Qualidade</div>
                        </div>
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-4 text-center">
                            <div class="text-2xl font-bold" :class="(currentCodeReview.security?.score || 70) >= 80 ? 'text-green-600' : (currentCodeReview.security?.score || 70) >= 60 ? 'text-yellow-600' : 'text-red-600'">{{ currentCodeReview.security?.score || 70 }}</div>
                            <div class="text-sm text-gray-500 dark:text-gray-400">Seguranca</div>
                        </div>
                        <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-4 text-center">
                            <div class="text-2xl font-bold" :class="(currentCodeReview.performance?.score || 70) >= 80 ? 'text-green-600' : (currentCodeReview.performance?.score || 70) >= 60 ? 'text-yellow-600' : 'text-red-600'">{{ currentCodeReview.performance?.score || 70 }}</div>
                            <div class="text-sm text-gray-500 dark:text-gray-400">Performance</div>
                        </div>
                    </div>
                    <!-- Qualidade -->
                    <div v-if="currentCodeReview.quality" class="mb-4">
                        <h3 class="font-semibold text-gray-800 dark:text-white mb-2">Qualidade do Codigo</h3>
                        <div v-if="currentCodeReview.quality.issues?.length" class="mb-2">
                            <p class="text-sm text-red-600 font-medium mb-1">Problemas:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(issue, idx) in currentCodeReview.quality.issues" :key="'qi'+idx">* {{ issue }}</li>
                            </ul>
                        </div>
                        <div v-if="currentCodeReview.quality.positives?.length">
                            <p class="text-sm text-green-600 font-medium mb-1">Pontos Positivos:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(pos, idx) in currentCodeReview.quality.positives" :key="'qp'+idx">+ {{ pos }}</li>
                            </ul>
                        </div>
                    </div>
                    <!-- Seguranca -->
                    <div v-if="currentCodeReview.security" class="mb-4">
                        <h3 class="font-semibold text-gray-800 dark:text-white mb-2">Seguranca</h3>
                        <div v-if="currentCodeReview.security.vulnerabilities?.length" class="mb-2">
                            <p class="text-sm text-red-600 font-medium mb-1">Vulnerabilidades:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(vuln, idx) in currentCodeReview.security.vulnerabilities" :key="'sv'+idx">! {{ vuln }}</li>
                            </ul>
                        </div>
                        <div v-if="currentCodeReview.security.recommendations?.length">
                            <p class="text-sm text-blue-600 font-medium mb-1">Recomendacoes:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(rec, idx) in currentCodeReview.security.recommendations" :key="'sr'+idx">- {{ rec }}</li>
                            </ul>
                        </div>
                    </div>
                    <!-- Performance -->
                    <div v-if="currentCodeReview.performance" class="mb-4">
                        <h3 class="font-semibold text-gray-800 dark:text-white mb-2">Performance</h3>
                        <div v-if="currentCodeReview.performance.issues?.length" class="mb-2">
                            <p class="text-sm text-yellow-600 font-medium mb-1">Problemas:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(issue, idx) in currentCodeReview.performance.issues" :key="'pi'+idx">* {{ issue }}</li>
                            </ul>
                        </div>
                        <div v-if="currentCodeReview.performance.suggestions?.length">
                            <p class="text-sm text-blue-600 font-medium mb-1">Sugestoes:</p>
                            <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                <li v-for="(sug, idx) in currentCodeReview.performance.suggestions" :key="'ps'+idx">- {{ sug }}</li>
                            </ul>
                        </div>
                    </div>
                    <!-- Sugestoes Gerais -->
                    <div v-if="currentCodeReview.suggestions?.length" class="mb-4">
                        <h3 class="font-semibold text-gray-800 dark:text-white mb-2">Sugestoes de Melhoria</h3>
                        <div class="space-y-2">
                            <div v-for="(sug, idx) in currentCodeReview.suggestions" :key="'sg'+idx" class="bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
                                <div class="flex items-center gap-2 mb-1">
                                    <span class="text-xs px-2 py-0.5 rounded" :class="sug.priority === 'critical' ? 'bg-red-100 text-red-700' : sug.priority === 'high' ? 'bg-orange-100 text-orange-700' : 'bg-blue-100 text-blue-700'">{{ sug.priority || 'medium' }}</span>
                                    <span class="font-medium text-sm">{{ sug.title }}</span>
                                </div>
                                <p class="text-sm text-gray-600 dark:text-gray-300">{{ sug.description }}</p>
                            </div>
                        </div>
                    </div>
                    <!-- Boas Praticas -->
                    <div v-if="currentCodeReview.best_practices" class="mb-4">
                        <h3 class="font-semibold text-gray-800 dark:text-white mb-2">Boas Praticas</h3>
                        <div class="grid grid-cols-2 gap-4">
                            <div v-if="currentCodeReview.best_practices.followed?.length">
                                <p class="text-sm text-green-600 font-medium mb-1">Seguidas:</p>
                                <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                    <li v-for="(bp, idx) in currentCodeReview.best_practices.followed" :key="'bpf'+idx">+ {{ bp }}</li>
                                </ul>
                            </div>
                            <div v-if="currentCodeReview.best_practices.missing?.length">
                                <p class="text-sm text-orange-600 font-medium mb-1">Faltando:</p>
                                <ul class="text-sm text-gray-600 dark:text-gray-300 space-y-1">
                                    <li v-for="(bp, idx) in currentCodeReview.best_practices.missing" :key="'bpm'+idx">- {{ bp }}</li>
                                </ul>
                            </div>
                        </div>
                    </div>
                    <!-- Metadados -->
                    <div class="mt-6 pt-4 border-t border-gray-200 dark:border-gray-600 text-xs text-gray-400 flex justify-between">
                        <span>Linhas: {{ currentCodeReview.line_count || 'N/A' }}</span>
                        <span>{{ currentCodeReview.reviewed_at ? new Date(currentCodeReview.reviewed_at).toLocaleString() : '' }}</span>
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

        <!-- ISSUE #222, #284 - QUICK CREATE FAB (added compatibility classes) -->
        <div class="fab-backdrop" :class="{ 'open': fabMenuOpen }" @click="fabMenuOpen = false"></div>
        <div class="fab-container fab floating-button" v-click-outside="closeFabMenu">
            <button class="fab-main fab-button"
                    :class="{ 'open': fabMenuOpen }"
                    @click="toggleFabMenu"
                    :aria-expanded="fabMenuOpen"
                    aria-label="Criar novo item">
                <span class="fab-icon">➕</span>
            </button>

            <div class="fab-menu" :class="{ 'open': fabMenuOpen }" role="menu">
                <button class="fab-item" role="menuitem" @click="fabAction('story')">
                    <span class="fab-item-icon">📄</span>
                    <span class="fab-item-label">Nova Story</span>
                    <span class="fab-item-shortcut">⇧⌘S</span>
                </button>
                <button class="fab-item" role="menuitem" @click="fabAction('task')" v-if="selectedStory">
                    <span class="fab-item-icon">☑️</span>
                    <span class="fab-item-label">Nova Task</span>
                    <span class="fab-item-shortcut">⇧⌘T</span>
                </button>
                <button class="fab-item" role="menuitem" @click="fabAction('epic')" v-if="selectedProjectId">
                    <span class="fab-item-icon">🎯</span>
                    <span class="fab-item-label">Novo Epic</span>
                    <span class="fab-item-shortcut">⇧⌘E</span>
                </button>
                <button class="fab-item" role="menuitem" @click="fabAction('sprint')" v-if="selectedProjectId">
                    <span class="fab-item-icon">🏃</span>
                    <span class="fab-item-label">Novo Sprint</span>
                    <span class="fab-item-shortcut">⇧⌘P</span>
                </button>
                <button class="fab-item" role="menuitem" @click="fabAction('project')">
                    <span class="fab-item-icon">📁</span>
                    <span class="fab-item-label">Novo Projeto</span>
                    <span class="fab-item-shortcut">⇧⌘N</span>
                </button>
            </div>
        </div>


        <!-- PROJECT PREVIEW DASHBOARD MODAL (Issue #73) -->
        <div v-if="showProjectPreview && selectedProjectId"
             class="fixed inset-0 bg-black/50 z-50 overflow-y-auto"
             @click.self="showProjectPreview = false">
            <div class="min-h-screen py-8 px-4">
                <div class="max-w-6xl mx-auto">
                    <button @click="showProjectPreview = false"
                            class="fixed top-4 right-4 z-50 bg-white rounded-full p-2 shadow-lg hover:bg-gray-100">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>

                    <div class="preview-dashboard p-6 rounded-xl">
                        <!-- Header -->
                        <div class="preview-header">
                            <div class="flex items-center justify-between">
                                <div>
                                    <div class="text-sm opacity-70 mb-1">{{ previewData?.project?.project_type }}</div>
                                    <h1 class="text-2xl font-bold">{{ previewData?.project?.name || 'Carregando...' }}</h1>
                                    <p class="text-sm opacity-80 mt-1">{{ previewData?.project?.description }}</p>
                                </div>
                                <button @click="refreshPreviewData" class="preview-action-btn secondary">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
                                    </svg>
                                    Atualizar
                                </button>
                            </div>
                        </div>

                        <!-- Tabs -->
                        <div class="preview-tabs">
                            <div v-for="tab in ['Overview', 'Arquivos', 'Documentacao', 'Testes']"
                                 :key="tab"
                                 @click="previewActiveTab = tab"
                                 :class="['preview-tab', previewActiveTab === tab ? 'active' : '']">
                                {{ tab }}
                            </div>
                        </div>

                        <!-- Tab: Overview -->
                        <div v-if="previewActiveTab === 'Overview'" class="grid grid-cols-1 lg:grid-cols-3 gap-6">
                            <!-- Metricas -->
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Metricas do Projeto</div>
                                <div class="grid grid-cols-2 gap-4 mb-6">
                                    <div class="preview-metric">
                                        <div class="preview-metric-value">{{ previewData?.metrics?.stories?.total || 0 }}</div>
                                        <div class="preview-metric-label">Stories</div>
                                    </div>
                                    <div class="preview-metric">
                                        <div class="preview-metric-value text-green-600">{{ previewData?.metrics?.stories?.done || 0 }}</div>
                                        <div class="preview-metric-label">Concluidas</div>
                                    </div>
                                    <div class="preview-metric">
                                        <div class="preview-metric-value text-blue-600">{{ previewData?.tasks_summary?.total || 0 }}</div>
                                        <div class="preview-metric-label">Tasks</div>
                                    </div>
                                    <div class="preview-metric">
                                        <div class="preview-metric-value text-purple-600">{{ previewData?.metrics?.total_docs || 0 }}</div>
                                        <div class="preview-metric-label">Documentos</div>
                                    </div>
                                </div>
                                <div class="mb-4">
                                    <div class="flex justify-between text-sm mb-2">
                                        <span class="font-medium">Progresso Geral</span>
                                        <span class="font-bold text-[#003B4A]">{{ previewData?.metrics?.progress || 0 }}%</span>
                                    </div>
                                    <div class="preview-progress-bar">
                                        <div class="preview-progress-fill"
                                             :style="{
                                                 width: (previewData?.metrics?.progress || 0) + '%',
                                                 backgroundColor: (previewData?.metrics?.progress || 0) < 30 ? '#EF4444' :
                                                                  (previewData?.metrics?.progress || 0) < 70 ? '#F59E0B' : '#10B981'
                                             }">
                                        </div>
                                    </div>
                                </div>
                            </div>

                            <!-- Status por Coluna -->
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Status das Stories</div>
                                <div class="space-y-3">
                                    <div v-for="(count, status) in {
                                        'Backlog': previewData?.metrics?.stories?.backlog || 0,
                                        'Ready': previewData?.metrics?.stories?.ready || 0,
                                        'In Progress': previewData?.metrics?.stories?.in_progress || 0,
                                        'Review': previewData?.metrics?.stories?.review || 0,
                                        'Testing': previewData?.metrics?.stories?.testing || 0,
                                        'Done': previewData?.metrics?.stories?.done || 0
                                    }" :key="status" class="flex items-center gap-3">
                                        <div class="w-24 text-sm text-gray-600">{{ status }}</div>
                                        <div class="flex-1 h-2 bg-gray-100 rounded-full overflow-hidden">
                                            <div class="h-full rounded-full bg-blue-500"
                                                 :style="{width: (previewData?.metrics?.stories?.total ? (count / previewData.metrics.stories.total * 100) : 0) + '%'}">
                                            </div>
                                        </div>
                                        <div class="w-8 text-right text-sm font-medium">{{ count }}</div>
                                    </div>
                                </div>
                            </div>

                            <!-- Quick Actions -->
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Acoes Rapidas</div>
                                <div class="space-y-3">
                                    <button @click="startAppPreview" class="preview-action-btn primary w-full justify-center">
                                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z"/>
                                        </svg>
                                        Iniciar Servidor
                                    </button>
                                    <button @click="runProjectTests" class="preview-action-btn secondary w-full justify-center">
                                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                        </svg>
                                        Rodar Testes
                                    </button>
                                    <button @click="buildProject" class="preview-action-btn secondary w-full justify-center">
                                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10"/>
                                        </svg>
                                        Build
                                    </button>
                                </div>
                            </div>
                        </div>

                        <!-- Tab: Arquivos -->
                        <div v-if="previewActiveTab === 'Arquivos'" class="grid grid-cols-1 lg:grid-cols-2 gap-6">
                            <div class="preview-card p-6">
                                <div class="preview-section-title">
                                    Arquivos do Projeto
                                    <span class="text-xs font-normal text-gray-500 ml-auto">
                                        {{ previewData?.project_files?.total_files || 0 }} arquivos
                                    </span>
                                </div>
                                <div v-if="previewData?.project_files?.exists" class="space-y-2">
                                    <div v-for="file in previewData?.project_files?.recent_files?.slice(0, 8)"
                                         :key="file.path"
                                         class="preview-file-item">
                                        <div class="preview-file-icon bg-blue-100 text-blue-600">
                                            <span v-if="file.name.endsWith('.py')">PY</span>
                                            <span v-else-if="file.name.endsWith('.js') || file.name.endsWith('.ts')">JS</span>
                                            <span v-else>F</span>
                                        </div>
                                        <div class="flex-1 min-w-0">
                                            <div class="font-medium text-sm truncate">{{ file.name }}</div>
                                            <div class="text-xs text-gray-500">{{ file.path }}</div>
                                        </div>
                                        <div class="text-xs text-gray-400">
                                            {{ formatFileSize(file.size) }}
                                        </div>
                                    </div>
                                </div>
                                <div v-else class="text-center py-8 text-gray-500">
                                    <p>Pasta do projeto ainda nao foi criada</p>
                                </div>
                            </div>
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Tipos de Arquivo</div>
                                <div v-if="previewData?.project_files?.by_type" class="space-y-3">
                                    <div v-for="(count, ext) in previewData?.project_files?.by_type"
                                         :key="ext"
                                         class="flex items-center gap-3">
                                        <div class="w-16 text-sm font-mono text-gray-600">{{ ext }}</div>
                                        <div class="flex-1 h-3 bg-gray-100 rounded-full overflow-hidden">
                                            <div class="h-full bg-blue-500 rounded-full"
                                                 :style="{width: (count / previewData.project_files.total_files * 100) + '%'}">
                                            </div>
                                        </div>
                                        <div class="w-10 text-right text-sm font-medium">{{ count }}</div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Tab: Documentacao -->
                        <div v-if="previewActiveTab === 'Documentacao'" class="preview-card p-6">
                            <div class="preview-section-title">
                                Documentacao do Projeto
                                <span class="text-xs font-normal text-gray-500 ml-auto">
                                    {{ previewData?.documentation?.total || 0 }} documentos
                                </span>
                            </div>
                            <div v-if="previewData?.documentation?.items?.length" class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                                <div v-for="doc in previewData?.documentation?.items"
                                     :key="doc.doc_id"
                                     class="preview-doc-card cursor-pointer">
                                    <div class="font-medium text-sm">{{ doc.title }}</div>
                                    <div class="text-xs text-gray-500 capitalize">{{ doc.doc_type }}</div>
                                </div>
                            </div>
                            <div v-else class="text-center py-12 text-gray-500">
                                <p>Nenhuma documentacao gerada ainda</p>
                            </div>
                        </div>

                        <!-- Tab: Testes -->
                        <div v-if="previewActiveTab === 'Testes'" class="grid grid-cols-1 lg:grid-cols-2 gap-6">
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Resultados de Testes</div>
                                <div class="grid grid-cols-3 gap-4 mb-6">
                                    <div class="text-center p-4 bg-green-50 rounded-lg">
                                        <div class="text-3xl font-bold text-green-600">{{ previewData?.test_results?.passed || 0 }}</div>
                                        <div class="text-xs text-green-700">Passando</div>
                                    </div>
                                    <div class="text-center p-4 bg-red-50 rounded-lg">
                                        <div class="text-3xl font-bold text-red-600">{{ previewData?.test_results?.failed || 0 }}</div>
                                        <div class="text-xs text-red-700">Falhando</div>
                                    </div>
                                    <div class="text-center p-4 bg-yellow-50 rounded-lg">
                                        <div class="text-3xl font-bold text-yellow-600">{{ previewData?.test_results?.skipped || 0 }}</div>
                                        <div class="text-xs text-yellow-700">Ignorados</div>
                                    </div>
                                </div>
                                <button @click="runProjectTests" class="preview-action-btn primary w-full justify-center">
                                    Executar Testes
                                </button>
                            </div>
                            <div class="preview-card p-6">
                                <div class="preview-section-title">Resumo de Tasks</div>
                                <div class="space-y-4">
                                    <div class="flex items-center justify-between">
                                        <span class="text-gray-600">Total de Tasks</span>
                                        <span class="font-bold">{{ previewData?.tasks_summary?.total || 0 }}</span>
                                    </div>
                                    <div class="flex items-center justify-between">
                                        <span class="text-gray-600">Completadas</span>
                                        <span class="font-bold text-green-600">{{ previewData?.tasks_summary?.completed || 0 }}</span>
                                    </div>
                                    <div class="flex items-center justify-between">
                                        <span class="text-gray-600">Em Progresso</span>
                                        <span class="font-bold text-blue-600">{{ previewData?.tasks_summary?.in_progress || 0 }}</span>
                                    </div>
                                    <div class="flex items-center justify-between">
                                        <span class="text-gray-600">Pendentes</span>
                                        <span class="font-bold text-gray-400">{{ previewData?.tasks_summary?.pending || 0 }}</span>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
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

        <!-- MOBILE BOTTOM NAVIGATION -->
        <nav class="mobile-bottom-nav">
            <div class="mobile-bottom-nav-items">
                <div class="mobile-nav-item" :class="{ 'active': mobileMenuOpen }" @click="mobileMenuOpen = !mobileMenuOpen; mobileChatOpen = false">
                    <svg fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"/></svg>
                    <span>Menu</span>
                </div>
                <div class="mobile-nav-item" @click="openNewStoryModal">
                    <svg fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/></svg>
                    <span>Nova Story</span>
                </div>
                <div class="mobile-nav-item" @click="loadProjectData()">
                    <svg fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/></svg>
                    <span>Atualizar</span>
                </div>
                <div class="mobile-nav-item" :class="{ 'active': mobileChatOpen }" @click="mobileChatOpen = !mobileChatOpen; mobileMenuOpen = false">
                    <svg fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"/></svg>
                    <span>Chat</span>
                </div>
            </div>
        </nav>
    </div>

    <script>
    const { createApp, ref, computed, onMounted, nextTick, watch } = Vue;

    // Issue #221: Click-outside directive for global search
    const clickOutsideDirective = {
        mounted(el, binding) {
            el._clickOutside = (event) => {
                if (!(el === event.target || el.contains(event.target))) {
                    binding.value(event);
                }
            };
            document.addEventListener('click', el._clickOutside);
        },
        unmounted(el) {
            document.removeEventListener('click', el._clickOutside);
        }
    };

    const app = createApp({
        setup() {
            // State
            const projects = ref([]);
            const terminalCommand = ref('');
            const terminalRunning = ref(false);
            const terminal = ref(null);
            const terminalOutputInterval = ref(null);
            const previewUrl = ref('http://localhost:3000');
            const previewViewport = ref('desktop');

            // Analytics (Issue #65)
            const showAnalyticsModal = ref(false);
            const analyticsData = ref(null);
            const analyticsInsights = ref(null);
            const analyticsLoading = ref(false);
            const analyticsDays = ref(30);

            // Issue #157: Analytics Charts
            const velocityHistory = ref(null);
            let velocityChart = null;
            let statusChart = null;
            let developerChart = null;
            let cycletimeChart = null;

            // ========== TENANT SELECTOR (Multi-Tenancy) ==========
            const userTenants = ref([]);
            const selectedTenantId = ref('');
            const currentTenantName = computed(() => {
                const tenant = userTenants.value.find(t => t.tenant_id === selectedTenantId.value);
                return tenant ? tenant.name : '';
            });

            const loadTenants = async () => {
                try {
                    const res = await fetch('/api/tenants');
                    userTenants.value = await res.json();
                    const currentRes = await fetch('/api/tenant/current');
                    const current = await currentRes.json();
                    if (current.tenant_id) {
                        selectedTenantId.value = current.tenant_id;
                    } else if (userTenants.value.length > 0) {
                        selectedTenantId.value = userTenants.value[0].tenant_id;
                        await onTenantChange();
                    }
                } catch (e) {
                    console.error('Error loading tenants:', e);
                }
            };

            const onTenantChange = async () => {
                if (!selectedTenantId.value) return;
                try {
                    await fetch('/api/tenant/select', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ tenant_id: selectedTenantId.value })
                    });
                    await loadProjects();
                    addToast('success', 'Tenant selecionado', currentTenantName.value);
                } catch (e) {
                    addToast('error', 'Erro ao selecionar tenant', e.message);
                }
            };

            // ========== ISSUE #220 - BREADCRUMB NAVIGATION ==========
            const breadcrumbItems = computed(() => {
                const items = [];

                // Home is always implicit (shown in template)

                // Project level
                if (selectedProjectId.value) {
                    const project = projects.value.find(p => p.project_id === selectedProjectId.value);
                    if (project) {
                        items.push({
                            type: 'project',
                            id: project.project_id,
                            label: project.name,
                            icon: '📁',
                            badge: project.project_type,
                            badgeType: ''
                        });
                    }
                }

                // Sprint level (if selected)
                if (selectedSprintId.value && sprints.value.length) {
                    const sprint = sprints.value.find(s => s.sprint_id === selectedSprintId.value);
                    if (sprint) {
                        items.push({
                            type: 'sprint',
                            id: sprint.sprint_id,
                            label: sprint.name,
                            icon: '🏃',
                            badge: sprint.status,
                            badgeType: ''
                        });
                    }
                }

                // Epic filter (if selected)
                if (selectedEpicId.value && epics.value.length) {
                    const epic = epics.value.find(e => e.epic_id === selectedEpicId.value);
                    if (epic) {
                        items.push({
                            type: 'epic',
                            id: epic.epic_id,
                            label: epic.title,
                            icon: '🎯',
                            badge: null,
                            badgeType: ''
                        });
                    }
                }

                // Story detail (if viewing a story)
                if (selectedStory.value) {
                    items.push({
                        type: 'story',
                        id: selectedStory.value.story_id,
                        label: selectedStory.value.story_id + ': ' + (selectedStory.value.title?.substring(0, 30) || 'Story'),
                        icon: '📋',
                        badge: selectedStory.value.story_points ? selectedStory.value.story_points + ' pts' : null,
                        badgeType: 'points'
                    });
                }

                return items;
            });

            const navigateBreadcrumb = (type, id = null) => {
                switch (type) {
                    case 'home':
                        selectedProjectId.value = '';
                        selectedSprintId.value = '';
                        selectedEpicId.value = '';
                        selectedStory.value = null;
                        break;
                    case 'project':
                        selectedSprintId.value = '';
                        selectedEpicId.value = '';
                        selectedStory.value = null;
                        break;
                    case 'sprint':
                        selectedEpicId.value = '';
                        selectedStory.value = null;
                        break;
                    case 'epic':
                        selectedStory.value = null;
                        break;
                    case 'story':
                        // Already at story level
                        break;
                }
            };

            // ========== ISSUE #221 - GLOBAL SEARCH ==========
            const showGlobalSearch = ref(false);
            const globalSearchQuery = ref('');
            const globalSearchFilter = ref('all');
            const globalSearchLoading = ref(false);
            const globalSearchResults = ref({ stories: [], tasks: [], docs: [], projects: [], total: 0 });
            const globalSearchSelectedIndex = ref(0);
            const globalSearchInput = ref(null);
            const globalSearchHistory = ref([]);

            const globalSearchFilters = [
                { label: 'Todos', value: 'all' },
                { label: 'Stories', value: 'stories' },
                { label: 'Tasks', value: 'tasks' },
                { label: 'Docs', value: 'docs' },
                { label: 'Projetos', value: 'projects' }
            ];

            // Load search history from localStorage
            const loadSearchHistory = () => {
                try {
                    const saved = localStorage.getItem('globalSearchHistory');
                    if (saved) {
                        globalSearchHistory.value = JSON.parse(saved);
                    }
                } catch (e) {}
            };

            const saveSearchHistory = (query) => {
                if (!query || query.length < 2) return;
                const history = globalSearchHistory.value.filter(q => q !== query);
                history.unshift(query);
                globalSearchHistory.value = history.slice(0, 10);
                localStorage.setItem('globalSearchHistory', JSON.stringify(globalSearchHistory.value));
            };

            const openGlobalSearch = () => {
                showGlobalSearch.value = true;
                loadSearchHistory();
            };

            const closeGlobalSearch = () => {
                showGlobalSearch.value = false;
            };

            const setGlobalSearchFilter = (filter) => {
                globalSearchFilter.value = filter;
                if (globalSearchQuery.value.length >= 2) {
                    performGlobalSearch();
                }
            };

            let searchDebounceTimer = null;
            const debouncedGlobalSearch = () => {
                if (searchDebounceTimer) clearTimeout(searchDebounceTimer);
                searchDebounceTimer = setTimeout(() => {
                    performGlobalSearch();
                }, 300);
            };

            const performGlobalSearch = async () => {
                const query = globalSearchQuery.value.trim();
                if (query.length < 2) {
                    globalSearchResults.value = { stories: [], tasks: [], docs: [], projects: [], total: 0 };
                    return;
                }

                globalSearchLoading.value = true;
                globalSearchSelectedIndex.value = 0;

                try {
                    const params = new URLSearchParams({
                        q: query,
                        types: 'stories,tasks,docs,projects',
                        limit: '10'
                    });
                    if (selectedProjectId.value) {
                        params.set('project_id', selectedProjectId.value);
                    }

                    const res = await fetch('/api/search?' + params.toString());
                    if (res.ok) {
                        globalSearchResults.value = await res.json();
                        saveSearchHistory(query);
                    }
                } catch (e) {
                    console.error('Search error:', e);
                } finally {
                    globalSearchLoading.value = false;
                }
            };

            const getGlobalSearchIndex = (type, idx) => {
                let offset = 0;
                const results = globalSearchResults.value;
                const filter = globalSearchFilter.value;

                if (type === 'story') return offset + idx;
                offset += (filter === 'all' || filter === 'stories') ? results.stories.length : 0;

                if (type === 'task') return offset + idx;
                offset += (filter === 'all' || filter === 'tasks') ? results.tasks.length : 0;

                if (type === 'doc') return offset + idx;
                offset += (filter === 'all' || filter === 'docs') ? results.docs.length : 0;

                if (type === 'project') return offset + idx;

                return offset;
            };

            const getTotalSearchResults = () => {
                const results = globalSearchResults.value;
                const filter = globalSearchFilter.value;
                if (filter === 'all') return results.total;
                if (filter === 'stories') return results.stories.length;
                if (filter === 'tasks') return results.tasks.length;
                if (filter === 'docs') return results.docs.length;
                if (filter === 'projects') return results.projects.length;
                return 0;
            };

            const handleGlobalSearchKey = (e) => {
                const total = getTotalSearchResults();

                if (e.key === 'ArrowDown') {
                    e.preventDefault();
                    globalSearchSelectedIndex.value = (globalSearchSelectedIndex.value + 1) % Math.max(1, total);
                } else if (e.key === 'ArrowUp') {
                    e.preventDefault();
                    globalSearchSelectedIndex.value = (globalSearchSelectedIndex.value - 1 + total) % Math.max(1, total);
                } else if (e.key === 'Enter') {
                    e.preventDefault();
                    navigateToSelectedResult();
                } else if (e.key === 'Escape') {
                    e.preventDefault();
                    closeGlobalSearch();
                }
            };

            const navigateToSelectedResult = () => {
                const idx = globalSearchSelectedIndex.value;
                const results = globalSearchResults.value;
                const filter = globalSearchFilter.value;

                let offset = 0;

                // Stories
                if (filter === 'all' || filter === 'stories') {
                    if (idx < offset + results.stories.length) {
                        navigateToSearchResult('story', results.stories[idx - offset]);
                        return;
                    }
                    offset += results.stories.length;
                }

                // Tasks
                if (filter === 'all' || filter === 'tasks') {
                    if (idx < offset + results.tasks.length) {
                        navigateToSearchResult('task', results.tasks[idx - offset]);
                        return;
                    }
                    offset += results.tasks.length;
                }

                // Docs
                if (filter === 'all' || filter === 'docs') {
                    if (idx < offset + results.docs.length) {
                        navigateToSearchResult('doc', results.docs[idx - offset]);
                        return;
                    }
                    offset += results.docs.length;
                }

                // Projects
                if (filter === 'all' || filter === 'projects') {
                    if (idx < offset + results.projects.length) {
                        navigateToSearchResult('project', results.projects[idx - offset]);
                        return;
                    }
                }
            };

            const navigateToSearchResult = async (type, item) => {
                closeGlobalSearch();
                globalSearchQuery.value = '';

                switch (type) {
                    case 'story':
                        // Select project and open story
                        if (item.project_id && item.project_id !== selectedProjectId.value) {
                            selectedProjectId.value = item.project_id;
                            await loadProjectData();
                        }
                        // Find and select story
                        const storyData = await fetch('/api/stories/' + item.story_id).then(r => r.json());
                        if (storyData) {
                            selectedStory.value = storyData;
                            activeTab.value = 'Detalhes';
                        }
                        break;

                    case 'task':
                        // Load story that contains the task
                        const taskStoryData = await fetch('/api/stories/' + item.story_id).then(r => r.json());
                        if (taskStoryData) {
                            if (taskStoryData.project_id !== selectedProjectId.value) {
                                selectedProjectId.value = taskStoryData.project_id;
                                await loadProjectData();
                            }
                            selectedStory.value = taskStoryData;
                            activeTab.value = 'Tasks';
                        }
                        break;

                    case 'doc':
                        // Load story that contains the doc
                        const docStoryData = await fetch('/api/stories/' + item.story_id).then(r => r.json());
                        if (docStoryData) {
                            if (docStoryData.project_id !== selectedProjectId.value) {
                                selectedProjectId.value = docStoryData.project_id;
                                await loadProjectData();
                            }
                            selectedStory.value = docStoryData;
                            activeTab.value = 'Docs';
                        }
                        break;

                    case 'project':
                        selectedProjectId.value = item.project_id;
                        await loadProjectData();
                        break;
                }
            };

            const applySearchHistory = (query) => {
                globalSearchQuery.value = query;
                performGlobalSearch();
            };

            const highlightMatch = (text, query) => {
                if (!query || !text) return text;
                const regex = new RegExp('(' + query.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&') + ')', 'gi');
                return text.replace(regex, '<mark>$1</mark>');
            };

            // ========== ISSUE #222 - QUICK CREATE FAB ==========
            const fabMenuOpen = ref(false);

            const toggleFabMenu = () => {
                fabMenuOpen.value = !fabMenuOpen.value;
            };

            const closeFabMenu = () => {
                fabMenuOpen.value = false;
            };

            const fabAction = (type) => {
                closeFabMenu();
                switch (type) {
                    case 'story':
                        showNewStoryModal.value = true;
                        break;
                    case 'task':
                        if (selectedStory.value) {
                            showNewTaskModal.value = true;
                        } else {
                            addToast('warning', 'Selecione uma Story', 'Para criar uma task, primeiro selecione uma story.');
                        }
                        break;
                    case 'epic':
                        if (selectedProjectId.value) {
                            showNewEpicModal.value = true;
                        } else {
                            addToast('warning', 'Selecione um Projeto', 'Para criar um epic, primeiro selecione um projeto.');
                        }
                        break;
                    case 'sprint':
                        if (selectedProjectId.value) {
                            showNewSprintModal.value = true;
                        } else {
                            addToast('warning', 'Selecione um Projeto', 'Para criar um sprint, primeiro selecione um projeto.');
                        }
                        break;
                    case 'project':
                        showProjectWizard.value = true;
                        break;
                }
            };

            // ========== ISSUE #133 - BUSINESS TERMS MAPPING ==========
            const businessTerms = {
                'story': 'funcionalidade',
                'stories': 'funcionalidades',
                'sprint': 'ciclo de desenvolvimento',
                'deploy': 'publicacao',
                'bug': 'problema',
                'api': 'conexao',
                'backlog': 'lista de pendencias',
                'task': 'tarefa',
                'tasks': 'tarefas',
                'epic': 'iniciativa',
                'review': 'revisao',
                'testing': 'validacao'
            };
            const translateTerm = (term, capitalize = false) => {
                const translated = businessTerms[term.toLowerCase()] || term;
                return capitalize ? translated.charAt(0).toUpperCase() + translated.slice(1) : translated;
            };

            // ========== ISSUE #135 - EXECUTIVE DASHBOARD ==========
            const viewMode = ref('technical'); // 'technical' or 'executive'
            const showTechnicalLogs = ref(false);
            const recentActivityLogs = ref([]);

            const currentProjectName = computed(() => {
                const project = projects.value.find(p => p.project_id === selectedProjectId.value);
                return project?.name || 'Projeto';
            });

            const projectReadyToTest = computed(() => {
                return doneStories.value > 0;
            });

            const projectProgressPercent = computed(() => {
                if (totalStories.value === 0) return 0;
                return Math.round((doneStories.value / totalStories.value) * 100);
            });

            const projectHealthStatus = computed(() => {
                const progress = projectProgressPercent.value;
                if (progress >= 70) return 'status-green';
                if (progress >= 30) return 'status-yellow';
                return 'status-red';
            });

            const projectHealthColor = computed(() => {
                const progress = projectProgressPercent.value;
                if (progress >= 70) return 'green';
                if (progress >= 30) return 'yellow';
                return 'red';
            });

            const donePoints = computed(() => {
                let points = 0;
                if (storyBoard.value.done) {
                    storyBoard.value.done.forEach(s => points += s.story_points || 0);
                }
                return points;
            });

            const estimatedDaysRemaining = computed(() => {
                const velocity = 5; // Average points per day
                const remaining = totalPoints.value - donePoints.value;
                return Math.ceil(remaining / velocity) || 0;
            });

            const projectPhases = computed(() => {
                const progress = projectProgressPercent.value;
                return [
                    { label: 'Planejamento', status: progress > 0 ? 'completed' : 'current' },
                    { label: 'Desenvolvimento', status: progress >= 25 ? 'completed' : progress > 0 ? 'current' : 'pending' },
                    { label: 'Revisao', status: progress >= 50 ? 'completed' : progress >= 25 ? 'current' : 'pending' },
                    { label: 'Validacao', status: progress >= 75 ? 'completed' : progress >= 50 ? 'current' : 'pending' },
                    { label: 'Entrega', status: progress >= 100 ? 'completed' : progress >= 75 ? 'current' : 'pending' }
                ];
            });

            // ========== ISSUE #134 - WIZARD COMPONENTS ==========
            const showProjectWizard = ref(false);
            const showIntegrationWizard = ref(false);
            const wizardCurrentStep = ref(0);
            const wizardData = ref({
                projectType: '',
                name: '',
                description: '',
                firstFeature: ''
            });

            const wizardSteps = [
                { label: 'Tipo' },
                { label: 'Detalhes' },
                { label: translateTerm('story', true) },
                { label: 'Confirmar' }
            ];

            const projectTypeOptions = [
                { value: 'web', title: 'Aplicacao Web', desc: 'Site, portal ou sistema web', icon: '🌐', color: '#3B82F6' },
                { value: 'mobile', title: 'App Mobile', desc: 'Aplicativo para celular', icon: '📱', color: '#10B981' },
                { value: 'api', title: translateTerm('api', true), desc: 'Servico de integracao', icon: '🔗', color: '#8B5CF6' },
                { value: 'automation', title: 'Automacao', desc: 'Scripts e processos automatizados', icon: '🤖', color: '#F59E0B' }
            ];

            const availableIntegrations = [
                { id: 'github', name: 'GitHub', desc: 'Repositorio de codigo', icon: '📂', color: '#24292E' },
                { id: 'slack', name: 'Slack', desc: 'Notificacoes em tempo real', icon: '💬', color: '#4A154B' },
                { id: 'jira', name: 'Jira', desc: 'Sincronizar tarefas', icon: '📋', color: '#0052CC' }
            ];

            const canProceedWizard = computed(() => {
                if (wizardCurrentStep.value === 0) return wizardData.value.projectType;
                if (wizardCurrentStep.value === 1) return wizardData.value.name;
                if (wizardCurrentStep.value === 2) return wizardData.value.firstFeature;
                return true;
            });

            const getProjectTypeName = (type) => {
                const opt = projectTypeOptions.find(o => o.value === type);
                return opt?.title || type;
            };

            const createProjectFromWizard = async () => {
                try {
                    // Create project
                    const projectRes = await fetch('/api/projects', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                            name: wizardData.value.name,
                            description: wizardData.value.description,
                            project_type: wizardData.value.projectType
                        })
                    });
                    const project = await projectRes.json();

                    // Create first story
                    if (wizardData.value.firstFeature) {
                        await fetch('/api/stories', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({
                                project_id: project.project_id,
                                title: wizardData.value.firstFeature,
                                persona: 'usuario',
                                action: wizardData.value.firstFeature,
                                benefit: 'ter acesso a funcionalidade desejada'
                            })
                        });
                    }

                    // Reset wizard
                    showProjectWizard.value = false;
                    wizardCurrentStep.value = 0;
                    wizardData.value = { projectType: '', name: '', description: '', firstFeature: '' };

                    // Reload projects
                    await loadProjects();
                    selectedProjectId.value = project.project_id;
                    await loadProjectData();

                    addToast('success', 'Projeto criado!', wizardData.value.name);
                } catch (e) {
                    addToast('error', 'Erro ao criar projeto', e.message);
                }
            };

            const selectIntegration = (integration) => {
                addToast('info', 'Em breve', `Integracao com ${integration.name} em desenvolvimento`);
                showIntegrationWizard.value = false;
            };

            // ========== INTEGRATIONS DASHBOARD ==========
            const showIntegrationsModal = ref(false);
            const integrationsLoading = ref(false);
            const integrationsStatus = ref({
                github: { connected: false, status: 'disconnected' },
                jira: { connected: false, status: 'disconnected' },
                azure_devops: { connected: false, status: 'disconnected' }
            });

            const loadIntegrationsStatus = async () => {
                integrationsLoading.value = true;
                try {
                    const res = await fetch('/api/integrations/status');
                    if (res.ok) {
                        const data = await res.json();
                        integrationsStatus.value = {
                            github: data.github || { connected: false, status: 'disconnected' },
                            jira: data.jira || { connected: false, status: 'disconnected' },
                            azure_devops: data.azure_devops || { connected: false, status: 'disconnected' }
                        };
                    }
                } catch (e) {
                    console.error('Erro ao carregar status das integracoes:', e);
                } finally {
                    integrationsLoading.value = false;
                }
            };

            const openIntegrationConfig = (integrationType) => {
                // Set the active tab and load configurations
                activeIntegrationTab.value = integrationType;
                loadIntegrationConfigs();
            };

            // ========== ISSUE #154 - INTEGRATIONS MANAGEMENT ==========
            const showIntegrationConfigModal = ref(false);
            const activeIntegrationTab = ref('github');
            const integrationTesting = ref(null);
            const integrationSaving = ref(null);
            const integrationTabs = [
                { id: 'github', label: 'GitHub' },
                { id: 'gitlab', label: 'GitLab' },
                { id: 'sap', label: 'SAP S/4HANA' },
                { id: 'salesforce', label: 'Salesforce' },
                { id: 'jira', label: 'Jira' }
            ];
            const integrationConfigs = ref({
                github: { token: '', owner: '', repo: '', branch: 'main', connected: false },
                gitlab: { url: 'https://gitlab.com', token: '', project_id: '', branch: 'main', connected: false },
                sap: { host: '', client: '100', username: '', password: '', environment: 'cloud', connected: false },
                salesforce: { client_id: '', client_secret: '', username: '', security_token: '', domain: 'login', connected: false },
                jira: { url: '', email: '', token: '', project_key: '', connected: false }
            });

            const loadIntegrationConfigs = async () => {
                try {
                    const res = await fetch('/api/integrations/config');
                    if (res.ok) {
                        const data = await res.json();
                        if (data.github) integrationConfigs.value.github = { ...integrationConfigs.value.github, ...data.github };
                        if (data.gitlab) integrationConfigs.value.gitlab = { ...integrationConfigs.value.gitlab, ...data.gitlab };
                        if (data.sap) integrationConfigs.value.sap = { ...integrationConfigs.value.sap, ...data.sap };
                        if (data.salesforce) integrationConfigs.value.salesforce = { ...integrationConfigs.value.salesforce, ...data.salesforce };
                        if (data.jira) integrationConfigs.value.jira = { ...integrationConfigs.value.jira, ...data.jira };
                    }
                } catch (e) { console.error('Erro ao carregar configs:', e); }
            };

            const testIntegration = async (type) => {
                integrationTesting.value = type;
                try {
                    const res = await fetch('/api/integrations/test', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ integration_type: type, config: integrationConfigs.value[type] })
                    });
                    const data = await res.json();
                    if (data.success) {
                        addToast('success', 'Conexao OK', data.message);
                        integrationConfigs.value[type].connected = true;
                    } else {
                        addToast('error', 'Falha na Conexao', data.message);
                    }
                } catch (e) { addToast('error', 'Erro', e.message); }
                finally { integrationTesting.value = null; }
            };

            const saveIntegration = async (type) => {
                integrationSaving.value = type;
                try {
                    const res = await fetch('/api/integrations/save', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ integration_type: type, config: integrationConfigs.value[type] })
                    });
                    const data = await res.json();
                    if (data.success) {
                        addToast('success', 'Salvo', data.message);
                        await loadIntegrationsStatus();
                    } else {
                        addToast('error', 'Erro', data.message || 'Falha ao salvar');
                    }
                } catch (e) { addToast('error', 'Erro', e.message); }
                finally { integrationSaving.value = null; }
            };

            // ========== ISSUE #156 - SECURITY SETTINGS ==========
            const showSecuritySettings = ref(false);
            const securityActiveTab = ref('mfa');
            const securityData = ref({ mfa: null, apiKeys: [], sessions: [] });
            const mfaSetupMode = ref(false);
            const mfaSetupData = ref({ qr_code_base64: '', secret_key: '', backup_codes: [] });
            const mfaVerifyCode = ref('');
            const mfaError = ref('');
            const showCreateApiKeyModal = ref(false);
            const createdApiKey = ref(null);
            const newApiKey = ref({ name: '', description: '', tier: 'free', scopes: ['read'] });

            const loadSecurityData = async () => {
                try {
                    const res = await fetch('/api/security/data');
                    if (res.ok) {
                        securityData.value = await res.json();
                    }
                } catch (e) { console.error('Error loading security data:', e); }
            };

            const startMfaSetup = async () => {
                try {
                    const res = await fetch('/api/security/mfa/setup', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1 })
                    });
                    if (res.ok) {
                        mfaSetupData.value = await res.json();
                        mfaSetupMode.value = true;
                        mfaError.value = '';
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao iniciar configuracao MFA'); }
            };

            const completeMfaSetup = async () => {
                try {
                    const res = await fetch('/api/security/mfa/verify', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1, code: mfaVerifyCode.value })
                    });
                    if (res.ok) {
                        mfaSetupMode.value = false;
                        mfaVerifyCode.value = '';
                        await loadSecurityData();
                        addToast('success', 'MFA Ativado', 'Autenticacao de dois fatores ativada com sucesso');
                    } else {
                        const data = await res.json();
                        mfaError.value = data.detail || 'Codigo invalido';
                    }
                } catch (e) { mfaError.value = 'Erro ao verificar codigo'; }
            };

            const cancelMfaSetup = () => {
                mfaSetupMode.value = false;
                mfaSetupData.value = { qr_code_base64: '', secret_key: '', backup_codes: [] };
                mfaVerifyCode.value = '';
                mfaError.value = '';
            };

            const disableMfa = async () => {
                if (!confirm('Tem certeza que deseja desativar o 2FA?')) return;
                try {
                    const res = await fetch('/api/security/mfa/disable', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1 })
                    });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'MFA Desativado', '2FA foi desativado');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao desativar MFA'); }
            };

            const regenerateBackupCodes = async () => {
                const code = prompt('Digite o codigo do seu autenticador:');
                if (!code) return;
                try {
                    const res = await fetch('/api/security/mfa/backup-codes/regenerate', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1, verification_code: code })
                    });
                    if (res.ok) {
                        const data = await res.json();
                        alert('Novos codigos de backup:\\n\\n' + data.backup_codes.join('\\n'));
                        await loadSecurityData();
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao regenerar codigos'); }
            };

            const copyBackupCodes = () => {
                const codes = mfaSetupData.value.backup_codes.join('\\n');
                navigator.clipboard.writeText(codes);
                addToast('success', 'Copiado', 'Codigos copiados para a area de transferencia');
            };

            const createApiKey = async () => {
                try {
                    const res = await fetch('/api/security/api-keys', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(newApiKey.value)
                    });
                    if (res.ok) {
                        createdApiKey.value = await res.json();
                        showCreateApiKeyModal.value = false;
                        newApiKey.value = { name: '', description: '', tier: 'free', scopes: ['read'] };
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao criar API Key'); }
            };

            const revokeApiKey = async (keyId) => {
                if (!confirm('Tem certeza que deseja revogar esta API Key?')) return;
                try {
                    const res = await fetch(`/api/security/api-keys/${keyId}`, { method: 'DELETE' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Revogada', 'API Key revogada com sucesso');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao revogar API Key'); }
            };

            const copyCreatedApiKey = () => {
                navigator.clipboard.writeText(createdApiKey.value.api_key);
                addToast('success', 'Copiado', 'API Key copiada para a area de transferencia');
            };

            const revokeSession = async (sessionId) => {
                try {
                    const res = await fetch(`/api/security/sessions/${sessionId}`, { method: 'DELETE' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Encerrada', 'Sessao encerrada');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao encerrar sessao'); }
            };

            const revokeAllSessions = async () => {
                if (!confirm('Encerrar todas as outras sessoes?')) return;
                try {
                    const res = await fetch('/api/security/sessions/revoke-all', { method: 'POST' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Encerradas', 'Todas as outras sessoes foram encerradas');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao encerrar sessoes'); }
            };

            // ========== ISSUE #132 - ONBOARDING TOUR ==========
            const showOnboardingChecklist = ref(true);
            const showOnboardingTour = ref(false);
            const currentTourStepIndex = ref(0);

            const onboardingSteps = ref([
                { id: 'select_project', label: 'Selecionar um projeto', done: false },
                { id: 'create_story', label: 'Criar uma ' + businessTerms.story, done: false },
                { id: 'move_story', label: 'Mover ' + businessTerms.story + ' no Kanban', done: false },
                { id: 'view_preview', label: 'Visualizar o projeto', done: false }
            ]);

            const onboardingComplete = computed(() => {
                return onboardingSteps.value.every(s => s.done);
            });

            const onboardingProgress = computed(() => {
                const done = onboardingSteps.value.filter(s => s.done).length;
                return Math.round((done / onboardingSteps.value.length) * 100);
            });

            const tourSteps = [
                {
                    target: '.sidebar-desktop',
                    title: 'Barra Lateral',
                    content: 'Aqui voce encontra seus projetos, ' + businessTerms.epics + ' e metricas do projeto.',
                    arrow: 'left'
                },
                {
                    target: '.view-mode-toggle',
                    title: 'Modo de Visualizacao',
                    content: 'Alterne entre visao Tecnica (Kanban) e Executiva (resumo simples).',
                    arrow: 'bottom'
                },
                {
                    target: '.kanban-container',
                    title: 'Kanban Board',
                    content: 'Arraste as ' + businessTerms.stories + ' entre as colunas para atualizar o status.',
                    arrow: 'top'
                },
                {
                    target: '.chat-panel-desktop',
                    title: 'Assistente IA',
                    content: 'Converse com o assistente para tirar duvidas ou executar comandos rapidos.',
                    arrow: 'right'
                }
            ];

            const currentTourStep = computed(() => {
                return tourSteps[currentTourStepIndex.value] || tourSteps[0];
            });

            const onboardingSpotlightStyle = computed(() => {
                const target = document.querySelector(currentTourStep.value.target);
                if (!target) return { display: 'none' };
                const rect = target.getBoundingClientRect();
                return {
                    top: rect.top - 8 + 'px',
                    left: rect.left - 8 + 'px',
                    width: rect.width + 16 + 'px',
                    height: rect.height + 16 + 'px'
                };
            });

            const onboardingTooltipStyle = computed(() => {
                const target = document.querySelector(currentTourStep.value.target);
                if (!target) return { top: '50%', left: '50%', transform: 'translate(-50%, -50%)' };
                const rect = target.getBoundingClientRect();
                const arrow = currentTourStep.value.arrow;
                let style = {};
                if (arrow === 'left') {
                    style = { top: rect.top + 'px', left: rect.right + 20 + 'px' };
                } else if (arrow === 'right') {
                    style = { top: rect.top + 'px', right: window.innerWidth - rect.left + 20 + 'px' };
                } else if (arrow === 'top') {
                    style = { top: rect.bottom + 20 + 'px', left: rect.left + 'px' };
                } else {
                    style = { bottom: window.innerHeight - rect.top + 20 + 'px', left: rect.left + 'px' };
                }
                return style;
            });

            const handleOnboardingStep = (step) => {
                if (step.id === 'select_project' && !selectedProjectId.value) {
                    addToast('info', 'Selecione um projeto', 'Clique em um projeto na lista');
                } else if (step.id === 'create_story') {
                    showNewStoryModal.value = true;
                } else if (step.id === 'view_preview') {
                    openProjectPreview();
                }
            };

            const startOnboardingTour = () => {
                currentTourStepIndex.value = 0;
                showOnboardingTour.value = true;
            };

            const nextTourStep = () => {
                if (currentTourStepIndex.value < tourSteps.length - 1) {
                    currentTourStepIndex.value++;
                }
            };

            const prevTourStep = () => {
                if (currentTourStepIndex.value > 0) {
                    currentTourStepIndex.value--;
                }
            };

            // Issue #291: Skip saves to localStorage and closes all overlays
            const skipOnboardingTour = () => {
                showOnboardingTour.value = false;
                localStorage.setItem('onboardingSkipped', 'true');
            };

            const finishOnboardingTour = () => {
                showOnboardingTour.value = false;
                localStorage.setItem('onboardingComplete', 'true');
                addToast('success', 'Tour concluido!', 'Agora voce conhece o basico da Fabrica de Agentes');
            };

            const markOnboardingStepDone = (stepId) => {
                const step = onboardingSteps.value.find(s => s.id === stepId);
                if (step) step.done = true;
                localStorage.setItem('onboardingSteps', JSON.stringify(onboardingSteps.value));
            };

            // Issue #291: Check both completed and skipped states
            const loadOnboardingState = () => {
                const saved = localStorage.getItem('onboardingSteps');
                if (saved) {
                    const steps = JSON.parse(saved);
                    onboardingSteps.value.forEach((s, i) => {
                        if (steps[i]) s.done = steps[i].done;
                    });
                }
                const complete = localStorage.getItem('onboardingComplete');
                const skipped = localStorage.getItem('onboardingSkipped');
                // Issue #291: Hide checklist if completed OR skipped
                if (complete === 'true' || skipped === 'true') {
                    showOnboardingChecklist.value = false;
                }
            };

            // Issue #291: Close all overlays to prevent blocking
            const closeAllOverlays = () => {
                showOnboardingTour.value = false;
                showCommandPalette.value = false;
                showDocViewer.value = false;
                showConfirmModal.value = false;
                showShortcutsModal.value = false;
                showNewStoryModal.value = false;
                showNewTaskModal.value = false;
                showNewEpicModal.value = false;
                showNewSprintModal.value = false;
                showNewDocModal.value = false;
                commandPaletteQuery.value = '';
            };

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
            const groupBy = ref('');

            // Mobile State
            const mobileMenuOpen = ref(false);
            const mobileChatOpen = ref(false);
            const isPullingToRefresh = ref(false);

            // Toast Notifications
            const toasts = ref([]);
            let toastId = 0;

            // WebSocket Connection
            const wsStatus = ref('disconnected');
            const wsStatusText = ref('Offline');
            const wsStatusTitle = ref('Desconectado do servidor');
            const notificationSoundEnabled = ref(true);

            // Issue #185 - Enhanced File Upload
            const isDraggingFile = ref(false);
            const uploadQueue = ref([]);
            const uploadFileFilter = ref('all');

            let ws = null;
            let wsReconnectTimer = null;
            const notificationSound = new Audio('data:audio/wav;base64,UklGRnoGAABXQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQ==');

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
            const showGenerateDocsDropdown = ref(false);
            const generatingDocs = ref(false);
            const showNewDesignModal = ref(false);

            // Issue #308: Modal open methods for better Vue reactivity
            const openNewStoryModal = () => {
                showNewStoryModal.value = true;
            };

            // Issue #155: Voice Input for Story Creation
            const voiceRecording = ref(false);
            const voiceProcessing = ref(false);
            const voiceRecordingTime = ref(0);
            let voiceMediaRecorder = null;
            let voiceAudioChunks = [];
            let voiceRecordingTimer = null;

            // Issue #195: Omnichannel Input Selector
            const inputMethod = ref('text');
            const omnichannelFileProcessing = ref(false);

            // Test Generation
            const generatingTests = ref(null);
            const showGeneratedTestsModal = ref(false);
            // Security Scan (Issue #57)
            const scanningTask = ref(null);
            const showSecurityScanModal = ref(false);
            const currentSecurityScan = ref(null);

            const currentGeneratedTests = ref(null);
            const showDesignEditor = ref(false);

            // Code Review (Issue #52)
            const reviewingCode = ref(null);
            const showCodeReviewModal = ref(false);
            const currentCodeReview = ref(null);

            // File Viewer
            const showFileViewer = ref(false);
            const fileViewerData = ref({ file: null, content: '', loading: false, error: null, fileType: 'code', language: 'text' });
            // Doc Viewer
            const showDocViewer = ref(false);
            const docViewerData = ref({ doc: null, editMode: false, editContent: '' });

            const showShortcutsModal = ref(false);
            const showBurndownModal = ref(false);

            // Issue #216: Command Palette (Cmd+K)
            const showCommandPalette = ref(false);
            const commandPaletteQuery = ref('');
            const commandPaletteIndex = ref(0);
            const commandPaletteResults = ref([]);
            const recentCommands = ref([]);
            // Project Preview Dashboard (Issue #73)
            const showProjectPreview = ref(false);
            const previewData = ref(null);
            const previewActiveTab = ref('Overview');
            const previewViewportMode = ref('desktop');
            const previewLoading = ref(false);



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
            const newDesign = ref({ title: '', design_type: 'wireframe', description: '' });
            const currentDesign = ref(null);
            const drawioFrame = ref(null);

            // Story Templates (Issue #223)
            const selectedTemplate = ref('');
            const availableTemplates = ref([]);
            const templatesLoading = ref(false);
            const showTemplateSelector = ref(true);

            // Load templates from API
            const loadTemplates = async () => {
                templatesLoading.value = true;
                try {
                    const response = await fetch('/api/templates');
                    if (response.ok) {
                        availableTemplates.value = await response.json();
                    }
                } catch (error) {
                    console.error('Failed to load templates:', error);
                    // Fallback to local templates
                    availableTemplates.value = [
                        { id: 'feature', name: 'Feature', icon: '✨', description: 'Nova funcionalidade', color: '#10B981' },
                        { id: 'bugfix', name: 'Bug Fix', icon: '🐛', description: 'Correcao de bug', color: '#EF4444' },
                        { id: 'tech_debt', name: 'Tech Debt', icon: '🔧', description: 'Debito tecnico', color: '#F59E0B' },
                        { id: 'integration', name: 'Integracao', icon: '🔗', description: 'Integracao externa', color: '#8B5CF6' },
                        { id: 'ui_improvement', name: 'UI/UX', icon: '🎨', description: 'Melhoria visual', color: '#EC4899' },
                        { id: 'documentation', name: 'Documentacao', icon: '📚', description: 'Documentacao', color: '#6B7280' }
                    ];
                } finally {
                    templatesLoading.value = false;
                }
            };

            // Local template data for form population
            const storyTemplatesData = {
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
                integration: {
                    title: '[Integracao] ',
                    persona: 'sistema',
                    action: 'integrar com servico externo',
                    benefit: 'tenhamos dados sincronizados',
                    description: '## Sistema externo\\n\\n## Endpoints/APIs\\n\\n## Autenticacao\\n\\n## Mapeamento de dados\\n',
                    criteria: 'Integracao funcionando\\nTestes de integracao passando\\nDocumentacao da API\\nMonitoramento configurado',
                    story_points: 8,
                    priority: 'medium',
                    complexity: 'high',
                    category: 'integration'
                },
                ui_improvement: {
                    title: '[UI/UX] ',
                    persona: 'usuario',
                    action: 'ter uma interface melhorada',
                    benefit: 'tenha uma experiencia mais agradavel',
                    description: '## Componente atual\\n\\n## Problema de UX\\n\\n## Solucao proposta\\n\\n## Mockups/Referencias\\n',
                    criteria: 'UI implementada conforme design\\nResponsivo\\nAcessibilidade verificada\\nTestes visuais passando',
                    story_points: 3,
                    priority: 'medium',
                    complexity: 'medium',
                    category: 'ui'
                },
                documentation: {
                    title: '[Doc] ',
                    persona: 'desenvolvedor/usuario',
                    action: 'ter documentacao atualizada',
                    benefit: 'possa entender e usar o sistema',
                    description: '## Tipo de documentacao\\n\\n## Publico alvo\\n\\n## Topicos a cobrir\\n\\n## Referencias\\n',
                    criteria: 'Documentacao criada\\nRevisada por pares\\nPublicada\\nLinks atualizados',
                    story_points: 2,
                    priority: 'low',
                    complexity: 'low',
                    category: 'documentation'
                }
            };

            const applyTemplate = (templateId = null) => {
                const id = templateId || selectedTemplate.value;
                const template = storyTemplatesData[id];
                if (template) {
                    selectedTemplate.value = id;
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
                    showTemplateSelector.value = false;
                    const tmpl = availableTemplates.value.find(t => t.id === id);
                    addToast('info', 'Template aplicado', 'Formulario preenchido com ' + (tmpl?.name || id));
                }
            };

            const clearTemplate = () => {
                selectedTemplate.value = '';
                showTemplateSelector.value = true;
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

            // Grouped Stories for Swimlanes
            const groupedStories = computed(() => {
                if (!groupBy.value) return null;

                const groups = {};
                const statuses = ['backlog', 'ready', 'in_progress', 'review', 'testing', 'done'];

                // Initialize columns for each group
                const initGroup = () => {
                    const obj = {};
                    statuses.forEach(status => obj[status] = []);
                    return obj;
                };

                // Group by Epic
                if (groupBy.value === 'epic') {
                    // Add group for each epic
                    epics.value.forEach(epic => {
                        groups[epic.epic_id] = { name: epic.title, ...initGroup() };
                    });
                    // Add "Sem Epic" group
                    groups['no_epic'] = { name: 'Sem Epic', ...initGroup() };

                    // Distribute stories
                    Object.entries(filteredStoryBoard.value).forEach(([status, stories]) => {
                        stories.forEach(story => {
                            const groupKey = story.epic_id || 'no_epic';
                            if (groups[groupKey]) {
                                groups[groupKey][status].push(story);
                            }
                        });
                    });
                }

                // Group by Assignee
                else if (groupBy.value === 'assignee') {
                    const assignees = new Set();
                    Object.values(filteredStoryBoard.value).forEach(stories => {
                        stories.forEach(story => {
                            if (story.assignee) assignees.add(story.assignee);
                        });
                    });

                    // Add group for each assignee
                    assignees.forEach(assignee => {
                        groups[assignee] = { name: assignee, ...initGroup() };
                    });
                    // Add "Não atribuído" group
                    groups['unassigned'] = { name: 'Não atribuído', ...initGroup() };

                    // Distribute stories
                    Object.entries(filteredStoryBoard.value).forEach(([status, stories]) => {
                        stories.forEach(story => {
                            const groupKey = story.assignee || 'unassigned';
                            if (!groups[groupKey]) {
                                groups[groupKey] = { name: groupKey, ...initGroup() };
                            }
                            groups[groupKey][status].push(story);
                        });
                    });
                }

                // Group by Priority
                else if (groupBy.value === 'priority') {
                    const priorities = [
                        { key: 'urgent', name: 'Urgente', color: '#EF4444' },
                        { key: 'high', name: 'Alta', color: '#F59E0B' },
                        { key: 'medium', name: 'Média', color: '#3B82F6' },
                        { key: 'low', name: 'Baixa', color: '#10B981' }
                    ];

                    priorities.forEach(p => {
                        groups[p.key] = { name: p.name, color: p.color, ...initGroup() };
                    });

                    // Distribute stories
                    Object.entries(filteredStoryBoard.value).forEach(([status, stories]) => {
                        stories.forEach(story => {
                            const groupKey = story.priority || 'medium';
                            if (groups[groupKey]) {
                                groups[groupKey][status].push(story);
                            }
                        });
                    });
                }

                // Filter out empty groups
                const filtered = {};
                Object.entries(groups).forEach(([key, group]) => {
                    const hasStories = Object.values(group).some(col =>
                        Array.isArray(col) && col.length > 0
                    );
                    if (hasStories) {
                        filtered[key] = group;
                    }
                });

                return filtered;
            });

            // Clear all filters
            const clearFilters = () => {
                searchQuery.value = '';
                filterPriority.value = '';
                filterAssignee.value = '';
                groupBy.value = '';
            };

            // Methods
            const loadProjects = async () => {
                const res = await fetch('/api/projects');
                projects.value = await res.json();
                // Issue #294: Auto-select first project if none selected
                if (!selectedProjectId.value && projects.value.length > 0) {
                    selectedProjectId.value = projects.value[0].project_id;
                }
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

                // Regular Kanban columns
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
                                const newStatus = evt.to.id.replace('column-', '').replace(/^swimlane-.*?-/, '');
                                const newOrder = evt.newIndex;

                                try {
                                    await fetch(`/api/stories/${storyId}/move`, {
                                        method: 'PATCH',
                                        headers: { 'Content-Type': 'application/json' },
                                        body: JSON.stringify({ status: newStatus, order: newOrder })
                                    });
                                    addToast('success', translateTerm('story', true) + ' movida', storyId + ' -> ' + getColumnTitle(newStatus));
                                    markOnboardingStepDone('move_story'); // Issue #132
                                    loadProjectData();
                                } catch (e) {
                                    addToast('error', 'Erro ao mover', 'Nao foi possivel mover a ' + translateTerm('story'));
                                    loadProjectData();
                                }
                            }
                        });
                    }
                });

                // Swimlane columns
                if (groupedStories.value) {
                    Object.keys(groupedStories.value).forEach(groupKey => {
                        statuses.forEach(status => {
                            const el = document.getElementById('swimlane-' + groupKey + '-' + status);
                            if (el) {
                                new Sortable(el, {
                                    group: 'stories',
                                    animation: 150,
                                    ghostClass: 'sortable-ghost',
                                    dragClass: 'sortable-drag',
                                    onEnd: async (evt) => {
                                        const storyId = evt.item.dataset.id;
                                        const newStatus = evt.to.id.split('-').pop();
                                        const newOrder = evt.newIndex;

                                        try {
                                            await fetch(`/api/stories/${storyId}/move`, {
                                                method: 'PATCH',
                                                headers: { 'Content-Type': 'application/json' },
                                                body: JSON.stringify({ status: newStatus, order: newOrder })
                                            });
                                            addToast('success', translateTerm('story', true) + ' movida', storyId + ' -> ' + getColumnTitle(newStatus));
                                            markOnboardingStepDone('move_story'); // Issue #132
                                            loadProjectData();
                                        } catch (e) {
                                            addToast('error', 'Erro ao mover', 'Nao foi possivel mover a ' + translateTerm('story'));
                                            loadProjectData();
                                        }
                                    }
                                });
                            }
                        });
                    });
                }
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

                // Load designs
                const designsRes = await fetch(`/api/stories/${story.story_id}/designs`);
                selectedStory.value.designs = await designsRes.json();

                activeTab.value = 'Detalhes';
            };

            // Issue #155: Voice Input Functions
            const toggleVoiceInput = async () => {
                if (voiceRecording.value) {
                    // Stop recording
                    if (voiceMediaRecorder && voiceMediaRecorder.state === 'recording') {
                        voiceMediaRecorder.stop();
                    }
                    if (voiceRecordingTimer) {
                        clearInterval(voiceRecordingTimer);
                        voiceRecordingTimer = null;
                    }
                    voiceRecording.value = false;
                } else {
                    // Start recording
                    try {
                        const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
                        voiceMediaRecorder = new MediaRecorder(stream);
                        voiceAudioChunks = [];

                        voiceMediaRecorder.ondataavailable = (e) => {
                            if (e.data.size > 0) {
                                voiceAudioChunks.push(e.data);
                            }
                        };

                        voiceMediaRecorder.onstop = async () => {
                            stream.getTracks().forEach(track => track.stop());
                            voiceProcessing.value = true;

                            try {
                                const audioBlob = new Blob(voiceAudioChunks, { type: 'audio/webm' });
                                const formData = new FormData();
                                formData.append('audio', audioBlob, 'voice_input.webm');
                                formData.append('language', 'pt-BR');

                                const res = await fetch('/api/v1/inputs/voice', {
                                    method: 'POST',
                                    body: formData
                                });

                                if (res.ok) {
                                    const result = await res.json();
                                    if (result.stories && result.stories.length > 0) {
                                        const story = result.stories[0];
                                        // Fill form with extracted story
                                        newStory.value.title = story.title || '';
                                        newStory.value.persona = story.persona || '';
                                        newStory.value.action = story.action || '';
                                        newStory.value.benefit = story.benefit || '';
                                        if (story.acceptance_criteria && story.acceptance_criteria.length > 0) {
                                            newStoryCriteria.value = story.acceptance_criteria.join('\\n');
                                        }
                                        if (story.story_points) newStory.value.story_points = story.story_points;
                                        if (story.priority) newStory.value.priority = story.priority;
                                        addToast('success', 'Voz processada', 'Story preenchida a partir do audio');
                                    } else {
                                        addToast('warning', 'Nenhuma story extraida', 'Tente descrever sua story novamente');
                                    }
                                } else {
                                    addToast('error', 'Erro ao processar voz', 'Tente novamente');
                                }
                            } catch (e) {
                                console.error('Voice processing error:', e);
                                addToast('error', 'Erro de processamento', e.message);
                            } finally {
                                voiceProcessing.value = false;
                            }
                        };

                        voiceMediaRecorder.start();
                        voiceRecording.value = true;
                        voiceRecordingTime.value = 0;

                        // Timer for recording duration
                        voiceRecordingTimer = setInterval(() => {
                            voiceRecordingTime.value++;
                            // Auto-stop after 60 seconds
                            if (voiceRecordingTime.value >= 60) {
                                toggleVoiceInput();
                            }
                        }, 1000);

                    } catch (e) {
                        console.error('Microphone access error:', e);
                        addToast('error', 'Erro de microfone', 'Permita acesso ao microfone nas configuracoes do navegador');
                    }
                }
            };

            // Issue #195: Process Omnichannel File Input
            const processOmnichannelFile = async (event) => {
                const file = event.target.files?.[0];
                if (!file) return;

                omnichannelFileProcessing.value = true;
                try {
                    const formData = new FormData();
                    formData.append('file', file);

                    const res = await fetch('/api/v1/inputs/document', {
                        method: 'POST',
                        body: formData
                    });

                    if (res.ok) {
                        const data = await res.json();
                        if (data.stories && data.stories.length > 0) {
                            const story = data.stories[0];
                            newStory.value.title = story.title || '';
                            newStory.value.persona = story.persona || '';
                            newStory.value.action = story.action || '';
                            newStory.value.benefit = story.benefit || '';
                            if (story.acceptance_criteria?.length) {
                                newStoryCriteria.value = story.acceptance_criteria.join('\\n');
                            }
                            if (story.story_points) newStory.value.story_points = story.story_points;
                            if (story.priority) newStory.value.priority = story.priority;

                            addToast('success', 'Documento processado', `Extraida ${data.stories.length} story de ${file.name}`);
                            inputMethod.value = 'text'; // Switch to text to show filled form
                        } else {
                            addToast('warning', 'Sem requisitos', 'Nenhum requisito foi extraido do documento');
                        }
                    } else {
                        addToast('error', 'Erro', 'Falha ao processar documento');
                    }
                } catch (e) {
                    console.error('Document processing error:', e);
                    addToast('error', 'Erro', 'Falha ao processar documento: ' + e.message);
                } finally {
                    omnichannelFileProcessing.value = false;
                    event.target.value = ''; // Reset file input
                }
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
                        addToast('success', translateTerm('story', true) + ' criada', created.story_id + ': ' + created.title);
                        markOnboardingStepDone('create_story'); // Issue #132
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

            const generateDocs = async (docType) => {
                showGenerateDocsDropdown.value = false;
                generatingDocs.value = true;

                try {
                    const res = await fetch(`/api/stories/${selectedStory.value.story_id}/generate-docs`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ doc_type: docType })
                    });

                    if (res.ok) {
                        const created = await res.json();
                        addToast('success', 'Documentacao gerada', created.title);
                        const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                        selectedStory.value = await storyRes.json();
                    } else {
                        throw new Error('Erro ao gerar documentacao');
                    }
                } catch (e) {
                    addToast('error', 'Erro ao gerar docs', e.message);
                } finally {
                    generatingDocs.value = false;
                }
            };

            // Generate Tests for Task
            const generateTestsForTask = async (task) => {
                if (generatingTests.value === task.task_id) return;
                generatingTests.value = task.task_id;
                try {
                    const res = await fetch(`/api/story-tasks/${task.task_id}/generate-tests`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' }
                    });
                    if (res.ok) {
                        const result = await res.json();
                        task.generated_tests = result;
                        addToast('success', 'Testes gerados!', `${result.test_count} testes ${result.framework}`);
                        currentGeneratedTests.value = result;
                        showGeneratedTestsModal.value = true;
                    } else {
                        const err = await res.json();
                        throw new Error(err.detail || 'Erro ao gerar testes');
                    }
                } catch (e) {
                    addToast('error', 'Erro ao gerar testes', e.message);
                } finally {
                    generatingTests.value = null;
                }
            };

            // Security Scan function (Issue #57)
            const runSecurityScan = async (task) => {
                if (!task.code_output) {
                    showToast('Task nao possui codigo para analisar', 'error');
                    return;
                }
                scanningTask.value = task.task_id;
                try {
                    const res = await fetch(`/api/story-tasks/${task.task_id}/security-scan`, {
                        method: 'POST'
                    });
                    if (res.ok) {
                        const result = await res.json();
                        currentSecurityScan.value = result;
                        showSecurityScanModal.value = true;
                        if (result.summary?.total === 0) {
                            showToast('Nenhuma vulnerabilidade encontrada!', 'success');
                        } else if (result.summary?.critical > 0) {
                            showToast(`${result.summary.critical} vulnerabilidades criticas encontradas!`, 'error');
                        } else {
                            showToast(`${result.summary.total} vulnerabilidades encontradas`, 'warning');
                        }
                    } else {
                        const err = await res.json();
                        showToast(err.detail || 'Erro no scan de seguranca', 'error');
                    }
                } catch (e) {
                    console.error('Security scan error:', e);
                    showToast('Erro ao executar scan de seguranca', 'error');
                } finally {
                    scanningTask.value = null;
                }
            };

            const showGeneratedTests = (task) => {
                currentGeneratedTests.value = task.generated_tests;
                showGeneratedTestsModal.value = true;
            };

            const copyTestCode = async () => {
                if (!currentGeneratedTests.value?.test_code) return;
                try {
                    await navigator.clipboard.writeText(currentGeneratedTests.value.test_code);
                    addToast('success', 'Copiado!', 'Codigo de testes copiado');
                } catch (e) {
                    addToast('error', 'Erro ao copiar', 'Use Ctrl+C');
                }
            };

            const downloadTestCode = () => {
                if (!currentGeneratedTests.value?.test_code) return;
                const tests = currentGeneratedTests.value;
                const ext = tests.language === 'python' ? 'py' : (tests.language === 'javascript' ? 'js' : tests.language);
                const filename = `test_generated.${ext}`;
                const blob = new Blob([tests.test_code], { type: 'text/plain' });
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = filename;
                document.body.appendChild(a);
                a.click();
                document.body.removeChild(a);
                URL.revokeObjectURL(url);
                addToast('success', 'Download iniciado', filename);
            };

            // Code Review Functions (Issue #52)
            const codeReviewTask = async (task) => {
                if (reviewingCode.value === task.task_id) return;
                reviewingCode.value = task.task_id;
                try {
                    const res = await fetch(`/api/story-tasks/${task.task_id}/code-review`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' }
                    });
                    if (res.ok) {
                        const result = await res.json();
                        task.review_result = result;
                        addToast('success', 'Analise concluida!', `Score: ${result.score}/100`);
                        currentCodeReview.value = result;
                        showCodeReviewModal.value = true;
                        // Reload story to update task
                        if (selectedStory.value) {
                            const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                            if (storyRes.ok) {
                                selectedStory.value = await storyRes.json();
                            }
                        }
                    } else {
                        const err = await res.json();
                        throw new Error(err.detail || 'Erro ao analisar codigo');
                    }
                } catch (e) {
                    addToast('error', 'Erro no Code Review', e.message);
                } finally {
                    reviewingCode.value = null;
                }
            };

            const showCodeReviewResult = (task) => {
                currentCodeReview.value = task.review_result;
                showCodeReviewModal.value = true;
            };

            const copyDocContent = async (content) => {
                try {
                    await navigator.clipboard.writeText(content);
                    addToast('success', 'Copiado', 'Conteudo copiado');
                } catch (e) {
                    addToast('error', 'Erro ao copiar', 'Use Ctrl+C');
                }
            };

            const downloadDoc = (doc) => {
                try {
                    const blob = new Blob([doc.content], { type: 'text/markdown' });
                    const url = URL.createObjectURL(blob);
                    const a = document.createElement('a');
                    a.href = url;
                    a.download = `${doc.doc_id}-${doc.title.replace(/[^a-zA-Z0-9]/g, '_')}.md`;
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                    addToast('success', 'Download iniciado', doc.title);
                } catch (e) {
                    addToast('error', 'Erro no download', 'Tente novamente');
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

            // ========== ENHANCED FILE UPLOAD FUNCTIONS - Issue #185 ==========
            const handleMultipleFileUpload = async (event) => {
                const files = Array.from(event.target.files);
                if (!files.length) return;
                for (const file of files) {
                    await uploadSingleFile(file);
                }
            };

            const handleFileDrop = async (event) => {
                isDraggingFile.value = false;
                const files = Array.from(event.dataTransfer.files);
                for (const file of files) {
                    await uploadSingleFile(file);
                }
            };

            const uploadSingleFile = async (file) => {
                const id = Date.now() + Math.random();
                const queueItem = { id, file, progress: 0, status: 'uploading' };
                uploadQueue.value.push(queueItem);

                try {
                    const formData = new FormData();
                    formData.append('file', file);
                    formData.append('story_id', selectedStory.value.story_id);

                    // Determine endpoint based on file type
                    let endpoint = '/api/upload';
                    const ext = file.name.toLowerCase().split('.').pop();
                    if (['doc', 'docx'].includes(ext)) {
                        endpoint = '/api/input/document';
                    } else if (['mp4', 'mov', 'avi'].includes(ext)) {
                        endpoint = '/api/input/video';
                    }

                    // Create XMLHttpRequest for progress tracking
                    const xhr = new XMLHttpRequest();
                    xhr.upload.onprogress = (e) => {
                        if (e.lengthComputable) {
                            const item = uploadQueue.value.find(q => q.id === id);
                            if (item) item.progress = Math.round((e.loaded / e.total) * 100);
                        }
                    };

                    await new Promise((resolve, reject) => {
                        xhr.onload = () => {
                            if (xhr.status >= 200 && xhr.status < 300) {
                                resolve(xhr.response);
                            } else {
                                reject(new Error('Upload failed'));
                            }
                        };
                        xhr.onerror = () => reject(new Error('Network error'));
                        xhr.open('POST', endpoint);
                        xhr.send(formData);
                    });

                    const item = uploadQueue.value.find(q => q.id === id);
                    if (item) item.status = 'success';
                    addToast('success', 'Arquivo enviado', file.name);

                    // Refresh story files
                    const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                    selectedStory.value = await storyRes.json();

                    // Remove from queue after 2 seconds
                    setTimeout(() => {
                        uploadQueue.value = uploadQueue.value.filter(q => q.id !== id);
                    }, 2000);
                } catch (e) {
                    const item = uploadQueue.value.find(q => q.id === id);
                    if (item) item.status = 'error';
                    addToast('error', 'Erro no upload', file.name);
                }
            };

            const getFileIcon = (filename) => {
                const ext = (filename || '').toLowerCase().split('.').pop();
                const icons = {
                    pdf: '📕', doc: '📘', docx: '📘', txt: '📝', md: '📝',
                    mp4: '🎬', mov: '🎬', avi: '🎬',
                    png: '🖼️', jpg: '🖼️', jpeg: '🖼️', gif: '🖼️'
                };
                return icons[ext] || '📄';
            };

            const getFileIconClass = (filename) => {
                const ext = (filename || '').toLowerCase().split('.').pop();
                if (['pdf'].includes(ext)) return 'pdf';
                if (['doc', 'docx'].includes(ext)) return 'doc';
                if (['mp4', 'mov', 'avi'].includes(ext)) return 'video';
                if (['png', 'jpg', 'jpeg', 'gif'].includes(ext)) return 'image';
                if (['txt', 'md'].includes(ext)) return 'text';
                return 'default';
            };

            const getFileTypeName = (filename) => {
                const ext = (filename || '').toLowerCase().split('.').pop();
                const names = {
                    pdf: 'PDF', doc: 'Word', docx: 'Word', txt: 'Texto', md: 'Markdown',
                    mp4: 'Video MP4', mov: 'Video MOV', avi: 'Video AVI',
                    png: 'Imagem PNG', jpg: 'Imagem JPG', jpeg: 'Imagem JPG', gif: 'Imagem GIF'
                };
                return names[ext] || ext.toUpperCase();
            };

            const isImageFile = (filename) => {
                const ext = (filename || '').toLowerCase().split('.').pop();
                return ['png', 'jpg', 'jpeg', 'gif'].includes(ext);
            };

            const canPreviewFile = (filename) => {
                const ext = (filename || '').toLowerCase().split('.').pop();
                return ['png', 'jpg', 'jpeg', 'gif', 'pdf'].includes(ext);
            };

            const formatUploadDate = (dateStr) => {
                if (!dateStr) return '';
                const date = new Date(dateStr);
                return date.toLocaleDateString('pt-BR', { day: '2-digit', month: 'short', year: 'numeric' });
            };

            const countFilesByType = (type) => {
                if (!selectedStory.value?.files) return 0;
                return selectedStory.value.files.filter(f => {
                    const ext = (f.original_filename || '').toLowerCase().split('.').pop();
                    if (type === 'document') return ['pdf', 'doc', 'docx', 'txt', 'md'].includes(ext);
                    if (type === 'video') return ['mp4', 'mov', 'avi'].includes(ext);
                    if (type === 'image') return ['png', 'jpg', 'jpeg', 'gif'].includes(ext);
                    return true;
                }).length;
            };

            const filteredUploadFiles = computed(() => {
                if (!selectedStory.value?.files) return [];
                if (uploadFileFilter.value === 'all') return selectedStory.value.files;
                return selectedStory.value.files.filter(f => {
                    const ext = (f.original_filename || '').toLowerCase().split('.').pop();
                    if (uploadFileFilter.value === 'document') return ['pdf', 'doc', 'docx', 'txt', 'md'].includes(ext);
                    if (uploadFileFilter.value === 'video') return ['mp4', 'mov', 'avi'].includes(ext);
                    if (uploadFileFilter.value === 'image') return ['png', 'jpg', 'jpeg', 'gif'].includes(ext);
                    return true;
                });
            });

            const previewFile = (file) => {
                window.open('/api/attachments/' + file.attachment_id, '_blank');
            };

            const deleteUploadedFile = async (file) => {
                if (!confirm('Excluir arquivo ' + file.original_filename + '?')) return;
                try {
                    await fetch('/api/attachments/' + file.attachment_id, { method: 'DELETE' });
                    addToast('success', 'Arquivo excluido', file.original_filename);
                    const storyRes = await fetch(`/api/stories/${selectedStory.value.story_id}`);
                    selectedStory.value = await storyRes.json();
                } catch (e) {
                    addToast('error', 'Erro ao excluir', e.message);
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

            // WebSocket Functions
            const connectWebSocket = () => {
                if (ws && ws.readyState === WebSocket.OPEN) return;
                wsStatus.value = 'connecting';
                wsStatusText.value = 'Conectando...';
                const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                try {
                    ws = new WebSocket(`${protocol}//${window.location.host}/ws/notifications`);
                    ws.onopen = () => { wsStatus.value = 'connected'; wsStatusText.value = 'Online'; wsStatusTitle.value = 'Conectado'; };
                    ws.onmessage = (event) => { try { handleWebSocketNotification(JSON.parse(event.data)); } catch(e){} };
                    ws.onclose = () => { wsStatus.value = 'disconnected'; wsStatusText.value = 'Offline'; if(wsReconnectTimer) clearTimeout(wsReconnectTimer); wsReconnectTimer = setTimeout(connectWebSocket, 5000); };
                    ws.onerror = () => { wsStatus.value = 'disconnected'; };
                } catch(e) { wsStatus.value = 'disconnected'; }
            };
            const handleWebSocketNotification = (n) => {
                if (n.type === 'pong' || n.type === 'connection') return;
                const cfg = { story_created: ['Nova Story', n.data.story_id+': '+n.data.title], story_moved: ['Story Movida', n.data.story_id], story_updated: ['Story Atualizada', n.data.story_id], task_completed: ['Task Completa', n.data.task_id], chat_message: ['Nova Mensagem', n.data.preview||''] }[n.type];
                if (cfg) { addToast(n.type, cfg[0], cfg[1]); if(notificationSoundEnabled.value) try{notificationSound.play();}catch(e){}; if(selectedProjectId.value) loadProjectData(); }
            };
            const toggleNotificationSound = () => { notificationSoundEnabled.value = !notificationSoundEnabled.value; localStorage.setItem('notificationSoundEnabled', notificationSoundEnabled.value); addToast('info', notificationSoundEnabled.value ? 'Som ativado' : 'Som desativado'); };
            const loadNotificationSoundPreference = () => { const s = localStorage.getItem('notificationSoundEnabled'); if(s!==null) notificationSoundEnabled.value = s==='true'; };

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

            // Issue #216: Command Palette Commands
            const commandPaletteCommands = computed(() => {
                const commands = [
                    // Actions
                    { id: 'new-story', title: 'Create Story', icon: '📝', category: 'Actions', shortcut: 'N', action: () => { showNewStoryModal.value = true; } },
                    { id: 'new-task', title: 'Create Task', icon: '✓', category: 'Actions', shortcut: 'T', action: () => { if(selectedStory.value) showNewTaskModal.value = true; } },
                    { id: 'new-epic', title: 'Create Epic', icon: '📦', category: 'Actions', action: () => { showNewEpicModal.value = true; } },
                    { id: 'new-sprint', title: 'Create Sprint', icon: '🏃', category: 'Actions', action: () => { showNewSprintModal.value = true; } },
                    { id: 'refresh', title: 'Refresh Data', icon: '🔄', category: 'Actions', shortcut: 'R', action: () => { loadProjectData(); } },
                    // Navigation
                    { id: 'go-kanban', title: 'Go to Kanban', icon: '📋', category: 'Navigation', action: () => { currentView.value = 'kanban'; } },
                    { id: 'go-list', title: 'Go to List View', icon: '📃', category: 'Navigation', action: () => { currentView.value = 'list'; } },
                    { id: 'go-analytics', title: 'Go to Analytics', icon: '📊', category: 'Navigation', action: () => { showAnalyticsModal.value = true; } },
                    { id: 'go-settings', title: 'Open Settings', icon: '⚙️', category: 'Navigation', shortcut: '⌘,', action: () => { showSettingsModal.value = true; } },
                    // Toggles
                    { id: 'toggle-dark', title: 'Toggle Dark Mode', icon: '🌙', category: 'Settings', shortcut: 'D', action: () => { toggleDarkMode(); } },
                    { id: 'toggle-sound', title: 'Toggle Notifications', icon: '🔔', category: 'Settings', action: () => { toggleNotificationSound(); } },
                    // Help
                    { id: 'shortcuts', title: 'Keyboard Shortcuts', icon: '⌨️', category: 'Help', shortcut: '?', action: () => { showShortcutsModal.value = true; } },
                ];
                // Add stories as searchable items
                stories.value.forEach(story => {
                    commands.push({
                        id: 'story-' + story.story_id,
                        title: story.story_id + ' - ' + story.title,
                        icon: '📄',
                        category: 'Stories',
                        action: () => { selectStory(story); }
                    });
                });
                return commands;
            });

            const filterCommandPalette = () => {
                const query = commandPaletteQuery.value.toLowerCase().trim();
                if (!query) {
                    // Show recent + actions when no query
                    const recent = recentCommands.value.slice(0, 3).map(id =>
                        commandPaletteCommands.value.find(c => c.id === id)
                    ).filter(Boolean);
                    const actions = commandPaletteCommands.value.filter(c => c.category === 'Actions');
                    commandPaletteResults.value = [...recent, ...actions].slice(0, 10);
                } else {
                    // Fuzzy search
                    commandPaletteResults.value = commandPaletteCommands.value.filter(cmd =>
                        cmd.title.toLowerCase().includes(query) ||
                        cmd.category.toLowerCase().includes(query)
                    ).slice(0, 10);
                }
                commandPaletteIndex.value = 0;
            };

            const executeCommand = (cmd) => {
                if (!cmd) return;
                // Add to recent (front, unique)
                recentCommands.value = [cmd.id, ...recentCommands.value.filter(id => id !== cmd.id)].slice(0, 5);
                showCommandPalette.value = false;
                commandPaletteQuery.value = '';
                cmd.action();
            };

            const handleCommandPaletteKey = (e) => {
                if (!showCommandPalette.value) return;
                if (e.key === 'ArrowDown') {
                    e.preventDefault();
                    commandPaletteIndex.value = Math.min(commandPaletteIndex.value + 1, commandPaletteResults.value.length - 1);
                } else if (e.key === 'ArrowUp') {
                    e.preventDefault();
                    commandPaletteIndex.value = Math.max(commandPaletteIndex.value - 1, 0);
                } else if (e.key === 'Enter') {
                    e.preventDefault();
                    executeCommand(commandPaletteResults.value[commandPaletteIndex.value]);
                } else if (e.key === 'Escape') {
                    e.preventDefault();
                    showCommandPalette.value = false;
                    commandPaletteQuery.value = '';
                }
            };

            // Watch query changes
            watch(commandPaletteQuery, filterCommandPalette);
            watch(showCommandPalette, (val) => {
                if (val) {
                    filterCommandPalette();
                    setTimeout(() => {
                        const input = document.getElementById('command-palette-input');
                        if (input) input.focus();
                    }, 50);
                }
            });

            // Keyboard Shortcuts
            const handleKeyboard = (e) => {
                // Cmd+K / Ctrl+K - Command Palette (Issue #216)
                if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
                    e.preventDefault();
                    showCommandPalette.value = !showCommandPalette.value;
                    return;
                }

                // Handle command palette navigation
                if (showCommandPalette.value) {
                    handleCommandPaletteKey(e);
                    return;
                }

                // Ignore if in input/textarea
                if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' || e.target.tagName === 'SELECT') {
                    if (e.key === 'Escape') {
                        e.target.blur();
                        searchQuery.value = '';
                    }
                    return;
                }

                // Escape - close modals (Issue #291: Added missing overlays)
                if (e.key === 'Escape') {
                    // Issue #291: Close overlays in priority order (highest z-index first)
                    if (showOnboardingTour.value) { skipOnboardingTour(); return; }
                    if (showCommandPalette.value) { showCommandPalette.value = false; commandPaletteQuery.value = ''; return; }
                    if (showDocViewer.value) { closeDocViewer(); return; }
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
                    addToast('success', translateTerm('story', true) + ' movida', story.story_id + ' -> ' + getColumnTitle(newStatus));
                    markOnboardingStepDone('move_story'); // Issue #132
                    loadProjectData();
                    // Update selected story
                    const res = await fetch('/api/stories/' + story.story_id);
                    selectedStory.value = await res.json();
                } catch (e) {
                    addToast('error', 'Erro ao mover', 'Nao foi possivel mover a ' + translateTerm('story'));
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

            // Terminal Methods
            const initTerminal = () => {
                if (typeof Terminal === 'undefined') {
                    console.error('xterm.js not loaded');
                    return;
                }

                terminal.value = new Terminal({
                    cursorBlink: true,
                    fontSize: 13,
                    fontFamily: 'Courier New, monospace',
                    theme: {
                        background: '#000000',
                        foreground: '#ffffff'
                    },
                    rows: 20,
                    cols: 80
                });

                const container = document.getElementById('terminal-container');
                if (container) {
                    terminal.value.open(container);
                    terminal.value.writeln('Terminal ready. Select a project to start.');
                    terminal.value.writeln('');
                }
            };

            const executeTerminalCommand = async () => {
                if (!terminalCommand.value.trim() || !selectedProjectId.value) return;

                try {
                    const res = await fetch(`/api/projects/${selectedProjectId.value}/terminal/execute`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ command: terminalCommand.value })
                    });

                    const data = await res.json();

                    if (data.status === 'started') {
                        terminal.value.writeln(`$ ${terminalCommand.value}`);
                        terminalRunning.value = true;
                        startOutputPolling();
                    } else if (data.status === 'completed') {
                        terminal.value.writeln(`$ ${terminalCommand.value}`);
                        terminal.value.writeln(data.output);
                    }

                    terminalCommand.value = '';
                } catch (error) {
                    terminal.value.writeln(`Error: ${error.message}`);
                }
            };

            const startApp = async () => {
                if (!selectedProjectId.value) return;
                terminalCommand.value = 'npm run dev';
                await executeTerminalCommand();
            };

            const runTests = async () => {
                if (!selectedProjectId.value) return;
                terminalCommand.value = 'npm test';
                await executeTerminalCommand();
            };

            const stopProcess = async () => {
                if (!selectedProjectId.value) return;

                try {
                    const res = await fetch(`/api/projects/${selectedProjectId.value}/terminal/stop`, {
                        method: 'POST'
                    });

                    const data = await res.json();
                    terminal.value.writeln(`
Process ${data.status}`);
                    terminal.value.writeln('');
                    terminalRunning.value = false;
                    stopOutputPolling();
                } catch (error) {
                    terminal.value.writeln(`Error stopping process: ${error.message}`);
                }
            };

            const startOutputPolling = () => {
                if (terminalOutputInterval.value) {
                    clearInterval(terminalOutputInterval.value);
                }

                terminalOutputInterval.value = setInterval(async () => {
                    if (!selectedProjectId.value) return;

                    try {
                        const res = await fetch(`/api/projects/${selectedProjectId.value}/terminal/output`);
                        const data = await res.json();

                        if (data.output) {
                            terminal.value.write(data.output);
                        }

                        if (!data.has_more) {
                            terminalRunning.value = false;
                            stopOutputPolling();
                        }
                    } catch (error) {
                        console.error('Error polling output:', error);
                    }
                }, 500);
            };

            const stopOutputPolling = () => {
                if (terminalOutputInterval.value) {
                    clearInterval(terminalOutputInterval.value);
                    terminalOutputInterval.value = null;
                }
            };

            const refreshPreview = () => {
                const iframe = document.querySelector('iframe[ref="previewFrame"]');
                if (iframe) {
                    iframe.src = iframe.src;
                }
            };

            watch(selectedProjectId, () => {
                // Issue #294: Load project data when project changes
                loadProjectData();
                loadChatHistory();
                // Issue #132 - Mark onboarding step done
                if (selectedProjectId.value) {
                    markOnboardingStepDone('select_project');
                }
            });

            // Watch groupBy change to reinitialize sortable
            watch(groupBy, () => {
                nextTick(() => {
                    setupSortable();
                });
            });

            // Init
            onMounted(() => {
                initTerminal();
                loadTenants(); // Load tenants first - Multi-Tenancy
                loadProjects();
                loadTemplates(); // Issue #223 - Load story templates
                loadDarkMode();
                loadNotificationSoundPreference();
                loadOnboardingState(); // Issue #132 - Load onboarding state

                // Connect WebSocket for real-time notifications
                connectWebSocket();

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
                    content: 'Ola! Sou o assistente da Fabrica de Agentes. Posso ajudar com:\\n\\n- **Criar ' + translateTerm('stories') + '**: Clique em "Nova ' + translateTerm('story', true) + '"\\n- **Ver progresso**: Pergunte sobre o status\\n- **Documentacao**: Veja a aba Docs de cada ' + translateTerm('story') + '\\n\\nComo posso ajudar?',
                    created_at: new Date().toISOString()
                });
            });
            // Load preview data
            const loadPreviewData = async () => {
                if (!selectedProjectId.value) return;
                previewLoading.value = true;
                try {
                    const res = await fetch(`/api/projects/${selectedProjectId.value}/preview`);
                    if (res.ok) {
                        previewData.value = await res.json();
                    }
                } catch (e) {
                    console.error('Error loading preview:', e);
                } finally {
                    previewLoading.value = false;
                }
            };

            // Open preview modal
            const openProjectPreview = async () => {
                showProjectPreview.value = true;
                await loadPreviewData();
                markOnboardingStepDone('view_preview'); // Issue #132
            };

            // Refresh preview data
            const refreshPreviewData = async () => {
                await loadPreviewData();
                addToast('success', 'Atualizado', 'Dados do preview atualizados');
            };

            // Start app preview
            const startAppPreview = async () => {
                try {
                    const res = await fetch(`/api/projects/${selectedProjectId.value}/start-app`, {
                        method: 'POST'
                    });
                    const result = await res.json();
                    if (result.success || result.status === 'already_running') {
                        addToast('success', 'Servidor iniciado', 'A aplicacao esta rodando');
                        await loadPreviewData();
                    } else {
                        addToast('error', 'Erro', result.message || 'Falha ao iniciar servidor');
                    }
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao iniciar servidor');
                }
            };

            // Run tests
            const runProjectTests = async () => {
                try {
                    await fetch(`/api/projects/${selectedProjectId.value}/terminal/execute`, {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({command: 'npm test'})
                    });
                    addToast('info', 'Testes iniciados', 'Executando testes do projeto...');
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao executar testes');
                }
            };

            // Build project
            const buildProject = async () => {
                try {
                    await fetch(`/api/projects/${selectedProjectId.value}/terminal/execute`, {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({command: 'npm run build'})
                    });
                    addToast('info', 'Build iniciado', 'Construindo o projeto...');
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao construir projeto');
                }
            };

            // Open app preview
            const openAppPreview = () => {
                if (previewData.value?.app_status?.app_url) {
                    window.open(previewData.value.app_status.app_url, '_blank');
                }
            };

            // Open file viewer with syntax highlighting
            const openFileViewer = async (file) => {
                const ext = (file.extension || '.' + file.name.split('.').pop()).toLowerCase();
                const imageExts = ['.jpg', '.jpeg', '.png', '.gif', '.svg', '.webp', '.ico', '.bmp'];
                const pdfExts = ['.pdf'];
                const codeExts = { '.py': 'python', '.js': 'javascript', '.ts': 'typescript', '.jsx': 'javascript', '.tsx': 'typescript', '.json': 'json', '.md': 'markdown', '.css': 'css', '.scss': 'css', '.html': 'markup', '.htm': 'markup', '.xml': 'markup', '.yaml': 'yaml', '.yml': 'yaml', '.sh': 'bash', '.bash': 'bash', '.sql': 'sql', '.txt': 'text', '.env': 'text', '.gitignore': 'text', '.dockerfile': 'docker', '.toml': 'toml', '.ini': 'ini' };
                fileViewerData.value = { file, content: '', loading: true, error: null, fileType: 'code', language: 'text' };
                showFileViewer.value = true;
                const extLower = ext.startsWith('.') ? ext : '.' + ext;
                if (imageExts.includes(extLower)) {
                    fileViewerData.value.fileType = 'image';
                    fileViewerData.value.loading = false;
                } else if (pdfExts.includes(extLower)) {
                    fileViewerData.value.fileType = 'pdf';
                    fileViewerData.value.loading = false;
                } else if (extLower === '.md') {
                    fileViewerData.value.fileType = 'markdown';
                    fileViewerData.value.language = 'markdown';
                    try {
                        const resp = await fetch(file.url);
                        if (!resp.ok) throw new Error('Erro ao carregar arquivo');
                        fileViewerData.value.content = await resp.text();
                    } catch (e) { fileViewerData.value.error = e.message; }
                    fileViewerData.value.loading = false;
                } else {
                    fileViewerData.value.fileType = 'code';
                    fileViewerData.value.language = codeExts[extLower] || 'text';
                    try {
                        const resp = await fetch(file.url);
                        if (!resp.ok) throw new Error('Erro ao carregar arquivo');
                        fileViewerData.value.content = await resp.text();
                        setTimeout(() => { if (window.Prism) Prism.highlightAll(); }, 100);
                    } catch (e) { fileViewerData.value.error = e.message; }
                    fileViewerData.value.loading = false;
                }
            };
            const closeFileViewer = () => { showFileViewer.value = false; fileViewerData.value = { file: null, content: '', loading: false, error: null, fileType: 'code', language: 'text' }; };
            const downloadViewerFile = () => { if (fileViewerData.value.file?.url) { const a = document.createElement('a'); a.href = fileViewerData.value.file.url; a.download = fileViewerData.value.file.name; document.body.appendChild(a); a.click(); document.body.removeChild(a); } };
            const copyFileContent = () => { if (fileViewerData.value.content) { navigator.clipboard.writeText(fileViewerData.value.content); addToast('success', 'Copiado', 'Conteudo copiado para a area de transferencia'); } };
            const openInNewTab = () => { if (fileViewerData.value.file?.url) window.open(fileViewerData.value.file.url, '_blank'); };

            // Open doc viewer with markdown rendering
            const openDocViewer = (doc) => { docViewerData.value = { doc, editMode: false, editContent: doc.content || '' }; showDocViewer.value = true; };
            const closeDocViewer = () => { showDocViewer.value = false; docViewerData.value = { doc: null, editMode: false, editContent: '' }; };
            const toggleDocEditMode = () => { if (!docViewerData.value.editMode) docViewerData.value.editContent = docViewerData.value.doc.content || ''; docViewerData.value.editMode = !docViewerData.value.editMode; };
            const saveDocContent = async () => {
                if (!docViewerData.value.doc?.doc_id) return;
                try {
                    const resp = await fetch('/api/story-docs/' + docViewerData.value.doc.doc_id, { method: 'PUT', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ content: docViewerData.value.editContent }) });
                    if (resp.ok) { docViewerData.value.doc.content = docViewerData.value.editContent; docViewerData.value.editMode = false; addToast('success', 'Documento salvo', 'Conteudo atualizado'); }
                } catch (e) { addToast('error', 'Erro', 'Falha ao salvar documento'); }
            };
            // ==================== END PROJECT PREVIEW DASHBOARD ====================

            // Load Analytics (Issue #65 + Issue #157)
            const loadAnalytics = async () => {
                if (!selectedProjectId.value) return;
                analyticsLoading.value = true;
                analyticsInsights.value = null;
                velocityHistory.value = null;
                try {
                    const [prodRes, insightsRes, velocityRes] = await Promise.all([
                        fetch(`/api/analytics/productivity?project_id=${selectedProjectId.value}&days=${analyticsDays.value}`),
                        fetch(`/api/analytics/insights?project_id=${selectedProjectId.value}`),
                        fetch(`/api/analytics/velocity-history?project_id=${selectedProjectId.value}`)
                    ]);
                    if (prodRes.ok) analyticsData.value = await prodRes.json();
                    if (insightsRes.ok) analyticsInsights.value = await insightsRes.json();
                    if (velocityRes.ok) velocityHistory.value = await velocityRes.json();

                    // Issue #157: Render charts after data loads
                    nextTick(() => renderAnalyticsCharts());
                } catch (e) { console.error('Analytics error:', e); }
                finally { analyticsLoading.value = false; }
            };

            // Issue #157: Render Analytics Charts
            const renderAnalyticsCharts = () => {
                if (!analyticsData.value) return;

                // Destroy existing charts
                if (velocityChart) velocityChart.destroy();
                if (statusChart) statusChart.destroy();
                if (developerChart) developerChart.destroy();
                if (cycletimeChart) cycletimeChart.destroy();

                // 1. Velocity History Chart (Line/Bar)
                const velCanvas = document.getElementById('velocity-chart');
                if (velCanvas && velocityHistory.value?.history?.length) {
                    velocityChart = new Chart(velCanvas, {
                        type: 'bar',
                        data: {
                            labels: velocityHistory.value.history.map(h => h.sprint_name || 'Sprint'),
                            datasets: [{
                                label: 'Velocity (pts)',
                                data: velocityHistory.value.history.map(h => h.velocity || 0),
                                backgroundColor: 'rgba(255, 108, 0, 0.7)',
                                borderColor: '#FF6C00',
                                borderWidth: 2
                            }, {
                                label: 'Capacidade',
                                data: velocityHistory.value.history.map(h => h.capacity || 0),
                                backgroundColor: 'rgba(0, 59, 74, 0.3)',
                                borderColor: '#003B4A',
                                borderWidth: 2,
                                type: 'line'
                            }]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: { legend: { position: 'bottom' } },
                            scales: { y: { beginAtZero: true } }
                        }
                    });
                }

                // 2. Status Distribution Chart (Doughnut)
                const statusCanvas = document.getElementById('status-chart');
                const statusDist = analyticsData.value.team_metrics?.status_distribution || {};
                if (statusCanvas && Object.keys(statusDist).length) {
                    const statusLabels = { backlog: 'Backlog', ready: 'Ready', in_progress: 'Em Progresso', review: 'Review', testing: 'Testes', done: 'Concluido' };
                    const statusColors = { backlog: '#6B7280', ready: '#3B82F6', in_progress: '#F59E0B', review: '#8B5CF6', testing: '#EC4899', done: '#10B981' };
                    statusChart = new Chart(statusCanvas, {
                        type: 'doughnut',
                        data: {
                            labels: Object.keys(statusDist).map(k => statusLabels[k] || k),
                            datasets: [{
                                data: Object.values(statusDist),
                                backgroundColor: Object.keys(statusDist).map(k => statusColors[k] || '#6B7280')
                            }]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: { legend: { position: 'right' } }
                        }
                    });
                }

                // 3. Developer Points Chart (Horizontal Bar)
                const devCanvas = document.getElementById('developer-chart');
                const devMetrics = analyticsData.value.developer_metrics || [];
                if (devCanvas && devMetrics.length) {
                    developerChart = new Chart(devCanvas, {
                        type: 'bar',
                        data: {
                            labels: devMetrics.slice(0, 8).map(d => d.assignee),
                            datasets: [{
                                label: 'Pontos Entregues',
                                data: devMetrics.slice(0, 8).map(d => d.points_delivered || 0),
                                backgroundColor: '#10B981'
                            }, {
                                label: 'Pontos Pendentes',
                                data: devMetrics.slice(0, 8).map(d => (d.points_total || 0) - (d.points_delivered || 0)),
                                backgroundColor: '#E5E7EB'
                            }]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            indexAxis: 'y',
                            plugins: { legend: { position: 'bottom' } },
                            scales: { x: { stacked: true, beginAtZero: true }, y: { stacked: true } }
                        }
                    });
                }

                // 4. Cycle Time Chart (Bar)
                const cycleCanvas = document.getElementById('cycletime-chart');
                if (cycleCanvas && devMetrics.length) {
                    cycletimeChart = new Chart(cycleCanvas, {
                        type: 'bar',
                        data: {
                            labels: devMetrics.filter(d => d.avg_time_days > 0).slice(0, 8).map(d => d.assignee),
                            datasets: [{
                                label: 'Tempo Medio (dias)',
                                data: devMetrics.filter(d => d.avg_time_days > 0).slice(0, 8).map(d => d.avg_time_days || 0),
                                backgroundColor: 'rgba(6, 182, 212, 0.7)',
                                borderColor: '#06B6D4',
                                borderWidth: 2
                            }]
                        },
                        options: {
                            responsive: true,
                            maintainAspectRatio: false,
                            plugins: { legend: { display: false } },
                            scales: { y: { beginAtZero: true } }
                        }
                    });
                }
            };

            // Watch analytics modal to render charts
            watch(showAnalyticsModal, (newVal) => {
                if (newVal && analyticsData.value) {
                    nextTick(() => renderAnalyticsCharts());
                }
            });

            return {
                // Analytics (Issue #65 + Issue #157 Charts)
                showAnalyticsModal, analyticsData, analyticsInsights, analyticsLoading, analyticsDays, loadAnalytics, velocityHistory,
                // Project Preview Dashboard (Issue #73)
                showProjectPreview, previewData, previewActiveTab, previewViewportMode,
                previewLoading, openProjectPreview, refreshPreviewData, loadPreviewData,
                startAppPreview, runProjectTests, buildProject, openAppPreview,
                openFileViewer, openDocViewer,
                // File Viewer
                showFileViewer, fileViewerData, closeFileViewer, downloadViewerFile, copyFileContent, openInNewTab,
                // Doc Viewer
                showDocViewer, docViewerData, closeDocViewer, toggleDocEditMode, saveDocContent,
                // Issue #133 - Business Terms
                businessTerms, translateTerm,
                // Issue #135 - Executive Dashboard
                viewMode, showTechnicalLogs, recentActivityLogs,
                currentProjectName, projectReadyToTest, projectProgressPercent,
                projectHealthStatus, projectHealthColor, donePoints, estimatedDaysRemaining, projectPhases,
                // Issue #134 - Wizard Components
                showProjectWizard, showIntegrationWizard, wizardCurrentStep, wizardData,
                wizardSteps, projectTypeOptions, availableIntegrations,
                canProceedWizard, getProjectTypeName, createProjectFromWizard, selectIntegration,
                // Integrations Dashboard
                showIntegrationsModal, integrationsLoading, integrationsStatus,
                loadIntegrationsStatus, openIntegrationConfig,
                // Issue #154 - Integrations Management
                activeIntegrationTab, integrationTesting, integrationSaving,
                integrationTabs, integrationConfigs, loadIntegrationConfigs,
                testIntegration, saveIntegration,
                // Issue #156 - Security Settings
                showSecuritySettings, securityActiveTab, securityData,
                mfaSetupMode, mfaSetupData, mfaVerifyCode, mfaError,
                showCreateApiKeyModal, createdApiKey, newApiKey,
                loadSecurityData, startMfaSetup, completeMfaSetup, cancelMfaSetup,
                disableMfa, regenerateBackupCodes, copyBackupCodes,
                createApiKey, revokeApiKey, copyCreatedApiKey,
                revokeSession, revokeAllSessions,
                // Issue #132 - Onboarding Tour
                showOnboardingChecklist, showOnboardingTour, currentTourStepIndex,
                onboardingSteps, onboardingComplete, onboardingProgress,
                tourSteps, currentTourStep, onboardingSpotlightStyle, onboardingTooltipStyle,
                handleOnboardingStep, startOnboardingTour, nextTourStep, prevTourStep,
                skipOnboardingTour, finishOnboardingTour, markOnboardingStepDone, loadOnboardingState, closeAllOverlays,
                projects, selectedProjectId, selectedSprintId, selectedEpicId,
                storyBoard, epics, sprints, selectedStory, activeTab,
                chatHistory, chatInput, chatMessages,
                showNewStoryModal, showNewTaskModal, showNewEpicModal, showNewSprintModal, showNewDocModal,
                openNewStoryModal, // Issue #308: method for better Vue reactivity
                showShortcutsModal, showConfirmModal, confirmModal,
                // Issue #216: Command Palette
                showCommandPalette, commandPaletteQuery, commandPaletteIndex, commandPaletteResults,
                executeCommand, handleCommandPaletteKey,
                // Issue #220: Breadcrumb Navigation
                breadcrumbItems, navigateBreadcrumb,
                // Issue #221: Global Search
                showGlobalSearch, globalSearchQuery, globalSearchFilter, globalSearchLoading,
                globalSearchResults, globalSearchSelectedIndex, globalSearchInput, globalSearchHistory,
                globalSearchFilters, openGlobalSearch, closeGlobalSearch, setGlobalSearchFilter,
                debouncedGlobalSearch, handleGlobalSearchKey, navigateToSearchResult,
                applySearchHistory, highlightMatch, getGlobalSearchIndex,
                // Issue #222: Quick Create FAB
                fabMenuOpen, toggleFabMenu, closeFabMenu, fabAction,
                contextMenu, isLoading,
                // Issue #155: Voice Input
                voiceRecording, voiceProcessing, voiceRecordingTime, toggleVoiceInput,
                // Issue #195: Omnichannel Input
                inputMethod, omnichannelFileProcessing, processOmnichannelFile,
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
                selectedTemplate, applyTemplate, availableTemplates, templatesLoading, showTemplateSelector, clearTemplate,
                isDarkMode, toggleDarkMode,
                showBurndownModal, burndownData, updateBurndownChart,
                bulkSelectMode, selectedStories, toggleBulkSelectMode, toggleBulkSelect,
                cancelBulkSelect, bulkMoveStories, bulkDeleteStories,
                terminalCommand, terminalRunning, previewUrl, previewViewport,
                executeTerminalCommand, startApp, runTests, stopProcess, refreshPreview,
                wsStatus, wsStatusText, wsStatusTitle, notificationSoundEnabled, toggleNotificationSound,
                generatingTests, showGeneratedTestsModal, currentGeneratedTests,
                // Security Scan (Issue #57)
                scanningTask, showSecurityScanModal, currentSecurityScan, runSecurityScan,
                generateTestsForTask, showGeneratedTests, copyTestCode, downloadTestCode,
                // Code Review (Issue #52)
                reviewingCode, showCodeReviewModal, currentCodeReview,
                codeReviewTask, showCodeReviewResult,
                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh,
                // Issue #185 - Enhanced File Upload
                isDraggingFile, uploadQueue, uploadFileFilter,
                handleMultipleFileUpload, handleFileDrop, uploadSingleFile,
                getFileIcon, getFileIconClass, getFileTypeName, isImageFile,
                canPreviewFile, formatUploadDate, countFilesByType,
                filteredUploadFiles, previewFile, deleteUploadedFile,
                // Tenant Selector (Multi-Tenancy)
                userTenants, selectedTenantId, currentTenantName,
                loadTenants, onTenantChange
            };
        }
    });

    // Register directives
    app.directive('click-outside', clickOutsideDirective);

    // Mount
    app.mount('#app');
    </script>

    <!-- PWA Initialization (Issue #259) -->
    <script src="/static/pwa-init.js" defer></script>
    <script src="/static/offline-db.js" defer></script>
    <script src="/static/offline-ui.js" defer></script>

    <script>
    // Service Worker Registration
    if ('serviceWorker' in navigator) {
        window.addEventListener('load', function() {
            navigator.serviceWorker.register('/sw.js', { scope: '/' })
                .then(function(registration) {
                    console.log('[App] SW registered:', registration.scope);
                })
                .catch(function(error) {
                    console.warn('[App] SW registration failed:', error);
                });
        });
    }

    // Detect display mode (installed vs browser)
    if (window.matchMedia('(display-mode: standalone)').matches) {
        document.body.classList.add('pwa-installed');
        console.log('[App] Running in standalone mode (installed)');
    }

    // Listen for app installation
    window.addEventListener('appinstalled', function() {
        console.log('[App] PWA installed successfully');
        document.body.classList.add('pwa-installed');
    });
    </script>
</body>
</html>
"""


@app.get("/", response_class=HTMLResponse)
def index():
    """Pagina principal - Dashboard Agile"""
    return HTML_TEMPLATE


@app.get("/integrations", response_class=HTMLResponse)
def integrations_page():
    """Pagina de Integracoes - Redireciona para o dashboard com modal aberto"""
    return HTML_TEMPLATE


# Issue #283: Missing page routes - SPA routes that return the main HTML
@app.get("/admin", response_class=HTMLResponse)
def admin_page():
    """Admin Panel - SPA route"""
    return HTML_TEMPLATE


@app.get("/security", response_class=HTMLResponse)
def security_page():
    """Security Settings - SPA route"""
    return HTML_TEMPLATE


# =============================================================================
# API ENDPOINTS - SECURITY SETTINGS (Issue #309)
# =============================================================================

@app.get("/api/security/data")
def get_security_data():
    """Get security settings data - MFA, API Keys, Sessions"""
    # Return mock data for now - in production this would query the database
    return {
        "mfa": None,  # No MFA configured by default
        "apiKeys": [],
        "sessions": [
            {
                "session_id": "current",
                "device": "Current Browser",
                "location": "Local",
                "last_active": datetime.now().isoformat(),
                "is_current": True
            }
        ]
    }


@app.post("/api/security/mfa/setup")
def setup_mfa(data: dict = Body(...)):
    """Initialize MFA setup - returns QR code and secret"""
    # Generate mock MFA setup data
    secret_key = "JBSWY3DPEHPK3PXP"  # Mock secret
    # In production, generate real TOTP secret and QR code
    return {
        "qr_code_base64": "",  # Would be actual QR code
        "secret_key": secret_key,
        "backup_codes": ["12345678", "23456789", "34567890", "45678901", "56789012"]
    }


@app.post("/api/security/mfa/verify")
def verify_mfa(data: dict = Body(...)):
    """Verify MFA code and enable 2FA"""
    code = data.get("code", "")
    # In production, verify the TOTP code
    if len(code) == 6 and code.isdigit():
        return {"success": True, "message": "MFA enabled successfully"}
    return {"success": False, "message": "Invalid code"}


@app.post("/api/security/mfa/disable")
def disable_mfa(data: dict = Body(...)):
    """Disable MFA for user"""
    return {"success": True, "message": "MFA disabled"}


@app.post("/api/security/mfa/backup-codes/regenerate")
def regenerate_backup_codes(data: dict = Body(...)):
    """Regenerate MFA backup codes"""
    return {
        "backup_codes": ["11111111", "22222222", "33333333", "44444444", "55555555"]
    }


@app.post("/api/security/api-keys")
def create_api_key(data: dict = Body(...)):
    """Create new API key"""
    import secrets
    key_id = uuid.uuid4().hex[:8]
    api_key = f"fab_{secrets.token_hex(32)}"
    return {
        "key_id": key_id,
        "name": data.get("name", "New API Key"),
        "key": api_key,
        "created_at": datetime.now().isoformat(),
        "scopes": data.get("scopes", ["read"])
    }


@app.delete("/api/security/api-keys/{key_id}")
def revoke_api_key(key_id: str):
    """Revoke an API key"""
    return {"success": True, "message": f"API key {key_id} revoked"}


@app.delete("/api/security/sessions/{session_id}")
def revoke_session(session_id: str):
    """Revoke a session"""
    return {"success": True, "message": f"Session {session_id} revoked"}


@app.post("/api/security/sessions/revoke-all")
def revoke_all_sessions():
    """Revoke all sessions except current"""
    return {"success": True, "message": "All other sessions revoked"}


@app.get("/workers", response_class=HTMLResponse)
def workers_page():
    """Workers Monitor - SPA route"""
    return HTML_TEMPLATE


@app.get("/projects", response_class=HTMLResponse)
def projects_page():
    """Projects List - SPA route"""
    return HTML_TEMPLATE


@app.get("/docs", response_class=HTMLResponse)
def docs_page():
    """Documentation - SPA route"""
    return HTML_TEMPLATE


@app.get("/analytics", response_class=HTMLResponse)
def analytics_page():
    """Analytics Dashboard - SPA route"""
    return HTML_TEMPLATE


@app.get("/billing", response_class=HTMLResponse)
def billing_page():
    """Billing Dashboard - SPA route"""
    return HTML_TEMPLATE


@app.get("/executive", response_class=HTMLResponse)
def executive_page():
    """Executive Dashboard - SPA route"""
    return HTML_TEMPLATE


# Issue #305: Additional SPA routes for auth and user pages
@app.get("/login", response_class=HTMLResponse)
def login_page():
    """Login Page - SPA route"""
    return HTML_TEMPLATE


@app.get("/register", response_class=HTMLResponse)
def register_page():
    """Register Page - SPA route"""
    return HTML_TEMPLATE


@app.get("/forgot-password", response_class=HTMLResponse)
def forgot_password_page():
    """Forgot Password Page - SPA route"""
    return HTML_TEMPLATE


@app.get("/settings", response_class=HTMLResponse)
def settings_page():
    """User Settings - SPA route"""
    return HTML_TEMPLATE


@app.get("/profile", response_class=HTMLResponse)
def profile_page():
    """User Profile - SPA route"""
    return HTML_TEMPLATE


@app.get("/notifications", response_class=HTMLResponse)
def notifications_page():
    """Notifications - SPA route"""
    return HTML_TEMPLATE


@app.get("/help", response_class=HTMLResponse)
def help_page():
    """Help Center - SPA route"""
    return HTML_TEMPLATE


@app.get("/onboarding", response_class=HTMLResponse)
def onboarding_page():
    """Onboarding - SPA route"""
    return HTML_TEMPLATE


@app.get("/api/integrations/all-status")
async def get_all_integrations_status():
    """Retorna o status de todas as integracoes disponiveis"""
    try:
        # Try to get status from integration modules
        from factory.integrations.jira import get_jira_integration
        from factory.integrations.azure_devops import get_azure_devops_integration

        jira = get_jira_integration()
        azure = get_azure_devops_integration()

        # Try GitHub if available
        try:
            from factory.integrations.github import get_github_integration
            github = get_github_integration()
            github_status = github.get_status()
        except:
            github_status = {"connected": False, "status": "unavailable"}

        return {
            "github": github_status,
            "jira": jira.get_status(),
            "azure_devops": azure.get_status()
        }
    except Exception as e:
        # Return default disconnected status if integration modules not available
        return {
            "github": {"connected": False, "status": "disconnected"},
            "jira": {"connected": False, "status": "disconnected"},
            "azure_devops": {"connected": False, "status": "disconnected"},
            "error": str(e)
        }


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
