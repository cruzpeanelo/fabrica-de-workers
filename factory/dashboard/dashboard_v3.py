"""
Dashboard de Controle v3.0 - Plataforma E
===============================================

Dashboard completamente redesenhado com:
- Visualizacao de organograma por area (Negocios vs TI)
- Perfis detalhados de agentes com experiencias e habilidades
- Metricas de confiabilidade e produtividade
- Timeout editavel por agente decisor
- UX/UI moderna e intuitiva

Acesse em: http://localhost:9000
"""

import json
import secrets
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional
from fastapi import FastAPI, HTTPException, Depends, Query
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel
import uvicorn
from jose import JWTError, jwt
from passlib.context import CryptContext

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import init_db, SessionLocal
from factory.database.models import (
    Project, Story, Agent, Skill, Task, ActivityLog, FactoryEvent, Template, User
)
from factory.database.repositories import (
    ProjectRepository, StoryRepository, AgentRepository, SkillRepository,
    TaskRepository, ActivityLogRepository, FactoryEventRepository, TemplateRepository
)
from factory.config import DASHBOARD_HOST, DASHBOARD_PORT, DASHBOARD_TITLE, AGENTS

# Import corporate hierarchy
try:
    from factory.agents.corporate_hierarchy import (
        HierarchyApprovalSystem, ALL_CORPORATE_AGENTS,
        get_agents_by_area, get_hierarchy_statistics, AgentStatus
    )
    HAS_CORPORATE_HIERARCHY = True
except ImportError:
    HAS_CORPORATE_HIERARCHY = False

# Import profile service
try:
    from factory.agents.profile_service import get_profile_service, ProfileService
    HAS_PROFILE_SERVICE = True
except ImportError:
    HAS_PROFILE_SERVICE = False

# Initialize database
init_db()

# FastAPI App
app = FastAPI(title=DASHBOARD_TITLE, version="3.0.0")

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


class TimeoutUpdate(BaseModel):
    timeout_hours: float


class ProjectCreate(BaseModel):
    name: str
    description: Optional[str] = None
    project_type: str
    template_id: Optional[str] = None
    config: Optional[dict] = None


class ProjectUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    progress: Optional[float] = None


class StoryCreate(BaseModel):
    title: str
    description: Optional[str] = None
    project_id: Optional[str] = None
    sprint: int = 1
    points: int = 0
    priority: int = 5


class StoryUpdate(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    sprint: Optional[int] = None
    points: Optional[int] = None
    priority: Optional[int] = None


class AgentUpdate(BaseModel):
    status: Optional[str] = None
    priority: Optional[int] = None
    enabled: Optional[bool] = None


# =============================================================================
# AUTH HELPERS
# =============================================================================

def create_access_token(data: dict) -> str:
    to_encode = data.copy()
    expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    to_encode.update({"exp": expire})
    return jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)


def get_current_user(credentials: HTTPAuthorizationCredentials = Depends(security)):
    if not credentials:
        return None
    try:
        payload = jwt.decode(credentials.credentials, SECRET_KEY, algorithms=[ALGORITHM])
        return payload
    except JWTError:
        return None


# =============================================================================
# AUTH ENDPOINTS
# =============================================================================

@app.post("/api/auth/login")
async def login(request: LoginRequest):
    db = SessionLocal()
    try:
        user = db.query(User).filter(User.username == request.username).first()
        if not user or not pwd_context.verify(request.password, user.password_hash):
            raise HTTPException(status_code=401, detail="Credenciais invalidas")

        token = create_access_token({"sub": user.username, "role": user.role})
        return {"token": token, "user": {"username": user.username, "role": user.role}}
    finally:
        db.close()


# =============================================================================
# PROFILE ENDPOINTS
# =============================================================================

@app.get("/api/profiles")
async def list_profiles(area: str = None, department: str = None):
    """Lista perfis de agentes"""
    if not HAS_PROFILE_SERVICE:
        return {"profiles": [], "error": "Profile service not available"}

    service = get_profile_service()

    if area:
        profiles = service.get_profiles_by_area(area)
    elif department:
        profiles = service.get_profiles_by_department(department)
    else:
        profiles = service.get_all_profiles()

    return {
        "profiles": [p.to_dict() for p in profiles],
        "total": len(profiles)
    }


@app.get("/api/profiles/{agent_id}")
async def get_profile(agent_id: str):
    """Busca perfil detalhado de um agente"""
    if not HAS_PROFILE_SERVICE:
        raise HTTPException(status_code=503, detail="Profile service not available")

    service = get_profile_service()
    profile = service.get_profile(agent_id)

    if not profile:
        raise HTTPException(status_code=404, detail="Profile not found")

    return {"profile": profile.to_dict()}


@app.put("/api/profiles/{agent_id}/timeout")
async def update_agent_timeout(agent_id: str, data: TimeoutUpdate):
    """Atualiza timeout de aprovacao de um agente decisor"""
    if not HAS_PROFILE_SERVICE:
        raise HTTPException(status_code=503, detail="Profile service not available")

    service = get_profile_service()
    success = service.update_timeout(agent_id, data.timeout_hours)

    if not success:
        raise HTTPException(status_code=400, detail="Agent is not a decision maker or not found")

    return {"success": True, "timeout_hours": data.timeout_hours}


@app.get("/api/profiles/decision-makers")
async def list_decision_makers():
    """Lista agentes decisores"""
    if not HAS_PROFILE_SERVICE:
        return {"decision_makers": []}

    service = get_profile_service()
    profiles = service.get_decision_makers()

    return {
        "decision_makers": [
            {
                "agent_id": p.agent_id,
                "name": p.name,
                "title": p.title,
                "department": p.department,
                "level": p.level,
                "timeout_hours": p.approval_timeout_hours,
                "decisions_made": p.decisions_made
            }
            for p in profiles
        ],
        "total": len(profiles)
    }


@app.get("/api/profiles/org-chart")
async def get_org_chart():
    """Retorna dados do organograma"""
    if not HAS_PROFILE_SERVICE:
        return {"business": {}, "technology": {}}

    service = get_profile_service()
    return service.get_org_chart_data()


@app.get("/api/profiles/hierarchy/{area}")
async def get_hierarchy_by_area(area: str):
    """Retorna hierarquia de uma area especifica (business/technology)"""
    if not HAS_PROFILE_SERVICE:
        return {"roots": [], "nodes": {}}

    if area not in ["business", "technology"]:
        raise HTTPException(status_code=400, detail="Area must be 'business' or 'technology'")

    service = get_profile_service()
    return service.get_hierarchy_by_area(area)


@app.get("/api/profiles/top-performers")
async def get_top_performers(limit: int = 10):
    """Retorna top performers"""
    if not HAS_PROFILE_SERVICE:
        return {"top_performers": []}

    service = get_profile_service()
    return {"top_performers": service.get_top_performers(limit)}


# =============================================================================
# EXISTING ENDPOINTS (mantidos do dashboard original)
# =============================================================================

@app.get("/api/status")
async def get_status():
    """Status geral do sistema"""
    db = SessionLocal()
    try:
        agents = db.query(Agent).filter(Agent.enabled == True).all()
        projects = db.query(Project).all()
        stories = db.query(Story).all()
        logs = db.query(ActivityLog).order_by(ActivityLog.timestamp.desc()).limit(20).all()

        working = len([a for a in agents if a.status in ['WORKING', 'EXECUTING']])

        return {
            "factory": {
                "status": "running",
                "uptime": "active"
            },
            "agents": {
                "total": len(agents),
                "working": working,
                "standby": len(agents) - working
            },
            "projects": {
                "total": len(projects),
                "active": len([p for p in projects if p.status == 'active'])
            },
            "stories": {
                "total": len(stories),
                "in_progress": len([s for s in stories if s.status == 'in_progress']),
                "completed": len([s for s in stories if s.status == 'done'])
            },
            "recent_logs": [l.to_dict() for l in logs]
        }
    finally:
        db.close()


@app.get("/api/projects")
async def list_projects():
    """Lista projetos"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        projects = repo.get_all()
        return {"projects": [p.to_dict() for p in projects]}
    finally:
        db.close()


@app.get("/api/agents")
async def list_agents(domain: str = None, status: str = None):
    """Lista agentes"""
    db = SessionLocal()
    try:
        repo = AgentRepository(db)
        if domain:
            agents = repo.get_by_domain(domain)
        elif status:
            agents = repo.get_by_status(status)
        else:
            agents = repo.get_all()
        return {"agents": [a.to_dict() for a in agents]}
    finally:
        db.close()


@app.get("/api/agents/{agent_id}/activity")
async def get_agent_activity(agent_id: str, limit: int = 50):
    """Busca atividades recentes de um agente"""
    db = SessionLocal()
    try:
        logs = db.query(ActivityLog).filter(
            ActivityLog.agent_id == agent_id
        ).order_by(ActivityLog.timestamp.desc()).limit(limit).all()

        project_ids = set()
        for log in logs:
            if log.project_id:
                project_ids.add(log.project_id)

        return {
            "agent_id": agent_id,
            "recent_logs": [l.to_dict() for l in logs],
            "projects_worked": list(project_ids)
        }
    finally:
        db.close()


@app.get("/api/stories")
async def list_stories(project_id: str = None, sprint: int = None, status: str = None):
    """Lista stories"""
    db = SessionLocal()
    try:
        query = db.query(Story)
        if project_id:
            query = query.filter(Story.project_id == project_id)
        if sprint:
            query = query.filter(Story.sprint == sprint)
        if status:
            query = query.filter(Story.status == status)
        stories = query.order_by(Story.priority.desc(), Story.created_at.desc()).all()
        return {"stories": [s.to_dict() for s in stories]}
    finally:
        db.close()


@app.get("/api/skills")
async def list_skills(skill_type: str = None, category: str = None):
    """Lista skills"""
    db = SessionLocal()
    try:
        repo = SkillRepository(db)
        if skill_type:
            skills = repo.get_by_type(skill_type)
        elif category:
            skills = repo.get_by_category(category)
        else:
            skills = repo.get_all()
        return {"skills": [s.to_dict() for s in skills]}
    finally:
        db.close()


@app.get("/api/logs")
async def list_logs(limit: int = 100, level: str = None, agent_id: str = None, project_id: str = None):
    """Lista logs"""
    db = SessionLocal()
    try:
        query = db.query(ActivityLog)
        if level:
            query = query.filter(ActivityLog.level == level)
        if agent_id:
            query = query.filter(ActivityLog.agent_id == agent_id)
        if project_id:
            query = query.filter(ActivityLog.project_id == project_id)

        logs = query.order_by(ActivityLog.timestamp.desc()).limit(limit).all()
        return {"logs": [l.to_dict() for l in logs]}
    finally:
        db.close()


@app.get("/api/hierarchy")
async def get_hierarchy():
    """Retorna hierarquia corporativa"""
    if not HAS_CORPORATE_HIERARCHY:
        return {"agents": [], "total": 0}

    agents_list = []
    for agent_id, agent in ALL_CORPORATE_AGENTS.items():
        agents_list.append(agent.to_dict())

    return {
        "agents": agents_list,
        "total": len(agents_list)
    }


@app.get("/api/productivity")
async def get_productivity():
    """Retorna metricas de produtividade"""
    db = SessionLocal()
    try:
        agents = db.query(Agent).filter(Agent.enabled == True).all()
        productivity_list = []

        for agent in agents:
            logs = db.query(ActivityLog).filter(ActivityLog.agent_id == agent.agent_id).all()
            tasks_completed = len([l for l in logs if l.action == 'task_complete'])
            tasks_failed = len([l for l in logs if l.action == 'task_fail'])
            total_activities = len(logs)

            if tasks_completed + tasks_failed > 0:
                success_rate = (tasks_completed / (tasks_completed + tasks_failed)) * 100
            else:
                success_rate = 0

            productivity_list.append({
                "agent_id": agent.agent_id,
                "name": agent.name,
                "domain": agent.domain,
                "status": agent.status,
                "tasks_completed": tasks_completed,
                "tasks_failed": tasks_failed,
                "total_activities": total_activities,
                "success_rate": round(success_rate, 1),
                "productivity_score": round(success_rate * 0.7 + min(total_activities, 100) * 0.3, 1)
            })

        productivity_list.sort(key=lambda x: x['productivity_score'], reverse=True)

        working = len([a for a in agents if a.status in ['WORKING', 'EXECUTING']])
        standby = len([a for a in agents if a.status == 'STANDBY'])

        return {
            "agents": productivity_list,
            "summary": {
                "total": len(agents),
                "working": working,
                "standby": standby,
                "avg_productivity": round(sum(a['productivity_score'] for a in productivity_list) / len(productivity_list), 1) if productivity_list else 0
            },
            "top_performers": productivity_list[:5]
        }
    finally:
        db.close()


# =============================================================================
# DASHBOARD HTML - REDESIGNED v3.0
# =============================================================================

DASHBOARD_HTML = '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Plataforma E | Dashboard v3.0</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap" rel="stylesheet">
    <script>
        tailwind.config = {
            theme: {
                extend: {
                    colors: {
                        primary: { 50: '#eff6ff', 100: '#dbeafe', 200: '#bfdbfe', 300: '#93c5fd', 400: '#60a5fa', 500: '#3b82f6', 600: '#2563eb', 700: '#1d4ed8', 800: '#1e40af', 900: '#1e3a8a' },
                        dark: { 50: '#f8fafc', 100: '#f1f5f9', 200: '#e2e8f0', 300: '#cbd5e1', 400: '#94a3b8', 500: '#64748b', 600: '#475569', 700: '#334155', 800: '#1e293b', 900: '#0f172a', 950: '#020617' },
                        business: { 50: '#fef3c7', 100: '#fde68a', 200: '#fcd34d', 300: '#fbbf24', 400: '#f59e0b', 500: '#d97706', 600: '#b45309' },
                        tech: { 50: '#ecfeff', 100: '#cffafe', 200: '#a5f3fc', 300: '#67e8f9', 400: '#22d3ee', 500: '#06b6d4', 600: '#0891b2' },
                        success: '#10b981',
                        warning: '#f59e0b',
                        danger: '#ef4444'
                    },
                    fontFamily: { 'sans': ['Inter', 'sans-serif'] }
                }
            }
        }
    </script>
    <style>
        * { font-family: 'Inter', sans-serif; }
        .scrollbar-thin::-webkit-scrollbar { width: 6px; height: 6px; }
        .scrollbar-thin::-webkit-scrollbar-track { background: #1e293b; border-radius: 3px; }
        .scrollbar-thin::-webkit-scrollbar-thumb { background: #475569; border-radius: 3px; }
        .scrollbar-thin::-webkit-scrollbar-thumb:hover { background: #64748b; }

        .org-node { transition: all 0.3s ease; }
        .org-node:hover { transform: translateY(-2px); box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.3); }

        .skill-bar { transition: width 0.5s ease-out; }

        .tab-active { border-bottom: 2px solid #3b82f6; color: #3b82f6; }

        .glass { background: rgba(30, 41, 59, 0.8); backdrop-filter: blur(10px); }

        .gradient-business { background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%); }
        .gradient-tech { background: linear-gradient(135deg, #06b6d4 0%, #0891b2 100%); }

        .fade-in { animation: fadeIn 0.3s ease-out; }
        @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }

        .pulse-dot { animation: pulse 2s infinite; }
        @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
    </style>
</head>
<body class="bg-dark-950 text-white min-h-screen">
    <div id="app" class="min-h-screen">
        <!-- Header Moderno -->
        <header class="bg-dark-900/80 backdrop-blur-md border-b border-dark-700 sticky top-0 z-50">
            <div class="max-w-[1920px] mx-auto px-6 py-3">
                <div class="flex items-center justify-between">
                    <div class="flex items-center gap-4">
                        <div class="flex items-center gap-3">
                            <div class="w-10 h-10 rounded-xl bg-gradient-to-br from-primary-500 to-primary-700 flex items-center justify-center">
                                <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"/>
                                </svg>
                            </div>
                            <div>
                                <h1 class="text-xl font-bold">Plataforma E</h1>
                                <p class="text-xs text-dark-400">Plataforma de Agentes Autonomos</p>
                            </div>
                        </div>
                        <span class="px-2.5 py-1 text-xs bg-primary-500/20 text-primary-400 rounded-full font-medium">v3.0</span>
                    </div>

                    <div class="flex items-center gap-6">
                        <div class="flex items-center gap-3 px-4 py-2 bg-dark-800 rounded-lg">
                            <span class="w-2 h-2 rounded-full bg-success pulse-dot"></span>
                            <span class="text-sm text-dark-300">{{ status?.agents?.total || 0 }} Agentes</span>
                            <span class="text-dark-600">|</span>
                            <span class="text-sm text-success">{{ status?.agents?.working || 0 }} Ativos</span>
                        </div>
                        <div class="text-sm text-dark-400">{{ currentTime }}</div>
                    </div>
                </div>
            </div>
        </header>

        <div class="flex">
            <!-- Sidebar Moderna -->
            <aside class="w-72 min-h-[calc(100vh-65px)] bg-dark-900 border-r border-dark-800 p-4 sticky top-[65px]">
                <nav class="space-y-1">
                    <button @click="currentView = 'overview'"
                            :class="['w-full text-left px-4 py-3 rounded-xl flex items-center gap-3 transition-all font-medium',
                                     currentView === 'overview' ? 'bg-primary-500/20 text-primary-400' : 'hover:bg-dark-800 text-dark-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2V6z M14 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V6z M4 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2v-2z M14 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"/></svg>
                        Visao Geral
                    </button>

                    <button @click="currentView = 'agents'"
                            :class="['w-full text-left px-4 py-3 rounded-xl flex items-center gap-3 transition-all font-medium',
                                     currentView === 'agents' ? 'bg-primary-500/20 text-primary-400' : 'hover:bg-dark-800 text-dark-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"/></svg>
                        Equipe de Agentes
                        <span class="ml-auto px-2 py-0.5 text-xs bg-dark-700 rounded-full">{{ profiles.length }}</span>
                    </button>

                    <button @click="currentView = 'kanban'"
                            :class="['w-full text-left px-4 py-3 rounded-xl flex items-center gap-3 transition-all font-medium',
                                     currentView === 'kanban' ? 'bg-primary-500/20 text-primary-400' : 'hover:bg-dark-800 text-dark-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 17V7m0 10a2 2 0 01-2 2H5a2 2 0 01-2-2V7a2 2 0 012-2h2a2 2 0 012 2m0 10a2 2 0 002 2h2a2 2 0 002-2M9 7a2 2 0 012-2h2a2 2 0 012 2m0 10V7m0 10a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 00-2-2h-2a2 2 0 00-2 2"/></svg>
                        Kanban
                    </button>

                    <button @click="currentView = 'projects'"
                            :class="['w-full text-left px-4 py-3 rounded-xl flex items-center gap-3 transition-all font-medium',
                                     currentView === 'projects' ? 'bg-primary-500/20 text-primary-400' : 'hover:bg-dark-800 text-dark-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"/></svg>
                        Projetos
                    </button>

                    <button @click="currentView = 'logs'"
                            :class="['w-full text-left px-4 py-3 rounded-xl flex items-center gap-3 transition-all font-medium',
                                     currentView === 'logs' ? 'bg-primary-500/20 text-primary-400' : 'hover:bg-dark-800 text-dark-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"/></svg>
                        Logs de Atividade
                    </button>
                </nav>

                <!-- Stats Sidebar -->
                <div class="mt-8 p-4 bg-dark-800/50 rounded-xl">
                    <h3 class="text-sm font-semibold text-dark-300 mb-3">Resumo</h3>
                    <div class="space-y-3">
                        <div class="flex justify-between items-center">
                            <span class="text-dark-400 text-sm">Negocios</span>
                            <span class="text-business-400 font-medium">{{ orgChart?.total_business || 0 }}</span>
                        </div>
                        <div class="flex justify-between items-center">
                            <span class="text-dark-400 text-sm">Tecnologia</span>
                            <span class="text-tech-400 font-medium">{{ orgChart?.total_technology || 0 }}</span>
                        </div>
                        <div class="flex justify-between items-center">
                            <span class="text-dark-400 text-sm">Decisores</span>
                            <span class="text-primary-400 font-medium">{{ decisionMakers.length }}</span>
                        </div>
                    </div>
                </div>
            </aside>

            <!-- Main Content -->
            <main class="flex-1 p-6 overflow-auto scrollbar-thin">
                <!-- VIEW: OVERVIEW -->
                <div v-if="currentView === 'overview'" class="fade-in">
                    <h2 class="text-2xl font-bold mb-6">Visao Geral</h2>

                    <!-- Stats Cards -->
                    <div class="grid grid-cols-4 gap-4 mb-8">
                        <div class="bg-dark-800 rounded-xl p-5 border border-dark-700">
                            <div class="flex items-center justify-between mb-3">
                                <span class="text-dark-400 text-sm">Total de Agentes</span>
                                <div class="w-10 h-10 rounded-lg bg-primary-500/20 flex items-center justify-center">
                                    <svg class="w-5 h-5 text-primary-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0z"/></svg>
                                </div>
                            </div>
                            <p class="text-3xl font-bold">{{ profiles.length }}</p>
                        </div>

                        <div class="bg-dark-800 rounded-xl p-5 border border-dark-700">
                            <div class="flex items-center justify-between mb-3">
                                <span class="text-dark-400 text-sm">Agentes Ativos</span>
                                <div class="w-10 h-10 rounded-lg bg-success/20 flex items-center justify-center">
                                    <svg class="w-5 h-5 text-success" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>
                                </div>
                            </div>
                            <p class="text-3xl font-bold text-success">{{ status?.agents?.working || 0 }}</p>
                        </div>

                        <div class="bg-dark-800 rounded-xl p-5 border border-dark-700">
                            <div class="flex items-center justify-between mb-3">
                                <span class="text-dark-400 text-sm">Projetos</span>
                                <div class="w-10 h-10 rounded-lg bg-business-500/20 flex items-center justify-center">
                                    <svg class="w-5 h-5 text-business-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"/></svg>
                                </div>
                            </div>
                            <p class="text-3xl font-bold">{{ projects.length }}</p>
                        </div>

                        <div class="bg-dark-800 rounded-xl p-5 border border-dark-700">
                            <div class="flex items-center justify-between mb-3">
                                <span class="text-dark-400 text-sm">Decisores</span>
                                <div class="w-10 h-10 rounded-lg bg-tech-500/20 flex items-center justify-center">
                                    <svg class="w-5 h-5 text-tech-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z"/></svg>
                                </div>
                            </div>
                            <p class="text-3xl font-bold">{{ decisionMakers.length }}</p>
                        </div>
                    </div>

                    <!-- Top Performers -->
                    <div class="bg-dark-800 rounded-xl border border-dark-700 p-6 mb-8">
                        <h3 class="text-lg font-semibold mb-4">Top Performers</h3>
                        <div class="grid grid-cols-5 gap-4">
                            <div v-for="(performer, index) in topPerformers" :key="performer.agent_id"
                                 class="bg-dark-900 rounded-lg p-4 text-center">
                                <div class="w-12 h-12 mx-auto rounded-full bg-gradient-to-br from-primary-500 to-primary-700 flex items-center justify-center text-lg font-bold mb-2">
                                    {{ index + 1 }}
                                </div>
                                <p class="font-medium text-sm truncate">{{ performer.name }}</p>
                                <p class="text-xs text-dark-400 truncate">{{ performer.title }}</p>
                                <p class="text-success font-bold mt-2">{{ performer.reliability_score }}%</p>
                            </div>
                        </div>
                    </div>

                    <!-- Recent Logs -->
                    <div class="bg-dark-800 rounded-xl border border-dark-700 p-6">
                        <h3 class="text-lg font-semibold mb-4">Atividades Recentes</h3>
                        <div class="space-y-2 max-h-64 overflow-auto scrollbar-thin">
                            <div v-for="log in status?.recent_logs?.slice(0, 10)" :key="log.id"
                                 class="flex items-center gap-3 p-3 bg-dark-900 rounded-lg">
                                <span :class="['w-2 h-2 rounded-full', log.level === 'ERROR' ? 'bg-danger' : log.level === 'WARNING' ? 'bg-warning' : 'bg-success']"></span>
                                <span class="text-dark-400 text-xs">{{ log.source }}</span>
                                <span class="text-sm flex-1 truncate">{{ log.message }}</span>
                                <span class="text-dark-500 text-xs">{{ formatTime(log.timestamp) }}</span>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- VIEW: AGENTS - Organograma Hierarquico -->
                <div v-if="currentView === 'agents'" class="fade-in">
                    <div class="flex items-center justify-between mb-6">
                        <h2 class="text-2xl font-bold">Organograma Corporativo</h2>

                        <!-- Area Filter Tabs -->
                        <div class="flex bg-dark-800 rounded-lg p-1">
                            <button @click="selectedArea = 'business'; fetchHierarchy('business')"
                                    :class="['px-4 py-2 rounded-md text-sm font-medium transition-all',
                                             selectedArea === 'business' ? 'bg-business-500 text-white' : 'text-dark-400 hover:text-white']">
                                Area de Negocios
                            </button>
                            <button @click="selectedArea = 'technology'; fetchHierarchy('technology')"
                                    :class="['px-4 py-2 rounded-md text-sm font-medium transition-all',
                                             selectedArea === 'technology' ? 'bg-tech-500 text-white' : 'text-dark-400 hover:text-white']">
                                Area de Tecnologia
                            </button>
                        </div>
                    </div>

                    <!-- Legenda -->
                    <div class="flex gap-6 mb-6 text-sm">
                        <div class="flex items-center gap-2">
                            <span class="w-3 h-3 rounded-full bg-warning"></span>
                            <span class="text-dark-400">Agente Decisor</span>
                        </div>
                        <div class="flex items-center gap-2">
                            <span class="w-3 h-3 rounded-full bg-success"></span>
                            <span class="text-dark-400">Confiabilidade > 90%</span>
                        </div>
                        <div class="flex items-center gap-2">
                            <svg class="w-4 h-4 text-dark-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                            </svg>
                            <span class="text-dark-400">Reporta para</span>
                        </div>
                    </div>

                    <!-- Organograma em Arvore Hierarquica -->
                    <div class="bg-dark-800 rounded-xl border border-dark-700 p-6 overflow-x-auto">
                        <div v-if="hierarchyData.roots && hierarchyData.roots.length > 0" class="org-tree">
                            <!-- Renderiza cada raiz e seus subordinados recursivamente -->
                            <template v-for="rootId in hierarchyData.roots" :key="rootId">
                                <div class="org-branch mb-8">
                                    <!-- Componente recursivo de no da arvore -->
                                    <div class="org-tree-node">
                                        <!-- No raiz -->
                                        <div @click="selectAgent(rootId)"
                                             :class="['org-card p-4 rounded-xl cursor-pointer transition-all hover:scale-105 mx-auto',
                                                      selectedArea === 'business' ? 'bg-gradient-to-br from-business-500/30 to-business-600/20 border border-business-500/50' :
                                                      'bg-gradient-to-br from-tech-500/30 to-tech-600/20 border border-tech-500/50']"
                                             style="width: 280px;">
                                            <div class="flex items-start gap-3">
                                                <div :class="['w-12 h-12 rounded-full flex items-center justify-center text-sm font-bold flex-shrink-0',
                                                              selectedArea === 'business' ? 'gradient-business' : 'gradient-tech']">
                                                    {{ getInitials(hierarchyData.nodes[rootId]?.name) }}
                                                </div>
                                                <div class="flex-1 min-w-0">
                                                    <div class="flex items-center gap-2">
                                                        <p class="font-semibold truncate">{{ hierarchyData.nodes[rootId]?.name }}</p>
                                                        <span v-if="hierarchyData.nodes[rootId]?.is_decision_maker"
                                                              class="w-2 h-2 rounded-full bg-warning flex-shrink-0" title="Decisor"></span>
                                                        <span v-if="hierarchyData.nodes[rootId]?.reliability_score >= 90"
                                                              class="w-2 h-2 rounded-full bg-success flex-shrink-0" title="Alta Confiabilidade"></span>
                                                    </div>
                                                    <p class="text-xs text-dark-300">{{ hierarchyData.nodes[rootId]?.title }}</p>
                                                    <p class="text-xs text-dark-500 mt-1">{{ hierarchyData.nodes[rootId]?.department }}</p>
                                                    <div class="flex items-center gap-3 mt-2 text-xs">
                                                        <span class="text-dark-400">Nivel {{ hierarchyData.nodes[rootId]?.level }}</span>
                                                        <span class="text-success">{{ hierarchyData.nodes[rootId]?.reliability_score }}%</span>
                                                        <span v-if="hierarchyData.nodes[rootId]?.direct_reports?.length > 0" class="text-dark-400">
                                                            {{ hierarchyData.nodes[rootId]?.direct_reports?.length }} subordinados
                                                        </span>
                                                    </div>
                                                </div>
                                            </div>
                                        </div>

                                        <!-- Linha conectora para subordinados -->
                                        <div v-if="hierarchyData.nodes[rootId]?.direct_reports?.length > 0" class="flex flex-col items-center">
                                            <div class="w-0.5 h-6 bg-dark-600"></div>
                                            <div class="flex items-start">
                                                <div class="h-0.5 bg-dark-600" :style="{width: (hierarchyData.nodes[rootId]?.direct_reports?.length * 160) + 'px'}"></div>
                                            </div>

                                            <!-- Subordinados diretos -->
                                            <div class="flex flex-wrap justify-center gap-4 mt-4">
                                                <template v-for="subId in hierarchyData.nodes[rootId]?.direct_reports" :key="subId">
                                                    <div v-if="hierarchyData.nodes[subId]" class="flex flex-col items-center">
                                                        <div class="w-0.5 h-4 bg-dark-600"></div>
                                                        <div @click="selectAgent(subId)"
                                                             :class="['org-card p-3 rounded-lg cursor-pointer transition-all hover:scale-105',
                                                                      selectedArea === 'business' ? 'bg-dark-900 border border-business-500/30 hover:border-business-500/60' :
                                                                      'bg-dark-900 border border-tech-500/30 hover:border-tech-500/60']"
                                                             style="width: 240px;">
                                                            <div class="flex items-center gap-2">
                                                                <div :class="['w-9 h-9 rounded-full flex items-center justify-center text-xs font-bold flex-shrink-0',
                                                                              selectedArea === 'business' ? 'gradient-business' : 'gradient-tech']">
                                                                    {{ getInitials(hierarchyData.nodes[subId]?.name) }}
                                                                </div>
                                                                <div class="flex-1 min-w-0">
                                                                    <div class="flex items-center gap-1">
                                                                        <p class="text-sm font-medium truncate">{{ hierarchyData.nodes[subId]?.name }}</p>
                                                                        <span v-if="hierarchyData.nodes[subId]?.is_decision_maker"
                                                                              class="w-1.5 h-1.5 rounded-full bg-warning flex-shrink-0"></span>
                                                                    </div>
                                                                    <p class="text-xs text-dark-400 truncate">{{ hierarchyData.nodes[subId]?.title }}</p>
                                                                </div>
                                                            </div>
                                                            <div class="flex items-center gap-2 mt-2 text-xs text-dark-500">
                                                                <span>{{ hierarchyData.nodes[subId]?.department }}</span>
                                                                <span class="text-success">{{ hierarchyData.nodes[subId]?.reliability_score }}%</span>
                                                            </div>
                                                            <!-- Indicador de subordinados -->
                                                            <div v-if="hierarchyData.nodes[subId]?.direct_reports?.length > 0"
                                                                 class="mt-2 pt-2 border-t border-dark-700">
                                                                <span class="text-xs text-dark-400">
                                                                    {{ hierarchyData.nodes[subId]?.direct_reports?.length }} subordinado(s)
                                                                </span>
                                                            </div>
                                                        </div>

                                                        <!-- Terceiro nivel (subordinados dos subordinados) -->
                                                        <div v-if="hierarchyData.nodes[subId]?.direct_reports?.length > 0" class="flex flex-col items-center mt-2">
                                                            <div class="w-0.5 h-4 bg-dark-700"></div>
                                                            <div class="flex flex-wrap justify-center gap-2">
                                                                <template v-for="(sub2Id, idx) in hierarchyData.nodes[subId]?.direct_reports?.slice(0, 4)" :key="sub2Id">
                                                                    <div v-if="hierarchyData.nodes[sub2Id]"
                                                                         @click="selectAgent(sub2Id)"
                                                                         class="org-card-mini p-2 rounded bg-dark-800 border border-dark-700 cursor-pointer hover:border-dark-500 transition-all"
                                                                         style="width: 140px;">
                                                                        <p class="text-xs font-medium truncate">{{ hierarchyData.nodes[sub2Id]?.name }}</p>
                                                                        <p class="text-xs text-dark-500 truncate">{{ hierarchyData.nodes[sub2Id]?.title }}</p>
                                                                    </div>
                                                                </template>
                                                                <div v-if="hierarchyData.nodes[subId]?.direct_reports?.length > 4"
                                                                     class="p-2 rounded bg-dark-800 border border-dark-700 text-xs text-dark-400">
                                                                    +{{ hierarchyData.nodes[subId]?.direct_reports?.length - 4 }} mais
                                                                </div>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </template>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </template>
                        </div>

                        <!-- Estado vazio -->
                        <div v-else class="text-center py-12 text-dark-400">
                            <svg class="w-16 h-16 mx-auto mb-4 opacity-50" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0z"/>
                            </svg>
                            <p>Selecione uma area para visualizar o organograma</p>
                        </div>
                    </div>

                    <!-- Resumo da Area -->
                    <div v-if="hierarchyData.nodes" class="grid grid-cols-4 gap-4 mt-6">
                        <div class="bg-dark-800 rounded-xl p-4 border border-dark-700">
                            <p class="text-dark-400 text-sm">Total de Agentes</p>
                            <p class="text-2xl font-bold mt-1">{{ Object.keys(hierarchyData.nodes || {}).length }}</p>
                        </div>
                        <div class="bg-dark-800 rounded-xl p-4 border border-dark-700">
                            <p class="text-dark-400 text-sm">Decisores</p>
                            <p class="text-2xl font-bold mt-1 text-warning">
                                {{ Object.values(hierarchyData.nodes || {}).filter(n => n.is_decision_maker).length }}
                            </p>
                        </div>
                        <div class="bg-dark-800 rounded-xl p-4 border border-dark-700">
                            <p class="text-dark-400 text-sm">Alta Confiabilidade</p>
                            <p class="text-2xl font-bold mt-1 text-success">
                                {{ Object.values(hierarchyData.nodes || {}).filter(n => n.reliability_score >= 90).length }}
                            </p>
                        </div>
                        <div class="bg-dark-800 rounded-xl p-4 border border-dark-700">
                            <p class="text-dark-400 text-sm">Niveis Hierarquicos</p>
                            <p class="text-2xl font-bold mt-1">
                                {{ [...new Set(Object.values(hierarchyData.nodes || {}).map(n => n.level))].length }}
                            </p>
                        </div>
                    </div>
                </div>

                <!-- VIEW: KANBAN -->
                <div v-if="currentView === 'kanban'" class="fade-in">
                    <h2 class="text-2xl font-bold mb-6">Kanban</h2>
                    <div class="grid grid-cols-4 gap-4">
                        <div v-for="status in ['backlog', 'in_progress', 'review', 'done']" :key="status"
                             class="bg-dark-800 rounded-xl border border-dark-700 p-4 min-h-[400px]">
                            <h3 class="font-semibold mb-4 capitalize">{{ status.replace('_', ' ') }}</h3>
                            <div class="space-y-2">
                                <div v-for="story in stories.filter(s => s.status === status)" :key="story.story_id"
                                     class="bg-dark-900 rounded-lg p-3">
                                    <p class="text-sm font-medium">{{ story.title }}</p>
                                    <div class="flex items-center gap-2 mt-2 text-xs text-dark-400">
                                        <span>{{ story.points }} pts</span>
                                        <span>Sprint {{ story.sprint }}</span>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- VIEW: PROJECTS -->
                <div v-if="currentView === 'projects'" class="fade-in">
                    <h2 class="text-2xl font-bold mb-6">Projetos</h2>
                    <div class="grid grid-cols-3 gap-4">
                        <div v-for="project in projects" :key="project.project_id"
                             class="bg-dark-800 rounded-xl border border-dark-700 p-5">
                            <h3 class="font-semibold mb-2">{{ project.name }}</h3>
                            <p class="text-sm text-dark-400 mb-4">{{ project.description }}</p>
                            <div class="flex items-center justify-between">
                                <span :class="['px-2 py-1 rounded-full text-xs',
                                              project.status === 'active' ? 'bg-success/20 text-success' : 'bg-dark-700 text-dark-400']">
                                    {{ project.status }}
                                </span>
                                <span class="text-sm text-dark-400">{{ Math.round(project.progress || 0) }}%</span>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- VIEW: LOGS -->
                <div v-if="currentView === 'logs'" class="fade-in">
                    <h2 class="text-2xl font-bold mb-6">Logs de Atividade</h2>
                    <div class="bg-dark-800 rounded-xl border border-dark-700 p-4">
                        <div class="space-y-2 max-h-[600px] overflow-auto scrollbar-thin">
                            <div v-for="log in logs" :key="log.id"
                                 class="flex items-center gap-3 p-3 bg-dark-900 rounded-lg">
                                <span :class="['w-2 h-2 rounded-full', log.level === 'ERROR' ? 'bg-danger' : log.level === 'WARNING' ? 'bg-warning' : 'bg-success']"></span>
                                <span class="text-dark-400 text-xs w-20">{{ log.source }}</span>
                                <span class="text-dark-400 text-xs w-24">{{ log.action }}</span>
                                <span class="text-sm flex-1">{{ log.message }}</span>
                                <span class="text-dark-500 text-xs">{{ formatTime(log.timestamp) }}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </main>

            <!-- Agent Detail Sidebar -->
            <aside v-if="selectedProfile" class="w-96 min-h-[calc(100vh-65px)] bg-dark-900 border-l border-dark-800 p-6 sticky top-[65px] overflow-auto scrollbar-thin">
                <div class="flex items-center justify-between mb-6">
                    <h3 class="text-lg font-semibold">Perfil do Agente</h3>
                    <button @click="selectedProfile = null" class="text-dark-400 hover:text-white">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/></svg>
                    </button>
                </div>

                <!-- Profile Header -->
                <div class="text-center mb-6">
                    <div :class="['w-20 h-20 mx-auto rounded-full flex items-center justify-center text-2xl font-bold mb-3',
                                 selectedProfile.area === 'business' ? 'gradient-business' : 'gradient-tech']">
                        {{ getInitials(selectedProfile.name) }}
                    </div>
                    <h4 class="text-xl font-bold">{{ selectedProfile.name }}</h4>
                    <p class="text-dark-400">{{ selectedProfile.title }}</p>
                    <p class="text-sm text-dark-500">{{ selectedProfile.department }}</p>
                </div>

                <!-- Reliability Score -->
                <div class="bg-dark-800 rounded-xl p-4 mb-4">
                    <div class="flex items-center justify-between mb-2">
                        <span class="text-sm text-dark-400">Score de Confiabilidade</span>
                        <span class="text-success font-bold">{{ selectedProfile.metrics?.reliability_score || 0 }}%</span>
                    </div>
                    <div class="w-full bg-dark-700 rounded-full h-2">
                        <div class="bg-success h-2 rounded-full skill-bar" :style="{width: (selectedProfile.metrics?.reliability_score || 0) + '%'}"></div>
                    </div>
                </div>

                <!-- Bio -->
                <div class="mb-6">
                    <h5 class="text-sm font-semibold text-dark-300 mb-2">Sobre</h5>
                    <p class="text-sm text-dark-400">{{ selectedProfile.bio }}</p>
                </div>

                <!-- Top Skills -->
                <div class="mb-6">
                    <h5 class="text-sm font-semibold text-dark-300 mb-3">Principais Habilidades</h5>
                    <div class="space-y-3">
                        <div v-for="skill in selectedProfile.top_skills" :key="skill.skill_id">
                            <div class="flex items-center justify-between mb-1">
                                <span class="text-sm">{{ skill.name }}</span>
                                <span class="text-xs text-dark-400">{{ skill.level }}</span>
                            </div>
                            <div class="w-full bg-dark-700 rounded-full h-1.5">
                                <div class="bg-primary-500 h-1.5 rounded-full skill-bar" :style="{width: (skill.level_num * 20) + '%'}"></div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Metrics -->
                <div class="mb-6">
                    <h5 class="text-sm font-semibold text-dark-300 mb-3">Metricas</h5>
                    <div class="grid grid-cols-2 gap-3">
                        <div class="bg-dark-800 rounded-lg p-3 text-center">
                            <p class="text-2xl font-bold text-primary-400">{{ selectedProfile.metrics?.total_projects || 0 }}</p>
                            <p class="text-xs text-dark-400">Projetos</p>
                        </div>
                        <div class="bg-dark-800 rounded-lg p-3 text-center">
                            <p class="text-2xl font-bold text-success">{{ selectedProfile.metrics?.total_tasks_completed || 0 }}</p>
                            <p class="text-xs text-dark-400">Tarefas</p>
                        </div>
                        <div class="bg-dark-800 rounded-lg p-3 text-center">
                            <p class="text-2xl font-bold text-business-400">{{ Math.round(selectedProfile.metrics?.total_hours_worked || 0) }}h</p>
                            <p class="text-xs text-dark-400">Horas</p>
                        </div>
                        <div class="bg-dark-800 rounded-lg p-3 text-center">
                            <p class="text-2xl font-bold text-tech-400">{{ selectedProfile.achievements_count || 0 }}</p>
                            <p class="text-xs text-dark-400">Conquistas</p>
                        </div>
                    </div>
                </div>

                <!-- Decision Maker Settings -->
                <div v-if="selectedProfile.decision_maker?.is_decision_maker" class="mb-6">
                    <h5 class="text-sm font-semibold text-dark-300 mb-3">Configuracoes de Decisor</h5>
                    <div class="bg-dark-800 rounded-xl p-4">
                        <label class="block text-sm text-dark-400 mb-2">Timeout de Aprovacao (horas)</label>
                        <div class="flex gap-2">
                            <input type="number" v-model="timeoutInput" step="0.5" min="0.5" max="24"
                                   class="flex-1 bg-dark-700 border border-dark-600 rounded-lg px-3 py-2 text-sm">
                            <button @click="updateTimeout" class="px-4 py-2 bg-primary-500 rounded-lg text-sm font-medium hover:bg-primary-600">
                                Salvar
                            </button>
                        </div>
                        <p class="text-xs text-dark-500 mt-2">Apos este tempo sem resposta, o agente subordinado ganha autonomia.</p>
                    </div>
                </div>

                <!-- Recent Experiences -->
                <div>
                    <h5 class="text-sm font-semibold text-dark-300 mb-3">Experiencias Recentes</h5>
                    <div class="space-y-2 max-h-48 overflow-auto scrollbar-thin">
                        <div v-for="exp in selectedProfile.recent_experiences?.slice(0, 5)" :key="exp.experience_id"
                             class="bg-dark-800 rounded-lg p-3">
                            <p class="text-sm font-medium">{{ exp.title }}</p>
                            <div class="flex items-center gap-2 mt-1">
                                <span :class="['px-2 py-0.5 rounded text-xs',
                                              exp.outcome === 'success' ? 'bg-success/20 text-success' : 'bg-warning/20 text-warning']">
                                    {{ exp.outcome }}
                                </span>
                                <span class="text-xs text-dark-400">{{ exp.type }}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </aside>
        </div>
    </div>

    <script>
        const { createApp, ref, computed, onMounted, watch } = Vue;

        createApp({
            setup() {
                // State
                const currentView = ref('overview');
                const status = ref(null);
                const profiles = ref([]);
                const projects = ref([]);
                const stories = ref([]);
                const logs = ref([]);
                const orgChart = ref(null);
                const hierarchyData = ref({ roots: [], nodes: {} });
                const topPerformers = ref([]);
                const decisionMakers = ref([]);
                const selectedArea = ref('business');
                const selectedProfile = ref(null);
                const timeoutInput = ref(1);

                const currentTime = computed(() => new Date().toLocaleTimeString('pt-BR'));

                // Methods
                const fetchStatus = async () => {
                    try {
                        const res = await fetch('/api/status');
                        status.value = await res.json();
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchProfiles = async () => {
                    try {
                        const res = await fetch('/api/profiles');
                        const data = await res.json();
                        profiles.value = data.profiles || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchProjects = async () => {
                    try {
                        const res = await fetch('/api/projects');
                        const data = await res.json();
                        projects.value = data.projects || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchStories = async () => {
                    try {
                        const res = await fetch('/api/stories');
                        const data = await res.json();
                        stories.value = data.stories || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchLogs = async () => {
                    try {
                        const res = await fetch('/api/logs?limit=100');
                        const data = await res.json();
                        logs.value = data.logs || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchOrgChart = async () => {
                    try {
                        const res = await fetch('/api/profiles/org-chart');
                        orgChart.value = await res.json();
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchTopPerformers = async () => {
                    try {
                        const res = await fetch('/api/profiles/top-performers?limit=5');
                        const data = await res.json();
                        topPerformers.value = data.top_performers || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchDecisionMakers = async () => {
                    try {
                        const res = await fetch('/api/profiles/decision-makers');
                        const data = await res.json();
                        decisionMakers.value = data.decision_makers || [];
                    } catch (e) { console.error('Error:', e); }
                };

                const fetchHierarchy = async (area) => {
                    try {
                        const res = await fetch(`/api/profiles/hierarchy/${area}`);
                        const data = await res.json();
                        hierarchyData.value = data;
                    } catch (e) { console.error('Error:', e); }
                };

                const selectAgent = async (agentId) => {
                    try {
                        const res = await fetch(`/api/profiles/${agentId}`);
                        const data = await res.json();
                        selectedProfile.value = data.profile;
                        timeoutInput.value = data.profile?.decision_maker?.approval_timeout_hours || 1;
                    } catch (e) { console.error('Error:', e); }
                };

                const updateTimeout = async () => {
                    if (!selectedProfile.value) return;
                    try {
                        await fetch(`/api/profiles/${selectedProfile.value.agent_id}/timeout`, {
                            method: 'PUT',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({ timeout_hours: parseFloat(timeoutInput.value) })
                        });
                        selectedProfile.value.decision_maker.approval_timeout_hours = parseFloat(timeoutInput.value);
                    } catch (e) { console.error('Error:', e); }
                };

                const getInitials = (name) => {
                    if (!name) return '?';
                    return name.split(' ').map(n => n[0]).join('').substring(0, 2).toUpperCase();
                };

                const formatTime = (timestamp) => {
                    if (!timestamp) return '';
                    const date = new Date(timestamp);
                    return date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
                };

                // Lifecycle
                onMounted(() => {
                    fetchStatus();
                    fetchProfiles();
                    fetchProjects();
                    fetchStories();
                    fetchLogs();
                    fetchOrgChart();
                    fetchTopPerformers();
                    fetchDecisionMakers();
                    fetchHierarchy('business');

                    setInterval(fetchStatus, 5000);
                    setInterval(fetchLogs, 10000);
                });

                return {
                    currentView, status, profiles, projects, stories, logs,
                    orgChart, hierarchyData, topPerformers, decisionMakers, selectedArea,
                    selectedProfile, timeoutInput, currentTime,
                    selectAgent, updateTimeout, getInitials, formatTime, fetchHierarchy
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
'''


@app.get("/", response_class=HTMLResponse)
async def dashboard():
    """Dashboard principal v3.0"""
    return DASHBOARD_HTML


if __name__ == "__main__":
    print("=" * 60)
    print("FABRICA DE AGENTES - Dashboard v3.0")
    print("=" * 60)
    print(f"Acesse em: http://{DASHBOARD_HOST}:{DASHBOARD_PORT}")
    print("=" * 60)

    uvicorn.run(app, host=DASHBOARD_HOST, port=DASHBOARD_PORT)
