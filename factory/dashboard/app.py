"""
Dashboard de Controle - Fabrica de Agentes
Centro de comando para gerenciamento de projetos e agentes

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
    Project, Story, Agent, Skill, Task, ActivityLog, FactoryEvent, Template, User, Sprint
)
from factory.database.repositories import (
    ProjectRepository, StoryRepository, AgentRepository, SkillRepository,
    TaskRepository, ActivityLogRepository, FactoryEventRepository, TemplateRepository,
    SprintRepository
)
from factory.config import DASHBOARD_HOST, DASHBOARD_PORT, DASHBOARD_TITLE, AGENTS

# Import corporate hierarchy for org chart
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

# Import Story Executor for automatic story execution
try:
    from factory.orchestrator.story_executor import get_executor, start_executor, stop_executor
    HAS_STORY_EXECUTOR = True
except ImportError:
    HAS_STORY_EXECUTOR = False

# Import Agent Runner for real agent execution
try:
    from factory.orchestrator.agent_runner import get_runner, start_runner, stop_runner
    HAS_AGENT_RUNNER = True
except ImportError:
    HAS_AGENT_RUNNER = False

# Import Project Orchestrator for autonomous project processing
try:
    from factory.orchestrator.project_orchestrator import (
        get_orchestrator, start_orchestrator, stop_orchestrator
    )
    HAS_PROJECT_ORCHESTRATOR = True
except ImportError:
    HAS_PROJECT_ORCHESTRATOR = False

# Initialize database
init_db()

# Import OpenAPI config
try:
    from factory.api.openapi_config import TAGS_METADATA, OPENAPI_METADATA
    HAS_OPENAPI_CONFIG = True
except ImportError:
    HAS_OPENAPI_CONFIG = False
    TAGS_METADATA = []
    OPENAPI_METADATA = {}

# Import logging middleware
try:
    from factory.api.middleware import RequestLoggingMiddleware, SecurityHeadersMiddleware
    HAS_MIDDLEWARE = True
except ImportError:
    HAS_MIDDLEWARE = False

# Import structured logging
try:
    from factory.core.logging_system import get_logger, log_info, log_error
    HAS_STRUCTURED_LOGGING = True
    logger = get_logger()
except ImportError:
    HAS_STRUCTURED_LOGGING = False

# FastAPI App with OpenAPI config
app = FastAPI(
    title=OPENAPI_METADATA.get("title", DASHBOARD_TITLE),
    description=OPENAPI_METADATA.get("description", "API da Fabrica de Agentes"),
    version=OPENAPI_METADATA.get("version", "3.0.0"),
    openapi_tags=TAGS_METADATA if TAGS_METADATA else None,
    contact=OPENAPI_METADATA.get("contact"),
    license_info=OPENAPI_METADATA.get("license_info"),
    docs_url="/docs",
    redoc_url="/redoc"
)

# Add middleware
if HAS_MIDDLEWARE:
    app.add_middleware(RequestLoggingMiddleware)
    app.add_middleware(SecurityHeadersMiddleware)

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


class SprintCreate(BaseModel):
    project_id: str
    sprint_number: int
    name: Optional[str] = None
    goal: Optional[str] = None
    start_date: Optional[str] = None
    end_date: Optional[str] = None


class SprintUpdate(BaseModel):
    name: Optional[str] = None
    goal: Optional[str] = None
    status: Optional[str] = None
    start_date: Optional[str] = None
    end_date: Optional[str] = None
    planned_points: Optional[int] = None


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
        return {
            "success": True,
            "token": token,
            "user": user.to_dict()
        }
    finally:
        db.close()


# =============================================================================
# STATUS ENDPOINT
# =============================================================================

@app.get("/api/status")
async def get_status():
    """Retorna status geral da fabrica"""
    db = SessionLocal()
    try:
        # Conta projetos por status
        projects = db.query(Project).all()
        projects_by_status = {}
        for p in projects:
            projects_by_status[p.status] = projects_by_status.get(p.status, 0) + 1

        # Conta agentes por status
        agents = db.query(Agent).filter(Agent.enabled == True).all()
        agents_by_status = {}
        for a in agents:
            agents_by_status[a.status] = agents_by_status.get(a.status, 0) + 1

        # Logs recentes
        recent_logs = db.query(ActivityLog).order_by(
            ActivityLog.timestamp.desc()
        ).limit(20).all()

        # Skills disponiveis
        skills_count = db.query(Skill).filter(Skill.enabled == True).count()

        return {
            "factory": {
                "name": DASHBOARD_TITLE,
                "version": "3.0.0",
                "status": "running"
            },
            "projects": {
                "total": len(projects),
                "by_status": projects_by_status,
                "list": [p.to_dict() for p in projects]
            },
            "agents": {
                "total": len(agents),
                "by_status": agents_by_status,
                "list": [a.to_dict() for a in agents]
            },
            "skills": {
                "total": skills_count
            },
            "recent_logs": [l.to_dict() for l in recent_logs],
            "timestamp": datetime.utcnow().isoformat()
        }
    finally:
        db.close()


# =============================================================================
# PROJECT ENDPOINTS
# =============================================================================

@app.get("/api/projects")
async def list_projects(status: str = None, project_type: str = None):
    """Lista todos os projetos"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        projects = repo.get_all(status=status, project_type=project_type)
        return {"projects": [p.to_dict() for p in projects]}
    finally:
        db.close()


@app.post("/api/projects")
async def create_project(data: ProjectCreate):
    """Cria novo projeto"""
    db = SessionLocal()
    try:
        # Gera ID do projeto
        count = db.query(Project).count()
        project_id = f"PRJ-{count + 1:03d}"

        # Cria pasta do projeto
        from factory.config import PROJECTS_DIR
        folder_path = PROJECTS_DIR / project_id.lower().replace("-", "_")
        folder_path.mkdir(parents=True, exist_ok=True)

        repo = ProjectRepository(db)
        project = repo.create({
            "project_id": project_id,
            "name": data.name,
            "description": data.description,
            "project_type": data.project_type,
            "template_used": data.template_id,
            "folder_path": str(folder_path),
            "config": data.config or {},
            "status": "PLANNING"
        })

        return {"success": True, "project": project.to_dict()}
    finally:
        db.close()


@app.get("/api/projects/{project_id}")
async def get_project(project_id: str):
    """Busca projeto por ID"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        project = repo.get_by_id(project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Projeto nao encontrado")
        return {"project": project.to_dict()}
    finally:
        db.close()


@app.put("/api/projects/{project_id}")
async def update_project(project_id: str, data: ProjectUpdate):
    """Atualiza projeto"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        project = repo.update(project_id, data.dict(exclude_none=True))
        if not project:
            raise HTTPException(status_code=404, detail="Projeto nao encontrado")
        return {"success": True, "project": project.to_dict()}
    finally:
        db.close()


@app.delete("/api/projects/{project_id}")
async def delete_project(project_id: str):
    """Remove projeto"""
    db = SessionLocal()
    try:
        repo = ProjectRepository(db)
        if repo.delete(project_id):
            return {"success": True}
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")
    finally:
        db.close()


# =============================================================================
# STORY ENDPOINTS
# =============================================================================

@app.get("/api/stories")
async def list_stories(project_id: str = None, sprint: int = None, status: str = None):
    """Lista stories com filtros combinados"""
    db = SessionLocal()
    try:
        query = db.query(Story)

        # Filtros
        if project_id:
            query = query.filter(Story.project_id == project_id)
        if sprint:
            query = query.filter(Story.sprint == sprint)
        if status:
            query = query.filter(Story.status == status)

        # Ordena por prioridade e data
        stories = query.order_by(Story.priority.desc(), Story.updated_at.desc()).all()
        return {"stories": [s.to_dict() for s in stories]}
    finally:
        db.close()


@app.get("/api/projects/{project_id}/kanban")
async def get_project_kanban(project_id: str, sprint: int = None):
    """Retorna stories organizadas para Kanban de um projeto"""
    db = SessionLocal()
    try:
        query = db.query(Story).filter(Story.project_id == project_id)
        if sprint:
            query = query.filter(Story.sprint == sprint)

        stories = query.order_by(Story.priority.desc()).all()

        # Organiza por status (colunas do Kanban)
        kanban = {
            "TO_DO": [],
            "IN_PROGRESS": [],
            "TESTING": [],
            "DONE": [],
            "BLOCKED": []
        }

        for story in stories:
            status = story.status or "TO_DO"
            if status not in kanban:
                kanban[status] = []
            kanban[status].append(story.to_dict())

        # Busca sprints do projeto
        sprints = db.query(Sprint).filter(
            Sprint.project_id == project_id
        ).order_by(Sprint.sprint_number).all()

        # Se nao houver sprints, cria sprints baseados nas stories
        if not sprints:
            story_sprints = db.query(Story.sprint).filter(
                Story.project_id == project_id
            ).distinct().all()
            sprint_numbers = sorted(set(s[0] for s in story_sprints if s[0]))
            sprints_data = [{"sprint_number": n, "name": f"Sprint {n}", "status": "planned"} for n in sprint_numbers]
        else:
            sprints_data = [s.to_dict() for s in sprints]

        return {
            "project_id": project_id,
            "current_sprint": sprint,
            "sprints": sprints_data,
            "kanban": kanban,
            "total_stories": len(stories)
        }
    finally:
        db.close()


@app.get("/api/stories/{story_id}")
async def get_story(story_id: str):
    """Busca story por ID com detalhes completos"""
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        # Busca tasks relacionadas
        tasks = db.query(Task).filter(Task.story_id == story_id).all()

        # Busca logs relacionados
        logs = db.query(ActivityLog).filter(
            ActivityLog.story_id == story_id
        ).order_by(ActivityLog.timestamp.desc()).limit(20).all()

        result = story.to_dict()
        result['tasks'] = [t.to_dict() for t in tasks]
        result['activity_logs'] = [l.to_dict() for l in logs]

        return {"story": result}
    finally:
        db.close()


@app.post("/api/stories")
async def create_story(data: StoryCreate):
    """Cria nova story"""
    db = SessionLocal()
    try:
        # Gera ID da story
        count = db.query(Story).count()
        story_id = f"US-{count + 1:03d}"

        repo = StoryRepository(db)
        story = repo.create({
            "story_id": story_id,
            "title": data.title,
            "description": data.description,
            "project_id": data.project_id,
            "sprint": data.sprint,
            "points": data.points,
            "priority": data.priority,
            "status": "TO_DO"
        })

        return {"success": True, "story": story.to_dict()}
    finally:
        db.close()


@app.put("/api/stories/{story_id}")
async def update_story(story_id: str, data: StoryUpdate):
    """Atualiza story - dispara execucao automatica se mover para TO_DO"""
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        old_status = story.status

        for key, value in data.dict(exclude_none=True).items():
            setattr(story, key, value)
        story.updated_at = datetime.utcnow()
        db.commit()
        db.refresh(story)

        # Dispara execucao automatica se moveu para TO_DO
        if HAS_STORY_EXECUTOR and data.status == "TO_DO" and old_status != "TO_DO":
            try:
                executor = get_executor()
                executor.trigger_story(story_id)
                print(f"[Dashboard] Execucao automatica disparada para story {story_id}")
            except Exception as e:
                print(f"[Dashboard] Erro ao disparar execucao: {e}")

        return {"success": True, "story": story.to_dict()}
    finally:
        db.close()


@app.post("/api/stories/{story_id}/trigger")
async def trigger_story_execution(story_id: str):
    """Dispara execucao de uma story pelo AgentRunner"""
    if HAS_AGENT_RUNNER:
        try:
            runner = get_runner()
            result = runner.trigger_story(story_id)
            return result
        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))
    else:
        raise HTTPException(status_code=501, detail="AgentRunner nao disponivel")


@app.get("/api/stories/{story_id}/details")
async def get_story_details(story_id: str):
    """Retorna detalhes completos de uma story para visualizacao"""
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        # Monta objeto com todos os detalhes
        import json
        details = {
            "story_id": story.story_id,
            "title": story.title,
            "description": story.description,
            "status": story.status,
            "sprint": story.sprint,
            "points": story.points,
            "priority": story.priority,
            "epic_id": story.epic_id,
            "category": story.category,
            # Narrativa
            "narrative": {
                "persona": story.narrative_persona,
                "action": story.narrative_action,
                "benefit": story.narrative_benefit
            },
            # Listas (parse JSON)
            "acceptance_criteria": json.loads(story.acceptance_criteria or "[]"),
            "business_rules": json.loads(story.business_rules or "[]"),
            "definition_of_done": json.loads(story.definition_of_done or "[]"),
            "technical_notes": json.loads(story.technical_notes or "[]"),
            "tags": json.loads(story.tags or "[]"),
            "artifacts": json.loads(story.artifacts or "[]"),
            "agents": json.loads(story.agents or "[]"),
            # Atribuicao
            "assigned_to": story.assigned_to,
            "reviewer": story.reviewer,
            "qa_agent": story.qa_agent,
            # Estimativas
            "estimated_hours": story.estimated_hours,
            "actual_hours": story.actual_hours,
            "complexity": story.complexity,
            "risk_level": story.risk_level,
            # Datas
            "started_at": story.started_at.isoformat() if story.started_at else None,
            "completed_at": story.completed_at.isoformat() if story.completed_at else None,
            "tested_at": story.tested_at.isoformat() if story.tested_at else None,
            "created_at": story.created_at.isoformat() if story.created_at else None,
            # Metadata
            "created_by": story.created_by,
            "source": story.source
        }

        return details
    finally:
        db.close()


class StoryFullUpdate(BaseModel):
    """Modelo para atualizacao completa de story"""
    title: Optional[str] = None
    description: Optional[str] = None
    status: Optional[str] = None
    sprint: Optional[int] = None
    points: Optional[int] = None
    priority: Optional[int] = None
    epic_id: Optional[str] = None
    category: Optional[str] = None
    narrative_persona: Optional[str] = None
    narrative_action: Optional[str] = None
    narrative_benefit: Optional[str] = None
    acceptance_criteria: Optional[List[str]] = None
    business_rules: Optional[List[str]] = None
    definition_of_done: Optional[List[str]] = None
    technical_notes: Optional[List[str]] = None
    tags: Optional[List[str]] = None
    assigned_to: Optional[str] = None
    reviewer: Optional[str] = None
    qa_agent: Optional[str] = None
    estimated_hours: Optional[float] = None
    complexity: Optional[str] = None
    risk_level: Optional[str] = None


@app.put("/api/stories/{story_id}/full")
async def update_story_full(story_id: str, data: StoryFullUpdate):
    """Atualiza story com todos os campos (edicao manual completa)"""
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()
        if not story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        import json

        # Atualiza campos simples
        simple_fields = [
            'title', 'description', 'status', 'sprint', 'points', 'priority',
            'epic_id', 'category', 'narrative_persona', 'narrative_action',
            'narrative_benefit', 'assigned_to', 'reviewer', 'qa_agent',
            'estimated_hours', 'complexity', 'risk_level'
        ]

        for field in simple_fields:
            value = getattr(data, field, None)
            if value is not None:
                setattr(story, field, value)

        # Atualiza campos JSON (listas)
        json_fields = [
            'acceptance_criteria', 'business_rules', 'definition_of_done',
            'technical_notes', 'tags'
        ]

        for field in json_fields:
            value = getattr(data, field, None)
            if value is not None:
                setattr(story, field, json.dumps(value))

        story.updated_at = datetime.utcnow()
        db.commit()
        db.refresh(story)

        return {"success": True, "story_id": story_id}
    finally:
        db.close()


@app.get("/api/runner/approvals")
async def get_pending_approvals_from_runner(project_id: str = None):
    """Retorna aprovacoes pendentes do AgentRunner"""
    if HAS_AGENT_RUNNER:
        try:
            runner = get_runner()
            approvals = runner.get_pending_approvals(project_id)
            return {"approvals": approvals, "count": len(approvals)}
        except Exception as e:
            return {"approvals": [], "count": 0, "error": str(e)}
    return {"approvals": [], "count": 0}


class ApprovalDecision(BaseModel):
    approved: bool
    approved_by: str
    notes: Optional[str] = ""


@app.post("/api/runner/approvals/{approval_id}")
async def process_approval(approval_id: str, decision: ApprovalDecision):
    """Processa uma aprovacao (aprova ou rejeita)"""
    if HAS_AGENT_RUNNER:
        try:
            runner = get_runner()
            result = runner.approve(approval_id, decision.approved_by, decision.approved, decision.notes)
            return result
        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))
    else:
        raise HTTPException(status_code=501, detail="AgentRunner nao disponivel")


@app.get("/api/runner/status")
async def get_runner_status():
    """Retorna status do AgentRunner"""
    return {
        "agent_runner_available": HAS_AGENT_RUNNER,
        "story_executor_available": HAS_STORY_EXECUTOR,
        "project_orchestrator_available": HAS_PROJECT_ORCHESTRATOR
    }


# =============================================================================
# PROJECT ORCHESTRATOR ENDPOINTS - AGENTES AUTONOMOS
# =============================================================================

@app.post("/api/orchestrator/process/{project_id}")
async def process_project_autonomous(project_id: str):
    """
    Dispara processamento AUTONOMO de um projeto pelos agentes.
    Os agentes vao:
    1. Ler todos os inputs do projeto (documentos, videos, etc)
    2. Analisar o conteudo
    3. Criar stories automaticamente
    4. Mover stories para o pipeline
    """
    if not HAS_PROJECT_ORCHESTRATOR:
        raise HTTPException(status_code=501, detail="ProjectOrchestrator nao disponivel")

    try:
        orchestrator = get_orchestrator()
        result = orchestrator.process_project_now(project_id)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/orchestrator/status")
async def get_orchestrator_status():
    """Retorna status do ProjectOrchestrator"""
    if not HAS_PROJECT_ORCHESTRATOR:
        return {"available": False, "running": False}

    try:
        orchestrator = get_orchestrator()
        return {
            "available": True,
            "running": orchestrator._running
        }
    except Exception as e:
        return {"available": True, "running": False, "error": str(e)}


@app.post("/api/orchestrator/start")
async def start_project_orchestrator():
    """Inicia o ProjectOrchestrator para processamento autonomo"""
    if not HAS_PROJECT_ORCHESTRATOR:
        raise HTTPException(status_code=501, detail="ProjectOrchestrator nao disponivel")

    try:
        start_orchestrator()
        return {"success": True, "message": "ProjectOrchestrator iniciado"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/api/orchestrator/stop")
async def stop_project_orchestrator():
    """Para o ProjectOrchestrator"""
    if not HAS_PROJECT_ORCHESTRATOR:
        raise HTTPException(status_code=501, detail="ProjectOrchestrator nao disponivel")

    try:
        stop_orchestrator()
        return {"success": True, "message": "ProjectOrchestrator parado"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# AUTONOMOUS DEVELOPER - DESENVOLVIMENTO REAL DE PROJETOS
# =============================================================================

# Import do desenvolvedor autonomo
try:
    from factory.orchestrator.autonomous_developer import (
        get_developer, start_developer, stop_developer
    )
    HAS_DEVELOPER = True
except ImportError:
    HAS_DEVELOPER = False


@app.post("/api/developer/develop/{project_id}")
async def develop_project_autonomous(project_id: str):
    """
    Inicia desenvolvimento REAL e AUTONOMO de um projeto.

    Os agentes vao:
    1. Criar estrutura de diretorios
    2. Gerar modelos SQLAlchemy
    3. Criar routers FastAPI
    4. Criar componentes Vue.js
    5. Criar testes automatizados

    Tudo de forma REAL - arquivos sao criados de verdade!
    """
    if not HAS_DEVELOPER:
        raise HTTPException(status_code=501, detail="AutonomousDeveloper nao disponivel")

    try:
        developer = get_developer()
        if not developer._running:
            developer.start()

        result = developer.develop_project(project_id)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/developer/status/{project_id}")
async def get_development_status(project_id: str):
    """Retorna status do desenvolvimento de um projeto"""
    if not HAS_DEVELOPER:
        return {"available": False}

    try:
        developer = get_developer()
        return developer.get_project_status(project_id)
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/developer/agent-memory/{agent_id}")
async def get_agent_memory(agent_id: str):
    """Retorna memoria e aprendizado de um agente"""
    if not HAS_DEVELOPER:
        return {"available": False}

    try:
        developer = get_developer()
        return developer.get_agent_memory_summary(agent_id)
    except Exception as e:
        return {"error": str(e)}


@app.post("/api/developer/start")
async def start_autonomous_developer():
    """Inicia o desenvolvedor autonomo"""
    if not HAS_DEVELOPER:
        raise HTTPException(status_code=501, detail="AutonomousDeveloper nao disponivel")

    try:
        start_developer()
        return {"success": True, "message": "AutonomousDeveloper iniciado"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# INTELLIGENT DEVELOPER - DESENVOLVIMENTO COM LLM (CLAUDE)
# =============================================================================

# Import do desenvolvedor inteligente
try:
    from factory.orchestrator.intelligent_developer import (
        get_intelligent_developer, start_intelligent_developer, stop_intelligent_developer
    )
    HAS_INTELLIGENT_DEVELOPER = True
except ImportError:
    HAS_INTELLIGENT_DEVELOPER = False


@app.post("/api/intelligent-developer/develop/{project_id}")
async def develop_project_intelligent(project_id: str):
    """
    Inicia desenvolvimento INTELIGENTE de um projeto usando Claude (LLM).

    Os agentes usam seu "cerebro" (AgentBrain) para:
    1. Pensar sobre cada tarefa antes de executar
    2. Gerar codigo contextualizado e adaptado
    3. Tomar decisoes arquiteturais
    4. Aprender com cada execucao

    Se Claude API nao estiver disponivel, faz fallback para templates.
    """
    if not HAS_INTELLIGENT_DEVELOPER:
        raise HTTPException(status_code=501, detail="IntelligentDeveloper nao disponivel")

    try:
        developer = get_intelligent_developer()
        if not developer._running:
            developer.start()

        result = developer.develop_project(project_id)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/intelligent-developer/status/{project_id}")
async def get_intelligent_development_status(project_id: str):
    """Retorna status do desenvolvimento inteligente de um projeto"""
    if not HAS_INTELLIGENT_DEVELOPER:
        return {"available": False}

    try:
        developer = get_intelligent_developer()
        return developer.get_project_status(project_id)
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/intelligent-developer/agent/{agent_id}")
async def get_intelligent_agent_status(agent_id: str):
    """Retorna status de um agente inteligente (cerebro, memoria, decisoes)"""
    if not HAS_INTELLIGENT_DEVELOPER:
        return {"available": False}

    try:
        developer = get_intelligent_developer()
        return developer.get_agent_status(agent_id)
    except Exception as e:
        return {"error": str(e)}


@app.post("/api/intelligent-developer/start")
async def start_intelligent_developer_endpoint():
    """Inicia o desenvolvedor inteligente"""
    if not HAS_INTELLIGENT_DEVELOPER:
        raise HTTPException(status_code=501, detail="IntelligentDeveloper nao disponivel")

    try:
        start_intelligent_developer()
        return {"success": True, "message": "IntelligentDeveloper iniciado"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/intelligent-developer/info")
async def get_intelligent_developer_info():
    """Retorna informacoes sobre o desenvolvedor inteligente"""
    if not HAS_INTELLIGENT_DEVELOPER:
        return {"available": False}

    try:
        developer = get_intelligent_developer()
        return {
            "available": True,
            "running": developer._running,
            "intelligence_enabled": developer.use_intelligence,
            "agents_configured": len(developer.AGENT_CONFIG),
            "active_projects": len(developer.active_projects),
            "tasks_pending": developer.task_queue.qsize()
        }
    except Exception as e:
        return {"error": str(e)}


# =============================================================================
# SPRINT ENDPOINTS
# =============================================================================

@app.get("/api/sprints")
async def list_sprints(project_id: str = None):
    """Lista sprints (filtro por projeto obrigatorio para isolamento)"""
    db = SessionLocal()
    try:
        if project_id:
            repo = SprintRepository(db)
            sprints = repo.get_by_project(project_id)
        else:
            sprints = db.query(Sprint).order_by(Sprint.project_id, Sprint.sprint_number).all()
        return {"sprints": [s.to_dict() for s in sprints]}
    finally:
        db.close()


@app.get("/api/projects/{project_id}/sprints")
async def list_project_sprints(project_id: str):
    """Lista sprints de um projeto especifico"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprints = repo.get_by_project(project_id)

        # Calcula metricas para cada sprint
        result = []
        for sprint in sprints:
            sprint_data = sprint.to_dict()
            # Conta stories do sprint
            stories = db.query(Story).filter(
                Story.project_id == project_id,
                Story.sprint == sprint.sprint_number
            ).all()
            sprint_data['stories_count'] = len(stories)
            sprint_data['stories_by_status'] = {}
            for s in stories:
                status = s.status or 'unknown'
                sprint_data['stories_by_status'][status] = sprint_data['stories_by_status'].get(status, 0) + 1
            result.append(sprint_data)

        return {"sprints": result, "project_id": project_id}
    finally:
        db.close()


@app.post("/api/sprints")
async def create_sprint(data: SprintCreate):
    """Cria novo sprint para um projeto"""
    db = SessionLocal()
    try:
        # Verifica se projeto existe
        project = db.query(Project).filter(Project.project_id == data.project_id).first()
        if not project:
            raise HTTPException(status_code=404, detail="Projeto nao encontrado")

        # Verifica se sprint ja existe
        existing = db.query(Sprint).filter(
            Sprint.project_id == data.project_id,
            Sprint.sprint_number == data.sprint_number
        ).first()
        if existing:
            raise HTTPException(status_code=400, detail="Sprint ja existe para este projeto")

        repo = SprintRepository(db)
        sprint = repo.create({
            "project_id": data.project_id,
            "sprint_number": data.sprint_number,
            "name": data.name or f"Sprint {data.sprint_number}",
            "goal": data.goal,
            "status": "planned"
        })

        return {"success": True, "sprint": sprint.to_dict()}
    finally:
        db.close()


@app.put("/api/sprints/{sprint_id}")
async def update_sprint(sprint_id: int, data: SprintUpdate):
    """Atualiza sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprint = repo.update(sprint_id, data.dict(exclude_none=True))
        if not sprint:
            raise HTTPException(status_code=404, detail="Sprint nao encontrado")
        return {"success": True, "sprint": sprint.to_dict()}
    finally:
        db.close()


@app.post("/api/sprints/{sprint_id}/activate")
async def activate_sprint(sprint_id: int):
    """Ativa um sprint (desativa os outros do projeto)"""
    db = SessionLocal()
    try:
        sprint = db.query(Sprint).filter(Sprint.id == sprint_id).first()
        if not sprint:
            raise HTTPException(status_code=404, detail="Sprint nao encontrado")

        repo = SprintRepository(db)
        activated = repo.activate_sprint(sprint.project_id, sprint.sprint_number)
        return {"success": True, "sprint": activated.to_dict()}
    finally:
        db.close()


@app.post("/api/sprints/{sprint_id}/complete")
async def complete_sprint(sprint_id: int):
    """Completa um sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        sprint = repo.complete_sprint(sprint_id)
        if not sprint:
            raise HTTPException(status_code=404, detail="Sprint nao encontrado")
        return {"success": True, "sprint": sprint.to_dict()}
    finally:
        db.close()


@app.delete("/api/sprints/{sprint_id}")
async def delete_sprint(sprint_id: int):
    """Remove sprint"""
    db = SessionLocal()
    try:
        repo = SprintRepository(db)
        if repo.delete(sprint_id):
            return {"success": True}
        raise HTTPException(status_code=404, detail="Sprint nao encontrado")
    finally:
        db.close()


# =============================================================================
# AGENT ENDPOINTS
# =============================================================================

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

        # Busca projetos em que o agente trabalhou
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


@app.put("/api/agents/{agent_id}")
async def update_agent(agent_id: str, data: AgentUpdate):
    """Atualiza agente"""
    db = SessionLocal()
    try:
        repo = AgentRepository(db)
        agent = repo.get_by_id(agent_id)
        if not agent:
            raise HTTPException(status_code=404, detail="Agente nao encontrado")

        for key, value in data.dict(exclude_none=True).items():
            setattr(agent, key, value)
        agent.updated_at = datetime.utcnow()
        db.commit()
        db.refresh(agent)

        return {"success": True, "agent": agent.to_dict()}
    finally:
        db.close()


# =============================================================================
# SKILL ENDPOINTS
# =============================================================================

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


# =============================================================================
# TEMPLATE ENDPOINTS
# =============================================================================

@app.get("/api/templates")
async def list_templates(project_type: str = None):
    """Lista templates"""
    db = SessionLocal()
    try:
        repo = TemplateRepository(db)
        if project_type:
            templates = repo.get_by_type(project_type)
        else:
            templates = repo.get_all()
        return {"templates": [t.to_dict() for t in templates]}
    finally:
        db.close()


# =============================================================================
# LOG ENDPOINTS
# =============================================================================

@app.get("/api/logs")
async def list_logs(
    project_id: str = None,
    agent_id: str = None,
    level: str = None,
    limit: int = Query(default=100, le=500)
):
    """Lista logs de atividades"""
    db = SessionLocal()
    try:
        repo = ActivityLogRepository(db)
        if agent_id:
            logs = repo.get_by_agent(agent_id, limit)
        elif level:
            logs = repo.get_by_level(level, limit)
        else:
            logs = repo.get_recent(limit, project_id)
        return {"logs": [l.to_dict() for l in logs]}
    finally:
        db.close()


# =============================================================================
# DASHBOARD METRICS ENDPOINT
# =============================================================================

@app.get("/api/metrics")
async def get_metrics(project_id: str = None):
    """Retorna metricas agregadas"""
    db = SessionLocal()
    try:
        # Stories metrics
        query = db.query(Story)
        if project_id:
            query = query.filter(Story.project_id == project_id)
        stories = query.all()

        total_stories = len(stories)
        done_stories = len([s for s in stories if s.status.upper() in ['DONE', 'COMPLETED']])
        in_progress = len([s for s in stories if s.status.upper() == 'IN_PROGRESS'])
        total_points = sum(s.points or 0 for s in stories)
        done_points = sum(s.points or 0 for s in stories if s.status.upper() in ['DONE', 'COMPLETED'])

        # Agent activity
        agents = db.query(Agent).filter(Agent.enabled == True).all()
        active_agents = [a for a in agents if a.status != 'STANDBY']

        return {
            "stories": {
                "total": total_stories,
                "done": done_stories,
                "in_progress": in_progress,
                "completion_rate": round((done_stories / total_stories * 100) if total_stories > 0 else 0, 1)
            },
            "points": {
                "total": total_points,
                "done": done_points,
                "velocity": done_points
            },
            "agents": {
                "total": len(agents),
                "active": len(active_agents)
            }
        }
    finally:
        db.close()


# =============================================================================
# HIERARCHY & PRODUCTIVITY ENDPOINTS
# =============================================================================

@app.get("/api/hierarchy")
async def get_hierarchy():
    """Retorna organograma completo da empresa"""
    if not HAS_CORPORATE_HIERARCHY:
        return {"error": "Corporate hierarchy module not available", "org_chart": {}}

    try:
        system = HierarchyApprovalSystem()
        return {
            "org_chart": system.get_org_chart(),
            "statistics": system.get_statistics()
        }
    except Exception as e:
        return {"error": str(e), "org_chart": {}}


@app.get("/api/hierarchy/areas")
async def get_hierarchy_by_areas():
    """Retorna agentes agrupados por area (business vs technology)"""
    if not HAS_CORPORATE_HIERARCHY:
        return {"error": "Corporate hierarchy module not available"}

    try:
        business_agents = get_agents_by_area("business")
        tech_agents = get_agents_by_area("technology")

        def agent_to_dict(agent):
            return {
                "id": agent.agent_id,
                "name": agent.name,
                "title": agent.title,
                "level": agent.level.title,
                "level_num": agent.level.level_num,
                "department": agent.department.display_name,
                "status": agent.status.value,
                "reports_to": agent.reports_to,
                "direct_reports_count": len(agent.direct_reports),
                "metrics": agent.metrics.to_dict(),
                "skills": agent.skills[:5] if agent.skills else []
            }

        return {
            "business": {
                "count": len(business_agents),
                "agents": [agent_to_dict(a) for a in sorted(business_agents, key=lambda x: x.level.level_num)]
            },
            "technology": {
                "count": len(tech_agents),
                "agents": [agent_to_dict(a) for a in sorted(tech_agents, key=lambda x: x.level.level_num)]
            }
        }
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/hierarchy/departments")
async def get_hierarchy_departments():
    """Retorna agentes agrupados por departamento"""
    if not HAS_CORPORATE_HIERARCHY:
        return {"error": "Corporate hierarchy module not available"}

    try:
        departments = {}
        for agent in ALL_CORPORATE_AGENTS.values():
            dept_name = agent.department.display_name
            if dept_name not in departments:
                departments[dept_name] = {
                    "name": dept_name,
                    "area": agent.department.area,
                    "agents": [],
                    "working": 0,
                    "standby": 0
                }

            departments[dept_name]["agents"].append({
                "id": agent.agent_id,
                "name": agent.name,
                "title": agent.title,
                "level": agent.level.title,
                "level_num": agent.level.level_num,
                "status": agent.status.value
            })

            if agent.status == AgentStatus.WORKING:
                departments[dept_name]["working"] += 1
            else:
                departments[dept_name]["standby"] += 1

        return {"departments": list(departments.values())}
    except Exception as e:
        return {"error": str(e)}


# =============================================================================
# PROFILE ENDPOINTS - Sistema de Perfis de Agentes
# =============================================================================

class TimeoutUpdate(BaseModel):
    timeout_hours: float


@app.get("/api/profiles")
async def list_profiles(area: str = None, department: str = None):
    """Lista perfis de agentes com habilidades e experiencias"""
    if not HAS_PROFILE_SERVICE:
        return {"profiles": [], "total": 0, "error": "Profile service not available"}

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


@app.get("/api/profiles/hierarchy")
async def get_full_hierarchy():
    """Retorna hierarquia completa com todos os agentes em estrutura de arvore"""
    if not HAS_PROFILE_SERVICE:
        return {"roots": [], "nodes": {}, "total": 0}

    service = get_profile_service()

    # Busca hierarquia de ambas as areas
    business = service.get_hierarchy_by_area("business")
    tech = service.get_hierarchy_by_area("technology")

    # Combina os nodes
    all_nodes = {**business["nodes"], **tech["nodes"]}

    # Encontra raizes globais (CEO e CIO)
    all_roots = business["roots"] + tech["roots"]

    return {
        "roots": all_roots,
        "nodes": all_nodes,
        "total": len(all_nodes),
        "business_count": len(business["nodes"]),
        "technology_count": len(tech["nodes"])
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
    """Lista agentes decisores com seus timeouts"""
    if not HAS_PROFILE_SERVICE:
        return {"decision_makers": [], "total": 0}

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
                "decisions_made": p.decisions_made,
                "reliability_score": p.calculate_reliability_score()
            }
            for p in profiles
        ],
        "total": len(profiles)
    }


@app.get("/api/profiles/top-performers")
async def get_top_performers(limit: int = 10):
    """Retorna top performers"""
    if not HAS_PROFILE_SERVICE:
        return {"top_performers": []}

    service = get_profile_service()
    return {"top_performers": service.get_top_performers(limit)}


@app.get("/api/productivity")
async def get_productivity():
    """Retorna metricas de produtividade dos agentes"""
    db = SessionLocal()
    try:
        agents = db.query(Agent).filter(Agent.enabled == True).all()

        # Conta atividades por agente
        from sqlalchemy import func
        agent_activity = db.query(
            ActivityLog.agent_id,
            func.count(ActivityLog.id).label('total_activities')
        ).group_by(ActivityLog.agent_id).all()

        activity_map = {a.agent_id: a.total_activities for a in agent_activity}

        # Calcula produtividade
        productivity_list = []
        for agent in agents:
            config = agent.config or {}
            metrics = config.get('metrics', {}) if isinstance(config, dict) else {}

            tasks_completed = metrics.get('tasks_completed', 0) if metrics else 0
            tasks_failed = metrics.get('tasks_failed', 0) if metrics else 0
            total_activities = activity_map.get(agent.agent_id, 0)

            # Score de produtividade
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

        # Ordena por produtividade
        productivity_list.sort(key=lambda x: x['productivity_score'], reverse=True)

        # Status summary
        working = len([a for a in agents if a.status in ['WORKING', 'EXECUTING']])
        standby = len([a for a in agents if a.status == 'STANDBY'])
        other = len(agents) - working - standby

        return {
            "agents": productivity_list,
            "summary": {
                "total": len(agents),
                "working": working,
                "standby": standby,
                "other": other,
                "avg_productivity": round(sum(a['productivity_score'] for a in productivity_list) / len(productivity_list), 1) if productivity_list else 0
            },
            "top_performers": productivity_list[:5],
            "needs_attention": [a for a in productivity_list if a['productivity_score'] < 30][-5:]
        }
    finally:
        db.close()


@app.get("/api/agents/status-summary")
async def get_agents_status_summary():
    """Retorna resumo de status dos agentes"""
    db = SessionLocal()
    try:
        agents = db.query(Agent).filter(Agent.enabled == True).all()

        status_summary = {}
        domain_summary = {}

        for agent in agents:
            # Por status
            status = agent.status or 'STANDBY'
            status_summary[status] = status_summary.get(status, 0) + 1

            # Por dominio
            domain = agent.domain or 'other'
            if domain not in domain_summary:
                domain_summary[domain] = {"working": 0, "standby": 0, "total": 0}

            domain_summary[domain]["total"] += 1
            if status in ['WORKING', 'EXECUTING']:
                domain_summary[domain]["working"] += 1
            else:
                domain_summary[domain]["standby"] += 1

        return {
            "by_status": status_summary,
            "by_domain": domain_summary,
            "total": len(agents)
        }
    finally:
        db.close()


# =============================================================================
# ORCHESTRATOR ENDPOINTS - Sistema de Orquestracao Autonoma
# =============================================================================

# Import do orquestrador
try:
    from factory.orchestrator.autonomous_orchestrator import (
        get_orchestrator, ProjectConfig, AutonomousOrchestrator
    )
    from factory.orchestrator.project_processor import ProjectProcessor
    from factory.orchestrator.story_generator import StoryGenerator
    HAS_ORCHESTRATOR = True
except ImportError:
    HAS_ORCHESTRATOR = False

# Armazena processadores de projetos ativos
_project_processors: Dict[str, ProjectProcessor] = {}


class ProjectCreateRequest(BaseModel):
    name: str
    description: str
    source_path: str
    output_path: Optional[str] = None
    process_videos: bool = True
    process_audio: bool = True
    process_documents: bool = True


class StoryCreateRequest(BaseModel):
    title: str
    as_a: str
    i_want: str
    so_that: str
    description: Optional[str] = ""
    acceptance_criteria: List[str]
    story_points: int = 3
    priority: int = 3
    sprint: int = 1
    tags: List[str] = []


class StoryEditRequest(BaseModel):
    title: Optional[str] = None
    as_a: Optional[str] = None
    i_want: Optional[str] = None
    so_that: Optional[str] = None
    description: Optional[str] = None
    story_points: Optional[int] = None
    priority: Optional[int] = None
    sprint: Optional[int] = None
    status: Optional[str] = None


class ApprovalRequest(BaseModel):
    approver_id: str
    notes: Optional[str] = None
    edits: Optional[Dict] = None


class RejectionRequest(BaseModel):
    rejector_id: str
    reason: str


@app.post("/api/orchestrator/projects")
async def create_orchestrator_project(request: ProjectCreateRequest):
    """Cria um novo projeto no orquestrador"""
    if not HAS_ORCHESTRATOR:
        raise HTTPException(status_code=503, detail="Orchestrator not available")

    project_id = f"PROJ-{datetime.now().strftime('%Y%m%d%H%M%S')}"
    output_path = request.output_path or f"C:\\Users\\lcruz\\Fabrica de Agentes\\projects\\{project_id}"

    # Cria processador do projeto
    processor = ProjectProcessor(
        project_id=project_id,
        project_name=request.name,
        source_path=request.source_path,
        output_path=output_path
    )

    _project_processors[project_id] = processor

    # Registra no orquestrador
    orchestrator = get_orchestrator()
    config = ProjectConfig(
        project_id=project_id,
        name=request.name,
        description=request.description,
        source_path=request.source_path,
        output_path=output_path,
        process_videos=request.process_videos,
        process_audio=request.process_audio,
        process_documents=request.process_documents
    )

    orchestrator.create_project(config)

    return {
        "success": True,
        "project_id": project_id,
        "message": f"Projeto '{request.name}' criado com sucesso"
    }


@app.post("/api/orchestrator/projects/{project_id}/start")
async def start_project_processing(project_id: str):
    """Inicia processamento autonomo do projeto"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    result = processor.start_processing()

    return {"success": True, "status": result['status'], "project_id": project_id}


@app.get("/api/orchestrator/projects/{project_id}")
async def get_project_status(project_id: str):
    """Retorna status detalhado do projeto"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    return processor.get_pm_report()


@app.get("/api/orchestrator/projects/{project_id}/stories")
async def get_project_stories(project_id: str, status: str = None):
    """Lista historias do projeto"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]

    if status:
        stories = processor.story_generator.get_stories_by_status(status)
    else:
        stories = processor.story_generator.stories

    return {
        "stories": [s.to_dict() for s in stories],
        "total": len(stories),
        "summary": processor.story_generator.get_backlog_summary()
    }


@app.post("/api/orchestrator/projects/{project_id}/stories")
async def create_story_manually(project_id: str, request: StoryCreateRequest):
    """Cria historia manualmente"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    story = processor.create_story_manually({
        "title": request.title,
        "as_a": request.as_a,
        "i_want": request.i_want,
        "so_that": request.so_that,
        "description": request.description,
        "acceptance_criteria": request.acceptance_criteria,
        "story_points": request.story_points,
        "priority": request.priority,
        "sprint": request.sprint,
        "tags": request.tags,
        "created_by": "user"
    })

    return {"success": True, "story": story.to_dict()}


@app.put("/api/orchestrator/projects/{project_id}/stories/{story_id}")
async def edit_story(project_id: str, story_id: str, request: StoryEditRequest):
    """Edita uma historia"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    updates = {k: v for k, v in request.dict().items() if v is not None}
    story = processor.edit_story(story_id, updates)

    if not story:
        raise HTTPException(status_code=404, detail="Historia nao encontrada")

    return {"success": True, "story": story.to_dict()}


@app.get("/api/orchestrator/projects/{project_id}/approvals")
async def get_pending_approvals(project_id: str):
    """Lista itens pendentes de aprovacao (Cockpit de Aprovacoes)"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    pending = processor.get_pending_approvals()

    return {
        "pending_approvals": pending,
        "total": len(pending)
    }


@app.post("/api/orchestrator/projects/{project_id}/stories/{story_id}/approve")
async def approve_story(project_id: str, story_id: str, request: ApprovalRequest):
    """Aprova uma historia"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    success = processor.approve_story(story_id, request.approver_id, request.notes)

    return {"success": success, "message": "Historia aprovada" if success else "Falha na aprovacao"}


@app.post("/api/orchestrator/projects/{project_id}/stories/{story_id}/reject")
async def reject_story(project_id: str, story_id: str, request: RejectionRequest):
    """Rejeita uma historia"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    success = processor.reject_story(story_id, request.rejector_id, request.reason)

    return {"success": success, "message": "Historia rejeitada" if success else "Erro ao rejeitar"}


@app.get("/api/orchestrator/projects/{project_id}/pm-report")
async def get_pm_report(project_id: str):
    """Relatorio do Project Manager"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    return processor.get_pm_report()


@app.get("/api/orchestrator/projects/{project_id}/agents-performance")
async def get_agents_performance(project_id: str):
    """Performance dos agentes no projeto"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    return {
        "agents": processor.get_agent_performance_report(),
        "total": len(processor.agent_performance)
    }


@app.get("/api/orchestrator/projects/{project_id}/burndown")
async def get_burndown_chart(project_id: str):
    """Dados para grafico de burndown"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    return {"burndown": processor.get_burndown_data()}


@app.get("/api/orchestrator/projects/{project_id}/timeline")
async def get_project_timeline(project_id: str):
    """Timeline de atividades do projeto"""
    if project_id not in _project_processors:
        raise HTTPException(status_code=404, detail="Projeto nao encontrado")

    processor = _project_processors[project_id]
    return {"timeline": processor.timeline}


@app.get("/api/orchestrator/all-approvals")
async def get_all_pending_approvals():
    """Lista TODAS as aprovacoes pendentes de todos os projetos (Cockpit Global)"""
    all_approvals = []

    for project_id, processor in _project_processors.items():
        pending = processor.get_pending_approvals()
        for item in pending:
            item['project_id'] = project_id
            item['project_name'] = processor.project_name
            all_approvals.append(item)

    return {
        "pending_approvals": all_approvals,
        "total": len(all_approvals),
        "by_project": {
            pid: len([a for a in all_approvals if a['project_id'] == pid])
            for pid in _project_processors.keys()
        }
    }


@app.get("/api/orchestrator/projects")
async def list_orchestrator_projects():
    """Lista todos os projetos do orquestrador"""
    projects = []
    for project_id, processor in _project_processors.items():
        report = processor.get_pm_report()
        projects.append({
            "project_id": project_id,
            "name": processor.project_name,
            "status": processor.status,
            "progress": report['progress']['percentage'],
            "stories_total": report['progress']['stories_total'],
            "stories_completed": report['progress']['stories_completed'],
            "estimated_completion": report['timeline']['estimated_completion']
        })

    return {"projects": projects, "total": len(projects)}


# =============================================================================
# DASHBOARD HTML
# =============================================================================

@app.get("/", response_class=HTMLResponse)
async def dashboard():
    """Dashboard principal"""
    return """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Fabrica de Agentes | Dashboard</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;500;600;700;800&display=swap" rel="stylesheet">
    <script>
        tailwind.config = {
            theme: {
                extend: {
                    colors: {
                        // Brand Colors
                        brand: {
                            blue: '#003B4A',      // Azul principal
                            'blue-light': '#006277',  // Azul claro
                            'blue-sky': '#00B5F1',    // Azul celeste
                            red: '#ED1C24',       // Vermelho
                            'red-dark': '#962F34',    // Vermelho escuro
                            orange: '#FF6C00',    // Laranja
                            yellow: '#FDB913',    // Amarelo
                            green: '#00A799',     // Verde
                            purple: '#634976',    // Lilas
                            gray: '#636466',      // Cinza
                            'gray-light': '#D3CAB7',  // Cinza claro
                        }
                    },
                    fontFamily: {
                        'sans': ['Open Sans', 'sans-serif'],
                    }
                }
            }
        }
    </script>
    <style>
        * { font-family: 'Open Sans', sans-serif; }

        /* Scrollbar customizada */
        .scrollbar-thin::-webkit-scrollbar { width: 6px; }
        .scrollbar-thin::-webkit-scrollbar-track { background: #003B4A; }
        .scrollbar-thin::-webkit-scrollbar-thumb { background: #006277; border-radius: 3px; }
        .scrollbar-thin::-webkit-scrollbar-thumb:hover { background: #00B5F1; }

        /* Utilitarios */
        .line-clamp-2 { display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden; }

        /* Status badges */
        .status-standby { background-color: #636466; }
        .status-executing { background-color: #00A799; }
        .status-error { background-color: #ED1C24; }
        .status-planning { background-color: #634976; }
        .status-in_progress { background-color: #FF6C00; }
        .status-completed, .status-done { background-color: #00A799; }

        /* Animacoes de entrada */
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        @keyframes slideInRight {
            from { opacity: 0; transform: translateX(20px); }
            to { opacity: 1; transform: translateX(0); }
        }
        @keyframes slideInLeft {
            from { opacity: 0; transform: translateX(-20px); }
            to { opacity: 1; transform: translateX(0); }
        }
        @keyframes scaleIn {
            from { opacity: 0; transform: scale(0.95); }
            to { opacity: 1; transform: scale(1); }
        }
        @keyframes pulse-glow {
            0%, 100% { box-shadow: 0 0 0 0 rgba(0, 181, 241, 0.4); }
            50% { box-shadow: 0 0 20px 5px rgba(0, 181, 241, 0.2); }
        }

        .animate-fade-in { animation: fadeIn 0.3s ease-out forwards; }
        .animate-slide-right { animation: slideInRight 0.3s ease-out forwards; }
        .animate-slide-left { animation: slideInLeft 0.3s ease-out forwards; }
        .animate-scale-in { animation: scaleIn 0.2s ease-out forwards; }
        .animate-pulse-glow { animation: pulse-glow 2s ease-in-out infinite; }

        /* Delays de animacao */
        .delay-100 { animation-delay: 100ms; }
        .delay-200 { animation-delay: 200ms; }
        .delay-300 { animation-delay: 300ms; }
        .delay-400 { animation-delay: 400ms; }

        /* Hover effects melhorados */
        .hover-lift { transition: transform 0.2s ease, box-shadow 0.2s ease; }
        .hover-lift:hover { transform: translateY(-2px); box-shadow: 0 8px 25px rgba(0,0,0,0.3); }

        .hover-glow { transition: box-shadow 0.2s ease; }
        .hover-glow:hover { box-shadow: 0 0 20px rgba(255, 108, 0, 0.3); }

        /* Loading skeleton */
        @keyframes shimmer {
            0% { background-position: -200% 0; }
            100% { background-position: 200% 0; }
        }
        .skeleton {
            background: linear-gradient(90deg, #006277 25%, #00B5F1 50%, #006277 75%);
            background-size: 200% 100%;
            animation: shimmer 1.5s infinite;
            border-radius: 4px;
        }

        /* Toast notifications */
        .toast-container {
            position: fixed;
            top: 20px;
            right: 20px;
            z-index: 9999;
            display: flex;
            flex-direction: column;
            gap: 10px;
        }
        .toast {
            padding: 12px 20px;
            border-radius: 8px;
            color: white;
            font-size: 14px;
            font-weight: 500;
            box-shadow: 0 4px 20px rgba(0,0,0,0.3);
            animation: slideInRight 0.3s ease-out;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .toast-success { background: linear-gradient(135deg, #00A799, #006277); }
        .toast-error { background: linear-gradient(135deg, #ED1C24, #962F34); }
        .toast-info { background: linear-gradient(135deg, #00B5F1, #006277); }
        .toast-warning { background: linear-gradient(135deg, #FF6C00, #FDB913); }

        /* Focus states para acessibilidade */
        button:focus-visible, a:focus-visible, input:focus-visible, select:focus-visible {
            outline: 2px solid #00B5F1;
            outline-offset: 2px;
        }

        /* Transicoes globais */
        * { transition-property: background-color, border-color, color, fill, stroke, opacity, box-shadow, transform;
            transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
            transition-duration: 150ms; }

        /* Cards interativos */
        .card-interactive {
            transition: all 0.2s ease;
            cursor: pointer;
        }
        .card-interactive:hover {
            transform: translateY(-2px);
            box-shadow: 0 10px 30px rgba(0,0,0,0.2);
            border-color: #FF6C00 !important;
        }
        .card-interactive:active {
            transform: translateY(0);
        }

        /* Sidebar item hover */
        .sidebar-item {
            position: relative;
            overflow: hidden;
        }
        .sidebar-item::before {
            content: '';
            position: absolute;
            left: 0;
            top: 0;
            height: 100%;
            width: 3px;
            background: #FF6C00;
            transform: scaleY(0);
            transition: transform 0.2s ease;
        }
        .sidebar-item:hover::before,
        .sidebar-item.active::before {
            transform: scaleY(1);
        }

        /* Badge pulse para notificacoes */
        .badge-pulse {
            position: relative;
        }
        .badge-pulse::after {
            content: '';
            position: absolute;
            top: -2px;
            right: -2px;
            width: 8px;
            height: 8px;
            background: #ED1C24;
            border-radius: 50%;
            animation: pulse 1.5s infinite;
        }
        @keyframes pulse {
            0%, 100% { transform: scale(1); opacity: 1; }
            50% { transform: scale(1.2); opacity: 0.8; }
        }

        /* Empty states */
        .empty-state {
            text-align: center;
            padding: 60px 20px;
            color: #636466;
        }
        .empty-state svg {
            width: 80px;
            height: 80px;
            margin: 0 auto 20px;
            opacity: 0.3;
        }

        /* Toast transitions */
        .toast-enter-active { animation: slideInRight 0.3s ease-out; }
        .toast-leave-active { animation: slideInRight 0.3s ease-out reverse; }

        /* Agent card hover effect */
        .agent-card {
            transition: all 0.2s ease;
            cursor: pointer;
        }
        .agent-card:hover {
            transform: translateX(4px);
            background: rgba(0, 98, 119, 0.5) !important;
            border-left: 3px solid #FF6C00;
        }

        /* Department header effect */
        .dept-header {
            transition: all 0.2s ease;
        }
        .dept-header:hover {
            background: rgba(0, 98, 119, 0.3) !important;
        }

        /* Profile panel slide animation */
        .profile-panel {
            animation: slideInRight 0.3s ease-out;
        }

        /* Stats counter animation */
        @keyframes countUp {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .stat-value {
            animation: countUp 0.4s ease-out;
        }

        /* Scroll smooth */
        .scroll-smooth {
            scroll-behavior: smooth;
        }

        /* Custom scrollbar */
        ::-webkit-scrollbar { width: 8px; height: 8px; }
        ::-webkit-scrollbar-track { background: #003B4A; }
        ::-webkit-scrollbar-thumb { background: #006277; border-radius: 4px; }
        ::-webkit-scrollbar-thumb:hover { background: #00B5F1; }
    </style>
</head>
<body class="bg-brand-blue text-white min-h-screen">
    <div id="app">
        <!-- Toast Container -->
        <div class="toast-container">
            <transition-group name="toast">
                <div v-for="toast in toasts" :key="toast.id"
                     :class="['toast', 'toast-' + toast.type]"
                     @click="removeToast(toast.id)">
                    <div class="flex items-center gap-3">
                        <svg v-if="toast.type === 'success'" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                        </svg>
                        <svg v-else-if="toast.type === 'error'" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                        </svg>
                        <svg v-else class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                        </svg>
                        <span>{{ toast.message }}</span>
                    </div>
                </div>
            </transition-group>
        </div>

        <!-- Header -->
        <header class="bg-gradient-to-r from-brand-red-dark to-brand-red px-6 py-4 shadow-lg">
            <div class="flex items-center justify-between">
                <div class="flex items-center gap-4">
                    <h1 class="text-2xl font-bold">Fabrica de Agentes</h1>
                    <span class="px-3 py-1 text-xs bg-brand-orange rounded-full font-semibold animate-pulse">v3.0</span>
                </div>
                <div class="flex items-center gap-6">
                    <!-- Seletor de Projeto Global -->
                    <div class="flex items-center gap-2">
                        <span class="text-sm opacity-80">Projeto:</span>
                        <select v-model="selectedProject" @change="onProjectChange"
                                class="bg-white/20 border border-white/30 rounded px-3 py-1.5 text-sm font-medium focus:outline-none focus:ring-2 focus:ring-white/50">
                            <option value="all">Todos os Projetos</option>
                            <option v-for="p in projects" :key="p.project_id" :value="p.project_id">
                                {{ p.name }}
                            </option>
                        </select>
                    </div>
                    <div class="flex items-center gap-4 text-sm opacity-80">
                        <span class="flex items-center gap-2">
                            <span class="w-2 h-2 rounded-full bg-brand-green animate-pulse"></span>
                            {{ status?.factory?.status || 'conectando...' }}
                        </span>
                        <span>{{ currentTime }}</span>
                    </div>
                </div>
            </div>
        </header>

        <div class="flex">
            <!-- Sidebar -->
            <aside class="w-64 bg-brand-blue min-h-screen border-r border-brand-blue-light/30 p-4">
                <nav class="space-y-1">
                    <button @click="currentView = 'overview'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'overview' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2V6zM14 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V6zM4 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2v-2zM14 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"></path></svg>
                        Visao Geral
                    </button>
                    <button @click="currentView = 'kanban'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'kanban' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 17V7m0 10a2 2 0 01-2 2H5a2 2 0 01-2-2V7a2 2 0 012-2h2a2 2 0 012 2m0 10a2 2 0 002 2h2a2 2 0 002-2M9 7a2 2 0 012-2h2a2 2 0 012 2m0 10V7m0 10a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 00-2-2h-2a2 2 0 00-2 2"></path></svg>
                        Kanban
                    </button>
                    <button @click="currentView = 'sprints'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'sprints' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path></svg>
                        Sprints
                    </button>
                    <button @click="currentView = 'agents'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'agents' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path></svg>
                        Agentes
                    </button>
                    <button @click="currentView = 'skills'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'skills' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"></path></svg>
                        Skills
                    </button>
                    <button @click="currentView = 'projects'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'projects' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"></path></svg>
                        Projetos
                    </button>
                    <button @click="currentView = 'logs'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'logs' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path></svg>
                        Logs
                    </button>

                    <div class="border-t border-brand-blue-light/30 my-3 pt-3">
                        <span class="px-2 text-xs text-brand-red font-semibold uppercase tracking-wider">Gestao</span>
                    </div>

                    <button @click="currentView = 'approvals'; fetchPendingApprovals();"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all relative',
                                     currentView === 'approvals' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"></path></svg>
                        Aprovacoes
                        <span v-if="pendingApprovals.length > 0" class="absolute right-2 bg-brand-red text-white text-xs rounded-full w-5 h-5 flex items-center justify-center animate-pulse">
                            {{ pendingApprovals.length }}
                        </span>
                    </button>

                    <button @click="currentView = 'pmReport'; fetchPMReport();"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'pmReport' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
                        PM Report
                    </button>

                    <button @click="currentView = 'createStory'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'createStory' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path></svg>
                        Criar Historia
                    </button>

                    <div class="border-t border-brand-blue-light/30 my-3 pt-3">
                        <span class="px-2 text-xs text-brand-orange font-semibold uppercase tracking-wider">Analytics</span>
                    </div>

                    <button @click="currentView = 'productivity'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'productivity' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
                        Produtividade
                    </button>
                    <button @click="currentView = 'hierarchy'"
                            :class="['sidebar-item w-full text-left px-4 py-2.5 rounded-lg flex items-center gap-3 transition-all',
                                     currentView === 'hierarchy' ? 'bg-brand-orange text-white active' : 'hover:bg-brand-blue-light/50 text-gray-300']">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"></path></svg>
                        Timeouts
                    </button>
                </nav>

                <!-- Quick Stats -->
                <div class="mt-8 space-y-3">
                    <h3 class="text-xs font-bold text-brand-orange uppercase tracking-wider px-2">Resumo Geral</h3>
                    <div class="bg-brand-blue-light/30 rounded-lg p-3 border border-brand-blue-light/20">
                        <div class="text-2xl font-bold text-brand-yellow">{{ status?.projects?.total || 0 }}</div>
                        <div class="text-sm text-gray-400">Projetos Ativos</div>
                    </div>
                    <div class="bg-brand-blue-light/30 rounded-lg p-3 border border-brand-blue-light/20">
                        <div class="text-2xl font-bold text-brand-green">{{ status?.agents?.total || 0 }}</div>
                        <div class="text-sm text-gray-400">Agentes</div>
                    </div>
                    <div class="bg-brand-blue-light/30 rounded-lg p-3 border border-brand-blue-light/20">
                        <div class="text-2xl font-bold text-brand-blue-sky">{{ filteredStories.length }}</div>
                        <div class="text-sm text-gray-400">Stories</div>
                    </div>
                    <div class="bg-brand-blue-light/30 rounded-lg p-3 border border-brand-blue-light/20">
                        <div class="text-2xl font-bold text-brand-orange">{{ metrics?.stories?.completion_rate || 0 }}%</div>
                        <div class="text-sm text-gray-400">Conclusao</div>
                    </div>
                </div>
            </aside>

            <!-- Main Content -->
            <main class="flex-1 p-6 overflow-auto" style="max-height: calc(100vh - 72px);">

                <!-- OVERVIEW VIEW -->
                <div v-if="currentView === 'overview'">
                    <div class="flex justify-between items-center mb-6">
                        <h2 class="text-xl font-bold text-white">
                            {{ selectedProject === 'all' ? 'Visao Geral - Todos os Projetos' : 'Visao Geral - ' + selectedProjectName }}
                        </h2>
                    </div>

                    <!-- KPIs -->
                    <div class="grid grid-cols-4 gap-4 mb-6">
                        <div class="bg-gradient-to-br from-brand-blue-light to-brand-blue rounded-xl p-4 border border-brand-blue-light/30 hover-lift animate-fade-in">
                            <div class="flex justify-between items-start">
                                <div>
                                    <div class="text-3xl font-bold text-white">{{ metrics?.stories?.total || 0 }}</div>
                                    <div class="text-sm text-gray-300 mt-1">Total Stories</div>
                                </div>
                                <div class="p-2 bg-brand-orange/20 rounded-lg">
                                    <svg class="w-6 h-6 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path></svg>
                                </div>
                            </div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-green/80 to-brand-green rounded-xl p-4 border border-brand-green/30 hover-lift animate-fade-in delay-100">
                            <div class="flex justify-between items-start">
                                <div>
                                    <div class="text-3xl font-bold text-white">{{ metrics?.stories?.done || 0 }}</div>
                                    <div class="text-sm text-gray-100 mt-1">Concluidas</div>
                                </div>
                                <div class="p-2 bg-white/20 rounded-lg">
                                    <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"></path></svg>
                                </div>
                            </div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-orange/80 to-brand-orange rounded-xl p-4 border border-brand-orange/30 hover-lift animate-fade-in delay-200">
                            <div class="flex justify-between items-start">
                                <div>
                                    <div class="text-3xl font-bold text-white">{{ metrics?.stories?.in_progress || 0 }}</div>
                                    <div class="text-sm text-gray-100 mt-1">Em Progresso</div>
                                </div>
                                <div class="p-2 bg-white/20 rounded-lg">
                                    <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path></svg>
                                </div>
                            </div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-yellow/80 to-brand-yellow rounded-xl p-4 border border-brand-yellow/30 hover-lift animate-fade-in delay-300">
                            <div class="flex justify-between items-start">
                                <div>
                                    <div class="text-3xl font-bold text-brand-blue">{{ metrics?.points?.done || 0 }}</div>
                                    <div class="text-sm text-brand-blue/80 mt-1">Pontos Entregues</div>
                                </div>
                                <div class="p-2 bg-brand-blue/20 rounded-lg">
                                    <svg class="w-6 h-6 text-brand-blue" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11.049 2.927c.3-.921 1.603-.921 1.902 0l1.519 4.674a1 1 0 00.95.69h4.915c.969 0 1.371 1.24.588 1.81l-3.976 2.888a1 1 0 00-.363 1.118l1.518 4.674c.3.922-.755 1.688-1.538 1.118l-3.976-2.888a1 1 0 00-1.176 0l-3.976 2.888c-.783.57-1.838-.197-1.538-1.118l1.518-4.674a1 1 0 00-.363-1.118l-3.976-2.888c-.784-.57-.38-1.81.588-1.81h4.914a1 1 0 00.951-.69l1.519-4.674z"></path></svg>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Agents Grid + Recent Activity -->
                    <div class="grid grid-cols-3 gap-6">
                        <!-- Agents Status -->
                        <div class="col-span-2">
                            <h3 class="text-lg font-semibold mb-4 text-brand-orange">Status dos Agentes</h3>
                            <div class="grid grid-cols-2 md:grid-cols-4 gap-3">
                                <div v-for="agent in status?.agents?.list" :key="agent.id"
                                     class="bg-brand-blue-light/20 rounded-lg p-3 border border-brand-blue-light/30 hover:border-brand-orange/50 transition-all cursor-pointer"
                                     @click="showAgentDetail(agent)">
                                    <div class="flex items-center gap-2 mb-2">
                                        <div :class="['w-2.5 h-2.5 rounded-full', getAgentStatusColor(agent.status)]"></div>
                                        <span class="font-semibold text-sm text-brand-yellow">{{ agent.id }}</span>
                                    </div>
                                    <div class="text-xs text-gray-300 truncate">{{ agent.name }}</div>
                                    <div class="flex items-center justify-between mt-2">
                                        <span class="text-xs px-2 py-0.5 rounded-full" :class="getAgentStatusBadge(agent.status)">
                                            {{ agent.status }}
                                        </span>
                                        <span class="text-xs text-gray-500">{{ agent.metrics?.tasks_completed || 0 }} tasks</span>
                                    </div>
                                    <!-- Projeto atual -->
                                    <div v-if="agent.current_project" class="mt-2 text-xs text-brand-blue-sky">
                                        Projeto: {{ agent.current_project }}
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Recent Activity -->
                        <div>
                            <h3 class="text-lg font-semibold mb-4 text-brand-orange">Atividade Recente</h3>
                            <div class="bg-brand-blue-light/20 rounded-lg border border-brand-blue-light/30 overflow-hidden max-h-80 overflow-y-auto scrollbar-thin">
                                <div v-for="log in status?.recent_logs?.slice(0, 15)" :key="log.id"
                                     class="px-3 py-2 border-b border-brand-blue-light/20 text-sm hover:bg-brand-blue-light/30">
                                    <div class="flex items-center gap-2">
                                        <span :class="['px-1.5 py-0.5 rounded text-xs font-medium', getLogClass(log.level)]">
                                            {{ log.level }}
                                        </span>
                                        <span class="text-brand-yellow text-xs">{{ log.agent_id || log.source }}</span>
                                    </div>
                                    <div class="text-gray-300 text-xs mt-1 truncate">{{ log.message }}</div>
                                    <div class="text-gray-500 text-xs mt-0.5">{{ formatTime(log.timestamp) }}</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- KANBAN VIEW -->
                <div v-if="currentView === 'kanban'">
                    <!-- AVISO: Selecionar Projeto -->
                    <div v-if="selectedProject === 'all'" class="bg-brand-orange/20 border border-brand-orange rounded-xl p-8 text-center mb-6">
                        <svg class="w-16 h-16 mx-auto text-brand-orange mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"></path>
                        </svg>
                        <h3 class="text-xl font-bold text-white mb-2">Selecione um Projeto</h3>
                        <p class="text-gray-300 mb-4">Para usar o Kanban, selecione um projeto especifico no seletor acima.</p>
                        <div class="flex justify-center gap-2 flex-wrap">
                            <button v-for="p in projects" :key="p.project_id"
                                    @click="selectedProject = p.project_id; onProjectChange();"
                                    class="px-4 py-2 bg-brand-blue-light hover:bg-brand-orange rounded-lg transition text-sm">
                                {{ p.name }}
                            </button>
                        </div>
                    </div>

                    <div v-else>
                        <div class="flex justify-between items-center mb-6">
                            <h2 class="text-xl font-bold text-white">
                                Kanban - {{ selectedProjectName }}
                            </h2>
                            <div class="flex items-center gap-4">
                                <select v-model="selectedSprint" class="bg-brand-blue-light/30 border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                                    <option value="all">Todos os Sprints</option>
                                    <option v-for="sp in sprintList" :key="sp" :value="sp">Sprint {{ sp }}</option>
                                </select>
                                <button @click="showQuickAddStory = true"
                                        class="px-4 py-2 bg-brand-orange text-white rounded-lg hover:bg-brand-orange/80 transition flex items-center gap-2">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path></svg>
                                    Nova Story
                                </button>
                            </div>
                        </div>

                    <!-- Quick Add Story Form -->
                    <div v-if="showQuickAddStory" class="bg-brand-blue-light/30 rounded-xl p-4 mb-6 border border-brand-orange/30">
                        <div class="flex justify-between items-center mb-4">
                            <h3 class="font-semibold text-white">Adicionar Story Rapida</h3>
                            <button @click="showQuickAddStory = false" class="text-gray-400 hover:text-white">
                                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
                            </button>
                        </div>
                        <div class="grid grid-cols-3 gap-4">
                            <div class="col-span-2">
                                <input v-model="quickStory.title" type="text" placeholder="Titulo da Story"
                                       class="w-full bg-brand-blue border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                            </div>
                            <div>
                                <select v-model="quickStory.sprint" class="w-full bg-brand-blue border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                                    <option v-for="sp in sprintList" :key="sp" :value="sp">Sprint {{ sp }}</option>
                                    <option :value="Math.max(...sprintList, 0) + 1">Novo Sprint ({{ Math.max(...sprintList, 0) + 1 }})</option>
                                </select>
                            </div>
                        </div>
                        <div class="grid grid-cols-2 gap-4 mt-3">
                            <textarea v-model="quickStory.description" placeholder="Descricao (opcional)" rows="2"
                                      class="bg-brand-blue border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange"></textarea>
                            <div class="flex gap-4">
                                <div class="flex-1">
                                    <label class="text-xs text-gray-400 mb-1 block">Pontos</label>
                                    <input v-model.number="quickStory.points" type="number" min="0" max="21"
                                           class="w-full bg-brand-blue border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                                </div>
                                <div class="flex-1">
                                    <label class="text-xs text-gray-400 mb-1 block">Prioridade</label>
                                    <select v-model.number="quickStory.priority" class="w-full bg-brand-blue border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                                        <option :value="1">1 - Alta</option>
                                        <option :value="3">3 - Media</option>
                                        <option :value="5">5 - Baixa</option>
                                    </select>
                                </div>
                            </div>
                        </div>
                        <div class="flex justify-end mt-4 gap-2">
                            <button @click="showQuickAddStory = false" class="px-4 py-2 bg-brand-gray text-gray-300 rounded-lg hover:bg-brand-gray/80 transition">Cancelar</button>
                            <button @click="createQuickStory()" class="px-4 py-2 bg-brand-orange text-white rounded-lg hover:bg-brand-orange/80 transition">Criar Story</button>
                        </div>
                    </div>

                    <div class="grid grid-cols-6 gap-3">
                        <!-- Backlog -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-gray/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-gray-400 text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-gray"></span>
                                Backlog <span class="ml-auto text-xs font-normal">({{ kanbanStories.backlog.length }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in kanbanStories.backlog" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-blue-light/20 hover:border-brand-orange/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-yellow">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-blue-light/30 rounded">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2">
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <button @click.stop="setStoryStatus(story, 'TO_DO')"
                                                class="px-2 py-1 bg-brand-yellow/20 text-brand-yellow rounded hover:bg-brand-yellow/40 text-xs">
                                            Iniciar →
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- To Do -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-yellow/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-brand-yellow text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-yellow"></span>
                                To Do <span class="ml-auto text-xs font-normal">({{ kanbanStories.todo.length }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in kanbanStories.todo" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-yellow/20 hover:border-brand-yellow/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-yellow">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-yellow/20 rounded text-brand-yellow">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2 gap-1">
                                        <button @click.stop="setStoryStatus(story, 'BACKLOG')"
                                                class="px-1.5 py-0.5 bg-brand-gray/30 text-gray-400 rounded hover:bg-brand-gray/50 text-xs">←</button>
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <button @click.stop="setStoryStatus(story, 'IN_PROGRESS')"
                                                class="px-2 py-1 bg-brand-orange/20 text-brand-orange rounded hover:bg-brand-orange/40 text-xs">
                                            Iniciar →
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- In Progress -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-orange/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-brand-orange text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-orange animate-pulse"></span>
                                Em Prog. <span class="ml-auto text-xs font-normal">({{ kanbanStories.inProgress.length }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in kanbanStories.inProgress" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-orange/20 hover:border-brand-orange/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-orange">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-orange/20 rounded text-brand-orange">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2 gap-1">
                                        <button @click.stop="setStoryStatus(story, 'TO_DO')"
                                                class="px-1.5 py-0.5 bg-brand-gray/30 text-gray-400 rounded hover:bg-brand-gray/50 text-xs">←</button>
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <button @click.stop="setStoryStatus(story, 'TESTING')"
                                                class="px-2 py-1 bg-brand-purple/20 text-brand-purple rounded hover:bg-brand-purple/40 text-xs">
                                            Testar →
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Testing -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-purple/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-brand-purple text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-purple"></span>
                                Teste <span class="ml-auto text-xs font-normal">({{ kanbanStories.testing?.length || 0 }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in (kanbanStories.testing || [])" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-purple/20 hover:border-brand-purple/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-purple">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-purple/20 rounded text-brand-purple">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2 gap-1">
                                        <button @click.stop="setStoryStatus(story, 'IN_PROGRESS')"
                                                class="px-1.5 py-0.5 bg-brand-gray/30 text-gray-400 rounded hover:bg-brand-gray/50 text-xs">←</button>
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <button @click.stop="setStoryStatus(story, 'DONE')"
                                                class="px-2 py-1 bg-brand-green/20 text-brand-green rounded hover:bg-brand-green/40 text-xs">
                                            Concluir →
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Blocked -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-red/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-brand-red text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-red"></span>
                                Bloq. <span class="ml-auto text-xs font-normal">({{ kanbanStories.blocked?.length || 0 }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in (kanbanStories.blocked || [])" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-red/20 hover:border-brand-red/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-red">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-red/20 rounded text-brand-red">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2 gap-1">
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <button @click.stop="setStoryStatus(story, 'TO_DO')"
                                                class="px-2 py-1 bg-brand-yellow/20 text-brand-yellow rounded hover:bg-brand-yellow/40 text-xs">
                                            Desbloquear →
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Done -->
                        <div class="bg-brand-blue-light/20 rounded-xl p-3 border border-brand-green/30">
                            <h3 class="font-semibold mb-3 flex items-center gap-2 text-brand-green text-sm">
                                <span class="w-2.5 h-2.5 rounded-full bg-brand-green"></span>
                                Done <span class="ml-auto text-xs font-normal">({{ kanbanStories.done.length }})</span>
                            </h3>
                            <div class="space-y-2 max-h-[calc(100vh-320px)] overflow-y-auto scrollbar-thin pr-1">
                                <div v-for="story in kanbanStories.done" :key="story.story_id"
                                     class="bg-brand-blue rounded-lg p-2.5 text-xs transition-all border border-brand-green/20 hover:border-brand-green/50">
                                    <div class="flex justify-between items-start cursor-pointer" @click="showStoryDetail(story)">
                                        <div class="font-medium text-brand-green">{{ story.story_id }}</div>
                                        <span class="px-1.5 py-0.5 bg-brand-green/20 rounded text-brand-green">{{ story.points }}p</span>
                                    </div>
                                    <div class="text-gray-300 mt-1 line-clamp-2 cursor-pointer" @click="showStoryDetail(story)">{{ story.title }}</div>
                                    <div class="flex items-center justify-between mt-2 gap-1">
                                        <button @click.stop="setStoryStatus(story, 'TESTING')"
                                                class="px-1.5 py-0.5 bg-brand-gray/30 text-gray-400 rounded hover:bg-brand-gray/50 text-xs">← Reabrir</button>
                                        <span class="text-gray-500">S{{ story.sprint }}</span>
                                        <span class="text-brand-green text-xs">Concluido</span>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    </div>
                </div>

                <!-- SPRINTS VIEW -->
                <div v-if="currentView === 'sprints'">
                    <!-- AVISO: Selecionar Projeto -->
                    <div v-if="selectedProject === 'all'" class="bg-brand-orange/20 border border-brand-orange rounded-xl p-8 text-center">
                        <svg class="w-16 h-16 mx-auto text-brand-orange mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"></path>
                        </svg>
                        <h3 class="text-xl font-bold text-white mb-2">Selecione um Projeto</h3>
                        <p class="text-gray-300 mb-4">Para gerenciar Sprints, selecione um projeto especifico no seletor acima.</p>
                        <div class="flex justify-center gap-2 flex-wrap">
                            <button v-for="p in projects" :key="p.project_id"
                                    @click="selectedProject = p.project_id; onProjectChange();"
                                    class="px-4 py-2 bg-brand-blue-light hover:bg-brand-orange rounded-lg transition text-sm">
                                {{ p.name }}
                            </button>
                        </div>
                    </div>

                    <div v-else>
                        <div class="flex justify-between items-center mb-6">
                            <h2 class="text-xl font-bold text-white">
                                Sprints - {{ selectedProjectName }}
                            </h2>
                            <button @click="showQuickAddStory = true"
                                    class="px-4 py-2 bg-brand-orange text-white rounded-lg hover:bg-brand-orange/80 transition flex items-center gap-2">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path></svg>
                                Nova Story
                            </button>
                        </div>

                    <div class="grid gap-4">
                        <div v-for="sprint in sprintData" :key="sprint.number"
                             class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30">
                            <div class="flex justify-between items-start mb-4">
                                <div>
                                    <h3 class="font-bold text-lg text-white">Sprint {{ sprint.number }}</h3>
                                    <p class="text-gray-400 text-sm">{{ sprint.total }} stories | {{ sprint.points }} pontos</p>
                                </div>
                                <span :class="['px-4 py-1.5 rounded-full text-sm font-semibold',
                                              sprint.done === sprint.total ? 'bg-brand-green text-white' : 'bg-brand-orange text-white']">
                                    {{ sprint.done === sprint.total ? 'Completo' : 'Em andamento' }}
                                </span>
                            </div>

                            <!-- Progress bar -->
                            <div class="mb-4">
                                <div class="flex justify-between text-sm text-gray-400 mb-2">
                                    <span>Progresso</span>
                                    <span class="font-semibold text-brand-green">{{ sprint.percentage }}%</span>
                                </div>
                                <div class="w-full bg-brand-blue rounded-full h-3">
                                    <div class="bg-gradient-to-r from-brand-green to-brand-blue-sky h-3 rounded-full transition-all duration-500"
                                         :style="{ width: sprint.percentage + '%' }"></div>
                                </div>
                            </div>

                            <!-- Stats -->
                            <div class="grid grid-cols-4 gap-3 text-center">
                                <div class="bg-brand-green/20 rounded-lg p-3 border border-brand-green/30">
                                    <div class="text-2xl font-bold text-brand-green">{{ sprint.done }}</div>
                                    <div class="text-xs text-gray-400">Done</div>
                                </div>
                                <div class="bg-brand-orange/20 rounded-lg p-3 border border-brand-orange/30">
                                    <div class="text-2xl font-bold text-brand-orange">{{ sprint.inProgress }}</div>
                                    <div class="text-xs text-gray-400">In Progress</div>
                                </div>
                                <div class="bg-brand-yellow/20 rounded-lg p-3 border border-brand-yellow/30">
                                    <div class="text-2xl font-bold text-brand-yellow">{{ sprint.todo }}</div>
                                    <div class="text-xs text-gray-400">To Do</div>
                                </div>
                                <div class="bg-brand-gray/20 rounded-lg p-3 border border-brand-gray/30">
                                    <div class="text-2xl font-bold text-gray-400">{{ sprint.backlog }}</div>
                                    <div class="text-xs text-gray-400">Backlog</div>
                                </div>
                            </div>

                            <!-- Toggle stories -->
                            <div class="mt-4">
                                <button @click="sprint.expanded = !sprint.expanded"
                                        class="text-sm text-brand-orange hover:text-brand-yellow transition-colors flex items-center gap-2">
                                    <svg :class="['w-4 h-4 transition-transform', sprint.expanded ? 'rotate-90' : '']" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"></path>
                                    </svg>
                                    {{ sprint.expanded ? 'Ocultar stories' : 'Ver stories (' + sprint.total + ')' }}
                                </button>
                                <div v-if="sprint.expanded" class="mt-3 space-y-2">
                                    <div v-for="story in sprint.stories" :key="story.story_id"
                                         @click="showStoryDetail(story)"
                                         class="bg-brand-blue rounded-lg p-3 text-sm flex justify-between items-center cursor-pointer hover:bg-brand-blue-light/30 transition-all">
                                        <div class="flex items-center gap-3">
                                            <span class="text-brand-yellow font-mono font-semibold">{{ story.story_id }}</span>
                                            <span class="text-gray-300">{{ story.title }}</span>
                                        </div>
                                        <div class="flex items-center gap-2">
                                            <span v-if="story.agents && story.agents.length" class="text-xs text-brand-blue-sky">
                                                {{ story.agents.length }} agentes
                                            </span>
                                            <span :class="['px-2 py-0.5 rounded text-xs font-medium', getStatusClass(story.status)]">
                                                {{ story.status }}
                                            </span>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    </div>
                </div>

                <!-- AGENTS VIEW - ORGANOGRAMA HIERARQUICO POR DEPARTAMENTO -->
                <div v-if="currentView === 'agents'">
                    <!-- Header com estatisticas -->
                    <div class="flex justify-between items-center mb-6">
                        <div>
                            <h2 class="text-xl font-bold text-white">Estrutura Organizacional</h2>
                            <p class="text-sm text-gray-400 mt-1">{{ orgDepartments?.length || 0 }} departamentos | {{ Object.keys(fullHierarchy?.nodes || {}).length }} agentes</p>
                        </div>
                        <div class="flex items-center gap-4">
                            <div class="flex items-center gap-3 text-xs">
                                <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-brand-purple"></span> Negocios</span>
                                <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-brand-blue-sky"></span> Tecnologia</span>
                                <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-brand-yellow"></span> Decisor</span>
                            </div>
                        </div>
                    </div>

                    <!-- Layout Principal: Organograma em Arvore + Perfil LinkedIn -->
                    <div class="flex gap-6">
                        <!-- ORGANOGRAMA EM ARVORE (lado esquerdo) -->
                        <div class="flex-1 space-y-4 overflow-auto pr-2" style="max-height: calc(100vh - 220px);">

                            <!-- AREA: NEGOCIOS -->
                            <div class="bg-gradient-to-r from-brand-purple/20 to-transparent rounded-xl border border-brand-purple/30">
                                <div @click="expandedAreas.business = !expandedAreas.business"
                                     class="flex items-center justify-between p-4 cursor-pointer hover:bg-brand-purple/10 transition-all rounded-t-xl">
                                    <div class="flex items-center gap-3">
                                        <div class="w-10 h-10 rounded-lg bg-brand-purple flex items-center justify-center">
                                            <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4"/>
                                            </svg>
                                        </div>
                                        <div>
                                            <h3 class="text-lg font-bold text-white">Area de Negocios</h3>
                                            <p class="text-sm text-gray-400">{{ getBusinessDepartments().length }} departamentos | {{ countAgentsByArea('business') }} agentes</p>
                                        </div>
                                    </div>
                                    <svg :class="['w-5 h-5 text-brand-purple transition-transform', expandedAreas.business ? 'rotate-180' : '']" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                    </svg>
                                </div>

                                <!-- Departamentos de Negocios -->
                                <div v-if="expandedAreas.business" class="px-4 pb-4 space-y-2">
                                    <div v-for="dept in getBusinessDepartments()" :key="dept.name" class="ml-4">
                                        <!-- Header do Departamento -->
                                        <div @click="toggleDepartment(dept.name)"
                                             class="flex items-center justify-between py-2 px-3 rounded-lg cursor-pointer hover:bg-brand-purple/10 transition-all border-l-2 border-brand-purple/50">
                                            <div class="flex items-center gap-2">
                                                <svg :class="['w-4 h-4 text-brand-purple transition-transform', expandedDepts[dept.name] ? 'rotate-90' : '']" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                                                </svg>
                                                <span class="font-medium text-white">{{ dept.name }}</span>
                                                <span class="text-xs text-gray-500">({{ dept.agents?.length || 0 }})</span>
                                            </div>
                                            <div class="flex items-center gap-1">
                                                <span class="w-2 h-2 rounded-full bg-brand-green" :title="dept.working + ' ativos'"></span>
                                                <span class="text-xs text-gray-500">{{ dept.working }}/{{ dept.agents?.length || 0 }}</span>
                                            </div>
                                        </div>

                                        <!-- Agentes do Departamento -->
                                        <div v-if="expandedDepts[dept.name]" class="ml-6 mt-2 space-y-1">
                                            <div v-for="agent in sortAgentsByLevel(dept.agents)" :key="agent.id"
                                                 @click="selectAgentProfile(agent.id)"
                                                 :class="['flex items-center gap-3 p-2 rounded-lg cursor-pointer transition-all',
                                                         selectedAgentId === agent.id ? 'bg-brand-purple/30 border border-brand-purple' : 'hover:bg-brand-purple/10 border border-transparent']">
                                                <div class="w-8 h-8 rounded-full bg-brand-purple/30 flex items-center justify-center text-xs font-bold text-brand-purple">
                                                    {{ getInitials(agent.name) }}
                                                </div>
                                                <div class="flex-1 min-w-0">
                                                    <div class="flex items-center gap-2">
                                                        <span class="text-sm font-medium text-white truncate">{{ agent.name }}</span>
                                                        <span v-if="agent.level <= 6" class="w-2 h-2 rounded-full bg-brand-yellow" title="Decisor"></span>
                                                    </div>
                                                    <p class="text-xs text-gray-500 truncate">{{ agent.title }}</p>
                                                </div>
                                                <span class="text-xs text-brand-purple/70">N{{ agent.level_num }}</span>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>

                            <!-- AREA: TECNOLOGIA -->
                            <div class="bg-gradient-to-r from-brand-blue-sky/20 to-transparent rounded-xl border border-brand-blue-sky/30">
                                <div @click="expandedAreas.technology = !expandedAreas.technology"
                                     class="flex items-center justify-between p-4 cursor-pointer hover:bg-brand-blue-sky/10 transition-all rounded-t-xl">
                                    <div class="flex items-center gap-3">
                                        <div class="w-10 h-10 rounded-lg bg-brand-blue-sky flex items-center justify-center">
                                            <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"/>
                                            </svg>
                                        </div>
                                        <div>
                                            <h3 class="text-lg font-bold text-white">Area de Tecnologia</h3>
                                            <p class="text-sm text-gray-400">{{ getTechDepartments().length }} departamentos | {{ countAgentsByArea('technology') }} agentes</p>
                                        </div>
                                    </div>
                                    <svg :class="['w-5 h-5 text-brand-blue-sky transition-transform', expandedAreas.technology ? 'rotate-180' : '']" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                                    </svg>
                                </div>

                                <!-- Departamentos de Tecnologia -->
                                <div v-if="expandedAreas.technology" class="px-4 pb-4 space-y-2">
                                    <div v-for="dept in getTechDepartments()" :key="dept.name" class="ml-4">
                                        <!-- Header do Departamento -->
                                        <div @click="toggleDepartment(dept.name)"
                                             class="flex items-center justify-between py-2 px-3 rounded-lg cursor-pointer hover:bg-brand-blue-sky/10 transition-all border-l-2 border-brand-blue-sky/50">
                                            <div class="flex items-center gap-2">
                                                <svg :class="['w-4 h-4 text-brand-blue-sky transition-transform', expandedDepts[dept.name] ? 'rotate-90' : '']" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                                                </svg>
                                                <span class="font-medium text-white">{{ dept.name }}</span>
                                                <span class="text-xs text-gray-500">({{ dept.agents?.length || 0 }})</span>
                                            </div>
                                            <div class="flex items-center gap-1">
                                                <span class="w-2 h-2 rounded-full bg-brand-green" :title="dept.working + ' ativos'"></span>
                                                <span class="text-xs text-gray-500">{{ dept.working }}/{{ dept.agents?.length || 0 }}</span>
                                            </div>
                                        </div>

                                        <!-- Agentes do Departamento -->
                                        <div v-if="expandedDepts[dept.name]" class="ml-6 mt-2 space-y-1">
                                            <div v-for="agent in sortAgentsByLevel(dept.agents)" :key="agent.id"
                                                 @click="selectAgentProfile(agent.id)"
                                                 :class="['flex items-center gap-3 p-2 rounded-lg cursor-pointer transition-all',
                                                         selectedAgentId === agent.id ? 'bg-brand-blue-sky/30 border border-brand-blue-sky' : 'hover:bg-brand-blue-sky/10 border border-transparent']">
                                                <div class="w-8 h-8 rounded-full bg-brand-blue-sky/30 flex items-center justify-center text-xs font-bold text-brand-blue-sky">
                                                    {{ getInitials(agent.name) }}
                                                </div>
                                                <div class="flex-1 min-w-0">
                                                    <div class="flex items-center gap-2">
                                                        <span class="text-sm font-medium text-white truncate">{{ agent.name }}</span>
                                                        <span v-if="agent.level_num <= 6" class="w-2 h-2 rounded-full bg-brand-yellow" title="Decisor"></span>
                                                    </div>
                                                    <p class="text-xs text-gray-500 truncate">{{ agent.title }}</p>
                                                </div>
                                                <span class="text-xs text-brand-blue-sky/70">N{{ agent.level_num }}</span>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- PERFIL ESTILO LINKEDIN (lado direito) -->
                        <div v-if="selectedAgentProfile" class="w-[420px] bg-brand-blue-light/5 rounded-xl border border-brand-blue-light/20 overflow-auto" style="max-height: calc(100vh - 220px);">
                            <!-- Banner/Cover -->
                            <div :class="['h-24 rounded-t-xl relative',
                                         selectedAgentProfile.area === 'business'
                                         ? 'bg-gradient-to-r from-brand-purple via-brand-purple/70 to-brand-red-dark'
                                         : 'bg-gradient-to-r from-brand-blue-sky via-brand-blue-light to-brand-blue']">
                                <!-- Badge de area -->
                                <div class="absolute top-3 right-3 px-2 py-1 rounded-full text-xs font-medium bg-white/20 text-white backdrop-blur-sm">
                                    {{ selectedAgentProfile.area === 'business' ? 'Negocios' : 'Tecnologia' }}
                                </div>
                            </div>

                            <!-- Avatar e Info Principal -->
                            <div class="px-5 pb-4 -mt-12 relative">
                                <div class="flex items-end gap-4">
                                    <div :class="['w-24 h-24 rounded-xl flex items-center justify-center text-2xl font-bold border-4 border-brand-blue shadow-lg',
                                                 selectedAgentProfile.area === 'business' ? 'bg-brand-purple text-white' : 'bg-brand-blue-sky text-white']">
                                        {{ getInitials(selectedAgentProfile.name) }}
                                    </div>
                                    <div class="pb-1">
                                        <div class="flex items-center gap-2">
                                            <h3 class="text-xl font-bold text-white">{{ selectedAgentProfile.name }}</h3>
                                            <span v-if="selectedAgentProfile.decision_maker?.is_decision_maker"
                                                  class="px-2 py-0.5 rounded-full text-xs bg-brand-yellow/20 text-brand-yellow border border-brand-yellow/30">
                                                Decisor
                                            </span>
                                        </div>
                                        <p class="text-sm text-gray-300">{{ selectedAgentProfile.title }}</p>
                                    </div>
                                </div>

                                <!-- Localizacao e Nivel -->
                                <div class="flex items-center gap-4 mt-3 text-sm text-gray-400">
                                    <span class="flex items-center gap-1">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4"/>
                                        </svg>
                                        {{ selectedAgentProfile.department }}
                                    </span>
                                    <span class="flex items-center gap-1">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4M7.835 4.697a3.42 3.42 0 001.946-.806 3.42 3.42 0 014.438 0 3.42 3.42 0 001.946.806 3.42 3.42 0 013.138 3.138 3.42 3.42 0 00.806 1.946 3.42 3.42 0 010 4.438 3.42 3.42 0 00-.806 1.946 3.42 3.42 0 01-3.138 3.138 3.42 3.42 0 00-1.946.806 3.42 3.42 0 01-4.438 0 3.42 3.42 0 00-1.946-.806 3.42 3.42 0 01-3.138-3.138 3.42 3.42 0 00-.806-1.946 3.42 3.42 0 010-4.438 3.42 3.42 0 00.806-1.946 3.42 3.42 0 013.138-3.138z"/>
                                        </svg>
                                        Nivel {{ selectedAgentProfile.level }} - {{ getLevelName(selectedAgentProfile.level) }}
                                    </span>
                                </div>

                                <!-- Score de Confiabilidade (estilo LinkedIn) -->
                                <div class="mt-4 p-3 bg-brand-blue-light/10 rounded-lg border border-brand-blue-light/20">
                                    <div class="flex items-center justify-between">
                                        <span class="text-sm text-gray-300">Indice de Confiabilidade</span>
                                        <span class="text-2xl font-bold text-brand-green">{{ selectedAgentProfile.metrics?.reliability_score || 0 }}%</span>
                                    </div>
                                    <div class="w-full bg-brand-blue-light/30 rounded-full h-2 mt-2">
                                        <div class="bg-gradient-to-r from-brand-green to-brand-yellow h-2 rounded-full transition-all" :style="{width: (selectedAgentProfile.metrics?.reliability_score || 0) + '%'}"></div>
                                    </div>
                                    <p class="text-xs text-gray-500 mt-1">Baseado em {{ selectedAgentProfile.total_experiences || 0 }} experiencias registradas</p>
                                </div>
                            </div>

                            <!-- Secao: Sobre -->
                            <div class="px-5 py-4 border-t border-brand-blue-light/20">
                                <h4 class="text-sm font-bold text-white mb-2 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                    </svg>
                                    Sobre
                                </h4>
                                <p class="text-sm text-gray-400 leading-relaxed">{{ selectedAgentProfile.bio }}</p>
                                <p v-if="selectedAgentProfile.specialization" class="text-xs text-brand-orange mt-2">
                                    Especializacao: {{ selectedAgentProfile.specialization }}
                                </p>
                            </div>

                            <!-- Secao: Estatisticas -->
                            <div class="px-5 py-4 border-t border-brand-blue-light/20">
                                <h4 class="text-sm font-bold text-white mb-3 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/>
                                    </svg>
                                    Estatisticas
                                </h4>
                                <div class="grid grid-cols-4 gap-2">
                                    <div class="text-center p-2 bg-brand-blue-light/10 rounded-lg">
                                        <p class="text-lg font-bold text-brand-blue-sky">{{ selectedAgentProfile.metrics?.total_projects || 0 }}</p>
                                        <p class="text-xs text-gray-500">Projetos</p>
                                    </div>
                                    <div class="text-center p-2 bg-brand-blue-light/10 rounded-lg">
                                        <p class="text-lg font-bold text-brand-green">{{ selectedAgentProfile.metrics?.total_tasks_completed || 0 }}</p>
                                        <p class="text-xs text-gray-500">Tarefas</p>
                                    </div>
                                    <div class="text-center p-2 bg-brand-blue-light/10 rounded-lg">
                                        <p class="text-lg font-bold text-brand-purple">{{ Math.round(selectedAgentProfile.metrics?.total_hours_worked || 0) }}</p>
                                        <p class="text-xs text-gray-500">Horas</p>
                                    </div>
                                    <div class="text-center p-2 bg-brand-blue-light/10 rounded-lg">
                                        <p class="text-lg font-bold text-brand-yellow">{{ selectedAgentProfile.achievements_count || 0 }}</p>
                                        <p class="text-xs text-gray-500">Badges</p>
                                    </div>
                                </div>
                            </div>

                            <!-- Secao: Competencias -->
                            <div class="px-5 py-4 border-t border-brand-blue-light/20">
                                <h4 class="text-sm font-bold text-white mb-3 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"/>
                                    </svg>
                                    Competencias ({{ selectedAgentProfile.skills_count || 0 }})
                                </h4>
                                <div class="space-y-3">
                                    <div v-for="skill in selectedAgentProfile.top_skills" :key="skill.skill_id" class="group">
                                        <div class="flex items-center justify-between mb-1">
                                            <span class="text-sm text-gray-300 group-hover:text-white transition-colors">{{ skill.name }}</span>
                                            <span :class="['text-xs px-2 py-0.5 rounded-full',
                                                          skill.level_num >= 4 ? 'bg-brand-green/20 text-brand-green' :
                                                          skill.level_num >= 3 ? 'bg-brand-yellow/20 text-brand-yellow' :
                                                          'bg-brand-blue-sky/20 text-brand-blue-sky']">
                                                {{ skill.level }}
                                            </span>
                                        </div>
                                        <div class="w-full bg-brand-blue-light/20 rounded-full h-2">
                                            <div :class="['h-2 rounded-full transition-all',
                                                         skill.level_num >= 4 ? 'bg-gradient-to-r from-brand-green to-brand-yellow' :
                                                         skill.level_num >= 3 ? 'bg-brand-yellow' : 'bg-brand-blue-sky']"
                                                 :style="{width: (skill.level_num * 20) + '%'}"></div>
                                        </div>
                                        <div class="flex justify-between text-xs text-gray-600 mt-0.5">
                                            <span>{{ skill.experience_points || 0 }} XP</span>
                                            <span v-if="skill.progress_to_next?.next">Prox: {{ skill.progress_to_next.next }} XP</span>
                                        </div>
                                    </div>
                                </div>
                            </div>

                            <!-- Secao: Conquistas -->
                            <div v-if="selectedAgentProfile.achievements?.length > 0" class="px-5 py-4 border-t border-brand-blue-light/20">
                                <h4 class="text-sm font-bold text-white mb-3 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z"/>
                                    </svg>
                                    Conquistas
                                </h4>
                                <div class="flex flex-wrap gap-2">
                                    <div v-for="achievement in selectedAgentProfile.achievements" :key="achievement.id"
                                         class="flex items-center gap-2 px-3 py-1.5 bg-brand-yellow/10 rounded-full border border-brand-yellow/30"
                                         :title="achievement.description">
                                        <span class="text-lg">{{ achievement.icon }}</span>
                                        <span class="text-xs text-brand-yellow">{{ achievement.name }}</span>
                                    </div>
                                </div>
                            </div>

                            <!-- Secao: Configuracao de Decisor -->
                            <div v-if="selectedAgentProfile.decision_maker?.is_decision_maker" class="px-5 py-4 border-t border-brand-blue-light/20 bg-brand-yellow/5">
                                <h4 class="text-sm font-bold text-white mb-3 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-yellow" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                    </svg>
                                    Configuracao de Decisor
                                </h4>
                                <div class="flex items-center justify-between p-3 bg-brand-blue-light/10 rounded-lg">
                                    <div>
                                        <p class="text-sm text-white">Timeout de Aprovacao</p>
                                        <p class="text-xs text-gray-500">Tempo ate autonomia dos subordinados</p>
                                    </div>
                                    <div class="flex items-center gap-2">
                                        <input type="number" v-model="timeoutInput" step="0.5" min="0.5" max="24"
                                               class="w-16 bg-brand-blue-light/30 border border-brand-blue-light/50 rounded px-2 py-1 text-sm text-white text-center">
                                        <span class="text-xs text-gray-400">horas</span>
                                        <button @click="updateAgentTimeout"
                                                class="px-3 py-1 bg-brand-yellow text-brand-blue rounded text-xs font-bold hover:bg-brand-orange transition-colors">
                                            Salvar
                                        </button>
                                    </div>
                                </div>
                                <div class="grid grid-cols-2 gap-2 mt-3 text-center">
                                    <div class="p-2 bg-brand-blue-light/10 rounded">
                                        <p class="text-lg font-bold text-brand-yellow">{{ selectedAgentProfile.decision_maker?.decisions_made || 0 }}</p>
                                        <p class="text-xs text-gray-500">Decisoes Tomadas</p>
                                    </div>
                                    <div class="p-2 bg-brand-blue-light/10 rounded">
                                        <p class="text-lg font-bold text-brand-orange">{{ selectedAgentProfile.decision_maker?.decisions_delegated || 0 }}</p>
                                        <p class="text-xs text-gray-500">Delegadas</p>
                                    </div>
                                </div>
                            </div>

                            <!-- Secao: Experiencia (Timeline) -->
                            <div class="px-5 py-4 border-t border-brand-blue-light/20">
                                <h4 class="text-sm font-bold text-white mb-3 flex items-center gap-2">
                                    <svg class="w-4 h-4 text-brand-orange" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 13.255A23.931 23.931 0 0112 15c-3.183 0-6.22-.62-9-1.745M16 6V4a2 2 0 00-2-2h-4a2 2 0 00-2 2v2m4 6h.01M5 20h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"/>
                                    </svg>
                                    Experiencia Recente
                                </h4>
                                <div class="space-y-3 max-h-64 overflow-auto pr-2">
                                    <div v-for="exp in selectedAgentProfile.recent_experiences" :key="exp.experience_id"
                                         class="relative pl-6 pb-3 border-l-2 border-brand-blue-light/30 last:border-l-0 last:pb-0">
                                        <div class="absolute left-0 top-0 w-3 h-3 rounded-full -translate-x-[7px]"
                                             :class="exp.outcome === 'success' ? 'bg-brand-green' : 'bg-brand-yellow'"></div>
                                        <div class="bg-brand-blue-light/10 rounded-lg p-3">
                                            <div class="flex items-start justify-between gap-2">
                                                <h5 class="text-sm font-medium text-white">{{ exp.title }}</h5>
                                                <span :class="['text-xs px-2 py-0.5 rounded-full shrink-0',
                                                              exp.outcome === 'success' ? 'bg-brand-green/20 text-brand-green' : 'bg-brand-yellow/20 text-brand-yellow']">
                                                    {{ exp.outcome === 'success' ? 'Sucesso' : 'Parcial' }}
                                                </span>
                                            </div>
                                            <p class="text-xs text-gray-500 mt-1">{{ exp.type }} | {{ exp.duration_hours }}h | Complexidade {{ exp.complexity }}/10</p>
                                            <div v-if="exp.skills_used?.length" class="flex flex-wrap gap-1 mt-2">
                                                <span v-for="skill in exp.skills_used.slice(0,3)" :key="skill"
                                                      class="text-xs px-1.5 py-0.5 bg-brand-blue-light/20 rounded text-gray-400">
                                                    {{ skill }}
                                                </span>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Placeholder quando nenhum agente selecionado -->
                        <div v-else class="w-[420px] bg-brand-blue-light/5 rounded-xl border border-brand-blue-light/20 flex items-center justify-center" style="min-height: 500px;">
                            <div class="text-center text-gray-500 p-8">
                                <div class="w-20 h-20 mx-auto mb-4 rounded-full bg-brand-blue-light/10 flex items-center justify-center">
                                    <svg class="w-10 h-10 opacity-50" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"/>
                                    </svg>
                                </div>
                                <p class="font-medium text-gray-400">Selecione um Agente</p>
                                <p class="text-sm mt-2">Expanda uma area e departamento<br>para ver os agentes disponiveis</p>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- SKILLS VIEW -->
                <div v-if="currentView === 'skills'">
                    <h2 class="text-xl font-bold mb-6 text-white">Skills Disponiveis</h2>
                    <div class="grid md:grid-cols-2 lg:grid-cols-3 gap-4">
                        <div v-for="skill in skills" :key="skill.skill_id"
                             class="bg-brand-blue-light/20 rounded-xl p-4 border border-brand-blue-light/30 hover:border-brand-purple/50 transition-all">
                            <div class="flex justify-between items-start mb-2">
                                <h3 class="font-semibold text-white">{{ skill.name }}</h3>
                                <span class="px-2 py-0.5 rounded-full text-xs bg-brand-purple/30 text-brand-purple">{{ skill.skill_type }}</span>
                            </div>
                            <p class="text-gray-400 text-sm">{{ skill.description }}</p>
                            <div class="mt-3 flex items-center gap-2 text-xs text-gray-500">
                                <span class="px-2 py-0.5 bg-brand-blue-light/30 rounded">{{ skill.category }}</span>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- PROJECTS VIEW -->
                <div v-if="currentView === 'projects'">
                    <div class="flex justify-between items-center mb-6">
                        <h2 class="text-xl font-bold text-white">Projetos</h2>
                        <button @click="showNewProject = true"
                                class="px-4 py-2 bg-brand-orange hover:bg-brand-red rounded-lg font-semibold text-white transition-all flex items-center gap-2">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path></svg>
                            Novo Projeto
                        </button>
                    </div>

                    <div class="grid gap-4">
                        <div v-for="project in projects" :key="project.project_id"
                             class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30 hover:border-brand-orange/50 transition-all">
                            <div class="flex justify-between items-start">
                                <div>
                                    <div class="flex items-center gap-3">
                                        <span class="text-brand-yellow font-mono font-semibold">{{ project.project_id }}</span>
                                        <h3 class="font-bold text-lg text-white">{{ project.name }}</h3>
                                    </div>
                                    <p class="text-gray-400 text-sm mt-2">{{ project.description }}</p>
                                </div>
                                <span :class="['px-4 py-1.5 rounded-full text-sm font-semibold', getProjectStatusClass(project.status)]">
                                    {{ project.status }}
                                </span>
                            </div>
                            <div class="flex gap-6 mt-4 text-sm text-gray-400">
                                <span class="flex items-center gap-1">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z"></path></svg>
                                    {{ project.project_type }}
                                </span>
                                <span class="flex items-center gap-1">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
                                    {{ project.progress }}%
                                </span>
                            </div>
                            <!-- Progress bar -->
                            <div class="mt-3">
                                <div class="w-full bg-brand-blue rounded-full h-2">
                                    <div class="bg-gradient-to-r from-brand-orange to-brand-yellow h-2 rounded-full" :style="{ width: project.progress + '%' }"></div>
                                </div>
                            </div>
                        </div>
                        <div v-if="!projects.length" class="text-center py-12 text-gray-500">
                            Nenhum projeto criado ainda
                        </div>
                    </div>
                </div>

                <!-- LOGS VIEW -->
                <div v-if="currentView === 'logs'">
                    <h2 class="text-xl font-bold mb-6 text-white">Logs de Atividade</h2>
                    <div class="bg-brand-blue-light/20 rounded-xl border border-brand-blue-light/30 overflow-hidden">
                        <div v-for="log in logs" :key="log.id"
                             class="px-4 py-3 border-b border-brand-blue-light/20 hover:bg-brand-blue-light/30 transition-all">
                            <div class="flex items-start gap-3">
                                <span :class="['px-2 py-0.5 rounded text-xs font-medium shrink-0', getLogClass(log.level)]">
                                    {{ log.level }}
                                </span>
                                <div class="flex-1 min-w-0">
                                    <div class="flex items-center gap-2 text-sm">
                                        <span class="text-brand-orange font-semibold">{{ log.source }}</span>
                                        <span v-if="log.agent_id" class="text-brand-blue-sky">Agent {{ log.agent_id }}</span>
                                        <span v-if="log.project_id" class="text-brand-yellow">{{ log.project_id }}</span>
                                    </div>
                                    <p class="text-gray-300 mt-1">{{ log.message }}</p>
                                </div>
                                <span class="text-gray-500 text-xs shrink-0">{{ formatTime(log.timestamp) }}</span>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- HIERARCHY VIEW -->
                <div v-if="currentView === 'hierarchy'">
                    <div class="flex justify-between items-center mb-6">
                        <h2 class="text-xl font-bold text-white">Organograma Corporativo</h2>
                        <div class="flex gap-2">
                            <button @click="selectedArea = 'all'"
                                    :class="['px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                            selectedArea === 'all' ? 'bg-brand-orange text-white' : 'bg-brand-blue-light/30 text-gray-300 hover:bg-brand-blue-light/50']">
                                Todos
                            </button>
                            <button @click="selectedArea = 'business'"
                                    :class="['px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                            selectedArea === 'business' ? 'bg-brand-purple text-white' : 'bg-brand-blue-light/30 text-gray-300 hover:bg-brand-blue-light/50']">
                                Negocios
                            </button>
                            <button @click="selectedArea = 'technology'"
                                    :class="['px-4 py-2 rounded-lg text-sm font-medium transition-all',
                                            selectedArea === 'technology' ? 'bg-brand-blue-sky text-white' : 'bg-brand-blue-light/30 text-gray-300 hover:bg-brand-blue-light/50']">
                                Tecnologia
                            </button>
                        </div>
                    </div>

                    <!-- Statistics Cards -->
                    <div class="grid grid-cols-4 gap-4 mb-6">
                        <div class="bg-gradient-to-br from-brand-purple/80 to-brand-purple rounded-xl p-4 border border-brand-purple/30">
                            <div class="text-3xl font-bold text-white">{{ hierarchyStats?.by_area?.business || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Agentes Negocios</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-blue-sky/80 to-brand-blue-sky rounded-xl p-4 border border-brand-blue-sky/30">
                            <div class="text-3xl font-bold text-white">{{ hierarchyStats?.by_area?.technology || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Agentes TI</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-green/80 to-brand-green rounded-xl p-4 border border-brand-green/30">
                            <div class="text-3xl font-bold text-white">{{ hierarchyStats?.by_status?.WORKING || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Trabalhando</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-gray/80 to-brand-gray rounded-xl p-4 border border-brand-gray/30">
                            <div class="text-3xl font-bold text-white">{{ hierarchyStats?.by_status?.STANDBY || hierarchyStats?.total_agents || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Standby</div>
                        </div>
                    </div>

                    <!-- Departments Grid -->
                    <div class="grid grid-cols-2 lg:grid-cols-3 gap-4">
                        <div v-for="dept in filteredDepartments" :key="dept.name"
                             class="bg-brand-blue-light/20 rounded-xl p-4 border hover:border-brand-orange/50 transition-all cursor-pointer"
                             :class="dept.area === 'business' ? 'border-brand-purple/30' : 'border-brand-blue-sky/30'">
                            <div class="flex justify-between items-start mb-3">
                                <div>
                                    <h3 class="font-semibold text-white">{{ dept.name }}</h3>
                                    <span :class="['text-xs px-2 py-0.5 rounded-full',
                                                  dept.area === 'business' ? 'bg-brand-purple/30 text-brand-purple' : 'bg-brand-blue-sky/30 text-brand-blue-sky']">
                                        {{ dept.area === 'business' ? 'Negocios' : 'Tecnologia' }}
                                    </span>
                                </div>
                                <div class="text-right">
                                    <div class="text-lg font-bold text-brand-yellow">{{ dept.agents.length }}</div>
                                    <div class="text-xs text-gray-500">agentes</div>
                                </div>
                            </div>

                            <!-- Status Indicators -->
                            <div class="flex gap-2 mb-3">
                                <div class="flex items-center gap-1 text-xs">
                                    <span class="w-2 h-2 rounded-full bg-brand-green"></span>
                                    <span class="text-gray-400">{{ dept.working }} trabalhando</span>
                                </div>
                                <div class="flex items-center gap-1 text-xs">
                                    <span class="w-2 h-2 rounded-full bg-brand-gray"></span>
                                    <span class="text-gray-400">{{ dept.standby }} standby</span>
                                </div>
                            </div>

                            <!-- Agent List (collapsed) -->
                            <div class="space-y-1 max-h-40 overflow-y-auto scrollbar-thin">
                                <div v-for="agent in dept.agents.slice(0, 5)" :key="agent.id"
                                     class="flex items-center justify-between text-xs p-1.5 rounded bg-brand-blue/50 hover:bg-brand-blue-light/30">
                                    <div class="flex items-center gap-2">
                                        <span :class="['w-1.5 h-1.5 rounded-full', agent.status === 'WORKING' ? 'bg-brand-green' : 'bg-brand-gray']"></span>
                                        <span class="text-gray-300 truncate" style="max-width: 120px;">{{ agent.name }}</span>
                                    </div>
                                    <span class="text-gray-500 text-xs">{{ agent.level.split(' ')[0] }}</span>
                                </div>
                                <div v-if="dept.agents.length > 5" class="text-xs text-brand-orange text-center py-1">
                                    +{{ dept.agents.length - 5 }} mais agentes
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Hierarchy Levels -->
                    <div class="mt-6">
                        <h3 class="text-lg font-semibold mb-4 text-brand-orange">Niveis Hierarquicos</h3>
                        <div class="grid grid-cols-5 gap-2">
                            <div v-for="(count, level) in hierarchyStats?.by_level" :key="level"
                                 class="bg-brand-blue-light/20 rounded-lg p-3 text-center border border-brand-blue-light/30">
                                <div class="text-xl font-bold text-brand-yellow">{{ count }}</div>
                                <div class="text-xs text-gray-400 truncate">{{ level }}</div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- PRODUCTIVITY VIEW -->
                <div v-if="currentView === 'productivity'">
                    <div class="flex justify-between items-center mb-6">
                        <h2 class="text-xl font-bold text-white">Metricas de Produtividade</h2>
                        <button @click="fetchProductivity" class="px-4 py-2 bg-brand-orange hover:bg-brand-red rounded-lg text-sm font-medium transition-all">
                            Atualizar
                        </button>
                    </div>

                    <!-- Summary Cards -->
                    <div class="grid grid-cols-5 gap-4 mb-6">
                        <div class="bg-gradient-to-br from-brand-blue-light to-brand-blue rounded-xl p-4 border border-brand-blue-light/30">
                            <div class="text-3xl font-bold text-white">{{ productivityData?.summary?.total || 0 }}</div>
                            <div class="text-sm text-gray-300 mt-1">Total Agentes</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-green/80 to-brand-green rounded-xl p-4 border border-brand-green/30">
                            <div class="text-3xl font-bold text-white">{{ productivityData?.summary?.working || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Trabalhando</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-gray/80 to-brand-gray rounded-xl p-4 border border-brand-gray/30">
                            <div class="text-3xl font-bold text-white">{{ productivityData?.summary?.standby || 0 }}</div>
                            <div class="text-sm text-gray-100 mt-1">Standby</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-orange/80 to-brand-orange rounded-xl p-4 border border-brand-orange/30">
                            <div class="text-3xl font-bold text-white">{{ productivityData?.summary?.avg_productivity || 0 }}%</div>
                            <div class="text-sm text-gray-100 mt-1">Media Produtividade</div>
                        </div>
                        <div class="bg-gradient-to-br from-brand-yellow/80 to-brand-yellow rounded-xl p-4 border border-brand-yellow/30">
                            <div class="text-3xl font-bold text-brand-blue">{{ productivityData?.top_performers?.length || 0 }}</div>
                            <div class="text-sm text-brand-blue/80 mt-1">Top Performers</div>
                        </div>
                    </div>

                    <div class="grid grid-cols-3 gap-6">
                        <!-- Top Performers -->
                        <div>
                            <h3 class="text-lg font-semibold mb-4 text-brand-green flex items-center gap-2">
                                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z"></path></svg>
                                Top Performers
                            </h3>
                            <div class="space-y-2">
                                <div v-for="(agent, idx) in productivityData?.top_performers" :key="agent.agent_id"
                                     class="bg-brand-green/10 rounded-lg p-3 border border-brand-green/30 hover:border-brand-green/60 transition-all">
                                    <div class="flex items-center gap-3">
                                        <span class="w-8 h-8 rounded-full bg-brand-green/30 flex items-center justify-center text-brand-green font-bold">
                                            {{ idx + 1 }}
                                        </span>
                                        <div class="flex-1">
                                            <div class="font-medium text-white">{{ agent.name }}</div>
                                            <div class="text-xs text-gray-400">{{ agent.domain }} | {{ agent.agent_id }}</div>
                                        </div>
                                        <div class="text-right">
                                            <div class="text-lg font-bold text-brand-green">{{ agent.productivity_score }}%</div>
                                            <div class="text-xs text-gray-500">{{ agent.tasks_completed }} tarefas</div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- All Agents Ranking -->
                        <div class="col-span-2">
                            <h3 class="text-lg font-semibold mb-4 text-brand-orange">Ranking de Produtividade</h3>
                            <div class="bg-brand-blue-light/20 rounded-xl border border-brand-blue-light/30 overflow-hidden">
                                <table class="w-full">
                                    <thead class="bg-brand-blue-light/30">
                                        <tr class="text-left text-xs text-gray-400">
                                            <th class="px-4 py-3">#</th>
                                            <th class="px-4 py-3">Agente</th>
                                            <th class="px-4 py-3">Dominio</th>
                                            <th class="px-4 py-3">Status</th>
                                            <th class="px-4 py-3">Tarefas</th>
                                            <th class="px-4 py-3">Atividades</th>
                                            <th class="px-4 py-3">Score</th>
                                        </tr>
                                    </thead>
                                    <tbody class="divide-y divide-brand-blue-light/20">
                                        <tr v-for="(agent, idx) in productivityData?.agents?.slice(0, 20)" :key="agent.agent_id"
                                            class="hover:bg-brand-blue-light/20 transition-all">
                                            <td class="px-4 py-2 text-gray-500 text-sm">{{ idx + 1 }}</td>
                                            <td class="px-4 py-2">
                                                <div class="font-medium text-white text-sm">{{ agent.name }}</div>
                                                <div class="text-xs text-gray-500">{{ agent.agent_id }}</div>
                                            </td>
                                            <td class="px-4 py-2 text-sm text-gray-300">{{ agent.domain }}</td>
                                            <td class="px-4 py-2">
                                                <span :class="['px-2 py-0.5 rounded-full text-xs font-medium',
                                                              agent.status === 'WORKING' || agent.status === 'EXECUTING' ? 'bg-brand-green/30 text-brand-green' : 'bg-brand-gray/30 text-gray-400']">
                                                    {{ agent.status }}
                                                </span>
                                            </td>
                                            <td class="px-4 py-2 text-sm">
                                                <span class="text-brand-green">{{ agent.tasks_completed }}</span>
                                                <span class="text-gray-500">/</span>
                                                <span class="text-brand-red">{{ agent.tasks_failed }}</span>
                                            </td>
                                            <td class="px-4 py-2 text-sm text-brand-blue-sky">{{ agent.total_activities }}</td>
                                            <td class="px-4 py-2">
                                                <div class="flex items-center gap-2">
                                                    <div class="w-16 bg-brand-blue rounded-full h-2">
                                                        <div class="h-2 rounded-full transition-all"
                                                             :class="agent.productivity_score >= 70 ? 'bg-brand-green' : agent.productivity_score >= 40 ? 'bg-brand-yellow' : 'bg-brand-red'"
                                                             :style="{ width: agent.productivity_score + '%' }"></div>
                                                    </div>
                                                    <span class="text-sm font-medium" :class="agent.productivity_score >= 70 ? 'text-brand-green' : agent.productivity_score >= 40 ? 'text-brand-yellow' : 'text-brand-red'">
                                                        {{ agent.productivity_score }}%
                                                    </span>
                                                </div>
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- APPROVALS VIEW (Cockpit de Aprovacoes) -->
                <div v-if="currentView === 'approvals'">
                    <div class="flex justify-between items-center mb-6">
                        <div>
                            <h2 class="text-xl font-bold text-white">Cockpit de Aprovacoes</h2>
                            <p class="text-sm text-gray-400 mt-1">{{ pendingApprovals.length }} itens pendentes de aprovacao</p>
                        </div>
                        <button @click="fetchPendingApprovals" class="px-4 py-2 bg-brand-orange hover:bg-brand-red rounded-lg text-sm font-medium transition-all flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path></svg>
                            Atualizar
                        </button>
                    </div>

                    <!-- Lista de Aprovacoes Pendentes -->
                    <div v-if="pendingApprovals.length === 0" class="bg-brand-blue-light/20 rounded-xl p-8 text-center border border-brand-blue-light/30">
                        <svg class="w-16 h-16 mx-auto text-brand-green opacity-50 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                        </svg>
                        <h3 class="text-lg font-semibold text-white mb-2">Nenhuma aprovacao pendente</h3>
                        <p class="text-gray-400">Todas as historias foram revisadas e aprovadas.</p>
                    </div>

                    <div v-else class="space-y-4">
                        <div v-for="item in pendingApprovals" :key="item.story_id"
                             class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30 hover:border-brand-orange/50 transition-all">
                            <div class="flex justify-between items-start mb-4">
                                <div class="flex-1">
                                    <div class="flex items-center gap-3 mb-2">
                                        <span class="text-brand-yellow font-mono font-bold">{{ item.story_id }}</span>
                                        <span class="px-2 py-0.5 bg-brand-purple/30 text-brand-purple rounded text-xs font-medium">
                                            {{ item.source || 'media_extraction' }}
                                        </span>
                                        <span class="px-2 py-0.5 bg-brand-orange/30 text-brand-orange rounded text-xs font-medium">
                                            {{ item.story_points || 3 }} pts
                                        </span>
                                    </div>
                                    <h3 class="text-lg font-semibold text-white mb-2">{{ item.title }}</h3>

                                    <!-- User Story Format -->
                                    <div class="bg-brand-blue/50 rounded-lg p-3 mb-3 text-sm">
                                        <p class="text-brand-blue-sky">
                                            <strong>Como</strong> {{ item.as_a || 'usuario' }},<br>
                                            <strong>eu quero</strong> {{ item.i_want || item.title }}<br>
                                            <strong>para que</strong> {{ item.so_that || 'possa realizar minhas tarefas' }}
                                        </p>
                                    </div>

                                    <p v-if="item.description" class="text-gray-400 text-sm mb-3">{{ item.description }}</p>

                                    <!-- Criterios de Aceitacao -->
                                    <div v-if="item.acceptance_criteria && item.acceptance_criteria.length" class="mb-3">
                                        <h4 class="text-sm font-semibold text-brand-orange mb-2">Criterios de Aceitacao:</h4>
                                        <ul class="space-y-1">
                                            <li v-for="(criteria, idx) in item.acceptance_criteria" :key="idx"
                                                class="flex items-start gap-2 text-sm text-gray-300">
                                                <svg class="w-4 h-4 text-brand-green shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                                                </svg>
                                                {{ typeof criteria === 'object' ? criteria.text : criteria }}
                                            </li>
                                        </ul>
                                    </div>

                                    <!-- Tags -->
                                    <div v-if="item.tags && item.tags.length" class="flex flex-wrap gap-1">
                                        <span v-for="tag in item.tags" :key="tag"
                                              class="px-2 py-0.5 bg-brand-gray/30 text-gray-300 rounded text-xs">
                                            {{ tag }}
                                        </span>
                                    </div>
                                </div>

                                <!-- Actions -->
                                <div class="flex flex-col gap-2 ml-4">
                                    <button @click="approveItem(item)"
                                            class="px-4 py-2 bg-brand-green hover:bg-brand-green/80 rounded-lg text-white text-sm font-medium transition-all flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                                        </svg>
                                        Aprovar
                                    </button>
                                    <button @click="showRejectModal(item)"
                                            class="px-4 py-2 bg-brand-red hover:bg-brand-red-dark rounded-lg text-white text-sm font-medium transition-all flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
                                        </svg>
                                        Rejeitar
                                    </button>
                                    <button @click="editStoryFromApproval(item)"
                                            class="px-4 py-2 bg-brand-blue-light hover:bg-brand-blue-sky rounded-lg text-white text-sm font-medium transition-all flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"></path>
                                        </svg>
                                        Editar
                                    </button>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- PM REPORT VIEW -->
                <div v-if="currentView === 'pmReport'">
                    <div class="flex justify-between items-center mb-6">
                        <div>
                            <h2 class="text-xl font-bold text-white">Relatorio do Project Manager</h2>
                            <p class="text-sm text-gray-400 mt-1">Visao executiva do projeto {{ selectedProjectName }}</p>
                        </div>
                        <div class="flex items-center gap-3">
                            <select v-model="selectedPMProject" @change="fetchPMReport"
                                    class="bg-brand-blue-light/30 border border-brand-blue-light/50 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-brand-orange">
                                <option v-for="p in orchestratorProjects" :key="p.project_id" :value="p.project_id">
                                    {{ p.name || p.project_id }}
                                </option>
                            </select>
                            <button @click="fetchPMReport" class="px-4 py-2 bg-brand-orange hover:bg-brand-red rounded-lg text-sm font-medium transition-all">
                                Atualizar
                            </button>
                        </div>
                    </div>

                    <div v-if="pmReport">
                        <!-- KPIs do Projeto -->
                        <div class="grid grid-cols-4 gap-4 mb-6">
                            <div class="bg-gradient-to-br from-brand-blue-light to-brand-blue rounded-xl p-4 border border-brand-blue-light/30">
                                <div class="text-3xl font-bold text-white">{{ pmReport.progress?.percentage || 0 }}%</div>
                                <div class="text-sm text-gray-300 mt-1">Progresso Geral</div>
                                <div class="w-full bg-brand-blue rounded-full h-2 mt-2">
                                    <div class="bg-brand-green h-2 rounded-full transition-all" :style="{ width: (pmReport.progress?.percentage || 0) + '%' }"></div>
                                </div>
                            </div>
                            <div class="bg-gradient-to-br from-brand-green/80 to-brand-green rounded-xl p-4 border border-brand-green/30">
                                <div class="text-3xl font-bold text-white">{{ pmReport.progress?.stories_completed || 0 }}/{{ pmReport.progress?.stories_total || 0 }}</div>
                                <div class="text-sm text-gray-100 mt-1">Stories Concluidas</div>
                            </div>
                            <div class="bg-gradient-to-br from-brand-yellow/80 to-brand-yellow rounded-xl p-4 border border-brand-yellow/30">
                                <div class="text-3xl font-bold text-brand-blue">{{ pmReport.progress?.points_completed || 0 }}/{{ pmReport.progress?.points_total || 0 }}</div>
                                <div class="text-sm text-brand-blue/80 mt-1">Pontos Entregues</div>
                            </div>
                            <div class="bg-gradient-to-br from-brand-purple/80 to-brand-purple rounded-xl p-4 border border-brand-purple/30">
                                <div class="text-3xl font-bold text-white">{{ pmReport.timeline?.velocity_points_per_day || 0 }}</div>
                                <div class="text-sm text-gray-100 mt-1">Velocidade (pts/dia)</div>
                            </div>
                        </div>

                        <div class="grid grid-cols-3 gap-6">
                            <!-- Timeline e Estimativas -->
                            <div class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30">
                                <h3 class="text-lg font-semibold mb-4 text-brand-orange flex items-center gap-2">
                                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>
                                    Timeline
                                </h3>
                                <div class="space-y-4">
                                    <div class="flex justify-between items-center">
                                        <span class="text-gray-400">Inicio</span>
                                        <span class="text-white font-medium">{{ formatDate(pmReport.timeline?.started_at) }}</span>
                                    </div>
                                    <div class="flex justify-between items-center">
                                        <span class="text-gray-400">Previsao Conclusao</span>
                                        <span class="text-brand-green font-medium">{{ formatDate(pmReport.timeline?.estimated_completion) }}</span>
                                    </div>
                                    <div class="flex justify-between items-center">
                                        <span class="text-gray-400">Status</span>
                                        <span :class="['px-3 py-1 rounded-full text-xs font-semibold', getProjectStatusClass(pmReport.status)]">
                                            {{ pmReport.status }}
                                        </span>
                                    </div>
                                </div>
                            </div>

                            <!-- Equipe -->
                            <div class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30">
                                <h3 class="text-lg font-semibold mb-4 text-brand-blue-sky flex items-center gap-2">
                                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0z"></path></svg>
                                    Equipe
                                </h3>
                                <div class="grid grid-cols-3 gap-3 text-center">
                                    <div class="bg-brand-blue/50 rounded-lg p-3">
                                        <div class="text-2xl font-bold text-white">{{ pmReport.team?.agents_total || 0 }}</div>
                                        <div class="text-xs text-gray-400">Total</div>
                                    </div>
                                    <div class="bg-brand-green/20 rounded-lg p-3">
                                        <div class="text-2xl font-bold text-brand-green">{{ pmReport.team?.agents_working || 0 }}</div>
                                        <div class="text-xs text-gray-400">Ativos</div>
                                    </div>
                                    <div class="bg-brand-gray/20 rounded-lg p-3">
                                        <div class="text-2xl font-bold text-gray-400">{{ pmReport.team?.agents_idle || 0 }}</div>
                                        <div class="text-xs text-gray-400">Idle</div>
                                    </div>
                                </div>
                            </div>

                            <!-- Backlog Summary -->
                            <div class="bg-brand-blue-light/20 rounded-xl p-5 border border-brand-blue-light/30">
                                <h3 class="text-lg font-semibold mb-4 text-brand-yellow flex items-center gap-2">
                                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path></svg>
                                    Backlog
                                </h3>
                                <div class="space-y-2">
                                    <div class="flex justify-between items-center text-sm">
                                        <span class="text-gray-400">Total Stories</span>
                                        <span class="text-white font-semibold">{{ pmReport.backlog?.total_stories || 0 }}</span>
                                    </div>
                                    <div class="flex justify-between items-center text-sm">
                                        <span class="text-gray-400">Total Pontos</span>
                                        <span class="text-white font-semibold">{{ pmReport.backlog?.total_points || 0 }}</span>
                                    </div>
                                    <div class="flex justify-between items-center text-sm">
                                        <span class="text-gray-400">Pendente Revisao</span>
                                        <span class="text-brand-orange font-semibold">{{ pmReport.backlog?.by_status?.pending_review || 0 }}</span>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Top Performers -->
                        <div v-if="pmReport.top_performers && pmReport.top_performers.length" class="mt-6">
                            <h3 class="text-lg font-semibold mb-4 text-brand-green">Top Performers do Projeto</h3>
                            <div class="grid grid-cols-5 gap-3">
                                <div v-for="(agent, idx) in pmReport.top_performers" :key="agent.agent_id"
                                     class="bg-brand-green/10 rounded-lg p-4 border border-brand-green/30 text-center">
                                    <div class="w-10 h-10 mx-auto rounded-full bg-brand-green/30 flex items-center justify-center text-brand-green font-bold mb-2">
                                        {{ idx + 1 }}
                                    </div>
                                    <div class="font-medium text-white text-sm">{{ agent.agent_name }}</div>
                                    <div class="text-lg font-bold text-brand-green mt-1">{{ agent.points_delivered }} pts</div>
                                    <div class="text-xs text-gray-500">{{ agent.stories_completed }} stories</div>
                                </div>
                            </div>
                        </div>

                        <!-- Atividade Recente -->
                        <div v-if="pmReport.recent_activity && pmReport.recent_activity.length" class="mt-6">
                            <h3 class="text-lg font-semibold mb-4 text-brand-orange">Atividade Recente</h3>
                            <div class="bg-brand-blue-light/20 rounded-xl border border-brand-blue-light/30 overflow-hidden">
                                <div v-for="activity in pmReport.recent_activity" :key="activity.timestamp"
                                     class="px-4 py-3 border-b border-brand-blue-light/20 last:border-b-0 hover:bg-brand-blue-light/30">
                                    <div class="flex items-center justify-between">
                                        <span class="text-gray-300">{{ activity.message }}</span>
                                        <span class="text-xs text-gray-500">{{ formatTime(activity.timestamp) }}</span>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div v-else class="bg-brand-blue-light/20 rounded-xl p-8 text-center border border-brand-blue-light/30">
                        <p class="text-gray-400">Selecione um projeto para ver o relatorio do PM</p>
                    </div>
                </div>

                <!-- CREATE STORY VIEW -->
                <div v-if="currentView === 'createStory'">
                    <div class="flex justify-between items-center mb-6">
                        <h2 class="text-xl font-bold text-white">Criar Historia Manual</h2>
                    </div>

                    <div class="bg-brand-blue-light/20 rounded-xl p-6 border border-brand-blue-light/30 max-w-4xl">
                        <form @submit.prevent="submitNewStory">
                            <div class="grid grid-cols-2 gap-6">
                                <!-- Coluna Esquerda -->
                                <div class="space-y-4">
                                    <div>
                                        <label class="block text-sm text-gray-400 mb-1">Projeto *</label>
                                        <select v-model="newStory.project_id" required
                                                class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                                            <option value="">Selecione...</option>
                                            <option v-for="p in orchestratorProjects" :key="p.project_id" :value="p.project_id">
                                                {{ p.name || p.project_id }}
                                            </option>
                                        </select>
                                    </div>

                                    <div>
                                        <label class="block text-sm text-gray-400 mb-1">Titulo *</label>
                                        <input v-model="newStory.title" type="text" required
                                               class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white"
                                               placeholder="Ex: Implementar tela de login">
                                    </div>

                                    <div class="bg-brand-blue/50 rounded-lg p-4">
                                        <h4 class="text-sm font-semibold text-brand-orange mb-3">User Story Format</h4>
                                        <div class="space-y-3">
                                            <div>
                                                <label class="block text-xs text-gray-400 mb-1">Como (persona)</label>
                                                <input v-model="newStory.as_a" type="text"
                                                       class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white text-sm"
                                                       placeholder="usuario do sistema">
                                            </div>
                                            <div>
                                                <label class="block text-xs text-gray-400 mb-1">Eu quero</label>
                                                <input v-model="newStory.i_want" type="text"
                                                       class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white text-sm"
                                                       placeholder="fazer login na plataforma">
                                            </div>
                                            <div>
                                                <label class="block text-xs text-gray-400 mb-1">Para que</label>
                                                <input v-model="newStory.so_that" type="text"
                                                       class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white text-sm"
                                                       placeholder="possa acessar minhas informacoes">
                                            </div>
                                        </div>
                                    </div>

                                    <div>
                                        <label class="block text-sm text-gray-400 mb-1">Descricao</label>
                                        <textarea v-model="newStory.description" rows="3"
                                                  class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white"
                                                  placeholder="Detalhes adicionais sobre a historia..."></textarea>
                                    </div>
                                </div>

                                <!-- Coluna Direita -->
                                <div class="space-y-4">
                                    <div class="grid grid-cols-3 gap-3">
                                        <div>
                                            <label class="block text-sm text-gray-400 mb-1">Story Points</label>
                                            <select v-model="newStory.story_points"
                                                    class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                                                <option value="1">1</option>
                                                <option value="2">2</option>
                                                <option value="3">3</option>
                                                <option value="5">5</option>
                                                <option value="8">8</option>
                                                <option value="13">13</option>
                                                <option value="21">21</option>
                                            </select>
                                        </div>
                                        <div>
                                            <label class="block text-sm text-gray-400 mb-1">Prioridade</label>
                                            <select v-model="newStory.priority"
                                                    class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                                                <option value="1">1 - Critica</option>
                                                <option value="2">2 - Alta</option>
                                                <option value="3">3 - Media</option>
                                                <option value="4">4 - Baixa</option>
                                                <option value="5">5 - Muito Baixa</option>
                                            </select>
                                        </div>
                                        <div>
                                            <label class="block text-sm text-gray-400 mb-1">Sprint</label>
                                            <input v-model="newStory.sprint" type="number" min="1"
                                                   class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                                        </div>
                                    </div>

                                    <!-- Criterios de Aceitacao -->
                                    <div>
                                        <div class="flex justify-between items-center mb-2">
                                            <label class="text-sm text-gray-400">Criterios de Aceitacao</label>
                                            <button type="button" @click="addCriteria"
                                                    class="text-brand-orange hover:text-brand-yellow text-sm flex items-center gap-1">
                                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"></path>
                                                </svg>
                                                Adicionar
                                            </button>
                                        </div>
                                        <div class="space-y-2 max-h-48 overflow-y-auto">
                                            <div v-for="(criteria, idx) in newStory.acceptance_criteria" :key="idx"
                                                 class="flex items-center gap-2">
                                                <input v-model="newStory.acceptance_criteria[idx]" type="text"
                                                       class="flex-1 px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white text-sm"
                                                       placeholder="Ex: O sistema deve validar email">
                                                <button type="button" @click="removeCriteria(idx)"
                                                        class="p-2 text-brand-red hover:bg-brand-red/20 rounded-lg">
                                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
                                                    </svg>
                                                </button>
                                            </div>
                                        </div>
                                    </div>

                                    <!-- Tags -->
                                    <div>
                                        <label class="block text-sm text-gray-400 mb-1">Tags (separadas por virgula)</label>
                                        <input v-model="newStory.tags_input" type="text"
                                               class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white"
                                               placeholder="login, autenticacao, seguranca">
                                    </div>
                                </div>
                            </div>

                            <div class="flex justify-end gap-3 mt-6 pt-4 border-t border-brand-blue-light/30">
                                <button type="button" @click="resetNewStory"
                                        class="px-4 py-2 bg-brand-gray hover:bg-brand-gray/70 rounded-lg text-white transition-all">
                                    Limpar
                                </button>
                                <button type="submit"
                                        class="px-6 py-2 bg-brand-orange hover:bg-brand-red rounded-lg text-white font-semibold transition-all flex items-center gap-2">
                                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                                    </svg>
                                    Criar Historia
                                </button>
                            </div>
                        </form>
                    </div>
                </div>

            </main>
        </div>

        <!-- Rejection Modal -->
        <div v-if="showRejectModalFlag" class="fixed inset-0 bg-black/70 flex items-center justify-center z-50 p-4">
            <div class="bg-brand-blue rounded-xl p-6 w-full max-w-md border border-brand-blue-light/30">
                <h3 class="text-xl font-bold mb-4 text-white">Rejeitar Historia</h3>
                <p class="text-gray-400 mb-4">Informe o motivo da rejeicao para {{ rejectingItem?.story_id }}:</p>
                <textarea v-model="rejectionReason" rows="4"
                          class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white mb-4"
                          placeholder="Descreva o motivo da rejeicao..."></textarea>
                <div class="flex justify-end gap-3">
                    <button @click="showRejectModalFlag = false; rejectingItem = null; rejectionReason = ''"
                            class="px-4 py-2 bg-brand-gray hover:bg-brand-gray/70 rounded-lg text-white transition-all">
                        Cancelar
                    </button>
                    <button @click="confirmReject"
                            class="px-4 py-2 bg-brand-red hover:bg-brand-red-dark rounded-lg text-white font-semibold transition-all">
                        Confirmar Rejeicao
                    </button>
                </div>
            </div>
        </div>

        <!-- Story Detail Modal -->
        <div v-if="selectedStory" class="fixed inset-0 bg-black/70 flex items-center justify-center z-50 p-4">
            <div class="bg-brand-blue rounded-xl w-full max-w-2xl max-h-[90vh] overflow-y-auto border border-brand-blue-light/30">
                <div class="sticky top-0 bg-brand-blue p-4 border-b border-brand-blue-light/30 flex justify-between items-center">
                    <div>
                        <span class="text-brand-yellow font-mono font-bold text-lg">{{ selectedStory.story_id }}</span>
                        <span :class="['ml-3 px-3 py-1 rounded-full text-xs font-semibold', getStatusClass(selectedStory.status)]">
                            {{ selectedStory.status }}
                        </span>
                    </div>
                    <button @click="selectedStory = null" class="text-gray-400 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
                    </button>
                </div>
                <div class="p-4">
                    <h3 class="font-bold text-xl text-white mb-2">{{ selectedStory.title }}</h3>
                    <p class="text-gray-400 mb-4">{{ selectedStory.description }}</p>

                    <div class="grid grid-cols-3 gap-4 mb-4">
                        <div class="bg-brand-blue-light/30 rounded-lg p-3 text-center">
                            <div class="text-2xl font-bold text-brand-yellow">{{ selectedStory.sprint }}</div>
                            <div class="text-xs text-gray-400">Sprint</div>
                        </div>
                        <div class="bg-brand-blue-light/30 rounded-lg p-3 text-center">
                            <div class="text-2xl font-bold text-brand-orange">{{ selectedStory.points }}</div>
                            <div class="text-xs text-gray-400">Pontos</div>
                        </div>
                        <div class="bg-brand-blue-light/30 rounded-lg p-3 text-center">
                            <div class="text-2xl font-bold text-brand-green">{{ selectedStory.priority }}</div>
                            <div class="text-xs text-gray-400">Prioridade</div>
                        </div>
                    </div>

                    <!-- Agentes que trabalharam -->
                    <div v-if="selectedStory.agents && selectedStory.agents.length" class="mb-4">
                        <h4 class="text-sm font-semibold text-brand-orange mb-2">Agentes Envolvidos</h4>
                        <div class="flex flex-wrap gap-2">
                            <span v-for="a in selectedStory.agents" :key="a"
                                  class="px-3 py-1 bg-brand-blue-sky/20 text-brand-blue-sky rounded-full text-sm font-medium">
                                Agente {{ a }}
                            </span>
                        </div>
                    </div>

                    <!-- Criterios de Aceitacao -->
                    <div v-if="selectedStory.criteria && selectedStory.criteria.length" class="mb-4">
                        <h4 class="text-sm font-semibold text-brand-orange mb-2">Criterios de Aceitacao</h4>
                        <ul class="space-y-1">
                            <li v-for="(c, i) in selectedStory.criteria" :key="i" class="flex items-start gap-2 text-sm text-gray-300">
                                <svg class="w-4 h-4 text-brand-green shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path></svg>
                                {{ typeof c === 'object' ? c.text : c }}
                            </li>
                        </ul>
                    </div>
                </div>
            </div>
        </div>

        <!-- Agent Detail Modal -->
        <div v-if="selectedAgent" class="fixed inset-0 bg-black/70 flex items-center justify-center z-50 p-4">
            <div class="bg-brand-blue rounded-xl w-full max-w-lg border border-brand-blue-light/30">
                <div class="p-4 border-b border-brand-blue-light/30 flex justify-between items-center">
                    <div class="flex items-center gap-3">
                        <span class="text-brand-yellow font-mono font-bold text-xl">{{ selectedAgent.id }}</span>
                        <div :class="['w-3 h-3 rounded-full', getAgentStatusColor(selectedAgent.status)]"></div>
                    </div>
                    <button @click="selectedAgent = null" class="text-gray-400 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
                    </button>
                </div>
                <div class="p-4">
                    <h3 class="font-bold text-xl text-white">{{ selectedAgent.name }}</h3>
                    <p class="text-brand-orange mb-4">{{ selectedAgent.role }}</p>

                    <div class="grid grid-cols-2 gap-4 mb-4">
                        <div class="bg-brand-blue-light/30 rounded-lg p-3">
                            <div class="text-sm text-gray-400">Status</div>
                            <span :class="['px-3 py-1 rounded-full text-sm font-semibold', getAgentStatusBadge(selectedAgent.status)]">
                                {{ selectedAgent.status }}
                            </span>
                        </div>
                        <div class="bg-brand-blue-light/30 rounded-lg p-3">
                            <div class="text-sm text-gray-400">Dominio</div>
                            <div class="text-white font-semibold">{{ selectedAgent.domain }}</div>
                        </div>
                    </div>

                    <div class="grid grid-cols-3 gap-3 text-center">
                        <div class="bg-brand-green/20 rounded-lg p-3">
                            <div class="text-2xl font-bold text-brand-green">{{ selectedAgent.metrics?.tasks_completed || 0 }}</div>
                            <div class="text-xs text-gray-400">Completadas</div>
                        </div>
                        <div class="bg-brand-red/20 rounded-lg p-3">
                            <div class="text-2xl font-bold text-brand-red">{{ selectedAgent.metrics?.tasks_failed || 0 }}</div>
                            <div class="text-xs text-gray-400">Falhas</div>
                        </div>
                        <div class="bg-brand-yellow/20 rounded-lg p-3">
                            <div class="text-2xl font-bold text-brand-yellow">{{ selectedAgent.priority }}</div>
                            <div class="text-xs text-gray-400">Prioridade</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- New Project Modal -->
        <div v-if="showNewProject" class="fixed inset-0 bg-black/70 flex items-center justify-center z-50 p-4">
            <div class="bg-brand-blue rounded-xl p-6 w-full max-w-md border border-brand-blue-light/30">
                <h3 class="text-xl font-bold mb-4 text-white">Novo Projeto</h3>
                <form @submit.prevent="createProject">
                    <div class="space-y-4">
                        <div>
                            <label class="block text-sm text-gray-400 mb-1">Nome</label>
                            <input v-model="newProject.name" type="text" required
                                   class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                        </div>
                        <div>
                            <label class="block text-sm text-gray-400 mb-1">Descricao</label>
                            <textarea v-model="newProject.description" rows="3"
                                      class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white"></textarea>
                        </div>
                        <div>
                            <label class="block text-sm text-gray-400 mb-1">Tipo</label>
                            <select v-model="newProject.project_type" required
                                    class="w-full px-3 py-2 bg-brand-blue-light/30 rounded-lg border border-brand-blue-light/50 focus:border-brand-orange outline-none text-white">
                                <option value="web-app">Aplicacao Web</option>
                                <option value="api-service">API Service</option>
                                <option value="data-analysis">Analise de Dados</option>
                                <option value="document">Documento</option>
                                <option value="automation">Automacao</option>
                                <option value="integration">Integracao</option>
                            </select>
                        </div>
                    </div>
                    <div class="flex justify-end gap-3 mt-6">
                        <button type="button" @click="showNewProject = false"
                                class="px-4 py-2 bg-brand-gray hover:bg-brand-gray/70 rounded-lg text-white transition-all">
                            Cancelar
                        </button>
                        <button type="submit" class="px-4 py-2 bg-brand-orange hover:bg-brand-red rounded-lg text-white font-semibold transition-all">
                            Criar Projeto
                        </button>
                    </div>
                </form>
            </div>
        </div>
    </div>

    <script>
    const { createApp, ref, onMounted, computed, watch } = Vue;

    createApp({
        setup() {
            const currentView = ref('overview');
            const status = ref(null);
            const projects = ref([]);
            const agents = ref([]);
            const skills = ref([]);
            const logs = ref([]);
            const stories = ref([]);
            const metrics = ref(null);
            const showNewProject = ref(false);
            const newProject = ref({ name: '', description: '', project_type: 'web-app' });
            const selectedProject = ref('all');
            const selectedSprint = ref('all');
            const selectedStory = ref(null);
            const selectedAgent = ref(null);
            const selectedDomain = ref('all');

            // Quick add story state
            const showQuickAddStory = ref(false);
            const quickStory = ref({
                title: '',
                description: '',
                sprint: 1,
                points: 3,
                priority: 3
            });
            const selectedArea = ref('all');
            const hierarchyStats = ref(null);
            const departments = ref([]);
            const productivityData = ref(null);
            const agentViewMode = ref('hierarchy');
            const selectedAgentArea = ref('all');

            // Novos states para sistema de perfis unificado
            const fullHierarchy = ref({ roots: [], nodes: {} });
            const selectedAgentId = ref(null);
            const selectedAgentProfile = ref(null);
            const timeoutInput = ref(1);

            // Estados para arvore organizacional
            const orgDepartments = ref([]);
            const expandedAreas = ref({ business: true, technology: true });
            const expandedDepts = ref({});

            // Toast notifications
            const toasts = ref([]);
            const isLoading = ref(false);

            // Estados para Cockpit de Aprovacoes e PM Report
            const pendingApprovals = ref([]);
            const pmReport = ref(null);
            const orchestratorProjects = ref([]);
            const selectedPMProject = ref(null);
            const showRejectModalFlag = ref(false);
            const rejectingItem = ref(null);
            const rejectionReason = ref('');

            // Estados para criacao de historia
            const newStory = ref({
                project_id: '',
                title: '',
                as_a: 'usuario',
                i_want: '',
                so_that: '',
                description: '',
                story_points: 3,
                priority: 3,
                sprint: 1,
                acceptance_criteria: [''],
                tags_input: ''
            });

            const showToast = (message, type = 'success', duration = 3000) => {
                const id = Date.now();
                toasts.value.push({ id, message, type });
                setTimeout(() => {
                    toasts.value = toasts.value.filter(t => t.id !== id);
                }, duration);
            };

            const removeToast = (id) => {
                toasts.value = toasts.value.filter(t => t.id !== id);
            };

            const currentTime = computed(() => new Date().toLocaleTimeString('pt-BR'));

            const selectedProjectName = computed(() => {
                if (selectedProject.value === 'all') return 'Todos';
                const p = projects.value.find(p => p.project_id === selectedProject.value);
                return p ? p.name : selectedProject.value;
            });

            const filteredStories = computed(() => {
                if (selectedProject.value === 'all') return stories.value;
                return stories.value.filter(s => s.project_id === selectedProject.value);
            });

            const sprintList = computed(() => {
                const sprints = [...new Set(filteredStories.value.map(s => s.sprint))];
                return sprints.sort((a, b) => a - b);
            });

            const kanbanStories = computed(() => {
                let filtered = filteredStories.value;
                if (selectedSprint.value !== 'all') {
                    filtered = filtered.filter(s => s.sprint === parseInt(selectedSprint.value));
                }
                return {
                    backlog: filtered.filter(s => s.status.toUpperCase() === 'BACKLOG'),
                    todo: filtered.filter(s => ['TO_DO', 'TODO', 'PENDING_REVIEW', 'APPROVED'].includes(s.status.toUpperCase())),
                    inProgress: filtered.filter(s => s.status.toUpperCase() === 'IN_PROGRESS'),
                    testing: filtered.filter(s => s.status.toUpperCase() === 'TESTING'),
                    blocked: filtered.filter(s => ['BLOCKED', 'REJECTED'].includes(s.status.toUpperCase())),
                    done: filtered.filter(s => ['DONE', 'COMPLETED'].includes(s.status.toUpperCase()))
                };
            });

            const sprintData = computed(() => {
                const data = {};
                filteredStories.value.forEach(s => {
                    if (!data[s.sprint]) {
                        data[s.sprint] = { number: s.sprint, total: 0, done: 0, inProgress: 0, todo: 0, backlog: 0, points: 0, stories: [], expanded: false };
                    }
                    data[s.sprint].total++;
                    data[s.sprint].points += s.points || 0;
                    data[s.sprint].stories.push(s);
                    const status = s.status.toUpperCase();
                    if (['DONE', 'COMPLETED'].includes(status)) data[s.sprint].done++;
                    else if (status === 'IN_PROGRESS') data[s.sprint].inProgress++;
                    else if (['TO_DO', 'TODO'].includes(status)) data[s.sprint].todo++;
                    else data[s.sprint].backlog++;
                });
                return Object.values(data).map(sp => ({
                    ...sp,
                    percentage: sp.total > 0 ? Math.round((sp.done / sp.total) * 100) : 0
                })).sort((a, b) => a.number - b.number);
            });

            const agentDomains = computed(() => {
                const domains = [...new Set(agents.value.map(a => a.domain))];
                return ['all', ...domains.filter(d => d)];
            });

            const filteredAgents = computed(() => {
                if (selectedDomain.value === 'all') return agents.value;
                return agents.value.filter(a => a.domain === selectedDomain.value);
            });

            const filteredDepartments = computed(() => {
                if (selectedArea.value === 'all') return departments.value;
                return departments.value.filter(d => d.area === selectedArea.value);
            });

            const filteredAgentsByArea = computed(() => {
                let filtered = agents.value;
                if (selectedAgentArea.value === 'business') {
                    filtered = filtered.filter(a => a.config?.area === 'business');
                } else if (selectedAgentArea.value === 'technology') {
                    filtered = filtered.filter(a => a.config?.area === 'technology');
                }
                if (selectedDomain.value !== 'all') {
                    filtered = filtered.filter(a => a.domain === selectedDomain.value);
                }
                return filtered.sort((a, b) => (a.config?.level || 99) - (b.config?.level || 99));
            });

            const getAgentsByLevel = (level) => {
                return agents.value.filter(a => a.config?.level === level)
                    .filter(a => {
                        if (selectedAgentArea.value === 'all') return true;
                        return a.config?.area === selectedAgentArea.value;
                    });
            };

            const formatBudget = (value) => {
                if (!value) return '0';
                if (value >= 1000000) return (value / 1000000).toFixed(0) + 'M';
                if (value >= 1000) return (value / 1000).toFixed(0) + 'K';
                return value.toString();
            };

            const fetchStatus = async () => {
                try {
                    const res = await fetch('/api/status');
                    status.value = await res.json();
                } catch (e) { console.error('Error fetching status:', e); }
            };

            const fetchProjects = async () => {
                try {
                    const res = await fetch('/api/projects');
                    const data = await res.json();
                    projects.value = data.projects;
                } catch (e) { console.error('Error fetching projects:', e); }
            };

            const fetchAgents = async () => {
                try {
                    const res = await fetch('/api/agents');
                    const data = await res.json();
                    agents.value = data.agents;
                } catch (e) { console.error('Error fetching agents:', e); }
            };

            const fetchSkills = async () => {
                try {
                    const res = await fetch('/api/skills');
                    const data = await res.json();
                    skills.value = data.skills;
                } catch (e) { console.error('Error fetching skills:', e); }
            };

            const fetchLogs = async () => {
                try {
                    let url = '/api/logs?limit=100';
                    if (selectedProject.value !== 'all') {
                        url += '&project_id=' + selectedProject.value;
                    }
                    const res = await fetch(url);
                    const data = await res.json();
                    logs.value = data.logs;
                } catch (e) { console.error('Error fetching logs:', e); }
            };

            const fetchStories = async () => {
                try {
                    let url = '/api/stories';
                    if (selectedProject.value !== 'all') {
                        url += '?project_id=' + selectedProject.value;
                    }
                    const res = await fetch(url);
                    const data = await res.json();
                    stories.value = data.stories;
                } catch (e) { console.error('Error fetching stories:', e); }
            };

            const createQuickStory = async () => {
                if (!quickStory.value.title.trim()) {
                    alert('Por favor, insira um titulo para a story');
                    return;
                }
                try {
                    const res = await fetch('/api/stories', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                            title: quickStory.value.title,
                            description: quickStory.value.description,
                            project_id: selectedProject.value,
                            sprint: quickStory.value.sprint,
                            points: quickStory.value.points,
                            priority: quickStory.value.priority
                        })
                    });
                    const data = await res.json();
                    if (data.success) {
                        showQuickAddStory.value = false;
                        quickStory.value = { title: '', description: '', sprint: 1, points: 3, priority: 3 };
                        showToast('Story criada com sucesso!', 'success');
                        await fetchStories();
                    } else {
                        alert('Erro ao criar story');
                    }
                } catch (e) {
                    console.error('Error creating story:', e);
                    alert('Erro ao criar story');
                }
            };

            // Ordem dos status para movimentacao no Kanban
            const statusOrder = ['BACKLOG', 'TO_DO', 'IN_PROGRESS', 'TESTING', 'DONE'];

            const moveStoryStatus = async (story, direction) => {
                const currentIndex = statusOrder.findIndex(s =>
                    s === story.status.toUpperCase() ||
                    (s === 'TO_DO' && ['TODO', 'PENDING_REVIEW', 'APPROVED'].includes(story.status.toUpperCase()))
                );

                let newIndex;
                if (direction === 'next') {
                    newIndex = Math.min(currentIndex + 1, statusOrder.length - 1);
                } else {
                    newIndex = Math.max(currentIndex - 1, 0);
                }

                const newStatus = statusOrder[newIndex];

                try {
                    const res = await fetch('/api/stories/' + story.story_id, {
                        method: 'PUT',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ status: newStatus })
                    });
                    if (res.ok) {
                        showToast('Status atualizado: ' + newStatus, 'success');
                        await fetchStories();
                    }
                } catch (e) {
                    console.error('Error updating story status:', e);
                    showToast('Erro ao atualizar status', 'error');
                }
            };

            const setStoryStatus = async (story, newStatus) => {
                try {
                    const res = await fetch('/api/stories/' + story.story_id, {
                        method: 'PUT',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ status: newStatus })
                    });
                    if (res.ok) {
                        showToast('Status atualizado: ' + newStatus, 'success');
                        await fetchStories();
                    }
                } catch (e) {
                    console.error('Error updating story status:', e);
                    showToast('Erro ao atualizar status', 'error');
                }
            };

            const fetchMetrics = async () => {
                try {
                    let url = '/api/metrics';
                    if (selectedProject.value !== 'all') {
                        url += '?project_id=' + selectedProject.value;
                    }
                    const res = await fetch(url);
                    metrics.value = await res.json();
                } catch (e) { console.error('Error fetching metrics:', e); }
            };

            const fetchHierarchy = async () => {
                try {
                    const res = await fetch('/api/hierarchy');
                    const data = await res.json();
                    hierarchyStats.value = data.statistics;
                } catch (e) { console.error('Error fetching hierarchy:', e); }
            };

            const fetchDepartments = async () => {
                try {
                    const res = await fetch('/api/hierarchy/departments');
                    const data = await res.json();
                    departments.value = data.departments || [];
                } catch (e) { console.error('Error fetching departments:', e); }
            };

            const fetchProductivity = async () => {
                try {
                    const res = await fetch('/api/productivity');
                    productivityData.value = await res.json();
                } catch (e) { console.error('Error fetching productivity:', e); }
            };

            // Funcoes para sistema de perfis unificado
            const fetchFullHierarchy = async () => {
                try {
                    const res = await fetch('/api/profiles/hierarchy');
                    const data = await res.json();
                    fullHierarchy.value = data;
                } catch (e) { console.error('Error fetching full hierarchy:', e); }
            };

            const getAgentsByLevelFromHierarchy = (level) => {
                if (!fullHierarchy.value?.nodes) return [];
                return Object.values(fullHierarchy.value.nodes)
                    .filter(agent => agent.level === level)
                    .sort((a, b) => a.name.localeCompare(b.name));
            };

            const getLevelName = (level) => {
                const names = {
                    1: 'CEO',
                    2: 'C-Level',
                    3: 'Vice-Presidentes',
                    4: 'Diretores',
                    5: 'Gerentes Seniores',
                    6: 'Gerentes',
                    7: 'Coordenadores',
                    8: 'Tech Leads / Especialistas Sr',
                    9: 'Analistas / Desenvolvedores',
                    10: 'Assistentes / Trainees'
                };
                return names[level] || `Nivel ${level}`;
            };

            const selectAgentProfile = async (agentId) => {
                selectedAgentId.value = agentId;
                try {
                    const res = await fetch(`/api/profiles/${agentId}`);
                    const data = await res.json();
                    selectedAgentProfile.value = data.profile || data;
                    if (selectedAgentProfile.value?.decision_maker?.timeout_hours) {
                        timeoutInput.value = selectedAgentProfile.value.decision_maker.timeout_hours;
                    }
                } catch (e) { console.error('Error fetching agent profile:', e); }
            };

            const getInitials = (name) => {
                if (!name) return '??';
                const parts = name.split(' ');
                if (parts.length >= 2) {
                    return (parts[0][0] + parts[parts.length - 1][0]).toUpperCase();
                }
                return name.substring(0, 2).toUpperCase();
            };

            const updateAgentTimeout = async () => {
                if (!selectedAgentId.value) return;
                try {
                    const res = await fetch(`/api/profiles/${selectedAgentId.value}/timeout`, {
                        method: 'PUT',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ timeout_hours: parseFloat(timeoutInput.value) })
                    });
                    if (res.ok) {
                        await selectAgentProfile(selectedAgentId.value);
                        showToast(`Timeout atualizado para ${timeoutInput.value}h`, 'success');
                    } else {
                        showToast('Erro ao atualizar timeout', 'error');
                    }
                } catch (e) {
                    console.error('Error updating timeout:', e);
                    showToast('Erro ao atualizar timeout', 'error');
                }
            };

            // Funcoes para arvore organizacional
            const fetchOrgDepartments = async () => {
                try {
                    const res = await fetch('/api/hierarchy/departments');
                    const data = await res.json();
                    orgDepartments.value = data.departments || [];
                } catch (e) { console.error('Error fetching org departments:', e); }
            };

            const getBusinessDepartments = () => {
                return orgDepartments.value.filter(d => d.area === 'business').sort((a, b) => a.name.localeCompare(b.name));
            };

            const getTechDepartments = () => {
                return orgDepartments.value.filter(d => d.area === 'technology').sort((a, b) => a.name.localeCompare(b.name));
            };

            const toggleDepartment = (deptName) => {
                expandedDepts.value[deptName] = !expandedDepts.value[deptName];
            };

            const countAgentsByArea = (area) => {
                return orgDepartments.value
                    .filter(d => d.area === area)
                    .reduce((sum, d) => sum + (d.agents?.length || 0), 0);
            };

            const sortAgentsByLevel = (agents) => {
                if (!agents) return [];
                return [...agents].sort((a, b) => (a.level_num || 10) - (b.level_num || 10));
            };

            // ============================================
            // FUNCOES PARA COCKPIT DE APROVACOES
            // ============================================

            const fetchOrchestratorProjects = async () => {
                try {
                    // Por enquanto usa a mesma API de projetos
                    const res = await fetch('/api/projects');
                    const data = await res.json();
                    orchestratorProjects.value = data.projects || [];
                    if (orchestratorProjects.value.length > 0 && !selectedPMProject.value) {
                        selectedPMProject.value = orchestratorProjects.value[0].project_id;
                    }
                } catch (e) { console.error('Error fetching orchestrator projects:', e); }
            };

            const fetchPendingApprovals = async () => {
                try {
                    const res = await fetch('/api/orchestrator/all-approvals');
                    if (res.ok) {
                        const data = await res.json();
                        let approvals = data.pending_approvals || [];
                        // Filtra pelo projeto selecionado se nao for "all"
                        if (selectedProject.value !== 'all') {
                            approvals = approvals.filter(a => a.project_id === selectedProject.value);
                        }
                        pendingApprovals.value = approvals;
                    } else {
                        // Fallback para projetos sem orchestrator
                        pendingApprovals.value = [];
                    }
                } catch (e) {
                    console.error('Error fetching pending approvals:', e);
                    pendingApprovals.value = [];
                }
            };

            const fetchPMReport = async () => {
                if (!selectedPMProject.value) return;
                try {
                    const res = await fetch(`/api/orchestrator/projects/${selectedPMProject.value}/pm-report`);
                    if (res.ok) {
                        pmReport.value = await res.json();
                    } else {
                        pmReport.value = null;
                    }
                } catch (e) {
                    console.error('Error fetching PM report:', e);
                    pmReport.value = null;
                }
            };

            const approveItem = async (item) => {
                try {
                    const res = await fetch(`/api/orchestrator/projects/${item.project_id}/stories/${item.story_id}/approve`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ approver_id: 'user', notes: 'Aprovado via dashboard' })
                    });
                    if (res.ok) {
                        showToast(`Historia ${item.story_id} aprovada com sucesso!`, 'success');
                        await fetchPendingApprovals();
                    } else {
                        showToast('Erro ao aprovar historia', 'error');
                    }
                } catch (e) {
                    console.error('Error approving item:', e);
                    showToast('Erro ao aprovar historia', 'error');
                }
            };

            const showRejectModal = (item) => {
                rejectingItem.value = item;
                rejectionReason.value = '';
                showRejectModalFlag.value = true;
            };

            const confirmReject = async () => {
                if (!rejectingItem.value || !rejectionReason.value.trim()) {
                    showToast('Informe o motivo da rejeicao', 'warning');
                    return;
                }
                try {
                    const res = await fetch(`/api/orchestrator/projects/${rejectingItem.value.project_id}/stories/${rejectingItem.value.story_id}/reject`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ rejector_id: 'user', reason: rejectionReason.value })
                    });
                    if (res.ok) {
                        showToast(`Historia ${rejectingItem.value.story_id} rejeitada`, 'info');
                        showRejectModalFlag.value = false;
                        rejectingItem.value = null;
                        rejectionReason.value = '';
                        await fetchPendingApprovals();
                    } else {
                        showToast('Erro ao rejeitar historia', 'error');
                    }
                } catch (e) {
                    console.error('Error rejecting item:', e);
                    showToast('Erro ao rejeitar historia', 'error');
                }
            };

            const editStoryFromApproval = (item) => {
                // Preenche o formulario de edicao com os dados da historia
                newStory.value = {
                    project_id: item.project_id || '',
                    title: item.title || '',
                    as_a: item.as_a || 'usuario',
                    i_want: item.i_want || '',
                    so_that: item.so_that || '',
                    description: item.description || '',
                    story_points: item.story_points || 3,
                    priority: item.priority?.value || 3,
                    sprint: item.sprint || 1,
                    acceptance_criteria: item.acceptance_criteria?.map(c => typeof c === 'object' ? c.text : c) || [''],
                    tags_input: item.tags?.join(', ') || ''
                };
                currentView.value = 'createStory';
            };

            // ============================================
            // FUNCOES PARA CRIACAO DE HISTORIA
            // ============================================

            const addCriteria = () => {
                newStory.value.acceptance_criteria.push('');
            };

            const removeCriteria = (idx) => {
                if (newStory.value.acceptance_criteria.length > 1) {
                    newStory.value.acceptance_criteria.splice(idx, 1);
                }
            };

            const resetNewStory = () => {
                newStory.value = {
                    project_id: '',
                    title: '',
                    as_a: 'usuario',
                    i_want: '',
                    so_that: '',
                    description: '',
                    story_points: 3,
                    priority: 3,
                    sprint: 1,
                    acceptance_criteria: [''],
                    tags_input: ''
                };
            };

            const submitNewStory = async () => {
                if (!newStory.value.project_id || !newStory.value.title) {
                    showToast('Preencha projeto e titulo', 'warning');
                    return;
                }

                const storyData = {
                    title: newStory.value.title,
                    as_a: newStory.value.as_a,
                    i_want: newStory.value.i_want,
                    so_that: newStory.value.so_that,
                    description: newStory.value.description,
                    story_points: parseInt(newStory.value.story_points),
                    priority: parseInt(newStory.value.priority),
                    sprint: parseInt(newStory.value.sprint),
                    acceptance_criteria: newStory.value.acceptance_criteria.filter(c => c.trim()),
                    tags: newStory.value.tags_input.split(',').map(t => t.trim()).filter(t => t),
                    created_by: 'user'
                };

                try {
                    const res = await fetch(`/api/orchestrator/projects/${newStory.value.project_id}/stories`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(storyData)
                    });
                    if (res.ok) {
                        showToast('Historia criada com sucesso!', 'success');
                        resetNewStory();
                        await fetchStories();
                    } else {
                        const error = await res.json();
                        showToast(error.detail || 'Erro ao criar historia', 'error');
                    }
                } catch (e) {
                    console.error('Error creating story:', e);
                    showToast('Erro ao criar historia', 'error');
                }
            };

            const formatDate = (dateStr) => {
                if (!dateStr) return '-';
                const date = new Date(dateStr);
                return date.toLocaleDateString('pt-BR');
            };

            const onProjectChange = () => {
                fetchStories();
                fetchLogs();
                fetchMetrics();
                fetchPendingApprovals();
            };

            const createProject = async () => {
                try {
                    const res = await fetch('/api/projects', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(newProject.value)
                    });
                    if (res.ok) {
                        showNewProject.value = false;
                        newProject.value = { name: '', description: '', project_type: 'web-app' };
                        fetchProjects();
                        fetchStatus();
                    }
                } catch (e) { console.error('Error creating project:', e); }
            };

            const showStoryDetail = (story) => { selectedStory.value = story; };
            const showAgentDetail = (agent) => { selectedAgent.value = agent; };

            const getLogClass = (level) => {
                const classes = {
                    'DEBUG': 'bg-brand-gray text-white',
                    'INFO': 'bg-brand-blue-sky text-white',
                    'WARNING': 'bg-brand-yellow text-brand-blue',
                    'ERROR': 'bg-brand-red text-white',
                    'CRITICAL': 'bg-brand-red-dark text-white'
                };
                return classes[level] || 'bg-brand-gray text-white';
            };

            const getStatusClass = (status) => {
                const classes = {
                    'DONE': 'bg-brand-green text-white',
                    'COMPLETED': 'bg-brand-green text-white',
                    'IN_PROGRESS': 'bg-brand-orange text-white',
                    'TO_DO': 'bg-brand-yellow text-brand-blue',
                    'TODO': 'bg-brand-yellow text-brand-blue',
                    'BACKLOG': 'bg-brand-gray text-white',
                    'BLOCKED': 'bg-brand-red text-white'
                };
                return classes[status?.toUpperCase()] || 'bg-brand-gray text-white';
            };

            const getProjectStatusClass = (status) => {
                const classes = {
                    'PLANNING': 'bg-brand-purple text-white',
                    'IN_PROGRESS': 'bg-brand-orange text-white',
                    'COMPLETED': 'bg-brand-green text-white',
                    'ON_HOLD': 'bg-brand-yellow text-brand-blue'
                };
                return classes[status?.toUpperCase()] || 'bg-brand-gray text-white';
            };

            const getAgentStatusColor = (status) => {
                const colors = {
                    'STANDBY': 'bg-brand-gray',
                    'EXECUTING': 'bg-brand-green',
                    'ERROR': 'bg-brand-red',
                    'PLANNING': 'bg-brand-purple'
                };
                return colors[status] || 'bg-brand-gray';
            };

            const getAgentStatusBadge = (status) => {
                const badges = {
                    'STANDBY': 'bg-brand-gray/30 text-gray-300',
                    'EXECUTING': 'bg-brand-green/30 text-brand-green',
                    'ERROR': 'bg-brand-red/30 text-brand-red',
                    'PLANNING': 'bg-brand-purple/30 text-brand-purple'
                };
                return badges[status] || 'bg-brand-gray/30 text-gray-300';
            };

            const formatTime = (timestamp) => {
                if (!timestamp) return '';
                const date = new Date(timestamp);
                return date.toLocaleString('pt-BR', { day: '2-digit', month: '2-digit', hour: '2-digit', minute: '2-digit' });
            };

            onMounted(() => {
                fetchStatus();
                fetchProjects();
                fetchAgents();
                fetchSkills();
                fetchLogs();
                fetchStories();
                fetchMetrics();
                fetchHierarchy();
                fetchDepartments();
                fetchProductivity();
                fetchFullHierarchy();
                fetchOrgDepartments();
                fetchOrchestratorProjects();
                fetchPendingApprovals();

                setInterval(fetchStatus, 10000);
                setInterval(fetchLogs, 30000);
                setInterval(fetchMetrics, 60000);
                setInterval(fetchProductivity, 120000);
                setInterval(fetchPendingApprovals, 60000);
            });

            return {
                currentView, status, projects, agents, skills, logs, stories, metrics,
                showNewProject, newProject, selectedProject, selectedSprint,
                selectedStory, selectedAgent, selectedDomain,
                // Quick add story e movimentacao
                showQuickAddStory, quickStory, createQuickStory,
                moveStoryStatus, setStoryStatus, statusOrder,
                selectedArea, hierarchyStats, departments, productivityData,
                agentViewMode, selectedAgentArea,
                // Novos para sistema de perfis unificado
                fullHierarchy, selectedAgentId, selectedAgentProfile, timeoutInput,
                // Estados para arvore organizacional
                orgDepartments, expandedAreas, expandedDepts,
                // Toast notifications
                toasts, isLoading, showToast, removeToast,
                // Estados para Cockpit de Aprovacoes e PM Report
                pendingApprovals, pmReport, orchestratorProjects, selectedPMProject,
                showRejectModalFlag, rejectingItem, rejectionReason, newStory,
                currentTime, selectedProjectName, filteredStories, sprintList,
                kanbanStories, sprintData, agentDomains, filteredAgents, filteredDepartments,
                filteredAgentsByArea, getAgentsByLevel, formatBudget,
                // Novas funcoes para perfis
                getAgentsByLevelFromHierarchy, getLevelName, selectAgentProfile, getInitials, updateAgentTimeout,
                // Funcoes para arvore organizacional
                getBusinessDepartments, getTechDepartments, toggleDepartment, countAgentsByArea, sortAgentsByLevel,
                // Funcoes para Cockpit de Aprovacoes
                fetchPendingApprovals, fetchPMReport, fetchOrchestratorProjects,
                approveItem, showRejectModal, confirmReject, editStoryFromApproval,
                // Funcoes para Criacao de Historia
                addCriteria, removeCriteria, resetNewStory, submitNewStory, formatDate,
                onProjectChange, createProject, showStoryDetail, showAgentDetail,
                fetchProductivity, fetchHierarchy, fetchDepartments,
                getLogClass, getStatusClass, getProjectStatusClass,
                getAgentStatusColor, getAgentStatusBadge, formatTime
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
"""


# =============================================================================
# MAIN
# =============================================================================

def run_dashboard():
    """Inicia o dashboard"""
    print("=" * 60)
    print(f"  {DASHBOARD_TITLE}")
    print(f"  Dashboard disponivel em: http://{DASHBOARD_HOST}:{DASHBOARD_PORT}")

    # Inicia o Agent Runner para execucao real de agentes
    if HAS_AGENT_RUNNER:
        try:
            runner = start_runner()
            print("  Agent Runner: ATIVO (execucao real de agentes)")
        except Exception as e:
            print(f"  Agent Runner: ERRO - {e}")
    else:
        print("  Agent Runner: NAO DISPONIVEL")

    # Inicia o Project Orchestrator para processamento autonomo de projetos
    if HAS_PROJECT_ORCHESTRATOR:
        try:
            orchestrator = start_orchestrator()
            print("  Project Orchestrator: ATIVO (agentes autonomos)")
        except Exception as e:
            print(f"  Project Orchestrator: ERRO - {e}")
    else:
        print("  Project Orchestrator: NAO DISPONIVEL")

    # Inicia o Story Executor para execucao automatica (legado)
    if HAS_STORY_EXECUTOR:
        try:
            executor = start_executor()
            print("  Story Executor: ATIVO (compatibilidade)")
        except Exception as e:
            print(f"  Story Executor: ERRO - {e}")

    print("=" * 60)
    uvicorn.run(app, host=DASHBOARD_HOST, port=DASHBOARD_PORT)


if __name__ == "__main__":
    run_dashboard()
