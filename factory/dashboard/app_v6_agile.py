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

from fastapi import FastAPI, HTTPException, UploadFile, File, Form, Query, WebSocket, WebSocketDisconnect
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


# Project Import Module (Issue #69)
from factory.dashboard.project_import import (
    analyze_project_structure,
    generate_stories_from_analysis,
    process_import as import_project_process,
    get_progress as get_import_progress_data,
    import_progress
)

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
# API ENDPOINTS - SECURITY SCAN (SAST)
# =============================================================================

@app.post("/api/story-tasks/{task_id}/security-scan")
def security_scan_for_task(task_id: str):
    """
    Analise de Seguranca Automatizada (SAST) para o codigo de uma task.
    Usa Claude AI para identificar vulnerabilidades de seguranca.
    """
    db = SessionLocal()
    try:
        repo = StoryTaskRepository(db)
        task = repo.get_by_id(task_id)
        if not task:
            raise HTTPException(404, "Task not found")

        if not task.code_output:
            raise HTTPException(400, "Task has no code output to analyze")

        code = task.code_output

        # Detectar linguagem
        language = "python"
        if "function " in code or "const " in code or "let " in code or "=>" in code:
            language = "javascript"
        elif "func " in code and "package " in code:
            language = "go"
        elif "public class " in code or "private void " in code:
            language = "java"
        elif "<?php" in code:
            language = "php"

        # Se Claude disponivel, usar IA para analise profunda
        if HAS_CLAUDE:
            try:
                claude = get_claude_client()
                if claude.is_available():
                    return perform_ai_security_scan(code, language, task, claude)
            except Exception as e:
                print(f"[SecurityScan] Erro ao usar Claude: {e}")

        # Fallback: analise basica com regex
        return perform_basic_security_scan(code, language, task)

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error performing security scan: {str(e)}")
    finally:
        db.close()


def perform_ai_security_scan(code: str, language: str, task, claude) -> dict:
    """Realiza analise de seguranca usando Claude AI"""

    system_prompt = """Voce e um especialista em seguranca de aplicacoes (AppSec) com profundo conhecimento em analise estatica de codigo (SAST).

Sua tarefa e analisar o codigo fornecido linha por linha e identificar vulnerabilidades de seguranca.

Tipos de vulnerabilidades a detectar:
1. SQL Injection - Queries SQL com concatenacao de strings ou inputs nao sanitizados
2. XSS (Cross-Site Scripting) - Output nao escapado em HTML, dados de usuario nao sanitizados
3. Command Injection - Execucao de comandos shell com input do usuario
4. Path Traversal - Acesso a arquivos com input nao validado (../ etc)
5. Hardcoded Secrets - Senhas, API keys, tokens hardcoded no codigo
6. Insecure Dependencies - Uso de funcoes/metodos inseguros ou deprecados

Para cada vulnerabilidade encontrada, forneca:
- Tipo da vulnerabilidade
- Severidade (Critical, High, Medium, Low)
- Linha(s) afetada(s)
- Descricao do problema
- Sugestao de correcao
- Referencia CWE quando aplicavel

Responda APENAS em JSON valido no formato:
{
    "vulnerabilities": [
        {
            "type": "SQL Injection",
            "severity": "Critical",
            "line": 15,
            "line_content": "codigo da linha afetada",
            "description": "Descricao detalhada do problema",
            "fix_suggestion": "Como corrigir",
            "cwe": "CWE-89"
        }
    ],
    "summary": {
        "total": 0,
        "critical": 0,
        "high": 0,
        "medium": 0,
        "low": 0
    },
    "recommendations": ["Recomendacao geral 1", "Recomendacao geral 2"],
    "secure_patterns_found": ["Padrao seguro identificado 1"]
}

Se nenhuma vulnerabilidade for encontrada, retorne vulnerabilities como lista vazia."""

    # Adicionar numeros de linha ao codigo
    lines = code.split('\n')
    numbered_code = '\n'.join(f"{i+1}: {line}" for i, line in enumerate(lines))

    message = f"""Analise o seguinte codigo {language} em busca de vulnerabilidades de seguranca:

```{language}
{numbered_code}
```

Contexto: Este codigo faz parte da task "{task.title}".

Realize uma analise SAST completa e retorne os resultados em JSON."""

    response = claude.chat(message, system_prompt, max_tokens=4096)

    if response.success:
        try:
            content = response.content.strip()
            if content.startswith("```"):
                lines = content.split("\n")
                content = "\n".join(lines[1:-1])
            if content.startswith("```json"):
                content = content[7:]
            if content.endswith("```"):
                content = content[:-3]

            result = json.loads(content)

            if "vulnerabilities" not in result:
                result["vulnerabilities"] = []
            if "summary" not in result:
                result["summary"] = calculate_summary(result["vulnerabilities"])

            result["scan_type"] = "ai"
            result["language"] = language
            result["scanned_at"] = datetime.utcnow().isoformat()
            result["task_id"] = task.task_id

            return result

        except json.JSONDecodeError:
            return {
                "vulnerabilities": [],
                "summary": {"total": 0, "critical": 0, "high": 0, "medium": 0, "low": 0},
                "scan_type": "ai",
                "language": language,
                "scanned_at": datetime.utcnow().isoformat(),
                "task_id": task.task_id,
                "recommendations": ["Analise manual recomendada"]
            }

    return perform_basic_security_scan(code, language, task)


def calculate_summary(vulnerabilities: list) -> dict:
    """Calcula resumo das vulnerabilidades"""
    summary = {"total": len(vulnerabilities), "critical": 0, "high": 0, "medium": 0, "low": 0}
    for v in vulnerabilities:
        severity = v.get("severity", "").lower()
        if severity in summary:
            summary[severity] += 1
    return summary


def perform_basic_security_scan(code: str, language: str, task) -> dict:
    """Analise basica de seguranca com regex (fallback)"""
    import re

    vulnerabilities = []
    lines = code.split('\n')

    patterns = {
        "SQL Injection": [
            (r'execute\s*\(\s*["'].*%s', "String formatting em query SQL"),
            (r'execute\s*\(\s*f["']', "f-string em query SQL"),
            (r'cursor\.execute\s*\([^,]+\+', "Concatenacao em cursor.execute"),
        ],
        "XSS": [
            (r'innerHTML\s*=', "Uso de innerHTML sem sanitizacao"),
            (r'document\.write\s*\(', "Uso de document.write"),
            (r'dangerouslySetInnerHTML', "React dangerouslySetInnerHTML"),
        ],
        "Command Injection": [
            (r'os\.system\s*\(', "Uso de os.system"),
            (r'subprocess\.call\s*\([^)]*shell\s*=\s*True', "subprocess com shell=True"),
            (r'eval\s*\(', "Uso de eval()"),
            (r'exec\s*\(', "Uso de exec()"),
        ],
        "Path Traversal": [
            (r'open\s*\([^)]*\+', "open() com concatenacao"),
            (r'\.\.\/|\.\.\\\\', "Path traversal pattern"),
        ],
        "Hardcoded Secrets": [
            (r'password\s*=\s*["'][^"']+["']', "Senha hardcoded"),
            (r'api_key\s*=\s*["'][^"']+["']', "API key hardcoded"),
            (r'secret\s*=\s*["'][^"']+["']', "Secret hardcoded"),
        ],
        "Insecure Dependencies": [
            (r'pickle\.load', "Deserializacao insegura com pickle"),
            (r'yaml\.load\s*\([^)]*\)', "yaml.load sem Loader seguro"),
        ],
    }

    for line_num, line in enumerate(lines, 1):
        for vuln_type, type_patterns in patterns.items():
            for pattern, description in type_patterns:
                if re.search(pattern, line, re.IGNORECASE):
                    severity = "Medium"
                    if vuln_type in ["SQL Injection", "Command Injection"]:
                        severity = "Critical"
                    elif vuln_type in ["XSS", "Hardcoded Secrets", "Path Traversal"]:
                        severity = "High"

                    vulnerabilities.append({
                        "type": vuln_type,
                        "severity": severity,
                        "line": line_num,
                        "line_content": line.strip()[:100],
                        "description": description,
                        "fix_suggestion": get_fix_suggestion(vuln_type),
                        "cwe": get_cwe(vuln_type)
                    })
                    break

    return {
        "vulnerabilities": vulnerabilities,
        "summary": calculate_summary(vulnerabilities),
        "scan_type": "basic",
        "language": language,
        "scanned_at": datetime.utcnow().isoformat(),
        "task_id": task.task_id,
        "recommendations": get_general_recommendations(vulnerabilities)
    }


def get_fix_suggestion(vuln_type: str) -> str:
    """Retorna sugestao de correcao para cada tipo de vulnerabilidade"""
    suggestions = {
        "SQL Injection": "Use prepared statements/parameterized queries",
        "XSS": "Sanitize e escape output. Use textContent ao inves de innerHTML",
        "Command Injection": "Evite os.system() e shell=True. Use subprocess com lista de argumentos",
        "Path Traversal": "Valide e normalize paths. Verifique se o path esta dentro do diretorio permitido",
        "Hardcoded Secrets": "Use variaveis de ambiente ou um secrets manager",
        "Insecure Dependencies": "Use alternativas seguras: yaml.safe_load(), etc"
    }
    return suggestions.get(vuln_type, "Revise o codigo e aplique boas praticas de seguranca")


def get_cwe(vuln_type: str) -> str:
    """Retorna CWE ID para cada tipo de vulnerabilidade"""
    cwes = {
        "SQL Injection": "CWE-89",
        "XSS": "CWE-79",
        "Command Injection": "CWE-78",
        "Path Traversal": "CWE-22",
        "Hardcoded Secrets": "CWE-798",
        "Insecure Dependencies": "CWE-327"
    }
    return cwes.get(vuln_type, "")


def get_general_recommendations(vulnerabilities: list) -> list:
    """Gera recomendacoes gerais baseadas nas vulnerabilidades encontradas"""
    recommendations = []
    types_found = set(v["type"] for v in vulnerabilities)

    if "SQL Injection" in types_found:
        recommendations.append("Implemente ORM ou use prepared statements em todas as queries")
    if "XSS" in types_found:
        recommendations.append("Implemente Content Security Policy (CSP) headers")
    if "Command Injection" in types_found:
        recommendations.append("Revise todas as chamadas de sistema e subprocess")
    if "Hardcoded Secrets" in types_found:
        recommendations.append("Configure um sistema de gerenciamento de secrets")

    if not vulnerabilities:
        recommendations.append("Nenhuma vulnerabilidade detectada na analise basica")
        recommendations.append("Considere usar ferramentas SAST dedicadas como Bandit (Python)")

    return recommendations


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
# API ENDPOINTS - PROJECT IMPORT (Issue #69)
# =============================================================================

@app.post("/api/projects/import")
async def import_project(
    file: Optional[UploadFile] = File(None),
    github_url: Optional[str] = Form(None),
    project_name: Optional[str] = Form(None),
    generate_stories: bool = Form(True)
):
    """
    Imports an existing project from ZIP file or GitHub URL.

    - Accepts ZIP file upload OR GitHub URL
    - Analyzes project structure (Python, Node, etc.)
    - Extracts README for description
    - Identifies modules/components
    - Optionally generates stories from TODOs and AI analysis
    """
    db = SessionLocal()
    try:
        claude_client = get_claude_client() if HAS_CLAUDE else None
        projects_dir = r'C:\Users\lcruz\Fabrica de Agentes\projects'

        result = await import_project_process(
            file=file,
            github_url=github_url,
            project_name=project_name,
            generate_stories=generate_stories,
            db_session=db,
            project_repo=ProjectRepository(db),
            story_repo=StoryRepository(db),
            claude_client=claude_client,
            notify_func=notify,
            projects_base_dir=projects_dir
        )
        return result
    finally:
        db.close()


@app.get("/api/projects/import/progress/{import_id}")
def get_import_progress(import_id: str):
    """Returns the current progress of a project import"""
    return get_import_progress_data(import_id)


@app.post("/api/projects/{project_id}/analyze")
def analyze_existing_project(project_id: str):
    """Analyzes an existing project and returns structure information."""
    db = SessionLocal()
    try:
        project_repo = ProjectRepository(db)
        project = project_repo.get_by_id(project_id)

        if not project:
            raise HTTPException(404, "Project not found")

        if not project.folder_path or not Path(project.folder_path).exists():
            raise HTTPException(400, "Project folder not found")

        analysis = analyze_project_structure(project.folder_path)
        return {"project_id": project_id, "analysis": analysis}
    finally:
        db.close()


@app.post("/api/projects/{project_id}/generate-stories-from-code")
def generate_stories_from_code(project_id: str):
    """Generates User Stories based on existing project code analysis."""
    db = SessionLocal()
    try:
        project_repo = ProjectRepository(db)
        story_repo = StoryRepository(db)
        project = project_repo.get_by_id(project_id)

        if not project:
            raise HTTPException(404, "Project not found")

        if not project.folder_path or not Path(project.folder_path).exists():
            raise HTTPException(400, "Project folder not found")

        analysis = analyze_project_structure(project.folder_path)
        claude_client = get_claude_client() if HAS_CLAUDE else None
        stories = generate_stories_from_analysis(project_id, analysis, story_repo, claude_client)

        return {
            "project_id": project_id,
            "stories_created": len(stories),
            "stories": stories
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
# APP GENERATOR - TESTE DE APLICACOES
# =============================================================================

from factory.core.app_generator import AppGenerator, analyze_project, generate_app, start_app as start_project_app

@app.get("/api/projects/{project_id}/app-status")
def get_project_app_status(project_id: str):
    """Analisa o projeto e retorna status da aplicacao para teste."""
    try:
        result = analyze_project(project_id)
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao analisar projeto: {str(e)}")


@app.post("/api/projects/{project_id}/generate-app")
def generate_project_app(project_id: str):
    """Gera uma aplicacao testavel para o projeto."""
    try:
        result = generate_app(project_id)
        if result.get("success"):
            notify("app_generated", {
                "project_id": project_id,
                "message": result.get("message"),
                "app_url": result.get("app_url")
            })
        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao gerar aplicacao: {str(e)}")


@app.post("/api/projects/{project_id}/start-app")
def start_project_test_app(project_id: str):
    """Inicia a aplicacao do projeto para teste."""
    try:
        # Primeiro verifica/gera a app
        generator = AppGenerator(project_id)
        analysis = generator.analyze_project()

        if not analysis.get("ready_to_test"):
            gen_result = generator.generate_testable_app()
            if not gen_result.get("success"):
                return gen_result

        # Iniciar a aplicacao
        result = generator.start_app()

        if result.get("success"):
            notify("app_started", {
                "project_id": project_id,
                "app_url": result.get("app_url"),
                "message": "Aplicacao iniciada!"
            })

        return result
    except Exception as e:
        raise HTTPException(500, f"Erro ao iniciar aplicacao: {str(e)}")




# =============================================================================
# SECURITY ANALYZER - Issue #57
# =============================================================================

from factory.core.security_analyzer import (
    SecurityAnalyzer, SecurityScanConfig, ScanType, get_security_badge
)

class SecurityScanRequest(BaseModel):
    use_bandit: Optional[bool] = True
    include_info: Optional[bool] = False
    scan_dependencies: Optional[bool] = False


@app.post("/api/projects/{project_id}/security-scan")
async def run_security_scan(project_id: str, request: Optional[SecurityScanRequest] = None):
    """Executa analise de seguranca SAST no projeto."""
    try:
        scan_types = [ScanType.SAST]
        if request and request.scan_dependencies:
            scan_types.append(ScanType.SCA)
        config = SecurityScanConfig(
            scan_types=scan_types,
            use_bandit=request.use_bandit if request else True,
            include_info=request.include_info if request else False,
            verbose=False
        )
        analyzer = SecurityAnalyzer(config)
        report = await analyzer.analyze(project_id)
        badge = get_security_badge(report.security_score)
        notify("security_scan_complete", {
            "project_id": project_id,
            "score": report.security_score,
            "risk_level": report.risk_level,
            "vulnerabilities_count": len(report.vulnerabilities),
            "badge": badge
        })
        return report.to_dict()
    except Exception as e:
        raise HTTPException(500, f"Erro ao executar scan de seguranca: {str(e)}")


@app.get("/api/projects/{project_id}/security-status")
def get_security_status(project_id: str):
    """Retorna status de seguranca do projeto."""
    return {"project_id": project_id, "has_scan": False}


@app.get("/api/security/badge/{project_id}")
def get_project_security_badge(project_id: str, score: int = 100):
    """Retorna badge de seguranca."""
    return {"project_id": project_id, "badge": get_security_badge(score)}


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
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
    <!-- xterm.js for terminal -->
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css">
    <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js"></script>
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
        .markdown-content code { background: #f3f4f6; padding: 0.125rem 0.25rem; border-radius: 0.25rem; font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace; font-size: 0.85em; }
        .markdown-content pre { background: #1e293b; color: #e2e8f0; padding: 1rem; border-radius: 0.5rem; overflow-x: auto; margin: 0.75rem 0; }
        .markdown-content pre code { background: transparent; color: inherit; padding: 0; font-size: 0.8rem; }

        /* Chat UX Improvements */
        .chat-message { animation: chatMessageSlideIn 0.3s ease-out; position: relative; }
        .chat-message:hover .chat-message-actions { opacity: 1; }
        .chat-message-actions { opacity: 0; transition: opacity 0.2s ease; position: absolute; top: -8px; right: 8px; display: flex; gap: 4px; background: white; border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); padding: 4px; }
        .chat-message-actions button { background: none; border: none; cursor: pointer; padding: 4px 6px; border-radius: 4px; color: #6b7280; font-size: 0.75rem; display: flex; align-items: center; gap: 2px; transition: all 0.2s; }
        .chat-message-actions button:hover { background: #f3f4f6; color: #1f2937; }
        .chat-message-actions button.reaction-active { color: #FF6C00; background: #fff7ed; }
        @keyframes chatMessageSlideIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }

        /* Typing Indicator */
        .typing-indicator { display: flex; align-items: center; gap: 4px; padding: 12px 16px; background: #f3f4f6; border-radius: 12px; border-bottom-left-radius: 4px; width: fit-content; }
        .typing-indicator span { width: 8px; height: 8px; background: #9ca3af; border-radius: 50%; animation: typingBounce 1.4s infinite ease-in-out both; }
        .typing-indicator span:nth-child(1) { animation-delay: -0.32s; }
        .typing-indicator span:nth-child(2) { animation-delay: -0.16s; }
        .typing-indicator span:nth-child(3) { animation-delay: 0s; }
        @keyframes typingBounce { 0%, 80%, 100% { transform: scale(0.6); opacity: 0.5; } 40% { transform: scale(1); opacity: 1; } }

        /* Quick Action Chips */
        .quick-actions { display: flex; flex-wrap: wrap; gap: 8px; padding: 8px 0; }
        .quick-action-chip { display: inline-flex; align-items: center; gap: 4px; padding: 6px 12px; background: #f3f4f6; border: 1px solid #e5e7eb; border-radius: 20px; font-size: 0.75rem; color: #374151; cursor: pointer; transition: all 0.2s; white-space: nowrap; }
        .quick-action-chip:hover { background: #003B4A; color: white; border-color: #003B4A; }
        .quick-action-chip svg { width: 14px; height: 14px; }

        /* New Message Indicator */
        .new-message-indicator { position: absolute; bottom: 80px; left: 50%; transform: translateX(-50%); background: #003B4A; color: white; padding: 8px 16px; border-radius: 20px; font-size: 0.75rem; cursor: pointer; display: flex; align-items: center; gap: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.2); animation: bounceIn 0.3s ease; z-index: 10; }
        .new-message-indicator:hover { background: #004d5f; }
        @keyframes bounceIn { 0% { transform: translateX(-50%) scale(0.8); opacity: 0; } 50% { transform: translateX(-50%) scale(1.05); } 100% { transform: translateX(-50%) scale(1); opacity: 1; } }

        /* Chat Error and Actions */
        .chat-error { background: #fef2f2 !important; border: 1px solid #fecaca; color: #dc2626 !important; }
        .chat-error-retry { display: inline-flex; align-items: center; gap: 4px; padding: 4px 8px; margin-top: 8px; background: #dc2626; color: white; border: none; border-radius: 4px; font-size: 0.7rem; cursor: pointer; transition: background 0.2s; }
        .chat-error-retry:hover { background: #b91c1c; }
        .chat-header-actions { display: flex; gap: 8px; }
        .chat-header-btn { background: rgba(255,255,255,0.1); border: none; color: white; padding: 4px 8px; border-radius: 4px; cursor: pointer; font-size: 0.7rem; display: flex; align-items: center; gap: 4px; transition: background 0.2s; }
        .chat-header-btn:hover { background: rgba(255,255,255,0.2); }

        /* Code Syntax Highlighting */
        .hljs-keyword { color: #c792ea; }
        .hljs-string { color: #c3e88d; }
        .hljs-number { color: #f78c6c; }
        .hljs-function { color: #82aaff; }
        .hljs-comment { color: #676e95; font-style: italic; }

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

                        <!-- Nova Story -->
                        <button @click="showNewStoryModal = true"
                                class="bg-[#FF6C00] hover:bg-orange-600 px-4 py-1.5 rounded text-sm font-medium transition">
                            + Nova Story
                        </button>
                    </div>
                </div>
            </div>
        </header>

        <div class="flex main-content-mobile" style="height: calc(100vh - 64px);">
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
                <!-- Header with Clear Button -->
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center gap-2">
                            <span class="text-xl">🤖</span>
                            <span class="font-semibold">Assistente</span>
                        </div>
                        <div class="chat-header-actions">
                            <button @click="confirmClearChat" class="chat-header-btn" title="Limpar conversa">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                                </svg>
                            </button>
                        </div>
                    </div>
                </div>

                <!-- Messages Container -->
                <div class="flex-1 overflow-y-auto p-4 chat-messages relative" ref="chatMessages" @scroll="handleChatScroll">
                    <!-- Messages -->
                    <div v-for="msg in chatHistory" :key="msg.message_id"
                         :class="['mb-4 chat-message', msg.role === 'user' ? 'text-right' : 'text-left']">
                        <div :class="['inline-block max-w-[90%] p-3 rounded-lg text-sm relative',
                                      msg.role === 'user'
                                        ? 'bg-[#003B4A] text-white rounded-br-none'
                                        : msg.isError ? 'chat-error rounded-bl-none' : 'bg-gray-100 text-gray-800 rounded-bl-none']">
                            <!-- Message Content -->
                            <div v-if="msg.role === 'assistant'" class="markdown-content" v-html="renderMarkdown(msg.content)"></div>
                            <div v-else>{{ msg.content }}</div>

                            <!-- Retry Button for Errors -->
                            <button v-if="msg.isError" @click="retryMessage(msg)" class="chat-error-retry">
                                <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
                                </svg>
                                Tentar novamente
                            </button>

                            <!-- Message Actions -->
                            <div v-if="msg.role === 'assistant' && !msg.isError" class="chat-message-actions">
                                <button @click="copyMessage(msg.content)" title="Copiar">
                                    <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                                    </svg>
                                </button>
                                <button @click="toggleReaction(msg, 'thumbsUp')" :class="{ 'reaction-active': msg.reaction === 'thumbsUp' }" title="Util">
                                    <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M14 10h4.764a2 2 0 011.789 2.894l-3.5 7A2 2 0 0115.263 21h-4.017c-.163 0-.326-.02-.485-.06L7 20m7-10V5a2 2 0 00-2-2h-.095c-.5 0-.905.405-.905.905 0 .714-.211 1.412-.608 2.006L7 11v9m7-10h-2M7 20H5a2 2 0 01-2-2v-6a2 2 0 012-2h2.5"/>
                                    </svg>
                                </button>
                                <button @click="toggleReaction(msg, 'thumbsDown')" :class="{ 'reaction-active': msg.reaction === 'thumbsDown' }" title="Nao util">
                                    <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 14H5.236a2 2 0 01-1.789-2.894l3.5-7A2 2 0 018.736 3h4.018a2 2 0 01.485.06l3.76.94m-7 10v5a2 2 0 002 2h.096c.5 0 .905-.405.905-.904 0-.715.211-1.413.608-2.008L17 13V4m-7 10h2m5-10h2a2 2 0 012 2v6a2 2 0 01-2 2h-2.5"/>
                                    </svg>
                                </button>
                            </div>
                        </div>
                        <!-- Timestamp -->
                        <div class="text-xs text-gray-400 mt-1">
                            {{ formatChatTime(msg.created_at) }}
                        </div>
                    </div>

                    <!-- Typing Indicator -->
                    <div v-if="isTyping" class="mb-4 text-left">
                        <div class="typing-indicator">
                            <span></span>
                            <span></span>
                            <span></span>
                        </div>
                    </div>

                    <!-- New Message Indicator -->
                    <div v-if="hasNewMessages && !isAtBottom"
                         class="new-message-indicator"
                         @click="scrollChatToBottom">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 14l-7 7m0 0l-7-7m7 7V3"/>
                        </svg>
                        Nova mensagem
                    </div>
                </div>

                <!-- Quick Actions -->
                <div class="px-4 py-2 border-t border-gray-100">
                    <div class="quick-actions">
                        <button class="quick-action-chip" @click="quickAction('criar story')">
                            <svg fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/>
                            </svg>
                            Criar Story
                        </button>
                        <button class="quick-action-chip" @click="quickAction('listar stories')">
                            <svg fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 10h16M4 14h16M4 18h16"/>
                            </svg>
                            Listar Stories
                        </button>
                        <button class="quick-action-chip" @click="quickAction('status do projeto')">
                            <svg fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/>
                            </svg>
                            Status
                        </button>
                    </div>
                </div>

                <!-- Input -->
                <div class="p-4 border-t border-gray-200">
                    <div class="flex gap-2">
                        <input v-model="chatInput"
                               @keyup.enter="sendMessage"
                               :disabled="isTyping"
                               type="text"
                               placeholder="Digite sua mensagem..."
                               class="flex-1 border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:border-[#003B4A] disabled:bg-gray-50 disabled:cursor-not-allowed">
                        <button @click="sendMessage"
                                :disabled="isTyping || !chatInput.trim()"
                                class="bg-[#FF6C00] text-white px-4 py-2 rounded-lg hover:bg-orange-600 transition disabled:opacity-50 disabled:cursor-not-allowed">
                            <svg v-if="!isTyping" class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"/>
                            </svg>
                            <svg v-else class="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
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
                                    <div v-if="task.generated_tests?.test_code" class="mt-1">
                                        <button @click.stop="showGeneratedTests(task)"
                                                class="text-xs text-purple-600 hover:text-purple-800 underline">
                                            Ver testes gerados ({{ task.generated_tests.test_count || 0 }} testes)
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

        <!-- Terminal and Preview Section -->
        <!-- AMBIENTE DE TESTE - Interface Amigavel -->
        <div v-if="selectedProjectId" class="bg-white rounded-lg shadow p-6 mt-4">
            <!-- Header com Status do Projeto -->
            <div class="flex items-center justify-between mb-6">
                <div>
                    <h3 class="text-xl font-semibold text-[#003B4A]">Status do Projeto</h3>
                    <p class="text-sm text-gray-500 mt-1">Acompanhe o progresso e saiba quando podera testar</p>
                </div>
                <div class="flex items-center gap-3">
                    <span :class="projectReadinessClass" class="px-4 py-2 rounded-full text-sm font-medium flex items-center gap-2">
                        <span v-html="projectReadinessIcon"></span>
                        {{ projectReadinessText }}
                    </span>
                </div>
            </div>

            <!-- Progresso Visual -->
            <div class="mb-6">
                <div class="flex items-center justify-between mb-2">
                    <span class="text-sm font-medium text-gray-700">Progresso Geral</span>
                    <span class="text-sm font-bold text-[#003B4A]">{{ projectProgress }}%</span>
                </div>
                <div class="w-full bg-gray-200 rounded-full h-4 overflow-hidden">
                    <div class="h-full rounded-full transition-all duration-500 flex items-center justify-end pr-2"
                         :style="{ width: projectProgress + '%', backgroundColor: projectProgress < 30 ? '#EF4444' : projectProgress < 70 ? '#F59E0B' : '#10B981' }">
                        <span v-if="projectProgress > 15" class="text-xs text-white font-medium">{{ projectProgress }}%</span>
                    </div>
                </div>
            </div>

            <!-- Timeline de Etapas -->
            <div class="mb-6">
                <h4 class="text-sm font-semibold text-gray-700 mb-4">Etapas do Desenvolvimento</h4>
                <div class="flex items-center justify-between relative">
                    <div class="absolute top-5 left-0 right-0 h-1 bg-gray-200 z-0"></div>
                    <div class="absolute top-5 left-0 h-1 bg-[#10B981] z-0 transition-all duration-500" :style="{ width: timelineProgress + '%' }"></div>

                    <div v-for="(step, idx) in projectSteps" :key="idx" class="flex flex-col items-center z-10 relative" style="width: 20%;">
                        <div :class="step.completed ? 'bg-[#10B981] text-white' : step.current ? 'bg-[#FF6C00] text-white animate-pulse' : 'bg-gray-300 text-gray-600'"
                             class="w-10 h-10 rounded-full flex items-center justify-center text-lg font-bold shadow-md transition-all">
                            <span v-if="step.completed">&#10003;</span>
                            <span v-else>{{ idx + 1 }}</span>
                        </div>
                        <span class="text-xs mt-2 text-center font-medium" :class="step.completed ? 'text-[#10B981]' : step.current ? 'text-[#FF6C00]' : 'text-gray-500'">{{ step.name }}</span>
                        <span class="text-xs text-gray-400 text-center">{{ step.description }}</span>
                    </div>
                </div>
            </div>

            <!-- Resumo das Stories -->
            <div class="grid grid-cols-4 gap-4 mb-6">
                <div class="bg-gray-50 rounded-lg p-4 text-center border border-gray-200">
                    <div class="text-3xl font-bold text-gray-400">{{ storyCounts.backlog }}</div>
                    <div class="text-xs text-gray-500 mt-1">Backlog</div>
                </div>
                <div class="bg-blue-50 rounded-lg p-4 text-center border border-blue-200">
                    <div class="text-3xl font-bold text-blue-600">{{ storyCounts.inProgress }}</div>
                    <div class="text-xs text-blue-600 mt-1">Em Desenvolvimento</div>
                </div>
                <div class="bg-purple-50 rounded-lg p-4 text-center border border-purple-200">
                    <div class="text-3xl font-bold text-purple-600">{{ storyCounts.testing }}</div>
                    <div class="text-xs text-purple-600 mt-1">Em Teste</div>
                </div>
                <div class="bg-green-50 rounded-lg p-4 text-center border border-green-200">
                    <div class="text-3xl font-bold text-green-600">{{ storyCounts.done }}</div>
                    <div class="text-xs text-green-600 mt-1">Concluidas</div>
                </div>
            </div>

            <!-- Status da Aplicacao para Teste -->
            <div class="mb-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
                <div class="flex items-center justify-between mb-3">
                    <h4 class="text-sm font-semibold text-gray-700">Aplicacao para Teste</h4>
                    <button @click="checkAppStatus" class="text-xs text-blue-600 hover:text-blue-800">
                        &#8635; Atualizar Status
                    </button>
                </div>

                <!-- Loading -->
                <div v-if="appStatusLoading" class="flex items-center gap-2 text-gray-500">
                    <div class="animate-spin w-5 h-5 border-2 border-gray-300 border-t-blue-600 rounded-full"></div>
                    <span>Verificando status...</span>
                </div>

                <!-- App Status Info -->
                <div v-else-if="appStatus">
                    <!-- Projeto nao encontrado -->
                    <div v-if="appStatus.status === 'not_found'" class="text-amber-600">
                        <p>&#128193; Pasta do projeto ainda nao foi criada pelos workers.</p>
                    </div>

                    <!-- Projeto analisado -->
                    <div v-else class="space-y-2">
                        <div class="flex items-center gap-4 text-sm">
                            <span class="text-gray-600">Tipo: <strong>{{ appStatus.project_type || 'Detectando...' }}</strong></span>
                            <span class="text-gray-600">Arquivos: <strong>{{ Object.values(appStatus.files_count || {}).reduce((a,b) => a+b, 0) }}</strong></span>
                            <span class="text-gray-600">Modelos: <strong>{{ (appStatus.models || []).length }}</strong></span>
                        </div>

                        <!-- Lista de modelos encontrados -->
                        <div v-if="appStatus.models && appStatus.models.length > 0" class="mt-2">
                            <p class="text-xs text-gray-500 mb-1">Modelos detectados:</p>
                            <div class="flex flex-wrap gap-1">
                                <span v-for="model in appStatus.models.slice(0, 8)" :key="model.name"
                                      class="px-2 py-1 bg-blue-100 text-blue-700 rounded text-xs">
                                    {{ model.name }}
                                </span>
                                <span v-if="appStatus.models.length > 8" class="px-2 py-1 bg-gray-100 text-gray-600 rounded text-xs">
                                    +{{ appStatus.models.length - 8 }} mais
                                </span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Mensagem de Status e Acao -->
            <div v-if="!appStatus?.ready_to_test && !appStatus?.can_generate_app" class="bg-amber-50 border border-amber-200 rounded-lg p-5">
                <div class="flex items-start gap-4">
                    <div class="text-4xl">&#128679;</div>
                    <div class="flex-1">
                        <h4 class="font-semibold text-amber-800 text-lg">Projeto em Desenvolvimento</h4>
                        <p class="text-amber-700 mt-1">{{ appStatus?.message || projectStatusMessage }}</p>
                        <div class="mt-3 text-sm text-amber-600">
                            <strong>Proximos passos:</strong>
                            <ul class="list-disc ml-5 mt-1 space-y-1">
                                <li v-for="step in nextSteps" :key="step">{{ step }}</li>
                            </ul>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Pode Gerar App -->
            <div v-else-if="appStatus?.can_generate_app && !appStatus?.ready_to_test" class="bg-blue-50 border border-blue-200 rounded-lg p-5">
                <div class="flex items-start gap-4">
                    <div class="text-4xl">&#128736;</div>
                    <div class="flex-1">
                        <h4 class="font-semibold text-blue-800 text-lg">Codigo Pronto - Preparar para Teste</h4>
                        <p class="text-blue-700 mt-1">{{ appStatus.message }}</p>
                        <p class="text-blue-600 text-sm mt-2">Clique no botao abaixo para gerar uma aplicacao testavel automaticamente.</p>
                        <div class="mt-4">
                            <button @click="generateAndStartApp"
                                    :disabled="generatingApp"
                                    class="px-6 py-3 bg-[#FF6C00] text-white rounded-lg font-medium hover:bg-orange-600 transition-colors flex items-center gap-2 shadow-md disabled:opacity-50">
                                <span v-if="!generatingApp" class="text-xl">&#9881;</span>
                                <div v-else class="animate-spin w-5 h-5 border-2 border-white border-t-transparent rounded-full"></div>
                                {{ generatingApp ? 'Preparando...' : 'Preparar Aplicacao para Teste' }}
                            </button>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Pronto para Testar -->
            <div v-else-if="appStatus?.ready_to_test" class="bg-green-50 border border-green-200 rounded-lg p-5">
                <div class="flex items-start gap-4">
                    <div class="text-4xl">&#9989;</div>
                    <div class="flex-1">
                        <h4 class="font-semibold text-green-800 text-lg">Pronto para Testar!</h4>
                        <p class="text-green-700 mt-1">A aplicacao esta disponivel para testes.</p>
                        <p v-if="appStatus.app_url" class="text-green-600 text-sm mt-1">
                            Endereco: <strong>{{ appStatus.app_url }}</strong>
                        </p>
                        <div class="mt-4 flex flex-wrap gap-3">
                            <button @click="startAndOpenApp"
                                    :disabled="startingApp"
                                    class="px-6 py-3 bg-[#10B981] text-white rounded-lg font-medium hover:bg-green-600 transition-colors flex items-center gap-2 shadow-md disabled:opacity-50">
                                <span v-if="!startingApp" class="text-xl">&#9654;</span>
                                <div v-else class="animate-spin w-5 h-5 border-2 border-white border-t-transparent rounded-full"></div>
                                {{ startingApp ? 'Iniciando...' : 'Testar Aplicacao' }}
                            </button>
                            <button v-if="appStatus.docs_url" @click="openDocs"
                                    class="px-6 py-3 bg-white text-[#003B4A] border border-[#003B4A] rounded-lg font-medium hover:bg-gray-50 transition-colors flex items-center gap-2">
                                <span class="text-xl">&#128214;</span>
                                Ver Documentacao API
                            </button>
                        </div>
                    </div>
                </div>
            </div>

            
            <!-- Technical Debt Score (Issue #60) -->
            <div v-if="debtScore" class="bg-gray-50 border border-gray-200 rounded-lg p-5 mt-4">
                <div class="flex items-start gap-4">
                    <div class="text-4xl">&#128200;</div>
                    <div class="flex-1">
                        <div class="flex items-center justify-between">
                            <h4 class="font-semibold text-gray-800 text-lg">Technical Debt Score</h4>
                            <span :class="['px-3 py-1 rounded-full text-sm font-medium',
                                debtScore.debt_score?.overall >= 80 ? 'bg-green-100 text-green-800' :
                                debtScore.debt_score?.overall >= 60 ? 'bg-blue-100 text-blue-800' :
                                debtScore.debt_score?.overall >= 40 ? 'bg-yellow-100 text-yellow-800' :
                                'bg-red-100 text-red-800']">
                                {{ debtScore.debt_score?.overall || 0 }}/100
                            </span>
                        </div>
                        <p class="text-gray-600 mt-1">{{ debtScore.summary?.status_message || 'Analisando...' }}</p>

                        <!-- Score bars -->
                        <div class="mt-3 space-y-2">
                            <div class="flex items-center gap-2 text-sm">
                                <span class="w-28 text-gray-500">Duplicacao:</span>
                                <div class="flex-1 bg-gray-200 rounded-full h-2">
                                    <div class="bg-blue-500 h-2 rounded-full transition-all"
                                         :style="{width: (debtScore.debt_score?.duplication || 0) + '%'}"></div>
                                </div>
                                <span class="w-10 text-right text-gray-600">{{ debtScore.debt_score?.duplication || 0 }}%</span>
                            </div>
                            <div class="flex items-center gap-2 text-sm">
                                <span class="w-28 text-gray-500">Complexidade:</span>
                                <div class="flex-1 bg-gray-200 rounded-full h-2">
                                    <div class="bg-purple-500 h-2 rounded-full transition-all"
                                         :style="{width: (debtScore.debt_score?.complexity || 0) + '%'}"></div>
                                </div>
                                <span class="w-10 text-right text-gray-600">{{ debtScore.debt_score?.complexity || 0 }}%</span>
                            </div>
                            <div class="flex items-center gap-2 text-sm">
                                <span class="w-28 text-gray-500">Manutencao:</span>
                                <div class="flex-1 bg-gray-200 rounded-full h-2">
                                    <div class="bg-green-500 h-2 rounded-full transition-all"
                                         :style="{width: (debtScore.debt_score?.maintainability || 0) + '%'}"></div>
                                </div>
                                <span class="w-10 text-right text-gray-600">{{ debtScore.debt_score?.maintainability || 0 }}%</span>
                            </div>
                        </div>

                        <div class="mt-4 flex flex-wrap gap-3">
                            <button @click="analyzeDebt"
                                    :disabled="analyzingDebt"
                                    class="px-4 py-2 bg-[#003B4A] text-white rounded-lg font-medium hover:bg-blue-900 transition-colors flex items-center gap-2 disabled:opacity-50">
                                <span v-if="!analyzingDebt">&#128269;</span>
                                <div v-else class="animate-spin w-4 h-4 border-2 border-white border-t-transparent rounded-full"></div>
                                {{ analyzingDebt ? 'Analisando...' : 'Analisar Debt' }}
                            </button>
                            <button @click="applyRefactoring"
                                    :disabled="refactoring || !debtScore.debt_score"
                                    class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg font-medium hover:bg-orange-600 transition-colors flex items-center gap-2 disabled:opacity-50">
                                <span v-if="!refactoring">&#9881;</span>
                                <div v-else class="animate-spin w-4 h-4 border-2 border-white border-t-transparent rounded-full"></div>
                                {{ refactoring ? 'Refatorando...' : 'Refatorar Codigo' }}
                            </button>
                            <button @click="generateDebtStories"
                                    :disabled="generatingDebtStories || !debtScore.debt_score"
                                    class="px-4 py-2 bg-white text-[#003B4A] border border-[#003B4A] rounded-lg font-medium hover:bg-gray-50 transition-colors flex items-center gap-2 disabled:opacity-50">
                                <span>&#128221;</span>
                                Gerar Stories
                            </button>
                        </div>

                        <!-- Recommendations -->
                        <div v-if="debtScore.summary?.recommendations?.length" class="mt-4 p-3 bg-yellow-50 rounded-lg border border-yellow-200">
                            <h5 class="text-sm font-medium text-yellow-800 mb-2">Recomendacoes:</h5>
                            <ul class="text-sm text-yellow-700 space-y-1">
                                <li v-for="rec in debtScore.summary.recommendations" :key="rec" class="flex items-start gap-2">
                                    <span>&#8226;</span>
                                    <span>{{ rec }}</span>
                                </li>
                            </ul>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Terminal Avancado (colapsavel para usuarios tecnicos) -->
            <details class="mt-6 bg-gray-50 rounded-lg border border-gray-200">
                <summary class="p-4 cursor-pointer text-sm font-medium text-gray-600 hover:text-gray-800 flex items-center gap-2">
                    <span>&#128736;</span> Ferramentas Avancadas (para desenvolvedores)
                </summary>
                <div class="p-4 border-t border-gray-200">
                    <div class="grid grid-cols-2 gap-4">
                        <!-- Terminal -->
                        <div class="flex flex-col">
                            <div class="flex items-center justify-between mb-2">
                                <h4 class="text-sm font-medium text-gray-600">Terminal</h4>
                                <div class="flex gap-2">
                                    <button @click="startApp" :disabled="terminalRunning"
                                            class="px-3 py-1 bg-green-500 text-white rounded text-xs hover:bg-green-600 disabled:opacity-50">
                                        &#9654; Iniciar
                                    </button>
                                    <button @click="runTests" :disabled="terminalRunning"
                                            class="px-3 py-1 bg-blue-500 text-white rounded text-xs hover:bg-blue-600 disabled:opacity-50">
                                        Testes
                                    </button>
                                    <button @click="stopProcess" :disabled="!terminalRunning"
                                            class="px-3 py-1 bg-red-500 text-white rounded text-xs hover:bg-red-600 disabled:opacity-50">
                                        Parar
                                    </button>
                                </div>
                            </div>
                            <div id="terminal-container" class="bg-black rounded border border-gray-300" style="height: 300px;"></div>
                            <div class="mt-2 flex gap-2">
                                <input v-model="terminalCommand" @keyup.enter="executeTerminalCommand" type="text"
                                       placeholder="Digite um comando..." class="flex-1 px-3 py-1 border border-gray-300 rounded text-sm">
                                <button @click="executeTerminalCommand" class="px-3 py-1 bg-[#FF6C00] text-white rounded text-xs">Executar</button>
                            </div>
                        </div>
                        <!-- Preview -->
                        <div class="flex flex-col">
                            <div class="flex items-center justify-between mb-2">
                                <h4 class="text-sm font-medium text-gray-600">Preview</h4>
                                <button @click="refreshPreview" class="px-3 py-1 bg-gray-200 text-gray-700 rounded text-xs hover:bg-gray-300">Refresh</button>
                            </div>
                            <div class="bg-gray-100 rounded border border-gray-300 flex items-center justify-center" style="height: 300px;">
                                <iframe ref="previewFrame" :src="previewUrl" style="width: 100%; height: 100%; border: none; border-radius: 4px;"></iframe>
                            </div>
                            <div class="mt-2 flex gap-2">
                                <input v-model="previewUrl" type="text" placeholder="http://localhost:3000" class="flex-1 px-3 py-1 border border-gray-300 rounded text-sm">
                                <button @click="refreshPreview" class="px-3 py-1 bg-[#003B4A] text-white rounded text-xs">Carregar</button>
                            </div>
                        </div>
                    </div>
                </div>
            </details>

            <!-- Preview Environments Section (Issue #66) -->
            <details class="mt-6 bg-white rounded-lg border border-gray-200 shadow-sm">
                <summary class="p-4 cursor-pointer text-sm font-semibold text-[#003B4A] hover:bg-gray-50 flex items-center gap-2">
                    <span class="text-xl">&#127760;</span> Ambientes de Preview e Staging
                    <span v-if="previewEnvironments.length > 0" class="ml-2 px-2 py-0.5 bg-blue-100 text-blue-700 rounded-full text-xs">{{ previewEnvironments.length }} ativos</span>
                </summary>
                <div class="p-4 border-t border-gray-200">
                    <!-- Header Actions -->
                    <div class="flex items-center justify-between mb-4">
                        <p class="text-sm text-gray-600">Crie ambientes isolados para testar stories e branches</p>
                        <div class="flex gap-2">
                            <button @click="createStoryPreview"
                                    :disabled="!selectedStory || creatingPreview"
                                    class="px-3 py-1.5 bg-[#FF6C00] text-white rounded text-xs font-medium hover:bg-orange-600 disabled:opacity-50 flex items-center gap-1">
                                <span v-if="!creatingPreview">&#128640;</span>
                                <div v-else class="animate-spin w-3 h-3 border-2 border-white border-t-transparent rounded-full"></div>
                                Preview Story
                            </button>
                            <button @click="showCreateBranchPreviewModal = true"
                                    class="px-3 py-1.5 bg-[#003B4A] text-white rounded text-xs font-medium hover:bg-opacity-90 flex items-center gap-1">
                                <span>&#128194;</span> Preview Branch
                            </button>
                            <button @click="createStagingEnv"
                                    :disabled="hasStagingEnv || creatingPreview"
                                    class="px-3 py-1.5 bg-purple-600 text-white rounded text-xs font-medium hover:bg-purple-700 disabled:opacity-50 flex items-center gap-1">
                                <span>&#9889;</span> Staging
                            </button>
                            <button @click="loadPreviewEnvironments"
                                    class="px-2 py-1.5 bg-gray-200 text-gray-700 rounded text-xs hover:bg-gray-300">
                                &#8635;
                            </button>
                        </div>
                    </div>

                    <!-- Loading State -->
                    <div v-if="previewsLoading" class="flex items-center justify-center py-8">
                        <div class="animate-spin w-6 h-6 border-2 border-gray-300 border-t-blue-600 rounded-full"></div>
                        <span class="ml-2 text-gray-500">Carregando ambientes...</span>
                    </div>

                    <!-- Preview List -->
                    <div v-else-if="previewEnvironments.length > 0" class="space-y-3">
                        <div v-for="preview in previewEnvironments" :key="preview.preview_id"
                             class="border border-gray-200 rounded-lg p-4 hover:border-gray-300 transition">
                            <div class="flex items-start justify-between">
                                <div class="flex-1">
                                    <div class="flex items-center gap-2 mb-1">
                                        <span :class="getPreviewTypeClass(preview.preview_type)" class="px-2 py-0.5 rounded text-xs font-medium">
                                            {{ preview.preview_type }}
                                        </span>
                                        <span :class="getPreviewStatusClass(preview.status)" class="px-2 py-0.5 rounded text-xs">
                                            {{ preview.status }}
                                        </span>
                                        <span v-if="preview.health_status === 'healthy'" class="text-green-500 text-xs">&#9679; Online</span>
                                        <span v-else-if="preview.health_status === 'unhealthy'" class="text-red-500 text-xs">&#9679; Problema</span>
                                    </div>
                                    <h4 class="font-medium text-gray-800">{{ preview.name }}</h4>
                                    <p v-if="preview.url" class="text-sm text-blue-600 mt-1">
                                        <a :href="preview.internal_url || preview.url" target="_blank" class="hover:underline flex items-center gap-1">
                                            {{ preview.url }} <span class="text-xs">&#8599;</span>
                                        </a>
                                    </p>
                                    <div class="flex items-center gap-4 mt-2 text-xs text-gray-500">
                                        <span v-if="preview.story_id">Story: {{ preview.story_id }}</span>
                                        <span v-if="preview.branch_name">Branch: {{ preview.branch_name }}</span>
                                        <span>Porta: {{ preview.port }}</span>
                                        <span v-if="preview.expires_at">Expira: {{ formatPreviewDate(preview.expires_at) }}</span>
                                    </div>
                                </div>
                                <div class="flex items-center gap-2">
                                    <button v-if="preview.qr_code" @click="showPreviewQRCode(preview)"
                                            class="p-2 text-gray-400 hover:text-gray-600 hover:bg-gray-100 rounded" title="QR Code">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v1m6 11h2m-6 0h-2v4m0-11v3m0 0h.01M12 12h4.01M16 20h4M4 12h4m12 0h.01M5 8h2a1 1 0 001-1V5a1 1 0 00-1-1H5a1 1 0 00-1 1v2a1 1 0 001 1zm12 0h2a1 1 0 001-1V5a1 1 0 00-1-1h-2a1 1 0 00-1 1v2a1 1 0 001 1zM5 20h2a1 1 0 001-1v-2a1 1 0 00-1-1H5a1 1 0 00-1 1v2a1 1 0 001 1z"/>
                                        </svg>
                                    </button>
                                    <button v-if="preview.status === 'stopped'" @click="startPreviewEnv(preview.preview_id)"
                                            class="p-2 text-green-500 hover:text-green-600 hover:bg-green-50 rounded" title="Iniciar">&#9654;</button>
                                    <button v-else-if="preview.status === 'running'" @click="stopPreviewEnv(preview.preview_id)"
                                            class="p-2 text-amber-500 hover:text-amber-600 hover:bg-amber-50 rounded" title="Parar">&#9632;</button>
                                    <button @click="destroyPreviewEnv(preview.preview_id)"
                                            class="p-2 text-red-400 hover:text-red-600 hover:bg-red-50 rounded" title="Destruir">&#128465;</button>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Empty State -->
                    <div v-else class="text-center py-8 text-gray-500">
                        <div class="text-4xl mb-2">&#127760;</div>
                        <p class="text-sm">Nenhum ambiente de preview ativo</p>
                        <p class="text-xs mt-1">Crie um preview para testar stories ou branches em ambiente isolado</p>
                    </div>

                    <!-- Preview Stats -->
                    <div v-if="previewStats" class="mt-4 pt-4 border-t border-gray-200 flex items-center justify-between text-xs text-gray-500">
                        <div class="flex gap-4">
                            <span>Total: {{ previewStats.total || 0 }}</span>
                            <span>Running: {{ previewStats.by_status?.running || 0 }}</span>
                            <span>Stopped: {{ previewStats.by_status?.stopped || 0 }}</span>
                        </div>
                        <button v-if="previewStats.expiring_soon > 0" @click="cleanupExpiredPreviews"
                                class="text-amber-600 hover:text-amber-700">
                            {{ previewStats.expiring_soon }} expirando em breve - Limpar
                        </button>
                    </div>
                </div>
            </details>
        </div>

        <!-- MODAL: Create Branch Preview -->
        <div v-if="showCreateBranchPreviewModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[400px]">
                <div class="p-4 border-b border-gray-200 bg-[#003B4A] text-white rounded-t-lg">
                    <h2 class="text-lg font-semibold">Criar Preview de Branch</h2>
                </div>
                <div class="p-6 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Nome da Branch *</label>
                        <input v-model="newBranchPreview.branch_name" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2"
                               placeholder="Ex: feature/login">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Nome do Preview</label>
                        <input v-model="newBranchPreview.name" type="text"
                               class="w-full border border-gray-300 rounded-lg px-3 py-2"
                               placeholder="Ex: Preview Feature Login">
                    </div>
                    <div class="flex items-center gap-4">
                        <label class="flex items-center gap-2 text-sm text-gray-600">
                            <input v-model="newBranchPreview.auto_destroy" type="checkbox" class="rounded">
                            Auto-destruir
                        </label>
                        <div v-if="newBranchPreview.auto_destroy">
                            <label class="text-sm text-gray-600">Apos</label>
                            <input v-model.number="newBranchPreview.destroy_after_hours" type="number" min="1" max="168"
                                   class="w-16 ml-1 border border-gray-300 rounded px-2 py-1 text-sm">
                            <span class="text-sm text-gray-600">horas</span>
                        </div>
                    </div>
                </div>
                <div class="p-4 border-t border-gray-200 flex justify-end gap-3">
                    <button @click="showCreateBranchPreviewModal = false"
                            class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">Cancelar</button>
                    <button @click="createBranchPreview"
                            :disabled="!newBranchPreview.branch_name || creatingPreview"
                            class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600 disabled:opacity-50">
                        Criar Preview
                    </button>
                </div>
            </div>
        </div>

        <!-- MODAL: QR Code -->
        <div v-if="showQRCodeModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-[350px] p-6 text-center">
                <h3 class="text-lg font-semibold mb-4">QR Code - Acesso Mobile</h3>
                <div class="mb-4">
                    <img :src="currentQRCode.qr_code" alt="QR Code" class="mx-auto w-48 h-48">
                </div>
                <p class="text-sm text-gray-600 mb-2">{{ currentQRCode.name }}</p>
                <p class="text-xs text-blue-600 break-all">{{ currentQRCode.url }}</p>
                <button @click="showQRCodeModal = false"
                        class="mt-4 px-4 py-2 bg-gray-200 text-gray-700 rounded-lg hover:bg-gray-300">Fechar</button>
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

        <!-- MODAL: Notification Preferences -->
        <div v-if="showNotificationPreferences" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showNotificationPreferences = false">
            <div class="bg-white rounded-lg w-[500px] shadow-xl">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <h2 class="text-lg font-semibold">Preferencias de Notificacao</h2>
                    <button @click="showNotificationPreferences = false" class="text-white/70 hover:text-white">
                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
                <div class="p-4 space-y-4">
                    <p class="text-sm text-gray-600 mb-4">Escolha quais tipos de notificacao voce deseja receber:</p>
                    <div class="space-y-3">
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">📝</span>
                                <div>
                                    <span class="font-medium">Nova Story</span>
                                    <p class="text-xs text-gray-500">Quando uma story e criada</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.story_created" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">➡️</span>
                                <div>
                                    <span class="font-medium">Story Movida</span>
                                    <p class="text-xs text-gray-500">Quando uma story muda de coluna</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.story_moved" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">✅</span>
                                <div>
                                    <span class="font-medium">Task Completa</span>
                                    <p class="text-xs text-gray-500">Quando uma task e completada</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.task_completed" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">🚀</span>
                                <div>
                                    <span class="font-medium">App Pronto</span>
                                    <p class="text-xs text-gray-500">Quando a aplicacao esta pronta para teste</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.app_ready" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">📦</span>
                                <div>
                                    <span class="font-medium">Build Completo</span>
                                    <p class="text-xs text-gray-500">Quando um build e finalizado</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.build_completed" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">❌</span>
                                <div>
                                    <span class="font-medium">Build Falhou</span>
                                    <p class="text-xs text-gray-500">Quando um build falha</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.build_failed" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                        <label class="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 cursor-pointer">
                            <div class="flex items-center gap-3">
                                <span class="text-xl">💬</span>
                                <div>
                                    <span class="font-medium">Nova Mensagem</span>
                                    <p class="text-xs text-gray-500">Mensagens do assistente IA</p>
                                </div>
                            </div>
                            <input type="checkbox" v-model="notificationPreferences.chat_message" class="w-5 h-5 text-[#FF6C00] rounded">
                        </label>
                    </div>
                </div>
                <div class="p-4 border-t flex justify-end gap-3">
                    <button @click="showNotificationPreferences = false" class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded-lg">
                        Cancelar
                    </button>
                    <button @click="saveNotificationPreferences" class="px-4 py-2 bg-[#FF6C00] text-white rounded-lg hover:bg-orange-600">
                        Salvar Preferencias
                    </button>
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

        <!-- FLOATING TEST BUTTON -->
        <div v-if="selectedProjectId" class="fixed bottom-24 right-6 z-50 flex flex-col items-end gap-3">
            <!-- Status Badge -->
            <div v-if="appStatus && !appStatusLoading"
                 class="bg-white rounded-full shadow-lg px-4 py-2 flex items-center gap-2 border-2 transition-all duration-300"
                 :class="{
                     'border-amber-400': !appStatus.can_generate_app && !appStatus.ready_to_test,
                     'border-blue-400': appStatus.can_generate_app && !appStatus.ready_to_test,
                     'border-green-400': appStatus.ready_to_test
                 }">
                <span class="text-sm font-medium"
                      :class="{
                          'text-amber-600': !appStatus.can_generate_app && !appStatus.ready_to_test,
                          'text-blue-600': appStatus.can_generate_app && !appStatus.ready_to_test,
                          'text-green-600': appStatus.ready_to_test
                      }">
                    {{ appStatus.ready_to_test ? 'Pronto!' : appStatus.can_generate_app ? 'Pode testar' : 'Desenvolvendo...' }}
                </span>
            </div>

            <!-- Main FAB Button -->
            <button v-if="appStatus?.ready_to_test || appStatus?.can_generate_app"
                    @click="appStatus.ready_to_test ? startAndOpenApp() : generateAndStartApp()"
                    :disabled="generatingApp || startingApp"
                    class="w-16 h-16 rounded-full shadow-xl flex items-center justify-center transition-all duration-300 hover:scale-110 active:scale-95 disabled:opacity-50"
                    :class="{
                        'bg-blue-500 hover:bg-blue-600': appStatus.can_generate_app && !appStatus.ready_to_test,
                        'bg-green-500 hover:bg-green-600': appStatus.ready_to_test
                    }"
                    :title="appStatus.ready_to_test ? 'Abrir aplicacao para teste' : 'Gerar e testar aplicacao'">
                <!-- Loading spinner -->
                <div v-if="generatingApp || startingApp"
                     class="w-8 h-8 border-3 border-white border-t-transparent rounded-full animate-spin"></div>
                <!-- Play icon when ready -->
                <svg v-else-if="appStatus.ready_to_test" class="w-8 h-8 text-white" fill="currentColor" viewBox="0 0 24 24">
                    <path d="M8 5v14l11-7z"/>
                </svg>
                <!-- Gear/Build icon when can generate -->
                <svg v-else class="w-8 h-8 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"/>
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
                </svg>
            </button>

            <!-- Development status FAB (grey, not clickable) -->
            <div v-else-if="appStatus && !appStatus.can_generate_app && !appStatus.ready_to_test"
                 class="w-16 h-16 rounded-full shadow-xl flex items-center justify-center bg-gray-400 cursor-not-allowed"
                 title="Projeto ainda em desenvolvimento">
                <svg class="w-8 h-8 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"/>
                </svg>
            </div>
        </div>

        <!-- MOBILE BOTTOM NAVIGATION -->
        <nav class="mobile-bottom-nav">
            <div class="mobile-bottom-nav-items">
                <div class="mobile-nav-item" :class="{ 'active': mobileMenuOpen }" @click="mobileMenuOpen = !mobileMenuOpen; mobileChatOpen = false">
                    <svg fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"/></svg>
                    <span>Menu</span>
                </div>
                <div class="mobile-nav-item" @click="showNewStoryModal = true">
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

    createApp({
        setup() {
            // State
            const projects = ref([]);
            const terminalCommand = ref('');
            const terminalRunning = ref(false);
            const terminal = ref(null);
            const terminalOutputInterval = ref(null);
            const previewUrl = ref('http://localhost:3000');
            const previewViewport = ref('desktop');

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
            const isTyping = ref(false);
            const hasNewMessages = ref(false);
            const isAtBottom = ref(true);
            const lastFailedMessage = ref(null);

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
            const currentKanbanColumn = ref(0);

            // Kanban scroll tracking for mobile
            const initKanbanScrollTracking = () => {
                const kanbanContainer = document.querySelector('.kanban-container');
                if (!kanbanContainer) return;

                const updateCurrentColumn = () => {
                    const containerWidth = kanbanContainer.clientWidth;
                    const scrollLeft = kanbanContainer.scrollLeft;
                    const columnWidth = containerWidth * 0.85;
                    const newColumn = Math.round(scrollLeft / columnWidth);
                    currentKanbanColumn.value = Math.min(5, Math.max(0, newColumn));
                };

                kanbanContainer.addEventListener('scroll', updateCurrentColumn, { passive: true });
            };

            // Scroll to specific Kanban column (for mobile navigation)
            const scrollToKanbanColumn = (columnIndex) => {
                const kanbanContainer = document.querySelector('.kanban-container');
                if (!kanbanContainer) return;

                const containerWidth = kanbanContainer.clientWidth;
                const columnWidth = containerWidth * 0.85;
                const targetScroll = columnIndex * columnWidth;

                kanbanContainer.scrollTo({
                    left: targetScroll,
                    behavior: 'smooth'
                });

                currentKanbanColumn.value = columnIndex;
            };

            // Toast Notifications
            const toasts = ref([]);
            let toastId = 0;

            // WebSocket Connection
            const wsStatus = ref('disconnected');
            const wsStatusText = ref('Offline');
            const wsStatusTitle = ref('Desconectado do servidor');
            const notificationSoundEnabled = ref(true);
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

            // Test Generation
            const generatingTests = ref(null);
            const showGeneratedTestsModal = ref(false);
            const currentGeneratedTests = ref(null);
            const showDesignEditor = ref(false);
            const showShortcutsModal = ref(false);
            const showBurndownModal = ref(false);

            // A/B Testing (Issue #71)
            const showABTestModal = ref(false);
            const abTests = ref([]);
            const currentABTest = ref(null);
            const abTestLoading = ref(false);
            const newABTest = ref({
                title: '',
                num_variants: 3,
                requirements: '',
                test_code: ''
            });

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

            // ==================== PROJECT STATUS FOR NON-TECH USERS ====================

            // Story counts by status
            const storyCounts = computed(() => {
                const board = storyBoard.value;
                return {
                    backlog: (board.backlog?.length || 0) + (board.ready?.length || 0),
                    inProgress: (board.in_progress?.length || 0) + (board.review?.length || 0),
                    testing: board.testing?.length || 0,
                    done: board.done?.length || 0
                };
            });

            // Project progress percentage
            const projectProgress = computed(() => {
                const counts = storyCounts.value;
                const total = counts.backlog + counts.inProgress + counts.testing + counts.done;
                if (total === 0) return 0;
                const completed = counts.done;
                const inProgress = counts.inProgress * 0.5 + counts.testing * 0.8;
                return Math.round(((completed + inProgress) / total) * 100);
            });

            // Is project ready for testing?
            const isProjectReady = computed(() => {
                const counts = storyCounts.value;
                return counts.testing > 0 || (counts.done > 0 && projectProgress.value >= 80);
            });

            // Project readiness text
            const projectReadinessText = computed(() => {
                const progress = projectProgress.value;
                if (progress === 0) return 'Aguardando Inicio';
                if (progress < 30) return 'Fase Inicial';
                if (progress < 60) return 'Em Desenvolvimento';
                if (progress < 80) return 'Quase Pronto';
                if (progress < 100) return 'Pronto para Testes';
                return 'Concluido';
            });

            // Project readiness class
            const projectReadinessClass = computed(() => {
                const progress = projectProgress.value;
                if (progress === 0) return 'bg-gray-200 text-gray-600';
                if (progress < 30) return 'bg-red-100 text-red-700';
                if (progress < 60) return 'bg-amber-100 text-amber-700';
                if (progress < 80) return 'bg-blue-100 text-blue-700';
                return 'bg-green-100 text-green-700';
            });

            // Project readiness icon
            const projectReadinessIcon = computed(() => {
                const progress = projectProgress.value;
                if (progress === 0) return '&#9711;';
                if (progress < 30) return '&#128679;';
                if (progress < 60) return '&#128736;';
                if (progress < 80) return '&#9881;';
                return '&#10003;';
            });

            // Project status message
            const projectStatusMessage = computed(() => {
                const counts = storyCounts.value;
                const progress = projectProgress.value;
                if (progress === 0) return 'O projeto ainda nao foi iniciado. Aguarde a equipe comecar o desenvolvimento.';
                if (progress < 30) return `Estamos na fase inicial do desenvolvimento. ${counts.inProgress} funcionalidade(s) em andamento.`;
                if (progress < 60) return `O desenvolvimento esta em andamento. ${counts.done} funcionalidade(s) ja foram concluidas.`;
                if (progress < 80) return `Estamos quase la! ${counts.done} funcionalidades prontas, ${counts.inProgress + counts.testing} em finalizacao.`;
                return `O projeto esta pronto para testes! ${counts.done} funcionalidades disponiveis.`;
            });

            // Next steps for the user
            const nextSteps = computed(() => {
                const counts = storyCounts.value;
                const progress = projectProgress.value;
                const steps = [];
                if (progress < 30) {
                    steps.push('Aguardar conclusao das primeiras funcionalidades');
                    steps.push('Acompanhar o progresso no quadro Kanban acima');
                } else if (progress < 60) {
                    steps.push('Revisar os requisitos das funcionalidades em desenvolvimento');
                    steps.push('Preparar cenarios de teste para quando estiver pronto');
                } else if (progress < 80) {
                    steps.push('Validar as funcionalidades ja concluidas');
                    steps.push('Reportar ajustes necessarios via chat');
                } else {
                    steps.push('Testar as funcionalidades disponiveis');
                    steps.push('Documentar problemas encontrados');
                }
                if (counts.testing > 0) {
                    steps.unshift(`${counts.testing} funcionalidade(s) em fase de testes`);
                }
                return steps;
            });

            // Project development steps for timeline
            const projectSteps = computed(() => {
                const progress = projectProgress.value;
                return [
                    { name: 'Planejamento', description: 'Requisitos', completed: progress > 0, current: progress === 0 },
                    { name: 'Desenvolvimento', description: 'Codificacao', completed: progress >= 30, current: progress > 0 && progress < 30 },
                    { name: 'Revisao', description: 'Code Review', completed: progress >= 60, current: progress >= 30 && progress < 60 },
                    { name: 'Testes', description: 'Validacao', completed: progress >= 80, current: progress >= 60 && progress < 80 },
                    { name: 'Entrega', description: 'Producao', completed: progress >= 100, current: progress >= 80 && progress < 100 }
                ];
            });

            // Timeline progress
            const timelineProgress = computed(() => {
                const progress = projectProgress.value;
                if (progress === 0) return 0;
                if (progress < 30) return 20;
                if (progress < 60) return 40;
                if (progress < 80) return 60;
                if (progress < 100) return 80;
                return 100;
            });

            // Show test instructions modal
            const showTestInstructions = ref(false);

            // Open test environment
            const openTestEnvironment = () => {
                window.open(previewUrl.value, '_blank');
            };

            // ==================== END PROJECT STATUS ====================

            // ==================== APP TESTING FUNCTIONS ====================

            // App Status State
            const appStatus = ref(null);
            const appStatusLoading = ref(false);

            // Refactoring (Issue #60)
            const debtScore = ref(null);
            const analyzingDebt = ref(false);
            const refactoring = ref(false);
            const generatingDebtStories = ref(false);

            const analyzeDebt = async () => {
                if (!selectedProjectId.value) return;
                analyzingDebt.value = true;
                try {
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/debt-score`);
                    const result = await response.json();
                    debtScore.value = result;
                    addToast('info', 'Analise Completa', `Score: ${result.debt_score?.overall || 0}/100`);
                } catch (error) {
                    addToast('error', 'Erro', 'Falha ao analisar debt');
                    console.error('Error analyzing debt:', error);
                } finally {
                    analyzingDebt.value = false;
                }
            };

            const applyRefactoring = async () => {
                if (!selectedProjectId.value) return;
                refactoring.value = true;
                try {
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/refactor`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ apply_all: true })
                    });
                    const result = await response.json();
                    if (result.refactoring_result?.applied_count > 0) {
                        addToast('success', 'Refatoracao Aplicada',
                            `${result.refactoring_result.applied_count} refatoracoes aplicadas`);
                        await analyzeDebt(); // Refresh score
                    } else {
                        addToast('info', 'Sem Refatoracoes', 'Nenhuma refatoracao automatica disponivel');
                    }
                } catch (error) {
                    addToast('error', 'Erro', 'Falha ao refatorar');
                    console.error('Error refactoring:', error);
                } finally {
                    refactoring.value = false;
                }
            };

            const generateDebtStories = async () => {
                if (!selectedProjectId.value) return;
                generatingDebtStories.value = true;
                try {
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/generate-debt-stories`, {
                        method: 'POST'
                    });
                    const result = await response.json();
                    if (result.suggested_stories?.length > 0) {
                        addToast('success', 'Stories Geradas',
                            `${result.suggested_stories.length} stories de debt sugeridas`);
                    } else {
                        addToast('info', 'Sem Sugestoes', 'Nenhuma story de debt sugerida');
                    }
                } catch (error) {
                    addToast('error', 'Erro', 'Falha ao gerar stories');
                    console.error('Error generating debt stories:', error);
                } finally {
                    generatingDebtStories.value = false;
                }
            };

            // Auto-analyze debt when project is selected
            watch(selectedProjectId, async (newId) => {
                if (newId) {
                    await analyzeDebt();
                }
            });

            const generatingApp = ref(false);
            const startingApp = ref(false);

            // Check app status for current project
            const checkAppStatus = async () => {
                if (!selectedProjectId.value) return;
                appStatusLoading.value = true;
                try {
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/app-status`);
                    if (response.ok) {
                        appStatus.value = await response.json();
                    } else {
                        appStatus.value = { status: 'not_found', message: 'Projeto nao encontrado' };
                    }
                } catch (e) {
                    appStatus.value = { status: 'error', message: 'Erro ao verificar status' };
                } finally {
                    appStatusLoading.value = false;
                }
            };

            // Generate and start the test app
            const generateAndStartApp = async () => {
                if (!selectedProjectId.value) return;
                generatingApp.value = true;
                try {
                    addToast('info', 'Gerando aplicacao', 'Preparando aplicacao para testes...');
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/generate-app`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' }
                    });
                    const result = await response.json();
                    if (response.ok && result.status === 'success') {
                        addToast('success', 'Aplicacao gerada', 'Iniciando servidor de teste...');
                        await startAndOpenApp();
                        await checkAppStatus();
                    } else {
                        addToast('error', 'Erro', result.message || 'Nao foi possivel gerar a aplicacao');
                    }
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao gerar aplicacao: ' + e.message);
                } finally {
                    generatingApp.value = false;
                }
            };

            // Start the test app and open browser
            const startAndOpenApp = async () => {
                if (!selectedProjectId.value) return;
                startingApp.value = true;
                try {
                    const response = await fetch(`/api/projects/${selectedProjectId.value}/start-app`, {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' }
                    });
                    const result = await response.json();
                    if (response.ok && result.status === 'success') {
                        addToast('success', 'Servidor iniciado', 'Abrindo aplicacao no navegador...');
                        // Wait a moment for the server to start
                        await new Promise(resolve => setTimeout(resolve, 2000));
                        if (result.app_url) {
                            window.open(result.app_url, '_blank');
                        }
                        await checkAppStatus();
                    } else if (result.status === 'already_running') {
                        addToast('info', 'Servidor ativo', 'A aplicacao ja esta rodando');
                        if (result.app_url) {
                            window.open(result.app_url, '_blank');
                        }
                    } else {
                        addToast('error', 'Erro', result.message || 'Nao foi possivel iniciar o servidor');
                    }
                } catch (e) {
                    addToast('error', 'Erro', 'Falha ao iniciar servidor: ' + e.message);
                } finally {
                    startingApp.value = false;
                }
            };

            // Open API docs
            const openDocs = () => {
                if (appStatus.value?.docs_url) {
                    window.open(appStatus.value.docs_url, '_blank');
                }
            };

            // ==================== END APP TESTING FUNCTIONS ====================

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

                // Check app status for testing
                checkAppStatus();

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

            const sendMessage = async (messageOverride = null) => {
                const messageContent = messageOverride || chatInput.value;
                if (!messageContent.trim()) return;

                chatInput.value = ''; // Clear immediately for better UX
                isTyping.value = true;

                // Add user message immediately
                const userMsg = {
                    message_id: 'user-' + Date.now(),
                    role: 'user',
                    content: messageContent,
                    created_at: new Date().toISOString()
                };
                chatHistory.value.push(userMsg);
                nextTick(() => scrollChatToBottom());

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
                    // Remove the temporary user message if the server returned one
                    if (data.user_message) {
                        const idx = chatHistory.value.findIndex(m => m.message_id === userMsg.message_id);
                        if (idx !== -1) chatHistory.value.splice(idx, 1);
                        chatHistory.value.push(data.user_message);
                    }
                    if (data.assistant_message) {
                        chatHistory.value.push(data.assistant_message);
                        if (!isAtBottom.value) {
                            hasNewMessages.value = true;
                        }
                    }
                    lastFailedMessage.value = null;
                    nextTick(() => scrollChatToBottom());
                } catch (error) {
                    console.error('Chat error:', error);
                    lastFailedMessage.value = messageContent;
                    // Show error message with retry option
                    chatHistory.value.push({
                        message_id: 'error-' + Date.now(),
                        role: 'assistant',
                        content: 'Desculpe, ocorreu um erro ao processar sua mensagem.',
                        created_at: new Date().toISOString(),
                        isError: true,
                        originalMessage: messageContent
                    });
                    nextTick(() => scrollChatToBottom());
                } finally {
                    isTyping.value = false;
                }
            };

            const scrollChatToBottom = () => {
                if (chatMessages.value) {
                    chatMessages.value.scrollTop = chatMessages.value.scrollHeight;
                    isAtBottom.value = true;
                    hasNewMessages.value = false;
                }
            };

            const handleChatScroll = () => {
                if (chatMessages.value) {
                    const { scrollTop, scrollHeight, clientHeight } = chatMessages.value;
                    isAtBottom.value = scrollHeight - scrollTop - clientHeight < 50;
                    if (isAtBottom.value) {
                        hasNewMessages.value = false;
                    }
                }
            };

            const copyMessage = async (content) => {
                try {
                    // Strip HTML tags for plain text copy
                    const plainText = content.replace(/<[^>]*>/g, '');
                    await navigator.clipboard.writeText(plainText);
                    addToast('success', 'Copiado!', 'Mensagem copiada para a area de transferencia');
                } catch (err) {
                    addToast('error', 'Erro', 'Nao foi possivel copiar');
                }
            };

            const toggleReaction = (msg, reaction) => {
                if (msg.reaction === reaction) {
                    msg.reaction = null;
                } else {
                    msg.reaction = reaction;
                }
            };

            const retryMessage = (errorMsg) => {
                // Remove error message
                const idx = chatHistory.value.findIndex(m => m.message_id === errorMsg.message_id);
                if (idx !== -1) {
                    chatHistory.value.splice(idx, 1);
                }
                // Also remove the user message that caused the error
                if (idx > 0 && chatHistory.value[idx - 1]?.role === 'user') {
                    chatHistory.value.splice(idx - 1, 1);
                }
                // Retry sending
                sendMessage(errorMsg.originalMessage || lastFailedMessage.value);
            };

            const quickAction = (action) => {
                chatInput.value = action;
                sendMessage();
            };

            const confirmClearChat = () => {
                confirmModal.value = {
                    title: 'Limpar Conversa',
                    message: 'Tem certeza que deseja limpar todo o historico de conversa?',
                    itemName: 'conversa',
                    confirmText: 'Limpar',
                    onConfirm: clearChat
                };
                showConfirmModal.value = true;
            };

            const clearChat = async () => {
                try {
                    await fetch('/api/chat/history?project_id=' + selectedProjectId.value, { method: 'DELETE' });
                    chatHistory.value = [{
                        message_id: 'welcome',
                        role: 'assistant',
                        content: 'Conversa limpa! Como posso ajudar?',
                        created_at: new Date().toISOString()
                    }];
                    addToast('success', 'Conversa limpa', 'Historico de mensagens foi apagado');
                } catch (err) {
                    addToast('error', 'Erro', 'Nao foi possivel limpar a conversa');
                }
                showConfirmModal.value = false;
            };

            const formatChatTime = (isoString) => {
                if (!isoString) return '';
                const date = new Date(isoString);
                const now = new Date();
                const diffMs = now - date;
                const diffMins = Math.floor(diffMs / 60000);
                const diffHours = Math.floor(diffMs / 3600000);
                const diffDays = Math.floor(diffMs / 86400000);

                if (diffMins < 1) return 'agora';
                if (diffMins < 60) return diffMins + ' min atras';
                if (diffHours < 24) return date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
                if (diffDays < 7) return date.toLocaleDateString('pt-BR', { weekday: 'short', hour: '2-digit', minute: '2-digit' });
                return date.toLocaleDateString('pt-BR', { day: '2-digit', month: 'short' });
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
                loadChatHistory();
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
                loadProjects();
                loadDarkMode();
                loadNotificationSoundPreference();

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
                formatTime, formatFileSize, renderMarkdown, formatChatTime,
                isTyping, hasNewMessages, isAtBottom, handleChatScroll,
                copyMessage, toggleReaction, retryMessage, quickAction,
                confirmClearChat, clearChat,
                addToast, removeToast, getToastIcon, handleUndo,
                cancelConfirm, executeConfirm, deleteStoryWithConfirm, deleteTaskWithConfirm,
                showContextMenu, hideContextMenu, contextMenuAction, moveToNextColumn,
                selectedTemplate, applyTemplate, isDarkMode, toggleDarkMode,
                showBurndownModal, burndownData, updateBurndownChart,
                bulkSelectMode, selectedStories, toggleBulkSelectMode, toggleBulkSelect,
                cancelBulkSelect, bulkMoveStories, bulkDeleteStories,
                terminalCommand, terminalRunning, previewUrl, previewViewport,
                executeTerminalCommand, startApp, runTests, stopProcess, refreshPreview,
                wsStatus, wsStatusText, wsStatusTitle, notificationSoundEnabled, toggleNotificationSound,
                generatingTests, showGeneratedTestsModal, currentGeneratedTests,
                generateTestsForTask, showGeneratedTests, copyTestCode, downloadTestCode,
                // Project Status (user-friendly)
                storyCounts, projectProgress, isProjectReady, projectReadinessText,
                projectReadinessClass, projectReadinessIcon, projectStatusMessage,
                nextSteps, projectSteps, timelineProgress, showTestInstructions, openTestEnvironment,
                // App Testing
                appStatus, appStatusLoading, generatingApp, startingApp,
                debtScore, analyzingDebt, refactoring, generatingDebtStories,
                analyzeDebt, applyRefactoring, generateDebtStories,
                checkAppStatus, generateAndStartApp, startAndOpenApp, openDocs,
                // Mobile State
                mobileMenuOpen, mobileChatOpen, isPullingToRefresh, currentKanbanColumn, scrollToKanbanColumn
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
