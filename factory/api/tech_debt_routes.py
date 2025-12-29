# -*- coding: utf-8 -*-
"""
Tech Debt API Routes - Fabrica de Agentes
==========================================
Endpoints para analise de debito tecnico e refatoracao automatica.
"""

import os
import json
import shutil
from datetime import datetime
from typing import Optional, List
from pydantic import BaseModel

from fastapi import APIRouter, HTTPException, Form

# Importar tech debt analyzer
from factory.core.tech_debt_analyzer import (
    analyze_project_tech_debt,
    add_to_history,
    get_history,
    get_trend,
    get_debt_recommendations
)

# Criar router
router = APIRouter(prefix="/api/projects", tags=["tech-debt"])


def register_routes(app):
    """Register tech debt routes with the FastAPI app"""
    app.include_router(router)
    print("[TechDebt] Routes registered successfully")


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class TechDebtAnalysisRequest(BaseModel):
    """Request para analise de debito tecnico"""
    file_patterns: Optional[List[str]] = ["*.py", "*.js", "*.ts", "*.tsx", "*.jsx"]
    exclude_patterns: Optional[List[str]] = ["node_modules", "__pycache__", ".git", "venv", "dist", "build"]
    include_suggestions: Optional[bool] = True


class RefactoringRequest(BaseModel):
    """Request para aplicar refatoracao"""
    file_path: str
    suggestion_id: str
    refactoring_type: str
    original_code: Optional[str] = None
    apply_automatically: Optional[bool] = False


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_project_path(project_id: str) -> str:
    """Retorna o caminho do projeto"""
    # Tentar buscar do banco primeiro
    try:
        from factory.database.connection import SessionLocal
        from factory.database.repositories import ProjectRepository

        db = SessionLocal()
        try:
            repo = ProjectRepository(db)
            project = repo.get_by_id(project_id)
            if project and project.folder_path:
                return project.folder_path
        finally:
            db.close()
    except:
        pass

    # Fallback para caminho padrao
    return f"C:/Users/lcruz/Fabrica de Agentes/projects/{project_id}"


def notify_event(event_type: str, data: dict):
    """Envia notificacao via WebSocket"""
    try:
        from factory.dashboard.app_v6_agile import notify
        notify(event_type, data)
    except:
        pass  # Notificacao e opcional


# =============================================================================
# API ENDPOINTS
# =============================================================================

@router.post("/{project_id}/tech-debt-analysis")
def analyze_tech_debt(project_id: str, request: TechDebtAnalysisRequest = None):
    """
    Analisa debito tecnico de um projeto

    Retorna:
    - Metricas gerais (complexity score, duplicacao, TODOs)
    - Code smells por arquivo
    - Sugestoes de refatoracao priorizadas
    """
    if request is None:
        request = TechDebtAnalysisRequest()

    project_path = get_project_path(project_id)

    if not os.path.exists(project_path):
        return {
            "project_id": project_id,
            "status": "no_files",
            "message": f"Pasta do projeto nao encontrada: {project_path}",
            "metrics": {
                "complexity_score": 0,
                "duplication_percentage": 0,
                "todos_count": 0,
                "total_files": 0,
                "total_lines": 0
            },
            "files": [],
            "suggestions": []
        }

    # Executar analise
    result = analyze_project_tech_debt(
        project_path=project_path,
        file_patterns=request.file_patterns,
        exclude_patterns=request.exclude_patterns,
        include_suggestions=request.include_suggestions
    )

    # Adicionar ao historico
    if result.get("status") == "completed":
        add_to_history(project_id, result.get("metrics", {}))

    # Adicionar info do projeto
    result["project_id"] = project_id
    result["history"] = get_history(project_id)[-10:]

    # Notificar
    notify_event("tech_debt_analyzed", {
        "project_id": project_id,
        "complexity_score": result.get("metrics", {}).get("complexity_score", 0),
        "total_files": result.get("metrics", {}).get("total_files", 0)
    })

    return result


@router.get("/{project_id}/tech-debt")
def get_tech_debt_summary(project_id: str):
    """
    Retorna resumo do debito tecnico (ultima analise + historico)
    """
    history = get_history(project_id)

    if not history:
        return {
            "project_id": project_id,
            "has_analysis": False,
            "message": "Nenhuma analise de debito tecnico encontrada. Execute POST /api/projects/{project_id}/tech-debt-analysis primeiro."
        }

    latest = history[-1]
    trend = get_trend(project_id)

    return {
        "project_id": project_id,
        "has_analysis": True,
        "latest_analysis": latest,
        "trend": trend,
        "history": history,
        "recommendations": get_debt_recommendations(latest)
    }


@router.post("/{project_id}/apply-refactoring")
def apply_refactoring(project_id: str, request: RefactoringRequest):
    """
    Aplica uma refatoracao sugerida usando Claude AI

    Retorna diff antes/depois e opcao de aceitar ou rejeitar
    """
    project_path = get_project_path(project_id)
    file_full_path = os.path.join(project_path, request.file_path)

    if not os.path.exists(file_full_path):
        raise HTTPException(404, f"Arquivo nao encontrado: {request.file_path}")

    # Ler codigo original
    with open(file_full_path, 'r', encoding='utf-8') as f:
        original_code = f.read()

    # Tentar usar Claude para refatoracao
    refactored_code = original_code
    refactoring_explanation = ""

    try:
        from factory.ai.claude_integration import get_claude_client

        claude = get_claude_client()
        if claude.is_available():
            prompt = f"""Voce e um especialista em refatoracao de codigo.

TAREFA: {request.refactoring_type}
ARQUIVO: {request.file_path}
SUGESTAO ID: {request.suggestion_id}

CODIGO ORIGINAL:
```
{original_code[:8000]}
```

Aplique a refatoracao sugerida ({request.refactoring_type}) para melhorar a qualidade do codigo.

Responda em JSON:
{{
    "refactored_code": "codigo completo refatorado",
    "explanation": "explicacao das mudancas feitas",
    "changes_summary": ["lista de mudancas principais"]
}}

IMPORTANTE: Mantenha a funcionalidade original. Apenas melhore a estrutura."""

            system_prompt = """Voce e um engenheiro de software senior especializado em refatoracao.
Seu objetivo e melhorar a qualidade do codigo mantendo a funcionalidade.
Sempre responda em JSON valido."""

            response = claude.chat(prompt, system_prompt, max_tokens=8000)

            if response.success:
                try:
                    content = response.content.strip()
                    if content.startswith("```"):
                        lines = content.split("\n")
                        content = "\n".join(lines[1:-1])

                    result = json.loads(content)
                    refactored_code = result.get("refactored_code", original_code)
                    refactoring_explanation = result.get("explanation", "")
                except json.JSONDecodeError:
                    refactoring_explanation = "Erro ao processar resposta da IA"

    except Exception as e:
        print(f"[Refactoring] Erro ao usar Claude: {e}")
        refactoring_explanation = f"IA nao disponivel. Refatoracao manual necessaria."

    # Gerar diff
    import difflib
    diff = list(difflib.unified_diff(
        original_code.splitlines(keepends=True),
        refactored_code.splitlines(keepends=True),
        fromfile=f"a/{request.file_path}",
        tofile=f"b/{request.file_path}"
    ))

    # Se aplicar automaticamente
    applied = False
    if request.apply_automatically and refactored_code != original_code:
        try:
            # Backup
            backup_path = file_full_path + ".bak"
            shutil.copy2(file_full_path, backup_path)

            with open(file_full_path, 'w', encoding='utf-8') as f:
                f.write(refactored_code)
            applied = True
        except Exception as e:
            raise HTTPException(500, f"Erro ao aplicar refatoracao: {str(e)}")

    result = {
        "project_id": project_id,
        "file_path": request.file_path,
        "suggestion_id": request.suggestion_id,
        "refactoring_type": request.refactoring_type,
        "original_code": original_code[:5000],
        "refactored_code": refactored_code[:5000],
        "diff": ''.join(diff)[:10000],
        "explanation": refactoring_explanation,
        "applied": applied,
        "has_changes": refactored_code != original_code
    }

    if applied:
        notify_event("refactoring_applied", {
            "project_id": project_id,
            "file_path": request.file_path,
            "type": request.refactoring_type
        })

    return result


@router.post("/{project_id}/accept-refactoring")
def accept_refactoring(project_id: str, file_path: str, refactored_code: str = Form(...)):
    """
    Aceita e aplica uma refatoracao proposta
    """
    project_path = get_project_path(project_id)
    file_full_path = os.path.join(project_path, file_path)

    if not os.path.exists(file_full_path):
        raise HTTPException(404, f"Arquivo nao encontrado: {file_path}")

    # Backup do original
    backup_path = file_full_path + ".bak"
    shutil.copy2(file_full_path, backup_path)

    try:
        with open(file_full_path, 'w', encoding='utf-8') as f:
            f.write(refactored_code)

        notify_event("refactoring_accepted", {
            "project_id": project_id,
            "file_path": file_path
        })

        return {
            "success": True,
            "file_path": file_path,
            "backup_path": backup_path,
            "message": "Refatoracao aplicada com sucesso"
        }
    except Exception as e:
        # Restaurar backup em caso de erro
        if os.path.exists(backup_path):
            shutil.copy2(backup_path, file_full_path)
        raise HTTPException(500, f"Erro ao aplicar: {str(e)}")


@router.post("/{project_id}/reject-refactoring")
def reject_refactoring(project_id: str, suggestion_id: str):
    """
    Rejeita uma sugestao de refatoracao
    """
    return {
        "success": True,
        "suggestion_id": suggestion_id,
        "message": "Sugestao rejeitada"
    }
