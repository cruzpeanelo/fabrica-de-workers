# -*- coding: utf-8 -*-
"""
Project Preview Dashboard API - Issue #73
==========================================
Endpoints para visao unificada do projeto com:
- Overview do projeto
- Metricas e progresso
- Arquivos gerados
- Documentacao
- Status de testes
"""

import os
from datetime import datetime
from pathlib import Path
from typing import Optional

from fastapi import APIRouter, HTTPException

# Database
from factory.database.connection import SessionLocal
from factory.database.repositories import (
    ProjectRepository, StoryRepository, StoryDocumentationRepository
)

# Router para Project Preview
router = APIRouter(prefix="/api/projects", tags=["Project Preview"])


@router.get("/{project_id}/preview")
def get_project_preview(project_id: str):
    """
    Retorna dados unificados para o Project Preview Dashboard.
    Inclui: overview, progresso, arquivos, documentacao, testes e metricas.
    """
    db = SessionLocal()
    try:
        # Repositorios
        project_repo = ProjectRepository(db)
        story_repo = StoryRepository(db)
        doc_repo = StoryDocumentationRepository(db)

        # Buscar projeto
        project = project_repo.get_by_id(project_id)
        if not project:
            raise HTTPException(404, "Projeto nao encontrado")

        # Buscar todas as stories do projeto
        stories = story_repo.get_all(project_id=project_id)

        # Calcular metricas de stories
        story_metrics = {
            "total": len(stories),
            "backlog": 0,
            "ready": 0,
            "in_progress": 0,
            "review": 0,
            "testing": 0,
            "done": 0,
            "total_points": 0,
            "completed_points": 0
        }

        all_docs = []
        all_designs = []
        all_files = []
        all_tasks = []
        test_results = {"passed": 0, "failed": 0, "skipped": 0, "total": 0}

        for story in stories:
            status = story.status.value if hasattr(story.status, 'value') else story.status
            if status in story_metrics:
                story_metrics[status] += 1

            story_metrics["total_points"] += story.story_points or 0
            if status == "done":
                story_metrics["completed_points"] += story.story_points or 0

            # Coletar documentos
            story_docs = doc_repo.get_by_story(story.story_id)
            for doc in story_docs:
                all_docs.append({
                    "doc_id": doc.doc_id,
                    "story_id": doc.story_id,
                    "doc_type": doc.doc_type.value if hasattr(doc.doc_type, 'value') else doc.doc_type,
                    "title": doc.title,
                    "created_at": doc.created_at.isoformat() if doc.created_at else None
                })

            # Coletar designs
            if hasattr(story, 'designs') and story.designs:
                for design in story.designs:
                    all_designs.append({
                        "design_id": design.design_id,
                        "story_id": design.story_id,
                        "design_type": design.design_type.value if hasattr(design.design_type, 'value') else design.design_type,
                        "title": design.title,
                        "thumbnail": design.thumbnail
                    })

            # Coletar tasks e resultados de teste
            if hasattr(story, 'tasks') and story.tasks:
                for task in story.tasks:
                    task_dict = {
                        "task_id": task.task_id,
                        "title": task.title,
                        "status": task.status.value if hasattr(task.status, 'value') else task.status,
                        "task_type": task.task_type.value if hasattr(task.task_type, 'value') else task.task_type,
                        "files_created": task.files_created or []
                    }
                    all_tasks.append(task_dict)

                    # Acumular arquivos criados
                    if task.files_created:
                        all_files.extend(task.files_created)

                    # Acumular resultados de teste
                    if task.test_results:
                        test_results["passed"] += task.test_results.get("passed", 0)
                        test_results["failed"] += task.test_results.get("failed", 0)
                        test_results["skipped"] += task.test_results.get("skipped", 0)

        test_results["total"] = test_results["passed"] + test_results["failed"] + test_results["skipped"]

        # Calcular progresso geral
        progress = 0
        if story_metrics["total"] > 0:
            progress = round((story_metrics["done"] / story_metrics["total"]) * 100, 1)

        # Verificar diretorio do projeto
        project_path = Path(f"C:/Users/lcruz/Fabrica de Agentes/projects/{project_id}")
        project_files = {
            "exists": project_path.exists(),
            "total_files": 0,
            "by_type": {},
            "recent_files": []
        }

        if project_path.exists():
            for file in project_path.rglob("*"):
                if file.is_file() and not any(p in str(file) for p in ['node_modules', '__pycache__', '.git', 'venv']):
                    project_files["total_files"] += 1
                    ext = file.suffix.lower() or "other"
                    project_files["by_type"][ext] = project_files["by_type"].get(ext, 0) + 1

            # Arquivos recentes (ultimos 10)
            all_project_files = [f for f in project_path.rglob("*") if f.is_file() and not any(p in str(f) for p in ['node_modules', '__pycache__', '.git', 'venv'])]
            sorted_files = sorted(all_project_files, key=lambda f: f.stat().st_mtime, reverse=True)[:10]
            project_files["recent_files"] = [{"name": f.name, "path": str(f.relative_to(project_path)), "size": f.stat().st_size} for f in sorted_files]

        # Verificar status da aplicacao
        app_status = None
        try:
            from factory.core.app_generator import analyze_project
            app_status = analyze_project(project_id)
        except:
            app_status = {"status": "unknown", "message": "Nao foi possivel analisar a aplicacao"}

        return {
            "project": {
                "project_id": project.project_id,
                "name": project.name,
                "description": project.description,
                "project_type": project.project_type,
                "status": project.status,
                "created_at": project.created_at.isoformat() if project.created_at else None,
                "updated_at": project.updated_at.isoformat() if project.updated_at else None
            },
            "metrics": {
                "stories": story_metrics,
                "progress": progress,
                "total_docs": len(all_docs),
                "total_designs": len(all_designs),
                "total_tasks": len(all_tasks),
                "total_files": len(set(all_files))
            },
            "test_results": test_results,
            "documentation": {
                "items": all_docs[:20],
                "total": len(all_docs),
                "by_type": {}
            },
            "designs": all_designs[:10],
            "project_files": project_files,
            "app_status": app_status,
            "tasks_summary": {
                "total": len(all_tasks),
                "completed": len([t for t in all_tasks if t["status"] == "completed"]),
                "in_progress": len([t for t in all_tasks if t["status"] == "in_progress"]),
                "pending": len([t for t in all_tasks if t["status"] == "pending"])
            }
        }
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Erro ao buscar preview do projeto: {str(e)}")
    finally:
        db.close()


@router.get("/{project_id}/files")
def get_project_files(project_id: str, path: str = ""):
    """
    Lista arquivos do projeto para navegacao.
    """
    project_path = Path(f"C:/Users/lcruz/Fabrica de Agentes/projects/{project_id}")

    if not project_path.exists():
        return {"exists": False, "files": [], "message": "Pasta do projeto nao encontrada"}

    target_path = project_path / path if path else project_path

    if not target_path.exists():
        raise HTTPException(404, "Caminho nao encontrado")

    files = []
    try:
        for item in sorted(target_path.iterdir(), key=lambda x: (not x.is_dir(), x.name.lower())):
            if item.name.startswith('.') or item.name in ['node_modules', '__pycache__', 'venv', '.git']:
                continue

            files.append({
                "name": item.name,
                "path": str(item.relative_to(project_path)),
                "is_dir": item.is_dir(),
                "size": item.stat().st_size if item.is_file() else None,
                "modified": datetime.fromtimestamp(item.stat().st_mtime).isoformat()
            })
    except Exception as e:
        raise HTTPException(500, f"Erro ao listar arquivos: {str(e)}")

    return {
        "exists": True,
        "current_path": path,
        "files": files,
        "parent_path": str(Path(path).parent) if path else None
    }


@router.get("/{project_id}/file-content")
def get_project_file_content(project_id: str, path: str):
    """
    Retorna conteudo de um arquivo do projeto.
    """
    project_path = Path(f"C:/Users/lcruz/Fabrica de Agentes/projects/{project_id}")
    file_path = project_path / path

    if not file_path.exists():
        raise HTTPException(404, "Arquivo nao encontrado")

    if not file_path.is_file():
        raise HTTPException(400, "Caminho especificado nao e um arquivo")

    # Verificar tamanho do arquivo (max 1MB)
    if file_path.stat().st_size > 1024 * 1024:
        return {"content": None, "message": "Arquivo muito grande para visualizacao", "size": file_path.stat().st_size}

    # Verificar se e arquivo de texto
    text_extensions = {'.py', '.js', '.ts', '.jsx', '.tsx', '.html', '.css', '.json', '.md', '.txt', '.yaml', '.yml', '.toml', '.ini', '.cfg', '.sql', '.sh', '.bat', '.env', '.gitignore'}
    if file_path.suffix.lower() not in text_extensions:
        return {"content": None, "message": "Tipo de arquivo nao suportado para visualizacao", "extension": file_path.suffix}

    try:
        content = file_path.read_text(encoding='utf-8')
        return {
            "content": content,
            "filename": file_path.name,
            "extension": file_path.suffix,
            "size": len(content),
            "lines": content.count('\n') + 1
        }
    except UnicodeDecodeError:
        return {"content": None, "message": "Arquivo binario nao pode ser visualizado"}
    except Exception as e:
        raise HTTPException(500, f"Erro ao ler arquivo: {str(e)}")
