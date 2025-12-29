# -*- coding: utf-8 -*-
"""
Project Import Module - Issue #69
=================================
Provides functionality to import existing projects from ZIP files or GitHub URLs,
analyze their structure, and generate User Stories automatically.
"""

import os
import re
import json
import uuid
import shutil
import zipfile
import tempfile
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Optional, List

from fastapi import UploadFile, File, Form, HTTPException
from pydantic import BaseModel


class ProjectImportRequest(BaseModel):
    """Request for importing a project from GitHub URL"""
    github_url: str
    project_name: Optional[str] = None
    generate_stories: bool = True


class ProjectImportProgress(BaseModel):
    """Progress update during import"""
    step: str
    progress: int
    message: str
    details: Optional[dict] = None


# Global import progress storage
import_progress = {}


def analyze_project_structure(project_path: str) -> dict:
    """
    Analyzes an imported project and detects its structure.

    Args:
        project_path: Path to the project directory

    Returns:
        dict with project type, language, modules, README content, TODOs, etc.
    """
    project_path = Path(project_path)
    analysis = {
        "language": "unknown",
        "framework": None,
        "project_type": "generic",
        "has_readme": False,
        "readme_content": "",
        "modules": [],
        "files_count": 0,
        "todos": [],
        "dependencies": [],
        "entry_points": [],
        "structure": {}
    }

    # Count files
    all_files = list(project_path.rglob("*"))
    analysis["files_count"] = len([f for f in all_files if f.is_file()])

    # Check for README
    readme_patterns = ["README.md", "README.rst", "README.txt", "README"]
    for pattern in readme_patterns:
        readme_file = project_path / pattern
        if readme_file.exists():
            analysis["has_readme"] = True
            try:
                with open(readme_file, "r", encoding="utf-8", errors="ignore") as f:
                    analysis["readme_content"] = f.read()[:5000]  # Limit size
            except:
                pass
            break

    # Detect language and framework - Python
    if (project_path / "requirements.txt").exists() or (project_path / "setup.py").exists() or (project_path / "pyproject.toml").exists():
        analysis["language"] = "python"
        analysis["project_type"] = "python-app"

        # Read requirements
        req_file = project_path / "requirements.txt"
        if req_file.exists():
            try:
                with open(req_file, "r", encoding="utf-8") as f:
                    analysis["dependencies"] = [line.strip() for line in f if line.strip() and not line.startswith("#")]
            except:
                pass

        # Detect framework
        deps_str = " ".join(analysis["dependencies"]).lower()
        if "django" in deps_str:
            analysis["framework"] = "django"
            analysis["project_type"] = "django-app"
        elif "flask" in deps_str:
            analysis["framework"] = "flask"
            analysis["project_type"] = "flask-app"
        elif "fastapi" in deps_str:
            analysis["framework"] = "fastapi"
            analysis["project_type"] = "fastapi-app"

        # Find Python modules
        for py_file in project_path.rglob("*.py"):
            rel_path = py_file.relative_to(project_path)
            if len(rel_path.parts) == 1 or rel_path.parts[0] not in ["venv", ".venv", "env", "__pycache__", "node_modules"]:
                analysis["modules"].append(str(rel_path))

        # Find entry points
        for entry in ["main.py", "app.py", "run.py", "manage.py", "wsgi.py"]:
            if (project_path / entry).exists():
                analysis["entry_points"].append(entry)

    # Node.js
    elif (project_path / "package.json").exists():
        analysis["language"] = "javascript"
        analysis["project_type"] = "node-app"

        try:
            with open(project_path / "package.json", "r", encoding="utf-8") as f:
                pkg = json.load(f)
                analysis["dependencies"] = list(pkg.get("dependencies", {}).keys())

                # Detect framework
                deps = analysis["dependencies"]
                if "react" in deps:
                    analysis["framework"] = "react"
                    analysis["project_type"] = "react-app"
                elif "vue" in deps:
                    analysis["framework"] = "vue"
                    analysis["project_type"] = "vue-app"
                elif "next" in deps:
                    analysis["framework"] = "next"
                    analysis["project_type"] = "nextjs-app"
                elif "express" in deps:
                    analysis["framework"] = "express"
                    analysis["project_type"] = "express-api"
        except:
            pass

        # Find JS/TS files
        for ext in ["*.js", "*.jsx", "*.ts", "*.tsx"]:
            for js_file in project_path.rglob(ext):
                rel_path = js_file.relative_to(project_path)
                if "node_modules" not in str(rel_path):
                    analysis["modules"].append(str(rel_path))

    # Go
    elif (project_path / "go.mod").exists():
        analysis["language"] = "go"
        analysis["project_type"] = "go-app"

        for go_file in project_path.rglob("*.go"):
            analysis["modules"].append(str(go_file.relative_to(project_path)))

    # Java/Kotlin
    elif (project_path / "pom.xml").exists() or (project_path / "build.gradle").exists():
        analysis["language"] = "java"
        analysis["project_type"] = "java-app"

        for java_file in project_path.rglob("*.java"):
            analysis["modules"].append(str(java_file.relative_to(project_path)))

    # Find TODOs in source files
    todo_pattern = re.compile(r'(?:TODO|FIXME|HACK|XXX|BUG)[\s:]+(.+?)(?:\n|$)', re.IGNORECASE)
    source_extensions = [".py", ".js", ".jsx", ".ts", ".tsx", ".java", ".go", ".rs", ".rb", ".php"]

    for file_path in all_files:
        if file_path.is_file() and file_path.suffix.lower() in source_extensions:
            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
                    matches = todo_pattern.findall(content)
                    for match in matches[:10]:  # Limit TODOs per file
                        analysis["todos"].append({
                            "file": str(file_path.relative_to(project_path)),
                            "text": match.strip()[:200]
                        })
            except:
                pass

        # Limit total TODOs
        if len(analysis["todos"]) >= 50:
            break

    # Build structure tree (first 2 levels)
    structure = {}
    for item in project_path.iterdir():
        if item.name.startswith(".") or item.name in ["node_modules", "__pycache__", "venv", ".venv"]:
            continue
        if item.is_dir():
            structure[item.name] = {
                "type": "directory",
                "children": [f.name for f in item.iterdir() if not f.name.startswith(".")][:20]
            }
        else:
            structure[item.name] = {"type": "file"}
    analysis["structure"] = structure

    # Limit modules list
    analysis["modules"] = analysis["modules"][:100]

    return analysis


def generate_stories_from_analysis(project_id: str, analysis: dict, story_repo, claude_client=None) -> list:
    """
    Generates User Stories based on project analysis.

    Args:
        project_id: ID of the project
        analysis: Result from analyze_project_structure
        story_repo: StoryRepository instance
        claude_client: Optional Claude client for AI story generation

    Returns:
        list of created story dicts
    """
    created_stories = []

    # Generate stories from TODOs
    for idx, todo in enumerate(analysis.get("todos", [])[:10]):
        story_data = {
            "project_id": project_id,
            "title": f"[TODO] {todo['text'][:80]}",
            "description": f"TODO encontrado em: {todo['file']}\n\n{todo['text']}",
            "persona": "desenvolvedor",
            "action": "resolver o TODO identificado no codigo",
            "benefit": "manter o codigo limpo e completo",
            "category": "tech_debt",
            "story_points": 2,
            "priority": "medium",
            "tags": ["imported", "todo", "tech-debt"]
        }
        try:
            story = story_repo.create(story_data)
            created_stories.append(story.to_dict())
        except:
            pass

    # Generate stories based on README if Claude is available
    if claude_client and analysis.get("readme_content"):
        try:
            if claude_client.is_available():
                prompt = f"""Analise este README de um projeto {analysis['language']} e sugira 3-5 User Stories para melhorias.

README:
{analysis['readme_content'][:3000]}

ESTRUTURA DO PROJETO:
- Linguagem: {analysis['language']}
- Framework: {analysis.get('framework', 'N/A')}
- Modulos principais: {', '.join(analysis.get('modules', [])[:10])}
- Dependencias: {', '.join(analysis.get('dependencies', [])[:15])}

Retorne um JSON array com stories no formato:
[
  {{
    "title": "Titulo curto e descritivo",
    "persona": "tipo de usuario",
    "action": "o que o usuario quer fazer",
    "benefit": "beneficio esperado",
    "category": "feature|improvement|bug|tech_debt",
    "story_points": 3,
    "priority": "low|medium|high"
  }}
]

Responda APENAS com o JSON array."""

                response = claude_client.chat(prompt, max_tokens=2000)

                if response.success:
                    content = response.content.strip()
                    # Remove markdown code blocks if present
                    if content.startswith("```"):
                        lines = content.split("\n")
                        content = "\n".join(lines[1:-1])

                    try:
                        suggested_stories = json.loads(content)
                        for story_data in suggested_stories:
                            story_data["project_id"] = project_id
                            story_data["tags"] = ["imported", "ai-generated"]
                            story_data.setdefault("story_points", 3)
                            story = story_repo.create(story_data)
                            created_stories.append(story.to_dict())
                    except json.JSONDecodeError:
                        pass
        except Exception as e:
            print(f"[ImportProject] Claude error: {e}")

    # Fallback: Generate basic stories based on analysis
    if not created_stories or len(created_stories) < 3:
        # Documentation story
        if not analysis.get("has_readme"):
            story_data = {
                "project_id": project_id,
                "title": "Criar documentacao README",
                "persona": "desenvolvedor",
                "action": "ter uma documentacao clara do projeto",
                "benefit": "facilitar onboarding de novos contribuidores",
                "category": "documentation",
                "story_points": 3,
                "priority": "high",
                "tags": ["imported", "documentation"]
            }
            try:
                story = story_repo.create(story_data)
                created_stories.append(story.to_dict())
            except:
                pass

        # Tests story
        story_data = {
            "project_id": project_id,
            "title": "Implementar testes automatizados",
            "persona": "desenvolvedor",
            "action": "ter cobertura de testes adequada",
            "benefit": "garantir qualidade e evitar regressoes",
            "category": "improvement",
            "story_points": 8,
            "priority": "medium",
            "acceptance_criteria": [
                "Cobertura de testes acima de 70%",
                "Testes unitarios para funcoes principais",
                "CI/CD configurado para rodar testes"
            ],
            "tags": ["imported", "testing"]
        }
        try:
            story = story_repo.create(story_data)
            created_stories.append(story.to_dict())
        except:
            pass

        # Code review story
        story_data = {
            "project_id": project_id,
            "title": "Revisar e refatorar codigo existente",
            "persona": "desenvolvedor",
            "action": "melhorar a qualidade do codigo",
            "benefit": "aumentar manutenibilidade e legibilidade",
            "category": "tech_debt",
            "story_points": 5,
            "priority": "medium",
            "tags": ["imported", "refactoring"]
        }
        try:
            story = story_repo.create(story_data)
            created_stories.append(story.to_dict())
        except:
            pass

    return created_stories


async def process_import(
    file: Optional[UploadFile],
    github_url: Optional[str],
    project_name: Optional[str],
    generate_stories: bool,
    db_session,
    project_repo,
    story_repo,
    claude_client,
    notify_func,
    projects_base_dir: str
) -> dict:
    """
    Main import processing function.

    Args:
        file: Uploaded ZIP file (optional)
        github_url: GitHub repository URL (optional)
        project_name: Custom project name (optional)
        generate_stories: Whether to auto-generate stories
        db_session: Database session
        project_repo: ProjectRepository instance
        story_repo: StoryRepository instance
        claude_client: Claude AI client (optional)
        notify_func: WebSocket notification function
        projects_base_dir: Base directory for projects

    Returns:
        dict with import results
    """
    import_id = uuid.uuid4().hex[:8]

    try:
        import_progress[import_id] = {"step": "starting", "progress": 0, "message": "Iniciando importacao..."}

        # Validate input
        if not file and not github_url:
            raise HTTPException(400, "Envie um arquivo ZIP ou informe a URL do GitHub")

        # Create temp directory
        temp_dir = Path(tempfile.mkdtemp())
        project_path = None

        try:
            # Step 1: Get project files
            import_progress[import_id] = {"step": "downloading", "progress": 10, "message": "Obtendo arquivos do projeto..."}

            if file:
                # Handle ZIP upload
                if not file.filename.endswith('.zip'):
                    raise HTTPException(400, "Apenas arquivos ZIP sao aceitos")

                zip_path = temp_dir / file.filename
                with open(zip_path, "wb") as f:
                    content = await file.read()
                    f.write(content)

                # Extract ZIP
                import_progress[import_id] = {"step": "extracting", "progress": 20, "message": "Extraindo arquivos..."}
                with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                    zip_ref.extractall(temp_dir)

                # Find project root (handle nested directories)
                extracted_items = list(temp_dir.iterdir())
                if len(extracted_items) == 2 and any(p.suffix == '.zip' for p in extracted_items):
                    for item in extracted_items:
                        if item.is_dir():
                            project_path = item
                            break
                else:
                    project_path = temp_dir

                if not project_name:
                    project_name = file.filename.replace('.zip', '')

            else:
                # Handle GitHub URL
                import_progress[import_id] = {"step": "cloning", "progress": 15, "message": "Clonando repositorio..."}

                github_url = github_url.strip()
                if github_url.endswith('.git'):
                    github_url = github_url[:-4]

                if not project_name:
                    project_name = github_url.split('/')[-1]

                clone_path = temp_dir / project_name
                result = subprocess.run(
                    ["git", "clone", "--depth", "1", github_url, str(clone_path)],
                    capture_output=True,
                    text=True,
                    timeout=120
                )

                if result.returncode != 0:
                    raise HTTPException(400, f"Erro ao clonar repositorio: {result.stderr}")

                project_path = clone_path

            # Step 2: Analyze project
            import_progress[import_id] = {"step": "analyzing", "progress": 40, "message": "Analisando estrutura do projeto..."}
            analysis = analyze_project_structure(str(project_path))

            # Step 3: Create project in database
            import_progress[import_id] = {"step": "creating", "progress": 60, "message": "Criando projeto na Fabrica..."}

            # Generate description from README
            description = ""
            if analysis.get("readme_content"):
                readme_lines = analysis["readme_content"].split("\n")
                for line in readme_lines:
                    if line.strip() and not line.startswith("#") and not line.startswith("!["):
                        description = line.strip()[:500]
                        break

            if not description:
                description = f"Projeto {analysis['language']} importado"
                if analysis.get("framework"):
                    description += f" ({analysis['framework']})"

            # Copy project to projects folder
            projects_dir = Path(projects_base_dir)
            projects_dir.mkdir(exist_ok=True)

            # Generate unique project ID
            project_id = f"IMP-{project_name[:20].upper().replace(' ', '-').replace('_', '-')}-{uuid.uuid4().hex[:4].upper()}"

            final_path = projects_dir / project_id
            if final_path.exists():
                shutil.rmtree(final_path)
            shutil.copytree(project_path, final_path)

            # Create project record
            project_data = {
                "project_id": project_id,
                "name": project_name,
                "description": description,
                "project_type": analysis.get("project_type", "generic"),
                "status": "IN_PROGRESS",
                "folder_path": str(final_path),
                "github_url": github_url if github_url else None,
                "config": {
                    "language": analysis.get("language"),
                    "framework": analysis.get("framework"),
                    "imported": True,
                    "import_date": datetime.utcnow().isoformat()
                },
                "tags": ["imported", analysis.get("language", "unknown")]
            }

            project = project_repo.create(project_data)

            # Step 4: Generate stories (optional)
            created_stories = []
            if generate_stories:
                import_progress[import_id] = {"step": "generating_stories", "progress": 80, "message": "Gerando User Stories..."}
                created_stories = generate_stories_from_analysis(project_id, analysis, story_repo, claude_client)

            # Complete
            import_progress[import_id] = {"step": "complete", "progress": 100, "message": "Importacao concluida!"}

            result = {
                "success": True,
                "project": project.to_dict(),
                "analysis": {
                    "language": analysis.get("language"),
                    "framework": analysis.get("framework"),
                    "files_count": analysis.get("files_count"),
                    "modules_count": len(analysis.get("modules", [])),
                    "todos_count": len(analysis.get("todos", [])),
                    "has_readme": analysis.get("has_readme")
                },
                "stories_created": len(created_stories),
                "stories": created_stories,
                "import_id": import_id
            }

            # Notify via WebSocket
            if notify_func:
                notify_func("project_imported", {
                    "project_id": project_id,
                    "name": project_name,
                    "stories_created": len(created_stories)
                })

            return result

        finally:
            # Cleanup temp directory
            try:
                shutil.rmtree(temp_dir)
            except:
                pass

    except HTTPException:
        raise
    except Exception as e:
        import_progress[import_id] = {"step": "error", "progress": 0, "message": str(e)}
        raise HTTPException(500, f"Erro ao importar projeto: {str(e)}")


def get_progress(import_id: str) -> dict:
    """Returns the current progress of a project import"""
    if import_id not in import_progress:
        raise HTTPException(404, "Import not found")
    return import_progress[import_id]
