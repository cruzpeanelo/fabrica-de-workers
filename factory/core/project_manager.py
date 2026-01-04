"""
Project Manager - Plataforma E
Gerencia criacao e estrutura de projetos
"""
import shutil
from pathlib import Path
from typing import Dict, Optional, List
from datetime import datetime

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.config import PROJECTS_DIR, TEMPLATES_DIR, PROJECT_TYPES
from factory.database.connection import SessionLocal
from factory.database.models import Project, Template
from factory.database.repositories import ProjectRepository, TemplateRepository


class ProjectManager:
    """Gerenciador de Projetos da Fabrica"""

    def __init__(self):
        self.projects_dir = PROJECTS_DIR
        self.templates_dir = TEMPLATES_DIR

    def create_project(
        self,
        name: str,
        project_type: str,
        description: str = None,
        template_id: str = None,
        config: Dict = None
    ) -> Optional[Dict]:
        """Cria um novo projeto"""

        db = SessionLocal()
        try:
            # Gera ID do projeto
            count = db.query(Project).count()
            project_id = f"PRJ-{count + 1:03d}"

            # Cria pasta do projeto
            folder_name = name.lower().replace(" ", "_").replace("-", "_")
            folder_path = self.projects_dir / folder_name

            if folder_path.exists():
                folder_path = self.projects_dir / f"{folder_name}_{project_id.lower()}"

            folder_path.mkdir(parents=True, exist_ok=True)

            # Aplica template se especificado
            if template_id:
                self._apply_template(folder_path, template_id, db)
            else:
                self._create_basic_structure(folder_path, project_type)

            # Cria projeto no banco
            repo = ProjectRepository(db)
            project = repo.create({
                "project_id": project_id,
                "name": name,
                "description": description,
                "project_type": project_type,
                "template_used": template_id,
                "folder_path": str(folder_path),
                "config": config or PROJECT_TYPES.get(project_type, {}).get("default_stack", {}),
                "status": "PLANNING",
                "progress": 0.0
            })

            # Cria README do projeto
            self._create_readme(folder_path, project.to_dict())

            return project.to_dict()

        except Exception as e:
            print(f"[ProjectManager] Erro ao criar projeto: {e}")
            if folder_path.exists():
                shutil.rmtree(folder_path)
            raise
        finally:
            db.close()

    def _create_basic_structure(self, folder_path: Path, project_type: str):
        """Cria estrutura basica do projeto"""

        structures = {
            "web-app": {
                "dirs": ["src", "src/components", "src/pages", "src/services", "public", "tests"],
                "files": {
                    "src/index.ts": "// Entry point\n",
                    "package.json": '{"name": "project", "version": "1.0.0"}\n'
                }
            },
            "api-service": {
                "dirs": ["app", "app/routers", "app/services", "app/models", "tests"],
                "files": {
                    "app/__init__.py": '"""API Service"""\n',
                    "app/main.py": 'from fastapi import FastAPI\n\napp = FastAPI()\n',
                    "requirements.txt": "fastapi\nuvicorn\n"
                }
            },
            "data-analysis": {
                "dirs": ["data", "notebooks", "scripts", "reports"],
                "files": {
                    "requirements.txt": "pandas\nnumpy\nmatplotlib\n",
                    "scripts/__init__.py": '"""Analysis scripts"""\n'
                }
            },
            "document": {
                "dirs": ["docs", "assets", "output"],
                "files": {
                    "docs/index.md": "# Documento\n\nConteudo aqui.\n"
                }
            },
            "automation": {
                "dirs": ["scripts", "config", "logs"],
                "files": {
                    "scripts/__init__.py": '"""Automation scripts"""\n',
                    "config/settings.json": '{"enabled": true}\n'
                }
            },
            "integration": {
                "dirs": ["connectors", "transformers", "config", "tests"],
                "files": {
                    "connectors/__init__.py": '"""API Connectors"""\n',
                    "config/endpoints.json": '{"apis": []}\n'
                }
            }
        }

        structure = structures.get(project_type, structures["web-app"])

        # Cria diretorios
        for dir_name in structure.get("dirs", []):
            (folder_path / dir_name).mkdir(parents=True, exist_ok=True)

        # Cria arquivos
        for file_path, content in structure.get("files", {}).items():
            full_path = folder_path / file_path
            full_path.parent.mkdir(parents=True, exist_ok=True)
            full_path.write_text(content, encoding="utf-8")

    def _apply_template(self, folder_path: Path, template_id: str, db):
        """Aplica template ao projeto"""
        repo = TemplateRepository(db)
        template = repo.get_by_id(template_id)

        if not template:
            return

        structure = template.structure or {}

        # Cria diretorios do template
        for dir_name in structure.get("directories", []):
            (folder_path / dir_name).mkdir(parents=True, exist_ok=True)

        # Cria arquivos do template
        for file_info in structure.get("files", []):
            file_path = folder_path / file_info.get("path", "")
            file_path.parent.mkdir(parents=True, exist_ok=True)
            file_path.write_text(file_info.get("content", ""), encoding="utf-8")

    def _create_readme(self, folder_path: Path, project: Dict):
        """Cria README do projeto"""
        readme_content = f"""# {project['name']}

> Projeto criado pela Plataforma E

## Informacoes

- **ID**: {project['project_id']}
- **Tipo**: {project['project_type']}
- **Status**: {project['status']}
- **Criado em**: {project['created_at']}

## Descricao

{project.get('description', 'Sem descricao.')}

## Estrutura

```
{folder_path.name}/
├── README.md
└── ...
```

---
*Gerado automaticamente pela Plataforma E*
"""
        (folder_path / "README.md").write_text(readme_content, encoding="utf-8")

    def get_project(self, project_id: str) -> Optional[Dict]:
        """Busca projeto por ID"""
        db = SessionLocal()
        try:
            repo = ProjectRepository(db)
            project = repo.get_by_id(project_id)
            return project.to_dict() if project else None
        finally:
            db.close()

    def list_projects(self, status: str = None) -> List[Dict]:
        """Lista projetos"""
        db = SessionLocal()
        try:
            repo = ProjectRepository(db)
            projects = repo.get_all(status=status)
            return [p.to_dict() for p in projects]
        finally:
            db.close()

    def update_project_status(self, project_id: str, status: str, progress: float = None) -> Optional[Dict]:
        """Atualiza status do projeto"""
        db = SessionLocal()
        try:
            repo = ProjectRepository(db)
            data = {"status": status}
            if progress is not None:
                data["progress"] = progress
            if status == "COMPLETED":
                data["completed_at"] = datetime.utcnow()

            project = repo.update(project_id, data)
            return project.to_dict() if project else None
        finally:
            db.close()

    def delete_project(self, project_id: str, delete_files: bool = False) -> bool:
        """Remove projeto"""
        db = SessionLocal()
        try:
            repo = ProjectRepository(db)
            project = repo.get_by_id(project_id)

            if not project:
                return False

            # Remove pasta do projeto se solicitado
            if delete_files and project.folder_path:
                folder = Path(project.folder_path)
                if folder.exists():
                    shutil.rmtree(folder)

            return repo.delete(project_id)
        finally:
            db.close()


# Instancia global
project_manager = ProjectManager()


def get_project_manager() -> ProjectManager:
    """Retorna instancia do gerenciador de projetos"""
    return project_manager
