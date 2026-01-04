# -*- coding: utf-8 -*-
"""
App Generator - Gera aplicacoes testaveis automaticamente
=========================================================

Este modulo analisa o codigo gerado pelos workers e cria uma aplicacao
executavel que o usuario pode testar com 1 clique.

Suporta:
- Python (FastAPI)
  - SQLAlchemy models
  - Pydantic models
- Node.js (Express) - Issue #75
  - Sequelize models
  - TypeORM entities
  - Mongoose schemas
- Frontend React/Vue - Issue #76
  - Veja: frontend_generator.py

Issue #75: Suporte a Node.js/Express
Issue #76: Suporte a Frontend React/Vue (implementado em frontend_generator.py)
Issue #78: Sincronizacao project_id com nome da pasta
"""

import os
import re
import subprocess
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from datetime import datetime


# =============================================================================
# UTILITY FUNCTIONS - Issue #78: Sincronizacao project_id com nome da pasta
# =============================================================================


def normalize_folder_name(name: str) -> str:
    """
    Normaliza um nome para formato de pasta valido.
    Converte para lowercase e substitui hifens por underscores.

    Issue #78: Sincronizacao project_id com nome da pasta

    Args:
        name: Nome a ser normalizado (pode ser project_id ou name)

    Returns:
        Nome normalizado para uso como pasta

    Exemplo:
        >>> normalize_folder_name("BELGO-BPM-001")
        'belgo_bpm_001'
    """
    return name.lower().replace("-", "_").replace(" ", "_")


def get_project_folder_variations(project_id: str) -> List[str]:
    """
    Gera todas as variacoes possiveis de nome de pasta para um project_id.

    Issue #78: Sincronizacao project_id com nome da pasta

    Args:
        project_id: ID do projeto

    Returns:
        Lista de possiveis nomes de pasta
    """
    variations = [
        project_id,
        project_id.lower(),
        project_id.upper(),
        project_id.lower().replace("-", "_"),
        project_id.upper().replace("-", "_"),
        project_id.replace("-", "_"),
        project_id.lower().replace("_", "-"),
        project_id.replace(" ", "_").lower(),
        project_id.replace(" ", "-").lower(),
    ]
    seen = set()
    unique = []
    for v in variations:
        if v not in seen:
            seen.add(v)
            unique.append(v)
    return unique


def get_folder_path_from_db(project_id: str) -> Optional[str]:
    """
    Busca o folder_path do projeto no banco de dados.

    Issue #78: Sincronizacao project_id com nome da pasta

    Args:
        project_id: ID do projeto

    Returns:
        Caminho da pasta do projeto ou None se nao encontrado
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Project

        db = SessionLocal()
        try:
            project = db.query(Project).filter(
                Project.project_id == project_id
            ).first()
            if project and project.folder_path:
                return project.folder_path
        finally:
            db.close()
    except Exception:
        pass
    return None


class AppGenerator:
    """Gera aplicacoes testaveis a partir do codigo criado pelos workers."""

    PROJECTS_DIR = Path(r"C:\Users\lcruz\Plataforma E\projects")

    def __init__(self, project_id: str):
        self.project_id = project_id
        self.project_path = self._find_project_path()
        self.project_type = None
        self.detected_models = []
        self.detected_routes = []
        self.app_ready = False
        self.app_url = None

    def _find_project_path(self) -> Optional[Path]:
        """
        Encontra o caminho do projeto usando multiplas estrategias.

        Issue #78: Sincronizacao project_id com nome da pasta

        Estrategias (em ordem de prioridade):
        1. Busca folder_path no banco de dados
        2. Tenta variacoes do project_id como nome de pasta
        3. Busca parcial por pastas que contem o project_id
        """
        # Estrategia 1: Buscar no banco de dados
        db_folder_path = get_folder_path_from_db(self.project_id)
        if db_folder_path:
            path = Path(db_folder_path)
            if path.exists():
                return path

        # Estrategia 2: Tentar variacoes do project_id
        for name in get_project_folder_variations(self.project_id):
            path = self.PROJECTS_DIR / name
            if path.exists():
                return path

        # Estrategia 3: Busca parcial
        if self.PROJECTS_DIR.exists():
            search_term = normalize_folder_name(self.project_id)
            base_search = re.sub(r'[-_]?\d{3,}$', '', search_term)

            for folder in self.PROJECTS_DIR.iterdir():
                if folder.is_dir():
                    folder_normalized = normalize_folder_name(folder.name)
                    if folder_normalized == search_term or \
                       search_term in folder_normalized or \
                       folder_normalized in search_term or \
                       (base_search and base_search in folder_normalized):
                        return folder

        return None

    def analyze_project(self) -> Dict:
        """Analisa o projeto e retorna informacoes sobre ele."""
        if not self.project_path or not self.project_path.exists():
            return {
                "status": "not_found",
                "message": "Pasta do projeto nao encontrada",
                "project_id": self.project_id,
                "ready_to_test": False
            }

        # Detectar tipo de projeto
        self.project_type = self._detect_project_type()

        # Analisar codigo
        analysis = {
            "status": "analyzed",
            "project_id": self.project_id,
            "project_path": str(self.project_path),
            "project_type": self.project_type,
            "files_count": self._count_files(),
            "models": [],
            "routes": [],
            "has_main": False,
            "has_requirements": False,
            "has_package_json": False,
            "ready_to_test": False,
            "app_url": None,
            "start_command": None,
            "message": ""
        }

        # Verificar arquivos de configuracao
        analysis["has_main"] = (self.project_path / "main.py").exists() or \
                               (self.project_path / "app.py").exists() or \
                               (self.project_path / "app" / "main.py").exists()
        analysis["has_requirements"] = (self.project_path / "requirements.txt").exists()
        analysis["has_package_json"] = (self.project_path / "package.json").exists()

        # Analisar codigo Python
        if self.project_type == "python":
            analysis["models"] = self._find_python_models()
            analysis["routes"] = self._find_python_routes()

            # Determinar se esta pronto para teste
            if analysis["has_main"]:
                analysis["ready_to_test"] = True
                analysis["start_command"] = "python main.py"
                analysis["app_url"] = "http://localhost:8000"
                analysis["message"] = "Aplicacao pronta para testar!"
            elif len(analysis["models"]) > 0:
                analysis["ready_to_test"] = False
                analysis["can_generate_app"] = True
                analysis["message"] = f"Encontrados {len(analysis['models'])} modelos. Posso gerar uma aplicacao automaticamente."
            else:
                analysis["message"] = "Projeto em fase inicial de desenvolvimento."

        # Analisar codigo Node.js (Issue #75)
        elif self.project_type == "nodejs":
            analysis["models"] = self.find_nodejs_models()
            analysis["routes"] = self._find_nodejs_routes()

            # Verificar arquivos de entrada Node.js
            has_index = (self.project_path / "index.js").exists() or \
                        (self.project_path / "app.js").exists() or \
                        (self.project_path / "server.js").exists() or \
                        (self.project_path / "src" / "index.js").exists() or \
                        (self.project_path / "src" / "app.js").exists()
            analysis["has_index"] = has_index

            # Determinar se esta pronto para teste
            if has_index and analysis["has_package_json"]:
                analysis["ready_to_test"] = True
                analysis["start_command"] = "npm start"
                analysis["app_url"] = "http://localhost:3000"
                analysis["message"] = "Aplicacao Node.js pronta para testar!"
            elif len(analysis["models"]) > 0:
                analysis["ready_to_test"] = False
                analysis["can_generate_app"] = True
                analysis["message"] = f"Encontrados {len(analysis['models'])} modelos Node.js. Posso gerar uma aplicacao Express automaticamente."
            else:
                analysis["can_generate_app"] = True
                analysis["message"] = "Projeto Node.js detectado. Posso gerar uma aplicacao Express de exemplo."

        return analysis

    def _detect_project_type(self) -> str:
        """Detecta o tipo de projeto baseado nos arquivos."""
        if not self.project_path:
            return "unknown"

        # Verificar arquivos Node.js primeiro (package.json tem prioridade)
        if self.detect_nodejs_project():
            return "nodejs"

        # Verificar arquivos Python
        py_files = list(self.project_path.rglob("*.py"))
        if py_files:
            return "python"

        # Verificar arquivos Node.js sem package.json
        js_files = list(self.project_path.rglob("*.js"))
        ts_files = list(self.project_path.rglob("*.ts"))
        if js_files or ts_files:
            return "nodejs"

        return "unknown"

    def detect_nodejs_project(self) -> bool:
        """
        Detecta se o projeto e Node.js baseado em arquivos caracteristicos.

        Verifica:
        - package.json
        - node_modules/
        - Arquivos .js ou .ts com imports de express, sequelize, typeorm, mongoose

        Returns:
            True se for um projeto Node.js, False caso contrario
        """
        if not self.project_path:
            return False

        # Verificar package.json
        package_json = self.project_path / "package.json"
        if package_json.exists():
            try:
                content = json.loads(package_json.read_text(encoding="utf-8"))
                deps = {**content.get("dependencies", {}), **content.get("devDependencies", {})}
                # Verificar dependencias tipicas de backend Node.js
                node_deps = ["express", "fastify", "koa", "hapi", "sequelize", "typeorm", "mongoose", "prisma"]
                if any(dep in deps for dep in node_deps):
                    return True
            except Exception:
                pass
            return True  # package.json existe, provavelmente e Node.js

        # Verificar node_modules
        if (self.project_path / "node_modules").exists():
            return True

        # Verificar arquivos JS/TS com imports caracteristicos
        for pattern in ["*.js", "*.ts"]:
            for file in self.project_path.rglob(pattern):
                if "node_modules" in str(file):
                    continue
                try:
                    content = file.read_text(encoding="utf-8", errors="ignore")
                    if any(imp in content for imp in ["require('express')", "from 'express'",
                                                       "require('sequelize')", "from 'sequelize'",
                                                       "require('mongoose')", "from 'mongoose'",
                                                       "require('typeorm')", "from 'typeorm'"]):
                        return True
                except Exception:
                    pass

        return False

    def _count_files(self) -> Dict[str, int]:
        """Conta arquivos por tipo."""
        if not self.project_path:
            return {}

        counts = {}
        for ext in [".py", ".js", ".ts", ".jsx", ".tsx", ".html", ".css", ".json"]:
            files = list(self.project_path.rglob(f"*{ext}"))
            if files:
                counts[ext] = len(files)
        return counts

    def _find_python_models(self) -> List[Dict]:
        """Encontra modelos SQLAlchemy/Pydantic no codigo."""
        models = []
        if not self.project_path:
            return models

        for py_file in self.project_path.rglob("*.py"):
            try:
                content = py_file.read_text(encoding="utf-8", errors="ignore")

                # Buscar classes que herdam de Base (SQLAlchemy)
                sqlalchemy_pattern = r"class\s+(\w+)\s*\(\s*(?:Base|db\.Model)\s*\)"
                for match in re.finditer(sqlalchemy_pattern, content):
                    model_name = match.group(1)
                    if model_name not in ["Base"]:
                        # Extrair campos
                        fields = self._extract_model_fields(content, model_name)
                        models.append({
                            "name": model_name,
                            "type": "sqlalchemy",
                            "file": str(py_file.relative_to(self.project_path)),
                            "fields": fields
                        })

                # Buscar classes Pydantic
                pydantic_pattern = r"class\s+(\w+)\s*\(\s*(?:BaseModel|BaseSchema)\s*\)"
                for match in re.finditer(pydantic_pattern, content):
                    model_name = match.group(1)
                    models.append({
                        "name": model_name,
                        "type": "pydantic",
                        "file": str(py_file.relative_to(self.project_path)),
                        "fields": []
                    })

            except Exception:
                pass

        return models

    def _extract_model_fields(self, content: str, model_name: str) -> List[str]:
        """Extrai campos de um modelo SQLAlchemy."""
        fields = []
        # Padrao para campos Column
        pattern = r"(\w+)\s*=\s*Column\s*\("
        for match in re.finditer(pattern, content):
            field_name = match.group(1)
            if not field_name.startswith("_"):
                fields.append(field_name)
        return fields[:10]  # Limitar a 10 campos

    def _find_python_routes(self) -> List[Dict]:
        """Encontra rotas FastAPI/Flask no codigo."""
        routes = []
        if not self.project_path:
            return routes

        for py_file in self.project_path.rglob("*.py"):
            try:
                content = py_file.read_text(encoding="utf-8", errors="ignore")

                # FastAPI routes
                fastapi_pattern = r"@(?:app|router)\.(get|post|put|delete|patch)\s*\(\s*['\"]([^'\"]+)['\"]"
                for match in re.finditer(fastapi_pattern, content):
                    routes.append({
                        "method": match.group(1).upper(),
                        "path": match.group(2),
                        "file": str(py_file.relative_to(self.project_path))
                    })

                # Flask routes
                flask_pattern = r"@(?:app|blueprint)\.(route)\s*\(\s*['\"]([^'\"]+)['\"]"
                for match in re.finditer(flask_pattern, content):
                    routes.append({
                        "method": "GET",
                        "path": match.group(2),
                        "file": str(py_file.relative_to(self.project_path))
                    })

            except Exception:
                pass

        return routes

    # ============================================================
    # NODE.JS/EXPRESS SUPPORT (Issue #75)
    # ============================================================

    def find_nodejs_models(self) -> List[Dict]:
        """
        Encontra modelos Node.js no codigo (Sequelize, TypeORM, Mongoose).

        Detecta:
        - Sequelize: Model.init(), sequelize.define(), extends Model
        - TypeORM: @Entity(), extends BaseEntity
        - Mongoose: mongoose.Schema, mongoose.model()

        Returns:
            Lista de modelos encontrados com nome, tipo, arquivo e campos
        """
        models = []
        if not self.project_path:
            return models

        # Buscar em arquivos .js e .ts, ignorando node_modules
        for pattern in ["**/*.js", "**/*.ts"]:
            for file in self.project_path.glob(pattern):
                if "node_modules" in str(file):
                    continue
                try:
                    content = file.read_text(encoding="utf-8", errors="ignore")
                    relative_path = str(file.relative_to(self.project_path))

                    # Detectar modelos Sequelize
                    sequelize_models = self._find_sequelize_models(content, relative_path)
                    models.extend(sequelize_models)

                    # Detectar modelos TypeORM
                    typeorm_models = self._find_typeorm_models(content, relative_path)
                    models.extend(typeorm_models)

                    # Detectar modelos Mongoose
                    mongoose_models = self._find_mongoose_models(content, relative_path)
                    models.extend(mongoose_models)

                except Exception:
                    pass

        return models

    def _find_sequelize_models(self, content: str, file_path: str) -> List[Dict]:
        """Encontra modelos Sequelize no codigo."""
        models = []

        # Padrao 1: class User extends Model
        pattern1 = r"class\s+(\w+)\s+extends\s+(?:Model|Sequelize\.Model)"
        for match in re.finditer(pattern1, content):
            model_name = match.group(1)
            fields = self._extract_sequelize_fields(content)
            models.append({
                "name": model_name,
                "type": "sequelize",
                "orm_type": "sequelize",
                "file": file_path,
                "fields": fields
            })

        # Padrao 2: sequelize.define('User', {...})
        pattern2 = r"sequelize\.define\s*\(\s*['\"](\w+)['\"]"
        for match in re.finditer(pattern2, content):
            model_name = match.group(1)
            fields = self._extract_sequelize_fields(content)
            if not any(m["name"] == model_name for m in models):
                models.append({
                    "name": model_name,
                    "type": "sequelize",
                    "orm_type": "sequelize",
                    "file": file_path,
                    "fields": fields
                })

        # Padrao 3: ModelName.init({...}, {...})
        pattern3 = r"(\w+)\.init\s*\(\s*\{"
        for match in re.finditer(pattern3, content):
            model_name = match.group(1)
            if model_name not in ["Model", "Sequelize"] and not any(m["name"] == model_name for m in models):
                fields = self._extract_sequelize_fields(content)
                models.append({
                    "name": model_name,
                    "type": "sequelize",
                    "orm_type": "sequelize",
                    "file": file_path,
                    "fields": fields
                })

        return models

    def _extract_sequelize_fields(self, content: str) -> List[str]:
        """Extrai campos de um modelo Sequelize."""
        fields = []
        # Buscar campos no formato: fieldName: { type: DataTypes.STRING }
        pattern = r"(\w+)\s*:\s*\{\s*type\s*:"
        for match in re.finditer(pattern, content):
            field_name = match.group(1)
            if field_name not in ["type", "Model", "Sequelize"] and not field_name.startswith("_"):
                fields.append(field_name)
        return fields[:10]

    def _find_typeorm_models(self, content: str, file_path: str) -> List[Dict]:
        """Encontra modelos TypeORM no codigo."""
        models = []

        # Padrao 1: @Entity() class User
        pattern1 = r"@Entity\s*\([^)]*\)\s*(?:export\s+)?class\s+(\w+)"
        for match in re.finditer(pattern1, content):
            model_name = match.group(1)
            fields = self._extract_typeorm_fields(content)
            models.append({
                "name": model_name,
                "type": "typeorm",
                "orm_type": "typeorm",
                "file": file_path,
                "fields": fields
            })

        # Padrao 2: class User extends BaseEntity
        pattern2 = r"class\s+(\w+)\s+extends\s+BaseEntity"
        for match in re.finditer(pattern2, content):
            model_name = match.group(1)
            if not any(m["name"] == model_name for m in models):
                fields = self._extract_typeorm_fields(content)
                models.append({
                    "name": model_name,
                    "type": "typeorm",
                    "orm_type": "typeorm",
                    "file": file_path,
                    "fields": fields
                })

        return models

    def _extract_typeorm_fields(self, content: str) -> List[str]:
        """Extrai campos de um modelo TypeORM."""
        fields = []
        # Buscar campos com decorators @Column, @PrimaryColumn, @PrimaryGeneratedColumn
        pattern = r"@(?:Column|PrimaryColumn|PrimaryGeneratedColumn)\s*\([^)]*\)\s*(\w+)\s*[:\!]"
        for match in re.finditer(pattern, content):
            field_name = match.group(1)
            if not field_name.startswith("_"):
                fields.append(field_name)
        return fields[:10]

    def _find_mongoose_models(self, content: str, file_path: str) -> List[Dict]:
        """Encontra modelos Mongoose no codigo."""
        models = []

        # Padrao 1: mongoose.model('User', userSchema)
        pattern1 = r"mongoose\.model\s*\(\s*['\"](\w+)['\"]"
        for match in re.finditer(pattern1, content):
            model_name = match.group(1)
            fields = self._extract_mongoose_fields(content)
            models.append({
                "name": model_name,
                "type": "mongoose",
                "orm_type": "mongoose",
                "file": file_path,
                "fields": fields
            })

        # Padrao 2: const UserSchema = new Schema({...}) ou new mongoose.Schema({...})
        pattern2 = r"const\s+(\w+)Schema\s*=\s*new\s+(?:mongoose\.)?Schema\s*\("
        for match in re.finditer(pattern2, content):
            schema_name = match.group(1)
            model_name = schema_name.replace("Schema", "").replace("schema", "")
            if model_name and not any(m["name"] == model_name for m in models):
                fields = self._extract_mongoose_fields(content)
                models.append({
                    "name": model_name,
                    "type": "mongoose",
                    "orm_type": "mongoose",
                    "file": file_path,
                    "fields": fields
                })

        # Padrao 3: export const User = model('User', ...)
        pattern3 = r"(?:export\s+)?(?:const|let|var)\s+(\w+)\s*=\s*model\s*\(\s*['\"](\w+)['\"]"
        for match in re.finditer(pattern3, content):
            model_name = match.group(2)
            if not any(m["name"] == model_name for m in models):
                fields = self._extract_mongoose_fields(content)
                models.append({
                    "name": model_name,
                    "type": "mongoose",
                    "orm_type": "mongoose",
                    "file": file_path,
                    "fields": fields
                })

        return models

    def _extract_mongoose_fields(self, content: str) -> List[str]:
        """Extrai campos de um modelo Mongoose."""
        fields = []
        # Buscar campos no formato do Schema: fieldName: { type: String }
        pattern = r"(\w+)\s*:\s*(?:\{|String|Number|Boolean|Date|ObjectId|Array|Mixed)"
        for match in re.finditer(pattern, content):
            field_name = match.group(1)
            if field_name not in ["type", "required", "default", "ref", "unique", "index"] and not field_name.startswith("_"):
                fields.append(field_name)
        return list(set(fields))[:10]

    def _find_nodejs_routes(self) -> List[Dict]:
        """Encontra rotas Express/Node.js no codigo."""
        routes = []
        if not self.project_path:
            return routes

        for pattern in ["**/*.js", "**/*.ts"]:
            for file in self.project_path.glob(pattern):
                if "node_modules" in str(file):
                    continue
                try:
                    content = file.read_text(encoding="utf-8", errors="ignore")
                    relative_path = str(file.relative_to(self.project_path))

                    # Express routes: router.get('/path', ...) ou app.get('/path', ...)
                    express_pattern = r"(?:router|app)\.(get|post|put|delete|patch)\s*\(\s*['\"]([^'\"]+)['\"]"
                    for match in re.finditer(express_pattern, content):
                        routes.append({
                            "method": match.group(1).upper(),
                            "path": match.group(2),
                            "file": relative_path
                        })

                except Exception:
                    pass

        return routes

    def generate_testable_app(self) -> Dict:
        """Gera uma aplicacao testavel baseada no codigo existente."""
        analysis = self.analyze_project()

        if analysis["status"] == "not_found":
            return {
                "success": False,
                "message": "Projeto nao encontrado"
            }

        if analysis.get("ready_to_test"):
            return {
                "success": True,
                "message": "Aplicacao ja esta pronta para testar",
                "app_url": analysis["app_url"],
                "start_command": analysis["start_command"]
            }

        if self.project_type == "python" and analysis.get("models"):
            return self._generate_python_app(analysis["models"])

        # Node.js/Express support (Issue #75)
        if self.project_type == "nodejs":
            return self._generate_nodejs_app(analysis.get("models", []))

        return {
            "success": False,
            "message": "Nao foi possivel gerar aplicacao automaticamente. Projeto ainda em desenvolvimento."
        }

    def _generate_python_app(self, models: List[Dict]) -> Dict:
        """Gera uma aplicacao FastAPI baseada nos modelos encontrados."""
        if not self.project_path:
            return {"success": False, "message": "Caminho do projeto nao encontrado"}

        app_dir = self.project_path / "app"
        app_dir.mkdir(exist_ok=True)

        # Gerar main.py
        main_content = self._generate_fastapi_main(models)
        main_file = self.project_path / "main.py"
        main_file.write_text(main_content, encoding="utf-8")

        # Gerar requirements.txt se nao existir
        req_file = self.project_path / "requirements.txt"
        if not req_file.exists():
            req_content = """fastapi==0.104.1
uvicorn==0.24.0
sqlalchemy==2.0.23
pydantic==2.5.2
"""
            req_file.write_text(req_content, encoding="utf-8")

        # Gerar script de inicializacao simples
        start_script = self.project_path / "iniciar_app.bat"
        start_content = f"""@echo off
echo ========================================
echo   Iniciando Aplicacao - {self.project_id}
echo ========================================
echo.
echo Instalando dependencias...
pip install -r requirements.txt -q
echo.
echo Iniciando servidor...
echo Acesse: http://localhost:8000/docs
echo.
python main.py
pause
"""
        start_script.write_text(start_content, encoding="utf-8")

        return {
            "success": True,
            "message": f"Aplicacao gerada com sucesso! {len(models)} modelos encontrados.",
            "app_url": "http://localhost:8000",
            "docs_url": "http://localhost:8000/docs",
            "start_command": "python main.py",
            "start_script": str(start_script),
            "files_created": [
                str(main_file),
                str(req_file) if not req_file.exists() else None,
                str(start_script)
            ]
        }

    def _generate_fastapi_main(self, models: List[Dict]) -> str:
        """Gera o arquivo main.py do FastAPI."""
        model_names = [m["name"] for m in models if m["type"] == "sqlalchemy"]

        # Gerar imports dos modelos
        model_imports = ""
        for model in models:
            if model["type"] == "sqlalchemy":
                file_path = model["file"].replace("\\", "/").replace(".py", "").replace("/", ".")
                model_imports += f"# from {file_path} import {model['name']}\n"

        content = f'''"""
Aplicacao de Teste - {self.project_id}
=====================================
Gerada automaticamente pela Plataforma E
Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}

Esta aplicacao permite testar os modelos e funcionalidades
desenvolvidas pelos workers.
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import List, Optional
from datetime import datetime
import uvicorn

# Imports dos modelos (ajuste conforme necessario)
{model_imports}

app = FastAPI(
    title="{self.project_id} - Aplicacao de Teste",
    description="Aplicacao gerada automaticamente para testes",
    version="1.0.0"
)

# CORS para permitir acesso do dashboard
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================================
# PAGINA INICIAL
# ============================================================

@app.get("/", response_class=HTMLResponse)
def home():
    """Pagina inicial com informacoes do projeto."""
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>{self.project_id} - Teste</title>
        <style>
            body {{ font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }}
            h1 {{ color: #003B4A; }}
            .card {{ background: #f5f5f5; padding: 20px; border-radius: 8px; margin: 20px 0; }}
            .btn {{ display: inline-block; padding: 10px 20px; background: #FF6C00; color: white;
                   text-decoration: none; border-radius: 5px; margin: 5px; }}
            .btn:hover {{ background: #e65c00; }}
            .success {{ color: #10B981; }}
            ul {{ line-height: 2; }}
        </style>
    </head>
    <body>
        <h1>{self.project_id}</h1>
        <p class="success">Aplicacao rodando com sucesso!</p>

        <div class="card">
            <h2>Modelos Disponiveis</h2>
            <ul>
                {"".join(f"<li><strong>{m}</strong></li>" for m in model_names) if model_names else "<li>Nenhum modelo encontrado</li>"}
            </ul>
        </div>

        <div class="card">
            <h2>Links Uteis</h2>
            <a href="/docs" class="btn">Documentacao API</a>
            <a href="/health" class="btn">Health Check</a>
        </div>

        <div class="card">
            <h2>Como Testar</h2>
            <ol>
                <li>Acesse a <a href="/docs">Documentacao da API</a></li>
                <li>Explore os endpoints disponiveis</li>
                <li>Use o botao "Try it out" para testar cada funcionalidade</li>
                <li>Reporte problemas no chat do Dashboard</li>
            </ol>
        </div>

        <p style="color: #666; font-size: 12px; margin-top: 40px;">
            Gerado automaticamente pela Plataforma E em {datetime.now().strftime("%d/%m/%Y %H:%M")}
        </p>
    </body>
    </html>
    """

# ============================================================
# HEALTH CHECK
# ============================================================

@app.get("/health")
def health_check():
    """Verifica se a aplicacao esta funcionando."""
    return {{
        "status": "healthy",
        "project": "{self.project_id}",
        "timestamp": datetime.now().isoformat(),
        "models_count": {len(model_names)}
    }}

# ============================================================
# ENDPOINTS DE EXEMPLO
# ============================================================

class ItemBase(BaseModel):
    """Modelo base de exemplo."""
    name: str
    description: Optional[str] = None

class ItemResponse(ItemBase):
    id: int
    created_at: datetime

# Dados de exemplo em memoria
sample_items = [
    {{"id": 1, "name": "Item 1", "description": "Primeiro item de exemplo", "created_at": datetime.now()}},
    {{"id": 2, "name": "Item 2", "description": "Segundo item de exemplo", "created_at": datetime.now()}},
]

@app.get("/api/items", response_model=List[dict])
def list_items():
    """Lista todos os itens de exemplo."""
    return sample_items

@app.get("/api/items/{{item_id}}")
def get_item(item_id: int):
    """Busca um item pelo ID."""
    for item in sample_items:
        if item["id"] == item_id:
            return item
    raise HTTPException(status_code=404, detail="Item nao encontrado")

@app.post("/api/items")
def create_item(item: ItemBase):
    """Cria um novo item."""
    new_id = max(i["id"] for i in sample_items) + 1 if sample_items else 1
    new_item = {{
        "id": new_id,
        "name": item.name,
        "description": item.description,
        "created_at": datetime.now()
    }}
    sample_items.append(new_item)
    return new_item

# ============================================================
# INICIALIZACAO
# ============================================================

if __name__ == "__main__":
    print("=" * 50)
    print(f"  {self.project_id} - Aplicacao de Teste")
    print("=" * 50)
    print(f"  Acesse: http://localhost:8000")
    print(f"  Docs:   http://localhost:8000/docs")
    print("=" * 50)
    uvicorn.run(app, host="0.0.0.0", port=8000)
'''
        return content

    # ============================================================
    # NODE.JS/EXPRESS APP GENERATION (Issue #75)
    # ============================================================

    def _generate_nodejs_app(self, models: List[Dict]) -> Dict:
        """Gera uma aplicacao Express.js baseada nos modelos encontrados."""
        if not self.project_path:
            return {"success": False, "message": "Caminho do projeto nao encontrado"}

        # Gerar app.js
        app_content = self._generate_express_main(models)
        app_file = self.project_path / "app.js"
        app_file.write_text(app_content, encoding="utf-8")

        # Gerar package.json se nao existir
        package_file = self.project_path / "package.json"
        package_created = False
        if not package_file.exists():
            package_content = self._generate_package_json(models)
            package_file.write_text(package_content, encoding="utf-8")
            package_created = True

        # Gerar script de inicializacao para Windows
        start_script = self.project_path / "iniciar_app.bat"
        start_bat = f"""@echo off
echo ========================================
echo   Iniciando Aplicacao Node.js - {self.project_id}
echo ========================================
echo.
echo Instalando dependencias...
call npm install
echo.
echo Iniciando servidor...
echo Acesse: http://localhost:3000
echo.
node app.js
pause
"""
        start_script.write_text(start_bat, encoding="utf-8")

        return {
            "success": True,
            "message": f"Aplicacao Express.js gerada com sucesso! {len(models)} modelos encontrados.",
            "app_url": "http://localhost:3000",
            "docs_url": "http://localhost:3000/api-docs",
            "start_command": "npm start",
            "start_script": str(start_script),
            "files_created": [str(app_file), str(package_file) if package_created else None, str(start_script)]
        }

    def _generate_package_json(self, models: List[Dict]) -> str:
        """Gera o arquivo package.json para o projeto Express."""
        orm_deps = {}
        for model in models:
            orm_type = model.get("orm_type", model.get("type", ""))
            if orm_type == "sequelize":
                orm_deps["sequelize"] = "^6.35.0"
                orm_deps["sqlite3"] = "^5.1.6"
            elif orm_type == "typeorm":
                orm_deps["typeorm"] = "^0.3.17"
                orm_deps["reflect-metadata"] = "^0.1.13"
            elif orm_type == "mongoose":
                orm_deps["mongoose"] = "^8.0.0"

        package = {
            "name": self.project_id.lower().replace(" ", "-").replace("_", "-"),
            "version": "1.0.0",
            "description": "Aplicacao de teste gerada pela Plataforma E",
            "main": "app.js",
            "scripts": {"start": "node app.js", "dev": "nodemon app.js"},
            "dependencies": {"express": "^4.18.2", "cors": "^2.8.5", "swagger-ui-express": "^5.0.0", "swagger-jsdoc": "^6.2.8", **orm_deps},
            "devDependencies": {"nodemon": "^3.0.1"},
            "author": "Plataforma E",
            "license": "MIT"
        }
        return json.dumps(package, indent=2, ensure_ascii=False)

    def _generate_express_main(self, models: List[Dict]) -> str:
        """Gera o arquivo app.js do Express com CRUD endpoints."""
        model_names = [m["name"] for m in models]
        routes_code = self._generate_express_routes(models)
        sample_data = self._generate_sample_data(models)

        return f'''/**
 * Aplicacao de Teste - {self.project_id}
 * Gerada automaticamente pela Plataforma E - {datetime.now().strftime("%Y-%m-%d %H:%M")}
 */
const express = require('express');
const cors = require('cors');
const swaggerUi = require('swagger-ui-express');
const swaggerJsdoc = require('swagger-jsdoc');

const app = express();
const PORT = process.env.PORT || 3000;

app.use(cors());
app.use(express.json());

const swaggerDocs = swaggerJsdoc({{
    definition: {{
        openapi: '3.0.0',
        info: {{ title: '{self.project_id} API', version: '1.0.0' }},
        servers: [{{ url: 'http://localhost:' + PORT }}]
    }},
    apis: ['./app.js']
}});
app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(swaggerDocs));

{sample_data}

app.get('/', (req, res) => res.send('<h1>{self.project_id}</h1><p>API rodando!</p><a href="/api-docs">Swagger Docs</a>'));
app.get('/health', (req, res) => res.json({{ status: 'healthy', project: '{self.project_id}' }}));

{routes_code}

app.listen(PORT, () => console.log('Servidor rodando em http://localhost:' + PORT));
module.exports = app;
'''

    def _generate_express_routes(self, models: List[Dict]) -> str:
        """Gera rotas CRUD Express para cada modelo."""
        if not models:
            return "app.get('/api/items', (req, res) => res.json({ data: itemData }));"
        routes = ""
        for m in models:
            n = m["name"].lower()
            routes += f"app.get('/api/{n}s', (req, res) => res.json({{ data: {n}Data }}));\n"
            routes += f"app.get('/api/{n}s/:id', (req, res) => {{ const i = {n}Data.find(x => x.id === +req.params.id); i ? res.json(i) : res.status(404).json({{error: 'Not found'}}); }});\n"
            routes += f"app.post('/api/{n}s', (req, res) => {{ const i = {{id: {n}Data.length+1, ...req.body}}; {n}Data.push(i); res.status(201).json(i); }});\n"
            routes += f"app.put('/api/{n}s/:id', (req, res) => {{ const idx = {n}Data.findIndex(x => x.id === +req.params.id); if(idx===-1) return res.status(404).json({{error:'Not found'}}); {n}Data[idx] = {{...{n}Data[idx], ...req.body}}; res.json({n}Data[idx]); }});\n"
            routes += f"app.delete('/api/{n}s/:id', (req, res) => {{ const idx = {n}Data.findIndex(x => x.id === +req.params.id); if(idx===-1) return res.status(404).json({{error:'Not found'}}); {n}Data.splice(idx,1); res.json({{success:true}}); }});\n"
        return routes

    def _generate_sample_data(self, models: List[Dict]) -> str:
        """Gera dados de exemplo para os modelos."""
        if not models:
            return "let itemData = [{ id: 1, name: 'Exemplo' }];"
        return "\n".join([f"let {m['name'].lower()}Data = [{{id: 1, name: 'Exemplo 1'}}, {{id: 2, name: 'Exemplo 2'}}];" for m in models])

    def _start_nodejs_app(self) -> Dict:
        """Inicia uma aplicacao Node.js."""
        app_file = self.project_path / "app.js"
        if not app_file.exists():
            return {"success": False, "message": "Arquivo app.js nao encontrado"}
        try:
            process = subprocess.Popen(
                ["node", str(app_file)],
                cwd=str(self.project_path),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                creationflags=subprocess.CREATE_NEW_CONSOLE if os.name == 'nt' else 0
            )
            return {"success": True, "message": "Aplicacao Node.js iniciada!", "app_url": "http://localhost:3000", "pid": process.pid}
        except Exception as e:
            return {"success": False, "message": f"Erro: {str(e)}"}

    def start_app(self) -> Dict:
        """Inicia a aplicacao para teste."""
        analysis = self.analyze_project()

        if not analysis.get("ready_to_test"):
            gen_result = self.generate_testable_app()
            if not gen_result.get("success"):
                return gen_result

        if self.project_type == "python":
            return self._start_python_app()
        if self.project_type == "nodejs":
            return self._start_nodejs_app()

        return {"success": False, "message": "Tipo de projeto nao suportado para inicio automatico"}

    def _start_python_app(self) -> Dict:
        """Inicia uma aplicacao Python."""
        main_file = self.project_path / "main.py"
        if not main_file.exists():
            return {
                "success": False,
                "message": "Arquivo main.py nao encontrado"
            }

        try:
            # Iniciar em background
            process = subprocess.Popen(
                ["python", str(main_file)],
                cwd=str(self.project_path),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                creationflags=subprocess.CREATE_NEW_CONSOLE if os.name == 'nt' else 0
            )

            return {
                "success": True,
                "message": "Aplicacao iniciada com sucesso!",
                "app_url": "http://localhost:8000",
                "docs_url": "http://localhost:8000/docs",
                "pid": process.pid
            }

        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao iniciar aplicacao: {str(e)}"
            }


# Funcoes utilitarias para uso direto
def analyze_project(project_id: str) -> Dict:
    """Analisa um projeto e retorna seu status."""
    generator = AppGenerator(project_id)
    return generator.analyze_project()


def generate_app(project_id: str) -> Dict:
    """Gera uma aplicacao testavel para o projeto."""
    generator = AppGenerator(project_id)
    return generator.generate_testable_app()


def start_app(project_id: str) -> Dict:
    """Inicia a aplicacao do projeto para teste."""
    generator = AppGenerator(project_id)
    return generator.start_app()


# ============================================================
# INTEGRACAO COM FRONTEND GENERATOR (Issue #76)
# ============================================================

def generate_frontend(project_id: str, framework: str = "react") -> Dict:
    """
    Gera frontend React ou Vue para um projeto.

    Args:
        project_id: ID do projeto
        framework: 'react' ou 'vue'

    Returns:
        Resultado da geracao do frontend

    Exemplo:
        >>> result = generate_frontend("BELGO-BPM-001", "react")
        >>> print(result["app_url"])  # http://localhost:5173
    """
    from factory.core.frontend_generator import FrontendGenerator

    generator = AppGenerator(project_id)
    analysis = generator.analyze_project()

    if analysis["status"] == "not_found":
        return {"success": False, "message": "Projeto nao encontrado"}

    models = analysis.get("models", [])
    project_path = analysis.get("project_path")

    frontend_gen = FrontendGenerator(project_path, models, project_id)

    if framework.lower() == "vue":
        return frontend_gen.generate_vue_app()
    else:
        return frontend_gen.generate_react_app()


def detect_frontend(project_id: str) -> Dict:
    """
    Detecta frontend existente em um projeto.

    Args:
        project_id: ID do projeto

    Returns:
        Informacoes sobre o frontend detectado

    Exemplo:
        >>> info = detect_frontend("gestao-estrategica")
        >>> print(info["frontend_type"])  # "react"
        >>> print(info["components_count"])  # 45
    """
    from factory.core.frontend_generator import FrontendGenerator

    generator = AppGenerator(project_id)
    if not generator.project_path:
        return {"frontend_type": None, "components_count": 0, "message": "Projeto nao encontrado"}

    frontend_gen = FrontendGenerator(str(generator.project_path))
    frontend_type = frontend_gen.detect_frontend_type()

    if frontend_type == "react":
        components = frontend_gen.detect_react_components()
    elif frontend_type == "vue":
        components = frontend_gen.detect_vue_components()
    else:
        components = []

    return {
        "project_id": project_id,
        "frontend_type": frontend_type,
        "components_count": len(components),
        "components": components
    }
