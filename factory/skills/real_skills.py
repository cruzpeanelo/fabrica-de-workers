"""
Skills REAIS para os Agentes
=============================

Este modulo contem skills que REALMENTE executam acoes:
- Criam arquivos de codigo
- Leem e processam documentos
- Executam comandos
- Registram aprendizado

Cada skill registra seu resultado na memoria do agente para aprendizado futuro.
"""

import os
import json
import shutil
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field, asdict

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import SessionLocal
from factory.database.models import Agent, Story, Project, ActivityLog, Task


@dataclass
class SkillResult:
    """Resultado de uma skill executada"""
    success: bool
    skill_name: str
    agent_id: str
    outputs: Dict = field(default_factory=dict)
    files_created: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    learning_notes: List[str] = field(default_factory=list)
    execution_time_ms: int = 0


class AgentMemory:
    """
    Sistema de memoria e aprendizado dos agentes

    Cada agente tem:
    - Historico de tarefas executadas
    - Conhecimentos adquiridos
    - Padroes aprendidos
    - Skills dominadas
    """

    MEMORY_PATH = Path(__file__).parent.parent / "memory"

    def __init__(self, agent_id: str):
        self.agent_id = agent_id
        self.memory_file = self.MEMORY_PATH / f"{agent_id}_memory.json"
        self.MEMORY_PATH.mkdir(parents=True, exist_ok=True)
        self.memory = self._load_memory()

    def _load_memory(self) -> Dict:
        """Carrega memoria do agente"""
        if self.memory_file.exists():
            try:
                with open(self.memory_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
            except:
                pass

        return {
            "agent_id": self.agent_id,
            "created_at": datetime.utcnow().isoformat(),
            "skills_executed": {},
            "knowledge": [],
            "patterns_learned": [],
            "files_created": [],
            "errors_encountered": [],
            "total_tasks": 0,
            "successful_tasks": 0
        }

    def _save_memory(self):
        """Salva memoria do agente"""
        self.memory["updated_at"] = datetime.utcnow().isoformat()
        with open(self.memory_file, 'w', encoding='utf-8') as f:
            json.dump(self.memory, f, indent=2, ensure_ascii=False)

    def record_skill_execution(self, result: SkillResult):
        """Registra execucao de uma skill"""
        skill_name = result.skill_name

        # Atualiza contadores
        if skill_name not in self.memory["skills_executed"]:
            self.memory["skills_executed"][skill_name] = {
                "count": 0,
                "successes": 0,
                "failures": 0,
                "last_executed": None
            }

        self.memory["skills_executed"][skill_name]["count"] += 1
        self.memory["skills_executed"][skill_name]["last_executed"] = datetime.utcnow().isoformat()

        if result.success:
            self.memory["skills_executed"][skill_name]["successes"] += 1
            self.memory["successful_tasks"] += 1
        else:
            self.memory["skills_executed"][skill_name]["failures"] += 1

        self.memory["total_tasks"] += 1

        # Registra arquivos criados
        for f in result.files_created:
            if f not in self.memory["files_created"]:
                self.memory["files_created"].append(f)

        # Registra aprendizados
        for note in result.learning_notes:
            if note not in self.memory["knowledge"]:
                self.memory["knowledge"].append(note)

        # Registra erros para aprender com eles
        for error in result.errors:
            self.memory["errors_encountered"].append({
                "skill": skill_name,
                "error": error,
                "timestamp": datetime.utcnow().isoformat()
            })

        self._save_memory()

    def add_knowledge(self, knowledge: str):
        """Adiciona conhecimento ao agente"""
        if knowledge not in self.memory["knowledge"]:
            self.memory["knowledge"].append(knowledge)
            self._save_memory()

    def add_pattern(self, pattern: Dict):
        """Adiciona padrao aprendido"""
        self.memory["patterns_learned"].append({
            **pattern,
            "learned_at": datetime.utcnow().isoformat()
        })
        self._save_memory()

    def get_skill_proficiency(self, skill_name: str) -> float:
        """Retorna proficiencia em uma skill (0-100%)"""
        if skill_name not in self.memory["skills_executed"]:
            return 0.0

        skill_data = self.memory["skills_executed"][skill_name]
        if skill_data["count"] == 0:
            return 0.0

        return (skill_data["successes"] / skill_data["count"]) * 100

    def get_summary(self) -> Dict:
        """Retorna resumo da memoria do agente"""
        return {
            "agent_id": self.agent_id,
            "total_tasks": self.memory["total_tasks"],
            "success_rate": (self.memory["successful_tasks"] / self.memory["total_tasks"] * 100) if self.memory["total_tasks"] > 0 else 0,
            "skills_count": len(self.memory["skills_executed"]),
            "knowledge_items": len(self.memory["knowledge"]),
            "files_created": len(self.memory["files_created"]),
            "top_skills": sorted(
                self.memory["skills_executed"].items(),
                key=lambda x: x[1]["count"],
                reverse=True
            )[:5]
        }


class RealSkills:
    """
    Skills REAIS que executam acoes de verdade
    """

    # Templates de codigo
    TEMPLATES = {
        "fastapi_router": '''"""
Router para {name}
Gerado automaticamente pela Plataforma E
"""

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from typing import List, Optional
from database import get_db
from models.{model_name} import {model_class}
from schemas.{schema_name} import {schema_class}Create, {schema_class}Update, {schema_class}Response

router = APIRouter(prefix="/api/{route_prefix}", tags=["{tag}"])


@router.get("/", response_model=List[{schema_class}Response])
def list_{route_prefix}(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    """Lista todos os {name}"""
    items = db.query({model_class}).offset(skip).limit(limit).all()
    return items


@router.get("/{{item_id}}", response_model={schema_class}Response)
def get_{route_prefix}(item_id: int, db: Session = Depends(get_db)):
    """Busca {name} por ID"""
    item = db.query({model_class}).filter({model_class}.id == item_id).first()
    if not item:
        raise HTTPException(status_code=404, detail="{name} nao encontrado")
    return item


@router.post("/", response_model={schema_class}Response)
def create_{route_prefix}(data: {schema_class}Create, db: Session = Depends(get_db)):
    """Cria novo {name}"""
    item = {model_class}(**data.dict())
    db.add(item)
    db.commit()
    db.refresh(item)
    return item


@router.put("/{{item_id}}", response_model={schema_class}Response)
def update_{route_prefix}(item_id: int, data: {schema_class}Update, db: Session = Depends(get_db)):
    """Atualiza {name}"""
    item = db.query({model_class}).filter({model_class}.id == item_id).first()
    if not item:
        raise HTTPException(status_code=404, detail="{name} nao encontrado")

    for key, value in data.dict(exclude_unset=True).items():
        setattr(item, key, value)

    db.commit()
    db.refresh(item)
    return item


@router.delete("/{{item_id}}")
def delete_{route_prefix}(item_id: int, db: Session = Depends(get_db)):
    """Deleta {name}"""
    item = db.query({model_class}).filter({model_class}.id == item_id).first()
    if not item:
        raise HTTPException(status_code=404, detail="{name} nao encontrado")

    db.delete(item)
    db.commit()
    return {{"message": "{name} deletado com sucesso"}}
''',

        "sqlalchemy_model": '''"""
Modelo {name}
Gerado automaticamente pela Plataforma E
"""

from sqlalchemy import Column, Integer, String, Text, DateTime, Float, Boolean, ForeignKey, JSON
from sqlalchemy.orm import relationship
from datetime import datetime
from database import Base


class {class_name}(Base):
    """{description}"""
    __tablename__ = "{table_name}"

    id = Column(Integer, primary_key=True, autoincrement=True)
{columns}

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {{
            "id": self.id,
{to_dict_fields}
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }}
''',

        "pydantic_schema": '''"""
Schemas Pydantic para {name}
Gerado automaticamente pela Plataforma E
"""

from pydantic import BaseModel
from typing import Optional, List
from datetime import datetime


class {class_name}Base(BaseModel):
    """{description}"""
{fields}


class {class_name}Create({class_name}Base):
    """Schema para criacao"""
    pass


class {class_name}Update(BaseModel):
    """Schema para atualizacao"""
{optional_fields}


class {class_name}Response({class_name}Base):
    """Schema de resposta"""
    id: int
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None

    class Config:
        from_attributes = True
''',

        "vue_component": '''<template>
  <div class="{component_name}">
    <h2>{{ title }}</h2>

    <!-- Lista de items -->
    <div v-if="loading" class="loading">Carregando...</div>

    <div v-else class="items-list">
      <div v-for="item in items" :key="item.id" class="item-card">
        <h3>{{ item.name || item.title }}</h3>
        <p>{{ item.description }}</p>
        <div class="actions">
          <button @click="editItem(item)">Editar</button>
          <button @click="deleteItem(item.id)" class="danger">Excluir</button>
        </div>
      </div>
    </div>

    <!-- Botao adicionar -->
    <button @click="showForm = true" class="btn-add">+ Adicionar</button>

    <!-- Formulario -->
    <div v-if="showForm" class="form-modal">
      <form @submit.prevent="saveItem">
        <h3>{{ editingId ? 'Editar' : 'Novo' }} {name}</h3>
{form_fields}
        <div class="form-actions">
          <button type="submit">Salvar</button>
          <button type="button" @click="cancelForm">Cancelar</button>
        </div>
      </form>
    </div>
  </div>
</template>

<script setup>
import {{ ref, onMounted }} from 'vue'

const title = '{name}'
const items = ref([])
const loading = ref(true)
const showForm = ref(false)
const editingId = ref(null)
const formData = ref({{}})

const API_URL = '{api_url}'

const fetchItems = async () => {{
  loading.value = true
  try {{
    const response = await fetch(API_URL)
    items.value = await response.json()
  }} catch (error) {{
    console.error('Erro ao carregar:', error)
  }} finally {{
    loading.value = false
  }}
}}

const saveItem = async () => {{
  const method = editingId.value ? 'PUT' : 'POST'
  const url = editingId.value ? `${{API_URL}}/${{editingId.value}}` : API_URL

  try {{
    await fetch(url, {{
      method,
      headers: {{ 'Content-Type': 'application/json' }},
      body: JSON.stringify(formData.value)
    }})
    await fetchItems()
    cancelForm()
  }} catch (error) {{
    console.error('Erro ao salvar:', error)
  }}
}}

const editItem = (item) => {{
  editingId.value = item.id
  formData.value = {{ ...item }}
  showForm.value = true
}}

const deleteItem = async (id) => {{
  if (!confirm('Confirma exclusao?')) return

  try {{
    await fetch(`${{API_URL}}/${{id}}`, {{ method: 'DELETE' }})
    await fetchItems()
  }} catch (error) {{
    console.error('Erro ao deletar:', error)
  }}
}}

const cancelForm = () => {{
  showForm.value = false
  editingId.value = null
  formData.value = {{}}
}}

onMounted(fetchItems)
</script>

<style scoped>
.{component_name} {{
  padding: 20px;
}}

.items-list {{
  display: grid;
  gap: 16px;
}}

.item-card {{
  background: #f5f5f5;
  padding: 16px;
  border-radius: 8px;
}}

.btn-add {{
  margin-top: 20px;
  padding: 10px 20px;
  background: #007bff;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}}

.form-modal {{
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0,0,0,0.5);
  display: flex;
  align-items: center;
  justify-content: center;
}}

.form-modal form {{
  background: white;
  padding: 24px;
  border-radius: 8px;
  min-width: 400px;
}}

.danger {{
  background: #dc3545;
  color: white;
}}
</style>
''',

        "test_file": '''"""
Testes para {name}
Gerado automaticamente pela Plataforma E
"""

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from main import app
from database import Base, get_db

# Setup test database
SQLALCHEMY_DATABASE_URL = "sqlite:///./test.db"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={{"check_same_thread": False}})
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


def override_get_db():
    db = TestingSessionLocal()
    try:
        yield db
    finally:
        db.close()


app.dependency_overrides[get_db] = override_get_db
client = TestClient(app)


@pytest.fixture(autouse=True)
def setup_database():
    Base.metadata.create_all(bind=engine)
    yield
    Base.metadata.drop_all(bind=engine)


class Test{class_name}:
    """Testes para {name}"""

    def test_create_{route_prefix}(self):
        """Testa criacao de {name}"""
        response = client.post(
            "/api/{route_prefix}/",
            json={test_create_data}
        )
        assert response.status_code == 200
        data = response.json()
        assert "id" in data

    def test_list_{route_prefix}(self):
        """Testa listagem de {name}"""
        response = client.get("/api/{route_prefix}/")
        assert response.status_code == 200
        assert isinstance(response.json(), list)

    def test_get_{route_prefix}(self):
        """Testa busca de {name} por ID"""
        # Primeiro cria
        create_response = client.post(
            "/api/{route_prefix}/",
            json={test_create_data}
        )
        item_id = create_response.json()["id"]

        # Depois busca
        response = client.get(f"/api/{route_prefix}/{{item_id}}")
        assert response.status_code == 200

    def test_update_{route_prefix}(self):
        """Testa atualizacao de {name}"""
        # Primeiro cria
        create_response = client.post(
            "/api/{route_prefix}/",
            json={test_create_data}
        )
        item_id = create_response.json()["id"]

        # Depois atualiza
        response = client.put(
            f"/api/{route_prefix}/{{item_id}}",
            json={test_update_data}
        )
        assert response.status_code == 200

    def test_delete_{route_prefix}(self):
        """Testa delecao de {name}"""
        # Primeiro cria
        create_response = client.post(
            "/api/{route_prefix}/",
            json={test_create_data}
        )
        item_id = create_response.json()["id"]

        # Depois deleta
        response = client.delete(f"/api/{route_prefix}/{{item_id}}")
        assert response.status_code == 200
'''
    }

    def __init__(self):
        self.memories: Dict[str, AgentMemory] = {}

    def get_memory(self, agent_id: str) -> AgentMemory:
        """Retorna memoria de um agente"""
        if agent_id not in self.memories:
            self.memories[agent_id] = AgentMemory(agent_id)
        return self.memories[agent_id]

    def create_fastapi_router(self, agent_id: str, project_path: str,
                              name: str, model_name: str, fields: List[Dict]) -> SkillResult:
        """
        Skill REAL: Cria um router FastAPI completo

        Args:
            agent_id: ID do agente executando
            project_path: Caminho do projeto
            name: Nome do recurso (ex: "Processo")
            model_name: Nome do modelo (ex: "process")
            fields: Lista de campos [{name, type, required}]
        """
        result = SkillResult(
            success=False,
            skill_name="create_fastapi_router",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            # Prepara variaveis
            route_prefix = model_name.lower().replace(" ", "_")
            model_class = name.replace(" ", "")
            schema_class = model_class
            tag = name

            # Gera codigo do router
            code = self.TEMPLATES["fastapi_router"].format(
                name=name,
                model_name=model_name,
                model_class=model_class,
                schema_name=model_name,
                schema_class=schema_class,
                route_prefix=route_prefix,
                tag=tag
            )

            # Cria diretorio se nao existe
            router_dir = Path(project_path) / "backend" / "routers"
            router_dir.mkdir(parents=True, exist_ok=True)

            # Salva arquivo
            file_path = router_dir / f"{route_prefix}.py"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {
                "file": str(file_path),
                "router_prefix": f"/api/{route_prefix}",
                "endpoints": ["GET /", f"GET /{{id}}", "POST /", f"PUT /{{id}}", f"DELETE /{{id}}"]
            }
            result.learning_notes.append(f"Criado router FastAPI para {name} com CRUD completo")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)

        # Registra na memoria do agente
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def create_sqlalchemy_model(self, agent_id: str, project_path: str,
                                name: str, table_name: str,
                                fields: List[Dict], description: str = "") -> SkillResult:
        """
        Skill REAL: Cria um modelo SQLAlchemy

        Args:
            agent_id: ID do agente
            project_path: Caminho do projeto
            name: Nome da classe
            table_name: Nome da tabela
            fields: Lista de campos [{name, type, nullable, default}]
            description: Descricao do modelo
        """
        result = SkillResult(
            success=False,
            skill_name="create_sqlalchemy_model",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            # Mapeia tipos Python para SQLAlchemy
            type_mapping = {
                "str": "String(200)",
                "string": "String(200)",
                "text": "Text",
                "int": "Integer",
                "integer": "Integer",
                "float": "Float",
                "bool": "Boolean",
                "boolean": "Boolean",
                "datetime": "DateTime",
                "json": "JSON",
                "dict": "JSON"
            }

            # Gera colunas
            columns = []
            to_dict_fields = []

            for field in fields:
                field_name = field.get("name", "")
                field_type = type_mapping.get(field.get("type", "str").lower(), "String(200)")
                nullable = field.get("nullable", True)
                default = field.get("default")

                col_def = f"    {field_name} = Column({field_type}"
                if not nullable:
                    col_def += ", nullable=False"
                if default is not None:
                    if isinstance(default, str):
                        col_def += f", default=\"{default}\""
                    else:
                        col_def += f", default={default}"
                col_def += ")"
                columns.append(col_def)

                to_dict_fields.append(f'            "{field_name}": self.{field_name},')

            # Gera codigo
            code = self.TEMPLATES["sqlalchemy_model"].format(
                name=name,
                class_name=name.replace(" ", ""),
                table_name=table_name,
                description=description or f"Modelo para {name}",
                columns="\n".join(columns),
                to_dict_fields="\n".join(to_dict_fields)
            )

            # Cria diretorio
            models_dir = Path(project_path) / "backend" / "models"
            models_dir.mkdir(parents=True, exist_ok=True)

            # Salva arquivo
            file_path = models_dir / f"{table_name}.py"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {
                "file": str(file_path),
                "class_name": name.replace(" ", ""),
                "table_name": table_name,
                "fields_count": len(fields)
            }
            result.learning_notes.append(f"Criado modelo SQLAlchemy {name} com {len(fields)} campos")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def create_vue_component(self, agent_id: str, project_path: str,
                             name: str, api_url: str,
                             fields: List[Dict]) -> SkillResult:
        """
        Skill REAL: Cria um componente Vue.js
        """
        result = SkillResult(
            success=False,
            skill_name="create_vue_component",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            component_name = name.lower().replace(" ", "-")

            # Gera campos do formulario
            form_fields = []
            for field in fields:
                field_name = field.get("name", "")
                field_type = field.get("type", "text")
                label = field.get("label", field_name.replace("_", " ").title())

                if field_type in ["text", "string", "str"]:
                    input_type = "text"
                elif field_type in ["int", "integer", "number"]:
                    input_type = "number"
                elif field_type == "email":
                    input_type = "email"
                elif field_type == "date":
                    input_type = "date"
                else:
                    input_type = "text"

                form_fields.append(f'''        <div class="form-group">
          <label>{label}</label>
          <input type="{input_type}" v-model="formData.{field_name}" />
        </div>''')

            code = self.TEMPLATES["vue_component"].format(
                name=name,
                component_name=component_name,
                api_url=api_url,
                form_fields="\n".join(form_fields)
            )

            # Cria diretorio
            components_dir = Path(project_path) / "frontend" / "src" / "components"
            components_dir.mkdir(parents=True, exist_ok=True)

            # Salva arquivo
            file_name = name.replace(" ", "") + ".vue"
            file_path = components_dir / file_name
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {
                "file": str(file_path),
                "component_name": component_name
            }
            result.learning_notes.append(f"Criado componente Vue {name} com formulario CRUD")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def create_test_file(self, agent_id: str, project_path: str,
                         name: str, route_prefix: str) -> SkillResult:
        """
        Skill REAL: Cria arquivo de testes
        """
        result = SkillResult(
            success=False,
            skill_name="create_test_file",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            class_name = name.replace(" ", "")

            code = self.TEMPLATES["test_file"].format(
                name=name,
                class_name=class_name,
                route_prefix=route_prefix,
                test_create_data='{"name": "Test", "description": "Test item"}',
                test_update_data='{"name": "Updated"}'
            )

            # Cria diretorio
            tests_dir = Path(project_path) / "backend" / "tests"
            tests_dir.mkdir(parents=True, exist_ok=True)

            file_path = tests_dir / f"test_{route_prefix}.py"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {"file": str(file_path)}
            result.learning_notes.append(f"Criado arquivo de testes para {name}")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def read_and_analyze_document(self, agent_id: str, file_path: str) -> SkillResult:
        """
        Skill REAL: Le e analisa um documento
        """
        result = SkillResult(
            success=False,
            skill_name="read_and_analyze_document",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            path = Path(file_path)

            if not path.exists():
                result.errors.append(f"Arquivo nao encontrado: {file_path}")
                return result

            content = ""

            if path.suffix.lower() in ['.txt', '.md']:
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()

            elif path.suffix.lower() == '.docx':
                try:
                    from docx import Document
                    doc = Document(path)
                    content = '\n'.join([p.text for p in doc.paragraphs])
                except ImportError:
                    # Tenta ler como texto
                    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()

            elif path.suffix.lower() == '.json':
                with open(path, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    content = json.dumps(data, indent=2)

            else:
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()

            # Analisa conteudo
            word_count = len(content.split())
            line_count = len(content.split('\n'))

            # Extrai palavras-chave basicas
            keywords = []
            important_words = ['processo', 'cliente', 'cadastro', 'vendas', 'pedido',
                             'sistema', 'api', 'banco', 'dados', 'usuario', 'tela']
            content_lower = content.lower()
            for word in important_words:
                if word in content_lower:
                    keywords.append(word)

            result.success = True
            result.outputs = {
                "file": str(path),
                "content_preview": content[:1000],
                "word_count": word_count,
                "line_count": line_count,
                "keywords_found": keywords,
                "full_content": content
            }
            result.learning_notes.append(f"Analisado documento {path.name}: {word_count} palavras, keywords: {keywords}")

            # Adiciona conhecimento ao agente
            memory = self.get_memory(agent_id)
            memory.add_knowledge(f"Documento {path.name} contem informacoes sobre: {', '.join(keywords)}")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def create_database_setup(self, agent_id: str, project_path: str,
                              db_name: str = "app.db") -> SkillResult:
        """
        Skill REAL: Cria configuracao de banco de dados
        """
        result = SkillResult(
            success=False,
            skill_name="create_database_setup",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            code = f'''"""
Configuracao do Banco de Dados
Gerado automaticamente pela Plataforma E
"""

from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from pathlib import Path

# Caminho do banco de dados
DB_PATH = Path(__file__).parent.parent / "database" / "{db_name}"
DB_PATH.parent.mkdir(parents=True, exist_ok=True)

DATABASE_URL = f"sqlite:///{{DB_PATH}}"

engine = create_engine(DATABASE_URL, connect_args={{"check_same_thread": False}})
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()


def init_db():
    """Inicializa o banco de dados criando todas as tabelas"""
    Base.metadata.create_all(bind=engine)
    print(f"Banco de dados inicializado: {{DB_PATH}}")


def get_db():
    """Retorna sessao do banco de dados"""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


if __name__ == "__main__":
    init_db()
'''

            # Cria diretorio
            backend_dir = Path(project_path) / "backend"
            backend_dir.mkdir(parents=True, exist_ok=True)

            file_path = backend_dir / "database.py"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            # Cria pasta database
            (Path(project_path) / "database").mkdir(parents=True, exist_ok=True)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {"file": str(file_path), "db_name": db_name}
            result.learning_notes.append(f"Criado setup de banco de dados SQLite: {db_name}")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result

    def create_main_app(self, agent_id: str, project_path: str,
                        app_name: str, routers: List[str]) -> SkillResult:
        """
        Skill REAL: Cria arquivo main.py do FastAPI
        """
        result = SkillResult(
            success=False,
            skill_name="create_main_app",
            agent_id=agent_id
        )

        start_time = datetime.utcnow()

        try:
            # Gera imports de routers
            router_imports = []
            router_includes = []
            for router in routers:
                router_imports.append(f"from routers.{router} import router as {router}_router")
                router_includes.append(f"app.include_router({router}_router)")

            code = f'''"""
{app_name} - API Backend
Gerado automaticamente pela Plataforma E
"""

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
import uvicorn

from database import init_db
{chr(10).join(router_imports)}

# Inicializa banco de dados
init_db()

# Cria aplicacao FastAPI
app = FastAPI(
    title="{app_name}",
    description="API gerada automaticamente pela Plataforma E",
    version="1.0.0"
)

# CORS - Restrict to specific origins for security
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:9001",
        "http://localhost:9000",
        "http://localhost:8000",
        "http://127.0.0.1:9001",
        "http://127.0.0.1:9000",
        "http://127.0.0.1:8000"
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Inclui routers
{chr(10).join(router_includes)}


@app.get("/")
def root():
    """Endpoint raiz"""
    return {{
        "app": "{app_name}",
        "status": "running",
        "docs": "/docs"
    }}


@app.get("/health")
def health():
    """Health check"""
    return {{"status": "healthy"}}


if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8000)
'''

            backend_dir = Path(project_path) / "backend"
            backend_dir.mkdir(parents=True, exist_ok=True)

            file_path = backend_dir / "main.py"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            result.success = True
            result.files_created.append(str(file_path))
            result.outputs = {
                "file": str(file_path),
                "routers": routers
            }
            result.learning_notes.append(f"Criado main.py com {len(routers)} routers")

        except Exception as e:
            result.errors.append(str(e))

        result.execution_time_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
        self.get_memory(agent_id).record_skill_execution(result)

        return result


# Instancia global
_real_skills: Optional[RealSkills] = None


def get_real_skills() -> RealSkills:
    """Retorna instancia global das skills reais"""
    global _real_skills
    if _real_skills is None:
        _real_skills = RealSkills()
    return _real_skills
