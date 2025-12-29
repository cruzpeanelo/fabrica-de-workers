# -*- coding: utf-8 -*-
"""
Frontend Generator - Gera aplicacoes React/Vue automaticamente
==============================================================

Este modulo complementa o App Generator com suporte a frontend React e Vue.
Permite gerar UI CRUD automatica baseada nos modelos do backend.

Issue #76: Suporte a Frontend React/Vue no App Generator

Suporta:
- React (Vite)
  - Create React App (CRA)
  - Vite (recomendado)
  - Deteccao de componentes existentes
  - Geracao de UI CRUD automatica
- Vue (Vite)
  - Vue CLI
  - Nuxt
  - Deteccao de componentes existentes
  - Geracao de UI CRUD automatica

Autor: Fabrica de Agentes
Data: 2024-12-29
"""

import os
import re
import subprocess
import json
import logging
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime

# Configurar logging
logger = logging.getLogger(__name__)


class FrontendGenerator:
    """
    Gera aplicacoes frontend React/Vue a partir dos modelos do backend.

    Uso:
        generator = FrontendGenerator(project_path, models)
        result = generator.generate_react_app()
        # ou
        result = generator.generate_vue_app()
    """

    def __init__(self, project_path: str, models: List[Dict] = None, project_id: str = None):
        """
        Inicializa o gerador de frontend.

        Args:
            project_path: Caminho do projeto
            models: Lista de modelos do backend (opcional)
            project_id: ID do projeto (opcional)
        """
        self.project_path = Path(project_path)
        self.project_id = project_id or self.project_path.name
        self.models = models or []
        self.frontend_type = None
        self.detected_components = []

    # ============================================================
    # DETECCAO DE FRONTEND EXISTENTE
    # ============================================================

    def detect_frontend_type(self) -> Optional[str]:
        """
        Detecta o tipo de frontend existente (React ou Vue).

        Retorna:
            'react': Se detectar projeto React
            'vue': Se detectar projeto Vue
            None: Se nao detectar frontend
        """
        # Verificar package.json em possiveis locais
        pkg_paths = [
            self.project_path / "package.json",
            self.project_path / "frontend" / "package.json",
            self.project_path / "client" / "package.json",
            self.project_path / "web" / "package.json",
        ]

        for pkg_path in pkg_paths:
            if pkg_path.exists():
                try:
                    content = pkg_path.read_text(encoding="utf-8")
                    pkg_data = json.loads(content)
                    deps = {
                        **pkg_data.get("dependencies", {}),
                        **pkg_data.get("devDependencies", {})
                    }

                    # Detectar React
                    if "react" in deps or "react-dom" in deps or "next" in deps:
                        self.frontend_type = "react"
                        return "react"

                    # Detectar Vue
                    if "vue" in deps or "nuxt" in deps or "@vue/cli-service" in deps:
                        self.frontend_type = "vue"
                        return "vue"

                except Exception as e:
                    logger.debug(f"Erro ao ler {pkg_path}: {e}")

        # Verificar por arquivos caracteristicos
        vue_files = [f for f in self.project_path.rglob("*.vue")
                     if "node_modules" not in str(f)]
        jsx_files = [f for f in self.project_path.rglob("*.jsx")
                     if "node_modules" not in str(f)]
        tsx_files = [f for f in self.project_path.rglob("*.tsx")
                     if "node_modules" not in str(f)]

        if vue_files:
            self.frontend_type = "vue"
            return "vue"

        if jsx_files or tsx_files:
            self.frontend_type = "react"
            return "react"

        return None

    def detect_react_components(self) -> List[Dict]:
        """
        Detecta componentes React existentes.

        Retorna:
            Lista de componentes encontrados
        """
        components = []

        jsx_files = list(self.project_path.rglob("*.jsx"))
        tsx_files = list(self.project_path.rglob("*.tsx"))
        all_files = [f for f in jsx_files + tsx_files if "node_modules" not in str(f)]

        for file_path in all_files:
            try:
                content = file_path.read_text(encoding="utf-8", errors="ignore")
                relative_path = str(file_path.relative_to(self.project_path))

                # Componentes funcionais
                func_pattern = r"(?:export\s+)?(?:default\s+)?(?:function|const)\s+([A-Z]\w+)"
                for match in re.finditer(func_pattern, content):
                    comp_name = match.group(1)
                    if comp_name not in ["React", "Component", "Fragment"]:
                        comp_type = self._detect_component_type(content, comp_name)
                        components.append({
                            "name": comp_name,
                            "type": comp_type,
                            "file": relative_path,
                            "framework": "react"
                        })

                # Componentes de classe
                class_pattern = r"class\s+([A-Z]\w+)\s+extends\s+(?:React\.)?Component"
                for match in re.finditer(class_pattern, content):
                    components.append({
                        "name": match.group(1),
                        "type": "class",
                        "file": relative_path,
                        "framework": "react"
                    })

            except Exception as e:
                logger.debug(f"Erro ao analisar {file_path}: {e}")

        self.detected_components = components
        return components

    def detect_vue_components(self) -> List[Dict]:
        """
        Detecta componentes Vue existentes.

        Retorna:
            Lista de componentes encontrados
        """
        components = []

        vue_files = [f for f in self.project_path.rglob("*.vue")
                     if "node_modules" not in str(f)]

        for file_path in vue_files:
            try:
                content = file_path.read_text(encoding="utf-8", errors="ignore")
                relative_path = str(file_path.relative_to(self.project_path))

                comp_name = file_path.stem
                api_type = "composition" if "<script setup" in content or "setup()" in content else "options"
                comp_type = self._detect_vue_component_type(content, comp_name)

                components.append({
                    "name": comp_name,
                    "type": comp_type,
                    "api": api_type,
                    "file": relative_path,
                    "framework": "vue"
                })

            except Exception as e:
                logger.debug(f"Erro ao analisar {file_path}: {e}")

        self.detected_components = components
        return components

    def _detect_component_type(self, content: str, name: str) -> str:
        """Detecta o tipo de componente (form, table, modal, etc)."""
        content_lower = content.lower()
        name_lower = name.lower()

        if "form" in name_lower or "<form" in content_lower:
            return "form"
        if "table" in name_lower or "<table" in content_lower:
            return "table"
        if "modal" in name_lower or "dialog" in content_lower:
            return "modal"
        if "card" in name_lower:
            return "card"
        if "list" in name_lower:
            return "list"
        if "chart" in name_lower:
            return "chart"

        return "functional"

    def _detect_vue_component_type(self, content: str, name: str) -> str:
        """Detecta o tipo de componente Vue."""
        content_lower = content.lower()
        name_lower = name.lower()

        if "form" in name_lower or "<form" in content_lower:
            return "form"
        if "table" in name_lower or "v-data-table" in content_lower:
            return "table"
        if "modal" in name_lower or "v-dialog" in content_lower:
            return "modal"
        if "card" in name_lower or "v-card" in content_lower:
            return "card"
        if "dashboard" in name_lower:
            return "dashboard"

        return "component"

    # ============================================================
    # GERACAO DE FRONTEND REACT
    # ============================================================

    def generate_react_app(self) -> Dict:
        """
        Gera aplicacao React completa com CRUD.

        Retorna:
            Dicionario com resultado da geracao
        """
        # Criar estrutura de diretorios
        frontend_dir = self.project_path / "frontend"
        frontend_dir.mkdir(exist_ok=True)

        src_dir = frontend_dir / "src"
        src_dir.mkdir(exist_ok=True)

        components_dir = src_dir / "components"
        components_dir.mkdir(exist_ok=True)

        services_dir = src_dir / "services"
        services_dir.mkdir(exist_ok=True)

        files_created = []

        # Se nao tem modelos, criar um de exemplo
        if not self.models:
            self.models = [{"name": "Item", "type": "example", "fields": [
                {"name": "name", "type": "string"},
                {"name": "description", "type": "string"}
            ]}]

        # 1. package.json
        pkg_content = self._generate_react_package_json()
        pkg_file = frontend_dir / "package.json"
        pkg_file.write_text(pkg_content, encoding="utf-8")
        files_created.append(str(pkg_file))

        # 2. vite.config.js
        vite_config = self._generate_vite_config("react")
        vite_file = frontend_dir / "vite.config.js"
        vite_file.write_text(vite_config, encoding="utf-8")
        files_created.append(str(vite_file))

        # 3. index.html
        index_html = self._generate_index_html("react")
        index_file = frontend_dir / "index.html"
        index_file.write_text(index_html, encoding="utf-8")
        files_created.append(str(index_file))

        # 4. main.jsx
        main_jsx = self._generate_react_main()
        main_file = src_dir / "main.jsx"
        main_file.write_text(main_jsx, encoding="utf-8")
        files_created.append(str(main_file))

        # 5. App.jsx
        app_jsx = self._generate_react_app()
        app_file = src_dir / "App.jsx"
        app_file.write_text(app_jsx, encoding="utf-8")
        files_created.append(str(app_file))

        # 6. api.js
        api_js = self._generate_api_service("react")
        api_file = services_dir / "api.js"
        api_file.write_text(api_js, encoding="utf-8")
        files_created.append(str(api_file))

        # 7. Componentes CRUD para cada modelo
        for model in self.models:
            crud_component = self._generate_react_crud_component(model)
            comp_file = components_dir / f"{model['name']}Crud.jsx"
            comp_file.write_text(crud_component, encoding="utf-8")
            files_created.append(str(comp_file))

        # 8. DataTable.jsx
        table_component = self._generate_react_data_table()
        table_file = components_dir / "DataTable.jsx"
        table_file.write_text(table_component, encoding="utf-8")
        files_created.append(str(table_file))

        # 9. FormModal.jsx
        form_component = self._generate_react_form_modal()
        form_file = components_dir / "FormModal.jsx"
        form_file.write_text(form_component, encoding="utf-8")
        files_created.append(str(form_file))

        # 10. App.css
        css_content = self._generate_styles()
        css_file = src_dir / "App.css"
        css_file.write_text(css_content, encoding="utf-8")
        files_created.append(str(css_file))

        # 11. Script de inicializacao
        start_script = frontend_dir / "iniciar_frontend.bat"
        start_content = self._generate_start_script("react")
        start_script.write_text(start_content, encoding="utf-8")
        files_created.append(str(start_script))

        return {
            "success": True,
            "message": f"Frontend React gerado com {len(self.models)} componentes CRUD.",
            "app_url": "http://localhost:5173",
            "start_command": "npm run dev",
            "frontend_dir": str(frontend_dir),
            "files_created": files_created
        }

    def _generate_react_package_json(self) -> str:
        """Gera package.json para React."""
        pkg = {
            "name": f"{self.project_id.lower().replace('-', '_')}_frontend",
            "version": "1.0.0",
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "vite build",
                "preview": "vite preview"
            },
            "dependencies": {
                "react": "^18.2.0",
                "react-dom": "^18.2.0",
                "react-router-dom": "^6.20.0",
                "axios": "^1.6.2"
            },
            "devDependencies": {
                "@vitejs/plugin-react": "^4.2.0",
                "vite": "^5.0.0"
            }
        }
        return json.dumps(pkg, indent=2, ensure_ascii=False)

    def _generate_react_main(self) -> str:
        """Gera main.jsx."""
        return '''import React from 'react'
import ReactDOM from 'react-dom/client'
import { BrowserRouter } from 'react-router-dom'
import App from './App'
import './App.css'

ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <BrowserRouter>
      <App />
    </BrowserRouter>
  </React.StrictMode>
)
'''

    def _generate_react_app(self) -> str:
        """Gera App.jsx principal."""
        model_names = [m["name"] for m in self.models]
        imports = "\n".join([f"import {m}Crud from './components/{m}Crud'" for m in model_names])
        nav_links = "\n          ".join([f'<NavLink to="/{m.lower()}s">{m}s</NavLink>' for m in model_names])
        routes = "\n          ".join([f'<Route path="/{m.lower()}s" element={{<{m}Crud />}} />' for m in model_names])
        cards = "\n        ".join([f'''<div className="model-card">
          <h3>{m}</h3>
          <p>Gerenciar {m.lower()}s</p>
          <NavLink to="/{m.lower()}s" className="btn">Acessar</NavLink>
        </div>''' for m in model_names])

        return f'''/**
 * App Principal - {self.project_id}
 * Gerado automaticamente pela Fabrica de Agentes
 * Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}
 */
import {{ Routes, Route, NavLink }} from 'react-router-dom'
{imports}

function App() {{
  return (
    <div className="app">
      <header className="header">
        <h1>{self.project_id}</h1>
        <nav className="nav">
          <NavLink to="/">Home</NavLink>
          {nav_links}
        </nav>
      </header>

      <main className="main">
        <Routes>
          <Route path="/" element={{<Home />}} />
          {routes}
        </Routes>
      </main>

      <footer className="footer">
        <p>Gerado pela Fabrica de Agentes</p>
      </footer>
    </div>
  )
}}

function Home() {{
  return (
    <div className="home">
      <h2>Bem-vindo ao {self.project_id}</h2>
      <p>Selecione um modelo no menu para gerenciar os dados.</p>
      <div className="models-grid">
        {cards}
      </div>
    </div>
  )
}}

export default App
'''

    def _generate_react_crud_component(self, model: Dict) -> str:
        """Gera componente CRUD React para um modelo."""
        name = model["name"]
        plural = name.lower() + "s"
        fields = model.get("fields", [])

        # Gerar campos do formulario
        form_fields = ""
        table_columns = ""

        if fields:
            for field in fields:
                field_name = field.get("name", field) if isinstance(field, dict) else field
                field_type = field.get("type", "string") if isinstance(field, dict) else "string"
                input_type = {"number": "number", "boolean": "checkbox", "date": "date"}.get(field_type, "text")

                form_fields += f'''
        <div className="form-group">
          <label>{field_name}</label>
          <input
            type="{input_type}"
            name="{field_name}"
            value={{formData.{field_name} || ''}}
            onChange={{handleChange}}
          />
        </div>'''
                table_columns += f"{{ key: '{field_name}', header: '{field_name.title()}' }},\n      "
        else:
            form_fields = '''
        <div className="form-group">
          <label>Nome</label>
          <input
            type="text"
            name="name"
            value={formData.name || ''}
            onChange={handleChange}
            required
          />
        </div>
        <div className="form-group">
          <label>Descricao</label>
          <textarea
            name="description"
            value={formData.description || ''}
            onChange={handleChange}
          />
        </div>'''
            table_columns = "{ key: 'name', header: 'Nome' },\n      { key: 'description', header: 'Descricao' },"

        return f'''/**
 * Componente CRUD - {name}
 * Gerado automaticamente pela Fabrica de Agentes
 */
import {{ useState, useEffect }} from 'react'
import {{ apiService }} from '../services/api'
import DataTable from './DataTable'
import FormModal from './FormModal'

function {name}Crud() {{
  const [items, setItems] = useState([])
  const [loading, setLoading] = useState(true)
  const [showModal, setShowModal] = useState(false)
  const [editingItem, setEditingItem] = useState(null)
  const [formData, setFormData] = useState({{}})
  const [error, setError] = useState(null)

  const columns = [
    {{ key: 'id', header: 'ID' }},
    {table_columns}
    {{ key: 'createdAt', header: 'Criado em' }}
  ]

  useEffect(() => {{
    loadData()
  }}, [])

  const loadData = async () => {{
    try {{
      setLoading(true)
      const response = await apiService.get{name}s()
      setItems(response.data || [])
    }} catch (err) {{
      setError('Erro ao carregar dados')
      console.error(err)
    }} finally {{
      setLoading(false)
    }}
  }}

  const handleChange = (e) => {{
    const {{ name, value, type, checked }} = e.target
    setFormData(prev => ({{
      ...prev,
      [name]: type === 'checkbox' ? checked : value
    }}))
  }}

  const handleSubmit = async (e) => {{
    e.preventDefault()
    try {{
      if (editingItem) {{
        await apiService.update{name}(editingItem.id, formData)
      }} else {{
        await apiService.create{name}(formData)
      }}
      setShowModal(false)
      setFormData({{}})
      setEditingItem(null)
      loadData()
    }} catch (err) {{
      setError('Erro ao salvar')
    }}
  }}

  const handleEdit = (item) => {{
    setEditingItem(item)
    setFormData(item)
    setShowModal(true)
  }}

  const handleDelete = async (item) => {{
    if (window.confirm('Deseja realmente excluir?')) {{
      try {{
        await apiService.delete{name}(item.id)
        loadData()
      }} catch (err) {{
        setError('Erro ao excluir')
      }}
    }}
  }}

  const handleNew = () => {{
    setEditingItem(null)
    setFormData({{}})
    setShowModal(true)
  }}

  return (
    <div className="crud-container">
      <div className="crud-header">
        <h2>{name}s</h2>
        <button className="btn btn-primary" onClick={{handleNew}}>
          + Novo {name}
        </button>
      </div>

      {{error && <div className="alert alert-error">{{error}}</div>}}

      <DataTable
        data={{items}}
        columns={{columns}}
        loading={{loading}}
        onEdit={{handleEdit}}
        onDelete={{handleDelete}}
      />

      {{showModal && (
        <FormModal
          title={{editingItem ? 'Editar {name}' : 'Novo {name}'}}
          onClose={{() => setShowModal(false)}}
          onSubmit={{handleSubmit}}
        >
          {form_fields}
        </FormModal>
      )}}
    </div>
  )
}}

export default {name}Crud
'''

    def _generate_react_data_table(self) -> str:
        """Gera componente DataTable reutilizavel."""
        return '''/**
 * Componente DataTable - Tabela de dados reutilizavel
 * Gerado automaticamente pela Fabrica de Agentes
 */
import { useState } from 'react'

function DataTable({ data, columns, loading, onEdit, onDelete, pageSize = 10 }) {
  const [currentPage, setCurrentPage] = useState(1)
  const [searchTerm, setSearchTerm] = useState('')

  const filteredData = data.filter(item =>
    columns.some(col => {
      const value = item[col.key]
      return value && String(value).toLowerCase().includes(searchTerm.toLowerCase())
    })
  )

  const totalPages = Math.ceil(filteredData.length / pageSize)
  const startIndex = (currentPage - 1) * pageSize
  const paginatedData = filteredData.slice(startIndex, startIndex + pageSize)

  if (loading) {
    return (
      <div className="loading">
        <div className="spinner"></div>
        <p>Carregando...</p>
      </div>
    )
  }

  return (
    <div className="data-table-container">
      <div className="table-controls">
        <input
          type="text"
          placeholder="Buscar..."
          value={searchTerm}
          onChange={(e) => {
            setSearchTerm(e.target.value)
            setCurrentPage(1)
          }}
          className="search-input"
        />
        <span className="total-records">{filteredData.length} registro(s)</span>
      </div>

      <table className="data-table">
        <thead>
          <tr>
            {columns.map(col => (
              <th key={col.key}>{col.header}</th>
            ))}
            <th>Acoes</th>
          </tr>
        </thead>
        <tbody>
          {paginatedData.length === 0 ? (
            <tr>
              <td colSpan={columns.length + 1} className="empty-message">
                Nenhum registro encontrado
              </td>
            </tr>
          ) : (
            paginatedData.map((item, index) => (
              <tr key={item.id || index}>
                {columns.map(col => (
                  <td key={col.key}>{item[col.key]}</td>
                ))}
                <td className="actions">
                  <button className="btn btn-sm btn-edit" onClick={() => onEdit(item)}>
                    Editar
                  </button>
                  <button className="btn btn-sm btn-delete" onClick={() => onDelete(item)}>
                    Excluir
                  </button>
                </td>
              </tr>
            ))
          )}
        </tbody>
      </table>

      {totalPages > 1 && (
        <div className="pagination">
          <button
            disabled={currentPage === 1}
            onClick={() => setCurrentPage(p => p - 1)}
          >
            Anterior
          </button>
          <span>{currentPage} / {totalPages}</span>
          <button
            disabled={currentPage === totalPages}
            onClick={() => setCurrentPage(p => p + 1)}
          >
            Proximo
          </button>
        </div>
      )}
    </div>
  )
}

export default DataTable
'''

    def _generate_react_form_modal(self) -> str:
        """Gera componente FormModal reutilizavel."""
        return '''/**
 * Componente FormModal - Modal de formulario reutilizavel
 * Gerado automaticamente pela Fabrica de Agentes
 */
function FormModal({ title, children, onClose, onSubmit }) {
  const handleBackdropClick = (e) => {
    if (e.target === e.currentTarget) {
      onClose()
    }
  }

  return (
    <div className="modal-backdrop" onClick={handleBackdropClick}>
      <div className="modal">
        <div className="modal-header">
          <h3>{title}</h3>
          <button className="modal-close" onClick={onClose}>&times;</button>
        </div>
        <form onSubmit={onSubmit}>
          <div className="modal-body">
            {children}
          </div>
          <div className="modal-footer">
            <button type="button" className="btn btn-secondary" onClick={onClose}>
              Cancelar
            </button>
            <button type="submit" className="btn btn-primary">
              Salvar
            </button>
          </div>
        </form>
      </div>
    </div>
  )
}

export default FormModal
'''

    # ============================================================
    # GERACAO DE FRONTEND VUE
    # ============================================================

    def generate_vue_app(self) -> Dict:
        """
        Gera aplicacao Vue completa com CRUD.

        Retorna:
            Dicionario com resultado da geracao
        """
        # Criar estrutura de diretorios
        frontend_dir = self.project_path / "frontend"
        frontend_dir.mkdir(exist_ok=True)

        src_dir = frontend_dir / "src"
        src_dir.mkdir(exist_ok=True)

        components_dir = src_dir / "components"
        components_dir.mkdir(exist_ok=True)

        views_dir = src_dir / "views"
        views_dir.mkdir(exist_ok=True)

        services_dir = src_dir / "services"
        services_dir.mkdir(exist_ok=True)

        router_dir = src_dir / "router"
        router_dir.mkdir(exist_ok=True)

        files_created = []

        # Se nao tem modelos, criar um de exemplo
        if not self.models:
            self.models = [{"name": "Item", "type": "example", "fields": [
                {"name": "name", "type": "string"},
                {"name": "description", "type": "string"}
            ]}]

        # 1. package.json
        pkg_content = self._generate_vue_package_json()
        pkg_file = frontend_dir / "package.json"
        pkg_file.write_text(pkg_content, encoding="utf-8")
        files_created.append(str(pkg_file))

        # 2. vite.config.js
        vite_config = self._generate_vite_config("vue")
        vite_file = frontend_dir / "vite.config.js"
        vite_file.write_text(vite_config, encoding="utf-8")
        files_created.append(str(vite_file))

        # 3. index.html
        index_html = self._generate_index_html("vue")
        index_file = frontend_dir / "index.html"
        index_file.write_text(index_html, encoding="utf-8")
        files_created.append(str(index_file))

        # 4. main.js
        main_js = self._generate_vue_main()
        main_file = src_dir / "main.js"
        main_file.write_text(main_js, encoding="utf-8")
        files_created.append(str(main_file))

        # 5. App.vue
        app_vue = self._generate_vue_app()
        app_file = src_dir / "App.vue"
        app_file.write_text(app_vue, encoding="utf-8")
        files_created.append(str(app_file))

        # 6. router/index.js
        router_js = self._generate_vue_router()
        router_file = router_dir / "index.js"
        router_file.write_text(router_js, encoding="utf-8")
        files_created.append(str(router_file))

        # 7. api.js
        api_js = self._generate_api_service("vue")
        api_file = services_dir / "api.js"
        api_file.write_text(api_js, encoding="utf-8")
        files_created.append(str(api_file))

        # 8. Views CRUD para cada modelo
        for model in self.models:
            crud_view = self._generate_vue_crud_view(model)
            view_file = views_dir / f"{model['name']}View.vue"
            view_file.write_text(crud_view, encoding="utf-8")
            files_created.append(str(view_file))

        # 9. Home view
        home_view = self._generate_vue_home_view()
        home_file = views_dir / "HomeView.vue"
        home_file.write_text(home_view, encoding="utf-8")
        files_created.append(str(home_file))

        # 10. DataTable.vue
        table_component = self._generate_vue_data_table()
        table_file = components_dir / "DataTable.vue"
        table_file.write_text(table_component, encoding="utf-8")
        files_created.append(str(table_file))

        # 11. FormModal.vue
        form_component = self._generate_vue_form_modal()
        form_file = components_dir / "FormModal.vue"
        form_file.write_text(form_component, encoding="utf-8")
        files_created.append(str(form_file))

        # 12. style.css
        css_content = self._generate_styles()
        css_file = src_dir / "style.css"
        css_file.write_text(css_content, encoding="utf-8")
        files_created.append(str(css_file))

        # 13. Script de inicializacao
        start_script = frontend_dir / "iniciar_frontend.bat"
        start_content = self._generate_start_script("vue")
        start_script.write_text(start_content, encoding="utf-8")
        files_created.append(str(start_script))

        return {
            "success": True,
            "message": f"Frontend Vue gerado com {len(self.models)} componentes CRUD.",
            "app_url": "http://localhost:5173",
            "start_command": "npm run dev",
            "frontend_dir": str(frontend_dir),
            "files_created": files_created
        }

    def _generate_vue_package_json(self) -> str:
        """Gera package.json para Vue."""
        pkg = {
            "name": f"{self.project_id.lower().replace('-', '_')}_frontend",
            "version": "1.0.0",
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "vite build",
                "preview": "vite preview"
            },
            "dependencies": {
                "vue": "^3.3.11",
                "vue-router": "^4.2.5",
                "axios": "^1.6.2"
            },
            "devDependencies": {
                "@vitejs/plugin-vue": "^4.5.2",
                "vite": "^5.0.0"
            }
        }
        return json.dumps(pkg, indent=2, ensure_ascii=False)

    def _generate_vue_main(self) -> str:
        """Gera main.js para Vue."""
        return '''import { createApp } from 'vue'
import App from './App.vue'
import router from './router'
import './style.css'

createApp(App).use(router).mount('#app')
'''

    def _generate_vue_app(self) -> str:
        """Gera App.vue principal."""
        model_names = [m["name"] for m in self.models]
        nav_links = "\n        ".join([f'<router-link to="/{m.lower()}s">{m}s</router-link>' for m in model_names])

        return f'''<template>
  <div class="app">
    <header class="header">
      <h1>{self.project_id}</h1>
      <nav class="nav">
        <router-link to="/">Home</router-link>
        {nav_links}
      </nav>
    </header>

    <main class="main">
      <router-view />
    </main>

    <footer class="footer">
      <p>Gerado pela Fabrica de Agentes</p>
    </footer>
  </div>
</template>

<script setup>
/**
 * App Principal - {self.project_id}
 * Gerado automaticamente pela Fabrica de Agentes
 * Data: {datetime.now().strftime("%Y-%m-%d %H:%M")}
 */
</script>
'''

    def _generate_vue_router(self) -> str:
        """Gera router/index.js para Vue."""
        model_names = [m["name"] for m in self.models]
        imports = "\n".join([f"import {m}View from '../views/{m}View.vue'" for m in model_names])
        routes = ",\n    ".join([f"{{ path: '/{m.lower()}s', name: '{m.lower()}s', component: {m}View }}" for m in model_names])

        return f'''import {{ createRouter, createWebHistory }} from 'vue-router'
import HomeView from '../views/HomeView.vue'
{imports}

const router = createRouter({{
  history: createWebHistory(),
  routes: [
    {{ path: '/', name: 'home', component: HomeView }},
    {routes}
  ]
}})

export default router
'''

    def _generate_vue_crud_view(self, model: Dict) -> str:
        """Gera view CRUD Vue para um modelo."""
        name = model["name"]
        fields = model.get("fields", [])

        # Gerar campos do formulario
        form_fields = ""
        table_columns = "{ key: 'id', header: 'ID' },\n        "

        if fields:
            for field in fields:
                field_name = field.get("name", field) if isinstance(field, dict) else field
                field_type = field.get("type", "string") if isinstance(field, dict) else "string"
                input_type = {"number": "number", "boolean": "checkbox", "date": "date"}.get(field_type, "text")

                form_fields += f'''
          <div class="form-group">
            <label>{field_name}</label>
            <input type="{input_type}" v-model="formData.{field_name}" />
          </div>'''
                table_columns += f"{{ key: '{field_name}', header: '{field_name.title()}' }},\n        "
        else:
            form_fields = '''
          <div class="form-group">
            <label>Nome</label>
            <input type="text" v-model="formData.name" required />
          </div>
          <div class="form-group">
            <label>Descricao</label>
            <textarea v-model="formData.description"></textarea>
          </div>'''
            table_columns += "{ key: 'name', header: 'Nome' },\n        { key: 'description', header: 'Descricao' },"

        return f'''<template>
  <div class="crud-container">
    <div class="crud-header">
      <h2>{name}s</h2>
      <button class="btn btn-primary" @click="openModal()">+ Novo {name}</button>
    </div>

    <div v-if="error" class="alert alert-error">{{{{ error }}}}</div>

    <DataTable
      :data="items"
      :columns="columns"
      :loading="loading"
      @edit="handleEdit"
      @delete="handleDelete"
    />

    <FormModal
      v-if="showModal"
      :title="editingItem ? 'Editar {name}' : 'Novo {name}'"
      @close="closeModal"
      @submit="handleSubmit"
    >
      {form_fields}
    </FormModal>
  </div>
</template>

<script setup>
/**
 * View CRUD - {name}
 * Gerado automaticamente pela Fabrica de Agentes
 */
import {{ ref, onMounted }} from 'vue'
import {{ apiService }} from '../services/api'
import DataTable from '../components/DataTable.vue'
import FormModal from '../components/FormModal.vue'

const items = ref([])
const loading = ref(true)
const showModal = ref(false)
const editingItem = ref(null)
const formData = ref({{}})
const error = ref(null)

const columns = [
  {table_columns}
  {{ key: 'createdAt', header: 'Criado em' }}
]

const loadData = async () => {{
  try {{
    loading.value = true
    const response = await apiService.get{name}s()
    items.value = response.data || []
  }} catch (err) {{
    error.value = 'Erro ao carregar dados'
  }} finally {{
    loading.value = false
  }}
}}

const openModal = (item = null) => {{
  editingItem.value = item
  formData.value = item ? {{ ...item }} : {{}}
  showModal.value = true
}}

const closeModal = () => {{
  showModal.value = false
  editingItem.value = null
  formData.value = {{}}
}}

const handleEdit = (item) => openModal(item)

const handleDelete = async (item) => {{
  if (confirm('Deseja realmente excluir?')) {{
    try {{
      await apiService.delete{name}(item.id)
      loadData()
    }} catch (err) {{
      error.value = 'Erro ao excluir'
    }}
  }}
}}

const handleSubmit = async () => {{
  try {{
    if (editingItem.value) {{
      await apiService.update{name}(editingItem.value.id, formData.value)
    }} else {{
      await apiService.create{name}(formData.value)
    }}
    closeModal()
    loadData()
  }} catch (err) {{
    error.value = 'Erro ao salvar'
  }}
}}

onMounted(() => loadData())
</script>
'''

    def _generate_vue_home_view(self) -> str:
        """Gera HomeView.vue."""
        model_names = [m["name"] for m in self.models]
        cards = "\n      ".join([f'''<div class="model-card">
        <h3>{m}</h3>
        <p>Gerenciar {m.lower()}s</p>
        <router-link to="/{m.lower()}s" class="btn">Acessar</router-link>
      </div>''' for m in model_names])

        return f'''<template>
  <div class="home">
    <h2>Bem-vindo ao {self.project_id}</h2>
    <p>Selecione um modelo no menu para gerenciar os dados.</p>
    <div class="models-grid">
      {cards}
    </div>
  </div>
</template>

<script setup>
/**
 * HomeView - Pagina inicial
 * Gerado automaticamente pela Fabrica de Agentes
 */
</script>
'''

    def _generate_vue_data_table(self) -> str:
        """Gera componente DataTable Vue."""
        return '''<template>
  <div class="data-table-container">
    <div class="table-controls">
      <input type="text" placeholder="Buscar..." v-model="searchTerm" class="search-input" />
      <span class="total-records">{{ filteredData.length }} registro(s)</span>
    </div>

    <div v-if="loading" class="loading">
      <div class="spinner"></div>
      <p>Carregando...</p>
    </div>

    <table v-else class="data-table">
      <thead>
        <tr>
          <th v-for="col in columns" :key="col.key">{{ col.header }}</th>
          <th>Acoes</th>
        </tr>
      </thead>
      <tbody>
        <tr v-if="paginatedData.length === 0">
          <td :colspan="columns.length + 1" class="empty-message">Nenhum registro encontrado</td>
        </tr>
        <tr v-else v-for="item in paginatedData" :key="item.id">
          <td v-for="col in columns" :key="col.key">{{ item[col.key] }}</td>
          <td class="actions">
            <button class="btn btn-sm btn-edit" @click="$emit('edit', item)">Editar</button>
            <button class="btn btn-sm btn-delete" @click="$emit('delete', item)">Excluir</button>
          </td>
        </tr>
      </tbody>
    </table>

    <div v-if="totalPages > 1" class="pagination">
      <button :disabled="currentPage === 1" @click="currentPage--">Anterior</button>
      <span>{{ currentPage }} / {{ totalPages }}</span>
      <button :disabled="currentPage === totalPages" @click="currentPage++">Proximo</button>
    </div>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const props = defineProps({
  data: { type: Array, default: () => [] },
  columns: { type: Array, default: () => [] },
  loading: { type: Boolean, default: false },
  pageSize: { type: Number, default: 10 }
})

defineEmits(['edit', 'delete'])

const searchTerm = ref('')
const currentPage = ref(1)

const filteredData = computed(() => {
  if (!searchTerm.value) return props.data
  return props.data.filter(item =>
    props.columns.some(col => {
      const value = item[col.key]
      return value && String(value).toLowerCase().includes(searchTerm.value.toLowerCase())
    })
  )
})

const totalPages = computed(() => Math.ceil(filteredData.value.length / props.pageSize))

const paginatedData = computed(() => {
  const start = (currentPage.value - 1) * props.pageSize
  return filteredData.value.slice(start, start + props.pageSize)
})
</script>
'''

    def _generate_vue_form_modal(self) -> str:
        """Gera componente FormModal Vue."""
        return '''<template>
  <div class="modal-backdrop" @click.self="$emit('close')">
    <div class="modal">
      <div class="modal-header">
        <h3>{{ title }}</h3>
        <button class="modal-close" @click="$emit('close')">&times;</button>
      </div>
      <form @submit.prevent="$emit('submit')">
        <div class="modal-body">
          <slot></slot>
        </div>
        <div class="modal-footer">
          <button type="button" class="btn btn-secondary" @click="$emit('close')">Cancelar</button>
          <button type="submit" class="btn btn-primary">Salvar</button>
        </div>
      </form>
    </div>
  </div>
</template>

<script setup>
defineProps({
  title: { type: String, default: 'Modal' }
})
defineEmits(['close', 'submit'])
</script>
'''

    # ============================================================
    # ARQUIVOS COMPARTILHADOS
    # ============================================================

    def _generate_vite_config(self, framework: str) -> str:
        """Gera vite.config.js."""
        plugin = "react" if framework == "react" else "vue"
        plugin_import = f"@vitejs/plugin-{plugin}"

        return f'''import {{ defineConfig }} from 'vite'
import {plugin} from '{plugin_import}'

export default defineConfig({{
  plugins: [{plugin}()],
  server: {{
    port: 5173,
    proxy: {{
      '/api': {{
        target: 'http://localhost:8000',
        changeOrigin: true
      }}
    }}
  }}
}})
'''

    def _generate_index_html(self, framework: str) -> str:
        """Gera index.html."""
        root_id = "root" if framework == "react" else "app"
        main_file = "/src/main.jsx" if framework == "react" else "/src/main.js"

        return f'''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.project_id} - Frontend</title>
</head>
<body>
    <div id="{root_id}"></div>
    <script type="module" src="{main_file}"></script>
</body>
</html>
'''

    def _generate_api_service(self, framework: str) -> str:
        """Gera servico de API."""
        model_names = [m["name"] for m in self.models]

        api_methods = ""
        for model in self.models:
            name = model["name"]
            plural = name.lower() + "s"
            api_methods += f'''
  // {name}
  get{name}s: () => api.get('/api/{plural}'),
  get{name}: (id) => api.get(`/api/{plural}/${{id}}`),
  create{name}: (data) => api.post('/api/{plural}', data),
  update{name}: (id, data) => api.put(`/api/{plural}/${{id}}`, data),
  delete{name}: (id) => api.delete(`/api/{plural}/${{id}}`),
'''

        return f'''/**
 * Servico de API - {self.project_id}
 * Gerado automaticamente pela Fabrica de Agentes
 */
import axios from 'axios'

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000'

const api = axios.create({{
  baseURL: API_BASE_URL,
  headers: {{
    'Content-Type': 'application/json'
  }}
}})

api.interceptors.response.use(
  response => response.data,
  error => {{
    console.error('Erro na API:', error.response?.data || error.message)
    throw error
  }}
)

export const apiService = {{{api_methods}}}

export default api
'''

    def _generate_styles(self) -> str:
        """Gera estilos CSS."""
        return '''/**
 * Estilos - Fabrica de Agentes
 * Cores baseadas na identidade visual Belgo Arames
 */

:root {
  --primary: #003B4A;
  --secondary: #FF6C00;
  --success: #10B981;
  --danger: #EF4444;
  --gray-50: #F9FAFB;
  --gray-100: #F3F4F6;
  --gray-200: #E5E7EB;
  --gray-300: #D1D5DB;
  --gray-500: #6B7280;
  --gray-700: #374151;
  --gray-900: #111827;
  --white: #FFFFFF;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: var(--gray-100);
  color: var(--gray-900);
  line-height: 1.6;
}

.app { min-height: 100vh; display: flex; flex-direction: column; }

.header {
  background: var(--primary);
  color: var(--white);
  padding: 1rem 2rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.header h1 { font-size: 1.5rem; }

.nav { display: flex; gap: 1rem; }

.nav a {
  color: var(--white);
  text-decoration: none;
  padding: 0.5rem 1rem;
  border-radius: 4px;
  transition: background 0.2s;
}

.nav a:hover, .nav a.active, .nav a.router-link-active {
  background: rgba(255, 255, 255, 0.1);
}

.main { flex: 1; padding: 2rem; max-width: 1200px; margin: 0 auto; width: 100%; }

.home { text-align: center; }
.home h2 { color: var(--primary); margin-bottom: 1rem; }

.models-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  gap: 1.5rem;
  margin-top: 2rem;
}

.model-card {
  background: var(--white);
  padding: 1.5rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  text-align: center;
}

.model-card h3 { color: var(--primary); margin-bottom: 0.5rem; }
.model-card p { color: var(--gray-500); margin-bottom: 1rem; }

.btn {
  display: inline-block;
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 4px;
  font-size: 1rem;
  cursor: pointer;
  text-decoration: none;
  transition: all 0.2s;
}

.btn-primary { background: var(--secondary); color: var(--white); }
.btn-primary:hover { background: #e65c00; }
.btn-secondary { background: var(--gray-200); color: var(--gray-700); }
.btn-secondary:hover { background: var(--gray-300); }
.btn-sm { padding: 0.25rem 0.75rem; font-size: 0.875rem; }
.btn-edit { background: var(--primary); color: var(--white); }
.btn-delete { background: var(--danger); color: var(--white); }

.crud-container {
  background: var(--white);
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  padding: 1.5rem;
}

.crud-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
}

.crud-header h2 { color: var(--primary); }

.data-table-container { overflow-x: auto; }

.table-controls {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.search-input {
  padding: 0.5rem 1rem;
  border: 1px solid var(--gray-300);
  border-radius: 4px;
  width: 250px;
}

.search-input:focus { outline: none; border-color: var(--primary); }

.total-records { color: var(--gray-500); font-size: 0.875rem; }

.data-table { width: 100%; border-collapse: collapse; }

.data-table th, .data-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid var(--gray-200);
}

.data-table th { background: var(--gray-50); font-weight: 600; color: var(--gray-700); }
.data-table tr:hover { background: var(--gray-50); }
.data-table .actions { display: flex; gap: 0.5rem; }

.empty-message { text-align: center; color: var(--gray-500); padding: 2rem !important; }

.pagination {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 1rem;
  margin-top: 1rem;
  padding-top: 1rem;
  border-top: 1px solid var(--gray-200);
}

.pagination button {
  padding: 0.5rem 1rem;
  border: 1px solid var(--gray-300);
  background: var(--white);
  border-radius: 4px;
  cursor: pointer;
}

.pagination button:disabled { opacity: 0.5; cursor: not-allowed; }

.modal-backdrop {
  position: fixed;
  top: 0; left: 0; right: 0; bottom: 0;
  background: rgba(0,0,0,0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal {
  background: var(--white);
  border-radius: 8px;
  width: 100%;
  max-width: 500px;
  max-height: 90vh;
  overflow-y: auto;
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 1.5rem;
  border-bottom: 1px solid var(--gray-200);
}

.modal-header h3 { color: var(--primary); }

.modal-close {
  background: none;
  border: none;
  font-size: 1.5rem;
  cursor: pointer;
  color: var(--gray-500);
}

.modal-body { padding: 1.5rem; }

.modal-footer {
  padding: 1rem 1.5rem;
  border-top: 1px solid var(--gray-200);
  display: flex;
  justify-content: flex-end;
  gap: 0.5rem;
}

.form-group { margin-bottom: 1rem; }
.form-group label { display: block; margin-bottom: 0.5rem; font-weight: 500; color: var(--gray-700); }

.form-group input, .form-group textarea, .form-group select {
  width: 100%;
  padding: 0.75rem;
  border: 1px solid var(--gray-300);
  border-radius: 4px;
  font-size: 1rem;
}

.form-group input:focus, .form-group textarea:focus, .form-group select:focus {
  outline: none;
  border-color: var(--primary);
}

.form-group textarea { min-height: 100px; resize: vertical; }

.alert { padding: 1rem; border-radius: 4px; margin-bottom: 1rem; }
.alert-error { background: #FEE2E2; color: var(--danger); border: 1px solid var(--danger); }
.alert-success { background: #D1FAE5; color: var(--success); border: 1px solid var(--success); }

.loading { display: flex; flex-direction: column; align-items: center; padding: 2rem; }

.spinner {
  width: 40px;
  height: 40px;
  border: 4px solid var(--gray-200);
  border-top-color: var(--primary);
  border-radius: 50%;
  animation: spin 1s linear infinite;
}

@keyframes spin { to { transform: rotate(360deg); } }

.footer {
  background: var(--gray-200);
  padding: 1rem;
  text-align: center;
  color: var(--gray-500);
  font-size: 0.875rem;
}
'''

    def _generate_start_script(self, framework: str) -> str:
        """Gera script de inicializacao."""
        return f"""@echo off
echo ========================================
echo   Iniciando Frontend {framework.title()} - {self.project_id}
echo ========================================
echo.
echo Instalando dependencias...
call npm install
echo.
echo Iniciando servidor de desenvolvimento...
echo Acesse: http://localhost:5173
echo.
call npm run dev
pause
"""

    # ============================================================
    # INICIALIZACAO
    # ============================================================

    def start_frontend(self) -> Dict:
        """Inicia o servidor de desenvolvimento frontend."""
        frontend_dir = self.project_path / "frontend"

        if not frontend_dir.exists():
            return {"success": False, "message": "Frontend nao encontrado. Execute generate_react_app() ou generate_vue_app() primeiro."}

        try:
            process = subprocess.Popen(
                ["npm", "run", "dev"],
                cwd=str(frontend_dir),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                creationflags=subprocess.CREATE_NEW_CONSOLE if os.name == 'nt' else 0,
                shell=True
            )

            return {
                "success": True,
                "message": "Frontend iniciado!",
                "app_url": "http://localhost:5173",
                "pid": process.pid
            }

        except Exception as e:
            return {"success": False, "message": f"Erro: {str(e)}"}

    def install_dependencies(self) -> Dict:
        """Instala dependencias do frontend."""
        frontend_dir = self.project_path / "frontend"

        if not frontend_dir.exists():
            return {"success": False, "message": "Frontend nao encontrado."}

        try:
            result = subprocess.run(
                ["npm", "install"],
                cwd=str(frontend_dir),
                capture_output=True,
                text=True,
                shell=True
            )

            if result.returncode == 0:
                return {"success": True, "message": "Dependencias instaladas com sucesso!"}
            else:
                return {"success": False, "message": f"Erro: {result.stderr}"}

        except Exception as e:
            return {"success": False, "message": f"Erro: {str(e)}"}


# ============================================================
# FUNCOES UTILITARIAS
# ============================================================

def generate_react_frontend(project_path: str, models: List[Dict] = None, project_id: str = None) -> Dict:
    """
    Gera frontend React para um projeto.

    Args:
        project_path: Caminho do projeto
        models: Lista de modelos do backend
        project_id: ID do projeto

    Retorna:
        Resultado da geracao
    """
    generator = FrontendGenerator(project_path, models, project_id)
    return generator.generate_react_app()


def generate_vue_frontend(project_path: str, models: List[Dict] = None, project_id: str = None) -> Dict:
    """
    Gera frontend Vue para um projeto.

    Args:
        project_path: Caminho do projeto
        models: Lista de modelos do backend
        project_id: ID do projeto

    Retorna:
        Resultado da geracao
    """
    generator = FrontendGenerator(project_path, models, project_id)
    return generator.generate_vue_app()


def detect_frontend(project_path: str) -> Dict:
    """
    Detecta frontend existente em um projeto.

    Args:
        project_path: Caminho do projeto

    Retorna:
        Informacoes sobre o frontend detectado
    """
    generator = FrontendGenerator(project_path)
    frontend_type = generator.detect_frontend_type()

    if frontend_type == "react":
        components = generator.detect_react_components()
    elif frontend_type == "vue":
        components = generator.detect_vue_components()
    else:
        components = []

    return {
        "frontend_type": frontend_type,
        "components_count": len(components),
        "components": components
    }
