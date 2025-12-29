# -*- coding: utf-8 -*-
"""
Tableau Connector Module
========================
Conector para exportar dados para Tableau.

Funcionalidades:
- Exportar dados em formato Tableau Data Extract (TDE/Hyper)
- Web Data Connector (WDC) para conexao direta
- API REST para integracao
- Isolamento por tenant

Configuracao via variaveis de ambiente:
- TABLEAU_SERVER_URL: URL do Tableau Server/Online
- TABLEAU_SITE_ID: ID do site
- TABLEAU_API_TOKEN: Token de API
- TABLEAU_PROJECT_ID: ID do projeto Tableau
"""

import os
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class TableauDataFormat(str, Enum):
    """Formatos de dados para Tableau"""
    JSON = "json"
    CSV = "csv"
    HYPER = "hyper"
    TDE = "tde"


@dataclass
class TableauConfig:
    """Configuracao para Tableau Connector"""
    server_url: str = ""
    site_id: str = ""
    api_token: str = ""
    project_id: str = ""
    api_version: str = "3.18"

    @classmethod
    def from_env(cls) -> "TableauConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            server_url=os.getenv("TABLEAU_SERVER_URL", ""),
            site_id=os.getenv("TABLEAU_SITE_ID", ""),
            api_token=os.getenv("TABLEAU_API_TOKEN", ""),
            project_id=os.getenv("TABLEAU_PROJECT_ID", ""),
            api_version=os.getenv("TABLEAU_API_VERSION", "3.18")
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.server_url and self.api_token)


class TableauConnector:
    """
    Conector para Tableau com suporte a WDC e Data Extract.

    Permite exportar dados da Fabrica de Agentes para Tableau
    em varios formatos, incluindo Web Data Connector.

    Exemplo de uso:
    ```python
    connector = TableauConnector(tenant_id="...")

    # Exportar para WDC
    wdc_data = connector.get_wdc_data(project_id="PROJ-001")

    # Gerar schema para Tableau
    schema = connector.get_tableau_schema()
    ```
    """

    def __init__(self, tenant_id: str, config: Optional[TableauConfig] = None):
        """
        Inicializa o conector Tableau.

        Args:
            tenant_id: ID do tenant para isolamento de dados
            config: Configuracao opcional
        """
        self.tenant_id = tenant_id
        self.config = config or TableauConfig.from_env()
        self._rate_limit_remaining = 100
        self._rate_limit_reset: Optional[datetime] = None

    def _check_rate_limit(self) -> bool:
        """Verifica se esta dentro do rate limit"""
        if self._rate_limit_reset and datetime.utcnow() > self._rate_limit_reset:
            self._rate_limit_remaining = 100
            self._rate_limit_reset = None

        if self._rate_limit_remaining <= 0:
            logger.warning(f"[Tableau] Rate limit atingido para tenant {self.tenant_id}")
            return False

        self._rate_limit_remaining -= 1
        return True

    def _log_operation(self, operation: str, details: Dict[str, Any] = None):
        """Registra operacao para auditoria"""
        logger.info(f"[Tableau] {operation} - tenant: {self.tenant_id}, details: {details}")

    def get_tableau_schema(self) -> Dict[str, Any]:
        """
        Retorna schema de dados para Tableau.

        Returns:
            Dict com definicao de tabelas e colunas
        """
        return {
            "tables": [
                {
                    "id": "stories",
                    "alias": "Funcionalidades",
                    "columns": [
                        {"id": "story_id", "alias": "ID", "dataType": "string"},
                        {"id": "project_id", "alias": "Projeto", "dataType": "string"},
                        {"id": "title", "alias": "Titulo", "dataType": "string"},
                        {"id": "description", "alias": "Descricao", "dataType": "string"},
                        {"id": "persona", "alias": "Persona", "dataType": "string"},
                        {"id": "action", "alias": "Acao", "dataType": "string"},
                        {"id": "benefit", "alias": "Beneficio", "dataType": "string"},
                        {"id": "status", "alias": "Status", "dataType": "string"},
                        {"id": "priority", "alias": "Prioridade", "dataType": "string"},
                        {"id": "story_points", "alias": "Story Points", "dataType": "int"},
                        {"id": "complexity", "alias": "Complexidade", "dataType": "string"},
                        {"id": "category", "alias": "Categoria", "dataType": "string"},
                        {"id": "epic_id", "alias": "Epico", "dataType": "string"},
                        {"id": "sprint_id", "alias": "Sprint", "dataType": "string"},
                        {"id": "assignee", "alias": "Responsavel", "dataType": "string"},
                        {"id": "progress", "alias": "Progresso", "dataType": "float"},
                        {"id": "tasks_total", "alias": "Total Tasks", "dataType": "int"},
                        {"id": "tasks_completed", "alias": "Tasks Concluidas", "dataType": "int"},
                        {"id": "created_at", "alias": "Criado em", "dataType": "datetime"},
                        {"id": "updated_at", "alias": "Atualizado em", "dataType": "datetime"},
                        {"id": "completed_at", "alias": "Concluido em", "dataType": "datetime"},
                        {"id": "tenant_id", "alias": "Tenant", "dataType": "string"}
                    ]
                },
                {
                    "id": "tasks",
                    "alias": "Tarefas",
                    "columns": [
                        {"id": "task_id", "alias": "ID", "dataType": "string"},
                        {"id": "story_id", "alias": "Funcionalidade", "dataType": "string"},
                        {"id": "title", "alias": "Titulo", "dataType": "string"},
                        {"id": "task_type", "alias": "Tipo", "dataType": "string"},
                        {"id": "status", "alias": "Status", "dataType": "string"},
                        {"id": "progress", "alias": "Progresso", "dataType": "int"},
                        {"id": "estimated_hours", "alias": "Horas Estimadas", "dataType": "float"},
                        {"id": "actual_hours", "alias": "Horas Reais", "dataType": "float"},
                        {"id": "assignee", "alias": "Responsavel", "dataType": "string"},
                        {"id": "agent_id", "alias": "Agente IA", "dataType": "string"},
                        {"id": "created_at", "alias": "Criado em", "dataType": "datetime"},
                        {"id": "completed_at", "alias": "Concluido em", "dataType": "datetime"},
                        {"id": "tenant_id", "alias": "Tenant", "dataType": "string"}
                    ]
                },
                {
                    "id": "metrics",
                    "alias": "Metricas",
                    "columns": [
                        {"id": "metric_id", "alias": "ID", "dataType": "string"},
                        {"id": "project_id", "alias": "Projeto", "dataType": "string"},
                        {"id": "metric_type", "alias": "Tipo", "dataType": "string"},
                        {"id": "value", "alias": "Valor", "dataType": "float"},
                        {"id": "period_start", "alias": "Inicio Periodo", "dataType": "datetime"},
                        {"id": "period_end", "alias": "Fim Periodo", "dataType": "datetime"},
                        {"id": "tenant_id", "alias": "Tenant", "dataType": "string"}
                    ]
                }
            ],
            "standardConnections": [
                {
                    "alias": "Funcionalidades com Tarefas",
                    "tables": [
                        {"id": "stories", "alias": "Funcionalidades"},
                        {"id": "tasks", "alias": "Tarefas"}
                    ],
                    "joins": [
                        {
                            "left": {"tableAlias": "Funcionalidades", "columnId": "story_id"},
                            "right": {"tableAlias": "Tarefas", "columnId": "story_id"},
                            "joinType": "left"
                        }
                    ]
                }
            ]
        }

    def get_wdc_data(
        self,
        project_id: str,
        table_id: str = "stories",
        stories: Optional[List[Dict]] = None,
        include_metadata: bool = True
    ) -> Dict[str, Any]:
        """
        Retorna dados formatados para Web Data Connector.

        Args:
            project_id: ID do projeto
            table_id: ID da tabela (stories, tasks, metrics)
            stories: Lista de stories (opcional)
            include_metadata: Incluir metadata WDC

        Returns:
            Dict com dados formatados para WDC
        """
        if not self._check_rate_limit():
            return {"error": "Rate limit exceeded", "retry_after": 60}

        self._log_operation("get_wdc_data", {"project_id": project_id, "table_id": table_id})

        # Buscar dados se nao fornecidos
        if stories is None:
            stories = self._get_stories_from_db(project_id)

        # Filtrar por tenant
        stories = [s for s in stories if self._is_tenant_authorized(s)]

        # Formatar dados conforme tabela solicitada
        if table_id == "stories":
            data = self._format_stories_for_wdc(stories)
        elif table_id == "tasks":
            data = self._format_tasks_for_wdc(stories)
        elif table_id == "metrics":
            data = self._format_metrics_for_wdc(project_id, stories)
        else:
            return {"error": f"Tabela desconhecida: {table_id}"}

        response = {
            "data": data,
            "hasMore": False,
            "incrementColumn": "updated_at"
        }

        if include_metadata:
            response["schema"] = self.get_tableau_schema()

        return response

    def _format_stories_for_wdc(self, stories: List[Dict]) -> List[Dict]:
        """Formata stories para WDC"""
        return [
            {
                "story_id": s.get("story_id"),
                "project_id": s.get("project_id"),
                "title": s.get("title", ""),
                "description": s.get("description", ""),
                "persona": s.get("persona", ""),
                "action": s.get("action", ""),
                "benefit": s.get("benefit", ""),
                "status": s.get("status", "backlog"),
                "priority": s.get("priority", "medium"),
                "story_points": s.get("story_points", 0),
                "complexity": s.get("complexity", "medium"),
                "category": s.get("category", "feature"),
                "epic_id": s.get("epic_id"),
                "sprint_id": s.get("sprint_id"),
                "assignee": s.get("assignee"),
                "progress": s.get("progress", 0),
                "tasks_total": s.get("tasks_total", 0),
                "tasks_completed": s.get("tasks_completed", 0),
                "created_at": self._format_datetime(s.get("created_at")),
                "updated_at": self._format_datetime(s.get("updated_at")),
                "completed_at": self._format_datetime(s.get("completed_at")),
                "tenant_id": self.tenant_id
            }
            for s in stories
        ]

    def _format_tasks_for_wdc(self, stories: List[Dict]) -> List[Dict]:
        """Formata tasks para WDC"""
        tasks = []
        for story in stories:
            story_tasks = story.get("tasks", [])
            for task in story_tasks:
                tasks.append({
                    "task_id": task.get("task_id"),
                    "story_id": story.get("story_id"),
                    "title": task.get("title", ""),
                    "task_type": task.get("task_type", "development"),
                    "status": task.get("status", "pending"),
                    "progress": task.get("progress", 0),
                    "estimated_hours": task.get("estimated_hours", 0),
                    "actual_hours": task.get("actual_hours", 0),
                    "assignee": task.get("assignee"),
                    "agent_id": task.get("agent_id"),
                    "created_at": self._format_datetime(task.get("created_at")),
                    "completed_at": self._format_datetime(task.get("completed_at")),
                    "tenant_id": self.tenant_id
                })
        return tasks

    def _format_metrics_for_wdc(self, project_id: str, stories: List[Dict]) -> List[Dict]:
        """Formata metricas para WDC"""
        now = datetime.utcnow()
        metrics = []

        # Velocity
        total_points = sum(s.get("story_points", 0) for s in stories)
        done_points = sum(s.get("story_points", 0) for s in stories if s.get("status") == "done")

        metrics.append({
            "metric_id": f"velocity_{project_id}_{now.strftime('%Y%m')}",
            "project_id": project_id,
            "metric_type": "velocity",
            "value": done_points,
            "period_start": now.replace(day=1).isoformat(),
            "period_end": now.isoformat(),
            "tenant_id": self.tenant_id
        })

        # Throughput
        done_count = len([s for s in stories if s.get("status") == "done"])
        metrics.append({
            "metric_id": f"throughput_{project_id}_{now.strftime('%Y%m')}",
            "project_id": project_id,
            "metric_type": "throughput",
            "value": done_count,
            "period_start": now.replace(day=1).isoformat(),
            "period_end": now.isoformat(),
            "tenant_id": self.tenant_id
        })

        # Completion rate
        total = len(stories)
        completion_rate = (done_count / total * 100) if total > 0 else 0
        metrics.append({
            "metric_id": f"completion_{project_id}_{now.strftime('%Y%m')}",
            "project_id": project_id,
            "metric_type": "completion_rate",
            "value": completion_rate,
            "period_start": now.replace(day=1).isoformat(),
            "period_end": now.isoformat(),
            "tenant_id": self.tenant_id
        })

        return metrics

    def _format_datetime(self, dt: Any) -> Optional[str]:
        """Formata datetime para Tableau"""
        if dt is None:
            return None
        if isinstance(dt, str):
            return dt
        if isinstance(dt, datetime):
            return dt.isoformat()
        return str(dt)

    def get_wdc_html(self, project_id: str) -> str:
        """
        Retorna HTML do Web Data Connector.

        Args:
            project_id: ID do projeto

        Returns:
            str: HTML do conector WDC
        """
        self._log_operation("get_wdc_html", {"project_id": project_id})

        return f'''<!DOCTYPE html>
<html>
<head>
    <title>Fabrica de Agentes - Tableau Connector</title>
    <meta charset="utf-8">
    <script src="https://connectors.tableau.com/libs/tableauwdc-2.3.latest.js"></script>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: linear-gradient(135deg, #003B4A 0%, #006880 100%);
            min-height: 100vh;
            margin: 0;
            display: flex;
            align-items: center;
            justify-content: center;
        }}
        .container {{
            background: white;
            padding: 40px;
            border-radius: 12px;
            box-shadow: 0 10px 40px rgba(0,0,0,0.3);
            max-width: 500px;
            width: 90%;
        }}
        h1 {{
            color: #003B4A;
            margin: 0 0 20px 0;
            font-size: 24px;
        }}
        .logo {{
            font-size: 32px;
            margin-bottom: 20px;
        }}
        label {{
            display: block;
            margin-bottom: 8px;
            color: #333;
            font-weight: 500;
        }}
        select, input {{
            width: 100%;
            padding: 12px;
            margin-bottom: 20px;
            border: 2px solid #ddd;
            border-radius: 6px;
            font-size: 16px;
            box-sizing: border-box;
        }}
        select:focus, input:focus {{
            border-color: #003B4A;
            outline: none;
        }}
        button {{
            width: 100%;
            padding: 14px;
            background: #FF6C00;
            color: white;
            border: none;
            border-radius: 6px;
            font-size: 16px;
            font-weight: 600;
            cursor: pointer;
            transition: background 0.3s;
        }}
        button:hover {{
            background: #e55f00;
        }}
        .info {{
            margin-top: 20px;
            padding: 15px;
            background: #f0f9ff;
            border-radius: 6px;
            font-size: 14px;
            color: #0369a1;
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="logo">Fabrica de Agentes</div>
        <h1>Conectar ao Tableau</h1>

        <label for="project">Projeto:</label>
        <input type="text" id="project" value="{project_id}" placeholder="ID do Projeto">

        <label for="table">Dados:</label>
        <select id="table">
            <option value="stories">Funcionalidades (Stories)</option>
            <option value="tasks">Tarefas</option>
            <option value="metrics">Metricas</option>
        </select>

        <button id="connect">Conectar</button>

        <div class="info">
            Este conector permite visualizar dados da Fabrica de Agentes
            diretamente no Tableau Desktop ou Tableau Server.
        </div>
    </div>

    <script>
        (function() {{
            var connector = tableau.makeConnector();

            connector.getSchema = function(schemaCallback) {{
                var projectId = tableau.connectionData ? JSON.parse(tableau.connectionData).project : '{project_id}';
                var tableId = tableau.connectionData ? JSON.parse(tableau.connectionData).table : 'stories';

                fetch('/api/v1/analytics/tableau/schema')
                    .then(response => response.json())
                    .then(schema => {{
                        var table = schema.tables.find(t => t.id === tableId);
                        if (table) {{
                            var tableInfo = {{
                                id: table.id,
                                alias: table.alias,
                                columns: table.columns
                            }};
                            schemaCallback([tableInfo]);
                        }}
                    }});
            }};

            connector.getData = function(table, doneCallback) {{
                var connData = JSON.parse(tableau.connectionData);

                fetch('/api/v1/analytics/tableau/data?project_id=' + connData.project + '&table=' + connData.table)
                    .then(response => response.json())
                    .then(result => {{
                        table.appendRows(result.data);
                        doneCallback();
                    }});
            }};

            tableau.registerConnector(connector);

            document.getElementById('connect').addEventListener('click', function() {{
                var projectId = document.getElementById('project').value;
                var tableId = document.getElementById('table').value;

                tableau.connectionData = JSON.stringify({{
                    project: projectId,
                    table: tableId
                }});
                tableau.connectionName = 'Fabrica de Agentes - ' + tableId;
                tableau.submit();
            }});
        }})();
    </script>
</body>
</html>'''

    def export_to_hyper(self, project_id: str, stories: Optional[List[Dict]] = None) -> Dict[str, Any]:
        """
        Prepara dados para exportacao no formato Hyper.

        Nota: A criacao do arquivo .hyper requer o Tableau Hyper API
        que precisa ser instalado separadamente.

        Args:
            project_id: ID do projeto
            stories: Lista de stories (opcional)

        Returns:
            Dict com dados preparados para Hyper
        """
        if not self._check_rate_limit():
            return {"error": "Rate limit exceeded"}

        self._log_operation("export_to_hyper", {"project_id": project_id})

        if stories is None:
            stories = self._get_stories_from_db(project_id)

        stories = [s for s in stories if self._is_tenant_authorized(s)]

        # Retornar dados estruturados que podem ser usados pelo Hyper API
        return {
            "format": "hyper",
            "project_id": project_id,
            "tenant_id": self.tenant_id,
            "tables": {
                "stories": {
                    "schema": self._get_hyper_schema("stories"),
                    "data": self._format_stories_for_wdc(stories)
                },
                "tasks": {
                    "schema": self._get_hyper_schema("tasks"),
                    "data": self._format_tasks_for_wdc(stories)
                }
            },
            "generated_at": datetime.utcnow().isoformat(),
            "instructions": "Use o Tableau Hyper API para criar o arquivo .hyper"
        }

    def _get_hyper_schema(self, table_id: str) -> Dict[str, str]:
        """Retorna schema para Hyper API"""
        schemas = {
            "stories": {
                "story_id": "text",
                "project_id": "text",
                "title": "text",
                "description": "text",
                "status": "text",
                "priority": "text",
                "story_points": "int",
                "progress": "double",
                "created_at": "timestamp",
                "updated_at": "timestamp"
            },
            "tasks": {
                "task_id": "text",
                "story_id": "text",
                "title": "text",
                "status": "text",
                "progress": "int",
                "estimated_hours": "double",
                "actual_hours": "double",
                "created_at": "timestamp"
            }
        }
        return schemas.get(table_id, {})

    def _is_tenant_authorized(self, resource: Dict) -> bool:
        """Verifica se o recurso pertence ao tenant"""
        resource_tenant = resource.get("tenant_id")
        if resource_tenant is None:
            return True
        return resource_tenant == self.tenant_id

    def _get_stories_from_db(self, project_id: str) -> List[Dict]:
        """Busca stories do banco de dados"""
        try:
            from factory.database.connection import get_session
            from factory.database.models import Story, StoryTask

            with get_session() as session:
                stories = session.query(Story).filter(
                    Story.project_id == project_id
                ).all()

                result = []
                for story in stories:
                    story_dict = story.to_dict()
                    tasks = session.query(StoryTask).filter(
                        StoryTask.story_id == story.story_id
                    ).all()
                    story_dict["tasks"] = [t.to_dict() for t in tasks]
                    result.append(story_dict)

                return result
        except Exception as e:
            logger.error(f"[Tableau] Erro ao buscar stories: {e}")
            return []


# Cache de conectores por tenant
_connectors: Dict[str, TableauConnector] = {}


def get_tableau_connector(tenant_id: str) -> TableauConnector:
    """
    Retorna instancia do conector Tableau para o tenant.

    Args:
        tenant_id: ID do tenant

    Returns:
        TableauConnector isolado por tenant
    """
    if tenant_id not in _connectors:
        _connectors[tenant_id] = TableauConnector(tenant_id)
    return _connectors[tenant_id]
