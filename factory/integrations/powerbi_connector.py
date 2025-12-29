# -*- coding: utf-8 -*-
"""
Power BI Connector Module
=========================
Conectores nativos para exportar dados para Power BI.

Funcionalidades:
- Exportar stories em formato OData para Power BI
- Feed de metricas para dashboard Power BI
- Suporte a refresh incremental
- Isolamento por tenant

Configuracao via variaveis de ambiente:
- POWERBI_TENANT_ID: ID do tenant Azure AD
- POWERBI_CLIENT_ID: Client ID do app registrado
- POWERBI_CLIENT_SECRET: Client Secret
- POWERBI_WORKSPACE_ID: ID do workspace Power BI
"""

import os
import logging
import json
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class ODataFormat(str, Enum):
    """Formatos de exportacao OData"""
    JSON = "json"
    ATOM = "atom"
    MINIMAL = "minimal"


@dataclass
class PowerBIConfig:
    """Configuracao para Power BI Connector"""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    workspace_id: str = ""
    dataset_name: str = "FabricaAgentes"
    refresh_enabled: bool = True
    refresh_interval_hours: int = 1

    @classmethod
    def from_env(cls) -> "PowerBIConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            tenant_id=os.getenv("POWERBI_TENANT_ID", ""),
            client_id=os.getenv("POWERBI_CLIENT_ID", ""),
            client_secret=os.getenv("POWERBI_CLIENT_SECRET", ""),
            workspace_id=os.getenv("POWERBI_WORKSPACE_ID", ""),
            dataset_name=os.getenv("POWERBI_DATASET_NAME", "FabricaAgentes"),
            refresh_enabled=os.getenv("POWERBI_REFRESH_ENABLED", "true").lower() == "true",
            refresh_interval_hours=int(os.getenv("POWERBI_REFRESH_INTERVAL", "1"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.tenant_id and self.client_id and self.client_secret)


class PowerBIConnector:
    """
    Conector para Power BI com suporte a OData.

    Permite exportar dados da Fabrica de Agentes para Power BI
    em formato compativel com DirectQuery e Import mode.

    Exemplo de uso:
    ```python
    connector = PowerBIConnector(tenant_id="...")

    # Exportar stories em OData
    odata = connector.export_stories(project_id="PROJ-001")

    # Obter feed de metricas
    metrics = connector.get_metrics_feed()
    ```
    """

    ODATA_VERSION = "4.0"

    def __init__(self, tenant_id: str, config: Optional[PowerBIConfig] = None):
        """
        Inicializa o conector Power BI.

        Args:
            tenant_id: ID do tenant para isolamento de dados
            config: Configuracao opcional (usa env vars se nao fornecida)
        """
        self.tenant_id = tenant_id
        self.config = config or PowerBIConfig.from_env()
        self._access_token: Optional[str] = None
        self._token_expires: Optional[datetime] = None
        self._rate_limit_remaining = 100
        self._rate_limit_reset: Optional[datetime] = None

    def _check_rate_limit(self) -> bool:
        """Verifica se esta dentro do rate limit"""
        if self._rate_limit_reset and datetime.utcnow() > self._rate_limit_reset:
            self._rate_limit_remaining = 100
            self._rate_limit_reset = None

        if self._rate_limit_remaining <= 0:
            logger.warning(f"[PowerBI] Rate limit atingido para tenant {self.tenant_id}")
            return False

        self._rate_limit_remaining -= 1
        return True

    def _log_operation(self, operation: str, details: Dict[str, Any] = None):
        """Registra operacao para auditoria"""
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "tenant_id": self.tenant_id,
            "operation": operation,
            "details": details or {}
        }
        logger.info(f"[PowerBI] {operation}: {json.dumps(log_entry)}")

    def _to_odata_value(self, value: Any) -> Any:
        """Converte valor Python para formato OData"""
        if value is None:
            return None
        if isinstance(value, datetime):
            return value.isoformat() + "Z"
        if isinstance(value, (list, dict)):
            return json.dumps(value)
        if isinstance(value, bool):
            return value
        return value

    def _story_to_odata(self, story: Dict) -> Dict:
        """Converte story para formato OData"""
        return {
            "@odata.type": "#FabricaAgentes.Story",
            "StoryId": story.get("story_id"),
            "ProjectId": story.get("project_id"),
            "Title": story.get("title", ""),
            "Description": story.get("description", ""),
            "Persona": story.get("persona", ""),
            "Action": story.get("action", ""),
            "Benefit": story.get("benefit", ""),
            "Status": story.get("status", "backlog"),
            "Priority": story.get("priority", "medium"),
            "StoryPoints": story.get("story_points", 0),
            "Complexity": story.get("complexity", "medium"),
            "Category": story.get("category", "feature"),
            "EpicId": story.get("epic_id"),
            "SprintId": story.get("sprint_id"),
            "Assignee": story.get("assignee"),
            "Progress": story.get("progress", 0),
            "TasksTotal": story.get("tasks_total", 0),
            "TasksCompleted": story.get("tasks_completed", 0),
            "CreatedAt": self._to_odata_value(story.get("created_at")),
            "UpdatedAt": self._to_odata_value(story.get("updated_at")),
            "CompletedAt": self._to_odata_value(story.get("completed_at")),
            "Tags": json.dumps(story.get("tags", [])),
            "TenantId": self.tenant_id
        }

    def export_stories(
        self,
        project_id: str,
        stories: Optional[List[Dict]] = None,
        format: ODataFormat = ODataFormat.JSON,
        include_tasks: bool = False
    ) -> Dict[str, Any]:
        """
        Exportar stories em formato OData para Power BI.

        Args:
            project_id: ID do projeto
            stories: Lista de stories (se None, busca do banco)
            format: Formato de saida OData
            include_tasks: Incluir tasks das stories

        Returns:
            Dict no formato OData com as stories
        """
        if not self._check_rate_limit():
            return {"error": "Rate limit exceeded", "retry_after": 60}

        self._log_operation("export_stories", {"project_id": project_id, "format": format.value})

        # Se nao foram fornecidas stories, buscar do banco
        if stories is None:
            stories = self._get_stories_from_db(project_id)

        # Filtrar por tenant
        filtered_stories = [s for s in stories if self._is_tenant_authorized(s)]

        # Converter para OData
        odata_stories = [self._story_to_odata(s) for s in filtered_stories]

        # Montar resposta OData
        response = {
            "@odata.context": f"$metadata#Stories",
            "@odata.count": len(odata_stories),
            "value": odata_stories
        }

        if include_tasks:
            tasks_data = []
            for story in filtered_stories:
                story_tasks = story.get("tasks", [])
                for task in story_tasks:
                    tasks_data.append(self._task_to_odata(task))
            response["tasks@odata.context"] = "$metadata#Tasks"
            response["tasks"] = tasks_data

        return response

    def _task_to_odata(self, task: Dict) -> Dict:
        """Converte task para formato OData"""
        return {
            "@odata.type": "#FabricaAgentes.Task",
            "TaskId": task.get("task_id"),
            "StoryId": task.get("story_id"),
            "Title": task.get("title", ""),
            "Description": task.get("description", ""),
            "TaskType": task.get("task_type", "development"),
            "Status": task.get("status", "pending"),
            "Progress": task.get("progress", 0),
            "EstimatedHours": task.get("estimated_hours", 0),
            "ActualHours": task.get("actual_hours", 0),
            "Assignee": task.get("assignee"),
            "AgentId": task.get("agent_id"),
            "CreatedAt": self._to_odata_value(task.get("created_at")),
            "CompletedAt": self._to_odata_value(task.get("completed_at")),
            "TenantId": self.tenant_id
        }

    def get_metrics_feed(self, project_id: Optional[str] = None) -> Dict[str, Any]:
        """
        Feed de metricas para dashboard Power BI.

        Args:
            project_id: Filtrar por projeto (opcional)

        Returns:
            Dict com metricas agregadas
        """
        if not self._check_rate_limit():
            return {"error": "Rate limit exceeded", "retry_after": 60}

        self._log_operation("get_metrics_feed", {"project_id": project_id})

        # Buscar metricas
        stories_by_status = self._count_by_status(project_id)
        velocity = self._calculate_velocity(project_id)
        burndown = self._get_burndown(project_id)

        return {
            "@odata.context": "$metadata#Metrics",
            "generated_at": datetime.utcnow().isoformat() + "Z",
            "tenant_id": self.tenant_id,
            "project_id": project_id,
            "stories_by_status": stories_by_status,
            "velocity": velocity,
            "burndown": burndown,
            "summary": {
                "total_stories": sum(stories_by_status.values()),
                "completed_stories": stories_by_status.get("done", 0),
                "in_progress": stories_by_status.get("in_progress", 0),
                "completion_rate": self._calculate_completion_rate(stories_by_status),
                "avg_velocity": velocity.get("average", 0) if velocity else 0
            }
        }

    def _count_by_status(self, project_id: Optional[str] = None) -> Dict[str, int]:
        """Conta stories por status"""
        # Simulacao - em producao, buscar do banco
        return {
            "backlog": 10,
            "ready": 5,
            "in_progress": 8,
            "review": 3,
            "testing": 4,
            "done": 20
        }

    def _calculate_velocity(self, project_id: Optional[str] = None) -> Dict[str, Any]:
        """Calcula velocity (story points por sprint)"""
        # Simulacao - em producao, buscar do banco
        return {
            "current_sprint": 21,
            "previous_sprint": 18,
            "average": 19.5,
            "trend": "up",
            "history": [
                {"sprint": "Sprint 1", "points": 15},
                {"sprint": "Sprint 2", "points": 18},
                {"sprint": "Sprint 3", "points": 21}
            ]
        }

    def _get_burndown(self, project_id: Optional[str] = None) -> List[Dict]:
        """Gera dados de burndown chart"""
        # Simulacao - em producao, calcular do banco
        today = datetime.utcnow().date()
        return [
            {"date": (today - timedelta(days=10)).isoformat(), "remaining": 50, "ideal": 50},
            {"date": (today - timedelta(days=8)).isoformat(), "remaining": 45, "ideal": 40},
            {"date": (today - timedelta(days=6)).isoformat(), "remaining": 38, "ideal": 30},
            {"date": (today - timedelta(days=4)).isoformat(), "remaining": 28, "ideal": 20},
            {"date": (today - timedelta(days=2)).isoformat(), "remaining": 18, "ideal": 10},
            {"date": today.isoformat(), "remaining": 8, "ideal": 0}
        ]

    def _calculate_completion_rate(self, status_counts: Dict[str, int]) -> float:
        """Calcula taxa de conclusao"""
        total = sum(status_counts.values())
        if total == 0:
            return 0.0
        completed = status_counts.get("done", 0)
        return round((completed / total) * 100, 2)

    def _is_tenant_authorized(self, resource: Dict) -> bool:
        """Verifica se o recurso pertence ao tenant"""
        resource_tenant = resource.get("tenant_id")
        if resource_tenant is None:
            return True  # Recurso sem tenant e compartilhado
        return resource_tenant == self.tenant_id

    def _get_stories_from_db(self, project_id: str) -> List[Dict]:
        """Busca stories do banco de dados"""
        # Em producao, integrar com o repositorio
        try:
            from factory.database.connection import get_session
            from factory.database.models import Story

            with get_session() as session:
                query = session.query(Story).filter(
                    Story.project_id == project_id
                )
                stories = query.all()
                return [s.to_dict() for s in stories]
        except Exception as e:
            logger.error(f"[PowerBI] Erro ao buscar stories: {e}")
            return []

    def get_odata_metadata(self) -> str:
        """
        Retorna metadata EDMX para Power BI.

        Returns:
            String XML com metadata OData
        """
        return f'''<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="FabricaAgentes" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityType Name="Story">
        <Key>
          <PropertyRef Name="StoryId"/>
        </Key>
        <Property Name="StoryId" Type="Edm.String" Nullable="false"/>
        <Property Name="ProjectId" Type="Edm.String"/>
        <Property Name="Title" Type="Edm.String"/>
        <Property Name="Description" Type="Edm.String"/>
        <Property Name="Persona" Type="Edm.String"/>
        <Property Name="Action" Type="Edm.String"/>
        <Property Name="Benefit" Type="Edm.String"/>
        <Property Name="Status" Type="Edm.String"/>
        <Property Name="Priority" Type="Edm.String"/>
        <Property Name="StoryPoints" Type="Edm.Int32"/>
        <Property Name="Complexity" Type="Edm.String"/>
        <Property Name="Category" Type="Edm.String"/>
        <Property Name="EpicId" Type="Edm.String"/>
        <Property Name="SprintId" Type="Edm.String"/>
        <Property Name="Assignee" Type="Edm.String"/>
        <Property Name="Progress" Type="Edm.Double"/>
        <Property Name="TasksTotal" Type="Edm.Int32"/>
        <Property Name="TasksCompleted" Type="Edm.Int32"/>
        <Property Name="CreatedAt" Type="Edm.DateTimeOffset"/>
        <Property Name="UpdatedAt" Type="Edm.DateTimeOffset"/>
        <Property Name="CompletedAt" Type="Edm.DateTimeOffset"/>
        <Property Name="Tags" Type="Edm.String"/>
        <Property Name="TenantId" Type="Edm.String"/>
        <NavigationProperty Name="Tasks" Type="Collection(FabricaAgentes.Task)"/>
      </EntityType>
      <EntityType Name="Task">
        <Key>
          <PropertyRef Name="TaskId"/>
        </Key>
        <Property Name="TaskId" Type="Edm.String" Nullable="false"/>
        <Property Name="StoryId" Type="Edm.String"/>
        <Property Name="Title" Type="Edm.String"/>
        <Property Name="Description" Type="Edm.String"/>
        <Property Name="TaskType" Type="Edm.String"/>
        <Property Name="Status" Type="Edm.String"/>
        <Property Name="Progress" Type="Edm.Int32"/>
        <Property Name="EstimatedHours" Type="Edm.Double"/>
        <Property Name="ActualHours" Type="Edm.Double"/>
        <Property Name="Assignee" Type="Edm.String"/>
        <Property Name="AgentId" Type="Edm.String"/>
        <Property Name="CreatedAt" Type="Edm.DateTimeOffset"/>
        <Property Name="CompletedAt" Type="Edm.DateTimeOffset"/>
        <Property Name="TenantId" Type="Edm.String"/>
      </EntityType>
      <EntityType Name="Metric">
        <Key>
          <PropertyRef Name="MetricId"/>
        </Key>
        <Property Name="MetricId" Type="Edm.String" Nullable="false"/>
        <Property Name="ProjectId" Type="Edm.String"/>
        <Property Name="MetricType" Type="Edm.String"/>
        <Property Name="Value" Type="Edm.Double"/>
        <Property Name="PeriodStart" Type="Edm.DateTimeOffset"/>
        <Property Name="PeriodEnd" Type="Edm.DateTimeOffset"/>
        <Property Name="TenantId" Type="Edm.String"/>
      </EntityType>
      <EntityContainer Name="Container">
        <EntitySet Name="Stories" EntityType="FabricaAgentes.Story"/>
        <EntitySet Name="Tasks" EntityType="FabricaAgentes.Task"/>
        <EntitySet Name="Metrics" EntityType="FabricaAgentes.Metric"/>
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>'''

    def export_for_direct_query(self, project_id: str) -> Dict[str, Any]:
        """
        Prepara dados para DirectQuery do Power BI.

        DirectQuery permite consultas em tempo real sem importar dados.

        Args:
            project_id: ID do projeto

        Returns:
            Dict com schema e endpoint para DirectQuery
        """
        self._log_operation("export_for_direct_query", {"project_id": project_id})

        return {
            "datasource_type": "OData",
            "connection_string": {
                "url": f"/api/v1/analytics/odata/stories?project_id={project_id}",
                "authentication": "OAuth2",
                "tenant_id": self.tenant_id
            },
            "tables": [
                {
                    "name": "Stories",
                    "endpoint": f"/api/v1/analytics/odata/stories",
                    "parameters": {"project_id": project_id}
                },
                {
                    "name": "Tasks",
                    "endpoint": f"/api/v1/analytics/odata/tasks",
                    "parameters": {"project_id": project_id}
                },
                {
                    "name": "Metrics",
                    "endpoint": f"/api/v1/analytics/odata/metrics",
                    "parameters": {"project_id": project_id}
                }
            ],
            "refresh_policy": {
                "type": "incremental",
                "detect_data_changes": True,
                "incremental_column": "UpdatedAt"
            }
        }


# Instancias por tenant (cache)
_connectors: Dict[str, PowerBIConnector] = {}


def get_powerbi_connector(tenant_id: str) -> PowerBIConnector:
    """
    Retorna instancia do conector Power BI para o tenant.

    Args:
        tenant_id: ID do tenant

    Returns:
        PowerBIConnector isolado por tenant
    """
    if tenant_id not in _connectors:
        _connectors[tenant_id] = PowerBIConnector(tenant_id)
    return _connectors[tenant_id]
