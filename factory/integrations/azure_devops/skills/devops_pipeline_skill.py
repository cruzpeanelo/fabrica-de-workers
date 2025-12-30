# -*- coding: utf-8 -*-
"""
Azure DevOps Pipeline Skill
===========================
Skill para gerenciamento de pipelines e builds do Azure DevOps.

Funcionalidades:
- Listagem de pipelines
- Disparo de builds
- Status de builds
- Logs de builds

Uso pelos agentes:
    from factory.integrations.azure_devops.skills import DevOpsPipelineSkill

    skill = DevOpsPipelineSkill(azure_client)

    # Listar pipelines
    result = await skill.get_pipelines()

    # Disparar build
    result = await skill.trigger_build(pipeline_id=123)

    # Status do build
    result = await skill.get_build_status(build_id=456)
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class BuildStatus(str, Enum):
    """Status de build no Azure DevOps"""
    NONE = "none"
    IN_PROGRESS = "inProgress"
    COMPLETED = "completed"
    CANCELLING = "cancelling"
    POSTPONED = "postponed"
    NOT_STARTED = "notStarted"
    ALL = "all"


class BuildResult(str, Enum):
    """Resultado de build no Azure DevOps"""
    NONE = "none"
    SUCCEEDED = "succeeded"
    PARTIALLY_SUCCEEDED = "partiallySucceeded"
    FAILED = "failed"
    CANCELED = "canceled"


@dataclass
class SkillResult:
    """Resultado padrao de uma skill"""
    success: bool
    data: Any = None
    message: str = ""
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "errors": self.errors
        }


class DevOpsPipelineSkill:
    """
    Skill para gerenciamento de pipelines do Azure DevOps.

    Fornece funcionalidades para listar, disparar e monitorar
    pipelines de CI/CD.
    """

    skill_name = "devops_pipeline"
    skill_description = "Gerenciamento de pipelines e builds do Azure DevOps"

    # Acoes disponiveis para Claude
    available_actions = [
        "get_pipelines",
        "trigger_build",
        "get_build_status",
        "get_build_logs"
    ]

    # Schema de ferramenta compativel com Claude
    tool_schema = {
        "name": "devops_pipeline",
        "description": "Gerenciamento de pipelines e builds do Azure DevOps (listar, disparar, status, logs)",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "description": "Acao a executar",
                    "enum": ["get_pipelines", "trigger_build", "get_build_status", "get_build_logs"]
                },
                "params": {
                    "type": "object",
                    "description": "Parametros da acao",
                    "properties": {
                        "pipeline_id": {
                            "type": "integer",
                            "description": "ID do pipeline (para trigger_build)"
                        },
                        "build_id": {
                            "type": "integer",
                            "description": "ID do build (para get_build_status, get_build_logs)"
                        },
                        "branch": {
                            "type": "string",
                            "description": "Branch para build (padrao: main)"
                        },
                        "parameters": {
                            "type": "object",
                            "description": "Parametros customizados para o build"
                        },
                        "folder": {
                            "type": "string",
                            "description": "Pasta para filtrar pipelines"
                        },
                        "max_results": {
                            "type": "integer",
                            "description": "Maximo de resultados (padrao: 50)"
                        }
                    }
                }
            },
            "required": ["action"]
        }
    }

    def __init__(self, azure_client):
        """
        Inicializa a skill.

        Args:
            azure_client: AzureDevOpsIntegration autenticado
        """
        self.azure = azure_client

    # ==================== PIPELINES ====================

    async def get_pipelines(
        self,
        folder: Optional[str] = None,
        max_results: int = 50
    ) -> SkillResult:
        """
        Lista pipelines do projeto.

        Args:
            folder: Pasta para filtrar (ex: "\\Build\\CI")
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de pipelines
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            session = await self.azure._ensure_session()

            # API de Pipelines (YAML)
            url = f"{self.azure.project_url}/_apis/pipelines"
            params = {
                "api-version": "7.1",
                "$top": max_results
            }

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    pipelines = data.get("value", [])

                    # Filtra por pasta se especificado
                    if folder:
                        pipelines = [
                            p for p in pipelines
                            if p.get("folder", "").startswith(folder)
                        ]

                    simplified = []
                    for p in pipelines:
                        simplified.append({
                            "id": p.get("id"),
                            "name": p.get("name"),
                            "folder": p.get("folder"),
                            "revision": p.get("revision"),
                            "url": p.get("url"),
                            "_links": {
                                "web": p.get("_links", {}).get("web", {}).get("href")
                            }
                        })

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message=f"Encontrados {len(simplified)} pipelines"
                    )
                else:
                    error_text = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao listar pipelines: {response.status}",
                        errors=[error_text]
                    )

        except Exception as e:
            logger.error(f"Erro ao listar pipelines: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao listar pipelines: {str(e)}",
                errors=[str(e)]
            )

    async def trigger_build(
        self,
        pipeline_id: int,
        branch: str = "main",
        parameters: Optional[Dict[str, str]] = None
    ) -> SkillResult:
        """
        Dispara um build em um pipeline.

        Args:
            pipeline_id: ID do pipeline
            branch: Branch para build (padrao: main)
            parameters: Parametros customizados do pipeline

        Returns:
            SkillResult com informacoes do build disparado
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            session = await self.azure._ensure_session()

            # API de Pipelines para disparar run
            url = f"{self.azure.project_url}/_apis/pipelines/{pipeline_id}/runs"
            params = {"api-version": "7.1"}

            # Corpo da requisicao
            body = {
                "resources": {
                    "repositories": {
                        "self": {
                            "refName": f"refs/heads/{branch}"
                        }
                    }
                }
            }

            # Adiciona parametros customizados se fornecidos
            if parameters:
                body["templateParameters"] = parameters

            async with session.post(url, params=params, json=body) as response:
                if response.status in (200, 201):
                    run = await response.json()

                    result_data = {
                        "run_id": run.get("id"),
                        "name": run.get("name"),
                        "state": run.get("state"),
                        "result": run.get("result"),
                        "created_date": run.get("createdDate"),
                        "url": run.get("url"),
                        "web_url": run.get("_links", {}).get("web", {}).get("href"),
                        "pipeline": {
                            "id": run.get("pipeline", {}).get("id"),
                            "name": run.get("pipeline", {}).get("name")
                        }
                    }

                    return SkillResult(
                        success=True,
                        data=result_data,
                        message=f"Build disparado com sucesso: Run #{run.get('id')}"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Pipeline {pipeline_id} nao encontrado",
                        errors=["Pipeline nao existe"]
                    )
                else:
                    error_text = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao disparar build: {response.status}",
                        errors=[error_text]
                    )

        except Exception as e:
            logger.error(f"Erro ao disparar build no pipeline {pipeline_id}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao disparar build: {str(e)}",
                errors=[str(e)]
            )

    async def get_build_status(self, build_id: int) -> SkillResult:
        """
        Obtem status de um build.

        Args:
            build_id: ID do build/run

        Returns:
            SkillResult com status do build
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            session = await self.azure._ensure_session()

            # Usa API de Builds (mais detalhada)
            url = f"{self.azure.project_url}/_apis/build/builds/{build_id}"
            params = {"api-version": "7.1"}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    build = await response.json()

                    # Calcula duracao se concluido
                    duration = None
                    if build.get("finishTime") and build.get("startTime"):
                        try:
                            start = datetime.fromisoformat(
                                build["startTime"].replace("Z", "+00:00")
                            )
                            finish = datetime.fromisoformat(
                                build["finishTime"].replace("Z", "+00:00")
                            )
                            duration_secs = (finish - start).total_seconds()
                            duration = f"{int(duration_secs // 60)}m {int(duration_secs % 60)}s"
                        except Exception:
                            pass

                    result_data = {
                        "id": build.get("id"),
                        "build_number": build.get("buildNumber"),
                        "status": build.get("status"),
                        "result": build.get("result"),
                        "source_branch": build.get("sourceBranch"),
                        "source_version": build.get("sourceVersion"),
                        "queue_time": build.get("queueTime"),
                        "start_time": build.get("startTime"),
                        "finish_time": build.get("finishTime"),
                        "duration": duration,
                        "definition": {
                            "id": build.get("definition", {}).get("id"),
                            "name": build.get("definition", {}).get("name")
                        },
                        "requested_for": build.get("requestedFor", {}).get("displayName"),
                        "logs_url": build.get("logs", {}).get("url"),
                        "web_url": build.get("_links", {}).get("web", {}).get("href")
                    }

                    status_msg = result_data["status"]
                    if result_data["result"]:
                        status_msg += f" ({result_data['result']})"

                    return SkillResult(
                        success=True,
                        data=result_data,
                        message=f"Build #{build.get('buildNumber')}: {status_msg}"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Build {build_id} nao encontrado",
                        errors=["Build nao existe"]
                    )
                else:
                    error_text = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar build: {response.status}",
                        errors=[error_text]
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar status do build {build_id}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao buscar status: {str(e)}",
                errors=[str(e)]
            )

    async def get_build_logs(
        self,
        build_id: int,
        log_id: Optional[int] = None
    ) -> SkillResult:
        """
        Obtem logs de um build.

        Args:
            build_id: ID do build
            log_id: ID do log especifico (opcional, lista todos se nao informado)

        Returns:
            SkillResult com logs do build
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            session = await self.azure._ensure_session()

            if log_id:
                # Busca log especifico
                url = f"{self.azure.project_url}/_apis/build/builds/{build_id}/logs/{log_id}"
                params = {"api-version": "7.1"}

                async with session.get(url, params=params) as response:
                    if response.status == 200:
                        # Log vem como texto
                        log_content = await response.text()

                        return SkillResult(
                            success=True,
                            data={
                                "build_id": build_id,
                                "log_id": log_id,
                                "content": log_content
                            },
                            message=f"Log {log_id} do build {build_id} obtido"
                        )
                    else:
                        error_text = await response.text()
                        return SkillResult(
                            success=False,
                            message=f"Erro ao buscar log: {response.status}",
                            errors=[error_text]
                        )
            else:
                # Lista todos os logs
                url = f"{self.azure.project_url}/_apis/build/builds/{build_id}/logs"
                params = {"api-version": "7.1"}

                async with session.get(url, params=params) as response:
                    if response.status == 200:
                        data = await response.json()
                        logs = data.get("value", [])

                        simplified = []
                        for log in logs:
                            simplified.append({
                                "id": log.get("id"),
                                "type": log.get("type"),
                                "url": log.get("url"),
                                "line_count": log.get("lineCount"),
                                "created_on": log.get("createdOn")
                            })

                        return SkillResult(
                            success=True,
                            data={
                                "build_id": build_id,
                                "logs": simplified,
                                "total_logs": len(simplified)
                            },
                            message=f"Build {build_id} tem {len(simplified)} logs"
                        )
                    elif response.status == 404:
                        return SkillResult(
                            success=False,
                            message=f"Build {build_id} nao encontrado",
                            errors=["Build nao existe"]
                        )
                    else:
                        error_text = await response.text()
                        return SkillResult(
                            success=False,
                            message=f"Erro ao listar logs: {response.status}",
                            errors=[error_text]
                        )

        except Exception as e:
            logger.error(f"Erro ao buscar logs do build {build_id}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao buscar logs: {str(e)}",
                errors=[str(e)]
            )

    # ==================== SKILL INTERFACE ====================

    async def execute(
        self,
        action: str,
        params: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Executa uma acao do skill.

        Args:
            action: Acao a executar
            params: Parametros da acao

        Returns:
            Resultado da acao como dicionario
        """
        params = params or {}

        actions = {
            "get_pipelines": self._action_get_pipelines,
            "trigger_build": self._action_trigger_build,
            "get_build_status": self._action_get_build_status,
            "get_build_logs": self._action_get_build_logs
        }

        handler = actions.get(action)
        if not handler:
            return {
                "success": False,
                "error": f"Acao desconhecida: {action}. Acoes disponiveis: {', '.join(self.available_actions)}"
            }

        try:
            result = await handler(params)
            return result.to_dict()
        except Exception as e:
            logger.error(f"Erro ao executar skill {action}: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _action_get_pipelines(self, params: Dict) -> SkillResult:
        """Acao para listar pipelines"""
        return await self.get_pipelines(
            folder=params.get("folder"),
            max_results=params.get("max_results", 50)
        )

    async def _action_trigger_build(self, params: Dict) -> SkillResult:
        """Acao para disparar build"""
        pipeline_id = params.get("pipeline_id")
        if not pipeline_id:
            return SkillResult(
                success=False,
                message="Parametro pipeline_id obrigatorio",
                errors=["pipeline_id nao informado"]
            )

        return await self.trigger_build(
            pipeline_id=int(pipeline_id),
            branch=params.get("branch", "main"),
            parameters=params.get("parameters")
        )

    async def _action_get_build_status(self, params: Dict) -> SkillResult:
        """Acao para buscar status de build"""
        build_id = params.get("build_id")
        if not build_id:
            return SkillResult(
                success=False,
                message="Parametro build_id obrigatorio",
                errors=["build_id nao informado"]
            )

        return await self.get_build_status(int(build_id))

    async def _action_get_build_logs(self, params: Dict) -> SkillResult:
        """Acao para buscar logs de build"""
        build_id = params.get("build_id")
        if not build_id:
            return SkillResult(
                success=False,
                message="Parametro build_id obrigatorio",
                errors=["build_id nao informado"]
            )

        return await self.get_build_logs(
            build_id=int(build_id),
            log_id=params.get("log_id")
        )
