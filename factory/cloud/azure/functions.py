# -*- coding: utf-8 -*-
"""
Azure Functions Manager
=======================
Gerenciador de Azure Functions para a Fabrica de Agentes.

Funcionalidades:
- Criacao de Function Apps
- Deploy de funcoes
- Invocacao de funcoes
- Gerenciamento de configuracao
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import json
import base64

logger = logging.getLogger(__name__)

# Precos Azure Functions (Consumption plan)
FUNCTIONS_PRICING = {
    "executions": 0.20,           # Por 1M de execucoes
    "gb_seconds": 0.000016,       # Por GB-segundo
    "monthly_free_executions": 1_000_000,
    "monthly_free_gb_seconds": 400_000,
}

# Runtimes suportados
SUPPORTED_RUNTIMES = {
    "python": ["3.9", "3.10", "3.11"],
    "node": ["18", "20"],
    "dotnet": ["6.0", "7.0", "8.0"],
    "java": ["11", "17"],
    "powershell": ["7.2"],
}


class FunctionsManager:
    """
    Gerenciador de Azure Functions.

    Responsavel por todas as operacoes relacionadas a Functions:
    - Criacao de Function Apps
    - Deploy de funcoes
    - Invocacao e monitoramento
    """

    def __init__(self, web_client, storage_client, config):
        """
        Inicializa o gerenciador de Functions.

        Args:
            web_client: Cliente Azure Web Management
            storage_client: Cliente Azure Storage Management
            config: Configuracao Azure
        """
        self.web_client = web_client
        self.storage_client = storage_client
        self.config = config
        self.resource_group = config.resource_group
        self.location = config.location

    async def create_function_app(
        self,
        name: str,
        runtime: str = "python",
        runtime_version: str = "3.11",
        storage_account_name: Optional[str] = None,
        app_settings: Optional[Dict[str, str]] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova Function App.

        Args:
            name: Nome da Function App
            runtime: Runtime (python, node, dotnet)
            runtime_version: Versao do runtime
            storage_account_name: Storage account para a function
            app_settings: Configuracoes da app
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes da Function App
        """
        try:
            func_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                func_tags.update(tags)

            # Criar storage account se nao fornecida
            if not storage_account_name:
                storage_account_name = f"{name.lower().replace('-', '')}storage"[:24]
                await self._create_storage_for_function(storage_account_name, func_tags)

            # Obter connection string do storage
            keys = self.storage_client.storage_accounts.list_keys(
                self.resource_group, storage_account_name
            )
            storage_connection = (
                f"DefaultEndpointsProtocol=https;"
                f"AccountName={storage_account_name};"
                f"AccountKey={keys.keys[0].value};"
                f"EndpointSuffix=core.windows.net"
            )

            # Configuracoes da app
            settings = {
                "FUNCTIONS_WORKER_RUNTIME": runtime,
                "FUNCTIONS_EXTENSION_VERSION": "~4",
                "AzureWebJobsStorage": storage_connection,
                "WEBSITE_CONTENTAZUREFILECONNECTIONSTRING": storage_connection,
                "WEBSITE_CONTENTSHARE": name.lower(),
            }
            if app_settings:
                settings.update(app_settings)

            # Configurar runtime
            site_config = {
                "app_settings": [
                    {"name": k, "value": v}
                    for k, v in settings.items()
                ],
            }

            # Configurar runtime especifico
            if runtime == "python":
                site_config["linux_fx_version"] = f"PYTHON|{runtime_version}"
            elif runtime == "node":
                site_config["linux_fx_version"] = f"NODE|{runtime_version}"

            # Criar Function App
            function_app = {
                "location": self.location,
                "kind": "functionapp,linux",
                "tags": func_tags,
                "reserved": True,  # Linux
                "site_config": site_config,
                "https_only": True,
            }

            poller = self.web_client.web_apps.begin_create_or_update(
                self.resource_group,
                name,
                function_app
            )
            app = poller.result()

            logger.info(f"Function App criada: {name}")

            return {
                "success": True,
                "app_name": app.name,
                "app_id": app.id,
                "default_hostname": app.default_host_name,
                "url": f"https://{app.default_host_name}",
                "runtime": runtime,
                "runtime_version": runtime_version,
                "state": app.state,
            }

        except Exception as e:
            logger.error(f"Erro ao criar Function App: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _create_storage_for_function(
        self,
        name: str,
        tags: Dict[str, str]
    ) -> bool:
        """Cria storage account para a function"""
        try:
            params = {
                "location": self.location,
                "sku": {"name": "Standard_LRS"},
                "kind": "StorageV2",
                "tags": tags,
            }

            poller = self.storage_client.storage_accounts.begin_create(
                self.resource_group, name, params
            )
            poller.result()
            return True
        except Exception as e:
            logger.warning(f"Erro ao criar storage para function: {e}")
            return False

    async def delete_function_app(self, name: str) -> bool:
        """
        Deleta uma Function App.

        Args:
            name: Nome da Function App

        Returns:
            True se deletou com sucesso
        """
        try:
            self.web_client.web_apps.delete(self.resource_group, name)
            logger.info(f"Function App {name} deletada")
            return True
        except Exception as e:
            logger.error(f"Erro ao deletar Function App: {e}")
            return False

    async def get_function_app(self, name: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma Function App.

        Args:
            name: Nome da Function App

        Returns:
            Dicionario com informacoes da Function App
        """
        try:
            app = self.web_client.web_apps.get(self.resource_group, name)

            return {
                "app_name": app.name,
                "app_id": app.id,
                "default_hostname": app.default_host_name,
                "url": f"https://{app.default_host_name}",
                "state": app.state,
                "location": app.location,
                "tags": dict(app.tags) if app.tags else {},
            }

        except Exception as e:
            logger.error(f"Erro ao obter Function App: {e}")
            return {"error": str(e)}

    async def list_function_apps(
        self,
        tags: Optional[Dict[str, str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista Function Apps do resource group.

        Args:
            tags: Filtrar por tags

        Returns:
            Lista de Function Apps
        """
        try:
            apps_list = self.web_client.web_apps.list_by_resource_group(
                self.resource_group
            )

            apps = []
            for app in apps_list:
                # Filtrar apenas Function Apps
                if not app.kind or "functionapp" not in app.kind.lower():
                    continue

                # Filtrar por tags
                if tags:
                    if not app.tags:
                        continue
                    match = all(app.tags.get(k) == v for k, v in tags.items())
                    if not match:
                        continue

                apps.append({
                    "app_name": app.name,
                    "app_id": app.id,
                    "url": f"https://{app.default_host_name}",
                    "state": app.state,
                    "location": app.location,
                })

            return apps

        except Exception as e:
            logger.error(f"Erro ao listar Function Apps: {e}")
            return []

    async def list_functions(self, app_name: str) -> List[Dict[str, Any]]:
        """
        Lista funcoes dentro de uma Function App.

        Args:
            app_name: Nome da Function App

        Returns:
            Lista de funcoes
        """
        try:
            functions_list = self.web_client.web_apps.list_functions(
                self.resource_group, app_name
            )

            functions = []
            for func in functions_list:
                functions.append({
                    "name": func.name.split("/")[-1],
                    "id": func.id,
                    "invoke_url": func.invoke_url_template,
                })

            return functions

        except Exception as e:
            logger.error(f"Erro ao listar funcoes: {e}")
            return []

    async def invoke_function(
        self,
        app_name: str,
        function_name: str,
        payload: Optional[Dict[str, Any]] = None,
        method: str = "POST"
    ) -> Dict[str, Any]:
        """
        Invoca uma funcao Azure.

        Args:
            app_name: Nome da Function App
            function_name: Nome da funcao
            payload: Payload a enviar
            method: Metodo HTTP

        Returns:
            Dicionario com resposta da funcao
        """
        try:
            import requests

            # Obter chave de invocacao
            keys = self.web_client.web_apps.list_function_keys(
                self.resource_group, app_name, function_name
            )

            function_key = None
            if keys.additional_properties:
                function_key = list(keys.additional_properties.values())[0]

            # URL de invocacao
            url = f"https://{app_name}.azurewebsites.net/api/{function_name}"

            headers = {"Content-Type": "application/json"}
            if function_key:
                headers["x-functions-key"] = function_key

            if method.upper() == "POST":
                response = requests.post(
                    url,
                    json=payload,
                    headers=headers,
                    timeout=30
                )
            else:
                response = requests.get(
                    url,
                    params=payload,
                    headers=headers,
                    timeout=30
                )

            return {
                "success": response.ok,
                "status_code": response.status_code,
                "response": response.json() if response.content else None,
            }

        except Exception as e:
            logger.error(f"Erro ao invocar funcao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def update_app_settings(
        self,
        app_name: str,
        settings: Dict[str, str]
    ) -> bool:
        """
        Atualiza configuracoes de uma Function App.

        Args:
            app_name: Nome da Function App
            settings: Novas configuracoes

        Returns:
            True se atualizou com sucesso
        """
        try:
            # Obter configuracoes atuais
            current = self.web_client.web_apps.list_application_settings(
                self.resource_group, app_name
            )

            # Mesclar configuracoes
            if current.properties:
                current.properties.update(settings)
            else:
                current.properties = settings

            # Atualizar
            self.web_client.web_apps.update_application_settings(
                self.resource_group, app_name, current
            )

            logger.info(f"Configuracoes de {app_name} atualizadas")
            return True

        except Exception as e:
            logger.error(f"Erro ao atualizar configuracoes: {e}")
            return False

    async def start_function_app(self, app_name: str) -> bool:
        """
        Inicia uma Function App.

        Args:
            app_name: Nome da Function App

        Returns:
            True se iniciou com sucesso
        """
        try:
            self.web_client.web_apps.start(self.resource_group, app_name)
            logger.info(f"Function App {app_name} iniciada")
            return True
        except Exception as e:
            logger.error(f"Erro ao iniciar Function App: {e}")
            return False

    async def stop_function_app(self, app_name: str) -> bool:
        """
        Para uma Function App.

        Args:
            app_name: Nome da Function App

        Returns:
            True se parou com sucesso
        """
        try:
            self.web_client.web_apps.stop(self.resource_group, app_name)
            logger.info(f"Function App {app_name} parada")
            return True
        except Exception as e:
            logger.error(f"Erro ao parar Function App: {e}")
            return False

    async def restart_function_app(self, app_name: str) -> bool:
        """
        Reinicia uma Function App.

        Args:
            app_name: Nome da Function App

        Returns:
            True se reiniciou com sucesso
        """
        try:
            self.web_client.web_apps.restart(self.resource_group, app_name)
            logger.info(f"Function App {app_name} reiniciada")
            return True
        except Exception as e:
            logger.error(f"Erro ao reiniciar Function App: {e}")
            return False

    def estimate_cost(
        self,
        monthly_executions: int,
        avg_memory_mb: int = 256,
        avg_duration_ms: int = 200
    ) -> Dict[str, float]:
        """
        Estima custo mensal de Azure Functions.

        Args:
            monthly_executions: Numero de execucoes mensais
            avg_memory_mb: Memoria media em MB
            avg_duration_ms: Duracao media em ms

        Returns:
            Dicionario com estimativas de custo
        """
        # Calcular GB-segundos
        gb = avg_memory_mb / 1024
        seconds = avg_duration_ms / 1000
        gb_seconds = monthly_executions * gb * seconds

        # Aplicar tier gratuito
        billable_executions = max(0, monthly_executions - FUNCTIONS_PRICING["monthly_free_executions"])
        billable_gb_seconds = max(0, gb_seconds - FUNCTIONS_PRICING["monthly_free_gb_seconds"])

        # Calcular custos
        execution_cost = (billable_executions / 1_000_000) * FUNCTIONS_PRICING["executions"]
        compute_cost = billable_gb_seconds * FUNCTIONS_PRICING["gb_seconds"]

        total_cost = execution_cost + compute_cost

        return {
            "execution_cost": round(execution_cost, 4),
            "compute_cost": round(compute_cost, 4),
            "total_monthly_cost": round(total_cost, 2),
            "gb_seconds": round(gb_seconds, 2),
            "executions": monthly_executions,
            "free_tier_applied": True,
        }
