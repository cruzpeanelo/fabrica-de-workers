# -*- coding: utf-8 -*-
"""
GCP Cloud Functions Manager
===========================
Gerenciador de Cloud Functions para a Plataforma E.

Funcionalidades:
- Criacao de funcoes (1st e 2nd gen)
- Deploy de codigo
- Invocacao de funcoes
- Gerenciamento de configuracao
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import logging
import json

logger = logging.getLogger(__name__)

# Precos Cloud Functions (por invocacao e GB-segundo)
FUNCTIONS_PRICING = {
    "invocations": 0.40,           # Por 1M de invocacoes
    "compute_128mb": 0.000000231,  # Por 100ms para 128MB
    "compute_256mb": 0.000000463,  # Por 100ms para 256MB
    "compute_512mb": 0.000000925,  # Por 100ms para 512MB
    "compute_1024mb": 0.000001850, # Por 100ms para 1GB
    "compute_2048mb": 0.000003700, # Por 100ms para 2GB
    "networking_egress": 0.12,     # Por GB de egress
    "monthly_free_invocations": 2_000_000,
    "monthly_free_compute_gb_seconds": 400_000,
}

# Runtimes suportados
SUPPORTED_RUNTIMES = {
    "python": ["python39", "python310", "python311", "python312"],
    "nodejs": ["nodejs18", "nodejs20"],
    "go": ["go121", "go122"],
    "java": ["java11", "java17", "java21"],
    "dotnet": ["dotnet6", "dotnet8"],
    "ruby": ["ruby32", "ruby33"],
    "php": ["php82", "php83"],
}


class CloudFunctionsManager:
    """
    Gerenciador de Cloud Functions.

    Responsavel por todas as operacoes relacionadas a Cloud Functions:
    - Criacao e deploy de funcoes
    - Invocacao
    - Monitoramento
    """

    def __init__(self, functions_client, config):
        """
        Inicializa o gerenciador de Functions.

        Args:
            functions_client: Cliente Google Cloud Functions
            config: Configuracao GCP
        """
        self.client = functions_client
        self.config = config
        self.project_id = config.project_id
        self.region = config.region

    async def create_function(
        self,
        name: str,
        runtime: str = "python311",
        entry_point: str = "main",
        source_bucket: Optional[str] = None,
        source_object: Optional[str] = None,
        source_url: Optional[str] = None,
        memory_mb: int = 256,
        timeout_seconds: int = 60,
        max_instances: int = 100,
        min_instances: int = 0,
        environment_variables: Optional[Dict[str, str]] = None,
        trigger_http: bool = True,
        allow_unauthenticated: bool = False,
        service_account: Optional[str] = None,
        labels: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova Cloud Function (2nd gen).

        Args:
            name: Nome da funcao
            runtime: Runtime (python311, nodejs18, etc)
            entry_point: Ponto de entrada da funcao
            source_bucket: Bucket com o codigo fonte
            source_object: Objeto (zip) com o codigo
            source_url: URL do codigo fonte
            memory_mb: Memoria em MB
            timeout_seconds: Timeout em segundos
            max_instances: Maximo de instancias
            min_instances: Minimo de instancias
            environment_variables: Variaveis de ambiente
            trigger_http: Usar trigger HTTP
            allow_unauthenticated: Permitir acesso sem autenticacao
            service_account: Service account a usar
            labels: Labels GCP

        Returns:
            Dicionario com informacoes da funcao criada
        """
        try:
            func_labels = {"project": "plataforma-e"}
            if labels:
                func_labels.update(labels)

            # Nome completo da funcao
            function_name = f"projects/{self.project_id}/locations/{self.region}/functions/{name}"

            # Configuracao de build
            build_config = {
                "runtime": runtime,
                "entryPoint": entry_point,
            }

            # Fonte do codigo
            if source_bucket and source_object:
                build_config["source"] = {
                    "storageSource": {
                        "bucket": source_bucket,
                        "object": source_object,
                    }
                }
            elif source_url:
                build_config["source"] = {
                    "repoSource": {
                        "url": source_url,
                    }
                }
            else:
                # Codigo inline minimo para placeholder
                build_config["source"] = {
                    "storageSource": {
                        "bucket": f"{self.project_id}-functions",
                        "object": f"{name}.zip",
                    }
                }

            # Configuracao de servico
            service_config = {
                "availableMemory": f"{memory_mb}M",
                "timeoutSeconds": timeout_seconds,
                "maxInstanceCount": max_instances,
                "minInstanceCount": min_instances,
            }

            if environment_variables:
                service_config["environmentVariables"] = environment_variables

            if service_account:
                service_config["serviceAccountEmail"] = service_account

            # Configuracao da funcao
            function_config = {
                "name": function_name,
                "buildConfig": build_config,
                "serviceConfig": service_config,
                "labels": func_labels,
            }

            # Trigger HTTP
            if trigger_http:
                function_config["httpsTrigger"] = {
                    "securityLevel": "SECURE_ALWAYS",
                }

            # Criar funcao via API
            parent = f"projects/{self.project_id}/locations/{self.region}"

            operation = self.client.create_function(
                request={
                    "parent": parent,
                    "function": function_config,
                    "function_id": name,
                }
            )

            # Aguardar conclusao
            result = operation.result(timeout=300)

            logger.info(f"Cloud Function criada: {name}")

            # Obter URL se HTTP trigger
            function_url = None
            if hasattr(result, 'service_config') and hasattr(result.service_config, 'uri'):
                function_url = result.service_config.uri

            return {
                "success": True,
                "function_name": name,
                "full_name": function_name,
                "runtime": runtime,
                "entry_point": entry_point,
                "memory_mb": memory_mb,
                "timeout_seconds": timeout_seconds,
                "url": function_url,
                "state": str(result.state) if hasattr(result, 'state') else "ACTIVE",
            }

        except Exception as e:
            logger.error(f"Erro ao criar Cloud Function: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_function(self, name: str) -> bool:
        """
        Deleta uma Cloud Function.

        Args:
            name: Nome da funcao

        Returns:
            True se deletou com sucesso
        """
        try:
            function_name = f"projects/{self.project_id}/locations/{self.region}/functions/{name}"

            operation = self.client.delete_function(name=function_name)
            operation.result(timeout=120)

            logger.info(f"Cloud Function {name} deletada")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar Cloud Function: {e}")
            return False

    async def get_function(self, name: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma Cloud Function.

        Args:
            name: Nome da funcao

        Returns:
            Dicionario com informacoes da funcao
        """
        try:
            function_name = f"projects/{self.project_id}/locations/{self.region}/functions/{name}"

            func = self.client.get_function(name=function_name)

            url = None
            if hasattr(func, 'service_config') and hasattr(func.service_config, 'uri'):
                url = func.service_config.uri

            return {
                "function_name": name,
                "full_name": func.name,
                "state": str(func.state),
                "runtime": func.build_config.runtime if func.build_config else None,
                "entry_point": func.build_config.entry_point if func.build_config else None,
                "url": url,
                "update_time": str(func.update_time) if func.update_time else None,
                "labels": dict(func.labels) if func.labels else {},
            }

        except Exception as e:
            logger.error(f"Erro ao obter Cloud Function: {e}")
            return {"error": str(e)}

    async def list_functions(
        self,
        labels: Optional[Dict[str, str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista Cloud Functions do projeto.

        Args:
            labels: Filtrar por labels

        Returns:
            Lista de funcoes
        """
        try:
            parent = f"projects/{self.project_id}/locations/{self.region}"

            functions = []
            for func in self.client.list_functions(parent=parent):
                # Filtrar por labels
                if labels:
                    func_labels = dict(func.labels) if func.labels else {}
                    if not all(func_labels.get(k) == v for k, v in labels.items()):
                        continue

                url = None
                if hasattr(func, 'service_config') and hasattr(func.service_config, 'uri'):
                    url = func.service_config.uri

                functions.append({
                    "function_name": func.name.split("/")[-1],
                    "full_name": func.name,
                    "state": str(func.state),
                    "runtime": func.build_config.runtime if func.build_config else None,
                    "url": url,
                    "labels": dict(func.labels) if func.labels else {},
                })

            return functions

        except Exception as e:
            logger.error(f"Erro ao listar Cloud Functions: {e}")
            return []

    async def invoke_function(
        self,
        name: str,
        payload: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Invoca uma Cloud Function.

        Args:
            name: Nome da funcao
            payload: Payload a enviar

        Returns:
            Dicionario com resposta da funcao
        """
        try:
            import requests

            # Obter URL da funcao
            func_info = await self.get_function(name)
            url = func_info.get("url")

            if not url:
                return {
                    "success": False,
                    "error": "URL da funcao nao encontrada"
                }

            # Fazer requisicao HTTP
            headers = {"Content-Type": "application/json"}

            if payload:
                response = requests.post(url, json=payload, headers=headers, timeout=60)
            else:
                response = requests.get(url, timeout=60)

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

    async def update_function(
        self,
        name: str,
        memory_mb: Optional[int] = None,
        timeout_seconds: Optional[int] = None,
        environment_variables: Optional[Dict[str, str]] = None,
        max_instances: Optional[int] = None,
        min_instances: Optional[int] = None
    ) -> Dict[str, Any]:
        """
        Atualiza configuracao de uma Cloud Function.

        Args:
            name: Nome da funcao
            memory_mb: Nova memoria
            timeout_seconds: Novo timeout
            environment_variables: Novas variaveis de ambiente
            max_instances: Novo maximo de instancias
            min_instances: Novo minimo de instancias

        Returns:
            Dicionario com informacoes da atualizacao
        """
        try:
            function_name = f"projects/{self.project_id}/locations/{self.region}/functions/{name}"

            # Obter funcao atual
            func = self.client.get_function(name=function_name)

            # Atualizar campos
            update_mask = []

            if memory_mb:
                func.service_config.available_memory = f"{memory_mb}M"
                update_mask.append("service_config.available_memory")

            if timeout_seconds:
                func.service_config.timeout_seconds = timeout_seconds
                update_mask.append("service_config.timeout_seconds")

            if max_instances is not None:
                func.service_config.max_instance_count = max_instances
                update_mask.append("service_config.max_instance_count")

            if min_instances is not None:
                func.service_config.min_instance_count = min_instances
                update_mask.append("service_config.min_instance_count")

            if environment_variables:
                func.service_config.environment_variables = environment_variables
                update_mask.append("service_config.environment_variables")

            # Atualizar
            from google.protobuf import field_mask_pb2

            operation = self.client.update_function(
                request={
                    "function": func,
                    "update_mask": field_mask_pb2.FieldMask(paths=update_mask),
                }
            )

            result = operation.result(timeout=300)

            logger.info(f"Cloud Function {name} atualizada")

            return {
                "success": True,
                "function_name": name,
                "updated_fields": update_mask,
            }

        except Exception as e:
            logger.error(f"Erro ao atualizar funcao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    def estimate_cost(
        self,
        monthly_invocations: int,
        avg_memory_mb: int = 256,
        avg_duration_ms: int = 200,
        egress_gb: float = 0
    ) -> Dict[str, float]:
        """
        Estima custo mensal de Cloud Functions.

        Args:
            monthly_invocations: Numero de invocacoes mensais
            avg_memory_mb: Memoria media em MB
            avg_duration_ms: Duracao media em ms
            egress_gb: Egress em GB

        Returns:
            Dicionario com estimativas de custo
        """
        # Calcular GB-segundos
        gb = avg_memory_mb / 1024
        seconds = avg_duration_ms / 1000
        gb_seconds = monthly_invocations * gb * seconds

        # Aplicar tier gratuito
        billable_invocations = max(0, monthly_invocations - FUNCTIONS_PRICING["monthly_free_invocations"])
        billable_gb_seconds = max(0, gb_seconds - FUNCTIONS_PRICING["monthly_free_compute_gb_seconds"])

        # Custo de invocacoes
        invocation_cost = (billable_invocations / 1_000_000) * FUNCTIONS_PRICING["invocations"]

        # Custo de compute
        # Usar preco baseado na memoria
        if avg_memory_mb <= 128:
            compute_rate = FUNCTIONS_PRICING["compute_128mb"]
        elif avg_memory_mb <= 256:
            compute_rate = FUNCTIONS_PRICING["compute_256mb"]
        elif avg_memory_mb <= 512:
            compute_rate = FUNCTIONS_PRICING["compute_512mb"]
        elif avg_memory_mb <= 1024:
            compute_rate = FUNCTIONS_PRICING["compute_1024mb"]
        else:
            compute_rate = FUNCTIONS_PRICING["compute_2048mb"]

        # Converter para segundos (rate e por 100ms)
        compute_cost = billable_gb_seconds * (compute_rate * 10)

        # Custo de networking
        networking_cost = egress_gb * FUNCTIONS_PRICING["networking_egress"]

        total_cost = invocation_cost + compute_cost + networking_cost

        return {
            "invocation_cost": round(invocation_cost, 4),
            "compute_cost": round(compute_cost, 4),
            "networking_cost": round(networking_cost, 4),
            "total_monthly_cost": round(total_cost, 2),
            "gb_seconds": round(gb_seconds, 2),
            "invocations": monthly_invocations,
            "free_tier_applied": True,
        }
