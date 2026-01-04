# -*- coding: utf-8 -*-
"""
AWS Lambda Manager
==================
Gerenciador de funcoes Lambda para a Plataforma E.

Funcionalidades:
- Criacao e deploy de funcoes Lambda
- Invocacao de funcoes
- Gerenciamento de layers
- Configuracao de triggers
"""

from datetime import datetime
from typing import Any, Dict, List, Optional
import base64
import json
import logging
import zipfile
import os
import tempfile

logger = logging.getLogger(__name__)

# Precos Lambda (por 1M de requisicoes e GB-segundo)
LAMBDA_PRICING = {
    "requests": 0.20,        # Por 1M de requests
    "duration_128mb": 0.0000002083,   # Por ms para 128MB
    "duration_256mb": 0.0000004167,   # Por ms para 256MB
    "duration_512mb": 0.0000008333,   # Por ms para 512MB
    "duration_1024mb": 0.0000016667,  # Por ms para 1024MB
    "duration_2048mb": 0.0000033333,  # Por ms para 2048MB
}

# Runtimes suportados
SUPPORTED_RUNTIMES = {
    "python": ["python3.9", "python3.10", "python3.11", "python3.12"],
    "nodejs": ["nodejs18.x", "nodejs20.x"],
    "java": ["java11", "java17", "java21"],
    "dotnet": ["dotnet6", "dotnet8"],
    "go": ["provided.al2023"],
    "ruby": ["ruby3.2", "ruby3.3"],
}


class LambdaManager:
    """
    Gerenciador de funcoes Lambda.

    Responsavel por todas as operacoes relacionadas ao Lambda:
    - Criacao e atualizacao de funcoes
    - Invocacao sincrona e assincrona
    - Gerenciamento de configuracao
    - Monitoramento e logs
    """

    def __init__(self, lambda_client, iam_client, config):
        """
        Inicializa o gerenciador Lambda.

        Args:
            lambda_client: Cliente boto3 Lambda
            iam_client: Cliente boto3 IAM
            config: Configuracao AWS
        """
        self.client = lambda_client
        self.iam_client = iam_client
        self.config = config
        self.region = config.region

    async def create_function(
        self,
        name: str,
        runtime: str,
        handler: str,
        code_path: str,
        role_arn: Optional[str] = None,
        memory_mb: int = 256,
        timeout_seconds: int = 30,
        environment: Optional[Dict[str, str]] = None,
        description: Optional[str] = None,
        layers: Optional[List[str]] = None,
        vpc_config: Optional[Dict] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Cria uma nova funcao Lambda.

        Args:
            name: Nome da funcao
            runtime: Runtime (python3.11, nodejs18.x, etc)
            handler: Handler (arquivo.funcao)
            code_path: Caminho do codigo (diretorio ou arquivo zip)
            role_arn: ARN da role IAM (cria uma se None)
            memory_mb: Memoria em MB (128-10240)
            timeout_seconds: Timeout em segundos (1-900)
            environment: Variaveis de ambiente
            description: Descricao da funcao
            layers: ARNs de layers a usar
            vpc_config: Configuracao de VPC
            tags: Tags adicionais

        Returns:
            Dicionario com informacoes da funcao criada
        """
        try:
            # Criar role se nao fornecida
            if not role_arn:
                role_arn = await self._create_execution_role(name)
                if not role_arn:
                    return {
                        "success": False,
                        "error": "Falha ao criar role de execucao"
                    }

            # Preparar codigo
            code_zip = await self._prepare_code(code_path)
            if not code_zip:
                return {
                    "success": False,
                    "error": "Falha ao preparar codigo"
                }

            # Parametros da funcao
            params = {
                "FunctionName": name,
                "Runtime": runtime,
                "Role": role_arn,
                "Handler": handler,
                "Code": {"ZipFile": code_zip},
                "MemorySize": memory_mb,
                "Timeout": timeout_seconds,
            }

            if description:
                params["Description"] = description

            if environment:
                params["Environment"] = {"Variables": environment}

            if layers:
                params["Layers"] = layers

            if vpc_config:
                params["VpcConfig"] = vpc_config

            # Tags
            function_tags = {"Project": "FabricaDeAgentes"}
            if tags:
                function_tags.update(tags)
            params["Tags"] = function_tags

            # Criar funcao
            response = self.client.create_function(**params)

            logger.info(f"Funcao Lambda criada: {name}")

            return {
                "success": True,
                "function_name": response["FunctionName"],
                "function_arn": response["FunctionArn"],
                "runtime": response["Runtime"],
                "handler": response["Handler"],
                "memory_mb": response["MemorySize"],
                "timeout": response["Timeout"],
                "last_modified": response["LastModified"],
                "state": response.get("State", "Active"),
            }

        except self.client.exceptions.ResourceConflictException:
            logger.warning(f"Funcao {name} ja existe, atualizando...")
            return await self.update_function_code(name, code_path)

        except Exception as e:
            logger.error(f"Erro ao criar funcao Lambda: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def update_function_code(
        self,
        function_name: str,
        code_path: str
    ) -> Dict[str, Any]:
        """
        Atualiza o codigo de uma funcao.

        Args:
            function_name: Nome da funcao
            code_path: Caminho do novo codigo

        Returns:
            Dicionario com informacoes da atualizacao
        """
        try:
            code_zip = await self._prepare_code(code_path)
            if not code_zip:
                return {
                    "success": False,
                    "error": "Falha ao preparar codigo"
                }

            response = self.client.update_function_code(
                FunctionName=function_name,
                ZipFile=code_zip
            )

            logger.info(f"Codigo da funcao {function_name} atualizado")

            return {
                "success": True,
                "function_name": response["FunctionName"],
                "function_arn": response["FunctionArn"],
                "last_modified": response["LastModified"],
            }

        except Exception as e:
            logger.error(f"Erro ao atualizar funcao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def update_function_config(
        self,
        function_name: str,
        memory_mb: Optional[int] = None,
        timeout_seconds: Optional[int] = None,
        environment: Optional[Dict[str, str]] = None,
        handler: Optional[str] = None,
        runtime: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Atualiza a configuracao de uma funcao.

        Args:
            function_name: Nome da funcao
            memory_mb: Nova memoria
            timeout_seconds: Novo timeout
            environment: Novas variaveis de ambiente
            handler: Novo handler
            runtime: Novo runtime

        Returns:
            Dicionario com informacoes da atualizacao
        """
        try:
            params = {"FunctionName": function_name}

            if memory_mb:
                params["MemorySize"] = memory_mb
            if timeout_seconds:
                params["Timeout"] = timeout_seconds
            if environment:
                params["Environment"] = {"Variables": environment}
            if handler:
                params["Handler"] = handler
            if runtime:
                params["Runtime"] = runtime

            response = self.client.update_function_configuration(**params)

            return {
                "success": True,
                "function_name": response["FunctionName"],
                "memory_mb": response["MemorySize"],
                "timeout": response["Timeout"],
            }

        except Exception as e:
            logger.error(f"Erro ao atualizar configuracao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def invoke_function(
        self,
        function_name: str,
        payload: Optional[Dict[str, Any]] = None,
        async_invoke: bool = False
    ) -> Dict[str, Any]:
        """
        Invoca uma funcao Lambda.

        Args:
            function_name: Nome da funcao
            payload: Payload a enviar
            async_invoke: Se True, invoca de forma assincrona

        Returns:
            Dicionario com resposta da funcao
        """
        try:
            params = {
                "FunctionName": function_name,
                "InvocationType": "Event" if async_invoke else "RequestResponse",
            }

            if payload:
                params["Payload"] = json.dumps(payload)

            response = self.client.invoke(**params)

            result = {
                "success": True,
                "status_code": response["StatusCode"],
                "executed_version": response.get("ExecutedVersion", "$LATEST"),
            }

            # Para invocacao sincrona, incluir resposta
            if not async_invoke and "Payload" in response:
                payload_response = response["Payload"].read()
                try:
                    result["response"] = json.loads(payload_response)
                except json.JSONDecodeError:
                    result["response"] = payload_response.decode("utf-8")

            # Verificar erros
            if "FunctionError" in response:
                result["success"] = False
                result["error"] = response["FunctionError"]

            return result

        except Exception as e:
            logger.error(f"Erro ao invocar funcao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def delete_function(self, function_name: str) -> bool:
        """
        Deleta uma funcao Lambda.

        Args:
            function_name: Nome da funcao

        Returns:
            True se deletou com sucesso
        """
        try:
            self.client.delete_function(FunctionName=function_name)
            logger.info(f"Funcao {function_name} deletada")
            return True

        except Exception as e:
            logger.error(f"Erro ao deletar funcao: {e}")
            return False

    async def get_function(self, function_name: str) -> Dict[str, Any]:
        """
        Obtem informacoes de uma funcao.

        Args:
            function_name: Nome da funcao

        Returns:
            Dicionario com informacoes da funcao
        """
        try:
            response = self.client.get_function(FunctionName=function_name)

            config = response["Configuration"]

            return {
                "function_name": config["FunctionName"],
                "function_arn": config["FunctionArn"],
                "runtime": config["Runtime"],
                "handler": config["Handler"],
                "memory_mb": config["MemorySize"],
                "timeout": config["Timeout"],
                "last_modified": config["LastModified"],
                "state": config.get("State", "Active"),
                "description": config.get("Description", ""),
                "role": config["Role"],
                "code_size": config["CodeSize"],
            }

        except self.client.exceptions.ResourceNotFoundException:
            return {"error": "Funcao nao encontrada"}

        except Exception as e:
            logger.error(f"Erro ao obter funcao: {e}")
            return {"error": str(e)}

    async def list_functions(
        self,
        max_items: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Lista todas as funcoes Lambda.

        Args:
            max_items: Numero maximo de funcoes

        Returns:
            Lista de funcoes
        """
        try:
            response = self.client.list_functions(MaxItems=max_items)

            functions = []
            for func in response.get("Functions", []):
                functions.append({
                    "function_name": func["FunctionName"],
                    "function_arn": func["FunctionArn"],
                    "runtime": func.get("Runtime"),
                    "memory_mb": func["MemorySize"],
                    "timeout": func["Timeout"],
                    "last_modified": func["LastModified"],
                    "code_size": func["CodeSize"],
                })

            return functions

        except Exception as e:
            logger.error(f"Erro ao listar funcoes: {e}")
            return []

    async def add_function_url(
        self,
        function_name: str,
        auth_type: str = "NONE"
    ) -> Dict[str, Any]:
        """
        Adiciona URL publica a uma funcao Lambda.

        Args:
            function_name: Nome da funcao
            auth_type: Tipo de autenticacao (NONE ou AWS_IAM)

        Returns:
            Dicionario com URL da funcao
        """
        try:
            response = self.client.create_function_url_config(
                FunctionName=function_name,
                AuthType=auth_type
            )

            return {
                "success": True,
                "function_url": response["FunctionUrl"],
                "auth_type": response["AuthType"],
            }

        except self.client.exceptions.ResourceConflictException:
            # URL ja existe, obter a existente
            response = self.client.get_function_url_config(
                FunctionName=function_name
            )
            return {
                "success": True,
                "function_url": response["FunctionUrl"],
                "auth_type": response["AuthType"],
                "already_exists": True,
            }

        except Exception as e:
            logger.error(f"Erro ao criar URL da funcao: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _prepare_code(self, code_path: str) -> Optional[bytes]:
        """
        Prepara codigo para deploy (cria zip se necessario).

        Args:
            code_path: Caminho do codigo

        Returns:
            Bytes do arquivo zip ou None se erro
        """
        try:
            # Se ja e um zip, ler diretamente
            if code_path.endswith(".zip") and os.path.isfile(code_path):
                with open(code_path, "rb") as f:
                    return f.read()

            # Se e um diretorio, criar zip
            if os.path.isdir(code_path):
                with tempfile.NamedTemporaryFile(suffix=".zip", delete=False) as tmp:
                    tmp_path = tmp.name

                with zipfile.ZipFile(tmp_path, "w", zipfile.ZIP_DEFLATED) as zf:
                    for root, dirs, files in os.walk(code_path):
                        for file in files:
                            file_path = os.path.join(root, file)
                            arcname = os.path.relpath(file_path, code_path)
                            zf.write(file_path, arcname)

                with open(tmp_path, "rb") as f:
                    zip_content = f.read()

                os.unlink(tmp_path)
                return zip_content

            # Se e um arquivo Python unico, criar zip com ele
            if os.path.isfile(code_path):
                with tempfile.NamedTemporaryFile(suffix=".zip", delete=False) as tmp:
                    tmp_path = tmp.name

                with zipfile.ZipFile(tmp_path, "w", zipfile.ZIP_DEFLATED) as zf:
                    zf.write(code_path, os.path.basename(code_path))

                with open(tmp_path, "rb") as f:
                    zip_content = f.read()

                os.unlink(tmp_path)
                return zip_content

            return None

        except Exception as e:
            logger.error(f"Erro ao preparar codigo: {e}")
            return None

    async def _create_execution_role(self, function_name: str) -> Optional[str]:
        """
        Cria role de execucao para Lambda.

        Args:
            function_name: Nome da funcao (para nome da role)

        Returns:
            ARN da role ou None se erro
        """
        try:
            role_name = f"lambda-{function_name}-role"

            # Politica de confianca
            assume_role_policy = {
                "Version": "2012-10-17",
                "Statement": [
                    {
                        "Effect": "Allow",
                        "Principal": {"Service": "lambda.amazonaws.com"},
                        "Action": "sts:AssumeRole"
                    }
                ]
            }

            # Criar role
            response = self.iam_client.create_role(
                RoleName=role_name,
                AssumeRolePolicyDocument=json.dumps(assume_role_policy),
                Description=f"Role de execucao para Lambda {function_name}",
                Tags=[
                    {"Key": "Project", "Value": "FabricaDeAgentes"}
                ]
            )

            role_arn = response["Role"]["Arn"]

            # Anexar politica basica de logs
            self.iam_client.attach_role_policy(
                RoleName=role_name,
                PolicyArn="arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
            )

            # Aguardar propagacao da role
            import time
            time.sleep(5)

            logger.info(f"Role {role_name} criada")
            return role_arn

        except self.iam_client.exceptions.EntityAlreadyExistsException:
            # Role ja existe, obter ARN
            response = self.iam_client.get_role(RoleName=f"lambda-{function_name}-role")
            return response["Role"]["Arn"]

        except Exception as e:
            logger.error(f"Erro ao criar role: {e}")
            return None

    def estimate_cost(
        self,
        memory_mb: int,
        avg_duration_ms: int,
        monthly_invocations: int
    ) -> Dict[str, float]:
        """
        Estima custo mensal de uma funcao Lambda.

        Args:
            memory_mb: Memoria em MB
            avg_duration_ms: Duracao media em ms
            monthly_invocations: Numero de invocacoes mensais

        Returns:
            Dicionario com estimativas de custo
        """
        # Custo de requests
        request_cost = (monthly_invocations / 1_000_000) * LAMBDA_PRICING["requests"]

        # Custo de duracao (GB-segundo)
        gb_seconds = (memory_mb / 1024) * (avg_duration_ms / 1000) * monthly_invocations

        # Obter preco por GB-segundo baseado na memoria
        if memory_mb <= 128:
            duration_price = LAMBDA_PRICING["duration_128mb"] * 1000  # Converter para segundo
        elif memory_mb <= 256:
            duration_price = LAMBDA_PRICING["duration_256mb"] * 1000
        elif memory_mb <= 512:
            duration_price = LAMBDA_PRICING["duration_512mb"] * 1000
        elif memory_mb <= 1024:
            duration_price = LAMBDA_PRICING["duration_1024mb"] * 1000
        else:
            duration_price = LAMBDA_PRICING["duration_2048mb"] * 1000

        duration_cost = gb_seconds * duration_price

        total_cost = request_cost + duration_cost

        return {
            "request_cost": round(request_cost, 4),
            "duration_cost": round(duration_cost, 4),
            "total_monthly_cost": round(total_cost, 2),
            "gb_seconds": round(gb_seconds, 2),
            "invocations": monthly_invocations,
        }
