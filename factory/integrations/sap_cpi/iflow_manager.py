# -*- coding: utf-8 -*-
"""
SAP CPI iFlow Manager
=====================

Gerenciamento de Integration Flows (iFlows) no SAP Cloud Platform Integration.

Funcionalidades:
- Listar iFlows de um package
- Criar novos iFlows
- Atualizar iFlows existentes
- Download/Upload de iFlows (ZIP)
- Gerenciar configurações de adaptadores
- Monitorar status de runtime

Os iFlows são o coração das integrações SAP CPI, definindo o fluxo
de processamento de mensagens entre sistemas.
"""

import logging
import base64
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, TYPE_CHECKING
from enum import Enum

if TYPE_CHECKING:
    from .client import SAPCPIClient

logger = logging.getLogger(__name__)


class IFlowStatus(str, Enum):
    """Status de um iFlow no design time"""
    DRAFT = "Draft"
    ACTIVE = "Active"
    ERROR = "Error"


class IFlowRuntimeStatus(str, Enum):
    """Status de um iFlow no runtime"""
    STARTED = "STARTED"
    STOPPED = "STOPPED"
    ERROR = "ERROR"
    STARTING = "STARTING"
    STOPPING = "STOPPING"


class AdapterType(str, Enum):
    """Tipos de adaptadores suportados"""
    HTTP = "HTTP"
    HTTPS = "HTTPS"
    SOAP = "SOAP"
    SFTP = "SFTP"
    FTP = "FTP"
    MAIL = "Mail"
    IDOC = "IDoc"
    RFC = "RFC"
    ODATA = "OData"
    SUCCESSFACTORS = "SuccessFactors"
    ARIBA = "Ariba"
    AS2 = "AS2"
    AS4 = "AS4"
    AMQP = "AMQP"
    KAFKA = "Kafka"
    JDBC = "JDBC"
    LDAP = "LDAP"
    XI = "XI"
    ELSTER = "ELSTER"
    SPLITTER = "Splitter"
    AGGREGATOR = "Aggregator"
    ROUTER = "Router"


@dataclass
class IFlowConfiguration:
    """Configuração de um iFlow"""
    parameter_key: str
    parameter_value: str
    data_type: str = "xsd:string"

    def to_dict(self) -> Dict[str, str]:
        return {
            "ParameterKey": self.parameter_key,
            "ParameterValue": self.parameter_value,
            "DataType": self.data_type
        }


@dataclass
class IntegrationFlow:
    """
    Representa um Integration Flow no SAP CPI.

    Atributos:
        id: ID único do iFlow
        name: Nome amigável
        description: Descrição do iFlow
        version: Versão (ex: 1.0.0, Active)
        package_id: ID do package que contém este iFlow
        artifact_type: Tipo de artefato (sempre IntegrationFlow)
        sender_adapter: Adaptador de entrada
        receiver_adapter: Adaptador de saída
        created_by: Usuário que criou
        created_at: Data de criação
        modified_by: Último usuário que modificou
        modified_at: Data da última modificação
        deployed_by: Usuário que fez deploy
        deployed_on: Worker onde está deployado
        runtime_status: Status no runtime (STARTED, STOPPED, etc)
        configurations: Lista de configurações externalizadas
    """
    id: str
    name: str
    description: str = ""
    version: str = "1.0.0"
    package_id: str = ""
    artifact_type: str = "IntegrationFlow"
    sender_adapter: Optional[str] = None
    receiver_adapter: Optional[str] = None
    created_by: str = ""
    created_at: Optional[datetime] = None
    modified_by: str = ""
    modified_at: Optional[datetime] = None
    deployed_by: str = ""
    deployed_on: str = ""
    runtime_status: IFlowRuntimeStatus = IFlowRuntimeStatus.STOPPED
    configurations: List[IFlowConfiguration] = field(default_factory=list)
    runtime_artifact_id: str = ""

    @classmethod
    def from_odata(cls, data: Dict[str, Any]) -> "IntegrationFlow":
        """
        Cria IntegrationFlow a partir de resposta OData.

        Args:
            data: Dicionário com dados do OData

        Returns:
            Instância de IntegrationFlow
        """
        # Parse configurations se presente
        configs = []
        config_data = data.get("Configurations", {}).get("results", [])
        for cfg in config_data:
            configs.append(IFlowConfiguration(
                parameter_key=cfg.get("ParameterKey", ""),
                parameter_value=cfg.get("ParameterValue", ""),
                data_type=cfg.get("DataType", "xsd:string")
            ))

        return cls(
            id=data.get("Id", ""),
            name=data.get("Name", ""),
            description=data.get("Description", ""),
            version=data.get("Version", "1.0.0"),
            package_id=data.get("PackageId", ""),
            artifact_type=data.get("ArtifactType", "IntegrationFlow"),
            created_by=data.get("CreatedBy", ""),
            created_at=cls._parse_datetime(data.get("CreationDate")),
            modified_by=data.get("ModifiedBy", ""),
            modified_at=cls._parse_datetime(data.get("ModifiedDate")),
            deployed_by=data.get("DeployedBy", ""),
            deployed_on=data.get("DeployedOn", ""),
            runtime_status=IFlowRuntimeStatus(data.get("Status", "STOPPED")) if data.get("Status") else IFlowRuntimeStatus.STOPPED,
            configurations=configs,
            runtime_artifact_id=data.get("RuntimeArtifactId", ""),
        )

    @staticmethod
    def _parse_datetime(value: Any) -> Optional[datetime]:
        """Converte valor OData para datetime"""
        if not value:
            return None
        if isinstance(value, str):
            # Formato: /Date(1234567890000)/
            if "/Date(" in value:
                try:
                    timestamp = int(value.replace("/Date(", "").replace(")/", "")) / 1000
                    return datetime.fromtimestamp(timestamp)
                except (ValueError, TypeError):
                    return None
            # ISO format
            try:
                return datetime.fromisoformat(value.replace("Z", "+00:00"))
            except (ValueError, TypeError):
                return None
        return None

    def to_odata(self) -> Dict[str, Any]:
        """
        Converte para formato OData para criação/atualização.

        Returns:
            Dicionário no formato OData
        """
        return {
            "Id": self.id,
            "Name": self.name,
            "Description": self.description,
            "PackageId": self.package_id,
        }

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionário Python"""
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "version": self.version,
            "package_id": self.package_id,
            "artifact_type": self.artifact_type,
            "sender_adapter": self.sender_adapter,
            "receiver_adapter": self.receiver_adapter,
            "created_by": self.created_by,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "modified_by": self.modified_by,
            "modified_at": self.modified_at.isoformat() if self.modified_at else None,
            "deployed_by": self.deployed_by,
            "deployed_on": self.deployed_on,
            "runtime_status": self.runtime_status.value if self.runtime_status else None,
            "configurations": [c.to_dict() for c in self.configurations],
        }


class IFlowManager:
    """
    Gerenciador de Integration Flows do SAP CPI.

    Exemplo:
    ```python
    manager = IFlowManager(client)

    # Listar iFlows de um package
    iflows = await manager.list_iflows("MyPackageId")

    # Buscar iFlow específico
    iflow = await manager.get_iflow("MyIFlowId", "Active")

    # Download do conteúdo
    content = await manager.download_iflow("MyPackageId", "MyIFlowId")

    # Status no runtime
    status = await manager.get_runtime_status("MyIFlowId")
    ```
    """

    DESIGNTIME_ENDPOINT = "/api/v1/IntegrationDesigntimeArtifacts"
    RUNTIME_ENDPOINT = "/api/v1/IntegrationRuntimeArtifacts"
    CONFIGURATIONS_ENDPOINT = "/api/v1/Configurations"

    def __init__(self, client: "SAPCPIClient"):
        """
        Inicializa o gerenciador de iFlows.

        Args:
            client: Cliente SAPCPIClient autenticado
        """
        self.client = client
        self._cache: Dict[str, IntegrationFlow] = {}

    async def list_iflows(
        self,
        package_id: Optional[str] = None,
        filter_expr: Optional[str] = None,
        select: Optional[List[str]] = None,
        top: int = 100,
        skip: int = 0
    ) -> List[IntegrationFlow]:
        """
        Lista Integration Flows.

        Args:
            package_id: Filtrar por package (opcional)
            filter_expr: Expressão de filtro OData adicional
            select: Campos a selecionar
            top: Número máximo de resultados
            skip: Pular N primeiros resultados (paginação)

        Returns:
            Lista de IntegrationFlow

        Exemplo:
            # Todos os iFlows de um package
            iflows = await manager.list_iflows(package_id="MyPackage")

            # Com filtro adicional
            iflows = await manager.list_iflows(
                filter_expr="substringof('Invoice', Name)"
            )
        """
        params: Dict[str, Any] = {
            "$top": top,
            "$skip": skip,
        }

        # Constrói filtro
        filters = []
        if package_id:
            filters.append(f"PackageId eq '{package_id}'")
        if filter_expr:
            filters.append(filter_expr)

        if filters:
            params["$filter"] = " and ".join(filters)

        if select:
            params["$select"] = ",".join(select)

        response = await self.client.get(self.DESIGNTIME_ENDPOINT, params=params)

        if response is None:
            logger.error("Falha ao listar iFlows")
            return []

        iflows = []
        results = response.get("d", {}).get("results", [])

        for item in results:
            iflow = IntegrationFlow.from_odata(item)
            iflows.append(iflow)
            cache_key = f"{iflow.id}:{iflow.version}"
            self._cache[cache_key] = iflow

        logger.info(f"Listados {len(iflows)} iFlows")
        return iflows

    async def get_iflow(
        self,
        iflow_id: str,
        version: str = "Active",
        expand_configurations: bool = True
    ) -> Optional[IntegrationFlow]:
        """
        Busca um iFlow específico por ID e versão.

        Args:
            iflow_id: ID do iFlow
            version: Versão (default: Active)
            expand_configurations: Incluir configurações externalizadas

        Returns:
            IntegrationFlow ou None se não encontrado

        Exemplo:
            iflow = await manager.get_iflow("MyIFlowId")
            if iflow:
                print(f"iFlow: {iflow.name} - Status: {iflow.runtime_status}")
        """
        cache_key = f"{iflow_id}:{version}"
        if cache_key in self._cache and not expand_configurations:
            return self._cache[cache_key]

        endpoint = f"{self.DESIGNTIME_ENDPOINT}(Id='{iflow_id}',Version='{version}')"
        params = {}

        if expand_configurations:
            params["$expand"] = "Configurations"

        response = await self.client.get(endpoint, params=params)

        if response is None:
            logger.warning(f"iFlow não encontrado: {iflow_id}")
            return None

        data = response.get("d", response)
        iflow = IntegrationFlow.from_odata(data)

        self._cache[cache_key] = iflow
        return iflow

    async def create_iflow(
        self,
        package_id: str,
        iflow_id: str,
        iflow_name: str,
        iflow_content: bytes,
        description: str = ""
    ) -> Optional[IntegrationFlow]:
        """
        Cria um novo iFlow em um package.

        Args:
            package_id: ID do package destino
            iflow_id: ID para o novo iFlow
            iflow_name: Nome do iFlow
            iflow_content: Conteúdo do iFlow em bytes (ZIP ou BPMN2)
            description: Descrição do iFlow

        Returns:
            IntegrationFlow criado ou None se erro

        Exemplo:
            with open("my_iflow.zip", "rb") as f:
                content = f.read()

            iflow = await manager.create_iflow(
                package_id="MyPackage",
                iflow_id="NewIFlow",
                iflow_name="Meu Novo iFlow",
                iflow_content=content,
                description="Integração para processar pedidos"
            )
        """
        # Codifica conteúdo em base64
        content_base64 = base64.b64encode(iflow_content).decode("utf-8")

        data = {
            "Id": iflow_id,
            "Name": iflow_name,
            "Description": description,
            "PackageId": package_id,
            "ArtifactContent": content_base64,
        }

        response = await self.client.post(self.DESIGNTIME_ENDPOINT, data=data)

        if response is None:
            logger.error(f"Falha ao criar iFlow: {iflow_id}")
            return None

        created_data = response.get("d", response)
        iflow = IntegrationFlow.from_odata(created_data)

        logger.info(f"iFlow criado: {iflow.id}")
        return iflow

    async def update_iflow(
        self,
        iflow_id: str,
        version: str,
        iflow_content: bytes
    ) -> bool:
        """
        Atualiza o conteúdo de um iFlow existente.

        Args:
            iflow_id: ID do iFlow
            version: Versão atual
            iflow_content: Novo conteúdo em bytes

        Returns:
            bool: True se atualizado com sucesso

        Nota:
            A atualização cria uma nova versão do iFlow.
        """
        endpoint = f"{self.DESIGNTIME_ENDPOINT}(Id='{iflow_id}',Version='{version}')"

        content_base64 = base64.b64encode(iflow_content).decode("utf-8")
        data = {
            "ArtifactContent": content_base64
        }

        response = await self.client.put(endpoint, data=data)

        if response is None:
            logger.error(f"Falha ao atualizar iFlow: {iflow_id}")
            return False

        # Invalida cache
        cache_key = f"{iflow_id}:{version}"
        self._cache.pop(cache_key, None)

        logger.info(f"iFlow atualizado: {iflow_id}")
        return True

    async def delete_iflow(
        self,
        iflow_id: str,
        version: str = "Active"
    ) -> bool:
        """
        Deleta um iFlow.

        Args:
            iflow_id: ID do iFlow
            version: Versão a deletar

        Returns:
            bool: True se deletado com sucesso

        Atenção:
            O iFlow precisa estar STOPPED antes de deletar.
        """
        endpoint = f"{self.DESIGNTIME_ENDPOINT}(Id='{iflow_id}',Version='{version}')"

        success = await self.client.delete(endpoint)

        if success:
            cache_key = f"{iflow_id}:{version}"
            self._cache.pop(cache_key, None)
            logger.info(f"iFlow deletado: {iflow_id}")
        else:
            logger.error(f"Falha ao deletar iFlow: {iflow_id}")

        return success

    async def download_iflow(
        self,
        iflow_id: str,
        version: str = "Active"
    ) -> Optional[bytes]:
        """
        Faz download do conteúdo de um iFlow.

        Args:
            iflow_id: ID do iFlow
            version: Versão (default: Active)

        Returns:
            bytes com conteúdo ZIP ou None se erro

        O arquivo ZIP contém:
        - META-INF/MANIFEST.MF
        - src/main/resources/*.iflw (BPMN2)
        - src/main/resources/script/*.groovy
        - src/main/resources/mapping/*.mmap

        Exemplo:
            content = await manager.download_iflow("MyIFlowId")
            if content:
                with open("iflow_backup.zip", "wb") as f:
                    f.write(content)
        """
        endpoint = f"{self.DESIGNTIME_ENDPOINT}(Id='{iflow_id}',Version='{version}')/$value"

        content = await self.client.download(endpoint)

        if content:
            logger.info(f"iFlow baixado: {iflow_id} ({len(content)} bytes)")
        else:
            logger.error(f"Falha ao baixar iFlow: {iflow_id}")

        return content

    async def get_runtime_status(
        self,
        iflow_id: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtém status de runtime de um iFlow deployado.

        Args:
            iflow_id: ID do iFlow

        Returns:
            Dict com informações de runtime ou None se não deployado

        Informações retornadas:
        - status: STARTED, STOPPED, ERROR, etc
        - deployed_by: Usuário que fez deploy
        - deployed_on: Worker onde está rodando
        - version: Versão deployada
        - error_info: Detalhes de erro se houver

        Exemplo:
            status = await manager.get_runtime_status("MyIFlowId")
            if status:
                print(f"Status: {status['status']}")
                if status['status'] == 'ERROR':
                    print(f"Erro: {status.get('error_info')}")
        """
        endpoint = f"{self.RUNTIME_ENDPOINT}('{iflow_id}')"

        response = await self.client.get(endpoint)

        if response is None:
            logger.warning(f"iFlow não encontrado no runtime: {iflow_id}")
            return None

        data = response.get("d", response)

        return {
            "id": data.get("Id", ""),
            "version": data.get("Version", ""),
            "name": data.get("Name", ""),
            "type": data.get("Type", ""),
            "deployed_by": data.get("DeployedBy", ""),
            "deployed_on": data.get("DeployedOn", ""),
            "status": data.get("Status", ""),
            "error_info": data.get("ErrorInformation", ""),
        }

    async def list_runtime_artifacts(
        self,
        top: int = 100,
        skip: int = 0,
        filter_expr: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista todos os artefatos no runtime.

        Args:
            top: Número máximo de resultados
            skip: Pular N primeiros resultados
            filter_expr: Filtro OData opcional

        Returns:
            Lista de artefatos com status de runtime

        Exemplo:
            # Todos os artefatos
            artifacts = await manager.list_runtime_artifacts()

            # Apenas os com erro
            errors = await manager.list_runtime_artifacts(
                filter_expr="Status eq 'ERROR'"
            )
        """
        params: Dict[str, Any] = {
            "$top": top,
            "$skip": skip,
        }

        if filter_expr:
            params["$filter"] = filter_expr

        response = await self.client.get(self.RUNTIME_ENDPOINT, params=params)

        if response is None:
            logger.error("Falha ao listar artefatos de runtime")
            return []

        results = response.get("d", {}).get("results", [])

        artifacts = []
        for item in results:
            artifacts.append({
                "id": item.get("Id", ""),
                "version": item.get("Version", ""),
                "name": item.get("Name", ""),
                "type": item.get("Type", ""),
                "deployed_by": item.get("DeployedBy", ""),
                "deployed_on": item.get("DeployedOn", ""),
                "status": item.get("Status", ""),
                "error_info": item.get("ErrorInformation", ""),
            })

        logger.info(f"Listados {len(artifacts)} artefatos de runtime")
        return artifacts

    async def get_configurations(
        self,
        iflow_id: str,
        version: str = "Active"
    ) -> List[IFlowConfiguration]:
        """
        Lista configurações externalizadas de um iFlow.

        Args:
            iflow_id: ID do iFlow
            version: Versão

        Returns:
            Lista de IFlowConfiguration

        As configurações externalizadas permitem alterar parâmetros
        sem modificar o iFlow em si.

        Exemplo:
            configs = await manager.get_configurations("MyIFlowId")
            for config in configs:
                print(f"{config.parameter_key} = {config.parameter_value}")
        """
        endpoint = f"{self.DESIGNTIME_ENDPOINT}(Id='{iflow_id}',Version='{version}')/Configurations"

        response = await self.client.get(endpoint)

        if response is None:
            logger.warning(f"Não foi possível obter configurações: {iflow_id}")
            return []

        results = response.get("d", {}).get("results", [])

        configs = []
        for item in results:
            configs.append(IFlowConfiguration(
                parameter_key=item.get("ParameterKey", ""),
                parameter_value=item.get("ParameterValue", ""),
                data_type=item.get("DataType", "xsd:string")
            ))

        return configs

    async def update_configuration(
        self,
        iflow_id: str,
        parameter_key: str,
        parameter_value: str,
        version: str = "Active"
    ) -> bool:
        """
        Atualiza uma configuração externalizada.

        Args:
            iflow_id: ID do iFlow
            parameter_key: Chave do parâmetro
            parameter_value: Novo valor
            version: Versão do iFlow

        Returns:
            bool: True se atualizado com sucesso

        Exemplo:
            success = await manager.update_configuration(
                iflow_id="MyIFlowId",
                parameter_key="endpoint_url",
                parameter_value="https://new-endpoint.com/api"
            )
        """
        endpoint = f"{self.CONFIGURATIONS_ENDPOINT}(ArtifactId='{iflow_id}',Version='{version}',ParameterKey='{parameter_key}')"

        data = {
            "ParameterValue": parameter_value
        }

        response = await self.client.put(endpoint, data=data)

        if response is None:
            logger.error(f"Falha ao atualizar configuração: {parameter_key}")
            return False

        logger.info(f"Configuração atualizada: {parameter_key} = {parameter_value}")
        return True

    async def search_iflows(
        self,
        query: str,
        package_id: Optional[str] = None
    ) -> List[IntegrationFlow]:
        """
        Busca iFlows por texto.

        Args:
            query: Texto para buscar
            package_id: Filtrar por package (opcional)

        Returns:
            Lista de IntegrationFlow que correspondem à busca

        Exemplo:
            # Buscar iFlows de invoice
            iflows = await manager.search_iflows("invoice")

            # Buscar em package específico
            iflows = await manager.search_iflows("order", package_id="SalesPackage")
        """
        filters = [f"substringof('{query}', Name) or substringof('{query}', Description)"]

        if package_id:
            filters.append(f"PackageId eq '{package_id}'")

        filter_expr = " and ".join(filters)

        return await self.list_iflows(filter_expr=filter_expr)

    def clear_cache(self):
        """Limpa o cache local de iFlows"""
        self._cache.clear()
        logger.debug("Cache de iFlows limpo")

    async def get_statistics(
        self,
        package_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Retorna estatísticas sobre os iFlows.

        Args:
            package_id: Filtrar por package (opcional)

        Returns:
            Dicionário com estatísticas
        """
        iflows = await self.list_iflows(package_id=package_id, top=1000)
        runtime = await self.list_runtime_artifacts(top=1000)

        # Conta por status de runtime
        runtime_by_status: Dict[str, int] = {}
        for item in runtime:
            status = item.get("status", "Unknown")
            runtime_by_status[status] = runtime_by_status.get(status, 0) + 1

        return {
            "total_iflows": len(iflows),
            "total_runtime": len(runtime),
            "runtime_by_status": runtime_by_status,
        }
