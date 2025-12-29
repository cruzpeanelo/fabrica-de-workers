# -*- coding: utf-8 -*-
"""
Salesforce Connector
====================
Conector principal para integracao com Salesforce CRM.

Este modulo fornece uma interface unificada para todas as
funcionalidades de integracao com Salesforce, incluindo:

- Conexao via OAuth 2.0 ou Username/Password
- REST API para operacoes CRUD
- Metadata API para deploy de componentes
- Tooling API para desenvolvimento Apex
- Bulk API para operacoes em massa
- Analisadores de objetos, codigo e fluxos
- Geradores de codigo Apex, LWC e Flows
- Skills para agentes especializados

Exemplo de uso:
    from factory.integrations.salesforce_connector import SalesforceConnector

    # Criar conector
    connector = SalesforceConnector.from_env()

    # Conectar
    await connector.connect()

    # Usar funcionalidades
    result = await connector.query("SELECT Id, Name FROM Account LIMIT 10")
    analysis = await connector.analyze_object("Account")
    code = connector.generate_apex_class("AccountService", "Account")

    # Skills para agentes
    skill = connector.get_read_skill()
    result = await skill.list_custom_objects()
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional

from .salesforce import (
    SalesforceConfig,
    SalesforceOAuthConfig,
    SalesforceClient,
    MetadataClient,
    ToolingClient,
    BulkClient
)
from .salesforce.analyzers import (
    ObjectAnalyzer,
    ApexAnalyzer,
    FlowAnalyzer,
    LWCAnalyzer
)
from .salesforce.generators import (
    ApexGenerator,
    LWCGenerator,
    FlowGenerator,
    ObjectGenerator
)
from .salesforce.deployers import (
    MetadataDeployer,
    SFDXDeployer
)
from .salesforce.skills import (
    SalesforceReadSkill,
    SalesforceApexSkill,
    SalesforceDeploySkill
)

logger = logging.getLogger(__name__)


@dataclass
class ConnectionStatus:
    """Status da conexao com Salesforce"""
    connected: bool = False
    instance_url: Optional[str] = None
    username: Optional[str] = None
    organization_id: Optional[str] = None
    api_version: str = "59.0"
    is_sandbox: bool = False
    connected_at: Optional[datetime] = None
    api_calls_used: int = 0
    api_calls_limit: int = 0


class SalesforceConnector:
    """
    Conector principal para Salesforce

    Fornece acesso unificado a todas as funcionalidades
    de integracao com Salesforce.
    """

    def __init__(self, config: SalesforceConfig):
        """
        Inicializa o conector

        Args:
            config: Configuracao de conexao Salesforce
        """
        self.config = config
        self._client: Optional[SalesforceClient] = None
        self._metadata: Optional[MetadataClient] = None
        self._tooling: Optional[ToolingClient] = None
        self._bulk: Optional[BulkClient] = None

        # Analyzers
        self._object_analyzer: Optional[ObjectAnalyzer] = None
        self._apex_analyzer: Optional[ApexAnalyzer] = None
        self._flow_analyzer: Optional[FlowAnalyzer] = None
        self._lwc_analyzer: Optional[LWCAnalyzer] = None

        # Generators
        self._apex_generator: Optional[ApexGenerator] = None
        self._lwc_generator: Optional[LWCGenerator] = None
        self._flow_generator: Optional[FlowGenerator] = None
        self._object_generator: Optional[ObjectGenerator] = None

        # Deployers
        self._metadata_deployer: Optional[MetadataDeployer] = None
        self._sfdx_deployer: Optional[SFDXDeployer] = None

        # Skills
        self._read_skill: Optional[SalesforceReadSkill] = None
        self._apex_skill: Optional[SalesforceApexSkill] = None
        self._deploy_skill: Optional[SalesforceDeploySkill] = None

        self._connection_status = ConnectionStatus()

    @classmethod
    def from_env(cls) -> "SalesforceConnector":
        """
        Cria conector a partir de variaveis de ambiente

        Variaveis:
            SALESFORCE_USERNAME
            SALESFORCE_PASSWORD
            SALESFORCE_SECURITY_TOKEN
            SALESFORCE_DOMAIN
            SALESFORCE_API_VERSION
            SALESFORCE_CLIENT_ID (para OAuth)
            SALESFORCE_CLIENT_SECRET (para OAuth)

        Returns:
            SalesforceConnector configurado
        """
        config = SalesforceConfig.from_env()
        return cls(config)

    @classmethod
    def from_credentials(
        cls,
        username: str,
        password: str,
        security_token: str,
        domain: str = "login",
        api_version: str = "59.0"
    ) -> "SalesforceConnector":
        """
        Cria conector com credenciais diretas

        Args:
            username: Nome de usuario
            password: Senha
            security_token: Token de seguranca
            domain: "login" (producao) ou "test" (sandbox)
            api_version: Versao da API

        Returns:
            SalesforceConnector configurado
        """
        config = SalesforceConfig(
            username=username,
            password=password,
            security_token=security_token,
            domain=domain,
            api_version=api_version
        )
        return cls(config)

    @classmethod
    def from_oauth(
        cls,
        client_id: str,
        client_secret: str,
        redirect_uri: str,
        is_sandbox: bool = False
    ) -> "SalesforceConnector":
        """
        Cria conector com OAuth

        Args:
            client_id: Consumer Key
            client_secret: Consumer Secret
            redirect_uri: URL de callback
            is_sandbox: Se e sandbox

        Returns:
            SalesforceConnector configurado
        """
        oauth = SalesforceOAuthConfig(
            client_id=client_id,
            client_secret=client_secret,
            redirect_uri=redirect_uri
        )
        if is_sandbox:
            oauth.set_sandbox()

        config = SalesforceConfig(
            domain="test" if is_sandbox else "login",
            oauth_config=oauth
        )
        return cls(config)

    # ==================== CONEXAO ====================

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._client is not None and self._client.is_connected

    @property
    def client(self) -> SalesforceClient:
        """Cliente REST API"""
        if self._client is None:
            self._client = SalesforceClient(self.config)
        return self._client

    async def connect(self) -> bool:
        """
        Estabelece conexao com Salesforce

        Returns:
            True se conectou com sucesso
        """
        try:
            await self.client.connect()

            self._connection_status = ConnectionStatus(
                connected=True,
                instance_url=self.config.instance_url,
                username=self.config.username,
                organization_id=self.config.organization_id,
                api_version=self.config.api_version,
                is_sandbox=self.config.is_sandbox,
                connected_at=datetime.now()
            )

            logger.info(f"Conectado ao Salesforce: {self.config.instance_url}")
            return True

        except Exception as e:
            logger.error(f"Falha na conexao: {e}")
            self._connection_status.connected = False
            raise

    async def disconnect(self):
        """Encerra conexao"""
        if self._client:
            await self._client.close()
            self._connection_status.connected = False
            logger.info("Desconectado do Salesforce")

    async def test_connection(self) -> bool:
        """Testa a conexao"""
        return await self.client.test_connection()

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da conexao"""
        usage = self.client.get_api_usage() if self.is_connected else {}

        return {
            "connected": self._connection_status.connected,
            "instance_url": self._connection_status.instance_url,
            "username": self._connection_status.username,
            "organization_id": self._connection_status.organization_id,
            "api_version": self._connection_status.api_version,
            "is_sandbox": self._connection_status.is_sandbox,
            "connected_at": self._connection_status.connected_at.isoformat() if self._connection_status.connected_at else None,
            "api_usage": usage
        }

    def get_oauth_url(self, state: Optional[str] = None) -> Optional[str]:
        """Obtem URL de autorizacao OAuth"""
        if self.config.oauth_config:
            return self.config.oauth_config.get_authorization_url(state)
        return None

    async def exchange_oauth_code(self, code: str) -> Dict[str, Any]:
        """Troca codigo OAuth por tokens"""
        return await self.client.exchange_oauth_code(code)

    # ==================== CLIENTES ====================

    @property
    def metadata(self) -> MetadataClient:
        """Cliente Metadata API"""
        if self._metadata is None:
            self._metadata = MetadataClient(self.client)
        return self._metadata

    @property
    def tooling(self) -> ToolingClient:
        """Cliente Tooling API"""
        if self._tooling is None:
            self._tooling = ToolingClient(self.client)
        return self._tooling

    @property
    def bulk(self) -> BulkClient:
        """Cliente Bulk API"""
        if self._bulk is None:
            self._bulk = BulkClient(self.client)
        return self._bulk

    # ==================== ANALYZERS ====================

    @property
    def object_analyzer(self) -> ObjectAnalyzer:
        """Analisador de objetos"""
        if self._object_analyzer is None:
            self._object_analyzer = ObjectAnalyzer(self.client)
        return self._object_analyzer

    @property
    def apex_analyzer(self) -> ApexAnalyzer:
        """Analisador de Apex"""
        if self._apex_analyzer is None:
            self._apex_analyzer = ApexAnalyzer(self.client)
        return self._apex_analyzer

    @property
    def flow_analyzer(self) -> FlowAnalyzer:
        """Analisador de Flows"""
        if self._flow_analyzer is None:
            self._flow_analyzer = FlowAnalyzer(self.client)
        return self._flow_analyzer

    @property
    def lwc_analyzer(self) -> LWCAnalyzer:
        """Analisador de LWC"""
        if self._lwc_analyzer is None:
            self._lwc_analyzer = LWCAnalyzer(self.client)
        return self._lwc_analyzer

    # ==================== GENERATORS ====================

    @property
    def apex_generator(self) -> ApexGenerator:
        """Gerador de codigo Apex"""
        if self._apex_generator is None:
            self._apex_generator = ApexGenerator(self.config.api_version)
        return self._apex_generator

    @property
    def lwc_generator(self) -> LWCGenerator:
        """Gerador de LWC"""
        if self._lwc_generator is None:
            self._lwc_generator = LWCGenerator(self.config.api_version)
        return self._lwc_generator

    @property
    def flow_generator(self) -> FlowGenerator:
        """Gerador de Flows"""
        if self._flow_generator is None:
            self._flow_generator = FlowGenerator(self.config.api_version)
        return self._flow_generator

    @property
    def object_generator(self) -> ObjectGenerator:
        """Gerador de objetos"""
        if self._object_generator is None:
            self._object_generator = ObjectGenerator(self.config.api_version)
        return self._object_generator

    # ==================== DEPLOYERS ====================

    @property
    def metadata_deployer(self) -> MetadataDeployer:
        """Deployer via Metadata API"""
        if self._metadata_deployer is None:
            self._metadata_deployer = MetadataDeployer(self.client, self.config.api_version)
        return self._metadata_deployer

    @property
    def sfdx_deployer(self) -> SFDXDeployer:
        """Deployer via SFDX CLI"""
        if self._sfdx_deployer is None:
            self._sfdx_deployer = SFDXDeployer()
        return self._sfdx_deployer

    # ==================== SKILLS ====================

    def get_read_skill(self) -> SalesforceReadSkill:
        """Obtem skill de leitura"""
        if self._read_skill is None:
            self._read_skill = SalesforceReadSkill(self.client)
        return self._read_skill

    def get_apex_skill(self) -> SalesforceApexSkill:
        """Obtem skill de Apex"""
        if self._apex_skill is None:
            self._apex_skill = SalesforceApexSkill(self.client)
        return self._apex_skill

    def get_deploy_skill(self) -> SalesforceDeploySkill:
        """Obtem skill de deploy"""
        if self._deploy_skill is None:
            self._deploy_skill = SalesforceDeploySkill(self.client)
        return self._deploy_skill

    # ==================== METODOS CONVENIENTES ====================

    async def query(self, soql: str) -> List[Dict[str, Any]]:
        """Executa query SOQL"""
        result = await self.client.query(soql)
        return result.records

    async def search(self, sosl: str) -> List[Dict[str, Any]]:
        """Executa busca SOSL"""
        return await self.client.search(sosl)

    async def create(self, sobject: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Cria registro"""
        return await self.client.create(sobject, data)

    async def update(self, sobject: str, record_id: str, data: Dict[str, Any]) -> bool:
        """Atualiza registro"""
        return await self.client.update(sobject, record_id, data)

    async def delete(self, sobject: str, record_id: str) -> bool:
        """Deleta registro"""
        return await self.client.delete(sobject, record_id)

    async def get(
        self,
        sobject: str,
        record_id: str,
        fields: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Obtem registro por ID"""
        return await self.client.get(sobject, record_id, fields)

    async def describe(self, sobject: str):
        """Descreve objeto"""
        return await self.client.describe(sobject)

    async def analyze_object(self, sobject: str):
        """Analisa objeto"""
        return await self.object_analyzer.analyze(sobject)

    async def analyze_apex_class(self, class_name: str):
        """Analisa classe Apex"""
        return await self.apex_analyzer.analyze_class(class_name)

    async def analyze_flow(self, flow_name: str):
        """Analisa flow"""
        return await self.flow_analyzer.analyze(flow_name)

    def generate_apex_class(
        self,
        class_name: str,
        sobject: str,
        class_type: str = "service"
    ) -> str:
        """
        Gera classe Apex

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            class_type: Tipo (service, selector, domain, batch)

        Returns:
            Codigo Apex
        """
        if class_type == "service":
            return self.apex_generator.generate_service_class(class_name, sobject)
        elif class_type == "selector":
            return self.apex_generator.generate_selector_class(class_name, sobject)
        elif class_type == "domain":
            return self.apex_generator.generate_domain_class(class_name, sobject)
        elif class_type == "batch":
            return self.apex_generator.generate_batch_class(class_name, sobject)
        else:
            raise ValueError(f"Tipo de classe invalido: {class_type}")

    def generate_lwc(
        self,
        name: str,
        label: str,
        component_type: str = "basic"
    ) -> Dict[str, str]:
        """
        Gera componente LWC

        Args:
            name: Nome do componente
            label: Label
            component_type: Tipo (basic, form, list)

        Returns:
            Dict com arquivos
        """
        if component_type == "basic":
            return self.lwc_generator.generate_component(name, label)
        elif component_type == "modal":
            return self.lwc_generator.generate_modal_component(name, label)
        else:
            return self.lwc_generator.generate_component(name, label)

    async def deploy_apex(
        self,
        class_name: str,
        body: str,
        run_tests: bool = False
    ):
        """Faz deploy de classe Apex"""
        return await self.metadata_deployer.deploy_apex_class(class_name, body)

    async def execute_anonymous(self, apex_code: str):
        """Executa codigo Apex anonimo"""
        return await self.tooling.execute_anonymous(apex_code)

    async def run_tests(self, class_names: List[str]):
        """Executa testes Apex"""
        return await self.tooling.run_tests_and_wait(class_names)

    # ==================== BULK OPERATIONS ====================

    async def bulk_insert(
        self,
        sobject: str,
        records: List[Dict[str, Any]]
    ):
        """Insert em massa"""
        return await self.bulk.insert(sobject, records)

    async def bulk_update(
        self,
        sobject: str,
        records: List[Dict[str, Any]]
    ):
        """Update em massa"""
        return await self.bulk.update(sobject, records)

    async def bulk_delete(
        self,
        sobject: str,
        record_ids: List[str]
    ):
        """Delete em massa"""
        return await self.bulk.delete(sobject, record_ids)

    async def bulk_query(self, soql: str) -> List[Dict[str, Any]]:
        """Query em massa"""
        return await self.bulk.query(soql)

    # ==================== CONTEXT MANAGER ====================

    async def __aenter__(self):
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.disconnect()
