# -*- coding: utf-8 -*-
"""
SAP S/4HANA Integration
=======================
Modulo principal de integracao com SAP S/4HANA.

Este modulo fornece uma interface unificada para interagir com SAP S/4HANA,
incluindo conexao OData, SAP Graph, analise de artefatos e geracao de codigo.

Autor: Fabrica de Agentes
Versao: 1.0.0

Exemplo de uso:
```python
from factory.integrations.sap_s4hana import SAPS4HanaIntegration

# Configurar a partir de variaveis de ambiente
integration = SAPS4HanaIntegration.from_environment()

# Ou configurar manualmente
integration = SAPS4HanaIntegration(
    system_url="https://my-s4.s4hana.ondemand.com",
    oauth_config={
        "token_url": "https://my-s4.s4hana.ondemand.com/sap/bc/sec/oauth2/token",
        "client_id": "MY_CLIENT_ID",
        "client_secret": "MY_CLIENT_SECRET"
    }
)

# Conectar
await integration.connect()

# Ler dados
bp_list = await integration.read_business_partners(country="BR", limit=100)

# Gerar CDS View
cds_code = integration.generate_cds_view(
    name="Z_MY_VIEW",
    base_view="I_SalesOrder",
    fields=["SalesOrder", "Customer", "TotalAmount"]
)

# Gerar app Fiori
app_files = integration.generate_fiori_app(
    app_id="com.company.myapp",
    title="Minha Aplicacao",
    entity_set="SalesOrder"
)
```
"""

import os
from dataclasses import dataclass
from typing import Any, Dict, List, Optional
import logging

# Importar componentes
from .sap_s4.sap_config import (
    SAPS4Config,
    SAPS4Environment,
    load_config_from_env
)
from .sap_s4.sap_auth import (
    SAPAuthenticator,
    SAPOAuthConfig,
    SAPBasicAuthConfig,
    SAPAuthError,
    create_authenticator_from_config
)
from .sap_s4.odata_v4_client import (
    ODataV4Client,
    ODataConfig,
    ODataError,
    ODataQueryBuilder
)
from .sap_s4.graph_client import (
    SAPGraphClient,
    GraphConfig,
    GraphError
)

# Skills
from .sap_s4.skills.s4_read_skill import S4ReadSkill, S4EntityType
from .sap_s4.skills.s4_cds_skill import S4CDSSkill
from .sap_s4.skills.s4_fiori_skill import S4FioriSkill

# Geradores
from .sap_s4.generators.cds_generator import CDSGenerator, CDSViewSpec
from .sap_s4.generators.fiori_generator import FioriGenerator, FioriAppSpec
from .sap_s4.generators.rap_generator import RAPGenerator, RAPServiceSpec

logger = logging.getLogger(__name__)


@dataclass
class ConnectionStatus:
    """Status de conexao com SAP"""
    connected: bool
    system_url: str
    environment: str
    auth_method: str
    message: str = ""
    odata_available: bool = False
    graph_available: bool = False


class SAPS4HanaIntegration:
    """
    Integracao completa com SAP S/4HANA

    Esta classe fornece uma interface unificada para:
    - Conexao e autenticacao com SAP S/4HANA
    - Leitura de dados mestres e transacionais
    - Analise de artefatos SAP (CDS, Fiori, RAP)
    - Geracao de codigo (CDS Views, Apps Fiori, Servicos RAP)

    Atributos:
        config: Configuracao do sistema SAP
        authenticator: Autenticador (OAuth ou Basic)
        odata_client: Cliente OData V4
        graph_client: Cliente SAP Graph
        read_skill: Skill de leitura de dados
        cds_skill: Skill de CDS Views
        fiori_skill: Skill de aplicacoes Fiori
    """

    def __init__(
        self,
        system_url: str = "",
        oauth_config: Optional[Dict] = None,
        basic_auth: Optional[Dict] = None,
        environment: str = "cloud",
        config: Optional[SAPS4Config] = None
    ):
        """
        Inicializa integracao SAP S/4HANA

        Args:
            system_url: URL base do sistema SAP
            oauth_config: Configuracao OAuth {"token_url", "client_id", "client_secret"}
            basic_auth: Autenticacao basica {"username", "password", "client"}
            environment: Ambiente (cloud, private, on_premise)
            config: Objeto SAPS4Config completo (alternativa aos parametros acima)
        """
        # Usar config fornecida ou criar nova
        if config:
            self.config = config
        else:
            env_map = {
                "cloud": SAPS4Environment.CLOUD,
                "private": SAPS4Environment.CLOUD_PRIVATE,
                "on_premise": SAPS4Environment.ON_PREMISE
            }

            self.config = SAPS4Config(
                system_url=system_url,
                environment=env_map.get(environment, SAPS4Environment.CLOUD)
            )

            if oauth_config:
                self.config.oauth_token_url = oauth_config.get("token_url", "")
                self.config.oauth_client_id = oauth_config.get("client_id", "")
                self.config.oauth_client_secret = oauth_config.get("client_secret", "")
                self.config.oauth_scope = oauth_config.get("scope", "")

            if basic_auth:
                self.config.username = basic_auth.get("username", "")
                self.config.password = basic_auth.get("password", "")
                self.config.client = basic_auth.get("client", "100")

        # Inicializar componentes
        self.authenticator: Optional[SAPAuthenticator] = None
        self.odata_client: Optional[ODataV4Client] = None
        self.graph_client: Optional[SAPGraphClient] = None

        # Skills
        self.read_skill: Optional[S4ReadSkill] = None
        self.cds_skill: Optional[S4CDSSkill] = None
        self.fiori_skill: Optional[S4FioriSkill] = None

        # Geradores (sempre disponiveis)
        self.cds_generator = CDSGenerator()
        self.fiori_generator = FioriGenerator()
        self.rap_generator = RAPGenerator()

        # Status
        self._connected = False

    @classmethod
    def from_environment(cls) -> "SAPS4HanaIntegration":
        """
        Cria instancia a partir de variaveis de ambiente

        Variaveis suportadas:
        - SAP_S4_SYSTEM_URL
        - SAP_S4_OAUTH_TOKEN_URL
        - SAP_S4_OAUTH_CLIENT_ID
        - SAP_S4_OAUTH_CLIENT_SECRET
        - SAP_S4_USERNAME
        - SAP_S4_PASSWORD
        - SAP_S4_CLIENT
        - SAP_S4_ENVIRONMENT

        Returns:
            SAPS4HanaIntegration configurada
        """
        config = load_config_from_env()
        return cls(config=config)

    async def connect(self) -> ConnectionStatus:
        """
        Conecta ao SAP S/4HANA

        Inicializa autenticacao e clientes OData/Graph.

        Returns:
            ConnectionStatus com resultado da conexao
        """
        # Validar configuracao
        errors = self.config.validate()
        if errors:
            return ConnectionStatus(
                connected=False,
                system_url=self.config.system_url,
                environment=self.config.environment.value,
                auth_method="none",
                message=f"Configuracao invalida: {', '.join(errors)}"
            )

        try:
            # Criar autenticador
            self.authenticator = create_authenticator_from_config(self.config)

            # Validar credenciais
            valid, msg = await self.authenticator.validate_credentials()
            if not valid:
                return ConnectionStatus(
                    connected=False,
                    system_url=self.config.system_url,
                    environment=self.config.environment.value,
                    auth_method=self.authenticator.auth_method,
                    message=msg
                )

            # Criar cliente OData
            odata_config = ODataConfig(
                base_url=self.config.system_url,
                timeout=self.config.timeout,
                verify_ssl=self.config.verify_ssl,
                language=self.config.language
            )
            self.odata_client = ODataV4Client(odata_config, self.authenticator)

            # Criar cliente Graph (se disponivel)
            graph_url = self.config.get_full_url("graph")
            if graph_url:
                graph_config = GraphConfig(
                    graph_url=graph_url,
                    timeout=self.config.timeout,
                    verify_ssl=self.config.verify_ssl,
                    language=self.config.language
                )
                self.graph_client = SAPGraphClient(graph_config, self.authenticator)

            # Inicializar skills
            self.read_skill = S4ReadSkill(self.odata_client, self.graph_client)
            self.cds_skill = S4CDSSkill(self.odata_client)
            self.fiori_skill = S4FioriSkill(self.odata_client)

            self._connected = True

            return ConnectionStatus(
                connected=True,
                system_url=self.config.system_url,
                environment=self.config.environment.value,
                auth_method=self.authenticator.auth_method,
                message="Conectado com sucesso",
                odata_available=True,
                graph_available=self.graph_client is not None
            )

        except Exception as e:
            logger.error(f"Erro ao conectar: {e}")
            return ConnectionStatus(
                connected=False,
                system_url=self.config.system_url,
                environment=self.config.environment.value,
                auth_method="unknown",
                message=f"Erro ao conectar: {str(e)}"
            )

    async def disconnect(self):
        """Desconecta do SAP S/4HANA"""
        if self.authenticator:
            self.authenticator.clear_token_cache()

        self.odata_client = None
        self.graph_client = None
        self.read_skill = None
        self.cds_skill = None
        self.fiori_skill = None
        self._connected = False

        logger.info("Desconectado do SAP S/4HANA")

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._connected

    def get_status(self) -> Dict[str, Any]:
        """
        Retorna status da integracao

        Returns:
            Dict com status detalhado
        """
        return {
            "connected": self._connected,
            "config": self.config.to_dict(),
            "auth_method": self.authenticator.auth_method if self.authenticator else "none",
            "token_info": self.authenticator.get_token_info() if self.authenticator else None,
            "odata_available": self.odata_client is not None,
            "graph_available": self.graph_client is not None,
            "available_agents": self.config.available_agents
        }

    # =====================================
    # Metodos de Leitura de Dados
    # =====================================

    async def read_business_partners(
        self,
        country: Optional[str] = None,
        category: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ) -> Dict[str, Any]:
        """
        Le Business Partners do S/4HANA

        Args:
            country: Codigo do pais (BR, US, etc)
            category: Categoria (1=Org, 2=Pessoa)
            search: Termo de busca
            limit: Limite de registros

        Returns:
            Dict com dados e metadados
        """
        if not self.read_skill:
            return {"success": False, "message": "Nao conectado"}

        result = await self.read_skill.read_business_partners(
            country=country,
            category=category,
            search=search,
            limit=limit
        )

        return {
            "success": result.success,
            "data": result.data,
            "count": result.count,
            "message": result.message
        }

    async def read_sales_orders(
        self,
        customer: Optional[str] = None,
        date_from: Optional[str] = None,
        date_to: Optional[str] = None,
        limit: int = 100
    ) -> Dict[str, Any]:
        """
        Le Sales Orders do S/4HANA

        Args:
            customer: Codigo do cliente
            date_from: Data inicial (YYYY-MM-DD)
            date_to: Data final
            limit: Limite

        Returns:
            Dict com ordens de venda
        """
        if not self.read_skill:
            return {"success": False, "message": "Nao conectado"}

        result = await self.read_skill.read_sales_orders(
            customer=customer,
            date_from=date_from,
            date_to=date_to,
            limit=limit
        )

        return {
            "success": result.success,
            "data": result.data,
            "count": result.count,
            "message": result.message
        }

    async def read_purchase_orders(
        self,
        supplier: Optional[str] = None,
        date_from: Optional[str] = None,
        date_to: Optional[str] = None,
        limit: int = 100
    ) -> Dict[str, Any]:
        """
        Le Purchase Orders do S/4HANA

        Args:
            supplier: Codigo do fornecedor
            date_from: Data inicial
            date_to: Data final
            limit: Limite

        Returns:
            Dict com pedidos de compra
        """
        if not self.read_skill:
            return {"success": False, "message": "Nao conectado"}

        result = await self.read_skill.read_purchase_orders(
            supplier=supplier,
            date_from=date_from,
            date_to=date_to,
            limit=limit
        )

        return {
            "success": result.success,
            "data": result.data,
            "count": result.count,
            "message": result.message
        }

    async def search(
        self,
        term: str,
        entity_types: Optional[List[str]] = None,
        limit_per_entity: int = 10
    ) -> Dict[str, Any]:
        """
        Pesquisa em multiplas entidades

        Args:
            term: Termo de busca
            entity_types: Tipos de entidade
            limit_per_entity: Limite por tipo

        Returns:
            Dict com resultados por entidade
        """
        if not self.read_skill:
            return {"success": False, "message": "Nao conectado"}

        types = None
        if entity_types:
            types = [S4EntityType(t) for t in entity_types]

        results = await self.read_skill.search(
            term=term,
            entity_types=types,
            limit_per_entity=limit_per_entity
        )

        return {
            "success": True,
            "results": {
                k: {"success": v.success, "data": v.data, "count": v.count}
                for k, v in results.items()
            }
        }

    # =====================================
    # Metodos de Geracao de Codigo
    # =====================================

    def generate_cds_view(
        self,
        name: str,
        base_view: str,
        fields: List[str],
        description: str = ""
    ) -> Dict[str, Any]:
        """
        Gera CDS View de consumo

        Args:
            name: Nome da view (ex: Z_MY_VIEW)
            base_view: View base (ex: I_SalesOrder)
            fields: Lista de campos
            description: Descricao

        Returns:
            Dict com codigo CDS gerado
        """
        if self.cds_skill:
            result = self.cds_skill.generate_consumption_view(
                name=name,
                base_view=base_view,
                fields=fields,
                description=description
            )
            return {
                "success": result.success,
                "code": result.code,
                "data": result.data,
                "message": result.message,
                "warnings": result.warnings
            }
        else:
            # Usar gerador diretamente
            code = self.cds_generator.generate_consumption_view(
                name=name,
                base_view=base_view,
                fields=fields,
                description=description
            )
            return {
                "success": True,
                "code": code,
                "message": "CDS View gerada (modo offline)"
            }

    def generate_analytical_view(
        self,
        name: str,
        base_view: str,
        dimensions: List[str],
        measures: List[Dict],
        description: str = ""
    ) -> Dict[str, Any]:
        """
        Gera CDS View analitica

        Args:
            name: Nome da view
            base_view: View base
            dimensions: Campos de dimensao
            measures: Medidas [{"name": "Amount", "aggregation": "SUM"}]
            description: Descricao

        Returns:
            Dict com codigo CDS
        """
        code = self.cds_generator.generate_analytical_view(
            name=name,
            base_view=base_view,
            dimensions=dimensions,
            measures=measures,
            description=description
        )
        return {
            "success": True,
            "code": code,
            "message": "CDS View analitica gerada"
        }

    def generate_fiori_app(
        self,
        app_id: str,
        title: str,
        entity_set: str,
        odata_service: str = "",
        list_fields: Optional[List[Dict]] = None,
        floorplan: str = "list_report"
    ) -> Dict[str, Any]:
        """
        Gera aplicacao Fiori Elements

        Args:
            app_id: ID da aplicacao
            title: Titulo
            entity_set: Entity set principal
            odata_service: URL do servico OData
            list_fields: Campos da lista [{"name": "...", "label": "..."}]
            floorplan: Tipo de floorplan

        Returns:
            Dict com arquivos gerados
        """
        if self.fiori_skill:
            if floorplan == "list_report":
                result = self.fiori_skill.generate_list_report(
                    app_id=app_id,
                    title=title,
                    odata_service=odata_service or "/sap/opu/odata4/sap/api/",
                    entity_set=entity_set,
                    list_fields=list_fields or []
                )
            elif floorplan == "worklist":
                result = self.fiori_skill.generate_worklist(
                    app_id=app_id,
                    title=title,
                    odata_service=odata_service or "/sap/opu/odata4/sap/api/",
                    entity_set=entity_set,
                    list_fields=list_fields or []
                )
            else:
                return {
                    "success": False,
                    "message": f"Floorplan '{floorplan}' nao suportado"
                }

            return {
                "success": result.success,
                "files": result.files,
                "data": result.data,
                "message": result.message
            }
        else:
            return {
                "success": False,
                "message": "Skill Fiori nao inicializada"
            }

    def generate_rap_service(
        self,
        name: str,
        entities: List[Dict],
        description: str = ""
    ) -> Dict[str, Any]:
        """
        Gera servico RAP

        Args:
            name: Nome do servico
            entities: Entidades [{"name": "...", "table": "...", "fields": [...]}]
            description: Descricao

        Returns:
            Dict com arquivos RAP gerados
        """
        from .sap_s4.generators.rap_generator import (
            RAPServiceSpec,
            RAPEntitySpec,
            RAPFieldSpec,
            RAPScenarioType
        )

        entity_specs = []
        for i, e in enumerate(entities):
            fields = [
                RAPFieldSpec(
                    name=f.get("name"),
                    abap_type=f.get("type", "abap.char(50)"),
                    is_key=f.get("is_key", False)
                )
                for f in e.get("fields", [])
            ]

            entity_specs.append(RAPEntitySpec(
                name=e["name"],
                table_name=e.get("table", f"z{e['name'].lower()}"),
                is_root=(i == 0),
                fields=fields
            ))

        spec = RAPServiceSpec(
            name=name,
            description=description,
            entities=entity_specs
        )

        try:
            files = self.rap_generator.generate(spec)
            return {
                "success": True,
                "files": files,
                "message": "Servico RAP gerado"
            }
        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao gerar RAP: {str(e)}"
            }

    # =====================================
    # Metodos de Analise
    # =====================================

    async def analyze_cds_view(self, view_name: str) -> Dict[str, Any]:
        """
        Analisa CDS View do sistema

        Args:
            view_name: Nome da view

        Returns:
            Dict com informacoes da view
        """
        if not self.cds_skill:
            return {"success": False, "message": "Skill CDS nao inicializada"}

        result = await self.cds_skill.analyze_view(view_name)
        return {
            "success": result.success,
            "data": result.data,
            "message": result.message
        }

    def parse_cds_source(self, source: str) -> Dict[str, Any]:
        """
        Faz parse de codigo CDS

        Args:
            source: Codigo fonte CDS

        Returns:
            Dict com informacoes extraidas
        """
        if self.cds_skill:
            result = self.cds_skill.parse_cds_source(source)
            return {
                "success": result.success,
                "data": result.data,
                "message": result.message,
                "warnings": result.warnings
            }

        from .sap_s4.analyzers.cds_analyzer import CDSAnalyzer
        analyzer = CDSAnalyzer()
        view_info = analyzer.parse_cds_source(source)
        return {
            "success": True,
            "data": view_info.to_dict(),
            "message": "Parse realizado (modo offline)"
        }

    def analyze_manifest(self, manifest: Dict) -> Dict[str, Any]:
        """
        Analisa manifest.json Fiori

        Args:
            manifest: Conteudo do manifest

        Returns:
            Dict com informacoes do app
        """
        if self.fiori_skill:
            result = self.fiori_skill.analyze_manifest(manifest)
            return {
                "success": result.success,
                "data": result.data,
                "message": result.message
            }

        from .sap_s4.analyzers.fiori_analyzer import FioriAnalyzer
        analyzer = FioriAnalyzer()
        app_info = analyzer.parse_manifest(manifest)
        return {
            "success": True,
            "data": app_info.to_dict(),
            "message": "Manifest analisado (modo offline)"
        }


# Funcoes auxiliares para uso simplificado
async def connect_sap_s4hana() -> SAPS4HanaIntegration:
    """
    Conecta ao SAP S/4HANA usando variaveis de ambiente

    Returns:
        SAPS4HanaIntegration conectada
    """
    integration = SAPS4HanaIntegration.from_environment()
    await integration.connect()
    return integration


def create_sap_s4hana_integration(
    system_url: str,
    oauth_client_id: str,
    oauth_client_secret: str,
    oauth_token_url: Optional[str] = None
) -> SAPS4HanaIntegration:
    """
    Cria integracao SAP S/4HANA com OAuth

    Args:
        system_url: URL do sistema
        oauth_client_id: Client ID
        oauth_client_secret: Client Secret
        oauth_token_url: URL do token (opcional, usa padrao)

    Returns:
        SAPS4HanaIntegration configurada (nao conectada)
    """
    if not oauth_token_url:
        oauth_token_url = f"{system_url}/sap/bc/sec/oauth2/token"

    return SAPS4HanaIntegration(
        system_url=system_url,
        oauth_config={
            "token_url": oauth_token_url,
            "client_id": oauth_client_id,
            "client_secret": oauth_client_secret
        }
    )
