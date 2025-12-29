# -*- coding: utf-8 -*-
"""
SAP S/4HANA Configuration Module
================================
Configuracoes para conexao com SAP S/4HANA Cloud e On-Premise.

Este modulo define as configuracoes necessarias para conectar-se ao SAP S/4HANA,
incluindo URLs de API, credenciais OAuth2 e endpoints de servicos.

Autor: Fabrica de Agentes
"""

import os
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class SAPS4Environment(str, Enum):
    """Ambiente SAP S/4HANA"""
    CLOUD = "cloud"              # SAP S/4HANA Cloud
    CLOUD_PRIVATE = "private"    # SAP S/4HANA Cloud Private Edition
    ON_PREMISE = "on_premise"    # SAP S/4HANA On-Premise


@dataclass
class SAPS4Config:
    """
    Configuracao completa para SAP S/4HANA

    Attributes:
        system_url: URL base do sistema SAP (ex: https://my-s4.s4hana.ondemand.com)
        environment: Tipo de ambiente (cloud, private, on_premise)
        client: Mandante SAP (ex: "100")
        oauth_token_url: URL para obter token OAuth2
        oauth_client_id: Client ID para OAuth2
        oauth_client_secret: Client Secret para OAuth2
        oauth_scope: Escopo OAuth2 (opcional)
        username: Usuario para autenticacao basica (alternativa ao OAuth)
        password: Senha para autenticacao basica
        api_version: Versao da API OData (v2 ou v4)
        language: Idioma preferencial (ex: "PT", "EN")
        timeout: Timeout para requisicoes em segundos
        verify_ssl: Verificar certificado SSL
        proxy: Configuracao de proxy (opcional)
        communication_arrangement: Nome do Communication Arrangement
        communication_system: Nome do Communication System
    """
    # Conexao basica
    system_url: str = ""
    environment: SAPS4Environment = SAPS4Environment.CLOUD
    client: str = "100"

    # OAuth 2.0 (recomendado para Cloud)
    oauth_token_url: str = ""
    oauth_client_id: str = ""
    oauth_client_secret: str = ""
    oauth_scope: str = ""

    # Autenticacao basica (alternativa)
    username: str = ""
    password: str = ""

    # Configuracoes de API
    api_version: str = "v4"
    language: str = "PT"
    timeout: int = 60
    verify_ssl: bool = True
    proxy: Optional[Dict[str, str]] = None

    # Communication Arrangement
    communication_arrangement: str = ""
    communication_system: str = ""

    # APIs disponiveis (caminhos relativos)
    api_endpoints: Dict[str, str] = field(default_factory=lambda: {
        # Business Partner
        "business_partner": "/sap/opu/odata4/sap/api_business_partner/srvd_a2x/sap/a_businesspartner/0001/",

        # Sales
        "sales_order": "/sap/opu/odata4/sap/api_sales_order_srv/srvd_a2x/sap/salesorder/0001/",
        "sales_quotation": "/sap/opu/odata4/sap/api_sales_quotation/srvd_a2x/sap/salesquotation/0001/",

        # Purchasing
        "purchase_order": "/sap/opu/odata4/sap/api_purchaseorder_2/srvd_a2x/sap/purchaseorder/0001/",
        "purchase_requisition": "/sap/opu/odata4/sap/api_purchasereq_2/srvd_a2x/sap/purchaserequisition/0001/",

        # Materials
        "product": "/sap/opu/odata4/sap/api_product_2/srvd_a2x/sap/product/0002/",
        "material_document": "/sap/opu/odata4/sap/api_material_document_srv/srvd_a2x/sap/materialdocument/0001/",

        # Finance
        "journal_entry": "/sap/opu/odata4/sap/api_journalentryitembasic/srvd_a2x/sap/journalentryitembasic/0001/",
        "gl_account": "/sap/opu/odata4/sap/api_glaccount_2/srvd_a2x/sap/glaccount/0001/",
        "cost_center": "/sap/opu/odata4/sap/api_costcenter_2/srvd_a2x/sap/costcenter/0001/",

        # Asset Management
        "fixed_asset": "/sap/opu/odata4/sap/api_fixedasset/srvd_a2x/sap/fixedasset/0001/",

        # Production
        "production_order": "/sap/opu/odata4/sap/api_production_order_2/srvd_a2x/sap/productionorder/0001/",

        # Project System
        "project": "/sap/opu/odata4/sap/api_project_2/srvd_a2x/sap/project/0001/",

        # HR / HCM
        "employee": "/sap/opu/odata4/sap/api_employee/srvd_a2x/sap/employee/0001/",

        # CDS Views (via SQL Services)
        "cds_catalog": "/sap/opu/odata4/iwbep/v4_gw_tech_introspection/default/iwbep/v4_gw_catalog/0001/",

        # SAP Graph
        "graph": "/sap/graph/api/v1/"
    })

    # Agentes especializados disponiveis
    available_agents: List[str] = field(default_factory=lambda: [
        "AGT-S4-FINANCE",
        "AGT-S4-PROCUREMENT",
        "AGT-S4-SALES",
        "AGT-S4-MANUFACTURING",
        "AGT-S4-ASSET",
        "AGT-S4-PROJECT"
    ])

    def get_full_url(self, endpoint_key: str) -> str:
        """
        Retorna URL completa para um endpoint

        Args:
            endpoint_key: Chave do endpoint (ex: "business_partner")

        Returns:
            URL completa do endpoint
        """
        endpoint = self.api_endpoints.get(endpoint_key, "")
        return f"{self.system_url.rstrip('/')}{endpoint}"

    def is_oauth_configured(self) -> bool:
        """Verifica se OAuth esta configurado"""
        return bool(
            self.oauth_token_url and
            self.oauth_client_id and
            self.oauth_client_secret
        )

    def is_basic_auth_configured(self) -> bool:
        """Verifica se autenticacao basica esta configurada"""
        return bool(self.username and self.password)

    def validate(self) -> List[str]:
        """
        Valida a configuracao e retorna lista de erros

        Returns:
            Lista de mensagens de erro (vazia se valido)
        """
        errors = []

        if not self.system_url:
            errors.append("URL do sistema SAP nao configurada (system_url)")

        if not self.is_oauth_configured() and not self.is_basic_auth_configured():
            errors.append(
                "Nenhum metodo de autenticacao configurado. "
                "Configure OAuth2 ou autenticacao basica."
            )

        if self.environment == SAPS4Environment.CLOUD and not self.is_oauth_configured():
            errors.append(
                "Para SAP S/4HANA Cloud, OAuth2 e recomendado. "
                "Configure oauth_token_url, oauth_client_id e oauth_client_secret."
            )

        return errors

    def to_dict(self) -> Dict:
        """Converte configuracao para dicionario (sem dados sensiveis)"""
        return {
            "system_url": self.system_url,
            "environment": self.environment.value,
            "client": self.client,
            "api_version": self.api_version,
            "language": self.language,
            "timeout": self.timeout,
            "oauth_configured": self.is_oauth_configured(),
            "basic_auth_configured": self.is_basic_auth_configured(),
            "communication_arrangement": self.communication_arrangement,
            "available_agents": self.available_agents
        }


def get_default_config() -> SAPS4Config:
    """
    Retorna configuracao padrao

    Returns:
        SAPS4Config com valores padrao
    """
    return SAPS4Config()


def load_config_from_env() -> SAPS4Config:
    """
    Carrega configuracao a partir de variaveis de ambiente

    Variaveis de ambiente suportadas:
    - SAP_S4_SYSTEM_URL: URL base do sistema
    - SAP_S4_ENVIRONMENT: cloud, private ou on_premise
    - SAP_S4_CLIENT: Mandante SAP
    - SAP_S4_OAUTH_TOKEN_URL: URL do token OAuth
    - SAP_S4_OAUTH_CLIENT_ID: Client ID OAuth
    - SAP_S4_OAUTH_CLIENT_SECRET: Client Secret OAuth
    - SAP_S4_OAUTH_SCOPE: Escopo OAuth
    - SAP_S4_USERNAME: Usuario para autenticacao basica
    - SAP_S4_PASSWORD: Senha para autenticacao basica
    - SAP_S4_LANGUAGE: Idioma (default: PT)
    - SAP_S4_TIMEOUT: Timeout em segundos
    - SAP_S4_VERIFY_SSL: Verificar SSL (true/false)
    - SAP_S4_PROXY_HTTP: Proxy HTTP
    - SAP_S4_PROXY_HTTPS: Proxy HTTPS
    - SAP_S4_COMMUNICATION_ARRANGEMENT: Nome do Communication Arrangement
    - SAP_S4_COMMUNICATION_SYSTEM: Nome do Communication System

    Returns:
        SAPS4Config carregado das variaveis de ambiente
    """
    # Determinar ambiente
    env_str = os.getenv("SAP_S4_ENVIRONMENT", "cloud").lower()
    environment = {
        "cloud": SAPS4Environment.CLOUD,
        "private": SAPS4Environment.CLOUD_PRIVATE,
        "on_premise": SAPS4Environment.ON_PREMISE
    }.get(env_str, SAPS4Environment.CLOUD)

    # Configurar proxy se definido
    proxy = None
    proxy_http = os.getenv("SAP_S4_PROXY_HTTP")
    proxy_https = os.getenv("SAP_S4_PROXY_HTTPS")
    if proxy_http or proxy_https:
        proxy = {}
        if proxy_http:
            proxy["http"] = proxy_http
        if proxy_https:
            proxy["https"] = proxy_https

    config = SAPS4Config(
        system_url=os.getenv("SAP_S4_SYSTEM_URL", ""),
        environment=environment,
        client=os.getenv("SAP_S4_CLIENT", "100"),
        oauth_token_url=os.getenv("SAP_S4_OAUTH_TOKEN_URL", ""),
        oauth_client_id=os.getenv("SAP_S4_OAUTH_CLIENT_ID", ""),
        oauth_client_secret=os.getenv("SAP_S4_OAUTH_CLIENT_SECRET", ""),
        oauth_scope=os.getenv("SAP_S4_OAUTH_SCOPE", ""),
        username=os.getenv("SAP_S4_USERNAME", ""),
        password=os.getenv("SAP_S4_PASSWORD", ""),
        language=os.getenv("SAP_S4_LANGUAGE", "PT"),
        timeout=int(os.getenv("SAP_S4_TIMEOUT", "60")),
        verify_ssl=os.getenv("SAP_S4_VERIFY_SSL", "true").lower() == "true",
        proxy=proxy,
        communication_arrangement=os.getenv("SAP_S4_COMMUNICATION_ARRANGEMENT", ""),
        communication_system=os.getenv("SAP_S4_COMMUNICATION_SYSTEM", "")
    )

    # Validar e logar warnings
    errors = config.validate()
    for error in errors:
        logger.warning(f"SAP S/4HANA Config: {error}")

    return config


# Exemplos de configuracao para documentacao
EXAMPLE_CONFIGS = {
    "cloud": {
        "description": "SAP S/4HANA Cloud Public Edition",
        "config": {
            "system_url": "https://my-api.s4hana.ondemand.com",
            "environment": "cloud",
            "oauth_token_url": "https://my-api.s4hana.ondemand.com/sap/bc/sec/oauth2/token",
            "oauth_client_id": "COMM_USER_CLIENT_ID",
            "oauth_client_secret": "***",
            "communication_arrangement": "Z_MY_COMM_ARRANGEMENT"
        }
    },
    "cloud_private": {
        "description": "SAP S/4HANA Cloud Private Edition",
        "config": {
            "system_url": "https://my-private.s4hana.cloud",
            "environment": "private",
            "username": "COMM_USER",
            "password": "***",
            "client": "100"
        }
    },
    "on_premise": {
        "description": "SAP S/4HANA On-Premise",
        "config": {
            "system_url": "https://sap-server.mycompany.local:443",
            "environment": "on_premise",
            "username": "TECHNICAL_USER",
            "password": "***",
            "client": "100",
            "verify_ssl": False
        }
    }
}
