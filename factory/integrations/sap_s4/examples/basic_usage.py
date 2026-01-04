# -*- coding: utf-8 -*-
"""
SAP S/4HANA Integration - Exemplos de Uso
==========================================
Este arquivo contem exemplos praticos de uso da integracao SAP S/4HANA.

Autor: Plataforma E
"""

import asyncio
import os

# Importar integracao principal
from factory.integrations.sap_s4hana import (
    SAPS4HanaIntegration,
    create_sap_s4hana_integration
)

# Importar componentes individuais
from factory.integrations.sap_s4 import (
    ODataV4Client,
    ODataConfig,
    ODataQueryBuilder,
    SAPAuthenticator,
    SAPOAuthConfig,
    CDSGenerator,
    CDSViewSpec,
    CDSFieldSpec,
    FioriGenerator,
    FioriAppSpec,
    FioriFloorplanType,
    FioriFieldSpec
)


# =============================================================================
# Exemplo 1: Conexao usando variaveis de ambiente
# =============================================================================
async def exemplo_conexao_env():
    """
    Conecta ao SAP S/4HANA usando variaveis de ambiente.

    Variaveis necessarias:
    - SAP_S4_SYSTEM_URL
    - SAP_S4_OAUTH_TOKEN_URL
    - SAP_S4_OAUTH_CLIENT_ID
    - SAP_S4_OAUTH_CLIENT_SECRET
    """
    print("=== Exemplo 1: Conexao via Ambiente ===")

    # Criar integracao a partir das variaveis de ambiente
    integration = SAPS4HanaIntegration.from_environment()

    # Conectar
    status = await integration.connect()

    if status.connected:
        print(f"Conectado ao SAP S/4HANA!")
        print(f"URL: {status.system_url}")
        print(f"Ambiente: {status.environment}")
        print(f"Metodo de autenticacao: {status.auth_method}")

        # Desconectar
        await integration.disconnect()
    else:
        print(f"Falha na conexao: {status.message}")


# =============================================================================
# Exemplo 2: Conexao com configuracao manual
# =============================================================================
async def exemplo_conexao_manual():
    """
    Conecta ao SAP S/4HANA com configuracao manual.
    """
    print("=== Exemplo 2: Conexao Manual ===")

    integration = create_sap_s4hana_integration(
        system_url="https://my-s4.s4hana.ondemand.com",
        oauth_client_id="MEU_CLIENT_ID",
        oauth_client_secret="MEU_CLIENT_SECRET"
    )

    status = await integration.connect()
    print(f"Status: {'Conectado' if status.connected else 'Desconectado'}")


# =============================================================================
# Exemplo 3: Leitura de Business Partners
# =============================================================================
async def exemplo_leitura_dados():
    """
    Exemplo de leitura de dados do SAP S/4HANA.
    """
    print("=== Exemplo 3: Leitura de Dados ===")

    integration = SAPS4HanaIntegration.from_environment()
    await integration.connect()

    if integration.is_connected:
        # Ler Business Partners do Brasil
        result = await integration.read_business_partners(
            country="BR",
            limit=10
        )

        if result["success"]:
            print(f"Encontrados {result['count']} Business Partners")
            for bp in result["data"][:3]:
                print(f"  - {bp.get('BusinessPartner')}: {bp.get('BusinessPartnerFullName')}")

        # Ler ordens de venda
        orders = await integration.read_sales_orders(
            date_from="2024-01-01",
            limit=5
        )

        if orders["success"]:
            print(f"\nEncontradas {orders['count']} ordens de venda")

        await integration.disconnect()


# =============================================================================
# Exemplo 4: Geracao de CDS View
# =============================================================================
def exemplo_gerar_cds():
    """
    Exemplo de geracao de CDS View.
    """
    print("=== Exemplo 4: Geracao de CDS View ===")

    integration = SAPS4HanaIntegration()

    # Gerar view de consumo
    result = integration.generate_cds_view(
        name="Z_MY_SALES_ANALYSIS",
        base_view="I_SalesOrder",
        fields=[
            "SalesOrder",
            "SoldToParty",
            "TotalNetAmount",
            "TransactionCurrency",
            "SalesOrderDate"
        ],
        description="Analise de Ordens de Venda"
    )

    if result["success"]:
        print("CDS View gerada:")
        print("-" * 50)
        print(result["code"][:500])
        print("...")
    else:
        print(f"Erro: {result['message']}")


# =============================================================================
# Exemplo 5: Geracao de App Fiori
# =============================================================================
def exemplo_gerar_fiori():
    """
    Exemplo de geracao de aplicacao Fiori Elements.
    """
    print("=== Exemplo 5: Geracao de App Fiori ===")

    integration = SAPS4HanaIntegration()

    # Gerar List Report
    result = integration.generate_fiori_app(
        app_id="com.empresa.salesorders",
        title="Ordens de Venda",
        entity_set="SalesOrder",
        odata_service="/sap/opu/odata4/sap/api_sales_order/",
        list_fields=[
            {"name": "SalesOrder", "label": "Ordem", "is_key": True},
            {"name": "SoldToParty", "label": "Cliente"},
            {"name": "TotalNetAmount", "label": "Valor Total"},
            {"name": "TransactionCurrency", "label": "Moeda"}
        ],
        floorplan="list_report"
    )

    if result["success"]:
        print(f"Arquivos gerados: {list(result['files'].keys())}")
        print("\nmanifest.json (primeiros 300 caracteres):")
        print("-" * 50)
        print(result["files"]["manifest.json"][:300])
    else:
        print(f"Erro: {result['message']}")


# =============================================================================
# Exemplo 6: Uso do Query Builder OData
# =============================================================================
def exemplo_query_builder():
    """
    Exemplo de uso do OData Query Builder.
    """
    print("=== Exemplo 6: Query Builder ===")

    # Construir query complexa
    query = (ODataQueryBuilder()
        .select("SalesOrder", "SoldToParty", "TotalNetAmount", "SalesOrderDate")
        .filter("Country", "eq", "BR")
        .filter("TotalNetAmount", "gt", 10000)
        .expand("to_Item")
        .orderby("SalesOrderDate", desc=True)
        .top(100)
        .skip(0)
        .count()
    )

    query_string = query.build()
    print(f"Query gerada: {query_string}")


# =============================================================================
# Exemplo 7: Geracao de CDS View Analitica
# =============================================================================
def exemplo_cds_analitica():
    """
    Exemplo de geracao de CDS View analitica.
    """
    print("=== Exemplo 7: CDS View Analitica ===")

    integration = SAPS4HanaIntegration()

    result = integration.generate_analytical_view(
        name="Z_SALES_ANALYTICS",
        base_view="I_SalesOrder",
        dimensions=["SalesOrganization", "SoldToParty", "TransactionCurrency"],
        measures=[
            {"name": "TotalNetAmount", "aggregation": "SUM"},
            {"name": "NumberOfItems", "aggregation": "COUNT"}
        ],
        description="Analise de Vendas por Organizacao"
    )

    if result["success"]:
        print("CDS Analitica gerada:")
        print("-" * 50)
        print(result["code"][:600])


# =============================================================================
# Main
# =============================================================================
def main():
    """Executa todos os exemplos."""
    print("=" * 60)
    print("SAP S/4HANA Integration - Exemplos de Uso")
    print("=" * 60)
    print()

    # Exemplos sincronos
    exemplo_gerar_cds()
    print()

    exemplo_gerar_fiori()
    print()

    exemplo_query_builder()
    print()

    exemplo_cds_analitica()
    print()

    # Exemplos assincronos (requerem conexao real)
    print("Para executar exemplos de conexao, configure as variaveis de ambiente:")
    print("  - SAP_S4_SYSTEM_URL")
    print("  - SAP_S4_OAUTH_TOKEN_URL")
    print("  - SAP_S4_OAUTH_CLIENT_ID")
    print("  - SAP_S4_OAUTH_CLIENT_SECRET")


if __name__ == "__main__":
    main()
