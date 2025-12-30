# -*- coding: utf-8 -*-
"""
Config Analyzer
===============
Analisador de configuracoes IMG (Implementation Guide) do SAP.

Este modulo fornece:
- Listagem de atividades de customizing
- Analise de tabelas de configuracao
- Verificacao de parametros por modulo
- Documentacao de configuracoes

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.analyzers import ConfigAnalyzer

analyzer = ConfigAnalyzer(rfc_client)

# Listar configuracoes de precos SD
configs = await analyzer.search_configs(area="SD", category="Pricing")

# Analisar tabela de customizing
config = await analyzer.analyze_config_table("T685")
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class ConfigCategory(str, Enum):
    """Categorias de configuracao"""
    ENTERPRISE_STRUCTURE = "enterprise_structure"
    MASTER_DATA = "master_data"
    TRANSACTIONS = "transactions"
    PRICING = "pricing"
    OUTPUT = "output"
    WORKFLOW = "workflow"
    REPORTING = "reporting"
    INTEGRATION = "integration"


class ConfigStatus(str, Enum):
    """Status da configuracao"""
    STANDARD = "standard"  # Configuracao SAP padrao
    CUSTOMIZED = "customized"  # Customizada pelo cliente
    PENDING = "pending"  # Pendente de configuracao
    OBSOLETE = "obsolete"  # Obsoleta


@dataclass
class ConfigEntry:
    """Entrada de configuracao"""
    key_fields: Dict[str, str]
    values: Dict[str, Any]
    description: str = ""
    created_by: str = ""
    created_on: Optional[datetime] = None
    changed_by: str = ""
    changed_on: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "key": self.key_fields,
            "values": self.values,
            "description": self.description,
            "created_by": self.created_by,
            "changed_by": self.changed_by
        }


@dataclass
class IMGConfig:
    """Configuracao do IMG (Implementation Guide)"""
    activity_id: str
    activity_name: str = ""
    description: str = ""
    transaction: str = ""
    table_name: str = ""
    view_name: str = ""
    img_path: List[str] = field(default_factory=list)
    module: str = ""
    category: ConfigCategory = ConfigCategory.TRANSACTIONS

    # Dados da tabela de config
    entries: List[ConfigEntry] = field(default_factory=list)
    total_entries: int = 0

    # Metadata
    package: str = ""
    status: ConfigStatus = ConfigStatus.STANDARD
    documentation: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "activity_id": self.activity_id,
            "activity_name": self.activity_name,
            "description": self.description,
            "transaction": self.transaction,
            "table": self.table_name,
            "view": self.view_name,
            "img_path": self.img_path,
            "module": self.module,
            "category": self.category.value,
            "entries_count": self.total_entries,
            "status": self.status.value
        }


@dataclass
class TransportEntry:
    """Entrada de transport request"""
    transport_number: str
    description: str = ""
    owner: str = ""
    target_system: str = ""
    status: str = ""
    release_date: Optional[datetime] = None
    objects: List[Dict[str, str]] = field(default_factory=list)


class ConfigAnalyzer:
    """
    Analisador de configuracoes SAP.

    Fornece analise de:
    - Atividades IMG
    - Tabelas de customizing
    - Parametros de modulos
    - Transport requests
    """

    # Tabelas de customizing por modulo
    CONFIG_TABLES = {
        "SD": {
            "pricing": ["T685", "T685A", "T682", "T682I", "T683", "T683S"],
            "output": ["NAST", "NACH", "T685B"],
            "partners": ["TPAR", "TPAER", "TB24"],
            "sales_org": ["TVKO", "TVKOV", "TVKZ"],
            "item_category": ["T184", "TVAP", "TVAU"],
            "delivery": ["TVLK", "TVLP", "TVLS"]
        },
        "MM": {
            "purchasing_org": ["T024", "T024E", "T024W"],
            "material_types": ["T134", "T134T", "T130M"],
            "valuation": ["T025", "T025T"],
            "vendors": ["LFA1", "LFB1", "LFM1"],
            "conditions": ["T681", "T681F", "T683V"]
        },
        "FI": {
            "company_code": ["T001", "T001B", "T001E"],
            "chart_of_accounts": ["T004", "T004T", "SKA1"],
            "fiscal_year": ["T009", "T009B", "T009Y"],
            "posting_keys": ["T074", "T074T", "T074U"],
            "document_types": ["T003", "T003T"]
        },
        "CO": {
            "controlling_area": ["TKA01", "TKA02", "TKA09"],
            "cost_elements": ["CSKA", "CSKB", "CSKU"],
            "cost_centers": ["CSKS", "CSKT"],
            "profit_centers": ["CEPC", "CEPCT"]
        },
        "PP": {
            "plant": ["T001W", "T001K", "T024W"],
            "mrp": ["T399D", "T399X", "T438A"],
            "production": ["T024F", "T003O", "T399I"],
            "bom": ["T415", "T416", "T418"]
        },
        "QM": {
            "inspection": ["TQ01", "TQ02", "TQ03"],
            "catalogs": ["TQ04", "TQ07", "QPCD"],
            "sampling": ["QAPO", "QAPP"]
        }
    }

    # Transacoes de customizing importantes
    CUSTOMIZING_TRANSACTIONS = {
        "SD": {
            "OVA1": "Determination Condition Type",
            "OVKK": "Pricing Procedure",
            "OV31": "Partner Determination",
            "VN01": "Condition Table",
            "OVB3": "Item Categories"
        },
        "MM": {
            "OMGN": "Purchase Order Types",
            "OME1": "Number Ranges",
            "OLMB": "MRP Configuration",
            "OMJJ": "Movement Types"
        },
        "FI": {
            "OBA7": "Document Types",
            "OB52": "Posting Periods",
            "FBZP": "Payment Program",
            "OB41": "Posting Keys"
        }
    }

    def __init__(self, rfc_client):
        """
        Inicializa o analisador.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client

    async def search_configs(
        self,
        area: Optional[str] = None,
        category: Optional[str] = None,
        table_name: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Busca atividades de customizing.

        Args:
            area: Area SAP (SD, MM, FI, etc.)
            category: Categoria (pricing, output, etc.)
            table_name: Nome da tabela
            max_results: Maximo de resultados

        Returns:
            Lista de configuracoes
        """
        result = []

        # Se area e categoria foram informadas, retornar tabelas conhecidas
        if area and category:
            tables = self.CONFIG_TABLES.get(area.upper(), {}).get(category.lower(), [])
            for table in tables:
                result.append({
                    "table_name": table,
                    "area": area.upper(),
                    "category": category
                })

        # Se table_name foi informada, buscar informacoes
        if table_name:
            info = await self._get_table_config_info(table_name)
            if info:
                result.append(info)

        # Buscar na tabela de IMG activities
        if not result and (area or category):
            img_activities = await self._search_img_activities(area, category)
            result.extend(img_activities[:max_results])

        return result

    async def _search_img_activities(
        self,
        area: Optional[str],
        category: Optional[str]
    ) -> List[Dict[str, Any]]:
        """Busca atividades IMG"""
        result = []

        try:
            # Buscar na CUS_IMGACH (atividades IMG)
            options = []
            if area:
                options.append({"TEXT": f"APPLFC = '{area}'"})

            activities = await self.rfc_client.read_table(
                "CUS_IMGACH",
                fields=["ACTIVITY", "ATTRIBUTES", "APPLFC", "TCODE"],
                options=options if options else None,
                max_rows=50
            )

            for act in activities:
                result.append({
                    "activity_id": act.get("ACTIVITY", ""),
                    "transaction": act.get("TCODE", ""),
                    "area": act.get("APPLFC", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao buscar atividades IMG: {e}")

        return result

    async def _get_table_config_info(self, table_name: str) -> Optional[Dict[str, Any]]:
        """Obtem informacoes de uma tabela de config"""
        try:
            # Verificar se e tabela de customizing (delivery class C ou E)
            table_info = await self.rfc_client.read_table(
                "DD02L",
                fields=["TABNAME", "CONTFLAG", "TABCLASS", "DEVCLASS"],
                options=[{"TEXT": f"TABNAME = '{table_name.upper()}'"}],
                max_rows=1
            )

            if table_info:
                info = table_info[0]
                delivery_class = info.get("CONTFLAG", "")

                # C = Customizing, E = Customizing (sem comparacao)
                is_customizing = delivery_class in ("C", "E")

                return {
                    "table_name": info.get("TABNAME", ""),
                    "is_customizing": is_customizing,
                    "delivery_class": delivery_class,
                    "package": info.get("DEVCLASS", ""),
                    "table_class": info.get("TABCLASS", "")
                }

        except Exception as e:
            logger.warning(f"Erro ao obter info da tabela {table_name}: {e}")

        return None

    async def analyze_config_table(
        self,
        table_name: str,
        filters: Optional[Dict[str, str]] = None,
        max_entries: int = 100
    ) -> IMGConfig:
        """
        Analisa uma tabela de configuracao.

        Args:
            table_name: Nome da tabela
            filters: Filtros para os dados
            max_entries: Maximo de entradas a retornar

        Returns:
            IMGConfig com a analise
        """
        table_name = table_name.upper()

        # Criar objeto de resultado
        config = IMGConfig(
            activity_id="",
            table_name=table_name
        )

        # Obter informacoes da tabela
        table_info = await self._get_table_config_info(table_name)
        if table_info:
            config.package = table_info.get("package", "")

        # Obter estrutura da tabela
        from .table_analyzer import TableAnalyzer
        table_analyzer = TableAnalyzer(self.rfc_client)
        structure = await table_analyzer.analyze_table(table_name)

        # Buscar dados
        options = []
        if filters:
            for field, value in filters.items():
                options.append({"TEXT": f"{field} = '{value}'"})

        try:
            data = await self.rfc_client.read_table(
                table_name,
                options=options if options else None,
                max_rows=max_entries
            )

            config.total_entries = len(data)

            # Converter para ConfigEntry
            key_fields = [f.name for f in structure.fields if f.is_key]

            for row in data[:max_entries]:
                keys = {k: row.get(k, "") for k in key_fields}
                values = {k: v for k, v in row.items() if k not in key_fields}

                config.entries.append(ConfigEntry(
                    key_fields=keys,
                    values=values
                ))

        except Exception as e:
            logger.warning(f"Erro ao ler dados de {table_name}: {e}")

        return config

    async def get_pricing_config(
        self,
        pricing_procedure: str,
        sales_org: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem configuracao de precos (pricing).

        Args:
            pricing_procedure: Procedimento de precos
            sales_org: Organizacao de vendas

        Returns:
            Configuracao de precos
        """
        result = {
            "procedure": pricing_procedure,
            "sales_org": sales_org,
            "condition_types": [],
            "access_sequences": [],
            "condition_tables": []
        }

        try:
            # Buscar condition types do procedure
            options = [{"TEXT": f"KVEWE = 'A' AND KAPPL = 'V' AND KALSM = '{pricing_procedure}'"}]

            steps = await self.rfc_client.read_table(
                "T683S",  # Pricing procedure steps
                fields=["STUNR", "ZAESSION", "KSCHL", "KITEFR", "KRESSION"],
                options=options,
                max_rows=100
            )

            for step in sorted(steps, key=lambda x: int(x.get("STUNR", 0) or 0)):
                result["condition_types"].append({
                    "step": step.get("STUNR", ""),
                    "counter": step.get("ZAESSION", ""),
                    "condition_type": step.get("KSCHL", ""),
                    "from_step": step.get("KITEFR", ""),
                    "to_step": step.get("KRERSION", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter pricing config: {e}")

        return result

    async def get_output_config(
        self,
        application: str = "V1",
        output_type: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem configuracao de output (NAST).

        Args:
            application: Aplicacao (V1=SD, E1=Purchasing, etc.)
            output_type: Tipo de output

        Returns:
            Configuracao de output
        """
        result = {
            "application": application,
            "output_types": []
        }

        try:
            options = [{"TEXT": f"KAPPL = '{application}'"}]
            if output_type:
                options.append({"TEXT": f"AND KSCHL = '{output_type}'"})

            outputs = await self.rfc_client.read_table(
                "T685B",  # Output determination conditions
                fields=["KAPPL", "KSCHL", "NAESSION", "BEESSION"],
                options=options,
                max_rows=50
            )

            for out in outputs:
                result["output_types"].append({
                    "type": out.get("KSCHL", ""),
                    "access_sequence": out.get("NAESSION", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter output config: {e}")

        return result

    async def get_partner_determination(
        self,
        partner_procedure: str
    ) -> Dict[str, Any]:
        """
        Obtem configuracao de determinacao de parceiros.

        Args:
            partner_procedure: Procedimento de parceiros

        Returns:
            Configuracao de parceiros
        """
        result = {
            "procedure": partner_procedure,
            "partner_functions": []
        }

        try:
            options = [{"TEXT": f"PARTNERSCHEMA = '{partner_procedure}'"}]

            partners = await self.rfc_client.read_table(
                "TPAR",  # Partner functions
                fields=["PARVW", "PARTNERSCHEMA", "PESSION"],
                options=options,
                max_rows=50
            )

            for partner in partners:
                result["partner_functions"].append({
                    "function": partner.get("PARVW", ""),
                    "mandatory": partner.get("PFLESSION", "") == "X"
                })

        except Exception as e:
            logger.warning(f"Erro ao obter partner determination: {e}")

        return result

    async def compare_configs(
        self,
        table_name: str,
        key_fields: Dict[str, str],
        compare_systems: List[str] = None
    ) -> Dict[str, Any]:
        """
        Compara configuracao entre entradas ou sistemas.

        Args:
            table_name: Nome da tabela
            key_fields: Campos chave para filtrar
            compare_systems: Sistemas para comparar (se RFC multi-sistema)

        Returns:
            Resultado da comparacao
        """
        result = {
            "table": table_name,
            "differences": []
        }

        # Por ora, apenas busca os dados atuais
        config = await self.analyze_config_table(table_name, key_fields)
        result["current"] = [e.to_dict() for e in config.entries]

        return result

    async def get_transport_requests(
        self,
        owner: Optional[str] = None,
        table_name: Optional[str] = None,
        max_results: int = 50
    ) -> List[TransportEntry]:
        """
        Lista transport requests relacionados a configuracao.

        Args:
            owner: Owner do transport
            table_name: Tabela de config especifica
            max_results: Maximo de resultados

        Returns:
            Lista de transports
        """
        transports = []

        try:
            options = []
            if owner:
                options.append({"TEXT": f"AS4USER = '{owner}'"})

            # Buscar transports modificaveis
            tr_data = await self.rfc_client.read_table(
                "E070",  # Transport header
                fields=["TRKORR", "TRFUNCTION", "TRSTATUS", "AS4USER", "AS4DATE"],
                options=options if options else None,
                max_rows=max_results
            )

            for tr in tr_data:
                status = tr.get("TRSTATUS", "")
                if status != "R":  # R = Released
                    transports.append(TransportEntry(
                        transport_number=tr.get("TRKORR", ""),
                        owner=tr.get("AS4USER", ""),
                        status=status
                    ))

        except Exception as e:
            logger.warning(f"Erro ao buscar transport requests: {e}")

        return transports

    def get_config_tables(self, module: str, category: Optional[str] = None) -> List[str]:
        """
        Retorna tabelas de configuracao de um modulo.

        Args:
            module: Modulo SAP (SD, MM, FI, etc.)
            category: Categoria especifica

        Returns:
            Lista de nomes de tabelas
        """
        module_tables = self.CONFIG_TABLES.get(module.upper(), {})

        if category:
            return module_tables.get(category.lower(), [])

        # Retornar todas as tabelas do modulo
        all_tables = []
        for tables in module_tables.values():
            all_tables.extend(tables)
        return list(set(all_tables))

    def get_customizing_transactions(self, module: str) -> Dict[str, str]:
        """
        Retorna transacoes de customizing de um modulo.

        Args:
            module: Modulo SAP

        Returns:
            Dicionario com transacao e descricao
        """
        return self.CUSTOMIZING_TRANSACTIONS.get(module.upper(), {})

    async def get_company_codes(
        self,
        company_code: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem lista de company codes (empresas) configuradas.

        Args:
            company_code: Filtrar por codigo especifico

        Returns:
            Lista de company codes com configuracoes
        """
        result = []

        try:
            options = []
            if company_code:
                options.append({"TEXT": f"BUKRS = '{company_code}'"})

            # Buscar na T001 (tabela principal de company codes)
            companies = await self.rfc_client.read_table(
                "T001",
                fields=["BUKRS", "BUTXT", "ORT01", "LAND1", "WAESSION", "SPRAS",
                        "KTOPL", "PERIV", "MESSION", "RCOMP"],
                options=options if options else None,
                max_rows=100
            )

            for company in companies:
                bukrs = company.get("BUKRS", "")

                # Buscar configuracoes adicionais na T001B
                add_configs = await self.rfc_client.read_table(
                    "T001B",
                    fields=["BUKRS", "BESSION", "OPESSION", "CLESSION"],
                    options=[{"TEXT": f"BUKRS = '{bukrs}'"}],
                    max_rows=10
                )

                result.append({
                    "company_code": bukrs,
                    "name": company.get("BUTXT", ""),
                    "city": company.get("ORT01", ""),
                    "country": company.get("LAND1", ""),
                    "currency": company.get("WAESSION", ""),
                    "language": company.get("SPRAS", ""),
                    "chart_of_accounts": company.get("KTOPL", ""),
                    "fiscal_year_variant": company.get("PERIV", ""),
                    "company": company.get("RCOMP", ""),
                    "additional_configs": [c for c in add_configs]
                })

        except Exception as e:
            logger.warning(f"Erro ao obter company codes: {e}")

        return result

    async def get_plants(
        self,
        plant: Optional[str] = None,
        company_code: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem lista de plantas (centros) configuradas.

        Args:
            plant: Filtrar por planta especifica
            company_code: Filtrar por company code

        Returns:
            Lista de plantas com configuracoes
        """
        result = []

        try:
            options = []
            if plant:
                options.append({"TEXT": f"WERKS = '{plant}'"})

            # Buscar na T001W (tabela de plantas)
            plants = await self.rfc_client.read_table(
                "T001W",
                fields=["WERKS", "NAME1", "NAME2", "STRAS", "ORT01", "LAND1",
                        "REESSION", "IESSION", "KUNNR", "LIFNR", "FABKL", "TXJCD"],
                options=options if options else None,
                max_rows=100
            )

            for p in plants:
                werks = p.get("WERKS", "")

                # Buscar ligacao com company code na T001K
                cc_link = await self.rfc_client.read_table(
                    "T001K",
                    fields=["BWKEY", "BUKRS", "XESSION"],
                    options=[{"TEXT": f"BWKEY = '{werks}'"}],
                    max_rows=1
                )

                plant_company = ""
                if cc_link:
                    plant_company = cc_link[0].get("BUKRS", "")

                # Aplicar filtro de company code se especificado
                if company_code and plant_company != company_code:
                    continue

                # Buscar MRP area
                mrp_data = await self.rfc_client.read_table(
                    "T399D",
                    fields=["WERKS", "BEESSION", "MRPPP"],
                    options=[{"TEXT": f"WERKS = '{werks}'"}],
                    max_rows=1
                )

                result.append({
                    "plant": werks,
                    "name": p.get("NAME1", ""),
                    "name2": p.get("NAME2", ""),
                    "street": p.get("STRAS", ""),
                    "city": p.get("ORT01", ""),
                    "country": p.get("LAND1", ""),
                    "region": p.get("REESSION", ""),
                    "customer_number": p.get("KUNNR", ""),
                    "vendor_number": p.get("LIFNR", ""),
                    "factory_calendar": p.get("FABKL", ""),
                    "company_code": plant_company,
                    "mrp_settings": mrp_data[0] if mrp_data else {}
                })

        except Exception as e:
            logger.warning(f"Erro ao obter plantas: {e}")

        return result

    async def get_organizational_structure(
        self,
        company_code: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem estrutura organizacional completa.

        Args:
            company_code: Filtrar por company code

        Returns:
            Estrutura organizacional hierarquica
        """
        result = {
            "company_codes": [],
            "plants": [],
            "storage_locations": [],
            "sales_organizations": [],
            "distribution_channels": [],
            "divisions": [],
            "purchasing_organizations": [],
            "purchasing_groups": []
        }

        try:
            # Company codes
            result["company_codes"] = await self.get_company_codes(company_code)

            # Plantas
            result["plants"] = await self.get_plants(company_code=company_code)

            # Depositos (storage locations)
            plant_list = [p["plant"] for p in result["plants"]]
            if plant_list:
                for plant in plant_list:
                    storage_locs = await self.rfc_client.read_table(
                        "T001L",
                        fields=["WERKS", "LGORT", "LGOBE", "XLESSION", "XBUFESSION"],
                        options=[{"TEXT": f"WERKS = '{plant}'"}],
                        max_rows=50
                    )
                    for sl in storage_locs:
                        result["storage_locations"].append({
                            "plant": sl.get("WERKS", ""),
                            "storage_location": sl.get("LGORT", ""),
                            "description": sl.get("LGOBE", "")
                        })

            # Organizacoes de vendas
            sales_orgs = await self.rfc_client.read_table(
                "TVKO",
                fields=["VKORG", "WAESSION", "BUKRS", "ADESSION"],
                options=None,
                max_rows=100
            )

            # Filtrar por company code se especificado
            for so in sales_orgs:
                if company_code and so.get("BUKRS", "") != company_code:
                    continue

                # Buscar texto
                so_text = await self.rfc_client.read_table(
                    "TVKOT",
                    fields=["VKORG", "VTEXT"],
                    options=[{"TEXT": f"VKORG = '{so.get('VKORG', '')}' AND SPRAS = 'P'"}],
                    max_rows=1
                )

                result["sales_organizations"].append({
                    "sales_org": so.get("VKORG", ""),
                    "currency": so.get("WAESSION", ""),
                    "company_code": so.get("BUKRS", ""),
                    "description": so_text[0].get("VTEXT", "") if so_text else ""
                })

            # Canais de distribuicao
            dist_channels = await self.rfc_client.read_table(
                "TVTW",
                fields=["VTWEG"],
                options=None,
                max_rows=50
            )

            for dc in dist_channels:
                dc_text = await self.rfc_client.read_table(
                    "TVTWT",
                    fields=["VTWEG", "VTEXT"],
                    options=[{"TEXT": f"VTWEG = '{dc.get('VTWEG', '')}' AND SPRAS = 'P'"}],
                    max_rows=1
                )

                result["distribution_channels"].append({
                    "distribution_channel": dc.get("VTWEG", ""),
                    "description": dc_text[0].get("VTEXT", "") if dc_text else ""
                })

            # Divisoes
            divisions = await self.rfc_client.read_table(
                "TSPA",
                fields=["SESSION"],
                options=None,
                max_rows=50
            )

            for div in divisions:
                div_text = await self.rfc_client.read_table(
                    "TSPAT",
                    fields=["SPART", "VTEXT"],
                    options=[{"TEXT": f"SPART = '{div.get('SPART', '')}' AND SPRAS = 'P'"}],
                    max_rows=1
                )

                result["divisions"].append({
                    "division": div.get("SPART", ""),
                    "description": div_text[0].get("VTEXT", "") if div_text else ""
                })

            # Organizacoes de compras
            purch_orgs = await self.rfc_client.read_table(
                "T024E",
                fields=["EKORG", "BUKRS", "LAND1"],
                options=None,
                max_rows=50
            )

            for po in purch_orgs:
                if company_code and po.get("BUKRS", "") != company_code:
                    continue

                po_text = await self.rfc_client.read_table(
                    "T024",
                    fields=["EKORG", "EKOTX"],
                    options=[{"TEXT": f"EKORG = '{po.get('EKORG', '')}'"}],
                    max_rows=1
                )

                result["purchasing_organizations"].append({
                    "purchasing_org": po.get("EKORG", ""),
                    "company_code": po.get("BUKRS", ""),
                    "country": po.get("LAND1", ""),
                    "description": po_text[0].get("EKOTX", "") if po_text else ""
                })

            # Grupos de compras
            purch_groups = await self.rfc_client.read_table(
                "T024",
                fields=["EKGRP", "EKNAM", "EKTEL"],
                options=None,
                max_rows=100
            )

            for pg in purch_groups:
                result["purchasing_groups"].append({
                    "purchasing_group": pg.get("EKGRP", ""),
                    "name": pg.get("EKNAM", ""),
                    "phone": pg.get("EKTEL", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter estrutura organizacional: {e}")

        return result

    async def get_sales_area_config(
        self,
        sales_org: str,
        distribution_channel: Optional[str] = None,
        division: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem configuracao completa de uma area de vendas.

        Args:
            sales_org: Organizacao de vendas
            distribution_channel: Canal de distribuicao
            division: Divisao

        Returns:
            Configuracao da area de vendas
        """
        sales_org = sales_org.upper()
        result = {
            "sales_org": sales_org,
            "distribution_channels": [],
            "divisions": [],
            "sales_areas": [],
            "pricing_procedures": [],
            "partner_determination": [],
            "output_determination": []
        }

        try:
            # Buscar canais de distribuicao associados
            channels = await self.rfc_client.read_table(
                "TVKOV",
                fields=["VKORG", "VTWEG"],
                options=[{"TEXT": f"VKORG = '{sales_org}'"}],
                max_rows=20
            )

            result["distribution_channels"] = [c.get("VTWEG", "") for c in channels]

            # Buscar divisoes associadas
            if channels:
                for channel in channels:
                    vtweg = channel.get("VTWEG", "")
                    if distribution_channel and vtweg != distribution_channel:
                        continue

                    divs = await self.rfc_client.read_table(
                        "TVKOS",
                        fields=["VKORG", "VTWEG", "SESSION"],
                        options=[{"TEXT": f"VKORG = '{sales_org}' AND VTWEG = '{vtweg}'"}],
                        max_rows=20
                    )

                    for d in divs:
                        spart = d.get("SPART", "")
                        if division and spart != division:
                            continue

                        result["sales_areas"].append({
                            "sales_org": sales_org,
                            "distribution_channel": vtweg,
                            "division": spart
                        })

            # Buscar procedimentos de pricing
            pricing = await self.rfc_client.read_table(
                "TVKV",
                fields=["VKORG", "VTWEG", "SPART", "KALKS"],
                options=[{"TEXT": f"VKORG = '{sales_org}'"}],
                max_rows=50
            )

            for p in pricing:
                result["pricing_procedures"].append({
                    "sales_org": p.get("VKORG", ""),
                    "dist_channel": p.get("VTWEG", ""),
                    "division": p.get("SPART", ""),
                    "procedure": p.get("KALKS", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter config de sales area: {e}")

        return result

    async def get_material_type_config(
        self,
        material_type: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem configuracao de tipos de material.

        Args:
            material_type: Tipo de material especifico

        Returns:
            Lista de tipos de material e suas configuracoes
        """
        result = []

        try:
            options = []
            if material_type:
                options.append({"TEXT": f"MTART = '{material_type}'"})

            # Buscar na T134 (tipos de material)
            types = await self.rfc_client.read_table(
                "T134",
                fields=["MTART", "MBESSION", "MTPESSION", "KZESSION", "MTYESSION",
                        "VMTPO", "EKESSION", "XKESSION", "MESSION", "PESSION"],
                options=options if options else None,
                max_rows=50
            )

            for t in types:
                mtart = t.get("MTART", "")

                # Buscar texto
                text_data = await self.rfc_client.read_table(
                    "T134T",
                    fields=["MTART", "MTBEZ"],
                    options=[{"TEXT": f"MTART = '{mtart}' AND SPRAS = 'P'"}],
                    max_rows=1
                )

                result.append({
                    "material_type": mtart,
                    "description": text_data[0].get("MTBEZ", "") if text_data else "",
                    "valuation_required": t.get("MBESSION", "") == "X",
                    "price_control": t.get("VPESSION", ""),
                    "qty_updating": t.get("MESSION", "") == "X",
                    "value_updating": t.get("WESSION", "") == "X",
                    "ext_purchase_orders": t.get("EKESSION", "") == "X"
                })

        except Exception as e:
            logger.warning(f"Erro ao obter config de material types: {e}")

        return result

    async def get_document_type_config(
        self,
        doc_type: Optional[str] = None,
        category: str = "FI"
    ) -> List[Dict[str, Any]]:
        """
        Obtem configuracao de tipos de documento.

        Args:
            doc_type: Tipo de documento especifico
            category: Categoria (FI, MM, SD)

        Returns:
            Lista de tipos de documento
        """
        result = []

        try:
            if category == "FI":
                # Tipos de documento FI (T003)
                options = []
                if doc_type:
                    options.append({"TEXT": f"BLART = '{doc_type}'"})

                docs = await self.rfc_client.read_table(
                    "T003",
                    fields=["BLART", "NUMKR", "XKOESSION", "STESSION"],
                    options=options if options else None,
                    max_rows=100
                )

                for d in docs:
                    blart = d.get("BLART", "")

                    text_data = await self.rfc_client.read_table(
                        "T003T",
                        fields=["BLART", "LTEXT"],
                        options=[{"TEXT": f"BLART = '{blart}' AND SPRAS = 'P'"}],
                        max_rows=1
                    )

                    result.append({
                        "doc_type": blart,
                        "description": text_data[0].get("LTEXT", "") if text_data else "",
                        "number_range": d.get("NUMKR", ""),
                        "category": "FI"
                    })

            elif category == "MM":
                # Tipos de documento MM (movimento - T156)
                options = []
                if doc_type:
                    options.append({"TEXT": f"BWART = '{doc_type}'"})

                docs = await self.rfc_client.read_table(
                    "T156",
                    fields=["BWART", "XSESSION", "XESSION", "KZBEW"],
                    options=options if options else None,
                    max_rows=100
                )

                for d in docs:
                    bwart = d.get("BWART", "")

                    text_data = await self.rfc_client.read_table(
                        "T156T",
                        fields=["BWART", "BTEXT"],
                        options=[{"TEXT": f"BWART = '{bwart}' AND SPRAS = 'P'"}],
                        max_rows=1
                    )

                    result.append({
                        "movement_type": bwart,
                        "description": text_data[0].get("BTEXT", "") if text_data else "",
                        "category": "MM"
                    })

            elif category == "SD":
                # Tipos de documento SD (vendas - TVAK)
                options = []
                if doc_type:
                    options.append({"TEXT": f"AUART = '{doc_type}'"})

                docs = await self.rfc_client.read_table(
                    "TVAK",
                    fields=["AUART", "NUMKI", "VBTYP", "AUFNR"],
                    options=options if options else None,
                    max_rows=100
                )

                for d in docs:
                    auart = d.get("AUART", "")

                    text_data = await self.rfc_client.read_table(
                        "TVAKT",
                        fields=["AUART", "BEZEI"],
                        options=[{"TEXT": f"AUART = '{auart}' AND SPRAS = 'P'"}],
                        max_rows=1
                    )

                    result.append({
                        "order_type": auart,
                        "description": text_data[0].get("BEZEI", "") if text_data else "",
                        "doc_category": d.get("VBTYP", ""),
                        "category": "SD"
                    })

        except Exception as e:
            logger.warning(f"Erro ao obter config de document types: {e}")

        return result

    async def get_number_ranges(
        self,
        object_name: str
    ) -> List[Dict[str, Any]]:
        """
        Obtem configuracao de faixas de numeracao.

        Args:
            object_name: Nome do objeto de numeracao

        Returns:
            Lista de faixas de numeracao
        """
        result = []
        object_name = object_name.upper()

        try:
            # Buscar na NRIV (intervalos de numeracao)
            ranges = await self.rfc_client.read_table(
                "NRIV",
                fields=["OBJECT", "SUBOBJECT", "NRRANGENR", "FROMNUMBER",
                        "TONUMBER", "NRLEVEL", "EXTERNIND"],
                options=[{"TEXT": f"OBJECT = '{object_name}'"}],
                max_rows=50
            )

            for r in ranges:
                result.append({
                    "object": r.get("OBJECT", ""),
                    "subobject": r.get("SUBOBJECT", ""),
                    "range_number": r.get("NRRANGENR", ""),
                    "from": r.get("FROMNUMBER", ""),
                    "to": r.get("TONUMBER", ""),
                    "current_number": r.get("NRLEVEL", ""),
                    "external": r.get("EXTERNIND", "") == "X"
                })

        except Exception as e:
            logger.warning(f"Erro ao obter number ranges: {e}")

        return result

    async def analyze_config_consistency(
        self,
        company_code: str
    ) -> Dict[str, Any]:
        """
        Analisa consistencia de configuracoes de um company code.

        Args:
            company_code: Codigo da empresa

        Returns:
            Relatorio de consistencia
        """
        company_code = company_code.upper()
        result = {
            "company_code": company_code,
            "checks_passed": [],
            "warnings": [],
            "errors": []
        }

        try:
            # Verificar se company code existe
            cc = await self.get_company_codes(company_code)
            if not cc:
                result["errors"].append(f"Company code {company_code} nao encontrado")
                return result

            cc_info = cc[0]
            result["checks_passed"].append("Company code exists")

            # Verificar plano de contas
            if cc_info.get("chart_of_accounts"):
                result["checks_passed"].append(f"Chart of accounts: {cc_info['chart_of_accounts']}")
            else:
                result["errors"].append("Chart of accounts not configured")

            # Verificar variante de exercicio
            if cc_info.get("fiscal_year_variant"):
                result["checks_passed"].append(f"Fiscal year variant: {cc_info['fiscal_year_variant']}")
            else:
                result["errors"].append("Fiscal year variant not configured")

            # Verificar plantas associadas
            plants = await self.get_plants(company_code=company_code)
            if plants:
                result["checks_passed"].append(f"Plants configured: {len(plants)}")
            else:
                result["warnings"].append("No plants assigned to company code")

            # Verificar organizacoes de vendas
            org_struct = await self.get_organizational_structure(company_code)
            if org_struct.get("sales_organizations"):
                result["checks_passed"].append(
                    f"Sales organizations: {len(org_struct['sales_organizations'])}"
                )
            else:
                result["warnings"].append("No sales organizations assigned")

            if org_struct.get("purchasing_organizations"):
                result["checks_passed"].append(
                    f"Purchasing organizations: {len(org_struct['purchasing_organizations'])}"
                )
            else:
                result["warnings"].append("No purchasing organizations assigned")

        except Exception as e:
            result["errors"].append(f"Error during analysis: {str(e)}")

        return result

    async def export_config_documentation(
        self,
        company_code: str
    ) -> Dict[str, Any]:
        """
        Exporta documentacao completa de configuracao.

        Args:
            company_code: Codigo da empresa

        Returns:
            Documentacao completa de configuracao
        """
        company_code = company_code.upper()

        result = {
            "company_code": company_code,
            "generated_at": None,
            "enterprise_structure": {},
            "financial_accounting": {},
            "materials_management": {},
            "sales_distribution": {},
            "controlling": {}
        }

        try:
            from datetime import datetime
            result["generated_at"] = datetime.now().isoformat()

            # Estrutura empresarial
            result["enterprise_structure"] = await self.get_organizational_structure(company_code)

            # Configuracoes FI
            fi_config = {
                "company_codes": await self.get_company_codes(company_code),
                "document_types": await self.get_document_type_config(category="FI"),
                "posting_keys": [],
                "tax_codes": []
            }

            # Posting keys
            posting_keys = await self.rfc_client.read_table(
                "T074",
                fields=["BSCHL", "KOESSION", "XESSION"],
                options=None,
                max_rows=100
            )
            fi_config["posting_keys"] = [
                {"key": pk.get("BSCHL", ""), "account_type": pk.get("KOART", "")}
                for pk in posting_keys
            ]

            result["financial_accounting"] = fi_config

            # Configuracoes MM
            mm_config = {
                "material_types": await self.get_material_type_config(),
                "movement_types": await self.get_document_type_config(category="MM"),
                "purchasing_organizations": result["enterprise_structure"].get("purchasing_organizations", []),
                "plants": result["enterprise_structure"].get("plants", [])
            }
            result["materials_management"] = mm_config

            # Configuracoes SD
            sd_config = {
                "sales_organizations": result["enterprise_structure"].get("sales_organizations", []),
                "distribution_channels": result["enterprise_structure"].get("distribution_channels", []),
                "divisions": result["enterprise_structure"].get("divisions", []),
                "order_types": await self.get_document_type_config(category="SD")
            }
            result["sales_distribution"] = sd_config

        except Exception as e:
            logger.warning(f"Erro ao exportar documentacao: {e}")
            result["error"] = str(e)

        return result

    async def get_valuation_config(
        self,
        plant: str
    ) -> Dict[str, Any]:
        """
        Obtem configuracao de avaliacao (valuation) de uma planta.

        Args:
            plant: Codigo da planta

        Returns:
            Configuracao de avaliacao
        """
        plant = plant.upper()
        result = {
            "plant": plant,
            "valuation_area": "",
            "valuation_level": "",
            "company_code": "",
            "price_control": "",
            "valuation_classes": []
        }

        try:
            # Buscar area de avaliacao
            val_area = await self.rfc_client.read_table(
                "T001K",
                fields=["BWKEY", "BUKRS", "XESSION", "MLESSION"],
                options=[{"TEXT": f"BWKEY = '{plant}'"}],
                max_rows=1
            )

            if val_area:
                va = val_area[0]
                result["valuation_area"] = va.get("BWKEY", "")
                result["company_code"] = va.get("BUKRS", "")

            # Buscar classes de avaliacao
            val_classes = await self.rfc_client.read_table(
                "T025",
                fields=["BKLAS", "OKLAS", "KONTF"],
                options=None,
                max_rows=100
            )

            for vc in val_classes:
                bklas = vc.get("BKLAS", "")

                text_data = await self.rfc_client.read_table(
                    "T025T",
                    fields=["BKLAS", "BKBEZ"],
                    options=[{"TEXT": f"BKLAS = '{bklas}' AND SPRAS = 'P'"}],
                    max_rows=1
                )

                result["valuation_classes"].append({
                    "class": bklas,
                    "description": text_data[0].get("BKBEZ", "") if text_data else "",
                    "account_category": vc.get("OKLAS", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter valuation config: {e}")

        return result

    async def get_workflow_config(
        self,
        task_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem configuracao de workflows.

        Args:
            task_id: ID da tarefa de workflow

        Returns:
            Lista de configuracoes de workflow
        """
        result = []

        try:
            options = []
            if task_id:
                options.append({"TEXT": f"WI_TASK = '{task_id}'"})

            # Buscar tarefas de workflow
            tasks = await self.rfc_client.read_table(
                "SWWTASK",
                fields=["WI_TASK", "TASK_TYPE", "TASK_STATUS", "CREATED_BY", "CREATED_AT"],
                options=options if options else None,
                max_rows=100
            )

            for task in tasks:
                result.append({
                    "task_id": task.get("WI_TASK", ""),
                    "type": task.get("TASK_TYPE", ""),
                    "status": task.get("TASK_STATUS", ""),
                    "created_by": task.get("CREATED_BY", ""),
                    "created_at": task.get("CREATED_AT", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter workflow config: {e}")

        return result
