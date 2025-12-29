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
