# -*- coding: utf-8 -*-
"""
Table Analyzer
==============
Analisador de tabelas e estruturas de dados do SAP.

Este modulo fornece:
- Listagem de tabelas SAP
- Analise de estrutura de tabelas
- Extracao de campos e tipos
- Relacoes entre tabelas
- Indices e chaves

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.analyzers import TableAnalyzer

analyzer = TableAnalyzer(rfc_client)

# Listar tabelas de materiais
tables = await analyzer.list_tables(prefix="MAR")

# Analisar estrutura da tabela MARA
structure = await analyzer.analyze_table("MARA")
print(structure.fields)
print(structure.primary_key)
```
"""

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class TableCategory(str, Enum):
    """Categoria de tabela SAP"""
    TRANSPARENT = "T"  # Tabela transparente
    POOLED = "P"  # Tabela pool
    CLUSTER = "C"  # Tabela cluster
    VIEW = "V"  # View
    STRUCTURE = "S"  # Estrutura
    APPEND = "A"  # Append structure


class DeliveryClass(str, Enum):
    """Classe de entrega da tabela"""
    APPLICATION = "A"  # Tabela de aplicacao
    CUSTOMIZING = "C"  # Tabela de customizing
    SYSTEM = "S"  # Tabela de sistema
    TEMPORARY = "L"  # Dados temporarios
    GENERATED = "G"  # Tabela gerada
    CUSTOMIZING_NO_COMPARE = "E"  # Customizing sem comparacao
    CUSTOMER_DATA = "W"  # Dados do cliente


@dataclass
class TableField:
    """Campo de tabela SAP"""
    name: str
    description: str = ""
    data_element: str = ""
    data_type: str = ""
    length: int = 0
    decimals: int = 0
    is_key: bool = False
    is_nullable: bool = True
    check_table: Optional[str] = None
    domain: Optional[str] = None
    position: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "data_element": self.data_element,
            "data_type": self.data_type,
            "length": self.length,
            "decimals": self.decimals,
            "is_key": self.is_key,
            "is_nullable": self.is_nullable,
            "check_table": self.check_table,
            "domain": self.domain,
            "position": self.position
        }


@dataclass
class TableIndex:
    """Indice de tabela SAP"""
    name: str
    is_unique: bool = False
    fields: List[str] = field(default_factory=list)
    description: str = ""


@dataclass
class TableRelation:
    """Relacao entre tabelas (foreign key)"""
    foreign_table: str
    fields_mapping: Dict[str, str] = field(default_factory=dict)
    cardinality: str = "1:N"  # 1:1, 1:N, N:M


@dataclass
class TableStructure:
    """Estrutura completa de uma tabela SAP"""
    name: str
    description: str = ""
    category: TableCategory = TableCategory.TRANSPARENT
    delivery_class: DeliveryClass = DeliveryClass.APPLICATION
    package: str = ""
    table_class: str = ""  # APPL0, APPL1, etc.

    # Campos
    fields: List[TableField] = field(default_factory=list)
    primary_key: List[str] = field(default_factory=list)

    # Indices
    indexes: List[TableIndex] = field(default_factory=list)

    # Relacoes
    relations: List[TableRelation] = field(default_factory=list)
    check_tables: Set[str] = field(default_factory=set)

    # Estatisticas
    total_records: Optional[int] = None
    size_bytes: Optional[int] = None

    # Enhancement category
    enhancement_category: str = ""  # Can be enhanced, Cannot be enhanced, etc.

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "category": self.category.value,
            "delivery_class": self.delivery_class.value,
            "package": self.package,
            "fields": [f.to_dict() for f in self.fields],
            "primary_key": self.primary_key,
            "indexes": [
                {"name": idx.name, "unique": idx.is_unique, "fields": idx.fields}
                for idx in self.indexes
            ],
            "check_tables": list(self.check_tables),
            "total_records": self.total_records
        }


class TableAnalyzer:
    """
    Analisador de tabelas SAP.

    Fornece analise de estrutura de tabelas incluindo:
    - Campos e tipos de dados
    - Chaves primarias e indices
    - Relacoes com outras tabelas
    - Check tables e dominios
    """

    # Mapeamento de tipos ABAP para descricao
    DATA_TYPES = {
        "C": "Character",
        "N": "Numeric Text",
        "D": "Date",
        "T": "Time",
        "X": "Hexadecimal",
        "I": "Integer",
        "P": "Packed Decimal",
        "F": "Float",
        "STRING": "String",
        "XSTRING": "Byte String",
        "INT1": "1-byte Integer",
        "INT2": "2-byte Integer",
        "INT4": "4-byte Integer",
        "INT8": "8-byte Integer",
        "DEC": "Decimal",
        "NUMC": "Numeric Character",
        "CHAR": "Character",
        "RAW": "Raw Data",
        "RAWSTRING": "Raw String",
        "FLTP": "Floating Point",
        "CURR": "Currency",
        "CUKY": "Currency Key",
        "QUAN": "Quantity",
        "UNIT": "Unit",
        "LANG": "Language",
        "CLNT": "Client",
        "DATS": "Date",
        "TIMS": "Time",
        "ACCP": "Posting Period",
        "PREC": "Precision"
    }

    # Tabelas importantes do SAP por modulo
    IMPORTANT_TABLES = {
        "MM": ["MARA", "MARC", "MARD", "MAKT", "MBEW", "EKKO", "EKPO", "EBAN", "EBKN"],
        "SD": ["VBAK", "VBAP", "VBUK", "VBUP", "LIKP", "LIPS", "VBRK", "VBRP", "KNA1"],
        "FI": ["BKPF", "BSEG", "BSIK", "BSID", "BSAK", "BSAD", "SKA1", "SKB1", "T001"],
        "CO": ["CSKA", "CSKB", "CSKS", "COSS", "COSP", "AUFK", "AFKO", "AFPO"],
        "PP": ["AFKO", "AFPO", "AFVC", "AFVV", "MAST", "STKO", "STPO", "PLKO", "PLPO"],
        "HR": ["PA0000", "PA0001", "PA0002", "PA0006", "PA0008", "HRP1000", "HRP1001"],
        "QM": ["QALS", "QAVE", "QAMV", "QMFE", "QMEL", "QMSM"],
        "PM": ["EQUI", "ILOA", "IFLOT", "AUFK", "AFIH", "MHIS"]
    }

    def __init__(self, rfc_client):
        """
        Inicializa o analisador.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client

    async def list_tables(
        self,
        prefix: str = "",
        package: Optional[str] = None,
        description_contains: Optional[str] = None,
        max_results: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Lista tabelas do dicionario de dados.

        Args:
            prefix: Prefixo do nome da tabela
            package: Pacote/Devclass
            description_contains: Filtro por descricao
            max_results: Maximo de resultados

        Returns:
            Lista de tabelas
        """
        options = []
        if prefix:
            options.append({"TEXT": f"TABNAME LIKE '{prefix}%'"})
        if package:
            if options:
                options.append({"TEXT": f"AND DEVCLASS = '{package}'"})
            else:
                options.append({"TEXT": f"DEVCLASS = '{package}'"})

        tables = await self.rfc_client.read_table(
            "DD02L",
            fields=["TABNAME", "TABCLASS", "CONTFLAG", "SQLTAB", "MATEFLAG"],
            options=options,
            max_rows=max_results
        )

        # Buscar descricoes
        table_names = [t["TABNAME"] for t in tables if t.get("TABNAME")]
        descriptions = await self._get_table_descriptions(table_names)

        result = []
        for table in tables:
            tab_name = table.get("TABNAME", "")
            result.append({
                "name": tab_name,
                "description": descriptions.get(tab_name, ""),
                "category": table.get("TABCLASS", ""),
                "delivery_class": table.get("CONTFLAG", ""),
                "sql_name": table.get("SQLTAB", "") or tab_name
            })

        return result

    async def _get_table_descriptions(
        self,
        table_names: List[str],
        language: str = "P"
    ) -> Dict[str, str]:
        """Busca descricoes das tabelas"""
        if not table_names:
            return {}

        # Montar WHERE para lista de tabelas
        names_str = "', '".join(table_names)
        options = [{"TEXT": f"TABNAME IN ('{names_str}') AND DDLANGUAGE = '{language}'"}]

        texts = await self.rfc_client.read_table(
            "DD02T",
            fields=["TABNAME", "DDTEXT"],
            options=options,
            max_rows=len(table_names) * 2
        )

        return {t["TABNAME"]: t.get("DDTEXT", "") for t in texts}

    async def analyze_table(self, table_name: str) -> TableStructure:
        """
        Analisa estrutura completa de uma tabela.

        Args:
            table_name: Nome da tabela

        Returns:
            TableStructure com a analise completa
        """
        table_name = table_name.upper()

        # Obter informacoes basicas
        table_info = await self._get_table_info(table_name)

        if not table_info:
            return TableStructure(
                name=table_name,
                description="Tabela nao encontrada"
            )

        # Criar estrutura
        structure = TableStructure(
            name=table_name,
            description=table_info.get("description", ""),
            category=self._parse_category(table_info.get("category", "T")),
            delivery_class=self._parse_delivery_class(table_info.get("delivery_class", "A")),
            package=table_info.get("package", "")
        )

        # Obter campos
        structure.fields = await self._get_table_fields(table_name)

        # Identificar chave primaria
        structure.primary_key = [
            f.name for f in structure.fields if f.is_key
        ]

        # Obter indices
        structure.indexes = await self._get_table_indexes(table_name)

        # Identificar check tables
        structure.check_tables = {
            f.check_table for f in structure.fields
            if f.check_table
        }

        return structure

    async def _get_table_info(self, table_name: str) -> Dict[str, Any]:
        """Obtem informacoes basicas da tabela"""
        # Buscar na DD02L
        tables = await self.rfc_client.read_table(
            "DD02L",
            fields=["TABNAME", "TABCLASS", "CONTFLAG", "DEVCLASS", "MATEFLAG"],
            options=[{"TEXT": f"TABNAME = '{table_name}'"}],
            max_rows=1
        )

        if not tables:
            return {}

        info = tables[0]

        # Buscar descricao
        descriptions = await self._get_table_descriptions([table_name])

        return {
            "name": info.get("TABNAME", ""),
            "category": info.get("TABCLASS", "T"),
            "delivery_class": info.get("CONTFLAG", "A"),
            "package": info.get("DEVCLASS", ""),
            "description": descriptions.get(table_name, "")
        }

    async def _get_table_fields(self, table_name: str) -> List[TableField]:
        """Obtem campos da tabela"""
        # Buscar campos na DD03L
        fields_data = await self.rfc_client.read_table(
            "DD03L",
            fields=["FIELDNAME", "POSITION", "KEYFLAG", "ROLLNAME", "CHECKTABLE",
                    "DATATYPE", "LENG", "DECIMALS", "DOMNAME", "NOTNULL"],
            options=[{"TEXT": f"TABNAME = '{table_name}'"}],
            max_rows=500
        )

        # Coletar data elements para buscar descricoes
        data_elements = list(set([f.get("ROLLNAME", "") for f in fields_data if f.get("ROLLNAME")]))

        # Buscar descricoes dos data elements
        descriptions = await self._get_data_element_descriptions(data_elements)

        fields = []
        for fd in sorted(fields_data, key=lambda x: int(x.get("POSITION", 0) or 0)):
            field_name = fd.get("FIELDNAME", "")

            # Ignorar campos de inclusao (.INCLUDE, .APPEND)
            if field_name.startswith("."):
                continue

            data_element = fd.get("ROLLNAME", "")
            description = descriptions.get(data_element, field_name)

            fields.append(TableField(
                name=field_name,
                description=description,
                data_element=data_element,
                data_type=fd.get("DATATYPE", ""),
                length=int(fd.get("LENG", 0) or 0),
                decimals=int(fd.get("DECIMALS", 0) or 0),
                is_key=fd.get("KEYFLAG", "") == "X",
                is_nullable=fd.get("NOTNULL", "") != "X",
                check_table=fd.get("CHECKTABLE", "") or None,
                domain=fd.get("DOMNAME", "") or None,
                position=int(fd.get("POSITION", 0) or 0)
            ))

        return fields

    async def _get_data_element_descriptions(
        self,
        data_elements: List[str],
        language: str = "P"
    ) -> Dict[str, str]:
        """Busca descricoes dos data elements"""
        if not data_elements:
            return {}

        names_str = "', '".join(data_elements)
        options = [{"TEXT": f"ROLLNAME IN ('{names_str}') AND DDLANGUAGE = '{language}'"}]

        texts = await self.rfc_client.read_table(
            "DD04T",
            fields=["ROLLNAME", "DDTEXT"],
            options=options,
            max_rows=len(data_elements) * 2
        )

        return {t["ROLLNAME"]: t.get("DDTEXT", "") for t in texts}

    async def _get_table_indexes(self, table_name: str) -> List[TableIndex]:
        """Obtem indices da tabela"""
        # Buscar indices na DD12L
        indexes_data = await self.rfc_client.read_table(
            "DD12L",
            fields=["SQLTAB", "INDEXNAME", "UNIQUEFLAG"],
            options=[{"TEXT": f"SQLTAB = '{table_name}'"}],
            max_rows=50
        )

        indexes = []
        for idx_data in indexes_data:
            idx_name = idx_data.get("INDEXNAME", "")

            # Buscar campos do indice
            idx_fields = await self.rfc_client.read_table(
                "DD17S",
                fields=["SQLTAB", "INDEXNAME", "FIELDNAME", "POSITION"],
                options=[{"TEXT": f"SQLTAB = '{table_name}' AND INDEXNAME = '{idx_name}'"}],
                max_rows=20
            )

            field_names = [
                f["FIELDNAME"]
                for f in sorted(idx_fields, key=lambda x: int(x.get("POSITION", 0) or 0))
            ]

            indexes.append(TableIndex(
                name=idx_name,
                is_unique=idx_data.get("UNIQUEFLAG", "") == "X",
                fields=field_names
            ))

        return indexes

    async def get_table_content_sample(
        self,
        table_name: str,
        fields: Optional[List[str]] = None,
        max_rows: int = 10,
        where: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem amostra de dados da tabela.

        Args:
            table_name: Nome da tabela
            fields: Campos a retornar (None = todos)
            max_rows: Maximo de linhas
            where: Clausula WHERE

        Returns:
            Lista de registros
        """
        options = []
        if where:
            options.append({"TEXT": where})

        return await self.rfc_client.read_table(
            table_name,
            fields=fields,
            options=options if options else None,
            max_rows=max_rows
        )

    async def get_related_tables(
        self,
        table_name: str
    ) -> Dict[str, List[str]]:
        """
        Encontra tabelas relacionadas.

        Args:
            table_name: Nome da tabela

        Returns:
            Dicionario com check_tables e dependent_tables
        """
        # Analisar a tabela
        structure = await self.analyze_table(table_name)

        # Check tables (a tabela usa estas)
        check_tables = list(structure.check_tables)

        # Tabelas dependentes (outras tabelas que usam esta como check table)
        dependent = await self.rfc_client.read_table(
            "DD03L",
            fields=["TABNAME"],
            options=[{"TEXT": f"CHECKTABLE = '{table_name}'"}],
            max_rows=100
        )

        dependent_tables = list(set([d["TABNAME"] for d in dependent]))

        return {
            "check_tables": check_tables,
            "dependent_tables": dependent_tables
        }

    async def search_tables_by_field(
        self,
        field_name: str,
        data_element: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Busca tabelas que contem um campo especifico.

        Args:
            field_name: Nome do campo
            data_element: Data element do campo

        Returns:
            Lista de tabelas
        """
        options = [{"TEXT": f"FIELDNAME = '{field_name}'"}]
        if data_element:
            options.append({"TEXT": f"AND ROLLNAME = '{data_element}'"})

        fields = await self.rfc_client.read_table(
            "DD03L",
            fields=["TABNAME", "FIELDNAME", "ROLLNAME", "KEYFLAG"],
            options=options,
            max_rows=100
        )

        # Obter descricoes das tabelas
        table_names = list(set([f["TABNAME"] for f in fields]))
        descriptions = await self._get_table_descriptions(table_names)

        result = []
        for f in fields:
            tab_name = f["TABNAME"]
            result.append({
                "table": tab_name,
                "description": descriptions.get(tab_name, ""),
                "field": f["FIELDNAME"],
                "data_element": f.get("ROLLNAME", ""),
                "is_key": f.get("KEYFLAG", "") == "X"
            })

        return result

    def _parse_category(self, category: str) -> TableCategory:
        """Converte string para TableCategory"""
        try:
            return TableCategory(category.upper())
        except ValueError:
            return TableCategory.TRANSPARENT

    def _parse_delivery_class(self, delivery_class: str) -> DeliveryClass:
        """Converte string para DeliveryClass"""
        try:
            return DeliveryClass(delivery_class.upper())
        except ValueError:
            return DeliveryClass.APPLICATION

    def get_important_tables(self, module: str) -> List[str]:
        """
        Retorna lista de tabelas importantes de um modulo.

        Args:
            module: Modulo SAP (MM, SD, FI, etc.)

        Returns:
            Lista de nomes de tabelas
        """
        return self.IMPORTANT_TABLES.get(module.upper(), [])

    def get_data_type_description(self, data_type: str) -> str:
        """
        Retorna descricao de um tipo de dados ABAP.

        Args:
            data_type: Codigo do tipo

        Returns:
            Descricao do tipo
        """
        return self.DATA_TYPES.get(data_type.upper(), "Unknown")

    async def get_foreign_key_relationships(
        self,
        table_name: str
    ) -> List[TableRelation]:
        """
        Obtem relacionamentos de foreign key de uma tabela.

        Args:
            table_name: Nome da tabela

        Returns:
            Lista de relacionamentos (foreign keys)
        """
        table_name = table_name.upper()
        relations = []

        try:
            # Buscar foreign keys na DD05S (tabela de foreign keys)
            fk_data = await self.rfc_client.read_table(
                "DD05S",
                fields=["TABNAME", "FIELDNAME", "FORTABLE", "FORKEY", "CHECKTABLE",
                        "CHECKFIELD", "PRIMPOS", "FRKART"],
                options=[{"TEXT": f"TABNAME = '{table_name}'"}],
                max_rows=200
            )

            # Agrupar por tabela estrangeira
            fk_by_table: Dict[str, Dict[str, str]] = {}
            for fk in fk_data:
                check_table = fk.get("CHECKTABLE", "") or fk.get("FORTABLE", "")
                if not check_table:
                    continue

                if check_table not in fk_by_table:
                    fk_by_table[check_table] = {}

                local_field = fk.get("FIELDNAME", "")
                foreign_field = fk.get("CHECKFIELD", "") or fk.get("FORKEY", "")

                if local_field and foreign_field:
                    fk_by_table[check_table][local_field] = foreign_field

            # Criar objetos TableRelation
            for foreign_table, field_mapping in fk_by_table.items():
                relations.append(TableRelation(
                    foreign_table=foreign_table,
                    fields_mapping=field_mapping,
                    cardinality="N:1"  # Foreign key tipicamente e N:1
                ))

        except Exception as e:
            logger.warning(f"Erro ao obter foreign keys de {table_name}: {e}")

        return relations

    async def get_data_element_info(
        self,
        data_element: str
    ) -> Dict[str, Any]:
        """
        Obtem informacoes detalhadas de um data element.

        Args:
            data_element: Nome do data element

        Returns:
            Dicionario com informacoes do data element
        """
        data_element = data_element.upper()
        result = {
            "name": data_element,
            "description": "",
            "domain": "",
            "data_type": "",
            "length": 0,
            "decimals": 0,
            "labels": {}
        }

        try:
            # Buscar definicao do data element na DD04L
            de_data = await self.rfc_client.read_table(
                "DD04L",
                fields=["ROLLNAME", "DOMNAME", "DATATYPE", "LENG", "DECIMALS",
                        "HEADLEN", "SCRLEN1", "SCRLEN2", "SCRLEN3"],
                options=[{"TEXT": f"ROLLNAME = '{data_element}'"}],
                max_rows=1
            )

            if de_data:
                de = de_data[0]
                result["domain"] = de.get("DOMNAME", "")
                result["data_type"] = de.get("DATATYPE", "")
                result["length"] = int(de.get("LENG", 0) or 0)
                result["decimals"] = int(de.get("DECIMALS", 0) or 0)
                result["labels"] = {
                    "header": int(de.get("HEADLEN", 0) or 0),
                    "short": int(de.get("SCRLEN1", 0) or 0),
                    "medium": int(de.get("SCRLEN2", 0) or 0),
                    "long": int(de.get("SCRLEN3", 0) or 0)
                }

            # Buscar textos (descricoes) na DD04T
            texts = await self.rfc_client.read_table(
                "DD04T",
                fields=["ROLLNAME", "DDLANGUAGE", "DDTEXT", "REPTEXT", "SCRTEXT_S",
                        "SCRTEXT_M", "SCRTEXT_L"],
                options=[{"TEXT": f"ROLLNAME = '{data_element}' AND DDLANGUAGE IN ('P', 'E')"}],
                max_rows=2
            )

            for text in texts:
                lang = text.get("DDLANGUAGE", "")
                if lang == "P" or (lang == "E" and not result["description"]):
                    result["description"] = text.get("DDTEXT", "")
                    result["labels"]["header_text"] = text.get("REPTEXT", "")
                    result["labels"]["short_text"] = text.get("SCRTEXT_S", "")
                    result["labels"]["medium_text"] = text.get("SCRTEXT_M", "")
                    result["labels"]["long_text"] = text.get("SCRTEXT_L", "")

        except Exception as e:
            logger.warning(f"Erro ao obter info do data element {data_element}: {e}")

        return result

    async def get_domain_info(
        self,
        domain: str
    ) -> Dict[str, Any]:
        """
        Obtem informacoes detalhadas de um domain.

        Args:
            domain: Nome do domain

        Returns:
            Dicionario com informacoes do domain incluindo valores fixos
        """
        domain = domain.upper()
        result = {
            "name": domain,
            "description": "",
            "data_type": "",
            "length": 0,
            "decimals": 0,
            "output_length": 0,
            "conversion_exit": "",
            "value_table": "",
            "fixed_values": []
        }

        try:
            # Buscar definicao do domain na DD01L
            dom_data = await self.rfc_client.read_table(
                "DD01L",
                fields=["DOMNAME", "DATATYPE", "LENG", "DECIMALS", "OUTPUTLEN",
                        "CONVEXIT", "VALEXI", "ENTITYTAB"],
                options=[{"TEXT": f"DOMNAME = '{domain}'"}],
                max_rows=1
            )

            if dom_data:
                dom = dom_data[0]
                result["data_type"] = dom.get("DATATYPE", "")
                result["length"] = int(dom.get("LENG", 0) or 0)
                result["decimals"] = int(dom.get("DECIMALS", 0) or 0)
                result["output_length"] = int(dom.get("OUTPUTLEN", 0) or 0)
                result["conversion_exit"] = dom.get("CONVEXIT", "")
                result["value_table"] = dom.get("ENTITYTAB", "")

                # Verificar se tem valores fixos
                has_fixed_values = dom.get("VALEXI", "") == "X"

                if has_fixed_values:
                    # Buscar valores fixos na DD07L
                    values = await self.rfc_client.read_table(
                        "DD07L",
                        fields=["DOMNAME", "VALPOS", "DDLANGUAGE", "DOMVALUE_L", "DOMVALUE_H"],
                        options=[{"TEXT": f"DOMNAME = '{domain}'"}],
                        max_rows=100
                    )

                    # Buscar textos dos valores
                    value_texts = await self.rfc_client.read_table(
                        "DD07T",
                        fields=["DOMNAME", "VALPOS", "DDLANGUAGE", "DDTEXT"],
                        options=[{"TEXT": f"DOMNAME = '{domain}' AND DDLANGUAGE IN ('P', 'E')"}],
                        max_rows=200
                    )

                    # Montar mapa de textos
                    text_map: Dict[str, str] = {}
                    for vt in value_texts:
                        key = f"{vt.get('VALPOS', '')}"
                        lang = vt.get("DDLANGUAGE", "")
                        if lang == "P" or key not in text_map:
                            text_map[key] = vt.get("DDTEXT", "")

                    # Processar valores
                    for val in values:
                        val_pos = val.get("VALPOS", "")
                        result["fixed_values"].append({
                            "position": val_pos,
                            "low_value": val.get("DOMVALUE_L", ""),
                            "high_value": val.get("DOMVALUE_H", ""),
                            "description": text_map.get(val_pos, "")
                        })

            # Buscar descricao na DD01T
            texts = await self.rfc_client.read_table(
                "DD01T",
                fields=["DOMNAME", "DDLANGUAGE", "DDTEXT"],
                options=[{"TEXT": f"DOMNAME = '{domain}' AND DDLANGUAGE IN ('P', 'E')"}],
                max_rows=2
            )

            for text in texts:
                if text.get("DDLANGUAGE", "") == "P" or not result["description"]:
                    result["description"] = text.get("DDTEXT", "")

        except Exception as e:
            logger.warning(f"Erro ao obter info do domain {domain}: {e}")

        return result

    async def analyze_table_complete(
        self,
        table_name: str
    ) -> Dict[str, Any]:
        """
        Analise completa de uma tabela incluindo:
        - Estrutura basica
        - Campos com data elements e domains
        - Foreign keys
        - Indices
        - Estatisticas

        Args:
            table_name: Nome da tabela

        Returns:
            Dicionario com analise completa
        """
        table_name = table_name.upper()

        # Obter estrutura basica
        structure = await self.analyze_table(table_name)

        # Obter foreign keys
        relations = await self.get_foreign_key_relationships(table_name)
        structure.relations = relations

        # Enriquecer campos com info de data element e domain
        enriched_fields = []
        for field in structure.fields:
            field_info = field.to_dict()

            # Obter info do data element
            if field.data_element:
                de_info = await self.get_data_element_info(field.data_element)
                field_info["data_element_info"] = de_info

                # Obter info do domain
                if de_info.get("domain"):
                    dom_info = await self.get_domain_info(de_info["domain"])
                    field_info["domain_info"] = dom_info

            enriched_fields.append(field_info)

        # Montar resultado completo
        result = structure.to_dict()
        result["fields"] = enriched_fields
        result["relations"] = [
            {
                "foreign_table": r.foreign_table,
                "fields_mapping": r.fields_mapping,
                "cardinality": r.cardinality
            }
            for r in relations
        ]

        return result

    async def get_table_technical_settings(
        self,
        table_name: str
    ) -> Dict[str, Any]:
        """
        Obtem configuracoes tecnicas da tabela.

        Args:
            table_name: Nome da tabela

        Returns:
            Dicionario com configuracoes tecnicas
        """
        table_name = table_name.upper()
        result = {
            "name": table_name,
            "data_class": "",
            "size_category": "",
            "buffering_type": "",
            "buffering_allowed": False,
            "logging_enabled": False,
            "change_document": False
        }

        try:
            # Buscar na DD09L (configuracoes tecnicas)
            tech_data = await self.rfc_client.read_table(
                "DD09L",
                fields=["TABNAME", "TABKAT", "TABART", "PUFFERUNG", "PROTOKOLL",
                        "UEBESSION", "BUFALLOW"],
                options=[{"TEXT": f"TABNAME = '{table_name}'"}],
                max_rows=1
            )

            if tech_data:
                tech = tech_data[0]
                result["data_class"] = tech.get("TABKAT", "")
                result["size_category"] = tech.get("TABART", "")
                result["buffering_type"] = tech.get("PUFFERUNG", "")
                result["buffering_allowed"] = tech.get("BUFALLOW", "") == "X"
                result["logging_enabled"] = tech.get("PROTOKOLL", "") == "X"

        except Exception as e:
            logger.warning(f"Erro ao obter settings de {table_name}: {e}")

        return result

    async def search_tables_by_content(
        self,
        search_term: str,
        area: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Busca tabelas por termo na descricao ou nome.

        Args:
            search_term: Termo de busca
            area: Area/Modulo SAP (MM, SD, FI, etc.)
            max_results: Maximo de resultados

        Returns:
            Lista de tabelas encontradas
        """
        results = []
        search_term = search_term.upper()

        try:
            # Buscar por nome da tabela
            by_name = await self.rfc_client.read_table(
                "DD02L",
                fields=["TABNAME", "TABCLASS", "CONTFLAG"],
                options=[{"TEXT": f"TABNAME LIKE '%{search_term}%'"}],
                max_rows=max_results
            )

            table_names = [t["TABNAME"] for t in by_name]

            # Buscar por descricao
            by_desc = await self.rfc_client.read_table(
                "DD02T",
                fields=["TABNAME", "DDTEXT"],
                options=[{"TEXT": f"DDTEXT LIKE '%{search_term}%' AND DDLANGUAGE = 'P'"}],
                max_rows=max_results
            )

            # Adicionar tabelas encontradas por descricao
            for t in by_desc:
                if t["TABNAME"] not in table_names:
                    table_names.append(t["TABNAME"])

            # Obter descricoes
            descriptions = await self._get_table_descriptions(table_names[:max_results])

            # Montar resultado
            for t in by_name[:max_results]:
                tab_name = t["TABNAME"]
                results.append({
                    "name": tab_name,
                    "description": descriptions.get(tab_name, ""),
                    "category": t.get("TABCLASS", ""),
                    "delivery_class": t.get("CONTFLAG", ""),
                    "match_type": "name"
                })

            for t in by_desc:
                tab_name = t["TABNAME"]
                if not any(r["name"] == tab_name for r in results):
                    results.append({
                        "name": tab_name,
                        "description": t.get("DDTEXT", ""),
                        "match_type": "description"
                    })

        except Exception as e:
            logger.warning(f"Erro ao buscar tabelas por conteudo: {e}")

        return results[:max_results]

    async def get_table_dependencies(
        self,
        table_name: str
    ) -> Dict[str, Any]:
        """
        Obtem todas as dependencias de uma tabela.

        Args:
            table_name: Nome da tabela

        Returns:
            Dicionario com dependencias (views, includes, programs, etc.)
        """
        table_name = table_name.upper()
        result = {
            "table": table_name,
            "views_using": [],
            "programs_using": [],
            "function_modules_using": [],
            "includes_from": [],
            "check_tables": [],
            "dependent_tables": []
        }

        try:
            # Buscar views que usam a tabela
            views = await self.rfc_client.read_table(
                "DD26S",
                fields=["VIEWNAME", "TABNAME", "TABPOS"],
                options=[{"TEXT": f"TABNAME = '{table_name}'"}],
                max_rows=50
            )
            result["views_using"] = list(set([v["VIEWNAME"] for v in views]))

            # Buscar includes (estruturas usadas pela tabela)
            includes = await self.rfc_client.read_table(
                "DD03L",
                fields=["TABNAME", "FIELDNAME", "PRECFIELD"],
                options=[{"TEXT": f"TABNAME = '{table_name}' AND FIELDNAME LIKE '.INCLUDE%'"}],
                max_rows=20
            )
            result["includes_from"] = [i.get("PRECFIELD", "") for i in includes if i.get("PRECFIELD")]

            # Obter tabelas relacionadas
            related = await self.get_related_tables(table_name)
            result["check_tables"] = related.get("check_tables", [])
            result["dependent_tables"] = related.get("dependent_tables", [])

        except Exception as e:
            logger.warning(f"Erro ao obter dependencias de {table_name}: {e}")

        return result

    async def compare_table_structures(
        self,
        table1: str,
        table2: str
    ) -> Dict[str, Any]:
        """
        Compara estrutura de duas tabelas.

        Args:
            table1: Nome da primeira tabela
            table2: Nome da segunda tabela

        Returns:
            Dicionario com diferencas encontradas
        """
        struct1 = await self.analyze_table(table1)
        struct2 = await self.analyze_table(table2)

        fields1 = {f.name: f for f in struct1.fields}
        fields2 = {f.name: f for f in struct2.fields}

        result = {
            "table1": table1,
            "table2": table2,
            "fields_only_in_table1": [],
            "fields_only_in_table2": [],
            "fields_with_differences": [],
            "common_fields": []
        }

        # Campos apenas na tabela 1
        for name, field in fields1.items():
            if name not in fields2:
                result["fields_only_in_table1"].append(field.to_dict())
            else:
                # Comparar campos em comum
                field2 = fields2[name]
                differences = []

                if field.data_type != field2.data_type:
                    differences.append(f"data_type: {field.data_type} vs {field2.data_type}")
                if field.length != field2.length:
                    differences.append(f"length: {field.length} vs {field2.length}")
                if field.is_key != field2.is_key:
                    differences.append(f"is_key: {field.is_key} vs {field2.is_key}")

                if differences:
                    result["fields_with_differences"].append({
                        "field": name,
                        "differences": differences
                    })
                else:
                    result["common_fields"].append(name)

        # Campos apenas na tabela 2
        for name, field in fields2.items():
            if name not in fields1:
                result["fields_only_in_table2"].append(field.to_dict())

        return result
