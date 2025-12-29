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
