# -*- coding: utf-8 -*-
"""
SAP CPI Mapping Generator
=========================

Gerador de Message Mappings para SAP CPI.

Funcionalidades:
- Geração de mapeamentos XML
- Mapeamento direto de campos
- Funções de transformação
- Condições e filtros
- Mapeamento de estruturas complexas
- Validação de tipos de dados
"""

import logging
import uuid
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from enum import Enum
from datetime import datetime

logger = logging.getLogger(__name__)


class DataType(str, Enum):
    """Tipos de dados suportados"""
    STRING = "string"
    INTEGER = "integer"
    DECIMAL = "decimal"
    BOOLEAN = "boolean"
    DATE = "date"
    DATETIME = "datetime"
    TIME = "time"
    BINARY = "binary"


class TransformFunction(str, Enum):
    """Funções de transformação disponíveis"""
    DIRECT = "direct"  # Mapeamento direto 1:1
    CONCAT = "concat"  # Concatenação
    SUBSTRING = "substring"  # Substring
    REPLACE = "replace"  # Substituição
    UPPER = "upper"  # Maiúsculas
    LOWER = "lower"  # Minúsculas
    TRIM = "trim"  # Remove espaços
    FORMAT_DATE = "formatDate"  # Formata data
    FORMAT_NUMBER = "formatNumber"  # Formata número
    CONVERT_TYPE = "convertType"  # Conversão de tipo
    IF_ELSE = "ifElse"  # Condicional
    CONSTANT = "constant"  # Valor constante
    LOOKUP = "lookup"  # Value Mapping lookup
    CUSTOM = "custom"  # Função customizada


@dataclass
class FieldDefinition:
    """Definição de um campo"""
    name: str
    path: str
    data_type: DataType = DataType.STRING
    is_required: bool = False
    is_list: bool = False
    min_occurs: int = 0
    max_occurs: int = 1
    description: str = ""

    def to_xml(self) -> str:
        """Converte para XML"""
        attrs = [
            f'name="{self.name}"',
            f'path="{self.path}"',
            f'type="{self.data_type.value}"',
            f'minOccurs="{self.min_occurs}"',
            f'maxOccurs="{self.max_occurs if not self.is_list else "unbounded"}"',
        ]
        return f'<field {" ".join(attrs)}/>'


@dataclass
class MappingRule:
    """Regra de mapeamento"""
    source_fields: List[str]  # Paths dos campos fonte
    target_field: str  # Path do campo destino
    function: TransformFunction = TransformFunction.DIRECT
    function_params: Dict[str, str] = field(default_factory=dict)
    condition: str = ""
    description: str = ""

    def to_xml(self) -> str:
        """Converte para XML"""
        rule_id = uuid.uuid4().hex[:8]

        sources_xml = "\n".join([
            f'        <source path="{src}"/>' for src in self.source_fields
        ])

        params_xml = ""
        if self.function_params:
            params_xml = "\n" + "\n".join([
                f'        <param name="{k}" value="{v}"/>'
                for k, v in self.function_params.items()
            ])

        condition_xml = ""
        if self.condition:
            condition_xml = f'\n        <condition>{self.condition}</condition>'

        return f'''    <mapping id="mapping_{rule_id}">
{sources_xml}
        <target path="{self.target_field}"/>
        <function name="{self.function.value}"/>{params_xml}{condition_xml}
    </mapping>'''


@dataclass
class StructureDefinition:
    """Definição de uma estrutura de mensagem"""
    name: str
    namespace: str = ""
    root_element: str = ""
    fields: List[FieldDefinition] = field(default_factory=list)

    def to_xml(self) -> str:
        """Converte para XML"""
        fields_xml = "\n        ".join([f.to_xml() for f in self.fields])

        return f'''    <structure name="{self.name}" namespace="{self.namespace}">
        <root element="{self.root_element or self.name}"/>
        {fields_xml}
    </structure>'''


@dataclass
class MappingDefinition:
    """Definição completa de um mapeamento"""
    id: str
    name: str
    description: str = ""
    source_structure: Optional[StructureDefinition] = None
    target_structure: Optional[StructureDefinition] = None
    rules: List[MappingRule] = field(default_factory=list)

    def validate(self) -> List[str]:
        """Valida a definição"""
        errors = []

        if not self.id:
            errors.append("ID do mapeamento é obrigatório")

        if not self.source_structure:
            errors.append("Estrutura fonte é obrigatória")

        if not self.target_structure:
            errors.append("Estrutura destino é obrigatória")

        if not self.rules:
            errors.append("Pelo menos uma regra de mapeamento é necessária")

        return errors


class MappingGenerator:
    """
    Gerador de Message Mappings para SAP CPI.

    Exemplo:
    ```python
    generator = MappingGenerator()

    # Define estrutura fonte
    source = StructureDefinition(
        name="SourceOrder",
        root_element="order",
        fields=[
            FieldDefinition("orderId", "/order/id", DataType.STRING, is_required=True),
            FieldDefinition("customerName", "/order/customer/name", DataType.STRING),
            FieldDefinition("orderDate", "/order/date", DataType.DATE),
            FieldDefinition("totalAmount", "/order/total", DataType.DECIMAL),
        ]
    )

    # Define estrutura destino
    target = StructureDefinition(
        name="SAPOrder",
        namespace="urn:sap:document:sap:rfc",
        root_element="BAPI_ORDER",
        fields=[
            FieldDefinition("VBELN", "/BAPI_ORDER/VBELN", DataType.STRING, is_required=True),
            FieldDefinition("KUNNR", "/BAPI_ORDER/KUNNR", DataType.STRING),
            FieldDefinition("ERDAT", "/BAPI_ORDER/ERDAT", DataType.DATE),
            FieldDefinition("NETWR", "/BAPI_ORDER/NETWR", DataType.DECIMAL),
        ]
    )

    # Define regras de mapeamento
    rules = [
        MappingRule(
            source_fields=["/order/id"],
            target_field="/BAPI_ORDER/VBELN",
            function=TransformFunction.DIRECT
        ),
        MappingRule(
            source_fields=["/order/customer/name"],
            target_field="/BAPI_ORDER/KUNNR",
            function=TransformFunction.UPPER
        ),
        MappingRule(
            source_fields=["/order/date"],
            target_field="/BAPI_ORDER/ERDAT",
            function=TransformFunction.FORMAT_DATE,
            function_params={"sourceFormat": "yyyy-MM-dd", "targetFormat": "yyyyMMdd"}
        ),
    ]

    # Cria definição
    definition = MappingDefinition(
        id="order_mapping",
        name="Order to SAP Mapping",
        description="Mapeia pedido externo para BAPI SAP",
        source_structure=source,
        target_structure=target,
        rules=rules
    )

    # Gera XML
    xml_content = generator.generate(definition)
    print(xml_content)
    ```
    """

    # Template XML do mapeamento
    MAPPING_TEMPLATE = '''<?xml version="1.0" encoding="UTF-8"?>
<messageMapping id="{mapping_id}" name="{name}" version="1.0"
    xmlns="http://www.sap.com/xi/mapping"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <description>{description}</description>
    <createdAt>{created_at}</createdAt>

    <!-- Estrutura Fonte -->
{source_structure}

    <!-- Estrutura Destino -->
{target_structure}

    <!-- Regras de Mapeamento -->
    <mappings>
{rules}
    </mappings>
</messageMapping>'''

    def __init__(self):
        """Inicializa o gerador"""
        pass

    def generate(self, definition: MappingDefinition) -> str:
        """
        Gera o XML do mapeamento.

        Args:
            definition: Definição do mapeamento

        Returns:
            String com XML do mapeamento
        """
        errors = definition.validate()
        if errors:
            raise ValueError(f"Definição inválida: {', '.join(errors)}")

        # Gera partes do XML
        source_xml = definition.source_structure.to_xml() if definition.source_structure else ""
        target_xml = definition.target_structure.to_xml() if definition.target_structure else ""
        rules_xml = "\n".join([rule.to_xml() for rule in definition.rules])

        return self.MAPPING_TEMPLATE.format(
            mapping_id=definition.id,
            name=self._escape_xml(definition.name),
            description=self._escape_xml(definition.description),
            created_at=datetime.utcnow().isoformat(),
            source_structure=source_xml,
            target_structure=target_xml,
            rules=rules_xml
        )

    def generate_from_schemas(
        self,
        mapping_id: str,
        name: str,
        source_fields: List[Dict[str, Any]],
        target_fields: List[Dict[str, Any]],
        auto_map: bool = True
    ) -> str:
        """
        Gera mapeamento a partir de listas de campos.

        Args:
            mapping_id: ID do mapeamento
            name: Nome do mapeamento
            source_fields: Lista de campos fonte [{name, path, type}]
            target_fields: Lista de campos destino [{name, path, type}]
            auto_map: Se True, tenta mapear campos automaticamente por nome

        Returns:
            String com XML do mapeamento
        """
        # Converte campos para FieldDefinition
        source_defs = []
        for f in source_fields:
            source_defs.append(FieldDefinition(
                name=f.get("name", ""),
                path=f.get("path", f.get("name", "")),
                data_type=DataType(f.get("type", "string")),
                is_required=f.get("required", False),
                is_list=f.get("is_list", False)
            ))

        target_defs = []
        for f in target_fields:
            target_defs.append(FieldDefinition(
                name=f.get("name", ""),
                path=f.get("path", f.get("name", "")),
                data_type=DataType(f.get("type", "string")),
                is_required=f.get("required", False),
                is_list=f.get("is_list", False)
            ))

        # Cria estruturas
        source_structure = StructureDefinition(
            name="SourceMessage",
            fields=source_defs
        )

        target_structure = StructureDefinition(
            name="TargetMessage",
            fields=target_defs
        )

        # Auto-mapeia se solicitado
        rules = []
        if auto_map:
            rules = self._auto_map_fields(source_defs, target_defs)

        definition = MappingDefinition(
            id=mapping_id,
            name=name,
            source_structure=source_structure,
            target_structure=target_structure,
            rules=rules
        )

        return self.generate(definition)

    def _auto_map_fields(
        self,
        source_fields: List[FieldDefinition],
        target_fields: List[FieldDefinition]
    ) -> List[MappingRule]:
        """
        Mapeia automaticamente campos com nomes similares.

        Args:
            source_fields: Campos fonte
            target_fields: Campos destino

        Returns:
            Lista de MappingRule
        """
        rules = []

        # Cria mapa de nomes normalizados
        source_map = {}
        for f in source_fields:
            normalized = f.name.lower().replace("_", "").replace("-", "")
            source_map[normalized] = f

        for target in target_fields:
            normalized = target.name.lower().replace("_", "").replace("-", "")

            if normalized in source_map:
                source = source_map[normalized]

                # Determina função baseado em tipos
                function = TransformFunction.DIRECT
                params = {}

                if source.data_type != target.data_type:
                    function = TransformFunction.CONVERT_TYPE
                    params["sourceType"] = source.data_type.value
                    params["targetType"] = target.data_type.value

                rules.append(MappingRule(
                    source_fields=[source.path],
                    target_field=target.path,
                    function=function,
                    function_params=params,
                    description=f"Auto-mapeado: {source.name} -> {target.name}"
                ))

        return rules

    def create_direct_mapping(
        self,
        source_path: str,
        target_path: str
    ) -> MappingRule:
        """
        Cria regra de mapeamento direto.

        Args:
            source_path: Path do campo fonte
            target_path: Path do campo destino

        Returns:
            MappingRule
        """
        return MappingRule(
            source_fields=[source_path],
            target_field=target_path,
            function=TransformFunction.DIRECT
        )

    def create_concat_mapping(
        self,
        source_paths: List[str],
        target_path: str,
        separator: str = " "
    ) -> MappingRule:
        """
        Cria regra de concatenação.

        Args:
            source_paths: Lista de paths fonte
            target_path: Path do campo destino
            separator: Separador entre valores

        Returns:
            MappingRule
        """
        return MappingRule(
            source_fields=source_paths,
            target_field=target_path,
            function=TransformFunction.CONCAT,
            function_params={"separator": separator}
        )

    def create_constant_mapping(
        self,
        target_path: str,
        constant_value: str
    ) -> MappingRule:
        """
        Cria regra com valor constante.

        Args:
            target_path: Path do campo destino
            constant_value: Valor constante

        Returns:
            MappingRule
        """
        return MappingRule(
            source_fields=[],
            target_field=target_path,
            function=TransformFunction.CONSTANT,
            function_params={"value": constant_value}
        )

    def create_conditional_mapping(
        self,
        source_path: str,
        target_path: str,
        condition: str,
        true_value: str = "",
        false_value: str = ""
    ) -> MappingRule:
        """
        Cria regra condicional.

        Args:
            source_path: Path do campo fonte
            target_path: Path do campo destino
            condition: Expressão de condição
            true_value: Valor se verdadeiro (opcional)
            false_value: Valor se falso (opcional)

        Returns:
            MappingRule
        """
        params = {}
        if true_value:
            params["trueValue"] = true_value
        if false_value:
            params["falseValue"] = false_value

        return MappingRule(
            source_fields=[source_path],
            target_field=target_path,
            function=TransformFunction.IF_ELSE,
            function_params=params,
            condition=condition
        )

    def create_date_mapping(
        self,
        source_path: str,
        target_path: str,
        source_format: str,
        target_format: str
    ) -> MappingRule:
        """
        Cria regra de formatação de data.

        Args:
            source_path: Path do campo fonte
            target_path: Path do campo destino
            source_format: Formato da data fonte
            target_format: Formato da data destino

        Returns:
            MappingRule
        """
        return MappingRule(
            source_fields=[source_path],
            target_field=target_path,
            function=TransformFunction.FORMAT_DATE,
            function_params={
                "sourceFormat": source_format,
                "targetFormat": target_format
            }
        )

    def _escape_xml(self, text: str) -> str:
        """Escapa caracteres especiais XML"""
        if not text:
            return ""
        return (
            text
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace('"', "&quot;")
            .replace("'", "&apos;")
        )
