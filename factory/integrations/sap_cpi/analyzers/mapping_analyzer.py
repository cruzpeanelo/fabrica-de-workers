# -*- coding: utf-8 -*-
"""
SAP CPI Mapping Analyzer
========================

Analisador de Message Mappings do SAP CPI.

Funcionalidades:
- Análise de estrutura de mapeamentos
- Identificação de campos fonte e destino
- Extração de funções de transformação
- Detecção de campos não mapeados
- Validação de tipos de dados
- Análise de condições e filtros
"""

import logging
import re
import zipfile
import io
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple
from enum import Enum
import xml.etree.ElementTree as ET

logger = logging.getLogger(__name__)


class MappingFieldType(str, Enum):
    """Tipos de campos em mapeamentos"""
    SOURCE = "source"
    TARGET = "target"
    CONSTANT = "constant"
    VARIABLE = "variable"
    PARAMETER = "parameter"


class TransformationType(str, Enum):
    """Tipos de transformação"""
    DIRECT = "Direct"
    CONCAT = "Concat"
    SUBSTRING = "Substring"
    REPLACE = "Replace"
    IF_ELSE = "IfElse"
    FORMAT_DATE = "FormatDate"
    FORMAT_NUMBER = "FormatNumber"
    LOOKUP = "Lookup"
    CONVERT_TYPE = "ConvertType"
    MATH = "Math"
    CUSTOM_FUNCTION = "CustomFunction"


@dataclass
class MappingField:
    """Representa um campo em um mapeamento"""
    name: str
    path: str
    field_type: MappingFieldType
    data_type: str = "string"
    is_required: bool = False
    is_list: bool = False
    default_value: str = ""
    description: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "path": self.path,
            "field_type": self.field_type.value,
            "data_type": self.data_type,
            "is_required": self.is_required,
            "is_list": self.is_list,
            "default_value": self.default_value,
            "description": self.description,
        }


@dataclass
class MappingRule:
    """Representa uma regra de mapeamento"""
    id: str
    source_fields: List[MappingField] = field(default_factory=list)
    target_field: Optional[MappingField] = None
    transformation: TransformationType = TransformationType.DIRECT
    transformation_params: Dict[str, str] = field(default_factory=dict)
    condition: str = ""
    description: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "source_fields": [f.to_dict() for f in self.source_fields],
            "target_field": self.target_field.to_dict() if self.target_field else None,
            "transformation": self.transformation.value,
            "transformation_params": self.transformation_params,
            "condition": self.condition,
            "description": self.description,
        }


@dataclass
class MappingAnalysis:
    """Resultado da análise de um mapeamento"""
    mapping_id: str
    mapping_name: str
    source_structure: str = ""
    target_structure: str = ""
    source_fields: List[MappingField] = field(default_factory=list)
    target_fields: List[MappingField] = field(default_factory=list)
    rules: List[MappingRule] = field(default_factory=list)
    unmapped_source_fields: List[str] = field(default_factory=list)
    unmapped_target_fields: List[str] = field(default_factory=list)
    custom_functions: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "mapping_id": self.mapping_id,
            "mapping_name": self.mapping_name,
            "source_structure": self.source_structure,
            "target_structure": self.target_structure,
            "source_fields": [f.to_dict() for f in self.source_fields],
            "target_fields": [f.to_dict() for f in self.target_fields],
            "rules": [r.to_dict() for r in self.rules],
            "unmapped_source_fields": self.unmapped_source_fields,
            "unmapped_target_fields": self.unmapped_target_fields,
            "custom_functions": self.custom_functions,
            "warnings": self.warnings,
            "errors": self.errors,
            "total_rules": len(self.rules),
            "coverage": self._calculate_coverage(),
        }

    def _calculate_coverage(self) -> float:
        """Calcula cobertura do mapeamento"""
        total_target = len(self.target_fields)
        if total_target == 0:
            return 100.0

        mapped = total_target - len(self.unmapped_target_fields)
        return round((mapped / total_target) * 100, 2)

    def get_summary(self) -> str:
        """Retorna resumo textual da análise"""
        lines = [
            f"Mapping: {self.mapping_name} ({self.mapping_id})",
            f"Estrutura fonte: {self.source_structure or 'N/A'}",
            f"Estrutura destino: {self.target_structure or 'N/A'}",
            f"Campos fonte: {len(self.source_fields)}",
            f"Campos destino: {len(self.target_fields)}",
            f"Regras de mapeamento: {len(self.rules)}",
            f"Cobertura: {self._calculate_coverage()}%",
        ]

        if self.unmapped_source_fields:
            lines.append(f"\nCampos fonte não utilizados ({len(self.unmapped_source_fields)}):")
            for field_name in self.unmapped_source_fields[:10]:
                lines.append(f"  - {field_name}")
            if len(self.unmapped_source_fields) > 10:
                lines.append(f"  ... e mais {len(self.unmapped_source_fields) - 10}")

        if self.unmapped_target_fields:
            lines.append(f"\nCampos destino não mapeados ({len(self.unmapped_target_fields)}):")
            for field_name in self.unmapped_target_fields[:10]:
                lines.append(f"  - {field_name}")
            if len(self.unmapped_target_fields) > 10:
                lines.append(f"  ... e mais {len(self.unmapped_target_fields) - 10}")

        if self.errors:
            lines.append(f"\nErros ({len(self.errors)}):")
            for error in self.errors:
                lines.append(f"  - {error}")

        if self.warnings:
            lines.append(f"\nAvisos ({len(self.warnings)}):")
            for warning in self.warnings:
                lines.append(f"  - {warning}")

        return "\n".join(lines)


class MappingAnalyzer:
    """
    Analisador de Message Mappings do SAP CPI.

    Os mapeamentos SAP CPI usam formato XML proprietário (.mmap)
    que define transformações entre estruturas de dados.

    Exemplo:
    ```python
    analyzer = MappingAnalyzer()

    # Analisar arquivo de mapeamento
    with open("mapping.mmap", "rb") as f:
        content = f.read()

    analysis = analyzer.analyze(content)
    print(analysis.get_summary())

    # Verificar campos não mapeados
    for field in analysis.unmapped_target_fields:
        print(f"Campo obrigatório não mapeado: {field}")
    ```
    """

    # Namespaces comuns em mapeamentos SAP
    NAMESPACES = {
        "mapping": "http://www.sap.com/xi/mapping",
        "mm": "http://www.sap.com/xi/mapping/model",
        "xsd": "http://www.w3.org/2001/XMLSchema",
    }

    def __init__(self):
        """Inicializa o analisador"""
        pass

    def analyze(
        self,
        content: bytes,
        validate: bool = True
    ) -> MappingAnalysis:
        """
        Analisa conteúdo de um arquivo de mapeamento.

        Args:
            content: Conteúdo do arquivo em bytes
            validate: Realizar validações

        Returns:
            MappingAnalysis com resultado
        """
        analysis = MappingAnalysis(mapping_id="", mapping_name="")

        try:
            # Tenta parsear como XML
            root = ET.fromstring(content)
            self._analyze_xml_mapping(root, analysis)

        except ET.ParseError:
            # Pode ser um formato diferente
            try:
                # Tenta como JSON
                import json
                data = json.loads(content)
                self._analyze_json_mapping(data, analysis)
            except json.JSONDecodeError:
                analysis.errors.append("Formato de mapeamento não reconhecido")

        except Exception as e:
            analysis.errors.append(f"Erro ao analisar mapeamento: {str(e)}")
            logger.error(f"Erro ao analisar mapping: {e}")

        if validate:
            self._validate_mapping(analysis)

        return analysis

    def analyze_from_zip(
        self,
        zip_content: bytes,
        mapping_path: Optional[str] = None
    ) -> List[MappingAnalysis]:
        """
        Analisa mapeamentos de um arquivo ZIP de iFlow.

        Args:
            zip_content: Conteúdo do ZIP em bytes
            mapping_path: Caminho específico do mapeamento (opcional)

        Returns:
            Lista de MappingAnalysis
        """
        analyses = []

        try:
            with zipfile.ZipFile(io.BytesIO(zip_content)) as zf:
                # Encontra arquivos de mapeamento
                mapping_files = [
                    f for f in zf.namelist()
                    if f.endswith('.mmap') or f.endswith('.mapping') or 'mapping' in f.lower()
                ]

                if mapping_path:
                    mapping_files = [f for f in mapping_files if mapping_path in f]

                for mapping_file in mapping_files:
                    try:
                        with zf.open(mapping_file) as mf:
                            content = mf.read()
                            analysis = self.analyze(content)
                            analysis.mapping_id = mapping_file
                            analyses.append(analysis)
                    except Exception as e:
                        logger.warning(f"Erro ao analisar {mapping_file}: {e}")

        except zipfile.BadZipFile:
            logger.error("Conteúdo não é um arquivo ZIP válido")
        except Exception as e:
            logger.error(f"Erro ao processar ZIP: {e}")

        return analyses

    def _analyze_xml_mapping(
        self,
        root: ET.Element,
        analysis: MappingAnalysis
    ) -> None:
        """Analisa mapeamento em formato XML"""

        # Extrai informações básicas
        analysis.mapping_id = root.get("id", root.get("name", ""))
        analysis.mapping_name = root.get("name", root.get("id", ""))

        # Busca estruturas fonte e destino
        for elem in root.iter():
            tag_name = elem.tag.split("}")[-1] if "}" in elem.tag else elem.tag

            if tag_name in ["sourceMessage", "source", "sourceStructure"]:
                analysis.source_structure = elem.get("name", elem.get("type", ""))
                self._extract_fields_from_element(elem, analysis.source_fields, MappingFieldType.SOURCE)

            elif tag_name in ["targetMessage", "target", "targetStructure"]:
                analysis.target_structure = elem.get("name", elem.get("type", ""))
                self._extract_fields_from_element(elem, analysis.target_fields, MappingFieldType.TARGET)

            elif tag_name in ["mapping", "rule", "transformation"]:
                rule = self._extract_mapping_rule(elem)
                if rule:
                    analysis.rules.append(rule)

            elif tag_name in ["function", "customFunction"]:
                func_name = elem.get("name", "")
                if func_name:
                    analysis.custom_functions.append(func_name)

        # Identifica campos não mapeados
        self._identify_unmapped_fields(analysis)

    def _analyze_json_mapping(
        self,
        data: Dict[str, Any],
        analysis: MappingAnalysis
    ) -> None:
        """Analisa mapeamento em formato JSON"""

        analysis.mapping_id = data.get("id", data.get("name", ""))
        analysis.mapping_name = data.get("name", data.get("id", ""))

        # Extrai estruturas
        if "source" in data:
            source = data["source"]
            analysis.source_structure = source.get("name", source.get("type", ""))

            if "fields" in source:
                for field_data in source["fields"]:
                    field = self._field_from_dict(field_data, MappingFieldType.SOURCE)
                    analysis.source_fields.append(field)

        if "target" in data:
            target = data["target"]
            analysis.target_structure = target.get("name", target.get("type", ""))

            if "fields" in target:
                for field_data in target["fields"]:
                    field = self._field_from_dict(field_data, MappingFieldType.TARGET)
                    analysis.target_fields.append(field)

        # Extrai regras
        if "mappings" in data or "rules" in data:
            rules_data = data.get("mappings", data.get("rules", []))
            for rule_data in rules_data:
                rule = self._rule_from_dict(rule_data)
                if rule:
                    analysis.rules.append(rule)

        self._identify_unmapped_fields(analysis)

    def _extract_fields_from_element(
        self,
        elem: ET.Element,
        fields: List[MappingField],
        field_type: MappingFieldType,
        parent_path: str = ""
    ) -> None:
        """Extrai campos de um elemento XML recursivamente"""

        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag

            if tag_name in ["field", "element", "attribute", "node"]:
                name = child.get("name", "")
                path = f"{parent_path}/{name}" if parent_path else name

                field = MappingField(
                    name=name,
                    path=path,
                    field_type=field_type,
                    data_type=child.get("type", child.get("dataType", "string")),
                    is_required=child.get("required", "false").lower() == "true",
                    is_list=child.get("maxOccurs", "1") != "1" or child.get("isList", "false").lower() == "true",
                    description=child.get("description", ""),
                )
                fields.append(field)

                # Processa filhos (estruturas aninhadas)
                self._extract_fields_from_element(child, fields, field_type, path)

    def _extract_mapping_rule(self, elem: ET.Element) -> Optional[MappingRule]:
        """Extrai regra de mapeamento de um elemento XML"""

        rule_id = elem.get("id", elem.get("name", ""))
        if not rule_id:
            return None

        rule = MappingRule(id=rule_id)

        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag

            if tag_name in ["sourceField", "source", "input"]:
                field = MappingField(
                    name=child.get("name", ""),
                    path=child.get("path", child.get("name", "")),
                    field_type=MappingFieldType.SOURCE,
                )
                rule.source_fields.append(field)

            elif tag_name in ["targetField", "target", "output"]:
                rule.target_field = MappingField(
                    name=child.get("name", ""),
                    path=child.get("path", child.get("name", "")),
                    field_type=MappingFieldType.TARGET,
                )

            elif tag_name in ["function", "transformation"]:
                func_name = child.get("name", child.get("type", ""))
                rule.transformation = self._get_transformation_type(func_name)
                rule.transformation_params = dict(child.attrib)

            elif tag_name == "condition":
                rule.condition = child.text or child.get("expression", "")

        return rule

    def _get_transformation_type(self, func_name: str) -> TransformationType:
        """Mapeia nome de função para tipo de transformação"""
        mapping = {
            "concat": TransformationType.CONCAT,
            "concatenate": TransformationType.CONCAT,
            "substring": TransformationType.SUBSTRING,
            "replace": TransformationType.REPLACE,
            "if": TransformationType.IF_ELSE,
            "ifelse": TransformationType.IF_ELSE,
            "formatdate": TransformationType.FORMAT_DATE,
            "formatnumber": TransformationType.FORMAT_NUMBER,
            "lookup": TransformationType.LOOKUP,
            "valuemapping": TransformationType.LOOKUP,
            "convert": TransformationType.CONVERT_TYPE,
            "cast": TransformationType.CONVERT_TYPE,
        }

        func_lower = func_name.lower()
        for key, value in mapping.items():
            if key in func_lower:
                return value

        if func_name:
            return TransformationType.CUSTOM_FUNCTION

        return TransformationType.DIRECT

    def _field_from_dict(
        self,
        data: Dict[str, Any],
        field_type: MappingFieldType
    ) -> MappingField:
        """Cria MappingField a partir de dicionário"""
        return MappingField(
            name=data.get("name", ""),
            path=data.get("path", data.get("name", "")),
            field_type=field_type,
            data_type=data.get("type", data.get("dataType", "string")),
            is_required=data.get("required", False),
            is_list=data.get("isList", data.get("isArray", False)),
            default_value=data.get("default", ""),
            description=data.get("description", ""),
        )

    def _rule_from_dict(self, data: Dict[str, Any]) -> Optional[MappingRule]:
        """Cria MappingRule a partir de dicionário"""
        rule_id = data.get("id", data.get("name", ""))
        if not rule_id:
            return None

        rule = MappingRule(id=rule_id)

        # Campos fonte
        sources = data.get("sources", data.get("source", []))
        if isinstance(sources, dict):
            sources = [sources]
        for src in sources:
            if isinstance(src, str):
                field = MappingField(name=src, path=src, field_type=MappingFieldType.SOURCE)
            else:
                field = self._field_from_dict(src, MappingFieldType.SOURCE)
            rule.source_fields.append(field)

        # Campo destino
        target = data.get("target", data.get("output", {}))
        if isinstance(target, str):
            rule.target_field = MappingField(name=target, path=target, field_type=MappingFieldType.TARGET)
        elif target:
            rule.target_field = self._field_from_dict(target, MappingFieldType.TARGET)

        # Transformação
        func = data.get("function", data.get("transformation", ""))
        if func:
            rule.transformation = self._get_transformation_type(func)
            rule.transformation_params = data.get("params", data.get("parameters", {}))

        rule.condition = data.get("condition", "")
        rule.description = data.get("description", "")

        return rule

    def _identify_unmapped_fields(self, analysis: MappingAnalysis) -> None:
        """Identifica campos fonte e destino não mapeados"""

        # Campos fonte utilizados em regras
        used_sources = set()
        for rule in analysis.rules:
            for field in rule.source_fields:
                used_sources.add(field.path)

        # Campos destino mapeados em regras
        mapped_targets = set()
        for rule in analysis.rules:
            if rule.target_field:
                mapped_targets.add(rule.target_field.path)

        # Identifica não utilizados
        for field in analysis.source_fields:
            if field.path not in used_sources:
                analysis.unmapped_source_fields.append(field.path)

        # Identifica não mapeados
        for field in analysis.target_fields:
            if field.path not in mapped_targets:
                analysis.unmapped_target_fields.append(field.path)

    def _validate_mapping(self, analysis: MappingAnalysis) -> None:
        """Valida o mapeamento e adiciona avisos/erros"""

        # Verifica campos obrigatórios não mapeados
        for field in analysis.target_fields:
            if field.is_required and field.path in analysis.unmapped_target_fields:
                analysis.errors.append(
                    f"Campo obrigatório '{field.name}' não está mapeado"
                )

        # Verifica transformações de tipo potencialmente problemáticas
        for rule in analysis.rules:
            if rule.target_field and rule.source_fields:
                source_type = rule.source_fields[0].data_type if rule.source_fields else "string"
                target_type = rule.target_field.data_type

                if source_type != target_type and rule.transformation == TransformationType.DIRECT:
                    analysis.warnings.append(
                        f"Conversão implícita de tipo em '{rule.target_field.name}': "
                        f"{source_type} -> {target_type}"
                    )

        # Verifica cobertura
        coverage = analysis._calculate_coverage()
        if coverage < 50:
            analysis.warnings.append(
                f"Baixa cobertura de mapeamento ({coverage}%). "
                "Verifique se todos os campos destino necessários estão mapeados."
            )

    def generate_mapping_matrix(self, analysis: MappingAnalysis) -> str:
        """
        Gera matriz de mapeamento em formato texto.

        Args:
            analysis: Análise do mapeamento

        Returns:
            String com matriz de mapeamento
        """
        lines = []
        lines.append("=" * 80)
        lines.append(f" MATRIZ DE MAPEAMENTO: {analysis.mapping_name}")
        lines.append("=" * 80)
        lines.append("")
        lines.append(f"{'FONTE':<35} -> {'DESTINO':<35} {'TRANSFORMACAO':<15}")
        lines.append("-" * 80)

        for rule in analysis.rules:
            sources = ", ".join([f.name for f in rule.source_fields]) or "[constante]"
            target = rule.target_field.name if rule.target_field else "[N/A]"
            transform = rule.transformation.value

            # Trunca se necessário
            sources = sources[:33] + ".." if len(sources) > 35 else sources
            target = target[:33] + ".." if len(target) > 35 else target

            lines.append(f"{sources:<35} -> {target:<35} {transform:<15}")

            if rule.condition:
                lines.append(f"{'':>35}    Condição: {rule.condition[:40]}")

        lines.append("")
        lines.append("-" * 80)
        lines.append(f"Total de regras: {len(analysis.rules)}")
        lines.append(f"Cobertura: {analysis._calculate_coverage()}%")

        if analysis.unmapped_target_fields:
            lines.append(f"\nCampos destino não mapeados: {len(analysis.unmapped_target_fields)}")

        return "\n".join(lines)
