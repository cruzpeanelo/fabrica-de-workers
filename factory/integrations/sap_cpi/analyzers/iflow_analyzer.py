# -*- coding: utf-8 -*-
"""
SAP CPI iFlow Analyzer
======================

Analisador de Integration Flows (iFlows) do SAP CPI.

Funcionalidades:
- Extração de estrutura do iFlow
- Identificação de adaptadores (sender/receiver)
- Análise de steps de processamento
- Extração de mapeamentos e scripts
- Identificação de configurações externalizadas
- Validação de boas práticas
- Detecção de erros potenciais

O iFlow é armazenado em formato BPMN2 (Business Process Model and Notation).
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


class StepType(str, Enum):
    """Tipos de steps em um iFlow"""
    START = "Start"
    END = "End"
    SENDER = "Sender"
    RECEIVER = "Receiver"
    REQUEST_REPLY = "RequestReply"
    CONTENT_MODIFIER = "ContentModifier"
    MAPPING = "Mapping"
    MESSAGE_MAPPING = "MessageMapping"
    XSLT_MAPPING = "XSLTMapping"
    SCRIPT = "Script"
    GROOVY = "Groovy"
    JAVASCRIPT = "JavaScript"
    ROUTER = "Router"
    SPLITTER = "Splitter"
    AGGREGATOR = "Aggregator"
    FILTER = "Filter"
    EXCEPTION_SUBPROCESS = "ExceptionSubprocess"
    PROCESS_CALL = "ProcessCall"
    LOCAL_INTEGRATION = "LocalIntegration"
    PERSISTENCE = "Persistence"
    ENCRYPTOR = "Encryptor"
    DECRYPTOR = "Decryptor"
    SIGNER = "Signer"
    VERIFIER = "Verifier"
    XML_VALIDATOR = "XMLValidator"
    JSON_VALIDATOR = "JSONValidator"
    CONVERTER = "Converter"
    ENCODER = "Encoder"
    DECODER = "Decoder"
    UNKNOWN = "Unknown"


@dataclass
class IFlowStep:
    """Representa um step dentro de um iFlow"""
    id: str
    name: str
    step_type: StepType
    adapter_type: str = ""
    properties: Dict[str, str] = field(default_factory=dict)
    script_path: str = ""
    mapping_path: str = ""
    position: int = 0
    incoming: List[str] = field(default_factory=list)
    outgoing: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "name": self.name,
            "step_type": self.step_type.value,
            "adapter_type": self.adapter_type,
            "properties": self.properties,
            "script_path": self.script_path,
            "mapping_path": self.mapping_path,
            "position": self.position,
        }


@dataclass
class IFlowConnection:
    """Representa uma conexão entre steps"""
    id: str
    source_id: str
    target_id: str
    condition: str = ""

    def to_dict(self) -> Dict[str, str]:
        return {
            "id": self.id,
            "source_id": self.source_id,
            "target_id": self.target_id,
            "condition": self.condition,
        }


@dataclass
class IFlowAnalysis:
    """Resultado da análise de um iFlow"""
    iflow_id: str
    iflow_name: str
    description: str = ""
    sender_adapter: str = ""
    receiver_adapters: List[str] = field(default_factory=list)
    steps: List[IFlowStep] = field(default_factory=list)
    connections: List[IFlowConnection] = field(default_factory=list)
    scripts: List[str] = field(default_factory=list)
    mappings: List[str] = field(default_factory=list)
    externalized_params: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    best_practices_violations: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "iflow_id": self.iflow_id,
            "iflow_name": self.iflow_name,
            "description": self.description,
            "sender_adapter": self.sender_adapter,
            "receiver_adapters": self.receiver_adapters,
            "steps": [s.to_dict() for s in self.steps],
            "connections": [c.to_dict() for c in self.connections],
            "scripts": self.scripts,
            "mappings": self.mappings,
            "externalized_params": self.externalized_params,
            "warnings": self.warnings,
            "errors": self.errors,
            "best_practices_violations": self.best_practices_violations,
            "total_steps": len(self.steps),
            "has_errors": len(self.errors) > 0,
        }

    def get_summary(self) -> str:
        """Retorna resumo textual da análise"""
        lines = [
            f"iFlow: {self.iflow_name} ({self.iflow_id})",
            f"Descrição: {self.description or 'N/A'}",
            f"Adaptador de entrada: {self.sender_adapter or 'N/A'}",
            f"Adaptadores de saída: {', '.join(self.receiver_adapters) or 'N/A'}",
            f"Total de steps: {len(self.steps)}",
            f"Scripts: {len(self.scripts)}",
            f"Mapeamentos: {len(self.mappings)}",
            f"Parâmetros externalizados: {len(self.externalized_params)}",
        ]

        if self.errors:
            lines.append(f"\nErros ({len(self.errors)}):")
            for error in self.errors:
                lines.append(f"  - {error}")

        if self.warnings:
            lines.append(f"\nAvisos ({len(self.warnings)}):")
            for warning in self.warnings:
                lines.append(f"  - {warning}")

        if self.best_practices_violations:
            lines.append(f"\nViolações de boas práticas ({len(self.best_practices_violations)}):")
            for violation in self.best_practices_violations:
                lines.append(f"  - {violation}")

        return "\n".join(lines)


class IFlowAnalyzer:
    """
    Analisador de Integration Flows do SAP CPI.

    Exemplo:
    ```python
    analyzer = IFlowAnalyzer()

    # Analisar a partir de conteúdo ZIP
    with open("iflow.zip", "rb") as f:
        content = f.read()

    analysis = analyzer.analyze_from_zip(content)
    print(analysis.get_summary())

    # Verificar problemas
    if analysis.errors:
        print("Erros encontrados!")
        for error in analysis.errors:
            print(f"  - {error}")
    ```
    """

    # Namespaces BPMN2 e SAP CPI
    NAMESPACES = {
        "bpmn2": "http://www.omg.org/spec/BPMN/20100524/MODEL",
        "ifl": "http:///com.sap.ifl.model/Ifl.xsd",
        "di": "http://www.omg.org/spec/DD/20100524/DI",
        "bpmndi": "http://www.omg.org/spec/BPMN/20100524/DI",
        "dc": "http://www.omg.org/spec/DD/20100524/DC",
    }

    # Mapeamento de elementos BPMN2 para tipos de step
    ELEMENT_TO_STEP_TYPE = {
        "startEvent": StepType.START,
        "endEvent": StepType.END,
        "serviceTask": StepType.REQUEST_REPLY,
        "scriptTask": StepType.SCRIPT,
        "callActivity": StepType.PROCESS_CALL,
        "subProcess": StepType.EXCEPTION_SUBPROCESS,
        "exclusiveGateway": StepType.ROUTER,
        "parallelGateway": StepType.SPLITTER,
        "complexGateway": StepType.AGGREGATOR,
        "participant": StepType.SENDER,  # ou RECEIVER dependendo do contexto
    }

    def __init__(self):
        """Inicializa o analisador"""
        pass

    def analyze_from_zip(
        self,
        zip_content: bytes,
        validate_best_practices: bool = True
    ) -> IFlowAnalysis:
        """
        Analisa um iFlow a partir do conteúdo ZIP.

        Args:
            zip_content: Conteúdo do arquivo ZIP em bytes
            validate_best_practices: Verificar violações de boas práticas

        Returns:
            IFlowAnalysis com resultado da análise
        """
        analysis = IFlowAnalysis(iflow_id="", iflow_name="")

        try:
            with zipfile.ZipFile(io.BytesIO(zip_content)) as zf:
                # Lista arquivos no ZIP
                file_list = zf.namelist()
                logger.debug(f"Arquivos no ZIP: {file_list}")

                # Encontra o arquivo .iflw (BPMN2)
                iflw_files = [f for f in file_list if f.endswith('.iflw')]
                if not iflw_files:
                    analysis.errors.append("Arquivo .iflw não encontrado no ZIP")
                    return analysis

                # Analisa o arquivo BPMN2 principal
                iflw_path = iflw_files[0]
                with zf.open(iflw_path) as iflw_file:
                    iflw_content = iflw_file.read()
                    self._analyze_bpmn2(iflw_content, analysis)

                # Lista scripts Groovy
                for f in file_list:
                    if f.endswith('.groovy'):
                        analysis.scripts.append(f)
                    elif f.endswith('.mmap') or f.endswith('.mapping'):
                        analysis.mappings.append(f)

                # Valida boas práticas
                if validate_best_practices:
                    self._validate_best_practices(analysis)

        except zipfile.BadZipFile:
            analysis.errors.append("Conteúdo não é um arquivo ZIP válido")
        except Exception as e:
            analysis.errors.append(f"Erro ao analisar ZIP: {str(e)}")
            logger.error(f"Erro ao analisar iFlow: {e}")

        return analysis

    def analyze_from_bpmn2(
        self,
        bpmn2_content: bytes,
        validate_best_practices: bool = True
    ) -> IFlowAnalysis:
        """
        Analisa um iFlow a partir do conteúdo BPMN2 XML.

        Args:
            bpmn2_content: Conteúdo XML BPMN2 em bytes
            validate_best_practices: Verificar violações de boas práticas

        Returns:
            IFlowAnalysis com resultado da análise
        """
        analysis = IFlowAnalysis(iflow_id="", iflow_name="")

        self._analyze_bpmn2(bpmn2_content, analysis)

        if validate_best_practices:
            self._validate_best_practices(analysis)

        return analysis

    def _analyze_bpmn2(
        self,
        content: bytes,
        analysis: IFlowAnalysis
    ) -> None:
        """
        Analisa o conteúdo BPMN2 XML.

        Args:
            content: Conteúdo XML em bytes
            analysis: Objeto IFlowAnalysis para preencher
        """
        try:
            # Parse XML
            root = ET.fromstring(content)

            # Registra namespaces
            for prefix, uri in self.NAMESPACES.items():
                ET.register_namespace(prefix, uri)

            # Extrai informações básicas
            self._extract_basic_info(root, analysis)

            # Extrai participants (adaptadores)
            self._extract_participants(root, analysis)

            # Extrai process (steps e conexões)
            self._extract_process(root, analysis)

            # Extrai parâmetros externalizados
            self._extract_externalized_params(root, analysis)

        except ET.ParseError as e:
            analysis.errors.append(f"Erro de parsing XML: {str(e)}")
        except Exception as e:
            analysis.errors.append(f"Erro ao analisar BPMN2: {str(e)}")
            logger.error(f"Erro ao analisar BPMN2: {e}")

    def _extract_basic_info(
        self,
        root: ET.Element,
        analysis: IFlowAnalysis
    ) -> None:
        """Extrai informações básicas do iFlow"""
        # Tenta diferentes formas de obter ID e nome
        analysis.iflow_id = root.get("id", "")
        analysis.iflow_name = root.get("name", analysis.iflow_id)

        # Busca descrição em documentação BPMN2
        for doc in root.iter():
            if "documentation" in doc.tag.lower():
                analysis.description = doc.text or ""
                break

    def _extract_participants(
        self,
        root: ET.Element,
        analysis: IFlowAnalysis
    ) -> None:
        """Extrai participantes (adaptadores sender/receiver)"""
        receivers = []

        for elem in root.iter():
            tag_name = elem.tag.split("}")[-1] if "}" in elem.tag else elem.tag

            if tag_name == "participant":
                name = elem.get("name", "")
                participant_id = elem.get("id", "")

                # Identifica tipo de participante
                ifl_type = elem.get("{http:///com.sap.ifl.model/Ifl.xsd}type", "")

                if "Sender" in name or "sender" in participant_id.lower():
                    # Busca adaptador do sender
                    adapter = self._extract_adapter_type(elem)
                    analysis.sender_adapter = adapter
                elif "Receiver" in name or "receiver" in participant_id.lower():
                    adapter = self._extract_adapter_type(elem)
                    receivers.append(adapter)

        analysis.receiver_adapters = receivers

    def _extract_adapter_type(self, elem: ET.Element) -> str:
        """Extrai o tipo de adaptador de um participante"""
        # Busca em propriedades do elemento
        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag

            if "property" in tag_name.lower():
                key = child.get("key", "")
                value = child.get("value", "")

                if key == "ComponentType" or key == "AdapterType":
                    return value

        # Tenta inferir do nome
        name = elem.get("name", "")
        for adapter in ["HTTP", "HTTPS", "SOAP", "SFTP", "OData", "RFC", "IDoc", "AMQP", "Kafka", "Mail"]:
            if adapter.lower() in name.lower():
                return adapter

        return "Unknown"

    def _extract_process(
        self,
        root: ET.Element,
        analysis: IFlowAnalysis
    ) -> None:
        """Extrai steps e conexões do processo"""
        position = 0

        for elem in root.iter():
            tag_name = elem.tag.split("}")[-1] if "}" in elem.tag else elem.tag

            # Identifica steps
            step_type = self.ELEMENT_TO_STEP_TYPE.get(tag_name)

            if step_type:
                step = self._create_step_from_element(elem, step_type, position)
                analysis.steps.append(step)
                position += 1

            # Identifica serviceTask com tipos específicos
            elif tag_name == "serviceTask":
                step = self._analyze_service_task(elem, position)
                analysis.steps.append(step)
                position += 1

            # Identifica conexões (sequenceFlow)
            elif tag_name == "sequenceFlow":
                connection = IFlowConnection(
                    id=elem.get("id", ""),
                    source_id=elem.get("sourceRef", ""),
                    target_id=elem.get("targetRef", ""),
                    condition=self._extract_condition(elem)
                )
                analysis.connections.append(connection)

    def _create_step_from_element(
        self,
        elem: ET.Element,
        step_type: StepType,
        position: int
    ) -> IFlowStep:
        """Cria um IFlowStep a partir de um elemento XML"""
        properties = {}

        # Extrai propriedades do elemento
        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag
            if "property" in tag_name.lower():
                key = child.get("key", child.get("name", ""))
                value = child.get("value", child.text or "")
                if key:
                    properties[key] = value

        return IFlowStep(
            id=elem.get("id", ""),
            name=elem.get("name", ""),
            step_type=step_type,
            properties=properties,
            position=position
        )

    def _analyze_service_task(
        self,
        elem: ET.Element,
        position: int
    ) -> IFlowStep:
        """Analisa um serviceTask para identificar o tipo específico"""
        name = elem.get("name", "")
        step_id = elem.get("id", "")

        # Identifica tipo baseado em propriedades
        activity_type = ""
        properties = {}

        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag
            if "property" in tag_name.lower():
                key = child.get("key", child.get("name", ""))
                value = child.get("value", child.text or "")
                if key:
                    properties[key] = value
                    if key == "activityType":
                        activity_type = value

        # Mapeia activityType para StepType
        type_mapping = {
            "Mapping": StepType.MAPPING,
            "MessageMapping": StepType.MESSAGE_MAPPING,
            "XSLTMapping": StepType.XSLT_MAPPING,
            "Script": StepType.SCRIPT,
            "Groovy": StepType.GROOVY,
            "JavaScript": StepType.JAVASCRIPT,
            "ContentModifier": StepType.CONTENT_MODIFIER,
            "Router": StepType.ROUTER,
            "Splitter": StepType.SPLITTER,
            "Aggregator": StepType.AGGREGATOR,
            "Filter": StepType.FILTER,
            "Encoder": StepType.ENCODER,
            "Decoder": StepType.DECODER,
            "Converter": StepType.CONVERTER,
            "XMLValidator": StepType.XML_VALIDATOR,
            "Encryptor": StepType.ENCRYPTOR,
            "Decryptor": StepType.DECRYPTOR,
            "Signer": StepType.SIGNER,
            "Persistence": StepType.PERSISTENCE,
        }

        step_type = type_mapping.get(activity_type, StepType.REQUEST_REPLY)

        # Extrai caminho de script ou mapping
        script_path = properties.get("script", properties.get("scriptBundleName", ""))
        mapping_path = properties.get("mappingPath", properties.get("resource", ""))

        return IFlowStep(
            id=step_id,
            name=name,
            step_type=step_type,
            properties=properties,
            script_path=script_path,
            mapping_path=mapping_path,
            position=position
        )

    def _extract_condition(self, elem: ET.Element) -> str:
        """Extrai condição de um sequenceFlow"""
        for child in elem:
            tag_name = child.tag.split("}")[-1] if "}" in child.tag else child.tag
            if "conditionExpression" in tag_name.lower():
                return child.text or ""
        return ""

    def _extract_externalized_params(
        self,
        root: ET.Element,
        analysis: IFlowAnalysis
    ) -> None:
        """Extrai parâmetros externalizados ({{param}})"""
        # Busca em todos os valores de propriedade
        pattern = r'\{\{([^}]+)\}\}'

        for elem in root.iter():
            for attr_value in elem.attrib.values():
                matches = re.findall(pattern, str(attr_value))
                for match in matches:
                    if match not in analysis.externalized_params:
                        analysis.externalized_params.append(match)

            # Verifica também texto do elemento
            if elem.text:
                matches = re.findall(pattern, elem.text)
                for match in matches:
                    if match not in analysis.externalized_params:
                        analysis.externalized_params.append(match)

    def _validate_best_practices(self, analysis: IFlowAnalysis) -> None:
        """Valida boas práticas e adiciona avisos"""

        # 1. Verifica se há muitos steps (complexidade)
        if len(analysis.steps) > 20:
            analysis.best_practices_violations.append(
                f"iFlow muito complexo ({len(analysis.steps)} steps). "
                "Considere dividir em sub-processos."
            )

        # 2. Verifica se há tratamento de exceção
        has_exception_handling = any(
            s.step_type == StepType.EXCEPTION_SUBPROCESS
            for s in analysis.steps
        )
        if not has_exception_handling:
            analysis.warnings.append(
                "Sem tratamento de exceção definido. "
                "Adicione um Exception Subprocess para tratar erros."
            )

        # 3. Verifica parâmetros não externalizados (hardcoded)
        for step in analysis.steps:
            for key, value in step.properties.items():
                if key.lower() in ["url", "endpoint", "address", "host"]:
                    if "{{" not in value and value and "://" in value:
                        analysis.best_practices_violations.append(
                            f"Endpoint hardcoded em {step.name}: {key}={value}. "
                            "Externalize o parâmetro."
                        )

        # 4. Verifica credenciais hardcoded
        sensitive_keys = ["password", "secret", "token", "key", "apikey", "credential"]
        for step in analysis.steps:
            for key, value in step.properties.items():
                if any(s in key.lower() for s in sensitive_keys):
                    if "{{" not in value and value:
                        analysis.best_practices_violations.append(
                            f"Credencial potencialmente hardcoded em {step.name}: {key}. "
                            "Use Security Artifacts para credenciais."
                        )

        # 5. Verifica se há logging excessivo
        content_modifiers = [s for s in analysis.steps if s.step_type == StepType.CONTENT_MODIFIER]
        if len(content_modifiers) > 5:
            analysis.warnings.append(
                f"Muitos Content Modifiers ({len(content_modifiers)}). "
                "Verifique se todos são necessários."
            )

        # 6. Verifica mapeamentos sem fallback
        if analysis.mappings and not any(
            s.step_type in [StepType.EXCEPTION_SUBPROCESS, StepType.FILTER]
            for s in analysis.steps
        ):
            analysis.warnings.append(
                "Mapeamentos sem validação de entrada. "
                "Considere adicionar validação XML/JSON antes do mapeamento."
            )

    def get_flow_diagram(self, analysis: IFlowAnalysis) -> str:
        """
        Gera representação ASCII do fluxo.

        Args:
            analysis: Análise do iFlow

        Returns:
            String com diagrama ASCII
        """
        if not analysis.steps:
            return "Nenhum step encontrado"

        lines = []
        lines.append("=" * 60)
        lines.append(f" iFlow: {analysis.iflow_name}")
        lines.append("=" * 60)
        lines.append("")

        # Ordena steps por posição
        sorted_steps = sorted(analysis.steps, key=lambda s: s.position)

        for i, step in enumerate(sorted_steps):
            step_icon = self._get_step_icon(step.step_type)
            lines.append(f"  {step_icon} [{step.step_type.value}] {step.name}")

            if step.adapter_type:
                lines.append(f"      Adaptador: {step.adapter_type}")

            if step.script_path:
                lines.append(f"      Script: {step.script_path}")

            if step.mapping_path:
                lines.append(f"      Mapping: {step.mapping_path}")

            if i < len(sorted_steps) - 1:
                lines.append("      |")
                lines.append("      v")

        lines.append("")
        lines.append("=" * 60)

        return "\n".join(lines)

    def _get_step_icon(self, step_type: StepType) -> str:
        """Retorna ícone ASCII para tipo de step"""
        icons = {
            StepType.START: "(o)",
            StepType.END: "(x)",
            StepType.SENDER: "[S]",
            StepType.RECEIVER: "[R]",
            StepType.REQUEST_REPLY: "[<>]",
            StepType.CONTENT_MODIFIER: "[CM]",
            StepType.MAPPING: "[M]",
            StepType.SCRIPT: "[SC]",
            StepType.GROOVY: "[GR]",
            StepType.ROUTER: "[/\\]",
            StepType.SPLITTER: "[||]",
            StepType.AGGREGATOR: "[><]",
            StepType.FILTER: "[F]",
            StepType.EXCEPTION_SUBPROCESS: "[!]",
        }
        return icons.get(step_type, "[?]")
