# -*- coding: utf-8 -*-
"""
SAP CPI iFlow Generator
=======================

Gerador de Integration Flows (iFlows) para SAP CPI.

Funcionalidades:
- Geração de iFlows em formato BPMN2
- Suporte a diversos adaptadores (HTTP, SOAP, SFTP, etc)
- Geração de fluxos de processamento
- Configuração de mapeamentos e scripts
- Criação de exception handling
- Empacotamento em ZIP para deploy

O formato BPMN2 é o padrão usado pelo SAP CPI para definir
fluxos de integração.
"""

import logging
import uuid
import zipfile
import io
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from enum import Enum
from datetime import datetime

logger = logging.getLogger(__name__)


class AdapterType(str, Enum):
    """Tipos de adaptadores disponíveis"""
    HTTP = "HTTP"
    HTTPS = "HTTPS"
    SOAP = "SOAP"
    SFTP = "SFTP"
    FTP = "FTP"
    MAIL = "Mail"
    ODATA = "OData"
    RFC = "RFC"
    IDOC = "IDoc"
    JDBC = "JDBC"
    AMQP = "AMQP"
    KAFKA = "Kafka"
    SUCCESS_FACTORS = "SuccessFactors"
    ARIBA = "Ariba"
    AS2 = "AS2"


class StepType(str, Enum):
    """Tipos de steps de processamento"""
    CONTENT_MODIFIER = "ContentModifier"
    MESSAGE_MAPPING = "MessageMapping"
    XSLT_MAPPING = "XSLTMapping"
    GROOVY_SCRIPT = "GroovyScript"
    JAVASCRIPT = "JavaScript"
    ROUTER = "Router"
    SPLITTER = "Splitter"
    AGGREGATOR = "Aggregator"
    FILTER = "Filter"
    REQUEST_REPLY = "RequestReply"
    SEND = "Send"
    ENCODER = "Encoder"
    DECODER = "Decoder"
    ENCRYPTOR = "Encryptor"
    DECRYPTOR = "Decryptor"
    XML_VALIDATOR = "XMLValidator"
    JSON_TO_XML = "JSONToXML"
    XML_TO_JSON = "XMLToJSON"
    PERSIST = "Persist"


@dataclass
class AdapterConfig:
    """Configuração de um adaptador"""
    adapter_type: AdapterType
    address: str = ""
    authentication_type: str = "None"  # None, Basic, OAuth, Certificate
    credential_name: str = ""
    additional_properties: Dict[str, str] = field(default_factory=dict)

    def to_properties(self) -> Dict[str, str]:
        """Converte para propriedades do iFlow"""
        props = {
            "ComponentType": self.adapter_type.value,
            "Address": self.address,
            "AuthenticationType": self.authentication_type,
        }

        if self.credential_name:
            props["CredentialName"] = self.credential_name

        props.update(self.additional_properties)
        return props


@dataclass
class ProcessStep:
    """Configuração de um step de processamento"""
    step_type: StepType
    name: str
    description: str = ""
    properties: Dict[str, str] = field(default_factory=dict)
    script_path: str = ""
    mapping_path: str = ""
    condition: str = ""


@dataclass
class IFlowDefinition:
    """Definição completa de um iFlow"""
    id: str
    name: str
    description: str = ""
    sender_adapter: Optional[AdapterConfig] = None
    receiver_adapter: Optional[AdapterConfig] = None
    processing_steps: List[ProcessStep] = field(default_factory=list)
    exception_handling: bool = True
    logging_enabled: bool = True
    externalized_params: Dict[str, str] = field(default_factory=dict)

    def validate(self) -> List[str]:
        """Valida a definição e retorna lista de erros"""
        errors = []

        if not self.id:
            errors.append("ID do iFlow é obrigatório")

        if not self.name:
            errors.append("Nome do iFlow é obrigatório")

        if not self.sender_adapter:
            errors.append("Adaptador de entrada (sender) é obrigatório")

        # Valida ID (apenas alfanuméricos e underscore)
        import re
        if not re.match(r'^[A-Za-z][A-Za-z0-9_]*$', self.id):
            errors.append("ID deve começar com letra e conter apenas letras, números e underscore")

        return errors


class IFlowGenerator:
    """
    Gerador de Integration Flows para SAP CPI.

    Exemplo:
    ```python
    generator = IFlowGenerator()

    # Define o iFlow
    definition = IFlowDefinition(
        id="Invoice_Replication",
        name="Invoice Replication to SAP",
        description="Replica faturas do sistema X para SAP ERP",
        sender_adapter=AdapterConfig(
            adapter_type=AdapterType.HTTP,
            address="/invoice/replicate",
            authentication_type="Basic"
        ),
        receiver_adapter=AdapterConfig(
            adapter_type=AdapterType.RFC,
            address="BAPI_INVOICE_CREATE",
            authentication_type="Certificate",
            credential_name="SAP_ERP_CERT"
        ),
        processing_steps=[
            ProcessStep(
                step_type=StepType.MESSAGE_MAPPING,
                name="Map to BAPI Format",
                mapping_path="mapping/invoice_mapping.mmap"
            ),
            ProcessStep(
                step_type=StepType.GROOVY_SCRIPT,
                name="Validate Invoice",
                script_path="script/validate_invoice.groovy"
            )
        ],
        externalized_params={
            "target_system": "SAP_PROD",
            "max_retries": "3"
        }
    )

    # Gera ZIP para deploy
    zip_content = generator.generate(definition)

    # Salva para arquivo
    with open("invoice_iflow.zip", "wb") as f:
        f.write(zip_content)
    ```
    """

    # Template BPMN2 base
    BPMN2_TEMPLATE = '''<?xml version="1.0" encoding="UTF-8"?>
<bpmn2:definitions xmlns:bpmn2="http://www.omg.org/spec/BPMN/20100524/MODEL"
    xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
    xmlns:dc="http://www.omg.org/spec/DD/20100524/DC"
    xmlns:di="http://www.omg.org/spec/DD/20100524/DI"
    xmlns:ifl="http:///com.sap.ifl.model/Ifl.xsd"
    id="Definitions_{def_id}"
    name="{iflow_name}"
    targetNamespace="http://www.sap.com/ifl">
    <bpmn2:collaboration id="Collaboration_{def_id}">
        <bpmn2:documentation>{description}</bpmn2:documentation>
        {participants}
    </bpmn2:collaboration>
    {processes}
    {bpmndi}
</bpmn2:definitions>'''

    # Template de participante (sender/receiver)
    PARTICIPANT_TEMPLATE = '''
        <bpmn2:participant id="{participant_id}" name="{name}" ifl:type="{participant_type}">
            {properties}
        </bpmn2:participant>'''

    # Template de processo de integração
    PROCESS_TEMPLATE = '''
    <bpmn2:process id="{process_id}" name="{name}" isExecutable="true">
        {start_event}
        {steps}
        {end_event}
        {sequence_flows}
    </bpmn2:process>'''

    def __init__(self):
        """Inicializa o gerador"""
        pass

    def generate(
        self,
        definition: IFlowDefinition,
        include_scripts: Dict[str, str] = None,
        include_mappings: Dict[str, bytes] = None
    ) -> bytes:
        """
        Gera um iFlow empacotado em ZIP.

        Args:
            definition: Definição do iFlow
            include_scripts: Dicionário {caminho: conteúdo} de scripts
            include_mappings: Dicionário {caminho: conteúdo} de mapeamentos

        Returns:
            bytes com conteúdo do ZIP
        """
        # Valida definição
        errors = definition.validate()
        if errors:
            raise ValueError(f"Definição inválida: {', '.join(errors)}")

        # Gera BPMN2
        bpmn2_content = self._generate_bpmn2(definition)

        # Cria ZIP
        zip_buffer = io.BytesIO()

        with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zf:
            # MANIFEST
            manifest = self._generate_manifest(definition)
            zf.writestr("META-INF/MANIFEST.MF", manifest)

            # BPMN2 principal
            iflw_path = f"src/main/resources/{definition.id}.iflw"
            zf.writestr(iflw_path, bpmn2_content)

            # Parameters externalizados
            if definition.externalized_params:
                params = self._generate_parameters(definition.externalized_params)
                zf.writestr("src/main/resources/parameters.prop", params)

            # Scripts incluídos
            if include_scripts:
                for path, content in include_scripts.items():
                    script_path = f"src/main/resources/script/{path}"
                    zf.writestr(script_path, content)

            # Mapeamentos incluídos
            if include_mappings:
                for path, content in include_mappings.items():
                    mapping_path = f"src/main/resources/mapping/{path}"
                    zf.writestr(mapping_path, content)

        zip_buffer.seek(0)
        return zip_buffer.getvalue()

    def generate_bpmn2(self, definition: IFlowDefinition) -> str:
        """
        Gera apenas o conteúdo BPMN2 XML.

        Args:
            definition: Definição do iFlow

        Returns:
            String com XML BPMN2
        """
        return self._generate_bpmn2(definition)

    def _generate_bpmn2(self, definition: IFlowDefinition) -> str:
        """Gera conteúdo BPMN2 XML"""
        def_id = self._generate_id()

        # Gera participantes
        participants = self._generate_participants(definition, def_id)

        # Gera processo
        processes = self._generate_process(definition, def_id)

        # Gera BPMNDI (diagrama visual)
        bpmndi = self._generate_bpmndi(definition, def_id)

        # Monta BPMN2 final
        bpmn2 = self.BPMN2_TEMPLATE.format(
            def_id=def_id,
            iflow_name=definition.name,
            description=self._escape_xml(definition.description),
            participants=participants,
            processes=processes,
            bpmndi=bpmndi
        )

        return bpmn2

    def _generate_participants(self, definition: IFlowDefinition, def_id: str) -> str:
        """Gera participantes (sender e receivers)"""
        participants = []

        # Sender
        if definition.sender_adapter:
            sender_props = self._generate_adapter_properties(definition.sender_adapter)
            sender = self.PARTICIPANT_TEMPLATE.format(
                participant_id=f"Participant_Sender_{def_id}",
                name="Sender",
                participant_type="EndpointSender",
                properties=sender_props
            )
            participants.append(sender)

        # Integration Process (participante especial)
        integration_process = self.PARTICIPANT_TEMPLATE.format(
            participant_id=f"Participant_Process_{def_id}",
            name="Integration Process",
            participant_type="IntegrationProcess",
            properties=""
        )
        participants.append(integration_process)

        # Receiver
        if definition.receiver_adapter:
            receiver_props = self._generate_adapter_properties(definition.receiver_adapter)
            receiver = self.PARTICIPANT_TEMPLATE.format(
                participant_id=f"Participant_Receiver_{def_id}",
                name="Receiver",
                participant_type="EndpointReceiver",
                properties=receiver_props
            )
            participants.append(receiver)

        return "\n".join(participants)

    def _generate_adapter_properties(self, adapter: AdapterConfig) -> str:
        """Gera propriedades XML de um adaptador"""
        props = adapter.to_properties()
        prop_lines = []

        for key, value in props.items():
            prop_lines.append(
                f'            <ifl:property key="{key}" value="{self._escape_xml(value)}"/>'
            )

        return "\n".join(prop_lines)

    def _generate_process(self, definition: IFlowDefinition, def_id: str) -> str:
        """Gera o processo de integração"""
        process_id = f"Process_{def_id}"

        # Start Event
        start_event = f'''
        <bpmn2:startEvent id="StartEvent_{def_id}" name="Start">
            <bpmn2:outgoing>Flow_Start_{def_id}</bpmn2:outgoing>
        </bpmn2:startEvent>'''

        # Processing Steps
        steps = []
        step_ids = []

        for i, step in enumerate(definition.processing_steps):
            step_id = f"Step_{i}_{def_id}"
            step_ids.append(step_id)

            step_xml = self._generate_step(step, step_id, def_id)
            steps.append(step_xml)

        # End Event
        end_event = f'''
        <bpmn2:endEvent id="EndEvent_{def_id}" name="End">
            <bpmn2:incoming>Flow_End_{def_id}</bpmn2:incoming>
        </bpmn2:endEvent>'''

        # Sequence Flows (conexões entre steps)
        sequence_flows = self._generate_sequence_flows(step_ids, def_id)

        # Exception Handling subprocess (se habilitado)
        if definition.exception_handling:
            exception_subprocess = self._generate_exception_subprocess(def_id)
            steps.append(exception_subprocess)

        return self.PROCESS_TEMPLATE.format(
            process_id=process_id,
            name=definition.name,
            start_event=start_event,
            steps="\n".join(steps),
            end_event=end_event,
            sequence_flows=sequence_flows
        )

    def _generate_step(self, step: ProcessStep, step_id: str, def_id: str) -> str:
        """Gera XML de um step de processamento"""
        # Mapeia tipo de step para elemento BPMN2
        step_mapping = {
            StepType.CONTENT_MODIFIER: "serviceTask",
            StepType.MESSAGE_MAPPING: "serviceTask",
            StepType.XSLT_MAPPING: "serviceTask",
            StepType.GROOVY_SCRIPT: "scriptTask",
            StepType.JAVASCRIPT: "scriptTask",
            StepType.ROUTER: "exclusiveGateway",
            StepType.SPLITTER: "parallelGateway",
            StepType.REQUEST_REPLY: "serviceTask",
            StepType.SEND: "serviceTask",
        }

        element_type = step_mapping.get(step.step_type, "serviceTask")

        # Propriedades do step
        props = [f'            <ifl:property key="activityType" value="{step.step_type.value}"/>']

        if step.script_path:
            props.append(f'            <ifl:property key="script" value="{step.script_path}"/>')

        if step.mapping_path:
            props.append(f'            <ifl:property key="mappingPath" value="{step.mapping_path}"/>')

        for key, value in step.properties.items():
            props.append(f'            <ifl:property key="{key}" value="{self._escape_xml(value)}"/>')

        properties = "\n".join(props)

        return f'''
        <bpmn2:{element_type} id="{step_id}" name="{self._escape_xml(step.name)}">
            <bpmn2:documentation>{self._escape_xml(step.description)}</bpmn2:documentation>
{properties}
        </bpmn2:{element_type}>'''

    def _generate_sequence_flows(self, step_ids: List[str], def_id: str) -> str:
        """Gera conexões (sequence flows) entre steps"""
        flows = []

        # Start -> primeiro step (ou end se não há steps)
        if step_ids:
            flows.append(f'''
        <bpmn2:sequenceFlow id="Flow_Start_{def_id}" sourceRef="StartEvent_{def_id}" targetRef="{step_ids[0]}"/>''')

            # Conexões entre steps
            for i in range(len(step_ids) - 1):
                flows.append(f'''
        <bpmn2:sequenceFlow id="Flow_{i}_{def_id}" sourceRef="{step_ids[i]}" targetRef="{step_ids[i+1]}"/>''')

            # Último step -> end
            flows.append(f'''
        <bpmn2:sequenceFlow id="Flow_End_{def_id}" sourceRef="{step_ids[-1]}" targetRef="EndEvent_{def_id}"/>''')
        else:
            flows.append(f'''
        <bpmn2:sequenceFlow id="Flow_Direct_{def_id}" sourceRef="StartEvent_{def_id}" targetRef="EndEvent_{def_id}"/>''')

        return "\n".join(flows)

    def _generate_exception_subprocess(self, def_id: str) -> str:
        """Gera subprocess de tratamento de exceções"""
        return f'''
        <bpmn2:subProcess id="ExceptionSubprocess_{def_id}" name="Exception Handling" triggeredByEvent="true">
            <bpmn2:startEvent id="ExceptionStart_{def_id}" name="Error Start">
                <bpmn2:errorEventDefinition/>
            </bpmn2:startEvent>
            <bpmn2:serviceTask id="LogError_{def_id}" name="Log Error">
                <ifl:property key="activityType" value="ContentModifier"/>
            </bpmn2:serviceTask>
            <bpmn2:endEvent id="ExceptionEnd_{def_id}" name="Error End">
                <bpmn2:errorEventDefinition/>
            </bpmn2:endEvent>
            <bpmn2:sequenceFlow id="ExFlow1_{def_id}" sourceRef="ExceptionStart_{def_id}" targetRef="LogError_{def_id}"/>
            <bpmn2:sequenceFlow id="ExFlow2_{def_id}" sourceRef="LogError_{def_id}" targetRef="ExceptionEnd_{def_id}"/>
        </bpmn2:subProcess>'''

    def _generate_bpmndi(self, definition: IFlowDefinition, def_id: str) -> str:
        """Gera BPMNDI (informações visuais do diagrama)"""
        # Simplificado - apenas para satisfazer o parser CPI
        return f'''
    <bpmndi:BPMNDiagram id="BPMNDiagram_{def_id}" name="{definition.name}">
        <bpmndi:BPMNPlane id="BPMNPlane_{def_id}" bpmnElement="Collaboration_{def_id}">
        </bpmndi:BPMNPlane>
    </bpmndi:BPMNDiagram>'''

    def _generate_manifest(self, definition: IFlowDefinition) -> str:
        """Gera conteúdo do MANIFEST.MF"""
        return f'''Manifest-Version: 1.0
Bundle-ManifestVersion: 2
Bundle-Name: {definition.name}
Bundle-SymbolicName: {definition.id}
Bundle-Version: 1.0.0
SAP-BundleType: IntegrationFlow
SAP-NodeType: IFLMAP
Import-Package: com.sap.gateway.ip.core.customdev.util
SAP-RuntimeProfile: iflmap
'''

    def _generate_parameters(self, params: Dict[str, str]) -> str:
        """Gera arquivo de parâmetros externalizados"""
        lines = [
            "# SAP CPI Externalized Parameters",
            f"# Generated: {datetime.utcnow().isoformat()}",
            ""
        ]

        for key, value in params.items():
            lines.append(f"{key}={value}")

        return "\n".join(lines)

    def _generate_id(self) -> str:
        """Gera ID único"""
        return uuid.uuid4().hex[:12]

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

    # Templates prontos para cenários comuns
    @staticmethod
    def create_http_to_http_iflow(
        iflow_id: str,
        name: str,
        source_path: str,
        target_url: str,
        description: str = ""
    ) -> IFlowDefinition:
        """
        Cria definição de iFlow HTTP para HTTP simples.

        Args:
            iflow_id: ID do iFlow
            name: Nome do iFlow
            source_path: Path do endpoint de entrada
            target_url: URL do sistema destino
            description: Descrição

        Returns:
            IFlowDefinition pronta para gerar
        """
        return IFlowDefinition(
            id=iflow_id,
            name=name,
            description=description,
            sender_adapter=AdapterConfig(
                adapter_type=AdapterType.HTTPS,
                address=source_path,
                authentication_type="Basic"
            ),
            receiver_adapter=AdapterConfig(
                adapter_type=AdapterType.HTTP,
                address=target_url,
                authentication_type="None"
            ),
            processing_steps=[
                ProcessStep(
                    step_type=StepType.CONTENT_MODIFIER,
                    name="Set Headers",
                    properties={
                        "Content-Type": "application/json"
                    }
                )
            ],
            externalized_params={
                "target_url": target_url
            }
        )

    @staticmethod
    def create_sftp_to_sap_iflow(
        iflow_id: str,
        name: str,
        sftp_directory: str,
        file_pattern: str,
        rfc_function: str,
        description: str = ""
    ) -> IFlowDefinition:
        """
        Cria definição de iFlow SFTP para SAP (RFC).

        Args:
            iflow_id: ID do iFlow
            name: Nome do iFlow
            sftp_directory: Diretório SFTP para polling
            file_pattern: Padrão de arquivo (ex: *.xml)
            rfc_function: Nome da função RFC/BAPI
            description: Descrição

        Returns:
            IFlowDefinition pronta para gerar
        """
        return IFlowDefinition(
            id=iflow_id,
            name=name,
            description=description,
            sender_adapter=AdapterConfig(
                adapter_type=AdapterType.SFTP,
                address=sftp_directory,
                authentication_type="Certificate",
                additional_properties={
                    "fileName": file_pattern,
                    "pollInterval": "5",
                    "pollIntervalUnit": "minutes"
                }
            ),
            receiver_adapter=AdapterConfig(
                adapter_type=AdapterType.RFC,
                address=rfc_function,
                authentication_type="Certificate"
            ),
            processing_steps=[
                ProcessStep(
                    step_type=StepType.XML_VALIDATOR,
                    name="Validate XML"
                ),
                ProcessStep(
                    step_type=StepType.MESSAGE_MAPPING,
                    name="Map to RFC Format"
                )
            ],
            externalized_params={
                "sftp_directory": sftp_directory,
                "rfc_function": rfc_function
            }
        )
