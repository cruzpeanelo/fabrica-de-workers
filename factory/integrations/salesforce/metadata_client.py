# -*- coding: utf-8 -*-
"""
Salesforce Metadata API Client
==============================
Cliente para Metadata API do Salesforce.

A Metadata API permite:
- Ler e modificar configuracoes da organizacao
- Criar/atualizar objetos customizados
- Criar/atualizar campos
- Gerenciar profiles e permission sets
- Deploy de componentes (classes, triggers, pages, etc.)

Suporta isolamento multi-tenant atraves do tenant_id do SalesforceClient (Issue #314).

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient, MetadataClient, SalesforceConfig

    config = SalesforceConfig(
        tenant_id="TENANT-001",
        username="user@empresa.com",
        password="senha123",
        security_token="token"
    )
    sf_client = SalesforceClient(config)
    await sf_client.connect()

    metadata = MetadataClient(sf_client)

    # Listar objetos customizados
    objects = await metadata.list_metadata("CustomObject")

    # Ler metadata de um objeto
    obj = await metadata.read_metadata("CustomObject", ["Account", "Contact"])

    # Criar campo customizado
    field = metadata.build_custom_field(
        "Account",
        "CodCliente__c",
        "Text",
        length=20,
        label="Codigo do Cliente"
    )
    await metadata.create_metadata("CustomField", [field])
"""

import asyncio
import base64
import io
import logging
import zipfile
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union, TYPE_CHECKING
import xml.etree.ElementTree as ET

if TYPE_CHECKING:
    from .client import SalesforceClient

logger = logging.getLogger(__name__)

# Namespace da Metadata API
METADATA_NS = "http://soap.sforce.com/2006/04/metadata"


class MetadataType(str, Enum):
    """Tipos de metadata do Salesforce"""
    APEX_CLASS = "ApexClass"
    APEX_TRIGGER = "ApexTrigger"
    APEX_PAGE = "ApexPage"
    APEX_COMPONENT = "ApexComponent"
    CUSTOM_OBJECT = "CustomObject"
    CUSTOM_FIELD = "CustomField"
    CUSTOM_TAB = "CustomTab"
    VALIDATION_RULE = "ValidationRule"
    WORKFLOW_RULE = "WorkflowRule"
    FLOW = "Flow"
    PROCESS_BUILDER = "Flow"
    LIGHTNING_COMPONENT = "LightningComponentBundle"
    AURA_COMPONENT = "AuraDefinitionBundle"
    STATIC_RESOURCE = "StaticResource"
    REMOTE_SITE = "RemoteSiteSetting"
    NAMED_CREDENTIAL = "NamedCredential"
    PROFILE = "Profile"
    PERMISSION_SET = "PermissionSet"
    LAYOUT = "Layout"
    RECORD_TYPE = "RecordType"
    CUSTOM_APPLICATION = "CustomApplication"
    CUSTOM_LABEL = "CustomLabel"
    CUSTOM_METADATA = "CustomMetadata"
    REPORT = "Report"
    DASHBOARD = "Dashboard"


@dataclass
class DeployResult:
    """Resultado de um deploy"""
    id: str
    success: bool
    done: bool
    status: str
    state_detail: Optional[str] = None
    error_message: Optional[str] = None
    errors: List[Dict[str, Any]] = field(default_factory=list)
    component_successes: List[Dict[str, Any]] = field(default_factory=list)
    component_failures: List[Dict[str, Any]] = field(default_factory=list)
    tests_completed: int = 0
    tests_total: int = 0
    test_failures: List[Dict[str, Any]] = field(default_factory=list)
    created_date: Optional[datetime] = None
    completed_date: Optional[datetime] = None

    @property
    def has_failures(self) -> bool:
        return len(self.component_failures) > 0 or len(self.test_failures) > 0


@dataclass
class RetrieveResult:
    """Resultado de um retrieve"""
    id: str
    success: bool
    done: bool
    status: str
    error_message: Optional[str] = None
    zip_file: Optional[bytes] = None
    file_properties: List[Dict[str, Any]] = field(default_factory=list)


class MetadataClient:
    """
    Cliente para Metadata API do Salesforce

    Permite gerenciar configuracoes, objetos customizados,
    campos, classes Apex e outros componentes.

    Herda o contexto de tenant do SalesforceClient para isolamento multi-tenant.
    """

    def __init__(self, sf_client: "SalesforceClient"):
        """
        Inicializa o cliente de Metadata

        Args:
            sf_client: SalesforceClient autenticado (deve ter tenant_id configurado)
        """
        self.sf = sf_client
        self._session_id = None

    @property
    def tenant_id(self) -> str:
        """ID do tenant para isolamento (herdado do SalesforceClient)"""
        return self.sf.tenant_id

    @property
    def metadata_url(self) -> str:
        """URL da Metadata API"""
        return self.sf.config.metadata_url

    async def _soap_request(
        self,
        action: str,
        body: str,
        timeout: int = 120
    ) -> str:
        """
        Faz requisicao SOAP para Metadata API

        Args:
            action: Nome da operacao SOAP
            body: Corpo da requisicao XML
            timeout: Timeout em segundos

        Returns:
            Resposta XML
        """
        envelope = f"""<?xml version="1.0" encoding="UTF-8"?>
        <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                          xmlns:met="{METADATA_NS}">
            <soapenv:Header>
                <met:SessionHeader>
                    <met:sessionId>{self.sf.config.access_token}</met:sessionId>
                </met:SessionHeader>
            </soapenv:Header>
            <soapenv:Body>
                {body}
            </soapenv:Body>
        </soapenv:Envelope>"""

        headers = {
            "Content-Type": "text/xml; charset=UTF-8",
            "SOAPAction": action
        }

        session = await self.sf._get_session()
        async with session.post(
            self.metadata_url,
            data=envelope,
            headers=headers,
            timeout=timeout
        ) as response:
            return await response.text()

    # ==================== LIST METADATA ====================

    async def list_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        folder: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista metadatas de um tipo especifico

        Args:
            metadata_type: Tipo de metadata (ex: "CustomObject", "ApexClass")
            folder: Pasta para tipos que usam (ex: Reports, Documents)

        Returns:
            Lista de metadatas encontrados

        Exemplo:
            # Listar todos os objetos customizados
            objects = await metadata.list_metadata("CustomObject")

            # Listar classes Apex
            classes = await metadata.list_metadata("ApexClass")
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        folder_xml = f"<met:folder>{folder}</met:folder>" if folder else ""

        body = f"""
        <met:listMetadata>
            <met:queries>
                <met:type>{metadata_type}</met:type>
                {folder_xml}
            </met:queries>
            <met:asOfVersion>{self.sf.config.api_version}</met:asOfVersion>
        </met:listMetadata>
        """

        response = await self._soap_request("listMetadata", body)
        return self._parse_list_response(response)

    def _parse_list_response(self, response: str) -> List[Dict[str, Any]]:
        """Parseia resposta de listMetadata"""
        results = []
        try:
            root = ET.fromstring(response)

            for elem in root.iter():
                if 'result' in elem.tag.lower() and elem.tag.endswith('result'):
                    item = {}
                    for child in elem:
                        tag = child.tag.split('}')[-1]
                        item[tag] = child.text
                    if item:
                        results.append(item)

        except ET.ParseError as e:
            logger.error(f"Erro ao parsear resposta: {e}")

        return results

    # ==================== READ METADATA ====================

    async def read_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        full_names: List[str]
    ) -> List[Dict[str, Any]]:
        """
        Le metadatas especificos

        Args:
            metadata_type: Tipo de metadata
            full_names: Lista de nomes completos

        Returns:
            Lista de metadatas com detalhes

        Exemplo:
            # Ler definicao de objetos
            objs = await metadata.read_metadata("CustomObject", ["Account", "Opportunity__c"])
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        names_xml = "\n".join(f"<met:fullNames>{name}</met:fullNames>" for name in full_names)

        body = f"""
        <met:readMetadata>
            <met:type>{metadata_type}</met:type>
            {names_xml}
        </met:readMetadata>
        """

        response = await self._soap_request("readMetadata", body)
        return self._parse_read_response(response)

    def _parse_read_response(self, response: str) -> List[Dict[str, Any]]:
        """Parseia resposta de readMetadata"""
        results = []
        try:
            root = ET.fromstring(response)

            for elem in root.iter():
                if 'records' in elem.tag.lower():
                    item = self._element_to_dict(elem)
                    if item:
                        results.append(item)

        except ET.ParseError as e:
            logger.error(f"Erro ao parsear resposta: {e}")

        return results

    def _element_to_dict(self, elem: ET.Element) -> Dict[str, Any]:
        """Converte elemento XML para dicionario"""
        result = {}

        for child in elem:
            tag = child.tag.split('}')[-1]

            if len(child) > 0:
                # Elemento tem filhos
                if tag in result:
                    # Ja existe, converter para lista
                    if not isinstance(result[tag], list):
                        result[tag] = [result[tag]]
                    result[tag].append(self._element_to_dict(child))
                else:
                    result[tag] = self._element_to_dict(child)
            else:
                # Elemento simples
                if tag in result:
                    if not isinstance(result[tag], list):
                        result[tag] = [result[tag]]
                    result[tag].append(child.text)
                else:
                    result[tag] = child.text

        return result

    # ==================== CREATE/UPDATE METADATA ====================

    async def create_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        metadata: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Cria novos metadatas

        Args:
            metadata_type: Tipo de metadata
            metadata: Lista de metadatas para criar

        Returns:
            Lista de resultados (success, fullName, errors)
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        metadata_xml = self._build_metadata_xml(metadata_type, metadata)

        body = f"""
        <met:createMetadata>
            {metadata_xml}
        </met:createMetadata>
        """

        response = await self._soap_request("createMetadata", body)
        return self._parse_save_response(response)

    async def update_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        metadata: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Atualiza metadatas existentes

        Args:
            metadata_type: Tipo de metadata
            metadata: Lista de metadatas para atualizar

        Returns:
            Lista de resultados
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        metadata_xml = self._build_metadata_xml(metadata_type, metadata)

        body = f"""
        <met:updateMetadata>
            {metadata_xml}
        </met:updateMetadata>
        """

        response = await self._soap_request("updateMetadata", body)
        return self._parse_save_response(response)

    async def upsert_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        metadata: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Cria ou atualiza metadatas

        Args:
            metadata_type: Tipo de metadata
            metadata: Lista de metadatas

        Returns:
            Lista de resultados
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        metadata_xml = self._build_metadata_xml(metadata_type, metadata)

        body = f"""
        <met:upsertMetadata>
            {metadata_xml}
        </met:upsertMetadata>
        """

        response = await self._soap_request("upsertMetadata", body)
        return self._parse_save_response(response)

    async def delete_metadata(
        self,
        metadata_type: Union[str, MetadataType],
        full_names: List[str]
    ) -> List[Dict[str, Any]]:
        """
        Deleta metadatas

        Args:
            metadata_type: Tipo de metadata
            full_names: Lista de nomes para deletar

        Returns:
            Lista de resultados
        """
        if isinstance(metadata_type, MetadataType):
            metadata_type = metadata_type.value

        names_xml = "\n".join(f"<met:fullNames>{name}</met:fullNames>" for name in full_names)

        body = f"""
        <met:deleteMetadata>
            <met:type>{metadata_type}</met:type>
            {names_xml}
        </met:deleteMetadata>
        """

        response = await self._soap_request("deleteMetadata", body)
        return self._parse_save_response(response)

    def _build_metadata_xml(
        self,
        metadata_type: str,
        metadata_list: List[Dict[str, Any]]
    ) -> str:
        """Constroi XML de metadatas"""
        xml_parts = []

        for metadata in metadata_list:
            xml_parts.append(f'<met:metadata xsi:type="met:{metadata_type}" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">')
            xml_parts.append(self._dict_to_xml(metadata))
            xml_parts.append('</met:metadata>')

        return "\n".join(xml_parts)

    def _dict_to_xml(self, data: Dict[str, Any], indent: int = 0) -> str:
        """Converte dicionario para XML"""
        xml_parts = []
        prefix = "  " * indent

        for key, value in data.items():
            if value is None:
                continue

            if isinstance(value, list):
                for item in value:
                    if isinstance(item, dict):
                        xml_parts.append(f"{prefix}<met:{key}>")
                        xml_parts.append(self._dict_to_xml(item, indent + 1))
                        xml_parts.append(f"{prefix}</met:{key}>")
                    else:
                        xml_parts.append(f"{prefix}<met:{key}>{item}</met:{key}>")
            elif isinstance(value, dict):
                xml_parts.append(f"{prefix}<met:{key}>")
                xml_parts.append(self._dict_to_xml(value, indent + 1))
                xml_parts.append(f"{prefix}</met:{key}>")
            elif isinstance(value, bool):
                xml_parts.append(f"{prefix}<met:{key}>{str(value).lower()}</met:{key}>")
            else:
                xml_parts.append(f"{prefix}<met:{key}>{value}</met:{key}>")

        return "\n".join(xml_parts)

    def _parse_save_response(self, response: str) -> List[Dict[str, Any]]:
        """Parseia resposta de create/update/delete"""
        results = []
        try:
            root = ET.fromstring(response)

            for elem in root.iter():
                if 'result' in elem.tag.lower() and elem.tag.endswith('result'):
                    item = {}
                    for child in elem:
                        tag = child.tag.split('}')[-1]
                        if tag == 'errors':
                            if 'errors' not in item:
                                item['errors'] = []
                            error = {}
                            for err_child in child:
                                err_tag = err_child.tag.split('}')[-1]
                                error[err_tag] = err_child.text
                            item['errors'].append(error)
                        else:
                            item[tag] = child.text
                    if item:
                        results.append(item)

        except ET.ParseError as e:
            logger.error(f"Erro ao parsear resposta: {e}")

        return results

    # ==================== DEPLOY ====================

    async def deploy(
        self,
        zip_file: bytes,
        options: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Inicia deploy de componentes

        Args:
            zip_file: Arquivo ZIP com componentes
            options: Opcoes de deploy (testLevel, checkOnly, etc.)

        Returns:
            ID do deploy job

        Exemplo:
            # Criar ZIP com componentes
            zip_bytes = metadata.create_deploy_package([
                ("classes/MinhaClasse.cls", apex_code),
                ("classes/MinhaClasse.cls-meta.xml", meta_xml)
            ])

            # Iniciar deploy
            deploy_id = await metadata.deploy(zip_bytes, {"testLevel": "RunLocalTests"})

            # Verificar status
            result = await metadata.check_deploy_status(deploy_id)
        """
        options = options or {}

        # Configurar opcoes de deploy
        options_xml = ""
        if options:
            options_parts = []
            for key, value in options.items():
                if isinstance(value, bool):
                    value = str(value).lower()
                options_parts.append(f"<met:{key}>{value}</met:{key}>")
            options_xml = f"<met:DeployOptions>{''.join(options_parts)}</met:DeployOptions>"

        # Codificar ZIP em base64
        zip_base64 = base64.b64encode(zip_file).decode('utf-8')

        body = f"""
        <met:deploy>
            <met:ZipFile>{zip_base64}</met:ZipFile>
            {options_xml}
        </met:deploy>
        """

        response = await self._soap_request("deploy", body, timeout=300)

        # Extrair ID do deploy
        try:
            root = ET.fromstring(response)
            for elem in root.iter():
                if 'id' in elem.tag.lower() and elem.text:
                    return elem.text
        except ET.ParseError:
            pass

        raise Exception("Nao foi possivel obter ID do deploy")

    async def check_deploy_status(
        self,
        deploy_id: str,
        include_details: bool = True
    ) -> DeployResult:
        """
        Verifica status de um deploy

        Args:
            deploy_id: ID do deploy
            include_details: Incluir detalhes de componentes

        Returns:
            DeployResult com status e detalhes
        """
        body = f"""
        <met:checkDeployStatus>
            <met:asyncProcessId>{deploy_id}</met:asyncProcessId>
            <met:includeDetails>{str(include_details).lower()}</met:includeDetails>
        </met:checkDeployStatus>
        """

        response = await self._soap_request("checkDeployStatus", body)
        return self._parse_deploy_result(response, deploy_id)

    def _parse_deploy_result(self, response: str, deploy_id: str) -> DeployResult:
        """Parseia resultado de deploy"""
        result = DeployResult(
            id=deploy_id,
            success=False,
            done=False,
            status="Unknown"
        )

        try:
            root = ET.fromstring(response)

            for elem in root.iter():
                tag = elem.tag.split('}')[-1].lower()

                if tag == 'done':
                    result.done = elem.text.lower() == 'true'
                elif tag == 'success':
                    result.success = elem.text.lower() == 'true'
                elif tag == 'status':
                    result.status = elem.text
                elif tag == 'statedetail':
                    result.state_detail = elem.text
                elif tag == 'errormessage':
                    result.error_message = elem.text
                elif tag == 'componentfailures':
                    failure = {}
                    for child in elem:
                        child_tag = child.tag.split('}')[-1]
                        failure[child_tag] = child.text
                    result.component_failures.append(failure)
                elif tag == 'componentsuccesses':
                    success_item = {}
                    for child in elem:
                        child_tag = child.tag.split('}')[-1]
                        success_item[child_tag] = child.text
                    result.component_successes.append(success_item)

        except ET.ParseError as e:
            logger.error(f"Erro ao parsear resultado: {e}")

        return result

    async def deploy_and_wait(
        self,
        zip_file: bytes,
        options: Optional[Dict[str, Any]] = None,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> DeployResult:
        """
        Faz deploy e aguarda conclusao

        Args:
            zip_file: Arquivo ZIP com componentes
            options: Opcoes de deploy
            poll_interval: Intervalo de verificacao em segundos
            timeout: Timeout maximo em segundos

        Returns:
            DeployResult final
        """
        deploy_id = await self.deploy(zip_file, options)
        logger.info(f"Deploy iniciado: {deploy_id}")

        start_time = asyncio.get_event_loop().time()

        while True:
            result = await self.check_deploy_status(deploy_id)

            if result.done:
                return result

            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed > timeout:
                raise TimeoutError(f"Deploy timeout apos {timeout}s")

            logger.debug(f"Deploy status: {result.status}")
            await asyncio.sleep(poll_interval)

    # ==================== RETRIEVE ====================

    async def retrieve(
        self,
        package: Dict[str, List[str]],
        single_package: bool = True
    ) -> str:
        """
        Inicia retrieve de componentes

        Args:
            package: Dicionario {tipo: [nomes]}
            single_package: Retornar como pacote unico

        Returns:
            ID do retrieve job

        Exemplo:
            retrieve_id = await metadata.retrieve({
                "ApexClass": ["MinhaClasse", "OutraClasse"],
                "ApexTrigger": ["MeuTrigger"]
            })
        """
        # Construir package XML
        types_xml = []
        for type_name, members in package.items():
            members_xml = "\n".join(f"<met:members>{m}</met:members>" for m in members)
            types_xml.append(f"""
            <met:types>
                {members_xml}
                <met:name>{type_name}</met:name>
            </met:types>
            """)

        body = f"""
        <met:retrieve>
            <met:retrieveRequest>
                <met:apiVersion>{self.sf.config.api_version}</met:apiVersion>
                <met:singlePackage>{str(single_package).lower()}</met:singlePackage>
                <met:unpackaged>
                    {"".join(types_xml)}
                </met:unpackaged>
            </met:retrieveRequest>
        </met:retrieve>
        """

        response = await self._soap_request("retrieve", body, timeout=300)

        # Extrair ID
        try:
            root = ET.fromstring(response)
            for elem in root.iter():
                if 'id' in elem.tag.lower() and elem.text:
                    return elem.text
        except ET.ParseError:
            pass

        raise Exception("Nao foi possivel obter ID do retrieve")

    async def check_retrieve_status(self, retrieve_id: str) -> RetrieveResult:
        """
        Verifica status de um retrieve

        Args:
            retrieve_id: ID do retrieve

        Returns:
            RetrieveResult com status e arquivo ZIP
        """
        body = f"""
        <met:checkRetrieveStatus>
            <met:asyncProcessId>{retrieve_id}</met:asyncProcessId>
            <met:includeZip>true</met:includeZip>
        </met:checkRetrieveStatus>
        """

        response = await self._soap_request("checkRetrieveStatus", body)
        return self._parse_retrieve_result(response, retrieve_id)

    def _parse_retrieve_result(self, response: str, retrieve_id: str) -> RetrieveResult:
        """Parseia resultado de retrieve"""
        result = RetrieveResult(
            id=retrieve_id,
            success=False,
            done=False,
            status="Unknown"
        )

        try:
            root = ET.fromstring(response)

            for elem in root.iter():
                tag = elem.tag.split('}')[-1].lower()

                if tag == 'done':
                    result.done = elem.text.lower() == 'true'
                elif tag == 'success':
                    result.success = elem.text.lower() == 'true'
                elif tag == 'status':
                    result.status = elem.text
                elif tag == 'errormessage':
                    result.error_message = elem.text
                elif tag == 'zipfile' and elem.text:
                    result.zip_file = base64.b64decode(elem.text)

        except ET.ParseError as e:
            logger.error(f"Erro ao parsear resultado: {e}")

        return result

    async def retrieve_and_wait(
        self,
        package: Dict[str, List[str]],
        poll_interval: int = 5,
        timeout: int = 300
    ) -> RetrieveResult:
        """
        Faz retrieve e aguarda conclusao

        Args:
            package: Dicionario {tipo: [nomes]}
            poll_interval: Intervalo de verificacao
            timeout: Timeout maximo

        Returns:
            RetrieveResult com arquivo ZIP
        """
        retrieve_id = await self.retrieve(package)
        logger.info(f"Retrieve iniciado: {retrieve_id}")

        start_time = asyncio.get_event_loop().time()

        while True:
            result = await self.check_retrieve_status(retrieve_id)

            if result.done:
                return result

            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed > timeout:
                raise TimeoutError(f"Retrieve timeout apos {timeout}s")

            await asyncio.sleep(poll_interval)

    # ==================== HELPERS ====================

    def create_deploy_package(
        self,
        files: List[tuple],
        package_name: str = "FabricaAgentes"
    ) -> bytes:
        """
        Cria pacote ZIP para deploy

        Args:
            files: Lista de (caminho, conteudo)
            package_name: Nome do pacote

        Returns:
            Bytes do arquivo ZIP

        Exemplo:
            zip_bytes = metadata.create_deploy_package([
                ("classes/MinhaClasse.cls", "public class MinhaClasse { }"),
                ("classes/MinhaClasse.cls-meta.xml", meta_xml)
            ])
        """
        zip_buffer = io.BytesIO()

        with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zf:
            # Adicionar arquivos
            for path, content in files:
                if isinstance(content, str):
                    content = content.encode('utf-8')
                zf.writestr(path, content)

            # Adicionar package.xml
            package_xml = self._generate_package_xml(files)
            zf.writestr("package.xml", package_xml)

        return zip_buffer.getvalue()

    def _generate_package_xml(self, files: List[tuple]) -> str:
        """Gera package.xml para lista de arquivos"""
        # Mapear tipos por extensao
        type_mapping = {
            '.cls': 'ApexClass',
            '.trigger': 'ApexTrigger',
            '.page': 'ApexPage',
            '.component': 'ApexComponent',
            '.resource': 'StaticResource',
            '.object': 'CustomObject',
            '.field': 'CustomField',
            '.tab': 'CustomTab',
            '.flow': 'Flow',
            '.layout': 'Layout',
            '.profile': 'Profile',
            '.permissionset': 'PermissionSet'
        }

        # Agrupar por tipo
        types = {}
        for path, _ in files:
            if path.endswith('-meta.xml'):
                continue

            for ext, type_name in type_mapping.items():
                if path.endswith(ext):
                    if type_name not in types:
                        types[type_name] = []

                    # Extrair nome do componente
                    name = path.split('/')[-1].replace(ext, '')
                    types[type_name].append(name)
                    break

        # Gerar XML
        types_xml = []
        for type_name, members in types.items():
            members_xml = "\n        ".join(f"<members>{m}</members>" for m in members)
            types_xml.append(f"""
    <types>
        {members_xml}
        <name>{type_name}</name>
    </types>""")

        return f"""<?xml version="1.0" encoding="UTF-8"?>
<Package xmlns="{METADATA_NS}">
    {"".join(types_xml)}
    <version>{self.sf.config.api_version}</version>
</Package>"""

    def build_custom_field(
        self,
        object_name: str,
        field_name: str,
        field_type: str,
        label: Optional[str] = None,
        length: Optional[int] = None,
        precision: Optional[int] = None,
        scale: Optional[int] = None,
        required: bool = False,
        unique: bool = False,
        external_id: bool = False,
        description: Optional[str] = None,
        default_value: Optional[str] = None,
        picklist_values: Optional[List[str]] = None,
        reference_to: Optional[str] = None,
        relationship_name: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Constroi definicao de campo customizado

        Args:
            object_name: Nome do objeto
            field_name: Nome do campo (deve terminar em __c)
            field_type: Tipo do campo (Text, Number, Checkbox, etc.)
            label: Label do campo
            length: Comprimento para campos texto
            precision: Precisao para campos numero
            scale: Escala para campos numero
            required: Campo obrigatorio
            unique: Valor unico
            external_id: External ID
            description: Descricao do campo
            default_value: Valor padrao
            picklist_values: Valores para picklist
            reference_to: Objeto referenciado (para Lookup/Master-Detail)
            relationship_name: Nome do relacionamento

        Returns:
            Dict com definicao do campo
        """
        if not field_name.endswith('__c'):
            field_name += '__c'

        field = {
            'fullName': f"{object_name}.{field_name}",
            'label': label or field_name.replace('__c', '').replace('_', ' '),
            'type': field_type,
            'required': required
        }

        # Configuracoes por tipo
        if field_type in ('Text', 'TextArea', 'LongTextArea', 'Html'):
            if length:
                field['length'] = length

        elif field_type in ('Number', 'Currency', 'Percent'):
            if precision:
                field['precision'] = precision
            if scale is not None:
                field['scale'] = scale

        elif field_type == 'Checkbox':
            field['defaultValue'] = default_value or 'false'

        elif field_type == 'Picklist':
            if picklist_values:
                field['picklist'] = {
                    'picklistValues': [
                        {'fullName': v, 'default': i == 0}
                        for i, v in enumerate(picklist_values)
                    ]
                }

        elif field_type in ('Lookup', 'MasterDetail'):
            if reference_to:
                field['referenceTo'] = reference_to
                field['relationshipName'] = relationship_name or reference_to.replace('__c', '') + 's'
                if field_type == 'Lookup':
                    field['deleteConstraint'] = 'SetNull'

        # Configuracoes opcionais
        if unique:
            field['unique'] = True
        if external_id:
            field['externalId'] = True
        if description:
            field['description'] = description

        return field

    def build_custom_object(
        self,
        object_name: str,
        label: str,
        label_plural: Optional[str] = None,
        description: Optional[str] = None,
        name_field_type: str = 'Text',
        name_field_label: str = 'Nome',
        sharing_model: str = 'ReadWrite',
        enable_activities: bool = True,
        enable_reports: bool = True,
        enable_search: bool = True,
        enable_feeds: bool = False
    ) -> Dict[str, Any]:
        """
        Constroi definicao de objeto customizado

        Args:
            object_name: Nome do objeto (deve terminar em __c)
            label: Label singular
            label_plural: Label plural
            description: Descricao
            name_field_type: Tipo do campo Name (Text ou AutoNumber)
            name_field_label: Label do campo Name
            sharing_model: Modelo de compartilhamento
            enable_*: Features habilitadas

        Returns:
            Dict com definicao do objeto
        """
        if not object_name.endswith('__c'):
            object_name += '__c'

        obj = {
            'fullName': object_name,
            'label': label,
            'pluralLabel': label_plural or label + 's',
            'deploymentStatus': 'Deployed',
            'sharingModel': sharing_model,
            'enableActivities': enable_activities,
            'enableReports': enable_reports,
            'enableSearch': enable_search,
            'enableFeeds': enable_feeds,
            'nameField': {
                'label': name_field_label,
                'type': name_field_type
            }
        }

        if description:
            obj['description'] = description

        if name_field_type == 'AutoNumber':
            obj['nameField']['displayFormat'] = f"{object_name[:3].upper()}-{{00000}}"

        return obj
