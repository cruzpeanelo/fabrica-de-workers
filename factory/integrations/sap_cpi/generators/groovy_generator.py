# -*- coding: utf-8 -*-
"""
SAP CPI Groovy Generator
========================

Gerador de scripts Groovy para SAP CPI.

Funcionalidades:
- Templates de scripts comuns
- Geração de scripts de processamento
- Funções utilitárias CPI
- Scripts de validação
- Scripts de transformação
- Geração de exception handlers
"""

import logging
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from enum import Enum
from datetime import datetime

logger = logging.getLogger(__name__)


class ScriptPurpose(str, Enum):
    """Propósito do script"""
    PROCESSOR = "processor"  # Processamento de mensagem
    VALIDATOR = "validator"  # Validação de dados
    TRANSFORMER = "transformer"  # Transformação de dados
    ROUTER = "router"  # Roteamento condicional
    LOGGER = "logger"  # Logging avançado
    ERROR_HANDLER = "error_handler"  # Tratamento de erro
    SECURITY = "security"  # Operações de segurança
    UTILITY = "utility"  # Funções utilitárias


@dataclass
class ScriptFunction:
    """Definição de uma função do script"""
    name: str
    parameters: List[str] = field(default_factory=list)
    return_type: str = "def"
    body: str = ""
    description: str = ""
    is_entry_point: bool = False


@dataclass
class ScriptDefinition:
    """Definição de um script Groovy"""
    name: str
    purpose: ScriptPurpose
    description: str = ""
    imports: List[str] = field(default_factory=list)
    functions: List[ScriptFunction] = field(default_factory=list)
    global_variables: Dict[str, str] = field(default_factory=dict)
    entry_point: str = "processData"


class GroovyGenerator:
    """
    Gerador de scripts Groovy para SAP CPI.

    Exemplo:
    ```python
    generator = GroovyGenerator()

    # Gera script de processamento simples
    script = generator.generate_processor_script(
        name="transform_order",
        description="Transforma pedido para formato SAP",
        body='''
            def body = message.getBody(String)
            def xml = new XmlSlurper().parseText(body)

            // Transforma campos
            def result = "<SAP_ORDER>"
            result += "<VBELN>" + xml.orderId.text() + "</VBELN>"
            result += "<KUNNR>" + xml.customer.id.text() + "</KUNNR>"
            result += "</SAP_ORDER>"

            message.setBody(result)
            return message
        '''
    )
    print(script)

    # Gera script de validação
    validation_script = generator.generate_validation_script(
        name="validate_invoice",
        required_fields=["invoiceNumber", "customerId", "totalAmount"],
        field_validators={
            "totalAmount": "value.toDouble() > 0",
            "invoiceNumber": "value.matches('[A-Z]{2}[0-9]{8}')"
        }
    )
    ```
    """

    # Template base de script CPI
    SCRIPT_TEMPLATE = '''// -*- coding: utf-8 -*-
/**
 * {name}
 * {description}
 *
 * Propósito: {purpose}
 * Gerado em: {generated_at}
 *
 * @author SAP CPI Groovy Generator
 */

{imports}

{global_vars}

{functions}
'''

    # Imports comuns
    COMMON_IMPORTS = [
        "com.sap.gateway.ip.core.customdev.util.Message",
        "java.util.HashMap",
    ]

    # Imports por propósito
    PURPOSE_IMPORTS = {
        ScriptPurpose.PROCESSOR: [
            "groovy.xml.XmlSlurper",
            "groovy.xml.MarkupBuilder",
        ],
        ScriptPurpose.VALIDATOR: [
            "groovy.json.JsonSlurper",
            "groovy.xml.XmlSlurper",
        ],
        ScriptPurpose.TRANSFORMER: [
            "groovy.xml.XmlSlurper",
            "groovy.xml.MarkupBuilder",
            "groovy.json.JsonSlurper",
            "groovy.json.JsonOutput",
        ],
        ScriptPurpose.LOGGER: [
            "com.sap.it.api.ITApiFactory",
            "com.sap.it.api.msglog.MessageLogFactory",
        ],
        ScriptPurpose.ERROR_HANDLER: [
            "com.sap.gateway.ip.core.customdev.util.Message",
        ],
        ScriptPurpose.SECURITY: [
            "java.security.MessageDigest",
            "javax.crypto.Cipher",
            "javax.crypto.spec.SecretKeySpec",
            "java.util.Base64",
        ],
    }

    def __init__(self):
        """Inicializa o gerador"""
        pass

    def generate(self, definition: ScriptDefinition) -> str:
        """
        Gera script Groovy completo.

        Args:
            definition: Definição do script

        Returns:
            String com código Groovy
        """
        # Coleta imports
        imports = set(self.COMMON_IMPORTS)
        imports.update(self.PURPOSE_IMPORTS.get(definition.purpose, []))
        imports.update(definition.imports)

        imports_str = "\n".join([f"import {imp}" for imp in sorted(imports)])

        # Variáveis globais
        global_vars = ""
        if definition.global_variables:
            vars_list = [f"def {k} = {v}" for k, v in definition.global_variables.items()]
            global_vars = "\n".join(vars_list)

        # Funções
        functions_str = "\n\n".join([
            self._generate_function(func) for func in definition.functions
        ])

        return self.SCRIPT_TEMPLATE.format(
            name=definition.name,
            description=definition.description,
            purpose=definition.purpose.value,
            generated_at=datetime.utcnow().isoformat(),
            imports=imports_str,
            global_vars=global_vars,
            functions=functions_str
        )

    def _generate_function(self, func: ScriptFunction) -> str:
        """Gera código de uma função"""
        params = ", ".join(func.parameters)

        docstring = ""
        if func.description:
            docstring = f'''/**
 * {func.description}
 */
'''

        return f'''{docstring}{func.return_type} {func.name}({params}) {{
{self._indent(func.body, 4)}
}}'''

    def generate_processor_script(
        self,
        name: str,
        body: str,
        description: str = "",
        additional_imports: List[str] = None
    ) -> str:
        """
        Gera script de processamento de mensagem.

        Args:
            name: Nome do script
            body: Corpo do processamento
            description: Descrição
            additional_imports: Imports adicionais

        Returns:
            String com código Groovy
        """
        definition = ScriptDefinition(
            name=name,
            purpose=ScriptPurpose.PROCESSOR,
            description=description or f"Script de processamento: {name}",
            imports=additional_imports or [],
            functions=[
                ScriptFunction(
                    name="processData",
                    parameters=["Message message"],
                    return_type="def",
                    body=body,
                    description="Ponto de entrada para processamento de mensagem",
                    is_entry_point=True
                )
            ]
        )

        return self.generate(definition)

    def generate_validation_script(
        self,
        name: str,
        required_fields: List[str],
        field_validators: Dict[str, str] = None,
        message_format: str = "xml"  # xml ou json
    ) -> str:
        """
        Gera script de validação de campos.

        Args:
            name: Nome do script
            required_fields: Lista de campos obrigatórios
            field_validators: Validadores customizados {campo: expressão}
            message_format: Formato da mensagem (xml/json)

        Returns:
            String com código Groovy
        """
        field_validators = field_validators or {}

        # Gera validação de campos obrigatórios
        required_checks = []
        for field in required_fields:
            required_checks.append(f'''
        // Valida campo obrigatório: {field}
        def {self._to_var_name(field)} = {self._get_field_accessor(field, message_format)}
        if ({self._to_var_name(field)} == null || {self._to_var_name(field)}.toString().trim().isEmpty()) {{
            errors.add("Campo obrigatório '{field}' está vazio ou ausente")
        }}''')

        # Gera validações customizadas
        custom_checks = []
        for field, validator in field_validators.items():
            custom_checks.append(f'''
        // Validação customizada: {field}
        def {self._to_var_name(field)}_val = {self._get_field_accessor(field, message_format)}
        if ({self._to_var_name(field)}_val != null) {{
            def value = {self._to_var_name(field)}_val.toString()
            if (!({validator})) {{
                errors.add("Campo '{field}' não passou na validação: {validator}")
            }}
        }}''')

        parser = "new XmlSlurper().parseText(body)" if message_format == "xml" else "new JsonSlurper().parseText(body)"

        body = f'''
    def body = message.getBody(String)
    def errors = []

    try {{
        def data = {parser}
        {chr(10).join(required_checks)}
        {chr(10).join(custom_checks)}
    }} catch (Exception e) {{
        errors.add("Erro ao parsear mensagem: " + e.getMessage())
    }}

    if (!errors.isEmpty()) {{
        def errorMessage = "Validação falhou:\\n" + errors.join("\\n")
        message.setProperty("validationErrors", errors)
        message.setProperty("validationStatus", "FAILED")
        throw new Exception(errorMessage)
    }}

    message.setProperty("validationStatus", "PASSED")
    return message'''

        imports = ["groovy.json.JsonSlurper"] if message_format == "json" else ["groovy.xml.XmlSlurper"]

        definition = ScriptDefinition(
            name=name,
            purpose=ScriptPurpose.VALIDATOR,
            description=f"Validação de mensagem {message_format.upper()}",
            imports=imports,
            functions=[
                ScriptFunction(
                    name="processData",
                    parameters=["Message message"],
                    return_type="def",
                    body=body,
                    is_entry_point=True
                )
            ]
        )

        return self.generate(definition)

    def generate_transformation_script(
        self,
        name: str,
        source_format: str,  # xml ou json
        target_format: str,  # xml ou json
        field_mappings: Dict[str, str],
        description: str = ""
    ) -> str:
        """
        Gera script de transformação de dados.

        Args:
            name: Nome do script
            source_format: Formato fonte (xml/json)
            target_format: Formato destino (xml/json)
            field_mappings: Mapeamento de campos {destino: fonte}
            description: Descrição

        Returns:
            String com código Groovy
        """
        # Parser baseado no formato fonte
        parser = "new XmlSlurper().parseText(body)" if source_format == "xml" else "new JsonSlurper().parseText(body)"

        # Gera mapeamentos
        mappings = []
        for target_field, source_field in field_mappings.items():
            accessor = self._get_field_accessor(source_field, source_format)
            mappings.append(f'result.{target_field} = {accessor}')

        if target_format == "json":
            output_code = f'''
    def result = [:]
    {chr(10).join(mappings)}

    def outputJson = JsonOutput.toJson(result)
    message.setBody(outputJson)'''
        else:
            # XML output usando MarkupBuilder
            xml_elements = []
            for target_field, source_field in field_mappings.items():
                accessor = self._get_field_accessor(source_field, source_format)
                xml_elements.append(f'{target_field}({accessor})')

            output_code = f'''
    def writer = new StringWriter()
    def xml = new MarkupBuilder(writer)

    xml.root {{
        {chr(10).join(xml_elements)}
    }}

    message.setBody(writer.toString())'''

        body = f'''
    def body = message.getBody(String)
    def source = {parser}
    {output_code}

    return message'''

        imports = []
        if source_format == "json" or target_format == "json":
            imports.extend(["groovy.json.JsonSlurper", "groovy.json.JsonOutput"])
        if source_format == "xml" or target_format == "xml":
            imports.extend(["groovy.xml.XmlSlurper", "groovy.xml.MarkupBuilder"])

        definition = ScriptDefinition(
            name=name,
            purpose=ScriptPurpose.TRANSFORMER,
            description=description or f"Transformação {source_format.upper()} -> {target_format.upper()}",
            imports=list(set(imports)),
            functions=[
                ScriptFunction(
                    name="processData",
                    parameters=["Message message"],
                    return_type="def",
                    body=body,
                    is_entry_point=True
                )
            ]
        )

        return self.generate(definition)

    def generate_logging_script(
        self,
        name: str,
        log_headers: bool = True,
        log_properties: bool = True,
        log_body: bool = False,
        custom_log_fields: List[str] = None
    ) -> str:
        """
        Gera script de logging avançado.

        Args:
            name: Nome do script
            log_headers: Logar headers
            log_properties: Logar properties
            log_body: Logar body da mensagem
            custom_log_fields: Campos específicos para logar

        Returns:
            String com código Groovy
        """
        log_sections = []

        if log_headers:
            log_sections.append('''
    // Log Headers
    def headers = message.getHeaders()
    messageLog.addAttachmentAsString("Headers", headers.toString(), "text/plain")''')

        if log_properties:
            log_sections.append('''
    // Log Properties
    def properties = message.getProperties()
    messageLog.addAttachmentAsString("Properties", properties.toString(), "text/plain")''')

        if log_body:
            log_sections.append('''
    // Log Body (cuidado com payloads grandes!)
    def body = message.getBody(String)
    if (body?.length() < 100000) {
        messageLog.addAttachmentAsString("Body", body, "text/plain")
    } else {
        messageLog.addAttachmentAsString("Body", "Body muito grande para logar: " + body.length() + " caracteres", "text/plain")
    }''')

        if custom_log_fields:
            for field_path in custom_log_fields:
                var_name = self._to_var_name(field_path)
                log_sections.append(f'''
    // Log campo: {field_path}
    def {var_name} = message.getProperty("{field_path}") ?: message.getHeaders().get("{field_path}")
    if ({var_name} != null) {{
        messageLog.addAttachmentAsString("{field_path}", {var_name}.toString(), "text/plain")
    }}''')

        body = f'''
    def messageLogFactory = ITApiFactory.getApi(MessageLogFactory.class, null)
    def messageLog = messageLogFactory.getMessageLog(message)
    {chr(10).join(log_sections)}

    return message'''

        definition = ScriptDefinition(
            name=name,
            purpose=ScriptPurpose.LOGGER,
            description="Script de logging avançado para debug",
            imports=[
                "com.sap.it.api.ITApiFactory",
                "com.sap.it.api.msglog.MessageLogFactory"
            ],
            functions=[
                ScriptFunction(
                    name="processData",
                    parameters=["Message message"],
                    return_type="def",
                    body=body,
                    is_entry_point=True
                )
            ]
        )

        return self.generate(definition)

    def generate_error_handler_script(
        self,
        name: str,
        error_response_format: str = "json",
        include_stack_trace: bool = False,
        notification_property: str = ""
    ) -> str:
        """
        Gera script de tratamento de erros.

        Args:
            name: Nome do script
            error_response_format: Formato da resposta de erro
            include_stack_trace: Incluir stack trace na resposta
            notification_property: Property para indicar que deve notificar

        Returns:
            String com código Groovy
        """
        stack_trace_code = ""
        if include_stack_trace:
            stack_trace_code = '''
    def sw = new StringWriter()
    ex?.printStackTrace(new PrintWriter(sw))
    errorDetails["stackTrace"] = sw.toString()'''

        notification_code = ""
        if notification_property:
            notification_code = f'''
    // Sinaliza para notificação
    message.setProperty("{notification_property}", true)'''

        if error_response_format == "json":
            response_code = '''
    def errorJson = JsonOutput.toJson(errorDetails)
    message.setBody(errorJson)
    message.setHeader("Content-Type", "application/json")'''
            imports = ["groovy.json.JsonOutput"]
        else:
            response_code = '''
    def writer = new StringWriter()
    def xml = new MarkupBuilder(writer)
    xml.error {
        code(errorDetails["code"])
        message(errorDetails["message"])
        timestamp(errorDetails["timestamp"])
        correlationId(errorDetails["correlationId"])
    }
    message.setBody(writer.toString())
    message.setHeader("Content-Type", "application/xml")'''
            imports = ["groovy.xml.MarkupBuilder"]

        body = f'''
    // Obtém informações do erro
    def ex = message.getProperty("CamelExceptionCaught")
    def correlationId = message.getProperty("SAP_MessageProcessingLogID") ?: UUID.randomUUID().toString()

    def errorDetails = [
        "code": "ERR_PROCESSING",
        "message": ex?.getMessage() ?: "Erro desconhecido",
        "timestamp": new Date().format("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
        "correlationId": correlationId
    ]
    {stack_trace_code}

    // Loga o erro
    def messageLog = message.getProperty("SAP_MessageLog")
    if (messageLog) {{
        messageLog.addAttachmentAsString("ErrorDetails", errorDetails.toString(), "text/plain")
    }}
    {notification_code}

    // Gera resposta de erro
    {response_code}

    // Define status HTTP de erro
    message.setHeader("CamelHttpResponseCode", 500)

    return message'''

        definition = ScriptDefinition(
            name=name,
            purpose=ScriptPurpose.ERROR_HANDLER,
            description="Tratamento padronizado de erros",
            imports=imports,
            functions=[
                ScriptFunction(
                    name="processData",
                    parameters=["Message message"],
                    return_type="def",
                    body=body,
                    is_entry_point=True
                )
            ]
        )

        return self.generate(definition)

    def _get_field_accessor(self, field_path: str, format_type: str) -> str:
        """Gera accessor para campo baseado no formato"""
        # Remove barras iniciais
        clean_path = field_path.lstrip("/")

        if format_type == "json":
            # JSON: data.field1.field2
            parts = clean_path.replace("/", ".").split(".")
            return "data." + ".".join(parts)
        else:
            # XML: data.field1.field2.text()
            parts = clean_path.replace("/", ".").split(".")
            return "data." + ".".join(parts) + ".text()"

    def _to_var_name(self, field_path: str) -> str:
        """Converte path de campo para nome de variável válido"""
        import re
        # Remove caracteres especiais e converte para camelCase
        clean = re.sub(r'[^a-zA-Z0-9]', '_', field_path)
        clean = re.sub(r'_+', '_', clean).strip('_')
        return clean.lower()

    def _indent(self, text: str, spaces: int) -> str:
        """Indenta texto com número de espaços"""
        indent = " " * spaces
        lines = text.strip().split("\n")
        return "\n".join([indent + line for line in lines])
