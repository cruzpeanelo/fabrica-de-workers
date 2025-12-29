# -*- coding: utf-8 -*-
"""
Salesforce Apex Code Generator
==============================
Gerador de codigo Apex (classes e triggers).

Funcionalidades:
- Geracao de classes Apex (Service, Selector, Domain, etc)
- Geracao de triggers
- Geracao de classes de teste
- Templates customizaveis
- Boas praticas automaticas

Exemplo de uso:
    from factory.integrations.salesforce.generators import ApexGenerator

    generator = ApexGenerator()

    # Gerar classe service
    code = generator.generate_service_class("AccountService", "Account")

    # Gerar trigger com handler
    trigger_code, handler_code = generator.generate_trigger_with_handler(
        "Account", ["before insert", "after update"]
    )

    # Gerar classe de teste
    test_code = generator.generate_test_class("AccountServiceTest", "AccountService")
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)


class ApexClassType(str, Enum):
    """Tipos de classes Apex"""
    SERVICE = "Service"
    SELECTOR = "Selector"
    DOMAIN = "Domain"
    TRIGGER_HANDLER = "TriggerHandler"
    CONTROLLER = "Controller"
    BATCH = "Batch"
    SCHEDULED = "Scheduled"
    QUEUEABLE = "Queueable"
    REST_RESOURCE = "RestResource"
    TEST = "Test"
    UTILITY = "Utility"
    WRAPPER = "Wrapper"
    EXCEPTION = "Exception"


@dataclass
class ApexMethod:
    """Definicao de um metodo Apex"""
    name: str
    return_type: str = "void"
    parameters: List[Tuple[str, str]] = field(default_factory=list)
    modifiers: List[str] = field(default_factory=lambda: ["public"])
    body: str = ""
    description: str = ""
    is_test: bool = False
    annotations: List[str] = field(default_factory=list)


@dataclass
class ApexProperty:
    """Definicao de uma propriedade Apex"""
    name: str
    type: str
    modifiers: List[str] = field(default_factory=lambda: ["private"])
    getter: bool = True
    setter: bool = True
    default_value: Optional[str] = None


class ApexGenerator:
    """
    Gerador de codigo Apex

    Gera classes, triggers e testes seguindo
    boas praticas do Salesforce.
    """

    def __init__(self, api_version: str = "59.0"):
        """
        Inicializa o gerador

        Args:
            api_version: Versao da API Salesforce
        """
        self.api_version = api_version
        self._indent = "    "  # 4 espacos

    def generate_service_class(
        self,
        class_name: str,
        sobject: str,
        methods: Optional[List[ApexMethod]] = None,
        include_crud: bool = True
    ) -> str:
        """
        Gera uma classe Service

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            methods: Metodos adicionais
            include_crud: Incluir metodos CRUD basicos

        Returns:
            Codigo Apex da classe
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, "Service Layer para " + sobject))

        # Classe
        lines.append(f"public with sharing class {class_name} {{")
        lines.append("")

        # Singleton instance
        lines.append(f"{self._indent}private static {class_name} instance;")
        lines.append("")

        # Selector reference
        selector_name = f"{sobject}Selector"
        lines.append(f"{self._indent}private {selector_name} selector;")
        lines.append("")

        # Constructor
        lines.append(f"{self._indent}private {class_name}() {{")
        lines.append(f"{self._indent}{self._indent}this.selector = new {selector_name}();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # getInstance
        lines.append(f"{self._indent}public static {class_name} getInstance() {{")
        lines.append(f"{self._indent}{self._indent}if (instance == null) {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}instance = new {class_name}();")
        lines.append(f"{self._indent}{self._indent}}}")
        lines.append(f"{self._indent}{self._indent}return instance;")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # CRUD methods
        if include_crud:
            lines.extend(self._generate_crud_methods(sobject))

        # Custom methods
        if methods:
            for method in methods:
                lines.extend(self._generate_method(method))

        lines.append("}")

        return "\n".join(lines)

    def generate_selector_class(
        self,
        class_name: str,
        sobject: str,
        fields: Optional[List[str]] = None,
        queries: Optional[List[Dict[str, Any]]] = None
    ) -> str:
        """
        Gera uma classe Selector

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            fields: Campos a selecionar
            queries: Queries customizadas

        Returns:
            Codigo Apex da classe
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, "Selector para " + sobject))

        # Classe
        lines.append(f"public inherited sharing class {class_name} {{")
        lines.append("")

        # Fields
        default_fields = fields or ["Id", "Name", "CreatedDate", "LastModifiedDate"]
        fields_str = "', '".join(default_fields)

        lines.append(f"{self._indent}private static final List<String> DEFAULT_FIELDS = new List<String>{{")
        lines.append(f"{self._indent}{self._indent}'{fields_str}'")
        lines.append(f"{self._indent}}};")
        lines.append("")

        # getFieldListString
        lines.append(f"{self._indent}private String getFieldListString() {{")
        lines.append(f"{self._indent}{self._indent}return String.join(DEFAULT_FIELDS, ', ');")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # selectById
        lines.append(f"{self._indent}public {sobject} selectById(Id recordId) {{")
        lines.append(f"{self._indent}{self._indent}List<{sobject}> records = selectByIds(new Set<Id>{{recordId}});")
        lines.append(f"{self._indent}{self._indent}return records.isEmpty() ? null : records[0];")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # selectByIds
        lines.append(f"{self._indent}public List<{sobject}> selectByIds(Set<Id> recordIds) {{")
        lines.append(f"{self._indent}{self._indent}String query = 'SELECT ' + getFieldListString() +")
        lines.append(f"{self._indent}{self._indent}{self._indent}' FROM {sobject} WHERE Id IN :recordIds';")
        lines.append(f"{self._indent}{self._indent}return Database.query(query);")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # selectAll
        lines.append(f"{self._indent}public List<{sobject}> selectAll(Integer limitCount) {{")
        lines.append(f"{self._indent}{self._indent}String query = 'SELECT ' + getFieldListString() +")
        lines.append(f"{self._indent}{self._indent}{self._indent}' FROM {sobject} ORDER BY CreatedDate DESC LIMIT :limitCount';")
        lines.append(f"{self._indent}{self._indent}return Database.query(query);")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # Custom queries
        if queries:
            for query_def in queries:
                lines.extend(self._generate_selector_query(sobject, query_def))

        lines.append("}")

        return "\n".join(lines)

    def generate_domain_class(
        self,
        class_name: str,
        sobject: str,
        validations: Optional[List[Dict[str, Any]]] = None,
        before_triggers: Optional[List[str]] = None,
        after_triggers: Optional[List[str]] = None
    ) -> str:
        """
        Gera uma classe Domain

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            validations: Regras de validacao
            before_triggers: Logica before trigger
            after_triggers: Logica after trigger

        Returns:
            Codigo Apex da classe
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, "Domain Layer para " + sobject))

        # Classe
        lines.append(f"public with sharing class {class_name} {{")
        lines.append("")

        # Records
        lines.append(f"{self._indent}private List<{sobject}> records;")
        lines.append(f"{self._indent}private Map<Id, {sobject}> oldMap;")
        lines.append("")

        # Constructor
        lines.append(f"{self._indent}public {class_name}(List<{sobject}> records) {{")
        lines.append(f"{self._indent}{self._indent}this(records, null);")
        lines.append(f"{self._indent}}}")
        lines.append("")

        lines.append(f"{self._indent}public {class_name}(List<{sobject}> records, Map<Id, {sobject}> oldMap) {{")
        lines.append(f"{self._indent}{self._indent}this.records = records;")
        lines.append(f"{self._indent}{self._indent}this.oldMap = oldMap;")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onBeforeInsert
        lines.append(f"{self._indent}public void onBeforeInsert() {{")
        lines.append(f"{self._indent}{self._indent}validate();")
        if before_triggers:
            for logic in before_triggers:
                lines.append(f"{self._indent}{self._indent}{logic};")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onBeforeUpdate
        lines.append(f"{self._indent}public void onBeforeUpdate() {{")
        lines.append(f"{self._indent}{self._indent}validate();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterInsert
        lines.append(f"{self._indent}public void onAfterInsert() {{")
        if after_triggers:
            for logic in after_triggers:
                lines.append(f"{self._indent}{self._indent}{logic};")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterUpdate
        lines.append(f"{self._indent}public void onAfterUpdate() {{")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # validate
        lines.append(f"{self._indent}private void validate() {{")
        lines.append(f"{self._indent}{self._indent}for ({sobject} record : records) {{")
        if validations:
            for validation in validations:
                field = validation.get("field", "Name")
                condition = validation.get("condition", "== null")
                message = validation.get("message", "Campo obrigatorio")
                lines.append(f"{self._indent}{self._indent}{self._indent}if (record.{field} {condition}) {{")
                lines.append(f"{self._indent}{self._indent}{self._indent}{self._indent}record.addError('{message}');")
                lines.append(f"{self._indent}{self._indent}{self._indent}}}")
        else:
            lines.append(f"{self._indent}{self._indent}{self._indent}// Adicionar validacoes aqui")
        lines.append(f"{self._indent}{self._indent}}}")
        lines.append(f"{self._indent}}}")
        lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def generate_trigger(
        self,
        trigger_name: str,
        sobject: str,
        events: List[str],
        handler_class: Optional[str] = None
    ) -> str:
        """
        Gera um trigger

        Args:
            trigger_name: Nome do trigger
            sobject: Objeto do trigger
            events: Lista de eventos (before insert, after update, etc)
            handler_class: Classe handler (opcional)

        Returns:
            Codigo do trigger
        """
        lines = []

        # Header
        lines.append(f"/**")
        lines.append(f" * Trigger: {trigger_name}")
        lines.append(f" * Objeto: {sobject}")
        lines.append(f" * Eventos: {', '.join(events)}")
        lines.append(f" * Gerado em: {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        lines.append(f" */")

        # Events
        event_clauses = []
        for event in events:
            parts = event.lower().split()
            if len(parts) == 2:
                event_clauses.append(f"{parts[0]} {parts[1]}")

        events_str = ", ".join(event_clauses)

        # Trigger
        lines.append(f"trigger {trigger_name} on {sobject} ({events_str}) {{")
        lines.append("")

        if handler_class:
            # Usar handler
            lines.append(f"{self._indent}{handler_class} handler = new {handler_class}();")
            lines.append("")
            lines.append(f"{self._indent}if (Trigger.isBefore) {{")
            lines.append(f"{self._indent}{self._indent}if (Trigger.isInsert) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onBeforeInsert(Trigger.new);")
            lines.append(f"{self._indent}{self._indent}}} else if (Trigger.isUpdate) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onBeforeUpdate(Trigger.new, Trigger.oldMap);")
            lines.append(f"{self._indent}{self._indent}}} else if (Trigger.isDelete) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onBeforeDelete(Trigger.old);")
            lines.append(f"{self._indent}{self._indent}}}")
            lines.append(f"{self._indent}}}")
            lines.append("")
            lines.append(f"{self._indent}if (Trigger.isAfter) {{")
            lines.append(f"{self._indent}{self._indent}if (Trigger.isInsert) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onAfterInsert(Trigger.new);")
            lines.append(f"{self._indent}{self._indent}}} else if (Trigger.isUpdate) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onAfterUpdate(Trigger.new, Trigger.oldMap);")
            lines.append(f"{self._indent}{self._indent}}} else if (Trigger.isDelete) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onAfterDelete(Trigger.old);")
            lines.append(f"{self._indent}{self._indent}}} else if (Trigger.isUndelete) {{")
            lines.append(f"{self._indent}{self._indent}{self._indent}handler.onAfterUndelete(Trigger.new);")
            lines.append(f"{self._indent}{self._indent}}}")
            lines.append(f"{self._indent}}}")
        else:
            lines.append(f"{self._indent}// Implementar logica do trigger aqui")
            lines.append(f"{self._indent}// Recomendado: criar uma classe handler")

        lines.append("")
        lines.append("}")

        return "\n".join(lines)

    def generate_trigger_handler(
        self,
        class_name: str,
        sobject: str
    ) -> str:
        """
        Gera classe handler de trigger

        Args:
            class_name: Nome da classe
            sobject: Objeto do trigger

        Returns:
            Codigo da classe handler
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, f"Handler de Trigger para {sobject}"))

        # Classe
        lines.append(f"public with sharing class {class_name} {{")
        lines.append("")

        # Domain reference
        domain_class = f"{sobject}Domain"

        # onBeforeInsert
        lines.append(f"{self._indent}public void onBeforeInsert(List<{sobject}> newRecords) {{")
        lines.append(f"{self._indent}{self._indent}new {domain_class}(newRecords).onBeforeInsert();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onBeforeUpdate
        lines.append(f"{self._indent}public void onBeforeUpdate(List<{sobject}> newRecords, Map<Id, {sobject}> oldMap) {{")
        lines.append(f"{self._indent}{self._indent}new {domain_class}(newRecords, oldMap).onBeforeUpdate();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onBeforeDelete
        lines.append(f"{self._indent}public void onBeforeDelete(List<{sobject}> oldRecords) {{")
        lines.append(f"{self._indent}{self._indent}// Implementar se necessario")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterInsert
        lines.append(f"{self._indent}public void onAfterInsert(List<{sobject}> newRecords) {{")
        lines.append(f"{self._indent}{self._indent}new {domain_class}(newRecords).onAfterInsert();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterUpdate
        lines.append(f"{self._indent}public void onAfterUpdate(List<{sobject}> newRecords, Map<Id, {sobject}> oldMap) {{")
        lines.append(f"{self._indent}{self._indent}new {domain_class}(newRecords, oldMap).onAfterUpdate();")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterDelete
        lines.append(f"{self._indent}public void onAfterDelete(List<{sobject}> oldRecords) {{")
        lines.append(f"{self._indent}{self._indent}// Implementar se necessario")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # onAfterUndelete
        lines.append(f"{self._indent}public void onAfterUndelete(List<{sobject}> newRecords) {{")
        lines.append(f"{self._indent}{self._indent}// Implementar se necessario")
        lines.append(f"{self._indent}}}")
        lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def generate_trigger_with_handler(
        self,
        sobject: str,
        events: List[str]
    ) -> Tuple[str, str]:
        """
        Gera trigger e handler juntos

        Args:
            sobject: Objeto do trigger
            events: Lista de eventos

        Returns:
            Tuple (trigger_code, handler_code)
        """
        trigger_name = f"{sobject}Trigger"
        handler_name = f"{sobject}TriggerHandler"

        trigger_code = self.generate_trigger(trigger_name, sobject, events, handler_name)
        handler_code = self.generate_trigger_handler(handler_name, sobject)

        return trigger_code, handler_code

    def generate_test_class(
        self,
        class_name: str,
        target_class: str,
        sobject: Optional[str] = None,
        test_methods: Optional[List[str]] = None
    ) -> str:
        """
        Gera classe de teste

        Args:
            class_name: Nome da classe de teste
            target_class: Classe sendo testada
            sobject: Objeto principal (para test data)
            test_methods: Nomes dos metodos de teste

        Returns:
            Codigo da classe de teste
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, f"Classe de Teste para {target_class}"))

        # Annotation e classe
        lines.append("@IsTest")
        lines.append(f"private class {class_name} {{")
        lines.append("")

        # Test data factory
        if sobject:
            lines.append(f"{self._indent}@TestSetup")
            lines.append(f"{self._indent}static void setup() {{")
            lines.append(f"{self._indent}{self._indent}// Criar dados de teste")
            lines.append(f"{self._indent}{self._indent}{sobject} testRecord = new {sobject}(")
            lines.append(f"{self._indent}{self._indent}{self._indent}Name = 'Test Record'")
            lines.append(f"{self._indent}{self._indent});")
            lines.append(f"{self._indent}{self._indent}insert testRecord;")
            lines.append(f"{self._indent}}}")
            lines.append("")

        # Test methods
        methods = test_methods or ["testPositiveScenario", "testNegativeScenario", "testBulkOperation"]

        for method_name in methods:
            lines.append(f"{self._indent}@IsTest")
            lines.append(f"{self._indent}static void {method_name}() {{")
            lines.append(f"{self._indent}{self._indent}// Arrange")
            if sobject:
                lines.append(f"{self._indent}{self._indent}{sobject} testRecord = [{sobject}].get(0);")
            lines.append("")
            lines.append(f"{self._indent}{self._indent}// Act")
            lines.append(f"{self._indent}{self._indent}Test.startTest();")
            lines.append(f"{self._indent}{self._indent}// Chamar metodo sendo testado")
            lines.append(f"{self._indent}{self._indent}Test.stopTest();")
            lines.append("")
            lines.append(f"{self._indent}{self._indent}// Assert")
            lines.append(f"{self._indent}{self._indent}System.assertNotEquals(null, testRecord.Id, 'Registro deve existir');")
            lines.append(f"{self._indent}}}")
            lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def generate_batch_class(
        self,
        class_name: str,
        sobject: str,
        query: Optional[str] = None,
        batch_size: int = 200
    ) -> str:
        """
        Gera classe Batch

        Args:
            class_name: Nome da classe
            sobject: Objeto processado
            query: Query para buscar registros
            batch_size: Tamanho do batch

        Returns:
            Codigo da classe batch
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, f"Batch para processar {sobject}"))

        # Classe
        lines.append(f"public class {class_name} implements Database.Batchable<sObject>, Database.Stateful {{")
        lines.append("")

        # Variables
        lines.append(f"{self._indent}private Integer recordsProcessed = 0;")
        lines.append(f"{self._indent}private List<String> errors = new List<String>();")
        lines.append("")

        # start
        default_query = query or f"SELECT Id, Name FROM {sobject}"
        lines.append(f"{self._indent}public Database.QueryLocator start(Database.BatchableContext bc) {{")
        lines.append(f"{self._indent}{self._indent}return Database.getQueryLocator('{default_query}');")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # execute
        lines.append(f"{self._indent}public void execute(Database.BatchableContext bc, List<{sobject}> scope) {{")
        lines.append(f"{self._indent}{self._indent}List<{sobject}> toUpdate = new List<{sobject}>();")
        lines.append("")
        lines.append(f"{self._indent}{self._indent}for ({sobject} record : scope) {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}try {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}{self._indent}// Processar registro")
        lines.append(f"{self._indent}{self._indent}{self._indent}{self._indent}toUpdate.add(record);")
        lines.append(f"{self._indent}{self._indent}{self._indent}{self._indent}recordsProcessed++;")
        lines.append(f"{self._indent}{self._indent}{self._indent}}} catch (Exception e) {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}{self._indent}errors.add(record.Id + ': ' + e.getMessage());")
        lines.append(f"{self._indent}{self._indent}{self._indent}}}")
        lines.append(f"{self._indent}{self._indent}}}")
        lines.append("")
        lines.append(f"{self._indent}{self._indent}if (!toUpdate.isEmpty()) {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}update toUpdate;")
        lines.append(f"{self._indent}{self._indent}}}")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # finish
        lines.append(f"{self._indent}public void finish(Database.BatchableContext bc) {{")
        lines.append(f"{self._indent}{self._indent}System.debug('Registros processados: ' + recordsProcessed);")
        lines.append(f"{self._indent}{self._indent}if (!errors.isEmpty()) {{")
        lines.append(f"{self._indent}{self._indent}{self._indent}System.debug('Erros: ' + String.join(errors, '\\n'));")
        lines.append(f"{self._indent}{self._indent}}}")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # execute method
        lines.append(f"{self._indent}public static Id execute() {{")
        lines.append(f"{self._indent}{self._indent}return Database.executeBatch(new {class_name}(), {batch_size});")
        lines.append(f"{self._indent}}}")
        lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def generate_rest_resource(
        self,
        class_name: str,
        url_mapping: str,
        sobject: Optional[str] = None,
        methods: Optional[List[str]] = None
    ) -> str:
        """
        Gera classe REST Resource

        Args:
            class_name: Nome da classe
            url_mapping: URL mapping (/services/apexrest/...)
            sobject: Objeto principal
            methods: Metodos HTTP a implementar

        Returns:
            Codigo da classe
        """
        lines = []

        # Header
        lines.append(self._generate_header(class_name, f"REST Resource: {url_mapping}"))

        # Annotation e classe
        lines.append(f"@RestResource(urlMapping='{url_mapping}/*')")
        lines.append(f"global with sharing class {class_name} {{")
        lines.append("")

        http_methods = methods or ["GET", "POST", "PUT", "DELETE"]

        # GET
        if "GET" in http_methods:
            lines.append(f"{self._indent}@HttpGet")
            lines.append(f"{self._indent}global static String doGet() {{")
            lines.append(f"{self._indent}{self._indent}RestRequest req = RestContext.request;")
            lines.append(f"{self._indent}{self._indent}String recordId = req.requestURI.substring(req.requestURI.lastIndexOf('/') + 1);")
            lines.append("")
            if sobject:
                lines.append(f"{self._indent}{self._indent}if (String.isNotBlank(recordId) && recordId != '{url_mapping.split('/')[-1]}') {{")
                lines.append(f"{self._indent}{self._indent}{self._indent}{sobject} record = [{sobject}].get(0);")
                lines.append(f"{self._indent}{self._indent}{self._indent}return JSON.serialize(record);")
                lines.append(f"{self._indent}{self._indent}}}")
                lines.append("")
                lines.append(f"{self._indent}{self._indent}List<{sobject}> records = [SELECT Id, Name FROM {sobject} LIMIT 100];")
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(records);")
            else:
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'status' => 'ok'}});")
            lines.append(f"{self._indent}}}")
            lines.append("")

        # POST
        if "POST" in http_methods:
            lines.append(f"{self._indent}@HttpPost")
            lines.append(f"{self._indent}global static String doPost() {{")
            lines.append(f"{self._indent}{self._indent}RestRequest req = RestContext.request;")
            lines.append(f"{self._indent}{self._indent}String requestBody = req.requestBody.toString();")
            lines.append("")
            if sobject:
                lines.append(f"{self._indent}{self._indent}{sobject} record = ({sobject}) JSON.deserialize(requestBody, {sobject}.class);")
                lines.append(f"{self._indent}{self._indent}insert record;")
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'id' => record.Id, 'success' => true}});")
            else:
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'success' => true}});")
            lines.append(f"{self._indent}}}")
            lines.append("")

        # PUT
        if "PUT" in http_methods:
            lines.append(f"{self._indent}@HttpPut")
            lines.append(f"{self._indent}global static String doPut() {{")
            lines.append(f"{self._indent}{self._indent}RestRequest req = RestContext.request;")
            lines.append(f"{self._indent}{self._indent}String requestBody = req.requestBody.toString();")
            lines.append("")
            if sobject:
                lines.append(f"{self._indent}{self._indent}{sobject} record = ({sobject}) JSON.deserialize(requestBody, {sobject}.class);")
                lines.append(f"{self._indent}{self._indent}update record;")
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'id' => record.Id, 'success' => true}});")
            else:
                lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'success' => true}});")
            lines.append(f"{self._indent}}}")
            lines.append("")

        # DELETE
        if "DELETE" in http_methods:
            lines.append(f"{self._indent}@HttpDelete")
            lines.append(f"{self._indent}global static String doDelete() {{")
            lines.append(f"{self._indent}{self._indent}RestRequest req = RestContext.request;")
            lines.append(f"{self._indent}{self._indent}String recordId = req.requestURI.substring(req.requestURI.lastIndexOf('/') + 1);")
            lines.append("")
            if sobject:
                lines.append(f"{self._indent}{self._indent}{sobject} record = [SELECT Id FROM {sobject} WHERE Id = :recordId];")
                lines.append(f"{self._indent}{self._indent}delete record;")
            lines.append(f"{self._indent}{self._indent}return JSON.serialize(new Map<String, Object>{{'success' => true}});")
            lines.append(f"{self._indent}}}")
            lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def generate_meta_xml(self, class_name: str) -> str:
        """
        Gera arquivo meta.xml para classe Apex

        Args:
            class_name: Nome da classe

        Returns:
            Conteudo do meta.xml
        """
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>{self.api_version}</apiVersion>
    <status>Active</status>
</ApexClass>"""

    def generate_trigger_meta_xml(self, trigger_name: str) -> str:
        """
        Gera arquivo meta.xml para trigger

        Args:
            trigger_name: Nome do trigger

        Returns:
            Conteudo do meta.xml
        """
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<ApexTrigger xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>{self.api_version}</apiVersion>
    <status>Active</status>
</ApexTrigger>"""

    # ==================== HELPERS ====================

    def _generate_header(self, name: str, description: str) -> str:
        """Gera header de classe"""
        return f"""/**
 * {name}
 * {description}
 *
 * @author Fabrica de Agentes
 * @date {datetime.now().strftime('%Y-%m-%d')}
 */
"""

    def _generate_crud_methods(self, sobject: str) -> List[str]:
        """Gera metodos CRUD basicos"""
        lines = []

        # Create
        lines.append(f"{self._indent}public {sobject} create({sobject} record) {{")
        lines.append(f"{self._indent}{self._indent}insert record;")
        lines.append(f"{self._indent}{self._indent}return record;")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # Get by ID
        lines.append(f"{self._indent}public {sobject} getById(Id recordId) {{")
        lines.append(f"{self._indent}{self._indent}return selector.selectById(recordId);")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # Update
        lines.append(f"{self._indent}public {sobject} updateRecord({sobject} record) {{")
        lines.append(f"{self._indent}{self._indent}update record;")
        lines.append(f"{self._indent}{self._indent}return record;")
        lines.append(f"{self._indent}}}")
        lines.append("")

        # Delete
        lines.append(f"{self._indent}public void deleteRecord(Id recordId) {{")
        lines.append(f"{self._indent}{self._indent}{sobject} record = new {sobject}(Id = recordId);")
        lines.append(f"{self._indent}{self._indent}delete record;")
        lines.append(f"{self._indent}}}")
        lines.append("")

        return lines

    def _generate_method(self, method: ApexMethod) -> List[str]:
        """Gera um metodo"""
        lines = []

        # Annotations
        for annotation in method.annotations:
            lines.append(f"{self._indent}@{annotation}")

        # Signature
        modifiers = " ".join(method.modifiers)
        params = ", ".join([f"{t} {n}" for t, n in method.parameters])

        lines.append(f"{self._indent}{modifiers} {method.return_type} {method.name}({params}) {{")

        # Body
        if method.body:
            for line in method.body.split('\n'):
                lines.append(f"{self._indent}{self._indent}{line}")
        else:
            lines.append(f"{self._indent}{self._indent}// TODO: Implementar")

        lines.append(f"{self._indent}}}")
        lines.append("")

        return lines

    def _generate_selector_query(
        self,
        sobject: str,
        query_def: Dict[str, Any]
    ) -> List[str]:
        """Gera metodo de query customizado"""
        lines = []

        method_name = query_def.get("name", "selectCustom")
        params = query_def.get("parameters", [])
        where_clause = query_def.get("where", "")
        order_by = query_def.get("order_by", "")

        params_str = ", ".join([f"{p['type']} {p['name']}" for p in params])

        lines.append(f"{self._indent}public List<{sobject}> {method_name}({params_str}) {{")
        lines.append(f"{self._indent}{self._indent}String query = 'SELECT ' + getFieldListString() +")
        lines.append(f"{self._indent}{self._indent}{self._indent}' FROM {sobject}'")

        if where_clause:
            lines.append(f"{self._indent}{self._indent}{self._indent}+ ' WHERE {where_clause}'")

        if order_by:
            lines.append(f"{self._indent}{self._indent}{self._indent}+ ' ORDER BY {order_by}'")

        lines.append(f"{self._indent}{self._indent};")
        lines.append(f"{self._indent}{self._indent}return Database.query(query);")
        lines.append(f"{self._indent}}}")
        lines.append("")

        return lines
