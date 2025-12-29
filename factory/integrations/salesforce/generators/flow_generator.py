# -*- coding: utf-8 -*-
"""
Salesforce Flow Generator
=========================
Gerador de Flows do Salesforce.

Funcionalidades:
- Geracao de Record-Triggered Flows
- Geracao de Screen Flows
- Geracao de Scheduled Flows
- Templates para casos comuns

Exemplo de uso:
    from factory.integrations.salesforce.generators import FlowGenerator

    generator = FlowGenerator()

    # Gerar record-triggered flow
    flow_xml = generator.generate_record_triggered_flow(
        "Account_Update_Flow",
        "Account",
        "afterSave",
        actions=[...]
    )
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import xml.etree.ElementTree as ET
from xml.dom import minidom

logger = logging.getLogger(__name__)


class FlowTriggerType(str, Enum):
    """Tipos de trigger para flows"""
    RECORD_BEFORE_SAVE = "RecordBeforeSave"
    RECORD_AFTER_SAVE = "RecordAfterSave"
    SCHEDULED = "Scheduled"
    PLATFORM_EVENT = "PlatformEvent"


class FlowRecordTriggerType(str, Enum):
    """Tipos de operacao que disparam o flow"""
    CREATE = "Create"
    UPDATE = "Update"
    CREATE_AND_UPDATE = "CreateAndUpdate"
    DELETE = "Delete"


@dataclass
class FlowDecision:
    """Definicao de uma decisao no flow"""
    name: str
    label: str
    default_connector: str
    rules: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class FlowRecordUpdate:
    """Definicao de uma atualizacao de registro"""
    name: str
    label: str
    object_name: str
    filters: List[Dict[str, str]] = field(default_factory=list)
    field_assignments: List[Dict[str, str]] = field(default_factory=list)
    connector: Optional[str] = None


@dataclass
class FlowRecordCreate:
    """Definicao de uma criacao de registro"""
    name: str
    label: str
    object_name: str
    field_assignments: List[Dict[str, str]] = field(default_factory=list)
    connector: Optional[str] = None
    store_output_variable: Optional[str] = None


class FlowGenerator:
    """
    Gerador de Flows Salesforce

    Gera definicoes de flow em formato XML para deploy.
    """

    def __init__(self, api_version: str = "59.0"):
        """
        Inicializa o gerador

        Args:
            api_version: Versao da API Salesforce
        """
        self.api_version = api_version
        self.ns = "http://soap.sforce.com/2006/04/metadata"

    def generate_record_triggered_flow(
        self,
        flow_name: str,
        sobject: str,
        trigger_type: str = "afterSave",
        record_trigger_type: str = "CreateAndUpdate",
        entry_conditions: Optional[List[Dict[str, str]]] = None,
        actions: Optional[List[Dict[str, Any]]] = None,
        description: str = ""
    ) -> str:
        """
        Gera um Record-Triggered Flow

        Args:
            flow_name: Nome do flow
            sobject: Objeto que dispara o flow
            trigger_type: beforeSave ou afterSave
            record_trigger_type: Create, Update, CreateAndUpdate, Delete
            entry_conditions: Condicoes de entrada
            actions: Acoes do flow
            description: Descricao

        Returns:
            XML do flow
        """
        root = ET.Element("Flow", xmlns=self.ns)

        # Informacoes basicas
        ET.SubElement(root, "apiVersion").text = self.api_version
        if description:
            ET.SubElement(root, "description").text = description
        ET.SubElement(root, "label").text = flow_name.replace("_", " ")
        ET.SubElement(root, "processType").text = "AutoLaunchedFlow"
        ET.SubElement(root, "status").text = "Draft"

        # Start element
        start = ET.SubElement(root, "start")
        ET.SubElement(start, "locationX").text = "50"
        ET.SubElement(start, "locationY").text = "0"
        ET.SubElement(start, "object").text = sobject

        # Trigger type
        if trigger_type.lower() == "beforesave":
            ET.SubElement(start, "triggerType").text = FlowTriggerType.RECORD_BEFORE_SAVE.value
        else:
            ET.SubElement(start, "triggerType").text = FlowTriggerType.RECORD_AFTER_SAVE.value

        # Record trigger type
        trigger_map = {
            "create": FlowRecordTriggerType.CREATE.value,
            "update": FlowRecordTriggerType.UPDATE.value,
            "createandupdate": FlowRecordTriggerType.CREATE_AND_UPDATE.value,
            "delete": FlowRecordTriggerType.DELETE.value
        }
        ET.SubElement(start, "recordTriggerType").text = trigger_map.get(
            record_trigger_type.lower(),
            FlowRecordTriggerType.CREATE_AND_UPDATE.value
        )

        # Entry conditions
        if entry_conditions:
            filter_logic = "and"
            for i, condition in enumerate(entry_conditions, 1):
                entry_cond = ET.SubElement(start, "filterFormula")
                # Simplificado - usar formula
                conditions = []
                for cond in entry_conditions:
                    field = cond.get("field", "Id")
                    operator = cond.get("operator", "EqualTo")
                    value = cond.get("value", "")
                    conditions.append(f"$Record.{field} {self._get_operator_symbol(operator)} '{value}'")
                entry_cond.text = " && ".join(conditions)

        # Connector para primeira acao
        if actions:
            connector = ET.SubElement(start, "connector")
            ET.SubElement(connector, "targetReference").text = actions[0].get("name", "Action_1")

        # Adicionar acoes
        y_pos = 150
        for i, action in enumerate(actions or []):
            self._add_action(root, action, y_pos)
            y_pos += 150

        return self._prettify_xml(root)

    def generate_screen_flow(
        self,
        flow_name: str,
        screens: List[Dict[str, Any]],
        variables: Optional[List[Dict[str, str]]] = None,
        description: str = ""
    ) -> str:
        """
        Gera um Screen Flow

        Args:
            flow_name: Nome do flow
            screens: Lista de telas
            variables: Variaveis do flow
            description: Descricao

        Returns:
            XML do flow
        """
        root = ET.Element("Flow", xmlns=self.ns)

        # Informacoes basicas
        ET.SubElement(root, "apiVersion").text = self.api_version
        if description:
            ET.SubElement(root, "description").text = description
        ET.SubElement(root, "label").text = flow_name.replace("_", " ")
        ET.SubElement(root, "processType").text = "Flow"
        ET.SubElement(root, "status").text = "Draft"

        # Variaveis
        for var in (variables or []):
            variable = ET.SubElement(root, "variables")
            ET.SubElement(variable, "name").text = var.get("name", "var1")
            ET.SubElement(variable, "dataType").text = var.get("type", "String")
            ET.SubElement(variable, "isCollection").text = str(var.get("isCollection", False)).lower()
            ET.SubElement(variable, "isInput").text = str(var.get("isInput", False)).lower()
            ET.SubElement(variable, "isOutput").text = str(var.get("isOutput", False)).lower()

        # Start
        start = ET.SubElement(root, "start")
        ET.SubElement(start, "locationX").text = "50"
        ET.SubElement(start, "locationY").text = "0"

        if screens:
            connector = ET.SubElement(start, "connector")
            ET.SubElement(connector, "targetReference").text = screens[0].get("name", "Screen_1")

        # Adicionar telas
        y_pos = 150
        for i, screen_def in enumerate(screens):
            self._add_screen(root, screen_def, y_pos, i, len(screens))
            y_pos += 200

        return self._prettify_xml(root)

    def generate_scheduled_flow(
        self,
        flow_name: str,
        sobject: str,
        schedule: Dict[str, Any],
        actions: Optional[List[Dict[str, Any]]] = None,
        description: str = ""
    ) -> str:
        """
        Gera um Scheduled Flow

        Args:
            flow_name: Nome do flow
            sobject: Objeto a processar
            schedule: Configuracao de agendamento
            actions: Acoes do flow
            description: Descricao

        Returns:
            XML do flow
        """
        root = ET.Element("Flow", xmlns=self.ns)

        # Informacoes basicas
        ET.SubElement(root, "apiVersion").text = self.api_version
        if description:
            ET.SubElement(root, "description").text = description
        ET.SubElement(root, "label").text = flow_name.replace("_", " ")
        ET.SubElement(root, "processType").text = "AutoLaunchedFlow"
        ET.SubElement(root, "status").text = "Draft"

        # Start com schedule
        start = ET.SubElement(root, "start")
        ET.SubElement(start, "locationX").text = "50"
        ET.SubElement(start, "locationY").text = "0"
        ET.SubElement(start, "object").text = sobject
        ET.SubElement(start, "triggerType").text = "Scheduled"

        # Scheduled paths
        schedule_config = ET.SubElement(start, "scheduledPaths")
        ET.SubElement(schedule_config, "name").text = "Run_Schedule"

        # Frequency
        frequency = schedule.get("frequency", "Daily")
        ET.SubElement(schedule_config, "frequency").text = frequency

        if actions:
            connector = ET.SubElement(schedule_config, "connector")
            ET.SubElement(connector, "targetReference").text = actions[0].get("name", "Action_1")

        # Adicionar acoes
        y_pos = 150
        for action in (actions or []):
            self._add_action(root, action, y_pos)
            y_pos += 150

        return self._prettify_xml(root)

    def _add_action(self, root: ET.Element, action: Dict[str, Any], y_pos: int):
        """Adiciona uma acao ao flow"""
        action_type = action.get("type", "recordUpdate")

        if action_type == "recordUpdate":
            elem = ET.SubElement(root, "recordUpdates")
            ET.SubElement(elem, "name").text = action.get("name", "Update_Record")
            ET.SubElement(elem, "label").text = action.get("label", "Atualizar Registro")
            ET.SubElement(elem, "locationX").text = "176"
            ET.SubElement(elem, "locationY").text = str(y_pos)

            # Input reference
            ET.SubElement(elem, "inputReference").text = action.get("inputReference", "$Record")

            # Field assignments
            for assignment in action.get("fieldAssignments", []):
                input_assign = ET.SubElement(elem, "inputAssignments")
                ET.SubElement(input_assign, "field").text = assignment.get("field", "")
                value = ET.SubElement(input_assign, "value")
                ET.SubElement(value, "stringValue").text = assignment.get("value", "")

            # Connector
            if action.get("connector"):
                connector = ET.SubElement(elem, "connector")
                ET.SubElement(connector, "targetReference").text = action["connector"]

        elif action_type == "recordCreate":
            elem = ET.SubElement(root, "recordCreates")
            ET.SubElement(elem, "name").text = action.get("name", "Create_Record")
            ET.SubElement(elem, "label").text = action.get("label", "Criar Registro")
            ET.SubElement(elem, "locationX").text = "176"
            ET.SubElement(elem, "locationY").text = str(y_pos)
            ET.SubElement(elem, "object").text = action.get("object", "")

            for assignment in action.get("fieldAssignments", []):
                input_assign = ET.SubElement(elem, "inputAssignments")
                ET.SubElement(input_assign, "field").text = assignment.get("field", "")
                value = ET.SubElement(input_assign, "value")
                ET.SubElement(value, "stringValue").text = assignment.get("value", "")

        elif action_type == "decision":
            elem = ET.SubElement(root, "decisions")
            ET.SubElement(elem, "name").text = action.get("name", "Decision_1")
            ET.SubElement(elem, "label").text = action.get("label", "Decisao")
            ET.SubElement(elem, "locationX").text = "176"
            ET.SubElement(elem, "locationY").text = str(y_pos)

            # Default connector
            default_connector = ET.SubElement(elem, "defaultConnector")
            ET.SubElement(default_connector, "targetReference").text = action.get("defaultConnector", "")
            ET.SubElement(elem, "defaultConnectorLabel").text = "Padrao"

            # Rules
            for rule in action.get("rules", []):
                rule_elem = ET.SubElement(elem, "rules")
                ET.SubElement(rule_elem, "name").text = rule.get("name", "Rule_1")
                ET.SubElement(rule_elem, "conditionLogic").text = "and"
                ET.SubElement(rule_elem, "label").text = rule.get("label", "Regra")

                for condition in rule.get("conditions", []):
                    cond_elem = ET.SubElement(rule_elem, "conditions")
                    ET.SubElement(cond_elem, "leftValueReference").text = condition.get("field", "")
                    ET.SubElement(cond_elem, "operator").text = condition.get("operator", "EqualTo")
                    right_value = ET.SubElement(cond_elem, "rightValue")
                    ET.SubElement(right_value, "stringValue").text = condition.get("value", "")

                if rule.get("connector"):
                    connector = ET.SubElement(rule_elem, "connector")
                    ET.SubElement(connector, "targetReference").text = rule["connector"]

    def _add_screen(
        self,
        root: ET.Element,
        screen_def: Dict[str, Any],
        y_pos: int,
        index: int,
        total: int
    ):
        """Adiciona uma tela ao flow"""
        screen = ET.SubElement(root, "screens")
        screen_name = screen_def.get("name", f"Screen_{index + 1}")

        ET.SubElement(screen, "name").text = screen_name
        ET.SubElement(screen, "label").text = screen_def.get("label", f"Tela {index + 1}")
        ET.SubElement(screen, "locationX").text = "176"
        ET.SubElement(screen, "locationY").text = str(y_pos)
        ET.SubElement(screen, "allowBack").text = "true"
        ET.SubElement(screen, "allowFinish").text = "true"
        ET.SubElement(screen, "allowPause").text = "true"
        ET.SubElement(screen, "showFooter").text = "true"
        ET.SubElement(screen, "showHeader").text = "true"

        # Campos da tela
        for field in screen_def.get("fields", []):
            field_elem = ET.SubElement(screen, "fields")
            ET.SubElement(field_elem, "name").text = field.get("name", "Field_1")
            ET.SubElement(field_elem, "fieldType").text = field.get("type", "InputField")
            ET.SubElement(field_elem, "dataType").text = field.get("dataType", "String")
            ET.SubElement(field_elem, "isRequired").text = str(field.get("required", False)).lower()

            field_text = ET.SubElement(field_elem, "fieldText")
            field_text.text = field.get("label", "Campo")

        # Connector para proxima tela
        if index < total - 1:
            next_screen = screen_def.get("nextScreen")
            if next_screen:
                connector = ET.SubElement(screen, "connector")
                ET.SubElement(connector, "targetReference").text = next_screen

    def _get_operator_symbol(self, operator: str) -> str:
        """Converte operador para simbolo"""
        operators = {
            "EqualTo": "==",
            "NotEqualTo": "!=",
            "GreaterThan": ">",
            "LessThan": "<",
            "GreaterThanOrEqualTo": ">=",
            "LessThanOrEqualTo": "<=",
            "Contains": "CONTAINS",
            "StartsWith": "BEGINS"
        }
        return operators.get(operator, "==")

    def _prettify_xml(self, elem: ET.Element) -> str:
        """Formata XML de forma legivel"""
        rough_string = ET.tostring(elem, encoding='unicode')
        reparsed = minidom.parseString(rough_string)
        return reparsed.toprettyxml(indent="    ")
