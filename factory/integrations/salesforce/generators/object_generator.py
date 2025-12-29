# -*- coding: utf-8 -*-
"""
Salesforce Object Generator
===========================
Gerador de objetos e campos customizados do Salesforce.

Funcionalidades:
- Geracao de objetos customizados
- Geracao de campos customizados
- Geracao de relacionamentos
- Geracao de validation rules
- Templates para casos comuns

Exemplo de uso:
    from factory.integrations.salesforce.generators import ObjectGenerator

    generator = ObjectGenerator()

    # Gerar objeto customizado
    obj_meta = generator.generate_custom_object(
        "Projeto__c",
        "Projeto",
        fields=[
            {"name": "Codigo__c", "type": "Text", "length": 20},
            {"name": "DataInicio__c", "type": "Date"},
        ]
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


class FieldType(str, Enum):
    """Tipos de campos Salesforce"""
    AUTO_NUMBER = "AutoNumber"
    CHECKBOX = "Checkbox"
    CURRENCY = "Currency"
    DATE = "Date"
    DATETIME = "DateTime"
    EMAIL = "Email"
    ENCRYPTED_TEXT = "EncryptedText"
    EXTERNAL_LOOKUP = "ExternalLookup"
    FORMULA = "Formula"
    GEOLOCATION = "Location"
    HIERARCHY = "Hierarchy"
    HTML = "Html"
    INDIRECT_LOOKUP = "IndirectLookup"
    LONG_TEXT_AREA = "LongTextArea"
    LOOKUP = "Lookup"
    MASTER_DETAIL = "MasterDetail"
    MULTI_SELECT_PICKLIST = "MultiselectPicklist"
    NUMBER = "Number"
    PERCENT = "Percent"
    PHONE = "Phone"
    PICKLIST = "Picklist"
    RICH_TEXT_AREA = "Html"
    ROLLUP_SUMMARY = "Summary"
    TEXT = "Text"
    TEXT_AREA = "TextArea"
    TIME = "Time"
    URL = "Url"


class SharingModel(str, Enum):
    """Modelos de compartilhamento"""
    READ_WRITE = "ReadWrite"
    READ = "Read"
    PRIVATE = "Private"
    CONTROLLED_BY_PARENT = "ControlledByParent"


@dataclass
class FieldDefinition:
    """Definicao de um campo customizado"""
    name: str
    label: str
    type: FieldType
    description: str = ""
    help_text: str = ""
    required: bool = False
    unique: bool = False
    external_id: bool = False

    # Para Text
    length: int = 255

    # Para Number, Currency, Percent
    precision: int = 18
    scale: int = 2

    # Para Picklist
    picklist_values: List[str] = field(default_factory=list)
    restricted: bool = False

    # Para LongTextArea
    visible_lines: int = 3

    # Para Lookup/MasterDetail
    reference_to: str = ""
    relationship_label: str = ""
    relationship_name: str = ""
    delete_constraint: str = "SetNull"

    # Para Formula
    formula: str = ""
    formula_return_type: str = "Text"

    # Para Rollup Summary
    summary_operation: str = "count"
    summary_foreign_key: str = ""
    summary_field: str = ""

    # Default value
    default_value: Optional[str] = None


@dataclass
class ValidationRuleDefinition:
    """Definicao de uma validation rule"""
    name: str
    description: str
    error_condition_formula: str
    error_message: str
    error_display_field: Optional[str] = None
    active: bool = True


class ObjectGenerator:
    """
    Gerador de objetos Salesforce

    Gera definicoes de objetos customizados e campos
    em formato XML para deploy via Metadata API.
    """

    def __init__(self, api_version: str = "59.0"):
        """
        Inicializa o gerador

        Args:
            api_version: Versao da API Salesforce
        """
        self.api_version = api_version
        self.ns = "http://soap.sforce.com/2006/04/metadata"

    def generate_custom_object(
        self,
        api_name: str,
        label: str,
        label_plural: str = "",
        description: str = "",
        name_field_type: str = "Text",
        name_field_label: str = "Nome",
        sharing_model: SharingModel = SharingModel.READ_WRITE,
        enable_activities: bool = True,
        enable_reports: bool = True,
        enable_search: bool = True,
        enable_feeds: bool = False,
        fields: Optional[List[Dict[str, Any]]] = None,
        validation_rules: Optional[List[Dict[str, Any]]] = None
    ) -> str:
        """
        Gera objeto customizado completo

        Args:
            api_name: Nome API do objeto (deve terminar em __c)
            label: Label singular
            label_plural: Label plural
            description: Descricao
            name_field_type: Tipo do campo Name (Text ou AutoNumber)
            name_field_label: Label do campo Name
            sharing_model: Modelo de compartilhamento
            enable_*: Features habilitadas
            fields: Lista de campos
            validation_rules: Lista de validation rules

        Returns:
            XML do objeto
        """
        if not api_name.endswith("__c"):
            api_name += "__c"

        root = ET.Element("CustomObject", xmlns=self.ns)

        # Informacoes basicas
        ET.SubElement(root, "label").text = label
        ET.SubElement(root, "pluralLabel").text = label_plural or f"{label}s"
        if description:
            ET.SubElement(root, "description").text = description

        # Deployment status
        ET.SubElement(root, "deploymentStatus").text = "Deployed"

        # Features
        ET.SubElement(root, "enableActivities").text = str(enable_activities).lower()
        ET.SubElement(root, "enableReports").text = str(enable_reports).lower()
        ET.SubElement(root, "enableSearch").text = str(enable_search).lower()
        ET.SubElement(root, "enableFeeds").text = str(enable_feeds).lower()

        # Sharing model
        ET.SubElement(root, "sharingModel").text = sharing_model.value

        # Name field
        name_field = ET.SubElement(root, "nameField")
        ET.SubElement(name_field, "label").text = name_field_label
        ET.SubElement(name_field, "type").text = name_field_type

        if name_field_type == "AutoNumber":
            prefix = api_name[:3].upper().replace("_", "")
            ET.SubElement(name_field, "displayFormat").text = f"{prefix}-{{00000}}"

        # Adicionar campos
        for field_def in (fields or []):
            self._add_field(root, field_def)

        # Adicionar validation rules
        for rule_def in (validation_rules or []):
            self._add_validation_rule(root, rule_def)

        return self._prettify_xml(root)

    def generate_custom_field(
        self,
        object_name: str,
        field_def: Dict[str, Any]
    ) -> str:
        """
        Gera campo customizado individual

        Args:
            object_name: Nome do objeto
            field_def: Definicao do campo

        Returns:
            XML do campo
        """
        root = ET.Element("CustomField", xmlns=self.ns)

        field_name = field_def.get("name", "Campo__c")
        if not field_name.endswith("__c"):
            field_name += "__c"

        ET.SubElement(root, "fullName").text = f"{object_name}.{field_name}"
        ET.SubElement(root, "label").text = field_def.get("label", field_name.replace("__c", ""))
        ET.SubElement(root, "type").text = field_def.get("type", "Text")

        self._add_field_attributes(root, field_def)

        return self._prettify_xml(root)

    def generate_validation_rule(
        self,
        object_name: str,
        rule_def: Dict[str, Any]
    ) -> str:
        """
        Gera validation rule individual

        Args:
            object_name: Nome do objeto
            rule_def: Definicao da regra

        Returns:
            XML da regra
        """
        root = ET.Element("ValidationRule", xmlns=self.ns)

        rule_name = rule_def.get("name", "Rule_1")
        ET.SubElement(root, "fullName").text = f"{object_name}.{rule_name}"
        ET.SubElement(root, "active").text = str(rule_def.get("active", True)).lower()

        if rule_def.get("description"):
            ET.SubElement(root, "description").text = rule_def["description"]

        ET.SubElement(root, "errorConditionFormula").text = rule_def.get("formula", "false")
        ET.SubElement(root, "errorMessage").text = rule_def.get("message", "Erro de validacao")

        if rule_def.get("errorField"):
            ET.SubElement(root, "errorDisplayField").text = rule_def["errorField"]

        return self._prettify_xml(root)

    def generate_lookup_field(
        self,
        object_name: str,
        field_name: str,
        reference_to: str,
        label: str = "",
        relationship_name: str = "",
        required: bool = False,
        delete_constraint: str = "SetNull"
    ) -> str:
        """
        Gera campo de lookup

        Args:
            object_name: Objeto onde criar o campo
            field_name: Nome do campo
            reference_to: Objeto referenciado
            label: Label do campo
            relationship_name: Nome do relacionamento
            required: Se e obrigatorio
            delete_constraint: SetNull ou Restrict

        Returns:
            XML do campo
        """
        if not field_name.endswith("__c"):
            field_name += "__c"

        field_def = {
            "name": field_name,
            "label": label or field_name.replace("__c", "").replace("_", " "),
            "type": "Lookup",
            "referenceTo": reference_to,
            "relationshipName": relationship_name or reference_to.replace("__c", "") + "s",
            "relationshipLabel": label or field_name.replace("__c", "").replace("_", " ") + "s",
            "required": required,
            "deleteConstraint": delete_constraint
        }

        return self.generate_custom_field(object_name, field_def)

    def generate_master_detail_field(
        self,
        object_name: str,
        field_name: str,
        reference_to: str,
        label: str = "",
        relationship_name: str = "",
        reparentable: bool = False
    ) -> str:
        """
        Gera campo Master-Detail

        Args:
            object_name: Objeto filho (onde criar o campo)
            field_name: Nome do campo
            reference_to: Objeto pai
            label: Label do campo
            relationship_name: Nome do relacionamento
            reparentable: Se permite reparent

        Returns:
            XML do campo
        """
        if not field_name.endswith("__c"):
            field_name += "__c"

        field_def = {
            "name": field_name,
            "label": label or field_name.replace("__c", "").replace("_", " "),
            "type": "MasterDetail",
            "referenceTo": reference_to,
            "relationshipName": relationship_name or object_name.replace("__c", "") + "s",
            "relationshipLabel": label or object_name.replace("__c", "").replace("_", " ") + "s",
            "reparentable": reparentable
        }

        return self.generate_custom_field(object_name, field_def)

    def generate_picklist_field(
        self,
        object_name: str,
        field_name: str,
        values: List[str],
        label: str = "",
        required: bool = False,
        restricted: bool = False,
        default_value: Optional[str] = None
    ) -> str:
        """
        Gera campo picklist

        Args:
            object_name: Nome do objeto
            field_name: Nome do campo
            values: Lista de valores
            label: Label do campo
            required: Se e obrigatorio
            restricted: Se e restricted picklist
            default_value: Valor padrao

        Returns:
            XML do campo
        """
        if not field_name.endswith("__c"):
            field_name += "__c"

        field_def = {
            "name": field_name,
            "label": label or field_name.replace("__c", "").replace("_", " "),
            "type": "Picklist",
            "picklistValues": values,
            "required": required,
            "restricted": restricted,
            "defaultValue": default_value
        }

        return self.generate_custom_field(object_name, field_def)

    def generate_formula_field(
        self,
        object_name: str,
        field_name: str,
        formula: str,
        return_type: str = "Text",
        label: str = "",
        description: str = ""
    ) -> str:
        """
        Gera campo formula

        Args:
            object_name: Nome do objeto
            field_name: Nome do campo
            formula: Formula Salesforce
            return_type: Tipo de retorno (Text, Number, Date, etc)
            label: Label do campo
            description: Descricao

        Returns:
            XML do campo
        """
        if not field_name.endswith("__c"):
            field_name += "__c"

        field_def = {
            "name": field_name,
            "label": label or field_name.replace("__c", "").replace("_", " "),
            "type": "Formula",
            "formula": formula,
            "formulaTreatBlanksAs": "BlankAsZero",
            "returnType": return_type,
            "description": description
        }

        return self.generate_custom_field(object_name, field_def)

    def _add_field(self, root: ET.Element, field_def: Dict[str, Any]):
        """Adiciona campo ao objeto"""
        field = ET.SubElement(root, "fields")

        field_name = field_def.get("name", "Campo__c")
        if not field_name.endswith("__c"):
            field_name += "__c"

        ET.SubElement(field, "fullName").text = field_name
        ET.SubElement(field, "label").text = field_def.get("label", field_name.replace("__c", ""))
        ET.SubElement(field, "type").text = field_def.get("type", "Text")

        self._add_field_attributes(field, field_def)

    def _add_field_attributes(self, field_elem: ET.Element, field_def: Dict[str, Any]):
        """Adiciona atributos especificos do tipo de campo"""
        field_type = field_def.get("type", "Text")

        # Descricao e help text
        if field_def.get("description"):
            ET.SubElement(field_elem, "description").text = field_def["description"]
        if field_def.get("helpText"):
            ET.SubElement(field_elem, "inlineHelpText").text = field_def["helpText"]

        # Required
        if field_def.get("required"):
            ET.SubElement(field_elem, "required").text = "true"
        else:
            ET.SubElement(field_elem, "required").text = "false"

        # Unique
        if field_def.get("unique"):
            ET.SubElement(field_elem, "unique").text = "true"

        # External ID
        if field_def.get("externalId"):
            ET.SubElement(field_elem, "externalId").text = "true"

        # Atributos por tipo
        if field_type == "Text":
            ET.SubElement(field_elem, "length").text = str(field_def.get("length", 255))

        elif field_type in ("Number", "Currency", "Percent"):
            ET.SubElement(field_elem, "precision").text = str(field_def.get("precision", 18))
            ET.SubElement(field_elem, "scale").text = str(field_def.get("scale", 2))

        elif field_type in ("LongTextArea", "Html"):
            ET.SubElement(field_elem, "length").text = str(field_def.get("length", 32768))
            ET.SubElement(field_elem, "visibleLines").text = str(field_def.get("visibleLines", 3))

        elif field_type == "TextArea":
            # TextArea nao precisa de length

            pass

        elif field_type in ("Picklist", "MultiselectPicklist"):
            picklist = ET.SubElement(field_elem, "picklist")
            ET.SubElement(picklist, "restrictedPicklist").text = str(field_def.get("restricted", False)).lower()

            for value in field_def.get("picklistValues", []):
                value_elem = ET.SubElement(picklist, "picklistValues")
                ET.SubElement(value_elem, "fullName").text = value
                ET.SubElement(value_elem, "default").text = "false"

            ET.SubElement(picklist, "sorted").text = "false"

        elif field_type in ("Lookup", "MasterDetail"):
            ET.SubElement(field_elem, "referenceTo").text = field_def.get("referenceTo", "")
            ET.SubElement(field_elem, "relationshipName").text = field_def.get("relationshipName", "")
            ET.SubElement(field_elem, "relationshipLabel").text = field_def.get("relationshipLabel", "")

            if field_type == "Lookup":
                ET.SubElement(field_elem, "deleteConstraint").text = field_def.get("deleteConstraint", "SetNull")
            else:
                if field_def.get("reparentable"):
                    ET.SubElement(field_elem, "reparentableMasterDetail").text = "true"

        elif field_type == "Formula":
            ET.SubElement(field_elem, "formula").text = field_def.get("formula", "")
            ET.SubElement(field_elem, "formulaTreatBlanksAs").text = field_def.get("formulaTreatBlanksAs", "BlankAsZero")

        elif field_type == "Checkbox":
            default = field_def.get("defaultValue", "false")
            ET.SubElement(field_elem, "defaultValue").text = str(default).lower()

        # Default value para outros tipos
        if field_def.get("defaultValue") and field_type not in ("Checkbox", "Formula"):
            ET.SubElement(field_elem, "defaultValue").text = str(field_def["defaultValue"])

    def _add_validation_rule(self, root: ET.Element, rule_def: Dict[str, Any]):
        """Adiciona validation rule ao objeto"""
        rule = ET.SubElement(root, "validationRules")

        ET.SubElement(rule, "fullName").text = rule_def.get("name", "Rule_1")
        ET.SubElement(rule, "active").text = str(rule_def.get("active", True)).lower()

        if rule_def.get("description"):
            ET.SubElement(rule, "description").text = rule_def["description"]

        ET.SubElement(rule, "errorConditionFormula").text = rule_def.get("formula", "false")
        ET.SubElement(rule, "errorMessage").text = rule_def.get("message", "Erro de validacao")

        if rule_def.get("errorField"):
            ET.SubElement(rule, "errorDisplayField").text = rule_def["errorField"]

    def _prettify_xml(self, elem: ET.Element) -> str:
        """Formata XML de forma legivel"""
        rough_string = ET.tostring(elem, encoding='unicode')
        reparsed = minidom.parseString(rough_string)
        return reparsed.toprettyxml(indent="    ")

    # ==================== TEMPLATES ====================

    def generate_project_object(
        self,
        api_name: str = "Projeto__c"
    ) -> str:
        """
        Gera objeto de Projeto com campos comuns

        Args:
            api_name: Nome API do objeto

        Returns:
            XML do objeto
        """
        fields = [
            {"name": "Codigo__c", "type": "Text", "length": 20, "unique": True, "externalId": True},
            {"name": "Descricao__c", "type": "LongTextArea", "length": 5000},
            {"name": "Status__c", "type": "Picklist", "picklistValues": ["Planejamento", "Em Andamento", "Concluido", "Cancelado"]},
            {"name": "DataInicio__c", "type": "Date", "label": "Data de Inicio"},
            {"name": "DataFim__c", "type": "Date", "label": "Data de Fim"},
            {"name": "Orcamento__c", "type": "Currency", "label": "Orcamento", "precision": 16, "scale": 2},
            {"name": "Responsavel__c", "type": "Lookup", "referenceTo": "User", "relationshipName": "Projetos", "relationshipLabel": "Projetos"},
            {"name": "Progresso__c", "type": "Percent", "precision": 5, "scale": 2}
        ]

        validation_rules = [
            {
                "name": "Data_Fim_Maior_Inicio",
                "description": "Data de fim deve ser maior que data de inicio",
                "formula": "DataFim__c < DataInicio__c",
                "message": "A Data de Fim deve ser posterior a Data de Inicio"
            }
        ]

        return self.generate_custom_object(
            api_name,
            "Projeto",
            "Projetos",
            "Objeto para gerenciamento de projetos",
            fields=fields,
            validation_rules=validation_rules
        )

    def generate_task_object(
        self,
        api_name: str = "Tarefa__c",
        parent_object: str = "Projeto__c"
    ) -> str:
        """
        Gera objeto de Tarefa relacionado a Projeto

        Args:
            api_name: Nome API do objeto
            parent_object: Objeto pai

        Returns:
            XML do objeto
        """
        fields = [
            {"name": "Projeto__c", "type": "MasterDetail", "referenceTo": parent_object, "relationshipName": "Tarefas", "relationshipLabel": "Tarefas"},
            {"name": "Descricao__c", "type": "TextArea"},
            {"name": "Status__c", "type": "Picklist", "picklistValues": ["Pendente", "Em Andamento", "Concluida", "Bloqueada"]},
            {"name": "Prioridade__c", "type": "Picklist", "picklistValues": ["Baixa", "Media", "Alta", "Urgente"]},
            {"name": "DataVencimento__c", "type": "Date", "label": "Data de Vencimento"},
            {"name": "Responsavel__c", "type": "Lookup", "referenceTo": "User", "relationshipName": "Tarefas", "relationshipLabel": "Tarefas"},
            {"name": "HorasEstimadas__c", "type": "Number", "precision": 4, "scale": 1, "label": "Horas Estimadas"},
            {"name": "HorasRealizadas__c", "type": "Number", "precision": 4, "scale": 1, "label": "Horas Realizadas"}
        ]

        return self.generate_custom_object(
            api_name,
            "Tarefa",
            "Tarefas",
            "Objeto para gerenciamento de tarefas de projeto",
            fields=fields
        )
