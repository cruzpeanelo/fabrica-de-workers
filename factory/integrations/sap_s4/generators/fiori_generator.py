# -*- coding: utf-8 -*-
"""
Fiori App Generator
===================
Gerador de aplicacoes SAP Fiori Elements e Freestyle.

Este modulo gera estrutura completa de apps Fiori incluindo:
- manifest.json
- Views XML
- Controllers JavaScript
- Annotations

Autor: Plataforma E
"""

import json
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class FioriFloorplanType(str, Enum):
    """Tipos de floorplan Fiori Elements"""
    LIST_REPORT = "list_report"
    WORKLIST = "worklist"
    OBJECT_PAGE = "object_page"
    OVERVIEW_PAGE = "overview_page"
    ANALYTICAL_LIST_PAGE = "analytical_list_page"
    FORM_ENTRY = "form_entry"


class FioriDeviceType(str, Enum):
    """Tipos de dispositivo"""
    DESKTOP = "desktop"
    TABLET = "tablet"
    PHONE = "phone"


@dataclass
class FioriFieldSpec:
    """Especificacao de campo para UI"""
    name: str
    label: str
    data_type: str = "string"
    position: int = 0
    is_key: bool = False
    is_hidden: bool = False
    is_filterable: bool = True
    is_sortable: bool = True
    importance: str = "HIGH"  # HIGH, MEDIUM, LOW


@dataclass
class FioriActionSpec:
    """Especificacao de action"""
    name: str
    label: str
    is_determining: bool = False
    requires_context: bool = True
    criticality: Optional[str] = None  # Positive, Negative, Critical


@dataclass
class FioriNavigationSpec:
    """Especificacao de navegacao"""
    source_entity: str
    target_entity: str
    navigation_property: str
    is_collection: bool = False


@dataclass
class FioriAppSpec:
    """
    Especificacao completa para geracao de aplicacao Fiori

    Contem todas as informacoes necessarias para gerar
    uma aplicacao Fiori Elements completa.
    """
    # Identificacao
    app_id: str
    title: str
    description: str = ""
    version: str = "1.0.0"

    # Tipo de app
    floorplan: FioriFloorplanType = FioriFloorplanType.LIST_REPORT

    # Servico OData
    odata_service_url: str = ""
    odata_version: str = "4.0"
    main_entity_set: str = ""

    # Campos e estrutura
    list_fields: List[FioriFieldSpec] = field(default_factory=list)
    filter_fields: List[FioriFieldSpec] = field(default_factory=list)
    header_fields: List[FioriFieldSpec] = field(default_factory=list)
    detail_fields: List[FioriFieldSpec] = field(default_factory=list)

    # Actions
    table_actions: List[FioriActionSpec] = field(default_factory=list)
    header_actions: List[FioriActionSpec] = field(default_factory=list)

    # Navegacao
    navigations: List[FioriNavigationSpec] = field(default_factory=list)

    # Configuracoes
    devices: List[FioriDeviceType] = field(default_factory=lambda: [
        FioriDeviceType.DESKTOP,
        FioriDeviceType.TABLET,
        FioriDeviceType.PHONE
    ])
    themes: List[str] = field(default_factory=lambda: ["sap_horizon", "sap_fiori_3"])
    min_ui5_version: str = "1.108.0"

    # Fiori Launchpad
    semantic_object: Optional[str] = None
    action: Optional[str] = None
    tile_title: Optional[str] = None
    tile_icon: Optional[str] = None

    # Namespace
    namespace: str = ""

    def validate(self) -> List[str]:
        """Valida especificacao"""
        errors = []

        if not self.app_id:
            errors.append("ID da aplicacao e obrigatorio")

        if not self.title:
            errors.append("Titulo e obrigatorio")

        if not self.odata_service_url:
            errors.append("URL do servico OData e obrigatoria")

        if not self.main_entity_set:
            errors.append("Entity Set principal e obrigatorio")

        return errors


class FioriGenerator:
    """
    Gerador de aplicacoes SAP Fiori

    Gera estrutura completa de aplicacao Fiori Elements:
    - manifest.json
    - Annotations
    - i18n
    - Views (para freestyle)

    Exemplo de uso:
    ```python
    generator = FioriGenerator()

    spec = FioriAppSpec(
        app_id="com.mycompany.salesorders",
        title="Ordens de Venda",
        description="Gerenciamento de ordens de venda",
        floorplan=FioriFloorplanType.LIST_REPORT,
        odata_service_url="/sap/opu/odata4/sap/api_sales_order/",
        main_entity_set="SalesOrder",
        list_fields=[
            FioriFieldSpec(name="SalesOrder", label="Ordem", is_key=True),
            FioriFieldSpec(name="SoldToParty", label="Cliente"),
            FioriFieldSpec(name="TotalNetAmount", label="Valor Total")
        ]
    )

    files = generator.generate(spec)
    # files = {
    #     "manifest.json": "...",
    #     "i18n/i18n.properties": "...",
    #     "annotations/annotation.xml": "..."
    # }
    ```
    """

    def __init__(self):
        """Inicializa gerador"""
        pass

    def generate(self, spec: FioriAppSpec) -> Dict[str, str]:
        """
        Gera todos os arquivos da aplicacao

        Args:
            spec: Especificacao da aplicacao

        Returns:
            Dict com nome do arquivo -> conteudo

        Raises:
            ValueError: Se especificacao invalida
        """
        errors = spec.validate()
        if errors:
            raise ValueError(f"Especificacao invalida: {', '.join(errors)}")

        files = {}

        # manifest.json
        files["manifest.json"] = self._generate_manifest(spec)

        # i18n
        files["i18n/i18n.properties"] = self._generate_i18n(spec)

        # Annotations (para Fiori Elements)
        if spec.floorplan != FioriFloorplanType.FORM_ENTRY:
            files["annotations/annotation.xml"] = self._generate_annotations(spec)

        # Component.js (se necessario)
        files["Component.js"] = self._generate_component(spec)

        return files

    def _generate_manifest(self, spec: FioriAppSpec) -> str:
        """Gera manifest.json"""
        namespace = spec.namespace or spec.app_id.rsplit(".", 1)[0] if "." in spec.app_id else spec.app_id

        # Template component baseado no floorplan
        template_component = self._get_template_component(spec.floorplan)

        manifest = {
            "_version": "1.42.0",
            "sap.app": {
                "id": spec.app_id,
                "type": "application",
                "i18n": "i18n/i18n.properties",
                "applicationVersion": {
                    "version": spec.version
                },
                "title": "{{appTitle}}",
                "description": "{{appDescription}}",
                "resources": "resources.json",
                "ach": "FIN-FSCM-CLM",
                "dataSources": {
                    "mainService": {
                        "uri": spec.odata_service_url,
                        "type": "OData",
                        "settings": {
                            "annotations": ["annotation"],
                            "localUri": "localService/metadata.xml",
                            "odataVersion": spec.odata_version
                        }
                    },
                    "annotation": {
                        "type": "ODataAnnotation",
                        "uri": "annotations/annotation.xml",
                        "settings": {
                            "localUri": "annotations/annotation.xml"
                        }
                    }
                }
            },
            "sap.ui": {
                "technology": "UI5",
                "icons": {
                    "icon": spec.tile_icon or "sap-icon://list"
                },
                "deviceTypes": {
                    "desktop": FioriDeviceType.DESKTOP in spec.devices,
                    "tablet": FioriDeviceType.TABLET in spec.devices,
                    "phone": FioriDeviceType.PHONE in spec.devices
                }
            },
            "sap.ui5": {
                "flexEnabled": True,
                "dependencies": {
                    "minUI5Version": spec.min_ui5_version,
                    "libs": self._get_required_libs(spec.floorplan)
                },
                "contentDensities": {
                    "compact": True,
                    "cozy": True
                },
                "models": {
                    "i18n": {
                        "type": "sap.ui.model.resource.ResourceModel",
                        "settings": {
                            "bundleName": f"{spec.app_id}.i18n.i18n"
                        }
                    },
                    "": {
                        "dataSource": "mainService",
                        "preload": True,
                        "settings": {
                            "synchronizationMode": "None",
                            "operationMode": "Server",
                            "autoExpandSelect": True,
                            "earlyRequests": True
                        }
                    }
                },
                "routing": self._generate_routing(spec, template_component)
            },
            "sap.fiori": {
                "registrationIds": [],
                "archeType": "transactional"
            }
        }

        # Adicionar semantic object para Launchpad
        if spec.semantic_object and spec.action:
            manifest["sap.app"]["crossNavigation"] = {
                "inbounds": {
                    "intent1": {
                        "semanticObject": spec.semantic_object,
                        "action": spec.action,
                        "title": spec.tile_title or spec.title,
                        "signature": {
                            "parameters": {},
                            "additionalParameters": "allowed"
                        }
                    }
                }
            }

        return json.dumps(manifest, indent=2, ensure_ascii=False)

    def _get_template_component(self, floorplan: FioriFloorplanType) -> str:
        """Retorna nome do componente template"""
        mapping = {
            FioriFloorplanType.LIST_REPORT: "sap.fe.templates.ListReport",
            FioriFloorplanType.WORKLIST: "sap.fe.templates.Worklist",
            FioriFloorplanType.OBJECT_PAGE: "sap.fe.templates.ObjectPage",
            FioriFloorplanType.OVERVIEW_PAGE: "sap.ovp",
            FioriFloorplanType.ANALYTICAL_LIST_PAGE: "sap.fe.templates.AnalyticalListPage",
            FioriFloorplanType.FORM_ENTRY: "sap.fe.templates.FormEntry"
        }
        return mapping.get(floorplan, "sap.fe.templates.ListReport")

    def _get_required_libs(self, floorplan: FioriFloorplanType) -> Dict[str, Dict]:
        """Retorna bibliotecas requeridas"""
        base_libs = {
            "sap.m": {},
            "sap.ui.core": {},
            "sap.ushell": {},
            "sap.fe.templates": {}
        }

        if floorplan == FioriFloorplanType.OVERVIEW_PAGE:
            base_libs["sap.ovp"] = {}

        if floorplan == FioriFloorplanType.ANALYTICAL_LIST_PAGE:
            base_libs["sap.suite.ui.commons"] = {}

        return base_libs

    def _generate_routing(self, spec: FioriAppSpec, template_component: str) -> Dict:
        """Gera configuracao de routing"""
        routing = {
            "routes": [],
            "targets": {}
        }

        # Rota principal
        main_route = {
            "pattern": ":?query:",
            "name": f"{spec.main_entity_set}List",
            "target": f"{spec.main_entity_set}List"
        }
        routing["routes"].append(main_route)

        # Target principal
        routing["targets"][f"{spec.main_entity_set}List"] = {
            "type": "Component",
            "id": f"{spec.main_entity_set}List",
            "name": template_component,
            "options": {
                "settings": {
                    "entitySet": spec.main_entity_set,
                    "variantManagement": "Page",
                    "navigation": {}
                }
            }
        }

        # Adicionar Object Page se for List Report
        if spec.floorplan == FioriFloorplanType.LIST_REPORT:
            # Rota para detalhes
            detail_route = {
                "pattern": f"{spec.main_entity_set}({{key}}):?query:",
                "name": f"{spec.main_entity_set}ObjectPage",
                "target": f"{spec.main_entity_set}ObjectPage"
            }
            routing["routes"].append(detail_route)

            # Target de detalhes
            routing["targets"][f"{spec.main_entity_set}ObjectPage"] = {
                "type": "Component",
                "id": f"{spec.main_entity_set}ObjectPage",
                "name": "sap.fe.templates.ObjectPage",
                "options": {
                    "settings": {
                        "entitySet": spec.main_entity_set,
                        "navigation": {}
                    }
                }
            }

            # Configurar navegacao
            routing["targets"][f"{spec.main_entity_set}List"]["options"]["settings"]["navigation"] = {
                spec.main_entity_set: {
                    "detail": {
                        "route": f"{spec.main_entity_set}ObjectPage"
                    }
                }
            }

        return routing

    def _generate_i18n(self, spec: FioriAppSpec) -> str:
        """Gera arquivo i18n"""
        lines = [
            f"appTitle={spec.title}",
            f"appDescription={spec.description}",
            ""
        ]

        # Labels dos campos
        for field_spec in spec.list_fields + spec.filter_fields:
            lines.append(f"{field_spec.name}={field_spec.label}")

        # Actions
        for action in spec.table_actions + spec.header_actions:
            lines.append(f"{action.name}={action.label}")

        return "\n".join(lines)

    def _generate_annotations(self, spec: FioriAppSpec) -> str:
        """Gera arquivo de annotations XML"""
        xml_parts = [
            '<?xml version="1.0" encoding="utf-8"?>',
            '<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">',
            '  <edmx:Reference Uri="' + spec.odata_service_url + '$metadata">',
            f'    <edmx:Include Namespace="com.sap.gateway.srvd_a2x.sap.{spec.main_entity_set.lower()}" Alias="SAP"/>',
            '  </edmx:Reference>',
            '  <edmx:Reference Uri="/sap/opu/odata/sap/UI_VOCAB/UI.xml">',
            '    <edmx:Include Namespace="com.sap.vocabularies.UI.v1" Alias="UI"/>',
            '  </edmx:Reference>',
            '  <edmx:Reference Uri="/sap/opu/odata/sap/COMMON_VOCAB/Common.xml">',
            '    <edmx:Include Namespace="com.sap.vocabularies.Common.v1" Alias="Common"/>',
            '  </edmx:Reference>',
            '  <edmx:DataServices>',
            f'    <Schema Namespace="{spec.app_id}.annotations" xmlns="http://docs.oasis-open.org/odata/ns/edm">',
            f'      <Annotations Target="SAP.{spec.main_entity_set}Type">'
        ]

        # SelectionFields (filtros)
        if spec.filter_fields:
            xml_parts.append('        <Annotation Term="UI.SelectionFields">')
            xml_parts.append('          <Collection>')
            for field_spec in spec.filter_fields:
                xml_parts.append(f'            <PropertyPath>{field_spec.name}</PropertyPath>')
            xml_parts.append('          </Collection>')
            xml_parts.append('        </Annotation>')

        # LineItem (colunas da tabela)
        if spec.list_fields:
            xml_parts.append('        <Annotation Term="UI.LineItem">')
            xml_parts.append('          <Collection>')
            for i, field_spec in enumerate(spec.list_fields):
                xml_parts.append('            <Record Type="UI.DataField">')
                xml_parts.append(f'              <PropertyValue Property="Value" Path="{field_spec.name}"/>')
                xml_parts.append(f'              <PropertyValue Property="Label" String="{field_spec.label}"/>')
                if field_spec.importance:
                    xml_parts.append(f'              <Annotation Term="UI.Importance" EnumMember="UI.ImportanceType/{field_spec.importance}"/>')
                xml_parts.append('            </Record>')
            xml_parts.append('          </Collection>')
            xml_parts.append('        </Annotation>')

        # HeaderInfo
        xml_parts.append('        <Annotation Term="UI.HeaderInfo">')
        xml_parts.append('          <Record Type="UI.HeaderInfoType">')
        xml_parts.append(f'            <PropertyValue Property="TypeName" String="{spec.title}"/>')
        xml_parts.append(f'            <PropertyValue Property="TypeNamePlural" String="{spec.title}s"/>')
        if spec.list_fields:
            key_field = next((f for f in spec.list_fields if f.is_key), spec.list_fields[0])
            xml_parts.append('            <PropertyValue Property="Title">')
            xml_parts.append('              <Record Type="UI.DataField">')
            xml_parts.append(f'                <PropertyValue Property="Value" Path="{key_field.name}"/>')
            xml_parts.append('              </Record>')
            xml_parts.append('            </PropertyValue>')
        xml_parts.append('          </Record>')
        xml_parts.append('        </Annotation>')

        # Facets para Object Page
        if spec.floorplan == FioriFloorplanType.LIST_REPORT and spec.detail_fields:
            xml_parts.append('        <Annotation Term="UI.Facets">')
            xml_parts.append('          <Collection>')
            xml_parts.append('            <Record Type="UI.ReferenceFacet">')
            xml_parts.append('              <PropertyValue Property="Label" String="Detalhes"/>')
            xml_parts.append('              <PropertyValue Property="ID" String="GeneralInfoFacet"/>')
            xml_parts.append('              <PropertyValue Property="Target" AnnotationPath="@UI.FieldGroup#GeneralInfo"/>')
            xml_parts.append('            </Record>')
            xml_parts.append('          </Collection>')
            xml_parts.append('        </Annotation>')

            # FieldGroup para detalhes
            xml_parts.append('        <Annotation Term="UI.FieldGroup" Qualifier="GeneralInfo">')
            xml_parts.append('          <Record Type="UI.FieldGroupType">')
            xml_parts.append('            <PropertyValue Property="Data">')
            xml_parts.append('              <Collection>')
            for field_spec in spec.detail_fields:
                xml_parts.append('                <Record Type="UI.DataField">')
                xml_parts.append(f'                  <PropertyValue Property="Value" Path="{field_spec.name}"/>')
                xml_parts.append(f'                  <PropertyValue Property="Label" String="{field_spec.label}"/>')
                xml_parts.append('                </Record>')
            xml_parts.append('              </Collection>')
            xml_parts.append('            </PropertyValue>')
            xml_parts.append('          </Record>')
            xml_parts.append('        </Annotation>')

        xml_parts.extend([
            '      </Annotations>',
            '    </Schema>',
            '  </edmx:DataServices>',
            '</edmx:Edmx>'
        ])

        return "\n".join(xml_parts)

    def _generate_component(self, spec: FioriAppSpec) -> str:
        """Gera Component.js"""
        return f'''sap.ui.define([
    "sap/fe/core/AppComponent"
], function (Component) {{
    "use strict";

    return Component.extend("{spec.app_id}.Component", {{
        metadata: {{
            manifest: "json"
        }}
    }});
}});
'''

    def generate_view_xml(
        self,
        view_name: str,
        controller_name: str,
        content: str,
        namespace: str
    ) -> str:
        """
        Gera View XML (para apps freestyle)

        Args:
            view_name: Nome da view
            controller_name: Nome do controller
            content: Conteudo XML interno
            namespace: Namespace da aplicacao

        Returns:
            String com XML da view
        """
        return f'''<mvc:View
    controllerName="{namespace}.controller.{controller_name}"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    xmlns:semantic="sap.f.semantic">
    <semantic:SemanticPage
        headerPinnable="false"
        toggleHeaderOnTitleClick="false">
        <semantic:titleHeading>
            <Title text="{{i18n>{view_name}Title}}"/>
        </semantic:titleHeading>
        <semantic:content>
{content}
        </semantic:content>
    </semantic:SemanticPage>
</mvc:View>
'''

    def generate_controller_js(
        self,
        controller_name: str,
        namespace: str,
        methods: List[str] = None
    ) -> str:
        """
        Gera Controller JavaScript (para apps freestyle)

        Args:
            controller_name: Nome do controller
            namespace: Namespace da aplicacao
            methods: Lista de nomes de metodos adicionais

        Returns:
            String com codigo JavaScript
        """
        methods_str = ""
        if methods:
            for method in methods:
                methods_str += f'''
        {method}: function() {{
            // TODO: Implementar {method}
        }},
'''

        return f'''sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (Controller, JSONModel, MessageToast) {{
    "use strict";

    return Controller.extend("{namespace}.controller.{controller_name}", {{
        onInit: function () {{
            // Inicializacao do controller
        }},{methods_str}

        onAfterRendering: function () {{
            // Apos renderizacao
        }}
    }});
}});
'''
