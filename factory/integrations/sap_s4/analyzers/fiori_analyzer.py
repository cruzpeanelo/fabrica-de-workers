# -*- coding: utf-8 -*-
"""
Fiori App Analyzer
==================
Analisador de aplicacoes SAP Fiori.

Analisa apps Fiori Elements e Freestyle para extrair metadados,
configuracoes e dependencias.

Autor: Fabrica de Agentes
"""

import json
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set
import logging

logger = logging.getLogger(__name__)


class FioriAppType(str, Enum):
    """Tipos de aplicacao Fiori"""
    LIST_REPORT = "list_report"
    OBJECT_PAGE = "object_page"
    WORKLIST = "worklist"
    ANALYTICAL_LIST_PAGE = "analytical_list_page"
    OVERVIEW_PAGE = "overview_page"
    FREESTYLE = "freestyle"
    SMART_TEMPLATE = "smart_template"
    CUSTOM = "custom"


class FioriFloorplan(str, Enum):
    """Floorplans Fiori Elements"""
    LIST_REPORT_OBJECT_PAGE = "ListReportObjectPage"
    WORKLIST = "Worklist"
    OVERVIEW_PAGE = "OverviewPage"
    ANALYTICAL_LIST_PAGE = "AnalyticalListPage"
    FORM_ENTRY = "FormEntry"
    WIZARD = "Wizard"


class UIFramework(str, Enum):
    """Frameworks UI suportados"""
    SAPUI5 = "sapui5"
    UI5_WEB_COMPONENTS = "ui5_web_components"
    FREESTYLE_SAPUI5 = "freestyle_sapui5"


@dataclass
class FioriComponent:
    """Representa um componente/control Fiori"""
    name: str
    module_path: str
    type: str = "control"
    properties: Dict[str, Any] = field(default_factory=dict)
    aggregations: List[str] = field(default_factory=list)


@dataclass
class FioriRoute:
    """Representa uma rota da aplicacao"""
    name: str
    pattern: str
    target: str
    title: Optional[str] = None


@dataclass
class FioriTarget:
    """Representa um target de navegacao"""
    name: str
    view_name: str
    view_type: str = "XML"
    controller_name: Optional[str] = None


@dataclass
class FioriDataSource:
    """Representa uma fonte de dados OData"""
    name: str
    uri: str
    type: str = "OData"
    version: str = "4.0"
    local_uri: Optional[str] = None
    annotations: List[str] = field(default_factory=list)


@dataclass
class FioriAppInfo:
    """
    Informacoes completas sobre uma aplicacao Fiori

    Contem todos os metadados necessarios para entender
    e reproduzir a estrutura de um app Fiori.
    """
    # Identificacao
    app_id: str
    title: str
    description: str = ""
    version: str = "1.0.0"

    # Classificacao
    app_type: FioriAppType = FioriAppType.FREESTYLE
    floorplan: Optional[FioriFloorplan] = None
    framework: UIFramework = UIFramework.SAPUI5

    # Configuracao
    min_ui5_version: str = "1.102.0"
    device_types: List[str] = field(default_factory=lambda: ["desktop", "tablet", "phone"])
    supported_themes: List[str] = field(default_factory=lambda: ["sap_fiori_3", "sap_horizon"])

    # Estrutura
    namespace: str = ""
    root_view: Optional[str] = None
    root_controller: Optional[str] = None

    # Navegacao
    routes: List[FioriRoute] = field(default_factory=list)
    targets: List[FioriTarget] = field(default_factory=list)

    # Dados
    data_sources: List[FioriDataSource] = field(default_factory=list)
    main_entity_set: Optional[str] = None

    # Componentes
    components: List[FioriComponent] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)
    libraries: List[str] = field(default_factory=list)

    # Fiori Launchpad
    semantic_object: Optional[str] = None
    action: Optional[str] = None
    tile_title: Optional[str] = None
    tile_subtitle: Optional[str] = None
    tile_icon: Optional[str] = None

    # Extensibilidade
    extension_points: List[str] = field(default_factory=list)
    custom_annotations: Dict[str, Any] = field(default_factory=dict)

    # Metadados
    created_at: Optional[datetime] = None
    modified_at: Optional[datetime] = None
    author: Optional[str] = None

    @property
    def is_fiori_elements(self) -> bool:
        """Verifica se e Fiori Elements"""
        return self.app_type != FioriAppType.FREESTYLE

    @property
    def launchpad_intent(self) -> Optional[str]:
        """Retorna intent para Fiori Launchpad"""
        if self.semantic_object and self.action:
            return f"{self.semantic_object}-{self.action}"
        return None

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "app_id": self.app_id,
            "title": self.title,
            "description": self.description,
            "version": self.version,
            "app_type": self.app_type.value,
            "floorplan": self.floorplan.value if self.floorplan else None,
            "framework": self.framework.value,
            "namespace": self.namespace,
            "main_entity_set": self.main_entity_set,
            "is_fiori_elements": self.is_fiori_elements,
            "launchpad_intent": self.launchpad_intent,
            "routes_count": len(self.routes),
            "data_sources_count": len(self.data_sources)
        }


class FioriAnalyzer:
    """
    Analisador de aplicacoes SAP Fiori

    Pode analisar:
    - manifest.json (descriptor da aplicacao)
    - Views XML
    - Controllers JavaScript
    - Annotations

    Exemplo de uso:
    ```python
    analyzer = FioriAnalyzer()

    # Analisar manifest.json
    app_info = analyzer.parse_manifest(manifest_json)

    # Analisar estrutura completa
    app_info = analyzer.analyze_app_folder(app_path)

    # Listar apps do sistema
    apps = await analyzer.list_apps_from_catalog(odata_client)
    ```
    """

    def __init__(self, odata_client=None):
        """
        Inicializa analisador

        Args:
            odata_client: Cliente OData opcional para queries
        """
        self.odata_client = odata_client

    def parse_manifest(self, manifest: Dict) -> FioriAppInfo:
        """
        Parse do manifest.json de uma aplicacao Fiori

        Args:
            manifest: Conteudo do manifest.json como dict

        Returns:
            FioriAppInfo com dados extraidos
        """
        # Secao sap.app
        sap_app = manifest.get("sap.app", {})
        app_id = sap_app.get("id", "")
        title = sap_app.get("title", "")
        description = sap_app.get("description", "")
        version = sap_app.get("applicationVersion", {}).get("version", "1.0.0")

        # Determinar tipo de app
        app_type, floorplan = self._detect_app_type(manifest)

        # Secao sap.ui5
        sap_ui5 = manifest.get("sap.ui5", {})

        # Fontes de dados
        data_sources = self._extract_data_sources(sap_app)

        # Roteamento
        routing = sap_ui5.get("routing", {})
        routes = self._extract_routes(routing)
        targets = self._extract_targets(routing)

        # Dependencias
        dependencies = sap_ui5.get("dependencies", {})
        min_version = dependencies.get("minUI5Version", "1.102.0")
        libs = list(dependencies.get("libs", {}).keys())

        # Secao sap.fiori (Launchpad)
        sap_fiori = manifest.get("sap.fiori", {})

        # Root view
        root_view = sap_ui5.get("rootView", {})
        if isinstance(root_view, dict):
            root_view_name = root_view.get("viewName")
        else:
            root_view_name = root_view

        # Criar FioriAppInfo
        app_info = FioriAppInfo(
            app_id=app_id,
            title=title,
            description=description,
            version=version,
            app_type=app_type,
            floorplan=floorplan,
            min_ui5_version=min_version,
            namespace=app_id.rsplit(".", 1)[0] if "." in app_id else app_id,
            root_view=root_view_name,
            routes=routes,
            targets=targets,
            data_sources=data_sources,
            libraries=libs
        )

        # Entity set principal
        if data_sources:
            # Usar primeira fonte de dados como principal
            main_source = data_sources[0]
            app_info.main_entity_set = main_source.name

        # Configuracoes Fiori Launchpad
        registration = sap_fiori.get("registrationIds", [])
        if registration:
            app_info.semantic_object = sap_fiori.get("semanticObject")
            app_info.action = sap_fiori.get("action")

        # Devices suportados
        sap_ui = manifest.get("sap.ui", {})
        device_types = sap_ui.get("deviceTypes", {})
        devices = []
        if device_types.get("desktop", True):
            devices.append("desktop")
        if device_types.get("tablet", True):
            devices.append("tablet")
        if device_types.get("phone", True):
            devices.append("phone")
        app_info.device_types = devices

        return app_info

    def _detect_app_type(self, manifest: Dict) -> tuple:
        """
        Detecta tipo de aplicacao Fiori

        Returns:
            Tuple (FioriAppType, FioriFloorplan opcional)
        """
        sap_ui5 = manifest.get("sap.ui5", {})

        # Verificar se usa Fiori Elements
        extends = sap_ui5.get("extends", {})
        component = extends.get("component", "")

        # Mapeamento de componentes para floorplans
        floorplan_mapping = {
            "sap.fe.templates.ListReport": (FioriAppType.LIST_REPORT, FioriFloorplan.LIST_REPORT_OBJECT_PAGE),
            "sap.fe.templates.ObjectPage": (FioriAppType.OBJECT_PAGE, FioriFloorplan.LIST_REPORT_OBJECT_PAGE),
            "sap.fe.templates.AnalyticalListPage": (FioriAppType.ANALYTICAL_LIST_PAGE, FioriFloorplan.ANALYTICAL_LIST_PAGE),
            "sap.fe.templates.Worklist": (FioriAppType.WORKLIST, FioriFloorplan.WORKLIST),
            "sap.ovp": (FioriAppType.OVERVIEW_PAGE, FioriFloorplan.OVERVIEW_PAGE),
            "sap.suite.ui.generic.template": (FioriAppType.SMART_TEMPLATE, None)
        }

        for prefix, (app_type, floorplan) in floorplan_mapping.items():
            if component.startswith(prefix):
                return app_type, floorplan

        return FioriAppType.FREESTYLE, None

    def _extract_data_sources(self, sap_app: Dict) -> List[FioriDataSource]:
        """Extrai fontes de dados"""
        sources = []
        data_sources_config = sap_app.get("dataSources", {})

        for name, config in data_sources_config.items():
            source = FioriDataSource(
                name=name,
                uri=config.get("uri", ""),
                type=config.get("type", "OData"),
                version=config.get("settings", {}).get("odataVersion", "4.0"),
                local_uri=config.get("settings", {}).get("localUri"),
                annotations=config.get("settings", {}).get("annotations", [])
            )
            sources.append(source)

        return sources

    def _extract_routes(self, routing: Dict) -> List[FioriRoute]:
        """Extrai rotas de navegacao"""
        routes = []
        routes_config = routing.get("routes", [])

        for route_config in routes_config:
            route = FioriRoute(
                name=route_config.get("name", ""),
                pattern=route_config.get("pattern", ""),
                target=route_config.get("target", ""),
                title=route_config.get("titleTarget")
            )
            routes.append(route)

        return routes

    def _extract_targets(self, routing: Dict) -> List[FioriTarget]:
        """Extrai targets de navegacao"""
        targets = []
        targets_config = routing.get("targets", {})

        for name, config in targets_config.items():
            target = FioriTarget(
                name=name,
                view_name=config.get("viewName", ""),
                view_type=config.get("viewType", "XML"),
                controller_name=config.get("controllerName")
            )
            targets.append(target)

        return targets

    def parse_manifest_json(self, manifest_content: str) -> FioriAppInfo:
        """
        Parse de manifest.json como string

        Args:
            manifest_content: Conteudo JSON como string

        Returns:
            FioriAppInfo
        """
        manifest = json.loads(manifest_content)
        return self.parse_manifest(manifest)

    def analyze_view_xml(self, xml_content: str) -> Dict[str, Any]:
        """
        Analisa uma View XML

        Args:
            xml_content: Conteudo XML da view

        Returns:
            Dict com informacoes da view
        """
        analysis = {
            "controls": [],
            "bindings": [],
            "custom_data": [],
            "fragments": [],
            "i18n_keys": []
        }

        # Extrair controls usados
        control_pattern = re.compile(r"<(\w+:)?(\w+)\s", re.MULTILINE)
        for match in control_pattern.finditer(xml_content):
            namespace = match.group(1) or ""
            control = match.group(2)
            analysis["controls"].append(f"{namespace}{control}")

        # Extrair bindings
        binding_pattern = re.compile(r"\{([^}]+)\}", re.MULTILINE)
        for match in binding_pattern.finditer(xml_content):
            binding = match.group(1)
            if binding.startswith("/") or binding.startswith("i18n>"):
                analysis["bindings"].append(binding)

        # Extrair referencias a fragments
        fragment_pattern = re.compile(r'fragmentName="([^"]+)"', re.MULTILINE)
        for match in fragment_pattern.finditer(xml_content):
            analysis["fragments"].append(match.group(1))

        # Extrair chaves i18n
        i18n_pattern = re.compile(r"\{i18n>(\w+)\}", re.MULTILINE)
        for match in i18n_pattern.finditer(xml_content):
            analysis["i18n_keys"].append(match.group(1))

        # Remover duplicatas
        analysis["controls"] = list(set(analysis["controls"]))
        analysis["i18n_keys"] = list(set(analysis["i18n_keys"]))

        return analysis

    def analyze_controller_js(self, js_content: str) -> Dict[str, Any]:
        """
        Analisa um Controller JavaScript

        Args:
            js_content: Conteudo JavaScript

        Returns:
            Dict com informacoes do controller
        """
        analysis = {
            "extends": None,
            "methods": [],
            "event_handlers": [],
            "imports": [],
            "odata_calls": []
        }

        # Extrair classe base (extends)
        extends_pattern = re.compile(r"extends\s+([^\s{]+)")
        match = extends_pattern.search(js_content)
        if match:
            analysis["extends"] = match.group(1)

        # Extrair metodos
        method_pattern = re.compile(r"(\w+)\s*[=:]\s*(?:async\s+)?function\s*\(|(\w+)\s*\([^)]*\)\s*{", re.MULTILINE)
        for match in method_pattern.finditer(js_content):
            method = match.group(1) or match.group(2)
            if method:
                analysis["methods"].append(method)

        # Identificar event handlers (on*, handle*)
        for method in analysis["methods"]:
            if method.startswith("on") or method.startswith("handle") or method.startswith("_on"):
                analysis["event_handlers"].append(method)

        # Extrair imports/requires
        import_pattern = re.compile(r'sap\.ui\.(?:define|require)\(\[(.*?)\]', re.DOTALL)
        match = import_pattern.search(js_content)
        if match:
            imports_str = match.group(1)
            import_items = re.findall(r'"([^"]+)"', imports_str)
            analysis["imports"] = import_items

        # Identificar chamadas OData
        odata_patterns = [
            r"read\s*\(",
            r"create\s*\(",
            r"update\s*\(",
            r"delete\s*\(",
            r"callFunction\s*\(",
            r"submitBatch\s*\("
        ]
        for pattern in odata_patterns:
            if re.search(pattern, js_content):
                operation = pattern.replace(r"\s*\(", "").replace("\\", "")
                analysis["odata_calls"].append(operation)

        return analysis

    def generate_manifest_template(
        self,
        app_id: str,
        title: str,
        entity_set: str,
        odata_service_url: str,
        app_type: FioriAppType = FioriAppType.LIST_REPORT
    ) -> Dict:
        """
        Gera template de manifest.json

        Args:
            app_id: ID da aplicacao
            title: Titulo
            entity_set: Entity set principal
            odata_service_url: URL do servico OData
            app_type: Tipo de aplicacao

        Returns:
            Dict com manifest.json
        """
        manifest = {
            "_version": "1.42.0",
            "sap.app": {
                "id": app_id,
                "type": "application",
                "i18n": "i18n/i18n.properties",
                "applicationVersion": {
                    "version": "1.0.0"
                },
                "title": f"{{{{appTitle}}}}",
                "description": f"{{{{appDescription}}}}",
                "dataSources": {
                    "mainService": {
                        "uri": odata_service_url,
                        "type": "OData",
                        "settings": {
                            "odataVersion": "4.0",
                            "localUri": f"localService/metadata.xml"
                        }
                    }
                }
            },
            "sap.ui": {
                "technology": "UI5",
                "deviceTypes": {
                    "desktop": True,
                    "tablet": True,
                    "phone": True
                }
            },
            "sap.ui5": {
                "flexEnabled": True,
                "dependencies": {
                    "minUI5Version": "1.102.0",
                    "libs": {
                        "sap.m": {},
                        "sap.ui.core": {},
                        "sap.ushell": {},
                        "sap.fe.templates": {}
                    }
                },
                "models": {
                    "i18n": {
                        "type": "sap.ui.model.resource.ResourceModel",
                        "settings": {
                            "bundleName": f"{app_id}.i18n.i18n"
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
                "routing": {
                    "routes": [
                        {
                            "pattern": ":?query:",
                            "name": "main",
                            "target": "main"
                        }
                    ],
                    "targets": {
                        "main": {
                            "type": "Component",
                            "id": "main",
                            "name": "sap.fe.templates.ListReport",
                            "options": {
                                "settings": {
                                    "entitySet": entity_set,
                                    "navigation": {}
                                }
                            }
                        }
                    }
                }
            },
            "sap.fiori": {
                "registrationIds": [],
                "archeType": "transactional"
            }
        }

        # Ajustar para tipo de app
        if app_type == FioriAppType.WORKLIST:
            manifest["sap.ui5"]["routing"]["targets"]["main"]["name"] = "sap.fe.templates.Worklist"
        elif app_type == FioriAppType.ANALYTICAL_LIST_PAGE:
            manifest["sap.ui5"]["routing"]["targets"]["main"]["name"] = "sap.fe.templates.AnalyticalListPage"
        elif app_type == FioriAppType.OVERVIEW_PAGE:
            manifest["sap.ui5"]["dependencies"]["libs"]["sap.ovp"] = {}
            manifest["sap.ui5"]["routing"]["targets"]["main"]["name"] = "sap.ovp"

        return manifest

    async def list_apps_from_catalog(
        self,
        filter_pattern: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict]:
        """
        Lista aplicacoes Fiori do catalogo do sistema

        Args:
            filter_pattern: Padrao de filtro
            limit: Limite

        Returns:
            Lista de apps
        """
        if not self.odata_client:
            raise ValueError("Cliente OData nao configurado")

        # Query ao catalogo Fiori (depende da configuracao do sistema)
        # Este e um exemplo generico
        try:
            filter_expr = None
            if filter_pattern:
                filter_expr = f"contains(ApplicationId, '{filter_pattern}')"

            response = await self.odata_client.get(
                "FioriApplication",
                filter_expr=filter_expr,
                top=limit
            )

            return response.items

        except Exception as e:
            logger.warning(f"Catalogo Fiori nao disponivel: {e}")
            return []
