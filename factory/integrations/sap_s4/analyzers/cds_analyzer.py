# -*- coding: utf-8 -*-
"""
CDS View Analyzer
=================
Analisador de CDS Views do SAP S/4HANA.

CDS (Core Data Services) e a base de modelagem de dados no S/4HANA,
permitindo definir views semanticas, associacoes e anotacoes.

Autor: Plataforma E
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set
import logging

logger = logging.getLogger(__name__)


class CDSViewType(str, Enum):
    """Tipos de CDS View conforme VDM (Virtual Data Model)"""
    BASIC = "BASIC"              # Interface View basica (I_)
    COMPOSITE = "COMPOSITE"       # View composta
    CONSUMPTION = "CONSUMPTION"   # View de consumo (C_)
    EXTENSION = "EXTENSION"       # View de extensao
    REMOTE = "REMOTE"             # View remota
    TRANSACTIONAL = "TRANSACTIONAL"  # View transacional


class CDSAnnotationCategory(str, Enum):
    """Categorias de anotacoes CDS"""
    SEMANTIC = "semantic"          # @Semantics
    UI = "ui"                      # @UI
    ODATA = "odata"               # @OData
    ANALYTICS = "analytics"        # @Analytics
    AGGREGATION = "aggregation"    # @Aggregation
    CONSUMPTION = "consumption"    # @Consumption
    OBJECT_MODEL = "object_model"  # @ObjectModel
    VDM = "vdm"                   # @VDM
    ACCESS_CONTROL = "access_control"  # @AccessControl
    METADATA = "metadata"         # @Metadata
    END_USER_TEXT = "end_user_text"  # @EndUserText
    SEARCH = "search"             # @Search
    OTHER = "other"


@dataclass
class CDSField:
    """Representa um campo em uma CDS View"""
    name: str
    alias: Optional[str] = None
    data_type: Optional[str] = None
    length: Optional[int] = None
    decimals: Optional[int] = None
    is_key: bool = False
    is_localized: bool = False
    annotations: Dict[str, Any] = field(default_factory=dict)
    source_field: Optional[str] = None
    source_entity: Optional[str] = None
    expression: Optional[str] = None

    @property
    def display_name(self) -> str:
        """Nome para exibicao"""
        return self.alias or self.name


@dataclass
class CDSAssociation:
    """Representa uma associacao em uma CDS View"""
    name: str
    target_entity: str
    cardinality: str = "[0..1]"
    condition: Optional[str] = None
    is_composition: bool = False
    annotations: Dict[str, Any] = field(default_factory=dict)


@dataclass
class CDSParameter:
    """Representa um parametro de uma CDS View"""
    name: str
    data_type: str
    default_value: Optional[str] = None
    is_mandatory: bool = True
    annotations: Dict[str, Any] = field(default_factory=dict)


@dataclass
class CDSAnnotation:
    """Representa uma anotacao CDS"""
    name: str
    value: Any
    category: CDSAnnotationCategory = CDSAnnotationCategory.OTHER
    target: Optional[str] = None  # Campo alvo (se for anotacao de campo)


@dataclass
class CDSViewInfo:
    """
    Informacoes completas sobre uma CDS View

    Representa todos os metadados extraidos de uma CDS View,
    incluindo campos, associacoes, parametros e anotacoes.
    """
    # Identificacao
    name: str
    sql_view_name: Optional[str] = None
    package: Optional[str] = None
    description: Optional[str] = None

    # Tipo e classificacao
    view_type: CDSViewType = CDSViewType.BASIC
    is_root: bool = False
    is_draft_enabled: bool = False

    # Estrutura
    fields: List[CDSField] = field(default_factory=list)
    associations: List[CDSAssociation] = field(default_factory=list)
    parameters: List[CDSParameter] = field(default_factory=list)

    # Fonte de dados
    source_entities: List[str] = field(default_factory=list)
    base_entity: Optional[str] = None

    # Anotacoes
    annotations: List[CDSAnnotation] = field(default_factory=list)

    # Metadados
    created_by: Optional[str] = None
    created_at: Optional[datetime] = None
    modified_by: Optional[str] = None
    modified_at: Optional[datetime] = None

    # Analise
    complexity_score: int = 0
    odata_published: bool = False
    analytics_enabled: bool = False

    @property
    def key_fields(self) -> List[CDSField]:
        """Retorna campos chave"""
        return [f for f in self.fields if f.is_key]

    @property
    def field_count(self) -> int:
        """Numero de campos"""
        return len(self.fields)

    @property
    def association_count(self) -> int:
        """Numero de associacoes"""
        return len(self.associations)

    def get_annotations_by_category(self, category: CDSAnnotationCategory) -> List[CDSAnnotation]:
        """Filtra anotacoes por categoria"""
        return [a for a in self.annotations if a.category == category]

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "name": self.name,
            "sql_view_name": self.sql_view_name,
            "package": self.package,
            "description": self.description,
            "view_type": self.view_type.value,
            "is_root": self.is_root,
            "is_draft_enabled": self.is_draft_enabled,
            "field_count": self.field_count,
            "association_count": self.association_count,
            "source_entities": self.source_entities,
            "odata_published": self.odata_published,
            "analytics_enabled": self.analytics_enabled,
            "complexity_score": self.complexity_score,
            "fields": [
                {
                    "name": f.name,
                    "alias": f.alias,
                    "is_key": f.is_key,
                    "data_type": f.data_type
                }
                for f in self.fields
            ],
            "associations": [
                {
                    "name": a.name,
                    "target": a.target_entity,
                    "cardinality": a.cardinality
                }
                for a in self.associations
            ]
        }


class CDSAnalyzer:
    """
    Analisador de CDS Views do SAP S/4HANA

    Este analisador pode:
    - Fazer parse de codigo-fonte CDS
    - Extrair metadados via API OData
    - Analisar estrutura e dependencias
    - Calcular metricas de complexidade

    Exemplo de uso:
    ```python
    analyzer = CDSAnalyzer(odata_client)

    # Analisar view do sistema
    view_info = await analyzer.analyze_view("I_SalesOrder")

    # Parse de codigo fonte
    view_info = analyzer.parse_cds_source(cds_source_code)

    # Listar views do sistema
    views = await analyzer.list_views(filter_pattern="Z*")
    ```
    """

    # Regex patterns para parse
    VIEW_HEADER_PATTERN = re.compile(
        r"define\s+(root\s+)?view\s+(\w+)\s+(?:with\s+parameters\s+(.+?))?\s*as\s+select\s+from\s+(\w+)",
        re.IGNORECASE | re.DOTALL
    )

    ANNOTATION_PATTERN = re.compile(
        r"@(\w+(?:\.\w+)*)\s*(?::\s*(.+?))?(?=\s*@|\s*define|\n)",
        re.MULTILINE
    )

    FIELD_PATTERN = re.compile(
        r"(key\s+)?(?:(\w+)\.)?(\w+)(?:\s+as\s+(\w+))?",
        re.IGNORECASE
    )

    ASSOCIATION_PATTERN = re.compile(
        r"association\s+(\[[\d\.\*]+\])\s+to\s+(\w+)\s+as\s+(\w+)\s+on\s+(.+?)(?=\s*association|\s*\{|\s*\}|$)",
        re.IGNORECASE | re.DOTALL
    )

    def __init__(self, odata_client=None):
        """
        Inicializa analisador

        Args:
            odata_client: Cliente OData V4 opcional para queries ao sistema
        """
        self.odata_client = odata_client
        self._view_cache: Dict[str, CDSViewInfo] = {}

    def parse_cds_source(self, source: str) -> CDSViewInfo:
        """
        Faz parse de codigo-fonte CDS

        Args:
            source: Codigo fonte CDS

        Returns:
            CDSViewInfo com dados extraidos
        """
        view_info = CDSViewInfo(name="")

        # Extrair anotacoes de header
        self._extract_header_annotations(source, view_info)

        # Extrair definicao da view
        self._extract_view_definition(source, view_info)

        # Extrair campos
        self._extract_fields(source, view_info)

        # Extrair associacoes
        self._extract_associations(source, view_info)

        # Calcular complexidade
        view_info.complexity_score = self._calculate_complexity(view_info)

        return view_info

    def _extract_header_annotations(self, source: str, view_info: CDSViewInfo):
        """Extrai anotacoes do header"""
        # Pegar tudo antes do 'define'
        define_pos = source.lower().find("define")
        if define_pos > 0:
            header = source[:define_pos]

            for match in self.ANNOTATION_PATTERN.finditer(header):
                annotation_name = match.group(1)
                annotation_value = match.group(2)

                if annotation_value:
                    annotation_value = annotation_value.strip()

                # Classificar anotacao
                category = self._classify_annotation(annotation_name)

                annotation = CDSAnnotation(
                    name=annotation_name,
                    value=annotation_value,
                    category=category
                )
                view_info.annotations.append(annotation)

                # Processar anotacoes especiais
                self._process_special_annotation(annotation_name, annotation_value, view_info)

    def _classify_annotation(self, name: str) -> CDSAnnotationCategory:
        """Classifica anotacao por categoria"""
        name_lower = name.lower()

        if name_lower.startswith("semantics"):
            return CDSAnnotationCategory.SEMANTIC
        elif name_lower.startswith("ui"):
            return CDSAnnotationCategory.UI
        elif name_lower.startswith("odata"):
            return CDSAnnotationCategory.ODATA
        elif name_lower.startswith("analytics"):
            return CDSAnnotationCategory.ANALYTICS
        elif name_lower.startswith("aggregation"):
            return CDSAnnotationCategory.AGGREGATION
        elif name_lower.startswith("consumption"):
            return CDSAnnotationCategory.CONSUMPTION
        elif name_lower.startswith("objectmodel"):
            return CDSAnnotationCategory.OBJECT_MODEL
        elif name_lower.startswith("vdm"):
            return CDSAnnotationCategory.VDM
        elif name_lower.startswith("accesscontrol"):
            return CDSAnnotationCategory.ACCESS_CONTROL
        elif name_lower.startswith("metadata"):
            return CDSAnnotationCategory.METADATA
        elif name_lower.startswith("endusertext"):
            return CDSAnnotationCategory.END_USER_TEXT
        elif name_lower.startswith("search"):
            return CDSAnnotationCategory.SEARCH
        else:
            return CDSAnnotationCategory.OTHER

    def _process_special_annotation(self, name: str, value: Any, view_info: CDSViewInfo):
        """Processa anotacoes com significado especial"""
        name_lower = name.lower()

        if name_lower == "abapcatalog.sqlviewname":
            view_info.sql_view_name = str(value).strip("'\"")

        elif name_lower == "endusertext.label":
            view_info.description = str(value).strip("'\"")

        elif name_lower == "vdm.viewtype":
            type_value = str(value).strip("#'\"").upper()
            try:
                view_info.view_type = CDSViewType[type_value]
            except KeyError:
                pass

        elif name_lower == "odata.publish":
            view_info.odata_published = str(value).lower() == "true"

        elif name_lower == "analytics.dataextraction.enabled":
            view_info.analytics_enabled = str(value).lower() == "true"

        elif name_lower == "objectmodel.draftenabled":
            view_info.is_draft_enabled = str(value).lower() == "true"

    def _extract_view_definition(self, source: str, view_info: CDSViewInfo):
        """Extrai definicao principal da view"""
        match = self.VIEW_HEADER_PATTERN.search(source)
        if match:
            view_info.is_root = match.group(1) is not None
            view_info.name = match.group(2)
            view_info.base_entity = match.group(4)
            view_info.source_entities = [match.group(4)]

            # Extrair parametros se existirem
            params_str = match.group(3)
            if params_str:
                self._parse_parameters(params_str, view_info)

    def _parse_parameters(self, params_str: str, view_info: CDSViewInfo):
        """Parse de parametros da view"""
        # Formato: p_param : type
        param_pattern = re.compile(r"(\w+)\s*:\s*(\w+(?:\(\d+\))?)")

        for match in param_pattern.finditer(params_str):
            param = CDSParameter(
                name=match.group(1),
                data_type=match.group(2)
            )
            view_info.parameters.append(param)

    def _extract_fields(self, source: str, view_info: CDSViewInfo):
        """Extrai campos da view"""
        # Encontrar bloco de selecao (entre { e })
        select_start = source.find("{")
        select_end = source.rfind("}")

        if select_start > 0 and select_end > select_start:
            select_block = source[select_start + 1:select_end]

            # Processar cada linha
            lines = select_block.split(",")
            for line in lines:
                line = line.strip()
                if not line or line.startswith("//") or line.startswith("association"):
                    continue

                # Tentar fazer parse do campo
                field = self._parse_field_line(line)
                if field:
                    view_info.fields.append(field)

    def _parse_field_line(self, line: str) -> Optional[CDSField]:
        """Parse de uma linha de campo"""
        line = line.strip()
        if not line:
            return None

        # Verificar se e chave
        is_key = line.lower().startswith("key ")
        if is_key:
            line = line[4:].strip()

        # Regex mais flexivel
        match = re.match(
            r"(?:(\w+)\.)?(\w+)(?:\s+as\s+(\w+))?",
            line,
            re.IGNORECASE
        )

        if match:
            source_entity = match.group(1)
            field_name = match.group(2)
            alias = match.group(3)

            return CDSField(
                name=alias or field_name,
                alias=alias,
                source_field=field_name,
                source_entity=source_entity,
                is_key=is_key
            )

        return None

    def _extract_associations(self, source: str, view_info: CDSViewInfo):
        """Extrai associacoes da view"""
        for match in self.ASSOCIATION_PATTERN.finditer(source):
            cardinality = match.group(1)
            target = match.group(2)
            name = match.group(3)
            condition = match.group(4).strip()

            association = CDSAssociation(
                name=name,
                target_entity=target,
                cardinality=cardinality,
                condition=condition
            )
            view_info.associations.append(association)

    def _calculate_complexity(self, view_info: CDSViewInfo) -> int:
        """
        Calcula score de complexidade da view

        Fatores considerados:
        - Numero de campos
        - Numero de associacoes
        - Numero de parametros
        - Presenca de anotacoes complexas
        """
        score = 0

        # Campos: 1 ponto cada
        score += len(view_info.fields)

        # Associacoes: 3 pontos cada
        score += len(view_info.associations) * 3

        # Parametros: 2 pontos cada
        score += len(view_info.parameters) * 2

        # Anotacoes UI: 1 ponto cada
        ui_annotations = view_info.get_annotations_by_category(CDSAnnotationCategory.UI)
        score += len(ui_annotations)

        # Analytics enabled: +5
        if view_info.analytics_enabled:
            score += 5

        # Draft enabled: +5
        if view_info.is_draft_enabled:
            score += 5

        return score

    async def analyze_view(self, view_name: str) -> CDSViewInfo:
        """
        Analisa CDS View do sistema via OData

        Args:
            view_name: Nome da view (ex: "I_SalesOrder")

        Returns:
            CDSViewInfo com dados da view
        """
        if view_name in self._view_cache:
            return self._view_cache[view_name]

        if not self.odata_client:
            raise ValueError("Cliente OData nao configurado para analise remota")

        # Buscar metadados da view via catalogo
        try:
            # Usando o catalogo de introspeccao do Gateway
            response = await self.odata_client.get(
                "EntitySets",
                filter_expr=f"Name eq '{view_name}'"
            )

            if response.items:
                entity_data = response.first
                view_info = self._map_catalog_to_view_info(entity_data)
                self._view_cache[view_name] = view_info
                return view_info

            # Se nao encontrou, criar info basica
            return CDSViewInfo(
                name=view_name,
                description=f"View {view_name} nao encontrada no catalogo"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar view {view_name}: {e}")
            return CDSViewInfo(name=view_name)

    def _map_catalog_to_view_info(self, entity_data: Dict) -> CDSViewInfo:
        """Mapeia dados do catalogo para CDSViewInfo"""
        return CDSViewInfo(
            name=entity_data.get("Name", ""),
            description=entity_data.get("Label", ""),
            sql_view_name=entity_data.get("SqlViewName"),
            odata_published=True  # Esta no catalogo, entao esta publicada
        )

    async def list_views(
        self,
        filter_pattern: Optional[str] = None,
        view_type: Optional[CDSViewType] = None,
        limit: int = 100
    ) -> List[CDSViewInfo]:
        """
        Lista CDS Views disponiveis no sistema

        Args:
            filter_pattern: Padrao de filtro (ex: "Z*" para customizadas)
            view_type: Filtrar por tipo de view
            limit: Limite de resultados

        Returns:
            Lista de CDSViewInfo
        """
        if not self.odata_client:
            raise ValueError("Cliente OData nao configurado")

        filter_expr = None
        if filter_pattern:
            # Converter padrao para filtro OData
            if filter_pattern.endswith("*"):
                prefix = filter_pattern[:-1]
                filter_expr = f"startswith(Name, '{prefix}')"
            else:
                filter_expr = f"Name eq '{filter_pattern}'"

        try:
            response = await self.odata_client.get(
                "EntitySets",
                filter_expr=filter_expr,
                top=limit
            )

            views = []
            for item in response.items:
                view_info = self._map_catalog_to_view_info(item)
                views.append(view_info)

            return views

        except Exception as e:
            logger.error(f"Erro ao listar views: {e}")
            return []

    def get_dependencies(self, view_info: CDSViewInfo) -> Dict[str, List[str]]:
        """
        Analisa dependencias de uma view

        Args:
            view_info: Informacoes da view

        Returns:
            Dict com dependencias categorizadas
        """
        return {
            "source_entities": view_info.source_entities,
            "associations": [a.target_entity for a in view_info.associations],
            "all_dependencies": list(set(
                view_info.source_entities +
                [a.target_entity for a in view_info.associations]
            ))
        }

    def validate_view(self, view_info: CDSViewInfo) -> List[str]:
        """
        Valida uma CDS View e retorna lista de problemas

        Args:
            view_info: View a validar

        Returns:
            Lista de mensagens de validacao
        """
        issues = []

        # Verificar nome
        if not view_info.name:
            issues.append("View sem nome definido")

        # Verificar prefixo para customizacoes
        if view_info.name and not view_info.name.startswith(("I_", "C_", "Z", "Y")):
            issues.append(
                f"Nome '{view_info.name}' nao segue convencao VDM "
                "(deve comecar com I_, C_ ou Z/Y para custom)"
            )

        # Verificar campos chave
        if not view_info.key_fields:
            issues.append("View nao possui campos chave definidos")

        # Verificar anotacoes obrigatorias
        has_sql_view_name = any(
            a.name.lower() == "abapcatalog.sqlviewname"
            for a in view_info.annotations
        )
        if not has_sql_view_name and not view_info.sql_view_name:
            issues.append("Anotacao @AbapCatalog.sqlViewName nao definida")

        # Verificar tipo VDM
        has_vdm_type = any(
            a.name.lower() == "vdm.viewtype"
            for a in view_info.annotations
        )
        if not has_vdm_type:
            issues.append("Anotacao @VDM.viewType recomendada para classificacao")

        return issues
