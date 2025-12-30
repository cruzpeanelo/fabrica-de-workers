# -*- coding: utf-8 -*-
"""
BADI Analyzer
=============
Analisador de BADIs (Business Add-Ins) do SAP.

Este modulo fornece:
- Listagem de BADIs disponiveis
- Analise de interfaces BADI
- Verificacao de implementacoes existentes
- Busca de BADIs por transacao

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.analyzers import BADIAnalyzer

analyzer = BADIAnalyzer(rfc_client)

# Buscar BADIs de vendas
badis = await analyzer.search_badis(area="SD")

# Analisar BADI especifica
badi_info = await analyzer.analyze_badi("BADI_SD_SALES")
print(badi_info.methods)
print(badi_info.implementations)
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class BADIType(str, Enum):
    """Tipo de BADI"""
    CLASSIC = "classic"  # BADI classica (SE18/SE19)
    NEW = "new"  # Nova BADI (Enhancement Framework)


class EnhancementType(str, Enum):
    """Tipo de enhancement"""
    BADI = "BADI"
    USER_EXIT = "EXIT"
    CUSTOMER_EXIT = "CMOD"
    ENHANCEMENT_SPOT = "ENHS"
    ENHANCEMENT_POINT = "ENHP"
    ENHANCEMENT_SECTION = "ENSC"


@dataclass
class BADIMethod:
    """Metodo de uma BADI"""
    name: str
    description: str = ""
    is_abstract: bool = False
    is_final: bool = False
    parameters: List[Dict[str, Any]] = field(default_factory=list)
    exceptions: List[str] = field(default_factory=list)


@dataclass
class BADIImplementation:
    """Implementacao de uma BADI"""
    name: str
    description: str = ""
    implementing_class: str = ""
    is_active: bool = True
    package: str = ""
    created_by: str = ""
    created_on: Optional[datetime] = None
    filter_values: Dict[str, Any] = field(default_factory=dict)


@dataclass
class BADIInfo:
    """Informacoes completas de uma BADI"""
    name: str
    description: str = ""
    badi_type: BADIType = BADIType.NEW
    interface_name: str = ""
    enhancement_spot: str = ""

    # Propriedades
    is_multiple_use: bool = False
    is_filter_dependent: bool = False
    is_fallback_enabled: bool = False

    # Metodos
    methods: List[BADIMethod] = field(default_factory=list)

    # Implementacoes
    implementations: List[BADIImplementation] = field(default_factory=list)

    # Metadata
    package: str = ""
    created_by: str = ""
    created_on: Optional[datetime] = None
    application_area: str = ""

    # Transacoes relacionadas
    related_transactions: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "type": self.badi_type.value,
            "interface": self.interface_name,
            "enhancement_spot": self.enhancement_spot,
            "is_multiple_use": self.is_multiple_use,
            "is_filter_dependent": self.is_filter_dependent,
            "methods_count": len(self.methods),
            "implementations_count": len(self.implementations),
            "methods": [
                {"name": m.name, "description": m.description}
                for m in self.methods
            ],
            "implementations": [
                {"name": i.name, "class": i.implementing_class, "active": i.is_active}
                for i in self.implementations
            ],
            "package": self.package,
            "application_area": self.application_area
        }


@dataclass
class UserExit:
    """User Exit SAP"""
    name: str
    description: str = ""
    exit_type: str = ""  # FORM, FUNCTION, etc.
    program: str = ""
    include: str = ""
    is_implemented: bool = False


@dataclass
class EnhancementSpot:
    """Enhancement Spot"""
    name: str
    description: str = ""
    package: str = ""
    badis: List[str] = field(default_factory=list)
    enhancement_points: List[str] = field(default_factory=list)


class BADIAnalyzer:
    """
    Analisador de BADIs e pontos de extensao SAP.

    Fornece analise de:
    - BADIs classicas e novas
    - User Exits e Customer Exits
    - Enhancement Spots e Points
    - Implementacoes existentes
    """

    # Tabelas de BADIs
    BADI_TABLES = {
        "classic": {
            "definition": "SXS_ATTR",  # Definicao BADI classica
            "interface": "SXS_INTER",  # Interface
            "implementations": "SXC_ATTR"  # Implementacoes
        },
        "new": {
            "definition": "BADI_NAME",  # BADIs novas
            "spot": "ENH_SPOT",  # Enhancement Spots
            "impl": "ENH_IMPL"  # Implementacoes
        }
    }

    # BADIs comuns por area
    COMMON_BADIS = {
        "SD": [
            "BADI_SD_SALES",
            "SD_PARTNER_CREATE",
            "ITEM_CATEGORY",
            "PRICING",
            "DELIVERY_UPDATE"
        ],
        "MM": [
            "ME_PROCESS_PO",
            "ME_PROCESS_REQ",
            "ME_GUI_PO_CUST",
            "MB_MIGO_BADI",
            "ME_PROCESS_OUT"
        ],
        "FI": [
            "BADI_FI_CLEARING",
            "BADI_FI_POSTING",
            "BADI_FI_FBCF",
            "FAGL_BADI_BALANCE"
        ],
        "PP": [
            "WORKORDER_UPDATE",
            "WORKORDER_INFOSYS",
            "WORKORDER_GOODSMVT"
        ],
        "QM": [
            "QEVA_BADI",
            "QM_RESULTS_RECORDING"
        ]
    }

    def __init__(self, rfc_client):
        """
        Inicializa o analisador.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client

    async def search_badis(
        self,
        prefix: str = "",
        area: Optional[str] = None,
        description_contains: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Busca BADIs disponiveis.

        Args:
            prefix: Prefixo do nome
            area: Area de aplicacao (SD, MM, FI, etc.)
            description_contains: Texto na descricao
            max_results: Maximo de resultados

        Returns:
            Lista de BADIs
        """
        # Buscar novas BADIs (Enhancement Framework)
        result = await self._search_new_badis(prefix, area, max_results)

        # Buscar BADIs classicas
        classic = await self._search_classic_badis(prefix, max_results)
        result.extend(classic)

        return result[:max_results]

    async def _search_new_badis(
        self,
        prefix: str,
        area: Optional[str],
        max_results: int
    ) -> List[Dict[str, Any]]:
        """Busca novas BADIs"""
        # Buscar na tabela de BADIs
        options = []
        if prefix:
            options.append({"TEXT": f"BADI_NAME LIKE '{prefix}%'"})

        try:
            badis = await self.rfc_client.read_table(
                "SXS_ATTR",  # Tabela de atributos de BADIs
                fields=["EXIT_NAME", "ACTIVE", "INTERNAL", "SWITCHABLE"],
                options=options if options else None,
                max_rows=max_results
            )

            result = []
            for badi in badis:
                result.append({
                    "name": badi.get("EXIT_NAME", ""),
                    "type": "classic",
                    "is_active": badi.get("ACTIVE", "") == "X",
                    "is_internal": badi.get("INTERNAL", "") == "X"
                })

            return result

        except Exception as e:
            logger.warning(f"Erro ao buscar novas BADIs: {e}")
            return []

    async def _search_classic_badis(
        self,
        prefix: str,
        max_results: int
    ) -> List[Dict[str, Any]]:
        """Busca BADIs classicas"""
        options = []
        if prefix:
            options.append({"TEXT": f"EXIT_NAME LIKE '{prefix}%'"})

        try:
            badis = await self.rfc_client.read_table(
                "SXS_ATTR",
                fields=["EXIT_NAME", "ACTIVE", "MULTIPLE_USE", "FILTER_DEPEND"],
                options=options if options else None,
                max_rows=max_results
            )

            result = []
            for badi in badis:
                result.append({
                    "name": badi.get("EXIT_NAME", ""),
                    "type": "classic",
                    "is_active": badi.get("ACTIVE", "") == "X",
                    "is_multiple_use": badi.get("MULTIPLE_USE", "") == "X",
                    "is_filter_dependent": badi.get("FILTER_DEPEND", "") == "X"
                })

            return result

        except Exception as e:
            logger.warning(f"Erro ao buscar BADIs classicas: {e}")
            return []

    async def analyze_badi(self, badi_name: str) -> BADIInfo:
        """
        Analisa uma BADI em detalhes.

        Args:
            badi_name: Nome da BADI

        Returns:
            BADIInfo com informacoes completas
        """
        badi_name = badi_name.upper()

        # Buscar informacoes basicas
        basic_info = await self._get_badi_basic_info(badi_name)

        if not basic_info:
            return BADIInfo(
                name=badi_name,
                description="BADI nao encontrada"
            )

        # Criar objeto BADIInfo
        badi_info = BADIInfo(
            name=badi_name,
            description=basic_info.get("description", ""),
            badi_type=BADIType.CLASSIC if basic_info.get("type") == "classic" else BADIType.NEW,
            interface_name=basic_info.get("interface", ""),
            is_multiple_use=basic_info.get("is_multiple_use", False),
            is_filter_dependent=basic_info.get("is_filter_dependent", False),
            package=basic_info.get("package", "")
        )

        # Buscar metodos da interface
        if badi_info.interface_name:
            badi_info.methods = await self._get_interface_methods(badi_info.interface_name)

        # Buscar implementacoes
        badi_info.implementations = await self._get_badi_implementations(badi_name)

        return badi_info

    async def _get_badi_basic_info(self, badi_name: str) -> Dict[str, Any]:
        """Obtem informacoes basicas da BADI"""
        try:
            # Buscar na SXS_ATTR
            attrs = await self.rfc_client.read_table(
                "SXS_ATTR",
                fields=["EXIT_NAME", "ACTIVE", "MULTIPLE_USE", "FILTER_DEPEND", "DEVCLASS"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=1
            )

            if attrs:
                attr = attrs[0]

                # Buscar interface na SXS_INTER
                interfaces = await self.rfc_client.read_table(
                    "SXS_INTER",
                    fields=["EXIT_NAME", "INTER_NAME"],
                    options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                    max_rows=1
                )

                interface_name = ""
                if interfaces:
                    interface_name = interfaces[0].get("INTER_NAME", "")

                return {
                    "type": "classic",
                    "interface": interface_name,
                    "is_active": attr.get("ACTIVE", "") == "X",
                    "is_multiple_use": attr.get("MULTIPLE_USE", "") == "X",
                    "is_filter_dependent": attr.get("FILTER_DEPEND", "") == "X",
                    "package": attr.get("DEVCLASS", "")
                }

        except Exception as e:
            logger.warning(f"Erro ao buscar info da BADI {badi_name}: {e}")

        return {}

    async def _get_interface_methods(self, interface_name: str) -> List[BADIMethod]:
        """Obtem metodos de uma interface"""
        methods = []

        try:
            # Buscar metodos na SEOCOMPO
            method_data = await self.rfc_client.read_table(
                "SEOCOMPO",
                fields=["CMPNAME", "CMPTYPE", "DESCRIPT", "EXPOSURE"],
                options=[{"TEXT": f"CLSNAME = '{interface_name}' AND CMPTYPE = '1'"}],
                max_rows=50
            )

            for md in method_data:
                methods.append(BADIMethod(
                    name=md.get("CMPNAME", ""),
                    description=md.get("DESCRIPT", "")
                ))

        except Exception as e:
            logger.warning(f"Erro ao buscar metodos de {interface_name}: {e}")

        return methods

    async def _get_badi_implementations(self, badi_name: str) -> List[BADIImplementation]:
        """Obtem implementacoes de uma BADI"""
        implementations = []

        try:
            # Buscar na SXC_ATTR
            impl_data = await self.rfc_client.read_table(
                "SXC_ATTR",
                fields=["IMP_NAME", "IMP_CLASS", "ACTIVE", "DEVCLASS", "UNAME", "UDATE"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=50
            )

            for impl in impl_data:
                implementations.append(BADIImplementation(
                    name=impl.get("IMP_NAME", ""),
                    implementing_class=impl.get("IMP_CLASS", ""),
                    is_active=impl.get("ACTIVE", "") == "X",
                    package=impl.get("DEVCLASS", ""),
                    created_by=impl.get("UNAME", "")
                ))

        except Exception as e:
            logger.warning(f"Erro ao buscar implementacoes de {badi_name}: {e}")

        return implementations

    async def search_user_exits(
        self,
        program: Optional[str] = None,
        transaction: Optional[str] = None
    ) -> List[UserExit]:
        """
        Busca User Exits.

        Args:
            program: Nome do programa
            transaction: Codigo da transacao

        Returns:
            Lista de User Exits
        """
        exits = []

        try:
            # Se informada transacao, buscar programa correspondente
            if transaction and not program:
                program = await self._get_transaction_program(transaction)

            if program:
                # Buscar includes de user exit (ZXXX)
                options = [{"TEXT": f"NAME LIKE 'ZX%' AND MASTER = '{program}'"}]

                includes = await self.rfc_client.read_table(
                    "D010INC",
                    fields=["INCLUDE", "MASTER"],
                    options=options,
                    max_rows=50
                )

                for inc in includes:
                    exits.append(UserExit(
                        name=inc.get("INCLUDE", ""),
                        exit_type="INCLUDE",
                        program=inc.get("MASTER", "")
                    ))

        except Exception as e:
            logger.warning(f"Erro ao buscar user exits: {e}")

        return exits

    async def _get_transaction_program(self, transaction: str) -> Optional[str]:
        """Obtem programa de uma transacao"""
        try:
            data = await self.rfc_client.read_table(
                "TSTC",
                fields=["TCODE", "PGMNA"],
                options=[{"TEXT": f"TCODE = '{transaction}'"}],
                max_rows=1
            )

            if data:
                return data[0].get("PGMNA", "")

        except Exception as e:
            logger.warning(f"Erro ao buscar programa da transacao {transaction}: {e}")

        return None

    async def search_enhancement_spots(
        self,
        prefix: str = "",
        package: Optional[str] = None
    ) -> List[EnhancementSpot]:
        """
        Busca Enhancement Spots.

        Args:
            prefix: Prefixo do nome
            package: Pacote

        Returns:
            Lista de Enhancement Spots
        """
        spots = []

        try:
            options = []
            if prefix:
                options.append({"TEXT": f"ENHSPOT LIKE '{prefix}%'"})
            if package:
                options.append({"TEXT": f"AND DEVCLASS = '{package}'" if options else f"DEVCLASS = '{package}'"})

            # Buscar na ENHSPOTOBJ (ou tabela equivalente)
            data = await self.rfc_client.read_table(
                "ENHSPOTOBJ",
                fields=["ENHSPOT", "ENHSPOTCLASS", "DEVCLASS"],
                options=options if options else None,
                max_rows=50
            )

            for spot in data:
                spots.append(EnhancementSpot(
                    name=spot.get("ENHSPOT", ""),
                    package=spot.get("DEVCLASS", "")
                ))

        except Exception as e:
            logger.warning(f"Erro ao buscar enhancement spots: {e}")

        return spots

    async def find_badis_for_transaction(self, transaction: str) -> List[Dict[str, Any]]:
        """
        Encontra BADIs relevantes para uma transacao.

        Esta funcao tenta encontrar BADIs executando a transacao
        em modo debug ou analisando o programa.

        Args:
            transaction: Codigo da transacao

        Returns:
            Lista de BADIs potencialmente relevantes
        """
        result = []

        # Obter programa da transacao
        program = await self._get_transaction_program(transaction)

        if not program:
            return result

        # Buscar chamadas de BADI no programa
        try:
            # Ler codigo fonte do programa
            source = await self.rfc_client.call_function(
                "RPY_PROGRAM_READ",
                {"PROGRAM_NAME": program, "WITH_LOWERCASE": "X"}
            )

            if source.success:
                source_lines = source.tables.get("SOURCE", [])
                code = "\n".join([l.get("LINE", "") for l in source_lines])

                # Procurar chamadas de BADI (GET BADI ou CL_EXITHANDLER)
                import re

                # GET BADI <nome>
                get_badi_matches = re.findall(
                    r'GET\s+BADI\s+(\w+)',
                    code,
                    re.IGNORECASE
                )

                for match in get_badi_matches:
                    result.append({
                        "name": match,
                        "type": "new",
                        "found_in": program
                    })

                # CL_EXITHANDLER=>GET_INSTANCE( CHANGING instance = <var> )
                exithandler_matches = re.findall(
                    r"CALL\s+METHOD\s+CL_EXITHANDLER=>GET_INSTANCE.*?EXIT_NAME\s*=\s*['\"](\w+)['\"]",
                    code,
                    re.IGNORECASE | re.DOTALL
                )

                for match in exithandler_matches:
                    result.append({
                        "name": match,
                        "type": "classic",
                        "found_in": program
                    })

        except Exception as e:
            logger.warning(f"Erro ao buscar BADIs para transacao {transaction}: {e}")

        return result

    def get_common_badis(self, area: str) -> List[str]:
        """
        Retorna BADIs comuns de uma area.

        Args:
            area: Area SAP (SD, MM, FI, etc.)

        Returns:
            Lista de nomes de BADIs
        """
        return self.COMMON_BADIS.get(area.upper(), [])

    async def get_badi_filter_values(
        self,
        badi_name: str
    ) -> List[Dict[str, Any]]:
        """
        Obtem os valores de filtro de uma BADI filter-dependent.

        Args:
            badi_name: Nome da BADI

        Returns:
            Lista de valores de filtro configurados
        """
        badi_name = badi_name.upper()
        filter_values = []

        try:
            # Buscar definicao de filtros na SXS_ATTRT (tabela de tipos de filtro)
            filter_def = await self.rfc_client.read_table(
                "SXS_ATTRT",
                fields=["EXIT_NAME", "FILTER_FIELD", "FILTER_TYPE", "FILTER_TAB"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=10
            )

            if not filter_def:
                # Tentar tabela alternativa
                filter_def = await self.rfc_client.read_table(
                    "SXS_ATTR",
                    fields=["EXIT_NAME", "FILTER_DEPEND", "FILTER_TAB"],
                    options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                    max_rows=1
                )

            # Buscar valores de filtro das implementacoes na SXC_FILTER
            impl_filters = await self.rfc_client.read_table(
                "SXC_FILTER",
                fields=["EXIT_NAME", "IMP_NAME", "FILT_NAME", "FILT_VAL"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=100
            )

            # Agrupar por implementacao
            by_impl: Dict[str, List[Dict]] = {}
            for fv in impl_filters:
                impl_name = fv.get("IMP_NAME", "")
                if impl_name not in by_impl:
                    by_impl[impl_name] = []

                by_impl[impl_name].append({
                    "filter_name": fv.get("FILT_NAME", ""),
                    "filter_value": fv.get("FILT_VAL", "")
                })

            for impl_name, filters in by_impl.items():
                filter_values.append({
                    "implementation": impl_name,
                    "filters": filters
                })

        except Exception as e:
            logger.warning(f"Erro ao obter filter values de {badi_name}: {e}")

        return filter_values

    async def get_enhancement_spot_details(
        self,
        spot_name: str
    ) -> Dict[str, Any]:
        """
        Obtem detalhes completos de um Enhancement Spot.

        Args:
            spot_name: Nome do enhancement spot

        Returns:
            Dicionario com detalhes do spot
        """
        spot_name = spot_name.upper()
        result = {
            "name": spot_name,
            "description": "",
            "package": "",
            "badis": [],
            "enhancement_points": [],
            "enhancement_sections": [],
            "composite_spots": [],
            "implementations": []
        }

        try:
            # Buscar info basica do spot
            spot_info = await self.rfc_client.read_table(
                "ENHSPOTOBJ",
                fields=["ENHSPOT", "ENHSPOTCLASS", "DEVCLASS", "VERSION"],
                options=[{"TEXT": f"ENHSPOT = '{spot_name}'"}],
                max_rows=1
            )

            if spot_info:
                result["package"] = spot_info[0].get("DEVCLASS", "")

            # Buscar BADIs do spot
            badis = await self.rfc_client.read_table(
                "ENHSPOTOBJ_B",
                fields=["ENHSPOT", "BADI_NAME"],
                options=[{"TEXT": f"ENHSPOT = '{spot_name}'"}],
                max_rows=50
            )

            result["badis"] = [b.get("BADI_NAME", "") for b in badis]

            # Buscar enhancement points
            points = await self.rfc_client.read_table(
                "ENHSPOTOBJ_P",
                fields=["ENHSPOT", "ENHPOINT_NAME", "MAINTYPE"],
                options=[{"TEXT": f"ENHSPOT = '{spot_name}'"}],
                max_rows=50
            )

            for point in points:
                result["enhancement_points"].append({
                    "name": point.get("ENHPOINT_NAME", ""),
                    "type": point.get("MAINTYPE", "")
                })

            # Buscar enhancement sections
            sections = await self.rfc_client.read_table(
                "ENHSPOTOBJ_S",
                fields=["ENHSPOT", "ENHSECTION_NAME"],
                options=[{"TEXT": f"ENHSPOT = '{spot_name}'"}],
                max_rows=50
            )

            result["enhancement_sections"] = [s.get("ENHSECTION_NAME", "") for s in sections]

            # Buscar implementacoes do spot
            impls = await self.rfc_client.read_table(
                "ENHIMPL",
                fields=["ENHNAME", "ENHSPOT", "DEVCLASS", "VERSION"],
                options=[{"TEXT": f"ENHSPOT = '{spot_name}'"}],
                max_rows=50
            )

            for impl in impls:
                result["implementations"].append({
                    "name": impl.get("ENHNAME", ""),
                    "package": impl.get("DEVCLASS", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao obter detalhes do spot {spot_name}: {e}")

        return result

    async def get_badi_active_status(
        self,
        badi_name: str
    ) -> Dict[str, Any]:
        """
        Verifica status de ativacao de uma BADI e suas implementacoes.

        Args:
            badi_name: Nome da BADI

        Returns:
            Dicionario com status de ativacao
        """
        badi_name = badi_name.upper()
        result = {
            "badi": badi_name,
            "is_active": False,
            "implementations": []
        }

        try:
            # Verificar se BADI esta ativa
            badi_attr = await self.rfc_client.read_table(
                "SXS_ATTR",
                fields=["EXIT_NAME", "ACTIVE", "SWITCHABLE"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=1
            )

            if badi_attr:
                result["is_active"] = badi_attr[0].get("ACTIVE", "") == "X"
                result["is_switchable"] = badi_attr[0].get("SWITCHABLE", "") == "X"

            # Buscar implementacoes e status
            impls = await self.rfc_client.read_table(
                "SXC_ATTR",
                fields=["EXIT_NAME", "IMP_NAME", "IMP_CLASS", "ACTIVE", "RUNTIME"],
                options=[{"TEXT": f"EXIT_NAME = '{badi_name}'"}],
                max_rows=50
            )

            for impl in impls:
                result["implementations"].append({
                    "name": impl.get("IMP_NAME", ""),
                    "class": impl.get("IMP_CLASS", ""),
                    "is_active": impl.get("ACTIVE", "") == "X",
                    "runtime": impl.get("RUNTIME", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao verificar status de {badi_name}: {e}")

        return result

    async def analyze_badi_complete(
        self,
        badi_name: str
    ) -> Dict[str, Any]:
        """
        Analise completa de uma BADI incluindo:
        - Informacoes basicas
        - Metodos da interface
        - Implementacoes
        - Filtros
        - Status de ativacao

        Args:
            badi_name: Nome da BADI

        Returns:
            Dicionario com analise completa
        """
        badi_name = badi_name.upper()

        # Obter informacoes basicas
        badi_info = await self.analyze_badi(badi_name)

        # Obter status de ativacao
        status = await self.get_badi_active_status(badi_name)

        # Obter filtros se for filter-dependent
        filter_values = []
        if badi_info.is_filter_dependent:
            filter_values = await self.get_badi_filter_values(badi_name)

        # Montar resultado completo
        result = badi_info.to_dict()
        result["is_active"] = status.get("is_active", False)
        result["is_switchable"] = status.get("is_switchable", False)
        result["filter_values"] = filter_values
        result["implementations_detail"] = status.get("implementations", [])

        return result

    async def get_interface_parameters(
        self,
        interface_name: str
    ) -> Dict[str, Any]:
        """
        Obtem parametros detalhados dos metodos de uma interface BADI.

        Args:
            interface_name: Nome da interface

        Returns:
            Dicionario com metodos e seus parametros
        """
        interface_name = interface_name.upper()
        result = {
            "interface": interface_name,
            "methods": []
        }

        try:
            # Buscar metodos na SEOCOMPO
            methods = await self.rfc_client.read_table(
                "SEOCOMPO",
                fields=["CLSNAME", "CMPNAME", "CMPTYPE", "EXPOSURE"],
                options=[{"TEXT": f"CLSNAME = '{interface_name}' AND CMPTYPE = '1'"}],
                max_rows=50
            )

            for method in methods:
                method_name = method.get("CMPNAME", "")

                # Buscar parametros do metodo
                params = await self.rfc_client.read_table(
                    "SEOSUBCO",
                    fields=["CLSNAME", "CMPNAME", "SCONAME", "SCOTYPE", "PARDECLTYP",
                            "PARPASSTYP", "TYPE", "PAROPTIONL"],
                    options=[{"TEXT": f"CLSNAME = '{interface_name}' AND CMPNAME = '{method_name}'"}],
                    max_rows=50
                )

                method_params = {
                    "importing": [],
                    "exporting": [],
                    "changing": [],
                    "returning": None,
                    "exceptions": []
                }

                for param in params:
                    param_info = {
                        "name": param.get("SCONAME", ""),
                        "type": param.get("TYPE", ""),
                        "optional": param.get("PAROPTIONL", "") == "X",
                        "pass_by": param.get("PARPASSTYP", "")  # 0=value, 1=ref
                    }

                    decl_type = param.get("PARDECLTYP", "")
                    if decl_type == "0":  # Importing
                        method_params["importing"].append(param_info)
                    elif decl_type == "1":  # Exporting
                        method_params["exporting"].append(param_info)
                    elif decl_type == "2":  # Changing
                        method_params["changing"].append(param_info)
                    elif decl_type == "3":  # Returning
                        method_params["returning"] = param_info

                    # Exceptions (scotype = 4)
                    if param.get("SCOTYPE", "") == "4":
                        method_params["exceptions"].append(param_info["name"])

                result["methods"].append({
                    "name": method_name,
                    "visibility": method.get("EXPOSURE", ""),
                    "parameters": method_params
                })

        except Exception as e:
            logger.warning(f"Erro ao obter parametros de {interface_name}: {e}")

        return result

    async def search_customer_exits(
        self,
        component: Optional[str] = None,
        exit_type: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Busca Customer Exits (CMOD/SMOD).

        Args:
            component: Componente SAP (MM, SD, FI, etc.)
            exit_type: Tipo do exit (E=Exit, C=Customer, etc.)

        Returns:
            Lista de customer exits
        """
        exits = []

        try:
            # Buscar projetos de ampliacao (CMOD)
            options = []
            if component:
                options.append({"TEXT": f"COMPONENT LIKE '{component}%'"})

            projects = await self.rfc_client.read_table(
                "MODSAP",  # Enhancement projects
                fields=["NAME", "MEMBER", "MODACT", "TYP"],
                options=options if options else None,
                max_rows=100
            )

            # Buscar exits (SMOD)
            exit_options = []
            if component:
                exit_options.append({"TEXT": f"NAME LIKE '%{component}%'"})

            smod_exits = await self.rfc_client.read_table(
                "MODSAPT",  # Enhancement texts
                fields=["NAME", "SPRSL", "MODTEXT"],
                options=exit_options if exit_options else None,
                max_rows=100
            )

            # Montar resultado
            exit_texts = {e.get("NAME", ""): e.get("MODTEXT", "") for e in smod_exits}

            for proj in projects:
                exits.append({
                    "project": proj.get("NAME", ""),
                    "member": proj.get("MEMBER", ""),
                    "is_active": proj.get("MODACT", "") == "X",
                    "type": proj.get("TYP", ""),
                    "description": exit_texts.get(proj.get("NAME", ""), "")
                })

        except Exception as e:
            logger.warning(f"Erro ao buscar customer exits: {e}")

        return exits

    async def get_implicit_enhancements(
        self,
        program_name: str
    ) -> List[Dict[str, Any]]:
        """
        Busca pontos de enhancement implicitos em um programa.

        Args:
            program_name: Nome do programa ABAP

        Returns:
            Lista de pontos de enhancement implicitos
        """
        enhancements = []
        program_name = program_name.upper()

        try:
            # Buscar enhancement implementations no programa
            impl_data = await self.rfc_client.read_table(
                "ENHOBJ",
                fields=["ENHNAME", "OBJNAME", "OBJTYPE", "VERSION"],
                options=[{"TEXT": f"OBJNAME = '{program_name}'"}],
                max_rows=50
            )

            for impl in impl_data:
                enhancements.append({
                    "enhancement_name": impl.get("ENHNAME", ""),
                    "object_name": impl.get("OBJNAME", ""),
                    "object_type": impl.get("OBJTYPE", ""),
                    "version": impl.get("VERSION", "")
                })

        except Exception as e:
            logger.warning(f"Erro ao buscar implicit enhancements de {program_name}: {e}")

        return enhancements

    async def compare_badi_implementations(
        self,
        badi_name: str
    ) -> Dict[str, Any]:
        """
        Compara todas as implementacoes de uma BADI.

        Args:
            badi_name: Nome da BADI

        Returns:
            Comparacao entre implementacoes
        """
        badi_name = badi_name.upper()

        result = {
            "badi": badi_name,
            "total_implementations": 0,
            "active_implementations": 0,
            "inactive_implementations": 0,
            "implementations_by_package": {},
            "filter_distribution": {}
        }

        try:
            # Obter todas as implementacoes
            impls = await self._get_badi_implementations(badi_name)
            result["total_implementations"] = len(impls)

            active = [i for i in impls if i.is_active]
            result["active_implementations"] = len(active)
            result["inactive_implementations"] = len(impls) - len(active)

            # Agrupar por pacote
            by_package: Dict[str, int] = {}
            for impl in impls:
                pkg = impl.package or "UNKNOWN"
                by_package[pkg] = by_package.get(pkg, 0) + 1

            result["implementations_by_package"] = by_package

            # Distribuicao de filtros
            filter_values = await self.get_badi_filter_values(badi_name)
            for fv in filter_values:
                for f in fv.get("filters", []):
                    filter_name = f.get("filter_name", "")
                    if filter_name not in result["filter_distribution"]:
                        result["filter_distribution"][filter_name] = []
                    result["filter_distribution"][filter_name].append(fv.get("implementation"))

        except Exception as e:
            logger.warning(f"Erro ao comparar implementacoes de {badi_name}: {e}")

        return result

    async def get_new_badi_info(
        self,
        badi_name: str
    ) -> Dict[str, Any]:
        """
        Obtem informacoes de uma nova BADI (Enhancement Framework).

        As novas BADIs usam tabelas diferentes das classicas.

        Args:
            badi_name: Nome da BADI

        Returns:
            Informacoes da nova BADI
        """
        badi_name = badi_name.upper()
        result = {
            "name": badi_name,
            "type": "new",
            "enhancement_spot": "",
            "interface": "",
            "fallback_class": "",
            "is_multiple_use": False,
            "is_filter_dependent": False,
            "methods": [],
            "implementations": []
        }

        try:
            # Buscar na BADI_DATA (tabela de novas BADIs)
            badi_data = await self.rfc_client.read_table(
                "BADI_DATA",
                fields=["BADI_NAME", "ENHSPOT", "INTER_NAME", "FALLBACK_CL",
                        "MULTIPLE_USE", "FILTER_BADI"],
                options=[{"TEXT": f"BADI_NAME = '{badi_name}'"}],
                max_rows=1
            )

            if badi_data:
                bd = badi_data[0]
                result["enhancement_spot"] = bd.get("ENHSPOT", "")
                result["interface"] = bd.get("INTER_NAME", "")
                result["fallback_class"] = bd.get("FALLBACK_CL", "")
                result["is_multiple_use"] = bd.get("MULTIPLE_USE", "") == "X"
                result["is_filter_dependent"] = bd.get("FILTER_BADI", "") == "X"

                # Buscar metodos
                if result["interface"]:
                    interface_info = await self.get_interface_parameters(result["interface"])
                    result["methods"] = interface_info.get("methods", [])

            # Buscar implementacoes na BADI_IMPL
            impls = await self.rfc_client.read_table(
                "BADI_IMPL",
                fields=["BADI_NAME", "IMPL_NAME", "IMPL_CLASS", "ACTIVE"],
                options=[{"TEXT": f"BADI_NAME = '{badi_name}'"}],
                max_rows=50
            )

            for impl in impls:
                result["implementations"].append({
                    "name": impl.get("IMPL_NAME", ""),
                    "class": impl.get("IMPL_CLASS", ""),
                    "is_active": impl.get("ACTIVE", "") == "X"
                })

        except Exception as e:
            logger.warning(f"Erro ao obter nova BADI {badi_name}: {e}")

        return result

    async def list_badis_by_package(
        self,
        package: str
    ) -> List[Dict[str, Any]]:
        """
        Lista todas as BADIs de um pacote.

        Args:
            package: Nome do pacote (DEVCLASS)

        Returns:
            Lista de BADIs do pacote
        """
        package = package.upper()
        badis = []

        try:
            # BADIs classicas
            classic = await self.rfc_client.read_table(
                "SXS_ATTR",
                fields=["EXIT_NAME", "ACTIVE", "MULTIPLE_USE", "FILTER_DEPEND"],
                options=[{"TEXT": f"DEVCLASS = '{package}'"}],
                max_rows=100
            )

            for b in classic:
                badis.append({
                    "name": b.get("EXIT_NAME", ""),
                    "type": "classic",
                    "is_active": b.get("ACTIVE", "") == "X",
                    "is_multiple_use": b.get("MULTIPLE_USE", "") == "X"
                })

            # Novas BADIs
            new_badis = await self.rfc_client.read_table(
                "BADI_DATA",
                fields=["BADI_NAME", "ENHSPOT", "MULTIPLE_USE"],
                options=[{"TEXT": f"DEVCLASS = '{package}'"}],
                max_rows=100
            )

            for b in new_badis:
                badis.append({
                    "name": b.get("BADI_NAME", ""),
                    "type": "new",
                    "enhancement_spot": b.get("ENHSPOT", ""),
                    "is_multiple_use": b.get("MULTIPLE_USE", "") == "X"
                })

        except Exception as e:
            logger.warning(f"Erro ao listar BADIs do pacote {package}: {e}")

        return badis
