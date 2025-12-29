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
