# -*- coding: utf-8 -*-
"""
Function Module Generator
=========================
Gerador de Function Modules ABAP.

Este modulo fornece:
- Geracao de Function Modules RFC
- Geracao de BAPIs
- Templates padronizados
- Documentacao automatica

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.generators import FunctionGenerator

generator = FunctionGenerator()

# Gerar Function Module RFC
fm = generator.generate_function(
    name="Z_GET_MATERIALS",
    group="ZMM_FUNCTIONS",
    description="Obtem lista de materiais",
    importing=[
        {"name": "IV_MTART", "type": "MTART", "optional": True}
    ],
    tables=[
        {"name": "ET_MATERIALS", "type": "MARA"}
    ],
    is_rfc=True
)

print(fm.source_code)
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class FunctionProcessingType(str, Enum):
    """Tipo de processamento da funcao"""
    NORMAL = "N"
    RFC = "R"
    UPDATE = "U"


@dataclass
class FunctionParameter:
    """Parametro de Function Module"""
    name: str
    type_ref: str = ""  # TYPE ou LIKE
    type_value: str = ""  # Nome do tipo
    optional: bool = False
    default_value: str = ""
    description: str = ""
    pass_by_value: bool = False

    def to_abap(self, category: str) -> str:
        """Gera declaracao ABAP do parametro"""
        parts = [f"    {self.name}"]

        if self.type_ref and self.type_value:
            parts.append(f"TYPE {self.type_value}")
        elif self.type_value:
            parts.append(f"LIKE {self.type_value}")

        if self.optional and category == "IMPORTING":
            parts.append("OPTIONAL")

        if self.default_value and category == "IMPORTING":
            parts.append(f"DEFAULT {self.default_value}")

        if self.pass_by_value:
            parts.append("VALUE")

        return " ".join(parts)


@dataclass
class FunctionException:
    """Excecao de Function Module"""
    name: str
    description: str = ""


@dataclass
class FunctionModule:
    """Function Module gerado"""
    name: str
    group: str = ""
    description: str = ""
    processing_type: FunctionProcessingType = FunctionProcessingType.NORMAL

    importing: List[FunctionParameter] = field(default_factory=list)
    exporting: List[FunctionParameter] = field(default_factory=list)
    changing: List[FunctionParameter] = field(default_factory=list)
    tables: List[FunctionParameter] = field(default_factory=list)
    exceptions: List[FunctionException] = field(default_factory=list)

    source_code: str = ""
    is_rfc: bool = False
    author: str = "FABRICA_AGENTES"
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "group": self.group,
            "description": self.description,
            "is_rfc": self.is_rfc,
            "importing": [p.name for p in self.importing],
            "exporting": [p.name for p in self.exporting],
            "changing": [p.name for p in self.changing],
            "tables": [p.name for p in self.tables],
            "exceptions": [e.name for e in self.exceptions],
            "source_code": self.source_code
        }


class FunctionGenerator:
    """
    Gerador de Function Modules ABAP.

    Fornece templates e geracao de:
    - Function Modules normais
    - Function Modules RFC
    - BAPIs padronizadas
    """

    HEADER_TEMPLATE = '''FUNCTION {name}.
*"----------------------------------------------------------------------
*"*"Interface local:
{interface}
*"----------------------------------------------------------------------
'''

    INTERFACE_TEMPLATE = '''*"  {category}
{parameters}'''

    def __init__(self):
        """Inicializa o gerador"""
        self.indent = "  "

    def generate_function(
        self,
        name: str,
        group: str = "",
        description: str = "",
        importing: Optional[List[Dict[str, Any]]] = None,
        exporting: Optional[List[Dict[str, Any]]] = None,
        changing: Optional[List[Dict[str, Any]]] = None,
        tables: Optional[List[Dict[str, Any]]] = None,
        exceptions: Optional[List[str]] = None,
        is_rfc: bool = False,
        logic: str = ""
    ) -> FunctionModule:
        """
        Gera um Function Module.

        Args:
            name: Nome da funcao (ex: Z_GET_MATERIALS)
            group: Grupo de funcoes
            description: Descricao
            importing: Parametros de entrada
            exporting: Parametros de saida
            changing: Parametros changing
            tables: Tabelas
            exceptions: Excecoes
            is_rfc: Se e Function Module RFC
            logic: Codigo da logica

        Returns:
            FunctionModule com o codigo gerado
        """
        name = name.upper()

        # Converter parametros para objetos
        import_params = self._parse_parameters(importing or [])
        export_params = self._parse_parameters(exporting or [])
        change_params = self._parse_parameters(changing or [])
        table_params = self._parse_parameters(tables or [])
        exc_list = [FunctionException(name=e) for e in (exceptions or [])]

        # Adicionar excecoes padrao para RFC
        if is_rfc and not any(e.name == "SYSTEM_FAILURE" for e in exc_list):
            exc_list.append(FunctionException(name="SYSTEM_FAILURE"))
            exc_list.append(FunctionException(name="COMMUNICATION_FAILURE"))

        # Gerar interface
        interface = self._generate_interface(
            import_params, export_params, change_params, table_params, exc_list
        )

        # Gerar cabecalho
        header = self.HEADER_TEMPLATE.format(
            name=name,
            interface=interface
        )

        # Gerar codigo
        source_code = header
        source_code += self._generate_body(
            description, import_params, export_params,
            table_params, logic, is_rfc
        )
        source_code += "\nENDFUNCTION.\n"

        return FunctionModule(
            name=name,
            group=group,
            description=description,
            processing_type=FunctionProcessingType.RFC if is_rfc else FunctionProcessingType.NORMAL,
            importing=import_params,
            exporting=export_params,
            changing=change_params,
            tables=table_params,
            exceptions=exc_list,
            source_code=source_code,
            is_rfc=is_rfc
        )

    def _parse_parameters(
        self,
        params: List[Dict[str, Any]]
    ) -> List[FunctionParameter]:
        """Converte lista de dicts para FunctionParameter"""
        result = []
        for p in params:
            result.append(FunctionParameter(
                name=p.get("name", "").upper(),
                type_ref=p.get("type_ref", "TYPE"),
                type_value=p.get("type", p.get("type_value", "")),
                optional=p.get("optional", False),
                default_value=p.get("default", ""),
                description=p.get("description", ""),
                pass_by_value=p.get("pass_by_value", False)
            ))
        return result

    def _generate_interface(
        self,
        importing: List[FunctionParameter],
        exporting: List[FunctionParameter],
        changing: List[FunctionParameter],
        tables: List[FunctionParameter],
        exceptions: List[FunctionException]
    ) -> str:
        """Gera secao de interface"""
        sections = []

        if importing:
            params = "\n".join([f'*"     {p.to_abap("IMPORTING")}' for p in importing])
            sections.append(f'*"  IMPORTING\n{params}')

        if exporting:
            params = "\n".join([f'*"     {p.to_abap("EXPORTING")}' for p in exporting])
            sections.append(f'*"  EXPORTING\n{params}')

        if changing:
            params = "\n".join([f'*"     {p.to_abap("CHANGING")}' for p in changing])
            sections.append(f'*"  CHANGING\n{params}')

        if tables:
            params = "\n".join([f'*"     {p.name} STRUCTURE {p.type_value}' for p in tables])
            sections.append(f'*"  TABLES\n{params}')

        if exceptions:
            exc_list = "\n".join([f'*"     {e.name}' for e in exceptions])
            sections.append(f'*"  EXCEPTIONS\n{exc_list}')

        return "\n".join(sections)

    def _generate_body(
        self,
        description: str,
        importing: List[FunctionParameter],
        exporting: List[FunctionParameter],
        tables: List[FunctionParameter],
        logic: str,
        is_rfc: bool
    ) -> str:
        """Gera corpo da funcao"""
        lines = []

        # Descricao
        if description:
            lines.append(f"* {description}")
            lines.append("*")

        # Variaveis locais
        lines.append("* Variaveis locais")
        lines.append("  DATA: lv_subrc TYPE sy-subrc.")
        lines.append("")

        # Logica
        if logic:
            lines.append(logic)
        else:
            lines.append("* TODO: Implementar logica")
            lines.append("")

            # Gerar exemplo de preenchimento de tabela de saida
            if tables:
                table_name = tables[0].name
                table_type = tables[0].type_value

                lines.append(f"* Exemplo: Preencher {table_name}")
                lines.append(f"* SELECT * FROM {table_type.lower()} INTO TABLE {table_name}")
                lines.append("*   UP TO 100 ROWS.")
                lines.append("")

        return "\n".join(lines)

    def generate_bapi(
        self,
        name: str,
        group: str = "",
        description: str = "",
        object_type: str = "",
        importing: Optional[List[Dict[str, Any]]] = None,
        exporting: Optional[List[Dict[str, Any]]] = None,
        tables: Optional[List[Dict[str, Any]]] = None
    ) -> FunctionModule:
        """
        Gera uma BAPI (Business Application Programming Interface).

        BAPIs seguem padrao especifico:
        - Nome comeca com BAPI_
        - Sempre RFC enabled
        - Sempre tem tabela RETURN
        - Padrao de mensagens

        Args:
            name: Nome da BAPI
            group: Grupo de funcoes
            description: Descricao
            object_type: Tipo de objeto (MATERIAL, CUSTOMER, etc.)
            importing: Parametros de entrada
            exporting: Parametros de saida
            tables: Tabelas

        Returns:
            FunctionModule com a BAPI
        """
        # Garantir prefixo BAPI_
        if not name.upper().startswith("BAPI_"):
            name = f"BAPI_{name}"

        name = name.upper()

        # Garantir tabela RETURN
        tables = tables or []
        if not any(t.get("name", "").upper() == "RETURN" for t in tables):
            tables.append({
                "name": "RETURN",
                "type": "BAPIRET2"
            })

        # Logica padrao de BAPI
        logic = self._generate_bapi_logic(object_type, importing or [], tables)

        return self.generate_function(
            name=name,
            group=group,
            description=description,
            importing=importing,
            exporting=exporting,
            tables=tables,
            is_rfc=True,
            logic=logic
        )

    def _generate_bapi_logic(
        self,
        object_type: str,
        importing: List[Dict[str, Any]],
        tables: List[Dict[str, Any]]
    ) -> str:
        """Gera logica padrao de BAPI"""
        lines = [
            "* BAPI Logic",
            "  DATA: ls_return TYPE bapiret2.",
            "",
            "* Validacoes iniciais"
        ]

        # Validacoes de parametros obrigatorios
        for param in importing:
            if not param.get("optional", False):
                param_name = param.get("name", "")
                lines.append(f"  IF {param_name} IS INITIAL.")
                lines.append(f"    CLEAR ls_return.")
                lines.append(f"    ls_return-type = 'E'.")
                lines.append(f"    ls_return-id = 'ZBAPI'.")
                lines.append(f"    ls_return-number = '001'.")
                lines.append(f"    ls_return-message = 'Parametro {param_name} e obrigatorio'.")
                lines.append(f"    APPEND ls_return TO return.")
                lines.append(f"    RETURN.")
                lines.append(f"  ENDIF.")
                lines.append("")

        lines.append("* Processar logica principal")
        lines.append("  TRY.")
        lines.append("*     TODO: Implementar logica de negocio")
        lines.append("")
        lines.append("*     Sucesso")
        lines.append("      CLEAR ls_return.")
        lines.append("      ls_return-type = 'S'.")
        lines.append("      ls_return-id = 'ZBAPI'.")
        lines.append("      ls_return-number = '000'.")
        lines.append("      ls_return-message = 'Processado com sucesso'.")
        lines.append("      APPEND ls_return TO return.")
        lines.append("")
        lines.append("    CATCH cx_root INTO DATA(lx_error).")
        lines.append("      CLEAR ls_return.")
        lines.append("      ls_return-type = 'E'.")
        lines.append("      ls_return-id = 'ZBAPI'.")
        lines.append("      ls_return-number = '999'.")
        lines.append("      ls_return-message = lx_error->get_text( ).")
        lines.append("      APPEND ls_return TO return.")
        lines.append("  ENDTRY.")

        return "\n".join(lines)

    def generate_rfc_wrapper(
        self,
        function_name: str,
        description: str = "",
        group: str = ""
    ) -> FunctionModule:
        """
        Gera um wrapper RFC para funcao existente.

        Cria uma funcao RFC que chama outra funcao interna,
        permitindo acesso remoto.

        Args:
            function_name: Nome da funcao a encapsular
            description: Descricao
            group: Grupo de funcoes

        Returns:
            FunctionModule wrapper
        """
        wrapper_name = f"Z_RFC_{function_name}"
        if wrapper_name.startswith("Z_RFC_Z_"):
            wrapper_name = wrapper_name.replace("Z_RFC_Z_", "Z_RFC_")

        logic = f'''* Wrapper RFC para {function_name}

* Chamar funcao original
  CALL FUNCTION '{function_name}'
*   EXPORTING
*     parametros...
*   IMPORTING
*     parametros...
*   TABLES
*     tabelas...
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc <> 0.
*   Tratar erro
  ENDIF.
'''

        return self.generate_function(
            name=wrapper_name,
            group=group,
            description=f"RFC Wrapper para {function_name}. {description}",
            is_rfc=True,
            logic=logic
        )

    def generate_update_function(
        self,
        name: str,
        group: str = "",
        description: str = "",
        importing: Optional[List[Dict[str, Any]]] = None
    ) -> FunctionModule:
        """
        Gera uma Function Module de Update.

        Funcoes de update sao usadas para atualizacoes
        assincronas no SAP (update task).

        Args:
            name: Nome da funcao
            group: Grupo
            description: Descricao
            importing: Parametros

        Returns:
            FunctionModule de update
        """
        logic = '''* Update Function Module
* Esta funcao e executada na update task (assincrono)

* Realizar atualizacoes no banco
* UPDATE tabela SET ...

* Ou chamar outras funcoes de update
* CALL FUNCTION 'xxx' IN UPDATE TASK
*   EXPORTING ...
'''

        fm = self.generate_function(
            name=name,
            group=group,
            description=description,
            importing=importing,
            logic=logic
        )

        fm.processing_type = FunctionProcessingType.UPDATE

        return fm
