# -*- coding: utf-8 -*-
"""
ABAP Generator
==============
Gerador de programas ABAP.

Este modulo fornece:
- Geracao de reports ABAP
- Templates padrao SAP
- Boas praticas de codigo
- Geracao de ALV reports

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.generators import ABAPGenerator

generator = ABAPGenerator()

# Gerar report simples
program = generator.generate_report(
    name="ZLIST_MATERIALS",
    title="Listagem de Materiais",
    tables=["MARA", "MAKT"],
    select_options=["MATNR", "MTART"],
    output_fields=["MATNR", "MAKTX", "MTART"]
)

print(program.source_code)
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class ProgramType(str, Enum):
    """Tipo de programa ABAP"""
    REPORT = "REPORT"
    INCLUDE = "INCLUDE"
    MODULE_POOL = "PROGRAM"
    FUNCTION_POOL = "FUNCTION-POOL"


class ALVType(str, Enum):
    """Tipo de ALV"""
    CLASSIC = "classic"
    OO = "oo"
    SALV = "salv"


@dataclass
class ABAPProgram:
    """Programa ABAP gerado"""
    name: str
    title: str = ""
    program_type: ProgramType = ProgramType.REPORT
    source_code: str = ""
    includes: List[str] = field(default_factory=list)
    author: str = "FABRICA_AGENTES"
    created_at: datetime = field(default_factory=datetime.now)
    description: str = ""
    package: str = "$TMP"

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "title": self.title,
            "type": self.program_type.value,
            "source_code": self.source_code,
            "includes": self.includes,
            "author": self.author,
            "description": self.description,
            "package": self.package
        }


class ABAPGenerator:
    """
    Gerador de programas ABAP.

    Fornece templates e geracao de:
    - Reports simples
    - ALV reports (classico e OO)
    - Reports com selection screen
    - Batch input programs
    """

    # Templates ABAP
    TEMPLATES = {
        "header": '''*&---------------------------------------------------------------------*
*& Report {name}
*&---------------------------------------------------------------------*
*& Titulo: {title}
*& Descricao: {description}
*& Autor: {author}
*& Data: {date}
*&---------------------------------------------------------------------*
''',

        "report_declaration": '''REPORT {name}.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
{tables}

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
{types}

*----------------------------------------------------------------------*
* Variaveis Globais
*----------------------------------------------------------------------*
{data}

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
{selection_screen}
''',

        "selection_screen_block": '''SELECTION-SCREEN BEGIN OF BLOCK {block_name} WITH FRAME TITLE TEXT-{text_id}.
{parameters}
SELECTION-SCREEN END OF BLOCK {block_name}.
''',

        "alv_data": '''*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_events   TYPE slis_t_event,
      gs_variant  TYPE disvariant.
''',

        "initialization": '''*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
{init_code}
''',

        "start_of_selection": '''*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
{main_code}
''',

        "end_of_selection": '''*----------------------------------------------------------------------*
* End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
{end_code}
''',

        "form_template": '''*&---------------------------------------------------------------------*
*& Form {form_name}
*&---------------------------------------------------------------------*
*& {description}
*&---------------------------------------------------------------------*
FORM {form_name} {using_params} {changing_params}.
{code}
ENDFORM.
'''
    }

    def __init__(self):
        """Inicializa o gerador"""
        self.indent = "  "

    def generate_report(
        self,
        name: str,
        title: str = "",
        description: str = "",
        tables: Optional[List[str]] = None,
        select_options: Optional[List[Dict[str, str]]] = None,
        parameters: Optional[List[Dict[str, str]]] = None,
        output_fields: Optional[List[Dict[str, str]]] = None,
        alv_type: Optional[ALVType] = None,
        author: str = "FABRICA_AGENTES"
    ) -> ABAPProgram:
        """
        Gera um report ABAP completo.

        Args:
            name: Nome do programa (ex: ZLIST_MATERIALS)
            title: Titulo do report
            description: Descricao
            tables: Tabelas SAP usadas
            select_options: Lista de SELECT-OPTIONS
            parameters: Lista de PARAMETERS
            output_fields: Campos de saida
            alv_type: Tipo de ALV (se usar)
            author: Autor

        Returns:
            ABAPProgram com o codigo gerado
        """
        name = name.upper()
        tables = tables or []
        select_options = select_options or []
        parameters = parameters or []
        output_fields = output_fields or []

        # Gerar partes do codigo
        header = self._generate_header(name, title, description, author)
        tables_section = self._generate_tables_section(tables)
        types_section = self._generate_types_section(output_fields)
        data_section = self._generate_data_section(output_fields, alv_type)
        selection_screen = self._generate_selection_screen(select_options, parameters)

        # Inicializacao
        init_code = self._generate_initialization()

        # Codigo principal
        main_code = self._generate_main_logic(tables, select_options, output_fields)

        # Saida (ALV ou lista simples)
        if alv_type:
            end_code = self._generate_alv_output(alv_type, output_fields)
        else:
            end_code = self._generate_list_output(output_fields)

        # Montar codigo completo
        source_code = header

        source_code += self.TEMPLATES["report_declaration"].format(
            name=name,
            tables=tables_section,
            types=types_section,
            data=data_section,
            selection_screen=selection_screen
        )

        if alv_type:
            source_code += self.TEMPLATES["alv_data"]

        source_code += self.TEMPLATES["initialization"].format(init_code=init_code)
        source_code += self.TEMPLATES["start_of_selection"].format(main_code=main_code)
        source_code += self.TEMPLATES["end_of_selection"].format(end_code=end_code)

        # Forms auxiliares
        source_code += self._generate_forms(alv_type)

        return ABAPProgram(
            name=name,
            title=title,
            description=description,
            source_code=source_code,
            author=author
        )

    def _generate_header(
        self,
        name: str,
        title: str,
        description: str,
        author: str
    ) -> str:
        """Gera cabecalho do programa"""
        return self.TEMPLATES["header"].format(
            name=name,
            title=title or name,
            description=description or "Programa gerado automaticamente",
            author=author,
            date=datetime.now().strftime("%d.%m.%Y")
        )

    def _generate_tables_section(self, tables: List[str]) -> str:
        """Gera secao TABLES"""
        if not tables:
            return "* Nenhuma tabela declarada"

        lines = []
        for table in tables:
            lines.append(f"TABLES: {table.lower()}.")

        return "\n".join(lines)

    def _generate_types_section(self, output_fields: List[Dict[str, str]]) -> str:
        """Gera secao TYPES"""
        if not output_fields:
            return "* Nenhum type customizado"

        lines = [
            "TYPES: BEGIN OF ty_output,"
        ]

        for field in output_fields:
            field_name = field.get("name", "")
            field_type = field.get("type", "string")
            lines.append(f"         {field_name.lower()} TYPE {field_type.lower()},")

        # Remover virgula da ultima linha
        if lines:
            lines[-1] = lines[-1].rstrip(",") + "."

        lines.append("       END OF ty_output.")
        lines.append("")
        lines.append("TYPES: tt_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.")

        return "\n".join(lines)

    def _generate_data_section(
        self,
        output_fields: List[Dict[str, str]],
        alv_type: Optional[ALVType]
    ) -> str:
        """Gera secao DATA"""
        lines = [
            "DATA: gt_output TYPE tt_output,",
            "      gs_output TYPE ty_output,",
            "      gv_lines  TYPE i."
        ]

        return "\n".join(lines)

    def _generate_selection_screen(
        self,
        select_options: List[Dict[str, str]],
        parameters: List[Dict[str, str]]
    ) -> str:
        """Gera SELECTION-SCREEN"""
        if not select_options and not parameters:
            return "* Selection screen vazia"

        lines = [
            'SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.'
        ]

        # SELECT-OPTIONS
        for so in select_options:
            name = so.get("name", "")
            table_field = so.get("for", "")
            obligatory = " OBLIGATORY" if so.get("obligatory", False) else ""
            lines.append(f"SELECT-OPTIONS: s_{name[:8].lower()} FOR {table_field}{obligatory}.")

        # PARAMETERS
        for param in parameters:
            name = param.get("name", "")
            param_type = param.get("type", "char10")
            default = param.get("default", "")
            obligatory = " OBLIGATORY" if param.get("obligatory", False) else ""

            if default:
                lines.append(f"PARAMETERS: p_{name[:8].lower()} TYPE {param_type} DEFAULT '{default}'{obligatory}.")
            else:
                lines.append(f"PARAMETERS: p_{name[:8].lower()} TYPE {param_type}{obligatory}.")

        lines.append('SELECTION-SCREEN END OF BLOCK b1.')

        return "\n".join(lines)

    def _generate_initialization(self) -> str:
        """Gera codigo de INITIALIZATION"""
        return self.indent + "* Valores default\n"

    def _generate_main_logic(
        self,
        tables: List[str],
        select_options: List[Dict[str, str]],
        output_fields: List[Dict[str, str]]
    ) -> str:
        """Gera logica principal do programa"""
        if not tables:
            return self.indent + "* Implementar logica de busca\n"

        lines = []

        # Gerar SELECT principal
        main_table = tables[0] if tables else "mara"
        fields = [f.get("name", "").lower() for f in output_fields] or ["*"]

        lines.append(self.indent + "* Buscar dados")
        lines.append(self.indent + f"SELECT {' '.join(fields)}")
        lines.append(self.indent + f"  INTO TABLE gt_output")
        lines.append(self.indent + f"  FROM {main_table.lower()}")

        # WHERE com SELECT-OPTIONS
        if select_options:
            where_parts = []
            for so in select_options:
                name = so.get("name", "")
                field = so.get("for", "").split("-")[-1] if "-" in so.get("for", "") else so.get("name", "")
                where_parts.append(f"{field.lower()} IN s_{name[:8].lower()}")

            lines.append(self.indent + f"  WHERE {' AND '.join(where_parts)}.")
        else:
            lines.append(self.indent + "  UP TO 1000 ROWS.")

        lines.append("")
        lines.append(self.indent + "* Verificar resultado")
        lines.append(self.indent + "IF sy-subrc <> 0.")
        lines.append(self.indent + "  MESSAGE 'Nenhum registro encontrado' TYPE 'I'.")
        lines.append(self.indent + "  RETURN.")
        lines.append(self.indent + "ENDIF.")
        lines.append("")
        lines.append(self.indent + "DESCRIBE TABLE gt_output LINES gv_lines.")

        return "\n".join(lines)

    def _generate_list_output(self, output_fields: List[Dict[str, str]]) -> str:
        """Gera saida em lista simples"""
        lines = [
            self.indent + "* Exibir resultado",
            self.indent + "LOOP AT gt_output INTO gs_output."
        ]

        if output_fields:
            write_fields = " ".join([f"gs_output-{f.get('name', '').lower()}" for f in output_fields])
            lines.append(self.indent + f"  WRITE: / {write_fields}.")
        else:
            lines.append(self.indent + "  WRITE: / gs_output.")

        lines.append(self.indent + "ENDLOOP.")
        lines.append("")
        lines.append(self.indent + f"WRITE: / 'Total:', gv_lines, 'registros'.")

        return "\n".join(lines)

    def _generate_alv_output(
        self,
        alv_type: ALVType,
        output_fields: List[Dict[str, str]]
    ) -> str:
        """Gera saida ALV"""
        if alv_type == ALVType.SALV:
            return self._generate_salv_output()
        elif alv_type == ALVType.OO:
            return self._generate_alv_oo_output()
        else:
            return self._generate_alv_classic_output(output_fields)

    def _generate_alv_classic_output(self, output_fields: List[Dict[str, str]]) -> str:
        """Gera ALV classico"""
        lines = [
            self.indent + "* Preparar ALV",
            self.indent + "PERFORM build_fieldcat.",
            self.indent + "PERFORM build_layout.",
            "",
            self.indent + "* Exibir ALV",
            self.indent + "CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'",
            self.indent + "  EXPORTING",
            self.indent + "    i_callback_program = sy-repid",
            self.indent + "    is_layout          = gs_layout",
            self.indent + "    it_fieldcat        = gt_fieldcat",
            self.indent + "  TABLES",
            self.indent + "    t_outtab           = gt_output",
            self.indent + "  EXCEPTIONS",
            self.indent + "    program_error      = 1",
            self.indent + "    OTHERS             = 2.",
            "",
            self.indent + "IF sy-subrc <> 0.",
            self.indent + "  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno",
            self.indent + "    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.",
            self.indent + "ENDIF."
        ]
        return "\n".join(lines)

    def _generate_salv_output(self) -> str:
        """Gera SALV output"""
        lines = [
            self.indent + "* SALV Output",
            self.indent + "DATA: lo_alv TYPE REF TO cl_salv_table.",
            "",
            self.indent + "TRY.",
            self.indent + "    cl_salv_table=>factory(",
            self.indent + "      IMPORTING r_salv_table = lo_alv",
            self.indent + "      CHANGING  t_table      = gt_output ).",
            "",
            self.indent + "    lo_alv->get_columns( )->set_optimize( abap_true ).",
            self.indent + "    lo_alv->display( ).",
            "",
            self.indent + "  CATCH cx_salv_msg INTO DATA(lx_msg).",
            self.indent + "    MESSAGE lx_msg->get_text( ) TYPE 'E'.",
            self.indent + "ENDTRY."
        ]
        return "\n".join(lines)

    def _generate_alv_oo_output(self) -> str:
        """Gera ALV OO output"""
        lines = [
            self.indent + "* ALV OO Output",
            self.indent + "DATA: lo_container TYPE REF TO cl_gui_custom_container,",
            self.indent + "      lo_grid      TYPE REF TO cl_gui_alv_grid.",
            "",
            self.indent + "* Para ALV OO fullscreen, use cl_gui_alv_grid com cl_gui_container",
            self.indent + "CREATE OBJECT lo_grid",
            self.indent + "  EXPORTING",
            self.indent + "    i_parent = cl_gui_container=>screen0.",
            "",
            self.indent + "lo_grid->set_table_for_first_display(",
            self.indent + "  CHANGING it_outtab = gt_output )."
        ]
        return "\n".join(lines)

    def _generate_forms(self, alv_type: Optional[ALVType]) -> str:
        """Gera FORMs auxiliares"""
        forms = []

        if alv_type == ALVType.CLASSIC:
            # Form build_fieldcat
            forms.append('''
*&---------------------------------------------------------------------*
*& Form build_fieldcat
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'CAMPO1'.
  gs_fieldcat-seltext_m = 'Campo 1'.
  gs_fieldcat-outputlen = 20.
  APPEND gs_fieldcat TO gt_fieldcat.

  * Adicionar mais campos conforme necessario
ENDFORM.
''')

            # Form build_layout
            forms.append('''
*&---------------------------------------------------------------------*
*& Form build_layout
*&---------------------------------------------------------------------*
FORM build_layout.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
ENDFORM.
''')

        return "\n".join(forms)

    def generate_include(
        self,
        name: str,
        title: str = "",
        content: str = ""
    ) -> ABAPProgram:
        """
        Gera um include ABAP.

        Args:
            name: Nome do include
            title: Titulo
            content: Conteudo do include

        Returns:
            ABAPProgram com o include
        """
        header = self._generate_header(name, title, "", "FABRICA_AGENTES")

        source_code = header
        source_code += content or "* Include vazio\n"

        return ABAPProgram(
            name=name,
            title=title,
            program_type=ProgramType.INCLUDE,
            source_code=source_code
        )

    def generate_batch_input(
        self,
        name: str,
        transaction: str,
        title: str = "",
        description: str = ""
    ) -> ABAPProgram:
        """
        Gera estrutura para Batch Input.

        Args:
            name: Nome do programa
            transaction: Transacao a ser executada
            title: Titulo
            description: Descricao

        Returns:
            ABAPProgram com estrutura de batch input
        """
        header = self._generate_header(name, title, description, "FABRICA_AGENTES")

        source_code = header + f'''
REPORT {name}.

*----------------------------------------------------------------------*
* Batch Input para transacao {transaction}
*----------------------------------------------------------------------*

DATA: gt_bdcdata TYPE TABLE OF bdcdata,
      gs_bdcdata TYPE bdcdata,
      gt_msgs    TYPE TABLE OF bdcmsgcoll,
      gs_msg     TYPE bdcmsgcoll.

DATA: gv_mode TYPE c VALUE 'N'. " N=Background, A=Display all, E=Display errors

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Preencher BDCDATA
  PERFORM fill_bdcdata.

* Executar transacao
  CALL TRANSACTION '{transaction}'
    USING gt_bdcdata
    MODE gv_mode
    MESSAGES INTO gt_msgs.

* Processar mensagens
  LOOP AT gt_msgs INTO gs_msg.
    WRITE: / gs_msg-msgtyp, gs_msg-msgid, gs_msg-msgnr,
             gs_msg-msgv1, gs_msg-msgv2, gs_msg-msgv3, gs_msg-msgv4.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Form fill_bdcdata
*&---------------------------------------------------------------------*
FORM fill_bdcdata.
* Exemplo de preenchimento:

* Tela inicial
  PERFORM bdc_dynpro USING 'SAPMXXXX' '0100'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTER'.
  PERFORM bdc_field  USING 'CAMPO' 'VALOR'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form bdc_dynpro
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING p_program p_dynpro.
  CLEAR gs_bdcdata.
  gs_bdcdata-program  = p_program.
  gs_bdcdata-dynpro   = p_dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
FORM bdc_field USING p_fnam p_fval.
  CLEAR gs_bdcdata.
  gs_bdcdata-fnam = p_fnam.
  gs_bdcdata-fval = p_fval.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.
'''

        return ABAPProgram(
            name=name,
            title=title,
            description=description,
            source_code=source_code
        )
