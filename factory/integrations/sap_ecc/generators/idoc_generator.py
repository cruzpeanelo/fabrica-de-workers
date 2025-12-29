# -*- coding: utf-8 -*-
"""
IDoc Generator
==============
Gerador de IDocs (Intermediate Documents) para integracao SAP.

Este modulo fornece:
- Definicao de tipos de IDoc
- Geracao de estrutura de segmentos
- Templates de processamento
- Codigo de envio e recebimento

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.generators import IDocGenerator

generator = IDocGenerator()

# Definir IDoc customizado
idoc = generator.define_idoc(
    name="ZMATMAS",
    description="IDoc de Material Customizado",
    segments=[
        {
            "name": "Z1MARA",
            "fields": [
                {"name": "MATNR", "type": "MATNR"},
                {"name": "MTART", "type": "MTART"},
                {"name": "MAKTX", "type": "MAKTX"}
            ],
            "parent": None,
            "min_occurs": 1,
            "max_occurs": 1
        }
    ]
)

print(idoc.definition_code)
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class IDocDirection(str, Enum):
    """Direcao do IDoc"""
    INBOUND = "1"  # Entrada no SAP
    OUTBOUND = "2"  # Saida do SAP


class IDocStatus(str, Enum):
    """Status do IDoc"""
    CREATED = "30"  # Criado
    SENT = "03"  # Enviado
    RECEIVED = "50"  # Recebido
    PROCESSED = "53"  # Processado
    ERROR = "51"  # Erro de processamento


@dataclass
class IDocField:
    """Campo de segmento IDoc"""
    name: str
    type_value: str = ""
    length: int = 0
    description: str = ""
    data_element: str = ""
    mandatory: bool = False

    def to_definition(self) -> str:
        """Gera definicao do campo"""
        return f"{self.name} TYPE {self.type_value or 'c'}"


@dataclass
class IDocSegment:
    """Segmento de IDoc"""
    name: str
    description: str = ""
    fields: List[IDocField] = field(default_factory=list)
    parent_segment: Optional[str] = None
    min_occurs: int = 0
    max_occurs: int = 999999
    level: int = 1
    mandatory: bool = False

    def to_structure_definition(self) -> str:
        """Gera definicao de estrutura do segmento"""
        lines = [
            f"*----------------------------------------------------------------------*",
            f"* Segmento: {self.name}",
            f"* Descricao: {self.description}",
            f"*----------------------------------------------------------------------*",
            f"TYPES: BEGIN OF ty_{self.name.lower()},"
        ]

        for fld in self.fields:
            lines.append(f"         {fld.name.lower()} TYPE {fld.type_value},")

        # Remover virgula da ultima linha
        if lines:
            lines[-1] = lines[-1].rstrip(",") + "."

        lines.append(f"       END OF ty_{self.name.lower()}.")

        return "\n".join(lines)


@dataclass
class IDocDefinition:
    """Definicao completa de IDoc"""
    name: str  # Tipo de IDoc (ex: MATMAS, ORDERS)
    basic_type: str = ""  # Tipo basico (ex: MATMAS05)
    extension: str = ""  # Extensao customizada
    description: str = ""
    direction: IDocDirection = IDocDirection.OUTBOUND

    segments: List[IDocSegment] = field(default_factory=list)

    # Codigo gerado
    definition_code: str = ""
    processing_code: str = ""
    sending_code: str = ""

    # Metadata
    message_type: str = ""
    partner_type: str = "LS"  # LS=Logical System, KU=Customer, LI=Vendor
    author: str = "FABRICA_AGENTES"
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "basic_type": self.basic_type,
            "extension": self.extension,
            "description": self.description,
            "direction": self.direction.value,
            "segments_count": len(self.segments),
            "message_type": self.message_type,
            "partner_type": self.partner_type
        }


class IDocGenerator:
    """
    Gerador de IDocs SAP.

    Fornece geracao de:
    - Definicao de tipos de IDoc
    - Estruturas de segmentos
    - Codigo de processamento (inbound)
    - Codigo de envio (outbound)
    """

    # IDocs padrao SAP
    STANDARD_IDOCS = {
        "MATMAS": {
            "description": "Material Master",
            "basic_types": ["MATMAS01", "MATMAS02", "MATMAS03", "MATMAS04", "MATMAS05"],
            "segments": ["E1MARAM", "E1MAKTM", "E1MARCM", "E1MARDM", "E1MBEWM"]
        },
        "ORDERS": {
            "description": "Purchase Order",
            "basic_types": ["ORDERS01", "ORDERS02", "ORDERS03", "ORDERS04", "ORDERS05"],
            "segments": ["E1EDK01", "E1EDK14", "E1EDP01", "E1EDP19"]
        },
        "INVOIC": {
            "description": "Invoice",
            "basic_types": ["INVOIC01", "INVOIC02"],
            "segments": ["E1EDK01", "E1EDKA1", "E1EDP01"]
        },
        "DELVRY": {
            "description": "Delivery",
            "basic_types": ["DELVRY01", "DELVRY02", "DELVRY03"],
            "segments": ["E1EDL20", "E1EDL24", "E1EDL37"]
        },
        "DEBMAS": {
            "description": "Customer Master",
            "basic_types": ["DEBMAS01", "DEBMAS02", "DEBMAS03", "DEBMAS04", "DEBMAS05", "DEBMAS06", "DEBMAS07"],
            "segments": ["E1KNA1M", "E1KNB1M", "E1KNVVM"]
        },
        "CREMAS": {
            "description": "Vendor Master",
            "basic_types": ["CREMAS01", "CREMAS02", "CREMAS03", "CREMAS04", "CREMAS05"],
            "segments": ["E1LFA1M", "E1LFB1M", "E1LFM1M"]
        }
    }

    def __init__(self):
        """Inicializa o gerador"""
        self.indent = "  "

    def define_idoc(
        self,
        name: str,
        description: str = "",
        basic_type: str = "",
        segments: Optional[List[Dict[str, Any]]] = None,
        direction: IDocDirection = IDocDirection.OUTBOUND,
        message_type: str = ""
    ) -> IDocDefinition:
        """
        Define um IDoc customizado.

        Args:
            name: Nome do tipo de IDoc
            description: Descricao
            basic_type: Tipo basico (se extensao de IDoc padrao)
            segments: Lista de segmentos
            direction: Direcao (inbound/outbound)
            message_type: Tipo de mensagem

        Returns:
            IDocDefinition com a definicao completa
        """
        name = name.upper()
        segments = segments or []

        # Converter segmentos para objetos
        segment_list = self._parse_segments(segments)

        # Gerar codigo de definicao
        definition_code = self._generate_definition(name, description, segment_list)

        # Gerar codigo de processamento
        if direction == IDocDirection.INBOUND:
            processing_code = self._generate_inbound_processing(name, segment_list)
        else:
            processing_code = self._generate_outbound_processing(name, segment_list)

        # Gerar codigo de envio
        sending_code = self._generate_sending_code(name, segment_list)

        return IDocDefinition(
            name=name,
            basic_type=basic_type,
            description=description,
            direction=direction,
            segments=segment_list,
            definition_code=definition_code,
            processing_code=processing_code,
            sending_code=sending_code,
            message_type=message_type or name
        )

    def _parse_segments(
        self,
        segments: List[Dict[str, Any]]
    ) -> List[IDocSegment]:
        """Converte dicionarios para IDocSegment"""
        result = []

        for seg in segments:
            fields = []
            for fld in seg.get("fields", []):
                fields.append(IDocField(
                    name=fld.get("name", "").upper(),
                    type_value=fld.get("type", ""),
                    length=fld.get("length", 0),
                    description=fld.get("description", ""),
                    mandatory=fld.get("mandatory", False)
                ))

            result.append(IDocSegment(
                name=seg.get("name", "").upper(),
                description=seg.get("description", ""),
                fields=fields,
                parent_segment=seg.get("parent"),
                min_occurs=seg.get("min_occurs", 0),
                max_occurs=seg.get("max_occurs", 999999),
                level=seg.get("level", 1),
                mandatory=seg.get("mandatory", False)
            ))

        return result

    def _generate_definition(
        self,
        name: str,
        description: str,
        segments: List[IDocSegment]
    ) -> str:
        """Gera codigo de definicao do IDoc"""
        lines = [
            f"*----------------------------------------------------------------------*",
            f"* IDoc Type: {name}",
            f"* Description: {description}",
            f"*----------------------------------------------------------------------*",
            "",
            "* Estruturas de segmentos",
            ""
        ]

        # Gerar estrutura de cada segmento
        for segment in segments:
            lines.append(segment.to_structure_definition())
            lines.append("")

        # Gerar tabelas para os segmentos
        lines.append("* Tabelas de dados")
        for segment in segments:
            lines.append(f"DATA: gt_{segment.name.lower()} TYPE TABLE OF ty_{segment.name.lower()},")
            lines.append(f"      gs_{segment.name.lower()} TYPE ty_{segment.name.lower()}.")
        lines.append("")

        # Estrutura de controle IDoc
        lines.append("* Estruturas de controle IDoc")
        lines.append("DATA: gs_edidc TYPE edidc,  \" Control record")
        lines.append("      gt_edidd TYPE TABLE OF edidd,  \" Data records")
        lines.append("      gs_edidd TYPE edidd.")

        return "\n".join(lines)

    def _generate_inbound_processing(
        self,
        name: str,
        segments: List[IDocSegment]
    ) -> str:
        """Gera codigo de processamento inbound"""
        lines = [
            f"*----------------------------------------------------------------------*",
            f"* Processamento Inbound - {name}",
            f"*----------------------------------------------------------------------*",
            "",
            "FORM process_idoc USING pt_idoc_data TYPE edidd_tt",
            "                        ps_idoc_control TYPE edidc",
            "                  CHANGING pv_status TYPE any.",
            "",
            "  DATA: ls_segment TYPE any.",
            "",
            "  LOOP AT pt_idoc_data INTO DATA(ls_edidd).",
            ""
        ]

        # Processar cada segmento
        for segment in segments:
            lines.append(f"    \" Segmento {segment.name}")
            lines.append(f"    IF ls_edidd-segnam = '{segment.name}'.")
            lines.append(f"      MOVE ls_edidd-sdata TO gs_{segment.name.lower()}.")
            lines.append(f"      APPEND gs_{segment.name.lower()} TO gt_{segment.name.lower()}.")
            lines.append(f"    ENDIF.")
            lines.append("")

        lines.append("  ENDLOOP.")
        lines.append("")
        lines.append("  \" Processar dados")
        lines.append("  PERFORM process_data.")
        lines.append("")
        lines.append("  \" Definir status de sucesso")
        lines.append("  pv_status = '53'. \" Processado com sucesso")
        lines.append("")
        lines.append("ENDFORM.")
        lines.append("")

        # Form de processamento de dados
        lines.append("*&---------------------------------------------------------------------*")
        lines.append("*& Form process_data")
        lines.append("*&---------------------------------------------------------------------*")
        lines.append("FORM process_data.")
        lines.append("  \" TODO: Implementar logica de processamento")

        for segment in segments:
            lines.append(f"  LOOP AT gt_{segment.name.lower()} INTO gs_{segment.name.lower()}.")
            lines.append(f"    \" Processar {segment.name}")
            lines.append(f"  ENDLOOP.")

        lines.append("ENDFORM.")

        return "\n".join(lines)

    def _generate_outbound_processing(
        self,
        name: str,
        segments: List[IDocSegment]
    ) -> str:
        """Gera codigo de processamento outbound"""
        lines = [
            f"*----------------------------------------------------------------------*",
            f"* Processamento Outbound - {name}",
            f"*----------------------------------------------------------------------*",
            "",
            "FORM create_idoc CHANGING pt_edidd TYPE edidd_tt",
            "                          ps_edidc TYPE edidc.",
            "",
            "  DATA: ls_edidd TYPE edidd,",
            "        lv_segnum TYPE edi_segnum.",
            "",
            "  \" Inicializar contador de segmentos",
            "  lv_segnum = 0.",
            ""
        ]

        # Criar segmentos
        for segment in segments:
            parent_num = "0" if not segment.parent_segment else "lv_parent_segnum"

            lines.append(f"  \" Segmento {segment.name}")
            lines.append(f"  LOOP AT gt_{segment.name.lower()} INTO gs_{segment.name.lower()}.")
            lines.append(f"    CLEAR ls_edidd.")
            lines.append(f"    ADD 1 TO lv_segnum.")
            lines.append(f"    ls_edidd-segnam = '{segment.name}'.")
            lines.append(f"    ls_edidd-segnum = lv_segnum.")
            lines.append(f"    ls_edidd-psgnum = {parent_num}.")
            lines.append(f"    ls_edidd-hlession = '{segment.level}'.")
            lines.append(f"    ls_edidd-sdata = gs_{segment.name.lower()}.")
            lines.append(f"    APPEND ls_edidd TO pt_edidd.")
            lines.append(f"  ENDLOOP.")
            lines.append("")

        lines.append("  \" Preencher control record")
        lines.append(f"  ps_edidc-mestyp = '{name}'.")
        lines.append(f"  ps_edidc-idoctp = '{name}'.")
        lines.append("  ps_edidc-rcvprt = 'LS'. \" Logical System")
        lines.append("  ps_edidc-rcvprn = 'RECEIVER'. \" Nome do receptor")
        lines.append("")
        lines.append("ENDFORM.")

        return "\n".join(lines)

    def _generate_sending_code(
        self,
        name: str,
        segments: List[IDocSegment]
    ) -> str:
        """Gera codigo para envio de IDoc"""
        lines = [
            f"*----------------------------------------------------------------------*",
            f"* Envio de IDoc - {name}",
            f"*----------------------------------------------------------------------*",
            "",
            "FORM send_idoc.",
            "",
            "  DATA: ls_edidc TYPE edidc,",
            "        lt_edidd TYPE TABLE OF edidd,",
            "        lv_docnum TYPE edi_docnum.",
            "",
            "  \" Criar dados do IDoc",
            "  PERFORM create_idoc CHANGING lt_edidd ls_edidc.",
            "",
            "  \" Chamar funcao de distribuicao",
            "  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'",
            "    EXPORTING",
            "      master_idoc_control            = ls_edidc",
            "    TABLES",
            "      communication_idoc_control     = lt_edidc_control",
            "      master_idoc_data               = lt_edidd",
            "    EXCEPTIONS",
            "      error_in_idoc_control          = 1",
            "      error_writing_idoc_status      = 2",
            "      error_in_idoc_data             = 3",
            "      sending_logical_system_unknown = 4",
            "      OTHERS                         = 5.",
            "",
            "  IF sy-subrc = 0.",
            "    COMMIT WORK AND WAIT.",
            "    MESSAGE 'IDoc criado com sucesso' TYPE 'S'.",
            "  ELSE.",
            "    MESSAGE 'Erro ao criar IDoc' TYPE 'E'.",
            "  ENDIF.",
            "",
            "ENDFORM."
        ]

        return "\n".join(lines)

    def generate_extension(
        self,
        base_idoc: str,
        extension_name: str,
        additional_segments: Optional[List[Dict[str, Any]]] = None,
        additional_fields: Optional[Dict[str, List[Dict[str, Any]]]] = None
    ) -> IDocDefinition:
        """
        Gera extensao de IDoc padrao.

        Args:
            base_idoc: IDoc base (ex: MATMAS05)
            extension_name: Nome da extensao
            additional_segments: Segmentos adicionais
            additional_fields: Campos adicionais por segmento

        Returns:
            IDocDefinition da extensao
        """
        extension_name = extension_name.upper()
        additional_segments = additional_segments or []
        additional_fields = additional_fields or {}

        # Buscar info do IDoc base
        base_info = None
        for idoc_type, info in self.STANDARD_IDOCS.items():
            if base_idoc in info.get("basic_types", []):
                base_info = info
                break

        description = f"Extensao de {base_idoc}"
        if base_info:
            description = f"Extensao de {base_info['description']} ({base_idoc})"

        # Criar segmentos da extensao (Z1 prefix)
        segments = []
        for seg in additional_segments:
            seg_name = seg.get("name", "")
            if not seg_name.startswith("Z1"):
                seg["name"] = f"Z1{seg_name}"
            segments.append(seg)

        return self.define_idoc(
            name=extension_name,
            description=description,
            basic_type=base_idoc,
            segments=segments
        )

    def generate_function_module_inbound(
        self,
        idoc_type: str
    ) -> str:
        """
        Gera Function Module para processamento inbound.

        Args:
            idoc_type: Tipo do IDoc

        Returns:
            Codigo do Function Module
        """
        fm_name = f"Z_IDOC_INPUT_{idoc_type}".upper()

        return f'''FUNCTION {fm_name}.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) TYPE  CHAR1
*"     VALUE(MASS_PROCESSING) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) TYPE  BDWF_RESULT
*"     VALUE(APPLICATION_VARIABLE) TYPE  CHAR1
*"     VALUE(IN_UPDATE_TASK) TYPE  CHAR1
*"     VALUE(CALL_TRANSACTION_DONE) TYPE  CHAR1
*"  TABLES
*"      IDOC_CONTRL TYPE  STANDARD TABLE
*"      IDOC_DATA TYPE  STANDARD TABLE
*"      IDOC_STATUS TYPE  STANDARD TABLE
*"      RETURN_VARIABLES TYPE  STANDARD TABLE
*"      SERIALIZATION_INFO TYPE  STANDARD TABLE
*"----------------------------------------------------------------------

  DATA: ls_idoc_control TYPE edidc,
        lt_idoc_data TYPE TABLE OF edidd,
        ls_idoc_status TYPE bdidocstat.

  \" Processar cada IDoc
  LOOP AT idoc_contrl INTO ls_idoc_control.

    \" Selecionar dados do IDoc
    LOOP AT idoc_data INTO DATA(ls_data)
         WHERE docnum = ls_idoc_control-docnum.
      APPEND ls_data TO lt_idoc_data.
    ENDLOOP.

    \" Processar IDoc
    PERFORM process_idoc USING lt_idoc_data
                               ls_idoc_control
                         CHANGING ls_idoc_status-status.

    \" Atualizar status
    ls_idoc_status-docnum = ls_idoc_control-docnum.
    APPEND ls_idoc_status TO idoc_status.

    CLEAR lt_idoc_data.

  ENDLOOP.

ENDFUNCTION.
'''

    def get_standard_idoc_info(self, idoc_type: str) -> Optional[Dict[str, Any]]:
        """
        Retorna informacoes de um IDoc padrao.

        Args:
            idoc_type: Tipo do IDoc (ex: MATMAS)

        Returns:
            Informacoes do IDoc ou None
        """
        return self.STANDARD_IDOCS.get(idoc_type.upper())

    def list_standard_idocs(self) -> List[Dict[str, Any]]:
        """
        Lista IDocs padrao conhecidos.

        Returns:
            Lista de IDocs com informacoes
        """
        result = []
        for name, info in self.STANDARD_IDOCS.items():
            result.append({
                "name": name,
                "description": info["description"],
                "basic_types": info["basic_types"],
                "main_segments": info["segments"][:3]  # Primeiros 3 segmentos
            })
        return result
