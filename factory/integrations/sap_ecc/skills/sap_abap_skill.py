# -*- coding: utf-8 -*-
"""
SAP ABAP Skill
==============
Skill para desenvolvimento ABAP no SAP.

Esta skill fornece capacidades de:
- Analise de codigo ABAP
- Geracao de programas
- Geracao de Function Modules
- Geracao de Classes OO
- Validacao de codigo

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.skills import SAPABAPSkill

skill = SAPABAPSkill(rfc_client)

# Analisar programa
analysis = await skill.analyze_program("ZSAMPLE_REPORT")

# Gerar report ALV
code = await skill.generate_alv_report(
    name="ZLIST_MATERIALS",
    table="MARA",
    fields=["MATNR", "MTART", "MATKL"]
)
```
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional

from ..analyzers import ABAPAnalyzer, TableAnalyzer, BADIAnalyzer
from ..generators import ABAPGenerator, FunctionGenerator, ClassGenerator, ALVType

logger = logging.getLogger(__name__)


@dataclass
class ABAPSkillResult:
    """Resultado de execucao de skill ABAP"""
    success: bool
    code: str = ""
    analysis: Optional[Dict[str, Any]] = None
    message: str = ""
    error: Optional[str] = None
    execution_time_ms: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "code": self.code,
            "analysis": self.analysis,
            "message": self.message,
            "error": self.error,
            "execution_time_ms": self.execution_time_ms
        }


class SAPABAPSkill:
    """
    Skill de desenvolvimento ABAP.

    Combina analisadores e geradores para fornecer:
    - Analise de codigo existente
    - Geracao de novo codigo
    - Sugestoes de melhorias
    - Validacao de boas praticas
    """

    def __init__(self, rfc_client):
        """
        Inicializa a skill.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client
        self.abap_analyzer = ABAPAnalyzer(rfc_client)
        self.table_analyzer = TableAnalyzer(rfc_client)
        self.badi_analyzer = BADIAnalyzer(rfc_client)
        self.abap_generator = ABAPGenerator()
        self.function_generator = FunctionGenerator()
        self.class_generator = ClassGenerator()

    # =========================================================================
    # Analise
    # =========================================================================

    async def analyze_program(self, program_name: str) -> ABAPSkillResult:
        """
        Analisa um programa ABAP.

        Args:
            program_name: Nome do programa

        Returns:
            ABAPSkillResult com a analise
        """
        start_time = datetime.now()

        try:
            analysis = await self.abap_analyzer.analyze_program(program_name)

            return ABAPSkillResult(
                success=True,
                analysis=analysis.to_dict(),
                code="\n".join(analysis.source_code),
                message=f"Programa {program_name} analisado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao analisar programa {program_name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def analyze_table(self, table_name: str) -> ABAPSkillResult:
        """
        Analisa estrutura de uma tabela SAP.

        Args:
            table_name: Nome da tabela

        Returns:
            ABAPSkillResult com a estrutura
        """
        start_time = datetime.now()

        try:
            structure = await self.table_analyzer.analyze_table(table_name)

            return ABAPSkillResult(
                success=True,
                analysis=structure.to_dict(),
                message=f"Tabela {table_name} analisada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao analisar tabela {table_name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def find_badi(self, transaction: str) -> ABAPSkillResult:
        """
        Encontra BADIs para uma transacao.

        Args:
            transaction: Codigo da transacao

        Returns:
            ABAPSkillResult com BADIs encontradas
        """
        start_time = datetime.now()

        try:
            badis = await self.badi_analyzer.find_badis_for_transaction(transaction)

            return ABAPSkillResult(
                success=True,
                analysis={"badis": badis, "count": len(badis)},
                message=f"Encontradas {len(badis)} BADIs para {transaction}",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao buscar BADIs para {transaction}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Geracao de Reports
    # =========================================================================

    async def generate_report(
        self,
        name: str,
        title: str = "",
        description: str = "",
        tables: Optional[List[str]] = None,
        select_options: Optional[List[Dict]] = None,
        output_fields: Optional[List[Dict]] = None
    ) -> ABAPSkillResult:
        """
        Gera um report ABAP.

        Args:
            name: Nome do programa
            title: Titulo
            description: Descricao
            tables: Tabelas usadas
            select_options: SELECT-OPTIONS
            output_fields: Campos de saida

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            program = self.abap_generator.generate_report(
                name=name,
                title=title,
                description=description,
                tables=tables,
                select_options=select_options,
                output_fields=output_fields
            )

            return ABAPSkillResult(
                success=True,
                code=program.source_code,
                message=f"Report {name} gerado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar report {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def generate_alv_report(
        self,
        name: str,
        table: str,
        fields: List[str],
        title: str = "",
        select_options: Optional[List[Dict]] = None,
        alv_type: str = "classic"
    ) -> ABAPSkillResult:
        """
        Gera um report ALV.

        Args:
            name: Nome do programa
            table: Tabela principal
            fields: Campos a exibir
            title: Titulo
            select_options: SELECT-OPTIONS
            alv_type: Tipo de ALV (classic, oo, salv)

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            # Buscar tipos dos campos
            structure = await self.table_analyzer.analyze_table(table)
            field_types = {f.name: f.data_type for f in structure.fields}

            # Montar output_fields com tipos
            output_fields = []
            for field_name in fields:
                field_type = field_types.get(field_name.upper(), "string")
                output_fields.append({
                    "name": field_name,
                    "type": f"{table.lower()}-{field_name.lower()}"
                })

            # Determinar tipo de ALV
            alv_enum = {
                "classic": ALVType.CLASSIC,
                "oo": ALVType.OO,
                "salv": ALVType.SALV
            }.get(alv_type.lower(), ALVType.CLASSIC)

            program = self.abap_generator.generate_report(
                name=name,
                title=title or f"Listagem de {table}",
                tables=[table],
                select_options=select_options,
                output_fields=output_fields,
                alv_type=alv_enum
            )

            return ABAPSkillResult(
                success=True,
                code=program.source_code,
                message=f"Report ALV {name} gerado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar ALV {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Geracao de Function Modules
    # =========================================================================

    async def generate_function(
        self,
        name: str,
        group: str = "",
        description: str = "",
        importing: Optional[List[Dict]] = None,
        exporting: Optional[List[Dict]] = None,
        tables: Optional[List[Dict]] = None,
        is_rfc: bool = False
    ) -> ABAPSkillResult:
        """
        Gera uma Function Module.

        Args:
            name: Nome da funcao
            group: Grupo de funcoes
            description: Descricao
            importing: Parametros de entrada
            exporting: Parametros de saida
            tables: Tabelas
            is_rfc: Se e RFC enabled

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            fm = self.function_generator.generate_function(
                name=name,
                group=group,
                description=description,
                importing=importing,
                exporting=exporting,
                tables=tables,
                is_rfc=is_rfc
            )

            return ABAPSkillResult(
                success=True,
                code=fm.source_code,
                analysis=fm.to_dict(),
                message=f"Function Module {name} gerada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar FM {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def generate_bapi(
        self,
        name: str,
        object_type: str,
        description: str = "",
        importing: Optional[List[Dict]] = None,
        tables: Optional[List[Dict]] = None
    ) -> ABAPSkillResult:
        """
        Gera uma BAPI.

        Args:
            name: Nome da BAPI
            object_type: Tipo de objeto (MATERIAL, CUSTOMER, etc.)
            description: Descricao
            importing: Parametros de entrada
            tables: Tabelas

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            bapi = self.function_generator.generate_bapi(
                name=name,
                object_type=object_type,
                description=description,
                importing=importing,
                tables=tables
            )

            return ABAPSkillResult(
                success=True,
                code=bapi.source_code,
                analysis=bapi.to_dict(),
                message=f"BAPI {name} gerada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar BAPI {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Geracao de Classes
    # =========================================================================

    async def generate_class(
        self,
        name: str,
        description: str = "",
        superclass: str = "",
        interfaces: Optional[List[str]] = None,
        methods: Optional[List[Dict]] = None,
        attributes: Optional[List[Dict]] = None
    ) -> ABAPSkillResult:
        """
        Gera uma classe ABAP OO.

        Args:
            name: Nome da classe
            description: Descricao
            superclass: Classe pai
            interfaces: Interfaces implementadas
            methods: Metodos
            attributes: Atributos

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            cls = self.class_generator.generate_class(
                name=name,
                description=description,
                superclass=superclass,
                interfaces=interfaces,
                methods=methods,
                attributes=attributes
            )

            return ABAPSkillResult(
                success=True,
                code=cls.source_code,
                analysis=cls.to_dict(),
                message=f"Classe {name} gerada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar classe {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def generate_badi_implementation(
        self,
        badi_name: str,
        implementation_name: str,
        methods_impl: Optional[Dict[str, str]] = None
    ) -> ABAPSkillResult:
        """
        Gera implementacao de BADI.

        Args:
            badi_name: Nome da BADI
            implementation_name: Nome da implementacao
            methods_impl: Implementacao dos metodos

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            # Buscar informacoes da BADI
            badi_info = await self.badi_analyzer.analyze_badi(badi_name)

            cls = self.class_generator.generate_badi_implementation(
                badi_name=badi_name,
                implementation_name=implementation_name,
                interface_name=badi_info.interface_name,
                methods_impl=methods_impl
            )

            return ABAPSkillResult(
                success=True,
                code=cls.source_code,
                analysis={
                    "badi": badi_info.to_dict(),
                    "implementation": cls.to_dict()
                },
                message=f"Implementacao de BADI {badi_name} gerada",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar implementacao de BADI: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Geracao de Batch Input
    # =========================================================================

    async def generate_batch_input(
        self,
        name: str,
        transaction: str,
        title: str = "",
        description: str = ""
    ) -> ABAPSkillResult:
        """
        Gera programa de Batch Input.

        Args:
            name: Nome do programa
            transaction: Transacao a executar
            title: Titulo
            description: Descricao

        Returns:
            ABAPSkillResult com o codigo
        """
        start_time = datetime.now()

        try:
            program = self.abap_generator.generate_batch_input(
                name=name,
                transaction=transaction,
                title=title,
                description=description
            )

            return ABAPSkillResult(
                success=True,
                code=program.source_code,
                message=f"Batch Input {name} gerado",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao gerar Batch Input {name}: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    # =========================================================================
    # Listagens
    # =========================================================================

    async def list_programs(
        self,
        prefix: str = "Z",
        max_results: int = 50
    ) -> ABAPSkillResult:
        """
        Lista programas ABAP.

        Args:
            prefix: Prefixo
            max_results: Maximo

        Returns:
            ABAPSkillResult com a lista
        """
        start_time = datetime.now()

        try:
            programs = await self.abap_analyzer.list_programs(
                prefix=prefix,
                max_results=max_results
            )

            return ABAPSkillResult(
                success=True,
                analysis={"programs": programs, "count": len(programs)},
                message=f"Encontrados {len(programs)} programas",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao listar programas: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def list_function_modules(
        self,
        prefix: str = "Z",
        function_group: Optional[str] = None
    ) -> ABAPSkillResult:
        """
        Lista Function Modules.

        Args:
            prefix: Prefixo
            function_group: Grupo

        Returns:
            ABAPSkillResult com a lista
        """
        start_time = datetime.now()

        try:
            functions = await self.abap_analyzer.list_function_modules(
                prefix=prefix,
                function_group=function_group
            )

            return ABAPSkillResult(
                success=True,
                analysis={"functions": functions, "count": len(functions)},
                message=f"Encontradas {len(functions)} funcoes",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao listar funcoes: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    async def list_classes(
        self,
        prefix: str = "Z"
    ) -> ABAPSkillResult:
        """
        Lista classes ABAP.

        Args:
            prefix: Prefixo

        Returns:
            ABAPSkillResult com a lista
        """
        start_time = datetime.now()

        try:
            classes = await self.abap_analyzer.list_classes(prefix=prefix)

            return ABAPSkillResult(
                success=True,
                analysis={"classes": classes, "count": len(classes)},
                message=f"Encontradas {len(classes)} classes",
                execution_time_ms=self._elapsed_ms(start_time)
            )

        except Exception as e:
            logger.error(f"Erro ao listar classes: {e}")
            return ABAPSkillResult(
                success=False,
                error=str(e),
                execution_time_ms=self._elapsed_ms(start_time)
            )

    def _elapsed_ms(self, start_time: datetime) -> float:
        """Calcula tempo decorrido em milissegundos"""
        return (datetime.now() - start_time).total_seconds() * 1000
