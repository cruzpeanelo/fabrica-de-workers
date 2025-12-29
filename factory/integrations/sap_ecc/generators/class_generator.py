# -*- coding: utf-8 -*-
"""
Class Generator
===============
Gerador de Classes ABAP OO.

Este modulo fornece:
- Geracao de classes ABAP OO
- Interfaces
- Implementacao de BADIs
- Padroes de design (Singleton, Factory, etc.)

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.generators import ClassGenerator

generator = ClassGenerator()

# Gerar classe simples
cls = generator.generate_class(
    name="ZCL_MATERIAL_HANDLER",
    description="Handler para materiais",
    methods=[
        {"name": "GET_MATERIAL", "visibility": "PUBLIC", "returning": {"type": "MARA"}}
    ]
)

print(cls.source_code)
```
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class ClassVisibility(str, Enum):
    """Visibilidade de membros da classe"""
    PUBLIC = "PUBLIC"
    PROTECTED = "PROTECTED"
    PRIVATE = "PRIVATE"


class ClassCategory(str, Enum):
    """Categoria da classe"""
    GENERAL = "0"  # Classe geral
    EXIT = "1"  # Classe de exit
    PERSISTENT = "2"  # Classe persistente
    EXCEPTION = "3"  # Classe de excecao
    TEST = "4"  # Classe de teste


@dataclass
class ClassAttribute:
    """Atributo de classe"""
    name: str
    type_value: str
    visibility: ClassVisibility = ClassVisibility.PRIVATE
    is_static: bool = False
    is_read_only: bool = False
    is_constant: bool = False
    initial_value: str = ""
    description: str = ""

    def to_definition(self) -> str:
        """Gera definicao do atributo"""
        parts = []

        if self.is_constant:
            parts.append(f"CONSTANTS {self.name}")
        elif self.is_static:
            parts.append(f"CLASS-DATA {self.name}")
        else:
            parts.append(f"DATA {self.name}")

        parts.append(f"TYPE {self.type_value}")

        if self.is_read_only and not self.is_constant:
            parts.append("READ-ONLY")

        if self.initial_value:
            parts.append(f"VALUE {self.initial_value}")

        return " ".join(parts) + "."


@dataclass
class MethodParameter:
    """Parametro de metodo"""
    name: str
    type_value: str
    category: str = "IMPORTING"  # IMPORTING, EXPORTING, CHANGING, RETURNING
    optional: bool = False
    default_value: str = ""
    is_reference: bool = True  # False = VALUE

    def to_definition(self) -> str:
        """Gera definicao do parametro"""
        parts = [self.name]

        if not self.is_reference and self.category in ("IMPORTING", "RETURNING"):
            parts.insert(0, "VALUE(")
            parts.append(")")
        else:
            parts = [self.name]

        parts.append(f"TYPE {self.type_value}")

        if self.optional:
            parts.append("OPTIONAL")

        if self.default_value:
            parts.append(f"DEFAULT {self.default_value}")

        return " ".join(parts)


@dataclass
class ClassMethod:
    """Metodo de classe"""
    name: str
    visibility: ClassVisibility = ClassVisibility.PUBLIC
    is_static: bool = False
    is_abstract: bool = False
    is_final: bool = False
    is_redefinition: bool = False
    description: str = ""
    importing: List[MethodParameter] = field(default_factory=list)
    exporting: List[MethodParameter] = field(default_factory=list)
    changing: List[MethodParameter] = field(default_factory=list)
    returning: Optional[MethodParameter] = None
    raising: List[str] = field(default_factory=list)
    implementation: str = ""

    def to_definition(self) -> str:
        """Gera definicao do metodo"""
        lines = []

        # Metodo classico ou estatico
        if self.is_static:
            method_type = "CLASS-METHODS"
        else:
            method_type = "METHODS"

        # Nome e modificadores
        parts = [method_type, self.name]

        if self.is_abstract:
            parts.append("ABSTRACT")
        if self.is_final:
            parts.append("FINAL")
        if self.is_redefinition:
            parts.append("REDEFINITION")

        lines.append(" ".join(parts))

        # Parametros
        if self.importing:
            lines.append("  IMPORTING")
            for param in self.importing:
                lines.append(f"    {param.to_definition()}")

        if self.exporting:
            lines.append("  EXPORTING")
            for param in self.exporting:
                lines.append(f"    {param.to_definition()}")

        if self.changing:
            lines.append("  CHANGING")
            for param in self.changing:
                lines.append(f"    {param.to_definition()}")

        if self.returning:
            lines.append(f"  RETURNING VALUE({self.returning.name}) TYPE {self.returning.type_value}")

        if self.raising:
            lines.append(f"  RAISING {' '.join(self.raising)}")

        # Ponto final
        result = "\n".join(lines)
        if not result.endswith("."):
            result += "."

        return result


@dataclass
class ABAPClass:
    """Classe ABAP gerada"""
    name: str
    description: str = ""
    superclass: str = ""
    interfaces: List[str] = field(default_factory=list)
    category: ClassCategory = ClassCategory.GENERAL
    is_final: bool = False
    is_abstract: bool = False
    is_create_private: bool = False
    is_create_protected: bool = False

    attributes: List[ClassAttribute] = field(default_factory=list)
    methods: List[ClassMethod] = field(default_factory=list)

    definition_code: str = ""
    implementation_code: str = ""
    author: str = "FABRICA_AGENTES"
    created_at: datetime = field(default_factory=datetime.now)
    package: str = "$TMP"

    @property
    def source_code(self) -> str:
        """Codigo completo da classe"""
        return self.definition_code + "\n\n" + self.implementation_code

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "superclass": self.superclass,
            "interfaces": self.interfaces,
            "is_final": self.is_final,
            "is_abstract": self.is_abstract,
            "attributes_count": len(self.attributes),
            "methods_count": len(self.methods),
            "package": self.package
        }


class ClassGenerator:
    """
    Gerador de Classes ABAP OO.

    Fornece templates e geracao de:
    - Classes simples
    - Classes com heranca
    - Implementacoes de interface
    - Padroes de design
    - Classes de BADI
    """

    DEFINITION_HEADER = '''*----------------------------------------------------------------------*
* Class {name}
*----------------------------------------------------------------------*
* {description}
*----------------------------------------------------------------------*
'''

    def __init__(self):
        """Inicializa o gerador"""
        self.indent = "  "

    def generate_class(
        self,
        name: str,
        description: str = "",
        superclass: str = "",
        interfaces: Optional[List[str]] = None,
        attributes: Optional[List[Dict[str, Any]]] = None,
        methods: Optional[List[Dict[str, Any]]] = None,
        is_final: bool = False,
        is_abstract: bool = False,
        create_visibility: str = "PUBLIC"
    ) -> ABAPClass:
        """
        Gera uma classe ABAP.

        Args:
            name: Nome da classe (ex: ZCL_MATERIAL_HANDLER)
            description: Descricao
            superclass: Classe pai
            interfaces: Interfaces implementadas
            attributes: Atributos da classe
            methods: Metodos da classe
            is_final: Se e classe final
            is_abstract: Se e classe abstrata
            create_visibility: Visibilidade do CREATE (PUBLIC, PROTECTED, PRIVATE)

        Returns:
            ABAPClass com o codigo gerado
        """
        name = name.upper()
        interfaces = interfaces or []
        attributes = attributes or []
        methods = methods or []

        # Converter para objetos
        attr_list = self._parse_attributes(attributes)
        method_list = self._parse_methods(methods)

        # Gerar definicao
        definition = self._generate_definition(
            name, description, superclass, interfaces,
            attr_list, method_list, is_final, is_abstract, create_visibility
        )

        # Gerar implementacao
        implementation = self._generate_implementation(name, method_list)

        return ABAPClass(
            name=name,
            description=description,
            superclass=superclass,
            interfaces=interfaces,
            is_final=is_final,
            is_abstract=is_abstract,
            is_create_private=create_visibility == "PRIVATE",
            is_create_protected=create_visibility == "PROTECTED",
            attributes=attr_list,
            methods=method_list,
            definition_code=definition,
            implementation_code=implementation
        )

    def _parse_attributes(
        self,
        attributes: List[Dict[str, Any]]
    ) -> List[ClassAttribute]:
        """Converte dicionarios para ClassAttribute"""
        result = []
        for attr in attributes:
            visibility = attr.get("visibility", "PRIVATE").upper()
            result.append(ClassAttribute(
                name=attr.get("name", "").upper(),
                type_value=attr.get("type", ""),
                visibility=ClassVisibility[visibility],
                is_static=attr.get("is_static", False),
                is_read_only=attr.get("is_read_only", False),
                is_constant=attr.get("is_constant", False),
                initial_value=attr.get("initial_value", ""),
                description=attr.get("description", "")
            ))
        return result

    def _parse_methods(
        self,
        methods: List[Dict[str, Any]]
    ) -> List[ClassMethod]:
        """Converte dicionarios para ClassMethod"""
        result = []
        for method in methods:
            visibility = method.get("visibility", "PUBLIC").upper()

            # Parse parametros
            importing = []
            for p in method.get("importing", []):
                importing.append(MethodParameter(
                    name=p.get("name", "").upper(),
                    type_value=p.get("type", ""),
                    category="IMPORTING",
                    optional=p.get("optional", False)
                ))

            exporting = []
            for p in method.get("exporting", []):
                exporting.append(MethodParameter(
                    name=p.get("name", "").upper(),
                    type_value=p.get("type", ""),
                    category="EXPORTING"
                ))

            returning = None
            if method.get("returning"):
                r = method["returning"]
                returning = MethodParameter(
                    name=r.get("name", "RV_RESULT"),
                    type_value=r.get("type", ""),
                    category="RETURNING"
                )

            result.append(ClassMethod(
                name=method.get("name", "").upper(),
                visibility=ClassVisibility[visibility],
                is_static=method.get("is_static", False),
                is_abstract=method.get("is_abstract", False),
                is_final=method.get("is_final", False),
                description=method.get("description", ""),
                importing=importing,
                exporting=exporting,
                returning=returning,
                raising=method.get("raising", []),
                implementation=method.get("implementation", "")
            ))
        return result

    def _generate_definition(
        self,
        name: str,
        description: str,
        superclass: str,
        interfaces: List[str],
        attributes: List[ClassAttribute],
        methods: List[ClassMethod],
        is_final: bool,
        is_abstract: bool,
        create_visibility: str
    ) -> str:
        """Gera definicao da classe"""
        lines = [self.DEFINITION_HEADER.format(name=name, description=description)]

        # Cabecalho da classe
        class_def = f"CLASS {name} DEFINITION"

        if is_final:
            class_def += " FINAL"
        if is_abstract:
            class_def += " ABSTRACT"

        if superclass:
            class_def += f" INHERITING FROM {superclass}"

        if create_visibility == "PRIVATE":
            class_def += " CREATE PRIVATE"
        elif create_visibility == "PROTECTED":
            class_def += " CREATE PROTECTED"

        class_def += "."
        lines.append(class_def)
        lines.append("")

        # Secao PUBLIC
        lines.append("  PUBLIC SECTION.")

        # Interfaces
        if interfaces:
            for iface in interfaces:
                lines.append(f"    INTERFACES {iface}.")
            lines.append("")

        # Tipos
        lines.append("    TYPES:")
        lines.append("      BEGIN OF ty_data,")
        lines.append("        field TYPE string,")
        lines.append("      END OF ty_data.")
        lines.append("")

        # Atributos publicos
        public_attrs = [a for a in attributes if a.visibility == ClassVisibility.PUBLIC]
        if public_attrs:
            for attr in public_attrs:
                lines.append(f"    {attr.to_definition()}")
            lines.append("")

        # Metodos publicos
        public_methods = [m for m in methods if m.visibility == ClassVisibility.PUBLIC]
        for method in public_methods:
            lines.append(f"    {method.to_definition()}")
        lines.append("")

        # Secao PROTECTED
        protected_attrs = [a for a in attributes if a.visibility == ClassVisibility.PROTECTED]
        protected_methods = [m for m in methods if m.visibility == ClassVisibility.PROTECTED]

        if protected_attrs or protected_methods:
            lines.append("  PROTECTED SECTION.")
            for attr in protected_attrs:
                lines.append(f"    {attr.to_definition()}")
            for method in protected_methods:
                lines.append(f"    {method.to_definition()}")
            lines.append("")

        # Secao PRIVATE
        private_attrs = [a for a in attributes if a.visibility == ClassVisibility.PRIVATE]
        private_methods = [m for m in methods if m.visibility == ClassVisibility.PRIVATE]

        if private_attrs or private_methods:
            lines.append("  PRIVATE SECTION.")
            for attr in private_attrs:
                lines.append(f"    {attr.to_definition()}")
            for method in private_methods:
                lines.append(f"    {method.to_definition()}")
            lines.append("")

        lines.append("ENDCLASS.")

        return "\n".join(lines)

    def _generate_implementation(
        self,
        name: str,
        methods: List[ClassMethod]
    ) -> str:
        """Gera implementacao da classe"""
        lines = [
            f"CLASS {name} IMPLEMENTATION.",
            ""
        ]

        for method in methods:
            if not method.is_abstract:
                lines.append(f"  METHOD {method.name.lower()}.")

                if method.implementation:
                    # Indentar implementacao
                    impl_lines = method.implementation.split("\n")
                    for impl_line in impl_lines:
                        lines.append(f"    {impl_line}")
                else:
                    lines.append("*   TODO: Implementar")
                    lines.append("  ")

                lines.append("  ENDMETHOD.")
                lines.append("")

        lines.append("ENDCLASS.")

        return "\n".join(lines)

    def generate_interface(
        self,
        name: str,
        description: str = "",
        methods: Optional[List[Dict[str, Any]]] = None,
        attributes: Optional[List[Dict[str, Any]]] = None,
        constants: Optional[List[Dict[str, Any]]] = None
    ) -> str:
        """
        Gera uma interface ABAP.

        Args:
            name: Nome da interface (ex: ZIF_MATERIAL)
            description: Descricao
            methods: Metodos da interface
            attributes: Atributos (sempre publicos em interfaces)
            constants: Constantes

        Returns:
            Codigo da interface
        """
        name = name.upper()
        methods = methods or []
        attributes = attributes or []
        constants = constants or []

        lines = [
            f'*----------------------------------------------------------------------*',
            f'* Interface {name}',
            f'*----------------------------------------------------------------------*',
            f'* {description}',
            f'*----------------------------------------------------------------------*',
            f'INTERFACE {name} PUBLIC.',
            ''
        ]

        # Constantes
        if constants:
            lines.append('  " Constantes')
            for const in constants:
                const_name = const.get("name", "")
                const_type = const.get("type", "string")
                const_value = const.get("value", "")
                lines.append(f"  CONSTANTS {const_name} TYPE {const_type} VALUE {const_value}.")
            lines.append('')

        # Tipos
        lines.append('  " Tipos')
        lines.append('  TYPES: BEGIN OF ty_data,')
        lines.append('           field TYPE string,')
        lines.append('         END OF ty_data.')
        lines.append('')

        # Atributos
        if attributes:
            lines.append('  " Atributos')
            for attr in attributes:
                attr_name = attr.get("name", "")
                attr_type = attr.get("type", "string")
                lines.append(f"  DATA {attr_name} TYPE {attr_type}.")
            lines.append('')

        # Metodos
        lines.append('  " Metodos')
        for method in methods:
            method_obj = self._parse_methods([method])[0]
            lines.append(f"  {method_obj.to_definition()}")

        lines.append('')
        lines.append('ENDINTERFACE.')

        return "\n".join(lines)

    def generate_badi_implementation(
        self,
        badi_name: str,
        implementation_name: str,
        interface_name: str,
        methods_impl: Optional[Dict[str, str]] = None
    ) -> ABAPClass:
        """
        Gera classe de implementacao de BADI.

        Args:
            badi_name: Nome da BADI
            implementation_name: Nome da implementacao
            interface_name: Interface da BADI
            methods_impl: Implementacao dos metodos {nome: codigo}

        Returns:
            ABAPClass com a implementacao
        """
        class_name = f"ZCL_{implementation_name}".upper()
        methods_impl = methods_impl or {}

        # Metodos da interface (assumindo alguns comuns)
        methods = []
        for method_name, impl_code in methods_impl.items():
            methods.append({
                "name": method_name,
                "visibility": "PUBLIC",
                "implementation": impl_code or "* TODO: Implementar"
            })

        cls = self.generate_class(
            name=class_name,
            description=f"Implementacao da BADI {badi_name}",
            interfaces=[interface_name],
            methods=methods
        )

        return cls

    def generate_singleton(
        self,
        name: str,
        description: str = ""
    ) -> ABAPClass:
        """
        Gera classe com padrao Singleton.

        Args:
            name: Nome da classe
            description: Descricao

        Returns:
            ABAPClass singleton
        """
        return self.generate_class(
            name=name,
            description=f"{description} (Singleton Pattern)",
            create_visibility="PRIVATE",
            attributes=[
                {
                    "name": "GO_INSTANCE",
                    "type": f"REF TO {name.upper()}",
                    "visibility": "PRIVATE",
                    "is_static": True,
                    "description": "Instancia unica"
                }
            ],
            methods=[
                {
                    "name": "GET_INSTANCE",
                    "visibility": "PUBLIC",
                    "is_static": True,
                    "returning": {"name": "RO_INSTANCE", "type": f"REF TO {name.upper()}"},
                    "implementation": f'''IF go_instance IS NOT BOUND.
  CREATE OBJECT go_instance.
ENDIF.
ro_instance = go_instance.'''
                },
                {
                    "name": "CONSTRUCTOR",
                    "visibility": "PRIVATE",
                    "implementation": "* Construtor privado"
                }
            ]
        )

    def generate_exception_class(
        self,
        name: str,
        description: str = "",
        text_id: str = ""
    ) -> ABAPClass:
        """
        Gera classe de excecao.

        Args:
            name: Nome da classe (ex: ZCX_MATERIAL_ERROR)
            description: Descricao
            text_id: ID do texto

        Returns:
            ABAPClass de excecao
        """
        return self.generate_class(
            name=name,
            description=description,
            superclass="CX_STATIC_CHECK",
            is_final=True,
            methods=[
                {
                    "name": "CONSTRUCTOR",
                    "visibility": "PUBLIC",
                    "importing": [
                        {"name": "TEXTID", "type": "SCX_T100KEY", "optional": True},
                        {"name": "PREVIOUS", "type": "REF TO CX_ROOT", "optional": True}
                    ],
                    "implementation": '''CALL METHOD super->constructor
  EXPORTING
    previous = previous.
IF textid IS NOT INITIAL.
  me->textid = textid.
ENDIF.'''
                }
            ]
        )
