# -*- coding: utf-8 -*-
"""
SAP CPI Generators Module
=========================

Geradores de artefatos SAP CPI.

Classes disponíveis:
- IFlowGenerator: Geração de Integration Flows (BPMN2)
- MappingGenerator: Geração de Message Mappings
- GroovyGenerator: Geração de scripts Groovy
"""

from .iflow_generator import IFlowGenerator
from .mapping_generator import MappingGenerator
from .groovy_generator import GroovyGenerator

__all__ = [
    'IFlowGenerator',
    'MappingGenerator',
    'GroovyGenerator',
]
