# -*- coding: utf-8 -*-
"""
SAP CPI Analyzers Module
========================

Analisadores para artefatos SAP CPI.

Classes disponíveis:
- IFlowAnalyzer: Análise de Integration Flows (BPMN2)
- MappingAnalyzer: Análise de Message Mappings
- ScriptAnalyzer: Análise de scripts Groovy
"""

from .iflow_analyzer import IFlowAnalyzer
from .mapping_analyzer import MappingAnalyzer
from .script_analyzer import ScriptAnalyzer

__all__ = [
    'IFlowAnalyzer',
    'MappingAnalyzer',
    'ScriptAnalyzer',
]
