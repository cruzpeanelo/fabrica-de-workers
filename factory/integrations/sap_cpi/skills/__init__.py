# -*- coding: utf-8 -*-
"""
SAP CPI Skills Module
=====================

Skills para agentes IA interagirem com SAP CPI.

Classes disponíveis:
- CPIReadSkill: Leitura e análise de artefatos
- CPIIFlowSkill: Criação e modificação de iFlows
- CPIDeploySkill: Deploy e operação de integrações
"""

from .cpi_read_skill import CPIReadSkill
from .cpi_iflow_skill import CPIIFlowSkill
from .cpi_deploy_skill import CPIDeploySkill

__all__ = [
    'CPIReadSkill',
    'CPIIFlowSkill',
    'CPIDeploySkill',
]
