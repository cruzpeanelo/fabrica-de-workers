# -*- coding: utf-8 -*-
"""
SAP S/4HANA Skills
==================
Skills especializadas para interacao com SAP S/4HANA.
"""

from .s4_read_skill import S4ReadSkill
from .s4_cds_skill import S4CDSSkill
from .s4_fiori_skill import S4FioriSkill

__all__ = [
    'S4ReadSkill',
    'S4CDSSkill',
    'S4FioriSkill'
]
