# -*- coding: utf-8 -*-
"""
Reports Module - Plataforma E
=============================

Report generation system for PDF, Excel, HTML exports.

Issue #446: Report Generator - Geracao de Relatorios
"""

from .report_generator import (
    ReportType,
    ReportFormat,
    ReportConfig,
    ReportData,
    ReportGenerator,
    get_report_generator,
)

from .exporters import (
    BaseExporter,
    HTMLExporter,
    CSVExporter,
    JSONExporter,
    ExporterFactory,
)

__all__ = [
    "ReportType",
    "ReportFormat",
    "ReportConfig",
    "ReportData",
    "ReportGenerator",
    "get_report_generator",
    "BaseExporter",
    "HTMLExporter",
    "CSVExporter",
    "JSONExporter",
    "ExporterFactory",
]
