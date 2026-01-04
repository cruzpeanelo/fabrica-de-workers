# -*- coding: utf-8 -*-
"""
Report Generator - Plataforma E
===============================

Main report generation engine.

Issue #446: Report Generator - Geracao de Relatorios
"""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Callable
import json


class ReportType(str, Enum):
    """Types of reports available."""
    SPRINT = "sprint"
    TEAM = "team"
    PROJECT = "project"
    EXECUTIVE = "executive"
    VELOCITY = "velocity"
    BURNDOWN = "burndown"
    CUSTOM = "custom"


class ReportFormat(str, Enum):
    """Output formats for reports."""
    HTML = "html"
    CSV = "csv"
    JSON = "json"
    PDF = "pdf"
    EXCEL = "excel"


@dataclass
class ReportConfig:
    """Configuration for report generation."""
    report_type: ReportType
    title: str
    format: ReportFormat = ReportFormat.HTML
    date_from: Optional[datetime] = None
    date_to: Optional[datetime] = None
    filters: Dict[str, Any] = field(default_factory=dict)
    include_charts: bool = True
    include_summary: bool = True
    template: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert config to dictionary."""
        return {
            "report_type": self.report_type.value,
            "title": self.title,
            "format": self.format.value,
            "date_from": self.date_from.isoformat() if self.date_from else None,
            "date_to": self.date_to.isoformat() if self.date_to else None,
            "filters": self.filters,
            "include_charts": self.include_charts,
            "include_summary": self.include_summary,
            "template": self.template,
        }


@dataclass
class ReportSection:
    """A section within a report."""
    title: str
    content: Any
    section_type: str = "text"  # text, table, chart, summary
    order: int = 0

    def to_dict(self) -> Dict[str, Any]:
        """Convert section to dictionary."""
        return {
            "title": self.title,
            "content": self.content,
            "section_type": self.section_type,
            "order": self.order,
        }


@dataclass
class ReportData:
    """Container for report data."""
    config: ReportConfig
    sections: List[ReportSection] = field(default_factory=list)
    generated_at: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def add_section(self, title: str, content: Any,
                    section_type: str = "text") -> "ReportSection":
        """Add a section to the report."""
        section = ReportSection(
            title=title,
            content=content,
            section_type=section_type,
            order=len(self.sections)
        )
        self.sections.append(section)
        return section

    def to_dict(self) -> Dict[str, Any]:
        """Convert report data to dictionary."""
        return {
            "config": self.config.to_dict(),
            "sections": [s.to_dict() for s in self.sections],
            "generated_at": self.generated_at.isoformat(),
            "metadata": self.metadata,
        }


class ReportGenerator:
    """Main report generator class."""

    def __init__(self):
        self._data_providers: Dict[ReportType, Callable] = {}
        self._register_default_providers()

    def _register_default_providers(self):
        """Register default data providers for each report type."""
        self._data_providers[ReportType.SPRINT] = self._get_sprint_data
        self._data_providers[ReportType.TEAM] = self._get_team_data
        self._data_providers[ReportType.PROJECT] = self._get_project_data
        self._data_providers[ReportType.EXECUTIVE] = self._get_executive_data
        self._data_providers[ReportType.VELOCITY] = self._get_velocity_data
        self._data_providers[ReportType.BURNDOWN] = self._get_burndown_data

    def register_provider(self, report_type: ReportType,
                         provider: Callable) -> None:
        """Register a custom data provider for a report type."""
        self._data_providers[report_type] = provider

    def generate(self, config: ReportConfig) -> ReportData:
        """Generate a report based on configuration."""
        report = ReportData(config=config)

        # Get data provider for report type
        provider = self._data_providers.get(config.report_type)
        if provider:
            data = provider(config)
            self._populate_report(report, data)

        # Add summary if configured
        if config.include_summary:
            self._add_summary(report)

        return report

    def _populate_report(self, report: ReportData,
                        data: Dict[str, Any]) -> None:
        """Populate report with data from provider."""
        for key, value in data.items():
            section_type = "table" if isinstance(value, list) else "text"
            report.add_section(
                title=key.replace("_", " ").title(),
                content=value,
                section_type=section_type
            )

    def _add_summary(self, report: ReportData) -> None:
        """Add summary section to report."""
        summary = {
            "total_sections": len(report.sections),
            "report_type": report.config.report_type.value,
            "generated_at": report.generated_at.isoformat(),
        }
        report.add_section(
            title="Summary",
            content=summary,
            section_type="summary"
        )

    # Default data providers

    def _get_sprint_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get sprint report data."""
        return {
            "sprint_info": {
                "name": "Sprint 1",
                "start_date": (datetime.now() - timedelta(days=14)).isoformat(),
                "end_date": datetime.now().isoformat(),
                "status": "active",
            },
            "stories": [
                {"id": "STR-001", "title": "Story 1", "points": 5, "status": "done"},
                {"id": "STR-002", "title": "Story 2", "points": 8, "status": "in_progress"},
                {"id": "STR-003", "title": "Story 3", "points": 3, "status": "backlog"},
            ],
            "velocity": {
                "planned": 21,
                "completed": 13,
                "percentage": 62,
            },
            "burndown": [
                {"day": 1, "remaining": 21},
                {"day": 7, "remaining": 15},
                {"day": 14, "remaining": 8},
            ],
        }

    def _get_team_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get team report data."""
        return {
            "team_info": {
                "name": "Squad Plataforma E",
                "members": 11,
                "active_sprint": "Sprint 1",
            },
            "members": [
                {"id": "ORCH", "name": "Orquestrador", "stories_done": 5, "commits": 12},
                {"id": "BACK", "name": "Backend", "stories_done": 8, "commits": 25},
                {"id": "FRONT", "name": "Frontend", "stories_done": 6, "commits": 18},
                {"id": "QA", "name": "Quality", "stories_done": 10, "commits": 8},
            ],
            "performance": {
                "total_stories": 29,
                "total_commits": 63,
                "avg_cycle_time_days": 3.2,
            },
        }

    def _get_project_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get project report data."""
        return {
            "project_info": {
                "name": "Plataforma E",
                "version": "6.5",
                "status": "active",
            },
            "progress": {
                "total_stories": 50,
                "completed": 35,
                "in_progress": 8,
                "backlog": 7,
                "percentage": 70,
            },
            "metrics": {
                "code_coverage": 82.5,
                "test_pass_rate": 98.2,
                "avg_build_time_sec": 45,
            },
            "milestones": [
                {"name": "MVP", "status": "done", "date": "2025-12-01"},
                {"name": "Beta", "status": "in_progress", "date": "2026-01-15"},
                {"name": "GA", "status": "planned", "date": "2026-02-01"},
            ],
        }

    def _get_executive_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get executive summary data."""
        return {
            "kpis": {
                "velocity_trend": "+15%",
                "team_satisfaction": 4.2,
                "defect_rate": "2.3%",
                "on_time_delivery": "94%",
            },
            "highlights": [
                "Sprint velocity increased by 15% compared to last quarter",
                "Zero critical bugs in production for 30 days",
                "Team expanded from 8 to 11 agents",
            ],
            "risks": [
                {"description": "Technical debt accumulation", "severity": "medium"},
                {"description": "Resource constraints", "severity": "low"},
            ],
            "next_steps": [
                "Complete Plugin System implementation",
                "Launch APM Dashboard",
                "Increase test coverage to 90%",
            ],
        }

    def _get_velocity_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get velocity report data."""
        return {
            "velocity_history": [
                {"sprint": "Sprint -4", "planned": 25, "completed": 22},
                {"sprint": "Sprint -3", "planned": 28, "completed": 26},
                {"sprint": "Sprint -2", "planned": 30, "completed": 28},
                {"sprint": "Sprint -1", "planned": 32, "completed": 30},
                {"sprint": "Current", "planned": 35, "completed": 20},
            ],
            "average_velocity": 26.4,
            "velocity_trend": "increasing",
            "prediction_next_sprint": 32,
        }

    def _get_burndown_data(self, config: ReportConfig) -> Dict[str, Any]:
        """Get burndown chart data."""
        days = 14
        ideal_burndown = []
        actual_burndown = []
        total_points = 35

        for day in range(days + 1):
            ideal_remaining = total_points - (total_points / days * day)
            ideal_burndown.append({"day": day, "points": round(ideal_remaining, 1)})

            # Simulate actual (slightly behind ideal)
            if day <= 10:
                actual = ideal_remaining + (day * 0.5)
            else:
                actual = ideal_remaining - 2
            actual_burndown.append({"day": day, "points": max(0, round(actual, 1))})

        return {
            "ideal_burndown": ideal_burndown,
            "actual_burndown": actual_burndown,
            "total_points": total_points,
            "days_remaining": 4,
            "status": "on_track",
        }

    def export(self, report: ReportData, format: Optional[ReportFormat] = None) -> str:
        """Export report to specified format."""
        from .exporters import ExporterFactory

        export_format = format or report.config.format
        exporter = ExporterFactory.create(export_format)
        return exporter.export(report)


# Singleton instance
_generator: Optional[ReportGenerator] = None


def get_report_generator() -> ReportGenerator:
    """Get the global report generator instance."""
    global _generator
    if _generator is None:
        _generator = ReportGenerator()
    return _generator
