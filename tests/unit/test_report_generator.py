# -*- coding: utf-8 -*-
"""
Tests for Report Generator
Plataforma E v6.5

Tests for Issue #446
"""

import pytest
from datetime import datetime, timedelta


class TestReportType:
    """Tests for ReportType enum"""

    def test_report_types(self):
        from factory.reports.report_generator import ReportType

        assert ReportType.SPRINT.value == "sprint"
        assert ReportType.TEAM.value == "team"
        assert ReportType.PROJECT.value == "project"
        assert ReportType.EXECUTIVE.value == "executive"
        assert ReportType.VELOCITY.value == "velocity"
        assert ReportType.BURNDOWN.value == "burndown"


class TestReportFormat:
    """Tests for ReportFormat enum"""

    def test_report_formats(self):
        from factory.reports.report_generator import ReportFormat

        assert ReportFormat.HTML.value == "html"
        assert ReportFormat.CSV.value == "csv"
        assert ReportFormat.JSON.value == "json"
        assert ReportFormat.PDF.value == "pdf"
        assert ReportFormat.EXCEL.value == "excel"


class TestReportConfig:
    """Tests for ReportConfig dataclass"""

    def test_config_creation(self):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Sprint Report"
        )

        assert config.report_type == ReportType.SPRINT
        assert config.title == "Sprint Report"

    def test_config_defaults(self):
        from factory.reports.report_generator import ReportConfig, ReportType, ReportFormat

        config = ReportConfig(
            report_type=ReportType.TEAM,
            title="Team Report"
        )

        assert config.format == ReportFormat.HTML
        assert config.include_charts is True
        assert config.include_summary is True
        assert config.filters == {}

    def test_config_to_dict(self):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.PROJECT,
            title="Project Report"
        )

        d = config.to_dict()
        assert d["report_type"] == "project"
        assert d["title"] == "Project Report"
        assert d["format"] == "html"


class TestReportData:
    """Tests for ReportData dataclass"""

    def test_data_creation(self):
        from factory.reports.report_generator import ReportData, ReportConfig, ReportType

        config = ReportConfig(report_type=ReportType.SPRINT, title="Test")
        report = ReportData(config=config)

        assert report.config == config
        assert report.sections == []
        assert report.generated_at is not None

    def test_add_section(self):
        from factory.reports.report_generator import ReportData, ReportConfig, ReportType

        config = ReportConfig(report_type=ReportType.SPRINT, title="Test")
        report = ReportData(config=config)

        section = report.add_section("Test Section", "Test content", "text")

        assert len(report.sections) == 1
        assert section.title == "Test Section"
        assert section.content == "Test content"

    def test_to_dict(self):
        from factory.reports.report_generator import ReportData, ReportConfig, ReportType

        config = ReportConfig(report_type=ReportType.SPRINT, title="Test")
        report = ReportData(config=config)
        report.add_section("Section 1", "Content 1")

        d = report.to_dict()
        assert "config" in d
        assert "sections" in d
        assert "generated_at" in d
        assert len(d["sections"]) == 1


class TestReportGenerator:
    """Tests for ReportGenerator class"""

    @pytest.fixture
    def generator(self):
        from factory.reports.report_generator import ReportGenerator
        return ReportGenerator()

    def test_generator_creation(self, generator):
        assert generator is not None
        assert len(generator._data_providers) > 0

    def test_generate_sprint_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Sprint Report"
        )

        report = generator.generate(config)

        assert report is not None
        assert len(report.sections) > 0

    def test_generate_team_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.TEAM,
            title="Team Report"
        )

        report = generator.generate(config)

        assert report is not None
        # Should have team data sections
        section_titles = [s.title for s in report.sections]
        assert any("Team" in t for t in section_titles)

    def test_generate_project_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.PROJECT,
            title="Project Report"
        )

        report = generator.generate(config)

        assert report is not None
        section_titles = [s.title for s in report.sections]
        assert any("Project" in t or "Progress" in t for t in section_titles)

    def test_generate_executive_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.EXECUTIVE,
            title="Executive Summary"
        )

        report = generator.generate(config)

        assert report is not None
        section_titles = [s.title for s in report.sections]
        assert any("Kpis" in t or "Highlights" in t for t in section_titles)

    def test_generate_velocity_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.VELOCITY,
            title="Velocity Report"
        )

        report = generator.generate(config)

        assert report is not None

    def test_generate_burndown_report(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.BURNDOWN,
            title="Burndown Report"
        )

        report = generator.generate(config)

        assert report is not None

    def test_include_summary(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Test",
            include_summary=True
        )

        report = generator.generate(config)

        section_titles = [s.title for s in report.sections]
        assert "Summary" in section_titles

    def test_exclude_summary(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Test",
            include_summary=False
        )

        report = generator.generate(config)

        section_titles = [s.title for s in report.sections]
        assert "Summary" not in section_titles

    def test_register_custom_provider(self, generator):
        from factory.reports.report_generator import ReportConfig, ReportType

        def custom_provider(config):
            return {"custom_data": "test value"}

        generator.register_provider(ReportType.CUSTOM, custom_provider)

        config = ReportConfig(
            report_type=ReportType.CUSTOM,
            title="Custom Report"
        )

        report = generator.generate(config)

        assert report is not None
        section_titles = [s.title for s in report.sections]
        assert "Custom Data" in section_titles


class TestHTMLExporter:
    """Tests for HTMLExporter"""

    @pytest.fixture
    def report(self):
        from factory.reports.report_generator import (
            ReportConfig, ReportType, ReportData
        )

        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Test Sprint Report"
        )
        report = ReportData(config=config)
        report.add_section("Overview", {"status": "active", "days": 14})
        report.add_section("Stories", [
            {"id": "STR-001", "title": "Story 1", "points": 5},
            {"id": "STR-002", "title": "Story 2", "points": 8},
        ], "table")
        return report

    def test_export_html(self, report):
        from factory.reports.exporters import HTMLExporter

        exporter = HTMLExporter()
        html = exporter.export(report)

        assert "<!DOCTYPE html>" in html
        assert "Test Sprint Report" in html
        assert "Overview" in html
        assert "Stories" in html

    def test_html_contains_table(self, report):
        from factory.reports.exporters import HTMLExporter

        exporter = HTMLExporter()
        html = exporter.export(report)

        assert "<table>" in html
        assert "STR-001" in html


class TestCSVExporter:
    """Tests for CSVExporter"""

    @pytest.fixture
    def report(self):
        from factory.reports.report_generator import (
            ReportConfig, ReportType, ReportData
        )

        config = ReportConfig(
            report_type=ReportType.TEAM,
            title="Team CSV Report"
        )
        report = ReportData(config=config)
        report.add_section("Members", [
            {"name": "Agent 1", "stories": 5},
            {"name": "Agent 2", "stories": 8},
        ], "table")
        return report

    def test_export_csv(self, report):
        from factory.reports.exporters import CSVExporter

        exporter = CSVExporter()
        csv_content = exporter.export(report)

        assert "Team CSV Report" in csv_content
        assert "Agent 1" in csv_content
        assert "Agent 2" in csv_content


class TestJSONExporter:
    """Tests for JSONExporter"""

    @pytest.fixture
    def report(self):
        from factory.reports.report_generator import (
            ReportConfig, ReportType, ReportData
        )

        config = ReportConfig(
            report_type=ReportType.PROJECT,
            title="Project JSON Report"
        )
        report = ReportData(config=config)
        report.add_section("Progress", {"completed": 75})
        return report

    def test_export_json(self, report):
        from factory.reports.exporters import JSONExporter
        import json

        exporter = JSONExporter()
        json_content = exporter.export(report)

        data = json.loads(json_content)
        assert data["config"]["title"] == "Project JSON Report"
        assert len(data["sections"]) == 1


class TestExporterFactory:
    """Tests for ExporterFactory"""

    def test_create_html_exporter(self):
        from factory.reports.exporters import ExporterFactory, HTMLExporter
        from factory.reports.report_generator import ReportFormat

        exporter = ExporterFactory.create(ReportFormat.HTML)
        assert isinstance(exporter, HTMLExporter)

    def test_create_csv_exporter(self):
        from factory.reports.exporters import ExporterFactory, CSVExporter
        from factory.reports.report_generator import ReportFormat

        exporter = ExporterFactory.create(ReportFormat.CSV)
        assert isinstance(exporter, CSVExporter)

    def test_create_json_exporter(self):
        from factory.reports.exporters import ExporterFactory, JSONExporter
        from factory.reports.report_generator import ReportFormat

        exporter = ExporterFactory.create(ReportFormat.JSON)
        assert isinstance(exporter, JSONExporter)

    def test_available_formats(self):
        from factory.reports.exporters import ExporterFactory

        formats = ExporterFactory.available_formats()
        assert "html" in formats
        assert "csv" in formats
        assert "json" in formats
        assert "pdf" in formats
        assert "excel" in formats

    def test_unknown_format(self):
        from factory.reports.exporters import ExporterFactory

        with pytest.raises(ValueError):
            ExporterFactory.create("unknown")


class TestGlobalReportGenerator:
    """Tests for global report generator"""

    def test_get_report_generator_singleton(self):
        from factory.reports.report_generator import get_report_generator

        gen1 = get_report_generator()
        gen2 = get_report_generator()
        assert gen1 is gen2


class TestReportExport:
    """Tests for report export via generator"""

    def test_export_via_generator(self):
        from factory.reports.report_generator import (
            ReportGenerator, ReportConfig, ReportType, ReportFormat
        )

        generator = ReportGenerator()
        config = ReportConfig(
            report_type=ReportType.SPRINT,
            title="Export Test",
            format=ReportFormat.JSON
        )

        report = generator.generate(config)
        output = generator.export(report)

        assert output is not None
        assert "Export Test" in output


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
