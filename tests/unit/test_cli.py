# -*- coding: utf-8 -*-
"""
Tests for CLI Tool
Plataforma E v6.5

Tests for Issue #445
"""

import pytest
from io import StringIO


class TestOutput:
    """Tests for Output class"""

    def test_output_creation(self):
        from factory.cli.output import Output

        output = Output()
        assert output._color_enabled is True

    def test_disable_color(self):
        from factory.cli.output import Output

        output = Output()
        output.disable_color()
        assert output._color_enabled is False

    def test_print_to_stream(self):
        from factory.cli.output import Output

        stream = StringIO()
        output = Output(stream=stream)
        output.disable_color()
        output.print("Hello")

        assert "Hello" in stream.getvalue()

    def test_header(self):
        from factory.cli.output import Output

        stream = StringIO()
        output = Output(stream=stream)
        output.disable_color()
        output.header("Test Header")

        assert "Test Header" in stream.getvalue()

    def test_success(self):
        from factory.cli.output import Output

        stream = StringIO()
        output = Output(stream=stream)
        output.disable_color()
        output.success("Done")

        assert "[OK]" in stream.getvalue()

    def test_error(self):
        from factory.cli.output import Output

        stream = StringIO()
        output = Output(stream=stream)
        output.disable_color()
        output.error("Failed")

        assert "[ERROR]" in stream.getvalue()

    def test_warning(self):
        from factory.cli.output import Output

        stream = StringIO()
        output = Output(stream=stream)
        output.disable_color()
        output.warning("Caution")

        assert "[WARN]" in stream.getvalue()


class TestColor:
    """Tests for Color enum"""

    def test_color_values(self):
        from factory.cli.output import Color

        assert Color.RESET.value == "\033[0m"
        assert Color.RED.value == "\033[91m"
        assert Color.GREEN.value == "\033[92m"


class TestCLI:
    """Tests for CLI class"""

    @pytest.fixture
    def cli(self):
        from factory.cli.main import CLI
        return CLI()

    def test_cli_creation(self, cli):
        assert cli.parser is not None
        assert cli.output is not None

    def test_no_command(self, cli):
        result = cli.run([])
        assert result == 0

    def test_status_command(self, cli):
        result = cli.run(["status", "--json"])
        assert result == 0

    def test_status_returns_data(self, cli):
        cli.output.disable_color()
        data = cli.cmd_status(type("Args", (), {"json": True})())
        assert "system" in data
        assert "status" in data

    def test_story_list(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {"json": True, "status": None, "limit": 10})()
        data = cli._story_list(args)
        assert "stories" in data
        assert "total" in data

    def test_story_create(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {
            "json": True,
            "title": "Test Story",
            "description": "Test Desc",
            "points": 5
        })()
        data = cli._story_create(args)
        assert data["title"] == "Test Story"
        assert data["points"] == 5

    def test_sprint_status(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {"json": True})()
        data = cli._sprint_status(args)
        assert "current_sprint" in data
        assert "velocity" in data

    def test_agent_list(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {"json": True})()
        data = cli._agent_list(args)
        assert "agents" in data
        assert data["total"] == 11

    def test_agent_status(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {"json": True})()
        data = cli._agent_status(args)
        assert data["total"] == 11

    def test_metrics_summary(self, cli):
        cli.output.disable_color()
        args = type("Args", (), {"json": True})()
        data = cli._metrics_summary(args)
        assert "total_requests" in data or data == {}


class TestCLICommands:
    """Tests for CLI command routing"""

    @pytest.fixture
    def cli(self):
        from factory.cli.main import CLI
        c = CLI()
        c.output.disable_color()
        return c

    def test_cmd_story_list(self, cli):
        args = type("Args", (), {
            "story_command": "list",
            "json": True,
            "status": None,
            "limit": 10
        })()
        result = cli.cmd_story(args)
        assert result is not None

    def test_cmd_story_create(self, cli):
        args = type("Args", (), {
            "story_command": "create",
            "json": True,
            "title": "New Story",
            "description": None,
            "points": None
        })()
        result = cli.cmd_story(args)
        assert result is not None
        assert result["title"] == "New Story"

    def test_cmd_sprint_status(self, cli):
        args = type("Args", (), {
            "sprint_command": "status",
            "json": True
        })()
        result = cli.cmd_sprint(args)
        assert result is not None

    def test_cmd_agent_list(self, cli):
        args = type("Args", (), {
            "agent_command": "list",
            "json": True
        })()
        result = cli.cmd_agent(args)
        assert result is not None

    def test_cmd_metrics_summary(self, cli):
        args = type("Args", (), {
            "metrics_command": "summary",
            "json": True
        })()
        result = cli.cmd_metrics(args)
        assert result is not None


class TestRunCLI:
    """Tests for run_cli function"""

    def test_run_cli_status(self):
        from factory.cli.main import run_cli
        result = run_cli(["status", "--no-color", "--json"])
        assert result == 0

    def test_run_cli_help(self):
        from factory.cli.main import run_cli
        # --help raises SystemExit(0)
        with pytest.raises(SystemExit) as exc_info:
            run_cli(["--help"])
        assert exc_info.value.code == 0


class TestCLIIntegration:
    """Integration tests for CLI"""

    def test_full_workflow(self):
        from factory.cli.main import CLI

        cli = CLI()
        cli.output.disable_color()

        # Check status
        result = cli.run(["status", "--json"])
        assert result == 0

        # List stories
        result = cli.run(["story", "list", "--json"])
        assert result == 0

        # List agents
        result = cli.run(["agent", "list", "--json"])
        assert result == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
