# -*- coding: utf-8 -*-
"""
CLI Main - Plataforma E
=======================

Command-line interface for the platform.

Issue #445: CLI Tool - Interface de Linha de Comando

Usage:
    plataforma-e story list
    plataforma-e story create --title "My Story"
    plataforma-e sprint status
    plataforma-e status
"""

import argparse
import sys
import json
from typing import Optional, List
from pathlib import Path

from .output import Output, Color


class CLI:
    """Main CLI class."""

    def __init__(self):
        self.output = Output()
        self.parser = self._create_parser()

    def _create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser."""
        # Parent parser for common arguments
        parent_parser = argparse.ArgumentParser(add_help=False)
        parent_parser.add_argument(
            "--json",
            action="store_true",
            help="Output in JSON format"
        )
        parent_parser.add_argument(
            "--no-color",
            action="store_true",
            help="Disable colored output"
        )

        parser = argparse.ArgumentParser(
            prog="plataforma-e",
            description="Plataforma E - Sistema de Desenvolvimento Autonomo",
            formatter_class=argparse.RawDescriptionHelpFormatter,
            parents=[parent_parser],
            epilog="""
Exemplos:
  plataforma-e status              Ver status do sistema
  plataforma-e story list          Listar stories
  plataforma-e story create        Criar nova story
  plataforma-e sprint status       Ver status do sprint
  plataforma-e agent list          Listar agentes
"""
        )

        parser.add_argument(
            "--version", "-v",
            action="version",
            version="Plataforma E v6.5"
        )

        subparsers = parser.add_subparsers(dest="command", help="Commands")
        self._parent_parser = parent_parser

        # Status command
        subparsers.add_parser("status", help="Show system status", parents=[parent_parser])

        # Story commands
        story_parser = subparsers.add_parser("story", help="Story commands", parents=[parent_parser])
        story_sub = story_parser.add_subparsers(dest="story_command")

        story_list = story_sub.add_parser("list", help="List stories", parents=[parent_parser])
        story_list.add_argument("--status", help="Filter by status")
        story_list.add_argument("--limit", type=int, default=10, help="Limit results")

        story_create = story_sub.add_parser("create", help="Create a story", parents=[parent_parser])
        story_create.add_argument("--title", "-t", required=True, help="Story title")
        story_create.add_argument("--description", "-d", help="Description")
        story_create.add_argument("--points", "-p", type=int, help="Story points")

        story_show = story_sub.add_parser("show", help="Show story details", parents=[parent_parser])
        story_show.add_argument("story_id", help="Story ID")

        # Sprint commands
        sprint_parser = subparsers.add_parser("sprint", help="Sprint commands", parents=[parent_parser])
        sprint_sub = sprint_parser.add_subparsers(dest="sprint_command")
        sprint_sub.add_parser("status", help="Show sprint status", parents=[parent_parser])
        sprint_sub.add_parser("list", help="List sprints", parents=[parent_parser])

        # Agent commands
        agent_parser = subparsers.add_parser("agent", help="Agent commands", parents=[parent_parser])
        agent_sub = agent_parser.add_subparsers(dest="agent_command")
        agent_sub.add_parser("list", help="List agents", parents=[parent_parser])
        agent_sub.add_parser("status", help="Show agent status", parents=[parent_parser])

        # Metrics commands
        metrics_parser = subparsers.add_parser("metrics", help="Metrics commands", parents=[parent_parser])
        metrics_sub = metrics_parser.add_subparsers(dest="metrics_command")
        metrics_sub.add_parser("summary", help="Show metrics summary", parents=[parent_parser])
        metrics_sub.add_parser("slow", help="Show slow endpoints", parents=[parent_parser])

        return parser

    def run(self, args: Optional[List[str]] = None) -> int:
        """
        Run CLI with given arguments.

        Returns:
            Exit code (0 for success)
        """
        parsed = self.parser.parse_args(args)

        if parsed.no_color:
            self.output.disable_color()

        if not parsed.command:
            self.parser.print_help()
            return 0

        # Route to command handler
        handler = getattr(self, f"cmd_{parsed.command}", None)
        if handler:
            try:
                result = handler(parsed)
                if parsed.json and result is not None:
                    print(json.dumps(result, indent=2, ensure_ascii=False))
                return 0
            except Exception as e:
                self.output.error(f"Error: {e}")
                return 1
        else:
            self.output.error(f"Unknown command: {parsed.command}")
            return 1

    def cmd_status(self, args) -> dict:
        """Show system status."""
        status = {
            "system": "Plataforma E v6.5",
            "status": "running",
            "agents": {
                "total": 11,
                "active": 0
            },
            "stories": {
                "backlog": 0,
                "in_progress": 0,
                "done": 0
            }
        }

        if not args.json:
            self.output.header("Plataforma E - Status")
            self.output.success("Sistema: Online")
            self.output.info(f"Agentes: {status['agents']['total']} configurados")
            self.output.info("Stories: Use 'story list' para ver")

        return status

    def cmd_story(self, args) -> Optional[dict]:
        """Story commands."""
        if args.story_command == "list":
            return self._story_list(args)
        elif args.story_command == "create":
            return self._story_create(args)
        elif args.story_command == "show":
            return self._story_show(args)
        else:
            self.output.warning("Use: story [list|create|show]")
            return None

    def _story_list(self, args) -> dict:
        """List stories."""
        # Mock data for now
        stories = [
            {"id": "STR-001", "title": "Exemplo Story 1", "status": "backlog", "points": 5},
            {"id": "STR-002", "title": "Exemplo Story 2", "status": "in_progress", "points": 8},
        ]

        if not args.json:
            self.output.header("Stories")
            for s in stories:
                status_color = Color.GREEN if s["status"] == "done" else Color.YELLOW
                self.output.print(
                    f"  [{s['id']}] {s['title']} "
                    f"({s['points']} pts) - ",
                    end=""
                )
                self.output.print(s["status"], color=status_color)

        return {"stories": stories, "total": len(stories)}

    def _story_create(self, args) -> dict:
        """Create a story."""
        story = {
            "id": "STR-NEW",
            "title": args.title,
            "description": args.description or "",
            "points": args.points or 0,
            "status": "backlog"
        }

        if not args.json:
            self.output.success(f"Story criada: {story['id']}")
            self.output.info(f"  Titulo: {story['title']}")

        return story

    def _story_show(self, args) -> dict:
        """Show story details."""
        story = {
            "id": args.story_id,
            "title": "Exemplo Story",
            "description": "Descricao da story",
            "status": "backlog",
            "points": 5,
            "tasks": []
        }

        if not args.json:
            self.output.header(f"Story {story['id']}")
            self.output.print(f"  Titulo: {story['title']}")
            self.output.print(f"  Status: {story['status']}")
            self.output.print(f"  Points: {story['points']}")

        return story

    def cmd_sprint(self, args) -> Optional[dict]:
        """Sprint commands."""
        if args.sprint_command == "status":
            return self._sprint_status(args)
        elif args.sprint_command == "list":
            return self._sprint_list(args)
        else:
            self.output.warning("Use: sprint [status|list]")
            return None

    def _sprint_status(self, args) -> dict:
        """Show sprint status."""
        status = {
            "current_sprint": "Sprint 1",
            "start_date": "2025-01-01",
            "end_date": "2025-01-15",
            "velocity": 32,
            "stories_done": 3,
            "stories_total": 8
        }

        if not args.json:
            self.output.header("Sprint Atual")
            self.output.print(f"  Nome: {status['current_sprint']}")
            self.output.print(f"  Periodo: {status['start_date']} - {status['end_date']}")
            self.output.print(f"  Progresso: {status['stories_done']}/{status['stories_total']} stories")
            self.output.print(f"  Velocity: {status['velocity']} pts")

        return status

    def _sprint_list(self, args) -> dict:
        """List sprints."""
        sprints = [
            {"id": "SPR-001", "name": "Sprint 1", "status": "active"},
            {"id": "SPR-002", "name": "Sprint 2", "status": "planned"},
        ]

        if not args.json:
            self.output.header("Sprints")
            for s in sprints:
                self.output.print(f"  [{s['id']}] {s['name']} - {s['status']}")

        return {"sprints": sprints}

    def cmd_agent(self, args) -> Optional[dict]:
        """Agent commands."""
        if args.agent_command == "list":
            return self._agent_list(args)
        elif args.agent_command == "status":
            return self._agent_status(args)
        else:
            self.output.warning("Use: agent [list|status]")
            return None

    def _agent_list(self, args) -> dict:
        """List agents."""
        agents = [
            {"id": "ORCH", "name": "Orquestrador", "status": "idle"},
            {"id": "BACK", "name": "Backend", "status": "idle"},
            {"id": "FRONT", "name": "Frontend", "status": "idle"},
            {"id": "QA", "name": "Quality Assurance", "status": "idle"},
            {"id": "SEC", "name": "Security", "status": "idle"},
            {"id": "DEVOPS", "name": "DevOps", "status": "idle"},
            {"id": "ARCH", "name": "Arquiteto", "status": "idle"},
            {"id": "PROD", "name": "Produto", "status": "idle"},
            {"id": "INOV", "name": "Inovacao", "status": "idle"},
            {"id": "FIN", "name": "Financeiro", "status": "idle"},
            {"id": "GROWTH", "name": "Growth", "status": "idle"},
        ]

        if not args.json:
            self.output.header("Agentes (11)")
            for a in agents:
                status_color = Color.GREEN if a["status"] == "active" else Color.GRAY
                self.output.print(f"  [{a['id']}] {a['name']}", end=" - ")
                self.output.print(a["status"], color=status_color)

        return {"agents": agents, "total": len(agents)}

    def _agent_status(self, args) -> dict:
        """Show agent status."""
        status = {
            "total": 11,
            "active": 0,
            "idle": 11,
            "error": 0
        }

        if not args.json:
            self.output.header("Status dos Agentes")
            self.output.print(f"  Total: {status['total']}")
            self.output.success(f"  Ativos: {status['active']}")
            self.output.info(f"  Ociosos: {status['idle']}")

        return status

    def cmd_metrics(self, args) -> Optional[dict]:
        """Metrics commands."""
        if args.metrics_command == "summary":
            return self._metrics_summary(args)
        elif args.metrics_command == "slow":
            return self._metrics_slow(args)
        else:
            self.output.warning("Use: metrics [summary|slow]")
            return None

    def _metrics_summary(self, args) -> dict:
        """Show metrics summary."""
        try:
            from factory.monitoring.apm import get_metrics_store
            store = get_metrics_store()
            summary = store.get_summary()
        except ImportError:
            summary = {
                "total_requests": 0,
                "avg_duration_ms": 0,
                "error_rate": 0
            }

        if not args.json:
            self.output.header("Metricas")
            self.output.print(f"  Requests: {summary.get('total_requests', 0)}")
            self.output.print(f"  Avg Duration: {summary.get('avg_duration_ms', 0):.2f}ms")
            self.output.print(f"  Error Rate: {summary.get('error_rate', 0):.2f}%")

        return summary

    def _metrics_slow(self, args) -> dict:
        """Show slow endpoints."""
        try:
            from factory.monitoring.apm import get_metrics_store
            store = get_metrics_store()
            slow = store.get_slow_endpoints()
        except ImportError:
            slow = []

        if not args.json:
            self.output.header("Endpoints Lentos")
            if slow:
                for s in slow:
                    self.output.warning(
                        f"  {s['method']} {s['endpoint']} - {s['avg_duration_ms']:.2f}ms"
                    )
            else:
                self.output.success("  Nenhum endpoint lento detectado")

        return {"slow_endpoints": slow}


# Global CLI instance
cli = CLI()


def run_cli(args: Optional[List[str]] = None) -> int:
    """Run the CLI."""
    return cli.run(args)


def main():
    """Entry point."""
    sys.exit(run_cli())


if __name__ == "__main__":
    main()
