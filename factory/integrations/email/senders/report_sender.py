# -*- coding: utf-8 -*-
"""
Report Sender
=============
Enviador de relatorios automaticos por email.

Tipos de relatorios:
- Relatorio diario
- Relatorio semanal
- Relatorio de projeto
- Relatorio de sprint

Uso:
    from factory.integrations.email.senders import ReportSender

    sender = ReportSender(graph_client)
    await sender.send_daily_report(report_data, recipients)
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class ReportType(str, Enum):
    """Tipos de relatorio"""
    DAILY = "daily"
    WEEKLY = "weekly"
    MONTHLY = "monthly"
    PROJECT = "project"
    SPRINT = "sprint"
    CUSTOM = "custom"


@dataclass
class ReportConfig:
    """Configuracao de relatorios"""
    enabled: bool = True
    daily_report_time: str = "09:00"
    weekly_report_day: int = 1  # Segunda-feira
    default_recipients: List[str] = field(default_factory=list)
    include_charts: bool = True
    dashboard_url: str = "http://localhost:9001"


class ReportSender:
    """
    Enviador de relatorios automaticos.

    Gera e envia relatorios periodicos sobre o estado dos projetos,
    stories e atividades do sistema.

    Exemplo:
        client = MicrosoftGraphClient(config)
        sender = ReportSender(client)

        # Enviar relatorio diario
        await sender.send_daily_report()

        # Enviar relatorio de projeto
        await sender.send_project_report(project_data)
    """

    def __init__(
        self,
        email_client,
        config: Optional[ReportConfig] = None
    ):
        """
        Inicializa o sender.

        Args:
            email_client: Cliente de email (Graph ou SMTP)
            config: Configuracao de relatorios
        """
        self.client = email_client
        self.config = config or ReportConfig()

    async def send_report(
        self,
        report_type: ReportType,
        report_data: Dict[str, Any],
        recipients: Optional[List[str]] = None,
        subject: Optional[str] = None,
        attachments: Optional[List] = None
    ) -> bool:
        """
        Envia um relatorio.

        Args:
            report_type: Tipo do relatorio
            report_data: Dados do relatorio
            recipients: Destinatarios
            subject: Assunto customizado
            attachments: Anexos

        Returns:
            bool: True se enviado
        """
        if not self.config.enabled:
            logger.debug("Relatorios desabilitados")
            return False

        if not recipients:
            recipients = self.config.default_recipients

        if not recipients:
            logger.warning("Nenhum destinatario para o relatorio")
            return False

        # Define template baseado no tipo
        template_mapping = {
            ReportType.DAILY: "daily_report",
            ReportType.WEEKLY: "daily_report",  # Usa mesmo template
            ReportType.MONTHLY: "daily_report",
            ReportType.PROJECT: "project_completed",
            ReportType.SPRINT: "daily_report"
        }

        template_name = template_mapping.get(report_type, "daily_report")

        # Adiciona dados padrao
        report_data["dashboard_url"] = self.config.dashboard_url
        report_data["report_type"] = report_type.value
        report_data["generated_at"] = datetime.now().strftime("%d/%m/%Y %H:%M")

        # Define assunto padrao
        if not subject:
            subject = self._get_default_subject(report_type, report_data)

        try:
            return await self.client.send_email_with_template(
                to=recipients,
                template_name=template_name,
                template_vars=report_data,
                subject=subject,
                attachments=attachments
            )
        except Exception as e:
            logger.exception(f"Erro ao enviar relatorio: {e}")
            return False

    def _get_default_subject(
        self,
        report_type: ReportType,
        report_data: Dict[str, Any]
    ) -> str:
        """Gera assunto padrao"""
        date_str = datetime.now().strftime("%d/%m/%Y")

        subjects = {
            ReportType.DAILY: f"[Fabrica] Relatorio Diario - {date_str}",
            ReportType.WEEKLY: f"[Fabrica] Relatorio Semanal - Semana {datetime.now().isocalendar()[1]}",
            ReportType.MONTHLY: f"[Fabrica] Relatorio Mensal - {datetime.now().strftime('%B %Y')}",
            ReportType.PROJECT: f"[Fabrica] Relatorio: {report_data.get('project_name', 'Projeto')}",
            ReportType.SPRINT: f"[Fabrica] Relatorio Sprint {report_data.get('sprint_number', '')}"
        }

        return subjects.get(report_type, f"[Fabrica] Relatorio - {date_str}")

    # =========================================================================
    # RELATORIOS ESPECIFICOS
    # =========================================================================

    async def send_daily_report(
        self,
        data: Optional[Dict[str, Any]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Envia relatorio diario.

        Args:
            data: Dados do relatorio (gera automaticamente se nao informado)
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        report_data = data or await self._generate_daily_report_data()

        report_data["report_date"] = datetime.now().strftime("%d/%m/%Y")
        report_data["next_report_time"] = self.config.daily_report_time

        return await self.send_report(
            ReportType.DAILY,
            report_data,
            recipients
        )

    async def send_weekly_report(
        self,
        data: Optional[Dict[str, Any]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Envia relatorio semanal.

        Args:
            data: Dados do relatorio
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        report_data = data or await self._generate_weekly_report_data()

        today = datetime.now()
        week_start = today - timedelta(days=today.weekday())
        week_end = week_start + timedelta(days=6)

        report_data["report_date"] = f"{week_start.strftime('%d/%m')} - {week_end.strftime('%d/%m/%Y')}"
        report_data["week_number"] = today.isocalendar()[1]

        return await self.send_report(
            ReportType.WEEKLY,
            report_data,
            recipients
        )

    async def send_project_report(
        self,
        project_data: Dict[str, Any],
        execution_data: Optional[Dict[str, Any]] = None,
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Envia relatorio de projeto.

        Args:
            project_data: Dados do projeto
            execution_data: Dados de execucao
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        exec_data = execution_data or {}

        report_data = {
            "project_id": project_data.get("project_id", ""),
            "project_name": project_data.get("name", project_data.get("project_name", "")),
            "started_at": exec_data.get("started_at", ""),
            "completed_at": exec_data.get("completed_at", datetime.now().strftime("%d/%m/%Y %H:%M")),
            "execution_time": exec_data.get("execution_time", "N/A"),
            "files_count": exec_data.get("files_count", 0),
            "stories_completed": exec_data.get("stories_completed", 0),
            "lines_of_code": exec_data.get("lines_of_code", "N/A"),
            "tests_passed": exec_data.get("tests_passed"),
            "tests_failed": exec_data.get("tests_failed", 0),
            "agents_count": exec_data.get("agents_count", 1),
            "files_generated": exec_data.get("files_generated", []),
            "project_url": f"{self.config.dashboard_url}/projects/{project_data.get('project_id', '')}",
            "documentation_url": exec_data.get("documentation_url")
        }

        return await self.send_report(
            ReportType.PROJECT,
            report_data,
            recipients
        )

    async def send_sprint_report(
        self,
        sprint_data: Dict[str, Any],
        stories: List[Dict[str, Any]],
        recipients: Optional[List[str]] = None
    ) -> bool:
        """
        Envia relatorio de sprint.

        Args:
            sprint_data: Dados do sprint
            stories: Lista de stories do sprint
            recipients: Destinatarios

        Returns:
            bool: True se enviado
        """
        # Calcula metricas do sprint
        total_points = sum(s.get("story_points", 0) for s in stories)
        completed_points = sum(
            s.get("story_points", 0)
            for s in stories
            if s.get("status") == "done"
        )

        completed_stories = [s for s in stories if s.get("status") == "done"]
        in_progress_stories = [s for s in stories if s.get("status") == "in_progress"]
        pending_stories = [s for s in stories if s.get("status") in ("backlog", "ready")]

        report_data = {
            "report_date": datetime.now().strftime("%d/%m/%Y"),
            "sprint_number": sprint_data.get("sprint_number", ""),
            "sprint_name": sprint_data.get("name", ""),
            "sprint_goal": sprint_data.get("goal", ""),
            "start_date": sprint_data.get("start_date", ""),
            "end_date": sprint_data.get("end_date", ""),

            # Metricas
            "total_points": total_points,
            "completed_points": completed_points,
            "velocity": completed_points,
            "completion_rate": round(completed_points / total_points * 100) if total_points > 0 else 0,

            # Stories
            "stories_total": len(stories),
            "stories_completed": len(completed_stories),
            "tasks_completed": sum(s.get("tasks_completed", 0) for s in stories),
            "completed_stories": [
                {
                    "id": s.get("story_id"),
                    "title": s.get("title"),
                    "project": s.get("project_name", ""),
                    "points": s.get("story_points", 0)
                }
                for s in completed_stories[:10]
            ],
            "pending_stories": [
                {
                    "id": s.get("story_id"),
                    "title": s.get("title"),
                    "points": s.get("story_points", 0)
                }
                for s in pending_stories[:5]
            ],

            # Projetos ativos
            "projects_active": len(set(s.get("project_id") for s in stories if s.get("project_id"))),
            "projects": self._group_stories_by_project(stories)
        }

        return await self.send_report(
            ReportType.SPRINT,
            report_data,
            recipients
        )

    def _group_stories_by_project(
        self,
        stories: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """Agrupa stories por projeto"""
        projects = {}

        for story in stories:
            project_id = story.get("project_id", "unknown")
            if project_id not in projects:
                projects[project_id] = {
                    "id": project_id,
                    "name": story.get("project_name", project_id),
                    "stories_done": 0,
                    "stories_total": 0,
                    "status": "in_progress"
                }

            projects[project_id]["stories_total"] += 1
            if story.get("status") == "done":
                projects[project_id]["stories_done"] += 1

        # Calcula progresso
        for proj in projects.values():
            if proj["stories_total"] > 0:
                proj["progress"] = round(proj["stories_done"] / proj["stories_total"] * 100)
            else:
                proj["progress"] = 0

            if proj["stories_done"] == proj["stories_total"]:
                proj["status"] = "done"

        return list(projects.values())

    # =========================================================================
    # GERACAO DE DADOS
    # =========================================================================

    async def _generate_daily_report_data(self) -> Dict[str, Any]:
        """
        Gera dados para relatorio diario.

        Retorna dados mockados - em producao, buscar do banco de dados.
        """
        # Em producao, isso buscaria dados reais do sistema
        return {
            "projects_active": 0,
            "stories_completed": 0,
            "tasks_completed": 0,
            "workers_active": 0,
            "workers_total": 2,
            "total_processing_time": "0h",
            "files_generated": 0,
            "lines_of_code": 0,
            "errors_count": 0,
            "projects": [],
            "completed_stories": [],
            "pending_stories": [],
            "alerts": []
        }

    async def _generate_weekly_report_data(self) -> Dict[str, Any]:
        """
        Gera dados para relatorio semanal.
        """
        # Em producao, isso buscaria dados reais agregados da semana
        return await self._generate_daily_report_data()


# =============================================================================
# SCHEDULER DE RELATORIOS
# =============================================================================

class ReportScheduler:
    """
    Agendador de envio automatico de relatorios.

    Exemplo:
        scheduler = ReportScheduler(report_sender)
        await scheduler.start()
    """

    def __init__(
        self,
        report_sender: ReportSender,
        daily_time: str = "09:00",
        weekly_day: int = 1  # Segunda
    ):
        self.sender = report_sender
        self.daily_time = daily_time
        self.weekly_day = weekly_day
        self._running = False

    async def start(self):
        """Inicia o agendador"""
        import asyncio

        self._running = True
        logger.info("Agendador de relatorios iniciado")

        while self._running:
            now = datetime.now()

            # Verifica se deve enviar relatorio diario
            if now.strftime("%H:%M") == self.daily_time:
                try:
                    await self.sender.send_daily_report()
                    logger.info("Relatorio diario enviado")
                except Exception as e:
                    logger.error(f"Erro ao enviar relatorio diario: {e}")

                # Verifica se e dia de relatorio semanal
                if now.weekday() == self.weekly_day:
                    try:
                        await self.sender.send_weekly_report()
                        logger.info("Relatorio semanal enviado")
                    except Exception as e:
                        logger.error(f"Erro ao enviar relatorio semanal: {e}")

            # Aguarda 1 minuto antes de verificar novamente
            await asyncio.sleep(60)

    def stop(self):
        """Para o agendador"""
        self._running = False
        logger.info("Agendador de relatorios parado")


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_report_sender: Optional[ReportSender] = None


def get_report_sender(email_client=None) -> ReportSender:
    """Retorna instancia global do report sender"""
    global _report_sender
    if _report_sender is None:
        if email_client is None:
            from ..graph_mail_client import get_graph_client
            email_client = get_graph_client()
        _report_sender = ReportSender(email_client)
    return _report_sender
