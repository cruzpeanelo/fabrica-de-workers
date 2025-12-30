# -*- coding: utf-8 -*-
"""
Scheduled Reports Module (Issue #257)
======================================
Sistema de relatorios agendados por email.

Funcionalidades:
- Configurar relatorios recorrentes
- Multiplos formatos (PDF, Excel, HTML)
- Agendamento flexivel (diario, semanal, mensal)
- Lista de destinatarios
- Templates de relatorios pre-definidos
"""

from fastapi import APIRouter, HTTPException, Query, BackgroundTasks
from fastapi.responses import HTMLResponse
from pydantic import BaseModel, EmailStr, validator
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from enum import Enum
import uuid
import json

# Database imports
from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask, Sprint, Epic, Project


# =============================================================================
# ENUMS
# =============================================================================

class ReportFormat(str, Enum):
    """Formatos de relatorio disponiveis."""
    PDF = "pdf"
    EXCEL = "excel"
    HTML = "html"


class ReportFrequency(str, Enum):
    """Frequencia de envio do relatorio."""
    DAILY = "daily"
    WEEKLY = "weekly"
    BIWEEKLY = "biweekly"
    MONTHLY = "monthly"
    QUARTERLY = "quarterly"


class ReportType(str, Enum):
    """Tipos de relatorio disponiveis."""
    SPRINT_SUMMARY = "sprint_summary"
    VELOCITY = "velocity"
    BURNDOWN = "burndown"
    STATUS = "status"
    PRODUCTIVITY = "productivity"
    TEAM_PERFORMANCE = "team_performance"
    PROJECT_OVERVIEW = "project_overview"
    EPIC_PROGRESS = "epic_progress"


class DayOfWeek(str, Enum):
    """Dias da semana para agendamento semanal."""
    MONDAY = "monday"
    TUESDAY = "tuesday"
    WEDNESDAY = "wednesday"
    THURSDAY = "thursday"
    FRIDAY = "friday"
    SATURDAY = "saturday"
    SUNDAY = "sunday"


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class Recipient(BaseModel):
    """Destinatario do relatorio."""
    email: str
    name: Optional[str] = None
    role: Optional[str] = None  # manager, developer, stakeholder

    class Config:
        schema_extra = {
            "example": {
                "email": "manager@empresa.com",
                "name": "Joao Silva",
                "role": "manager"
            }
        }


class ScheduleConfig(BaseModel):
    """Configuracao de agendamento."""
    frequency: ReportFrequency
    time: str = "09:00"  # Horario de envio (HH:MM)
    day_of_week: Optional[DayOfWeek] = None  # Para semanal
    day_of_month: Optional[int] = None  # Para mensal (1-28)
    timezone: str = "America/Sao_Paulo"

    @validator('day_of_month')
    def validate_day_of_month(cls, v):
        if v is not None and (v < 1 or v > 28):
            raise ValueError('day_of_month must be between 1 and 28')
        return v

    @validator('time')
    def validate_time(cls, v):
        try:
            datetime.strptime(v, '%H:%M')
        except ValueError:
            raise ValueError('time must be in HH:MM format')
        return v


class ReportFilters(BaseModel):
    """Filtros para geracao do relatorio."""
    project_id: Optional[str] = None
    sprint_id: Optional[str] = None
    epic_id: Optional[str] = None
    date_range_days: Optional[int] = 30
    include_completed: bool = True
    include_blocked: bool = True
    assignees: Optional[List[str]] = None
    priorities: Optional[List[str]] = None


class ScheduledReportCreate(BaseModel):
    """Request para criar um relatorio agendado."""
    name: str
    description: Optional[str] = None
    report_type: ReportType
    format: ReportFormat = ReportFormat.HTML
    schedule: ScheduleConfig
    recipients: List[Recipient]
    filters: Optional[ReportFilters] = None
    include_charts: bool = True
    include_summary: bool = True
    custom_branding: Optional[Dict[str, str]] = None

    class Config:
        schema_extra = {
            "example": {
                "name": "Relatorio Semanal de Sprint",
                "description": "Resumo semanal do progresso do sprint atual",
                "report_type": "sprint_summary",
                "format": "pdf",
                "schedule": {
                    "frequency": "weekly",
                    "time": "09:00",
                    "day_of_week": "monday"
                },
                "recipients": [
                    {"email": "manager@empresa.com", "name": "Joao Silva"}
                ],
                "filters": {
                    "project_id": "PRJ-001"
                }
            }
        }


class ScheduledReportUpdate(BaseModel):
    """Request para atualizar um relatorio agendado."""
    name: Optional[str] = None
    description: Optional[str] = None
    report_type: Optional[ReportType] = None
    format: Optional[ReportFormat] = None
    schedule: Optional[ScheduleConfig] = None
    recipients: Optional[List[Recipient]] = None
    filters: Optional[ReportFilters] = None
    include_charts: Optional[bool] = None
    include_summary: Optional[bool] = None
    is_active: Optional[bool] = None
    custom_branding: Optional[Dict[str, str]] = None


class ScheduledReport(BaseModel):
    """Modelo de relatorio agendado (resposta)."""
    id: str
    name: str
    description: Optional[str]
    report_type: ReportType
    format: ReportFormat
    schedule: ScheduleConfig
    recipients: List[Recipient]
    filters: Optional[ReportFilters]
    include_charts: bool
    include_summary: bool
    is_active: bool
    custom_branding: Optional[Dict[str, str]]
    created_at: datetime
    updated_at: datetime
    last_sent_at: Optional[datetime]
    next_run_at: Optional[datetime]
    send_count: int
    created_by: str


class ReportTemplate(BaseModel):
    """Template de relatorio pre-definido."""
    id: str
    name: str
    description: str
    report_type: ReportType
    default_format: ReportFormat
    default_schedule: ScheduleConfig
    required_filters: List[str]
    preview_image: Optional[str]
    category: str


class SendNowRequest(BaseModel):
    """Request para enviar relatorio imediatamente."""
    additional_recipients: Optional[List[Recipient]] = None
    override_format: Optional[ReportFormat] = None


class SendNowResponse(BaseModel):
    """Resposta do envio imediato."""
    success: bool
    message: str
    sent_to: List[str]
    report_url: Optional[str]


# =============================================================================
# IN-MEMORY STORAGE (substituir por banco de dados em producao)
# =============================================================================

scheduled_reports_db: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def calculate_next_run(schedule: ScheduleConfig, from_time: datetime = None) -> datetime:
    """Calcula a proxima execucao do relatorio baseado no agendamento."""
    now = from_time or datetime.utcnow()
    hour, minute = map(int, schedule.time.split(':'))

    if schedule.frequency == ReportFrequency.DAILY:
        next_run = now.replace(hour=hour, minute=minute, second=0, microsecond=0)
        if next_run <= now:
            next_run += timedelta(days=1)

    elif schedule.frequency == ReportFrequency.WEEKLY:
        days = {
            DayOfWeek.MONDAY: 0, DayOfWeek.TUESDAY: 1, DayOfWeek.WEDNESDAY: 2,
            DayOfWeek.THURSDAY: 3, DayOfWeek.FRIDAY: 4, DayOfWeek.SATURDAY: 5,
            DayOfWeek.SUNDAY: 6
        }
        target_day = days.get(schedule.day_of_week, 0)
        current_day = now.weekday()
        days_ahead = target_day - current_day
        if days_ahead <= 0:
            days_ahead += 7
        next_run = (now + timedelta(days=days_ahead)).replace(
            hour=hour, minute=minute, second=0, microsecond=0
        )

    elif schedule.frequency == ReportFrequency.BIWEEKLY:
        target_day = schedule.day_of_week or DayOfWeek.MONDAY
        days = {
            DayOfWeek.MONDAY: 0, DayOfWeek.TUESDAY: 1, DayOfWeek.WEDNESDAY: 2,
            DayOfWeek.THURSDAY: 3, DayOfWeek.FRIDAY: 4, DayOfWeek.SATURDAY: 5,
            DayOfWeek.SUNDAY: 6
        }
        target_day_num = days.get(target_day, 0)
        current_day = now.weekday()
        days_ahead = target_day_num - current_day
        if days_ahead <= 0:
            days_ahead += 14
        next_run = (now + timedelta(days=days_ahead)).replace(
            hour=hour, minute=minute, second=0, microsecond=0
        )

    elif schedule.frequency == ReportFrequency.MONTHLY:
        target_day = schedule.day_of_month or 1
        next_run = now.replace(day=target_day, hour=hour, minute=minute, second=0, microsecond=0)
        if next_run <= now:
            if now.month == 12:
                next_run = next_run.replace(year=now.year + 1, month=1)
            else:
                next_run = next_run.replace(month=now.month + 1)

    elif schedule.frequency == ReportFrequency.QUARTERLY:
        target_day = schedule.day_of_month or 1
        current_quarter = (now.month - 1) // 3
        next_quarter_start = (current_quarter + 1) * 3 + 1
        if next_quarter_start > 12:
            next_run = datetime(now.year + 1, 1, target_day, hour, minute)
        else:
            next_run = datetime(now.year, next_quarter_start, target_day, hour, minute)

    else:
        next_run = now + timedelta(days=1)

    return next_run


def generate_report_content(report_type: ReportType, filters: ReportFilters = None) -> Dict[str, Any]:
    """Gera o conteudo do relatorio baseado no tipo."""
    db = SessionLocal()
    try:
        content = {"generated_at": datetime.utcnow().isoformat(), "type": report_type.value}

        # Aplicar filtros
        stories_query = db.query(Story)
        if filters:
            if filters.project_id:
                stories_query = stories_query.filter(Story.project_id == filters.project_id)
            if filters.sprint_id:
                stories_query = stories_query.filter(Story.sprint_id == filters.sprint_id)
            if filters.epic_id:
                stories_query = stories_query.filter(Story.epic_id == filters.epic_id)

        stories = stories_query.all()

        if report_type == ReportType.SPRINT_SUMMARY:
            content.update(_generate_sprint_summary(stories, filters))
        elif report_type == ReportType.VELOCITY:
            content.update(_generate_velocity_report(stories, db))
        elif report_type == ReportType.BURNDOWN:
            content.update(_generate_burndown_report(stories, filters))
        elif report_type == ReportType.STATUS:
            content.update(_generate_status_report(stories))
        elif report_type == ReportType.PRODUCTIVITY:
            content.update(_generate_productivity_report(stories, db))
        elif report_type == ReportType.TEAM_PERFORMANCE:
            content.update(_generate_team_performance_report(stories))
        elif report_type == ReportType.PROJECT_OVERVIEW:
            content.update(_generate_project_overview(stories, db, filters))
        elif report_type == ReportType.EPIC_PROGRESS:
            content.update(_generate_epic_progress(stories, db, filters))

        return content
    finally:
        db.close()


def _generate_sprint_summary(stories: List, filters: ReportFilters = None) -> Dict:
    """Gera resumo do sprint."""
    total = len(stories)
    by_status = {}
    total_points = 0
    completed_points = 0

    for story in stories:
        status = story.status.value if hasattr(story.status, 'value') else str(story.status)
        by_status[status] = by_status.get(status, 0) + 1
        points = story.story_points or 0
        total_points += points
        if status == 'done':
            completed_points += points

    return {
        "total_stories": total,
        "by_status": by_status,
        "total_points": total_points,
        "completed_points": completed_points,
        "completion_rate": round((completed_points / total_points * 100) if total_points > 0 else 0, 1)
    }


def _generate_velocity_report(stories: List, db) -> Dict:
    """Gera relatorio de velocidade."""
    sprints = db.query(Sprint).order_by(Sprint.created_at.desc()).limit(6).all()
    velocity_data = []

    for sprint in sprints:
        sprint_stories = [s for s in stories if s.sprint_id == sprint.sprint_id]
        completed = [s for s in sprint_stories if (s.status.value if hasattr(s.status, 'value') else s.status) == 'done']
        velocity_data.append({
            "sprint_name": sprint.name,
            "planned_points": sum(s.story_points or 0 for s in sprint_stories),
            "completed_points": sum(s.story_points or 0 for s in completed)
        })

    avg_velocity = sum(v["completed_points"] for v in velocity_data) / len(velocity_data) if velocity_data else 0

    return {
        "velocity_by_sprint": velocity_data,
        "average_velocity": round(avg_velocity, 1),
        "trend": "up" if len(velocity_data) >= 2 and velocity_data[0]["completed_points"] > velocity_data[-1]["completed_points"] else "down"
    }


def _generate_burndown_report(stories: List, filters: ReportFilters = None) -> Dict:
    """Gera relatorio de burndown."""
    total_points = sum(s.story_points or 0 for s in stories)
    completed_points = sum(s.story_points or 0 for s in stories if (s.status.value if hasattr(s.status, 'value') else s.status) == 'done')
    remaining = total_points - completed_points

    return {
        "total_points": total_points,
        "completed_points": completed_points,
        "remaining_points": remaining,
        "burn_rate": round((completed_points / total_points * 100) if total_points > 0 else 0, 1)
    }


def _generate_status_report(stories: List) -> Dict:
    """Gera relatorio de status."""
    by_status = {}
    by_priority = {}
    blocked = []

    for story in stories:
        status = story.status.value if hasattr(story.status, 'value') else str(story.status)
        priority = story.priority.value if hasattr(story.priority, 'value') else str(story.priority) if story.priority else 'medium'

        by_status[status] = by_status.get(status, 0) + 1
        by_priority[priority] = by_priority.get(priority, 0) + 1

        if status == 'blocked':
            blocked.append({"story_id": story.story_id, "title": story.title})

    return {
        "by_status": by_status,
        "by_priority": by_priority,
        "blocked_stories": blocked,
        "total": len(stories)
    }


def _generate_productivity_report(stories: List, db) -> Dict:
    """Gera relatorio de produtividade."""
    tasks = db.query(StoryTask).all()
    completed_tasks = [t for t in tasks if (t.status.value if hasattr(t.status, 'value') else t.status) == 'completed']

    return {
        "total_stories": len(stories),
        "total_tasks": len(tasks),
        "completed_tasks": len(completed_tasks),
        "task_completion_rate": round((len(completed_tasks) / len(tasks) * 100) if tasks else 0, 1)
    }


def _generate_team_performance_report(stories: List) -> Dict:
    """Gera relatorio de performance do time."""
    by_assignee = {}

    for story in stories:
        assignee = story.assignee or "Nao Atribuido"
        if assignee not in by_assignee:
            by_assignee[assignee] = {"total": 0, "completed": 0, "points": 0}

        by_assignee[assignee]["total"] += 1
        by_assignee[assignee]["points"] += story.story_points or 0

        status = story.status.value if hasattr(story.status, 'value') else str(story.status)
        if status == 'done':
            by_assignee[assignee]["completed"] += 1

    return {"by_assignee": by_assignee}


def _generate_project_overview(stories: List, db, filters: ReportFilters = None) -> Dict:
    """Gera visao geral do projeto."""
    epics = db.query(Epic).all()
    sprints = db.query(Sprint).all()

    return {
        "total_stories": len(stories),
        "total_epics": len(epics),
        "total_sprints": len(sprints),
        "stories_by_status": _generate_status_report(stories)["by_status"],
        "total_points": sum(s.story_points or 0 for s in stories)
    }


def _generate_epic_progress(stories: List, db, filters: ReportFilters = None) -> Dict:
    """Gera progresso por epico."""
    epics = db.query(Epic).all()
    epic_progress = []

    for epic in epics:
        epic_stories = [s for s in stories if s.epic_id == epic.epic_id]
        total = len(epic_stories)
        completed = len([s for s in epic_stories if (s.status.value if hasattr(s.status, 'value') else s.status) == 'done'])

        epic_progress.append({
            "epic_id": epic.epic_id,
            "title": epic.title,
            "total_stories": total,
            "completed_stories": completed,
            "progress": round((completed / total * 100) if total > 0 else 0, 1)
        })

    return {"epics": epic_progress}


def format_report_as_html(content: Dict, report_name: str, branding: Dict = None) -> str:
    """Formata o relatorio como HTML."""
    brand_color = branding.get('color', '#003B4A') if branding else '#003B4A'
    company_name = branding.get('company', 'Fabrica de Agentes') if branding else 'Fabrica de Agentes'

    html = f'''
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <style>
            body {{ font-family: 'Segoe UI', Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }}
            .container {{ max-width: 800px; margin: 0 auto; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
            .header {{ background: {brand_color}; color: white; padding: 20px 30px; border-radius: 8px 8px 0 0; }}
            .header h1 {{ margin: 0; font-size: 24px; }}
            .header .subtitle {{ opacity: 0.8; font-size: 14px; margin-top: 5px; }}
            .content {{ padding: 30px; }}
            .section {{ margin-bottom: 25px; }}
            .section h2 {{ color: {brand_color}; border-bottom: 2px solid {brand_color}; padding-bottom: 8px; font-size: 18px; }}
            .metric-grid {{ display: flex; flex-wrap: wrap; gap: 15px; margin: 15px 0; }}
            .metric-box {{ flex: 1; min-width: 150px; text-align: center; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid {brand_color}; }}
            .metric-value {{ font-size: 32px; font-weight: bold; color: {brand_color}; }}
            .metric-label {{ font-size: 12px; color: #666; margin-top: 5px; }}
            table {{ width: 100%; border-collapse: collapse; margin: 15px 0; }}
            th {{ background: {brand_color}; color: white; padding: 12px; text-align: left; }}
            td {{ padding: 10px 12px; border-bottom: 1px solid #eee; }}
            tr:hover {{ background: #f8f9fa; }}
            .footer {{ text-align: center; padding: 20px; color: #999; font-size: 12px; border-top: 1px solid #eee; }}
            .status-badge {{ display: inline-block; padding: 4px 12px; border-radius: 20px; font-size: 12px; font-weight: 500; }}
            .status-done {{ background: #D1FAE5; color: #059669; }}
            .status-in_progress {{ background: #FEF3C7; color: #D97706; }}
            .status-blocked {{ background: #FEE2E2; color: #DC2626; }}
        </style>
    </head>
    <body>
        <div class="container">
            <div class="header">
                <h1>{report_name}</h1>
                <div class="subtitle">{company_name} | Gerado em {datetime.now().strftime('%d/%m/%Y %H:%M')}</div>
            </div>
            <div class="content">
    '''

    # Metricas principais
    if 'total_stories' in content:
        html += '''
            <div class="section">
                <h2>Resumo Geral</h2>
                <div class="metric-grid">
        '''
        html += f'''
                    <div class="metric-box">
                        <div class="metric-value">{content.get('total_stories', 0)}</div>
                        <div class="metric-label">Total Stories</div>
                    </div>
        '''
        if 'completed_points' in content:
            html += f'''
                    <div class="metric-box">
                        <div class="metric-value">{content.get('completed_points', 0)}</div>
                        <div class="metric-label">Pontos Completados</div>
                    </div>
            '''
        if 'completion_rate' in content:
            html += f'''
                    <div class="metric-box">
                        <div class="metric-value">{content.get('completion_rate', 0)}%</div>
                        <div class="metric-label">Taxa de Conclusao</div>
                    </div>
            '''
        if 'average_velocity' in content:
            html += f'''
                    <div class="metric-box">
                        <div class="metric-value">{content.get('average_velocity', 0)}</div>
                        <div class="metric-label">Velocidade Media</div>
                    </div>
            '''
        html += '</div></div>'

    # Status distribution
    if 'by_status' in content:
        html += '''
            <div class="section">
                <h2>Distribuicao por Status</h2>
                <table>
                    <tr><th>Status</th><th>Quantidade</th></tr>
        '''
        for status, count in content['by_status'].items():
            html += f'<tr><td><span class="status-badge status-{status}">{status}</span></td><td>{count}</td></tr>'
        html += '</table></div>'

    # Team performance
    if 'by_assignee' in content:
        html += '''
            <div class="section">
                <h2>Performance por Membro</h2>
                <table>
                    <tr><th>Membro</th><th>Total</th><th>Completadas</th><th>Pontos</th></tr>
        '''
        for assignee, data in content['by_assignee'].items():
            html += f'<tr><td>{assignee}</td><td>{data["total"]}</td><td>{data["completed"]}</td><td>{data["points"]}</td></tr>'
        html += '</table></div>'

    # Epics progress
    if 'epics' in content:
        html += '''
            <div class="section">
                <h2>Progresso por Epic</h2>
                <table>
                    <tr><th>Epic</th><th>Stories</th><th>Progresso</th></tr>
        '''
        for epic in content['epics']:
            html += f'<tr><td>{epic["title"]}</td><td>{epic["completed_stories"]}/{epic["total_stories"]}</td><td>{epic["progress"]}%</td></tr>'
        html += '</table></div>'

    html += '''
            </div>
            <div class="footer">
                Relatorio gerado automaticamente pela Fabrica de Agentes<br>
                Este e um email automatico, nao responda.
            </div>
        </div>
    </body>
    </html>
    '''

    return html


async def send_report_email(report: Dict, recipients: List[Recipient], format: ReportFormat) -> bool:
    """Envia o relatorio por email (simulado)."""
    # Em producao, integrar com servico de email (SendGrid, SES, SMTP, etc.)
    print(f"[Scheduled Reports] Enviando relatorio para {len(recipients)} destinatarios")
    for recipient in recipients:
        print(f"  - {recipient.email} ({recipient.name or 'N/A'})")
    return True


# =============================================================================
# REPORT TEMPLATES
# =============================================================================

REPORT_TEMPLATES: List[ReportTemplate] = [
    ReportTemplate(
        id="tpl-sprint-weekly",
        name="Resumo Semanal de Sprint",
        description="Visao geral do progresso do sprint com metricas de velocidade e status",
        report_type=ReportType.SPRINT_SUMMARY,
        default_format=ReportFormat.HTML,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.WEEKLY,
            time="09:00",
            day_of_week=DayOfWeek.MONDAY
        ),
        required_filters=["sprint_id"],
        preview_image="/static/templates/sprint-weekly.png",
        category="Sprint"
    ),
    ReportTemplate(
        id="tpl-velocity",
        name="Relatorio de Velocidade",
        description="Analise de velocidade do time nos ultimos sprints",
        report_type=ReportType.VELOCITY,
        default_format=ReportFormat.PDF,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.BIWEEKLY,
            time="09:00",
            day_of_week=DayOfWeek.FRIDAY
        ),
        required_filters=["project_id"],
        preview_image="/static/templates/velocity.png",
        category="Metricas"
    ),
    ReportTemplate(
        id="tpl-burndown-daily",
        name="Burndown Diario",
        description="Acompanhamento diario do burndown do sprint",
        report_type=ReportType.BURNDOWN,
        default_format=ReportFormat.HTML,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.DAILY,
            time="18:00"
        ),
        required_filters=["sprint_id"],
        preview_image="/static/templates/burndown.png",
        category="Sprint"
    ),
    ReportTemplate(
        id="tpl-status-weekly",
        name="Status Semanal",
        description="Resumo semanal de status de todas as stories",
        report_type=ReportType.STATUS,
        default_format=ReportFormat.HTML,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.WEEKLY,
            time="17:00",
            day_of_week=DayOfWeek.FRIDAY
        ),
        required_filters=[],
        preview_image="/static/templates/status.png",
        category="Geral"
    ),
    ReportTemplate(
        id="tpl-team-monthly",
        name="Performance Mensal do Time",
        description="Analise mensal de produtividade por membro do time",
        report_type=ReportType.TEAM_PERFORMANCE,
        default_format=ReportFormat.PDF,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.MONTHLY,
            time="09:00",
            day_of_month=1
        ),
        required_filters=["project_id"],
        preview_image="/static/templates/team.png",
        category="Time"
    ),
    ReportTemplate(
        id="tpl-project-monthly",
        name="Visao Geral do Projeto",
        description="Resumo executivo mensal do projeto",
        report_type=ReportType.PROJECT_OVERVIEW,
        default_format=ReportFormat.PDF,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.MONTHLY,
            time="09:00",
            day_of_month=1
        ),
        required_filters=["project_id"],
        preview_image="/static/templates/project.png",
        category="Projeto"
    ),
    ReportTemplate(
        id="tpl-epic-biweekly",
        name="Progresso de Epics",
        description="Acompanhamento quinzenal do progresso dos epics",
        report_type=ReportType.EPIC_PROGRESS,
        default_format=ReportFormat.HTML,
        default_schedule=ScheduleConfig(
            frequency=ReportFrequency.BIWEEKLY,
            time="09:00",
            day_of_week=DayOfWeek.MONDAY
        ),
        required_filters=["project_id"],
        preview_image="/static/templates/epic.png",
        category="Epics"
    )
]


# =============================================================================
# API ROUTER
# =============================================================================

router = APIRouter(prefix="/api/reports", tags=["Scheduled Reports"])


@router.get("/scheduled", response_model=List[ScheduledReport])
async def list_scheduled_reports(
    is_active: Optional[bool] = Query(None, description="Filtrar por status ativo/inativo"),
    report_type: Optional[ReportType] = Query(None, description="Filtrar por tipo de relatorio")
):
    """Lista todos os relatorios agendados."""
    reports = list(scheduled_reports_db.values())

    if is_active is not None:
        reports = [r for r in reports if r["is_active"] == is_active]

    if report_type is not None:
        reports = [r for r in reports if r["report_type"] == report_type.value]

    return [ScheduledReport(**r) for r in reports]


@router.post("/scheduled", response_model=ScheduledReport, status_code=201)
async def create_scheduled_report(report: ScheduledReportCreate):
    """Cria um novo relatorio agendado."""
    report_id = f"SR-{uuid.uuid4().hex[:8].upper()}"
    now = datetime.utcnow()

    report_data = {
        "id": report_id,
        "name": report.name,
        "description": report.description,
        "report_type": report.report_type.value,
        "format": report.format.value,
        "schedule": report.schedule.dict(),
        "recipients": [r.dict() for r in report.recipients],
        "filters": report.filters.dict() if report.filters else None,
        "include_charts": report.include_charts,
        "include_summary": report.include_summary,
        "is_active": True,
        "custom_branding": report.custom_branding,
        "created_at": now,
        "updated_at": now,
        "last_sent_at": None,
        "next_run_at": calculate_next_run(report.schedule),
        "send_count": 0,
        "created_by": "system"
    }

    scheduled_reports_db[report_id] = report_data

    return ScheduledReport(**report_data)


@router.get("/scheduled/{report_id}", response_model=ScheduledReport)
async def get_scheduled_report(report_id: str):
    """Busca um relatorio agendado pelo ID."""
    if report_id not in scheduled_reports_db:
        raise HTTPException(status_code=404, detail="Relatorio agendado nao encontrado")

    return ScheduledReport(**scheduled_reports_db[report_id])


@router.put("/scheduled/{report_id}", response_model=ScheduledReport)
async def update_scheduled_report(report_id: str, update: ScheduledReportUpdate):
    """Atualiza um relatorio agendado."""
    if report_id not in scheduled_reports_db:
        raise HTTPException(status_code=404, detail="Relatorio agendado nao encontrado")

    report_data = scheduled_reports_db[report_id]
    update_dict = update.dict(exclude_unset=True)

    for key, value in update_dict.items():
        if value is not None:
            if key == "schedule":
                report_data["schedule"] = value
                report_data["next_run_at"] = calculate_next_run(ScheduleConfig(**value))
            elif key == "recipients":
                report_data["recipients"] = [r if isinstance(r, dict) else r.dict() for r in value]
            elif key == "filters":
                report_data["filters"] = value if isinstance(value, dict) else value.dict()
            elif key in ["report_type", "format"]:
                report_data[key] = value.value if hasattr(value, 'value') else value
            else:
                report_data[key] = value

    report_data["updated_at"] = datetime.utcnow()
    scheduled_reports_db[report_id] = report_data

    return ScheduledReport(**report_data)


@router.delete("/scheduled/{report_id}", status_code=204)
async def delete_scheduled_report(report_id: str):
    """Remove um relatorio agendado."""
    if report_id not in scheduled_reports_db:
        raise HTTPException(status_code=404, detail="Relatorio agendado nao encontrado")

    del scheduled_reports_db[report_id]
    return None


@router.post("/scheduled/{report_id}/send-now", response_model=SendNowResponse)
async def send_report_now(
    report_id: str,
    request: SendNowRequest = None,
    background_tasks: BackgroundTasks = None
):
    """Envia o relatorio imediatamente, fora do agendamento."""
    if report_id not in scheduled_reports_db:
        raise HTTPException(status_code=404, detail="Relatorio agendado nao encontrado")

    report_data = scheduled_reports_db[report_id]

    # Gerar conteudo do relatorio
    filters = ReportFilters(**report_data["filters"]) if report_data["filters"] else ReportFilters()
    content = generate_report_content(ReportType(report_data["report_type"]), filters)

    # Destinatarios
    recipients = [Recipient(**r) for r in report_data["recipients"]]
    if request and request.additional_recipients:
        recipients.extend(request.additional_recipients)

    # Formato
    format_to_use = request.override_format if request and request.override_format else ReportFormat(report_data["format"])

    # Formatar relatorio
    html_content = format_report_as_html(content, report_data["name"], report_data.get("custom_branding"))

    # Enviar (em background se disponivel)
    sent_emails = [r.email for r in recipients]

    if background_tasks:
        background_tasks.add_task(send_report_email, report_data, recipients, format_to_use)
    else:
        await send_report_email(report_data, recipients, format_to_use)

    # Atualizar contadores
    report_data["last_sent_at"] = datetime.utcnow()
    report_data["send_count"] += 1
    scheduled_reports_db[report_id] = report_data

    return SendNowResponse(
        success=True,
        message=f"Relatorio enviado para {len(recipients)} destinatarios",
        sent_to=sent_emails,
        report_url=f"/api/reports/scheduled/{report_id}/preview"
    )


@router.get("/scheduled/{report_id}/preview", response_class=HTMLResponse)
async def preview_scheduled_report(report_id: str):
    """Visualiza uma previa do relatorio."""
    if report_id not in scheduled_reports_db:
        raise HTTPException(status_code=404, detail="Relatorio agendado nao encontrado")

    report_data = scheduled_reports_db[report_id]

    # Gerar conteudo
    filters = ReportFilters(**report_data["filters"]) if report_data["filters"] else ReportFilters()
    content = generate_report_content(ReportType(report_data["report_type"]), filters)

    # Formatar como HTML
    html = format_report_as_html(content, report_data["name"], report_data.get("custom_branding"))

    return HTMLResponse(content=html)


@router.get("/templates", response_model=List[ReportTemplate])
async def list_report_templates(
    category: Optional[str] = Query(None, description="Filtrar por categoria"),
    report_type: Optional[ReportType] = Query(None, description="Filtrar por tipo")
):
    """Lista templates de relatorio disponiveis."""
    templates = REPORT_TEMPLATES.copy()

    if category:
        templates = [t for t in templates if t.category.lower() == category.lower()]

    if report_type:
        templates = [t for t in templates if t.report_type == report_type]

    return templates


@router.get("/templates/{template_id}", response_model=ReportTemplate)
async def get_report_template(template_id: str):
    """Busca um template especifico."""
    template = next((t for t in REPORT_TEMPLATES if t.id == template_id), None)
    if not template:
        raise HTTPException(status_code=404, detail="Template nao encontrado")
    return template


@router.post("/templates/{template_id}/create-schedule", response_model=ScheduledReport)
async def create_from_template(
    template_id: str,
    name: str = Query(..., description="Nome do relatorio"),
    recipients: List[str] = Query(..., description="Lista de emails"),
    project_id: Optional[str] = Query(None),
    sprint_id: Optional[str] = Query(None)
):
    """Cria um relatorio agendado a partir de um template."""
    template = next((t for t in REPORT_TEMPLATES if t.id == template_id), None)
    if not template:
        raise HTTPException(status_code=404, detail="Template nao encontrado")

    # Criar recipients
    recipient_list = [Recipient(email=email) for email in recipients]

    # Criar filtros
    filters = ReportFilters(project_id=project_id, sprint_id=sprint_id)

    # Criar relatorio
    report_create = ScheduledReportCreate(
        name=name,
        description=template.description,
        report_type=template.report_type,
        format=template.default_format,
        schedule=template.default_schedule,
        recipients=recipient_list,
        filters=filters
    )

    return await create_scheduled_report(report_create)


# =============================================================================
# VUE.JS COMPONENT
# =============================================================================

SCHEDULED_REPORTS_VUE_COMPONENT = '''
<!-- Scheduled Reports Component (Issue #257) -->
<div id="scheduled-reports-app">
    <div class="reports-container">
        <!-- Header -->
        <div class="reports-header">
            <h2>
                <i class="fas fa-calendar-alt"></i>
                Relatorios Agendados
            </h2>
            <button class="btn-primary" @click="showCreateModal = true">
                <i class="fas fa-plus"></i> Novo Agendamento
            </button>
        </div>

        <!-- Filtros -->
        <div class="reports-filters">
            <select v-model="filterType">
                <option value="">Todos os Tipos</option>
                <option v-for="type in reportTypes" :value="type.value">{{ type.label }}</option>
            </select>
            <select v-model="filterStatus">
                <option value="">Todos os Status</option>
                <option value="true">Ativos</option>
                <option value="false">Inativos</option>
            </select>
        </div>

        <!-- Lista de Relatorios -->
        <div class="reports-list">
            <div v-for="report in filteredReports" :key="report.id" class="report-card">
                <div class="report-header">
                    <div class="report-info">
                        <h3>{{ report.name }}</h3>
                        <span class="report-type">{{ formatReportType(report.report_type) }}</span>
                    </div>
                    <div class="report-status" :class="report.is_active ? 'active' : 'inactive'">
                        {{ report.is_active ? 'Ativo' : 'Inativo' }}
                    </div>
                </div>

                <div class="report-details">
                    <div class="detail-item">
                        <i class="fas fa-clock"></i>
                        <span>{{ formatSchedule(report.schedule) }}</span>
                    </div>
                    <div class="detail-item">
                        <i class="fas fa-users"></i>
                        <span>{{ report.recipients.length }} destinatario(s)</span>
                    </div>
                    <div class="detail-item">
                        <i class="fas fa-file"></i>
                        <span>{{ report.format.toUpperCase() }}</span>
                    </div>
                    <div class="detail-item" v-if="report.next_run_at">
                        <i class="fas fa-calendar"></i>
                        <span>Proximo: {{ formatDate(report.next_run_at) }}</span>
                    </div>
                </div>

                <div class="report-stats">
                    <div class="stat">
                        <span class="stat-value">{{ report.send_count }}</span>
                        <span class="stat-label">Envios</span>
                    </div>
                    <div class="stat" v-if="report.last_sent_at">
                        <span class="stat-value">{{ formatDate(report.last_sent_at) }}</span>
                        <span class="stat-label">Ultimo Envio</span>
                    </div>
                </div>

                <div class="report-actions">
                    <button class="btn-icon" @click="sendNow(report.id)" title="Enviar Agora">
                        <i class="fas fa-paper-plane"></i>
                    </button>
                    <button class="btn-icon" @click="previewReport(report.id)" title="Visualizar">
                        <i class="fas fa-eye"></i>
                    </button>
                    <button class="btn-icon" @click="editReport(report)" title="Editar">
                        <i class="fas fa-edit"></i>
                    </button>
                    <button class="btn-icon" @click="toggleActive(report)" title="Ativar/Desativar">
                        <i :class="report.is_active ? 'fas fa-pause' : 'fas fa-play'"></i>
                    </button>
                    <button class="btn-icon danger" @click="deleteReport(report.id)" title="Excluir">
                        <i class="fas fa-trash"></i>
                    </button>
                </div>
            </div>

            <div v-if="filteredReports.length === 0" class="empty-state">
                <i class="fas fa-inbox"></i>
                <p>Nenhum relatorio agendado encontrado</p>
                <button class="btn-primary" @click="showTemplates = true">
                    Criar a partir de Template
                </button>
            </div>
        </div>

        <!-- Templates Section -->
        <div class="templates-section" v-if="showTemplates || reports.length === 0">
            <h3>Templates Disponiveis</h3>
            <div class="templates-grid">
                <div v-for="template in templates" :key="template.id" class="template-card" @click="createFromTemplate(template)">
                    <div class="template-icon">
                        <i :class="getTemplateIcon(template.report_type)"></i>
                    </div>
                    <h4>{{ template.name }}</h4>
                    <p>{{ template.description }}</p>
                    <span class="template-category">{{ template.category }}</span>
                </div>
            </div>
        </div>
    </div>

    <!-- Modal Criar/Editar -->
    <div class="modal" v-if="showCreateModal || showEditModal" @click.self="closeModals">
        <div class="modal-content">
            <div class="modal-header">
                <h3>{{ showEditModal ? 'Editar Relatorio' : 'Novo Relatorio Agendado' }}</h3>
                <button class="btn-close" @click="closeModals">&times;</button>
            </div>

            <form @submit.prevent="saveReport" class="report-form">
                <div class="form-group">
                    <label>Nome do Relatorio</label>
                    <input type="text" v-model="form.name" required>
                </div>

                <div class="form-group">
                    <label>Descricao</label>
                    <textarea v-model="form.description" rows="2"></textarea>
                </div>

                <div class="form-row">
                    <div class="form-group">
                        <label>Tipo de Relatorio</label>
                        <select v-model="form.report_type" required>
                            <option v-for="type in reportTypes" :value="type.value">{{ type.label }}</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label>Formato</label>
                        <select v-model="form.format">
                            <option value="html">HTML</option>
                            <option value="pdf">PDF</option>
                            <option value="excel">Excel</option>
                        </select>
                    </div>
                </div>

                <div class="form-section">
                    <h4>Agendamento</h4>
                    <div class="form-row">
                        <div class="form-group">
                            <label>Frequencia</label>
                            <select v-model="form.schedule.frequency" @change="onFrequencyChange">
                                <option value="daily">Diario</option>
                                <option value="weekly">Semanal</option>
                                <option value="biweekly">Quinzenal</option>
                                <option value="monthly">Mensal</option>
                                <option value="quarterly">Trimestral</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Horario</label>
                            <input type="time" v-model="form.schedule.time">
                        </div>
                    </div>

                    <div class="form-group" v-if="form.schedule.frequency === 'weekly' || form.schedule.frequency === 'biweekly'">
                        <label>Dia da Semana</label>
                        <select v-model="form.schedule.day_of_week">
                            <option value="monday">Segunda-feira</option>
                            <option value="tuesday">Terca-feira</option>
                            <option value="wednesday">Quarta-feira</option>
                            <option value="thursday">Quinta-feira</option>
                            <option value="friday">Sexta-feira</option>
                            <option value="saturday">Sabado</option>
                            <option value="sunday">Domingo</option>
                        </select>
                    </div>

                    <div class="form-group" v-if="form.schedule.frequency === 'monthly' || form.schedule.frequency === 'quarterly'">
                        <label>Dia do Mes (1-28)</label>
                        <input type="number" v-model.number="form.schedule.day_of_month" min="1" max="28">
                    </div>
                </div>

                <div class="form-section">
                    <h4>Destinatarios</h4>
                    <div class="recipients-list">
                        <div v-for="(recipient, index) in form.recipients" :key="index" class="recipient-row">
                            <input type="email" v-model="recipient.email" placeholder="Email" required>
                            <input type="text" v-model="recipient.name" placeholder="Nome (opcional)">
                            <select v-model="recipient.role">
                                <option value="">Funcao</option>
                                <option value="manager">Gerente</option>
                                <option value="developer">Desenvolvedor</option>
                                <option value="stakeholder">Stakeholder</option>
                            </select>
                            <button type="button" class="btn-icon danger" @click="removeRecipient(index)">
                                <i class="fas fa-times"></i>
                            </button>
                        </div>
                    </div>
                    <button type="button" class="btn-secondary" @click="addRecipient">
                        <i class="fas fa-plus"></i> Adicionar Destinatario
                    </button>
                </div>

                <div class="form-section">
                    <h4>Filtros</h4>
                    <div class="form-row">
                        <div class="form-group">
                            <label>Projeto</label>
                            <select v-model="form.filters.project_id">
                                <option value="">Todos</option>
                                <option v-for="project in projects" :value="project.id">{{ project.name }}</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Sprint</label>
                            <select v-model="form.filters.sprint_id">
                                <option value="">Todos</option>
                                <option v-for="sprint in sprints" :value="sprint.id">{{ sprint.name }}</option>
                            </select>
                        </div>
                    </div>
                </div>

                <div class="form-actions">
                    <button type="button" class="btn-secondary" @click="closeModals">Cancelar</button>
                    <button type="submit" class="btn-primary">
                        {{ showEditModal ? 'Salvar Alteracoes' : 'Criar Agendamento' }}
                    </button>
                </div>
            </form>
        </div>
    </div>
</div>

<style>
.reports-container {
    padding: 20px;
    max-width: 1200px;
    margin: 0 auto;
}

.reports-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 20px;
}

.reports-header h2 {
    color: #003B4A;
    display: flex;
    align-items: center;
    gap: 10px;
}

.reports-filters {
    display: flex;
    gap: 15px;
    margin-bottom: 20px;
}

.reports-filters select {
    padding: 8px 12px;
    border: 1px solid #ddd;
    border-radius: 6px;
    min-width: 150px;
}

.reports-list {
    display: grid;
    gap: 15px;
}

.report-card {
    background: white;
    border-radius: 8px;
    padding: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    transition: box-shadow 0.2s;
}

.report-card:hover {
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
}

.report-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 15px;
}

.report-info h3 {
    margin: 0 0 5px 0;
    color: #003B4A;
}

.report-type {
    font-size: 12px;
    color: #666;
    background: #f0f0f0;
    padding: 2px 8px;
    border-radius: 4px;
}

.report-status {
    padding: 4px 12px;
    border-radius: 20px;
    font-size: 12px;
    font-weight: 500;
}

.report-status.active {
    background: #D1FAE5;
    color: #059669;
}

.report-status.inactive {
    background: #FEE2E2;
    color: #DC2626;
}

.report-details {
    display: flex;
    flex-wrap: wrap;
    gap: 15px;
    margin-bottom: 15px;
    padding-bottom: 15px;
    border-bottom: 1px solid #eee;
}

.detail-item {
    display: flex;
    align-items: center;
    gap: 6px;
    color: #666;
    font-size: 13px;
}

.detail-item i {
    color: #003B4A;
}

.report-stats {
    display: flex;
    gap: 30px;
    margin-bottom: 15px;
}

.stat {
    display: flex;
    flex-direction: column;
}

.stat-value {
    font-size: 18px;
    font-weight: 600;
    color: #003B4A;
}

.stat-label {
    font-size: 11px;
    color: #999;
}

.report-actions {
    display: flex;
    gap: 8px;
    justify-content: flex-end;
}

.btn-icon {
    width: 36px;
    height: 36px;
    border: none;
    border-radius: 6px;
    background: #f5f5f5;
    color: #666;
    cursor: pointer;
    transition: all 0.2s;
}

.btn-icon:hover {
    background: #003B4A;
    color: white;
}

.btn-icon.danger:hover {
    background: #DC2626;
}

.btn-primary {
    background: #FF6C00;
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 6px;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 8px;
    font-weight: 500;
}

.btn-primary:hover {
    background: #e56200;
}

.btn-secondary {
    background: #f5f5f5;
    color: #333;
    border: 1px solid #ddd;
    padding: 10px 20px;
    border-radius: 6px;
    cursor: pointer;
}

.templates-section {
    margin-top: 30px;
    padding-top: 20px;
    border-top: 1px solid #eee;
}

.templates-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
    gap: 15px;
    margin-top: 15px;
}

.template-card {
    background: white;
    border: 2px solid #eee;
    border-radius: 8px;
    padding: 20px;
    cursor: pointer;
    transition: all 0.2s;
}

.template-card:hover {
    border-color: #FF6C00;
    transform: translateY(-2px);
}

.template-icon {
    width: 50px;
    height: 50px;
    background: #003B4A;
    border-radius: 10px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 20px;
    margin-bottom: 15px;
}

.template-card h4 {
    margin: 0 0 8px 0;
    color: #003B4A;
}

.template-card p {
    margin: 0 0 10px 0;
    color: #666;
    font-size: 13px;
}

.template-category {
    font-size: 11px;
    background: #f0f0f0;
    padding: 2px 8px;
    border-radius: 4px;
    color: #666;
}

.modal {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0,0,0,0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
}

.modal-content {
    background: white;
    border-radius: 12px;
    width: 90%;
    max-width: 700px;
    max-height: 90vh;
    overflow-y: auto;
}

.modal-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 20px;
    border-bottom: 1px solid #eee;
}

.modal-header h3 {
    margin: 0;
    color: #003B4A;
}

.btn-close {
    background: none;
    border: none;
    font-size: 24px;
    cursor: pointer;
    color: #999;
}

.report-form {
    padding: 20px;
}

.form-group {
    margin-bottom: 15px;
}

.form-group label {
    display: block;
    margin-bottom: 5px;
    font-weight: 500;
    color: #333;
}

.form-group input,
.form-group select,
.form-group textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 6px;
    font-size: 14px;
}

.form-row {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 15px;
}

.form-section {
    margin-top: 20px;
    padding-top: 20px;
    border-top: 1px solid #eee;
}

.form-section h4 {
    margin: 0 0 15px 0;
    color: #003B4A;
}

.recipients-list {
    margin-bottom: 10px;
}

.recipient-row {
    display: grid;
    grid-template-columns: 1fr 1fr 120px 40px;
    gap: 10px;
    margin-bottom: 10px;
}

.recipient-row input,
.recipient-row select {
    padding: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
}

.form-actions {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
    margin-top: 20px;
    padding-top: 20px;
    border-top: 1px solid #eee;
}

.empty-state {
    text-align: center;
    padding: 60px 20px;
    color: #999;
}

.empty-state i {
    font-size: 48px;
    margin-bottom: 15px;
}
</style>

<script>
const ScheduledReportsApp = {
    data() {
        return {
            reports: [],
            templates: [],
            projects: [],
            sprints: [],
            filterType: '',
            filterStatus: '',
            showCreateModal: false,
            showEditModal: false,
            showTemplates: false,
            editingId: null,
            form: this.getEmptyForm(),
            reportTypes: [
                { value: 'sprint_summary', label: 'Resumo de Sprint' },
                { value: 'velocity', label: 'Velocidade' },
                { value: 'burndown', label: 'Burndown' },
                { value: 'status', label: 'Status' },
                { value: 'productivity', label: 'Produtividade' },
                { value: 'team_performance', label: 'Performance do Time' },
                { value: 'project_overview', label: 'Visao do Projeto' },
                { value: 'epic_progress', label: 'Progresso de Epics' }
            ]
        };
    },
    computed: {
        filteredReports() {
            let result = this.reports;
            if (this.filterType) {
                result = result.filter(r => r.report_type === this.filterType);
            }
            if (this.filterStatus !== '') {
                const isActive = this.filterStatus === 'true';
                result = result.filter(r => r.is_active === isActive);
            }
            return result;
        }
    },
    methods: {
        getEmptyForm() {
            return {
                name: '',
                description: '',
                report_type: 'sprint_summary',
                format: 'html',
                schedule: {
                    frequency: 'weekly',
                    time: '09:00',
                    day_of_week: 'monday',
                    day_of_month: 1
                },
                recipients: [{ email: '', name: '', role: '' }],
                filters: {
                    project_id: '',
                    sprint_id: ''
                }
            };
        },
        async loadReports() {
            const response = await fetch('/api/reports/scheduled');
            this.reports = await response.json();
        },
        async loadTemplates() {
            const response = await fetch('/api/reports/templates');
            this.templates = await response.json();
        },
        formatReportType(type) {
            const found = this.reportTypes.find(t => t.value === type);
            return found ? found.label : type;
        },
        formatSchedule(schedule) {
            const freq = {
                daily: 'Diariamente',
                weekly: 'Semanalmente',
                biweekly: 'Quinzenalmente',
                monthly: 'Mensalmente',
                quarterly: 'Trimestralmente'
            };
            return `${freq[schedule.frequency] || schedule.frequency} as ${schedule.time}`;
        },
        formatDate(dateStr) {
            if (!dateStr) return '-';
            return new Date(dateStr).toLocaleDateString('pt-BR', {
                day: '2-digit',
                month: '2-digit',
                year: 'numeric',
                hour: '2-digit',
                minute: '2-digit'
            });
        },
        getTemplateIcon(type) {
            const icons = {
                sprint_summary: 'fas fa-running',
                velocity: 'fas fa-tachometer-alt',
                burndown: 'fas fa-chart-line',
                status: 'fas fa-tasks',
                productivity: 'fas fa-chart-bar',
                team_performance: 'fas fa-users',
                project_overview: 'fas fa-project-diagram',
                epic_progress: 'fas fa-mountain'
            };
            return icons[type] || 'fas fa-file-alt';
        },
        addRecipient() {
            this.form.recipients.push({ email: '', name: '', role: '' });
        },
        removeRecipient(index) {
            this.form.recipients.splice(index, 1);
        },
        onFrequencyChange() {
            // Reset fields based on frequency
        },
        editReport(report) {
            this.editingId = report.id;
            this.form = JSON.parse(JSON.stringify(report));
            this.showEditModal = true;
        },
        async saveReport() {
            const url = this.showEditModal
                ? `/api/reports/scheduled/${this.editingId}`
                : '/api/reports/scheduled';
            const method = this.showEditModal ? 'PUT' : 'POST';

            await fetch(url, {
                method,
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(this.form)
            });

            this.closeModals();
            this.loadReports();
        },
        async sendNow(reportId) {
            await fetch(`/api/reports/scheduled/${reportId}/send-now`, { method: 'POST' });
            alert('Relatorio enviado com sucesso!');
            this.loadReports();
        },
        previewReport(reportId) {
            window.open(`/api/reports/scheduled/${reportId}/preview`, '_blank');
        },
        async toggleActive(report) {
            await fetch(`/api/reports/scheduled/${report.id}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ is_active: !report.is_active })
            });
            this.loadReports();
        },
        async deleteReport(reportId) {
            if (confirm('Tem certeza que deseja excluir este agendamento?')) {
                await fetch(`/api/reports/scheduled/${reportId}`, { method: 'DELETE' });
                this.loadReports();
            }
        },
        createFromTemplate(template) {
            this.form = this.getEmptyForm();
            this.form.name = template.name;
            this.form.description = template.description;
            this.form.report_type = template.report_type;
            this.form.format = template.default_format;
            this.form.schedule = { ...template.default_schedule };
            this.showCreateModal = true;
        },
        closeModals() {
            this.showCreateModal = false;
            this.showEditModal = false;
            this.editingId = null;
            this.form = this.getEmptyForm();
        }
    },
    mounted() {
        this.loadReports();
        this.loadTemplates();
    }
};

// Mount if Vue is available
if (typeof Vue !== 'undefined') {
    Vue.createApp(ScheduledReportsApp).mount('#scheduled-reports-app');
}
</script>
'''


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_scheduled_reports(app):
    """Registra os endpoints de relatorios agendados no app FastAPI."""
    app.include_router(router)

    @app.get("/scheduled-reports", response_class=HTMLResponse)
    async def scheduled_reports_page():
        """Pagina de relatorios agendados."""
        return f'''
        <!DOCTYPE html>
        <html lang="pt-BR">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Relatorios Agendados - Fabrica de Agentes</title>
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
            <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
            <style>
                * {{ box-sizing: border-box; margin: 0; padding: 0; }}
                body {{ font-family: 'Segoe UI', Arial, sans-serif; background: #f5f5f5; }}
            </style>
        </head>
        <body>
            {SCHEDULED_REPORTS_VUE_COMPONENT}
        </body>
        </html>
        '''

    print("[Dashboard] Scheduled Reports loaded: /api/reports/scheduled, /scheduled-reports")
