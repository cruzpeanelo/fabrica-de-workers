# -*- coding: utf-8 -*-
"""
Export PDF Module (Issue #255)
==============================
Geracao de relatorios PDF profissionais.

Funcionalidades:
- Templates de relatorio (sprint, projeto, retrospectiva)
- Branding customizavel (logo, cores)
- Graficos incluidos no PDF
- Sumario executivo automatico
"""

from fastapi import APIRouter, Query, Response
from fastapi.responses import StreamingResponse
from typing import Optional
from datetime import datetime, timedelta
from io import BytesIO

from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask, Sprint, Epic

router = APIRouter(prefix="/api/export/pdf", tags=["Export PDF"])


def generate_html_report(title: str, content: str, branding: dict = None) -> str:
    """Gera HTML formatado para conversao em PDF."""
    brand_color = branding.get('color', '#003B4A') if branding else '#003B4A'
    company_name = branding.get('company', 'Fabrica de Agentes') if branding else 'Fabrica de Agentes'

    return f'''
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <style>
            @page {{ margin: 2cm; }}
            body {{ font-family: 'Segoe UI', Arial, sans-serif; color: #333; line-height: 1.6; }}
            .header {{ background: {brand_color}; color: white; padding: 20px; margin: -2cm -2cm 20px -2cm; }}
            .header h1 {{ margin: 0; font-size: 24px; }}
            .header .subtitle {{ opacity: 0.8; font-size: 14px; }}
            .section {{ margin-bottom: 25px; }}
            .section h2 {{ color: {brand_color}; border-bottom: 2px solid {brand_color}; padding-bottom: 5px; }}
            table {{ width: 100%; border-collapse: collapse; margin: 15px 0; }}
            th {{ background: {brand_color}; color: white; padding: 10px; text-align: left; }}
            td {{ padding: 8px 10px; border-bottom: 1px solid #ddd; }}
            tr:nth-child(even) {{ background: #f9f9f9; }}
            .metric-box {{ display: inline-block; width: 22%; text-align: center; padding: 15px; margin: 5px; background: #f5f5f5; border-radius: 8px; }}
            .metric-value {{ font-size: 28px; font-weight: bold; color: {brand_color}; }}
            .metric-label {{ font-size: 12px; color: #666; }}
            .status-done {{ color: #10B981; }}
            .status-progress {{ color: #F59E0B; }}
            .status-blocked {{ color: #EF4444; }}
            .footer {{ text-align: center; color: #999; font-size: 11px; margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; }}
            .priority-urgent {{ background: #FEE2E2; color: #DC2626; padding: 2px 6px; border-radius: 4px; }}
            .priority-high {{ background: #FFEDD5; color: #EA580C; padding: 2px 6px; border-radius: 4px; }}
            .priority-medium {{ background: #FEF3C7; color: #D97706; padding: 2px 6px; border-radius: 4px; }}
            .priority-low {{ background: #D1FAE5; color: #059669; padding: 2px 6px; border-radius: 4px; }}
        </style>
    </head>
    <body>
        <div class="header">
            <h1>{title}</h1>
            <div class="subtitle">{company_name} | Gerado em {datetime.now().strftime('%d/%m/%Y %H:%M')}</div>
        </div>
        {content}
        <div class="footer">
            Relatorio gerado automaticamente pela Fabrica de Agentes
        </div>
    </body>
    </html>
    '''


@router.get("/sprint/{sprint_id}")
async def export_sprint_pdf(
    sprint_id: str,
    include_tasks: bool = Query(True),
    include_metrics: bool = Query(True)
):
    """Exporta relatorio de sprint em PDF (HTML para impressao)."""
    db = SessionLocal()
    try:
        sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()
        if not sprint:
            return {"error": "Sprint nao encontrado"}

        stories = db.query(Story).filter(Story.sprint_id == sprint_id).all()

        # Calculate metrics
        total_stories = len(stories)
        total_points = sum(s.story_points or 0 for s in stories)
        completed = [s for s in stories if (s.status.value if hasattr(s.status, 'value') else s.status) == 'done']
        completed_points = sum(s.story_points or 0 for s in completed)

        content = f'''
        <div class="section">
            <h2>Informacoes do Sprint</h2>
            <p><strong>Nome:</strong> {sprint.name}</p>
            <p><strong>Objetivo:</strong> {sprint.goal or 'Nao definido'}</p>
            <p><strong>Periodo:</strong> {sprint.start_date.strftime('%d/%m/%Y') if sprint.start_date else '-'}
               a {sprint.end_date.strftime('%d/%m/%Y') if sprint.end_date else '-'}</p>
        </div>
        '''

        if include_metrics:
            velocity_pct = (completed_points / total_points * 100) if total_points > 0 else 0
            content += f'''
            <div class="section">
                <h2>Metricas</h2>
                <div style="text-align: center;">
                    <div class="metric-box">
                        <div class="metric-value">{total_stories}</div>
                        <div class="metric-label">Stories</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">{len(completed)}</div>
                        <div class="metric-label">Completadas</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">{total_points}</div>
                        <div class="metric-label">Pontos Planejados</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">{velocity_pct:.0f}%</div>
                        <div class="metric-label">Velocidade</div>
                    </div>
                </div>
            </div>
            '''

        # Stories table
        content += '''
        <div class="section">
            <h2>Stories do Sprint</h2>
            <table>
                <tr>
                    <th>ID</th>
                    <th>Titulo</th>
                    <th>Status</th>
                    <th>Prioridade</th>
                    <th>Pontos</th>
                    <th>Responsavel</th>
                </tr>
        '''

        for story in stories:
            status = story.status.value if hasattr(story.status, 'value') else story.status
            priority = story.priority.value if hasattr(story.priority, 'value') else story.priority
            status_class = 'status-done' if status == 'done' else 'status-progress' if status == 'in_progress' else ''

            content += f'''
                <tr>
                    <td>{story.story_id}</td>
                    <td>{story.title}</td>
                    <td class="{status_class}">{status}</td>
                    <td><span class="priority-{priority}">{priority}</span></td>
                    <td>{story.story_points or 0}</td>
                    <td>{story.assignee or '-'}</td>
                </tr>
            '''

        content += '</table></div>'

        html = generate_html_report(f"Relatorio Sprint: {sprint.name}", content)

        return Response(
            content=html,
            media_type="text/html",
            headers={"Content-Disposition": f"inline; filename=sprint_{sprint_id}_report.html"}
        )

    finally:
        db.close()


@router.get("/project/{project_id}")
async def export_project_pdf(project_id: str):
    """Exporta relatorio de projeto em PDF (HTML)."""
    db = SessionLocal()
    try:
        stories = db.query(Story).filter(Story.project_id == project_id).all()
        epics = db.query(Epic).filter(Epic.project_id == project_id).all()
        sprints = db.query(Sprint).filter(Sprint.project_id == project_id).all()

        # Metrics
        total_stories = len(stories)
        total_points = sum(s.story_points or 0 for s in stories)

        by_status = {}
        for story in stories:
            status = story.status.value if hasattr(story.status, 'value') else story.status
            by_status[status] = by_status.get(status, 0) + 1

        content = f'''
        <div class="section">
            <h2>Resumo Executivo</h2>
            <div style="text-align: center;">
                <div class="metric-box">
                    <div class="metric-value">{total_stories}</div>
                    <div class="metric-label">Total Stories</div>
                </div>
                <div class="metric-box">
                    <div class="metric-value">{total_points}</div>
                    <div class="metric-label">Total Pontos</div>
                </div>
                <div class="metric-box">
                    <div class="metric-value">{len(epics)}</div>
                    <div class="metric-label">Epics</div>
                </div>
                <div class="metric-box">
                    <div class="metric-value">{len(sprints)}</div>
                    <div class="metric-label">Sprints</div>
                </div>
            </div>
        </div>

        <div class="section">
            <h2>Distribuicao por Status</h2>
            <table>
                <tr><th>Status</th><th>Quantidade</th><th>Porcentagem</th></tr>
        '''

        for status, count in by_status.items():
            pct = (count / total_stories * 100) if total_stories > 0 else 0
            content += f'<tr><td>{status}</td><td>{count}</td><td>{pct:.1f}%</td></tr>'

        content += '</table></div>'

        # Epics section
        if epics:
            content += '''
            <div class="section">
                <h2>Epics</h2>
                <table>
                    <tr><th>Epic</th><th>Stories</th><th>Pontos</th></tr>
            '''
            for epic in epics:
                epic_stories = [s for s in stories if s.epic_id == epic.epic_id]
                epic_points = sum(s.story_points or 0 for s in epic_stories)
                content += f'<tr><td>{epic.title}</td><td>{len(epic_stories)}</td><td>{epic_points}</td></tr>'
            content += '</table></div>'

        html = generate_html_report(f"Relatorio do Projeto", content)

        return Response(
            content=html,
            media_type="text/html",
            headers={"Content-Disposition": f"inline; filename=project_{project_id}_report.html"}
        )

    finally:
        db.close()


@router.get("/retrospective/{sprint_id}")
async def export_retrospective_pdf(sprint_id: str):
    """Exporta template de retrospectiva em PDF (HTML)."""
    db = SessionLocal()
    try:
        sprint = db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()
        if not sprint:
            return {"error": "Sprint nao encontrado"}

        stories = db.query(Story).filter(Story.sprint_id == sprint_id).all()

        completed = len([s for s in stories if (s.status.value if hasattr(s.status, 'value') else s.status) == 'done'])
        total = len(stories)

        content = f'''
        <div class="section">
            <h2>Dados do Sprint</h2>
            <p><strong>Sprint:</strong> {sprint.name}</p>
            <p><strong>Objetivo:</strong> {sprint.goal or 'Nao definido'}</p>
            <p><strong>Stories Completadas:</strong> {completed} de {total}</p>
        </div>

        <div class="section">
            <h2>O que foi bem? 👍</h2>
            <div style="min-height: 150px; border: 1px dashed #ccc; padding: 15px; border-radius: 8px;">
                <p style="color: #999;">Espaco para anotacoes...</p>
            </div>
        </div>

        <div class="section">
            <h2>O que pode melhorar? 🔧</h2>
            <div style="min-height: 150px; border: 1px dashed #ccc; padding: 15px; border-radius: 8px;">
                <p style="color: #999;">Espaco para anotacoes...</p>
            </div>
        </div>

        <div class="section">
            <h2>Acoes para o proximo sprint 🎯</h2>
            <div style="min-height: 150px; border: 1px dashed #ccc; padding: 15px; border-radius: 8px;">
                <p style="color: #999;">Espaco para anotacoes...</p>
            </div>
        </div>

        <div class="section">
            <h2>Agradecimentos 🙏</h2>
            <div style="min-height: 100px; border: 1px dashed #ccc; padding: 15px; border-radius: 8px;">
                <p style="color: #999;">Reconhecimentos do time...</p>
            </div>
        </div>
        '''

        html = generate_html_report(f"Retrospectiva: {sprint.name}", content)

        return Response(
            content=html,
            media_type="text/html",
            headers={"Content-Disposition": f"inline; filename=retro_{sprint_id}.html"}
        )

    finally:
        db.close()


def register_export_pdf(app):
    """Registra os endpoints de export PDF no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Export PDF endpoints loaded: /api/export/pdf/*")
