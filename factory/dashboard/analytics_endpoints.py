# -*- coding: utf-8 -*-
"""
Analytics de Produtividade e Time Insights (Issue #65)
======================================================
Endpoints para metricas de produtividade do time.
"""
from datetime import datetime, timedelta
from typing import Optional
from pydantic import BaseModel
from fastapi import HTTPException


class AnalyticsTimeRange(BaseModel):
    start_date: Optional[str] = None
    end_date: Optional[str] = None
    sprint_id: Optional[str] = None


def register_analytics_endpoints(app, SessionLocal, Story, Sprint, HAS_CLAUDE, get_claude_client):
    """Registra os endpoints de analytics no app FastAPI."""

    @app.get("/api/analytics/productivity")
    def get_productivity_analytics(
        project_id: Optional[str] = None,
        sprint_id: Optional[str] = None,
        days: int = 30
    ):
        """
        Retorna metricas de produtividade do time e desenvolvedores.

        Metricas incluidas:
        - Por desenvolvedor: stories completadas, pontos entregues, tempo medio, taxa de retrabalho
        - Do time: velocity media, predictability score, colaboracao
        """
        db = SessionLocal()
        try:
            end_date = datetime.utcnow()
            start_date = end_date - timedelta(days=days)

            query = db.query(Story)
            if project_id:
                query = query.filter(Story.project_id == project_id)
            if sprint_id:
                query = query.filter(Story.sprint_id == sprint_id)

            all_stories = query.all()
            completed_stories = [s for s in all_stories if s.status == 'done' and s.completed_at and s.completed_at >= start_date]

            # METRICAS POR DESENVOLVEDOR
            developer_metrics = {}
            for story in all_stories:
                assignee = story.assignee or "Nao atribuido"
                if assignee not in developer_metrics:
                    developer_metrics[assignee] = {
                        "assignee": assignee, "stories_total": 0, "stories_completed": 0,
                        "points_total": 0, "points_delivered": 0, "stories_in_progress": 0,
                        "stories_in_review": 0, "completed_times": []
                    }
                dev = developer_metrics[assignee]
                dev["stories_total"] += 1
                dev["points_total"] += story.story_points or 0

                if story.status == 'done':
                    dev["stories_completed"] += 1
                    dev["points_delivered"] += story.story_points or 0
                    if story.started_at and story.completed_at:
                        dev["completed_times"].append((story.completed_at - story.started_at).total_seconds() / 3600)
                elif story.status == 'in_progress':
                    dev["stories_in_progress"] += 1
                elif story.status == 'review':
                    dev["stories_in_review"] += 1

            developer_stats = []
            for assignee, dev in developer_metrics.items():
                avg_time = sum(dev["completed_times"]) / len(dev["completed_times"]) if dev["completed_times"] else 0
                rework_rate = (dev["stories_in_review"] / (dev["stories_completed"] + dev["stories_in_review"])) * 100 if dev["stories_completed"] > 0 else 0
                developer_stats.append({
                    "assignee": assignee,
                    "stories_completed": dev["stories_completed"],
                    "stories_total": dev["stories_total"],
                    "points_delivered": dev["points_delivered"],
                    "points_total": dev["points_total"],
                    "stories_in_progress": dev["stories_in_progress"],
                    "avg_time_hours": round(avg_time, 1),
                    "avg_time_days": round(avg_time / 24, 1) if avg_time > 0 else 0,
                    "rework_rate": round(rework_rate, 1),
                    "completion_rate": round((dev["stories_completed"] / dev["stories_total"]) * 100, 1) if dev["stories_total"] > 0 else 0
                })
            developer_stats.sort(key=lambda x: x["points_delivered"], reverse=True)

            # METRICAS DO TIME
            total_points = sum(s.story_points or 0 for s in all_stories)
            done_points = sum(s.story_points or 0 for s in all_stories if s.status == 'done')

            sprints_data = db.query(Sprint).filter(Sprint.status == 'completed').all()
            velocities = [s.velocity for s in sprints_data if s.velocity and s.velocity > 0]
            avg_velocity = sum(velocities) / len(velocities) if velocities else 0
            if not velocities and completed_stories:
                avg_velocity = sum(s.story_points or 0 for s in completed_stories) / max(1, days / 7)

            predictability = 100
            if len(velocities) >= 2:
                import statistics
                mean_vel = statistics.mean(velocities)
                if mean_vel > 0:
                    predictability = max(0, 100 - (statistics.stdev(velocities) / mean_vel) * 100)

            stories_with_tasks = [s for s in all_stories if s.story_tasks]
            collab_count = sum(1 for s in stories_with_tasks if len(set(t.assignee for t in s.story_tasks if t.assignee)) > 1)
            collab_rate = (collab_count / len(stories_with_tasks) * 100) if stories_with_tasks else 0

            status_counts = {}
            for story in all_stories:
                status_counts[story.status] = status_counts.get(story.status, 0) + 1

            cycle_times = [(s.completed_at - s.started_at).total_seconds() / 3600 / 24 for s in completed_stories if s.started_at and s.completed_at]
            avg_cycle_time = sum(cycle_times) / len(cycle_times) if cycle_times else 0
            wip_count = sum(1 for s in all_stories if s.status in ['in_progress', 'review'])

            # TENDENCIAS
            prev_start = start_date - timedelta(days=days)
            prev_completed = [s for s in all_stories if s.status == 'done' and s.completed_at and prev_start <= s.completed_at < start_date]
            curr_points = sum(s.story_points or 0 for s in completed_stories)
            prev_points = sum(s.story_points or 0 for s in prev_completed)
            velocity_trend = ((curr_points - prev_points) / prev_points * 100) if prev_points > 0 else (100 if curr_points > 0 else 0)

            # ALERTAS
            alerts = []
            if wip_count > 5:
                alerts.append({"type": "warning", "category": "wip", "title": "WIP Elevado", "message": f"{wip_count} stories em progresso simultaneamente. Considere limitar o WIP."})
            if avg_cycle_time > 14:
                alerts.append({"type": "warning", "category": "cycle_time", "title": "Cycle Time Alto", "message": f"Media de {avg_cycle_time:.1f} dias por story. Identifique gargalos."})
            if velocity_trend < -20:
                alerts.append({"type": "danger", "category": "velocity", "title": "Velocity em Queda", "message": f"Reducao de {abs(velocity_trend):.0f}% em relacao ao periodo anterior."})
            review_count = sum(1 for s in all_stories if s.status == 'review')
            if review_count > 3:
                alerts.append({"type": "info", "category": "review", "title": "Acumulo em Review", "message": f"{review_count} stories aguardando revisao."})

            return {
                "period": {"start_date": start_date.isoformat(), "end_date": end_date.isoformat(), "days": days},
                "team_metrics": {
                    "total_stories": len(all_stories),
                    "stories_completed": len(completed_stories),
                    "total_points": total_points,
                    "points_delivered": done_points,
                    "completion_rate": round((done_points / total_points * 100) if total_points > 0 else 0, 1),
                    "avg_velocity": round(avg_velocity, 1),
                    "velocity_unit": "pontos/sprint" if velocities else "pontos/semana",
                    "velocity_trend": round(velocity_trend, 1),
                    "predictability_score": round(predictability, 1),
                    "collaboration_rate": round(collab_rate, 1),
                    "throughput": round(len(completed_stories) / max(1, days / 7), 2),
                    "throughput_unit": "stories/semana",
                    "avg_cycle_time_days": round(avg_cycle_time, 1),
                    "wip_count": wip_count,
                    "status_distribution": status_counts
                },
                "developer_metrics": developer_stats,
                "top_contributors": developer_stats[:5],
                "alerts": alerts,
                "generated_at": datetime.utcnow().isoformat()
            }
        except Exception as e:
            raise HTTPException(500, f"Erro ao calcular analytics: {str(e)}")
        finally:
            db.close()

    @app.get("/api/analytics/insights")
    def get_ai_insights(project_id: Optional[str] = None, sprint_id: Optional[str] = None):
        """Gera insights inteligentes sobre produtividade usando Claude AI."""
        db = SessionLocal()
        try:
            query = db.query(Story)
            if project_id:
                query = query.filter(Story.project_id == project_id)
            if sprint_id:
                query = query.filter(Story.sprint_id == sprint_id)

            all_stories = query.all()
            if not all_stories:
                return {
                    "insights": [],
                    "summary": "Nao ha dados suficientes para gerar insights.",
                    "recommendations": [],
                    "bottlenecks": [],
                    "positive_patterns": [],
                    "generated_at": datetime.utcnow().isoformat()
                }

            total_stories = len(all_stories)
            done_stories = [s for s in all_stories if s.status == 'done']
            in_progress = [s for s in all_stories if s.status == 'in_progress']
            blocked_review = [s for s in all_stories if s.status == 'review']
            total_points = sum(s.story_points or 0 for s in all_stories)
            done_points = sum(s.story_points or 0 for s in done_stories)

            # Cycle times
            cycle_times = []
            for s in done_stories:
                if s.started_at and s.completed_at:
                    cycle_times.append((s.completed_at - s.started_at).days)
            avg_cycle = sum(cycle_times) / len(cycle_times) if cycle_times else 0

            # Assignees
            assignees = {}
            for s in all_stories:
                a = s.assignee or "Nao atribuido"
                if a not in assignees:
                    assignees[a] = {"total": 0, "done": 0, "points": 0}
                assignees[a]["total"] += 1
                assignees[a]["points"] += s.story_points or 0
                if s.status == 'done':
                    assignees[a]["done"] += 1

            context = f"""METRICAS DO PROJETO:
- Total de Stories: {total_stories}
- Stories Concluidas: {len(done_stories)} ({round(len(done_stories)/total_stories*100, 1)}%)
- Stories em Progresso: {len(in_progress)}
- Stories em Review: {len(blocked_review)}
- Total de Pontos: {total_points}
- Pontos Entregues: {done_points} ({round(done_points/total_points*100, 1) if total_points else 0}%)
- Cycle Time Medio: {avg_cycle:.1f} dias

DISTRIBUICAO POR DESENVOLVEDOR:
{chr(10).join(f"- {a}: {d['done']}/{d['total']} stories ({d['points']} pts)" for a, d in assignees.items())}"""

            if HAS_CLAUDE:
                try:
                    claude = get_claude_client()
                    if claude.is_available():
                        system_prompt = """Voce e um especialista em metodologias Agile. Analise as metricas e responda em JSON:
{
    "summary": "Resumo em 2-3 frases",
    "insights": [{"category": "velocity|quality|process|collaboration", "title": "...", "description": "...", "impact": "high|medium|low"}],
    "recommendations": [{"priority": 1, "title": "...", "description": "...", "expected_impact": "..."}],
    "bottlenecks": [{"area": "...", "description": "...", "suggestion": "..."}],
    "positive_patterns": ["..."]
}"""
                        response = claude.chat(
                            message=f"Analise estas metricas:\n\n{context}",
                            system_prompt=system_prompt,
                            max_tokens=2048
                        )
                        if response.success:
                            try:
                                import json as json_module
                                content = response.content.strip()
                                if content.startswith("```"):
                                    lines = content.split("\n")
                                    content = "\n".join(lines[1:-1])
                                result = json_module.loads(content)
                                result["generated_at"] = datetime.utcnow().isoformat()
                                result["source"] = "claude_ai"
                                return result
                            except:
                                pass
                except Exception as e:
                    print(f"[Analytics] Claude error: {e}")

            # Fallback: Insights baseados em regras
            insights = []
            recommendations = []
            bottlenecks = []
            positive_patterns = []

            completion_rate = len(done_stories) / total_stories * 100 if total_stories else 0

            if completion_rate > 70:
                positive_patterns.append(f"Excelente taxa de conclusao: {completion_rate:.0f}%")
            elif completion_rate < 30:
                insights.append({
                    "category": "velocity",
                    "title": "Taxa de conclusao baixa",
                    "description": f"Apenas {completion_rate:.0f}% das stories foram concluidas.",
                    "impact": "high"
                })
                recommendations.append({
                    "priority": 1,
                    "title": "Revisar escopo e priorizacao",
                    "description": "Verifique se as stories estao bem definidas e priorizadas.",
                    "expected_impact": "Aumento na taxa de conclusao"
                })

            if len(blocked_review) > 3:
                bottlenecks.append({
                    "area": "Code Review",
                    "description": f"{len(blocked_review)} stories aguardando revisao",
                    "suggestion": "Estabelecer rotina de reviews diarios ou pair programming"
                })

            if len(in_progress) > 5:
                insights.append({
                    "category": "process",
                    "title": "WIP muito alto",
                    "description": f"{len(in_progress)} stories em progresso pode indicar falta de foco.",
                    "impact": "medium"
                })
                recommendations.append({
                    "priority": 2,
                    "title": "Implementar limite de WIP",
                    "description": "Limite o trabalho em progresso a 2-3 items por desenvolvedor.",
                    "expected_impact": "Melhor foco e reducao do cycle time"
                })

            if avg_cycle > 14:
                insights.append({
                    "category": "velocity",
                    "title": "Cycle time elevado",
                    "description": f"Media de {avg_cycle:.0f} dias por story esta acima do ideal.",
                    "impact": "high"
                })

            # Verificar distribuicao de carga
            if assignees:
                loads = [(a, d["total"]) for a, d in assignees.items() if a != "Nao atribuido"]
                if loads and len(loads) > 1:
                    max_load = max(l[1] for l in loads)
                    min_load = min(l[1] for l in loads)
                    if max_load > min_load * 3:
                        insights.append({
                            "category": "collaboration",
                            "title": "Distribuicao desigual de trabalho",
                            "description": "Alguns desenvolvedores tem muito mais stories que outros.",
                            "impact": "medium"
                        })

            summary = f"O projeto tem {total_stories} stories com {completion_rate:.0f}% de conclusao. "
            if len(bottlenecks) > 0:
                summary += f"Identificados {len(bottlenecks)} gargalo(s). "
            if len(positive_patterns) > 0:
                summary += "Ha padroes positivos a manter."

            return {
                "summary": summary,
                "insights": insights,
                "recommendations": recommendations,
                "bottlenecks": bottlenecks,
                "positive_patterns": positive_patterns,
                "generated_at": datetime.utcnow().isoformat(),
                "source": "rule_based"
            }
        except Exception as e:
            raise HTTPException(500, f"Erro ao gerar insights: {str(e)}")
        finally:
            db.close()

    @app.get("/api/analytics/velocity-history")
    def get_velocity_history(project_id: Optional[str] = None, limit: int = 10):
        """Retorna historico de velocity por sprint para graficos de tendencia."""
        db = SessionLocal()
        try:
            query = db.query(Sprint).filter(Sprint.status.in_(['completed', 'active']))
            if project_id:
                query = query.filter(Sprint.project_id == project_id)
            sprints = query.order_by(Sprint.end_date.desc()).limit(limit).all()

            history = []
            for sprint in reversed(sprints):
                stories = db.query(Story).filter(
                    Story.sprint_id == sprint.sprint_id,
                    Story.status == 'done'
                ).all()
                points = sum(s.story_points or 0 for s in stories)
                history.append({
                    "sprint_id": sprint.sprint_id,
                    "sprint_name": sprint.name,
                    "velocity": sprint.velocity or points,
                    "capacity": sprint.capacity or 0,
                    "stories_completed": len(stories),
                    "start_date": sprint.start_date.isoformat() if sprint.start_date else None,
                    "end_date": sprint.end_date.isoformat() if sprint.end_date else None,
                    "status": sprint.status
                })

            velocities = [h["velocity"] for h in history if h["velocity"] > 0]
            avg_velocity = sum(velocities) / len(velocities) if velocities else 0
            trend = "stable"
            if len(velocities) >= 3:
                recent = sum(velocities[-3:]) / 3
                older = sum(velocities[:-3]) / max(1, len(velocities) - 3) if len(velocities) > 3 else recent
                if recent > older * 1.1:
                    trend = "increasing"
                elif recent < older * 0.9:
                    trend = "decreasing"

            return {
                "history": history,
                "avg_velocity": round(avg_velocity, 1),
                "trend": trend,
                "total_sprints": len(history)
            }
        finally:
            db.close()

    print("[Analytics] Endpoints de produtividade registrados: /api/analytics/productivity, /api/analytics/insights, /api/analytics/velocity-history")
