# -*- coding: utf-8 -*-
"""
Templates Padrao de Notificacao
Fabrica de Agentes v6.0

Define templates para cada tipo de evento e canal.
Os templates usam {variaveis} que sao substituidas pelos dados do evento.
"""

from typing import Dict, Any, List

# =============================================================================
# TEMPLATES DE EMAIL
# =============================================================================

EMAIL_TEMPLATES: Dict[str, Dict[str, str]] = {
    # -------------------------------------------------------------------------
    # PROJETOS
    # -------------------------------------------------------------------------
    "project_created": {
        "subject": "Novo Projeto Criado: {project_name}",
        "body": """
Ola!

Um novo projeto foi criado na Fabrica de Agentes.

Projeto: {project_name}
Tipo: {project_type}
Criado por: {created_by}
Data: {created_at}

{description}

Acesse o dashboard para mais detalhes.
        """.strip(),
        "body_html": """
<h2>Novo Projeto Criado</h2>
<table>
    <tr><td><strong>Projeto:</strong></td><td>{project_name}</td></tr>
    <tr><td><strong>Tipo:</strong></td><td>{project_type}</td></tr>
    <tr><td><strong>Criado por:</strong></td><td>{created_by}</td></tr>
    <tr><td><strong>Data:</strong></td><td>{created_at}</td></tr>
</table>
<p>{description}</p>
        """
    },

    "project_completed": {
        "subject": "Projeto Concluido: {project_name}",
        "body": """
Projeto concluido com sucesso!

Projeto: {project_name}
Status: CONCLUIDO
Tempo de execucao: {duration}
Arquivos gerados: {files_count}

Resultados:
{summary}

Acesse o dashboard para ver os detalhes completos.
        """.strip()
    },

    # -------------------------------------------------------------------------
    # STORIES
    # -------------------------------------------------------------------------
    "story_created": {
        "subject": "[{story_id}] Nova Story: {title}",
        "body": """
Nova User Story criada no projeto {project_name}.

{story_id}: {title}
Prioridade: {priority}
Story Points: {story_points}

Narrativa:
Como um {persona}, eu quero {action}, para que {benefit}.

Criterios de Aceite:
{acceptance_criteria}
        """.strip()
    },

    "story_started": {
        "subject": "[{story_id}] Story Iniciada: {title}",
        "body": """
Uma story comecou a ser trabalhada.

{story_id}: {title}
Responsavel: {assignee}
Sprint: {sprint_name}

O desenvolvimento foi iniciado. Acompanhe o progresso no dashboard.
        """.strip()
    },

    "story_completed": {
        "subject": "[{story_id}] Story Concluida: {title}",
        "body": """
Story finalizada com sucesso!

{story_id}: {title}
Responsavel: {assignee}
Tempo: {duration}

Tasks completadas:
{tasks_summary}

Arquivos criados/modificados:
{files_list}
        """.strip()
    },

    "story_blocked": {
        "subject": "[BLOQUEADO] {story_id}: {title}",
        "body": """
ATENCAO: Uma story foi bloqueada e requer atencao.

{story_id}: {title}
Responsavel: {assignee}

Motivo do bloqueio:
{blocked_reason}

Bloqueado por:
{blocked_by}

Por favor, verifique e tome as acoes necessarias.
        """.strip()
    },

    "story_urgent": {
        "subject": "[URGENTE] Nova Story de Alta Prioridade: {title}",
        "body": """
ATENCAO: Uma story de alta prioridade foi criada.

{story_id}: {title}
Prioridade: {priority}
Story Points: {story_points}

Esta story requer atencao imediata.

Narrativa:
Como um {persona}, eu quero {action}, para que {benefit}.
        """.strip()
    },

    # -------------------------------------------------------------------------
    # TASKS
    # -------------------------------------------------------------------------
    "task_completed": {
        "subject": "[{task_id}] Task Concluida: {title}",
        "body": """
Task finalizada.

{task_id}: {title}
Story: {story_id}
Tipo: {task_type}

Progresso da Story: {story_progress}%
        """.strip()
    },

    "task_failed": {
        "subject": "[ERRO] Task Falhou: {title}",
        "body": """
Uma task falhou durante a execucao.

{task_id}: {title}
Story: {story_id}

Erro:
{error_message}

Detalhes:
{error_details}

Por favor, verifique os logs e tente novamente.
        """.strip()
    },

    # -------------------------------------------------------------------------
    # JOBS
    # -------------------------------------------------------------------------
    "job_started": {
        "subject": "[{job_id}] Job Iniciado",
        "body": """
Um job foi iniciado para processamento.

Job: {job_id}
Projeto: {project_name}
Worker: {worker_id}

Descricao:
{description}

Acompanhe o progresso no dashboard.
        """.strip()
    },

    "job_completed": {
        "subject": "[{job_id}] Job Concluido com Sucesso",
        "body": """
Job finalizado com sucesso!

Job: {job_id}
Projeto: {project_name}
Duracao: {duration}

Arquivos gerados: {files_count}
Output: {output_path}

{summary}
        """.strip()
    },

    "job_failed": {
        "subject": "[ERRO] Job Falhou: {job_id}",
        "body": """
Um job falhou durante a execucao.

Job: {job_id}
Projeto: {project_name}
Etapa: {current_step}
Tentativa: {attempt}/{max_attempts}

Erro:
{error_message}

Verifique os logs para mais detalhes.
        """.strip()
    },

    # -------------------------------------------------------------------------
    # SISTEMA
    # -------------------------------------------------------------------------
    "system_error": {
        "subject": "[ERRO CRITICO] {error_type}",
        "body": """
ATENCAO: Um erro critico ocorreu no sistema.

Tipo: {error_type}
Severidade: {severity}
Componente: {component}

Mensagem:
{error_message}

Stack Trace:
{stack_trace}

Timestamp: {timestamp}

Acao imediata pode ser necessaria.
        """.strip()
    },

    "limit_alert": {
        "subject": "[ALERTA] Limite Atingido: {limit_type}",
        "body": """
Um limite de uso foi atingido ou esta proximo.

Tipo: {limit_type}
Uso atual: {current_usage}
Limite: {limit_value}
Porcentagem: {percentage}%

Usuario/Projeto: {resource_name}

Considere aumentar o limite ou otimizar o uso.
        """.strip()
    },

    "approval_required": {
        "subject": "[APROVACAO] Acao Requer Aprovacao",
        "body": """
Uma acao requer sua aprovacao.

Tipo: {action_type}
Solicitante: {requester}
Projeto: {project_name}

Descricao:
{description}

Para aprovar ou rejeitar, acesse:
{approval_url}
        """.strip()
    },

    "worker_offline": {
        "subject": "[ALERTA] Worker Offline: {worker_id}",
        "body": """
Um worker ficou offline.

Worker: {worker_id}
Ultimo heartbeat: {last_heartbeat}
Jobs em andamento: {pending_jobs}

Verifique o status do worker e reinicie se necessario.
        """.strip()
    },

    # -------------------------------------------------------------------------
    # RELATORIOS
    # -------------------------------------------------------------------------
    "report_daily": {
        "subject": "Relatorio Diario - {date}",
        "body": """
RELATORIO DIARIO - Fabrica de Agentes
Data: {date}

RESUMO:
- Projetos ativos: {projects_active}
- Stories completadas: {stories_completed}
- Tasks executadas: {tasks_executed}
- Jobs processados: {jobs_processed}

DESTAQUES:
{highlights}

METRICAS:
- Taxa de sucesso: {success_rate}%
- Tempo medio de job: {avg_job_time}
- Erros: {errors_count}

{details}
        """.strip()
    },

    "report_weekly": {
        "subject": "Relatorio Semanal - Semana {week_number}",
        "body": """
RELATORIO SEMANAL - Fabrica de Agentes
Semana: {week_number} ({start_date} - {end_date})

RESUMO EXECUTIVO:
{executive_summary}

METRICAS DA SEMANA:
- Projetos criados: {projects_created}
- Projetos concluidos: {projects_completed}
- Stories finalizadas: {stories_completed}
- Total de jobs: {total_jobs}

COMPARATIVO:
- vs semana anterior: {comparison}

PROXIMOS PASSOS:
{next_steps}
        """.strip()
    }
}


# =============================================================================
# TEMPLATES DE SLACK
# =============================================================================

SLACK_TEMPLATES: Dict[str, Dict[str, str]] = {
    "project_created": {
        "subject": "Novo Projeto: {project_name}",
        "body": "*Projeto:* {project_name}\n*Tipo:* {project_type}\n*Por:* {created_by}"
    },
    "project_completed": {
        "subject": "Projeto Concluido: {project_name}",
        "body": "*{project_name}* finalizado com sucesso!\n\n*Tempo:* {duration}\n*Arquivos:* {files_count}"
    },
    "story_created": {
        "subject": "[{story_id}] {title}",
        "body": "*{title}*\n\nComo um _{persona}_, eu quero _{action}_, para que _{benefit}_.\n\n*Pontos:* {story_points} | *Prioridade:* {priority}"
    },
    "story_completed": {
        "subject": "Story Concluida: {title}",
        "body": "*{story_id}* - {title}\n*Responsavel:* {assignee}\n*Tasks:* {tasks_completed}/{tasks_total}"
    },
    "story_blocked": {
        "subject": "BLOQUEADO: {title}",
        "body": "*{story_id}* esta bloqueada!\n\n*Motivo:* {blocked_reason}"
    },
    "job_failed": {
        "subject": "Job Falhou: {job_id}",
        "body": "*Job:* {job_id}\n*Etapa:* {current_step}\n*Erro:* {error_message}"
    },
    "system_error": {
        "subject": "ERRO: {error_type}",
        "body": "*Severidade:* {severity}\n*Componente:* {component}\n\n```{error_message}```"
    }
}


# =============================================================================
# TEMPLATES DE TEAMS
# =============================================================================

TEAMS_TEMPLATES: Dict[str, Dict[str, str]] = {
    "project_created": {
        "subject": "Novo Projeto: {project_name}",
        "body": "**Projeto:** {project_name}\n\n**Tipo:** {project_type}\n\n**Criado por:** {created_by}\n\n{description}"
    },
    "project_completed": {
        "subject": "Projeto Concluido: {project_name}",
        "body": "**{project_name}** foi finalizado com sucesso!\n\n**Tempo de execucao:** {duration}\n\n**Arquivos gerados:** {files_count}"
    },
    "story_created": {
        "subject": "[{story_id}] Nova Story: {title}",
        "body": "**{title}**\n\nComo um *{persona}*, eu quero *{action}*, para que *{benefit}*.\n\n**Story Points:** {story_points}\n\n**Prioridade:** {priority}"
    },
    "story_completed": {
        "subject": "Story Concluida: {title}",
        "body": "**{story_id}** - {title}\n\n**Responsavel:** {assignee}\n\n**Tasks:** {tasks_completed}/{tasks_total}"
    },
    "story_blocked": {
        "subject": "Story Bloqueada: {title}",
        "body": "**{story_id}** - {title}\n\n**Motivo:** {blocked_reason}\n\n**Bloqueado por:** {blocked_by}"
    },
    "job_completed": {
        "subject": "Job Concluido: {job_id}",
        "body": "**Job:** {job_id}\n\n**Projeto:** {project_name}\n\n**Duracao:** {duration}\n\n**Arquivos:** {files_count}"
    },
    "job_failed": {
        "subject": "Job Falhou: {job_id}",
        "body": "**Job:** {job_id}\n\n**Etapa:** {current_step}\n\n**Tentativa:** {attempt}/{max_attempts}\n\n**Erro:**\n\n{error_message}"
    },
    "system_error": {
        "subject": "Erro Critico: {error_type}",
        "body": "**Severidade:** {severity}\n\n**Componente:** {component}\n\n**Mensagem:**\n\n{error_message}"
    },
    "worker_offline": {
        "subject": "Worker Offline: {worker_id}",
        "body": "**Worker:** {worker_id}\n\n**Ultimo heartbeat:** {last_heartbeat}\n\n**Jobs pendentes:** {pending_jobs}"
    }
}


# =============================================================================
# FUNCOES AUXILIARES
# =============================================================================

def get_template(event_type: str, channel_type: str) -> Dict[str, str]:
    """
    Retorna template para evento/canal.

    Args:
        event_type: Tipo do evento
        channel_type: Tipo do canal (email, slack, teams)

    Returns:
        Dicionario com subject, body, body_html
    """
    templates_map = {
        "email": EMAIL_TEMPLATES,
        "slack": SLACK_TEMPLATES,
        "teams": TEAMS_TEMPLATES
    }

    templates = templates_map.get(channel_type, {})
    return templates.get(event_type, {
        "subject": f"[{event_type}] Notificacao",
        "body": "Evento: {event_type}"
    })


def list_available_templates() -> Dict[str, List[str]]:
    """
    Lista templates disponiveis por canal.

    Returns:
        {"email": [...], "slack": [...], "teams": [...]}
    """
    return {
        "email": list(EMAIL_TEMPLATES.keys()),
        "slack": list(SLACK_TEMPLATES.keys()),
        "teams": list(TEAMS_TEMPLATES.keys())
    }


def get_template_variables(event_type: str) -> List[str]:
    """
    Retorna variaveis usadas em um template.

    Args:
        event_type: Tipo do evento

    Returns:
        Lista de nomes de variaveis
    """
    import re

    variables = set()
    template = EMAIL_TEMPLATES.get(event_type, {})

    for field in ["subject", "body", "body_html"]:
        if field in template:
            # Encontrar {variavel}
            matches = re.findall(r'\{(\w+)\}', template[field])
            variables.update(matches)

    return sorted(list(variables))
