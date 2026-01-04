# -*- coding: utf-8 -*-
"""
Regras Padrao de Notificacao
Plataforma E v6.0

Define regras padrao para os eventos mais comuns do sistema.
"""

from typing import List, Dict, Any

# =============================================================================
# REGRAS PADRAO
# =============================================================================

DEFAULT_RULES: List[Dict[str, Any]] = [
    # -------------------------------------------------------------------------
    # PROJETOS
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-001",
        "name": "Projeto Criado",
        "description": "Notifica quando um novo projeto e criado",
        "event_type": "project_created",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["owner", "admin"],
        "template": "project_created",
        "priority": "normal",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-002",
        "name": "Projeto Concluido",
        "description": "Notifica quando um projeto e finalizado com sucesso",
        "event_type": "project_completed",
        "conditions": {},
        "channels": ["email", "teams", "slack"],
        "recipients": ["owner", "team"],
        "template": "project_completed",
        "priority": "normal",
        "is_active": True,
        "config": {"include_summary": True}
    },

    # -------------------------------------------------------------------------
    # STORIES
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-010",
        "name": "Story Criada",
        "description": "Notifica sobre nova user story",
        "event_type": "story_created",
        "conditions": {},
        "channels": ["teams"],
        "recipients": ["team"],
        "template": "story_created",
        "priority": "low",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-011",
        "name": "Story Iniciada",
        "description": "Notifica quando uma story comeca a ser trabalhada",
        "event_type": "story_started",
        "conditions": {},
        "channels": ["teams"],
        "recipients": ["assignee", "reporter"],
        "template": "story_started",
        "priority": "normal",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-012",
        "name": "Story Concluida",
        "description": "Notifica quando uma story e finalizada",
        "event_type": "story_completed",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["reporter", "team"],
        "template": "story_completed",
        "priority": "normal",
        "is_active": True,
        "config": {"include_tasks": True}
    },
    {
        "rule_id": "RULE-013",
        "name": "Story Bloqueada",
        "description": "Notifica urgentemente quando uma story e bloqueada",
        "event_type": "story_blocked",
        "conditions": {},
        "channels": ["email", "teams", "slack"],
        "recipients": ["assignee", "reporter", "admin"],
        "template": "story_blocked",
        "priority": "high",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-014",
        "name": "Story de Alta Prioridade Criada",
        "description": "Notifica sobre stories urgentes",
        "event_type": "story_created",
        "conditions": {
            "operator": "OR",
            "conditions": [
                {"field": "priority", "op": "eq", "value": "urgent"},
                {"field": "priority", "op": "eq", "value": "high"}
            ]
        },
        "channels": ["email", "teams", "slack"],
        "recipients": ["team", "admin"],
        "template": "story_urgent",
        "priority": "high",
        "is_active": True,
        "config": {}
    },

    # -------------------------------------------------------------------------
    # TASKS
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-020",
        "name": "Task Concluida",
        "description": "Notifica quando uma task e finalizada",
        "event_type": "task_completed",
        "conditions": {},
        "channels": ["teams"],
        "recipients": ["assignee"],
        "template": "task_completed",
        "priority": "low",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-021",
        "name": "Task Falhou",
        "description": "Notifica quando uma task falha",
        "event_type": "task_failed",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["assignee", "admin"],
        "template": "task_failed",
        "priority": "high",
        "is_active": True,
        "config": {"include_error_details": True}
    },

    # -------------------------------------------------------------------------
    # JOBS
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-030",
        "name": "Job Iniciado",
        "description": "Notifica quando um job comeca a ser processado",
        "event_type": "job_started",
        "conditions": {},
        "channels": ["teams"],
        "recipients": ["owner"],
        "template": "job_started",
        "priority": "low",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-031",
        "name": "Job Concluido",
        "description": "Notifica quando um job termina com sucesso",
        "event_type": "job_completed",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["owner"],
        "template": "job_completed",
        "priority": "normal",
        "is_active": True,
        "config": {"include_artifacts": True}
    },
    {
        "rule_id": "RULE-032",
        "name": "Job Falhou",
        "description": "Notifica quando um job falha",
        "event_type": "job_failed",
        "conditions": {},
        "channels": ["email", "teams", "slack"],
        "recipients": ["owner", "admin"],
        "template": "job_failed",
        "priority": "high",
        "is_active": True,
        "config": {"include_error_log": True}
    },

    # -------------------------------------------------------------------------
    # SISTEMA
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-040",
        "name": "Erro Critico",
        "description": "Notifica sobre erros criticos do sistema",
        "event_type": "error",
        "conditions": {
            "operator": "OR",
            "conditions": [
                {"field": "severity", "op": "eq", "value": "critical"},
                {"field": "severity", "op": "eq", "value": "fatal"}
            ]
        },
        "channels": ["email", "teams", "slack"],
        "recipients": ["admin"],
        "template": "system_error",
        "priority": "urgent",
        "is_active": True,
        "config": {"bypass_quiet_hours": True}
    },
    {
        "rule_id": "RULE-041",
        "name": "Alerta de Limite",
        "description": "Notifica quando limites de uso sao atingidos",
        "event_type": "limit_alert",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["admin", "owner"],
        "template": "limit_alert",
        "priority": "high",
        "is_active": True,
        "config": {}
    },
    {
        "rule_id": "RULE-042",
        "name": "Aprovacao Necessaria",
        "description": "Notifica quando uma aprovacao e necessaria",
        "event_type": "approval_required",
        "conditions": {},
        "channels": ["email", "teams"],
        "recipients": ["approvers"],
        "template": "approval_required",
        "priority": "high",
        "is_active": True,
        "config": {}
    },

    # -------------------------------------------------------------------------
    # WORKERS
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-050",
        "name": "Worker Offline",
        "description": "Notifica quando um worker fica offline",
        "event_type": "worker_offline",
        "conditions": {},
        "channels": ["teams", "slack"],
        "recipients": ["admin"],
        "template": "worker_offline",
        "priority": "high",
        "is_active": True,
        "config": {}
    },

    # -------------------------------------------------------------------------
    # RELATORIOS
    # -------------------------------------------------------------------------
    {
        "rule_id": "RULE-060",
        "name": "Relatorio Diario",
        "description": "Envia resumo diario de atividades",
        "event_type": "report_daily",
        "conditions": {},
        "channels": ["email"],
        "recipients": ["team", "admin"],
        "template": "report_daily",
        "priority": "low",
        "is_active": True,
        "config": {
            "schedule": "08:00",
            "include_metrics": True
        }
    },
    {
        "rule_id": "RULE-061",
        "name": "Relatorio Semanal",
        "description": "Envia resumo semanal de atividades",
        "event_type": "report_weekly",
        "conditions": {},
        "channels": ["email"],
        "recipients": ["admin", "stakeholders"],
        "template": "report_weekly",
        "priority": "low",
        "is_active": True,
        "config": {
            "schedule": "monday_08:00",
            "include_charts": True
        }
    }
]


def create_default_rules() -> List[Dict[str, Any]]:
    """
    Retorna uma copia das regras padrao.
    Pode ser usada para inicializar o banco de dados.

    Returns:
        Lista de regras padrao
    """
    return [dict(rule) for rule in DEFAULT_RULES]


def get_rule_by_event(event_type: str) -> List[Dict[str, Any]]:
    """
    Retorna regras padrao para um tipo de evento.

    Args:
        event_type: Tipo do evento

    Returns:
        Lista de regras que correspondem ao evento
    """
    return [
        rule for rule in DEFAULT_RULES
        if rule["event_type"] == event_type and rule["is_active"]
    ]


def get_high_priority_rules() -> List[Dict[str, Any]]:
    """
    Retorna regras de alta prioridade.

    Returns:
        Lista de regras urgentes ou de alta prioridade
    """
    return [
        rule for rule in DEFAULT_RULES
        if rule["priority"] in ["urgent", "high"] and rule["is_active"]
    ]


# =============================================================================
# CONFIGURACAO PADRAO DE CANAIS
# =============================================================================

DEFAULT_CHANNEL_CONFIG = {
    "email": {
        "enabled": True,
        "priority": 1,
        "description": "Canal de email SMTP",
        "rate_limit_per_minute": 30,
        "rate_limit_per_hour": 500
    },
    "teams": {
        "enabled": True,
        "priority": 2,
        "description": "Microsoft Teams via webhook",
        "rate_limit_per_minute": 60,
        "rate_limit_per_hour": 1000
    },
    "slack": {
        "enabled": False,
        "priority": 3,
        "description": "Slack via webhook",
        "rate_limit_per_minute": 60,
        "rate_limit_per_hour": 1000
    },
    "whatsapp": {
        "enabled": False,
        "priority": 4,
        "description": "WhatsApp Business API",
        "rate_limit_per_minute": 10,
        "rate_limit_per_hour": 100
    },
    "sms": {
        "enabled": False,
        "priority": 5,
        "description": "SMS via Twilio",
        "rate_limit_per_minute": 10,
        "rate_limit_per_hour": 50
    }
}


# =============================================================================
# CONFIGURACAO PADRAO DE QUIET HOURS
# =============================================================================

DEFAULT_QUIET_HOURS = {
    "enabled": True,
    "start": "22:00",
    "end": "07:00",
    "timezone": "America/Sao_Paulo",
    "exceptions": [
        "error",
        "approval_required",
        "worker_offline"
    ],
    "weekend_all_day": False
}
