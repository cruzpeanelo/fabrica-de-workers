# -*- coding: utf-8 -*-
"""
Wizards e Assistentes Passo-a-Passo - Issue #134
================================================
Sistema de wizards interativos para guiar usuarios:
- Wizard para criar projeto
- Wizard para criar story
- Wizard para configurar integracoes
- Validacao em cada passo
"""

from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import APIRouter
from fastapi.responses import HTMLResponse, JSONResponse
from pydantic import BaseModel, validator


# =============================================================================
# ROUTER
# =============================================================================

wizard_router = APIRouter(prefix="/api/wizards", tags=["Wizards"])


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class WizardStep(BaseModel):
    step_id: str
    title: str
    description: str
    fields: List[Dict[str, Any]]
    validation_rules: Optional[Dict[str, Any]] = None
    help_text: Optional[str] = None


class WizardProgress(BaseModel):
    wizard_id: str
    user_id: str
    current_step: int = 0
    completed_steps: List[str] = []
    data: Dict[str, Any] = {}
    started_at: Optional[str] = None
    completed_at: Optional[str] = None


class ProjectWizardData(BaseModel):
    name: str
    description: Optional[str] = None
    project_type: str = "web"
    team_size: int = 1
    deadline: Optional[str] = None
    priority: str = "medium"

    @validator('name')
    def name_not_empty(cls, v):
        if not v or len(v.strip()) < 3:
            raise ValueError('O nome do projeto deve ter pelo menos 3 caracteres')
        return v.strip()


class StoryWizardData(BaseModel):
    title: str
    persona: str
    action: str
    benefit: str
    acceptance_criteria: List[str] = []
    priority: str = "medium"
    story_points: int = 3

    @validator('title')
    def title_not_empty(cls, v):
        if not v or len(v.strip()) < 5:
            raise ValueError('O titulo deve ter pelo menos 5 caracteres')
        return v.strip()


class IntegrationWizardData(BaseModel):
    integration_type: str
    api_url: Optional[str] = None
    api_key: Optional[str] = None
    webhook_url: Optional[str] = None
    settings: Dict[str, Any] = {}


# =============================================================================
# WIZARD DEFINITIONS
# =============================================================================

PROJECT_WIZARD = {
    "wizard_id": "create_project",
    "title": "Criar Novo Projeto",
    "description": "Vamos criar seu projeto em poucos passos simples",
    "steps": [
        {
            "step_id": "basics",
            "title": "Informacoes Basicas",
            "description": "Vamos comecar com o nome e descricao do seu projeto",
            "help_text": "Escolha um nome claro que descreva o objetivo do projeto",
            "fields": [
                {
                    "name": "name",
                    "label": "Nome do Projeto",
                    "type": "text",
                    "placeholder": "Ex: Sistema de Vendas Online",
                    "required": True,
                    "help": "Um nome curto e descritivo para identificar o projeto"
                },
                {
                    "name": "description",
                    "label": "Descricao",
                    "type": "textarea",
                    "placeholder": "Descreva brevemente o objetivo do projeto...",
                    "required": False,
                    "help": "Explique o que o projeto deve fazer"
                }
            ],
            "validation_rules": {
                "name": {"min_length": 3, "max_length": 100}
            }
        },
        {
            "step_id": "type",
            "title": "Tipo de Projeto",
            "description": "Que tipo de aplicacao voce quer criar?",
            "help_text": "Isso nos ajuda a configurar as melhores opcoes para voce",
            "fields": [
                {
                    "name": "project_type",
                    "label": "Tipo de Aplicacao",
                    "type": "select",
                    "options": [
                        {"value": "web", "label": "Site ou Sistema Web", "icon": "globe", "description": "Aplicacao acessada pelo navegador"},
                        {"value": "api", "label": "API / Servico", "icon": "server", "description": "Backend que conecta sistemas"},
                        {"value": "mobile", "label": "Aplicativo Mobile", "icon": "smartphone", "description": "App para celular"},
                        {"value": "desktop", "label": "Aplicativo Desktop", "icon": "monitor", "description": "Programa para computador"},
                        {"value": "automation", "label": "Automacao", "icon": "bot", "description": "Scripts e processos automaticos"}
                    ],
                    "required": True,
                    "display": "cards"
                }
            ]
        },
        {
            "step_id": "team",
            "title": "Equipe e Prazo",
            "description": "Quantas pessoas vao trabalhar e qual o prazo?",
            "help_text": "Essas informacoes ajudam a planejar melhor as tarefas",
            "fields": [
                {
                    "name": "team_size",
                    "label": "Tamanho da Equipe",
                    "type": "number",
                    "min": 1,
                    "max": 50,
                    "default": 1,
                    "required": True,
                    "help": "Quantas pessoas vao trabalhar no projeto"
                },
                {
                    "name": "deadline",
                    "label": "Prazo de Entrega",
                    "type": "date",
                    "required": False,
                    "help": "Quando o projeto precisa estar pronto (opcional)"
                },
                {
                    "name": "priority",
                    "label": "Prioridade",
                    "type": "select",
                    "options": [
                        {"value": "low", "label": "Baixa - Pode esperar"},
                        {"value": "medium", "label": "Normal - Prazo padrao"},
                        {"value": "high", "label": "Alta - Urgente"},
                        {"value": "critical", "label": "Critica - Imediato"}
                    ],
                    "default": "medium",
                    "required": True
                }
            ]
        },
        {
            "step_id": "review",
            "title": "Revisar e Criar",
            "description": "Confira as informacoes antes de criar o projeto",
            "help_text": "Voce pode voltar e editar se precisar",
            "fields": [],
            "is_summary": True
        }
    ]
}


STORY_WIZARD = {
    "wizard_id": "create_story",
    "title": "Criar Nova Tarefa",
    "description": "Descreva o que precisa ser feito de forma simples",
    "steps": [
        {
            "step_id": "what",
            "title": "O Que Precisa Ser Feito?",
            "description": "Descreva a tarefa de forma clara e objetiva",
            "help_text": "Pense no resultado final que voce espera",
            "fields": [
                {
                    "name": "title",
                    "label": "Titulo da Tarefa",
                    "type": "text",
                    "placeholder": "Ex: Criar pagina de login",
                    "required": True,
                    "help": "Um titulo curto que resume a tarefa"
                }
            ],
            "validation_rules": {
                "title": {"min_length": 5, "max_length": 200}
            }
        },
        {
            "step_id": "who",
            "title": "Para Quem?",
            "description": "Quem vai usar essa funcionalidade?",
            "help_text": "Identificar o usuario ajuda a entender melhor a necessidade",
            "fields": [
                {
                    "name": "persona",
                    "label": "Tipo de Usuario",
                    "type": "select",
                    "options": [
                        {"value": "cliente", "label": "Cliente", "icon": "user", "description": "Usuario final do sistema"},
                        {"value": "admin", "label": "Administrador", "icon": "shield", "description": "Gerencia o sistema"},
                        {"value": "operador", "label": "Operador", "icon": "headphones", "description": "Atendimento e suporte"},
                        {"value": "gestor", "label": "Gestor", "icon": "briefcase", "description": "Toma decisoes"},
                        {"value": "visitante", "label": "Visitante", "icon": "eye", "description": "Usuario nao logado"},
                        {"value": "outro", "label": "Outro", "icon": "users", "description": "Especificar abaixo"}
                    ],
                    "required": True,
                    "display": "cards"
                },
                {
                    "name": "persona_custom",
                    "label": "Especifique o tipo de usuario",
                    "type": "text",
                    "placeholder": "Ex: Vendedor externo",
                    "required": False,
                    "show_if": {"persona": "outro"}
                }
            ]
        },
        {
            "step_id": "action",
            "title": "Qual a Acao?",
            "description": "O que o usuario quer fazer?",
            "help_text": "Descreva a acao principal que o usuario precisa realizar",
            "fields": [
                {
                    "name": "action",
                    "label": "O que o usuario quer fazer?",
                    "type": "textarea",
                    "placeholder": "Ex: Fazer login com email e senha",
                    "required": True,
                    "help": "Descreva a acao que o usuario precisa realizar",
                    "rows": 3
                }
            ]
        },
        {
            "step_id": "benefit",
            "title": "Por Que?",
            "description": "Qual o beneficio dessa funcionalidade?",
            "help_text": "Entender o 'por que' ajuda a fazer melhor",
            "fields": [
                {
                    "name": "benefit",
                    "label": "Qual o beneficio?",
                    "type": "textarea",
                    "placeholder": "Ex: Para acessar sua conta e ver suas compras",
                    "required": True,
                    "help": "Por que essa funcionalidade e importante?",
                    "rows": 3
                }
            ]
        },
        {
            "step_id": "criteria",
            "title": "Como Saber se Funcionou?",
            "description": "Defina o que precisa acontecer para a tarefa estar completa",
            "help_text": "Esses criterios ajudam a verificar se tudo esta certo",
            "fields": [
                {
                    "name": "acceptance_criteria",
                    "label": "Criterios de Aceite",
                    "type": "checklist",
                    "placeholder": "Ex: Usuario consegue fazer login com email valido",
                    "required": False,
                    "help": "Liste o que precisa funcionar",
                    "add_button": "Adicionar criterio"
                }
            ]
        },
        {
            "step_id": "priority",
            "title": "Prioridade e Esforco",
            "description": "Quao urgente e trabalhosa e essa tarefa?",
            "help_text": "Isso ajuda a planejar quando sera feita",
            "fields": [
                {
                    "name": "priority",
                    "label": "Prioridade",
                    "type": "select",
                    "options": [
                        {"value": "low", "label": "Baixa - Pode esperar", "color": "#6B7280"},
                        {"value": "medium", "label": "Normal", "color": "#3B82F6"},
                        {"value": "high", "label": "Alta - Importante", "color": "#F59E0B"},
                        {"value": "urgent", "label": "Urgente - Fazer agora", "color": "#EF4444"}
                    ],
                    "default": "medium",
                    "required": True
                },
                {
                    "name": "story_points",
                    "label": "Tamanho da Tarefa",
                    "type": "slider",
                    "options": [
                        {"value": 1, "label": "Muito pequena (1)", "description": "Algumas horas"},
                        {"value": 2, "label": "Pequena (2)", "description": "Menos de 1 dia"},
                        {"value": 3, "label": "Media (3)", "description": "1-2 dias"},
                        {"value": 5, "label": "Grande (5)", "description": "3-5 dias"},
                        {"value": 8, "label": "Muito grande (8)", "description": "1 semana"},
                        {"value": 13, "label": "Enorme (13)", "description": "2 semanas ou mais"}
                    ],
                    "default": 3,
                    "required": True,
                    "help": "Estime o esforco necessario"
                }
            ]
        },
        {
            "step_id": "review",
            "title": "Revisar Tarefa",
            "description": "Confira as informacoes da sua tarefa",
            "help_text": "Tudo certo? Clique em Criar para finalizar",
            "fields": [],
            "is_summary": True
        }
    ]
}


INTEGRATION_WIZARD = {
    "wizard_id": "setup_integration",
    "title": "Configurar Integracao",
    "description": "Conecte seu projeto a outros sistemas",
    "steps": [
        {
            "step_id": "type",
            "title": "Escolha a Integracao",
            "description": "Com qual sistema voce quer conectar?",
            "help_text": "Escolha o tipo de integracao que precisa",
            "fields": [
                {
                    "name": "integration_type",
                    "label": "Tipo de Integracao",
                    "type": "select",
                    "options": [
                        {"value": "github", "label": "GitHub", "icon": "github", "description": "Repositorio de codigo"},
                        {"value": "gitlab", "label": "GitLab", "icon": "gitlab", "description": "Repositorio de codigo"},
                        {"value": "jira", "label": "Jira", "icon": "trello", "description": "Gestao de projetos"},
                        {"value": "slack", "label": "Slack", "icon": "message-square", "description": "Notificacoes em tempo real"},
                        {"value": "teams", "label": "Microsoft Teams", "icon": "message-circle", "description": "Notificacoes em tempo real"},
                        {"value": "webhook", "label": "Webhook Customizado", "icon": "webhook", "description": "Integracao personalizada"},
                        {"value": "api", "label": "API Externa", "icon": "plug", "description": "Conectar a qualquer API"}
                    ],
                    "required": True,
                    "display": "cards"
                }
            ]
        },
        {
            "step_id": "credentials",
            "title": "Credenciais",
            "description": "Insira as informacoes de acesso",
            "help_text": "Essas informacoes sao necessarias para a conexao",
            "fields": [
                {
                    "name": "api_url",
                    "label": "URL da API",
                    "type": "url",
                    "placeholder": "https://api.exemplo.com",
                    "required": False,
                    "show_if": {"integration_type": ["api", "webhook"]},
                    "help": "Endereco completo da API"
                },
                {
                    "name": "api_key",
                    "label": "Chave de API / Token",
                    "type": "password",
                    "placeholder": "Cole sua chave aqui",
                    "required": True,
                    "help": "Token de autenticacao (nunca compartilhe)"
                },
                {
                    "name": "webhook_url",
                    "label": "URL do Webhook",
                    "type": "url",
                    "placeholder": "https://seu-servidor.com/webhook",
                    "required": False,
                    "show_if": {"integration_type": "webhook"},
                    "help": "Endereco para receber notificacoes"
                }
            ]
        },
        {
            "step_id": "settings",
            "title": "Configuracoes",
            "description": "Ajuste como a integracao vai funcionar",
            "help_text": "Personalize o comportamento da conexao",
            "fields": [
                {
                    "name": "notify_on_story_created",
                    "label": "Notificar quando tarefa for criada",
                    "type": "checkbox",
                    "default": True
                },
                {
                    "name": "notify_on_story_completed",
                    "label": "Notificar quando tarefa for concluida",
                    "type": "checkbox",
                    "default": True
                },
                {
                    "name": "sync_comments",
                    "label": "Sincronizar comentarios",
                    "type": "checkbox",
                    "default": False
                },
                {
                    "name": "auto_update_status",
                    "label": "Atualizar status automaticamente",
                    "type": "checkbox",
                    "default": True
                }
            ]
        },
        {
            "step_id": "test",
            "title": "Testar Conexao",
            "description": "Vamos verificar se a integracao funciona",
            "help_text": "Clique em 'Testar' para verificar a conexao",
            "fields": [],
            "has_test": True
        },
        {
            "step_id": "review",
            "title": "Revisar e Ativar",
            "description": "Confira as configuracoes e ative a integracao",
            "help_text": "Tudo certo! Clique em Ativar para finalizar",
            "fields": [],
            "is_summary": True
        }
    ]
}


# All wizards registry
WIZARDS = {
    "create_project": PROJECT_WIZARD,
    "create_story": STORY_WIZARD,
    "setup_integration": INTEGRATION_WIZARD
}


# In-memory storage for wizard progress (use database in production)
wizard_progress: Dict[str, Dict[str, Any]] = {}


# =============================================================================
# API ENDPOINTS
# =============================================================================

@wizard_router.get("/list")
async def list_wizards():
    """Lista todos os wizards disponiveis"""
    return {
        "wizards": [
            {
                "wizard_id": w["wizard_id"],
                "title": w["title"],
                "description": w["description"],
                "steps_count": len(w["steps"])
            }
            for w in WIZARDS.values()
        ]
    }


@wizard_router.get("/{wizard_id}")
async def get_wizard(wizard_id: str):
    """Retorna a configuracao completa de um wizard"""
    if wizard_id not in WIZARDS:
        return JSONResponse(
            status_code=404,
            content={"error": "Wizard nao encontrado"}
        )
    return WIZARDS[wizard_id]


@wizard_router.get("/{wizard_id}/step/{step_index}")
async def get_wizard_step(wizard_id: str, step_index: int):
    """Retorna um passo especifico do wizard"""
    if wizard_id not in WIZARDS:
        return JSONResponse(
            status_code=404,
            content={"error": "Wizard nao encontrado"}
        )

    wizard = WIZARDS[wizard_id]
    if step_index < 0 or step_index >= len(wizard["steps"]):
        return JSONResponse(
            status_code=400,
            content={"error": "Passo invalido"}
        )

    return {
        "step": wizard["steps"][step_index],
        "step_index": step_index,
        "total_steps": len(wizard["steps"]),
        "is_first": step_index == 0,
        "is_last": step_index == len(wizard["steps"]) - 1
    }


@wizard_router.post("/{wizard_id}/validate/{step_index}")
async def validate_step(wizard_id: str, step_index: int, data: Dict[str, Any]):
    """Valida os dados de um passo"""
    if wizard_id not in WIZARDS:
        return JSONResponse(
            status_code=404,
            content={"error": "Wizard nao encontrado"}
        )

    wizard = WIZARDS[wizard_id]
    if step_index < 0 or step_index >= len(wizard["steps"]):
        return JSONResponse(
            status_code=400,
            content={"error": "Passo invalido"}
        )

    step = wizard["steps"][step_index]
    errors = []

    # Check required fields
    for field in step.get("fields", []):
        field_name = field["name"]
        is_required = field.get("required", False)

        if is_required and (field_name not in data or not data[field_name]):
            errors.append({
                "field": field_name,
                "message": f"O campo '{field['label']}' e obrigatorio"
            })

    # Check validation rules
    rules = step.get("validation_rules", {})
    for field_name, field_rules in rules.items():
        if field_name in data and data[field_name]:
            value = data[field_name]

            if "min_length" in field_rules and len(str(value)) < field_rules["min_length"]:
                errors.append({
                    "field": field_name,
                    "message": f"Minimo de {field_rules['min_length']} caracteres"
                })

            if "max_length" in field_rules and len(str(value)) > field_rules["max_length"]:
                errors.append({
                    "field": field_name,
                    "message": f"Maximo de {field_rules['max_length']} caracteres"
                })

    return {
        "valid": len(errors) == 0,
        "errors": errors
    }


@wizard_router.get("/progress/{wizard_id}/{user_id}")
async def get_progress(wizard_id: str, user_id: str):
    """Retorna o progresso do usuario no wizard"""
    key = f"{wizard_id}:{user_id}"
    if key not in wizard_progress:
        wizard_progress[key] = {
            "wizard_id": wizard_id,
            "user_id": user_id,
            "current_step": 0,
            "completed_steps": [],
            "data": {},
            "started_at": None,
            "completed_at": None
        }
    return wizard_progress[key]


@wizard_router.post("/progress/{wizard_id}/{user_id}/save")
async def save_progress(wizard_id: str, user_id: str, step_index: int, data: Dict[str, Any]):
    """Salva o progresso do usuario no wizard"""
    key = f"{wizard_id}:{user_id}"

    if key not in wizard_progress:
        wizard_progress[key] = {
            "wizard_id": wizard_id,
            "user_id": user_id,
            "current_step": 0,
            "completed_steps": [],
            "data": {},
            "started_at": datetime.utcnow().isoformat(),
            "completed_at": None
        }

    # Update data
    wizard_progress[key]["data"].update(data)
    wizard_progress[key]["current_step"] = step_index

    # Mark step as completed
    step_id = WIZARDS[wizard_id]["steps"][step_index]["step_id"]
    if step_id not in wizard_progress[key]["completed_steps"]:
        wizard_progress[key]["completed_steps"].append(step_id)

    return wizard_progress[key]


@wizard_router.post("/progress/{wizard_id}/{user_id}/complete")
async def complete_wizard(wizard_id: str, user_id: str):
    """Marca o wizard como completo"""
    key = f"{wizard_id}:{user_id}"

    if key in wizard_progress:
        wizard_progress[key]["completed_at"] = datetime.utcnow().isoformat()
        return {
            "success": True,
            "data": wizard_progress[key]["data"],
            "message": "Wizard completado com sucesso!"
        }

    return JSONResponse(
        status_code=404,
        content={"error": "Progresso nao encontrado"}
    )


@wizard_router.delete("/progress/{wizard_id}/{user_id}")
async def reset_progress(wizard_id: str, user_id: str):
    """Reinicia o progresso do wizard"""
    key = f"{wizard_id}:{user_id}"
    if key in wizard_progress:
        del wizard_progress[key]
    return {"success": True, "message": "Progresso reiniciado"}


# =============================================================================
# HTML TEMPLATE
# =============================================================================

WIZARD_TEMPLATE = '''<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Wizard - Fabrica de Agentes</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="https://unpkg.com/lucide-static@latest/font/lucide.css">
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
            background: #F3F4F6;
            min-height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 20px;
        }

        /* Wizard Container */
        .wizard-container {
            background: white;
            border-radius: 20px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.1);
            max-width: 640px;
            width: 100%;
            overflow: hidden;
        }

        /* Progress Bar */
        .wizard-progress {
            background: #F3F4F6;
            padding: 24px 32px;
            border-bottom: 1px solid #E5E7EB;
        }

        .progress-steps {
            display: flex;
            justify-content: space-between;
            position: relative;
        }

        .progress-steps::before {
            content: '';
            position: absolute;
            top: 16px;
            left: 20px;
            right: 20px;
            height: 2px;
            background: #E5E7EB;
            z-index: 0;
        }

        .progress-step {
            display: flex;
            flex-direction: column;
            align-items: center;
            position: relative;
            z-index: 1;
        }

        .step-indicator {
            width: 32px;
            height: 32px;
            border-radius: 50%;
            background: white;
            border: 2px solid #E5E7EB;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 13px;
            font-weight: 600;
            color: #9CA3AF;
            transition: all 0.3s;
        }

        .progress-step.active .step-indicator {
            background: #003B4A;
            border-color: #003B4A;
            color: white;
        }

        .progress-step.completed .step-indicator {
            background: #10B981;
            border-color: #10B981;
            color: white;
        }

        .step-label {
            margin-top: 8px;
            font-size: 11px;
            color: #9CA3AF;
            text-align: center;
            max-width: 80px;
        }

        .progress-step.active .step-label {
            color: #003B4A;
            font-weight: 500;
        }

        /* Wizard Content */
        .wizard-content {
            padding: 32px;
        }

        .wizard-header {
            margin-bottom: 32px;
        }

        .wizard-title {
            font-size: 24px;
            font-weight: 700;
            color: #1F2937;
            margin-bottom: 8px;
        }

        .wizard-description {
            color: #6B7280;
            line-height: 1.5;
        }

        .wizard-help {
            background: #EEF2FF;
            border-left: 3px solid #4F46E5;
            padding: 12px 16px;
            border-radius: 0 8px 8px 0;
            margin-top: 16px;
            font-size: 13px;
            color: #4338CA;
            display: flex;
            align-items: flex-start;
            gap: 10px;
        }

        /* Form Fields */
        .wizard-form {
            margin-bottom: 32px;
        }

        .form-group {
            margin-bottom: 24px;
        }

        .form-label {
            display: block;
            font-size: 14px;
            font-weight: 500;
            color: #374151;
            margin-bottom: 8px;
        }

        .form-label .required {
            color: #EF4444;
            margin-left: 4px;
        }

        .form-input {
            width: 100%;
            padding: 12px 16px;
            border: 2px solid #E5E7EB;
            border-radius: 10px;
            font-size: 15px;
            transition: all 0.2s;
        }

        .form-input:focus {
            outline: none;
            border-color: #003B4A;
        }

        .form-input.error {
            border-color: #EF4444;
        }

        textarea.form-input {
            resize: vertical;
            min-height: 100px;
        }

        .form-help {
            font-size: 12px;
            color: #9CA3AF;
            margin-top: 6px;
        }

        .form-error {
            font-size: 12px;
            color: #EF4444;
            margin-top: 6px;
        }

        /* Card Selection */
        .card-options {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
            gap: 12px;
        }

        .card-option {
            padding: 16px;
            border: 2px solid #E5E7EB;
            border-radius: 12px;
            cursor: pointer;
            transition: all 0.2s;
            text-align: center;
        }

        .card-option:hover {
            border-color: #003B4A;
            background: #F9FAFB;
        }

        .card-option.selected {
            border-color: #003B4A;
            background: #EEF2FF;
        }

        .card-option-icon {
            width: 40px;
            height: 40px;
            background: #F3F4F6;
            border-radius: 10px;
            display: flex;
            align-items: center;
            justify-content: center;
            margin: 0 auto 12px;
            color: #003B4A;
        }

        .card-option.selected .card-option-icon {
            background: #003B4A;
            color: white;
        }

        .card-option-label {
            font-size: 14px;
            font-weight: 500;
            color: #1F2937;
            margin-bottom: 4px;
        }

        .card-option-desc {
            font-size: 12px;
            color: #9CA3AF;
        }

        /* Slider */
        .slider-container {
            padding: 12px 0;
        }

        .slider-track {
            display: flex;
            justify-content: space-between;
            margin-bottom: 16px;
        }

        .slider-option {
            text-align: center;
            cursor: pointer;
            padding: 8px;
            border-radius: 8px;
            transition: all 0.2s;
            flex: 1;
        }

        .slider-option:hover {
            background: #F3F4F6;
        }

        .slider-option.selected {
            background: #003B4A;
            color: white;
        }

        .slider-value {
            font-size: 20px;
            font-weight: 700;
        }

        .slider-label {
            font-size: 11px;
            opacity: 0.8;
        }

        /* Checklist */
        .checklist-container {
            border: 2px solid #E5E7EB;
            border-radius: 10px;
            overflow: hidden;
        }

        .checklist-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px 16px;
            border-bottom: 1px solid #E5E7EB;
        }

        .checklist-item:last-child {
            border-bottom: none;
        }

        .checklist-item input {
            flex-shrink: 0;
        }

        .checklist-item-input {
            flex: 1;
            border: none;
            font-size: 14px;
            background: transparent;
        }

        .checklist-item-input:focus {
            outline: none;
        }

        .checklist-add {
            padding: 12px 16px;
            background: #F9FAFB;
            color: #003B4A;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 8px;
            transition: all 0.2s;
        }

        .checklist-add:hover {
            background: #EEF2FF;
        }

        /* Summary */
        .summary-section {
            background: #F9FAFB;
            border-radius: 12px;
            padding: 20px;
            margin-bottom: 16px;
        }

        .summary-title {
            font-size: 12px;
            font-weight: 600;
            color: #9CA3AF;
            text-transform: uppercase;
            margin-bottom: 12px;
        }

        .summary-item {
            display: flex;
            justify-content: space-between;
            padding: 8px 0;
            border-bottom: 1px solid #E5E7EB;
        }

        .summary-item:last-child {
            border-bottom: none;
        }

        .summary-label {
            color: #6B7280;
            font-size: 14px;
        }

        .summary-value {
            color: #1F2937;
            font-size: 14px;
            font-weight: 500;
        }

        /* Actions */
        .wizard-actions {
            display: flex;
            justify-content: space-between;
            padding-top: 24px;
            border-top: 1px solid #E5E7EB;
        }

        .btn {
            padding: 12px 24px;
            border-radius: 10px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .btn-back {
            background: white;
            border: 2px solid #E5E7EB;
            color: #6B7280;
        }

        .btn-back:hover {
            border-color: #9CA3AF;
            color: #374151;
        }

        .btn-next {
            background: #003B4A;
            border: none;
            color: white;
        }

        .btn-next:hover {
            background: #004d5e;
        }

        .btn-next:disabled {
            opacity: 0.5;
            cursor: not-allowed;
        }

        .btn-complete {
            background: #10B981;
            border: none;
            color: white;
        }

        .btn-complete:hover {
            background: #059669;
        }
    </style>
</head>
<body>
    <div class="wizard-container">
        <!-- Progress -->
        <div class="wizard-progress">
            <div class="progress-steps" id="progressSteps">
                <!-- Steps populated by JS -->
            </div>
        </div>

        <!-- Content -->
        <div class="wizard-content">
            <div class="wizard-header">
                <h2 class="wizard-title" id="stepTitle">Titulo do Passo</h2>
                <p class="wizard-description" id="stepDescription">Descricao do passo</p>
                <div class="wizard-help" id="stepHelp">
                    <i class="lucide-lightbulb" style="width: 18px; height: 18px; flex-shrink: 0;"></i>
                    <span id="stepHelpText">Dica</span>
                </div>
            </div>

            <form class="wizard-form" id="wizardForm">
                <!-- Fields populated by JS -->
            </form>

            <div class="wizard-actions">
                <button type="button" class="btn btn-back" id="btnBack" onclick="previousStep()">
                    <i class="lucide-arrow-left" style="width: 16px; height: 16px;"></i>
                    Voltar
                </button>
                <button type="button" class="btn btn-next" id="btnNext" onclick="nextStep()">
                    Proximo
                    <i class="lucide-arrow-right" style="width: 16px; height: 16px;"></i>
                </button>
            </div>
        </div>
    </div>

    <script>
        // State
        let currentWizard = null;
        let currentStep = 0;
        let wizardData = {};
        const userId = 'demo-user-' + Date.now();

        // Get wizard ID from URL params
        const urlParams = new URLSearchParams(window.location.search);
        const wizardId = urlParams.get('wizard') || 'create_project';

        // Initialize
        document.addEventListener('DOMContentLoaded', async () => {
            await loadWizard(wizardId);
        });

        // Load wizard configuration
        async function loadWizard(id) {
            try {
                const response = await fetch(`/api/wizards/${id}`);
                currentWizard = await response.json();

                // Load saved progress
                const progressResponse = await fetch(`/api/wizards/progress/${id}/${userId}`);
                const progress = await progressResponse.json();

                wizardData = progress.data || {};
                currentStep = progress.current_step || 0;

                renderProgress();
                renderStep();
            } catch (error) {
                console.error('Error loading wizard:', error);
            }
        }

        // Render progress bar
        function renderProgress() {
            const container = document.getElementById('progressSteps');
            container.innerHTML = currentWizard.steps.map((step, index) => {
                let className = 'progress-step';
                if (index < currentStep) className += ' completed';
                if (index === currentStep) className += ' active';

                return `
                    <div class="${className}">
                        <div class="step-indicator">
                            ${index < currentStep ? '<i class="lucide-check" style="width: 14px; height: 14px;"></i>' : index + 1}
                        </div>
                        <span class="step-label">${step.title}</span>
                    </div>
                `;
            }).join('');
        }

        // Render current step
        function renderStep() {
            const step = currentWizard.steps[currentStep];

            document.getElementById('stepTitle').textContent = step.title;
            document.getElementById('stepDescription').textContent = step.description;

            const helpEl = document.getElementById('stepHelp');
            if (step.help_text) {
                helpEl.style.display = 'flex';
                document.getElementById('stepHelpText').textContent = step.help_text;
            } else {
                helpEl.style.display = 'none';
            }

            // Render fields or summary
            const form = document.getElementById('wizardForm');
            if (step.is_summary) {
                form.innerHTML = renderSummary();
            } else {
                form.innerHTML = step.fields.map(field => renderField(field)).join('');
            }

            // Update buttons
            const btnBack = document.getElementById('btnBack');
            const btnNext = document.getElementById('btnNext');

            btnBack.style.display = currentStep === 0 ? 'none' : 'flex';

            if (currentStep === currentWizard.steps.length - 1) {
                btnNext.innerHTML = `
                    Concluir
                    <i class="lucide-check" style="width: 16px; height: 16px;"></i>
                `;
                btnNext.className = 'btn btn-complete';
            } else {
                btnNext.innerHTML = `
                    Proximo
                    <i class="lucide-arrow-right" style="width: 16px; height: 16px;"></i>
                `;
                btnNext.className = 'btn btn-next';
            }
        }

        // Render a form field
        function renderField(field) {
            const value = wizardData[field.name] || field.default || '';
            const required = field.required ? '<span class="required">*</span>' : '';

            switch (field.type) {
                case 'text':
                case 'url':
                case 'password':
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <input type="${field.type}" class="form-input" name="${field.name}"
                                   value="${value}" placeholder="${field.placeholder || ''}"
                                   onchange="updateData('${field.name}', this.value)">
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                case 'textarea':
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <textarea class="form-input" name="${field.name}"
                                      placeholder="${field.placeholder || ''}"
                                      rows="${field.rows || 4}"
                                      onchange="updateData('${field.name}', this.value)">${value}</textarea>
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                case 'number':
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <input type="number" class="form-input" name="${field.name}"
                                   value="${value || field.default || 1}"
                                   min="${field.min || 0}" max="${field.max || 100}"
                                   onchange="updateData('${field.name}', parseInt(this.value))">
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                case 'date':
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <input type="date" class="form-input" name="${field.name}"
                                   value="${value}"
                                   onchange="updateData('${field.name}', this.value)">
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                case 'select':
                    if (field.display === 'cards') {
                        return `
                            <div class="form-group">
                                <label class="form-label">${field.label}${required}</label>
                                <div class="card-options">
                                    ${field.options.map(opt => `
                                        <div class="card-option ${value === opt.value ? 'selected' : ''}"
                                             onclick="selectCardOption('${field.name}', '${opt.value}', this)">
                                            <div class="card-option-icon">
                                                <i class="lucide-${opt.icon || 'circle'}" style="width: 20px; height: 20px;"></i>
                                            </div>
                                            <div class="card-option-label">${opt.label}</div>
                                            ${opt.description ? `<div class="card-option-desc">${opt.description}</div>` : ''}
                                        </div>
                                    `).join('')}
                                </div>
                            </div>
                        `;
                    } else {
                        return `
                            <div class="form-group">
                                <label class="form-label">${field.label}${required}</label>
                                <select class="form-input" name="${field.name}"
                                        onchange="updateData('${field.name}', this.value)">
                                    ${field.options.map(opt => `
                                        <option value="${opt.value}" ${value === opt.value ? 'selected' : ''}>
                                            ${opt.label}
                                        </option>
                                    `).join('')}
                                </select>
                                ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                            </div>
                        `;
                    }

                case 'slider':
                    const selectedValue = value || field.default || field.options[0].value;
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <div class="slider-container">
                                <div class="slider-track">
                                    ${field.options.map(opt => `
                                        <div class="slider-option ${selectedValue === opt.value ? 'selected' : ''}"
                                             onclick="selectSliderOption('${field.name}', ${opt.value}, this)">
                                            <div class="slider-value">${opt.value}</div>
                                            <div class="slider-label">${opt.label.split('(')[0]}</div>
                                        </div>
                                    `).join('')}
                                </div>
                            </div>
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                case 'checkbox':
                    const checked = value === true || value === 'true' ? 'checked' : '';
                    return `
                        <div class="form-group" style="display: flex; align-items: center; gap: 12px;">
                            <input type="checkbox" id="${field.name}" name="${field.name}"
                                   ${checked} style="width: 20px; height: 20px; accent-color: #003B4A;"
                                   onchange="updateData('${field.name}', this.checked)">
                            <label for="${field.name}" style="margin: 0; cursor: pointer;">${field.label}</label>
                        </div>
                    `;

                case 'checklist':
                    const items = Array.isArray(value) ? value : [];
                    return `
                        <div class="form-group">
                            <label class="form-label">${field.label}${required}</label>
                            <div class="checklist-container" id="checklist-${field.name}">
                                ${items.map((item, i) => `
                                    <div class="checklist-item">
                                        <input type="text" class="checklist-item-input"
                                               value="${item}" placeholder="${field.placeholder || ''}"
                                               onchange="updateChecklistItem('${field.name}', ${i}, this.value)">
                                        <button type="button" onclick="removeChecklistItem('${field.name}', ${i})"
                                                style="background: none; border: none; color: #EF4444; cursor: pointer;">
                                            <i class="lucide-x" style="width: 16px; height: 16px;"></i>
                                        </button>
                                    </div>
                                `).join('')}
                                <div class="checklist-add" onclick="addChecklistItem('${field.name}')">
                                    <i class="lucide-plus" style="width: 16px; height: 16px;"></i>
                                    ${field.add_button || 'Adicionar item'}
                                </div>
                            </div>
                            ${field.help ? `<p class="form-help">${field.help}</p>` : ''}
                        </div>
                    `;

                default:
                    return '';
            }
        }

        // Render summary
        function renderSummary() {
            const steps = currentWizard.steps.filter(s => !s.is_summary);
            return steps.map(step => {
                const items = step.fields.map(field => {
                    let value = wizardData[field.name];
                    if (value === undefined || value === null || value === '') return null;

                    // Format value for display
                    if (field.type === 'select' && field.options) {
                        const opt = field.options.find(o => o.value === value);
                        value = opt ? opt.label : value;
                    } else if (Array.isArray(value)) {
                        value = value.join(', ') || '-';
                    } else if (typeof value === 'boolean') {
                        value = value ? 'Sim' : 'Nao';
                    }

                    return `
                        <div class="summary-item">
                            <span class="summary-label">${field.label}</span>
                            <span class="summary-value">${value}</span>
                        </div>
                    `;
                }).filter(Boolean).join('');

                if (!items) return '';

                return `
                    <div class="summary-section">
                        <div class="summary-title">${step.title}</div>
                        ${items}
                    </div>
                `;
            }).join('');
        }

        // Update data
        function updateData(name, value) {
            wizardData[name] = value;
        }

        // Select card option
        function selectCardOption(name, value, element) {
            document.querySelectorAll(`.card-option`).forEach(el => el.classList.remove('selected'));
            element.classList.add('selected');
            updateData(name, value);
        }

        // Select slider option
        function selectSliderOption(name, value, element) {
            const container = element.parentElement;
            container.querySelectorAll('.slider-option').forEach(el => el.classList.remove('selected'));
            element.classList.add('selected');
            updateData(name, value);
        }

        // Checklist functions
        function addChecklistItem(name) {
            if (!wizardData[name]) wizardData[name] = [];
            wizardData[name].push('');
            renderStep();
        }

        function updateChecklistItem(name, index, value) {
            if (!wizardData[name]) wizardData[name] = [];
            wizardData[name][index] = value;
        }

        function removeChecklistItem(name, index) {
            if (wizardData[name]) {
                wizardData[name].splice(index, 1);
                renderStep();
            }
        }

        // Navigation
        async function nextStep() {
            // Validate current step
            const validation = await validateStep();
            if (!validation.valid) {
                showErrors(validation.errors);
                return;
            }

            // Save progress
            await saveProgress();

            if (currentStep === currentWizard.steps.length - 1) {
                // Complete wizard
                await completeWizard();
            } else {
                currentStep++;
                renderProgress();
                renderStep();
            }
        }

        function previousStep() {
            if (currentStep > 0) {
                currentStep--;
                renderProgress();
                renderStep();
            }
        }

        // Validate step
        async function validateStep() {
            try {
                const response = await fetch(
                    `/api/wizards/${currentWizard.wizard_id}/validate/${currentStep}`,
                    {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(wizardData)
                    }
                );
                return await response.json();
            } catch (error) {
                return { valid: true, errors: [] };
            }
        }

        // Show validation errors
        function showErrors(errors) {
            // Clear previous errors
            document.querySelectorAll('.form-error').forEach(el => el.remove());
            document.querySelectorAll('.form-input.error').forEach(el => el.classList.remove('error'));

            errors.forEach(err => {
                const input = document.querySelector(`[name="${err.field}"]`);
                if (input) {
                    input.classList.add('error');
                    const errorEl = document.createElement('p');
                    errorEl.className = 'form-error';
                    errorEl.textContent = err.message;
                    input.parentElement.appendChild(errorEl);
                }
            });
        }

        // Save progress
        async function saveProgress() {
            try {
                await fetch(
                    `/api/wizards/progress/${currentWizard.wizard_id}/${userId}/save?step_index=${currentStep}`,
                    {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(wizardData)
                    }
                );
            } catch (error) {
                console.error('Error saving progress:', error);
            }
        }

        // Complete wizard
        async function completeWizard() {
            try {
                const response = await fetch(
                    `/api/wizards/progress/${currentWizard.wizard_id}/${userId}/complete`,
                    { method: 'POST' }
                );
                const result = await response.json();

                if (result.success) {
                    alert('Wizard completado com sucesso!');
                    // Redirect or callback
                    window.location.href = '/';
                }
            } catch (error) {
                console.error('Error completing wizard:', error);
            }
        }
    </script>
</body>
</html>'''


@wizard_router.get("/", response_class=HTMLResponse)
async def get_wizard_page():
    """Retorna a pagina do wizard"""
    return HTMLResponse(content=WIZARD_TEMPLATE)


# =============================================================================
# REGISTRATION FUNCTION
# =============================================================================

def register_wizard_endpoints(app):
    """Registra os endpoints dos wizards no app FastAPI"""
    app.include_router(wizard_router)

    @app.get("/wizard", response_class=HTMLResponse)
    async def wizard_page():
        return HTMLResponse(content=WIZARD_TEMPLATE)

    print("[Dashboard] Wizard endpoints registered")
