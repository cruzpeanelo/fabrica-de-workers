#!/usr/bin/env python3
"""
Start Agent - Script de inicializacao de agente individual.

Este script e executado em cada terminal Git Bash spawneado pelo Orquestrador.
Ele carrega o prompt do agente e executa Claude Code para processar a task.

Uso:
    python start_agent.py --agent BACK --task-id task_BACK_1234567890
    python start_agent.py --agent FRONT --issue 123
"""

import argparse
import os
import sys
import json
import subprocess
from pathlib import Path
from datetime import datetime

# Diretorio base do projeto
BASE_PATH = Path(__file__).parent

# Adicionar ao path para imports
sys.path.insert(0, str(BASE_PATH))

# Importar activity logger
try:
    from factory.core.activity_logger import (
        log_agent_start, log_agent_stop, log_task_start, log_task_complete,
        log_thinking, log_file_action, log_code_generate, log_error, log_activity
    )
    ACTIVITY_LOGGING = True
except ImportError:
    ACTIVITY_LOGGING = False
    def log_activity(*args, **kwargs): pass
    def log_agent_start(*args, **kwargs): pass
    def log_agent_stop(*args, **kwargs): pass
    def log_task_start(*args, **kwargs): pass
    def log_task_complete(*args, **kwargs): pass
    def log_thinking(*args, **kwargs): pass
    def log_file_action(*args, **kwargs): pass
    def log_code_generate(*args, **kwargs): pass
    def log_error(*args, **kwargs): pass


PROMPTS_PATH = BASE_PATH / "prompts"
STATE_PATH = BASE_PATH / "factory" / "state"
TASKS_PATH = STATE_PATH / "tasks"
RESULTS_PATH = STATE_PATH / "results"


# Mapeamento de tipos de agente para arquivos de prompt
AGENT_PROMPTS = {
    "ORCH": "agente_orquestrador.md",
    "ARCH": "agente_arquiteto.md",
    "BACK": "agente_backend.md",
    "FRONT": "agente_frontend.md",
    "DEVOPS": "agente_devops.md",
    "SEC": "agente_security.md",
    "QA": "agente_qa.md",
    "PROD": "agente_produto.md",
    "INOV": "agente_inovacao.md",
    "FIN": "agente_financeiro.md",
    "GROWTH": "agente_growth.md",
}


def load_prompt(agent_type: str) -> str:
    """Carrega o prompt do agente."""
    prompt_file = PROMPTS_PATH / AGENT_PROMPTS.get(agent_type, "")

    if not prompt_file.exists():
        print(f"Erro: Arquivo de prompt nao encontrado: {prompt_file}")
        sys.exit(1)

    with open(prompt_file, 'r', encoding='utf-8') as f:
        return f.read()


def load_task(agent_type: str, task_id: str = None) -> dict:
    """Carrega a task do arquivo JSON."""
    task_file = TASKS_PATH / f"{agent_type}_task.json"

    if not task_file.exists():
        return None

    try:
        with open(task_file, 'r', encoding='utf-8') as f:
            task = json.load(f)

        # Verificar se e a task correta
        if task_id and task.get("task_id") != task_id:
            print(f"Aviso: Task ID nao corresponde. Esperado: {task_id}, Encontrado: {task.get('task_id')}")

        return task
    except Exception as e:
        print(f"Erro ao carregar task: {e}")
        return None


def save_result(agent_type: str, result: dict):
    """Salva o resultado no arquivo JSON."""
    RESULTS_PATH.mkdir(parents=True, exist_ok=True)
    result_file = RESULTS_PATH / f"{agent_type}_result.json"

    try:
        with open(result_file, 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2, ensure_ascii=False)
        print(f"Resultado salvo em: {result_file}")
    except Exception as e:
        print(f"Erro ao salvar resultado: {e}")


def build_agent_prompt(agent_type: str, base_prompt: str, task: dict) -> str:
    """Constroi o prompt completo para o agente."""
    prompt_parts = [
        f"# Voce e o Agente [{agent_type}]",
        "",
        "## Sua Identidade e Instrucoes",
        base_prompt,
        "",
        "---",
        "",
        "## Task Atual",
        "",
    ]

    if task:
        prompt_parts.extend([
            f"**Task ID:** {task.get('task_id', 'N/A')}",
            f"**Titulo:** {task.get('title', 'N/A')}",
            f"**Prioridade:** {task.get('priority', 'medium')}",
            "",
            "**Descricao:**",
            task.get('description', 'Sem descricao'),
            "",
        ])

        if task.get('issue_number'):
            prompt_parts.extend([
                f"**Issue GitHub:** #{task.get('issue_number')}",
                "",
            ])

        if task.get('context'):
            prompt_parts.extend([
                "**Contexto Adicional:**",
                json.dumps(task.get('context'), indent=2, ensure_ascii=False),
                "",
            ])

    prompt_parts.extend([
        "---",
        "",
        "## Instrucoes de Execucao",
        "",
        "1. Analise a task acima",
        "2. Execute o trabalho necessario seguindo suas instrucoes de agente",
        "3. Faca commits com o prefixo correto",
        "4. Ao finalizar, informe o status e proximo passo (handoff)",
        "",
        "## Formato de Resposta Final",
        "",
        "Ao concluir, sua ultima mensagem deve conter:",
        "```",
        "STATUS: completed | failed | blocked",
        "HANDOFF: [AGENTE] ou NONE",
        "ARQUIVOS: lista de arquivos alterados",
        "RESUMO: breve descricao do que foi feito",
        "```",
    ])

    return "\n".join(prompt_parts)


def run_claude(prompt: str, agent_type: str, task_id: str = None) -> dict:
    """Executa Claude Code com o prompt do agente."""
    print(f"\n{'='*60}")
    print(f" AGENTE [{agent_type}] INICIANDO ")
    print(f"{'='*60}\n")

    # Log inicio
    log_activity(agent_type, "thinking", "Analisando task...",
                 description="Preparando para executar Claude Code", task_id=task_id)

    # Salvar prompt em arquivo temporario
    prompt_file = STATE_PATH / f"prompt_{agent_type}.md"
    prompt_file.parent.mkdir(parents=True, exist_ok=True)

    with open(prompt_file, 'w', encoding='utf-8') as f:
        f.write(prompt)

    log_file_action(agent_type, "write", str(prompt_file), task_id=task_id)

    # Construir comando
    cmd = [
        "claude",
        "--dangerously-skip-permissions",
        "-p", str(prompt_file),
    ]

    log_activity(agent_type, "task_progress", "Executando Claude Code...",
                 description="Processo Claude iniciado", task_id=task_id)

    try:
        # Executar Claude Code de forma interativa
        # O processo herda stdin/stdout para interacao
        result = subprocess.run(
            cmd,
            cwd=str(BASE_PATH),
            # Nao capturar output para permitir interacao
        )

        if result.returncode == 0:
            log_activity(agent_type, "task_progress", "Claude Code finalizado com sucesso",
                        task_id=task_id)
        else:
            log_activity(agent_type, "warning", f"Claude Code finalizou com codigo {result.returncode}",
                        task_id=task_id)

        return {
            "success": result.returncode == 0,
            "returncode": result.returncode
        }

    except FileNotFoundError:
        error_msg = "Claude CLI nao encontrado. Verifique se esta instalado."
        print(f"Erro: {error_msg}")
        log_error(agent_type, error_msg, task_id=task_id)
        return {"success": False, "error": "Claude CLI not found"}
    except Exception as e:
        print(f"Erro ao executar Claude: {e}")
        log_error(agent_type, str(e), task_id=task_id)
        return {"success": False, "error": str(e)}


def parse_output_for_result(output: str) -> dict:
    """Tenta extrair resultado estruturado do output."""
    result = {
        "status": "completed",
        "handoff_to": None,
        "files_changed": [],
        "summary": ""
    }

    lines = output.split('\n')
    for line in lines:
        line_lower = line.lower().strip()

        if line_lower.startswith("status:"):
            status = line.split(":", 1)[1].strip().lower()
            if status in ["completed", "failed", "blocked"]:
                result["status"] = status

        elif line_lower.startswith("handoff:"):
            handoff = line.split(":", 1)[1].strip()
            if handoff.upper() != "NONE":
                result["handoff_to"] = handoff

        elif line_lower.startswith("arquivos:"):
            files = line.split(":", 1)[1].strip()
            result["files_changed"] = [f.strip() for f in files.split(",") if f.strip()]

        elif line_lower.startswith("resumo:"):
            result["summary"] = line.split(":", 1)[1].strip()

    return result


def main():
    parser = argparse.ArgumentParser(description="Inicializa um agente da Fabrica")
    parser.add_argument(
        "--agent", "-a",
        type=str,
        required=True,
        choices=list(AGENT_PROMPTS.keys()),
        help="Tipo do agente (BACK, FRONT, QA, etc.)"
    )
    parser.add_argument(
        "--task-id", "-t",
        type=str,
        help="ID da task a executar"
    )
    parser.add_argument(
        "--issue", "-i",
        type=int,
        help="Numero da issue GitHub"
    )
    parser.add_argument(
        "--interactive",
        action="store_true",
        default=True,
        help="Modo interativo (padrao)"
    )

    args = parser.parse_args()

    agent_type = args.agent.upper()

    print(f"\n{'#'*60}")
    print(f"#  FABRICA DE AGENTES - Agente [{agent_type}]")
    print(f"#  Iniciando em: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"{'#'*60}\n")

    # Log inicio do agente
    log_agent_start(agent_type, args.task_id)

    # Carregar prompt base do agente
    base_prompt = load_prompt(agent_type)
    print(f"Prompt do agente carregado: {AGENT_PROMPTS[agent_type]}")

    # Carregar task
    task = load_task(agent_type, args.task_id)

    if task:
        print(f"Task carregada: {task.get('title', 'N/A')}")
    elif args.issue:
        # Criar task a partir do numero da issue
        task = {
            "task_id": f"task_{agent_type}_{int(datetime.now().timestamp())}",
            "agent_type": agent_type,
            "issue_number": args.issue,
            "title": f"Issue #{args.issue}",
            "description": f"Processar issue #{args.issue} do GitHub",
            "priority": "medium",
            "created_at": datetime.now().isoformat(),
            "status": "pending"
        }
        print(f"Task criada para issue #{args.issue}")
    else:
        print("Nenhuma task especifica. Agente em modo livre.")
        task = {
            "task_id": f"task_{agent_type}_{int(datetime.now().timestamp())}",
            "agent_type": agent_type,
            "title": "Modo livre",
            "description": "Verificar issues pendentes e trabalhar na proxima tarefa disponivel",
            "priority": "medium",
            "created_at": datetime.now().isoformat(),
            "status": "pending"
        }

    # Construir prompt completo
    full_prompt = build_agent_prompt(agent_type, base_prompt, task)

    # Log inicio da task
    task_id = task.get("task_id")
    log_task_start(agent_type, task_id, task.get("title", "Task"))

    # Executar Claude
    execution_result = run_claude(full_prompt, agent_type, task_id)

    # Salvar resultado
    result = {
        "task_id": task.get("task_id"),
        "agent_type": agent_type,
        "status": "completed" if execution_result.get("success") else "failed",
        "files_changed": [],
        "commit_hash": None,
        "handoff_to": None,
        "error_message": execution_result.get("error"),
        "completed_at": datetime.now().isoformat(),
        "output": ""
    }

    save_result(agent_type, result)

    # Log conclusao
    if execution_result.get("success"):
        log_task_complete(agent_type, task_id)
    else:
        log_error(agent_type, execution_result.get("error", "Erro desconhecido"), task_id)

    # Log fim do agente
    log_agent_stop(agent_type)

    print(f"\n{'='*60}")
    print(f" AGENTE [{agent_type}] FINALIZADO ")
    print(f"{'='*60}\n")


if __name__ == "__main__":
    main()
