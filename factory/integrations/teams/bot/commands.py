# -*- coding: utf-8 -*-
"""
Bot Command Handler
===================
Processador de comandos do bot do Microsoft Teams.
Define e executa comandos disponiveis para usuarios.
"""

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Awaitable
import asyncio

from .cards import BotCardBuilder

logger = logging.getLogger(__name__)


class CommandType(str, Enum):
    """Tipos de comando"""
    STATUS = "status"
    LIST = "listar"
    PROJECT = "projeto"
    STORY = "story"
    DEVELOP = "desenvolver"
    HELP = "ajuda"
    CREATE = "criar"
    APPROVE = "aprovar"
    REJECT = "rejeitar"


@dataclass
class CommandContext:
    """Contexto de execucao de comando"""
    user_id: str
    user_name: str
    team_id: str = ""
    channel_id: str = ""
    chat_id: str = ""
    is_channel: bool = False
    raw_text: str = ""
    mentioned_users: List[str] = field(default_factory=list)
    attachments: List[Dict] = field(default_factory=list)


@dataclass
class CommandResult:
    """Resultado da execucao de comando"""
    success: bool
    message: str = ""
    card: Optional[Dict[str, Any]] = None
    data: Dict[str, Any] = field(default_factory=dict)
    error: str = ""

    @staticmethod
    def ok(message: str = "", card: Dict = None, data: Dict = None) -> 'CommandResult':
        """Cria resultado de sucesso"""
        return CommandResult(
            success=True,
            message=message,
            card=card,
            data=data or {}
        )

    @staticmethod
    def error(message: str, suggestion: str = "") -> 'CommandResult':
        """Cria resultado de erro"""
        return CommandResult(
            success=False,
            error=message,
            data={"suggestion": suggestion} if suggestion else {}
        )


@dataclass
class CommandDefinition:
    """Definicao de um comando"""
    name: str
    aliases: List[str]
    description: str
    usage: str
    examples: List[str]
    handler: Callable[[CommandContext, List[str]], Awaitable[CommandResult]]
    requires_args: bool = False
    min_args: int = 0
    max_args: int = 10


class BotCommandHandler:
    """
    Processador de comandos do bot do Teams.

    Processa comandos enviados pelos usuarios e retorna
    respostas apropriadas via cards adaptativos.

    Comandos disponiveis:
    - /fabrica status - Status geral
    - /fabrica projeto [nome] - Criar projeto
    - /fabrica listar - Listar projetos
    - /fabrica status [id] - Status de projeto
    - /fabrica desenvolver [id] - Iniciar desenvolvimento
    - /fabrica ajuda - Mostra comandos

    Exemplo:
        handler = BotCommandHandler()

        # Registra callback para acoes
        handler.set_action_callback("create_project", create_project_func)

        # Processa comando
        context = CommandContext(user_id="xxx", user_name="Joao")
        result = await handler.process("/fabrica status", context)
    """

    def __init__(self):
        self._commands: Dict[str, CommandDefinition] = {}
        self._card_builder = BotCardBuilder()
        self._action_callbacks: Dict[str, Callable] = {}

        # Registra comandos padrao
        self._register_default_commands()

    def _register_default_commands(self):
        """Registra comandos padrao"""

        # Status
        self.register_command(CommandDefinition(
            name="status",
            aliases=["s", "st"],
            description="Mostra status da plataforma ou projeto",
            usage="/fabrica status [projeto_id]",
            examples=["/fabrica status", "/fabrica status PROJ-001"],
            handler=self._handle_status
        ))

        # Listar
        self.register_command(CommandDefinition(
            name="listar",
            aliases=["l", "list", "ls"],
            description="Lista projetos ou stories",
            usage="/fabrica listar [projetos|stories] [projeto_id]",
            examples=["/fabrica listar", "/fabrica listar stories PROJ-001"],
            handler=self._handle_list
        ))

        # Projeto
        self.register_command(CommandDefinition(
            name="projeto",
            aliases=["p", "project", "proj"],
            description="Cria novo projeto",
            usage="/fabrica projeto <nome>",
            examples=["/fabrica projeto Sistema de Vendas"],
            handler=self._handle_project,
            requires_args=True,
            min_args=1
        ))

        # Desenvolver
        self.register_command(CommandDefinition(
            name="desenvolver",
            aliases=["d", "dev", "develop"],
            description="Inicia desenvolvimento de projeto/story",
            usage="/fabrica desenvolver <id>",
            examples=["/fabrica desenvolver PROJ-001", "/fabrica desenvolver STR-001"],
            handler=self._handle_develop,
            requires_args=True,
            min_args=1
        ))

        # Ajuda
        self.register_command(CommandDefinition(
            name="ajuda",
            aliases=["h", "help", "?"],
            description="Mostra comandos disponiveis",
            usage="/fabrica ajuda [comando]",
            examples=["/fabrica ajuda", "/fabrica ajuda status"],
            handler=self._handle_help
        ))

        # Story
        self.register_command(CommandDefinition(
            name="story",
            aliases=["st", "historia"],
            description="Gerencia stories",
            usage="/fabrica story <acao> [args]",
            examples=["/fabrica story criar 'Titulo'", "/fabrica story mover STR-001 done"],
            handler=self._handle_story
        ))

        # Aprovar
        self.register_command(CommandDefinition(
            name="aprovar",
            aliases=["approve", "ok"],
            description="Aprova uma solicitacao",
            usage="/fabrica aprovar <id>",
            examples=["/fabrica aprovar REQ-001"],
            handler=self._handle_approve,
            requires_args=True,
            min_args=1
        ))

        # Rejeitar
        self.register_command(CommandDefinition(
            name="rejeitar",
            aliases=["reject", "nok"],
            description="Rejeita uma solicitacao",
            usage="/fabrica rejeitar <id> [motivo]",
            examples=["/fabrica rejeitar REQ-001 'Falta informacao'"],
            handler=self._handle_reject,
            requires_args=True,
            min_args=1
        ))

    def register_command(self, command: CommandDefinition):
        """Registra um comando"""
        self._commands[command.name] = command
        for alias in command.aliases:
            self._commands[alias] = command

    def set_action_callback(
        self,
        action: str,
        callback: Callable[..., Awaitable[Any]]
    ):
        """
        Define callback para uma acao.

        Args:
            action: Nome da acao
            callback: Funcao async a chamar
        """
        self._action_callbacks[action] = callback

    async def _call_action(self, action: str, *args, **kwargs) -> Any:
        """Chama callback de acao"""
        if action in self._action_callbacks:
            return await self._action_callbacks[action](*args, **kwargs)
        return None

    def parse_command(self, text: str) -> tuple:
        """
        Faz parsing do texto do comando.

        Args:
            text: Texto do comando

        Returns:
            (comando, argumentos)
        """
        # Remove mencoes do bot
        text = re.sub(r'<at>.*?</at>', '', text).strip()

        # Extrai comando e argumentos
        # Formatos suportados: /fabrica comando, @fabrica comando, fabrica comando
        patterns = [
            r'^/fabrica\s+(\w+)(?:\s+(.*))?$',
            r'^@fabrica\s+(\w+)(?:\s+(.*))?$',
            r'^fabrica\s+(\w+)(?:\s+(.*))?$'
        ]

        for pattern in patterns:
            match = re.match(pattern, text.lower().strip())
            if match:
                cmd = match.group(1)
                args_str = match.group(2) or ""

                # Parse argumentos (suporta strings entre aspas)
                args = []
                if args_str:
                    # Regex para capturar strings entre aspas ou palavras
                    arg_pattern = r'"([^"]+)"|\'([^\']+)\'|(\S+)'
                    for m in re.finditer(arg_pattern, args_str):
                        args.append(m.group(1) or m.group(2) or m.group(3))

                return cmd, args

        return None, []

    async def process(
        self,
        text: str,
        context: CommandContext
    ) -> CommandResult:
        """
        Processa comando.

        Args:
            text: Texto do comando
            context: Contexto de execucao

        Returns:
            Resultado do comando
        """
        context.raw_text = text

        cmd, args = self.parse_command(text)

        if not cmd:
            # Nao e um comando, pode ser mensagem normal
            return await self._handle_natural_language(text, context)

        command_def = self._commands.get(cmd)

        if not command_def:
            return CommandResult.error(
                f"Comando '{cmd}' nao encontrado.",
                "Digite '/fabrica ajuda' para ver comandos disponiveis."
            )

        # Valida argumentos
        if command_def.requires_args and len(args) < command_def.min_args:
            return CommandResult.error(
                f"Argumentos insuficientes para '{cmd}'.",
                f"Uso: {command_def.usage}"
            )

        if len(args) > command_def.max_args:
            return CommandResult.error(
                f"Muitos argumentos para '{cmd}'.",
                f"Uso: {command_def.usage}"
            )

        try:
            return await command_def.handler(context, args)
        except Exception as e:
            logger.error(f"Erro ao executar comando {cmd}: {e}")
            return CommandResult.error(
                f"Erro ao executar comando: {str(e)}"
            )

    async def process_action(
        self,
        action_data: Dict[str, Any],
        context: CommandContext
    ) -> CommandResult:
        """
        Processa acao de card (submit).

        Args:
            action_data: Dados da acao
            context: Contexto

        Returns:
            Resultado da acao
        """
        action = action_data.get("action") or action_data.get("command")

        if not action:
            return CommandResult.error("Acao invalida")

        # Acoes de confirmacao
        if action == "confirm":
            return await self._handle_confirm(action_data, context)
        elif action == "cancel":
            return CommandResult.ok("Acao cancelada.")

        # Trata como comando
        args = action_data.get("args", [])
        if isinstance(args, str):
            args = [args]

        context.raw_text = f"/fabrica {action} {' '.join(args)}"
        return await self.process(context.raw_text, context)

    # =========================================================================
    # Handlers de Comandos
    # =========================================================================

    async def _handle_status(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando status"""

        if args:
            # Status de projeto especifico
            project_id = args[0]
            project = await self._call_action("get_project", project_id)

            if not project:
                return CommandResult.error(
                    f"Projeto '{project_id}' nao encontrado."
                )

            card = self._card_builder.create_project_status_card(
                project_id=project.get("id", project_id),
                project_name=project.get("name", ""),
                status=project.get("status", ""),
                stories_done=project.get("stories_done", 0),
                stories_total=project.get("stories_total", 0),
                progress=project.get("progress", 0),
                url=project.get("url", "")
            )

            return CommandResult.ok(card=card)

        # Status geral
        status_data = await self._call_action("get_platform_status") or {}

        card = self._card_builder.create_status_card(
            status=status_data.get("status", "Online"),
            stories_count=status_data.get("stories_count", 0),
            tasks_in_progress=status_data.get("tasks_in_progress", 0),
            recent_completions=status_data.get("recent_completions", 0),
            uptime=status_data.get("uptime", ""),
            version=status_data.get("version", "6.0")
        )

        return CommandResult.ok(card=card)

    async def _handle_list(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando listar"""

        list_type = args[0] if args else "projetos"

        if list_type in ["projetos", "projects", "p"]:
            projects = await self._call_action("list_projects") or []

            if not projects:
                return CommandResult.ok("Nenhum projeto encontrado.")

            card = self._card_builder.create_project_list_card(
                projects=projects,
                total_count=len(projects)
            )

            return CommandResult.ok(card=card)

        elif list_type in ["stories", "s", "historias"]:
            project_id = args[1] if len(args) > 1 else None
            stories = await self._call_action("list_stories", project_id) or []

            if not stories:
                return CommandResult.ok("Nenhuma story encontrada.")

            card = self._card_builder.create_story_list_card(
                stories=stories,
                project_name=project_id or ""
            )

            return CommandResult.ok(card=card)

        return CommandResult.error(
            f"Tipo de listagem '{list_type}' nao reconhecido.",
            "Use: projetos ou stories"
        )

    async def _handle_project(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando projeto"""

        project_name = " ".join(args)

        # Cria card de confirmacao
        card = self._card_builder.create_confirmation_card(
            action="criar projeto",
            target=f"'{project_name}'",
            details=[
                ("Nome", project_name),
                ("Solicitado por", context.user_name)
            ],
            confirm_data={
                "action": "create_project",
                "name": project_name,
                "user_id": context.user_id
            }
        )

        return CommandResult.ok(
            message="Confirme a criacao do projeto:",
            card=card
        )

    async def _handle_develop(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando desenvolver"""

        target_id = args[0]

        # Verifica se e projeto ou story
        if target_id.startswith("PROJ") or target_id.startswith("PRJ"):
            entity_type = "projeto"
        elif target_id.startswith("STR") or target_id.startswith("STORY"):
            entity_type = "story"
        else:
            entity_type = "item"

        card = self._card_builder.create_confirmation_card(
            action="iniciar desenvolvimento",
            target=target_id,
            details=[
                ("ID", target_id),
                ("Tipo", entity_type),
                ("Solicitado por", context.user_name)
            ],
            confirm_data={
                "action": "start_development",
                "target_id": target_id,
                "user_id": context.user_id
            }
        )

        return CommandResult.ok(
            message="Confirme para iniciar o desenvolvimento:",
            card=card
        )

    async def _handle_help(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando ajuda"""

        if args:
            # Ajuda especifica de comando
            cmd_name = args[0]
            cmd = self._commands.get(cmd_name)

            if cmd:
                self._card_builder._builder.clear()
                self._card_builder._builder.add_heading(f"Comando: {cmd.name}")
                self._card_builder._builder.add_text(cmd.description)
                self._card_builder._builder.add_text(
                    f"**Uso**: {cmd.usage}",
                    separator=True
                )

                if cmd.examples:
                    self._card_builder._builder.add_text(
                        "**Exemplos**:",
                        separator=True
                    )
                    for ex in cmd.examples:
                        self._card_builder._builder.add_text(f"• {ex}")

                card = self._card_builder._builder.build()
                return CommandResult.ok(card=card)

            return CommandResult.error(
                f"Comando '{cmd_name}' nao encontrado."
            )

        # Lista todos os comandos
        unique_commands = {}
        for name, cmd in self._commands.items():
            if cmd.name not in unique_commands:
                unique_commands[cmd.name] = cmd

        commands = [
            {"command": f"/fabrica {cmd.name}", "description": cmd.description}
            for cmd in unique_commands.values()
        ]

        card = self._card_builder.create_help_card(commands)
        return CommandResult.ok(card=card)

    async def _handle_story(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando story"""

        if not args:
            return CommandResult.error(
                "Acao de story nao especificada.",
                "Use: /fabrica story criar|mover|deletar ..."
            )

        action = args[0]

        if action in ["criar", "create", "new"]:
            if len(args) < 2:
                # Mostra formulario
                card = self._card_builder.create_input_card(
                    title="Criar Nova Story",
                    fields=[
                        {
                            "id": "title",
                            "label": "Titulo",
                            "placeholder": "Ex: Implementar login",
                            "required": True
                        },
                        {
                            "id": "persona",
                            "label": "Como um(a)...",
                            "placeholder": "Ex: usuario do sistema",
                            "required": True
                        },
                        {
                            "id": "action",
                            "label": "Eu quero...",
                            "placeholder": "Ex: fazer login com meu email",
                            "required": True
                        },
                        {
                            "id": "benefit",
                            "label": "Para que...",
                            "placeholder": "Ex: acessar minhas informacoes",
                            "required": True
                        }
                    ],
                    submit_title="Criar Story",
                    submit_data={"action": "create_story"}
                )
                return CommandResult.ok(card=card)

            # Cria com titulo fornecido
            title = " ".join(args[1:])
            result = await self._call_action("create_story", {"title": title})

            if result:
                return CommandResult.ok(
                    f"Story criada: {result.get('id', '')}"
                )
            return CommandResult.error("Erro ao criar story")

        elif action in ["mover", "move", "mv"]:
            if len(args) < 3:
                return CommandResult.error(
                    "Argumentos insuficientes.",
                    "Use: /fabrica story mover <id> <status>"
                )

            story_id = args[1]
            new_status = args[2]

            result = await self._call_action(
                "move_story",
                story_id,
                new_status
            )

            if result:
                return CommandResult.ok(
                    f"Story {story_id} movida para {new_status}"
                )
            return CommandResult.error(f"Erro ao mover story {story_id}")

        return CommandResult.error(
            f"Acao de story '{action}' nao reconhecida."
        )

    async def _handle_approve(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando aprovar"""

        request_id = args[0]

        result = await self._call_action(
            "approve_request",
            request_id,
            context.user_id
        )

        if result:
            card = self._card_builder.create_success_card(
                title="Aprovado!",
                message=f"Solicitacao {request_id} foi aprovada.",
                details=[
                    ("Aprovado por", context.user_name),
                    ("Data", datetime.now().strftime("%d/%m/%Y %H:%M"))
                ]
            )
            return CommandResult.ok(card=card)

        return CommandResult.error(
            f"Erro ao aprovar solicitacao {request_id}"
        )

    async def _handle_reject(
        self,
        context: CommandContext,
        args: List[str]
    ) -> CommandResult:
        """Handler do comando rejeitar"""

        request_id = args[0]
        reason = " ".join(args[1:]) if len(args) > 1 else ""

        result = await self._call_action(
            "reject_request",
            request_id,
            context.user_id,
            reason
        )

        if result:
            card = self._card_builder.create_success_card(
                title="Rejeitado",
                message=f"Solicitacao {request_id} foi rejeitada.",
                details=[
                    ("Rejeitado por", context.user_name),
                    ("Motivo", reason or "Nao especificado"),
                    ("Data", datetime.now().strftime("%d/%m/%Y %H:%M"))
                ]
            )
            return CommandResult.ok(card=card)

        return CommandResult.error(
            f"Erro ao rejeitar solicitacao {request_id}"
        )

    async def _handle_confirm(
        self,
        action_data: Dict[str, Any],
        context: CommandContext
    ) -> CommandResult:
        """Handler de confirmacao"""

        original_action = action_data.get("original_action") or action_data.get("action")

        if original_action == "create_project":
            result = await self._call_action(
                "create_project",
                action_data.get("name"),
                action_data.get("user_id")
            )

            if result:
                return CommandResult.ok(
                    f"Projeto criado com sucesso! ID: {result.get('id', '')}"
                )
            return CommandResult.error("Erro ao criar projeto")

        elif original_action == "start_development":
            result = await self._call_action(
                "start_development",
                action_data.get("target_id"),
                action_data.get("user_id")
            )

            if result:
                return CommandResult.ok(
                    f"Desenvolvimento iniciado para {action_data.get('target_id')}"
                )
            return CommandResult.error("Erro ao iniciar desenvolvimento")

        return CommandResult.error("Acao de confirmacao desconhecida")

    async def _handle_natural_language(
        self,
        text: str,
        context: CommandContext
    ) -> CommandResult:
        """
        Processa mensagem em linguagem natural.
        Tenta entender a intencao do usuario.
        """
        text_lower = text.lower()

        # Detecta intencoes basicas
        if any(w in text_lower for w in ["ola", "oi", "hey", "bom dia", "boa tarde"]):
            card = self._card_builder.create_welcome_card(context.user_name)
            return CommandResult.ok(card=card)

        if any(w in text_lower for w in ["ajuda", "help", "comandos"]):
            return await self._handle_help(context, [])

        if any(w in text_lower for w in ["status", "como esta"]):
            return await self._handle_status(context, [])

        if any(w in text_lower for w in ["listar", "mostrar", "ver projetos"]):
            return await self._handle_list(context, ["projetos"])

        # Nao entendeu
        return CommandResult.ok(
            "Desculpe, nao entendi. Digite '/fabrica ajuda' para ver comandos disponiveis."
        )
