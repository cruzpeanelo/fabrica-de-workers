# -*- coding: utf-8 -*-
"""
MCP Client - Cliente para integracao com Workers e Claude API

Este modulo fornece uma interface para usar as ferramentas MCP
diretamente no codigo Python, sem necessidade de servidor MCP externo.

Tambem fornece funcoes para converter as tools MCP para o formato
esperado pela Claude API (tools parameter).

Uso com Workers:
    from factory.mcp.client import MCPToolsClient

    client = MCPToolsClient()
    result = await client.create_story(
        project_id="PROJ-001",
        title="Nova feature",
        persona="usuario",
        action="fazer login",
        benefit="acessar sistema"
    )

Uso com Claude API:
    from factory.mcp.client import get_claude_tools, execute_tool_call

    # Obter tools no formato Claude API
    tools = get_claude_tools()

    # Na resposta do Claude, executar tool_use
    result = await execute_tool_call(tool_name, tool_input)
"""

import asyncio
import json
from typing import Any, Dict, List, Optional, Callable
from datetime import datetime

from factory.mcp.tools import (
    get_all_tools,
    get_tool_handlers,
    StoryTools,
    TaskTools,
    WorkerTools,
    ProjectTools,
    SystemTools,
    ToolDefinition,
)


# =============================================================================
# MCP TOOLS CLIENT
# =============================================================================

class MCPToolsClient:
    """
    Cliente para usar ferramentas MCP programaticamente

    Fornece metodos convenientes para cada ferramenta MCP,
    permitindo uso direto no codigo Python.

    Exemplo:
        client = MCPToolsClient()

        # Criar story
        result = await client.create_story(
            project_id="PROJ-001",
            title="Implementar login"
        )

        # Listar stories
        stories = await client.list_stories(project_id="PROJ-001")

        # Criar job
        job = await client.create_job(
            description="Criar API REST",
            tech_stack="python, fastapi"
        )
    """

    def __init__(self):
        self._handlers = get_tool_handlers()
        self._tools = get_all_tools()

        # Criar metodos dinamicos para cada tool
        for tool in self._tools:
            setattr(self, tool.name, self._create_method(tool.name))

    def _create_method(self, tool_name: str) -> Callable:
        """Cria metodo para uma tool especifica"""
        handler = self._handlers[tool_name]

        async def method(**kwargs):
            return await handler(**kwargs)

        # Copiar docstring da tool
        for tool in self._tools:
            if tool.name == tool_name:
                method.__doc__ = f"{tool.description}\n\nParametros:\n"
                for param in tool.parameters:
                    req = "(obrigatorio)" if param.required else "(opcional)"
                    method.__doc__ += f"  {param.name} {req}: {param.description}\n"
                break

        return method

    async def call(self, tool_name: str, **kwargs) -> Dict[str, Any]:
        """
        Chama uma ferramenta pelo nome

        Args:
            tool_name: Nome da ferramenta
            **kwargs: Argumentos da ferramenta

        Returns:
            Resultado da execucao
        """
        if tool_name not in self._handlers:
            return {
                "success": False,
                "error": f"Ferramenta '{tool_name}' nao encontrada"
            }

        return await self._handlers[tool_name](**kwargs)

    def list_tools(self) -> List[str]:
        """Lista nomes das ferramentas disponiveis"""
        return list(self._handlers.keys())

    def get_tool_info(self, tool_name: str) -> Optional[Dict]:
        """Retorna informacoes sobre uma ferramenta"""
        for tool in self._tools:
            if tool.name == tool_name:
                return tool.to_mcp_schema()
        return None


# =============================================================================
# CLAUDE API INTEGRATION
# =============================================================================

def get_claude_tools() -> List[Dict]:
    """
    Retorna as ferramentas MCP no formato esperado pela Claude API

    Este formato e usado no parametro 'tools' da API de Messages.

    Exemplo:
        from anthropic import Anthropic
        from factory.mcp.client import get_claude_tools

        client = Anthropic()
        tools = get_claude_tools()

        response = client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=4096,
            tools=tools,
            messages=[{"role": "user", "content": "Crie uma nova story..."}]
        )
    """
    tools = get_all_tools()
    claude_tools = []

    for tool in tools:
        # Construir schema de input
        properties = {}
        required = []

        for param in tool.parameters:
            prop = {
                "type": param.type,
                "description": param.description
            }

            if param.enum:
                prop["enum"] = param.enum

            properties[param.name] = prop

            if param.required:
                required.append(param.name)

        claude_tool = {
            "name": tool.name,
            "description": tool.description,
            "input_schema": {
                "type": "object",
                "properties": properties,
                "required": required
            }
        }

        claude_tools.append(claude_tool)

    return claude_tools


def get_claude_tools_by_category(categories: List[str]) -> List[Dict]:
    """
    Retorna ferramentas filtradas por categoria

    Categorias: story, task, worker, project, system

    Exemplo:
        # Apenas ferramentas de story e task
        tools = get_claude_tools_by_category(["story", "task"])
    """
    all_tools = get_all_tools()
    filtered_tools = [t for t in all_tools if t.category.value in categories]

    claude_tools = []
    for tool in filtered_tools:
        properties = {}
        required = []

        for param in tool.parameters:
            prop = {
                "type": param.type,
                "description": param.description
            }
            if param.enum:
                prop["enum"] = param.enum
            properties[param.name] = prop
            if param.required:
                required.append(param.name)

        claude_tools.append({
            "name": tool.name,
            "description": tool.description,
            "input_schema": {
                "type": "object",
                "properties": properties,
                "required": required
            }
        })

    return claude_tools


async def execute_tool_call(tool_name: str, tool_input: Dict[str, Any]) -> Dict[str, Any]:
    """
    Executa uma chamada de ferramenta retornada pelo Claude

    Quando Claude retorna um tool_use block, use esta funcao
    para executar a ferramenta e obter o resultado.

    Exemplo:
        # Na resposta do Claude
        for block in response.content:
            if block.type == "tool_use":
                result = await execute_tool_call(block.name, block.input)
                # result contem o retorno da ferramenta
    """
    handlers = get_tool_handlers()

    if tool_name not in handlers:
        return {
            "success": False,
            "error": f"Ferramenta '{tool_name}' nao encontrada"
        }

    try:
        result = await handlers[tool_name](**tool_input)
        return result
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "tool": tool_name,
            "input": tool_input
        }


def format_tool_result_for_claude(result: Dict[str, Any]) -> str:
    """
    Formata resultado de tool para enviar de volta ao Claude

    O resultado e convertido para string JSON para ser incluido
    na mensagem de tool_result.

    Exemplo:
        result = await execute_tool_call(tool_name, tool_input)
        formatted = format_tool_result_for_claude(result)

        messages.append({
            "role": "user",
            "content": [{
                "type": "tool_result",
                "tool_use_id": tool_use_id,
                "content": formatted
            }]
        })
    """
    return json.dumps(result, indent=2, ensure_ascii=False)


# =============================================================================
# HELPER: CLAUDE CONVERSATION WITH TOOLS
# =============================================================================

class ClaudeToolsConversation:
    """
    Helper para conversas com Claude usando ferramentas MCP

    Gerencia o loop de conversacao incluindo execucao automatica
    de ferramentas quando Claude solicita.

    Exemplo:
        from anthropic import Anthropic
        from factory.mcp.client import ClaudeToolsConversation

        anthropic = Anthropic()
        conv = ClaudeToolsConversation(anthropic)

        # Conversa com execucao automatica de tools
        response = await conv.chat(
            "Crie uma story para implementar login no projeto PROJ-001"
        )
    """

    def __init__(
        self,
        anthropic_client,
        model: str = "claude-sonnet-4-20250514",
        max_tokens: int = 4096,
        tool_categories: Optional[List[str]] = None
    ):
        """
        Args:
            anthropic_client: Cliente Anthropic inicializado
            model: Modelo Claude a usar
            max_tokens: Maximo de tokens na resposta
            tool_categories: Categorias de tools a disponibilizar
                           (None = todas)
        """
        self.client = anthropic_client
        self.model = model
        self.max_tokens = max_tokens

        # Obter tools
        if tool_categories:
            self.tools = get_claude_tools_by_category(tool_categories)
        else:
            self.tools = get_claude_tools()

        self.messages = []

    async def chat(
        self,
        message: str,
        auto_execute_tools: bool = True,
        max_tool_iterations: int = 5
    ) -> Dict[str, Any]:
        """
        Envia mensagem e processa resposta

        Args:
            message: Mensagem do usuario
            auto_execute_tools: Se True, executa tools automaticamente
            max_tool_iterations: Maximo de iteracoes de tools

        Returns:
            {
                "response": texto final,
                "tool_calls": lista de tools executadas,
                "messages": historico de mensagens
            }
        """
        # Adicionar mensagem do usuario
        self.messages.append({
            "role": "user",
            "content": message
        })

        tool_calls = []
        iterations = 0

        while iterations < max_tool_iterations:
            iterations += 1

            # Chamar Claude
            response = self.client.messages.create(
                model=self.model,
                max_tokens=self.max_tokens,
                tools=self.tools,
                messages=self.messages
            )

            # Processar resposta
            assistant_content = []
            has_tool_use = False

            for block in response.content:
                if block.type == "text":
                    assistant_content.append({
                        "type": "text",
                        "text": block.text
                    })
                elif block.type == "tool_use":
                    has_tool_use = True
                    assistant_content.append({
                        "type": "tool_use",
                        "id": block.id,
                        "name": block.name,
                        "input": block.input
                    })

            # Adicionar resposta do assistente
            self.messages.append({
                "role": "assistant",
                "content": assistant_content
            })

            # Se nao tem tool_use ou nao deve executar, retornar
            if not has_tool_use or not auto_execute_tools:
                final_text = ""
                for block in response.content:
                    if block.type == "text":
                        final_text += block.text

                return {
                    "response": final_text,
                    "tool_calls": tool_calls,
                    "messages": self.messages
                }

            # Executar tools
            tool_results = []
            for block in response.content:
                if block.type == "tool_use":
                    result = await execute_tool_call(block.name, block.input)
                    tool_calls.append({
                        "tool": block.name,
                        "input": block.input,
                        "result": result
                    })
                    tool_results.append({
                        "type": "tool_result",
                        "tool_use_id": block.id,
                        "content": format_tool_result_for_claude(result)
                    })

            # Adicionar resultados
            self.messages.append({
                "role": "user",
                "content": tool_results
            })

            # Se stop_reason nao e tool_use, terminar
            if response.stop_reason != "tool_use":
                break

        # Extrair texto final
        final_text = ""
        for msg in reversed(self.messages):
            if msg["role"] == "assistant":
                for block in msg["content"]:
                    if isinstance(block, dict) and block.get("type") == "text":
                        final_text = block["text"]
                        break
                    elif hasattr(block, "type") and block.type == "text":
                        final_text = block.text
                        break
                break

        return {
            "response": final_text,
            "tool_calls": tool_calls,
            "messages": self.messages
        }

    def clear_history(self):
        """Limpa historico de mensagens"""
        self.messages = []


# =============================================================================
# SYNC WRAPPER
# =============================================================================

class SyncMCPClient:
    """
    Versao sincrona do MCPToolsClient

    Para uso em contextos onde async nao e suportado.

    Exemplo:
        client = SyncMCPClient()
        result = client.create_story(
            project_id="PROJ-001",
            title="Nova feature"
        )
    """

    def __init__(self):
        self._async_client = MCPToolsClient()

    def __getattr__(self, name: str):
        """Proxy para metodos async"""
        async_method = getattr(self._async_client, name, None)
        if async_method and callable(async_method):
            def sync_wrapper(**kwargs):
                return asyncio.run(async_method(**kwargs))
            return sync_wrapper
        raise AttributeError(f"'{type(self).__name__}' object has no attribute '{name}'")

    def call(self, tool_name: str, **kwargs) -> Dict[str, Any]:
        """Chama ferramenta de forma sincrona"""
        return asyncio.run(self._async_client.call(tool_name, **kwargs))

    def list_tools(self) -> List[str]:
        """Lista ferramentas disponiveis"""
        return self._async_client.list_tools()
