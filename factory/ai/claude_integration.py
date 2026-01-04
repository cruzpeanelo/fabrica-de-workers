"""
Integracao com Claude API
=========================

Este modulo fornece integracao com a API do Claude (Anthropic)
para dar inteligencia REAL aos agentes da Fabrica.

Os agentes usam Claude para:
- Analisar requisitos e tomar decisoes
- Gerar codigo contextualizado
- Revisar e melhorar codigo
- Aprender com feedback
"""

import os
import json
import time
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from pathlib import Path

# Tenta importar anthropic
try:
    import anthropic
    HAS_ANTHROPIC = True
except ImportError:
    HAS_ANTHROPIC = False
    print("[ClaudeIntegration] AVISO: anthropic nao instalado. Execute: pip install anthropic")


@dataclass
class ClaudeMessage:
    """Mensagem para/de Claude"""
    role: str  # "user" ou "assistant"
    content: str
    timestamp: datetime = field(default_factory=datetime.utcnow)


@dataclass
class ClaudeResponse:
    """Resposta do Claude"""
    success: bool
    content: str
    model: str = ""
    tokens_used: int = 0
    error: Optional[str] = None
    thinking: Optional[str] = None


class ClaudeClient:
    """
    Cliente para API do Claude

    Fornece metodos para interagir com Claude de forma
    estruturada para uso pelos agentes.
    """

    DEFAULT_MODEL = "claude-sonnet-4-20250514"
    MAX_TOKENS = 4096

    def __init__(self, api_key: Optional[str] = None):
        """
        Inicializa cliente Claude

        Args:
            api_key: Chave da API. Se None, usa ANTHROPIC_API_KEY do ambiente
        """
        self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        self.client = None
        self.conversation_history: List[ClaudeMessage] = []

        if HAS_ANTHROPIC and self.api_key:
            self.client = anthropic.Anthropic(api_key=self.api_key)
            print("[ClaudeClient] Inicializado com sucesso")
        elif not HAS_ANTHROPIC:
            print("[ClaudeClient] ERRO: Biblioteca anthropic nao instalada")
        else:
            print("[ClaudeClient] ERRO: API key nao configurada")

    def is_available(self) -> bool:
        """Verifica se Claude esta disponivel"""
        return self.client is not None

    def chat(self, message: str, system_prompt: str = None,
             context: List[Dict] = None, max_tokens: int = None) -> ClaudeResponse:
        """
        Envia mensagem para Claude e recebe resposta

        Args:
            message: Mensagem do usuario
            system_prompt: Prompt de sistema (define comportamento)
            context: Historico de conversa anterior
            max_tokens: Maximo de tokens na resposta
        """
        if not self.is_available():
            return ClaudeResponse(
                success=False,
                content="",
                error="Claude API nao disponivel. Verifique API key e instalacao."
            )

        try:
            # Monta mensagens
            messages = []

            # Adiciona contexto se fornecido
            if context:
                for ctx in context:
                    messages.append({
                        "role": ctx.get("role", "user"),
                        "content": ctx.get("content", "")
                    })

            # Adiciona mensagem atual
            messages.append({
                "role": "user",
                "content": message
            })

            # Chama API
            response = self.client.messages.create(
                model=self.DEFAULT_MODEL,
                max_tokens=max_tokens or self.MAX_TOKENS,
                system=system_prompt or "Voce e um assistente inteligente.",
                messages=messages
            )

            # Extrai conteudo
            content = ""
            if response.content:
                for block in response.content:
                    if hasattr(block, 'text'):
                        content += block.text

            # Registra no historico
            self.conversation_history.append(ClaudeMessage(role="user", content=message))
            self.conversation_history.append(ClaudeMessage(role="assistant", content=content))

            return ClaudeResponse(
                success=True,
                content=content,
                model=response.model,
                tokens_used=response.usage.output_tokens if response.usage else 0
            )

        except anthropic.APIError as e:
            return ClaudeResponse(
                success=False,
                content="",
                error=f"Erro na API: {str(e)}"
            )
        except Exception as e:
            return ClaudeResponse(
                success=False,
                content="",
                error=f"Erro: {str(e)}"
            )

    def analyze_requirements(self, requirements: str, project_context: str = "") -> ClaudeResponse:
        """
        Analisa requisitos e extrai informacoes estruturadas

        Args:
            requirements: Texto com requisitos
            project_context: Contexto do projeto
        """
        system_prompt = """Voce e um analista de sistemas experiente.
Sua tarefa e analisar requisitos e extrair informacoes estruturadas.

Responda SEMPRE em JSON valido com a seguinte estrutura:
{
    "entidades": [{"nome": "", "descricao": "", "atributos": []}],
    "funcionalidades": [{"nome": "", "descricao": "", "prioridade": 1-5}],
    "regras_negocio": [""],
    "integrações": [""],
    "riscos": [""],
    "estimativa_complexidade": "baixa|media|alta"
}"""

        message = f"""Analise os seguintes requisitos:

{requirements}

Contexto do projeto: {project_context}

Extraia as entidades, funcionalidades, regras de negocio e demais informacoes."""

        return self.chat(message, system_prompt)

    def generate_code(self, specification: str, language: str = "python",
                     framework: str = "", existing_code: str = "") -> ClaudeResponse:
        """
        Gera codigo baseado em especificacao

        Args:
            specification: O que deve ser implementado
            language: Linguagem de programacao
            framework: Framework a usar (FastAPI, Vue, etc)
            existing_code: Codigo existente para contexto
        """
        system_prompt = f"""Voce e um desenvolvedor senior especialista em {language}.
Gere codigo limpo, bem documentado e seguindo boas praticas.

Regras:
- Use {framework} se especificado
- Inclua docstrings e comentarios
- Siga PEP8 para Python
- Codigo deve ser funcional e completo
- Responda APENAS com o codigo, sem explicacoes extras
- Use ``` para delimitar blocos de codigo"""

        message = f"""Gere codigo {language} para:

{specification}

"""
        if framework:
            message += f"Use o framework: {framework}\n"

        if existing_code:
            message += f"\nCodigo existente para referencia:\n```\n{existing_code}\n```"

        return self.chat(message, system_prompt)

    def review_code(self, code: str, language: str = "python") -> ClaudeResponse:
        """
        Revisa codigo e sugere melhorias

        Args:
            code: Codigo a revisar
            language: Linguagem do codigo
        """
        system_prompt = """Voce e um revisor de codigo senior.
Analise o codigo e forneca feedback construtivo.

Responda em JSON com:
{
    "qualidade_geral": 1-10,
    "problemas": [{"tipo": "", "linha": 0, "descricao": "", "severidade": "baixa|media|alta"}],
    "sugestoes": [""],
    "pontos_positivos": [""],
    "seguranca": {"score": 1-10, "vulnerabilidades": []},
    "aprovado": true|false
}"""

        message = f"""Revise o seguinte codigo {language}:

```{language}
{code}
```

Analise qualidade, seguranca, boas praticas e forneca feedback."""

        return self.chat(message, system_prompt)

    def create_user_story(self, requirement: str, project_context: str = "") -> ClaudeResponse:
        """
        Cria user story estruturada a partir de requisito

        Args:
            requirement: Requisito em texto livre
            project_context: Contexto do projeto
        """
        system_prompt = """Voce e um Product Owner experiente.
Crie user stories bem estruturadas seguindo boas praticas ageis.

Responda em JSON com:
{
    "titulo": "",
    "narrativa": {
        "como": "",
        "quero": "",
        "para_que": ""
    },
    "criterios_aceitacao": ["DADO... QUANDO... ENTAO..."],
    "regras_negocio": [""],
    "notas_tecnicas": [""],
    "pontos": 1-21,
    "prioridade": 1-5,
    "dependencias": [],
    "riscos": []
}"""

        message = f"""Crie uma user story para:

{requirement}

Contexto do projeto: {project_context}"""

        return self.chat(message, system_prompt)

    def decide_next_action(self, current_state: Dict, available_actions: List[str],
                          agent_role: str, project_context: str = "") -> ClaudeResponse:
        """
        Decide proxima acao baseado no estado atual

        Args:
            current_state: Estado atual do projeto/tarefa
            available_actions: Acoes disponiveis
            agent_role: Papel do agente
            project_context: Contexto do projeto
        """
        system_prompt = f"""Voce e um agente autonomo com o papel de: {agent_role}

Analise o estado atual e decida a proxima acao.
Considere prioridades, dependencias e impacto.

Responda em JSON com:
{{
    "acao_escolhida": "",
    "justificativa": "",
    "parametros": {{}},
    "confianca": 0-100,
    "proximas_acoes_sugeridas": []
}}"""

        message = f"""Estado atual:
{json.dumps(current_state, indent=2, ensure_ascii=False)}

Acoes disponiveis: {available_actions}

Contexto: {project_context}

Qual a proxima acao?"""

        return self.chat(message, system_prompt)

    def clear_history(self):
        """Limpa historico de conversa"""
        self.conversation_history.clear()


class AgentBrain:
    """
    Cerebro inteligente de um agente

    Usa Claude para tomar decisoes, gerar codigo e aprender.
    Cada agente tem seu proprio contexto e memoria.
    """

    def __init__(self, agent_id: str, agent_role: str, agent_capabilities: List[str]):
        """
        Inicializa cerebro do agente

        Args:
            agent_id: ID do agente
            agent_role: Papel/funcao do agente
            agent_capabilities: Lista de capacidades
        """
        self.agent_id = agent_id
        self.agent_role = agent_role
        self.capabilities = agent_capabilities
        self.claude = ClaudeClient()

        # Memoria do agente
        self.memory: List[Dict] = []
        self.learned_patterns: List[Dict] = []
        self.decision_history: List[Dict] = []

        # System prompt especifico do agente
        self.system_prompt = self._build_system_prompt()

    def _build_system_prompt(self) -> str:
        """Constroi system prompt especifico do agente"""
        return f"""Voce e um agente autonomo inteligente na Plataforma E.

Seu ID: {self.agent_id}
Seu papel: {self.agent_role}
Suas capacidades: {', '.join(self.capabilities)}

Voce deve:
1. Tomar decisoes baseadas em seu papel e capacidades
2. Respeitar hierarquia e pedir aprovacao quando necessario
3. Aprender com cada tarefa executada
4. Colaborar com outros agentes
5. Priorizar qualidade e boas praticas

Sempre responda de forma estruturada e acionavel."""

    def think(self, situation: str, context: Dict = None) -> ClaudeResponse:
        """
        Agente "pensa" sobre uma situacao

        Args:
            situation: Descricao da situacao
            context: Contexto adicional
        """
        if not self.claude.is_available():
            return ClaudeResponse(
                success=False,
                content="",
                error="Claude nao disponivel"
            )

        message = f"""Situacao: {situation}

Contexto: {json.dumps(context or {}, indent=2, ensure_ascii=False)}

Considerando seu papel como {self.agent_role}, como voce abordaria isso?"""

        response = self.claude.chat(message, self.system_prompt)

        # Registra pensamento
        self.memory.append({
            "type": "thinking",
            "situation": situation,
            "response": response.content if response.success else response.error,
            "timestamp": datetime.utcnow().isoformat()
        })

        return response

    def decide(self, options: List[str], criteria: str = "") -> ClaudeResponse:
        """
        Agente decide entre opcoes

        Args:
            options: Lista de opcoes
            criteria: Criterios para decisao
        """
        message = f"""Preciso decidir entre as seguintes opcoes:
{json.dumps(options, indent=2, ensure_ascii=False)}

Criterios: {criteria or 'Escolha a melhor opcao para o projeto'}

Responda em JSON:
{{
    "opcao_escolhida": "",
    "justificativa": "",
    "confianca": 0-100,
    "riscos": []
}}"""

        response = self.claude.chat(message, self.system_prompt)

        # Registra decisao
        self.decision_history.append({
            "options": options,
            "criteria": criteria,
            "response": response.content if response.success else None,
            "timestamp": datetime.utcnow().isoformat()
        })

        return response

    def generate_code_intelligent(self, task: str, language: str = "python",
                                 framework: str = "", context: str = "") -> ClaudeResponse:
        """
        Gera codigo de forma inteligente considerando o contexto do agente
        """
        enhanced_task = f"""Como {self.agent_role}, gere codigo para:

{task}

Minhas capacidades incluem: {', '.join(self.capabilities)}

Contexto adicional: {context}"""

        return self.claude.generate_code(enhanced_task, language, framework)

    def learn(self, experience: Dict):
        """
        Agente aprende com uma experiencia

        Args:
            experience: Dicionario com detalhes da experiencia
        """
        self.memory.append({
            "type": "learning",
            "experience": experience,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Se foi um padrao util, registra
        if experience.get("success") and experience.get("pattern"):
            self.learned_patterns.append({
                "pattern": experience["pattern"],
                "context": experience.get("context"),
                "learned_at": datetime.utcnow().isoformat()
            })

    def get_relevant_memories(self, context: str, limit: int = 5) -> List[Dict]:
        """
        Busca memorias relevantes para um contexto

        Args:
            context: Contexto para buscar memorias relacionadas
            limit: Numero maximo de memorias
        """
        # Por enquanto, retorna as mais recentes
        # TODO: Implementar busca semantica
        return self.memory[-limit:]

    def should_ask_approval(self, action: str, impact: str = "medium") -> bool:
        """
        Decide se deve pedir aprovacao humana

        Args:
            action: Acao a ser executada
            impact: Impacto da acao (low, medium, high, critical)
        """
        # Acoes de alto impacto sempre precisam aprovacao
        high_impact_actions = [
            "delete", "deploy", "modify_production",
            "change_architecture", "remove", "alter_database"
        ]

        for keyword in high_impact_actions:
            if keyword in action.lower():
                return True

        # Impacto alto ou critico precisa aprovacao
        if impact in ["high", "critical"]:
            return True

        return False


# Instancia global do cliente Claude
_claude_client: Optional[ClaudeClient] = None


def get_claude_client() -> ClaudeClient:
    """Retorna instancia global do cliente Claude"""
    global _claude_client
    if _claude_client is None:
        _claude_client = ClaudeClient()
    return _claude_client


def create_agent_brain(agent_id: str, agent_role: str,
                      capabilities: List[str]) -> AgentBrain:
    """Cria cerebro inteligente para um agente"""
    return AgentBrain(agent_id, agent_role, capabilities)
