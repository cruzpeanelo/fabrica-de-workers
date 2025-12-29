"""
LLM Manager - Suporte a Multiplas LLMs
======================================

Este modulo fornece uma interface unificada para trabalhar com
diferentes provedores de LLM (Large Language Models).

Provedores suportados:
- Claude (Anthropic)
- Azure OpenAI
- AWS Bedrock
- Google Vertex AI

Uso:
    from factory.ai.llm_manager import get_llm_client

    client = get_llm_client("claude")
    response = await client.complete("Ola, como voce esta?")
"""

import os
import json
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Any
from enum import Enum


# =============================================================================
# ENUMS E DATACLASSES
# =============================================================================

class LLMProvider(Enum):
    """Provedores de LLM suportados"""
    CLAUDE = "claude"
    AZURE_OPENAI = "azure_openai"
    AWS_BEDROCK = "aws_bedrock"
    GOOGLE_VERTEX = "google_vertex"


@dataclass
class LLMConfig:
    """Configuracao de um provedor de LLM"""
    provider: LLMProvider
    api_key: Optional[str] = None
    endpoint: Optional[str] = None
    model: str = ""
    region: Optional[str] = None
    project_id: Optional[str] = None
    max_tokens: int = 4096
    temperature: float = 0.7
    extra_params: Dict[str, Any] = field(default_factory=dict)


@dataclass
class LLMResponse:
    """Resposta padronizada de qualquer LLM"""
    success: bool
    content: str
    provider: str = ""
    model: str = ""
    tokens_used: int = 0
    latency_ms: float = 0
    error: Optional[str] = None
    raw_response: Optional[Dict] = None


@dataclass
class LLMMessage:
    """Mensagem para conversacao com LLM"""
    role: str  # "user", "assistant", "system"
    content: str
    timestamp: datetime = field(default_factory=datetime.utcnow)


# =============================================================================
# INTERFACE BASE
# =============================================================================

class LLMClient(ABC):
    """
    Interface base para clientes de LLM

    Todas as implementacoes devem herdar desta classe e
    implementar os metodos abstratos.
    """

    def __init__(self, config: Optional[LLMConfig] = None):
        """
        Inicializa o cliente LLM

        Args:
            config: Configuracao do provedor
        """
        self.config = config or self._default_config()
        self._is_initialized = False
        self.conversation_history: List[LLMMessage] = []

    @abstractmethod
    def _default_config(self) -> LLMConfig:
        """Retorna configuracao padrao do provedor"""
        pass

    @abstractmethod
    async def complete(self, prompt: str, **kwargs) -> LLMResponse:
        """
        Completa um prompt de texto

        Args:
            prompt: Texto de entrada
            **kwargs: Parametros adicionais

        Returns:
            LLMResponse com o resultado
        """
        pass

    @abstractmethod
    async def chat(self, messages: List[LLMMessage],
                   system_prompt: Optional[str] = None,
                   **kwargs) -> LLMResponse:
        """
        Realiza chat com historico de mensagens

        Args:
            messages: Lista de mensagens
            system_prompt: Prompt de sistema
            **kwargs: Parametros adicionais

        Returns:
            LLMResponse com a resposta
        """
        pass

    @abstractmethod
    async def test_connection(self) -> bool:
        """
        Testa conexao com o provedor

        Returns:
            True se conexao bem sucedida
        """
        pass

    @abstractmethod
    def is_available(self) -> bool:
        """
        Verifica se o cliente esta disponivel

        Returns:
            True se cliente configurado e pronto
        """
        pass

    def get_provider_name(self) -> str:
        """Retorna nome do provedor"""
        return self.config.provider.value if self.config else "unknown"

    def clear_history(self):
        """Limpa historico de conversa"""
        self.conversation_history.clear()


# =============================================================================
# IMPLEMENTACAO CLAUDE (Anthropic)
# =============================================================================

class ClaudeClient(LLMClient):
    """
    Cliente para Claude API (Anthropic)

    Requer:
        - ANTHROPIC_API_KEY no ambiente
        - pip install anthropic
    """

    DEFAULT_MODEL = "claude-sonnet-4-20250514"

    def __init__(self, config: Optional[LLMConfig] = None):
        super().__init__(config)
        self.client = None
        self._init_client()

    def _default_config(self) -> LLMConfig:
        return LLMConfig(
            provider=LLMProvider.CLAUDE,
            api_key=os.environ.get("ANTHROPIC_API_KEY"),
            model=os.environ.get("CLAUDE_MODEL", self.DEFAULT_MODEL),
            max_tokens=int(os.environ.get("CLAUDE_MAX_TOKENS", 4096))
        )

    def _init_client(self):
        """Inicializa cliente Anthropic"""
        try:
            import anthropic
            if self.config.api_key:
                self.client = anthropic.Anthropic(api_key=self.config.api_key)
                self._is_initialized = True
                print("[ClaudeClient] Inicializado com sucesso")
            else:
                print("[ClaudeClient] AVISO: API key nao configurada")
        except ImportError:
            print("[ClaudeClient] ERRO: anthropic nao instalado. Execute: pip install anthropic")

    def is_available(self) -> bool:
        return self.client is not None and self._is_initialized

    async def complete(self, prompt: str, **kwargs) -> LLMResponse:
        """Completa um prompt usando Claude"""
        messages = [LLMMessage(role="user", content=prompt)]
        return await self.chat(messages, **kwargs)

    async def chat(self, messages: List[LLMMessage],
                   system_prompt: Optional[str] = None,
                   **kwargs) -> LLMResponse:
        """Chat com Claude"""
        import time
        start_time = time.time()

        if not self.is_available():
            return LLMResponse(
                success=False,
                content="",
                provider="claude",
                error="Claude API nao disponivel. Verifique API key e instalacao."
            )

        try:
            import anthropic

            # Converte mensagens para formato Anthropic
            formatted_messages = [
                {"role": msg.role, "content": msg.content}
                for msg in messages
            ]

            # Chama API
            response = self.client.messages.create(
                model=kwargs.get("model", self.config.model),
                max_tokens=kwargs.get("max_tokens", self.config.max_tokens),
                system=system_prompt or "Voce e um assistente inteligente.",
                messages=formatted_messages
            )

            # Extrai conteudo
            content = ""
            if response.content:
                for block in response.content:
                    if hasattr(block, 'text'):
                        content += block.text

            latency = (time.time() - start_time) * 1000

            return LLMResponse(
                success=True,
                content=content,
                provider="claude",
                model=response.model,
                tokens_used=response.usage.output_tokens if response.usage else 0,
                latency_ms=latency
            )

        except anthropic.APIError as e:
            return LLMResponse(
                success=False,
                content="",
                provider="claude",
                error=f"Erro na API Anthropic: {str(e)}"
            )
        except Exception as e:
            return LLMResponse(
                success=False,
                content="",
                provider="claude",
                error=f"Erro inesperado: {str(e)}"
            )

    async def test_connection(self) -> bool:
        """Testa conexao com Claude"""
        if not self.is_available():
            return False

        try:
            response = await self.complete("Responda apenas 'OK'", max_tokens=10)
            return response.success
        except Exception:
            return False


# =============================================================================
# IMPLEMENTACAO AZURE OPENAI
# =============================================================================

class AzureOpenAIClient(LLMClient):
    """
    Cliente para Azure OpenAI Service

    Requer:
        - AZURE_OPENAI_KEY no ambiente
        - AZURE_OPENAI_ENDPOINT no ambiente
        - AZURE_OPENAI_DEPLOYMENT no ambiente
        - pip install openai
    """

    DEFAULT_MODEL = "gpt-4"
    API_VERSION = "2024-02-15-preview"

    def __init__(self, config: Optional[LLMConfig] = None):
        super().__init__(config)
        self.client = None
        self._init_client()

    def _default_config(self) -> LLMConfig:
        return LLMConfig(
            provider=LLMProvider.AZURE_OPENAI,
            api_key=os.environ.get("AZURE_OPENAI_KEY"),
            endpoint=os.environ.get("AZURE_OPENAI_ENDPOINT"),
            model=os.environ.get("AZURE_OPENAI_DEPLOYMENT", self.DEFAULT_MODEL),
            max_tokens=int(os.environ.get("AZURE_OPENAI_MAX_TOKENS", 4096)),
            extra_params={
                "api_version": os.environ.get("AZURE_OPENAI_API_VERSION", self.API_VERSION)
            }
        )

    def _init_client(self):
        """Inicializa cliente Azure OpenAI"""
        try:
            from openai import AzureOpenAI

            if self.config.api_key and self.config.endpoint:
                self.client = AzureOpenAI(
                    api_key=self.config.api_key,
                    api_version=self.config.extra_params.get("api_version", self.API_VERSION),
                    azure_endpoint=self.config.endpoint
                )
                self._is_initialized = True
                print("[AzureOpenAIClient] Inicializado com sucesso")
            else:
                missing = []
                if not self.config.api_key:
                    missing.append("AZURE_OPENAI_KEY")
                if not self.config.endpoint:
                    missing.append("AZURE_OPENAI_ENDPOINT")
                print(f"[AzureOpenAIClient] AVISO: Variaveis faltando: {', '.join(missing)}")
        except ImportError:
            print("[AzureOpenAIClient] ERRO: openai nao instalado. Execute: pip install openai")

    def is_available(self) -> bool:
        return self.client is not None and self._is_initialized

    async def complete(self, prompt: str, **kwargs) -> LLMResponse:
        """Completa um prompt usando Azure OpenAI"""
        messages = [LLMMessage(role="user", content=prompt)]
        return await self.chat(messages, **kwargs)

    async def chat(self, messages: List[LLMMessage],
                   system_prompt: Optional[str] = None,
                   **kwargs) -> LLMResponse:
        """Chat com Azure OpenAI"""
        import time
        start_time = time.time()

        if not self.is_available():
            return LLMResponse(
                success=False,
                content="",
                provider="azure_openai",
                error="Azure OpenAI nao disponivel. Verifique configuracoes."
            )

        try:
            # Monta mensagens no formato OpenAI
            formatted_messages = []

            if system_prompt:
                formatted_messages.append({
                    "role": "system",
                    "content": system_prompt
                })

            for msg in messages:
                formatted_messages.append({
                    "role": msg.role,
                    "content": msg.content
                })

            # Chama API
            response = self.client.chat.completions.create(
                model=kwargs.get("model", self.config.model),
                messages=formatted_messages,
                max_tokens=kwargs.get("max_tokens", self.config.max_tokens),
                temperature=kwargs.get("temperature", self.config.temperature)
            )

            content = response.choices[0].message.content if response.choices else ""
            latency = (time.time() - start_time) * 1000

            return LLMResponse(
                success=True,
                content=content,
                provider="azure_openai",
                model=response.model,
                tokens_used=response.usage.completion_tokens if response.usage else 0,
                latency_ms=latency
            )

        except Exception as e:
            return LLMResponse(
                success=False,
                content="",
                provider="azure_openai",
                error=f"Erro Azure OpenAI: {str(e)}"
            )

    async def test_connection(self) -> bool:
        """Testa conexao com Azure OpenAI"""
        if not self.is_available():
            return False

        try:
            response = await self.complete("Responda apenas 'OK'", max_tokens=10)
            return response.success
        except Exception:
            return False


# =============================================================================
# IMPLEMENTACAO AWS BEDROCK
# =============================================================================

class AWSBedrockClient(LLMClient):
    """
    Cliente para AWS Bedrock

    Requer:
        - AWS_ACCESS_KEY_ID no ambiente
        - AWS_SECRET_ACCESS_KEY no ambiente
        - AWS_BEDROCK_REGION no ambiente
        - pip install boto3
    """

    # Modelos disponiveis no Bedrock
    MODELS = {
        "claude-3-sonnet": "anthropic.claude-3-sonnet-20240229-v1:0",
        "claude-3-haiku": "anthropic.claude-3-haiku-20240307-v1:0",
        "claude-3-opus": "anthropic.claude-3-opus-20240229-v1:0",
        "titan-text": "amazon.titan-text-express-v1",
        "llama2-70b": "meta.llama2-70b-chat-v1"
    }
    DEFAULT_MODEL = "anthropic.claude-3-sonnet-20240229-v1:0"

    def __init__(self, config: Optional[LLMConfig] = None):
        super().__init__(config)
        self.client = None
        self._init_client()

    def _default_config(self) -> LLMConfig:
        return LLMConfig(
            provider=LLMProvider.AWS_BEDROCK,
            region=os.environ.get("AWS_BEDROCK_REGION", "us-east-1"),
            model=os.environ.get("AWS_BEDROCK_MODEL", self.DEFAULT_MODEL),
            max_tokens=int(os.environ.get("AWS_BEDROCK_MAX_TOKENS", 4096)),
            extra_params={
                "access_key": os.environ.get("AWS_ACCESS_KEY_ID"),
                "secret_key": os.environ.get("AWS_SECRET_ACCESS_KEY")
            }
        )

    def _init_client(self):
        """Inicializa cliente AWS Bedrock"""
        try:
            import boto3

            access_key = self.config.extra_params.get("access_key")
            secret_key = self.config.extra_params.get("secret_key")
            region = self.config.region

            if access_key and secret_key:
                self.client = boto3.client(
                    "bedrock-runtime",
                    region_name=region,
                    aws_access_key_id=access_key,
                    aws_secret_access_key=secret_key
                )
                self._is_initialized = True
                print(f"[AWSBedrockClient] Inicializado na regiao {region}")
            elif os.environ.get("AWS_PROFILE"):
                # Usa profile configurado
                self.client = boto3.client(
                    "bedrock-runtime",
                    region_name=region
                )
                self._is_initialized = True
                print(f"[AWSBedrockClient] Inicializado com AWS profile na regiao {region}")
            else:
                print("[AWSBedrockClient] AVISO: Credenciais AWS nao configuradas")
        except ImportError:
            print("[AWSBedrockClient] ERRO: boto3 nao instalado. Execute: pip install boto3")
        except Exception as e:
            print(f"[AWSBedrockClient] ERRO: {str(e)}")

    def is_available(self) -> bool:
        return self.client is not None and self._is_initialized

    async def complete(self, prompt: str, **kwargs) -> LLMResponse:
        """Completa um prompt usando AWS Bedrock"""
        messages = [LLMMessage(role="user", content=prompt)]
        return await self.chat(messages, **kwargs)

    async def chat(self, messages: List[LLMMessage],
                   system_prompt: Optional[str] = None,
                   **kwargs) -> LLMResponse:
        """Chat com AWS Bedrock"""
        import time
        start_time = time.time()

        if not self.is_available():
            return LLMResponse(
                success=False,
                content="",
                provider="aws_bedrock",
                error="AWS Bedrock nao disponivel. Verifique credenciais AWS."
            )

        try:
            model_id = kwargs.get("model", self.config.model)

            # Determina formato do payload baseado no modelo
            if "anthropic" in model_id:
                payload = self._format_anthropic_payload(messages, system_prompt, **kwargs)
            elif "amazon" in model_id:
                payload = self._format_titan_payload(messages, system_prompt, **kwargs)
            elif "meta" in model_id:
                payload = self._format_llama_payload(messages, system_prompt, **kwargs)
            else:
                payload = self._format_anthropic_payload(messages, system_prompt, **kwargs)

            # Chama Bedrock
            response = self.client.invoke_model(
                modelId=model_id,
                body=json.dumps(payload),
                contentType="application/json",
                accept="application/json"
            )

            # Parse resposta
            response_body = json.loads(response["body"].read())
            content = self._extract_content(response_body, model_id)

            latency = (time.time() - start_time) * 1000

            return LLMResponse(
                success=True,
                content=content,
                provider="aws_bedrock",
                model=model_id,
                latency_ms=latency,
                raw_response=response_body
            )

        except Exception as e:
            return LLMResponse(
                success=False,
                content="",
                provider="aws_bedrock",
                error=f"Erro AWS Bedrock: {str(e)}"
            )

    def _format_anthropic_payload(self, messages: List[LLMMessage],
                                   system_prompt: Optional[str], **kwargs) -> Dict:
        """Formata payload para modelos Anthropic no Bedrock"""
        formatted_messages = [
            {"role": msg.role, "content": msg.content}
            for msg in messages
        ]

        return {
            "anthropic_version": "bedrock-2023-05-31",
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "system": system_prompt or "Voce e um assistente inteligente.",
            "messages": formatted_messages
        }

    def _format_titan_payload(self, messages: List[LLMMessage],
                               system_prompt: Optional[str], **kwargs) -> Dict:
        """Formata payload para Amazon Titan"""
        # Concatena mensagens em texto
        text_parts = []
        if system_prompt:
            text_parts.append(f"System: {system_prompt}")

        for msg in messages:
            role = "User" if msg.role == "user" else "Assistant"
            text_parts.append(f"{role}: {msg.content}")

        return {
            "inputText": "\n\n".join(text_parts),
            "textGenerationConfig": {
                "maxTokenCount": kwargs.get("max_tokens", self.config.max_tokens),
                "temperature": kwargs.get("temperature", self.config.temperature)
            }
        }

    def _format_llama_payload(self, messages: List[LLMMessage],
                               system_prompt: Optional[str], **kwargs) -> Dict:
        """Formata payload para Meta Llama"""
        # Formato Llama 2 chat
        prompt_parts = []

        if system_prompt:
            prompt_parts.append(f"<s>[INST] <<SYS>>\n{system_prompt}\n<</SYS>>\n\n")
        else:
            prompt_parts.append("<s>[INST] ")

        for i, msg in enumerate(messages):
            if msg.role == "user":
                if i > 0:
                    prompt_parts.append("[INST] ")
                prompt_parts.append(f"{msg.content} [/INST]")
            else:
                prompt_parts.append(f" {msg.content} </s>")

        return {
            "prompt": "".join(prompt_parts),
            "max_gen_len": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature)
        }

    def _extract_content(self, response_body: Dict, model_id: str) -> str:
        """Extrai conteudo da resposta baseado no modelo"""
        if "anthropic" in model_id:
            if "content" in response_body:
                return response_body["content"][0].get("text", "")
        elif "amazon" in model_id:
            if "results" in response_body:
                return response_body["results"][0].get("outputText", "")
        elif "meta" in model_id:
            return response_body.get("generation", "")

        return str(response_body)

    async def test_connection(self) -> bool:
        """Testa conexao com AWS Bedrock"""
        if not self.is_available():
            return False

        try:
            response = await self.complete("Responda apenas 'OK'", max_tokens=10)
            return response.success
        except Exception:
            return False


# =============================================================================
# IMPLEMENTACAO GOOGLE VERTEX AI
# =============================================================================

class GoogleVertexClient(LLMClient):
    """
    Cliente para Google Vertex AI

    Requer:
        - GOOGLE_VERTEX_PROJECT no ambiente
        - GOOGLE_VERTEX_LOCATION no ambiente
        - GOOGLE_APPLICATION_CREDENTIALS no ambiente (path para service account JSON)
        - pip install google-cloud-aiplatform
    """

    # Modelos disponiveis
    MODELS = {
        "gemini-pro": "gemini-1.0-pro",
        "gemini-pro-vision": "gemini-1.0-pro-vision",
        "gemini-1.5-pro": "gemini-1.5-pro-preview-0514",
        "palm2": "text-bison@002"
    }
    DEFAULT_MODEL = "gemini-1.0-pro"

    def __init__(self, config: Optional[LLMConfig] = None):
        super().__init__(config)
        self.client = None
        self._init_client()

    def _default_config(self) -> LLMConfig:
        return LLMConfig(
            provider=LLMProvider.GOOGLE_VERTEX,
            project_id=os.environ.get("GOOGLE_VERTEX_PROJECT"),
            region=os.environ.get("GOOGLE_VERTEX_LOCATION", "us-central1"),
            model=os.environ.get("GOOGLE_VERTEX_MODEL", self.DEFAULT_MODEL),
            max_tokens=int(os.environ.get("GOOGLE_VERTEX_MAX_TOKENS", 4096)),
            extra_params={
                "credentials_path": os.environ.get("GOOGLE_APPLICATION_CREDENTIALS")
            }
        )

    def _init_client(self):
        """Inicializa cliente Google Vertex AI"""
        try:
            import vertexai
            from vertexai.generative_models import GenerativeModel

            project_id = self.config.project_id
            location = self.config.region

            if project_id:
                vertexai.init(project=project_id, location=location)
                self.client = GenerativeModel(self.config.model)
                self._is_initialized = True
                print(f"[GoogleVertexClient] Inicializado no projeto {project_id}")
            else:
                print("[GoogleVertexClient] AVISO: GOOGLE_VERTEX_PROJECT nao configurado")
        except ImportError:
            print("[GoogleVertexClient] ERRO: google-cloud-aiplatform nao instalado.")
            print("Execute: pip install google-cloud-aiplatform")
        except Exception as e:
            print(f"[GoogleVertexClient] ERRO: {str(e)}")

    def is_available(self) -> bool:
        return self.client is not None and self._is_initialized

    async def complete(self, prompt: str, **kwargs) -> LLMResponse:
        """Completa um prompt usando Google Vertex AI"""
        messages = [LLMMessage(role="user", content=prompt)]
        return await self.chat(messages, **kwargs)

    async def chat(self, messages: List[LLMMessage],
                   system_prompt: Optional[str] = None,
                   **kwargs) -> LLMResponse:
        """Chat com Google Vertex AI"""
        import time
        start_time = time.time()

        if not self.is_available():
            return LLMResponse(
                success=False,
                content="",
                provider="google_vertex",
                error="Google Vertex AI nao disponivel. Verifique configuracoes."
            )

        try:
            from vertexai.generative_models import GenerationConfig

            # Monta o prompt
            prompt_parts = []

            if system_prompt:
                prompt_parts.append(f"System: {system_prompt}\n\n")

            for msg in messages:
                role = "User" if msg.role == "user" else "Assistant"
                prompt_parts.append(f"{role}: {msg.content}\n\n")

            prompt_parts.append("Assistant: ")
            full_prompt = "".join(prompt_parts)

            # Configura geracao
            generation_config = GenerationConfig(
                max_output_tokens=kwargs.get("max_tokens", self.config.max_tokens),
                temperature=kwargs.get("temperature", self.config.temperature)
            )

            # Gera resposta
            response = self.client.generate_content(
                full_prompt,
                generation_config=generation_config
            )

            content = response.text if hasattr(response, 'text') else str(response)
            latency = (time.time() - start_time) * 1000

            return LLMResponse(
                success=True,
                content=content,
                provider="google_vertex",
                model=self.config.model,
                latency_ms=latency
            )

        except Exception as e:
            return LLMResponse(
                success=False,
                content="",
                provider="google_vertex",
                error=f"Erro Google Vertex: {str(e)}"
            )

    async def test_connection(self) -> bool:
        """Testa conexao com Google Vertex AI"""
        if not self.is_available():
            return False

        try:
            response = await self.complete("Responda apenas 'OK'", max_tokens=10)
            return response.success
        except Exception:
            return False


# =============================================================================
# FACTORY PATTERN
# =============================================================================

# Registry de clientes disponiveis
_CLIENT_REGISTRY: Dict[str, type] = {
    "claude": ClaudeClient,
    "azure_openai": AzureOpenAIClient,
    "aws_bedrock": AWSBedrockClient,
    "google_vertex": GoogleVertexClient
}

# Cache de clientes instanciados
_client_cache: Dict[str, LLMClient] = {}


def get_llm_client(provider: str = None, config: Optional[LLMConfig] = None,
                   use_cache: bool = True) -> LLMClient:
    """
    Factory para obter cliente LLM

    Args:
        provider: Nome do provedor ("claude", "azure_openai", "aws_bedrock", "google_vertex")
                  Se None, usa LLM_PROVIDER do ambiente ou "claude" como padrao
        config: Configuracao customizada (opcional)
        use_cache: Se True, reutiliza instancia existente

    Returns:
        Instancia de LLMClient para o provedor especificado

    Raises:
        ValueError: Se provedor nao suportado

    Exemplo:
        client = get_llm_client("claude")
        response = await client.complete("Ola!")
    """
    # Determina provedor
    if provider is None:
        provider = os.environ.get("LLM_PROVIDER", "claude")

    provider = provider.lower()

    # Valida provedor
    if provider not in _CLIENT_REGISTRY:
        available = ", ".join(_CLIENT_REGISTRY.keys())
        raise ValueError(f"Provedor '{provider}' nao suportado. Disponiveis: {available}")

    # Verifica cache
    cache_key = f"{provider}_{id(config) if config else 'default'}"
    if use_cache and cache_key in _client_cache:
        return _client_cache[cache_key]

    # Cria nova instancia
    client_class = _CLIENT_REGISTRY[provider]
    client = client_class(config)

    # Armazena no cache
    if use_cache:
        _client_cache[cache_key] = client

    return client


def register_provider(name: str, client_class: type):
    """
    Registra um novo provedor de LLM

    Args:
        name: Nome do provedor
        client_class: Classe que implementa LLMClient

    Exemplo:
        class MyCustomClient(LLMClient):
            ...

        register_provider("my_custom", MyCustomClient)
    """
    if not issubclass(client_class, LLMClient):
        raise TypeError(f"{client_class} deve herdar de LLMClient")

    _CLIENT_REGISTRY[name.lower()] = client_class


def get_available_providers() -> List[Dict[str, Any]]:
    """
    Retorna lista de provedores disponiveis com status

    Returns:
        Lista de dicionarios com info de cada provedor
    """
    providers = []

    for name, client_class in _CLIENT_REGISTRY.items():
        try:
            client = get_llm_client(name)
            status = "available" if client.is_available() else "not_configured"
        except Exception as e:
            status = f"error: {str(e)}"

        providers.append({
            "name": name,
            "display_name": name.replace("_", " ").title(),
            "status": status,
            "is_available": status == "available"
        })

    return providers


async def test_all_providers() -> Dict[str, bool]:
    """
    Testa conexao com todos os provedores

    Returns:
        Dicionario com resultado de cada provedor
    """
    results = {}

    for name in _CLIENT_REGISTRY.keys():
        try:
            client = get_llm_client(name)
            results[name] = await client.test_connection()
        except Exception:
            results[name] = False

    return results


def clear_client_cache():
    """Limpa cache de clientes"""
    _client_cache.clear()


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_default_provider() -> str:
    """Retorna provedor padrao configurado"""
    return os.environ.get("LLM_PROVIDER", "claude")


def set_default_provider(provider: str):
    """Define provedor padrao via variavel de ambiente"""
    os.environ["LLM_PROVIDER"] = provider


# =============================================================================
# COMPATIBILIDADE COM CODIGO EXISTENTE
# =============================================================================

def get_claude_client_compat() -> LLMClient:
    """
    Funcao de compatibilidade com codigo existente

    Retorna cliente Claude usando nova interface
    """
    return get_llm_client("claude")


# Exporta para uso externo
__all__ = [
    # Classes base
    "LLMClient",
    "LLMConfig",
    "LLMResponse",
    "LLMMessage",
    "LLMProvider",

    # Implementacoes
    "ClaudeClient",
    "AzureOpenAIClient",
    "AWSBedrockClient",
    "GoogleVertexClient",

    # Factory
    "get_llm_client",
    "register_provider",
    "get_available_providers",
    "test_all_providers",
    "clear_client_cache",

    # Helpers
    "get_default_provider",
    "set_default_provider",
    "get_claude_client_compat"
]
