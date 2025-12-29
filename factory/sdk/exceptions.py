# -*- coding: utf-8 -*-
"""
Excecoes do SDK - Fabrica de Agentes
====================================

Excecoes customizadas para tratamento de erros da API.
"""


class FabricaError(Exception):
    """
    Excecao base para erros da Fabrica de Agentes.

    Attributes:
        message: Mensagem de erro
        status_code: Codigo HTTP (se aplicavel)
        error_code: Codigo de erro da API
        details: Detalhes adicionais do erro
    """

    def __init__(
        self,
        message: str,
        status_code: int = None,
        error_code: str = None,
        details: dict = None
    ):
        self.message = message
        self.status_code = status_code
        self.error_code = error_code
        self.details = details or {}
        super().__init__(self.message)

    def __str__(self):
        parts = [self.message]
        if self.error_code:
            parts.insert(0, f"[{self.error_code}]")
        if self.status_code:
            parts.append(f"(HTTP {self.status_code})")
        return " ".join(parts)

    def __repr__(self):
        return f"{self.__class__.__name__}({self.message!r}, status_code={self.status_code})"


class AuthenticationError(FabricaError):
    """
    Erro de autenticacao.

    Levantado quando:
    - API Key nao fornecida
    - API Key invalida
    - API Key expirada
    - API Key revogada
    """

    def __init__(self, message: str = "Erro de autenticacao", **kwargs):
        super().__init__(message, status_code=401, **kwargs)


class AuthorizationError(FabricaError):
    """
    Erro de autorizacao (permissao insuficiente).

    Levantado quando:
    - Scope insuficiente para a operacao
    - Tier insuficiente para a operacao
    """

    def __init__(self, message: str = "Permissao insuficiente", **kwargs):
        super().__init__(message, status_code=403, **kwargs)


class RateLimitError(FabricaError):
    """
    Erro de rate limit excedido.

    Attributes:
        retry_after: Segundos para aguardar antes de tentar novamente
        limit: Limite de requisicoes
        remaining: Requisicoes restantes
    """

    def __init__(
        self,
        message: str = "Rate limit excedido",
        retry_after: int = 60,
        limit: int = None,
        remaining: int = 0,
        **kwargs
    ):
        super().__init__(message, status_code=429, **kwargs)
        self.retry_after = retry_after
        self.limit = limit
        self.remaining = remaining

    def __str__(self):
        base = super().__str__()
        return f"{base} - Aguarde {self.retry_after}s"


class NotFoundError(FabricaError):
    """
    Recurso nao encontrado.

    Levantado quando:
    - Projeto nao existe
    - Story nao existe
    - Job nao existe
    - Webhook nao existe
    """

    def __init__(self, message: str = "Recurso nao encontrado", resource_type: str = None, resource_id: str = None, **kwargs):
        super().__init__(message, status_code=404, **kwargs)
        self.resource_type = resource_type
        self.resource_id = resource_id


class ValidationError(FabricaError):
    """
    Erro de validacao dos dados enviados.

    Attributes:
        errors: Lista de erros de validacao
    """

    def __init__(self, message: str = "Dados invalidos", errors: list = None, **kwargs):
        super().__init__(message, status_code=400, **kwargs)
        self.errors = errors or []

    def __str__(self):
        base = super().__str__()
        if self.errors:
            error_list = "; ".join(str(e) for e in self.errors[:3])
            return f"{base}: {error_list}"
        return base


class ServerError(FabricaError):
    """
    Erro interno do servidor.

    Levantado para erros 5xx da API.
    """

    def __init__(self, message: str = "Erro interno do servidor", **kwargs):
        super().__init__(message, status_code=500, **kwargs)


class ConnectionError(FabricaError):
    """
    Erro de conexao com a API.

    Levantado quando:
    - Timeout na conexao
    - Servidor indisponivel
    - Erro de rede
    """

    def __init__(self, message: str = "Erro de conexao com a API", **kwargs):
        super().__init__(message, **kwargs)


class TimeoutError(FabricaError):
    """
    Timeout na operacao.

    Levantado quando:
    - Request excede timeout
    - Job excede tempo maximo de espera
    """

    def __init__(self, message: str = "Timeout na operacao", **kwargs):
        super().__init__(message, **kwargs)
