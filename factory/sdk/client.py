# -*- coding: utf-8 -*-
"""
Cliente SDK - Plataforma E
================================

Cliente principal para interacao com a API da Plataforma E.
"""
import os
import time
from typing import Optional, List, Dict, Any
from urllib.parse import urljoin

import requests

from factory.sdk.models import (
    Project, Story, Job, Webhook, APIKeyInfo, PaginatedList
)
from factory.sdk.exceptions import (
    FabricaError,
    AuthenticationError,
    AuthorizationError,
    RateLimitError,
    NotFoundError,
    ValidationError,
    ServerError,
    ConnectionError as FabricaConnectionError,
)


# =============================================================================
# CONFIGURACAO
# =============================================================================

DEFAULT_BASE_URL = "http://localhost:9001"
DEFAULT_TIMEOUT = 30
DEFAULT_MAX_RETRIES = 3
USER_AGENT = "fabrica-sdk-python/1.0.0"


# =============================================================================
# HTTP CLIENT
# =============================================================================

class HTTPClient:
    """
    Cliente HTTP para comunicacao com a API.

    Gerencia autenticacao, rate limiting, retries e parsing de erros.
    """

    def __init__(
        self,
        api_key: str,
        base_url: str = None,
        timeout: int = DEFAULT_TIMEOUT,
        max_retries: int = DEFAULT_MAX_RETRIES,
    ):
        self.api_key = api_key
        self.base_url = (base_url or os.getenv("FABRICA_API_URL", DEFAULT_BASE_URL)).rstrip("/")
        self.timeout = timeout
        self.max_retries = max_retries

        self.session = requests.Session()
        self.session.headers.update({
            "X-API-Key": api_key,
            "User-Agent": USER_AGENT,
            "Accept": "application/json",
            "Content-Type": "application/json",
        })

    def _build_url(self, path: str) -> str:
        """Constroi URL completa"""
        if not path.startswith("/"):
            path = f"/{path}"
        if not path.startswith("/api/v1"):
            path = f"/api/v1{path}"
        return f"{self.base_url}{path}"

    def _handle_response(self, response: requests.Response) -> Dict:
        """Processa resposta e levanta excecoes apropriadas"""
        status_code = response.status_code

        # Tentar parse do JSON
        try:
            data = response.json()
        except ValueError:
            data = {"message": response.text}

        # Sucesso
        if 200 <= status_code < 300:
            return data

        # Erros de cliente
        error_message = data.get("message") or data.get("detail") or str(data)

        if status_code == 401:
            raise AuthenticationError(
                message=error_message,
                error_code=data.get("error"),
                details=data
            )

        if status_code == 403:
            raise AuthorizationError(
                message=error_message,
                error_code=data.get("error"),
                details=data
            )

        if status_code == 404:
            raise NotFoundError(
                message=error_message,
                error_code=data.get("error"),
                details=data
            )

        if status_code == 429:
            retry_after = int(response.headers.get("Retry-After", 60))
            limit = int(response.headers.get("X-RateLimit-Limit", 100))
            remaining = int(response.headers.get("X-RateLimit-Remaining", 0))
            raise RateLimitError(
                message=error_message,
                retry_after=retry_after,
                limit=limit,
                remaining=remaining,
                details=data
            )

        if status_code == 400 or status_code == 422:
            errors = data.get("detail", []) if isinstance(data.get("detail"), list) else []
            raise ValidationError(
                message=error_message,
                errors=errors,
                details=data
            )

        if status_code >= 500:
            raise ServerError(
                message=error_message,
                status_code=status_code,
                details=data
            )

        # Erro generico
        raise FabricaError(
            message=error_message,
            status_code=status_code,
            details=data
        )

    def request(
        self,
        method: str,
        path: str,
        params: Dict = None,
        json: Dict = None,
        retry_on_rate_limit: bool = True,
    ) -> Dict:
        """
        Executa request HTTP.

        Args:
            method: GET, POST, PUT, DELETE, PATCH
            path: Caminho da API
            params: Query parameters
            json: Body JSON
            retry_on_rate_limit: Se deve retry automatico em 429

        Returns:
            Resposta JSON parseada
        """
        url = self._build_url(path)
        retries = 0

        while retries <= self.max_retries:
            try:
                response = self.session.request(
                    method=method,
                    url=url,
                    params=params,
                    json=json,
                    timeout=self.timeout,
                )
                return self._handle_response(response)

            except RateLimitError as e:
                if not retry_on_rate_limit or retries >= self.max_retries:
                    raise
                # Aguardar e tentar novamente
                time.sleep(min(e.retry_after, 60))
                retries += 1

            except requests.exceptions.ConnectionError as e:
                raise FabricaConnectionError(
                    f"Erro de conexao com {self.base_url}: {e}"
                )

            except requests.exceptions.Timeout as e:
                raise FabricaConnectionError(
                    f"Timeout na conexao com {self.base_url}: {e}"
                )

            except requests.exceptions.RequestException as e:
                raise FabricaError(f"Erro na requisicao: {e}")

        raise FabricaError(f"Maximo de retries ({self.max_retries}) excedido")

    def get(self, path: str, **kwargs) -> Dict:
        """GET request"""
        return self.request("GET", path, **kwargs)

    def post(self, path: str, **kwargs) -> Dict:
        """POST request"""
        return self.request("POST", path, **kwargs)

    def put(self, path: str, **kwargs) -> Dict:
        """PUT request"""
        return self.request("PUT", path, **kwargs)

    def patch(self, path: str, **kwargs) -> Dict:
        """PATCH request"""
        return self.request("PATCH", path, **kwargs)

    def delete(self, path: str, **kwargs) -> Dict:
        """DELETE request"""
        return self.request("DELETE", path, **kwargs)


# =============================================================================
# RECURSOS
# =============================================================================

class ProjectsResource:
    """Operacoes com projetos"""

    def __init__(self, http: HTTPClient):
        self._http = http

    def list(
        self,
        page: int = 1,
        per_page: int = 20,
        status: str = None
    ) -> PaginatedList:
        """
        Lista projetos.

        Args:
            page: Pagina (default: 1)
            per_page: Itens por pagina (default: 20, max: 100)
            status: Filtrar por status

        Returns:
            PaginatedList[Project]
        """
        params = {"page": page, "per_page": per_page}
        if status:
            params["status"] = status

        data = self._http.get("/projects", params=params)

        projects = [
            Project.from_dict(p, client=self._http)
            for p in data.get("projects", [])
        ]

        return PaginatedList(
            items=projects,
            total=data.get("total", len(projects)),
            page=data.get("page", page),
            per_page=data.get("per_page", per_page),
            has_next=data.get("has_next", False),
        )

    def get(self, project_id: str) -> Project:
        """
        Busca projeto por ID.

        Args:
            project_id: ID do projeto (ex: PRJ-ABC123)

        Returns:
            Project
        """
        data = self._http.get(f"/projects/{project_id}")
        return Project.from_dict(data, client=self._http)

    def create(
        self,
        name: str,
        project_type: str = "web-app",
        description: str = None,
    ) -> Project:
        """
        Cria um novo projeto.

        Args:
            name: Nome do projeto
            project_type: Tipo (web-app, api-service, data-analysis, automation)
            description: Descricao

        Returns:
            Project criado
        """
        data = self._http.post("/projects", json={
            "name": name,
            "project_type": project_type,
            "description": description,
        })
        return Project.from_dict(data, client=self._http)


class StoriesResource:
    """Operacoes com stories"""

    def __init__(self, http: HTTPClient):
        self._http = http

    def list(
        self,
        project_id: str,
        status: str = None
    ) -> List[Story]:
        """
        Lista stories de um projeto.

        Args:
            project_id: ID do projeto
            status: Filtrar por status

        Returns:
            Lista de Story
        """
        params = {}
        if status:
            params["status"] = status

        data = self._http.get(f"/projects/{project_id}/stories", params=params)

        return [
            Story.from_dict(s, client=self._http)
            for s in data.get("stories", [])
        ]

    def get(self, story_id: str) -> Story:
        """
        Busca story por ID.

        Args:
            story_id: ID da story

        Returns:
            Story
        """
        # Nota: endpoint get por ID nao implementado na API ainda
        # Esta e uma simplificacao
        raise NotImplementedError("Endpoint nao implementado ainda")

    def create(
        self,
        project_id: str,
        title: str,
        persona: str = None,
        action: str = None,
        benefit: str = None,
        acceptance_criteria: List[str] = None,
        story_points: int = 0,
        priority: str = "medium",
    ) -> Story:
        """
        Cria uma nova story.

        Args:
            project_id: ID do projeto
            title: Titulo da story
            persona: "Como um [tipo de usuario]"
            action: "Eu quero [funcionalidade]"
            benefit: "Para que [beneficio]"
            acceptance_criteria: Lista de criterios
            story_points: Pontos (Fibonacci)
            priority: Prioridade

        Returns:
            Story criada
        """
        data = self._http.post(f"/projects/{project_id}/stories", json={
            "title": title,
            "persona": persona,
            "action": action,
            "benefit": benefit,
            "acceptance_criteria": acceptance_criteria or [],
            "story_points": story_points,
            "priority": priority,
        })
        return Story.from_dict(data, client=self._http)

    def execute(self, story_id: str) -> Job:
        """
        Inicia desenvolvimento autonomo da story.

        Args:
            story_id: ID da story

        Returns:
            Job de desenvolvimento
        """
        data = self._http.post(f"/stories/{story_id}/execute")
        return Job.from_dict(data, client=self._http)


class JobsResource:
    """Operacoes com jobs"""

    def __init__(self, http: HTTPClient):
        self._http = http

    def list(
        self,
        status: str = None,
        limit: int = 20
    ) -> List[Job]:
        """
        Lista jobs.

        Args:
            status: Filtrar por status
            limit: Maximo de itens

        Returns:
            Lista de Job
        """
        params = {"limit": limit}
        if status:
            params["status"] = status

        data = self._http.get("/jobs", params=params)

        return [
            Job.from_dict(j, client=self._http)
            for j in data.get("jobs", [])
        ]

    def get(self, job_id: str) -> Job:
        """
        Busca job por ID.

        Args:
            job_id: ID do job

        Returns:
            Job
        """
        data = self._http.get(f"/jobs/{job_id}")
        return Job.from_dict(data, client=self._http)

    def create(
        self,
        description: str,
        tech_stack: str = None,
        features: List[str] = None,
        story_id: str = None,
    ) -> Job:
        """
        Cria um novo job de desenvolvimento.

        Args:
            description: Descricao do que construir
            tech_stack: Stack tecnologica
            features: Lista de features
            story_id: ID da story associada

        Returns:
            Job criado
        """
        data = self._http.post("/jobs", json={
            "description": description,
            "tech_stack": tech_stack,
            "features": features or [],
            "story_id": story_id,
        })
        return Job.from_dict(data, client=self._http)


class WebhooksResource:
    """Operacoes com webhooks"""

    def __init__(self, http: HTTPClient):
        self._http = http

    def list(self) -> List[Webhook]:
        """
        Lista webhooks configurados.

        Returns:
            Lista de Webhook
        """
        data = self._http.get("/webhooks")
        return [
            Webhook.from_dict(w, client=self._http)
            for w in data.get("webhooks", [])
        ]

    def create(
        self,
        name: str,
        url: str,
        events: List[str],
        secret: str = None,
    ) -> Webhook:
        """
        Cria um novo webhook.

        Args:
            name: Nome amigavel
            url: URL de destino (HTTPS recomendado)
            events: Lista de eventos para assinar
            secret: Secret para assinatura HMAC (gerado automaticamente se nao fornecido)

        Returns:
            Webhook criado (com secret visivel)
        """
        payload = {
            "name": name,
            "url": url,
            "events": events,
        }
        if secret:
            payload["secret"] = secret

        data = self._http.post("/webhooks", json=payload)
        return Webhook.from_dict(data, client=self._http)

    def delete(self, webhook_id: str) -> bool:
        """
        Remove um webhook.

        Args:
            webhook_id: ID do webhook

        Returns:
            True se removido com sucesso
        """
        self._http.delete(f"/webhooks/{webhook_id}")
        return True


class APIKeysResource:
    """Operacoes com API Keys"""

    def __init__(self, http: HTTPClient):
        self._http = http

    def get_current(self) -> APIKeyInfo:
        """
        Retorna informacoes da API Key atual.

        Returns:
            APIKeyInfo
        """
        data = self._http.get("/api-keys/me")
        return APIKeyInfo.from_dict(data)

    def list(self) -> List[dict]:
        """
        Lista API Keys do usuario.

        Returns:
            Lista de dicionarios com info das keys (sem a chave completa)
        """
        data = self._http.get("/api-keys")
        return data.get("api_keys", [])


# =============================================================================
# CLIENTE PRINCIPAL
# =============================================================================

class FabricaClient:
    """
    Cliente principal para a API da Plataforma E.

    Usage:
        from fabrica import FabricaClient

        client = FabricaClient(api_key='sk-fab_xxx_yyy')

        # Listar projetos
        projects = client.projects.list()

        # Criar projeto
        project = client.projects.create(
            name='Meu App',
            project_type='web-app'
        )

        # Criar story
        story = client.stories.create(
            project_id=project.id,
            title='Login com Google',
            persona='usuario',
            action='fazer login com conta Google',
            benefit='nao criar nova senha'
        )

        # Executar desenvolvimento
        job = client.stories.execute(story.id)
        job.wait_for_completion()
    """

    def __init__(
        self,
        api_key: str = None,
        base_url: str = None,
        timeout: int = DEFAULT_TIMEOUT,
        max_retries: int = DEFAULT_MAX_RETRIES,
    ):
        """
        Inicializa o cliente.

        Args:
            api_key: API Key (ou use env FABRICA_API_KEY)
            base_url: URL base da API (ou use env FABRICA_API_URL)
            timeout: Timeout para requests em segundos
            max_retries: Maximo de retries para rate limiting
        """
        api_key = api_key or os.getenv("FABRICA_API_KEY")

        if not api_key:
            raise AuthenticationError(
                "API Key nao fornecida. Use o parametro api_key ou "
                "defina a variavel de ambiente FABRICA_API_KEY"
            )

        self._http = HTTPClient(
            api_key=api_key,
            base_url=base_url,
            timeout=timeout,
            max_retries=max_retries,
        )

        # Recursos
        self.projects = ProjectsResource(self._http)
        self.stories = StoriesResource(self._http)
        self.jobs = JobsResource(self._http)
        self.webhooks = WebhooksResource(self._http)
        self.api_keys = APIKeysResource(self._http)

    def health_check(self) -> dict:
        """
        Verifica saude da API.

        Returns:
            Status da API
        """
        return self._http.get("/health")

    def get_api_info(self) -> dict:
        """
        Retorna informacoes da API.

        Returns:
            Informacoes da API (versao, docs, etc)
        """
        return self._http.get("/")

    def __repr__(self):
        return f"FabricaClient(base_url={self._http.base_url!r})"
