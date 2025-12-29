"""
OpenAPI Spec Generator - Issue #114
===================================

Implementacao de API-First Design Pattern com geracao automatica de OpenAPI 3.0 spec.

Funcionalidades:
- Geracao automatica de spec OpenAPI 3.0 a partir de rotas FastAPI
- Validacao de requests contra schema
- Documentacao automatica
- Suporte a versionamento
- Exportacao para YAML e JSON

Uso:
    from factory.api.openapi_spec import OpenAPIGenerator, validate_request

    generator = OpenAPIGenerator(app)

    # Gerar spec
    spec = generator.generate()

    # Exportar
    generator.export_yaml("openapi.yaml")
    generator.export_json("openapi.json")

    # Validar request
    @router.post("/items")
    @validate_request
    async def create_item(item: ItemCreate):
        ...
"""

import json
import yaml
import re
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Set, Type, Union
from functools import wraps
from pathlib import Path
from enum import Enum

from fastapi import FastAPI, Request, Response, HTTPException
from fastapi.routing import APIRoute
from fastapi.openapi.utils import get_openapi
from pydantic import BaseModel, Field
from starlette.middleware.base import BaseHTTPMiddleware


# =============================================================================
# OpenAPI Models
# =============================================================================

class ServerInfo(BaseModel):
    """Informacoes do servidor"""
    url: str
    description: str = ""
    variables: Dict[str, Dict[str, Any]] = {}


class ContactInfo(BaseModel):
    """Informacoes de contato"""
    name: str = "Fabrica de Agentes Team"
    email: str = "support@fabricadeagentes.com"
    url: str = "https://github.com/cruzpeanelo/fabrica-de-agentes"


class LicenseInfo(BaseModel):
    """Informacoes de licenca"""
    name: str = "Proprietary"
    url: str = "https://github.com/cruzpeanelo/fabrica-de-agentes/blob/main/LICENSE"


class APIInfo(BaseModel):
    """Informacoes da API"""
    title: str = "Fabrica de Agentes API"
    description: str = """
## Fabrica de Agentes - Plataforma de Desenvolvimento Autonomo

API REST para a plataforma de construcao de software com agentes autonomos.

### Recursos Principais

* **Projetos** - Crie e gerencie projetos de software
* **User Stories** - Defina requisitos no formato Agile
* **Agentes** - 242 agentes autonomos especializados
* **Skills** - 39+ skills para processamento de documentos, codigo, multimedia
* **Desenvolvimento Autonomo** - Os agentes desenvolvem codigo automaticamente

### Autenticacao

A API usa JWT (JSON Web Tokens) para autenticacao.

1. Faca login em `/api/auth/login`
2. Use o token retornado no header `Authorization: Bearer <token>`

### Versionamento

A API segue versionamento semantico. Use o header `Accept-Version` ou o prefixo `/api/v1/` para especificar a versao.
"""
    version: str = "3.0.0"
    terms_of_service: str = "https://github.com/cruzpeanelo/fabrica-de-agentes"
    contact: ContactInfo = ContactInfo()
    license_info: LicenseInfo = LicenseInfo()


class TagInfo(BaseModel):
    """Informacoes de uma tag"""
    name: str
    description: str = ""
    external_docs: Optional[Dict[str, str]] = None


class SecuritySchemeType(str, Enum):
    """Tipos de esquemas de seguranca"""
    API_KEY = "apiKey"
    HTTP = "http"
    OAUTH2 = "oauth2"
    OPENID_CONNECT = "openIdConnect"


class SecurityScheme(BaseModel):
    """Esquema de seguranca"""
    type: str
    scheme: Optional[str] = None
    bearer_format: Optional[str] = None
    description: str = ""
    name: Optional[str] = None
    in_: Optional[str] = None  # query, header, cookie


# =============================================================================
# OpenAPI Generator
# =============================================================================

class OpenAPIGenerator:
    """
    Gerador de especificacao OpenAPI 3.0.

    Gera especificacao automaticamente a partir de uma aplicacao FastAPI,
    com suporte a customizacao e extensao.
    """

    def __init__(
        self,
        app: FastAPI,
        info: APIInfo = None,
        servers: List[ServerInfo] = None,
        tags: List[TagInfo] = None,
        security_schemes: Dict[str, SecurityScheme] = None
    ):
        """
        Inicializa o gerador.

        Args:
            app: Aplicacao FastAPI
            info: Informacoes da API
            servers: Lista de servidores
            tags: Lista de tags
            security_schemes: Esquemas de seguranca
        """
        self.app = app
        self.info = info or APIInfo()
        self.servers = servers or self._default_servers()
        self.tags = tags or self._default_tags()
        self.security_schemes = security_schemes or self._default_security_schemes()

    def _default_servers(self) -> List[ServerInfo]:
        """Servidores padrao"""
        return [
            ServerInfo(
                url="http://localhost:9001",
                description="Servidor de desenvolvimento local"
            ),
            ServerInfo(
                url="https://api.fabricadeagentes.com",
                description="Servidor de producao"
            ),
            ServerInfo(
                url="https://staging.fabricadeagentes.com",
                description="Servidor de staging"
            )
        ]

    def _default_tags(self) -> List[TagInfo]:
        """Tags padrao"""
        return [
            TagInfo(
                name="Status",
                description="Endpoints de status e health check"
            ),
            TagInfo(
                name="Authentication",
                description="Autenticacao e autorizacao"
            ),
            TagInfo(
                name="Projects",
                description="Gerenciamento de projetos"
            ),
            TagInfo(
                name="Stories",
                description="Gerenciamento de User Stories"
            ),
            TagInfo(
                name="Jobs",
                description="Fila de jobs e processamento"
            ),
            TagInfo(
                name="Workers",
                description="Workers e processamento autonomo"
            ),
            TagInfo(
                name="Agents",
                description="Agentes IA especializados"
            ),
            TagInfo(
                name="Skills",
                description="Skills disponiveis para agentes"
            ),
            TagInfo(
                name="Queue",
                description="Fila de processamento"
            )
        ]

    def _default_security_schemes(self) -> Dict[str, SecurityScheme]:
        """Esquemas de seguranca padrao"""
        return {
            "BearerAuth": SecurityScheme(
                type="http",
                scheme="bearer",
                bearer_format="JWT",
                description="Token JWT obtido via /api/v1/auth/login"
            ),
            "ApiKeyAuth": SecurityScheme(
                type="apiKey",
                name="X-API-Key",
                in_="header",
                description="API Key para autenticacao de servicos"
            )
        }

    def generate(self) -> Dict[str, Any]:
        """
        Gera especificacao OpenAPI completa.

        Returns:
            Dicionario com especificacao OpenAPI 3.0
        """
        # Obter spec base do FastAPI
        base_spec = get_openapi(
            title=self.info.title,
            version=self.info.version,
            description=self.info.description,
            routes=self.app.routes,
            terms_of_service=self.info.terms_of_service,
        )

        # Adicionar informacoes extras
        base_spec["info"]["contact"] = {
            "name": self.info.contact.name,
            "email": self.info.contact.email,
            "url": self.info.contact.url
        }

        base_spec["info"]["license"] = {
            "name": self.info.license_info.name,
            "url": self.info.license_info.url
        }

        # Adicionar servidores
        base_spec["servers"] = [
            {
                "url": s.url,
                "description": s.description,
                **({"variables": s.variables} if s.variables else {})
            }
            for s in self.servers
        ]

        # Adicionar tags
        base_spec["tags"] = [
            {
                "name": t.name,
                "description": t.description,
                **({"externalDocs": t.external_docs} if t.external_docs else {})
            }
            for t in self.tags
        ]

        # Adicionar esquemas de seguranca
        if "components" not in base_spec:
            base_spec["components"] = {}

        base_spec["components"]["securitySchemes"] = {
            name: self._security_scheme_to_dict(scheme)
            for name, scheme in self.security_schemes.items()
        }

        # Adicionar respostas comuns
        base_spec["components"]["responses"] = self._common_responses()

        # Adicionar exemplos
        base_spec["components"]["examples"] = self._common_examples()

        # Enriquecer paths com metadados
        self._enrich_paths(base_spec)

        return base_spec

    def _security_scheme_to_dict(self, scheme: SecurityScheme) -> Dict[str, Any]:
        """Converte SecurityScheme para dicionario"""
        result = {"type": scheme.type}

        if scheme.scheme:
            result["scheme"] = scheme.scheme
        if scheme.bearer_format:
            result["bearerFormat"] = scheme.bearer_format
        if scheme.description:
            result["description"] = scheme.description
        if scheme.name:
            result["name"] = scheme.name
        if scheme.in_:
            result["in"] = scheme.in_

        return result

    def _common_responses(self) -> Dict[str, Any]:
        """Respostas HTTP comuns"""
        return {
            "BadRequest": {
                "description": "Requisicao invalida",
                "content": {
                    "application/json": {
                        "schema": {
                            "type": "object",
                            "properties": {
                                "error_code": {"type": "string"},
                                "message": {"type": "string"},
                                "success": {"type": "boolean", "default": False}
                            }
                        },
                        "example": {
                            "error_code": "VALIDATION_ERROR",
                            "message": "Campo 'name' e obrigatorio",
                            "success": False
                        }
                    }
                }
            },
            "Unauthorized": {
                "description": "Nao autorizado - Token invalido ou ausente",
                "content": {
                    "application/json": {
                        "schema": {
                            "type": "object",
                            "properties": {
                                "error_code": {"type": "string"},
                                "message": {"type": "string"},
                                "success": {"type": "boolean", "default": False}
                            }
                        },
                        "example": {
                            "error_code": "UNAUTHORIZED",
                            "message": "Token de autenticacao invalido ou expirado",
                            "success": False
                        }
                    }
                }
            },
            "Forbidden": {
                "description": "Acesso negado - Permissoes insuficientes",
                "content": {
                    "application/json": {
                        "example": {
                            "error_code": "FORBIDDEN",
                            "message": "Voce nao tem permissao para acessar este recurso",
                            "success": False
                        }
                    }
                }
            },
            "NotFound": {
                "description": "Recurso nao encontrado",
                "content": {
                    "application/json": {
                        "example": {
                            "error_code": "NOT_FOUND",
                            "message": "Recurso nao encontrado",
                            "success": False
                        }
                    }
                }
            },
            "Conflict": {
                "description": "Conflito - Recurso ja existe",
                "content": {
                    "application/json": {
                        "example": {
                            "error_code": "CONFLICT",
                            "message": "Recurso com este identificador ja existe",
                            "success": False
                        }
                    }
                }
            },
            "TooManyRequests": {
                "description": "Rate limit excedido",
                "headers": {
                    "Retry-After": {
                        "description": "Segundos para aguardar antes de tentar novamente",
                        "schema": {"type": "integer"}
                    },
                    "X-RateLimit-Limit": {
                        "description": "Limite de requisicoes",
                        "schema": {"type": "integer"}
                    },
                    "X-RateLimit-Remaining": {
                        "description": "Requisicoes restantes",
                        "schema": {"type": "integer"}
                    }
                },
                "content": {
                    "application/json": {
                        "example": {
                            "error_code": "RATE_LIMITED",
                            "message": "Muitas requisicoes. Aguarde antes de tentar novamente.",
                            "success": False,
                            "retry_after": 60
                        }
                    }
                }
            },
            "InternalError": {
                "description": "Erro interno do servidor",
                "content": {
                    "application/json": {
                        "example": {
                            "error_code": "INTERNAL_ERROR",
                            "message": "Ocorreu um erro interno. Nossa equipe foi notificada.",
                            "success": False,
                            "correlation_id": "550e8400-e29b-41d4-a716-446655440000"
                        }
                    }
                }
            }
        }

    def _common_examples(self) -> Dict[str, Any]:
        """Exemplos comuns para reutilizacao"""
        return {
            "ProjectExample": {
                "summary": "Exemplo de projeto",
                "value": {
                    "project_id": "proj-001",
                    "name": "Meu Projeto",
                    "description": "Descricao do projeto",
                    "tech_stack": "python,fastapi,postgresql",
                    "status": "active",
                    "created_at": "2025-01-15T10:30:00Z"
                }
            },
            "StoryExample": {
                "summary": "Exemplo de User Story",
                "value": {
                    "story_id": "STR-0001",
                    "title": "Implementar autenticacao",
                    "persona": "usuario",
                    "action": "fazer login com email e senha",
                    "benefit": "acessar recursos protegidos",
                    "acceptance_criteria": [
                        "Login com email valido",
                        "Senha com minimo 8 caracteres",
                        "Token JWT retornado"
                    ],
                    "story_points": 5,
                    "status": "in_progress",
                    "priority": "high"
                }
            },
            "JobExample": {
                "summary": "Exemplo de Job",
                "value": {
                    "job_id": "job-abc123",
                    "description": "Criar API REST para usuarios",
                    "tech_stack": "python,fastapi",
                    "features": ["crud", "auth", "pagination"],
                    "status": "running",
                    "progress": 65.5,
                    "current_step": "Gerando testes"
                }
            },
            "PaginatedResponse": {
                "summary": "Exemplo de resposta paginada",
                "value": {
                    "items": [],
                    "next_cursor": "eyJsYXN0X2lkIjogIjEwMCJ9",
                    "previous_cursor": None,
                    "has_more": True,
                    "has_previous": False,
                    "total_count": 150
                }
            }
        }

    def _enrich_paths(self, spec: Dict[str, Any]) -> None:
        """Enriquece paths com metadados adicionais"""
        if "paths" not in spec:
            return

        for path, methods in spec["paths"].items():
            for method, operation in methods.items():
                if method in ("get", "post", "put", "patch", "delete"):
                    # Adicionar seguranca aos endpoints protegidos
                    if not path.startswith("/api/v1/health") and not path.endswith("/login"):
                        if "security" not in operation:
                            operation["security"] = [{"BearerAuth": []}]

                    # Adicionar respostas comuns
                    if "responses" not in operation:
                        operation["responses"] = {}

                    # Adicionar respostas de erro padrao
                    if "400" not in operation["responses"]:
                        operation["responses"]["400"] = {"$ref": "#/components/responses/BadRequest"}
                    if "401" not in operation["responses"]:
                        operation["responses"]["401"] = {"$ref": "#/components/responses/Unauthorized"}
                    if "500" not in operation["responses"]:
                        operation["responses"]["500"] = {"$ref": "#/components/responses/InternalError"}

    def export_json(self, filepath: str) -> None:
        """
        Exporta especificacao para arquivo JSON.

        Args:
            filepath: Caminho do arquivo
        """
        spec = self.generate()
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(spec, f, indent=2, ensure_ascii=False)

    def export_yaml(self, filepath: str) -> None:
        """
        Exporta especificacao para arquivo YAML.

        Args:
            filepath: Caminho do arquivo
        """
        spec = self.generate()
        with open(filepath, "w", encoding="utf-8") as f:
            yaml.dump(spec, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

    def get_spec_as_yaml(self) -> str:
        """Retorna spec como string YAML"""
        spec = self.generate()
        return yaml.dump(spec, default_flow_style=False, allow_unicode=True, sort_keys=False)

    def get_spec_as_json(self) -> str:
        """Retorna spec como string JSON"""
        spec = self.generate()
        return json.dumps(spec, indent=2, ensure_ascii=False)


# =============================================================================
# Request Validation
# =============================================================================

class RequestValidationError(Exception):
    """Erro de validacao de request"""

    def __init__(self, errors: List[Dict[str, Any]]):
        self.errors = errors
        super().__init__(str(errors))


class SchemaValidator:
    """Validador de schema JSON"""

    def __init__(self, spec: Dict[str, Any]):
        """
        Inicializa validador.

        Args:
            spec: Especificacao OpenAPI
        """
        self.spec = spec
        self._schemas = spec.get("components", {}).get("schemas", {})

    def validate_request(
        self,
        path: str,
        method: str,
        body: Dict[str, Any] = None,
        query_params: Dict[str, Any] = None,
        path_params: Dict[str, Any] = None
    ) -> List[Dict[str, Any]]:
        """
        Valida request contra schema.

        Args:
            path: Path da rota
            method: Metodo HTTP
            body: Body do request
            query_params: Query parameters
            path_params: Path parameters

        Returns:
            Lista de erros (vazia se valido)
        """
        errors = []

        # Encontrar operacao
        path_spec = self._find_path(path)
        if not path_spec:
            return errors  # Path nao encontrado, nao validar

        operation = path_spec.get(method.lower())
        if not operation:
            return errors  # Metodo nao encontrado

        # Validar parametros
        if "parameters" in operation:
            param_errors = self._validate_parameters(
                operation["parameters"],
                query_params or {},
                path_params or {}
            )
            errors.extend(param_errors)

        # Validar body
        if "requestBody" in operation and body is not None:
            body_errors = self._validate_body(operation["requestBody"], body)
            errors.extend(body_errors)

        return errors

    def _find_path(self, path: str) -> Optional[Dict[str, Any]]:
        """Encontra path na spec (suporta parametros)"""
        paths = self.spec.get("paths", {})

        # Tentar match exato
        if path in paths:
            return paths[path]

        # Tentar match com parametros
        for spec_path, spec in paths.items():
            pattern = re.sub(r"\{[^}]+\}", r"[^/]+", spec_path)
            if re.fullmatch(pattern, path):
                return spec

        return None

    def _validate_parameters(
        self,
        params_spec: List[Dict[str, Any]],
        query_params: Dict[str, Any],
        path_params: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """Valida parametros"""
        errors = []

        for param in params_spec:
            name = param.get("name")
            location = param.get("in")
            required = param.get("required", False)

            value = None
            if location == "query":
                value = query_params.get(name)
            elif location == "path":
                value = path_params.get(name)

            if required and value is None:
                errors.append({
                    "loc": [location, name],
                    "msg": f"Parametro obrigatorio '{name}' nao fornecido",
                    "type": "missing"
                })

        return errors

    def _validate_body(
        self,
        body_spec: Dict[str, Any],
        body: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """Valida body do request"""
        errors = []

        content = body_spec.get("content", {})
        json_content = content.get("application/json", {})
        schema = json_content.get("schema", {})

        # Resolver referencia
        if "$ref" in schema:
            schema = self._resolve_ref(schema["$ref"])

        # Validar campos obrigatorios
        required_fields = schema.get("required", [])
        for field in required_fields:
            if field not in body:
                errors.append({
                    "loc": ["body", field],
                    "msg": f"Campo obrigatorio '{field}' nao fornecido",
                    "type": "missing"
                })

        return errors

    def _resolve_ref(self, ref: str) -> Dict[str, Any]:
        """Resolve referencia $ref"""
        # Remove "#/components/schemas/"
        schema_name = ref.split("/")[-1]
        return self._schemas.get(schema_name, {})


def validate_request(func: Callable) -> Callable:
    """
    Decorator para validar request contra schema OpenAPI.

    Uso:
        @router.post("/items")
        @validate_request
        async def create_item(item: ItemCreate, request: Request):
            ...
    """
    @wraps(func)
    async def wrapper(*args, **kwargs):
        # Extrair request
        request = None
        for arg in args:
            if isinstance(arg, Request):
                request = arg
                break
        if not request:
            request = kwargs.get("request")

        if request:
            # Obter app e spec
            app = request.app
            if hasattr(app, "_openapi_spec"):
                validator = SchemaValidator(app._openapi_spec)

                # Validar
                errors = validator.validate_request(
                    path=request.url.path,
                    method=request.method,
                    body=await request.json() if request.method in ("POST", "PUT", "PATCH") else None,
                    query_params=dict(request.query_params),
                    path_params=request.path_params
                )

                if errors:
                    raise HTTPException(
                        status_code=422,
                        detail=errors
                    )

        return await func(*args, **kwargs)

    return wrapper


# =============================================================================
# OpenAPI Middleware
# =============================================================================

class OpenAPIValidationMiddleware(BaseHTTPMiddleware):
    """
    Middleware para validacao automatica de requests contra OpenAPI spec.
    """

    def __init__(self, app, spec: Dict[str, Any], validate_responses: bool = False):
        """
        Inicializa middleware.

        Args:
            app: Aplicacao FastAPI
            spec: Especificacao OpenAPI
            validate_responses: Se deve validar responses tambem
        """
        super().__init__(app)
        self.validator = SchemaValidator(spec)
        self.validate_responses = validate_responses

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Processa request com validacao"""

        # Validar request para metodos que tem body
        if request.method in ("POST", "PUT", "PATCH"):
            try:
                body = await request.json()
            except Exception:
                body = None

            errors = self.validator.validate_request(
                path=request.url.path,
                method=request.method,
                body=body,
                query_params=dict(request.query_params),
                path_params=request.path_params
            )

            if errors:
                return Response(
                    content=json.dumps({
                        "error_code": "VALIDATION_ERROR",
                        "message": "Request nao passou na validacao",
                        "success": False,
                        "errors": errors
                    }),
                    status_code=422,
                    media_type="application/json"
                )

            # Recriar body para proximo handler
            request._body = json.dumps(body).encode() if body else b""

        response = await call_next(request)
        return response


# =============================================================================
# Convenience Functions
# =============================================================================

def create_openapi_generator(
    app: FastAPI,
    title: str = None,
    version: str = None,
    description: str = None
) -> OpenAPIGenerator:
    """
    Cria gerador OpenAPI com configuracao simplificada.

    Args:
        app: Aplicacao FastAPI
        title: Titulo da API
        version: Versao da API
        description: Descricao

    Returns:
        OpenAPIGenerator configurado
    """
    info = APIInfo()
    if title:
        info.title = title
    if version:
        info.version = version
    if description:
        info.description = description

    return OpenAPIGenerator(app, info=info)


def setup_openapi(app: FastAPI) -> None:
    """
    Configura OpenAPI para a aplicacao.

    Adiciona:
    - Geracao automatica de spec
    - Endpoints para exportar spec
    - Armazenamento da spec para validacao

    Args:
        app: Aplicacao FastAPI
    """
    generator = OpenAPIGenerator(app)
    spec = generator.generate()

    # Armazenar spec para uso em validacao
    app._openapi_spec = spec

    # Sobrescrever funcao de OpenAPI do FastAPI
    def custom_openapi():
        return spec

    app.openapi = custom_openapi


def get_openapi_routes(generator: OpenAPIGenerator) -> List[APIRoute]:
    """
    Cria rotas para servir OpenAPI spec.

    Args:
        generator: Gerador OpenAPI

    Returns:
        Lista de rotas
    """
    from fastapi import APIRouter
    from fastapi.responses import JSONResponse, PlainTextResponse

    router = APIRouter(tags=["OpenAPI"])

    @router.get("/openapi.json")
    async def get_openapi_json():
        """Retorna especificacao OpenAPI em JSON"""
        return JSONResponse(content=generator.generate())

    @router.get("/openapi.yaml")
    async def get_openapi_yaml():
        """Retorna especificacao OpenAPI em YAML"""
        return PlainTextResponse(
            content=generator.get_spec_as_yaml(),
            media_type="text/yaml"
        )

    @router.get("/api/versions")
    async def list_api_versions():
        """Lista versoes disponiveis da API"""
        return {
            "versions": [
                {
                    "version": "1.0.0",
                    "status": "active",
                    "base_url": "/api/v1"
                },
                {
                    "version": "2.0.0",
                    "status": "active",
                    "base_url": "/api/v2"
                }
            ],
            "current": "2.0.0",
            "deprecated": []
        }

    return router.routes
