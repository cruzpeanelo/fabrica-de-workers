"""
API Versioning Middleware - Issue #90
=====================================

Implementacao de versionamento semantico e politica de deprecacao para a API.

Suporta multiplas versoes da API simultaneamente com:
- Roteamento baseado em URL prefix (/api/v1, /api/v2)
- Header Accept-Version para override
- Deprecation warnings em headers de resposta
- Sunset header para datas de descontinuacao

Uso:
    from factory.api.versioning import VersioningMiddleware, api_version

    app.add_middleware(VersioningMiddleware)

    @router.get("/items")
    @api_version("1.0", deprecated=True, sunset="2025-06-01")
    async def list_items_v1():
        ...
"""

import re
from datetime import datetime, date
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, List, Optional, Tuple, Set

from fastapi import Request, Response, HTTPException, status
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel


class APIVersion(BaseModel):
    """Representacao de uma versao da API"""
    major: int
    minor: int = 0
    patch: int = 0

    @classmethod
    def parse(cls, version_str: str) -> "APIVersion":
        """
        Parse string de versao para APIVersion.

        Suporta formatos:
        - "1" -> 1.0.0
        - "1.0" -> 1.0.0
        - "1.0.0" -> 1.0.0
        - "v1" -> 1.0.0
        - "v1.0" -> 1.0.0
        """
        # Remove prefixo 'v' se presente
        version_str = version_str.lower().lstrip('v')

        parts = version_str.split('.')
        try:
            major = int(parts[0]) if len(parts) > 0 else 1
            minor = int(parts[1]) if len(parts) > 1 else 0
            patch = int(parts[2]) if len(parts) > 2 else 0
            return cls(major=major, minor=minor, patch=patch)
        except (ValueError, IndexError):
            return cls(major=1, minor=0, patch=0)

    def __str__(self) -> str:
        return f"{self.major}.{self.minor}.{self.patch}"

    def __eq__(self, other) -> bool:
        if isinstance(other, str):
            other = APIVersion.parse(other)
        return (self.major, self.minor, self.patch) == (other.major, other.minor, other.patch)

    def __lt__(self, other) -> bool:
        if isinstance(other, str):
            other = APIVersion.parse(other)
        return (self.major, self.minor, self.patch) < (other.major, other.minor, other.patch)

    def __le__(self, other) -> bool:
        return self == other or self < other

    def __gt__(self, other) -> bool:
        return not self <= other

    def __ge__(self, other) -> bool:
        return not self < other

    def __hash__(self) -> int:
        return hash((self.major, self.minor, self.patch))


class DeprecationStatus(str, Enum):
    """Status de deprecacao de uma versao ou endpoint"""
    ACTIVE = "active"              # Versao atual e recomendada
    DEPRECATED = "deprecated"      # Ainda funciona, mas sera removida
    SUNSET = "sunset"              # Data de remocao definida
    REMOVED = "removed"            # Versao removida


class VersionInfo(BaseModel):
    """Informacoes de uma versao da API"""
    version: str
    status: DeprecationStatus = DeprecationStatus.ACTIVE
    deprecated_at: Optional[date] = None
    sunset_date: Optional[date] = None
    replacement_version: Optional[str] = None
    changelog_url: Optional[str] = None
    documentation_url: Optional[str] = None


class DeprecationPolicy:
    """
    Politica de deprecacao de versoes da API.

    Define:
    - Quais versoes estao ativas
    - Quais estao deprecadas
    - Datas de sunset
    - Versoes de substituicao
    """

    # Periodo minimo entre deprecacao e sunset (dias)
    MINIMUM_DEPRECATION_PERIOD = 90

    # Numero maximo de versoes major suportadas simultaneamente
    MAX_SUPPORTED_VERSIONS = 3

    def __init__(self):
        self._versions: Dict[str, VersionInfo] = {}
        self._deprecated_endpoints: Dict[str, Dict[str, Any]] = {}

        # Registrar versoes padrao
        self._register_default_versions()

    def _register_default_versions(self):
        """Registra versoes padrao da API"""
        self.register_version(VersionInfo(
            version="1.0.0",
            status=DeprecationStatus.ACTIVE,
            documentation_url="/docs/v1"
        ))
        self.register_version(VersionInfo(
            version="2.0.0",
            status=DeprecationStatus.ACTIVE,
            documentation_url="/docs/v2",
            changelog_url="/changelog#v2.0.0"
        ))

    def register_version(self, info: VersionInfo) -> None:
        """Registra uma versao da API"""
        self._versions[info.version] = info

    def get_version_info(self, version: str) -> Optional[VersionInfo]:
        """Retorna informacoes de uma versao"""
        # Normalizar versao
        parsed = APIVersion.parse(version)

        # Buscar versao exata
        if str(parsed) in self._versions:
            return self._versions[str(parsed)]

        # Buscar versao major.minor
        short = f"{parsed.major}.{parsed.minor}.0"
        if short in self._versions:
            return self._versions[short]

        # Buscar versao major
        major = f"{parsed.major}.0.0"
        if major in self._versions:
            return self._versions[major]

        return None

    def is_deprecated(self, version: str) -> bool:
        """Verifica se versao esta deprecada"""
        info = self.get_version_info(version)
        if not info:
            return False
        return info.status in (DeprecationStatus.DEPRECATED, DeprecationStatus.SUNSET)

    def is_sunset(self, version: str) -> bool:
        """Verifica se versao passou da data de sunset"""
        info = self.get_version_info(version)
        if not info or not info.sunset_date:
            return False
        return date.today() >= info.sunset_date

    def get_active_versions(self) -> List[VersionInfo]:
        """Retorna versoes ativas"""
        return [v for v in self._versions.values()
                if v.status == DeprecationStatus.ACTIVE]

    def get_latest_version(self) -> Optional[VersionInfo]:
        """Retorna versao mais recente"""
        active = self.get_active_versions()
        if not active:
            return None

        sorted_versions = sorted(
            active,
            key=lambda v: APIVersion.parse(v.version),
            reverse=True
        )
        return sorted_versions[0] if sorted_versions else None

    def deprecate_version(
        self,
        version: str,
        sunset_date: date,
        replacement_version: str = None
    ) -> None:
        """
        Depreca uma versao.

        Args:
            version: Versao a deprecar
            sunset_date: Data de sunset
            replacement_version: Versao de substituicao
        """
        info = self.get_version_info(version)
        if not info:
            info = VersionInfo(version=version)
            self._versions[version] = info

        info.status = DeprecationStatus.DEPRECATED
        info.deprecated_at = date.today()
        info.sunset_date = sunset_date
        info.replacement_version = replacement_version

    def deprecate_endpoint(
        self,
        path: str,
        method: str,
        version: str,
        sunset_date: date = None,
        replacement_endpoint: str = None,
        message: str = None
    ) -> None:
        """
        Depreca um endpoint especifico.

        Args:
            path: Path do endpoint
            method: Metodo HTTP
            version: Versao da API
            sunset_date: Data de sunset
            replacement_endpoint: Endpoint de substituicao
            message: Mensagem customizada
        """
        key = f"{method}:{path}:{version}"
        self._deprecated_endpoints[key] = {
            "path": path,
            "method": method,
            "version": version,
            "sunset_date": sunset_date,
            "replacement_endpoint": replacement_endpoint,
            "message": message,
            "deprecated_at": date.today()
        }

    def get_deprecated_endpoint_info(
        self,
        path: str,
        method: str,
        version: str
    ) -> Optional[Dict[str, Any]]:
        """Retorna info de deprecacao de endpoint"""
        key = f"{method}:{path}:{version}"
        return self._deprecated_endpoints.get(key)


# Instancia global da politica
deprecation_policy = DeprecationPolicy()


class VersioningMiddleware(BaseHTTPMiddleware):
    """
    Middleware de versionamento da API.

    Funcionalidades:
    - Extrai versao da URL ou header
    - Adiciona headers de deprecacao
    - Bloqueia versoes em sunset
    - Redireciona para versao mais recente (opcional)
    """

    # Padrao para extrair versao da URL
    VERSION_PATTERN = re.compile(r'/api/v(\d+(?:\.\d+)?(?:\.\d+)?)')

    # Header para override de versao
    VERSION_HEADER = "Accept-Version"

    # Header alternativo (X- prefix)
    ALT_VERSION_HEADER = "X-API-Version"

    def __init__(
        self,
        app,
        policy: DeprecationPolicy = None,
        default_version: str = "1.0.0",
        strict_mode: bool = False,
        block_sunset: bool = True
    ):
        """
        Inicializa middleware.

        Args:
            app: Aplicacao FastAPI
            policy: Politica de deprecacao
            default_version: Versao padrao
            strict_mode: Se True, rejeita versoes desconhecidas
            block_sunset: Se True, bloqueia versoes em sunset
        """
        super().__init__(app)
        self.policy = policy or deprecation_policy
        self.default_version = APIVersion.parse(default_version)
        self.strict_mode = strict_mode
        self.block_sunset = block_sunset

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Processa request com versionamento"""

        # Extrair versao
        version = self._extract_version(request)

        # Armazenar versao no state
        request.state.api_version = version

        # Verificar se versao existe
        version_info = self.policy.get_version_info(str(version))

        if self.strict_mode and not version_info:
            return JSONResponse(
                status_code=status.HTTP_400_BAD_REQUEST,
                content={
                    "error_code": "UNKNOWN_API_VERSION",
                    "message": f"Versao de API '{version}' nao reconhecida",
                    "available_versions": [v.version for v in self.policy.get_active_versions()],
                    "success": False
                }
            )

        # Verificar sunset
        if self.block_sunset and self.policy.is_sunset(str(version)):
            latest = self.policy.get_latest_version()
            return JSONResponse(
                status_code=status.HTTP_410_GONE,
                content={
                    "error_code": "API_VERSION_SUNSET",
                    "message": f"Versao {version} foi descontinuada",
                    "recommended_version": latest.version if latest else None,
                    "migration_guide": version_info.changelog_url if version_info else None,
                    "success": False
                }
            )

        # Processar request
        response = await call_next(request)

        # Adicionar headers de versionamento
        response.headers["X-API-Version"] = str(version)

        # Adicionar headers de deprecacao se aplicavel
        if version_info and self.policy.is_deprecated(str(version)):
            self._add_deprecation_headers(response, version_info)

        # Verificar deprecacao de endpoint especifico
        endpoint_deprecation = self.policy.get_deprecated_endpoint_info(
            request.url.path,
            request.method,
            str(version)
        )
        if endpoint_deprecation:
            self._add_endpoint_deprecation_headers(response, endpoint_deprecation)

        return response

    def _extract_version(self, request: Request) -> APIVersion:
        """
        Extrai versao da requisicao.

        Prioridade:
        1. Header Accept-Version
        2. Header X-API-Version
        3. URL path (/api/v1/...)
        4. Versao padrao
        """
        # Verificar headers
        version_header = (
            request.headers.get(self.VERSION_HEADER) or
            request.headers.get(self.ALT_VERSION_HEADER)
        )

        if version_header:
            return APIVersion.parse(version_header)

        # Extrair da URL
        match = self.VERSION_PATTERN.search(request.url.path)
        if match:
            return APIVersion.parse(match.group(1))

        return self.default_version

    def _add_deprecation_headers(
        self,
        response: Response,
        info: VersionInfo
    ) -> None:
        """Adiciona headers de deprecacao de versao"""

        # Deprecation header (RFC 8594)
        if info.deprecated_at:
            response.headers["Deprecation"] = info.deprecated_at.isoformat()
        else:
            response.headers["Deprecation"] = "true"

        # Sunset header (RFC 8594)
        if info.sunset_date:
            # Formato: Sun, 01 Jun 2025 00:00:00 GMT
            sunset_dt = datetime.combine(info.sunset_date, datetime.min.time())
            response.headers["Sunset"] = sunset_dt.strftime("%a, %d %b %Y %H:%M:%S GMT")

        # Link para nova versao
        if info.replacement_version:
            response.headers["Link"] = f'</api/v{info.replacement_version.split(".")[0]}>; rel="successor-version"'

        # Warning header (deprecation notice)
        warning_msg = f"API version {info.version} is deprecated"
        if info.sunset_date:
            warning_msg += f" and will be removed on {info.sunset_date}"
        if info.replacement_version:
            warning_msg += f". Please migrate to version {info.replacement_version}"

        response.headers["Warning"] = f'299 - "{warning_msg}"'

        # Header customizado para facilitar parsing
        response.headers["X-Deprecation-Notice"] = warning_msg

    def _add_endpoint_deprecation_headers(
        self,
        response: Response,
        info: Dict[str, Any]
    ) -> None:
        """Adiciona headers de deprecacao de endpoint"""

        response.headers["X-Endpoint-Deprecated"] = "true"

        if info.get("sunset_date"):
            sunset_dt = datetime.combine(info["sunset_date"], datetime.min.time())
            response.headers["X-Endpoint-Sunset"] = sunset_dt.strftime("%a, %d %b %Y %H:%M:%S GMT")

        if info.get("replacement_endpoint"):
            response.headers["X-Endpoint-Replacement"] = info["replacement_endpoint"]

        if info.get("message"):
            response.headers["X-Deprecation-Message"] = info["message"]


def api_version(
    version: str,
    deprecated: bool = False,
    sunset: str = None,
    replacement: str = None,
    message: str = None
):
    """
    Decorator para marcar versao de endpoint.

    Uso:
        @router.get("/items")
        @api_version("1.0", deprecated=True, sunset="2025-06-01")
        async def list_items():
            ...

    Args:
        version: Versao do endpoint
        deprecated: Se endpoint esta deprecado
        sunset: Data de sunset (YYYY-MM-DD)
        replacement: Endpoint de substituicao
        message: Mensagem de deprecacao
    """
    def decorator(func: Callable) -> Callable:
        # Armazenar metadata no endpoint
        func._api_version = APIVersion.parse(version)
        func._deprecated = deprecated
        func._sunset_date = (
            datetime.strptime(sunset, "%Y-%m-%d").date()
            if sunset else None
        )
        func._replacement = replacement
        func._deprecation_message = message

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

            # Executar funcao
            result = await func(*args, **kwargs)

            return result

        return wrapper
    return decorator


class VersionedRouter:
    """
    Helper para criar routers versionados.

    Uso:
        v1 = VersionedRouter(prefix="/api/v1", version="1.0")
        v2 = VersionedRouter(prefix="/api/v2", version="2.0")

        @v1.get("/items")
        async def list_items_v1():
            ...

        @v2.get("/items")
        async def list_items_v2():
            ...
    """

    def __init__(
        self,
        prefix: str = "",
        version: str = "1.0.0",
        deprecated: bool = False,
        sunset: str = None,
        tags: List[str] = None
    ):
        from fastapi import APIRouter

        self.version = APIVersion.parse(version)
        self.deprecated = deprecated
        self.sunset_date = (
            datetime.strptime(sunset, "%Y-%m-%d").date()
            if sunset else None
        )
        self.router = APIRouter(
            prefix=prefix,
            tags=tags or [f"v{self.version.major}"]
        )

    def get(self, path: str, **kwargs):
        """Decorator para GET endpoint"""
        return self._versioned_route("GET", path, **kwargs)

    def post(self, path: str, **kwargs):
        """Decorator para POST endpoint"""
        return self._versioned_route("POST", path, **kwargs)

    def put(self, path: str, **kwargs):
        """Decorator para PUT endpoint"""
        return self._versioned_route("PUT", path, **kwargs)

    def patch(self, path: str, **kwargs):
        """Decorator para PATCH endpoint"""
        return self._versioned_route("PATCH", path, **kwargs)

    def delete(self, path: str, **kwargs):
        """Decorator para DELETE endpoint"""
        return self._versioned_route("DELETE", path, **kwargs)

    def _versioned_route(self, method: str, path: str, **kwargs):
        """Cria rota versionada"""
        def decorator(func: Callable) -> Callable:
            # Aplicar metadata de versao
            func._api_version = self.version
            func._deprecated = self.deprecated
            func._sunset_date = self.sunset_date

            # Registrar rota
            route_decorator = getattr(self.router, method.lower())
            return route_decorator(path, **kwargs)(func)

        return decorator


# Funcoes utilitarias
def get_api_versions() -> List[VersionInfo]:
    """Retorna todas as versoes registradas"""
    return list(deprecation_policy._versions.values())


def get_supported_versions() -> List[str]:
    """Retorna lista de versoes suportadas (nao em sunset)"""
    return [
        v.version for v in deprecation_policy._versions.values()
        if v.status != DeprecationStatus.REMOVED and not deprecation_policy.is_sunset(v.version)
    ]


def register_api_version(
    version: str,
    status: DeprecationStatus = DeprecationStatus.ACTIVE,
    sunset_date: date = None,
    replacement: str = None,
    docs_url: str = None,
    changelog_url: str = None
) -> None:
    """
    Registra uma nova versao da API.

    Args:
        version: String da versao (ex: "2.0.0")
        status: Status de deprecacao
        sunset_date: Data de sunset (opcional)
        replacement: Versao de substituicao (opcional)
        docs_url: URL da documentacao
        changelog_url: URL do changelog
    """
    info = VersionInfo(
        version=version,
        status=status,
        sunset_date=sunset_date,
        replacement_version=replacement,
        documentation_url=docs_url,
        changelog_url=changelog_url
    )
    deprecation_policy.register_version(info)


# Documentacao da Politica de Deprecacao
DEPRECATION_POLICY_DOC = """
# Politica de Deprecacao da API - Plataforma E

## Versionamento Semantico

A API segue o padrao de Versionamento Semantico (SemVer):

- **MAJOR** (X.0.0): Mudancas incompativeis com versoes anteriores
- **MINOR** (0.X.0): Novas funcionalidades mantendo compatibilidade
- **PATCH** (0.0.X): Correcoes de bugs mantendo compatibilidade

## Ciclo de Vida de Versoes

1. **Active**: Versao atual e recomendada
2. **Deprecated**: Ainda funciona, mas sera removida (aviso em headers)
3. **Sunset**: Data de remocao definida e comunicada
4. **Removed**: Versao removida, retorna erro 410 Gone

## Periodos Minimos

- **Deprecacao -> Sunset**: Minimo 90 dias
- **Sunset -> Remocao**: Minimo 30 dias
- **Versoes Major Suportadas**: 3 simultaneamente

## Headers de Resposta

Quando uma versao ou endpoint esta deprecado, os seguintes headers sao incluidos:

- `Deprecation`: Data de deprecacao ou "true"
- `Sunset`: Data de remocao no formato RFC 7231
- `Link`: Link para versao substituta
- `Warning`: Mensagem de aviso
- `X-Deprecation-Notice`: Mensagem legivel

## Comunicacao

- Changelog publicado em /changelog
- Notificacao via email para API keys ativas
- Anuncio no dashboard com 90 dias de antecedencia

## Endpoints de Versao

- `GET /api/versions`: Lista todas as versoes
- `GET /api/versions/current`: Versao atual recomendada
- `GET /api/versions/{version}`: Detalhes de uma versao
"""
