"""
Configuracoes da Fabrica de Agentes v4.0
Nova arquitetura com Workers Claude
"""
import os
from pathlib import Path
from enum import Enum
from dotenv import load_dotenv

# Carregar variaveis de ambiente
load_dotenv()

# =============================================================================
# PATHS
# =============================================================================

# Diretorio raiz do projeto
PROJECT_ROOT = Path(__file__).resolve().parent.parent

# Diretorios principais
FACTORY_DIR = PROJECT_ROOT / "factory"
PROJECTS_DIR = PROJECT_ROOT / "projects"
TEMPLATES_DIR = PROJECT_ROOT / "templates"

# Subdiretorios da fabrica
DATABASE_DIR = FACTORY_DIR / "database"
DASHBOARD_DIR = FACTORY_DIR / "dashboard"
CORE_DIR = FACTORY_DIR / "core"
API_DIR = FACTORY_DIR / "api"
SCRIPTS_DIR = FACTORY_DIR / "scripts"

# Arquivos importantes
FACTORY_DB = DATABASE_DIR / "factory.db"
# JWT_SECRET_FILE removido - Issue #192: usar apenas variáveis de ambiente

# =============================================================================
# DATABASE
# =============================================================================

# PostgreSQL (producao) ou SQLite (fallback)
DATABASE_URL = os.getenv(
    "DATABASE_URL",
    f"sqlite:///{FACTORY_DB}"
)

# Redis
REDIS_URL = os.getenv("REDIS_URL", "redis://localhost:6379")

# Connection pool
DB_POOL_SIZE = int(os.getenv("DB_POOL_SIZE", 5))
DB_MAX_OVERFLOW = int(os.getenv("DB_MAX_OVERFLOW", 10))

# =============================================================================
# WORKERS
# =============================================================================

# Numero de workers Claude
MIN_WORKERS = int(os.getenv("MIN_WORKERS", 2))
MAX_WORKERS = int(os.getenv("MAX_WORKERS", 5))
DEFAULT_WORKERS = int(os.getenv("DEFAULT_WORKERS", 2))

# Timeouts
WORKER_TIMEOUT = int(os.getenv("WORKER_TIMEOUT", 600))  # 10 minutos
WORKER_HEARTBEAT_INTERVAL = int(os.getenv("WORKER_HEARTBEAT_INTERVAL", 30))
WORKER_IDLE_TIMEOUT = int(os.getenv("WORKER_IDLE_TIMEOUT", 300))

# Job settings
JOB_MAX_RETRIES = int(os.getenv("JOB_MAX_RETRIES", 3))
JOB_RETRY_DELAY = int(os.getenv("JOB_RETRY_DELAY", 30))

# =============================================================================
# LLM PROVIDER SETTINGS (Multi-provider support)
# =============================================================================

# Provedor padrao (claude, azure_openai, aws_bedrock, google_vertex)
LLM_PROVIDER = os.getenv("LLM_PROVIDER", "claude")

# Claude (Anthropic)
ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY", "")
CLAUDE_MODEL = os.getenv("CLAUDE_MODEL", "claude-sonnet-4-20250514")
CLAUDE_MAX_TOKENS = int(os.getenv("CLAUDE_MAX_TOKENS", 4096))
CLAUDE_CONTEXT_LIMIT = int(os.getenv("CLAUDE_CONTEXT_LIMIT", 200000))

# Azure OpenAI
AZURE_OPENAI_KEY = os.getenv("AZURE_OPENAI_KEY", "")
AZURE_OPENAI_ENDPOINT = os.getenv("AZURE_OPENAI_ENDPOINT", "")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT", "gpt-4")
AZURE_OPENAI_API_VERSION = os.getenv("AZURE_OPENAI_API_VERSION", "2024-02-15-preview")

# AWS Bedrock
AWS_BEDROCK_REGION = os.getenv("AWS_BEDROCK_REGION", "us-east-1")
AWS_BEDROCK_MODEL = os.getenv("AWS_BEDROCK_MODEL", "anthropic.claude-3-sonnet-20240229-v1:0")

# Google Vertex AI
GOOGLE_VERTEX_PROJECT = os.getenv("GOOGLE_VERTEX_PROJECT", "")
GOOGLE_VERTEX_LOCATION = os.getenv("GOOGLE_VERTEX_LOCATION", "us-central1")
GOOGLE_VERTEX_MODEL = os.getenv("GOOGLE_VERTEX_MODEL", "gemini-1.0-pro")

# Autonomous loop settings
AUTONOMOUS_MAX_ITERATIONS = int(os.getenv("AUTONOMOUS_MAX_ITERATIONS", 5))
AUTONOMOUS_FIX_ATTEMPTS = int(os.getenv("AUTONOMOUS_FIX_ATTEMPTS", 3))

# Claude Model Registry
CLAUDE_MODELS = {
    "opus": {
        "id": "claude-opus-4-5-20251101",
        "name": "Claude Opus 4.5",
        "description": "Modelo mais avancado para tarefas complexas",
        "cost_tier": "high",
        "recommended_for": ["complex_code", "architecture", "research"]
    },
    "sonnet": {
        "id": "claude-sonnet-4-20250514",
        "name": "Claude Sonnet 4",
        "description": "Equilibrio entre performance e custo",
        "cost_tier": "medium",
        "recommended_for": ["general", "development", "analysis"]
    },
    "haiku": {
        "id": "claude-3-5-haiku-20241022",
        "name": "Claude Haiku 3.5",
        "description": "Rapido e economico",
        "cost_tier": "low",
        "recommended_for": ["simple_tasks", "quick_fixes", "documentation"]
    }
}


def get_available_claude_models() -> list:
    """Retorna lista de modelos Claude disponiveis"""
    return [{"key": k, **v} for k, v in CLAUDE_MODELS.items()]


def get_claude_model(model_key: str) -> str:
    """Retorna ID do modelo pelo key"""
    model_info = CLAUDE_MODELS.get(model_key.lower(), {})
    return model_info.get("id", CLAUDE_MODEL)


def get_model_for_complexity(complexity: str) -> str:
    """Retorna modelo apropriado para a complexidade da tarefa"""
    complexity_mapping = {
        "simple": "claude-3-5-haiku-20241022",
        "low": "claude-3-5-haiku-20241022",
        "medium": "claude-sonnet-4-20250514",
        "high": "claude-sonnet-4-20250514",
        "complex": "claude-sonnet-4-20250514",
        "very_high": "claude-opus-4-5-20251101",
        "very_complex": "claude-opus-4-5-20251101"
    }
    return complexity_mapping.get(complexity.lower(), CLAUDE_MODEL)

# =============================================================================
# MULTIPLOS MODELOS CLAUDE (Issue #26)
# =============================================================================

# Modelos disponiveis com suas configuracoes
CLAUDE_MODELS = {
    "opus": {
        "id": "claude-opus-4-5-20251101",
        "name": "Claude Opus 4.5",
        "description": "Modelo avancado para tarefas complexas e raciocinio profundo",
        "max_tokens": 8192,
        "cost_tier": "high",
        "recommended_for": ["architecture", "security", "complex_refactoring", "critical_bugs"]
    },
    "sonnet": {
        "id": "claude-sonnet-4-20250514",
        "name": "Claude Sonnet 4",
        "description": "Balanco entre qualidade e velocidade (padrao)",
        "max_tokens": 8192,
        "cost_tier": "medium",
        "recommended_for": ["development", "code_review", "testing", "bug_fixes"]
    },
    "haiku": {
        "id": "claude-haiku-3-5-20241022",
        "name": "Claude Haiku 3.5",
        "description": "Modelo rapido e economico para tarefas simples",
        "max_tokens": 4096,
        "cost_tier": "low",
        "recommended_for": ["formatting", "documentation", "simple_fixes", "translations"]
    }
}

# Modelo padrao por nivel de complexidade
CLAUDE_MODEL_DEFAULTS = {
    "simple": "haiku",      # Tarefas simples -> Haiku (economico)
    "standard": "sonnet",   # Desenvolvimento padrao -> Sonnet
    "complex": "opus",      # Tarefas complexas -> Opus
}

# Configuracoes de selecao automatica
CLAUDE_AUTO_MODEL_SELECTION = os.getenv("CLAUDE_AUTO_MODEL_SELECTION", "true").lower() == "true"
CLAUDE_PREFER_ECONOMY = os.getenv("CLAUDE_PREFER_ECONOMY", "false").lower() == "true"
CLAUDE_PREFER_QUALITY = os.getenv("CLAUDE_PREFER_QUALITY", "false").lower() == "true"
CLAUDE_ENABLE_FALLBACK = os.getenv("CLAUDE_ENABLE_FALLBACK", "true").lower() == "true"


def get_claude_model(model_key: str = None) -> str:
    """Retorna o ID do modelo Claude baseado na chave."""
    if not model_key:
        return CLAUDE_MODEL
    if model_key.startswith("claude-"):
        return model_key
    model_config = CLAUDE_MODELS.get(model_key.lower())
    if model_config:
        return model_config["id"]
    return CLAUDE_MODEL


def get_model_for_complexity(complexity: str) -> str:
    """Retorna o modelo recomendado para um nivel de complexidade."""
    model_key = CLAUDE_MODEL_DEFAULTS.get(complexity.lower(), "sonnet")
    return get_claude_model(model_key)


def get_available_claude_models() -> list:
    """Retorna lista de modelos disponiveis para UI."""
    return [
        {
            "key": key,
            "id": config["id"],
            "name": config["name"],
            "description": config["description"],
            "cost_tier": config["cost_tier"],
            "recommended_for": config["recommended_for"]
        }
        for key, config in CLAUDE_MODELS.items()
    ]

# =============================================================================
# AUTHENTICATION
# =============================================================================

# JWT
JWT_SECRET_KEY = os.getenv("JWT_SECRET_KEY", "")  # Gerado automaticamente se vazio
JWT_ALGORITHM = os.getenv("JWT_ALGORITHM", "HS256")
ACCESS_TOKEN_EXPIRE_MINUTES = int(os.getenv("ACCESS_TOKEN_EXPIRE_MINUTES", 1440))  # 24h

# Admin padrao (desenvolvimento)
DEFAULT_ADMIN_USER = os.getenv("DEFAULT_ADMIN_USER", "admin")
DEFAULT_ADMIN_PASS = os.getenv("DEFAULT_ADMIN_PASS", "admin123")

# =============================================================================
# SECURITY - Issue #138: Block default credentials in production
# =============================================================================

# Lista de credenciais bloqueadas em produção (username:password hashes ou patterns)
BLOCKED_CREDENTIALS = [
    # Credenciais default conhecidas
    ("admin", "admin"),
    ("admin", "admin123"),
    ("admin", "admin1234"),
    ("admin", "password"),
    ("admin", "password123"),
    ("administrator", "administrator"),
    ("root", "root"),
    ("root", "toor"),
    ("test", "test"),
    ("test", "test123"),
    ("user", "user"),
    ("user", "user123"),
    ("demo", "demo"),
]

# Padrões de senha fraca (regex)
WEAK_PASSWORD_PATTERNS = [
    r"^(.)\1+$",           # Caracteres repetidos (aaa, 111)
    r"^(12345|123456|1234567|12345678|123456789)$",  # Sequências numéricas
    r"^password\d*$",      # password, password1, password123
    r"^admin\d*$",         # admin, admin1, admin123
    r"^qwerty\d*$",        # qwerty, qwerty123
    r"^letmein\d*$",       # letmein
    r"^welcome\d*$",       # welcome
]

# Requisitos mínimos de senha em produção
PASSWORD_MIN_LENGTH = int(os.getenv("PASSWORD_MIN_LENGTH", 8))
PASSWORD_REQUIRE_UPPERCASE = os.getenv("PASSWORD_REQUIRE_UPPERCASE", "true").lower() == "true"
PASSWORD_REQUIRE_LOWERCASE = os.getenv("PASSWORD_REQUIRE_LOWERCASE", "true").lower() == "true"
PASSWORD_REQUIRE_DIGIT = os.getenv("PASSWORD_REQUIRE_DIGIT", "true").lower() == "true"
PASSWORD_REQUIRE_SPECIAL = os.getenv("PASSWORD_REQUIRE_SPECIAL", "false").lower() == "true"


def is_credential_blocked(username: str, password: str) -> bool:
    """
    Issue #138: Verifica se credencial está na lista de bloqueio.

    Em produção, bloqueia credenciais conhecidas como inseguras.
    Em desenvolvimento, apenas loga um warning.

    Returns:
        True se a credencial está bloqueada
    """
    if not is_production():
        return False

    username_lower = username.lower()
    for blocked_user, blocked_pass in BLOCKED_CREDENTIALS:
        if username_lower == blocked_user.lower() and password == blocked_pass:
            return True
    return False


def validate_password_strength(password: str, is_prod: bool = None) -> tuple[bool, str]:
    """
    Issue #138: Valida força da senha.

    Args:
        password: Senha a validar
        is_prod: Se None, usa is_production()

    Returns:
        tuple: (is_valid, error_message)
    """
    import re

    if is_prod is None:
        is_prod = is_production()

    # Em desenvolvimento, aceita qualquer senha
    if not is_prod:
        return True, ""

    # Verificar comprimento mínimo
    if len(password) < PASSWORD_MIN_LENGTH:
        return False, f"Senha deve ter no mínimo {PASSWORD_MIN_LENGTH} caracteres"

    # Verificar padrões fracos
    for pattern in WEAK_PASSWORD_PATTERNS:
        if re.match(pattern, password, re.IGNORECASE):
            return False, "Senha muito fraca: padrão comum detectado"

    # Verificar requisitos
    if PASSWORD_REQUIRE_UPPERCASE and not re.search(r"[A-Z]", password):
        return False, "Senha deve conter pelo menos uma letra maiúscula"

    if PASSWORD_REQUIRE_LOWERCASE and not re.search(r"[a-z]", password):
        return False, "Senha deve conter pelo menos uma letra minúscula"

    if PASSWORD_REQUIRE_DIGIT and not re.search(r"\d", password):
        return False, "Senha deve conter pelo menos um número"

    if PASSWORD_REQUIRE_SPECIAL and not re.search(r"[!@#$%^&*(),.?\":{}|<>]", password):
        return False, "Senha deve conter pelo menos um caractere especial"

    return True, ""

# =============================================================================
# STRIPE / BILLING
# =============================================================================

# Stripe API Keys
STRIPE_API_KEY = os.getenv("STRIPE_API_KEY", "")
STRIPE_WEBHOOK_SECRET = os.getenv("STRIPE_WEBHOOK_SECRET", "")
STRIPE_PUBLISHABLE_KEY = os.getenv("STRIPE_PUBLISHABLE_KEY", "")

# Billing Settings
BILLING_ENABLED = os.getenv("BILLING_ENABLED", "true").lower() == "true"
BILLING_TRIAL_DAYS = int(os.getenv("BILLING_TRIAL_DAYS", 14))
BILLING_GRACE_PERIOD_DAYS = int(os.getenv("BILLING_GRACE_PERIOD_DAYS", 7))

# Multi-Tenant Settings
MULTI_TENANT_ENABLED = os.getenv("MULTI_TENANT_ENABLED", "true").lower() == "true"
TENANT_BASE_DOMAIN = os.getenv("TENANT_BASE_DOMAIN", "fabricadeagentes.com")

# =============================================================================
# RATE LIMITING
# =============================================================================

RATE_LIMIT_ENABLED = os.getenv("RATE_LIMIT_ENABLED", "true").lower() == "true"
RATE_LIMIT_REQUESTS = int(os.getenv("RATE_LIMIT_REQUESTS", 100))
RATE_LIMIT_WINDOW = int(os.getenv("RATE_LIMIT_WINDOW", 60))  # segundos

# Limites por endpoint (requests/window)
RATE_LIMITS = {
    "default": (100, 60),      # 100 req/min
    "jobs_create": (10, 60),   # 10 criacao de jobs/min
    "auth_login": (5, 60),     # 5 tentativas login/min
    "health": (1000, 60),      # 1000 health checks/min
}

# =============================================================================
# AUTONOMOUS LOOP
# =============================================================================

class LoopStep(Enum):
    """Etapas do loop autonomo"""
    SETUP = "setup"
    GENERATE = "generate"
    LINT = "lint"
    TEST = "test"
    FIX = "fix"
    COMPLETE = "complete"
    FAILED = "failed"


# Comandos por etapa
LINT_COMMANDS = {
    "python": ["ruff", "check", "."],
    "javascript": ["eslint", "."],
    "typescript": ["eslint", ".", "--ext", ".ts,.tsx"],
}

TEST_COMMANDS = {
    "python": ["pytest", "-v"],
    "javascript": ["npm", "test"],
    "typescript": ["npm", "test"],
}

# =============================================================================
# MCP TOOLS
# =============================================================================

MCP_ENABLED = os.getenv("MCP_ENABLED", "true").lower() == "true"

MCP_SERVERS = {
    "playwright": {
        "name": "Playwright MCP",
        "description": "Automacao de browser com Playwright",
        "command": "npx",
        "args": ["@anthropic/mcp-server-playwright"],
        "enabled": os.getenv("MCP_PLAYWRIGHT_ENABLED", "true").lower() == "true"
    },
    "filesystem": {
        "name": "Filesystem MCP",
        "description": "Operacoes avancadas de filesystem",
        "command": "npx",
        "args": ["@anthropic/mcp-server-filesystem"],
        "enabled": os.getenv("MCP_FILESYSTEM_ENABLED", "true").lower() == "true"
    },
    "github": {
        "name": "GitHub MCP",
        "description": "Integracao com GitHub",
        "command": "npx",
        "args": ["@anthropic/mcp-server-github"],
        "enabled": os.getenv("MCP_GITHUB_ENABLED", "false").lower() == "true"
    },
    "memory": {
        "name": "Memory MCP",
        "description": "Memoria persistente para contexto",
        "command": "npx",
        "args": ["@anthropic/mcp-server-memory"],
        "enabled": os.getenv("MCP_MEMORY_ENABLED", "true").lower() == "true"
    }
}

# =============================================================================
# DASHBOARD / API
# =============================================================================

DASHBOARD_HOST = os.getenv("DASHBOARD_HOST", "127.0.0.1")
DASHBOARD_PORT = int(os.getenv("DASHBOARD_PORT", 9000))
DASHBOARD_TITLE = "Fabrica de Agentes v4.0"
DASHBOARD_VERSION = "4.0.0"

# CORS
CORS_ORIGINS = os.getenv("CORS_ORIGINS", "http://localhost:9000,http://127.0.0.1:9000").split(",")
CORS_ALLOW_CREDENTIALS = True

# =============================================================================
# LOGGING
# =============================================================================

LOG_LEVEL = os.getenv("LOG_LEVEL", "INFO")
LOG_FORMAT = "{time:YYYY-MM-DD HH:mm:ss} | {level: <8} | {name}:{function}:{line} | {message}"
LOG_ROTATION = "10 MB"
LOG_RETENTION = "7 days"
LOG_DIR = FACTORY_DIR / "logs"

# =============================================================================
# PAUSE & CONTROL
# =============================================================================

class PauseCondition(Enum):
    """Condicoes que disparam pausa automatica"""
    CRITICAL_ERROR = "critical_error"
    WORKER_FAILED = "worker_failed"
    CONSECUTIVE_ERRORS = "consecutive_errors"
    QUEUE_FULL = "queue_full"
    MANUAL_PAUSE = "manual_pause"


PAUSE_FILE = FACTORY_DIR / ".pause"
STOP_FILE = FACTORY_DIR / ".stop"

# Limites de erro
MAX_CONSECUTIVE_ERRORS = int(os.getenv("MAX_CONSECUTIVE_ERRORS", 5))
MAX_QUEUE_SIZE = int(os.getenv("MAX_QUEUE_SIZE", 100))

# =============================================================================
# PROJECT TYPES (simplificado)
# =============================================================================

PROJECT_TYPES = {
    "web-app": {
        "name": "Aplicacao Web",
        "description": "Aplicacao web fullstack",
        "default_stack": "python,fastapi,react"
    },
    "api-service": {
        "name": "API Service",
        "description": "Servico de API REST",
        "default_stack": "python,fastapi"
    },
    "data-analysis": {
        "name": "Analise de Dados",
        "description": "Projeto de analise de dados",
        "default_stack": "python,pandas,plotly"
    },
    "automation": {
        "name": "Automacao",
        "description": "Scripts e automacoes",
        "default_stack": "python"
    },
    "integration": {
        "name": "Integracao",
        "description": "Integracoes entre sistemas",
        "default_stack": "python,fastapi"
    }
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_enabled_mcp_tools() -> list:
    """Retorna lista de ferramentas MCP habilitadas"""
    if not MCP_ENABLED:
        return []
    return [name for name, config in MCP_SERVERS.items() if config.get("enabled")]


def is_production() -> bool:
    """Verifica se esta em ambiente de producao"""
    return os.getenv("ENVIRONMENT", "development").lower() == "production"


def get_database_url() -> str:
    """Retorna URL do banco apropriada para o ambiente"""
    if is_production():
        return DATABASE_URL
    # Em desenvolvimento, pode usar SQLite como fallback
    if "postgresql" not in DATABASE_URL and not os.getenv("DATABASE_URL"):
        return f"sqlite:///{FACTORY_DB}"
    return DATABASE_URL


# =============================================================================
# ENVIRONMENT VALIDATION - Issue #171
# =============================================================================

class ConfigValidationError(Exception):
    """Erro de validação de configuração"""
    pass


# Variáveis obrigatórias por ambiente
REQUIRED_ENV_VARS = {
    "production": [
        ("ANTHROPIC_API_KEY", "Chave de API da Anthropic"),
        ("JWT_SECRET_KEY", "Chave secreta para JWT (gerar com: python -c \"import secrets; print(secrets.token_urlsafe(32))\")"),
        ("DATABASE_URL", "URL de conexão PostgreSQL"),
    ],
    "development": [
        # Em desenvolvimento, apenas API key é obrigatória se usar Claude
    ]
}

# Variáveis recomendadas (warning se ausentes)
RECOMMENDED_ENV_VARS = [
    ("REDIS_URL", "URL do Redis para filas e cache"),
    ("JWT_SECRET_KEY", "Chave secreta para JWT"),
]


def validate_environment(raise_on_error: bool = False) -> dict:
    """
    Valida variáveis de ambiente no startup - Issue #171

    Args:
        raise_on_error: Se True, levanta exceção em caso de erro

    Returns:
        Dict com status da validação:
        {
            "valid": bool,
            "errors": list[str],
            "warnings": list[str],
            "environment": str
        }
    """
    import logging
    logger = logging.getLogger(__name__)

    result = {
        "valid": True,
        "errors": [],
        "warnings": [],
        "environment": "production" if is_production() else "development"
    }

    env = result["environment"]

    # Validar variáveis obrigatórias
    for var_name, description in REQUIRED_ENV_VARS.get(env, []):
        value = os.getenv(var_name, "")
        if not value:
            error_msg = f"Variável obrigatória ausente: {var_name} - {description}"
            result["errors"].append(error_msg)
            result["valid"] = False
            logger.error(f"[CONFIG] {error_msg}")

    # Validar variáveis recomendadas
    for var_name, description in RECOMMENDED_ENV_VARS:
        value = os.getenv(var_name, "")
        if not value:
            warning_msg = f"Variável recomendada ausente: {var_name} - {description}"
            result["warnings"].append(warning_msg)
            logger.warning(f"[CONFIG] {warning_msg}")

    # Validações específicas
    if ANTHROPIC_API_KEY and not ANTHROPIC_API_KEY.startswith("sk-ant-"):
        warning_msg = "ANTHROPIC_API_KEY não parece ser uma chave válida (deve começar com 'sk-ant-')"
        result["warnings"].append(warning_msg)
        logger.warning(f"[CONFIG] {warning_msg}")

    if JWT_SECRET_KEY and len(JWT_SECRET_KEY) < 32:
        warning_msg = "JWT_SECRET_KEY é muito curta (recomendado: 32+ caracteres)"
        result["warnings"].append(warning_msg)
        logger.warning(f"[CONFIG] {warning_msg}")

    # Log resultado
    if result["valid"]:
        logger.info(f"[CONFIG] Validação concluída: ambiente={env}, warnings={len(result['warnings'])}")
    else:
        logger.error(f"[CONFIG] Validação falhou: {len(result['errors'])} erros, {len(result['warnings'])} warnings")

    if raise_on_error and not result["valid"]:
        raise ConfigValidationError(
            f"Configuração inválida: {', '.join(result['errors'])}"
        )

    return result


def print_config_status():
    """Imprime status da configuração para debug"""
    result = validate_environment()
    env = result["environment"]

    print(f"\n{'='*60}")
    print(f"FÁBRICA DE AGENTES - Configuração ({env.upper()})")
    print(f"{'='*60}")

    print(f"\n📦 Database: {'PostgreSQL' if 'postgresql' in DATABASE_URL else 'SQLite'}")
    print(f"🔧 Redis: {REDIS_URL}")
    print(f"🤖 LLM Provider: {LLM_PROVIDER}")
    print(f"🔐 JWT Configured: {'✓' if JWT_SECRET_KEY else '✗'}")
    print(f"🔑 API Key Configured: {'✓' if ANTHROPIC_API_KEY else '✗'}")

    if result["warnings"]:
        print(f"\n⚠️  Warnings ({len(result['warnings'])}):")
        for w in result["warnings"]:
            print(f"   - {w}")

    if result["errors"]:
        print(f"\n❌ Errors ({len(result['errors'])}):")
        for e in result["errors"]:
            print(f"   - {e}")

    print(f"\n{'='*60}\n")
