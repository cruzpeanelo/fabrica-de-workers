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
JWT_SECRET_FILE = FACTORY_DIR / ".jwt_secret"

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
# CLAUDE API
# =============================================================================

# API Key
ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY", "")

# Modelo padrao
CLAUDE_MODEL = os.getenv("CLAUDE_MODEL", "claude-sonnet-4-20250514")

# Token limits
CLAUDE_MAX_TOKENS = int(os.getenv("CLAUDE_MAX_TOKENS", 4096))
CLAUDE_CONTEXT_LIMIT = int(os.getenv("CLAUDE_CONTEXT_LIMIT", 200000))

# Autonomous loop settings
AUTONOMOUS_MAX_ITERATIONS = int(os.getenv("AUTONOMOUS_MAX_ITERATIONS", 5))
AUTONOMOUS_FIX_ATTEMPTS = int(os.getenv("AUTONOMOUS_FIX_ATTEMPTS", 3))

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
