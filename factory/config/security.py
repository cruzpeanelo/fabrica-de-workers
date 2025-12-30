# -*- coding: utf-8 -*-
"""
Security Configuration - Issue #138, #200
==========================================

Configurações de segurança para autenticação, validação de senhas,
e criptografia de dados em repouso.
"""

import os
import re
from typing import Tuple, List, Optional


def _is_production() -> bool:
    """Verifica se está em ambiente de produção"""
    return os.getenv("ENVIRONMENT", "development").lower() == "production"


# =============================================================================
# BLOCKED CREDENTIALS - Issue #138
# =============================================================================

# Lista de credenciais bloqueadas em produção (username, password)
BLOCKED_CREDENTIALS: List[Tuple[str, str]] = [
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

# =============================================================================
# PASSWORD REQUIREMENTS
# =============================================================================

PASSWORD_MIN_LENGTH = int(os.getenv("PASSWORD_MIN_LENGTH", "8"))
PASSWORD_REQUIRE_UPPERCASE = os.getenv("PASSWORD_REQUIRE_UPPERCASE", "true").lower() == "true"
PASSWORD_REQUIRE_LOWERCASE = os.getenv("PASSWORD_REQUIRE_LOWERCASE", "true").lower() == "true"
PASSWORD_REQUIRE_DIGIT = os.getenv("PASSWORD_REQUIRE_DIGIT", "true").lower() == "true"
PASSWORD_REQUIRE_SPECIAL = os.getenv("PASSWORD_REQUIRE_SPECIAL", "false").lower() == "true"


# =============================================================================
# SECURITY FUNCTIONS
# =============================================================================

def is_credential_blocked(username: str, password: str) -> bool:
    """
    Issue #138: Verifica se credencial está na lista de bloqueio.

    Em produção, bloqueia credenciais conhecidas como inseguras.
    Em desenvolvimento, retorna False (permite tudo para facilitar testes).

    Args:
        username: Nome de usuário
        password: Senha em texto plano

    Returns:
        True se a credencial está bloqueada (não pode ser usada em produção)
    """
    if not _is_production():
        return False

    username_lower = username.lower()
    for blocked_user, blocked_pass in BLOCKED_CREDENTIALS:
        if username_lower == blocked_user.lower() and password == blocked_pass:
            return True
    return False


def validate_password_strength(password: str, is_prod: bool = None) -> Tuple[bool, str]:
    """
    Issue #138: Valida força da senha.

    Args:
        password: Senha a validar
        is_prod: Se None, usa _is_production()

    Returns:
        tuple: (is_valid, error_message)
    """
    if is_prod is None:
        is_prod = _is_production()

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


def get_password_requirements() -> dict:
    """
    Retorna os requisitos de senha atuais.

    Returns:
        Dict com requisitos para exibição na UI
    """
    return {
        "min_length": PASSWORD_MIN_LENGTH,
        "require_uppercase": PASSWORD_REQUIRE_UPPERCASE,
        "require_lowercase": PASSWORD_REQUIRE_LOWERCASE,
        "require_digit": PASSWORD_REQUIRE_DIGIT,
        "require_special": PASSWORD_REQUIRE_SPECIAL,
        "is_production": _is_production()
    }


# =============================================================================
# DATA ENCRYPTION - Issue #200
# =============================================================================

# Master encryption key (must be set in production)
ENCRYPTION_MASTER_KEY = os.getenv("ENCRYPTION_MASTER_KEY")

# Encryption salt (unique per deployment)
ENCRYPTION_SALT = os.getenv("ENCRYPTION_SALT", "fabrica-agentes-v1")

# Key rotation interval in days
ENCRYPTION_KEY_ROTATION_DAYS = int(os.getenv("ENCRYPTION_KEY_ROTATION_DAYS", "90"))

# Vault integration
VAULT_ENABLED = os.getenv("VAULT_ENABLED", "false").lower() == "true"
VAULT_ADDR = os.getenv("VAULT_ADDR", "")
VAULT_TOKEN = os.getenv("VAULT_TOKEN", "")
VAULT_PATH_PREFIX = os.getenv("VAULT_PATH_PREFIX", "secret/fabrica")

# Fields that require encryption (by model)
ENCRYPTED_FIELDS = {
    "Integration": ["api_token", "webhook_secret", "oauth_refresh_token"],
    "User": ["phone"],  # PII
    "APIKey": ["key_hash"],
    "WebhookConfig": ["secret"],
}


def is_encryption_configured() -> bool:
    """
    Issue #200: Check if encryption is properly configured.

    Returns:
        True if encryption can be used
    """
    if _is_production():
        return bool(ENCRYPTION_MASTER_KEY)
    return True  # Allow in dev without key


def get_encryption_status() -> dict:
    """
    Issue #200: Get current encryption configuration status.

    Returns:
        Dict with encryption status info
    """
    return {
        "encryption_configured": is_encryption_configured(),
        "vault_enabled": VAULT_ENABLED,
        "vault_connected": VAULT_ENABLED and bool(VAULT_ADDR) and bool(VAULT_TOKEN),
        "key_rotation_days": ENCRYPTION_KEY_ROTATION_DAYS,
        "encrypted_models": list(ENCRYPTED_FIELDS.keys()),
        "is_production": _is_production()
    }


def generate_encryption_key() -> str:
    """
    Issue #200: Generate a new encryption master key.

    Should be called once during initial setup.

    Returns:
        Base64-encoded key
    """
    import base64
    import secrets
    return base64.urlsafe_b64encode(secrets.token_bytes(32)).decode()


__all__ = [
    # Password security
    "is_credential_blocked",
    "validate_password_strength",
    "get_password_requirements",
    "BLOCKED_CREDENTIALS",
    "WEAK_PASSWORD_PATTERNS",
    "PASSWORD_MIN_LENGTH",
    "PASSWORD_REQUIRE_UPPERCASE",
    "PASSWORD_REQUIRE_LOWERCASE",
    "PASSWORD_REQUIRE_DIGIT",
    "PASSWORD_REQUIRE_SPECIAL",
    # Encryption (Issue #200)
    "ENCRYPTION_MASTER_KEY",
    "ENCRYPTION_SALT",
    "ENCRYPTION_KEY_ROTATION_DAYS",
    "VAULT_ENABLED",
    "VAULT_ADDR",
    "VAULT_TOKEN",
    "ENCRYPTED_FIELDS",
    "is_encryption_configured",
    "get_encryption_status",
    "generate_encryption_key",
]
